#!/usr/bin/env python3
"""Suggest Fortran procedures that could be marked PURE/ELEMENTAL.

Heuristic static analyzer (conservative):
- scans procedures in a source file
- ignores those already marked pure/elemental
- flags obvious purity blockers (I/O, RNG/system intrinsics, calls to known non-pure procedures)
- reports remaining procedures as likely candidates
"""

from __future__ import annotations

import argparse
import difflib
import fortran_build as fbuild
import fortran_scan as fscan
import re
import shutil
import subprocess
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Set, Tuple


PROC_START_RE = re.compile(
    r"^\s*(?P<prefix>(?:(?:pure|elemental|impure|recursive|module)\s+)*)"
    r"(?P<kind>function|subroutine)\s+"
    r"(?P<name>[a-z][a-z0-9_]*)\s*(?P<arglist>\([^)]*\))?",
    re.IGNORECASE,
)

PRINT_RE = re.compile(r"^\s*print\b", re.IGNORECASE)
READ_RE = re.compile(r"^\s*read\s*\(", re.IGNORECASE)
WRITE_RE = re.compile(r"^\s*write\s*\(", re.IGNORECASE)
FILE_IO_RE = re.compile(r"^\s*(open|close|rewind|backspace|flush|inquire)\b", re.IGNORECASE)

ERROR_STOP_RE = re.compile(r"\berror\s+stop\b", re.IGNORECASE)
STOP_RE = re.compile(r"\bstop\b", re.IGNORECASE)

CALL_RE = re.compile(r"\bcall\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
USE_RE = re.compile(
    r"^\s*use\b(?:\s*,\s*(?:non_intrinsic|intrinsic)\s*)?(?:\s*::\s*|\s+)([a-z][a-z0-9_]*)",
    re.IGNORECASE,
)
MODULE_DEF_RE = re.compile(r"^\s*module\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
INTERFACE_START_RE = re.compile(r"^\s*(abstract\s+)?interface\b(?:\s+([a-z][a-z0-9_]*))?", re.IGNORECASE)
END_INTERFACE_RE = re.compile(r"^\s*end\s+interface\b", re.IGNORECASE)
MODULE_PROCEDURE_RE = re.compile(r"^\s*module\s+procedure\b(.+)$", re.IGNORECASE)
PROCEDURE_DECL_RE = re.compile(r"^\s*procedure\s*\(", re.IGNORECASE)
TYPE_DECL_RE = re.compile(
    r"^\s*(integer|real|logical|character|complex|type\b|class\b|procedure\b)",
    re.IGNORECASE,
)
ASSIGN_RE = re.compile(
    r"^\s*([a-z][a-z0-9_]*(?:\s*%\s*[a-z][a-z0-9_]*)?)\s*(?:\([^)]*\))?\s*=",
    re.IGNORECASE,
)
ALLOC_DEALLOC_RE = re.compile(r"^\s*(allocate|deallocate)\s*\((.+)\)\s*$", re.IGNORECASE)
DECL_RE = re.compile(
    r"^(\s*)(?P<prefix>(?:(?:pure|elemental|impure|recursive|module)\s+)*)"
    r"(?P<kind>function|subroutine)\b",
    re.IGNORECASE,
)

IMPURE_INTRINSICS = {
    "random_number",
    "random_seed",
    "random_normal",
    "date_and_time",
    "cpu_time",
    "system_clock",
    "execute_command_line",
    "get_command",
    "get_command_argument",
    "get_environment_variable",
}


@dataclass
class Procedure:
    name: str
    kind: str
    start: int
    end: int = -1
    attrs: Set[str] = field(default_factory=set)
    body: List[Tuple[int, str]] = field(default_factory=list)
    parent: Optional[str] = None
    dummy_names: Set[str] = field(default_factory=set)
    result_name: Optional[str] = None

    @property
    def is_pure_or_elemental(self) -> bool:
        return "pure" in self.attrs or "elemental" in self.attrs

    @property
    def selector(self) -> str:
        return f"{self.name}@{self.start}"


@dataclass
class AnalysisResult:
    procedures: List[Procedure]
    candidates: List[Procedure]
    rejected: List[Tuple[Procedure, List[str]]]


@dataclass
class SourceFileInfo:
    path: Path
    lines: List[str]
    parsed_lines: List[str]
    procedures: List[Procedure]
    defined_modules: Set[str]
    used_modules: Set[str]
    generic_interfaces: Dict[str, Set[str]]


def display_path(path: Path) -> str:
    return fscan.display_path(path)


def quote_cmd_arg(arg: str) -> str:
    return fbuild.quote_cmd_arg(arg)


def run_compiler_command(command: str, files: List[Path], phase: str) -> bool:
    return fbuild.run_compiler_command(command, files, phase, display_path)


def strip_comment(line: str) -> str:
    # Good-enough comment stripper for typical free-form Fortran.
    in_single = False
    in_double = False
    for i, ch in enumerate(line):
        if ch == "'" and not in_double:
            in_single = not in_single
        elif ch == '"' and not in_single:
            in_double = not in_double
        elif ch == "!" and not in_single and not in_double:
            return line[:i]
    return line


def parse_arglist(arglist: Optional[str]) -> Set[str]:
    if not arglist:
        return set()
    inner = arglist.strip()[1:-1].strip()
    if not inner:
        return set()
    out: Set[str] = set()
    for tok in inner.split(","):
        name = tok.strip().lower()
        if re.match(r"^[a-z][a-z0-9_]*$", name):
            out.add(name)
    return out


def parse_declared_names_from_decl(line: str) -> Set[str]:
    if "::" not in line:
        return set()
    rhs = line.split("::", 1)[1]
    out: Set[str] = set()
    for chunk in rhs.split(","):
        name = chunk.strip()
        if not name:
            continue
        if "=" in name and "=>" not in name:
            name = name.split("=", 1)[0].strip()
        if "=>" in name:
            name = name.split("=>", 1)[0].strip()
        m = re.match(r"^([a-z][a-z0-9_]*)", name, re.IGNORECASE)
        if m:
            out.add(m.group(1).lower())
    return out


def parse_declared_entities(line: str) -> List[Tuple[str, bool]]:
    """Return (name, has_array_spec_after_name) for entities in a declaration."""
    if "::" not in line:
        return []
    rhs = line.split("::", 1)[1]
    out: List[Tuple[str, bool]] = []
    for chunk in rhs.split(","):
        text = chunk.strip()
        if not text:
            continue
        m = re.match(r"^([a-z][a-z0-9_]*)\s*(\()?", text, re.IGNORECASE)
        if not m:
            continue
        out.append((m.group(1).lower(), m.group(2) is not None))
    return out


def base_identifier(expr: str) -> Optional[str]:
    m = re.match(r"^\s*([a-z][a-z0-9_]*)", expr, re.IGNORECASE)
    if not m:
        return None
    return m.group(1).lower()


def split_top_level_commas(text: str) -> List[str]:
    out: List[str] = []
    cur: List[str] = []
    depth = 0
    in_single = False
    in_double = False
    for ch in text:
        if ch == "'" and not in_double:
            in_single = not in_single
        elif ch == '"' and not in_single:
            in_double = not in_double
        elif not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")" and depth > 0:
                depth -= 1
            elif ch == "," and depth == 0:
                out.append("".join(cur).strip())
                cur = []
                continue
        cur.append(ch)
    tail = "".join(cur).strip()
    if tail:
        out.append(tail)
    return out


def extract_io_control_list(line: str, keyword: str) -> Optional[str]:
    s = line.strip()
    if not s.lower().startswith(keyword):
        return None
    p0 = s.find("(")
    if p0 < 0:
        return None
    depth = 0
    in_single = False
    in_double = False
    for i in range(p0, len(s)):
        ch = s[i]
        if ch == "'" and not in_double:
            in_single = not in_single
        elif ch == '"' and not in_single:
            in_double = not in_double
        elif not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
                if depth == 0:
                    return s[p0 + 1:i]
    return None


def parse_decl_intent(line_low: str) -> Optional[str]:
    m = re.search(r"\bintent\s*\(\s*(inout|out|in)\s*\)", line_low)
    if m:
        return m.group(1).lower()
    if re.search(r"\bvalue\b", line_low):
        return "value"
    return None


def io_unit_expr_from_control(control: str) -> Optional[str]:
    tokens = split_top_level_commas(control)
    if not tokens:
        return None
    for tok in tokens:
        if "=" in tok:
            lhs, rhs = tok.split("=", 1)
            if lhs.strip().lower() == "unit":
                return rhs.strip()
    first = tokens[0].strip()
    if "=" in first:
        return None
    return first


def parse_procedures(lines: List[str]) -> List[Procedure]:
    stack: List[Procedure] = []
    out: List[Procedure] = []
    interface_depth = 0

    for lineno, raw in enumerate(lines, start=1):
        code = strip_comment(raw).rstrip()
        low = code.lower().strip()

        if re.match(r"^\s*(abstract\s+)?interface\b", low):
            interface_depth += 1
            continue
        if re.match(r"^\s*end\s+interface\b", low):
            if interface_depth > 0:
                interface_depth -= 1
            continue

        if interface_depth > 0:
            continue

        m_start = PROC_START_RE.match(low)
        if m_start:
            attrs = set(m_start.group("prefix").split()) if m_start.group("prefix") else set()
            parent = stack[-1].name if stack else None
            dummy_names = parse_arglist(m_start.group("arglist"))
            m_result = re.search(r"\bresult\s*\(\s*([a-z][a-z0-9_]*)\s*\)", low, re.IGNORECASE)
            result_name = m_result.group(1).lower() if m_result else None
            stack.append(
                Procedure(
                    name=m_start.group("name"),
                    kind=m_start.group("kind"),
                    start=lineno,
                    attrs=attrs,
                    parent=parent,
                    dummy_names=dummy_names,
                    result_name=result_name,
                )
            )
            continue

        if stack and low.startswith("end"):
            toks = low.split()
            # Accept only:
            #   end
            #   end function [name]
            #   end subroutine [name]
            # and ignore constructs like "end if", "end do", etc.
            is_proc_end = False
            end_kind: Optional[str] = None
            if len(toks) == 1:
                is_proc_end = True
            elif len(toks) >= 2 and toks[1] in {"function", "subroutine"}:
                is_proc_end = True
                end_kind = toks[1]
            if is_proc_end:
                top = stack[-1]
                if end_kind is None or end_kind == top.kind:
                    top.end = lineno
                    out.append(stack.pop())
                    continue

        if stack:
            stack[-1].body.append((lineno, code))

    # Best effort for malformed files.
    while stack:
        top = stack.pop()
        top.end = len(lines)
        out.append(top)

    out.sort(key=lambda p: p.start)
    return out


def has_function_reference(line: str, callee: str) -> bool:
    # Looks for token like "callee(".
    return re.search(rf"\b{re.escape(callee)}\s*\(", line, flags=re.IGNORECASE) is not None


def parse_modules_and_generics(lines: List[str]) -> Tuple[Set[str], Set[str], Dict[str, Set[str]]]:
    defined: Set[str] = set()
    used: Set[str] = set()
    generics: Dict[str, Set[str]] = {}
    interface_depth = 0
    current_generic: Optional[str] = None
    current_is_abstract = False
    for raw in lines:
        code = strip_comment(raw).strip()
        low = code.lower()
        if not low:
            continue
        m_if = INTERFACE_START_RE.match(low)
        if m_if:
            interface_depth += 1
            current_is_abstract = bool(m_if.group(1))
            name = m_if.group(2)
            if not current_is_abstract and name:
                current_generic = name.lower()
                generics.setdefault(current_generic, set())
            else:
                current_generic = None
            continue
        if END_INTERFACE_RE.match(low):
            if interface_depth > 0:
                interface_depth -= 1
            if interface_depth == 0:
                current_generic = None
                current_is_abstract = False
            continue
        m_mod = MODULE_DEF_RE.match(low)
        if m_mod:
            toks = low.split()
            if len(toks) >= 2 and toks[1] != "procedure":
                defined.add(m_mod.group(1).lower())
        m_use = USE_RE.match(low)
        if m_use:
            used.add(m_use.group(1).lower())
        if interface_depth > 0 and not current_is_abstract and current_generic:
            m_mp = MODULE_PROCEDURE_RE.match(low)
            if m_mp:
                names = [n.strip().lower() for n in m_mp.group(1).split(",")]
                for name in names:
                    if re.match(r"^[a-z][a-z0-9_]*$", name):
                        generics[current_generic].add(name)
    return defined, used, generics


def load_source_files(paths: Iterable[Path]) -> Tuple[List[SourceFileInfo], bool]:
    infos: List[SourceFileInfo] = []
    any_missing = False
    for p in paths:
        if not p.exists():
            print(f"File not found: {display_path(p)}")
            any_missing = True
            continue
        text = p.read_text(encoding="utf-8", errors="ignore")
        lines = text.splitlines(keepends=True)
        parsed_lines = [ln.rstrip("\r\n") for ln in lines]
        procs = parse_procedures(parsed_lines)
        defined_modules, used_modules, generic_interfaces = parse_modules_and_generics(parsed_lines)
        infos.append(
            SourceFileInfo(
                path=p,
                lines=lines,
                parsed_lines=parsed_lines,
                procedures=procs,
                defined_modules=defined_modules,
                used_modules=used_modules,
                generic_interfaces=generic_interfaces,
            )
        )
    return infos, any_missing


def compute_file_dependencies(files: List[SourceFileInfo]) -> Dict[Path, Set[Path]]:
    proc_name_to_files: Dict[str, Set[Path]] = {}
    module_to_file: Dict[str, Path] = {}

    for finfo in files:
        for proc in finfo.procedures:
            if proc.parent is None:
                proc_name_to_files.setdefault(proc.name.lower(), set()).add(finfo.path)
        for mod in finfo.defined_modules:
            if mod not in module_to_file:
                module_to_file[mod] = finfo.path

    deps: Dict[Path, Set[Path]] = {f.path: set() for f in files}
    for finfo in files:
        fdeps: Set[Path] = set()
        for mod in finfo.used_modules:
            provider = module_to_file.get(mod)
            if provider and provider != finfo.path:
                fdeps.add(provider)
        for proc in finfo.procedures:
            for _, code in proc.body:
                low = code.lower()
                for m in CALL_RE.finditer(low):
                    callee = m.group(1).lower()
                    providers = proc_name_to_files.get(callee, set())
                    for provider in providers:
                        if provider != finfo.path:
                            fdeps.add(provider)
        deps[finfo.path] = fdeps
    return deps


def order_files_least_dependent(files: List[SourceFileInfo]) -> Tuple[List[SourceFileInfo], bool]:
    if len(files) <= 1:
        return files[:], False

    deps = compute_file_dependencies(files)
    remaining = {f.path for f in files}
    ordered_paths: List[Path] = []
    had_cycle = False

    while remaining:
        ready = sorted([p for p in remaining if not (deps[p] & remaining)], key=lambda x: str(x).lower())
        if not ready:
            had_cycle = True
            ready = sorted(remaining, key=lambda x: str(x).lower())
        for p in ready:
            ordered_paths.append(p)
            remaining.remove(p)

    by_path = {f.path: f for f in files}
    return [by_path[p] for p in ordered_paths], had_cycle


def build_compile_closure(requested_files: List[SourceFileInfo]) -> Tuple[List[Path], Set[str]]:
    # Discover nearby sources so module providers can be included automatically.
    candidate_paths: Set[Path] = {f.path.resolve() for f in requested_files}
    for finfo in requested_files:
        parent = finfo.path.resolve().parent
        candidate_paths.update(p.resolve() for p in parent.glob("*.f90"))
        candidate_paths.update(p.resolve() for p in parent.glob("*.F90"))

    all_infos, _ = load_source_files(sorted(candidate_paths, key=lambda p: str(p).lower()))
    by_path: Dict[Path, SourceFileInfo] = {f.path.resolve(): f for f in all_infos}

    module_to_file: Dict[str, Path] = {}
    for finfo in all_infos:
        for mod in finfo.defined_modules:
            module_to_file.setdefault(mod, finfo.path.resolve())

    needed_paths: Set[Path] = {f.path.resolve() for f in requested_files}
    unresolved: Set[str] = set()
    changed = True
    while changed:
        changed = False
        for p in list(needed_paths):
            finfo = by_path.get(p)
            if finfo is None:
                continue
            for mod in finfo.used_modules:
                provider = module_to_file.get(mod)
                if provider is None:
                    unresolved.add(mod)
                    continue
                if provider not in needed_paths:
                    needed_paths.add(provider)
                    changed = True

    needed_infos = [by_path[p] for p in needed_paths if p in by_path]
    ordered_infos, _ = order_files_least_dependent(needed_infos)
    return [f.path for f in ordered_infos], unresolved


def analyze_lines(
    lines: List[str],
    external_name_status: Optional[Dict[str, bool]] = None,
    generic_interfaces: Optional[Dict[str, Set[str]]] = None,
    strict_unknown_calls: bool = False,
) -> AnalysisResult:
    procs = parse_procedures(lines)
    if not procs:
        return AnalysisResult(procs, [], [])

    by_name: Dict[str, List[Procedure]] = {}
    for p in procs:
        by_name.setdefault(p.name.lower(), []).append(p)
    has_nonpure_name = {
        name for name, plist in by_name.items() if any(not p.is_pure_or_elemental for p in plist)
    }
    external_nonpure = {
        name for name, is_pure in (external_name_status or {}).items() if not is_pure
    }
    external_known = set((external_name_status or {}).keys())
    generics = generic_interfaces or {}
    generic_nonpure_names = {
        gname
        for gname, targets in generics.items()
        if any(t in has_nonpure_name or t in external_nonpure for t in targets)
    }
    ref_nonpure_names = has_nonpure_name | external_nonpure | generic_nonpure_names

    candidates: List[Procedure] = []
    rejected: List[Tuple[Procedure, List[str]]] = []

    for proc in procs:
        if proc.is_pure_or_elemental:
            continue

        reasons: List[str] = []
        local_names: Set[str] = set(proc.dummy_names)
        if proc.result_name:
            local_names.add(proc.result_name.lower())
        dummy_with_intent_or_value: Set[str] = set()
        dummy_intent: Dict[str, str] = {}
        character_names: Set[str] = set()

        children = [p for p in procs if p.parent and p.parent.lower() == proc.name.lower()]
        nonpure_children = [c.name for c in children if not c.is_pure_or_elemental]
        if nonpure_children:
            reasons.append(f"contains non-pure internal procedures: {', '.join(nonpure_children)}")

        for ln, code in proc.body:
            low = code.lower()
            if not low.strip():
                continue

            if TYPE_DECL_RE.match(low):
                declared = parse_declared_names_from_decl(low)
                if declared:
                    local_names.update(declared)
                    if low.strip().startswith("character"):
                        character_names.update(declared)
                    intent_attr = parse_decl_intent(low)
                    if intent_attr:
                        for d in proc.dummy_names:
                            if d in declared:
                                dummy_intent[d] = intent_attr
                    if proc.kind == "subroutine" and (
                        "intent(" in low or re.search(r"\bvalue\b", low)
                    ):
                        for d in proc.dummy_names:
                            if d in declared:
                                dummy_with_intent_or_value.add(d)

            if PROCEDURE_DECL_RE.match(low):
                reasons.append(
                    f"line {ln}: procedure dummy/pointer declaration (conservatively treated as non-pure candidate)"
                )

            if PRINT_RE.match(low):
                reasons.append(f"line {ln}: PRINT statement")
            if FILE_IO_RE.match(low):
                reasons.append(f"line {ln}: file I/O statement")
            if READ_RE.match(low) or WRITE_RE.match(low):
                io_kw = "write" if WRITE_RE.match(low) else "read"
                control = extract_io_control_list(low, io_kw)
                if control is None:
                    reasons.append(f"line {ln}: malformed {io_kw.upper()} control list")
                else:
                    unit_expr = io_unit_expr_from_control(control)
                    if unit_expr is None:
                        reasons.append(
                            f"line {ln}: {io_kw.upper()} without clear internal unit (conservative block)"
                        )
                    else:
                        ue = unit_expr.strip()
                        if ue == "*" or re.match(r"^[+-]?\d+$", ue):
                            reasons.append(f"line {ln}: external {io_kw.upper()} unit")
                        elif re.match(r"^['\"]", ue):
                            # char literal internal file: READ can be valid, WRITE should be blocked.
                            if io_kw == "write":
                                reasons.append(f"line {ln}: WRITE to literal internal file is not allowed")
                        else:
                            unit_base = base_identifier(ue)
                            if unit_base is None or unit_base not in character_names:
                                reasons.append(
                                    f"line {ln}: {io_kw.upper()} appears external/unknown (unit '{ue}')"
                                )
                            elif io_kw == "write":
                                # Internal WRITE allowed only to legal mutable targets.
                                if unit_base in proc.dummy_names:
                                    if proc.kind == "function":
                                        reasons.append(
                                            f"line {ln}: internal WRITE target '{unit_base}' is dummy in function"
                                        )
                                    else:
                                        dint = dummy_intent.get(unit_base, "")
                                        if dint not in {"out", "inout", "value"}:
                                            reasons.append(
                                                f"line {ln}: internal WRITE target dummy '{unit_base}' "
                                                "needs INTENT(OUT/INOUT) or VALUE"
                                            )
                                elif unit_base not in local_names:
                                    reasons.append(
                                        f"line {ln}: internal WRITE target '{unit_base}' is non-local"
                                    )
            stop_check = ERROR_STOP_RE.sub("", low)
            if STOP_RE.search(stop_check):
                reasons.append(f"line {ln}: STOP statement")

            for intr in IMPURE_INTRINSICS:
                if re.search(rf"\b(?:call\s+)?{re.escape(intr)}\b", low):
                    reasons.append(f"line {ln}: impure intrinsic '{intr}'")

            for m in CALL_RE.finditer(low):
                callee = m.group(1).lower()
                generic_targets = generics.get(callee, set())
                if callee in has_nonpure_name:
                    reasons.append(f"line {ln}: calls non-pure procedure '{callee}'")
                elif callee in external_nonpure:
                    reasons.append(f"line {ln}: calls known non-pure external procedure '{callee}'")
                elif generic_targets:
                    nonpure_targets = [t for t in generic_targets if t in ref_nonpure_names]
                    if nonpure_targets:
                        reasons.append(
                            f"line {ln}: generic '{callee}' resolves to non-pure procedure(s): "
                            + ", ".join(sorted(nonpure_targets))
                        )
                    elif strict_unknown_calls:
                        unknown_targets = [
                            t for t in generic_targets if t not in by_name and t not in external_known
                        ]
                        if unknown_targets:
                            reasons.append(
                                f"line {ln}: generic '{callee}' has unknown procedure(s): "
                                + ", ".join(sorted(unknown_targets))
                            )
                elif strict_unknown_calls and callee not in by_name and callee not in external_known:
                    reasons.append(f"line {ln}: call to unknown external procedure '{callee}'")

            m_assign = ASSIGN_RE.match(low)
            if m_assign:
                lhs_base = base_identifier(m_assign.group(1))
                if lhs_base and lhs_base not in local_names:
                    reasons.append(
                        f"line {ln}: assignment to non-local variable '{lhs_base}' (possible host/module state)"
                    )

            m_alloc = ALLOC_DEALLOC_RE.match(low)
            if m_alloc:
                inner = m_alloc.group(2)
                first_obj = inner.split(",", 1)[0].strip()
                obj_base = base_identifier(first_obj)
                if obj_base and obj_base not in local_names:
                    reasons.append(
                        f"line {ln}: {m_alloc.group(1).lower()} of non-local variable '{obj_base}'"
                    )

            for callee in ref_nonpure_names:
                if callee == proc.name.lower():
                    continue
                if has_function_reference(low, callee):
                    reasons.append(f"line {ln}: references non-pure function '{callee}'")

        if proc.kind == "subroutine":
            for d in sorted(proc.dummy_names):
                if d not in dummy_with_intent_or_value:
                    reasons.append(
                        f"dummy argument '{d}' lacks explicit INTENT/VALUE declaration (conservative pure check)"
                    )

        # Deduplicate while preserving order.
        deduped: List[str] = []
        seen = set()
        for r in reasons:
            if r not in seen:
                deduped.append(r)
                seen.add(r)

        if deduped:
            rejected.append((proc, deduped))
        else:
            candidates.append(proc)

    return AnalysisResult(procs, candidates, rejected)


def print_analysis(path: Path, result: AnalysisResult, show_rejections: bool = False) -> None:
    print(f"File: {display_path(path)}")
    n_candidates = len(result.candidates)
    print(f"\n{n_candidates} Likely PURE candidates (not currently marked pure/elemental):")
    if n_candidates > 0:
        for p in result.candidates:
            print(f"  - {p.kind} {p.name}  [lines {p.start}-{p.end}]")

    if show_rejections:
        print("\nRejected by heuristic:")
        if not result.rejected:
            print("  (none)")
        else:
            for p, reasons in result.rejected:
                print(f"  - {p.kind} {p.name}  [lines {p.start}-{p.end}]")
                for r in reasons[:6]:
                    print(f"      * {r}")
                if len(reasons) > 6:
                    print(f"      * ... {len(reasons) - 6} more")


def print_suggestion_list(path: Path, label: str, procs: List[Procedure]) -> None:
    print(f"File: {display_path(path)}")
    print(f"\n{len(procs)} {label}:")
    for p in procs:
        print(f"  - {p.kind} {p.name}  [lines {p.start}-{p.end}]")

def update_external_name_status(
    external_name_status: Dict[str, bool],
    result: AnalysisResult,
) -> None:
    candidate_ids = {id(p) for p in result.candidates}
    for proc in result.procedures:
        if proc.parent is not None:
            continue
        is_pure_now = proc.is_pure_or_elemental or (id(proc) in candidate_ids)
        name = proc.name.lower()
        if name not in external_name_status:
            external_name_status[name] = is_pure_now
        else:
            external_name_status[name] = external_name_status[name] and is_pure_now


def unique_names_by_kind(candidates: List[Procedure], kind: str) -> List[str]:
    names: List[str] = []
    seen: Set[str] = set()
    for p in candidates:
        if p.kind != kind:
            continue
        key = p.name.lower()
        if key in seen:
            continue
        seen.add(key)
        names.append(p.name)
    return names


def suggest_elemental_candidates(result: AnalysisResult) -> List[Procedure]:
    # Elemental needs pure semantics and scalar dummy arguments.
    pure_candidate_ids = {id(p) for p in result.candidates}
    out: List[Procedure] = []
    for proc in result.procedures:
        if "elemental" in proc.attrs:
            continue
        if proc.kind not in {"function", "subroutine"}:
            continue
        if len(proc.dummy_names) == 0:
            continue
        is_pure_or_pure_candidate = ("pure" in proc.attrs) or (id(proc) in pure_candidate_ids)
        if not is_pure_or_pure_candidate:
            continue

        declared: Set[str] = set()
        nonscalar: Set[str] = set()
        result_name = (proc.result_name or proc.name).lower()
        pointer_or_allocatable: Set[str] = set()
        for _, code in proc.body:
            low = code.lower()
            if not TYPE_DECL_RE.match(low):
                continue
            declared_here = parse_declared_names_from_decl(low)
            declared.update(declared_here)
            if "allocatable" in low or re.search(r"\bpointer\b", low):
                pointer_or_allocatable.update(declared_here)
            line_has_dimension = "dimension" in low
            for name, has_entity_paren in parse_declared_entities(low):
                if line_has_dimension or has_entity_paren:
                    nonscalar.add(name)

        # Conservative: every dummy must be explicitly declared and scalar.
        ok = True
        for d in proc.dummy_names:
            if d not in declared or d in nonscalar or d in pointer_or_allocatable:
                ok = False
                break
        # Elemental function result must be scalar as well.
        if ok and proc.kind == "function":
            if result_name not in declared or result_name in nonscalar:
                ok = False
            # Elemental function results cannot be ALLOCATABLE/POINTER.
            if result_name in pointer_or_allocatable:
                ok = False
        if ok:
            out.append(proc)
    return out


def add_changed_names(
    changed_summary: Dict[Tuple[str, str], List[str]],
    filename: str,
    changed_names: List[Tuple[str, str]],
) -> None:
    for kind, name in changed_names:
        key = (filename, kind)
        bucket = changed_summary.setdefault(key, [])
        if name.lower() not in {n.lower() for n in bucket}:
            bucket.append(name)


def print_changed_summary(
    changed_summary: Dict[Tuple[str, str], List[str]],
    attribute_label: str = "pure",
) -> None:
    if not changed_summary:
        return
    n_functions = sum(
        len(names) for (fname, kind), names in changed_summary.items() if kind == "function"
    )
    n_subroutines = sum(
        len(names) for (fname, kind), names in changed_summary.items() if kind == "subroutine"
    )
    nfiles = len({fname for (fname, _kind) in changed_summary.keys()})
    fun_word = "function" if n_functions == 1 else "functions"
    sub_word = "subroutine" if n_subroutines == 1 else "subroutines"
    file_word = "source file" if nfiles == 1 else "source files"
    print(
        f"\nSummary of {n_functions} {fun_word} and {n_subroutines} {sub_word} "
        f"marked {attribute_label} in {nfiles} {file_word}:"
    )
    for (filename, kind) in sorted(changed_summary.keys(), key=lambda x: (x[0].lower(), x[1])):
        names = changed_summary[(filename, kind)]
        if not names:
            continue
        label = kind + ("" if len(names) == 1 else "s")
        print(f"{filename} {len(names)} {label}: {' '.join(names)}")


def maybe_git_commit(
    do_git: bool,
    changed_files: Set[Path],
    changed_summary: Dict[Tuple[str, str], List[str]],
    attribute_label: str,
) -> None:
    if not do_git or not changed_files:
        return
    n_functions = sum(
        len(names) for (fname, kind), names in changed_summary.items() if kind == "function"
    )
    n_subroutines = sum(
        len(names) for (fname, kind), names in changed_summary.items() if kind == "subroutine"
    )
    n_files = len({fname for (fname, _k) in changed_summary.keys()})
    msg = (
        f"{attribute_label}: mark {n_functions} function(s) and "
        f"{n_subroutines} subroutine(s) in {n_files} file(s)"
    )
    fbuild.git_commit_files(sorted(changed_files, key=lambda p: str(p).lower()), msg, display_path)


def parse_only_selectors(only: str) -> Set[str]:
    out = set()
    for tok in only.split(","):
        t = tok.strip().lower()
        if t:
            out.add(t)
    return out


def choose_targets(candidates: List[Procedure], only: Optional[str], all_candidates: bool) -> Tuple[List[Procedure], List[str]]:
    if all_candidates:
        return candidates[:], []
    if not only:
        return [], []

    selectors = parse_only_selectors(only)
    chosen: List[Procedure] = []
    missing: List[str] = []
    for token in selectors:
        if "@" in token:
            matched = [p for p in candidates if p.selector.lower() == token]
        else:
            matched = [p for p in candidates if p.name.lower() == token]
        if not matched:
            missing.append(token)
            continue
        for p in matched:
            if p not in chosen:
                chosen.append(p)
    return chosen, missing


def split_code_comment(line: str) -> Tuple[str, str]:
    in_single = False
    in_double = False
    for i, ch in enumerate(line):
        if ch == "'" and not in_double:
            in_single = not in_single
        elif ch == '"' and not in_single:
            in_double = not in_double
        elif ch == "!" and not in_single and not in_double:
            return line[:i], line[i:]
    return line, ""


def add_pure_to_declaration(line: str) -> Tuple[str, bool]:
    code, comment = split_code_comment(line)
    m = DECL_RE.match(code)
    if not m:
        return line, False
    prefix = m.group("prefix") or ""
    attrs = {a.lower() for a in prefix.split()}
    if "pure" in attrs or "elemental" in attrs or "impure" in attrs:
        return line, False
    indent = m.group(1)
    kind = m.group("kind")
    tail = code[m.end("kind"):]
    new_code = f"{indent}pure {prefix}{kind}{tail}"
    return f"{new_code}{comment}", True


def add_elemental_to_declaration(line: str) -> Tuple[str, bool]:
    code, comment = split_code_comment(line)
    m = DECL_RE.match(code)
    if not m:
        return line, False
    prefix = m.group("prefix") or ""
    attrs = [a for a in prefix.split() if a]
    attrs_l = [a.lower() for a in attrs]
    if "elemental" in attrs_l:
        return line, False
    # This mode intentionally upgrades already-pure procedures to elemental.
    if "pure" not in attrs_l:
        return line, False
    filtered = [a for a in attrs if a.lower() != "pure"]
    indent = m.group(1)
    kind = m.group("kind")
    tail = code[m.end("kind"):]
    new_prefix = " ".join(["elemental"] + filtered)
    if new_prefix:
        new_prefix += " "
    new_code = f"{indent}{new_prefix}{kind}{tail}"
    return f"{new_code}{comment}", True


def apply_fix(
    path: Path,
    original_lines: List[str],
    targets: List[Procedure],
    backup: bool,
    show_diff: bool,
) -> Tuple[int, Optional[Path], List[Tuple[str, str]]]:
    if not targets:
        return 0, None, []

    updated = original_lines[:]
    changed = 0
    changed_names: List[Tuple[str, str]] = []
    for proc in targets:
        idx = proc.start - 1
        if idx < 0 or idx >= len(updated):
            continue
        new_line, did_change = add_pure_to_declaration(updated[idx])
        if did_change:
            updated[idx] = new_line
            changed += 1
            changed_names.append((proc.kind, proc.name))

    if changed == 0:
        return 0, None, []

    if show_diff:
        diff = difflib.unified_diff(
            original_lines,
            updated,
            fromfile=str(path),
            tofile=str(path),
            lineterm="",
        )
        print("\nProposed diff:")
        for line in diff:
            print(line)

    backup_path: Optional[Path] = None
    if backup:
        backup_path = path.with_name(path.name + ".bak")
        shutil.copy2(path, backup_path)
        print(f"\nBackup written: {backup_path}")

    with path.open("w", encoding="utf-8", newline="") as f:
        f.write("".join(updated))
    return changed, backup_path, changed_names


def apply_elemental_fix(
    path: Path,
    original_lines: List[str],
    targets: List[Procedure],
    backup: bool,
    show_diff: bool,
) -> Tuple[int, Optional[Path], List[Tuple[str, str]]]:
    if not targets:
        return 0, None, []

    updated = original_lines[:]
    changed = 0
    changed_names: List[Tuple[str, str]] = []
    for proc in targets:
        idx = proc.start - 1
        if idx < 0 or idx >= len(updated):
            continue
        new_line, did_change = add_elemental_to_declaration(updated[idx])
        if did_change:
            updated[idx] = new_line
            changed += 1
            changed_names.append((proc.kind, proc.name))

    if changed == 0:
        return 0, None, []

    if show_diff:
        diff = difflib.unified_diff(
            original_lines,
            updated,
            fromfile=str(path),
            tofile=str(path),
            lineterm="",
        )
        print("\nProposed diff:")
        for line in diff:
            print(line)

    backup_path: Optional[Path] = None
    if backup:
        backup_path = path.with_name(path.name + ".bak")
        shutil.copy2(path, backup_path)
        print(f"\nBackup written: {backup_path}")

    with path.open("w", encoding="utf-8", newline="") as f:
        f.write("".join(updated))
    return changed, backup_path, changed_names


def rollback_backups(backup_pairs: List[Tuple[Path, Path]]) -> None:
    fbuild.rollback_backups(backup_pairs, display_path)


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Suggest Fortran procedures that may be markable as PURE"
    )
    parser.add_argument(
        "fortran_files",
        type=Path,
        nargs="*",
        help="Path(s) to .f90/.f95 source file(s). If omitted, uses *.f90/*.F90 in current directory.",
    )
    parser.add_argument(
        "--show-rejections",
        action="store_true",
        help="Also print procedures rejected and why",
    )
    parser.add_argument(
        "--exclude",
        action="append",
        default=[],
        help="Glob pattern to exclude files (can be repeated)",
    )
    parser.add_argument(
        "--verbose",
        action="store_true",
        help="Print per-file analysis details (default prints summary only)",
    )
    parser.add_argument(
        "--fix",
        action="store_true",
        help="Rewrite source to add PURE to selected suggested procedures",
    )
    parser.add_argument(
        "--only",
        type=str,
        help="Comma-separated selectors to fix (name or name@line)",
    )
    parser.add_argument(
        "--all-candidates",
        action="store_true",
        help="With --fix, apply to all suggested candidates",
    )
    parser.add_argument(
        "--diff",
        action="store_true",
        help="With --fix, print a unified diff of proposed edits",
    )
    parser.add_argument(
        "--backup",
        dest="backup",
        action="store_true",
        default=True,
        help="With --fix, write a .bak backup before modifying (default)",
    )
    parser.add_argument(
        "--no-backup",
        dest="backup",
        action="store_false",
        help="With --fix, do not create a backup file",
    )
    parser.add_argument(
        "--git",
        action="store_true",
        help="Commit changed files to git after successful run",
    )
    parser.add_argument(
        "--compiler",
        type=str,
        help="Compilation command. Use {files} as placeholder for source files, or files are appended.",
    )
    parser.add_argument(
        "--strict-unknown-calls",
        action="store_true",
        help="Treat calls to unknown external procedures as purity blockers",
    )
    parser.add_argument(
        "--suggest-elemental",
        action="store_true",
        help="Suggest procedures that could be marked ELEMENTAL (heuristic, conservative)",
    )
    parser.add_argument(
        "--iterate",
        action="store_true",
        help="With --fix, repeat analyze/fix passes until no more changes",
    )
    parser.add_argument(
        "--max-iter",
        type=int,
        default=10,
        help="Maximum iterations for --iterate (default: 10)",
    )
    args = parser.parse_args()
    verbose = args.verbose or args.show_rejections

    if not args.fortran_files:
        auto_files = sorted(
            set(Path(".").glob("*.f90")) | set(Path(".").glob("*.F90")),
            key=lambda p: p.name.lower(),
        )
        if not auto_files:
            print("No source files provided and no .f90/.F90 files found in current directory.")
            return 2
        args.fortran_files = auto_files
        if verbose:
            print("No source files specified; using current directory files:")
            for p in args.fortran_files:
                print(f"  - {display_path(p)}")
    args.fortran_files = fscan.apply_excludes(args.fortran_files, args.exclude)
    if not args.fortran_files:
        print("No source files remain after applying --exclude filters.")
        return 2

    if args.iterate and not args.fix:
        print("--iterate requires --fix.")
        return 3
    if args.max_iter < 1:
        print("--max-iter must be >= 1.")
        return 3

    max_passes = args.max_iter if args.iterate else 1
    did_baseline_compile = False
    changed_summary: Dict[Tuple[str, str], List[str]] = {}
    changed_attr_label = "elemental" if args.suggest_elemental else "pure"
    changed_files: Set[Path] = set()

    for it in range(1, max_passes + 1):
        any_analyzed = False
        unmatched_tokens: Set[str] = parse_only_selectors(args.only) if args.only else set()
        external_name_status: Dict[str, bool] = {}
        summary_lines: List[str] = []
        backup_pairs: List[Tuple[Path, Path]] = []
        changed_total = 0

        source_files, any_missing_file = fscan.load_source_files(args.fortran_files)
        if not source_files:
            return 2 if any_missing_file else 1
        global_generics: Dict[str, Set[str]] = {}
        for finfo in source_files:
            for gname, targets in finfo.generic_interfaces.items():
                global_generics.setdefault(gname, set()).update(targets)

        ordered_files, had_cycle = fscan.order_files_least_dependent(source_files)
        if len(ordered_files) > 1:
            order_text = " ".join(display_path(finfo.path) for finfo in ordered_files)
            print(f"Processing order: {order_text}")
            if verbose and had_cycle:
                print("(dependency cycle detected; cycle members processed in path order)")
        compile_paths = [f.path for f in ordered_files]
        if args.compiler:
            compile_paths, unresolved_mods = fscan.build_compile_closure(ordered_files)
            if verbose and unresolved_mods:
                print(
                    "Unresolved modules for dependency closure: "
                    + " ".join(sorted(unresolved_mods))
                )

        if args.compiler and args.fix and not did_baseline_compile:
            if not run_compiler_command(args.compiler, compile_paths, phase="baseline"):
                return 5
            did_baseline_compile = True

        for idx, finfo in enumerate(ordered_files):
            if verbose and idx > 0:
                print("\n" + "=" * 72 + "\n")
            result = analyze_lines(
                finfo.parsed_lines,
                external_name_status=external_name_status,
                generic_interfaces=global_generics,
                strict_unknown_calls=args.strict_unknown_calls,
            )
            if not result.procedures:
                if verbose:
                    print(f"No procedures found in {display_path(finfo.path)}")
                continue
            any_analyzed = True

            elemental_suggestions: List[Procedure] = []
            if args.suggest_elemental:
                elemental_suggestions = suggest_elemental_candidates(result)

            if verbose:
                if args.suggest_elemental:
                    print_suggestion_list(
                        finfo.path,
                        "Likely ELEMENTAL candidates (not currently marked elemental)",
                        elemental_suggestions,
                    )
                else:
                    print_analysis(finfo.path, result, show_rejections=args.show_rejections)
            update_external_name_status(external_name_status, result)
            if not args.fix:
                base_for_summary = elemental_suggestions if args.suggest_elemental else result.candidates
                subroutine_names = unique_names_by_kind(base_for_summary, "subroutine")
                function_names = unique_names_by_kind(base_for_summary, "function")
                if subroutine_names:
                    summary_lines.append(
                        f"{display_path(finfo.path)} {len(subroutine_names)} subroutine"
                        f"{'' if len(subroutine_names) == 1 else 's'}: {' '.join(subroutine_names)}"
                    )
                if function_names:
                    summary_lines.append(
                        f"{display_path(finfo.path)} {len(function_names)} function"
                        f"{'' if len(function_names) == 1 else 's'}: {' '.join(function_names)}"
                    )

            if not args.fix:
                continue

            apply_all = args.all_candidates or not args.only
            base_targets = elemental_suggestions if args.suggest_elemental else result.candidates
            targets, _ = choose_targets(base_targets, args.only, apply_all)
            if args.suggest_elemental:
                targets = [t for t in targets if ("pure" in t.attrs and "elemental" not in t.attrs)]
            selected_here = {t.selector.lower() for t in targets} | {t.name.lower() for t in targets}
            unmatched_tokens -= {tok for tok in unmatched_tokens if tok in selected_here}

            if not targets:
                continue

            if args.suggest_elemental:
                changed, backup_path, changed_names = apply_elemental_fix(
                    finfo.path, finfo.lines, targets, backup=args.backup, show_diff=args.diff
                )
            else:
                changed, backup_path, changed_names = apply_fix(
                    finfo.path, finfo.lines, targets, backup=args.backup, show_diff=args.diff
                )
            changed_total += changed
            if backup_path is not None:
                backup_pairs.append((finfo.path, backup_path))
            if changed > 0:
                add_changed_names(changed_summary, display_path(finfo.path), changed_names)
                changed_files.add(finfo.path)
                print(f"\nApplied {changed_attr_label.upper()} to {changed} procedure declaration(s).")

        if any_missing_file:
            return 2
        if not any_analyzed:
            return 1
        if args.fix and args.only and unmatched_tokens and it == 1:
            print(f"\nUnmatched selector(s): {', '.join(sorted(unmatched_tokens))}")
            return 4
        if args.compiler:
            phase = "after-fix" if args.fix else "current"
            if not run_compiler_command(args.compiler, compile_paths, phase=phase):
                if args.fix and args.backup and backup_pairs:
                    rollback_backups(backup_pairs)
                return 5

        if not args.iterate:
            maybe_git_commit(args.git and args.fix, changed_files, changed_summary, changed_attr_label)
            if args.fix:
                print_changed_summary(changed_summary, attribute_label=changed_attr_label)
            elif summary_lines:
                print("\nSummary:")
                for line in summary_lines:
                    print(line)
            return 0

        if changed_total == 0:
            maybe_git_commit(args.git and args.fix, changed_files, changed_summary, changed_attr_label)
            if args.fix:
                print_changed_summary(changed_summary, attribute_label=changed_attr_label)
            elif summary_lines:
                print("\nSummary:")
                for line in summary_lines:
                    print(line)
            return 0
        if it == max_passes:
            print(f"Reached max iterations ({args.max_iter}).")
            maybe_git_commit(args.git and args.fix, changed_files, changed_summary, changed_attr_label)
            if args.fix:
                print_changed_summary(changed_summary, attribute_label=changed_attr_label)
            elif summary_lines:
                print("\nSummary:")
                for line in summary_lines:
                    print(line)
            return 0

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
