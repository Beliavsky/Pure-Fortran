#!/usr/bin/env python3
"""Shared Fortran source scanning utilities."""

from __future__ import annotations

import re
import fnmatch
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Set, Tuple

PROC_START_RE = re.compile(
    r"^\s*(?P<prefix>(?:(?:pure|elemental|impure|recursive|module)\s+)*)"
    r"(?P<kind>function|subroutine)\s+"
    r"(?P<name>[a-z][a-z0-9_]*)\s*(?P<arglist>\([^)]*\))?",
    re.IGNORECASE,
)

USE_RE = re.compile(
    r"^\s*use\b(?:\s*,\s*(?:non_intrinsic|intrinsic)\s*)?(?:\s*::\s*|\s+)([a-z][a-z0-9_]*)",
    re.IGNORECASE,
)
MODULE_DEF_RE = re.compile(r"^\s*module\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
INTERFACE_START_RE = re.compile(r"^\s*(abstract\s+)?interface\b(?:\s+([a-z][a-z0-9_]*))?", re.IGNORECASE)
END_INTERFACE_RE = re.compile(r"^\s*end\s+interface\b", re.IGNORECASE)
MODULE_PROCEDURE_RE = re.compile(r"^\s*module\s+procedure\b(.+)$", re.IGNORECASE)
CALL_RE = re.compile(r"\bcall\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)


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
        """is pure or elemental."""
        return "pure" in self.attrs or "elemental" in self.attrs

    @property
    def selector(self) -> str:
        """selector."""
        return f"{self.name}@{self.start}"


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
    """Return the short display form for a source path."""
    return path.name


def apply_excludes(paths: Iterable[Path], exclude_patterns: Iterable[str]) -> List[Path]:
    """Filter paths by glob-style exclusion patterns."""
    pats = [p for p in exclude_patterns if p]
    if not pats:
        return list(paths)

    kept: List[Path] = []
    for p in paths:
        full = str(p).replace("\\", "/")
        name = p.name
        excluded = False
        for pat in pats:
            patn = pat.replace("\\", "/")
            if fnmatch.fnmatch(full, patn) or fnmatch.fnmatch(name, patn):
                excluded = True
                break
        if not excluded:
            kept.append(p)
    return kept


def strip_comment(line: str) -> str:
    """Remove trailing Fortran comments while respecting quoted strings."""
    if line.startswith("\ufeff"):
        line = line[1:]
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
    """Parse procedure argument text into normalized dummy argument names."""
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
    """Extract declared entity names from a Fortran declaration statement."""
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
    """Extract declared names and whether each has an inline array spec."""
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
    """Return the base identifier at the start of an expression, if present."""
    m = re.match(r"^\s*([a-z][a-z0-9_]*)", expr, re.IGNORECASE)
    if not m:
        return None
    return m.group(1).lower()


def split_fortran_statements(code: str) -> List[str]:
    """Split code into semicolon-delimited statements, respecting quoted strings."""
    out: List[str] = []
    cur: List[str] = []
    in_single = False
    in_double = False
    for ch in code:
        if ch == "'" and not in_double:
            in_single = not in_single
        elif ch == '"' and not in_single:
            in_double = not in_double
        if ch == ";" and not in_single and not in_double:
            seg = "".join(cur).strip()
            if seg:
                out.append(seg)
            cur = []
        else:
            cur.append(ch)
    tail = "".join(cur).strip()
    if tail:
        out.append(tail)
    return out


def join_continued_lines(lines: Iterable[str]) -> List[Tuple[int, str]]:
    """Join free-form continuation lines and keep the originating start line."""
    out: List[Tuple[int, str]] = []
    cur_parts: List[str] = []
    cur_start: Optional[int] = None
    need_more = False

    for lineno, raw in enumerate(lines, start=1):
        code = strip_comment(raw).rstrip("\r\n")
        seg = code.rstrip()
        if not seg and not need_more:
            continue

        if cur_start is None:
            cur_start = lineno

        if cur_parts:
            lead = seg.lstrip()
            if lead.startswith("&"):
                seg = lead[1:].lstrip()

        seg = seg.rstrip()
        has_trailing_cont = seg.endswith("&")
        if has_trailing_cont:
            seg = seg[:-1].rstrip()

        if seg:
            cur_parts.append(seg)

        need_more = has_trailing_cont
        if need_more:
            continue

        joined = " ".join(cur_parts).strip()
        if joined:
            out.append((cur_start, joined))
        cur_parts = []
        cur_start = None

    if cur_parts and cur_start is not None:
        joined = " ".join(cur_parts).strip()
        if joined:
            out.append((cur_start, joined))
    return out


def iter_fortran_statements(lines: Iterable[str]) -> List[Tuple[int, str]]:
    """Return semicolon-split statements as (start_line, statement_text)."""
    out: List[Tuple[int, str]] = []
    for lineno, joined in join_continued_lines(lines):
        for stmt in split_fortran_statements(joined):
            if stmt:
                out.append((lineno, stmt))
    return out


def split_statements_to_lines(lines: Iterable[str]) -> List[str]:
    """Expand semicolon-delimited statements so each returned item is one statement."""
    out: List[str] = []
    for _lineno, stmt in iter_fortran_statements(lines):
        out.append(stmt)
    return out


def parse_procedures(lines: List[str]) -> List[Procedure]:
    """Parse procedure blocks and metadata from preprocessed source lines."""
    stack: List[Procedure] = []
    out: List[Procedure] = []
    interface_depth = 0

    for lineno, stmt in iter_fortran_statements(lines):
        low = stmt.lower().strip()

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
            stack[-1].body.append((lineno, stmt))

    while stack:
        top = stack.pop()
        top.end = len(lines)
        out.append(top)

    out.sort(key=lambda p: p.start)
    return out


def parse_modules_and_generics(lines: List[str]) -> Tuple[Set[str], Set[str], Dict[str, Set[str]]]:
    """Collect defined modules, used modules, and generic interface bindings."""
    defined: Set[str] = set()
    used: Set[str] = set()
    generics: Dict[str, Set[str]] = {}
    interface_depth = 0
    current_generic: Optional[str] = None
    current_is_abstract = False
    for _lineno, stmt in iter_fortran_statements(lines):
        low = stmt.strip().lower()
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
    """Load source files and return parsed metadata objects plus missing-file status."""
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
    """Infer inter-file dependencies from USE statements and procedure calls."""
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
    """Topologically order files so independent providers are processed first."""
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
    """Build ordered compile inputs by adding files that provide used modules."""
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
