#!/usr/bin/env python3
"""Wrap external procedures into modules and update main program USE statements."""

from __future__ import annotations

import argparse
import re
import shutil
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Sequence, Set, Tuple

import cli_paths as cpaths
import fortran_scan as fscan


SUB_START_RE = re.compile(
    r"^\s*(?:(?:pure|elemental|impure|recursive|module)\s+)*"
    r"subroutine\s+"
    r"(?P<name>[a-z][a-z0-9_]*)\b",
    re.IGNORECASE,
)
FUNC_START_RE = re.compile(
    r"^\s*(?!end\b)"
    r"(?:(?:double\s+precision|integer|real|logical|complex|character\b(?:\s*\([^)]*\))?"
    r"|type\s*\([^)]*\)|class\s*\([^)]*\))\s*(?:\([^)]*\))?\s*,?\s*)?"
    r"(?:(?:pure|elemental|impure|recursive|module)\s+)*"
    r"function\s+"
    r"(?P<name>[a-z][a-z0-9_]*)\b",
    re.IGNORECASE,
)
MODULE_START_RE = re.compile(r"^\s*module\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
MODULE_PROC_RE = re.compile(r"^\s*module\s+procedure\b", re.IGNORECASE)
END_MODULE_RE = re.compile(r"^\s*end\s*module\b", re.IGNORECASE)
INTERFACE_START_RE = re.compile(r"^\s*(abstract\s+)?interface\b", re.IGNORECASE)
END_INTERFACE_RE = re.compile(r"^\s*end\s+interface\b", re.IGNORECASE)
PROGRAM_START_RE = re.compile(r"^\s*program\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
END_PROGRAM_RE = re.compile(r"^\s*end\s*program\b", re.IGNORECASE)
USE_RE = re.compile(r"^\s*use\s*(?:::)?\s*([a-z][a-z0-9_]*)\b", re.IGNORECASE)
IMPLICIT_RE = re.compile(r"^\s*implicit\b", re.IGNORECASE)
CALL_STMT_RE = re.compile(r"^\s*call\s+([a-z][a-z0-9_]*)\s*(?:\((.*)\))?\s*$", re.IGNORECASE)
TYPE_DECL_RE = re.compile(
    r"^\s*(?:integer|real|double\s+precision|complex|logical|character|type\s*\([^)]*\)|class\s*\([^)]*\))\b",
    re.IGNORECASE,
)
NO_COLON_DECL_RE = re.compile(
    r"^\s*(?P<spec>(?:double\s+precision|integer|real|logical|complex|character(?:\s*\([^)]*\)|\s*\*\s*\d+)?|"
    r"type\s*\([^)]*\)|class\s*\([^)]*\))\s*(?:\([^)]*\))?)\s+(?P<rhs>.+)$",
    re.IGNORECASE,
)
EXTERNAL_RE = re.compile(r"^\s*external\b", re.IGNORECASE)
ASSUMED_LEN_CHAR_FUNC_RE = re.compile(
    r"^\s*character\s*(?:\(\s*len\s*=\s*\*\s*\)|\(\s*\*\s*\)|\*\s*\(\s*\*\s*\)|\*\s*\*)\s*function\b",
    re.IGNORECASE,
)


@dataclass
class FileInfo:
    path: Path
    lines: List[str]
    has_program: bool
    has_outside_procs: bool
    has_any_module: bool


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


def split_top_level_commas(text: str) -> List[str]:
    out: List[str] = []
    buf: List[str] = []
    depth = 0
    in_single = False
    in_double = False
    i = 0
    while i < len(text):
        ch = text[i]
        if ch == "'" and not in_double:
            in_single = not in_single
            buf.append(ch)
            i += 1
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            buf.append(ch)
            i += 1
            continue
        if not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")":
                depth = max(0, depth - 1)
            elif ch == "," and depth == 0:
                out.append("".join(buf).strip())
                buf = []
                i += 1
                continue
        buf.append(ch)
        i += 1
    out.append("".join(buf).strip())
    return [p for p in out if p]


def proc_start_name_and_kind(stmt_low: str) -> Optional[Tuple[str, str]]:
    m = SUB_START_RE.match(stmt_low)
    if m:
        return m.group("name").lower(), "subroutine"
    m = FUNC_START_RE.match(stmt_low)
    if m:
        return m.group("name").lower(), "function"
    return None


def choose_files(args_files: Sequence[Path], exclude: Iterable[str]) -> List[Path]:
    if args_files:
        files = cpaths.expand_path_args(args_files)
    else:
        files = sorted(Path(".").glob("*.f90"), key=lambda p: p.name.lower())
    out: List[Path] = []
    for p in files:
        if not p.exists():
            continue
        if any(p.match(pat) for pat in exclude):
            continue
        if p.suffix.lower() not in {".f90", ".f", ".for", ".f95", ".f03", ".f08"}:
            continue
        out.append(p)
    seen: Set[Path] = set()
    uniq: List[Path] = []
    for p in out:
        rp = p.resolve()
        if rp in seen:
            continue
        seen.add(rp)
        uniq.append(p)
    return uniq


def is_proc_end(stmt_low: str, expected_kind: str) -> bool:
    s = stmt_low.strip()
    if s == "end":
        return True
    if s in {"end function", "end subroutine"}:
        return s.endswith(expected_kind)
    if s == "endfunction":
        return expected_kind == "function"
    if s == "endsubroutine":
        return expected_kind == "subroutine"
    return False


def proc_start_kind(stmt_low: str) -> Optional[str]:
    if SUB_START_RE.match(stmt_low):
        return "subroutine"
    if FUNC_START_RE.match(stmt_low):
        return "function"
    return None


def outside_proc_names(lines: List[str]) -> List[str]:
    module_depth = 0
    interface_depth = 0
    stack: List[Tuple[str, bool, str]] = []  # kind, in_mod, name
    out: List[str] = []
    seen: Set[str] = set()
    for _lineno, stmt in fscan.iter_fortran_statements(lines):
        low = stmt.strip().lower()
        if not low:
            continue
        if INTERFACE_START_RE.match(low):
            interface_depth += 1
            continue
        if END_INTERFACE_RE.match(low):
            if interface_depth > 0:
                interface_depth -= 1
            continue
        if interface_depth == 0:
            if END_MODULE_RE.match(low):
                if module_depth > 0:
                    module_depth -= 1
                continue
            m_mod = MODULE_START_RE.match(low)
            if m_mod and not MODULE_PROC_RE.match(low) and not low.startswith("end"):
                module_depth += 1
                continue
            p = proc_start_name_and_kind(low)
            if p is not None:
                name, kind = p
                in_mod = module_depth > 0
                stack.append((kind, in_mod, name))
                continue
            if stack:
                kind, in_mod, name = stack[-1]
                if is_proc_end(low, kind):
                    stack.pop()
                    if not in_mod and name not in seen:
                        seen.add(name)
                        out.append(name)
    for kind, in_mod, name in stack:
        if not in_mod and name not in seen:
            seen.add(name)
            out.append(name)
    return out


def outside_proc_spans(lines: List[str]) -> List[Tuple[int, int]]:
    """Return (start_line, end_line) spans for top-level external procedures."""
    module_depth = 0
    interface_depth = 0
    stack: List[Tuple[str, bool, int]] = []  # kind, in_mod, start_line
    spans: List[Tuple[int, int]] = []
    for lineno, stmt in fscan.iter_fortran_statements(lines):
        low = stmt.strip().lower()
        if not low:
            continue
        if INTERFACE_START_RE.match(low):
            interface_depth += 1
            continue
        if END_INTERFACE_RE.match(low):
            if interface_depth > 0:
                interface_depth -= 1
            continue
        if interface_depth > 0:
            continue
        if END_MODULE_RE.match(low):
            if module_depth > 0:
                module_depth -= 1
            continue
        m_mod = MODULE_START_RE.match(low)
        if m_mod and not MODULE_PROC_RE.match(low) and not low.startswith("end"):
            module_depth += 1
            continue
        kind = proc_start_kind(low)
        if kind is not None:
            stack.append((kind, module_depth > 0, lineno))
            continue
        if stack:
            pkind, in_mod, start_line = stack[-1]
            if is_proc_end(low, pkind):
                stack.pop()
                if not in_mod:
                    spans.append((start_line, lineno))
    for _pkind, in_mod, start_line in stack:
        if not in_mod:
            spans.append((start_line, len(lines)))
    spans.sort()
    return spans


def all_proc_spans(lines: List[str]) -> List[Tuple[int, int]]:
    """Return (start_line, end_line) spans for all procedures (including module procedures)."""
    parsed = [ln.rstrip("\r\n") for ln in lines]
    procs = fscan.parse_procedures(parsed)
    spans: List[Tuple[int, int]] = []
    for p in procs:
        if p.start > 0 and p.end >= p.start:
            spans.append((p.start, p.end))
    spans.sort()
    return spans


def program_spans(lines: List[str]) -> List[Tuple[int, int]]:
    """Return (start_line, end_line) spans for top-level PROGRAM units."""
    module_depth = 0
    interface_depth = 0
    in_prog = False
    prog_start = -1
    spans: List[Tuple[int, int]] = []
    for lineno, stmt in fscan.iter_fortran_statements(lines):
        low = stmt.strip().lower()
        if not low:
            continue
        if INTERFACE_START_RE.match(low):
            interface_depth += 1
            continue
        if END_INTERFACE_RE.match(low):
            if interface_depth > 0:
                interface_depth -= 1
            continue
        if interface_depth > 0:
            continue
        if END_MODULE_RE.match(low):
            if module_depth > 0:
                module_depth -= 1
            continue
        m_mod = MODULE_START_RE.match(low)
        if m_mod and not MODULE_PROC_RE.match(low) and not low.startswith("end"):
            module_depth += 1
            continue
        if module_depth > 0:
            continue
        if not in_prog and PROGRAM_START_RE.match(low) and not END_PROGRAM_RE.match(low):
            in_prog = True
            prog_start = lineno
            continue
        if in_prog and (END_PROGRAM_RE.match(low) or low == "end"):
            spans.append((prog_start, lineno))
            in_prog = False
            prog_start = -1
    if in_prog and prog_start > 0:
        spans.append((prog_start, len(lines)))
    spans.sort()
    return spans


def has_outside_proc(lines: List[str]) -> bool:
    module_depth = 0
    interface_depth = 0
    stack: List[Tuple[str, bool]] = []
    for _lineno, stmt in fscan.iter_fortran_statements(lines):
        low = stmt.strip().lower()
        if not low:
            continue
        if INTERFACE_START_RE.match(low):
            interface_depth += 1
            continue
        if END_INTERFACE_RE.match(low):
            if interface_depth > 0:
                interface_depth -= 1
            continue
        if interface_depth == 0:
            if END_MODULE_RE.match(low):
                if module_depth > 0:
                    module_depth -= 1
                continue
            m_mod = MODULE_START_RE.match(low)
            if m_mod and not MODULE_PROC_RE.match(low) and not low.startswith("end"):
                module_depth += 1
                continue
            kind = proc_start_kind(low)
            if kind is not None:
                stack.append((kind, module_depth > 0))
                continue
            if stack:
                kind, in_mod = stack[-1]
                if is_proc_end(low, kind):
                    stack.pop()
                    if not in_mod:
                        return True
    return any(not in_mod for _k, in_mod in stack)


def sanitize_module_name(stem: str) -> str:
    name = re.sub(r"[^a-z0-9_]", "_", stem.lower())
    if not name:
        name = "mod"
    if not re.match(r"^[a-z]", name):
        name = f"m_{name}"
    if not name.endswith("_mod"):
        name = f"{name}_mod"
    return name


def detect_newline(lines: List[str]) -> str:
    for ln in lines:
        if ln.endswith("\r\n"):
            return "\r\n"
        if ln.endswith("\n"):
            return "\n"
    return "\n"


def wrap_in_module(lines: List[str], module_name: str) -> List[str]:
    nl = detect_newline(lines)
    body = [ln if ln.endswith(("\n", "\r\n")) else ln + nl for ln in lines]
    out: List[str] = []
    out.append(f"module {module_name}{nl}")
    out.append(f"implicit none{nl}")
    out.append(f"contains{nl}")
    if body and body[0].strip():
        out.append(nl)
    out.extend(body)
    if out and out[-1].strip():
        out.append(nl)
    out.append(f"end module {module_name}{nl}")
    return out


def has_program(lines: List[str]) -> bool:
    for _lineno, stmt in fscan.iter_fortran_statements(lines):
        low = stmt.strip().lower()
        if PROGRAM_START_RE.match(low) and not END_PROGRAM_RE.match(low):
            return True
    return False


def has_any_module(lines: List[str]) -> bool:
    for _lineno, stmt in fscan.iter_fortran_statements(lines):
        low = stmt.strip().lower()
        if MODULE_START_RE.match(low) and not MODULE_PROC_RE.match(low) and not low.startswith("end"):
            return True
    return False


def analyze_file(path: Path) -> FileInfo:
    text = path.read_text(encoding="utf-8", errors="ignore")
    lines = text.splitlines(keepends=True)
    return FileInfo(
        path=path,
        lines=lines,
        has_program=has_program(lines),
        has_outside_procs=has_outside_proc(lines),
        has_any_module=has_any_module(lines),
    )


def has_unsupported_assumed_len_char_function(lines: List[str]) -> bool:
    for _lineno, stmt in fscan.iter_fortran_statements(lines):
        s = stmt.strip()
        if not s:
            continue
        if ASSUMED_LEN_CHAR_FUNC_RE.match(s):
            return True
    parsed = [ln.rstrip("\r\n") for ln in lines]
    procs = fscan.parse_procedures(parsed)
    for p in procs:
        if p.kind.lower() != "function":
            continue
        start = max(0, p.start - 1)
        stop = min(len(lines), p.end)
        pname = p.name.lower()
        for _lineno, stmt in fscan.iter_fortran_statements(lines[start:stop]):
            s = stmt.strip()
            low = s.lower()
            if not low.startswith("character"):
                continue
            if not re.match(
                r"^\s*character\s*(?:\(\s*len\s*=\s*\*\s*\)|\(\s*\*\s*\)|\*\s*\(\s*\*\s*\)|\*\s*\*)",
                low,
            ):
                continue
            rhs = ""
            if "::" in s:
                _spec, rhs = s.split("::", 1)
            else:
                m_no = NO_COLON_DECL_RE.match(s)
                if m_no:
                    rhs = m_no.group("rhs")
            if not rhs:
                continue
            for ent in split_top_level_commas(rhs):
                m = re.match(r"^\s*([a-z][a-z0-9_]*)\b", ent, re.IGNORECASE)
                if m and m.group(1).lower() == pname:
                    return True
    return False


def has_proc_name_local_conflict(lines: List[str]) -> bool:
    """Detect likely conflicts when wrapping externals into one module.

    If a procedure name is used as a local variable name (or assignment target)
    in another procedure, module host association can turn that name into a
    procedure reference and break compilation.
    """
    parsed = [ln.rstrip("\r\n") for ln in lines]
    procs = fscan.parse_procedures(parsed)
    if not procs:
        return False
    proc_names = {p.name.lower() for p in procs}

    def iter_proc_statements(p) -> Iterable[str]:
        start = max(0, p.start - 1)
        stop = min(len(lines), p.end)
        for _lineno, stmt in fscan.iter_fortran_statements(lines[start:stop]):
            s = stmt.strip()
            if s:
                yield s

    for p in procs:
        self_name = p.name.lower()
        result_name = (p.result_name or "").lower()

        for s in iter_proc_statements(p):
            low = s.lower()

            if SUB_START_RE.match(low) or FUNC_START_RE.match(low):
                continue
            if low.startswith("end"):
                continue

            m_assign = re.match(r"^\s*([a-z][a-z0-9_]*)\s*=", low)
            if m_assign:
                lhs = m_assign.group(1).lower()
                if lhs in proc_names and lhs not in {self_name, result_name}:
                    return True

            if TYPE_DECL_RE.match(low):
                rhs = ""
                if "::" in s:
                    _spec, rhs = s.split("::", 1)
                else:
                    m_no = NO_COLON_DECL_RE.match(s)
                    if m_no:
                        rhs = m_no.group("rhs")
                if rhs:
                    for ent in split_top_level_commas(rhs):
                        m = re.match(r"^\s*([a-z][a-z0-9_]*)\b", ent, re.IGNORECASE)
                        if not m:
                            continue
                        nm = m.group(1).lower()
                        if nm in proc_names and nm not in {self_name, result_name}:
                            return True
    return False


def _parse_decl_entities_with_array(stmt: str) -> List[Tuple[str, bool]]:
    s = stmt.strip()
    if not TYPE_DECL_RE.match(s.lower()):
        return []
    rhs = ""
    if "::" in s:
        _spec, rhs = s.split("::", 1)
    else:
        m_no = NO_COLON_DECL_RE.match(s)
        if not m_no:
            return []
        rhs = m_no.group("rhs")
    out: List[Tuple[str, bool]] = []
    for ent in split_top_level_commas(rhs):
        lhs = ent.split("=", 1)[0].strip()
        m = re.match(r"^([a-z][a-z0-9_]*)(.*)$", lhs, re.IGNORECASE)
        if not m:
            continue
        out.append((m.group(1).lower(), "(" in m.group(2)))
    return out


def _proc_dummy_shape_by_position(lines: List[str]) -> Dict[str, List[Optional[bool]]]:
    """Return proc -> dummy list by position, where True=array, False=scalar, None=unknown."""
    out: Dict[str, List[Optional[bool]]] = {}
    parsed = [ln.rstrip("\r\n") for ln in lines]
    procs = fscan.parse_procedures(parsed)
    for p in procs:
        start = max(0, p.start - 1)
        stop = min(len(lines), p.end)
        proc_lines = lines[start:stop]
        arg_order: List[str] = []
        for _lineno, stmt in fscan.iter_fortran_statements(proc_lines):
            low = stmt.strip().lower()
            m_sub = SUB_START_RE.match(low)
            m_fun = FUNC_START_RE.match(low)
            if m_sub or m_fun:
                m = m_sub or m_fun
                m_arg = re.search(r"\((.*)\)", stmt)
                if m_arg:
                    for tok in split_top_level_commas(m_arg.group(1)):
                        nm = tok.strip().lower()
                        if re.match(r"^[a-z][a-z0-9_]*$", nm):
                            arg_order.append(nm)
                break
        if not arg_order:
            continue
        dshape: Dict[str, bool] = {}
        for _lineno, stmt in fscan.iter_fortran_statements(proc_lines):
            for nm, is_arr in _parse_decl_entities_with_array(stmt):
                dshape[nm] = is_arr
        out[p.name.lower()] = [dshape.get(a) if a in dshape else None for a in arg_order]
    return out


def _classify_actual_is_array(actual: str, decl_infos: Dict[str, Dict[str, bool]]) -> Optional[bool]:
    """Classify an actual argument as array/scalar when obvious.

    Returns:
      True: definitely array
      False: definitely scalar
      None: unknown
    """
    a = actual.strip()
    if not a:
        return None

    # Bare variable name.
    if re.match(r"^[a-z][a-z0-9_]*$", a, re.IGNORECASE):
        info = decl_infos.get(a.lower())
        if info is None:
            return None
        return bool(info.get("array", False))

    # Simple indexed/sectioned reference name(...)
    m_ref = re.match(r"^([a-z][a-z0-9_]*)\s*\((.*)\)$", a, re.IGNORECASE)
    if m_ref:
        base = m_ref.group(1).lower()
        subs = m_ref.group(2)
        info = decl_infos.get(base)
        # If base isn't a known declared variable in unit, this may be a function call.
        if info is None:
            return None
        if ":" in subs:
            return True
        return False

    # Numeric literals are scalar.
    if re.match(r"^[+-]?(?:\d+\.?\d*|\.\d+)(?:[deq][+-]?\d+)?(?:_[a-z0-9_]+)?$", a, re.IGNORECASE):
        return False

    return None


def has_array_to_scalar_call_mismatch(proc_lines: List[str], caller_lines: List[str]) -> bool:
    """Detect rank mismatches that appear only after explicit interfaces are introduced."""
    proc_shapes = _proc_dummy_shape_by_position(proc_lines)
    if not proc_shapes:
        return False
    unit_spans = sorted(program_spans(caller_lines) + outside_proc_spans(caller_lines))
    for sline, eline in unit_spans:
        start = max(0, sline - 1)
        stop = min(len(caller_lines), eline)
        unit_lines = caller_lines[start:stop]
        decl_infos = _decl_infos_in_unit(unit_lines)
        for _lineno, stmt in fscan.iter_fortran_statements(unit_lines):
            m_call = CALL_STMT_RE.match(stmt.strip())
            if not m_call:
                continue
            callee = m_call.group(1).lower()
            if callee not in proc_shapes:
                continue
            argtxt = m_call.group(2) or ""
            args = split_top_level_commas(argtxt) if argtxt.strip() else []
            dummies = proc_shapes[callee]
            n = min(len(args), len(dummies))
            for i in range(n):
                dshape = dummies[i]
                if dshape is None:
                    continue
                actual_shape = _classify_actual_is_array(args[i], decl_infos)
                if actual_shape is None:
                    continue
                # dummy scalar vs actual array
                if dshape is False and actual_shape is True:
                    return True
                # dummy array vs actual scalar
                if dshape is True and actual_shape is False:
                    return True
    return False


def has_interface_for_proc_names(lines: List[str], proc_names: Set[str]) -> bool:
    """Return True if explicit interface blocks mention any target procedure names."""
    if not proc_names:
        return False
    interface_depth = 0
    for _lineno, stmt in fscan.iter_fortran_statements(lines):
        low = stmt.strip().lower()
        if not low:
            continue
        if INTERFACE_START_RE.match(low):
            interface_depth += 1
            continue
        if END_INTERFACE_RE.match(low):
            if interface_depth > 0:
                interface_depth -= 1
            continue
        if interface_depth <= 0:
            continue
        p = proc_start_name_and_kind(low)
        if p is not None:
            name, _kind = p
            if name.lower() in proc_names:
                return True
    return False


def has_local_decl_conflict_with_proc_names(lines: List[str], proc_names: Set[str]) -> bool:
    """Return True when a unit locally declares a name that is also a proc name."""
    if not proc_names:
        return False
    for _lineno, stmt in fscan.iter_fortran_statements(lines):
        low = stmt.strip().lower()
        if not low:
            continue
        if EXTERNAL_RE.match(low):
            # Explicit external declarations are compatible with imported procedure names.
            continue
        for nm, _is_arr in _parse_decl_entities_with_array(stmt):
            if nm in proc_names:
                return True
    return False


def has_call_to_function_mismatch(proc_lines: List[str], caller_lines: List[str]) -> bool:
    """Detect CALL statements that target names defined as functions in proc_lines."""
    parsed = [ln.rstrip("\r\n") for ln in proc_lines]
    funcs = {p.name.lower() for p in fscan.parse_procedures(parsed) if p.kind.lower() == "function"}
    if not funcs:
        return False
    for _lineno, stmt in fscan.iter_fortran_statements(caller_lines):
        m_call = CALL_STMT_RE.match(stmt.strip())
        if not m_call:
            continue
        callee = m_call.group(1).lower()
        if callee in funcs:
            return True
    return False


def module_already_used(lines: List[str], module_name: str) -> bool:
    tgt = module_name.lower()
    for raw in lines:
        stmt = fscan.strip_comment(raw).strip()
        if not stmt:
            continue
        m = USE_RE.match(stmt.lower())
        if m and m.group(1).lower() == tgt:
            return True
    return False


def rewrite_decl_remove_names(raw: str, names: Set[str]) -> Tuple[str, bool]:
    body = raw.rstrip("\r\n")
    eol = raw[len(body) :]
    code, comment = split_code_comment(body)
    if not TYPE_DECL_RE.match(code.strip()):
        return raw, False
    if "::" in code:
        spec, rhs = code.split("::", 1)
        head = f"{spec.strip()} :: "
    else:
        m_no = NO_COLON_DECL_RE.match(code.strip())
        if not m_no:
            return raw, False
        head = m_no.group("spec").strip() + " "
        rhs = m_no.group("rhs")
    ents = split_top_level_commas(rhs)
    kept: List[str] = []
    removed = False
    for ent in ents:
        lhs = ent.split("=", 1)[0].strip()
        m = re.match(r"^([a-z][a-z0-9_]*)\b", lhs, re.IGNORECASE)
        if m and m.group(1).lower() in names:
            removed = True
            continue
        kept.append(ent)
    if not removed:
        return raw, False
    if not kept:
        return "", True
    indent = re.match(r"^\s*", code).group(0)
    out = indent + head + ", ".join(kept)
    if comment:
        out += comment
    return out + eol, True


def rewrite_external_remove_names(raw: str, names: Set[str]) -> Tuple[str, bool]:
    body = raw.rstrip("\r\n")
    eol = raw[len(body) :]
    code, comment = split_code_comment(body)
    if not EXTERNAL_RE.match(code.strip()):
        return raw, False
    rest = re.sub(r"^\s*external\s*(?:::)?\s*", "", code, flags=re.IGNORECASE)
    parts = split_top_level_commas(rest)
    kept: List[str] = []
    removed = False
    for p in parts:
        m = re.match(r"^\s*([a-z][a-z0-9_]*)\s*$", p, re.IGNORECASE)
        if m and m.group(1).lower() in names:
            removed = True
            continue
        kept.append(p.strip())
    if not removed:
        return raw, False
    if not kept:
        return "", True
    indent = re.match(r"^\s*", code).group(0)
    out = indent + "external :: " + ", ".join(kept)
    if comment:
        out += comment
    return out + eol, True


def remove_imported_proc_decls(lines: List[str], imported_proc_names: Set[str]) -> List[str]:
    if not imported_proc_names:
        return lines[:]
    out = lines[:]
    parsed = [ln.rstrip("\r\n") for ln in out]
    procs = fscan.parse_procedures(parsed)
    proc_by_span: Dict[Tuple[int, int], Set[str]] = {}
    for p in procs:
        protect = {p.name.lower()}
        if p.result_name:
            protect.add(p.result_name.lower())
        proc_by_span[(p.start, p.end)] = protect

    spans = program_spans(out) + all_proc_spans(out)

    def in_continuation(idx: int) -> bool:
        cur = out[idx].rstrip("\r\n")
        cur_code = fscan.strip_comment(cur).rstrip()
        if cur_code.endswith("&"):
            return True
        if idx > 0:
            prev = out[idx - 1].rstrip("\r\n")
            prev_code = fscan.strip_comment(prev).rstrip()
            if prev_code.endswith("&"):
                return True
        stripped = out[idx].lstrip()
        if stripped.startswith("&"):
            return True
        return False

    for sline, eline in spans:
        sidx = max(0, sline - 1)
        eidx = min(len(out), eline)
        protect = proc_by_span.get((sline, eline), set())
        names_here = set(imported_proc_names) - protect
        if not names_here:
            continue
        for i in range(sidx, eidx):
            raw = out[i]
            if not in_continuation(i):
                new_raw, ch = rewrite_decl_remove_names(raw, names_here)
                if ch:
                    out[i] = new_raw
                    continue
            new_raw, ch = rewrite_external_remove_names(raw, names_here)
            if ch:
                out[i] = new_raw
    return out


def _entity_name_and_tail(ent: str) -> Tuple[Optional[str], str]:
    lhs = ent.split("=", 1)[0].strip()
    m = re.match(r"^([a-z][a-z0-9_]*)(.*)$", lhs, re.IGNORECASE)
    if not m:
        return None, ""
    return m.group(1).lower(), m.group(2)


def _decl_infos_in_unit(unit_lines: List[str]) -> Dict[str, Dict[str, bool]]:
    infos: Dict[str, Dict[str, bool]] = {}
    for _lineno, stmt in fscan.iter_fortran_statements(unit_lines):
        s = stmt.strip()
        if not s:
            continue
        low = s.lower()
        if EXTERNAL_RE.match(low):
            rest = re.sub(r"^\s*external\s*(?:::)?\s*", "", s, flags=re.IGNORECASE)
            for p in split_top_level_commas(rest):
                m = re.match(r"^\s*([a-z][a-z0-9_]*)\s*$", p, re.IGNORECASE)
                if not m:
                    continue
                nm = m.group(1).lower()
                cur = infos.setdefault(
                    nm,
                    {"declared": False, "external": False, "initializer": False, "array": False, "parameter": False},
                )
                cur["external"] = True
            continue

        if not TYPE_DECL_RE.match(low):
            continue
        spec_low = ""
        rhs = ""
        if "::" in s:
            spec, rhs = s.split("::", 1)
            spec_low = spec.lower()
        else:
            m_no = NO_COLON_DECL_RE.match(s)
            if not m_no:
                continue
            spec_low = m_no.group("spec").lower()
            rhs = m_no.group("rhs")
        is_param = "parameter" in spec_low
        for ent in split_top_level_commas(rhs):
            nm, tail = _entity_name_and_tail(ent)
            if not nm:
                continue
            cur = infos.setdefault(
                nm,
                {"declared": False, "external": False, "initializer": False, "array": False, "parameter": False},
            )
            cur["declared"] = True
            cur["parameter"] = cur["parameter"] or is_param
            cur["initializer"] = cur["initializer"] or ("=" in ent)
            cur["array"] = cur["array"] or ("(" in tail)
    return infos


def remove_imported_proc_decls_by_unit(lines: List[str], span_to_imported: Dict[Tuple[int, int], Set[str]]) -> List[str]:
    if not span_to_imported:
        return lines[:]
    out = lines[:]
    parsed = [ln.rstrip("\r\n") for ln in out]
    procs = fscan.parse_procedures(parsed)
    proc_by_span: Dict[Tuple[int, int], Set[str]] = {}
    for p in procs:
        protect = {p.name.lower()}
        if p.result_name:
            protect.add(p.result_name.lower())
        proc_by_span[(p.start, p.end)] = protect

    def in_continuation(idx: int) -> bool:
        cur = out[idx].rstrip("\r\n")
        cur_code = fscan.strip_comment(cur).rstrip()
        if cur_code.endswith("&"):
            return True
        if idx > 0:
            prev = out[idx - 1].rstrip("\r\n")
            prev_code = fscan.strip_comment(prev).rstrip()
            if prev_code.endswith("&"):
                return True
        stripped = out[idx].lstrip()
        if stripped.startswith("&"):
            return True
        return False

    for (sline, eline), imported_proc_names in span_to_imported.items():
        if not imported_proc_names:
            continue
        sidx = max(0, sline - 1)
        eidx = min(len(out), eline)
        protect = proc_by_span.get((sline, eline), set())
        names_here = set(imported_proc_names) - protect
        if not names_here:
            continue
        for i in range(sidx, eidx):
            raw = out[i]
            if not in_continuation(i):
                new_raw, ch = rewrite_decl_remove_names(raw, names_here)
                if ch:
                    out[i] = new_raw
                    continue
            new_raw, ch = rewrite_external_remove_names(raw, names_here)
            if ch:
                out[i] = new_raw
    return out


def insert_use_lines(lines: List[str], module_to_procs: List[Tuple[str, List[str]]]) -> List[str]:
    if not module_to_procs:
        return lines[:]
    nl = detect_newline(lines)
    out = lines[:]
    new_uses: List[str] = []
    imported: Set[str] = set()
    for mod, procs in module_to_procs:
        imported.update(p.lower() for p in procs)
        if module_already_used(out, mod):
            continue
        if procs:
            new_uses.extend(format_use_only_lines(mod, procs, nl))
        else:
            new_uses.append(f"use {mod}{nl}")
    spans = sorted(program_spans(out) + outside_proc_spans(out))
    if not spans:
        return out
    span_to_imported: Dict[Tuple[int, int], Set[str]] = {}
    for sline, eline in reversed(spans):
        start = max(0, sline - 1)
        stop = min(len(out), eline)
        unit_lines = out[start:stop]
        decl_infos = _decl_infos_in_unit(unit_lines)
        unit_new_uses: List[str] = []
        unit_imported: Set[str] = set()
        for mod, procs in module_to_procs:
            if module_already_used(unit_lines, mod):
                continue
            if procs:
                filtered: List[str] = []
                for p in procs:
                    info = decl_infos.get(p.lower())
                    if info and not info.get("external", False):
                        if info.get("initializer", False) or info.get("array", False) or info.get("parameter", False):
                            continue
                    filtered.append(p)
                if not filtered:
                    continue
                unit_imported.update(p.lower() for p in filtered)
                unit_new_uses.extend(format_use_only_lines(mod, filtered, nl))
            else:
                unit_new_uses.append(f"use {mod}{nl}")
        if not unit_new_uses:
            continue
        insert_at = start + 1
        last_use = -1
        implicit_idx = -1
        for i in range(start + 1, stop):
            stmt = fscan.strip_comment(out[i]).strip()
            low = stmt.lower()
            if not stmt:
                continue
            if USE_RE.match(low):
                last_use = i
                continue
            if IMPLICIT_RE.match(low):
                implicit_idx = i
                break
            if implicit_idx < 0:
                implicit_idx = i
            break
        if last_use >= 0:
            insert_at = last_use + 1
        elif implicit_idx >= 0:
            insert_at = implicit_idx
        out[insert_at:insert_at] = unit_new_uses
        span_to_imported[(sline, eline)] = unit_imported
    return remove_imported_proc_decls_by_unit(out, span_to_imported)


def format_use_only_lines(mod: str, names: List[str], nl: str, max_len: int = 120) -> List[str]:
    """Format USE, ONLY list with continuation so lines stay within max_len."""
    if not names:
        return [f"use {mod}{nl}"]
    prefix = f"use {mod}, only: "
    lines: List[str] = []
    current = prefix
    for i, name in enumerate(names):
        token = name
        if i < len(names) - 1:
            token += ", "
        if len(current) + len(token) > max_len and current != prefix:
            lines.append(current.rstrip() + " &" + nl)
            current = "  " + token
        else:
            current += token
    lines.append(current.rstrip() + nl)
    return lines


def write_out(path: Path, out_dir: Path, lines: List[str]) -> Path:
    out_dir.mkdir(parents=True, exist_ok=True)
    dst = out_dir / path.name
    dst.write_text("".join(lines), encoding="utf-8")
    return dst


def build_compile_cmd(spec: str, files: List[Path]) -> str:
    base = spec.strip() if spec.strip() else "gfortran"
    files_part = " ".join(f'"{str(p)}"' for p in files)
    if "{files}" in base:
        return base.replace("{files}", files_part)
    return f"{base} {files_part}".strip()


def print_text_safe(text: str) -> None:
    """Print text safely even when console encoding cannot represent some chars."""
    if not text:
        return
    try:
        print(text)
        return
    except UnicodeEncodeError:
        pass

    enc = sys.stdout.encoding or "utf-8"
    data = text.encode(enc, errors="replace")
    if sys.stdout.buffer:
        sys.stdout.buffer.write(data + b"\n")
        sys.stdout.flush()
    else:
        print(data.decode(enc, errors="replace"))


def run_compile(label: str, spec: str, files: List[Path]) -> bool:
    cmd = build_compile_cmd(spec, files)
    print(f"Compile ({label}): {cmd}")
    cp = subprocess.run(cmd, shell=True, capture_output=True, text=True, errors="replace")
    if cp.returncode == 0:
        print(f"Compile ({label}): PASS")
        return True
    print(f"Compile ({label}): FAIL (exit {cp.returncode})")
    if cp.stderr.strip():
        print_text_safe(cp.stderr.strip())
    elif cp.stdout.strip():
        print_text_safe(cp.stdout.strip())
    return False


def run_executable(exe: Path) -> bool:
    print(f"Run: {exe}")
    cp = subprocess.run(str(exe), shell=True, capture_output=True, text=True, errors="replace")
    if cp.returncode != 0:
        print(f"Run: FAIL (exit {cp.returncode})")
        if cp.stderr.strip():
            print_text_safe(cp.stderr.strip())
        elif cp.stdout.strip():
            print_text_safe(cp.stdout.strip())
        return False
    print("Run: PASS")
    if cp.stdout.strip():
        print_text_safe(cp.stdout.rstrip())
    return True


def build_and_run_set(
    *,
    label: str,
    mains: List[Path],
    supports: List[Path],
    out_dir: Path,
    suffix: str,
) -> bool:
    for mainf in mains:
        exe_path = out_dir / f"{mainf.stem}_{suffix}.exe"
        files_part = " ".join(f'"{str(p)}"' for p in (supports + [mainf]))
        cmd = f'gfortran {files_part} -o "{str(exe_path)}"'
        print(f"Build exe ({label}): {cmd}")
        cp = subprocess.run(cmd, shell=True, capture_output=True, text=True, errors="replace")
        if cp.returncode != 0:
            print(f"Build exe ({label}): FAIL (exit {cp.returncode})")
            if cp.stderr.strip():
                print_text_safe(cp.stderr.strip())
            elif cp.stdout.strip():
                print_text_safe(cp.stdout.strip())
            return False
        print(f"Build exe ({label}): PASS")
        print(f"Run ({label}): {exe_path}")
        rp = subprocess.run(str(exe_path), shell=True, capture_output=True, text=True, errors="replace")
        if rp.returncode != 0:
            print(f"Run ({label}): FAIL (exit {rp.returncode})")
            if rp.stderr.strip():
                print_text_safe(rp.stderr.strip())
            elif rp.stdout.strip():
                print_text_safe(rp.stdout.strip())
            return False
        print(f"Run ({label}): PASS")
        if rp.stdout.strip():
            print_text_safe(rp.stdout.rstrip())
    return True


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Convert external-procedure files to modules and update main-program USE statements"
    )
    parser.add_argument("fortran_files", type=Path, nargs="+")
    parser.add_argument("--out-dir", type=Path, required=True, help="Output directory for transformed sources")
    parser.add_argument("--module-name", type=str, help="Override module name (only valid with one procedure file)")
    parser.add_argument(
        "--compile",
        nargs="?",
        const="",
        default=None,
        metavar="CMD",
        help='Compile baseline and transformed file sets. Bare --compile uses "gfortran <files>".',
    )
    parser.add_argument(
        "--compile-cmd",
        type=str,
        default=None,
        metavar="CMD",
        help='Compiler command template; implies --compile. Supports optional "{files}" placeholder.',
    )
    parser.add_argument("--run", action="store_true", help="Build and run each transformed main-program executable")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--verbose", action="store_true")
    args = parser.parse_args()
    compile_enabled = args.compile is not None or args.compile_cmd is not None
    compile_spec = args.compile_cmd if args.compile_cmd is not None else (args.compile or "")

    files = choose_files(args.fortran_files, args.exclude)
    if not files:
        print("No source files found.")
        return 2

    infos = [analyze_file(p) for p in files]
    main_infos = [fi for fi in infos if fi.has_program]
    all_proc_infos = [fi for fi in infos if fi.has_outside_procs and not fi.has_program]
    skipped_proc_infos: List[FileInfo] = []
    proc_infos: List[FileInfo] = []
    for fi in all_proc_infos:
        fi_proc_names = set(outside_proc_names(fi.lines))
        if has_unsupported_assumed_len_char_function(fi.lines):
            skipped_proc_infos.append(fi)
        elif has_proc_name_local_conflict(fi.lines):
            skipped_proc_infos.append(fi)
        elif any(has_local_decl_conflict_with_proc_names(mi.lines, fi_proc_names) for mi in main_infos):
            skipped_proc_infos.append(fi)
        elif any(has_interface_for_proc_names(mi.lines, fi_proc_names) for mi in main_infos):
            skipped_proc_infos.append(fi)
        elif any(has_call_to_function_mismatch(fi.lines, mi.lines) for mi in main_infos):
            skipped_proc_infos.append(fi)
        elif any(has_array_to_scalar_call_mismatch(fi.lines, mi.lines) for mi in main_infos):
            skipped_proc_infos.append(fi)
        else:
            proc_infos.append(fi)
    if not all_proc_infos:
        print("No external-procedure source file detected.")
        return 2
    if not main_infos:
        print("No main program source file detected.")
        return 2
    if skipped_proc_infos:
        names = ", ".join(fi.path.name for fi in skipped_proc_infos)
        print(
            "Note: skipped modularizing file(s) with unsupported module conversion patterns "
            "(assumed-length character function result, procedure-name/local-name conflicts, "
            "caller local-declaration conflicts, explicit-interface conflicts, "
            f"call/function mismatches, or argument-rank mismatches): {names}"
        )
    if args.module_name and len(proc_infos) != 1:
        print("--module-name is only supported when exactly one external-procedure file is detected.")
        return 2
    if compile_enabled:
        if not run_compile("baseline", compile_spec, files):
            return 5

    module_map: Dict[Path, str] = {}
    proc_names_map: Dict[Path, List[str]] = {}
    for fi in proc_infos:
        if args.module_name:
            mod = sanitize_module_name(args.module_name)
        else:
            mod = sanitize_module_name(fi.path.stem)
        module_map[fi.path] = mod
        proc_names_map[fi.path] = outside_proc_names(fi.lines)

    out_dir = args.out_dir
    out_dir.mkdir(parents=True, exist_ok=True)
    out_files: List[Path] = []
    out_by_src: Dict[Path, Path] = {}

    for fi in infos:
        out_lines = fi.lines
        if fi.path in module_map:
            out_lines = wrap_in_module(fi.lines, module_map[fi.path])
            moved_names = set(proc_names_map.get(fi.path, []))
            if moved_names:
                out_lines = remove_imported_proc_decls(out_lines, moved_names)
            if args.verbose:
                print(f"{fi.path.name}: wrapped procedures in module {module_map[fi.path]}")
        elif fi.has_program:
            mods = [(module_map[p.path], proc_names_map.get(p.path, [])) for p in proc_infos]
            out_lines = insert_use_lines(fi.lines, mods)
            if args.verbose:
                print(f"{fi.path.name}: inserted USE for {', '.join(m for m, _ in mods)}")
        dst = write_out(fi.path, out_dir, out_lines)
        out_files.append(dst)
        out_by_src[fi.path.resolve()] = dst
        if not args.verbose:
            print(f"Wrote {dst}")

    print(
        f"Done. Wrote {len(infos)} file(s) to {out_dir} "
        f"({len(proc_infos)} modularized, {len(main_infos)} main program file(s) updated)."
    )
    if compile_enabled:
        if not run_compile("after-fix", compile_spec, out_files):
            return 5
    if args.run:
        main_srcs = [fi.path.resolve() for fi in main_infos]
        main_src_set = set(main_srcs)
        orig_mains = [p for p in files if p.resolve() in main_src_set]
        orig_support = [p for p in files if p.resolve() not in main_src_set]
        if not build_and_run_set(
            label="baseline",
            mains=orig_mains,
            supports=orig_support,
            out_dir=out_dir,
            suffix="orig",
        ):
            return 5

        mod_mains: List[Path] = []
        mod_support: List[Path] = []
        for p in files:
            rp = p.resolve()
            outp = out_by_src.get(rp)
            if outp is None:
                continue
            if rp in main_src_set:
                mod_mains.append(outp)
            else:
                mod_support.append(outp)
        if not build_and_run_set(
            label="transformed",
            mains=mod_mains,
            supports=mod_support,
            out_dir=out_dir,
            suffix="mod",
        ):
            return 5
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
