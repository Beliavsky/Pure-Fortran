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


@dataclass
class DeadStoreEdits:
    """Conservative edit actions for set-but-never-read local variables."""

    decl_remove_by_line: Dict[int, Set[str]] = field(default_factory=dict)  # 1-based
    remove_stmt_lines: Set[int] = field(default_factory=set)  # 1-based


def display_path(path: Path) -> str:
    """Return the short display form for a source path."""
    return path.name


def read_text_flexible(path: Path) -> str:
    """Read text with fallback encodings for legacy source files."""
    for enc in ("utf-8", "utf-8-sig", "cp1252", "latin-1"):
        try:
            return path.read_text(encoding=enc)
        except UnicodeDecodeError:
            continue
    return path.read_text(encoding="utf-8", errors="replace")


def count_loc(
    path: Path,
    *,
    exclude_blank: bool = True,
    exclude_comment: bool = True,
) -> int:
    """Count lines of code in a source file with configurable exclusions."""
    try:
        text = read_text_flexible(path)
    except Exception:
        return 0
    count = 0
    for raw in text.splitlines():
        stripped = raw.strip()
        if exclude_blank and not stripped:
            continue
        code = strip_comment(raw).strip()
        if exclude_comment and not code:
            continue
        count += 1
    return count


def print_loc_summary_table(
    rows: List[Tuple[str, int, int, int]],
    *,
    source_col: str = "source",
    blocks_col: str = "blocks_rep",
    compile_old: Optional[Dict[str, Optional[bool]]] = None,
    compile_new: Optional[Dict[str, Optional[bool]]] = None,
) -> None:
    """Print aligned LOC summary rows: source, old/new/diff/ratio, blocks."""
    include_compile = (compile_old is not None) or (compile_new is not None)
    headers = [source_col, "lines_old", "lines_new", "diff", "ratio", blocks_col]
    if include_compile:
        headers.extend(["compile_old", "compile_new"])
    formatted: List[List[str]] = []
    for src, old_loc, new_loc, blocks in rows:
        diff = old_loc - new_loc
        ratio = "inf" if new_loc == 0 else f"{(old_loc / new_loc):.2f}"
        rec = [src, str(old_loc), str(new_loc), str(diff), ratio, str(blocks)]
        if include_compile:
            def _fmt(v: Optional[bool]) -> str:
                if v is None:
                    return "NA"
                return "True" if v else "False"

            rec.append(_fmt((compile_old or {}).get(src)))
            rec.append(_fmt((compile_new or {}).get(src)))
        formatted.append(rec)

    widths = [len(h) for h in headers]
    for r in formatted:
        for i, cell in enumerate(r):
            if len(cell) > widths[i]:
                widths[i] = len(cell)

    print("  ".join(headers[i].ljust(widths[i]) if i == 0 else headers[i].rjust(widths[i]) for i in range(len(headers))))
    for r in formatted:
        print(
            "  ".join(
                r[i].ljust(widths[i]) if i == 0 else r[i].rjust(widths[i])
                for i in range(len(r))
            )
        )


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


def split_fortran_units_simple(text: str) -> List[Dict[str, object]]:
    """Split source into simple top-level units (function/program).

    Returned entries are dicts with keys:
    - `kind`: `"function"`, `"subroutine"` or `"program"`
    - `name`: unit name (lowercase)
    - `args`: list of dummy names (for function; may be empty)
    - `result`: function result name or ``None``
    - `body_lines`: raw body lines between header and end
    """
    lines = [ln.rstrip("\r\n") for ln in text.splitlines()]
    stmts = iter_fortran_statements(lines)
    out: List[Dict[str, object]] = []
    i = 0
    module_depth = 0
    while i < len(stmts):
        hdr_lineno, code = stmts[i]
        low = code.lower()
        if re.match(r"^module\s+[a-z_]\w*\b", low) and not low.startswith("module procedure"):
            module_depth += 1
            i += 1
            continue
        if re.match(r"^end\s+module\b", low):
            module_depth = max(0, module_depth - 1)
            i += 1
            continue
        if low == "contains":
            i += 1
            continue
        m_fun = re.match(
            r"^(?:(?:pure|elemental|impure|recursive|module)\s+)*(?:(?:integer|real(?:\s*\([^)]*\))?|logical|character(?:\s*\([^)]*\))?|complex(?:\s*\([^)]*\))?|double\s+precision)\s+)?function\s+([a-z_]\w*)\s*\(([^)]*)\)\s*(?:result\s*\(\s*([a-z_]\w*)\s*\))?\s*$",
            low,
            re.IGNORECASE,
        )
        m_sub = re.match(
            r"^(?:(?:pure|elemental|impure|recursive|module)\s+)*subroutine\s+([a-z_]\w*)\s*\(([^)]*)\)\s*$",
            low,
            re.IGNORECASE,
        )
        m_prog = re.match(r"^program\s+([a-z_]\w*)\s*$", low, re.IGNORECASE)
        if m_fun:
            name = m_fun.group(1)
            args = [a.strip() for a in m_fun.group(2).split(",") if a.strip()]
            result = m_fun.group(3).strip() if m_fun.group(3) else None
            j = i + 1
            body: List[str] = []
            body_line_nos: List[int] = []
            while j < len(stmts):
                lineno_j, stmt_j = stmts[j]
                c = stmt_j.strip().lower()
                if re.match(r"^end\s+function\b", c) or c == "end":
                    break
                if stmt_j:
                    body.append(stmt_j)
                    body_line_nos.append(lineno_j)
                j += 1
            out.append(
                {
                    "kind": "function",
                    "name": name,
                    "args": args,
                    "result": result,
                    "body_lines": body,
                    "body_line_nos": body_line_nos,
                    "header_line_no": hdr_lineno,
                    "body_start_line_no": body_line_nos[0] if body_line_nos else (hdr_lineno + 1),
                    "source_lines": lines,
                }
            )
            i = j + 1
            continue
        if m_sub:
            name = m_sub.group(1)
            args = [a.strip() for a in m_sub.group(2).split(",") if a.strip()]
            j = i + 1
            body: List[str] = []
            body_line_nos: List[int] = []
            while j < len(stmts):
                lineno_j, stmt_j = stmts[j]
                c = stmt_j.strip().lower()
                if re.match(r"^end\s+subroutine\b", c) or c == "end":
                    break
                if stmt_j:
                    body.append(stmt_j)
                    body_line_nos.append(lineno_j)
                j += 1
            out.append(
                {
                    "kind": "subroutine",
                    "name": name,
                    "args": args,
                    "result": None,
                    "body_lines": body,
                    "body_line_nos": body_line_nos,
                    "header_line_no": hdr_lineno,
                    "body_start_line_no": body_line_nos[0] if body_line_nos else (hdr_lineno + 1),
                    "source_lines": lines,
                }
            )
            i = j + 1
            continue
        if m_prog:
            name = m_prog.group(1)
            j = i + 1
            body = []
            body_line_nos: List[int] = []
            while j < len(stmts):
                lineno_j, stmt_j = stmts[j]
                c = stmt_j.strip().lower()
                if re.match(r"^end\s+program\b", c) or c == "end":
                    break
                if stmt_j:
                    body.append(stmt_j)
                    body_line_nos.append(lineno_j)
                j += 1
            out.append(
                {
                    "kind": "program",
                    "name": name,
                    "args": [],
                    "result": None,
                    "body_lines": body,
                    "body_line_nos": body_line_nos,
                    "header_line_no": hdr_lineno,
                    "body_start_line_no": body_line_nos[0] if body_line_nos else (hdr_lineno + 1),
                    "source_lines": lines,
                }
            )
            i = j + 1
            continue
        # Implicit main program (no PROGRAM statement): consume top-level
        # executable/declaration statements until bare END / END PROGRAM.
        if module_depth == 0 and low != "contains":
            j = i
            body: List[str] = []
            body_line_nos: List[int] = []
            while j < len(stmts):
                lineno_j, stmt_j = stmts[j]
                c = stmt_j.strip().lower()
                if re.match(r"^end\s+program\b", c) or c == "end":
                    break
                if stmt_j:
                    body.append(stmt_j)
                    body_line_nos.append(lineno_j)
                j += 1
            if body:
                out.append(
                    {
                        "kind": "program",
                        "name": "main",
                        "args": [],
                        "result": None,
                        "body_lines": body,
                        "body_line_nos": body_line_nos,
                        "header_line_no": hdr_lineno,
                        "body_start_line_no": body_line_nos[0] if body_line_nos else hdr_lineno,
                        "source_lines": lines,
                    }
                )
                i = j + 1
                continue
        i += 1
    return out


def find_implicit_none_undeclared_identifiers(
    text: str,
    *,
    known_procedure_names: Optional[Set[str]] = None,
) -> List[str]:
    """Find likely undeclared identifiers in units under `implicit none`.

    This is a lightweight, statement-based check intended for tooling gates.
    """
    units = split_fortran_units_simple(text)
    known = {n.lower() for n in (known_procedure_names or set())}
    has_any_implicit_none = re.search(
        r"^\s*implicit\s+none\b", text, re.IGNORECASE | re.MULTILINE
    ) is not None
    errs: List[str] = []

    keywords = {
        "do", "end", "if", "then", "else", "call", "print", "write", "read",
        "open", "close", "result", "function", "program", "module", "contains",
        "use", "only", "implicit", "none", "intent", "in", "out", "inout", "return",
        "real", "integer", "logical", "character", "complex", "type", "class",
        "kind", "len", "parameter", "optional", "double", "precision", "select", "case", "default",
        "save", "external", "dimension", "allocatable", "exit", "stop", "and", "or", "not",
        "true", "false",
    }
    intrinsics = {
        "sqrt", "real", "sum", "size", "kind", "max", "min", "sin", "cos", "tan",
        "abs", "exp", "log", "random_number", "random_seed", "present", "product", "epsilon",
        "reshape", "spread", "pack", "count", "norm2",
        "int8", "int16", "int32", "int64", "real32", "real64", "real128",
    }

    declish_re = re.compile(
        r"^\s*(?:integer|real|logical|character|complex|type\b|class\b|double\s+precision)\b",
        re.IGNORECASE,
    )
    use_only_re = re.compile(
        r"^\s*use\b.*\bonly\s*:\s*(.+)$",
        re.IGNORECASE,
    )

    def _names_from_use_only(code: str) -> Set[str]:
        m = use_only_re.match(code)
        out_n: Set[str] = set()
        if not m:
            return out_n
        rhs = m.group(1)
        for part in rhs.split(","):
            p = part.strip()
            if not p:
                continue
            if "=>" in p:
                p = p.split("=>", 1)[0].strip()
            n = re.match(r"^([a-z_]\w*)$", p, re.IGNORECASE)
            if n:
                out_n.add(n.group(1).lower())
        return out_n

    def _host_declared_before_unit(u: Dict[str, object]) -> Set[str]:
        """Collect names declared in containing module spec-part before CONTAINS."""
        src = list(u.get("source_lines", []))
        hline = int(u.get("header_line_no", 0))
        if not src or hline <= 0:
            return set()
        # nearest enclosing module start before unit
        mod_start = None
        for i in range(hline - 2, -1, -1):
            c = strip_comment(src[i]).strip().lower()
            if re.match(r"^module\s+[a-z_]\w*\b", c) and not c.startswith("module procedure"):
                mod_start = i
                break
        if mod_start is None:
            return set()
        # module contains location before unit header
        contains_i = None
        for i in range(mod_start + 1, hline - 1):
            c = strip_comment(src[i]).strip().lower()
            if c == "contains":
                contains_i = i
                break
        if contains_i is None:
            return set()
        out_n: Set[str] = set()
        for i in range(mod_start + 1, contains_i):
            c = strip_comment(src[i]).strip()
            if not c:
                continue
            if declish_re.match(c):
                out_n.update(parse_declared_names_from_decl(c))
            out_n.update(_names_from_use_only(c))
        return out_n

    def _strip_string_literals(s: str) -> str:
        out: List[str] = []
        in_single = False
        in_double = False
        i = 0
        while i < len(s):
            ch = s[i]
            if ch == "'" and not in_double:
                if in_single and i + 1 < len(s) and s[i + 1] == "'":
                    out.append("  ")
                    i += 2
                    continue
                in_single = not in_single
                out.append(" ")
            elif ch == '"' and not in_single:
                in_double = not in_double
                out.append(" ")
            else:
                out.append(" " if (in_single or in_double) else ch)
            i += 1
        return "".join(out)
    for u in units:
        body = list(u.get("body_lines", []))
        line_nos = list(u.get("body_line_nos", []))
        has_local_implicit_none = any(strip_comment(s).strip().lower() == "implicit none" for s in body)
        implicit_none_on = has_local_implicit_none or has_any_implicit_none
        if not implicit_none_on:
            continue

        declared: Set[str] = set()
        arg_names = {a.lower() for a in u.get("args", [])}
        imported: Set[str] = set()
        host_declared = _host_declared_before_unit(u)
        for idx, stmt in enumerate(body):
            code = strip_comment(stmt).strip()
            if not code:
                continue
            imported.update(_names_from_use_only(code))
            if declish_re.match(code):
                declared.update(parse_declared_names_from_decl(code))
        for a in sorted(arg_names):
            if a not in declared:
                errs.append(
                    f"{u['kind']} {u['name']}: undeclared dummy argument '{a}' with implicit none"
                )
        declared |= arg_names
        # In Fortran functions without explicit RESULT(...), the function name
        # is the implicit result variable and may appear on assignment LHS.
        if str(u.get("kind", "")).lower() == "function":
            result_name = str(u.get("result") or "").strip().lower()
            if result_name:
                declared.add(result_name)
            else:
                declared.add(str(u.get("name", "")).lower())
        known_decl = declared | imported | host_declared

        # Validate declaration-spec identifiers (e.g. kind=dp, x(n)).
        for idx, stmt in enumerate(body):
            code = strip_comment(stmt).strip()
            if not code or "::" not in code or not declish_re.match(code):
                continue
            line_no = line_nos[idx] if idx < len(line_nos) else -1
            lhs, rhs = code.split("::", 1)
            scan_txt = _strip_string_literals(lhs + " " + rhs)
            kw_arg_names = {m.group(1).lower() for m in re.finditer(r"\b([a-z_]\w*)\s*=", scan_txt, flags=re.IGNORECASE)}
            for tok in re.findall(r"\b[a-z_]\w*\b", scan_txt, flags=re.IGNORECASE):
                t = tok.lower()
                if t in kw_arg_names:
                    continue
                if t in known_decl or t in keywords or t in intrinsics:
                    continue
                if re.fullmatch(r"[de]\d*", t):
                    continue
                errs.append(
                    f"{u['kind']} {u['name']}:{line_no} undeclared identifier '{t}' in declaration with implicit none"
                )
                break

        for idx, stmt in enumerate(body):
            code = strip_comment(stmt).strip()
            low = code.lower()
            if not code:
                continue
            if low in {"implicit none", "contains"} or low.startswith("use "):
                continue
            if low.startswith("allocate(") or low.startswith("allocate ("):
                continue
            if low.startswith("deallocate(") or low.startswith("deallocate ("):
                continue
            if "::" in code and declish_re.match(code):
                continue
            line_no = line_nos[idx] if idx < len(line_nos) else -1

            m_do = re.match(r"^do\s+([a-z_]\w*)\s*=", code, re.IGNORECASE)
            if m_do:
                v = m_do.group(1).lower()
                if v not in known_decl:
                    errs.append(
                        f"{u['kind']} {u['name']}:{line_no} undeclared loop variable '{v}' with implicit none"
                    )
            m_asn = re.match(r"^([a-z_]\w*)\b(?:\s*\([^)]*\))?\s*=\s*(.+)$", code, re.IGNORECASE)
            if m_asn:
                lhs = m_asn.group(1).lower()
                if lhs not in known_decl and lhs not in keywords:
                    errs.append(
                        f"{u['kind']} {u['name']}:{line_no} undeclared variable '{lhs}' on assignment LHS with implicit none"
                    )

            call_callee: Optional[str] = None
            m_call = re.match(r"^call\s+([a-z_]\w*)\s*\(", code, re.IGNORECASE)
            if m_call:
                call_callee = m_call.group(1).lower()
            else:
                m_if_call = re.match(r"^if\s*\(.+\)\s*call\s+([a-z_]\w*)\s*\(", code, re.IGNORECASE)
                if m_if_call:
                    call_callee = m_if_call.group(1).lower()

            scan_txt = _strip_string_literals(code)
            kw_arg_names = {m.group(1).lower() for m in re.finditer(r"\b([a-z_]\w*)\s*=", scan_txt, flags=re.IGNORECASE)}
            for tok in re.findall(r"\b[a-z_]\w*\b", scan_txt, flags=re.IGNORECASE):
                t = tok.lower()
                if call_callee and t == call_callee:
                    continue
                if t in known_decl or t in keywords or t in intrinsics or t in known:
                    continue
                if t in kw_arg_names:
                    continue
                if re.fullmatch(r"[de]\d*", t):
                    continue
                errs.append(
                    f"{u['kind']} {u['name']}:{line_no} undeclared identifier '{t}' with implicit none"
                )
                break
    return errs


def validate_fortran_basic_statements(text: str) -> List[str]:
    """Return unrecognized-statement diagnostics for a basic free-form subset."""
    errs: List[str] = []
    lines = text.splitlines()
    unit_stack: List[Tuple[str, str, int]] = []  # (kind, name, start_line)
    in_implicit_main = False

    def _balanced_parens(s: str) -> bool:
        depth = 0
        in_single = False
        in_double = False
        i = 0
        while i < len(s):
            ch = s[i]
            if ch == "'" and not in_double:
                if in_single and i + 1 < len(s) and s[i + 1] == "'":
                    i += 2
                    continue
                in_single = not in_single
            elif ch == '"' and not in_single:
                in_double = not in_double
            elif not in_single and not in_double:
                if ch == "(":
                    depth += 1
                elif ch == ")":
                    depth -= 1
                    if depth < 0:
                        return False
            i += 1
        return depth == 0 and not in_single and not in_double

    for lineno, stmt in iter_fortran_statements(lines):
        s = stmt.strip()
        low = s.lower()
        if not s:
            continue
        if not _balanced_parens(s):
            errs.append(f"line {lineno}: unbalanced parentheses in statement: {s}")
            continue
        if low == "end":
            if unit_stack:
                unit_stack.pop()
            elif in_implicit_main:
                in_implicit_main = False
            else:
                errs.append(f"line {lineno}: unexpected end")
            continue
        m_mod = re.match(r"^module\s+([a-z_]\w*)\b", low)
        if m_mod:
            unit_stack.append(("module", m_mod.group(1).lower(), lineno))
            continue
        m_end_mod = re.match(r"^end\s+module(?:\s+([a-z_]\w*))?\b", low)
        if m_end_mod:
            end_name = (m_end_mod.group(1) or "").lower()
            if unit_stack and unit_stack[-1][0] == "module":
                _, start_name, _start_line = unit_stack.pop()
                if end_name and end_name != start_name:
                    errs.append(
                        f"line {lineno}: mismatched end module name '{end_name}' (expected '{start_name}')"
                    )
            else:
                errs.append(f"line {lineno}: unexpected end module")
            continue
        if low == "contains":
            continue
        m_prog = re.match(r"^program\s+([a-z_]\w*)\b", low)
        if m_prog:
            unit_stack.append(("program", m_prog.group(1).lower(), lineno))
            continue
        m_end_prog = re.match(r"^end\s+program(?:\s+([a-z_]\w*))?\b", low)
        if m_end_prog:
            end_name = (m_end_prog.group(1) or "").lower()
            if unit_stack and unit_stack[-1][0] == "program":
                _, start_name, _start_line = unit_stack.pop()
                if end_name and end_name != start_name:
                    errs.append(
                        f"line {lineno}: mismatched end program name '{end_name}' (expected '{start_name}')"
                    )
            else:
                errs.append(f"line {lineno}: unexpected end program")
            continue
        m_fun = re.match(
            r"^(?:(?:pure|elemental|impure|recursive|module)\s+)*(?:(?:integer|real(?:\s*\([^)]*\))?|logical|character(?:\s*\([^)]*\))?|complex(?:\s*\([^)]*\))?|double\s+precision)\s+)?function\s+[a-z_]\w*\s*\([^)]*\)\s*(?:result\s*\(\s*[a-z_]\w*\s*\))?\s*$",
            low,
        )
        if m_fun:
            m_name = re.match(
                r"^(?:(?:pure|elemental|impure|recursive|module)\s+)*(?:(?:integer|real(?:\s*\([^)]*\))?|logical|character(?:\s*\([^)]*\))?|complex(?:\s*\([^)]*\))?|double\s+precision)\s+)?function\s+([a-z_]\w*)\b",
                low,
            )
            if m_name:
                unit_stack.append(("function", m_name.group(1).lower(), lineno))
            continue
        m_end_fun = re.match(r"^end\s+function(?:\s+([a-z_]\w*))?\b", low)
        if m_end_fun:
            end_name = (m_end_fun.group(1) or "").lower()
            if unit_stack and unit_stack[-1][0] == "function":
                _, start_name, _start_line = unit_stack.pop()
                if end_name and end_name != start_name:
                    errs.append(
                        f"line {lineno}: mismatched end function name '{end_name}' (expected '{start_name}')"
                    )
            else:
                errs.append(f"line {lineno}: unexpected end function")
            continue
        m_sub = re.match(
            r"^(?:(?:pure|elemental|impure|recursive|module)\s+)*subroutine\s+([a-z_]\w*)\s*\([^)]*\)\s*$",
            low,
        )
        if m_sub:
            unit_stack.append(("subroutine", m_sub.group(1).lower(), lineno))
            continue
        m_end_sub = re.match(r"^end\s+subroutine(?:\s+([a-z_]\w*))?\b", low)
        if m_end_sub:
            end_name = (m_end_sub.group(1) or "").lower()
            if unit_stack and unit_stack[-1][0] == "subroutine":
                _, start_name, _start_line = unit_stack.pop()
                if end_name and end_name != start_name:
                    errs.append(
                        f"line {lineno}: mismatched end subroutine name '{end_name}' (expected '{start_name}')"
                    )
            else:
                errs.append(f"line {lineno}: unexpected end subroutine")
            continue
        if low == "implicit none":
            if not unit_stack:
                in_implicit_main = True
            continue
        if re.match(r"^use\b", low):
            if not unit_stack:
                in_implicit_main = True
            continue
        if re.match(
            r"^(?:integer(?:\s*\([^)]*\))?|real(?:\s*\([^)]*\))?|logical|character(?:\s*\([^)]*\))?|complex(?:\s*\([^)]*\))?|type(?:\s*\([^)]*\))?|class(?:\s*\([^)]*\))?|double\s+precision)(?=\s|,|::|$)(?:(?:\s*,\s*[^:]*)?\s*::\s*.+|\s+.+)$",
            low,
        ):
            if not unit_stack:
                in_implicit_main = True
            continue
        if re.match(r"^do\s+[a-z_]\w*\s*=\s*.+$", low):
            if not unit_stack:
                in_implicit_main = True
            continue
        if low == "do":
            if not unit_stack:
                in_implicit_main = True
            continue
        if low == "end do":
            if not unit_stack:
                in_implicit_main = True
            continue
        if low == "exit":
            if not unit_stack:
                in_implicit_main = True
            continue
        if low == "return":
            if not unit_stack:
                in_implicit_main = True
            continue
        if re.match(r"^stop(?:\s*\(\s*[^)]*\s*\)|\s+.+)?\s*$", low):
            if not unit_stack:
                in_implicit_main = True
            continue
        if re.match(r"^call\s+random_number(?:\s*\(.*\))?\s*$", low):
            if not unit_stack:
                in_implicit_main = True
            continue
        if re.match(r"^allocate\s*\(\s*[a-z_]\w*\s*\([^)]*\)\s*\)\s*$", low):
            if not unit_stack:
                in_implicit_main = True
            continue
        if re.match(r"^deallocate\s*\(\s*[a-z_]\w*(?:\s*,\s*[a-z_]\w*)*\s*\)\s*$", low):
            if not unit_stack:
                in_implicit_main = True
            continue
        if re.match(r"^call\s+[a-z_]\w*(?:\s*\(.*\))?\s*$", low):
            if not unit_stack:
                in_implicit_main = True
            continue
        if re.match(r"^if\s*\(.+\)\s*return\s*$", low):
            if not unit_stack:
                in_implicit_main = True
            continue
        if re.match(r"^if\s*\(.+\)\s*then\s*$", low):
            if not unit_stack:
                in_implicit_main = True
            continue
        if re.match(r"^else\s+if\s*\(.+\)\s*then\s*$", low):
            if not unit_stack:
                in_implicit_main = True
            continue
        if low == "else":
            if not unit_stack:
                in_implicit_main = True
            continue
        if re.match(r"^end\s+if\s*$", low):
            if not unit_stack:
                in_implicit_main = True
            continue
        if re.match(r"^if\s*\(.+\)\s*call\s+[a-z_]\w*(?:\s*\(.*\))?\s*$", low):
            if not unit_stack:
                in_implicit_main = True
            continue
        if re.match(r"^if\s*\(.+\)\s*[a-z_]\w*(?:\s*\([^)]*\))?\s*=\s*.+$", low):
            if not unit_stack:
                in_implicit_main = True
            continue
        if re.match(r"^select\s+case\s*\(.+\)\s*$", low):
            if not unit_stack:
                in_implicit_main = True
            continue
        if re.match(r"^case\s*\(.+\)\s*$", low):
            if not unit_stack:
                in_implicit_main = True
            continue
        if re.match(r"^case\s+default\s*$", low):
            if not unit_stack:
                in_implicit_main = True
            continue
        if re.match(r"^end\s+select\s*$", low):
            if not unit_stack:
                in_implicit_main = True
            continue
        if re.match(r"^print\s*\*\s*,", low):
            if not unit_stack:
                in_implicit_main = True
            continue
        if re.match(r"^write\s*\(.*\)\s*(?:,\s*.+|\s+.+)?$", low):
            if not unit_stack:
                in_implicit_main = True
            continue
        if re.match(r"^[a-z_]\w*(?:\s*\([^)]*\))?\s*=\s*.+$", low):
            if not unit_stack:
                in_implicit_main = True
            continue
        errs.append(f"line {lineno}: unrecognized statement: {s}")
    for kind, name, start_line in reversed(unit_stack):
        errs.append(f"line {start_line}: unterminated {kind} '{name}'")
    return errs


def find_duplicate_procedure_definitions(text: str) -> List[str]:
    """Detect duplicate function/subroutine definitions in the same parent scope."""
    lines = text.splitlines()
    procs = parse_procedures(lines)
    by_key: Dict[Tuple[str, str, str], List[Procedure]] = {}
    for p in procs:
        parent = (p.parent or "").lower()
        key = (parent, p.name.lower(), p.kind.lower())
        by_key.setdefault(key, []).append(p)
    errs: List[str] = []
    for (parent, name, kind), items in by_key.items():
        if len(items) <= 1:
            continue
        locs = ", ".join(str(p.start) for p in sorted(items, key=lambda q: q.start))
        if parent:
            errs.append(
                f"duplicate {kind} definition '{name}' in scope '{parent}' at lines {locs}"
            )
        else:
            errs.append(f"duplicate {kind} definition '{name}' at lines {locs}")
    return errs


def find_duplicate_declarations(text: str) -> List[str]:
    """Detect duplicate declarations of entities within a unit declaration section."""
    units = split_fortran_units_simple(text)
    errs: List[str] = []
    declish_re = re.compile(
        r"^\s*(?:integer|real|logical|character|complex|type\b|class\b|double\s+precision)\b",
        re.IGNORECASE,
    )
    for u in units:
        body = list(u.get("body_lines", []))
        line_nos = list(u.get("body_line_nos", []))
        first_decl_line: Dict[str, int] = {}
        seen_exec = False
        for idx, stmt in enumerate(body):
            code = strip_comment(stmt).strip()
            if not code:
                continue
            low = code.lower()
            if seen_exec:
                break
            if low == "implicit none" or low.startswith("use "):
                continue
            if "::" in code and declish_re.match(code):
                rhs = code.split("::", 1)[1]
                entities = _split_top_level_commas(rhs)
                line_no = line_nos[idx] if idx < len(line_nos) else -1
                for ent in entities:
                    ent0 = ent.strip()
                    if not ent0:
                        continue
                    if "=" in ent0 and "=>" not in ent0:
                        ent0 = ent0.split("=", 1)[0].strip()
                    if "=>" in ent0:
                        ent0 = ent0.split("=>", 1)[0].strip()
                    m = re.match(r"^([a-z][a-z0-9_]*)", ent0, re.IGNORECASE)
                    if not m:
                        continue
                    nm = m.group(1).lower()
                    if nm in first_decl_line:
                        errs.append(
                            f"{u['kind']} {u['name']}:{line_no} duplicate declaration of '{nm}' (first at line {first_decl_line[nm]})"
                        )
                    else:
                        first_decl_line[nm] = line_no
                continue
            # first non-declaration statement ends declaration section
            seen_exec = True
    return errs


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
    if "::" in line:
        rhs = line.split("::", 1)[1]
    else:
        # Support old-style declarations without `::`, e.g.:
        #   double precision x
        #   real(kind=dp) a, b
        m = re.match(
            r"^\s*(?:integer(?:\s*\([^)]*\))?|real(?:\s*\([^)]*\))?|logical|character(?:\s*\([^)]*\))?|complex(?:\s*\([^)]*\))?|double\s+precision|type\s*\([^)]*\)|class\s*\([^)]*\))(?:\s*,\s*[^:]*)?\s+(.+)$",
            line,
            re.IGNORECASE,
        )
        if not m:
            return set()
        rhs = m.group(1)
    out: Set[str] = set()
    for chunk in _split_top_level_commas(rhs):
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


def _is_wrapped_by_outer_parens(expr: str) -> bool:
    """True if expr is fully wrapped by one outer (...) pair."""
    s = expr.strip()
    if len(s) < 2 or s[0] != "(" or s[-1] != ")":
        return False
    depth = 0
    in_single = False
    in_double = False
    for i, ch in enumerate(s):
        if ch == "'" and not in_double:
            in_single = not in_single
        elif ch == '"' and not in_single:
            in_double = not in_double
        elif not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
                if depth == 0 and i != len(s) - 1:
                    return False
                if depth < 0:
                    return False
    return depth == 0


def strip_redundant_outer_parens_expr(expr: str) -> str:
    """Remove redundant full-expression outer parentheses conservatively.

    Examples:
      '((n <= 0))' -> 'n <= 0'
      '(a + b)'    -> 'a + b'
      'a*(b+c)'    -> unchanged
    """
    s = expr.strip()
    while _is_wrapped_by_outer_parens(s):
        s = s[1:-1].strip()
    return s


def simplify_redundant_parens_in_stmt(stmt: str) -> str:
    """Conservatively simplify redundant outer parens in common statement forms."""
    s = stmt
    m_if = re.match(r"^(\s*if\s*\()(.+?)(\)\s*then\s*)$", s, re.IGNORECASE)
    if m_if:
        cond = strip_redundant_outer_parens_expr(m_if.group(2))
        return f"{m_if.group(1)}{cond}{m_if.group(3)}"
    m_do_while = re.match(r"^(\s*do\s+while\s*\()(.+?)(\)\s*)$", s, re.IGNORECASE)
    if m_do_while:
        cond = strip_redundant_outer_parens_expr(m_do_while.group(2))
        return f"{m_do_while.group(1)}{cond}{m_do_while.group(3)}"
    m_asn = re.match(r"^(\s*[^!]*?=\s*)(.+)$", s)
    if m_asn:
        rhs = strip_redundant_outer_parens_expr(m_asn.group(2))
        return f"{m_asn.group(1)}{rhs}"
    return s


def _can_drop_inline_parens(expr: str) -> bool:
    """Conservatively decide whether `(expr)` can be inlined in arithmetic."""
    s = expr.strip()
    if not s:
        return False
    # Keep conservative: avoid cases with + or - inside, commas, relations, logicals.
    if re.search(r"[,+<>=]|\.and\.|\.or\.|\.not\.", s, re.IGNORECASE):
        return False
    if "+" in s or "-" in s:
        return False
    return True


def simplify_redundant_parens_in_line(line: str) -> str:
    """Simplify redundant parentheses in one Fortran source line conservatively."""
    code, comment = _split_code_comment(line.rstrip("\r\n"))
    eol = _line_eol(line)
    s = simplify_redundant_parens_in_stmt(code)
    if re.match(r"^\s*if\s*\(.+\)\s*then\s*$", s, re.IGNORECASE):
        return f"{s}{comment}{eol}"
    if re.match(r"^\s*do\s+while\s*\(.+\)\s*$", s, re.IGNORECASE):
        return f"{s}{comment}{eol}"

    # Remove parentheses around simple atoms globally.
    atom_pat = re.compile(
        r"(?:(?<=^)|(?<=[\s=,+\-*/(\[]))"
        r"\(\s*([a-z][a-z0-9_]*|[+\-]?[0-9]+(?:\.[0-9]*)?(?:[de][+\-]?[0-9]+)?(?:_[a-z][a-z0-9_]*)?)\s*\)"
        r"(?:(?=$)|(?=[\s,+\-*/)\]]))",
        re.IGNORECASE,
    )
    while True:
        ns = atom_pat.sub(r"\1", s)
        if ns == s:
            break
        s = ns

    # Remove parentheses around inline multiplicative/power expressions in
    # arithmetic contexts, e.g. "(b**2) - ((4.0_dp * a) * c)".
    inline_expr_pat = re.compile(
        r"(?:(?<=^)|(?<=[\s=,+\-*/]))\(\s*([^()]+?)\s*\)(?:(?=$)|(?=[\s,+\-*/]))",
        re.IGNORECASE,
    )

    def _inline_expr_repl(m: re.Match[str]) -> str:
        inner = m.group(1).strip()
        if _can_drop_inline_parens(inner):
            return inner
        return m.group(0)

    prev = None
    while prev != s:
        prev = s
        s = inline_expr_pat.sub(_inline_expr_repl, s)

    # Unwrap one redundant nested layer around multiplicative groups:
    # "((X) * Y)" -> "(X) * Y", then previous passes can simplify further.
    # Match only single * or / operators here; do not split exponentiation (**).
    nested_mul_pat = re.compile(
        r"\(\s*\(\s*([^()]+?)\s*\)\s*((?<!\*)[*/](?!\*))\s*([^()]+?)\s*\)",
        re.IGNORECASE,
    )
    prev = None
    while prev != s:
        prev = s
        s = nested_mul_pat.sub(r"(\1) \2 \3", s)

    # Nested unwrapping can expose new inline-removal opportunities.
    prev = None
    while prev != s:
        prev = s
        s = inline_expr_pat.sub(_inline_expr_repl, s)

    # Remove parentheses around multiplicative-only expressions when followed by +/-
    inline_pat = re.compile(
        r"(?:(?<=^)|(?<=[\s=,+\-*/(]))\(\s*([^()]+?)\s*\)\s*([+\-])\s*([0-9]+)",
        re.IGNORECASE,
    )

    def _inline_repl(m: re.Match[str]) -> str:
        inner = m.group(1).strip()
        op = m.group(2)
        rhs = m.group(3)
        if _can_drop_inline_parens(inner):
            return f"{inner}{op}{rhs}"
        return m.group(0)

    prev = None
    while prev != s:
        prev = s
        s = inline_pat.sub(_inline_repl, s)

    return f"{s}{comment}{eol}"


def simplify_redundant_parens_in_lines(lines: List[str]) -> List[str]:
    """Apply conservative redundant-parentheses simplification to source lines."""
    return [simplify_redundant_parens_in_line(ln) for ln in lines]


def simplify_do_bounds_parens(lines: List[str]) -> List[str]:
    """Simplify redundant parentheses in DO loop bounds/step expressions."""
    out: List[str] = []
    do_re = re.compile(
        r"^(?P<indent>\s*)do\s+(?P<ivar>[a-z][a-z0-9_]*)\s*=\s*(?P<lb>[^,]+)\s*,\s*(?P<ub>[^,]+)(?:\s*,\s*(?P<st>.+))?$",
        re.IGNORECASE,
    )
    for raw in lines:
        code, comment = _split_code_comment(raw.rstrip("\r\n"))
        m = do_re.match(code)
        if m is None:
            out.append(raw)
            continue
        lb = strip_redundant_outer_parens_expr(m.group("lb").strip())
        ub = strip_redundant_outer_parens_expr(m.group("ub").strip())
        st0 = m.group("st")
        if st0 is None:
            rebuilt = f"{m.group('indent')}do {m.group('ivar')} = {lb}, {ub}"
        else:
            st = strip_redundant_outer_parens_expr(st0.strip())
            rebuilt = f"{m.group('indent')}do {m.group('ivar')} = {lb}, {ub}, {st}"
        eol = _line_eol(raw)
        out.append(f"{rebuilt}{comment}{eol}")
    return out


def _split_simple_relational(expr: str) -> Optional[Tuple[str, str, str]]:
    """Split a simple top-level relational expression into lhs/op/rhs."""
    ops = [">=", "<=", "==", "/=", ">", "<", ".ge.", ".le.", ".eq.", ".ne.", ".gt.", ".lt."]
    in_single = False
    in_double = False
    depth = 0
    s = expr.strip()
    i = 0
    while i < len(s):
        ch = s[i]
        if ch == "'" and not in_double:
            in_single = not in_single
            i += 1
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            i += 1
            continue
        if not in_single and not in_double:
            if ch == "(":
                depth += 1
                i += 1
                continue
            if ch == ")" and depth > 0:
                depth -= 1
                i += 1
                continue
            if depth == 0:
                rest = s[i:].lower()
                hit = None
                for op in ops:
                    if rest.startswith(op):
                        hit = op
                        break
                if hit is not None:
                    lhs = s[:i].strip()
                    rhs = s[i + len(hit) :].strip()
                    if lhs and rhs:
                        return lhs, hit, rhs
                    return None
        i += 1
    return None


def _has_top_level_logical_ops(expr: str) -> bool:
    """Return True if expression has top-level logical operators."""
    s = expr.strip().lower()
    in_single = False
    in_double = False
    depth = 0
    i = 0
    while i < len(s):
        ch = s[i]
        if ch == "'" and not in_double:
            in_single = not in_single
            i += 1
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            i += 1
            continue
        if not in_single and not in_double:
            if ch == "(":
                depth += 1
                i += 1
                continue
            if ch == ")" and depth > 0:
                depth -= 1
                i += 1
                continue
            if depth == 0:
                rest = s[i:]
                if rest.startswith(".and.") or rest.startswith(".or.") or rest.startswith(".eqv.") or rest.startswith(".neqv."):
                    return True
        i += 1
    return False


def _negate_rel_op(op: str) -> Optional[str]:
    m = {
        ">=": "<",
        "<=": ">",
        ">": "<=",
        "<": ">=",
        "==": "/=",
        "/=": "==",
        ".ge.": ".lt.",
        ".le.": ".gt.",
        ".gt.": ".le.",
        ".lt.": ".ge.",
        ".eq.": ".ne.",
        ".ne.": ".eq.",
    }
    return m.get(op.lower())


def simplify_negated_relational_conditions_in_line(line: str) -> str:
    """Simplify IF/DO WHILE conditions of form `.not. (a <op> b)`."""
    code, comment = _split_code_comment(line.rstrip("\r\n"))
    eol = _line_eol(line)

    def _rewrite_condition(cond: str) -> Optional[str]:
        c = cond.strip()
        m_not = re.match(r"^\s*\.not\.\s*\((.+)\)\s*$", c, re.IGNORECASE)
        if not m_not:
            return None
        inner = strip_redundant_outer_parens_expr(m_not.group(1).strip())
        if _has_top_level_logical_ops(inner):
            return None
        rel = _split_simple_relational(inner)
        if rel is None:
            return None
        lhs, op, rhs = rel
        nop = _negate_rel_op(op)
        if nop is None:
            return None
        return f"{lhs} {nop} {rhs}"

    def _find_matching_paren(text: str, open_idx: int) -> int:
        in_single = False
        in_double = False
        depth = 0
        for i in range(open_idx, len(text)):
            ch = text[i]
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
                        return i
        return -1

    m_if_head = re.match(r"^\s*if\s*\(", code, re.IGNORECASE)
    if m_if_head:
        open_idx = code.find("(", m_if_head.start())
        close_idx = _find_matching_paren(code, open_idx)
        if close_idx > open_idx:
            cond = code[open_idx + 1 : close_idx]
            rest = code[close_idx + 1 :]
            new_cond = _rewrite_condition(cond)
            if new_cond is not None:
                rebuilt = code[: open_idx + 1] + new_cond + code[close_idx:]
                return f"{rebuilt}{comment}{eol}"
            return f"{code}{comment}{eol}"

    m_do_head = re.match(r"^\s*do\s+while\s*\(", code, re.IGNORECASE)
    if m_do_head:
        open_idx = code.find("(", m_do_head.start())
        close_idx = _find_matching_paren(code, open_idx)
        if close_idx > open_idx:
            cond = code[open_idx + 1 : close_idx]
            new_cond = _rewrite_condition(cond)
            if new_cond is not None:
                rebuilt = code[: open_idx + 1] + new_cond + code[close_idx:]
                return f"{rebuilt}{comment}{eol}"
            return f"{code}{comment}{eol}"

    return f"{code}{comment}{eol}"


def simplify_negated_relational_conditions_in_lines(lines: List[str]) -> List[str]:
    """Apply `.not.(relational)` simplification across source lines."""
    return [simplify_negated_relational_conditions_in_line(ln) for ln in lines]


def rewrite_list_directed_print_reals(
    lines: List[str],
    *,
    real_fmt: str = "f0.6",
    unknown_fmt: str = "g0",
) -> List[str]:
    """Rewrite `print *, ...` to explicit `write` format descriptors.

    The pass is conservative and type-hinted by simple declarations seen in file:
    - known integer entities -> `i0`
    - known logical entities -> `l1`
    - known real entities -> `real_fmt` (default `f0.6`)
    - quoted string literals -> `a`
    - unknown expressions -> `unknown_fmt` (default `g0`)
    """
    int_names: Set[str] = set()
    real_names: Set[str] = set()
    logical_names: Set[str] = set()
    array_names: Set[str] = set()
    int_comp_names: Set[str] = set()
    real_comp_names: Set[str] = set()
    logical_comp_names: Set[str] = set()

    decl_head_re = re.compile(
        r"^\s*(?P<head>integer|real(?:\s*\([^)]*\))?|double\s+precision|logical)\b",
        re.IGNORECASE,
    )
    print_re = re.compile(r"^(?P<indent>\s*)print\s*\*\s*(?:,\s*(?P<args>.+))?$", re.IGNORECASE)
    str_lit_re = re.compile(r"^\s*(?:'([^']|'')*'|\"([^\"]|\"\")*\")\s*$")
    int_lit_re = re.compile(r"^\s*[+-]?\d+\s*$")
    real_lit_re = re.compile(r"^\s*[+-]?(?:\d+\.\d*|\d*\.\d+|\d+[deDE][+-]?\d+)(?:_[a-z][a-z0-9_]*)?\s*$")
    log_lit_re = re.compile(r"^\s*\.(?:true|false)\.\s*$", re.IGNORECASE)

    out: List[str] = []
    for raw in lines:
        code, comment = _split_code_comment(raw.rstrip("\r\n"))
        eol = _line_eol(raw)
        code_s = code.strip()

        m_decl = decl_head_re.match(code_s)
        if m_decl:
            names = parse_declared_names_from_decl(code_s)
            ents = parse_declared_entities(code_s)
            head = m_decl.group("head").lower()
            if head.startswith("integer"):
                int_names.update(names)
                int_comp_names.update(names)
            elif head.startswith("logical"):
                logical_names.update(names)
                logical_comp_names.update(names)
            else:
                real_names.update(names)
                real_comp_names.update(names)
            for nm, is_arr in ents:
                if is_arr:
                    array_names.add(nm)
            out.append(raw)
            continue

        m_pr = print_re.match(code)
        if m_pr is None:
            out.append(raw)
            continue

        args_txt = (m_pr.group("args") or "").strip()
        if not args_txt:
            out.append(raw)
            continue
        items = _split_top_level_commas(args_txt)
        if not items:
            out.append(raw)
            continue

        fmts: List[str] = []
        for it in items:
            t = it.strip()
            if not t:
                continue
            if str_lit_re.match(t):
                fmts.append("a")
                continue
            if log_lit_re.match(t):
                fmts.append("l1")
                continue
            if int_lit_re.match(t):
                fmts.append("i0")
                continue
            if real_lit_re.match(t):
                fmts.append(real_fmt)
                continue
            base = base_identifier(t)
            if base is not None:
                if base in int_names:
                    fmts.append("i0")
                    continue
                if base in logical_names:
                    fmts.append("l1")
                    continue
                if base in real_names:
                    fmts.append(real_fmt)
                    continue
            # Derived-type component fallback: use final component name.
            comp_hits = re.findall(r"%\s*([a-z][a-z0-9_]*)", t, re.IGNORECASE)
            if comp_hits:
                comp = comp_hits[-1].lower()
                if comp in int_comp_names:
                    fmts.append("i0")
                    continue
                if comp in logical_comp_names:
                    fmts.append("l1")
                    continue
                if comp in real_comp_names:
                    fmts.append(real_fmt)
                    continue
            fmts.append(unknown_fmt)

        if len(fmts) != len(items):
            out.append(raw)
            continue

        def _is_likely_array_expr(text: str) -> bool:
            t = text.strip()
            if not t:
                return False
            if t.startswith("[") and t.endswith("]"):
                return True
            # Array section / slicing form.
            if re.search(r"\([^()]*:[^()]*\)", t):
                return True
            b = base_identifier(t)
            if b is not None and b in array_names:
                return True
            # real(array_expr, kind=...) style.
            m_real = re.match(r"^real\s*\((.+)\)$", t, re.IGNORECASE)
            if m_real:
                parts = _split_top_level_commas(m_real.group(1))
                if parts:
                    return _is_likely_array_expr(parts[0])
            # Known helper calls returning vectors.
            m_call = re.match(r"^([a-z][a-z0-9_]*)\s*\(", t, re.IGNORECASE)
            if m_call:
                return m_call.group(1).lower() in {
                    "r_seq_int",
                    "r_seq_len",
                    "r_seq_int_by",
                    "r_seq_int_length",
                    "r_seq_real_by",
                    "r_seq_real_length",
                    "r_rep_int",
                    "r_rep_real",
                    "runif_vec",
                    "rnorm_vec",
                    "numeric",
                    "pack",
                }
            # Vector-returning calls inside arithmetic expressions.
            if re.search(
                r"\b(?:r_seq_int|r_seq_len|r_seq_int_by|r_seq_int_length|r_seq_real_by|r_seq_real_length|r_rep_int|r_rep_real|runif_vec|rnorm_vec|numeric|pack)\s*\(",
                t,
                re.IGNORECASE,
            ):
                return True
            return False

        if len(items) == 1 and _is_likely_array_expr(items[0]):
            fmt = "*(g0,1x)"
        else:
            fmt = ", ".join(fmts)
        rebuilt = f'{m_pr.group("indent")}write(*,"({fmt})") {", ".join(it.strip() for it in items)}'
        out.append(f"{rebuilt}{comment}{eol}")

    return out


def _split_format_items(fmt_body: str) -> List[str]:
    items: List[str] = []
    cur: List[str] = []
    depth = 0
    in_single = False
    in_double = False
    i = 0
    n = len(fmt_body)
    while i < n:
        ch = fmt_body[i]
        if ch == "'" and not in_double:
            in_single = not in_single
            cur.append(ch)
            i += 1
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            cur.append(ch)
            i += 1
            continue
        if not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")" and depth > 0:
                depth -= 1
            elif ch == "," and depth == 0:
                t = "".join(cur).strip()
                if t:
                    items.append(t)
                cur = []
                i += 1
                continue
        cur.append(ch)
        i += 1
    t = "".join(cur).strip()
    if t:
        items.append(t)
    return items


def _compact_format_items(items: List[str]) -> List[str]:
    out: List[str] = []
    i = 0
    n = len(items)
    simple_desc_re = re.compile(r"^[A-Za-z][A-Za-z0-9.]*$")

    while i < n:
        best_len = 0
        best_rep = 1
        max_len = (n - i) // 2
        for blen in range(max_len, 0, -1):
            base = items[i : i + blen]
            rep = 1
            while i + (rep + 1) * blen <= n and items[i + rep * blen : i + (rep + 1) * blen] == base:
                rep += 1
            if rep >= 2:
                best_len = blen
                best_rep = rep
                break

        if best_rep >= 2 and best_len >= 1:
            block = items[i : i + best_len]
            if best_len == 1 and simple_desc_re.match(block[0]):
                out.append(f"{best_rep}{block[0]}")
            else:
                out.append(f"{best_rep}({', '.join(block)})")
            i += best_rep * best_len
            continue

        out.append(items[i])
        i += 1

    return out


def compact_format_descriptor_string(fmt_body: str) -> str:
    """Compact repeated edit descriptors in a format list.

    Example:
    `a, a, f0.6, a, f0.6, a` -> `2a, 2(f0.6, a)`
    """
    items = _split_format_items(fmt_body)
    if len(items) <= 1:
        return fmt_body.strip()
    compacted = _compact_format_items(items)
    return ", ".join(compacted)


def compact_repeated_edit_descriptors(lines: List[str]) -> List[str]:
    """Rewrite WRITE/READ/PRINT format strings using repeat descriptors when possible."""
    fmt_lit_pat = re.compile(r"(['\"])\(([^'\"]*)\)\1")

    def _rewrite_code(code: str) -> str:
        def _fmt_sub(m: re.Match[str]) -> str:
            q, body = m.group(1), m.group(2)
            new_body = compact_format_descriptor_string(body)
            return f"{q}({new_body}){q}"

        return fmt_lit_pat.sub(_fmt_sub, code)

    out: List[str] = []
    for raw in lines:
        code, comment = _split_code_comment(raw.rstrip("\r\n"))
        eol = _line_eol(raw)
        if re.search(r"(?i)\b(?:write|read|print)\b", code):
            code = _rewrite_code(code)
        out.append(f"{code}{comment}{eol}")
    return out


def _fold_simple_integer_arithmetic(stmt: str) -> str:
    """Fold very simple integer-literal arithmetic conservatively.

    Supported form: `<int> <op> <int>` where op is +, -, *, /.
    Division folds only when exact and denominator is nonzero.
    """
    pat = re.compile(r"(?<![\w.])([+-]?\d+)\s*([+\-*/])\s*([+-]?\d+)(?![\w.])")

    def _repl(m: re.Match[str]) -> str:
        a = int(m.group(1))
        op = m.group(2)
        b = int(m.group(3))
        if op == "+":
            return str(a + b)
        if op == "-":
            return str(a - b)
        if op == "*":
            return str(a * b)
        if b == 0:
            return m.group(0)
        if a % b != 0:
            return m.group(0)
        return str(a // b)

    prev = None
    out = stmt
    while prev != out:
        prev = out
        out = pat.sub(_repl, out)
    return out


def simplify_integer_arithmetic_in_line(line: str) -> str:
    """Simplify integer-literal arithmetic in one Fortran source line."""
    code, comment = _split_code_comment(line.rstrip("\r\n"))
    eol = _line_eol(line)
    code = _fold_simple_integer_arithmetic(code)
    return f"{code}{comment}{eol}"


def simplify_integer_arithmetic_in_lines(lines: List[str]) -> List[str]:
    """Apply simple integer-literal arithmetic folding to source lines."""
    return [simplify_integer_arithmetic_in_line(ln) for ln in lines]


def simplify_square_multiplications_in_line(line: str) -> str:
    """Rewrite repeated self-multiplication to exponent form (`x*x` -> `x**2`)."""
    code, comment = _split_code_comment(line.rstrip("\r\n"))
    eol = _line_eol(line)
    pat = re.compile(
        r"(?P<a>\b[a-z][a-z0-9_]*(?:\s*\([^()]*\))?)\s*\*(?!\*)\s*(?P<b>\b[a-z][a-z0-9_]*(?:\s*\([^()]*\))?)",
        re.IGNORECASE,
    )

    def _norm(s: str) -> str:
        return re.sub(r"\s+", "", s).lower()

    def _repl(m: re.Match[str]) -> str:
        a = m.group("a")
        b = m.group("b")
        if _norm(a) != _norm(b):
            return m.group(0)
        return f"{a}**2"

    prev = None
    s = code
    while prev != s:
        prev = s
        s = pat.sub(_repl, s)
    return f"{s}{comment}{eol}"


def simplify_square_multiplications_in_lines(lines: List[str]) -> List[str]:
    """Apply `x*x -> x**2` simplification line-wise."""
    return [simplify_square_multiplications_in_line(ln) for ln in lines]


def suffix_real_literals_with_kind(lines: List[str], *, kind_name: str = "dp") -> List[str]:
    """Add kind suffix to unsuffixed real literals outside strings/comments.

    Example: `0.0` -> `0.0_dp`, `1.25e-3` -> `1.25e-3_dp`.
    """
    lit_re = re.compile(
        r"(?<![\w.])([+-]?(?:(?:\d+\.\d*|\.\d+)(?:[eEdD][+-]?\d+)?|\d+[eEdD][+-]?\d+))(?![\w.])",
        re.IGNORECASE,
    )

    def _suffix_segment(seg: str) -> str:
        def _repl(m: re.Match[str]) -> str:
            tok = m.group(1)
            # Skip if already kind-suffixed (defensive; regex usually prevents this).
            tail_i = m.end(1)
            if tail_i < len(seg) and seg[tail_i] == "_":
                return tok
            return f"{tok}_{kind_name}"

        return lit_re.sub(_repl, seg)

    out: List[str] = []
    for raw in lines:
        code, comment = _split_code_comment(raw.rstrip("\r\n"))
        eol = _line_eol(raw) or "\n"
        # Rewrite only outside quoted strings.
        chunks: List[str] = []
        i = 0
        in_single = False
        in_double = False
        start = 0
        while i < len(code):
            ch = code[i]
            if ch == "'" and not in_double:
                if not in_single:
                    chunks.append(_suffix_segment(code[start:i]))
                    start = i
                in_single = not in_single
            elif ch == '"' and not in_single:
                if not in_double:
                    chunks.append(_suffix_segment(code[start:i]))
                    start = i
                in_double = not in_double
            i += 1
            if (not in_single and not in_double) and start < i and code[start] in ("'", '"'):
                chunks.append(code[start:i])
                start = i
        if start < len(code):
            if in_single or in_double:
                chunks.append(code[start:])
            else:
                chunks.append(_suffix_segment(code[start:]))
        new_code = "".join(chunks)
        out.append(f"{new_code}{comment}{eol}")
    return out


def collapse_single_stmt_if_blocks(lines: List[str]) -> List[str]:
    """Collapse 3-line single-statement IF blocks to one-line IF.

    Rewrites:
      if (cond) then
         stmt
      end if
    as:
      if (cond) stmt

    Conservatively skips blocks with inline comments on IF/END IF lines, nested
    IF bodies, semicolon bodies, or body lines not indented beyond the IF line.
    """
    out: List[str] = []
    i = 0
    if_re = re.compile(r"^(\s*)if\s*\((.+)\)\s*then\s*$", re.IGNORECASE)
    endif_re = re.compile(r"^\s*end\s*if\s*$", re.IGNORECASE)
    while i < len(lines):
        ln_if = lines[i]
        if_code, if_comment = _split_code_comment(ln_if.rstrip("\r\n"))
        m_if = if_re.match(if_code.rstrip())
        if (
            m_if is None
            or if_comment.strip()
            or i + 2 >= len(lines)
        ):
            out.append(ln_if)
            i += 1
            continue

        ln_body = lines[i + 1]
        ln_end = lines[i + 2]
        body_code, body_comment = _split_code_comment(ln_body.rstrip("\r\n"))
        end_code, end_comment = _split_code_comment(ln_end.rstrip("\r\n"))
        if not endif_re.match(end_code.rstrip()) or end_comment.strip():
            out.append(ln_if)
            i += 1
            continue

        if_indent = len(m_if.group(1))
        body_indent = len(body_code) - len(body_code.lstrip(" \t"))
        body_stmt = body_code.strip()
        if (
            not body_stmt
            or body_indent <= if_indent
            or ";" in body_stmt
            or re.match(r"^if\s*\(", body_stmt, re.IGNORECASE) is not None
            or body_stmt.lower().startswith(("else", "elseif", "end "))
        ):
            out.append(ln_if)
            i += 1
            continue

        cond = m_if.group(2).strip()
        indent = m_if.group(1)
        eol = _line_eol(ln_if)
        out.append(f"{indent}if ({cond}) {body_stmt}{body_comment}{eol}")
        i += 3
    return out


def _expr_is_declared_integer(expr: str, int_names: Set[str]) -> bool:
    """Conservative check that expr is integer-valued from local integer names.

    Allows only integer literals, declared integer identifiers, arithmetic
    operators (+,-,*,/, parentheses), and whitespace.
    """
    s = expr.strip()
    if not s:
        return False
    if "," in s or ":" in s:
        return False
    if re.search(r"[<>=]|\.and\.|\.or\.|\.not\.", s, re.IGNORECASE):
        return False
    if re.search(r"[.][0-9]|[0-9][.]", s):
        return False
    if re.search(r"[de][+\-]?\d+", s, re.IGNORECASE):
        return False
    if re.search(r"[^a-z0-9_+\-*/()\s]", s, re.IGNORECASE):
        return False

    integer_intrinsics = {"int", "size", "lbound", "ubound", "len", "kind", "rank"}

    # Reject likely function calls except known integer-valued intrinsics.
    for m in re.finditer(r"\b([a-z_]\w*)\s*\(", s, re.IGNORECASE):
        name = m.group(1).lower()
        if name not in integer_intrinsics:
            return False

    # Replace known integer-valued intrinsic calls with a scalar placeholder.
    # This avoids falsely treating their argument identifiers as standalone vars.
    intr_pat = re.compile(r"\b(?:size|lbound|ubound|len|kind|rank|int)\s*\([^()]*\)", re.IGNORECASE)
    prev = None
    while prev != s:
        prev = s
        s = intr_pat.sub("1", s)

    for m in re.finditer(r"\b([a-z_]\w*)\b", s, re.IGNORECASE):
        name = m.group(1).lower()
        if name in integer_intrinsics:
            continue
        if name not in int_names:
            return False
    return True


def _remove_redundant_int_casts_in_stmt(stmt: str, int_names: Set[str]) -> str:
    """Remove redundant int(expr) wrappers when expr is provably integer."""
    def _find_matching_paren(text: str, open_idx: int) -> int:
        depth = 0
        in_single = False
        in_double = False
        i = open_idx
        while i < len(text):
            ch = text[i]
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
                        return i
            i += 1
        return -1

    i = 0
    out_parts: List[str] = []
    n = len(stmt)
    while i < n:
        m = re.search(r"\bint\b", stmt[i:], re.IGNORECASE)
        if m is None:
            out_parts.append(stmt[i:])
            break
        abs_start = i + m.start()
        abs_end = i + m.end()
        out_parts.append(stmt[i:abs_start])
        k = abs_end
        while k < n and stmt[k].isspace():
            k += 1
        if k >= n or stmt[k] != "(":
            out_parts.append(stmt[abs_start:abs_end])
            i = abs_end
            continue
        close_idx = _find_matching_paren(stmt, k)
        if close_idx < 0:
            out_parts.append(stmt[abs_start:])
            break
        inner = stmt[k + 1 : close_idx]
        inner_new = _remove_redundant_int_casts_in_stmt(inner, int_names)
        if _expr_is_declared_integer(inner_new.strip(), int_names):
            out_parts.append(inner_new.strip())
        else:
            out_parts.append(f"int({inner_new})")
        i = close_idx + 1
    return "".join(out_parts)


def _simplify_size_expr_in_stmt(stmt: str, array_names: Set[str]) -> str:
    """Simplify `size(expr)` to `size(array)` for safe elementwise-power cases."""

    def _find_matching_paren(text: str, open_idx: int) -> int:
        depth = 0
        in_single = False
        in_double = False
        i = open_idx
        while i < len(text):
            ch = text[i]
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
                        return i
            i += 1
        return -1

    i = 0
    out_parts: List[str] = []
    n = len(stmt)
    while i < n:
        m = re.search(r"\bsize\b", stmt[i:], re.IGNORECASE)
        if m is None:
            out_parts.append(stmt[i:])
            break
        abs_start = i + m.start()
        abs_end = i + m.end()
        out_parts.append(stmt[i:abs_start])
        k = abs_end
        while k < n and stmt[k].isspace():
            k += 1
        if k >= n or stmt[k] != "(":
            out_parts.append(stmt[abs_start:abs_end])
            i = abs_end
            continue
        close_idx = _find_matching_paren(stmt, k)
        if close_idx < 0:
            out_parts.append(stmt[abs_start:])
            break
        inner = stmt[k + 1 : close_idx]
        inner_new = _simplify_size_expr_in_stmt(inner, array_names).strip()
        inner_s = strip_redundant_outer_parens_expr(inner_new)
        mpow = re.fullmatch(r"([a-z_]\w*)\s*\*\*\s*[+-]?\d+", inner_s, re.IGNORECASE)
        if mpow is not None:
            nm = mpow.group(1).lower()
            if nm in array_names:
                out_parts.append(f"size({nm})")
            else:
                out_parts.append(f"size({inner_new})")
        else:
            out_parts.append(f"size({inner_new})")
        i = close_idx + 1
    return "".join(out_parts)


def simplify_size_expressions(lines: List[str]) -> List[str]:
    """Simplify `size(expr)` where `expr` is an elementwise power of one array.

    Example: `size(d**3)` -> `size(d)` when `d` is declared as an array in the unit.
    """
    out = list(lines)
    unit_start_re = re.compile(
        r"^\s*(?:[a-z][a-z0-9_()\s=,:]*\s+)?(?:function|subroutine)\b|^\s*program\b",
        re.IGNORECASE,
    )
    unit_end_re = re.compile(r"^\s*end\s+(?:function|subroutine|program)\b", re.IGNORECASE)
    declish_re = re.compile(
        r"^\s*(?:implicit\b|use\b|integer\b|real\b|logical\b|character\b|complex\b|type\b|class\b|procedure\b|save\b|parameter\b|external\b|intrinsic\b|common\b|equivalence\b|dimension\b)",
        re.IGNORECASE,
    )
    i = 0
    while i < len(out):
        if not unit_start_re.match(out[i].strip()):
            i += 1
            continue
        u_start = i
        j = i + 1
        while j < len(out) and not unit_end_re.match(out[j].strip()):
            j += 1
        u_end = j

        k = u_start + 1
        while k < u_end and (not out[k].strip() or out[k].lstrip().startswith("!")):
            k += 1
        while k < u_end:
            s = out[k].strip()
            if not s or s.startswith("!") or declish_re.match(s):
                k += 1
                continue
            break
        exec_start = k

        array_names: Set[str] = set()
        for di in range(u_start + 1, exec_start):
            code, _comment = _split_code_comment(out[di].rstrip("\r\n"))
            if "::" not in code:
                continue
            lhs, rhs = code.split("::", 1)
            lhs_low = lhs.lower()
            lhs_has_dimension = "dimension" in lhs_low
            for ent in _split_top_level_commas(rhs):
                mname = re.match(r"^\s*([a-z_]\w*)", ent, re.IGNORECASE)
                if not mname:
                    continue
                nm = mname.group(1).lower()
                if lhs_has_dimension or re.match(r"^\s*[a-z_]\w*\s*\(", ent, re.IGNORECASE):
                    array_names.add(nm)

        if array_names:
            for li in range(exec_start, u_end):
                raw = out[li]
                eol = _line_eol(raw) or "\n"
                code, comment = _split_code_comment(raw.rstrip("\r\n"))
                new_code = _simplify_size_expr_in_stmt(code, array_names)
                out[li] = f"{new_code}{comment}{eol}"

        i = u_end + 1
    return out


def _rhs_preserves_array_size_of(rhs: str, arr: str, array_names: Set[str]) -> bool:
    """Conservative test: rhs is elementwise expression preserving size(arr)."""
    s = strip_redundant_outer_parens_expr(rhs.strip())
    a = re.escape(arr)
    # arr
    if re.fullmatch(rf"{a}", s, re.IGNORECASE):
        return True
    # arr ** k
    if re.fullmatch(rf"{a}\s*\*\*\s*[+\-]?\d+", s, re.IGNORECASE):
        return True
    # arr op scalar or scalar op arr
    m1 = re.fullmatch(rf"{a}\s*([+\-*/])\s*(.+)", s, re.IGNORECASE)
    m2 = re.fullmatch(rf"(.+)\s*([+\-*/])\s*{a}", s, re.IGNORECASE)
    for m in (m1, m2):
        if m is None:
            continue
        other = (m.group(2) if m is m1 else m.group(1)).strip()
        bad = False
        for tok in re.finditer(r"\b([a-z_]\w*)\b", other, re.IGNORECASE):
            nm = tok.group(1).lower()
            if nm in array_names and nm != arr.lower():
                bad = True
                break
        if not bad:
            return True
    return False


def propagate_array_size_aliases(lines: List[str]) -> List[str]:
    """Propagate `size(lhs)` to base array when `lhs` keeps same shape as base.

    Example:
    - `d = x - m`  -> marks `d` as same-sized as `x`
    - later `size(d)` -> `size(x)`
    """
    out = list(lines)
    unit_start_re = re.compile(
        r"^\s*(?:[a-z][a-z0-9_()\s=,:]*\s+)?(?:function|subroutine)\b|^\s*program\b",
        re.IGNORECASE,
    )
    unit_end_re = re.compile(r"^\s*end\s+(?:function|subroutine|program)\b", re.IGNORECASE)
    declish_re = re.compile(
        r"^\s*(?:implicit\b|use\b|integer\b|real\b|logical\b|character\b|complex\b|type\b|class\b|procedure\b|save\b|parameter\b|external\b|intrinsic\b|common\b|equivalence\b|dimension\b)",
        re.IGNORECASE,
    )
    asn_re = re.compile(r"^\s*([a-z_]\w*)\s*=\s*(.+)$", re.IGNORECASE)
    i = 0
    while i < len(out):
        if not unit_start_re.match(out[i].strip()):
            i += 1
            continue
        u_start = i
        j = i + 1
        while j < len(out) and not unit_end_re.match(out[j].strip()):
            j += 1
        u_end = j

        k = u_start + 1
        while k < u_end and (not out[k].strip() or out[k].lstrip().startswith("!")):
            k += 1
        while k < u_end:
            s = out[k].strip()
            if not s or s.startswith("!") or declish_re.match(s):
                k += 1
                continue
            break
        exec_start = k

        array_names: Set[str] = set()
        for di in range(u_start + 1, exec_start):
            code, _comment = _split_code_comment(out[di].rstrip("\r\n"))
            if "::" not in code:
                continue
            lhs, rhs = code.split("::", 1)
            lhs_low = lhs.lower()
            lhs_has_dimension = "dimension" in lhs_low
            for ent in _split_top_level_commas(rhs):
                mname = re.match(r"^\s*([a-z_]\w*)", ent, re.IGNORECASE)
                if not mname:
                    continue
                nm = mname.group(1).lower()
                if lhs_has_dimension or re.match(r"^\s*[a-z_]\w*\s*\(", ent, re.IGNORECASE):
                    array_names.add(nm)

        alias: Dict[str, str] = {}
        for li in range(exec_start, u_end):
            raw = out[li]
            eol = _line_eol(raw) or "\n"
            code, comment = _split_code_comment(raw.rstrip("\r\n"))
            s = code.strip()
            if not s:
                continue

            # Rewrite size(lhs_alias) in current statement.
            for lhs_nm, base_nm in list(alias.items()):
                size_pat = re.compile(rf"\bsize\s*\(\s*{re.escape(lhs_nm)}\s*\)", re.IGNORECASE)
                code = size_pat.sub(f"size({base_nm})", code)

            # Update aliases with simple whole-array assignment.
            m_as = asn_re.match(code.strip())
            if m_as:
                lhs = m_as.group(1).lower()
                rhs = m_as.group(2).strip()
                alias.pop(lhs, None)
                if lhs in array_names:
                    for arr in array_names:
                        base = alias.get(arr, arr)
                        if _rhs_preserves_array_size_of(rhs, arr, array_names):
                            alias[lhs] = base
                            break

            out[li] = f"{code}{comment}{eol}"

        i = u_end + 1
    return out


def propagate_cached_size_values(lines: List[str]) -> List[str]:
    """Propagate cached `n = size(x)` into later `size(x)` uses conservatively.

    Rewrites `size(x)` to `n` only until a potential mutation/escape of `x`:
    - assignment to `x` (including indexed/component forms),
    - `allocate(...x...)` / `deallocate(...x...)` / `move_alloc(..., x)`,
    - `call ...(...x...)`.
    """
    out = list(lines)
    unit_start_re = re.compile(
        r"^\s*(?:[a-z][a-z0-9_()\s=,:]*\s+)?(?:function|subroutine)\b|^\s*program\b",
        re.IGNORECASE,
    )
    unit_end_re = re.compile(r"^\s*end\s+(?:function|subroutine|program)\b", re.IGNORECASE)
    declish_re = re.compile(
        r"^\s*(?:implicit\b|use\b|integer\b|real\b|logical\b|character\b|complex\b|type\b|class\b|procedure\b|save\b|parameter\b|external\b|intrinsic\b|common\b|equivalence\b|dimension\b)",
        re.IGNORECASE,
    )
    asn_re = re.compile(r"^\s*([a-z_]\w*(?:\([^)]*\))?(?:%[a-z_]\w*(?:\([^)]*\))?)*)\s*=\s*(.+)$", re.IGNORECASE)
    cache_re = re.compile(r"^\s*([a-z_]\w*)\s*=\s*size\s*\(\s*([a-z_]\w*)\s*\)\s*$", re.IGNORECASE)
    i = 0
    while i < len(out):
        if not unit_start_re.match(out[i].strip()):
            i += 1
            continue
        u_start = i
        j = i + 1
        while j < len(out) and not unit_end_re.match(out[j].strip()):
            j += 1
        u_end = j

        k = u_start + 1
        while k < u_end and (not out[k].strip() or out[k].lstrip().startswith("!")):
            k += 1
        while k < u_end:
            s = out[k].strip()
            if not s or s.startswith("!") or declish_re.match(s):
                k += 1
                continue
            break
        exec_start = k

        idx = exec_start
        while idx < u_end:
            raw = out[idx]
            eol = _line_eol(raw) or "\n"
            code, comment = _split_code_comment(raw.rstrip("\r\n"))
            m_cache = cache_re.match(code.strip())
            if not m_cache:
                idx += 1
                continue
            nvar = m_cache.group(1).lower()
            xvar = m_cache.group(2).lower()
            size_pat = re.compile(rf"\bsize\s*\(\s*{re.escape(xvar)}\s*\)", re.IGNORECASE)

            j2 = idx + 1
            while j2 < u_end:
                raw2 = out[j2]
                eol2 = _line_eol(raw2) or "\n"
                code2, comment2 = _split_code_comment(raw2.rstrip("\r\n"))
                low2 = code2.lower()

                # Barrier: direct calls with xvar, potential mutation/alias.
                if re.search(r"\bcall\b", low2) and re.search(rf"\b{re.escape(xvar)}\b", low2):
                    break
                # Barrier: allocation/deallocation/move_alloc mentions xvar.
                if (
                    (("allocate(" in low2) or ("allocate (" in low2) or ("deallocate(" in low2) or ("deallocate (" in low2))
                    and re.search(rf"\b{re.escape(xvar)}\b", low2)
                ):
                    break
                if "move_alloc" in low2 and re.search(rf"\b{re.escape(xvar)}\b", low2):
                    break
                # Barrier: assignment to xvar (possibly indexed/componented).
                m_as = asn_re.match(code2.strip())
                if m_as:
                    lhs = m_as.group(1).strip().lower()
                    lhs_base = re.split(r"[%(\s]", lhs, maxsplit=1)[0]
                    if lhs_base == xvar:
                        break
                    if lhs_base == nvar:
                        break

                if size_pat.search(code2):
                    new_code2 = size_pat.sub(nvar, code2)
                    out[j2] = f"{new_code2}{comment2}{eol2}"

                j2 += 1
            idx += 1

        i = u_end + 1
    return out


def remove_redundant_int_casts(lines: List[str]) -> List[str]:
    """Remove unnecessary int(...) casts based on local integer declarations."""
    out = list(lines)
    unit_start_re = re.compile(
        r"^\s*(?:[a-z][a-z0-9_()\s=,:]*\s+)?(?:function|subroutine)\b|^\s*program\b",
        re.IGNORECASE,
    )
    unit_end_re = re.compile(r"^\s*end\s+(?:function|subroutine|program)\b", re.IGNORECASE)
    declish_re = re.compile(
        r"^\s*(?:implicit\b|use\b|integer\b|real\b|logical\b|character\b|complex\b|type\b|class\b|procedure\b|save\b|parameter\b|external\b|intrinsic\b|common\b|equivalence\b|dimension\b)",
        re.IGNORECASE,
    )
    i = 0
    while i < len(out):
        if not unit_start_re.match(out[i].strip()):
            i += 1
            continue
        u_start = i
        j = i + 1
        while j < len(out) and not unit_end_re.match(out[j].strip()):
            j += 1
        u_end = j

        # Declaration section
        k = u_start + 1
        while k < u_end and (not out[k].strip() or out[k].lstrip().startswith("!")):
            k += 1
        while k < u_end:
            s = out[k].strip()
            if not s or s.startswith("!") or declish_re.match(s):
                k += 1
                continue
            break
        exec_start = k

        int_names: Set[str] = set()
        for di in range(u_start + 1, exec_start):
            code, _comment = _split_code_comment(out[di].rstrip("\r\n"))
            if "::" not in code:
                continue
            lhs, rhs = code.split("::", 1)
            if not re.match(r"^\s*integer\b", lhs, re.IGNORECASE):
                continue
            for ent in _split_top_level_commas(rhs):
                m = re.match(r"^\s*([a-z_]\w*)", ent, re.IGNORECASE)
                if m:
                    int_names.add(m.group(1).lower())

        if int_names:
            for li in range(exec_start, u_end):
                raw = out[li]
                eol = _line_eol(raw) or "\n"
                code, comment = _split_code_comment(raw.rstrip("\r\n"))
                new_code = _remove_redundant_int_casts_in_stmt(code, int_names)
                out[li] = f"{new_code}{comment}{eol}"

        i = u_end + 1
    return out


def remove_redundant_real_casts(lines: List[str]) -> List[str]:
    """Remove unnecessary `real(x, kind=dp|real64)` casts for known real vars."""
    out = list(lines)
    unit_start_re = re.compile(
        r"^\s*(?:[a-z][a-z0-9_()\s=,:]*\s+)?(?:function|subroutine)\b|^\s*program\b",
        re.IGNORECASE,
    )
    unit_end_re = re.compile(r"^\s*end\s+(?:function|subroutine|program)\b", re.IGNORECASE)
    declish_re = re.compile(
        r"^\s*(?:implicit\b|use\b|integer\b|real\b|logical\b|character\b|complex\b|type\b|class\b|procedure\b|save\b|parameter\b|external\b|intrinsic\b|common\b|equivalence\b|dimension\b)",
        re.IGNORECASE,
    )
    i = 0
    while i < len(out):
        if not unit_start_re.match(out[i].strip()):
            i += 1
            continue
        u_start = i
        j = i + 1
        while j < len(out) and not unit_end_re.match(out[j].strip()):
            j += 1
        u_end = j

        k = u_start + 1
        while k < u_end and (not out[k].strip() or out[k].lstrip().startswith("!")):
            k += 1
        while k < u_end:
            s = out[k].strip()
            if not s or s.startswith("!") or declish_re.match(s):
                k += 1
                continue
            break
        exec_start = k

        real_names: Set[str] = set()
        for di in range(u_start + 1, exec_start):
            code, _comment = _split_code_comment(out[di].rstrip("\r\n"))
            if "::" not in code:
                continue
            lhs, rhs = code.split("::", 1)
            low_lhs = lhs.lower()
            if not re.match(r"^\s*real\b", low_lhs):
                continue
            if "kind=dp" not in low_lhs and "kind = dp" not in low_lhs and "kind=real64" not in low_lhs and "kind = real64" not in low_lhs:
                continue
            for ent in _split_top_level_commas(rhs):
                m = re.match(r"^\s*([a-z_]\w*)", ent, re.IGNORECASE)
                if m:
                    real_names.add(m.group(1).lower())

        if real_names:
            cast_re = re.compile(
                r"\breal\s*\(\s*([+-]?\s*[a-z_]\w*)\s*,\s*kind\s*=\s*(?:dp|real64)\s*\)",
                re.IGNORECASE,
            )
            for li in range(exec_start, u_end):
                raw = out[li]
                eol = _line_eol(raw) or "\n"
                code, comment = _split_code_comment(raw.rstrip("\r\n"))

                def _repl(m: re.Match[str]) -> str:
                    expr = m.group(1)
                    nm = re.sub(r"^\s*[+-]\s*", "", expr).strip().lower()
                    if nm in real_names:
                        return expr.strip()
                    return m.group(0)

                new_code = cast_re.sub(_repl, code)
                out[li] = f"{new_code}{comment}{eol}"

        i = u_end + 1
    return out


def _expr_is_real_like(expr: str, real_names: Set[str]) -> bool:
    """Conservative real-typed heuristic for local expression fragments."""
    s = expr.strip()
    if not s:
        return False
    if re.search(r"\b\d+\.\d*(?:[de][+\-]?\d+)?(?:_dp)?\b", s, re.IGNORECASE):
        return True
    if re.search(r"\b\d+[de][+\-]?\d+(?:_dp)?\b", s, re.IGNORECASE):
        return True
    m = re.match(r"^[a-z_]\w*(?:\s*\([^)]*\))?$", s, re.IGNORECASE)
    if m:
        base = re.match(r"^([a-z_]\w*)", s, re.IGNORECASE)
        if base and base.group(1).lower() in real_names:
            return True
    # Common real-valued intrinsic calls / constructors.
    if re.search(
        r"\b(?:real|sqrt|log|exp|sin|cos|tan|asin|acos|atan|minval|maxval|sum|mean|dot_product)\s*\(",
        s,
        re.IGNORECASE,
    ):
        return True
    return False


def simplify_real_int_casts_in_mixed_expr(lines: List[str]) -> List[str]:
    """Drop `real(int_expr, kind=dp|real64)` where mixed arithmetic already promotes.

    Safe conservative cases:
    - `REAL_TERM op real(INT_EXPR, kind=dp|real64)` -> `REAL_TERM op INT_EXPR`
    - `real(INT_EXPR, kind=dp|real64) op REAL_TERM` -> `INT_EXPR op REAL_TERM`
    with `op` in `+ - * /`, `INT_EXPR` provably integer, and opposite term
    provably real-like.
    """
    out = list(lines)
    unit_start_re = re.compile(
        r"^\s*(?:[a-z][a-z0-9_()\s=,:]*\s+)?(?:function|subroutine)\b|^\s*program\b",
        re.IGNORECASE,
    )
    unit_end_re = re.compile(r"^\s*end\s+(?:function|subroutine|program)\b", re.IGNORECASE)
    declish_re = re.compile(
        r"^\s*(?:implicit\b|use\b|integer\b|real\b|logical\b|character\b|complex\b|type\b|class\b|procedure\b|save\b|parameter\b|external\b|intrinsic\b|common\b|equivalence\b|dimension\b)",
        re.IGNORECASE,
    )
    i = 0
    while i < len(out):
        if not unit_start_re.match(out[i].strip()):
            i += 1
            continue
        u_start = i
        j = i + 1
        while j < len(out) and not unit_end_re.match(out[j].strip()):
            j += 1
        u_end = j

        k = u_start + 1
        while k < u_end and (not out[k].strip() or out[k].lstrip().startswith("!")):
            k += 1
        while k < u_end:
            s = out[k].strip()
            if not s or s.startswith("!") or declish_re.match(s):
                k += 1
                continue
            break
        exec_start = k

        int_names: Set[str] = set()
        real_names: Set[str] = set()
        for di in range(u_start + 1, exec_start):
            code, _comment = _split_code_comment(out[di].rstrip("\r\n"))
            if "::" not in code:
                continue
            lhs, rhs = code.split("::", 1)
            lhs_low = lhs.lower()
            if re.match(r"^\s*integer\b", lhs_low):
                for ent in _split_top_level_commas(rhs):
                    m = re.match(r"^\s*([a-z_]\w*)", ent, re.IGNORECASE)
                    if m:
                        int_names.add(m.group(1).lower())
            elif re.match(r"^\s*real\b", lhs_low):
                for ent in _split_top_level_commas(rhs):
                    m = re.match(r"^\s*([a-z_]\w*)", ent, re.IGNORECASE)
                    if m:
                        real_names.add(m.group(1).lower())

        if int_names:
            cast_pat = (
                r"real\s*\(\s*(?P<inner>.+?)\s*,\s*kind\s*=\s*(?:dp|real64)\s*\)"
            )
            div_rhs_pat = re.compile(
                rf"(?P<lhs>.+?)\s*/\s*{cast_pat}",
                re.IGNORECASE,
            )
            div_lhs_pat = re.compile(
                rf"{cast_pat}\s*/\s*(?P<rhs>.+)",
                re.IGNORECASE,
            )
            lhs_pat = re.compile(
                rf"(?P<lhs>[a-z_]\w*(?:\s*\([^)]*\))?)\s*(?P<op>[+\-*/])\s*{cast_pat}",
                re.IGNORECASE,
            )
            rhs_pat = re.compile(
                rf"{cast_pat}\s*(?P<op>[+\-*/])\s*(?P<rhs>[a-z_]\w*(?:\s*\([^)]*\))?)",
                re.IGNORECASE,
            )
            for li in range(exec_start, u_end):
                raw = out[li]
                eol = _line_eol(raw) or "\n"
                code, comment = _split_code_comment(raw.rstrip("\r\n"))

                def _repl_lhs(m: re.Match[str]) -> str:
                    lhs = m.group("lhs")
                    op = m.group("op")
                    inner = m.group("inner").strip()
                    if _expr_is_declared_integer(inner, int_names) and _expr_is_real_like(lhs, real_names):
                        return f"{lhs} {op} {inner}"
                    return m.group(0)

                def _repl_rhs(m: re.Match[str]) -> str:
                    rhs = m.group("rhs")
                    op = m.group("op")
                    inner = m.group("inner").strip()
                    if _expr_is_declared_integer(inner, int_names) and _expr_is_real_like(rhs, real_names):
                        return f"{inner} {op} {rhs}"
                    return m.group(0)

                def _repl_div_rhs(m: re.Match[str]) -> str:
                    lhs = m.group("lhs").strip()
                    inner = m.group("inner").strip()
                    lhs_expr = lhs.rsplit("=", 1)[-1].strip()
                    if _expr_is_declared_integer(inner, int_names) and _expr_is_real_like(lhs_expr, real_names):
                        return f"{lhs} / {inner}"
                    return m.group(0)

                def _repl_div_lhs(m: re.Match[str]) -> str:
                    rhs = m.group("rhs").strip()
                    inner = m.group("inner").strip()
                    if _expr_is_declared_integer(inner, int_names) and _expr_is_real_like(rhs, real_names):
                        return f"{inner} / {rhs}"
                    return m.group(0)

                new_code = div_rhs_pat.sub(_repl_div_rhs, code)
                new_code = div_lhs_pat.sub(_repl_div_lhs, new_code)
                new_code = lhs_pat.sub(_repl_lhs, new_code)
                new_code = rhs_pat.sub(_repl_rhs, new_code)
                out[li] = f"{new_code}{comment}{eol}"

        i = u_end + 1
    return out


_END_PROGRAM_UNIT_RE = re.compile(r"^\s*end\s+(program|function|subroutine)\b", re.IGNORECASE)
_DEALLOC_SIMPLE_RE = re.compile(
    r"^\s*deallocate\s*\(\s*[a-z_]\w*(?:\s*,\s*[a-z_]\w*)*\s*\)\s*$",
    re.IGNORECASE,
)
_IF_ALLOC_DEALLOC_RE = re.compile(
    r"^\s*if\s*\(\s*allocated\s*\(\s*[a-z_]\w*\s*\)\s*\)\s*deallocate\s*\(\s*[a-z_]\w*(?:\s*,\s*[a-z_]\w*)*\s*\)\s*$",
    re.IGNORECASE,
)


def _is_tail_deallocate_stmt(code: str) -> bool:
    """True for simple tail deallocate statements safe to remove."""
    s = code.strip()
    if not s or ";" in s:
        return False
    return bool(_DEALLOC_SIMPLE_RE.match(s) or _IF_ALLOC_DEALLOC_RE.match(s))


def remove_redundant_tail_deallocations(lines: List[str]) -> List[str]:
    """Remove trailing deallocate cleanup right before end of program units.

    Removes contiguous trailing statements matching either:
    - `deallocate(name[, ...])`
    - `if (allocated(name)) deallocate(name[, ...])`
    when they appear immediately before `end program|function|subroutine`
    (allowing intervening blank/comment-only lines).
    """
    out = list(lines)
    i = 0
    while i < len(out):
        if not _END_PROGRAM_UNIT_RE.match(out[i].strip()):
            i += 1
            continue
        k = i - 1
        to_remove: List[int] = []
        while k >= 0:
            s = out[k].strip()
            if not s or s.startswith("!"):
                k -= 1
                continue
            code, _comment = _split_code_comment(out[k].rstrip("\r\n"))
            if _is_tail_deallocate_stmt(code):
                to_remove.append(k)
                k -= 1
                continue
            break
        if to_remove:
            for idx in sorted(to_remove, reverse=True):
                del out[idx]
                if idx < i:
                    i -= 1
        i += 1
    return out


def coalesce_simple_declarations(lines: List[str], max_len: int = 80) -> List[str]:
    """Merge adjacent declaration lines with identical type-spec.

    Conservative scope:
    - only one declared entity per line
    - entity may include simple shape, e.g. `a(:)` or `x(1:n)`
    - skips lines with inline comments
    - skips initialized entities (`= ...`)
    - preserves non-declaration lines and order
    """
    out: List[str] = []
    i = 0
    decl_re = re.compile(
        r"^(\s*)([^:][^:]*)\s*::\s*([a-z][a-z0-9_]*(?:\s*\([^)]*\))?)\s*$",
        re.IGNORECASE,
    )
    while i < len(lines):
        line = lines[i]
        code0 = line.rstrip("\r\n")
        code, comment = _split_code_comment(code0)
        # Keep lines with inline comments untouched.
        if comment.strip():
            out.append(line)
            i += 1
            continue
        code = code.rstrip()
        m = decl_re.match(code)
        if not m:
            out.append(line)
            i += 1
            continue
        indent = m.group(1)
        spec = m.group(2).strip()
        entity = m.group(3).strip()
        # Skip initialized declarations.
        # Note: entity may legally contain commas inside shape, e.g. a(:,:).
        if "=" in entity:
            out.append(line)
            i += 1
            continue
        names = [entity]
        j = i + 1
        eol = "\r\n" if line.endswith("\r\n") else ("\n" if line.endswith("\n") else "")
        while j < len(lines):
            codej0 = lines[j].rstrip("\r\n")
            code_j, comment_j = _split_code_comment(codej0)
            if comment_j.strip():
                break
            code_j = code_j.rstrip()
            mj = decl_re.match(code_j)
            if not mj:
                break
            if mj.group(1) != indent or mj.group(2).strip().lower() != spec.lower():
                break
            entj = mj.group(3).strip()
            if "=" in entj:
                break
            names.append(entj)
            eol = "\r\n" if lines[j].endswith("\r\n") else ("\n" if lines[j].endswith("\n") else eol)
            j += 1
        if len(names) == 1:
            out.append(line)
        else:
            merged = f"{indent}{spec} :: {', '.join(names)}"
            if len(merged) <= max_len:
                out.append(f"{merged}{eol}")
            else:
                first = f"{indent}{spec} :: {names[0]}, &"
                if len(first) <= max_len:
                    out.append(f"{first}{eol}")
                    start_idx = 1
                else:
                    out.append(f"{indent}{spec} :: &{eol}")
                    start_idx = 0
                for k in range(start_idx, len(names)):
                    nm = names[k]
                    is_last = (k == len(names) - 1)
                    tail = "" if is_last else ", &"
                    out.append(f"{indent}   & {nm}{tail}{eol}")
        i = j
    def _consume_statement(src: List[str], start: int) -> Tuple[int, str, bool, str, List[str]]:
        """Consume one free-form statement with continuation lines."""
        k = start
        parts: List[str] = []
        raw_chunk: List[str] = []
        has_inline_comment = False
        eol = "\n"
        first = True
        while k < len(src):
            raw = src[k]
            raw_chunk.append(raw)
            eol_k = _line_eol(raw)
            if eol_k:
                eol = eol_k
            body = raw.rstrip("\r\n")
            code_k, comment_k = _split_code_comment(body)
            if comment_k.strip() and code_k.strip():
                has_inline_comment = True
            seg = code_k.rstrip()
            if not first:
                lead = seg.lstrip()
                if lead.startswith("&"):
                    seg = lead[1:].lstrip()
            first = False
            cont = seg.endswith("&")
            if cont:
                seg = seg[:-1].rstrip()
            if seg:
                parts.append(seg)
            k += 1
            if not cont:
                break
        return k, " ".join(parts).strip(), has_inline_comment, eol, raw_chunk

    def _coalesce_visibility_statements(src: List[str]) -> List[str]:
        out_vis: List[str] = []
        i_vis = 0
        vis_re = re.compile(r"^(\s*)(public|private)\s*::\s*(.+)$", re.IGNORECASE)
        while i_vis < len(src):
            j1, stmt1, cmt1, _eol1, raw1 = _consume_statement(src, i_vis)
            m1 = vis_re.match(stmt1)
            if not m1 or cmt1:
                out_vis.extend(raw1)
                i_vis = j1
                continue

            indent = m1.group(1)
            vis_kw = m1.group(2).lower()
            names: List[str] = []
            names.extend(_split_top_level_commas(m1.group(3).strip()))
            group_end = j1
            eol = _eol1
            n_stmts = 1

            while group_end < len(src):
                j2, stmt2, cmt2, eol2, raw2 = _consume_statement(src, group_end)
                m2 = vis_re.match(stmt2)
                if not m2 or cmt2 or m2.group(1) != indent or m2.group(2).lower() != vis_kw:
                    break
                names.extend(_split_top_level_commas(m2.group(3).strip()))
                eol = eol2
                n_stmts += 1
                group_end = j2

            if n_stmts == 1:
                out_vis.extend(raw1)
                i_vis = j1
                continue

            merged = f"{indent}{vis_kw} :: {', '.join(names)}"
            if len(merged) <= max_len:
                out_vis.append(f"{merged}{eol}")
            else:
                wrapped_vis = wrap_long_declaration_lines([f"{merged}{eol}"], max_len=max_len)
                out_vis.extend(wrapped_vis)
            i_vis = group_end
        return out_vis

    return _coalesce_visibility_statements(out)


def wrap_long_declaration_lines(lines: List[str], max_len: int = 80) -> List[str]:
    """Wrap long declaration lines with free-form continuation at entity commas."""
    out: List[str] = []
    decl_re = re.compile(r"^(\s*)([^:][^:]*)\s*::\s*(.+?)\s*$", re.IGNORECASE)

    def _split_entities(s: str) -> List[str]:
        items: List[str] = []
        cur: List[str] = []
        depth = 0
        in_single = False
        in_double = False
        for ch in s:
            if ch == "'" and not in_double:
                in_single = not in_single
                cur.append(ch)
                continue
            if ch == '"' and not in_single:
                in_double = not in_double
                cur.append(ch)
                continue
            if in_single or in_double:
                cur.append(ch)
                continue
            if ch == "(":
                depth += 1
                cur.append(ch)
                continue
            if ch == ")":
                depth = max(0, depth - 1)
                cur.append(ch)
                continue
            if ch == "," and depth == 0:
                part = "".join(cur).strip()
                if part:
                    items.append(part)
                cur = []
                continue
            cur.append(ch)
        part = "".join(cur).strip()
        if part:
            items.append(part)
        return items

    i = 0
    while i < len(lines):
        raw = lines[i]
        code0 = raw.rstrip("\r\n")
        code, comment = _split_code_comment(code0)
        eol = _line_eol(raw)
        if comment.strip():
            out.append(raw)
            i += 1
            continue
        m = decl_re.match(code.rstrip())
        if not m:
            out.append(raw)
            i += 1
            continue
        indent, spec, ent_text = m.group(1), m.group(2).strip(), m.group(3).strip()

        # If this declaration already uses continuation lines, normalize them
        # into one entity list before re-wrapping.
        j = i
        ent_chunks: List[str] = [ent_text]
        while True:
            cur = ent_chunks[-1].rstrip()
            if not cur.endswith("&"):
                break
            j += 1
            if j >= len(lines):
                break
            nxt = lines[j].rstrip("\r\n")
            nxt_code, nxt_comment = _split_code_comment(nxt)
            if nxt_comment.strip():
                break
            ms = re.match(r"^\s*&\s*(.*)\s*$", nxt_code)
            if not ms:
                break
            ent_chunks[-1] = cur[:-1].rstrip()
            ent_chunks.append(ms.group(1).strip())
        if j > i:
            cleaned = ", ".join(s.strip().rstrip(",") for s in ent_chunks if s.strip())
            ent_text = cleaned
            i = j

        full = f"{indent}{spec} :: {ent_text}"
        if len(full) <= max_len:
            out.append(f"{full}{eol}")
            i += 1
            continue
        ents = _split_entities(ent_text)
        if len(ents) <= 1:
            out.append(f"{full}{eol}")
            i += 1
            continue
        first_prefix = f"{indent}{spec} :: "
        cont_prefix = f"{indent}   & "
        rows: List[List[str]] = []
        cur: List[str] = []
        cur_prefix = first_prefix
        for ent in ents:
            trial = ", ".join(cur + [ent])
            if len(cur_prefix + trial) <= max_len or not cur:
                cur.append(ent)
            else:
                rows.append(cur)
                cur = [ent]
                cur_prefix = cont_prefix
        if cur:
            rows.append(cur)

        for ridx, row in enumerate(rows):
            is_last_row = (ridx == len(rows) - 1)
            prefix = first_prefix if ridx == 0 else cont_prefix
            body = ", ".join(row)
            if is_last_row:
                out.append(f"{prefix}{body}{eol}")
            else:
                out.append(f"{prefix}{body}, &{eol}")
        i += 1
    return out


def _is_comment_or_preproc_body(body: str) -> bool:
    """True for comment/preprocessor-only physical lines."""
    s = body.lstrip()
    if not s:
        return False
    return s.startswith("!") or s.startswith("#")


def _break_candidates_for_wrap(body: str, start: int, end: int) -> List[int]:
    """Safe split points outside quoted strings."""
    out: List[int] = []
    in_single = False
    in_double = False
    i = 0
    while i < len(body):
        ch = body[i]
        if ch == "'" and not in_double:
            if in_single and i + 1 < len(body) and body[i + 1] == "'":
                i += 2
                continue
            in_single = not in_single
            i += 1
            continue
        if ch == '"' and not in_single:
            if in_double and i + 1 < len(body) and body[i + 1] == '"':
                i += 2
                continue
            in_double = not in_double
            i += 1
            continue
        if not in_single and not in_double and start <= i <= end:
            if ch.isspace() or ch in ",+-*/)=]":
                out.append(i)
        i += 1
    return out


def _preferred_named_arg_break(body: str, start: int, end: int) -> Optional[int]:
    """Prefer wrapping after a comma before a `name = value` argument."""
    in_single = False
    in_double = False
    best: Optional[int] = None
    i = 0
    n = len(body)
    while i < n:
        ch = body[i]
        if ch == "'" and not in_double:
            if in_single and i + 1 < n and body[i + 1] == "'":
                i += 2
                continue
            in_single = not in_single
            i += 1
            continue
        if ch == '"' and not in_single:
            if in_double and i + 1 < n and body[i + 1] == '"':
                i += 2
                continue
            in_double = not in_double
            i += 1
            continue
        if not in_single and not in_double and ch == "," and start <= i < end:
            j = i + 1
            while j < n and body[j].isspace():
                j += 1
            if re.match(r"[a-z][a-z0-9_]*\s*=", body[j:], re.IGNORECASE):
                best = i + 1  # keep comma on the left line
        i += 1
    return best


def wrap_long_fortran_line(body: str, max_len: int = 80) -> Optional[List[str]]:
    """Wrap one long free-form Fortran line with `&` continuation.

    Returns `None` when no conservative wrap point is found.
    """
    if len(body) <= max_len:
        return [body]
    if _is_comment_or_preproc_body(body):
        return None

    indent = re.match(r"^\s*", body).group(0)
    cont_indent = indent + "   "
    lines: List[str] = []
    cur = body
    first = True

    while len(cur) > max_len:
        prefix = indent if first else (cont_indent + "& ")
        min_split = len(prefix) + 8
        max_split = max_len - 2  # reserve for trailing " &"
        if max_split <= min_split:
            return None
        cands = _break_candidates_for_wrap(cur, min_split, max_split)
        if not cands:
            return None
        cut = _preferred_named_arg_break(cur, min_split, max_split)
        if cut is None:
            cut = cands[-1]
        left = cur[:cut].rstrip()
        right = cur[cut:].lstrip()
        if not left or not right:
            return None
        lines.append(f"{left} &")
        cur = f"{cont_indent}& {right}"
        first = False

    lines.append(cur)
    return lines


def wrap_long_fortran_lines(lines: List[str], max_len: int = 80) -> List[str]:
    """Wrap long free-form Fortran lines; keep unwrappable lines unchanged."""
    out: List[str] = []
    for line in lines:
        wrapped = wrap_long_fortran_line(line, max_len=max_len)
        if wrapped is None:
            out.append(line)
        else:
            out.extend(wrapped)
    return out


def remove_empty_if_blocks(lines: List[str]) -> List[str]:
    """Remove conservative empty IF blocks:

      if (<cond>) then
         ! comments / blank only
      end if

    Keeps blocks that contain executable statements or any ELSE/ELSE IF branch.
    """
    out: List[str] = []
    i = 0
    if_then_re = re.compile(r"^\s*if\s*\(.*\)\s*then\s*$", re.IGNORECASE)
    else_re = re.compile(r"^\s*else(\s+if\b.*)?\s*$", re.IGNORECASE)
    end_if_re = re.compile(r"^\s*end\s*if\b", re.IGNORECASE)

    while i < len(lines):
        code_i = strip_comment(lines[i]).strip()
        if not if_then_re.match(code_i):
            out.append(lines[i])
            i += 1
            continue

        depth = 1
        j = i + 1
        has_else = False
        has_exec = False
        end_idx = -1
        while j < len(lines):
            code_j = strip_comment(lines[j]).strip()
            low_j = code_j.lower()
            if if_then_re.match(code_j):
                depth += 1
                has_exec = True
                j += 1
                continue
            if end_if_re.match(low_j):
                depth -= 1
                if depth == 0:
                    end_idx = j
                    break
                has_exec = True
                j += 1
                continue
            if depth == 1 and else_re.match(code_j):
                has_else = True
                j += 1
                continue
            if code_j:
                has_exec = True
            j += 1

        if end_idx >= 0 and (not has_else) and (not has_exec):
            i = end_idx + 1
            continue

        out.append(lines[i])
        i += 1

    return out


def _split_code_comment(line: str) -> Tuple[str, str]:
    """Split a Fortran source line into code and trailing comment."""
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
            return line[:i], line[i:]
    return line, ""


def _line_scope_context_map(lines: List[str]) -> Dict[int, str]:
    """Return map of 1-based line number to simple scope context text."""
    out: Dict[int, str] = {}
    module_name: Optional[str] = None
    proc_stack: List[Tuple[str, str]] = []

    mod_start_re = re.compile(r"^\s*module\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
    mod_end_re = re.compile(r"^\s*end\s*module\b", re.IGNORECASE)
    proc_end_re = re.compile(r"^\s*end\s*(function|subroutine)\b", re.IGNORECASE)

    for lineno, raw in enumerate(lines, start=1):
        code = strip_comment(raw).strip()
        low = code.lower()

        # End statements first.
        if proc_end_re.match(low):
            if proc_stack:
                proc_stack.pop()
        elif mod_end_re.match(low):
            module_name = None

        parts: List[str] = []
        if module_name:
            parts.append(f"module {module_name}")
        if proc_stack:
            k, nm = proc_stack[-1]
            parts.append(f"{k} {nm}")
        out[lineno] = "::".join(parts)

        # Start statements apply from the next line onward.
        m_mod = mod_start_re.match(code)
        if m_mod and not re.match(r"^\s*module\s+procedure\b", low):
            module_name = m_mod.group(1).lower()
            continue
        m_proc = PROC_START_RE.match(code)
        if m_proc:
            proc_stack.append((m_proc.group("kind").lower(), m_proc.group("name").lower()))

    return out


def normalize_location_tag_separators(lines: List[str]) -> List[str]:
    """Normalize scoped location tags by collapsing spaces around '::'."""
    out: List[str] = []
    bracket_re = re.compile(r"\[[^\]\n]*\]")
    for line in lines:
        def _fix(m: re.Match[str]) -> str:
            txt = m.group(0)
            inner = txt[1:-1]
            inner = re.sub(r"\s*::\s*", "::", inner)
            return f"[{inner}]"

        out.append(bracket_re.sub(_fix, line))
    return out


def append_error_stop_locations(
    lines: List[str],
    *,
    file_label: Optional[str] = None,
) -> Tuple[List[str], int]:
    """Append ``[file:line]`` to literal ``error stop`` messages.

    Only rewrites conservative forms where ``error stop`` is followed by a
    single quoted literal (double or single quotes) on the same line.
    """
    out: List[str] = []
    changed = 0
    err_re = re.compile(r"\berror\s+stop\b", re.IGNORECASE)
    tagged_re = re.compile(r"\[[^][]*(?::\d+|::line\s+\d+)\]\s*$", re.IGNORECASE)
    label = file_label if file_label else "<input>"
    scope_map = _line_scope_context_map(lines)

    def _loc_for_line(n: int) -> str:
        ctx = scope_map.get(n, "")
        if ctx:
            return f"{label}::{ctx}::line {n}"
        return f"{label}::line {n}"

    for lineno, raw in enumerate(lines, start=1):
        eol = ""
        body = raw
        if body.endswith("\r\n"):
            body = body[:-2]
            eol = "\r\n"
        elif body.endswith("\n"):
            body = body[:-1]
            eol = "\n"

        code, comment = _split_code_comment(body)
        m = err_re.search(code)
        if not m:
            out.append(raw)
            continue
        i = m.end()
        while i < len(code) and code[i].isspace():
            i += 1
        if i >= len(code) or code[i] not in ("'", '"'):
            out.append(raw)
            continue

        quote = code[i]
        j = i + 1
        while j < len(code):
            if code[j] == quote:
                if j + 1 < len(code) and code[j + 1] == quote:
                    j += 2
                    continue
                break
            j += 1
        if j >= len(code) or code[j] != quote:
            out.append(raw)
            continue
        if code[j + 1 :].strip():
            out.append(raw)
            continue

        msg = code[i + 1 : j]
        if tagged_re.search(msg):
            out.append(raw)
            continue

        msg = f"{msg} [{_loc_for_line(lineno)}]"
        new_code = code[: i + 1] + msg + quote + code[j + 1 :]
        out.append(new_code + comment + eol)
        changed += 1

    return out, changed


def rewrite_error_stop_blocks_with_condition_values(
    lines: List[str],
    *,
    file_label: Optional[str] = None,
    msg_len: int = 1000,
) -> Tuple[List[str], int]:
    """Rewrite ``if (...) then`` + literal ``error stop`` to include values.

    Conservative rewrite only when the IF block has exactly one executable
    statement and it is ``error stop "<literal>"`` (no ELSE branch).
    """
    out: List[str] = []
    changed = 0
    i = 0
    label = file_label if file_label else "<input>"
    if_re = re.compile(r"^(\s*)if\s*\((.*)\)\s*then\s*$", re.IGNORECASE)
    if_then_re = re.compile(r"^\s*if\s*\(.*\)\s*then\s*$", re.IGNORECASE)
    else_re = re.compile(r"^\s*else(\s+if\b.*)?\s*$", re.IGNORECASE)
    end_if_re = re.compile(r"^\s*end\s*if\b", re.IGNORECASE)
    err_re = re.compile(r"\berror\s+stop\b", re.IGNORECASE)
    kw_exclude = {
        "if",
        "then",
        "and",
        "or",
        "not",
        "eqv",
        "neqv",
        "true",
        "false",
        "present",
        "allocated",
        "associated",
        "size",
        "all",
        "any",
        "count",
        "int",
        "real",
        "logical",
        "abs",
        "max",
        "min",
        "mod",
    }
    scope_map = _line_scope_context_map(lines)

    def _loc_for_line(n: int) -> str:
        ctx = scope_map.get(n, "")
        if ctx:
            return f"{label}::{ctx}::line {n}"
        return f"{label}::line {n}"

    def _quote_dq(s: str) -> str:
        return s.replace('"', '""')

    def _parse_one_line_if(code_line: str) -> Optional[Tuple[str, str, str]]:
        m_if0 = re.match(r"^(\s*)if\s*\(", code_line, re.IGNORECASE)
        if not m_if0:
            return None
        indent = m_if0.group(1)
        if " then" in code_line.lower():
            return None
        lpar = code_line.find("(", m_if0.start())
        if lpar < 0:
            return None
        depth = 0
        rpar = -1
        for pos in range(lpar, len(code_line)):
            ch = code_line[pos]
            if ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
                if depth == 0:
                    rpar = pos
                    break
        if rpar < 0:
            return None
        cond = code_line[lpar + 1 : rpar].strip()
        tail = code_line[rpar + 1 :].strip()
        if not cond or not tail:
            return None
        return indent, cond, tail

    def _extract_vars(cond: str) -> List[str]:
        found: List[str] = []
        seen: Set[str] = set()
        low = cond.lower()
        protected_spans: List[Tuple[int, int]] = []

        # Prefer reporting SIZE(arg) explicitly when present.
        for m_sz in re.finditer(r"\bsize\s*\(\s*([a-z][a-z0-9_]*)\s*\)", low):
            token = f"size({m_sz.group(1)})"
            if token not in seen:
                seen.add(token)
                found.append(token)
            protected_spans.append((m_sz.start(), m_sz.end()))

        def _in_protected(pos: int) -> bool:
            for a, b in protected_spans:
                if a <= pos < b:
                    return True
            return False

        for m in re.finditer(r"\b([a-z][a-z0-9_]*)\b", low):
            if _in_protected(m.start()):
                continue
            name = m.group(1)
            if name in kw_exclude:
                continue
            k = m.end()
            while k < len(low) and low[k].isspace():
                k += 1
            if k < len(low) and low[k] == "(":
                continue
            if name not in seen:
                seen.add(name)
                found.append(name)
        return found

    while i < len(lines):
        raw = lines[i]
        code_i = strip_comment(raw).strip()
        parsed_inline = _parse_one_line_if(strip_comment(raw).rstrip())
        if parsed_inline is not None:
            indent, cond, tail_stmt = parsed_inline
            m_err = err_re.search(tail_stmt)
            if m_err:
                p = m_err.end()
                while p < len(tail_stmt) and tail_stmt[p].isspace():
                    p += 1
                if p < len(tail_stmt) and tail_stmt[p] in ("'", '"'):
                    q = tail_stmt[p]
                    r = p + 1
                    while r < len(tail_stmt):
                        if tail_stmt[r] == q:
                            if r + 1 < len(tail_stmt) and tail_stmt[r + 1] == q:
                                r += 2
                                continue
                            break
                        r += 1
                    if r < len(tail_stmt) and tail_stmt[r] == q and not tail_stmt[r + 1 :].strip():
                        # Conservative mode: if the next significant source line
                        # is END IF, treat source as structurally invalid and skip.
                        nxt = i + 1
                        while nxt < len(lines) and not strip_comment(lines[nxt]).strip():
                            nxt += 1
                        if nxt < len(lines):
                            nxt_code = strip_comment(lines[nxt]).strip().lower()
                            if end_if_re.match(nxt_code):
                                out.append(raw)
                                i += 1
                                continue

                        eol = "\n"
                        if raw.endswith("\r\n"):
                            eol = "\r\n"
                        elif raw.endswith("\n"):
                            eol = "\n"
                        i3 = indent + " " * 3
                        i6 = indent + " " * 6
                        vars_in_cond = _extract_vars(cond)
                        msg_base = f"{cond} [{_loc_for_line(i + 1)}]"
                        fmt = "(a" + ",a,g0" * len(vars_in_cond) + ")"
                        args: List[str] = [f"\"{_quote_dq(msg_base)}\""]
                        for idx, name in enumerate(vars_in_cond):
                            sep = ": " if idx == 0 else ", "
                            args.append(f"\"{_quote_dq(sep + name + ' = ')}\"")
                            args.append(name)
                        arg_text = ", ".join(args)

                        out.append(f"{indent}if ({cond}) then{eol}")
                        out.append(f"{i3}block{eol}")
                        out.append(f"{i6}character(len={msg_len}) :: msg{eol}")
                        out.append(f"{i6}write(msg, \"{fmt}\") {arg_text}{eol}")
                        out.append(f"{i6}error stop trim(msg){eol}")
                        out.append(f"{i3}end block{eol}")
                        out.append(f"{indent}end if{eol}")
                        changed += 1
                        i += 1
                        continue

        m_if = if_re.match(code_i)
        if not m_if:
            out.append(raw)
            i += 1
            continue

        depth = 1
        j = i + 1
        has_else = False
        end_idx = -1
        while j < len(lines):
            code_j = strip_comment(lines[j]).strip()
            if if_then_re.match(code_j):
                depth += 1
                j += 1
                continue
            if end_if_re.match(code_j):
                depth -= 1
                if depth == 0:
                    end_idx = j
                    break
                j += 1
                continue
            if depth == 1 and else_re.match(code_j):
                has_else = True
            j += 1
        if end_idx < 0 or has_else:
            out.append(raw)
            i += 1
            continue

        body_idxs = list(range(i + 1, end_idx))
        exec_idxs = [k for k in body_idxs if strip_comment(lines[k]).strip()]
        if len(exec_idxs) != 1:
            out.append(raw)
            i += 1
            continue
        err_idx = exec_idxs[0]
        err_code = strip_comment(lines[err_idx]).strip()
        m_err = err_re.search(err_code)
        if not m_err:
            out.append(raw)
            i += 1
            continue
        p = m_err.end()
        while p < len(err_code) and err_code[p].isspace():
            p += 1
        if p >= len(err_code) or err_code[p] not in ("'", '"'):
            out.append(raw)
            i += 1
            continue
        q = err_code[p]
        r = p + 1
        while r < len(err_code):
            if err_code[r] == q:
                if r + 1 < len(err_code) and err_code[r + 1] == q:
                    r += 2
                    continue
                break
            r += 1
        if r >= len(err_code) or err_code[r] != q or err_code[r + 1 :].strip():
            out.append(raw)
            i += 1
            continue

        cond = m_if.group(2).strip()
        vars_in_cond = _extract_vars(cond)
        indent = m_if.group(1)
        eol = "\n"
        if raw.endswith("\r\n"):
            eol = "\r\n"
        elif raw.endswith("\n"):
            eol = "\n"
        i3 = indent + " " * 3
        i6 = indent + " " * 6
        msg_base = f"{cond} [{_loc_for_line(err_idx + 1)}]"
        fmt = "(a" + ",a,g0" * len(vars_in_cond) + ")"
        args: List[str] = [f"\"{_quote_dq(msg_base)}\""]
        for idx, name in enumerate(vars_in_cond):
            sep = ": " if idx == 0 else ", "
            args.append(f"\"{_quote_dq(sep + name + ' = ')}\"")
            args.append(name)
        arg_text = ", ".join(args)

        out.append(raw)
        out.append(f"{i3}block{eol}")
        out.append(f"{i6}character(len={msg_len}) :: msg{eol}")
        out.append(f"{i6}write(msg, \"{fmt}\") {arg_text}{eol}")
        out.append(f"{i6}error stop trim(msg){eol}")
        out.append(f"{i3}end block{eol}")
        out.append(lines[end_idx])
        changed += 1
        i = end_idx + 1

    return out, changed


def _strip_outer_parens(s: str) -> str:
    """Strip one or more layers of redundant outer parentheses."""
    t = s.strip()
    while t.startswith("(") and t.endswith(")"):
        depth = 0
        ok = True
        in_single = False
        in_double = False
        for idx, ch in enumerate(t):
            if ch == "'" and not in_double:
                in_single = not in_single
            elif ch == '"' and not in_single:
                in_double = not in_double
            elif not in_single and not in_double:
                if ch == "(":
                    depth += 1
                elif ch == ")":
                    depth -= 1
                    if depth == 0 and idx != len(t) - 1:
                        ok = False
                        break
        if ok and depth == 0:
            t = t[1:-1].strip()
        else:
            break
    return t


def _tokenize_const_logical_expr(expr: str) -> Optional[List[str]]:
    """Tokenize a logical expression containing only literal logical tokens."""
    s = expr.strip().lower()
    tokens: List[str] = []
    i = 0
    while i < len(s):
        ch = s[i]
        if ch.isspace():
            i += 1
            continue
        if ch in "()":
            tokens.append(ch)
            i += 1
            continue
        if s.startswith(".true.", i):
            tokens.append(".true.")
            i += 6
            continue
        if s.startswith(".false.", i):
            tokens.append(".false.")
            i += 7
            continue
        if s.startswith(".not.", i):
            tokens.append(".not.")
            i += 5
            continue
        if s.startswith(".and.", i):
            tokens.append(".and.")
            i += 5
            continue
        if s.startswith(".or.", i):
            tokens.append(".or.")
            i += 4
            continue
        if s.startswith(".eqv.", i):
            tokens.append(".eqv.")
            i += 5
            continue
        if s.startswith(".neqv.", i):
            tokens.append(".neqv.")
            i += 6
            continue
        return None
    return tokens


def _eval_const_logical_expr(expr: str, *, aggressive: bool) -> Optional[bool]:
    """Evaluate logical expressions that are compile-time constants."""
    s = _strip_outer_parens(expr)
    if not aggressive:
        # Conservative default: only literal true/false with optional .not. chain.
        t = re.sub(r"\s+", "", s.lower())
        t = t.replace("(", "").replace(")", "")
        not_count = 0
        while t.startswith(".not."):
            not_count += 1
            t = t[5:]
        if t not in (".true.", ".false."):
            return None
        base = (t == ".true.")
        return (not base) if (not_count % 2 == 1) else base

    tokens = _tokenize_const_logical_expr(s)
    if tokens is None:
        return None
    pos = 0

    def parse_primary() -> Optional[bool]:
        nonlocal pos
        if pos >= len(tokens):
            return None
        tk = tokens[pos]
        if tk == ".true.":
            pos += 1
            return True
        if tk == ".false.":
            pos += 1
            return False
        if tk == "(":
            pos += 1
            val = parse_or()
            if val is None or pos >= len(tokens) or tokens[pos] != ")":
                return None
            pos += 1
            return val
        return None

    def parse_not() -> Optional[bool]:
        nonlocal pos
        if pos < len(tokens) and tokens[pos] == ".not.":
            pos += 1
            rhs = parse_not()
            return (None if rhs is None else (not rhs))
        return parse_primary()

    def parse_and() -> Optional[bool]:
        nonlocal pos
        left = parse_not()
        if left is None:
            return None
        while pos < len(tokens) and tokens[pos] == ".and.":
            pos += 1
            right = parse_not()
            if right is None:
                return None
            left = left and right
        return left

    def parse_or() -> Optional[bool]:
        nonlocal pos
        left = parse_and()
        if left is None:
            return None
        while pos < len(tokens) and tokens[pos] in (".or.", ".eqv.", ".neqv."):
            op = tokens[pos]
            pos += 1
            right = parse_and()
            if right is None:
                return None
            if op == ".or.":
                left = left or right
            elif op == ".eqv.":
                left = (left == right)
            else:
                left = (left != right)
        return left

    val = parse_or()
    if val is None or pos != len(tokens):
        return None
    return val


def simplify_constant_if_blocks(lines: List[str], *, aggressive: bool = False) -> List[str]:
    """Fold IF constructs whose conditions are compile-time logical constants.

    Conservative default (`aggressive=False`) simplifies only literal-constant
    conditions like `.true.`, `.false.`, and optional `.not.` chains.
    Aggressive mode also folds constant logical expressions over
    `.true./.false.` with `.not./.and./.or./.eqv./.neqv.` and parentheses.
    """
    if_then_re = re.compile(r"^(?P<indent>\s*)if\s*\((?P<cond>.*)\)\s*then\s*$", re.IGNORECASE)
    elseif_re = re.compile(r"^\s*else\s*if\s*\((?P<cond>.*)\)\s*then\s*$", re.IGNORECASE)
    else_re = re.compile(r"^\s*else\s*$", re.IGNORECASE)
    end_if_re = re.compile(r"^\s*end\s*if\b", re.IGNORECASE)
    one_if_re = re.compile(r"^(?P<indent>\s*)if\s*\((?P<cond>[^)]*)\)\s*(?P<stmt>.+?)\s*$", re.IGNORECASE)

    def _deindent_branch(branch_lines: List[str], if_indent: str) -> List[str]:
        """De-indent selected folded branch by one block level."""
        if not branch_lines:
            return branch_lines
        nonblank = [ln for ln in branch_lines if ln.strip()]
        if not nonblank:
            return branch_lines

        min_extra: Optional[int] = None
        for ln in nonblank:
            m = re.match(r"^(\s*)", ln)
            lead = m.group(1) if m else ""
            if not lead.startswith(if_indent):
                continue
            extra = len(lead) - len(if_indent)
            if extra <= 0:
                continue
            if min_extra is None or extra < min_extra:
                min_extra = extra
        if not min_extra:
            return branch_lines

        out_b: List[str] = []
        for ln in branch_lines:
            if not ln.strip():
                out_b.append(ln)
                continue
            m = re.match(r"^(\s*)", ln)
            lead = m.group(1) if m else ""
            if lead.startswith(if_indent) and len(lead) >= len(if_indent) + min_extra:
                new_lead = if_indent + lead[len(if_indent) + min_extra :]
                out_b.append(new_lead + ln[len(lead) :])
            else:
                out_b.append(ln)
        return out_b

    def _recurse(seg: List[str]) -> List[str]:
        out: List[str] = []
        i = 0
        while i < len(seg):
            code_i, cmt_i = _split_code_comment(seg[i])
            stmt_i = code_i.strip()
            m_block = if_then_re.match(code_i)
            if m_block:
                depth = 1
                j = i + 1
                branches: List[Tuple[str, Optional[str], int, int]] = []
                branch_kind = "if"
                branch_cond: Optional[str] = m_block.group("cond")
                branch_start = i + 1
                end_idx = -1
                while j < len(seg):
                    code_j = strip_comment(seg[j]).strip()
                    if if_then_re.match(code_j):
                        depth += 1
                        j += 1
                        continue
                    if end_if_re.match(code_j):
                        depth -= 1
                        if depth == 0:
                            branches.append((branch_kind, branch_cond, branch_start, j))
                            end_idx = j
                            break
                        j += 1
                        continue
                    if depth == 1:
                        m_elseif = elseif_re.match(code_j)
                        if m_elseif:
                            branches.append((branch_kind, branch_cond, branch_start, j))
                            branch_kind = "elseif"
                            branch_cond = m_elseif.group("cond")
                            branch_start = j + 1
                            j += 1
                            continue
                        if else_re.match(code_j):
                            branches.append((branch_kind, branch_cond, branch_start, j))
                            branch_kind = "else"
                            branch_cond = None
                            branch_start = j + 1
                            j += 1
                            continue
                    j += 1

                if end_idx < 0:
                    out.append(seg[i])
                    i += 1
                    continue

                evals: List[Tuple[str, Optional[bool], int, int]] = []
                can_fold = True
                for kind, cond, bstart, bend in branches:
                    if kind == "else":
                        evals.append((kind, None, bstart, bend))
                        continue
                    cval = _eval_const_logical_expr(cond or "", aggressive=aggressive)
                    if cval is None:
                        can_fold = False
                        break
                    evals.append((kind, cval, bstart, bend))
                if not can_fold:
                    out.append(seg[i])
                    i += 1
                    continue

                chosen: Optional[Tuple[int, int]] = None
                for kind, cval, bstart, bend in evals:
                    if kind == "else":
                        chosen = (bstart, bend)
                        break
                    if cval:
                        chosen = (bstart, bend)
                        break
                if chosen is not None:
                    selected = _recurse(seg[chosen[0] : chosen[1]])
                    selected = _deindent_branch(selected, m_block.group("indent"))
                    out.extend(selected)
                i = end_idx + 1
                continue

            m_one = one_if_re.match(code_i)
            if m_one:
                stmt_rhs = m_one.group("stmt").strip()
                if stmt_rhs.lower() != "then":
                    cval = _eval_const_logical_expr(m_one.group("cond"), aggressive=aggressive)
                    if cval is True:
                        rebuilt = f"{m_one.group('indent')}{stmt_rhs}"
                        if cmt_i:
                            rebuilt = f"{rebuilt} {cmt_i.strip()}"
                        out.append(rebuilt)
                        i += 1
                        continue
                    if cval is False:
                        i += 1
                        continue

            out.append(seg[i])
            i += 1
        return out

    return _recurse(lines)


def demote_fixed_size_single_allocatables(lines: List[str]) -> List[str]:
    """Demote simple allocatables to fixed-size arrays when safely possible.

    Conservative pattern:
    - declaration: `<type>, allocatable :: x(:)` (single entity)
    - exactly one matching `allocate(x(<shape>))`
    - no `deallocate(x)`, `allocated(x)`, or `move_alloc(..., x)` use

    Rewrites declaration to `<type> :: x(<shape>)` and removes the ALLOCATE.
    """
    out = list(lines)
    decl_re = re.compile(
        r"^(?P<indent>\s*)(?P<head>[^:][^:]*)\s*,\s*allocatable\s*::\s*(?P<name>[a-z][a-z0-9_]*)\s*\(\s*:\s*\)\s*$",
        re.IGNORECASE,
    )

    decls: Dict[str, int] = {}
    for i, raw in enumerate(out):
        code, comment = _split_code_comment(raw.rstrip("\r\n"))
        if comment.strip():
            continue
        m = decl_re.match(code.strip())
        if m is not None:
            decls[m.group("name").lower()] = i

    for name, d_idx in list(decls.items()):
        alloc_re = re.compile(
            rf"^\s*allocate\s*\(\s*{re.escape(name)}\s*\(\s*(?P<shape>[^)]+?)\s*\)\s*\)\s*$",
            re.IGNORECASE,
        )
        alloc_hits: List[Tuple[int, str]] = []
        blocked = False
        for i, raw in enumerate(out):
            code = strip_comment(raw).strip()
            if not code:
                continue
            ma = alloc_re.match(code)
            if ma is not None:
                alloc_hits.append((i, ma.group("shape").strip()))
            if re.search(rf"\bdeallocate\s*\(\s*{re.escape(name)}\s*\)", code, re.IGNORECASE):
                blocked = True
            if re.search(rf"\ballocated\s*\(\s*{re.escape(name)}\s*\)", code, re.IGNORECASE):
                blocked = True
            if re.search(rf"\bmove_alloc\s*\([^)]*,\s*{re.escape(name)}\s*\)", code, re.IGNORECASE):
                blocked = True
        if blocked or len(alloc_hits) != 1:
            continue

        a_idx, shape = alloc_hits[0]
        d_code, d_comment = _split_code_comment(out[d_idx].rstrip("\r\n"))
        md = decl_re.match(d_code.strip())
        if md is None:
            continue
        indent = md.group("indent")
        head = md.group("head").strip()
        eol_d = _line_eol(out[d_idx]) or "\n"
        out[d_idx] = f"{indent}{head} :: {name}({shape}){d_comment}{eol_d}"
        out[a_idx] = ""

    return [ln for ln in out if ln != ""]


def coalesce_adjacent_allocate_statements(lines: List[str], max_len: int = 80) -> List[str]:
    """Merge adjacent single-object ALLOCATE statements.

    Conservative scope:
    - only adjacent lines of form `allocate(<one-object>)`
    - skips lines with inline comments
    - skips ALLOCATE entries that use keyword args (`=`), e.g. SOURCE=
    - only merges statements with the same indentation
    - wraps using free-form continuation if merged line would exceed max_len
    """
    out: List[str] = []
    i = 0
    alloc_re = re.compile(r"^\s*allocate\s*\((.*)\)\s*$", re.IGNORECASE)
    while i < len(lines):
        raw = lines[i]
        code0 = raw.rstrip("\r\n")
        code, comment = _split_code_comment(code0)
        if comment.strip():
            out.append(raw)
            i += 1
            continue
        m = alloc_re.match(code.strip())
        if not m:
            out.append(raw)
            i += 1
            continue
        indent = code[: len(code) - len(code.lstrip())]
        items = _split_top_level_commas(m.group(1).strip())
        if len(items) != 1 or "=" in items[0]:
            out.append(raw)
            i += 1
            continue
        objs = [items[0].strip()]
        j = i + 1
        eol = _line_eol(raw) or "\n"
        while j < len(lines):
            rj = lines[j]
            codej0 = rj.rstrip("\r\n")
            codej, commentj = _split_code_comment(codej0)
            if commentj.strip():
                break
            mj = alloc_re.match(codej.strip())
            if not mj:
                break
            indentj = codej[: len(codej) - len(codej.lstrip())]
            if indentj != indent:
                break
            parts = _split_top_level_commas(mj.group(1).strip())
            if len(parts) != 1 or "=" in parts[0]:
                break
            objs.append(parts[0].strip())
            eol = _line_eol(rj) or eol
            j += 1

        if len(objs) == 1:
            out.append(raw)
            i += 1
            continue

        merged = f"{indent}allocate({', '.join(objs)})"
        if len(merged) <= max_len:
            out.append(f"{merged}{eol}")
        else:
            out.append(f"{indent}allocate({objs[0]}, &{eol}")
            for obj in objs[1:-1]:
                out.append(f"{indent}   & {obj}, &{eol}")
            out.append(f"{indent}   & {objs[-1]}){eol}")
        i = j
    return out


def coalesce_contiguous_scalar_assignments_to_constructor(lines: List[str]) -> List[str]:
    """Merge adjacent scalar-index assignments into one constructor assignment.

    Example:
      A(1)=x1; A(2)=x2; A(3)=x3
    becomes:
      A(1:3) = [x1, x2, x3]

    Conservative scope:
    - same indent, no inline comments
    - simple form `name(<integer literal>) = <expr>`
    - contiguous indices increasing by 1
    """
    out: List[str] = []
    i = 0
    asn_re = re.compile(
        r"^(\s*)([a-z][a-z0-9_]*)\s*\(\s*([+-]?\d+)\s*\)\s*=\s*(.+?)\s*$",
        re.IGNORECASE,
    )
    while i < len(lines):
        raw = lines[i]
        code0 = raw.rstrip("\r\n")
        code, comment = _split_code_comment(code0)
        if comment.strip():
            out.append(raw)
            i += 1
            continue
        m = asn_re.match(code)
        if not m:
            out.append(raw)
            i += 1
            continue
        indent = m.group(1)
        name = m.group(2)
        idx0 = int(m.group(3))
        rhs_vals = [m.group(4).strip()]
        j = i + 1
        idx = idx0
        eol = _line_eol(raw) or "\n"
        while j < len(lines):
            rj = lines[j]
            codej0 = rj.rstrip("\r\n")
            codej, commentj = _split_code_comment(codej0)
            if commentj.strip():
                break
            mj = asn_re.match(codej)
            if not mj:
                break
            if mj.group(1) != indent or mj.group(2).lower() != name.lower():
                break
            next_idx = int(mj.group(3))
            if next_idx != idx + 1:
                break
            rhs_vals.append(mj.group(4).strip())
            idx = next_idx
            eol = _line_eol(rj) or eol
            j += 1
        if len(rhs_vals) == 1:
            out.append(raw)
            i += 1
            continue
        out.append(f"{indent}{name}({idx0}:{idx}) = [{', '.join(rhs_vals)}]{eol}")
        i = j
    return out


def collapse_random_number_element_loops(lines: List[str]) -> List[str]:
    """Collapse simple elementwise RANDOM_NUMBER loops to whole-array calls.

    Rewrites:
      do i = 1, n
         call random_number(x(i))
      end do
    as:
      call random_number(x)
    """
    out: List[str] = []
    i = 0
    do_re = re.compile(
        r"^(?P<indent>\s*)do\s+(?P<ivar>[a-z][a-z0-9_]*)\s*=\s*1\s*,\s*(?P<ub>[^!]+?)\s*$",
        re.IGNORECASE,
    )
    call_re = re.compile(
        r"^(?P<indent>\s*)call\s+random_number\s*\(\s*(?P<arr>[a-z][a-z0-9_]*)\s*\(\s*(?P<idx>[a-z][a-z0-9_]*)\s*\)\s*\)\s*$",
        re.IGNORECASE,
    )
    end_re = re.compile(r"^\s*end\s*do\s*$", re.IGNORECASE)

    while i < len(lines):
        if i + 2 >= len(lines):
            out.append(lines[i])
            i += 1
            continue
        c0, cm0 = _split_code_comment(lines[i].rstrip("\r\n"))
        c1, cm1 = _split_code_comment(lines[i + 1].rstrip("\r\n"))
        c2, cm2 = _split_code_comment(lines[i + 2].rstrip("\r\n"))
        m0 = do_re.match(c0.strip())
        m1 = call_re.match(c1.strip())
        m2 = end_re.match(c2.strip())
        if (
            m0 is not None
            and m1 is not None
            and m2 is not None
            and not cm0.strip()
            and not cm1.strip()
            and not cm2.strip()
            and m0.group("ivar").lower() == m1.group("idx").lower()
        ):
            indent = m0.group("indent")
            arr = m1.group("arr")
            nl = _line_eol(lines[i]) or "\n"
            out.append(f"{indent}call random_number({arr}){nl}")
            i += 3
            continue
        out.append(lines[i])
        i += 1
    return out


def _is_simple_parameter_value(expr: str) -> bool:
    """True for conservative scalar values safe for PARAMETER initialization."""
    s = expr.strip()
    if not s:
        return False
    if re.fullmatch(r"[+-]?\d+", s):
        return True
    if re.fullmatch(r"[+-]?(?:\d+\.\d*|\.\d+|\d+)(?:[de][+-]?\d+)?(?:_[a-z][a-z0-9_]*)?", s, re.IGNORECASE):
        return True
    if re.fullmatch(r"\.(?:true|false)\.", s, re.IGNORECASE):
        return True
    if (len(s) >= 2 and s[0] == "'" and s[-1] == "'") or (len(s) >= 2 and s[0] == '"' and s[-1] == '"'):
        return True
    return False


def promote_scalar_constants_to_parameters(lines: List[str]) -> List[str]:
    """Promote scalar locals assigned once to simple constants into PARAMETERs.

    Example:
      integer :: i, m, n
      m = 2
      n = 3
    ->
      integer :: i
      integer, parameter :: m = 2, n = 3
    """
    out = list(lines)
    unit_start_re = re.compile(
        r"^\s*(?:[a-z][a-z0-9_()\s=,:]*\s+)?(?:function|subroutine)\b|^\s*program\b",
        re.IGNORECASE,
    )
    unit_end_re = re.compile(r"^\s*end\s+(?:function|subroutine|program)\b", re.IGNORECASE)
    declish_re = re.compile(
        r"^\s*(?:implicit\b|use\b|integer\b|real\b|logical\b|character\b|complex\b|type\b|class\b|procedure\b|save\b|parameter\b|external\b|intrinsic\b|common\b|equivalence\b|dimension\b)",
        re.IGNORECASE,
    )
    do_var_re = re.compile(r"^\s*do\s+([a-z_]\w*)\s*=", re.IGNORECASE)
    asn_re = re.compile(r"^\s*([a-z_]\w*)\s*=\s*(.+?)\s*$", re.IGNORECASE)

    i = 0
    while i < len(out):
        if not unit_start_re.match(out[i].strip()):
            i += 1
            continue
        u_start = i
        j = i + 1
        while j < len(out) and not unit_end_re.match(out[j].strip()):
            j += 1
        u_end = j  # exclusive

        # declaration section start/end
        k = u_start + 1
        while k < u_end and (not out[k].strip() or out[k].lstrip().startswith("!")):
            k += 1
        decl_start = k
        while k < u_end:
            s = out[k].strip()
            if not s or s.startswith("!") or declish_re.match(s):
                k += 1
                continue
            break
        exec_start = k

        decl_info: Dict[str, Tuple[int, str, str]] = {}
        for di in range(decl_start, exec_start):
            code, _comment = _split_code_comment(out[di].rstrip("\r\n"))
            if "::" not in code:
                continue
            lhs, rhs = code.split("::", 1)
            lhs_spec = lhs.strip()
            if "parameter" in lhs_spec.lower():
                continue
            indent = code[: len(code) - len(code.lstrip())]
            for ent in _split_top_level_commas(rhs):
                e = ent.strip()
                if not e or "=" in e or "(" in e:
                    continue
                mname = re.fullmatch(r"([a-z_]\w*)", e, re.IGNORECASE)
                if not mname:
                    continue
                name = mname.group(1).lower()
                if name not in decl_info:
                    decl_info[name] = (di, lhs_spec, indent)

        assign_count: Dict[str, int] = {}
        assign_rhs: Dict[str, str] = {}
        do_assigned: Set[str] = set()
        for ei in range(exec_start, u_end):
            code, _comment = _split_code_comment(out[ei].rstrip("\r\n"))
            s = code.strip()
            if not s:
                continue
            md = do_var_re.match(s)
            if md:
                do_assigned.add(md.group(1).lower())
            ma = asn_re.match(s)
            if ma:
                name = ma.group(1).lower()
                rhs = ma.group(2).strip()
                assign_count[name] = assign_count.get(name, 0) + 1
                assign_rhs[name] = rhs

        promote: Dict[str, str] = {}
        for name, (_decl_i, _spec, _indent) in decl_info.items():
            if do_assigned.__contains__(name):
                continue
            if assign_count.get(name, 0) != 1:
                continue
            rhs = assign_rhs.get(name, "")
            if not _is_simple_parameter_value(rhs):
                continue
            promote[name] = rhs

        if not promote:
            i = u_end + 1
            continue

        remove_assign_idx: Set[int] = set()
        for ei in range(exec_start, u_end):
            code, _comment = _split_code_comment(out[ei].rstrip("\r\n"))
            ma = asn_re.match(code.strip())
            if not ma:
                continue
            name = ma.group(1).lower()
            if name in promote:
                remove_assign_idx.add(ei)

        by_decl: Dict[int, List[Tuple[str, str, str]]] = {}
        for name, rhs in promote.items():
            decl_i, spec, indent = decl_info[name]
            by_decl.setdefault(decl_i, []).append((name, rhs, spec))

        new_out: List[str] = []
        for idx, line in enumerate(out):
            if idx in remove_assign_idx:
                continue
            if idx not in by_decl:
                new_out.append(line)
                continue
            remove_names = {nm for nm, _rhs, _spec in by_decl[idx]}
            rewritten, _changed = rewrite_decl_remove_names(line, remove_names)
            if rewritten is not None:
                new_out.append(rewritten)
            code, _comment = _split_code_comment(line.rstrip("\r\n"))
            indent = code[: len(code) - len(code.lstrip())]
            eol = _line_eol(line) or "\n"
            grouped: Dict[str, List[Tuple[str, str]]] = {}
            for nm, rhs, spec in by_decl[idx]:
                grouped.setdefault(spec, []).append((nm, rhs))
            for spec, pairs in grouped.items():
                rhs_text = ", ".join(f"{nm} = {rv}" for nm, rv in sorted(pairs))
                new_out.append(f"{indent}{spec}, parameter :: {rhs_text}{eol}")
        out = new_out
        i = u_start + 1

    return out


def _parse_numeric_literal_value(tok: str) -> Optional[float]:
    """Parse a simple Fortran numeric literal token to float, else None."""
    s = tok.strip()
    if not s:
        return None
    # Strip kind suffix: 1.0_dp, 1_int32, etc.
    m = re.match(r"^([+-]?(?:\d+\.\d*|\.\d+|\d+)(?:[de][+-]?\d+)?)(?:_[a-z][a-z0-9_]*|_\d+)?$", s, re.IGNORECASE)
    if not m:
        return None
    core = m.group(1).replace("d", "e").replace("D", "E")
    try:
        return float(core)
    except ValueError:
        return None


def compact_consecutive_constructor_literals_to_implied_do(lines: List[str], min_items: int = 4) -> List[str]:
    """Rewrite constructors with consecutive numeric literals as implied-do.

    Example:
      a = [1.0, 2.0, 3.0, 4.0]
    ->
      a = [(1.0 + (i-1) * (2.0 - 1.0), i=1,4)]

    Conservative scope:
    - assignment form `lhs = [ ... ]` on one line, no inline comment
    - constructor has at least `min_items` numeric literal items
    - values form an arithmetic progression with nonzero constant step
    - uses an existing local integer scalar name (prefers `i`)
    """
    out = list(lines)
    unit_start_re = re.compile(
        r"^\s*(?:[a-z][a-z0-9_()\s=,:]*\s+)?(?:function|subroutine)\b|^\s*program\b",
        re.IGNORECASE,
    )
    unit_end_re = re.compile(r"^\s*end\s+(?:function|subroutine|program)\b", re.IGNORECASE)
    declish_re = re.compile(
        r"^\s*(?:implicit\b|use\b|integer\b|real\b|logical\b|character\b|complex\b|type\b|class\b|procedure\b|save\b|parameter\b|external\b|intrinsic\b|common\b|equivalence\b|dimension\b)",
        re.IGNORECASE,
    )
    asn_ctor_re = re.compile(r"^(\s*[^=][^=]*?=\s*)\[(.*)\]\s*$")

    i = 0
    while i < len(out):
        if not unit_start_re.match(out[i].strip()):
            i += 1
            continue
        u_start = i
        j = i + 1
        while j < len(out) and not unit_end_re.match(out[j].strip()):
            j += 1
        u_end = j

        # Find declaration section and collect integer scalar locals.
        k = u_start + 1
        while k < u_end and (not out[k].strip() or out[k].lstrip().startswith("!")):
            k += 1
        decl_start = k
        while k < u_end:
            s = out[k].strip()
            if not s or s.startswith("!") or declish_re.match(s):
                k += 1
                continue
            break
        exec_start = k

        int_scalars: Set[str] = set()
        real_kind_by_name: Dict[str, str] = {}
        for di in range(decl_start, exec_start):
            code, _comment = _split_code_comment(out[di].rstrip("\r\n"))
            if "::" not in code:
                continue
            lhs, rhs = code.split("::", 1)
            lhs_low = lhs.lower()
            if re.match(r"^\s*integer\b", lhs, re.IGNORECASE):
                if re.search(r"\bparameter\b", lhs, re.IGNORECASE):
                    continue
                for ent in _split_top_level_commas(rhs):
                    e = ent.strip()
                    if not e or "=" in e or "(" in e:
                        continue
                    mname = re.match(r"^([a-z_]\w*)$", e, re.IGNORECASE)
                    if mname:
                        int_scalars.add(mname.group(1))
            if re.match(r"^\s*real\b", lhs, re.IGNORECASE):
                mk = re.search(r"\bkind\s*=\s*([a-z_]\w*)", lhs_low, re.IGNORECASE)
                if not mk:
                    continue
                kname = mk.group(1)
                for ent in _split_top_level_commas(rhs):
                    e = ent.strip()
                    if not e:
                        continue
                    mname = re.match(r"^([a-z_]\w*)", e, re.IGNORECASE)
                    if mname:
                        real_kind_by_name[mname.group(1).lower()] = kname

        if not int_scalars:
            i = u_end + 1
            continue
        ivar = "i" if "i" in {n.lower() for n in int_scalars} else sorted(int_scalars, key=str.lower)[0]

        for ei in range(exec_start, u_end):
            raw = out[ei]
            code0 = raw.rstrip("\r\n")
            code, comment = _split_code_comment(code0)
            if comment.strip():
                continue
            m = asn_ctor_re.match(code.strip())
            if not m:
                continue
            lhs_eq = m.group(1).strip()
            rhs_inner = m.group(2).strip()
            items = _split_top_level_commas(rhs_inner)
            if len(items) < min_items:
                continue
            vals: List[float] = []
            ok = True
            for it in items:
                v = _parse_numeric_literal_value(it.strip())
                if v is None:
                    ok = False
                    break
                vals.append(v)
            if not ok or len(vals) < min_items:
                continue
            step = vals[1] - vals[0]
            if abs(step) < 1.0e-15:
                continue
            if any(abs((vals[p] - vals[p - 1]) - step) > 1.0e-12 for p in range(2, len(vals))):
                continue

            first = items[0].strip()
            second = items[1].strip()
            eol = _line_eol(raw) or "\n"
            indent = re.match(r"^\s*", code).group(0) if code else ""
            lhs_name = lhs_eq[:-1].rstrip()
            mbase = re.match(r"^\s*([a-z_]\w*)", lhs_name, re.IGNORECASE)
            base_name = mbase.group(1).lower() if mbase else ""

            # Preferred form for real(kind=K) targets with integer-step literals:
            #   real([(i, i=s,e)], kind=K)
            all_int_like = all(abs(v - round(v)) <= 1.0e-12 for v in vals)
            start_i = int(round(vals[0])) if all_int_like else 0
            if (
                base_name in real_kind_by_name
                and all_int_like
                and abs(step - 1.0) <= 1.0e-12
            ):
                end_i = start_i + len(vals) - 1
                kname = real_kind_by_name[base_name]
                rhs = f"real([({ivar}, {ivar}={start_i},{end_i})], kind={kname})"
            else:
                expr = f"{first} + ({ivar}-1) * ({second} - {first})"
                rhs = f"[({expr}, {ivar}=1,{len(items)})]"
            out[ei] = f"{indent}{lhs_name} = {rhs}{eol}"

        i = u_end + 1
    return out


def _replace_tokens_with_case_map(code: str, case_map: Dict[str, str]) -> str:
    """Replace identifier tokens in code (outside strings) using case_map."""
    out: List[str] = []
    i = 0
    in_single = False
    in_double = False
    while i < len(code):
        ch = code[i]
        if ch == "'" and not in_double:
            in_single = not in_single
            out.append(ch)
            i += 1
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            out.append(ch)
            i += 1
            continue
        if not in_single and not in_double and (ch.isalpha() or ch == "_"):
            j = i + 1
            while j < len(code) and (code[j].isalnum() or code[j] == "_"):
                j += 1
            tok = code[i:j]
            repl = case_map.get(tok.lower())
            out.append(repl if repl is not None else tok)
            i = j
            continue
        out.append(ch)
        i += 1
    return "".join(out)


def normalize_identifier_case_to_declarations(lines: List[str]) -> List[str]:
    """Normalize variable identifier case to match declaration spelling per unit.

    For each program/function/subroutine unit, collect declared entity names and
    rewrite identifier tokens in that unit (outside comments/strings) to match
    the declaration case.
    """
    out = list(lines)
    unit_start_re = re.compile(
        r"^\s*(?:[a-z][a-z0-9_()\s=,:]*\s+)?(?:function|subroutine)\b|^\s*program\b",
        re.IGNORECASE,
    )
    unit_end_re = re.compile(r"^\s*end\s+(?:function|subroutine|program)\b", re.IGNORECASE)

    i = 0
    while i < len(out):
        if not unit_start_re.match(out[i].strip()):
            i += 1
            continue
        u_start = i
        j = i + 1
        while j < len(out) and not unit_end_re.match(out[j].strip()):
            j += 1
        u_end = j  # exclusive

        case_map: Dict[str, str] = {}
        for k in range(u_start, u_end):
            raw = out[k].rstrip("\r\n")
            code, _comment = _split_code_comment(raw)
            if "::" not in code:
                continue
            _lhs, rhs = code.split("::", 1)
            for ent in _split_top_level_commas(rhs):
                m = re.match(r"^\s*([a-z_]\w*)", ent, re.IGNORECASE)
                if not m:
                    continue
                name = m.group(1)
                case_map.setdefault(name.lower(), name)

        if not case_map:
            i = u_end + 1
            continue

        for k in range(u_start, u_end):
            raw = out[k]
            eol = _line_eol(raw) or "\n"
            body = raw.rstrip("\r\n")
            code, comment = _split_code_comment(body)
            if not code.strip():
                continue
            new_code = _replace_tokens_with_case_map(code, case_map)
            out[k] = f"{new_code}{comment}{eol}"

        i = u_end + 1
    return out


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


def indent_fortran_blocks(
    text: str,
    *,
    indent_step: int = 3,
    indent_proc: bool = False,
    indent_module: bool = False,
    indent_program: bool = False,
    indent_contains: bool = False,
) -> str:
    """Indent Fortran control blocks (do/if/select/case/block) consistently.

    This is a lightweight, line-based indenter intended for generated code.
    It keeps blank lines and re-indents code/comment lines according to
    surrounding control-flow nesting.
    """
    lines = text.splitlines()
    out: List[str] = []
    level = 0
    step = " " * max(0, indent_step)

    dedent_before = re.compile(
        r"^\s*(?:"
        r"end\s+(?:do|if|select|block|associate|where|type)"
        r"|else(?:\s+if\b.*\bthen)?"
        r"|case\b(?:\s+default|\s*\()?"
        r")\b",
        re.IGNORECASE,
    )
    indent_after = re.compile(
        r"^\s*(?:"
        r"do\b"
        r"|if\b.*\bthen\b\s*$"
        r"|select\s+case\b"
        r"|block\b"
        r"|else(?:\s+if\b.*\bthen)?\b"
        r"|case\b(?:\s+default|\s*\()?"
        r")",
        re.IGNORECASE,
    )
    type_start_re = re.compile(r"^\s*type\b(?!\s*\()", re.IGNORECASE)

    def _is_derived_type_start(code_line: str) -> bool:
        c = code_line.strip()
        if not c or not type_start_re.match(c):
            return False
        if re.match(r"^\s*type\s+is\s*\(", c, re.IGNORECASE):
            return False
        if re.match(r"^\s*type\s*\(", c, re.IGNORECASE):
            return False
        return "::" in c and not re.match(r"^\s*end\s+type\b", c, re.IGNORECASE)

    unit_start_re = re.compile(
        r"^\s*(?:(?:pure|elemental|impure|recursive|module)\s+)*(?:(?:[a-z][a-z0-9_]*(?:\s*\([^)]*\))?)\s+)?(function|subroutine)\b",
        re.IGNORECASE,
    )
    module_start_re = re.compile(r"^\s*module\s+[a-z][a-z0-9_]*\b", re.IGNORECASE)
    module_proc_re = re.compile(r"^\s*module\s+procedure\b", re.IGNORECASE)
    program_start_re = re.compile(r"^\s*program\b", re.IGNORECASE)
    end_module_re = re.compile(r"^\s*end\s+module\b", re.IGNORECASE)
    end_program_re = re.compile(r"^\s*end\s+program\b", re.IGNORECASE)
    end_proc_re = re.compile(r"^\s*end\s+(function|subroutine)\b", re.IGNORECASE)
    contains_re = re.compile(r"^\s*contains\b", re.IGNORECASE)

    unit_stack: List[dict] = []

    def _unit_kind_start(code_line: str) -> Optional[str]:
        c = code_line.strip()
        if not c:
            return None
        if re.match(r"^\s*end\b", c, re.IGNORECASE):
            return None
        if module_proc_re.match(c):
            return None
        if module_start_re.match(c):
            return "module"
        if program_start_re.match(c):
            return "program"
        m = unit_start_re.match(c)
        if m:
            return m.group(1).lower()
        return None

    def _unit_end_matches(code_line: str, kind: str) -> bool:
        c = code_line.strip()
        if kind == "module":
            return bool(end_module_re.match(c))
        if kind == "program":
            return bool(end_program_re.match(c))
        return bool(end_proc_re.match(c))

    for raw in lines:
        stripped = raw.strip()
        if not stripped:
            out.append("")
            continue

        code = strip_comment(raw).strip()
        if code and unit_stack and _unit_end_matches(code, unit_stack[-1]["kind"]):
            if unit_stack[-1].get("contains_active", False):
                level = max(0, level - 1)
            if unit_stack[-1].get("unit_indent", False):
                level = max(0, level - 1)
            unit_stack.pop()

        if code and dedent_before.match(code):
            level = max(0, level - 1)

        out.append(f"{step * level}{stripped}")

        if code and _is_derived_type_start(code):
            level += 1
            continue

        if code and contains_re.match(code) and unit_stack:
            if indent_contains and not unit_stack[-1].get("contains_active", False):
                unit_stack[-1]["contains_active"] = True
                level += 1
            continue

        if code:
            k = _unit_kind_start(code)
            if k is not None:
                indent_unit = (
                    (k == "module" and indent_module)
                    or (k == "program" and indent_program)
                    or (k in {"function", "subroutine"} and indent_proc)
                )
                unit_stack.append({"kind": k, "unit_indent": bool(indent_unit), "contains_active": False})
                if indent_unit:
                    level += 1
                continue

        if code and indent_after.match(code):
            # Exclude one-line IF from opening a new block unless it ends with THEN.
            if re.match(r"^\s*if\b", code, re.IGNORECASE) and not re.search(r"\bthen\s*$", code, re.IGNORECASE):
                continue
            # END lines already handled as dedent-only.
            if not re.match(r"^\s*end\b", code, re.IGNORECASE):
                level += 1

    return "\n".join(out) + ("\n" if text.endswith("\n") or out else "")


def split_statements_to_lines(lines: Iterable[str]) -> List[str]:
    """Expand semicolon-delimited statements so each returned item is one statement."""
    out: List[str] = []
    for _lineno, stmt in iter_fortran_statements(lines):
        out.append(stmt)
    return out


def _split_top_level_commas(text: str) -> List[str]:
    out: List[str] = []
    cur: List[str] = []
    depth = 0
    in_single = False
    in_double = False
    i = 0
    while i < len(text):
        ch = text[i]
        if ch == "'" and not in_double:
            if in_single and i + 1 < len(text) and text[i + 1] == "'":
                cur.append("''")
                i += 2
                continue
            in_single = not in_single
            cur.append(ch)
            i += 1
            continue
        if ch == '"' and not in_single:
            if in_double and i + 1 < len(text) and text[i + 1] == '"':
                cur.append('""')
                i += 2
                continue
            in_double = not in_double
            cur.append(ch)
            i += 1
            continue
        if not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")" and depth > 0:
                depth -= 1
            elif ch == "," and depth == 0:
                out.append("".join(cur).strip())
                cur = []
                i += 1
                continue
        cur.append(ch)
        i += 1
    tail = "".join(cur).strip()
    if tail:
        out.append(tail)
    return out


def _split_code_comment(line: str) -> Tuple[str, str]:
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


def ensure_space_before_inline_comments(lines: List[str]) -> List[str]:
    """Ensure there is a space before inline `!` comments on code lines."""
    out: List[str] = []
    for raw in lines:
        eol = _line_eol(raw)
        body = raw[:-len(eol)] if eol else raw
        code, comment = _split_code_comment(body)
        if comment and code.strip():
            out.append(f"{code.rstrip()} {comment.lstrip()}{eol}")
        else:
            out.append(raw)
    return out


def _line_eol(line: str) -> str:
    if line.endswith("\r\n"):
        return "\r\n"
    if line.endswith("\n"):
        return "\n"
    return ""


def rewrite_decl_remove_names(line: str, remove_names: Set[str]) -> Tuple[Optional[str], bool]:
    """Remove selected entity names from one declaration line."""
    code, comment = _split_code_comment(line.rstrip("\r\n"))
    if "::" not in code:
        return line, False
    lhs, rhs = code.split("::", 1)
    ents = _split_top_level_commas(rhs)
    kept: List[str] = []
    changed = False
    for ent in ents:
        m = re.match(r"^\s*([a-z][a-z0-9_]*)", ent, re.IGNORECASE)
        if m and m.group(1).lower() in remove_names:
            changed = True
            continue
        kept.append(ent.strip())
    if not changed:
        return line, False
    if not kept:
        return None, True
    return f"{lhs.rstrip()} :: {', '.join(kept)}{comment}{_line_eol(line)}", True


def inline_temp_assign_into_immediate_use(
    lines: List[str],
    *,
    require_write_stmt: bool = False,
) -> List[str]:
    """Inline `v = expr` into next nonblank statement when `v` is single-use.

    Conservative behavior:
    - assignment must be single-line `v = expr` (no ';' or '&')
    - `v` must appear exactly once in next statement
    - `v` must not be used elsewhere in same unit (excluding declaration)
    - optionally require next statement to be WRITE
    - removes now-unused declaration entities for inlined vars
    """
    out = list(lines)
    unit_start_re = re.compile(
        r"^\s*(?:[a-z][a-z0-9_()\s=,:]*\s+)?(?:function|subroutine)\b|^\s*program\b",
        re.IGNORECASE,
    )
    unit_end_re = re.compile(r"^\s*end\s+(?:function|subroutine|program)\b", re.IGNORECASE)
    assign_re = re.compile(r"^\s*([a-z][a-z0-9_]*)\s*=\s*(.+)\s*$", re.IGNORECASE)
    write_re = re.compile(r"^\s*write\s*\(", re.IGNORECASE)
    ident_re = re.compile(r"[a-z][a-z0-9_]*", re.IGNORECASE)

    unit_ranges: List[Tuple[int, int]] = []
    s: Optional[int] = None
    for i, raw in enumerate(out):
        code = strip_comment(raw).strip()
        if not code:
            continue
        if s is None and unit_start_re.match(code):
            s = i
            continue
        if s is not None and unit_end_re.match(code):
            unit_ranges.append((s, i))
            s = None
    if s is not None:
        unit_ranges.append((s, len(out) - 1))

    for us, ue in unit_ranges:
        removed_vars: Set[str] = set()
        i = us
        while i <= ue:
            code_i, _ = _split_code_comment(out[i].rstrip("\r\n"))
            stmt = code_i.strip()
            m = assign_re.match(stmt)
            if not m:
                i += 1
                continue
            var = m.group(1).lower()
            rhs = m.group(2).strip()
            if ";" in stmt or "&" in stmt:
                i += 1
                continue
            if any(tok.group(0).lower() == var for tok in ident_re.finditer(rhs)):
                i += 1
                continue
            j = i + 1
            while j <= ue and not strip_comment(out[j]).strip():
                j += 1
            if j > ue:
                i += 1
                continue
            code_j, comment_j = _split_code_comment(out[j].rstrip("\r\n"))
            if require_write_stmt and not write_re.match(code_j.strip()):
                i += 1
                continue
            occ = [mm for mm in ident_re.finditer(code_j) if mm.group(0).lower() == var]
            if len(occ) != 1:
                i += 1
                continue
            use_count = 0
            for k in range(us, ue + 1):
                code_k, _ = _split_code_comment(out[k].rstrip("\r\n"))
                if k == i or "::" in code_k:
                    continue
                use_count += sum(1 for mm in ident_re.finditer(code_k) if mm.group(0).lower() == var)
            if use_count != 1:
                i += 1
                continue
            m0 = occ[0]
            new_code_j = f"{code_j[:m0.start()]}{rhs}{code_j[m0.end():]}"
            out[j] = f"{new_code_j}{comment_j}{_line_eol(out[j]) or '\n'}"
            out[i] = ""
            removed_vars.add(var)
            i = j + 1

        if removed_vars:
            for k in range(us, ue + 1):
                code_k, _ = _split_code_comment(out[k].rstrip("\r\n"))
                if "::" not in code_k:
                    continue
                present: Set[str] = set()
                for v in removed_vars:
                    if any(mm.group(0).lower() == v for mm in ident_re.finditer(code_k)):
                        present.add(v)
                if not present:
                    continue
                to_remove: Set[str] = set()
                for v in present:
                    used_elsewhere = False
                    for kk in range(us, ue + 1):
                        if kk == k:
                            continue
                        code_kk, _ = _split_code_comment(out[kk].rstrip("\r\n"))
                        if "::" in code_kk:
                            continue
                        if any(mm.group(0).lower() == v for mm in ident_re.finditer(code_kk)):
                            used_elsewhere = True
                            break
                    if not used_elsewhere:
                        to_remove.add(v)
                if not to_remove:
                    continue
                new_ln, _changed = rewrite_decl_remove_names(out[k], to_remove)
                out[k] = "" if new_ln is None else new_ln

    return [ln for ln in out if ln != ""]


def prune_unused_use_only_lines(lines: List[str]) -> List[str]:
    """Drop unused USE, ONLY entities per program unit; remove empty USE lines.

    Conservative scope:
    - free-form, single-line USE statements (no continuation '&').
    - only USE lines with explicit ONLY lists are edited.
    """
    unit_start_re = re.compile(
        r"^\s*(?:[a-z][a-z0-9_()\s=,:]*\s+)?(?:function|subroutine)\b|^\s*program\b",
        re.IGNORECASE,
    )
    unit_end_re = re.compile(r"^\s*end\s+(?:function|subroutine|program)\b", re.IGNORECASE)
    use_only_re = re.compile(r"^\s*use\b.*\bonly\s*:\s*(.+)$", re.IGNORECASE)

    out = list(lines)
    unit_ranges: List[Tuple[int, int]] = []
    cur_start: Optional[int] = None
    for i, raw in enumerate(out):
        code = strip_comment(raw).strip()
        if not code:
            continue
        if cur_start is None and unit_start_re.match(code):
            cur_start = i
            continue
        if cur_start is not None and unit_end_re.match(code):
            unit_ranges.append((cur_start, i))
            cur_start = None
    if cur_start is not None:
        unit_ranges.append((cur_start, len(out) - 1))

    for s, t in unit_ranges:
        used_ids: Set[str] = set()
        use_line_idxs: List[int] = []
        for i in range(s, t + 1):
            raw = out[i]
            code = strip_comment(raw)
            if use_only_re.match(code.strip()) and "&" not in code:
                use_line_idxs.append(i)
                continue
            for m in re.finditer(r"[a-z][a-z0-9_]*", code, re.IGNORECASE):
                used_ids.add(m.group(0).lower())

        for i in use_line_idxs:
            raw = out[i]
            code, comment = _split_code_comment(raw.rstrip("\r\n"))
            if "&" in code:
                continue
            m = re.search(r"^(?P<prefix>\s*use\b.*?\bonly\s*:\s*)(?P<tail>.+)$", code, re.IGNORECASE)
            if not m:
                continue
            prefix = m.group("prefix")
            tail = m.group("tail")
            parts = _split_top_level_commas(tail)
            kept_parts: List[str] = []
            for p in parts:
                ent = p.strip()
                low = ent.lower()
                if low.startswith("operator(") or low.startswith("assignment("):
                    kept_parts.append(ent)
                    continue
                local = ""
                if "=>" in ent:
                    local = ent.split("=>", 1)[0].strip().lower()
                else:
                    local = (base_identifier(ent) or "").lower()
                if not local:
                    kept_parts.append(ent)
                    continue
                if local in used_ids:
                    kept_parts.append(ent)
            if not kept_parts:
                out[i] = ""
                continue
            new_code = f"{prefix}{', '.join(kept_parts)}"
            out[i] = f"{new_code}{comment}{_line_eol(raw)}"
    return [ln for ln in out if ln != ""]


FORTRAN_RESERVED_IDENTIFIERS: Set[str] = {
    # statements/keywords
    "program", "module", "subroutine", "function", "contains", "implicit", "none",
    "if", "then", "else", "end", "do", "select", "case", "where", "forall",
    "call", "use", "only", "result", "integer", "real", "logical", "character",
    "complex", "type", "class", "public", "private", "interface", "procedure",
    "allocate", "deallocate", "return", "stop", "print", "read", "write", "open",
    "close", "rewind", "backspace", "flush", "inquire", "intent", "in", "out",
    "inout", "value", "optional", "allocatable", "pointer", "parameter", "save",
    "target", "pure", "elemental", "recursive", "impure",
    # common intrinsics often collided in transpiled names
    "sum", "product", "minval", "maxval", "matmul", "transpose", "dot_product",
    "reshape", "spread", "pack", "count", "norm2", "abs", "sqrt", "floor", "mod",
    "int", "real", "size", "lbound", "ubound", "merge", "random_number", "random_seed",
}


def _replace_identifiers_outside_strings(code: str, mapping: Dict[str, str]) -> str:
    if not mapping:
        return code
    out: List[str] = []
    i = 0
    in_single = False
    in_double = False
    while i < len(code):
        ch = code[i]
        if ch == "'" and not in_double:
            out.append(ch)
            if in_single and i + 1 < len(code) and code[i + 1] == "'":
                out.append("'")
                i += 2
                continue
            in_single = not in_single
            i += 1
            continue
        if ch == '"' and not in_single:
            out.append(ch)
            if in_double and i + 1 < len(code) and code[i + 1] == '"':
                out.append('"')
                i += 2
                continue
            in_double = not in_double
            i += 1
            continue
        if in_single or in_double:
            out.append(ch)
            i += 1
            continue
        if re.match(r"[a-z_]", ch, re.IGNORECASE):
            j = i + 1
            while j < len(code) and re.match(r"[a-z0-9_]", code[j], re.IGNORECASE):
                j += 1
            tok = code[i:j]
            repl = mapping.get(tok.lower())
            out.append(repl if repl is not None else tok)
            i = j
            continue
        out.append(ch)
        i += 1
    return "".join(out)


def avoid_reserved_identifier_definitions(
    lines: List[str],
    *,
    forbidden: Optional[Set[str]] = None,
) -> List[str]:
    """Rename defined procedures/declared entities that collide with reserved names."""
    bad = {x.lower() for x in (forbidden or FORTRAN_RESERVED_IDENTIFIERS)}
    proc_re = re.compile(
        r"^\s*(?:(?:double\s+precision|integer|real|logical|complex|character\b(?:\s*\([^)]*\))?|type\s*\([^)]*\)|class\s*\([^)]*\))\s*(?:\([^)]*\))?\s*,?\s*)?(?:(?:pure|elemental|impure|recursive|module)\s+)*(function|subroutine)\s+([a-z][a-z0-9_]*)\b",
        re.IGNORECASE,
    )
    defined: Set[str] = set()
    proc_names: Set[str] = set()
    var_names: Set[str] = set()
    for raw in lines:
        code = strip_comment(raw).strip().lower()
        if not code:
            continue
        for m in re.finditer(r"[a-z][a-z0-9_]*", code, re.IGNORECASE):
            defined.add(m.group(0).lower())
        mp = proc_re.match(code)
        if mp:
            proc_names.add(mp.group(2).lower())
        if "::" in code and not re.match(r"^\s*use\b", code, re.IGNORECASE):
            var_names.update(parse_declared_names_from_decl(code))

    mapping: Dict[str, str] = {}
    used = set(defined)

    def mk_name(base: str, suffix: str) -> str:
        cand = f"{base}{suffix}"
        while cand.lower() in used or cand.lower() in bad:
            cand = cand + "_"
        used.add(cand.lower())
        return cand

    for n in sorted(proc_names):
        if n in bad:
            mapping[n] = mk_name(n, "_f")
    for n in sorted(var_names):
        if n in bad and n not in mapping:
            mapping[n] = mk_name(n, "_v")
    if not mapping:
        return lines

    out: List[str] = []
    for raw in lines:
        code, comment = _split_code_comment(raw.rstrip("\r\n"))
        eol = _line_eol(raw)
        out.append(f"{_replace_identifiers_outside_strings(code, mapping)}{comment}{eol}")
    return out


def _extract_ident_reads(expr: str, tracked: Set[str]) -> Set[str]:
    out: Set[str] = set()
    for m in re.finditer(r"[a-z][a-z0-9_]*", expr, re.IGNORECASE):
        n = m.group(0).lower()
        if n in tracked:
            out.add(n)
    return out


def _rhs_has_disallowed_calls(rhs: str, declared: Set[str]) -> bool:
    """True when RHS contains likely impure/unknown calls."""
    allowed_intrinsics = {
        "size",
        "lbound",
        "ubound",
        "kind",
        "len",
        "int",
        "real",
        "dble",
        "floor",
        "sqrt",
        "abs",
        "min",
        "max",
        "mod",
        "merge",
    }
    for m in re.finditer(r"([a-z][a-z0-9_]*)\s*\(", rhs, re.IGNORECASE):
        name = m.group(1).lower()
        if name in allowed_intrinsics:
            continue
        if name in declared:
            # likely array reference
            continue
        return True
    return False


def find_set_but_never_read_local_edits(lines: List[str]) -> DeadStoreEdits:
    """Find conservative edits for locals that are written but never read.

    Scope:
    - Free-form Fortran, statement-level scan.
    - Per program/function/subroutine unit.
    - Removes only local declaration entities and safe assignment statements.
    """
    edits = DeadStoreEdits()
    stmts = iter_fortran_statements(lines)
    unit_start_re = re.compile(
        r"^\s*(?:[a-z][a-z0-9_()\s=,:]*\s+)?(?:function|subroutine)\b|^\s*program\b",
        re.IGNORECASE,
    )
    unit_end_re = re.compile(r"^\s*end\s+(?:function|subroutine|program)\b", re.IGNORECASE)
    decl_re = re.compile(
        r"^\s*(?:integer|real|logical|character|complex|type|class)\b",
        re.IGNORECASE,
    )
    assign_re = re.compile(r"^\s*([a-z][a-z0-9_]*(?:\([^)]*\))?)\s*=\s*(.+)$", re.IGNORECASE)
    do_re = re.compile(r"^\s*do\s+([a-z][a-z0-9_]*)\s*=\s*(.+)$", re.IGNORECASE)
    unit_sig_re = re.compile(
        r"^\s*(?:(?:pure|elemental|impure|recursive|module)\s+)*(function|subroutine)\s+([a-z][a-z0-9_]*)\s*(\([^)]*\))?",
        re.IGNORECASE,
    )

    i = 0
    while i < len(stmts):
        ln, st = stmts[i]
        if not unit_start_re.match(st.strip()):
            i += 1
            continue
        j = i + 1
        while j < len(stmts) and not unit_end_re.match(stmts[j][1].strip()):
            j += 1
        if j >= len(stmts):
            break
        body = stmts[i + 1 : j]
        unit_stmt = stmts[i][1].strip().lower()
        skip_names: Set[str] = set()
        m_sig = unit_sig_re.match(unit_stmt)
        if m_sig:
            argtxt = m_sig.group(3) or ""
            if argtxt.startswith("(") and argtxt.endswith(")"):
                for tok in argtxt[1:-1].split(","):
                    n = tok.strip().lower()
                    if re.match(r"^[a-z][a-z0-9_]*$", n):
                        skip_names.add(n)
            if m_sig.group(1).lower() == "function":
                skip_names.add(m_sig.group(2).lower())
            m_res = re.search(r"\bresult\s*\(\s*([a-z][a-z0-9_]*)\s*\)", unit_stmt, re.IGNORECASE)
            if m_res:
                skip_names.add(m_res.group(1).lower())

        decl_line_by_name: Dict[str, int] = {}
        declared: Set[str] = set()
        writes: Dict[str, Set[int]] = {}
        reads: Dict[str, Set[int]] = {}
        assign_stmt_line_by_var: Dict[str, Set[int]] = {}

        for sln, s in body:
            low = s.strip().lower()
            if not low:
                continue
            if decl_re.match(low) and "::" in low:
                names = parse_declared_names_from_decl(low)
                for n in names:
                    declared.add(n)
                    decl_line_by_name.setdefault(n, sln)
                if "=" in low and "=>" not in low:
                    # inline init: mark writes and read deps from rhs
                    rhs = low.split("::", 1)[1]
                    for ent in rhs.split(","):
                        if "=" in ent and "=>" not in ent:
                            lhs, rr = ent.split("=", 1)
                            bn = base_identifier(lhs) or ""
                            if bn in declared:
                                writes.setdefault(bn, set()).add(sln)
                            for r in _extract_ident_reads(rr, declared):
                                reads.setdefault(r, set()).add(sln)
                continue

            m_as = assign_re.match(low)
            if m_as:
                lhs = m_as.group(1)
                rhs = m_as.group(2)
                bn = base_identifier(lhs) or ""
                if bn in declared:
                    writes.setdefault(bn, set()).add(sln)
                    assign_stmt_line_by_var.setdefault(bn, set()).add(sln)
                for r in _extract_ident_reads(rhs, declared):
                    reads.setdefault(r, set()).add(sln)
                continue

            m_do = do_re.match(low)
            if m_do:
                iv = m_do.group(1).lower()
                rest = m_do.group(2)
                if iv in declared:
                    writes.setdefault(iv, set()).add(sln)
                for r in _extract_ident_reads(rest, declared):
                    reads.setdefault(r, set()).add(sln)
                continue

            for r in _extract_ident_reads(low, declared):
                reads.setdefault(r, set()).add(sln)

        dead = [n for n in declared if n not in skip_names and writes.get(n) and not reads.get(n)]
        for n in dead:
            dln = decl_line_by_name.get(n)
            if dln is not None:
                edits.decl_remove_by_line.setdefault(dln, set()).add(n)
            for sln in sorted(assign_stmt_line_by_var.get(n, set())):
                raw = lines[sln - 1]
                code = strip_comment(raw).strip()
                m_asn = assign_re.match(code)
                if not m_asn:
                    continue
                lhs = m_asn.group(1)
                rhs = m_asn.group(2)
                if base_identifier(lhs) != n:
                    continue
                if ";" in code or "&" in code:
                    continue
                if _rhs_has_disallowed_calls(rhs, declared):
                    continue
                edits.remove_stmt_lines.add(sln)

        i = j + 1
    return edits


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


def normalize_numeric_leading_zeros_text(text: str) -> str:
    """Normalize numeric tokens like `.5`/`-.5` to `0.5`/`-0.5` in text output."""
    out = re.sub(r"(?<![\w.])-\.(\d+(?:[eE][+-]?\d+)?)", r"-0.\1", text)
    out = re.sub(r"(?<![\w.])\+\.(\d+(?:[eE][+-]?\d+)?)", r"+0.\1", out)
    out = re.sub(r"(?<![\w.+-])\.(\d+(?:[eE][+-]?\d+)?)", r"0.\1", out)
    return out


@dataclass
class ProcedureInterface:
    name: str
    args: List[str]
    optional_args: Set[str]


def _split_top_level_commas_expr(text: str) -> List[str]:
    parts: List[str] = []
    cur: List[str] = []
    depth = 0
    in_single = False
    in_double = False
    i = 0
    n = len(text)
    while i < n:
        ch = text[i]
        if ch == "'" and not in_double:
            in_single = not in_single
            cur.append(ch)
            i += 1
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            cur.append(ch)
            i += 1
            continue
        if not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")" and depth > 0:
                depth -= 1
            elif ch == "," and depth == 0:
                parts.append("".join(cur).strip())
                cur = []
                i += 1
                continue
        cur.append(ch)
        i += 1
    tail = "".join(cur).strip()
    if tail:
        parts.append(tail)
    return parts


def _is_literal_expr_for_named_arg(arg: str) -> bool:
    t = arg.strip()
    if re.match(r"^[+-]?\d+[a-zA-Z0-9_]*$", t):
        return True
    if re.match(r"^[+-]?(?:\d+\.\d*|\d*\.\d+|\d+[eEdD][+-]?\d+)(?:_[A-Za-z]\w*)?$", t):
        return True
    if re.match(r"^\.(?:true|false)\.$", t, re.IGNORECASE):
        return True
    if re.match(r"^(?:'([^']|'')*'|\"([^\"]|\"\")*\")$", t):
        return True
    return False


def _collect_declared_dummy_names(stmt: str) -> List[str]:
    try:
        ents = parse_declared_entities(stmt)
        out: List[str] = []
        for e in ents:
            if e.name:
                out.append(e.name.lower())
        return out
    except Exception:
        return []


def collect_procedure_interfaces_from_lines(lines: List[str]) -> Dict[str, ProcedureInterface]:
    """Collect available explicit interfaces from procedure definitions in source."""
    out: Dict[str, ProcedureInterface] = {}
    proc_stack: List[Tuple[str, List[str], Set[str]]] = []
    proc_start_re = re.compile(
        r"^\s*(?:pure\s+|elemental\s+|recursive\s+|impure\s+|module\s+|pure\s+module\s+|elemental\s+module\s+|recursive\s+module\s+)*"
        r"(?:subroutine|(?:[a-z][a-z0-9_]*(?:\s*\([^)]*\))?\s+)?function)\s+([a-z][a-z0-9_]*)\s*\(([^)]*)\)",
        re.IGNORECASE,
    )
    end_proc_re = re.compile(r"^\s*end\s*(?:subroutine|function)?\b", re.IGNORECASE)

    for _lineno, stmt in iter_fortran_statements(lines):
        low = stmt.strip().lower()
        m = proc_start_re.match(low)
        if m:
            name = m.group(1).lower()
            arg_text = m.group(2).strip()
            args = [a.strip().lower() for a in arg_text.split(",") if a.strip()]
            proc_stack.append((name, args, set()))
            continue
        if proc_stack:
            name, args, optional_args = proc_stack[-1]
            if "::" in low and "optional" in low:
                for nm in _collect_declared_dummy_names(stmt):
                    if nm in args:
                        optional_args.add(nm)
                proc_stack[-1] = (name, args, optional_args)
            if end_proc_re.match(low):
                out[name] = ProcedureInterface(name=name, args=args, optional_args=set(optional_args))
                proc_stack.pop()
    return out


def _find_call_like_segments(code: str) -> List[Tuple[int, int, str, str]]:
    """Find `name(args)` segments, returning (start,end,name,args)."""
    out: List[Tuple[int, int, str, str]] = []
    i = 0
    n = len(code)
    in_single = False
    in_double = False
    while i < n:
        ch = code[i]
        if ch == "'" and not in_double:
            in_single = not in_single
            i += 1
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            i += 1
            continue
        if in_single or in_double:
            i += 1
            continue
        if ch.isalpha():
            j = i + 1
            while j < n and (code[j].isalnum() or code[j] == "_"):
                j += 1
            name = code[i:j]
            k = j
            while k < n and code[k].isspace():
                k += 1
            if k < n and code[k] == "(":
                depth = 0
                p = k
                while p < n:
                    cp = code[p]
                    if cp == "(":
                        depth += 1
                    elif cp == ")":
                        depth -= 1
                        if depth == 0:
                            args = code[k + 1 : p]
                            out.append((i, p + 1, name, args))
                            i = p + 1
                            break
                    p += 1
                else:
                    i = k + 1
                    continue
                continue
            i = j
            continue
        i += 1
    return out


def rewrite_named_arguments_in_statement(
    code: str,
    interfaces: Dict[str, ProcedureInterface],
    *,
    max_positional: int = 3,
) -> Tuple[str, bool, Set[str]]:
    """Rewrite call arguments to named form when literals/optional/long positional lists.

    Returns `(rewritten_code, changed, unavailable_procedures_seen)`.
    """
    unknown: Set[str] = set()
    m_call_head = re.match(r"^\s*call\s+([a-z][a-z0-9_]*)\s*\(", code, re.IGNORECASE)
    call_head_name = m_call_head.group(1).lower() if m_call_head else None
    intrinsic_ignore = {
        "abs", "acos", "aimag", "aint", "all", "anint", "any", "asin", "atan", "atan2",
        "ceiling", "cmplx", "conjg", "cos", "cosh", "count", "cpu_time", "dble", "digits",
        "dot_product", "epsilon", "exp", "floor", "huge", "iachar", "ichar", "index", "int",
        "kind", "len", "log", "log10", "matmul", "max", "maxval", "merge", "min", "minval",
        "mod", "nint", "pack", "present", "product", "real", "reshape", "scan", "selected_int_kind",
        "selected_real_kind", "shape", "sign", "sin", "sinh", "size", "spacing", "spread", "sqrt",
        "sum", "tan", "tanh", "tiny", "trim", "ubound", "lbound",
    }
    segments = _find_call_like_segments(code)
    if not segments:
        return code, False, unknown

    pieces: List[str] = []
    last = 0
    changed = False

    for start, end, name_raw, arg_text in segments:
        name = name_raw.lower()
        iface = interfaces.get(name)
        if iface is None:
            # Avoid obvious declarations/casts.
            if not re.match(r"^(real|integer|logical|character|type|kind|len)$", name, re.IGNORECASE):
                if call_head_name == name and name not in intrinsic_ignore:
                    unknown.add(name)
            pieces.append(code[last:end])
            last = end
            continue
        args_in = _split_top_level_commas_expr(arg_text)
        if not args_in:
            pieces.append(code[last:end])
            last = end
            continue
        unnamed_count = sum(1 for a in args_in if "=" not in a)
        unnamed_idx = 0
        out_args: List[str] = []
        ok = True
        for a in args_in:
            t = a.strip()
            if "=" in t:
                out_args.append(t)
                continue
            if unnamed_idx >= len(iface.args):
                ok = False
                break
            dummy = iface.args[unnamed_idx]
            unnamed_idx += 1
            need_named = (
                _is_literal_expr_for_named_arg(t)
                or dummy in iface.optional_args
                or (unnamed_count > max_positional and unnamed_idx > max_positional)
            )
            if need_named:
                out_args.append(f"{dummy}={t}")
                changed = True
            else:
                out_args.append(t)
        if not ok:
            pieces.append(code[last:end])
            last = end
            continue
        new_call = f"{name_raw}(" + ", ".join(out_args) + ")"
        pieces.append(code[last:start])
        pieces.append(new_call)
        last = end

    pieces.append(code[last:])
    new_code = "".join(pieces)
    return new_code, (changed or new_code != code), unknown


def rewrite_named_arguments_in_lines(
    lines: List[str],
    *,
    max_positional: int = 3,
) -> Tuple[List[str], int, Set[str]]:
    """Rewrite named arguments across source lines using available local interfaces."""
    interfaces = collect_procedure_interfaces_from_lines(lines)

    def _split_code_comment_local(line: str) -> Tuple[str, str]:
        in_single = False
        in_double = False
        for i, ch in enumerate(line):
            if ch == "'" and not in_double:
                in_single = not in_single
                continue
            if ch == '"' and not in_single:
                in_double = not in_double
                continue
            if ch == "!" and not in_single and not in_double:
                return line[:i], line[i:]
        return line, ""

    def _is_nonexec_statement_local(code: str) -> bool:
        t = code.strip().lower()
        if not t:
            return True
        if t.startswith(("call ", "if ", "do ", "select ", "where ", "forall ", "block", "print ", "write(", "read(")):
            return False
        heads = (
            "program ",
            "module ",
            "use ",
            "implicit ",
            "integer",
            "real",
            "logical",
            "character",
            "type ",
            "contains",
            "interface",
            "end ",
            "subroutine ",
            "function ",
            "pure ",
            "elemental ",
            "recursive ",
        )
        return t.startswith(heads)

    out_lines: List[str] = []
    n_changes = 0
    unavailable: Set[str] = set()
    for raw in lines:
        eol = _line_eol(raw)
        line = raw.rstrip("\r\n")
        code, comment = _split_code_comment_local(line)
        if "&" in code or _is_nonexec_statement_local(code):
            out_lines.append(raw)
            continue
        new_code, changed, unknown = rewrite_named_arguments_in_statement(
            code, interfaces, max_positional=max_positional
        )
        if changed:
            n_changes += 1
        unavailable.update(unknown)
        out_lines.append(f"{new_code}{comment}{eol}")
    return out_lines, n_changes, unavailable
