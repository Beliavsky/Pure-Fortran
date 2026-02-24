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
    nested_mul_pat = re.compile(
        r"\(\s*\(\s*([^()]+?)\s*\)\s*([*/])\s*([^()]+?)\s*\)",
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
        m = do_re.match(code.strip())
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
    return out


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


def indent_fortran_blocks(text: str, *, indent_step: int = 3) -> str:
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
        r"end\s+(?:do|if|select|block|associate|where)"
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

    for raw in lines:
        stripped = raw.strip()
        if not stripped:
            out.append("")
            continue

        code = strip_comment(raw).strip()
        if code and dedent_before.match(code):
            level = max(0, level - 1)

        out.append(f"{step * level}{stripped}")

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
