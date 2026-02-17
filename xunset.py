#!/usr/bin/env python3
"""Advisory checker for possible use-before-set variables in Fortran."""

from __future__ import annotations

import argparse
import re
from dataclasses import dataclass
from itertools import product
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Set, Tuple

import cli_paths as cpaths
import fortran_scan as fscan

PROGRAM_START_RE = re.compile(r"^\s*program\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
END_RE = re.compile(r"^\s*end(?:\s+(program|function|subroutine|module))?\b", re.IGNORECASE)
PROC_START_RE = re.compile(
    r"^\s*(?:(?:pure|elemental|impure|recursive|module)\s+)*(function|subroutine)\s+([a-z][a-z0-9_]*)\b",
    re.IGNORECASE,
)
TYPE_DECL_RE = re.compile(
    r"^\s*(integer|real|logical|character|complex|type\b|class\b|procedure\b)",
    re.IGNORECASE,
)
ALLOCATABLE_ATTR_RE = re.compile(r"\ballocatable\b", re.IGNORECASE)
NO_COLON_DECL_RE = re.compile(
    r"^\s*(?P<spec>(?:integer|real|logical|complex|character)\s*(?:\([^)]*\))?"
    r"|type\s*\([^)]*\)|class\s*\([^)]*\))\s+(?P<rhs>.+)$",
    re.IGNORECASE,
)
ASSIGN_RE = re.compile(
    r"^\s*([a-z][a-z0-9_]*(?:\s*%\s*[a-z][a-z0-9_]*)?)\s*(?:\([^)]*\))?\s*=",
    re.IGNORECASE,
)
IDENT_RE = re.compile(r"\b([a-z][a-z0-9_]*)\b", re.IGNORECASE)
INT_RE = re.compile(r"^\s*([+-]?\d+)\s*$")
RANGE_RE = re.compile(r"^\s*([+-]?\d+)?\s*:\s*([+-]?\d+)?\s*$")
INTENT_RE = re.compile(r"\bintent\s*\(\s*(inout|out|in)\s*\)", re.IGNORECASE)
VALUE_RE = re.compile(r"\bvalue\b", re.IGNORECASE)
CONTROL_BARRIER_RE = re.compile(
    r"^\s*(if\b|else\b|elseif\b|end\s+if\b|do\b|end\s+do\b|select\b|case\b|where\b|forall\b|cycle\b|exit\b|goto\b)",
    re.IGNORECASE,
)
IF_THEN_RE = re.compile(r"^\s*if\s*\(.*\)\s*then\b", re.IGNORECASE)
ELSE_IF_RE = re.compile(r"^\s*else\s*if\s*\(.*\)\s*then\b", re.IGNORECASE)
ELSE_RE = re.compile(r"^\s*else\b", re.IGNORECASE)
END_IF_RE = re.compile(r"^\s*(end\s*if|endif)\b", re.IGNORECASE)
CALL_RE = re.compile(r"^\s*call\b", re.IGNORECASE)
DO_ITER_RE = re.compile(r"^\s*do\b(?:\s+\d+)?\s+([a-z][a-z0-9_]*)\s*=", re.IGNORECASE)
ALLOCATE_RE = re.compile(r"^\s*allocate\s*\((.*)\)\s*$", re.IGNORECASE)
DEALLOCATE_RE = re.compile(r"^\s*deallocate\s*\((.*)\)\s*$", re.IGNORECASE)
MAX_TRACKED_RANK = 3
MAX_TRACKED_ELEMENTS = 200000


def parse_call_stmt(stmt: str) -> Tuple[Optional[str], str]:
    """Return (callee_name, arg_text) for CALL statement."""
    m = re.match(r"^\s*call\s+([a-z][a-z0-9_]*)\s*(?:\((.*)\))?\s*$", stmt, re.IGNORECASE)
    if not m:
        return None, ""
    return m.group(1).lower(), (m.group(2) or "").strip()


def call_arg_chunks(arg_text: str) -> List[str]:
    """Split CALL argument text by top-level commas."""
    if not arg_text:
        return []
    return split_top_level_commas(arg_text)


@dataclass
class Unit:
    """Represent one analyzable program unit."""

    path: Path
    kind: str
    name: str
    start: int
    end: int
    body: List[Tuple[int, str]]
    dummy_names: Set[str]


@dataclass
class Issue:
    """Represent one use-before-set warning."""

    path: Path
    unit_kind: str
    unit_name: str
    line: int
    var_name: str
    detail: str


def choose_files(args_files: List[Path], exclude: Iterable[str]) -> List[Path]:
    """Resolve source files from args or current-directory defaults."""
    if args_files:
        files = cpaths.expand_path_args(args_files)
    else:
        files = sorted(
            set(Path(".").glob("*.f90")) | set(Path(".").glob("*.F90")),
            key=lambda p: p.name.lower(),
        )
    return fscan.apply_excludes(files, exclude)


def split_top_level_commas(text: str) -> List[str]:
    """Split declaration RHS on top-level commas only."""
    out: List[str] = []
    cur: List[str] = []
    depth = 0
    sq_depth = 0
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
            elif ch == "[":
                sq_depth += 1
            elif ch == "]" and sq_depth > 0:
                sq_depth -= 1
            elif ch == "," and depth == 0 and sq_depth == 0:
                out.append("".join(cur).strip())
                cur = []
                continue
        cur.append(ch)
    tail = "".join(cur).strip()
    if tail:
        out.append(tail)
    return out


def parse_declared_entities_with_init(stmt: str) -> Dict[str, bool]:
    """Parse declaration entities and whether each is initialized."""
    rhs = ""
    if "::" in stmt:
        rhs = stmt.split("::", 1)[1]
    else:
        m = NO_COLON_DECL_RE.match(stmt.strip())
        if not m:
            return {}
        rhs = m.group("rhs")
    out: Dict[str, bool] = {}
    for chunk in split_top_level_commas(rhs):
        text = chunk.strip()
        if not text:
            continue
        mm = re.match(r"^([a-z][a-z0-9_]*)", text, re.IGNORECASE)
        if not mm:
            continue
        name = mm.group(1).lower()
        initialized = ("=" in text and "=>" not in text)
        out[name] = initialized
    return out


def parse_declared_constant_shapes(stmt: str) -> Dict[str, Tuple[int, ...]]:
    """Parse rank-1/2/3 declared variables with integer-constant extents."""
    rhs = ""
    if "::" in stmt:
        rhs = stmt.split("::", 1)[1]
    else:
        m = NO_COLON_DECL_RE.match(stmt.strip())
        if not m:
            return {}
        rhs = m.group("rhs")
    out: Dict[str, Tuple[int, ...]] = {}
    for chunk in split_top_level_commas(rhs):
        text = chunk.strip()
        if not text:
            continue
        if "=" in text and "=>" not in text:
            text = text.split("=", 1)[0].strip()
        m = re.match(r"^([a-z][a-z0-9_]*)\s*\(([^)]*)\)\s*$", text, re.IGNORECASE)
        if not m:
            continue
        name = m.group(1).lower()
        dims = [d.strip() for d in split_top_level_commas(m.group(2).strip()) if d.strip()]
        if not dims or len(dims) > MAX_TRACKED_RANK:
            continue
        shape: List[int] = []
        ok = True
        for d in dims:
            mm = INT_RE.match(d)
            if mm:
                try:
                    n = int(mm.group(1))
                except ValueError:
                    ok = False
                    break
                if n <= 0:
                    ok = False
                    break
                shape.append(n)
                continue
            if ":" in d:
                a, b = [p.strip() for p in d.split(":", 1)]
                ma = INT_RE.match(a)
                mb = INT_RE.match(b)
                if not ma or not mb:
                    ok = False
                    break
                try:
                    lo = int(ma.group(1))
                    hi = int(mb.group(1))
                except ValueError:
                    ok = False
                    break
                if hi < lo:
                    ok = False
                    break
                shape.append(hi - lo + 1)
                continue
            ok = False
            break
        if ok and shape:
            out[name] = tuple(shape)
    return out


def parse_literal_dim_indices(token: str, dim_n: int) -> Optional[Set[int]]:
    """Parse one dimension selector into concrete indices for a known extent."""
    s = token.strip()
    if not s:
        return None
    m_int = INT_RE.match(s)
    if m_int:
        try:
            return {int(m_int.group(1))}
        except ValueError:
            return None
    m_rng = RANGE_RE.match(s)
    if m_rng:
        lo_t = m_rng.group(1)
        hi_t = m_rng.group(2)
        try:
            lo = int(lo_t) if lo_t is not None else 1
            hi = int(hi_t) if hi_t is not None else dim_n
        except ValueError:
            return None
        if hi < lo:
            return set()
        return set(range(lo, hi + 1))
    payload = s
    if payload.startswith("[") and payload.endswith("]"):
        payload = payload[1:-1].strip()
    elif payload.startswith("(/") and payload.endswith("/)"):
        payload = payload[2:-2].strip()
    else:
        return None
    if not payload:
        return None
    out: Set[int] = set()
    for part in split_top_level_commas(payload):
        t = part.strip()
        mm = INT_RE.match(t)
        if not mm:
            return None
        try:
            out.add(int(mm.group(1)))
        except ValueError:
            return None
    return out


def parse_literal_index_footprint(base_args: str, shape: Tuple[int, ...]) -> Optional[Set[Tuple[int, ...]]]:
    """Parse rank-1/2/3 literal indexing into concrete tuple footprint."""
    args = [a.strip() for a in split_top_level_commas(base_args) if a.strip()]
    if len(args) != len(shape):
        return None
    dims: List[List[int]] = []
    for tok, dim_n in zip(args, shape):
        idxs = parse_literal_dim_indices(tok, dim_n)
        if idxs is None:
            return None
        if not idxs:
            return set()
        # in-bounds literal indices only
        idxs2 = [i for i in sorted(idxs) if 1 <= i <= dim_n]
        if not idxs2:
            return set()
        dims.append(idxs2)
    combos = 1
    for d in dims:
        combos *= len(d)
        if combos > MAX_TRACKED_ELEMENTS:
            return None
    out: Set[Tuple[int, ...]] = set()
    for tup in product(*dims):
        out.add(tuple(int(v) for v in tup))
    return out


def extract_index_args(expr: str, base: str) -> Optional[List[str]]:
    """Extract argument text for each base(...) occurrence in expr; None on parse failure."""
    out: List[str] = []
    s = expr
    pat = re.compile(rf"\b{re.escape(base)}\s*\(", re.IGNORECASE)
    pos = 0
    while True:
        m = pat.search(s, pos)
        if not m:
            break
        i = m.end() - 1  # points at '('
        depth = 1
        j = i + 1
        while j < len(s):
            ch = s[j]
            if ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
                if depth == 0:
                    out.append(s[i + 1 : j])
                    pos = j + 1
                    break
            j += 1
        else:
            return None
    return out


def read_required_indices(expr: str, base: str, shape: Tuple[int, ...]) -> Optional[Set[Tuple[int, ...]]]:
    """Return concrete read footprint for base in expr, or None when unknown/whole-array."""
    bare_re = re.compile(rf"\b{re.escape(base)}\b(?!\s*\()", re.IGNORECASE)
    if bare_re.search(expr):
        return None
    args_list = extract_index_args(expr, base)
    if args_list is None:
        return None
    if not args_list:
        return set()
    req: Set[Tuple[int, ...]] = set()
    for args in args_list:
        idxs = parse_literal_index_footprint(args, shape)
        if idxs is None:
            return None
        req.update(idxs)
    return req


def missing_detail_for_var(
    name: str,
    shape: Tuple[int, ...],
    known: Set[Tuple[int, ...]],
    read_expr: str,
) -> str:
    """Build specific unset-element detail for one variable read."""
    def fmt_idx(idx: Tuple[int, ...]) -> str:
        return f"{name}(" + ", ".join(str(v) for v in idx) + ")"

    req = read_required_indices(read_expr, name, shape)
    if req is not None:
        miss = [k for k in sorted(req) if k not in known]
    else:
        miss = []
        for idx in product(*[range(1, n + 1) for n in shape]):
            t = tuple(int(v) for v in idx)
            if t not in known:
                miss.append(t)
                if len(miss) > 6:
                    break
    if not miss:
        return ""
    if len(miss) == 1:
        return f"array element {fmt_idx(miss[0])} may be used before being set"
    show = ", ".join(fmt_idx(k) for k in miss[:6])
    if len(miss) > 6:
        show += ", ..."
    return f"array elements may be used before being set: {show}"


def shape_element_count(shape: Tuple[int, ...]) -> int:
    """Return total number of elements in an array shape."""
    n = 1
    for d in shape:
        n *= d
    return n


def full_shape_footprint(shape: Tuple[int, ...]) -> Optional[Set[Tuple[int, ...]]]:
    """Return full tuple footprint when shape is small enough, else None."""
    total = shape_element_count(shape)
    if total > MAX_TRACKED_ELEMENTS:
        return None
    out: Set[Tuple[int, ...]] = set()
    for idx in product(*[range(1, d + 1) for d in shape]):
        out.add(tuple(int(v) for v in idx))
    return out


def parse_program_units(finfo: fscan.SourceFileInfo) -> List[Unit]:
    """Parse main program blocks from one source file."""
    units: List[Unit] = []
    in_program = False
    prog_name = ""
    prog_start = -1
    prog_body: List[Tuple[int, str]] = []
    proc_depth = 0
    interface_depth = 0
    for lineno, stmt in fscan.iter_fortran_statements(finfo.parsed_lines):
        low = stmt.strip().lower()
        if not low:
            continue
        if re.match(r"^\s*(abstract\s+)?interface\b", low):
            interface_depth += 1
            continue
        if re.match(r"^\s*end\s+interface\b", low):
            interface_depth = max(0, interface_depth - 1)
            continue
        if interface_depth > 0:
            continue

        m_prog = PROGRAM_START_RE.match(low)
        if m_prog and not in_program:
            in_program = True
            prog_name = m_prog.group(1).lower()
            prog_start = lineno
            prog_body = []
            proc_depth = 0
            continue
        if not in_program:
            continue

        m_proc = PROC_START_RE.match(low)
        if m_proc:
            proc_depth += 1
        elif low.startswith("end"):
            toks = low.split()
            if len(toks) == 1 or (len(toks) >= 2 and toks[1] in {"function", "subroutine"}):
                if proc_depth > 0:
                    proc_depth -= 1
            if (len(toks) >= 2 and toks[1] == "program") or len(toks) == 1:
                if proc_depth == 0:
                    units.append(
                        Unit(
                            path=finfo.path,
                            kind="program",
                            name=prog_name,
                            start=prog_start,
                            end=lineno,
                            body=prog_body,
                            dummy_names=set(),
                        )
                    )
                    in_program = False
                    continue

        if proc_depth == 0:
            prog_body.append((lineno, stmt))
    return units


def parse_implicit_main_unit(finfo: fscan.SourceFileInfo) -> Optional[Unit]:
    """Parse top-level statements as an implicit main program when no PROGRAM exists."""
    explicit_programs = parse_program_units(finfo)
    if explicit_programs:
        return None

    body: List[Tuple[int, str]] = []
    interface_depth = 0
    module_depth = 0
    proc_depth = 0
    start_line = -1
    end_line = -1

    for lineno, stmt in fscan.iter_fortran_statements(finfo.parsed_lines):
        low = stmt.strip().lower()
        if not low:
            continue
        if re.match(r"^\s*(abstract\s+)?interface\b", low):
            interface_depth += 1
            continue
        if re.match(r"^\s*end\s+interface\b", low):
            interface_depth = max(0, interface_depth - 1)
            continue
        if interface_depth > 0:
            continue

        if PROGRAM_START_RE.match(low):
            return None
        if re.match(r"^\s*module\b", low):
            toks = low.split()
            if len(toks) >= 2 and toks[1] != "procedure":
                module_depth += 1
            continue
        if low.startswith("end"):
            toks = low.split()
            if len(toks) >= 2 and toks[1] == "module" and module_depth > 0:
                module_depth -= 1
                continue
            if len(toks) >= 2 and toks[1] in {"function", "subroutine"} and proc_depth > 0:
                proc_depth -= 1
                continue
            if len(toks) >= 2 and toks[1] == "program":
                # Explicit program close should not happen without explicit start, but be safe.
                continue
            if len(toks) == 1 and proc_depth > 0:
                proc_depth -= 1
                continue
            if len(toks) == 1 and module_depth > 0:
                module_depth -= 1
                continue

        if module_depth > 0:
            # Ignore module scope and module contained procedures.
            if PROC_START_RE.match(low):
                proc_depth += 1
            continue

        # External procedure definitions are not part of implicit main body.
        if PROC_START_RE.match(low):
            proc_depth += 1
            continue
        if proc_depth > 0:
            continue

        if start_line < 0:
            start_line = lineno
        end_line = lineno
        body.append((lineno, stmt))

    if not body:
        return None
    return Unit(
        path=finfo.path,
        kind="program",
        name="main",
        start=start_line,
        end=end_line,
        body=body,
        dummy_names=set(),
    )


def collect_units(finfo: fscan.SourceFileInfo) -> List[Unit]:
    """Collect procedures and program units for analysis."""
    out: List[Unit] = []
    for p in finfo.procedures:
        out.append(
            Unit(
                path=finfo.path,
                kind=p.kind.lower(),
                name=p.name.lower(),
                start=p.start,
                end=p.end,
                body=p.body,
                dummy_names=set(p.dummy_names),
            )
        )
    out.extend(parse_program_units(finfo))
    implicit_main = parse_implicit_main_unit(finfo)
    if implicit_main is not None:
        out.append(implicit_main)
    return out


def extract_ident_reads(expr: str, tracked: Set[str]) -> List[str]:
    """Extract tracked identifiers referenced in an expression."""
    expr = strip_shape_inquiry_calls(expr)
    implied_idx_vars = implied_do_index_vars(expr)
    out: List[str] = []
    seen: Set[str] = set()
    for m in IDENT_RE.finditer(expr.lower()):
        n = m.group(1).lower()
        if n in implied_idx_vars:
            continue
        if n in tracked and n not in seen:
            seen.add(n)
            out.append(n)
    return out


def implied_do_index_vars(expr: str) -> Set[str]:
    """Collect implied-DO index variable names used in array constructors."""
    out: Set[str] = set()

    def scan_payload(payload: str) -> None:
        # In implied-DO controls, the index appears after a comma: "(..., i=...)"
        for m in re.finditer(r",\s*([a-z][a-z0-9_]*)\s*=", payload, re.IGNORECASE):
            out.add(m.group(1).lower())

    s = expr
    # Bracket constructors: [ ... ]
    i = 0
    while i < len(s):
        if s[i] == "[":
            depth = 1
            j = i + 1
            while j < len(s):
                if s[j] == "[":
                    depth += 1
                elif s[j] == "]":
                    depth -= 1
                    if depth == 0:
                        scan_payload(s[i + 1 : j])
                        i = j + 1
                        break
                j += 1
            else:
                break
            continue
        i += 1

    # F77-style constructors: (/ ... /)
    for m in re.finditer(r"\(/\s*(.*?)\s*/\)", s, re.IGNORECASE):
        scan_payload(m.group(1))

    return out


def extract_if_condition(stmt: str) -> str:
    """Extract condition text from an IF ... THEN statement."""
    s = stmt.strip()
    m = re.match(r"^\s*if\s*\(", s, re.IGNORECASE)
    if not m:
        return ""
    i = m.end() - 1
    depth = 0
    in_single = False
    in_double = False
    for j in range(i, len(s)):
        ch = s[j]
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
                    return s[i + 1 : j].strip()
    return ""


def split_one_line_if(stmt: str) -> Optional[Tuple[str, str]]:
    """Split one-line IF statement into (condition, tail statement)."""
    s = stmt.strip()
    m = re.match(r"^\s*if\s*\(", s, re.IGNORECASE)
    if not m:
        return None
    i = m.end() - 1
    depth = 0
    in_single = False
    in_double = False
    for j in range(i, len(s)):
        ch = s[j]
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
                    cond = s[i + 1 : j].strip()
                    tail = s[j + 1 :].strip()
                    if not tail or tail.lower().startswith("then"):
                        return None
                    return cond, tail
    return None


def extract_allocatable_reads(expr: str, allocatable_vars: Set[str]) -> List[str]:
    """Extract allocatable variables whose value/shape is used in expression context."""
    out: List[str] = []
    seen: Set[str] = set()
    n = len(expr)
    i = 0
    skip_calls = {"allocated", "kind", "rank"}
    inquiry_calls = {"size", "shape", "lbound", "ubound"}
    while i < n:
        ch = expr[i]
        if ch.isalpha() or ch == "_":
            j = i + 1
            while j < n and (expr[j].isalnum() or expr[j] == "_"):
                j += 1
            name = expr[i:j].lower()
            k = j
            while k < n and expr[k].isspace():
                k += 1
            if k < n and expr[k] == "(":
                depth = 1
                m = k + 1
                while m < n:
                    if expr[m] == "(":
                        depth += 1
                    elif expr[m] == ")":
                        depth -= 1
                        if depth == 0:
                            break
                    m += 1
                if m >= n:
                    # malformed; fall back to identifier treatment
                    if name in allocatable_vars and name not in seen:
                        seen.add(name)
                        out.append(name)
                    i = j
                    continue
                inner = expr[k + 1 : m]
                if name in skip_calls:
                    i = m + 1
                    continue
                if name in inquiry_calls:
                    for n2 in extract_ident_reads(inner, allocatable_vars):
                        if n2 not in seen:
                            seen.add(n2)
                            out.append(n2)
                    i = m + 1
                    continue
                # Generic call/reference: include possible variable read + recurse.
                if name in allocatable_vars and name not in seen:
                    seen.add(name)
                    out.append(name)
                for n2 in extract_allocatable_reads(inner, allocatable_vars):
                    if n2 not in seen:
                        seen.add(n2)
                        out.append(n2)
                i = m + 1
                continue
            if name in allocatable_vars and name not in seen:
                seen.add(name)
                out.append(name)
            i = j
            continue
        i += 1
    return out


def call_actual_exprs(arg_text: str) -> List[str]:
    """Return actual argument expressions from CALL arg text."""
    out: List[str] = []
    for chunk in split_top_level_commas(arg_text):
        t = chunk.strip()
        if not t:
            continue
        if "=" in t and "=>" not in t:
            _, val = t.split("=", 1)
            out.append(val.strip())
        else:
            out.append(t)
    return out


def strip_shape_inquiry_calls(expr: str) -> str:
    """Strip arguments of inquiry intrinsics so identifiers there are not treated as value reads."""
    names = {"size", "shape", "lbound", "ubound", "kind", "rank", "allocated"}
    out: List[str] = []
    i = 0
    n = len(expr)
    while i < n:
        ch = expr[i]
        if ch.isalpha() or ch == "_":
            j = i + 1
            while j < n and (expr[j].isalnum() or expr[j] == "_"):
                j += 1
            name = expr[i:j].lower()
            k = j
            while k < n and expr[k].isspace():
                k += 1
            if name in names and k < n and expr[k] == "(":
                depth = 1
                m = k + 1
                while m < n:
                    if expr[m] == "(":
                        depth += 1
                    elif expr[m] == ")":
                        depth -= 1
                        if depth == 0:
                            out.append(" ")
                            i = m + 1
                            break
                    m += 1
                else:
                    out.append(expr[i:j])
                    i = j
                continue
            out.append(expr[i:j])
            i = j
            continue
        out.append(ch)
        i += 1
    return "".join(out)


def resolve_alloc_extent(expr: str, array_shapes: Dict[str, Tuple[int, ...]]) -> Optional[int]:
    """Resolve one ALLOCATE extent when statically known."""
    s = expr.strip()
    m_int = INT_RE.match(s)
    if m_int:
        try:
            v = int(m_int.group(1))
        except ValueError:
            return None
        return v if v > 0 else None
    m_sz = re.match(
        r"^size\s*\(\s*([a-z][a-z0-9_]*)\s*(?:,\s*([1-9]\d*)\s*)?\)\s*$",
        s,
        re.IGNORECASE,
    )
    if m_sz:
        base = m_sz.group(1).lower()
        dim_txt = m_sz.group(2)
        shp = array_shapes.get(base)
        if not shp:
            return None
        if dim_txt is None:
            return shape_element_count(shp)
        try:
            dim = int(dim_txt)
        except ValueError:
            return None
        if dim < 1 or dim > len(shp):
            return None
        return shp[dim - 1]
    return None


def resolve_shape_from_expr(expr: str, array_shapes: Dict[str, Tuple[int, ...]]) -> Optional[Tuple[int, ...]]:
    """Resolve whole-array shape when expression is a known array variable."""
    s = expr.strip()
    m = re.match(r"^([a-z][a-z0-9_]*)$", s, re.IGNORECASE)
    if not m:
        return None
    return array_shapes.get(m.group(1).lower())


def analyze_unit(unit: Unit) -> List[Issue]:
    """Run conservative v1 use-before-set analysis on one unit."""
    issues: List[Issue] = []
    tracked: Set[str] = set()
    defined: Set[str] = set()
    intent_out: Set[str] = set()
    local_declared: Set[str] = set()
    dummy_intent: Dict[str, str] = {}
    allocatable_vars: Set[str] = set()
    allocated_vars: Set[str] = set()
    array_shapes: Dict[str, Tuple[int, ...]] = {}
    known_set_elems: Dict[str, Set[Tuple[int, ...]]] = {}
    partial_unknown: Set[str] = set()
    unalloc_warned: Set[Tuple[int, str]] = set()
    if_stack: List[Dict[str, object]] = []
    conditional_unset_reason: Dict[str, str] = {}

    def warn_unallocated(var_name: str, line_no: int) -> None:
        key = (line_no, var_name)
        if key in unalloc_warned:
            return
        unalloc_warned.add(key)
        issues.append(
            Issue(
                path=unit.path,
                unit_kind=unit.kind,
                unit_name=unit.name,
                line=line_no,
                var_name=var_name,
                detail="allocatable variable may be used while unallocated",
            )
        )

    def warn_unset_read(var_name: str, line_no: int, stmt_text: str) -> None:
        if var_name in allocatable_vars and var_name not in allocated_vars:
            return
        if var_name in defined:
            return
        if var_name in known_set_elems and var_name in array_shapes and var_name not in partial_unknown:
            detail = missing_detail_for_var(var_name, array_shapes[var_name], known_set_elems[var_name], stmt_text)
            if not detail:
                return
        else:
            detail = "variable may be used before being set"
            cond = conditional_unset_reason.get(var_name)
            if cond:
                detail = f"variable may be used before being set if condition ({cond}) not met"
        kind = "INTENT(OUT) dummy" if var_name in intent_out else "variable"
        issues.append(
            Issue(
                path=unit.path,
                unit_kind=unit.kind,
                unit_name=unit.name,
                line=line_no,
                var_name=var_name,
                detail=detail if kind == "variable" else f"{kind} may be used before being set",
            )
        )

    # Dummies default: treat as defined except explicit INTENT(OUT).
    for d in unit.dummy_names:
        tracked.add(d)
        defined.add(d)

    for ln, stmt in unit.body:
        low = stmt.lower().strip()
        if not low:
            continue

        one_if = split_one_line_if(low)
        if one_if is not None:
            cond, tail = one_if
            # Condition reads.
            for n in extract_allocatable_reads(cond, allocatable_vars):
                if n not in allocated_vars:
                    warn_unallocated(n, ln)
            for n in extract_ident_reads(cond, tracked):
                warn_unset_read(n, ln, cond)

            # Handle assignment tails with conditional-set reasoning.
            m_tail_as = ASSIGN_RE.match(tail)
            if m_tail_as:
                lhs_text = tail.split("=", 1)[0].strip()
                lhs_base = fscan.base_identifier(lhs_text) or ""
                rhs = tail.split("=", 1)[1] if "=" in tail else ""
                lhs_has_index = "(" in lhs_text and ")" in lhs_text

                for n in extract_allocatable_reads(rhs, allocatable_vars):
                    if n not in allocated_vars:
                        warn_unallocated(n, ln)
                for n in extract_allocatable_reads(lhs_text, allocatable_vars):
                    if not lhs_has_index and n == lhs_base:
                        continue
                    if n not in allocated_vars:
                        warn_unallocated(n, ln)

                lhs_idx_reads = extract_ident_reads(lhs_text, tracked)
                if lhs_base and lhs_base in lhs_idx_reads:
                    lhs_idx_reads = [x for x in lhs_idx_reads if x != lhs_base]
                reads = lhs_idx_reads + [x for x in extract_ident_reads(rhs, tracked) if x not in lhs_idx_reads]
                for n in reads:
                    warn_unset_read(n, ln, tail)

                if lhs_base and lhs_base in tracked and not lhs_has_index and lhs_base not in defined:
                    conditional_unset_reason[lhs_base] = cond
            else:
                # Non-assignment one-line IF tail: still process reads conservatively.
                for n in extract_allocatable_reads(tail, allocatable_vars):
                    if n not in allocated_vars:
                        warn_unallocated(n, ln)
                for n in extract_ident_reads(tail, tracked):
                    warn_unset_read(n, ln, tail)
            continue

        if IF_THEN_RE.match(low):
            for n in extract_allocatable_reads(low, allocatable_vars):
                if n not in allocated_vars:
                    warn_unallocated(n, ln)
            for n in extract_ident_reads(low, tracked):
                warn_unset_read(n, ln, low)
            if_stack.append(
                {
                    "pre": set(defined),
                    "branches": [],  # type: List[Set[str]]
                    "has_else": False,
                    "has_elseif": False,
                    "cond": extract_if_condition(low),
                }
            )
            continue

        if ELSE_IF_RE.match(low):
            if if_stack:
                frame = if_stack[-1]
                frame["branches"].append(set(defined))  # type: ignore[index]
                frame["has_elseif"] = True
                defined.clear()
                defined.update(frame["pre"])  # type: ignore[arg-type]
            for n in extract_allocatable_reads(low, allocatable_vars):
                if n not in allocated_vars:
                    warn_unallocated(n, ln)
            for n in extract_ident_reads(low, tracked):
                warn_unset_read(n, ln, low)
            continue

        if ELSE_RE.match(low) and not ELSE_IF_RE.match(low):
            if if_stack:
                frame = if_stack[-1]
                frame["branches"].append(set(defined))  # type: ignore[index]
                frame["has_else"] = True
                defined.clear()
                defined.update(frame["pre"])  # type: ignore[arg-type]
            continue

        if END_IF_RE.match(low):
            if if_stack:
                frame = if_stack.pop()
                branches: List[Set[str]] = frame["branches"]  # type: ignore[assignment]
                branches.append(set(defined))
                pre_set: Set[str] = set(frame["pre"])  # type: ignore[arg-type]
                # Simple IF(no else/elseif): remember vars defined only in THEN branch
                # to enrich future unset-read diagnostics.
                if not frame["has_else"] and not frame["has_elseif"] and branches:
                    then_only = branches[0] - pre_set
                    cond = str(frame.get("cond") or "").strip()
                    if cond:
                        for v in then_only:
                            conditional_unset_reason[v] = cond
                if not frame["has_else"]:  # type: ignore[index]
                    branches.append(set(frame["pre"]))  # type: ignore[arg-type]
                merged = set(branches[0]) if branches else set(frame["pre"])  # type: ignore[arg-type]
                for b in branches[1:]:
                    merged.intersection_update(b)
                defined.clear()
                defined.update(merged)
            continue

        if TYPE_DECL_RE.match(low):
            decls = parse_declared_entities_with_init(low)
            array_shapes.update(parse_declared_constant_shapes(low))
            if decls:
                tracked.update(decls.keys())
                local_declared.update(decls.keys())
                if ALLOCATABLE_ATTR_RE.search(low):
                    allocatable_vars.update(decls.keys())
                for n, init in decls.items():
                    if init:
                        defined.add(n)
                        conditional_unset_reason.pop(n, None)
                        if n in array_shapes:
                            fp = full_shape_footprint(array_shapes[n])
                            if fp is not None:
                                known_set_elems[n] = fp
            m_int = INTENT_RE.search(low)
            if m_int:
                intent = m_int.group(1).lower()
                for d in unit.dummy_names:
                    if d in decls:
                        dummy_intent[d] = intent
                        if intent == "out":
                            if d in defined:
                                defined.remove(d)
                            intent_out.add(d)
                        elif intent in {"in", "inout"}:
                            defined.add(d)
            elif VALUE_RE.search(low):
                for d in unit.dummy_names:
                    if d in decls:
                        defined.add(d)
            continue

        if low.startswith("implicit ") or low.startswith("use ") or low.startswith("contains"):
            continue
        m_do = DO_ITER_RE.match(low)
        if m_do:
            iv = m_do.group(1).lower()
            if iv in tracked:
                defined.add(iv)
            continue
        m_alloc = ALLOCATE_RE.match(low)
        if m_alloc:
            chunks = split_top_level_commas(m_alloc.group(1))
            has_source = False
            mold_shape: Optional[Tuple[int, ...]] = None
            for chunk in chunks:
                t = chunk.strip()
                if not t:
                    continue
                if "=" in t and "=>" not in t:
                    key, val = t.split("=", 1)
                    key_l = key.strip().lower()
                    if key_l == "source":
                        has_source = True
                    elif key_l == "mold":
                        mold_shape = resolve_shape_from_expr(val, array_shapes)
                    continue
            for chunk in chunks:
                t = chunk.strip()
                if not t:
                    continue
                # Option clauses, e.g. source=...
                if "=" in t and "=>" not in t:
                    key, val = t.split("=", 1)
                    key = key.strip().lower()
                    if key in {"source", "mold"}:
                        if key == "source":
                            has_source = True
                            for n in extract_ident_reads(val, tracked):
                                if n not in defined:
                                    issues.append(
                                        Issue(
                                            path=unit.path,
                                            unit_kind=unit.kind,
                                            unit_name=unit.name,
                                            line=ln,
                                            var_name=n,
                                            detail="variable may be used before being set",
                                        )
                                    )
                        for n in extract_allocatable_reads(val, allocatable_vars):
                            if n not in allocated_vars:
                                warn_unallocated(n, ln)
                    continue
                base = fscan.base_identifier(t)
                # Infer ALLOCATE target shape when extents are simple constants/size(...).
                if base and base in tracked and "(" in t and ")" in t:
                    idxs = extract_index_args(t, base)
                    if idxs and len(idxs) == 1:
                        dims = [d.strip() for d in split_top_level_commas(idxs[0]) if d.strip()]
                        if 1 <= len(dims) <= MAX_TRACKED_RANK:
                            resolved: List[int] = []
                            ok = True
                            for d in dims:
                                v = resolve_alloc_extent(d, array_shapes)
                                if v is None or v <= 0:
                                    ok = False
                                    break
                                resolved.append(v)
                            if ok:
                                array_shapes[base] = tuple(resolved)
                elif base and base in tracked and mold_shape is not None:
                    # allocate(x, mold=y) => x gets y's shape.
                    array_shapes[base] = mold_shape
                # allocate(x(...)) without source does not initialize element values.
                if has_source and base and base in tracked:
                    defined.add(base)
                    conditional_unset_reason.pop(base, None)
                    if base in array_shapes:
                        fp = full_shape_footprint(array_shapes[base])
                        if fp is not None:
                            known_set_elems[base] = fp
                if base and base in allocatable_vars:
                    allocated_vars.add(base)
                # Bounds in allocate(x(n)) may read vars (except base itself).
                t_bounds = strip_shape_inquiry_calls(t)
                for n in extract_ident_reads(t_bounds, tracked):
                    if n == (base or ""):
                        continue
                    if n not in defined:
                        issues.append(
                            Issue(
                                path=unit.path,
                                unit_kind=unit.kind,
                                unit_name=unit.name,
                                line=ln,
                                var_name=n,
                                detail="variable may be used before being set",
                            )
                        )
                for n in extract_allocatable_reads(t, allocatable_vars):
                    if n == (base or ""):
                        continue
                    if n not in allocated_vars:
                        warn_unallocated(n, ln)
            continue
        m_dealloc = DEALLOCATE_RE.match(low)
        if m_dealloc:
            for chunk in split_top_level_commas(m_dealloc.group(1)):
                base = fscan.base_identifier(chunk.strip()) or ""
                if base in allocated_vars:
                    allocated_vars.remove(base)
            continue
        if CALL_RE.match(low):
            callee, arg_text = parse_call_stmt(low)
            for expr in call_actual_exprs(arg_text):
                for n in extract_allocatable_reads(expr, allocatable_vars):
                    if n not in allocated_vars:
                        warn_unallocated(n, ln)
            if callee == "random_seed":
                for chunk in call_arg_chunks(arg_text):
                    t = chunk.strip()
                    if not t:
                        continue
                    if "=" in t and "=>" not in t:
                        key, val = t.split("=", 1)
                        key = key.strip().lower()
                        val = val.strip()
                        base = fscan.base_identifier(val) or ""
                        if key in {"size", "get"}:
                            if base and base in tracked:
                                defined.add(base)
                        elif key == "put":
                            for n in extract_ident_reads(val, tracked):
                                if n not in defined:
                                    issues.append(
                                        Issue(
                                            path=unit.path,
                                            unit_kind=unit.kind,
                                            unit_name=unit.name,
                                            line=ln,
                                            var_name=n,
                                            detail="variable may be used before being set",
                                        )
                                    )
                    else:
                        # Positional random_seed args are uncommon; treat conservatively as reads.
                        for n in extract_ident_reads(t, tracked):
                            if n not in defined:
                                issues.append(
                                    Issue(
                                        path=unit.path,
                                        unit_kind=unit.kind,
                                        unit_name=unit.name,
                                        line=ln,
                                        var_name=n,
                                        detail="variable may be used before being set",
                                    )
                                )
                continue
            if callee == "random_number":
                # random_number harvest) sets its argument.
                chunks = call_arg_chunks(arg_text)
                if chunks:
                    base = fscan.base_identifier(chunks[0].strip()) or ""
                    if base and base in tracked:
                        defined.add(base)
                continue
            # Conservative default: skip unknown CALL semantics.
            continue
        if CONTROL_BARRIER_RE.match(low):
            # Conservative v1: skip control-flow-heavy statements.
            continue

        m_assign = ASSIGN_RE.match(low)
        if m_assign:
            lhs_text = low.split("=", 1)[0].strip()
            lhs_base = fscan.base_identifier(lhs_text) or ""
            rhs = low.split("=", 1)[1] if "=" in low else ""
            lhs_has_index = "(" in lhs_text and ")" in lhs_text
            for n in extract_allocatable_reads(rhs, allocatable_vars):
                if n not in allocated_vars:
                    warn_unallocated(n, ln)
            for n in extract_allocatable_reads(lhs_text, allocatable_vars):
                if not lhs_has_index and n == lhs_base:
                    continue
                if n not in allocated_vars:
                    warn_unallocated(n, ln)
            lhs_idx_reads = extract_ident_reads(lhs_text, tracked)
            if lhs_base and lhs_base in lhs_idx_reads:
                lhs_idx_reads = [x for x in lhs_idx_reads if x != lhs_base]
            reads = lhs_idx_reads + [x for x in extract_ident_reads(rhs, tracked) if x not in lhs_idx_reads]
            for n in reads:
                warn_unset_read(n, ln, low)
            # Indexed assignment (e.g. x(i)=...) does not prove whole variable initialized.
            if lhs_base and lhs_base in tracked and not lhs_has_index:
                defined.add(lhs_base)
                conditional_unset_reason.pop(lhs_base, None)
                if lhs_base in allocatable_vars:
                    allocated_vars.add(lhs_base)
                if lhs_base in array_shapes:
                    fp = full_shape_footprint(array_shapes[lhs_base])
                    if fp is not None:
                        known_set_elems[lhs_base] = fp
                    partial_unknown.discard(lhs_base)
            elif lhs_base and lhs_base in tracked and lhs_has_index and lhs_base in array_shapes:
                arg_chunks = extract_index_args(lhs_text, lhs_base)
                if not arg_chunks or len(arg_chunks) != 1:
                    partial_unknown.add(lhs_base)
                else:
                    fp = parse_literal_index_footprint(arg_chunks[0], array_shapes[lhs_base])
                    if fp is None:
                        partial_unknown.add(lhs_base)
                        continue
                    s = known_set_elems.setdefault(lhs_base, set())
                    s.update(fp)
                    if len(s) >= shape_element_count(array_shapes[lhs_base]):
                        defined.add(lhs_base)
                        partial_unknown.discard(lhs_base)
            continue

        for n in extract_allocatable_reads(low, allocatable_vars):
            if n not in allocated_vars:
                warn_unallocated(n, ln)
        reads = extract_ident_reads(low, tracked)
        for n in reads:
            warn_unset_read(n, ln, low)
    return issues


def analyze_infos(infos: List[fscan.SourceFile]) -> List[Issue]:
    """Analyze loaded sources and return all use-before-set issues."""
    ordered_infos, _ = fscan.order_files_least_dependent(infos)
    all_issues: List[Issue] = []
    for finfo in ordered_infos:
        for unit in collect_units(finfo):
            all_issues.extend(analyze_unit(unit))
    return all_issues


def analyze_paths(paths: List[Path]) -> List[Issue]:
    """Load and analyze source files by path."""
    infos, _any_missing = fscan.load_source_files(paths)
    if not infos:
        return []
    return analyze_infos(infos)


def main() -> int:
    """Run advisory use-before-set checks across selected Fortran files."""
    parser = argparse.ArgumentParser(
        description="Advisory checker for possible use-before-set variables (no --fix mode)"
    )
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--verbose", action="store_true", help="Print all findings (default prints summary + first)")
    args = parser.parse_args()

    files = choose_files(args.fortran_files, args.exclude)
    if not files:
        print("No source files remain after applying --exclude filters.")
        return 2

    infos, any_missing = fscan.load_source_files(files)
    if not infos:
        return 2 if any_missing else 1
    all_issues = analyze_infos(infos)

    if not all_issues:
        print("No possible use-before-set findings.")
        return 0

    all_issues.sort(key=lambda x: (x.path.name.lower(), x.line, x.unit_kind, x.unit_name, x.var_name))
    by_file: Dict[str, int] = {}
    for i in all_issues:
        by_file[i.path.name] = by_file.get(i.path.name, 0) + 1

    print(f"{len(all_issues)} possible use-before-set finding(s) in {len(by_file)} file(s).")
    if args.verbose:
        for i in all_issues:
            print(f"{i.path.name}:{i.line} {i.unit_kind} {i.unit_name} {i.var_name} - {i.detail}")
    else:
        for fname in sorted(by_file.keys(), key=str.lower):
            print(f"{fname}: {by_file[fname]}")
        first = all_issues[0]
        print(
            f"\nFirst finding: {first.path.name}:{first.line} "
            f"{first.unit_kind} {first.unit_name} {first.var_name} - {first.detail}"
        )
        print("Run with --verbose to list all findings.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
