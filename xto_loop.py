#!/usr/bin/env python3
"""Convert selected Fortran array operations back to explicit loops."""

from __future__ import annotations

import argparse
import difflib
import math
import re
import subprocess
import tempfile
import time
from datetime import datetime
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Sequence, Tuple

import cli_paths as cpaths
import fortran_build as fbuild
import fortran_scan as fscan

USE_XTO_INDICES = False

TYPE_DECL_RE = re.compile(r"^\s*(integer|real|logical|character|complex|type\b|class\b)", re.IGNORECASE)
ASSIGN_RE = re.compile(r"^\s*(.+?)\s*=\s*(.+)$", re.IGNORECASE)
LHS_ASSIGNABLE_RE = re.compile(r"^\s*[a-z][a-z0-9_]*(?:\s*\([^=]*\))?\s*$", re.IGNORECASE)
SIMPLE_NAME_RE = re.compile(r"^\s*([a-z][a-z0-9_]*)\s*$", re.IGNORECASE)
NAME_WITH_ARGS_RE = re.compile(r"^\s*([a-z][a-z0-9_]*)\s*\((.*)\)\s*$", re.IGNORECASE)
MATMUL_RE = re.compile(r"^\s*matmul\s*\(\s*(.+)\s*,\s*(.+)\)\s*$", re.IGNORECASE)
DOT_PRODUCT_RE = re.compile(r"^\s*dot_product\s*\(\s*(.+)\s*\)\s*$", re.IGNORECASE)
TRANSPOSE_RE = re.compile(r"^\s*transpose\s*\(\s*([a-z][a-z0-9_]*)\s*\)\s*$", re.IGNORECASE)
MINMAX_RE = re.compile(r"^\s*(minval|maxval)\s*\(\s*(.+)\s*\)\s*$", re.IGNORECASE)
PACK_RE = re.compile(r"^\s*pack\s*\(\s*(.+)\s*\)\s*$", re.IGNORECASE)
SPREAD_RE = re.compile(
    r"^\s*spread\s*\(\s*(.+?)\s*,\s*dim\s*=\s*([123])\s*,\s*ncopies\s*=\s*(.+)\)\s*$",
    re.IGNORECASE,
)
SUMPROD_MASK_RE = re.compile(
    r"^\s*(sum|product)\s*\(\s*(.+)\s*,\s*mask\s*=\s*(.+)\)\s*$",
    re.IGNORECASE,
)
IMPLIED_DO_TRAIL_RE = re.compile(
    r"\(\s*(.+)\s*,\s*([a-z][a-z0-9_]*)\s*=\s*([^,()]+)\s*,\s*([^,()]+)(?:\s*,\s*([^,()]+))?\s*\)\s*$",
    re.IGNORECASE,
)
BRACKETED_IMPLIED_DO_RE = re.compile(r"^\[\s*(\(.+\))\s*\]$", re.IGNORECASE)
CONSTRUCTOR_RE = re.compile(r"^\s*([a-z][a-z0-9_]*)\s*=\s*\[(.+)\]\s*$", re.IGNORECASE)
CONSTRUCTOR_IMPLIED_DO_RE = re.compile(
    r"^\s*([a-z][a-z0-9_]*)\s*=\s*\[\s*\(\s*(.+)\s*,\s*([a-z][a-z0-9_]*)\s*=\s*([^,\]]+)\s*,\s*([^,\]]+)(?:\s*,\s*([^,\]]+))?\s*\)\s*\]\s*$",
    re.IGNORECASE,
)
FORALL_END_RE = re.compile(r"^\s*end\s*forall\b", re.IGNORECASE)
NUM_TOKEN_RE = re.compile(r"[+-]?(?:\d+\.\d*|\.\d+|\d+)(?:[eEdD][+-]?\d+)?")
PROGRAM_START_RE = re.compile(r"^\s*program\b", re.IGNORECASE)
PROGRAM_END_RE = re.compile(r"^\s*end\s+program\b", re.IGNORECASE)
RANDOM_NUMBER_RE = re.compile(r"^\s*call\s+random_number\s*\(", re.IGNORECASE)
RANDOM_SEED_CTRL_RE = re.compile(r"^\s*call\s+random_(?:seed|init)\s*\(", re.IGNORECASE)
ALLOCATABLE_ATTR_RE = re.compile(r"\ballocatable\b", re.IGNORECASE)
ALLOCATE_STMT_RE = re.compile(r"^\s*allocate\s*\((.*)\)\s*$", re.IGNORECASE)
CALL_LIKE_RE = re.compile(r"\b([a-z][a-z0-9_]*)\s*\(", re.IGNORECASE)

# Conservative set: calls outside this set are treated as potentially
# non-elemental and are not loopified by the generic whole-assign rule.
ELEMENTAL_INTRINSICS = {
    "abs", "acos", "acosd", "acosh", "asin", "asind", "asinh", "atan",
    "atan2", "atan2d", "atand", "atanh", "aint", "anint", "bessel_j0",
    "bessel_j1", "bessel_y0", "bessel_y1", "ceiling", "cmplx", "conjg",
    "cos", "cosd", "cosh", "dble", "dim", "dprod", "exp", "floor", "iachar",
    "ichar", "int", "log", "log10", "log_gamma", "max", "min", "mod",
    "modulo", "nint", "real", "sign", "sin", "sind", "sinh", "spacing",
    "sqrt", "tan", "tand", "tanh", "gamma",
}

# Array transformational intrinsics that loopified output must not contain.
ARRAY_TRANSFORM_INTRINSICS = {
    "all",
    "any",
    "count",
    "cshift",
    "dot_product",
    "eoshift",
    "findloc",
    "iall",
    "iany",
    "iparity",
    "maxloc",
    "maxval",
    "matmul",
    "minloc",
    "minval",
    "norm2",
    "pack",
    "parity",
    "product",
    "reduce",
    "reshape",
    "spread",
    "sum",
    "transpose",
    "unpack",
}


@dataclass
class VarInfo:
    rank: int
    bounds: List[str]


@dataclass
class Finding:
    path: Path
    rule: str
    start_line: int
    end_line: int
    suggestion: str


def split_top_level_commas(text: str) -> List[str]:
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


def strip_string_literals(text: str) -> str:
    out: List[str] = []
    in_single = False
    in_double = False
    for ch in text:
        if ch == "'" and not in_double:
            in_single = not in_single
            out.append(" ")
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            out.append(" ")
            continue
        out.append(" " if (in_single or in_double) else ch)
    return "".join(out)


def split_range(rng: str) -> Tuple[str, str]:
    txt = rng.strip()
    if ":" in txt:
        a, b = txt.split(":", 1)
        return a.strip(), b.strip()
    return "1", txt


def resolve_dim_bounds(arr_name: str, dim0: int, bound_spec: str) -> Tuple[str, str]:
    """Return robust (lb, ub) for one declared dimension spec.

    For deferred/assumed shape specs like ":" or "lo:"/":hi", fallback to
    lbound/ubound expressions.
    """
    txt = bound_spec.strip()
    if ":" not in txt:
        return "1", txt
    lo, hi = [p.strip() for p in txt.split(":", 1)]
    d = dim0 + 1
    if not lo:
        # For declarations like (:), (:n), lower bound is implied as 1.
        lo = "1"
    if not hi:
        hi = f"ubound({arr_name}, {d})"
    return lo, hi


def parse_declared_entities(line: str) -> Dict[str, VarInfo]:
    code = fscan.strip_comment(line).strip()
    if not TYPE_DECL_RE.match(code):
        return {}
    if "::" in code:
        rhs = code.split("::", 1)[1]
    else:
        m = re.match(
            r"^\s*(?:integer|real|logical|character|complex|type\s*\([^)]*\)|class\s*\([^)]*\))\s+(.+)$",
            code,
            re.IGNORECASE,
        )
        if not m:
            return {}
        rhs = m.group(1)
    out: Dict[str, VarInfo] = {}
    for chunk in split_top_level_commas(rhs):
        txt = chunk.strip()
        if not txt:
            continue
        if "=" in txt and "=>" not in txt:
            txt = txt.split("=", 1)[0].strip()
        m2 = NAME_WITH_ARGS_RE.match(txt)
        if m2:
            name = m2.group(1).lower()
            dims = [d.strip() for d in split_top_level_commas(m2.group(2)) if d.strip()]
            bounds: List[str] = []
            for d in dims:
                if ":" in d:
                    bounds.append(d)
                else:
                    bounds.append(f"1:{d}")
            out[name] = VarInfo(rank=len(bounds), bounds=bounds)
            continue
        m3 = SIMPLE_NAME_RE.match(txt)
        if m3:
            out[m3.group(1).lower()] = VarInfo(rank=0, bounds=[])
    return out


def parse_declared_allocatable_names(line: str) -> List[str]:
    code = fscan.strip_comment(line).strip()
    if not TYPE_DECL_RE.match(code):
        return []
    if not ALLOCATABLE_ATTR_RE.search(code):
        return []
    return list(parse_declared_entities(line).keys())


def collect_declared_info(raw_lines: List[str]) -> Tuple[Dict[str, VarInfo], set[str]]:
    """Collect declared entities, handling simple free-form continuations."""
    vars_info: Dict[str, VarInfo] = {}
    allocatable_names: set[str] = set()
    buf: Optional[str] = None

    def flush_buf() -> None:
        nonlocal buf
        if not buf:
            return
        vars_info.update(parse_declared_entities(buf))
        allocatable_names.update(parse_declared_allocatable_names(buf))
        buf = None

    for ln in raw_lines:
        code = fscan.strip_comment(ln).rstrip()
        if not code:
            flush_buf()
            continue

        if buf is not None:
            part = code.lstrip()
            if part.startswith("&"):
                part = part[1:].lstrip()
            cont = part.endswith("&")
            if cont:
                part = part[:-1].rstrip()
            buf = f"{buf} {part}".strip()
            if not cont:
                flush_buf()
            continue

        if TYPE_DECL_RE.match(code):
            cont = code.endswith("&")
            head = code[:-1].rstrip() if cont else code
            if cont:
                buf = head
            else:
                vars_info.update(parse_declared_entities(head))
                allocatable_names.update(parse_declared_allocatable_names(head))
            continue

        # Ignore non-declaration lines here.

    flush_buf()
    return vars_info, allocatable_names


PROC_START_RE = re.compile(r"^\s*(program|subroutine|function|module|block\s+data)\b", re.IGNORECASE)
CONTAINS_RE = re.compile(r"^\s*contains\b", re.IGNORECASE)


def local_declared_rank(raw_lines: List[str], line_1based: int, name: str) -> Optional[int]:
    """Best-effort local declaration rank lookup within current procedure scope."""
    nm = name.lower()
    i = line_1based - 1
    while i >= 1:
        code = fscan.strip_comment(raw_lines[i - 1]).strip()
        if not code:
            i -= 1
            continue
        decls = parse_declared_entities(raw_lines[i - 1])
        if nm in decls:
            return decls[nm].rank
        if CONTAINS_RE.match(code) or PROC_START_RE.match(code):
            break
        i -= 1
    return None


def local_declared_var(raw_lines: List[str], line_1based: int, name: str) -> Optional[VarInfo]:
    """Best-effort local declaration lookup within current procedure scope."""
    nm = name.lower()
    i = line_1based - 1
    while i >= 1:
        code = fscan.strip_comment(raw_lines[i - 1]).strip()
        if not code:
            i -= 1
            continue
        decls = parse_declared_entities(raw_lines[i - 1])
        if nm in decls:
            return decls[nm]
        if CONTAINS_RE.match(code) or PROC_START_RE.match(code):
            break
        i -= 1
    return None


def collect_decl_type_specs(raw_lines: List[str]) -> Dict[str, str]:
    """Collect base type specs (e.g. 'real(kind=dp)') for declared names."""
    type_map: Dict[str, str] = {}
    buf: Optional[str] = None

    def base_type(spec: str) -> str:
        spec = spec.strip()
        depth = 0
        out: List[str] = []
        for ch in spec:
            if ch == "(":
                depth += 1
            elif ch == ")" and depth > 0:
                depth -= 1
            elif ch == "," and depth == 0:
                break
            out.append(ch)
        return "".join(out).strip()

    def flush_buf() -> None:
        nonlocal buf
        if not buf:
            return
        code = fscan.strip_comment(buf).strip()
        m = re.match(
            r"^\s*([a-z][a-z0-9_]*(?:\s*\([^)]*\))?(?:\s*,[^:]*)?)\s*::\s*(.+)$",
            code,
            re.IGNORECASE,
        )
        if m:
            typ = base_type(m.group(1))
            for ent in split_top_level_commas(m.group(2)):
                txt = ent.strip()
                if not txt:
                    continue
                txt = txt.split("=", 1)[0].strip()
                mm = re.match(r"^([a-z][a-z0-9_]*)", txt, re.IGNORECASE)
                if mm:
                    type_map[mm.group(1).lower()] = typ
        buf = None

    for ln in raw_lines:
        code = fscan.strip_comment(ln).rstrip()
        if not code:
            flush_buf()
            continue
        if buf is not None:
            part = code.lstrip()
            if part.startswith("&"):
                part = part[1:].lstrip()
            cont = part.endswith("&")
            if cont:
                part = part[:-1].rstrip()
            buf = f"{buf} {part}".strip()
            if not cont:
                flush_buf()
            continue
        if "::" in code:
            cont = code.endswith("&")
            head = code[:-1].rstrip() if cont else code
            if cont:
                buf = head
            else:
                buf = head
                flush_buf()
    flush_buf()
    return type_map


def has_prior_allocate_for(name: str, raw_lines: List[str], upto_line_1based: int) -> bool:
    pat = re.compile(rf"\b{re.escape(name)}\s*\(", re.IGNORECASE)
    for i in range(max(0, upto_line_1based - 1)):
        code = fscan.strip_comment(raw_lines[i]).strip()
        if not code:
            continue
        m = ALLOCATE_STMT_RE.match(code)
        if not m:
            continue
        if pat.search(m.group(1)):
            return True
    return False


def choose_files(args_files: List[Path], exclude: Iterable[str]) -> List[Path]:
    if args_files:
        files = cpaths.expand_source_inputs(args_files)
    else:
        files = sorted(
            set(Path(".").glob("*.f90")) | set(Path(".").glob("*.F90")),
            key=lambda p: p.name.lower(),
        )
    return fscan.apply_excludes(files, exclude)


def loop_vars_for_rank(rank: int, vars_info: Optional[Dict[str, VarInfo]] = None) -> List[str]:
    if USE_XTO_INDICES:
        return ["i_xto", "j_xto", "k_xto", "l_xto"][:rank]
    bases = ["i", "j", "k", "l"][:rank]
    existing = set((vars_info or {}).keys())
    picked: List[str] = []
    for base in bases:
        cand = base
        while cand.lower() in existing or cand.lower() in {p.lower() for p in picked}:
            cand = cand + "_"
        picked.append(cand)
    return picked


def generated_loop_var_decl(vars_info: Optional[Dict[str, VarInfo]] = None, rank: int = 3) -> str:
    return f"integer :: {', '.join(loop_vars_for_rank(rank, vars_info))}"


def generated_loop_var_decl_used(
    body_lines: Sequence[str],
    vars_info: Optional[Dict[str, VarInfo]] = None,
    rank: int = 3,
) -> Optional[str]:
    txt = "\n".join(body_lines)
    used: List[str] = []
    for v in loop_vars_for_rank(rank, vars_info):
        if re.search(rf"\b{re.escape(v)}\b", txt):
            used.append(v)
    if not used:
        return None
    return f"integer :: {', '.join(used)}"


def is_generated_loop_var(name: str) -> bool:
    n = name.lower()
    if n in {"i_xto", "j_xto", "k_xto", "l_xto"}:
        return True
    return re.fullmatch(r"[ijkl]_*$", n) is not None


def parse_name_and_args(text: str) -> Optional[Tuple[str, List[str]]]:
    m = NAME_WITH_ARGS_RE.match(text.strip())
    if not m:
        return None
    return m.group(1).lower(), [a.strip() for a in split_top_level_commas(m.group(2))]


def parse_dot_product_args(text: str) -> Optional[Tuple[str, str]]:
    m = DOT_PRODUCT_RE.match(text.strip())
    if not m:
        return None
    args = split_top_level_commas(m.group(1).strip())
    if len(args) != 2:
        return None
    return args[0].strip(), args[1].strip()


def parse_matmul_args(text: str) -> Optional[Tuple[str, str]]:
    m = MATMUL_RE.match(text.strip())
    if not m:
        return None
    inner = text.strip()
    if "(" not in inner or ")" not in inner:
        return None
    inner = inner[inner.find("(") + 1 : inner.rfind(")")].strip()
    args = split_top_level_commas(inner)
    if len(args) != 2:
        return None
    return args[0].strip(), args[1].strip()


def parse_pack_args(text: str) -> Optional[Tuple[str, str, Optional[str]]]:
    m = PACK_RE.match(text.strip())
    if not m:
        return None
    inner = m.group(1).strip()
    args = split_top_level_commas(inner)
    if len(args) < 2 or len(args) > 3:
        return None
    arr = args[0].strip()
    mask = args[1].strip()
    vec = args[2].strip() if len(args) == 3 else None
    return arr, mask, vec


def norm_expr(text: str) -> str:
    return re.sub(r"\s+", "", text).lower()


def vector_view(expr: str, vars_info: Dict[str, VarInfo], ivar: str) -> Optional[Tuple[str, str, str]]:
    """Resolve a vector expression into (element_expr, lb, ub)."""
    e = expr.strip()
    m = SIMPLE_NAME_RE.match(e)
    if m:
        nm = m.group(1).lower()
        vi = vars_info.get(nm)
        if vi is None or vi.rank != 1:
            return None
        lb, ub = resolve_dim_bounds(nm, 0, vi.bounds[0])
        return f"{nm}({ivar})", lb, ub

    pa = parse_name_and_args(e)
    if pa is None:
        return None
    nm, args = pa
    vi = vars_info.get(nm)
    if vi is None:
        return None

    if vi.rank == 1 and len(args) == 1:
        a0 = args[0].strip()
        if a0 == ":":
            lb, ub = resolve_dim_bounds(nm, 0, vi.bounds[0])
            return f"{nm}({ivar})", lb, ub
        if ":" in a0:
            lb, ub = split_range(a0)
            return f"{nm}({ivar})", lb, ub
        return None

    if vi.rank == 2 and len(args) == 2:
        a0 = args[0].strip()
        a1 = args[1].strip()
        if a0 == ":" and a1 != ":":
            lb, ub = resolve_dim_bounds(nm, 0, vi.bounds[0])
            return f"{nm}({ivar},{a1})", lb, ub
        if a1 == ":" and a0 != ":":
            lb, ub = resolve_dim_bounds(nm, 1, vi.bounds[1])
            return f"{nm}({a0},{ivar})", lb, ub
    return None


def bound_from_arg(arg: str, arr_name: str, dim0: int, default_bound: str) -> Optional[Tuple[str, str]]:
    a = arg.strip()
    if a == ":":
        return resolve_dim_bounds(arr_name, dim0, default_bound)
    if ":" in a:
        return split_range(a)
    return None


def matrix_view(expr: str, vars_info: Dict[str, VarInfo], ivar: str, jvar: str) -> Optional[Tuple[str, Tuple[str, str], Tuple[str, str]]]:
    e = expr.strip()
    m = SIMPLE_NAME_RE.match(e)
    if m:
        nm = m.group(1).lower()
        vi = vars_info.get(nm)
        if vi is None or vi.rank != 2:
            return None
        b1 = resolve_dim_bounds(nm, 0, vi.bounds[0])
        b2 = resolve_dim_bounds(nm, 1, vi.bounds[1])
        return f"{nm}({ivar},{jvar})", b1, b2
    pa = parse_name_and_args(e)
    if pa is None:
        return None
    nm, args = pa
    vi = vars_info.get(nm)
    if vi is None or vi.rank != 2 or len(args) != 2:
        return None
    b1 = bound_from_arg(args[0], nm, 0, vi.bounds[0])
    b2 = bound_from_arg(args[1], nm, 1, vi.bounds[1])
    if b1 is None or b2 is None:
        return None
    return f"{nm}({ivar},{jvar})", b1, b2


def index_expr_for_arrays(expr: str, rank: int, idx_vars: Sequence[str], vars_info: Dict[str, VarInfo]) -> str:
    out = expr
    names = sorted(vars_info.keys(), key=len, reverse=True)
    idx = ", ".join(idx_vars[:rank])
    for name in names:
        vi = vars_info[name]
        if vi.rank != rank:
            continue
        # Do not indexize when already subscripted/called, or when used as
        # a derived-type base (name%field).
        out = re.sub(rf"\b{re.escape(name)}\b(?!\s*[\(%])", f"{name}({idx})", out, flags=re.IGNORECASE)
    return out


def build_nested_loops(
    indent: str,
    bounds: List[str],
    body_line: str,
    *,
    arr_name: str,
    vars_info: Optional[Dict[str, VarInfo]] = None,
) -> str:
    idx = loop_vars_for_rank(len(bounds), vars_info)
    lines: List[str] = []
    # Fortran is column-major: keep first index as innermost loop.
    for level, d in enumerate(reversed(range(len(bounds)))):
        b = bounds[d]
        lb, ub = resolve_dim_bounds(arr_name, d, b)
        lines.append(f"{indent}{' ' * (3 * level)}do {idx[d]} = {lb}, {ub}")
    lines.append(f"{indent}{' ' * (3 * len(bounds))}{body_line}")
    for level in reversed(range(len(bounds))):
        lines.append(f"{indent}{' ' * (3 * level)}end do")
    return "\n".join(lines)


def maybe_reverse_implied_do(code: str, indent: str) -> Optional[Tuple[str, str]]:
    low = code.lstrip().lower()
    if low.startswith("print"):
        mp = re.match(r"^(\s*print\s+)(.+)$", code, re.IGNORECASE)
        if not mp:
            return None
        rest = mp.group(2).strip()
        parts = split_top_level_commas(rest)
        if len(parts) < 2:
            return None
        prefix = f"{mp.group(1)}{', '.join(p.strip() for p in parts[:-1])}"
        group = parts[-1].strip()
        bracketed = False
        bm = BRACKETED_IMPLIED_DO_RE.fullmatch(group)
        if bm:
            bracketed = True
            group = bm.group(1).strip()
        m = IMPLIED_DO_TRAIL_RE.fullmatch(group)
        if not m:
            return None
        if bracketed:
            return "implied_do_io_unwrap", f"{indent}{prefix}, {group}"
        body = m.group(1).strip()
        stmt = f"{prefix}, {body}"
    else:
        m_rw = re.match(r"^\s*((?:read|write)\s*\([^)]*\)\s*)\s*(\(.+\))\s*$", code, re.IGNORECASE)
        if not m_rw:
            return None
        prefix = m_rw.group(1).rstrip()
        group = m_rw.group(2).strip()
        bracketed = False
        bm = BRACKETED_IMPLIED_DO_RE.fullmatch(group)
        if bm:
            bracketed = True
            group = bm.group(1).strip()
        m = IMPLIED_DO_TRAIL_RE.fullmatch(group)
        if not m:
            return None
        if bracketed:
            return "implied_do_io_unwrap", f"{indent}{prefix}{group}"
        body = m.group(1).strip()
        stmt = f"{prefix}({body})"
    var = m.group(2).strip()
    lb = m.group(3).strip()
    ub = m.group(4).strip()
    st = m.group(5).strip() if m.group(5) else None
    do_head = f"do {var} = {lb}, {ub}" + (f", {st}" if st else "")
    return "implied_do_io", "\n".join([f"{indent}{do_head}", f"{indent}   {stmt}", f"{indent}end do"])


IMPLIED_DO_TERM_RE = re.compile(
    r"^\(\s*(.+)\s*,\s*([a-z][a-z0-9_]*)\s*=\s*([^,\]]+)\s*,\s*([^,\]]+)(?:\s*,\s*([^,\]]+))?\s*\)$",
    re.IGNORECASE,
)


def parse_array_constructor_body(expr: str) -> Optional[str]:
    t = expr.strip()
    if t.startswith("[") and t.endswith("]"):
        return t[1:-1].strip()
    if t.startswith("(/") and t.endswith("/)"):
        return t[2:-2].strip()
    return None


def maybe_reverse_random_seed_put_constructor(code: str, indent: str) -> Optional[Tuple[str, str]]:
    m = re.match(r"^\s*call\s+random_seed\s*\((.*)\)\s*$", code, re.IGNORECASE)
    if not m:
        return None
    args = split_top_level_commas(m.group(1).strip())
    put_idx = -1
    put_rhs = ""
    for i, a in enumerate(args):
        ma = re.match(r"^\s*put\s*=\s*(.+)\s*$", a, re.IGNORECASE)
        if ma:
            put_idx = i
            put_rhs = ma.group(1).strip()
            break
    if put_idx < 0:
        return None
    ctor_body = parse_array_constructor_body(put_rhs)
    if ctor_body is None:
        return None
    terms = [t.strip() for t in split_top_level_commas(ctor_body) if t.strip()]
    if not terms:
        return None

    lines: List[str] = [
        f"{indent}block",
        f"{indent}   integer, allocatable :: put_tmp(:)",
    ]
    # Fast path: single implied-do term; allocate directly from bounds.
    if len(terms) == 1:
        mi0 = IMPLIED_DO_TERM_RE.match(terms[0])
        if mi0:
            expr = mi0.group(1).strip()
            iv = mi0.group(2).strip()
            lb = mi0.group(3).strip()
            ub = mi0.group(4).strip()
            st = mi0.group(5).strip() if mi0.group(5) else None
            if st is None:
                lb_norm = lb.replace(" ", "").lower()
                if lb_norm == "1":
                    lines.append(f"{indent}   allocate(put_tmp({ub}))")
                else:
                    lines.append(f"{indent}   allocate(put_tmp(({ub})-({lb})+1))")
                lines.append(f"{indent}   do {iv} = {lb}, {ub}")
                if lb_norm == "1":
                    lines.append(f"{indent}      put_tmp({iv}) = {expr}")
                else:
                    lines.append(f"{indent}      put_tmp(({iv})-({lb})+1) = {expr}")
                lines.append(f"{indent}   end do")
                new_args = list(args)
                new_args[put_idx] = "put=put_tmp"
                lines.append(f"{indent}   call random_seed({', '.join(new_args)})")
                lines.append(f"{indent}   if (allocated(put_tmp)) deallocate(put_tmp)")
                lines.append(f"{indent}end block")
                return "random_seed_put_constructor_to_loop", "\n".join(lines)

    lines.extend(
        [
            f"{indent}   integer :: i_, k_",
            f"{indent}   k_ = 0",
        ]
    )
    for t in terms:
        mi = IMPLIED_DO_TERM_RE.match(t)
        if mi:
            iv = mi.group(2).strip()
            lb = mi.group(3).strip()
            ub = mi.group(4).strip()
            st = mi.group(5).strip() if mi.group(5) else None
            head = f"do {iv} = {lb}, {ub}" + (f", {st}" if st else "")
            lines.append(f"{indent}   {head}")
            lines.append(f"{indent}      k_ = k_ + 1")
            lines.append(f"{indent}   end do")
        else:
            lines.append(f"{indent}   k_ = k_ + 1")
    lines.append(f"{indent}   allocate(put_tmp(k_))")
    lines.append(f"{indent}   k_ = 1")
    for t in terms:
        mi = IMPLIED_DO_TERM_RE.match(t)
        if mi:
            expr = mi.group(1).strip()
            iv = mi.group(2).strip()
            lb = mi.group(3).strip()
            ub = mi.group(4).strip()
            st = mi.group(5).strip() if mi.group(5) else None
            head = f"do {iv} = {lb}, {ub}" + (f", {st}" if st else "")
            lines.append(f"{indent}   {head}")
            lines.append(f"{indent}      put_tmp(k_) = {expr}")
            lines.append(f"{indent}      k_ = k_ + 1")
            lines.append(f"{indent}   end do")
        else:
            lines.append(f"{indent}   put_tmp(k_) = {t}")
            lines.append(f"{indent}   k_ = k_ + 1")
    new_args = list(args)
    new_args[put_idx] = "put=put_tmp"
    lines.append(f"{indent}   call random_seed({', '.join(new_args)})")
    lines.append(f"{indent}   if (allocated(put_tmp)) deallocate(put_tmp)")
    lines.append(f"{indent}end block")
    return "random_seed_put_constructor_to_loop", "\n".join(lines)


def maybe_reverse_random_number_call(code: str, indent: str, vars_info: Dict[str, VarInfo]) -> Optional[Tuple[str, str]]:
    m = re.match(r"^\s*call\s+random_number\s*\((.+)\)\s*$", code, re.IGNORECASE)
    if not m:
        return None
    arg = m.group(1).strip()
    base = fscan.base_identifier(arg)
    if base is None:
        return None
    vi = vars_info.get(base.lower())
    if vi is None or vi.rank < 1 or vi.rank > 3:
        return None
    idx = loop_vars_for_rank(vi.rank, vars_info)
    body = f"call random_number({base}({', '.join(idx[:vi.rank])}))"
    loop_block = build_nested_loops(indent + "   ", vi.bounds, body, arr_name=base.lower(), vars_info=vars_info)
    lines = [
        f"{indent}block",
        f"{indent}   {generated_loop_var_decl(vars_info, rank=max(1, vi.rank))}",
        loop_block,
        f"{indent}end block",
    ]
    return "random_number_array_to_loop", "\n".join(lines)


def maybe_reverse_io_array_expr(code: str, indent: str, vars_info: Dict[str, VarInfo]) -> Optional[Tuple[str, str]]:
    """Rewrite print/write trailing rank-1 array binary expr to implied-do.

    Example:
      print "...", a - b
    -> print "...", (a(i) - b(i), i = 1, size(a))
    """
    prefix = ""
    parts: List[str] = []

    mp = re.match(r"^\s*print\s+(.+)$", code, re.IGNORECASE)
    if mp:
        rest = mp.group(1).strip()
        parts = split_top_level_commas(rest)
        if len(parts) < 2:
            return None
        prefix = f"print {', '.join(p.strip() for p in parts[:-1])}"
    else:
        mw = re.match(r"^\s*write\s*\(([^)]*)\)\s*,\s*(.+)$", code, re.IGNORECASE)
        if not mw:
            return None
        prefix = f"write ({mw.group(1).strip()})"
        parts = split_top_level_commas(mw.group(2).strip())
        if len(parts) < 1:
            return None

    last = parts[-1].strip()
    m = re.match(r"^\s*([a-z][a-z0-9_]*)\s*([+\-*/])\s*([a-z][a-z0-9_]*)\s*$", last, re.IGNORECASE)
    if not m:
        return None
    a = m.group(1).lower()
    op = m.group(2)
    b = m.group(3).lower()

    va = vars_info.get(a)
    vb = vars_info.get(b)
    ra = va.rank if va is not None else 0
    rb = vb.rank if vb is not None else 0
    if ra > 1 or rb > 1:
        return None
    if ra == 0 and rb == 0:
        return None

    arr = a if ra == 1 else b
    iv = loop_vars_for_rank(1, vars_info)[0]
    ea = f"{a}({iv})" if ra == 1 else a
    eb = f"{b}({iv})" if rb == 1 else b
    parts[-1] = f"({ea} {op} {eb}, {iv} = 1, size({arr}))"
    return "io_array_expr_to_implied_do", f"{indent}{prefix}, {', '.join(p.strip() for p in parts)}"


def maybe_reverse_matmul(lhs: str, rhs: str, indent: str, vars_info: Dict[str, VarInfo]) -> Optional[Tuple[str, str]]:
    m_tscaled = re.match(
        r"^\s*matmul\s*\(\s*transpose\s*\(\s*([a-z][a-z0-9_]*)\s*\)\s*,\s*([a-z][a-z0-9_]*)\s*\)\s*/\s*(.+)\s*$",
        rhs,
        re.IGNORECASE,
    )
    if m_tscaled is not None:
        a_name = m_tscaled.group(1).lower()
        b_name = m_tscaled.group(2).lower()
        scale = m_tscaled.group(3).strip()
        lhs_name = fscan.base_identifier(lhs)
        if lhs_name is None:
            return None
        vi_lhs = vars_info.get(lhs_name.lower())
        vi_a = vars_info.get(a_name)
        vi_b = vars_info.get(b_name)
        if (
            vi_lhs is None
            or vi_a is None
            or vi_b is None
            or vi_lhs.rank != 2
            or vi_a.rank != 2
            or vi_b.rank != 2
        ):
            return None
        i, j, k = loop_vars_for_rank(3, vars_info)[:3]
        lhs_v = matrix_view(lhs, vars_info, i, k)
        if lhs_v is None:
            return None
        lhs_elem, (lb_i, ub_i), (lb_k, ub_k) = lhs_v
        lb_j, ub_j = split_range(vi_a.bounds[0])
        lines = [
            f"{indent}block",
            f"{indent}   {generated_loop_var_decl(vars_info, rank=3)}",
            f"{indent}   do {i} = {lb_i}, {ub_i}",
            f"{indent}      do {k} = {lb_k}, {ub_k}",
            f"{indent}         {lhs_elem} = 0.0",
            f"{indent}         do {j} = {lb_j}, {ub_j}",
            f"{indent}            {lhs_elem} = {lhs_elem} + {a_name}({j},{i}) * {b_name}({j},{k})",
            f"{indent}         end do",
            f"{indent}         {lhs_elem} = {lhs_elem} / ({scale})",
            f"{indent}      end do",
            f"{indent}   end do",
            f"{indent}end block",
        ]
        return "matmul_transpose_scaled_to_loop", "\n".join(lines)

    mm = parse_matmul_args(rhs)
    if mm is None:
        return None
    a_expr, b_expr = mm
    lhs_name = fscan.base_identifier(lhs)
    if lhs_name is None:
        return None
    vi_lhs = vars_info.get(lhs_name.lower())
    if vi_lhs is None:
        return None
    a_name = fscan.base_identifier(a_expr)
    b_name = fscan.base_identifier(b_expr)
    vi_a = vars_info.get(a_name.lower()) if a_name is not None else None
    vi_b = vars_info.get(b_name.lower()) if b_name is not None else None
    if vi_lhs.rank == 1 and vi_a is not None and vi_b is not None and vi_a.rank == 2 and vi_b.rank == 1:
        i, j = loop_vars_for_rank(2, vars_info)[:2]
        lb1, ub1 = split_range(vi_lhs.bounds[0])
        lb2, ub2 = split_range(vi_b.bounds[0])
        lines = [
            f"{indent}block",
            f"{indent}   {generated_loop_var_decl(vars_info, rank=2)}",
            f"{indent}   {lhs_name} = 0.0",
            f"{indent}   do {j} = {lb2}, {ub2}",
            f"{indent}      do {i} = {lb1}, {ub1}",
            f"{indent}         {lhs_name}({i}) = {lhs_name}({i}) + {a_name}({i},{j}) * {b_name}({j})",
            f"{indent}      end do",
            f"{indent}   end do",
            f"{indent}end block",
        ]
        return "matmul_to_loop_mv", "\n".join(lines)
    if vi_lhs.rank == 2:
        i, j, k = loop_vars_for_rank(3, vars_info)[:3]
        lhs_v = matrix_view(lhs, vars_info, i, k)
        a_v = matrix_view(a_expr, vars_info, i, j)
        b_v = matrix_view(b_expr, vars_info, j, k)
        if lhs_v is None or a_v is None or b_v is None:
            return None
        lhs_elem, (lb_i, ub_i), (lb_k, ub_k) = lhs_v
        a_elem, (_, _), (lb_j, ub_j) = a_v
        b_elem, (_lb_j_b, _ub_j_b), (_, _) = b_v
        lines = [
            f"{indent}block",
            f"{indent}   {generated_loop_var_decl(vars_info, rank=3)}",
            f"{indent}   do {i} = {lb_i}, {ub_i}",
            f"{indent}      do {k} = {lb_k}, {ub_k}",
            f"{indent}         {lhs_elem} = 0.0",
            f"{indent}         do {j} = {lb_j}, {ub_j}",
            f"{indent}            {lhs_elem} = {lhs_elem} + {a_elem} * {b_elem}",
            f"{indent}         end do",
            f"{indent}      end do",
            f"{indent}   end do",
            f"{indent}end block",
        ]
        return "matmul_to_loop_mm", "\n".join(lines)
    return None


def maybe_reverse_dot_product(lhs: str, rhs: str, indent: str, vars_info: Dict[str, VarInfo], type_map: Dict[str, str]) -> Optional[Tuple[str, str]]:
    parsed = parse_dot_product_args(rhs)
    if parsed is None:
        return None
    lhs_name = fscan.base_identifier(lhs)
    if lhs_name is None:
        return None
    i = loop_vars_for_rank(1, vars_info)[0]
    lhs_norm = lhs.strip()
    view_a = vector_view(parsed[0], vars_info, i)
    view_b = vector_view(parsed[1], vars_info, i)
    if view_a is None or view_b is None:
        return None
    a_elem, lba, uba = view_a
    b_elem, lbb, ubb = view_b
    if norm_expr(lba) != norm_expr(lbb):
        return None
    if norm_expr(uba) != norm_expr(ubb):
        return None
    typ = type_map.get(lhs_name.lower(), "real")
    init = init_literal_for_type(typ)
    lines = [
        f"{indent}block",
        f"{indent}   {generated_loop_var_decl(vars_info, rank=1)}",
        f"{indent}   {lhs_norm} = {init}",
        f"{indent}   do {i} = {lba}, {uba}",
        f"{indent}      {lhs_norm} = {lhs_norm} + {a_elem} * {b_elem}",
        f"{indent}   end do",
        f"{indent}end block",
    ]
    return "dot_product_to_loop", "\n".join(lines)


def maybe_reverse_transpose(lhs: str, rhs: str, indent: str, vars_info: Dict[str, VarInfo]) -> Optional[Tuple[str, str]]:
    m = TRANSPOSE_RE.match(rhs)
    if not m:
        return None
    lhs_name = fscan.base_identifier(lhs)
    a = m.group(1).lower()
    if lhs_name is None:
        return None
    vl = vars_info.get(lhs_name.lower())
    va = vars_info.get(a)
    if vl is None or va is None or vl.rank != 2 or va.rank != 2:
        return None
    i, j = loop_vars_for_rank(2, vars_info)[:2]
    l1, u1 = resolve_dim_bounds(lhs_name.lower(), 0, vl.bounds[0])
    l2, u2 = resolve_dim_bounds(lhs_name.lower(), 1, vl.bounds[1])
    lines = [
        f"{indent}block",
        f"{indent}   {generated_loop_var_decl(vars_info, rank=2)}",
        f"{indent}   do {j} = {l2}, {u2}",
        f"{indent}      do {i} = {l1}, {u1}",
        f"{indent}         {lhs_name}({i},{j}) = {a}({j},{i})",
        f"{indent}      end do",
        f"{indent}   end do",
        f"{indent}end block",
    ]
    return "transpose_to_loop", "\n".join(lines)


def maybe_reverse_reshape(
    lhs: str,
    rhs: str,
    indent: str,
    vars_info: Dict[str, VarInfo],
    allocatable_names: Optional[set[str]] = None,
) -> Optional[Tuple[str, str]]:
    """Reverse simple RESHAPE(source, [d1,...]) assignments to nested loops."""
    lhs_name = fscan.base_identifier(lhs)
    if lhs_name is None:
        return None
    vl = vars_info.get(lhs_name.lower())
    if vl is None or vl.rank < 1 or vl.rank > 3:
        return None

    pa = parse_name_and_args(rhs)
    if pa is None:
        return None
    fn, args = pa
    if fn.lower() != "reshape" or len(args) < 2:
        return None
    src_expr = args[0].strip()
    shape_expr = args[1].strip()
    pad_expr: Optional[str] = None
    for extra in args[2:]:
        ex = extra.strip()
        m_kw = re.match(r"^\s*([a-z][a-z0-9_]*)\s*=\s*(.+)$", ex, re.IGNORECASE)
        if m_kw:
            kw = m_kw.group(1).lower()
            val = m_kw.group(2).strip()
            if kw == "pad":
                pad_expr = val
            elif kw == "order":
                # ORDER changes element order; skip for conservative correctness.
                return None
            else:
                # Unknown keyword argument.
                return None
        else:
            # Positional 3rd arg is PAD; 4th is ORDER (unsupported).
            if pad_expr is None:
                pad_expr = ex
            else:
                return None

    msrc = SIMPLE_NAME_RE.match(src_expr)
    if msrc is None:
        return None
    src_name = msrc.group(1).lower()
    vs = vars_info.get(src_name)
    if vs is None or vs.rank != 1:
        return None

    mshape = re.match(r"^\[\s*(.+)\s*\]$", shape_expr)
    if mshape is None:
        return None
    shape_items = [t.strip() for t in split_top_level_commas(mshape.group(1)) if t.strip()]
    if len(shape_items) != vl.rank:
        return None

    is_alloc = allocatable_names is not None and lhs_name.lower() in allocatable_names
    pad_name: Optional[str] = None
    if pad_expr is not None:
        mpad = SIMPLE_NAME_RE.match(pad_expr)
        if mpad is None:
            return None
        pad_name = mpad.group(1).lower()
        vp = vars_info.get(pad_name)
        if vp is None or vp.rank != 1:
            return None
    idx = loop_vars_for_rank(vl.rank, vars_info)
    taken = set(vars_info.keys())
    taken.update(v.lower() for v in idx)
    pos = unique_temp_name("k_reshape", taken)
    src_n = unique_temp_name("n_src", taken)
    pad_n = unique_temp_name("n_pad", taken)
    pad_k = unique_temp_name("k_pad", taken)

    lines: List[str] = [f"{indent}block"]
    d = generated_loop_var_decl_used(
        [f"{lhs_name}({', '.join(idx[:vl.rank])}) = {src_name}({pos})"],
        vars_info,
        rank=vl.rank,
    )
    if d:
        lines.append(f"{indent}   {d}")
    lines.append(f"{indent}   integer :: {pos}")
    lines.append(f"{indent}   integer :: {src_n}")
    if pad_name is not None:
        lines.append(f"{indent}   integer :: {pad_n}, {pad_k}")
    if is_alloc:
        lines.append(f"{indent}   if (allocated({lhs_name})) deallocate({lhs_name})")
        lines.append(f"{indent}   allocate({lhs_name}({', '.join(shape_items)}))")
    lines.append(f"{indent}   {src_n} = size({src_name})")
    if pad_name is not None:
        lines.append(f"{indent}   {pad_n} = size({pad_name})")
        lines.append(f"{indent}   if ({pad_n} <= 0) stop 'xto_loop: reshape pad has zero size'")
    lines.append(f"{indent}   {pos} = 1")
    # Fortran array element order: first index varies fastest.
    for level, d0 in enumerate(reversed(range(vl.rank))):
        if is_alloc:
            lb, ub = "1", shape_items[d0]
        else:
            lb, ub = resolve_dim_bounds(lhs_name.lower(), d0, vl.bounds[d0])
        lines.append(f"{indent}{' ' * (3 * (level + 1))}do {idx[d0]} = {lb}, {ub}")
    inner = 3 * (vl.rank + 1)
    if pad_name is None:
        lines.append(f"{indent}{' ' * inner}{lhs_name}({', '.join(idx[:vl.rank])}) = {src_name}({pos})")
    else:
        lines.append(f"{indent}{' ' * inner}if ({pos} <= {src_n}) then")
        lines.append(f"{indent}{' ' * (inner + 3)}{lhs_name}({', '.join(idx[:vl.rank])}) = {src_name}({pos})")
        lines.append(f"{indent}{' ' * inner}else")
        lines.append(f"{indent}{' ' * (inner + 3)}{pad_k} = mod({pos} - {src_n} - 1, {pad_n}) + 1")
        lines.append(f"{indent}{' ' * (inner + 3)}{lhs_name}({', '.join(idx[:vl.rank])}) = {pad_name}({pad_k})")
        lines.append(f"{indent}{' ' * inner}end if")
    lines.append(f"{indent}{' ' * inner}{pos} = {pos} + 1")
    for level in reversed(range(vl.rank)):
        lines.append(f"{indent}{' ' * (3 * (level + 1))}end do")
    lines.append(f"{indent}end block")
    return "reshape_to_loop", "\n".join(lines)


def maybe_reverse_sumprod_mask(lhs: str, rhs: str, indent: str, vars_info: Dict[str, VarInfo]) -> Optional[Tuple[str, str]]:
    m = SUMPROD_MASK_RE.match(rhs)
    if not m:
        return None
    op = m.group(1).lower()
    expr = m.group(2).strip()
    cond = m.group(3).strip()
    lhs_name = fscan.base_identifier(lhs)
    if lhs_name is None:
        return None
    ref_names = [
        n
        for n in vars_info
        if re.search(rf"\b{re.escape(n)}\b", expr, re.IGNORECASE)
        or re.search(rf"\b{re.escape(n)}\b", cond, re.IGNORECASE)
    ]
    if not ref_names:
        return None
    ref = vars_info[ref_names[0]]
    if ref.rank < 1 or ref.rank > 3:
        return None
    idx = loop_vars_for_rank(ref.rank, vars_info)
    expr_i = index_expr_for_arrays(expr, ref.rank, idx, vars_info)
    cond_i = index_expr_for_arrays(cond, ref.rank, idx, vars_info)
    init = "0.0" if op == "sum" else "1.0"
    update_op = "+" if op == "sum" else "*"
    lines: List[str] = [f"{indent}{lhs_name} = {init}"]
    ref_name = ref_names[0]
    for level, d in enumerate(reversed(range(ref.rank))):
        b = ref.bounds[d]
        lb, ub = resolve_dim_bounds(ref_name, d, b)
        lines.append(f"{indent}{' ' * (3 * level)}do {idx[d]} = {lb}, {ub}")
    lines.append(f"{indent}{' ' * (3 * ref.rank)}if ({cond_i}) then")
    lines.append(f"{indent}{' ' * (3 * (ref.rank + 1))}{lhs_name} = {lhs_name} {update_op} {expr_i}")
    lines.append(f"{indent}{' ' * (3 * ref.rank)}end if")
    for level in reversed(range(ref.rank)):
        lines.append(f"{indent}{' ' * (3 * level)}end do")
    return ("sum_mask_to_loop" if op == "sum" else "product_mask_to_loop"), "\n".join(lines)


def maybe_reverse_minmax(lhs: str, rhs: str, indent: str, vars_info: Dict[str, VarInfo]) -> Optional[Tuple[str, str]]:
    m = MINMAX_RE.match(rhs)
    if not m:
        return None
    fn = m.group(1).lower()
    arr = fscan.base_identifier(m.group(2).strip())
    lhs_name = fscan.base_identifier(lhs)
    if arr is None or lhs_name is None:
        return None
    vi = vars_info.get(arr.lower())
    if vi is None or vi.rank < 1 or vi.rank > 3:
        return None
    idx = loop_vars_for_rank(vi.rank, vars_info)
    first_idx = ",".join(resolve_dim_bounds(arr, d, b)[0] for d, b in enumerate(vi.bounds))
    cmp = "<" if fn == "minval" else ">"
    lines: List[str] = [f"{indent}block", f"{indent}   {generated_loop_var_decl(vars_info, rank=vi.rank)}", f"{indent}   {lhs_name} = {arr}({first_idx})"]
    def _next_start(lb: str) -> str:
        t = lb.strip()
        if re.fullmatch(r"[+-]?\d+", t):
            return str(int(t) + 1)
        return f"({t}) + 1"
    for level, d in enumerate(reversed(range(vi.rank))):
        b = vi.bounds[d]
        lb, ub = resolve_dim_bounds(arr, d, b)
        if vi.rank == 1 and d == 0:
            lb = _next_start(lb)
        lines.append(f"{indent}{' ' * (3 * (level + 1))}do {idx[d]} = {lb}, {ub}")
    idx_txt = ",".join(idx[: vi.rank])
    lines.append(f"{indent}{' ' * (3 * (vi.rank + 1))}if ({arr}({idx_txt}) {cmp} {lhs_name}) {lhs_name} = {arr}({idx_txt})")
    for level in reversed(range(vi.rank)):
        lines.append(f"{indent}{' ' * (3 * (level + 1))}end do")
    lines.append(f"{indent}end block")
    return f"{fn}_to_loop", "\n".join(lines)


def maybe_reverse_spread(lhs: str, rhs: str, indent: str, vars_info: Dict[str, VarInfo]) -> Optional[Tuple[str, str]]:
    lhs_name = fscan.base_identifier(lhs)
    if lhs_name is None:
        return None
    vi_lhs = vars_info.get(lhs_name.lower())
    if vi_lhs is None or vi_lhs.rank != 2:
        return None
    i, j = loop_vars_for_rank(2, vars_info)[:2]
    lb1, ub1 = split_range(vi_lhs.bounds[0])
    lb2, ub2 = split_range(vi_lhs.bounds[1])

    def spread_elem_from_match(mm: re.Match[str]) -> Optional[str]:
        arg = fscan.base_identifier(mm.group(1).strip())
        if arg is None:
            return None
        dim = int(mm.group(2).strip())
        return f"{arg}({i})" if dim == 2 else f"{arg}({j})"

    def indexed_elem(expr: str) -> Optional[str]:
        e = expr.strip()
        if re.fullmatch(r"[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[edED][+-]?\d+)?(?:_[a-z][a-z0-9_]*)?", e, re.IGNORECASE):
            return e
        b = fscan.base_identifier(e)
        if b is None:
            return None
        vi = vars_info.get(b.lower())
        if vi is None:
            return e
        if vi.rank == 0:
            return b
        if vi.rank == 2:
            return f"{b}({i},{j})"
        return None

    m = SPREAD_RE.match(rhs)
    if m:
        rhs_elem = spread_elem_from_match(m)
        if rhs_elem is None:
            return None
        lines = [
            f"{indent}do {j} = {lb2}, {ub2}",
            f"{indent}   do {i} = {lb1}, {ub1}",
            f"{indent}      {lhs_name}({i},{j}) = {rhs_elem}",
            f"{indent}   end do",
            f"{indent}end do",
        ]
        return "spread_to_loop", "\n".join(lines)

    m_bin = re.match(r"^\s*(.+?)\s*([+\-*/])\s*spread\s*\(\s*(.+)\s*,\s*dim\s*=\s*([12])\s*,\s*ncopies\s*=\s*(.+)\)\s*$", rhs, re.IGNORECASE)
    if m_bin:
        left = indexed_elem(m_bin.group(1))
        op = m_bin.group(2).strip()
        m_sp = re.match(r"^\s*(.+)\s*,\s*([12])\s*,\s*(.+)\s*$", f"{m_bin.group(3)}, {m_bin.group(4)}, {m_bin.group(5)}")
        # Rebuild a spread-like match object by regex for helper.
        m_spread = SPREAD_RE.match(f"spread({m_bin.group(3)}, dim={m_bin.group(4)}, ncopies={m_bin.group(5)})")
        right = spread_elem_from_match(m_spread) if m_spread is not None else None
        if left is None or right is None:
            return None
        lines = [
            f"{indent}do {j} = {lb2}, {ub2}",
            f"{indent}   do {i} = {lb1}, {ub1}",
            f"{indent}      {lhs_name}({i},{j}) = {left} {op} {right}",
            f"{indent}   end do",
            f"{indent}end do",
        ]
        return "spread_expr_to_loop", "\n".join(lines)

    m_bin2 = re.match(r"^\s*spread\s*\(\s*(.+)\s*,\s*dim\s*=\s*([12])\s*,\s*ncopies\s*=\s*(.+)\)\s*([+\-*/])\s*(.+)\s*$", rhs, re.IGNORECASE)
    if m_bin2:
        m_spread = SPREAD_RE.match(f"spread({m_bin2.group(1)}, dim={m_bin2.group(2)}, ncopies={m_bin2.group(3)})")
        left = spread_elem_from_match(m_spread) if m_spread is not None else None
        op = m_bin2.group(4).strip()
        right = indexed_elem(m_bin2.group(5))
        if left is None or right is None:
            return None
        lines = [
            f"{indent}do {j} = {lb2}, {ub2}",
            f"{indent}   do {i} = {lb1}, {ub1}",
            f"{indent}      {lhs_name}({i},{j}) = {left} {op} {right}",
            f"{indent}   end do",
            f"{indent}end do",
        ]
        return "spread_expr_to_loop", "\n".join(lines)

    return None


def maybe_reverse_pack(lhs: str, rhs: str, indent: str, vars_info: Dict[str, VarInfo]) -> Optional[Tuple[str, str]]:
    pp = parse_pack_args(rhs)
    if pp is None:
        return None
    lhs_name = fscan.base_identifier(lhs)
    if lhs_name is None:
        return None
    vl = vars_info.get(lhs_name.lower())
    if vl is None or vl.rank != 1:
        return None
    arr_expr, mask_expr, vec_expr = pp
    arr_base = fscan.base_identifier(arr_expr)
    if arr_base is None:
        return None
    va = vars_info.get(arr_base.lower())
    if va is None or va.rank < 1 or va.rank > 3:
        return None

    # Build indexed array-element expression for ARRAY argument.
    idx = loop_vars_for_rank(va.rank, vars_info)
    arr_elem = None
    pa = parse_name_and_args(arr_expr)
    if pa is None:
        # Simple array variable reference.
        arr_elem = f"{arr_base}({', '.join(idx[:va.rank])})"
    else:
        nm, args = pa
        if nm.lower() != arr_base.lower() or len(args) != va.rank:
            return None
        subs: List[str] = []
        for d, a in enumerate(args):
            aa = a.strip()
            if aa == ":":
                subs.append(idx[d])
            elif ":" in aa:
                return None
            else:
                subs.append(aa)
        arr_elem = f"{arr_base}({', '.join(subs)})"

    mask_idx = index_expr_for_arrays(mask_expr, va.rank, idx, vars_info)
    lines: List[str] = [f"{indent}block", f"{indent}   {generated_loop_var_decl(vars_info, rank=va.rank)}", f"{indent}   integer :: n_pack, k"]
    lines.append(f"{indent}   n_pack = 0")
    for level, d in enumerate(reversed(range(va.rank))):
        lb, ub = resolve_dim_bounds(arr_base.lower(), d, va.bounds[d])
        lines.append(f"{indent}{' ' * (3 * (level + 1))}do {idx[d]} = {lb}, {ub}")
    lines.append(f"{indent}{' ' * (3 * (va.rank + 1))}if ({mask_idx}) n_pack = n_pack + 1")
    for level in reversed(range(va.rank)):
        lines.append(f"{indent}{' ' * (3 * (level + 1))}end do")

    lhs_is_deferred = any(b.strip() == ":" for b in vl.bounds)
    if vec_expr is not None:
        vec_base = fscan.base_identifier(vec_expr)
        vv = vars_info.get(vec_base.lower()) if vec_base is not None else None
        if vec_base is None or vv is None or vv.rank != 1:
            return None
        if lhs_is_deferred:
            lines.append(f"{indent}   if (allocated({lhs_name})) deallocate({lhs_name})")
            lines.append(f"{indent}   allocate({lhs_name}(size({vec_base})))")
        # Initialize from VECTOR then overwrite first packed segment.
        lines.append(f"{indent}   {lhs_name} = {vec_base}")
        lines.append(f"{indent}   k = lbound({lhs_name}, 1)")
    else:
        if lhs_is_deferred:
            lines.append(f"{indent}   if (allocated({lhs_name})) deallocate({lhs_name})")
            lines.append(f"{indent}   allocate({lhs_name}(n_pack))")
            lines.append(f"{indent}   k = lbound({lhs_name}, 1)")
        else:
            # Conservative: without VECTOR, fixed-size LHS may not be conformable.
            return None

    for level, d in enumerate(reversed(range(va.rank))):
        lb, ub = resolve_dim_bounds(arr_base.lower(), d, va.bounds[d])
        lines.append(f"{indent}{' ' * (3 * (level + 1))}do {idx[d]} = {lb}, {ub}")
    lines.append(f"{indent}{' ' * (3 * (va.rank + 1))}if ({mask_idx}) then")
    lines.append(f"{indent}{' ' * (3 * (va.rank + 2))}{lhs_name}(k) = {arr_elem}")
    lines.append(f"{indent}{' ' * (3 * (va.rank + 2))}k = k + 1")
    lines.append(f"{indent}{' ' * (3 * (va.rank + 1))}end if")
    for level in reversed(range(va.rank)):
        lines.append(f"{indent}{' ' * (3 * (level + 1))}end do")
    lines.append(f"{indent}end block")
    return "pack_to_loop", "\n".join(lines)


def maybe_reverse_pack_in_expr(
    lhs: str,
    rhs: str,
    indent: str,
    vars_info: Dict[str, VarInfo],
    type_map: Dict[str, str],
) -> Optional[Tuple[str, str]]:
    """Rewrite assignment RHS containing pack(...) into temp+loop form (conservative)."""
    if "pack(" not in rhs.lower():
        return None

    def find_pack_calls(txt: str) -> List[Tuple[int, int, str]]:
        out: List[Tuple[int, int, str]] = []
        low = txt.lower()
        pos = 0
        while True:
            i = low.find("pack(", pos)
            if i < 0:
                break
            j = i + 5
            depth = 1
            while j < len(txt) and depth > 0:
                ch = txt[j]
                if ch == "(":
                    depth += 1
                elif ch == ")":
                    depth -= 1
                j += 1
            if depth != 0:
                break
            out.append((i, j, txt[i:j]))
            pos = j
        return out

    calls = find_pack_calls(rhs)
    if not calls:
        return None

    for s, e, call_txt in calls:
        pp = parse_pack_args(call_txt)
        if pp is None:
            continue
        arr_expr, mask_expr, vec_expr = pp
        if vec_expr is not None:
            continue
        arr_base = fscan.base_identifier(arr_expr)
        if arr_base is None:
            continue
        va = vars_info.get(arr_base.lower())
        if va is None or va.rank < 1 or va.rank > 3:
            continue

        idx_pool = loop_vars_for_rank(3, vars_info)
        pa = parse_name_and_args(arr_expr)
        loop_dims: List[Tuple[int, str]] = []
        if pa is None:
            for d in range(va.rank):
                loop_dims.append((d, idx_pool[len(loop_dims)]))
            arr_elem = f"{arr_base}({', '.join(iv for _, iv in loop_dims)})"
        else:
            nm, args = pa
            if nm.lower() != arr_base.lower() or len(args) != va.rank:
                continue
            subs: List[str] = []
            ok = True
            for d, a in enumerate(args):
                aa = a.strip()
                if aa == ":":
                    iv = idx_pool[len(loop_dims)]
                    loop_dims.append((d, iv))
                    subs.append(iv)
                elif ":" in aa:
                    ok = False
                    break
                else:
                    subs.append(aa)
            if not ok or not loop_dims:
                continue
            arr_elem = f"{arr_base}({', '.join(subs)})"

        tmp = "pack_tmp1"
        idx_eff = [iv for _, iv in loop_dims]
        dim_to_iv = {d: iv for d, iv in loop_dims}

        def _indexize_sections(expr_text: str) -> str:
            out = expr_text
            for nm, vi_nm in sorted(vars_info.items(), key=lambda kv: len(kv[0]), reverse=True):
                if vi_nm.rank < 1:
                    continue
                pat = re.compile(rf"\b{re.escape(nm)}\s*\(([^()]*)\)", re.IGNORECASE)

                def repl(m: re.Match[str]) -> str:
                    args_txt = m.group(1)
                    args = split_top_level_commas(args_txt)
                    if len(args) != vi_nm.rank:
                        return m.group(0)
                    changed = False
                    for d, a in enumerate(args):
                        if d in dim_to_iv and a.strip() == ":":
                            args[d] = dim_to_iv[d]
                            changed = True
                    if not changed:
                        return m.group(0)
                    return f"{nm}({', '.join(args)})"

                out = pat.sub(repl, out)
            return out

        mask_idx = _indexize_sections(mask_expr)
        mask_idx = index_expr_for_arrays(mask_idx, len(idx_eff), idx_eff, vars_info)
        rhs_new = rhs[:s] + tmp + rhs[e:]
        arr_typ = type_map.get(arr_base.lower(), "real")

        lines: List[str] = [
            f"{indent}block",
            f"{indent}   {generated_loop_var_decl(vars_info, rank=3)}",
            f"{indent}   integer :: n_pack, k",
            f"{indent}   {arr_typ}, allocatable :: {tmp}(:)",
            f"{indent}   n_pack = 0",
        ]
        for level, (d, iv) in enumerate(reversed(loop_dims)):
            lb, ub = resolve_dim_bounds(arr_base.lower(), d, va.bounds[d])
            lines.append(f"{indent}{' ' * (3 * (level + 1))}do {iv} = {lb}, {ub}")
        lines.append(f"{indent}{' ' * (3 * (len(loop_dims) + 1))}if ({mask_idx}) n_pack = n_pack + 1")
        for level in reversed(range(len(loop_dims))):
            lines.append(f"{indent}{' ' * (3 * (level + 1))}end do")
        lines.append(f"{indent}   allocate({tmp}(n_pack))")
        lines.append(f"{indent}   k = 1")
        for level, (d, iv) in enumerate(reversed(loop_dims)):
            lb, ub = resolve_dim_bounds(arr_base.lower(), d, va.bounds[d])
            lines.append(f"{indent}{' ' * (3 * (level + 1))}do {iv} = {lb}, {ub}")
        lines.append(f"{indent}{' ' * (3 * (len(loop_dims) + 1))}if ({mask_idx}) then")
        lines.append(f"{indent}{' ' * (3 * (len(loop_dims) + 2))}{tmp}(k) = {arr_elem}")
        lines.append(f"{indent}{' ' * (3 * (len(loop_dims) + 2))}k = k + 1")
        lines.append(f"{indent}{' ' * (3 * (len(loop_dims) + 1))}end if")
        for level in reversed(range(len(loop_dims))):
            lines.append(f"{indent}{' ' * (3 * (level + 1))}end do")
        lines.append(f"{indent}   {lhs} = {rhs_new}")
        lines.append(f"{indent}   if (allocated({tmp})) deallocate({tmp})")
        lines.append(f"{indent}end block")
        return "pack_expr_to_loop", "\n".join(lines)

    return None


def maybe_reverse_constructor(code: str, indent: str, vars_info: Dict[str, VarInfo]) -> Optional[Tuple[str, str]]:
    m = CONSTRUCTOR_RE.match(code)
    if not m:
        return None
    lhs = m.group(1).lower()
    vi = vars_info.get(lhs)
    ctor_body = m.group(2).strip()
    vals = split_top_level_commas(ctor_body)
    if not vals:
        return None
    if vi is not None and vi.rank != 1:
        return None

    implied_do_re = re.compile(
        r"^\(\s*(.+)\s*,\s*([a-z][a-z0-9_]*)\s*=\s*([^,\]]+)\s*,\s*([^,\]]+)(?:\s*,\s*([^,\]]+))?\s*\)$",
        re.IGNORECASE,
    )

    def term_is_scalar_simple(term: str) -> bool:
        t = term.strip()
        if not t:
            return False
        if implied_do_re.match(t):
            return False
        mname = SIMPLE_NAME_RE.match(t)
        if mname:
            nm = mname.group(1).lower()
            v = vars_info.get(nm)
            return not (v is not None and v.rank >= 1)
        pa = parse_name_and_args(t)
        if pa is not None:
            nm, args = pa
            v = vars_info.get(nm)
            if v is None:
                return True
            if v.rank == len(args):
                # Treat sections as non-scalar.
                for a in args:
                    aa = a.strip()
                    if aa == ":" or ":" in aa:
                        return False
                return v.rank == 0
        return True

    # Idiomatic fast-path: scalar constructor terms only -> direct indexed assigns.
    if all(term_is_scalar_simple(v) for v in vals):
        lb = split_range(vi.bounds[0])[0] if (vi is not None and vi.rank == 1) else "1"
        lines: List[str] = []
        if re.fullmatch(r"[+-]?\d+", lb):
            lo = int(lb)
            for k, v in enumerate(vals):
                lines.append(f"{indent}{lhs}({lo + k}) = {v.strip()}")
        else:
            base = lb
            for k, v in enumerate(vals):
                idx = base if k == 0 else f"({base}) + {k}"
                lines.append(f"{indent}{lhs}({idx}) = {v.strip()}")
        return "constructor_to_scalar_assigns", "\n".join(lines)

    lb = split_range(vi.bounds[0])[0] if (vi is not None and vi.rank == 1) else None
    kname = loop_vars_for_rank(3, vars_info)[-1]
    body: List[str] = []
    emit_constructor_fill_rank1(
        body,
        indent=f"{indent}   ",
        target_arr=lhs,
        ctor_body=ctor_body,
        vars_info=vars_info,
        pos_name=kname,
        lb_expr=lb,
    )
    lines: List[str] = [f"{indent}block"]
    d = generated_loop_var_decl_used(body, vars_info, rank=3)
    if d:
        lines.append(f"{indent}   {d}")
    lines.extend(body)
    lines.append(f"{indent}end block")
    return "constructor_to_scalar_assigns", "\n".join(lines)


def maybe_reverse_section_constructor(lhs: str, rhs: str, indent: str) -> Optional[Tuple[str, str]]:
    """Reverse section constructor assignment: a(l:u) = [v1, v2, ...]."""
    m_lhs = re.match(r"^\s*([a-z][a-z0-9_]*)\s*\(\s*([^()]+)\s*\)\s*$", lhs, re.IGNORECASE)
    if not m_lhs:
        return None
    arr = m_lhs.group(1).lower()
    sub = m_lhs.group(2).strip()
    # Only handle single-dimension contiguous section l:u
    if "," in sub or ":" not in sub:
        return None
    lb_txt, ub_txt = [p.strip() for p in sub.split(":", 1)]
    if not lb_txt or not ub_txt:
        return None
    m_rhs = re.match(r"^\s*\[\s*(.+)\s*\]\s*$", rhs, re.IGNORECASE)
    if not m_rhs:
        return None
    vals = split_top_level_commas(m_rhs.group(1))
    if not vals:
        return None
    # For section assignments, keep scalarized direct assignments for now.
    # (Mixed array/scalar constructor terms are handled by full-array rewrite.)
    lines: List[str] = []
    if re.fullmatch(r"[+-]?\d+", lb_txt) and re.fullmatch(r"[+-]?\d+", ub_txt):
        lo = int(lb_txt)
        hi = int(ub_txt)
        if hi < lo or (hi - lo + 1) != len(vals):
            return None
        for k, v in enumerate(vals):
            lines.append(f"{indent}{arr}({lo + k}) = {v.strip()}")
        return "section_constructor_to_scalar_assigns", "\n".join(lines)

    base = lb_txt
    for k, v in enumerate(vals):
        idx = base if k == 0 else f"({base}) + {k}"
        lines.append(f"{indent}{arr}({idx}) = {v.strip()}")
    return "section_constructor_to_scalar_assigns", "\n".join(lines)


def maybe_reverse_rank2_section_constructor(
    lhs: str, rhs: str, indent: str, vars_info: Dict[str, VarInfo]
) -> Optional[Tuple[str, str]]:
    """Reverse rank-2 row/column section constructor assignment.

    Examples:
      x(i,:) = [a, b, c]
      x(:,j) = [a, b, c]
    """
    m_lhs = re.match(
        r"^\s*([a-z][a-z0-9_]*)\s*\(\s*([^(),]+)\s*,\s*([^(),]+)\s*\)\s*$",
        lhs,
        re.IGNORECASE,
    )
    if not m_lhs:
        return None
    arr = m_lhs.group(1).lower()
    a1 = m_lhs.group(2).strip()
    a2 = m_lhs.group(3).strip()
    if not ((a1 == ":" and a2 != ":") or (a2 == ":" and a1 != ":")):
        return None

    m_rhs = re.match(r"^\s*\[\s*(.+)\s*\]\s*$", rhs, re.IGNORECASE)
    if not m_rhs:
        return None
    vals = split_top_level_commas(m_rhs.group(1))
    if not vals:
        return None

    vi = vars_info.get(arr.lower())
    if vi is None or vi.rank != 2:
        return None

    if a2 == ":":
        fixed = a1
        lb, ub = split_range(vi.bounds[1])
        if re.fullmatch(r"[+-]?\d+", lb) and re.fullmatch(r"[+-]?\d+", ub):
            lo = int(lb)
            hi = int(ub)
            if hi >= lo and (hi - lo + 1) != len(vals):
                return None
        lines = []
        for k, v in enumerate(vals):
            idx2 = lb if k == 0 else f"({lb}) + {k}"
            lines.append(f"{indent}{arr}({fixed}, {idx2}) = {v.strip()}")
        return "rank2_section_constructor_to_scalar_assigns", "\n".join(lines)

    fixed = a2
    lb, ub = split_range(vi.bounds[0])
    if re.fullmatch(r"[+-]?\d+", lb) and re.fullmatch(r"[+-]?\d+", ub):
        lo = int(lb)
        hi = int(ub)
        if hi >= lo and (hi - lo + 1) != len(vals):
            return None
    lines = []
    for k, v in enumerate(vals):
        idx1 = lb if k == 0 else f"({lb}) + {k}"
        lines.append(f"{indent}{arr}({idx1}, {fixed}) = {v.strip()}")
    return "rank2_section_constructor_to_scalar_assigns", "\n".join(lines)


def maybe_reverse_section_copy(lhs: str, rhs: str, indent: str, vars_info: Dict[str, VarInfo]) -> Optional[Tuple[str, str]]:
    """Reverse simple section copy: a(l1:u1) = b(l2:u2)."""
    lp = parse_name_and_args(lhs)
    rp = parse_name_and_args(rhs)
    if lp is None or rp is None:
        return None
    lnm, largs = lp
    rnm, rargs = rp
    if len(largs) != 1 or len(rargs) != 1:
        return None
    lsec = largs[0].strip()
    rsec = rargs[0].strip()
    if ":" not in lsec or ":" not in rsec:
        return None
    # Keep contiguous sections only.
    if lsec.count(":") != 1 or rsec.count(":") != 1:
        return None
    llb, lub = split_range(lsec)
    rlb, rub = split_range(rsec)
    if llb is None or lub is None or rlb is None or rub is None:
        return None
    # Require explicit bounds on both sides; omitted bounds (for example :j2)
    # need declaration-aware defaults and are not handled here.
    if not llb.strip() or not lub.strip() or not rlb.strip() or not rub.strip():
        return None
    vi_l = vars_info.get(lnm.lower())
    vi_r = vars_info.get(rnm.lower())
    if vi_l is None or vi_r is None or vi_l.rank != 1 or vi_r.rank != 1:
        return None
    i = loop_vars_for_rank(1, vars_info)[0]
    lines = [
        f"{indent}block",
        f"{indent}   {generated_loop_var_decl(vars_info, rank=1)}",
        f"{indent}   do {i} = 0, min(({lub})-({llb}), ({rub})-({rlb}))",
        f"{indent}      {lnm}(({llb})+{i}) = {rnm}(({rlb})+{i})",
        f"{indent}   end do",
        f"{indent}end block",
    ]
    return "section_copy_to_loop", "\n".join(lines)


def maybe_reverse_vector_index_constructor(lhs: str, rhs: str, indent: str) -> Optional[Tuple[str, str]]:
    """Reverse indexed-lhs constructor assignment: a([i1,i2,...]) = [v1,v2,...]."""
    m_lhs = re.match(r"^\s*([a-z][a-z0-9_]*)\s*\(\s*\[\s*(.+)\s*\]\s*\)\s*$", lhs, re.IGNORECASE)
    if not m_lhs:
        return None
    arr = m_lhs.group(1).lower()
    idxs = split_top_level_commas(m_lhs.group(2))
    m_rhs = re.match(r"^\s*\[\s*(.+)\s*\]\s*$", rhs, re.IGNORECASE)
    if not m_rhs:
        return None
    vals = split_top_level_commas(m_rhs.group(1))
    if not idxs or len(idxs) != len(vals):
        return None
    lines = [f"{indent}{arr}({idxs[k].strip()}) = {vals[k].strip()}" for k in range(len(vals))]
    return "vector_index_constructor_to_scalar_assigns", "\n".join(lines)


def maybe_reverse_constructor_implied_do(
    code: str,
    indent: str,
    vars_info: Dict[str, VarInfo],
    *,
    need_allocate: bool = False,
) -> Optional[Tuple[str, str]]:
    """
    Reverse rank-1 implied-do constructor assignment:
      a = [(expr, i=lb,ub[,step])]
    into an explicit loop:
      do i = lb, ub[, step]
         a(i) = expr
      end do
    """
    m = CONSTRUCTOR_IMPLIED_DO_RE.match(code)
    if not m:
        return None
    lhs = m.group(1).lower()
    expr = m.group(2).strip()
    ivar = m.group(3).strip()
    lb = m.group(4).strip()
    ub = m.group(5).strip()
    st = m.group(6).strip() if m.group(6) else None
    # Do not require rank metadata here; in large multi-procedure files
    # declarations can be ambiguous across scopes. Constructor implied-do
    # assignment syntax itself implies array context.
    loop = f"do {ivar} = {lb}, {ub}" + (f", {st}" if st else "")
    lines: List[str] = []
    if need_allocate:
        if st is None:
            if lb.strip() == "1":
                lines.append(f"{indent}allocate({lhs}({ub}))")
            else:
                lines.append(f"{indent}allocate({lhs}({lb}:{ub}))")
        else:
            # Conservative fallback when step is present.
            lines.append(f"{indent}allocate({lhs}({lb}:{ub}))")
    lines.extend(
        [
            f"{indent}{loop}",
            f"{indent}   {lhs}({ivar}) = {expr}",
            f"{indent}end do",
        ]
    )
    return "constructor_implied_do_to_loop", "\n".join(lines)


def maybe_reverse_count_constructor_implied_do(lhs: str, rhs: str, indent: str) -> Optional[Tuple[str, str]]:
    """Reverse: lhs = count([(cond, i=lb,ub[,step])])."""
    m = re.match(
        r"^\s*count\s*\(\s*\[\s*\(\s*(.+)\s*,\s*([a-z][a-z0-9_]*)\s*=\s*([^,\]]+)\s*,\s*([^,\]]+)(?:\s*,\s*([^,\]]+))?\s*\)\s*\]\s*\)\s*$",
        rhs,
        re.IGNORECASE,
    )
    if not m:
        return None
    cond = m.group(1).strip()
    ivar = m.group(2).strip()
    lb = m.group(3).strip()
    ub = m.group(4).strip()
    st = m.group(5).strip() if m.group(5) else None
    do_head = f"do {ivar} = {lb}, {ub}" + (f", {st}" if st else "")
    lines = [
        f"{indent}{lhs} = 0",
        f"{indent}{do_head}",
        f"{indent}   if ({cond}) {lhs} = {lhs} + 1",
        f"{indent}end do",
    ]
    return "count_constructor_implied_do_to_loop", "\n".join(lines)


def maybe_reverse_decl_constructor(code: str, indent: str) -> Optional[Tuple[str, str]]:
    m = re.match(
        r"^\s*(.+::\s*)([a-z][a-z0-9_]*\s*\([^)]*\))\s*=\s*\[\s*(.+)\s*\]\s*$",
        code,
        re.IGNORECASE,
    )
    if not m:
        return None
    decl_prefix = m.group(1).rstrip()
    ent = m.group(2).strip()
    vals = [v.strip() for v in split_top_level_commas(m.group(3)) if v.strip()]
    if not vals:
        return None
    name = fscan.base_identifier(ent)
    if name is None:
        return None
    decl_line = f"{indent}{decl_prefix}{ent}"
    data_line = f"{indent}data {name} / {', '.join(vals)} /"
    return "decl_constructor_to_data", f"{decl_line}\n{data_line}"


SUMPROD_SIMPLE_RE = re.compile(r"^\s*(sum|product)\s*\(\s*([a-z][a-z0-9_]*)\s*\)\s*$", re.IGNORECASE)
SUMPROD_CONSTR_RE = re.compile(r"^\s*(sum|product)\s*\(\s*\[\s*(.+)\s*\]\s*\)\s*$", re.IGNORECASE)
COUNT_CONSTR_RE = re.compile(r"^\s*count\s*\(\s*\[\s*(.+)\s*\]\s*\)\s*$", re.IGNORECASE)
COUNT_SIMPLE_RE = re.compile(r"^\s*count\s*\(\s*([a-z][a-z0-9_]*)\s*\)\s*$", re.IGNORECASE)
COUNT_MASK_RE = re.compile(
    r"^\s*count\s*\(\s*([a-z][a-z0-9_]*)\s*,\s*mask\s*=\s*(.+)\)\s*$",
    re.IGNORECASE,
)
SUMPROD_MASK_RE = re.compile(
    r"^\s*(sum|product)\s*\(\s*([a-z][a-z0-9_]*)\s*,\s*mask\s*=\s*(.+)\)\s*$",
    re.IGNORECASE,
)
SUM_DIM_RE = re.compile(r"^\s*sum\s*\(\s*([a-z][a-z0-9_]*)\s*,\s*dim\s*=\s*([123])\s*\)\s*$", re.IGNORECASE)
SUM_DIM_POS_RE = re.compile(r"^\s*sum\s*\(\s*([a-z][a-z0-9_]*)\s*,\s*([123])\s*\)\s*$", re.IGNORECASE)
SUM_DIM_MASK_RE = re.compile(
    r"^\s*sum\s*\(\s*([a-z][a-z0-9_]*)\s*,\s*dim\s*=\s*([123])\s*,\s*mask\s*=\s*(.+)\)\s*$",
    re.IGNORECASE,
)
SUM_MASK_DIM_RE = re.compile(
    r"^\s*sum\s*\(\s*([a-z][a-z0-9_]*)\s*,\s*mask\s*=\s*(.+)\s*,\s*dim\s*=\s*([123])\s*\)\s*$",
    re.IGNORECASE,
)
SUM_DIM_POS_MASK_RE = re.compile(
    r"^\s*sum\s*\(\s*([a-z][a-z0-9_]*)\s*,\s*([123])\s*,\s*mask\s*=\s*(.+)\)\s*$",
    re.IGNORECASE,
)
PRODUCT_DIM_RE = re.compile(r"^\s*product\s*\(\s*([a-z][a-z0-9_]*)\s*,\s*dim\s*=\s*([123])\s*\)\s*$", re.IGNORECASE)
PRODUCT_DIM_POS_RE = re.compile(r"^\s*product\s*\(\s*([a-z][a-z0-9_]*)\s*,\s*([123])\s*\)\s*$", re.IGNORECASE)
PRODUCT_DIM_MASK_RE = re.compile(
    r"^\s*product\s*\(\s*([a-z][a-z0-9_]*)\s*,\s*dim\s*=\s*([123])\s*,\s*mask\s*=\s*(.+)\)\s*$",
    re.IGNORECASE,
)
PRODUCT_MASK_DIM_RE = re.compile(
    r"^\s*product\s*\(\s*([a-z][a-z0-9_]*)\s*,\s*mask\s*=\s*(.+)\s*,\s*dim\s*=\s*([123])\s*\)\s*$",
    re.IGNORECASE,
)
PRODUCT_DIM_POS_MASK_RE = re.compile(
    r"^\s*product\s*\(\s*([a-z][a-z0-9_]*)\s*,\s*([123])\s*,\s*mask\s*=\s*(.+)\)\s*$",
    re.IGNORECASE,
)
MINMAX_SIMPLE_RE = re.compile(r"^\s*(minval|maxval)\s*\(\s*([a-z][a-z0-9_]*)\s*\)\s*$", re.IGNORECASE)
MINMAX_SUM_DIM_RE = re.compile(
    r"^\s*(minval|maxval)\s*\(\s*sum\s*\(\s*([a-z][a-z0-9_]*)\s*,\s*dim\s*=\s*([123])(?:\s*,\s*mask\s*=\s*(.+))?\s*\)\s*\)\s*$",
    re.IGNORECASE,
)
MINMAX_SUM_DIM_POS_RE = re.compile(
    r"^\s*(minval|maxval)\s*\(\s*sum\s*\(\s*([a-z][a-z0-9_]*)\s*,\s*([123])(?:\s*,\s*mask\s*=\s*(.+))?\s*\)\s*\)\s*$",
    re.IGNORECASE,
)
MINMAX_SUM_MASK_RE = re.compile(
    r"^\s*(minval|maxval)\s*\(\s*sum\s*\(\s*([a-z][a-z0-9_]*)\s*,\s*mask\s*=\s*(.+)\s*\)\s*\)\s*$",
    re.IGNORECASE,
)
MINMAX_PRODUCT_DIM_RE = re.compile(
    r"^\s*(minval|maxval)\s*\(\s*product\s*\(\s*([a-z][a-z0-9_]*)\s*,\s*dim\s*=\s*([123])(?:\s*,\s*mask\s*=\s*(.+))?\s*\)\s*\)\s*$",
    re.IGNORECASE,
)
MINMAX_PRODUCT_DIM_POS_RE = re.compile(
    r"^\s*(minval|maxval)\s*\(\s*product\s*\(\s*([a-z][a-z0-9_]*)\s*,\s*([123])(?:\s*,\s*mask\s*=\s*(.+))?\s*\)\s*\)\s*$",
    re.IGNORECASE,
)
MINMAX_PRODUCT_MASK_RE = re.compile(
    r"^\s*(minval|maxval)\s*\(\s*product\s*\(\s*([a-z][a-z0-9_]*)\s*,\s*mask\s*=\s*(.+)\s*\)\s*\)\s*$",
    re.IGNORECASE,
)


def _parse_dim_value(txt: str) -> Optional[int]:
    t = txt.strip()
    m = re.match(r"^dim\s*=\s*([123])$", t, re.IGNORECASE)
    if m:
        return int(m.group(1))
    if re.fullmatch(r"[123]", t):
        return int(t)
    return None


def parse_norm2_like_expr(expr: str) -> Optional[Tuple[str, Optional[int]]]:
    """
    Parse norm-2 forms:
      norm2(x)
      norm2(x, dim=1) / norm2(x,1)
      sqrt(sum(x**2))
      sqrt(sum(x**2, dim=1)) / sqrt(sum(x**2,1))
    Returns (array_name, dim_or_none) when recognized.
    """
    s = expr.strip()
    pa = parse_name_and_args(s)
    if pa is not None and pa[0].lower() == "norm2":
        args = pa[1]
        if len(args) == 1:
            m = SIMPLE_NAME_RE.match(args[0].strip())
            if m:
                return m.group(1).lower(), None
            return None
        if len(args) == 2:
            m = SIMPLE_NAME_RE.match(args[0].strip())
            if not m:
                return None
            d = _parse_dim_value(args[1])
            if d is None:
                return None
            return m.group(1).lower(), d
        return None
    pa = parse_name_and_args(s)
    if pa is None or pa[0].lower() != "sqrt" or len(pa[1]) != 1:
        return None
    inner = pa[1][0].strip()
    pb = parse_name_and_args(inner)
    if pb is None or pb[0].lower() != "sum":
        return None
    sargs = pb[1]
    if len(sargs) not in (1, 2):
        return None
    m_pow = re.match(r"^\s*([a-z][a-z0-9_]*)\s*\*\*\s*2(?:\.0+)?\s*$", sargs[0].strip(), re.IGNORECASE)
    if not m_pow:
        return None
    arr = m_pow.group(1).lower()
    if len(sargs) == 1:
        return arr, None
    d = _parse_dim_value(sargs[1])
    if d is None:
        return None
    return arr, d


def init_literal_for_type(typ: str, *, one: bool = False) -> str:
    t = typ.strip().lower()
    if t.startswith("integer"):
        return "1" if one else "0"
    if t.startswith("real"):
        return "1.0" if one else "0.0"
    if t.startswith("complex"):
        return "(1.0,0.0)" if one else "(0.0,0.0)"
    return "1.0" if one else "0.0"


def indexize_mask_expr(mask: str, arr: str, idx: List[str], rank: int) -> str:
    """Conservatively rewrite whole-array mask refs like `x > 0` to indexed refs."""
    return re.sub(rf"\b{re.escape(arr)}\b", f"{arr}({', '.join(idx[:rank])})", mask)


def sanitize_name_fragment(text: str) -> str:
    t = text.lower()
    # Old-style relational operators.
    t = re.sub(r"\.ge\.", "_ge_", t, flags=re.IGNORECASE)
    t = re.sub(r"\.gt\.", "_gt_", t, flags=re.IGNORECASE)
    t = re.sub(r"\.le\.", "_le_", t, flags=re.IGNORECASE)
    t = re.sub(r"\.lt\.", "_lt_", t, flags=re.IGNORECASE)
    t = re.sub(r"\.eq\.", "_eq_", t, flags=re.IGNORECASE)
    t = re.sub(r"\.ne\.", "_ne_", t, flags=re.IGNORECASE)
    # Modern operators.
    t = t.replace(">=", "_ge_").replace("<=", "_le_").replace("==", "_eq_")
    t = t.replace("/=", "_ne_").replace("!=", "_ne_")
    t = t.replace(">", "_gt_").replace("<", "_lt_")
    t = t.replace("+", "_plus_").replace("-", "_minus_")
    t = t.replace("*", "_mul_").replace("/", "_div_")
    # Keep only Fortran identifier chars via normalization.
    t = re.sub(r"[^a-z0-9_]", "_", t)
    t = re.sub(r"_+", "_", t).strip("_")
    if not t:
        t = "tmp"
    if re.match(r"^\d", t):
        t = "t_" + t
    # Keep names reasonably short; Fortran compilers commonly support >=63.
    if len(t) > 63:
        t = t[:63].rstrip("_")
        if not t:
            t = "tmp"
    return t


def unique_temp_name(base: str, taken: set[str]) -> str:
    cand = sanitize_name_fragment(base)
    while cand.lower() in taken:
        cand = cand + "_"
        if len(cand) > 63:
            cand = cand[:63]
    taken.add(cand.lower())
    return cand


def expr_is_scalar_by_decl(expr: str, vars_info: Dict[str, VarInfo]) -> bool:
    """Best-effort scalar test using declared ranks."""
    txt = expr.strip()
    if not txt:
        return True
    m_simple = SIMPLE_NAME_RE.match(txt)
    if m_simple:
        nm = m_simple.group(1).lower()
        vi = vars_info.get(nm)
        return not (vi is not None and vi.rank >= 1)
    pa = parse_name_and_args(txt)
    if pa is not None:
        nm, args = pa
        vi = vars_info.get(nm)
        if vi is not None and vi.rank >= 1:
            if len(args) != vi.rank:
                return False
            for a in args:
                aa = a.strip()
                if ":" in aa:
                    return False
                if not expr_is_scalar_by_decl(aa, vars_info):
                    return False
            return True
        # Function call (including intrinsic/merge): scalar if all args scalar.
        return all(expr_is_scalar_by_decl(a, vars_info) for a in args)
    return True


def merge_needs_expansion(expr: str, vars_info: Dict[str, VarInfo]) -> bool:
    """True for MERGE(a,b,m) when any argument is non-scalar."""
    pa = parse_name_and_args(expr)
    if pa is None:
        return False
    nm, args = pa
    if nm.lower() != "merge" or len(args) != 3:
        return False
    return any(not expr_is_scalar_by_decl(a, vars_info) for a in args)


def emit_sumprod_constructor_terms(
    lines: List[str],
    *,
    indent: str,
    target: str,
    op: str,
    ctor_body: str,
    vars_info: Dict[str, VarInfo],
) -> None:
    sym = "*" if op == "product" else "+"
    idx_pool = loop_vars_for_rank(3, vars_info)

    def emit_scalar(expr: str) -> None:
        lines.append(f"{indent}{target} = {target} {sym} {expr}")

    def emit_array_term(expr: str) -> bool:
        m = SIMPLE_NAME_RE.match(expr)
        if m:
            nm = m.group(1).lower()
            vi = vars_info.get(nm)
            if vi is None or vi.rank < 1 or vi.rank > 3:
                return False
            loop_dims: List[Tuple[int, str, str, str]] = []
            sub: List[str] = []
            for d, b in enumerate(vi.bounds):
                iv = idx_pool[len(loop_dims)]
                lb, ub = resolve_dim_bounds(nm, d, b)
                loop_dims.append((d, iv, lb, ub))
                sub.append(iv)
            elem = f"{nm}({', '.join(sub)})"
        else:
            pa = parse_name_and_args(expr)
            if pa is None:
                return False
            nm, args = pa
            vi = vars_info.get(nm)
            if vi is None or vi.rank != len(args) or vi.rank < 1 or vi.rank > 3:
                return False
            loop_dims = []
            sub = []
            for d, (a, b) in enumerate(zip(args, vi.bounds)):
                aa = a.strip()
                if aa == ":":
                    iv = idx_pool[len(loop_dims)]
                    lb, ub = resolve_dim_bounds(nm, d, b)
                    loop_dims.append((d, iv, lb, ub))
                    sub.append(iv)
                elif ":" in aa:
                    iv = idx_pool[len(loop_dims)]
                    lb, ub = split_range(aa)
                    loop_dims.append((d, iv, lb, ub))
                    sub.append(iv)
                else:
                    sub.append(aa)
            elem = f"{nm}({', '.join(sub)})"

        if not loop_dims:
            emit_scalar(elem)
            return True
        for level, (_d, iv, lb, ub) in enumerate(reversed(loop_dims)):
            lines.append(f"{indent}{' ' * (3 * level)}do {iv} = {lb}, {ub}")
        lines.append(f"{indent}{' ' * (3 * len(loop_dims))}{target} = {target} {sym} {elem}")
        for level in reversed(range(len(loop_dims))):
            lines.append(f"{indent}{' ' * (3 * level)}end do")
        return True

    implied_do_re = re.compile(
        r"^\(\s*(.+)\s*,\s*([a-z][a-z0-9_]*)\s*=\s*([^,\]]+)\s*,\s*([^,\]]+)(?:\s*,\s*([^,\]]+))?\s*\)$",
        re.IGNORECASE,
    )

    terms = split_top_level_commas(ctor_body)
    for term in terms:
        t = term.strip()
        if not t:
            continue
        mi = implied_do_re.match(t)
        if mi:
            expr = mi.group(1).strip()
            iv = mi.group(2).strip()
            lb = mi.group(3).strip()
            ub = mi.group(4).strip()
            st = mi.group(5).strip() if mi.group(5) else None
            head = f"do {iv} = {lb}, {ub}" + (f", {st}" if st else "")
            lines.append(f"{indent}{head}")
            lines.append(f"{indent}   {target} = {target} {sym} {expr}")
            lines.append(f"{indent}end do")
            continue
        if emit_array_term(t):
            continue
        emit_scalar(t)


def emit_constructor_fill_rank1(
    lines: List[str],
    *,
    indent: str,
    target_arr: str,
    ctor_body: str,
    vars_info: Dict[str, VarInfo],
    pos_name: str = "k",
    lb_expr: Optional[str] = None,
) -> None:
    idx_pool = loop_vars_for_rank(3, vars_info)
    pos = pos_name
    lb0 = lb_expr if (lb_expr is not None and lb_expr.strip()) else f"lbound({target_arr}, 1)"
    lines.append(f"{indent}{pos} = {lb0}")

    def emit_scalar(expr: str) -> None:
        lines.append(f"{indent}{target_arr}({pos}) = {expr}")
        lines.append(f"{indent}{pos} = {pos} + 1")

    def emit_array_term(expr: str) -> bool:
        m = SIMPLE_NAME_RE.match(expr)
        if m:
            nm = m.group(1).lower()
            vi = vars_info.get(nm)
            if vi is None or vi.rank < 1 or vi.rank > 3:
                return False
            loop_dims: List[Tuple[str, str, str]] = []
            sub: List[str] = []
            for d, b in enumerate(vi.bounds):
                iv = idx_pool[len(loop_dims)]
                lb, ub = resolve_dim_bounds(nm, d, b)
                loop_dims.append((iv, lb, ub))
                sub.append(iv)
            elem = f"{nm}({', '.join(sub)})"
        else:
            pa = parse_name_and_args(expr)
            if pa is None:
                return False
            nm, args = pa
            vi = vars_info.get(nm)
            if vi is None or vi.rank != len(args) or vi.rank < 1 or vi.rank > 3:
                return False
            loop_dims = []
            sub = []
            for d, (a, b) in enumerate(zip(args, vi.bounds)):
                aa = a.strip()
                if aa == ":":
                    iv = idx_pool[len(loop_dims)]
                    lb, ub = resolve_dim_bounds(nm, d, b)
                    loop_dims.append((iv, lb, ub))
                    sub.append(iv)
                elif ":" in aa:
                    iv = idx_pool[len(loop_dims)]
                    lb, ub = split_range(aa)
                    loop_dims.append((iv, lb, ub))
                    sub.append(iv)
                else:
                    sub.append(aa)
            elem = f"{nm}({', '.join(sub)})"
        if not loop_dims:
            emit_scalar(elem)
            return True
        for level, (iv, lb, ub) in enumerate(reversed(loop_dims)):
            lines.append(f"{indent}{' ' * (3 * level)}do {iv} = {lb}, {ub}")
        lines.append(f"{indent}{' ' * (3 * len(loop_dims))}{target_arr}({pos}) = {elem}")
        lines.append(f"{indent}{' ' * (3 * len(loop_dims))}{pos} = {pos} + 1")
        for level in reversed(range(len(loop_dims))):
            lines.append(f"{indent}{' ' * (3 * level)}end do")
        return True

    implied_do_re = re.compile(
        r"^\(\s*(.+)\s*,\s*([a-z][a-z0-9_]*)\s*=\s*([^,\]]+)\s*,\s*([^,\]]+)(?:\s*,\s*([^,\]]+))?\s*\)$",
        re.IGNORECASE,
    )
    for term in split_top_level_commas(ctor_body):
        t = term.strip()
        if not t:
            continue
        mi = implied_do_re.match(t)
        if mi:
            expr = mi.group(1).strip()
            iv = mi.group(2).strip()
            lb = mi.group(3).strip()
            ub = mi.group(4).strip()
            st = mi.group(5).strip() if mi.group(5) else None
            head = f"do {iv} = {lb}, {ub}" + (f", {st}" if st else "")
            lines.append(f"{indent}{head}")
            lines.append(f"{indent}   {target_arr}({pos}) = {expr}")
            lines.append(f"{indent}   {pos} = {pos} + 1")
            lines.append(f"{indent}end do")
            continue
        if emit_array_term(t):
            continue
        emit_scalar(t)


def emit_count_constructor_terms(
    lines: List[str],
    *,
    indent: str,
    target: str,
    ctor_body: str,
    vars_info: Dict[str, VarInfo],
) -> None:
    idx_pool = loop_vars_for_rank(3, vars_info)

    def emit_scalar(expr: str) -> None:
        lines.append(f"{indent}if ({expr}) {target} = {target} + 1")

    def emit_logical_array_term(expr: str) -> bool:
        m = SIMPLE_NAME_RE.match(expr)
        if m:
            nm = m.group(1).lower()
            vi = vars_info.get(nm)
            if vi is None or vi.rank < 1 or vi.rank > 3:
                return False
            loop_dims: List[Tuple[str, str, str]] = []
            sub: List[str] = []
            for d, b in enumerate(vi.bounds):
                iv = idx_pool[len(loop_dims)]
                lb, ub = resolve_dim_bounds(nm, d, b)
                loop_dims.append((iv, lb, ub))
                sub.append(iv)
            elem = f"{nm}({', '.join(sub)})"
        else:
            pa = parse_name_and_args(expr)
            if pa is None:
                return False
            nm, args = pa
            vi = vars_info.get(nm)
            if vi is None or vi.rank != len(args) or vi.rank < 1 or vi.rank > 3:
                return False
            loop_dims = []
            sub = []
            for d, (a, b) in enumerate(zip(args, vi.bounds)):
                aa = a.strip()
                if aa == ":":
                    iv = idx_pool[len(loop_dims)]
                    lb, ub = resolve_dim_bounds(nm, d, b)
                    loop_dims.append((iv, lb, ub))
                    sub.append(iv)
                elif ":" in aa:
                    iv = idx_pool[len(loop_dims)]
                    lb, ub = split_range(aa)
                    loop_dims.append((iv, lb, ub))
                    sub.append(iv)
                else:
                    sub.append(aa)
            elem = f"{nm}({', '.join(sub)})"
        if not loop_dims:
            emit_scalar(elem)
            return True
        for level, (iv, lb, ub) in enumerate(reversed(loop_dims)):
            lines.append(f"{indent}{' ' * (3 * level)}do {iv} = {lb}, {ub}")
        lines.append(f"{indent}{' ' * (3 * len(loop_dims))}if ({elem}) {target} = {target} + 1")
        for level in reversed(range(len(loop_dims))):
            lines.append(f"{indent}{' ' * (3 * level)}end do")
        return True

    implied_do_re = re.compile(
        r"^\(\s*(.+)\s*,\s*([a-z][a-z0-9_]*)\s*=\s*([^,\]]+)\s*,\s*([^,\]]+)(?:\s*,\s*([^,\]]+))?\s*\)$",
        re.IGNORECASE,
    )
    for term in split_top_level_commas(ctor_body):
        t = term.strip()
        if not t:
            continue
        mi = implied_do_re.match(t)
        if mi:
            cond = mi.group(1).strip()
            iv = mi.group(2).strip()
            lb = mi.group(3).strip()
            ub = mi.group(4).strip()
            st = mi.group(5).strip() if mi.group(5) else None
            head = f"do {iv} = {lb}, {ub}" + (f", {st}" if st else "")
            lines.append(f"{indent}{head}")
            lines.append(f"{indent}   if ({cond}) {target} = {target} + 1")
            lines.append(f"{indent}end do")
            continue
        if emit_logical_array_term(t):
            continue
        emit_scalar(t)


def maybe_reverse_sum_assign(lhs: str, rhs: str, indent: str, vars_info: Dict[str, VarInfo], type_map: Dict[str, str]) -> Optional[Tuple[str, str]]:
    mcount = COUNT_CONSTR_RE.match(rhs)
    if mcount:
        body = [f"{indent}   {lhs} = 0"]
        emit_count_constructor_terms(
            body,
            indent=f"{indent}   ",
            target=lhs,
            ctor_body=mcount.group(1).strip(),
            vars_info=vars_info,
        )
        lines = [f"{indent}block"]
        d = generated_loop_var_decl_used(body, vars_info, rank=3)
        if d:
            lines.append(f"{indent}   {d}")
        lines.extend(body)
        lines.append(f"{indent}end block")
        return "count_constructor_to_loop", "\n".join(lines)

    mctor = SUMPROD_CONSTR_RE.match(rhs)
    if mctor:
        op = mctor.group(1).lower()
        init = init_literal_for_type(type_map.get(lhs.lower(), "real"), one=(op == "product"))
        body = [f"{indent}   {lhs} = {init}"]
        emit_sumprod_constructor_terms(
            body,
            indent=f"{indent}   ",
            target=lhs,
            op=op,
            ctor_body=mctor.group(2).strip(),
            vars_info=vars_info,
        )
        lines = [f"{indent}block"]
        d = generated_loop_var_decl_used(body, vars_info, rank=3)
        if d:
            lines.append(f"{indent}   {d}")
        lines.extend(body)
        lines.append(f"{indent}end block")
        return "sum_product_constructor_to_loop", "\n".join(lines)

    mcount_simple = COUNT_SIMPLE_RE.match(rhs)
    if mcount_simple:
        arr = mcount_simple.group(1).lower()
        vi = vars_info.get(arr)
        if vi is None or vi.rank < 1 or vi.rank > 3:
            return None
        idx = loop_vars_for_rank(vi.rank, vars_info)
        lines = [f"{indent}block", f"{indent}   {generated_loop_var_decl(vars_info, rank=vi.rank)}", f"{indent}   {lhs} = 0"]
        for level, d in enumerate(reversed(range(vi.rank))):
            b = vi.bounds[d]
            lb, ub = resolve_dim_bounds(arr, d, b)
            lines.append(f"{indent}{' ' * (3 * (level + 1))}do {idx[d]} = {lb}, {ub}")
        lines.append(f"{indent}{' ' * (3 * (vi.rank + 1))}if ({arr}({', '.join(idx[:vi.rank])})) {lhs} = {lhs} + 1")
        for level in reversed(range(vi.rank)):
            lines.append(f"{indent}{' ' * (3 * (level + 1))}end do")
        lines.append(f"{indent}end block")
        return "count_to_loop", "\n".join(lines)

    mcount_mask = COUNT_MASK_RE.match(rhs)
    if mcount_mask:
        arr = mcount_mask.group(1).lower()
        mask = mcount_mask.group(2).strip()
        vi = vars_info.get(arr)
        if vi is None or vi.rank < 1 or vi.rank > 3:
            return None
        idx = loop_vars_for_rank(vi.rank, vars_info)
        mask_idx = indexize_mask_expr(mask, arr, idx, vi.rank)
        lines = [f"{indent}block", f"{indent}   {generated_loop_var_decl(vars_info, rank=vi.rank)}", f"{indent}   {lhs} = 0"]
        for level, d in enumerate(reversed(range(vi.rank))):
            b = vi.bounds[d]
            lb, ub = resolve_dim_bounds(arr, d, b)
            lines.append(f"{indent}{' ' * (3 * (level + 1))}do {idx[d]} = {lb}, {ub}")
        lines.append(
            f"{indent}{' ' * (3 * (vi.rank + 1))}if ({arr}({', '.join(idx[:vi.rank])}) .and. ({mask_idx})) {lhs} = {lhs} + 1"
        )
        for level in reversed(range(vi.rank)):
            lines.append(f"{indent}{' ' * (3 * (level + 1))}end do")
        lines.append(f"{indent}end block")
        return "count_mask_to_loop", "\n".join(lines)

    norm2_info = parse_norm2_like_expr(rhs)
    if norm2_info is not None:
        arr, dim = norm2_info
        vi = vars_info.get(arr)
        if vi is None or vi.rank < 1 or vi.rank > 3:
            return None
        vl = vars_info.get(lhs.lower())
        if dim is None:
            if vl is not None and vl.rank != 0:
                return None
            idx = loop_vars_for_rank(vi.rank, vars_info)
            lines = [f"{indent}block", f"{indent}   {generated_loop_var_decl(vars_info, rank=vi.rank)}"]
            lines.append(f"{indent}   {lhs} = 0.0")
            for level, d in enumerate(reversed(range(vi.rank))):
                lb, ub = resolve_dim_bounds(arr, d, vi.bounds[d])
                lines.append(f"{indent}{' ' * (3 * (level + 1))}do {idx[d]} = {lb}, {ub}")
            lines.append(f"{indent}{' ' * (3 * (vi.rank + 1))}{lhs} = {lhs} + {arr}({', '.join(idx[:vi.rank])})**2")
            for level in reversed(range(vi.rank)):
                lines.append(f"{indent}{' ' * (3 * (level + 1))}end do")
            lines.append(f"{indent}   {lhs} = sqrt({lhs})")
            lines.append(f"{indent}end block")
            return "norm2_to_loop", "\n".join(lines)
        if dim < 1 or dim > vi.rank or vi.rank < 2:
            return None
        red = dim - 1
        other_dims = [d for d in range(vi.rank) if d != red]
        if vl is None or vl.rank != len(other_dims):
            return None
        idx = loop_vars_for_rank(3, vars_info)
        lines = [f"{indent}block", f"{indent}   {generated_loop_var_decl(vars_info, rank=vi.rank)}"]
        for level, d in enumerate(reversed(other_dims)):
            lb, ub = resolve_dim_bounds(arr, d, vi.bounds[d])
            lines.append(f"{indent}{' ' * (3 * (level + 1))}do {idx[d]} = {lb}, {ub}")
        lhs_ref = f"{lhs}({', '.join(idx[d] for d in other_dims)})"
        arr_ref = f"{arr}({', '.join(idx[d] for d in range(vi.rank))})"
        inner_base = 3 * (len(other_dims) + 1)
        lb_r, ub_r = resolve_dim_bounds(arr, red, vi.bounds[red])
        lines.append(f"{indent}{' ' * inner_base}{lhs_ref} = 0.0")
        lines.append(f"{indent}{' ' * inner_base}do {idx[red]} = {lb_r}, {ub_r}")
        lines.append(f"{indent}{' ' * (inner_base + 3)}{lhs_ref} = {lhs_ref} + {arr_ref}**2")
        lines.append(f"{indent}{' ' * inner_base}end do")
        lines.append(f"{indent}{' ' * inner_base}{lhs_ref} = sqrt({lhs_ref})")
        for level in reversed(range(len(other_dims))):
            lines.append(f"{indent}{' ' * (3 * (level + 1))}end do")
        lines.append(f"{indent}end block")
        return "norm2_dim_to_loop", "\n".join(lines)

    def _sumprod_expr_to_loop(op: str, expr: str, tail: Optional[str] = None) -> Optional[Tuple[str, str]]:
        # Conservative: skip constructor/slice-heavy expressions that this
        # scalarizer cannot safely indexize.
        expr_strip = expr.strip()
        if "[" in expr_strip or "(/" in expr_strip or "/)" in expr_strip:
            return None
        if re.search(r"\([^)]*:[^)]*\)", expr_strip):
            return None
        ref_names = [
            n for n, vi in vars_info.items()
            if vi.rank >= 1 and vi.rank <= 3 and re.search(rf"\b{re.escape(n)}\b", expr, re.IGNORECASE)
        ]
        if not ref_names:
            return None
        ref_name = ref_names[0]
        ref = vars_info[ref_name]
        idx = loop_vars_for_rank(ref.rank, vars_info)
        expr_i = index_expr_for_arrays(expr, ref.rank, idx, vars_info)
        init = init_literal_for_type(type_map.get(lhs.lower(), "real"), one=(op == "product"))
        sym = "*" if op == "product" else "+"
        lines = [f"{indent}block", f"{indent}   {generated_loop_var_decl(vars_info, rank=ref.rank)}", f"{indent}   {lhs} = {init}"]
        for level, d in enumerate(reversed(range(ref.rank))):
            b = ref.bounds[d]
            lb, ub = resolve_dim_bounds(ref_name, d, b)
            lines.append(f"{indent}{' ' * (3 * (level + 1))}do {idx[d]} = {lb}, {ub}")
        if merge_needs_expansion(expr, vars_info):
            pa_i = parse_name_and_args(expr_i)
            if pa_i is not None and pa_i[0].lower() == "merge" and len(pa_i[1]) == 3:
                a_i, b_i, m_i = [x.strip() for x in pa_i[1]]
                lines.append(f"{indent}{' ' * (3 * (ref.rank + 1))}if ({m_i}) then")
                lines.append(f"{indent}{' ' * (3 * (ref.rank + 2))}{lhs} = {lhs} {sym} {a_i}")
                lines.append(f"{indent}{' ' * (3 * (ref.rank + 1))}else")
                lines.append(f"{indent}{' ' * (3 * (ref.rank + 2))}{lhs} = {lhs} {sym} {b_i}")
                lines.append(f"{indent}{' ' * (3 * (ref.rank + 1))}end if")
            else:
                lines.append(f"{indent}{' ' * (3 * (ref.rank + 1))}{lhs} = {lhs} {sym} {expr_i}")
        else:
            lines.append(f"{indent}{' ' * (3 * (ref.rank + 1))}{lhs} = {lhs} {sym} {expr_i}")
        for level in reversed(range(ref.rank)):
            lines.append(f"{indent}{' ' * (3 * (level + 1))}end do")
        if tail:
            lines.append(f"{indent}   {lhs} = {lhs} {tail}")
        lines.append(f"{indent}end block")
        return "sum_product_to_loop", "\n".join(lines)

    m = SUMPROD_SIMPLE_RE.match(rhs)
    if m:
        op = m.group(1).lower()
        arr = m.group(2).lower()
        vi = vars_info.get(arr)
        if vi is None or vi.rank < 1 or vi.rank > 3:
            return None
        return _sumprod_expr_to_loop(op, arr)

    # Handle scalar-tail forms with balanced parsing, e.g.:
    #   lhs = sum(arr) / n
    #   lhs = sum(ww*(xx-xmean)**2) * (nw/(nw-1.0d0))
    mhead = re.match(r"^\s*(sum|product)\s*\(", rhs, re.IGNORECASE)
    if mhead:
        op = mhead.group(1).lower()
        p0 = mhead.end() - 1  # points at '('
        depth = 0
        close_at: Optional[int] = None
        in_single = False
        in_double = False
        for i, ch in enumerate(rhs[p0:], start=p0):
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
                        close_at = i
                        break
        if close_at is not None:
            inner = rhs[p0 + 1 : close_at].strip()
            rest = rhs[close_at + 1 :].strip()
            if len(split_top_level_commas(inner)) == 1:
                if not rest:
                    inv = _sumprod_expr_to_loop(op, inner)
                    if inv is not None:
                        return inv
                elif rest.startswith("**") and rest[2:].strip():
                    tail_expr = rest[2:].strip()
                    inv = _sumprod_expr_to_loop(op, inner, tail=f"** {tail_expr}")
                    if inv is not None:
                        return inv
                elif rest[0] in "+-*/" and rest[1:].strip():
                    tail_op = rest[0]
                    tail_expr = rest[1:].strip()
                    inv = _sumprod_expr_to_loop(op, inner, tail=f"{tail_op} {tail_expr}")
                    if inv is not None:
                        return inv

    # Handle typed wrappers around simple reductions, e.g. dble(sum(ivec))/n
    mtail_wrap = re.match(
        r"^\s*([a-z][a-z0-9_]*)\s*\(\s*(sum|product)\s*\(\s*([a-z][a-z0-9_]*)\s*\)\s*\)\s*([+\-*/])\s*(.+)\s*$",
        rhs,
        re.IGNORECASE,
    )
    if mtail_wrap:
        wrap_fn = mtail_wrap.group(1).strip()
        op = mtail_wrap.group(2).lower()
        arr = mtail_wrap.group(3).strip().lower()
        tail_op = mtail_wrap.group(4).strip()
        tail_expr = mtail_wrap.group(5).strip()
        vi = vars_info.get(arr)
        if vi is not None and vi.rank >= 1 and vi.rank <= 3:
            inv = _sumprod_expr_to_loop(op, arr)
            if inv is not None:
                lines = inv[1].splitlines()
                out_lines: List[str] = []
                wrapped = False
                for ln in lines:
                    m_upd = re.search(rf"\b{re.escape(lhs)}\s*=\s*{re.escape(lhs)}\s*([+*])\s*(.+)\s*$", ln)
                    if m_upd and not wrapped:
                        rhs_term = m_upd.group(2).strip()
                        ln = re.sub(
                            rf"(=\s*{re.escape(lhs)}\s*[+*]\s*).+$",
                            rf"\1{wrap_fn}({rhs_term})",
                            ln,
                        )
                        wrapped = True
                    out_lines.append(ln)
                # apply trailing scalar op to the accumulated lhs
                if out_lines and out_lines[-1].strip().lower() == "end block":
                    out_lines.insert(len(out_lines) - 1, f"{indent}   {lhs} = {lhs} {tail_op} {tail_expr}")
                return "sum_product_to_loop", "\n".join(out_lines)

    m_sum_abs = re.match(r"^\s*sum\s*\(\s*abs\s*\(\s*([a-z][a-z0-9_]*)\s*\)\s*\)\s*$", rhs, re.IGNORECASE)
    if m_sum_abs:
        return _sumprod_expr_to_loop("sum", f"abs({m_sum_abs.group(1).strip()})")

    mmask = SUMPROD_MASK_RE.match(rhs)
    if mmask:
        op = mmask.group(1).lower()
        arr = mmask.group(2).lower()
        mask = mmask.group(3).strip()
        vi = vars_info.get(arr)
        if vi is None or vi.rank < 1 or vi.rank > 3:
            return None
        idx = loop_vars_for_rank(vi.rank, vars_info)
        init = init_literal_for_type(type_map.get(lhs.lower(), "real"), one=(op == "product"))
        sym = "*" if op == "product" else "+"
        mask_idx = indexize_mask_expr(mask, arr, idx, vi.rank)
        lines = [f"{indent}block", f"{indent}   {generated_loop_var_decl(vars_info, rank=vi.rank)}", f"{indent}   {lhs} = {init}"]
        for level, d in enumerate(reversed(range(vi.rank))):
            b = vi.bounds[d]
            lb, ub = resolve_dim_bounds(arr, d, b)
            lines.append(f"{indent}{' ' * (3 * (level + 1))}do {idx[d]} = {lb}, {ub}")
        lines.append(
            f"{indent}{' ' * (3 * (vi.rank + 1))}if ({mask_idx}) {lhs} = {lhs} {sym} {arr}({', '.join(idx[:vi.rank])})"
        )
        for level in reversed(range(vi.rank)):
            lines.append(f"{indent}{' ' * (3 * (level + 1))}end do")
        lines.append(f"{indent}end block")
        return "sum_product_mask_to_loop", "\n".join(lines)

    mdm = (
        SUM_DIM_MASK_RE.match(rhs)
        or SUM_MASK_DIM_RE.match(rhs)
        or SUM_DIM_POS_MASK_RE.match(rhs)
        or PRODUCT_DIM_MASK_RE.match(rhs)
        or PRODUCT_MASK_DIM_RE.match(rhs)
        or PRODUCT_DIM_POS_MASK_RE.match(rhs)
    )
    if mdm:
        reduce_op = "sum"
        if SUM_DIM_MASK_RE.match(rhs):
            arr = mdm.group(1).lower()
            dim = int(mdm.group(2))
            mask = mdm.group(3).strip()
        elif SUM_MASK_DIM_RE.match(rhs):
            arr = mdm.group(1).lower()
            mask = mdm.group(2).strip()
            dim = int(mdm.group(3))
        elif PRODUCT_DIM_MASK_RE.match(rhs):
            reduce_op = "product"
            arr = mdm.group(1).lower()
            dim = int(mdm.group(2))
            mask = mdm.group(3).strip()
        elif PRODUCT_MASK_DIM_RE.match(rhs):
            reduce_op = "product"
            arr = mdm.group(1).lower()
            mask = mdm.group(2).strip()
            dim = int(mdm.group(3))
        else:
            reduce_op = "product" if PRODUCT_DIM_POS_MASK_RE.match(rhs) else "sum"
            arr = mdm.group(1).lower()
            dim = int(mdm.group(2))
            mask = mdm.group(3).strip()
        vi = vars_info.get(arr)
        vl = vars_info.get(lhs.lower())
        if vi is None or vi.rank != 2 or vl is None or vl.rank != 1 or dim not in (1, 2):
            return None
        i, j = loop_vars_for_rank(2, vars_info)[:2]
        l1, u1 = resolve_dim_bounds(arr, 0, vi.bounds[0])
        l2, u2 = resolve_dim_bounds(arr, 1, vi.bounds[1])
        mask_idx = indexize_mask_expr(mask, arr, [i, j], 2)
        lines = [f"{indent}block", f"{indent}   {generated_loop_var_decl(vars_info, rank=2)}"]
        init_val = "1.0" if reduce_op == "product" else "0.0"
        acc_op = "*" if reduce_op == "product" else "+"
        if dim == 1:
            lines.append(f"{indent}   do {j} = {l2}, {u2}")
            lines.append(f"{indent}      {lhs}({j}) = {init_val}")
            lines.append(f"{indent}   end do")
            lines.append(f"{indent}   do {i} = {l1}, {u1}")
            lines.append(f"{indent}      do {j} = {l2}, {u2}")
            lines.append(f"{indent}         if ({mask_idx}) {lhs}({j}) = {lhs}({j}) {acc_op} {arr}({i},{j})")
            lines.append(f"{indent}      end do")
            lines.append(f"{indent}   end do")
        else:
            lines.append(f"{indent}   do {i} = {l1}, {u1}")
            lines.append(f"{indent}      {lhs}({i}) = {init_val}")
            lines.append(f"{indent}   end do")
            lines.append(f"{indent}   do {j} = {l2}, {u2}")
            lines.append(f"{indent}      do {i} = {l1}, {u1}")
            lines.append(f"{indent}         if ({mask_idx}) {lhs}({i}) = {lhs}({i}) {acc_op} {arr}({i},{j})")
            lines.append(f"{indent}      end do")
            lines.append(f"{indent}   end do")
        lines.append(f"{indent}end block")
        return f"{reduce_op}_dim_mask_to_loop", "\n".join(lines)

    # Handle vector-valued sqrt(sum(expr, dim=d)/den), e.g.:
    #   std_vec = sqrt(sum(centered_x**2, dim=1) / (n - 1))
    m_sqrt_dim = re.match(
        r"^\s*sqrt\s*\(\s*sum\s*\(\s*(.+)\s*,\s*dim\s*=\s*([12])\s*\)\s*/\s*(.+)\s*\)\s*$",
        rhs,
        re.IGNORECASE,
    )
    if m_sqrt_dim is None:
        m_sqrt_dim = re.match(
            r"^\s*sqrt\s*\(\s*sum\s*\(\s*(.+)\s*,\s*([12])\s*\)\s*/\s*(.+)\s*\)\s*$",
            rhs,
            re.IGNORECASE,
        )
    if m_sqrt_dim is not None:
        sum_expr = m_sqrt_dim.group(1).strip()
        dim = int(m_sqrt_dim.group(2))
        den_expr = m_sqrt_dim.group(3).strip()
        vl = vars_info.get(lhs.lower())
        if vl is None or vl.rank != 1 or dim not in (1, 2):
            return None
        ref_names = [
            n
            for n, vi in vars_info.items()
            if vi.rank == 2 and re.search(rf"\b{re.escape(n)}\b", sum_expr, re.IGNORECASE)
        ]
        if not ref_names:
            return None
        ref_name = ref_names[0]
        vref = vars_info.get(ref_name)
        if vref is None or vref.rank != 2:
            return None
        i, j = loop_vars_for_rank(2, vars_info)[:2]
        expr_ij = index_expr_for_arrays(sum_expr, 2, [i, j], vars_info)
        l1, u1 = resolve_dim_bounds(ref_name, 0, vref.bounds[0])
        l2, u2 = resolve_dim_bounds(ref_name, 1, vref.bounds[1])
        lines = [f"{indent}block", f"{indent}   {generated_loop_var_decl(vars_info, rank=2)}"]
        if dim == 1:
            lines.append(f"{indent}   do {j} = {l2}, {u2}")
            lines.append(f"{indent}      {lhs}({j}) = 0.0")
            lines.append(f"{indent}      do {i} = {l1}, {u1}")
            lines.append(f"{indent}         {lhs}({j}) = {lhs}({j}) + {expr_ij}")
            lines.append(f"{indent}      end do")
            lines.append(f"{indent}      {lhs}({j}) = sqrt({lhs}({j}) / ({den_expr}))")
            lines.append(f"{indent}   end do")
        else:
            lines.append(f"{indent}   do {i} = {l1}, {u1}")
            lines.append(f"{indent}      {lhs}({i}) = 0.0")
            lines.append(f"{indent}      do {j} = {l2}, {u2}")
            lines.append(f"{indent}         {lhs}({i}) = {lhs}({i}) + {expr_ij}")
            lines.append(f"{indent}      end do")
            lines.append(f"{indent}      {lhs}({i}) = sqrt({lhs}({i}) / ({den_expr}))")
            lines.append(f"{indent}   end do")
        lines.append(f"{indent}end block")
        return "sqrt_sum_dim_to_loop", "\n".join(lines)

    # Strict embedded SUM form: sqrt(sum(expr)/den), used in semidev/sd-like code.
    m_sqrt = re.match(r"^\s*sqrt\s*\(\s*(.+)\s*\)\s*$", rhs, re.IGNORECASE)
    if m_sqrt:
        inside = m_sqrt.group(1).strip()
        depth = 0
        in_single = False
        in_double = False
        slash_at: Optional[int] = None
        for i, ch in enumerate(inside):
            if ch == "'" and not in_double:
                in_single = not in_single
            elif ch == '"' and not in_single:
                in_double = not in_double
            elif not in_single and not in_double:
                if ch == "(":
                    depth += 1
                elif ch == ")":
                    depth -= 1
                elif ch == "/" and depth == 0:
                    slash_at = i
                    break
        if slash_at is not None:
            left = inside[:slash_at].strip()
            den_expr = inside[slash_at + 1 :].strip()
            m_sum_left = re.match(r"^\s*sum\s*\(\s*(.+)\s*\)\s*$", left, re.IGNORECASE)
            if m_sum_left is not None and den_expr:
                sum_expr = m_sum_left.group(1).strip()
                if ":" not in sum_expr and "[" not in sum_expr and "]" not in sum_expr:
                    inner_args = split_top_level_commas(sum_expr)
                    if len(inner_args) == 1:
                        sum_expr = inner_args[0].strip()
                    ref_names = [
                        n
                        for n, vi in vars_info.items()
                        if vi.rank >= 1 and vi.rank <= 3 and re.search(rf"\b{re.escape(n)}\b", sum_expr, re.IGNORECASE)
                    ]
                    if ref_names:
                        ref_name = ref_names[0]
                        ref = vars_info[ref_name]
                        idx = loop_vars_for_rank(ref.rank, vars_info)
                        expr_i = index_expr_for_arrays(sum_expr, ref.rank, idx, vars_info)
                        tmp = "sum_tmp"
                        tmp_type = type_map.get((fscan.base_identifier(lhs) or lhs).lower(), "real")
                        init = init_literal_for_type(tmp_type, one=False)
                        lines = [
                            f"{indent}block",
                            f"{indent}   {generated_loop_var_decl(vars_info, rank=3)}",
                            f"{indent}   {tmp_type} :: {tmp}",
                            f"{indent}   {tmp} = {init}",
                        ]
                        for level, d in enumerate(reversed(range(ref.rank))):
                            b = ref.bounds[d]
                            lb, ub = resolve_dim_bounds(ref_name, d, b)
                            lines.append(f"{indent}{' ' * (3 * (level + 1))}do {idx[d]} = {lb}, {ub}")
                        lines.append(f"{indent}{' ' * (3 * (ref.rank + 1))}{tmp} = {tmp} + {expr_i}")
                        for level in reversed(range(ref.rank)):
                            lines.append(f"{indent}{' ' * (3 * (level + 1))}end do")
                        lines.append(f"{indent}   {lhs} = sqrt({tmp}/{den_expr})")
                        lines.append(f"{indent}end block")
                        return "sum_expr_to_loop", "\n".join(lines)

    md = SUM_DIM_RE.match(rhs) or SUM_DIM_POS_RE.match(rhs) or PRODUCT_DIM_RE.match(rhs) or PRODUCT_DIM_POS_RE.match(rhs)
    mdt = re.match(
        r"^\s*(sum|product)\s*\(\s*([a-z][a-z0-9_]*)\s*,\s*(?:dim\s*=\s*)?([123])\s*\)\s*([+\-*/])\s*(.+)\s*$",
        rhs,
        re.IGNORECASE,
    )
    tail_op: Optional[str] = None
    tail_expr: Optional[str] = None
    if md is None and mdt is not None:
        reduce_op_t = mdt.group(1).lower()
        arr_t = mdt.group(2).lower()
        dim_t = int(mdt.group(3))
        vi_t = vars_info.get(arr_t)
        vl_t = vars_info.get(lhs.lower())
        if vi_t is not None and vi_t.rank == 2 and vl_t is not None and vl_t.rank == 1 and dim_t in (1, 2):
            # Rebuild synthetic md-like match payload via locals.
            reduce_op = reduce_op_t
            arr = arr_t
            dim = dim_t
            vi = vi_t
            vl = vl_t
            tail_op = mdt.group(4).strip()
            tail_expr = mdt.group(5).strip()
            md = mdt

    if not md:
        return None
    if mdt is None:
        reduce_op = "product" if (PRODUCT_DIM_RE.match(rhs) or PRODUCT_DIM_POS_RE.match(rhs)) else "sum"
        arr = md.group(1).lower()
        dim = int(md.group(2))
        vi = vars_info.get(arr)
        vl = vars_info.get(lhs.lower())
        if vi is None or vi.rank != 2 or vl is None or vl.rank != 1 or dim not in (1, 2):
            return None
    i, j = loop_vars_for_rank(2, vars_info)[:2]
    l1, u1 = resolve_dim_bounds(arr, 0, vi.bounds[0])
    l2, u2 = resolve_dim_bounds(arr, 1, vi.bounds[1])
    lines = [f"{indent}block", f"{indent}   {generated_loop_var_decl(vars_info, rank=2)}"]
    init_val = "1.0" if reduce_op == "product" else "0.0"
    acc_op = "*" if reduce_op == "product" else "+"
    if dim == 1:
        lines.append(f"{indent}   do {j} = {l2}, {u2}")
        lines.append(f"{indent}      {lhs}({j}) = {init_val}")
        lines.append(f"{indent}   end do")
        lines.append(f"{indent}   do {i} = {l1}, {u1}")
        lines.append(f"{indent}      do {j} = {l2}, {u2}")
        lines.append(f"{indent}         {lhs}({j}) = {lhs}({j}) {acc_op} {arr}({i},{j})")
        lines.append(f"{indent}      end do")
        lines.append(f"{indent}   end do")
    else:
        lines.append(f"{indent}   do {i} = {l1}, {u1}")
        lines.append(f"{indent}      {lhs}({i}) = {init_val}")
        lines.append(f"{indent}   end do")
        lines.append(f"{indent}   do {j} = {l2}, {u2}")
        lines.append(f"{indent}      do {i} = {l1}, {u1}")
        lines.append(f"{indent}         {lhs}({i}) = {lhs}({i}) {acc_op} {arr}({i},{j})")
        lines.append(f"{indent}      end do")
        lines.append(f"{indent}   end do")
    if tail_op is not None and tail_expr is not None:
        if dim == 1:
            lines.append(f"{indent}   do {j} = {l2}, {u2}")
            lines.append(f"{indent}      {lhs}({j}) = {lhs}({j}) {tail_op} {tail_expr}")
            lines.append(f"{indent}   end do")
        else:
            lines.append(f"{indent}   do {i} = {l1}, {u1}")
            lines.append(f"{indent}      {lhs}({i}) = {lhs}({i}) {tail_op} {tail_expr}")
            lines.append(f"{indent}   end do")
    lines.append(f"{indent}end block")
    return (
        f"{reduce_op}_dim_scalar_to_loop" if (tail_op is not None and tail_expr is not None) else f"{reduce_op}_dim_to_loop",
        "\n".join(lines),
    )


def maybe_reverse_print_reductions(code: str, indent: str, vars_info: Dict[str, VarInfo], type_map: Dict[str, str]) -> Optional[Tuple[str, str]]:
    mp = re.match(r"^\s*print\b\s*(.+)$", code, re.IGNORECASE)
    if not mp:
        return None
    parts = split_top_level_commas(mp.group(1).strip())
    if len(parts) < 2:
        return None
    decls: List[str] = []
    execs: List[str] = []
    rewritten: List[str] = []
    taken_names: set[str] = set(vars_info.keys())
    for p in parts:
        item = p.strip()
        mbr = re.match(r"^\[\s*(.+)\s*\]$", item, re.IGNORECASE)
        if mbr:
            inner_terms = [t.strip() for t in split_top_level_commas(mbr.group(1)) if t.strip()]
            if inner_terms:
                rewritten.extend(inner_terms)
                continue

        mc = COUNT_CONSTR_RE.match(item)
        if mc:
            t = unique_temp_name("count_constructor", taken_names)
            decls.append(f"{indent}   integer :: {t}")
            execs.append(f"{indent}   {t} = 0")
            emit_count_constructor_terms(
                execs,
                indent=f"{indent}   ",
                target=t,
                ctor_body=mc.group(1).strip(),
                vars_info=vars_info,
            )
            rewritten.append(t)
            continue
        mcs = COUNT_SIMPLE_RE.match(item)
        if mcs:
            arr = mcs.group(1).lower()
            vi = vars_info.get(arr)
            if vi is None or vi.rank < 1 or vi.rank > 3:
                rewritten.append(item)
                continue
            t = unique_temp_name(f"count_{arr}", taken_names)
            decls.append(f"{indent}   integer :: {t}")
            execs.append(f"{indent}   {t} = 0")
            idx = loop_vars_for_rank(vi.rank, vars_info)
            for level, d in enumerate(reversed(range(vi.rank))):
                b = vi.bounds[d]
                lb, ub = resolve_dim_bounds(arr, d, b)
                execs.append(f"{indent}{' ' * (3 * (level + 1))}do {idx[d]} = {lb}, {ub}")
            execs.append(
                f"{indent}{' ' * (3 * (vi.rank + 1))}if ({arr}({', '.join(idx[:vi.rank])})) {t} = {t} + 1"
            )
            for level in reversed(range(vi.rank)):
                execs.append(f"{indent}{' ' * (3 * (level + 1))}end do")
            rewritten.append(t)
            continue
        mcm = COUNT_MASK_RE.match(item)
        if mcm:
            arr = mcm.group(1).lower()
            mask = mcm.group(2).strip()
            vi = vars_info.get(arr)
            if vi is None or vi.rank < 1 or vi.rank > 3:
                rewritten.append(item)
                continue
            t = unique_temp_name(f"count_{arr}_mask_{mask}", taken_names)
            decls.append(f"{indent}   integer :: {t}")
            execs.append(f"{indent}   {t} = 0")
            idx = loop_vars_for_rank(vi.rank, vars_info)
            mask_idx = indexize_mask_expr(mask, arr, idx, vi.rank)
            for level, d in enumerate(reversed(range(vi.rank))):
                b = vi.bounds[d]
                lb, ub = resolve_dim_bounds(arr, d, b)
                execs.append(f"{indent}{' ' * (3 * (level + 1))}do {idx[d]} = {lb}, {ub}")
            execs.append(
                f"{indent}{' ' * (3 * (vi.rank + 1))}if ({arr}({', '.join(idx[:vi.rank])}) .and. ({mask_idx})) {t} = {t} + 1"
            )
            for level in reversed(range(vi.rank)):
                execs.append(f"{indent}{' ' * (3 * (level + 1))}end do")
            rewritten.append(t)
            continue

        norm2_info = parse_norm2_like_expr(item)
        if norm2_info is not None:
            arr, dim = norm2_info
            vi = vars_info.get(arr)
            if vi is None or vi.rank < 1 or vi.rank > 3:
                rewritten.append(item)
                continue
            typ = type_map.get(arr, "real")
            if dim is None:
                t = unique_temp_name(f"norm2_{arr}", taken_names)
                decls.append(f"{indent}   {typ} :: {t}")
                execs.append(f"{indent}   {t} = 0.0")
                idx = loop_vars_for_rank(vi.rank, vars_info)
                for level, d in enumerate(reversed(range(vi.rank))):
                    lb, ub = resolve_dim_bounds(arr, d, vi.bounds[d])
                    execs.append(f"{indent}{' ' * (3 * (level + 1))}do {idx[d]} = {lb}, {ub}")
                execs.append(f"{indent}{' ' * (3 * (vi.rank + 1))}{t} = {t} + {arr}({', '.join(idx[:vi.rank])})**2")
                for level in reversed(range(vi.rank)):
                    execs.append(f"{indent}{' ' * (3 * (level + 1))}end do")
                execs.append(f"{indent}   {t} = sqrt({t})")
                rewritten.append(t)
                continue
            if dim < 1 or dim > vi.rank or vi.rank < 2:
                rewritten.append(item)
                continue
            red = dim - 1
            other_dims = [d for d in range(vi.rank) if d != red]
            t = unique_temp_name(f"norm2_{arr}_dim_{dim}", taken_names)
            idx = loop_vars_for_rank(3, vars_info)
            decl_bounds = []
            for d in other_dims:
                lb, ub = resolve_dim_bounds(arr, d, vi.bounds[d])
                decl_bounds.append(f"{lb}:{ub}")
            decls.append(f"{indent}   {typ} :: {t}({', '.join(decl_bounds)})")
            for level, d in enumerate(reversed(other_dims)):
                lb, ub = resolve_dim_bounds(arr, d, vi.bounds[d])
                execs.append(f"{indent}{' ' * (3 * (level + 1))}do {idx[d]} = {lb}, {ub}")
            t_ref = f"{t}({', '.join(idx[d] for d in other_dims)})"
            arr_ref = f"{arr}({', '.join(idx[d] for d in range(vi.rank))})"
            inner_base = 3 * (len(other_dims) + 1)
            lb_r, ub_r = resolve_dim_bounds(arr, red, vi.bounds[red])
            execs.append(f"{indent}{' ' * inner_base}{t_ref} = 0.0")
            execs.append(f"{indent}{' ' * inner_base}do {idx[red]} = {lb_r}, {ub_r}")
            execs.append(f"{indent}{' ' * (inner_base + 3)}{t_ref} = {t_ref} + {arr_ref}**2")
            execs.append(f"{indent}{' ' * inner_base}end do")
            execs.append(f"{indent}{' ' * inner_base}{t_ref} = sqrt({t_ref})")
            for level in reversed(range(len(other_dims))):
                execs.append(f"{indent}{' ' * (3 * (level + 1))}end do")
            rewritten.append(t)
            continue

        mctor = SUMPROD_CONSTR_RE.match(item)
        if mctor:
            op = mctor.group(1).lower()
            t = unique_temp_name(f"{op}_constructor", taken_names)
            typ = "real"
            init = init_literal_for_type(typ, one=(op == "product"))
            decls.append(f"{indent}   {typ} :: {t}")
            execs.append(f"{indent}   {t} = {init}")
            emit_sumprod_constructor_terms(
                execs,
                indent=f"{indent}   ",
                target=t,
                op=op,
                ctor_body=mctor.group(2).strip(),
                vars_info=vars_info,
            )
            rewritten.append(t)
            continue

        mms = (
            MINMAX_SUM_DIM_RE.match(item)
            or MINMAX_SUM_DIM_POS_RE.match(item)
            or MINMAX_PRODUCT_DIM_RE.match(item)
            or MINMAX_PRODUCT_DIM_POS_RE.match(item)
        )
        if mms:
            reduce_op = "sum"
            if MINMAX_SUM_DIM_RE.match(item):
                fn = mms.group(1).lower()
                arr = mms.group(2).lower()
                dim = int(mms.group(3))
                mask = mms.group(4).strip() if mms.group(4) else None
            elif MINMAX_SUM_DIM_POS_RE.match(item):
                fn = mms.group(1).lower()
                arr = mms.group(2).lower()
                dim = int(mms.group(3))
                mask = mms.group(4).strip() if mms.group(4) else None
            elif MINMAX_PRODUCT_DIM_RE.match(item):
                reduce_op = "product"
                fn = mms.group(1).lower()
                arr = mms.group(2).lower()
                dim = int(mms.group(3))
                mask = mms.group(4).strip() if mms.group(4) else None
            else:
                reduce_op = "product"
                fn = mms.group(1).lower()
                arr = mms.group(2).lower()
                dim = int(mms.group(3))
                mask = mms.group(4).strip() if mms.group(4) else None
            vi = vars_info.get(arr)
            if vi is None or vi.rank not in (2, 3) or dim < 1 or dim > vi.rank:
                rewritten.append(item)
                continue
            typ = type_map.get(arr, "real")
            t = unique_temp_name(f"{fn}_{reduce_op}_{arr}_dim_{dim}" + (f"_mask_{mask}" if mask else ""), taken_names)
            s = unique_temp_name(f"{reduce_op}_{arr}_dim_{dim}_work", taken_names)
            firstv = unique_temp_name(f"{fn}_first", taken_names)
            decls.append(f"{indent}   {typ} :: {t}")
            decls.append(f"{indent}   {typ} :: {s}")
            decls.append(f"{indent}   logical :: {firstv}")
            idx = loop_vars_for_rank(vi.rank, vars_info)
            cmp = "<" if fn == "minval" else ">"
            execs.append(f"{indent}   {firstv} = .true.")
            reduce_init = "1.0" if reduce_op == "product" else "0.0"
            reduce_acc = "*" if reduce_op == "product" else "+"
            red = dim - 1
            other_dims = [d for d in range(vi.rank) if d != red]
            # Favor first-index locality among the non-reduced loops.
            other_order = list(reversed(other_dims))
            for level, d in enumerate(other_order):
                lb, ub = resolve_dim_bounds(arr, d, vi.bounds[d])
                execs.append(f"{indent}{' ' * (3 * (level + 1))}do {idx[d]} = {lb}, {ub}")
            inner_base = 3 * (len(other_order) + 1)
            execs.append(f"{indent}{' ' * inner_base}{s} = {reduce_init}")
            lb_r, ub_r = resolve_dim_bounds(arr, red, vi.bounds[red])
            execs.append(f"{indent}{' ' * inner_base}do {idx[red]} = {lb_r}, {ub_r}")
            if mask:
                mask_idx = indexize_mask_expr(mask, arr, idx, vi.rank)
                execs.append(
                    f"{indent}{' ' * (inner_base + 3)}if ({mask_idx}) {s} = {s} {reduce_acc} {arr}({', '.join(idx[:vi.rank])})"
                )
            else:
                execs.append(
                    f"{indent}{' ' * (inner_base + 3)}{s} = {s} {reduce_acc} {arr}({', '.join(idx[:vi.rank])})"
                )
            execs.append(f"{indent}{' ' * inner_base}end do")
            execs.append(f"{indent}{' ' * inner_base}if ({firstv}) then")
            execs.append(f"{indent}{' ' * (inner_base + 3)}{t} = {s}")
            execs.append(f"{indent}{' ' * (inner_base + 3)}{firstv} = .false.")
            execs.append(f"{indent}{' ' * inner_base}else")
            execs.append(f"{indent}{' ' * (inner_base + 3)}if ({s} {cmp} {t}) {t} = {s}")
            execs.append(f"{indent}{' ' * inner_base}end if")
            for level in reversed(range(len(other_order))):
                execs.append(f"{indent}{' ' * (3 * (level + 1))}end do")
            rewritten.append(t)
            continue

        msmn = MINMAX_SUM_MASK_RE.match(item) or MINMAX_PRODUCT_MASK_RE.match(item)
        if msmn:
            # SUM/PRODUCT(array, MASK=...) is scalar; MINVAL/MAXVAL expects array.
            # Do not rewrite this invalid/nonsensical nested form here.
            rewritten.append(item)
            continue

        dp = parse_dot_product_args(item)
        if dp is not None:
            i = loop_vars_for_rank(1, vars_info)[0]
            va = vector_view(dp[0], vars_info, i)
            vb = vector_view(dp[1], vars_info, i)
            if va is None or vb is None:
                rewritten.append(item)
                continue
            a_elem, lba, uba = va
            b_elem, lbb, ubb = vb
            if norm_expr(lba) != norm_expr(lbb) or norm_expr(uba) != norm_expr(ubb):
                rewritten.append(item)
                continue
            a_name = fscan.base_identifier(dp[0]) or "x"
            b_name = fscan.base_identifier(dp[1]) or "y"
            t = unique_temp_name(f"dot_product_{a_name}_{b_name}", taken_names)
            typ = type_map.get(a_name.lower(), "real")
            init = init_literal_for_type(typ)
            decls.append(f"{indent}   {typ} :: {t}")
            execs.append(f"{indent}   {t} = {init}")
            execs.append(f"{indent}   do {i} = {lba}, {uba}")
            execs.append(f"{indent}      {t} = {t} + {a_elem} * {b_elem}")
            execs.append(f"{indent}   end do")
            rewritten.append(t)
            continue

        mtp = TRANSPOSE_RE.match(item)
        if mtp:
            a = mtp.group(1).lower()
            va = vars_info.get(a)
            if va is None or va.rank != 2:
                rewritten.append(item)
                continue
            t = unique_temp_name(f"transpose_{a}", taken_names)
            typ = type_map.get(a, "real")
            i, j = loop_vars_for_rank(2, vars_info)[:2]
            lb1, ub1 = resolve_dim_bounds(a, 0, va.bounds[0])
            lb2, ub2 = resolve_dim_bounds(a, 1, va.bounds[1])
            decls.append(f"{indent}   {typ} :: {t}({lb2}:{ub2},{lb1}:{ub1})")
            execs.append(f"{indent}   do {j} = {lb1}, {ub1}")
            execs.append(f"{indent}      do {i} = {lb2}, {ub2}")
            execs.append(f"{indent}         {t}({i},{j}) = {a}({j},{i})")
            execs.append(f"{indent}      end do")
            execs.append(f"{indent}   end do")
            rewritten.append(t)
            continue

        pa_reshape = parse_name_and_args(item)
        if pa_reshape is not None and pa_reshape[0] == "reshape":
            rargs = pa_reshape[1]
            if len(rargs) == 2:
                src_expr = rargs[0].strip()
                shape_expr = rargs[1].strip()
                src_m = SIMPLE_NAME_RE.match(src_expr)
                shp_m = re.match(r"^\[\s*([^,\]]+)\s*,\s*([^\]]+)\s*\]\s*$", shape_expr)
                if src_m is not None and shp_m is not None:
                    src = src_m.group(1).lower()
                    vsrc = vars_info.get(src)
                    if vsrc is not None and vsrc.rank == 1:
                        d1 = shp_m.group(1).strip()
                        d2 = shp_m.group(2).strip()
                        t = unique_temp_name(f"reshape_{src}", taken_names)
                        typ = type_map.get(src, "real")
                        i, j, k = loop_vars_for_rank(3, vars_info)[:3]
                        decls.append(f"{indent}   {typ} :: {t}(1:{d1},1:{d2})")
                        execs.append(f"{indent}   {k} = 1")
                        execs.append(f"{indent}   do {j} = 1, {d2}")
                        execs.append(f"{indent}      do {i} = 1, {d1}")
                        execs.append(f"{indent}         {t}({i},{j}) = {src}({k})")
                        execs.append(f"{indent}         {k} = {k} + 1")
                        execs.append(f"{indent}      end do")
                        execs.append(f"{indent}   end do")
                        rewritten.append(t)
                        continue

        mm = MINMAX_SIMPLE_RE.match(item)
        if mm:
            fn = mm.group(1).lower()
            arr = mm.group(2).lower()
            vi = vars_info.get(arr)
            if vi is None or vi.rank < 1 or vi.rank > 3:
                rewritten.append(item)
                continue
            t = unique_temp_name(f"{fn}_{arr}", taken_names)
            typ = type_map.get(arr, "real")
            decls.append(f"{indent}   {typ} :: {t}")
            first = ",".join(resolve_dim_bounds(arr, d, b)[0] for d, b in enumerate(vi.bounds))
            execs.append(f"{indent}   {t} = {arr}({first})")
            idx = loop_vars_for_rank(vi.rank, vars_info)
            cmp = "<" if fn == "minval" else ">"
            def _next_start(lb: str) -> str:
                tt = lb.strip()
                if re.fullmatch(r"[+-]?\d+", tt):
                    return str(int(tt) + 1)
                return f"({tt}) + 1"
            for level, d in enumerate(reversed(range(vi.rank))):
                b = vi.bounds[d]
                lb, ub = resolve_dim_bounds(arr, d, b)
                if vi.rank == 1 and d == 0:
                    lb = _next_start(lb)
                execs.append(f"{indent}{' ' * (3 * (level+1))}do {idx[d]} = {lb}, {ub}")
            execs.append(f"{indent}{' ' * (3 * (vi.rank+1))}if ({arr}({', '.join(idx[:vi.rank])}) {cmp} {t}) {t} = {arr}({', '.join(idx[:vi.rank])})")
            for level in reversed(range(vi.rank)):
                execs.append(f"{indent}{' ' * (3 * (level+1))}end do")
            rewritten.append(t)
            continue
        ms = SUMPROD_SIMPLE_RE.match(item)
        if ms:
            op = ms.group(1).lower()
            arr = ms.group(2).lower()
            vi = vars_info.get(arr)
            if vi is None or vi.rank < 1 or vi.rank > 3:
                rewritten.append(item)
                continue
            t = unique_temp_name(f"{op}_{arr}", taken_names)
            typ = type_map.get(arr, "real")
            init = init_literal_for_type(typ, one=(op == "product"))
            sym = "*" if op == "product" else "+"
            decls.append(f"{indent}   {typ} :: {t}")
            execs.append(f"{indent}   {t} = {init}")
            idx = loop_vars_for_rank(vi.rank, vars_info)
            for level, d in enumerate(reversed(range(vi.rank))):
                b = vi.bounds[d]
                lb, ub = resolve_dim_bounds(arr, d, b)
                execs.append(f"{indent}{' ' * (3 * (level+1))}do {idx[d]} = {lb}, {ub}")
            execs.append(f"{indent}{' ' * (3 * (vi.rank+1))}{t} = {t} {sym} {arr}({', '.join(idx[:vi.rank])})")
            for level in reversed(range(vi.rank)):
                execs.append(f"{indent}{' ' * (3 * (level+1))}end do")
            rewritten.append(t)
            continue
        # General scalar reduction form:
        #   sum(expr), product(expr)
        # where expr may contain non-scalar MERGE(...), etc.
        mhead = re.match(r"^\s*(sum|product)\s*\(", item, re.IGNORECASE)
        if mhead:
            op = mhead.group(1).lower()
            p0 = mhead.end() - 1
            depth = 0
            close_at: Optional[int] = None
            in_single = False
            in_double = False
            for ii, ch in enumerate(item[p0:], start=p0):
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
                            close_at = ii
                            break
            if close_at is not None and not item[close_at + 1 :].strip():
                inner = item[p0 + 1 : close_at].strip()
                if len(split_top_level_commas(inner)) == 1 and not re.search(r"\b(dim|mask)\s*=", inner, re.IGNORECASE):
                    ref_names = [
                        n
                        for n, vi in vars_info.items()
                        if vi.rank >= 1 and vi.rank <= 3 and re.search(rf"\b{re.escape(n)}\b", inner, re.IGNORECASE)
                    ]
                    if ref_names:
                        ref_name = ref_names[0]
                        ref = vars_info[ref_name]
                        idx = loop_vars_for_rank(ref.rank, vars_info)
                        expr_i = index_expr_for_arrays(inner, ref.rank, idx, vars_info)
                        t = unique_temp_name(f"{op}_{inner}", taken_names)
                        typ = type_map.get(ref_name, "real")
                        init = init_literal_for_type(typ, one=(op == "product"))
                        sym = "*" if op == "product" else "+"
                        decls.append(f"{indent}   {typ} :: {t}")
                        execs.append(f"{indent}   {t} = {init}")
                        for level, d in enumerate(reversed(range(ref.rank))):
                            b = ref.bounds[d]
                            lb, ub = resolve_dim_bounds(ref_name, d, b)
                            execs.append(f"{indent}{' ' * (3 * (level + 1))}do {idx[d]} = {lb}, {ub}")
                        if merge_needs_expansion(inner, vars_info):
                            pa_i = parse_name_and_args(expr_i)
                            if pa_i is not None and pa_i[0].lower() == "merge" and len(pa_i[1]) == 3:
                                a_i, b_i, m_i = [x.strip() for x in pa_i[1]]
                                execs.append(f"{indent}{' ' * (3 * (ref.rank + 1))}if ({m_i}) then")
                                execs.append(f"{indent}{' ' * (3 * (ref.rank + 2))}{t} = {t} {sym} {a_i}")
                                execs.append(f"{indent}{' ' * (3 * (ref.rank + 1))}else")
                                execs.append(f"{indent}{' ' * (3 * (ref.rank + 2))}{t} = {t} {sym} {b_i}")
                                execs.append(f"{indent}{' ' * (3 * (ref.rank + 1))}end if")
                            else:
                                execs.append(f"{indent}{' ' * (3 * (ref.rank + 1))}{t} = {t} {sym} {expr_i}")
                        else:
                            execs.append(f"{indent}{' ' * (3 * (ref.rank + 1))}{t} = {t} {sym} {expr_i}")
                        for level in reversed(range(ref.rank)):
                            execs.append(f"{indent}{' ' * (3 * (level + 1))}end do")
                        rewritten.append(t)
                        continue
        msm = SUMPROD_MASK_RE.match(item)
        if msm:
            op = msm.group(1).lower()
            arr = msm.group(2).lower()
            mask = msm.group(3).strip()
            vi = vars_info.get(arr)
            if vi is None or vi.rank < 1 or vi.rank > 3:
                rewritten.append(item)
                continue
            t = unique_temp_name(f"{op}_{arr}_mask_{mask}", taken_names)
            typ = type_map.get(arr, "real")
            init = init_literal_for_type(typ, one=(op == "product"))
            sym = "*" if op == "product" else "+"
            decls.append(f"{indent}   {typ} :: {t}")
            execs.append(f"{indent}   {t} = {init}")
            idx = loop_vars_for_rank(vi.rank, vars_info)
            mask_idx = indexize_mask_expr(mask, arr, idx, vi.rank)
            for level, d in enumerate(reversed(range(vi.rank))):
                b = vi.bounds[d]
                lb, ub = resolve_dim_bounds(arr, d, b)
                execs.append(f"{indent}{' ' * (3 * (level+1))}do {idx[d]} = {lb}, {ub}")
            execs.append(
                f"{indent}{' ' * (3 * (vi.rank+1))}if ({mask_idx}) {t} = {t} {sym} {arr}({', '.join(idx[:vi.rank])})"
            )
            for level in reversed(range(vi.rank)):
                execs.append(f"{indent}{' ' * (3 * (level+1))}end do")
            rewritten.append(t)
            continue
        mscaled = re.match(
            r"^\s*(sum|product)\s*\(\s*([a-z][a-z0-9_]*)\s*\)\s*([+\-*/])\s*(.+)\s*$",
            item,
            re.IGNORECASE,
        )
        if mscaled:
            op = mscaled.group(1).lower()
            arr = mscaled.group(2).lower()
            tail_op = mscaled.group(3).strip()
            tail_expr = mscaled.group(4).strip()
            vi = vars_info.get(arr)
            if vi is not None and 1 <= vi.rank <= 3:
                t = unique_temp_name(f"{op}_{arr}", taken_names)
                typ = type_map.get(arr, "real")
                init = init_literal_for_type(typ, one=(op == "product"))
                sym = "*" if op == "product" else "+"
                decls.append(f"{indent}   {typ} :: {t}")
                execs.append(f"{indent}   {t} = {init}")
                idx = loop_vars_for_rank(vi.rank, vars_info)
                for level, d in enumerate(reversed(range(vi.rank))):
                    b = vi.bounds[d]
                    lb, ub = resolve_dim_bounds(arr, d, b)
                    execs.append(f"{indent}{' ' * (3 * (level+1))}do {idx[d]} = {lb}, {ub}")
                execs.append(f"{indent}{' ' * (3 * (vi.rank+1))}{t} = {t} {sym} {arr}({', '.join(idx[:vi.rank])})")
                for level in reversed(range(vi.rank)):
                    execs.append(f"{indent}{' ' * (3 * (level+1))}end do")
                rewritten.append(f"({t} {tail_op} {tail_expr})")
                continue

        mdt = re.match(
            r"^\s*(sum|product)\s*\(\s*([a-z][a-z0-9_]*)\s*,\s*(?:dim\s*=\s*)?([123])\s*\)\s*([+\-*/])\s*(.+)\s*$",
            item,
            re.IGNORECASE,
        )
        if mdt:
            reduce_op = mdt.group(1).lower()
            arr = mdt.group(2).lower()
            dim = int(mdt.group(3))
            tail_op = mdt.group(4).strip()
            tail_expr = mdt.group(5).strip()
            vi = vars_info.get(arr)
            if vi is not None and vi.rank in (2, 3) and 1 <= dim <= vi.rank:
                t = unique_temp_name(f"{reduce_op}_{arr}_dim_{dim}", taken_names)
                typ = type_map.get(arr, "real")
                reduce_init = init_literal_for_type(typ, one=(reduce_op == "product"))
                reduce_acc = "*" if reduce_op == "product" else "+"
                idx = loop_vars_for_rank(max(vi.rank, 3), vars_info)
                i, j, k = idx[:3]
                if vi.rank == 2:
                    l1, u1 = resolve_dim_bounds(arr, 0, vi.bounds[0])
                    l2, u2 = resolve_dim_bounds(arr, 1, vi.bounds[1])
                    if dim == 1:
                        decls.append(f"{indent}   {typ} :: {t}({l2}:{u2})")
                        execs.append(f"{indent}   do {j} = {l2}, {u2}")
                        execs.append(f"{indent}      {t}({j}) = {reduce_init}")
                        execs.append(f"{indent}   end do")
                        execs.append(f"{indent}   do {i} = {l1}, {u1}")
                        execs.append(f"{indent}      do {j} = {l2}, {u2}")
                        execs.append(f"{indent}         {t}({j}) = {t}({j}) {reduce_acc} {arr}({i},{j})")
                        execs.append(f"{indent}      end do")
                        execs.append(f"{indent}   end do")
                        execs.append(f"{indent}   do {j} = {l2}, {u2}")
                        execs.append(f"{indent}      {t}({j}) = {t}({j}) {tail_op} {tail_expr}")
                        execs.append(f"{indent}   end do")
                    else:
                        decls.append(f"{indent}   {typ} :: {t}({l1}:{u1})")
                        execs.append(f"{indent}   do {i} = {l1}, {u1}")
                        execs.append(f"{indent}      {t}({i}) = {reduce_init}")
                        execs.append(f"{indent}   end do")
                        execs.append(f"{indent}   do {j} = {l2}, {u2}")
                        execs.append(f"{indent}      do {i} = {l1}, {u1}")
                        execs.append(f"{indent}         {t}({i}) = {t}({i}) {reduce_acc} {arr}({i},{j})")
                        execs.append(f"{indent}      end do")
                        execs.append(f"{indent}   end do")
                        execs.append(f"{indent}   do {i} = {l1}, {u1}")
                        execs.append(f"{indent}      {t}({i}) = {t}({i}) {tail_op} {tail_expr}")
                        execs.append(f"{indent}   end do")
                else:
                    l1, u1 = resolve_dim_bounds(arr, 0, vi.bounds[0])
                    l2, u2 = resolve_dim_bounds(arr, 1, vi.bounds[1])
                    l3, u3 = resolve_dim_bounds(arr, 2, vi.bounds[2])
                    if dim == 1:
                        decls.append(f"{indent}   {typ} :: {t}({l2}:{u2},{l3}:{u3})")
                        execs.append(f"{indent}   do {k} = {l3}, {u3}")
                        execs.append(f"{indent}      do {j} = {l2}, {u2}")
                        execs.append(f"{indent}         {t}({j},{k}) = {reduce_init}")
                        execs.append(f"{indent}      end do")
                        execs.append(f"{indent}   end do")
                        execs.append(f"{indent}   do {k} = {l3}, {u3}")
                        execs.append(f"{indent}      do {j} = {l2}, {u2}")
                        execs.append(f"{indent}         do {i} = {l1}, {u1}")
                        execs.append(f"{indent}            {t}({j},{k}) = {t}({j},{k}) {reduce_acc} {arr}({i},{j},{k})")
                        execs.append(f"{indent}         end do")
                        execs.append(f"{indent}      end do")
                        execs.append(f"{indent}   end do")
                        execs.append(f"{indent}   do {k} = {l3}, {u3}")
                        execs.append(f"{indent}      do {j} = {l2}, {u2}")
                        execs.append(f"{indent}         {t}({j},{k}) = {t}({j},{k}) {tail_op} {tail_expr}")
                        execs.append(f"{indent}      end do")
                        execs.append(f"{indent}   end do")
                    elif dim == 2:
                        decls.append(f"{indent}   {typ} :: {t}({l1}:{u1},{l3}:{u3})")
                        execs.append(f"{indent}   do {k} = {l3}, {u3}")
                        execs.append(f"{indent}      do {i} = {l1}, {u1}")
                        execs.append(f"{indent}         {t}({i},{k}) = {reduce_init}")
                        execs.append(f"{indent}      end do")
                        execs.append(f"{indent}   end do")
                        execs.append(f"{indent}   do {k} = {l3}, {u3}")
                        execs.append(f"{indent}      do {i} = {l1}, {u1}")
                        execs.append(f"{indent}         do {j} = {l2}, {u2}")
                        execs.append(f"{indent}            {t}({i},{k}) = {t}({i},{k}) {reduce_acc} {arr}({i},{j},{k})")
                        execs.append(f"{indent}         end do")
                        execs.append(f"{indent}      end do")
                        execs.append(f"{indent}   end do")
                        execs.append(f"{indent}   do {k} = {l3}, {u3}")
                        execs.append(f"{indent}      do {i} = {l1}, {u1}")
                        execs.append(f"{indent}         {t}({i},{k}) = {t}({i},{k}) {tail_op} {tail_expr}")
                        execs.append(f"{indent}      end do")
                        execs.append(f"{indent}   end do")
                    else:
                        decls.append(f"{indent}   {typ} :: {t}({l1}:{u1},{l2}:{u2})")
                        execs.append(f"{indent}   do {j} = {l2}, {u2}")
                        execs.append(f"{indent}      do {i} = {l1}, {u1}")
                        execs.append(f"{indent}         {t}({i},{j}) = {reduce_init}")
                        execs.append(f"{indent}      end do")
                        execs.append(f"{indent}   end do")
                        execs.append(f"{indent}   do {j} = {l2}, {u2}")
                        execs.append(f"{indent}      do {i} = {l1}, {u1}")
                        execs.append(f"{indent}         do {k} = {l3}, {u3}")
                        execs.append(f"{indent}            {t}({i},{j}) = {t}({i},{j}) {reduce_acc} {arr}({i},{j},{k})")
                        execs.append(f"{indent}         end do")
                        execs.append(f"{indent}      end do")
                        execs.append(f"{indent}   end do")
                        execs.append(f"{indent}   do {j} = {l2}, {u2}")
                        execs.append(f"{indent}      do {i} = {l1}, {u1}")
                        execs.append(f"{indent}         {t}({i},{j}) = {t}({i},{j}) {tail_op} {tail_expr}")
                        execs.append(f"{indent}      end do")
                        execs.append(f"{indent}   end do")
                rewritten.append(t)
                continue

        md = SUM_DIM_RE.match(item) or SUM_DIM_POS_RE.match(item) or PRODUCT_DIM_RE.match(item) or PRODUCT_DIM_POS_RE.match(item)
        if md:
            arr = md.group(1).lower()
            dim = int(md.group(2))
            vi = vars_info.get(arr)
            if vi is None or vi.rank not in (2, 3) or dim < 1 or dim > vi.rank:
                rewritten.append(item)
                continue
            reduce_op = "product" if (PRODUCT_DIM_RE.match(item) or PRODUCT_DIM_POS_RE.match(item)) else "sum"
            t = unique_temp_name(f"{reduce_op}_{arr}_dim_{dim}", taken_names)
            typ = type_map.get(arr, "real")
            reduce_init = "1.0" if reduce_op == "product" else "0.0"
            reduce_acc = "*" if reduce_op == "product" else "+"
            i, j, k = loop_vars_for_rank(max(vi.rank, 3), vars_info)[:3]
            l1, u1 = resolve_dim_bounds(arr, 0, vi.bounds[0])
            l2, u2 = resolve_dim_bounds(arr, 1, vi.bounds[1])
            if vi.rank == 2:
                if dim == 1:
                    decls.append(f"{indent}   {typ} :: {t}({l2}:{u2})")
                    execs.append(f"{indent}   do {j} = {l2}, {u2}")
                    execs.append(f"{indent}      {t}({j}) = {reduce_init}")
                    execs.append(f"{indent}   end do")
                    execs.append(f"{indent}   do {i} = {l1}, {u1}")
                    execs.append(f"{indent}      do {j} = {l2}, {u2}")
                    execs.append(f"{indent}         {t}({j}) = {t}({j}) {reduce_acc} {arr}({i},{j})")
                    execs.append(f"{indent}      end do")
                    execs.append(f"{indent}   end do")
                else:
                    decls.append(f"{indent}   {typ} :: {t}({l1}:{u1})")
                    execs.append(f"{indent}   do {i} = {l1}, {u1}")
                    execs.append(f"{indent}      {t}({i}) = {reduce_init}")
                    execs.append(f"{indent}   end do")
                    execs.append(f"{indent}   do {j} = {l2}, {u2}")
                    execs.append(f"{indent}      do {i} = {l1}, {u1}")
                    execs.append(f"{indent}         {t}({i}) = {t}({i}) {reduce_acc} {arr}({i},{j})")
                    execs.append(f"{indent}      end do")
                    execs.append(f"{indent}   end do")
            else:
                l3, u3 = resolve_dim_bounds(arr, 2, vi.bounds[2])
                if dim == 1:
                    decls.append(f"{indent}   {typ} :: {t}({l2}:{u2},{l3}:{u3})")
                    execs.append(f"{indent}   do {k} = {l3}, {u3}")
                    execs.append(f"{indent}      do {j} = {l2}, {u2}")
                    execs.append(f"{indent}         {t}({j},{k}) = {reduce_init}")
                    execs.append(f"{indent}      end do")
                    execs.append(f"{indent}   end do")
                    execs.append(f"{indent}   do {k} = {l3}, {u3}")
                    execs.append(f"{indent}      do {j} = {l2}, {u2}")
                    execs.append(f"{indent}         do {i} = {l1}, {u1}")
                    execs.append(f"{indent}            {t}({j},{k}) = {t}({j},{k}) {reduce_acc} {arr}({i},{j},{k})")
                    execs.append(f"{indent}         end do")
                    execs.append(f"{indent}      end do")
                    execs.append(f"{indent}   end do")
                elif dim == 2:
                    decls.append(f"{indent}   {typ} :: {t}({l1}:{u1},{l3}:{u3})")
                    execs.append(f"{indent}   do {k} = {l3}, {u3}")
                    execs.append(f"{indent}      do {i} = {l1}, {u1}")
                    execs.append(f"{indent}         {t}({i},{k}) = {reduce_init}")
                    execs.append(f"{indent}      end do")
                    execs.append(f"{indent}   end do")
                    execs.append(f"{indent}   do {k} = {l3}, {u3}")
                    execs.append(f"{indent}      do {i} = {l1}, {u1}")
                    execs.append(f"{indent}         do {j} = {l2}, {u2}")
                    execs.append(f"{indent}            {t}({i},{k}) = {t}({i},{k}) {reduce_acc} {arr}({i},{j},{k})")
                    execs.append(f"{indent}         end do")
                    execs.append(f"{indent}      end do")
                    execs.append(f"{indent}   end do")
                else:
                    decls.append(f"{indent}   {typ} :: {t}({l1}:{u1},{l2}:{u2})")
                    execs.append(f"{indent}   do {j} = {l2}, {u2}")
                    execs.append(f"{indent}      do {i} = {l1}, {u1}")
                    execs.append(f"{indent}         {t}({i},{j}) = {reduce_init}")
                    execs.append(f"{indent}      end do")
                    execs.append(f"{indent}   end do")
                    execs.append(f"{indent}   do {j} = {l2}, {u2}")
                    execs.append(f"{indent}      do {i} = {l1}, {u1}")
                    execs.append(f"{indent}         do {k} = {l3}, {u3}")
                    execs.append(f"{indent}            {t}({i},{j}) = {t}({i},{j}) {reduce_acc} {arr}({i},{j},{k})")
                    execs.append(f"{indent}         end do")
                    execs.append(f"{indent}      end do")
                    execs.append(f"{indent}   end do")
            rewritten.append(t)
            continue
        mdm = (
            SUM_DIM_MASK_RE.match(item)
            or SUM_MASK_DIM_RE.match(item)
            or SUM_DIM_POS_MASK_RE.match(item)
            or PRODUCT_DIM_MASK_RE.match(item)
            or PRODUCT_MASK_DIM_RE.match(item)
            or PRODUCT_DIM_POS_MASK_RE.match(item)
        )
        if mdm:
            reduce_op = "sum"
            if SUM_DIM_MASK_RE.match(item):
                arr = mdm.group(1).lower()
                dim = int(mdm.group(2))
                mask = mdm.group(3).strip()
            elif SUM_MASK_DIM_RE.match(item):
                arr = mdm.group(1).lower()
                mask = mdm.group(2).strip()
                dim = int(mdm.group(3))
            elif PRODUCT_DIM_MASK_RE.match(item):
                reduce_op = "product"
                arr = mdm.group(1).lower()
                dim = int(mdm.group(2))
                mask = mdm.group(3).strip()
            elif PRODUCT_MASK_DIM_RE.match(item):
                reduce_op = "product"
                arr = mdm.group(1).lower()
                mask = mdm.group(2).strip()
                dim = int(mdm.group(3))
            else:
                reduce_op = "product" if PRODUCT_DIM_POS_MASK_RE.match(item) else "sum"
                arr = mdm.group(1).lower()
                dim = int(mdm.group(2))
                mask = mdm.group(3).strip()
            vi = vars_info.get(arr)
            if vi is None or vi.rank != 2 or dim not in (1, 2):
                rewritten.append(item)
                continue
            t = unique_temp_name(f"{reduce_op}_{arr}_dim_{dim}_mask_{mask}", taken_names)
            typ = type_map.get(arr, "real")
            i, j = loop_vars_for_rank(2, vars_info)[:2]
            l1, u1 = resolve_dim_bounds(arr, 0, vi.bounds[0])
            l2, u2 = resolve_dim_bounds(arr, 1, vi.bounds[1])
            mask_idx = indexize_mask_expr(mask, arr, [i, j], 2)
            reduce_init = "1.0" if reduce_op == "product" else "0.0"
            reduce_acc = "*" if reduce_op == "product" else "+"
            if dim == 1:
                decls.append(f"{indent}   {typ} :: {t}({l2}:{u2})")
                execs.append(f"{indent}   do {j} = {l2}, {u2}")
                execs.append(f"{indent}      {t}({j}) = {reduce_init}")
                execs.append(f"{indent}   end do")
                execs.append(f"{indent}   do {i} = {l1}, {u1}")
                execs.append(f"{indent}      do {j} = {l2}, {u2}")
                execs.append(f"{indent}         if ({mask_idx}) {t}({j}) = {t}({j}) {reduce_acc} {arr}({i},{j})")
                execs.append(f"{indent}      end do")
                execs.append(f"{indent}   end do")
            else:
                decls.append(f"{indent}   {typ} :: {t}({l1}:{u1})")
                execs.append(f"{indent}   do {i} = {l1}, {u1}")
                execs.append(f"{indent}      {t}({i}) = {reduce_init}")
                execs.append(f"{indent}   end do")
                execs.append(f"{indent}   do {j} = {l2}, {u2}")
                execs.append(f"{indent}      do {i} = {l1}, {u1}")
                execs.append(f"{indent}         if ({mask_idx}) {t}({i}) = {t}({i}) {reduce_acc} {arr}({i},{j})")
                execs.append(f"{indent}      end do")
                execs.append(f"{indent}   end do")
            rewritten.append(t)
            continue
        rewritten.append(item)
    if not execs and rewritten == [p.strip() for p in parts]:
        return None
    print_line = f"{indent}   print {', '.join(rewritten)}"
    body_preview = list(decls) + list(execs) + [print_line]
    lines = [f"{indent}block"]
    d = generated_loop_var_decl_used(body_preview, vars_info, rank=3)
    if d:
        lines.append(f"{indent}   {d}")
    lines.extend(decls)
    lines.extend(execs)
    lines.extend([print_line, f"{indent}end block"])
    return "print_reduction_to_loop", "\n".join(lines)


def parse_forall_controls(ctrl: str) -> Tuple[List[Tuple[str, str, str, Optional[str]]], Optional[str]]:
    """Parse FORALL control list into iterators and optional mask."""
    iters: List[Tuple[str, str, str, Optional[str]]] = []
    mask_parts: List[str] = []
    for tok in split_top_level_commas(ctrl):
        t = tok.strip()
        m = re.match(
            r"^([a-z][a-z0-9_]*)\s*=\s*([^:]+)\s*:\s*([^:]+?)(?:\s*:\s*(.+))?$",
            t,
            re.IGNORECASE,
        )
        if m:
            var = m.group(1).strip()
            lb = m.group(2).strip()
            ub = m.group(3).strip()
            st = m.group(4).strip() if m.group(4) else None
            iters.append((var, lb, ub, st))
        else:
            mask_parts.append(t)
    mask = ", ".join(mask_parts).strip() if mask_parts else None
    return iters, (mask if mask else None)


def split_forall_parts(code: str) -> Optional[Tuple[str, str]]:
    """Split FORALL line into (control, trailing_text_after_paren)."""
    m = re.match(r"^\s*forall\s*\(", code, re.IGNORECASE)
    if not m:
        return None
    i = m.end()
    depth = 1
    in_single = False
    in_double = False
    start = i
    while i < len(code):
        ch = code[i]
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
                    ctrl = code[start:i].strip()
                    rest = code[i + 1 :].strip()
                    return ctrl, rest
        i += 1
    return None


def forall_to_loops_block(
    indent: str,
    ctrl: str,
    body_lines: List[str],
) -> Optional[str]:
    iters, mask = parse_forall_controls(ctrl)
    if not iters:
        return None
    out: List[str] = []
    for d, (v, lb, ub, st) in enumerate(iters):
        head = f"do {v} = {lb}, {ub}" + (f", {st}" if st else "")
        out.append(f"{indent}{' ' * (3 * d)}{head}")
    body_indent = f"{indent}{' ' * (3 * len(iters))}"
    if mask is not None:
        out.append(f"{body_indent}if ({mask}) then")
        inner_indent = f"{indent}{' ' * (3 * (len(iters) + 1))}"
    else:
        inner_indent = body_indent
    for raw in body_lines:
        txt = raw.rstrip("\n")
        if not txt.strip():
            out.append("")
        else:
            out.append(f"{inner_indent}{txt.lstrip()}")
    if mask is not None:
        out.append(f"{body_indent}end if")
    for d in reversed(range(len(iters))):
        out.append(f"{indent}{' ' * (3 * d)}end do")
    return "\n".join(out)


def maybe_reverse_whole_assign(lhs: str, rhs: str, indent: str, vars_info: Dict[str, VarInfo]) -> Optional[Tuple[str, str]]:
    lhs_name_m = SIMPLE_NAME_RE.match(lhs)
    if not lhs_name_m:
        return None
    lhs_name = lhs_name_m.group(1).lower()
    vi = vars_info.get(lhs_name)
    if vi is None or vi.rank < 1 or vi.rank > 3:
        return None
    # Conservative: relational expressions often rely on array semantics that
    # are unsafe under scope ambiguity during scalarization.
    if re.search(r"(<=|>=|==|/=|<|>|\\.lt\\.|\\.le\\.|\\.gt\\.|\\.ge\\.|\\.eq\\.|\\.ne\\.)", rhs, re.IGNORECASE):
        return None
    # Conservative but practical: if RHS references any array name without
    # explicit indexing/section, allow only when that array rank matches LHS
    # rank (so index injection is well-defined).
    for nm, v in vars_info.items():
        if v.rank >= 1 and re.search(rf"\b{re.escape(nm)}\b(?!\s*[\(%])", rhs, re.IGNORECASE):
            if v.rank != vi.rank:
                return None
    # Conservative: section syntax on RHS often indicates rank-changing
    # operations that are not safe to scalarize with simple index injection.
    if ":" in rhs:
        return None
    rhs_low = rhs.lower()
    if any(k in rhs_low for k in ("matmul(", "sum(", "product(", "minval(", "maxval(", "spread(", "reshape(", "[")):
        return None
    # If RHS contains unknown calls, it may be a non-elemental array-valued
    # function (for example: x = acf(...)); avoid scalarizing arguments.
    for m in CALL_LIKE_RE.finditer(rhs):
        name = m.group(1).lower()
        if name in vars_info:
            continue  # variable reference with indexing/section
        if name in ELEMENTAL_INTRINSICS:
            continue
        return None
    idx = loop_vars_for_rank(vi.rank, vars_info)
    body_rhs = index_expr_for_arrays(rhs, vi.rank, idx, vars_info)
    body = f"{lhs_name}({', '.join(idx[:vi.rank])}) = {body_rhs}"
    loop_block = build_nested_loops(indent + "   ", vi.bounds, body, arr_name=lhs_name, vars_info=vars_info)
    lines = [
        f"{indent}block",
        f"{indent}   {generated_loop_var_decl(vars_info, rank=vi.rank)}",
        loop_block,
        f"{indent}end block",
    ]
    return "whole_assign_to_loop", "\n".join(lines)


def maybe_reverse_matmul_spread_special(lhs: str, rhs: str, indent: str, vars_info: Dict[str, VarInfo]) -> Optional[Tuple[str, str]]:
    m = re.match(
        r"^\s*matmul\s*\(\s*reshape\s*\(\s*([a-z][a-z0-9_]*)\s*,\s*\[\s*([^,\]]+)\s*,\s*1\s*\]\s*\)\s*,\s*reshape\s*\(\s*spread\s*\(\s*1\.0(?:_[a-z0-9_]+)?\s*,\s*dim\s*=\s*1\s*,\s*ncopies\s*=\s*([^\)]+)\)\s*,\s*\[\s*1\s*,\s*([^\]]+)\]\s*\)\s*\)\s*$",
        rhs,
        re.IGNORECASE,
    )
    if not m:
        return None
    lhs_name = fscan.base_identifier(lhs)
    if lhs_name is None:
        return None
    vi = vars_info.get(lhs_name.lower())
    if vi is None or vi.rank != 2:
        return None
    key = m.group(1).strip()
    i, j = loop_vars_for_rank(2, vars_info)[:2]
    lb1, ub1 = split_range(vi.bounds[0])
    lb2, ub2 = split_range(vi.bounds[1])
    lines = [
        f"{indent}do {i} = {lb1}, {ub1}",
        f"{indent}   do {j} = {lb2}, {ub2}",
        f"{indent}      {lhs_name}({i},{j}) = {key}({i})",
        f"{indent}   end do",
        f"{indent}end do",
    ]
    return "matmul_reshape_spread_to_loop", "\n".join(lines)


def analyze_file(path: Path) -> List[Finding]:
    text = fscan.read_text_flexible(path)
    raw_lines = text.splitlines(keepends=True)
    vars_info, allocatable_names = collect_declared_info(raw_lines)
    type_map = collect_decl_type_specs(raw_lines)

    findings: List[Finding] = []
    idx = 1
    nlines = len(raw_lines)
    while idx <= nlines:
        raw = raw_lines[idx - 1]
        code = fscan.strip_comment(raw).strip()
        if not code:
            idx += 1
            continue
        indent = re.match(r"^(\s*)", raw).group(1)
        if re.match(r"^\s*print\b", code, re.IGNORECASE):
            ptxt = re.sub(r"^\s*print\b", "", code, flags=re.IGNORECASE).strip()
            for it in split_top_level_commas(ptxt):
                token = it.strip()
                if MINMAX_SUM_MASK_RE.match(token):
                    print(
                        f"Warning: {path.name}:{idx}: skipping unsupported scalar-SUM nest `{token}`."
                    )
                if MINMAX_PRODUCT_MASK_RE.match(token):
                    print(
                        f"Warning: {path.name}:{idx}: skipping unsupported scalar-PRODUCT nest `{token}`."
                    )

        # FORALL forms.
        fparts = split_forall_parts(code)
        if fparts is not None:
            ctrl, rest = fparts
            # Inline FORALL: forall (...) stmt
            if rest:
                stmt = rest
                repl = forall_to_loops_block(indent, ctrl, [stmt])
                if repl is not None:
                    findings.append(Finding(path=path, rule="forall_to_do", start_line=idx, end_line=idx, suggestion=repl))
                    idx += 1
                    continue
            # Block FORALL header: forall (...)
            depth = 1
            j = idx + 1
            while j <= nlines:
                c = fscan.strip_comment(raw_lines[j - 1]).strip()
                cparts = split_forall_parts(c)
                if cparts is not None and cparts[1] == "":
                    depth += 1
                elif FORALL_END_RE.match(c):
                    depth -= 1
                    if depth == 0:
                        break
                j += 1
            if j <= nlines and depth == 0:
                body = raw_lines[idx:j - 1]
                repl = forall_to_loops_block(indent, ctrl, body)
                if repl is not None:
                    findings.append(Finding(path=path, rule="forall_to_do", start_line=idx, end_line=j, suggestion=repl))
                    idx = j + 1
                    continue
            # If unmatched FORALL, continue normal analysis.
            idx += 1
            continue

        # Not a FORALL line; continue with assignment and other rewrites.
        if "=>" in code:
            idx += 1
            continue

        def try_assignment_rewrite(lhs: str, rhs: str, stmt_indent: str) -> Optional[Tuple[str, str]]:
            if len(split_top_level_commas(rhs)) > 1:
                return None
            # Prefer local procedure declarations over file-global merged names.
            vars_view = dict(vars_info)
            for tok in set(re.findall(r"\b[a-z][a-z0-9_]*\b", f"{lhs} {rhs}", re.IGNORECASE)):
                lv = local_declared_var(raw_lines, idx, tok)
                if lv is not None:
                    vars_view[tok.lower()] = lv
            lhs_base = fscan.base_identifier(lhs)
            lhs_local_rank = local_declared_rank(raw_lines, idx, lhs_base) if lhs_base is not None else None
            lhs_global_rank = vars_view.get(lhs_base.lower()).rank if (lhs_base is not None and lhs_base.lower() in vars_view) else None
            for fn in (
                maybe_reverse_sum_assign,
                maybe_reverse_pack_in_expr,
                maybe_reverse_vector_index_constructor,
                maybe_reverse_rank2_section_constructor,
                maybe_reverse_section_constructor,
                maybe_reverse_section_copy,
                maybe_reverse_count_constructor_implied_do,
                maybe_reverse_matmul_spread_special,
                maybe_reverse_matmul,
                maybe_reverse_dot_product,
                maybe_reverse_transpose,
                maybe_reverse_reshape,
                maybe_reverse_sumprod_mask,
                maybe_reverse_minmax,
                maybe_reverse_spread,
                maybe_reverse_pack,
                maybe_reverse_constructor_implied_do,
                maybe_reverse_constructor,
                maybe_reverse_whole_assign,
            ):
                if fn is maybe_reverse_whole_assign and lhs_base is not None:
                    if lhs_local_rank == 0:
                        continue
                    if lhs_local_rank is not None and lhs_global_rank is not None and lhs_local_rank != lhs_global_rank:
                        continue
                if fn is maybe_reverse_constructor_implied_do:
                    lhs0 = fscan.base_identifier(lhs)
                    need_alloc = False
                    if lhs0 is not None and lhs0.lower() in allocatable_names:
                        need_alloc = not has_prior_allocate_for(lhs0.lower(), raw_lines, idx)
                    inv2 = fn(f"{lhs} = {rhs}", stmt_indent, vars_view, need_allocate=need_alloc)
                elif fn is maybe_reverse_constructor:
                    inv2 = fn(f"{lhs} = {rhs}", stmt_indent, vars_view)
                elif fn is maybe_reverse_rank2_section_constructor:
                    inv2 = fn(lhs, rhs, stmt_indent, vars_view)
                elif fn is maybe_reverse_section_constructor:
                    inv2 = fn(lhs, rhs, stmt_indent)
                elif fn is maybe_reverse_section_copy:
                    inv2 = fn(lhs, rhs, stmt_indent, vars_view)
                elif fn is maybe_reverse_sum_assign:
                    inv2 = fn(lhs, rhs, stmt_indent, vars_view, type_map)
                elif fn is maybe_reverse_pack_in_expr:
                    inv2 = fn(lhs, rhs, stmt_indent, vars_view, type_map)
                elif fn is maybe_reverse_dot_product:
                    inv2 = fn(lhs, rhs, stmt_indent, vars_view, type_map)
                elif fn is maybe_reverse_vector_index_constructor:
                    inv2 = fn(lhs, rhs, stmt_indent)
                elif fn is maybe_reverse_count_constructor_implied_do:
                    inv2 = fn(lhs, rhs, stmt_indent)
                elif fn is maybe_reverse_reshape:
                    inv2 = fn(lhs, rhs, stmt_indent, vars_view, allocatable_names)
                else:
                    inv2 = fn(lhs, rhs, stmt_indent, vars_view)
                if inv2 is not None:
                    return inv2
            return None

        # Handle one-line CASE arms that carry an assignment after ';', for example:
        #   case (...); x = sum(a)
        m_case_asn = re.match(r"^(?P<head>.*\bcase\b.*?;)\s*(?P<stmt>.+)$", code, re.IGNORECASE)
        if m_case_asn is not None:
            tail = m_case_asn.group("stmt").strip()
            m_tail_asn = ASSIGN_RE.match(tail)
            if m_tail_asn and LHS_ASSIGNABLE_RE.match(m_tail_asn.group(1).strip()):
                lhs_tail = m_tail_asn.group(1).strip()
                rhs_tail = m_tail_asn.group(2).strip()
                inv = try_assignment_rewrite(lhs_tail, rhs_tail, indent + "   ")
                if inv is not None:
                    case_head = m_case_asn.group("head").rstrip()
                    if case_head.endswith(";"):
                        case_head = case_head[:-1].rstrip()
                    body = inv[1].splitlines()
                    rebuilt = [f"{indent}{case_head}"]
                    for bl in body:
                        if bl.strip():
                            rebuilt.append(bl)
                    findings.append(
                        Finding(
                            path=path,
                            rule=inv[0],
                            start_line=idx,
                            end_line=idx,
                            suggestion="\n".join(rebuilt),
                        )
                    )
                    idx += 1
                    continue

        m_asn = ASSIGN_RE.match(code)
        if (not m_asn) or (not LHS_ASSIGNABLE_RE.match(m_asn.group(1).strip())):
            inv = maybe_reverse_random_seed_put_constructor(code, indent)
            if inv is not None:
                findings.append(Finding(path=path, rule=inv[0], start_line=idx, end_line=idx, suggestion=inv[1]))
                idx += 1
                continue
            inv = maybe_reverse_random_number_call(code, indent, vars_info)
            if inv is not None:
                findings.append(Finding(path=path, rule=inv[0], start_line=idx, end_line=idx, suggestion=inv[1]))
                idx += 1
                continue
            inv = maybe_reverse_print_reductions(code, indent, vars_info, type_map)
            if inv is not None:
                findings.append(Finding(path=path, rule=inv[0], start_line=idx, end_line=idx, suggestion=inv[1]))
                idx += 1
                continue
            inv = maybe_reverse_io_array_expr(code, indent, vars_info)
            if inv is not None:
                findings.append(Finding(path=path, rule=inv[0], start_line=idx, end_line=idx, suggestion=inv[1]))
                idx += 1
                continue
            inv = maybe_reverse_implied_do(code, indent)
            if inv is not None:
                findings.append(Finding(path=path, rule=inv[0], start_line=idx, end_line=idx, suggestion=inv[1]))
            idx += 1
            continue
        lhs = m_asn.group(1).strip()
        rhs = m_asn.group(2).strip()
        inv = try_assignment_rewrite(lhs, rhs, indent)
        if inv is not None:
            findings.append(Finding(path=path, rule=inv[0], start_line=idx, end_line=idx, suggestion=inv[1]))
        idx += 1
    return findings


def find_residual_array_ops(path: Path) -> List[Tuple[int, str, str]]:
    """Conservative residual check for prohibited array-operation forms."""
    text = fscan.read_text_flexible(path)
    lines = text.splitlines()
    vars_info, _ = collect_declared_info([ln + "\n" for ln in lines])

    issues: List[Tuple[int, str, str]] = []
    for idx, ln in enumerate(lines, start=1):
        code = fscan.strip_comment(ln).strip()
        if not code:
            continue

        code_no_str = strip_string_literals(code)

        if re.search(r"^\s*forall\b", code_no_str, re.IGNORECASE) or re.search(r"^\s*end\s*forall\b", code_no_str, re.IGNORECASE):
            issues.append((idx, "residual forall construct", ln.rstrip()))

        # Array transformational intrinsics are disallowed in loopified output.
        for name in ARRAY_TRANSFORM_INTRINSICS:
            if re.search(rf"\b{name}\s*\(", code_no_str, re.IGNORECASE):
                issues.append((idx, f"residual array transformational intrinsic '{name}'", ln.rstrip()))

        # Array constructors (excluding brackets that appear only in strings).
        # Allow declaration-time constructors (for example: real :: x(3) = [1,2,3]).
        if "[" in code_no_str and "]" in code_no_str:
            is_decl = TYPE_DECL_RE.match(code) is not None
            if not is_decl:
                issues.append((idx, "residual array constructor", ln.rstrip()))

        # print/write trailing array binary expression.
        mp = re.match(r"^\s*print\s+(.+)$", code, re.IGNORECASE)
        if mp:
            parts = split_top_level_commas(mp.group(1).strip())
            if len(parts) >= 2:
                last = parts[-1].strip()
                m = re.match(r"^\s*([a-z][a-z0-9_]*)\s*([+\-*/])\s*([a-z][a-z0-9_]*)\s*$", last, re.IGNORECASE)
                if m:
                    a = m.group(1).lower()
                    b = m.group(3).lower()
                    va = vars_info.get(a)
                    vb = vars_info.get(b)
                    if (va is not None and va.rank >= 1) or (vb is not None and vb.rank >= 1):
                        issues.append((idx, "residual print/write array expression", ln.rstrip()))

        # NOTE: We intentionally do not flag plain binary assignment
        # expressions here because file-level declaration collection can be
        # ambiguous across many subprogram scopes and causes false positives.

    return issues


def apply_fix_file(
    path: Path,
    findings: List[Finding],
    *,
    out_path: Optional[Path],
    create_backup: bool,
    add_trace: bool = True,
    annotate: bool = True,
) -> Tuple[int, Optional[Path]]:
    text = fscan.read_text_flexible(path)
    lines = text.splitlines(keepends=True)
    backup = None
    if out_path is None and create_backup:
        backup = Path(str(path) + ".bak")
        if backup.exists():
            k = 1
            while True:
                cand = Path(f"{path}.bak{k}")
                if not cand.exists():
                    backup = cand
                    break
                k += 1
        backup.write_text(text, encoding="utf-8")

    applied = 0
    for f in sorted(findings, key=lambda x: x.start_line, reverse=True):
        s = f.start_line - 1
        e = f.end_line
        if s < 0 or s >= len(lines):
            continue
        if e < f.start_line:
            e = f.start_line
        e0 = min(len(lines), e)
        new_block = f.suggestion
        old_block_lines = [ln.rstrip("\r\n") for ln in lines[s:e0]]
        indent = re.match(r"^(\s*)", lines[s]).group(1) if s < len(lines) else ""
        ann = ""
        if annotate:
            ann_lines: List[str] = []
            for old_ln in old_block_lines:
                old_code = fscan.strip_comment(old_ln).strip()
                if not old_code:
                    continue
                ann_lines.append(f"{indent}! {old_code}  !! replaced by xto_loop.py\n")
            ann = "".join(ann_lines)
        if not new_block.endswith("\n"):
            new_block += "\n"
        lines[s:e0] = [ann + new_block]
        applied += 1

    # Ensure generated loop variables are declared inside each procedure/program
    # scope that uses them (not at module scope).
    scope_stack: List[Dict[str, object]] = []
    insertions: List[Tuple[int, str]] = []

    def is_scope_start(code: str) -> bool:
        if re.match(r"^\s*end\b", code, re.IGNORECASE):
            return False
        return re.match(
            r"^\s*(?:\w+\s+)*(?:subroutine|function|program)\b",
            code,
            re.IGNORECASE,
        ) is not None

    def is_scope_end(code: str) -> bool:
        if re.match(
            r"^\s*end\s+(?:do|if|where|forall|associate|block|select|interface|module|type|critical|team|enum)\b",
            code,
            re.IGNORECASE,
        ):
            return False
        if re.match(r"^\s*end\s*$", code, re.IGNORECASE):
            return True
        return re.match(r"^\s*end\s+(?:subroutine|function|program)\b", code, re.IGNORECASE) is not None

    decl_like_re = re.compile(
        r"^\s*(use|implicit|parameter|dimension|save|data|common|external|intrinsic|equivalence|type\b|class\b|procedure\b|namelist|integer\b|real\b|complex\b|logical\b|character\b)\b",
        re.IGNORECASE,
    )

    for idx, ln in enumerate(lines):
        code = fscan.strip_comment(ln).strip().lstrip("\ufeffï»¿")
        if not code:
            continue

        if is_scope_start(code):
            scope_stack.append(
                {
                    "start_idx": idx,
                    "implicit_idx": None,
                    "declared": set(),
                    "used": set(),
                    "indent": re.match(r"^(\s*)", ln).group(1),
                    "block_depth": 0,
                }
            )

        if scope_stack:
            scope = scope_stack[-1]
            if re.match(r"^\s*block\s*$", code, re.IGNORECASE):
                scope["block_depth"] = int(scope["block_depth"]) + 1
            in_local_block = int(scope["block_depth"]) > 0
            if re.match(r"^\s*implicit\s+none\b", code, re.IGNORECASE):
                scope["implicit_idx"] = idx
                scope["indent"] = re.match(r"^(\s*)", ln).group(1)

            for nm in re.findall(r"\b[a-z][a-z0-9_]*\b", code, re.IGNORECASE):
                if is_generated_loop_var(nm):
                    scope["used"].add(nm.lower())

            if not in_local_block:
                for nm in parse_declared_entities(ln).keys():
                    if is_generated_loop_var(nm):
                        scope["declared"].add(nm.lower())

            if re.match(r"^\s*end\s+block\b", code, re.IGNORECASE):
                scope["block_depth"] = max(0, int(scope["block_depth"]) - 1)

        if scope_stack and is_scope_end(code):
            scope = scope_stack.pop()
            imp_idx = scope["implicit_idx"]
            used = scope["used"]
            declared = scope["declared"]
            missing = sorted(nm for nm in used if nm not in declared)
            if not missing:
                continue

            indent = scope["indent"]
            if imp_idx is not None:
                insertions.append((int(imp_idx) + 1, f"{indent}integer :: {', '.join(missing)} ! added by xto_loop.py\n"))
                continue

            # If no local IMPLICIT NONE exists (host implicit-none case), place
            # declaration after the declaration block at scope start.
            start_idx = int(scope["start_idx"])
            insert_at = start_idx + 1
            for j in range(start_idx + 1, idx):
                code_j = fscan.strip_comment(lines[j]).strip()
                if not code_j:
                    continue
                if TYPE_DECL_RE.match(code_j) or decl_like_re.match(code_j):
                    insert_at = j + 1
                    continue
                break
            insertions.append((insert_at, f"{indent}integer :: {', '.join(missing)} ! added by xto_loop.py\n"))

    for ins_idx, ins_line in sorted(insertions, key=lambda x: x[0], reverse=True):
        lines.insert(ins_idx, ins_line)

    # Prune unused generated loop vars from local BLOCK declarations.
    block_stack: List[int] = []
    block_ranges: List[Tuple[int, int]] = []
    for i, ln in enumerate(lines):
        code = fscan.strip_comment(ln).strip()
        if re.match(r"^\s*block\s*$", code, re.IGNORECASE):
            block_stack.append(i)
        elif re.match(r"^\s*end\s+block\b", code, re.IGNORECASE):
            if block_stack:
                block_ranges.append((block_stack.pop(), i))

    for s, e in block_ranges:
        used: Set[str] = set()
        decl_lines: List[int] = []
        for i in range(s + 1, e):
            code = fscan.strip_comment(lines[i]).strip()
            if not code:
                continue
            if re.match(r"^\s*integer\b.*::", code, re.IGNORECASE):
                decl_lines.append(i)
                continue
            for nm in re.findall(r"\b[a-z][a-z0-9_]*\b", code, re.IGNORECASE):
                if is_generated_loop_var(nm):
                    used.add(nm.lower())

        for i in decl_lines:
            ln = lines[i]
            m = re.match(r"^(\s*integer\b[^:\n]*::)(.*)$", ln, re.IGNORECASE)
            if not m:
                continue
            head = m.group(1)
            tail = m.group(2)
            eol = "\r\n" if ln.endswith("\r\n") else ("\n" if ln.endswith("\n") else "")
            comment = ""
            bang = tail.find("!")
            rhs = tail
            if bang >= 0:
                rhs = tail[:bang]
                comment = tail[bang:].rstrip("\r\n")
            items = split_top_level_commas(rhs.strip())
            kept: List[str] = []
            changed = False
            for it in items:
                tok = it.strip()
                if not tok:
                    continue
                base = tok.lower()
                if is_generated_loop_var(base) and base not in used:
                    changed = True
                    continue
                kept.append(tok)
            if not changed:
                continue
            if kept:
                rebuilt = f"{head} {', '.join(kept)}"
                if comment:
                    rebuilt += f" {comment}"
                lines[i] = rebuilt + eol
            else:
                lines[i] = ""

    out = out_path if out_path is not None else path
    out.parent.mkdir(parents=True, exist_ok=True)
    out_text = "".join(lines)
    if add_trace:
        out_text = with_trace_header(out_text, tool_name="xto_loop.py", source_name=path.name)
    out.write_text(out_text, encoding="utf-8")
    return applied, backup


def compile_one_for_summary(source: Path, command: str, *, phase: str) -> bool:
    q = fbuild.quote_cmd_arg(str(source))
    if "{file}" in command:
        cmd = command.replace("{file}", q)
    elif "{files}" in command:
        cmd = command.replace("{files}", q)
    else:
        cmd = f"{command} {q}".strip()
    proc = subprocess.run(cmd, shell=True, capture_output=True, text=True)
    if proc.returncode == 0:
        return True
    print(f"Compile ({phase}): FAIL (exit {proc.returncode})")
    print(f"  source: {source}")
    print(f"  command: {cmd}")
    if proc.stdout:
        print(proc.stdout.rstrip())
    if proc.stderr:
        print(fbuild.format_linker_stderr(proc.stderr).rstrip())
    return False


def outputs_compare_tolerant(
    a: str, b: str, *, rtol: float = 1.0e-6, atol: float = 1.0e-9
) -> Tuple[bool, float, float, int]:
    if a == b:
        return True, 0.0, 0.0, 0
    a_txt = a.rstrip()
    b_txt = b.rstrip()
    if a_txt == b_txt:
        return True, 0.0, 0.0, 0
    a_non = NUM_TOKEN_RE.sub("#", a_txt)
    b_non = NUM_TOKEN_RE.sub("#", b_txt)
    if a_non != b_non:
        return False, 0.0, 0.0, 0
    a_nums = NUM_TOKEN_RE.findall(a_txt)
    b_nums = NUM_TOKEN_RE.findall(b_txt)
    if len(a_nums) != len(b_nums):
        return False, 0.0, 0.0, 0
    max_abs = 0.0
    max_rel = 0.0
    for xa, xb in zip(a_nums, b_nums):
        va = float(xa.replace("d", "e").replace("D", "E"))
        vb = float(xb.replace("d", "e").replace("D", "E"))
        abs_diff = abs(va - vb)
        scale = max(abs(va), abs(vb))
        rel_diff = 0.0 if scale == 0.0 else abs_diff / scale
        if abs_diff > max_abs:
            max_abs = abs_diff
        if rel_diff > max_rel:
            max_rel = rel_diff
        if not math.isclose(va, vb, rel_tol=rtol, abs_tol=atol):
            return False, max_abs, max_rel, len(a_nums)
    return True, max_abs, max_rel, len(a_nums)


def outputs_match_tolerant(a: str, b: str, *, rtol: float = 1.0e-6, atol: float = 1.0e-9) -> bool:
    ok, _, _, _ = outputs_compare_tolerant(a, b, rtol=rtol, atol=atol)
    return ok


def count_file_lines(path: Path) -> int:
    return fscan.count_loc(path, exclude_blank=True, exclude_comment=True)


def with_trace_header(text: str, *, tool_name: str, source_name: str) -> str:
    """Prepend or refresh one-line provenance header in generated output."""
    stamp = datetime.now().strftime("%Y-%m-%d %H:%M")
    header = f"! created by {tool_name} from {source_name} on {stamp}\n"
    lines = text.splitlines(keepends=True)
    if lines and re.match(r"^\s*!\s*created by [a-z0-9_]+\.py\b", lines[0], re.IGNORECASE):
        lines[0] = header
        return "".join(lines)
    return header + text


def compile_and_run_capture(
    source: Path,
    *,
    exe_path: Path,
    label: str,
    quiet_run: bool = False,
    keep_exe: bool = False,
    deterministic_seed: bool = False,
    compile_flags: Optional[List[str]] = None,
) -> Tuple[bool, str, str]:
    compile_source = source
    seeded_temp_path: Optional[Path] = None
    if deterministic_seed:
        txt = fscan.read_text_flexible(source)
        has_random_number = re.search(RANDOM_NUMBER_RE, txt) is not None
        has_seed_control = re.search(RANDOM_SEED_CTRL_RE, txt) is not None
        if has_random_number and not has_seed_control:
            lines = txt.splitlines(keepends=True)
            inserted = False
            prog_idx = -1
            for i, ln in enumerate(lines):
                if PROGRAM_START_RE.match(ln) and not PROGRAM_END_RE.match(ln):
                    prog_idx = i
                    break
            if prog_idx >= 0:
                decl_like_re = re.compile(
                    r"^\s*(use|implicit|parameter|dimension|save|data|common|external|intrinsic|equivalence|type\b|class\b|procedure\b|namelist)\b",
                    re.IGNORECASE,
                )
                for i in range(prog_idx + 1, len(lines)):
                    code = fscan.strip_comment(lines[i]).strip()
                    if not code:
                        continue
                    if re.match(r"^\s*(contains|end\s+program)\b", code, re.IGNORECASE):
                        break
                    if TYPE_DECL_RE.match(code) or decl_like_re.match(code):
                        continue
                    indent = re.match(r"^(\s*)", lines[i]).group(1)
                    lines.insert(i, f"{indent}call random_init(.true., .false.) ! added by xto_loop.py run-diff\n")
                    inserted = True
                    break
            if inserted:
                fd, tmpname = tempfile.mkstemp(
                    prefix=f"{source.stem}_xto_seed_",
                    suffix=source.suffix if source.suffix else ".f90",
                    dir=str(source.parent),
                )
                with open(fd, "w", encoding="utf-8", closefd=True) as tf:
                    tf.write("".join(lines))
                seeded_temp_path = Path(tmpname)
                compile_source = seeded_temp_path

    compile_cmd = ["gfortran"]
    if compile_flags:
        compile_cmd.extend(compile_flags)
    compile_cmd.extend([str(compile_source), "-o", str(exe_path)])
    print(f"Build ({label}): {' '.join(fbuild.quote_cmd_arg(x) for x in compile_cmd)}")
    cp = subprocess.run(compile_cmd, capture_output=True, text=True)
    if cp.returncode != 0:
        print(f"Build ({label}): FAIL (exit {cp.returncode})")
        if cp.stdout:
            print(cp.stdout.rstrip())
        if cp.stderr:
            print(fbuild.format_linker_stderr(cp.stderr).rstrip())
        if seeded_temp_path is not None:
            seeded_temp_path.unlink(missing_ok=True)
        return False, "", ""
    print(f"Build ({label}): PASS")
    print(f"Run ({label}): {fbuild.quote_cmd_arg(str(exe_path))}")
    rp = subprocess.run([str(exe_path)], capture_output=True, text=True)
    try:
        if rp.returncode != 0:
            print(f"Run ({label}): FAIL (exit {rp.returncode})")
            if rp.stdout:
                print(rp.stdout.rstrip())
            if rp.stderr:
                print(rp.stderr.rstrip())
            return False, rp.stdout or "", rp.stderr or ""
        print(f"Run ({label}): PASS")
        if not quiet_run:
            if rp.stdout:
                print(rp.stdout.rstrip())
            if rp.stderr:
                print(rp.stderr.rstrip())
        return True, rp.stdout or "", rp.stderr or ""
    finally:
        if seeded_temp_path is not None:
            seeded_temp_path.unlink(missing_ok=True)
        if not keep_exe:
            exe_path.unlink(missing_ok=True)


def time_executable(exe_path: Path, *, label: str, reps: int) -> Tuple[bool, Optional[float], Optional[float]]:
    times: List[float] = []
    for _ in range(max(1, reps)):
        t0 = time.perf_counter()
        rp = subprocess.run([str(exe_path)], capture_output=True, text=True)
        dt = time.perf_counter() - t0
        if rp.returncode != 0:
            print(f"Time run ({label}): FAIL (exit {rp.returncode})")
            if rp.stdout:
                print(rp.stdout.rstrip())
            if rp.stderr:
                print(rp.stderr.rstrip())
            return False, None, None
        times.append(dt)
    if not times:
        return False, None, None
    xs = sorted(times)
    med = xs[len(xs) // 2] if len(xs) % 2 == 1 else 0.5 * (xs[len(xs) // 2 - 1] + xs[len(xs) // 2])
    avg = sum(times) / len(times)
    return True, med, avg


def main() -> int:
    global USE_XTO_INDICES
    parser = argparse.ArgumentParser(description="Convert selected Fortran array operations back to explicit loops")
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--verbose", action="store_true", help="Print full replacement suggestions")
    parser.add_argument("--progress", action="store_true", help="Print per-file progress while processing")
    parser.add_argument("--summary", action="store_true", help="Print per-file LOC summary")
    parser.add_argument("--fix", action="store_true", help="Apply suggested replacements in-place")
    parser.add_argument("--out", type=Path, help="With --fix, write transformed output to this file (single input)")
    parser.add_argument("--out-dir", type=Path, help="With --fix, write transformed outputs to this directory")
    parser.add_argument("--tee", action="store_true", help="With --fix and --out/--out-dir, print transformed output")
    parser.add_argument("--tee-both", action="store_true", help="With --fix and --out/--out-dir, print original/transformed")
    parser.add_argument("--run", action="store_true", help="After --fix with changes, build/run transformed source(s)")
    parser.add_argument("--run-both", action="store_true", help="Build/run original and transformed source(s)")
    parser.add_argument("--run-diff", action="store_true", help="Run original/transformed and compare outputs")
    parser.add_argument("--run-diff-rtol", type=float, default=1.0e-6, help="Relative tolerance for --run-diff numeric comparisons (default 1e-6)")
    parser.add_argument("--run-diff-atol", type=float, default=1.0e-9, help="Absolute tolerance for --run-diff numeric comparisons (default 1e-9)")
    parser.add_argument("--time-both", action="store_true", help="Time original and transformed executables (implies --run-diff)")
    parser.add_argument("--time-reps", type=int, default=3, help="Repetitions per executable for --time-both (default 3)")
    parser.add_argument("--quiet-run", action="store_true", help="Do not print stdout/stderr on successful runs")
    parser.add_argument("--keep-exe", action="store_true", help="Keep generated executables after running")
    parser.add_argument("--diff", action="store_true", help="With --fix, print unified diffs for changed files")
    parser.add_argument("--backup", dest="backup", action="store_true", default=True)
    parser.add_argument("--no-backup", dest="backup", action="store_false")
    parser.add_argument("--compiler", type=str, help="Compile command for baseline/after-fix validation")
    parser.add_argument("--compile-both", action="store_true", help="With --summary, add per-file compile_old/compile_new")
    parser.add_argument("--maxfail", type=int, help="With --compile-both, stop after N old-pass/new-fail cases")
    parser.add_argument("--limit", type=int, help="Maximum number of source files to process")
    parser.add_argument("--xto-indices", action="store_true", help="Use legacy loop indices i_xto/j_xto/k_xto/l_xto")
    parser.add_argument(
        "--no-annotate",
        "-no-annotate",
        dest="annotate",
        action="store_false",
        default=True,
        help="Do not emit trailing replacement comments in transformed code",
    )
    parser.add_argument(
        "--no-trace",
        "--notrace",
        dest="trace",
        action="store_false",
        default=True,
        help="Do not prepend created-by trace header to transformed output",
    )
    args = parser.parse_args()
    USE_XTO_INDICES = bool(args.xto_indices)

    if args.limit is not None and args.limit < 1:
        print("--limit must be >= 1.")
        return 2
    if args.maxfail is not None and args.maxfail < 1:
        print("--maxfail must be >= 1.")
        return 2
    if args.time_reps < 1:
        print("--time-reps must be >= 1.")
        return 2
    if args.run_diff_rtol < 0.0:
        print("--run-diff-rtol must be >= 0.")
        return 2
    if args.run_diff_atol < 0.0:
        print("--run-diff-atol must be >= 0.")
        return 2
    if args.time_both:
        args.run_diff = True
    if args.run_diff:
        args.run_both = True
        args.compile_both = True
    if args.run_both:
        args.run = True
    if args.tee_both:
        args.tee = True
    if args.tee and args.out is None and args.out_dir is None:
        args.out = Path("temp.f90")
    if args.run and args.out is None and args.out_dir is None:
        args.out = Path("temp.f90")
    if args.out is not None or args.out_dir is not None:
        args.fix = True
    if args.out is not None and args.out_dir is not None:
        print("Use only one of --out or --out-dir.")
        return 2
    if args.diff and not args.fix:
        print("--diff requires --fix.")
        return 2
    if args.compiler and not args.fix:
        print("--compiler requires --fix.")
        return 2
    if args.maxfail is not None and not (args.compile_both or args.run_diff):
        print("--maxfail requires --compile-both.")
        return 2

    files = choose_files(args.fortran_files, args.exclude)
    if args.limit is not None:
        files = files[: args.limit]
    if not files:
        print("No source files remain after applying --exclude filters.")
        return 2
    if args.out is not None and len(files) != 1:
        print("--out requires exactly one input source file.")
        return 2
    if args.out_dir is not None:
        if args.out_dir.exists() and not args.out_dir.is_dir():
            print("--out-dir exists but is not a directory.")
            return 2
        args.out_dir.mkdir(parents=True, exist_ok=True)

    findings: List[Finding] = []
    pre_lines: Dict[Path, int] = {p: count_file_lines(p) for p in files}
    for i, p in enumerate(files, start=1):
        if args.progress:
            print(f"[{i}/{len(files)}] Analyze {p}")
        findings.extend(analyze_file(p))
    by_file: Dict[Path, List[Finding]] = {}
    for f in findings:
        by_file.setdefault(f.path, []).append(f)

    if not findings:
        if args.summary:
            rows = [(fscan.display_path(p), pre_lines[p], pre_lines[p], 0) for p in files]
            fscan.print_loc_summary_table(rows, source_col="source", blocks_col="blocks_rep")
        else:
            print("No array-operation-to-loop replacement candidates found.")
        return 0

    findings.sort(key=lambda f: (f.path.name.lower(), f.start_line, f.end_line))
    print(f"{len(findings)} loopification candidate(s).")
    for f in findings:
        print(f"{f.path.name}:{f.start_line}-{f.end_line} {f.rule}")
        if args.verbose:
            print(f"  suggest: {f.suggestion}")

    transformed_by_file: Dict[Path, Path] = {p: p for p in files}
    replaced_by_file: Dict[Path, int] = {}
    post_lines: Dict[Path, int] = {p: pre_lines[p] for p in files}

    if args.fix:
        touched = 0
        total = 0
        residual_violations = 0
        for p in sorted(by_file.keys(), key=lambda x: x.name.lower()):
            out_path = args.out if args.out is not None else (args.out_dir / p.name if args.out_dir is not None else None)
            before = fscan.read_text_flexible(p)
            if out_path is not None and args.tee_both:
                print(f"--- original: {p} ---")
                print(before, end="")
                if not before.endswith("\n"):
                    print("")
            n, backup = apply_fix_file(
                p,
                by_file[p],
                out_path=out_path,
                create_backup=args.backup,
                add_trace=args.trace,
                annotate=args.annotate,
            )
            total += n
            replaced_by_file[p] = n
            target = out_path if out_path is not None else p
            transformed_by_file[p] = target
            post_lines[p] = count_file_lines(target)
            residual = find_residual_array_ops(target)
            if residual:
                residual_violations += 1
                print(f"\nResidual prohibited array operations in {fscan.display_path(target)}:")
                for lnno, reason, src in residual[:20]:
                    print(f"  {fscan.display_path(target)}:{lnno} ({reason})")
                    print(f"    {src}")
                if len(residual) > 20:
                    print(f"  ... and {len(residual) - 20} more")
            if n > 0:
                touched += 1
                if out_path is not None:
                    print(f"\nFixed {p.name}: replaced {n} block(s), wrote {out_path}")
                    if args.tee:
                        print(f"--- transformed: {out_path} ---")
                        print(fscan.read_text_flexible(out_path), end="")
                else:
                    print(f"\nFixed {p.name}: replaced {n} block(s), backup {backup.name if backup else '(none)'}")
                if args.diff:
                    after = fscan.read_text_flexible(target)
                    diff_lines = difflib.unified_diff(before.splitlines(), after.splitlines(), fromfile=f"a/{p.name}", tofile=f"b/{target.name}", lineterm="")
                    print("")
                    for ln in diff_lines:
                        print(ln)
        print(f"\n--fix summary: files changed {touched}, replaced {total}")
        if residual_violations:
            print(f"--residual-check: {residual_violations} file(s) still contain prohibited array-operation forms.")
            if not (args.run or args.run_both or args.compile_both):
                return 6

    compile_old_map: Optional[Dict[str, Optional[bool]]] = None
    compile_new_map: Optional[Dict[str, Optional[bool]]] = None
    if args.summary and args.compile_both:
        compile_old_map = {}
        compile_new_map = {}
        cmd = args.compiler or "gfortran -c -w -Wfatal-errors {file}"
        regressions = 0
        for p in files:
            key = fscan.display_path(p)
            ok_old = compile_one_for_summary(p, cmd, phase="old")
            compile_old_map[key] = ok_old
            ok_new = compile_one_for_summary(transformed_by_file.get(p, p), cmd, phase="new")
            compile_new_map[key] = ok_new
            if ok_old and not ok_new:
                regressions += 1
                if args.maxfail is not None and regressions >= args.maxfail:
                    print(f"Compile-both stopped at maxfail={args.maxfail} (old compiled, new failed).")
                    break

    run_match: Dict[str, Optional[bool]] = {}
    time_old_map: Dict[str, Optional[float]] = {}
    time_new_map: Dict[str, Optional[float]] = {}
    run_compile_flags: Optional[List[str]] = ["-O3", "-march=native", "-flto"] if args.time_both else None
    if args.run:
        run_regressions = 0
        for p in files:
            baseline_ok = True
            old_exe = Path(f"{p.stem}_orig.exe")
            if args.run_both:
                ok_old, out_old, err_old = compile_and_run_capture(
                    p,
                    exe_path=old_exe,
                    label="original",
                    quiet_run=args.quiet_run,
                    keep_exe=(args.keep_exe or args.time_both),
                    deterministic_seed=args.run_diff,
                    compile_flags=run_compile_flags,
                )
                if not ok_old:
                    baseline_ok = False
                    if args.progress or args.verbose:
                        print(f"Skip transformed run for {p}: baseline build/run failed.")
                    run_match[fscan.display_path(p)] = None
                    continue
            else:
                out_old, err_old = "", ""
            if (not args.run_both) and replaced_by_file.get(p, 0) <= 0:
                continue
            target = transformed_by_file.get(p, p)
            new_exe = Path(f"{target.stem}.exe")
            ok_new, out_new, err_new = compile_and_run_capture(
                target,
                exe_path=new_exe,
                label="transformed",
                quiet_run=args.quiet_run,
                keep_exe=(args.keep_exe or args.time_both),
                deterministic_seed=args.run_diff,
                compile_flags=run_compile_flags,
            )
            if not ok_new:
                run_match[fscan.display_path(p)] = False
                if baseline_ok:
                    run_regressions += 1
                    if args.maxfail is not None and run_regressions >= args.maxfail:
                        print(f"Run/compile stopped at maxfail={args.maxfail} (baseline passed, transformed failed).")
                        break
                continue
            if args.run_diff:
                same_out, max_abs_out, max_rel_out, n_out = outputs_compare_tolerant(
                    out_old, out_new, rtol=args.run_diff_rtol, atol=args.run_diff_atol
                )
                same_err, max_abs_err, max_rel_err, n_err = outputs_compare_tolerant(
                    err_old, err_new, rtol=args.run_diff_rtol, atol=args.run_diff_atol
                )
                same = same_out and same_err
                run_match[fscan.display_path(p)] = same
                n_num = n_out + n_err
                max_abs = max(max_abs_out, max_abs_err)
                max_rel = max(max_rel_out, max_rel_err)
                if n_num > 0:
                    print(
                        f"Run diff ({p.name}): {'MATCH' if same else 'DIFF'} "
                        f"(max_abs={max_abs:.3e}, max_rel={max_rel:.3e})"
                    )
                else:
                    print(f"Run diff ({p.name}): {'MATCH' if same else 'DIFF'}")
            if args.time_both and args.run_both:
                ok_t_old, med_old, _ = time_executable(old_exe, label="original", reps=args.time_reps)
                ok_t_new, med_new, _ = time_executable(new_exe, label="transformed", reps=args.time_reps)
                key = fscan.display_path(p)
                time_old_map[key] = med_old if ok_t_old else None
                time_new_map[key] = med_new if ok_t_new else None
                if ok_t_old and ok_t_new and med_old is not None and med_new is not None:
                    ratio = (med_new / med_old) if med_old > 0 else float("inf")
                    print(
                        f"Time ({p.name}): old {med_old:.6f}s, new {med_new:.6f}s, ratio {ratio:.3f} (reps {args.time_reps})"
                    )
                if not args.keep_exe:
                    old_exe.unlink(missing_ok=True)
                    new_exe.unlink(missing_ok=True)
        if args.run_diff:
            pairs = len(run_match)
            diffs = sum(1 for v in run_match.values() if v is False)
            print(f"Run diff summary: pairs {pairs}, matches {pairs - diffs}, diffs {diffs}")
        if args.time_both and time_old_map:
            print("Timing summary (median seconds):")
            for p in files:
                key = fscan.display_path(p)
                to = time_old_map.get(key)
                tn = time_new_map.get(key)
                if to is None or tn is None:
                    print(f"  {key}: old={to}, new={tn}")
                else:
                    ratio = (tn / to) if to > 0 else float("inf")
                    print(f"  {key}: old={to:.6f}, new={tn:.6f}, ratio={ratio:.3f}")

    if args.summary:
        rows = [(fscan.display_path(p), pre_lines[p], post_lines[p], replaced_by_file.get(p, 0)) for p in files]
        fscan.print_loc_summary_table(
            rows,
            source_col="source",
            blocks_col="blocks_rep",
            compile_old=compile_old_map,
            compile_new=compile_new_map,
        )
        if args.run_diff:
            print("runs_match:")
            for p in files:
                key = fscan.display_path(p)
                print(f"  {key}: {run_match.get(key, None)}")

    if args.fix and args.compiler and not args.compile_both:
        if not fbuild.run_compiler_command(args.compiler, files, "baseline", fscan.display_path):
            return 5
        targets = [transformed_by_file.get(p, p) for p in files]
        if not fbuild.run_compiler_command(args.compiler, targets, "after-fix", fscan.display_path):
            return 5
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
