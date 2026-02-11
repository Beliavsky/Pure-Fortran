#!/usr/bin/env python3
"""Advisory checker for likely out-of-bounds array indexing in Fortran."""

from __future__ import annotations

import argparse
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Set, Tuple

import cli_paths as cpaths
import fortran_scan as fscan
import xunset

TYPE_DECL_RE = re.compile(
    r"^\s*(integer|real|logical|character|complex|type\b|class\b|procedure\b)",
    re.IGNORECASE,
)
ALLOCATE_RE = re.compile(r"^\s*allocate\s*\((.*)\)\s*$", re.IGNORECASE)
INT_LIT_RE = re.compile(r"^[+-]?\d+$")
SIZE_SELF_RE = re.compile(r"^size\s*\(\s*([a-z][a-z0-9_]*)\s*(?:,\s*1\s*)?\)$", re.IGNORECASE)
SIZE_MINUS_RE = re.compile(
    r"^size\s*\(\s*([a-z][a-z0-9_]*)\s*(?:,\s*1\s*)?\)\s*-\s*([+-]?\d+)$",
    re.IGNORECASE,
)
SIZE_PLUS_RE = re.compile(
    r"^size\s*\(\s*([a-z][a-z0-9_]*)\s*(?:,\s*1\s*)?\)\s*\+\s*([+-]?\d+)$",
    re.IGNORECASE,
)
IF_THEN_RE = re.compile(r"^\s*if\s*\((.+)\)\s*then\s*$", re.IGNORECASE)
END_IF_RE = re.compile(r"^\s*end\s*if\b|^\s*endif\b", re.IGNORECASE)
ELSE_RE = re.compile(r"^\s*else\b", re.IGNORECASE)
RETURNISH_RE = re.compile(r"^\s*(return|stop\b|error\s*stop\b)", re.IGNORECASE)
ONE_LINE_RETURN_IF_RE = re.compile(
    r"^\s*if\s*\((.+)\)\s*(return|stop\b|error\s*stop\b)\s*$",
    re.IGNORECASE,
)
GUARD_LT_RE = re.compile(r"^\s*size\s*\(\s*([a-z][a-z0-9_]*)\s*\)\s*<\s*([+-]?\d+)\s*$", re.IGNORECASE)
GUARD_LE_RE = re.compile(r"^\s*size\s*\(\s*([a-z][a-z0-9_]*)\s*\)\s*<=\s*([+-]?\d+)\s*$", re.IGNORECASE)
GUARD_EQ_RE = re.compile(
    r"^\s*size\s*\(\s*([a-z][a-z0-9_]*)\s*\)\s*(?:==|\.eq\.)\s*([+-]?\d+)\s*$",
    re.IGNORECASE,
)
GUARD_VAR_GT_RE = re.compile(r"^\s*([a-z][a-z0-9_]*)\s*>\s*([+-]?\d+)\s*$", re.IGNORECASE)
GUARD_VAR_GE_RE = re.compile(r"^\s*([a-z][a-z0-9_]*)\s*>=\s*([+-]?\d+)\s*$", re.IGNORECASE)
SIZE_ASSIGN_RE = re.compile(
    r"^\s*([a-z][a-z0-9_]*)\s*=\s*size\s*\(\s*([a-z][a-z0-9_]*)\s*(?:,\s*1\s*)?\)\s*$",
    re.IGNORECASE,
)
MIN_ASSIGN_RE = re.compile(r"^\s*([a-z][a-z0-9_]*)\s*=\s*min\s*\((.+)\)\s*$", re.IGNORECASE)
SIMPLE_ASSIGN_LHS_RE = re.compile(r"^\s*([a-z][a-z0-9_]*)\s*=", re.IGNORECASE)
SIMPLE_IDENT_RE = re.compile(r"^\s*([a-z][a-z0-9_]*)\s*$", re.IGNORECASE)
DO_RE = re.compile(
    r"^\s*do\s+([a-z][a-z0-9_]*)\s*=\s*([^,]+?)\s*,\s*([^,]+?)(?:\s*,\s*([^,]+?))?\s*$",
    re.IGNORECASE,
)
END_DO_RE = re.compile(r"^\s*end\s*do\b|^\s*enddo\b", re.IGNORECASE)


@dataclass
class ArrayInfo:
    """Array declaration info used for simple bounds checks."""

    name: str
    rank: int
    is_dummy: bool
    is_assumed_shape: bool
    lb1: Optional[int]
    ub1: Optional[int]
    is_deferred_shape: bool


@dataclass
class Finding:
    """One possible out-of-bounds finding."""

    path: Path
    unit_kind: str
    unit_name: str
    line: int
    var: str
    index_expr: str
    severity: str
    detail: str
    stmt: str


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
    """Split text by top-level commas only."""
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


def parse_entity_dims(chunk: str) -> Tuple[Optional[str], List[str]]:
    """Return (name, dim-spec list) for one declaration entity chunk."""
    txt = chunk.strip()
    if not txt:
        return None, []
    if "=" in txt and "=>" not in txt:
        txt = txt.split("=", 1)[0].strip()
    mname = re.match(r"^([a-z][a-z0-9_]*)", txt, re.IGNORECASE)
    if not mname:
        return None, []
    name = mname.group(1).lower()
    rest = txt[mname.end() :].lstrip()
    if not rest.startswith("("):
        return name, []
    depth = 0
    end = -1
    for i, ch in enumerate(rest):
        if ch == "(":
            depth += 1
        elif ch == ")":
            depth -= 1
            if depth == 0:
                end = i
                break
    if end < 0:
        return name, []
    inner = rest[1:end].strip()
    if not inner:
        return name, []
    return name, split_top_level_commas(inner)


def parse_int(text: str) -> Optional[int]:
    """Parse integer literal text."""
    s = text.strip()
    if not INT_LIT_RE.match(s):
        return None
    try:
        return int(s)
    except ValueError:
        return None


def parse_declared_arrays(unit: xunset.Unit) -> Dict[str, ArrayInfo]:
    """Collect declared rank-1 arrays in one unit."""
    arrays: Dict[str, ArrayInfo] = {}
    for _ln, stmt in unit.body:
        low = stmt.strip().lower()
        if not low or not TYPE_DECL_RE.match(low) or "::" not in low:
            continue
        rhs = low.split("::", 1)[1]
        for chunk in split_top_level_commas(rhs):
            name, dims = parse_entity_dims(chunk)
            if not name or not dims:
                continue
            rank = len(dims)
            if rank != 1:
                continue
            d0 = dims[0].strip()
            lb1: Optional[int] = None
            ub1: Optional[int] = None
            is_assumed = False
            is_deferred = False
            if ":" in d0:
                parts = d0.split(":", 1)
                lb_txt = parts[0].strip()
                ub_txt = parts[1].strip()
                if name in unit.dummy_names:
                    # Assumed-shape dummy lower bound defaults to 1 unless explicitly set.
                    lb1 = parse_int(lb_txt) if lb_txt else 1
                    ub1 = parse_int(ub_txt) if ub_txt else None
                    is_assumed = True
                else:
                    # Deferred/assumed-shape non-dummy arrays get bounds at ALLOCATE time.
                    lb1 = parse_int(lb_txt) if lb_txt else None
                    ub1 = parse_int(ub_txt) if ub_txt else None
                    is_deferred = True
            else:
                lb1 = 1
                ub1 = parse_int(d0)
            arrays[name] = ArrayInfo(
                name=name,
                rank=1,
                is_dummy=(name in unit.dummy_names),
                is_assumed_shape=is_assumed,
                lb1=lb1,
                ub1=ub1,
                is_deferred_shape=is_deferred,
            )
    return arrays


def parse_allocate_bounds(stmt: str) -> Dict[str, Tuple[Optional[int], Optional[int]]]:
    """Parse rank-1 allocate specs as name -> (lb, ub), where bounds may be unknown."""
    out: Dict[str, Tuple[Optional[int], Optional[int]]] = {}
    m = ALLOCATE_RE.match(stmt.strip())
    if not m:
        return out
    for chunk in split_top_level_commas(m.group(1)):
        txt = chunk.strip()
        mm = re.match(r"^([a-z][a-z0-9_]*)\s*\((.*)\)\s*$", txt, re.IGNORECASE)
        if not mm:
            continue
        name = mm.group(1).lower()
        dims = split_top_level_commas(mm.group(2))
        if len(dims) != 1:
            continue
        d0 = dims[0].strip()
        if ":" in d0:
            lb_txt, ub_txt = d0.split(":", 1)
            lb = parse_int(lb_txt) if lb_txt.strip() else None
            ub = parse_int(ub_txt) if ub_txt.strip() else None
        else:
            lb = 1
            ub = parse_int(d0)
        out[name] = (lb, ub)
    return out


def allocated_names_in_stmt(stmt: str) -> Set[str]:
    """Return names that appear as allocate/deallocate targets anywhere in stmt."""
    out: Set[str] = set()
    low = stmt.lower()
    i = 0
    while i < len(low):
        m = re.search(r"\b(?:allocate|deallocate)\b", low[i:], re.IGNORECASE)
        if not m:
            break
        kw_start = i + m.start()
        kw_end = i + m.end()
        j = kw_end
        while j < len(low) and low[j].isspace():
            j += 1
        if j >= len(low) or low[j] != "(":
            i = kw_end
            continue
        # Match the argument list parentheses for allocate/deallocate.
        depth = 0
        end = -1
        k = j
        in_single = False
        in_double = False
        while k < len(stmt):
            ch = stmt[k]
            if ch == "'" and not in_double:
                if in_single and k + 1 < len(stmt) and stmt[k + 1] == "'":
                    k += 2
                    continue
                in_single = not in_single
            elif ch == '"' and not in_single:
                if in_double and k + 1 < len(stmt) and stmt[k + 1] == '"':
                    k += 2
                    continue
                in_double = not in_double
            elif not in_single and not in_double:
                if ch == "(":
                    depth += 1
                elif ch == ")":
                    depth -= 1
                    if depth == 0:
                        end = k
                        break
            k += 1
        if end < 0:
            i = kw_end
            continue
        inside = stmt[j + 1 : end]
        for chunk in split_top_level_commas(inside):
            n = fscan.base_identifier(chunk)
            if n:
                out.add(n)
        i = end + 1
    return out


def apply_allocate_hints(unit: xunset.Unit, arrays: Dict[str, ArrayInfo]) -> None:
    """Refine deferred-shape array bounds from ALLOCATE statements when consistent."""
    # Track consistent literal lower bounds seen for each array.
    seen_lb: Dict[str, Optional[int]] = {}
    seen_ub: Dict[str, Optional[int]] = {}
    for _ln, stmt in unit.body:
        al = parse_allocate_bounds(stmt)
        if not al:
            continue
        for n, (lb, ub) in al.items():
            ai = arrays.get(n)
            if ai is None or ai.is_dummy:
                continue
            if n not in seen_lb:
                seen_lb[n] = lb
            else:
                prev = seen_lb[n]
                seen_lb[n] = prev if (prev is not None and lb is not None and prev == lb) else None
            if n not in seen_ub:
                seen_ub[n] = ub
            else:
                prevu = seen_ub[n]
                seen_ub[n] = prevu if (prevu is not None and ub is not None and prevu == ub) else None
    for n, ai in arrays.items():
        if not ai.is_deferred_shape or ai.is_dummy:
            continue
        if n in seen_lb:
            ai.lb1 = seen_lb[n]
        if n in seen_ub:
            ai.ub1 = seen_ub[n]


def find_array_refs(stmt: str, array_names: List[str]) -> List[Tuple[str, str]]:
    """Extract rank-1 array reference pairs (name, index_expr) from statement."""
    refs: List[Tuple[str, str]] = []
    low = stmt.lower()
    for name in array_names:
        pat = re.compile(rf"\b{re.escape(name)}\s*\(", re.IGNORECASE)
        pos = 0
        while True:
            m = pat.search(low, pos)
            if not m:
                break
            start = low.find("(", m.start())
            if start < 0:
                break
            depth = 0
            in_single = False
            in_double = False
            end = -1
            i = start
            while i < len(stmt):
                ch = stmt[i]
                if ch == "'" and not in_double:
                    if in_single and i + 1 < len(stmt) and stmt[i + 1] == "'":
                        i += 2
                        continue
                    in_single = not in_single
                elif ch == '"' and not in_single:
                    if in_double and i + 1 < len(stmt) and stmt[i + 1] == '"':
                        i += 2
                        continue
                    in_double = not in_double
                elif not in_single and not in_double:
                    if ch == "(":
                        depth += 1
                    elif ch == ")":
                        depth -= 1
                        if depth == 0:
                            end = i
                            break
                i += 1
            if end < 0:
                break
            inside = stmt[start + 1 : end].strip()
            idxs = split_top_level_commas(inside)
            if len(idxs) == 1:
                refs.append((name, idxs[0].strip()))
            pos = end + 1
    return refs


def analyze_index(
    ai: ArrayInfo,
    idx_expr: str,
    known_size_lb: Optional[int] = None,
    known_size_eq: Optional[int] = None,
    idx_equals_size_of_same_array: bool = False,
) -> Optional[Tuple[str, str]]:
    """Return (severity, detail) for suspicious index expr, or None if clean."""
    idx = idx_expr.strip().lower()
    lit = parse_int(idx)
    if lit is not None:
        lb = ai.lb1 if ai.lb1 is not None else 1
        if lit < lb:
            return "high", f"index {lit} is below lower bound {lb}"
        if ai.ub1 is not None and lit > ai.ub1:
            return "high", f"index {lit} is above upper bound {ai.ub1}"
        if ai.is_assumed_shape and lit >= 1:
            if known_size_eq is not None and known_size_eq >= lit:
                return None
            if known_size_lb is not None and known_size_lb >= lit:
                return None
            return "medium", f"assumed-shape array may be empty; index {lit} can fail when size=0"
        return None

    m_size = SIZE_SELF_RE.match(idx)
    if m_size and m_size.group(1).lower() == ai.name:
        if ai.is_assumed_shape:
            if known_size_eq is not None and known_size_eq >= 1:
                return None
            if known_size_lb is not None and known_size_lb >= 1:
                return None
            return "medium", "size(array) can be 0; array(size(array)) may be out of bounds"
        return None

    m_sm = SIZE_MINUS_RE.match(idx)
    if m_sm and m_sm.group(1).lower() == ai.name:
        k = parse_int(m_sm.group(2))
        if k is not None:
            if ai.ub1 is not None and ai.lb1 is not None and ai.ub1 - k < ai.lb1:
                return "high", f"size(array)-{k} can be below lower bound"
            if ai.is_assumed_shape:
                if known_size_eq is not None and known_size_eq - k >= 1:
                    return None
                if known_size_lb is not None and known_size_lb >= (k + 1):
                    return None
                return "medium", f"size(array)-{k} can be < 1 for small/empty arrays"

    m_sp = SIZE_PLUS_RE.match(idx)
    if m_sp and m_sp.group(1).lower() == ai.name:
        k = parse_int(m_sp.group(2))
        if k is not None and k > 0:
            return "high", f"size(array)+{k} exceeds upper bound by construction"

    if ai.is_assumed_shape:
        if idx_equals_size_of_same_array and known_size_lb is not None and known_size_lb >= 1:
            return None
        return "low", "index expression on assumed-shape array has no visible bounds guard"
    return None


def alias_matches_size(expr: str, arr_name: str, size_alias: Dict[str, str]) -> bool:
    """True if expr is a variable alias currently known as size(arr_name)."""
    m = SIMPLE_IDENT_RE.match(expr.strip().lower())
    if not m:
        return False
    v = m.group(1).lower()
    return size_alias.get(v) == arr_name


def alias_minus_k(expr: str, arr_name: str, size_alias: Dict[str, str]) -> Optional[int]:
    """Return k for expressions alias-k where alias=size(arr_name), else None."""
    s = expr.strip().lower()
    m = re.match(r"^([a-z][a-z0-9_]*)\s*-\s*([+-]?\d+)$", s, re.IGNORECASE)
    if not m:
        return None
    v = m.group(1).lower()
    if size_alias.get(v) != arr_name:
        return None
    return parse_int(m.group(2))


def size_upper_minus_k(expr: str, arr_name: str, size_alias: Dict[str, str]) -> Optional[int]:
    """Return k when expr is known <= size(arr_name)-k; 0 means <= size(arr_name)."""
    s = expr.strip().lower()
    if alias_matches_size(s, arr_name, size_alias):
        return 0
    mself = SIZE_SELF_RE.match(s)
    if mself and mself.group(1).lower() == arr_name:
        return 0
    msm = SIZE_MINUS_RE.match(s)
    if msm and msm.group(1).lower() == arr_name:
        k = parse_int(msm.group(2))
        if k is not None and k >= 0:
            return k
    k = alias_minus_k(s, arr_name, size_alias)
    if k is not None and k >= 0:
        return k
    return None


def do_index_upper_safe_for_size(
    idx_var: str,
    arr_name: str,
    needed_k: int,
    do_stack: List[Tuple[str, str, str]],
    size_alias: Dict[str, str],
    size_minus_alias: Dict[str, Tuple[str, int]],
) -> bool:
    """True when active loop gives idx_var <= size(arr_name)-needed_k and idx_var >= 1."""
    for v, lb, ub in reversed(do_stack):
        if v != idx_var:
            continue
        lb_i = parse_int(lb.strip().lower())
        if lb_i is None or lb_i < 1:
            return False
        ub_s = ub.strip().lower()
        mk = size_upper_minus_k(ub_s, arr_name, size_alias)
        if mk is None:
            ms = SIMPLE_IDENT_RE.match(ub_s)
            if ms:
                ali = size_minus_alias.get(ms.group(1).lower())
                if ali is not None and ali[0] == arr_name:
                    mk = ali[1]
                elif alias_matches_size(ub_s, arr_name, size_alias):
                    mk = 0
        return mk is not None and mk >= needed_k
    return False


def analyze_slice_index(
    ai: ArrayInfo,
    idx_expr: str,
    *,
    known_size_lb: Optional[int],
    size_alias: Dict[str, str],
    do_stack: List[Tuple[str, str, str]],
    size_minus_alias: Dict[str, Tuple[str, int]],
) -> Optional[Tuple[str, str]]:
    """Analyze rank-1 slice index expressions like a(2:) or a(:n-1)."""
    s = idx_expr.strip().lower()
    if ":" not in s:
        return None
    lo_txt, hi_txt = s.split(":", 1)
    lo_txt = lo_txt.strip()
    hi_txt = hi_txt.strip()

    # Lower-bound safety for start index.
    lo_safe = False
    lo_risky = False
    if not lo_txt:
        lo_safe = True  # default lower bound 1
    else:
        lo_lit = parse_int(lo_txt)
        if lo_lit is not None:
            if lo_lit < 1:
                return "high", f"slice lower bound {lo_lit} is below lower bound 1"
            if known_size_lb is not None and known_size_lb >= lo_lit:
                lo_safe = True
            else:
                lo_risky = True
        elif alias_matches_size(lo_txt, ai.name, size_alias) or SIZE_SELF_RE.match(lo_txt):
            # x(size(x):...) requires at least one element.
            if known_size_lb is not None and known_size_lb >= 1:
                lo_safe = True
            else:
                lo_risky = True
        else:
            # size(x) + i / i + c style lower bounds under proven loop bounds.
            m1 = re.match(
                r"^\s*([+-]?\d+)\s*\+\s*([a-z][a-z0-9_]*)\s*$",
                lo_txt,
                re.IGNORECASE,
            )
            m2 = re.match(
                r"^\s*([a-z][a-z0-9_]*)\s*\+\s*([+-]?\d+)\s*$",
                lo_txt,
                re.IGNORECASE,
            )
            c: Optional[int] = None
            idxv: Optional[str] = None
            if m1:
                c = parse_int(m1.group(1))
                idxv = m1.group(2).lower()
            elif m2:
                c = parse_int(m2.group(2))
                idxv = m2.group(1).lower()
            if c is not None and idxv is not None and c >= 0:
                if do_index_upper_safe_for_size(
                    idxv, ai.name, c, do_stack, size_alias, size_minus_alias
                ):
                    lo_safe = True
                else:
                    lo_risky = True
            else:
                lo_risky = True

    # Upper-bound sanity for common safe forms.
    hi_safe = False
    hi_unknown = False
    if not hi_txt:
        hi_safe = True  # default upper bound size(array)
    elif alias_matches_size(hi_txt, ai.name, size_alias) or SIZE_SELF_RE.match(hi_txt):
        hi_safe = True
    else:
        k = alias_minus_k(hi_txt, ai.name, size_alias)
        if k is None:
            m_sm = SIZE_MINUS_RE.match(hi_txt)
            if m_sm and m_sm.group(1).lower() == ai.name:
                k = parse_int(m_sm.group(2))
        if k is not None:
            if k < 0:
                return "high", f"slice upper bound uses size(array)-({k}) which can exceed size(array)"
            if known_size_lb is not None and known_size_lb >= (k + 1):
                hi_safe = True
            else:
                hi_unknown = True
        else:
            hi_unknown = True
        if not hi_safe:
            # size(x)-i style upper bounds under proven loop bounds.
            mh1 = re.match(
                r"^\s*([a-z][a-z0-9_]*)\s*-\s*([a-z][a-z0-9_]*)\s*$",
                hi_txt,
                re.IGNORECASE,
            )
            mh2 = re.match(
                r"^\s*size\s*\(\s*([a-z][a-z0-9_]*)\s*(?:,\s*1\s*)?\)\s*-\s*([a-z][a-z0-9_]*)\s*$",
                hi_txt,
                re.IGNORECASE,
            )
            idxv: Optional[str] = None
            arr_ok = False
            if mh1:
                arr_ok = alias_matches_size(mh1.group(1).lower(), ai.name, size_alias)
                idxv = mh1.group(2).lower()
            elif mh2:
                arr_ok = mh2.group(1).lower() == ai.name
                idxv = mh2.group(2).lower()
            if arr_ok and idxv is not None:
                if do_index_upper_safe_for_size(
                    idxv, ai.name, 1, do_stack, size_alias, size_minus_alias
                ):
                    hi_safe = True
                    hi_unknown = False
                else:
                    hi_unknown = True

    # If we proved a safe upper bound, literal positive lower bounds are safe too.
    if lo_risky:
        lo_lit = parse_int(lo_txt)
        if lo_lit is not None and lo_lit >= 1 and hi_safe:
            lo_risky = False
            lo_safe = True

    if lo_safe and hi_safe:
        return None
    if lo_risky:
        return "medium", "slice lower bound may exceed array size without visible guard"
    if hi_unknown:
        return "low", "slice upper bound has no visible bounds guard"
    return "low", "slice index expression on assumed-shape array has no visible bounds guard"


def loop_index_known_safe(
    arr_name: str,
    idx_expr: str,
    do_stack: List[Tuple[str, str, str]],
    size_alias: Dict[str, str],
) -> bool:
    """True when idx_expr is active DO-index safely bounded by 1:size(arr_name)."""
    m = SIMPLE_IDENT_RE.match(idx_expr.strip().lower())
    if not m:
        return False
    idx_var = m.group(1).lower()
    for v, lb, ub in reversed(do_stack):
        if v != idx_var:
            continue
        lb_s = lb.strip().lower()
        ub_s = ub.strip().lower()
        lb_i = parse_int(lb_s)
        if lb_i is None or lb_i < 1:
            return False
        if SIZE_SELF_RE.match(ub_s):
            mm = SIZE_SELF_RE.match(ub_s)
            if mm and mm.group(1).lower() == arr_name:
                return True
        if alias_matches_size(ub_s, arr_name, size_alias):
            return True
        return False
    return False


def guard_condition_min_size(cond: str) -> Optional[Tuple[str, int]]:
    """Infer post-guard minimum size from a simple size(var) comparison.

    For guards that return on true, infer what must hold after the guard.
    """
    c = cond.strip().lower()
    m = GUARD_LT_RE.match(c)
    if m:
        n = parse_int(m.group(2))
        if n is None:
            return None
        return m.group(1).lower(), max(0, n)
    m = GUARD_LE_RE.match(c)
    if m:
        n = parse_int(m.group(2))
        if n is None:
            return None
        return m.group(1).lower(), max(0, n + 1)
    m = GUARD_EQ_RE.match(c)
    if m:
        n = parse_int(m.group(2))
        if n is None:
            return None
        if n == 0:
            return m.group(1).lower(), 1
    return None


def strip_outer_parens(text: str) -> str:
    """Strip balanced wrapping parentheses repeatedly."""
    s = text.strip()
    while s.startswith("(") and s.endswith(")"):
        depth = 0
        ok = True
        for i, ch in enumerate(s):
            if ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
                if depth == 0 and i != len(s) - 1:
                    ok = False
                    break
        if not ok or depth != 0:
            break
        s = s[1:-1].strip()
    return s


def split_top_level_or(cond: str) -> List[str]:
    """Split condition on top-level .or. operators."""
    out: List[str] = []
    cur: List[str] = []
    depth = 0
    in_single = False
    in_double = False
    i = 0
    s = cond
    low = cond.lower()
    while i < len(s):
        ch = s[i]
        if ch == "'" and not in_double:
            if in_single and i + 1 < len(s) and s[i + 1] == "'":
                cur.append("''")
                i += 2
                continue
            in_single = not in_single
            cur.append(ch)
            i += 1
            continue
        if ch == '"' and not in_single:
            if in_double and i + 1 < len(s) and s[i + 1] == '"':
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
            if depth == 0 and low.startswith(".or.", i):
                out.append("".join(cur).strip())
                cur = []
                i += 4
                continue
        cur.append(ch)
        i += 1
    tail = "".join(cur).strip()
    if tail:
        out.append(tail)
    return out


def guard_condition_min_sizes(cond: str) -> Dict[str, int]:
    """Infer post-guard minimum sizes from simple OR-combined size-conditions."""
    c = strip_outer_parens(cond.strip())
    clow = c.lower()
    # Avoid making unsound inferences from AND combinations.
    if ".and." in clow:
        g = guard_condition_min_size(c)
        if g is None:
            return {}
        return {g[0]: g[1]}
    out: Dict[str, int] = {}
    for part in split_top_level_or(c):
        g = guard_condition_min_size(strip_outer_parens(part))
        if g is None:
            continue
        v, k = g
        out[v] = max(k, out.get(v, 0))
    return out


def split_top_level_and(cond: str) -> List[str]:
    """Split condition on top-level .and. operators."""
    out: List[str] = []
    cur: List[str] = []
    depth = 0
    in_single = False
    in_double = False
    i = 0
    s = cond
    low = cond.lower()
    while i < len(s):
        ch = s[i]
        if ch == "'" and not in_double:
            if in_single and i + 1 < len(s) and s[i + 1] == "'":
                cur.append("''")
                i += 2
                continue
            in_single = not in_single
            cur.append(ch)
            i += 1
            continue
        if ch == '"' and not in_single:
            if in_double and i + 1 < len(s) and s[i + 1] == '"':
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
            if depth == 0 and low.startswith(".and.", i):
                out.append("".join(cur).strip())
                cur = []
                i += 5
                continue
        cur.append(ch)
        i += 1
    tail = "".join(cur).strip()
    if tail:
        out.append(tail)
    return out


def if_condition_exact_sizes(cond: str) -> Dict[str, int]:
    """Infer exact sizes for then-branch from simple size(var)==k conditions."""
    c = strip_outer_parens(cond.strip())
    parts = split_top_level_and(c)
    if not parts:
        parts = [c]
    out: Dict[str, int] = {}
    for p in parts:
        m = GUARD_EQ_RE.match(strip_outer_parens(p).strip().lower())
        if not m:
            continue
        k = parse_int(m.group(2))
        if k is None:
            continue
        out[m.group(1).lower()] = k
    return out


def if_condition_min_sizes_from_alias(cond: str, size_alias: Dict[str, str]) -> Dict[str, int]:
    """Infer then-branch minimum sizes from alias guards like n>0 with n=size(x)."""
    c = strip_outer_parens(cond.strip())
    clow = c.lower()
    if ".or." in clow:
        return {}
    parts = split_top_level_and(c)
    if not parts:
        parts = [c]
    out: Dict[str, int] = {}
    for p in parts:
        pp = strip_outer_parens(p).strip().lower()
        mgt = GUARD_VAR_GT_RE.match(pp)
        if mgt:
            v = mgt.group(1).lower()
            k = parse_int(mgt.group(2))
            arr = size_alias.get(v)
            if arr is not None and k is not None:
                out[arr] = max(out.get(arr, 0), k + 1)
            continue
        mge = GUARD_VAR_GE_RE.match(pp)
        if mge:
            v = mge.group(1).lower()
            k = parse_int(mge.group(2))
            arr = size_alias.get(v)
            if arr is not None and k is not None:
                out[arr] = max(out.get(arr, 0), k)
            continue
    return out


def return_guard_min_sizes_with_alias(cond: str, size_alias: Dict[str, str]) -> Dict[str, int]:
    """Infer post-return min sizes from simple guards with optional size aliases."""
    c = strip_outer_parens(cond.strip())
    clow = c.lower()
    # For return-on-true, OR clauses all become false after return path.
    parts = split_top_level_or(c) if ".or." in clow else [c]
    out: Dict[str, int] = {}
    for p in parts:
        pp = strip_outer_parens(p).strip().lower()
        # Native size(var) guards.
        g = guard_condition_min_size(pp)
        if g is not None:
            v, k = g
            out[v] = max(out.get(v, 0), k)
            continue
        # Alias guards: n < k or n <= k-1 where n=size(arr).
        mlt = re.match(r"^\s*([a-z][a-z0-9_]*)\s*<\s*([+-]?\d+)\s*$", pp, re.IGNORECASE)
        if mlt:
            v = mlt.group(1).lower()
            k = parse_int(mlt.group(2))
            arr = size_alias.get(v)
            if arr is not None and k is not None:
                out[arr] = max(out.get(arr, 0), max(0, k))
            continue
        mle = re.match(r"^\s*([a-z][a-z0-9_]*)\s*<=\s*([+-]?\d+)\s*$", pp, re.IGNORECASE)
        if mle:
            v = mle.group(1).lower()
            k = parse_int(mle.group(2))
            arr = size_alias.get(v)
            if arr is not None and k is not None:
                out[arr] = max(out.get(arr, 0), max(0, k + 1))
            continue
    return out


def infer_size_lower_bound_updates(unit: xunset.Unit) -> Dict[int, Dict[str, int]]:
    """Infer size-lower-bound facts at statement indices in one unit.

    Returns mapping: index -> {array_name: min_size} that becomes true before
    processing statement at that index.
    """
    updates: Dict[int, Dict[str, int]] = {}
    body = unit.body
    n = len(body)
    i = 0
    while i < n:
        _ln, stmt = body[i]
        s = stmt.strip()

        # One-line IF(cond) return
        m1 = ONE_LINE_RETURN_IF_RE.match(s)
        if m1:
            gs = guard_condition_min_sizes(m1.group(1))
            if gs:
                for v, k in gs.items():
                    updates.setdefault(i + 1, {})[v] = max(k, updates.get(i + 1, {}).get(v, 0))
            i += 1
            continue

        # Block IF(cond) ... return ... END IF with no ELSE at top level.
        m2 = IF_THEN_RE.match(s)
        if m2:
            depth = 1
            j = i + 1
            has_top_else = False
            has_top_return = False
            while j < n and depth > 0:
                _lnj, stj = body[j]
                sj = stj.strip()
                if IF_THEN_RE.match(sj):
                    depth += 1
                elif END_IF_RE.match(sj):
                    depth -= 1
                    if depth == 0:
                        break
                elif depth == 1:
                    if ELSE_RE.match(sj):
                        has_top_else = True
                    elif RETURNISH_RE.match(sj):
                        has_top_return = True
                j += 1
            if j < n and depth == 0:
                gs = guard_condition_min_sizes(m2.group(1))
                if gs and has_top_return and not has_top_else:
                    for v, k in gs.items():
                        updates.setdefault(j + 1, {})[v] = max(k, updates.get(j + 1, {}).get(v, 0))
                i = j + 1
                continue
        i += 1
    return updates


def analyze_unit(unit: xunset.Unit) -> List[Finding]:
    """Analyze one unit for likely OOB access patterns."""
    arrays = parse_declared_arrays(unit)
    apply_allocate_hints(unit, arrays)
    if not arrays:
        return []
    names = sorted(arrays.keys(), key=len, reverse=True)
    size_lb_updates = infer_size_lower_bound_updates(unit)
    known_size_lb: Dict[str, int] = {}
    if_eq_stack: List[Dict[str, int]] = []
    if_lb_stack: List[Dict[str, int]] = []
    size_alias: Dict[str, str] = {}
    size_minus_alias: Dict[str, Tuple[str, int]] = {}
    do_stack: List[Tuple[str, str, str]] = []
    findings: List[Finding] = []
    for idx, (ln, stmt) in enumerate(unit.body):
        upd = size_lb_updates.get(idx)
        if upd:
            for v, k in upd.items():
                known_size_lb[v] = max(k, known_size_lb.get(v, 0))
        low = stmt.strip().lower()
        m_do = DO_RE.match(low)
        if m_do:
            do_stack.append((m_do.group(1).lower(), m_do.group(2).strip(), m_do.group(3).strip()))
        elif END_DO_RE.match(low):
            if do_stack:
                do_stack.pop()
        m_if_then = IF_THEN_RE.match(low)
        if m_if_then:
            if_eq_stack.append(if_condition_exact_sizes(m_if_then.group(1)))
            if_lb_stack.append(if_condition_min_sizes_from_alias(m_if_then.group(1), size_alias))
        elif ELSE_RE.match(low):
            if if_eq_stack:
                if_eq_stack[-1] = {}
            if if_lb_stack:
                if_lb_stack[-1] = {}
        elif END_IF_RE.match(low):
            if if_eq_stack:
                if_eq_stack.pop()
            if if_lb_stack:
                if_lb_stack.pop()
        if not low:
            continue
        is_alloc_stmt = low.startswith("allocate")
        is_dealloc_stmt = low.startswith("deallocate")
        if (
            TYPE_DECL_RE.match(low)
            or low.startswith("use ")
            or low.startswith("implicit ")
            or low.startswith("contains")
            or is_alloc_stmt
            or is_dealloc_stmt
        ):
            continue
        # Track simple scalar aliases n = size(x) used in later guards.
        m_sz_as = SIZE_ASSIGN_RE.match(low)
        assigned_lhs: Optional[str] = None
        if m_sz_as:
            assigned_lhs = m_sz_as.group(1).lower()
            size_alias[assigned_lhs] = m_sz_as.group(2).lower()
            size_minus_alias.pop(assigned_lhs, None)
        else:
            m_min_as = MIN_ASSIGN_RE.match(low)
            if m_min_as:
                lhs = m_min_as.group(1).lower()
                assigned_lhs = lhs
                args = split_top_level_commas(m_min_as.group(2))
                bound: Optional[Tuple[str, int]] = None
                for a in args:
                    a1 = a.strip().lower()
                    msm = SIZE_MINUS_RE.match(a1)
                    if msm:
                        k = parse_int(msm.group(2))
                        if k is not None and k >= 0:
                            bound = (msm.group(1).lower(), k)
                            break
                    ms = re.match(
                        r"^\s*([a-z][a-z0-9_]*)\s*-\s*([+-]?\d+)\s*$", a1, re.IGNORECASE
                    )
                    if ms:
                        arr = size_alias.get(ms.group(1).lower())
                        k = parse_int(ms.group(2))
                        if arr is not None and k is not None and k >= 0:
                            bound = (arr, k)
                            break
                if bound is not None:
                    size_minus_alias[lhs] = bound
                else:
                    size_minus_alias.pop(lhs, None)
            m_lhs = SIMPLE_ASSIGN_LHS_RE.match(low)
            if m_lhs:
                lhs = m_lhs.group(1).lower()
                if assigned_lhs is not None and lhs == assigned_lhs:
                    pass
                elif lhs in size_alias:
                    size_alias.pop(lhs, None)
                if assigned_lhs is not None and lhs == assigned_lhs:
                    pass
                elif lhs in size_minus_alias:
                    size_minus_alias.pop(lhs, None)
        # Alias-aware one-line return guard updates (e.g., if (n < 2) return with n=size(x)).
        m_ret = ONE_LINE_RETURN_IF_RE.match(low)
        if m_ret:
            gs_alias = return_guard_min_sizes_with_alias(m_ret.group(1), size_alias)
            for v, k in gs_alias.items():
                known_size_lb[v] = max(k, known_size_lb.get(v, 0))

        alloc_like_names = allocated_names_in_stmt(stmt)
        known_eq: Dict[str, int] = {}
        for frame in if_eq_stack:
            for v, k in frame.items():
                if v in known_eq and known_eq[v] != k:
                    known_eq.pop(v, None)
                    continue
                known_eq[v] = k
        known_lb_local: Dict[str, int] = dict(known_size_lb)
        for frame in if_lb_stack:
            for v, k in frame.items():
                known_lb_local[v] = max(known_lb_local.get(v, 0), k)
        for name, idx_expr in find_array_refs(stmt, names):
            if name in alloc_like_names:
                continue
            ai = arrays.get(name)
            if ai is None:
                continue
            if ":" in idx_expr and ai.rank == 1 and ai.is_assumed_shape:
                rs = analyze_slice_index(
                    ai,
                    idx_expr,
                    known_size_lb=known_lb_local.get(name),
                    size_alias=size_alias,
                    do_stack=do_stack,
                    size_minus_alias=size_minus_alias,
                )
                if rs is None:
                    continue
                sev, detail = rs
                findings.append(
                    Finding(
                        path=unit.path,
                        unit_kind=unit.kind,
                        unit_name=unit.name,
                        line=ln,
                        var=name,
                        index_expr=idx_expr.strip(),
                        severity=sev,
                        detail=detail,
                        stmt=stmt.strip(),
                    )
                )
                continue
            idx_eq_size = False
            midx = SIMPLE_IDENT_RE.match(idx_expr.strip().lower())
            if midx:
                v = midx.group(1).lower()
                idx_eq_size = size_alias.get(v) == name
            if ai.is_assumed_shape and loop_index_known_safe(name, idx_expr, do_stack, size_alias):
                continue
            res = analyze_index(
                ai,
                idx_expr,
                known_size_lb=known_lb_local.get(name),
                known_size_eq=known_eq.get(name),
                idx_equals_size_of_same_array=idx_eq_size,
            )
            if res is None:
                continue
            sev, detail = res
            findings.append(
                Finding(
                    path=unit.path,
                    unit_kind=unit.kind,
                    unit_name=unit.name,
                    line=ln,
                    var=name,
                    index_expr=idx_expr.strip(),
                    severity=sev,
                    detail=detail,
                    stmt=stmt.strip(),
                )
            )
    return findings


def analyze_file(path: Path) -> List[Finding]:
    """Analyze one source file."""
    infos, any_missing = fscan.load_source_files([path])
    if not infos or any_missing:
        return []
    finfo = infos[0]
    out: List[Finding] = []
    for unit in xunset.collect_units(finfo):
        out.extend(analyze_unit(unit))
    return out


def main() -> int:
    """Run OOB advisory checks."""
    parser = argparse.ArgumentParser(
        description="Warn about likely out-of-bounds array indexing patterns in Fortran"
    )
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--verbose", action="store_true", help="Print full statement lines and details")
    args = parser.parse_args()

    files = choose_files(args.fortran_files, args.exclude)
    if not files:
        print("No source files remain after applying --exclude filters.")
        return 2

    findings: List[Finding] = []
    for p in files:
        findings.extend(analyze_file(p))

    if not findings:
        print("No likely out-of-bounds findings.")
        return 0

    sev_order = {"high": 0, "medium": 1, "low": 2}
    findings.sort(key=lambda f: (sev_order.get(f.severity, 9), f.path.name.lower(), f.line, f.var))
    print(f"{len(findings)} likely out-of-bounds finding(s).")
    if args.verbose:
        for f in findings:
            print(
                f"{f.path.name}:{f.line} [{f.severity}] {f.unit_kind} {f.unit_name} "
                f"{f.var}({f.index_expr}) - {f.detail}"
            )
            print(f"  {f.stmt}")
    else:
        by_file: Dict[str, int] = {}
        for f in findings:
            by_file[f.path.name] = by_file.get(f.path.name, 0) + 1
        for fn in sorted(by_file.keys(), key=str.lower):
            print(f"{fn}: {by_file[fn]}")
        first = findings[0]
        print(
            f"\nFirst finding: {first.path.name}:{first.line} [{first.severity}] "
            f"{first.var}({first.index_expr}) - {first.detail}"
        )
        print("Run with --verbose to print statements and details.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
