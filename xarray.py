#!/usr/bin/env python3
"""Advisory checker for simple DO loops replaceable by array operations."""

from __future__ import annotations

import argparse
import difflib
import re
import shutil
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Set, Tuple

import cli_paths as cpaths
import fortran_scan as fscan
import xunset

DO_RE = re.compile(
    r"^\s*do\s+([a-z][a-z0-9_]*)\s*=\s*([^,]+?)\s*,\s*([^,]+?)(?:\s*,\s*([^,]+?))?\s*$",
    re.IGNORECASE,
)
END_DO_RE = re.compile(r"^\s*end\s*do\b", re.IGNORECASE)
IF_THEN_RE = re.compile(r"^\s*if\s*\((.+)\)\s*then\s*$", re.IGNORECASE)
END_IF_RE = re.compile(r"^\s*end\s*if\b", re.IGNORECASE)
ELSE_RE = re.compile(r"^\s*else\b", re.IGNORECASE)
ONE_LINE_IF_RE = re.compile(r"^\s*if\s*\((.+)\)\s*(.+)$", re.IGNORECASE)
ASSIGN_RE = re.compile(r"^\s*(.+?)\s*=\s*(.+)$", re.IGNORECASE)
ALLOCATE_RE = re.compile(r"^\s*allocate\s*\((.*)\)\s*$", re.IGNORECASE)
TYPE_DECL_RE = re.compile(r"^\s*(integer|real|logical|character|complex|type\b|class\b)", re.IGNORECASE)
SIMPLE_NAME_RE = re.compile(r"^\s*([a-z][a-z0-9_]*)\s*$", re.IGNORECASE)
INDEXED_NAME_RE = re.compile(r"^\s*([a-z][a-z0-9_]*)\s*\(\s*([a-z][a-z0-9_]*)\s*\)\s*$", re.IGNORECASE)
ADD_ACC_RE = re.compile(
    r"^\s*([a-z][a-z0-9_]*)\s*\+\s*(.+)$",
    re.IGNORECASE,
)
MUL_ACC_RE = re.compile(
    r"^\s*([a-z][a-z0-9_]*)\s*\*\s*(.+)$",
    re.IGNORECASE,
)
ADD_ONE_RE = re.compile(
    r"^\s*([a-z][a-z0-9_]*)\s*\+\s*1(?:\.0+)?(?:_[a-z0-9]+)?\s*$",
    re.IGNORECASE,
)
ZERO_LITERAL_RE = re.compile(r"^\s*[+-]?0(?:\.0+)?(?:_[a-z0-9_]+)?\s*$", re.IGNORECASE)
NUM_LITERAL_RE = re.compile(
    r"^\s*[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[deq][+-]?\d+)?(?:_[a-z0-9_]+)?\s*$",
    re.IGNORECASE,
)
LOGICAL_LITERAL_RE = re.compile(r"^\s*\.(?:true|false)\.\s*$", re.IGNORECASE)
CHAR_LITERAL_RE = re.compile(r"^\s*(['\"]).*\1\s*$", re.IGNORECASE)

BEGIN_TAG = "!! beginning of code that xarray.py suggests replacing"
END_TAG = "!! end of code that xarray.py suggests replacing"
CHANGED_TAG = "!! changed by xarray.py"


@dataclass
class Finding:
    """One loop-to-array replacement suggestion."""

    path: Path
    rule: str
    unit_kind: str
    unit_name: str
    start_line: int
    end_line: int
    suggestion: str


CMP_RE = re.compile(r"^\s*(.+?)\s*(<=|>=|<|>)\s*(.+?)\s*$", re.IGNORECASE)
IDENT_RE = re.compile(r"\b([a-z][a-z0-9_]*)\b", re.IGNORECASE)
CALL_LIKE_RE = re.compile(r"\b([a-z][a-z0-9_]*)\s*\(", re.IGNORECASE)


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


def make_backup_path(path: Path) -> Path:
    """Create a non-overwriting backup path next to path."""
    base = Path(str(path) + ".bak")
    if not base.exists():
        return base
    idx = 1
    while True:
        cand = Path(f"{path}.bak{idx}")
        if not cand.exists():
            return cand
        idx += 1


def split_top_level_commas(text: str) -> List[str]:
    """Split text on top-level commas only."""
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


def split_code_comment(line: str) -> Tuple[str, str]:
    """Split one line into code and trailing comment, respecting quotes."""
    in_single = False
    in_double = False
    i = 0
    while i < len(line):
        ch = line[i]
        if ch == "'" and not in_double:
            if in_single and i + 1 < len(line) and line[i + 1] == "'":
                i += 2
                continue
            in_single = not in_single
            i += 1
            continue
        if ch == '"' and not in_single:
            if in_double and i + 1 < len(line) and line[i + 1] == '"':
                i += 2
                continue
            in_double = not in_double
            i += 1
            continue
        if ch == "!" and not in_single and not in_double:
            return line[:i], line[i:]
        i += 1
    return line, ""


def strip_quoted_text(text: str) -> str:
    """Replace quoted contents with spaces for token scanning."""
    out: List[str] = []
    in_single = False
    in_double = False
    i = 0
    while i < len(text):
        ch = text[i]
        if ch == "'" and not in_double:
            if in_single and i + 1 < len(text) and text[i + 1] == "'":
                out.append("  ")
                i += 2
                continue
            in_single = not in_single
            out.append(" ")
            i += 1
            continue
        if ch == '"' and not in_single:
            if in_double and i + 1 < len(text) and text[i + 1] == '"':
                out.append("  ")
                i += 2
                continue
            in_double = not in_double
            out.append(" ")
            i += 1
            continue
        out.append(" " if (in_single or in_double) else ch)
        i += 1
    return "".join(out)


def replace_index_with_slice(expr: str, idx_var: str, lb: str, ub: str) -> str:
    """Rewrite occurrences of name(idx_var) to name(lb:ub) in expression."""
    pat = re.compile(
        rf"\b([a-z][a-z0-9_]*)\s*\(\s*{re.escape(idx_var)}\s*\)",
        re.IGNORECASE,
    )
    return pat.sub(lambda m: f"{m.group(1)}({lb}:{ub})", expr)


def build_range(lb: str, ub: str, step: Optional[str]) -> str:
    """Build Fortran slice-range text from loop bounds and optional step."""
    lb_s = lb.strip()
    ub_s = ub.strip()
    if step is None:
        return f"{lb_s}:{ub_s}"
    st = step.strip()
    return f"{lb_s}:{ub_s}:{st}"


def has_loop_var(expr: str, loop_var: str) -> bool:
    """Check whether expression still contains loop index variable."""
    return re.search(rf"\b{re.escape(loop_var)}\b", expr, re.IGNORECASE) is not None


def split_range(rng: str) -> Tuple[str, str]:
    """Split range text into (lb, ub_or_ub_step)."""
    return rng.split(":", 1)[0], rng.split(":", 1)[1]


def normalize_expr(text: str) -> str:
    """Normalize expression text for conservative equality checks."""
    return "".join(text.lower().split())


def parse_rank1_decl_bounds(unit: xunset.Unit) -> Dict[str, str]:
    """Collect rank-1 declaration bounds text as 'lb:ub'."""
    out: Dict[str, str] = {}
    for _ln, stmt in unit.body:
        low = stmt.strip().lower()
        if not low or "::" not in low:
            continue
        if not re.match(r"^\s*(integer|real|logical|character|complex|type\b|class\b)", low, re.IGNORECASE):
            continue
        rhs = low.split("::", 1)[1]
        for chunk in split_top_level_commas(rhs):
            txt = chunk.strip()
            if not txt:
                continue
            # Strip init.
            if "=" in txt and "=>" not in txt:
                txt = txt.split("=", 1)[0].strip()
            mname = re.match(r"^([a-z][a-z0-9_]*)", txt, re.IGNORECASE)
            if not mname:
                continue
            name = mname.group(1).lower()
            rest = txt[mname.end() :].lstrip()
            if not rest.startswith("("):
                continue
            depth = 0
            end_pos = -1
            for j, ch in enumerate(rest):
                if ch == "(":
                    depth += 1
                elif ch == ")":
                    depth -= 1
                    if depth == 0:
                        end_pos = j
                        break
            if end_pos < 0:
                continue
            dims = rest[1:end_pos].strip()
            if not dims:
                continue
            if "," in dims:
                continue
            if ":" in dims:
                lb, ub = dims.split(":", 1)
                lb = lb.strip() or "1"
                ub = ub.strip()
                if not ub:
                    continue
                out.setdefault(name, f"{lb}:{ub}")
            else:
                d = dims.strip()
                if not d:
                    continue
                out.setdefault(name, f"1:{d}")
    return out


def collect_rank1_array_names(unit: xunset.Unit) -> Set[str]:
    """Collect rank-1 array names (including assumed/deferred shape) declared in one unit."""
    out: Set[str] = set()
    for _ln, stmt in unit.body:
        low = stmt.strip().lower()
        if not low or "::" not in low:
            continue
        if not re.match(r"^\s*(integer|real|logical|character|complex|type\b|class\b)", low, re.IGNORECASE):
            continue
        rhs = low.split("::", 1)[1]
        for chunk in split_top_level_commas(rhs):
            txt = chunk.strip()
            if not txt:
                continue
            if "=" in txt and "=>" not in txt:
                txt = txt.split("=", 1)[0].strip()
            mname = re.match(r"^([a-z][a-z0-9_]*)", txt, re.IGNORECASE)
            if not mname:
                continue
            name = mname.group(1).lower()
            rest = txt[mname.end() :].lstrip()
            if not rest.startswith("("):
                continue
            depth = 0
            end_pos = -1
            for j, ch in enumerate(rest):
                if ch == "(":
                    depth += 1
                elif ch == ")":
                    depth -= 1
                    if depth == 0:
                        end_pos = j
                        break
            if end_pos < 0:
                continue
            dims = rest[1:end_pos].strip()
            if not dims or "," in dims:
                continue
            out.add(name)
    return out


def simplify_section_expr(expr: str, decl_bounds: Dict[str, str]) -> str:
    """Simplify full-range rank-1 sections to whole arrays when clearly safe."""
    s = expr
    # Rule 1: a(1:size(a)) -> a
    s = re.sub(
        r"\b([a-z][a-z0-9_]*)\s*\(\s*1\s*:\s*size\s*\(\s*\1\s*\)\s*\)",
        r"\1",
        s,
        flags=re.IGNORECASE,
    )
    # Rule 2: a(lb:ub) -> a when section matches declaration bounds.
    for name, bnd in decl_bounds.items():
        pat = re.compile(rf"\b{re.escape(name)}\s*\(\s*([^)]*)\)", re.IGNORECASE)
        def repl(m: re.Match[str]) -> str:
            rng = m.group(1).strip()
            if ":" not in rng:
                return m.group(0)
            if normalize_expr(rng) == normalize_expr(bnd):
                return name
            return m.group(0)
        s = pat.sub(repl, s)
    return s


def parse_alloc_shape_spec(stmt: str) -> Dict[str, str]:
    """Parse simple allocate(var(size(expr))) patterns as var -> expr."""
    out: Dict[str, str] = {}
    m = ALLOCATE_RE.match(stmt.strip())
    if not m:
        return out
    for chunk in split_top_level_commas(m.group(1)):
        txt = chunk.strip()
        mm = re.match(
            r"^([a-z][a-z0-9_]*)\s*\(\s*size\s*\(\s*([a-z][a-z0-9_]*)\s*\)\s*\)\s*$",
            txt,
            re.IGNORECASE,
        )
        if not mm:
            continue
        out[mm.group(1).lower()] = mm.group(2).lower()
    return out


def can_collapse_lhs_alloc(name: str, rng: str, alloc_map: Dict[str, str]) -> bool:
    """True if lhs range matches known allocate(name(size(src))) pattern."""
    src = alloc_map.get(name.lower())
    if src is None:
        return False
    if normalize_expr(rng) == normalize_expr(f"1:size({src})"):
        return True
    return False


def maybe_elementwise(
    loop_var: str,
    rng: str,
    body_stmt: str,
    decl_bounds: Dict[str, str],
    alloc_map: Dict[str, str],
) -> Optional[str]:
    """Detect elementwise assignment loop and build replacement."""
    m = ASSIGN_RE.match(body_stmt.strip())
    if not m:
        return None
    lhs = m.group(1).strip()
    rhs = m.group(2).strip()
    m_lhs = INDEXED_NAME_RE.match(lhs)
    if not m_lhs:
        return None
    lhs_name = m_lhs.group(1)
    lhs_idx = m_lhs.group(2).lower()
    if lhs_idx != loop_var.lower():
        return None
    lb, ubtxt = split_range(rng)
    rhs_sliced = replace_index_with_slice(rhs, loop_var, lb, ubtxt)
    # The helper above expects (lb, ub-text). rng may include step in ub-text.
    if has_loop_var(rhs_sliced, loop_var):
        return None
    if rhs_sliced == rhs:
        return None
    lhs_expr = f"{lhs_name}({rng})"
    bnd = decl_bounds.get(lhs_name.lower())
    if bnd is not None and normalize_expr(rng) == normalize_expr(bnd):
        lhs_expr = lhs_name
    elif can_collapse_lhs_alloc(lhs_name, rng, alloc_map):
        lhs_expr = lhs_name
    rhs_sliced = simplify_section_expr(rhs_sliced, decl_bounds)
    return f"{lhs_expr} = {rhs_sliced}"


def maybe_temp_then_elementwise(
    loop_var: str,
    rng: str,
    stmt_temp: str,
    stmt_body: str,
    decl_bounds: Dict[str, str],
    alloc_map: Dict[str, str],
) -> Optional[str]:
    """Detect temp-scalar then elementwise assignment and inline temp expression."""
    m1 = ASSIGN_RE.match(stmt_temp.strip())
    m2 = ASSIGN_RE.match(stmt_body.strip())
    if not m1 or not m2:
        return None

    temp_name_m = SIMPLE_NAME_RE.match(m1.group(1).strip())
    if not temp_name_m:
        return None
    temp_name = temp_name_m.group(1)
    rhs1 = m1.group(2).strip()

    lhs2 = m2.group(1).strip()
    rhs2 = m2.group(2).strip()
    m_lhs2 = INDEXED_NAME_RE.match(lhs2)
    if not m_lhs2:
        return None
    lhs_name = m_lhs2.group(1)
    lhs_idx = m_lhs2.group(2).lower()
    if lhs_idx != loop_var.lower():
        return None
    if lhs_name.lower() == temp_name.lower():
        return None
    if not re.search(rf"\b{re.escape(temp_name)}\b", rhs2, re.IGNORECASE):
        return None

    lb, ubtxt = split_range(rng)
    rhs1_s = replace_index_with_slice(rhs1, loop_var, lb, ubtxt)
    if has_loop_var(rhs1_s, loop_var):
        return None
    rhs2_s = replace_index_with_slice(rhs2, loop_var, lb, ubtxt)
    rhs2_s = re.sub(
        rf"\b{re.escape(temp_name)}\b\s*\*\s*\b{re.escape(temp_name)}\b",
        f"(({rhs1_s})**2)",
        rhs2_s,
        flags=re.IGNORECASE,
    )
    rhs2_inlined = re.sub(
        rf"\b{re.escape(temp_name)}\b",
        f"({rhs1_s})",
        rhs2_s,
        flags=re.IGNORECASE,
    )
    if has_loop_var(rhs2_inlined, loop_var):
        return None

    lhs_expr = f"{lhs_name}({rng})"
    bnd = decl_bounds.get(lhs_name.lower())
    if bnd is not None and normalize_expr(rng) == normalize_expr(bnd):
        lhs_expr = lhs_name
    elif can_collapse_lhs_alloc(lhs_name, rng, alloc_map):
        lhs_expr = lhs_name

    rhs2_inlined = simplify_section_expr(rhs2_inlined, decl_bounds)
    return f"{lhs_expr} = {rhs2_inlined}"


def maybe_reduction_sum(
    loop_var: str,
    rng: str,
    body_stmt: str,
    prev_stmt: Optional[str],
) -> Optional[str]:
    """Detect sum-reduction loop with preceding initialization and build replacement."""
    if prev_stmt is None:
        return None
    mp = ASSIGN_RE.match(prev_stmt.strip())
    ml = ASSIGN_RE.match(body_stmt.strip())
    if not mp or not ml:
        return None
    acc_prev = SIMPLE_NAME_RE.match(mp.group(1).strip())
    acc_lhs = SIMPLE_NAME_RE.match(ml.group(1).strip())
    if not acc_prev or not acc_lhs:
        return None
    acc_name_prev = acc_prev.group(1).lower()
    acc_name_lhs = acc_lhs.group(1).lower()
    if acc_name_prev != acc_name_lhs:
        return None
    madd = ADD_ACC_RE.match(ml.group(2).strip())
    if not madd:
        return None
    acc_in_rhs = madd.group(1).lower()
    if acc_in_rhs != acc_name_lhs:
        return None
    expr = madd.group(2).strip()
    lb, ubtxt = rng.split(":", 1)
    expr_sliced = replace_index_with_slice(expr, loop_var, lb, ubtxt)
    if has_loop_var(expr_sliced, loop_var):
        return None
    if expr_sliced == expr:
        return None
    expr_sliced = simplify_section_expr(expr_sliced, decl_bounds={})
    return f"{acc_name_lhs} = sum({expr_sliced})"


def maybe_reduction_product(
    loop_var: str,
    rng: str,
    body_stmt: str,
    prev_stmt: Optional[str],
) -> Optional[str]:
    """Detect product-reduction loop and build replacement."""
    if prev_stmt is None:
        return None
    mp = ASSIGN_RE.match(prev_stmt.strip())
    ml = ASSIGN_RE.match(body_stmt.strip())
    if not mp or not ml:
        return None
    acc_prev = SIMPLE_NAME_RE.match(mp.group(1).strip())
    acc_lhs = SIMPLE_NAME_RE.match(ml.group(1).strip())
    if not acc_prev or not acc_lhs:
        return None
    acc_name_prev = acc_prev.group(1).lower()
    acc_name_lhs = acc_lhs.group(1).lower()
    if acc_name_prev != acc_name_lhs:
        return None
    mmul = MUL_ACC_RE.match(ml.group(2).strip())
    if not mmul:
        return None
    acc_in_rhs = mmul.group(1).lower()
    if acc_in_rhs != acc_name_lhs:
        return None
    expr = mmul.group(2).strip()
    lb, ubtxt = rng.split(":", 1)
    expr_sliced = replace_index_with_slice(expr, loop_var, lb, ubtxt)
    if has_loop_var(expr_sliced, loop_var):
        return None
    if expr_sliced == expr:
        return None
    expr_sliced = simplify_section_expr(expr_sliced, decl_bounds={})
    return f"{acc_name_lhs} = product({expr_sliced})"


def maybe_nested_inner_sum(
    outer_loop_var: str,
    init_stmt: str,
    inner_do_stmt: str,
    inner_body_stmt: str,
    decl_bounds: Dict[str, str],
) -> Optional[str]:
    """Detect y(i)=0; do j...; y(i)=y(i)+expr(j,...); end do -> y(i)=sum(expr(...))."""
    m_init = ASSIGN_RE.match(init_stmt.strip())
    if not m_init:
        return None
    lhs = m_init.group(1).strip()
    rhs0 = m_init.group(2).strip()
    m_lhs = INDEXED_NAME_RE.match(lhs)
    if not m_lhs:
        return None
    if m_lhs.group(2).lower() != outer_loop_var.lower():
        return None
    if not ZERO_LITERAL_RE.match(rhs0):
        return None

    m_do = DO_RE.match(inner_do_stmt.strip())
    if not m_do:
        return None
    inner_var = m_do.group(1)
    inner_rng = build_range(m_do.group(2), m_do.group(3), m_do.group(4))
    lb, ubtxt = split_range(inner_rng)

    m_acc = ASSIGN_RE.match(inner_body_stmt.strip())
    if not m_acc:
        return None
    lhs2 = m_acc.group(1).strip()
    if normalize_expr(lhs2) != normalize_expr(lhs):
        return None
    m_add = re.match(r"^\s*(.+?)\s*\+\s*(.+)$", m_acc.group(2).strip(), re.IGNORECASE)
    if not m_add:
        return None
    if normalize_expr(m_add.group(1)) != normalize_expr(lhs):
        return None
    expr = m_add.group(2).strip()

    expr_s = replace_index_with_slice(expr, inner_var, lb, ubtxt)
    if has_loop_var(expr_s, inner_var):
        return None
    if expr_s == expr:
        return None
    expr_s = simplify_section_expr(expr_s, decl_bounds)
    return f"{lhs} = sum({expr_s})"


def has_nonarray_call(expr: str, array_names: Set[str]) -> bool:
    """Conservative call detector: true when expr has call-like name(...) not known as array."""
    s = strip_quoted_text(expr.lower())
    for m in CALL_LIKE_RE.finditer(s):
        name = m.group(1).lower()
        if name in array_names:
            continue
        return True
    return False


def maybe_loop_to_concurrent_or_forall(
    loop_var: str,
    rng: str,
    body_stmt: str,
    *,
    allow_concurrent: bool,
    allow_forall: bool,
    array_names: Set[str],
) -> Optional[Tuple[str, str]]:
    """Detect simple one-statement loop and suggest one-line DO CONCURRENT or FORALL."""
    m = ASSIGN_RE.match(body_stmt.strip())
    if not m:
        return None
    lhs = m.group(1).strip()
    rhs = m.group(2).strip()
    m_lhs = INDEXED_NAME_RE.match(lhs)
    if not m_lhs:
        return None
    lhs_name = m_lhs.group(1).lower()
    lhs_idx = m_lhs.group(2).lower()
    if lhs_idx != loop_var.lower():
        return None

    # Conservative dependence block: if rhs references lhs at all, skip.
    if re.search(rf"\b{re.escape(lhs_name)}\s*\(", rhs, re.IGNORECASE):
        return None

    if allow_concurrent:
        # Conservative purity proxy for concurrent mode: reject non-array calls.
        if not has_nonarray_call(rhs, array_names):
            return f"do concurrent ({loop_var} = {rng}) {body_stmt.strip()}", "one_line_do_concurrent"

    if allow_forall:
        return f"forall ({loop_var} = {rng}) {body_stmt.strip()}", "one_line_forall"
    return None


def maybe_masked_sum(
    loop_var: str,
    rng: str,
    prev_stmt: Optional[str],
    if_stmt: str,
    body_stmt: str,
) -> Optional[str]:
    """Detect masked sum inside IF block within loop."""
    if prev_stmt is None:
        return None
    mp = ASSIGN_RE.match(prev_stmt.strip())
    if not mp:
        return None
    acc_prev = SIMPLE_NAME_RE.match(mp.group(1).strip())
    if not acc_prev:
        return None
    acc = acc_prev.group(1).lower()
    mif = IF_THEN_RE.match(if_stmt.strip())
    ml = ASSIGN_RE.match(body_stmt.strip())
    if not mif or not ml:
        return None
    lhs = SIMPLE_NAME_RE.match(ml.group(1).strip())
    if not lhs or lhs.group(1).lower() != acc:
        return None
    madd = ADD_ACC_RE.match(ml.group(2).strip())
    if not madd or madd.group(1).lower() != acc:
        return None
    cond = mif.group(1).strip()
    expr = madd.group(2).strip()
    lb, ubtxt = rng.split(":", 1)
    cond_s = replace_index_with_slice(cond, loop_var, lb, ubtxt)
    expr_s = replace_index_with_slice(expr, loop_var, lb, ubtxt)
    if has_loop_var(cond_s, loop_var) or has_loop_var(expr_s, loop_var):
        return None
    if cond_s == cond or expr_s == expr:
        return None
    expr_s = simplify_section_expr(expr_s, decl_bounds={})
    cond_s = simplify_section_expr(cond_s, decl_bounds={})
    return f"{acc} = sum({expr_s}, mask = {cond_s})"


def maybe_count(
    loop_var: str,
    rng: str,
    prev_stmt: Optional[str],
    if_stmt: str,
    body_stmt: str,
) -> Optional[str]:
    """Detect count-in-if pattern and build COUNT replacement."""
    if prev_stmt is None:
        return None
    mp = ASSIGN_RE.match(prev_stmt.strip())
    if not mp:
        return None
    k_prev = SIMPLE_NAME_RE.match(mp.group(1).strip())
    if not k_prev:
        return None
    kname = k_prev.group(1).lower()
    mif = IF_THEN_RE.match(if_stmt.strip())
    ml = ASSIGN_RE.match(body_stmt.strip())
    if not mif or not ml:
        return None
    lhs = SIMPLE_NAME_RE.match(ml.group(1).strip())
    if not lhs or lhs.group(1).lower() != kname:
        return None
    madd1 = ADD_ONE_RE.match(ml.group(2).strip())
    if not madd1 or madd1.group(1).lower() != kname:
        return None
    cond = mif.group(1).strip()
    lb, ubtxt = rng.split(":", 1)
    cond_s = replace_index_with_slice(cond, loop_var, lb, ubtxt)
    if has_loop_var(cond_s, loop_var):
        return None
    if cond_s == cond:
        return None
    cond_s = simplify_section_expr(cond_s, decl_bounds={})
    return f"{kname} = count({cond_s})"


def maybe_count_inline(
    loop_var: str,
    rng: str,
    prev_stmt: Optional[str],
    inline_if_stmt: str,
) -> Optional[str]:
    """Detect one-line IF count pattern and build COUNT replacement."""
    if prev_stmt is None:
        return None
    mp = ASSIGN_RE.match(prev_stmt.strip())
    if not mp:
        return None
    k_prev = SIMPLE_NAME_RE.match(mp.group(1).strip())
    if not k_prev:
        return None
    kname = k_prev.group(1).lower()

    mif = ONE_LINE_IF_RE.match(inline_if_stmt.strip())
    if not mif:
        return None
    cond = mif.group(1).strip()
    body_stmt = mif.group(2).strip()
    # Exclude block IF form handled elsewhere.
    if body_stmt.lower().startswith("then"):
        return None

    ml = ASSIGN_RE.match(body_stmt)
    if not ml:
        return None
    lhs = SIMPLE_NAME_RE.match(ml.group(1).strip())
    if not lhs or lhs.group(1).lower() != kname:
        return None
    madd1 = ADD_ONE_RE.match(ml.group(2).strip())
    if not madd1 or madd1.group(1).lower() != kname:
        return None

    lb, ubtxt = rng.split(":", 1)
    cond_s = replace_index_with_slice(cond, loop_var, lb, ubtxt)
    if has_loop_var(cond_s, loop_var):
        return None
    if cond_s == cond:
        return None
    cond_s = simplify_section_expr(cond_s, decl_bounds={})
    return f"{kname} = count({cond_s})"


def parse_extreme_condition(
    cond: str, loop_var: str
) -> Optional[Tuple[str, str, str, bool]]:
    """Parse min/max compare condition into (kind, scalar_var, array_name, strict)."""
    m = CMP_RE.match(cond.strip())
    if not m:
        return None
    lhs = m.group(1).strip()
    op = m.group(2)
    rhs = m.group(3).strip()
    lhs_idx = INDEXED_NAME_RE.match(lhs)
    rhs_idx = INDEXED_NAME_RE.match(rhs)
    lhs_s = SIMPLE_NAME_RE.match(lhs)
    rhs_s = SIMPLE_NAME_RE.match(rhs)
    strict = op in {"<", ">"}

    # arr(i) ? var
    if lhs_idx and rhs_s and lhs_idx.group(2).lower() == loop_var.lower():
        arr = lhs_idx.group(1)
        var = rhs_s.group(1).lower()
        if op in {"<", "<="}:
            return ("min", var, arr, strict)
        return ("max", var, arr, strict)

    # var ? arr(i)
    if rhs_idx and lhs_s and rhs_idx.group(2).lower() == loop_var.lower():
        arr = rhs_idx.group(1)
        var = lhs_s.group(1).lower()
        if op in {">", ">="}:
            return ("min", var, arr, strict)
        return ("max", var, arr, strict)
    return None


def maybe_extreme_value_inline(
    loop_var: str,
    rng: str,
    inline_if_stmt: str,
    decl_bounds: Dict[str, str],
) -> Optional[str]:
    """Detect one-line IF min/max update and build MINVAL/MAXVAL replacement."""
    mif = ONE_LINE_IF_RE.match(inline_if_stmt.strip())
    if not mif:
        return None
    cond = mif.group(1).strip()
    body_stmt = mif.group(2).strip()
    if body_stmt.lower().startswith("then"):
        return None

    ext = parse_extreme_condition(cond, loop_var)
    if ext is None:
        return None
    kind, scalar, arr, _strict = ext

    ma = ASSIGN_RE.match(body_stmt)
    if not ma:
        return None
    lhs = SIMPLE_NAME_RE.match(ma.group(1).strip())
    rhs = INDEXED_NAME_RE.match(ma.group(2).strip())
    if not lhs or not rhs:
        return None
    if lhs.group(1).lower() != scalar:
        return None
    if rhs.group(1).lower() != arr.lower() or rhs.group(2).lower() != loop_var.lower():
        return None

    sec = simplify_section_expr(f"{arr}({rng})", decl_bounds)
    fn = "minval" if kind == "min" else "maxval"
    return f"{scalar} = {fn}({sec})"


def maybe_extreme_value_block(
    loop_var: str,
    rng: str,
    if_stmt: str,
    body_stmt: str,
    decl_bounds: Dict[str, str],
) -> Optional[str]:
    """Detect IF-block min/max update and build MINVAL/MAXVAL replacement."""
    mif = IF_THEN_RE.match(if_stmt.strip())
    if not mif:
        return None
    ext = parse_extreme_condition(mif.group(1).strip(), loop_var)
    if ext is None:
        return None
    kind, scalar, arr, _strict = ext

    ma = ASSIGN_RE.match(body_stmt.strip())
    if not ma:
        return None
    lhs = SIMPLE_NAME_RE.match(ma.group(1).strip())
    rhs = INDEXED_NAME_RE.match(ma.group(2).strip())
    if not lhs or not rhs:
        return None
    if lhs.group(1).lower() != scalar:
        return None
    if rhs.group(1).lower() != arr.lower() or rhs.group(2).lower() != loop_var.lower():
        return None

    sec = simplify_section_expr(f"{arr}({rng})", decl_bounds)
    fn = "minval" if kind == "min" else "maxval"
    return f"{scalar} = {fn}({sec})"


def maybe_extreme_loc_block(
    loop_var: str,
    rng: str,
    step: Optional[str],
    if_stmt: str,
    stmt_a: str,
    stmt_b: str,
    decl_bounds: Dict[str, str],
) -> Optional[str]:
    """Detect IF-block min/max with index tracking and build MINLOC/MAXLOC replacement."""
    mif = IF_THEN_RE.match(if_stmt.strip())
    if not mif:
        return None
    ext = parse_extreme_condition(mif.group(1).strip(), loop_var)
    if ext is None:
        return None
    kind, scalar, arr, strict = ext
    if not strict:
        return None

    ma = ASSIGN_RE.match(stmt_a.strip())
    mb = ASSIGN_RE.match(stmt_b.strip())
    if not ma or not mb:
        return None

    assign_lhs_1 = SIMPLE_NAME_RE.match(ma.group(1).strip())
    assign_lhs_2 = SIMPLE_NAME_RE.match(mb.group(1).strip())
    if not assign_lhs_1 or not assign_lhs_2:
        return None
    lhs1 = assign_lhs_1.group(1).lower()
    lhs2 = assign_lhs_2.group(1).lower()

    rhs1_idx = INDEXED_NAME_RE.match(ma.group(2).strip())
    rhs2_idx = INDEXED_NAME_RE.match(mb.group(2).strip())
    rhs1_s = SIMPLE_NAME_RE.match(ma.group(2).strip())
    rhs2_s = SIMPLE_NAME_RE.match(mb.group(2).strip())

    idx_var: Optional[str] = None
    # Order A: scalar=arr(i), idx=i
    if (
        lhs1 == scalar
        and rhs1_idx
        and rhs1_idx.group(1).lower() == arr.lower()
        and rhs1_idx.group(2).lower() == loop_var.lower()
        and rhs2_s
        and rhs2_s.group(1).lower() == loop_var.lower()
    ):
        idx_var = lhs2
    # Order B: idx=i, scalar=arr(i)
    elif (
        lhs2 == scalar
        and rhs2_idx
        and rhs2_idx.group(1).lower() == arr.lower()
        and rhs2_idx.group(2).lower() == loop_var.lower()
        and rhs1_s
        and rhs1_s.group(1).lower() == loop_var.lower()
    ):
        idx_var = lhs1
    if idx_var is None:
        return None

    sec = simplify_section_expr(f"{arr}({rng})", decl_bounds)
    vfn = "minval" if kind == "min" else "maxval"
    lfn = "minloc" if kind == "min" else "maxloc"
    val_stmt = f"{scalar} = {vfn}({sec})"

    lb, ub = split_range(rng)
    lb_s = lb.strip()
    if step is None:
        loc_expr = f"{lfn}({sec}, dim=1)"
        if normalize_expr(lb_s) != "1":
            loc_expr = f"({lb_s}) - 1 + {loc_expr}"
    else:
        st = step.strip()
        loc_expr = f"({lb_s}) + ({st})*({lfn}({sec}, dim=1) - 1)"
    idx_stmt = f"{idx_var} = {loc_expr}"
    return f"{val_stmt}; {idx_stmt}"


def is_simple_merge_source(expr: str) -> bool:
    """True when expression is a simple scalar literal/name safe for MERGE sources."""
    s = expr.strip()
    if SIMPLE_NAME_RE.match(s):
        return True
    if NUM_LITERAL_RE.match(s):
        return True
    if LOGICAL_LITERAL_RE.match(s):
        return True
    if CHAR_LITERAL_RE.match(s):
        return True
    return False


def maybe_conditional_set(
    loop_var: str,
    rng: str,
    if_stmt: str,
    true_stmt: str,
    false_stmt: str,
    decl_bounds: Dict[str, str],
) -> Optional[Tuple[str, str]]:
    """Detect elementwise IF/ELSE assignment and suggest MERGE or WHERE/ELSEWHERE."""
    mif = IF_THEN_RE.match(if_stmt.strip())
    if not mif:
        return None
    mt = ASSIGN_RE.match(true_stmt.strip())
    mf = ASSIGN_RE.match(false_stmt.strip())
    if not mt or not mf:
        return None

    mlt = INDEXED_NAME_RE.match(mt.group(1).strip())
    mlf = INDEXED_NAME_RE.match(mf.group(1).strip())
    if not mlt or not mlf:
        return None
    if mlt.group(1).lower() != mlf.group(1).lower():
        return None
    if mlt.group(2).lower() != loop_var.lower() or mlf.group(2).lower() != loop_var.lower():
        return None
    lhs_name = mlt.group(1)

    lb, ubtxt = split_range(rng)
    cond = mif.group(1).strip()
    rhs_t = mt.group(2).strip()
    rhs_f = mf.group(2).strip()

    cond_s = replace_index_with_slice(cond, loop_var, lb, ubtxt)
    rhs_t_s = replace_index_with_slice(rhs_t, loop_var, lb, ubtxt)
    rhs_f_s = replace_index_with_slice(rhs_f, loop_var, lb, ubtxt)
    if has_loop_var(cond_s, loop_var) or has_loop_var(rhs_t_s, loop_var) or has_loop_var(rhs_f_s, loop_var):
        return None
    if cond_s == cond:
        return None

    lhs_expr = f"{lhs_name}({rng})"
    bnd = decl_bounds.get(lhs_name.lower())
    if bnd is not None and normalize_expr(rng) == normalize_expr(bnd):
        lhs_expr = lhs_name

    cond_s = simplify_section_expr(cond_s, decl_bounds)
    rhs_t_s = simplify_section_expr(rhs_t_s, decl_bounds)
    rhs_f_s = simplify_section_expr(rhs_f_s, decl_bounds)

    if is_simple_merge_source(rhs_t_s) and is_simple_merge_source(rhs_f_s):
        return f"{lhs_expr} = merge({rhs_t_s}, {rhs_f_s}, {cond_s})", "elementwise_conditional_merge"

    where_s = "\n".join(
        [
            f"where ({cond_s})",
            f"   {lhs_expr} = {rhs_t_s}",
            "elsewhere",
            f"   {lhs_expr} = {rhs_f_s}",
            "end where",
        ]
    )
    return where_s, "elementwise_conditional_where"


def analyze_unit(unit: xunset.Unit) -> List[Finding]:
    """Analyze one unit for simple loop-to-array opportunities."""
    findings: List[Finding] = []
    body = unit.body
    decl_bounds = parse_rank1_decl_bounds(unit)
    alloc_map: Dict[str, str] = {}
    i = 0
    while i < len(body):
        ln, stmt = body[i]
        alloc_map.update(parse_alloc_shape_spec(stmt))
        mdo = DO_RE.match(stmt.strip())
        if not mdo:
            i += 1
            continue
        loop_var = mdo.group(1)
        lb = mdo.group(2)
        ub = mdo.group(3)
        step = mdo.group(4)
        rng = build_range(lb, ub, step)

        # Form A: one-statement loop body.
        if i + 2 < len(body):
            ln_body, stmt_body = body[i + 1]
            ln_end, stmt_end = body[i + 2]
            if END_DO_RE.match(stmt_end.strip()):
                sugg_elem = maybe_elementwise(loop_var, rng, stmt_body, decl_bounds, alloc_map)
                if sugg_elem is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="elementwise",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=ln,
                            end_line=ln_end,
                            suggestion=sugg_elem,
                        )
                    )
                    i += 3
                    continue
                prev_stmt = body[i - 1][1] if i - 1 >= 0 else None
                start_line = body[i - 1][0] if i - 1 >= 0 else ln
                sugg_sum = maybe_reduction_sum(loop_var, rng, stmt_body, prev_stmt)
                if sugg_sum is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="reduction_sum",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=start_line,
                            end_line=ln_end,
                            suggestion=sugg_sum,
                        )
                    )
                    i += 3
                    continue
                sugg_prod = maybe_reduction_product(loop_var, rng, stmt_body, prev_stmt)
                if sugg_prod is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="reduction_product",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=start_line,
                            end_line=ln_end,
                            suggestion=sugg_prod,
                        )
                    )
                    i += 3
                    continue
                sugg_cnt_inline = maybe_count_inline(loop_var, rng, prev_stmt, stmt_body)
                if sugg_cnt_inline is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="reduction_count",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=start_line,
                            end_line=ln_end,
                            suggestion=sugg_cnt_inline,
                        )
                    )
                    i += 3
                    continue
                sugg_ext_inline = maybe_extreme_value_inline(loop_var, rng, stmt_body, decl_bounds)
                if sugg_ext_inline is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="reduction_minmax_value",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=ln,
                            end_line=ln_end,
                            suggestion=sugg_ext_inline,
                        )
                    )
                    i += 3
                    continue

        # Form A2: two-statement loop body (temp + elementwise assignment).
        if i + 3 < len(body):
            _ln_s1, stmt_s1 = body[i + 1]
            _ln_s2, stmt_s2 = body[i + 2]
            ln_end, stmt_end = body[i + 3]
            if END_DO_RE.match(stmt_end.strip()):
                sugg_temp_elem = maybe_temp_then_elementwise(loop_var, rng, stmt_s1, stmt_s2, decl_bounds, alloc_map)
                if sugg_temp_elem is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="elementwise_inline_temp",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=ln,
                            end_line=ln_end,
                            suggestion=sugg_temp_elem,
                        )
                    )
                    i += 4
                    continue

        # Form A3: nested inner reduction inside outer loop:
        # do i=...
        #    y(i)=0
        #    do j=...
        #       y(i)=y(i)+expr
        #    end do
        # end do
        if i + 5 < len(body):
            _ln_init, stmt_init = body[i + 1]
            _ln_ido, stmt_ido = body[i + 2]
            _ln_ibody, stmt_ibody = body[i + 3]
            ln_iend, stmt_iend = body[i + 4]
            ln_oend, stmt_oend = body[i + 5]
            if END_DO_RE.match(stmt_iend.strip()) and END_DO_RE.match(stmt_oend.strip()):
                sugg_nested = maybe_nested_inner_sum(loop_var, stmt_init, stmt_ido, stmt_ibody, decl_bounds)
                if sugg_nested is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="nested_reduction_sum",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=_ln_init,
                            end_line=ln_iend,
                            suggestion=sugg_nested,
                        )
                    )
                    i += 6
                    continue

        # Form D: IF/ELSE block inside loop body:
        # do ...
        #    if (cond) then
        #       lhs(i) = tsource
        #    else
        #       lhs(i) = fsource
        #    end if
        # end do
        if i + 6 < len(body):
            _ln_if, stmt_if = body[i + 1]
            _ln_t, stmt_t = body[i + 2]
            _ln_else, stmt_else = body[i + 3]
            _ln_f, stmt_f = body[i + 4]
            _ln_eif, stmt_eif = body[i + 5]
            ln_end, stmt_end = body[i + 6]
            if (
                IF_THEN_RE.match(stmt_if.strip())
                and ELSE_RE.match(stmt_else.strip())
                and END_IF_RE.match(stmt_eif.strip())
                and END_DO_RE.match(stmt_end.strip())
            ):
                sugg_cond = maybe_conditional_set(loop_var, rng, stmt_if, stmt_t, stmt_f, decl_bounds)
                if sugg_cond is not None:
                    suggestion, rule = sugg_cond
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule=rule,
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=ln,
                            end_line=ln_end,
                            suggestion=suggestion,
                        )
                    )
                    i += 7
                    continue

        # Form B: IF-block inside loop body:
        # do ...
        #    if (cond) then
        #       stmt
        #    end if
        # end do
        if i + 4 < len(body):
            _ln_if, stmt_if = body[i + 1]
            _ln_inside, stmt_inside = body[i + 2]
            _ln_eif, stmt_eif = body[i + 3]
            ln_end, stmt_end = body[i + 4]
            if IF_THEN_RE.match(stmt_if.strip()) and END_IF_RE.match(stmt_eif.strip()) and END_DO_RE.match(stmt_end.strip()):
                prev_stmt = body[i - 1][1] if i - 1 >= 0 else None
                start_line = body[i - 1][0] if i - 1 >= 0 else ln
                sugg_msum = maybe_masked_sum(loop_var, rng, prev_stmt, stmt_if, stmt_inside)
                if sugg_msum is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="reduction_masked_sum",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=start_line,
                            end_line=ln_end,
                            suggestion=sugg_msum,
                        )
                    )
                    i += 5
                    continue
                sugg_cnt = maybe_count(loop_var, rng, prev_stmt, stmt_if, stmt_inside)
                if sugg_cnt is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="reduction_count",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=start_line,
                            end_line=ln_end,
                            suggestion=sugg_cnt,
                        )
                    )
                    i += 5
                    continue
                sugg_ext = maybe_extreme_value_block(loop_var, rng, stmt_if, stmt_inside, decl_bounds)
                if sugg_ext is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="reduction_minmax_value",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=ln,
                            end_line=ln_end,
                            suggestion=sugg_ext,
                        )
                    )
                    i += 5
                    continue
        # Form C: IF-block with two statements inside loop body:
        # do ...
        #    if (cond) then
        #       stmt_a
        #       stmt_b
        #    end if
        # end do
        if i + 5 < len(body):
            _ln_if, stmt_if = body[i + 1]
            _ln_a, stmt_a = body[i + 2]
            _ln_b, stmt_b = body[i + 3]
            _ln_eif, stmt_eif = body[i + 4]
            ln_end, stmt_end = body[i + 5]
            if IF_THEN_RE.match(stmt_if.strip()) and END_IF_RE.match(stmt_eif.strip()) and END_DO_RE.match(stmt_end.strip()):
                sugg_loc = maybe_extreme_loc_block(loop_var, rng, step, stmt_if, stmt_a, stmt_b, decl_bounds)
                if sugg_loc is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="reduction_minmax_loc",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=ln,
                            end_line=ln_end,
                            suggestion=sugg_loc,
                        )
                    )
                    i += 6
                    continue
        i += 1
    return findings


def analyze_unit_concurrent_forall(
    unit: xunset.Unit,
    *,
    allow_concurrent: bool,
    allow_forall: bool,
) -> List[Finding]:
    """Analyze one unit for one-line DO CONCURRENT / FORALL rewrites."""
    findings: List[Finding] = []
    body = unit.body
    decl_bounds = parse_rank1_decl_bounds(unit)
    array_names = collect_rank1_array_names(unit)
    i = 0
    while i < len(body):
        ln, stmt = body[i]
        mdo = DO_RE.match(stmt.strip())
        if not mdo:
            i += 1
            continue
        if i + 2 >= len(body):
            i += 1
            continue
        _ln_body, stmt_body = body[i + 1]
        ln_end, stmt_end = body[i + 2]
        if not END_DO_RE.match(stmt_end.strip()):
            i += 1
            continue
        loop_var = mdo.group(1)
        rng = build_range(mdo.group(2), mdo.group(3), mdo.group(4))
        sug = maybe_loop_to_concurrent_or_forall(
            loop_var,
            rng,
            stmt_body,
            allow_concurrent=allow_concurrent,
            allow_forall=allow_forall,
            array_names=array_names,
        )
        if sug is not None:
            suggestion, rule = sug
            findings.append(
                Finding(
                    path=unit.path,
                    rule=rule,
                    unit_kind=unit.kind,
                    unit_name=unit.name,
                    start_line=ln,
                    end_line=ln_end,
                    suggestion=suggestion,
                )
            )
            i += 3
            continue
        # Also allow converted expression forms from existing elementwise detector
        # before wrapping, if the one-line body itself is not directly eligible.
        sugg_elem = maybe_elementwise(loop_var, rng, stmt_body, decl_bounds, {})
        if sugg_elem is not None:
            sug2 = maybe_loop_to_concurrent_or_forall(
                loop_var,
                rng,
                sugg_elem,
                allow_concurrent=allow_concurrent,
                allow_forall=allow_forall,
                array_names=array_names,
            )
            if sug2 is not None:
                suggestion, rule = sug2
                findings.append(
                    Finding(
                        path=unit.path,
                        rule=rule,
                        unit_kind=unit.kind,
                        unit_name=unit.name,
                        start_line=ln,
                        end_line=ln_end,
                        suggestion=suggestion,
                    )
                )
                i += 3
                continue
        i += 1
    return findings


def analyze_file(path: Path) -> List[Finding]:
    """Analyze one file."""
    infos, any_missing = fscan.load_source_files([path])
    if not infos or any_missing:
        return []
    finfo = infos[0]
    out: List[Finding] = []
    for unit in xunset.collect_units(finfo):
        out.extend(analyze_unit(unit))
    return out


def analyze_file_concurrent_forall(
    path: Path,
    *,
    allow_concurrent: bool,
    allow_forall: bool,
) -> List[Finding]:
    """Analyze one source file for one-line DO CONCURRENT / FORALL rewrites."""
    infos, any_missing = fscan.load_source_files([path])
    if not infos or any_missing:
        return []
    finfo = infos[0]
    out: List[Finding] = []
    for unit in xunset.collect_units(finfo):
        out.extend(
            analyze_unit_concurrent_forall(
                unit,
                allow_concurrent=allow_concurrent,
                allow_forall=allow_forall,
            )
        )
    return out


def parse_decl_entities(chunk_rhs: str) -> List[Tuple[str, str]]:
    """Parse declaration RHS into (name, raw_chunk)."""
    out: List[Tuple[str, str]] = []
    for chunk in split_top_level_commas(chunk_rhs):
        txt = chunk.strip()
        if not txt:
            continue
        m = re.match(r"^([a-z][a-z0-9_]*)", txt, re.IGNORECASE)
        if not m:
            continue
        out.append((m.group(1).lower(), txt))
    return out


def remove_unused_locals_from_lines(lines: List[str], path: Path) -> Tuple[List[str], List[str]]:
    """Remove clearly unused local scalar declaration entities; return (lines, removed_names)."""
    temp_text = "".join(lines)
    # Parse via scanner on synthetic lines.
    parsed_lines = temp_text.splitlines()
    finfo = fscan.SourceFileInfo(
        path=path,
        lines=parsed_lines,
        parsed_lines=parsed_lines,
        procedures=fscan.parse_procedures(parsed_lines),
        defined_modules=set(),
        used_modules=set(),
        generic_interfaces={},
    )
    units = xunset.collect_units(finfo)
    if not units:
        return lines, []

    remove_by_line: Dict[int, Set[str]] = {}
    removed_names: List[str] = []

    for unit in units:
        declared: Dict[str, int] = {}
        decl_chunks_by_line: Dict[int, List[Tuple[str, str]]] = {}
        used: Set[str] = set()

        for ln, stmt in unit.body:
            low = stmt.strip().lower()
            if not low:
                continue
            if TYPE_DECL_RE.match(low) and "::" in low:
                attrs = low.split("::", 1)[0]
                # Keep declaration classes that are risky or special.
                if any(
                    a in attrs
                    for a in ("parameter", "allocatable", "pointer", "target", "save", "intent", "optional")
                ):
                    continue
                rhs = low.split("::", 1)[1]
                ents = parse_decl_entities(rhs)
                if not ents:
                    continue
                kept_ents: List[Tuple[str, str]] = []
                for n, raw_chunk in ents:
                    # Conservative: only simple scalar entities without init/dims/pointer-assoc.
                    if "(" in raw_chunk or "=" in raw_chunk or "=>" in raw_chunk:
                        continue
                    declared[n] = ln
                    kept_ents.append((n, raw_chunk))
                if kept_ents:
                    decl_chunks_by_line[ln] = kept_ents
                continue

            txt = strip_quoted_text(low)
            for m in IDENT_RE.finditer(txt):
                used.add(m.group(1).lower())

        # Dummies should never be removed.
        used.update(unit.dummy_names)
        for n, ln in declared.items():
            if n in used:
                continue
            remove_by_line.setdefault(ln, set()).add(n)
            removed_names.append(f"{unit.name}:{n}")

    if not remove_by_line:
        return lines, []

    # Apply declaration-entity removals by physical line.
    new_lines = list(lines)
    for ln in sorted(remove_by_line.keys(), reverse=True):
        idx = ln - 1
        if idx < 0 or idx >= len(new_lines):
            continue
        raw = new_lines[idx]
        eol = ""
        body = raw
        if body.endswith("\r\n"):
            body, eol = body[:-2], "\r\n"
        elif body.endswith("\n"):
            body, eol = body[:-1], "\n"
        code, comment = split_code_comment(body)
        low = code.strip().lower()
        if "::" not in low or not TYPE_DECL_RE.match(low):
            continue
        left, rhs = code.split("::", 1)
        ents = parse_decl_entities(rhs)
        keep_raw: List[str] = []
        for n, raw_chunk in ents:
            if n in remove_by_line.get(ln, set()):
                continue
            keep_raw.append(raw_chunk.strip())
        if not keep_raw:
            # Drop full declaration line.
            new_lines.pop(idx)
            continue
        new_code = f"{left.strip()} :: {', '.join(keep_raw)}"
        prefix = re.match(r"^\s*", body).group(0) if body else ""
        new_lines[idx] = f"{prefix}{new_code}{comment}{eol}"

    return new_lines, sorted(set(removed_names))


def annotate_file(path: Path, findings: List[Finding]) -> Tuple[int, Optional[Path]]:
    """Insert begin/end markers and replacement comment around suggested blocks."""
    if not findings:
        return 0, None
    lines = path.read_text(encoding="utf-8").splitlines(keepends=True)
    inserted = 0
    for f in sorted(findings, key=lambda x: (x.start_line, x.end_line), reverse=True):
        sidx = f.start_line - 1
        eidx = f.end_line - 1
        if sidx < 0 or eidx < 0 or sidx >= len(lines) or eidx >= len(lines) or sidx > eidx:
            continue
        raw_s = lines[sidx]
        raw_e = lines[eidx]
        indent_s = re.match(r"^\s*", raw_s).group(0) if raw_s else ""
        indent_e = re.match(r"^\s*", raw_e).group(0) if raw_e else ""
        eol_s = "\r\n" if raw_s.endswith("\r\n") else ("\n" if raw_s.endswith("\n") else "\n")
        eol_e = "\r\n" if raw_e.endswith("\r\n") else ("\n" if raw_e.endswith("\n") else "\n")

        begin_msg = f"{indent_s}{BEGIN_TAG}{eol_s}"
        end_msg = f"{indent_e}{END_TAG}{eol_e}"
        sug_lines = f.suggestion.splitlines() if "\n" in f.suggestion else [f.suggestion]
        sugg_msgs = [f"{indent_e}! {sl} !! suggested replacement by xarray.py{eol_e}" for sl in sug_lines]

        # Skip if already annotated around this range.
        if sidx - 1 >= 0 and lines[sidx - 1].strip().lower() == BEGIN_TAG.lower():
            continue
        lines.insert(eidx + 1, end_msg)
        insert_at = eidx + 2
        for sm in sugg_msgs:
            lines.insert(insert_at, sm)
            insert_at += 1
        lines.insert(sidx, begin_msg)
        inserted += 2 + len(sugg_msgs)

    if inserted == 0:
        return 0, None
    backup = make_backup_path(path)
    shutil.copy2(path, backup)
    path.write_text("".join(lines), encoding="utf-8")
    return inserted, backup


def apply_fix_file(
    path: Path, findings: List[Finding], *, annotate: bool = False
) -> Tuple[int, Optional[Path], List[str]]:
    """Replace suggested blocks with array-operation statements and prune unused locals."""
    if not findings:
        return 0, None, []
    lines = path.read_text(encoding="utf-8").splitlines(keepends=True)
    changed = 0
    for f in sorted(findings, key=lambda x: (x.start_line, x.end_line), reverse=True):
        sidx = f.start_line - 1
        eidx = f.end_line - 1
        if sidx < 0 or eidx < 0 or sidx >= len(lines) or eidx >= len(lines) or sidx > eidx:
            continue
        raw_s = lines[sidx]
        eol = "\r\n" if raw_s.endswith("\r\n") else ("\n" if raw_s.endswith("\n") else "\n")
        indent = re.match(r"^\s*", raw_s).group(0) if raw_s else ""
        sug_lines = f.suggestion.splitlines() if "\n" in f.suggestion else [f.suggestion]
        repl_lines: List[str] = []
        for j, sl in enumerate(sug_lines):
            suffix = f"  {CHANGED_TAG}" if (annotate and j == len(sug_lines) - 1) else ""
            repl_lines.append(f"{indent}{sl}{suffix}{eol}")
        lines[sidx : eidx + 1] = repl_lines
        changed += 1
    if changed == 0:
        return 0, None, []
    lines, removed_locals = remove_unused_locals_from_lines(lines, path)
    backup = make_backup_path(path)
    shutil.copy2(path, backup)
    path.write_text("".join(lines), encoding="utf-8")
    return changed, backup, removed_locals


def main() -> int:
    """Run xarray advisory and optional annotation mode."""
    parser = argparse.ArgumentParser(
        description="Suggest replacing simple Fortran loops with array operations"
    )
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--verbose", action="store_true", help="Print full replacement suggestions")
    parser.add_argument("--fix", action="store_true", help="Apply suggested replacements in-place")
    parser.add_argument("--annotate", action="store_true", help="Insert annotated suggestion blocks")
    parser.add_argument("--diff", action="store_true", help="With --fix, print unified diffs for changed files")
    parser.add_argument(
        "--concurrent",
        action="store_true",
        help="Use one-line DO CONCURRENT rewrite mode for eligible simple loops",
    )
    parser.add_argument(
        "--forall",
        action="store_true",
        help="Use one-line FORALL rewrite mode for eligible simple loops",
    )
    args = parser.parse_args()
    if args.diff and not args.fix:
        print("--diff requires --fix.")
        return 2

    files = choose_files(args.fortran_files, args.exclude)
    if not files:
        print("No source files remain after applying --exclude filters.")
        return 2

    findings: List[Finding] = []
    for p in files:
        if args.concurrent or args.forall:
            findings.extend(
                analyze_file_concurrent_forall(
                    p,
                    allow_concurrent=args.concurrent,
                    allow_forall=args.forall,
                )
            )
        else:
            findings.extend(analyze_file(p))

    if not findings:
        print("No array-operation replacement candidates found.")
        return 0

    findings.sort(key=lambda f: (f.path.name.lower(), f.start_line, f.end_line))
    print(f"{len(findings)} array-operation replacement candidate(s).")
    for f in findings:
        print(
            f"{f.path.name}:{f.start_line}-{f.end_line} {f.rule} "
            f"{f.unit_kind} {f.unit_name}"
        )
        if args.verbose:
            print(f"  suggest: {f.suggestion}")

    if args.fix:
        by_file: Dict[Path, List[Finding]] = {}
        for f in findings:
            by_file.setdefault(f.path, []).append(f)
        touched = 0
        total = 0
        for p in sorted(by_file.keys(), key=lambda x: x.name.lower()):
            before = p.read_text(encoding="utf-8")
            n, backup, removed_locals = apply_fix_file(p, by_file[p], annotate=args.annotate)
            total += n
            if n > 0:
                touched += 1
                print(f"\nFixed {p.name}: replaced {n} block(s), backup {backup.name if backup else '(none)'}")
                if args.verbose and removed_locals:
                    print(f"  removed unused locals: {', '.join(removed_locals)}")
                if args.diff:
                    after = p.read_text(encoding="utf-8")
                    diff_lines = difflib.unified_diff(
                        before.splitlines(),
                        after.splitlines(),
                        fromfile=f"a/{p.name}",
                        tofile=f"b/{p.name}",
                        lineterm="",
                    )
                    print("")
                    for line in diff_lines:
                        print(line)
            elif args.verbose:
                print(f"\nNo fixes applied in {p.name}")
        print(f"\n--fix summary: files changed {touched}, replaced {total}")
    elif args.annotate:
        by_file: Dict[Path, List[Finding]] = {}
        for f in findings:
            by_file.setdefault(f.path, []).append(f)
        total = 0
        touched = 0
        for p in sorted(by_file.keys(), key=lambda x: x.name.lower()):
            n, backup = annotate_file(p, by_file[p])
            total += n
            if n > 0:
                touched += 1
                print(f"\nAnnotated {p.name}: inserted {n}, backup {backup.name if backup else '(none)'}")
            elif args.verbose:
                print(f"\nNo annotations inserted in {p.name}")
        print(f"\n--annotate summary: files changed {touched}, inserted {total}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
