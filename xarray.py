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
import fortran_build as fbuild
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
INDEXED_FIRST_DIM_RE = re.compile(
    r"^\s*([a-z][a-z0-9_]*)\s*\(\s*([a-z][a-z0-9_]*)\s*,\s*(.+)\)\s*$",
    re.IGNORECASE,
)
INDEXED_SECOND_DIM_RE = re.compile(
    r"^\s*([a-z][a-z0-9_]*)\s*\(\s*(.+)\s*,\s*([a-z][a-z0-9_]*)\s*\)\s*$",
    re.IGNORECASE,
)
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
EQ_RE = re.compile(r"^\s*(.+?)\s*(==|\.eq\.)\s*(.+?)\s*$", re.IGNORECASE)
IDENT_RE = re.compile(r"\b([a-z][a-z0-9_]*)\b", re.IGNORECASE)
CALL_LIKE_RE = re.compile(r"\b([a-z][a-z0-9_]*)\s*\(", re.IGNORECASE)
INDEXED_CONST_RE = re.compile(r"^\s*([a-z][a-z0-9_]*)\s*\(\s*(\d+)\s*\)\s*$", re.IGNORECASE)
PRINT_PREFIX_RE = re.compile(r"^\s*print\s*\*\s*,\s*(.+)\s*$", re.IGNORECASE)
READ_PREFIX_RE = re.compile(r"^\s*read\s*\*\s*,\s*(.+)\s*$", re.IGNORECASE)
PRINT_STMT_RE = re.compile(r"^\s*print\s+(.+)$", re.IGNORECASE)
LOOP_ASSIGN_RE = re.compile(r"^\s*([a-z][a-z0-9_]*)\s*=\s*(.+)$", re.IGNORECASE)
RANDOM_CALL_RE = re.compile(r"^\s*call\s+random_number\s*\(\s*(.+)\s*\)\s*$", re.IGNORECASE)

# Conservative set of elemental intrinsics that are safe in array expressions.
ELEMENTAL_INTRINSICS = {
    "abs",
    "acos",
    "asin",
    "atan",
    "aint",
    "anint",
    "ceiling",
    "cmplx",
    "conjg",
    "cos",
    "cosh",
    "dble",
    "dim",
    "dprod",
    "exp",
    "floor",
    "int",
    "log",
    "log10",
    "max",
    "min",
    "mod",
    "modulo",
    "nint",
    "real",
    "sign",
    "sin",
    "sinh",
    "sqrt",
    "tan",
    "tanh",
}


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


def is_continuation_only_line(line: str) -> bool:
    """True when a physical line is a free-form continuation starter (`& ...`)."""
    code, _comment = split_code_comment(line)
    return bool(re.match(r"^\s*&", code))


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
    """Rewrite occurrences of name(idx_var [+/- k]) to sliced sections."""

    def shift_bound(bound: str, delta: int) -> str:
        b = bound.strip()
        if delta == 0:
            return b
        # Simplify variable +/- integer forms, e.g. n-1 plus 1 -> n.
        b_nos = re.sub(r"\s+", "", b)
        m_aff = re.fullmatch(r"([a-z][a-z0-9_]*)([+-]\d+)?", b_nos, re.IGNORECASE)
        if m_aff:
            base = m_aff.group(1)
            offs = int(m_aff.group(2) or "0")
            total = offs + delta
            if total == 0:
                return base
            if total > 0:
                return f"{base}+{total}"
            return f"{base}{total}"
        if re.fullmatch(r"[+-]?\d+", b):
            return str(int(b) + delta)
        if delta > 0:
            return f"{b}+{delta}"
        return f"{b}-{abs(delta)}"

    def shift_ub_with_step(ubtxt: str, delta: int) -> str:
        u = ubtxt.strip()
        if ":" not in u:
            return shift_bound(u, delta)
        ub_part, step_part = u.split(":", 1)
        return f"{shift_bound(ub_part, delta)}:{step_part.strip()}"

    def section(delta: int) -> str:
        sec = f"{shift_bound(lb, delta)}:{shift_ub_with_step(ub, delta)}"
        # Final cleanup for simple canceling offset forms.
        sec = re.sub(r"\b([a-z][a-z0-9_]*)-1\+1\b", r"\1", sec, flags=re.IGNORECASE)
        sec = re.sub(r"\b([a-z][a-z0-9_]*)\+1-1\b", r"\1", sec, flags=re.IGNORECASE)
        sec = re.sub(r"\b([a-z][a-z0-9_]*)-0\b", r"\1", sec, flags=re.IGNORECASE)
        sec = re.sub(r"\b([a-z][a-z0-9_]*)\+0\b", r"\1", sec, flags=re.IGNORECASE)
        return sec

    out = expr
    # name(i +/- k)
    pat_idx_pm = re.compile(
        rf"\b([a-z][a-z0-9_]*)\s*\(\s*{re.escape(idx_var)}\s*([+-])\s*(\d+)\s*\)",
        re.IGNORECASE,
    )
    out = pat_idx_pm.sub(
        lambda m: f"{m.group(1)}({section(int(m.group(3)) if m.group(2) == '+' else -int(m.group(3)))})",
        out,
    )
    # name(k + i)
    pat_k_plus_idx = re.compile(
        rf"\b([a-z][a-z0-9_]*)\s*\(\s*(\d+)\s*\+\s*{re.escape(idx_var)}\s*\)",
        re.IGNORECASE,
    )
    out = pat_k_plus_idx.sub(lambda m: f"{m.group(1)}({section(int(m.group(2)))})", out)
    # name(i)
    pat_idx = re.compile(
        rf"\b([a-z][a-z0-9_]*)\s*\(\s*{re.escape(idx_var)}\s*\)",
        re.IGNORECASE,
    )
    out = pat_idx.sub(lambda m: f"{m.group(1)}({section(0)})", out)
    return out


def shifted_bound_expr(bound: str, delta: int) -> str:
    """Shift a bound expression by integer delta with small simplifications."""
    b = bound.strip()
    if delta == 0:
        return b
    b_nos = re.sub(r"\s+", "", b)
    m_aff = re.fullmatch(r"([a-z][a-z0-9_]*)([+-]\d+)?", b_nos, re.IGNORECASE)
    if m_aff:
        base = m_aff.group(1)
        offs = int(m_aff.group(2) or "0")
        total = offs + delta
        if total == 0:
            return base
        if total > 0:
            return f"{base}+{total}"
        return f"{base}{total}"
    if re.fullmatch(r"[+-]?\d+", b):
        return str(int(b) + delta)
    if delta > 0:
        return f"{b}+{delta}"
    return f"{b}-{abs(delta)}"


def shifted_ub_with_step_expr(ubtxt: str, delta: int) -> str:
    """Shift ub[:step] text by delta on ub only."""
    u = ubtxt.strip()
    if ":" not in u:
        return shifted_bound_expr(u, delta)
    ub_part, step_part = u.split(":", 1)
    return f"{shifted_bound_expr(ub_part, delta)}:{step_part.strip()}"


def shifted_section_expr(lb: str, ub: str, delta: int) -> str:
    """Build shifted section lb:ub with light algebra cleanup."""
    sec = f"{shifted_bound_expr(lb, delta)}:{shifted_ub_with_step_expr(ub, delta)}"
    sec = re.sub(r"\b([a-z][a-z0-9_]*)-1\+1\b", r"\1", sec, flags=re.IGNORECASE)
    sec = re.sub(r"\b([a-z][a-z0-9_]*)\+1-1\b", r"\1", sec, flags=re.IGNORECASE)
    sec = re.sub(r"\b([a-z][a-z0-9_]*)-0\b", r"\1", sec, flags=re.IGNORECASE)
    sec = re.sub(r"\b([a-z][a-z0-9_]*)\+0\b", r"\1", sec, flags=re.IGNORECASE)
    return sec


def parse_affine_loop_index(arg: str, loop_var: str) -> Optional[int]:
    """Parse arg as loop_var affine form and return integer delta."""
    s = re.sub(r"\s+", "", arg.lower())
    lv = loop_var.lower()
    if s == lv:
        return 0
    m = re.fullmatch(rf"{re.escape(lv)}([+-]\d+)", s)
    if m:
        return int(m.group(1))
    m = re.fullmatch(rf"(\d+)\+{re.escape(lv)}", s)
    if m:
        return int(m.group(1))
    return None


def find_matching_paren(text: str, open_pos: int) -> int:
    """Find matching ')' position for '(' at open_pos, or -1."""
    depth = 0
    in_single = False
    in_double = False
    i = open_pos
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


def replace_affine_index_anydim(
    expr: str, loop_var: str, lb: str, ub: str, array_names: Optional[Set[str]] = None
) -> str:
    """Rewrite name(..., i±k, ...) args to shifted slices for any dimension."""
    out: List[str] = []
    i = 0
    n = len(expr)
    while i < n:
        m = re.match(r"[a-z][a-z0-9_]*", expr[i:], re.IGNORECASE)
        if not m:
            out.append(expr[i])
            i += 1
            continue
        name = m.group(0)
        j = i + len(name)
        while j < n and expr[j].isspace():
            j += 1
        if j >= n or expr[j] != "(":
            out.append(expr[i])
            i += 1
            continue
        if array_names is not None and name.lower() not in array_names:
            out.append(expr[i])
            i += 1
            continue
        close = find_matching_paren(expr, j)
        if close < 0:
            out.append(expr[i])
            i += 1
            continue
        inside = expr[j + 1 : close]
        args = split_top_level_commas(inside)
        changed = False
        new_args: List[str] = []
        for a in args:
            delta = parse_affine_loop_index(a, loop_var)
            if delta is None:
                new_args.append(a.strip())
            else:
                new_args.append(shifted_section_expr(lb, ub, delta))
                changed = True
        if changed:
            out.append(f"{name}({', '.join(new_args)})")
            i = close + 1
            continue
        out.append(expr[i])
        i += 1
    return "".join(out)


def parse_lhs_indexed_by_loop(lhs: str, loop_var: str) -> Optional[Tuple[str, List[str], int]]:
    """Parse lhs name(args...) with exactly one subscript equal to loop_var."""
    m = re.match(r"^\s*([a-z][a-z0-9_]*)\s*\((.*)\)\s*$", lhs, re.IGNORECASE)
    if not m:
        return None
    name = m.group(1)
    args = split_top_level_commas(m.group(2))
    if not args:
        return None
    loop_positions = [k for k, a in enumerate(args) if a.strip().lower() == loop_var.lower()]
    if len(loop_positions) != 1:
        return None
    pos = loop_positions[0]
    for k, a in enumerate(args):
        if k == pos:
            continue
        if re.search(rf"\b{re.escape(loop_var)}\b", a, re.IGNORECASE):
            return None
    return name, [a.strip() for a in args], pos


def replace_first_index_with_slice(expr: str, idx_var: str, lb: str, ub: str) -> str:
    """Rewrite name(idx_var [+/- k], tail...) to name(slice, tail...)."""

    def shift_bound(bound: str, delta: int) -> str:
        b = bound.strip()
        if delta == 0:
            return b
        b_nos = re.sub(r"\s+", "", b)
        m_aff = re.fullmatch(r"([a-z][a-z0-9_]*)([+-]\d+)?", b_nos, re.IGNORECASE)
        if m_aff:
            base = m_aff.group(1)
            offs = int(m_aff.group(2) or "0")
            total = offs + delta
            if total == 0:
                return base
            if total > 0:
                return f"{base}+{total}"
            return f"{base}{total}"
        if re.fullmatch(r"[+-]?\d+", b):
            return str(int(b) + delta)
        if delta > 0:
            return f"{b}+{delta}"
        return f"{b}-{abs(delta)}"

    def shift_ub_with_step(ubtxt: str, delta: int) -> str:
        u = ubtxt.strip()
        if ":" not in u:
            return shift_bound(u, delta)
        ub_part, step_part = u.split(":", 1)
        return f"{shift_bound(ub_part, delta)}:{step_part.strip()}"

    def section(delta: int) -> str:
        sec = f"{shift_bound(lb, delta)}:{shift_ub_with_step(ub, delta)}"
        sec = re.sub(r"\b([a-z][a-z0-9_]*)-1\+1\b", r"\1", sec, flags=re.IGNORECASE)
        sec = re.sub(r"\b([a-z][a-z0-9_]*)\+1-1\b", r"\1", sec, flags=re.IGNORECASE)
        return sec

    out = expr
    # name(i +/- k, tail)
    pat_idx_pm = re.compile(
        rf"\b([a-z][a-z0-9_]*)\s*\(\s*{re.escape(idx_var)}\s*([+-])\s*(\d+)\s*,\s*([^)]*)\)",
        re.IGNORECASE,
    )
    out = pat_idx_pm.sub(
        lambda m: (
            f"{m.group(1)}("
            f"{section(int(m.group(3)) if m.group(2) == '+' else -int(m.group(3)))}, "
            f"{m.group(4).strip()})"
        ),
        out,
    )
    # name(k + i, tail)
    pat_k_plus_idx = re.compile(
        rf"\b([a-z][a-z0-9_]*)\s*\(\s*(\d+)\s*\+\s*{re.escape(idx_var)}\s*,\s*([^)]*)\)",
        re.IGNORECASE,
    )
    out = pat_k_plus_idx.sub(
        lambda m: f"{m.group(1)}({section(int(m.group(2)))}, {m.group(3).strip()})",
        out,
    )
    # name(i, tail)
    pat_idx = re.compile(
        rf"\b([a-z][a-z0-9_]*)\s*\(\s*{re.escape(idx_var)}\s*,\s*([^)]*)\)",
        re.IGNORECASE,
    )
    out = pat_idx.sub(lambda m: f"{m.group(1)}({section(0)}, {m.group(2).strip()})", out)
    return out


def replace_second_index_with_slice(expr: str, idx_var: str, lb: str, ub: str) -> str:
    """Rewrite name(head..., idx_var [+/- k]) to name(head..., slice)."""

    def shift_bound(bound: str, delta: int) -> str:
        b = bound.strip()
        if delta == 0:
            return b
        b_nos = re.sub(r"\s+", "", b)
        m_aff = re.fullmatch(r"([a-z][a-z0-9_]*)([+-]\d+)?", b_nos, re.IGNORECASE)
        if m_aff:
            base = m_aff.group(1)
            offs = int(m_aff.group(2) or "0")
            total = offs + delta
            if total == 0:
                return base
            if total > 0:
                return f"{base}+{total}"
            return f"{base}{total}"
        if re.fullmatch(r"[+-]?\d+", b):
            return str(int(b) + delta)
        if delta > 0:
            return f"{b}+{delta}"
        return f"{b}-{abs(delta)}"

    def shift_ub_with_step(ubtxt: str, delta: int) -> str:
        u = ubtxt.strip()
        if ":" not in u:
            return shift_bound(u, delta)
        ub_part, step_part = u.split(":", 1)
        return f"{shift_bound(ub_part, delta)}:{step_part.strip()}"

    def section(delta: int) -> str:
        sec = f"{shift_bound(lb, delta)}:{shift_ub_with_step(ub, delta)}"
        sec = re.sub(r"\b([a-z][a-z0-9_]*)-1\+1\b", r"\1", sec, flags=re.IGNORECASE)
        sec = re.sub(r"\b([a-z][a-z0-9_]*)\+1-1\b", r"\1", sec, flags=re.IGNORECASE)
        return sec

    out = expr
    # name(head, i +/- k)
    pat_idx_pm = re.compile(
        rf"\b([a-z][a-z0-9_]*)\s*\(\s*([^,]+?)\s*,\s*{re.escape(idx_var)}\s*([+-])\s*(\d+)\s*\)",
        re.IGNORECASE,
    )
    out = pat_idx_pm.sub(
        lambda m: (
            f"{m.group(1)}("
            f"{m.group(2).strip()}, "
            f"{section(int(m.group(4)) if m.group(3) == '+' else -int(m.group(4)))})"
        ),
        out,
    )
    # name(head, k + i)
    pat_k_plus_idx = re.compile(
        rf"\b([a-z][a-z0-9_]*)\s*\(\s*([^,]+?)\s*,\s*(\d+)\s*\+\s*{re.escape(idx_var)}\s*\)",
        re.IGNORECASE,
    )
    out = pat_k_plus_idx.sub(
        lambda m: f"{m.group(1)}({m.group(2).strip()}, {section(int(m.group(3)))})",
        out,
    )
    # name(head, i)
    pat_idx = re.compile(
        rf"\b([a-z][a-z0-9_]*)\s*\(\s*([^,]+?)\s*,\s*{re.escape(idx_var)}\s*\)",
        re.IGNORECASE,
    )
    out = pat_idx.sub(lambda m: f"{m.group(1)}({m.group(2).strip()}, {section(0)})", out)
    return out


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


def range_upper_if_one_based(rng: str) -> Optional[str]:
    """Return upper bound text for 1-based ranges like 1:n or 1:n:s, else None."""
    parts = rng.split(":", 2)
    if len(parts) < 2:
        return None
    lb = parts[0].strip()
    ub = parts[1].strip()
    if normalize_expr(lb) != normalize_expr("1") or not ub:
        return None
    return ub


def range_to_do_triplet(rng: str) -> str:
    """Convert lb:ub[:step] range text to implied-do triplet lb,ub[,step]."""
    parts = rng.split(":", 2)
    if len(parts) == 2:
        return f"{parts[0].strip()},{parts[1].strip()}"
    if len(parts) == 3:
        return f"{parts[0].strip()},{parts[1].strip()},{parts[2].strip()}"
    return rng


def normalize_expr(text: str) -> str:
    """Normalize expression text for conservative equality checks."""
    return "".join(text.lower().split())


def parse_rank1_decl_bounds(unit: xunset.Unit) -> Dict[str, str]:
    """Collect rank-1 declaration bounds text as 'lb:ub'."""
    out: Dict[str, str] = {}
    for _ln, stmt in unit.body:
        low = stmt.strip().lower()
        if not low:
            continue
        if not re.match(r"^\s*(integer|real|logical|character|complex|type\b|class\b)", low, re.IGNORECASE):
            continue
        rhs = ""
        if "::" in low:
            rhs = low.split("::", 1)[1]
        else:
            m_no = re.match(
                r"^\s*(integer|real|logical|character|complex)"
                r"(?:\s*(?:\([^)]*\)|\*\s*\d+))?\s+(.+)$",
                low,
                re.IGNORECASE,
            )
            if not m_no:
                continue
            rhs = m_no.group(2)
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


def parse_rank2_decl_bounds(unit: xunset.Unit) -> Dict[str, Tuple[str, str]]:
    """Collect explicit rank-2 declaration upper bounds as (ub1, ub2)."""
    out: Dict[str, Tuple[str, str]] = {}
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
            parts = split_top_level_commas(dims)
            if len(parts) != 2:
                continue

            def upper(d: str) -> str:
                t = d.strip()
                if ":" in t:
                    toks = t.split(":", 1)
                    return toks[1].strip() if toks[1].strip() else ":"
                return t

            ub1 = upper(parts[0])
            ub2 = upper(parts[1])
            if ub1 == ":" or ub2 == ":":
                continue
            out[name] = (ub1, ub2)
    return out


def parse_rank3_decl_bounds(unit: xunset.Unit) -> Dict[str, Tuple[str, str, str]]:
    """Collect explicit rank-3 declaration upper bounds as (ub1, ub2, ub3)."""
    out: Dict[str, Tuple[str, str, str]] = {}
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
            parts = split_top_level_commas(dims)
            if len(parts) != 3:
                continue

            def upper(d: str) -> str:
                t = d.strip()
                if ":" in t:
                    toks = t.split(":", 1)
                    return toks[1].strip() if toks[1].strip() else ":"
                return t

            ub1 = upper(parts[0])
            ub2 = upper(parts[1])
            ub3 = upper(parts[2])
            if ub1 == ":" or ub2 == ":" or ub3 == ":":
                continue
            out[name] = (ub1, ub2, ub3)
    return out


def collect_rank1_array_names(unit: xunset.Unit) -> Set[str]:
    """Collect rank-1 array names (including assumed/deferred shape) declared in one unit."""
    out: Set[str] = set()
    for _ln, stmt in unit.body:
        low = stmt.strip().lower()
        if not low:
            continue
        if not re.match(r"^\s*(integer|real|logical|character|complex|type\b|class\b)", low, re.IGNORECASE):
            continue
        rhs = ""
        if "::" in low:
            rhs = low.split("::", 1)[1]
        else:
            m_no = re.match(
                r"^\s*(integer|real|logical|character|complex)"
                r"(?:\s*(?:\([^)]*\)|\*\s*\d+))?\s+(.+)$",
                low,
                re.IGNORECASE,
            )
            if not m_no:
                continue
            rhs = m_no.group(2)
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


def collect_rank1_whole_assign_blocked(unit: xunset.Unit) -> Set[str]:
    """Collect rank-1 names where section->whole rewrite should be avoided."""
    blocked: Set[str] = set()
    for _ln, stmt in unit.body:
        low = stmt.strip().lower()
        if not low:
            continue
        if not re.match(r"^\s*(integer|real|logical|character|complex|type\b|class\b)", low, re.IGNORECASE):
            continue

        decl_head = ""
        rhs = ""
        if "::" in low:
            left, right = low.split("::", 1)
            decl_head = left.strip()
            rhs = right
        else:
            m_no = re.match(
                r"^\s*(integer|real|logical|character|complex)"
                r"(?:\s*(?:\([^)]*\)|\*\s*\d+))?\s+(.+)$",
                low,
                re.IGNORECASE,
            )
            if not m_no:
                continue
            decl_head = m_no.group(1).strip()
            rhs = m_no.group(2)

        attrs_block = (
            re.search(r"\ballocatable\b", decl_head, re.IGNORECASE) is not None
            or re.search(r"\bpointer\b", decl_head, re.IGNORECASE) is not None
            or re.match(r"^\s*class\b", decl_head, re.IGNORECASE) is not None
        )

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
            # Deferred/assumed-shape or assumed-size rank-1 should not be rewritten.
            if ":" in dims:
                blocked.add(name)
                continue
            if attrs_block:
                blocked.add(name)
    return blocked


def collect_rank1_nondefault_real_names(unit: xunset.Unit) -> Set[str]:
    """Collect rank-1 REAL/DOUBLE PRECISION arrays with nondefault real kind."""
    out: Set[str] = set()
    for _ln, stmt in unit.body:
        raw = stmt.strip()
        low = raw.lower()
        if not low:
            continue

        is_nondefault_real_decl = False
        if re.match(r"^\s*double\s+precision\b", low):
            is_nondefault_real_decl = True
        elif re.match(r"^\s*real\b", low):
            if re.search(r"^\s*real\s*\*", low):
                is_nondefault_real_decl = True
            elif re.search(r"^\s*real\s*\(", low):
                is_nondefault_real_decl = True
        if not is_nondefault_real_decl:
            continue

        rhs = ""
        if "::" in low:
            rhs = low.split("::", 1)[1]
        else:
            m_no = re.match(
                r"^\s*(?:double\s+precision|real(?:\s*(?:\([^)]*\)|\*\s*\d+))?)\s+(.+)$",
                low,
                re.IGNORECASE,
            )
            if not m_no:
                continue
            rhs = m_no.group(1)

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
            if dims and "," not in dims:
                out.add(name)
    return out


def collect_scalar_nondefault_real_names(unit: xunset.Unit) -> Set[str]:
    """Collect scalar names declared as nondefault REAL in one unit."""
    out: Set[str] = set()
    for _ln, stmt in unit.body:
        low = stmt.strip().lower()
        if not low:
            continue
        is_nondefault_real_decl = False
        if re.match(r"^\s*double\s+precision\b", low):
            is_nondefault_real_decl = True
        elif re.match(r"^\s*real\b", low):
            if re.search(r"^\s*real\s*\*", low):
                is_nondefault_real_decl = True
            elif re.search(r"^\s*real\s*\(", low):
                is_nondefault_real_decl = True
        if not is_nondefault_real_decl:
            continue

        rhs = ""
        if "::" in low:
            rhs = low.split("::", 1)[1]
        else:
            m_no = re.match(
                r"^\s*(?:double\s+precision|real(?:\s*(?:\([^)]*\)|\*\s*\d+))?)\s+(.+)$",
                low,
                re.IGNORECASE,
            )
            if not m_no:
                continue
            rhs = m_no.group(1)

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
            # Scalar declaration entity has no (...) declarator.
            if not rest.startswith("("):
                out.add(name)
    return out


def collect_rank1_complex_names(unit: xunset.Unit) -> Set[str]:
    """Collect rank-1 COMPLEX array names in one unit."""
    out: Set[str] = set()
    for _ln, stmt in unit.body:
        low = stmt.strip().lower()
        if not low:
            continue
        if not re.match(r"^\s*complex\b", low, re.IGNORECASE):
            continue
        rhs = ""
        if "::" in low:
            rhs = low.split("::", 1)[1]
        else:
            m_no = re.match(
                r"^\s*complex(?:\s*(?:\([^)]*\)|\*\s*\d+))?\s+(.+)$",
                low,
                re.IGNORECASE,
            )
            if not m_no:
                continue
            rhs = m_no.group(1)
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
            if dims and "," not in dims:
                out.add(name)
    return out


def collect_character_scalar_names(unit: xunset.Unit) -> Set[str]:
    """Collect scalar CHARACTER entity names (substring targets must not be packed)."""
    out: Set[str] = set()
    for _ln, stmt in unit.body:
        low = stmt.strip().lower()
        if not low:
            continue
        if not re.match(r"^\s*character\b", low, re.IGNORECASE):
            continue
        rhs = ""
        if "::" in low:
            rhs = low.split("::", 1)[1]
        else:
            m_no = re.match(r"^\s*character(?:\s*(?:\([^)]*\)|\*\s*\d+))?\s+(.+)$", low, re.IGNORECASE)
            if not m_no:
                continue
            rhs = m_no.group(1)
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
            # Scalar character has no array declarator after entity name.
            if not rest.startswith("("):
                out.add(name)
    return out


def parse_char_length_from_spec(spec: str) -> Optional[int]:
    """Parse fixed CHARACTER length from type-spec text, return None if unknown."""
    s = spec.strip().lower()
    m_star = re.search(r"character\s*\*\s*(\d+)", s, re.IGNORECASE)
    if m_star:
        return int(m_star.group(1))
    m_paren = re.search(r"character\s*\((.*)\)", s, re.IGNORECASE)
    if m_paren:
        inner = m_paren.group(1)
        m_len = re.search(r"\blen\s*=\s*(\d+)\b", inner, re.IGNORECASE)
        if m_len:
            return int(m_len.group(1))
        if re.fullmatch(r"\s*\d+\s*", inner):
            return int(inner.strip())
    return None


def collect_character_scalar_lengths(unit: xunset.Unit) -> Dict[str, int]:
    """Collect scalar CHARACTER names with fixed length in one unit."""
    out: Dict[str, int] = {}
    for _ln, stmt in unit.body:
        raw = stmt.strip()
        low = raw.lower()
        if not low:
            continue
        if not re.match(r"^\s*character\b", low, re.IGNORECASE):
            continue

        decl_head = ""
        rhs = ""
        if "::" in low:
            left, right = low.split("::", 1)
            decl_head = left.strip()
            rhs = right
        else:
            m_no = re.match(r"^\s*(character(?:\s*(?:\([^)]*\)|\*\s*\d+))?)\s+(.+)$", low, re.IGNORECASE)
            if not m_no:
                continue
            decl_head = m_no.group(1).strip()
            rhs = m_no.group(2)
        nlen = parse_char_length_from_spec(decl_head)
        if nlen is None:
            continue

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
            if rest.startswith("("):
                continue
            out[name] = nlen
    return out


def collect_rank1_class_names(unit: xunset.Unit) -> Set[str]:
    """Collect rank-1 CLASS(...) declaration names (always blocked for whole-array rewrite)."""
    out: Set[str] = set()
    for _ln, stmt in unit.body:
        low = stmt.strip().lower()
        if not low or "::" not in low:
            continue
        if not re.match(r"^\s*class\b", low, re.IGNORECASE):
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
            if dims and "," not in dims:
                out.add(name)
    return out


def simplify_section_expr(expr: str, decl_bounds: Dict[str, str]) -> str:
    """Simplify full-range rank-1 sections to whole arrays when clearly safe."""
    s = expr
    # Rule 1: a(1:size(a)) -> a, unless immediately followed by substring selector:
    # a(1:size(a))(i:j) must stay as-is.
    pat_full_size = re.compile(
        r"\b([a-z][a-z0-9_]*)\s*\(\s*1\s*:\s*size\s*\(\s*\1\s*\)\s*\)",
        re.IGNORECASE,
    )
    def repl_full_size(m: re.Match[str]) -> str:
        name = m.group(1)
        tail = m.string[m.end() :]
        if tail.lstrip().startswith("("):
            return f"{name}(:)"
        return name
    s = pat_full_size.sub(repl_full_size, s)
    # Rule 2: a(lb:ub) -> a when section matches declaration bounds.
    for name, bnd in decl_bounds.items():
        pat = re.compile(rf"\b{re.escape(name)}\s*\(\s*([^)]*)\)", re.IGNORECASE)
        def repl(m: re.Match[str]) -> str:
            rng = m.group(1).strip()
            if ":" not in rng:
                return m.group(0)
            tail = m.string[m.end() :]
            if normalize_expr(rng) == normalize_expr(bnd):
                if tail.lstrip().startswith("("):
                    return f"{name}(:)"
                return name
            return m.group(0)
        s = pat.sub(repl, s)
    return s


def maybe_whole_array_assign(
    stmt: str,
    decl_bounds: Dict[str, str],
    blocked_names: Set[str],
    class_names: Set[str],
    alloc_rank1_bounds: Dict[str, str],
) -> Optional[str]:
    """Detect full-rank1 section assignment and suggest whole-array assignment."""
    m = ASSIGN_RE.match(stmt.strip())
    if not m:
        return None
    lhs = m.group(1).strip()
    rhs = m.group(2).strip()
    parsed = parse_indexed_name(lhs)
    if parsed is None:
        return None
    name, args = parsed
    if len(args) != 1:
        return None
    lname = name.lower()
    if lname in class_names:
        return None
    rng = args[0].strip()
    full = False
    bnd = decl_bounds.get(lname)
    if bnd is not None:
        if rng == ":":
            full = True
        elif ":" in rng and normalize_expr(rng) == normalize_expr(bnd):
            full = True
    if not full and lname in alloc_rank1_bounds:
        abnd = alloc_rank1_bounds[lname]
        if ":" in rng and normalize_expr(rng) == normalize_expr(abnd):
            full = True
    if lname in blocked_names:
        # Allow deferred/alloc/pointer only for scalar literal rhs when
        # full allocation bounds are known from prior ALLOCATE(name(...)).
        scalar_lit = bool(
            NUM_LITERAL_RE.match(rhs) or LOGICAL_LITERAL_RE.match(rhs) or CHAR_LITERAL_RE.match(rhs)
        )
        if not (scalar_lit and lname in alloc_rank1_bounds and full):
            return None
    if not full:
        return None
    return f"{name} = {rhs}"


def parse_alloc_rank1_bounds(stmt: str) -> Dict[str, str]:
    """Parse simple ALLOCATE(name(ub)) or ALLOCATE(name(lb:ub)) into rank-1 bounds."""
    out: Dict[str, str] = {}
    m = ALLOCATE_RE.match(stmt.strip())
    if not m:
        return out
    for chunk in split_top_level_commas(m.group(1)):
        txt = chunk.strip()
        if not txt:
            continue
        if "=" in txt and "=>" not in txt:
            continue
        mm = re.match(r"^([a-z][a-z0-9_]*)\s*\((.*)\)\s*$", txt, re.IGNORECASE)
        if not mm:
            continue
        name = mm.group(1).lower()
        dims = mm.group(2).strip()
        if not dims or "," in dims:
            continue
        if ":" in dims:
            lb, ub = dims.split(":", 1)
            lb = lb.strip() or "1"
            ub = ub.strip()
            if not ub:
                continue
            out[name] = f"{lb}:{ub}"
        else:
            ub = dims.strip()
            if not ub:
                continue
            out[name] = f"1:{ub}"
    return out


def parse_allocate_for_source(stmt: str) -> Optional[Tuple[str, str, List[str]]]:
    """Parse allocate(...) as (base_name, alloc_object_text, option_chunks) for source rewrite."""
    m = ALLOCATE_RE.match(stmt.strip())
    if not m:
        return None
    inside = m.group(1).strip()
    if not inside:
        return None
    chunks = split_top_level_commas(inside)
    objs: List[str] = []
    opts: List[str] = []
    for ch in chunks:
        txt = ch.strip()
        if not txt:
            continue
        if "=" in txt and "=>" not in txt:
            key = txt.split("=", 1)[0].strip().lower()
            if key in {"source", "mold"}:
                return None
            opts.append(txt)
            continue
        objs.append(txt)
    if len(objs) != 1:
        return None
    obj = objs[0]
    mobj = re.match(r"^\s*([a-z][a-z0-9_]*)\b", obj, re.IGNORECASE)
    if not mobj:
        return None
    return mobj.group(1), obj, opts


def maybe_allocate_source_pair(stmt_alloc: str, stmt_next: str) -> Optional[str]:
    """Detect allocate(x(...)); x = rhs and suggest allocate(x(...), source=rhs)."""
    parsed = parse_allocate_for_source(stmt_alloc)
    if parsed is None:
        return None
    base, obj, opts = parsed
    m_asn = ASSIGN_RE.match(stmt_next.strip())
    if not m_asn:
        return None
    lhs = m_asn.group(1).strip()
    rhs = m_asn.group(2).strip()
    m_lhs = SIMPLE_NAME_RE.match(lhs)
    if not m_lhs or m_lhs.group(1).lower() != base.lower():
        return None
    # Conservative: skip self-reference in SOURCE expression.
    if re.search(rf"\b{re.escape(base)}\b", strip_quoted_text(rhs), re.IGNORECASE):
        return None
    inner_parts: List[str] = [obj]
    inner_parts.extend(opts)
    inner_parts.append(f"source={rhs}")
    return f"allocate({', '.join(inner_parts)})"


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
    array_names: Set[str],
) -> Optional[str]:
    """Detect elementwise assignment loop and build replacement."""
    m = ASSIGN_RE.match(body_stmt.strip())
    if not m:
        return None
    lhs = m.group(1).strip()
    rhs = m.group(2).strip()
    lhs_parsed = parse_lhs_indexed_by_loop(lhs, loop_var)
    if lhs_parsed is None:
        return None
    lhs_name, lhs_args, lhs_pos = lhs_parsed
    lb, ubtxt = split_range(rng)
    rhs_sliced = replace_affine_index_anydim(rhs, loop_var, lb, ubtxt, array_names=array_names)
    # The helper above expects (lb, ub-text). rng may include step in ub-text.
    if has_loop_var(rhs_sliced, loop_var):
        return None
    if rhs_sliced == rhs:
        return None
    lhs_args_sliced = list(lhs_args)
    lhs_args_sliced[lhs_pos] = rng
    if len(lhs_args_sliced) == 1:
        lhs_expr = f"{lhs_name}({rng})"
        bnd = decl_bounds.get(lhs_name.lower())
        if bnd is not None and normalize_expr(rng) == normalize_expr(bnd):
            lhs_expr = lhs_name
        elif can_collapse_lhs_alloc(lhs_name, rng, alloc_map):
            lhs_expr = lhs_name
    else:
        lhs_expr = f"{lhs_name}({', '.join(lhs_args_sliced)})"
        # Conservative rank-2 broadcast support:
        # a(sec, j_range) = a(sec, j_shifted) * x(sec)
        # -> a(sec, j_range) = a(sec, j_shifted) * spread(x(sec), dim=2, ncopies=ub)
        if len(lhs_args_sliced) == 2 and normalize_expr(lb) == normalize_expr("1") and ":" not in ubtxt:
            ncopy = ubtxt.strip()
            if lhs_pos == 1:
                sec = lhs_args_sliced[0].strip()
                pat = re.compile(rf"\b([a-z][a-z0-9_]*)\s*\(\s*{re.escape(sec)}\s*\)", re.IGNORECASE)
                rhs_sliced = pat.sub(lambda m: f"spread({m.group(1)}({sec}), dim=2, ncopies={ncopy})", rhs_sliced)
                rng_txt = lhs_args_sliced[1].strip()
                ncopy2 = range_upper_if_one_based(sec)
                if ncopy2 is not None:
                    pat2 = re.compile(rf"\b([a-z][a-z0-9_]*)\s*\(\s*{re.escape(rng_txt)}\s*\)", re.IGNORECASE)
                    rhs_sliced = pat2.sub(
                        lambda m: f"spread({m.group(1)}({rng_txt}), dim=1, ncopies={ncopy2})",
                        rhs_sliced,
                    )
            elif lhs_pos == 0:
                sec = lhs_args_sliced[1].strip()
                pat = re.compile(rf"\b([a-z][a-z0-9_]*)\s*\(\s*{re.escape(sec)}\s*\)", re.IGNORECASE)
                rhs_sliced = pat.sub(lambda m: f"spread({m.group(1)}({sec}), dim=1, ncopies={ncopy})", rhs_sliced)
                rng_txt = lhs_args_sliced[0].strip()
                ncopy2 = range_upper_if_one_based(sec)
                if ncopy2 is not None:
                    pat2 = re.compile(rf"\b([a-z][a-z0-9_]*)\s*\(\s*{re.escape(rng_txt)}\s*\)", re.IGNORECASE)
                    rhs_sliced = pat2.sub(
                        lambda m: f"spread({m.group(1)}({rng_txt}), dim=2, ncopies={ncopy2})",
                        rhs_sliced,
                    )
    rhs_sliced = simplify_section_expr(rhs_sliced, decl_bounds)
    if has_disallowed_function_calls(rhs_sliced, array_names):
        return None
    return f"{lhs_expr} = {rhs_sliced}"


def maybe_temp_then_elementwise(
    loop_var: str,
    rng: str,
    stmt_temp: str,
    stmt_body: str,
    decl_bounds: Dict[str, str],
    alloc_map: Dict[str, str],
    array_names: Set[str],
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
    if has_disallowed_function_calls(rhs2_inlined, array_names):
        return None
    return f"{lhs_expr} = {rhs2_inlined}"


def maybe_reduction_sum(
    loop_var: str,
    rng: str,
    body_stmt: str,
    prev_stmt: Optional[str],
    decl_bounds: Dict[str, str],
    array_names: Set[str],
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
    expr_sliced = simplify_section_expr(expr_sliced, decl_bounds)
    if has_disallowed_function_calls(expr_sliced, array_names):
        return None
    return f"{acc_name_lhs} = sum({expr_sliced})"


def maybe_reduction_sum_vector_2d(
    loop_var: str,
    rng: str,
    body_stmt: str,
    prev_stmt: Optional[str],
    decl_bounds: Dict[str, str],
    rank2_bounds: Dict[str, Tuple[str, str]],
) -> Optional[str]:
    """Detect vector accumulation from 2D slices and suggest SPREAD+SUM(dim=1)."""
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
    acc_name = acc_lhs.group(1)
    if acc_prev.group(1).lower() != acc_name.lower():
        return None
    acc_bnd = decl_bounds.get(acc_name.lower())
    if acc_bnd is None:
        return None
    madd = ADD_ACC_RE.match(ml.group(2).strip())
    if not madd:
        return None
    if madd.group(1).lower() != acc_name.lower():
        return None
    expr = madd.group(2).strip()
    lb, ubtxt = split_range(rng)
    # Keep this rewrite conservative: unit-step full-range row loops only.
    if normalize_expr(lb) != normalize_expr("1"):
        return None
    if ":" in ubtxt:
        return None

    # Require at least one rank-2 row-slice term involving loop index.
    touched_rank2 = False
    expr_mat = expr
    for r2name, rb in rank2_bounds.items():
        pat_row = re.compile(
            rf"\b{re.escape(r2name)}\s*\(\s*{re.escape(loop_var)}\s*,\s*:\s*\)",
            re.IGNORECASE,
        )
        if pat_row.search(expr_mat):
            touched_rank2 = True
            expr_mat = pat_row.sub(lambda m: f"{r2name}({lb}:{ubtxt}, 1:{rb[1]})", expr_mat)
    if not touched_rank2:
        return None

    # Broadcast bare rank-1 vectors across rows.
    for vname in decl_bounds.keys():
        pat_vec = re.compile(rf"\b{re.escape(vname)}\b(?!\s*\()", re.IGNORECASE)
        expr_mat = pat_vec.sub(
            lambda m: f"spread({vname}, dim=1, ncopies={ubtxt})",
            expr_mat,
        )

    if has_loop_var(expr_mat, loop_var):
        return None

    expr_mat = simplify_section_expr(expr_mat, decl_bounds)
    expr_mat = simplify_section_expr_rank2(expr_mat, rank2_bounds)
    return f"{acc_name} = sum({expr_mat}, dim=1)"


def maybe_reduction_product(
    loop_var: str,
    rng: str,
    body_stmt: str,
    prev_stmt: Optional[str],
    decl_bounds: Dict[str, str],
    array_names: Set[str],
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
    expr_sliced = simplify_section_expr(expr_sliced, decl_bounds)
    if has_disallowed_function_calls(expr_sliced, array_names):
        return None
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


def maybe_nested_transpose(
    outer_loop_var: str,
    outer_lb: str,
    outer_ub: str,
    outer_step: Optional[str],
    inner_do_stmt: str,
    inner_body_stmt: str,
    rank2_bounds: Dict[str, Tuple[str, str]],
) -> Optional[str]:
    """Detect nested-loop transpose assignment and suggest TRANSPOSE."""
    m_do = DO_RE.match(inner_do_stmt.strip())
    if not m_do:
        return None
    inner_loop_var = m_do.group(1)
    inner_lb = m_do.group(2).strip()
    inner_ub = m_do.group(3).strip()
    inner_step = m_do.group(4)
    if outer_lb.strip() != "1" or inner_lb != "1":
        return None
    if (outer_step is not None and outer_step.strip() != "1") or (inner_step is not None and inner_step.strip() != "1"):
        return None

    ma = ASSIGN_RE.match(inner_body_stmt.strip())
    if not ma:
        return None
    lhs = ma.group(1).strip()
    rhs = ma.group(2).strip()
    ml = re.match(r"^([a-z][a-z0-9_]*)\s*\(\s*([^,]+)\s*,\s*([^,]+)\s*\)$", lhs, re.IGNORECASE)
    mr = re.match(r"^([a-z][a-z0-9_]*)\s*\(\s*([^,]+)\s*,\s*([^,]+)\s*\)$", rhs, re.IGNORECASE)
    if not ml or not mr:
        return None
    lhs_name = ml.group(1)
    rhs_name = mr.group(1)
    lhs_a1 = ml.group(2).strip().lower()
    lhs_a2 = ml.group(3).strip().lower()
    rhs_a1 = mr.group(2).strip().lower()
    rhs_a2 = mr.group(3).strip().lower()
    if not (
        lhs_a1 == inner_loop_var.lower()
        and lhs_a2 == outer_loop_var.lower()
        and rhs_a1 == outer_loop_var.lower()
        and rhs_a2 == inner_loop_var.lower()
    ):
        return None

    yb = rank2_bounds.get(lhs_name.lower())
    xb = rank2_bounds.get(rhs_name.lower())
    if yb is not None and xb is not None:
        if not (
            normalize_expr(yb[0]) == normalize_expr(inner_ub)
            and normalize_expr(yb[1]) == normalize_expr(outer_ub.strip())
            and normalize_expr(xb[0]) == normalize_expr(outer_ub.strip())
            and normalize_expr(xb[1]) == normalize_expr(inner_ub)
        ):
            return None
        return f"{lhs_name} = transpose({rhs_name})"

    return (
        f"{lhs_name}(1:{inner_ub}, 1:{outer_ub.strip()}) = "
        f"transpose({rhs_name}(1:{outer_ub.strip()}, 1:{inner_ub}))"
    )


def parse_indexed_name(text: str) -> Optional[Tuple[str, List[str]]]:
    """Parse name(args...) expression into (name, args)."""
    m = re.match(r"^\s*([a-z][a-z0-9_]*)\s*\((.*)\)\s*$", text, re.IGNORECASE)
    if not m:
        return None
    name = m.group(1)
    args = [a.strip() for a in split_top_level_commas(m.group(2))]
    if not args:
        return None
    return name, args


def parse_rank1_const_lhs_target(lhs: str) -> Optional[Tuple[str, int, int]]:
    """Parse lhs as rank-1 name(k) or name(k1:k2) with integer bounds."""
    m = re.match(r"^\s*([a-z][a-z0-9_]*)\s*\(\s*(.+)\s*\)\s*$", lhs, re.IGNORECASE)
    if not m:
        return None
    name = m.group(1)
    inner = m.group(2).strip()
    if "," in inner:
        return None
    if ":" in inner:
        parts = inner.split(":", 1)
        lo_txt = parts[0].strip()
        hi_txt = parts[1].strip()
        if not lo_txt or not hi_txt:
            return None
        if not re.fullmatch(r"[+-]?\d+", lo_txt) or not re.fullmatch(r"[+-]?\d+", hi_txt):
            return None
        lo = int(lo_txt)
        hi = int(hi_txt)
        if hi < lo:
            return None
        return name, lo, hi
    if not re.fullmatch(r"[+-]?\d+", inner):
        return None
    k = int(inner)
    return name, k, k


def parse_array_constructor_values(rhs: str) -> Optional[List[str]]:
    """Parse bracket or slash-delimited constructor into value items."""
    txt = rhs.strip()
    if txt.startswith("[") and txt.endswith("]"):
        inside = txt[1:-1].strip()
    elif txt.startswith("(/") and txt.endswith("/)"):
        inside = txt[2:-2].strip()
    else:
        return None
    if not inside:
        return []
    vals = [v.strip() for v in split_top_level_commas(inside)]
    if any(not v for v in vals):
        return None
    return vals


def rhs_constructor_items(rhs: str) -> List[str]:
    """Return constructor items from rhs or scalar rhs as one item."""
    ctor = parse_array_constructor_values(rhs)
    if ctor is not None:
        return ctor
    return [rhs.strip()]


def is_numeric_literal_text(text: str) -> bool:
    """True for simple numeric literals (integer/real, optional kind/exponent)."""
    return NUM_LITERAL_RE.match(text.strip()) is not None


def allow_constructor_for_nondefault_real_with_scalars(
    parts: List[str], scalar_nondefault_real_names: Set[str]
) -> bool:
    """Conservative gate for nondefault-real constructor rewrites with scalar-name support."""
    if not parts:
        return False
    for p in parts:
        t = p.strip()
        if is_numeric_literal_text(t):
            continue
        mname = SIMPLE_NAME_RE.match(t)
        if mname and mname.group(1).lower() in scalar_nondefault_real_names:
            continue
        return False
    return True


def maybe_constructor_pack(
    body: List[Tuple[int, str]],
    start_idx: int,
    decl_bounds: Dict[str, str],
    nondefault_real_names: Set[str],
    scalar_nondefault_real_names: Set[str],
    complex_rank1_names: Set[str],
    character_scalar_names: Set[str],
) -> Optional[Tuple[int, str]]:
    """Detect consecutive rank-1 const-index assignments packable into one constructor."""
    if start_idx >= len(body):
        return None
    ln0, stmt0 = body[start_idx]
    m0 = ASSIGN_RE.match(stmt0.strip())
    if not m0:
        return None
    lhs0 = parse_rank1_const_lhs_target(m0.group(1).strip())
    if lhs0 is None:
        return None
    name, lo0, hi0 = lhs0
    if name.lower() in complex_rank1_names:
        return None
    if name.lower() in character_scalar_names:
        return None
    rhs0 = m0.group(2).strip()
    items0 = rhs_constructor_items(rhs0)
    if (hi0 - lo0 + 1) != len(items0):
        return None
    # Avoid self-reference: constructor assignment evaluates rhs before lhs update.
    if re.search(rf"\b{re.escape(name)}\b", strip_quoted_text(rhs0), re.IGNORECASE):
        return None

    parts: List[str] = list(items0)
    last_hi = hi0
    end_idx = start_idx
    j = start_idx + 1
    while j < len(body):
        _ln, stmt = body[j]
        m = ASSIGN_RE.match(stmt.strip())
        if not m:
            break
        lhs = parse_rank1_const_lhs_target(m.group(1).strip())
        if lhs is None or lhs[0].lower() != name.lower():
            break
        lo, hi = lhs[1], lhs[2]
        if lo != last_hi + 1:
            break
        rhs = m.group(2).strip()
        if re.search(rf"\b{re.escape(name)}\b", strip_quoted_text(rhs), re.IGNORECASE):
            break
        items = rhs_constructor_items(rhs)
        if (hi - lo + 1) != len(items):
            break
        parts.extend(items)
        last_hi = hi
        end_idx = j
        j += 1

    if end_idx == start_idx:
        return None
    if name.lower() in nondefault_real_names and not allow_constructor_for_nondefault_real_with_scalars(
        parts, scalar_nondefault_real_names
    ):
        return None
    lhs_expr = f"{name}({lo0}:{last_hi})"
    bnd = decl_bounds.get(name.lower())
    if bnd is not None and ":" in bnd:
        b_lo, b_hi = bnd.split(":", 1)
        if (
            normalize_expr(str(lo0)) == normalize_expr("1")
            and normalize_expr(b_lo or "1") == normalize_expr("1")
            and normalize_expr(str(last_hi)) == normalize_expr(b_hi)
        ):
            lhs_expr = name
    suggestion = f"{lhs_expr} = [{', '.join(parts)}]"
    return end_idx, suggestion


def maybe_constructor_pack_sparse(
    body: List[Tuple[int, str]],
    start_idx: int,
    nondefault_real_names: Set[str],
    scalar_nondefault_real_names: Set[str],
    complex_rank1_names: Set[str],
    character_scalar_names: Set[str],
) -> Optional[Tuple[int, str]]:
    """Detect consecutive scalar-index assignments to same rank-1 array with sparse indices."""
    if start_idx >= len(body):
        return None
    m0 = ASSIGN_RE.match(body[start_idx][1].strip())
    if not m0:
        return None
    lhs0 = parse_rank1_const_lhs_target(m0.group(1).strip())
    if lhs0 is None:
        return None
    name0, lo0, hi0 = lhs0
    if name0.lower() in complex_rank1_names:
        return None
    if name0.lower() in character_scalar_names:
        return None
    if lo0 != hi0:
        return None
    rhs0 = m0.group(2).strip()
    if re.search(rf"\b{re.escape(name0)}\b", strip_quoted_text(rhs0), re.IGNORECASE):
        return None

    idxs: List[int] = [lo0]
    vals: List[str] = [rhs0]
    seen: Set[int] = {lo0}
    end_idx = start_idx
    j = start_idx + 1
    while j < len(body):
        m = ASSIGN_RE.match(body[j][1].strip())
        if not m:
            break
        lhs = parse_rank1_const_lhs_target(m.group(1).strip())
        if lhs is None or lhs[0].lower() != name0.lower():
            break
        lo, hi = lhs[1], lhs[2]
        if lo != hi:
            break
        if lo in seen:
            return None
        rhs = m.group(2).strip()
        if re.search(rf"\b{re.escape(name0)}\b", strip_quoted_text(rhs), re.IGNORECASE):
            break
        idxs.append(lo)
        vals.append(rhs)
        seen.add(lo)
        end_idx = j
        j += 1

    if end_idx == start_idx:
        return None
    # If contiguous, let constructor_pack handle it.
    is_contig = True
    for k in range(1, len(idxs)):
        if idxs[k] != idxs[k - 1] + 1:
            is_contig = False
            break
    if is_contig:
        return None
    if name0.lower() in nondefault_real_names and not allow_constructor_for_nondefault_real_with_scalars(
        vals, scalar_nondefault_real_names
    ):
        return None

    idx_vec = ", ".join(str(k) for k in idxs)
    val_vec = ", ".join(vals)
    suggestion = f"{name0}([{idx_vec}]) = [{val_vec}]"
    return end_idx, suggestion


def split_top_level_concat(text: str) -> List[str]:
    """Split expression by top-level // operators."""
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
            elif ch == "/" and i + 1 < len(text) and text[i + 1] == "/" and depth == 0:
                out.append("".join(cur).strip())
                cur = []
                i += 2
                continue
        cur.append(ch)
        i += 1
    tail = "".join(cur).strip()
    if tail:
        out.append(tail)
    return out


def parse_char_literal_length(expr: str) -> Optional[int]:
    """Return character literal length if expr is a single char literal, else None."""
    t = expr.strip()
    if not t:
        return None
    # Optional kind-prefix: k_'abc' or 4_"abc"
    qpos1 = t.find("'")
    qpos2 = t.find('"')
    qpos = -1
    qch = ""
    if qpos1 >= 0 and (qpos2 < 0 or qpos1 < qpos2):
        qpos = qpos1
        qch = "'"
    elif qpos2 >= 0:
        qpos = qpos2
        qch = '"'
    if qpos < 0:
        return None
    prefix = t[:qpos].strip()
    if prefix and not prefix.endswith("_"):
        return None
    i = qpos + 1
    n = 0
    while i < len(t):
        ch = t[i]
        if ch == qch:
            if i + 1 < len(t) and t[i + 1] == qch:
                n += 1
                i += 2
                continue
            i += 1
            if t[i:].strip():
                return None
            return n
        n += 1
        i += 1
    return None


def strip_outer_parens(expr: str) -> str:
    """Strip one layer of outer parens if they wrap whole expression."""
    t = expr.strip()
    if len(t) < 2 or t[0] != "(" or t[-1] != ")":
        return t
    depth = 0
    in_single = False
    in_double = False
    for i, ch in enumerate(t):
        if ch == "'" and not in_double:
            in_single = not in_single
        elif ch == '"' and not in_single:
            in_double = not in_double
        elif not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
                if depth == 0 and i != len(t) - 1:
                    return t
    if depth == 0:
        return t[1:-1].strip()
    return t


def char_expr_known_length(expr: str, scalar_char_lengths: Dict[str, int]) -> Optional[int]:
    """Return fixed length for conservative subset of CHARACTER expressions."""
    t = strip_outer_parens(expr)
    lit_len = parse_char_literal_length(t)
    if lit_len is not None:
        return lit_len
    mname = SIMPLE_NAME_RE.match(t)
    if mname:
        return scalar_char_lengths.get(mname.group(1).lower())
    if re.match(r"^\s*(?:achar|char)\s*\(", t, re.IGNORECASE):
        return 1
    parts = split_top_level_concat(t)
    if len(parts) > 1:
        total = 0
        for p in parts:
            lp = char_expr_known_length(p, scalar_char_lengths)
            if lp is None:
                return None
            total += lp
        return total
    return None


def parse_scalar_char_substring_lhs(lhs: str, scalar_char_lengths: Dict[str, int]) -> Optional[Tuple[str, int, int]]:
    """Parse scalar CHARACTER substring target S(lo:hi) with integer bounds."""
    m = re.match(r"^\s*([a-z][a-z0-9_]*)\s*\(\s*([^:(),]+)\s*:\s*([^:(),]+)\s*\)\s*$", lhs, re.IGNORECASE)
    if not m:
        return None
    name = m.group(1)
    if name.lower() not in scalar_char_lengths:
        return None
    lo_t = m.group(2).strip()
    hi_t = m.group(3).strip()
    if not re.fullmatch(r"[+-]?\d+", lo_t) or not re.fullmatch(r"[+-]?\d+", hi_t):
        return None
    lo = int(lo_t)
    hi = int(hi_t)
    if hi < lo:
        return None
    return name, lo, hi


def maybe_character_concat_pack(
    body: List[Tuple[int, str]],
    start_idx: int,
    scalar_char_lengths: Dict[str, int],
) -> Optional[Tuple[int, str]]:
    """Detect contiguous scalar CHARACTER substring assignments and fold to // concat."""
    if start_idx >= len(body):
        return None
    m0 = ASSIGN_RE.match(body[start_idx][1].strip())
    if not m0:
        return None
    p0 = parse_scalar_char_substring_lhs(m0.group(1).strip(), scalar_char_lengths)
    if p0 is None:
        return None
    name, lo0, hi0 = p0
    rhs0 = m0.group(2).strip()
    if re.search(rf"\b{re.escape(name)}\b", strip_quoted_text(rhs0), re.IGNORECASE):
        return None
    l0 = char_expr_known_length(rhs0, scalar_char_lengths)
    if l0 is None or l0 != (hi0 - lo0 + 1):
        return None

    parts: List[str] = [rhs0]
    last_hi = hi0
    end_idx = start_idx
    j = start_idx + 1
    while j < len(body):
        m = ASSIGN_RE.match(body[j][1].strip())
        if not m:
            break
        p = parse_scalar_char_substring_lhs(m.group(1).strip(), scalar_char_lengths)
        if p is None or p[0].lower() != name.lower():
            break
        lo, hi = p[1], p[2]
        if lo != last_hi + 1:
            break
        rhs = m.group(2).strip()
        if re.search(rf"\b{re.escape(name)}\b", strip_quoted_text(rhs), re.IGNORECASE):
            break
        ln_rhs = char_expr_known_length(rhs, scalar_char_lengths)
        if ln_rhs is None or ln_rhs != (hi - lo + 1):
            break
        parts.append(rhs)
        last_hi = hi
        end_idx = j
        j += 1

    if end_idx == start_idx:
        return None
    total_len = scalar_char_lengths.get(name.lower())
    if total_len is None or lo0 != 1 or last_hi != total_len:
        return None
    return end_idx, f"{name} = {' // '.join(parts)}"


def maybe_matmul_mv_nested(
    outer_loop_var: str,
    outer_rng: str,
    init_stmt: str,
    inner_do_stmt: str,
    inner_body_stmt: str,
    decl_bounds1: Dict[str, str],
    rank2_bounds: Dict[str, Tuple[str, str]],
) -> Optional[str]:
    """Detect y(i)=0; do j; y(i)=y(i)+A(i,j)*x(j); enddo -> matmul(A,x)."""
    m_init = ASSIGN_RE.match(init_stmt.strip())
    m_acc = ASSIGN_RE.match(inner_body_stmt.strip())
    m_ido = DO_RE.match(inner_do_stmt.strip())
    if not m_init or not m_acc or not m_ido:
        return None
    lhs_init = parse_indexed_name(m_init.group(1).strip())
    lhs_acc = parse_indexed_name(m_acc.group(1).strip())
    if lhs_init is None or lhs_acc is None:
        return None
    if lhs_init != lhs_acc:
        return None
    y_name, y_args = lhs_init
    if len(y_args) != 1 or y_args[0].lower() != outer_loop_var.lower():
        return None
    if not ZERO_LITERAL_RE.match(m_init.group(2).strip()):
        return None

    inner_var = m_ido.group(1)
    inner_rng = build_range(m_ido.group(2), m_ido.group(3), m_ido.group(4))
    madd = re.match(r"^\s*(.+?)\s*\+\s*(.+)$", m_acc.group(2).strip(), re.IGNORECASE)
    if not madd:
        return None
    acc_ref = parse_indexed_name(madd.group(1).strip())
    if acc_ref != lhs_init:
        return None
    prod = madd.group(2).strip()
    mm = re.match(r"^\s*(.+?)\s*\*\s*(.+)\s*$", prod, re.IGNORECASE)
    if not mm:
        return None
    factors = [mm.group(1).strip(), mm.group(2).strip()]
    parsed = [parse_indexed_name(f) for f in factors]
    if parsed[0] is None or parsed[1] is None:
        return None

    # Normalize order to (A(i,j), x(j)).
    a_name = ""
    x_name = ""
    for p0, p1 in (parsed, parsed[::-1]):
        n0, a0 = p0
        n1, a1 = p1
        if len(a0) == 2 and len(a1) == 1 and a0[0].lower() == outer_loop_var.lower() and a0[1].lower() == inner_var.lower() and a1[0].lower() == inner_var.lower():
            a_name = n0
            x_name = n1
            break
    if not a_name:
        return None

    outer_lb, outer_ub = split_range(outer_rng)
    inner_lb, inner_ub = split_range(inner_rng)
    y_sec = f"{y_name}({outer_rng})"
    a_sec = f"{a_name}({outer_rng}, {inner_rng})"
    x_sec = f"{x_name}({inner_rng})"

    yb = decl_bounds1.get(y_name.lower())
    ab = rank2_bounds.get(a_name.lower())
    xb = decl_bounds1.get(x_name.lower())
    if (
        yb is not None
        and ab is not None
        and xb is not None
        and normalize_expr(yb) == normalize_expr(outer_rng)
        and normalize_expr(ab[0]) == normalize_expr(outer_ub.strip())
        and normalize_expr(ab[1]) == normalize_expr(inner_ub.strip())
        and normalize_expr(xb) == normalize_expr(inner_rng)
        and normalize_expr(outer_lb.strip()) == normalize_expr("1")
        and normalize_expr(inner_lb.strip()) == normalize_expr("1")
    ):
        return f"{y_name} = matmul({a_name}, {x_name})"

    a_sec = simplify_section_expr(a_sec, decl_bounds1)
    x_sec = simplify_section_expr(x_sec, decl_bounds1)
    y_sec = simplify_section_expr(y_sec, decl_bounds1)
    return f"{y_sec} = matmul({a_sec}, {x_sec})"


def maybe_matmul_mm_nested(
    outer_loop_var: str,
    outer_rng: str,
    mid_do_stmt: str,
    init_stmt: str,
    inner_do_stmt: str,
    inner_body_stmt: str,
    decl_bounds1: Dict[str, str],
    rank2_bounds: Dict[str, Tuple[str, str]],
) -> Optional[str]:
    """Detect C(i,k)=0; do j; C(i,k)=C(i,k)+A(i,j)*B(j,k); enddo -> matmul(A,B)."""
    m_mdo = DO_RE.match(mid_do_stmt.strip())
    m_init = ASSIGN_RE.match(init_stmt.strip())
    m_ido = DO_RE.match(inner_do_stmt.strip())
    m_acc = ASSIGN_RE.match(inner_body_stmt.strip())
    if not m_mdo or not m_init or not m_ido or not m_acc:
        return None
    mid_var = m_mdo.group(1)
    mid_rng = build_range(m_mdo.group(2), m_mdo.group(3), m_mdo.group(4))
    inner_var = m_ido.group(1)
    inner_rng = build_range(m_ido.group(2), m_ido.group(3), m_ido.group(4))

    lhs_init = parse_indexed_name(m_init.group(1).strip())
    lhs_acc = parse_indexed_name(m_acc.group(1).strip())
    if lhs_init is None or lhs_acc is None or lhs_init != lhs_acc:
        return None
    c_name, c_args = lhs_init
    if len(c_args) != 2:
        return None
    if not (c_args[0].lower() == outer_loop_var.lower() and c_args[1].lower() == mid_var.lower()):
        return None
    if not ZERO_LITERAL_RE.match(m_init.group(2).strip()):
        return None

    madd = re.match(r"^\s*(.+?)\s*\+\s*(.+)$", m_acc.group(2).strip(), re.IGNORECASE)
    if not madd:
        return None
    acc_ref = parse_indexed_name(madd.group(1).strip())
    if acc_ref != lhs_init:
        return None
    mm = re.match(r"^\s*(.+?)\s*\*\s*(.+)\s*$", madd.group(2).strip(), re.IGNORECASE)
    if not mm:
        return None
    factors = [mm.group(1).strip(), mm.group(2).strip()]
    parsed = [parse_indexed_name(f) for f in factors]
    if parsed[0] is None or parsed[1] is None:
        return None

    a_name = ""
    b_name = ""
    for p0, p1 in (parsed, parsed[::-1]):
        n0, a0 = p0
        n1, a1 = p1
        if (
            len(a0) == 2
            and len(a1) == 2
            and a0[0].lower() == outer_loop_var.lower()
            and a0[1].lower() == inner_var.lower()
            and a1[0].lower() == inner_var.lower()
            and a1[1].lower() == mid_var.lower()
        ):
            a_name = n0
            b_name = n1
            break
    if not a_name:
        return None

    outer_lb, outer_ub = split_range(outer_rng)
    mid_lb, mid_ub = split_range(mid_rng)
    inner_lb, inner_ub = split_range(inner_rng)
    c_sec = f"{c_name}({outer_rng}, {mid_rng})"
    a_sec = f"{a_name}({outer_rng}, {inner_rng})"
    b_sec = f"{b_name}({inner_rng}, {mid_rng})"

    cb = rank2_bounds.get(c_name.lower())
    ab = rank2_bounds.get(a_name.lower())
    bb = rank2_bounds.get(b_name.lower())
    if (
        cb is not None
        and ab is not None
        and bb is not None
        and normalize_expr(cb[0]) == normalize_expr(outer_ub.strip())
        and normalize_expr(cb[1]) == normalize_expr(mid_ub.strip())
        and normalize_expr(ab[0]) == normalize_expr(outer_ub.strip())
        and normalize_expr(ab[1]) == normalize_expr(inner_ub.strip())
        and normalize_expr(bb[0]) == normalize_expr(inner_ub.strip())
        and normalize_expr(bb[1]) == normalize_expr(mid_ub.strip())
        and normalize_expr(outer_lb.strip()) == normalize_expr("1")
        and normalize_expr(mid_lb.strip()) == normalize_expr("1")
        and normalize_expr(inner_lb.strip()) == normalize_expr("1")
    ):
        return f"{c_name} = matmul({a_name}, {b_name})"

    c_sec = simplify_section_expr(c_sec, decl_bounds1)
    a_sec = simplify_section_expr(a_sec, decl_bounds1)
    b_sec = simplify_section_expr(b_sec, decl_bounds1)
    return f"{c_sec} = matmul({a_sec}, {b_sec})"


def has_nonarray_call(expr: str, array_names: Set[str]) -> bool:
    """Conservative call detector: true when expr has call-like name(...) not known as array."""
    s = strip_quoted_text(expr.lower())
    for m in CALL_LIKE_RE.finditer(s):
        name = m.group(1).lower()
        if name in array_names:
            continue
        return True
    return False


def has_disallowed_function_calls(expr: str, array_names: Set[str]) -> bool:
    """True if expression calls non-array names outside a conservative elemental allowlist."""
    s = strip_quoted_text(expr.lower())
    for m in CALL_LIKE_RE.finditer(s):
        name = m.group(1).lower()
        if name in array_names:
            continue
        if name in ELEMENTAL_INTRINSICS:
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
    decl_bounds: Dict[str, str],
    array_names: Set[str],
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
    expr_s = simplify_section_expr(expr_s, decl_bounds)
    cond_s = simplify_section_expr(cond_s, decl_bounds)
    if has_disallowed_function_calls(expr_s, array_names) or has_disallowed_function_calls(cond_s, array_names):
        return None
    return f"{acc} = sum({expr_s}, mask = {cond_s})"


def maybe_masked_product(
    loop_var: str,
    rng: str,
    prev_stmt: Optional[str],
    if_stmt: str,
    body_stmt: str,
    decl_bounds: Dict[str, str],
    array_names: Set[str],
) -> Optional[str]:
    """Detect masked product inside IF block within loop."""
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
    mmul = MUL_ACC_RE.match(ml.group(2).strip())
    if not mmul or mmul.group(1).lower() != acc:
        return None
    cond = mif.group(1).strip()
    expr = mmul.group(2).strip()
    lb, ubtxt = rng.split(":", 1)
    cond_s = replace_index_with_slice(cond, loop_var, lb, ubtxt)
    expr_s = replace_index_with_slice(expr, loop_var, lb, ubtxt)
    if has_loop_var(cond_s, loop_var) or has_loop_var(expr_s, loop_var):
        return None
    if cond_s == cond or expr_s == expr:
        return None
    expr_s = simplify_section_expr(expr_s, decl_bounds)
    cond_s = simplify_section_expr(cond_s, decl_bounds)
    if has_disallowed_function_calls(expr_s, array_names) or has_disallowed_function_calls(cond_s, array_names):
        return None
    return f"{acc} = product({expr_s}, mask = {cond_s})"


def maybe_count(
    loop_var: str,
    rng: str,
    prev_stmt: Optional[str],
    if_stmt: str,
    body_stmt: str,
    array_names: Set[str],
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
    if kname == "count":
        # Avoid invalid/ambiguous rewrites like "count = count(...)".
        return None
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
    if has_nonarray_call(cond_s, array_names):
        triplet = range_to_do_triplet(rng)
        return f"{kname} = count([({cond}, {loop_var}={triplet})])"
    return f"{kname} = count({cond_s})"


def maybe_count_inline(
    loop_var: str,
    rng: str,
    prev_stmt: Optional[str],
    inline_if_stmt: str,
    array_names: Set[str],
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
    if kname == "count":
        # Avoid invalid/ambiguous rewrites like "count = count(...)".
        return None

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
    if has_nonarray_call(cond_s, array_names):
        triplet = range_to_do_triplet(rng)
        return f"{kname} = count([({cond}, {loop_var}={triplet})])"
    return f"{kname} = count({cond_s})"


def parse_find_condition(cond: str, loop_var: str) -> Optional[Tuple[str, str]]:
    """Parse equality condition arr(i) == value into (arr_name, value_expr)."""
    m = EQ_RE.match(cond.strip())
    if not m:
        return None
    lhs = m.group(1).strip()
    rhs = m.group(3).strip()
    li = INDEXED_NAME_RE.match(lhs)
    ri = INDEXED_NAME_RE.match(rhs)
    if li and li.group(2).lower() == loop_var.lower() and not ri:
        if has_loop_var(rhs, loop_var):
            return None
        return li.group(1), rhs
    if ri and ri.group(2).lower() == loop_var.lower() and not li:
        if has_loop_var(lhs, loop_var):
            return None
        return ri.group(1), lhs
    return None


def build_findloc_index_expr(sec: str, val: str, lb: str, step: Optional[str]) -> str:
    """Build index expression with FINDLOC and loop-range offset adjustment."""
    loc_expr = f"findloc({sec}, {val}, dim=1, back=.true.)"
    lb_s = lb.strip()
    if step is None:
        if normalize_expr(lb_s) != "1":
            return f"({lb_s}) - 1 + {loc_expr}"
        return loc_expr
    st = step.strip()
    return f"({lb_s}) + ({st})*({loc_expr} - 1)"


def maybe_findloc_inline(
    loop_var: str,
    rng: str,
    step: Optional[str],
    prev_stmt: Optional[str],
    inline_if_stmt: str,
    decl_bounds: Dict[str, str],
) -> Optional[str]:
    """Detect one-line IF find-index pattern and build FINDLOC replacement."""
    if prev_stmt is None:
        return None
    mp = ASSIGN_RE.match(prev_stmt.strip())
    if not mp:
        return None
    idx_prev = SIMPLE_NAME_RE.match(mp.group(1).strip())
    if not idx_prev:
        return None
    idx_name = idx_prev.group(1).lower()
    if not ZERO_LITERAL_RE.match(mp.group(2).strip()):
        return None

    mif = ONE_LINE_IF_RE.match(inline_if_stmt.strip())
    if not mif:
        return None
    cond = mif.group(1).strip()
    body_stmt = mif.group(2).strip()
    if body_stmt.lower().startswith("then"):
        return None
    mset = ASSIGN_RE.match(body_stmt)
    if not mset:
        return None
    lhs = SIMPLE_NAME_RE.match(mset.group(1).strip())
    rhs = SIMPLE_NAME_RE.match(mset.group(2).strip())
    if not lhs or not rhs:
        return None
    if lhs.group(1).lower() != idx_name or rhs.group(1).lower() != loop_var.lower():
        return None

    parsed = parse_find_condition(cond, loop_var)
    if parsed is None:
        return None
    arr, val = parsed
    lb, _ub = split_range(rng)
    sec = simplify_section_expr(f"{arr}({rng})", decl_bounds)
    idx_expr = build_findloc_index_expr(sec, val, lb, step)
    return f"{idx_name} = {idx_expr}"


def maybe_findloc_block(
    loop_var: str,
    rng: str,
    step: Optional[str],
    prev_stmt: Optional[str],
    if_stmt: str,
    body_stmt: str,
    decl_bounds: Dict[str, str],
) -> Optional[str]:
    """Detect IF-block find-index pattern and build FINDLOC replacement."""
    if prev_stmt is None:
        return None
    mp = ASSIGN_RE.match(prev_stmt.strip())
    if not mp:
        return None
    idx_prev = SIMPLE_NAME_RE.match(mp.group(1).strip())
    if not idx_prev:
        return None
    idx_name = idx_prev.group(1).lower()
    if not ZERO_LITERAL_RE.match(mp.group(2).strip()):
        return None

    mif = IF_THEN_RE.match(if_stmt.strip())
    if not mif:
        return None
    mset = ASSIGN_RE.match(body_stmt.strip())
    if not mset:
        return None
    lhs = SIMPLE_NAME_RE.match(mset.group(1).strip())
    rhs = SIMPLE_NAME_RE.match(mset.group(2).strip())
    if not lhs or not rhs:
        return None
    if lhs.group(1).lower() != idx_name or rhs.group(1).lower() != loop_var.lower():
        return None

    parsed = parse_find_condition(mif.group(1).strip(), loop_var)
    if parsed is None:
        return None
    arr, val = parsed
    lb, _ub = split_range(rng)
    sec = simplify_section_expr(f"{arr}({rng})", decl_bounds)
    idx_expr = build_findloc_index_expr(sec, val, lb, step)
    return f"{idx_name} = {idx_expr}"


def maybe_masked_product_inline(
    loop_var: str,
    rng: str,
    prev_stmt: Optional[str],
    inline_if_stmt: str,
    decl_bounds: Dict[str, str],
    array_names: Set[str],
) -> Optional[str]:
    """Detect one-line IF masked product reduction and build PRODUCT replacement."""
    if prev_stmt is None:
        return None
    mp = ASSIGN_RE.match(prev_stmt.strip())
    if not mp:
        return None
    acc_prev = SIMPLE_NAME_RE.match(mp.group(1).strip())
    if not acc_prev:
        return None
    acc = acc_prev.group(1).lower()

    mif = ONE_LINE_IF_RE.match(inline_if_stmt.strip())
    if not mif:
        return None
    cond = mif.group(1).strip()
    body_stmt = mif.group(2).strip()
    if body_stmt.lower().startswith("then"):
        return None

    ml = ASSIGN_RE.match(body_stmt)
    if not ml:
        return None
    lhs = SIMPLE_NAME_RE.match(ml.group(1).strip())
    if not lhs or lhs.group(1).lower() != acc:
        return None
    mmul = MUL_ACC_RE.match(ml.group(2).strip())
    if not mmul or mmul.group(1).lower() != acc:
        return None
    expr = mmul.group(2).strip()

    lb, ubtxt = rng.split(":", 1)
    cond_s = replace_index_with_slice(cond, loop_var, lb, ubtxt)
    expr_s = replace_index_with_slice(expr, loop_var, lb, ubtxt)
    if has_loop_var(cond_s, loop_var) or has_loop_var(expr_s, loop_var):
        return None
    if cond_s == cond or expr_s == expr:
        return None
    expr_s = simplify_section_expr(expr_s, decl_bounds)
    cond_s = simplify_section_expr(cond_s, decl_bounds)
    if has_disallowed_function_calls(expr_s, array_names) or has_disallowed_function_calls(cond_s, array_names):
        return None
    return f"{acc} = product({expr_s}, mask = {cond_s})"


def maybe_masked_sum_inline(
    loop_var: str,
    rng: str,
    prev_stmt: Optional[str],
    inline_if_stmt: str,
    decl_bounds: Dict[str, str],
    array_names: Set[str],
) -> Optional[str]:
    """Detect one-line IF masked sum reduction and build SUM replacement."""
    if prev_stmt is None:
        return None
    mp = ASSIGN_RE.match(prev_stmt.strip())
    if not mp:
        return None
    acc_prev = SIMPLE_NAME_RE.match(mp.group(1).strip())
    if not acc_prev:
        return None
    acc = acc_prev.group(1).lower()

    mif = ONE_LINE_IF_RE.match(inline_if_stmt.strip())
    if not mif:
        return None
    cond = mif.group(1).strip()
    body_stmt = mif.group(2).strip()
    if body_stmt.lower().startswith("then"):
        return None

    ml = ASSIGN_RE.match(body_stmt)
    if not ml:
        return None
    lhs = SIMPLE_NAME_RE.match(ml.group(1).strip())
    if not lhs or lhs.group(1).lower() != acc:
        return None
    madd = ADD_ACC_RE.match(ml.group(2).strip())
    if not madd or madd.group(1).lower() != acc:
        return None
    expr = madd.group(2).strip()

    lb, ubtxt = rng.split(":", 1)
    cond_s = replace_index_with_slice(cond, loop_var, lb, ubtxt)
    expr_s = replace_index_with_slice(expr, loop_var, lb, ubtxt)
    if has_loop_var(cond_s, loop_var) or has_loop_var(expr_s, loop_var):
        return None
    if cond_s == cond or expr_s == expr:
        return None
    expr_s = simplify_section_expr(expr_s, decl_bounds)
    cond_s = simplify_section_expr(cond_s, decl_bounds)
    if has_disallowed_function_calls(expr_s, array_names) or has_disallowed_function_calls(cond_s, array_names):
        return None
    return f"{acc} = sum({expr_s}, mask = {cond_s})"


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
    array_names: Set[str],
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
    if (
        has_disallowed_function_calls(cond_s, array_names)
        or has_disallowed_function_calls(rhs_t_s, array_names)
        or has_disallowed_function_calls(rhs_f_s, array_names)
    ):
        return None

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


def maybe_if_any_all(stmt: str, decl_bounds: Dict[str, str]) -> Optional[str]:
    """Detect one-line IF with x(1).or.x(2)... or .and. chain and suggest ANY/ALL."""
    m = ONE_LINE_IF_RE.match(stmt.strip())
    if not m:
        return None
    cond = m.group(1).strip()
    action = m.group(2).strip()
    if not action:
        return None
    parts = re.split(r"(?i)(\.(?:or|and)\.)", cond)
    if len(parts) < 3 or len(parts) % 2 == 0:
        return None
    op = parts[1].lower()
    if op not in {".or.", ".and."}:
        return None
    for k in range(3, len(parts), 2):
        if parts[k].lower() != op:
            return None

    name: Optional[str] = None
    idxs: List[int] = []
    for k in range(0, len(parts), 2):
        term = parts[k].strip()
        if term.startswith("(") and term.endswith(")"):
            term = term[1:-1].strip()
        mt = INDEXED_CONST_RE.match(term)
        if not mt:
            return None
        n = mt.group(1).lower()
        iv = int(mt.group(2))
        if name is None:
            name = n
        elif n != name:
            return None
        idxs.append(iv)
    if name is None or len(idxs) < 2:
        return None
    if idxs != list(range(idxs[0], idxs[0] + len(idxs))):
        return None

    sec = f"{name}({idxs[0]}:{idxs[-1]})"
    sec = simplify_section_expr(sec, decl_bounds)
    fn = "any" if op == ".or." else "all"
    return f"if ({fn}({sec})) {action}"


def parse_implied_do_inner(inner: str) -> Optional[Tuple[str, str, str, str, Optional[str]]]:
    """Parse one implied-DO body: expr, i=lb,ub[,step]."""
    parts = split_top_level_commas(inner)
    if len(parts) < 3 or len(parts) > 4:
        return None
    expr = parts[0].strip()
    m_asn = LOOP_ASSIGN_RE.match(parts[1].strip())
    if not m_asn:
        return None
    loop_var = m_asn.group(1).strip()
    lb = m_asn.group(2).strip()
    ub = parts[2].strip()
    step = parts[3].strip() if len(parts) == 4 else None
    return expr, loop_var, lb, ub, step


def simplify_section_expr_rank2(expr: str, rank2_bounds: Dict[str, Tuple[str, str]]) -> str:
    """Simplify full-range rank-2 sections a(lb1:ub1,lb2:ub2) -> a when safe."""
    s = expr
    for name, (b1, b2) in rank2_bounds.items():
        pat = re.compile(rf"\b{re.escape(name)}\s*\(\s*([^,]+)\s*,\s*([^)]+)\)", re.IGNORECASE)

        def repl(m: re.Match[str]) -> str:
            r1 = m.group(1).strip()
            r2 = m.group(2).strip()
            if normalize_expr(r1) == normalize_expr(f"1:{b1}") and normalize_expr(r2) == normalize_expr(f"1:{b2}"):
                return name
            return m.group(0)

        s = pat.sub(repl, s)
    return s


def maybe_print_implied_do(
    stmt: str,
    decl_bounds: Dict[str, str],
    rank2_bounds: Dict[str, Tuple[str, str]],
) -> Optional[str]:
    """Detect PRINT implied-DO items and simplify to array expressions."""
    m = PRINT_PREFIX_RE.match(stmt.strip())
    if not m:
        return None
    tail = m.group(1).strip()
    items = split_top_level_commas(tail)
    if not items:
        return None

    changed = False
    out_items: List[str] = []
    for item in items:
        txt = item.strip()
        if not (txt.startswith("(") and txt.endswith(")")):
            out_items.append(txt)
            continue

        outer = parse_implied_do_inner(txt[1:-1].strip())
        if outer is None:
            out_items.append(txt)
            continue
        expr, loop_var, lb, ub, step = outer
        ubtxt = ub if step is None else f"{ub}:{step}"

        expr_s = expr
        # Handle nested implied-DO, e.g. ((y(i,j), i=1,n1), j=1,n2).
        if expr.startswith("(") and expr.endswith(")"):
            inner = parse_implied_do_inner(expr[1:-1].strip())
            if inner is not None:
                in_expr, in_var, in_lb, in_ub, in_step = inner
                in_ubtxt = in_ub if in_step is None else f"{in_ub}:{in_step}"
                need_transpose = False
                arr_name: Optional[str] = None
                in_idx = parse_indexed_name(in_expr)
                if in_idx is not None and len(in_idx[1]) == 2:
                    arr_name = in_idx[0].lower()
                    a1 = in_idx[1][0].strip().lower()
                    a2 = in_idx[1][1].strip().lower()
                    if a1 == loop_var.lower() and a2 == in_var.lower():
                        need_transpose = True
                nested = replace_affine_index_anydim(in_expr, in_var, in_lb, in_ubtxt)
                nested = replace_affine_index_anydim(nested, loop_var, lb, ubtxt)
                if not has_loop_var(nested, in_var) and not has_loop_var(nested, loop_var):
                    expr_s = nested
                    if need_transpose and arr_name is not None:
                        sec_match = re.match(
                            rf"^\s*{re.escape(arr_name)}\s*\(\s*([^,]+)\s*,\s*([^)]+)\)\s*$",
                            expr_s,
                            re.IGNORECASE,
                        )
                        rb = rank2_bounds.get(arr_name)
                        if sec_match and rb is not None:
                            r1 = sec_match.group(1).strip()
                            r2 = sec_match.group(2).strip()
                            if (
                                normalize_expr(r1) == normalize_expr(f"1:{rb[0]}")
                                and normalize_expr(r2) == normalize_expr(f"1:{rb[1]}")
                            ):
                                expr_s = f"transpose({arr_name})"
                else:
                    out_items.append(txt)
                    continue
            else:
                out_items.append(txt)
                continue
        else:
            expr_s = replace_affine_index_anydim(expr_s, loop_var, lb, ubtxt)
            if has_loop_var(expr_s, loop_var) or expr_s == expr:
                out_items.append(txt)
                continue

        expr_s = simplify_section_expr(expr_s, decl_bounds)
        expr_s = simplify_section_expr_rank2(expr_s, rank2_bounds)
        out_items.append(expr_s)
        changed = True

    if not changed:
        return None
    return f"print*, {', '.join(out_items)}"


def maybe_read_implied_do(
    stmt: str,
    rank2_bounds: Dict[str, Tuple[str, str]],
) -> Optional[str]:
    """Detect simple READ nested implied-DO and suggest full-array READ."""
    m = READ_PREFIX_RE.match(stmt.strip())
    if not m:
        return None
    tail = m.group(1).strip()
    items = split_top_level_commas(tail)
    if len(items) != 1:
        return None
    txt = items[0].strip()
    if not (txt.startswith("(") and txt.endswith(")")):
        return None

    outer = parse_implied_do_inner(txt[1:-1].strip())
    if outer is None:
        return None
    expr, loop_var, lb, ub, step = outer
    if lb.strip() != "1" or (step is not None and step.strip() != "1"):
        return None
    if not (expr.startswith("(") and expr.endswith(")")):
        return None

    inner = parse_implied_do_inner(expr[1:-1].strip())
    if inner is None:
        return None
    in_expr, in_var, in_lb, in_ub, in_step = inner
    if in_lb.strip() != "1" or (in_step is not None and in_step.strip() != "1"):
        return None

    in_idx = parse_indexed_name(in_expr)
    if in_idx is None:
        return None
    arr_name, args = in_idx
    if len(args) != 2:
        return None
    if args[0].strip().lower() != in_var.lower():
        return None
    if args[1].strip().lower() != loop_var.lower():
        return None

    rb = rank2_bounds.get(arr_name.lower())
    if rb is None:
        return None
    if normalize_expr(in_ub) != normalize_expr(rb[0]):
        return None
    if normalize_expr(ub) != normalize_expr(rb[1]):
        return None
    return f"read*, {arr_name}"


def maybe_print_loop(
    loop_var: str,
    lb: str,
    ub: str,
    step: Optional[str],
    stmt: str,
    decl_bounds: Dict[str, str],
) -> Optional[str]:
    """Detect DO-loop scalar PRINT over i and suggest array PRINT."""
    m = PRINT_STMT_RE.match(stmt.strip())
    if not m:
        return None
    rest = m.group(1).strip()
    parts = split_top_level_commas(rest)
    if len(parts) != 2:
        return None
    fmt = parts[0].strip()
    item = parts[1].strip()
    if not fmt:
        return None
    # Keep this rule for formatted PRINT only; list-directed is handled elsewhere.
    if fmt == "*":
        return None

    ubtxt = ub if step is None else f"{ub}:{step}"
    item_s = replace_affine_index_anydim(item, loop_var, lb, ubtxt)
    if item_s == item or has_loop_var(item_s, loop_var):
        return None
    item_s = simplify_section_expr(item_s, decl_bounds)
    return f"print {fmt}, {item_s}"


def maybe_random_number_loop_1d(
    loop_var: str,
    rng: str,
    stmt: str,
    decl_bounds: Dict[str, str],
    rank2_bounds: Dict[str, Tuple[str, str]],
    rank3_bounds: Dict[str, Tuple[str, str, str]],
) -> Optional[str]:
    """Detect DO loop filling one array dimension via random_number scalar call."""
    m = RANDOM_CALL_RE.match(stmt.strip())
    if not m:
        return None
    harvest = parse_indexed_name(m.group(1).strip())
    if harvest is None:
        return None
    arr_name, args = harvest
    loop_ranges = {loop_var.lower(): rng}
    new_args: List[str] = []
    replaced = False
    for arg in args:
        a = arg.strip()
        r = loop_ranges.get(a.lower())
        if r is not None:
            new_args.append(r)
            replaced = True
        else:
            new_args.append(a)
    if not replaced:
        return None
    harvest_expr = f"{arr_name}({', '.join(new_args)})"
    if len(new_args) == 1:
        bnd = decl_bounds.get(arr_name.lower())
        if bnd is not None and normalize_expr(new_args[0]) == normalize_expr(bnd):
            harvest_expr = arr_name
    elif len(new_args) == 2:
        bnd2 = rank2_bounds.get(arr_name.lower())
        if (
            bnd2 is not None
            and normalize_expr(new_args[0]) == normalize_expr(f"1:{bnd2[0]}")
            and normalize_expr(new_args[1]) == normalize_expr(f"1:{bnd2[1]}")
        ):
            harvest_expr = arr_name
    elif len(new_args) == 3:
        bnd3 = rank3_bounds.get(arr_name.lower())
        if (
            bnd3 is not None
            and normalize_expr(new_args[0]) == normalize_expr(f"1:{bnd3[0]}")
            and normalize_expr(new_args[1]) == normalize_expr(f"1:{bnd3[1]}")
            and normalize_expr(new_args[2]) == normalize_expr(f"1:{bnd3[2]}")
        ):
            harvest_expr = arr_name
    return f"call random_number({harvest_expr})"


def maybe_random_number_nested_2d(
    outer_loop_var: str,
    outer_lb: str,
    outer_ub: str,
    outer_step: Optional[str],
    inner_do_stmt: str,
    inner_body_stmt: str,
) -> Optional[str]:
    """Detect two-level loop random fills and collapse innermost loop only."""
    m_ido = DO_RE.match(inner_do_stmt.strip())
    m_call = RANDOM_CALL_RE.match(inner_body_stmt.strip())
    if not m_ido or not m_call:
        return None
    inner_var = m_ido.group(1)
    inner_lb = m_ido.group(2).strip()
    inner_ub = m_ido.group(3).strip()
    inner_step = m_ido.group(4)
    if inner_lb != "1" or (inner_step is not None and inner_step.strip() != "1"):
        return None
    harvest = parse_indexed_name(m_call.group(1).strip())
    if harvest is None:
        return None
    arr_name, args = harvest
    if len(args) != 2:
        return None
    outer_l = outer_loop_var.lower()
    inner_l = inner_var.lower()
    new_args: List[str] = []
    saw_outer = False
    saw_inner = False
    for arg in args:
        a = arg.strip().lower()
        if a == outer_l:
            new_args.append(outer_loop_var)
            saw_outer = True
        elif a == inner_l:
            new_args.append(":")
            saw_inner = True
        else:
            return None
    if not (saw_outer and saw_inner):
        return None

    do_outer = f"do {outer_loop_var}={outer_lb.strip()},{outer_ub.strip()}"
    if outer_step is not None:
        do_outer = f"{do_outer},{outer_step.strip()}"
    call_line = f"  call random_number({arr_name}({', '.join(new_args)}))"
    return "\n".join([do_outer, call_line, "end do"])


def maybe_random_number_nested_3d(
    outer_loop_var: str,
    outer_lb: str,
    outer_ub: str,
    outer_step: Optional[str],
    mid_do_stmt: str,
    inner_do_stmt: str,
    inner_body_stmt: str,
) -> Optional[str]:
    """Detect three-level loop random fills and collapse innermost loop only."""
    m_mdo = DO_RE.match(mid_do_stmt.strip())
    m_ido = DO_RE.match(inner_do_stmt.strip())
    m_call = RANDOM_CALL_RE.match(inner_body_stmt.strip())
    if not m_mdo or not m_ido or not m_call:
        return None
    mid_var = m_mdo.group(1)
    mid_lb = m_mdo.group(2).strip()
    mid_ub = m_mdo.group(3).strip()
    mid_step = m_mdo.group(4)
    inner_var = m_ido.group(1)
    inner_lb = m_ido.group(2).strip()
    inner_ub = m_ido.group(3).strip()
    inner_step = m_ido.group(4)
    if inner_lb != "1" or (inner_step is not None and inner_step.strip() != "1"):
        return None

    harvest = parse_indexed_name(m_call.group(1).strip())
    if harvest is None:
        return None
    arr_name, args = harvest
    if len(args) != 3:
        return None
    outer_l = outer_loop_var.lower()
    mid_l = mid_var.lower()
    inner_l = inner_var.lower()
    new_args: List[str] = []
    saw_outer = False
    saw_mid = False
    saw_inner = False
    for arg in args:
        a = arg.strip().lower()
        if a == outer_l:
            new_args.append(outer_loop_var)
            saw_outer = True
        elif a == mid_l:
            new_args.append(mid_var)
            saw_mid = True
        elif a == inner_l:
            new_args.append(":")
            saw_inner = True
        else:
            return None
    if not (saw_outer and saw_mid and saw_inner):
        return None

    do_outer = f"do {outer_loop_var}={outer_lb.strip()},{outer_ub.strip()}"
    if outer_step is not None:
        do_outer = f"{do_outer},{outer_step.strip()}"
    do_mid = f"  do {mid_var}={mid_lb},{mid_ub}"
    if mid_step is not None:
        do_mid = f"{do_mid},{mid_step.strip()}"
    call_line = f"    call random_number({arr_name}({', '.join(new_args)}))"
    return "\n".join([do_outer, do_mid, call_line, "  end do", "end do"])


def maybe_spread_nested_2d(
    outer_loop_var: str,
    outer_lb: str,
    outer_ub: str,
    outer_step: Optional[str],
    inner_do_stmt: str,
    inner_body_stmt: str,
    decl_bounds1: Dict[str, str],
    rank2_bounds: Dict[str, Tuple[str, str]],
) -> Optional[str]:
    """Detect 2D nested loop assignments that map naturally to SPREAD."""
    m_do = DO_RE.match(inner_do_stmt.strip())
    m_asn = ASSIGN_RE.match(inner_body_stmt.strip())
    if not m_do or not m_asn:
        return None
    inner_loop_var = m_do.group(1).lower()
    inner_lb = m_do.group(2).strip()
    inner_ub = m_do.group(3).strip()
    inner_step = m_do.group(4)
    if outer_lb.strip() != "1" or inner_lb != "1":
        return None
    if (outer_step is not None and outer_step.strip() != "1") or (inner_step is not None and inner_step.strip() != "1"):
        return None

    lhs = parse_indexed_name(m_asn.group(1).strip())
    if lhs is None:
        return None
    lhs_name, lhs_args = lhs
    if len(lhs_args) != 2:
        return None
    a1 = lhs_args[0].strip().lower()
    a2 = lhs_args[1].strip().lower()
    outer_l = outer_loop_var.lower()
    if {a1, a2} != {outer_l, inner_loop_var}:
        return None

    # Map target dimensions to loop vars and copy counts.
    dim1_var = a1
    dim2_var = a2
    ub_by_var = {
        outer_l: outer_ub.strip(),
        inner_loop_var: inner_ub,
    }
    n1 = ub_by_var[dim1_var]
    n2 = ub_by_var[dim2_var]

    rhs = m_asn.group(2).strip()
    rhs_s = rhs
    # Replace rank-2 *array* element refs that use loop vars with full sections.
    for r2name in rank2_bounds.keys():
        pat2 = re.compile(
            rf"\b{re.escape(r2name)}\s*\(\s*{re.escape(dim1_var)}\s*,\s*{re.escape(dim2_var)}\s*\)",
            re.IGNORECASE,
        )
        rhs_s = pat2.sub(lambda _m: f"{r2name}(1:{n1}, 1:{n2})", rhs_s)
    # Convert rank-1 refs by loop variable to SPREAD broadcasts.
    pat_dim1 = re.compile(rf"\b([a-z][a-z0-9_]*)\s*\(\s*{re.escape(dim1_var)}\s*\)", re.IGNORECASE)
    rhs_s = pat_dim1.sub(lambda m: f"spread({m.group(1)}, dim=2, ncopies={n2})", rhs_s)
    pat_dim2 = re.compile(rf"\b([a-z][a-z0-9_]*)\s*\(\s*{re.escape(dim2_var)}\s*\)", re.IGNORECASE)
    rhs_s = pat_dim2.sub(lambda m: f"spread({m.group(1)}, dim=1, ncopies={n1})", rhs_s)

    if rhs_s == rhs:
        return None
    if has_loop_var(rhs_s, dim1_var) or has_loop_var(rhs_s, dim2_var):
        return None

    lhs_expr = f"{lhs_name}(1:{n1}, 1:{n2})"
    lbnd = rank2_bounds.get(lhs_name.lower())
    if lbnd is not None and normalize_expr(lbnd[0]) == normalize_expr(n1) and normalize_expr(lbnd[1]) == normalize_expr(n2):
        lhs_expr = lhs_name

    rhs_s = simplify_section_expr(rhs_s, decl_bounds1)
    rhs_s = simplify_section_expr_rank2(rhs_s, rank2_bounds)
    array_names = set(decl_bounds1.keys()) | set(rank2_bounds.keys())
    if has_disallowed_function_calls(rhs_s, array_names):
        return None
    if "spread(" not in rhs_s.lower():
        return None
    return f"{lhs_expr} = {rhs_s}"


def maybe_spread_nested_3d(
    outer_loop_var: str,
    outer_lb: str,
    outer_ub: str,
    outer_step: Optional[str],
    mid_do_stmt: str,
    inner_do_stmt: str,
    inner_body_stmt: str,
    rank2_bounds: Dict[str, Tuple[str, str]],
    rank3_bounds: Dict[str, Tuple[str, str, str]],
) -> Optional[str]:
    """Detect 3D replication loops t(...,k,...) = c(...,...) and suggest SPREAD."""
    m_mdo = DO_RE.match(mid_do_stmt.strip())
    m_ido = DO_RE.match(inner_do_stmt.strip())
    m_asn = ASSIGN_RE.match(inner_body_stmt.strip())
    if not m_mdo or not m_ido or not m_asn:
        return None
    mid_var = m_mdo.group(1).lower()
    mid_lb = m_mdo.group(2).strip()
    mid_ub = m_mdo.group(3).strip()
    mid_step = m_mdo.group(4)
    inner_var = m_ido.group(1).lower()
    inner_lb = m_ido.group(2).strip()
    inner_ub = m_ido.group(3).strip()
    inner_step = m_ido.group(4)
    outer_l = outer_loop_var.lower()

    if outer_lb.strip() != "1" or mid_lb != "1" or inner_lb != "1":
        return None
    if (
        (outer_step is not None and outer_step.strip() != "1")
        or (mid_step is not None and mid_step.strip() != "1")
        or (inner_step is not None and inner_step.strip() != "1")
    ):
        return None

    lhs = parse_indexed_name(m_asn.group(1).strip())
    rhs = parse_indexed_name(m_asn.group(2).strip())
    if lhs is None or rhs is None:
        return None
    lhs_name, lhs_args = lhs
    rhs_name, rhs_args = rhs
    if len(lhs_args) != 3 or len(rhs_args) != 2:
        return None
    vars3 = [a.strip().lower() for a in lhs_args]
    if set(vars3) != {outer_l, mid_var, inner_var}:
        return None

    # Find inserted dimension d where removing lhs arg yields rhs args in order.
    rhs_l = [a.strip().lower() for a in rhs_args]
    ins_dim: Optional[int] = None
    for d in range(3):
        if vars3[:d] + vars3[d + 1 :] == rhs_l:
            ins_dim = d + 1  # Fortran dim is 1-based.
            break
    if ins_dim is None:
        return None

    ub_by_var = {
        outer_l: outer_ub.strip(),
        mid_var: mid_ub.strip(),
        inner_var: inner_ub.strip(),
    }
    n1 = ub_by_var[vars3[0]]
    n2 = ub_by_var[vars3[1]]
    n3 = ub_by_var[vars3[2]]
    ncopies = ub_by_var[vars3[ins_dim - 1]]
    rhs_b1 = ub_by_var[rhs_l[0]]
    rhs_b2 = ub_by_var[rhs_l[1]]

    lhs_expr = f"{lhs_name}(1:{n1}, 1:{n2}, 1:{n3})"
    rb3 = rank3_bounds.get(lhs_name.lower())
    if (
        rb3 is not None
        and normalize_expr(rb3[0]) == normalize_expr(n1)
        and normalize_expr(rb3[1]) == normalize_expr(n2)
        and normalize_expr(rb3[2]) == normalize_expr(n3)
    ):
        lhs_expr = lhs_name

    rhs_expr = f"{rhs_name}(1:{rhs_b1}, 1:{rhs_b2})"
    rb2 = rank2_bounds.get(rhs_name.lower())
    if rb2 is not None and normalize_expr(rb2[0]) == normalize_expr(rhs_b1) and normalize_expr(rb2[1]) == normalize_expr(rhs_b2):
        rhs_expr = rhs_name
    return f"{lhs_expr} = spread({rhs_expr}, dim={ins_dim}, ncopies={ncopies})"


def analyze_unit(unit: xunset.Unit) -> List[Finding]:
    """Analyze one unit for simple loop-to-array opportunities."""
    findings: List[Finding] = []
    body = unit.body
    decl_bounds = parse_rank1_decl_bounds(unit)
    rank2_bounds = parse_rank2_decl_bounds(unit)
    rank3_bounds = parse_rank3_decl_bounds(unit)
    blocked_whole_names = collect_rank1_whole_assign_blocked(unit)
    class_rank1_names = collect_rank1_class_names(unit)
    nondefault_real_rank1_names = collect_rank1_nondefault_real_names(unit)
    scalar_nondefault_real_names = collect_scalar_nondefault_real_names(unit)
    complex_rank1_names = collect_rank1_complex_names(unit)
    character_scalar_names = collect_character_scalar_names(unit)
    scalar_char_lengths = collect_character_scalar_lengths(unit)
    array_names: Set[str] = set(decl_bounds.keys()) | set(rank2_bounds.keys()) | set(rank3_bounds.keys())
    array_names |= collect_rank1_array_names(unit)
    alloc_map: Dict[str, str] = {}
    alloc_rank1_bounds: Dict[str, str] = {}
    i = 0
    while i < len(body):
        ln, stmt = body[i]
        alloc_map.update(parse_alloc_shape_spec(stmt))
        alloc_rank1_bounds.update(parse_alloc_rank1_bounds(stmt))
        if i + 1 < len(body):
            sugg_alloc_src = maybe_allocate_source_pair(stmt, body[i + 1][1])
            if sugg_alloc_src is not None:
                findings.append(
                    Finding(
                        path=unit.path,
                        rule="allocate_source",
                        unit_kind=unit.kind,
                        unit_name=unit.name,
                        start_line=ln,
                        end_line=body[i + 1][0],
                        suggestion=sugg_alloc_src,
                    )
                )
                i += 2
                continue
        sugg_read = maybe_read_implied_do(stmt, rank2_bounds)
        if sugg_read is not None:
            findings.append(
                Finding(
                    path=unit.path,
                    rule="implied_do_read",
                    unit_kind=unit.kind,
                    unit_name=unit.name,
                    start_line=ln,
                    end_line=ln,
                    suggestion=sugg_read,
                )
            )
        sugg_print = maybe_print_implied_do(stmt, decl_bounds, rank2_bounds)
        if sugg_print is not None:
            findings.append(
                Finding(
                    path=unit.path,
                    rule="implied_do_print",
                    unit_kind=unit.kind,
                    unit_name=unit.name,
                    start_line=ln,
                    end_line=ln,
                    suggestion=sugg_print,
                )
            )
        sugg_ifaa = maybe_if_any_all(stmt, decl_bounds)
        if sugg_ifaa is not None:
            findings.append(
                Finding(
                    path=unit.path,
                    rule="logical_any_all",
                    unit_kind=unit.kind,
                    unit_name=unit.name,
                    start_line=ln,
                    end_line=ln,
                    suggestion=sugg_ifaa,
                )
            )
        char_pack = maybe_character_concat_pack(body, i, scalar_char_lengths)
        if char_pack is not None:
            end_idx, sugg_char = char_pack
            findings.append(
                Finding(
                    path=unit.path,
                    rule="character_concat_pack",
                    unit_kind=unit.kind,
                    unit_name=unit.name,
                    start_line=ln,
                    end_line=body[end_idx][0],
                    suggestion=sugg_char,
                )
            )
            i = end_idx + 1
            continue
        sugg_whole = maybe_whole_array_assign(
            stmt, decl_bounds, blocked_whole_names, class_rank1_names, alloc_rank1_bounds
        )
        if sugg_whole is not None:
            findings.append(
                Finding(
                    path=unit.path,
                    rule="whole_array_assign",
                    unit_kind=unit.kind,
                    unit_name=unit.name,
                    start_line=ln,
                    end_line=ln,
                    suggestion=sugg_whole,
                )
            )
        pack = maybe_constructor_pack(
            body,
            i,
            decl_bounds,
            nondefault_real_rank1_names,
            scalar_nondefault_real_names,
            complex_rank1_names,
            character_scalar_names,
        )
        if pack is not None:
            end_idx, sugg_pack = pack
            findings.append(
                Finding(
                    path=unit.path,
                    rule="constructor_pack",
                    unit_kind=unit.kind,
                    unit_name=unit.name,
                    start_line=ln,
                    end_line=body[end_idx][0],
                    suggestion=sugg_pack,
                )
            )
            i = end_idx + 1
            continue
        pack_sparse = maybe_constructor_pack_sparse(
            body,
            i,
            nondefault_real_rank1_names,
            scalar_nondefault_real_names,
            complex_rank1_names,
            character_scalar_names,
        )
        if pack_sparse is not None:
            end_idx, sugg_pack = pack_sparse
            findings.append(
                Finding(
                    path=unit.path,
                    rule="constructor_pack_sparse",
                    unit_kind=unit.kind,
                    unit_name=unit.name,
                    start_line=ln,
                    end_line=body[end_idx][0],
                    suggestion=sugg_pack,
                )
            )
            i = end_idx + 1
            continue
        mdo = DO_RE.match(stmt.strip())
        if not mdo:
            i += 1
            continue
        loop_var = mdo.group(1)
        lb = mdo.group(2)
        ub = mdo.group(3)
        step = mdo.group(4)
        rng = build_range(lb, ub, step)

        # Form A-1: nested matmul matrix-vector:
        # do i=...
        #    y(i)=0
        #    do j=...
        #       y(i)=y(i)+A(i,j)*x(j)
        #    end do
        # end do
        if i + 5 < len(body):
            _ln_init, stmt_init = body[i + 1]
            _ln_ido, stmt_ido = body[i + 2]
            _ln_ibody, stmt_ibody = body[i + 3]
            ln_iend, stmt_iend = body[i + 4]
            ln_oend, stmt_oend = body[i + 5]
            if END_DO_RE.match(stmt_iend.strip()) and END_DO_RE.match(stmt_oend.strip()):
                sugg_mv = maybe_matmul_mv_nested(
                    loop_var, rng, stmt_init, stmt_ido, stmt_ibody, decl_bounds, rank2_bounds
                )
                if sugg_mv is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="nested_matmul_mv",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=ln,
                            end_line=ln_oend,
                            suggestion=sugg_mv,
                        )
                    )
                    i += 6
                    continue

        # Form A-2: nested matmul matrix-matrix:
        # do i=...
        #    do k=...
        #       c(i,k)=0
        #       do j=...
        #          c(i,k)=c(i,k)+a(i,j)*b(j,k)
        #       end do
        #    end do
        # end do
        if i + 7 < len(body):
            _ln_mdo, stmt_mdo = body[i + 1]
            _ln_init, stmt_init = body[i + 2]
            _ln_ido, stmt_ido = body[i + 3]
            _ln_ibody, stmt_ibody = body[i + 4]
            ln_iend, stmt_iend = body[i + 5]
            ln_mend, stmt_mend = body[i + 6]
            ln_oend, stmt_oend = body[i + 7]
            if (
                END_DO_RE.match(stmt_iend.strip())
                and END_DO_RE.match(stmt_mend.strip())
                and END_DO_RE.match(stmt_oend.strip())
            ):
                sugg_mm = maybe_matmul_mm_nested(
                    loop_var,
                    rng,
                    stmt_mdo,
                    stmt_init,
                    stmt_ido,
                    stmt_ibody,
                    decl_bounds,
                    rank2_bounds,
                )
                if sugg_mm is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="nested_matmul_mm",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=ln,
                            end_line=ln_oend,
                            suggestion=sugg_mm,
                        )
                    )
                    i += 8
                    continue

        # Form S3D: nested 3D replication via spread:
        # do k=...
        #    do i=...
        #       do j=...
        #          t(i,j,k) = c(i,j)
        #       end do
        #    end do
        # end do
        if i + 6 < len(body):
            _ln_mdo, stmt_mdo = body[i + 1]
            _ln_ido, stmt_ido = body[i + 2]
            _ln_ibody, stmt_ibody = body[i + 3]
            ln_iend, stmt_iend = body[i + 4]
            ln_mend, stmt_mend = body[i + 5]
            ln_oend, stmt_oend = body[i + 6]
            if (
                END_DO_RE.match(stmt_iend.strip())
                and END_DO_RE.match(stmt_mend.strip())
                and END_DO_RE.match(stmt_oend.strip())
            ):
                sugg_rand3 = maybe_random_number_nested_3d(
                    loop_var, lb, ub, step, stmt_mdo, stmt_ido, stmt_ibody
                )
                if sugg_rand3 is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="random_number_fill",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=ln,
                            end_line=ln_oend,
                            suggestion=sugg_rand3,
                        )
                    )
                    i += 7
                    continue
                sugg_s3d = maybe_spread_nested_3d(
                    loop_var,
                    lb,
                    ub,
                    step,
                    stmt_mdo,
                    stmt_ido,
                    stmt_ibody,
                    rank2_bounds,
                    rank3_bounds,
                )
                if sugg_s3d is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="nested_spread_3d",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=ln,
                            end_line=ln_oend,
                            suggestion=sugg_s3d,
                        )
                    )
                    i += 7
                    continue

        # Form S2D: nested 2D broadcasts via spread:
        # do i=...
        #    do j=...
        #       a(i,j) = ...
        #    end do
        # end do
        if i + 4 < len(body):
            _ln_ido, stmt_ido = body[i + 1]
            _ln_ibody, stmt_ibody = body[i + 2]
            ln_iend, stmt_iend = body[i + 3]
            ln_oend, stmt_oend = body[i + 4]
            if END_DO_RE.match(stmt_iend.strip()) and END_DO_RE.match(stmt_oend.strip()):
                sugg_rand2 = maybe_random_number_nested_2d(loop_var, lb, ub, step, stmt_ido, stmt_ibody)
                if sugg_rand2 is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="random_number_fill",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=ln,
                            end_line=ln_oend,
                            suggestion=sugg_rand2,
                        )
                    )
                    i += 5
                    continue
                sugg_s2d = maybe_spread_nested_2d(
                    loop_var, lb, ub, step, stmt_ido, stmt_ibody, decl_bounds, rank2_bounds
                )
                if sugg_s2d is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="nested_spread_2d",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=ln,
                            end_line=ln_oend,
                            suggestion=sugg_s2d,
                        )
                    )
                    i += 5
                    continue

        # Form A0: nested transpose:
        # do i=...
        #    do j=...
        #       y(j,i) = x(i,j)
        #    end do
        # end do
        if i + 4 < len(body):
            _ln_ido, stmt_ido = body[i + 1]
            _ln_ibody, stmt_ibody = body[i + 2]
            ln_iend, stmt_iend = body[i + 3]
            ln_oend, stmt_oend = body[i + 4]
            if END_DO_RE.match(stmt_iend.strip()) and END_DO_RE.match(stmt_oend.strip()):
                sugg_trans = maybe_nested_transpose(
                    loop_var, lb, ub, step, stmt_ido, stmt_ibody, rank2_bounds
                )
                if sugg_trans is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="nested_transpose",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=ln,
                            end_line=ln_oend,
                            suggestion=sugg_trans,
                        )
                    )
                    i += 5
                    continue

        # Form A: one-statement loop body.
        if i + 2 < len(body):
            ln_body, stmt_body = body[i + 1]
            ln_end, stmt_end = body[i + 2]
            if END_DO_RE.match(stmt_end.strip()):
                sugg_rand1 = maybe_random_number_loop_1d(
                    loop_var, rng, stmt_body, decl_bounds, rank2_bounds, rank3_bounds
                )
                if sugg_rand1 is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="random_number_fill",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=ln,
                            end_line=ln_end,
                            suggestion=sugg_rand1,
                        )
                    )
                    i += 3
                    continue
                sugg_ploop = maybe_print_loop(loop_var, lb, ub, step, stmt_body, decl_bounds)
                if sugg_ploop is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="print_loop",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=ln,
                            end_line=ln_end,
                            suggestion=sugg_ploop,
                        )
                    )
                    i += 3
                    continue
                sugg_elem = maybe_elementwise(loop_var, rng, stmt_body, decl_bounds, alloc_map, array_names)
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
                sugg_sum = maybe_reduction_sum(loop_var, rng, stmt_body, prev_stmt, decl_bounds, array_names)
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
                sugg_sum_vec2 = maybe_reduction_sum_vector_2d(
                    loop_var, rng, stmt_body, prev_stmt, decl_bounds, rank2_bounds
                )
                if sugg_sum_vec2 is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="reduction_sum_vector_2d",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=start_line,
                            end_line=ln_end,
                            suggestion=sugg_sum_vec2,
                        )
                    )
                    i += 3
                    continue
                sugg_prod = maybe_reduction_product(loop_var, rng, stmt_body, prev_stmt, decl_bounds, array_names)
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
                sugg_cnt_inline = maybe_count_inline(loop_var, rng, prev_stmt, stmt_body, array_names)
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
                sugg_find_inline = maybe_findloc_inline(loop_var, rng, step, prev_stmt, stmt_body, decl_bounds)
                if sugg_find_inline is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="reduction_findloc",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=start_line,
                            end_line=ln_end,
                            suggestion=sugg_find_inline,
                        )
                    )
                    i += 3
                    continue
                sugg_msum_inline = maybe_masked_sum_inline(
                    loop_var, rng, prev_stmt, stmt_body, decl_bounds, array_names
                )
                if sugg_msum_inline is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="reduction_masked_sum",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=start_line,
                            end_line=ln_end,
                            suggestion=sugg_msum_inline,
                        )
                    )
                    i += 3
                    continue
                sugg_mprod_inline = maybe_masked_product_inline(
                    loop_var, rng, prev_stmt, stmt_body, decl_bounds, array_names
                )
                if sugg_mprod_inline is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="reduction_masked_product",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=start_line,
                            end_line=ln_end,
                            suggestion=sugg_mprod_inline,
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
                sugg_temp_elem = maybe_temp_then_elementwise(
                    loop_var, rng, stmt_s1, stmt_s2, decl_bounds, alloc_map, array_names
                )
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
                sugg_cond = maybe_conditional_set(loop_var, rng, stmt_if, stmt_t, stmt_f, decl_bounds, array_names)
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
                sugg_msum = maybe_masked_sum(loop_var, rng, prev_stmt, stmt_if, stmt_inside, decl_bounds, array_names)
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
                sugg_find = maybe_findloc_block(loop_var, rng, step, prev_stmt, stmt_if, stmt_inside, decl_bounds)
                if sugg_find is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="reduction_findloc",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=start_line,
                            end_line=ln_end,
                            suggestion=sugg_find,
                        )
                    )
                    i += 5
                    continue
                sugg_mprod = maybe_masked_product(
                    loop_var, rng, prev_stmt, stmt_if, stmt_inside, decl_bounds, array_names
                )
                if sugg_mprod is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="reduction_masked_product",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=start_line,
                            end_line=ln_end,
                            suggestion=sugg_mprod,
                        )
                    )
                    i += 5
                    continue
                sugg_cnt = maybe_count(loop_var, rng, prev_stmt, stmt_if, stmt_inside, array_names)
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
        sugg_elem = maybe_elementwise(loop_var, rng, stmt_body, decl_bounds, {}, array_names)
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
        # Do not edit declaration lines participating in free-form continuations.
        # Reconstructing trailing '&' safely requires whole-statement rewriting.
        code_rstripped = code.rstrip()
        if code_rstripped.endswith("&"):
            continue
        if idx + 1 < len(new_lines) and is_continuation_only_line(new_lines[idx + 1]):
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


def annotate_file(path: Path, findings: List[Finding], *, backup: bool = True) -> Tuple[int, Optional[Path]]:
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
        # Also include any immediate trailing continuation-only lines.
        while eidx + 1 < len(lines) and is_continuation_only_line(lines[eidx + 1]):
            eidx += 1
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
    backup_path: Optional[Path] = None
    if backup:
        backup_path = make_backup_path(path)
        shutil.copy2(path, backup_path)
    path.write_text("".join(lines), encoding="utf-8")
    return inserted, backup_path


def apply_fix_file(
    path: Path,
    findings: List[Finding],
    *,
    annotate: bool = False,
    out_path: Optional[Path] = None,
    create_backup: bool = True,
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
        # Also include any immediate trailing continuation-only lines.
        while eidx + 1 < len(lines) and is_continuation_only_line(lines[eidx + 1]):
            eidx += 1
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
    backup: Optional[Path] = None
    target = out_path if out_path is not None else path
    # With explicit output path, do not create input backups.
    if out_path is None and create_backup:
        backup = make_backup_path(path)
        shutil.copy2(path, backup)
    target.write_text("".join(lines), encoding="utf-8")
    return changed, backup, removed_locals


def count_file_lines(path: Path) -> int:
    """Return line count for a text file (best effort)."""
    try:
        return len(path.read_text(encoding="utf-8").splitlines())
    except Exception:
        return 0


def main() -> int:
    """Run xarray advisory and optional annotation mode."""
    parser = argparse.ArgumentParser(
        description="Suggest replacing simple Fortran loops with array operations"
    )
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--verbose", action="store_true", help="Print full replacement suggestions")
    parser.add_argument(
        "--summary",
        action="store_true",
        help="Print per-file summary: file candidates before_lines after_lines delta",
    )
    parser.add_argument("--fix", action="store_true", help="Apply suggested replacements in-place")
    parser.add_argument("--out", type=Path, help="With --fix, write transformed output to this file (single input)")
    parser.add_argument("--annotate", action="store_true", help="Insert annotated suggestion blocks")
    parser.add_argument("--diff", action="store_true", help="With --fix, print unified diffs for changed files")
    parser.add_argument("--backup", dest="backup", action="store_true", default=True)
    parser.add_argument("--no-backup", dest="backup", action="store_false")
    parser.add_argument("--compiler", type=str, help="Compile command for baseline/after-fix validation")
    parser.add_argument("--limit", type=int, help="Maximum number of source files to process")
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
    if args.limit is not None and args.limit < 1:
        print("--limit must be >= 1.")
        return 2
    if args.out is not None:
        args.fix = True
    if args.diff and not args.fix:
        print("--diff requires --fix.")
        return 2
    if args.compiler and not args.fix:
        print("--compiler requires --fix.")
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
    baseline_compile_paths = files
    after_compile_paths = [args.out] if (args.fix and args.out is not None) else files
    if args.fix and args.compiler:
        if not fbuild.run_compiler_command(args.compiler, baseline_compile_paths, "baseline", fscan.display_path):
            return 5

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

    by_file_candidates: Dict[Path, List[Finding]] = {}
    for f in findings:
        by_file_candidates.setdefault(f.path, []).append(f)
    pre_lines: Dict[Path, int] = {p: count_file_lines(p) for p in files}

    if not findings:
        if args.summary:
            for p in files:
                before = pre_lines.get(p, 0)
                print(f"{fscan.display_path(p)} 0 {before} {before} 0")
        else:
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
        by_file = by_file_candidates
        touched = 0
        total = 0
        post_lines: Dict[Path, int] = {}
        for p in sorted(by_file.keys(), key=lambda x: x.name.lower()):
            before = p.read_text(encoding="utf-8")
            out_path = args.out if args.out is not None else None
            n, backup, removed_locals = apply_fix_file(
                p,
                by_file[p],
                annotate=args.annotate,
                out_path=out_path,
                create_backup=args.backup,
            )
            total += n
            if n > 0:
                touched += 1
                if out_path is not None:
                    print(f"\nFixed {p.name}: replaced {n} block(s), wrote {out_path}")
                else:
                    print(f"\nFixed {p.name}: replaced {n} block(s), backup {backup.name if backup else '(none)'}")
                if args.verbose and removed_locals:
                    print(f"  removed unused locals: {', '.join(removed_locals)}")
                if args.diff:
                    after = (out_path if out_path is not None else p).read_text(encoding="utf-8")
                    diff_lines = difflib.unified_diff(
                        before.splitlines(),
                        after.splitlines(),
                        fromfile=f"a/{p.name}",
                        tofile=f"b/{(out_path.name if out_path is not None else p.name)}",
                        lineterm="",
                    )
                    print("")
                    for line in diff_lines:
                        print(line)
            elif args.verbose:
                print(f"\nNo fixes applied in {p.name}")
            target = out_path if out_path is not None else p
            post_lines[p] = count_file_lines(target)
        print(f"\n--fix summary: files changed {touched}, replaced {total}")
        if args.summary:
            for p in files:
                before_n = pre_lines.get(p, 0)
                after_n = post_lines.get(p, before_n)
                cand_n = len(by_file_candidates.get(p, []))
                print(f"{fscan.display_path(p)} {cand_n} {before_n} {after_n} {after_n - before_n}")
    elif args.annotate:
        by_file = by_file_candidates
        total = 0
        touched = 0
        post_lines: Dict[Path, int] = {}
        for p in sorted(by_file.keys(), key=lambda x: x.name.lower()):
            n, backup = annotate_file(p, by_file[p], backup=args.backup)
            total += n
            if n > 0:
                touched += 1
                print(f"\nAnnotated {p.name}: inserted {n}, backup {backup.name if backup else '(none)'}")
            elif args.verbose:
                print(f"\nNo annotations inserted in {p.name}")
            post_lines[p] = count_file_lines(p)
        print(f"\n--annotate summary: files changed {touched}, inserted {total}")
        if args.summary:
            for p in files:
                before_n = pre_lines.get(p, 0)
                after_n = post_lines.get(p, before_n)
                cand_n = len(by_file_candidates.get(p, []))
                print(f"{fscan.display_path(p)} {cand_n} {before_n} {after_n} {after_n - before_n}")
    elif args.summary:
        for p in files:
            before_n = pre_lines.get(p, 0)
            cand_n = len(by_file_candidates.get(p, []))
            print(f"{fscan.display_path(p)} {cand_n} {before_n} {before_n} 0")
    if args.fix and args.compiler:
        if not fbuild.run_compiler_command(args.compiler, after_compile_paths, "after-fix", fscan.display_path):
            return 5
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
