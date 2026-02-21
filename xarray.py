#!/usr/bin/env python3
"""Advisory checker for simple DO loops replaceable by array operations."""

from __future__ import annotations

import argparse
import difflib
import math
import re
import shutil
import subprocess
import tempfile
from datetime import datetime
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Set, Tuple

import cli_paths as cpaths
import fortran_build as fbuild
import fortran_scan as fscan
import xalloc_assign
import xno_variable
import xunset

_DO_EXPR = r"(?:[^(),]|\([^()]*\))+"
DO_RE = re.compile(
    rf"^\s*do\s+([a-z][a-z0-9_]*)\s*=\s*({_DO_EXPR})\s*,\s*({_DO_EXPR})(?:\s*,\s*({_DO_EXPR}))?\s*$",
    re.IGNORECASE,
)
END_DO_RE = re.compile(r"^\s*end\s*do\b", re.IGNORECASE)
IF_THEN_RE = re.compile(r"^\s*if\s*\((.+)\)\s*then\s*$", re.IGNORECASE)
END_IF_RE = re.compile(r"^\s*end\s*if\b", re.IGNORECASE)
ELSE_RE = re.compile(r"^\s*else\b", re.IGNORECASE)
CYCLE_RE = re.compile(r"^\s*cycle\b", re.IGNORECASE)
EXIT_RE = re.compile(r"^\s*exit\b", re.IGNORECASE)
RETURN_RE = re.compile(r"^\s*return\b", re.IGNORECASE)
SELECT_CASE_RE = re.compile(r"^\s*select\s+case\b", re.IGNORECASE)
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
ONE_LITERAL_RE = re.compile(r"^\s*[+-]?1(?:\.0+)?(?:_[a-z0-9_]+)?\s*$", re.IGNORECASE)
NUM_LITERAL_RE = re.compile(
    r"^\s*[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[deq][+-]?\d+)?(?:_[a-z0-9_]+)?\s*$",
    re.IGNORECASE,
)
INT_LITERAL_RE = re.compile(r"^\s*([+-]?\d+)(?:_[a-z0-9_]+)?\s*$", re.IGNORECASE)
LOGICAL_LITERAL_RE = re.compile(r"^\s*\.(?:true|false)\.\s*$", re.IGNORECASE)
CHAR_LITERAL_RE = re.compile(r"^\s*(['\"]).*\1\s*$", re.IGNORECASE)

BEGIN_TAG = "!! beginning of code that xarray.py suggests replacing"
END_TAG = "!! end of code that xarray.py suggests replacing"
CHANGED_TAG = "!! changed by xarray.py"
REMOVED_TAG = "!! removed by xarray.py"
CHANGED_BLOCK_BEGIN = "! begin block changed by xarray.py"
CHANGED_BLOCK_END = "! end block changed by xarray.py"
REMOVED_BLOCK_BEGIN = "! begin block removed by xarray.py"
REMOVED_BLOCK_END = "! end block removed by xarray.py"
AGGRESSIVE_MODE = False


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
    insert_only: bool = False


CMP_RE = re.compile(
    r"^\s*(.+?)\s*(<=|>=|<|>|\.lt\.|\.le\.|\.gt\.|\.ge\.)\s*(.+?)\s*$",
    re.IGNORECASE,
)
EQ_RE = re.compile(r"^\s*(.+?)\s*(==|\.eq\.)\s*(.+?)\s*$", re.IGNORECASE)
IDENT_RE = re.compile(r"\b([a-z][a-z0-9_]*)\b", re.IGNORECASE)
CALL_LIKE_RE = re.compile(r"\b([a-z][a-z0-9_]*)\s*\(", re.IGNORECASE)
INDEXED_CONST_RE = re.compile(r"^\s*([a-z][a-z0-9_]*)\s*\(\s*(\d+)\s*\)\s*$", re.IGNORECASE)
INDEXED_ANY_RE = re.compile(r"^\s*([a-z][a-z0-9_]*)\s*\(\s*([^)]+)\s*\)\s*$", re.IGNORECASE)
PRINT_PREFIX_RE = re.compile(r"^\s*print\s*\*\s*,\s*(.+)\s*$", re.IGNORECASE)
READ_PREFIX_RE = re.compile(r"^\s*read\s*\*\s*,\s*(.+)\s*$", re.IGNORECASE)
PRINT_STMT_RE = re.compile(r"^\s*print\s+(.+)$", re.IGNORECASE)
LOOP_ASSIGN_RE = re.compile(r"^\s*([a-z][a-z0-9_]*)\s*=\s*(.+)$", re.IGNORECASE)
RANDOM_CALL_RE = re.compile(r"^\s*call\s+random_number\s*\(\s*(.+)\s*\)\s*$", re.IGNORECASE)
CALL_STMT_RE = re.compile(r"^\s*call\s+([a-z][a-z0-9_]*)\s*\((.*)\)\s*$", re.IGNORECASE)

# Conservative set of elemental intrinsics that are safe in array expressions.
ELEMENTAL_INTRINSICS = {
    "abs",
    "acos",
    "acosd",
    "acosh",
    "asin",
    "asind",
    "asinh",
    "atan",
    "atan2",
    "atan2d",
    "atand",
    "atanh",
    "aint",
    "anint",
    "bessel_j0",
    "bessel_j1",
    "bessel_y0",
    "bessel_y1",
    "ceiling",
    "cmplx",
    "conjg",
    "cos",
    "cosd",
    "cosh",
    "dble",
    "dim",
    "dprod",
    "exp",
    "floor",
    "iachar",
    "ichar",
    "int",
    "log",
    "log10",
    "log_gamma",
    "max",
    "min",
    "mod",
    "modulo",
    "norm2",
    "nint",
    "real",
    "reshape",
    "sign",
    "sin",
    "sind",
    "sinh",
    "size",
    "lbound",
    "ubound",
    "shape",
    "rank",
    "kind",
    "spacing",
    "spread",
    "matmul",
    "sqrt",
    "tan",
    "tand",
    "tanh",
    "gamma",
}

# Per-file user-defined elemental function names (lowercase).
USER_ELEMENTAL_CALLS: Set[str] = set()
USER_ELEMENTAL_SUBROUTINES: Set[str] = set()


LETTER_RANGE_TOKEN_RE = re.compile(r"\[([a-zA-Z\*]?):([a-zA-Z\*]?)\]")
NUM_TOKEN_RE = re.compile(r"[+-]?(?:\d+\.\d*|\.\d+|\d+)(?:[eEdD][+-]?\d+)?")


def translate_single_letter_range_glob(raw: str) -> str:
    """Translate xarray pseudo-glob letter ranges to standard glob classes.

    Supported tokens:
    - [d:z] -> [d-z]
    - [d:]  -> [d-z]
    - [:m]  -> [a-m]
    - [d:*] -> [d-z]*
    - [*:m] -> [a-m]*
    """
    def repl(m: re.Match[str]) -> str:
        lo = m.group(1)
        hi = m.group(2)
        star_suffix = ""
        if hi == "*":
            hi = ""
            star_suffix = "*"
        if lo == "*":
            lo = ""
            star_suffix = "*"
        if not lo and not hi:
            return m.group(0)
        if not lo:
            lo = "a" if hi.islower() else "A"
        if not hi:
            hi = "z" if lo.islower() else "Z"
        if ord(lo) > ord(hi):
            lo, hi = hi, lo
        return f"[{lo}-{hi}]{star_suffix}"

    return LETTER_RANGE_TOKEN_RE.sub(repl, raw)


def choose_files(args_files: List[Path], exclude: Iterable[str]) -> List[Path]:
    """Resolve source files from args or current-directory defaults."""
    if args_files:
        translated = [Path(translate_single_letter_range_glob(str(p))) for p in args_files]
        files = cpaths.expand_source_inputs(translated)
    else:
        files = sorted(
            set(Path(".").glob("*.f90")) | set(Path(".").glob("*.F90")),
            key=lambda p: p.name.lower(),
        )
    return fscan.apply_excludes(files, exclude)


def outputs_match_tolerant(
    a: str,
    b: str,
    *,
    rtol: float = 1.0e-6,
    atol: float = 1.0e-9,
) -> bool:
    """Compare output text allowing tiny floating-point formatting/roundoff drift."""
    if a == b:
        return True
    a_txt = a.rstrip()
    b_txt = b.rstrip()
    if a_txt == b_txt:
        return True
    # Non-numeric parts must match exactly for conservative behavior.
    a_non = NUM_TOKEN_RE.sub("#", a_txt)
    b_non = NUM_TOKEN_RE.sub("#", b_txt)
    if a_non != b_non:
        return False
    a_nums = NUM_TOKEN_RE.findall(a_txt)
    b_nums = NUM_TOKEN_RE.findall(b_txt)
    if len(a_nums) != len(b_nums):
        return False
    try:
        for xa, xb in zip(a_nums, b_nums):
            va = float(xa.replace("d", "e").replace("D", "E"))
            vb = float(xb.replace("d", "e").replace("D", "E"))
            if not math.isclose(va, vb, rel_tol=rtol, abs_tol=atol):
                return False
    except Exception:
        return False
    return True


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


def read_text_flexible(path: Path) -> str:
    """Read text via shared scan helper with fallback encodings."""
    return fscan.read_text_flexible(path)


def is_continuation_only_line(line: str) -> bool:
    """True when a physical line is a free-form continuation starter (`& ...`)."""
    code, _comment = split_code_comment(line)
    return bool(re.match(r"^\s*&", code))


def has_trailing_continuation_amp(line: str) -> bool:
    """True when statement text (before comment) ends with free-form '&'."""
    code, _comment = split_code_comment(line)
    return code.rstrip().endswith("&")


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


def replace_index_var_in_multidim_refs(expr: str, idx_var: str, lb: str, ub: str) -> str:
    """Replace exact index variable in rank-2/3 array refs with lb:ub section."""
    sec = f"{lb.strip()}:{ub.strip()}"

    def repl2(m: re.Match[str]) -> str:
        nm = m.group(1)
        a1 = m.group(2).strip()
        a2 = m.group(3).strip()
        if normalize_expr(a1) == normalize_expr(idx_var):
            a1 = sec
        if normalize_expr(a2) == normalize_expr(idx_var):
            a2 = sec
        return f"{nm}({a1}, {a2})"

    out = re.sub(
        rf"\b([a-z][a-z0-9_]*)\s*\(\s*([^,()]+)\s*,\s*([^,()]+)\s*\)",
        repl2,
        expr,
        flags=re.IGNORECASE,
    )

    def repl3(m: re.Match[str]) -> str:
        nm = m.group(1)
        args = [m.group(2).strip(), m.group(3).strip(), m.group(4).strip()]
        args = [sec if normalize_expr(a) == normalize_expr(idx_var) else a for a in args]
        return f"{nm}({args[0]}, {args[1]}, {args[2]})"

    out = re.sub(
        rf"\b([a-z][a-z0-9_]*)\s*\(\s*([^,()]+)\s*,\s*([^,()]+)\s*,\s*([^,()]+)\s*\)",
        repl3,
        out,
        flags=re.IGNORECASE,
    )
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


def section_extent_expr(sec: str) -> Optional[str]:
    """Return extent expression for a simple section lo:hi (no explicit step)."""
    txt = sec.strip()
    parts = [p.strip() for p in txt.split(":")]
    if len(parts) != 2:
        return None
    lo, hi = parts
    if not lo or not hi:
        return None
    if normalize_expr(lo) == normalize_expr("1"):
        return hi
    return f"({hi})-({lo})+1"


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


def parse_one_line_if(stmt: str) -> Optional[Tuple[str, str]]:
    """Parse one-line IF into (condition, body_stmt), handling nested parentheses."""
    s = stmt.strip()
    m_if = re.match(r"^\s*if\b", s, re.IGNORECASE)
    if not m_if:
        return None
    open_pos = s.find("(", m_if.end())
    if open_pos < 0:
        return None
    close_pos = find_matching_paren(s, open_pos)
    if close_pos < 0:
        return None
    cond = s[open_pos + 1 : close_pos].strip()
    body = s[close_pos + 1 :].strip()
    if not cond or not body:
        return None
    return cond, body


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


def canonical_relop(op: str) -> str:
    """Map Fortran symbolic/dotted relational operators to symbolic form."""
    o = normalize_expr(op)
    if o == ".lt.":
        return "<"
    if o == ".le.":
        return "<="
    if o == ".gt.":
        return ">"
    if o == ".ge.":
        return ">="
    return o


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


def collect_rank1_character_array_names(unit: xunset.Unit) -> Set[str]:
    """Collect rank-1 CHARACTER array entity names in one unit."""
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
    simp_bounds = dict(decl_bounds)
    simp_bounds.update(alloc_rank1_bounds)
    rhs_s = simplify_section_expr(rhs, simp_bounds)
    return f"{name} = {rhs_s}"


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


def parse_scalar_decl_initializers(unit: xunset.Unit) -> Dict[str, str]:
    """Collect scalar declaration initializers: name -> rhs text (lower-cased parse)."""
    out: Dict[str, str] = {}
    for _ln, stmt in unit.body:
        low = stmt.strip().lower()
        if not low or not TYPE_DECL_RE.match(low):
            continue
        if "::" not in low:
            continue
        rhs = low.split("::", 1)[1]
        for chunk in split_top_level_commas(rhs):
            txt = chunk.strip()
            if not txt or "=" not in txt or "=>" in txt:
                continue
            lhs_txt, rhs_txt = txt.split("=", 1)
            lhs_txt = lhs_txt.strip()
            rhs_txt = rhs_txt.strip()
            mname = re.match(r"^([a-z][a-z0-9_]*)", lhs_txt, re.IGNORECASE)
            if not mname:
                continue
            # Scalar-only declaration entity.
            rest = lhs_txt[mname.end() :].lstrip()
            if rest.startswith("("):
                continue
            out.setdefault(mname.group(1).lower(), rhs_txt)
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
    # Conservative shape guard for constructor SOURCE:
    # only allow when allocate bounds are literal and match constructor size.
    ctor_vals = parse_array_constructor_values(rhs)
    if ctor_vals is not None:
        m_obj = re.match(r"^\s*[a-z][a-z0-9_]*\s*\((.*)\)\s*$", obj, re.IGNORECASE)
        if m_obj is None:
            return None
        dims = split_top_level_commas(m_obj.group(1))
        if len(dims) != 1:
            return None
        d = dims[0].strip()
        # rank-1 shape-spec: either "ub" or "lb:ub" with integer literals only.
        size_lit: Optional[int] = None
        if ":" in d:
            lo_s, hi_s = [t.strip() for t in d.split(":", 1)]
            if not (re.fullmatch(r"[+-]?\d+", lo_s) and re.fullmatch(r"[+-]?\d+", hi_s)):
                return None
            lo_i = int(lo_s)
            hi_i = int(hi_s)
            if hi_i < lo_i:
                return None
            size_lit = hi_i - lo_i + 1
        else:
            if not re.fullmatch(r"[+-]?\d+", d):
                return None
            ub_i = int(d)
            if ub_i < 0:
                return None
            size_lit = ub_i
        if size_lit is None or size_lit != len(ctor_vals):
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
        # Prefer MATMUL form for simple 2D broadcast created above:
        # spread(v(1:n), dim=2, ncopies=m) -> matmul(reshape(v(1:n), [n,1]), reshape(spread(1.0,...), [1,m]))
        if len(lhs_args_sliced) == 2:
            if lhs_pos == 0:
                rows = section_extent_expr(lhs_args_sliced[0].strip())
                m_sp = re.match(
                    rf"^\s*spread\(\s*([a-z][a-z0-9_]*)\s*\(\s*{re.escape(lhs_args_sliced[0].strip())}\s*\)\s*,\s*dim\s*=\s*2\s*,\s*ncopies\s*=\s*(.+)\)\s*$",
                    rhs_sliced,
                    re.IGNORECASE,
                )
                if m_sp and rows is not None:
                    cols = m_sp.group(2).strip()
                    rhs_sliced = (
                        f"matmul("
                        f"reshape({m_sp.group(1)}({lhs_args_sliced[0].strip()}), [{rows}, 1]), "
                        f"reshape(spread(1.0, dim=1, ncopies={cols}), [1, {cols}])"
                        f")"
                    )
            elif lhs_pos == 1:
                cols = section_extent_expr(lhs_args_sliced[1].strip())
                m_sp = re.match(
                    rf"^\s*spread\(\s*([a-z][a-z0-9_]*)\s*\(\s*{re.escape(lhs_args_sliced[1].strip())}\s*\)\s*,\s*dim\s*=\s*1\s*,\s*ncopies\s*=\s*(.+)\)\s*$",
                    rhs_sliced,
                    re.IGNORECASE,
                )
                if m_sp and cols is not None:
                    rows = m_sp.group(2).strip()
                    rhs_sliced = (
                        f"matmul("
                        f"reshape(spread(1.0, dim=1, ncopies={rows}), [{rows}, 1]), "
                        f"reshape({m_sp.group(1)}({lhs_args_sliced[1].strip()}), [1, {cols}])"
                        f")"
                    )
        # Fallback broadcast for rank-2 target when RHS is a simple rank-1 section
        # aligned with the looped dimension, e.g.
        #   a(i, j1:j2) = key(i)  ->  a(1:n, j1:j2) = spread(key(1:n), dim=2, ncopies=j2-j1+1)
        rhs_idx = parse_indexed_name(rhs_sliced)
        if rhs_idx is not None and len(rhs_idx[1]) == 1:
            rarg = rhs_idx[1][0].strip()
            if lhs_pos == 0:
                sec_other = lhs_args_sliced[1].strip()
                extent = section_extent_expr(sec_other)
                rows = section_extent_expr(lhs_args_sliced[0].strip())
                if (
                    extent is not None
                    and rows is not None
                    and normalize_expr(rarg) == normalize_expr(lhs_args_sliced[0].strip())
                ):
                    rhs_sliced = (
                        f"matmul("
                        f"reshape({rhs_idx[0]}({rarg}), [{rows}, 1]), "
                        f"reshape(spread(1.0, dim=1, ncopies={extent}), [1, {extent}])"
                        f")"
                    )
            elif lhs_pos == 1:
                sec_other = lhs_args_sliced[0].strip()
                extent = section_extent_expr(sec_other)
                cols = section_extent_expr(lhs_args_sliced[1].strip())
                if (
                    extent is not None
                    and cols is not None
                    and normalize_expr(rarg) == normalize_expr(lhs_args_sliced[1].strip())
                ):
                    rhs_sliced = (
                        f"matmul("
                        f"reshape(spread(1.0, dim=1, ncopies={extent}), [{extent}, 1]), "
                        f"reshape({rhs_idx[0]}({rarg}), [1, {cols}])"
                        f")"
                    )
    rhs_sliced = simplify_section_expr(rhs_sliced, decl_bounds)
    if has_disallowed_function_calls(rhs_sliced, array_names):
        return None
    return f"{lhs_expr} = {rhs_sliced}"


def maybe_elemental_subroutine_call(
    loop_var: str,
    rng: str,
    body_stmt: str,
    decl_bounds: Dict[str, str],
    array_names: Set[str],
) -> Optional[str]:
    """Detect looped calls to an elemental subroutine and suggest array call."""
    m = CALL_STMT_RE.match(body_stmt.strip())
    if not m:
        return None
    callee = m.group(1).lower()
    if callee not in USER_ELEMENTAL_SUBROUTINES:
        return None
    args_raw = m.group(2).strip()
    args = split_top_level_commas(args_raw) if args_raw else []
    if not args:
        return None
    lb, ubtxt = split_range(rng)
    out_args: List[str] = []
    changed_any = False
    for a in args:
        at = a.strip()
        # Conservative: skip keyword arguments.
        if re.search(r"(?<![<>=/])=(?![=])", at):
            return None
        as_s = replace_affine_index_anydim(at, loop_var, lb, ubtxt, array_names=array_names)
        if has_loop_var(as_s, loop_var):
            return None
        if as_s != at:
            changed_any = True
        as_s = simplify_section_expr(as_s, decl_bounds)
        out_args.append(as_s)
    if not changed_any:
        return None
    return f"call {callee}({', '.join(out_args)})"


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


def maybe_reshape_from_vector_fill(
    loop_var: str,
    lb: str,
    ub: str,
    step: Optional[str],
    stmt_temp: str,
    stmt_body: str,
    rank2_bounds: Dict[str, Tuple[str, str]],
) -> Optional[str]:
    """
    Detect:
      do j=1,n2
         k = 1 + (j-1)*n1
         a(:,j) = x(k:k+n1-1)
      end do
    and rewrite to:
      a = reshape(x, [n1, n2])
    """
    if lb.strip() != "1":
        return None
    if step is not None and step.strip() not in ("", "1"):
        return None

    m1 = ASSIGN_RE.match(stmt_temp.strip())
    m2 = ASSIGN_RE.match(stmt_body.strip())
    if not m1 or not m2:
        return None
    m_temp = SIMPLE_NAME_RE.match(m1.group(1).strip())
    if not m_temp:
        return None
    temp_name = m_temp.group(1)
    temp_rhs = m1.group(2).strip()

    lhs = m2.group(1).strip()
    rhs = m2.group(2).strip()
    lhs_idx = parse_indexed_name(lhs)
    rhs_idx = parse_indexed_name(rhs)
    if lhs_idx is None or rhs_idx is None:
        return None
    lhs_name, lhs_args = lhs_idx
    rhs_name, rhs_args = rhs_idx
    if len(lhs_args) != 2 or len(rhs_args) != 1:
        return None
    if lhs_args[0].strip() != ":" or lhs_args[1].strip().lower() != loop_var.lower():
        return None

    b2 = rank2_bounds.get(lhs_name.lower())
    if b2 is None:
        return None
    nrow, ncol = b2
    if normalize_expr(ub) != normalize_expr(ncol):
        return None

    sec = rhs_args[0].strip()
    if ":" not in sec:
        return None
    lo, hi = [p.strip() for p in sec.split(":", 1)]
    if normalize_expr(lo) != normalize_expr(temp_name):
        return None

    expected_hi = normalize_expr(f"{temp_name} + {nrow} - 1")
    if normalize_expr(hi) != expected_hi:
        return None

    expected_temp_1 = normalize_expr(f"1 + ({loop_var} - 1) * {nrow}")
    expected_temp_2 = normalize_expr(f"({loop_var} - 1) * {nrow} + 1")
    temp_norm = normalize_expr(temp_rhs)
    if temp_norm not in (expected_temp_1, expected_temp_2):
        return None

    return f"{lhs_name} = reshape({rhs_name}, [{nrow}, {ncol}])"


def maybe_nested_reshape_from_vector_fill(
    outer_loop_var: str,
    outer_lb: str,
    outer_ub: str,
    outer_step: Optional[str],
    stmt_temp: str,
    inner_do_stmt: str,
    inner_body_stmt: str,
    rank2_bounds: Dict[str, Tuple[str, str]],
) -> Optional[str]:
    """
    Detect:
      do j=1,n2
         k = 1 + (j-1)*n1
         do i=1,n1
            a(i,j) = x(k+i-1)
         end do
      end do
    and rewrite to:
      a = reshape(x, [n1, n2])
    """
    if outer_lb.strip() != "1":
        return None
    if outer_step is not None and outer_step.strip() not in ("", "1"):
        return None

    m_tmp = ASSIGN_RE.match(stmt_temp.strip())
    m_ido = DO_RE.match(inner_do_stmt.strip())
    m_asn = ASSIGN_RE.match(inner_body_stmt.strip())
    if not m_tmp or not m_ido or not m_asn:
        return None

    m_tmp_lhs = SIMPLE_NAME_RE.match(m_tmp.group(1).strip())
    if not m_tmp_lhs:
        return None
    temp_name = m_tmp_lhs.group(1)
    temp_rhs = m_tmp.group(2).strip()

    inner_var = m_ido.group(1)
    inner_lb = m_ido.group(2).strip()
    inner_ub = m_ido.group(3).strip()
    inner_step = m_ido.group(4)
    if inner_lb != "1":
        return None
    if inner_step is not None and inner_step.strip() not in ("", "1"):
        return None

    lhs = m_asn.group(1).strip()
    rhs = m_asn.group(2).strip()
    lhs_idx = parse_indexed_name(lhs)
    rhs_idx = parse_indexed_name(rhs)
    if lhs_idx is None or rhs_idx is None:
        return None
    lhs_name, lhs_args = lhs_idx
    rhs_name, rhs_args = rhs_idx
    if len(lhs_args) != 2 or len(rhs_args) != 1:
        return None
    if lhs_args[0].strip().lower() != inner_var.lower():
        return None
    if lhs_args[1].strip().lower() != outer_loop_var.lower():
        return None

    b2 = rank2_bounds.get(lhs_name.lower())
    if b2 is None:
        return None
    nrow, ncol = b2
    if normalize_expr(outer_ub) != normalize_expr(ncol):
        return None
    if normalize_expr(inner_ub) != normalize_expr(nrow):
        return None

    rhs_arg = rhs_args[0].strip()
    expected_rhs_a = normalize_expr(f"{temp_name} + {inner_var} - 1")
    expected_rhs_b = normalize_expr(f"{inner_var} + {temp_name} - 1")
    if normalize_expr(rhs_arg) not in (expected_rhs_a, expected_rhs_b):
        return None

    expected_temp_1 = normalize_expr(f"1 + ({outer_loop_var} - 1) * {nrow}")
    expected_temp_2 = normalize_expr(f"({outer_loop_var} - 1) * {nrow} + 1")
    if normalize_expr(temp_rhs) not in (expected_temp_1, expected_temp_2):
        return None

    return f"{lhs_name} = reshape({rhs_name}, [{nrow}, {ncol}])"


def maybe_multi_elementwise(
    loop_var: str,
    rng: str,
    body_stmts: List[str],
    decl_bounds: Dict[str, str],
    alloc_map: Dict[str, str],
    array_names: Set[str],
) -> Optional[str]:
    """Detect multiple independent elementwise assignments in one loop body."""
    if len(body_stmts) < 2:
        return None
    suggestions: List[str] = []
    lhs_names: List[str] = []
    lhs_targets: Set[str] = set()
    lhs_targets: Set[str] = set()
    lhs_targets: Set[str] = set()
    for stmt in body_stmts:
        m = ASSIGN_RE.match(stmt.strip())
        if not m:
            return None
        lhs = m.group(1).strip()
        rhs = m.group(2).strip()
        lhs_parsed = parse_lhs_indexed_by_loop(lhs, loop_var)
        if lhs_parsed is None:
            return None
        lhs_name, _lhs_args, _lhs_pos = lhs_parsed
        lhs_low = lhs_name.lower()
        # Conservative: require distinct assignment targets.
        if lhs_low in lhs_names:
            return None
        lhs_names.append(lhs_low)
        suggestions.append(maybe_elementwise(loop_var, rng, stmt, decl_bounds, alloc_map, array_names) or "")
        if not suggestions[-1]:
            return None
        # Conservative: reject if RHS references any target written in the same loop.
        rhs_low = strip_quoted_text(rhs.lower())
        for w in lhs_names:
            if re.search(rf"\b{re.escape(w)}\s*\(", rhs_low):
                return None
    return "\n".join(suggestions)


def maybe_temp_then_multi_elementwise(
    loop_var: str,
    rng: str,
    body_stmts: List[str],
    decl_bounds: Dict[str, str],
    rank2_bounds: Dict[str, Tuple[str, str]],
    decl_scalar_inits: Dict[str, str],
    alloc_map: Dict[str, str],
    array_names: Set[str],
) -> Optional[str]:
    """Detect temp-scalar assignment followed by multiple elementwise assignments."""
    if len(body_stmts) < 3:
        return None
    m0 = ASSIGN_RE.match(body_stmts[0].strip())
    if not m0:
        return None
    m0_lhs = SIMPLE_NAME_RE.match(m0.group(1).strip())
    if not m0_lhs:
        return None
    temp_name = m0_lhs.group(1)
    temp_rhs = m0.group(2).strip()
    # Temp must vary with loop variable so inlining is meaningful.
    if not has_loop_var(temp_rhs, loop_var):
        return None

    def _const_int_expr(expr: str) -> Optional[int]:
        e = expr.strip().lower()
        if re.fullmatch(r"[a-z][a-z0-9_]*", e):
            rhs = decl_scalar_inits.get(e)
            if rhs is None:
                return None
            return _const_int_expr(rhs)
        if not re.fullmatch(r"[0-9+\-*\s()]+", e):
            return None
        try:
            v = eval(e, {"__builtins__": {}}, {})  # noqa: S307 - tightly regex-guarded
        except Exception:
            return None
        if isinstance(v, bool):
            return None
        if isinstance(v, int):
            return int(v)
        return None

    # Inline temp expression into subsequent statements. Prefer normal
    # elementwise rewrite; fall back to constructor-based slice fill when the
    # RHS uses loop_var as a scalar (e.g. sin(real(i))).
    suggestions: List[str] = []
    lhs_names: List[str] = []
    lhs_targets: Set[str] = set()
    lb, ubtxt = split_range(rng)
    step_txt: Optional[str] = None
    m_rng = re.match(r"^\s*(.+?)\s*:\s*(.+?)\s*(?::\s*(.+?)\s*)?$", rng)
    if m_rng:
        step_txt = m_rng.group(3).strip() if m_rng.group(3) else None
    parsed_rows: List[Tuple[Tuple[str, List[str], int], str]] = []
    for stmt in body_stmts[1:]:
        m = ASSIGN_RE.match(stmt.strip())
        if not m:
            return None
        lhs = m.group(1).strip()
        rhs = m.group(2).strip()
        lhs_parsed = parse_lhs_indexed_by_loop(lhs, loop_var)
        if lhs_parsed is None:
            return None
        lhs_name = lhs_parsed[0].lower()
        lhs_target = normalize_expr(lhs)
        if lhs_name == temp_name.lower():
            return None
        if lhs_target in lhs_targets:
            return None
        lhs_targets.add(lhs_target)
        lhs_names.append(lhs_name)

        rhs_inlined = re.sub(
            rf"\b{re.escape(temp_name)}\b",
            f"({temp_rhs})",
            rhs,
            flags=re.IGNORECASE,
        )
        # Prevent dependence on values written earlier in this same loop body.
        rhs_low = strip_quoted_text(rhs_inlined.lower())
        for w in lhs_names:
            if re.search(rf"\b{re.escape(w)}\s*\(", rhs_low):
                return None
        parsed_rows.append((lhs_parsed, rhs_inlined))
        stmt_inlined = f"{lhs} = {rhs_inlined}"
        sugg = maybe_elementwise(loop_var, rng, stmt_inlined, decl_bounds, alloc_map, array_names)
        if sugg is None:
            # Fallback: loop var appears as scalar RHS term; use constructor.
            if not has_loop_var(rhs_inlined, loop_var):
                return None
            lhs_args_sliced = list(lhs_parsed[1])
            lhs_args_sliced[lhs_parsed[2]] = rng
            if len(lhs_args_sliced) == 1:
                lhs_expr = f"{lhs_parsed[0]}({rng})"
                bnd = decl_bounds.get(lhs_parsed[0].lower())
                if bnd is not None and normalize_expr(rng) == normalize_expr(bnd):
                    lhs_expr = lhs_parsed[0]
                elif can_collapse_lhs_alloc(lhs_parsed[0], rng, alloc_map):
                    lhs_expr = lhs_parsed[0]
            else:
                lhs_expr = f"{lhs_parsed[0]}({', '.join(lhs_args_sliced)})"
            iter_triplet = f"{loop_var}={lb},{ubtxt}"
            if step_txt not in (None, "", "1"):
                iter_triplet = f"{loop_var}={lb},{ubtxt},{step_txt}"
            rhs_ctor = f"[({rhs_inlined}, {iter_triplet})]"
            rhs_ctor = simplify_section_expr(rhs_ctor, decl_bounds)
            if has_disallowed_function_calls(rhs_ctor, array_names):
                return None
            sugg = f"{lhs_expr} = {rhs_ctor}"
        suggestions.append(sugg)

    # Prefer compact "loop + row/column constructor" when all writes target
    # fixed neighboring columns/rows of the same rank-2 array.
    if parsed_rows:
        arr0 = parsed_rows[0][0][0]
        lhs_pos0 = parsed_rows[0][0][2]
        if all(len(pr[0][1]) == 2 and pr[0][0].lower() == arr0.lower() and pr[0][2] == lhs_pos0 for pr in parsed_rows):
            other_pos = 1 - lhs_pos0
            idx_rhs: Dict[int, str] = {}
            ok = True
            for lhs_parsed, rhs_inlined in parsed_rows:
                idx_expr = lhs_parsed[1][other_pos]
                idx_val = _const_int_expr(idx_expr)
                if idx_val is None or idx_val in idx_rhs:
                    ok = False
                    break
                idx_rhs[idx_val] = rhs_inlined
            if ok and idx_rhs:
                idxs = sorted(idx_rhs.keys())
                contiguous = all(idxs[k] == idxs[0] + k for k in range(len(idxs)))
                if contiguous:
                    rank2 = rank2_bounds.get(arr0.lower())
                    full_other = False
                    if rank2 is not None:
                        b_other = rank2[other_pos]
                        b_other_v = _const_int_expr(b_other)
                        if b_other_v is not None and idxs[0] == 1 and idxs[-1] == b_other_v:
                            full_other = True
                    idx_join = ", ".join(idx_rhs[i] for i in idxs)
                    if lhs_pos0 == 0:
                        sec = ":" if full_other else f"{idxs[0]}:{idxs[-1]}"
                        lhs_compact = f"{arr0}({loop_var}, {sec})"
                    else:
                        sec = ":" if full_other else f"{idxs[0]}:{idxs[-1]}"
                        lhs_compact = f"{arr0}({sec}, {loop_var})"
                    do_head = f"do {loop_var}={lb},{ubtxt}"
                    if step_txt not in (None, "", "1"):
                        do_head = f"{do_head},{step_txt}"
                    return "\n".join(
                        [
                            do_head,
                            f"   {body_stmts[0].strip()}",
                            f"   {lhs_compact} = [{idx_join}]",
                            "end do",
                        ]
                    )

    return "\n".join(suggestions) if suggestions else None


def _normalize_affine_subscripts(
    expr: str, loop_var: str
) -> Tuple[Optional[str], Optional[List[int]]]:
    """Normalize rank-1 affine subscripts to IDX and return collected deltas."""
    out: List[str] = []
    deltas: List[int] = []
    i = 0
    n = len(expr)
    lv = loop_var.lower()
    while i < n:
        m = re.match(r"[a-z][a-z0-9_]*", expr[i:], re.IGNORECASE)
        if not m:
            ch = expr[i]
            # Any remaining loop-var token outside recognized rank-1 subscripts
            # makes this pattern unsafe.
            if re.match(r"[a-z]", ch, re.IGNORECASE):
                m_id = re.match(r"[a-z][a-z0-9_]*", expr[i:], re.IGNORECASE)
                if m_id and m_id.group(0).lower() == lv:
                    return None, None
            out.append(ch)
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
        close = find_matching_paren(expr, j)
        if close < 0:
            return None, None
        inside = expr[j + 1 : close]
        args = split_top_level_commas(inside)
        if len(args) != 1:
            return None, None
        delta = parse_affine_loop_index(args[0], loop_var)
        if delta is None:
            return None, None
        deltas.append(delta)
        out.append(f"{name}(IDX)")
        i = close + 1
    return "".join(out), deltas


def maybe_unrolled_elementwise_chunked(
    loop_var: str,
    lb: str,
    ub: str,
    step: Optional[str],
    body_stmts: List[str],
    array_names: Set[str],
    *,
    aggressive: bool,
) -> Optional[str]:
    """Detect fixed-width unrolled elementwise loop and collapse it."""
    if step is None:
        return None
    step_txt = step.strip()
    if not re.fullmatch(r"[+-]?\d+", step_txt):
        return None
    k = int(step_txt)
    if k <= 1:
        return None
    if len(body_stmts) != k:
        return None

    lhs_name: Optional[str] = None
    rhs_norm_ref: Optional[str] = None
    rhs_ref: Optional[str] = None
    seen_lhs_deltas: Set[int] = set()
    required_deltas = set(range(k))

    for st in body_stmts:
        m = ASSIGN_RE.match(st.strip())
        if not m:
            return None
        lhs = m.group(1).strip()
        rhs = m.group(2).strip()
        lhs_parsed = parse_indexed_name(lhs)
        if lhs_parsed is None:
            return None
        l_name, l_args = lhs_parsed
        if len(l_args) != 1:
            return None
        d_lhs = parse_affine_loop_index(l_args[0], loop_var)
        if d_lhs is None:
            return None
        seen_lhs_deltas.add(d_lhs)
        if lhs_name is None:
            lhs_name = l_name
        elif l_name.lower() != lhs_name.lower():
            return None

        # Avoid potential write-after-read ambiguity on target.
        if re.search(rf"\b{re.escape(l_name)}\s*\(", strip_quoted_text(rhs), re.IGNORECASE):
            return None

        rhs_norm, rhs_deltas = _normalize_affine_subscripts(rhs, loop_var)
        if rhs_norm is None or rhs_deltas is None or not rhs_deltas:
            return None
        # Each statement should be an offset-shifted clone.
        if any(d != d_lhs for d in rhs_deltas):
            return None
        if rhs_norm_ref is None:
            rhs_norm_ref = rhs_norm
            rhs_ref = rhs
        elif rhs_norm != rhs_norm_ref:
            return None

    if seen_lhs_deltas != required_deltas:
        return None
    if lhs_name is None or rhs_ref is None:
        return None

    chunk_ub = f"{loop_var}+{k-1}"
    rhs_chunk = replace_affine_index_anydim(rhs_ref, loop_var, loop_var, chunk_ub, array_names=array_names)

    if aggressive:
        rhs_full = replace_affine_index_anydim(rhs_ref, loop_var, lb, ub, array_names=array_names)
        if has_loop_var(rhs_full, loop_var):
            return None
        return f"{lhs_name}({lb}:{ub}) = {rhs_full}"

    return (
        f"do {loop_var}={lb},{ub},{k}\n"
        f"   {lhs_name}({loop_var}:{chunk_ub}) = {rhs_chunk}\n"
        f"end do"
    )


def maybe_nested_multi_elementwise(
    outer_var: str,
    outer_rng: str,
    inner_do_stmt: str,
    inner_body_stmts: List[str],
    decl_bounds: Dict[str, str],
    rank2_bounds: Dict[str, Tuple[str, str]],
    alloc_map: Dict[str, str],
    array_names: Set[str],
) -> Optional[str]:
    """Detect nested loops with elementwise assignments, allowing independent remainder statements."""
    def range_to_do_spec(r: str) -> Optional[str]:
        parts = [p.strip() for p in r.strip().split(":")]
        if len(parts) == 2:
            lb = parts[0].strip()
            ub = parts[1].strip()
            if lb and ub:
                return f"{lb},{ub}"
            return None
        if len(parts) == 3:
            lb = parts[0].strip()
            ub = parts[1].strip()
            st = parts[2].strip()
            if lb and ub and st:
                return f"{lb},{ub},{st}"
            return None
        return None

    m_ido = DO_RE.match(inner_do_stmt.strip())
    if not m_ido:
        return None
    inner_var = m_ido.group(1)
    inner_rng = build_range(m_ido.group(2), m_ido.group(3), m_ido.group(4))
    elem_stmts: List[str] = []
    rem_stmts: List[str] = []
    lhs_written: Set[str] = set()
    rhs_array_refs: Set[str] = set()
    for st in inner_body_stmts:
        ss = st.strip()
        m_asn = ASSIGN_RE.match(ss)
        if m_asn is not None:
            sug = maybe_elementwise(inner_var, inner_rng, ss, decl_bounds, alloc_map, array_names)
            if sug is not None:
                elem_stmts.append(sug)
                lhs0 = m_asn.group(1).strip()
                rhs0 = m_asn.group(2).strip()
                m_lhs = parse_lhs_indexed_by_loop(lhs0, inner_var)
                if m_lhs is None:
                    return None
                lhs_written.add(m_lhs[0].lower())
                rhs_low = strip_quoted_text(rhs0.lower())
                for mm in CALL_LIKE_RE.finditer(rhs_low):
                    nm = mm.group(1).lower()
                    if nm in array_names:
                        rhs_array_refs.add(nm)
                continue
        rem_stmts.append(ss)
    if not elem_stmts:
        return None
    out_vec_lines: List[str] = []
    for ln in elem_stmts:
        s = maybe_elementwise(outer_var, outer_rng, ln.strip(), decl_bounds, alloc_map, array_names)
        if s is None:
            return None
        s = simplify_section_expr_rank2(s, rank2_bounds)
        out_vec_lines.append(s)

    if not rem_stmts:
        return "\n".join(out_vec_lines)

    # Conservative: remainder must not touch vectorized targets/sources.
    for rs in rem_stmts:
        low = strip_quoted_text(rs.lower())
        m_asn = ASSIGN_RE.match(rs)
        if m_asn:
            lhsr = m_asn.group(1).strip()
            b = fscan.base_identifier(lhsr)
            if b and (b in lhs_written or b in rhs_array_refs):
                return None
        if any(re.search(rf"\b{re.escape(nm)}\s*\(", low) for nm in lhs_written):
            return None

    m_odo = DO_RE.match(f"do {outer_var}={outer_rng}")
    outer_spec = range_to_do_spec(outer_rng)
    inner_spec = range_to_do_spec(inner_rng)
    if outer_spec is None or inner_spec is None:
        return None
    outer_header = f"do {outer_var}={outer_spec}"
    inner_header = f"   do {inner_var}={inner_spec}"
    kept = [outer_header, inner_header]
    for rs in rem_stmts:
        kept.append(f"      {rs}")
    kept.append("   end do")
    kept.append("end do")
    kept.extend(out_vec_lines)
    return "\n".join(kept)


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


def reduction_sum_acc_name(body_stmt: str) -> Optional[str]:
    """Extract accumulator name from `acc = acc + expr` body form."""
    ml = ASSIGN_RE.match(body_stmt.strip())
    if not ml:
        return None
    acc_lhs = SIMPLE_NAME_RE.match(ml.group(1).strip())
    if not acc_lhs:
        return None
    madd = ADD_ACC_RE.match(ml.group(2).strip())
    if not madd:
        return None
    if madd.group(1).lower() != acc_lhs.group(1).lower():
        return None
    return acc_lhs.group(1).lower()


def simplify_sum_section_with_size_alias(sugg: str, body: List[Tuple[int, str]], do_idx: int) -> str:
    """Simplify `sum((a(1:nx)-...))` -> `sum((a-...))` when `nx = size(a)` nearby."""
    m = re.match(r"^\s*([a-z][a-z0-9_]*)\s*=\s*sum\((.+)\)\s*$", sugg, re.IGNORECASE)
    if m is None:
        return sugg
    expr = m.group(2)
    aliases: Dict[str, str] = {}
    for back in range(1, min(12, do_idx + 1)):
        stmt = body[do_idx - back][1].strip()
        ma = re.match(
            r"^\s*([a-z][a-z0-9_]*)\s*=\s*size\s*\(\s*([a-z][a-z0-9_]*)\s*\)\s*$",
            stmt,
            re.IGNORECASE,
        )
        if ma is not None:
            aliases[ma.group(1).lower()] = ma.group(2)
    if not aliases:
        return sugg
    for nvar, arr in aliases.items():
        expr_new = re.sub(
            rf"\b{re.escape(arr)}\s*\(\s*1\s*:\s*{re.escape(nvar)}\s*\)",
            arr,
            expr,
            flags=re.IGNORECASE,
        )
        expr = expr_new
    return f"{m.group(1)} = sum({expr})"


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


def maybe_block_reduction_sum_dim1_scaled(
    body: List[Tuple[int, str]],
    i: int,
    decl_bounds: Dict[str, str],
    rank2_bounds: Dict[str, Tuple[str, str]],
    array_names: Set[str],
) -> Optional[Tuple[int, str, int]]:
    """Detect BLOCK with init+nested accumulate+scale and fold to sum(...,dim=1)/scale.

    Pattern (allowing one optional declaration line after BLOCK):
      block
        [integer :: ...]
        do j = 1, C
          v(j) = 0
        end do
        do i = 1, R
          do j = 1, C
            v(j) = v(j) + expr(i,j)
          end do
        end do
        do j = 1, C
          v(j) = v(j) / s
        end do
      end block
    """
    if i >= len(body) or body[i][1].strip().lower() != "block":
        return None

    def _canon_extent(e: str) -> str:
        s = normalize_expr(e)
        s = re.sub(
            r"ubound\(\s*([a-z][a-z0-9_]*)\s*,\s*([123])\s*\)",
            lambda m: f"size({m.group(1)},{m.group(2)})",
            s,
            flags=re.IGNORECASE,
        )
        return s

    j = i + 1
    if j < len(body):
        st = body[j][1].strip().lower()
        if TYPE_DECL_RE.match(st):
            j += 1
    if j + 11 >= len(body):
        return None

    m_do_init = DO_RE.match(body[j][1].strip())
    m_asn_init = ASSIGN_RE.match(body[j + 1][1].strip())
    m_end_init = END_DO_RE.match(body[j + 2][1].strip())
    m_do_row = DO_RE.match(body[j + 3][1].strip())
    m_do_col = DO_RE.match(body[j + 4][1].strip())
    m_asn_acc = ASSIGN_RE.match(body[j + 5][1].strip())
    m_end_col = END_DO_RE.match(body[j + 6][1].strip())
    m_end_row = END_DO_RE.match(body[j + 7][1].strip())
    m_do_scale = DO_RE.match(body[j + 8][1].strip())
    m_asn_scale = ASSIGN_RE.match(body[j + 9][1].strip())
    m_end_scale = END_DO_RE.match(body[j + 10][1].strip())
    end_block = body[j + 11][1].strip().lower() == "end block"
    if not (
        m_do_init
        and m_asn_init
        and m_end_init
        and m_do_row
        and m_do_col
        and m_asn_acc
        and m_end_col
        and m_end_row
        and m_do_scale
        and m_asn_scale
        and m_end_scale
        and end_block
    ):
        return None

    col_var_init = m_do_init.group(1)
    col_lb = m_do_init.group(2).strip()
    col_ub = m_do_init.group(3).strip()
    col_step = m_do_init.group(4)
    if normalize_expr(col_lb) != normalize_expr("1"):
        return None
    if col_step is not None and col_step.strip() != "1":
        return None

    lhs_init = parse_indexed_name(m_asn_init.group(1).strip())
    if lhs_init is None or len(lhs_init[1]) != 1:
        return None
    acc_name = lhs_init[0]
    acc_idx = lhs_init[1][0].strip()
    if acc_idx.lower() != col_var_init.lower():
        return None
    if not ZERO_LITERAL_RE.match(m_asn_init.group(2).strip()):
        return None

    row_var = m_do_row.group(1)
    row_lb = m_do_row.group(2).strip()
    row_ub = m_do_row.group(3).strip()
    row_step = m_do_row.group(4)
    if normalize_expr(row_lb) != normalize_expr("1"):
        return None
    if row_step is not None and row_step.strip() != "1":
        return None

    col_var_acc = m_do_col.group(1)
    col_lb_acc = m_do_col.group(2).strip()
    col_ub_acc = m_do_col.group(3).strip()
    col_step_acc = m_do_col.group(4)
    if col_var_acc.lower() != col_var_init.lower():
        return None
    if normalize_expr(col_lb_acc) != normalize_expr(col_lb) or normalize_expr(col_ub_acc) != normalize_expr(col_ub):
        return None
    if col_step_acc is not None and col_step_acc.strip() != "1":
        return None

    lhs_acc = parse_indexed_name(m_asn_acc.group(1).strip())
    if lhs_acc is None or len(lhs_acc[1]) != 1 or lhs_acc[0].lower() != acc_name.lower():
        return None
    if lhs_acc[1][0].strip().lower() != col_var_init.lower():
        return None
    rhs_acc = m_asn_acc.group(2).strip()
    m_add = re.match(r"^\s*(.+?)\s*\+\s*(.+)\s*$", rhs_acc)
    if not m_add:
        return None
    if normalize_expr(m_add.group(1).strip()) != normalize_expr(m_asn_acc.group(1).strip()):
        return None
    expr = m_add.group(2).strip()

    col_var_scale = m_do_scale.group(1)
    col_lb_scale = m_do_scale.group(2).strip()
    col_ub_scale = m_do_scale.group(3).strip()
    col_step_scale = m_do_scale.group(4)
    if col_var_scale.lower() != col_var_init.lower():
        return None
    if normalize_expr(col_lb_scale) != normalize_expr(col_lb) or normalize_expr(col_ub_scale) != normalize_expr(col_ub):
        return None
    if col_step_scale is not None and col_step_scale.strip() != "1":
        return None

    lhs_scale = parse_indexed_name(m_asn_scale.group(1).strip())
    if lhs_scale is None or len(lhs_scale[1]) != 1 or lhs_scale[0].lower() != acc_name.lower():
        return None
    if lhs_scale[1][0].strip().lower() != col_var_init.lower():
        return None
    rhs_scale = m_asn_scale.group(2).strip()
    m_div = re.match(r"^\s*(.+?)\s*/\s*(.+)\s*$", rhs_scale)
    if not m_div:
        return None
    if normalize_expr(m_div.group(1).strip()) != normalize_expr(m_asn_scale.group(1).strip()):
        return None
    scale = m_div.group(2).strip()

    # Build matrix expression for sum(..., dim=1).
    local_array_names = set(array_names)
    for mm in CALL_LIKE_RE.finditer(strip_quoted_text(expr.lower())):
        nm = mm.group(1).lower()
        if nm in ELEMENTAL_INTRINSICS or nm in USER_ELEMENTAL_CALLS:
            continue
        local_array_names.add(nm)
    expr_m = replace_affine_index_anydim(expr, row_var, "1", row_ub, array_names=local_array_names)
    expr_m = replace_affine_index_anydim(expr_m, col_var_init, "1", col_ub, array_names=local_array_names)
    if has_loop_var(expr_m, row_var) or has_loop_var(expr_m, col_var_init):
        return None
    expr_m = simplify_section_expr(expr_m, decl_bounds)
    expr_m = simplify_section_expr_rank2(expr_m, rank2_bounds)
    # Collapse obvious whole-array sections: a(1:ubound(a,1),1:ubound(a,2)) -> a
    expr_m = re.sub(
        r"\b([a-z][a-z0-9_]*)\s*\(\s*1\s*:\s*ubound\(\s*\1\s*,\s*1\s*\)\s*,\s*1\s*:\s*ubound\(\s*\1\s*,\s*2\s*\)\s*\)",
        lambda m: m.group(1),
        expr_m,
        flags=re.IGNORECASE,
    )
    if has_disallowed_function_calls(expr_m, local_array_names):
        return None

    lhs_expr = f"{acc_name}(1:{col_ub})"
    bnd = decl_bounds.get(acc_name.lower())
    if bnd is not None and _canon_extent(f"1:{col_ub}") == _canon_extent(bnd):
        lhs_expr = acc_name
    else:
        m_src = re.match(r"^\s*([a-z][a-z0-9_]*)\s*$", expr_m, re.IGNORECASE)
        m_ub2 = re.match(r"^\s*ubound\(\s*([a-z][a-z0-9_]*)\s*,\s*2\s*\)\s*$", col_ub, re.IGNORECASE)
        if m_src and m_ub2 and m_src.group(1).lower() == m_ub2.group(1).lower():
            lhs_expr = acc_name
    suggestion = f"{lhs_expr} = sum({expr_m}, dim=1) / {scale}"
    return (j + 11, suggestion, body[j + 11][0])


def maybe_block_reduction_sum_dim2_scaled(
    body: List[Tuple[int, str]],
    i: int,
    decl_bounds: Dict[str, str],
    rank2_bounds: Dict[str, Tuple[str, str]],
    array_names: Set[str],
) -> Optional[Tuple[int, str, int]]:
    """Detect BLOCK with init+nested accumulate+scale and fold to sum(...,dim=2)/scale.

    Pattern (allowing one optional declaration line after BLOCK):
      block
        [integer :: ...]
        do i = 1, R
          v(i) = 0
        end do
        do i = 1, R
          do j = 1, C
            v(i) = v(i) + expr(i,j)
          end do
        end do
        do i = 1, R
          v(i) = v(i) / s
        end do
      end block
    """
    if i >= len(body) or body[i][1].strip().lower() != "block":
        return None

    def _canon_extent(e: str) -> str:
        s = normalize_expr(e)
        s = re.sub(
            r"ubound\(\s*([a-z][a-z0-9_]*)\s*,\s*([123])\s*\)",
            lambda m: f"size({m.group(1)},{m.group(2)})",
            s,
            flags=re.IGNORECASE,
        )
        return s

    j = i + 1
    if j < len(body):
        st = body[j][1].strip().lower()
        if TYPE_DECL_RE.match(st):
            j += 1
    if j + 11 >= len(body):
        return None

    m_do_init = DO_RE.match(body[j][1].strip())
    m_asn_init = ASSIGN_RE.match(body[j + 1][1].strip())
    m_end_init = END_DO_RE.match(body[j + 2][1].strip())
    m_do_row = DO_RE.match(body[j + 3][1].strip())
    m_do_col = DO_RE.match(body[j + 4][1].strip())
    m_asn_acc = ASSIGN_RE.match(body[j + 5][1].strip())
    m_end_col = END_DO_RE.match(body[j + 6][1].strip())
    m_end_row = END_DO_RE.match(body[j + 7][1].strip())
    m_do_scale = DO_RE.match(body[j + 8][1].strip())
    m_asn_scale = ASSIGN_RE.match(body[j + 9][1].strip())
    m_end_scale = END_DO_RE.match(body[j + 10][1].strip())
    end_block = body[j + 11][1].strip().lower() == "end block"
    if not (
        m_do_init
        and m_asn_init
        and m_end_init
        and m_do_row
        and m_do_col
        and m_asn_acc
        and m_end_col
        and m_end_row
        and m_do_scale
        and m_asn_scale
        and m_end_scale
        and end_block
    ):
        return None

    row_var_init = m_do_init.group(1)
    row_lb = m_do_init.group(2).strip()
    row_ub = m_do_init.group(3).strip()
    row_step = m_do_init.group(4)
    if normalize_expr(row_lb) != normalize_expr("1"):
        return None
    if row_step is not None and row_step.strip() != "1":
        return None

    lhs_init = parse_indexed_name(m_asn_init.group(1).strip())
    if lhs_init is None or len(lhs_init[1]) != 1:
        return None
    acc_name = lhs_init[0]
    acc_idx = lhs_init[1][0].strip()
    if acc_idx.lower() != row_var_init.lower():
        return None
    if not ZERO_LITERAL_RE.match(m_asn_init.group(2).strip()):
        return None

    row_var = m_do_row.group(1)
    row_lb2 = m_do_row.group(2).strip()
    row_ub2 = m_do_row.group(3).strip()
    row_step2 = m_do_row.group(4)
    if row_var.lower() != row_var_init.lower():
        return None
    if normalize_expr(row_lb2) != normalize_expr(row_lb) or normalize_expr(row_ub2) != normalize_expr(row_ub):
        return None
    if row_step2 is not None and row_step2.strip() != "1":
        return None

    col_var = m_do_col.group(1)
    col_lb = m_do_col.group(2).strip()
    col_ub = m_do_col.group(3).strip()
    col_step = m_do_col.group(4)
    if normalize_expr(col_lb) != normalize_expr("1"):
        return None
    if col_step is not None and col_step.strip() != "1":
        return None

    lhs_acc = parse_indexed_name(m_asn_acc.group(1).strip())
    if lhs_acc is None or len(lhs_acc[1]) != 1 or lhs_acc[0].lower() != acc_name.lower():
        return None
    if lhs_acc[1][0].strip().lower() != row_var_init.lower():
        return None
    rhs_acc = m_asn_acc.group(2).strip()
    m_add = re.match(r"^\s*(.+?)\s*\+\s*(.+)\s*$", rhs_acc)
    if not m_add:
        return None
    if normalize_expr(m_add.group(1).strip()) != normalize_expr(m_asn_acc.group(1).strip()):
        return None
    expr = m_add.group(2).strip()

    row_var_scale = m_do_scale.group(1)
    row_lb_scale = m_do_scale.group(2).strip()
    row_ub_scale = m_do_scale.group(3).strip()
    row_step_scale = m_do_scale.group(4)
    if row_var_scale.lower() != row_var_init.lower():
        return None
    if normalize_expr(row_lb_scale) != normalize_expr(row_lb) or normalize_expr(row_ub_scale) != normalize_expr(row_ub):
        return None
    if row_step_scale is not None and row_step_scale.strip() != "1":
        return None

    lhs_scale = parse_indexed_name(m_asn_scale.group(1).strip())
    if lhs_scale is None or len(lhs_scale[1]) != 1 or lhs_scale[0].lower() != acc_name.lower():
        return None
    if lhs_scale[1][0].strip().lower() != row_var_init.lower():
        return None
    rhs_scale = m_asn_scale.group(2).strip()
    m_div = re.match(r"^\s*(.+?)\s*/\s*(.+)\s*$", rhs_scale)
    if not m_div:
        return None
    if normalize_expr(m_div.group(1).strip()) != normalize_expr(m_asn_scale.group(1).strip()):
        return None
    scale = m_div.group(2).strip()

    local_array_names = set(array_names)
    for mm in CALL_LIKE_RE.finditer(strip_quoted_text(expr.lower())):
        nm = mm.group(1).lower()
        if nm in ELEMENTAL_INTRINSICS or nm in USER_ELEMENTAL_CALLS:
            continue
        local_array_names.add(nm)
    expr_m = replace_affine_index_anydim(expr, row_var, "1", row_ub, array_names=local_array_names)
    expr_m = replace_affine_index_anydim(expr_m, col_var, "1", col_ub, array_names=local_array_names)
    if has_loop_var(expr_m, row_var) or has_loop_var(expr_m, col_var):
        return None
    expr_m = simplify_section_expr(expr_m, decl_bounds)
    expr_m = simplify_section_expr_rank2(expr_m, rank2_bounds)
    expr_m = re.sub(
        r"\b([a-z][a-z0-9_]*)\s*\(\s*1\s*:\s*ubound\(\s*\1\s*,\s*1\s*\)\s*,\s*1\s*:\s*ubound\(\s*\1\s*,\s*2\s*\)\s*\)",
        lambda m: m.group(1),
        expr_m,
        flags=re.IGNORECASE,
    )
    expr_m = re.sub(
        r"\b([a-z][a-z0-9_]*)\s*\(\s*1\s*:\s*size\(\s*\1\s*,\s*1\s*\)\s*,\s*1\s*:\s*size\(\s*\1\s*,\s*2\s*\)\s*\)",
        lambda m: m.group(1),
        expr_m,
        flags=re.IGNORECASE,
    )
    for r2name, (b1, b2) in rank2_bounds.items():
        b1p = re.escape(re.sub(r"\s+", "", b1))
        b2p = re.escape(re.sub(r"\s+", "", b2))
        pat = re.compile(
            rf"\b{re.escape(r2name)}\s*\(\s*1\s*:\s*({b1p}|{re.escape(b1)})\s*,\s*1\s*:\s*({b2p}|{re.escape(b2)})\s*\)",
            re.IGNORECASE,
        )
        expr_m = pat.sub(r2name, expr_m)
    if has_disallowed_function_calls(expr_m, local_array_names):
        return None

    lhs_expr = f"{acc_name}(1:{row_ub})"
    bnd = decl_bounds.get(acc_name.lower())
    if bnd is not None and _canon_extent(f"1:{row_ub}") == _canon_extent(bnd):
        lhs_expr = acc_name
    else:
        m_src = re.match(r"^\s*([a-z][a-z0-9_]*)\s*$", expr_m, re.IGNORECASE)
        m_ub1 = re.match(r"^\s*ubound\(\s*([a-z][a-z0-9_]*)\s*,\s*1\s*\)\s*$", row_ub, re.IGNORECASE)
        if m_src and m_ub1 and m_src.group(1).lower() == m_ub1.group(1).lower():
            lhs_expr = acc_name
    suggestion = f"{lhs_expr} = sum({expr_m}, dim=2) / {scale}"
    return (j + 11, suggestion, body[j + 11][0])


def maybe_block_reduction_sqrt_sum_dim1_scaled(
    body: List[Tuple[int, str]],
    i: int,
    decl_bounds: Dict[str, str],
    rank2_bounds: Dict[str, Tuple[str, str]],
    array_names: Set[str],
) -> Optional[Tuple[int, str, int]]:
    """Detect BLOCK reduction pattern and fold to sqrt(sum(..., dim=1)/scale)."""
    base = maybe_block_reduction_sum_dim1_scaled(body, i, decl_bounds, rank2_bounds, array_names)
    if base is None:
        return None
    end_idx, sugg, ln_end = base
    m = ASSIGN_RE.match(sugg.strip())
    if not m:
        return None
    lhs = m.group(1).strip()
    rhs = m.group(2).strip()
    # Expect: sum(expr, dim=1) / scale
    md = re.match(r"^\s*sum\s*\(\s*(.+)\s*,\s*dim\s*=\s*1\s*\)\s*/\s*(.+)\s*$", rhs, re.IGNORECASE)
    if not md:
        return None
    expr_m = md.group(1).strip()
    scale = md.group(2).strip()

    # Verify original block's final update is sqrt(acc / scale).
    # end_idx points at END BLOCK; assignment is at end_idx-2.
    if end_idx - 2 < i:
        return None
    asn_scale = body[end_idx - 2][1]
    m_asn = ASSIGN_RE.match(asn_scale.strip())
    if not m_asn:
        return None
    if normalize_expr(m_asn.group(1).strip()) != normalize_expr(lhs):
        return None
    rhs_scale = m_asn.group(2).strip()
    m_sqrt = re.match(r"^\s*sqrt\s*\(\s*(.+)\s*\)\s*$", rhs_scale, re.IGNORECASE)
    if not m_sqrt:
        return None
    inner = m_sqrt.group(1).strip()
    m_div = re.match(r"^\s*(.+?)\s*/\s*(.+)\s*$", inner)
    if not m_div:
        return None
    if normalize_expr(m_div.group(1).strip()) != normalize_expr(lhs):
        return None
    if normalize_expr(m_div.group(2).strip()) != normalize_expr(scale):
        return None

    return (end_idx, f"{lhs} = sqrt(sum({expr_m}, dim=1) / {scale})", ln_end)


def maybe_block_reduction_sqrt_inside_outer_dim1(
    body: List[Tuple[int, str]],
    i: int,
    decl_bounds: Dict[str, str],
    rank2_bounds: Dict[str, Tuple[str, str]],
    array_names: Set[str],
) -> Optional[Tuple[int, str, int]]:
    """Detect BLOCK with outer-loop init+inner-sum+sqrt-scale and fold to sqrt(sum(...,dim=1)/scale)."""
    if i >= len(body) or body[i][1].strip().lower() != "block":
        return None

    j = i + 1
    if j < len(body):
        st = body[j][1].strip().lower()
        if TYPE_DECL_RE.match(st):
            j += 1
    if j + 8 >= len(body):
        return None

    m_do_o = DO_RE.match(body[j][1].strip())
    m_init = ASSIGN_RE.match(body[j + 1][1].strip())
    m_do_i = DO_RE.match(body[j + 2][1].strip())
    m_acc = ASSIGN_RE.match(body[j + 3][1].strip())
    m_end_i = END_DO_RE.match(body[j + 4][1].strip())
    m_sqrt = ASSIGN_RE.match(body[j + 5][1].strip())
    m_end_o = END_DO_RE.match(body[j + 6][1].strip())
    end_block = body[j + 7][1].strip().lower() == "end block"
    if not (m_do_o and m_init and m_do_i and m_acc and m_end_i and m_sqrt and m_end_o and end_block):
        return None

    o_var = m_do_o.group(1)
    o_lb = m_do_o.group(2).strip()
    o_ub = m_do_o.group(3).strip()
    o_step = m_do_o.group(4)
    if normalize_expr(o_lb) != normalize_expr("1"):
        return None
    if o_step is not None and o_step.strip() != "1":
        return None
    i_var = m_do_i.group(1)
    i_lb = m_do_i.group(2).strip()
    i_ub = m_do_i.group(3).strip()
    i_step = m_do_i.group(4)
    if normalize_expr(i_lb) != normalize_expr("1"):
        return None
    if i_step is not None and i_step.strip() != "1":
        return None

    lhs_init = parse_indexed_name(m_init.group(1).strip())
    if lhs_init is None or len(lhs_init[1]) != 1:
        return None
    acc_name = lhs_init[0]
    if lhs_init[1][0].strip().lower() != o_var.lower():
        return None
    if not ZERO_LITERAL_RE.match(m_init.group(2).strip()):
        return None

    lhs_acc = parse_indexed_name(m_acc.group(1).strip())
    if lhs_acc is None or len(lhs_acc[1]) != 1:
        return None
    if lhs_acc[0].lower() != acc_name.lower() or lhs_acc[1][0].strip().lower() != o_var.lower():
        return None
    rhs_acc = m_acc.group(2).strip()
    m_add = re.match(r"^\s*(.+?)\s*\+\s*(.+)\s*$", rhs_acc)
    if not m_add:
        return None
    if normalize_expr(m_add.group(1).strip()) != normalize_expr(m_acc.group(1).strip()):
        return None
    expr = m_add.group(2).strip()

    lhs_sqrt = parse_indexed_name(m_sqrt.group(1).strip())
    if lhs_sqrt is None or len(lhs_sqrt[1]) != 1:
        return None
    if lhs_sqrt[0].lower() != acc_name.lower() or lhs_sqrt[1][0].strip().lower() != o_var.lower():
        return None
    rhs_sqrt = m_sqrt.group(2).strip()
    m_sq = re.match(r"^\s*sqrt\s*\(\s*(.+)\s*\)\s*$", rhs_sqrt, re.IGNORECASE)
    if not m_sq:
        return None
    inner = m_sq.group(1).strip()
    m_div = re.match(r"^\s*(.+?)\s*/\s*(.+)\s*$", inner)
    if not m_div:
        return None
    if normalize_expr(m_div.group(1).strip()) != normalize_expr(m_sqrt.group(1).strip()):
        return None
    scale = m_div.group(2).strip()

    local_array_names = set(array_names)
    for mm in CALL_LIKE_RE.finditer(strip_quoted_text(expr.lower())):
        nm = mm.group(1).lower()
        if nm in ELEMENTAL_INTRINSICS or nm in USER_ELEMENTAL_CALLS:
            continue
        local_array_names.add(nm)
    expr_m = replace_affine_index_anydim(expr, i_var, "1", i_ub, array_names=local_array_names)
    expr_m = replace_affine_index_anydim(expr_m, o_var, "1", o_ub, array_names=local_array_names)
    if has_loop_var(expr_m, i_var) or has_loop_var(expr_m, o_var):
        return None
    expr_m = simplify_section_expr(expr_m, decl_bounds)
    expr_m = simplify_section_expr_rank2(expr_m, rank2_bounds)
    expr_m = re.sub(
        r"\b([a-z][a-z0-9_]*)\s*\(\s*1\s*:\s*ubound\(\s*\1\s*,\s*1\s*\)\s*,\s*1\s*:\s*ubound\(\s*\1\s*,\s*2\s*\)\s*\)",
        lambda m: m.group(1),
        expr_m,
        flags=re.IGNORECASE,
    )
    expr_m = re.sub(
        r"\b([a-z][a-z0-9_]*)\s*\(\s*1\s*:\s*size\(\s*\1\s*,\s*1\s*\)\s*,\s*1\s*:\s*size\(\s*\1\s*,\s*2\s*\)\s*\)",
        lambda m: m.group(1),
        expr_m,
        flags=re.IGNORECASE,
    )
    for r2name, (b1, b2) in rank2_bounds.items():
        pat = re.compile(
            rf"\b{re.escape(r2name)}\s*\(\s*1\s*:\s*{re.escape(b1)}\s*,\s*1\s*:\s*{re.escape(b2)}\s*\)",
            re.IGNORECASE,
        )
        expr_m = pat.sub(r2name, expr_m)
    if has_disallowed_function_calls(expr_m, local_array_names):
        return None

    scale_clean = scale.strip()
    while True:
        m_scale_par = re.match(r"^\(\s*(.+?)\s*\)$", scale_clean)
        if not m_scale_par:
            break
        scale_clean = m_scale_par.group(1).strip()

    lhs_expr = acc_name
    return (j + 7, f"{lhs_expr} = sqrt(sum({expr_m}, dim=1) / ({scale_clean}))", body[j + 7][0])


def maybe_block_matmul_transpose_scaled(
    body: List[Tuple[int, str]],
    i: int,
    rank2_bounds: Dict[str, Tuple[str, str]],
) -> Optional[Tuple[int, str, int]]:
    """Detect BLOCK triple-loop accumulation and fold to matmul(transpose(A),A)/scale.

    Pattern:
      block
        [integer :: ...]
        do i = 1, p
          do k = 1, p
            c(i,k) = 0
            do j = 1, n
              c(i,k) = c(i,k) + a(j,i) * a(j,k)
            end do
            c(i,k) = c(i,k) / s
          end do
        end do
      end block
    """
    if i >= len(body) or body[i][1].strip().lower() != "block":
        return None
    j = i + 1
    if j < len(body):
        st = body[j][1].strip().lower()
        if TYPE_DECL_RE.match(st):
            j += 1
    if j + 9 >= len(body):
        return None

    m_do_i = DO_RE.match(body[j][1].strip())
    m_do_k = DO_RE.match(body[j + 1][1].strip())
    m_init = ASSIGN_RE.match(body[j + 2][1].strip())
    m_do_j = DO_RE.match(body[j + 3][1].strip())
    m_acc = ASSIGN_RE.match(body[j + 4][1].strip())
    m_end_j = END_DO_RE.match(body[j + 5][1].strip())
    m_scale = ASSIGN_RE.match(body[j + 6][1].strip())
    m_end_k = END_DO_RE.match(body[j + 7][1].strip())
    m_end_i = END_DO_RE.match(body[j + 8][1].strip())
    end_block = body[j + 9][1].strip().lower() == "end block"
    if not (m_do_i and m_do_k and m_init and m_do_j and m_acc and m_end_j and m_scale and m_end_k and m_end_i and end_block):
        return None

    iv = m_do_i.group(1).strip()
    kv = m_do_k.group(1).strip()
    jv = m_do_j.group(1).strip()
    ilb, iub, istep = m_do_i.group(2).strip(), m_do_i.group(3).strip(), m_do_i.group(4)
    klb, kub, kstep = m_do_k.group(2).strip(), m_do_k.group(3).strip(), m_do_k.group(4)
    jlb, jub, jstep = m_do_j.group(2).strip(), m_do_j.group(3).strip(), m_do_j.group(4)
    if normalize_expr(ilb) != normalize_expr("1") or normalize_expr(klb) != normalize_expr("1") or normalize_expr(jlb) != normalize_expr("1"):
        return None
    if (istep is not None and istep.strip() != "1") or (kstep is not None and kstep.strip() != "1") or (jstep is not None and jstep.strip() != "1"):
        return None
    if normalize_expr(iub) != normalize_expr(kub):
        return None

    lhs_init = parse_indexed_name(m_init.group(1).strip())
    if lhs_init is None or len(lhs_init[1]) != 2:
        return None
    c_name, c_args = lhs_init
    if c_args[0].strip().lower() != iv.lower() or c_args[1].strip().lower() != kv.lower():
        return None
    if not ZERO_LITERAL_RE.match(m_init.group(2).strip()):
        return None

    lhs_acc = m_acc.group(1).strip()
    rhs_acc = m_acc.group(2).strip()
    if normalize_expr(lhs_acc) != normalize_expr(f"{c_name}({iv},{kv})"):
        return None
    m_add = re.match(r"^\s*(.+?)\s*\+\s*(.+)\s*$", rhs_acc)
    if not m_add:
        return None
    if normalize_expr(m_add.group(1).strip()) != normalize_expr(lhs_acc):
        return None
    expr = m_add.group(2).strip().replace(" ", "")
    m_prod = re.match(
        rf"^([a-z][a-z0-9_]*)\({re.escape(jv)},{re.escape(iv)}\)\*([a-z][a-z0-9_]*)\({re.escape(jv)},{re.escape(kv)}\)$",
        expr,
        re.IGNORECASE,
    )
    if not m_prod:
        # also allow commuted factors
        m_prod = re.match(
            rf"^([a-z][a-z0-9_]*)\({re.escape(jv)},{re.escape(kv)}\)\*([a-z][a-z0-9_]*)\({re.escape(jv)},{re.escape(iv)}\)$",
            expr,
            re.IGNORECASE,
        )
    if not m_prod:
        return None
    a1 = m_prod.group(1).lower()
    a2 = m_prod.group(2).lower()
    if a1 != a2:
        return None
    a_name = m_prod.group(1)

    lhs_scale = m_scale.group(1).strip()
    rhs_scale = m_scale.group(2).strip()
    if normalize_expr(lhs_scale) != normalize_expr(lhs_init[0] + f"({iv},{kv})"):
        return None
    m_div = re.match(r"^\s*(.+?)\s*/\s*(.+)\s*$", rhs_scale)
    if not m_div:
        return None
    if normalize_expr(m_div.group(1).strip()) != normalize_expr(lhs_scale):
        return None
    scale = m_div.group(2).strip()

    # Build compact lhs/rhs if bounds match declarations.
    lhs_expr = f"{c_name}(1:{iub}, 1:{kub})"
    cb = rank2_bounds.get(c_name.lower())
    if cb is not None and normalize_expr(cb[0]) == normalize_expr(iub) and normalize_expr(cb[1]) == normalize_expr(kub):
        lhs_expr = c_name

    rhs_a = f"{a_name}(1:{jub}, 1:{iub})"
    ab = rank2_bounds.get(a_name.lower())
    if ab is not None and normalize_expr(ab[0]) == normalize_expr(jub) and normalize_expr(ab[1]) == normalize_expr(iub):
        rhs_a = a_name

    scale_clean = scale
    while True:
        m_par = re.match(r"^\(\s*(.+?)\s*\)$", scale_clean)
        if not m_par:
            break
        scale_clean = m_par.group(1).strip()
    suggestion = f"{lhs_expr} = matmul(transpose({rhs_a}), {rhs_a}) / ({scale_clean})"
    return (j + 9, suggestion, body[j + 9][0])


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


def maybe_nested_inner_product(
    outer_loop_var: str,
    init_stmt: str,
    inner_do_stmt: str,
    inner_body_stmt: str,
    decl_bounds: Dict[str, str],
) -> Optional[str]:
    """Detect y(i)=1; do j...; y(i)=y(i)*expr(j,...); end do -> y(i)=product(expr(...))."""
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
    if not ONE_LITERAL_RE.match(rhs0):
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
    m_mul = re.match(r"^\s*(.+?)\s*\*\s*(.+)$", m_acc.group(2).strip(), re.IGNORECASE)
    if not m_mul:
        return None
    if normalize_expr(m_mul.group(1)) != normalize_expr(lhs):
        return None
    expr = m_mul.group(2).strip()

    expr_s = replace_index_with_slice(expr, inner_var, lb, ubtxt)
    if has_loop_var(expr_s, inner_var):
        return None
    if expr_s == expr:
        return None
    expr_s = simplify_section_expr(expr_s, decl_bounds)
    return f"{lhs} = product({expr_s})"


def maybe_nested_scalar_reduction_sum(
    outer_var: str,
    outer_rng: str,
    inner_do_stmt: str,
    accum_stmt: str,
    prev_stmt: Optional[str],
    decl_bounds: Dict[str, str],
    array_names: Set[str],
    rank2_bounds: Optional[Dict[str, Tuple[str, str]]] = None,
) -> Optional[str]:
    """Detect:
      acc = 0
      do outer=...
         do inner=...
            acc = acc + expr(outer,inner,...)
         end do
      end do
    and fold to acc = sum(expr(...)).
    """
    if prev_stmt is None:
        return None
    mp = ASSIGN_RE.match(prev_stmt.strip())
    mi = DO_RE.match(inner_do_stmt.strip())
    ma = ASSIGN_RE.match(accum_stmt.strip())
    if not mp or not mi or not ma:
        return None
    acc_prev = SIMPLE_NAME_RE.match(mp.group(1).strip())
    acc_lhs = SIMPLE_NAME_RE.match(ma.group(1).strip())
    if not acc_prev or not acc_lhs:
        return None
    acc = acc_lhs.group(1).lower()
    if acc_prev.group(1).lower() != acc:
        return None
    madd = ADD_ACC_RE.match(ma.group(2).strip())
    if not madd or madd.group(1).lower() != acc:
        return None
    inner_var = mi.group(1).strip()
    inner_lb = mi.group(2).strip()
    inner_ub = mi.group(3).strip()
    expr = madd.group(2).strip()

    lb_o, ub_o = split_range(outer_rng)
    expr_s = replace_index_with_slice(expr, inner_var, inner_lb, inner_ub)
    expr_s = replace_index_var_in_multidim_refs(expr_s, inner_var, inner_lb, inner_ub)
    expr_s = replace_index_with_slice(expr_s, outer_var, lb_o, ub_o)
    expr_s = replace_index_var_in_multidim_refs(expr_s, outer_var, lb_o, ub_o)
    if has_loop_var(expr_s, inner_var) or has_loop_var(expr_s, outer_var):
        return None
    if expr_s == expr:
        return None
    expr_s = simplify_section_expr(expr_s, decl_bounds)
    if rank2_bounds:
        expr_s = simplify_section_expr_rank2(expr_s, rank2_bounds)
    if has_disallowed_function_calls(expr_s, array_names):
        return None
    return f"{acc} = sum({expr_s})"


def maybe_nested_scalar_reduction_product(
    outer_var: str,
    outer_rng: str,
    inner_do_stmt: str,
    accum_stmt: str,
    prev_stmt: Optional[str],
    decl_bounds: Dict[str, str],
    array_names: Set[str],
    rank2_bounds: Optional[Dict[str, Tuple[str, str]]] = None,
) -> Optional[str]:
    """Product variant of maybe_nested_scalar_reduction_sum."""
    if prev_stmt is None:
        return None
    mp = ASSIGN_RE.match(prev_stmt.strip())
    mi = DO_RE.match(inner_do_stmt.strip())
    ma = ASSIGN_RE.match(accum_stmt.strip())
    if not mp or not mi or not ma:
        return None
    acc_prev = SIMPLE_NAME_RE.match(mp.group(1).strip())
    acc_lhs = SIMPLE_NAME_RE.match(ma.group(1).strip())
    if not acc_prev or not acc_lhs:
        return None
    acc = acc_lhs.group(1).lower()
    if acc_prev.group(1).lower() != acc:
        return None
    if not ONE_LITERAL_RE.match(mp.group(2).strip()):
        return None
    mmul = MUL_ACC_RE.match(ma.group(2).strip())
    if not mmul or mmul.group(1).lower() != acc:
        return None
    inner_var = mi.group(1).strip()
    inner_lb = mi.group(2).strip()
    inner_ub = mi.group(3).strip()
    expr = mmul.group(2).strip()

    lb_o, ub_o = split_range(outer_rng)
    expr_s = replace_index_with_slice(expr, inner_var, inner_lb, inner_ub)
    expr_s = replace_index_var_in_multidim_refs(expr_s, inner_var, inner_lb, inner_ub)
    expr_s = replace_index_with_slice(expr_s, outer_var, lb_o, ub_o)
    expr_s = replace_index_var_in_multidim_refs(expr_s, outer_var, lb_o, ub_o)
    if has_loop_var(expr_s, inner_var) or has_loop_var(expr_s, outer_var):
        return None
    if expr_s == expr:
        return None
    expr_s = simplify_section_expr(expr_s, decl_bounds)
    if rank2_bounds:
        expr_s = simplify_section_expr_rank2(expr_s, rank2_bounds)
    if has_disallowed_function_calls(expr_s, array_names):
        return None
    return f"{acc} = product({expr_s})"


def maybe_block_nested_scalar_reduction_sum(
    body: List[Tuple[int, str]],
    i: int,
    decl_bounds: Dict[str, str],
    array_names: Set[str],
    rank2_bounds: Dict[str, Tuple[str, str]],
) -> Optional[Tuple[int, int, str, int]]:
    """Detect inside BLOCK:
      acc = 0
      do outer=...
        do inner=...
          acc = acc + expr
        end do
      end do
    and fold to acc = sum(expr(...)).
    Returns (start_idx, end_idx, suggestion, end_line).
    """
    if i >= len(body) or body[i][1].strip().lower() != "block":
        return None
    j = i + 1
    while j < len(body) and TYPE_DECL_RE.match(body[j][1].strip().lower()):
        j += 1
    if j + 5 >= len(body):
        return None
    stmt_init = body[j][1]
    stmt_odo = body[j + 1][1]
    stmt_ido = body[j + 2][1]
    stmt_acc = body[j + 3][1]
    stmt_iend = body[j + 4][1]
    stmt_oend = body[j + 5][1]
    mdo = DO_RE.match(stmt_odo.strip())
    if mdo is None:
        return None
    if not END_DO_RE.match(stmt_iend.strip()) or not END_DO_RE.match(stmt_oend.strip()):
        return None
    outer_var = mdo.group(1).strip()
    outer_rng = build_range(mdo.group(2).strip(), mdo.group(3).strip(), mdo.group(4).strip() if mdo.group(4) else None)
    sugg = maybe_nested_scalar_reduction_sum(
        outer_var, outer_rng, stmt_ido, stmt_acc, stmt_init, decl_bounds, array_names, rank2_bounds
    )
    if sugg is None:
        return None
    return (j, j + 5, sugg, body[j + 5][0])


def maybe_block_nested_scalar_reduction_product(
    body: List[Tuple[int, str]],
    i: int,
    decl_bounds: Dict[str, str],
    array_names: Set[str],
    rank2_bounds: Dict[str, Tuple[str, str]],
) -> Optional[Tuple[int, int, str, int]]:
    """Product variant of maybe_block_nested_scalar_reduction_sum."""
    if i >= len(body) or body[i][1].strip().lower() != "block":
        return None
    j = i + 1
    while j < len(body) and TYPE_DECL_RE.match(body[j][1].strip().lower()):
        j += 1
    if j + 5 >= len(body):
        return None
    stmt_init = body[j][1]
    stmt_odo = body[j + 1][1]
    stmt_ido = body[j + 2][1]
    stmt_acc = body[j + 3][1]
    stmt_iend = body[j + 4][1]
    stmt_oend = body[j + 5][1]
    mdo = DO_RE.match(stmt_odo.strip())
    if mdo is None:
        return None
    if not END_DO_RE.match(stmt_iend.strip()) or not END_DO_RE.match(stmt_oend.strip()):
        return None
    outer_var = mdo.group(1).strip()
    outer_rng = build_range(mdo.group(2).strip(), mdo.group(3).strip(), mdo.group(4).strip() if mdo.group(4) else None)
    sugg = maybe_nested_scalar_reduction_product(
        outer_var, outer_rng, stmt_ido, stmt_acc, stmt_init, decl_bounds, array_names, rank2_bounds
    )
    if sugg is None:
        return None
    return (j, j + 5, sugg, body[j + 5][0])


def maybe_block_norm2_from_loops(
    body: List[Tuple[int, str]],
    i: int,
    rank2_bounds: Dict[str, Tuple[str, str]],
) -> Optional[Tuple[int, str, int]]:
    """Detect block loop patterns equivalent to norm2(x[, dim=...])."""
    if i >= len(body) or body[i][1].strip().lower() != "block":
        return None

    j = i + 1
    while j < len(body) and TYPE_DECL_RE.match(body[j][1].strip().lower()):
        j += 1
    if j + 6 >= len(body):
        return None

    m_do_o = DO_RE.match(body[j][1].strip())
    if m_do_o is None:
        return None
    outer = m_do_o.group(1).strip()
    lb_o = m_do_o.group(2).strip()
    ub_o = m_do_o.group(3).strip()

    m_init = ASSIGN_RE.match(body[j + 1][1].strip())
    m_do_i = DO_RE.match(body[j + 2][1].strip())
    m_acc = ASSIGN_RE.match(body[j + 3][1].strip())
    if m_init is None or m_do_i is None or m_acc is None:
        return None
    inner = m_do_i.group(1).strip()
    lb_i = m_do_i.group(2).strip()
    ub_i = m_do_i.group(3).strip()

    lhs_init = m_init.group(1).strip()
    lhs_acc = m_acc.group(1).strip()
    rhs_acc = m_acc.group(2).strip()
    if normalize_expr(lhs_init) != normalize_expr(lhs_acc):
        return None

    madd = re.match(r"^\s*(.+?)\s*\+\s*(.+)\s*$", rhs_acc)
    if not madd:
        return None
    if normalize_expr(madd.group(1).strip()) != normalize_expr(lhs_acc):
        return None
    term = madd.group(2).strip()
    m_pow = re.match(
        r"^\s*([a-z][a-z0-9_]*)\s*\(\s*([^,()]+)\s*,\s*([^,()]+)\s*\)\s*\*\*\s*2(?:\.0+)?\s*$",
        term,
        re.IGNORECASE,
    )
    if not m_pow:
        return None
    arr = m_pow.group(1).lower()
    a1 = m_pow.group(2).strip()
    a2 = m_pow.group(3).strip()

    if not END_DO_RE.match(body[j + 4][1].strip()):
        return None

    m_sqrt = ASSIGN_RE.match(body[j + 5][1].strip())
    if m_sqrt is None:
        return None
    lhs_s = m_sqrt.group(1).strip()
    rhs_s = m_sqrt.group(2).strip()
    if normalize_expr(lhs_s) != normalize_expr(lhs_acc):
        return None
    if normalize_expr(rhs_s) != normalize_expr(f"sqrt({lhs_acc})"):
        return None

    if not END_DO_RE.match(body[j + 6][1].strip()):
        return None

    has_print = False
    print_stmt = ""
    end_idx = j + 7
    if j + 8 < len(body) and PRINT_STMT_RE.match(body[j + 7][1].strip()) and body[j + 8][1].strip().lower() == "end block":
        has_print = True
        print_stmt = body[j + 7][1].strip()
        end_idx = j + 8
    elif j + 7 < len(body) and body[j + 7][1].strip().lower() == "end block":
        has_print = False
        end_idx = j + 7
    else:
        return None

    lhs_base = fscan.base_identifier(lhs_acc)
    if lhs_base is None:
        return None
    m_lhs_idx = INDEXED_NAME_RE.match(lhs_acc)
    lhs_idx = m_lhs_idx.group(2).strip() if m_lhs_idx else None

    # Scalar norm2: lhs scalar, nested loops over both dims.
    if lhs_idx is None:
        if {normalize_expr(a1), normalize_expr(a2)} != {normalize_expr(outer), normalize_expr(inner)}:
            return None
        expr = f"{arr}({lb_i}:{ub_i}, {lb_o}:{ub_o})"
        expr = simplify_section_expr_rank2(expr, rank2_bounds)
        norm_expr = f"norm2({expr})"
    else:
        # Dim-reduction norm2: lhs indexed by outer loop variable.
        if normalize_expr(lhs_idx) != normalize_expr(outer):
            return None
        if normalize_expr(a1) == normalize_expr(inner) and normalize_expr(a2) == normalize_expr(outer):
            dim = 1
            expr = f"{arr}({lb_i}:{ub_i}, {lb_o}:{ub_o})"
        elif normalize_expr(a1) == normalize_expr(outer) and normalize_expr(a2) == normalize_expr(inner):
            dim = 2
            expr = f"{arr}({lb_o}:{ub_o}, {lb_i}:{ub_i})"
        else:
            return None
        expr = simplify_section_expr_rank2(expr, rank2_bounds)
        norm_expr = f"norm2({expr}, dim={dim})"

    if has_print:
        # Prefer direct PRINT rewrite to avoid leaking block-local temporaries.
        sugg = re.sub(rf"\b{re.escape(lhs_base)}\b", norm_expr, print_stmt, flags=re.IGNORECASE)
    else:
        sugg = f"{lhs_base} = {norm_expr}"
    return (end_idx, sugg, body[end_idx][0])


def maybe_block_random_number_rank2(
    body: List[Tuple[int, str]],
    i: int,
    rank2_bounds: Dict[str, Tuple[str, str]],
) -> Optional[Tuple[int, str, int]]:
    """Detect block:
      do j = 1,n2
         call random_number(x(:,j))
      end do
    and rewrite to call random_number(x).
    """
    if i >= len(body) or body[i][1].strip().lower() != "block":
        return None
    j = i + 1
    while j < len(body) and TYPE_DECL_RE.match(body[j][1].strip().lower()):
        j += 1
    if j + 3 >= len(body):
        return None
    m_do = DO_RE.match(body[j][1].strip())
    m_call = RANDOM_CALL_RE.match(body[j + 1][1].strip())
    if m_do is None or m_call is None:
        return None
    if not END_DO_RE.match(body[j + 2][1].strip()):
        return None
    if body[j + 3][1].strip().lower() != "end block":
        return None
    iv = m_do.group(1).strip()
    lb = m_do.group(2).strip()
    ub = m_do.group(3).strip()
    arg = m_call.group(1).strip()
    m_idx = INDEXED_SECOND_DIM_RE.match(arg)
    if m_idx is None:
        return None
    arr = m_idx.group(1).strip().lower()
    sec = m_idx.group(2).strip()
    idx2 = m_idx.group(3).strip()
    if normalize_expr(sec) != normalize_expr(":"):
        return None
    if normalize_expr(idx2) != normalize_expr(iv):
        return None
    rb = rank2_bounds.get(arr)
    if rb is None:
        return None
    if normalize_expr(lb) != normalize_expr("1") or normalize_expr(ub) != normalize_expr(rb[1]):
        return None
    return (j + 3, f"call random_number({arr})", body[j + 3][0])


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


def maybe_sqrt_sum_sq_to_norm2_expr(expr: str) -> Optional[str]:
    """Rewrite sqrt(sum(a**2[, dim])) -> norm2(a[, dim]) when structurally safe."""
    s = expr.strip()
    if not s.lower().startswith("sqrt("):
        return None
    op = s.find("(")
    if op < 0:
        return None
    cp = find_matching_paren(s, op)
    if cp < 0 or cp != len(s) - 1:
        return None
    inside = s[op + 1 : cp].strip()
    if not inside.lower().startswith("sum("):
        return None
    op2 = inside.find("(")
    if op2 < 0:
        return None
    cp2 = find_matching_paren(inside, op2)
    if cp2 < 0 or cp2 != len(inside) - 1:
        return None
    sum_inside = inside[op2 + 1 : cp2].strip()
    args = split_top_level_commas(sum_inside)
    if len(args) not in (1, 2):
        return None
    term = args[0].strip()
    mpow = re.match(
        r"^\s*\(?\s*(.+?)\s*\)?\s*\*\*\s*2(?:\.0+)?(?:_[a-z0-9_]+)?\s*$",
        term,
        re.IGNORECASE,
    )
    if not mpow:
        return None
    base = mpow.group(1).strip()
    if len(args) == 1:
        return f"norm2({base})"
    dim_arg = args[1].strip()
    if not dim_arg:
        return None
    return f"norm2({base}, {dim_arg})"


def maybe_norm2_stmt_rewrite(stmt: str) -> Optional[str]:
    """Rewrite assignment/PRINT* statements that can use NORM2 intrinsic."""
    mp = PRINT_PREFIX_RE.match(stmt)
    if mp:
        tail = mp.group(1).strip()
        items = [p.strip() for p in split_top_level_commas(tail)] if tail else []
        if not items:
            return None
        changed = False
        out_items: List[str] = []
        for it in items:
            nr = maybe_sqrt_sum_sq_to_norm2_expr(it)
            if nr is not None:
                out_items.append(nr)
                changed = True
            else:
                out_items.append(it)
        if changed:
            return "print*," + ", ".join(out_items)

    ms = ASSIGN_RE.match(stmt.strip())
    if ms:
        lhs = ms.group(1).strip()
        rhs = ms.group(2).strip()
        nr = maybe_sqrt_sum_sq_to_norm2_expr(rhs)
        if nr is not None and nr != rhs:
            return f"{lhs} = {nr}"
        return None
    return None


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


def rhs_has_unsafe_rank1_self_ref(rhs: str, lhs_name: str, assigned: Set[int]) -> bool:
    """
    True when RHS has same-array rank-1 references that may be unsafe to pack.

    Safe:
    - references like lhs(k) where k is a literal integer not in assigned.
    Unsafe/unknown:
    - non-literal indices, slices/vector subscripts, or rank>1 refs.
    - literal index that overlaps assigned.
    """
    s = strip_quoted_text(rhs)
    pat = re.compile(rf"\b{re.escape(lhs_name)}\s*\(([^()]*)\)", re.IGNORECASE)
    for m in pat.finditer(s):
        inside = m.group(1).strip()
        if not inside:
            return True
        if "," in inside or ":" in inside:
            return True
        if not re.fullmatch(r"[+-]?\d+", inside):
            return True
        if int(inside) in assigned:
            return True
    return False


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


REAL_LITERAL_KIND_RE = re.compile(
    r"^\s*[+-]?(?:(?:\d+\.\d*|\.\d+|\d+\.)(?:[eEdDqQ][+-]?\d+)?|\d+[eEdDqQ][+-]?\d+)"
    r"(?:_([a-z][a-z0-9_]*|\d+))?\s*$",
    re.IGNORECASE,
)


def real_literal_kind_tag(text: str) -> Optional[str]:
    """Return normalized kind tag for a real literal, else None."""
    t = text.strip()
    m = REAL_LITERAL_KIND_RE.match(t)
    if not m:
        return None
    ksuffix = m.group(1)
    if ksuffix:
        return f"_{ksuffix.lower()}"
    if re.search(r"[dDqQ][+-]?\d+", t):
        return "d"
    return "default"


def has_mixed_real_literal_kinds(parts: List[str]) -> bool:
    """True when constructor parts contain mixed real literal kinds."""
    tags: Set[str] = set()
    for p in parts:
        tag = real_literal_kind_tag(p)
        if tag is not None:
            tags.add(tag)
            if len(tags) > 1:
                return True
    return False


def neutral_real_literal_base(text: str) -> Optional[str]:
    """Return neutral base ('0.0' or '1.0') for promotable default real literals."""
    t = text.strip()
    if t in {"0.0", "+0.0", "-0.0"}:
        return "0.0"
    if t in {"1.0", "+1.0", "-1.0"}:
        return "1.0" if not t.startswith("-") else "-1.0"
    return None


def format_promoted_real_literal(base: str, target_tag: str) -> str:
    """Render promoted real literal in the target kind tag format."""
    if target_tag == "d":
        return f"{base}D0"
    if target_tag.startswith("_"):
        return f"{base}{target_tag}"
    return base


def normalize_constructor_real_literal_kinds(parts: List[str]) -> Optional[List[str]]:
    """
    Normalize mixed real-literal kinds when safely possible.

    Strategy:
    - Keep as-is when there is no kind mixing.
    - If exactly one non-default real kind is present, promote default neutral
      literals (0.0/-0.0/1.0/+1.0/-1.0) to that non-default kind.
    - Otherwise, return None (unsafe to rewrite).
    """
    tags: Set[str] = set()
    for p in parts:
        tag = real_literal_kind_tag(p)
        if tag is not None:
            tags.add(tag)
    if len(tags) <= 1:
        return list(parts)
    nondefault = {t for t in tags if t != "default"}
    if len(nondefault) != 1:
        return None
    target = next(iter(nondefault))
    out: List[str] = []
    for p in parts:
        tag = real_literal_kind_tag(p)
        if tag != "default":
            out.append(p)
            continue
        base = neutral_real_literal_base(p)
        if base is None:
            return None
        out.append(format_promoted_real_literal(base, target))
    return out


def format_constructor_rhs(parts: List[str], *, lhs_name: str, lhs_is_char_rank1: bool) -> str:
    """Format constructor RHS; add CHARACTER type-spec and wrap long constructors."""
    joined = ", ".join(parts)
    if len(parts) <= 16 and len(joined) <= 120:
        if lhs_is_char_rank1:
            return f"[character(len=len({lhs_name})) :: {joined}]"
        return f"[{joined}]"

    prefix = f"character(len=len({lhs_name})) :: " if lhs_is_char_rank1 else ""
    max_width = 100
    lines: List[str] = []
    current = ""
    for p in parts:
        piece = p if not current else f", {p}"
        if current and (len(current) + len(piece) > max_width):
            lines.append(current)
            current = p
        else:
            current += piece
    if current:
        lines.append(current)

    out: List[str] = []
    if prefix:
        out.append(f"[{prefix}&")
    else:
        out.append("[ &")
    for i, line in enumerate(lines):
        if i + 1 < len(lines):
            out.append(f"{line}, &")
        else:
            out.append(f"{line} ]")
    return "\n".join(out)


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
    character_rank1_array_names: Set[str],
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
    # Avoid unsafe same-array RHS reads of assigned indices.
    if rhs_has_unsafe_rank1_self_ref(rhs0, name, set(range(lo0, hi0 + 1))):
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
        if rhs_has_unsafe_rank1_self_ref(rhs, name, set(range(lo0, hi + 1))):
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
    normalized_parts = normalize_constructor_real_literal_kinds(parts)
    if normalized_parts is None:
        return None
    parts = normalized_parts
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
    suggestion = (
        f"{lhs_expr} = "
        f"{format_constructor_rhs(parts, lhs_name=name, lhs_is_char_rank1=name.lower() in character_rank1_array_names)}"
    )
    return end_idx, suggestion


def maybe_constructor_pack_sparse(
    body: List[Tuple[int, str]],
    start_idx: int,
    nondefault_real_names: Set[str],
    scalar_nondefault_real_names: Set[str],
    complex_rank1_names: Set[str],
    character_scalar_names: Set[str],
    character_rank1_array_names: Set[str],
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
    if rhs_has_unsafe_rank1_self_ref(rhs0, name0, {lo0}):
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
        if rhs_has_unsafe_rank1_self_ref(rhs, name0, set(idxs) | {lo}):
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
    normalized_vals = normalize_constructor_real_literal_kinds(vals)
    if normalized_vals is None:
        return None
    vals = normalized_vals
    if name0.lower() in nondefault_real_names and not allow_constructor_for_nondefault_real_with_scalars(
        vals, scalar_nondefault_real_names
    ):
        return None

    idx_vec = ", ".join(str(k) for k in idxs)
    rhs = format_constructor_rhs(
        vals, lhs_name=name0, lhs_is_char_rank1=name0.lower() in character_rank1_array_names
    )
    suggestion = f"{name0}([{idx_vec}]) = {rhs}"
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


def maybe_matmul_mv_sum_loop(
    loop_var: str,
    rng: str,
    body_stmt: str,
    decl_bounds1: Dict[str, str],
    rank2_bounds: Dict[str, Tuple[str, str]],
) -> Optional[str]:
    """Detect y(i)=sum(A(i,:)*x) loop and suggest y = matmul(A, x)."""
    def sec_for_compare(a_nm: str, sec_txt: str) -> str:
        st = sec_txt.strip()
        if st == ":":
            rb = rank2_bounds.get(a_nm.lower())
            if rb is not None:
                return f"1:{rb[1]}"
        return st

    m = ASSIGN_RE.match(body_stmt.strip())
    if not m:
        return None
    lhs = parse_indexed_name(m.group(1).strip())
    if lhs is None or len(lhs[1]) != 1 or lhs[1][0].lower() != loop_var.lower():
        return None
    y_name = lhs[0]

    rhs = m.group(2).strip()
    msum = re.match(r"^\s*sum\s*\(\s*(.+)\s*\)\s*$", rhs, re.IGNORECASE)
    if not msum:
        return None
    sum_arg = msum.group(1).strip()
    # Keep this conservative: only plain SUM(expr), not SUM(expr, dim=...).
    if len(split_top_level_commas(sum_arg)) != 1:
        return None

    mm = re.match(r"^\s*(.+?)\s*\*\s*(.+)\s*$", sum_arg, re.IGNORECASE)
    if not mm:
        return None
    factors = [mm.group(1).strip(), mm.group(2).strip()]
    parsed = [parse_indexed_name(f) for f in factors]
    simple = [SIMPLE_NAME_RE.match(f) for f in factors]

    a_name = ""
    x_name = ""
    sec = ""
    for idx in (0, 1):
        p_a = parsed[idx]
        if p_a is None:
            continue
        n0, a0 = p_a
        if len(a0) != 2 or a0[0].lower() != loop_var.lower():
            continue
        if re.search(rf"\b{re.escape(loop_var)}\b", a0[1], re.IGNORECASE):
            continue
        other = 1 - idx
        p_x = parsed[other]
        if p_x is not None:
            n1, a1 = p_x
            if len(a1) == 1 and normalize_expr(sec_for_compare(n0, a0[1])) == normalize_expr(a1[0]):
                a_name = n0
                x_name = n1
                sec = a0[1].strip()
                break
        elif simple[other] is not None:
            n1 = simple[other].group(1)
            xb = decl_bounds1.get(n1.lower())
            if xb is not None and normalize_expr(xb) == normalize_expr(sec_for_compare(n0, a0[1])):
                a_name = n0
                x_name = n1
                sec = a0[1].strip()
                break
    if not a_name:
        return None

    outer_lb, outer_ub = split_range(rng)
    y_sec = f"{y_name}({rng})"
    a_sec = f"{a_name}({rng}, {sec})"
    x_sec = x_name

    yb = decl_bounds1.get(y_name.lower())
    ab = rank2_bounds.get(a_name.lower())
    xb = decl_bounds1.get(x_name.lower())
    if (
        yb is not None
        and ab is not None
        and xb is not None
        and normalize_expr(yb) == normalize_expr(rng)
        and normalize_expr(ab[0]) == normalize_expr(outer_ub.strip())
        and normalize_expr(ab[1]) == normalize_expr(sec_for_compare(a_name, sec))
        and normalize_expr(xb) == normalize_expr(sec_for_compare(a_name, sec))
        and normalize_expr(outer_lb.strip()) == normalize_expr("1")
    ):
        return f"{y_name} = matmul({a_name}, {x_name})"

    y_sec = simplify_section_expr(y_sec, decl_bounds1)
    a_sec = simplify_section_expr(a_sec, decl_bounds1)
    a_sec = simplify_section_expr_rank2(a_sec, rank2_bounds)
    ab2 = rank2_bounds.get(a_name.lower())
    if ab2 is not None:
        if normalize_expr(a_sec) == normalize_expr(f"{a_name}(1:{ab2[0]},:)"):
            a_sec = a_name
    return f"{y_sec} = matmul({a_sec}, {x_sec})"


def maybe_matmul_mv_preinit_nested(
    init_stmt: str,
    outer_do_stmt: str,
    inner_do_stmt: str,
    inner_body_stmt: str,
    decl_bounds1: Dict[str, str],
    rank2_bounds: Dict[str, Tuple[str, str]],
) -> Optional[str]:
    """Detect y(:)=0; do i; do j; y(i)=y(i)+A(i,j)*x(j); enddo; enddo -> matmul(A,x)."""
    m_init = ASSIGN_RE.match(init_stmt.strip())
    m_odo = DO_RE.match(outer_do_stmt.strip())
    m_ido = DO_RE.match(inner_do_stmt.strip())
    m_acc = ASSIGN_RE.match(inner_body_stmt.strip())
    if not m_init or not m_odo or not m_ido or not m_acc:
        return None
    if not ZERO_LITERAL_RE.match(m_init.group(2).strip()):
        return None

    outer_var = m_odo.group(1)
    outer_rng = build_range(m_odo.group(2), m_odo.group(3), m_odo.group(4))
    inner_var = m_ido.group(1)
    inner_rng = build_range(m_ido.group(2), m_ido.group(3), m_ido.group(4))

    lhs_acc = parse_indexed_name(m_acc.group(1).strip())
    if lhs_acc is None or len(lhs_acc[1]) != 1 or lhs_acc[1][0].lower() != outer_var.lower():
        return None
    y_name = lhs_acc[0]

    # Init must target same vector as accumulator target (whole or full section).
    lhs_init_txt = m_init.group(1).strip()
    init_name = fscan.base_identifier(lhs_init_txt)
    if init_name is None or init_name.lower() != y_name.lower():
        return None
    init_idx = parse_indexed_name(lhs_init_txt)
    if init_idx is not None:
        if len(init_idx[1]) != 1:
            return None
        init_arg = init_idx[1][0].strip()
        if not (
            init_arg.lower() == outer_var.lower()
            or normalize_expr(init_arg) == normalize_expr(outer_rng)
        ):
            return None

    madd = re.match(r"^\s*(.+?)\s*\+\s*(.+)$", m_acc.group(2).strip(), re.IGNORECASE)
    if not madd:
        return None
    acc_ref = parse_indexed_name(madd.group(1).strip())
    if acc_ref is None or acc_ref != lhs_acc:
        return None

    prod = madd.group(2).strip()
    mm = re.match(r"^\s*(.+?)\s*\*\s*(.+)\s*$", prod, re.IGNORECASE)
    if not mm:
        return None
    factors = [mm.group(1).strip(), mm.group(2).strip()]
    parsed = [parse_indexed_name(f) for f in factors]
    if parsed[0] is None or parsed[1] is None:
        return None

    a_name = ""
    x_name = ""
    for p0, p1 in (parsed, parsed[::-1]):
        n0, a0 = p0
        n1, a1 = p1
        if (
            len(a0) == 2
            and len(a1) == 1
            and a0[0].lower() == outer_var.lower()
            and a0[1].lower() == inner_var.lower()
            and a1[0].lower() == inner_var.lower()
        ):
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
        if name in USER_ELEMENTAL_CALLS:
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
        if name in USER_ELEMENTAL_CALLS:
            continue
        if name in ELEMENTAL_INTRINSICS:
            continue
        return True
    return False


def bound_expr_has_indexing_on_var(bound_expr: str, loop_var: str) -> bool:
    """True if bound expression contains indexed call/reference mentioning loop_var."""
    s = strip_quoted_text(bound_expr)
    i = 0
    n = len(s)
    lv = loop_var.lower()
    while i < n:
        m = re.match(r"[a-z][a-z0-9_]*", s[i:], re.IGNORECASE)
        if not m:
            i += 1
            continue
        j = i + len(m.group(0))
        while j < n and s[j].isspace():
            j += 1
        if j < n and s[j] == "(":
            close = find_matching_paren(s, j)
            if close < 0:
                return True
            inside = s[j + 1 : close].lower()
            if re.search(rf"\b{re.escape(lv)}\b", inside):
                return True
            i = close + 1
            continue
        i = j
    return False


def paren_bracket_balanced(text: str) -> bool:
    """Check balanced (), [], {} outside quotes."""
    s = strip_quoted_text(text)
    depth_par = 0
    depth_brk = 0
    depth_brc = 0
    for ch in s:
        if ch == "(":
            depth_par += 1
        elif ch == ")":
            depth_par -= 1
            if depth_par < 0:
                return False
        elif ch == "[":
            depth_brk += 1
        elif ch == "]":
            depth_brk -= 1
            if depth_brk < 0:
                return False
        elif ch == "{":
            depth_brc += 1
        elif ch == "}":
            depth_brc -= 1
            if depth_brc < 0:
                return False
    return depth_par == 0 and depth_brk == 0 and depth_brc == 0


def do_triplet_fields_well_formed(lb: str, ub: str, step: Optional[str]) -> bool:
    """Conservative check: each parsed DO triplet field must be internally balanced."""
    if not paren_bracket_balanced(lb):
        return False
    if not paren_bracket_balanced(ub):
        return False
    if step is not None and not paren_bracket_balanced(step):
        return False
    return True


def maybe_loop_to_concurrent_or_forall(
    loop_var: str,
    rng: str,
    body_stmt: str,
    *,
    allow_concurrent: bool,
    allow_forall: bool,
    array_names: Set[str],
) -> Optional[Tuple[str, str]]:
    """Detect simple one-statement loop and suggest DO CONCURRENT block or one-line FORALL."""
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
    # Conservative legality guard for dependent/indirect bounds.
    if bound_expr_has_indexing_on_var(rng, loop_var):
        return None

    # Conservative dependence block: if rhs references lhs at all, skip.
    if re.search(rf"\b{re.escape(lhs_name)}\s*\(", rhs, re.IGNORECASE):
        return None

    if allow_concurrent:
        # Conservative purity proxy for concurrent mode: allow array refs and
        # elemental intrinsic calls; reject everything else.
        if not has_disallowed_function_calls(rhs, array_names):
            return (
                f"do concurrent ({loop_var} = {rng})\n"
                f"   {body_stmt.strip()}\n"
                f"end do",
                "do_concurrent",
            )

    if allow_forall:
        return f"forall ({loop_var} = {rng}) {body_stmt.strip()}", "one_line_forall"
    return None


def maybe_loopnest_to_concurrent_or_forall(
    loop_vars: List[str],
    ranges: List[str],
    body_stmt: str,
    *,
    allow_concurrent: bool,
    allow_forall: bool,
    array_names: Set[str],
) -> Optional[Tuple[str, str]]:
    """Detect simple one-statement nested loop and suggest DO CONCURRENT block/one-line FORALL."""
    if not loop_vars or len(loop_vars) != len(ranges):
        return None
    # Conservative legality guard: in DO CONCURRENT/FORALL control lists,
    # each triplet must not reference peer control variables.
    lv_lows = [v.lower() for v in loop_vars]
    for idx, r in enumerate(ranges):
        if bound_expr_has_indexing_on_var(r, loop_vars[idx]):
            return None
        rr = strip_quoted_text(r.lower())
        for jdx, v in enumerate(lv_lows):
            if jdx == idx:
                continue
            if re.search(rf"\b{re.escape(v)}\b", rr):
                return None
    m = ASSIGN_RE.match(body_stmt.strip())
    if not m:
        return None
    lhs = m.group(1).strip()
    rhs = m.group(2).strip()

    lhs_idx = parse_indexed_name(lhs)
    if lhs_idx is None:
        return None
    lhs_name, lhs_args = lhs_idx

    # Require each loop variable to appear exactly once across LHS subscripts.
    for v in loop_vars:
        n = sum(1 for a in lhs_args if a.strip().lower() == v.lower())
        if n != 1:
            return None

    # Conservative dependence block: if rhs references lhs at all, skip.
    if re.search(rf"\b{re.escape(lhs_name)}\s*\(", rhs, re.IGNORECASE):
        return None

    control = ", ".join(f"{v} = {r}" for v, r in zip(loop_vars, ranges))
    if allow_concurrent:
        # Conservative purity proxy for concurrent mode: allow array refs and
        # elemental intrinsic calls; reject everything else.
        if not has_disallowed_function_calls(rhs, array_names):
            return (
                f"do concurrent ({control})\n"
                f"   {body_stmt.strip()}\n"
                f"end do",
                "do_concurrent",
            )

    if allow_forall:
        return f"forall ({control}) {body_stmt.strip()}", "one_line_forall"
    return None


def maybe_masked_sum(
    loop_var: str,
    rng: str,
    prev_stmt: Optional[str],
    if_stmt: str,
    body_stmt: str,
    decl_bounds: Dict[str, str],
    array_names: Set[str],
    decl_scalar_inits: Optional[Dict[str, str]] = None,
) -> Optional[str]:
    """Detect masked sum inside IF block within loop."""
    mif = IF_THEN_RE.match(if_stmt.strip())
    ml = ASSIGN_RE.match(body_stmt.strip())
    if not mif or not ml:
        return None
    lhs = SIMPLE_NAME_RE.match(ml.group(1).strip())
    if not lhs:
        return None
    acc = lhs.group(1).lower()
    init_seen = False
    init_expr: Optional[str] = None
    if prev_stmt is not None:
        mp = ASSIGN_RE.match(prev_stmt.strip())
        if mp:
            acc_prev = SIMPLE_NAME_RE.match(mp.group(1).strip())
            if acc_prev and acc_prev.group(1).lower() == acc:
                init_seen = True
                init_expr = mp.group(2).strip()
    if not init_seen and decl_scalar_inits is not None and acc in decl_scalar_inits:
        init_seen = True
        init_expr = decl_scalar_inits[acc].strip()
    if not init_seen:
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
    sum_expr = f"sum({expr_s}, mask = {cond_s})"
    if not init_expr:
        return f"{acc} = {sum_expr}"
    if normalize_expr(init_expr) in {"0", "+0", "0.0", "+0.0", "0d0", "+0d0", "0.0d0", "+0.0d0"}:
        return f"{acc} = {sum_expr}"
    return f"{acc} = {init_expr} + {sum_expr}"


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

    parsed_if = parse_one_line_if(inline_if_stmt)
    if parsed_if is None:
        return None
    cond, body_stmt = parsed_if
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

    parsed_if = parse_one_line_if(inline_if_stmt)
    if parsed_if is None:
        return None
    cond, body_stmt = parsed_if
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

    parsed_if = parse_one_line_if(inline_if_stmt)
    if parsed_if is None:
        return None
    cond, body_stmt = parsed_if
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
    decl_scalar_inits: Optional[Dict[str, str]] = None,
) -> Optional[str]:
    """Detect one-line IF masked sum reduction and build SUM replacement."""
    parsed_if = parse_one_line_if(inline_if_stmt)
    if parsed_if is None:
        return None
    cond, body_stmt = parsed_if
    if body_stmt.lower().startswith("then"):
        return None

    ml = ASSIGN_RE.match(body_stmt)
    if not ml:
        return None
    lhs = SIMPLE_NAME_RE.match(ml.group(1).strip())
    if not lhs:
        return None
    acc = lhs.group(1).lower()
    init_seen = False
    init_expr: Optional[str] = None
    if prev_stmt is not None:
        mp = ASSIGN_RE.match(prev_stmt.strip())
        if mp:
            acc_prev = SIMPLE_NAME_RE.match(mp.group(1).strip())
            if acc_prev and acc_prev.group(1).lower() == acc:
                init_seen = True
                init_expr = mp.group(2).strip()
    if not init_seen and decl_scalar_inits is not None and acc in decl_scalar_inits:
        init_seen = True
        init_expr = decl_scalar_inits[acc].strip()
    if not init_seen:
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
    sum_expr = f"sum({expr_s}, mask = {cond_s})"
    if not init_expr:
        return f"{acc} = {sum_expr}"
    if normalize_expr(init_expr) in {"0", "+0", "0.0", "+0.0", "0d0", "+0d0", "0.0d0", "+0.0d0"}:
        return f"{acc} = {sum_expr}"
    return f"{acc} = {init_expr} + {sum_expr}"


def collect_recent_scalar_assignments(body: List[Tuple[int, str]], do_idx: int, max_lookback: int = 6) -> Dict[str, str]:
    """Collect recent simple scalar assignments immediately before loop start."""
    out: Dict[str, str] = {}
    k = do_idx - 1
    seen = 0
    while k >= 0 and seen < max_lookback:
        stmt = body[k][1].strip()
        if (
            DO_RE.match(stmt)
            or END_DO_RE.match(stmt)
            or IF_THEN_RE.match(stmt)
            or END_IF_RE.match(stmt)
            or ELSE_RE.match(stmt)
        ):
            break
        m = ASSIGN_RE.match(stmt)
        if not m:
            break
        lhs = SIMPLE_NAME_RE.match(m.group(1).strip())
        if lhs:
            out.setdefault(lhs.group(1).lower(), m.group(2).strip())
        k -= 1
        seen += 1
    return out


def init_block_start_line_for_accs(
    body: List[Tuple[int, str]],
    do_idx: int,
    acc_names: Set[str],
    max_lookback: int = 6,
) -> Optional[int]:
    """Return earliest contiguous pre-loop assignment line covering all acc_names."""
    found: Dict[str, int] = {}
    k = do_idx - 1
    seen = 0
    while k >= 0 and seen < max_lookback:
        ln_k, stmt_k = body[k]
        st = stmt_k.strip()
        if (
            DO_RE.match(st)
            or END_DO_RE.match(st)
            or IF_THEN_RE.match(st)
            or END_IF_RE.match(st)
            or ELSE_RE.match(st)
        ):
            break
        m = ASSIGN_RE.match(st)
        if not m:
            break
        lhs = SIMPLE_NAME_RE.match(m.group(1).strip())
        if lhs:
            n = lhs.group(1).lower()
            if n in acc_names and n not in found:
                found[n] = ln_k
        k -= 1
        seen += 1
    if not acc_names.issubset(set(found.keys())):
        return None
    return min(found.values())


def maybe_masked_sum_product_block(
    loop_var: str,
    rng: str,
    if_stmt: str,
    stmt_a: str,
    stmt_b: str,
    decl_bounds: Dict[str, str],
    array_names: Set[str],
    init_map: Dict[str, str],
    decl_scalar_inits: Dict[str, str],
) -> Optional[str]:
    """Detect IF-block with sum+= and prod*= updates under same mask."""
    mif = IF_THEN_RE.match(if_stmt.strip())
    if not mif:
        return None
    cond = mif.group(1).strip()

    def parse_acc(stmt: str) -> Optional[Tuple[str, str, str]]:
        m = ASSIGN_RE.match(stmt.strip())
        if not m:
            return None
        lhs = SIMPLE_NAME_RE.match(m.group(1).strip())
        if not lhs:
            return None
        acc = lhs.group(1).lower()
        rhs = m.group(2).strip()
        madd = ADD_ACC_RE.match(rhs)
        if madd and madd.group(1).lower() == acc:
            return ("sum", acc, madd.group(2).strip())
        mmul = MUL_ACC_RE.match(rhs)
        if mmul and mmul.group(1).lower() == acc:
            return ("prod", acc, mmul.group(2).strip())
        return None

    p1 = parse_acc(stmt_a)
    p2 = parse_acc(stmt_b)
    if p1 is None or p2 is None:
        return None
    if p1[1] == p2[1]:
        return None
    kinds = {p1[0], p2[0]}
    if kinds != {"sum", "prod"}:
        return None

    items = [p1, p2]
    out_lines: List[str] = []
    lb, ubtxt = rng.split(":", 1)
    cond_s = replace_index_with_slice(cond, loop_var, lb, ubtxt)
    cond_s = simplify_section_expr(cond_s, decl_bounds)
    if has_loop_var(cond_s, loop_var) or cond_s == cond:
        return None

    for kind, acc, expr in items:
        expr_s = replace_index_with_slice(expr, loop_var, lb, ubtxt)
        if has_loop_var(expr_s, loop_var) or expr_s == expr:
            return None
        expr_s = simplify_section_expr(expr_s, decl_bounds)
        if has_disallowed_function_calls(expr_s, array_names) or has_disallowed_function_calls(cond_s, array_names):
            return None

        init_expr = init_map.get(acc, decl_scalar_inits.get(acc, "")).strip()
        if not init_expr:
            return None
        if kind == "sum":
            red = f"sum({expr_s}, mask = {cond_s})"
            if normalize_expr(init_expr) in {"0", "+0", "0.0", "+0.0", "0d0", "+0d0", "0.0d0", "+0.0d0"}:
                out_lines.append(f"{acc} = {red}")
            else:
                out_lines.append(f"{acc} = {init_expr} + {red}")
        else:
            red = f"product({expr_s}, mask = {cond_s})"
            if normalize_expr(init_expr) in {"1", "+1", "1.0", "+1.0", "1d0", "+1d0", "1.0d0", "+1.0d0"}:
                out_lines.append(f"{acc} = {red}")
            else:
                out_lines.append(f"{acc} = {init_expr} * {red}")
    return "\n".join(out_lines)


def prev_stmt_assigns_name(prev_stmt: Optional[str], name: str) -> bool:
    """True when prev_stmt is a simple assignment to NAME."""
    if prev_stmt is None:
        return False
    mp = ASSIGN_RE.match(prev_stmt.strip())
    if not mp:
        return False
    lhs = SIMPLE_NAME_RE.match(mp.group(1).strip())
    return lhs is not None and lhs.group(1).lower() == name.lower()


def masked_sum_acc_from_block_stmt(stmt_if: str, stmt_inside: str) -> Optional[str]:
    """Return accumulator name for `if(cond) acc=acc+...` block body, else None."""
    if not IF_THEN_RE.match(stmt_if.strip()):
        return None
    ml = ASSIGN_RE.match(stmt_inside.strip())
    if not ml:
        return None
    lhs = SIMPLE_NAME_RE.match(ml.group(1).strip())
    if not lhs:
        return None
    acc = lhs.group(1).lower()
    madd = ADD_ACC_RE.match(ml.group(2).strip())
    if not madd or madd.group(1).lower() != acc:
        return None
    return acc


def masked_sum_acc_from_inline_if(stmt_inline_if: str) -> Optional[str]:
    """Return accumulator name for one-line `if(cond) acc=acc+...`, else None."""
    parsed = parse_one_line_if(stmt_inline_if)
    if parsed is None:
        return None
    _cond, body_stmt = parsed
    if body_stmt.lower().startswith("then"):
        return None
    ml = ASSIGN_RE.match(body_stmt.strip())
    if not ml:
        return None
    lhs = SIMPLE_NAME_RE.match(ml.group(1).strip())
    if not lhs:
        return None
    acc = lhs.group(1).lower()
    madd = ADD_ACC_RE.match(ml.group(2).strip())
    if not madd or madd.group(1).lower() != acc:
        return None
    return acc


def parse_extreme_condition(
    cond: str, loop_var: str
) -> Optional[Tuple[str, str, str, bool]]:
    """Parse min/max compare condition into (kind, scalar_var, array_name, strict)."""
    m = CMP_RE.match(cond.strip())
    if not m:
        return None
    lhs = m.group(1).strip()
    op = canonical_relop(m.group(2))
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
    parsed_if = parse_one_line_if(inline_if_stmt)
    if parsed_if is None:
        return None
    cond, body_stmt = parsed_if
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

    lb, _ub = split_range(rng)
    if normalize_expr(lb) != normalize_expr("1"):
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

    lb, _ub = split_range(rng)
    if normalize_expr(lb) != normalize_expr("1"):
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


def maybe_sum_merge_ifelse(
    loop_var: str,
    rng: str,
    prev_stmt: Optional[str],
    if_stmt: str,
    true_stmt: str,
    false_stmt: str,
    decl_bounds: Dict[str, str],
    array_names: Set[str],
    decl_scalar_inits: Optional[Dict[str, str]] = None,
) -> Optional[str]:
    """Detect IF/ELSE accumulator fold and suggest SUM(MERGE(...))."""
    mif = IF_THEN_RE.match(if_stmt.strip())
    mt = ASSIGN_RE.match(true_stmt.strip())
    mf = ASSIGN_RE.match(false_stmt.strip())
    if not mif or not mt or not mf:
        return None

    lhs_t = SIMPLE_NAME_RE.match(mt.group(1).strip())
    lhs_f = SIMPLE_NAME_RE.match(mf.group(1).strip())
    if not lhs_t or not lhs_f:
        return None
    acc = lhs_t.group(1).lower()
    if lhs_f.group(1).lower() != acc:
        return None

    at = ADD_ACC_RE.match(mt.group(2).strip())
    af = ADD_ACC_RE.match(mf.group(2).strip())
    if not at or not af:
        return None
    if at.group(1).lower() != acc or af.group(1).lower() != acc:
        return None

    init_seen = False
    init_expr: Optional[str] = None
    if prev_stmt is not None:
        mp = ASSIGN_RE.match(prev_stmt.strip())
        if mp:
            lhs_prev = SIMPLE_NAME_RE.match(mp.group(1).strip())
            if lhs_prev and lhs_prev.group(1).lower() == acc:
                init_seen = True
                init_expr = mp.group(2).strip()
    if not init_seen and decl_scalar_inits is not None and acc in decl_scalar_inits:
        init_seen = True
        init_expr = decl_scalar_inits[acc].strip()
    if not init_seen:
        return None

    cond = mif.group(1).strip()
    expr_t = at.group(2).strip()
    expr_f = af.group(2).strip()
    lb, ubtxt = split_range(rng)
    cond_s = replace_index_with_slice(cond, loop_var, lb, ubtxt)
    expr_t_s = replace_index_with_slice(expr_t, loop_var, lb, ubtxt)
    expr_f_s = replace_index_with_slice(expr_f, loop_var, lb, ubtxt)
    if (
        has_loop_var(cond_s, loop_var)
        or has_loop_var(expr_t_s, loop_var)
        or has_loop_var(expr_f_s, loop_var)
    ):
        return None
    if cond_s == cond or expr_t_s == expr_t or expr_f_s == expr_f:
        return None
    cond_s = simplify_section_expr(cond_s, decl_bounds)
    expr_t_s = simplify_section_expr(expr_t_s, decl_bounds)
    expr_f_s = simplify_section_expr(expr_f_s, decl_bounds)
    if (
        has_disallowed_function_calls(cond_s, array_names)
        or has_disallowed_function_calls(expr_t_s, array_names)
        or has_disallowed_function_calls(expr_f_s, array_names)
    ):
        return None

    sum_expr = f"sum(merge({expr_t_s}, {expr_f_s}, {cond_s}))"
    if not init_expr:
        return f"{acc} = {sum_expr}"
    if normalize_expr(init_expr) in {"0", "+0", "0.0", "+0.0", "0d0", "+0d0", "0.0d0", "+0.0d0"}:
        return f"{acc} = {sum_expr}"
    return f"{acc} = {init_expr} + {sum_expr}"


def _rel_true_for_op(op: str, rel: int) -> bool:
    """Evaluate op for relation rel where rel = sign(scalar - array_elem)."""
    if op == "<":
        return rel < 0
    if op == "<=":
        return rel <= 0
    if op == ">":
        return rel > 0
    if op == ">=":
        return rel >= 0
    return False


def parse_minmax_fold_rhs(rhs: str, scalar: str, loop_var: str) -> Optional[Tuple[str, str]]:
    """Parse RHS as min/max fold and return (kind,min|max, array_name)."""
    # Canonical min/max call.
    mfun = re.match(r"^\s*(min|max)\s*\((.*)\)\s*$", rhs, re.IGNORECASE)
    if mfun:
        kind = mfun.group(1).lower()
        args = split_top_level_commas(mfun.group(2))
        if len(args) != 2:
            return None
        a0 = args[0].strip()
        a1 = args[1].strip()
        idx0 = INDEXED_NAME_RE.match(a0)
        idx1 = INDEXED_NAME_RE.match(a1)
        s0 = SIMPLE_NAME_RE.match(a0)
        s1 = SIMPLE_NAME_RE.match(a1)
        ok = False
        arr = ""
        if s0 and idx1:
            ok = (
                s0.group(1).lower() == scalar
                and idx1.group(2).lower() == loop_var.lower()
            )
            arr = idx1.group(1) if ok else ""
        if s1 and idx0:
            ok2 = (
                s1.group(1).lower() == scalar
                and idx0.group(2).lower() == loop_var.lower()
            )
            if ok2:
                ok = True
                arr = idx0.group(1)
        if ok:
            return kind, arr
        return None

    # Equivalent MERGE forms:
    #   merge(s, a(i), s < a(i))   -> min
    #   merge(a(i), s, a(i) < s)   -> min
    # and analogous max forms; < and <= treated the same (also old-style ops).
    mm = re.match(r"^\s*merge\s*\((.*)\)\s*$", rhs, re.IGNORECASE)
    if not mm:
        return None
    parts = split_top_level_commas(mm.group(1))
    if len(parts) != 3:
        return None
    texpr = parts[0].strip()
    fexpr = parts[1].strip()
    cexpr = parts[2].strip()

    def expr_kind(expr: str) -> Optional[Tuple[str, str]]:
        ms = SIMPLE_NAME_RE.match(expr)
        if ms and ms.group(1).lower() == scalar:
            return ("s", "")
        mi = INDEXED_NAME_RE.match(expr)
        if mi and mi.group(2).lower() == loop_var.lower():
            return ("a", mi.group(1).lower())
        return None

    tk = expr_kind(texpr)
    fk = expr_kind(fexpr)
    if tk is None or fk is None:
        return None
    if {tk[0], fk[0]} != {"s", "a"}:
        return None
    arr = tk[1] if tk[0] == "a" else fk[1]

    mc = CMP_RE.match(cexpr)
    if not mc:
        return None
    lk = expr_kind(mc.group(1).strip())
    rk = expr_kind(mc.group(3).strip())
    if lk is None or rk is None:
        return None
    if {lk[0], rk[0]} != {"s", "a"}:
        return None
    arr_c = lk[1] if lk[0] == "a" else rk[1]
    if arr_c != arr:
        return None
    op = canonical_relop(mc.group(2))

    # rel = sign(s - a): -1 means s<a, +1 means s>a
    def selected_for_rel(rel: int) -> str:
        cmp_rel = rel if lk[0] == "s" else -rel
        truth = _rel_true_for_op(op, cmp_rel)
        return tk[0] if truth else fk[0]

    sel_neg = selected_for_rel(-1)  # s < a
    sel_pos = selected_for_rel(1)   # s > a
    if sel_neg == "s" and sel_pos == "a":
        return ("min", arr)
    if sel_neg == "a" and sel_pos == "s":
        return ("max", arr)
    return None


def branch_init_on_true(cond: str, loop_var: str, lb: str) -> Optional[bool]:
    """Return whether init branch is the IF-true branch for first-iteration split."""
    c = cond.strip()
    lbn = normalize_expr(lb)
    vbn = normalize_expr(loop_var)

    meq = EQ_RE.match(c)
    if meq:
        c_l = normalize_expr(meq.group(1).strip())
        c_r = normalize_expr(meq.group(3).strip())
        if (c_l == vbn and c_r == lbn) or (c_r == vbn and c_l == lbn):
            return True
        return None

    mc = CMP_RE.match(c)
    if not mc:
        return None
    c_l = normalize_expr(mc.group(1).strip())
    c_r = normalize_expr(mc.group(3).strip())
    op = canonical_relop(mc.group(2))

    # Fold-on-true forms: i > lb, lb < i
    if (c_l == vbn and c_r == lbn and op == ">") or (c_l == lbn and c_r == vbn and op == "<"):
        return False
    # Init-on-true forms: i <= lb, lb >= i
    if (c_l == vbn and c_r == lbn and op == "<=") or (c_l == lbn and c_r == vbn and op == ">="):
        return True

    def int_value(expr: str) -> Optional[int]:
        mm = INT_LITERAL_RE.match(expr.strip())
        if not mm:
            return None
        try:
            return int(mm.group(1))
        except ValueError:
            return None

    # Optional support for >= lb+1 / <= lb+1 equivalents.
    lb1a = normalize_expr(f"{lb}+1")
    lb1b = normalize_expr(f"({lb})+1")
    if (c_l == vbn and c_r in {lb1a, lb1b} and op == ">=") or (
        c_l in {lb1a, lb1b} and c_r == vbn and op == "<="
    ):
        return False
    if (c_l == vbn and c_r in {lb1a, lb1b} and op == "<") or (
        c_l in {lb1a, lb1b} and c_r == vbn and op == ">"
    ):
        return True

    # Numeric fallback: if loop var compared to integer constants.
    lbv = int_value(lb)
    lv = int_value(c_l)
    rv = int_value(c_r)
    if lbv is not None:
        # loop_var op K
        if c_l == vbn and rv is not None:
            if op == ">=" and rv == lbv + 1:
                return False
            if op == ">" and rv == lbv:
                return False
            if op == "<=" and rv == lbv:
                return True
            if op == "<" and rv == lbv + 1:
                return True
        # K op loop_var
        if c_r == vbn and lv is not None:
            if op == "<=" and lv == lbv + 1:
                return False
            if op == "<" and lv == lbv:
                return False
            if op == ">=" and lv == lbv:
                return True
            if op == ">" and lv == lbv + 1:
                return True
    return None


def maybe_extreme_init_else_fold(
    loop_var: str,
    rng: str,
    if_stmt: str,
    true_stmt: str,
    false_stmt: str,
    decl_bounds: Dict[str, str],
) -> Optional[str]:
    """Detect if(i==lb) s=a(i) else s=min/max(s,a(i)) and suggest MINVAL/MAXVAL."""
    mif = IF_THEN_RE.match(if_stmt.strip())
    if not mif:
        return None
    cond = mif.group(1).strip()
    lb, _ub = split_range(rng)
    init_true = branch_init_on_true(cond, loop_var, lb)
    if init_true is None:
        return None

    init_stmt = true_stmt if init_true else false_stmt
    fold_stmt = false_stmt if init_true else true_stmt
    mt = ASSIGN_RE.match(init_stmt.strip())
    mf = ASSIGN_RE.match(fold_stmt.strip())
    if not mt or not mf:
        return None
    lhs_t = SIMPLE_NAME_RE.match(mt.group(1).strip())
    lhs_f = SIMPLE_NAME_RE.match(mf.group(1).strip())
    rhs_t = INDEXED_ANY_RE.match(mt.group(2).strip())
    if not lhs_t or not lhs_f or not rhs_t:
        return None
    scalar = lhs_t.group(1).lower()
    if lhs_f.group(1).lower() != scalar:
        return None
    arr = rhs_t.group(1)
    idx_t = rhs_t.group(2).strip()
    if not (
        normalize_expr(idx_t) == normalize_expr(loop_var)
        or normalize_expr(idx_t) == normalize_expr(lb)
    ):
        return None

    parsed_fold = parse_minmax_fold_rhs(mf.group(2).strip(), scalar, loop_var)
    if parsed_fold is None:
        return None
    kind, arr_fold = parsed_fold
    if arr_fold.lower() != arr.lower():
        return None

    sec = simplify_section_expr(f"{arr}({rng})", decl_bounds)
    fn = "minval" if kind == "min" else "maxval"
    return f"{scalar} = {fn}({sec})"


def maybe_extreme_init_else_fold_pair(
    loop_var: str,
    rng: str,
    if_stmt: str,
    t1: str,
    t2: str,
    f1: str,
    f2: str,
    decl_bounds: Dict[str, str],
) -> Optional[str]:
    """Detect paired xmin/xmax init+fold IF/ELSE and suggest MINVAL+MAXVAL."""
    mif = IF_THEN_RE.match(if_stmt.strip())
    if not mif:
        return None
    cond = mif.group(1).strip()
    lb, _ub = split_range(rng)
    init_true = branch_init_on_true(cond, loop_var, lb)
    if init_true is None:
        return None
    init_a, init_b = (t1, t2) if init_true else (f1, f2)
    fold_a, fold_b = (f1, f2) if init_true else (t1, t2)

    def parse_init(stmt: str) -> Optional[Tuple[str, str]]:
        m = ASSIGN_RE.match(stmt.strip())
        if not m:
            return None
        lhs = SIMPLE_NAME_RE.match(m.group(1).strip())
        rhs = INDEXED_ANY_RE.match(m.group(2).strip())
        if not lhs or not rhs:
            return None
        idx = rhs.group(2).strip()
        if not (
            normalize_expr(idx) == normalize_expr(loop_var)
            or normalize_expr(idx) == normalize_expr(lb)
        ):
            return None
        return lhs.group(1).lower(), rhs.group(1)

    def parse_fold(stmt: str) -> Optional[Tuple[str, str]]:
        m = ASSIGN_RE.match(stmt.strip())
        if not m:
            return None
        lhs = SIMPLE_NAME_RE.match(m.group(1).strip())
        if not lhs:
            return None
        scalar = lhs.group(1).lower()
        parsed = parse_minmax_fold_rhs(m.group(2).strip(), scalar, loop_var)
        if parsed is None:
            return None
        kind, arr = parsed
        fn = "minval" if kind == "min" else "maxval"
        return scalar, f"{scalar} = {fn}({simplify_section_expr(f'{arr}({rng})', decl_bounds)})"

    i1 = parse_init(init_a)
    i2 = parse_init(init_b)
    f_1 = parse_fold(fold_a)
    f_2 = parse_fold(fold_b)
    if not i1 or not i2 or not f_1 or not f_2:
        return None

    # Match each fold assignment to corresponding initialized scalar/array.
    init_map = {i1[0]: i1[1].lower(), i2[0]: i2[1].lower()}
    fold_map = {f_1[0]: f_1[1], f_2[0]: f_2[1]}
    if len(init_map) != 2 or len(fold_map) != 2:
        return None
    if set(init_map.keys()) != set(fold_map.keys()):
        return None

    # Ensure each scalar initialized from the same array used in fold expression.
    for scalar, arr in init_map.items():
        sec = simplify_section_expr(f"{arr}({rng})", decl_bounds)
        expr = fold_map[scalar]
        if sec not in expr:
            return None

    return "\n".join([fold_map[k] for k in sorted(fold_map.keys())])


def maybe_preinit_extreme_pair_then_loop(
    if_stmt: str,
    init1: str,
    init2: str,
    do_stmt: str,
    fold1: str,
    fold2: str,
    decl_bounds: Dict[str, str],
) -> Optional[str]:
    """Detect guarded xmin/xmax init followed by min/max fold loop."""
    mif = IF_THEN_RE.match(if_stmt.strip())
    mdo = DO_RE.match(do_stmt.strip())
    if not mif or not mdo:
        return None
    cond = mif.group(1).strip()
    loop_var = mdo.group(1).lower()
    lb = mdo.group(2).strip()
    ub = mdo.group(3).strip()
    step = mdo.group(4)
    if normalize_expr(lb) != "2":
        return None
    if step is not None and normalize_expr(step.strip()) != "1":
        return None

    def parse_init(stmt: str) -> Optional[Tuple[str, str]]:
        m = ASSIGN_RE.match(stmt.strip())
        if not m:
            return None
        lhs = SIMPLE_NAME_RE.match(m.group(1).strip())
        rhs = INDEXED_ANY_RE.match(m.group(2).strip())
        if not lhs or not rhs:
            return None
        idx = rhs.group(2).strip()
        if normalize_expr(idx) != "1":
            return None
        return lhs.group(1).lower(), rhs.group(1).lower()

    def parse_assign_xi(stmt: str) -> Optional[Tuple[str, str]]:
        m = ASSIGN_RE.match(stmt.strip())
        if not m:
            return None
        lhs = SIMPLE_NAME_RE.match(m.group(1).strip())
        rhs = INDEXED_NAME_RE.match(m.group(2).strip())
        if not lhs or not rhs:
            return None
        if rhs.group(2).lower() != loop_var:
            return None
        return lhs.group(1).lower(), rhs.group(1).lower()

    def parse_fold_assign(stmt: str) -> Optional[Tuple[str, str, str]]:
        m = ASSIGN_RE.match(stmt.strip())
        if not m:
            return None
        lhs = SIMPLE_NAME_RE.match(m.group(1).strip())
        if not lhs:
            return None
        scalar = lhs.group(1).lower()
        parsed = parse_minmax_fold_rhs(m.group(2).strip(), scalar, loop_var)
        if parsed is None:
            return None
        kind, arr = parsed
        return scalar, kind, arr

    def parse_fold_inline_if(stmt: str) -> Optional[Tuple[str, str, str]]:
        p = parse_one_line_if(stmt)
        if p is None:
            return None
        cond, body_stmt = p
        ax = parse_assign_xi(body_stmt)
        if ax is None:
            return None
        scalar, arr = ax
        ext = parse_extreme_condition(cond, loop_var)
        if ext is None:
            return None
        kind, scalar_c, arr_c, _strict = ext
        if scalar_c != scalar or arr_c.lower() != arr.lower():
            return None
        return scalar, kind, arr

    def parse_fold(stmt: str) -> Optional[Tuple[str, str, str]]:
        return parse_fold_assign(stmt) or parse_fold_inline_if(stmt)

    i1 = parse_init(init1)
    i2 = parse_init(init2)
    f1 = parse_fold(fold1)
    f2 = parse_fold(fold2)
    if not i1 or not i2 or not f1 or not f2:
        return None

    init_map = {i1[0]: i1[1], i2[0]: i2[1]}
    if len(init_map) != 2:
        return None
    fold_map = {f1[0]: (f1[1], f1[2]), f2[0]: (f2[1], f2[2])}
    if len(fold_map) != 2:
        return None
    if set(init_map.keys()) != set(fold_map.keys()):
        return None

    # Require one min and one max fold; arrays must match initial array.
    kinds = {fold_map[k][0] for k in fold_map}
    if kinds != {"min", "max"}:
        return None
    for s, arr_init in init_map.items():
        kind, arr_fold = fold_map[s]
        _ = kind
        if arr_init != arr_fold:
            return None

    # Build guarded replacement that preserves edge-case behavior when ub < 1.
    arr_name = next(iter(init_map.values()))
    sec_raw = f"{arr_name}(1:max(1,{ub}))"
    arr_bnd = decl_bounds.get(arr_name.lower())
    # If guard implies ub is positive and ub is full declared bound, simplify to whole array.
    arr_ub = None
    if arr_bnd is not None:
        arr_ub = arr_bnd.split(":", 1)[1].strip() if ":" in arr_bnd else arr_bnd.strip()
    if arr_ub is not None and normalize_expr(arr_ub) == normalize_expr(ub):
        mc = CMP_RE.match(cond)
        guard_pos = False
        if mc:
            cl = mc.group(1).strip()
            op = canonical_relop(mc.group(2))
            cr = mc.group(3).strip()
            u = normalize_expr(ub)
            l = normalize_expr(cl)
            r = normalize_expr(cr)
            guard_pos = (
                (l == u and ((op == ">" and r == "0") or (op == ">=" and r == "1")))
                or (r == u and ((op == "<" and l == "0") or (op == "<=" and l == "1")))
            )
        if guard_pos:
            sec_raw = arr_name
    sec = simplify_section_expr(sec_raw, decl_bounds)
    s_min = next(s for s in fold_map if fold_map[s][0] == "min")
    s_max = next(s for s in fold_map if fold_map[s][0] == "max")
    return "\n".join(
        [
            f"if ({cond}) then",
            f"   {s_min} = minval({sec})",
            f"   {s_max} = maxval({sec})",
            "end if",
        ]
    )


def maybe_preinit_extreme_pair_then_loop_blocks(
    if_stmt: str,
    init1: str,
    init2: str,
    do_stmt: str,
    if1: str,
    as1: str,
    eif1: str,
    if2: str,
    as2: str,
    eif2: str,
    decl_bounds: Dict[str, str],
) -> Optional[str]:
    """Detect guarded init followed by two IF-block updates for min/max."""
    if not END_IF_RE.match(eif1.strip()) or not END_IF_RE.match(eif2.strip()):
        return None
    m1 = IF_THEN_RE.match(if1.strip())
    m2 = IF_THEN_RE.match(if2.strip())
    if not m1 or not m2:
        return None
    s1 = f"if ({m1.group(1).strip()}) {as1.strip()}"
    s2 = f"if ({m2.group(1).strip()}) {as2.strip()}"
    return maybe_preinit_extreme_pair_then_loop(
        if_stmt, init1, init2, do_stmt, s1, s2, decl_bounds
    )


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


def simplify_section_expr_rank2_partial(expr: str, rank2_bounds: Dict[str, Tuple[str, str]]) -> str:
    """Simplify full first/second dimension sections to ':' when safe."""
    s = expr
    for name, (b1, b2) in rank2_bounds.items():
        pat = re.compile(rf"\b{re.escape(name)}\s*\(\s*([^,]+)\s*,\s*([^)]+)\)", re.IGNORECASE)

        def repl(m: re.Match[str]) -> str:
            r1 = m.group(1).strip()
            r2 = m.group(2).strip()
            r1n = normalize_expr(r1)
            r2n = normalize_expr(r2)
            full1 = normalize_expr(f"1:{b1}")
            full2 = normalize_expr(f"1:{b2}")
            if r1n == full1 and r2n == full2:
                return name
            if r1n == full1:
                return f"{name}(:, {r2})"
            if r2n == full2:
                return f"{name}({r1}, :)"
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


def _has_loop_var_ref(expr: str, loop_var: str) -> bool:
    return re.search(rf"\b{re.escape(loop_var)}\b", expr, re.IGNORECASE) is not None


def _parse_write_stmt(stmt: str) -> Optional[Tuple[str, str]]:
    s = stmt.strip()
    if not s.lower().startswith("write"):
        return None
    i = s.lower().find("write") + len("write")
    while i < len(s) and s[i].isspace():
        i += 1
    if i >= len(s) or s[i] != "(":
        return None
    depth = 0
    in_s = False
    in_d = False
    j = i
    while j < len(s):
        ch = s[j]
        if ch == "'" and not in_d:
            in_s = not in_s
        elif ch == '"' and not in_s:
            in_d = not in_d
        elif not in_s and not in_d:
            if ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
                if depth == 0:
                    ctrl = s[i + 1:j].strip()
                    tail = s[j + 1:].strip()
                    if ctrl and tail:
                        return ctrl, tail
                    return None
        j += 1
    return None


def maybe_io_loop_implied_do(
    loop_var: str,
    lb: str,
    ub: str,
    step: Optional[str],
    stmt: str,
) -> Optional[str]:
    """Detect one-statement PRINT/WRITE loop bodies and build an implied-DO I/O list."""
    s = stmt.strip()
    triplet = f"{loop_var}={lb},{ub}"
    if step is not None and step.strip() and step.strip() != "1":
        triplet += f",{step.strip()}"

    m = PRINT_STMT_RE.match(s)
    if m:
        rest = m.group(1).strip()
        parts = split_top_level_commas(rest)
        if len(parts) < 2:
            return None
        fmt = parts[0].strip()
        items = [p.strip() for p in parts[1:] if p.strip()]
        if not fmt or not items:
            return None
        if not any(_has_loop_var_ref(it, loop_var) for it in items):
            return None
        return f"print {fmt}, ({', '.join(items + [triplet])})"

    pw = _parse_write_stmt(s)
    if pw is not None:
        ctrl, tail = pw
        # Preserve record-by-record semantics for external file writes.
        # `do ...; write(fp,...) ...; end do` is generally not equivalent to one
        # implied-DO WRITE record, so only rewrite stdout-style unit `*`.
        ctrl_parts = split_top_level_commas(ctrl)
        if not ctrl_parts:
            return None
        unit_part = ctrl_parts[0].strip().lower()
        unit_expr = unit_part.split("=", 1)[1].strip() if unit_part.startswith("unit=") else unit_part
        if unit_expr != "*":
            return None
        items = [p.strip() for p in split_top_level_commas(tail) if p.strip()]
        if not items:
            return None
        if not any(_has_loop_var_ref(it, loop_var) for it in items):
            return None
        return f"write ({ctrl}) ({', '.join(items + [triplet])})"

    return None


def maybe_constructor_fill_loop(
    loop_var: str,
    lb: str,
    ub: str,
    step: Optional[str],
    stmt: str,
    decl_bounds: Dict[str, str],
    alloc_map: Dict[str, str],
    alloc_rank1_bounds: Dict[str, str],
) -> Optional[str]:
    """Detect rank-1 fill loop and rewrite as array-constructor implied-DO."""
    m = ASSIGN_RE.match(stmt.strip())
    if not m:
        return None
    lhs = m.group(1).strip()
    rhs = m.group(2).strip()

    lhs_idx = INDEXED_NAME_RE.match(lhs)
    if lhs_idx is None:
        return None
    arr = lhs_idx.group(1).strip()
    idx = lhs_idx.group(2).strip().lower()
    if idx != loop_var.lower():
        return None
    if not has_loop_var(rhs, loop_var):
        return None

    # Only whole-vector fills: 1:ub with optional unit step.
    if lb.strip() != "1":
        return None
    step_txt = None if step is None else step.strip()
    if step_txt not in (None, "", "1"):
        return None

    # Ensure this loop covers whole declared/allocated vector.
    full_ok = False
    bnd = decl_bounds.get(arr.lower())
    if bnd is not None and normalize_expr(f"1:{ub}") == normalize_expr(bnd):
        full_ok = True
    if can_collapse_lhs_alloc(arr, f"1:{ub}", alloc_map):
        full_ok = True
    abnd = alloc_rank1_bounds.get(arr.lower())
    if abnd is not None and normalize_expr(f"1:{ub}") == normalize_expr(abnd):
        full_ok = True
    if not full_ok:
        return None

    return f"{arr} = [({rhs}, {loop_var}={lb},{ub})]"


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


def maybe_random_fill_then_elementwise(
    loop_var: str,
    rng: str,
    stmt_fill: str,
    stmt_elem: str,
    decl_bounds: Dict[str, str],
    alloc_map: Dict[str, str],
    array_names: Set[str],
    rank2_bounds: Dict[str, Tuple[str, str]],
    rank3_bounds: Dict[str, Tuple[str, str, str]],
) -> Optional[str]:
    """Detect two-statement loop: random fill of one element, then elementwise update."""
    s_fill = maybe_random_number_loop_1d(
        loop_var, rng, stmt_fill, decl_bounds, rank2_bounds, rank3_bounds
    )
    if s_fill is None:
        return None
    s_elem = maybe_elementwise(loop_var, rng, stmt_elem, decl_bounds, alloc_map, array_names)
    if s_elem is None:
        return None
    return f"{s_fill}\n{s_elem}"


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
    # Also handle rank-2 dummy/assumed-shape arrays not present in rank2_bounds,
    # e.g. x(:,:) dummies referenced as x(i,j).
    pat_any2 = re.compile(
        rf"\b([a-z][a-z0-9_]*)\s*\(\s*{re.escape(dim1_var)}\s*,\s*{re.escape(dim2_var)}\s*\)",
        re.IGNORECASE,
    )
    rhs_s = pat_any2.sub(
        lambda m: m.group(0)
        if m.group(1).lower() in ELEMENTAL_INTRINSICS or m.group(1).lower() in USER_ELEMENTAL_CALLS
        else f"{m.group(1)}(1:{n1}, 1:{n2})",
        rhs_s,
    )
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
    array_names = set(decl_bounds1.keys()) | set(rank2_bounds.keys()) | {lhs_name.lower()}
    # Treat call-like names in the rewritten RHS as array references unless
    # they are known intrinsics/elementals. This keeps assumed-shape dummies
    # like x(:,:) from being misclassified as function calls.
    for mm in CALL_LIKE_RE.finditer(strip_quoted_text(rhs_s.lower())):
        nm = mm.group(1).lower()
        if nm in ELEMENTAL_INTRINSICS or nm in USER_ELEMENTAL_CALLS:
            continue
        array_names.add(nm)
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
    character_rank1_array_names = collect_rank1_character_array_names(unit)
    scalar_char_lengths = collect_character_scalar_lengths(unit)
    decl_scalar_inits = parse_scalar_decl_initializers(unit)
    array_names: Set[str] = set(decl_bounds.keys()) | set(rank2_bounds.keys()) | set(rank3_bounds.keys())
    array_names |= collect_rank1_array_names(unit)
    alloc_map: Dict[str, str] = {}
    alloc_rank1_bounds: Dict[str, str] = {}
    i = 0
    while i < len(body):
        ln, stmt = body[i]
        alloc_map.update(parse_alloc_shape_spec(stmt))
        alloc_rank1_bounds.update(parse_alloc_rank1_bounds(stmt))
        norm2_stmt = maybe_norm2_stmt_rewrite(stmt)
        if norm2_stmt is not None:
            findings.append(
                Finding(
                    path=unit.path,
                    rule="norm2_intrinsic",
                    unit_kind=unit.kind,
                    unit_name=unit.name,
                    start_line=ln,
                    end_line=ln,
                    suggestion=norm2_stmt,
                )
            )
            i += 1
            continue
        blk_nested_sum = maybe_block_nested_scalar_reduction_sum(
            body, i, decl_bounds, array_names, rank2_bounds
        )
        if blk_nested_sum is not None:
            s_idx, e_idx, sugg_nsum, ln_end_nsum = blk_nested_sum
            findings.append(
                Finding(
                    path=unit.path,
                    rule="nested_reduction_sum_scalar",
                    unit_kind=unit.kind,
                    unit_name=unit.name,
                    start_line=body[s_idx][0],
                    end_line=ln_end_nsum,
                    suggestion=sugg_nsum,
                )
            )
            i = e_idx + 1
            continue
        blk_nested_prod = maybe_block_nested_scalar_reduction_product(
            body, i, decl_bounds, array_names, rank2_bounds
        )
        if blk_nested_prod is not None:
            s_idx, e_idx, sugg_nprod, ln_end_nprod = blk_nested_prod
            findings.append(
                Finding(
                    path=unit.path,
                    rule="nested_reduction_product_scalar",
                    unit_kind=unit.kind,
                    unit_name=unit.name,
                    start_line=body[s_idx][0],
                    end_line=ln_end_nprod,
                    suggestion=sugg_nprod,
                )
            )
            i = e_idx + 1
            continue
        blk_rand2 = maybe_block_random_number_rank2(body, i, rank2_bounds)
        if blk_rand2 is not None:
            end_idx_r2, sugg_r2, ln_end_r2 = blk_rand2
            findings.append(
                Finding(
                    path=unit.path,
                    rule="random_number_fill_2d_block",
                    unit_kind=unit.kind,
                    unit_name=unit.name,
                    start_line=ln,
                    end_line=ln_end_r2,
                    suggestion=sugg_r2,
                )
            )
            i = end_idx_r2 + 1
            continue
        blk_norm2 = maybe_block_norm2_from_loops(body, i, rank2_bounds)
        if blk_norm2 is not None:
            end_idx_n2, sugg_n2, ln_end_n2 = blk_norm2
            findings.append(
                Finding(
                    path=unit.path,
                    rule="nested_reduction_norm2",
                    unit_kind=unit.kind,
                    unit_name=unit.name,
                    start_line=ln,
                    end_line=ln_end_n2,
                    suggestion=sugg_n2,
                )
            )
            i = end_idx_n2 + 1
            continue
        blk_mm = maybe_block_matmul_transpose_scaled(body, i, rank2_bounds)
        if blk_mm is not None:
            end_idx_mm, sugg_mm, ln_end_mm = blk_mm
            findings.append(
                Finding(
                    path=unit.path,
                    rule="nested_matmul_mm_transpose_scaled",
                    unit_kind=unit.kind,
                    unit_name=unit.name,
                    start_line=ln,
                    end_line=ln_end_mm,
                    suggestion=sugg_mm,
                )
            )
            i = end_idx_mm + 1
            continue
        blk_sqrt = maybe_block_reduction_sqrt_sum_dim1_scaled(
            body, i, decl_bounds, rank2_bounds, array_names
        )
        if blk_sqrt is not None:
            end_idx_s, sugg_s, ln_end_s = blk_sqrt
            findings.append(
                Finding(
                    path=unit.path,
                    rule="nested_reduction_sqrt_sum_dim1_scaled",
                    unit_kind=unit.kind,
                    unit_name=unit.name,
                    start_line=ln,
                    end_line=ln_end_s,
                    suggestion=sugg_s,
                )
            )
            i = end_idx_s + 1
            continue
        blk_sqrt_in = maybe_block_reduction_sqrt_inside_outer_dim1(
            body, i, decl_bounds, rank2_bounds, array_names
        )
        if blk_sqrt_in is not None:
            end_idx_si, sugg_si, ln_end_si = blk_sqrt_in
            findings.append(
                Finding(
                    path=unit.path,
                    rule="nested_reduction_sqrt_sum_dim1_scaled",
                    unit_kind=unit.kind,
                    unit_name=unit.name,
                    start_line=ln,
                    end_line=ln_end_si,
                    suggestion=sugg_si,
                )
            )
            i = end_idx_si + 1
            continue
        blk = maybe_block_reduction_sum_dim1_scaled(
            body, i, decl_bounds, rank2_bounds, array_names
        )
        if blk is not None:
            end_idx, sugg_blk, ln_end_blk = blk
            findings.append(
                Finding(
                    path=unit.path,
                    rule="nested_reduction_sum_dim1_scaled",
                    unit_kind=unit.kind,
                    unit_name=unit.name,
                    start_line=ln,
                    end_line=ln_end_blk,
                    suggestion=sugg_blk,
                )
            )
            i = end_idx + 1
            continue
        blk2 = maybe_block_reduction_sum_dim2_scaled(
            body, i, decl_bounds, rank2_bounds, array_names
        )
        if blk2 is not None:
            end_idx2, sugg_blk2, ln_end_blk2 = blk2
            findings.append(
                Finding(
                    path=unit.path,
                    rule="nested_reduction_sum_dim2_scaled",
                    unit_kind=unit.kind,
                    unit_name=unit.name,
                    start_line=ln,
                    end_line=ln_end_blk2,
                    suggestion=sugg_blk2,
                )
            )
            i = end_idx2 + 1
            continue
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
        # Form A-1b: pre-init + nested matmul matrix-vector:
        # y(1:m)=0
        # do i=...
        #    do j=...
        #       y(i)=y(i)+A(i,j)*x(j)
        #    end do
        # end do
        if i + 5 < len(body):
            _ln_odo, stmt_odo = body[i + 1]
            _ln_ido, stmt_ido = body[i + 2]
            _ln_ibody, stmt_ibody = body[i + 3]
            ln_iend, stmt_iend = body[i + 4]
            ln_oend, stmt_oend = body[i + 5]
            if (
                DO_RE.match(stmt_odo.strip())
                and DO_RE.match(stmt_ido.strip())
                and END_DO_RE.match(stmt_iend.strip())
                and END_DO_RE.match(stmt_oend.strip())
            ):
                sugg_mv_pre = maybe_matmul_mv_preinit_nested(
                    stmt,
                    stmt_odo,
                    stmt_ido,
                    stmt_ibody,
                    decl_bounds,
                    rank2_bounds,
                )
                if sugg_mv_pre is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="nested_matmul_mv",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=ln,
                            end_line=ln_oend,
                            suggestion=sugg_mv_pre,
                        )
                    )
                    i += 6
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
            character_rank1_array_names,
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
            character_rank1_array_names,
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
        # Form P0: guarded init + subsequent min/max fold loop:
        # if (cond) then
        #   xmin = x(1)
        #   xmax = x(1)
        # end if
        # do i=2,ub
        #   xmin = min(xmin, x(i))
        #   xmax = max(xmax, x(i))
        # end do
        if i + 7 < len(body):
            _ln_i1, stmt_i1 = body[i + 1]
            _ln_i2, stmt_i2 = body[i + 2]
            _ln_eif, stmt_eif = body[i + 3]
            _ln_do, stmt_do = body[i + 4]
            _ln_f1, stmt_f1 = body[i + 5]
            _ln_f2, stmt_f2 = body[i + 6]
            ln_end, stmt_end = body[i + 7]
            if (
                IF_THEN_RE.match(stmt.strip())
                and END_IF_RE.match(stmt_eif.strip())
                and DO_RE.match(stmt_do.strip())
                and END_DO_RE.match(stmt_end.strip())
            ):
                sugg_pre = maybe_preinit_extreme_pair_then_loop(
                    stmt, stmt_i1, stmt_i2, stmt_do, stmt_f1, stmt_f2, decl_bounds
                )
                if sugg_pre is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="reduction_minmax_value_pair",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=ln,
                            end_line=ln_end,
                            suggestion=sugg_pre,
                        )
                    )
                    i += 8
                    continue
        # Form P1: guarded init + fold loop with IF-block updates:
        # if (cond) then
        #   xmin = x(1)
        #   xmax = x(1)
        # end if
        # do i=2,ub
        #   if (...) then
        #      xmin = x(i)
        #   end if
        #   if (...) then
        #      xmax = x(i)
        #   end if
        # end do
        if i + 11 < len(body):
            _ln_i1, stmt_i1 = body[i + 1]
            _ln_i2, stmt_i2 = body[i + 2]
            _ln_eif, stmt_eif = body[i + 3]
            _ln_do, stmt_do = body[i + 4]
            _ln_if1, stmt_if1 = body[i + 5]
            _ln_as1, stmt_as1 = body[i + 6]
            _ln_eif1, stmt_eif1 = body[i + 7]
            _ln_if2, stmt_if2 = body[i + 8]
            _ln_as2, stmt_as2 = body[i + 9]
            _ln_eif2, stmt_eif2 = body[i + 10]
            ln_end, stmt_end = body[i + 11]
            if (
                IF_THEN_RE.match(stmt.strip())
                and END_IF_RE.match(stmt_eif.strip())
                and DO_RE.match(stmt_do.strip())
                and IF_THEN_RE.match(stmt_if1.strip())
                and IF_THEN_RE.match(stmt_if2.strip())
                and END_IF_RE.match(stmt_eif1.strip())
                and END_IF_RE.match(stmt_eif2.strip())
                and END_DO_RE.match(stmt_end.strip())
            ):
                sugg_pre_blk = maybe_preinit_extreme_pair_then_loop_blocks(
                    stmt,
                    stmt_i1,
                    stmt_i2,
                    stmt_do,
                    stmt_if1,
                    stmt_as1,
                    stmt_eif1,
                    stmt_if2,
                    stmt_as2,
                    stmt_eif2,
                    decl_bounds,
                )
                if sugg_pre_blk is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="reduction_minmax_value_pair",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=ln,
                            end_line=ln_end,
                            suggestion=sugg_pre_blk,
                        )
                    )
                    i += 12
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
        if i + 5 < len(body):
            _ln_tmp, stmt_tmp = body[i + 1]
            _ln_ido, stmt_ido = body[i + 2]
            _ln_ibody, stmt_ibody = body[i + 3]
            ln_iend, stmt_iend = body[i + 4]
            ln_oend, stmt_oend = body[i + 5]
            if END_DO_RE.match(stmt_iend.strip()) and END_DO_RE.match(stmt_oend.strip()):
                sugg_nreshape = maybe_nested_reshape_from_vector_fill(
                    loop_var, lb, ub, step, stmt_tmp, stmt_ido, stmt_ibody, rank2_bounds
                )
                if sugg_nreshape is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="nested_reshape_from_vector_fill",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=ln,
                            end_line=ln_oend,
                            suggestion=sugg_nreshape,
                        )
                    )
                    i += 6
                    continue

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
        # Form A2N: nested 2D loop with optional outer-prefix elementwise statements,
        # then inner multi-assignment elementwise body.
        # do i=...
        #   p(i)=...
        #   do j=...
        #      a(i,j)=...
        #      b(i,j)=...
        #   end do
        # end do
        if i + 5 < len(body):
            k = i + 1
            prefix_stmts: List[str] = []
            blocked_prefix = False
            while k < len(body):
                st = body[k][1].strip()
                if DO_RE.match(st):
                    break
                if (
                    not st
                    or IF_THEN_RE.match(st)
                    or ELSE_RE.match(st)
                    or END_IF_RE.match(st)
                    or CYCLE_RE.match(st)
                    or EXIT_RE.match(st)
                    or RETURN_RE.match(st)
                    or SELECT_CASE_RE.match(st)
                    or END_DO_RE.match(st)
                ):
                    blocked_prefix = True
                    break
                prefix_stmts.append(body[k][1])
                k += 1
            if blocked_prefix or k >= len(body):
                m_ido = None
            else:
                _ln_ido, stmt_ido = body[k]
                m_ido = DO_RE.match(stmt_ido.strip())
            if m_ido is not None:
                j = k + 1
                inner_stmts: List[str] = []
                inner_end = -1
                blocked = False
                while j < len(body):
                    _ln_j, stmt_j = body[j]
                    st = stmt_j.strip()
                    if DO_RE.match(st):
                        blocked = True
                        break
                    if END_DO_RE.match(st):
                        inner_end = j
                        break
                    if (
                        IF_THEN_RE.match(st)
                        or ELSE_RE.match(st)
                        or END_IF_RE.match(st)
                        or CYCLE_RE.match(st)
                        or EXIT_RE.match(st)
                        or RETURN_RE.match(st)
                        or SELECT_CASE_RE.match(st)
                    ):
                        blocked = True
                        break
                    if st:
                        inner_stmts.append(stmt_j)
                    j += 1
                if not blocked and inner_end > i + 2 and inner_end + 1 < len(body):
                    ln_oend, stmt_oend = body[inner_end + 1]
                    if END_DO_RE.match(stmt_oend.strip()):
                        prefix_suggs: List[str] = []
                        ok_prefix = True
                        for pst in prefix_stmts:
                            ps = maybe_elementwise(loop_var, rng, pst, decl_bounds, alloc_map, array_names)
                            if ps is None:
                                ok_prefix = False
                                break
                            ps = simplify_section_expr_rank2_partial(ps, rank2_bounds)
                            prefix_suggs.append(ps)
                        if not ok_prefix:
                            i += 1
                            continue
                        sugg_nmulti = maybe_nested_multi_elementwise(
                            loop_var,
                            rng,
                            stmt_ido,
                            inner_stmts,
                            decl_bounds,
                            rank2_bounds,
                            alloc_map,
                            array_names,
                        )
                        if sugg_nmulti is not None:
                            sug_all = "\n".join(prefix_suggs + [sugg_nmulti]) if prefix_suggs else sugg_nmulti
                            findings.append(
                                Finding(
                                    path=unit.path,
                                    rule="elementwise_multi_nested",
                                    unit_kind=unit.kind,
                                    unit_name=unit.name,
                                    start_line=ln,
                                    end_line=ln_oend,
                                    suggestion=sug_all,
                                )
                            )
                            i = inner_end + 2
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
                sugg_io = maybe_io_loop_implied_do(loop_var, lb, ub, step, stmt_body)
                if sugg_io is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="io_loop_implied_do",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=ln,
                            end_line=ln_end,
                            suggestion=sugg_io,
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
                sugg_ecall = maybe_elemental_subroutine_call(
                    loop_var, rng, stmt_body, decl_bounds, array_names
                )
                if sugg_ecall is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="elemental_subroutine_call",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=ln,
                            end_line=ln_end,
                            suggestion=sugg_ecall,
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
                sugg_msum = maybe_matmul_mv_sum_loop(
                    loop_var, rng, stmt_body, decl_bounds, rank2_bounds
                )
                if sugg_msum is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="matmul_mv_sum_loop",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=ln,
                            end_line=ln_end,
                            suggestion=sugg_msum,
                        )
                    )
                    i += 3
                    continue
                sugg_ctor = maybe_constructor_fill_loop(
                    loop_var, lb, ub, step, stmt_body, decl_bounds, alloc_map, alloc_rank1_bounds
                )
                if sugg_ctor is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="constructor_fill_loop",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=ln,
                            end_line=ln_end,
                            suggestion=sugg_ctor,
                        )
                    )
                    i += 3
                    continue
                prev_stmt = body[i - 1][1] if i - 1 >= 0 else None
                start_line = body[i - 1][0] if i - 1 >= 0 else ln
                sugg_sum = maybe_reduction_sum(loop_var, rng, stmt_body, prev_stmt, decl_bounds, array_names)
                # Also handle nearby init when unrelated statements sit between
                # accumulator initialization and the reduction loop, as long as
                # the accumulator is not referenced in between.
                if sugg_sum is None and i - 2 >= 0:
                    acc_nm = reduction_sum_acc_name(stmt_body)
                    if acc_nm is not None:
                        for back in range(2, min(8, i + 1)):
                            cand_idx = i - back
                            cand_stmt = body[cand_idx][1]
                            mp2 = ASSIGN_RE.match(cand_stmt.strip())
                            ok_prev2 = False
                            if mp2 is not None:
                                lhs2 = SIMPLE_NAME_RE.match(mp2.group(1).strip())
                                rhs2 = mp2.group(2).strip()
                                ok_prev2 = (
                                    lhs2 is not None
                                    and lhs2.group(1).lower() == acc_nm
                                    and ZERO_LITERAL_RE.match(rhs2) is not None
                                )
                            if not ok_prev2:
                                continue
                            safe_between = True
                            for mid in range(cand_idx + 1, i):
                                if re.search(rf"\b{re.escape(acc_nm)}\b", body[mid][1], re.IGNORECASE):
                                    safe_between = False
                                    break
                            if not safe_between:
                                continue
                            sugg_sum2 = maybe_reduction_sum(loop_var, rng, stmt_body, cand_stmt, decl_bounds, array_names)
                            if sugg_sum2 is not None:
                                sugg_sum = sugg_sum2
                                start_line = ln
                                break
                if sugg_sum is not None:
                    sugg_sum = simplify_sum_section_with_size_alias(sugg_sum, body, i)
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
                    loop_var, rng, prev_stmt, stmt_body, decl_bounds, array_names, decl_scalar_inits
                )
                if sugg_msum_inline is not None:
                    acc_name = sugg_msum_inline.split("=", 1)[0].strip().lower()
                    start_line_msum = body[i - 1][0] if prev_stmt_assigns_name(prev_stmt, acc_name) and i - 1 >= 0 else ln
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="reduction_masked_sum",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=start_line_msum,
                            end_line=ln_end,
                            suggestion=sugg_msum_inline,
                        )
                    )
                    i += 3
                    continue
                acc_inline = masked_sum_acc_from_inline_if(stmt_body)
                if acc_inline is not None and not prev_stmt_assigns_name(prev_stmt, acc_inline) and acc_inline not in decl_scalar_inits:
                    fake_inits = dict(decl_scalar_inits)
                    fake_inits[acc_inline] = "0.0"
                    if (
                        maybe_masked_sum_inline(
                            loop_var, rng, prev_stmt, stmt_body, decl_bounds, array_names, fake_inits
                        )
                        is not None
                    ):
                        findings.append(
                            Finding(
                                path=unit.path,
                                rule="warning_uninitialized_masked_sum",
                                unit_kind=unit.kind,
                                unit_name=unit.name,
                                start_line=ln,
                                end_line=ln,
                                suggestion=f"! xarray warning: skipped masked-sum rewrite because '{acc_inline}' has no detected initialization",
                                insert_only=True,
                            )
                        )
                sugg_mprod_inline = maybe_masked_product_inline(
                    loop_var, rng, prev_stmt, stmt_body, decl_bounds, array_names
                )
                if sugg_mprod_inline is not None:
                    acc_name = sugg_mprod_inline.split("=", 1)[0].strip().lower()
                    start_line_mprod = body[i - 1][0] if prev_stmt_assigns_name(prev_stmt, acc_name) and i - 1 >= 0 else ln
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="reduction_masked_product",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=start_line_mprod,
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
                sugg_reshape = maybe_reshape_from_vector_fill(
                    loop_var,
                    lb,
                    ub,
                    step,
                    stmt_s1,
                    stmt_s2,
                    rank2_bounds,
                )
                if sugg_reshape is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="reshape_from_vector_fill",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=ln,
                            end_line=ln_end,
                            suggestion=sugg_reshape,
                        )
                    )
                    i += 4
                    continue
                sugg_rand_elem = maybe_random_fill_then_elementwise(
                    loop_var,
                    rng,
                    stmt_s1,
                    stmt_s2,
                    decl_bounds,
                    alloc_map,
                    array_names,
                    rank2_bounds,
                    rank3_bounds,
                )
                if sugg_rand_elem is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="random_number_fill_elementwise",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=ln,
                            end_line=ln_end,
                            suggestion=sugg_rand_elem,
                        )
                    )
                    i += 4
                    continue
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
                sugg_nested_prod = maybe_nested_inner_product(loop_var, stmt_init, stmt_ido, stmt_ibody, decl_bounds)
                if sugg_nested_prod is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="nested_reduction_product",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=_ln_init,
                            end_line=ln_iend,
                            suggestion=sugg_nested_prod,
                        )
                    )
                    i += 6
                    continue

        # Form A3b: scalar nested reduction with init before outer loop:
        # acc = 0
        # do j=...
        #   do i=...
        #     acc = acc + expr(i,j)
        #   end do
        # end do
        if i + 4 < len(body):
            _ln_ido, stmt_ido = body[i + 1]
            _ln_ibody, stmt_ibody = body[i + 2]
            ln_iend, stmt_iend = body[i + 3]
            ln_oend, stmt_oend = body[i + 4]
            if END_DO_RE.match(stmt_iend.strip()) and END_DO_RE.match(stmt_oend.strip()):
                prev_stmt = body[i - 1][1] if i - 1 >= 0 else None
                start_line = body[i - 1][0] if i - 1 >= 0 else ln
                sugg_nested_scalar = maybe_nested_scalar_reduction_sum(
                    loop_var, rng, stmt_ido, stmt_ibody, prev_stmt, decl_bounds, array_names, rank2_bounds
                )
                if sugg_nested_scalar is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="nested_reduction_sum_scalar",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=start_line,
                            end_line=ln_oend,
                            suggestion=sugg_nested_scalar,
                        )
                    )
                    i += 5
                    continue
                sugg_nested_scalar_prod = maybe_nested_scalar_reduction_product(
                    loop_var, rng, stmt_ido, stmt_ibody, prev_stmt, decl_bounds, array_names, rank2_bounds
                )
                if sugg_nested_scalar_prod is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="nested_reduction_product_scalar",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=start_line,
                            end_line=ln_oend,
                            suggestion=sugg_nested_scalar_prod,
                        )
                    )
                    i += 5
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
                prev_stmt = body[i - 1][1] if i - 1 >= 0 else None
                sugg_sum_merge = maybe_sum_merge_ifelse(
                    loop_var,
                    rng,
                    prev_stmt,
                    stmt_if,
                    stmt_t,
                    stmt_f,
                    decl_bounds,
                    array_names,
                    decl_scalar_inits,
                )
                if sugg_sum_merge is not None:
                    acc_name = sugg_sum_merge.split("=", 1)[0].strip().lower()
                    start_line_acc = body[i - 1][0] if prev_stmt_assigns_name(prev_stmt, acc_name) and i - 1 >= 0 else ln
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="reduction_sum_merge",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=start_line_acc,
                            end_line=ln_end,
                            suggestion=sugg_sum_merge,
                        )
                    )
                    i += 7
                    continue
                sugg_ext_ifelse = maybe_extreme_init_else_fold(
                    loop_var, rng, stmt_if, stmt_t, stmt_f, decl_bounds
                )
                if sugg_ext_ifelse is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="reduction_minmax_value",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=ln,
                            end_line=ln_end,
                            suggestion=sugg_ext_ifelse,
                        )
                    )
                    i += 7
                    continue
        # Form D2: IF/ELSE block with paired min/max updates:
        # do ...
        #   if (...) then
        #      smin = a(i)
        #      smax = a(i)
        #   else
        #      smin = min(smin,a(i))
        #      smax = max(smax,a(i))
        #   end if
        # end do
        if i + 8 < len(body):
            _ln_if, stmt_if = body[i + 1]
            _ln_t1, stmt_t1 = body[i + 2]
            _ln_t2, stmt_t2 = body[i + 3]
            _ln_else, stmt_else = body[i + 4]
            _ln_f1, stmt_f1 = body[i + 5]
            _ln_f2, stmt_f2 = body[i + 6]
            _ln_eif, stmt_eif = body[i + 7]
            ln_end, stmt_end = body[i + 8]
            if (
                IF_THEN_RE.match(stmt_if.strip())
                and ELSE_RE.match(stmt_else.strip())
                and END_IF_RE.match(stmt_eif.strip())
                and END_DO_RE.match(stmt_end.strip())
            ):
                sugg_pair = maybe_extreme_init_else_fold_pair(
                    loop_var, rng, stmt_if, stmt_t1, stmt_t2, stmt_f1, stmt_f2, decl_bounds
                )
                if sugg_pair is not None:
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="reduction_minmax_value_pair",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=ln,
                            end_line=ln_end,
                            suggestion=sugg_pair,
                        )
                    )
                    i += 9
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
                sugg_msum = maybe_masked_sum(
                    loop_var, rng, prev_stmt, stmt_if, stmt_inside, decl_bounds, array_names, decl_scalar_inits
                )
                if sugg_msum is not None:
                    acc_name = sugg_msum.split("=", 1)[0].strip().lower()
                    start_line_msum = body[i - 1][0] if prev_stmt_assigns_name(prev_stmt, acc_name) and i - 1 >= 0 else ln
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="reduction_masked_sum",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=start_line_msum,
                            end_line=ln_end,
                            suggestion=sugg_msum,
                        )
                    )
                    i += 5
                    continue
                acc_block = masked_sum_acc_from_block_stmt(stmt_if, stmt_inside)
                if acc_block is not None and not prev_stmt_assigns_name(prev_stmt, acc_block) and acc_block not in decl_scalar_inits:
                    fake_inits = dict(decl_scalar_inits)
                    fake_inits[acc_block] = "0.0"
                    if (
                        maybe_masked_sum(
                            loop_var, rng, prev_stmt, stmt_if, stmt_inside, decl_bounds, array_names, fake_inits
                        )
                        is not None
                    ):
                        findings.append(
                            Finding(
                                path=unit.path,
                                rule="warning_uninitialized_masked_sum",
                                unit_kind=unit.kind,
                                unit_name=unit.name,
                                start_line=ln,
                                end_line=ln,
                                suggestion=f"! xarray warning: skipped masked-sum rewrite because '{acc_block}' has no detected initialization",
                                insert_only=True,
                            )
                        )
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
                    acc_name = sugg_mprod.split("=", 1)[0].strip().lower()
                    start_line_mprod = body[i - 1][0] if prev_stmt_assigns_name(prev_stmt, acc_name) and i - 1 >= 0 else ln
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="reduction_masked_product",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=start_line_mprod,
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
                recent_inits = collect_recent_scalar_assignments(body, i)
                sugg_msp = maybe_masked_sum_product_block(
                    loop_var,
                    rng,
                    stmt_if,
                    stmt_a,
                    stmt_b,
                    decl_bounds,
                    array_names,
                    recent_inits,
                    decl_scalar_inits,
                )
                if sugg_msp is not None:
                    acc_names = set()
                    for sl in sugg_msp.splitlines():
                        if "=" in sl:
                            acc_names.add(sl.split("=", 1)[0].strip().lower())
                    sline = init_block_start_line_for_accs(body, i, acc_names) or ln
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule="reduction_masked_sum_product",
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=sline,
                            end_line=ln_end,
                            suggestion=sugg_msp,
                        )
                    )
                    i += 6
                    continue
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
        # Form AM: multi-statement elementwise loop body:
        # do ...
        #    a(i)=...
        #    b(i)=...
        # end do
        # Conservative: no nested control flow; only top-level assignments.
        depth = 1
        j = i + 1
        inner_stmts: List[str] = []
        has_nested = False
        end_idx = -1
        while j < len(body):
            _ln_j, stmt_j = body[j]
            st = stmt_j.strip()
            if DO_RE.match(st):
                has_nested = True
                break
            if END_DO_RE.match(st):
                depth -= 1
                if depth == 0:
                    end_idx = j
                    break
            else:
                if (
                    IF_THEN_RE.match(st)
                    or ELSE_RE.match(st)
                    or END_IF_RE.match(st)
                    or CYCLE_RE.match(st)
                    or EXIT_RE.match(st)
                    or RETURN_RE.match(st)
                    or SELECT_CASE_RE.match(st)
                ):
                    has_nested = True
                    break
                if st:
                    inner_stmts.append(stmt_j)
            j += 1
        if not has_nested and end_idx > i + 2:
            ln_end = body[end_idx][0]
            sugg_unroll = maybe_unrolled_elementwise_chunked(
                loop_var,
                lb.strip(),
                ub.strip(),
                step,
                inner_stmts,
                array_names,
                aggressive=AGGRESSIVE_MODE,
            )
            if sugg_unroll is not None:
                findings.append(
                    Finding(
                        path=unit.path,
                        rule="elementwise_unrolled_chunk",
                        unit_kind=unit.kind,
                        unit_name=unit.name,
                        start_line=ln,
                        end_line=ln_end,
                        suggestion=sugg_unroll,
                    )
                )
                i = end_idx + 1
                continue
            sugg_temp_multi = maybe_temp_then_multi_elementwise(
                loop_var, rng, inner_stmts, decl_bounds, rank2_bounds, decl_scalar_inits, alloc_map, array_names
            )
            if sugg_temp_multi is not None:
                findings.append(
                    Finding(
                        path=unit.path,
                        rule="elementwise_inline_temp_multi",
                        unit_kind=unit.kind,
                        unit_name=unit.name,
                        start_line=ln,
                        end_line=ln_end,
                        suggestion=sugg_temp_multi,
                    )
                )
                i = end_idx + 1
                continue
            sugg_multi = maybe_multi_elementwise(
                loop_var, rng, inner_stmts, decl_bounds, alloc_map, array_names
            )
            if sugg_multi is not None:
                findings.append(
                    Finding(
                        path=unit.path,
                        rule="elementwise_multi",
                        unit_kind=unit.kind,
                        unit_name=unit.name,
                        start_line=ln,
                        end_line=ln_end,
                        suggestion=sugg_multi,
                    )
                )
                i = end_idx + 1
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
        if not do_triplet_fields_well_formed(mdo.group(2), mdo.group(3), mdo.group(4)):
            i += 1
            continue
        # 3D nested form:
        # do o
        #   do m
        #     do i
        #       stmt
        #     end do
        #   end do
        # end do
        if i + 6 < len(body):
            ln_mdo, stmt_mdo = body[i + 1]
            ln_ido, stmt_ido = body[i + 2]
            _ln_body, stmt_body3 = body[i + 3]
            ln_iend, stmt_iend = body[i + 4]
            ln_mend, stmt_mend = body[i + 5]
            ln_oend, stmt_oend = body[i + 6]
            m_mdo = DO_RE.match(stmt_mdo.strip())
            m_ido = DO_RE.match(stmt_ido.strip())
            if (
                m_mdo
                and m_ido
                and END_DO_RE.match(stmt_iend.strip())
                and END_DO_RE.match(stmt_mend.strip())
                and END_DO_RE.match(stmt_oend.strip())
            ):
                if not do_triplet_fields_well_formed(m_mdo.group(2), m_mdo.group(3), m_mdo.group(4)):
                    i += 1
                    continue
                if not do_triplet_fields_well_formed(m_ido.group(2), m_ido.group(3), m_ido.group(4)):
                    i += 1
                    continue
                loop_var_o = mdo.group(1)
                loop_var_m = m_mdo.group(1)
                loop_var_i = m_ido.group(1)
                rng_o = build_range(mdo.group(2), mdo.group(3), mdo.group(4))
                rng_m = build_range(m_mdo.group(2), m_mdo.group(3), m_mdo.group(4))
                rng_i = build_range(m_ido.group(2), m_ido.group(3), m_ido.group(4))
                sug3 = maybe_loopnest_to_concurrent_or_forall(
                    [loop_var_o, loop_var_m, loop_var_i],
                    [rng_o, rng_m, rng_i],
                    stmt_body3,
                    allow_concurrent=allow_concurrent,
                    allow_forall=allow_forall,
                    array_names=array_names,
                )
                if sug3 is not None:
                    suggestion, rule = sug3
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule=rule,
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=ln,
                            end_line=ln_oend,
                            suggestion=suggestion,
                        )
                    )
                    i += 7
                    continue
        # 2D nested form:
        # do o
        #   do i
        #     stmt
        #   end do
        # end do
        if i + 4 < len(body):
            ln_ido, stmt_ido = body[i + 1]
            _ln_body, stmt_body2 = body[i + 2]
            ln_iend, stmt_iend = body[i + 3]
            ln_oend, stmt_oend = body[i + 4]
            m_ido = DO_RE.match(stmt_ido.strip())
            if m_ido and END_DO_RE.match(stmt_iend.strip()) and END_DO_RE.match(stmt_oend.strip()):
                if not do_triplet_fields_well_formed(m_ido.group(2), m_ido.group(3), m_ido.group(4)):
                    i += 1
                    continue
                loop_var_o = mdo.group(1)
                loop_var_i = m_ido.group(1)
                rng_o = build_range(mdo.group(2), mdo.group(3), mdo.group(4))
                rng_i = build_range(m_ido.group(2), m_ido.group(3), m_ido.group(4))
                sug2d = maybe_loopnest_to_concurrent_or_forall(
                    [loop_var_o, loop_var_i],
                    [rng_o, rng_i],
                    stmt_body2,
                    allow_concurrent=allow_concurrent,
                    allow_forall=allow_forall,
                    array_names=array_names,
                )
                if sug2d is not None:
                    suggestion, rule = sug2d
                    findings.append(
                        Finding(
                            path=unit.path,
                            rule=rule,
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            start_line=ln,
                            end_line=ln_oend,
                            suggestion=suggestion,
                        )
                    )
                    i += 5
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
    global USER_ELEMENTAL_CALLS, USER_ELEMENTAL_SUBROUTINES
    USER_ELEMENTAL_CALLS = {
        p.name.lower()
        for p in finfo.procedures
        if p.kind == "function" and "elemental" in p.attrs
    }
    USER_ELEMENTAL_SUBROUTINES = {
        p.name.lower()
        for p in finfo.procedures
        if p.kind == "subroutine" and "elemental" in p.attrs
    }
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
            low = fscan.strip_comment(stmt).strip().lower()
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
    lines = read_text_flexible(path).splitlines(keepends=True)
    inserted = 0
    for f in sorted(findings, key=lambda x: (x.start_line, x.end_line), reverse=True):
        if f.insert_only:
            continue
        sidx = f.start_line - 1
        eidx = f.end_line - 1
        if sidx < 0 or eidx < 0 or sidx >= len(lines) or eidx >= len(lines) or sidx > eidx:
            continue
        # Also include physical continuation lines of this statement.
        while eidx + 1 < len(lines) and (
            has_trailing_continuation_amp(lines[eidx]) or is_continuation_only_line(lines[eidx + 1])
        ):
            eidx += 1
        raw_s = lines[sidx]
        raw_e = lines[eidx]
        indent_s = re.match(r"^\s*", raw_s).group(0) if raw_s else ""
        indent_e = re.match(r"^\s*", raw_e).group(0) if raw_e else ""
        eol_s = "\r\n" if raw_s.endswith("\r\n") else ("\n" if raw_s.endswith("\n") else "\n")
        eol_e = "\r\n" if raw_e.endswith("\r\n") else ("\n" if raw_e.endswith("\n") else "\n")

        begin_msg = f"{indent_s}{BEGIN_TAG}{eol_s}"
        end_msg = f"{indent_e}{END_TAG}{eol_e}"
        sug_lines = _nonblank_suggestion_lines(f.suggestion)
        if not sug_lines:
            continue
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
    annotate_removed: bool = False,
    out_path: Optional[Path] = None,
    create_backup: bool = True,
    add_trace: bool = True,
) -> Tuple[int, Optional[Path], List[str]]:
    """Replace suggested blocks with array-operation statements and prune unused locals."""
    if not findings:
        return 0, None, []
    lines = read_text_flexible(path).splitlines(keepends=True)
    changed = 0
    replaced = 0
    for f in sorted(findings, key=lambda x: (x.start_line, x.end_line), reverse=True):
        sidx = f.start_line - 1
        eidx = f.end_line - 1
        if sidx < 0 or eidx < 0 or sidx >= len(lines) or eidx >= len(lines) or sidx > eidx:
            continue
        if f.insert_only:
            raw_s = lines[sidx]
            eol = "\r\n" if raw_s.endswith("\r\n") else ("\n" if raw_s.endswith("\n") else "\n")
            indent = re.match(r"^\s*", raw_s).group(0) if raw_s else ""
            warn_line = f"{indent}{f.suggestion}{eol}"
            if sidx > 0 and lines[sidx - 1].strip() == f.suggestion.strip():
                continue
            lines.insert(sidx, warn_line)
            changed += 1
            continue
        # Also include physical continuation lines of this statement.
        while eidx + 1 < len(lines) and (
            has_trailing_continuation_amp(lines[eidx]) or is_continuation_only_line(lines[eidx + 1])
        ):
            eidx += 1
        raw_s = lines[sidx]
        eol = "\r\n" if raw_s.endswith("\r\n") else ("\n" if raw_s.endswith("\n") else "\n")
        indent = re.match(r"^\s*", raw_s).group(0) if raw_s else ""
        removed_lines: List[str] = []
        if annotate_removed and not _is_redundant_removed_block(lines[sidx : eidx + 1], f.suggestion):
            removed_lines.append(f"{indent}{REMOVED_BLOCK_BEGIN}{eol}")
            for old_raw in lines[sidx : eidx + 1]:
                old_body = old_raw.rstrip("\r\n")
                removed_lines.append(f"{indent}! {old_body}{eol}")
            removed_lines.append(f"{indent}{REMOVED_BLOCK_END}{eol}")
        sug_lines = _nonblank_suggestion_lines(f.suggestion)
        if not sug_lines:
            continue
        repl_lines: List[str] = []
        if annotate and len(sug_lines) > 1:
            repl_lines.append(f"{indent}{CHANGED_BLOCK_BEGIN}{eol}")
            for sl in sug_lines:
                repl_lines.append(f"{indent}{sl}{eol}")
            repl_lines.append(f"{indent}{CHANGED_BLOCK_END}{eol}")
        else:
            for j, sl in enumerate(sug_lines):
                suffix = f"  {CHANGED_TAG}" if (annotate and j == len(sug_lines) - 1) else ""
                repl_lines.append(f"{indent}{sl}{suffix}{eol}")
        lines[sidx : eidx + 1] = [*removed_lines, *repl_lines]
        changed += 1
        replaced += 1
    if changed == 0:
        return 0, None, []
    removed_locals: List[str] = []
    if replaced > 0:
        lines, removed_locals = remove_unused_locals_from_lines(lines, path)
        lines = _prune_unused_generated_block_decls(lines)
        lines = _remove_redundant_extreme_seed_assign(lines)
        lines = _unwrap_redundant_blocks(lines)
    # Remove stale xarray annotation artifacts from prior annotate runs so
    # repeated fix/annotate cycles stay readable and idempotent.
    cleaned: List[str] = []
    for raw in lines:
        low = raw.lower()
        if BEGIN_TAG.lower() in low or END_TAG.lower() in low:
            continue
        if "suggested replacement by xarray.py" in low:
            continue
        cleaned.append(raw)
    lines = cleaned
    # Apply lightweight structural cleanup before writing so key rewrites
    # (e.g., random_number block collapse) are present even without later passes.
    lines = _collapse_random_number_blocks(lines)
    lines = _collapse_xto_loop_print_sum_blocks(lines)
    lines = _collapse_xto_loop_print_minmax_block(lines)
    lines = _collapse_random_seed_put_temp_blocks(lines)
    lines = _collapse_seeded_minmax_print_blocks(lines)
    lines = _rewrite_norm2_lines(lines)
    lines = _strip_foreign_tool_comment_lines(lines)
    backup: Optional[Path] = None
    target = out_path if out_path is not None else path
    # With explicit output path, do not create input backups.
    if out_path is None and create_backup:
        backup = make_backup_path(path)
        shutil.copy2(path, backup)
    out_text = "".join(lines)
    if add_trace:
        out_text = with_trace_header(out_text, tool_name="xarray.py", source_name=path.name)
    target.write_text(out_text, encoding="utf-8")
    return changed, backup, removed_locals


def _normalize_stmt_for_removed_compare(stmt: str) -> str:
    """Canonicalize a statement for removed-vs-changed redundancy checks."""
    code = split_code_comment(stmt)[0].strip().lower()
    # Treat explicit full slices like a(1:n) as equivalent to whole-array a.
    code = re.sub(
        r"\b([a-z][a-z0-9_]*)\s*\(\s*1\s*:\s*[a-z][a-z0-9_]*\s*\)",
        r"\1",
        code,
        flags=re.IGNORECASE,
    )
    # Ignore formatting whitespace.
    return re.sub(r"\s+", "", code)


def _is_redundant_removed_block(old_lines: List[str], suggestion: str) -> bool:
    """
    True when removed block is effectively identical to replacement suggestion.

    This avoids noisy annotate-removed output such as:
      old: x(1:n) = ...
      new: x = ...
    """
    if len(old_lines) != 1:
        return False
    sug_lines = suggestion.splitlines() if "\n" in suggestion else [suggestion]
    if len(sug_lines) != 1:
        return False
    old_norm = _normalize_stmt_for_removed_compare(old_lines[0])
    new_norm = _normalize_stmt_for_removed_compare(sug_lines[0])
    return bool(old_norm) and old_norm == new_norm


def _nonblank_suggestion_lines(suggestion: str) -> List[str]:
    """Split suggestion text and drop blank lines."""
    lines = suggestion.splitlines() if "\n" in suggestion else [suggestion]
    return [ln for ln in lines if ln.strip()]


def _is_generated_loop_var(name: str) -> bool:
    n = name.strip().lower()
    return bool(re.fullmatch(r"[ijklm]_*$", n))


def _prune_unused_generated_block_decls(lines: List[str]) -> List[str]:
    """Remove unused generated loop vars from declarations inside BLOCK scopes."""
    out = list(lines)
    stack: List[int] = []
    pairs: List[Tuple[int, int]] = []
    for i, ln in enumerate(out):
        code = split_code_comment(ln.rstrip("\r\n"))[0].strip()
        if re.match(r"^block\b", code, re.IGNORECASE):
            stack.append(i)
        elif re.match(r"^end\s+block\b", code, re.IGNORECASE):
            if stack:
                pairs.append((stack.pop(), i))

    for s, e in pairs:
        used: Set[str] = set()
        decl_lines: List[int] = []
        for i in range(s + 1, e):
            code = split_code_comment(out[i].rstrip("\r\n"))[0].strip()
            if not code:
                continue
            if re.match(r"^\s*integer\b.*::", code, re.IGNORECASE):
                decl_lines.append(i)
                continue
            for nm in re.findall(r"\b[a-z][a-z0-9_]*\b", code, re.IGNORECASE):
                if _is_generated_loop_var(nm):
                    used.add(nm.lower())

        for i in decl_lines:
            raw = out[i]
            eol = "\r\n" if raw.endswith("\r\n") else ("\n" if raw.endswith("\n") else "")
            code, cmt = split_code_comment(raw.rstrip("\r\n"))
            m = re.match(r"^(\s*integer\b[^:\n]*::)\s*(.*)$", code, re.IGNORECASE)
            if not m:
                continue
            head = m.group(1)
            body = m.group(2).strip()
            items = [t.strip() for t in split_top_level_commas(body) if t.strip()]
            kept: List[str] = []
            changed = False
            for it in items:
                if _is_generated_loop_var(it) and it.lower() not in used:
                    changed = True
                    continue
                kept.append(it)
            if not changed:
                continue
            if kept:
                rebuilt = f"{head} {', '.join(kept)}"
                if cmt:
                    rebuilt += cmt
                out[i] = rebuilt + eol
            else:
                out[i] = ""
    return out


def _remove_redundant_extreme_seed_assign(lines: List[str]) -> List[str]:
    """Drop redundant `x = seed` immediately before `x = minval/maxval(...)`."""
    out = list(lines)
    i = 0
    while i + 1 < len(out):
        c1 = split_code_comment(out[i].rstrip("\r\n"))[0].strip()
        c2 = split_code_comment(out[i + 1].rstrip("\r\n"))[0].strip()
        m1 = ASSIGN_RE.match(c1)
        m2 = ASSIGN_RE.match(c2)
        if m1 and m2:
            lhs1 = m1.group(1).strip().lower()
            lhs2 = m2.group(1).strip().lower()
            rhs2 = m2.group(2).strip()
            if lhs1 == lhs2 and re.match(r"^(minval|maxval)\s*\(", rhs2, re.IGNORECASE):
                out.pop(i)
                continue
        i += 1
    return out


def _unwrap_redundant_blocks(lines: List[str]) -> List[str]:
    """Remove BLOCK/END BLOCK wrappers that contain no declarations."""
    out = list(lines)
    changed = True
    while changed:
        changed = False
        stack: List[int] = []
        pairs: List[Tuple[int, int]] = []
        for i, ln in enumerate(out):
            code = split_code_comment(ln.rstrip("\r\n"))[0].strip()
            if re.match(r"^block\b", code, re.IGNORECASE):
                stack.append(i)
            elif re.match(r"^end\s+block\b", code, re.IGNORECASE):
                if stack:
                    pairs.append((stack.pop(), i))
        for s, e in reversed(pairs):
            has_decl = False
            for j in range(s + 1, e):
                code = split_code_comment(out[j].rstrip("\r\n"))[0].strip()
                if not code:
                    continue
                if TYPE_DECL_RE.match(code):
                    has_decl = True
                    break
            if has_decl:
                continue
            for j in range(s + 1, e):
                ln = out[j]
                if ln.startswith("   "):
                    out[j] = ln[3:]
            del out[e]
            del out[s]
            changed = True
    return out


def _remove_unused_tool_marker_decls(lines: List[str]) -> List[str]:
    """Remove unused declaration markers like `! added by xto_loop.py`.

    This pass is scope-aware for BLOCK constructs so inner block-local uses do
    not keep an outer stale declaration alive.
    """
    out = list(lines)
    marker_idxs: List[int] = []
    marker_names: Dict[int, List[str]] = {}
    for i, ln in enumerate(out):
        body = ln.rstrip("\r\n")
        code, comment = split_code_comment(body)
        if "added by xto_loop.py" not in comment.lower():
            continue
        low = code.strip().lower()
        if not (TYPE_DECL_RE.match(low) and "::" in low):
            continue
        rhs = code.split("::", 1)[1]
        ents = parse_decl_entities(rhs)
        if not ents:
            continue
        names: List[str] = []
        for name, raw_chunk in ents:
            if "(" in raw_chunk or "=" in raw_chunk or "=>" in raw_chunk:
                continue
            names.append(name)
        if not names:
            continue
        marker_idxs.append(i)
        marker_names[i] = names

    if not marker_idxs:
        return out

    remove_names_by_idx: Dict[int, Set[str]] = {}
    for idx in marker_idxs:
        for name in marker_names[idx]:
            name_re = re.compile(rf"\b{re.escape(name)}\b", re.IGNORECASE)
            outer_used = False
            block_stack: List[Set[str]] = []
            for j, ln in enumerate(out):
                code = split_code_comment(ln.rstrip("\r\n"))[0].strip()
                low = code.lower()
                if not low:
                    continue
                if re.match(r"^block\b", low, re.IGNORECASE):
                    block_stack.append(set())
                    continue
                if re.match(r"^end\s+block\b", low, re.IGNORECASE):
                    if block_stack:
                        block_stack.pop()
                    continue
                if TYPE_DECL_RE.match(low) and "::" in low:
                    rhs = code.split("::", 1)[1]
                    for n, _raw in parse_decl_entities(rhs):
                        if block_stack:
                            block_stack[-1].add(n)
                    continue
                if not name_re.search(strip_quoted_text(code)):
                    continue
                if j == idx:
                    continue
                shadowed = any(name in decls for decls in reversed(block_stack))
                if not shadowed:
                    outer_used = True
                    break
            if not outer_used:
                remove_names_by_idx.setdefault(idx, set()).add(name)

    if not remove_names_by_idx:
        return out

    for i in sorted(remove_names_by_idx.keys(), reverse=True):
        raw = out[i].rstrip("\r\n")
        code, comment = split_code_comment(raw)
        if "::" not in code:
            continue
        head, rhs = code.split("::", 1)
        ents = parse_decl_entities(rhs)
        keep_chunks: List[str] = []
        removed = remove_names_by_idx[i]
        for name, raw_chunk in ents:
            if name in removed and "(" not in raw_chunk and "=" not in raw_chunk and "=>" not in raw_chunk:
                continue
            keep_chunks.append(raw_chunk.strip())
        if not keep_chunks:
            del out[i]
            continue
        nl = "\r\n" if out[i].endswith("\r\n") else ("\n" if out[i].endswith("\n") else "")
        rebuilt = f"{head.rstrip()} :: {', '.join(keep_chunks)}"
        if comment.strip():
            rebuilt += f" {comment}"
        out[i] = rebuilt + nl
    return out


def _collapse_assign_then_self_apply(lines: List[str]) -> List[str]:
    """Collapse consecutive:
      x = y
      x = f(x)
    to:
      x = f(y)
    Conservatively skip lines with trailing comments/continuations.
    """
    out = list(lines)
    i = 0
    while i + 1 < len(out):
        c1, cm1 = split_code_comment(out[i].rstrip("\r\n"))
        c2, cm2 = split_code_comment(out[i + 1].rstrip("\r\n"))
        s1 = c1.strip()
        s2 = c2.strip()
        if not s1 or not s2 or cm1.strip() or cm2.strip():
            i += 1
            continue
        if "&" in s1 or "&" in s2:
            i += 1
            continue
        m1 = ASSIGN_RE.match(s1)
        m2 = ASSIGN_RE.match(s2)
        if not m1 or not m2:
            i += 1
            continue
        lhs1 = m1.group(1).strip()
        lhs2 = m2.group(1).strip()
        m_l1 = SIMPLE_NAME_RE.match(lhs1)
        m_l2 = SIMPLE_NAME_RE.match(lhs2)
        if not m_l1 or not m_l2:
            i += 1
            continue
        v1 = m_l1.group(1)
        v2 = m_l2.group(1)
        if normalize_expr(v1) != normalize_expr(v2):
            i += 1
            continue
        rhs1 = m1.group(2).strip()
        rhs2 = m2.group(2).strip()
        if not re.search(rf"\b{re.escape(v1)}\b", rhs2, re.IGNORECASE):
            i += 1
            continue
        rhs2_new = re.sub(
            rf"\b{re.escape(v1)}\b",
            f"({rhs1})",
            rhs2,
            flags=re.IGNORECASE,
        )
        if rhs2_new == rhs2:
            i += 1
            continue
        indent = re.match(r"^\s*", out[i]).group(0)
        nl = "\r\n" if out[i].endswith("\r\n") else ("\n" if out[i].endswith("\n") else "")
        out[i] = f"{indent}{v1} = {rhs2_new}{nl}"
        del out[i + 1]
    return out


def _trim_redundant_parens_in_assignments(lines: List[str]) -> List[str]:
    """Clean simple redundant parens in single-line assignments, e.g. sqrt((sum(x))) -> sqrt(sum(x))."""
    out = list(lines)
    for i, raw in enumerate(out):
        code, comment = split_code_comment(raw.rstrip("\r\n"))
        s = code.strip()
        if not s:
            continue
        m = ASSIGN_RE.match(s)
        if not m:
            continue
        lhs = m.group(1).strip()
        rhs = m.group(2).strip()
        rhs2 = re.sub(
            r"\b([a-z][a-z0-9_]*)\s*\(\s*\((.+)\)\s*\)",
            lambda mm: (
                mm.group(0)
                if "," in mm.group(2)
                else f"{mm.group(1)}({mm.group(2)})"
            ),
            rhs,
            flags=re.IGNORECASE,
        )
        if rhs2 == rhs:
            continue
        indent = re.match(r"^\s*", raw).group(0)
        nl = "\r\n" if raw.endswith("\r\n") else ("\n" if raw.endswith("\n") else "")
        line = f"{indent}{lhs} = {rhs2}"
        if comment.strip():
            line += f" {comment}"
        out[i] = line + nl
    return out


def _inline_single_use_block_temp_into_print(lines: List[str]) -> List[str]:
    """Collapse:
      block
        <type> :: t
        t = expr
        print ..., t
      end block
    to:
      print ..., expr
    when t appears only in that print item list.
    """
    out = list(lines)
    i = 0
    while i + 4 < len(out):
        c0, cm0 = split_code_comment(out[i].rstrip("\r\n"))
        c1, cm1 = split_code_comment(out[i + 1].rstrip("\r\n"))
        c2, cm2 = split_code_comment(out[i + 2].rstrip("\r\n"))
        c3, cm3 = split_code_comment(out[i + 3].rstrip("\r\n"))
        c4, cm4 = split_code_comment(out[i + 4].rstrip("\r\n"))
        s0, s1, s2, s3, s4 = c0.strip(), c1.strip(), c2.strip(), c3.strip(), c4.strip()
        if cm0.strip() or cm1.strip() or cm2.strip() or cm3.strip() or cm4.strip():
            i += 1
            continue
        if s0.lower() != "block" or s4.lower() != "end block":
            i += 1
            continue
        if not (TYPE_DECL_RE.match(s1.lower()) and "::" in s1):
            i += 1
            continue
        ents = parse_decl_entities(s1.split("::", 1)[1])
        if len(ents) != 1:
            i += 1
            continue
        tname = ents[0][0]
        m_asn = ASSIGN_RE.match(s2)
        if not m_asn:
            i += 1
            continue
        lhs = m_asn.group(1).strip()
        rhs = m_asn.group(2).strip()
        if normalize_expr(lhs) != normalize_expr(tname):
            i += 1
            continue
        m_pr = PRINT_STMT_RE.match(s3)
        if not m_pr:
            i += 1
            continue
        parts = split_top_level_commas(m_pr.group(1).strip())
        if len(parts) < 2:
            i += 1
            continue
        items = [p.strip() for p in parts[1:]]
        item_hits = [k for k, it in enumerate(items) if normalize_expr(it) == normalize_expr(tname)]
        if len(item_hits) != 1:
            i += 1
            continue
        items[item_hits[0]] = rhs
        new_print = f"{parts[0].strip()}, " + ", ".join(items)
        indent = re.match(r"^\s*", out[i]).group(0)
        nl = "\r\n" if out[i + 3].endswith("\r\n") else ("\n" if out[i + 3].endswith("\n") else "")
        out[i] = f"{indent}print {new_print}{nl}"
        del out[i + 1 : i + 5]
    return out


def _collapse_random_number_blocks(lines: List[str]) -> List[str]:
    """Collapse common block wrappers around random_number fills to whole-array calls."""
    out = list(lines)
    i = 0
    while i < len(out):
        c0, _ = split_code_comment(out[i].rstrip("\r\n"))
        if c0.strip().lower() != "block":
            i += 1
            continue
        j = i + 1
        while j < len(out):
            cj, cmj = split_code_comment(out[j].rstrip("\r\n"))
            if cmj.strip():
                break
            sj = cj.strip()
            if TYPE_DECL_RE.match(sj.lower()):
                j += 1
                continue
            break
        # Form 1:
        # do j=1,n2
        #   call random_number(x(:,j))
        # end do
        # end block
        if j + 3 < len(out):
            s1 = split_code_comment(out[j].rstrip("\r\n"))[0].strip()
            s2 = split_code_comment(out[j + 1].rstrip("\r\n"))[0].strip()
            s3 = split_code_comment(out[j + 2].rstrip("\r\n"))[0].strip()
            s4 = split_code_comment(out[j + 3].rstrip("\r\n"))[0].strip()
            mdo = DO_RE.match(s1)
            mcall = RANDOM_CALL_RE.match(s2)
            if mdo and mcall and END_DO_RE.match(s3) and s4.lower() == "end block":
                iv = mdo.group(1).strip()
                arg = mcall.group(1).strip()
                msec = INDEXED_SECOND_DIM_RE.match(arg)
                if msec and normalize_expr(msec.group(2).strip()) == normalize_expr(":") and normalize_expr(msec.group(3).strip()) == normalize_expr(iv):
                    arr = msec.group(1).strip()
                    indent = re.match(r"^\s*", out[i]).group(0)
                    nl = "\r\n" if out[j + 3].endswith("\r\n") else ("\n" if out[j + 3].endswith("\n") else "")
                    out[i] = f"{indent}call random_number({arr}){nl}"
                    del out[i + 1 : j + 4]
                    continue
        # Form 2:
        # do j=...
        #   do i=...
        #     call random_number(x(i,j))
        #   end do
        # end do
        # end block
        if j + 5 < len(out):
            s1 = split_code_comment(out[j].rstrip("\r\n"))[0].strip()
            s2 = split_code_comment(out[j + 1].rstrip("\r\n"))[0].strip()
            s3 = split_code_comment(out[j + 2].rstrip("\r\n"))[0].strip()
            s4 = split_code_comment(out[j + 3].rstrip("\r\n"))[0].strip()
            s5 = split_code_comment(out[j + 4].rstrip("\r\n"))[0].strip()
            s6 = split_code_comment(out[j + 5].rstrip("\r\n"))[0].strip()
            mdo_o = DO_RE.match(s1)
            mdo_i = DO_RE.match(s2)
            mcall = RANDOM_CALL_RE.match(s3)
            if mdo_o and mdo_i and mcall and END_DO_RE.match(s4) and END_DO_RE.match(s5) and s6.lower() == "end block":
                ov = mdo_o.group(1).strip()
                iv = mdo_i.group(1).strip()
                arg = mcall.group(1).strip()
                m2 = INDEXED_SECOND_DIM_RE.match(arg)
                if m2 and normalize_expr(m2.group(3).strip()) == normalize_expr(ov):
                    left = m2.group(2).strip()
                    m1 = re.match(rf"^\s*{re.escape(iv)}\s*$", left, re.IGNORECASE)
                    if m1:
                        arr = m2.group(1).strip()
                        indent = re.match(r"^\s*", out[i]).group(0)
                        nl = "\r\n" if out[j + 5].endswith("\r\n") else ("\n" if out[j + 5].endswith("\n") else "")
                        out[i] = f"{indent}call random_number({arr}){nl}"
                        del out[i + 1 : j + 6]
                        continue
        # Form 3:
        # do k=...
        #   do j=...
        #     call random_number(x(:,j,k))
        #   end do
        # end do
        # end block
        if j + 5 < len(out):
            s1 = split_code_comment(out[j].rstrip("\r\n"))[0].strip()
            s2 = split_code_comment(out[j + 1].rstrip("\r\n"))[0].strip()
            s3 = split_code_comment(out[j + 2].rstrip("\r\n"))[0].strip()
            s4 = split_code_comment(out[j + 3].rstrip("\r\n"))[0].strip()
            s5 = split_code_comment(out[j + 4].rstrip("\r\n"))[0].strip()
            s6 = split_code_comment(out[j + 5].rstrip("\r\n"))[0].strip()
            mdo_o = DO_RE.match(s1)
            mdo_m = DO_RE.match(s2)
            mcall = RANDOM_CALL_RE.match(s3)
            if mdo_o and mdo_m and mcall and END_DO_RE.match(s4) and END_DO_RE.match(s5) and s6.lower() == "end block":
                ov = mdo_o.group(1).strip()
                mv = mdo_m.group(1).strip()
                arg = mcall.group(1).strip()
                m3 = re.match(
                    r"^\s*([a-z][a-z0-9_]*)\s*\(\s*:\s*,\s*([a-z][a-z0-9_]*)\s*,\s*([a-z][a-z0-9_]*)\s*\)\s*$",
                    arg,
                    re.IGNORECASE,
                )
                if m3 and normalize_expr(m3.group(2).strip()) == normalize_expr(mv) and normalize_expr(m3.group(3).strip()) == normalize_expr(ov):
                    arr = m3.group(1).strip()
                    indent = re.match(r"^\s*", out[i]).group(0)
                    nl = "\r\n" if out[j + 5].endswith("\r\n") else ("\n" if out[j + 5].endswith("\n") else "")
                    out[i] = f"{indent}call random_number({arr}){nl}"
                    del out[i + 1 : j + 6]
                    continue
        i += 1
    return out


def _collapse_xto_loop_print_sum_blocks(lines: List[str]) -> List[str]:
    """Collapse xto_loop rank-3 sum print blocks back to intrinsic sum prints."""
    out = list(lines)
    i = 0
    while i < len(out):
        c0, _ = split_code_comment(out[i].rstrip("\r\n"))
        if c0.strip().lower() != "block":
            i += 1
            continue
        # Find this block's end.
        e = i + 1
        while e < len(out):
            ce = split_code_comment(out[e].rstrip("\r\n"))[0].strip().lower()
            if ce == "end block":
                break
            e += 1
        if e >= len(out):
            i += 1
            continue
        body_codes: List[str] = []
        for k in range(i + 1, e):
            ck, cm = split_code_comment(out[k].rstrip("\r\n"))
            if cm.strip():
                continue
            sk = ck.strip()
            if not sk:
                continue
            if TYPE_DECL_RE.match(sk.lower()):
                continue
            body_codes.append(sk)

        # Scalar form:
        # t = 0
        # do k ... do j ... do i ... t = t + x(i,j,k) ... end do*3
        # print *, (t / size(x))
        if len(body_codes) == 9:
            m_init = ASSIGN_RE.match(body_codes[0])
            m_dok = DO_RE.match(body_codes[1])
            m_doj = DO_RE.match(body_codes[2])
            m_doi = DO_RE.match(body_codes[3])
            m_acc = ASSIGN_RE.match(body_codes[4])
            m_print = PRINT_PREFIX_RE.match(body_codes[8])
            if (
                m_init and m_dok and m_doj and m_doi and m_acc and m_print and
                END_DO_RE.match(body_codes[5]) and END_DO_RE.match(body_codes[6]) and END_DO_RE.match(body_codes[7])
            ):
                t = m_init.group(1).strip().lower()
                if t == m_acc.group(1).strip().lower() and ZERO_LITERAL_RE.match(m_init.group(2).strip()):
                    madd = re.match(rf"^\s*{re.escape(t)}\s*\+\s*([a-z][a-z0-9_]*)\s*\(([^)]*)\)\s*$", m_acc.group(2).strip(), re.IGNORECASE)
                    if madd:
                        arr = madd.group(1).strip()
                        mpr = re.match(
                            rf"^\s*\(?\s*{re.escape(t)}\s*/\s*size\s*\(\s*{re.escape(arr)}\s*\)\s*\)?\s*$",
                            m_print.group(1).strip(),
                            re.IGNORECASE,
                        )
                        idxs = [x.strip() for x in madd.group(2).split(",")]
                        if mpr and len(idxs) == 3:
                            indent = re.match(r"^\s*", out[i]).group(0)
                            nl = "\r\n" if out[e].endswith("\r\n") else ("\n" if out[e].endswith("\n") else "")
                            out[i] = f"{indent}print *, sum({arr})/size({arr}){nl}"
                            del out[i + 1 : e + 1]
                            continue

        # Dim forms:
        # do a; do b; tmp(...)=0; end do; end do;
        # do a; do b; do c; tmp(...)=tmp(...)+arr(...); end do*3;
        # tmp = tmp / den
        # print *, tmp
        if len(body_codes) == 14:
            m_zero = ASSIGN_RE.match(body_codes[2])
            m_acc = ASSIGN_RE.match(body_codes[8])
            m_scale = ASSIGN_RE.match(body_codes[12])
            m_print = PRINT_PREFIX_RE.match(body_codes[13])
            if (
                DO_RE.match(body_codes[0]) and DO_RE.match(body_codes[1]) and m_zero and
                END_DO_RE.match(body_codes[3]) and END_DO_RE.match(body_codes[4]) and
                DO_RE.match(body_codes[5]) and DO_RE.match(body_codes[6]) and DO_RE.match(body_codes[7]) and
                m_acc and END_DO_RE.match(body_codes[9]) and END_DO_RE.match(body_codes[10]) and END_DO_RE.match(body_codes[11]) and
                m_scale and m_print
            ):
                lhs0 = m_zero.group(1).strip()
                lhsa = m_acc.group(1).strip()
                lhss = m_scale.group(1).strip()
                m_base0 = re.match(r"^\s*([a-z][a-z0-9_]*)\s*\(", lhs0, re.IGNORECASE)
                m_bases = re.match(r"^\s*([a-z][a-z0-9_]*)\s*$", lhss, re.IGNORECASE)
                base0 = m_base0.group(1).strip().lower() if m_base0 else ""
                bases = m_bases.group(1).strip().lower() if m_bases else ""
                if base0 and bases and base0 == bases and normalize_expr(lhss) == normalize_expr(m_print.group(1).strip()) and ZERO_LITERAL_RE.match(m_zero.group(2).strip()):
                    madd = re.match(
                        rf"^\s*{re.escape(lhsa)}\s*\+\s*([a-z][a-z0-9_]*)\s*\(([^)]*)\)\s*$",
                        m_acc.group(2).strip(),
                        re.IGNORECASE,
                    )
                    mdiv = re.match(rf"^\s*{re.escape(lhss)}\s*/\s*(.+)\s*$", m_scale.group(2).strip(), re.IGNORECASE)
                    if madd and mdiv:
                        arr = madd.group(1).strip()
                        lhs_idx_m = re.match(r"^\s*[a-z][a-z0-9_]*\s*\(([^)]*)\)\s*$", lhsa, re.IGNORECASE)
                        if lhs_idx_m:
                            lhs_idxs = [normalize_expr(x.strip()) for x in lhs_idx_m.group(1).split(",")]
                            arr_idxs = [normalize_expr(x.strip()) for x in madd.group(2).split(",")]
                            if len(lhs_idxs) == 2 and len(arr_idxs) == 3:
                                if arr_idxs[0] not in lhs_idxs:
                                    dim = 1
                                elif arr_idxs[1] not in lhs_idxs:
                                    dim = 2
                                else:
                                    dim = 3
                                den = mdiv.group(1).strip()
                                indent = re.match(r"^\s*", out[i]).group(0)
                                nl = "\r\n" if out[e].endswith("\r\n") else ("\n" if out[e].endswith("\n") else "")
                                out[i] = f"{indent}print *, sum({arr},{dim})/{den}{nl}"
                                del out[i + 1 : e + 1]
                                continue
        i += 1
    return out


def _collapse_xto_loop_print_minmax_block(lines: List[str]) -> List[str]:
    """Collapse xto_loop min/max print blocks back to minval/maxval print."""
    out = list(lines)
    i = 0
    while i < len(out):
        c0, _ = split_code_comment(out[i].rstrip("\r\n"))
        if c0.strip().lower() != "block":
            i += 1
            continue
        e = i + 1
        while e < len(out):
            ce = split_code_comment(out[e].rstrip("\r\n"))[0].strip().lower()
            if ce == "end block":
                break
            e += 1
        if e >= len(out):
            i += 1
            continue
        body: List[str] = []
        for k in range(i + 1, e):
            ck, cm = split_code_comment(out[k].rstrip("\r\n"))
            sk = ck.strip()
            if cm.strip() or not sk:
                continue
            if TYPE_DECL_RE.match(sk.lower()):
                continue
            body.append(sk)
        # init + N do + if + N end do + print
        if len(body) < 5:
            i += 1
            continue
        m_init = ASSIGN_RE.match(body[0])
        m_print = PRINT_PREFIX_RE.match(body[-1])
        if not (m_init and m_print):
            i += 1
            continue
        t = m_init.group(1).strip()
        rhs0 = m_init.group(2).strip()
        m_rhs = re.match(r"^\s*([a-z][a-z0-9_]*)\s*\(([^)]*)\)\s*$", rhs0, re.IGNORECASE)
        if not m_rhs:
            i += 1
            continue
        arr = m_rhs.group(1).strip()
        if normalize_expr(m_print.group(1).strip()) != normalize_expr(t):
            i += 1
            continue
        do_count = 0
        p = 1
        while p < len(body) and DO_RE.match(body[p]):
            do_count += 1
            p += 1
        if do_count < 1:
            i += 1
            continue
        if p >= len(body):
            i += 1
            continue
        m_if = re.match(
            rf"^\s*if\s*\(\s*{re.escape(arr)}\s*\(([^)]*)\)\s*([<>])\s*{re.escape(t)}\s*\)\s*{re.escape(t)}\s*=\s*{re.escape(arr)}\s*\(([^)]*)\)\s*$",
            body[p],
            re.IGNORECASE,
        )
        if not m_if:
            i += 1
            continue
        idx_l = normalize_expr(m_if.group(1).strip())
        idx_r = normalize_expr(m_if.group(3).strip())
        if idx_l != idx_r:
            i += 1
            continue
        p += 1
        for _ in range(do_count):
            if p >= len(body) or not END_DO_RE.match(body[p]):
                break
            p += 1
        else:
            # consumed all expected end dos; require only trailing print
            if p == len(body) - 1:
                fn = "minval" if m_if.group(2) == "<" else "maxval"
                indent = re.match(r"^\s*", out[i]).group(0)
                nl = "\r\n" if out[e].endswith("\r\n") else ("\n" if out[e].endswith("\n") else "")
                out[i] = f"{indent}print *, {fn}({arr}){nl}"
                del out[i + 1 : e + 1]
                continue
        i += 1
    return out


def _collapse_random_seed_put_temp_blocks(lines: List[str]) -> List[str]:
    """Collapse block-temporary wrappers into call random_seed(put=<expr>)."""
    out = list(lines)
    i = 0
    while i < len(out):
        code0, _ = split_code_comment(out[i].rstrip("\r\n"))
        if code0.strip().lower() != "block":
            i += 1
            continue
        j = i + 1
        # Skip declaration lines.
        while j < len(out):
            cj, cmj = split_code_comment(out[j].rstrip("\r\n"))
            if cmj.strip():
                break
            sj = cj.strip()
            if TYPE_DECL_RE.match(sj.lower()):
                j += 1
                continue
            break
        if j + 2 >= len(out):
            i += 1
            continue
        s_asn = split_code_comment(out[j].rstrip("\r\n"))[0].strip()
        m_asn = ASSIGN_RE.match(s_asn)
        if not m_asn:
            i += 1
            continue
        tmp = m_asn.group(1).strip()
        rhs = m_asn.group(2).strip()
        if not SIMPLE_NAME_RE.match(tmp):
            i += 1
            continue
        k = j + 1
        # Skip pure comment lines between assignment and call.
        while k < len(out):
            ck, cmk = split_code_comment(out[k].rstrip("\r\n"))
            if ck.strip():
                break
            if cmk.strip():
                k += 1
                continue
            break
        if k >= len(out):
            i += 1
            continue
        s_call = split_code_comment(out[k].rstrip("\r\n"))[0].strip()
        m_call = re.match(
            rf"^\s*call\s+random_seed\s*\(\s*put\s*=\s*{re.escape(tmp)}\s*\)\s*$",
            s_call,
            re.IGNORECASE,
        )
        if not m_call:
            i += 1
            continue
        k2 = k + 1
        s_next = split_code_comment(out[k2].rstrip("\r\n"))[0].strip() if k2 < len(out) else ""
        # Optional deallocate guard.
        if re.match(
            rf"^\s*if\s*\(\s*allocated\s*\(\s*{re.escape(tmp)}\s*\)\s*\)\s*deallocate\s*\(\s*{re.escape(tmp)}\s*\)\s*$",
            s_next,
            re.IGNORECASE,
        ):
            k2 += 1
        if k2 >= len(out):
            i += 1
            continue
        s_end = split_code_comment(out[k2].rstrip("\r\n"))[0].strip()
        if s_end.lower() != "end block":
            i += 1
            continue
        indent = re.match(r"^\s*", out[i]).group(0)
        nl = "\r\n" if out[k2].endswith("\r\n") else ("\n" if out[k2].endswith("\n") else "")
        out[i] = f"{indent}call random_seed(put={rhs}){nl}"
        del out[i + 1 : k2 + 1]
    return out


def _collapse_seeded_minmax_print_blocks(lines: List[str]) -> List[str]:
    """Collapse xto_loop-style seeded min/max print block back to minval/maxval print."""
    out = list(lines)
    i = 0
    while i < len(out):
        c0, _ = split_code_comment(out[i].rstrip("\r\n"))
        if c0.strip().lower() != "block":
            i += 1
            continue
        j = i + 1
        while j < len(out):
            cj, cmj = split_code_comment(out[j].rstrip("\r\n"))
            if cmj.strip():
                break
            sj = cj.strip()
            if TYPE_DECL_RE.match(sj.lower()):
                j += 1
                continue
            break
        if j + 9 >= len(out):
            i += 1
            continue
        s_init_min = split_code_comment(out[j].rstrip("\r\n"))[0].strip()
        s_do_min = split_code_comment(out[j + 1].rstrip("\r\n"))[0].strip()
        s_if_min = split_code_comment(out[j + 2].rstrip("\r\n"))[0].strip()
        s_enddo_min = split_code_comment(out[j + 3].rstrip("\r\n"))[0].strip()
        s_init_max = split_code_comment(out[j + 4].rstrip("\r\n"))[0].strip()
        s_do_max = split_code_comment(out[j + 5].rstrip("\r\n"))[0].strip()
        s_if_max = split_code_comment(out[j + 6].rstrip("\r\n"))[0].strip()
        s_enddo_max = split_code_comment(out[j + 7].rstrip("\r\n"))[0].strip()
        s_print = split_code_comment(out[j + 8].rstrip("\r\n"))[0].strip()
        s_end = split_code_comment(out[j + 9].rstrip("\r\n"))[0].strip()

        m_init_min = ASSIGN_RE.match(s_init_min)
        m_init_max = ASSIGN_RE.match(s_init_max)
        m_do_min = DO_RE.match(s_do_min)
        m_do_max = DO_RE.match(s_do_max)
        m_if_min = re.match(
            r"^\s*if\s*\(\s*([a-z][a-z0-9_]*)\s*\(\s*([a-z][a-z0-9_]*)\s*\)\s*<\s*([a-z][a-z0-9_]*)\s*\)\s*([a-z][a-z0-9_]*)\s*=\s*\1\s*\(\s*\2\s*\)\s*$",
            s_if_min,
            re.IGNORECASE,
        )
        m_if_max = re.match(
            r"^\s*if\s*\(\s*([a-z][a-z0-9_]*)\s*\(\s*([a-z][a-z0-9_]*)\s*\)\s*>\s*([a-z][a-z0-9_]*)\s*\)\s*([a-z][a-z0-9_]*)\s*=\s*\1\s*\(\s*\2\s*\)\s*$",
            s_if_max,
            re.IGNORECASE,
        )
        m_print = PRINT_STMT_RE.match(s_print)
        if not (
            m_init_min and m_init_max and m_do_min and m_do_max and m_if_min and m_if_max and m_print and
            END_DO_RE.match(s_enddo_min) and END_DO_RE.match(s_enddo_max) and s_end.lower() == "end block"
        ):
            i += 1
            continue

        min_var = m_init_min.group(1).strip().lower()
        max_var = m_init_max.group(1).strip().lower()
        min_rhs = m_init_min.group(2).strip()
        max_rhs = m_init_max.group(2).strip()
        if min_var == max_var:
            i += 1
            continue
        m_rhs_min = INDEXED_ANY_RE.match(min_rhs)
        m_rhs_max = INDEXED_ANY_RE.match(max_rhs)
        if not (m_rhs_min and m_rhs_max):
            i += 1
            continue
        arr = m_rhs_min.group(1).strip().lower()
        if m_rhs_max.group(1).strip().lower() != arr:
            i += 1
            continue
        seed_idx = m_rhs_min.group(2).strip()
        if normalize_expr(seed_idx) != normalize_expr(m_rhs_max.group(2).strip()):
            i += 1
            continue

        iv1 = m_do_min.group(1).strip().lower()
        iv2 = m_do_max.group(1).strip().lower()
        if iv1 != iv2:
            i += 1
            continue
        lb1, ub1 = m_do_min.group(2).strip(), m_do_min.group(3).strip()
        lb2, ub2 = m_do_max.group(2).strip(), m_do_max.group(3).strip()
        if normalize_expr(lb1) != normalize_expr(lb2) or normalize_expr(ub1) != normalize_expr(ub2):
            i += 1
            continue
        if normalize_expr(lb1) != normalize_expr(f"({seed_idx}) + 1") and normalize_expr(lb1) != normalize_expr("2"):
            i += 1
            continue

        # Check IF bodies reference same array/index and target vars.
        if not (
            m_if_min.group(1).lower() == arr
            and m_if_max.group(1).lower() == arr
            and m_if_min.group(2).lower() == iv1
            and m_if_max.group(2).lower() == iv1
            and m_if_min.group(3).lower() == min_var
            and m_if_max.group(3).lower() == max_var
            and m_if_min.group(4).lower() == min_var
            and m_if_max.group(4).lower() == max_var
        ):
            i += 1
            continue

        parts = split_top_level_commas(m_print.group(1).strip())
        if len(parts) < 2:
            i += 1
            continue
        items = [p.strip().lower() for p in parts[1:]]
        if items != [min_var, max_var]:
            i += 1
            continue

        indent = re.match(r"^\s*", out[i]).group(0)
        nl = "\r\n" if out[j + 9].endswith("\r\n") else ("\n" if out[j + 9].endswith("\n") else "")
        out[i] = f"{indent}print *, minval({arr}), maxval({arr}){nl}"
        del out[i + 1 : j + 10]
    return out


def _rewrite_norm2_lines(lines: List[str]) -> List[str]:
    """Apply norm2 peephole rewrite on already-transformed assignment/print lines."""
    out = list(lines)
    for i, raw in enumerate(out):
        code, comment = split_code_comment(raw.rstrip("\r\n"))
        stmt = code.strip()
        if not stmt:
            continue
        rew = maybe_norm2_stmt_rewrite(stmt)
        if rew is None or rew.strip() == stmt:
            continue
        indent = re.match(r"^\s*", raw).group(0)
        nl = "\r\n" if raw.endswith("\r\n") else ("\n" if raw.endswith("\n") else "")
        line = f"{indent}{rew}"
        if comment.strip():
            line += f" {comment}"
        out[i] = line + nl
    return out


def _strip_foreign_tool_comment_lines(lines: List[str]) -> List[str]:
    """Remove standalone provenance comments from other transform tools."""
    out: List[str] = []
    pat = re.compile(r"^\s*!\s*.*\b(?:replaced by|changed by)\s+xto_loop\.py\b", re.IGNORECASE)
    for raw in lines:
        code, comment = split_code_comment(raw.rstrip("\r\n"))
        if code.strip():
            out.append(raw)
            continue
        c = comment.strip()
        if pat.match(c):
            continue
        out.append(raw)
    return out


def cleanup_redundant_generated_wrappers(path: Path) -> bool:
    """Run post-inline structural cleanup on one transformed file."""
    before = read_text_flexible(path)
    lines = before.splitlines(keepends=True)
    lines = _prune_unused_generated_block_decls(lines)
    lines = _remove_redundant_extreme_seed_assign(lines)
    lines = _unwrap_redundant_blocks(lines)
    lines = _remove_unused_tool_marker_decls(lines)
    lines = _collapse_assign_then_self_apply(lines)
    lines = _trim_redundant_parens_in_assignments(lines)
    lines = _inline_single_use_block_temp_into_print(lines)
    lines = _collapse_random_number_blocks(lines)
    lines = _collapse_xto_loop_print_sum_blocks(lines)
    lines = _collapse_xto_loop_print_minmax_block(lines)
    lines = _collapse_random_seed_put_temp_blocks(lines)
    lines = _collapse_seeded_minmax_print_blocks(lines)
    lines = _rewrite_norm2_lines(lines)
    lines = _strip_foreign_tool_comment_lines(lines)
    after = "".join(lines)
    if after != before:
        path.write_text(after, encoding="utf-8")
        return True
    return False


def remove_redundant_allocates_via_xalloc_assign(path: Path) -> int:
    """Delete redundant ALLOCATE-before-assignment statements in one file."""
    findings = xalloc_assign.analyze_file(path)
    if not findings:
        return 0
    lines = read_text_flexible(path).splitlines(keepends=True)
    removed = 0
    by_alloc_line: Dict[int, xalloc_assign.Finding] = {}
    for f in findings:
        by_alloc_line.setdefault(f.alloc_line, f)
    for alloc_line in sorted(by_alloc_line.keys(), reverse=True):
        f = by_alloc_line[alloc_line]
        if not (1 <= alloc_line <= len(lines)):
            continue
        if f.replace_assign_rhs is not None and 1 <= f.assign_line <= len(lines):
            lines[f.assign_line - 1] = xalloc_assign.rewrite_assignment_rhs_line(
                lines[f.assign_line - 1], f.var, f.replace_assign_rhs
            )
        end_line = xalloc_assign.end_line_for_stmt_start(lines, alloc_line)
        if end_line < alloc_line:
            continue
        del lines[alloc_line - 1 : end_line]
        removed += 1
    if removed > 0:
        path.write_text("".join(lines), encoding="utf-8")
    return removed


def inline_temps_via_xno_variable(path: Path, mode: str = "scalar") -> Tuple[int, int]:
    """Inline single-use temporaries using xno_variable logic on one file."""
    def has_attached_comment(raw_lines: List[str], line_no: int) -> bool:
        """Heuristic: treat trailing inline comments as attached to this line."""
        if line_no < 1 or line_no > len(raw_lines):
            return False
        body = raw_lines[line_no - 1].rstrip("\r\n")
        _code, comment = split_code_comment(body)
        return bool(comment.strip())

    def is_safe_array_inline_exception(f: xno_variable.Finding) -> bool:
        """Allow selected non-scalar inlines known to be safe in xarray post-pass."""
        use_low = f.use_stmt.strip().lower()
        var = re.escape(f.var.lower())
        # Safe case: constructor/array temp passed as PUT argument.
        if re.match(rf"^call\s+random_seed\s*\(.*\bput\s*=\s*{var}\b.*\)\s*$", use_low, re.IGNORECASE):
            return True
        return False

    total_inline = 0
    total_decl_removed = 0
    # Run to fixed point: one pass may inline only one temp on a shared use line.
    while True:
        findings = xno_variable.analyze_file(path)
        if not findings:
            break
        # Safety gate for scalar mode: only allow scalar-local temporary
        # inlining (plus narrow known-safe exceptions).
        scalar_by_unit: Dict[Tuple[str, str], Set[str]] = {}
        if mode == "scalar":
            infos, _missing = fscan.load_source_files([path])
            if infos:
                units = xunset.collect_units(infos[0])
                for u in units:
                    scal = set(xno_variable.parse_declared_scalar_locals(u).keys())
                    scalar_by_unit[(u.kind.lower(), u.name.lower())] = scal
        raw_lines = read_text_flexible(path).splitlines(keepends=True)
        filtered: List[xno_variable.Finding] = []
        for f in findings:
            unit_key = (f.unit_kind.lower(), f.unit_name.lower())
            if mode == "scalar":
                if f.var.lower() not in scalar_by_unit.get(unit_key, set()):
                    if not is_safe_array_inline_exception(f):
                        continue
            if has_attached_comment(raw_lines, f.assign_line) or has_attached_comment(raw_lines, f.use_line):
                continue
            filtered.append(f)
        if not filtered:
            break
        n_inline, n_decl_removed, _n_ann, _backup = xno_variable.apply_fix_file(
            path,
            filtered,
            annotate=False,
            out_path=None,
            create_backup=False,
        )
        if n_inline <= 0:
            break
        total_inline += n_inline
        total_decl_removed += n_decl_removed
    return total_inline, total_decl_removed


def remove_empty_changed_annotation_blocks(path: Path) -> int:
    """Remove empty ! begin/end block changed markers left after post passes."""
    lines = read_text_flexible(path).splitlines(keepends=True)
    if not lines:
        return 0
    out: List[str] = []
    removed = 0
    i = 0
    while i < len(lines):
        if CHANGED_BLOCK_BEGIN.lower() not in lines[i].lower():
            out.append(lines[i])
            i += 1
            continue
        j = i + 1
        only_blank = True
        while j < len(lines):
            low_j = lines[j].lower()
            if CHANGED_BLOCK_END.lower() in low_j:
                if only_blank:
                    removed += 1
                    i = j + 1
                else:
                    out.extend(lines[i : j + 1])
                    i = j + 1
                break
            if lines[j].strip():
                only_blank = False
            j += 1
        else:
            out.append(lines[i])
            i += 1
    if removed > 0:
        path.write_text("".join(out), encoding="utf-8")
    return removed


def relocate_section_comments(path: Path) -> int:
    """Move standalone section comments to a more relevant nearby statement."""
    lines = read_text_flexible(path).splitlines(keepends=True)
    if not lines:
        return 0

    stop = {
        "a",
        "an",
        "and",
        "by",
        "compute",
        "computed",
        "create",
        "de",
        "for",
        "from",
        "in",
        "is",
        "of",
        "set",
        "the",
        "to",
        "values",
        "with",
    }
    token_re = re.compile(r"\b([a-z][a-z0-9_]*)\b", re.IGNORECASE)
    search_window = 80
    moved = 0

    def is_comment_line(raw: str) -> bool:
        return raw.lstrip().startswith("!")

    def is_blank_line(raw: str) -> bool:
        return not raw.strip()

    def is_exec_line(raw: str) -> bool:
        t, _comment = split_code_comment(raw)
        t = t.strip()
        return bool(t) and not t.startswith("!")

    def comment_tokens(raw: str) -> List[str]:
        txt = raw.lstrip()
        while txt.startswith("!"):
            txt = txt[1:]
        out: List[str] = []
        seen: Set[str] = set()
        for m in token_re.finditer(txt.lower()):
            tok = m.group(1)
            if tok in stop:
                continue
            if tok not in seen:
                seen.add(tok)
                out.append(tok)
        return out

    def stmt_score(stmt: str, toks: List[str]) -> int:
        low = stmt.lower()
        s = 0
        for tok in toks:
            if re.search(rf"\b{re.escape(tok)}\b", low):
                s += 1
        return s

    i = 0
    while i < len(lines):
        raw = lines[i]
        if not is_comment_line(raw) or is_blank_line(raw):
            i += 1
            continue
        # Keep xarray-generated audit/marker comments in place.
        low_raw = raw.lower()
        if (
            "changed by xarray.py" in low_raw
            or "removed by xarray.py" in low_raw
            or "suggested replacement by xarray.py" in low_raw
            or "begin block changed by xarray.py" in low_raw
            or "end block changed by xarray.py" in low_raw
            or "begin block removed by xarray.py" in low_raw
            or "end block removed by xarray.py" in low_raw
            or BEGIN_TAG.lower() in low_raw
            or END_TAG.lower() in low_raw
        ):
            i += 1
            continue

        toks = comment_tokens(raw)
        if len(toks) < 1:
            i += 1
            continue

        next_exec = -1
        j = i + 1
        while j < len(lines):
            if is_exec_line(lines[j]):
                next_exec = j
                break
            j += 1
        if next_exec < 0:
            i += 1
            continue

        cur_stmt, _comment = split_code_comment(lines[next_exec])
        cur_stmt = cur_stmt.strip()
        cur_score = stmt_score(cur_stmt, toks)
        best_idx = next_exec
        best_score = cur_score

        end = min(len(lines), i + 1 + search_window)
        j = next_exec + 1
        while j < end:
            if not is_exec_line(lines[j]):
                j += 1
                continue
            stmt_j, _comment = split_code_comment(lines[j])
            s = stmt_score(stmt_j.strip(), toks)
            if s > best_score:
                best_score = s
                best_idx = j
            j += 1

        # Move only when clearly better and sufficiently specific.
        if best_idx > next_exec and best_score >= 2 and best_score > cur_score:
            line = lines.pop(i)
            insert_at = best_idx - 1
            lines.insert(insert_at, line)
            moved += 1
            # Continue after the moved line.
            i = insert_at + 1
            continue

        i += 1

    if moved > 0:
        path.write_text("".join(lines), encoding="utf-8")
    return moved


def count_file_lines(path: Path) -> int:
    """Return LOC count excluding blank and comment-only lines."""
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


def print_summary_table(rows: List[Tuple[str, int, int, int]]) -> None:
    """Print aligned LOC summary using shared helper."""
    fscan.print_loc_summary_table(rows, source_col="source", blocks_col="blocks_rep")


def print_loc_summary_table_extended(
    rows: List[Tuple[str, int, int, int]],
    *,
    source_col: str = "source",
    blocks_col: str = "blocks_rep",
    compile_old: Optional[Dict[str, Optional[bool]]] = None,
    compile_new: Optional[Dict[str, Optional[bool]]] = None,
    runs_match: Optional[Dict[str, Optional[bool]]] = None,
) -> None:
    """Print aligned LOC summary with optional compile/run-diff booleans."""
    include_compile = (compile_old is not None) or (compile_new is not None)
    include_diff = runs_match is not None
    headers = [source_col, "lines_old", "lines_new", "diff", "ratio", blocks_col]
    if include_compile:
        headers.extend(["compile_old", "compile_new"])
    if include_diff:
        headers.append("runs_match")

    def fmt_bool(v: Optional[bool]) -> str:
        if v is None:
            return "NA"
        return "True" if v else "False"

    formatted: List[List[str]] = []
    for src, old_loc, new_loc, blocks in rows:
        diff = old_loc - new_loc
        ratio = "inf" if new_loc == 0 else f"{(old_loc / new_loc):.2f}"
        rec = [src, str(old_loc), str(new_loc), str(diff), ratio, str(blocks)]
        if include_compile:
            rec.append(fmt_bool((compile_old or {}).get(src)))
            rec.append(fmt_bool((compile_new or {}).get(src)))
        if include_diff:
            rec.append(fmt_bool((runs_match or {}).get(src)))
        formatted.append(rec)

    widths = [len(h) for h in headers]
    for r in formatted:
        for i, cell in enumerate(r):
            widths[i] = max(widths[i], len(cell))

    print("  ".join(headers[i].ljust(widths[i]) if i == 0 else headers[i].rjust(widths[i]) for i in range(len(headers))))
    for r in formatted:
        print("  ".join(r[i].ljust(widths[i]) if i == 0 else r[i].rjust(widths[i]) for i in range(len(r))))


def compile_one_for_summary(source: Path, command: str, *, phase: str) -> bool:
    """Compile one source using command template; return True on success.

    On failure, print command and compiler diagnostics.
    """
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


def compute_compile_maps(
    files: List[Path],
    transformed_paths: Dict[Path, Path],
    *,
    do_compile_both: bool,
    compiler_cmd: Optional[str],
    did_fix: bool,
    maxfail: Optional[int] = None,
) -> Tuple[Optional[Dict[str, Optional[bool]]], Optional[Dict[str, Optional[bool]]]]:
    """Build per-source compile status maps for summary output."""
    if not do_compile_both:
        return None, None
    cmd = compiler_cmd or "gfortran -c -w -Wfatal-errors {file}"
    old_map: Dict[str, Optional[bool]] = {}
    new_map: Dict[str, Optional[bool]] = {}
    regressions = 0
    for p in files:
        key = fscan.display_path(p)
        ok_old = compile_one_for_summary(p, cmd, phase="old")
        old_map[key] = ok_old
        if did_fix:
            tp = transformed_paths.get(p, p)
            ok_new = compile_one_for_summary(tp, cmd, phase="new")
            new_map[key] = ok_new
            if ok_old and not ok_new:
                regressions += 1
                if maxfail is not None and regressions >= maxfail:
                    print(
                        f"Compile-both stopped at maxfail={maxfail} "
                        f"(old compiled, new failed)."
                    )
                    break
        else:
            new_map[key] = ok_old
    return old_map, new_map


def compile_and_run_capture(
    source: Path,
    *,
    exe_path: Path,
    label: str,
    quiet_run: bool = False,
    keep_exe: bool = False,
    deterministic_seed: bool = False,
) -> Tuple[bool, str, str]:
    compile_source = source
    seeded_temp_path: Optional[Path] = None
    if deterministic_seed:
        try:
            txt = read_text_flexible(source)
            has_random_number = re.search(r"^\s*call\s+random_number\s*\(", txt, re.IGNORECASE | re.MULTILINE) is not None
            has_seed_control = re.search(
                r"^\s*call\s+random_(?:seed|init)\s*\(",
                txt,
                re.IGNORECASE | re.MULTILINE,
            ) is not None
            if has_random_number and not has_seed_control:
                lines = txt.splitlines(keepends=True)
                injected = False
                prog_idx = -1
                for i, ln in enumerate(lines):
                    if re.match(r"^\s*program\b", ln, re.IGNORECASE) and not re.match(r"^\s*end\s+program\b", ln, re.IGNORECASE):
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
                        lines.insert(i, f"{indent}call random_init(.true., .false.) ! added by xarray.py run-diff\n")
                        injected = True
                        break
                if injected:
                    fd, tmpname = tempfile.mkstemp(
                        prefix=f"{source.stem}_xarray_seed_",
                        suffix=source.suffix if source.suffix else ".f90",
                        dir=str(source.parent),
                    )
                    with open(fd, "w", encoding="utf-8", closefd=True) as tf:
                        tf.write("".join(lines))
                    seeded_temp_path = Path(tmpname)
                    compile_source = seeded_temp_path
        except Exception:
            compile_source = source

    compile_cmd = ["gfortran", str(compile_source), "-o", str(exe_path)]
    print(f"Build ({label}): {' '.join(fbuild.quote_cmd_arg(x) for x in compile_cmd)}")
    cp = subprocess.run(compile_cmd, capture_output=True, text=True)
    if cp.returncode != 0:
        print(f"Build ({label}): FAIL (exit {cp.returncode})")
        if cp.stdout:
            print(cp.stdout.rstrip())
        if cp.stderr:
            print(fbuild.format_linker_stderr(cp.stderr).rstrip())
        if seeded_temp_path is not None:
            try:
                seeded_temp_path.unlink(missing_ok=True)
            except Exception:
                pass
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
            try:
                seeded_temp_path.unlink(missing_ok=True)
            except Exception:
                pass
        if not keep_exe:
            try:
                exe_path.unlink(missing_ok=True)
            except Exception:
                pass


def compile_and_run_fortran(
    source: Path,
    *,
    exe_path: Path,
    label: str,
    quiet_run: bool = False,
    keep_exe: bool = False,
) -> bool:
    ok, _out, _err = compile_and_run_capture(
        source, exe_path=exe_path, label=label, quiet_run=quiet_run, keep_exe=keep_exe
    )
    return ok


def main() -> int:
    """Run xarray advisory and optional annotation mode."""
    parser = argparse.ArgumentParser(
        description="Suggest replacing simple Fortran loops with array operations"
    )
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--verbose", action="store_true", help="Print full replacement suggestions")
    parser.add_argument("--progress", action="store_true", help="Print per-file progress while processing")
    parser.add_argument(
        "--summary",
        action="store_true",
        help="Print per-file summary: file candidates before_lines after_lines delta",
    )
    parser.add_argument("--fix", action="store_true", help="Apply suggested replacements in-place")
    parser.add_argument("--out", type=Path, help="With --fix, write transformed output to this file (single input)")
    parser.add_argument("--out-dir", type=Path, help="With --fix, write transformed outputs to this directory")
    parser.add_argument(
        "--tee",
        action="store_true",
        help="With --fix --out, also print transformed output text to stdout",
    )
    parser.add_argument(
        "--tee-both",
        action="store_true",
        help="With --fix --out, print original and transformed source text to stdout",
    )
    parser.add_argument("--run", action="store_true", help="After --fix with changes, build and run transformed source")
    parser.add_argument(
        "--run-both",
        action="store_true",
        help="Build/run original source, and build/run transformed source when changes are applied",
    )
    parser.add_argument(
        "--run-diff",
        action="store_true",
        help="Run original and transformed executables and compare stdout/stderr",
    )
    parser.add_argument("--quiet-run", action="store_true", help="Do not print program stdout/stderr on successful runs")
    parser.add_argument("--keep-exe", action="store_true", help="Keep generated executables after running")
    parser.add_argument("--annotate", action="store_true", help="Insert annotated suggestion blocks")
    parser.add_argument(
        "--no-annotate",
        "-no-annotate",
        dest="no_annotate",
        action="store_true",
        help="Do not emit inline xarray annotation comments in transformed output",
    )
    parser.add_argument(
        "--annotate-removed",
        action="store_true",
        help="When fixing, keep removed source lines as comments above replacements",
    )
    parser.add_argument("--diff", action="store_true", help="With --fix, print unified diffs for changed files")
    parser.add_argument("--backup", dest="backup", action="store_true", default=True)
    parser.add_argument("--no-backup", dest="backup", action="store_false")
    parser.add_argument("--compiler", type=str, help="Compile command for baseline/after-fix validation")
    parser.add_argument(
        "--compile-both",
        action="store_true",
        help="With --summary, add per-file compile_old/compile_new booleans",
    )
    parser.add_argument(
        "--maxfail",
        type=int,
        help="With --compile-both, stop compile checks after N old-pass/new-fail cases",
    )
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
    inline_group = parser.add_mutually_exclusive_group()
    inline_group.add_argument(
        "--inline",
        dest="inline_all",
        action="store_true",
        help="After xarray rewrites, run broad temporary inlining post-pass",
    )
    inline_group.add_argument(
        "--inline-scalar",
        dest="inline_scalar",
        action="store_true",
        help="After xarray rewrites, run restricted scalar-only inlining post-pass (plus known-safe exceptions)",
    )
    parser.add_argument(
        "--aggressive",
        action="store_true",
        help="Enable more aggressive rewrites (e.g., collapse unrolled chunk loops to full-section assignments)",
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
    args.post_inline_temp = bool(args.inline_all or args.inline_scalar)
    args.inline_mode = "all" if args.inline_all else ("scalar" if args.inline_scalar else "none")
    global AGGRESSIVE_MODE
    AGGRESSIVE_MODE = bool(args.aggressive)
    if args.limit is not None and args.limit < 1:
        print("--limit must be >= 1.")
        return 2
    if args.maxfail is not None and args.maxfail < 1:
        print("--maxfail must be >= 1.")
        return 2
    if args.no_annotate:
        args.annotate = False
        args.annotate_removed = False
    if args.run_diff:
        args.run_both = True
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
    if args.annotate_removed and not args.fix:
        print("--annotate-removed requires --fix.")
        return 2
    if args.compiler and not args.fix:
        print("--compiler requires --fix.")
        return 2
    if args.maxfail is not None and not args.compile_both:
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
    baseline_compile_paths = files
    if args.fix and args.out is not None:
        after_compile_paths = [args.out]
    elif args.fix and args.out_dir is not None:
        after_compile_paths = [args.out_dir / p.name for p in files]
    else:
        after_compile_paths = files
    if args.fix and args.compiler and not args.compile_both:
        if not fbuild.run_compiler_command(args.compiler, baseline_compile_paths, "baseline", fscan.display_path):
            return 5

    findings: List[Finding] = []
    total_files = len(files)
    for i_file, p in enumerate(files, start=1):
        if args.progress:
            print(f"[{i_file}/{total_files}] Analyze {p}")
        base_findings = analyze_file(p)
        if args.concurrent or args.forall:
            cf_findings = analyze_file_concurrent_forall(
                p,
                allow_concurrent=args.concurrent,
                allow_forall=args.forall,
            )
            # Prefer canonical array-operation rewrites.
            # Concurrent/FORALL suggestions are fallback-only for loop ranges
            # not already handled by the main analyzer.
            base_keys = {
                (f.path, f.unit_kind, f.unit_name, f.start_line, f.end_line)
                for f in base_findings
            }
            findings.extend(base_findings)
            for f in cf_findings:
                k = (f.path, f.unit_kind, f.unit_name, f.start_line, f.end_line)
                if k in base_keys:
                    continue
                findings.append(f)
        else:
            findings.extend(base_findings)

    by_file_candidates: Dict[Path, List[Finding]] = {}
    for f in findings:
        by_file_candidates.setdefault(f.path, []).append(f)
    pre_lines: Dict[Path, int] = {p: count_file_lines(p) for p in files}
    run_compile_old_map: Dict[str, Optional[bool]] = {}
    run_compile_new_map: Dict[str, Optional[bool]] = {}
    run_diff_match_map: Dict[str, Optional[bool]] = {}
    summary_rows: Optional[List[Tuple[str, int, int, int]]] = None
    summary_compile_old_map: Optional[Dict[str, Optional[bool]]] = None
    summary_compile_new_map: Optional[Dict[str, Optional[bool]]] = None

    if not findings:
        orig_out = ""
        orig_err = ""
        if args.run_both:
            for src in files:
                orig_exe = Path(f"{src.stem}_orig.exe")
                ok_orig, orig_out, orig_err = compile_and_run_capture(
                    src,
                    exe_path=orig_exe,
                    label="original",
                    quiet_run=args.quiet_run,
                    keep_exe=args.keep_exe,
                    deterministic_seed=args.run_diff,
                )
                run_compile_old_map[fscan.display_path(src)] = ok_orig
                if not ok_orig:
                    return 5
        if args.run_diff:
            print("Run diff: SKIP (no transformations suggested)")
        if args.summary:
            rows: List[Tuple[str, int, int, int]] = []
            for p in files:
                before = pre_lines.get(p, 0)
                rows.append((fscan.display_path(p), before, before, 0))
            compile_old_map, compile_new_map = compute_compile_maps(
                files,
                {},
                do_compile_both=args.compile_both,
                compiler_cmd=args.compiler,
                did_fix=False,
                maxfail=args.maxfail,
            )
            summary_rows = rows
            summary_compile_old_map = compile_old_map
            summary_compile_new_map = compile_new_map
            fscan.print_loc_summary_table(
                rows,
                source_col="source",
                blocks_col="blocks_rep",
                compile_old=compile_old_map,
                compile_new=compile_new_map,
            )
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

    transformed_changed = False
    transformed_target: Optional[Path] = None
    orig_out = ""
    orig_err = ""
    xform_out = ""
    xform_err = ""
    run_diff_pairs = 0
    run_diff_diffs = 0
    transformed_by_file: Dict[Path, Path] = {p: p for p in files}
    replaced_by_file: Dict[Path, int] = {}
    if args.fix:
        by_file = by_file_candidates
        touched = 0
        total = 0
        total_alloc_removed = 0
        total_post_inlined = 0
        total_post_decl_removed = 0
        total_comments_relocated = 0
        post_lines: Dict[Path, int] = {}
        compile_old_map_run: Optional[Dict[str, Optional[bool]]] = {} if args.compile_both else None
        compile_new_map_run: Optional[Dict[str, Optional[bool]]] = {} if args.compile_both else None
        compile_cmd = args.compiler or "gfortran -c -w -Wfatal-errors {file}"
        regressions = 0
        stop_early = False
        fix_files = sorted(by_file.keys(), key=lambda x: x.name.lower())
        process_files = files if args.compile_both else fix_files
        fix_total = len(process_files)
        for i_fix, p in enumerate(process_files, start=1):
            if stop_early:
                break
            if args.progress:
                print(f"\n[{i_fix}/{fix_total}] Fix {p}")
            out_path = args.out if args.out is not None else (args.out_dir / p.name if args.out_dir is not None else None)
            key = fscan.display_path(p)
            if args.compile_both:
                ok_old = compile_one_for_summary(p, compile_cmd, phase="old")
                compile_old_map_run[key] = ok_old
                if not ok_old:
                    # Skip transformation/new compile for this source.
                    compile_new_map_run[key] = None
                    replaced_by_file[p] = 0
                    transformed_by_file[p] = out_path if out_path is not None else p
                    post_lines[p] = pre_lines.get(p, count_file_lines(p))
                    continue
            if p not in by_file:
                replaced_by_file[p] = 0
                transformed_by_file[p] = out_path if out_path is not None else p
                post_lines[p] = pre_lines.get(p, count_file_lines(p))
                if args.compile_both:
                    compile_new_map_run[key] = compile_old_map_run.get(key)
                continue
            before = read_text_flexible(p)
            if out_path is not None and args.tee_both:
                print(f"--- original: {p} ---")
                print(before, end="")
                if not before.endswith("\n"):
                    print("")
            n, backup, removed_locals = apply_fix_file(
                p,
                by_file[p],
                annotate=args.annotate,
                annotate_removed=args.annotate_removed,
                out_path=out_path,
                create_backup=args.backup,
                add_trace=args.trace,
            )
            total += n
            replaced_by_file[p] = n
            if n > 0:
                transformed_changed = True
                touched += 1
                target = out_path if out_path is not None else p
                n_alloc_removed = remove_redundant_allocates_via_xalloc_assign(target)
                total_alloc_removed += n_alloc_removed
                n_post_inl = 0
                n_post_decl = 0
                n_comments = 0
                if args.post_inline_temp:
                    n_post_inl, n_post_decl = inline_temps_via_xno_variable(target, mode=args.inline_mode)
                    total_post_inlined += n_post_inl
                    total_post_decl_removed += n_post_decl
                    if args.annotate:
                        remove_empty_changed_annotation_blocks(target)
                    # Keep audit annotations stable and local.
                    if not (args.annotate or args.annotate_removed):
                        n_comments = relocate_section_comments(target)
                        total_comments_relocated += n_comments
                cleanup_redundant_generated_wrappers(target)
                if out_path is not None:
                    print(f"\nFixed {p.name}: replaced {n} block(s), wrote {out_path}")
                    if args.tee:
                        print(f"--- transformed: {out_path} ---")
                        print(read_text_flexible(out_path), end="")
                else:
                    print(f"\nFixed {p.name}: replaced {n} block(s), backup {backup.name if backup else '(none)'}")
                if args.verbose and removed_locals:
                    print(f"  removed unused locals: {', '.join(removed_locals)}")
                if n_alloc_removed > 0:
                    print(f"  removed redundant allocate-before-assignment: {n_alloc_removed}")
                if n_post_inl > 0:
                    print(
                        f"  post-inline-temp: inlined {n_post_inl}, "
                        f"decl-entities removed {n_post_decl}"
                    )
                if args.post_inline_temp and n_comments > 0:
                    print(f"  relocated section comments: {n_comments}")
                if args.diff:
                    after = read_text_flexible(out_path if out_path is not None else p)
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
            elif out_path is not None and args.tee:
                if args.tee_both:
                    print(f"--- transformed: {out_path} ---")
                print(read_text_flexible(out_path), end="")
            target = out_path if out_path is not None else p
            transformed_target = target
            transformed_by_file[p] = target
            post_lines[p] = count_file_lines(target)
            if args.compile_both:
                ok_new = compile_one_for_summary(target, compile_cmd, phase="new")
                compile_new_map_run[key] = ok_new
                if compile_old_map_run.get(key) and not ok_new:
                    regressions += 1
                    if args.maxfail is not None and regressions >= args.maxfail:
                        print(
                            f"Compile-both stopped at maxfail={args.maxfail} "
                            f"(old compiled, new failed)."
                        )
                        stop_early = True
        summary = (
            f"\n--fix summary: files changed {touched}, replaced {total}, "
            f"allocates removed {total_alloc_removed}"
        )
        if args.post_inline_temp:
            summary += (
                f", post-inlined {total_post_inlined}, "
                f"post decl-entities removed {total_post_decl_removed}, "
                f"comments relocated {total_comments_relocated}"
            )
        print(summary)
        if args.summary:
            rows = []
            for p in files:
                before_n = pre_lines.get(p, 0)
                after_n = post_lines.get(p, before_n)
                blocks_n = replaced_by_file.get(p, 0)
                rows.append((fscan.display_path(p), before_n, after_n, blocks_n))
            if args.compile_both:
                compile_old_map = compile_old_map_run
                compile_new_map = compile_new_map_run
            else:
                compile_old_map, compile_new_map = compute_compile_maps(
                    files,
                    transformed_by_file,
                    do_compile_both=False,
                    compiler_cmd=args.compiler,
                    did_fix=True,
                    maxfail=args.maxfail,
                )
            summary_rows = rows
            summary_compile_old_map = compile_old_map
            summary_compile_new_map = compile_new_map
            fscan.print_loc_summary_table(
                rows,
                source_col="source",
                blocks_col="blocks_rep",
                compile_old=compile_old_map,
                compile_new=compile_new_map,
            )
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
            rows = []
            for p in files:
                before_n = pre_lines.get(p, 0)
                after_n = post_lines.get(p, before_n)
                rows.append((fscan.display_path(p), before_n, after_n, 0))
            compile_old_map, compile_new_map = compute_compile_maps(
                files,
                {},
                do_compile_both=args.compile_both,
                compiler_cmd=args.compiler,
                did_fix=False,
                maxfail=args.maxfail,
            )
            summary_rows = rows
            summary_compile_old_map = compile_old_map
            summary_compile_new_map = compile_new_map
            fscan.print_loc_summary_table(
                rows,
                source_col="source",
                blocks_col="blocks_rep",
                compile_old=compile_old_map,
                compile_new=compile_new_map,
            )
    elif args.summary:
        rows = []
        for p in files:
            before_n = pre_lines.get(p, 0)
            cand_n = len(by_file_candidates.get(p, []))
            rows.append((fscan.display_path(p), before_n, before_n, cand_n))
        compile_old_map, compile_new_map = compute_compile_maps(
            files,
            {},
            do_compile_both=args.compile_both,
            compiler_cmd=args.compiler,
            did_fix=False,
            maxfail=args.maxfail,
        )
        summary_rows = rows
        summary_compile_old_map = compile_old_map
        summary_compile_new_map = compile_new_map
        fscan.print_loc_summary_table(
            rows,
            source_col="source",
            blocks_col="blocks_rep",
            compile_old=compile_old_map,
            compile_new=compile_new_map,
        )
    if args.fix and args.compiler and not args.compile_both:
        if not fbuild.run_compiler_command(args.compiler, after_compile_paths, "after-fix", fscan.display_path):
            return 5
    if args.run:
        for p in files:
            key = fscan.display_path(p)
            if args.run_both:
                orig_exe = Path(f"{p.stem}_orig.exe")
                ok_orig, orig_out, orig_err = compile_and_run_capture(
                    p,
                    exe_path=orig_exe,
                    label="original",
                    quiet_run=args.quiet_run,
                    keep_exe=args.keep_exe,
                    deterministic_seed=args.run_diff,
                )
                run_compile_old_map[key] = ok_orig
                if not ok_orig:
                    return 5
            out_src = transformed_by_file.get(p, p)
            if args.run_diff and args.fix and replaced_by_file.get(p, 0) <= 0:
                print(f"Run diff ({p.name}): SKIP (no transformations applied)")
                run_diff_match_map[key] = None
                continue
            # For --run (without --run-both), skip files with no applied transformation.
            if (not args.run_both) and (replaced_by_file.get(p, 0) <= 0):
                continue
            out_exe = Path(f"{out_src.stem}.exe")
            ok_xf, xform_out, xform_err = compile_and_run_capture(
                out_src,
                exe_path=out_exe,
                label="transformed",
                quiet_run=args.quiet_run,
                keep_exe=args.keep_exe,
                deterministic_seed=args.run_diff,
            )
            run_compile_new_map[key] = ok_xf
            if not ok_xf:
                return 5
            if args.run_diff:
                run_diff_pairs += 1
                same = outputs_match_tolerant(orig_out, xform_out) and outputs_match_tolerant(orig_err, xform_err)
                run_diff_match_map[key] = same
                if same:
                    print(f"Run diff ({p.name}): MATCH")
                else:
                    run_diff_diffs += 1
                    print(f"Run diff ({p.name}): DIFF")
                    orig_blob = f"STDOUT:\n{orig_out}\nSTDERR:\n{orig_err}\n"
                    xform_blob = f"STDOUT:\n{xform_out}\nSTDERR:\n{xform_err}\n"
                    for line in difflib.unified_diff(
                        orig_blob.splitlines(),
                        xform_blob.splitlines(),
                        fromfile=f"{p.name}:original",
                        tofile=f"{p.name}:transformed",
                        lineterm="",
                    ):
                        print(line)
    if args.run_diff:
        if run_diff_pairs == 0:
            print("Run diff: SKIP (no transformed pairs executed)")
        else:
            print(
                f"Run diff summary: pairs {run_diff_pairs}, "
                f"matches {run_diff_pairs - run_diff_diffs}, diffs {run_diff_diffs}"
            )
    if args.summary and summary_rows is not None and (
        args.run or args.run_both or args.run_diff
    ):
        merged_old = dict(summary_compile_old_map or {})
        merged_new = dict(summary_compile_new_map or {})
        merged_old.update(run_compile_old_map)
        merged_new.update(run_compile_new_map)
        print("\n--summary (with run/compile):")
        print_loc_summary_table_extended(
            summary_rows,
            source_col="source",
            blocks_col="blocks_rep",
            compile_old=(merged_old if merged_old else None),
            compile_new=(merged_new if merged_new else None),
            runs_match=(run_diff_match_map if args.run_diff else None),
        )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
