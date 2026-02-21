#!/usr/bin/env python3
"""Warn about non-advancing WRITE loops that can be collapsed to one array WRITE."""

from __future__ import annotations

import argparse
import os
import re
import shutil
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, List, Optional, Tuple

import cli_paths as cpaths
import fortran_scan as fscan

DO_INDEX_RE = re.compile(
    r"^\s*do\s+([a-z][a-z0-9_]*)\s*=\s*(.+?)\s*,\s*(.+?)\s*$",
    re.IGNORECASE,
)
END_DO_RE = re.compile(r"^\s*end\s*do\b", re.IGNORECASE)
WRITE_RE = re.compile(r"^\s*write\b", re.IGNORECASE)


@dataclass
class LoopCtx:
    """Minimal stack frame for one DO loop while scanning statements."""

    var: str
    lb: str
    ub: str
    start_line: int
    body: List[Tuple[int, str]]


@dataclass
class Finding:
    """One advisory finding."""

    path: Path
    line_start: int
    line_end: int
    original: str
    suggestion: str


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
    """Create a non-overwriting backup path next to a source file."""
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
    """Split text by commas outside quotes/parentheses."""
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


def split_top_level_equals(text: str) -> Optional[Tuple[str, str]]:
    """Split key=value at first top-level equals sign, if any."""
    depth = 0
    in_single = False
    in_double = False
    i = 0
    while i < len(text):
        ch = text[i]
        if ch == "'" and not in_double:
            if in_single and i + 1 < len(text) and text[i + 1] == "'":
                i += 2
                continue
            in_single = not in_single
            i += 1
            continue
        if ch == '"' and not in_single:
            if in_double and i + 1 < len(text) and text[i + 1] == '"':
                i += 2
                continue
            in_double = not in_double
            i += 1
            continue
        if not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")" and depth > 0:
                depth -= 1
            elif ch == "=" and depth == 0:
                return text[:i].strip(), text[i + 1 :].strip()
        i += 1
    return None


def find_matching_paren(text: str, start_idx: int) -> int:
    """Return index of matching ')' for '(' at start_idx, else -1."""
    depth = 0
    in_single = False
    in_double = False
    i = start_idx
    while i < len(text):
        ch = text[i]
        if ch == "'" and not in_double:
            if in_single and i + 1 < len(text) and text[i + 1] == "'":
                i += 2
                continue
            in_single = not in_single
            i += 1
            continue
        if ch == '"' and not in_single:
            if in_double and i + 1 < len(text) and text[i + 1] == '"':
                i += 2
                continue
            in_double = not in_double
            i += 1
            continue
        if not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
                if depth == 0:
                    return i
        i += 1
    return -1


def is_no_literal(expr: str) -> bool:
    """Return True when expr is a string literal equal to NO (case-insensitive)."""
    s = expr.strip()
    if len(s) < 2:
        return False
    if (s[0] == "'" and s[-1] == "'") or (s[0] == '"' and s[-1] == '"'):
        val = s[1:-1].strip().lower()
        return val == "no"
    return False


def collapse_fmt_expr(fmt_expr: str) -> str:
    """Convert literal '(...)' format to '(*( ... ))' for array/implied-do write."""
    s = fmt_expr.strip()
    if len(s) >= 2 and ((s[0] == "'" and s[-1] == "'") or (s[0] == '"' and s[-1] == '"')):
        q = s[0]
        inner = s[1:-1].strip()
        if inner.startswith("(") and inner.endswith(")"):
            body = inner[1:-1].strip()
            return f'{q}(*({body})){q}'
    return fmt_expr


def parse_write_nonadv(stmt: str) -> Optional[Tuple[str, str, List[str]]]:
    """Parse `write` with advance='no'; return (unit, fmt, items) when safe."""
    s = stmt.strip()
    if not WRITE_RE.match(s):
        return None
    pos_write = s.lower().find("write")
    pos_lpar = s.find("(", pos_write)
    if pos_lpar < 0:
        return None
    pos_rpar = find_matching_paren(s, pos_lpar)
    if pos_rpar < 0:
        return None

    ctl = s[pos_lpar + 1 : pos_rpar]
    items = s[pos_rpar + 1 :].strip()
    if not items:
        return None
    out_items = [it.strip() for it in split_top_level_commas(items)]
    if not out_items or any(not it for it in out_items):
        return None

    ctl_parts = split_top_level_commas(ctl)
    if not ctl_parts:
        return None

    unit_expr: Optional[str] = None
    fmt_expr: Optional[str] = None
    adv_is_no = False
    other_keys: List[str] = []
    positional: List[str] = []

    for p in ctl_parts:
        kv = split_top_level_equals(p)
        if kv is None:
            positional.append(p.strip())
            continue
        key, val = kv
        k = key.strip().lower()
        if k == "unit":
            unit_expr = val.strip()
        elif k == "fmt":
            fmt_expr = val.strip()
        elif k == "advance":
            adv_is_no = is_no_literal(val)
        else:
            other_keys.append(k)

    if unit_expr is None and positional:
        unit_expr = positional[0]
    if fmt_expr is None and len(positional) >= 2:
        fmt_expr = positional[1]
    if unit_expr is None or fmt_expr is None:
        return None
    if not adv_is_no:
        return None
    if other_keys:
        return None
    return unit_expr, fmt_expr, out_items


def build_replacement(loop_var: str, lb: str, ub: str, write_item: str) -> str:
    """Build replacement output list using array section or implied-DO."""
    m = re.match(rf"^\s*([a-z][a-z0-9_]*)\s*\(\s*{re.escape(loop_var)}\s*\)\s*$", write_item, re.IGNORECASE)
    if m:
        arr = m.group(1)
        return f"{arr}({lb}:{ub})"
    return f"({write_item}, {loop_var} = {lb}, {ub})"


def _format_implied_do_bound(expr: str) -> str:
    """Format implied-do bounds for readability (avoid ambiguous `m*n-1` style)."""
    s = re.sub(r"\s+", " ", expr.strip())
    # Clarify trailing +/- literal offsets, e.g. `m * n-1` -> `m * n - 1`.
    m = re.match(r"^(.*\S)\s*([+\-])\s*([0-9]+)\s*$", s)
    if m:
        lhs, op, rhs = m.group(1).strip(), m.group(2), m.group(3)
        s = f"{lhs} {op} {rhs}"
    return s


def _is_zero_literal(expr: str) -> bool:
    return expr.strip() == "0"


def _strip_outer_parens(expr: str) -> str:
    s = expr.strip()
    while len(s) >= 2 and s[0] == "(" and s[-1] == ")":
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
        if not ok:
            break
        s = s[1:-1].strip()
    return s


def _plus_one_expr(expr: str) -> str:
    """Return expression equivalent to (expr + 1), simplified when obvious."""
    s = _strip_outer_parens(expr)
    m = re.match(r"^(.*\S)\s*-\s*1\s*$", s)
    if m:
        return m.group(1).strip()
    m2 = re.match(r"^(.*\S)\s*\+\s*1\s*$", s)
    if m2:
        return f"{m2.group(1).strip()} + 2"
    return f"{s} + 1"


def _normalize_offset_implied_do(write_item: str, loop_var: str, lb: str, ub: str) -> Tuple[str, str, str]:
    """Normalize `(i+1)` style indexing with 0-based loop bounds to 1-based form."""
    if not _is_zero_literal(lb):
        return write_item, lb, ub
    pat = re.compile(rf"\b{re.escape(loop_var)}\s*\+\s*1\b", re.IGNORECASE)
    if not pat.search(write_item):
        return write_item, lb, ub
    new_item = pat.sub(loop_var, write_item)
    new_lb = "1"
    new_ub = _plus_one_expr(ub)
    return new_item, new_lb, new_ub


def _is_string_literal(expr: str) -> bool:
    s = expr.strip()
    return len(s) >= 2 and ((s[0] == "'" and s[-1] == "'") or (s[0] == '"' and s[-1] == '"'))


def _mentions_loop_var(expr: str, loop_var: str) -> bool:
    return re.search(rf"\b{re.escape(loop_var)}\b", expr, re.IGNORECASE) is not None


def maybe_finding(path: Path, loop: LoopCtx, end_line: int) -> Optional[Finding]:
    """Return finding when loop matches the supported non-advancing WRITE pattern."""
    if len(loop.body) != 1:
        return None
    wline, wstmt = loop.body[0]
    parsed = parse_write_nonadv(wstmt)
    if parsed is None:
        return None
    unit_expr, fmt_expr, out_items = parsed
    replacement_item: Optional[str] = None
    lb_raw = loop.lb
    ub_raw = loop.ub
    if len(out_items) == 1:
        item_raw = out_items[0]
        item_raw, lb_raw, ub_raw = _normalize_offset_implied_do(item_raw, loop.var, lb_raw, ub_raw)
        lb_fmt = _format_implied_do_bound(lb_raw)
        ub_fmt = _format_implied_do_bound(ub_raw)
        replacement_item = build_replacement(loop.var, lb_fmt, ub_fmt, item_raw)
    elif len(out_items) == 2 and _is_string_literal(out_items[0]) and _mentions_loop_var(out_items[1], loop.var):
        # Common pattern: write(..., advance='no') " ", item(i)
        item_raw = out_items[1]
        item_raw, lb_raw, ub_raw = _normalize_offset_implied_do(item_raw, loop.var, lb_raw, ub_raw)
        lb_fmt = _format_implied_do_bound(lb_raw)
        ub_fmt = _format_implied_do_bound(ub_raw)
        replacement_item = f"({out_items[0]}, {item_raw}, {loop.var} = {lb_fmt}, {ub_fmt})"
    if replacement_item is None:
        return None
    suggestion = f"write ({unit_expr}, {collapse_fmt_expr(fmt_expr)}) {replacement_item}"
    return Finding(
        path=path,
        line_start=loop.start_line,
        line_end=end_line,
        original=wstmt.strip(),
        suggestion=suggestion,
    )


def analyze_file(path: Path) -> List[Finding]:
    """Analyze one file for non-advancing WRITE loops that can be collapsed."""
    infos, any_missing = fscan.load_source_files([path])
    if not infos or any_missing:
        return []
    finfo = infos[0]
    findings: List[Finding] = []
    stack: List[LoopCtx] = []

    for lineno, stmt in fscan.iter_fortran_statements(finfo.parsed_lines):
        s = stmt.strip()
        m_do = DO_INDEX_RE.match(s)
        if m_do:
            if stack:
                stack[-1].body.append((lineno, s))
            stack.append(
                LoopCtx(
                    var=m_do.group(1).lower(),
                    lb=m_do.group(2).strip(),
                    ub=m_do.group(3).strip(),
                    start_line=lineno,
                    body=[],
                )
            )
            continue

        if END_DO_RE.match(s):
            if not stack:
                continue
            top = stack.pop()
            found = maybe_finding(path, top, lineno)
            if found is not None:
                findings.append(found)
            if stack:
                stack[-1].body.append((lineno, s))
            continue

        if stack:
            stack[-1].body.append((lineno, s))

    return findings


def annotate_file(path: Path, findings: List[Finding]) -> Tuple[int, Optional[Path]]:
    """Insert suggestion comments after matching end-do lines."""
    if not findings:
        return 0, None
    lines = path.read_text(encoding="utf-8").splitlines(keepends=True)
    inserts: List[Tuple[int, str]] = []

    for f in findings:
        idx = f.line_end - 1
        if idx < 0 or idx >= len(lines):
            continue
        raw = lines[idx]
        indent = re.match(r"^\s*", raw).group(0) if raw else ""
        eol = "\r\n" if raw.endswith("\r\n") else ("\n" if raw.endswith("\n") else "\n")
        msg = f"{indent}! {f.suggestion}  !! suggested by xadvance.py{eol}"
        nxt = idx + 1
        if 0 <= nxt < len(lines) and lines[nxt].strip().lower() == msg.strip().lower():
            continue
        inserts.append((idx + 1, msg))

    if not inserts:
        return 0, None

    backup = make_backup_path(path)
    shutil.copy2(path, backup)
    for at, msg in sorted(inserts, key=lambda x: x[0], reverse=True):
        lines.insert(at, msg)
    path.write_text("".join(lines), encoding="utf-8")
    return len(inserts), backup


def rewrite_text_collapse_nonadv_write_loops(text: str) -> Tuple[str, int]:
    """Rewrite collapsible non-advancing WRITE loops in source text.

    Returns (new_text, rewritten_count).
    """
    fd, tmp_name = tempfile.mkstemp(suffix=".f90", prefix="xadvance_")
    os.close(fd)
    tmp_path = Path(tmp_name)
    try:
        tmp_path.write_text(text, encoding="utf-8")
        findings = analyze_file(tmp_path)
        if not findings:
            return text, 0
        lines = tmp_path.read_text(encoding="utf-8").splitlines(keepends=True)
        applied = 0
        for f in sorted(findings, key=lambda x: (x.line_start, x.line_end), reverse=True):
            s = f.line_start - 1
            e = f.line_end - 1
            if s < 0 or e >= len(lines) or s > e:
                continue
            raw = lines[s]
            indent = re.match(r"^\s*", raw).group(0) if raw else ""
            eol = "\r\n" if raw.endswith("\r\n") else ("\n" if raw.endswith("\n") else "\n")
            lines[s : e + 1] = [f"{indent}{f.suggestion}{eol}"]
            applied += 1
        return "".join(lines), applied
    finally:
        try:
            tmp_path.unlink(missing_ok=True)
        except Exception:
            pass


def main() -> int:
    """Run advisory non-advancing-write loop checks across selected files."""
    parser = argparse.ArgumentParser(
        description="Warn when non-advancing WRITE loops can be replaced by one array/implied-do WRITE"
    )
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--verbose", action="store_true", help="Print original loop-write statement")
    parser.add_argument("--annotate", action="store_true", help="Insert suggested replacement comments after loops")
    args = parser.parse_args()

    files = choose_files(args.fortran_files, args.exclude)
    if not files:
        print("No source files remain after applying --exclude filters.")
        return 2

    findings: List[Finding] = []
    for p in files:
        findings.extend(analyze_file(p))

    if not findings:
        print("No collapsible non-advancing WRITE loops found.")
        return 0

    findings.sort(key=lambda f: (f.path.name.lower(), f.line_start, f.line_end))
    print(f"{len(findings)} collapsible non-advancing WRITE loop(s).")
    for f in findings:
        print(f"{f.path.name}:{f.line_start}-{f.line_end} -> {f.suggestion}")
        if args.verbose:
            print(f"  loop write: {f.original}")

    if args.annotate:
        by_file: dict[Path, List[Finding]] = {}
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
