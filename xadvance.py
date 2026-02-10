#!/usr/bin/env python3
"""Warn about non-advancing WRITE loops that can be collapsed to one array WRITE."""

from __future__ import annotations

import argparse
import re
import shutil
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


def parse_write_nonadv(stmt: str) -> Optional[Tuple[str, str, str]]:
    """Parse `write` with advance='no'; return (unit, fmt, item) when safe."""
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
    out_items = split_top_level_commas(items)
    if len(out_items) != 1:
        return None
    out_item = out_items[0].strip()
    if not out_item:
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
    return unit_expr, fmt_expr, out_item


def build_replacement(loop_var: str, lb: str, ub: str, write_item: str) -> str:
    """Build replacement output list using array section or implied-DO."""
    m = re.match(rf"^\s*([a-z][a-z0-9_]*)\s*\(\s*{re.escape(loop_var)}\s*\)\s*$", write_item, re.IGNORECASE)
    if m:
        arr = m.group(1)
        return f"{arr}({lb}:{ub})"
    return f"({write_item}, {loop_var} = {lb}, {ub})"


def maybe_finding(path: Path, loop: LoopCtx, end_line: int) -> Optional[Finding]:
    """Return finding when loop matches the supported non-advancing WRITE pattern."""
    if len(loop.body) != 1:
        return None
    wline, wstmt = loop.body[0]
    parsed = parse_write_nonadv(wstmt)
    if parsed is None:
        return None
    unit_expr, fmt_expr, out_item = parsed
    replacement_item = build_replacement(loop.var, loop.lb, loop.ub, out_item)
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
