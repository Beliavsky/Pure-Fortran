#!/usr/bin/env python3
"""Count goto-like statements in Fortran source trees.

Usage:
  python xcount_goto.py <dir1>
  python xcount_goto.py <dir1> <dir2>

Counts include:
- GO TO <label>
- IF (...) GO TO <label>
- computed GO TO (...), expr
- ASSIGN <label> TO var
"""

from __future__ import annotations

import argparse
import re
from pathlib import Path
from typing import Dict, Iterable, Tuple

GOTO_RE = re.compile(r"^\s*go\s*to\s+\d+\s*$", re.IGNORECASE)
IF_GOTO_RE = re.compile(r"^\s*(?:\d+\s+)?if\s*\(.+\)\s*go\s*to\s+\d+\s*$", re.IGNORECASE)
ASSIGNED_GOTO_RE = re.compile(r"^\s*go\s*to\s*\([^)]*\)\s*,\s*[a-z][a-z0-9_]*\s*$", re.IGNORECASE)
ASSIGN_RE = re.compile(r"^\s*assign\s+\d+\s+to\s+[a-z][a-z0-9_]*\s*$", re.IGNORECASE)
LABEL_LINE_RE = re.compile(r"^(?P<indent>\s*)(?P<label>\d+)(?P<sep>\s+)(?P<stmt>.*)$")


def split_code_comment(line: str) -> Tuple[str, str]:
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


def parse_labeled_stmt(code: str) -> str:
    m = LABEL_LINE_RE.match(code)
    if m:
        return m.group("stmt")
    return code


def count_file(path: Path) -> int:
    count = 0
    for raw in path.read_text(encoding="utf-8", errors="ignore").splitlines():
        code, _comment = split_code_comment(raw)
        stmt = parse_labeled_stmt(code).strip()
        if not stmt:
            continue
        if GOTO_RE.match(stmt) or IF_GOTO_RE.match(stmt) or ASSIGNED_GOTO_RE.match(stmt) or ASSIGN_RE.match(stmt):
            count += 1
    return count


def list_f90(dir_path: Path) -> Iterable[Path]:
    files = list(dir_path.glob("*.f90")) + list(dir_path.glob("*.F90"))
    return sorted(set(files), key=lambda p: p.name.lower())


def count_dir(dir_path: Path) -> Dict[str, int]:
    out: Dict[str, int] = {}
    for p in list_f90(dir_path):
        out[p.name] = count_file(p)
    return out


def main() -> int:
    ap = argparse.ArgumentParser(description="Count goto-like statements in .f90 files.")
    ap.add_argument("dir1", type=Path, help="First source directory")
    ap.add_argument("dir2", type=Path, nargs="?", help="Optional second directory for comparison")
    ap.add_argument("--only-changed", action="store_true", help="With dir2, show only files where counts differ")
    ap.add_argument(
        "--include-missing",
        action="store_true",
        help="With dir2, include files missing from either directory (default: compare only matched filenames).",
    )
    args = ap.parse_args()

    if not args.dir1.is_dir():
        print(f"Not a directory: {args.dir1}")
        return 2
    if args.dir2 is not None and not args.dir2.is_dir():
        print(f"Not a directory: {args.dir2}")
        return 2

    c1 = count_dir(args.dir1)

    if args.dir2 is None:
        total = 0
        for name in sorted(c1):
            n = c1[name]
            total += n
            print(f"{name}: {n}")
        print(f"TOTAL {args.dir1}: {total} ({len(c1)} file(s))")
        return 0

    c2 = count_dir(args.dir2)
    names = sorted(set(c1) | set(c2)) if args.include_missing else sorted(set(c1) & set(c2))
    t1 = 0
    t2 = 0
    missing2 = 0
    missing1 = 0
    for name in names:
        n1 = c1.get(name)
        n2 = c2.get(name)
        if n1 is None:
            missing1 += 1
            print(f"{name}: only in {args.dir2}")
            continue
        if n2 is None:
            missing2 += 1
            t1 += n1
            print(f"{name}: {n1} -> missing in {args.dir2}")
            continue
        t1 += n1
        t2 += n2
        if args.only_changed and n1 == n2:
            continue
        print(f"{name}: {n1} -> {n2} ({n2 - n1:+d})")

    print(f"TOTAL {args.dir1}: {t1}")
    print(f"TOTAL {args.dir2}: {t2}")
    print(f"DELTA: {t2 - t1:+d}")
    if args.include_missing and (missing2 or missing1):
        print(f"Missing: in {args.dir2} = {missing2}, in {args.dir1} = {missing1}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
