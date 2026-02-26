#!/usr/bin/env python3
from __future__ import annotations

import argparse
import glob
from pathlib import Path
from typing import List

import fortran_scan as fscan


def _expand_inputs(items: List[str]) -> List[Path]:
    out: List[Path] = []
    for it in items:
        hits = glob.glob(it)
        if hits:
            out.extend(Path(h) for h in hits)
        else:
            out.append(Path(it))
    seen = set()
    uniq: List[Path] = []
    for p in out:
        key = str(p.resolve()) if p.exists() else str(p)
        if key in seen:
            continue
        seen.add(key)
        uniq.append(p)
    return uniq


def main() -> int:
    ap = argparse.ArgumentParser(
        description=(
            "Coalesce adjacent Fortran declarations and contiguous "
            "PUBLIC/PRIVATE entity lists."
        )
    )
    ap.add_argument("inputs", nargs="+", help="Fortran source files (supports globs)")
    ap.add_argument("--fix", action="store_true", help="Rewrite files in place")
    ap.add_argument("--out", help="Write output to this file (single input only)")
    ap.add_argument("--max-len", type=int, default=80, help="Maximum line length (default: 80)")
    args = ap.parse_args()

    paths = _expand_inputs(args.inputs)
    missing = [p for p in paths if not p.exists()]
    for p in missing:
        print(f"Missing file: {p}")
    if missing:
        return 1

    if args.out and len(paths) != 1:
        print("--out requires exactly one input file.")
        return 2
    if args.max_len < 20:
        print("--max-len should be at least 20.")
        return 2

    changed = 0
    for p in paths:
        src = p.read_text(encoding="utf-8")
        lines = src.splitlines(True)
        new_lines = fscan.coalesce_simple_declarations(lines, max_len=args.max_len)
        dst = "".join(new_lines)
        if dst != src:
            changed += 1
        if args.fix:
            p.write_text(dst, encoding="utf-8")
        elif args.out:
            Path(args.out).write_text(dst, encoding="utf-8")
        elif len(paths) == 1:
            print(dst, end="")

    if args.fix:
        print(f"Applied coalescing to {changed} file(s).")
    elif not args.out and len(paths) > 1:
        print(f"Checked {len(paths)} file(s); {changed} would change. Use --fix to apply.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
