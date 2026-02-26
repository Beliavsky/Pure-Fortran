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
        matches = glob.glob(it)
        if matches:
            out.extend(Path(m) for m in matches)
        else:
            out.append(Path(it))
    seen = set()
    uniq: List[Path] = []
    for p in out:
        key = str(p.resolve()) if p.exists() else str(p)
        if key not in seen:
            seen.add(key)
            uniq.append(p)
    return uniq


def main() -> int:
    ap = argparse.ArgumentParser(
        description=(
            "Indent Fortran code: no extra body indent for program/module/procedures; "
            "3-space indent for block constructs and derived-type components; "
            "wrap overlong lines."
        )
    )
    ap.add_argument("inputs", nargs="+", help="Fortran source files (supports globs)")
    ap.add_argument("--fix", action="store_true", help="Rewrite files in place")
    ap.add_argument("--out", help="Write output to this file (single input only)")
    ap.add_argument("--indent", type=int, default=3, help="Indent size (default: 3)")
    ap.add_argument("--indent-proc", action="store_true", help="Indent function/subroutine bodies")
    ap.add_argument("--indent-module", action="store_true", help="Indent module bodies")
    ap.add_argument("--indent-program", action="store_true", help="Indent main program bodies")
    ap.add_argument("--indent-contains", action="store_true", help="Indent bodies under CONTAINS")
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
    for path in paths:
        src = path.read_text(encoding="utf-8")
        dst = fscan.indent_fortran_blocks(
            src,
            indent_step=args.indent,
            indent_proc=args.indent_proc,
            indent_module=args.indent_module,
            indent_program=args.indent_program,
            indent_contains=args.indent_contains,
        )
        lines = dst.splitlines()
        lines = fscan.wrap_long_declaration_lines(lines, max_len=args.max_len)
        lines = fscan.wrap_long_fortran_lines(lines, max_len=args.max_len)
        dst = "\n".join(lines) + ("\n" if src.endswith("\n") and lines else "")
        if dst != src:
            changed += 1
        if args.fix:
            path.write_text(dst, encoding="utf-8")
        elif args.out:
            Path(args.out).write_text(dst, encoding="utf-8")
        elif len(paths) == 1:
            print(dst, end="")

    if args.fix:
        print(f"Applied indentation to {changed} file(s).")
    elif not args.out and len(paths) > 1:
        print(f"Checked {len(paths)} file(s); {changed} would change. Use --fix to apply.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
