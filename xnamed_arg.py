#!/usr/bin/env python3
"""Rewrite Fortran calls to use named arguments for clarity.

Rules:
- name any literal positional argument
- name any positional argument whose dummy is OPTIONAL
- if unnamed positional args exceed --max-positional (default 3), name args beyond that threshold
"""

from __future__ import annotations

import argparse
from pathlib import Path
from typing import Dict, List, Set

import fortran_scan as fscan


def _rewrite_file(
    path: Path,
    *,
    max_positional: int,
    max_len: int,
) -> tuple[List[str], int, Set[str]]:
    raw_lines = path.read_text(encoding="utf-8", errors="replace").splitlines(keepends=True)
    out_lines, n_changes, unavailable = fscan.rewrite_named_arguments_in_lines(
        raw_lines, max_positional=max_positional
    )

    # Post-process to wrap long Fortran lines conservatively.
    # Normalize line endings first to avoid corrupting existing continuation lines.
    code_lines = [ln.rstrip("\r\n") for ln in out_lines]
    wrapped = fscan.wrap_long_declaration_lines(code_lines, max_len=max_len)
    wrapped = fscan.wrap_long_fortran_lines(wrapped, max_len=max_len)
    final_lines = [ln if ln.endswith("\n") else (ln + "\n") for ln in wrapped]
    return final_lines, n_changes, unavailable


def main() -> int:
    ap = argparse.ArgumentParser(description="Name literal/optional/late positional Fortran call arguments")
    ap.add_argument("files", nargs="+", help="Fortran source files that comprise the program")
    ap.add_argument("--max-positional", type=int, default=3, help="max unnamed positional args before naming (default 3)")
    ap.add_argument("--max-len", type=int, default=80, help="max line length for wrapped output (default 80)")
    ap.add_argument("--fix", action="store_true", help="rewrite files in place")
    ap.add_argument("--out-dir", help="write rewritten files to this directory (default: no write unless --fix)")
    args = ap.parse_args()

    paths = [Path(f) for f in args.files]
    for p in paths:
        if not p.exists():
            print(f"Missing file: {p}")
            return 1

    total_changes = 0
    all_unavailable: Set[str] = set()
    rewritten: Dict[Path, List[str]] = {}

    for p in paths:
        new_lines, n_changes, unavailable = _rewrite_file(
            p,
            max_positional=max(0, args.max_positional),
            max_len=max(20, args.max_len),
        )
        rewritten[p] = new_lines
        total_changes += n_changes
        all_unavailable.update(unavailable)

    # Report unavailable interfaces once.
    for nm in sorted(all_unavailable):
        print(f"procedure {nm} interface unavailable")

    if args.fix:
        for p, lines in rewritten.items():
            p.write_text("".join(lines), encoding="utf-8")
        print(f"Applied {total_changes} rewrite(s) across {len(paths)} file(s).")
        return 0

    if args.out_dir:
        out_dir = Path(args.out_dir)
        out_dir.mkdir(parents=True, exist_ok=True)
        for p, lines in rewritten.items():
            (out_dir / p.name).write_text("".join(lines), encoding="utf-8")
        print(f"Wrote rewritten files to {out_dir} ({total_changes} rewrite(s)).")
        return 0

    print(f"Planned {total_changes} rewrite(s) across {len(paths)} file(s). Use --fix or --out-dir to write.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
