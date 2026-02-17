#!/usr/bin/env python3
"""Index procedure definitions across Fortran sources."""

from __future__ import annotations

import argparse
from pathlib import Path
from typing import Dict, List, Set

import cli_paths as cpaths
import fortran_scan as fscan


def choose_files(inputs: List[Path], exclude: List[str]) -> List[Path]:
    """Resolve file inputs with wildcard and directory expansion."""
    files = cpaths.expand_source_inputs(inputs)
    if exclude:
        keep: List[Path] = []
        for p in files:
            s = str(p).lower()
            matched = False
            for pat in exclude:
                if Path(s).match(pat.lower()) or Path(p.name.lower()).match(pat.lower()):
                    matched = True
                    break
            if not matched:
                keep.append(p)
        files = keep
    return files


def main() -> int:
    parser = argparse.ArgumentParser(
        description=(
            "Scan Fortran source files and index procedure names, "
            "highlighting duplicate definitions"
        )
    )
    parser.add_argument("fortran_files", type=Path, nargs="*", help="Files, globs, and/or directories")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude")
    parser.add_argument(
        "--all",
        action="store_true",
        help="List all procedure names (default lists only duplicates)",
    )
    parser.add_argument(
        "--top-level-only",
        action="store_true",
        help="Only include top-level procedures (exclude internal procedures)",
    )
    parser.add_argument(
        "--show-files",
        action="store_true",
        help="Also print defining files for each name",
    )
    parser.add_argument(
        "--ascending",
        action="store_true",
        help="Sort by count ascending (default descending)",
    )
    parser.add_argument(
        "--limit",
        type=int,
        help="Maximum number of rows to print",
    )
    args = parser.parse_args()

    files = choose_files(args.fortran_files, args.exclude)
    if not files:
        print("No source files found.")
        return 2

    proc_to_files: Dict[str, Set[Path]] = {}
    proc_to_kinds: Dict[str, Set[str]] = {}

    infos, any_missing = fscan.load_source_files(files)
    if any_missing and not infos:
        print("No readable source files found.")
        return 2

    for finfo in infos:
        for p in finfo.procedures:
            if args.top_level_only and p.parent is not None:
                continue
            name = p.name.lower()
            proc_to_files.setdefault(name, set()).add(finfo.path)
            proc_to_kinds.setdefault(name, set()).add(p.kind.lower())

    rows = []
    for name, srcs in proc_to_files.items():
        count = len(srcs)
        if not args.all and count < 2:
            continue
        kinds = "/".join(sorted(proc_to_kinds.get(name, set())))
        rows.append((name, kinds, count, sorted(srcs, key=lambda p: str(p).lower())))

    if not rows:
        if args.all:
            print("No procedures found.")
        else:
            print("No duplicate procedure names found.")
        return 0

    rows.sort(key=lambda r: (r[2], r[0]) if args.ascending else (-r[2], r[0]))
    if args.limit is not None and args.limit >= 0:
        rows = rows[: args.limit]

    w_name = max(len("name"), max(len(r[0]) for r in rows))
    w_kind = max(len("kind"), max(len(r[1]) for r in rows))
    w_count = max(len("count"), max(len(str(r[2])) for r in rows))

    print(f"{'name':<{w_name}}  {'kind':<{w_kind}}  {'count':>{w_count}}")
    for name, kinds, count, srcs in rows:
        print(f"{name:<{w_name}}  {kinds:<{w_kind}}  {count:>{w_count}}")
        if args.show_files:
            for s in srcs:
                print(f"  - {s}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
