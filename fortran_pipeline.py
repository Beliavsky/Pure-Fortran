#!/usr/bin/env python3
"""Shared pipeline helpers for Fortran transformation wrappers."""

from __future__ import annotations

import subprocess
from pathlib import Path
from typing import List, Sequence, Tuple

import cli_paths as cpaths
import fortran_scan as fscan


def auto_files() -> List[Path]:
    """Return all .f90/.F90 files in the current directory in stable name order."""
    return sorted(
        set(Path(".").glob("*.f90")) | set(Path(".").glob("*.F90")),
        key=lambda p: p.name.lower(),
    )


def resolve_ordered_files(
    files: Sequence[Path],
    exclude: Sequence[str],
) -> Tuple[List[Path], int]:
    """Resolve input files, apply excludes, and return dependency-ordered paths."""
    selected = cpaths.expand_path_args(files) if files else auto_files()
    selected = fscan.apply_excludes(selected, exclude)
    if not selected:
        print("No source files provided (or all were excluded).")
        return [], 2

    infos, any_missing = fscan.load_source_files(selected)
    if not infos:
        return [], (2 if any_missing else 1)
    ordered, _ = fscan.order_files_least_dependent(infos)
    return [f.path for f in ordered], 0


def build_edit_phase_flags(args: object) -> List[str]:
    """Build common --fix phase CLI flags from parsed wrapper arguments."""
    flags: List[str] = ["--fix", "--iterate", "--max-iter", str(args.max_iter)]
    if args.compiler:
        flags += ["--compiler", args.compiler]
    if args.diff:
        flags += ["--diff"]
    if args.verbose:
        flags += ["--verbose"]
    if args.backup:
        flags += ["--backup"]
    else:
        flags += ["--no-backup"]
    if getattr(args, "git", False):
        flags += ["--git"]
    return flags


def run_phase(label: str, cmd: List[str]) -> int:
    """Execute one pipeline phase command and print a labeled header."""
    print(f"\n=== {label} ===", flush=True)
    print("Command:", " ".join(cmd), flush=True)
    proc = subprocess.run(cmd)
    return proc.returncode
