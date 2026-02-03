#!/usr/bin/env python3
"""Run intent then pure/elemental passes in one pipeline."""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

import fortran_pipeline as fpipeline


def main() -> int:
    """Run intent then pure and optional elemental passes over ordered source files."""
    parser = argparse.ArgumentParser(
        description="Apply intent(in/out), then pure, then optional elemental passes"
    )
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument(
        "--exclude",
        action="append",
        default=[],
        help="Glob pattern to exclude files (can be repeated)",
    )
    parser.add_argument("--compiler", type=str, help="Compilation command for each phase")
    parser.add_argument("--backup", dest="backup", action="store_true", default=True)
    parser.add_argument("--no-backup", dest="backup", action="store_false")
    parser.add_argument("--diff", action="store_true")
    parser.add_argument("--verbose", action="store_true")
    parser.add_argument("--max-iter", type=int, default=10)
    parser.add_argument("--strict-unknown-calls", action="store_true")
    parser.add_argument("--suggest-intent-out", action="store_true")
    parser.add_argument("--suggest-elemental", action="store_true")
    parser.add_argument("--git", action="store_true", help="Commit phase changes via underlying tools")
    args = parser.parse_args()

    ordered_files, rc = fpipeline.resolve_ordered_files(args.fortran_files, args.exclude)
    if rc != 0:
        return rc

    common_flags = fpipeline.build_edit_phase_flags(args)

    py = sys.executable

    intent_flags = common_flags[:]
    intent_cmd = [py, "xintent.py", *[str(p) for p in ordered_files], *intent_flags]
    if args.suggest_intent_out:
        intent_cmd += ["--suggest-intent-out"]
    rc = fpipeline.run_phase("Intent Phase", intent_cmd)
    if rc != 0:
        return rc

    pure_flags = common_flags[:]
    if args.strict_unknown_calls:
        pure_flags += ["--strict-unknown-calls"]
    pure_cmd = [py, "xpure.py", *[str(p) for p in ordered_files], *pure_flags]
    rc = fpipeline.run_phase("Pure Phase", pure_cmd)
    if rc != 0:
        return rc

    if args.suggest_elemental:
        elemental_cmd = [
            py,
            "xpure.py",
            *[str(p) for p in ordered_files],
            *pure_flags,
            "--suggest-elemental",
        ]
        rc = fpipeline.run_phase("Elemental Phase", elemental_cmd)
        if rc != 0:
            return rc

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
