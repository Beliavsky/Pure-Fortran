#!/usr/bin/env python3
"""xlegal.py: validate Fortran source legality."""

from __future__ import annotations

import argparse
import glob
import subprocess
from pathlib import Path
from typing import List

import fortran_scan as fscan


def _expand_inputs(inputs: List[str]) -> List[Path]:
    out: List[Path] = []
    seen = set()
    exts = (".f90", ".f95", ".f03", ".f08", ".f", ".for")
    for raw in inputs:
        p = Path(raw)
        cands: List[Path] = []
        if any(ch in raw for ch in "*?[]"):
            cands = [Path(s) for s in glob.glob(raw)]
        elif p.is_dir():
            for ext in exts:
                cands.extend(sorted(p.glob(f"*{ext}")))
        else:
            cands = [p]
        for c in cands:
            if c.exists() and c.is_file():
                k = str(c.resolve()).lower()
                if k not in seen:
                    seen.add(k)
                    out.append(c)
    return out


def _run_syntax_check(path: Path, compiler_cmd: str) -> List[str]:
    cmd = compiler_cmd.replace("{file}", str(path))
    cp = subprocess.run(cmd, capture_output=True, text=True, shell=True)
    if cp.returncode == 0:
        return []
    msgs: List[str] = []
    if cp.stdout.strip():
        msgs.extend(cp.stdout.rstrip().splitlines())
    if cp.stderr.strip():
        msgs.extend(cp.stderr.rstrip().splitlines())
    if not msgs:
        msgs.append(f"syntax check failed (exit {cp.returncode})")
    return msgs


def main() -> int:
    ap = argparse.ArgumentParser(description="Validate Fortran source files.")
    ap.add_argument("fortran_files", nargs="*", help="Fortran files, glob patterns, or directories.")
    ap.add_argument("--syntax", action="store_true", help="Also run compiler syntax check.")
    ap.add_argument(
        "--compiler-cmd",
        default="gfortran -fsyntax-only {file}",
        help="Compiler command template for --syntax; use {file}.",
    )
    ap.add_argument("--no-basic", action="store_true", help="Disable fast shared static checks.")
    ap.add_argument("--verbose", action="store_true", help="Show passing files too.")
    args = ap.parse_args()

    files = _expand_inputs(args.fortran_files or ["*.f90"])
    if not files:
        print("No Fortran files found.")
        return 1

    total_errs = 0
    files_with_errs = 0
    for p in files:
        text = p.read_text(encoding="utf-8", errors="ignore")
        errs: List[str] = []
        if not args.no_basic:
            errs.extend(fscan.validate_fortran_basic_statements(text))
            errs.extend(fscan.find_implicit_none_undeclared_identifiers(text))
            errs.extend(fscan.find_duplicate_procedure_definitions(text))
            errs.extend(fscan.find_duplicate_declarations(text))
        if args.syntax:
            syn = _run_syntax_check(p, args.compiler_cmd)
            errs.extend([f"syntax: {m}" for m in syn])

        if errs:
            files_with_errs += 1
            total_errs += len(errs)
            print(f"{p}: {len(errs)} issue(s)")
            for e in errs:
                print(f"  {e}")
        elif args.verbose:
            print(f"{p}: OK")

    if files_with_errs == 0:
        print(f"All {len(files)} file(s) passed.")
        return 0
    print(f"Validation failed: {total_errs} issue(s) in {files_with_errs} file(s).")
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
