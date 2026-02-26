#!/usr/bin/env python3
from __future__ import annotations

import argparse
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import List

import cli_paths as cpaths
import fortran_scan as fscan


def _read_lines(path: Path) -> List[str]:
    return fscan.read_text_flexible(path).splitlines(keepends=True)


def _print_text_stdout(text: str) -> None:
    text_out = text[1:] if text.startswith("\ufeff") else text
    try:
        print(text_out, end="")
    except UnicodeEncodeError:
        sys.stdout.buffer.write(text_out.encode("utf-8", errors="replace"))
    if not text_out.endswith("\n"):
        print()


def _compile_and_run(files: List[Path], *, label: str) -> int:
    exe = files[-1].with_suffix(f".{label}.exe")
    cmd = ["gfortran"] + [str(p) for p in files] + ["-o", str(exe)]
    print(f"Build ({label}): {' '.join(cmd)}")
    cp = subprocess.run(cmd, capture_output=True, text=True)
    if cp.returncode != 0:
        print(f"Build ({label}): FAIL (exit {cp.returncode})")
        if cp.stdout:
            print(cp.stdout.rstrip())
        if cp.stderr:
            print(cp.stderr.rstrip())
        return cp.returncode
    print(f"Build ({label}): PASS")
    print(f"Run ({label}): {exe.name}")
    rp = subprocess.run([str(exe)], capture_output=True, text=True)
    if rp.returncode != 0:
        print(f"Run ({label}): FAIL (exit {rp.returncode})")
        if rp.stdout:
            print(rp.stdout.rstrip())
        if rp.stderr:
            print(rp.stderr.rstrip())
        return rp.returncode
    print(f"Run ({label}): PASS")
    if rp.stdout:
        print(rp.stdout.rstrip())
    if rp.stderr:
        print(rp.stderr.rstrip())
    return 0


def main() -> int:
    ap = argparse.ArgumentParser(
        description="Append file/line location to literal Fortran error stop messages."
    )
    ap.add_argument("inputs", nargs="+", type=Path, help="Fortran source files (supports globs)")
    ap.add_argument("--fix", action="store_true", help="Rewrite files in place")
    ap.add_argument("--out", type=Path, help="Write transformed output to this file (single input)")
    ap.add_argument("--out-dir", type=Path, help="Write transformed outputs to this directory")
    ap.add_argument(
        "--condition-values",
        action="store_true",
        help="Rewrite IF+ERROR STOP blocks to include condition variable values in the message",
    )
    ap.add_argument("--max-len", type=int, default=80, help="Maximum line length after wrapping (default: 80)")
    ap.add_argument("--run", action="store_true", help="Compile and run transformed Fortran sources")
    ap.add_argument(
        "--run-both",
        action="store_true",
        help="Compile and run both original and transformed Fortran sources",
    )
    ap.add_argument("--tee", action="store_true", help="Print original and transformed source text")
    ap.add_argument("--tee-both", action="store_true", help="Alias for --tee")
    ap.add_argument("--verbose", action="store_true", help="Print per-file rewrite counts")
    args = ap.parse_args()
    if args.tee_both:
        args.tee = True

    if args.out is not None and args.out_dir is not None:
        print("--out and --out-dir are mutually exclusive.")
        return 2
    if args.run and args.run_both:
        print("--run and --run-both are mutually exclusive.")
        return 2
    if args.out is not None or args.out_dir is not None:
        args.fix = True

    paths = cpaths.expand_source_inputs(args.inputs, extensions=(".f90", ".F90", ".f", ".F"))
    missing = [p for p in paths if not p.exists()]
    for p in missing:
        print(f"Missing file: {p}")
    if missing:
        return 1
    if args.out is not None and len(paths) != 1:
        print("--out requires exactly one input file.")
        return 2
    if args.max_len < 20:
        print("--max-len should be at least 20.")
        return 2
    if args.out_dir is not None:
        if args.out_dir.exists() and not args.out_dir.is_dir():
            print("--out-dir exists but is not a directory.")
            return 2
        args.out_dir.mkdir(parents=True, exist_ok=True)

    files_changed = 0
    total_rewrites = 0
    total_cond_rewrites = 0
    rendered: dict[Path, str] = {}
    originals: dict[Path, str] = {}
    for p in paths:
        src_lines = _read_lines(p)
        new_lines = src_lines
        cond_rewrites = 0
        label_name = args.out.name if (args.out is not None and len(paths) == 1) else p.name
        if args.condition_values:
            new_lines, cond_rewrites = fscan.rewrite_error_stop_blocks_with_condition_values(
                new_lines, file_label=label_name
            )
        new_lines, rewrites = fscan.append_error_stop_locations(new_lines, file_label=label_name)
        code_lines = [ln.rstrip("\r\n") for ln in new_lines]
        code_lines = fscan.wrap_long_declaration_lines(code_lines, max_len=args.max_len)
        code_lines = fscan.wrap_long_fortran_lines(code_lines, max_len=args.max_len)
        code_lines = fscan.normalize_location_tag_separators(code_lines)
        new_lines = [ln if ln.endswith("\n") else (ln + "\n") for ln in code_lines]
        src = "".join(src_lines)
        dst = "".join(new_lines)
        originals[p] = src
        rendered[p] = dst
        if src != dst:
            files_changed += 1
        total_rewrites += rewrites
        total_cond_rewrites += cond_rewrites
        if args.verbose:
            print(
                f"{p.name}: rewrote {rewrites} literal error stop message(s), "
                f"{cond_rewrites} IF-block(s)"
            )
        if args.fix:
            out_path = args.out if args.out is not None else (args.out_dir / p.name if args.out_dir else p)
            out_path.write_text(dst, encoding="utf-8", newline="")

    if args.tee:
        for p in paths:
            print(f"--- original: {p.name} ---")
            _print_text_stdout(originals[p])
            print(f"--- transformed: {p.name} ---")
            _print_text_stdout(rendered[p])
    elif (not args.fix) and (not args.run) and (not args.run_both) and len(paths) == 1:
        _print_text_stdout(rendered[paths[0]])

    if args.fix:
        print(
            f"Applied {total_rewrites} location tag rewrite(s) and "
            f"{total_cond_rewrites} IF-block rewrite(s) across {files_changed} file(s)."
        )
    elif len(paths) > 1:
        print(
            f"Checked {len(paths)} file(s); {total_rewrites} location rewrite(s) and "
            f"{total_cond_rewrites} IF-block rewrite(s) available in {files_changed} file(s). "
            f"Use --fix to apply."
        )

    if args.run or args.run_both:
        with tempfile.TemporaryDirectory(prefix="xerror_loc_") as td:
            tmpdir = Path(td)
            transformed_files: List[Path] = []
            for p in paths:
                out_name = args.out.name if (args.out is not None and len(paths) == 1) else p.name
                tp = tmpdir / out_name
                tp.write_text(rendered[p], encoding="utf-8", newline="")
                transformed_files.append(tp)
            rc_orig = 0
            if args.run_both:
                rc_orig = _compile_and_run(paths, label="original-fortran")
            rc_new = _compile_and_run(transformed_files, label="transformed-fortran")
            if rc_orig != 0:
                return rc_orig
            if rc_new != 0:
                return rc_new
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
