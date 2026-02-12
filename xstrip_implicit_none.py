#!/usr/bin/env python3
"""Strip IMPLICIT NONE statements from Fortran sources."""

from __future__ import annotations

import argparse
import difflib
import re
import shutil
from pathlib import Path
from typing import Iterable, List, Tuple

import cli_paths as cpaths
import fortran_build as fbuild
import fortran_scan as fscan

IMPLICIT_NONE_RE = re.compile(r"^\s*implicit\s+none\b", re.IGNORECASE)


def split_code_comment(line: str) -> Tuple[str, str]:
    """Split one line into code and trailing comment text."""
    in_single = False
    in_double = False
    for i, ch in enumerate(line):
        if ch == "'" and not in_double:
            in_single = not in_single
        elif ch == '"' and not in_single:
            in_double = not in_double
        elif ch == "!" and not in_single and not in_double:
            return line[:i], line[i:]
    return line, ""


def get_eol(line: str) -> str:
    """Return the line-ending sequence for one source line."""
    if line.endswith("\r\n"):
        return "\r\n"
    if line.endswith("\n"):
        return "\n"
    return ""


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


def strip_implicit_in_statement_block(lines: List[str], start_idx: int) -> Tuple[int, List[str], int]:
    """Strip IMPLICIT NONE from one statement/continuation block."""
    eol = get_eol(lines[start_idx]) or "\n"
    block_lines: List[str] = []
    i = start_idx
    cont = False
    while i < len(lines):
        line = lines[i]
        block_lines.append(line)
        code, _comment = split_code_comment(line.rstrip("\r\n"))
        seg = code.rstrip()
        if i > start_idx:
            lead = seg.lstrip()
            if lead.startswith("&"):
                seg = lead[1:].lstrip()
        cont = seg.endswith("&")
        i += 1
        if not cont:
            if i < len(lines):
                nxt_code, _ = split_code_comment(lines[i].rstrip("\r\n"))
                if nxt_code.lstrip().startswith("&"):
                    cont = True
                else:
                    break
            else:
                break

    # Reconstruct joined statement text.
    joined_parts: List[str] = []
    for j, line in enumerate(block_lines):
        code, _comment = split_code_comment(line.rstrip("\r\n"))
        seg = code.rstrip()
        if j > 0:
            lead = seg.lstrip()
            if lead.startswith("&"):
                seg = lead[1:].lstrip()
        if seg.endswith("&"):
            seg = seg[:-1].rstrip()
        if seg.strip():
            joined_parts.append(seg.strip())
    joined = " ".join(joined_parts).strip()
    if not joined:
        return i, block_lines, 0

    kept_parts: List[str] = []
    removed = 0
    for stmt in fscan.split_fortran_statements(joined):
        if IMPLICIT_NONE_RE.match(stmt):
            removed += 1
        else:
            kept_parts.append(stmt.strip())

    if removed == 0:
        return i, block_lines, 0
    if not kept_parts:
        return i, [], removed

    indent = re.match(r"^\s*", block_lines[0]).group(0)
    rebuilt = f"{indent}{'; '.join(kept_parts)}{eol}"
    return i, [rebuilt], removed


def process_file(path: Path, fix: bool, backup: bool, show_diff: bool, out_path: Path | None = None) -> int:
    """Process one file and optionally write stripped IMPLICIT NONE lines."""
    text = path.read_text(encoding="utf-8", errors="ignore")
    lines = text.splitlines(keepends=True)
    updated: List[str] = []
    removed_total = 0

    i = 0
    while i < len(lines):
        end_idx, repl, removed = strip_implicit_in_statement_block(lines, i)
        updated.extend(repl)
        removed_total += removed
        i = end_idx

    if removed_total == 0:
        return 0

    diff_target = out_path if (out_path is not None and fix) else path
    if show_diff:
        diff = difflib.unified_diff(
            lines,
            updated,
            fromfile=str(path),
            tofile=str(diff_target),
            lineterm="",
        )
        print("\nProposed diff:")
        for d in diff:
            print(d)

    if fix:
        if backup and out_path is None:
            backup_path = path.with_name(path.name + ".bak")
            shutil.copy2(path, backup_path)
            print(f"Backup written: {backup_path.name}")
        target = out_path if out_path is not None else path
        target.write_text("".join(updated), encoding="utf-8", newline="")

    return removed_total


def main() -> int:
    """Run IMPLICIT NONE stripping workflow."""
    parser = argparse.ArgumentParser(description="Strip IMPLICIT NONE statements from Fortran files")
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--fix", action="store_true", help="Write changes to files")
    parser.add_argument("--out", type=Path, help="With --fix, write transformed output to this file (single input)")
    parser.add_argument("--backup", dest="backup", action="store_true", default=True)
    parser.add_argument("--no-backup", dest="backup", action="store_false")
    parser.add_argument("--diff", action="store_true")
    parser.add_argument("--compiler", type=str, help="Compile command for baseline/after-fix validation")
    args = parser.parse_args()
    if args.out is not None:
        args.fix = True
    if args.compiler and not args.fix:
        print("--compiler requires --fix.")
        return 2

    files = choose_files(args.fortran_files, args.exclude)
    if not files:
        print("No source files remain after applying --exclude filters.")
        return 2
    if args.out is not None and len(files) != 1:
        print("--out requires exactly one input source file.")
        return 2
    compile_paths = [args.out] if (args.fix and args.out is not None) else files
    if args.fix and args.compiler:
        if not fbuild.run_compiler_command(args.compiler, compile_paths, "baseline", fscan.display_path):
            return 5

    removed_total = 0
    changed_files = 0
    for p in files:
        if not p.exists():
            print(f"File not found: {p.name}")
            continue
        out_path = args.out if args.out is not None else None
        n = process_file(p, fix=args.fix, backup=args.backup, show_diff=args.diff, out_path=out_path)
        if n > 0:
            removed_total += n
            changed_files += 1

    action = "Removed" if args.fix else "Can remove"
    print(f"{action} {removed_total} IMPLICIT NONE statement(s) in {changed_files} file(s).")
    if args.fix and args.compiler:
        if not fbuild.run_compiler_command(args.compiler, compile_paths, "after-fix", fscan.display_path):
            return 5
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
