#!/usr/bin/env python3
"""Strip ONLY clauses from Fortran USE statements."""

from __future__ import annotations

import argparse
import difflib
import re
import shutil
from pathlib import Path
from typing import Iterable, List, Tuple

import cli_paths as cpaths
import fortran_scan as fscan

USE_ONLY_RE = re.compile(
    r"^(?P<prefix>\s*use\b(?:\s*,\s*(?:non_intrinsic|intrinsic)\s*)?(?:\s*::\s*|\s+))"
    r"(?P<module>[a-z][a-z0-9_]*)\s*,\s*only\s*:\s*.+$",
    re.IGNORECASE,
)


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
    """Resolve source file list from args or current-directory defaults."""
    if args_files:
        files = cpaths.expand_path_args(args_files)
    else:
        files = sorted(
            set(Path(".").glob("*.f90")) | set(Path(".").glob("*.F90")),
            key=lambda p: p.name.lower(),
        )
    return fscan.apply_excludes(files, exclude)


def rewrite_use_only_line(line: str) -> Tuple[str, bool]:
    """Rewrite a single-line USE, ONLY statement to broad USE form."""
    code, comment = split_code_comment(line.rstrip("\r\n"))
    if "&" in code:
        return line, False
    # Renamed imports (e.g. gplot => plot) cannot be represented by broad USE.
    if "=>" in code:
        return line, False
    m = USE_ONLY_RE.match(code)
    if not m:
        return line, False
    eol = get_eol(line)
    new_line = f"{m.group('prefix')}{m.group('module')}{comment}{eol}"
    return new_line, (new_line != line)


def collect_continuation_block(lines: List[str], start_idx: int) -> Tuple[int, str, str, str, bool]:
    """Collect a continued statement block and return parse metadata."""
    eol = get_eol(lines[start_idx]) or "\n"
    first_code, first_comment = split_code_comment(lines[start_idx].rstrip("\r\n"))
    if first_comment.strip():
        return start_idx + 1, "", "", eol, False

    indent = re.match(r"^\s*", first_code).group(0)
    parts: List[str] = []
    i = start_idx
    cont = False
    while i < len(lines):
        code, comment = split_code_comment(lines[i].rstrip("\r\n"))
        if comment.strip():
            return i + 1, "", "", eol, False
        seg = code.rstrip()
        if i > start_idx:
            lead = seg.lstrip()
            if lead.startswith("&"):
                seg = lead[1:].lstrip()
        cont = seg.endswith("&")
        if cont:
            seg = seg[:-1].rstrip()
        if seg.strip():
            parts.append(seg.strip())
        i += 1
        if not cont:
            break
    return i, " ".join(parts), indent, eol, True


def rewrite_use_only_block(lines: List[str], start_idx: int) -> Tuple[int, List[str], bool]:
    """Rewrite one USE, ONLY statement block, including continuation lines."""
    end_idx, joined, indent, eol, ok = collect_continuation_block(lines, start_idx)
    if not ok or not joined:
        return start_idx + 1, [lines[start_idx]], False
    # Renamed imports (e.g. gplot => plot) cannot be represented by broad USE.
    if "=>" in joined:
        return end_idx, lines[start_idx:end_idx], False
    m = USE_ONLY_RE.match(joined)
    if not m:
        return end_idx, lines[start_idx:end_idx], False
    new_line = f"{m.group('prefix')}{m.group('module')}{eol}"
    # Preserve original indentation from first line.
    if not new_line.startswith(indent):
        new_line = indent + new_line.lstrip()
    return end_idx, [new_line], True


def process_file(path: Path, fix: bool, backup: bool, show_diff: bool) -> int:
    """Process one file and optionally write stripped USE statements."""
    text = path.read_text(encoding="utf-8", errors="ignore")
    lines = text.splitlines(keepends=True)
    updated: List[str] = []
    changed = 0
    i = 0
    while i < len(lines):
        end_idx, repl, did = rewrite_use_only_block(lines, i)
        if did:
            changed += 1
        updated.extend(repl)
        i = end_idx

    if changed == 0:
        return 0

    if show_diff:
        diff = difflib.unified_diff(
            lines,
            updated,
            fromfile=str(path),
            tofile=str(path),
            lineterm="",
        )
        print("\nProposed diff:")
        for d in diff:
            print(d)

    if fix:
        if backup:
            backup_path = path.with_name(path.name + ".bak")
            shutil.copy2(path, backup_path)
            print(f"Backup written: {backup_path.name}")
        path.write_text("".join(updated), encoding="utf-8", newline="")

    return changed


def main() -> int:
    """Run USE, ONLY stripping workflow."""
    parser = argparse.ArgumentParser(description="Strip USE, ONLY clauses to broad USE imports")
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--fix", action="store_true", help="Write changes to files")
    parser.add_argument("--backup", dest="backup", action="store_true", default=True)
    parser.add_argument("--no-backup", dest="backup", action="store_false")
    parser.add_argument("--diff", action="store_true")
    args = parser.parse_args()

    files = choose_files(args.fortran_files, args.exclude)
    if not files:
        print("No source files remain after applying --exclude filters.")
        return 2

    total_changed = 0
    changed_files = 0
    for p in files:
        if not p.exists():
            print(f"File not found: {p.name}")
            continue
        c = process_file(p, fix=args.fix, backup=args.backup, show_diff=args.diff)
        if c > 0:
            total_changed += c
            changed_files += 1

    action = "Converted" if args.fix else "Can convert"
    print(f"{action} {total_changed} USE statement(s) in {changed_files} file(s).")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
