#!/usr/bin/env python3
"""Strip Fortran attributes (intent / pure / elemental) for test preparation."""

from __future__ import annotations

import argparse
import difflib
import fortran_build as fbuild
import re
import shutil
from pathlib import Path
from typing import Dict, Iterable, List, Tuple

import fortran_scan as fscan

INTENT_ATTR_RE = re.compile(r"(?i),?\s*intent\s*\(\s*(?:inout|out|in)\s*\)")
VALUE_ATTR_RE = re.compile(r"(?i),?\s*value\b")


def split_code_comment(line: str) -> Tuple[str, str]:
    """Split one line into code and comment parts."""
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
    """Return the end-of-line marker used by a line."""
    if line.endswith("\r\n"):
        return "\r\n"
    if line.endswith("\n"):
        return "\n"
    return ""


def collapse_decl_spaces(code: str) -> str:
    """Normalize spacing in declaration prefixes after attribute removal."""
    code = re.sub(r"\s+,", ",", code)
    code = re.sub(r",\s+", ", ", code)
    code = re.sub(r"\s*::\s*", " :: ", code)
    code = re.sub(r"\s+", " ", code)
    return code.strip()


def strip_intent_line(line: str, strip_value: bool) -> Tuple[str, int]:
    """Remove INTENT (and optionally VALUE) attributes from one declaration line."""
    eol = get_eol(line)
    code, comment = split_code_comment(line)
    n = 0
    new_code, c = INTENT_ATTR_RE.subn("", code)
    n += c
    if strip_value:
        new_code, c = VALUE_ATTR_RE.subn("", new_code)
        n += c
    if n == 0:
        return line, 0
    # Keep declaration readable after attribute removal.
    if "::" in new_code:
        lead, rhs = new_code.split("::", 1)
        lead = collapse_decl_spaces(lead)
        new_code = f"{lead} ::{rhs}"
    out = new_code + comment
    if eol and not out.endswith(("\n", "\r\n")):
        out += eol
    return out, n


def strip_pure_line(line: str) -> Tuple[str, int]:
    """Remove PURE/ELEMENTAL/IMPURE attributes from a procedure declaration line."""
    eol = get_eol(line)
    code, comment = split_code_comment(line)
    m_kind = re.search(r"\b(function|subroutine)\b", code, flags=re.IGNORECASE)
    if not m_kind:
        return line, 0
    prefix_part = code[:m_kind.start()]
    rest = code[m_kind.start():]
    indent_match = re.match(r"^\s*", prefix_part)
    indent = indent_match.group(0) if indent_match else ""
    body = prefix_part[len(indent):]
    tokens = [t for t in body.split() if t]
    keep: List[str] = []
    removed = 0
    for tok in tokens:
        tl = tok.lower()
        if tl in {"pure", "elemental", "impure"}:
            removed += 1
        else:
            keep.append(tok)
    if removed == 0:
        return line, 0
    new_prefix = indent + (" ".join(keep) + " " if keep else "")
    out = f"{new_prefix}{rest}{comment}"
    if eol and not out.endswith(("\n", "\r\n")):
        out += eol
    return out, removed


def process_file(
    path: Path,
    strip_intent: bool,
    strip_pure: bool,
    strip_value: bool,
    backup: bool,
    show_diff: bool,
) -> Tuple[int, int]:
    """Apply requested stripping operations to a file and write changes."""
    text = path.read_text(encoding="utf-8", errors="ignore")
    lines = text.splitlines(keepends=True)
    updated = lines[:]

    removed_intent = 0
    removed_pure = 0
    for i, line in enumerate(updated):
        new_line = line
        if strip_intent:
            new_line, n = strip_intent_line(new_line, strip_value=strip_value)
            removed_intent += n
        if strip_pure:
            new_line, n = strip_pure_line(new_line)
            removed_pure += n
        updated[i] = new_line

    if updated == lines:
        return 0, 0

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

    if backup:
        backup_path = path.with_name(path.name + ".bak")
        shutil.copy2(path, backup_path)

    with path.open("w", encoding="utf-8", newline="") as f:
        f.write("".join(updated))

    return removed_intent, removed_pure


def choose_files(args_files: List[Path], exclude: List[str]) -> List[Path]:
    """Resolve input files from arguments or current directory defaults."""
    if args_files:
        files = args_files
    else:
        files = sorted(
        set(Path(".").glob("*.f90")) | set(Path(".").glob("*.F90")),
        key=lambda p: p.name.lower(),
        )
    return fscan.apply_excludes(files, exclude)


def main() -> int:
    """Parse CLI options and strip selected attributes from Fortran files."""
    parser = argparse.ArgumentParser(description="Strip Fortran intent/pure/elemental annotations")
    parser.add_argument("fortran_files", type=Path, nargs="*", help="Source files (default: *.f90/*.F90 in cwd)")
    parser.add_argument(
        "--exclude",
        action="append",
        default=[],
        help="Glob pattern to exclude files (can be repeated)",
    )
    parser.add_argument(
        "--strip",
        choices=["intent", "pure", "all"],
        default="all",
        help="What to strip (default: all)",
    )
    parser.add_argument("--strip-value", action="store_true", help="Also strip VALUE when stripping intent")
    parser.add_argument("--backup", dest="backup", action="store_true", default=True)
    parser.add_argument("--no-backup", dest="backup", action="store_false")
    parser.add_argument("--diff", action="store_true")
    parser.add_argument("--summary", action="store_true", help="Print summary of changes")
    parser.add_argument("--git", action="store_true", help="Commit changed files to git after successful run")
    args = parser.parse_args()

    files = choose_files(args.fortran_files, args.exclude)
    if not files:
        print("No source files remain after applying --exclude filters.")
        return 2

    strip_intent = args.strip in {"intent", "all"}
    strip_pure = args.strip in {"pure", "all"}

    total_intent = 0
    total_pure = 0
    changed_files = 0
    changed_paths: List[Path] = []

    for p in files:
        if not p.exists():
            print(f"File not found: {fscan.display_path(p)}")
            continue
        ri, rp = process_file(
            p,
            strip_intent=strip_intent,
            strip_pure=strip_pure,
            strip_value=args.strip_value,
            backup=args.backup,
            show_diff=args.diff,
        )
        if ri > 0 or rp > 0:
            changed_files += 1
            changed_paths.append(p)
        total_intent += ri
        total_pure += rp

    if args.git and changed_paths:
        msg = (
            f"xstrip: remove {total_intent} intent/value and "
            f"{total_pure} pure/elemental attrs in {changed_files} file(s)"
        )
        fbuild.git_commit_files(changed_paths, msg, fscan.display_path)

    if args.summary:
        print("\nSummary:")
        print(f"files changed: {changed_files}")
        print(f"intent/value attributes removed: {total_intent}")
        print(f"pure/elemental/impure attributes removed: {total_pure}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
