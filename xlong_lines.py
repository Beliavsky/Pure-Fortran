#!/usr/bin/env python3
"""Warn about overly long Fortran source lines and optionally wrap them."""

from __future__ import annotations

import argparse
import difflib
import re
import shutil
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, List, Optional, Tuple

import cli_paths as cpaths
import fortran_build as fbuild
import fortran_scan as fscan


@dataclass
class Finding:
    """One overlong-line finding."""

    path: Path
    line: int
    length: int
    text: str


def choose_files(args_files: List[Path], exclude: Iterable[str]) -> List[Path]:
    """Resolve source files from args or current-directory defaults."""
    if args_files:
        files = cpaths.expand_source_inputs(args_files)
    else:
        files = sorted(
            set(Path(".").glob("*.f90")) | set(Path(".").glob("*.F90")),
            key=lambda p: p.name.lower(),
        )
    return fscan.apply_excludes(files, exclude)


def make_backup_path(path: Path) -> Path:
    """Create a non-overwriting backup path next to path."""
    base = Path(str(path) + ".bak")
    if not base.exists():
        return base
    idx = 1
    while True:
        cand = Path(f"{path}.bak{idx}")
        if not cand.exists():
            return cand
        idx += 1


def line_ending(line: str) -> str:
    """Return newline characters for a line, defaulting to \\n."""
    if line.endswith("\r\n"):
        return "\r\n"
    if line.endswith("\n"):
        return "\n"
    return "\n"


def split_body_eol(line: str) -> Tuple[str, str]:
    """Split raw line into body and eol."""
    if line.endswith("\r\n"):
        return line[:-2], "\r\n"
    if line.endswith("\n"):
        return line[:-1], "\n"
    return line, ""


def is_comment_or_preproc(body: str) -> bool:
    """Return True for comment-only or preprocessor lines."""
    s = body.lstrip()
    if not s:
        return False
    return s.startswith("!") or s.startswith("#")


def break_candidates(body: str, start: int, end: int) -> List[int]:
    """Find safe split candidates between start and end outside quoted strings."""
    out: List[int] = []
    in_single = False
    in_double = False
    i = 0
    while i < len(body):
        ch = body[i]
        if ch == "'" and not in_double:
            if in_single and i + 1 < len(body) and body[i + 1] == "'":
                i += 2
                continue
            in_single = not in_single
            i += 1
            continue
        if ch == '"' and not in_single:
            if in_double and i + 1 < len(body) and body[i + 1] == '"':
                i += 2
                continue
            in_double = not in_double
            i += 1
            continue
        if not in_single and not in_double and start <= i <= end:
            if ch.isspace() or ch in ",+-*/)=]":
                out.append(i)
        i += 1
    return out


def wrap_long_line(body: str, max_len: int) -> Optional[List[str]]:
    """Wrap one long non-comment line using free-form Fortran continuation.

    Returns None when no conservative wrap point is found.
    """
    if len(body) <= max_len:
        return [body]
    if is_comment_or_preproc(body):
        return None

    indent = re.match(r"^\s*", body).group(0)
    cont_indent = indent + "   "
    lines: List[str] = []
    cur = body
    first = True

    while len(cur) > max_len:
        prefix = indent if first else (cont_indent + "& ")
        min_split = len(prefix) + 8
        max_split = max_len - 2  # reserve for trailing " &"
        if max_split <= min_split:
            return None
        cands = break_candidates(cur, min_split, max_split)
        if not cands:
            return None
        cut = cands[-1]
        left = cur[:cut].rstrip()
        right = cur[cut:].lstrip()
        if not left or not right:
            return None
        lines.append(f"{left} &")
        cur = f"{cont_indent}& {right}"
        first = False

    lines.append(cur)
    return lines


def analyze_file(path: Path, max_len: int) -> List[Finding]:
    """Collect all lines longer than max_len."""
    out: List[Finding] = []
    for i, raw in enumerate(path.read_text(encoding="utf-8").splitlines(keepends=True), start=1):
        body, _eol = split_body_eol(raw)
        if len(body) > max_len:
            out.append(Finding(path=path, line=i, length=len(body), text=body))
    return out


def apply_fix_file(
    path: Path,
    max_len: int,
    out_path: Optional[Path] = None,
    create_backup: bool = True,
) -> Tuple[int, int, Optional[Path]]:
    """Wrap overlong lines in one file; returns (changed_lines, skipped_lines, backup)."""
    raw_lines = path.read_text(encoding="utf-8").splitlines(keepends=True)
    out: List[str] = []
    changed = 0
    skipped = 0
    touched = False

    for raw in raw_lines:
        body, eol = split_body_eol(raw)
        if len(body) <= max_len:
            out.append(raw)
            continue
        wrapped = wrap_long_line(body, max_len)
        if wrapped is None:
            skipped += 1
            out.append(raw)
            continue
        touched = True
        changed += 1
        use_eol = eol if eol else "\n"
        for w in wrapped:
            out.append(w + use_eol)

    if not touched:
        return 0, skipped, None
    backup: Optional[Path] = None
    target = out_path if out_path is not None else path
    if out_path is None and create_backup:
        backup = make_backup_path(path)
        shutil.copy2(path, backup)
    target.write_text("".join(out), encoding="utf-8")
    return changed, skipped, backup


def main() -> int:
    """Run long-line advisory/fix checks."""
    parser = argparse.ArgumentParser(description="Warn about overlong Fortran lines and optionally wrap them")
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--max-len", type=int, default=100, help="Maximum allowed line length (default: 100)")
    parser.add_argument("--verbose", action="store_true", help="Print full overlong lines")
    parser.add_argument("--fix", action="store_true", help="Wrap long lines with '&' continuation")
    parser.add_argument("--out", type=Path, help="With --fix, write transformed output to this file (single input)")
    parser.add_argument("--out-dir", type=Path, help="With --fix, write outputs to this directory")
    parser.add_argument("--backup", dest="backup", action="store_true", default=True)
    parser.add_argument("--no-backup", dest="backup", action="store_false")
    parser.add_argument("--diff", "-diff", action="store_true", help="With --fix, show unified diffs for changed files")
    parser.add_argument("--compiler", type=str, help="Compile command for baseline/after-fix validation")
    args = parser.parse_args()
    if args.out is not None and args.out_dir is not None:
        print("--out and --out-dir are mutually exclusive.")
        return 2
    if args.out is not None or args.out_dir is not None:
        args.fix = True

    if args.max_len < 20:
        print("--max-len should be at least 20.")
        return 2
    if args.diff and not args.fix:
        print("--diff requires --fix.")
        return 2
    if args.compiler and not args.fix:
        print("--compiler requires --fix.")
        return 2

    files = choose_files(args.fortran_files, args.exclude)
    if not files:
        print("No source files remain after applying --exclude filters.")
        return 2
    if args.out_dir is not None:
        if args.out_dir.exists() and not args.out_dir.is_dir():
            print("--out-dir exists but is not a directory.")
            return 2
        args.out_dir.mkdir(parents=True, exist_ok=True)
    if args.out is not None and len(files) != 1:
        print("--out requires exactly one input source file.")
        return 2
    if args.fix and args.out_dir is not None:
        for p in files:
            (args.out_dir / p.name).write_text(p.read_text(encoding="utf-8"), encoding="utf-8")
    compile_paths = (
        [args.out]
        if (args.fix and args.out is not None)
        else ([args.out_dir / p.name for p in files] if (args.fix and args.out_dir is not None) else files)
    )
    if args.fix and args.compiler:
        if not fbuild.run_compiler_command(args.compiler, compile_paths, "baseline", fscan.display_path):
            return 5

    findings: List[Finding] = []
    for p in files:
        findings.extend(analyze_file(p, args.max_len))

    if not findings:
        print(f"No lines longer than {args.max_len} characters found.")
        return 0

    findings.sort(key=lambda f: (f.path.name.lower(), f.line))
    print(f"{len(findings)} overlong line(s) found (max {args.max_len}).")
    if args.verbose:
        for f in findings:
            print(f"{f.path.name}:{f.line} len={f.length}")
            print(f"  {f.text}")
    else:
        by_file: dict[str, int] = {}
        for f in findings:
            by_file[f.path.name] = by_file.get(f.path.name, 0) + 1
        for fn in sorted(by_file.keys(), key=str.lower):
            print(f"{fn}: {by_file[fn]}")
        first = findings[0]
        print(f"\nFirst: {first.path.name}:{first.line} len={first.length}")
        print("Run with --verbose to print the full lines.")

    if args.fix:
        candidates = sorted({f.path for f in findings}, key=lambda p: p.name.lower())
        files_changed = 0
        total_changed = 0
        total_skipped = 0
        for p in candidates:
            before = p.read_text(encoding="utf-8")
            out_path = args.out if args.out is not None else (args.out_dir / p.name if args.out_dir is not None else None)
            ch, sk, backup = apply_fix_file(p, args.max_len, out_path=out_path, create_backup=args.backup)
            total_changed += ch
            total_skipped += sk
            if ch > 0:
                files_changed += 1
                if out_path is not None:
                    print(f"\nFixed {p.name}: wrapped {ch}, skipped {sk}, wrote {out_path}")
                else:
                    print(f"\nFixed {p.name}: wrapped {ch}, skipped {sk}, backup {backup.name if backup else '(none)'}")
                if args.diff:
                    after = (out_path if out_path is not None else p).read_text(encoding="utf-8")
                    diff = difflib.unified_diff(
                        before.splitlines(),
                        after.splitlines(),
                        fromfile=f"a/{p.name}",
                        tofile=f"b/{(out_path.name if out_path is not None else p.name)}",
                        lineterm="",
                    )
                    txt = "\n".join(diff)
                    if txt:
                        print(txt)
            else:
                print(f"\nNo fixes applied to {p.name} (skipped {sk})")
        print(
            f"\n--fix summary: files changed {files_changed}, wrapped {total_changed}, skipped {total_skipped}"
        )
        if args.compiler:
            if not fbuild.run_compiler_command(args.compiler, compile_paths, "after-fix", fscan.display_path):
                return 5
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
