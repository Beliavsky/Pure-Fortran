#!/usr/bin/env python3
"""Rewrite repeated multiplication terms like x*x to x**2 (conservative)."""

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

OPERAND_RE = r"(?:[a-z][a-z0-9_]*(?:\s*%\s*[a-z][a-z0-9_]*)*|\([^()]+\))"
MUL_PAIR_RE = re.compile(
    rf"(?P<lhs>{OPERAND_RE})\s*(?<!\*)\*(?!\*)\s*(?P<rhs>{OPERAND_RE})",
    re.IGNORECASE,
)
ANNOTATE_TAG = "!! changed by xpower.py"


@dataclass
class Finding:
    """One rewrite opportunity."""

    path: Path
    line: int
    original_line: str
    suggested_line: str
    rewrites: int


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


def split_code_comment(line: str) -> Tuple[str, str]:
    """Split one line into code and trailing comment, respecting quotes."""
    in_single = False
    in_double = False
    i = 0
    while i < len(line):
        ch = line[i]
        if ch == "'" and not in_double:
            if in_single and i + 1 < len(line) and line[i + 1] == "'":
                i += 2
                continue
            in_single = not in_single
            i += 1
            continue
        if ch == '"' and not in_single:
            if in_double and i + 1 < len(line) and line[i + 1] == '"':
                i += 2
                continue
            in_double = not in_double
            i += 1
            continue
        if ch == "!" and not in_single and not in_double:
            return line[:i], line[i:]
        i += 1
    return line, ""


def normalize_operand(text: str) -> str:
    """Normalize operand for equivalence checks."""
    return "".join(text.lower().split())


def rewrite_plain_segment(text: str) -> Tuple[str, int]:
    """Rewrite repeated multiplication in plain (non-string, non-comment) text."""
    out: List[str] = []
    pos = 0
    rewrites = 0
    for m in MUL_PAIR_RE.finditer(text):
        lhs = m.group("lhs")
        rhs = m.group("rhs")
        if normalize_operand(lhs) != normalize_operand(rhs):
            continue
        out.append(text[pos : m.start()])
        base = lhs.strip()
        out.append(f"{base}**2")
        pos = m.end()
        rewrites += 1
    out.append(text[pos:])
    return "".join(out), rewrites


def rewrite_code_outside_quotes(code: str) -> Tuple[str, int]:
    """Apply rewrite to code outside string literals."""
    out: List[str] = []
    cur_plain: List[str] = []
    rewrites = 0
    in_single = False
    in_double = False
    i = 0
    while i < len(code):
        ch = code[i]
        if ch == "'" and not in_double:
            if in_single:
                # Handle escaped '' in single-quoted literal.
                if i + 1 < len(code) and code[i + 1] == "'":
                    out.append("''")
                    i += 2
                    continue
                out.append(ch)
                in_single = False
                i += 1
                continue
            plain = "".join(cur_plain)
            new_plain, n = rewrite_plain_segment(plain)
            out.append(new_plain)
            rewrites += n
            cur_plain = []
            out.append(ch)
            in_single = True
            i += 1
            continue
        if ch == '"' and not in_single:
            if in_double:
                # Handle escaped "" in double-quoted literal.
                if i + 1 < len(code) and code[i + 1] == '"':
                    out.append('""')
                    i += 2
                    continue
                out.append(ch)
                in_double = False
                i += 1
                continue
            plain = "".join(cur_plain)
            new_plain, n = rewrite_plain_segment(plain)
            out.append(new_plain)
            rewrites += n
            cur_plain = []
            out.append(ch)
            in_double = True
            i += 1
            continue

        if in_single or in_double:
            out.append(ch)
        else:
            cur_plain.append(ch)
        i += 1

    if cur_plain:
        plain = "".join(cur_plain)
        new_plain, n = rewrite_plain_segment(plain)
        out.append(new_plain)
        rewrites += n
    return "".join(out), rewrites


def rewrite_line(line: str, *, annotate: bool = False) -> Tuple[str, int]:
    """Rewrite one source line and optionally tag changed lines."""
    eol = ""
    body = line
    if body.endswith("\r\n"):
        eol = "\r\n"
        body = body[:-2]
    elif body.endswith("\n"):
        eol = "\n"
        body = body[:-1]

    code, comment = split_code_comment(body)
    new_code, n = rewrite_code_outside_quotes(code)
    if n <= 0:
        return line, 0

    new_comment = comment
    if annotate and ANNOTATE_TAG.lower() not in comment.lower():
        if new_comment:
            new_comment = f"{new_comment}  {ANNOTATE_TAG}"
        else:
            new_comment = f"  {ANNOTATE_TAG}"
    return f"{new_code}{new_comment}{eol}", n


def analyze_file(path: Path) -> List[Finding]:
    """Analyze one file for x*x -> x**2 opportunities."""
    out: List[Finding] = []
    lines = path.read_text(encoding="utf-8").splitlines(keepends=True)
    for i, line in enumerate(lines, start=1):
        new_line, n = rewrite_line(line, annotate=False)
        if n > 0:
            out.append(
                Finding(
                    path=path,
                    line=i,
                    original_line=line.rstrip("\r\n"),
                    suggested_line=new_line.rstrip("\r\n"),
                    rewrites=n,
                )
            )
    return out


def apply_fix_file(
    path: Path,
    *,
    annotate: bool = False,
    out_path: Optional[Path] = None,
    create_backup: bool = True,
) -> Tuple[int, int, Optional[Path]]:
    """Apply rewrites in one file."""
    lines = path.read_text(encoding="utf-8").splitlines(keepends=True)
    changed_lines = 0
    rewrites = 0
    new_lines: List[str] = []
    for line in lines:
        new_line, n = rewrite_line(line, annotate=annotate)
        new_lines.append(new_line)
        if n > 0:
            changed_lines += 1
            rewrites += n
    if rewrites == 0:
        return 0, 0, None

    backup: Optional[Path] = None
    target = out_path if out_path is not None else path
    if out_path is None and create_backup:
        backup = make_backup_path(path)
        shutil.copy2(path, backup)
    target.write_text("".join(new_lines), encoding="utf-8")
    return changed_lines, rewrites, backup


def main() -> int:
    """Run x*x -> x**2 advisory or fix mode."""
    parser = argparse.ArgumentParser(description="Rewrite repeated multiplication terms as power expressions")
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--verbose", action="store_true", help="Print full source lines for findings")
    parser.add_argument("--fix", action="store_true", help="Apply rewrites in-place")
    parser.add_argument("--out", type=Path, help="With --fix, write transformed output to this file (single input)")
    parser.add_argument("--backup", dest="backup", action="store_true", default=True)
    parser.add_argument("--no-backup", dest="backup", action="store_false")
    parser.add_argument(
        "--annotate",
        action="store_true",
        help=f"With --fix, append '{ANNOTATE_TAG}' on changed lines",
    )
    parser.add_argument("--diff", action="store_true", help="With --fix, print unified diffs for changed files")
    parser.add_argument("--compiler", type=str, help="Compile command for baseline/after-fix validation")
    args = parser.parse_args()
    if args.out is not None:
        args.fix = True

    if args.annotate and not args.fix:
        print("--annotate requires --fix.")
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
    if args.out is not None and len(files) != 1:
        print("--out requires exactly one input source file.")
        return 2
    compile_paths = [args.out] if (args.fix and args.out is not None) else files
    if args.fix and args.compiler:
        if not fbuild.run_compiler_command(args.compiler, compile_paths, "baseline", fscan.display_path):
            return 5

    findings: List[Finding] = []
    for p in files:
        findings.extend(analyze_file(p))

    if not findings:
        print("No repeated-multiplication power candidates found.")
        return 0

    findings.sort(key=lambda f: (f.path.name.lower(), f.line))
    print(f"{len(findings)} repeated-multiplication power candidate line(s).")
    for f in findings:
        print(f"{f.path.name}:{f.line} rewrites={f.rewrites}")
        if args.verbose:
            print(f"  old: {f.original_line}")
            print(f"  new: {f.suggested_line}")

    if args.fix:
        by_file = sorted({f.path for f in findings}, key=lambda p: p.name.lower())
        total_lines = 0
        total_rewrites = 0
        changed_files = 0
        for p in by_file:
            before = p.read_text(encoding="utf-8")
            out_path = args.out if args.out is not None else None
            clines, nrew, backup = apply_fix_file(
                p, annotate=args.annotate, out_path=out_path, create_backup=args.backup
            )
            total_lines += clines
            total_rewrites += nrew
            if nrew > 0:
                changed_files += 1
                if out_path is not None:
                    print(f"\nFixed {p.name}: changed lines {clines}, rewrites {nrew}, wrote {out_path}")
                else:
                    print(
                        f"\nFixed {p.name}: changed lines {clines}, rewrites {nrew}, "
                        f"backup {backup.name if backup else '(none)'}"
                    )
                if args.diff:
                    after = (out_path if out_path is not None else p).read_text(encoding="utf-8")
                    before_disp = before.replace("\ufeff", "")
                    after_disp = after.replace("\ufeff", "")
                    diff = difflib.unified_diff(
                        before_disp.splitlines(),
                        after_disp.splitlines(),
                        fromfile=f"a/{p.name}",
                        tofile=f"b/{(out_path.name if out_path is not None else p.name)}",
                        lineterm="",
                    )
                    txt = "\n".join(diff)
                    if txt:
                        print(txt)
            else:
                print(f"\nNo fixes applied to {p.name}")
        print(
            f"\n--fix summary: files changed {changed_files}, "
            f"changed lines {total_lines}, rewrites {total_rewrites}"
        )
        if args.compiler:
            if not fbuild.run_compiler_command(args.compiler, compile_paths, "after-fix", fscan.display_path):
                return 5
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
