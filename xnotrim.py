#!/usr/bin/env python3
"""Warn about likely needless TRIM() usage in string equality/inequality comparisons."""

from __future__ import annotations

import argparse
import difflib
import re
import shutil
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, List

import cli_paths as cpaths
import fortran_scan as fscan

# High-confidence advisory patterns:
#   trim(var) == "lit"   trim(var) /= 'lit'
#   "lit" == trim(var)   'lit' /= trim(var)
LITERAL_RE = r"(?:'(?:''|[^'])*'|\"(?:\"\"|[^\"])*\")"
TRIM_VAR_RE = r"trim\s*\(\s*([a-z][a-z0-9_]*)\s*\)"
OP_RE = r"(==|/=|\.eq\.|\.ne\.)"
OP_TOKEN_RE = r"(?:==|/=|\.eq\.|\.ne\.)"
NEEDLESS_TRIM_LHS_RE = re.compile(
    rf"\b{TRIM_VAR_RE}\s*{OP_RE}\s*({LITERAL_RE})",
    re.IGNORECASE,
)
NEEDLESS_TRIM_RHS_RE = re.compile(
    rf"({LITERAL_RE})\s*{OP_RE}\s*{TRIM_VAR_RE}",
    re.IGNORECASE,
)
FIX_LHS_RE = re.compile(
    rf"\btrim\s*\(\s*([a-z][a-z0-9_]*)\s*\)(\s*{OP_TOKEN_RE}\s*{LITERAL_RE})",
    re.IGNORECASE,
)
FIX_RHS_RE = re.compile(
    rf"({LITERAL_RE}\s*{OP_TOKEN_RE}\s*)trim\s*\(\s*([a-z][a-z0-9_]*)\s*\)",
    re.IGNORECASE,
)


@dataclass
class Finding:
    """One needless-trim advisory finding."""

    path: Path
    line: int
    var: str
    op: str
    literal: str
    stmt: str


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


def analyze_file(path: Path) -> List[Finding]:
    """Analyze one file and return needless-trim findings."""
    infos, any_missing = fscan.load_source_files([path])
    if not infos or any_missing:
        return []
    finfo = infos[0]
    out: List[Finding] = []

    for lineno, stmt in fscan.iter_fortran_statements(finfo.parsed_lines):
        low = stmt.strip().lower()
        if not low:
            continue
        for m in NEEDLESS_TRIM_LHS_RE.finditer(stmt):
            out.append(
                Finding(
                    path=path,
                    line=lineno,
                    var=m.group(1),
                    op=m.group(2),
                    literal=m.group(3),
                    stmt=stmt.strip(),
                )
            )
        for m in NEEDLESS_TRIM_RHS_RE.finditer(stmt):
            out.append(
                Finding(
                    path=path,
                    line=lineno,
                    var=m.group(3),
                    op=m.group(2),
                    literal=m.group(1),
                    stmt=stmt.strip(),
                )
            )
    return out


def split_code_comment(line: str) -> tuple[str, str]:
    """Split a source line into code and trailing comment."""
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


def apply_fix_file(path: Path) -> tuple[int, Path | None]:
    """Apply needless-TRIM rewrites in one file."""
    lines = path.read_text(encoding="utf-8").splitlines(keepends=True)
    changed = 0
    for i, line in enumerate(lines):
        eol = ""
        body = line
        if body.endswith("\r\n"):
            eol = "\r\n"
            body = body[:-2]
        elif body.endswith("\n"):
            eol = "\n"
            body = body[:-1]
        code, comment = split_code_comment(body)
        new_code, c1 = FIX_LHS_RE.subn(r"\1\2", code)
        new_code, c2 = FIX_RHS_RE.subn(r"\1\2", new_code)
        if c1 + c2 > 0:
            changed += c1 + c2
            lines[i] = f"{new_code}{comment}{eol}"

    if changed == 0:
        return 0, None
    backup = make_backup_path(path)
    shutil.copy2(path, backup)
    path.write_text("".join(lines), encoding="utf-8")
    return changed, backup


def main() -> int:
    """Run needless-trim advisory checks across selected Fortran files."""
    parser = argparse.ArgumentParser(
        description="Warn about likely needless TRIM() in string ==/!= comparisons"
    )
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--verbose", action="store_true", help="Print every finding with offending statement line")
    parser.add_argument("--fix", action="store_true", help="Rewrite needless trim(var) wrappers in comparisons")
    parser.add_argument("--diff", action="store_true", help="With --fix, print unified diffs for changed files")
    args = parser.parse_args()

    files = choose_files(args.fortran_files, args.exclude)
    if not files:
        print("No source files remain after applying --exclude filters.")
        return 2

    findings: List[Finding] = []
    for p in files:
        findings.extend(analyze_file(p))

    if not findings:
        print("No needless TRIM() findings.")
        return 0

    findings.sort(key=lambda x: (x.path.name.lower(), x.line))
    print(f"{len(findings)} needless TRIM() finding(s).")
    if args.verbose:
        for f in findings:
            print(
                f"{f.path.name}:{f.line} needless trim in string comparison: "
                f"trim({f.var}) {f.op} {f.literal}"
            )
            print(f"  {f.stmt}")
    else:
        by_file = {}
        for f in findings:
            by_file[f.path.name] = by_file.get(f.path.name, 0) + 1
        for fname in sorted(by_file.keys(), key=str.lower):
            print(f"{fname}: {by_file[fname]}")
        first = findings[0]
        print(
            f"\nFirst finding: {first.path.name}:{first.line} "
            f"trim({first.var}) {first.op} {first.literal}"
        )
        print("Run with --verbose to list all findings and offending lines.")

    if args.fix:
        total_rewrites = 0
        changed_files = sorted({f.path for f in findings}, key=lambda p: p.name.lower())
        for p in changed_files:
            before = p.read_text(encoding="utf-8")
            nrewrites, backup = apply_fix_file(p)
            total_rewrites += nrewrites
            if nrewrites > 0 and backup is not None:
                print(f"\nFixed {p.name}: rewrites {nrewrites}, backup {backup.name}")
            elif nrewrites > 0:
                print(f"\nFixed {p.name}: rewrites {nrewrites}")
            else:
                print(f"\nNo fixes applied to {p.name}")
            if args.diff and nrewrites > 0:
                after = p.read_text(encoding="utf-8")
                diff_lines = difflib.unified_diff(
                    before.splitlines(),
                    after.splitlines(),
                    fromfile=f"a/{p.name}",
                    tofile=f"b/{p.name}",
                    lineterm="",
                )
                print("")
                for d in diff_lines:
                    print(d)
        print(f"\n--fix summary: rewrites {total_rewrites}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
