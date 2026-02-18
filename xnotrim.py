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
import fortran_build as fbuild
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
SELECT_CASE_TRIM_RE = re.compile(
    r"^\s*select\s+case\s*\(\s*trim\s*\(\s*([a-z][a-z0-9_]*)\s*\)\s*\)\s*$",
    re.IGNORECASE,
)
FIX_SELECT_CASE_TRIM_RE = re.compile(
    r"^(\s*select\s+case\s*\(\s*)trim\s*\(\s*([a-z][a-z0-9_]*)\s*\)(\s*\)\s*)$",
    re.IGNORECASE,
)
ANNOTATE_TAG = "!! changed by xnotrim.py"


@dataclass
class Finding:
    """One needless-trim advisory finding."""

    path: Path
    line: int
    kind: str
    var: str
    op: str
    literal: str
    stmt: str


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
                    kind="comparison",
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
                    kind="comparison",
                    var=m.group(3),
                    op=m.group(2),
                    literal=m.group(1),
                    stmt=stmt.strip(),
                )
            )
        m_sel = SELECT_CASE_TRIM_RE.match(stmt.strip())
        if m_sel:
            out.append(
                Finding(
                    path=path,
                    line=lineno,
                    kind="select_case",
                    var=m_sel.group(1),
                    op="select case",
                    literal="",
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


def apply_fix_file(
    path: Path,
    *,
    annotate: bool = False,
    out_path: Path | None = None,
    create_backup: bool = True,
) -> tuple[int, Path | None]:
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
        new_code, c3 = FIX_SELECT_CASE_TRIM_RE.subn(r"\1\2\3", new_code)
        if c1 + c2 + c3 > 0:
            changed += c1 + c2 + c3
            new_comment = comment
            if annotate and ANNOTATE_TAG.lower() not in comment.lower():
                if new_comment:
                    new_comment = f"{new_comment}  {ANNOTATE_TAG}"
                else:
                    new_comment = f"  {ANNOTATE_TAG}"
            lines[i] = f"{new_code}{new_comment}{eol}"

    if changed == 0:
        return 0, None
    backup: Path | None = None
    target = out_path if out_path is not None else path
    if out_path is None and create_backup:
        backup = make_backup_path(path)
        shutil.copy2(path, backup)
    target.write_text("".join(lines), encoding="utf-8")
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
    parser.add_argument("--out", type=Path, help="With --fix, write transformed output to this file (single input)")
    parser.add_argument("--out-dir", type=Path, help="With --fix, write outputs to this directory")
    parser.add_argument("--backup", dest="backup", action="store_true", default=True)
    parser.add_argument("--no-backup", dest="backup", action="store_false")
    parser.add_argument("--annotate", action="store_true", help="With --fix, append change tag comments")
    parser.add_argument("--diff", action="store_true", help="With --fix, print unified diffs for changed files")
    parser.add_argument("--compiler", type=str, help="Compile command for baseline/after-fix validation")
    args = parser.parse_args()
    if args.out is not None and args.out_dir is not None:
        print("--out and --out-dir are mutually exclusive.")
        return 2
    if args.out is not None or args.out_dir is not None:
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
        findings.extend(analyze_file(p))

    if not findings:
        print("No needless TRIM() findings.")
        return 0

    findings.sort(key=lambda x: (x.path.name.lower(), x.line))
    print(f"{len(findings)} needless TRIM() finding(s).")
    if args.verbose:
        for f in findings:
            if f.kind == "select_case":
                print(f"{f.path.name}:{f.line} needless trim in select case selector: trim({f.var})")
            else:
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
        if first.kind == "select_case":
            print(f"\nFirst finding: {first.path.name}:{first.line} trim({first.var}) in select case selector")
        else:
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
            out_path = args.out if args.out is not None else (args.out_dir / p.name if args.out_dir is not None else None)
            nrewrites, backup = apply_fix_file(
                p, annotate=args.annotate, out_path=out_path, create_backup=args.backup
            )
            total_rewrites += nrewrites
            if nrewrites > 0 and out_path is not None:
                print(f"\nFixed {p.name}: rewrites {nrewrites}, wrote {out_path}")
            elif nrewrites > 0 and backup is not None:
                print(f"\nFixed {p.name}: rewrites {nrewrites}, backup {backup.name}")
            elif nrewrites > 0:
                print(f"\nFixed {p.name}: rewrites {nrewrites}")
            else:
                print(f"\nNo fixes applied to {p.name}")
            if args.diff and nrewrites > 0:
                after = (out_path if out_path is not None else p).read_text(encoding="utf-8")
                diff_lines = difflib.unified_diff(
                    before.splitlines(),
                    after.splitlines(),
                    fromfile=f"a/{p.name}",
                    tofile=f"b/{(out_path.name if out_path is not None else p.name)}",
                    lineterm="",
                )
                print("")
                for d in diff_lines:
                    print(d)
        print(f"\n--fix summary: rewrites {total_rewrites}")
        if args.compiler:
            if not fbuild.run_compiler_command(args.compiler, compile_paths, "after-fix", fscan.display_path):
                return 5
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
