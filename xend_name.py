#!/usr/bin/env python3
"""Suggest/fix plain END statements to named unit END statements."""

from __future__ import annotations

import argparse
import difflib
import re
import shutil
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Tuple

import cli_paths as cpaths
import fortran_build as fbuild
import fortran_scan as fscan

PROGRAM_START_RE = re.compile(r"^\s*program\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
PROC_START_RE = re.compile(
    r"^\s*(?:(?:pure|elemental|impure|recursive|module)\s+)*(function|subroutine)\s+([a-z][a-z0-9_]*)\b",
    re.IGNORECASE,
)
MODULE_START_RE = re.compile(r"^\s*module\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
BLOCKDATA_START_RE = re.compile(r"^\s*block\s+data\b", re.IGNORECASE)
INTERFACE_START_RE = re.compile(r"^\s*(abstract\s+)?interface\b", re.IGNORECASE)
END_INTERFACE_RE = re.compile(r"^\s*end\s+interface\b", re.IGNORECASE)


@dataclass
class Unit:
    """Track an open Fortran program unit while scanning."""

    kind: str
    name: str
    start_line: int


@dataclass
class Finding:
    """One plain-END rewrite candidate."""

    path: Path
    line: int
    kind: str
    name: str

    @property
    def replacement(self) -> str:
        """Suggested replacement statement text."""
        return f"end {self.kind} {self.name}"


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
    """Create a non-overwriting backup path next to a source file."""
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
    """Split one source line into code and trailing comment text."""
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


def pop_matching(stack: List[Unit], end_kind: str) -> Optional[Unit]:
    """Pop the most-recent matching unit kind from the stack."""
    for i in range(len(stack) - 1, -1, -1):
        if stack[i].kind == end_kind:
            return stack.pop(i)
    return None


def analyze_file(path: Path) -> List[Finding]:
    """Analyze one source file and return plain END rewrite findings."""
    infos, any_missing = fscan.load_source_files([path])
    if not infos or any_missing:
        return []
    finfo = infos[0]
    lines = finfo.parsed_lines
    findings: List[Finding] = []
    stack: List[Unit] = []
    interface_depth = 0

    for lineno, stmt in fscan.iter_fortran_statements(lines):
        low = stmt.strip().lower()
        if not low:
            continue

        if INTERFACE_START_RE.match(low):
            interface_depth += 1
            continue
        if END_INTERFACE_RE.match(low):
            interface_depth = max(0, interface_depth - 1)
            continue
        if interface_depth > 0:
            continue

        m_prog = PROGRAM_START_RE.match(low)
        if m_prog:
            stack.append(Unit(kind="program", name=m_prog.group(1).lower(), start_line=lineno))
            continue

        m_mod = MODULE_START_RE.match(low)
        if m_mod:
            toks = low.split()
            if len(toks) >= 2 and toks[1] != "procedure":
                stack.append(Unit(kind="module", name=m_mod.group(1).lower(), start_line=lineno))
            continue

        if BLOCKDATA_START_RE.match(low):
            stack.append(Unit(kind="block", name="data", start_line=lineno))
            continue

        m_proc = PROC_START_RE.match(low)
        if m_proc:
            stack.append(Unit(kind=m_proc.group(1).lower(), name=m_proc.group(2).lower(), start_line=lineno))
            continue

        if not low.startswith("end"):
            continue

        toks = low.split()
        if len(toks) == 1:
            if 1 <= lineno <= len(lines):
                raw = lines[lineno - 1].rstrip("\r\n")
                code, _comment = split_code_comment(raw)
                if code.strip().lower() == "end" and stack:
                    top = stack[-1]
                    if top.kind in {"program", "subroutine", "function", "module"}:
                        findings.append(Finding(path=path, line=lineno, kind=top.kind, name=top.name))
            if stack:
                stack.pop()
            continue

        end_kind = toks[1]
        if end_kind in {"program", "subroutine", "function", "module", "block"}:
            pop_matching(stack, end_kind)

    return findings


def annotate_file(path: Path, findings: List[Finding], *, backup: bool = True) -> Tuple[int, Optional[Path]]:
    """Insert replacement suggestion comments after plain END lines."""
    if not findings:
        return 0, None
    lines = path.read_text(encoding="utf-8", errors="ignore").splitlines(keepends=True)
    inserts: List[Tuple[int, str]] = []

    for f in findings:
        idx = f.line - 1
        if idx < 0 or idx >= len(lines):
            continue
        raw = lines[idx]
        indent = re.match(r"^\s*", raw).group(0) if raw else ""
        eol = "\r\n" if raw.endswith("\r\n") else ("\n" if raw.endswith("\n") else "\n")
        msg = f"{indent}! {f.replacement}  !! suggested by xend_name.py{eol}"
        nxt = idx + 1
        if 0 <= nxt < len(lines) and lines[nxt].strip().lower() == msg.strip().lower():
            continue
        inserts.append((idx + 1, msg))

    if not inserts:
        return 0, None
    backup_path: Optional[Path] = None
    if backup:
        backup_path = make_backup_path(path)
        shutil.copy2(path, backup_path)
    for at, msg in sorted(inserts, key=lambda x: x[0], reverse=True):
        lines.insert(at, msg)
    path.write_text("".join(lines), encoding="utf-8")
    return len(inserts), backup_path


def apply_fix_file(
    path: Path,
    findings: List[Finding],
    *,
    annotate: bool = False,
    out_path: Optional[Path] = None,
    create_backup: bool = True,
) -> Tuple[int, Optional[Path]]:
    """Rewrite plain END lines into named END statements."""
    if not findings:
        return 0, None

    by_line: Dict[int, Finding] = {}
    for f in findings:
        by_line.setdefault(f.line, f)

    lines = path.read_text(encoding="utf-8", errors="ignore").splitlines(keepends=True)
    changed = 0
    for idx, raw in enumerate(lines):
        f = by_line.get(idx + 1)
        if f is None:
            continue
        body = raw.rstrip("\r\n")
        eol = raw[len(body) :]
        code, comment = split_code_comment(body)
        if code.strip().lower() != "end":
            continue
        indent = re.match(r"^\s*", code).group(0) if code else ""
        suffix = "  !! changed by xend_name.py" if annotate else ""
        trailing = f" {comment.strip()}" if comment.strip() else ""
        lines[idx] = f"{indent}{f.replacement}{suffix}{trailing}{eol}"
        changed += 1

    if changed == 0:
        return 0, None
    backup: Optional[Path] = None
    target = out_path if out_path is not None else path
    if out_path is None and create_backup:
        backup = make_backup_path(path)
        shutil.copy2(path, backup)
    target.write_text("".join(lines), encoding="utf-8")
    return changed, backup


def main() -> int:
    """Run plain-END naming advisory/fix workflow."""
    parser = argparse.ArgumentParser(
        description="Suggest/fix plain END statements to END PROGRAM/SUBROUTINE/FUNCTION/MODULE <name>"
    )
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--verbose", action="store_true", help="Print replacement text for each finding")
    parser.add_argument("--fix", action="store_true", help="Rewrite plain END to named END statements")
    parser.add_argument("--out", type=Path, help="With --fix, write transformed output to this file (single input)")
    parser.add_argument("--out-dir", type=Path, help="With --fix, write transformed outputs to this directory")
    parser.add_argument("--backup", dest="backup", action="store_true", default=True)
    parser.add_argument("--no-backup", dest="backup", action="store_false")
    parser.add_argument("--annotate", action="store_true", help="Insert suggestion comments (or changed tags with --fix)")
    parser.add_argument("--diff", action="store_true", help="With --fix, print unified diffs for changed files")
    parser.add_argument("--compiler", type=str, help="Compile command for baseline/after-fix validation")
    args = parser.parse_args()
    if args.out is not None or args.out_dir is not None:
        args.fix = True
    if args.out is not None and args.out_dir is not None:
        print("Use only one of --out or --out-dir.")
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
    if args.out_dir is not None:
        if args.out_dir.exists() and not args.out_dir.is_dir():
            print("--out-dir exists but is not a directory.")
            return 2
        args.out_dir.mkdir(parents=True, exist_ok=True)
    compile_paths = [args.out] if (args.fix and args.out is not None) else ([args.out_dir / p.name for p in files] if (args.fix and args.out_dir is not None) else files)
    if args.fix and args.compiler:
        if not fbuild.run_compiler_command(args.compiler, compile_paths, "baseline", fscan.display_path):
            return 5

    findings: List[Finding] = []
    for p in files:
        findings.extend(analyze_file(p))

    if not findings:
        print("No plain END statements needing names found.")
        return 0

    findings.sort(key=lambda f: (f.path.name.lower(), f.line))
    print(f"{len(findings)} plain END statement(s) can be named.")
    for f in findings:
        print(f"{f.path.name}:{f.line} {f.kind} {f.name}")
        if args.verbose:
            print(f"  suggest: {f.replacement}")

    if args.fix:
        by_file: Dict[Path, List[Finding]] = {}
        for f in findings:
            by_file.setdefault(f.path, []).append(f)
        touched = 0
        total = 0
        for p in sorted(by_file.keys(), key=lambda x: x.name.lower()):
            before = p.read_text(encoding="utf-8", errors="ignore")
            out_path = args.out if args.out is not None else (args.out_dir / p.name if args.out_dir is not None else None)
            n, backup = apply_fix_file(
                p, by_file[p], annotate=args.annotate, out_path=out_path, create_backup=args.backup
            )
            total += n
            if n > 0:
                touched += 1
                if out_path is not None:
                    print(f"\nFixed {p.name}: replaced {n}, wrote {out_path}")
                else:
                    print(f"\nFixed {p.name}: replaced {n}, backup {backup.name if backup else '(none)'}")
                if args.diff:
                    after = (out_path if out_path is not None else p).read_text(encoding="utf-8", errors="ignore")
                    diff_lines = difflib.unified_diff(
                        before.splitlines(),
                        after.splitlines(),
                        fromfile=f"a/{p.name}",
                        tofile=f"b/{(out_path.name if out_path is not None else p.name)}",
                        lineterm="",
                    )
                    print("")
                    for dl in diff_lines:
                        print(dl)
            elif args.verbose:
                print(f"\nNo fixes applied in {p.name}")
        print(f"\n--fix summary: files changed {touched}, replaced {total}")
    elif args.annotate:
        by_file: Dict[Path, List[Finding]] = {}
        for f in findings:
            by_file.setdefault(f.path, []).append(f)
        total = 0
        touched = 0
        for p in sorted(by_file.keys(), key=lambda x: x.name.lower()):
            n, backup = annotate_file(p, by_file[p], backup=args.backup)
            total += n
            if n > 0:
                touched += 1
                print(f"\nAnnotated {p.name}: inserted {n}, backup {backup.name if backup else '(none)'}")
            elif args.verbose:
                print(f"\nNo annotations inserted in {p.name}")
        print(f"\n--annotate summary: files changed {touched}, inserted {total}")
    if args.fix and args.compiler:
        if not fbuild.run_compiler_command(args.compiler, compile_paths, "after-fix", fscan.display_path):
            return 5
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
