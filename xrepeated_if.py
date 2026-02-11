#!/usr/bin/env python3
"""Warn about consecutive Fortran IF statements that test the same condition."""

from __future__ import annotations

import argparse
import difflib
import shutil
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Set, Tuple

import cli_paths as cpaths
import fortran_scan as fscan


@dataclass
class Finding:
    """One repeated-condition warning."""

    path: Path
    line1: int
    line2: int
    condition: str


@dataclass
class FixPlan:
    """One safe rewrite for two repeated single-line IF statements."""

    path: Path
    line1: int
    line2: int
    condition: str
    action1: str
    action2: str


def choose_files(args_files: List[Path], exclude: Iterable[str]) -> List[Path]:
    """Resolve input files from args or current directory defaults."""
    if args_files:
        files = cpaths.expand_path_args(args_files)
    else:
        files = sorted(
            set(Path(".").glob("*.f90")) | set(Path(".").glob("*.F90")),
            key=lambda p: p.name.lower(),
        )
    return fscan.apply_excludes(files, exclude)


def remove_ws_outside_quotes(text: str) -> str:
    """Remove whitespace not inside quoted strings."""
    out: List[str] = []
    in_single = False
    in_double = False
    for ch in text:
        if ch == "'" and not in_double:
            in_single = not in_single
            out.append(ch)
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            out.append(ch)
            continue
        if ch.isspace() and not in_single and not in_double:
            continue
        out.append(ch)
    return "".join(out)


def normalize_condition(cond: str) -> str:
    """Normalize IF condition text for simple equivalence checks."""
    return remove_ws_outside_quotes(cond.strip().lower())


def extract_if_condition(stmt: str) -> Optional[str]:
    """Extract IF condition from a statement if this is an IF statement."""
    s = stmt.strip()
    low = s.lower()
    if not low.startswith("if"):
        return None
    if low.startswith("if("):
        start = 2
    elif low.startswith("if "):
        start = 2
    elif low.startswith("if\t"):
        start = 2
    else:
        return None

    pos = s.find("(", start)
    if pos < 0:
        return None

    depth = 0
    in_single = False
    in_double = False
    end_pos = -1
    for i in range(pos, len(s)):
        ch = s[i]
        if ch == "'" and not in_double:
            in_single = not in_single
        elif ch == '"' and not in_single:
            in_double = not in_double
        elif not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
                if depth == 0:
                    end_pos = i
                    break
    if end_pos < 0:
        return None
    return s[pos + 1 : end_pos]


def parse_single_line_if(stmt: str) -> Optional[Tuple[str, str]]:
    """Parse single-line IF as (condition, action), excluding IF...THEN forms."""
    s = stmt.strip()
    cond = extract_if_condition(s)
    if cond is None:
        return None
    pos = s.find("(")
    if pos < 0:
        return None
    depth = 0
    in_single = False
    in_double = False
    end_pos = -1
    for i in range(pos, len(s)):
        ch = s[i]
        if ch == "'" and not in_double:
            in_single = not in_single
        elif ch == '"' and not in_single:
            in_double = not in_double
        elif not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
                if depth == 0:
                    end_pos = i
                    break
    if end_pos < 0:
        return None
    tail = s[end_pos + 1 :].strip()
    if not tail:
        return None
    if tail.lower().startswith("then"):
        return None
    return cond, tail


def make_backup_path(path: Path) -> Path:
    """Return next available backup path."""
    base = Path(str(path) + ".bak")
    if not base.exists():
        return base
    idx = 1
    while True:
        cand = Path(f"{path}.bak{idx}")
        if not cand.exists():
            return cand
        idx += 1


def analyze_file(path: Path) -> Tuple[List[Finding], List[FixPlan]]:
    """Analyze one file for consecutive repeated IF conditions."""
    infos, any_missing = fscan.load_source_files([path])
    if not infos or any_missing:
        return [], []
    finfo = infos[0]
    findings: List[Finding] = []
    plans: List[FixPlan] = []

    prev_if: Optional[Tuple[int, str, str, Optional[Tuple[str, str]]]] = None
    used_starts: Set[int] = set()
    for lineno, stmt in fscan.iter_fortran_statements(finfo.parsed_lines):
        cond = extract_if_condition(stmt)
        if cond is None:
            prev_if = None
            continue
        norm = normalize_condition(cond)
        single = parse_single_line_if(stmt)
        if prev_if is not None and prev_if[2] == norm:
            findings.append(
                Finding(
                    path=path,
                    line1=prev_if[0],
                    line2=lineno,
                    condition=cond.strip(),
                )
            )
            prev_single = prev_if[3]
            if (
                prev_single is not None
                and single is not None
                and prev_if[0] not in used_starts
                and lineno not in used_starts
                and prev_if[0] != lineno
            ):
                plans.append(
                    FixPlan(
                        path=path,
                        line1=prev_if[0],
                        line2=lineno,
                        condition=prev_single[0].strip(),
                        action1=prev_single[1].strip(),
                        action2=single[1].strip(),
                    )
                )
                used_starts.add(prev_if[0])
                used_starts.add(lineno)
        prev_if = (lineno, stmt, norm, single)
    return findings, plans


def apply_fixes_for_file(path: Path, plans: List[FixPlan]) -> Tuple[int, Optional[Path]]:
    """Apply non-overlapping plans in one file."""
    if not plans:
        return 0, None

    text = path.read_text(encoding="utf-8")
    lines = text.splitlines()
    backup: Optional[Path] = None
    applied = 0

    for p in sorted(plans, key=lambda x: (x.line1, x.line2), reverse=True):
        i1 = p.line1 - 1
        i2 = p.line2 - 1
        if i1 < 0 or i2 < 0 or i1 >= len(lines) or i2 >= len(lines) or i2 <= i1:
            continue
        first_line = lines[i1]
        indent = first_line[: len(first_line) - len(first_line.lstrip())]
        body_indent = indent + "   "
        middle = lines[i1 + 1 : i2]

        block = [f"{indent}if ({p.condition}) then", f"{body_indent}{p.action1}"]
        block.extend(middle)
        block.append(f"{body_indent}{p.action2}")
        block.append(f"{indent}end if")

        if backup is None:
            backup = make_backup_path(path)
            shutil.copy2(path, backup)
        lines[i1 : i2 + 1] = block
        applied += 1

    if applied > 0:
        out = "\n".join(lines)
        if text.endswith("\n"):
            out += "\n"
        path.write_text(out, encoding="utf-8")
    return applied, backup


def main() -> int:
    """Run repeated-condition IF checks across selected Fortran files."""
    parser = argparse.ArgumentParser(
        description="Warn about consecutive IF statements that test the same condition"
    )
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--verbose", action="store_true", help="Print all findings (default prints summary + first)")
    parser.add_argument("--fix", action="store_true", help="Rewrite repeated single-line IF pairs into IF...THEN blocks")
    parser.add_argument("--diff", action="store_true", help="With --fix, print unified diffs for changed files")
    args = parser.parse_args()

    files = choose_files(args.fortran_files, args.exclude)
    if not files:
        print("No source files remain after applying --exclude filters.")
        return 2

    all_findings: List[Finding] = []
    plans_by_file: Dict[Path, List[FixPlan]] = {}
    for path in files:
        findings, plans = analyze_file(path)
        all_findings.extend(findings)
        if plans:
            plans_by_file[path] = plans

    if not all_findings:
        print("No repeated consecutive IF-condition findings.")
        return 0

    all_findings.sort(key=lambda f: (f.path.name.lower(), f.line1, f.line2))
    print(f"{len(all_findings)} repeated consecutive IF-condition finding(s).")
    if args.verbose:
        for f in all_findings:
            print(f"{f.path.name}:{f.line1},{f.line2} repeated condition: ({f.condition})")
    else:
        by_file = {}
        for f in all_findings:
            by_file[f.path.name] = by_file.get(f.path.name, 0) + 1
        for fname in sorted(by_file.keys(), key=str.lower):
            print(f"{fname}: {by_file[fname]}")
        first = all_findings[0]
        print(
            f"\nFirst finding: {first.path.name}:{first.line1},{first.line2} "
            f"repeated condition ({first.condition})"
        )
        print("Run with --verbose to list all findings.")

    if args.fix:
        total_applied = 0
        for path in sorted(plans_by_file.keys(), key=lambda p: p.name.lower()):
            before = path.read_text(encoding="utf-8")
            applied, backup = apply_fixes_for_file(path, plans_by_file[path])
            total_applied += applied
            if applied > 0 and backup is not None:
                print(f"\nFixed {path.name}: applied {applied}, backup {backup.name}")
            elif applied > 0:
                print(f"\nFixed {path.name}: applied {applied}")
            else:
                print(f"\nNo fixes applied to {path.name}")
            if args.diff and applied > 0:
                after = path.read_text(encoding="utf-8")
                diff_lines = difflib.unified_diff(
                    before.splitlines(),
                    after.splitlines(),
                    fromfile=f"a/{path.name}",
                    tofile=f"b/{path.name}",
                    lineterm="",
                )
                print("")
                for line in diff_lines:
                    print(line)
        print(f"\n--fix summary: applied {total_applied}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
