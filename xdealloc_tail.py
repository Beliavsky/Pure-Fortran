#!/usr/bin/env python3
"""Suggest deleting redundant tail deallocations of local allocatables."""

from __future__ import annotations

import argparse
import re
import shutil
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Set, Tuple

import cli_paths as cpaths
import fortran_scan as fscan
import xunset

TYPE_DECL_RE = re.compile(
    r"^\s*(integer|real|logical|character|complex|type\b|class\b|procedure\b)",
    re.IGNORECASE,
)
DEALLOCATE_RE = re.compile(r"^\s*deallocate\s*\((.*)\)\s*$", re.IGNORECASE)
STRUCTURAL_ONLY_RE = re.compile(
    r"^\s*(?:"
    r"end\s*if\b|"
    r"else\b|"
    r"else\s*if\b|"
    r"endif\b|"
    r"end\s*do\b|"
    r"enddo\b|"
    r"end\s*select\b|"
    r"endselect\b|"
    r"case\b|"
    r"contains\b|"
    r"end\s*(?:function|subroutine|program|module|block|where|associate)\b|"
    r"return\b|"
    r"stop\b|"
    r"error\s*stop\b"
    r")",
    re.IGNORECASE,
)
TAG_TEXT = "!! xdealloc_tail.py suggests deleting this line"


@dataclass
class Finding:
    """One redundant tail deallocation finding."""

    path: Path
    unit_kind: str
    unit_name: str
    line: int
    names: List[str]


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


def split_top_level_commas(text: str) -> List[str]:
    """Split text by top-level commas only."""
    out: List[str] = []
    cur: List[str] = []
    depth = 0
    in_single = False
    in_double = False
    i = 0
    while i < len(text):
        ch = text[i]
        if ch == "'" and not in_double:
            if in_single and i + 1 < len(text) and text[i + 1] == "'":
                cur.append("''")
                i += 2
                continue
            in_single = not in_single
            cur.append(ch)
            i += 1
            continue
        if ch == '"' and not in_single:
            if in_double and i + 1 < len(text) and text[i + 1] == '"':
                cur.append('""')
                i += 2
                continue
            in_double = not in_double
            cur.append(ch)
            i += 1
            continue
        if not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")" and depth > 0:
                depth -= 1
            elif ch == "," and depth == 0:
                out.append("".join(cur).strip())
                cur = []
                i += 1
                continue
        cur.append(ch)
        i += 1
    tail = "".join(cur).strip()
    if tail:
        out.append(tail)
    return out


def is_meaningful_after(stmt: str) -> bool:
    """True for statements that count as meaningful post-deallocate work."""
    low = stmt.strip().lower()
    if not low:
        return False
    if STRUCTURAL_ONLY_RE.match(low):
        return False
    return True


def collect_local_allocatables(unit: xunset.Unit) -> Set[str]:
    """Collect local allocatable variable names declared in unit."""
    out: Set[str] = set()
    for _ln, stmt in unit.body:
        low = stmt.strip().lower()
        if not low or not TYPE_DECL_RE.match(low) or "::" not in low:
            continue
        lhs = low.split("::", 1)[0]
        if "allocatable" not in lhs:
            continue
        for n in fscan.parse_declared_names_from_decl(low):
            if n in unit.dummy_names:
                continue
            out.add(n)
    return out


def parse_dealloc_names(stmt: str) -> Optional[List[str]]:
    """Parse deallocate variable names from statement."""
    m = DEALLOCATE_RE.match(stmt.strip())
    if not m:
        return None
    names: List[str] = []
    for chunk in split_top_level_commas(m.group(1)):
        n = fscan.base_identifier(chunk)
        if n:
            names.append(n)
    return names if names else None


def analyze_unit(unit: xunset.Unit) -> List[Finding]:
    """Analyze one unit for redundant tail deallocate statements."""
    local_alloc = collect_local_allocatables(unit)
    if not local_alloc:
        return []
    findings: List[Finding] = []
    body = unit.body
    for i, (ln, stmt) in enumerate(body):
        names = parse_dealloc_names(stmt)
        if not names:
            continue
        if not all(n in local_alloc for n in names):
            continue
        if any(is_meaningful_after(s2) for _l2, s2 in body[i + 1 :]):
            continue
        findings.append(
            Finding(
                path=unit.path,
                unit_kind=unit.kind,
                unit_name=unit.name,
                line=ln,
                names=names,
            )
        )
    return findings


def analyze_file(path: Path) -> List[Finding]:
    """Analyze one file."""
    infos, any_missing = fscan.load_source_files([path])
    if not infos or any_missing:
        return []
    finfo = infos[0]
    out: List[Finding] = []
    for unit in xunset.collect_units(finfo):
        out.extend(analyze_unit(unit))
    return out


def annotate_file(path: Path, findings: List[Finding]) -> Tuple[int, Optional[Path]]:
    """Append inline annotation comments on finding lines."""
    if not findings:
        return 0, None
    by_line: Dict[int, Finding] = {f.line: f for f in findings}
    lines = path.read_text(encoding="utf-8").splitlines(keepends=True)
    changed = 0
    for ln in sorted(by_line.keys()):
        idx = ln - 1
        if idx < 0 or idx >= len(lines):
            continue
        raw = lines[idx]
        body = raw.rstrip("\r\n")
        eol = raw[len(body) :]
        if TAG_TEXT.lower() in body.lower():
            continue
        lines[idx] = f"{body}  {TAG_TEXT}{eol}"
        changed += 1
    if changed == 0:
        return 0, None
    backup = make_backup_path(path)
    shutil.copy2(path, backup)
    path.write_text("".join(lines), encoding="utf-8")
    return changed, backup


def main() -> int:
    """Run tail-deallocation advisory across selected files."""
    parser = argparse.ArgumentParser(
        description="Warn about redundant deallocate(...) of local allocatables at end of program units"
    )
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--verbose", action="store_true", help="Print local names for each finding")
    parser.add_argument(
        "--annotate",
        action="store_true",
        help=f"Append inline comments: {TAG_TEXT}",
    )
    args = parser.parse_args()

    files = choose_files(args.fortran_files, args.exclude)
    if not files:
        print("No source files remain after applying --exclude filters.")
        return 2

    findings: List[Finding] = []
    for p in files:
        findings.extend(analyze_file(p))

    if not findings:
        print("No redundant tail deallocate findings.")
        return 0

    findings.sort(key=lambda f: (f.path.name.lower(), f.line))
    print(f"{len(findings)} redundant tail deallocate finding(s).")
    for f in findings:
        print(f"{f.path.name}:{f.line} {f.unit_kind} {f.unit_name}")
        if args.verbose:
            print(f"  deallocate names: {', '.join(f.names)}")

    if args.annotate:
        by_file: Dict[Path, List[Finding]] = {}
        for f in findings:
            by_file.setdefault(f.path, []).append(f)
        total = 0
        touched = 0
        for p in sorted(by_file.keys(), key=lambda x: x.name.lower()):
            n, backup = annotate_file(p, by_file[p])
            total += n
            if n > 0:
                touched += 1
                print(f"\nAnnotated {p.name}: inserted {n}, backup {backup.name if backup else '(none)'}")
            elif args.verbose:
                print(f"\nNo annotations inserted in {p.name}")
        print(f"\n--annotate summary: files changed {touched}, inserted {total}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
