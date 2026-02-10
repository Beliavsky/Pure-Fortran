#!/usr/bin/env python3
"""Advisory checker for potential early DEALLOCATE opportunities in Fortran."""

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
ALLOCATE_RE = re.compile(r"^\s*allocate\s*\((.*)\)\s*$", re.IGNORECASE)
DEALLOCATE_RE = re.compile(r"^\s*deallocate\s*\((.*)\)\s*$", re.IGNORECASE)
ANNOTATED_DEALLOC_RE = re.compile(
    r"^(\s*)!\s*deallocate\s*\((.*?)\)\s*!!\s*suggested by xdealloc\.py\s*$",
    re.IGNORECASE,
)
IDENT_RE = re.compile(r"\b([a-z][a-z0-9_]*)\b", re.IGNORECASE)
DO_START_RE = re.compile(r"^\s*do\b", re.IGNORECASE)
END_DO_RE = re.compile(r"^\s*end\s*do\b", re.IGNORECASE)
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
    r"end\s*(?:function|subroutine|program|module|block|where|associate)\b"
    r")",
    re.IGNORECASE,
)


@dataclass
class Finding:
    """One advisory finding for a possible early deallocate."""

    path: Path
    unit_kind: str
    unit_name: str
    name: str
    decl_line: int
    allocate_line: int
    last_use_line: int
    suggest_after_line: int


@dataclass
class Exclusion:
    """Why a declared allocatable was not suggested."""

    path: Path
    unit_kind: str
    unit_name: str
    name: str
    decl_line: int
    reason: str


def unit_key(kind: str, name: str, start: int) -> Tuple[str, str, int]:
    """Build stable key for unit/procedure maps."""
    return (kind.lower(), name.lower(), start)


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


def split_top_level_commas(text: str) -> List[str]:
    """Split text on top-level commas only."""
    out: List[str] = []
    cur: List[str] = []
    depth = 0
    in_single = False
    in_double = False
    for ch in text:
        if ch == "'" and not in_double:
            in_single = not in_single
        elif ch == '"' and not in_single:
            in_double = not in_double
        elif not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")" and depth > 0:
                depth -= 1
            elif ch == "," and depth == 0:
                out.append("".join(cur).strip())
                cur = []
                continue
        cur.append(ch)
    tail = "".join(cur).strip()
    if tail:
        out.append(tail)
    return out


def first_identifier(expr: str) -> Optional[str]:
    """Return leading identifier in expression, if any."""
    return fscan.base_identifier(expr)


def contains_name(stmt: str, name: str) -> bool:
    """Check whether statement references identifier name."""
    low = stmt.lower()
    pat = rf"\b{re.escape(name.lower())}\b"
    return re.search(pat, low) is not None


def is_meaningful_after_stmt(stmt: str) -> bool:
    """Return True for non-structural statements that count as useful code."""
    low = stmt.strip().lower()
    if not low:
        return False
    if STRUCTURAL_ONLY_RE.match(low):
        return False
    return True


def analyze_unit(unit: xunset.Unit, function_result_name: Optional[str] = None) -> Tuple[List[Finding], List[Exclusion]]:
    """Analyze one program unit for early deallocate opportunities."""
    alloc_decl_line: Dict[str, int] = {}
    alloc_count: Dict[str, int] = {}
    dealloc_count: Dict[str, int] = {}
    first_alloc_line: Dict[str, int] = {}
    refs: Dict[str, List[int]] = {}
    line_in_loop: Dict[int, bool] = {}
    do_depth = 0

    for ln, stmt in unit.body:
        low = stmt.strip().lower()
        if not low:
            continue
        if END_DO_RE.match(low):
            do_depth = max(0, do_depth - 1)
        line_in_loop[ln] = do_depth > 0

        if TYPE_DECL_RE.match(low) and "::" in low:
            lhs = low.split("::", 1)[0]
            if "allocatable" in lhs:
                for n in fscan.parse_declared_names_from_decl(low):
                    if n in unit.dummy_names:
                        continue
                    alloc_decl_line.setdefault(n, ln)
                    refs.setdefault(n, [])
                    alloc_count.setdefault(n, 0)
                    dealloc_count.setdefault(n, 0)
            continue

        if DO_START_RE.match(low):
            do_depth += 1
            continue

        m_alloc = ALLOCATE_RE.match(low)
        if m_alloc:
            for chunk in split_top_level_commas(m_alloc.group(1)):
                n = first_identifier(chunk)
                if not n or n not in alloc_decl_line:
                    continue
                alloc_count[n] = alloc_count.get(n, 0) + 1
                first_alloc_line.setdefault(n, ln)
                refs.setdefault(n, []).append(ln)
            continue

        m_dealloc = DEALLOCATE_RE.match(low)
        if m_dealloc:
            for chunk in split_top_level_commas(m_dealloc.group(1)):
                n = first_identifier(chunk)
                if not n or n not in alloc_decl_line:
                    continue
                dealloc_count[n] = dealloc_count.get(n, 0) + 1
                refs.setdefault(n, []).append(ln)
            continue

        for n in alloc_decl_line.keys():
            if contains_name(low, n):
                refs.setdefault(n, []).append(ln)

    findings: List[Finding] = []
    exclusions: List[Exclusion] = []

    for n, decl_ln in sorted(alloc_decl_line.items(), key=lambda kv: kv[1]):
        if unit.kind.lower() == "function":
            result_name = (function_result_name or unit.name).lower()
            if n.lower() == result_name:
                exclusions.append(
                    Exclusion(
                        path=unit.path,
                        unit_kind=unit.kind,
                        unit_name=unit.name,
                        name=n,
                        decl_line=decl_ln,
                        reason="function result variable",
                    )
                )
                continue
        a_count = alloc_count.get(n, 0)
        d_count = dealloc_count.get(n, 0)
        if a_count == 0:
            reason = "never allocated"
        elif a_count > 1:
            reason = f"allocated {a_count} times"
        elif d_count > 0:
            reason = f"already deallocated ({d_count} time(s))"
        else:
            al = first_alloc_line.get(n, -1)
            use_lines = [x for x in refs.get(n, []) if x >= al]
            if not use_lines:
                reason = "no tracked uses after allocation"
            else:
                last_use = max(use_lines)
                if last_use >= unit.end:
                    reason = "last use reaches unit end"
                elif line_in_loop.get(last_use, False):
                    reason = "last use occurs inside loop"
                elif not any(
                    (ln > last_use and ln < unit.end and is_meaningful_after_stmt(stmt))
                    for ln, stmt in unit.body
                ):
                    reason = "no meaningful code after last use before unit end"
                else:
                    findings.append(
                        Finding(
                            path=unit.path,
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            name=n,
                            decl_line=decl_ln,
                            allocate_line=al,
                            last_use_line=last_use,
                            suggest_after_line=last_use,
                        )
                    )
                    continue
        exclusions.append(
            Exclusion(
                path=unit.path,
                unit_kind=unit.kind,
                unit_name=unit.name,
                name=n,
                decl_line=decl_ln,
                reason=reason,
            )
        )

    return findings, exclusions


def annotate_file(
    path: Path,
    findings: List[Finding],
    *,
    active_statements: bool = False,
    keep_trailing_tag: bool = True,
) -> Tuple[int, Optional[Path]]:
    """Insert advisory deallocate lines after suggestion lines."""
    if not findings:
        return 0, None
    lines = path.read_text(encoding="utf-8").splitlines(keepends=True)
    by_anchor: Dict[Tuple[str, str, int], List[str]] = {}
    for f in findings:
        key = (f.unit_kind, f.unit_name, f.suggest_after_line)
        by_anchor.setdefault(key, []).append(f.name)

    inserts: List[Tuple[int, str]] = []
    for (uk, un, anchor), names in by_anchor.items():
        uniq = sorted(set(names), key=str.lower)
        idx = anchor - 1
        if idx < 0 or idx >= len(lines):
            continue
        raw = lines[idx]
        indent = re.match(r"^\s*", raw).group(0) if raw else ""
        eol = "\r\n" if raw.endswith("\r\n") else ("\n" if raw.endswith("\n") else "\n")
        if active_statements:
            if keep_trailing_tag:
                msg = f"{indent}deallocate ({', '.join(uniq)}) !! suggested by xdealloc.py{eol}"
            else:
                msg = f"{indent}deallocate ({', '.join(uniq)}){eol}"
        else:
            msg = f"{indent}! deallocate ({', '.join(uniq)}) !! suggested by xdealloc.py{eol}"
        # Avoid duplicate adjacent annotation.
        next_idx = idx + 1
        if 0 <= next_idx < len(lines) and lines[next_idx].strip().lower() == msg.strip().lower():
            continue
        inserts.append((idx + 1, msg))

    if not inserts:
        return 0, None

    backup = make_backup_path(path)
    shutil.copy2(path, backup)

    for at, msg in sorted(inserts, key=lambda x: x[0], reverse=True):
        lines.insert(at, msg)
    path.write_text("".join(lines), encoding="utf-8")
    return len(inserts), backup


def apply_fix_file(path: Path) -> Tuple[int, Optional[Path]]:
    """Activate xdealloc annotations by converting them to deallocate statements."""
    lines = path.read_text(encoding="utf-8").splitlines(keepends=True)
    changed = 0
    out: List[str] = []
    for raw in lines:
        body = raw.rstrip("\r\n")
        eol = raw[len(body):]
        m = ANNOTATED_DEALLOC_RE.match(body)
        if not m:
            out.append(raw)
            continue
        indent, names = m.group(1), m.group(2).strip()
        out.append(f"{indent}deallocate ({names}){eol}")
        changed += 1

    if changed == 0:
        return 0, None

    backup = make_backup_path(path)
    shutil.copy2(path, backup)
    path.write_text("".join(out), encoding="utf-8")
    return changed, backup


def main() -> int:
    """Run advisory early-deallocate checks across selected files."""
    parser = argparse.ArgumentParser(
        description="Advisory checker for local allocatables that may be deallocated earlier"
    )
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--verbose", action="store_true", help="Print all findings and exclusions")
    parser.add_argument(
        "--annotate",
        action="store_true",
        help="Insert suggested deallocation lines from findings "
        "(comments by default; active statements when combined with --fix)",
    )
    parser.add_argument(
        "--fix",
        action="store_true",
        help="Convert existing xdealloc comment annotations into active statements "
        "(plain statements when used alone)",
    )
    args = parser.parse_args()

    files = choose_files(args.fortran_files, args.exclude)
    if not files:
        print("No source files remain after applying --exclude filters.")
        return 2

    infos, any_missing = fscan.load_source_files(files)
    if not infos:
        return 2 if any_missing else 1
    ordered_infos, _ = fscan.order_files_least_dependent(infos)

    findings: List[Finding] = []
    exclusions: List[Exclusion] = []
    for finfo in ordered_infos:
        func_result_by_unit: Dict[Tuple[str, str, int], str] = {}
        for p in finfo.procedures:
            if p.kind.lower() == "function":
                func_result_by_unit[unit_key(p.kind, p.name, p.start)] = (p.result_name or p.name).lower()
        for unit in xunset.collect_units(finfo):
            rname = func_result_by_unit.get(unit_key(unit.kind, unit.name, unit.start))
            fnd, ex = analyze_unit(unit, function_result_name=rname)
            findings.extend(fnd)
            exclusions.extend(ex)

    if not findings:
        print("No early DEALLOCATE advisory findings.")
    else:
        findings.sort(key=lambda x: (x.path.name.lower(), x.suggest_after_line, x.name))
        print(f"{len(findings)} early DEALLOCATE advisory finding(s).")
        for f in findings:
            print(
                f"{f.path.name}:{f.suggest_after_line} {f.unit_kind} {f.unit_name} {f.name} "
                f"(decl@{f.decl_line}, alloc@{f.allocate_line}, last-use@{f.last_use_line}) "
                f"-> suggest: deallocate({f.name}) after line {f.suggest_after_line}"
            )

    if args.verbose and exclusions:
        exclusions.sort(key=lambda x: (x.path.name.lower(), x.decl_line, x.name))
        print("\nExcluded allocatables:")
        for e in exclusions:
            print(f"{e.path.name}:{e.decl_line} {e.unit_kind} {e.unit_name} {e.name} - {e.reason}")

    did_annotate_pass = False
    if args.annotate and findings:
        did_annotate_pass = True
        by_file: Dict[Path, List[Finding]] = {}
        for f in findings:
            by_file.setdefault(f.path, []).append(f)
        total = 0
        for p in sorted(by_file.keys(), key=lambda x: x.name.lower()):
            n, backup = annotate_file(
                p,
                by_file[p],
                active_statements=args.fix,
                keep_trailing_tag=args.fix,
            )
            total += n
            if n > 0 and backup is not None:
                print(f"\nAnnotated {p.name}: inserted {n}, backup {backup.name}")
            elif n > 0:
                print(f"\nAnnotated {p.name}: inserted {n}")
            else:
                print(f"\nNo annotations inserted in {p.name}")
        print(f"\n--annotate summary: inserted {total}")

    if args.fix and not did_annotate_pass:
        total_fixed = 0
        touched = 0
        for p in sorted(files, key=lambda x: x.name.lower()):
            n, backup = apply_fix_file(p)
            total_fixed += n
            if n > 0:
                touched += 1
                if backup is not None:
                    print(f"Fixed {p.name}: activated {n}, backup {backup.name}")
                else:
                    print(f"Fixed {p.name}: activated {n}")
            elif args.verbose:
                print(f"No xdealloc annotations found in {p.name}")
        print(f"\n--fix summary: files changed {touched}, statements activated {total_fixed}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
