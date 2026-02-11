#!/usr/bin/env python3
"""Suggest array-constructor replacements for consecutive scalar element assignments."""

from __future__ import annotations

import argparse
import difflib
import re
import shutil
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Tuple

import cli_paths as cpaths
import fortran_scan as fscan
import xunset

TYPE_DECL_RE = re.compile(
    r"^\s*(integer|real|logical|character|complex|type\b|class\b)",
    re.IGNORECASE,
)
ELEM_ASSIGN_RE = re.compile(
    r"^\s*([a-z][a-z0-9_]*)\s*\(\s*([+-]?\d+)\s*\)\s*=\s*(.+)$",
    re.IGNORECASE,
)
INDEXED_VAR_RE = re.compile(r"^\s*([a-z][a-z0-9_]*)\s*\(\s*([+-]?\d+)\s*\)\s*$", re.IGNORECASE)
UNARY_CALL_INDEXED_ARG_RE = re.compile(
    r"^\s*([a-z][a-z0-9_]*)\s*\(\s*([a-z][a-z0-9_]*)\s*\(\s*([+-]?\d+)\s*\)\s*\)\s*$",
    re.IGNORECASE,
)


@dataclass
class DeclBounds:
    """Rank-1 explicit-shape declaration bounds for one array."""

    lb: str
    ub: str


@dataclass
class Finding:
    """One candidate replacement."""

    path: Path
    unit_kind: str
    unit_name: str
    start_line: int
    end_line: int
    target: str
    suggestion: str


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


def normalize_expr(text: str) -> str:
    """Normalize expression text for conservative equality checks."""
    return "".join(text.lower().split())


def parse_rank1_entity_bounds(chunk: str) -> Optional[Tuple[str, DeclBounds]]:
    """Parse one declaration entity and return rank-1 explicit bounds when available."""
    text = chunk.strip()
    if not text:
        return None
    m_name = re.match(r"^([a-z][a-z0-9_]*)", text, re.IGNORECASE)
    if not m_name:
        return None
    name = m_name.group(1).lower()
    rest = text[m_name.end() :].lstrip()
    if not rest.startswith("("):
        return None
    close = rest.find(")")
    if close < 0:
        return None
    dims = rest[1:close].strip()
    if "," in dims:
        return None
    if ":" in dims:
        parts = dims.split(":")
        if len(parts) != 2:
            return None
        lb = parts[0].strip() or "1"
        ub = parts[1].strip()
        if not ub:
            return None
        return name, DeclBounds(lb=lb, ub=ub)
    if not dims:
        return None
    return name, DeclBounds(lb="1", ub=dims)


def collect_decl_bounds(unit: xunset.Unit) -> Dict[str, DeclBounds]:
    """Collect rank-1 explicit-shape declaration bounds inside one unit."""
    out: Dict[str, DeclBounds] = {}
    for _ln, stmt in unit.body:
        low = stmt.strip().lower()
        if not low:
            continue
        if not TYPE_DECL_RE.match(low):
            continue
        if "::" not in low:
            continue
        rhs = low.split("::", 1)[1]
        for chunk in split_top_level_commas(rhs):
            parsed = parse_rank1_entity_bounds(chunk)
            if parsed is None:
                continue
            name, b = parsed
            out.setdefault(name, b)
    return out


def maybe_build_finding(
    path: Path,
    unit: xunset.Unit,
    decl_bounds: Dict[str, DeclBounds],
    run_lines: List[int],
    run_name: str,
    run_idxs: List[int],
    run_rhs: List[str],
    *,
    allow_vector_subscript: bool,
) -> Optional[Finding]:
    """Validate one assignment run and build replacement suggestion."""
    if len(run_lines) < 2:
        return None
    if len(run_idxs) != len(set(run_idxs)):
        return None
    is_consecutive = True
    for i in range(1, len(run_idxs)):
        if run_idxs[i] != run_idxs[i - 1] + 1:
            is_consecutive = False
            break

    if not is_consecutive and not allow_vector_subscript:
        return None

    lo = run_idxs[0]
    hi = run_idxs[-1]
    rhs_joined = ", ".join(r.strip() for r in run_rhs)
    rhs_same = len({normalize_expr(r) for r in run_rhs}) == 1
    rhs_scalar = run_rhs[0].strip() if rhs_same else ""
    lhs = f"{run_name}({lo}:{hi})"
    target_full = False

    if is_consecutive:
        b = decl_bounds.get(run_name)
        if b is not None:
            if normalize_expr(b.lb) == normalize_expr(str(lo)) and normalize_expr(b.ub) == normalize_expr(str(hi)):
                lhs = run_name
                target_full = True

        rhs_expr = maybe_vectorized_rhs(run_rhs, run_idxs, lo, hi, decl_bounds, target_full=target_full)
        if rhs_expr is not None:
            suggestion = f"{lhs} = {rhs_expr}"
        elif rhs_same:
            suggestion = f"{lhs} = {rhs_scalar}"
        else:
            suggestion = f"{lhs} = [{rhs_joined}]"
    else:
        idxs = ", ".join(str(i) for i in run_idxs)
        if rhs_same:
            suggestion = f"{run_name}([{idxs}]) = {rhs_scalar}"
        else:
            suggestion = f"{run_name}([{idxs}]) = [{rhs_joined}]"
    return Finding(
        path=path,
        unit_kind=unit.kind,
        unit_name=unit.name,
        start_line=run_lines[0],
        end_line=run_lines[-1],
        target=run_name,
        suggestion=suggestion,
    )


def maybe_vectorized_rhs(
    run_rhs: List[str],
    run_idxs: List[int],
    lo: int,
    hi: int,
    decl_bounds: Dict[str, DeclBounds],
    *,
    target_full: bool,
) -> Optional[str]:
    """Return vectorized RHS when terms are uniformly arr(i) or f(arr(i))."""
    if not run_rhs or len(run_rhs) != len(run_idxs):
        return None

    func_name: Optional[str] = None
    src_arr: Optional[str] = None
    for rhs, expect_idx in zip(run_rhs, run_idxs):
        s = rhs.strip()
        m_call = UNARY_CALL_INDEXED_ARG_RE.match(s)
        if m_call:
            f_name = m_call.group(1).lower()
            arr_name = m_call.group(2).lower()
            idx = int(m_call.group(3))
            if idx != expect_idx:
                return None
            if func_name is None:
                func_name = f_name
            elif func_name != f_name:
                return None
            if src_arr is None:
                src_arr = arr_name
            elif src_arr != arr_name:
                return None
            continue

        m_idx = INDEXED_VAR_RE.match(s)
        if m_idx:
            arr_name = m_idx.group(1).lower()
            idx = int(m_idx.group(2))
            if idx != expect_idx:
                return None
            if func_name is None:
                func_name = ""
            elif func_name != "":
                return None
            if src_arr is None:
                src_arr = arr_name
            elif src_arr != arr_name:
                return None
            continue
        return None

    if src_arr is None or func_name is None:
        return None

    src_ref = f"{src_arr}({lo}:{hi})"
    sb = decl_bounds.get(src_arr)
    if target_full and sb is not None:
        if normalize_expr(sb.lb) == normalize_expr(str(lo)) and normalize_expr(sb.ub) == normalize_expr(str(hi)):
            src_ref = src_arr

    if func_name == "":
        return src_ref
    return f"{func_name}({src_ref})"


def analyze_unit(unit: xunset.Unit, *, allow_vector_subscript: bool = False) -> List[Finding]:
    """Find candidate consecutive scalar assignments in one unit."""
    decl_bounds = collect_decl_bounds(unit)
    findings: List[Finding] = []

    run_lines: List[int] = []
    run_idxs: List[int] = []
    run_rhs: List[str] = []
    run_name = ""

    def flush() -> None:
        nonlocal run_lines, run_idxs, run_rhs, run_name
        if run_lines:
            f = maybe_build_finding(
                unit.path,
                unit,
                decl_bounds,
                run_lines,
                run_name,
                run_idxs,
                run_rhs,
                allow_vector_subscript=allow_vector_subscript,
            )
            if f is not None:
                findings.append(f)
        run_lines = []
        run_idxs = []
        run_rhs = []
        run_name = ""

    for ln, stmt in unit.body:
        s = stmt.strip()
        m = ELEM_ASSIGN_RE.match(s)
        if not m:
            flush()
            continue
        name = m.group(1).lower()
        idx = int(m.group(2))
        rhs = m.group(3).strip()
        if not run_lines:
            run_name = name
            run_lines = [ln]
            run_idxs = [idx]
            run_rhs = [rhs]
            continue
        if name != run_name:
            flush()
            run_name = name
            run_lines = [ln]
            run_idxs = [idx]
            run_rhs = [rhs]
            continue
        # Require physically consecutive source lines to stay conservative.
        if ln != run_lines[-1] + 1:
            flush()
            run_name = name
            run_lines = [ln]
            run_idxs = [idx]
            run_rhs = [rhs]
            continue
        run_lines.append(ln)
        run_idxs.append(idx)
        run_rhs.append(rhs)

    flush()
    return findings


def analyze_file(path: Path, *, allow_vector_subscript: bool = False) -> List[Finding]:
    """Analyze one source file."""
    infos, any_missing = fscan.load_source_files([path])
    if not infos or any_missing:
        return []
    finfo = infos[0]
    out: List[Finding] = []
    for unit in xunset.collect_units(finfo):
        out.extend(analyze_unit(unit, allow_vector_subscript=allow_vector_subscript))
    return out


def annotate_file(path: Path, findings: List[Finding]) -> Tuple[int, Optional[Path]]:
    """Insert suggestion comments after replaced line ranges."""
    if not findings:
        return 0, None
    lines = path.read_text(encoding="utf-8").splitlines(keepends=True)
    inserts: List[Tuple[int, str]] = []

    for f in findings:
        idx = f.end_line - 1
        if idx < 0 or idx >= len(lines):
            continue
        raw = lines[idx]
        indent = re.match(r"^\s*", raw).group(0) if raw else ""
        eol = "\r\n" if raw.endswith("\r\n") else ("\n" if raw.endswith("\n") else "\n")
        msg = f"{indent}! {f.suggestion}  !! suggested by xset_array.py{eol}"
        nxt = idx + 1
        if 0 <= nxt < len(lines) and lines[nxt].strip().lower() == msg.strip().lower():
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


def apply_fix_file(path: Path, findings: List[Finding], *, annotate: bool = False) -> Tuple[int, Optional[Path]]:
    """Replace assignment runs with suggested array-assignment lines."""
    if not findings:
        return 0, None
    lines = path.read_text(encoding="utf-8").splitlines(keepends=True)
    changed = 0

    for f in sorted(findings, key=lambda x: (x.start_line, x.end_line), reverse=True):
        sidx = f.start_line - 1
        eidx = f.end_line - 1
        if sidx < 0 or eidx < 0 or sidx >= len(lines) or eidx >= len(lines) or sidx > eidx:
            continue
        raw = lines[sidx]
        indent = re.match(r"^\s*", raw).group(0) if raw else ""
        eol = "\r\n" if raw.endswith("\r\n") else ("\n" if raw.endswith("\n") else "\n")
        suffix = "  !! changed by xset_array.py" if annotate else ""
        repl = f"{indent}{f.suggestion}{suffix}{eol}"
        lines[sidx : eidx + 1] = [repl]
        changed += 1

    if changed == 0:
        return 0, None
    backup = make_backup_path(path)
    shutil.copy2(path, backup)
    path.write_text("".join(lines), encoding="utf-8")
    return changed, backup


def main() -> int:
    """Run advisory check across selected files."""
    parser = argparse.ArgumentParser(
        description="Suggest replacing consecutive scalar array-element assignments with one array assignment"
    )
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--verbose", action="store_true", help="Print suggestions per finding")
    parser.add_argument("--fix", action="store_true", help="Apply suggested replacements in-place")
    parser.add_argument("--annotate", action="store_true", help="Insert suggestion comments into source files")
    parser.add_argument("--diff", action="store_true", help="With --fix, print unified diffs for changed files")
    parser.add_argument(
        "--vector-subscript",
        action="store_true",
        help="Also suggest vector-subscript rewrites for non-consecutive element assignments",
    )
    args = parser.parse_args()
    if args.diff and not args.fix:
        print("--diff requires --fix.")
        return 2

    files = choose_files(args.fortran_files, args.exclude)
    if not files:
        print("No source files remain after applying --exclude filters.")
        return 2

    findings: List[Finding] = []
    for p in files:
        findings.extend(analyze_file(p, allow_vector_subscript=args.vector_subscript))

    if not findings:
        print("No set-array replacement candidates found.")
        return 0

    findings.sort(key=lambda f: (f.path.name.lower(), f.start_line, f.end_line, f.target))
    print(f"{len(findings)} set-array replacement candidate(s).")
    for f in findings:
        print(f"{f.path.name}:{f.start_line}-{f.end_line} {f.unit_kind} {f.unit_name} {f.target}")
        if args.verbose:
            print(f"  suggest: {f.suggestion}")

    if args.fix:
        by_file: Dict[Path, List[Finding]] = {}
        for f in findings:
            by_file.setdefault(f.path, []).append(f)
        touched = 0
        total = 0
        for p in sorted(by_file.keys(), key=lambda x: x.name.lower()):
            before = p.read_text(encoding="utf-8")
            n, backup = apply_fix_file(p, by_file[p], annotate=args.annotate)
            total += n
            if n > 0:
                touched += 1
                print(f"\nFixed {p.name}: replaced {n}, backup {backup.name if backup else '(none)'}")
                if args.diff:
                    after = p.read_text(encoding="utf-8")
                    diff_lines = difflib.unified_diff(
                        before.splitlines(),
                        after.splitlines(),
                        fromfile=f"a/{p.name}",
                        tofile=f"b/{p.name}",
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
