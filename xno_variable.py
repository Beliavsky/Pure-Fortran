#!/usr/bin/env python3
"""Advisory checker for single-use local temporaries that can be inlined."""

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
ASSIGN_RE = re.compile(r"^\s*([a-z][a-z0-9_]*(?:\s*\([^)]*\))?)\s*=\s*(.+)$", re.IGNORECASE)
PLAIN_LHS_RE = re.compile(r"^\s*([a-z][a-z0-9_]*)\s*$", re.IGNORECASE)
IDENT_RE = re.compile(r"\b([a-z][a-z0-9_]*)\b", re.IGNORECASE)
LHS_BASE_RE = re.compile(r"^\s*([a-z][a-z0-9_]*)", re.IGNORECASE)


@dataclass
class Finding:
    """One inline-temporary advisory finding."""

    path: Path
    unit_kind: str
    unit_name: str
    var: str
    decl_line: int
    assign_line: int
    use_line: int
    expr: str
    use_stmt: str
    suggested_stmt: str
    unit_start: int
    unit_end: int


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
    """Split declaration RHS on top-level commas only."""
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


def parse_declared_scalar_locals(unit: xunset.Unit) -> Dict[str, int]:
    """Return scalar local variable names to declaration line numbers."""
    out: Dict[str, int] = {}
    for ln, stmt in unit.body:
        low = stmt.strip().lower()
        if not low or not TYPE_DECL_RE.match(low):
            continue
        if "::" not in low:
            continue
        rhs = low.split("::", 1)[1]
        for chunk in split_top_level_commas(rhs):
            text = chunk.strip()
            if not text:
                continue
            # Skip initialized entities.
            if "=" in text and "=>" not in text:
                text = text.split("=", 1)[0].strip()
            m = re.match(r"^([a-z][a-z0-9_]*)(.*)$", text, re.IGNORECASE)
            if not m:
                continue
            name = m.group(1).lower()
            tail = m.group(2).strip()
            if name in unit.dummy_names:
                continue
            # Scalar-only: no explicit dimensions in entity declarator.
            if tail.startswith("("):
                continue
            out.setdefault(name, ln)
    return out


def strip_wrapping_parens(text: str) -> str:
    """Strip balanced wrapping parentheses repeatedly."""
    s = text.strip()
    while s.startswith("(") and s.endswith(")"):
        depth = 0
        ok = True
        for i, ch in enumerate(s):
            if ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
                if depth == 0 and i != len(s) - 1:
                    ok = False
                    break
        if not ok or depth != 0:
            break
        s = s[1:-1].strip()
    return s


def rhs_is_var(rhs: str, var: str) -> bool:
    """True if RHS is exactly var (optionally wrapped in parentheses)."""
    s = strip_wrapping_parens(rhs)
    return s.lower() == var.lower()


def count_ident(text: str, name: str) -> int:
    """Count identifier occurrences of name in text."""
    n = 0
    for m in IDENT_RE.finditer(text.lower()):
        if m.group(1) == name.lower():
            n += 1
    return n


def referenced_names(expr: str) -> Set[str]:
    """Extract identifier names referenced in expression text."""
    out: Set[str] = set()
    for m in IDENT_RE.finditer(expr.lower()):
        out.add(m.group(1))
    return out


def lhs_base_name(lhs: str) -> Optional[str]:
    """Return base variable name from assignment LHS."""
    m = LHS_BASE_RE.match(lhs.strip())
    if not m:
        return None
    return m.group(1).lower()


def analyze_unit(unit: xunset.Unit) -> List[Finding]:
    """Analyze one unit for inline-temporary opportunities."""
    local_scalars = parse_declared_scalar_locals(unit)
    if not local_scalars:
        return []

    assign_lines: Dict[str, Tuple[int, str]] = {}
    read_count: Dict[str, int] = {n: 0 for n in local_scalars}
    use_line_stmt: Dict[str, Tuple[int, str]] = {}
    stmt_by_line: Dict[int, str] = {}

    for ln, stmt in unit.body:
        s = stmt.strip()
        stmt_by_line[ln] = s
        low = s.lower()
        if (
            TYPE_DECL_RE.match(low)
            or low.startswith("implicit ")
            or low.startswith("use ")
            or low.startswith("contains")
        ):
            continue
        m = ASSIGN_RE.match(s)
        if m:
            lhs = m.group(1).strip()
            rhs = m.group(2).strip()
            mplain = PLAIN_LHS_RE.match(lhs)
            lhs_name = mplain.group(1).lower() if mplain else None

            # Count reads from RHS.
            for n in local_scalars:
                c = count_ident(rhs, n)
                if c > 0:
                    read_count[n] += c
                    if rhs_is_var(rhs, n) and n not in use_line_stmt:
                        use_line_stmt[n] = (ln, s)

            if lhs_name and lhs_name in local_scalars:
                if lhs_name in assign_lines:
                    # More than one assignment to temp -> disable candidate.
                    assign_lines[lhs_name] = (-1, "")
                else:
                    assign_lines[lhs_name] = (ln, rhs)
            continue

        # Non-assignment statements: any variable mention counts as read/use.
        for n in local_scalars:
            c = count_ident(low, n)
            if c > 0:
                read_count[n] += c

    findings: List[Finding] = []
    for n, decl_ln in sorted(local_scalars.items(), key=lambda kv: kv[1]):
        if n not in assign_lines:
            continue
        al, expr = assign_lines[n]
        if al < 0:
            continue
        if read_count.get(n, 0) != 1:
            continue
        use = use_line_stmt.get(n)
        if use is None:
            continue
        ul, use_stmt = use
        if ul <= al:
            continue
        mm = ASSIGN_RE.match(use_stmt)
        if not mm:
            continue
        lhs = mm.group(1).strip()
        rhs = mm.group(2).strip()
        if not rhs_is_var(rhs, n):
            continue
        # Safety: variables referenced in temp RHS must not be written between
        # temp assignment and its only use (avoids invalid swap-style inlining).
        deps = referenced_names(expr)
        deps.discard(n)
        unsafe = False
        if deps:
            for ln2, s2 in unit.body:
                if ln2 <= al or ln2 >= ul:
                    continue
                m2 = ASSIGN_RE.match(s2.strip())
                if not m2:
                    continue
                b2 = lhs_base_name(m2.group(1))
                if b2 is not None and b2 in deps:
                    unsafe = True
                    break
        if unsafe:
            continue
        suggested_stmt = f"{lhs} = {expr}"
        findings.append(
            Finding(
                path=unit.path,
                unit_kind=unit.kind,
                unit_name=unit.name,
                var=n,
                decl_line=decl_ln,
                assign_line=al,
                use_line=ul,
                expr=expr,
                use_stmt=use_stmt,
                suggested_stmt=suggested_stmt,
                unit_start=unit.start,
                unit_end=unit.end,
            )
        )
    return findings


def analyze_file(path: Path) -> List[Finding]:
    """Analyze one file and return findings."""
    infos, any_missing = fscan.load_source_files([path])
    if not infos or any_missing:
        return []
    finfo = infos[0]
    out: List[Finding] = []
    for unit in xunset.collect_units(finfo):
        out.extend(analyze_unit(unit))
    return out


def split_code_comment(line: str) -> Tuple[str, str]:
    """Split one source line into code and trailing comment."""
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


def decl_entity_name(chunk: str) -> Optional[str]:
    """Extract declaration entity base name from one RHS chunk."""
    text = chunk.strip()
    if "=" in text and "=>" not in text:
        text = text.split("=", 1)[0].strip()
    m = re.match(r"^([a-z][a-z0-9_]*)", text, re.IGNORECASE)
    if not m:
        return None
    return m.group(1).lower()


def apply_fix_file(path: Path, findings: List[Finding], *, annotate: bool = False) -> Tuple[int, int, int, Optional[Path]]:
    """Apply inlining fixes and declaration cleanup for one source file."""
    if not findings:
        return 0, 0, 0, None
    lines = path.read_text(encoding="utf-8").splitlines(keepends=True)
    changed = 0
    removed_decl_entities = 0
    annotations_inserted = 0

    used_use_lines: Set[int] = set()
    delete_lines: Set[int] = set()
    decl_remove_candidates: List[Tuple[str, int, int, int, int]] = []
    # tuple: (var, decl_line, unit_start, unit_end, assign_line)

    for f in sorted(findings, key=lambda x: (x.use_line, x.assign_line, x.var)):
        uidx = f.use_line - 1
        aidx = f.assign_line - 1
        if uidx < 0 or uidx >= len(lines) or aidx < 0 or aidx >= len(lines):
            continue
        if f.use_line in used_use_lines:
            continue
        raw = lines[uidx]
        body = raw.rstrip("\r\n")
        eol = raw[len(body) :]
        indent = re.match(r"^\s*", body).group(0) if body else ""
        lines[uidx] = f"{indent}{f.suggested_stmt}{eol}"
        used_use_lines.add(f.use_line)
        delete_lines.add(aidx)
        changed += 1
        decl_remove_candidates.append((f.var, f.decl_line, f.unit_start, f.unit_end, f.assign_line))

    if changed == 0:
        return 0, 0, None

    # Determine which temp declarations are now unused within their units.
    decl_remove_by_line: Dict[int, Set[str]] = {}
    for var, decl_line, ustart, uend, assign_line in decl_remove_candidates:
        var_pat = re.compile(rf"\b{re.escape(var)}\b", re.IGNORECASE)
        still_used = False
        lo = max(1, ustart)
        hi = min(len(lines), uend)
        for ln in range(lo, hi + 1):
            idx = ln - 1
            if idx in delete_lines:
                continue
            if ln == decl_line:
                continue
            if ln == assign_line:
                continue
            code = fscan.strip_comment(lines[idx].rstrip("\r\n"))
            if var_pat.search(code):
                still_used = True
                break
        if not still_used:
            decl_remove_by_line.setdefault(decl_line - 1, set()).add(var)

    # Remove declaration entities conservatively.
    anno_by_decl_line: Dict[int, List[str]] = {}
    for didx, rm_vars in sorted(decl_remove_by_line.items()):
        if didx < 0 or didx >= len(lines):
            continue
        if didx in delete_lines:
            continue
        raw = lines[didx]
        body = raw.rstrip("\r\n")
        eol = raw[len(body) :]
        code, comment = split_code_comment(body)
        if "::" not in code:
            continue
        lhs, rhs = code.split("::", 1)
        chunks = split_top_level_commas(rhs)
        if not chunks:
            continue
        kept: List[str] = []
        removed_here = 0
        for ch in chunks:
            n = decl_entity_name(ch)
            if n is not None and n in rm_vars:
                removed_here += 1
                anno_by_decl_line.setdefault(didx, []).append(n)
                continue
            kept.append(ch.strip())
        if removed_here == 0:
            continue
        removed_decl_entities += removed_here
        if not kept:
            delete_lines.add(didx)
            continue
        new_code = f"{lhs.rstrip()} :: {', '.join(kept)}"
        lines[didx] = f"{new_code}{comment}{eol}"

    pending_annos: List[Tuple[int, str]] = []
    if annotate and anno_by_decl_line:
        for didx, names in sorted(anno_by_decl_line.items()):
            if didx < 0 or didx >= len(lines):
                continue
            raw = lines[didx]
            body = raw.rstrip("\r\n")
            eol = raw[len(body) :]
            indent = re.match(r"^\s*", body).group(0) if body else ""
            uniq = sorted(set(names), key=str.lower)
            msg = f"{indent}!! {', '.join(uniq)} removed by xno_variable.py{eol}"
            pending_annos.append((didx, msg))

    backup = make_backup_path(path)
    shutil.copy2(path, backup)

    for idx in sorted(delete_lines, reverse=True):
        if 0 <= idx < len(lines):
            lines.pop(idx)

    if annotate and pending_annos:
        dels = sorted(d for d in delete_lines if 0 <= d)
        inserts: List[Tuple[int, str]] = []
        for didx, msg in pending_annos:
            if didx in delete_lines:
                # Declaration line removed: place comment at the same logical position.
                shift = sum(1 for d in dels if d <= didx)
                at = didx - shift
            else:
                # Declaration line kept: place comment below it.
                shift = sum(1 for d in dels if d < didx)
                at = didx - shift + 1
            if at < 0:
                at = 0
            if at > len(lines):
                at = len(lines)
            if 0 <= at < len(lines) and lines[at].strip().lower() == msg.strip().lower():
                continue
            inserts.append((at, msg))
        for at, msg in sorted(inserts, key=lambda x: x[0], reverse=True):
            lines.insert(at, msg)
            annotations_inserted += 1
    path.write_text("".join(lines), encoding="utf-8")
    return changed, removed_decl_entities, annotations_inserted, backup


def main() -> int:
    """Run advisory checks across selected files."""
    parser = argparse.ArgumentParser(
        description="Warn about single-use scalar locals that can be inlined into their only use"
    )
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--verbose", action="store_true", help="Print full suggested replacement statements")
    parser.add_argument("--fix", action="store_true", help="Inline safe candidates and remove now-unused local declarations")
    parser.add_argument(
        "--annotate",
        action="store_true",
        help="With --fix, add comments below edited declarations listing removed names",
    )
    args = parser.parse_args()
    if args.annotate and not args.fix:
        print("--annotate requires --fix.")
        return 2

    files = choose_files(args.fortran_files, args.exclude)
    if not files:
        print("No source files remain after applying --exclude filters.")
        return 2

    findings: List[Finding] = []
    for p in files:
        findings.extend(analyze_file(p))

    if not findings:
        print("No inline-temporary candidates found.")
        return 0

    findings.sort(key=lambda f: (f.path.name.lower(), f.use_line, f.var))
    print(f"{len(findings)} inline-temporary candidate(s).")
    for f in findings:
        print(
            f"{f.path.name}:{f.assign_line}->{f.use_line} {f.unit_kind} {f.unit_name} "
            f"{f.var} (decl@{f.decl_line})"
        )
        if args.verbose:
            print(f"  current use: {f.use_stmt}")
            print(f"  suggest    : {f.suggested_stmt}")

    if args.fix:
        by_file: Dict[Path, List[Finding]] = {}
        for f in findings:
            by_file.setdefault(f.path, []).append(f)
        total_inline = 0
        total_decl_removed = 0
        total_ann = 0
        changed_files = 0
        for p in sorted(by_file.keys(), key=lambda x: x.name.lower()):
            ninl, ndecl, nann, backup = apply_fix_file(p, by_file[p], annotate=args.annotate)
            total_inline += ninl
            total_decl_removed += ndecl
            total_ann += nann
            if ninl > 0:
                changed_files += 1
                print(
                    f"\nFixed {p.name}: inlined {ninl}, decl-entities removed {ndecl}, "
                    f"annotations {nann}, "
                    f"backup {backup.name if backup else '(none)'}"
                )
            else:
                print(f"\nNo fixes applied to {p.name}")
        print(
            f"\n--fix summary: files changed {changed_files}, "
            f"inlined {total_inline}, decl-entities removed {total_decl_removed}, "
            f"annotations {total_ann}"
        )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
