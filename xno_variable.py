#!/usr/bin/env python3
"""Advisory checker for single-use local temporaries that can be inlined."""

from __future__ import annotations

import argparse
import difflib
import re
import shutil
import subprocess
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Set, Tuple

import cli_paths as cpaths
import fortran_build as fbuild
import fortran_scan as fscan
import xunset

TYPE_DECL_RE = re.compile(
    r"^\s*(integer|real|logical|character|complex|type\b|class\b|procedure\b)",
    re.IGNORECASE,
)
ASSIGN_RE = re.compile(r"^\s*([a-z][a-z0-9_]*(?:\s*\([^)]*\))?)\s*=\s*(.+)$", re.IGNORECASE)
ALLOCATE_RE = re.compile(r"^\s*allocate\s*\((.*)\)\s*$", re.IGNORECASE)
DEALLOCATE_RE = re.compile(r"^\s*deallocate\s*\((.*)\)\s*$", re.IGNORECASE)
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
    delete_lines: Tuple[int, ...] = ()


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


def parse_declared_locals(unit: xunset.Unit) -> Dict[str, int]:
    """Return local variable names (scalar or array) to declaration line numbers."""
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
            if "=" in text and "=>" not in text:
                text = text.split("=", 1)[0].strip()
            m = re.match(r"^([a-z][a-z0-9_]*)", text, re.IGNORECASE)
            if not m:
                continue
            name = m.group(1).lower()
            if name in unit.dummy_names:
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


def parse_allocate_source(stmt: str) -> Optional[Tuple[str, str]]:
    """Return (var, source_expr) for conservative single-object allocate(source=...)."""
    m = ALLOCATE_RE.match(stmt.strip())
    if not m:
        return None
    args = split_top_level_commas(m.group(1))
    if not args:
        return None
    object_specs: List[str] = []
    source_expr: Optional[str] = None
    for a in args:
        s = a.strip()
        if not s:
            continue
        mk = re.match(r"^([a-z][a-z0-9_]*)\s*=\s*(.+)$", s, re.IGNORECASE)
        if mk:
            k = mk.group(1).lower()
            v = mk.group(2).strip()
            # Conservative: only source= is allowed.
            if k != "source":
                return None
            source_expr = v
            continue
        object_specs.append(s)
    if len(object_specs) != 1 or source_expr is None:
        return None
    obj = object_specs[0]
    mo = re.match(r"^([a-z][a-z0-9_]*)(?:\s*\(.*\))?$", obj, re.IGNORECASE)
    if not mo:
        return None
    return mo.group(1).lower(), source_expr


def parse_allocate_no_source(stmt: str) -> Optional[str]:
    """Return var for conservative single-object allocate(var(...)) without keywords."""
    m = ALLOCATE_RE.match(stmt.strip())
    if not m:
        return None
    args = split_top_level_commas(m.group(1))
    if len(args) != 1:
        return None
    s = args[0].strip()
    # No keyword options in no-source mode.
    if re.match(r"^[a-z][a-z0-9_]*\s*=", s, re.IGNORECASE):
        return None
    mo = re.match(r"^([a-z][a-z0-9_]*)\s*\(.*\)\s*$", s, re.IGNORECASE)
    if not mo:
        return None
    return mo.group(1).lower()


def parse_deallocate_single(stmt: str) -> Optional[str]:
    """Return var for deallocate(var) with one simple object, else None."""
    m = DEALLOCATE_RE.match(stmt.strip())
    if not m:
        return None
    args = split_top_level_commas(m.group(1))
    if len(args) != 1:
        return None
    s = args[0].strip()
    mo = re.match(r"^([a-z][a-z0-9_]*)$", s, re.IGNORECASE)
    if not mo:
        return None
    return mo.group(1).lower()


def analyze_unit(unit: xunset.Unit) -> List[Finding]:
    """Analyze one unit for inline-temporary opportunities."""
    local_scalars = parse_declared_scalar_locals(unit)
    local_scalar_names = set(local_scalars.keys())
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
                    if n not in use_line_stmt:
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
                if n not in use_line_stmt:
                    use_line_stmt[n] = (ln, s)

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
        # Do not inline into self-overwrite assignment lhs.
        mm = ASSIGN_RE.match(use_stmt.strip())
        if mm and (lhs_base_name(mm.group(1)) == n):
            continue
        var_pat = re.compile(rf"\b{re.escape(n)}\b", re.IGNORECASE)
        if len(var_pat.findall(use_stmt)) != 1:
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
        suggested_stmt = var_pat.sub(expr, use_stmt, count=1)
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
                delete_lines=(),
            )
        )

    # Conservative array temporary pattern:
    #   allocate(a(...), source=expr)
    #   ... one use of a ...
    #   deallocate(a)
    # =>
    #   ... use expr directly ...
    local_vars = parse_declared_locals(unit)
    if local_vars:
        tracked = set(local_vars.keys())
        read_count: Dict[str, int] = {n: 0 for n in tracked}
        alloc_by_var: Dict[str, Tuple[int, str]] = {}
        alloc_nosrc_line_by_var: Dict[str, int] = {}
        assign_by_var: Dict[str, Tuple[int, str]] = {}
        use_line_stmt: Dict[str, Tuple[int, str]] = {}
        dealloc_line_by_var: Dict[str, int] = {}

        for ln, stmt in unit.body:
            s = stmt.strip()
            low = s.lower()
            if not low:
                continue
            if (
                TYPE_DECL_RE.match(low)
                or low.startswith("implicit ")
                or low.startswith("use ")
                or low.startswith("contains")
            ):
                continue
            m_alloc = parse_allocate_source(low)
            if m_alloc is not None:
                aname, src = m_alloc
                if aname in tracked:
                    if aname in alloc_by_var:
                        alloc_by_var[aname] = (-1, "")
                    else:
                        alloc_by_var[aname] = (ln, src)
                continue
            nsrc = parse_allocate_no_source(low)
            if nsrc is not None and nsrc in tracked:
                if nsrc in alloc_nosrc_line_by_var:
                    alloc_nosrc_line_by_var[nsrc] = -1
                else:
                    alloc_nosrc_line_by_var[nsrc] = ln
                continue
            dname = parse_deallocate_single(low)
            if dname is not None and dname in tracked:
                if dname in dealloc_line_by_var:
                    dealloc_line_by_var[dname] = -1
                else:
                    dealloc_line_by_var[dname] = ln
                continue
            m_as = ASSIGN_RE.match(s)
            if m_as:
                lhs = m_as.group(1).strip()
                rhs = m_as.group(2).strip()
                lhs_name = lhs_base_name(lhs)
                # Count reads from RHS and from LHS indices only.
                for n in tracked:
                    c_rhs = count_ident(rhs.lower(), n)
                    if c_rhs > 0:
                        read_count[n] += c_rhs
                        if n not in use_line_stmt:
                            use_line_stmt[n] = (ln, s)
                    c_lhs = count_ident(lhs.lower(), n)
                    if c_lhs > 0:
                        if lhs_name == n:
                            c_lhs -= 1
                        if c_lhs > 0:
                            read_count[n] += c_lhs
                            if n not in use_line_stmt:
                                use_line_stmt[n] = (ln, s)
                if lhs_name and lhs_name in tracked:
                    if lhs_name in assign_by_var:
                        assign_by_var[lhs_name] = (-1, "")
                    else:
                        assign_by_var[lhs_name] = (ln, rhs)
                continue

            for n in tracked:
                c = count_ident(low, n)
                if c > 0:
                    read_count[n] += c
                    if n not in use_line_stmt:
                        use_line_stmt[n] = (ln, s)

        for n, decl_ln in sorted(local_vars.items(), key=lambda kv: kv[1]):
            if n in local_scalar_names:
                continue
            has_src_alloc = n in alloc_by_var and alloc_by_var[n][0] >= 0
            has_nosrc_alloc = n in alloc_nosrc_line_by_var and alloc_nosrc_line_by_var[n] >= 0
            has_assign_only = n in assign_by_var and assign_by_var[n][0] >= 0
            if not has_src_alloc and not has_nosrc_alloc and not has_assign_only:
                continue
            al = -1
            expr = ""
            del_lines: List[int] = []
            assign_line_for_report = -1
            if has_src_alloc:
                al, expr = alloc_by_var[n]
                assign_line_for_report = al
            elif has_nosrc_alloc:
                al = alloc_nosrc_line_by_var[n]
                del_lines.append(al)
                asg = assign_by_var.get(n)
                if asg is None:
                    continue
                asl, rhs = asg
                if asl < 0:
                    continue
                expr = rhs
                assign_line_for_report = asl
            else:
                asg = assign_by_var.get(n)
                if asg is None:
                    continue
                asl, rhs = asg
                if asl < 0:
                    continue
                al = asl
                expr = rhs
                assign_line_for_report = asl
            if read_count.get(n, 0) != 1:
                continue
            use = use_line_stmt.get(n)
            if use is None:
                continue
            ul, use_stmt = use
            if ul <= al or ul <= assign_line_for_report:
                continue
            # Safety: vars referenced in source expr should not be assigned
            # between allocate and single use.
            deps = referenced_names(expr)
            deps.discard(n)
            unsafe = False
            if deps:
                for ln2, s2 in unit.body:
                    if ln2 <= assign_line_for_report or ln2 >= ul:
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
            var_pat = re.compile(rf"\b{re.escape(n)}\b", re.IGNORECASE)
            if len(var_pat.findall(use_stmt)) != 1:
                continue
            suggested_stmt = var_pat.sub(expr, use_stmt, count=1)
            dl = dealloc_line_by_var.get(n)
            if dl is not None and dl > ul:
                del_lines.append(dl)
            findings.append(
                Finding(
                    path=unit.path,
                    unit_kind=unit.kind,
                    unit_name=unit.name,
                    var=n,
                    decl_line=decl_ln,
                    assign_line=assign_line_for_report,
                    use_line=ul,
                    expr=expr,
                    use_stmt=use_stmt,
                    suggested_stmt=suggested_stmt,
                    unit_start=unit.start,
                    unit_end=unit.end,
                    delete_lines=tuple(del_lines),
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


def apply_fix_file(
    path: Path,
    findings: List[Finding],
    *,
    annotate: bool = False,
    out_path: Optional[Path] = None,
    create_backup: bool = True,
) -> Tuple[int, int, int, Optional[Path]]:
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
        for dl in f.delete_lines:
            didx = dl - 1
            if 0 <= didx < len(lines):
                delete_lines.add(didx)
        changed += 1
        decl_remove_candidates.append((f.var, f.decl_line, f.unit_start, f.unit_end, f.assign_line))

    if changed == 0:
        return 0, 0, 0, None

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

    backup: Optional[Path] = None
    if out_path is None and create_backup:
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
    target = out_path if out_path is not None else path
    target.write_text("".join(lines), encoding="utf-8")
    return changed, removed_decl_entities, annotations_inserted, backup


def compile_and_run_capture(
    source: Path,
    *,
    exe_path: Path,
    label: str,
    quiet_run: bool = False,
    keep_exe: bool = False,
) -> Tuple[bool, str, str]:
    """Compile one source file with gfortran and run produced executable."""
    compile_cmd = ["gfortran", str(source), "-o", str(exe_path)]
    print(f"Build ({label}): {' '.join(fbuild.quote_cmd_arg(x) for x in compile_cmd)}")
    cp = subprocess.run(compile_cmd, capture_output=True, text=True)
    if cp.returncode != 0:
        print(f"Build ({label}): FAIL (exit {cp.returncode})")
        if cp.stdout:
            print(cp.stdout.rstrip())
        if cp.stderr:
            print(fbuild.format_linker_stderr(cp.stderr).rstrip())
        return False, "", ""
    print(f"Build ({label}): PASS")
    print(f"Run ({label}): {fbuild.quote_cmd_arg(str(exe_path))}")
    rp = subprocess.run([str(exe_path)], capture_output=True, text=True)
    try:
        if rp.returncode != 0:
            print(f"Run ({label}): FAIL (exit {rp.returncode})")
            if rp.stdout:
                print(rp.stdout.rstrip())
            if rp.stderr:
                print(rp.stderr.rstrip())
            return False, rp.stdout or "", rp.stderr or ""
        print(f"Run ({label}): PASS")
        if not quiet_run:
            if rp.stdout:
                print(rp.stdout.rstrip())
            if rp.stderr:
                print(rp.stderr.rstrip())
        return True, rp.stdout or "", rp.stderr or ""
    finally:
        if not keep_exe:
            try:
                exe_path.unlink(missing_ok=True)
            except Exception:
                pass


def main() -> int:
    """Run advisory checks across selected files."""
    parser = argparse.ArgumentParser(
        description="Warn about single-use scalar locals that can be inlined into their only use"
    )
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--verbose", action="store_true", help="Print full suggested replacement statements")
    parser.add_argument("--fix", action="store_true", help="Inline safe candidates and remove now-unused local declarations")
    parser.add_argument("--out", type=Path, help="With --fix, write transformed output to this file (single input)")
    parser.add_argument("--tee-orig", action="store_true", help="With --fix --out, also print original source text to stdout")
    parser.add_argument("--tee", action="store_true", help="With --fix --out, also print transformed output text to stdout")
    parser.add_argument("--tee-both", action="store_true", help="With --fix --out, print original and transformed source text to stdout")
    parser.add_argument("--run", action="store_true", help="After --fix with changes, build and run transformed source")
    parser.add_argument("--run-both", action="store_true", help="Build/run original source, and build/run transformed source when changes are applied")
    parser.add_argument("--run-diff", action="store_true", help="Run original and transformed executables and compare stdout/stderr")
    parser.add_argument("--quiet-run", action="store_true", help="Do not print program stdout/stderr on successful runs")
    parser.add_argument("--keep-exe", action="store_true", help="Keep generated executables after running")
    parser.add_argument("--backup", dest="backup", action="store_true", default=True)
    parser.add_argument("--no-backup", dest="backup", action="store_false")
    parser.add_argument(
        "--annotate",
        action="store_true",
        help="With --fix, add comments below edited declarations listing removed names",
    )
    parser.add_argument("--compiler", type=str, help="Compile command for baseline/after-fix validation")
    args = parser.parse_args()
    if args.run_diff:
        args.run_both = True
    if args.run_both:
        args.run = True
    if args.tee_both:
        args.tee_orig = True
        args.tee = True
    if args.tee_orig and args.out is None:
        args.out = Path("temp.f90")
    if args.tee and args.out is None:
        args.out = Path("temp.f90")
    if args.run and args.out is None:
        args.out = Path("temp.f90")
    if args.out is not None:
        args.fix = True
    if args.annotate and not args.fix:
        print("--annotate requires --fix.")
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
    if args.run and len(files) != 1:
        print("--run/--run-both require exactly one input source file.")
        return 2
    compile_paths = [args.out] if (args.fix and args.out is not None) else files
    if args.fix and args.compiler:
        if not fbuild.run_compiler_command(args.compiler, compile_paths, "baseline", fscan.display_path):
            return 5

    findings: List[Finding] = []
    for p in files:
        findings.extend(analyze_file(p))

    if not findings:
        orig_out = ""
        orig_err = ""
        if args.run_both:
            src = files[0]
            orig_exe = Path(f"{src.stem}_orig.exe")
            ok_orig, orig_out, orig_err = compile_and_run_capture(
                src, exe_path=orig_exe, label="original", quiet_run=args.quiet_run, keep_exe=args.keep_exe
            )
            if not ok_orig:
                return 5
        if args.run_diff:
            print("Run diff: SKIP (no transformations suggested)")
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
        transformed_changed = False
        transformed_target: Optional[Path] = None
        orig_out = ""
        orig_err = ""
        xform_out = ""
        xform_err = ""
        for p in sorted(by_file.keys(), key=lambda x: x.name.lower()):
            out_path = args.out if args.out is not None else None
            before = p.read_text(encoding="utf-8")
            if out_path is not None and args.tee_orig:
                print(f"--- original: {p} ---")
                print(before, end="")
                if not before.endswith("\n"):
                    print("")
            ninl, ndecl, nann, backup = apply_fix_file(
                p, by_file[p], annotate=args.annotate, out_path=out_path, create_backup=args.backup
            )
            total_inline += ninl
            total_decl_removed += ndecl
            total_ann += nann
            if ninl > 0:
                transformed_changed = True
                changed_files += 1
                if out_path is not None:
                    print(
                        f"\nFixed {p.name}: inlined {ninl}, decl-entities removed {ndecl}, "
                        f"annotations {nann}, wrote {out_path}"
                    )
                    if args.tee:
                        print(f"--- transformed: {out_path} ---")
                        print((out_path).read_text(encoding="utf-8"), end="")
                else:
                    print(
                        f"\nFixed {p.name}: inlined {ninl}, decl-entities removed {ndecl}, "
                        f"annotations {nann}, "
                        f"backup {backup.name if backup else '(none)'}"
                    )
            else:
                print(f"\nNo fixes applied to {p.name}")
                if out_path is not None and args.tee:
                    if args.tee_orig:
                        print(f"--- transformed: {out_path} ---")
                    print((out_path).read_text(encoding="utf-8"), end="")
            transformed_target = out_path if out_path is not None else p
        print(
            f"\n--fix summary: files changed {changed_files}, "
            f"inlined {total_inline}, decl-entities removed {total_decl_removed}, "
            f"annotations {total_ann}"
        )
        if args.compiler:
            if not fbuild.run_compiler_command(args.compiler, compile_paths, "after-fix", fscan.display_path):
                return 5
        if args.run_both:
            src = files[0]
            orig_exe = Path(f"{src.stem}_orig.exe")
            ok_orig, orig_out, orig_err = compile_and_run_capture(
                src, exe_path=orig_exe, label="original", quiet_run=args.quiet_run, keep_exe=args.keep_exe
            )
            if not ok_orig:
                return 5
        if args.run and transformed_changed and transformed_target is not None:
            out_src = transformed_target
            out_exe = Path(f"{out_src.stem}.exe")
            ok_xf, xform_out, xform_err = compile_and_run_capture(
                out_src, exe_path=out_exe, label="transformed", quiet_run=args.quiet_run, keep_exe=args.keep_exe
            )
            if not ok_xf:
                return 5
        if args.run_diff:
            if not transformed_changed:
                print("Run diff: SKIP (no transformations applied)")
            else:
                same = (orig_out == xform_out) and (orig_err == xform_err)
                if same:
                    print("Run diff: MATCH")
                else:
                    print("Run diff: DIFF")
                    orig_blob = f"STDOUT:\n{orig_out}\nSTDERR:\n{orig_err}\n"
                    xform_blob = f"STDOUT:\n{xform_out}\nSTDERR:\n{xform_err}\n"
                    for line in difflib.unified_diff(
                        orig_blob.splitlines(),
                        xform_blob.splitlines(),
                        fromfile="original",
                        tofile="transformed",
                        lineterm="",
                    ):
                        print(line)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
