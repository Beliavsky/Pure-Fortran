#!/usr/bin/env python3
"""Find redundant ALLOCATE before whole-array assignment on allocatable arrays."""

from __future__ import annotations

import argparse
import difflib
import re
import shutil
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
ALLOCATE_STMT_RE = re.compile(r"^\s*allocate\s*\((.*)\)\s*$", re.IGNORECASE)
ASSIGN_RE_TEMPLATE = r"^\s*{name}\s*=\s*(?!>)(.+)$"
IDENT_RE = re.compile(r"\b([a-z][a-z0-9_]*)\b", re.IGNORECASE)
ANNOTATE_TAG = "!! commented out by xalloc_assign.py"


@dataclass
class DeclInfo:
    """Minimal declaration info for one local symbol."""

    allocatable: bool
    rank: Optional[int]


@dataclass
class Finding:
    """One redundant allocate-before-assignment candidate."""

    path: Path
    unit_kind: str
    unit_name: str
    alloc_line: int
    assign_line: int
    var: str
    assign_stmt: str
    replace_assign_rhs: Optional[str]


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


def parse_dim_rank(dim_txt: str) -> Optional[int]:
    """Return rank from a DIMENSION(...) or entity (...) spec."""
    inner = dim_txt.strip()
    if not inner:
        return None
    dims = split_top_level_commas(inner)
    if not dims:
        return None
    return len(dims)


def parse_decl_infos(unit: xunset.Unit) -> Dict[str, DeclInfo]:
    """Parse allocatable/rank facts for local declarations in one unit."""
    out: Dict[str, DeclInfo] = {}
    for _ln, stmt in unit.body:
        low = stmt.strip().lower()
        if not low or not TYPE_DECL_RE.match(low) or "::" not in low:
            continue
        lhs, rhs = low.split("::", 1)
        allocatable = bool(re.search(r"\ballocatable\b", lhs, re.IGNORECASE))
        dim_attr_rank: Optional[int] = None
        m_dim = re.search(r"\bdimension\s*\((.*)\)", lhs, re.IGNORECASE)
        if m_dim:
            dim_attr_rank = parse_dim_rank(m_dim.group(1))

        for chunk in split_top_level_commas(rhs):
            txt = chunk.strip()
            if not txt:
                continue
            if "=" in txt and "=>" not in txt:
                txt = txt.split("=", 1)[0].strip()
            m_name = re.match(r"^([a-z][a-z0-9_]*)", txt, re.IGNORECASE)
            if not m_name:
                continue
            name = m_name.group(1).lower()
            rank = dim_attr_rank
            rest = txt[m_name.end() :].lstrip()
            if rest.startswith("("):
                depth = 0
                end = -1
                for i, ch in enumerate(rest):
                    if ch == "(":
                        depth += 1
                    elif ch == ")":
                        depth -= 1
                        if depth == 0:
                            end = i
                            break
                if end > 0:
                    ent_rank = parse_dim_rank(rest[1:end])
                    if ent_rank is not None:
                        rank = ent_rank
            out[name] = DeclInfo(allocatable=allocatable, rank=rank)
    return out


def allocated_target(stmt: str) -> Optional[str]:
    """Return single allocate target name when statement is safe to simplify."""
    m = ALLOCATE_STMT_RE.match(stmt.strip())
    if not m:
        return None
    args = split_top_level_commas(m.group(1))
    if not args:
        return None

    # Ignore keyword options (stat=, errmsg=, source=, mold=) and multi-object allocate.
    object_specs: List[str] = []
    for a in args:
        s = a.strip()
        if not s:
            continue
        if re.match(r"^[a-z][a-z0-9_]*\s*=", s, re.IGNORECASE):
            return None
        object_specs.append(s)
    if len(object_specs) != 1:
        return None

    obj = object_specs[0]
    mobj = re.match(r"^([a-z][a-z0-9_]*)\s*\((.*)\)\s*$", obj, re.IGNORECASE)
    if not mobj:
        return None
    return mobj.group(1).lower()


def allocated_target_with_dim(stmt: str) -> Optional[Tuple[str, str]]:
    """Return (name, dim-spec text) for single-object ALLOCATE, or None."""
    m = ALLOCATE_STMT_RE.match(stmt.strip())
    if not m:
        return None
    args = split_top_level_commas(m.group(1))
    if not args:
        return None
    object_specs: List[str] = []
    for a in args:
        s = a.strip()
        if not s:
            continue
        if re.match(r"^[a-z][a-z0-9_]*\s*=", s, re.IGNORECASE):
            return None
        object_specs.append(s)
    if len(object_specs) != 1:
        return None
    obj = object_specs[0]
    mobj = re.match(r"^([a-z][a-z0-9_]*)\s*\((.*)\)\s*$", obj, re.IGNORECASE)
    if not mobj:
        return None
    return mobj.group(1).lower(), mobj.group(2).strip()


def assignment_rhs_of(stmt: str, name: str) -> Optional[str]:
    """Return RHS when statement is whole-variable intrinsic assignment to name."""
    m = re.match(ASSIGN_RE_TEMPLATE.format(name=re.escape(name)), stmt.strip(), re.IGNORECASE)
    if not m:
        return None
    return m.group(1).strip()


def contains_ident(expr: str, name: str) -> bool:
    """True when expr references identifier name."""
    for m in IDENT_RE.finditer(expr.lower()):
        if m.group(1).lower() == name:
            return True
    return False


def rhs_is_clearly_array(rhs: str, array_names: Set[str]) -> bool:
    """Conservative check: RHS likely carries array shape."""
    s = rhs.strip().lower()
    if s.startswith("["):
        return True
    for m in IDENT_RE.finditer(s):
        if m.group(1).lower() in array_names:
            return True
    return False


def allocate_is_singleton(dim_spec: str) -> bool:
    """True when allocate dimension text clearly allocates exactly one element."""
    d = dim_spec.strip().lower()
    if d == "1":
        return True
    m = re.match(r"^\s*([+-]?\d+)\s*:\s*([+-]?\d+)\s*$", d)
    if not m:
        return False
    try:
        lb = int(m.group(1))
        ub = int(m.group(2))
    except ValueError:
        return False
    return ub - lb + 1 == 1


def split_code_comment(line: str) -> Tuple[str, str]:
    """Split line into code and trailing comment while respecting quotes."""
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


def rewrite_assignment_rhs_line(raw: str, var: str, new_rhs: str) -> str:
    """Rewrite `var = ...` RHS in one physical line, preserving trailing comment/EOL."""
    eol = ""
    line = raw
    if line.endswith("\r\n"):
        line, eol = line[:-2], "\r\n"
    elif line.endswith("\n"):
        line, eol = line[:-1], "\n"
    code, comment = split_code_comment(line)
    m = re.match(rf"^(\s*{re.escape(var)}\s*=\s*)(?!>)(.+)$", code, re.IGNORECASE)
    if not m:
        return raw
    new_code = f"{m.group(1)}{new_rhs}"
    return f"{new_code}{comment}{eol}"


def end_line_for_stmt_start(lines: List[str], start_line: int) -> int:
    """Approximate statement end line from continuation markers."""
    i = start_line - 1
    end = start_line
    need_more = False
    while i < len(lines):
        raw = lines[i]
        code = fscan.strip_comment(raw).rstrip("\r\n")
        seg = code.rstrip()
        if i == start_line - 1 and not seg:
            return start_line
        if need_more:
            lead = seg.lstrip()
            if lead.startswith("&"):
                seg = lead[1:].lstrip()
        has_trailing_cont = seg.endswith("&")
        end = i + 1
        if has_trailing_cont:
            need_more = True
            i += 1
            continue
        break
    return end


def analyze_unit(unit: xunset.Unit) -> List[Finding]:
    """Analyze one unit for redundant allocate-before-assignment patterns."""
    decls = parse_decl_infos(unit)
    array_names: Set[str] = {n for n, d in decls.items() if d.rank is not None and d.rank >= 1}
    body = unit.body
    out: List[Finding] = []

    for i, (ln, stmt) in enumerate(body):
        alloc = allocated_target_with_dim(stmt)
        if alloc is None:
            continue
        var, dim_spec = alloc
        di = decls.get(var)
        if di is None or not di.allocatable:
            continue
        if di.rank is not None and di.rank != 1:
            continue

        if i + 1 >= len(body):
            continue
        ln2, stmt2 = body[i + 1]
        rhs = assignment_rhs_of(stmt2, var)
        if rhs is None:
            continue
        # Self-referential assignment needs existing allocation; do not suggest.
        if contains_ident(rhs, var):
            continue
        rhs_array = rhs_is_clearly_array(rhs, array_names)
        replace_rhs: Optional[str] = None
        if not rhs_array:
            # Scalar RHS only safe for allocate(...(1)) via explicit singleton constructor.
            if allocate_is_singleton(dim_spec):
                replace_rhs = f"[{rhs}]"
            else:
                continue

        out.append(
            Finding(
                path=unit.path,
                unit_kind=unit.kind,
                unit_name=unit.name,
                alloc_line=ln,
                assign_line=ln2,
                var=var,
                assign_stmt=stmt2.strip(),
                replace_assign_rhs=replace_rhs,
            )
        )
    return out


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


def apply_file_edits(
    path: Path,
    findings: List[Finding],
    *,
    fix: bool,
    annotate: bool,
    out_path: Optional[Path] = None,
    create_backup: bool = True,
) -> Tuple[int, int, Optional[Path]]:
    """Apply fix/annotation edits in one file."""
    if not findings:
        return 0, 0, None

    lines = path.read_text(encoding="utf-8").splitlines(keepends=True)
    changed = 0
    inserted = 0

    by_alloc_line: Dict[int, Finding] = {}
    for f in findings:
        by_alloc_line.setdefault(f.alloc_line, f)

    for alloc_line in sorted(by_alloc_line.keys(), reverse=True):
        f = by_alloc_line[alloc_line]
        if alloc_line < 1 or alloc_line > len(lines):
            continue

        if fix:
            # Apply assignment RHS rewrite first when needed (line numbers are stable).
            if f.replace_assign_rhs is not None and 1 <= f.assign_line <= len(lines):
                lines[f.assign_line - 1] = rewrite_assignment_rhs_line(
                    lines[f.assign_line - 1],
                    f.var,
                    f.replace_assign_rhs,
                )
            end_line = end_line_for_stmt_start(lines, alloc_line)
            for ln in range(end_line, alloc_line - 1, -1):
                idx = ln - 1
                raw = lines[idx]
                body = raw
                eol = ""
                if body.endswith("\r\n"):
                    body, eol = body[:-2], "\r\n"
                elif body.endswith("\n"):
                    body, eol = body[:-1], "\n"
                if body.lstrip().startswith("!"):
                    continue
                indent = re.match(r"^\s*", body).group(0) if body else ""
                code = body[len(indent):]
                suffix = f"  {ANNOTATE_TAG}" if annotate else ""
                lines[idx] = f"{indent}! {code}{suffix}{eol}"
                changed += 1
        elif annotate:
            idx = alloc_line - 1
            raw = lines[idx]
            body = raw.rstrip("\r\n")
            eol = "\n"
            if raw.endswith("\r\n"):
                eol = "\r\n"
            indent = re.match(r"^\s*", body).group(0) if body else ""
            msg = f"{indent}! remove redundant allocate for {f.var}; assignment on line {f.assign_line}  !! suggested by xalloc_assign.py{eol}"
            if idx + 1 < len(lines) and lines[idx + 1].strip().lower() == msg.strip().lower():
                continue
            lines.insert(idx + 1, msg)
            inserted += 1

    if changed == 0 and inserted == 0:
        return 0, 0, None

    backup: Optional[Path] = None
    target = out_path if (out_path is not None and fix) else path
    if create_backup and (out_path is None or not fix):
        backup = make_backup_path(path)
        shutil.copy2(path, backup)
    target.write_text("".join(lines), encoding="utf-8")
    return changed, inserted, backup


def main() -> int:
    """Run advisory/fix pass for redundant allocate-before-assignment."""
    parser = argparse.ArgumentParser(
        description="Find ALLOCATE statements that can be removed before whole-array assignment"
    )
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--verbose", action="store_true", help="Print full assignment statement lines")
    parser.add_argument("--fix", action="store_true", help="Comment out redundant allocate lines")
    parser.add_argument("--out", type=Path, help="With --fix, write transformed output to this file (single input)")
    parser.add_argument("--backup", dest="backup", action="store_true", default=True)
    parser.add_argument("--no-backup", dest="backup", action="store_false")
    parser.add_argument("--annotate", action="store_true", help="Annotate findings (or tagged comments with --fix)")
    parser.add_argument("--diff", action="store_true", help="With --fix, print unified diffs for changed files")
    parser.add_argument("--compiler", type=str, help="Compile command for baseline/after-fix validation")
    args = parser.parse_args()
    if args.out is not None:
        args.fix = True

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
        if not p.exists():
            print(f"File not found: {p}")
            continue
        findings.extend(analyze_file(p))

    if not findings:
        print("No redundant allocate-before-assignment candidates found.")
        return 0

    findings.sort(key=lambda f: (f.path.name.lower(), f.alloc_line, f.var))
    print(f"{len(findings)} candidate(s).")
    for f in findings:
        print(
            f"{f.path.name}:{f.alloc_line}->{f.assign_line} {f.unit_kind} {f.unit_name} {f.var} "
            f"(allocate before whole-array assignment)"
        )
        if args.verbose:
            print(f"  {f.assign_stmt}")
            if f.replace_assign_rhs is not None:
                print(f"  rewrite : {f.var} = {f.replace_assign_rhs}")

    if not args.fix and not args.annotate:
        return 0

    by_file: Dict[Path, List[Finding]] = {}
    for f in findings:
        by_file.setdefault(f.path, []).append(f)

    changed_files = 0
    changed_lines = 0
    inserted_notes = 0
    for path, fs in sorted(by_file.items(), key=lambda kv: kv[0].name.lower()):
        before = path.read_text(encoding="utf-8")
        out_path = args.out if args.out is not None else None
        n_changed, n_ins, backup = apply_file_edits(
            path, fs, fix=args.fix, annotate=args.annotate, out_path=out_path, create_backup=args.backup
        )
        if n_changed == 0 and n_ins == 0:
            continue
        changed_files += 1
        changed_lines += n_changed
        inserted_notes += n_ins
        if args.fix:
            if out_path is not None:
                print(f"Fixed {path.name}: commented {n_changed} line(s) (wrote: {out_path})")
            else:
                print(f"Fixed {path.name}: commented {n_changed} line(s) (backup: {backup.name})")
            if args.diff:
                after = (out_path if out_path is not None else path).read_text(encoding="utf-8")
                diff = difflib.unified_diff(
                    before.splitlines(),
                    after.splitlines(),
                    fromfile=path.name,
                    tofile=f"{(out_path.name if out_path is not None else path.name)} (updated)",
                    lineterm="",
                )
                print("\n".join(diff))
        else:
            print(f"Annotated {path.name}: inserted {n_ins} note(s) (backup: {backup.name})")

    mode = "--fix" if args.fix else "--annotate"
    print(
        f"\n{mode} summary: files changed {changed_files}, "
        f"lines commented {changed_lines}, notes inserted {inserted_notes}"
    )
    if args.fix and args.compiler:
        if not fbuild.run_compiler_command(args.compiler, compile_paths, "after-fix", fscan.display_path):
            return 5
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
