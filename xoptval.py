#!/usr/bin/env python3
"""Suggest/fix optional-argument defaulting patterns via stdlib optval()."""

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

ASSIGN_SIMPLE_RE = re.compile(r"^\s*([a-z][a-z0-9_]*)\s*=\s*(.+)$", re.IGNORECASE)
ONE_LINE_PRESENT_ASSIGN_RE = re.compile(
    r"^\s*if\s*\(\s*present\s*\(\s*([a-z][a-z0-9_]*)\s*\)\s*\)\s*([a-z][a-z0-9_]*)\s*=\s*(.+)$",
    re.IGNORECASE,
)
IF_PRESENT_THEN_RE = re.compile(
    r"^\s*if\s*\(\s*present\s*\(\s*([a-z][a-z0-9_]*)\s*\)\s*\)\s*then\s*$",
    re.IGNORECASE,
)
ELSE_RE = re.compile(r"^\s*else\s*$", re.IGNORECASE)
END_IF_RE = re.compile(r"^\s*end\s*if\b|^\s*endif\b", re.IGNORECASE)
SIMPLE_NAME_RE = re.compile(r"^\s*([a-z][a-z0-9_]*)\s*$", re.IGNORECASE)
CALL_LIKE_RE = re.compile(r"\b([a-z][a-z0-9_]*)\s*\(", re.IGNORECASE)
MODULE_START_RE = re.compile(r"^\s*module\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
END_MODULE_RE = re.compile(r"^\s*end\s*module\b", re.IGNORECASE)
CONTAINS_RE = re.compile(r"^\s*contains\s*$", re.IGNORECASE)
USE_STDLIB_OPTVAL_RE = re.compile(r"^\s*use\b.*\bstdlib_optval\b", re.IGNORECASE)
ONLY_RE = re.compile(r"\bonly\s*:\s*(.*)$", re.IGNORECASE)

ANNOTATE_SUGGEST = "!! suggested by xoptval.py"
ANNOTATE_CHANGED = "!! changed by xoptval.py"


@dataclass
class Finding:
    path: Path
    line_start: int
    line_end: int
    rule: str
    var: str
    opt_name: str
    default_expr: str
    suggestion: str


@dataclass
class ModuleBlock:
    name: str
    start: int
    end: int
    contains: int


def choose_files(args_files: List[Path], exclude: Iterable[str]) -> List[Path]:
    if args_files:
        files = cpaths.expand_source_inputs(args_files)
    else:
        files = sorted(set(Path(".").glob("*.f90")) | set(Path(".").glob("*.F90")), key=lambda p: p.name.lower())
    return fscan.apply_excludes(files, exclude)


def split_code_comment(line: str) -> Tuple[str, str]:
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


def make_backup_path(path: Path) -> Path:
    base = Path(str(path) + ".bak")
    if not base.exists():
        return base
    i = 1
    while True:
        cand = Path(f"{path}.bak{i}")
        if not cand.exists():
            return cand
        i += 1


def is_safe_default_expr(expr: str) -> bool:
    """Conservative: avoid function-call defaults that may have side effects."""
    s = expr.strip()
    if not s:
        return False
    # Calls look like name(...). Allow parenthesized arithmetic without call names.
    # If any call-like token appears, skip.
    if CALL_LIKE_RE.search(s):
        return False
    return True


def parse_modules(lines: List[str]) -> List[ModuleBlock]:
    out: List[ModuleBlock] = []
    stack: List[Tuple[str, int, int]] = []  # name, start, contains
    for i, raw in enumerate(lines, start=1):
        code, _ = split_code_comment(raw)
        low = code.strip().lower()
        if not low:
            continue
        m = MODULE_START_RE.match(low)
        if m:
            toks = low.split()
            if len(toks) >= 2 and toks[1] != "procedure":
                stack.append((m.group(1).lower(), i, -1))
            continue
        if CONTAINS_RE.match(low) and stack:
            name, s, c = stack[-1]
            if c < 0:
                stack[-1] = (name, s, i)
            continue
        if END_MODULE_RE.match(low) and stack:
            name, s, c = stack.pop()
            out.append(ModuleBlock(name=name, start=s, end=i, contains=(c if c > 0 else i)))
    out.sort(key=lambda b: b.start)
    return out


def module_for_line(mods: List[ModuleBlock], line_no: int) -> Optional[str]:
    for m in mods:
        if m.start <= line_no <= m.end:
            return m.name
    return None


def module_optval_usage(lines: List[str]) -> Set[str]:
    """Return module names that reference optval(...) anywhere in module scope/procedures."""
    mods = parse_modules(lines)
    out: Set[str] = set()
    pat = re.compile(r"\boptval\s*\(", re.IGNORECASE)
    for i, raw in enumerate(lines, start=1):
        code, _ = split_code_comment(raw)
        if not pat.search(code):
            continue
        mname = module_for_line(mods, i)
        if mname:
            out.add(mname)
    return out


def analyze_file(path: Path) -> List[Finding]:
    infos, any_missing = fscan.load_source_files([path])
    if not infos or any_missing:
        return []
    finfo = infos[0]
    stmts = fscan.iter_fortran_statements(finfo.parsed_lines)
    out: List[Finding] = []

    i = 0
    while i < len(stmts):
        ln, st = stmts[i]
        s = st.strip()

        # Pattern A:
        # v = default
        # if (present(opt)) v = opt
        m1 = ASSIGN_SIMPLE_RE.match(s)
        if m1 and i + 1 < len(stmts):
            var = m1.group(1).lower()
            default_expr = m1.group(2).strip()
            ln2, st2 = stmts[i + 1]
            m2 = ONE_LINE_PRESENT_ASSIGN_RE.match(st2.strip())
            if m2:
                opt_name = m2.group(1).lower()
                var2 = m2.group(2).lower()
                rhs2 = m2.group(3).strip()
                rhs2_name = SIMPLE_NAME_RE.match(rhs2)
                if var2 == var and rhs2_name and rhs2_name.group(1).lower() == opt_name and is_safe_default_expr(default_expr):
                    sugg = f"{var} = optval({opt_name}, {default_expr})"
                    out.append(
                        Finding(
                            path=path,
                            line_start=ln,
                            line_end=ln2,
                            rule="assign_then_present",
                            var=var,
                            opt_name=opt_name,
                            default_expr=default_expr,
                            suggestion=sugg,
                        )
                    )
                    i += 2
                    continue

        # Pattern B:
        # if (present(opt)) then
        #   v = opt
        # else
        #   v = default
        # end if
        m_if = IF_PRESENT_THEN_RE.match(s)
        if m_if and i + 4 < len(stmts):
            opt = m_if.group(1).lower()
            ln1, st1 = stmts[i + 1]
            ln2, st2 = stmts[i + 2]
            ln3, st3 = stmts[i + 3]
            ln4, st4 = stmts[i + 4]
            m_a = ASSIGN_SIMPLE_RE.match(st1.strip())
            m_b = ASSIGN_SIMPLE_RE.match(st3.strip())
            if m_a and ELSE_RE.match(st2.strip()) and m_b and END_IF_RE.match(st4.strip()):
                var_a = m_a.group(1).lower()
                rhs_a = m_a.group(2).strip()
                var_b = m_b.group(1).lower()
                rhs_b = m_b.group(2).strip()
                if var_a == var_b:
                    ra = SIMPLE_NAME_RE.match(rhs_a)
                    rb = SIMPLE_NAME_RE.match(rhs_b)
                    default_expr = ""
                    if ra and ra.group(1).lower() == opt and is_safe_default_expr(rhs_b):
                        default_expr = rhs_b
                    elif rb and rb.group(1).lower() == opt and is_safe_default_expr(rhs_a):
                        default_expr = rhs_a
                    if default_expr:
                        sugg = f"{var_a} = optval({opt}, {default_expr})"
                        out.append(
                            Finding(
                                path=path,
                                line_start=ln,
                                line_end=ln4,
                                rule="if_present_block",
                                var=var_a,
                                opt_name=opt,
                                default_expr=default_expr,
                                suggestion=sugg,
                            )
                        )
                        i += 5
                        continue
        i += 1

    return out


def ensure_optval_use(lines: List[str], module_names: Set[str]) -> Tuple[List[str], int]:
    """Ensure module-level use stdlib_optval, only: optval for selected modules."""
    if not module_names:
        return lines, 0
    out = list(lines)
    added = 0
    remove_idx: Set[int] = set()
    modules = parse_modules(out)

    for m in sorted([mm for mm in modules if mm.name in module_names], key=lambda x: x.start, reverse=True):
        # Search module spec (start+1 ... contains-1) for existing use.
        spec_lo = m.start + 1
        spec_hi = max(m.start, m.contains - 1)
        has_use = False
        patched = False
        last_use = m.start
        first_implicit = -1
        optval_use_line = -1

        for ln in range(spec_lo, spec_hi + 1):
            idx = ln - 1
            if idx < 0 or idx >= len(out):
                continue
            raw = out[idx]
            code, comment = split_code_comment(raw.rstrip("\r\n"))
            low = code.strip().lower()
            if not low:
                continue
            if low.startswith("use "):
                last_use = ln
            if low.startswith("implicit none") and first_implicit < 0:
                first_implicit = ln
            if not USE_STDLIB_OPTVAL_RE.match(low):
                continue
            has_use = True
            optval_use_line = ln
            m_only = ONLY_RE.search(low)
            if m_only is None:
                patched = True
                break
            only_txt = m_only.group(1)
            names = [t.strip().lower() for t in only_txt.split(",") if t.strip()]
            if "optval" in names:
                patched = True
                break
            # Patch this use line by appending optval.
            eol = "\n"
            if raw.endswith("\r\n"):
                eol = "\r\n"
            elif raw.endswith("\n"):
                eol = "\n"
            base = raw.rstrip("\r\n")
            out[idx] = f"{base}, optval{eol}"
            patched = True
            added += 1
            break

        # Mark misplaced/duplicate stdlib_optval USE lines outside module spec section.
        for ln in range(m.contains + 1, m.end):
            idx = ln - 1
            if idx < 0 or idx >= len(out):
                continue
            code, _comment = split_code_comment(out[idx].rstrip("\r\n"))
            if USE_STDLIB_OPTVAL_RE.match(code.strip().lower()):
                remove_idx.add(idx)

        if not has_use:
            ref = out[m.start - 1]
            indent = re.match(r"^\s*", ref).group(0) if ref else ""
            eol = "\r\n" if ref.endswith("\r\n") else ("\n" if ref.endswith("\n") else "\n")
            line = f"{indent}use stdlib_optval, only: optval{eol}"
            # USE statements must appear before IMPLICIT NONE.
            insert_at = (first_implicit - 1) if first_implicit > 0 else last_use
            out.insert(insert_at, line)
            added += 1
        elif first_implicit > 0 and optval_use_line > first_implicit:
            # Move misplaced USE above IMPLICIT NONE.
            idx_from = optval_use_line - 1
            moved = out.pop(idx_from)
            idx_to = first_implicit - 1
            out.insert(idx_to, moved)
            added += 1

    if remove_idx:
        for idx in sorted(remove_idx, reverse=True):
            out.pop(idx)
            added += 1

    return out, added


def apply_fix_file(
    path: Path,
    findings: List[Finding],
    *,
    annotate: bool,
    ensure_use_modules: Optional[Set[str]] = None,
    out_path: Optional[Path] = None,
    create_backup: bool = True,
) -> Tuple[int, int, Optional[Path]]:
    if not findings and not ensure_use_modules:
        return 0, 0, None

    lines = path.read_text(encoding="utf-8").splitlines(keepends=True)
    original_lines = list(lines)
    mods = parse_modules(lines)
    need_modules: Set[str] = set()

    changed_blocks = 0
    for f in sorted(findings, key=lambda x: (x.line_start, x.line_end), reverse=True):
        sidx = f.line_start - 1
        eidx = f.line_end - 1
        if sidx < 0 or eidx < 0 or sidx >= len(lines) or eidx >= len(lines) or sidx > eidx:
            continue
        raw = lines[sidx]
        indent = re.match(r"^\s*", raw).group(0) if raw else ""
        eol = "\r\n" if raw.endswith("\r\n") else ("\n" if raw.endswith("\n") else "\n")
        suffix = f"  {ANNOTATE_CHANGED}" if annotate else ""
        repl = f"{indent}{f.suggestion}{suffix}{eol}"
        lines[sidx : eidx + 1] = [repl]
        changed_blocks += 1
        mname = module_for_line(mods, f.line_start)
        if mname:
            need_modules.add(mname)

    if ensure_use_modules:
        need_modules.update(ensure_use_modules)

    lines2, use_changes = ensure_optval_use(lines, need_modules)

    if lines2 == original_lines and changed_blocks == 0:
        return 0, 0, None

    backup: Optional[Path] = None
    target = out_path if out_path is not None else path
    if out_path is None and create_backup:
        backup = make_backup_path(path)
        shutil.copy2(path, backup)
    target.write_text("".join(lines2), encoding="utf-8")
    return changed_blocks, use_changes, backup


def apply_annotate_file(path: Path, findings: List[Finding], *, backup: bool = True) -> Tuple[int, Optional[Path]]:
    if not findings:
        return 0, None
    lines = path.read_text(encoding="utf-8").splitlines(keepends=True)
    inserted = 0
    for f in sorted(findings, key=lambda x: (x.line_start, x.line_end), reverse=True):
        idx = f.line_end - 1
        if idx < 0 or idx >= len(lines):
            continue
        raw = lines[idx]
        indent = re.match(r"^\s*", raw).group(0) if raw else ""
        eol = "\r\n" if raw.endswith("\r\n") else ("\n" if raw.endswith("\n") else "\n")
        msg = f"{indent}! {f.suggestion}  {ANNOTATE_SUGGEST}{eol}"
        if idx + 1 < len(lines) and lines[idx + 1].strip().lower() == msg.strip().lower():
            continue
        lines.insert(idx + 1, msg)
        inserted += 1
    if inserted == 0:
        return 0, None
    backup_path: Optional[Path] = None
    if backup:
        backup_path = make_backup_path(path)
        shutil.copy2(path, backup_path)
    path.write_text("".join(lines), encoding="utf-8")
    return inserted, backup_path


def main() -> int:
    parser = argparse.ArgumentParser(description="Suggest/fix optional-defaulting code using stdlib optval()")
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--verbose", action="store_true", help="Print full suggestions and statements")
    parser.add_argument("--fix", action="store_true", help="Apply rewrites in-place and add required use statements")
    parser.add_argument("--out", type=Path, help="With --fix, write transformed output to this file (single input)")
    parser.add_argument("--out-dir", type=Path, help="With --fix, write outputs to this directory")
    parser.add_argument("--backup", dest="backup", action="store_true", default=True)
    parser.add_argument("--no-backup", dest="backup", action="store_false")
    parser.add_argument("--annotate", action="store_true", help="Insert suggestion comments (or changed tags with --fix)")
    parser.add_argument("--diff", action="store_true", help="With --fix, print unified diffs for changed files")
    parser.add_argument("--compiler", type=str, help="Compile command for baseline/after-fix validation")
    args = parser.parse_args()
    if args.out is not None and args.out_dir is not None:
        print("--out and --out-dir are mutually exclusive.")
        return 2
    if args.out is not None or args.out_dir is not None:
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
        if not p.exists():
            print(f"File not found: {p}")
            continue
        findings.extend(analyze_file(p))

    if not findings and not args.fix:
        print("No optval replacement candidates found.")
        return 0

    if findings:
        findings.sort(key=lambda f: (f.path.name.lower(), f.line_start, f.line_end))
        print(f"{len(findings)} optval replacement candidate(s).")
        for f in findings:
            print(f"{f.path.name}:{f.line_start}-{f.line_end} {f.rule} {f.var} <- optval({f.opt_name}, ...)" )
            if args.verbose:
                print(f"  suggest: {f.suggestion}")
    else:
        print("No optval replacement candidates found; checking module-level optval imports.")

    if args.fix:
        by_file: Dict[Path, List[Finding]] = {p: [] for p in files if p.exists()}
        for f in findings:
            by_file.setdefault(f.path, []).append(f)
        changed_files = 0
        total_blocks = 0
        total_use = 0
        for p in sorted(by_file.keys(), key=lambda x: x.name.lower()):
            before = p.read_text(encoding="utf-8")
            ensure_mods = module_optval_usage(p.read_text(encoding="utf-8").splitlines())
            out_path = args.out if args.out is not None else (args.out_dir / p.name if args.out_dir is not None else None)
            nblocks, nuse, backup = apply_fix_file(
                p,
                by_file[p],
                annotate=args.annotate,
                ensure_use_modules=ensure_mods,
                out_path=out_path,
                create_backup=args.backup,
            )
            if nblocks == 0 and nuse == 0:
                if args.verbose:
                    print(f"\nNo fixes applied to {p.name}")
                continue
            changed_files += 1
            total_blocks += nblocks
            total_use += nuse
            if out_path is not None:
                print(f"\nFixed {p.name}: rewrites {nblocks}, use-edits {nuse}, wrote {out_path}")
            else:
                print(f"\nFixed {p.name}: rewrites {nblocks}, use-edits {nuse}, backup {backup.name if backup else '(none)'}")
            if args.diff:
                after = (out_path if out_path is not None else p).read_text(encoding="utf-8")
                diff = difflib.unified_diff(
                    before.splitlines(),
                    after.splitlines(),
                    fromfile=f"a/{p.name}",
                    tofile=f"b/{(out_path.name if out_path is not None else p.name)}",
                    lineterm="",
                )
                print("")
                for d in diff:
                    print(d)
        print(f"\n--fix summary: files changed {changed_files}, rewrites {total_blocks}, use-edits {total_use}")
    elif args.annotate and findings:
        by_file: Dict[Path, List[Finding]] = {}
        for f in findings:
            by_file.setdefault(f.path, []).append(f)
        touched = 0
        total = 0
        for p in sorted(by_file.keys(), key=lambda x: x.name.lower()):
            n, backup = apply_annotate_file(p, by_file[p], backup=args.backup)
            total += n
            if n > 0:
                touched += 1
                print(f"\nAnnotated {p.name}: inserted {n}, backup {backup.name if backup else '(none)'}")
        print(f"\n--annotate summary: files changed {touched}, inserted {total}")

    if args.fix and args.compiler:
        if not fbuild.run_compiler_command(args.compiler, compile_paths, "after-fix", fscan.display_path):
            return 5

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
