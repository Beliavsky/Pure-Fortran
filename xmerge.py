#!/usr/bin/env python3
"""Suggest/fix simple IF assignment blocks to MERGE()."""

from __future__ import annotations

import argparse
import difflib
import re
import shutil
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, List, Optional, Tuple

import cli_paths as cpaths
import fortran_build as fbuild
import fortran_scan as fscan

IF_THEN_RE = re.compile(r"^\s*if\s*\((.+)\)\s*then\s*$", re.IGNORECASE)
ELSE_RE = re.compile(r"^\s*else\s*$", re.IGNORECASE)
END_IF_RE = re.compile(r"^\s*end\s*if\b|^\s*endif\b", re.IGNORECASE)
ASSIGN_RE = re.compile(r"^\s*(.+?)\s*=\s*(.+)$", re.IGNORECASE)
SIMPLE_NAME_RE = re.compile(r"^\s*([a-z][a-z0-9_]*)\s*$", re.IGNORECASE)
LOGICAL_LIT_RE = re.compile(r"^\s*\.(true|false)\.\s*$", re.IGNORECASE)
NUM_LIT_RE = re.compile(r"^\s*[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[deq][+-]?\d+)?(?:_[a-z0-9_]+)?\s*$", re.IGNORECASE)
STR_LIT_RE = re.compile(r"^\s*(?:'(?:''|[^'])*'|\"(?:\"\"|[^\"])*\")\s*$")
PRESENT_CALL_RE = re.compile(r"\bpresent\s*\(\s*([a-z][a-z0-9_]*)\s*\)", re.IGNORECASE)
NAME_TOKEN_RE = re.compile(r"\b([a-z][a-z0-9_]*)\b", re.IGNORECASE)
SUGGEST_TAG = "!! suggested by xmerge.py"
CHANGED_TAG = "!! changed by xmerge.py"


@dataclass
class Finding:
    path: Path
    line_if: int
    line_then: int
    line_else: int
    line_end: int
    lhs: str
    cond: str
    tsource: str
    fsource: str
    suggestion: str


def choose_files(args_files: List[Path], exclude: Iterable[str]) -> List[Path]:
    if args_files:
        files = cpaths.expand_source_inputs(args_files)
    else:
        files = sorted(set(Path('.').glob('*.f90')) | set(Path('.').glob('*.F90')), key=lambda p: p.name.lower())
    return fscan.apply_excludes(files, exclude)


def make_backup_path(path: Path) -> Path:
    base = Path(str(path) + '.bak')
    if not base.exists():
        return base
    i = 1
    while True:
        cand = Path(f"{path}.bak{i}")
        if not cand.exists():
            return cand
        i += 1


def split_body_eol(raw: str) -> Tuple[str, str]:
    if raw.endswith('\r\n'):
        return raw[:-2], '\r\n'
    if raw.endswith('\n'):
        return raw[:-1], '\n'
    return raw, ''


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
        if ch == '!' and not in_single and not in_double:
            return line[:i], line[i:]
        i += 1
    return line, ''


def normalize_expr(expr: str) -> str:
    return ''.join(expr.lower().split())


def is_simple_source(expr: str) -> bool:
    s = expr.strip()
    if SIMPLE_NAME_RE.match(s):
        return True
    if LOGICAL_LIT_RE.match(s):
        return True
    if NUM_LIT_RE.match(s):
        return True
    if STR_LIT_RE.match(s):
        return True
    return False


def _is_str_literal(expr: str) -> bool:
    return STR_LIT_RE.match(expr.strip()) is not None


def _str_lit_len(expr: str) -> Optional[int]:
    s = expr.strip()
    if len(s) < 2:
        return None
    quote = s[0]
    if quote not in ("'", '"') or s[-1] != quote:
        return None
    body = s[1:-1]
    if quote == "'":
        return len(body.replace("''", "'"))
    return len(body.replace('""', '"'))


def is_merge_compatible(tsource: str, fsource: str) -> bool:
    t_is_str = _is_str_literal(tsource)
    f_is_str = _is_str_literal(fsource)
    if t_is_str or f_is_str:
        # Be conservative for CHARACTER: only transform when both are
        # string literals with equal effective length.
        if not (t_is_str and f_is_str):
            return False
        lt = _str_lit_len(tsource)
        lf = _str_lit_len(fsource)
        return lt is not None and lf is not None and lt == lf
    return True


def _names_used(expr: str) -> set[str]:
    return {m.group(1).lower() for m in NAME_TOKEN_RE.finditer(expr)}


def _present_guarded_names(cond: str) -> set[str]:
    return {m.group(1).lower() for m in PRESENT_CALL_RE.finditer(cond)}


def is_present_safe(cond: str, tsource: str, fsource: str) -> bool:
    guarded = _present_guarded_names(cond)
    if not guarded:
        return True
    used = _names_used(tsource) | _names_used(fsource)
    # If either source references a symbol guarded via PRESENT(...),
    # MERGE rewrite is unsafe because both source args may be evaluated.
    return guarded.isdisjoint(used)


def analyze_file(path: Path) -> List[Finding]:
    infos, any_missing = fscan.load_source_files([path])
    if not infos or any_missing:
        return []
    finfo = infos[0]
    stmts = fscan.iter_fortran_statements(finfo.parsed_lines)
    stmt_count_by_line = {}
    for ln, _ in stmts:
        stmt_count_by_line[ln] = stmt_count_by_line.get(ln, 0) + 1

    raw_lines = path.read_text(encoding='utf-8').splitlines(keepends=True)
    out: List[Finding] = []
    i = 0
    while i + 4 < len(stmts):
        ln_if, s_if = stmts[i]
        ln_t, s_t = stmts[i + 1]
        ln_else, s_else = stmts[i + 2]
        ln_f, s_f = stmts[i + 3]
        ln_end, s_end = stmts[i + 4]

        m_if = IF_THEN_RE.match(s_if.strip())
        if not m_if:
            i += 1
            continue
        if not ELSE_RE.match(s_else.strip()) or not END_IF_RE.match(s_end.strip()):
            i += 1
            continue

        # conservative: contiguous physical lines and single statement on each
        if not (ln_if + 1 == ln_t and ln_t + 1 == ln_else and ln_else + 1 == ln_f and ln_f + 1 == ln_end):
            i += 1
            continue
        if any(stmt_count_by_line.get(ln, 0) != 1 for ln in [ln_if, ln_t, ln_else, ln_f, ln_end]):
            i += 1
            continue

        # skip trailing comments on branch assignment lines for safer rewrite
        ok_comments = True
        for ln in [ln_t, ln_f]:
            if 1 <= ln <= len(raw_lines):
                body, _eol = split_body_eol(raw_lines[ln - 1])
                _code, comment = split_code_comment(body)
                if comment.strip():
                    ok_comments = False
                    break
        if not ok_comments:
            i += 1
            continue

        mt = ASSIGN_RE.match(s_t.strip())
        mf = ASSIGN_RE.match(s_f.strip())
        if not mt or not mf:
            i += 1
            continue

        lhs_t = mt.group(1).strip()
        lhs_f = mf.group(1).strip()
        if normalize_expr(lhs_t) != normalize_expr(lhs_f):
            i += 1
            continue

        tsource = mt.group(2).strip()
        fsource = mf.group(2).strip()
        cond = m_if.group(1).strip()
        if not (is_simple_source(tsource) and is_simple_source(fsource)):
            i += 1
            continue
        if not is_merge_compatible(tsource, fsource):
            i += 1
            continue
        if not is_present_safe(cond, tsource, fsource):
            i += 1
            continue

        suggestion = f"{lhs_t} = merge({tsource}, {fsource}, {cond})"
        out.append(
            Finding(
                path=path,
                line_if=ln_if,
                line_then=ln_t,
                line_else=ln_else,
                line_end=ln_end,
                lhs=lhs_t,
                cond=cond,
                tsource=tsource,
                fsource=fsource,
                suggestion=suggestion,
            )
        )
        i += 5
    return out


def apply_fix(
    path: Path,
    findings: List[Finding],
    *,
    annotate: bool,
    out_path: Optional[Path] = None,
    create_backup: bool = True,
) -> Tuple[int, Optional[Path]]:
    if not findings:
        return 0, None
    lines = path.read_text(encoding='utf-8').splitlines(keepends=True)
    changed = 0
    for f in sorted(findings, key=lambda x: x.line_if, reverse=True):
        if_idx = f.line_if - 1
        end_idx = f.line_end - 1
        if if_idx < 0 or end_idx < 0 or if_idx >= len(lines) or end_idx >= len(lines) or if_idx > end_idx:
            continue
        raw = lines[if_idx]
        body, eol = split_body_eol(raw)
        indent = re.match(r"^\s*", body).group(0) if body else ''
        use_eol = eol if eol else '\n'
        suffix = f"  {CHANGED_TAG}" if annotate else ''
        repl = f"{indent}{f.suggestion}{suffix}{use_eol}"
        lines[if_idx:end_idx + 1] = [repl]
        changed += 1
    if changed == 0:
        return 0, None
    backup: Optional[Path] = None
    target = out_path if out_path is not None else path
    if out_path is None and create_backup:
        backup = make_backup_path(path)
        shutil.copy2(path, backup)
    target.write_text(''.join(lines), encoding='utf-8')
    return changed, backup


def apply_annotate(path: Path, findings: List[Finding], *, backup: bool = True) -> Tuple[int, Optional[Path]]:
    if not findings:
        return 0, None
    lines = path.read_text(encoding='utf-8').splitlines(keepends=True)
    inserted = 0
    for f in sorted(findings, key=lambda x: x.line_end, reverse=True):
        idx = f.line_end - 1
        if idx < 0 or idx >= len(lines):
            continue
        raw = lines[idx]
        body, eol = split_body_eol(raw)
        indent = re.match(r"^\s*", body).group(0) if body else ''
        use_eol = eol if eol else '\n'
        msg = f"{indent}! {f.suggestion}  {SUGGEST_TAG}{use_eol}"
        nxt = idx + 1
        if nxt < len(lines) and lines[nxt].strip().lower() == msg.strip().lower():
            continue
        lines.insert(nxt, msg)
        inserted += 1
    if inserted == 0:
        return 0, None
    backup_path: Optional[Path] = None
    if backup:
        backup_path = make_backup_path(path)
        shutil.copy2(path, backup_path)
    path.write_text(''.join(lines), encoding='utf-8')
    return inserted, backup_path


def main() -> int:
    parser = argparse.ArgumentParser(description='Suggest/fix simple IF assignments to MERGE()')
    parser.add_argument('fortran_files', type=Path, nargs='*')
    parser.add_argument('--exclude', action='append', default=[], help='Glob pattern to exclude files')
    parser.add_argument('--verbose', action='store_true', help='Print full suggestions')
    parser.add_argument('--fix', action='store_true', help='Apply rewrites in-place')
    parser.add_argument('--out', type=Path, help='With --fix, write transformed output to this file (single input)')
    parser.add_argument('--out-dir', type=Path, help='With --fix, write outputs to this directory')
    parser.add_argument('--backup', dest='backup', action='store_true', default=True)
    parser.add_argument('--no-backup', dest='backup', action='store_false')
    parser.add_argument('--annotate', action='store_true', help='Insert suggestion comments (or changed tag with --fix)')
    parser.add_argument('--diff', '-diff', action='store_true', help='With --fix, print unified diffs')
    parser.add_argument('--compiler', type=str, help='Compile command for baseline/after-fix validation')
    args = parser.parse_args()
    if args.out is not None and args.out_dir is not None:
        print('--out and --out-dir are mutually exclusive.')
        return 2
    if args.out is not None or args.out_dir is not None:
        args.fix = True

    if args.diff and not args.fix:
        print('--diff requires --fix.')
        return 2
    if args.compiler and not args.fix:
        print('--compiler requires --fix.')
        return 2

    files = choose_files(args.fortran_files, args.exclude)
    if not files:
        print('No source files remain after applying --exclude filters.')
        return 2
    if args.out_dir is not None:
        if args.out_dir.exists() and not args.out_dir.is_dir():
            print('--out-dir exists but is not a directory.')
            return 2
        args.out_dir.mkdir(parents=True, exist_ok=True)
    if args.out is not None and len(files) != 1:
        print('--out requires exactly one input source file.')
        return 2
    if args.fix and args.out_dir is not None:
        for p in files:
            (args.out_dir / p.name).write_text(p.read_text(encoding='utf-8'), encoding='utf-8')
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
            print(f'File not found: {p}')
            continue
        findings.extend(analyze_file(p))

    if not findings:
        print('No merge-rewrite candidates found.')
        return 0

    findings.sort(key=lambda f: (f.path.name.lower(), f.line_if))
    print(f"{len(findings)} merge-rewrite candidate(s).")
    for f in findings:
        print(f"{f.path.name}:{f.line_if}-{f.line_end} {f.lhs}")
        if args.verbose:
            print(f"  suggest: {f.suggestion}")

    if args.fix:
        by_file: Dict[Path, List[Finding]] = {}
        for f in findings:
            by_file.setdefault(f.path, []).append(f)
        touched = 0
        total = 0
        for p in sorted(by_file.keys(), key=lambda x: x.name.lower()):
            before = p.read_text(encoding='utf-8')
            out_path = args.out if args.out is not None else (args.out_dir / p.name if args.out_dir is not None else None)
            n, backup = apply_fix(
                p, by_file[p], annotate=args.annotate, out_path=out_path, create_backup=args.backup
            )
            total += n
            if n > 0:
                touched += 1
                if out_path is not None:
                    print(f"\nFixed {p.name}: rewrites {n}, wrote {out_path}")
                else:
                    print(f"\nFixed {p.name}: rewrites {n}, backup {backup.name if backup else '(none)'}")
                if args.diff:
                    after = (out_path if out_path is not None else p).read_text(encoding='utf-8')
                    diff_lines = difflib.unified_diff(
                        before.splitlines(),
                        after.splitlines(),
                        fromfile=f"a/{p.name}",
                        tofile=f"b/{(out_path.name if out_path is not None else p.name)}",
                        lineterm='',
                    )
                    print('')
                    for d in diff_lines:
                        print(d)
            elif args.verbose:
                print(f"\nNo fixes applied in {p.name}")
        print(f"\n--fix summary: files changed {touched}, rewrites {total}")
    elif args.annotate:
        by_file: Dict[Path, List[Finding]] = {}
        for f in findings:
            by_file.setdefault(f.path, []).append(f)
        touched = 0
        total = 0
        for p in sorted(by_file.keys(), key=lambda x: x.name.lower()):
            n, backup = apply_annotate(p, by_file[p], backup=args.backup)
            total += n
            if n > 0:
                touched += 1
                print(f"\nAnnotated {p.name}: inserted {n}, backup {backup.name if backup else '(none)'}")
        print(f"\n--annotate summary: files changed {touched}, inserted {total}")
    if args.fix and args.compiler:
        if not fbuild.run_compiler_command(args.compiler, compile_paths, "after-fix", fscan.display_path):
            return 5

    return 0


if __name__ == '__main__':
    raise SystemExit(main())
