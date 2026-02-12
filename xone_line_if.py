#!/usr/bin/env python3
"""Find IF blocks with one executable statement that can be collapsed to one-line IF."""

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
END_IF_RE = re.compile(r"^\s*end\s*if\s*$|^\s*endif\s*$", re.IGNORECASE)
BODY_DISALLOW_RE = re.compile(r"^\s*if\b", re.IGNORECASE)
ONE_LINE_IF_RE = re.compile(r"^\s*if\s*\((.+)\)\s*(.+)$", re.IGNORECASE)
ANNOTATION_SUFFIX = "!! suggested by xone_line_if.py"


@dataclass
class Finding:
    """One collapsible IF-block candidate."""

    path: Path
    line_if: int
    line_body: int
    line_end: int
    cond: str
    body_stmt: str
    suggestion: str


@dataclass
class ReverseFinding:
    """One one-line IF candidate expandable to 3-line IF block."""

    path: Path
    line: int
    cond: str
    body_stmt: str
    original: str


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


def split_body_eol(raw: str) -> Tuple[str, str]:
    """Split a raw line into body and EOL."""
    if raw.endswith("\r\n"):
        return raw[:-2], "\r\n"
    if raw.endswith("\n"):
        return raw[:-1], "\n"
    return raw, ""


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


def analyze_file(path: Path) -> List[Finding]:
    """Collect collapsible IF-block candidates from one source file."""
    infos, any_missing = fscan.load_source_files([path])
    if not infos or any_missing:
        return []
    finfo = infos[0]
    stmts = fscan.iter_fortran_statements(finfo.parsed_lines)
    # Track statement count per physical line to avoid semicolon-split ambiguities.
    stmt_count_by_line = {}
    for ln, _ in stmts:
        stmt_count_by_line[ln] = stmt_count_by_line.get(ln, 0) + 1

    raw_lines = path.read_text(encoding="utf-8").splitlines(keepends=True)
    findings: List[Finding] = []
    i = 0
    while i + 2 < len(stmts):
        ln_if, s_if = stmts[i]
        ln_body, s_body = stmts[i + 1]
        ln_end, s_end = stmts[i + 2]

        m_if = IF_THEN_RE.match(s_if.strip())
        if not m_if:
            i += 1
            continue
        if not END_IF_RE.match(s_end.strip()):
            i += 1
            continue
        if BODY_DISALLOW_RE.match(s_body.strip()):
            i += 1
            continue

        # Conservative: exactly three contiguous physical lines.
        if not (ln_if + 1 == ln_body and ln_body + 1 == ln_end):
            i += 1
            continue

        # Conservative: each line contains exactly one parsed statement.
        if stmt_count_by_line.get(ln_if, 0) != 1 or stmt_count_by_line.get(ln_body, 0) != 1 or stmt_count_by_line.get(ln_end, 0) != 1:
            i += 1
            continue

        # Conservative: do not touch body lines with trailing comments.
        if 1 <= ln_body <= len(raw_lines):
            body_raw = raw_lines[ln_body - 1]
            body_txt, _eol = split_body_eol(body_raw)
            _code, trailing = split_code_comment(body_txt)
            if trailing.strip():
                i += 1
                continue

        cond = m_if.group(1).strip()
        body_stmt = s_body.strip()
        suggestion = f"if ({cond}) {body_stmt}"
        findings.append(
            Finding(
                path=path,
                line_if=ln_if,
                line_body=ln_body,
                line_end=ln_end,
                cond=cond,
                body_stmt=body_stmt,
                suggestion=suggestion,
            )
        )
        i += 3
    return findings


def analyze_reverse_file(path: Path) -> List[ReverseFinding]:
    """Collect one-line IF candidates for reverse expansion."""
    infos, any_missing = fscan.load_source_files([path])
    if not infos or any_missing:
        return []
    finfo = infos[0]
    stmts = fscan.iter_fortran_statements(finfo.parsed_lines)
    stmt_count_by_line = {}
    for ln, _ in stmts:
        stmt_count_by_line[ln] = stmt_count_by_line.get(ln, 0) + 1

    out: List[ReverseFinding] = []
    for ln, stmt in stmts:
        s = stmt.strip()
        m = ONE_LINE_IF_RE.match(s)
        if not m:
            continue
        cond = m.group(1).strip()
        body = m.group(2).strip()
        if not body:
            continue
        if body.lower() == "then" or body.lower().startswith("then "):
            continue
        # Conservative: skip if body starts with IF (nested one-line-if).
        if BODY_DISALLOW_RE.match(body):
            continue
        if stmt_count_by_line.get(ln, 0) != 1:
            continue
        out.append(
            ReverseFinding(
                path=path,
                line=ln,
                cond=cond,
                body_stmt=body,
                original=s,
            )
        )
    return out


def apply_edits(
    path: Path,
    findings: List[Finding],
    *,
    fix: bool,
    annotate: bool,
    out_path: Optional[Path] = None,
    create_backup: bool = True,
) -> Tuple[int, int, Optional[Path]]:
    """Apply fix/annotation edits for one file."""
    if not findings:
        return 0, 0, None
    lines = path.read_text(encoding="utf-8").splitlines(keepends=True)
    inserts_or_changes = 0
    removed_lines = 0

    for f in sorted(findings, key=lambda x: x.line_if, reverse=True):
        if_idx = f.line_if - 1
        body_idx = f.line_body - 1
        end_idx = f.line_end - 1
        if min(if_idx, body_idx, end_idx) < 0 or max(if_idx, body_idx, end_idx) >= len(lines):
            continue

        if fix:
            if_raw = lines[if_idx]
            if_body, if_eol = split_body_eol(if_raw)
            indent = re.match(r"^\s*", if_body).group(0) if if_body else ""
            eol = if_eol if if_eol else "\n"
            lines[if_idx] = f"{indent}{f.suggestion}{eol}"
            # Remove body and end-if lines.
            lines.pop(end_idx)
            lines.pop(body_idx)
            removed_lines += 2
            inserts_or_changes += 1
            if annotate:
                msg = f"{indent}! {f.suggestion} {ANNOTATION_SUFFIX}{eol}"
                lines.insert(if_idx + 1, msg)
                inserts_or_changes += 1
        elif annotate:
            end_raw = lines[end_idx]
            end_body, end_eol = split_body_eol(end_raw)
            indent = re.match(r"^\s*", end_body).group(0) if end_body else ""
            eol = end_eol if end_eol else "\n"
            msg = f"{indent}! {f.suggestion} {ANNOTATION_SUFFIX}{eol}"
            nxt = end_idx + 1
            if 0 <= nxt < len(lines) and lines[nxt].strip().lower() == msg.strip().lower():
                continue
            lines.insert(nxt, msg)
            inserts_or_changes += 1

    if inserts_or_changes == 0:
        return 0, 0, None

    backup: Optional[Path] = None
    target = out_path if (out_path is not None and fix) else path
    if create_backup and (out_path is None or not fix):
        backup = make_backup_path(path)
        shutil.copy2(path, backup)
    target.write_text("".join(lines), encoding="utf-8")
    return inserts_or_changes, removed_lines, backup


def apply_reverse_edits(
    path: Path,
    findings: List[ReverseFinding],
    *,
    fix: bool,
    annotate: bool,
    out_path: Optional[Path] = None,
    create_backup: bool = True,
) -> Tuple[int, int, Optional[Path]]:
    """Apply reverse edits (one-line IF -> block) for one file."""
    if not findings:
        return 0, 0, None
    lines = path.read_text(encoding="utf-8").splitlines(keepends=True)
    edits = 0
    inserted_lines = 0

    for f in sorted(findings, key=lambda x: x.line, reverse=True):
        idx = f.line - 1
        if idx < 0 or idx >= len(lines):
            continue
        raw = lines[idx]
        body, eol = split_body_eol(raw)
        use_eol = eol if eol else "\n"
        indent = re.match(r"^\s*", body).group(0) if body else ""
        block = [
            f"{indent}if ({f.cond}) then{use_eol}",
            f"{indent}   {f.body_stmt}{use_eol}",
            f"{indent}end if{use_eol}",
        ]
        if annotate:
            block.append(f"{indent}! if ({f.cond}) {f.body_stmt} {ANNOTATION_SUFFIX}{use_eol}")
        if fix:
            lines[idx:idx + 1] = block
            edits += 1
            inserted_lines += len(block) - 1
        elif annotate:
            msg = f"{indent}! if ({f.cond}) then ; {f.body_stmt} ; end if {ANNOTATION_SUFFIX}{use_eol}"
            if idx + 1 < len(lines) and lines[idx + 1].strip().lower() == msg.strip().lower():
                continue
            lines.insert(idx + 1, msg)
            edits += 1
            inserted_lines += 1

    if edits == 0:
        return 0, 0, None
    backup: Optional[Path] = None
    target = out_path if (out_path is not None and fix) else path
    if create_backup and (out_path is None or not fix):
        backup = make_backup_path(path)
        shutil.copy2(path, backup)
    target.write_text("".join(lines), encoding="utf-8")
    return edits, inserted_lines, backup


def main() -> int:
    """Run one-line-IF advisory/fix/annotate checks."""
    parser = argparse.ArgumentParser(description="Suggest collapsing simple IF blocks to one-line IF")
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--verbose", action="store_true", help="Print full original body and suggestion")
    parser.add_argument("--fix", action="store_true", help="Rewrite matching 3-line IF blocks to one-line IF")
    parser.add_argument("--out", type=Path, help="With --fix, write transformed output to this file (single input)")
    parser.add_argument("--backup", dest="backup", action="store_true", default=True)
    parser.add_argument("--no-backup", dest="backup", action="store_false")
    parser.add_argument("--annotate", action="store_true", help="Insert suggested one-line IF comments")
    parser.add_argument("--diff", action="store_true", help="With edits, print unified diffs")
    parser.add_argument("--reverse", action="store_true", help="Operate in reverse: one-line IF -> 3-line IF block")
    parser.add_argument("--compiler", type=str, help="Compile command for baseline/after-fix validation")
    args = parser.parse_args()
    if args.out is not None:
        args.fix = True

    if args.diff and not (args.fix or args.annotate):
        print("--diff requires --fix or --annotate.")
        return 2
    if args.compiler and not args.fix:
        print("--compiler requires --fix.")
        return 2
    if not args.fix and not args.annotate and not args.verbose:
        # advisory summary mode is allowed; no action needed
        pass

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

    if not args.reverse:
        findings: List[Finding] = []
        for p in files:
            findings.extend(analyze_file(p))

        if not findings:
            print("No one-line IF replacement candidates found.")
            return 0

        findings.sort(key=lambda f: (f.path.name.lower(), f.line_if))
        print(f"{len(findings)} one-line IF replacement candidate(s).")
        for f in findings:
            print(f"{f.path.name}:{f.line_if}-{f.line_end}")
            if args.verbose:
                print(f"  body      : {f.body_stmt}")
                print(f"  suggested : {f.suggestion}")

        if args.fix or args.annotate:
            by_file = {}
            for f in findings:
                by_file.setdefault(f.path, []).append(f)
            total_changes = 0
            total_removed = 0
            touched = 0
            for p in sorted(by_file.keys(), key=lambda x: x.name.lower()):
                before = p.read_text(encoding="utf-8")
                out_path = args.out if args.out is not None else None
                ch, rem, backup = apply_edits(
                    p,
                    by_file[p],
                    fix=args.fix,
                    annotate=args.annotate,
                    out_path=out_path,
                    create_backup=args.backup,
                )
                total_changes += ch
                total_removed += rem
                if ch > 0:
                    touched += 1
                    if out_path is not None:
                        print(f"\nEdited {p.name}: edits {ch}, removed lines {rem}, wrote {out_path}")
                    else:
                        print(
                            f"\nEdited {p.name}: edits {ch}, removed lines {rem}, backup {backup.name if backup else '(none)'}"
                        )
                    if args.diff:
                        after = (out_path if out_path is not None else p).read_text(encoding="utf-8")
                        before_disp = before.replace("\ufeff", "")
                        after_disp = after.replace("\ufeff", "")
                        diff = difflib.unified_diff(
                            before_disp.splitlines(),
                            after_disp.splitlines(),
                            fromfile=f"a/{p.name}",
                            tofile=f"b/{(out_path.name if out_path is not None else p.name)}",
                            lineterm="",
                        )
                        txt = "\n".join(diff)
                        if txt:
                            print(txt)
                else:
                    print(f"\nNo edits applied to {p.name}")
            print(
                f"\nsummary: files changed {touched}, edits {total_changes}, removed lines {total_removed}"
            )
            if args.fix and args.compiler:
                if not fbuild.run_compiler_command(args.compiler, compile_paths, "after-fix", fscan.display_path):
                    return 5
        return 0
    # Reverse mode
    rfindings: List[ReverseFinding] = []
    for p in files:
        rfindings.extend(analyze_reverse_file(p))

    if not rfindings:
        print("No reverse one-line IF expansion candidates found.")
        return 0

    rfindings.sort(key=lambda f: (f.path.name.lower(), f.line))
    print(f"{len(rfindings)} reverse one-line IF expansion candidate(s).")
    for f in rfindings:
        print(f"{f.path.name}:{f.line}")
        if args.verbose:
            print(f"  original  : {f.original}")
            print(f"  suggested : if ({f.cond}) then ; {f.body_stmt} ; end if")

    if args.fix or args.annotate:
        by_file = {}
        for f in rfindings:
            by_file.setdefault(f.path, []).append(f)
        total_changes = 0
        total_inserted = 0
        touched = 0
        for p in sorted(by_file.keys(), key=lambda x: x.name.lower()):
            before = p.read_text(encoding="utf-8")
            out_path = args.out if args.out is not None else None
            ch, ins, backup = apply_reverse_edits(
                p,
                by_file[p],
                fix=args.fix,
                annotate=args.annotate,
                out_path=out_path,
                create_backup=args.backup,
            )
            total_changes += ch
            total_inserted += ins
            if ch > 0:
                touched += 1
                if out_path is not None:
                    print(f"\nEdited {p.name}: edits {ch}, inserted lines {ins}, wrote {out_path}")
                else:
                    print(
                        f"\nEdited {p.name}: edits {ch}, inserted lines {ins}, backup {backup.name if backup else '(none)'}"
                    )
                if args.diff:
                    after = (out_path if out_path is not None else p).read_text(encoding="utf-8")
                    before_disp = before.replace("\ufeff", "")
                    after_disp = after.replace("\ufeff", "")
                    diff = difflib.unified_diff(
                        before_disp.splitlines(),
                        after_disp.splitlines(),
                        fromfile=f"a/{p.name}",
                        tofile=f"b/{(out_path.name if out_path is not None else p.name)}",
                        lineterm="",
                    )
                    txt = "\n".join(diff)
                    if txt:
                        print(txt)
            else:
                print(f"\nNo edits applied to {p.name}")
        print(
            f"\nsummary: files changed {touched}, edits {total_changes}, inserted lines {total_inserted}"
        )
        if args.fix and args.compiler:
            if not fbuild.run_compiler_command(args.compiler, compile_paths, "after-fix", fscan.display_path):
                return 5
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
