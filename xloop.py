#!/usr/bin/env python3
"""Suggest/fix replacing labeled DO loops with END DO termination."""

from __future__ import annotations

import argparse
import difflib
import re
import shutil
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Tuple

import cli_paths as cpaths
import fortran_build as fbuild
import fortran_scan as fscan

DO_LABEL_STMT_RE = re.compile(r"^\s*do\s*(\d+)\s+(.+)$", re.IGNORECASE)
LABEL_LINE_RE = re.compile(r"^(?P<indent>\s*)(?P<label>\d+)(?P<sep>\s+)(?P<stmt>.*)$")
INDENT_STEP = "   "


@dataclass
class LabeledLine:
    indent: str
    label: Optional[str]
    sep: str
    stmt: str
    code: str
    comment: str
    eol: str


@dataclass
class LoopCandidate:
    path: Path
    do_line: int
    end_line: int
    target_label: str


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


def choose_files(args_files: List[Path], exclude: Iterable[str]) -> List[Path]:
    if args_files:
        files = cpaths.expand_path_args(args_files)
    else:
        files = sorted(set(Path(".").glob("*.f90")) | set(Path(".").glob("*.F90")), key=lambda p: p.name.lower())
    return fscan.apply_excludes(files, exclude)


def split_code_comment(line_noeol: str) -> Tuple[str, str]:
    in_single = False
    in_double = False
    i = 0
    while i < len(line_noeol):
        ch = line_noeol[i]
        if ch == "'" and not in_double:
            if in_single and i + 1 < len(line_noeol) and line_noeol[i + 1] == "'":
                i += 2
                continue
            in_single = not in_single
            i += 1
            continue
        if ch == '"' and not in_single:
            if in_double and i + 1 < len(line_noeol) and line_noeol[i + 1] == '"':
                i += 2
                continue
            in_double = not in_double
            i += 1
            continue
        if ch == "!" and not in_single and not in_double:
            return line_noeol[:i], line_noeol[i:]
        i += 1
    return line_noeol, ""


def split_eol(raw: str) -> Tuple[str, str]:
    if raw.endswith("\r\n"):
        return raw[:-2], "\r\n"
    if raw.endswith("\n"):
        return raw[:-1], "\n"
    return raw, ""


def parse_labeled_line(raw: str) -> LabeledLine:
    body, eol = split_eol(raw)
    code, comment = split_code_comment(body)
    m = LABEL_LINE_RE.match(code)
    if m:
        return LabeledLine(
            indent=m.group("indent"),
            label=m.group("label"),
            sep=m.group("sep"),
            stmt=m.group("stmt"),
            code=code,
            comment=comment,
            eol=eol,
        )
    return LabeledLine(
        indent=re.match(r"^\s*", code).group(0) if code else "",
        label=None,
        sep="",
        stmt=code,
        code=code,
        comment=comment,
        eol=eol,
    )


def stmt_has_semicolon(stmt: str) -> bool:
    return ";" in stmt


def do_header_info(stmt: str) -> Optional[Tuple[str, str]]:
    m = DO_LABEL_STMT_RE.match(stmt.strip())
    if not m:
        return None
    target = m.group(1)
    rest = m.group(2).rstrip()
    if not rest:
        return None
    return target, rest


def analyze_file(path: Path) -> Tuple[List[LoopCandidate], Dict[int, Tuple[str, str]], Dict[int, List[int]]]:
    raw_lines = path.read_text(encoding="utf-8", errors="ignore").splitlines(keepends=True)
    parsed = [parse_labeled_line(ln) for ln in raw_lines]

    # Header line -> (target_label, rest_clause)
    do_headers: Dict[int, Tuple[str, str]] = {}
    # Header line -> indent string to use for end do
    do_indents: Dict[int, str] = {}

    stack: List[Tuple[int, str]] = []
    bad_do_lines: set[int] = set()
    closures: Dict[int, List[int]] = {}

    for idx0, pl in enumerate(parsed):
        line_no = idx0 + 1
        stmt = pl.stmt
        if stmt_has_semicolon(stmt):
            continue

        info = do_header_info(stmt)
        if info is not None:
            target, rest = info
            do_headers[line_no] = (target, rest)
            # Indent where DO keyword begins in code segment.
            low_code = pl.code.lower()
            pos_do = low_code.find("do")
            do_indents[line_no] = (pl.code[:pos_do] if pos_do >= 0 else pl.indent)
            stack.append((line_no, target))

        if pl.label is not None:
            lbl = pl.label
            # If target label appears deeper than top, this is non-structured crossing.
            if any(t == lbl for _dl, t in stack) and (not stack or stack[-1][1] != lbl):
                for dl, t in stack:
                    if t == lbl:
                        bad_do_lines.add(dl)
            while stack and stack[-1][1] == lbl:
                do_line, _target = stack.pop()
                closures.setdefault(line_no, []).append(do_line)

    # Unmatched loops are skipped.
    for do_line, _target in stack:
        bad_do_lines.add(do_line)

    candidates: List[LoopCandidate] = []
    for end_line, do_lines in closures.items():
        for do_line in do_lines:
            if do_line in bad_do_lines:
                continue
            target, _rest = do_headers.get(do_line, ("", ""))
            if not target:
                continue
            candidates.append(LoopCandidate(path=path, do_line=do_line, end_line=end_line, target_label=target))

    return candidates, do_headers, closures


def apply_fix(
    path: Path,
    candidates: List[LoopCandidate],
    do_headers: Dict[int, Tuple[str, str]],
    closures: Dict[int, List[int]],
    *,
    out_path: Optional[Path] = None,
    create_backup: bool = True,
) -> Tuple[int, Optional[Path]]:
    if not candidates:
        if out_path is not None:
            out_path.write_text(path.read_text(encoding="utf-8", errors="ignore"), encoding="utf-8")
        return 0, None

    raw_lines = path.read_text(encoding="utf-8", errors="ignore").splitlines(keepends=True)
    parsed = [parse_labeled_line(ln) for ln in raw_lines]
    cand_do_lines = {c.do_line for c in candidates}

    # Rewrite DO headers.
    changed = 0
    for do_line in sorted(cand_do_lines, reverse=True):
        idx = do_line - 1
        pl = parsed[idx]
        target, rest = do_headers[do_line]
        _ = target
        stmt_lead = re.match(r"^\s*", pl.stmt).group(0) if pl.stmt else ""
        new_stmt = f"{stmt_lead}do {rest}".rstrip()
        if pl.label is not None:
            new_code = f"{pl.indent}{pl.label}{pl.sep}{new_stmt}"
        else:
            new_code = new_stmt
        raw_lines[idx] = f"{new_code}{pl.comment}{pl.eol}"
        changed += 1

    # Build per-terminator end-do insertions only for converted loops.
    close_map: Dict[int, List[int]] = {}
    for end_line, do_lines in closures.items():
        kept = [dl for dl in do_lines if dl in cand_do_lines]
        if kept:
            close_map[end_line] = kept

    # Decide whether a terminating statement label can be removed safely.
    parsed_now = [parse_labeled_line(ln) for ln in raw_lines]
    converted_target_by_do: Dict[int, str] = {dl: do_headers[dl][0] for dl in cand_do_lines if dl in do_headers}
    all_do_by_target: Dict[str, List[int]] = {}
    for dl, (target, _rest) in do_headers.items():
        all_do_by_target.setdefault(target, []).append(dl)

    def label_referenced_elsewhere(lbl: str, skip_do_lines: set[int]) -> bool:
        tok_re = re.compile(rf"(?<![A-Za-z0-9_]){re.escape(lbl)}(?![A-Za-z0-9_])")
        for i0, pl in enumerate(parsed_now):
            line_no = i0 + 1
            if line_no in skip_do_lines:
                continue
            if tok_re.search(pl.stmt):
                return True
        return False

    # Remove now-unneeded numeric labels from terminating statements.
    for end_line, do_lines in close_map.items():
        pl_end = parsed_now[end_line - 1]
        if pl_end.label is None:
            continue
        lbl = pl_end.label
        # This line can only close loops with the same target label.
        if any(converted_target_by_do.get(dl) != lbl for dl in do_lines):
            continue
        # Keep label if any loop with this target was not converted.
        all_for_lbl = set(all_do_by_target.get(lbl, []))
        if any(dl not in cand_do_lines for dl in all_for_lbl):
            continue
        # Keep label if referenced elsewhere (conservative token search).
        if label_referenced_elsewhere(lbl, skip_do_lines=all_for_lbl):
            continue

        # Rebuild line without numeric label: place statement one level inside the
        # innermost loop closed by this terminator.
        inner_do_line = do_lines[0]
        pl_inner_do = parse_labeled_line(raw_lines[inner_do_line - 1])
        if pl_inner_do.label is not None:
            base_indent = f"{pl_inner_do.indent}{pl_inner_do.sep}"
        else:
            base_indent = re.match(r"^\s*", pl_inner_do.code).group(0) if pl_inner_do.code else pl_inner_do.indent
        new_code = f"{base_indent}{INDENT_STEP}{pl_end.stmt}".rstrip()
        raw_lines[end_line - 1] = f"{new_code}{pl_end.comment}{pl_end.eol}"
        changed += 1

    # Insert end do lines (innermost first as popped by stack order).
    for end_line in sorted(close_map.keys(), reverse=True):
        idx = end_line
        eol = parse_labeled_line(raw_lines[end_line - 1]).eol or "\n"
        end_lines: List[str] = []
        for do_line in close_map[end_line]:
            pl_do = parse_labeled_line(raw_lines[do_line - 1])
            # Match the DO statement indentation without inheriting numeric labels.
            if pl_do.label is not None:
                indent = f"{pl_do.indent}{pl_do.sep}"
            else:
                indent = re.match(r"^\s*", pl_do.code).group(0) if pl_do.code else pl_do.indent
            end_lines.append(f"{indent}end do{eol}")
        raw_lines[idx:idx] = end_lines
        changed += len(end_lines)

    backup: Optional[Path] = None
    target = out_path if out_path is not None else path
    if out_path is None and create_backup:
        backup = make_backup_path(path)
        shutil.copy2(path, backup)
    target.write_text("".join(raw_lines), encoding="utf-8")
    return changed, backup


def show_diff(old_text: str, new_text: str, path: Path) -> None:
    diff = difflib.unified_diff(
        old_text.splitlines(keepends=True),
        new_text.splitlines(keepends=True),
        fromfile=str(path),
        tofile=str(path),
    )
    txt = "".join(diff).rstrip()
    if txt:
        print(txt)


def main() -> int:
    ap = argparse.ArgumentParser(description="Suggest/fix replacing labeled DO loops with END DO termination.")
    ap.add_argument("files", nargs="*", type=Path)
    ap.add_argument("--exclude", action="append", default=[])
    ap.add_argument("--verbose", action="store_true")
    ap.add_argument("--fix", action="store_true")
    ap.add_argument("--out", type=Path)
    ap.add_argument("--backup", dest="backup", action="store_true", default=True)
    ap.add_argument("--no-backup", dest="backup", action="store_false")
    ap.add_argument("--diff", action="store_true")
    ap.add_argument("--compiler", type=str)
    ap.add_argument("--tee", action="store_true")
    ap.add_argument("--tee-both", action="store_true")
    ap.add_argument("--run", action="store_true")
    ap.add_argument("--run-both", action="store_true")
    ap.add_argument("--run-diff", action="store_true")
    ap.add_argument("--quiet-run", action="store_true")
    ap.add_argument("--keep-exe", action="store_true")
    ap.add_argument("--limit", type=int)
    ap.add_argument("--git", action="store_true")
    args = ap.parse_args()

    if args.run_diff:
        args.run_both = True
    if args.run_both:
        args.run = True
    if args.tee_both:
        args.tee = True
    if args.run and args.out is None:
        args.out = Path("temp.f90")
    if args.out is not None:
        args.fix = True
    if args.out is not None and len(args.files) != 1:
        print("--out supports exactly one input file.")
        return 2
    if args.tee and args.out is None:
        print("--tee requires --out.")
        return 2
    if args.run and len(args.files) != 1:
        print("--run/--run-both/--run-diff require exactly one input file.")
        return 2
    if args.diff and not args.fix:
        print("--diff requires --fix.")
        return 2
    if args.compiler and not args.fix:
        print("--compiler requires --fix.")
        return 2

    files = choose_files(list(args.files), args.exclude)
    if not files:
        print("No source files remain after applying --exclude filters.")
        return 2
    if args.limit is not None:
        if args.limit < 1:
            print("--limit must be >= 1.")
            return 2
        files = files[: args.limit]

    baseline_compile_paths = files
    after_compile_paths = [args.out] if (args.fix and args.out is not None) else files
    if args.fix and args.compiler:
        if not fbuild.run_compiler_command(args.compiler, baseline_compile_paths, "baseline", fscan.display_path):
            return 5

    all_candidates: List[LoopCandidate] = []
    per_file: Dict[Path, Tuple[List[LoopCandidate], Dict[int, Tuple[str, str]], Dict[int, List[int]]]] = {}
    for p in files:
        cands, headers, clos = analyze_file(p)
        per_file[p] = (cands, headers, clos)
        all_candidates.extend(cands)
        if args.verbose and cands:
            print(f"{fscan.display_path(p)}: {len(cands)} candidate(s)")

    orig_out = ""
    orig_err = ""
    xform_out = ""
    xform_err = ""
    transformed_changed = False
    transformed_target: Optional[Path] = None
    if not all_candidates:
        if args.run_both:
            src = files[0]
            ok_orig, orig_out, orig_err = fbuild.compile_and_run_source(
                src,
                label="original",
                quiet_run=args.quiet_run,
                keep_exe=args.keep_exe,
                exe_path=Path(f"{src.stem}_orig.exe"),
            )
            if not ok_orig:
                return 5
        if args.run_diff:
            print("Run diff: SKIP (no transformations suggested)")
        print("No labeled-DO replacement candidates found.")
        return 0

    if not args.fix:
        print(f"{len(all_candidates)} labeled-DO replacement candidate(s).")
        for c in all_candidates:
            print(f"{fscan.display_path(c.path)}:{c.do_line}->{c.end_line} label {c.target_label}")
        return 0

    changed_files: List[Path] = []
    backup_pairs: List[Tuple[Path, Path]] = []
    total_edits = 0

    for p in files:
        cands, headers, clos = per_file[p]
        if not cands:
            continue
        old_text = p.read_text(encoding="utf-8", errors="ignore")
        out_path = args.out if args.out is not None else None
        if out_path is not None and args.tee_both:
            print(f"--- original: {p} ---")
            print(old_text, end="")
            if not old_text.endswith("\n"):
                print("")
        changed, backup = apply_fix(p, cands, headers, clos, out_path=out_path, create_backup=args.backup)
        if changed > 0:
            transformed_changed = True
            total_edits += changed
            new_path = out_path if out_path is not None else p
            transformed_target = new_path
            new_text = new_path.read_text(encoding="utf-8", errors="ignore")
            if args.diff:
                show_diff(old_text, new_text, p)
            changed_files.append(new_path)
            if backup is not None:
                backup_pairs.append((p, backup))
            if out_path is not None and args.tee:
                print(f"--- transformed: {out_path} ---")
                print(new_text, end="")
                if not new_text.endswith("\n"):
                    print("")
            if not args.verbose:
                if out_path is None:
                    print(f"Fixed {fscan.display_path(p)}: edits {changed}, backup {backup.name if backup else '(none)'}")
                else:
                    print(f"Fixed {fscan.display_path(p)}: edits {changed}, wrote {out_path}")
        elif out_path is not None and args.tee:
            new_text = out_path.read_text(encoding="utf-8", errors="ignore")
            if args.tee_both:
                print(f"--- transformed: {out_path} ---")
            print(new_text, end="")
            if not new_text.endswith("\n"):
                print("")

    if args.compiler and changed_files:
        ok = fbuild.run_compiler_command(args.compiler, after_compile_paths, "after-fix", fscan.display_path)
        if not ok:
            if args.out is None:
                fbuild.rollback_backups(backup_pairs, fscan.display_path)
            return 5

    if args.git and changed_files:
        fbuild.git_commit_files(changed_files, "xloop: replace labeled DO with END DO", fscan.display_path)

    print(f"\n--fix summary: files changed {len(changed_files)}, edits {total_edits}")
    if args.run_both:
        src = files[0]
        ok_orig, orig_out, orig_err = fbuild.compile_and_run_source(
            src,
            label="original",
            quiet_run=args.quiet_run,
            keep_exe=args.keep_exe,
            exe_path=Path(f"{src.stem}_orig.exe"),
        )
        if not ok_orig:
            return 5
    if args.run and transformed_changed and transformed_target is not None:
        ok_xf, xform_out, xform_err = fbuild.compile_and_run_source(
            transformed_target,
            label="transformed",
            quiet_run=args.quiet_run,
            keep_exe=args.keep_exe,
            exe_path=Path(f"{transformed_target.stem}.exe"),
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
                ob = f"STDOUT:\n{orig_out}\nSTDERR:\n{orig_err}\n"
                tb = f"STDOUT:\n{xform_out}\nSTDERR:\n{xform_err}\n"
                for line in difflib.unified_diff(
                    ob.splitlines(), tb.splitlines(), fromfile="original", tofile="transformed", lineterm=""
                ):
                    print(line)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
