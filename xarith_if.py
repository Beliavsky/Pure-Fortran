#!/usr/bin/env python3
"""Suggest/fix replacing arithmetic IF and reducing simple GOTO patterns."""

from __future__ import annotations

import argparse
import difflib
import re
import shutil
import shlex
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Tuple

import cli_paths as cpaths
import fortran_build as fbuild
import fortran_scan as fscan


ARITH_IF_RE = re.compile(
    r"^\s*(?:(?P<label>\d+)(?P<sep>\s+))?if\s*\((?P<expr>.+)\)\s*"
    r"(?P<ln>\d+)\s*,\s*(?P<lz>\d+)\s*,\s*(?P<lp>\d+)\s*$",
    re.IGNORECASE,
)
INLINE_LOGICAL_ARITH_IF_RE = re.compile(
    r"^\s*(?:(?P<label>\d+)(?P<sep>\s+))?"
    r"if\s*\((?P<outer>.+?)\)\s*if\s*\((?P<expr>.+)\)\s*"
    r"(?P<ln>\d+)\s*,\s*(?P<lz>\d+)\s*,\s*(?P<lp>\d+)\s*$",
    re.IGNORECASE,
)
LABEL_LINE_RE = re.compile(r"^(?P<indent>\s*)(?P<label>\d+)(?P<sep>\s+)(?P<stmt>.*)$")
GOTO_RE = re.compile(r"^\s*goto\s+(?P<label>\d+)\s*$", re.IGNORECASE)
IF_GOTO_RE = re.compile(
    r"^\s*(?:(?P<label>\d+)(?P<sep>\s+))?if\s*\((?P<cond>.+)\)\s*goto\s+(?P<target>\d+)\s*$",
    re.IGNORECASE,
)


@dataclass
class Finding:
    path: Path
    line: int
    kind: str
    suggest: str


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


def load_codes(path: Path) -> List[List[Path]]:
    """Load source entries from a text list (one or more paths per line)."""
    out: List[List[Path]] = []
    for raw in path.read_text(encoding="utf-8", errors="ignore").splitlines():
        line = raw.strip()
        if not line or line.startswith("#"):
            continue
        toks = shlex.split(line, posix=False)
        if not toks:
            continue
        out.append([Path(tok) for tok in toks])
    return out


def resolve_code_entries(entries: List[List[Path]], code_dir: Optional[Path]) -> List[List[Path]]:
    """Resolve relative code entries against --code-dir when provided."""
    if code_dir is None:
        return entries
    out: List[List[Path]] = []
    for group in entries:
        resolved_group: List[Path] = []
        for p in group:
            resolved_group.append(p if p.is_absolute() else (code_dir / p))
        out.append(resolved_group)
    return out


def split_eol(raw: str) -> Tuple[str, str]:
    if raw.endswith("\r\n"):
        return raw[:-2], "\r\n"
    if raw.endswith("\n"):
        return raw[:-1], "\n"
    return raw, ""


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


def parse_labeled_code(code: str) -> Tuple[str, Optional[str], str, str]:
    m = LABEL_LINE_RE.match(code)
    if not m:
        indent = re.match(r"^\s*", code).group(0) if code else ""
        return indent, None, "", code
    return m.group("indent"), m.group("label"), m.group("sep"), m.group("stmt")


def _arith_lines(indent: str, ln: str, lz: str, lp: str, expr: str) -> List[str]:
    cond = expr.strip()
    if ln == lz == lp:
        return [f"{indent}goto {ln}"]
    if ln == lz and lz != lp:
        return [f"{indent}if ({cond} <= 0) then", f"{indent}   goto {ln}", f"{indent}else", f"{indent}   goto {lp}", f"{indent}end if"]
    if lz == lp and ln != lz:
        return [f"{indent}if ({cond} < 0) then", f"{indent}   goto {ln}", f"{indent}else", f"{indent}   goto {lz}", f"{indent}end if"]
    if ln == lp and ln != lz:
        return [f"{indent}if ({cond} == 0) then", f"{indent}   goto {lz}", f"{indent}else", f"{indent}   goto {ln}", f"{indent}end if"]
    return [
        f"{indent}if ({cond} < 0) then",
        f"{indent}   goto {ln}",
        f"{indent}else if ({cond} == 0) then",
        f"{indent}   goto {lz}",
        f"{indent}else",
        f"{indent}   goto {lp}",
        f"{indent}end if",
    ]


def rewrite_arithmetic_if(raw_lines: List[str], path: Path) -> Tuple[List[str], List[Finding], int]:
    out: List[str] = []
    findings: List[Finding] = []
    edits = 0
    for i, raw in enumerate(raw_lines, start=1):
        body, eol = split_eol(raw)
        code, comment = split_code_comment(body)
        stripped = code.strip()
        m_inl = INLINE_LOGICAL_ARITH_IF_RE.match(stripped)
        if m_inl:
            indent0, label0, sep0, _stmt0 = parse_labeled_code(code)
            outer = m_inl.group("outer").strip()
            expr = m_inl.group("expr").strip()
            ln = m_inl.group("ln")
            lz = m_inl.group("lz")
            lp = m_inl.group("lp")
            lead = f"{indent0}{sep0}" if label0 is not None else indent0
            repl_lines: List[str] = [f"{lead}if ({outer}) then"]
            repl_lines.extend(_arith_lines(f"{lead}   ", ln, lz, lp, expr))
            repl_lines.append(f"{lead}end if")
            if label0 is not None and repl_lines:
                repl_lines[0] = f"{indent0}{label0}{sep0}{repl_lines[0].lstrip()}"
            if comment:
                repl_lines[0] = f"{repl_lines[0]} {comment.strip()}"
            for line in repl_lines:
                out.append(f"{line}{eol or '\n'}")
            findings.append(Finding(path=path, line=i, kind="arithmetic_if_inline", suggest="; ".join(repl_lines)))
            edits += 1
            continue

        m = ARITH_IF_RE.match(stripped)
        if not m:
            out.append(raw)
            continue
        indent0, label0, sep0, _stmt0 = parse_labeled_code(code)
        expr = m.group("expr").strip()
        ln = m.group("ln")
        lz = m.group("lz")
        lp = m.group("lp")
        inner_indent = f"{indent0}{sep0}" if label0 is not None else indent0
        repl_lines = _arith_lines(inner_indent, ln, lz, lp, expr)
        if label0 is not None and repl_lines:
            repl_lines[0] = f"{indent0}{label0}{sep0}{repl_lines[0].lstrip()}"
        if comment:
            repl_lines[0] = f"{repl_lines[0]} {comment.strip()}"
        for line in repl_lines:
            out.append(f"{line}{eol or '\n'}")
        findings.append(Finding(path=path, line=i, kind="arithmetic_if", suggest="; ".join(repl_lines)))
        edits += 1
    return out, findings, edits


def find_arithmetic_if_lines(raw_lines: List[str], *, stop_first: bool) -> List[int]:
    hits: List[int] = []
    for i, raw in enumerate(raw_lines, start=1):
        body, _eol = split_eol(raw)
        code, _comment = split_code_comment(body)
        if ARITH_IF_RE.match(code.strip()):
            hits.append(i)
            if stop_first:
                break
    return hits


def _build_label_map(lines: List[str]) -> Dict[str, int]:
    mp: Dict[str, int] = {}
    for i, raw in enumerate(lines, start=1):
        body, _eol = split_eol(raw)
        code, _comment = split_code_comment(body)
        _indent, lbl, _sep, _stmt = parse_labeled_code(code)
        if lbl is not None and lbl not in mp:
            mp[lbl] = i
    return mp


def minimize_gotos(lines: List[str], path: Path) -> Tuple[List[str], List[Finding], int]:
    out = list(lines)
    findings: List[Finding] = []
    edits = 0

    # Pass 1: bypass goto chains.
    label_map = _build_label_map(out)
    for i, raw in enumerate(list(out), start=1):
        body, eol = split_eol(raw)
        code, comment = split_code_comment(body)
        indent, lbl, sep, stmt = parse_labeled_code(code)
        mg = GOTO_RE.match(stmt.strip())
        if not mg:
            continue
        target = mg.group("label")
        tline = label_map.get(target)
        if tline is None:
            continue
        t_body, _t_eol = split_eol(out[tline - 1])
        t_code, _t_comment = split_code_comment(t_body)
        _ti, _tl, _ts, t_stmt = parse_labeled_code(t_code)
        mg2 = GOTO_RE.match(t_stmt.strip())
        if not mg2:
            continue
        new_t = mg2.group("label")
        if new_t == target:
            continue
        new_stmt = f"goto {new_t}"
        new_code = f"{indent}{lbl}{sep}{new_stmt}" if lbl is not None else f"{indent}{new_stmt}"
        out[i - 1] = f"{new_code}{comment}{eol}"
        findings.append(Finding(path=path, line=i, kind="goto_chain", suggest=new_stmt))
        edits += 1

    # Pass 2: remove goto to next labeled line.
    label_map = _build_label_map(out)
    i = 1
    while i <= len(out):
        body, eol = split_eol(out[i - 1])
        code, comment = split_code_comment(body)
        indent, lbl, sep, stmt = parse_labeled_code(code)
        mg = GOTO_RE.match(stmt.strip())
        if not mg:
            i += 1
            continue
        target = mg.group("label")
        # Find next non-blank physical line with code.
        j = i + 1
        next_lbl = None
        while j <= len(out):
            b2, _e2 = split_eol(out[j - 1])
            c2, _cm2 = split_code_comment(b2)
            if c2.strip():
                _ii, lb2, _s2, _st2 = parse_labeled_code(c2)
                next_lbl = lb2
                break
            j += 1
        if next_lbl == target:
            out.pop(i - 1)
            findings.append(Finding(path=path, line=i, kind="goto_next", suggest="remove goto"))
            edits += 1
            continue
        i += 1

    # Pass 3: if(cond) goto L ; goto M ; L:
    i = 1
    while i + 2 <= len(out):
        b1, e1 = split_eol(out[i - 1])
        c1, cm1 = split_code_comment(b1)
        i1, l1, s1, st1 = parse_labeled_code(c1)
        m_if = IF_GOTO_RE.match(st1.strip())
        if not m_if:
            i += 1
            continue
        cond = m_if.group("cond").strip()
        lt = m_if.group("target")

        b2, _e2 = split_eol(out[i])
        c2, _cm2 = split_code_comment(b2)
        i2, l2, s2, st2 = parse_labeled_code(c2)
        m_g2 = GOTO_RE.match(st2.strip())
        if not m_g2:
            i += 1
            continue
        mt = m_g2.group("label")

        b3, _e3 = split_eol(out[i + 1])
        c3, _cm3 = split_code_comment(b3)
        _i3, l3, _s3, _st3 = parse_labeled_code(c3)
        if l3 != lt:
            i += 1
            continue

        new_stmt = f"if (.not.({cond})) goto {mt}"
        new_code = f"{i1}{l1}{s1}{new_stmt}" if l1 is not None else f"{i1}{new_stmt}"
        out[i - 1] = f"{new_code}{cm1}{e1}"
        out.pop(i)  # remove the unconditional goto line
        findings.append(Finding(path=path, line=i, kind="if_goto_goto", suggest=new_stmt))
        edits += 1
        i += 1

    return out, findings, edits


def apply_fix(
    path: Path,
    new_text: str,
    *,
    out_path: Optional[Path] = None,
    create_backup: bool = True,
) -> Tuple[int, Optional[Path]]:
    old_text = path.read_text(encoding="utf-8", errors="ignore")
    if old_text == new_text:
        if out_path is not None:
            out_path.write_text(old_text, encoding="utf-8")
        return 0, None
    backup: Optional[Path] = None
    target = out_path if out_path is not None else path
    if out_path is None and create_backup:
        backup = make_backup_path(path)
        shutil.copy2(path, backup)
    target.write_text(new_text, encoding="utf-8")
    return 1, backup


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
    ap = argparse.ArgumentParser(description="Suggest/fix replacing arithmetic IF and reducing simple GOTO patterns.")
    ap.add_argument(
        "--codes",
        type=Path,
        help="Path to source list. Each line may contain one or more file paths (first path used).",
    )
    ap.add_argument(
        "--code-dir",
        type=Path,
        help="Base directory for relative paths in --codes entries.",
    )
    ap.add_argument("files", nargs="*", type=Path)
    ap.add_argument("--exclude", action="append", default=[])
    ap.add_argument("--verbose", action="store_true")
    ap.add_argument("--find", action="store_true", help="List files containing arithmetic IF statements.")
    ap.add_argument("--find-lines", action="store_true", help="With --find, append matching line number(s). Implies --find.")
    ap.add_argument(
        "--find-all-lines",
        action="store_true",
        help="With --find, show all matching line numbers per file as file:l1,l2,... (implies --find).",
    )
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

    if args.find_lines or args.find_all_lines:
        args.find = True
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

    if args.files:
        files = choose_files(args.files, args.exclude)
    elif args.codes is not None:
        entries = resolve_code_entries(load_codes(args.codes), args.code_dir)
        firsts: List[Path] = []
        seen = set()
        for group in entries:
            if not group:
                continue
            p = group[0]
            k = str(p).lower()
            if k in seen:
                continue
            seen.add(k)
            firsts.append(p)
        files = fscan.apply_excludes(firsts, args.exclude)
    else:
        files = choose_files([], args.exclude)

    if args.out is not None and len(files) != 1:
        print("--out supports exactly one input file.")
        return 2
    if args.tee and args.out is None:
        print("--tee requires --out.")
        return 2
    if args.run and len(files) != 1:
        print("--run/--run-both/--run-diff require exactly one input file.")
        return 2
    if not files:
        print("No Fortran files found.")
        return 0

    if args.find:
        stop_first = not args.verbose and not args.find_all_lines
        found = 0
        for p in files:
            text = p.read_text(encoding="utf-8", errors="ignore")
            lines = text.splitlines(keepends=True)
            hit_lines = find_arithmetic_if_lines(lines, stop_first=stop_first)
            if not hit_lines:
                continue
            found += 1
            if args.find_all_lines:
                line_txt = ",".join(str(ln) for ln in hit_lines)
                print(f"{fscan.display_path(p)}:{line_txt}")
            elif args.find_lines:
                if stop_first:
                    print(f"{fscan.display_path(p)}:{hit_lines[0]}")
                else:
                    for ln in hit_lines:
                        print(f"{fscan.display_path(p)}:{ln}")
            else:
                print(fscan.display_path(p))
        if args.verbose:
            print(f"\n--find summary: files with arithmetic IF {found}")
        return 0

    if args.compiler:
        if not fbuild.run_compiler_command(args.compiler, files, "baseline", fscan.display_path):
            return 1

    all_findings: List[Finding] = []
    per_file_newtext: Dict[Path, str] = {}
    per_file_edit_count: Dict[Path, int] = {}
    changed_files = 0
    orig_out = ""
    orig_err = ""
    xform_out = ""
    xform_err = ""
    transformed_changed = False
    transformed_target: Optional[Path] = None

    for p in files:
        old_text = p.read_text(encoding="utf-8", errors="ignore")
        lines = old_text.splitlines(keepends=True)
        after_arith, f_arith, e_arith = rewrite_arithmetic_if(lines, p)
        after_min, f_min, e_min = minimize_gotos(after_arith, p)
        new_text = "".join(after_min)
        per_file_newtext[p] = new_text
        per_file_edit_count[p] = e_arith + e_min
        all_findings.extend(f_arith)
        all_findings.extend(f_min)
        if new_text != old_text:
            changed_files += 1

    if args.limit and args.limit > 0 and len(all_findings) > args.limit:
        all_findings = all_findings[: args.limit]

    if not all_findings:
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
                return 1
        if args.run_diff:
            print("Run diff: SKIP (no transformations suggested)")
        print("No arithmetic-IF/GOTO cleanup candidates found.")
        if args.fix and args.out is not None:
            p = files[0]
            args.out.write_text(per_file_newtext[p], encoding="utf-8")
            print(f"Wrote unchanged output to {fscan.display_path(args.out)}")
        return 0

    if args.verbose:
        print(f"{len(all_findings)} replacement candidate(s).")
        for f in all_findings:
            print(f"{fscan.display_path(f.path)}:{f.line} {f.kind}")
            print(f"  suggest: {f.suggest}")
    else:
        counts: Dict[Path, int] = {}
        for f in all_findings:
            counts[f.path] = counts.get(f.path, 0) + 1
        for p, n in counts.items():
            print(f"{fscan.display_path(p)}: {n} candidate(s)")

    if not args.fix:
        return 0

    backup_pairs: List[Tuple[Path, Path]] = []
    changed_ct = 0
    total_edits = 0
    for p in files:
        old_text = p.read_text(encoding="utf-8", errors="ignore")
        new_text = per_file_newtext[p]
        if args.diff and old_text != new_text:
            show_diff(old_text, new_text, p)
        out_path = args.out if (args.out is not None and p == files[0]) else None
        if out_path is not None and args.tee_both:
            print(f"--- original: {p} ---")
            print(old_text, end="")
            if not old_text.endswith("\n"):
                print("")
        changed, backup = apply_fix(p, new_text, out_path=out_path, create_backup=args.backup)
        if changed:
            transformed_changed = True
            changed_ct += 1
            total_edits += per_file_edit_count.get(p, 0)
            transformed_target = out_path if out_path is not None else p
            if backup is not None:
                backup_pairs.append((p, backup))
            if out_path is not None and args.tee:
                print(f"--- transformed: {out_path} ---")
                print(new_text, end="")
                if not new_text.endswith("\n"):
                    print("")
            if out_path is not None:
                print(
                    f"Fixed {fscan.display_path(p)}: edits {per_file_edit_count.get(p, 0)}, wrote {fscan.display_path(out_path)}"
                )
            else:
                msg = f"Fixed {fscan.display_path(p)}: edits {per_file_edit_count.get(p, 0)}"
                if backup is not None:
                    msg += f", backup {fscan.display_path(backup)}"
                print(msg)
        elif out_path is not None and args.tee:
            if args.tee_both:
                print(f"--- transformed: {out_path} ---")
            print(new_text, end="")
            if not new_text.endswith("\n"):
                print("")

    if args.compiler:
        comp_files = [args.out] if args.out is not None else files
        ok = fbuild.run_compiler_command(args.compiler, comp_files, "after-fix", fscan.display_path)
        if not ok:
            if args.out is None:
                fbuild.rollback_backups(backup_pairs, fscan.display_path)
            return 1

    print(f"\n--fix summary: files changed {changed_ct}, edits {total_edits}")
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
            return 1
    if args.run and transformed_changed and transformed_target is not None:
        ok_xf, xform_out, xform_err = fbuild.compile_and_run_source(
            transformed_target,
            label="transformed",
            quiet_run=args.quiet_run,
            keep_exe=args.keep_exe,
            exe_path=Path(f"{transformed_target.stem}.exe"),
        )
        if not ok_xf:
            return 1
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

    if args.git and args.out is None and changed_ct > 0:
        changed_paths = [p for p in files if per_file_newtext[p] != p.read_text(encoding='utf-8', errors='ignore')]
        if not changed_paths:
            # fallback: include all requested files; git helper will ignore unstaged/no-op
            changed_paths = files
        _ = fbuild.git_commit_files(
            changed_paths,
            "xarith_if: replace arithmetic IF and reduce simple GOTO",
            fscan.display_path,
        )

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
