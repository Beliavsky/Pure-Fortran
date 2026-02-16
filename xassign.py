#!/usr/bin/env python3
"""Suggest/fix replacing ASSIGN and assigned GO TO usage."""

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

ASSIGN_RE = re.compile(r"^\s*assign\s+(?P<label>\d+)\s+to\s+(?P<var>[a-z][a-z0-9_]*)\s*$", re.IGNORECASE)
ASSIGNED_GOTO_RE = re.compile(
    r"^\s*go\s*to\s+(?P<var>[a-z][a-z0-9_]*)\s*,\s*\((?P<labels>[^)]*)\)\s*$",
    re.IGNORECASE,
)
FORMAT_STMT_RE = re.compile(r"^\s*(?P<label>\d+)\s*format\s*(?P<fmt>\(.+\))\s*$", re.IGNORECASE)
FORMAT_ONLY_RE = re.compile(r"^\s*format\s*(?P<fmt>\(.+\))\s*$", re.IGNORECASE)
LABEL_LINE_RE = re.compile(r"^(?P<indent>\s*)(?P<label>\d+)(?P<sep>\s+)(?P<stmt>.*)$")


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


def find_matching_paren(text: str, start_idx: int) -> int:
    depth = 0
    in_single = False
    in_double = False
    i = start_idx
    while i < len(text):
        ch = text[i]
        if ch == "'" and not in_double:
            if in_single and i + 1 < len(text) and text[i + 1] == "'":
                i += 2
                continue
            in_single = not in_single
            i += 1
            continue
        if ch == '"' and not in_single:
            if in_double and i + 1 < len(text) and text[i + 1] == '"':
                i += 2
                continue
            in_double = not in_double
            i += 1
            continue
        if not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
                if depth == 0:
                    return i
        i += 1
    return -1


def parse_read_write_stmt(stmt: str) -> Optional[Tuple[str, str, str]]:
    s = stmt.strip()
    low = s.lower()
    if low.startswith("write"):
        kw = "write"
        klen = 5
    elif low.startswith("read"):
        kw = "read"
        klen = 4
    else:
        return None
    i = klen
    while i < len(s) and s[i].isspace():
        i += 1
    if i >= len(s) or s[i] != "(":
        return None
    j = find_matching_paren(s, i)
    if j < 0:
        return None
    control = s[i + 1 : j]
    tail = s[j + 1 :].strip()
    return kw, control, tail


def split_top_level_commas(text: str) -> List[str]:
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


def split_top_level_equals(text: str) -> Optional[Tuple[str, str]]:
    depth = 0
    in_single = False
    in_double = False
    i = 0
    while i < len(text):
        ch = text[i]
        if ch == "'" and not in_double:
            if in_single and i + 1 < len(text) and text[i + 1] == "'":
                i += 2
                continue
            in_single = not in_single
            i += 1
            continue
        if ch == '"' and not in_single:
            if in_double and i + 1 < len(text) and text[i + 1] == '"':
                i += 2
                continue
            in_double = not in_double
            i += 1
            continue
        if not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")" and depth > 0:
                depth -= 1
            elif ch == "=" and depth == 0:
                return text[:i].strip(), text[i + 1 :].strip()
        i += 1
    return None


def fmt_to_char_literal(fmt: str) -> str:
    return '"' + fmt.replace('"', '""') + '"'


def rewrite_file(raw_lines: List[str], path: Path, *, fix_all: bool) -> Tuple[List[str], List[Finding], int]:
    findings: List[Finding] = []
    edits = 0
    out: List[str] = []

    # Collect FORMAT labels and labels assigned via ASSIGN.
    format_by_label: Dict[str, str] = {}
    fmt_assign_labels_by_var: Dict[str, List[str]] = {}
    vars_used_by_assigned_goto: set[str] = set()
    for raw in raw_lines:
        body, _eol = split_eol(raw)
        code, _comment = split_code_comment(body)
        _ind, lbl, _sep, stmt = parse_labeled_code(code)
        mfmt0 = FORMAT_STMT_RE.match(code.strip())
        if mfmt0:
            format_by_label[mfmt0.group("label")] = mfmt0.group("fmt").strip()
            continue
        mfmt = FORMAT_ONLY_RE.match(stmt.strip())
        if mfmt and lbl is not None:
            format_by_label[lbl] = mfmt.group("fmt").strip()
    for raw in raw_lines:
        body, _eol = split_eol(raw)
        code, _comment = split_code_comment(body)
        _ind, _lbl, _sep, stmt = parse_labeled_code(code)
        mg = ASSIGNED_GOTO_RE.match(stmt.strip())
        if mg:
            vars_used_by_assigned_goto.add(mg.group("var").lower())
        ma = ASSIGN_RE.match(stmt.strip())
        if not ma:
            continue
        lbl = ma.group("label")
        var = ma.group("var").lower()
        if lbl in format_by_label:
            fmt_assign_labels_by_var.setdefault(var, []).append(lbl)

    for lineno, raw in enumerate(raw_lines, start=1):
        body, eol = split_eol(raw)
        code, comment = split_code_comment(body)
        indent, label, sep, stmt = parse_labeled_code(code)
        s = stmt.strip()

        # ASSIGN n TO var  -> var = n
        ma = ASSIGN_RE.match(s)
        if ma:
            v = ma.group("var")
            v_l = v.lower()
            n = ma.group("label")
            # Safe mode: only rewrite ASSIGN for variables used by assigned GO TO.
            # Keep assigned-format ASSIGN intact unless --fix-all is enabled.
            if (v_l not in vars_used_by_assigned_goto) and (not fix_all):
                out.append(raw)
                continue
            new_stmt = f"{v} = {n}"
            new_code = f"{indent}{label}{sep}{new_stmt}" if label is not None else f"{indent}{new_stmt}"
            out.append(f"{new_code}{comment}{eol}")
            findings.append(Finding(path=path, line=lineno, kind="assign_stmt", suggest=new_stmt))
            edits += 1
            continue

        # GO TO var,(l1,l2,...) -> SELECT CASE(var); CASE(li); GOTO li; ...; END SELECT
        mg = ASSIGNED_GOTO_RE.match(s)
        if mg:
            v = mg.group("var")
            labels = [x.strip() for x in mg.group("labels").split(",") if x.strip()]
            if labels:
                lines: List[str] = [f"{indent}select case ({v})"]
                for lb in labels:
                    lines.append(f"{indent}case ({lb})")
                    lines.append(f"{indent}   goto {lb}")
                lines.append(f"{indent}end select")
                if label is not None:
                    lines[0] = f"{indent}{label}{sep}{lines[0].lstrip()}"
                if comment:
                    lines[0] = f"{lines[0]} {comment.strip()}"
                for ln in lines:
                    out.append(f"{ln}{eol or '\n'}")
                findings.append(Finding(path=path, line=lineno, kind="assigned_goto", suggest="; ".join(lines)))
                edits += 1
                continue

        # --fix-all: assigned FORMAT through select case(var) around READ/WRITE.
        if fix_all:
            rw = parse_read_write_stmt(s)
            if rw is not None:
                kw, control, tail = rw
                parts = split_top_level_commas(control)
                if parts:
                    fmt_idx: Optional[int] = None
                    fmt_is_keyword = False
                    fmt_var: Optional[str] = None
                    for i, p in enumerate(parts):
                        kv = split_top_level_equals(p)
                        if kv is None:
                            continue
                        k, v = kv
                        if k.strip().lower() == "fmt":
                            vv = v.strip()
                            if re.fullmatch(r"[a-z][a-z0-9_]*", vv, re.IGNORECASE):
                                fmt_idx = i
                                fmt_is_keyword = True
                                fmt_var = vv.lower()
                            break
                    if fmt_idx is None and len(parts) >= 2:
                        cand = parts[1].strip()
                        if re.fullmatch(r"[a-z][a-z0-9_]*", cand, re.IGNORECASE):
                            fmt_idx = 1
                            fmt_is_keyword = False
                            fmt_var = cand.lower()
                    if fmt_idx is not None and fmt_var is not None and fmt_var in fmt_assign_labels_by_var:
                        labels = []
                        seen = set()
                        for lb in fmt_assign_labels_by_var[fmt_var]:
                            if lb in format_by_label and lb not in seen:
                                seen.add(lb)
                                labels.append(lb)
                        if labels:
                            block: List[str] = [f"{indent}select case ({fmt_var})"]
                            for lb in labels:
                                fmt_lit = fmt_to_char_literal(format_by_label[lb])
                                p2 = list(parts)
                                if fmt_is_keyword:
                                    k, _v = split_top_level_equals(p2[fmt_idx])  # type: ignore[misc]
                                    p2[fmt_idx] = f"{k}={fmt_lit}"
                                else:
                                    p2[fmt_idx] = fmt_lit
                                stmt2 = f"{kw}({', '.join(p2)})"
                                if tail:
                                    stmt2 = f"{stmt2} {tail}"
                                block.append(f"{indent}case ({lb})")
                                block.append(f"{indent}   {stmt2}")
                            block.append(f"{indent}case default")
                            block.append(f"{indent}   continue")
                            block.append(f"{indent}end select")
                            if label is not None:
                                block[0] = f"{indent}{label}{sep}{block[0].lstrip()}"
                            if comment:
                                block[0] = f"{block[0]} {comment.strip()}"
                            for ln in block:
                                out.append(f"{ln}{eol or '\n'}")
                            findings.append(Finding(path=path, line=lineno, kind="assigned_format", suggest=f"select case({fmt_var})"))
                            edits += 1
                            continue

        out.append(raw)

    return out, findings, edits


def apply_fix(path: Path, new_text: str, *, out_path: Optional[Path] = None, create_backup: bool = True) -> Tuple[int, Optional[Path]]:
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


def out_path_for_source(out_dir: Path, src: Path) -> Path:
    """Return output path inside out-dir for a source file."""
    return out_dir / src.name


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
    ap = argparse.ArgumentParser(description="Suggest/fix replacing ASSIGN and assigned GO TO/FORMAT usage.")
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
    ap.add_argument("--find", action="store_true", help="List files containing ASSIGN usage.")
    ap.add_argument("--fix", action="store_true")
    ap.add_argument("--fix-all", action="store_true", help="Enable additional assigned-FORMAT rewrites.")
    ap.add_argument("--out", type=Path)
    ap.add_argument("--out-dir", type=Path, help="Output directory for transformed files (multi-file safe).")
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
    if args.out_dir is not None:
        args.fix = True
    if args.fix_all:
        args.fix = True
    if args.out is not None and args.out_dir is not None:
        print("Use either --out or --out-dir, not both.")
        return 2

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
    if args.run and args.out_dir is not None:
        print("--run/--run-both/--run-diff do not support --out-dir.")
        return 2
    if args.out_dir is not None:
        if args.out_dir.exists() and not args.out_dir.is_dir():
            print("--out-dir exists but is not a directory.")
            return 2
        args.out_dir.mkdir(parents=True, exist_ok=True)
    if not files:
        print("No Fortran files found.")
        return 0

    if args.find:
        count = 0
        for p in files:
            text = p.read_text(encoding="utf-8", errors="ignore")
            hit = False
            for raw in text.splitlines():
                code, _comment = split_code_comment(raw)
                _i, _l, _s, stmt = parse_labeled_code(code)
                if ASSIGN_RE.match(stmt.strip()):
                    hit = True
                    break
            if hit:
                print(fscan.display_path(p))
                count += 1
        if args.verbose:
            print(f"\n--find summary: files with ASSIGN {count}")
        return 0

    if args.compiler:
        if not fbuild.run_compiler_command(args.compiler, files, "baseline", fscan.display_path):
            return 1

    all_findings: List[Finding] = []
    per_file_newtext: Dict[Path, str] = {}
    per_file_edits: Dict[Path, int] = {}
    for p in files:
        old_text = p.read_text(encoding="utf-8", errors="ignore")
        lines = old_text.splitlines(keepends=True)
        new_lines, findings, edits = rewrite_file(lines, p, fix_all=args.fix_all)
        per_file_newtext[p] = "".join(new_lines)
        per_file_edits[p] = edits
        all_findings.extend(findings)
    orig_out = ""
    orig_err = ""
    xform_out = ""
    xform_err = ""
    transformed_changed = False
    transformed_target: Optional[Path] = None

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
        print("No ASSIGN replacement candidates found.")
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

    changed_files = 0
    total_edits = 0
    out_files: List[Path] = []
    for p in files:
        old_text = p.read_text(encoding="utf-8", errors="ignore")
        new_text = per_file_newtext[p]
        if args.diff and old_text != new_text:
            show_diff(old_text, new_text, p)
        out_path: Optional[Path] = None
        if args.out is not None and p == files[0]:
            out_path = args.out
        elif args.out_dir is not None:
            out_path = out_path_for_source(args.out_dir, p)
        if out_path is not None and args.tee_both:
            print(f"--- original: {p} ---")
            print(old_text, end="")
            if not old_text.endswith("\n"):
                print("")
        changed, backup = apply_fix(p, new_text, out_path=out_path, create_backup=args.backup)
        if out_path is not None:
            out_files.append(out_path)
        if changed:
            transformed_changed = True
            transformed_target = out_path if out_path is not None else p
            changed_files += 1
            total_edits += per_file_edits.get(p, 0)
            if out_path is not None and args.tee:
                print(f"--- transformed: {out_path} ---")
                print(new_text, end="")
                if not new_text.endswith("\n"):
                    print("")
            if out_path is not None:
                print(f"Fixed {fscan.display_path(p)}: edits {per_file_edits.get(p, 0)}, wrote {fscan.display_path(out_path)}")
            else:
                msg = f"Fixed {fscan.display_path(p)}: edits {per_file_edits.get(p, 0)}"
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
        if args.out is not None:
            comp_files = [args.out]
        elif args.out_dir is not None:
            comp_files = out_files if out_files else [out_path_for_source(args.out_dir, p) for p in files]
        else:
            comp_files = files
        ok = fbuild.run_compiler_command(args.compiler, comp_files, "after-fix", fscan.display_path)
        if not ok:
            return 1

    print(f"\n--fix summary: files changed {changed_files}, edits {total_edits}")
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
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
