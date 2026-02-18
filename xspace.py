#!/usr/bin/env python3
"""Suggest/fix simple Fortran spacing/layout normalization rules."""

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

CHANGE_TAG = "!! changed by xspace.py"


@dataclass
class Finding:
    """One rule finding at a source location."""

    path: Path
    line: int
    rule: int
    message: str
    suggestion: Optional[str] = None


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
    """Create a non-overwriting backup path next to a source file."""
    base = Path(str(path) + ".bak")
    if not base.exists():
        return base
    idx = 1
    while True:
        cand = Path(f"{path}.bak{idx}")
        if not cand.exists():
            return cand
        idx += 1


def split_code_comment(line: str) -> Tuple[str, str]:
    """Split one source line into code and trailing comment text."""
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


def split_body_eol(raw: str) -> Tuple[str, str]:
    """Split one raw line into body and end-of-line text."""
    body = raw.rstrip("\r\n")
    return body, raw[len(body) :]


def eol_from_neighbors(lines: List[str], idx: int) -> str:
    """Pick an EOL style for inserted lines based on nearby lines."""
    if 0 <= idx < len(lines):
        _b, eol = split_body_eol(lines[idx])
        if eol:
            return eol
    if 0 <= idx - 1 < len(lines):
        _b, eol = split_body_eol(lines[idx - 1])
        if eol:
            return eol
    return "\n"


def is_blank_line(raw: str) -> bool:
    """Return True when line is whitespace-only."""
    return raw.strip() == ""


def is_return_only(raw: str) -> bool:
    """Return True when code (ignoring comments/spaces/case) is only RETURN."""
    body, _eol = split_body_eol(raw)
    code, _comment = split_code_comment(body)
    return code.strip().lower() == "return"


def append_change_tag(line: str) -> str:
    """Append xspace change tag unless already present."""
    body, eol = split_body_eol(line)
    if CHANGE_TAG.lower() in body.lower():
        return line
    return f"{body}  {CHANGE_TAG}{eol}"


def squeeze_paren_spaces(code: str) -> str:
    """Remove spaces immediately after '(' and before ')' outside quoted strings."""
    out: List[str] = []
    in_single = False
    in_double = False
    i = 0
    while i < len(code):
        ch = code[i]
        if ch == "'" and not in_double:
            in_single = not in_single
            out.append(ch)
            i += 1
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            out.append(ch)
            i += 1
            continue
        if not in_single and not in_double:
            if ch == "(":
                out.append(ch)
                i += 1
                while i < len(code) and code[i] == " ":
                    i += 1
                continue
            if ch == ")":
                while out and out[-1] == " ":
                    out.pop()
                out.append(ch)
                i += 1
                continue
        out.append(ch)
        i += 1
    return "".join(out)


def tighten_proc_name_paren(code: str) -> str:
    """Remove spaces between procedure name and '(' on SUBROUTINE/FUNCTION starts."""
    pattern = re.compile(
        r"^(\s*(?:(?:pure|elemental|impure|recursive|module)\s+)*"
        r"(?:subroutine|function)\s+"
        r"[a-z][a-z0-9_]*)\s+(\()",
        re.IGNORECASE,
    )
    return pattern.sub(r"\1\2", code, count=1)


def parse_disabled_rules(values: List[str]) -> Tuple[Optional[Set[int]], Optional[str]]:
    """Parse rule numbers from repeated --disable options."""
    disabled: Set[int] = set()
    for raw in values:
        for tok in raw.split(","):
            t = tok.strip()
            if not t:
                continue
            if not t.isdigit():
                return None, f"Invalid --disable value '{t}'. Use rule numbers 1..6."
            n = int(t)
            if n < 1 or n > 6:
                return None, f"Invalid rule number '{n}' in --disable. Valid values: 1,2,3,4,5,6."
            disabled.add(n)
    return disabled, None


def statement_end_index(lines: List[str], start_idx: int) -> int:
    """Return the final line index of a continued free-form statement."""
    i = start_idx
    while i < len(lines):
        code = fscan.strip_comment(lines[i].rstrip("\r\n")).rstrip()
        cont = code.endswith("&")
        if not cont:
            if i + 1 < len(lines):
                nxt = fscan.strip_comment(lines[i + 1].rstrip("\r\n")).lstrip()
                if nxt.startswith("&"):
                    i += 1
                    continue
            break
        i += 1
    return i


def is_blank_bang_line(raw: str) -> bool:
    """Return True when a line is empty except optional spaces and one bare ! comment marker."""
    body, _eol = split_body_eol(raw)
    return re.match(r"^\s*!\s*$", body) is not None


def transform_lines(
    path: Path,
    lines: List[str],
    enabled_rules: Set[int],
    *,
    annotate_fix: bool = False,
) -> Tuple[List[str], List[Finding]]:
    """Apply enabled spacing rules and return transformed lines plus findings."""
    out = lines[:]
    findings: List[Finding] = []

    # Rule 3 and Rule 4: line-local transformations.
    for i in range(len(out)):
        raw = out[i]
        body, eol = split_body_eol(raw)
        code, comment = split_code_comment(body)
        new_code = code
        changed = False

        if 3 in enabled_rules:
            c3 = squeeze_paren_spaces(new_code)
            if c3 != new_code:
                new_code = c3
                changed = True
                findings.append(
                    Finding(
                        path=path,
                        line=i + 1,
                        rule=3,
                        message="remove spaces just inside parentheses",
                        suggestion=(c3 + comment).rstrip(),
                    )
                )

        if 4 in enabled_rules:
            c4 = tighten_proc_name_paren(new_code)
            if c4 != new_code:
                new_code = c4
                changed = True
                findings.append(
                    Finding(
                        path=path,
                        line=i + 1,
                        rule=4,
                        message="remove spaces between procedure name and '('",
                        suggestion=(c4 + comment).rstrip(),
                    )
                )

        if changed:
            updated = f"{new_code}{comment}{eol}"
            if annotate_fix:
                updated = append_change_tag(updated)
            out[i] = updated

    # Rule 1: remove blank line immediately before a bare RETURN line.
    if 6 in enabled_rules:
        i = 0
        while i < len(out):
            if is_blank_bang_line(out[i]):
                findings.append(
                    Finding(
                        path=path,
                        line=i + 1,
                        rule=6,
                        message="remove line that is blank except for '!'",
                        suggestion=None,
                    )
                )
                del out[i]
                continue
            i += 1

    # Rule 1: remove blank line immediately before a bare RETURN line.
    if 1 in enabled_rules:
        i = 0
        while i < len(out) - 1:
            if is_blank_line(out[i]) and is_return_only(out[i + 1]):
                findings.append(
                    Finding(
                        path=path,
                        line=i + 2,
                        rule=1,
                        message="remove blank line directly above RETURN",
                        suggestion=None,
                    )
                )
                del out[i]
                continue
            i += 1

    # Rule 5: remove blank line(s) immediately after subroutine/function headers.
    if 5 in enabled_rules:
        parsed_lines = [ln.rstrip("\r\n") for ln in out]
        procs = sorted(fscan.parse_procedures(parsed_lines), key=lambda p: p.start)
        delete_idxs: List[int] = []
        for p in procs:
            if p.kind.lower() not in {"subroutine", "function"}:
                continue
            header_end = statement_end_index(out, max(0, p.start - 1))
            j = header_end + 1
            while j < len(out) and is_blank_line(out[j]):
                findings.append(
                    Finding(
                        path=path,
                        line=j + 1,
                        rule=5,
                        message="remove blank line immediately after subroutine/function header",
                        suggestion=None,
                    )
                )
                delete_idxs.append(j)
                j += 1
        for idx in sorted(set(delete_idxs), reverse=True):
            if 0 <= idx < len(out):
                del out[idx]

    # Rule 2: ensure at least one blank line between consecutive procedures.
    if 2 in enabled_rules:
        parsed_lines = [ln.rstrip("\r\n") for ln in out]
        procs = sorted(fscan.parse_procedures(parsed_lines), key=lambda p: p.start)
        inserts: List[int] = []
        for i in range(len(procs) - 1):
            p1 = procs[i]
            p2 = procs[i + 1]
            if p1.end <= 0 or p2.start <= 1:
                continue
            lo = max(0, p1.end)
            hi = max(0, p2.start - 1)
            between = out[lo:hi]
            has_blank_between = any(is_blank_line(ln) for ln in between)
            if has_blank_between:
                continue
            insert_idx = p2.start - 1
            if 0 <= insert_idx <= len(out):
                inserts.append(insert_idx)
                findings.append(
                    Finding(
                        path=path,
                        line=p2.start,
                        rule=2,
                        message="add blank line between procedures",
                        suggestion=None,
                    )
                )

        for idx in sorted(set(inserts), reverse=True):
            out.insert(idx, eol_from_neighbors(out, idx))

    return out, findings


def analyze_file(path: Path, enabled_rules: Set[int]) -> Tuple[List[Finding], List[str]]:
    """Analyze one source file and return findings plus transformed lines."""
    try:
        lines = path.read_text(encoding="utf-8", errors="ignore").splitlines(keepends=True)
    except OSError:
        return [], []
    updated, findings = transform_lines(path, lines, enabled_rules, annotate_fix=False)
    return findings, updated


def annotate_file(path: Path, findings: List[Finding], *, backup: bool = True) -> Tuple[int, Optional[Path]]:
    """Insert advisory comments for findings without applying code edits."""
    if not findings:
        return 0, None
    lines = path.read_text(encoding="utf-8", errors="ignore").splitlines(keepends=True)
    inserts: List[Tuple[int, str]] = []

    for f in findings:
        line_idx = max(0, min(len(lines) - 1, f.line - 1))
        anchor = line_idx
        if f.rule == 1:
            anchor = line_idx
            text = "! rule 1: delete the blank line above this RETURN"
        elif f.rule == 2:
            anchor = max(0, line_idx - 1)
            text = "! rule 2: add a blank line before this procedure"
        elif f.rule == 3:
            text = f"! rule 3: {f.message}"
            if f.suggestion:
                text += f" -> {f.suggestion}"
        elif f.rule == 4:
            text = f"! rule 4: {f.message}"
            if f.suggestion:
                text += f" -> {f.suggestion}"
        elif f.rule == 5:
            text = "! rule 5: remove blank line(s) after subroutine/function header"
        else:
            text = "! rule 6: remove line that is blank except for '!'"

        raw = lines[anchor] if lines else "\n"
        indent = re.match(r"^\s*", raw).group(0) if raw else ""
        eol = "\r\n" if raw.endswith("\r\n") else ("\n" if raw.endswith("\n") else "\n")
        msg = f"{indent}{text}  !! suggested by xspace.py{eol}"
        nxt = anchor + 1
        if 0 <= nxt < len(lines) and lines[nxt].strip().lower() == msg.strip().lower():
            continue
        inserts.append((anchor + 1, msg))

    if not inserts:
        return 0, None
    backup_path: Optional[Path] = None
    if backup:
        backup_path = make_backup_path(path)
        shutil.copy2(path, backup_path)
    for at, msg in sorted(inserts, key=lambda x: x[0], reverse=True):
        lines.insert(at, msg)
    path.write_text("".join(lines), encoding="utf-8")
    return len(inserts), backup_path


def apply_fix_file(
    path: Path,
    enabled_rules: Set[int],
    *,
    annotate: bool = False,
    out_path: Optional[Path] = None,
    create_backup: bool = True,
) -> Tuple[int, Optional[Path], List[Finding], Optional[str], Optional[str]]:
    """Apply enabled rules to one file."""
    original = path.read_text(encoding="utf-8", errors="ignore")
    original_lines = original.splitlines(keepends=True)
    updated_lines, findings = transform_lines(path, original_lines, enabled_rules, annotate_fix=annotate)
    updated = "".join(updated_lines)
    if updated == original:
        return 0, None, findings, original, updated
    backup: Optional[Path] = None
    target = out_path if out_path is not None else path
    if out_path is None and create_backup:
        backup = make_backup_path(path)
        shutil.copy2(path, backup)
    target.write_text(updated, encoding="utf-8")
    return 1, backup, findings, original, updated


def main() -> int:
    """Run spacing-rule advisory/fix workflow."""
    parser = argparse.ArgumentParser(
        description="Suggest/fix Fortran spacing with configurable rules (1..6)"
    )
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--verbose", action="store_true", help="Print replacement text/details for each finding")
    parser.add_argument("--fix", action="store_true", help="Apply enabled spacing rules in-place")
    parser.add_argument("--out", type=Path, help="With --fix, write transformed output to this file (single input)")
    parser.add_argument("--out-dir", type=Path, help="With --fix, write transformed outputs to this directory")
    parser.add_argument("--backup", dest="backup", action="store_true", default=True)
    parser.add_argument("--no-backup", dest="backup", action="store_false")
    parser.add_argument(
        "--annotate",
        action="store_true",
        help="Insert suggestion comments (or changed tags with --fix)",
    )
    parser.add_argument("--diff", action="store_true", help="With --fix, print unified diffs for changed files")
    parser.add_argument("--compiler", type=str, help="Compile command for baseline/after-fix validation")
    parser.add_argument("--limit", type=int, help="Maximum number of source files to process")
    parser.add_argument(
        "--disable",
        action="append",
        default=[],
        metavar="RULES",
        help="Disable rule number(s), e.g. --disable 2 --disable 3,4",
    )
    args = parser.parse_args()
    if args.limit is not None and args.limit < 1:
        print("--limit must be >= 1.")
        return 2
    if args.out is not None or args.out_dir is not None:
        args.fix = True
    if args.out is not None and args.out_dir is not None:
        print("Use only one of --out or --out-dir.")
        return 2

    if args.diff and not args.fix:
        print("--diff requires --fix.")
        return 2
    if args.compiler and not args.fix:
        print("--compiler requires --fix.")
        return 2

    disabled, err = parse_disabled_rules(args.disable)
    if err:
        print(err)
        return 2
    assert disabled is not None
    enabled_rules = {1, 2, 3, 4, 5, 6} - disabled
    if not enabled_rules:
        print("All rules are disabled; nothing to do.")
        return 0

    files = choose_files(args.fortran_files, args.exclude)
    if args.limit is not None:
        files = files[: args.limit]
    if not files:
        print("No source files remain after applying --exclude filters.")
        return 2
    if args.out is not None and len(files) != 1:
        print("--out requires exactly one input source file.")
        return 2
    if args.out_dir is not None:
        if args.out_dir.exists() and not args.out_dir.is_dir():
            print("--out-dir exists but is not a directory.")
            return 2
        args.out_dir.mkdir(parents=True, exist_ok=True)
    compile_paths = [args.out] if (args.fix and args.out is not None) else ([args.out_dir / p.name for p in files] if (args.fix and args.out_dir is not None) else files)
    if args.fix and args.compiler:
        if not fbuild.run_compiler_command(args.compiler, compile_paths, "baseline", fscan.display_path):
            return 5

    all_findings: List[Finding] = []
    findings_by_file: Dict[Path, List[Finding]] = {}
    for p in files:
        fnds, _updated = analyze_file(p, enabled_rules)
        if not fnds:
            continue
        findings_by_file[p] = fnds
        all_findings.extend(fnds)

    if not all_findings:
        print("No spacing/layout changes needed.")
        return 0

    all_findings.sort(key=lambda f: (f.path.name.lower(), f.line, f.rule))
    print(f"{len(all_findings)} spacing/layout finding(s).")
    for f in all_findings:
        print(f"{f.path.name}:{f.line} rule {f.rule} {f.message}")
        if args.verbose and f.suggestion:
            print(f"  suggest: {f.suggestion}")

    if args.fix:
        touched = 0
        for p in sorted(findings_by_file.keys(), key=lambda x: x.name.lower()):
            out_path = args.out if args.out is not None else (args.out_dir / p.name if args.out_dir is not None else None)
            changed, backup, _f, before, after = apply_fix_file(
                p, enabled_rules, annotate=args.annotate, out_path=out_path, create_backup=args.backup
            )
            if changed > 0:
                touched += 1
                if out_path is not None:
                    print(f"\nFixed {p.name}: updated, wrote {out_path}")
                else:
                    print(f"\nFixed {p.name}: updated, backup {backup.name if backup else '(none)'}")
                if args.diff and before is not None and after is not None:
                    diff_lines = difflib.unified_diff(
                        before.splitlines(),
                        after.splitlines(),
                        fromfile=f"a/{p.name}",
                        tofile=f"b/{(out_path.name if out_path is not None else p.name)}",
                        lineterm="",
                    )
                    print("")
                    for dl in diff_lines:
                        print(dl)
            elif args.verbose:
                print(f"\nNo fixes applied in {p.name}")
        print(f"\n--fix summary: files changed {touched}")
    elif args.annotate:
        total = 0
        touched = 0
        for p in sorted(findings_by_file.keys(), key=lambda x: x.name.lower()):
            n, backup = annotate_file(p, findings_by_file[p], backup=args.backup)
            total += n
            if n > 0:
                touched += 1
                print(f"\nAnnotated {p.name}: inserted {n}, backup {backup.name if backup else '(none)'}")
            elif args.verbose:
                print(f"\nNo annotations inserted in {p.name}")
        print(f"\n--annotate summary: files changed {touched}, inserted {total}")
    if args.fix and args.compiler:
        if not fbuild.run_compiler_command(args.compiler, compile_paths, "after-fix", fscan.display_path):
            return 5
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
