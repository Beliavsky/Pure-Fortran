#!/usr/bin/env python3
"""Warn when Fortran functions perform external output (PRINT / WRITE(*,...))."""

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

PRINT_RE = re.compile(r"^\s*print\b", re.IGNORECASE)
WRITE_STAR_RE = re.compile(r"^\s*write\s*\(\s*\*", re.IGNORECASE)
WRITE_UNIT_STAR_RE = re.compile(r"^\s*write\s*\(\s*unit\s*=\s*\*", re.IGNORECASE)
LITERAL_TOKEN_RE = re.compile(r"'(?:''|[^'])*'|\"(?:\"\"|[^\"])*\"")
IF_THEN_RE = re.compile(r"^\s*if\s*\(.*\)\s*then\s*$", re.IGNORECASE)
END_IF_RE = re.compile(r"^\s*end\s*if\b", re.IGNORECASE)
ELSE_RE = re.compile(r"^\s*(else|elseif)\b", re.IGNORECASE)
BLOCK_START_RE = re.compile(r"^\s*(if\s*\(.*\)\s*then|do\b|select\b|where\b|forall\b|associate\b|block\b|critical\b)", re.IGNORECASE)
BLOCK_END_RE = re.compile(r"^\s*(end\s*if\b|end\s*do\b|end\s*select\b|end\s*where\b|end\s*forall\b|end\s*associate\b|end\s*block\b|end\s*critical\b)", re.IGNORECASE)


@dataclass
class Finding:
    """One function-output finding."""

    path: Path
    function_name: str
    line: int
    kind: str
    stmt: str


@dataclass
class ProcFindings:
    """Findings grouped by one function procedure."""

    proc: fscan.Procedure
    findings: List[Finding]


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
    """Return next available backup path."""
    base = Path(str(path) + ".bak")
    if not base.exists():
        return base
    idx = 1
    while True:
        cand = Path(f"{path}.bak{idx}")
        if not cand.exists():
            return cand
        idx += 1


def choose_unique_name(used: Set[str], base: str) -> str:
    """Pick a non-colliding identifier name."""
    if base.lower() not in used:
        return base
    i = 1
    while f"{base}{i}".lower() in used:
        i += 1
    return f"{base}{i}"


def decode_fortran_literal(token: str) -> str:
    """Decode a Fortran character literal token to plain text."""
    if len(token) < 2:
        return token
    if token[0] == "'" and token[-1] == "'":
        return token[1:-1].replace("''", "'")
    if token[0] == '"' and token[-1] == '"':
        return token[1:-1].replace('""', '"')
    return token


def encode_fortran_double_literal(text: str) -> str:
    """Encode plain text as a double-quoted Fortran literal."""
    return '"' + text.replace('"', '""') + '"'


def extract_message_literal(stmt: str) -> Optional[str]:
    """Extract first string literal from print/write statement and normalize as double-quoted literal."""
    m = LITERAL_TOKEN_RE.search(stmt)
    if not m:
        return None
    return encode_fortran_double_literal(decode_fortran_literal(m.group(0)))


def significant_code(line: str) -> str:
    """Return stripped code portion without trailing comment."""
    code, _comment = split_code_comment(line.rstrip("\r\n"))
    return code.strip()


def try_msg_error_stop_block_rewrite(lines: List[str], line_idx: int, arg: str, stmt_text: str) -> bool:
    """Try block-level rewrite for one print/write line; return True if applied."""
    if line_idx < 0 or line_idx >= len(lines):
        return False
    msg_lit = extract_message_literal(stmt_text)
    if msg_lit is None:
        esc = stmt_text.strip().replace("'", "''")
        msg_lit = f"'{esc}'"

    # Find enclosing immediate IF ... THEN line directly above (ignoring blank/comment lines).
    prev_idx = line_idx - 1
    while prev_idx >= 0 and not significant_code(lines[prev_idx]):
        prev_idx -= 1
    if prev_idx < 0 or not IF_THEN_RE.match(significant_code(lines[prev_idx])):
        return False

    # Find matching END IF for that outer IF (conservative depth tracking).
    depth = 1
    end_idx = -1
    for i in range(prev_idx + 1, len(lines)):
        code = significant_code(lines[i]).lower()
        if not code:
            continue
        if IF_THEN_RE.match(code):
            depth += 1
        elif END_IF_RE.match(code):
            depth -= 1
            if depth == 0:
                end_idx = i
                break
    if end_idx < 0 or end_idx <= line_idx:
        return False

    # Must not have else/elseif at outer level or nested blocks in the tail.
    tail_start = line_idx + 1
    tail_end = end_idx - 1
    for i in range(tail_start, end_idx):
        code = significant_code(lines[i])
        if not code:
            continue
        if ELSE_RE.match(code):
            return False
    for i in range(tail_start, tail_end + 1):
        code = significant_code(lines[i])
        if not code:
            continue
        if BLOCK_START_RE.match(code) or BLOCK_END_RE.match(code):
            return False

    tail = lines[tail_start : tail_end + 1]
    tail_shifted: List[str] = []
    for t in tail:
        if significant_code(t):
            tail_shifted.append("   " + t)
        else:
            tail_shifted.append(t)
    raw = lines[line_idx]
    indent = re.match(r"^\s*", raw).group(0) if raw else "      "
    eol = "\r\n" if raw.endswith("\r\n") else ("\n" if raw.endswith("\n") else "\n")
    body_indent = indent + "   "
    block: List[str] = [
        f"{indent}if (present({arg})) then{eol}",
        f"{body_indent}{arg} = {msg_lit}{eol}",
    ]
    block.extend(tail_shifted)
    block.extend(
        [
            f"{indent}else{eol}",
            f"{body_indent}error stop {msg_lit}{eol}",
            f"{indent}end if{eol}",
        ]
    )
    # Replace print/write + tail region; keep original outer end if.
    lines[line_idx:end_idx] = block
    return True


def parse_declared_names(proc: fscan.Procedure) -> Set[str]:
    """Collect declared names in one procedure body."""
    out: Set[str] = set(proc.dummy_names)
    if proc.result_name:
        out.add(proc.result_name.lower())
    for _ln, stmt in proc.body:
        low = stmt.strip().lower()
        if "::" in low:
            out.update(fscan.parse_declared_names_from_decl(low))
    return out


def declaration_insertion_line_for_new_dummy(proc: fscan.Procedure) -> int:
    """Choose insertion line: after last dummy declaration, before local declarations."""
    decl_re = re.compile(r"^\s*(integer|real|logical|character|complex|type\b|class\b|procedure\b)", re.IGNORECASE)
    first_local_decl_ln: Optional[int] = None
    last_dummy_decl_ln: Optional[int] = None

    for ln, stmt in proc.body:
        low = stmt.strip().lower()
        if not low:
            continue
        if low.startswith("use ") or low.startswith("implicit ") or low.startswith("import "):
            continue
        if not decl_re.match(low) or "::" not in low:
            # once executable code starts, stop scanning
            break
        names = fscan.parse_declared_names_from_decl(low)
        if not names:
            continue
        if any(n in proc.dummy_names for n in names):
            last_dummy_decl_ln = ln
        elif first_local_decl_ln is None:
            first_local_decl_ln = ln

    if first_local_decl_ln is not None:
        return first_local_decl_ln
    if last_dummy_decl_ln is not None:
        return last_dummy_decl_ln + 1
    # fallback: before first executable line
    for ln, stmt in proc.body:
        low = stmt.strip().lower()
        if not low:
            continue
        if low.startswith("use ") or low.startswith("implicit ") or low.startswith("import "):
            continue
        if decl_re.match(low):
            continue
        return ln
    return proc.end


def first_executable_line(proc: fscan.Procedure) -> int:
    """Return first executable statement line inside a procedure body."""
    decl_re = re.compile(r"^\s*(integer|real|logical|character|complex|type\b|class\b|procedure\b)", re.IGNORECASE)
    for ln, stmt in proc.body:
        low = stmt.strip().lower()
        if not low:
            continue
        if low.startswith("use ") or low.startswith("implicit ") or low.startswith("import "):
            continue
        if decl_re.match(low):
            continue
        return ln
    return proc.end


def patch_function_header_add_arg(line: str, argname: str) -> Optional[str]:
    """Add one dummy arg to a single-line function header."""
    code, comment = split_code_comment(line.rstrip("\r\n"))
    if "&" in code:
        return None
    m = re.search(r"\(([^)]*)\)", code)
    if not m:
        return None
    inner = m.group(1).strip()
    if inner:
        new_inner = f"{inner}, {argname}"
    else:
        new_inner = argname
    new_code = code[: m.start(1)] + new_inner + code[m.end(1) :]
    eol = "\r\n" if line.endswith("\r\n") else ("\n" if line.endswith("\n") else "")
    return f"{new_code}{comment}{eol}"


def analyze_file(path: Path) -> List[Finding]:
    """Analyze one source file for PRINT/WRITE(*) inside functions."""
    infos, any_missing = fscan.load_source_files([path])
    if not infos or any_missing:
        return []
    finfo = infos[0]
    out: List[Finding] = []

    for proc in finfo.procedures:
        if proc.kind.lower() != "function":
            continue
        for lineno, stmt in proc.body:
            s = stmt.strip()
            if not s:
                continue
            low = s.lower()
            if PRINT_RE.match(low):
                out.append(Finding(path=path, function_name=proc.name, line=lineno, kind="print", stmt=s))
            elif WRITE_STAR_RE.match(low) or WRITE_UNIT_STAR_RE.match(low):
                out.append(Finding(path=path, function_name=proc.name, line=lineno, kind="write(*)", stmt=s))
    return out


def group_findings_by_proc(path: Path, findings: List[Finding]) -> List[ProcFindings]:
    """Group findings by function procedure object."""
    infos, any_missing = fscan.load_source_files([path])
    if not infos or any_missing:
        return []
    finfo = infos[0]
    by_key: Dict[Tuple[str, int], List[Finding]] = {}
    for f in findings:
        by_key.setdefault((f.function_name.lower(), f.line), []).append(f)

    out: List[ProcFindings] = []
    for proc in finfo.procedures:
        if proc.kind.lower() != "function":
            continue
        pf: List[Finding] = []
        for ln, _stmt in proc.body:
            key = (proc.name.lower(), ln)
            if key in by_key:
                pf.extend(by_key[key])
        if pf:
            # de-dup by line
            uniq = {(x.line, x.stmt): x for x in pf}
            out.append(ProcFindings(proc=proc, findings=sorted(uniq.values(), key=lambda z: z.line)))
    return out


def apply_fix_file(
    path: Path,
    findings: List[Finding],
    mode: str,
    out_path: Optional[Path] = None,
    create_backup: bool = True,
) -> Tuple[int, int, Optional[Path], str]:
    """Apply one fix strategy to a file; returns (applied, skipped, backup, diff_text)."""
    infos, any_missing = fscan.load_source_files([path])
    if not infos or any_missing:
        return 0, 0, None, ""
    finfo = infos[0]
    lines = finfo.lines[:]
    before = "".join(lines)
    applied = 0
    skipped = 0

    groups = group_findings_by_proc(path, findings)
    line_to_finding: Dict[int, Finding] = {f.line: f for f in findings}

    if mode in {"suppress", "error-stop"}:
        for ln in sorted(line_to_finding.keys(), reverse=True):
            idx = ln - 1
            if idx < 0 or idx >= len(lines):
                skipped += 1
                continue
            raw = lines[idx]
            code, comment = split_code_comment(raw.rstrip("\r\n"))
            if ";" in code:
                skipped += 1
                continue
            indent = re.match(r"^\s*", raw).group(0) if raw else ""
            eol = "\r\n" if raw.endswith("\r\n") else ("\n" if raw.endswith("\n") else "")
            if mode == "suppress":
                lines[idx] = f"{indent}! xfunc_print suppressed: {code.strip()}{comment}{eol}"
            else:
                lines[idx] = f"{indent}error stop{comment}{eol}"
            applied += 1
    else:
        # msg/unit strategies need function header + declaration edits.
        for grp in sorted(groups, key=lambda g: g.proc.start, reverse=True):
            proc = grp.proc
            used = parse_declared_names(proc)
            is_msg_mode = mode in {"msg", "msg-error-stop", "msg-error-stop-block"}
            is_unit_mode = mode in {"unit", "unit-error-stop"}
            base = "msg" if is_msg_mode else "out_unit"
            arg = choose_unique_name(used, base)
            head_idx = proc.start - 1
            if head_idx < 0 or head_idx >= len(lines):
                skipped += len(grp.findings)
                continue
            new_head = patch_function_header_add_arg(lines[head_idx], arg)
            if new_head is None:
                skipped += len(grp.findings)
                continue

            decl_ins_line = declaration_insertion_line_for_new_dummy(proc)
            decl_ins_idx = max(0, min(len(lines), decl_ins_line - 1))
            exec_ins_line = first_executable_line(proc)
            exec_ins_idx = max(0, min(len(lines), exec_ins_line - 1))
            indent = "      "
            for ln, _stmt in proc.body:
                if ln == decl_ins_line:
                    raw = lines[ln - 1]
                    indent = re.match(r"^\s*", raw).group(0) if raw else indent
                    break
            eol = "\n"
            if lines and lines[head_idx].endswith("\r\n"):
                eol = "\r\n"

            decl = (
                f"{indent}character(len=*), intent(out), optional :: {arg}  !! added by xfunc_print.py{eol}"
                if is_msg_mode
                else f"{indent}integer, intent(in), optional :: {arg}  !! added by xfunc_print.py{eol}"
            )
            init = f"{indent}if (present({arg})) {arg} = \"\"{eol}" if is_msg_mode else ""

            # rewrite offending lines inside this function
            local_applied = 0
            for f in sorted(grp.findings, key=lambda x: x.line, reverse=True):
                idx = f.line - 1
                if idx < 0 or idx >= len(lines):
                    skipped += 1
                    continue
                raw = lines[idx]
                code, comment = split_code_comment(raw.rstrip("\r\n"))
                if ";" in code:
                    skipped += 1
                    continue
                s = code.strip()
                lindent = re.match(r"^\s*", raw).group(0) if raw else indent
                leol = "\r\n" if raw.endswith("\r\n") else ("\n" if raw.endswith("\n") else "")
                if is_msg_mode:
                    if mode == "msg-error-stop-block":
                        if try_msg_error_stop_block_rewrite(lines, idx, arg, s):
                            local_applied += 1
                            continue
                    lit = extract_message_literal(s)
                    if mode == "msg-error-stop":
                        if lit is None:
                            esc = s.replace("'", "''")
                            lit = f"'{esc}'"
                        lines[idx] = (
                            f"{lindent}if (present({arg})) then{comment}{leol}"
                            f"{lindent}   {arg} = {lit}{leol}"
                            f"{lindent}else{leol}"
                            f"{lindent}   error stop {lit}{leol}"
                            f"{lindent}end if{leol}"
                        )
                    elif lit is not None:
                        lines[idx] = f"{lindent}if (present({arg})) {arg} = {lit}{comment}{leol}"
                    else:
                        escaped = s.replace("'", "''")
                        lines[idx] = f"{lindent}if (present({arg})) {arg} = '{escaped}'{comment}{leol}"
                else:
                    escaped = s.replace("'", "''")
                    if mode == "unit-error-stop":
                        lit = extract_message_literal(s)
                        if lit is None:
                            lit = f"'{escaped}'"
                        lines[idx] = (
                            f"{lindent}if (present({arg})) then{comment}{leol}"
                            f"{lindent}   write({arg},'(a)') '{escaped}'{leol}"
                            f"{lindent}else{leol}"
                            f"{lindent}   error stop {lit}{leol}"
                            f"{lindent}end if{leol}"
                        )
                    else:
                        lines[idx] = f"{lindent}if (present({arg})) write({arg},'(a)') '{escaped}'{comment}{leol}"
                local_applied += 1

            if local_applied == 0:
                skipped += len(grp.findings)
                continue
            lines[head_idx] = new_head
            lines.insert(decl_ins_idx, decl)
            if decl_ins_idx <= exec_ins_idx:
                exec_ins_idx += 1
            if init:
                lines.insert(exec_ins_idx, init)
            applied += local_applied

    if applied == 0:
        return 0, skipped, None, ""
    backup: Optional[Path] = None
    if out_path is None and create_backup:
        backup = make_backup_path(path)
        shutil.copy2(path, backup)
    after = "".join(lines)
    target = out_path if out_path is not None else path
    target.write_text(after, encoding="utf-8")
    diff = "\n".join(
        difflib.unified_diff(
            before.splitlines(),
            after.splitlines(),
            fromfile=f"a/{path.name}",
            tofile=f"b/{(target.name if out_path is not None else path.name)}",
            lineterm="",
        )
    )
    return applied, skipped, backup, diff


def main() -> int:
    """Run function external-output checks across selected Fortran files."""
    parser = argparse.ArgumentParser(
        description="Warn when functions contain PRINT or WRITE(*,...) statements"
    )
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--verbose", action="store_true", help="Print offending statement text")
    parser.add_argument("--diff", action="store_true", help="With fix modes, print unified diffs for changed files")
    parser.add_argument("--out", type=Path, help="With a fix mode, write transformed output to this file (single input)")
    parser.add_argument("--backup", dest="backup", action="store_true", default=True)
    parser.add_argument("--no-backup", dest="backup", action="store_false")
    parser.add_argument("--compiler", type=str, help="Compile command for baseline/after-fix validation")
    fixg = parser.add_mutually_exclusive_group()
    fixg.add_argument("--fix-msg", action="store_true", help="Replace output statements with optional msg argument capture")
    fixg.add_argument(
        "--fix-msg-error-stop",
        action="store_true",
        help="Like --fix-msg, but use ERROR STOP when msg is not present",
    )
    fixg.add_argument(
        "--fix-msg-error-stop-block",
        action="store_true",
        help="Like --fix-msg-error-stop, but try block-level rewrite to keep following recovery lines in present(msg) path",
    )
    fixg.add_argument("--fix-unit", action="store_true", help="Route output statements to optional out_unit argument")
    fixg.add_argument(
        "--fix-unit-error-stop",
        action="store_true",
        help="Like --fix-unit, but use ERROR STOP when out_unit is not present",
    )
    fixg.add_argument("--fix-suppress", action="store_true", help="Comment out output statements in functions")
    fixg.add_argument("--fix-error-stop", action="store_true", help="Replace output statements with ERROR STOP")
    args = parser.parse_args()

    files = choose_files(args.fortran_files, args.exclude)
    if not files:
        print("No source files remain after applying --exclude filters.")
        return 2

    findings: List[Finding] = []
    for p in files:
        findings.extend(analyze_file(p))

    if not findings:
        print("No function PRINT/WRITE(*) findings.")
        return 0

    findings.sort(key=lambda x: (x.path.name.lower(), x.line, x.function_name.lower()))
    print(f"{len(findings)} function PRINT/WRITE(*) finding(s).")
    if args.verbose:
        for f in findings:
            print(f"{f.path.name}:{f.line} function {f.function_name} {f.kind}")
            print(f"  {f.stmt}")
    else:
        by_file = {}
        for f in findings:
            by_file[f.path.name] = by_file.get(f.path.name, 0) + 1
        for fname in sorted(by_file.keys(), key=str.lower):
            print(f"{fname}: {by_file[fname]}")
        first = findings[0]
        print(
            f"\nFirst finding: {first.path.name}:{first.line} "
            f"function {first.function_name} {first.kind}"
        )
        print("Run with --verbose to list all findings and offending lines.")

    mode = None
    if args.fix_msg:
        mode = "msg"
    elif args.fix_msg_error_stop:
        mode = "msg-error-stop"
    elif args.fix_msg_error_stop_block:
        mode = "msg-error-stop-block"
    elif args.fix_unit:
        mode = "unit"
    elif args.fix_unit_error_stop:
        mode = "unit-error-stop"
    elif args.fix_suppress:
        mode = "suppress"
    elif args.fix_error_stop:
        mode = "error-stop"

    if args.out is not None and mode is None:
        print("--out requires selecting one fix mode.")
        return 2
    if args.out is not None and len(files) != 1:
        print("--out requires exactly one input source file.")
        return 2
    if args.compiler and mode is None:
        print("--compiler requires selecting one fix mode.")
        return 2
    compile_paths = [args.out] if (mode is not None and args.out is not None) else files
    if mode is not None and args.compiler:
        if not fbuild.run_compiler_command(args.compiler, compile_paths, "baseline", fscan.display_path):
            return 5

    if mode is not None:
        by_file: Dict[Path, List[Finding]] = {}
        for f in findings:
            by_file.setdefault(f.path, []).append(f)
        total_applied = 0
        total_skipped = 0
        for p in sorted(by_file.keys(), key=lambda x: x.name.lower()):
            out_path = args.out if args.out is not None else None
            applied, skipped, backup, diff = apply_fix_file(
                p, by_file[p], mode=mode, out_path=out_path, create_backup=args.backup
            )
            total_applied += applied
            total_skipped += skipped
            if applied > 0 and out_path is not None:
                print(f"\nFixed {p.name}: applied {applied}, skipped {skipped}, wrote {out_path}")
            elif applied > 0 and backup is not None:
                print(f"\nFixed {p.name}: applied {applied}, skipped {skipped}, backup {backup.name}")
            elif applied > 0:
                print(f"\nFixed {p.name}: applied {applied}, skipped {skipped}")
            else:
                print(f"\nNo fixes applied to {p.name}")
            if args.diff and diff:
                print("")
                print(diff)
        print(f"\n--fix summary: applied {total_applied}, skipped {total_skipped}")
        if args.compiler:
            if not fbuild.run_compiler_command(args.compiler, compile_paths, "after-fix", fscan.display_path):
                return 5
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
