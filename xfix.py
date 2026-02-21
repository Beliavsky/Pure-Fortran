#!/usr/bin/env python3
"""xfix.py: conservative auto-fixes for common Fortran typos."""

from __future__ import annotations

import argparse
import glob
import re
from dataclasses import dataclass
from pathlib import Path
from typing import List, Tuple

import fortran_scan as fscan


@dataclass
class FixFinding:
    line: int
    kind: str
    message: str


def _expand_inputs(items: List[str]) -> List[Path]:
    out: List[Path] = []
    seen = set()
    exts = (".f90", ".f95", ".f03", ".f08", ".f", ".for")
    for raw in items:
        p = Path(raw)
        cands: List[Path]
        if any(ch in raw for ch in "*?[]"):
            cands = [Path(s) for s in glob.glob(raw)]
        elif p.is_dir():
            cands = []
            for ext in exts:
                cands.extend(sorted(p.glob(f"*{ext}")))
        else:
            cands = [p]
        for c in cands:
            if c.exists() and c.is_file():
                key = str(c.resolve()).lower()
                if key not in seen:
                    seen.add(key)
                    out.append(c)
    return out


def _split_code_comment(line: str) -> Tuple[str, str, str]:
    """Return (code, comment, eol)."""
    eol = "\n" if line.endswith("\n") else ""
    s = line[:-1] if eol else line
    in_single = False
    in_double = False
    for i, ch in enumerate(s):
        if ch == "'" and not in_double:
            in_single = not in_single
        elif ch == '"' and not in_single:
            in_double = not in_double
        elif ch == "!" and not in_single and not in_double:
            return s[:i], s[i:], eol
    return s, "", eol


def _paren_delta(stmt: str) -> int:
    depth = 0
    in_single = False
    in_double = False
    i = 0
    while i < len(stmt):
        ch = stmt[i]
        if ch == "'" and not in_double:
            if in_single and i + 1 < len(stmt) and stmt[i + 1] == "'":
                i += 2
                continue
            in_single = not in_single
        elif ch == '"' and not in_single:
            in_double = not in_double
        elif not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
        i += 1
    return depth


def _apply_end_name_fix(line: str, kind: str, expected: str) -> str:
    code, comment, eol = _split_code_comment(line)
    pat = re.compile(rf"^(\s*end\s+{kind}\s+)([a-z_]\w*)(\b.*)$", re.IGNORECASE)
    m = pat.match(code)
    if not m:
        return line
    new_code = f"{m.group(1)}{expected}{m.group(3)}"
    return f"{new_code}{comment}{eol}"


def fix_text(text: str) -> Tuple[str, List[FixFinding]]:
    lines = text.splitlines(keepends=True)
    findings: List[FixFinding] = []

    # Pass 0: remove duplicate function/subroutine definitions, keep last.
    def _scope_by_line(src_lines: List[str]) -> List[str]:
        scopes = ["global"] * len(src_lines)
        stack: List[str] = []
        mod_start = re.compile(r"^\s*module\s+([a-z_]\w*)\b", re.IGNORECASE)
        mod_end = re.compile(r"^\s*end\s+module\b", re.IGNORECASE)
        prog_start = re.compile(r"^\s*program\s+([a-z_]\w*)\b", re.IGNORECASE)
        prog_end = re.compile(r"^\s*end\s+program\b", re.IGNORECASE)
        for i, raw in enumerate(src_lines, start=1):
            code = fscan.strip_comment(raw).strip().lower()
            m = mod_start.match(code)
            if m and not code.startswith("module procedure"):
                stack.append(f"module:{m.group(1).lower()}")
            m = prog_start.match(code)
            if m:
                stack.append(f"program:{m.group(1).lower()}")
            scopes[i - 1] = stack[-1] if stack else "global"
            if mod_end.match(code):
                if stack and stack[-1].startswith("module:"):
                    stack.pop()
            elif prog_end.match(code):
                if stack and stack[-1].startswith("program:"):
                    stack.pop()
        return scopes

    procs = fscan.parse_procedures([ln.rstrip("\r\n") for ln in lines])
    scope_map = _scope_by_line(lines)
    by_key: dict[tuple[str, str, str, str], list] = {}
    for p in procs:
        if p.kind.lower() not in {"function", "subroutine"}:
            continue
        scope = scope_map[p.start - 1] if 1 <= p.start <= len(scope_map) else "global"
        parent = (p.parent or "").lower()
        key = (scope, parent, p.name.lower(), p.kind.lower())
        by_key.setdefault(key, []).append(p)

    remove_lines = set()
    for key, items in by_key.items():
        if len(items) <= 1:
            continue
        items_sorted = sorted(items, key=lambda q: q.start)
        keep = items_sorted[-1]
        for dup in items_sorted[:-1]:
            for ln in range(max(1, dup.start), min(len(lines), dup.end) + 1):
                remove_lines.add(ln)
            findings.append(
                FixFinding(
                    line=dup.start,
                    kind="duplicate-proc",
                    message=(
                        f"removed duplicate {dup.kind.lower()} '{dup.name}' "
                        f"(lines {dup.start}-{dup.end}); kept last at line {keep.start}"
                    ),
                )
            )
    if remove_lines:
        lines = [ln for idx, ln in enumerate(lines, start=1) if idx not in remove_lines]

    # Pass 0b: remove duplicate declarations, keeping earliest declaration.
    declish_re = re.compile(
        r"^\s*(?:integer|real|logical|character|complex|type\b|class\b|double\s+precision)\b",
        re.IGNORECASE,
    )
    units = fscan.split_fortran_units_simple("".join(lines))
    remove_names_by_line: dict[int, set[str]] = {}
    first_decl_line_by_name: dict[tuple[str, str], int] = {}

    for u in units:
        body = list(u.get("body_lines", []))
        line_nos = list(u.get("body_line_nos", []))
        seen_exec = False
        for idx, stmt in enumerate(body):
            code = fscan.strip_comment(stmt).strip()
            if not code:
                continue
            low = code.lower()
            if seen_exec:
                break
            if low == "implicit none" or low.startswith("use "):
                continue
            if "::" in code and declish_re.match(code):
                line_no = line_nos[idx] if idx < len(line_nos) else -1
                if not (1 <= line_no <= len(lines)):
                    continue
                # Only rewrite unambiguous one-statement lines.
                code_only = _split_code_comment(lines[line_no - 1])[0].strip()
                if len(fscan.split_fortran_statements(code_only)) != 1:
                    continue
                names = [nm for nm, _ in fscan.parse_declared_entities(code)]
                if not names:
                    continue
                for nm in names:
                    key = (f"{u['kind']}:{u['name']}", nm.lower())
                    first = first_decl_line_by_name.get(key)
                    if first is None:
                        first_decl_line_by_name[key] = line_no
                        continue
                    remove_names_by_line.setdefault(line_no, set()).add(nm.lower())
                    findings.append(
                        FixFinding(
                            line=line_no,
                            kind="duplicate-decl",
                            message=(
                                f"removed duplicate declaration of '{nm}' "
                                f"(kept earliest at line {first})"
                            ),
                        )
                    )
                continue
            seen_exec = True

    if remove_names_by_line:
        updated: dict[int, str | None] = {}
        for line_no, names in remove_names_by_line.items():
            new_line, changed = fscan.rewrite_decl_remove_names(lines[line_no - 1], names)
            if changed:
                updated[line_no] = new_line
        if updated:
            lines = [
                (updated[idx] if idx in updated else ln)
                for idx, ln in enumerate(lines, start=1)
                if not (idx in updated and updated[idx] is None)
            ]

    # Pass 1: mismatched end-name fixes.
    unit_stack: List[Tuple[str, str]] = []
    for lineno, stmt in fscan.iter_fortran_statements(lines):
        s = stmt.strip()
        low = s.lower()
        raw = lines[lineno - 1]
        code_only = _split_code_comment(raw)[0].strip()
        sole_stmt = (
            len(fscan.split_fortran_statements(code_only)) == 1
            and code_only == s
        )
        m_mod = re.match(r"^module\s+([a-z_]\w*)\b", low)
        if m_mod and not low.startswith("module procedure"):
            unit_stack.append(("module", m_mod.group(1).lower()))
            continue
        m_prog = re.match(r"^program\s+([a-z_]\w*)\b", low)
        if m_prog:
            unit_stack.append(("program", m_prog.group(1).lower()))
            continue
        m_fun = re.match(
            r"^(?:(?:pure|elemental|impure|recursive|module)\s+)*function\s+([a-z_]\w*)\b",
            low,
        )
        if m_fun:
            unit_stack.append(("function", m_fun.group(1).lower()))
            continue

        for kind in ("module", "program", "function"):
            m_end = re.match(rf"^end\s+{kind}(?:\s+([a-z_]\w*))?\b", low)
            if not m_end:
                continue
            end_name = (m_end.group(1) or "").lower()
            if unit_stack and unit_stack[-1][0] == kind:
                expected = unit_stack[-1][1]
                if end_name and end_name != expected and sole_stmt:
                    lines[lineno - 1] = _apply_end_name_fix(lines[lineno - 1], kind, expected)
                    findings.append(
                        FixFinding(
                            line=lineno,
                            kind="end-name",
                            message=f"fixed mismatched end {kind} name '{end_name}' -> '{expected}'",
                        )
                    )
                unit_stack.pop()
            break

    # Pass 2: unambiguous missing ')' fixes on one-line single statements.
    for lineno, raw in enumerate(lines, start=1):
        code, comment, eol = _split_code_comment(raw)
        parts = fscan.split_fortran_statements(code.strip())
        if len(parts) != 1:
            continue
        stmt = parts[0].strip()
        if not stmt:
            continue
        low = stmt.lower()
        if low.startswith("end"):
            continue
        if stmt.endswith("&"):
            continue
        # only fix when exactly one ')' missing and appending at end is plausible
        delta = _paren_delta(stmt)
        if delta == 1 and re.search(r"[a-z0-9_\]\)]\s*$", stmt, re.IGNORECASE):
            new_code = f"{code})"
            lines[lineno - 1] = f"{new_code}{comment}{eol}"
            findings.append(
                FixFinding(
                    line=lineno,
                    kind="paren",
                    message="added one closing parenthesis ')'",
                )
            )

    return "".join(lines), findings


def main() -> int:
    ap = argparse.ArgumentParser(description="Conservatively auto-fix simple Fortran typos.")
    ap.add_argument("fortran_files", nargs="*", help="Fortran files, globs, or directories.")
    ap.add_argument("--fix", action="store_true", help="Write fixed files.")
    ap.add_argument("--out", help="Output file for single input (implies --fix).")
    ap.add_argument("--out-dir", help="Output directory for multiple inputs (implies --fix).")
    ap.add_argument("--verbose", action="store_true", help="Show files without fixes.")
    args = ap.parse_args()

    files = _expand_inputs(args.fortran_files or ["*.f90"])
    if not files:
        print("No Fortran files found.")
        return 1

    do_fix = args.fix or bool(args.out) or bool(args.out_dir)
    if args.out and len(files) != 1:
        print("--out requires exactly one input file.")
        return 1
    if args.out and args.out_dir:
        print("Use either --out or --out-dir, not both.")
        return 1
    out_dir = Path(args.out_dir) if args.out_dir else None
    if out_dir is not None:
        out_dir.mkdir(parents=True, exist_ok=True)

    changed = 0
    total_findings = 0
    for p in files:
        before = p.read_text(encoding="utf-8", errors="ignore")
        after, findings = fix_text(before)
        total_findings += len(findings)
        if findings:
            print(f"{p}: {len(findings)} fix(es)")
            for f in findings:
                print(f"  line {f.line} [{f.kind}] {f.message}")
            if do_fix:
                if args.out:
                    out_path = Path(args.out)
                elif out_dir is not None:
                    out_path = out_dir / p.name
                else:
                    out_path = p
                out_path.write_text(after, encoding="utf-8")
                changed += 1
                print(f"  wrote {out_path}")
        elif args.verbose:
            print(f"{p}: no fixes")

    if not do_fix:
        print(f"Summary: {total_findings} potential fix(es) in {len(files)} file(s).")
    else:
        print(f"--fix summary: files changed {changed}, fixes applied {total_findings}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
