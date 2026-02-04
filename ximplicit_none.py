#!/usr/bin/env python3
"""Suggest/apply IMPLICIT NONE in Fortran program units."""

from __future__ import annotations

import argparse
import difflib
import shutil
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Set, Tuple
import re

import fortran_build as fbuild
import fortran_scan as fscan

PROGRAM_START_RE = re.compile(r"^\s*program\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
MODULE_START_RE = re.compile(r"^\s*module\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
PROC_START_RE = re.compile(
    r"^\s*(?:(?:pure|elemental|impure|recursive|module)\s+)*(function|subroutine)\s+([a-z][a-z0-9_]*)\b",
    re.IGNORECASE,
)
CONTAINS_RE = re.compile(r"^\s*contains\b", re.IGNORECASE)
IMPLICIT_NONE_RE = re.compile(r"^\s*implicit\s+none\b", re.IGNORECASE)
INTERFACE_START_RE = re.compile(r"^\s*(abstract\s+)?interface\b", re.IGNORECASE)
END_INTERFACE_RE = re.compile(r"^\s*end\s+interface\b", re.IGNORECASE)
USE_OR_IMPORT_RE = re.compile(r"^\s*(use\b|import\b)", re.IGNORECASE)


@dataclass
class UnitState:
    """Track one open Fortran program unit while scanning."""

    kind: str
    name: str
    start_idx: int
    indent: str
    has_implicit_none: bool
    in_module: bool
    parent_kind: Optional[str]


@dataclass
class Suggestion:
    """Represent one proposed IMPLICIT NONE insertion."""

    path: Path
    kind: str
    name: str
    line: int


def choose_files(args_files: List[Path], exclude: Iterable[str]) -> List[Path]:
    """Resolve source file list from args or current-directory defaults."""
    if args_files:
        files = args_files
    else:
        files = sorted(
            set(Path(".").glob("*.f90")) | set(Path(".").glob("*.F90")),
            key=lambda p: p.name.lower(),
        )
    return fscan.apply_excludes(files, exclude)


def eol_of(line: str) -> str:
    """Return line-ending characters used by the source line."""
    if line.endswith("\r\n"):
        return "\r\n"
    if line.endswith("\n"):
        return "\n"
    return "\n"


def header_end_index(lines: List[str], start_idx: int) -> int:
    """Return the last line index belonging to a continued header statement."""
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


def statement_text(lines: List[str], start_idx: int, end_idx: int) -> str:
    """Join a continued statement block into a single normalized string."""
    parts: List[str] = []
    for j in range(start_idx, end_idx + 1):
        code = fscan.strip_comment(lines[j].rstrip("\r\n")).rstrip()
        if j > start_idx:
            lead = code.lstrip()
            if lead.startswith("&"):
                code = lead[1:].lstrip()
        if code.endswith("&"):
            code = code[:-1].rstrip()
        if code.strip():
            parts.append(code.strip())
    return " ".join(parts).strip().lower()


def should_suggest(unit: UnitState) -> bool:
    """Return whether this unit should get an IMPLICIT NONE if missing."""
    if unit.has_implicit_none:
        return False
    if unit.kind in {"program", "module"}:
        return True
    # Procedures outside modules and not contained in another program unit.
    if unit.kind in {"function", "subroutine"} and not unit.in_module and unit.parent_kind is None:
        return True
    return False


def module_contains_insert_index(lines: List[str], start_idx: int) -> int:
    """Return index to insert IMPLICIT NONE in a module, after USE/IMPORT and before CONTAINS."""
    i = header_end_index(lines, start_idx) + 1
    while i < len(lines):
        low = fscan.strip_comment(lines[i].rstrip("\r\n")).strip().lower()
        if not low:
            i += 1
            continue
        if CONTAINS_RE.match(low):
            return i
        if low.startswith("end"):
            toks = low.split()
            if len(toks) == 1 or (len(toks) >= 2 and toks[1] == "module"):
                break
        end_idx = statement_end_index(lines, i)
        stmt_low = statement_text(lines, i, end_idx)
        if USE_OR_IMPORT_RE.match(stmt_low):
            i = end_idx + 1
            continue
        return i
        i += 1
    return header_end_index(lines, start_idx) + 1


def unit_insert_index(lines: List[str], start_idx: int) -> int:
    """Return insertion index for program/procedure units after header USE/IMPORT statements."""
    i = header_end_index(lines, start_idx) + 1
    while i < len(lines):
        low = fscan.strip_comment(lines[i].rstrip("\r\n")).strip().lower()
        if not low:
            i += 1
            continue
        if CONTAINS_RE.match(low):
            return i
        if low.startswith("end"):
            toks = low.split()
            if len(toks) == 1 or (len(toks) >= 2 and toks[1] in {"program", "function", "subroutine"}):
                return i
        end_idx = statement_end_index(lines, i)
        stmt_low = statement_text(lines, i, end_idx)
        if USE_OR_IMPORT_RE.match(stmt_low):
            i = end_idx + 1
            continue
        return i
    return header_end_index(lines, start_idx) + 1


def choose_indent(lines: List[str], start_idx: int, insert_at: int) -> str:
    """Choose indentation for inserted IMPLICIT NONE based on nearby specification lines."""
    # Prefer the nearest previous non-empty code line in the same unit.
    for i in range(min(insert_at - 1, len(lines) - 1), start_idx, -1):
        code = fscan.strip_comment(lines[i].rstrip("\r\n"))
        if code.strip():
            return re.match(r"^\s*", lines[i]).group(0)

    # Otherwise use indentation of the insertion-point line, if present.
    if 0 <= insert_at < len(lines):
        code = fscan.strip_comment(lines[insert_at].rstrip("\r\n"))
        if code.strip():
            return re.match(r"^\s*", lines[insert_at]).group(0)

    # Fallback: one level inside unit header.
    return re.match(r"^\s*", lines[start_idx]).group(0) + "  "


def scan_suggestions(path: Path, lines: List[str]) -> List[Suggestion]:
    """Scan one source file and return missing-IMPLICIT-NONE suggestions."""
    suggestions: List[Suggestion] = []
    stack: List[UnitState] = []
    interface_depth = 0

    for lineno, stmt in fscan.iter_fortran_statements(lines):
        low = stmt.strip().lower()
        if not low:
            continue

        if INTERFACE_START_RE.match(low):
            interface_depth += 1
            continue
        if END_INTERFACE_RE.match(low):
            interface_depth = max(0, interface_depth - 1)
            continue
        if interface_depth > 0:
            continue

        if IMPLICIT_NONE_RE.match(low):
            if stack:
                stack[-1].has_implicit_none = True
            continue

        m_prog = PROGRAM_START_RE.match(low)
        if m_prog:
            indent = re.match(r"^\s*", stmt).group(0)
            stack.append(
                UnitState(
                    kind="program",
                    name=m_prog.group(1).lower(),
                    start_idx=lineno - 1,
                    indent=indent,
                    has_implicit_none=False,
                    in_module=False,
                    parent_kind=stack[-1].kind if stack else None,
                )
            )
            continue

        m_mod = MODULE_START_RE.match(low)
        if m_mod:
            toks = low.split()
            if len(toks) >= 2 and toks[1] != "procedure":
                indent = re.match(r"^\s*", stmt).group(0)
                stack.append(
                    UnitState(
                        kind="module",
                        name=m_mod.group(1).lower(),
                        start_idx=lineno - 1,
                        indent=indent,
                        has_implicit_none=False,
                        in_module=False,
                        parent_kind=stack[-1].kind if stack else None,
                    )
                )
            continue

        m_proc = PROC_START_RE.match(low)
        if m_proc:
            indent = re.match(r"^\s*", stmt).group(0)
            in_module = any(u.kind == "module" for u in stack)
            stack.append(
                UnitState(
                    kind=m_proc.group(1).lower(),
                    name=m_proc.group(2).lower(),
                    start_idx=lineno - 1,
                    indent=indent,
                    has_implicit_none=False,
                    in_module=in_module,
                    parent_kind=stack[-1].kind if stack else None,
                )
            )
            continue

        if CONTAINS_RE.match(low):
            continue

        if low.startswith("end") and stack:
            toks = low.split()
            # Accept only unit-closing forms:
            #   end
            #   end program|module|function|subroutine
            # Ignore "end if", "end do", etc.
            is_unit_end = False
            end_kind = ""
            if len(toks) == 1:
                is_unit_end = True
            elif len(toks) >= 2 and toks[1] in {"program", "module", "function", "subroutine"}:
                is_unit_end = True
                end_kind = toks[1]
            if is_unit_end:
                if end_kind:
                    # Pop to matching unit kind, conservatively.
                    for i in range(len(stack) - 1, -1, -1):
                        if stack[i].kind == end_kind:
                            unit = stack.pop(i)
                            if should_suggest(unit):
                                suggestions.append(Suggestion(path, unit.kind, unit.name, unit.start_idx + 1))
                            break
                else:
                    unit = stack.pop()
                    if should_suggest(unit):
                        suggestions.append(Suggestion(path, unit.kind, unit.name, unit.start_idx + 1))

    while stack:
        unit = stack.pop()
        if should_suggest(unit):
            suggestions.append(Suggestion(path, unit.kind, unit.name, unit.start_idx + 1))
    return suggestions


def apply_fix(path: Path, suggestions: List[Suggestion], show_diff: bool, backup: bool) -> Tuple[int, Optional[Path]]:
    """Insert IMPLICIT NONE lines for selected suggestions in one file."""
    if not suggestions:
        return 0, None

    lines = path.read_text(encoding="utf-8", errors="ignore").splitlines(keepends=True)
    inserts: List[Tuple[int, str]] = []
    for s in suggestions:
        start_idx = s.line - 1
        if start_idx < 0 or start_idx >= len(lines):
            continue
        if s.kind == "module":
            insert_at = module_contains_insert_index(lines, start_idx)
        else:
            insert_at = unit_insert_index(lines, start_idx)
        indent = choose_indent(lines, start_idx, insert_at)
        new_line = f"{indent}implicit none{eol_of(lines[start_idx])}"
        inserts.append((insert_at, new_line))

    if not inserts:
        return 0, None

    # Deduplicate insertion points.
    dedup: Dict[int, str] = {}
    for idx, text in inserts:
        dedup.setdefault(idx, text)
    updated = lines[:]
    for idx, text in sorted(dedup.items(), key=lambda x: x[0], reverse=True):
        updated.insert(idx, text)

    if updated == lines:
        return 0, None

    if show_diff:
        diff = difflib.unified_diff(lines, updated, fromfile=str(path), tofile=str(path), lineterm="")
        print("\nProposed diff:")
        for d in diff:
            print(d)

    backup_path: Optional[Path] = None
    if backup:
        backup_path = path.with_name(path.name + ".bak")
        shutil.copy2(path, backup_path)
        print(f"Backup written: {backup_path.name}")

    path.write_text("".join(updated), encoding="utf-8", newline="")
    return len(dedup), backup_path


def print_summary(summary: Dict[Tuple[str, str], List[str]], label: str) -> None:
    """Print grouped summary lines for changed or suggested units."""
    if not summary:
        return
    n_fun = sum(len(v) for (fname, kind), v in summary.items() if kind == "function")
    n_sub = sum(len(v) for (fname, kind), v in summary.items() if kind == "subroutine")
    n_prog = sum(len(v) for (fname, kind), v in summary.items() if kind == "program")
    n_mod = sum(len(v) for (fname, kind), v in summary.items() if kind == "module")
    nfiles = len({fname for (fname, _k) in summary.keys()})
    file_word = "source file" if nfiles == 1 else "source files"
    print(
        f"\nSummary of {n_prog} program(s), {n_mod} module(s), "
        f"{n_fun} function(s), and {n_sub} subroutine(s) {label} in {nfiles} {file_word}:"
    )
    for (fname, kind) in sorted(summary.keys(), key=lambda x: (x[0].lower(), x[1])):
        items = summary[(fname, kind)]
        if items:
            print(f"{fname} {len(items)} {kind}(s): {' '.join(items)}")


def main() -> int:
    """Run missing-IMPLICIT-NONE suggestion and optional rewrite workflow."""
    parser = argparse.ArgumentParser(description="Suggest/apply IMPLICIT NONE in Fortran units")
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--fix", action="store_true", help="Insert IMPLICIT NONE where suggested")
    parser.add_argument("--backup", dest="backup", action="store_true", default=True)
    parser.add_argument("--no-backup", dest="backup", action="store_false")
    parser.add_argument("--diff", action="store_true")
    parser.add_argument("--compiler", type=str, help="Compile command for baseline/after-fix checks")
    args = parser.parse_args()

    files = choose_files(args.fortran_files, args.exclude)
    if not files:
        print("No source files remain after applying --exclude filters.")
        return 2

    infos, any_missing = fscan.load_source_files(files)
    if not infos:
        return 2 if any_missing else 1
    ordered_infos, _ = fscan.order_files_least_dependent(infos)
    if len(ordered_infos) > 1:
        print("Processing order:", " ".join(f.path.name for f in ordered_infos))

    compile_paths = [f.path for f in ordered_infos]
    if args.compiler:
        compile_paths, _ = fscan.build_compile_closure(ordered_infos)

    if args.fix and args.compiler:
        if not fbuild.run_compiler_command(args.compiler, compile_paths, "baseline", fscan.display_path):
            return 5

    summary: Dict[Tuple[str, str], List[str]] = {}
    changed = 0
    backups: List[Tuple[Path, Path]] = []
    for finfo in ordered_infos:
        suggestions = scan_suggestions(finfo.path, finfo.parsed_lines)
        for s in suggestions:
            summary.setdefault((finfo.path.name, s.kind), []).append(s.name)
        if args.fix:
            c, bak = apply_fix(finfo.path, suggestions, show_diff=args.diff, backup=args.backup)
            changed += c
            if bak:
                backups.append((finfo.path, bak))

    if args.fix and args.compiler:
        if not fbuild.run_compiler_command(args.compiler, compile_paths, "after-fix", fscan.display_path):
            fbuild.rollback_backups(backups, fscan.display_path)
            return 5

    if args.fix:
        print(f"\nInserted {changed} IMPLICIT NONE line(s).")
        print_summary(summary, "were marked implicit none")
    else:
        print_summary(summary, "can be marked implicit none")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
