#!/usr/bin/env python3
"""Advisory checker for possible use-before-set variables in Fortran."""

from __future__ import annotations

import argparse
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Set, Tuple

import cli_paths as cpaths
import fortran_scan as fscan

PROGRAM_START_RE = re.compile(r"^\s*program\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
END_RE = re.compile(r"^\s*end(?:\s+(program|function|subroutine|module))?\b", re.IGNORECASE)
PROC_START_RE = re.compile(
    r"^\s*(?:(?:pure|elemental|impure|recursive|module)\s+)*(function|subroutine)\s+([a-z][a-z0-9_]*)\b",
    re.IGNORECASE,
)
TYPE_DECL_RE = re.compile(
    r"^\s*(integer|real|logical|character|complex|type\b|class\b|procedure\b)",
    re.IGNORECASE,
)
NO_COLON_DECL_RE = re.compile(
    r"^\s*(?P<spec>(?:integer|real|logical|complex|character)\s*(?:\([^)]*\))?"
    r"|type\s*\([^)]*\)|class\s*\([^)]*\))\s+(?P<rhs>.+)$",
    re.IGNORECASE,
)
ASSIGN_RE = re.compile(
    r"^\s*([a-z][a-z0-9_]*(?:\s*%\s*[a-z][a-z0-9_]*)?)\s*(?:\([^)]*\))?\s*=",
    re.IGNORECASE,
)
IDENT_RE = re.compile(r"\b([a-z][a-z0-9_]*)\b", re.IGNORECASE)
INTENT_RE = re.compile(r"\bintent\s*\(\s*(inout|out|in)\s*\)", re.IGNORECASE)
VALUE_RE = re.compile(r"\bvalue\b", re.IGNORECASE)
CONTROL_BARRIER_RE = re.compile(
    r"^\s*(if\b|else\b|elseif\b|end\s+if\b|do\b|end\s+do\b|select\b|case\b|where\b|forall\b|cycle\b|exit\b|goto\b)",
    re.IGNORECASE,
)
CALL_RE = re.compile(r"^\s*call\b", re.IGNORECASE)


@dataclass
class Unit:
    """Represent one analyzable program unit."""

    path: Path
    kind: str
    name: str
    start: int
    end: int
    body: List[Tuple[int, str]]
    dummy_names: Set[str]


@dataclass
class Issue:
    """Represent one use-before-set warning."""

    path: Path
    unit_kind: str
    unit_name: str
    line: int
    var_name: str
    detail: str


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


def split_top_level_commas(text: str) -> List[str]:
    """Split declaration RHS on top-level commas only."""
    out: List[str] = []
    cur: List[str] = []
    depth = 0
    in_single = False
    in_double = False
    for ch in text:
        if ch == "'" and not in_double:
            in_single = not in_single
        elif ch == '"' and not in_single:
            in_double = not in_double
        elif not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")" and depth > 0:
                depth -= 1
            elif ch == "," and depth == 0:
                out.append("".join(cur).strip())
                cur = []
                continue
        cur.append(ch)
    tail = "".join(cur).strip()
    if tail:
        out.append(tail)
    return out


def parse_declared_entities_with_init(stmt: str) -> Dict[str, bool]:
    """Parse declaration entities and whether each is initialized."""
    rhs = ""
    if "::" in stmt:
        rhs = stmt.split("::", 1)[1]
    else:
        m = NO_COLON_DECL_RE.match(stmt.strip())
        if not m:
            return {}
        rhs = m.group("rhs")
    out: Dict[str, bool] = {}
    for chunk in split_top_level_commas(rhs):
        text = chunk.strip()
        if not text:
            continue
        mm = re.match(r"^([a-z][a-z0-9_]*)", text, re.IGNORECASE)
        if not mm:
            continue
        name = mm.group(1).lower()
        initialized = ("=" in text and "=>" not in text)
        out[name] = initialized
    return out


def parse_program_units(finfo: fscan.SourceFileInfo) -> List[Unit]:
    """Parse main program blocks from one source file."""
    units: List[Unit] = []
    in_program = False
    prog_name = ""
    prog_start = -1
    prog_body: List[Tuple[int, str]] = []
    proc_depth = 0
    interface_depth = 0
    for lineno, stmt in fscan.iter_fortran_statements(finfo.parsed_lines):
        low = stmt.strip().lower()
        if not low:
            continue
        if re.match(r"^\s*(abstract\s+)?interface\b", low):
            interface_depth += 1
            continue
        if re.match(r"^\s*end\s+interface\b", low):
            interface_depth = max(0, interface_depth - 1)
            continue
        if interface_depth > 0:
            continue

        m_prog = PROGRAM_START_RE.match(low)
        if m_prog and not in_program:
            in_program = True
            prog_name = m_prog.group(1).lower()
            prog_start = lineno
            prog_body = []
            proc_depth = 0
            continue
        if not in_program:
            continue

        m_proc = PROC_START_RE.match(low)
        if m_proc:
            proc_depth += 1
        elif low.startswith("end"):
            toks = low.split()
            if len(toks) == 1 or (len(toks) >= 2 and toks[1] in {"function", "subroutine"}):
                if proc_depth > 0:
                    proc_depth -= 1
            if (len(toks) >= 2 and toks[1] == "program") or len(toks) == 1:
                if proc_depth == 0:
                    units.append(
                        Unit(
                            path=finfo.path,
                            kind="program",
                            name=prog_name,
                            start=prog_start,
                            end=lineno,
                            body=prog_body,
                            dummy_names=set(),
                        )
                    )
                    in_program = False
                    continue

        if proc_depth == 0:
            prog_body.append((lineno, stmt))
    return units


def parse_implicit_main_unit(finfo: fscan.SourceFileInfo) -> Optional[Unit]:
    """Parse top-level statements as an implicit main program when no PROGRAM exists."""
    explicit_programs = parse_program_units(finfo)
    if explicit_programs:
        return None

    body: List[Tuple[int, str]] = []
    interface_depth = 0
    module_depth = 0
    proc_depth = 0
    start_line = -1
    end_line = -1

    for lineno, stmt in fscan.iter_fortran_statements(finfo.parsed_lines):
        low = stmt.strip().lower()
        if not low:
            continue
        if re.match(r"^\s*(abstract\s+)?interface\b", low):
            interface_depth += 1
            continue
        if re.match(r"^\s*end\s+interface\b", low):
            interface_depth = max(0, interface_depth - 1)
            continue
        if interface_depth > 0:
            continue

        if PROGRAM_START_RE.match(low):
            return None
        if re.match(r"^\s*module\b", low):
            toks = low.split()
            if len(toks) >= 2 and toks[1] != "procedure":
                module_depth += 1
            continue
        if low.startswith("end"):
            toks = low.split()
            if len(toks) >= 2 and toks[1] == "module" and module_depth > 0:
                module_depth -= 1
                continue
            if len(toks) >= 2 and toks[1] in {"function", "subroutine"} and proc_depth > 0:
                proc_depth -= 1
                continue
            if len(toks) >= 2 and toks[1] == "program":
                # Explicit program close should not happen without explicit start, but be safe.
                continue
            if len(toks) == 1 and proc_depth > 0:
                proc_depth -= 1
                continue
            if len(toks) == 1 and module_depth > 0:
                module_depth -= 1
                continue

        if module_depth > 0:
            # Ignore module scope and module contained procedures.
            if PROC_START_RE.match(low):
                proc_depth += 1
            continue

        # External procedure definitions are not part of implicit main body.
        if PROC_START_RE.match(low):
            proc_depth += 1
            continue
        if proc_depth > 0:
            continue

        if start_line < 0:
            start_line = lineno
        end_line = lineno
        body.append((lineno, stmt))

    if not body:
        return None
    return Unit(
        path=finfo.path,
        kind="program",
        name="main",
        start=start_line,
        end=end_line,
        body=body,
        dummy_names=set(),
    )


def collect_units(finfo: fscan.SourceFileInfo) -> List[Unit]:
    """Collect procedures and program units for analysis."""
    out: List[Unit] = []
    for p in finfo.procedures:
        out.append(
            Unit(
                path=finfo.path,
                kind=p.kind.lower(),
                name=p.name.lower(),
                start=p.start,
                end=p.end,
                body=p.body,
                dummy_names=set(p.dummy_names),
            )
        )
    out.extend(parse_program_units(finfo))
    implicit_main = parse_implicit_main_unit(finfo)
    if implicit_main is not None:
        out.append(implicit_main)
    return out


def extract_ident_reads(expr: str, tracked: Set[str]) -> List[str]:
    """Extract tracked identifiers referenced in an expression."""
    out: List[str] = []
    seen: Set[str] = set()
    for m in IDENT_RE.finditer(expr.lower()):
        n = m.group(1).lower()
        if n in tracked and n not in seen:
            seen.add(n)
            out.append(n)
    return out


def analyze_unit(unit: Unit) -> List[Issue]:
    """Run conservative v1 use-before-set analysis on one unit."""
    issues: List[Issue] = []
    tracked: Set[str] = set()
    defined: Set[str] = set()
    intent_out: Set[str] = set()
    local_declared: Set[str] = set()
    dummy_intent: Dict[str, str] = {}

    # Dummies default: treat as defined except explicit INTENT(OUT).
    for d in unit.dummy_names:
        tracked.add(d)
        defined.add(d)

    for ln, stmt in unit.body:
        low = stmt.lower().strip()
        if not low:
            continue

        if TYPE_DECL_RE.match(low):
            decls = parse_declared_entities_with_init(low)
            if decls:
                tracked.update(decls.keys())
                local_declared.update(decls.keys())
                for n, init in decls.items():
                    if init:
                        defined.add(n)
            m_int = INTENT_RE.search(low)
            if m_int:
                intent = m_int.group(1).lower()
                for d in unit.dummy_names:
                    if d in decls:
                        dummy_intent[d] = intent
                        if intent == "out":
                            if d in defined:
                                defined.remove(d)
                            intent_out.add(d)
                        elif intent in {"in", "inout"}:
                            defined.add(d)
            elif VALUE_RE.search(low):
                for d in unit.dummy_names:
                    if d in decls:
                        defined.add(d)
            continue

        if low.startswith("implicit ") or low.startswith("use ") or low.startswith("contains"):
            continue
        if CALL_RE.match(low):
            # Conservative v1: skip CALL argument semantics.
            continue
        if CONTROL_BARRIER_RE.match(low):
            # Conservative v1: skip control-flow-heavy statements.
            continue

        m_assign = ASSIGN_RE.match(low)
        if m_assign:
            lhs_full = m_assign.group(1)
            lhs_base = fscan.base_identifier(lhs_full) or ""
            rhs = low.split("=", 1)[1] if "=" in low else ""
            lhs_idx_reads = extract_ident_reads(lhs_full, tracked)
            if lhs_base and lhs_base in lhs_idx_reads:
                lhs_idx_reads = [x for x in lhs_idx_reads if x != lhs_base]
            reads = lhs_idx_reads + [x for x in extract_ident_reads(rhs, tracked) if x not in lhs_idx_reads]
            for n in reads:
                if n not in defined:
                    kind = "INTENT(OUT) dummy" if n in intent_out else "variable"
                    issues.append(
                        Issue(
                            path=unit.path,
                            unit_kind=unit.kind,
                            unit_name=unit.name,
                            line=ln,
                            var_name=n,
                            detail=f"{kind} may be used before being set",
                        )
                    )
            if lhs_base and lhs_base in tracked:
                defined.add(lhs_base)
            continue

        reads = extract_ident_reads(low, tracked)
        for n in reads:
            if n not in defined:
                kind = "INTENT(OUT) dummy" if n in intent_out else "variable"
                issues.append(
                    Issue(
                        path=unit.path,
                        unit_kind=unit.kind,
                        unit_name=unit.name,
                        line=ln,
                        var_name=n,
                        detail=f"{kind} may be used before being set",
                    )
                )
    return issues


def main() -> int:
    """Run advisory use-before-set checks across selected Fortran files."""
    parser = argparse.ArgumentParser(
        description="Advisory checker for possible use-before-set variables (no --fix mode)"
    )
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--verbose", action="store_true", help="Print all findings (default prints summary + first)")
    args = parser.parse_args()

    files = choose_files(args.fortran_files, args.exclude)
    if not files:
        print("No source files remain after applying --exclude filters.")
        return 2

    infos, any_missing = fscan.load_source_files(files)
    if not infos:
        return 2 if any_missing else 1
    ordered_infos, _ = fscan.order_files_least_dependent(infos)

    all_issues: List[Issue] = []
    for finfo in ordered_infos:
        for unit in collect_units(finfo):
            all_issues.extend(analyze_unit(unit))

    if not all_issues:
        print("No possible use-before-set findings.")
        return 0

    all_issues.sort(key=lambda x: (x.path.name.lower(), x.line, x.unit_kind, x.unit_name, x.var_name))
    by_file: Dict[str, int] = {}
    for i in all_issues:
        by_file[i.path.name] = by_file.get(i.path.name, 0) + 1

    print(f"{len(all_issues)} possible use-before-set finding(s) in {len(by_file)} file(s).")
    if args.verbose:
        for i in all_issues:
            print(f"{i.path.name}:{i.line} {i.unit_kind} {i.unit_name} {i.var_name} - {i.detail}")
    else:
        for fname in sorted(by_file.keys(), key=str.lower):
            print(f"{fname}: {by_file[fname]}")
        first = all_issues[0]
        print(
            f"\nFirst finding: {first.path.name}:{first.line} "
            f"{first.unit_kind} {first.unit_name} {first.var_name} - {first.detail}"
        )
        print("Run with --verbose to list all findings.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
