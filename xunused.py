#!/usr/bin/env python3
"""Advisory checker for likely unused set variables and constants in Fortran."""

from __future__ import annotations

import argparse
import difflib
import re
import shutil
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Set, Tuple

import fortran_scan as fscan

PROGRAM_START_RE = re.compile(r"^\s*program\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
MODULE_START_RE = re.compile(r"^\s*module\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
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
ACCESS_RE = re.compile(r"^\s*(public|private)\b(.*)$", re.IGNORECASE)
CONTAINS_RE = re.compile(r"^\s*contains\b", re.IGNORECASE)


@dataclass
class Unit:
    """Represent one analyzable procedure or main program unit."""

    path: Path
    kind: str
    name: str
    start: int
    end: int
    body: List[Tuple[int, str]]
    dummy_names: Set[str]
    result_name: Optional[str]


@dataclass
class ModuleSymbol:
    """Represent one module-scope declaration candidate."""

    path: Path
    module: str
    name: str
    decl_line: int
    is_public: bool
    is_parameter: bool
    initialized: bool


@dataclass
class Issue:
    """Represent one likely-unused finding."""

    path: Path
    line: int
    category: str
    context: str
    name: str
    detail: str


def choose_files(args_files: List[Path], exclude: Iterable[str]) -> List[Path]:
    """Resolve source files from args or current-directory defaults."""
    if args_files:
        files = args_files
    else:
        files = sorted(
            set(Path(".").glob("*.f90")) | set(Path(".").glob("*.F90")),
            key=lambda p: p.name.lower(),
        )
    return fscan.apply_excludes(files, exclude)


def split_top_level_commas(text: str) -> List[str]:
    """Split declaration RHS on top-level commas."""
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


def parse_decl_entities(stmt: str) -> Tuple[Dict[str, bool], bool]:
    """Parse declaration entity names with initialization and PARAMETER flag."""
    spec = ""
    rhs = ""
    if "::" in stmt:
        spec, rhs = stmt.split("::", 1)
    else:
        m = NO_COLON_DECL_RE.match(stmt.strip())
        if not m:
            return {}, False
        spec = m.group("spec")
        rhs = m.group("rhs")

    is_parameter = "parameter" in spec.lower()
    out: Dict[str, bool] = {}
    for chunk in split_top_level_commas(rhs):
        text = chunk.strip()
        if not text:
            continue
        m = re.match(r"^([a-z][a-z0-9_]*)", text, re.IGNORECASE)
        if not m:
            continue
        name = m.group(1).lower()
        initialized = ("=" in text and "=>" not in text)
        out[name] = initialized
    return out, is_parameter


def parse_program_units(finfo: fscan.SourceFileInfo) -> List[Unit]:
    """Parse explicit and implicit main program units from one file."""
    units: List[Unit] = []
    explicit = False
    in_program = False
    name = "main"
    start = -1
    body: List[Tuple[int, str]] = []
    proc_depth = 0
    module_depth = 0
    ext_proc_depth = 0

    for lineno, stmt in fscan.iter_fortran_statements(finfo.parsed_lines):
        low = stmt.strip().lower()
        if not low:
            continue
        if re.match(r"^\s*(abstract\s+)?interface\b", low):
            continue
        if re.match(r"^\s*end\s+interface\b", low):
            continue
        m_prog = PROGRAM_START_RE.match(low)
        if m_prog and not in_program:
            explicit = True
            in_program = True
            name = m_prog.group(1).lower()
            start = lineno
            body = []
            proc_depth = 0
            continue
        if in_program:
            m_proc = PROC_START_RE.match(low)
            if m_proc:
                proc_depth += 1
            elif low.startswith("end"):
                toks = low.split()
                if len(toks) == 1 or (len(toks) >= 2 and toks[1] in {"function", "subroutine"}):
                    if proc_depth > 0:
                        proc_depth -= 1
                if ((len(toks) >= 2 and toks[1] == "program") or len(toks) == 1) and proc_depth == 0:
                    units.append(Unit(finfo.path, "program", name, start, lineno, body, set(), None))
                    in_program = False
                    continue
            if proc_depth == 0:
                body.append((lineno, stmt))
        else:
            # implicit main (file without explicit PROGRAM and outside procedures/modules)
            if explicit:
                continue
            m_mod = MODULE_START_RE.match(low)
            if m_mod:
                toks = low.split()
                if len(toks) >= 2 and toks[1] != "procedure":
                    module_depth += 1
                continue
            if module_depth > 0:
                if low.startswith("end"):
                    toks = low.split()
                    if len(toks) >= 2 and toks[1] == "module":
                        module_depth = max(0, module_depth - 1)
                continue

            if PROC_START_RE.match(low):
                ext_proc_depth += 1
                continue
            if low.startswith("end"):
                toks = low.split()
                if len(toks) == 1 or (len(toks) >= 2 and toks[1] in {"function", "subroutine"}):
                    if ext_proc_depth > 0:
                        ext_proc_depth -= 1
                continue
            if ext_proc_depth > 0:
                continue
            body.append((lineno, stmt))

    if not explicit and body:
        units.append(Unit(finfo.path, "program", "main", body[0][0], body[-1][0], body, set(), None))
    return units


def collect_units(finfo: fscan.SourceFileInfo) -> List[Unit]:
    """Collect procedure and main-program units for local analysis."""
    out: List[Unit] = []
    for p in finfo.procedures:
        out.append(
            Unit(
                finfo.path,
                p.kind.lower(),
                p.name.lower(),
                p.start,
                p.end,
                p.body,
                set(p.dummy_names),
                p.result_name,
            )
        )
    out.extend(parse_program_units(finfo))
    return out


def extract_reads(stmt: str, tracked: Set[str]) -> List[str]:
    """Extract tracked identifier reads from one statement."""
    out: List[str] = []
    seen: Set[str] = set()
    for m in IDENT_RE.finditer(stmt.lower()):
        n = m.group(1).lower()
        if n in tracked and n not in seen:
            seen.add(n)
            out.append(n)
    return out


def analyze_unit(unit: Unit) -> List[Issue]:
    """Find locals/constants written but never read in a unit body."""
    issues: List[Issue] = []
    tracked: Set[str] = set()
    writes: Set[str] = set()
    reads: Set[str] = set()
    decl_line: Dict[str, int] = {}
    is_parameter: Set[str] = set()

    tracked.update(unit.dummy_names)
    if unit.result_name:
        tracked.add(unit.result_name.lower())

    for ln, stmt in unit.body:
        low = stmt.lower().strip()
        if not low:
            continue
        if TYPE_DECL_RE.match(low):
            decls, param = parse_decl_entities(low)
            for n, init in decls.items():
                tracked.add(n)
                decl_line.setdefault(n, ln)
                if param:
                    is_parameter.add(n)
                if init:
                    writes.add(n)
            continue

        m_as = ASSIGN_RE.match(low)
        if m_as:
            lhs = m_as.group(1)
            lhs_base = fscan.base_identifier(lhs) or ""
            rhs = low.split("=", 1)[1] if "=" in low else ""
            lhs_reads = extract_reads(lhs, tracked)
            if lhs_base:
                lhs_reads = [x for x in lhs_reads if x != lhs_base]
            for n in lhs_reads:
                reads.add(n)
            for n in extract_reads(rhs, tracked):
                reads.add(n)
            if lhs_base in tracked:
                writes.add(lhs_base)
                decl_line.setdefault(lhs_base, ln)
            continue

        for n in extract_reads(low, tracked):
            reads.add(n)

    skip = set(unit.dummy_names)
    if unit.result_name:
        skip.add(unit.result_name.lower())
    for n in sorted(tracked):
        if n in skip:
            continue
        if n in writes and n not in reads:
            cat = "named-constant" if n in is_parameter else "variable"
            issues.append(
                Issue(
                    path=unit.path,
                    line=decl_line.get(n, unit.body[0][0] if unit.body else 1),
                    category=cat,
                    context=f"{unit.kind} {unit.name}",
                    name=n,
                    detail="set but never read in this unit",
                )
            )
    return issues


def analyze_unit_dead_stores(unit: Unit) -> List[Issue]:
    """Find likely dead-store assignments with branch-aware IF/ELSE handling."""
    issues: List[Issue] = []
    emitted: Set[Tuple[int, str, str]] = set()
    tracked: Set[str] = set()
    skip = set(unit.dummy_names)
    if unit.result_name:
        skip.add(unit.result_name.lower())

    tracked.update(unit.dummy_names)
    if unit.result_name:
        tracked.add(unit.result_name.lower())

    IF_THEN_RE = re.compile(r"^\s*if\s*\(.*\)\s*then\b", re.IGNORECASE)
    ELSE_IF_RE = re.compile(r"^\s*else\s*if\s*\(.*\)\s*then\b", re.IGNORECASE)
    ELSE_RE = re.compile(r"^\s*else\b", re.IGNORECASE)
    END_IF_RE = re.compile(r"^\s*(end\s*if|endif)\b", re.IGNORECASE)

    def clone_states(states: List[Dict[str, int]]) -> List[Dict[str, int]]:
        return [dict(s) for s in states]

    def dedupe_states(states: List[Dict[str, int]]) -> List[Dict[str, int]]:
        seen: Set[Tuple[Tuple[str, int], ...]] = set()
        out: List[Dict[str, int]] = []
        for st in states:
            key = tuple(sorted(st.items()))
            if key in seen:
                continue
            seen.add(key)
            out.append(st)
        return out

    def emit_issue(line: int, name: str, detail: str) -> None:
        key = (line, name, detail)
        if key in emitted:
            return
        emitted.add(key)
        issues.append(
            Issue(
                path=unit.path,
                line=line,
                category="dead-store",
                context=f"{unit.kind} {unit.name}",
                name=name,
                detail=detail,
            )
        )

    def consume_reads(state: Dict[str, int], names: List[str]) -> None:
        for n in names:
            state.pop(n, None)

    def apply_stmt_to_state(state: Dict[str, int], low: str, ln: int) -> None:
        if TYPE_DECL_RE.match(low):
            decls, _param = parse_decl_entities(low)
            for n, init in decls.items():
                tracked.add(n)
                if init and n not in skip:
                    state[n] = ln
            return

        m_as = ASSIGN_RE.match(low)
        if m_as:
            lhs = m_as.group(1)
            lhs_base = fscan.base_identifier(lhs) or ""
            rhs = low.split("=", 1)[1] if "=" in low else ""
            lhs_reads = extract_reads(lhs, tracked)
            if lhs_base:
                lhs_reads = [x for x in lhs_reads if x != lhs_base]
            consume_reads(state, lhs_reads)
            consume_reads(state, extract_reads(rhs, tracked))
            if lhs_base in tracked and lhs_base not in skip:
                prev_ln = state.get(lhs_base)
                if prev_ln is not None:
                    emit_issue(prev_ln, lhs_base, "assigned value is overwritten before being read")
                state[lhs_base] = ln
            return

        consume_reads(state, extract_reads(low, tracked))

    states: List[Dict[str, int]] = [dict()]
    if_stack: List[Dict[str, object]] = []

    for ln, stmt in unit.body:
        low = stmt.lower().strip()
        if not low:
            continue

        if IF_THEN_RE.match(low):
            for st in states:
                consume_reads(st, extract_reads(low, tracked))
            if_stack.append(
                {
                    "pre": clone_states(states),
                    "branches": [],
                    "has_else": False,
                }
            )
            continue

        if ELSE_IF_RE.match(low):
            if if_stack:
                frame = if_stack[-1]
                frame["branches"].extend(clone_states(states))
                states = clone_states(frame["pre"])  # type: ignore[index]
                for st in states:
                    consume_reads(st, extract_reads(low, tracked))
            else:
                for st in states:
                    consume_reads(st, extract_reads(low, tracked))
            continue

        if ELSE_RE.match(low) and not ELSE_IF_RE.match(low):
            if if_stack:
                frame = if_stack[-1]
                frame["branches"].extend(clone_states(states))
                frame["has_else"] = True
                states = clone_states(frame["pre"])  # type: ignore[index]
            continue

        if END_IF_RE.match(low):
            if if_stack:
                frame = if_stack.pop()
                merged: List[Dict[str, int]] = []
                merged.extend(frame["branches"])  # type: ignore[arg-type]
                merged.extend(clone_states(states))
                if not frame["has_else"]:
                    merged.extend(clone_states(frame["pre"]))  # type: ignore[index]
                states = dedupe_states(merged)
            continue

        for st in states:
            apply_stmt_to_state(st, low, ln)

    for st in states:
        for n, wln in sorted(st.items(), key=lambda x: (x[1], x[0])):
            if n in skip:
                continue
            emit_issue(wln, n, "assigned value is never read before unit end")
    issues.sort(key=lambda x: (x.line, x.name))
    return issues


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


def get_eol(line: str) -> str:
    """Return the line-ending sequence for one source line."""
    if line.endswith("\r\n"):
        return "\r\n"
    if line.endswith("\n"):
        return "\n"
    return ""


def rewrite_decl_remove_names(line: str, remove_names: Set[str]) -> Tuple[Optional[str], bool]:
    """Remove selected names from a declaration line, if present."""
    code, comment = split_code_comment(line.rstrip("\r\n"))
    if "::" not in code:
        return line, False
    lhs, rhs = code.split("::", 1)
    ents = split_top_level_commas(rhs)
    kept: List[str] = []
    changed = False
    for ent in ents:
        m = re.match(r"^\s*([a-z][a-z0-9_]*)", ent, re.IGNORECASE)
        if m and m.group(1).lower() in remove_names:
            changed = True
            continue
        kept.append(ent.strip())
    if not changed:
        return line, False
    eol = get_eol(line)
    if not kept:
        return None, True
    new_line = f"{lhs.rstrip()} :: {', '.join(kept)}{comment}{eol}"
    return new_line, True


def can_remove_assignment(stmt: str, var_name: str) -> bool:
    """Return whether an assignment statement is safe to remove conservatively."""
    low = stmt.strip().lower()
    if ";" in low or "&" in low:
        return False
    m = ASSIGN_RE.match(low)
    if not m:
        return False
    lhs_base = fscan.base_identifier(m.group(1)) or ""
    if lhs_base != var_name:
        return False
    rhs = low.split("=", 1)[1] if "=" in low else ""
    # Keep conservative: do not remove assignments that may contain function calls.
    if re.search(r"\b[a-z][a-z0-9_]*\s*\(", rhs):
        return False
    return True


def build_fix_actions_for_unit(unit: Unit) -> Tuple[Dict[int, Set[str]], Set[int]]:
    """Build declaration-removal and assignment-removal actions for one unit."""
    tracked: Set[str] = set()
    writes: Set[str] = set()
    reads: Set[str] = set()
    write_lines_by_name: Dict[str, Set[int]] = {}
    removable_write_lines_by_name: Dict[str, Set[int]] = {}
    decl_line_by_name: Dict[str, int] = {}
    tracked.update(unit.dummy_names)
    if unit.result_name:
        tracked.add(unit.result_name.lower())

    for ln, stmt in unit.body:
        low = stmt.lower().strip()
        if not low:
            continue
        if TYPE_DECL_RE.match(low):
            decls, _param = parse_decl_entities(low)
            for n, init in decls.items():
                tracked.add(n)
                decl_line_by_name.setdefault(n, ln)
                if init:
                    writes.add(n)
                    write_lines_by_name.setdefault(n, set()).add(ln)
            continue
        m_as = ASSIGN_RE.match(low)
        if m_as:
            lhs = m_as.group(1)
            lhs_base = fscan.base_identifier(lhs) or ""
            rhs = low.split("=", 1)[1] if "=" in low else ""
            lhs_reads = extract_reads(lhs, tracked)
            if lhs_base:
                lhs_reads = [x for x in lhs_reads if x != lhs_base]
            reads.update(lhs_reads)
            reads.update(extract_reads(rhs, tracked))
            if lhs_base in tracked:
                writes.add(lhs_base)
                write_lines_by_name.setdefault(lhs_base, set()).add(ln)
                if can_remove_assignment(low, lhs_base):
                    removable_write_lines_by_name.setdefault(lhs_base, set()).add(ln)
            continue
        reads.update(extract_reads(low, tracked))

    skip = set(unit.dummy_names)
    if unit.result_name:
        skip.add(unit.result_name.lower())
    unused = {n for n in tracked if n not in skip and n in writes and n not in reads}
    if not unused:
        return {}, set()

    fixable_unused: Set[str] = set()
    for n in unused:
        write_lines = write_lines_by_name.get(n, set())
        if not write_lines:
            continue
        removable = removable_write_lines_by_name.get(n, set())
        # Only remove declarations for variables whose every write can be removed safely.
        if write_lines.issubset(removable):
            fixable_unused.add(n)
    if not fixable_unused:
        return {}, set()

    decl_actions: Dict[int, Set[str]] = {}
    remove_assign_lines: Set[int] = set()
    for ln, stmt in unit.body:
        low = stmt.lower().strip()
        if TYPE_DECL_RE.match(low):
            decls, _param = parse_decl_entities(low)
            names_here = set(decls.keys()) & fixable_unused
            if names_here:
                decl_actions.setdefault(ln, set()).update(names_here)
        for n in fixable_unused:
            if can_remove_assignment(low, n):
                remove_assign_lines.add(ln)
    return decl_actions, remove_assign_lines


def apply_fix(
    path: Path,
    decl_actions: Dict[int, Set[str]],
    remove_assign_lines: Set[int],
    backup: bool,
    show_diff: bool,
) -> Tuple[int, Optional[Path]]:
    """Apply conservative removal actions to one file."""
    if not decl_actions and not remove_assign_lines:
        return 0, None
    lines = path.read_text(encoding="utf-8", errors="ignore").splitlines(keepends=True)
    updated: List[str] = []
    changes = 0
    for i, line in enumerate(lines, start=1):
        if i in remove_assign_lines:
            changes += 1
            continue
        if i in decl_actions:
            new_line, changed = rewrite_decl_remove_names(line, decl_actions[i])
            if changed:
                changes += 1
                if new_line is None:
                    continue
                updated.append(new_line)
                continue
        updated.append(line)
    if changes == 0 or updated == lines:
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
    return changes, backup_path


def collect_module_symbols(finfo: fscan.SourceFileInfo) -> List[ModuleSymbol]:
    """Collect private module-scope variables/constants that are initialized."""
    out: List[ModuleSymbol] = []
    current_mod: Optional[str] = None
    in_contains = False
    default_private = False
    explicit_public: Set[str] = set()
    explicit_private: Set[str] = set()

    for lineno, stmt in fscan.iter_fortran_statements(finfo.parsed_lines):
        low = stmt.strip().lower()
        if not low:
            continue
        m_mod = MODULE_START_RE.match(low)
        if m_mod:
            toks = low.split()
            if len(toks) >= 2 and toks[1] != "procedure":
                current_mod = m_mod.group(1).lower()
                in_contains = False
                default_private = False
                explicit_public = set()
                explicit_private = set()
            continue
        if current_mod is None:
            continue
        if low.startswith("end"):
            toks = low.split()
            if len(toks) >= 2 and toks[1] == "module" or len(toks) == 1:
                current_mod = None
            continue
        if CONTAINS_RE.match(low):
            in_contains = True
            continue
        if in_contains:
            continue

        m_acc = ACCESS_RE.match(low)
        if m_acc:
            names = []
            rest = m_acc.group(2).strip()
            if rest.startswith("::"):
                rest = rest[2:].strip()
            elif rest.startswith(","):
                rest = rest[1:].strip()
            if rest:
                names = [x.strip().lower() for x in rest.split(",") if x.strip()]
            if names:
                if m_acc.group(1).lower() == "public":
                    explicit_public.update(names)
                else:
                    explicit_private.update(names)
            else:
                default_private = m_acc.group(1).lower() == "private"
            continue

        if TYPE_DECL_RE.match(low):
            decls, param = parse_decl_entities(low)
            if not decls:
                continue
            for n, init in decls.items():
                if not (init or param):
                    continue
                is_public = (n in explicit_public) or (not default_private and n not in explicit_private)
                if is_public:
                    continue
                out.append(
                    ModuleSymbol(
                        path=finfo.path,
                        module=current_mod,
                        name=n,
                        decl_line=lineno,
                        is_public=is_public,
                        is_parameter=param,
                        initialized=init,
                    )
                )
    return out


def module_internal_ref_counts(finfo: fscan.SourceFileInfo) -> Dict[Tuple[str, str], int]:
    """Count identifier references per module for conservative private-symbol checks."""
    counts: Dict[Tuple[str, str], int] = {}
    current_mod: Optional[str] = None
    for _lineno, stmt in fscan.iter_fortran_statements(finfo.parsed_lines):
        low = stmt.strip().lower()
        if not low:
            continue
        m_mod = MODULE_START_RE.match(low)
        if m_mod:
            toks = low.split()
            if len(toks) >= 2 and toks[1] != "procedure":
                current_mod = m_mod.group(1).lower()
            continue
        if current_mod is None:
            continue
        if low.startswith("end"):
            toks = low.split()
            if len(toks) >= 2 and toks[1] == "module" or len(toks) == 1:
                current_mod = None
            continue
        for m in IDENT_RE.finditer(low):
            n = m.group(1).lower()
            counts[(current_mod, n)] = counts.get((current_mod, n), 0) + 1
    return counts


def main() -> int:
    """Run advisory checks for likely unused set variables/constants."""
    parser = argparse.ArgumentParser(
        description="Advisory checker for likely unused set variables/constants"
    )
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--verbose", action="store_true", help="Print all findings")
    parser.add_argument("--fix", action="store_true", help="Apply conservative removals for some findings")
    parser.add_argument(
        "--warn-dead-store",
        action="store_true",
        help="Also warn about likely dead-store assignments (advisory)",
    )
    parser.add_argument("--backup", dest="backup", action="store_true", default=True)
    parser.add_argument("--no-backup", dest="backup", action="store_false")
    parser.add_argument("--diff", action="store_true")
    args = parser.parse_args()

    files = choose_files(args.fortran_files, args.exclude)
    if not files:
        print("No source files remain after applying --exclude filters.")
        return 2

    infos, any_missing = fscan.load_source_files(files)
    if not infos:
        return 2 if any_missing else 1
    ordered_infos, _ = fscan.order_files_least_dependent(infos)

    issues: List[Issue] = []
    by_path_unit: Dict[Tuple[Path, str, str], Unit] = {}
    for finfo in ordered_infos:
        for unit in collect_units(finfo):
            by_path_unit[(unit.path, unit.kind, unit.name)] = unit
            issues.extend(analyze_unit(unit))
            if args.warn_dead_store:
                issues.extend(analyze_unit_dead_stores(unit))

    for finfo in ordered_infos:
        refs = module_internal_ref_counts(finfo)
        for sym in collect_module_symbols(finfo):
            # One self-reference from declaration is expected; >1 implies usage.
            if refs.get((sym.module, sym.name), 0) <= 1:
                issues.append(
                    Issue(
                        path=sym.path,
                        line=sym.decl_line,
                        category="named-constant" if sym.is_parameter else "variable",
                        context=f"module {sym.module}",
                        name=sym.name,
                        detail="initialized private module symbol appears unused",
                    )
                )

    if not issues:
        if args.warn_dead_store:
            print("No likely unused or dead-store findings.")
        else:
            print("No likely unused set variables/constants found.")
        return 0

    if args.fix:
        per_file_decl_actions: Dict[Path, Dict[int, Set[str]]] = {}
        per_file_remove_assign: Dict[Path, Set[int]] = {}
        for unit in by_path_unit.values():
            d_actions, rem_assign = build_fix_actions_for_unit(unit)
            if d_actions:
                bucket = per_file_decl_actions.setdefault(unit.path, {})
                for ln, names in d_actions.items():
                    bucket.setdefault(ln, set()).update(names)
            if rem_assign:
                per_file_remove_assign.setdefault(unit.path, set()).update(rem_assign)

        total_changes = 0
        for p in sorted({*per_file_decl_actions.keys(), *per_file_remove_assign.keys()}, key=lambda x: x.name.lower()):
            c, _bak = apply_fix(
                p,
                per_file_decl_actions.get(p, {}),
                per_file_remove_assign.get(p, set()),
                backup=args.backup,
                show_diff=args.diff,
            )
            total_changes += c
        print(f"Applied {total_changes} conservative fix edit(s).")

    issues.sort(key=lambda x: (x.path.name.lower(), x.line, x.context, x.name))
    label = "finding(s)"
    if not args.warn_dead_store:
        label = "likely-unused finding(s)"
    print(f"{len(issues)} {label} in {len({i.path.name for i in issues})} file(s).")
    if args.verbose:
        for i in issues:
            print(f"{i.path.name}:{i.line} {i.context} {i.name} [{i.category}] - {i.detail}")
    else:
        by_file: Dict[str, int] = {}
        for i in issues:
            by_file[i.path.name] = by_file.get(i.path.name, 0) + 1
        for fname in sorted(by_file.keys(), key=str.lower):
            print(f"{fname}: {by_file[fname]}")
        first = issues[0]
        print(
            f"\nFirst finding: {first.path.name}:{first.line} {first.context} "
            f"{first.name} [{first.category}] - {first.detail}"
        )
        print("Run with --verbose to list all findings.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
