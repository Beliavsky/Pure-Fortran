#!/usr/bin/env python3
"""Suggest/apply USE, ONLY lists for broad USE imports."""

from __future__ import annotations

import argparse
import difflib
import re
import shutil
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Set, Tuple

import cli_paths as cpaths
import fortran_build as fbuild
import fortran_scan as fscan

USE_STMT_FULL_RE = re.compile(
    r"^(?P<prefix>\s*use\b(?:\s*,\s*(?:non_intrinsic|intrinsic)\s*)?(?:\s*::\s*|\s+))"
    r"(?P<module>[a-z][a-z0-9_]*)\s*$",
    re.IGNORECASE,
)
USE_RE = re.compile(
    r"^\s*use\b(?:\s*,\s*(?:non_intrinsic|intrinsic)\s*)?(?:\s*::\s*|\s+)([a-z][a-z0-9_]*)(.*)$",
    re.IGNORECASE,
)
USE_ONLY_LINE_RE = re.compile(
    r"^(?P<prefix>\s*use\b(?:\s*,\s*(?:non_intrinsic|intrinsic)\s*)?(?:\s*::\s*|\s+))"
    r"(?P<module>[a-z][a-z0-9_]*)\s*,\s*only\s*:\s*(?P<tail>.+)$",
    re.IGNORECASE,
)
MODULE_START_RE = re.compile(r"^\s*module\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
MODULE_END_RE = re.compile(r"^\s*end\s+module\b", re.IGNORECASE)
CONTAINS_RE = re.compile(r"^\s*contains\b", re.IGNORECASE)
PROC_START_RE = re.compile(
    r"^\s*(?:(?:pure|elemental|impure|recursive|module)\s+)*(function|subroutine)\s+([a-z][a-z0-9_]*)\b",
    re.IGNORECASE,
)
UNIT_START_RE = re.compile(
    r"^\s*(?!end\b)(?:(?:pure|elemental|impure|recursive|module)\s+)*(?:program|module|subroutine|function)\b",
    re.IGNORECASE,
)
UNIT_END_RE = re.compile(r"^\s*end\s+(?:program|module|subroutine|function)\b", re.IGNORECASE)
INTERFACE_RE = re.compile(r"^\s*interface\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
ACCESS_RE = re.compile(r"^\s*(public|private)\b(.*)$", re.IGNORECASE)
IDENT_RE = re.compile(r"\b([a-z][a-z0-9_]*)\b", re.IGNORECASE)
TYPE_DECL_RE = re.compile(
    r"^\s*(integer|real|logical|character|complex|type\b|class\b|procedure\b)",
    re.IGNORECASE,
)
FORTRAN_KEYWORDS = {
    "if",
    "then",
    "else",
    "elseif",
    "end",
    "do",
    "select",
    "case",
    "where",
    "forall",
    "function",
    "subroutine",
    "module",
    "program",
    "contains",
    "call",
    "use",
    "only",
    "result",
    "implicit",
    "none",
    "integer",
    "real",
    "logical",
    "character",
    "complex",
    "type",
    "class",
    "public",
    "private",
    "interface",
    "procedure",
    "allocate",
    "deallocate",
    "return",
    "stop",
    "error",
    "print",
    "read",
    "write",
    "open",
    "close",
    "rewind",
    "backspace",
    "flush",
    "inquire",
    "intent",
    "in",
    "out",
    "inout",
    "value",
    "optional",
    "allocatable",
    "pointer",
    "parameter",
    "save",
    "target",
    "pure",
    "elemental",
    "recursive",
}


@dataclass
class ModuleExports:
    """Store conservative export information for one module."""

    name: str
    default_private: bool = False
    explicit_public: Set[str] = field(default_factory=set)
    explicit_private: Set[str] = field(default_factory=set)
    symbols: Set[str] = field(default_factory=set)

    def exported(self) -> Set[str]:
        """Return currently exported symbol names."""
        if self.default_private:
            return set(self.explicit_public)
        return set(self.symbols) - set(self.explicit_private) | set(self.explicit_public)


def parse_access_names(rest: str) -> Optional[List[str]]:
    """Parse PUBLIC/PRIVATE name list text and return names."""
    r = rest.strip()
    if not r:
        return []
    if r.startswith("::"):
        r = r[2:].strip()
    elif r.startswith(","):
        r = r[1:].strip()
    if not r:
        return []
    out: List[str] = []
    for chunk in r.split(","):
        t = chunk.strip()
        if not t:
            continue
        if "=>" in t:
            t = t.split("=>", 1)[1].strip()
        m = re.match(r"^([a-z][a-z0-9_]*)$", t, re.IGNORECASE)
        if m:
            out.append(m.group(1).lower())
    return out


def split_top_level_commas(text: str) -> List[str]:
    """Split text by top-level commas only."""
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


def choose_files(args_files: List[Path], exclude: Iterable[str]) -> List[Path]:
    """Resolve input file paths from CLI args or current directory defaults."""
    if args_files:
        files = cpaths.expand_path_args(args_files)
    else:
        files = sorted(
            set(Path(".").glob("*.f90")) | set(Path(".").glob("*.F90")),
            key=lambda p: p.name.lower(),
        )
    return fscan.apply_excludes(files, exclude)


def collect_module_exports(infos: List[fscan.SourceFileInfo]) -> Dict[str, ModuleExports]:
    """Collect conservative module export sets across all source files."""
    out: Dict[str, ModuleExports] = {}

    for finfo in infos:
        lines = finfo.parsed_lines
        current: Optional[ModuleExports] = None
        in_contains = False
        proc_depth = 0
        for _lineno, stmt in fscan.iter_fortran_statements(lines):
            low = stmt.strip().lower()
            if not low:
                continue
            m_mod = MODULE_START_RE.match(low)
            if m_mod:
                toks = low.split()
                if len(toks) >= 2 and toks[1] != "procedure":
                    current = out.setdefault(m_mod.group(1).lower(), ModuleExports(m_mod.group(1).lower()))
                    in_contains = False
                    proc_depth = 0
                continue
            if current is None:
                continue
            if MODULE_END_RE.match(low):
                current = None
                in_contains = False
                proc_depth = 0
                continue
            if CONTAINS_RE.match(low):
                in_contains = True
                continue

            m_acc = ACCESS_RE.match(low)
            if m_acc:
                names = parse_access_names(m_acc.group(2))
                if names is not None:
                    if names:
                        if m_acc.group(1).lower() == "public":
                            current.explicit_public.update(names)
                        else:
                            current.explicit_private.update(names)
                    else:
                        current.default_private = m_acc.group(1).lower() == "private"
                continue

            m_iface = INTERFACE_RE.match(low)
            if m_iface:
                current.symbols.add(m_iface.group(1).lower())

            if not in_contains and TYPE_DECL_RE.match(low) and "::" in low:
                current.symbols.update(fscan.parse_declared_names_from_decl(low))
                continue

            if in_contains:
                m_proc = PROC_START_RE.match(low)
                if m_proc:
                    proc_depth += 1
                    if proc_depth == 1:
                        current.symbols.add(m_proc.group(2).lower())
                    continue
                if low.startswith("end") and proc_depth > 0:
                    toks = low.split()
                    if len(toks) == 1 or (len(toks) > 1 and toks[1] in {"function", "subroutine"}):
                        proc_depth -= 1
    return out


def used_names_in_file(finfo: fscan.SourceFileInfo, candidates: Set[str]) -> List[str]:
    """Return candidate names referenced in a file, preserving first-seen order."""
    seen: Set[str] = set()
    ordered: List[str] = []
    for _lineno, stmt in fscan.iter_fortran_statements(finfo.parsed_lines):
        low = stmt.lower()
        for m in IDENT_RE.finditer(low):
            n = m.group(1).lower()
            if n in FORTRAN_KEYWORDS or n not in candidates or n in seen:
                continue
            seen.add(n)
            ordered.append(n)
    return ordered


def used_names_in_file_excluding_use(finfo: fscan.SourceFileInfo, candidates: Set[str]) -> List[str]:
    """Return candidate names referenced outside USE statements, first-seen order."""
    seen: Set[str] = set()
    ordered: List[str] = []
    for _lineno, stmt in fscan.iter_fortran_statements(finfo.parsed_lines):
        low = stmt.lower().strip()
        if low.startswith("use "):
            continue
        for m in IDENT_RE.finditer(low):
            n = m.group(1).lower()
            if n in FORTRAN_KEYWORDS or n not in candidates or n in seen:
                continue
            seen.add(n)
            ordered.append(n)
    return ordered


def used_names_in_unit_excluding_use(
    finfo: fscan.SourceFileInfo, candidates: Set[str], target_line: int
) -> List[str]:
    """Return candidate names referenced in the same program unit as target_line."""
    # Find enclosing unit bounds for the target statement line.
    stack: List[Tuple[int, int]] = []  # (start_line, depth id)
    unit_bounds: List[Tuple[int, int]] = []
    depth = 0
    stmts = list(fscan.iter_fortran_statements(finfo.parsed_lines))
    for lineno, stmt in stmts:
        s = stmt.strip().lower()
        if UNIT_START_RE.match(s):
            depth += 1
            stack.append((lineno, depth))
            continue
        if UNIT_END_RE.match(s) and stack:
            start, _d = stack.pop()
            unit_bounds.append((start, lineno))
            continue
    lo, hi = 1, 10**9
    for a, b in unit_bounds:
        if a <= target_line <= b:
            lo, hi = a, b
            break

    seen: Set[str] = set()
    ordered: List[str] = []
    for lineno, stmt in stmts:
        if lineno < lo or lineno > hi:
            continue
        low = stmt.lower().strip()
        if low.startswith("use "):
            continue
        for m in IDENT_RE.finditer(low):
            n = m.group(1).lower()
            if n in FORTRAN_KEYWORDS or n not in candidates or n in seen:
                continue
            seen.add(n)
            ordered.append(n)
    return ordered


def parse_use_only_imports(code: str) -> Optional[Tuple[str, List[Tuple[str, str]]]]:
    """Parse one single-line USE, ONLY statement into module and imported locals.

    Returns (module, [(raw_token, local_name), ...]).
    """
    if "&" in code:
        return None
    m = USE_ONLY_LINE_RE.match(code)
    if not m:
        return None
    mod = m.group("module").lower()
    tail = m.group("tail").strip()
    if not tail:
        return None
    parts = split_top_level_commas(tail)
    imports: List[Tuple[str, str]] = []
    for p in parts:
        t = p.strip()
        if not t:
            continue
        if "=>" in t:
            local = t.split("=>", 1)[0].strip()
        else:
            local = t
        ml = re.match(r"^([a-z][a-z0-9_]*)$", local, re.IGNORECASE)
        if not ml:
            return None
        imports.append((t, ml.group(1).lower()))
    if not imports:
        return None
    return mod, imports


def rewrite_use_line(line: str, only_names: List[str]) -> Optional[str]:
    """Rewrite one broad USE line to USE, ONLY form."""
    code, comment = split_code_comment(line.rstrip("\r\n"))
    if "&" in code:
        return None
    m = USE_STMT_FULL_RE.match(code)
    if not m:
        return None
    eol = get_eol(line)
    return f"{m.group('prefix')}{m.group('module')}, only: {', '.join(only_names)}{comment}{eol}"


def rewrite_use_only_line(line: str, kept_raw: List[str]) -> Optional[str]:
    """Rewrite one USE, ONLY line keeping only selected raw import tokens.

    Returns None when no names remain (line can be deleted).
    """
    code, comment = split_code_comment(line.rstrip("\r\n"))
    if "&" in code:
        return None
    m = USE_ONLY_LINE_RE.match(code)
    if not m:
        return None
    if not kept_raw:
        return None
    eol = get_eol(line)
    return f"{m.group('prefix')}{m.group('module')}, only: {', '.join(kept_raw)}{comment}{eol}"


def split_code_comment(line: str) -> Tuple[str, str]:
    """Split one line into code and trailing comment."""
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
    """Return line-ending sequence from a source line."""
    if line.endswith("\r\n"):
        return "\r\n"
    if line.endswith("\n"):
        return "\n"
    return ""


def apply_fix_for_file(
    finfo: fscan.SourceFileInfo,
    module_exports: Dict[str, ModuleExports],
    backup: bool,
    show_diff: bool,
    out_path: Optional[Path] = None,
) -> Tuple[int, Optional[Path], Dict[str, List[str]]]:
    """Apply USE, ONLY rewrites for one file and return changed module-name mapping."""
    lines = finfo.lines[:]
    changed = 0
    changes: Dict[str, List[str]] = {}
    pruned: Dict[str, List[str]] = {}

    for idx, line in enumerate(lines):
        code, _comment = split_code_comment(line.rstrip("\r\n"))
        m = USE_RE.match(code)
        if not m:
            continue
        mod = m.group(1).lower()
        rest = (m.group(2) or "").lower()
        if "only" in rest:
            continue
        exp = module_exports.get(mod)
        if exp is None:
            continue
        candidate_names = exp.exported()
        if not candidate_names:
            continue
        used = used_names_in_file(finfo, candidate_names)
        if not used:
            continue
        new_line = rewrite_use_line(line, used)
        if new_line is None or new_line == line:
            continue
        lines[idx] = new_line
        changed += 1
        changes[mod] = used

    # Prune unused imports in existing USE, ONLY lines.
    for idx, line in enumerate(lines):
        code, _comment = split_code_comment(line.rstrip("\r\n"))
        parsed = parse_use_only_imports(code)
        if parsed is None:
            continue
        mod, imports = parsed
        locals_set = {loc for _raw, loc in imports}
        used_locals = set(used_names_in_unit_excluding_use(finfo, locals_set, idx + 1))
        if len(used_locals) == len(locals_set):
            continue
        kept_raw = [raw for raw, loc in imports if loc in used_locals]
        removed = [raw for raw, loc in imports if loc not in used_locals]
        new_line = rewrite_use_only_line(line, kept_raw)
        if new_line is None:
            lines[idx] = ""
        else:
            lines[idx] = new_line
        changed += 1
        pruned.setdefault(mod, []).extend(removed)

    if changed == 0:
        return 0, None, {}

    if show_diff:
        diff_to = str(out_path) if out_path is not None else str(finfo.path)
        diff = difflib.unified_diff(
            finfo.lines,
            lines,
            fromfile=str(finfo.path),
            tofile=diff_to,
            lineterm="",
        )
        print("\nProposed diff:")
        for d in diff:
            print(d)

    backup_path: Optional[Path] = None
    if backup and out_path is None:
        backup_path = finfo.path.with_name(finfo.path.name + ".bak")
        shutil.copy2(finfo.path, backup_path)
        print(f"Backup written: {backup_path.name}")

    target = out_path if out_path is not None else finfo.path
    target.write_text("".join(lines), encoding="utf-8", newline="")
    for mod, removed in pruned.items():
        if removed:
            changes.setdefault(mod, [])
            changes[mod].extend([f"-{r}" for r in removed])
    return changed, backup_path, changes


def main() -> int:
    """Run USE, ONLY suggestion or rewrite workflow."""
    parser = argparse.ArgumentParser(description="Suggest/apply USE, ONLY lists for broad USE imports")
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--fix", action="store_true", help="Rewrite USE statements to USE, ONLY")
    parser.add_argument("--out", type=Path, help="With --fix, write transformed output to this file (single input)")
    parser.add_argument("--backup", dest="backup", action="store_true", default=True)
    parser.add_argument("--no-backup", dest="backup", action="store_false")
    parser.add_argument("--diff", action="store_true")
    parser.add_argument("--compiler", type=str, help="Compilation command for fix validation")
    args = parser.parse_args()
    if args.out is not None:
        args.fix = True

    files = choose_files(args.fortran_files, args.exclude)
    if not files:
        print("No source files remain after applying --exclude filters.")
        return 2
    if args.out is not None and len(files) != 1:
        print("--out requires exactly one input source file.")
        return 2

    infos, any_missing = fscan.load_source_files(files)
    if not infos:
        return 2 if any_missing else 1
    ordered_infos, _ = fscan.order_files_least_dependent(infos)
    if len(ordered_infos) > 1:
        print("Processing order:", " ".join(f.path.name for f in ordered_infos))

    module_exports = collect_module_exports(ordered_infos)

    if not args.fix:
        total = 0
        for finfo in ordered_infos:
            for line in finfo.parsed_lines:
                m = USE_RE.match(line)
                if not m:
                    continue
                mod = m.group(1).lower()
                rest = (m.group(2) or "").lower()
                if "only" in rest:
                    continue
                exp = module_exports.get(mod)
                if exp is None:
                    continue
                used = used_names_in_file(finfo, exp.exported())
                if not used:
                    continue
                total += 1
                print(f"{finfo.path.name}: use {mod} -> only: {', '.join(used)}")
        print(f"\n{total} USE statement(s) can be converted to USE, ONLY.")
        return 0

    compile_paths = [f.path for f in ordered_infos]
    after_compile_paths = compile_paths
    if args.compiler:
        compile_paths, _ = fscan.build_compile_closure(ordered_infos)
        after_compile_paths = compile_paths
        if args.out is not None and args.fix:
            after_compile_paths = [args.out]

    if args.compiler and not fbuild.run_compiler_command(args.compiler, compile_paths, "baseline", fscan.display_path):
        return 5

    changed_total = 0
    backup_pairs: List[Tuple[Path, Path]] = []
    summary: Dict[str, Dict[str, List[str]]] = {}
    for finfo in ordered_infos:
        out_path = args.out if args.out is not None else None
        changed, backup_path, changes = apply_fix_for_file(
            finfo,
            module_exports=module_exports,
            backup=args.backup,
            show_diff=args.diff,
            out_path=out_path,
        )
        changed_total += changed
        if backup_path:
            backup_pairs.append((finfo.path, backup_path))
        if changes:
            summary[finfo.path.name] = changes

    if args.compiler and not fbuild.run_compiler_command(
        args.compiler,
        after_compile_paths,
        "after-fix",
        fscan.display_path,
    ):
        fbuild.rollback_backups(backup_pairs, fscan.display_path)
        return 5

    print(f"\nConverted {changed_total} USE statement(s) to USE, ONLY.")
    for fname in sorted(summary.keys(), key=str.lower):
        for mod, names in summary[fname].items():
            print(f"{fname}: use {mod}, only: {', '.join(names)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
