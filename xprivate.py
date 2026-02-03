#!/usr/bin/env python3
"""Suggest/mark module entities PRIVATE when safe (conservative)."""

from __future__ import annotations

import argparse
import difflib
import re
import shutil
import subprocess
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Set, Tuple

import fortran_build as fbuild
import fortran_scan as fscan


USE_RE = re.compile(
    r"^\s*use\b(?:\s*,\s*(?:non_intrinsic|intrinsic)\s*)?"
    r"(?:\s*::\s*|\s+)([a-z][a-z0-9_]*)(.*)$",
    re.IGNORECASE,
)
MODULE_START_RE = re.compile(r"^\s*module\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
MODULE_END_RE = re.compile(r"^\s*end\s+module\b", re.IGNORECASE)
CONTAINS_RE = re.compile(r"^\s*contains\b", re.IGNORECASE)
PROC_START_RE = re.compile(
    r"^\s*(?:(?:pure|elemental|impure|recursive|module)\s+)*(function|subroutine)\s+([a-z][a-z0-9_]*)\b",
    re.IGNORECASE,
)
PROC_END_RE = re.compile(r"^\s*end(?:\s+(?:function|subroutine))?\b", re.IGNORECASE)
INTERFACE_RE = re.compile(r"^\s*interface\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
ACCESS_STMT_RE = re.compile(r"^\s*(public|private)\b(.*)$", re.IGNORECASE)
TYPE_DECL_RE = re.compile(
    r"^\s*(integer|real|logical|character|complex|type\b|class\b|procedure\b)",
    re.IGNORECASE,
)
DECL_PUBLIC_ATTR_RE = re.compile(r"(?<![a-z])public(?![a-z])", re.IGNORECASE)


@dataclass
class Entity:
    name: str
    kind: str
    decl_public_attr: bool = False


@dataclass
class ModuleInfo:
    name: str
    path: Path
    start: int
    end: int
    contains_line: Optional[int]
    insert_line: int
    default_private: bool
    explicit_public: Set[str] = field(default_factory=set)
    explicit_private: Set[str] = field(default_factory=set)
    entities: Dict[str, Entity] = field(default_factory=dict)

    def is_public_now(self, name: str) -> bool:
        n = name.lower()
        if n in self.explicit_private:
            return False
        if n in self.explicit_public:
            return True
        return not self.default_private


@dataclass
class Candidate:
    path: Path
    module: str
    entity: str
    kind: str


def split_code_comment(line: str) -> Tuple[str, str]:
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
    if line.endswith("\r\n"):
        return "\r\n"
    if line.endswith("\n"):
        return "\n"
    return ""


def split_top_level_commas(text: str) -> List[str]:
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


def parse_access_stmt_names(code: str, keyword: str) -> Optional[List[str]]:
    m = ACCESS_STMT_RE.match(code.strip())
    if not m or m.group(1).lower() != keyword.lower():
        return None
    rest = m.group(2).strip()
    if not rest:
        return []
    if rest.startswith("::"):
        rest = rest[2:].strip()
    elif rest.startswith(","):
        rest = rest[1:].strip()
    if not rest:
        return []
    names: List[str] = []
    for tok in split_top_level_commas(rest):
        t = tok.strip()
        if "=>" in t:
            t = t.split("=>", 1)[1].strip()
        m_name = re.match(r"^([a-z][a-z0-9_]*)$", t, re.IGNORECASE)
        if m_name:
            names.append(m_name.group(1).lower())
    return names


def parse_use_only_names(rest: str) -> Optional[Set[str]]:
    m = re.search(r"\bonly\s*:\s*(.*)$", rest, re.IGNORECASE)
    if not m:
        return None
    rhs = m.group(1).strip()
    if not rhs:
        return set()
    out: Set[str] = set()
    for tok in split_top_level_commas(rhs):
        t = tok.strip()
        if not t:
            continue
        if "=>" in t:
            t = t.split("=>", 1)[1].strip()
        if t.lower().startswith(("operator(", "assignment(")):
            continue
        m_name = re.match(r"^([a-z][a-z0-9_]*)$", t, re.IGNORECASE)
        if m_name:
            out.add(m_name.group(1).lower())
    return out


def parse_module_block(path: Path, lines: List[str], start_idx: int) -> Tuple[ModuleInfo, int]:
    start_line = start_idx + 1
    m_start = MODULE_START_RE.match(fscan.strip_comment(lines[start_idx]).strip())
    assert m_start is not None
    mod_name = m_start.group(1).lower()

    i = start_idx + 1
    contains_line: Optional[int] = None
    default_private = False
    explicit_public: Set[str] = set()
    explicit_private: Set[str] = set()
    entities: Dict[str, Entity] = {}

    in_contains = False
    proc_depth = 0
    while i < len(lines):
        code = fscan.strip_comment(lines[i]).rstrip("\r\n")
        low = code.strip().lower()

        if MODULE_END_RE.match(low):
            break

        if not in_contains:
            if CONTAINS_RE.match(low):
                in_contains = True
                contains_line = i + 1
                i += 1
                continue

            names_public = parse_access_stmt_names(code, "public")
            if names_public is not None:
                if names_public:
                    explicit_public.update(names_public)
                    for n in names_public:
                        entities.setdefault(n, Entity(n, "unknown"))
                else:
                    default_private = False

            names_private = parse_access_stmt_names(code, "private")
            if names_private is not None:
                if names_private:
                    explicit_private.update(names_private)
                    for n in names_private:
                        entities.setdefault(n, Entity(n, "unknown"))
                else:
                    default_private = True

            m_iface = INTERFACE_RE.match(low)
            if m_iface:
                n = m_iface.group(1).lower()
                entities.setdefault(n, Entity(n, "interface"))

            if TYPE_DECL_RE.match(low) and "::" in low:
                lhs, _rhs = low.split("::", 1)
                declared = fscan.parse_declared_names_from_decl(low)
                has_public_attr = DECL_PUBLIC_ATTR_RE.search(lhs) is not None
                for n in declared:
                    ent = entities.setdefault(n, Entity(n, "entity"))
                    if has_public_attr:
                        ent.decl_public_attr = True
                        explicit_public.add(n)
        else:
            m_proc_start = PROC_START_RE.match(low)
            if m_proc_start:
                proc_depth += 1
                if proc_depth == 1:
                    n = m_proc_start.group(2).lower()
                    entities.setdefault(n, Entity(n, m_proc_start.group(1).lower()))
            elif proc_depth > 0 and PROC_END_RE.match(low):
                proc_depth -= 1

        i += 1

    end_line = i + 1 if i < len(lines) else len(lines)
    insert_line = (contains_line or end_line) - 1
    info = ModuleInfo(
        name=mod_name,
        path=path,
        start=start_line,
        end=end_line,
        contains_line=contains_line,
        insert_line=insert_line,
        default_private=default_private,
        explicit_public=explicit_public,
        explicit_private=explicit_private,
        entities=entities,
    )
    return info, i


def parse_modules_in_file(path: Path, lines: List[str]) -> List[ModuleInfo]:
    modules: List[ModuleInfo] = []
    i = 0
    while i < len(lines):
        code = fscan.strip_comment(lines[i]).strip().lower()
        m_start = MODULE_START_RE.match(code)
        if m_start:
            toks = code.split()
            if len(toks) >= 2 and toks[1] != "procedure":
                mod, end_idx = parse_module_block(path, lines, i)
                modules.append(mod)
                i = end_idx + 1
                continue
        i += 1
    return modules


def parse_external_uses(paths: Iterable[Path]) -> Tuple[Dict[str, Set[str]], Set[str]]:
    only_uses: Dict[str, Set[str]] = {}
    wildcard_uses: Set[str] = set()
    for p in paths:
        text = p.read_text(encoding="utf-8", errors="ignore")
        for raw in text.splitlines():
            code = fscan.strip_comment(raw).strip()
            m = USE_RE.match(code)
            if not m:
                continue
            mod = m.group(1).lower()
            only_names = parse_use_only_names(m.group(2) or "")
            if only_names is None:
                wildcard_uses.add(mod)
            else:
                only_uses.setdefault(mod, set()).update(only_names)
    return only_uses, wildcard_uses


def choose_files(args_files: List[Path], exclude: List[str]) -> List[Path]:
    if args_files:
        files = args_files
    else:
        files = sorted(
            set(Path(".").glob("*.f90")) | set(Path(".").glob("*.F90")),
            key=lambda p: p.name.lower(),
        )
    return fscan.apply_excludes(files, exclude)


def collect_candidates(
    modules_by_file: Dict[Path, List[ModuleInfo]],
    only_uses: Dict[str, Set[str]],
    wildcard_uses: Set[str],
    conservative_suggest: bool,
) -> List[Candidate]:
    out: List[Candidate] = []
    for path, mods in modules_by_file.items():
        for mod in mods:
            if conservative_suggest and mod.name in wildcard_uses:
                continue
            needed = only_uses.get(mod.name, set())
            for n, ent in mod.entities.items():
                if ent.decl_public_attr:
                    continue
                if not mod.is_public_now(n):
                    continue
                if conservative_suggest and n in needed:
                    continue
                out.append(Candidate(path=path, module=mod.name, entity=n, kind=ent.kind))
    out.sort(key=lambda c: (c.path.name.lower(), c.module, c.entity))
    return out


def remove_name_from_access_line(line: str, keyword: str, target: str) -> Tuple[str, bool]:
    eol = get_eol(line)
    code, comment = split_code_comment(line.rstrip("\r\n"))
    names = parse_access_stmt_names(code, keyword)
    if names is None or not names or target.lower() not in names:
        return line, False
    kept = [n for n in names if n != target.lower()]
    indent = re.match(r"^\s*", code).group(0)
    if kept:
        new_code = f"{indent}{keyword} :: {', '.join(kept)}"
    else:
        new_code = ""
    out = new_code + (comment if new_code else "")
    if eol:
        out += eol
    return out, True


def apply_private_change(path: Path, mod_name: str, entity: str, show_diff: bool) -> bool:
    text = path.read_text(encoding="utf-8", errors="ignore")
    lines = text.splitlines(keepends=True)
    modules = parse_modules_in_file(path, lines)
    target_mod = None
    for mod in modules:
        if mod.name == mod_name:
            target_mod = mod
            break
    if target_mod is None:
        return False
    if entity.lower() in target_mod.explicit_private:
        return False

    updated = lines[:]
    changed = False
    # Remove from explicit PUBLIC lists first, to avoid conflicting accessibility.
    for i in range(target_mod.start - 1, (target_mod.contains_line or target_mod.end) - 1):
        new_line, did = remove_name_from_access_line(updated[i], "public", entity)
        if did:
            updated[i] = new_line
            changed = True

    indent = "  "
    stmt = f"{indent}private :: {entity}\n"
    insert_idx = target_mod.insert_line - 1
    updated.insert(insert_idx, stmt)
    changed = True

    if not changed:
        return False

    if show_diff:
        diff = difflib.unified_diff(
            lines,
            updated,
            fromfile=str(path),
            tofile=str(path),
            lineterm="",
        )
        print("\nProposed diff:")
        for d in diff:
            print(d)

    with path.open("w", encoding="utf-8", newline="") as f:
        f.write("".join(updated))
    return True


def build_compile_cmd(command: str, files: List[Path]) -> Tuple[str, str]:
    file_args = " ".join(fbuild.quote_cmd_arg(str(p)) for p in files)
    file_args_display = " ".join(fbuild.quote_cmd_arg(fscan.display_path(p)) for p in files)
    if "{files}" in command:
        return command.replace("{files}", file_args), command.replace("{files}", file_args_display)
    return f"{command} {file_args}".strip(), f"{command} {file_args_display}".strip()


def compile_check(command: str, files: List[Path], phase: str, announce: bool = True) -> bool:
    cmd, cmd_display = build_compile_cmd(command, files)
    if announce:
        print(f"Compile ({phase}): {cmd_display}")
    proc = subprocess.run(cmd, shell=True, capture_output=True, text=True)
    if announce:
        if proc.returncode == 0:
            print(f"Compile ({phase}): PASS")
        else:
            print(f"Compile ({phase}): FAIL (exit {proc.returncode})")
            if proc.stdout:
                print(proc.stdout.rstrip())
            if proc.stderr:
                print(proc.stderr.rstrip())
    return proc.returncode == 0


def print_summary(summary: Dict[Tuple[str, str], List[str]], intro: str) -> None:
    if not summary:
        return
    n_items = sum(len(v) for v in summary.values())
    n_files = len({k[0] for k in summary.keys()})
    item_word = "entity" if n_items == 1 else "entities"
    file_word = "source file" if n_files == 1 else "source files"
    print(f"\n{intro} {n_items} {item_word} in {n_files} {file_word}:")
    for (fname, mod_name), names in sorted(summary.items(), key=lambda x: (x[0][0].lower(), x[0][1])):
        if not names:
            continue
        print(f"{fname} module {mod_name} {len(names)}: {' '.join(names)}")


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Suggest/mark module entities PRIVATE (conservative; compile-validated in --fix mode)"
    )
    parser.add_argument("fortran_files", type=Path, nargs="*", help="Source files (default: *.f90/*.F90 in cwd)")
    parser.add_argument(
        "--exclude",
        action="append",
        default=[],
        help="Glob pattern to exclude files (can be repeated)",
    )
    parser.add_argument("--fix", action="store_true", help="Apply changes")
    parser.add_argument("--iterate", action="store_true", help="Repeat passes until no more accepted changes")
    parser.add_argument("--max-iter", type=int, default=10, help="Max passes with --iterate (default: 10)")
    parser.add_argument("--compiler", type=str, help='Compilation command, e.g. "gfortran -o app.exe"')
    parser.add_argument("--backup", dest="backup", action="store_true", default=True)
    parser.add_argument("--no-backup", dest="backup", action="store_false")
    parser.add_argument("--diff", action="store_true")
    parser.add_argument("--verbose", action="store_true")
    parser.add_argument("--git", action="store_true", help="Commit changed files to git after successful run")
    args = parser.parse_args()

    files = choose_files(args.fortran_files, args.exclude)
    if not files:
        print("No source files remain after applying --exclude filters.")
        return 2

    infos, any_missing = fscan.load_source_files(files)
    if not infos:
        return 2 if any_missing else 1
    ordered_infos, _ = fscan.order_files_least_dependent(infos)
    ordered_files = [f.path for f in ordered_infos]
    print("Processing order:", " ".join(fscan.display_path(p) for p in ordered_files))

    compile_files = ordered_files
    if args.compiler:
        compile_files, _ = fscan.build_compile_closure(ordered_infos)

    if not args.fix:
        modules_by_file: Dict[Path, List[ModuleInfo]] = {}
        for p in ordered_files:
            text = p.read_text(encoding="utf-8", errors="ignore")
            modules_by_file[p] = parse_modules_in_file(p, text.splitlines(keepends=True))
        only_uses, wildcard_uses = parse_external_uses(ordered_files)
        cands = collect_candidates(modules_by_file, only_uses, wildcard_uses, conservative_suggest=True)
        summary: Dict[Tuple[str, str], List[str]] = {}
        for c in cands:
            key = (fscan.display_path(c.path), c.module)
            summary.setdefault(key, []).append(c.entity)
        print_summary(summary, "Summary of")
        return 0

    if not args.compiler:
        print("--fix requires --compiler so accepted changes are compile-validated.")
        return 2

    if not compile_check(args.compiler, compile_files, "baseline", announce=True):
        return 1

    backup_pairs: Dict[Path, Path] = {}
    changed_files: Set[Path] = set()
    accepted_summary: Dict[Tuple[str, str], List[str]] = {}

    max_passes = max(1, args.max_iter if args.iterate else 1)
    for iteration in range(1, max_passes + 1):
        if args.iterate and iteration > 1:
            print(f"\nIteration {iteration}...")

        modules_by_file = {}
        for p in ordered_files:
            text = p.read_text(encoding="utf-8", errors="ignore")
            modules_by_file[p] = parse_modules_in_file(p, text.splitlines(keepends=True))
        only_uses, wildcard_uses = parse_external_uses(ordered_files)
        candidates = collect_candidates(modules_by_file, only_uses, wildcard_uses, conservative_suggest=False)

        accepted_this_pass = 0
        for cand in candidates:
            before = cand.path.read_text(encoding="utf-8", errors="ignore")

            changed = apply_private_change(cand.path, cand.module, cand.entity, show_diff=args.diff)
            if not changed:
                continue
            if args.backup and cand.path not in backup_pairs:
                bak = cand.path.with_name(cand.path.name + ".bak")
                bak.write_text(before, encoding="utf-8", newline="")
                backup_pairs[cand.path] = bak
                print(f"Backup written: {fscan.display_path(bak)}")
            if not compile_check(args.compiler, compile_files, phase="trial", announce=False):
                with cand.path.open("w", encoding="utf-8", newline="") as f:
                    f.write(before)
                if args.verbose:
                    print(
                        f"Kept PUBLIC (compile needed): {fscan.display_path(cand.path)} "
                        f"module {cand.module} :: {cand.entity}"
                    )
                continue

            changed_files.add(cand.path)
            key = (fscan.display_path(cand.path), cand.module)
            accepted_summary.setdefault(key, []).append(cand.entity)
            accepted_this_pass += 1
            if args.verbose:
                print(
                    f"Marked PRIVATE: {fscan.display_path(cand.path)} module {cand.module} :: {cand.entity}"
                )

        if accepted_this_pass == 0:
            break

    ok = compile_check(args.compiler, compile_files, "after-fix", announce=True)
    if not ok:
        fbuild.rollback_backups([(p, b) for p, b in backup_pairs.items()], fscan.display_path)
        return 1

    if args.git and changed_files:
        n = sum(len(v) for v in accepted_summary.values())
        nfiles = len({k[0] for k in accepted_summary.keys()})
        msg = f"xprivate: mark {n} module entities private in {nfiles} file(s)"
        fbuild.git_commit_files(sorted(changed_files, key=lambda p: str(p).lower()), msg, fscan.display_path)

    print_summary(accepted_summary, "Summary of")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
