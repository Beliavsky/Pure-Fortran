#!/usr/bin/env python3
"""Suggest/mark missing INTENT(IN) for Fortran dummy arguments (conservative)."""

from __future__ import annotations

import argparse
import difflib
import fortran_build as fbuild
import re
import shutil
import subprocess
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Optional, Set, Tuple

import fortran_scan as fscan

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
ALLOC_DEALLOC_RE = re.compile(r"^\s*(allocate|deallocate)\s*\((.+)\)\s*$", re.IGNORECASE)
CALL_RE = re.compile(r"\bcall\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)


@dataclass
class IntentSuggestion:
    filename: str
    proc_kind: str
    proc_name: str
    proc_start: int
    dummy: str
    decl_line: int
    fixable: bool
    intent: str


def add_summary_item(summary: Dict[Tuple[str, str], List[str]], s: IntentSuggestion) -> None:
    """Add a deduplicated procedure:dummy entry to a summary bucket."""
    key = (s.filename, s.proc_kind)
    token = f"{s.proc_name}:{s.dummy}"
    bucket = summary.setdefault(key, [])
    if token.lower() not in {x.lower() for x in bucket}:
        bucket.append(token)


def print_summary(summary: Dict[Tuple[str, str], List[str]], label: str) -> None:
    """Print grouped function/subroutine intent summary lines."""
    if not summary:
        return
    n_functions = sum(len(v) for (fname, kind), v in summary.items() if kind == "function")
    n_subroutines = sum(len(v) for (fname, kind), v in summary.items() if kind == "subroutine")
    nfiles = len({fname for (fname, _k) in summary.keys()})
    fun_word = "function" if n_functions == 1 else "functions"
    sub_word = "subroutine" if n_subroutines == 1 else "subroutines"
    file_word = "source file" if nfiles == 1 else "source files"
    print(
        f"\nSummary of {n_functions} {fun_word} and {n_subroutines} {sub_word} "
        f"{label} in {nfiles} {file_word}:"
    )
    for (fname, kind) in sorted(summary.keys(), key=lambda x: (x[0].lower(), x[1])):
        items = summary[(fname, kind)]
        if not items:
            continue
        label_kind = kind if len(items) == 1 else f"{kind}s"
        print(f"{fname} {len(items)} {label_kind}: {' '.join(items)}")


def maybe_git_commit(
    do_git: bool,
    changed_files: Set[Path],
    changed_summary: Dict[Tuple[str, str], List[str]],
    intent_label: str,
) -> None:
    """Create a git commit for changed files when --git is enabled."""
    if not do_git or not changed_files:
        return
    n_functions = sum(len(v) for (fname, kind), v in changed_summary.items() if kind == "function")
    n_subroutines = sum(len(v) for (fname, kind), v in changed_summary.items() if kind == "subroutine")
    n_files = len({fname for (fname, _k) in changed_summary.keys()})
    msg = (
        f"intent: mark {intent_label} on {n_functions} function arg(s) and "
        f"{n_subroutines} subroutine arg(s) in {n_files} file(s)"
    )
    fbuild.git_commit_files(sorted(changed_files, key=lambda p: str(p).lower()), msg, fscan.display_path)


def quote_cmd_arg(arg: str) -> str:
    """Quote one shell argument for safe compiler command construction."""
    return fbuild.quote_cmd_arg(arg)


def run_compiler_command(command: str, files: List[Path], phase: str) -> bool:
    """Run the configured compiler command and report pass/fail status."""
    return fbuild.run_compiler_command(command, files, phase, fscan.display_path)


def split_code_comment(line: str) -> Tuple[str, str]:
    """Split a line into code and trailing comment parts."""
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


def add_intent_attr(line: str, intent: str) -> Tuple[str, bool]:
    """Insert an INTENT attribute into a declaration line when eligible."""
    code, comment = split_code_comment(line)
    if "::" not in code:
        return line, False
    lhs, rhs = code.split("::", 1)
    if "intent(" in lhs.lower() or re.search(r"\bvalue\b", lhs.lower()):
        return line, False
    new_code = lhs.rstrip() + f", intent({intent}) ::" + rhs
    return new_code + comment, True


def split_decl_entities(rhs: str) -> List[str]:
    """Split declaration RHS entities on top-level commas."""
    parts: List[str] = []
    cur: List[str] = []
    depth = 0
    for ch in rhs:
        if ch == "(":
            depth += 1
        elif ch == ")" and depth > 0:
            depth -= 1
        if ch == "," and depth == 0:
            parts.append("".join(cur).strip())
            cur = []
        else:
            cur.append(ch)
    tail = "".join(cur).strip()
    if tail:
        parts.append(tail)
    return parts


def parse_declared_names_any(code_line: str) -> Set[str]:
    """Parse declared entity names from declaration lines with or without ::."""
    if "::" in code_line:
        return fscan.parse_declared_names_from_decl(code_line)
    m = NO_COLON_DECL_RE.match(code_line.strip())
    if not m:
        return set()
    out: Set[str] = set()
    for ent in split_decl_entities(m.group("rhs").strip()):
        mm = re.match(r"^\s*([a-z][a-z0-9_]*)", ent, re.IGNORECASE)
        if mm:
            out.add(mm.group(1).lower())
    return out


def rewrite_decl_line_with_intents(line: str, intents_by_name: Dict[str, str]) -> Tuple[List[str], bool]:
    """Rewrite one declaration line to add intents to targeted entities."""
    code, comment = split_code_comment(line)
    lhs = ""
    rhs = ""
    if "::" in code:
        lhs, rhs = code.split("::", 1)
    else:
        m_no = NO_COLON_DECL_RE.match(code.strip())
        if not m_no:
            return [line], False
        lhs = m_no.group("spec")
        rhs = m_no.group("rhs")

    lhs_low = lhs.lower()
    if "intent(" in lhs_low or re.search(r"\bvalue\b", lhs_low):
        return [line], False

    entities = split_decl_entities(rhs)
    if not entities:
        return [line], False

    targeted: List[Tuple[str, str]] = []
    remaining: List[str] = []
    for ent in entities:
        m = re.match(r"^\s*([a-z][a-z0-9_]*)", ent, re.IGNORECASE)
        if not m:
            remaining.append(ent)
            continue
        name = m.group(1).lower()
        if name in intents_by_name:
            targeted.append((ent, intents_by_name[name]))
        else:
            remaining.append(ent)

    if not targeted:
        return [line], False

    out_lines: List[str] = []
    base_lhs = lhs.rstrip()

    for ent, intent in targeted:
        out_lines.append(f"{base_lhs}, intent({intent}) :: {ent}\n")

    if remaining:
        out_lines.append(f"{base_lhs} :: {', '.join(remaining)}")
        if comment:
            out_lines[-1] = out_lines[-1] + comment
        out_lines[-1] += "\n"
    elif comment:
        # keep trailing comment on last emitted line
        out_lines[-1] = out_lines[-1].rstrip("\n") + comment + "\n"

    return out_lines, True


def analyze_intent_suggestions(
    finfo: fscan.SourceFileInfo,
    target_intent: str = "in",
) -> List[IntentSuggestion]:
    """Suggest dummy arguments that can be marked with the target intent."""
    out: List[IntentSuggestion] = []

    for proc in finfo.procedures:
        if not proc.dummy_names:
            continue

        local_names: Set[str] = set(proc.dummy_names)
        if proc.result_name:
            local_names.add(proc.result_name)

        # dummy -> declaration metadata
        decl: Dict[str, Tuple[int, bool, bool, bool]] = {}
        # line, has_intent_or_value, has_alloc_ptr, fixable

        for ln, code in proc.body:
            low = code.lower().strip()
            if not low or not TYPE_DECL_RE.match(low):
                continue
            if low.startswith("procedure"):
                continue
            declared = parse_declared_names_any(low)
            local_names.update(declared)
            has_intent_or_value = ("intent(" in low) or (re.search(r"\bvalue\b", low) is not None)
            has_alloc_ptr = ("allocatable" in low) or (re.search(r"\bpointer\b", low) is not None)
            for d in proc.dummy_names:
                if d in declared and d not in decl:
                    decl[d] = (ln, has_intent_or_value, has_alloc_ptr, True)

        writes: Set[str] = set()
        maybe_written_via_call: Set[str] = set()
        reads: Set[str] = set()
        first_event: Dict[str, Tuple[int, str]] = {}
        for ln, code in proc.body:
            low = code.lower()
            m_assign = ASSIGN_RE.match(low)
            if m_assign:
                lhs_base = fscan.base_identifier(m_assign.group(1))
                if lhs_base in proc.dummy_names:
                    writes.add(lhs_base)
                    if lhs_base not in first_event:
                        first_event[lhs_base] = (ln, "write")
                rhs = low.split("=", 1)[1] if "=" in low else ""
                for d in proc.dummy_names:
                    if re.search(rf"\b{re.escape(d)}\b", rhs):
                        reads.add(d)
                        if d not in first_event:
                            first_event[d] = (ln, "read")

            m_alloc = ALLOC_DEALLOC_RE.match(low)
            if m_alloc:
                first_obj = m_alloc.group(2).split(",", 1)[0].strip()
                obj = fscan.base_identifier(first_obj)
                if obj in proc.dummy_names:
                    writes.add(obj)
                    if obj not in first_event:
                        first_event[obj] = (ln, "write")

            if "read" in low and "(" in low and ")" in low:
                for d in proc.dummy_names:
                    if re.search(rf"\b{re.escape(d)}\b", low):
                        writes.add(d)
                        if d not in first_event:
                            first_event[d] = (ln, "write")

            if "=>" in low:
                for d in proc.dummy_names:
                    if re.search(rf"^\s*{re.escape(d)}\s*=>", low):
                        writes.add(d)
                        if d not in first_event:
                            first_event[d] = (ln, "write")

            if CALL_RE.search(low):
                for d in proc.dummy_names:
                    if re.search(rf"\b{re.escape(d)}\b", low):
                        maybe_written_via_call.add(d)
                        if d not in first_event:
                            first_event[d] = (ln, "call")

        for d in sorted(proc.dummy_names):
            meta = decl.get(d)
            if meta is None:
                continue
            decl_line, has_intent_or_value, has_alloc_ptr, fixable = meta
            if has_intent_or_value or has_alloc_ptr:
                continue
            if target_intent == "in":
                if d in writes or d in maybe_written_via_call:
                    continue
            else:
                # Conservative INTENT(OUT): must be written and not read/called before first write.
                if d not in writes:
                    continue
                ev = first_event.get(d)
                if ev is None or ev[1] != "write":
                    continue
                if d in maybe_written_via_call:
                    continue
                if d in reads and (d not in writes or ev[1] != "write"):
                    continue
            out.append(
                IntentSuggestion(
                    filename=fscan.display_path(finfo.path),
                    proc_kind=proc.kind,
                    proc_name=proc.name,
                    proc_start=proc.start,
                    dummy=d,
                    decl_line=decl_line,
                    fixable=fixable,
                    intent=target_intent,
                )
            )

    return out


def collect_missing_intent_args(finfo: fscan.SourceFileInfo) -> List[IntentSuggestion]:
    """Collect dummies still lacking INTENT/VALUE annotations."""
    out: List[IntentSuggestion] = []
    for proc in finfo.procedures:
        if not proc.dummy_names:
            continue
        decl: Dict[str, Tuple[int, bool]] = {}
        for ln, code in proc.body:
            low = code.lower().strip()
            if not low or not TYPE_DECL_RE.match(low):
                continue
            if low.startswith("procedure"):
                continue
            declared = parse_declared_names_any(low)
            has_intent_or_value = ("intent(" in low) or (re.search(r"\bvalue\b", low) is not None)
            for d in proc.dummy_names:
                if d in declared and d not in decl:
                    decl[d] = (ln, has_intent_or_value)
        for d in sorted(proc.dummy_names):
            meta = decl.get(d)
            if meta is None:
                continue
            decl_line, has_intent_or_value = meta
            if has_intent_or_value:
                continue
            out.append(
                IntentSuggestion(
                    filename=fscan.display_path(finfo.path),
                    proc_kind=proc.kind,
                    proc_name=proc.name,
                    proc_start=proc.start,
                    dummy=d,
                    decl_line=decl_line,
                    fixable=False,
                    intent="",
                )
            )
    return out


def apply_fix(
    finfo: fscan.SourceFileInfo,
    suggestions: List[IntentSuggestion],
    backup: bool,
    show_diff: bool,
) -> Tuple[int, Optional[Path], List[IntentSuggestion]]:
    """Apply intent suggestions to a file and optionally create a backup."""
    if not suggestions:
        return 0, None, []

    line_to_suggestions: Dict[int, List[IntentSuggestion]] = {}
    for s in suggestions:
        if not s.fixable:
            continue
        line_to_suggestions.setdefault(s.decl_line, [])
        if all(x.dummy.lower() != s.dummy.lower() for x in line_to_suggestions[s.decl_line]):
            line_to_suggestions[s.decl_line].append(s)

    if not line_to_suggestions:
        return 0, None, []

    updated = finfo.lines[:]
    changed = 0
    changed_items: List[IntentSuggestion] = []
    line_offset = 0
    for ln, slist in sorted(line_to_suggestions.items()):
        idx = ln - 1
        idx_adj = idx + line_offset
        if idx_adj < 0 or idx_adj >= len(updated):
            continue
        intents_by_name = {s.dummy.lower(): s.intent for s in slist}
        new_lines, ok = rewrite_decl_line_with_intents(updated[idx_adj], intents_by_name)
        if ok:
            updated[idx_adj:idx_adj + 1] = new_lines
            line_offset += len(new_lines) - 1
            changed += len(intents_by_name)
            changed_items.extend(slist)

    if changed == 0:
        return 0, None, []

    if show_diff:
        diff = difflib.unified_diff(
            finfo.lines,
            updated,
            fromfile=str(finfo.path),
            tofile=str(finfo.path),
            lineterm="",
        )
        print("\nProposed diff:")
        for line in diff:
            print(line)

    backup_path: Optional[Path] = None
    if backup:
        backup_path = finfo.path.with_name(finfo.path.name + ".bak")
        shutil.copy2(finfo.path, backup_path)
        print(f"\nBackup written: {backup_path}")

    with finfo.path.open("w", encoding="utf-8", newline="") as f:
        f.write("".join(updated))
    return changed, backup_path, changed_items


def rollback_backups(backup_pairs: List[Tuple[Path, Path]]) -> None:
    """Restore files from backups after compile validation fails."""
    fbuild.rollback_backups(backup_pairs, fscan.display_path)


def main() -> int:
    """Parse CLI arguments, run intent analysis/fixes, and print summaries."""
    parser = argparse.ArgumentParser(description="Suggest or add INTENT(IN) for dummy args")
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument(
        "--exclude",
        action="append",
        default=[],
        help="Glob pattern to exclude files (can be repeated)",
    )
    parser.add_argument("--fix", action="store_true", help="Apply INTENT(IN) edits for safe suggestions")
    parser.add_argument("--diff", action="store_true", help="Show unified diff before writing")
    parser.add_argument("--backup", dest="backup", action="store_true", default=True)
    parser.add_argument("--no-backup", dest="backup", action="store_false")
    parser.add_argument("--compiler", type=str, help="Compile command; supports {files} placeholder")
    parser.add_argument("--verbose", action="store_true", help="Show per-file details")
    parser.add_argument("--git", action="store_true", help="Commit changed files to git after successful run")
    parser.add_argument(
        "--warn-missing-intent",
        action="store_true",
        help="After final pass, report dummy arguments still lacking INTENT/VALUE",
    )
    parser.add_argument(
        "--suggest-intent-out",
        action="store_true",
        help="Suggest/apply INTENT(OUT) (default is INTENT(IN))",
    )
    parser.add_argument(
        "--iterate",
        action="store_true",
        help="With --fix, repeat analyze/fix passes until no more changes",
    )
    parser.add_argument(
        "--max-iter",
        type=int,
        default=10,
        help="Maximum iterations for --iterate (default: 10)",
    )
    args = parser.parse_args()
    if args.iterate and not args.fix:
        print("--iterate requires --fix.")
        return 3
    if args.max_iter < 1:
        print("--max-iter must be >= 1.")
        return 3

    if not args.fortran_files:
        args.fortran_files = sorted(
            set(Path(".").glob("*.f90")) | set(Path(".").glob("*.F90")),
            key=lambda p: p.name.lower(),
        )
        if not args.fortran_files:
            print("No source files provided and no .f90/.F90 files found in current directory.")
            return 2
    args.fortran_files = fscan.apply_excludes(args.fortran_files, args.exclude)
    if not args.fortran_files:
        print("No source files remain after applying --exclude filters.")
        return 2

    changed_summary: Dict[Tuple[str, str], List[str]] = {}
    changed_files: Set[Path] = set()
    suggest_summary_last: Dict[Tuple[str, str], List[str]] = {}
    max_passes = args.max_iter if args.iterate else 1
    did_baseline_compile = False

    for it in range(1, max_passes + 1):
        source_files, any_missing = fscan.load_source_files(args.fortran_files)
        if not source_files:
            return 2 if any_missing else 1

        ordered_files, _ = fscan.order_files_least_dependent(source_files)
        if it == 1 and len(ordered_files) > 1:
            print("Processing order: " + " ".join(fscan.display_path(f.path) for f in ordered_files))

        compile_paths = [f.path for f in ordered_files]
        if args.compiler:
            compile_paths, _ = fscan.build_compile_closure(ordered_files)
            if args.fix and not did_baseline_compile:
                if not run_compiler_command(args.compiler, compile_paths, phase="baseline"):
                    return 5
                did_baseline_compile = True

        pass_suggest_summary: Dict[Tuple[str, str], List[str]] = {}
        backup_pairs: List[Tuple[Path, Path]] = []
        pass_changed = 0

        for finfo in ordered_files:
            suggestions_in = analyze_intent_suggestions(finfo, target_intent="in")
            suggestions_out: List[IntentSuggestion] = []
            if args.suggest_intent_out:
                suggestions_out = analyze_intent_suggestions(finfo, target_intent="out")
            suggestions = suggestions_in + suggestions_out
            if args.verbose:
                print(f"File: {fscan.display_path(finfo.path)}")
                if args.suggest_intent_out:
                    print(f"\n{len(suggestions)} Likely INTENT(IN/OUT) suggestions:")
                else:
                    print(f"\n{len(suggestions)} Likely INTENT(IN) suggestions:")
                for s in suggestions:
                    mark = "" if s.fixable else " (manual)"
                    print(f"  - {s.proc_kind} {s.proc_name}:{s.dummy} [line {s.decl_line}]{mark}")

            for s in suggestions:
                add_summary_item(pass_suggest_summary, s)

            if not args.fix:
                continue

            changed, backup_path, changed_items = apply_fix(
                finfo, suggestions, backup=args.backup, show_diff=args.diff
            )
            pass_changed += changed
            if backup_path:
                backup_pairs.append((finfo.path, backup_path))
            if changed > 0:
                for s in changed_items:
                    add_summary_item(changed_summary, s)
                changed_files.add(finfo.path)
                if args.suggest_intent_out:
                    print(f"\nApplied INTENT(IN/OUT) to {changed} declaration(s).")
                else:
                    print(f"\nApplied INTENT(IN) to {changed} declaration(s).")

        if args.compiler:
            phase = "after-fix" if args.fix else "current"
            if not run_compiler_command(args.compiler, compile_paths, phase=phase):
                if args.fix and args.backup and backup_pairs:
                    rollback_backups(backup_pairs)
                return 5

        if not args.fix:
            suggest_summary_last = pass_suggest_summary
            break

        if not args.iterate:
            break
        if pass_changed == 0:
            break
        if it == max_passes:
            print(f"Reached max iterations ({args.max_iter}).")

    if args.fix:
        maybe_git_commit(
            args.git,
            changed_files,
            changed_summary,
            "intent(in/out)" if args.suggest_intent_out else "intent(in)",
        )
        if args.suggest_intent_out:
            print_summary(changed_summary, label="with arguments marked intent(in/out)")
        else:
            print_summary(changed_summary, label="with arguments marked intent(in)")
    else:
        if args.suggest_intent_out:
            print_summary(suggest_summary_last, label="likely needing intent(in/out)")
        else:
            print_summary(suggest_summary_last, label="likely needing intent(in)")

    if args.warn_missing_intent:
        source_files, any_missing = fscan.load_source_files(args.fortran_files)
        if not source_files:
            return 2 if any_missing else 1
        final_ordered, _ = fscan.order_files_least_dependent(source_files)
        missing_summary: Dict[Tuple[str, str], List[str]] = {}
        missing_count = 0
        for finfo in final_ordered:
            missing = collect_missing_intent_args(finfo)
            for s in missing:
                add_summary_item(missing_summary, s)
                missing_count += 1
        if missing_count > 0:
            print_summary(missing_summary, label="with arguments still missing intent/value")
        else:
            print("\nNo procedure arguments are missing intent/value.")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
