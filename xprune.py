#!/usr/bin/env python3
"""Prune likely unused Fortran procedures with compile validation."""

from __future__ import annotations

import argparse
import re
import shutil
import subprocess
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Set, Tuple

import cli_paths as cpaths
import fortran_scan as fscan

PROC_START_RE = re.compile(
    r"^\s*(?:(?:pure|elemental|impure|recursive|module)\s+)*(function|subroutine)\s+([a-z][a-z0-9_]*)\b",
    re.IGNORECASE,
)
PROC_END_RE = re.compile(r"^\s*end(?:\s+(?:function|subroutine))?\b", re.IGNORECASE)
CALL_RE = re.compile(r"\bcall\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
USE_RE = re.compile(
    r"^\s*use\b(?:\s*,\s*(?:non_intrinsic|intrinsic)\s*)?"
    r"(?:\s*::\s*|\s+)([a-z][a-z0-9_]*)(.*)$",
    re.IGNORECASE,
)
FUNC_REF_RE = re.compile(r"\b([a-z][a-z0-9_]*)\s*\(", re.IGNORECASE)
PUBLIC_RE = re.compile(r"^\s*public\b(.*)$", re.IGNORECASE)


@dataclass
class ProcRef:
    """Store removable procedure location metadata."""

    path: Path
    name: str
    kind: str
    start: int
    end: int


def quote_cmd_arg(arg: str) -> str:
    """Quote one shell argument for command construction."""
    return subprocess.list2cmdline([arg])


def parse_use_only_names(rest: str) -> Optional[Set[str]]:
    """Return names imported by USE, ONLY or None for wildcard use."""
    m = re.search(r"\bonly\s*:\s*(.*)$", rest, re.IGNORECASE)
    if not m:
        return None
    rhs = m.group(1).strip()
    if not rhs:
        return set()
    names: Set[str] = set()
    for tok in rhs.split(","):
        t = tok.strip()
        if not t:
            continue
        if "=>" in t:
            t = t.split("=>", 1)[1].strip()
        if t.lower().startswith(("operator(", "assignment(")):
            continue
        mm = re.match(r"^([a-z][a-z0-9_]*)$", t, re.IGNORECASE)
        if mm:
            names.add(mm.group(1).lower())
    return names


def split_code_comment(line: str) -> Tuple[str, str]:
    """Split one line into code and trailing comment text."""
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
    """Return the input line ending sequence."""
    if line.endswith("\r\n"):
        return "\r\n"
    if line.endswith("\n"):
        return "\n"
    return ""


def split_top_level_commas(text: str) -> List[str]:
    """Split text on top-level commas outside parentheses and strings."""
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


def strip_name_from_public_stmt(stmt: str, target: str) -> Tuple[str, bool]:
    """Remove one name from a PUBLIC statement if present."""
    m = PUBLIC_RE.match(stmt.strip())
    if not m:
        return stmt, False
    rest = m.group(1).strip()
    if not rest:
        return stmt, False
    if rest.startswith("::"):
        rest = rest[2:].strip()
    elif rest.startswith(","):
        rest = rest[1:].strip()
    if not rest:
        return stmt, False
    names = split_top_level_commas(rest)
    kept: List[str] = []
    removed = False
    for n in names:
        token = n.strip().lower()
        if token == target.lower():
            removed = True
        else:
            kept.append(n.strip())
    if not removed:
        return stmt, False
    indent = re.match(r"^\s*", stmt).group(0)
    if kept:
        return f"{indent}public :: {', '.join(kept)}", True
    return "", True


def remove_name_from_public_lines(lines: List[str], target: str) -> List[str]:
    """Remove target symbol from PUBLIC accessibility statements across a file."""
    out: List[str] = []
    i = 0
    while i < len(lines):
        line = lines[i]
        code0, _comment0 = split_code_comment(line.rstrip("\r\n"))
        if not PUBLIC_RE.match(code0.strip()):
            out.append(line)
            i += 1
            continue

        # Collect continuation block for this PUBLIC statement.
        block_idx = [i]
        segs: List[str] = []
        seg0 = code0.rstrip()
        cont = seg0.endswith("&")
        if cont:
            seg0 = seg0[:-1].rstrip()
        if seg0.strip():
            segs.append(seg0)

        j = i + 1
        while j < len(lines):
            codej, _commentj = split_code_comment(lines[j].rstrip("\r\n"))
            lead = codej.lstrip()
            if not (cont or lead.startswith("&")):
                break
            if lead.startswith("&"):
                segj = lead[1:].lstrip()
            else:
                segj = lead
            cont = segj.rstrip().endswith("&")
            if cont:
                segj = segj[:-1].rstrip()
            if segj.strip():
                segs.append(segj)
            block_idx.append(j)
            j += 1

        combined = " ".join(segs).strip()
        changed_any = False
        kept_parts: List[str] = []
        for stmt in fscan.split_fortran_statements(combined):
            new_stmt, changed = strip_name_from_public_stmt(stmt, target)
            changed_any = changed_any or changed
            if new_stmt.strip():
                kept_parts.append(new_stmt.strip())

        if changed_any:
            eol = get_eol(lines[i]) or "\n"
            rebuilt = "; ".join(kept_parts).strip()
            if rebuilt:
                indent = re.match(r"^\s*", lines[i]).group(0)
                out.append(f"{indent}{rebuilt}{eol}")
            # If emptied, drop the PUBLIC block entirely.
        else:
            for k in block_idx:
                out.append(lines[k])
        i = j
    return out


def run_compile(command: str, files: List[Path], phase: str, cwd: Path) -> bool:
    """Run compile command in cwd and print pass/fail diagnostics."""
    file_args = " ".join(quote_cmd_arg(str(p.name)) for p in files)
    if "{files}" in command:
        cmd = command.replace("{files}", file_args)
    else:
        cmd = f"{command} {file_args}".strip()
    print(f"Compile ({phase}): {cmd}")
    proc = subprocess.run(cmd, shell=True, capture_output=True, text=True, cwd=str(cwd))
    if proc.returncode == 0:
        print(f"Compile ({phase}): PASS")
        return True
    print(f"Compile ({phase}): FAIL (exit {proc.returncode})")
    if proc.stdout:
        print(proc.stdout.rstrip())
    if proc.stderr:
        print(proc.stderr.rstrip())
    return False


def choose_files(args_files: List[Path], exclude: List[str]) -> List[Path]:
    """Resolve source files from CLI args or current directory defaults."""
    if args_files:
        files = cpaths.expand_path_args(args_files)
    else:
        files = sorted(
            set(Path(".").glob("*.f90")) | set(Path(".").glob("*.F90")),
            key=lambda p: p.name.lower(),
        )
    return fscan.apply_excludes(files, exclude)


def copy_to_out_dir(files: List[Path], out_dir: Path) -> List[Path]:
    """Copy source files into out_dir and return copied paths."""
    out_dir.mkdir(parents=True, exist_ok=True)
    copied: List[Path] = []
    for p in files:
        dst = out_dir / p.name
        shutil.copy2(p, dst)
        copied.append(dst)
    return copied


def collect_procedures(infos: List[fscan.SourceFileInfo]) -> List[ProcRef]:
    """Collect top-level procedures that are candidates for pruning."""
    out: List[ProcRef] = []
    for finfo in infos:
        for proc in finfo.procedures:
            if proc.parent is not None:
                continue
            out.append(ProcRef(finfo.path, proc.name.lower(), proc.kind, proc.start, proc.end))
    return out


def collect_used_names(infos: List[fscan.SourceFileInfo], known_names: Set[str]) -> Set[str]:
    """Collect referenced procedure names from use/call/function-reference patterns."""
    used: Set[str] = set()
    for finfo in infos:
        for _lineno, stmt in fscan.iter_fortran_statements(finfo.parsed_lines):
            low = stmt.strip().lower()
            if not low:
                continue
            if PROC_START_RE.match(low) or PROC_END_RE.match(low):
                continue

            m_use = USE_RE.match(low)
            if m_use:
                only_names = parse_use_only_names(m_use.group(2) or "")
                if only_names is None:
                    # wildcard use keeps all names conservative
                    used.update(known_names)
                else:
                    used.update(n for n in only_names if n in known_names)
                continue

            for m in CALL_RE.finditer(low):
                n = m.group(1).lower()
                if n in known_names:
                    used.add(n)

            for m in FUNC_REF_RE.finditer(low):
                n = m.group(1).lower()
                if n in known_names:
                    used.add(n)
    return used


def remove_procedure_block(path: Path, proc: ProcRef) -> str:
    """Delete a procedure block from a source file and return previous text."""
    old_text = path.read_text(encoding="utf-8", errors="ignore")
    lines = old_text.splitlines(keepends=True)
    start_idx = max(0, proc.start - 1)
    end_idx = min(len(lines), proc.end)
    del lines[start_idx:end_idx]
    lines = remove_name_from_public_lines(lines, proc.name)
    path.write_text("".join(lines), encoding="utf-8", newline="")
    return old_text


def main() -> int:
    """Run compile-validated pruning to remove likely unused procedures."""
    parser = argparse.ArgumentParser(description="Prune likely unused Fortran procedures")
    parser.add_argument("fortran_files", type=Path, nargs="*", help="Source files (default: *.f90/*.F90)")
    parser.add_argument(
        "--exclude",
        action="append",
        default=[],
        help="Glob pattern to exclude files (can be repeated)",
    )
    parser.add_argument(
        "--compiler",
        type=str,
        required=True,
        help='Compilation command, e.g. "gfortran -o app.exe"',
    )
    parser.add_argument("--out-dir", type=Path, default=Path("pruned"), help="Output directory (default: pruned)")
    parser.add_argument("--in-place", action="store_true", help="Modify sources in place (default: off)")
    parser.add_argument("--max-iter", type=int, default=10, help="Maximum prune passes (default: 10)")
    parser.add_argument("--verbose", action="store_true", help="Print accepted/rejected removals")
    args = parser.parse_args()

    if args.max_iter < 1:
        print("--max-iter must be >= 1.")
        return 2

    src_files = choose_files(args.fortran_files, args.exclude)
    if not src_files:
        print("No source files remain after applying --exclude filters.")
        return 2

    if args.in_place:
        work_files = src_files
        work_dir = Path(".").resolve()
    else:
        work_files = copy_to_out_dir(src_files, args.out_dir)
        work_dir = args.out_dir.resolve()
        print(f"Wrote working sources to: {work_dir}")

    infos, any_missing = fscan.load_source_files(work_files)
    if not infos:
        return 2 if any_missing else 1
    ordered_infos, _ = fscan.order_files_least_dependent(infos)
    compile_files, _ = fscan.build_compile_closure(ordered_infos)
    compile_files = [p for p in compile_files if p.parent.resolve() == work_dir]
    if not compile_files:
        compile_files = [f.path for f in ordered_infos]

    if not run_compile(args.compiler, compile_files, phase="baseline", cwd=work_dir):
        return 1

    removed: List[ProcRef] = []
    for it in range(1, args.max_iter + 1):
        infos, any_missing = fscan.load_source_files(work_files)
        if not infos:
            return 2 if any_missing else 1
        procedures = collect_procedures(infos)
        if not procedures:
            break
        known_names = {p.name for p in procedures}
        used_names = collect_used_names(infos, known_names)
        candidates = [p for p in procedures if p.name not in used_names]
        if not candidates:
            break

        changed_this_iter = 0
        for proc in sorted(candidates, key=lambda x: (x.path.name.lower(), x.start)):
            old_text = remove_procedure_block(proc.path, proc)
            if run_compile(args.compiler, compile_files, phase="trial", cwd=work_dir):
                removed.append(proc)
                changed_this_iter += 1
                if args.verbose:
                    print(f"Removed: {proc.path.name} {proc.kind} {proc.name} [{proc.start}-{proc.end}]")
            else:
                proc.path.write_text(old_text, encoding="utf-8", newline="")
                if args.verbose:
                    print(f"Kept: {proc.path.name} {proc.kind} {proc.name} [{proc.start}-{proc.end}]")

        if changed_this_iter == 0:
            break

    if not run_compile(args.compiler, compile_files, phase="final", cwd=work_dir):
        print("Final compile failed; no further changes applied.")
        return 1

    print(f"\nRemoved {len(removed)} procedure(s).")
    if removed:
        by_file: Dict[str, List[str]] = {}
        for p in removed:
            by_file.setdefault(p.path.name, []).append(p.name)
        for fname in sorted(by_file.keys(), key=str.lower):
            names = by_file[fname]
            print(f"{fname} {len(names)}: {' '.join(names)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
