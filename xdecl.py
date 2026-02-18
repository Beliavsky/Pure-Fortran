#!/usr/bin/env python3
"""Suggest/fix Fortran declarations missing :: separators."""

from __future__ import annotations

import argparse
import difflib
import re
import shutil
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Tuple

import cli_paths as cpaths
import fortran_build as fbuild
import fortran_scan as fscan


@dataclass
class Finding:
    """One declaration rewrite candidate."""

    path: Path
    line: int
    original: str
    suggestion: str


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


def split_top_level_commas(text: str) -> List[str]:
    """Split text on commas while respecting nested parentheses and quoted strings."""
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


def top_level_space_positions(text: str) -> List[int]:
    """Return space positions that occur at top level outside strings/parentheses."""
    out: List[int] = []
    depth = 0
    in_single = False
    in_double = False
    prev_space = False
    for i, ch in enumerate(text):
        if ch == "'" and not in_double:
            in_single = not in_single
        elif ch == '"' and not in_single:
            in_double = not in_double
        elif not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")" and depth > 0:
                depth -= 1
        is_space = ch.isspace()
        if depth == 0 and not in_single and not in_double and is_space and not prev_space:
            out.append(i)
        prev_space = is_space
    return out


def is_decl_head(text: str) -> bool:
    """Return whether text looks like a declaration head before variable list."""
    if text.strip().endswith(","):
        return False
    parts = split_top_level_commas(text)
    if not parts:
        return False
    first = parts[0].strip().lower()
    if re.match(r"^(integer|real|logical|complex|character|double\s+precision)\s*(\([^)]*\))?$", first):
        return True
    if re.match(r"^(type|class|procedure)\s*\([^)]*\)$", first):
        return True
    return False


def rhs_looks_like_decl_list(text: str) -> bool:
    """Return whether RHS looks like a declaration entity list."""
    reserved = {
        "function",
        "subroutine",
        "program",
        "module",
        "contains",
        "if",
        "then",
        "do",
        "select",
        "where",
        "block",
        "interface",
    }
    parts = split_top_level_commas(text)
    if not parts:
        return False
    for part in parts:
        p = part.strip()
        if not p:
            return False
        m = re.match(r"^([a-z][a-z0-9_]*)\b", p, re.IGNORECASE)
        if not m:
            return False
        if m.group(1).lower() in reserved:
            return False
    return True


def rewrite_decl_line(code: str) -> Tuple[str, bool]:
    """Insert :: into one declaration line when safe and missing."""
    if "::" in code:
        return code, False
    if ";" in code:
        return code, False
    if "&" in code:
        return code, False

    stripped = code.strip()
    if not stripped:
        return code, False
    if stripped.lower().startswith(("type is", "class is", "class default")):
        return code, False

    indent = re.match(r"^\s*", code).group(0)
    body = stripped
    for pos in top_level_space_positions(body):
        left = body[:pos].rstrip()
        right = body[pos + 1 :].lstrip()
        if not left or not right:
            continue
        if not is_decl_head(left):
            continue
        if not rhs_looks_like_decl_list(right):
            continue
        return f"{indent}{left} :: {right}", True
    return code, False


def analyze_file(path: Path) -> List[Finding]:
    """Analyze one source file and return declaration rewrite findings."""
    lines = path.read_text(encoding="utf-8", errors="ignore").splitlines(keepends=True)
    findings: List[Finding] = []
    for i, raw in enumerate(lines):
        body = raw.rstrip("\r\n")
        code, comment = split_code_comment(body)
        new_code, changed = rewrite_decl_line(code)
        if not changed:
            continue
        findings.append(
            Finding(
                path=path,
                line=i + 1,
                original=code.rstrip(),
                suggestion=(new_code + comment).rstrip(),
            )
        )
    return findings


def annotate_file(path: Path, findings: List[Finding], *, backup: bool = True) -> Tuple[int, Optional[Path]]:
    """Insert rewrite suggestion comments after matching lines."""
    if not findings:
        return 0, None
    lines = path.read_text(encoding="utf-8", errors="ignore").splitlines(keepends=True)
    inserts: List[Tuple[int, str]] = []
    for f in findings:
        idx = f.line - 1
        if idx < 0 or idx >= len(lines):
            continue
        raw = lines[idx]
        indent = re.match(r"^\s*", raw).group(0) if raw else ""
        eol = "\r\n" if raw.endswith("\r\n") else ("\n" if raw.endswith("\n") else "\n")
        msg = f"{indent}! {f.suggestion}  !! suggested by xdecl.py{eol}"
        nxt = idx + 1
        if 0 <= nxt < len(lines) and lines[nxt].strip().lower() == msg.strip().lower():
            continue
        inserts.append((idx + 1, msg))

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
    findings: List[Finding],
    *,
    annotate: bool = False,
    out_path: Optional[Path] = None,
    create_backup: bool = True,
) -> Tuple[int, Optional[Path]]:
    """Rewrite matching declaration lines to include ::."""
    if not findings:
        return 0, None
    by_line: Dict[int, Finding] = {f.line: f for f in findings}
    lines = path.read_text(encoding="utf-8", errors="ignore").splitlines(keepends=True)
    changed = 0
    for i, raw in enumerate(lines):
        f = by_line.get(i + 1)
        if f is None:
            continue
        body = raw.rstrip("\r\n")
        eol = raw[len(body) :]
        code, comment = split_code_comment(body)
        new_code, ok = rewrite_decl_line(code)
        if not ok:
            continue
        suffix = "  !! changed by xdecl.py" if annotate else ""
        trailing = f" {comment.strip()}" if comment.strip() else ""
        lines[i] = f"{new_code}{suffix}{trailing}{eol}"
        changed += 1
    if changed == 0:
        return 0, None
    backup: Optional[Path] = None
    target = out_path if out_path is not None else path
    if out_path is None and create_backup:
        backup = make_backup_path(path)
        shutil.copy2(path, backup)
    target.write_text("".join(lines), encoding="utf-8")
    return changed, backup


def main() -> int:
    """Run declaration-colon advisory/fix workflow."""
    parser = argparse.ArgumentParser(description="Suggest/fix Fortran declarations missing :: separators")
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--verbose", action="store_true", help="Print suggested rewritten declaration lines")
    parser.add_argument("--fix", action="store_true", help="Rewrite declaration lines to include ::")
    parser.add_argument("--out", type=Path, help="With --fix, write transformed output to this file (single input)")
    parser.add_argument("--out-dir", type=Path, help="With --fix, write transformed outputs to this directory")
    parser.add_argument("--backup", dest="backup", action="store_true", default=True)
    parser.add_argument("--no-backup", dest="backup", action="store_false")
    parser.add_argument("--annotate", action="store_true", help="Insert suggestion comments (or changed tags with --fix)")
    parser.add_argument("--diff", action="store_true", help="With --fix, print unified diffs for changed files")
    parser.add_argument("--compiler", type=str, help="Compile command for baseline/after-fix validation")
    parser.add_argument("--limit", type=int, help="Maximum number of source files to process")
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

    findings: List[Finding] = []
    by_file: Dict[Path, List[Finding]] = {}
    for p in files:
        fnds = analyze_file(p)
        if not fnds:
            continue
        by_file[p] = fnds
        findings.extend(fnds)

    if not findings:
        print("No declaration lines missing :: found.")
        return 0

    findings.sort(key=lambda f: (f.path.name.lower(), f.line))
    print(f"{len(findings)} declaration line(s) can be rewritten with ::.")
    for f in findings:
        print(f"{f.path.name}:{f.line}")
        if args.verbose:
            print(f"  suggest: {f.suggestion}")

    if args.fix:
        touched = 0
        total = 0
        for p in sorted(by_file.keys(), key=lambda x: x.name.lower()):
            before = p.read_text(encoding="utf-8", errors="ignore")
            out_path = args.out if args.out is not None else (args.out_dir / p.name if args.out_dir is not None else None)
            n, backup = apply_fix_file(
                p, by_file[p], annotate=args.annotate, out_path=out_path, create_backup=args.backup
            )
            total += n
            if n > 0:
                touched += 1
                if out_path is not None:
                    print(f"\nFixed {p.name}: replaced {n}, wrote {out_path}")
                else:
                    print(f"\nFixed {p.name}: replaced {n}, backup {backup.name if backup else '(none)'}")
                if args.diff:
                    after = (out_path if out_path is not None else p).read_text(encoding="utf-8", errors="ignore")
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
        print(f"\n--fix summary: files changed {touched}, replaced {total}")
    elif args.annotate:
        touched = 0
        total = 0
        for p in sorted(by_file.keys(), key=lambda x: x.name.lower()):
            n, backup = annotate_file(p, by_file[p], backup=args.backup)
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
