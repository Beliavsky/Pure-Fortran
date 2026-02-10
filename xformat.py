#!/usr/bin/env python3
"""Advisory checker for Fortran format literals that can use repeat descriptors."""

from __future__ import annotations

import argparse
import difflib
import re
import shutil
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, List, Optional, Tuple

import cli_paths as cpaths
import fortran_scan as fscan

PRINT_RE = re.compile(r"^\s*print\b", re.IGNORECASE)
WRITE_RE = re.compile(r"^\s*write\b", re.IGNORECASE)
SIMPLE_DESC_RE = re.compile(r"^[a-z][a-z0-9.]*$", re.IGNORECASE)


@dataclass
class Finding:
    """One advisory finding for a shorten-able format literal."""

    path: Path
    line: int
    original: str
    suggested: str
    stmt: str


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
    """Split text by top-level commas (outside quotes and parentheses)."""
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


def split_top_level_equals(text: str) -> Optional[Tuple[str, str]]:
    """Split `key=value` at first top-level equals sign, if present."""
    depth = 0
    in_single = False
    in_double = False
    i = 0
    while i < len(text):
        ch = text[i]
        if ch == "'" and not in_double:
            if in_single and i + 1 < len(text) and text[i + 1] == "'":
                i += 2
                continue
            in_single = not in_single
            i += 1
            continue
        if ch == '"' and not in_single:
            if in_double and i + 1 < len(text) and text[i + 1] == '"':
                i += 2
                continue
            in_double = not in_double
            i += 1
            continue
        if not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")" and depth > 0:
                depth -= 1
            elif ch == "=" and depth == 0:
                return text[:i].strip(), text[i + 1 :].strip()
        i += 1
    return None


def find_matching_paren(text: str, start_idx: int) -> int:
    """Return index of matching `)` for `(` at `start_idx`, else -1."""
    depth = 0
    in_single = False
    in_double = False
    i = start_idx
    while i < len(text):
        ch = text[i]
        if ch == "'" and not in_double:
            if in_single and i + 1 < len(text) and text[i + 1] == "'":
                i += 2
                continue
            in_single = not in_single
            i += 1
            continue
        if ch == '"' and not in_single:
            if in_double and i + 1 < len(text) and text[i + 1] == '"':
                i += 2
                continue
            in_double = not in_double
            i += 1
            continue
        if not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
                if depth == 0:
                    return i
        i += 1
    return -1


def parse_string_literal(expr: str) -> Optional[Tuple[str, str]]:
    """Return (delimiter, inner text) for a simple Fortran string literal."""
    s = expr.strip()
    if len(s) < 2:
        return None
    if s[0] == "'" and s[-1] == "'":
        return ("'", s[1:-1])
    if s[0] == '"' and s[-1] == '"':
        return ('"', s[1:-1])
    return None


def normalize_token(token: str) -> str:
    """Normalize a format token for matching: lowercase and remove whitespace."""
    out: List[str] = []
    in_single = False
    in_double = False
    i = 0
    while i < len(token):
        ch = token[i]
        if ch == "'" and not in_double:
            if in_single and i + 1 < len(token) and token[i + 1] == "'":
                out.append("''")
                i += 2
                continue
            in_single = not in_single
            out.append(ch)
            i += 1
            continue
        if ch == '"' and not in_single:
            if in_double and i + 1 < len(token) and token[i + 1] == '"':
                out.append('""')
                i += 2
                continue
            in_double = not in_double
            out.append(ch)
            i += 1
            continue
        if ch.isspace() and not in_single and not in_double:
            i += 1
            continue
        if in_single or in_double:
            out.append(ch)
        else:
            out.append(ch.lower())
        i += 1
    return "".join(out)


def can_group_token(tok: str) -> bool:
    """Return True when token is in the conservative set for grouping."""
    if not tok:
        return False
    if "'" in tok or '"' in tok:
        return False
    if "(" in tok or ")" in tok:
        return False
    if "/" in tok or ":" in tok:
        return False
    return SIMPLE_DESC_RE.match(tok) is not None


def compress_repeated_groups(tokens: List[str]) -> List[str]:
    """Compress repeated adjacent groups of 2+ tokens into k(...)."""
    out: List[str] = []
    i = 0
    n = len(tokens)
    while i < n:
        best_g = 0
        best_k = 0
        best_span = 0
        max_g = (n - i) // 2
        for g in range(2, max_g + 1):
            block = tokens[i : i + g]
            if not all(can_group_token(t) for t in block):
                continue
            k = 1
            while i + (k + 1) * g <= n and tokens[i + k * g : i + (k + 1) * g] == block:
                k += 1
            if k >= 2:
                span = g * k
                if span > best_span or (span == best_span and g > best_g):
                    best_g = g
                    best_k = k
                    best_span = span
        if best_k >= 2 and best_g >= 2:
            block = tokens[i : i + best_g]
            out.append(f"{best_k}({','.join(block)})")
            i += best_g * best_k
            continue
        out.append(tokens[i])
        i += 1
    return out


def compress_token_runs(tokens: List[str]) -> List[str]:
    """Compress repeated identical tokens into kTOKEN."""
    out: List[str] = []
    i = 0
    n = len(tokens)
    while i < n:
        j = i + 1
        while j < n and tokens[j] == tokens[i]:
            j += 1
        k = j - i
        tok = tokens[i]
        if k >= 2 and can_group_token(tok):
            out.append(f"{k}{tok}")
        else:
            out.extend(tokens[i:j])
        i = j
    return out


def shorten_format_inner(inner: str) -> Optional[str]:
    """Return shortened inner format text when safe, else None."""
    text = inner.strip()
    if not text:
        return None
    tokens = [normalize_token(t) for t in split_top_level_commas(text) if t.strip()]
    if len(tokens) < 2:
        return None
    stage1 = compress_token_runs(tokens)
    stage2 = compress_repeated_groups(stage1)
    stage3 = compress_token_runs(stage2)
    candidate = ",".join(stage3)
    baseline = ",".join(tokens)
    if candidate == baseline or len(candidate) >= len(baseline):
        return None
    return candidate


def extract_format_literal(stmt: str) -> Optional[Tuple[str, str]]:
    """Extract format literal from PRINT/WRITE statement as (delim, inner text)."""
    if PRINT_RE.match(stmt):
        rest = stmt.strip()[5:].strip()
        parts = split_top_level_commas(rest)
        if not parts:
            return None
        return parse_string_literal(parts[0])

    if WRITE_RE.match(stmt):
        low = stmt.lower()
        pos_write = low.find("write")
        if pos_write < 0:
            return None
        pos_lpar = stmt.find("(", pos_write)
        if pos_lpar < 0:
            return None
        pos_rpar = find_matching_paren(stmt, pos_lpar)
        if pos_rpar < 0:
            return None
        control = stmt[pos_lpar + 1 : pos_rpar]
        ctl_parts = split_top_level_commas(control)
        if not ctl_parts:
            return None

        fmt_expr: Optional[str] = None
        positional: List[str] = []
        for p in ctl_parts:
            kv = split_top_level_equals(p)
            if kv is None:
                positional.append(p.strip())
                continue
            key, val = kv
            if key.strip().lower() == "fmt":
                fmt_expr = val.strip()
        if fmt_expr is None and len(positional) >= 2:
            fmt_expr = positional[1]
        if fmt_expr is None:
            return None
        return parse_string_literal(fmt_expr)
    return None


def analyze_file(path: Path) -> List[Finding]:
    """Analyze one file and return format-shortening findings."""
    infos, any_missing = fscan.load_source_files([path])
    if not infos or any_missing:
        return []
    finfo = infos[0]
    out: List[Finding] = []
    for lineno, stmt in fscan.iter_fortran_statements(finfo.parsed_lines):
        fmt_lit = extract_format_literal(stmt)
        if fmt_lit is None:
            continue
        delim, inner_lit = fmt_lit
        txt = inner_lit.strip()
        if not (txt.startswith("(") and txt.endswith(")")):
            continue
        shortened = shorten_format_inner(txt[1:-1])
        if shortened is None:
            continue
        out.append(
            Finding(
                path=path,
                line=lineno,
                original=f"{delim}{txt}{delim}",
                suggested=f"{delim}({shortened}){delim}",
                stmt=stmt.strip(),
            )
        )
    return out


def apply_fix_file(path: Path, findings: List[Finding]) -> Tuple[int, Optional[Path]]:
    """Apply suggested format rewrites for one file in a conservative way."""
    by_line: dict[int, List[Finding]] = {}
    for f in findings:
        by_line.setdefault(f.line, []).append(f)

    lines = path.read_text(encoding="utf-8").splitlines(keepends=True)
    changed = 0
    touched = False

    for line_no, line_findings in sorted(by_line.items()):
        idx = line_no - 1
        if idx < 0 or idx >= len(lines):
            continue
        raw = lines[idx]
        body = raw.rstrip("\r\n")
        eol = raw[len(body) :]

        # Apply longer originals first to avoid nested partial overlap.
        cur = body
        line_changes = 0
        for f in sorted(line_findings, key=lambda x: len(x.original), reverse=True):
            count = cur.count(f.original)
            if count == 1:
                cur = cur.replace(f.original, f.suggested, 1)
                line_changes += 1
            else:
                # Skip ambiguous replacements on this line.
                continue

        if line_changes > 0 and cur != body:
            lines[idx] = cur + eol
            changed += line_changes
            touched = True

    if not touched:
        return 0, None

    backup = make_backup_path(path)
    shutil.copy2(path, backup)
    path.write_text("".join(lines), encoding="utf-8")
    return changed, backup


def main() -> int:
    """Run advisory format-shortening checks across selected Fortran files."""
    parser = argparse.ArgumentParser(
        description="Warn when Fortran format literals can be shortened with repeat descriptors"
    )
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--verbose", action="store_true", help="Print offending statement lines")
    parser.add_argument("--fix", action="store_true", help="Apply suggested format literal rewrites")
    parser.add_argument("--diff", action="store_true", help="With --fix, print unified diffs for changed files")
    args = parser.parse_args()

    if args.diff and not args.fix:
        print("--diff requires --fix.")
        return 2

    files = choose_files(args.fortran_files, args.exclude)
    if not files:
        print("No source files remain after applying --exclude filters.")
        return 2

    findings: List[Finding] = []
    for p in files:
        findings.extend(analyze_file(p))

    if not findings:
        print("No shorten-able format literals found.")
        return 0

    findings.sort(key=lambda f: (f.path.name.lower(), f.line))
    print(f"{len(findings)} shorten-able format literal finding(s).")
    for f in findings:
        print(f"{f.path.name}:{f.line} {f.original} -> {f.suggested}")
        if args.verbose:
            print(f"  {f.stmt}")

    if args.fix:
        total = 0
        changed_files = 0
        by_file: dict[Path, List[Finding]] = {}
        for f in findings:
            by_file.setdefault(f.path, []).append(f)
        for p in sorted(by_file.keys(), key=lambda x: x.name.lower()):
            before = p.read_text(encoding="utf-8")
            n, backup = apply_fix_file(p, by_file[p])
            total += n
            if n > 0:
                changed_files += 1
                print(f"\nFixed {p.name}: rewrites {n}, backup {backup.name if backup else '(none)'}")
                if args.diff:
                    after = p.read_text(encoding="utf-8")
                    diff = difflib.unified_diff(
                        before.splitlines(),
                        after.splitlines(),
                        fromfile=f"a/{p.name}",
                        tofile=f"b/{p.name}",
                        lineterm="",
                    )
                    txt = "\n".join(diff)
                    if txt:
                        print(txt)
            else:
                print(f"\nNo fixes applied to {p.name}")
        print(f"\n--fix summary: files changed {changed_files}, rewrites {total}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
