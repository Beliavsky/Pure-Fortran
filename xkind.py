#!/usr/bin/env python3
"""Warn/fix hard-coded Fortran kind numbers (e.g., _8, kind=8, parameter dp=8)."""

from __future__ import annotations

import argparse
import difflib
import re
import shutil
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Set, Tuple

import cli_paths as cpaths
import fortran_build as fbuild
import fortran_scan as fscan

TYPE_DECL_RE = re.compile(
    r"^\s*(integer|real|logical|character|complex|type\b|class\b|procedure\b)",
    re.IGNORECASE,
)
KIND_NUM_RE = re.compile(r"\bkind\s*=\s*(4|8|16)\b", re.IGNORECASE)
KIND_NAME_RE = re.compile(r"\bkind\s*=\s*([a-z][a-z0-9_]*)\b", re.IGNORECASE)
OLDSTYLE_KIND_RE = re.compile(r"\b(real|integer|complex|logical)\s*\(\s*(4|8|16)\s*\)", re.IGNORECASE)
LIT_KIND_RE = re.compile(
    r"(?<![A-Za-z0-9_])(?P<lit>(?:\d+\.\d*|\d*\.\d+|\d+)(?:[deqDEQ][+-]?\d+)?)_(?P<k>4|8|16)\b",
    re.IGNORECASE,
)
ANNOTATE_TAG = "!! changed by xkind.py"
MODULE_START_RE = re.compile(r"^\s*module\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
END_MODULE_RE = re.compile(r"^\s*end\s*module\b", re.IGNORECASE)


@dataclass
class Finding:
    """One hard-coded kind finding."""

    path: Path
    line: int
    kind: str
    detail: str
    stmt: str


def choose_files(args_files: List[Path], exclude: Iterable[str]) -> List[Path]:
    """Resolve source files from args or current-directory defaults."""
    if args_files:
        files = cpaths.expand_source_inputs(args_files)
    else:
        files = sorted(
            set(Path(".").glob("*.f90")) | set(Path(".").glob("*.F90")),
            key=lambda p: p.name.lower(),
        )
    return fscan.apply_excludes(files, exclude)


def make_backup_path(path: Path) -> Path:
    """Create a non-overwriting backup path next to path."""
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
    """Split one line into code and trailing comment, respecting quotes."""
    in_single = False
    in_double = False
    i = 0
    while i < len(line):
        ch = line[i]
        if ch == "'" and not in_double:
            if in_single and i + 1 < len(line) and line[i + 1] == "'":
                i += 2
                continue
            in_single = not in_single
            i += 1
            continue
        if ch == '"' and not in_single:
            if in_double and i + 1 < len(line) and line[i + 1] == '"':
                i += 2
                continue
            in_double = not in_double
            i += 1
            continue
        if ch == "!" and not in_single and not in_double:
            return line[:i], line[i:]
        i += 1
    return line, ""


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


def real_kind_expr(kn: str) -> str:
    """Portable replacement expression for real kind number."""
    if kn == "4":
        return "kind(1.0)"
    if kn == "8":
        return "kind(1.0d0)"
    return "selected_real_kind(33, 4931)"


def int_kind_expr(kn: str) -> str:
    """Portable replacement expression for integer kind number."""
    if kn == "4":
        return "selected_int_kind(9)"
    if kn == "8":
        return "selected_int_kind(18)"
    return "selected_int_kind(38)"


def infer_decl_family(code: str) -> Optional[str]:
    """Infer declaration family from statement start."""
    m = TYPE_DECL_RE.match(code.strip().lower())
    if not m:
        return None
    t = m.group(1).lower()
    if t in {"real", "complex"}:
        return "real"
    if t in {"integer", "logical"}:
        return "int"
    return None


def kind_expr_for_family(kn: str, fam: Optional[str]) -> Optional[str]:
    """Map kind number to replacement expression by family."""
    if fam == "real":
        return real_kind_expr(kn)
    if fam == "int":
        return int_kind_expr(kn)
    return None


def parse_module_ranges(lines: List[str]) -> List[Tuple[int, int]]:
    """Parse explicit module start/end line ranges (1-based, inclusive)."""
    ranges: List[Tuple[int, int]] = []
    stack: List[int] = []
    for i, raw in enumerate(lines, start=1):
        code, _ = split_code_comment(raw)
        low = code.strip().lower()
        if not low:
            continue
        m = MODULE_START_RE.match(low)
        if m:
            toks = low.split()
            if len(toks) >= 2 and toks[1] != "procedure":
                stack.append(i)
            continue
        if END_MODULE_RE.match(low):
            if stack:
                s = stack.pop()
                ranges.append((s, i))
    ranges.sort()
    return ranges


def in_any_module(line_no: int, ranges: List[Tuple[int, int]]) -> bool:
    """True if line number is inside any parsed module range."""
    for s, e in ranges:
        if s <= line_no <= e:
            return True
    return False


def parse_kind_param_usage(lines: List[str]) -> Dict[str, str]:
    """Infer whether kind parameter names are used in real/int kind selectors."""
    usage: Dict[str, Set[str]] = {}
    for raw in lines:
        code, _ = split_code_comment(raw)
        low = code.lower()
        if not low.strip():
            continue
        for m in KIND_NAME_RE.finditer(low):
            name = m.group(1).lower()
            usage.setdefault(name, set())
            prefix = low[: m.start()]
            fam: Optional[str] = None
            if re.search(r"\b(real|complex)\s*\([^)]*$", prefix):
                fam = "real"
            elif re.search(r"\b(integer|logical)\s*\([^)]*$", prefix):
                fam = "int"
            if fam is not None:
                usage[name].add(fam)
    out: Dict[str, str] = {}
    for n, fams in usage.items():
        if len(fams) == 1:
            out[n] = next(iter(fams))
    return out


def detect_findings(path: Path, lines: List[str], usage: Dict[str, str]) -> List[Finding]:
    """Collect findings from one file."""
    out: List[Finding] = []
    for i, raw in enumerate(lines, start=1):
        code, _ = split_code_comment(raw)
        stmt = code.strip()
        if not stmt:
            continue
        if LIT_KIND_RE.search(code):
            out.append(Finding(path, i, "literal_kind", "literal kind suffix uses 4/8/16", stmt))
        if KIND_NUM_RE.search(code):
            out.append(Finding(path, i, "kind_selector", "kind=4/8/16 uses hard-coded kind", stmt))
        if OLDSTYLE_KIND_RE.search(code):
            out.append(Finding(path, i, "oldstyle_kind", "type(4/8/16) uses hard-coded kind", stmt))

        low = code.lower().strip()
        if TYPE_DECL_RE.match(low) and "::" in low and "parameter" in low:
            lhs, rhs = low.split("::", 1)
            fam = infer_decl_family(lhs)
            for ent in split_top_level_commas(rhs):
                m = re.match(r"^([a-z][a-z0-9_]*)\s*=\s*(4|8|16)\s*$", ent.strip(), re.IGNORECASE)
                if not m:
                    continue
                n = m.group(1).lower()
                if n in usage and (fam is not None or usage.get(n)):
                    out.append(
                        Finding(
                            path,
                            i,
                            "kind_param",
                            f"kind parameter '{n}' initialized with hard-coded kind",
                            stmt,
                        )
                    )
    return out


def rewrite_kind_params(code: str, usage: Dict[str, str]) -> Tuple[str, bool]:
    """Rewrite parameter kind constants like integer, parameter :: dp = 8."""
    low = code.lower()
    if not (TYPE_DECL_RE.match(low.strip()) and "::" in low and "parameter" in low):
        return code, False
    lhs, rhs = code.split("::", 1)
    fam_decl = infer_decl_family(lhs)
    changed = False
    new_chunks: List[str] = []
    for ent in split_top_level_commas(rhs):
        txt = ent.strip()
        m = re.match(r"^([a-z][a-z0-9_]*)\s*=\s*(4|8|16)\s*$", txt, re.IGNORECASE)
        if not m:
            new_chunks.append(txt)
            continue
        name = m.group(1)
        kn = m.group(2)
        fam = usage.get(name.lower(), fam_decl)
        kexpr = kind_expr_for_family(kn, fam)
        if kexpr is None:
            new_chunks.append(txt)
            continue
        new_chunks.append(f"{name} = {kexpr}")
        changed = True
    if not changed:
        return code, False
    return f"{lhs.strip()} :: {', '.join(new_chunks)}", True


def rewrite_kind_selectors(code: str) -> Tuple[str, bool]:
    """Rewrite kind=8 and old-style type(8) selectors on declarations."""
    fam = infer_decl_family(code)
    changed = False
    txt = code

    def repl_kind(m: re.Match[str]) -> str:
        nonlocal changed
        kn = m.group(1)
        kexpr = kind_expr_for_family(kn, fam)
        if kexpr is None:
            return m.group(0)
        changed = True
        return f"kind={kexpr}"

    txt = KIND_NUM_RE.sub(repl_kind, txt)

    def repl_old(m: re.Match[str]) -> str:
        nonlocal changed
        tname = m.group(1).lower()
        kn = m.group(2)
        fam_local = "real" if tname in {"real", "complex"} else "int"
        kexpr = kind_expr_for_family(kn, fam_local)
        if kexpr is None:
            return m.group(0)
        changed = True
        return f"{tname}(kind={kexpr})"

    txt = OLDSTYLE_KIND_RE.sub(repl_old, txt)
    return txt, changed


def rewrite_literals_outside_quotes(code: str) -> Tuple[str, bool]:
    """Rewrite *_4/_8/_16 literal suffixes outside quoted strings."""
    out: List[str] = []
    plain: List[str] = []
    in_single = False
    in_double = False
    changed = False

    def flush_plain() -> None:
        nonlocal changed
        if not plain:
            return
        seg = "".join(plain)

        def repl(m: re.Match[str]) -> str:
            nonlocal changed
            lit = m.group("lit")
            kn = m.group("k")
            is_real = ("." in lit) or bool(re.search(r"[deq]", lit, re.IGNORECASE))
            changed = True
            if is_real:
                # Prefer named-kind literal suffix form.
                return f"{lit}_xkind_r{kn}"
            return f"{lit}_xkind_i{kn}"

        out.append(LIT_KIND_RE.sub(repl, seg))
        plain.clear()

    i = 0
    while i < len(code):
        ch = code[i]
        if ch == "'" and not in_double:
            if in_single and i + 1 < len(code) and code[i + 1] == "'":
                if in_single:
                    out.append("''")
                else:
                    plain.append("''")
                i += 2
                continue
            if in_single:
                out.append(ch)
                in_single = False
            else:
                flush_plain()
                out.append(ch)
                in_single = True
            i += 1
            continue
        if ch == '"' and not in_single:
            if in_double and i + 1 < len(code) and code[i + 1] == '"':
                if in_double:
                    out.append('""')
                else:
                    plain.append('""')
                i += 2
                continue
            if in_double:
                out.append(ch)
                in_double = False
            else:
                flush_plain()
                out.append(ch)
                in_double = True
            i += 1
            continue
        if in_single or in_double:
            out.append(ch)
        else:
            plain.append(ch)
        i += 1
    flush_plain()
    return "".join(out), changed


def rewrite_line(line: str, usage: Dict[str, str], annotate: bool) -> Tuple[str, bool]:
    """Apply one-line rewrite pass; return (new_line, changed)."""
    eol = ""
    body = line
    if body.endswith("\r\n"):
        body = body[:-2]
        eol = "\r\n"
    elif body.endswith("\n"):
        body = body[:-1]
        eol = "\n"

    code, comment = split_code_comment(body)
    changed = False

    c1, ch1 = rewrite_kind_params(code, usage)
    code = c1
    changed = changed or ch1

    c2, ch2 = rewrite_kind_selectors(code)
    code = c2
    changed = changed or ch2

    c3, ch3 = rewrite_literals_outside_quotes(code)
    code = c3
    changed = changed or ch3

    if changed and annotate and ANNOTATE_TAG.lower() not in comment.lower():
        if comment:
            comment = f"{comment}  {ANNOTATE_TAG}"
        else:
            comment = f"  {ANNOTATE_TAG}"

    return f"{code}{comment}{eol}", changed


def apply_fix_file(
    path: Path,
    annotate: bool,
    out_path: Optional[Path] = None,
    create_backup: bool = True,
) -> Tuple[int, Optional[Path]]:
    """Apply fixes in one file."""
    lines = path.read_text(encoding="utf-8").splitlines(keepends=True)
    usage = parse_kind_param_usage(lines)
    module_ranges = parse_module_ranges(lines)
    changed_lines = 0
    new_lines: List[str] = []
    for i, line in enumerate(lines, start=1):
        new_line, changed = rewrite_line(line, usage, annotate)
        # In module code, prefer named kind constants over repeated selected_*_kind expressions.
        if in_any_module(i, module_ranges):
            b = new_line
            body = b
            eol = ""
            if body.endswith("\r\n"):
                body, eol = body[:-2], "\r\n"
            elif body.endswith("\n"):
                body, eol = body[:-1], "\n"
            code, comment = split_code_comment(body)
            code2 = code
            # Do not rewrite xkind_* constant declarations themselves.
            if re.match(
                r"^\s*integer\s*,\s*parameter\s*::\s*xkind_[a-z0-9_]+\s*=",
                code2.strip(),
                re.IGNORECASE,
            ):
                new_lines.append(new_line)
                if changed:
                    changed_lines += 1
                continue
            code2 = re.sub(r"\bselected_int_kind\s*\(\s*9\s*\)", "xkind_i4", code2, flags=re.IGNORECASE)
            code2 = re.sub(r"\bselected_int_kind\s*\(\s*18\s*\)", "xkind_i8", code2, flags=re.IGNORECASE)
            code2 = re.sub(r"\bselected_int_kind\s*\(\s*38\s*\)", "xkind_i16", code2, flags=re.IGNORECASE)
            code2 = re.sub(r"\bkind\s*\(\s*1\.0d0\s*\)", "xkind_r8", code2, flags=re.IGNORECASE)
            code2 = re.sub(r"\bkind\s*\(\s*1\.0\s*\)", "xkind_r4", code2, flags=re.IGNORECASE)
            code2 = re.sub(
                r"\bselected_real_kind\s*\(\s*33\s*,\s*4931\s*\)",
                "xkind_r16",
                code2,
                flags=re.IGNORECASE,
            )
            if code2 != code:
                changed = True
                new_line = f"{code2}{comment}{eol}"
        new_lines.append(new_line)
        if changed:
            changed_lines += 1

    # Insert module-level kind constants where used.
    insertions = 0
    ranges2 = parse_module_ranges(new_lines)
    for s, e in sorted(ranges2, reverse=True):
        used: Set[str] = set()
        declared: Set[str] = set()
        contains_line = e + 1
        implicit_line = -1
        for ln in range(s, e + 1):
            raw = new_lines[ln - 1]
            code, _comment = split_code_comment(raw)
            low = code.strip().lower()
            if not low:
                continue
            if low == "contains" and contains_line > e:
                contains_line = ln
            if low.startswith("implicit none") and implicit_line < 0:
                implicit_line = ln
            if TYPE_DECL_RE.match(low) and "::" in low:
                rhs = low.split("::", 1)[1]
                for ent in split_top_level_commas(rhs):
                    m = re.match(r"^\s*(xkind_[a-z0-9]+)\b", ent.strip(), re.IGNORECASE)
                    if m:
                        declared.add(m.group(1).lower())
            for n in ("xkind_i4", "xkind_i8", "xkind_i16", "xkind_r4", "xkind_r8", "xkind_r16"):
                if re.search(rf"\b{re.escape(n)}\b", code, re.IGNORECASE):
                    used.add(n)
        needed = [n for n in sorted(used) if n not in declared]
        if not needed:
            continue
        insert_at = implicit_line if implicit_line > 0 else s
        ins_idx = insert_at
        ref_raw = new_lines[insert_at - 1] if 1 <= insert_at <= len(new_lines) else ""
        indent = re.match(r"^\s*", ref_raw).group(0) if ref_raw else ""
        eol = "\r\n" if ref_raw.endswith("\r\n") else ("\n" if ref_raw.endswith("\n") else "\n")

        def decl_for(name: str) -> str:
            if name == "xkind_i4":
                return f"{indent}integer, parameter :: xkind_i4 = selected_int_kind(9){eol}"
            if name == "xkind_i8":
                return f"{indent}integer, parameter :: xkind_i8 = selected_int_kind(18){eol}"
            if name == "xkind_i16":
                return f"{indent}integer, parameter :: xkind_i16 = selected_int_kind(38){eol}"
            if name == "xkind_r4":
                return f"{indent}integer, parameter :: xkind_r4 = kind(1.0){eol}"
            if name == "xkind_r8":
                return f"{indent}integer, parameter :: xkind_r8 = kind(1.0d0){eol}"
            return f"{indent}integer, parameter :: xkind_r16 = selected_real_kind(33, 4931){eol}"

        decl_lines = [decl_for(n) for n in needed]
        for off, dl in enumerate(decl_lines, start=1):
            new_lines.insert(ins_idx + off - 1, dl)
        insertions += len(decl_lines)
    changed_lines += insertions
    if changed_lines == 0:
        return 0, None
    backup: Optional[Path] = None
    target = out_path if out_path is not None else path
    if out_path is None and create_backup:
        backup = make_backup_path(path)
        shutil.copy2(path, backup)
    target.write_text("".join(new_lines), encoding="utf-8")
    return changed_lines, backup


def main() -> int:
    """Run hard-coded kind-number advisory/fix pass."""
    parser = argparse.ArgumentParser(description="Warn/fix hard-coded Fortran kind numbers")
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--verbose", action="store_true", help="Print full offending statements")
    parser.add_argument("--fix", action="store_true", help="Apply in-place rewrites")
    parser.add_argument("--out", type=Path, help="With --fix, write transformed output to this file (single input)")
    parser.add_argument("--out-dir", type=Path, help="With --fix, write outputs to this directory")
    parser.add_argument("--backup", dest="backup", action="store_true", default=True)
    parser.add_argument("--no-backup", dest="backup", action="store_false")
    parser.add_argument("--annotate", action="store_true", help="With --fix, append change tag comments")
    parser.add_argument("--diff", action="store_true", help="With --fix, print unified diffs")
    parser.add_argument("--compiler", type=str, help="Compile command for baseline/after-fix validation")
    args = parser.parse_args()
    if args.out is not None and args.out_dir is not None:
        print("--out and --out-dir are mutually exclusive.")
        return 2
    if args.out is not None or args.out_dir is not None:
        args.fix = True

    if args.annotate and not args.fix:
        print("--annotate requires --fix.")
        return 2
    if args.diff and not args.fix:
        print("--diff requires --fix.")
        return 2
    if args.compiler and not args.fix:
        print("--compiler requires --fix.")
        return 2

    files = choose_files(args.fortran_files, args.exclude)
    if not files:
        print("No source files remain after applying --exclude filters.")
        return 2
    if args.out_dir is not None:
        if args.out_dir.exists() and not args.out_dir.is_dir():
            print("--out-dir exists but is not a directory.")
            return 2
        args.out_dir.mkdir(parents=True, exist_ok=True)
    if args.out is not None and len(files) != 1:
        print("--out requires exactly one input source file.")
        return 2
    if args.fix and args.out_dir is not None:
        for p in files:
            (args.out_dir / p.name).write_text(p.read_text(encoding="utf-8"), encoding="utf-8")
    compile_paths = (
        [args.out]
        if (args.fix and args.out is not None)
        else ([args.out_dir / p.name for p in files] if (args.fix and args.out_dir is not None) else files)
    )
    if args.fix and args.compiler:
        if not fbuild.run_compiler_command(args.compiler, compile_paths, "baseline", fscan.display_path):
            return 5

    findings: List[Finding] = []
    for p in files:
        if not p.exists():
            print(f"File not found: {p}")
            continue
        lines = p.read_text(encoding="utf-8").splitlines()
        usage = parse_kind_param_usage(lines)
        findings.extend(detect_findings(p, lines, usage))

    if not findings:
        print("No hard-coded kind-number findings.")
    else:
        findings.sort(key=lambda f: (f.path.name.lower(), f.line, f.kind))
        print(f"{len(findings)} hard-coded kind-number finding(s).")
        for f in findings:
            print(f"{f.path.name}:{f.line} {f.kind} - {f.detail}")
            if args.verbose:
                print(f"  {f.stmt}")

    if not args.fix:
        return 0

    changed_files = 0
    total_changed_lines = 0
    for p in files:
        if not p.exists():
            continue
        before = p.read_text(encoding="utf-8")
        out_path = args.out if args.out is not None else (args.out_dir / p.name if args.out_dir is not None else None)
        n_changed, backup = apply_fix_file(
            p, annotate=args.annotate, out_path=out_path, create_backup=args.backup
        )
        if n_changed == 0:
            continue
        changed_files += 1
        total_changed_lines += n_changed
        if out_path is not None:
            print(f"\nFixed {p.name}: changed {n_changed} line(s), wrote {out_path}")
        else:
            print(f"\nFixed {p.name}: changed {n_changed} line(s), backup {backup.name if backup else '(none)'}")
        if args.diff:
            after = (out_path if out_path is not None else p).read_text(encoding="utf-8")
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

    print(f"\n--fix summary: files changed {changed_files}, lines changed {total_changed_lines}")
    if args.fix and args.compiler:
        if not fbuild.run_compiler_command(args.compiler, compile_paths, "after-fix", fscan.display_path):
            return 5
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
