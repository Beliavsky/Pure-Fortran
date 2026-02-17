#!/usr/bin/env python3
"""Suggest/apply initialization of local variables to sentinel/default values."""

from __future__ import annotations

import argparse
import re
import subprocess
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Optional, Set, Tuple

import cli_paths as cpaths
import xunset


UNIT_START_RE = re.compile(r"^\s*(program|subroutine|function)\b", re.IGNORECASE)
UNIT_START_NAME_RE = re.compile(r"^\s*(program|subroutine|function)\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
UNIT_END_RE = re.compile(
    r"^\s*end\s*$|^\s*end\s+(?:program|subroutine|function)(?:\s+[a-z][a-z0-9_]*)?\s*$",
    re.IGNORECASE,
)
INTERFACE_START_RE = re.compile(r"^\s*(abstract\s+)?interface\b", re.IGNORECASE)
END_INTERFACE_RE = re.compile(r"^\s*end\s+interface\b", re.IGNORECASE)
DECL_HEAD_RE = re.compile(
    r"^\s*(integer|real|character|logical|complex|double\s+precision|type\b|class\b|procedure\b)\b",
    re.IGNORECASE,
)
SPEC_STMT_RE = re.compile(
    r"^\s*(parameter|dimension|common|save|equivalence|external|intrinsic|namelist|data|public|private)\b",
    re.IGNORECASE,
)
EXTERNAL_STMT_RE = re.compile(r"^\s*external\b(.*)$", re.IGNORECASE)


def strip_comment(line: str) -> str:
    in_single = False
    in_double = False
    for i, ch in enumerate(line):
        if ch == "'" and not in_double:
            in_single = not in_single
        elif ch == '"' and not in_single:
            in_double = not in_double
        elif ch == "!" and not in_single and not in_double:
            return line[:i]
    return line


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


def parse_header_args(code: str) -> Set[str]:
    m = re.match(
        r"^\s*(?:subroutine|function|program)\s+[a-z][a-z0-9_]*\s*\(([^)]*)\)",
        code,
        re.IGNORECASE,
    )
    if not m:
        return set()
    return {a.strip().lower() for a in split_top_level_commas(m.group(1)) if a.strip()}


def parse_decl_line(code: str) -> Optional[Tuple[str, List[str]]]:
    if "::" not in code:
        return None
    lhs, rhs = code.split("::", 1)
    ents = [e for e in split_top_level_commas(rhs) if e.strip()]
    if not ents:
        return None
    return lhs.strip(), [e.strip() for e in ents]


def entity_name(ent: str) -> Optional[str]:
    m = re.match(r"^\s*([a-z][a-z0-9_]*)", ent, re.IGNORECASE)
    return m.group(1).lower() if m else None


def infer_type(spec: str) -> Optional[str]:
    s = spec.strip().lower()
    if s.startswith("integer"):
        return "integer"
    if s.startswith("real") or s.startswith("double precision"):
        return "real"
    if s.startswith("character"):
        return "character"
    return None


@dataclass
class InitCandidate:
    name: str
    vtype: str
    line_no: int
    unit_name: str


@dataclass
class UnitState:
    start: int
    unit_name: str
    header_args: Set[str]
    in_decl: bool
    insertion_idx: Optional[int]
    indent: str
    use_insert_idx: int
    has_ieee_use: bool
    use_indent: str
    decl_cont: bool
    external_names: Set[str]
    candidates: List[InitCandidate]
    seen: Set[str]


def build_init_line(indent: str, vtype: str, name: str, real_init: str, int_init: str, char_init: str, eol: str) -> str:
    if vtype == "real":
        rhs = real_init
    elif vtype == "integer":
        rhs = int_init
    else:
        rhs = char_init
    return f"{indent}{name} = {rhs} ! added by xinit.py{eol}"


def analyze_and_fix_text(
    lines: List[str],
    fix: bool,
    real_init: str,
    int_init: str,
    char_init: str,
    uncertain_only: bool = False,
    uncertain_vars: Optional[Set[Tuple[str, str]]] = None,
) -> Tuple[List[InitCandidate], List[str]]:
    candidates: List[InitCandidate] = []
    out_lines = list(lines)
    interface_depth = 0
    unit_stack: List[UnitState] = []
    pending_inserts: Dict[int, List[str]] = {}
    needs_ieee = ("ieee_value" in real_init.lower()) or ("ieee_quiet_nan" in real_init.lower())

    for i, raw in enumerate(lines):
        code = strip_comment(raw).strip().lstrip("\ufeff")
        code_nc = strip_comment(raw).rstrip("\r\n")
        low = code.lower()
        if INTERFACE_START_RE.match(low):
            interface_depth += 1
        elif END_INTERFACE_RE.match(low):
            interface_depth = max(0, interface_depth - 1)

        if interface_depth == 0 and UNIT_START_RE.match(low):
            mm_name = UNIT_START_NAME_RE.match(low)
            if mm_name:
                unit_name = mm_name.group(2).lower()
            elif low.startswith("program"):
                unit_name = "main"
            else:
                unit_name = "<unnamed>"
            indent = "   "
            unit_stack.append(
                UnitState(
                    start=i,
                    unit_name=unit_name,
                    header_args=parse_header_args(low),
                    in_decl=True,
                    insertion_idx=None,
                    indent=indent,
                    use_insert_idx=i + 1,
                    has_ieee_use=False,
                    use_indent="",
                    decl_cont=False,
                    external_names=set(),
                    candidates=[],
                    seen=set(),
                )
            )
            continue

        if not unit_stack:
            continue
        u = unit_stack[-1]

        if UNIT_END_RE.match(low):
            ins = u.insertion_idx if u.insertion_idx is not None else i
            kept_candidates = [c for c in u.candidates if c.name not in u.external_names]
            if needs_ieee and (not u.has_ieee_use) and any(c.vtype == "real" for c in kept_candidates):
                eol = "\n"
                if raw.endswith("\r\n"):
                    eol = "\r\n"
                ui = u.use_insert_idx
                pending_inserts.setdefault(ui, [])
                uind = u.use_indent if u.use_indent else u.indent
                pending_inserts[ui].append(
                    f"{uind}use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan ! added by xinit.py{eol}"
                )
            if kept_candidates:
                eol = "\n"
                if raw.endswith("\r\n"):
                    eol = "\r\n"
                pending_inserts.setdefault(ins, [])
                for c in kept_candidates:
                    pending_inserts[ins].append(
                        build_init_line(u.indent, c.vtype, c.name, real_init, int_init, char_init, eol)
                    )
                    candidates.append(c)
            unit_stack.pop()
            continue

        if not low:
            continue

        if u.in_decl:
            if u.decl_cont:
                u.decl_cont = code_nc.rstrip().endswith("&")
                continue
            if low.startswith("use ") or low.startswith("implicit "):
                if low.startswith("use "):
                    u.use_insert_idx = i + 1
                    if "ieee_arithmetic" in low:
                        u.has_ieee_use = True
                continue
            if SPEC_STMT_RE.match(low):
                mx = EXTERNAL_STMT_RE.match(code)
                if mx:
                    rhs = mx.group(1).strip()
                    if rhs.startswith("::"):
                        rhs = rhs[2:].strip()
                    for chunk in split_top_level_commas(rhs):
                        n = entity_name(chunk.strip())
                        if n:
                            u.external_names.add(n)
                u.decl_cont = code_nc.rstrip().endswith("&")
                continue
            if DECL_HEAD_RE.match(low):
                u.decl_cont = code_nc.rstrip().endswith("&")
                parsed = parse_decl_line(code)
                if parsed is None:
                    continue
                spec, ents = parsed
                vtype = infer_type(spec)
                if vtype is None:
                    continue
                sl = spec.lower()
                if "external" in sl:
                    for ent in ents:
                        n = entity_name(ent)
                        if n:
                            u.external_names.add(n)
                    continue
                has_alloc = ("allocatable" in sl) or ("pointer" in sl)
                has_param = "parameter" in sl
                is_intent_out = re.search(r"\bintent\s*\(\s*out\s*\)", sl, re.IGNORECASE) is not None
                is_intent_in = re.search(r"\bintent\s*\(\s*in\s*\)", sl, re.IGNORECASE) is not None
                for ent in ents:
                    n = entity_name(ent)
                    if not n or n in u.seen:
                        continue
                    if n in u.external_names:
                        continue
                    if "=" in ent:
                        continue
                    is_dummy = n in u.header_args
                    if has_param:
                        continue
                    if is_dummy and not is_intent_out:
                        continue
                    if is_dummy and is_intent_in:
                        continue
                    if has_alloc:
                        continue
                    if uncertain_only:
                        uv = uncertain_vars or set()
                        if (u.unit_name, n) not in uv:
                            continue
                    u.seen.add(n)
                    u.candidates.append(InitCandidate(name=n, vtype=vtype, line_no=i + 1, unit_name=u.unit_name))
                continue
            u.in_decl = False
            u.insertion_idx = i
            m_indent = re.match(r"^\s*", raw)
            # Keep inserted lines unindented by default to match common free-form style.
            u.indent = ""
            if not u.use_indent:
                u.use_indent = ""

    if not fix:
        return candidates, lines

    # apply insertions from bottom to top
    for idx in sorted(pending_inserts.keys(), reverse=True):
        out_lines[idx:idx] = pending_inserts[idx]
    return candidates, out_lines


def run_compile(cmd_template: str, label: str, path: Path) -> bool:
    cmd = cmd_template.format(file=str(path), files=str(path))
    print(f"Compile ({label}): {cmd}")
    cp = subprocess.run(cmd, shell=True, capture_output=True, text=True)
    if cp.returncode == 0:
        print(f"Compile ({label}): PASS")
        return True
    print(f"Compile ({label}): FAIL (exit {cp.returncode})")
    if cp.stdout:
        print(cp.stdout.rstrip())
    if cp.stderr:
        print(cp.stderr.rstrip())
    return False


def main() -> int:
    p = argparse.ArgumentParser(description="Suggest/apply initialization assignments for local variables.")
    p.add_argument("files", nargs="*", type=Path)
    p.add_argument("--fix", action="store_true", help="Apply initialization edits")
    p.add_argument("--out", type=Path, help="With --fix and one input file, write transformed output to this path")
    p.add_argument("--out-dir", type=Path, help="With --fix, write each transformed file to this directory")
    p.add_argument("--compiler", type=str, help="Compile command for baseline/after-fix validation")
    p.add_argument(
        "--real-init",
        type=str,
        default="ieee_value(0.0, ieee_quiet_nan)",
        help='Initializer expression for REAL variables (default: "ieee_value(0.0, ieee_quiet_nan)")',
    )
    p.add_argument("--int-init", type=str, default="-999", help='Initializer expression for INTEGER variables (default: "-999")')
    p.add_argument("--char-init", type=str, default='"?"', help='Initializer expression for CHARACTER variables (default: "\\"?\\"")')
    p.add_argument("--verbose", action="store_true")
    p.add_argument("--uncertain", action="store_true", help="Initialize only variables with uncertain use-before-set findings from xunset")
    args = p.parse_args()
    if args.out is not None or args.out_dir is not None:
        args.fix = True

    if args.files:
        files = cpaths.expand_path_args(args.files)
    else:
        files = sorted(set(Path(".").glob("*.f90")) | set(Path(".").glob("*.F90")), key=lambda p: p.name.lower())
    if not files:
        print("No input files found.")
        return 1
    if args.out is not None and args.out_dir is not None:
        print("Use only one of --out or --out-dir.")
        return 2
    if args.out is not None and len(files) != 1:
        print("--out requires exactly one input file.")
        return 2
    if args.out_dir is not None:
        if args.out_dir.exists() and not args.out_dir.is_dir():
            print("--out-dir exists but is not a directory.")
            return 2
        args.out_dir.mkdir(parents=True, exist_ok=True)
    if args.compiler and not args.fix:
        print("--compiler requires --fix.")
        return 2

    total = 0
    changed = 0
    for src in files:
        if not src.exists():
            print(f"Missing file: {src}")
            continue
        raw = src.read_text(encoding="utf-8-sig", errors="ignore").splitlines(keepends=True)
        uncertain_set: Set[Tuple[str, str]] = set()
        if args.uncertain:
            issues = xunset.analyze_paths([src])
            for iss in issues:
                d = iss.detail.lower()
                if "may" in d:
                    uncertain_set.add((iss.unit_name.lower(), iss.var_name.lower()))
        cands, updated = analyze_and_fix_text(
            raw,
            args.fix,
            args.real_init,
            args.int_init,
            args.char_init,
            uncertain_only=args.uncertain,
            uncertain_vars=uncertain_set,
        )
        if not cands:
            if args.verbose:
                print(f"{src}: no initialization candidates")
            if args.fix and args.out is not None:
                args.out.write_text("".join(raw), encoding="utf-8", newline="")
            continue
        total += len(cands)
        if args.verbose:
            for c in cands:
                print(f"{src}:{c.line_no} {c.unit_name} {c.name} [{c.vtype}]")
        if not args.fix:
            continue

        if args.compiler and not run_compile(args.compiler, "baseline", src):
            print("Baseline compile failed. Stopping without edits.")
            return 5

        if args.out is not None:
            dst = args.out
        elif args.out_dir is not None:
            dst = args.out_dir / src.name
        else:
            dst = src
        dst.write_text("".join(updated), encoding="utf-8", newline="")
        changed += 1
        if args.verbose:
            print(f"Fixed {src}: added {len(cands)} init assignment(s), wrote {dst}")

        if args.compiler and not run_compile(args.compiler, "after-fix", dst):
            return 5

    if args.fix:
        print(f"--fix summary: files changed {changed}, init assignments added {total}")
    else:
        if total == 0:
            print("No initialization candidates found.")
        else:
            print(f"{total} initialization candidate(s) found in {len(files)} file(s).")
            if not args.verbose:
                print("Run with --verbose for details.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
