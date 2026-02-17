#!/usr/bin/env python3
"""Advisory checker for likely PRINT/WRITE format vs argument type mismatches."""

from __future__ import annotations

import argparse
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Set, Tuple

import cli_paths as cpaths
import fortran_scan as fscan

PROGRAM_START_RE = re.compile(r"^\s*program\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
PROC_START_RE = re.compile(
    r"^\s*(?:(?:pure|elemental|impure|recursive|module)\s+)*(function|subroutine)\s+([a-z][a-z0-9_]*)\b",
    re.IGNORECASE,
)
TYPE_DECL_RE = re.compile(
    r"^\s*(integer|real|logical|character|complex|double\s+precision|type\b|class\b|procedure\b)",
    re.IGNORECASE,
)
NO_COLON_DECL_RE = re.compile(
    r"^\s*(?P<spec>(?:integer|real|logical|complex|character|double\s+precision)\s*(?:\([^)]*\))?"
    r"|type\s*\([^)]*\)|class\s*\([^)]*\))\s+(?P<rhs>.+)$",
    re.IGNORECASE,
)
IDENT_RE = re.compile(r"\b([a-z][a-z0-9_]*)\b", re.IGNORECASE)
ALLOCATE_RE = re.compile(r"^\s*allocate\s*\((.*)\)\s*$", re.IGNORECASE)


@dataclass
class Unit:
    path: Path
    kind: str
    name: str
    body: List[Tuple[int, str]]


@dataclass
class Finding:
    path: Path
    unit_kind: str
    unit_name: str
    line: int
    certainty: str  # definite|possible
    detail: str


def choose_files(args_files: List[Path], exclude: Iterable[str]) -> List[Path]:
    if args_files:
        files = cpaths.expand_path_args(args_files)
    else:
        files = sorted(set(Path(".").glob("*.f90")) | set(Path(".").glob("*.F90")), key=lambda p: p.name.lower())
    return fscan.apply_excludes(files, exclude)


def split_top_level_commas(text: str) -> List[str]:
    out: List[str] = []
    cur: List[str] = []
    depth = 0
    sq_depth = 0
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
            elif ch == "[":
                sq_depth += 1
            elif ch == "]" and sq_depth > 0:
                sq_depth -= 1
            elif ch == "," and depth == 0 and sq_depth == 0:
                out.append("".join(cur).strip())
                cur = []
                continue
        cur.append(ch)
    tail = "".join(cur).strip()
    if tail:
        out.append(tail)
    return out


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


def parse_program_units(finfo: fscan.SourceFileInfo) -> List[Unit]:
    units: List[Unit] = []
    in_program = False
    name = "main"
    body: List[Tuple[int, str]] = []
    proc_depth = 0

    for ln, stmt in fscan.iter_fortran_statements(finfo.parsed_lines):
        low = strip_comment(stmt).strip().lower()
        if not low:
            continue
        m_prog = PROGRAM_START_RE.match(low)
        if m_prog and not in_program:
            in_program = True
            name = m_prog.group(1).lower()
            body = []
            proc_depth = 0
            continue
        if not in_program:
            continue

        if PROC_START_RE.match(low):
            proc_depth += 1
        elif low.startswith("end"):
            toks = low.split()
            if len(toks) == 1 or (len(toks) >= 2 and toks[1] in {"function", "subroutine"}):
                if proc_depth > 0:
                    proc_depth -= 1
            if ((len(toks) >= 2 and toks[1] == "program") or len(toks) == 1) and proc_depth == 0:
                units.append(Unit(path=finfo.path, kind="program", name=name, body=body))
                in_program = False
                continue

        if proc_depth == 0:
            body.append((ln, stmt))
    return units


def parse_implicit_main_unit(finfo: fscan.SourceFileInfo) -> Optional[Unit]:
    """Parse top-level statements as implicit main when no explicit PROGRAM exists."""
    if parse_program_units(finfo):
        return None

    body: List[Tuple[int, str]] = []
    module_depth = 0
    proc_depth = 0
    start_line = -1

    for ln, stmt in fscan.iter_fortran_statements(finfo.parsed_lines):
        low = strip_comment(stmt).strip().lower()
        if not low:
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
            if len(toks) == 1 and proc_depth > 0:
                proc_depth -= 1
                continue
        if module_depth > 0:
            if PROC_START_RE.match(low):
                proc_depth += 1
            continue
        if PROC_START_RE.match(low):
            proc_depth += 1
            continue
        if proc_depth > 0:
            continue
        if start_line < 0:
            start_line = ln
        body.append((ln, stmt))

    if not body:
        return None
    return Unit(path=finfo.path, kind="program", name="main", body=body)


def collect_units(finfo: fscan.SourceFileInfo) -> List[Unit]:
    out: List[Unit] = []
    for p in finfo.procedures:
        out.append(Unit(path=finfo.path, kind=p.kind.lower(), name=p.name.lower(), body=p.body))
    out.extend(parse_program_units(finfo))
    imp = parse_implicit_main_unit(finfo)
    if imp is not None:
        out.append(imp)
    return out


def parse_decl_entities(stmt: str) -> List[str]:
    rhs = ""
    if "::" in stmt:
        rhs = stmt.split("::", 1)[1]
    else:
        m = NO_COLON_DECL_RE.match(stmt.strip())
        if not m:
            return []
        rhs = m.group("rhs")
    out: List[str] = []
    for chunk in split_top_level_commas(rhs):
        m = re.match(r"^\s*([a-z][a-z0-9_]*)", chunk, re.IGNORECASE)
        if m:
            out.append(m.group(1).lower())
    return out


def parse_decl_constant_sizes(stmt: str) -> Dict[str, int]:
    """Parse declaration entities with constant extents, returning total element counts."""
    rhs = ""
    if "::" in stmt:
        rhs = stmt.split("::", 1)[1]
    else:
        m = NO_COLON_DECL_RE.match(stmt.strip())
        if not m:
            return {}
        rhs = m.group("rhs")
    out: Dict[str, int] = {}
    for chunk in split_top_level_commas(rhs):
        t = chunk.strip()
        if not t:
            continue
        if "=" in t and "=>" not in t:
            t = t.split("=", 1)[0].strip()
        m = re.match(r"^([a-z][a-z0-9_]*)\s*\(([^)]*)\)\s*$", t, re.IGNORECASE)
        if not m:
            continue
        name = m.group(1).lower()
        dims = [d.strip() for d in split_top_level_commas(m.group(2)) if d.strip()]
        if not dims:
            continue
        n = 1
        ok = True
        for d in dims:
            md = re.match(r"^[+-]?\d+$", d)
            if md:
                v = int(d)
                if v < 0:
                    ok = False
                    break
                n *= v
                continue
            if ":" in d:
                a, b = [x.strip() for x in d.split(":", 1)]
                ma = re.match(r"^[+-]?\d+$", a)
                mb = re.match(r"^[+-]?\d+$", b)
                if not ma or not mb:
                    ok = False
                    break
                lo = int(a)
                hi = int(b)
                if hi < lo:
                    ok = False
                    break
                n *= (hi - lo + 1)
                continue
            ok = False
            break
        if ok:
            out[name] = n
    return out


def type_kind_from_decl_prefix(code: str) -> Optional[str]:
    low = code.strip().lower()
    if low.startswith("double precision"):
        return "real"
    if low.startswith("integer"):
        return "integer"
    if low.startswith("real"):
        return "real"
    if low.startswith("logical"):
        return "logical"
    if low.startswith("character"):
        return "character"
    if low.startswith("complex"):
        return "complex"
    return None


def parse_named_format_constants(unit: Unit) -> Dict[str, str]:
    out: Dict[str, str] = {}
    for _ln, stmt in unit.body:
        code = strip_comment(stmt).strip()
        low = code.lower()
        if not TYPE_DECL_RE.match(low):
            continue
        if "parameter" not in low or "character" not in low:
            continue
        rhs = ""
        if "::" in code:
            rhs = code.split("::", 1)[1]
        else:
            m = NO_COLON_DECL_RE.match(code)
            if not m:
                continue
            rhs = m.group("rhs")
        for chunk in split_top_level_commas(rhs):
            m = re.match(r"^\s*([a-z][a-z0-9_]*)\s*=\s*(.+)$", chunk, re.IGNORECASE)
            if not m:
                continue
            name = m.group(1).lower()
            val = m.group(2).strip()
            if (val.startswith("'") and val.endswith("'")) or (val.startswith('"') and val.endswith('"')):
                out[name] = val[1:-1]
    return out


def build_local_types(unit: Unit) -> Dict[str, str]:
    out: Dict[str, str] = {}
    for _ln, stmt in unit.body:
        code = strip_comment(stmt).strip()
        low = code.lower()
        if not TYPE_DECL_RE.match(low):
            continue
        kind = type_kind_from_decl_prefix(low)
        if not kind:
            continue
        for n in parse_decl_entities(code):
            out.setdefault(n, kind)
    return out


def build_local_sizes(unit: Unit) -> Dict[str, int]:
    """Build local declaration constant total element sizes for arrays."""
    out: Dict[str, int] = {}
    for _ln, stmt in unit.body:
        code = strip_comment(stmt).strip()
        low = code.lower()
        if not TYPE_DECL_RE.match(low):
            continue
        out.update(parse_decl_constant_sizes(code))
    return out


def infer_size_from_alloc_target(chunk: str) -> Optional[Tuple[str, int]]:
    """Infer total element count from one allocate target like k(2,3)."""
    t = chunk.strip()
    m = re.match(r"^([a-z][a-z0-9_]*)\s*\(([^)]*)\)\s*$", t, re.IGNORECASE)
    if not m:
        return None
    name = m.group(1).lower()
    dims = [d.strip() for d in split_top_level_commas(m.group(2)) if d.strip()]
    if not dims:
        return None
    total = 1
    for d in dims:
        if re.fullmatch(r"[+-]?\d+", d):
            v = int(d)
            if v < 0:
                return None
            total *= v
        elif ":" in d:
            a, b = [x.strip() for x in d.split(":", 1)]
            if not re.fullmatch(r"[+-]?\d+", a) or not re.fullmatch(r"[+-]?\d+", b):
                return None
            lo = int(a)
            hi = int(b)
            if hi < lo:
                return None
            total *= (hi - lo + 1)
        else:
            return None
    return name, total


def parse_print_stmt(stmt: str) -> Optional[Tuple[str, str]]:
    s = strip_comment(stmt).strip()
    low = s.lower()
    if not low.startswith("print"):
        return None
    rest = s[5:].strip()
    if not rest:
        return None
    depth = 0
    in_single = False
    in_double = False
    for i, ch in enumerate(rest):
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
                fmt = rest[:i].strip()
                iolist = rest[i + 1 :].strip()
                return fmt, iolist
    return None


def parse_write_stmt(stmt: str) -> Optional[Tuple[str, str]]:
    s = strip_comment(stmt).strip()
    low = s.lower()
    if not low.startswith("write"):
        return None
    m = re.match(r"^write\s*\(", low)
    if not m:
        return None
    i = s.lower().find("(")
    depth = 1
    j = i + 1
    in_single = False
    in_double = False
    while j < len(s):
        ch = s[j]
        if ch == "'" and not in_double:
            in_single = not in_single
        elif ch == '"' and not in_single:
            in_double = not in_double
        elif not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
                if depth == 0:
                    break
        j += 1
    if j >= len(s):
        return None
    ctrl = s[i + 1 : j].strip()
    rest = s[j + 1 :].strip()
    if rest.startswith(","):
        rest = rest[1:].strip()
    parts = split_top_level_commas(ctrl)
    fmt_expr = ""
    for p in parts:
        t = p.strip()
        if "=" in t and "=>" not in t:
            k, v = t.split("=", 1)
            if k.strip().lower() == "fmt":
                fmt_expr = v.strip()
                break
    if not fmt_expr and len(parts) >= 2:
        fmt_expr = parts[1].strip()
    if not fmt_expr:
        return None
    return fmt_expr, rest


def normalize_format_expr(fmt_expr: str, named_fmts: Dict[str, str]) -> Optional[str]:
    t = fmt_expr.strip()
    if t == "*":
        return None
    if (t.startswith("'") and t.endswith("'")) or (t.startswith('"') and t.endswith('"')):
        return t[1:-1]
    name = t.lower()
    if name in named_fmts:
        return named_fmts[name]
    return None


def parse_int(text: str, pos: int) -> Tuple[Optional[int], int]:
    j = pos
    while j < len(text) and text[j].isdigit():
        j += 1
    if j == pos:
        return None, pos
    return int(text[pos:j]), j


def skip_quoted(text: str, pos: int) -> int:
    q = text[pos]
    j = pos + 1
    while j < len(text):
        if text[j] == q:
            # doubled quote escape
            if j + 1 < len(text) and text[j + 1] == q:
                j += 2
                continue
            return j + 1
        j += 1
    return j


def parse_format_descriptors(fmt: str, cap: int = 200) -> List[str]:
    """Return descriptor classes consumed per data item: i|r|l|a|n|u."""

    def parse_group(s: str, pos: int, end_char: Optional[str]) -> Tuple[List[str], int]:
        out: List[str] = []
        i = pos
        while i < len(s):
            while i < len(s) and s[i] in " \t,":
                i += 1
            if i >= len(s):
                break
            if end_char is not None and s[i] == end_char:
                return out, i + 1
            if s[i] in "'\"":
                i = skip_quoted(s, i)
                continue
            rep, i2 = parse_int(s, i)
            i = i2
            r = rep if rep is not None else 1
            if i < len(s) and s[i] == "(":
                sub, j = parse_group(s, i + 1, ")")
                for _ in range(r):
                    out.extend(sub)
                    if len(out) >= cap:
                        return out[:cap], j
                i = j
                continue
            if i >= len(s):
                break
            ch = s[i]
            if ch == "/" or ch == ":":
                i += 1
                continue
            if ch in "xX":
                i += 1
                continue

            j = i
            while j < len(s) and s[j].isalpha():
                j += 1
            token = s[i:j].lower()
            if not token:
                i += 1
                continue

            if token in {"i", "o", "z", "b"}:
                d = "i"
            elif token in {"f", "e", "d", "en", "es"}:
                d = "r"
            elif token in {"l"}:
                d = "l"
            elif token in {"a"}:
                d = "a"
            elif token in {"g"}:
                d = "n"
            elif token in {"t", "tl", "tr", "p", "sp", "ss", "s", "bn", "bz", "ru", "rd", "rz", "rn", "rc", "rp", "dt"}:
                d = "u"
            else:
                d = "u"

            for _ in range(r):
                if d != "u":
                    out.append(d)
                    if len(out) >= cap:
                        return out[:cap], j
            i = j
            while i < len(s) and s[i] not in ",()'\"/":
                i += 1
        return out, i

    def extract_unlimited_repeat_group(s: str) -> Optional[str]:
        in_single = False
        in_double = False
        depth = 0
        i = 0
        while i < len(s):
            ch = s[i]
            if ch == "'" and not in_double:
                in_single = not in_single
                i += 1
                continue
            if ch == '"' and not in_single:
                in_double = not in_double
                i += 1
                continue
            if in_single or in_double:
                i += 1
                continue
            if ch == "(":
                depth += 1
                i += 1
                continue
            if ch == ")" and depth > 0:
                depth -= 1
                i += 1
                continue
            if ch == "*" and depth >= 1:
                j = i + 1
                while j < len(s) and s[j].isspace():
                    j += 1
                if j < len(s) and s[j] == "(":
                    k = j + 1
                    d2 = 1
                    while k < len(s):
                        if s[k] == "(":
                            d2 += 1
                        elif s[k] == ")":
                            d2 -= 1
                            if d2 == 0:
                                return s[j : k + 1]
                        k += 1
            i += 1
        return None

    text = fmt.strip()
    ulg = extract_unlimited_repeat_group(text)
    if ulg is not None:
        # Unlimited repeat: conservatively treat only this group as the effective repeated pattern.
        text = ulg
    if text.startswith("(") and text.endswith(")"):
        desc, _ = parse_group(text, 1, ")")
    else:
        desc, _ = parse_group(text, 0, None)
    return desc


def classify_expr(expr: str, types: Dict[str, str]) -> str:
    t = expr.strip()
    if not t:
        return "unknown"
    if (t.startswith("'") and t.endswith("'")) or (t.startswith('"') and t.endswith('"')):
        return "character"
    tl = t.lower()
    if tl in {".true.", ".false."}:
        return "logical"
    if re.fullmatch(r"[+-]?\d+", t):
        return "integer"
    if re.fullmatch(r"[+-]?(?:\d+\.\d*|\d*\.\d+)(?:[deDE][+-]?\d+)?(?:_[a-zA-Z0-9_]+)?", t) or re.fullmatch(r"[+-]?\d+[deDE][+-]?\d+(?:_[a-zA-Z0-9_]+)?", t):
        return "real"
    m = re.fullmatch(r"([a-z][a-z0-9_]*)\s*(?:\(.*\))?", tl)
    if m:
        n = m.group(1)
        return types.get(n, "unknown")
    return "unknown"


def expand_iolist_items(iolist: str) -> List[str]:
    items = split_top_level_commas(iolist)
    out: List[str] = []
    for it in items:
        t = it.strip()
        if not t:
            continue
        # implied-do: (expr, i=..., ...)
        if t.startswith("(") and t.endswith(")"):
            inner = t[1:-1].strip()
            parts = split_top_level_commas(inner)
            ctl_idx = -1
            for idx, p in enumerate(parts):
                if re.match(r"^[a-z][a-z0-9_]*\s*=", p.strip(), re.IGNORECASE):
                    ctl_idx = idx
                    break
            # Valid implied-do control requires at least (idx=start,end).
            if ctl_idx >= 1 and (len(parts) - ctl_idx) >= 2:
                for dp in parts[:ctl_idx]:
                    dpt = dp.strip()
                    if dpt:
                        out.append(dpt)
                continue
        out.append(t)
    return out


def iolist_arg_kinds(iolist: str, types: Dict[str, str], sizes: Dict[str, int]) -> List[str]:
    """Expand I/O list into argument kinds, expanding simple whole-array variables by constant size."""
    kinds: List[str] = []
    for it in expand_iolist_items(iolist):
        t = it.strip()
        m = re.fullmatch(r"([a-z][a-z0-9_]*)", t, re.IGNORECASE)
        if m:
            n = m.group(1).lower()
            k = types.get(n, "unknown")
            cnt = sizes.get(n, 1)
            for _ in range(cnt):
                kinds.append(k)
            continue
        kinds.append(classify_expr(t, types))
    return kinds


def descriptor_matches(desc: str, arg_kind: str) -> Optional[bool]:
    if arg_kind == "unknown":
        return None
    if desc == "i":
        return arg_kind == "integer"
    if desc == "r":
        return arg_kind == "real"
    if desc == "l":
        return arg_kind == "logical"
    if desc == "a":
        return arg_kind == "character"
    if desc == "n":
        return arg_kind in {"integer", "real", "complex"}
    return None


def analyze_unit(unit: Unit) -> List[Finding]:
    findings: List[Finding] = []
    types = build_local_types(unit)
    sizes = build_local_sizes(unit)
    named_fmts = parse_named_format_constants(unit)

    for ln, stmt in unit.body:
        code = strip_comment(stmt).strip()
        low = code.lower()
        if not low:
            continue

        m_alloc = ALLOCATE_RE.match(low)
        if m_alloc:
            for chunk in split_top_level_commas(m_alloc.group(1)):
                t = chunk.strip()
                if not t or ("=" in t and "=>" not in t):
                    continue
                inferred = infer_size_from_alloc_target(t)
                if inferred is not None:
                    n, total = inferred
                    sizes[n] = total
            continue

        parsed = parse_print_stmt(code)
        if parsed is None:
            parsed = parse_write_stmt(code)
        if parsed is None:
            continue
        fmt_expr, iolist = parsed
        fmt_text = normalize_format_expr(fmt_expr, named_fmts)
        if fmt_text is None:
            continue

        desc = parse_format_descriptors(fmt_text)
        if not desc:
            continue
        arg_kinds = iolist_arg_kinds(iolist, types, sizes)
        if not arg_kinds:
            continue

        for idx, ak in enumerate(arg_kinds, start=1):
            dk = desc[(idx - 1) % len(desc)]
            ok = descriptor_matches(dk, ak)
            if ok is True:
                continue
            if ok is False:
                findings.append(
                    Finding(
                        path=unit.path,
                        unit_kind=unit.kind,
                        unit_name=unit.name,
                        line=ln,
                        certainty="definite",
                        detail=(
                            f"format descriptor {dk!r} likely mismatches item {idx} type {ak}"
                        ),
                    )
                )
            else:
                findings.append(
                    Finding(
                        path=unit.path,
                        unit_kind=unit.kind,
                        unit_name=unit.name,
                        line=ln,
                        certainty="possible",
                        detail=(
                            f"format descriptor {dk!r} may mismatch item {idx} (type not resolved)"
                        ),
                    )
                )
    return findings


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Advisory checker for possible/definite PRINT/WRITE format/type mismatches"
    )
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--verbose", action="store_true", help="Print all findings")
    args = parser.parse_args()

    files = choose_files(args.fortran_files, args.exclude)
    if not files:
        print("No source files remain after applying --exclude filters.")
        return 2

    infos, any_missing = fscan.load_source_files(files)
    if not infos:
        return 2 if any_missing else 1

    ordered, _ = fscan.order_files_least_dependent(infos)
    findings: List[Finding] = []
    for finfo in ordered:
        for u in collect_units(finfo):
            findings.extend(analyze_unit(u))

    if not findings:
        print("No likely format/type mismatch findings.")
        return 0

    findings.sort(key=lambda x: (x.path.name.lower(), x.line, x.unit_kind, x.unit_name))
    by_file: Dict[str, int] = {}
    n_def = 0
    n_pos = 0
    for f in findings:
        by_file[f.path.name] = by_file.get(f.path.name, 0) + 1
        if f.certainty == "definite":
            n_def += 1
        else:
            n_pos += 1

    print(
        f"{len(findings)} format/type mismatch finding(s) in {len(by_file)} file(s) "
        f"(definite {n_def}, possible {n_pos})."
    )

    if args.verbose:
        for f in findings:
            print(f"{f.path.name}:{f.line} {f.unit_kind} {f.unit_name} [{f.certainty}] - {f.detail}")
    else:
        for fname in sorted(by_file.keys(), key=str.lower):
            print(f"{fname}: {by_file[fname]}")
        first = findings[0]
        print(
            f"\nFirst finding: {first.path.name}:{first.line} {first.unit_kind} {first.unit_name} "
            f"[{first.certainty}] - {first.detail}"
        )
        print("Run with --verbose to list all findings.")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
