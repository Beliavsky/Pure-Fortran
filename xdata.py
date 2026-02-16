#!/usr/bin/env python3
"""Advisory checker/fixer for DATA-initialized variables that look constant-like."""

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
import xunset

ASSIGN_RE = re.compile(
    r"^\s*(?:\d+\s+)?([a-z][a-z0-9_]*(?:\s*%\s*[a-z][a-z0-9_]*)?)\s*(?:\([^)]*\))?\s*=",
    re.IGNORECASE,
)
DATA_RE = re.compile(r"^\s*(?:\d+\s+)?data\b\s*(.*)$", re.IGNORECASE)
CALL_RE = re.compile(r"^\s*call\b", re.IGNORECASE)
READ_RE = re.compile(r"^\s*read\b", re.IGNORECASE)
COMMON_RE = re.compile(r"^\s*(?:\d+\s+)?common\b\s*(.*)$", re.IGNORECASE)
EQUIV_RE = re.compile(r"^\s*(?:\d+\s+)?equivalence\b\s*(.*)$", re.IGNORECASE)
SAVE_STMT_RE = re.compile(r"^\s*(?:\d+\s+)?save\b", re.IGNORECASE)
DO_RE = re.compile(
    r"^\s*do\b(?:\s+\d+\s+)?\s*([a-z][a-z0-9_]*)\s*=",
    re.IGNORECASE,
)
IDENT_RE = re.compile(r"\b([a-z][a-z0-9_]*)\b", re.IGNORECASE)
TYPE_DECL_RE = re.compile(
    r"^\s*(integer|real|logical|character|complex|double\s+precision|type\s*\([^)]*\)|class\s*\([^)]*\))\b",
    re.IGNORECASE,
)
NO_COLON_DECL_RE = re.compile(
    r"^\s*(?P<spec>(?:integer|real|logical|complex|character|double\s+precision)\s*(?:\([^)]*\))?"
    r"|type\s*\([^)]*\)|class\s*\([^)]*\))\s+(?P<rhs>.+)$",
    re.IGNORECASE,
)


@dataclass
class Candidate:
    """One DATA-initialized variable that appears write-once."""

    path: Path
    unit_kind: str
    unit_name: str
    var: str
    first_data_line: int
    data_count: int
    data_lines: Optional[List[int]] = None
    data_value: Optional[str] = None
    data_values: Optional[List[str]] = None
    data_array_ctor: Optional[str] = None
    decl_line: Optional[int] = None
    unit_start: int = -1
    unit_end: int = -1


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


def split_top_level_slashes(text: str) -> List[str]:
    """Split text by top-level slash delimiters only."""
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
            elif ch == "/" and depth == 0:
                out.append("".join(cur).strip())
                cur = []
                i += 1
                continue
        cur.append(ch)
        i += 1
    out.append("".join(cur).strip())
    return out


def split_code_comment(line: str) -> Tuple[str, str]:
    """Split one source line into code and trailing comment text."""
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


def statement_span(lines: List[str], start_idx: int) -> Tuple[int, int]:
    """Return [start, end) physical-line span for a continued free-form statement."""
    n = len(lines)
    if start_idx < 0 or start_idx >= n:
        return start_idx, start_idx
    i = start_idx
    pending = True
    saw_code = False
    while i < n and pending:
        raw = lines[i].rstrip("\r\n")
        code, _ = split_code_comment(raw)
        s = code.strip()
        if s:
            saw_code = True
            pending = code.rstrip().endswith("&")
        else:
            # Blank/comment line can appear between continuations.
            pending = saw_code and pending
        i += 1
        if not pending:
            break
    return start_idx, i


def parse_data_object_names(obj_text: str) -> Set[str]:
    """Extract base names from one DATA object-list segment (conservative)."""
    out: Set[str] = set()
    for tok in split_top_level_commas(obj_text):
        t = tok.strip()
        if not t:
            continue
        if t.startswith("(") and t.endswith(")") and "=" in t:
            # Implied-DO form, pull first item recursively.
            inner = t[1:-1].strip()
            first = split_top_level_commas(inner)
            if first:
                out.update(parse_data_object_names(first[0]))
            continue
        b = fscan.base_identifier(t)
        if b:
            out.add(b)
    return out


def parse_data_stmt_targets(stmt: str) -> Set[str]:
    """Parse one DATA statement into target base names (conservative)."""
    m = DATA_RE.match(stmt.strip())
    if not m:
        return set()
    tail = m.group(1).strip()
    if not tail:
        return set()
    segs = split_top_level_slashes(tail)
    if len(segs) < 3:
        return set()
    # DATA object/value pairs alternate across slash separators.
    out: Set[str] = set()
    for i in range(0, len(segs) - 1, 2):
        obj = segs[i].lstrip(", ").strip()
        if not obj:
            continue
        out.update(parse_data_object_names(obj))
    return out


def parse_common_names(stmt: str) -> Set[str]:
    """Parse COMMON statement names conservatively."""
    m = COMMON_RE.match(stmt.strip())
    if not m:
        return set()
    tail = m.group(1).strip()
    if not tail:
        return set()
    # Remove /block/ segments; keep only entity list text.
    cleaned = re.sub(r"/[^/]*/", " ", tail)
    out: Set[str] = set()
    for tok in split_top_level_commas(cleaned):
        b = fscan.base_identifier(tok)
        if b:
            out.add(b)
    return out


def parse_equivalence_names(stmt: str) -> Set[str]:
    """Parse EQUIVALENCE statement names conservatively."""
    m = EQUIV_RE.match(stmt.strip())
    if not m:
        return set()
    tail = m.group(1).strip()
    if not tail:
        return set()
    out: Set[str] = set()
    groups = re.findall(r"\(([^)]*)\)", tail)
    for grp in groups:
        for tok in split_top_level_commas(grp):
            b = fscan.base_identifier(tok)
            if b:
                out.add(b)
    return out


def parse_simple_data_scalar(stmt: str) -> Optional[Tuple[str, str]]:
    """Parse DATA var / value / for simple scalar-only fixes."""
    m = DATA_RE.match(stmt.strip())
    if not m:
        return None
    tail = m.group(1).strip()
    if not tail:
        return None
    segs = split_top_level_slashes(tail)
    if len(segs) != 3:
        return None
    obj = segs[0].lstrip(", ").strip()
    val = segs[1].strip()
    if not obj or not val:
        return None
    if "," in obj or "," in val:
        return None
    if "(" in obj or ")" in obj:
        return None
    if not re.match(r"^[a-z][a-z0-9_]*$", obj, re.IGNORECASE):
        return None
    return obj.lower(), val


def parse_simple_data_array(stmt: str) -> Optional[Tuple[str, List[str]]]:
    """Parse DATA a / v1, v2, ... / for simple array conversion."""
    m = DATA_RE.match(stmt.strip())
    if not m:
        return None
    tail = m.group(1).strip()
    if not tail:
        return None
    segs = split_top_level_slashes(tail)
    if len(segs) != 3:
        return None
    obj = segs[0].lstrip(", ").strip()
    val = segs[1].strip()
    if not obj or not val:
        return None
    if "," in obj:
        return None
    if "(" in obj or ")" in obj:
        return None
    if not re.match(r"^[a-z][a-z0-9_]*$", obj, re.IGNORECASE):
        return None
    vals = split_top_level_commas(val)
    if len(vals) < 2:
        return None
    expanded = expand_data_value_tokens(vals)
    if expanded is None or len(expanded) < 2:
        return None
    return obj.lower(), expanded


def parse_simple_data_array_implied_do(stmt: str) -> Optional[Tuple[str, Optional[List[str]], Optional[str]]]:
    """Parse DATA (a(i), i=1,n) / ... / for conservative array conversion."""
    m = DATA_RE.match(stmt.strip())
    if not m:
        return None
    tail = m.group(1).strip()
    if not tail:
        return None
    segs = split_top_level_slashes(tail)
    if len(segs) != 3:
        return None
    obj = segs[0].lstrip(", ").strip()
    val = segs[1].strip()
    if not obj or not val:
        return None
    mm = re.match(
        r"^\(\s*([a-z][a-z0-9_]*)\s*\(\s*([a-z][a-z0-9_]*)\s*\)\s*,\s*"
        r"([a-z][a-z0-9_]*)\s*=\s*([^,()]+)\s*,\s*([^,()]+)"
        r"(?:\s*,\s*([^,()]+))?\s*\)$",
        obj,
        re.IGNORECASE,
    )
    if not mm:
        return None
    arr = mm.group(1).lower()
    idx1 = mm.group(2).lower()
    idx2 = mm.group(3).lower()
    lo = mm.group(4).strip()
    hi = mm.group(5).strip()
    st = mm.group(6).strip() if mm.group(6) else "1"
    if idx1 != idx2:
        return None
    if lo != "1" or st != "1":
        return None
    vals = split_top_level_commas(val)
    if not vals:
        return None
    expanded = expand_data_value_tokens(vals)
    has_symbolic_repeat = any(
        re.match(r"^\s*[a-z][a-z0-9_]*\s*\*\s*.+$", tok.strip(), re.IGNORECASE) is not None
        for tok in vals
    )
    if expanded is not None and len(expanded) >= 2 and not has_symbolic_repeat:
        return arr, expanded, None
    if len(vals) == 1:
        tok = vals[0].strip()
        mrep = re.match(r"^\s*([a-z0-9_]+)\s*\*\s*(.+)\s*$", tok, re.IGNORECASE)
        if mrep and mrep.group(1).strip().lower() == hi.lower():
            rhs = mrep.group(2).strip()
            if rhs:
                ctor = f"({rhs}, {idx1}=1,{hi})"
                return arr, None, ctor
    # Pattern: DATA (a(i),i=1,m*n[+b]) / n*v1, n*v2, ..., scalar_terms... /
    # Convert to: [(v1, i=1,n), (v2, i=1,n), ..., scalar_terms...]
    reps: List[Tuple[str, str]] = []
    for tok in vals:
        mrep = re.match(r"^\s*([a-z0-9_]+)\s*\*\s*(.+)\s*$", tok.strip(), re.IGNORECASE)
        if not mrep:
            reps = []
            break
        reps.append((mrep.group(1).strip().lower(), mrep.group(2).strip()))
    if reps:
        rep_sym = reps[0][0]
        if all(r == rep_sym and v for r, v in reps):
            k = len(reps)
            hi_norm = hi.replace(" ", "").lower()
            rep_norm = rep_sym.replace(" ", "").lower()
            if hi_norm in {f"{k}*{rep_norm}", f"{rep_norm}*{k}"}:
                ctor = ", ".join(f"({v}, {idx1}=1,{rep_sym})" for _r, v in reps)
                return arr, None, ctor
    # Mixed symbolic repeats and non-repeats:
    #   DATA (a(i),i=1,2*n+1) / n*v1, n*v2, s1 /
    # -> [(v1, i=1,n), (v2, i=1,n), s1]
    sym_rep: Optional[str] = None
    ctor_parts: List[str] = []
    coeff_a = 0
    coeff_b = 0
    ok_mixed = True
    for tok in vals:
        t = tok.strip()
        if not t:
            ok_mixed = False
            break
        mrep = re.match(r"^\s*([a-z0-9_]+)\s*\*\s*(.+)\s*$", t, re.IGNORECASE)
        if mrep:
            rc = mrep.group(1).strip().lower()
            rv = mrep.group(2).strip()
            if not rv:
                ok_mixed = False
                break
            if re.match(r"^\d+$", rc):
                nn = int(rc)
                if nn <= 0:
                    ok_mixed = False
                    break
                ctor_parts.extend([rv] * nn)
                coeff_b += nn
                continue
            if sym_rep is None:
                sym_rep = rc
            if rc != sym_rep:
                ok_mixed = False
                break
            ctor_parts.append(f"({rv}, {idx1}=1,{sym_rep})")
            coeff_a += 1
            continue
        ctor_parts.append(t)
        coeff_b += 1
    if ok_mixed and ctor_parts and sym_rep is not None:
        hi_norm = hi.replace(" ", "").lower()
        sym = sym_rep
        mlin = re.match(
            rf"^(?:(\d+)\*)?{re.escape(sym)}(?:\+(\d+)|-(\d+))?$|^(\d+)(?:\+(?:(\d+)\*)?{re.escape(sym)}|-(?:(\d+)\*)?{re.escape(sym)})$",
            hi_norm,
            re.IGNORECASE,
        )
        hi_a: Optional[int] = None
        hi_b: Optional[int] = None
        if hi_norm == sym:
            hi_a, hi_b = 1, 0
        elif mlin:
            if mlin.group(1) is not None or mlin.group(2) is not None or mlin.group(3) is not None:
                hi_a = int(mlin.group(1)) if mlin.group(1) else 1
                if mlin.group(2):
                    hi_b = int(mlin.group(2))
                elif mlin.group(3):
                    hi_b = -int(mlin.group(3))
                else:
                    hi_b = 0
            else:
                base = int(mlin.group(4))
                if mlin.group(5) is not None:
                    hi_a = int(mlin.group(5))
                elif mlin.group(6) is not None:
                    hi_a = -int(mlin.group(6))
                else:
                    hi_a = 1 if "+" in hi_norm else -1
                hi_b = base
        if hi_a is not None and hi_b is not None and hi_a == coeff_a and hi_b == coeff_b:
            return arr, None, ", ".join(ctor_parts)
    return None


def parse_int_parameter_bindings(stmt: str) -> Dict[str, int]:
    """Parse simple integer PARAMETER bindings from one declaration/statement."""
    out: Dict[str, int] = {}
    s = stmt.strip()
    # INTEGER, PARAMETER :: n = 3, m = 4
    m = re.match(r"(?i)^\s*integer\b[^!]*\bparameter\b[^:]*::\s*(.+)$", s)
    if m:
        rhs = m.group(1).strip()
        for part in split_top_level_commas(rhs):
            mm = re.match(r"^\s*([a-z][a-z0-9_]*)\s*=\s*([+-]?\d+)\s*$", part, re.IGNORECASE)
            if mm:
                out[mm.group(1).lower()] = int(mm.group(2))
    # PARAMETER ( n = 3, m = 4 )
    for grp in re.findall(r"(?i)\bparameter\s*\(([^)]*)\)", s):
        for part in split_top_level_commas(grp):
            mm = re.match(r"^\s*([a-z][a-z0-9_]*)\s*=\s*([+-]?\d+)\s*$", part, re.IGNORECASE)
            if mm:
                out[mm.group(1).lower()] = int(mm.group(2))
    return out


def parse_ctor_index_var(ctor_text: str) -> Optional[str]:
    """Extract implied-DO index variable from constructor text '(..., i=1,n)'."""
    m = re.match(r"^\(\s*.+\s*,\s*([a-z][a-z0-9_]*)\s*=\s*1\s*,\s*.+\)$", ctor_text, re.IGNORECASE)
    if not m:
        return None
    return m.group(1).lower()


def parse_simple_data_indexed_scalar(stmt: str) -> Optional[Tuple[str, int, str]]:
    """Parse DATA a(i) / v / with literal integer index i (conservative)."""
    m = DATA_RE.match(stmt.strip())
    if not m:
        return None
    tail = m.group(1).strip()
    if not tail:
        return None
    segs = split_top_level_slashes(tail)
    if len(segs) != 3:
        return None
    obj = segs[0].lstrip(", ").strip()
    val = segs[1].strip()
    if not obj or not val:
        return None
    mm = re.match(r"^([a-z][a-z0-9_]*)\s*\(\s*([0-9]+)\s*\)$", obj, re.IGNORECASE)
    if not mm:
        return None
    return mm.group(1).lower(), int(mm.group(2)), val


def expand_data_value_tokens(tokens: List[str]) -> Optional[List[str]]:
    """Expand simple DATA repeats like n*value in token list."""
    out: List[str] = []
    for tok in tokens:
        t = tok.strip()
        if not t:
            return None
        mrep = re.match(r"^\s*(\d+)\s*\*\s*(.+)\s*$", t)
        if mrep:
            n = int(mrep.group(1))
            if n <= 0:
                return None
            v = mrep.group(2).strip()
            if not v:
                return None
            out.extend([v] * n)
            continue
        out.append(t)
    return out


def strip_one_line_if_prefix(stmt: str) -> str:
    """Strip leading one-line IF(condition) prefix if present."""
    s = stmt.lstrip()
    if not s.lower().startswith("if"):
        return stmt
    m = re.match(r"^if\s*\(", s, re.IGNORECASE)
    if not m:
        return stmt
    depth = 0
    in_single = False
    in_double = False
    for j in range(m.end() - 1, len(s)):
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
                    tail = s[j + 1 :].lstrip()
                    if tail.lower().startswith("then"):
                        return stmt
                    return tail if tail else stmt
    return stmt


def extract_written_names(stmt: str) -> Set[str]:
    """Return names likely written by one statement (conservative)."""
    out: Set[str] = set()
    s = strip_one_line_if_prefix(stmt.strip())
    low = s.lower()

    mdo = DO_RE.match(s)
    if mdo:
        out.add(mdo.group(1).lower())

    ma = ASSIGN_RE.match(s)
    if ma:
        lhs = ma.group(1)
        b = fscan.base_identifier(lhs)
        if b:
            out.add(b)

    if CALL_RE.match(s):
        # Conservative: arguments to CALL may be modified (pass-by-reference).
        start = s.find("(")
        end = s.rfind(")")
        if start >= 0 and end > start:
            args = s[start + 1 : end]
            for m in IDENT_RE.finditer(args):
                out.add(m.group(1).lower())

    if READ_RE.match(s):
        # Conservative: READ can assign to many names (including IOSTAT/SIZE vars).
        for m in IDENT_RE.finditer(low):
            out.add(m.group(1).lower())

    return out


def parse_decl_rhs_entities(rhs: str) -> List[str]:
    """Split declaration entity list into top-level chunks."""
    return [p for p in split_top_level_commas(rhs) if p]


def parse_decl_entity_name(entity_text: str) -> Optional[Tuple[str, bool, bool]]:
    """Parse entity declaration chunk into (name, has_dims, has_init)."""
    txt = entity_text.strip()
    if not txt:
        return None
    if "=>" in txt:
        return None
    has_init = "=" in txt
    lhs = txt.split("=", 1)[0].strip() if has_init else txt
    m = re.match(r"^([a-z][a-z0-9_]*)\s*(\((.*)\))?\s*$", lhs, re.IGNORECASE)
    if not m:
        return None
    return m.group(1).lower(), m.group(2) is not None, has_init


def parse_declaration_names(stmt: str) -> Dict[str, Tuple[bool, bool]]:
    """Parse one declaration statement into name -> (has_dims, has_init)."""
    low = stmt.strip().lower()
    if not TYPE_DECL_RE.match(low):
        return {}
    rhs = ""
    if "::" in stmt:
        rhs = stmt.split("::", 1)[1]
    else:
        m = NO_COLON_DECL_RE.match(stmt.strip())
        if not m:
            return {}
        rhs = m.group("rhs")
    out: Dict[str, Tuple[bool, bool]] = {}
    for ent in parse_decl_rhs_entities(rhs):
        parsed = parse_decl_entity_name(ent)
        if parsed is None:
            continue
        out[parsed[0]] = (parsed[1], parsed[2])
    return out


def rewrite_decl_line_for_parameter(
    raw_line: str, var_name: str, value_text: str
) -> Optional[str]:
    """Rewrite one declaration line into parameter form for a single scalar entity."""
    body = raw_line.rstrip("\r\n")
    eol = raw_line[len(body) :]
    code, comment = split_code_comment(body)
    if ";" in code or "&" in code:
        return None
    if not TYPE_DECL_RE.match(code.strip()):
        return None
    if "::" in code:
        spec, rhs = code.split("::", 1)
        spec_txt = spec.strip()
    else:
        m = NO_COLON_DECL_RE.match(code.strip())
        if not m:
            return None
        spec_txt = m.group("spec").strip()
        rhs = m.group("rhs")
    if "parameter" in spec_txt.lower():
        return None
    ents = parse_decl_rhs_entities(rhs)
    if len(ents) != 1:
        return None
    parsed = parse_decl_entity_name(ents[0])
    if parsed is None:
        return None
    name, has_dims, has_init = parsed
    if name != var_name.lower() or has_dims or has_init:
        return None
    indent = re.match(r"^\s*", code).group(0)
    spec_out = f"{spec_txt}, parameter"
    new_code = f"{indent}{spec_out} :: {name} = {value_text}"
    if comment:
        new_code += comment
    return new_code + eol


def has_disallowed_decl_attrs_for_parameter(spec_txt: str) -> bool:
    """Return True if declaration attrs make PARAMETER rewrite unsafe."""
    low = spec_txt.lower()
    bad_markers = [
        "intent(",
        "optional",
        "allocatable",
        "pointer",
        "target",
        "value",
        "volatile",
        "asynchronous",
        "external",
        "intrinsic",
        "dimension",
    ]
    return any(tok in low for tok in bad_markers)


def remove_save_attr_from_spec(spec_txt: str) -> str:
    """Remove SAVE attribute from declaration spec text."""
    parts = split_top_level_commas(spec_txt)
    if len(parts) > 1:
        kept = [p.strip() for p in parts if p.strip().lower() != "save"]
        if kept:
            return ", ".join(kept)
    # Fallback for non-comma forms.
    out = re.sub(r"(?i)\bsave\b\s*,?", "", spec_txt)
    out = re.sub(r"\s+,", ",", out)
    out = re.sub(r",\s*,", ",", out)
    out = re.sub(r"\s{2,}", " ", out).strip()
    return out if out else spec_txt


def rewrite_decl_line_extract_parameter(
    raw_line: str, var_name: str, value_text: str
) -> Optional[Tuple[str, str]]:
    """Rewrite declaration removing var_name and return (new_decl, parameter_decl)."""
    body = raw_line.rstrip("\r\n")
    eol = raw_line[len(body) :]
    code, comment = split_code_comment(body)
    if ";" in code or "&" in code:
        return None
    if not TYPE_DECL_RE.match(code.strip()):
        return None
    had_colons = "::" in code
    if had_colons:
        spec, rhs = code.split("::", 1)
        spec_txt = spec.strip()
    else:
        m = NO_COLON_DECL_RE.match(code.strip())
        if not m:
            return None
        spec_txt = m.group("spec").strip()
        rhs = m.group("rhs")
    if has_disallowed_decl_attrs_for_parameter(spec_txt):
        return None
    ents = parse_decl_rhs_entities(rhs)
    if not ents:
        return None
    target_idx = -1
    parsed_ents: List[Tuple[str, bool, bool]] = []
    for i, ent in enumerate(ents):
        parsed = parse_decl_entity_name(ent)
        if parsed is None:
            return None
        parsed_ents.append(parsed)
        if parsed[0] == var_name.lower():
            target_idx = i
    if target_idx < 0:
        return None
    t_name, t_dims, t_init = parsed_ents[target_idx]
    if t_dims or t_init:
        return None
    indent = re.match(r"^\s*", code).group(0)
    param_spec_src = remove_save_attr_from_spec(spec_txt)
    param_spec = param_spec_src if "parameter" in param_spec_src.lower() else f"{param_spec_src}, parameter"
    param_decl = f"{indent}{param_spec} :: {t_name} = {value_text}{eol}"

    rem_ents = [ent.strip() for i, ent in enumerate(ents) if i != target_idx]
    if not rem_ents:
        return param_decl, param_decl
    if had_colons:
        rem_decl = f"{indent}{spec_txt} :: {', '.join(rem_ents)}"
    else:
        rem_decl = f"{indent}{spec_txt} {', '.join(rem_ents)}"
    if comment:
        rem_decl += comment
    rem_decl += eol
    return rem_decl, param_decl


def rewrite_array_decl_line_extract_parameter(
    raw_line: str, var_name: str, value_text: str
) -> Optional[Tuple[str, str]]:
    """Rewrite array declaration removing var_name and return (new_decl, parameter_decl)."""
    body = raw_line.rstrip("\r\n")
    eol = raw_line[len(body) :]
    code, comment = split_code_comment(body)
    if ";" in code or "&" in code:
        return None
    if not TYPE_DECL_RE.match(code.strip()):
        return None
    had_colons = "::" in code
    if had_colons:
        spec, rhs = code.split("::", 1)
        spec_txt = spec.strip()
    else:
        m = NO_COLON_DECL_RE.match(code.strip())
        if not m:
            return None
        spec_txt = m.group("spec").strip()
        rhs = m.group("rhs")
    if has_disallowed_decl_attrs_for_parameter(spec_txt):
        return None
    ents = parse_decl_rhs_entities(rhs)
    if not ents:
        return None
    target_idx = -1
    target_name = ""
    target_dims = ""
    target_init = False
    for i, ent in enumerate(ents):
        e = ent.strip()
        mm = re.match(
            rf"^({re.escape(var_name)})\s*\(\s*([^)]+)\s*\)\s*(?:=\s*(.+))?$",
            e,
            re.IGNORECASE,
        )
        if mm:
            target_idx = i
            target_name = mm.group(1).lower()
            target_dims = mm.group(2).strip()
            target_init = mm.group(3) is not None and mm.group(3).strip() != ""
            break
    if target_idx < 0 or target_init:
        return None
    if "," in target_dims:
        return None
    indent = re.match(r"^\s*", code).group(0)
    param_spec_src = remove_save_attr_from_spec(spec_txt)
    param_spec = param_spec_src if "parameter" in param_spec_src.lower() else f"{param_spec_src}, parameter"
    param_decl = f"{indent}{param_spec} :: {target_name}({target_dims}) = [{value_text}]{eol}"

    rem_ents = [ent.strip() for i, ent in enumerate(ents) if i != target_idx]
    if not rem_ents:
        return param_decl, param_decl
    if had_colons:
        rem_decl = f"{indent}{spec_txt} :: {', '.join(rem_ents)}"
    else:
        rem_decl = f"{indent}{spec_txt} {', '.join(rem_ents)}"
    if comment:
        rem_decl += comment
    rem_decl += eol
    return rem_decl, param_decl


def parse_rank1_literal_extent(dims_text: str) -> Optional[int]:
    """Parse rank-1 literal bounds and return extent."""
    txt = dims_text.strip()
    if "," in txt:
        return None
    if ":" in txt:
        lo, hi = txt.split(":", 1)
        lo = lo.strip() or "1"
        hi = hi.strip()
        if not re.match(r"^[+-]?\d+$", lo) or not re.match(r"^[+-]?\d+$", hi):
            return None
        n = int(hi) - int(lo) + 1
        return n if n > 0 else None
    if not re.match(r"^[+-]?\d+$", txt):
        return None
    n = int(txt)
    return n if n > 0 else None


def decl_array_info_for_name(raw_line: str, var_name: str) -> Optional[Tuple[str, str, str]]:
    """Return (indent, spec_without_save, dims) for simple single-entity rank-1 declaration."""
    body = raw_line.rstrip("\r\n")
    code, _ = split_code_comment(body)
    if ";" in code or "&" in code:
        return None
    if not TYPE_DECL_RE.match(code.strip()):
        return None
    had_colons = "::" in code
    if had_colons:
        spec, rhs = code.split("::", 1)
        spec_txt = spec.strip()
    else:
        m = NO_COLON_DECL_RE.match(code.strip())
        if not m:
            return None
        spec_txt = m.group("spec").strip()
        rhs = m.group("rhs")
    low_spec = spec_txt.lower()
    # Conservative: skip declarations with attrs that need more complex handling.
    if "dimension" in low_spec or "save" in low_spec:
        return None
    if has_disallowed_decl_attrs_for_parameter(spec_txt):
        return None
    ents = parse_decl_rhs_entities(rhs)
    if len(ents) != 1:
        return None
    ent = ents[0].strip()
    m = re.match(
        rf"^({re.escape(var_name)})\s*\(\s*([^)]+)\s*\)\s*$",
        ent,
        re.IGNORECASE,
    )
    if not m:
        return None
    dims = m.group(2).strip()
    # Accept symbolic rank-1 extents (e.g., x(n)) as well as literal ones.
    if "," in dims:
        return None
    indent = re.match(r"^\s*", code).group(0)
    return indent, remove_save_attr_from_spec(spec_txt), dims


def rewrite_save_stmt_remove_name(raw_line: str, var_name: str) -> Tuple[str, bool]:
    """Remove one name from a SAVE statement line; may blank line if empty."""
    body = raw_line.rstrip("\r\n")
    eol = raw_line[len(body) :]
    code, comment = split_code_comment(body)
    if not SAVE_STMT_RE.match(code.strip()):
        return raw_line, False
    m = re.match(r"^(\s*(?:\d+\s+)?save\b)\s*(?:::)?\s*(.*)$", code, re.IGNORECASE)
    if not m:
        return raw_line, False
    head = m.group(1)
    tail = m.group(2).strip()
    if not tail:
        # Bare SAVE applies to entire unit; keep unchanged.
        return raw_line, False
    parts = split_top_level_commas(tail)
    kept: List[str] = []
    removed = False
    for p in parts:
        t = p.strip()
        if not t:
            continue
        # SAVE /blk/ common block spec; keep unchanged.
        if re.match(r"^/[^/]*/$", t):
            kept.append(t)
            continue
        b = fscan.base_identifier(t)
        if b == var_name.lower():
            removed = True
            continue
        kept.append(t)
    if not removed:
        return raw_line, False
    if not kept:
        return "", True
    new_code = f"{head} {', '.join(kept)}"
    if comment:
        new_code += comment
    return new_code + eol, True


def extract_decl_entity_to_standalone(raw_line: str, var_name: str) -> Optional[Tuple[str, str]]:
    """Extract one declared entity into a standalone declaration line.

    Returns (rewritten_original_line, standalone_decl_line).
    """
    body = raw_line.rstrip("\r\n")
    eol = raw_line[len(body) :]
    code, comment = split_code_comment(body)
    if ";" in code or "&" in code:
        return None
    if not TYPE_DECL_RE.match(code.strip()):
        return None
    had_colons = "::" in code
    if had_colons:
        spec, rhs = code.split("::", 1)
        spec_txt = spec.strip()
    else:
        m = NO_COLON_DECL_RE.match(code.strip())
        if not m:
            return None
        spec_txt = m.group("spec").strip()
        rhs = m.group("rhs")
    ents = parse_decl_rhs_entities(rhs)
    if not ents:
        return None
    target_idx = -1
    target_ent = ""
    for i, ent in enumerate(ents):
        parsed = parse_decl_entity_name(ent)
        if parsed is None:
            return None
        if parsed[0] == var_name.lower():
            target_idx = i
            target_ent = ent.strip()
            break
    if target_idx < 0:
        return None
    indent = re.match(r"^\s*", code).group(0)
    standalone = f"{indent}{spec_txt} :: {target_ent}{eol}"
    rem_ents = [ent.strip() for i, ent in enumerate(ents) if i != target_idx]
    if not rem_ents:
        return "", standalone
    if had_colons:
        rem_decl = f"{indent}{spec_txt} :: {', '.join(rem_ents)}"
    else:
        rem_decl = f"{indent}{spec_txt} {', '.join(rem_ents)}"
    if comment:
        rem_decl += comment
    rem_decl += eol
    return rem_decl, standalone


def analyze_unit(unit: xunset.Unit) -> List[Candidate]:
    """Analyze one unit and report DATA targets that look write-once."""
    data_lines: Dict[str, List[int]] = {}
    data_scalar: Dict[str, List[Tuple[int, str]]] = {}
    data_array: Dict[str, List[Tuple[int, List[str]]]] = {}
    data_array_ctor: Dict[str, List[Tuple[int, str]]] = {}
    data_indexed: Dict[str, List[Tuple[int, int, str]]] = {}
    decls: Dict[str, List[int]] = {}
    int_params: Dict[str, int] = {}
    writes_after_data: Set[str] = set()
    storage_assoc: Set[str] = set()

    for ln, stmt in unit.body:
        int_params.update(parse_int_parameter_bindings(stmt))
        storage_assoc.update(parse_common_names(stmt))
        storage_assoc.update(parse_equivalence_names(stmt))

        dnames = parse_declaration_names(stmt)
        for n in dnames.keys():
            decls.setdefault(n, []).append(ln)

        simple_data = parse_simple_data_scalar(stmt)
        if simple_data is not None:
            data_scalar.setdefault(simple_data[0], []).append((ln, simple_data[1]))
        simple_array = parse_simple_data_array(stmt)
        if simple_array is not None:
            data_array.setdefault(simple_array[0], []).append((ln, simple_array[1]))
        simple_array_do = parse_simple_data_array_implied_do(stmt)
        if simple_array_do is not None:
            n_do, vals_do, ctor_do = simple_array_do
            if vals_do is not None:
                data_array.setdefault(n_do, []).append((ln, vals_do))
            elif ctor_do is not None:
                mm_ctor = re.match(
                    r"^\(\s*(.+)\s*,\s*([a-z][a-z0-9_]*)\s*=\s*1\s*,\s*([a-z][a-z0-9_]*|[+-]?\d+)\s*\)$",
                    ctor_do,
                    re.IGNORECASE,
                )
                if mm_ctor:
                    rhs = mm_ctor.group(1).strip()
                    hi = mm_ctor.group(3).strip().lower()
                    cnt: Optional[int] = None
                    if re.match(r"^[+-]?\d+$", hi):
                        cnt = int(hi)
                    elif hi in int_params:
                        cnt = int_params[hi]
                    if cnt is not None and cnt > 0:
                        data_array.setdefault(n_do, []).append((ln, [rhs] * cnt))
                data_array_ctor.setdefault(n_do, []).append((ln, ctor_do))
        simple_indexed = parse_simple_data_indexed_scalar(stmt)
        if simple_indexed is not None:
            data_indexed.setdefault(simple_indexed[0], []).append(
                (ln, simple_indexed[1], simple_indexed[2])
            )

        targets = parse_data_stmt_targets(stmt)
        if targets:
            for n in targets:
                data_lines.setdefault(n, []).append(ln)
            continue
        if not data_lines:
            continue
        w = extract_written_names(stmt)
        for n in w:
            if n in data_lines:
                writes_after_data.add(n)

    out: List[Candidate] = []
    for n, lns in data_lines.items():
        if n in storage_assoc:
            continue
        if n in writes_after_data:
            continue
        sval: Optional[str] = None
        scalars = data_scalar.get(n, [])
        if len(lns) == 1 and len(scalars) == 1 and scalars[0][0] == lns[0]:
            sval = scalars[0][1]
        svals: Optional[List[str]] = None
        arrays = data_array.get(n, [])
        if len(lns) == 1 and len(arrays) == 1 and arrays[0][0] == lns[0]:
            svals = arrays[0][1]
        sctor: Optional[str] = None
        ctors = data_array_ctor.get(n, [])
        if len(lns) == 1 and len(ctors) == 1 and ctors[0][0] == lns[0]:
            sctor = ctors[0][1]
        if svals is None:
            idxs = data_indexed.get(n, [])
            if idxs and len(idxs) == len(lns):
                idx_map: Dict[int, str] = {}
                ok = True
                for _ln, iidx, vval in idxs:
                    if iidx < 1 or iidx in idx_map:
                        ok = False
                        break
                    idx_map[iidx] = vval
                if ok and idx_map:
                    max_i = max(idx_map.keys())
                    if set(idx_map.keys()) == set(range(1, max_i + 1)):
                        svals = [idx_map[i] for i in range(1, max_i + 1)]
        dline: Optional[int] = None
        dl = decls.get(n, [])
        if len(dl) == 1:
            dline = dl[0]
        out.append(
            Candidate(
                path=unit.path,
                unit_kind=unit.kind,
                unit_name=unit.name,
                var=n,
                first_data_line=min(lns),
                data_count=len(lns),
                data_lines=sorted(lns),
                data_value=sval,
                data_values=svals,
                data_array_ctor=sctor,
                decl_line=dline,
                unit_start=unit.start,
                unit_end=unit.end,
            )
        )
    return out


def analyze_file(path: Path) -> List[Candidate]:
    """Analyze one Fortran file for DATA constant candidates."""
    infos, any_missing = fscan.load_source_files([path])
    if not infos or any_missing:
        return []
    finfo = infos[0]
    units = xunset.collect_units(finfo)
    out: List[Candidate] = []
    for u in units:
        out.extend(analyze_unit(u))
    return out


def apply_fix_file(
    path: Path,
    candidates: List[Candidate],
    *,
    fix_arrays: bool = False,
    out_path: Optional[Path] = None,
    create_backup: bool = True,
) -> Tuple[int, int, Optional[Path]]:
    """Apply conservative scalar DATA->parameter fixes in one source file."""
    if not candidates:
        return 0, 0, None

    lines = path.read_text(encoding="utf-8", errors="ignore").splitlines(keepends=True)
    changed = 0
    removed = 0
    backup: Optional[Path] = None

    # Work in deterministic line order to avoid accidental overlap.
    ordered = sorted(
        candidates,
        key=lambda c: (c.first_data_line, c.decl_line or 0, c.var),
        reverse=True,
    )
    for c in ordered:
        if c.decl_line is None:
            continue
        if c.data_value is None and not (fix_arrays and (c.data_values is not None or c.data_array_ctor is not None)):
            continue
        dln = c.decl_line - 1
        data_lns = c.data_lines if c.data_lines else [c.first_data_line]
        iln = min(data_lns) - 1
        if dln < 0 or dln >= len(lines) or iln < 0 or iln >= len(lines):
            continue
        if any(ln < 1 or ln > len(lines) for ln in data_lns):
            continue
        data_code, _ = split_code_comment(lines[iln].rstrip("\r\n"))
        if re.match(r"^\s*\d+\s+data\b", data_code, re.IGNORECASE):
            continue
        if fix_arrays and (c.data_values is not None or c.data_array_ctor is not None):
            arr_info = decl_array_info_for_name(lines[dln], c.var)
            indent = ""
            spec_no_save = ""
            dims = ""
            if arr_info is not None:
                indent, spec_no_save, dims = arr_info
            # Prefer implied-DO constructor and ensure its loop variable is
            # declared before the PARAMETER declaration.
            values_txt: Optional[str] = None
            ctor_decl_line: Optional[str] = None
            staged_decl_edit: Optional[Tuple[int, str]] = None
            if c.data_array_ctor is not None:
                idxv = parse_ctor_index_var(c.data_array_ctor)
                if idxv is not None:
                    declared_before_int = False
                    declared_any = False
                    decl_line_idx: Optional[int] = None
                    s0 = max(0, c.unit_start - 1)
                    s1 = min(len(lines), c.unit_end)
                    for j in range(s0, s1):
                        stmt = fscan.strip_comment(lines[j]).strip()
                        if not stmt:
                            continue
                        dnames = parse_declaration_names(stmt)
                        if idxv in dnames:
                            declared_any = True
                            is_int_decl = re.match(r"^\s*(?:\d+\s+)?integer\b", stmt, re.IGNORECASE) is not None
                            if j < dln and is_int_decl:
                                declared_before_int = True
                            if is_int_decl and decl_line_idx is None:
                                decl_line_idx = j
                    if declared_before_int:
                        values_txt = c.data_array_ctor
                    else:
                        loop_var = idxv
                        ctor_txt = c.data_array_ctor
                        # If integer declaration exists later, move it before parameter.
                        moved_decl = False
                        if decl_line_idx is not None and decl_line_idx >= dln:
                            extracted = extract_decl_entity_to_standalone(lines[decl_line_idx], idxv)
                            if extracted is not None:
                                new_line, standalone = extracted
                                staged_decl_edit = (decl_line_idx, new_line)
                                ctor_decl_line = standalone
                                moved_decl = True
                            else:
                                # Fallback: simple integer declaration with only idxv.
                                stmt0 = fscan.strip_comment(lines[decl_line_idx]).strip()
                                if re.match(r"^\s*(?:\d+\s+)?integer\b", stmt0, re.IGNORECASE):
                                    ents = parse_declaration_names(stmt0)
                                    if idxv in ents and len(ents) == 1:
                                        body0 = lines[decl_line_idx].rstrip("\r\n")
                                        eol0 = lines[decl_line_idx][len(body0) :]
                                        indent0 = re.match(r"^\s*", body0).group(0)
                                        staged_decl_edit = (decl_line_idx, "")
                                        ctor_decl_line = f"{indent0}integer :: {idxv}{eol0}"
                                        moved_decl = True
                        # Otherwise if name is used elsewhere, choose a fresh loop variable.
                        if declared_any and not moved_decl:
                            base = f"{idxv}_dp"
                            cand = base
                            k = 2
                            unit_blob = "\n".join(
                                fscan.strip_comment(lines[j]).lower()
                                for j in range(max(0, c.unit_start - 1), min(len(lines), c.unit_end))
                            )
                            while re.search(rf"\b{re.escape(cand.lower())}\b", unit_blob):
                                cand = f"{base}{k}"
                                k += 1
                            loop_var = cand
                            ctor_txt = re.sub(
                                rf"(?i)(,\s*){re.escape(idxv)}(\s*=\s*1\s*,)",
                                rf"\1{loop_var}\2",
                                ctor_txt,
                                count=1,
                            )
                        values_txt = ctor_txt
                        if not moved_decl:
                            eol_decl = "\r\n" if lines[dln].endswith("\r\n") else ("\n" if lines[dln].endswith("\n") else "\n")
                            i_indent = indent if indent else re.match(r"^\s*", lines[dln]).group(0)
                            ctor_decl_line = f"{i_indent}integer :: {loop_var}{eol_decl}"
                else:
                    values_txt = c.data_array_ctor
            if values_txt is None and c.data_values is not None:
                if arr_info is not None:
                    extent = parse_rank1_literal_extent(dims)
                    if extent is not None and extent != len(c.data_values):
                        continue
                values_txt = ", ".join(c.data_values)
            if values_txt is None:
                continue
            use_data_line_for_param = False
            if arr_info is not None:
                eol = "\r\n" if lines[dln].endswith("\r\n") else ("\n" if lines[dln].endswith("\n") else "\n")
                param_decl = (
                    f"{indent}{spec_no_save}, parameter :: {c.var}({dims}) = [{values_txt}]{eol}"
                )
            else:
                rewritten_arr = rewrite_array_decl_line_extract_parameter(lines[dln], c.var, values_txt)
                if rewritten_arr is None:
                    continue
                new_decl, param_decl = rewritten_arr
                use_data_line_for_param = new_decl != param_decl
            if out_path is None and backup is None and create_backup:
                backup = make_backup_path(path)
                shutil.copy2(path, backup)
            if staged_decl_edit is not None:
                j_edit, new_line = staged_decl_edit
                if 0 <= j_edit < len(lines) and lines[j_edit] != new_line:
                    lines[j_edit] = new_line
                    changed += 1
            if arr_info is not None:
                if lines[dln] != param_decl:
                    lines[dln] = param_decl
                    changed += 1
            else:
                if lines[dln] != new_decl:
                    lines[dln] = new_decl
                    changed += 1
            if use_data_line_for_param:
                lines[iln] = param_decl
                changed += 1
            for ln in sorted(set(data_lns)):
                data_code_ln, _ = split_code_comment(lines[ln - 1].rstrip("\r\n"))
                if re.match(r"^\s*\d+\s+data\b", data_code_ln, re.IGNORECASE):
                    continue
                k0, k1 = statement_span(lines, ln - 1)
                for k in range(k0 + (1 if (use_data_line_for_param and k0 == iln) else 0), k1):
                    if lines[k].strip():
                        lines[k] = ""
                        removed += 1
            if ctor_decl_line is not None:
                lines.insert(dln, ctor_decl_line)
                changed += 1

            s0 = max(0, c.unit_start - 1)
            s1 = min(len(lines), c.unit_end)
            for j in range(s0, s1):
                rewritten_save, chg = rewrite_save_stmt_remove_name(lines[j], c.var)
                if chg:
                    lines[j] = rewritten_save
                    changed += 1
            continue
        data_stmt = fscan.strip_comment(lines[iln]).strip()
        data_span_start, data_span_end = statement_span(lines, iln)
        parsed_data = parse_simple_data_scalar(data_stmt)
        if parsed_data is None or parsed_data[0] != c.var:
            continue
        rewritten = rewrite_decl_line_extract_parameter(lines[dln], c.var, c.data_value or "")
        if rewritten is None:
            continue
        new_decl, param_decl = rewritten

        if out_path is None and backup is None and create_backup:
            backup = make_backup_path(path)
            shutil.copy2(path, backup)
        if lines[dln] != new_decl:
            lines[dln] = new_decl
            changed += 1
        # If declaration became parameter directly, remove DATA line.
        # Otherwise, place parameter declaration at DATA line location.
        if new_decl == param_decl:
            for k in range(data_span_start, data_span_end):
                if lines[k].strip():
                    lines[k] = ""
                    removed += 1
        else:
            if lines[data_span_start] != param_decl:
                lines[data_span_start] = param_decl
            for k in range(data_span_start + 1, data_span_end):
                if lines[k].strip():
                    lines[k] = ""
                    removed += 1

        # Remove conflicting SAVE references for converted variable
        # in the same program unit scope.
        s0 = max(0, c.unit_start - 1)
        s1 = min(len(lines), c.unit_end)
        for j in range(s0, s1):
            rewritten_save, chg = rewrite_save_stmt_remove_name(lines[j], c.var)
            if chg:
                lines[j] = rewritten_save
                changed += 1

    if changed == 0 and removed == 0:
        return 0, 0, None
    target = out_path if out_path is not None else path
    target.write_text("".join(lines), encoding="utf-8")
    return changed, removed, backup


def main() -> int:
    """Run DATA constant-candidate advisory/fix workflow."""
    parser = argparse.ArgumentParser(
        description="Find/fix DATA-initialized variables that appear never written afterward"
    )
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--verbose", action="store_true", help="Print candidate details")
    parser.add_argument("--fix", action="store_true", help="Rewrite simple scalar DATA constants as PARAMETERs")
    parser.add_argument(
        "--fix-arrays",
        action="store_true",
        help="With --fix, also rewrite simple full rank-1 array DATA constants as PARAMETERs",
    )
    parser.add_argument("--out", type=Path, help="With --fix, write transformed output to this file (single input)")
    parser.add_argument("--backup", dest="backup", action="store_true", default=True)
    parser.add_argument("--no-backup", dest="backup", action="store_false")
    parser.add_argument("--compiler", type=str, help="Compile command for baseline/after-fix validation")
    parser.add_argument("--tee", action="store_true")
    parser.add_argument("--tee-both", action="store_true")
    parser.add_argument("--run", action="store_true")
    parser.add_argument("--run-both", action="store_true")
    parser.add_argument("--run-diff", action="store_true")
    parser.add_argument("--quiet-run", action="store_true")
    parser.add_argument("--keep-exe", action="store_true")
    args = parser.parse_args()
    if args.run_diff:
        args.run_both = True
    if args.run_both:
        args.run = True
    if args.tee_both:
        args.tee = True
    if args.run and args.out is None:
        args.out = Path("temp.f90")
    if args.out is not None:
        args.fix = True
    if args.fix_arrays:
        args.fix = True
    if args.compiler and not args.fix:
        print("--compiler requires --fix.")
        return 2
    if args.tee and args.out is None:
        print("--tee requires --out.")
        return 2

    files = choose_files(args.fortran_files, args.exclude)
    if not files:
        print("No source files remain after applying --exclude filters.")
        return 2
    if args.out is not None and len(files) != 1:
        print("--out requires exactly one input source file.")
        return 2
    if args.run and len(files) != 1:
        print("--run/--run-both/--run-diff require exactly one input source file.")
        return 2
    baseline_compile_paths = files
    after_compile_paths = [args.out] if (args.fix and args.out is not None) else files
    if args.fix and args.compiler:
        if not fbuild.run_compiler_command(args.compiler, baseline_compile_paths, "baseline", fscan.display_path):
            return 5

    candidates: List[Candidate] = []
    by_file: Dict[Path, List[Candidate]] = {}
    for p in files:
        cands = analyze_file(p)
        if cands:
            by_file[p] = cands
            candidates.extend(cands)

    orig_out = ""
    orig_err = ""
    xform_out = ""
    xform_err = ""
    transformed_changed = False
    transformed_target: Optional[Path] = None
    if not candidates:
        if args.run_both:
            src = files[0]
            ok_orig, orig_out, orig_err = fbuild.compile_and_run_source(
                src,
                label="original",
                quiet_run=args.quiet_run,
                keep_exe=args.keep_exe,
                exe_path=Path(f"{src.stem}_orig.exe"),
            )
            if not ok_orig:
                return 5
        if args.run_diff:
            print("Run diff: SKIP (no transformations suggested)")
        if args.out is not None:
            src = files[0]
            if src.resolve() != args.out.resolve():
                shutil.copy2(src, args.out)
            if args.tee:
                txt = args.out.read_text(encoding="utf-8", errors="ignore")
                if args.tee_both:
                    print(f"--- original: {src} ---")
                    print(src.read_text(encoding="utf-8", errors="ignore"), end="")
                    if not txt.endswith("\n"):
                        print("")
                    print(f"--- transformed: {args.out} ---")
                print(txt, end="")
                if not txt.endswith("\n"):
                    print("")
            print(f"No DATA-to-constant candidates found. Wrote unchanged output to {args.out}")
            return 0
        print("No DATA-to-constant candidates found.")
        return 0

    candidates.sort(key=lambda c: (c.path.name.lower(), c.first_data_line, c.var))
    print(f"{len(candidates)} DATA-to-constant candidate(s).")
    if args.verbose:
        for c in candidates:
            print(
                f"{c.path.name}:{c.first_data_line} {c.unit_kind} {c.unit_name}: "
                f"{c.var} (DATA entries: {c.data_count})"
            )
    else:
        by_name: Dict[str, int] = {}
        for c in candidates:
            by_name[c.path.name] = by_name.get(c.path.name, 0) + 1
        for fn in sorted(by_name.keys(), key=str.lower):
            print(f"{fn}: {by_name[fn]}")
        first = candidates[0]
        print(
            f"\nFirst candidate: {first.path.name}:{first.first_data_line} "
            f"{first.unit_kind} {first.unit_name}:{first.var}"
        )
        print("Use --verbose for full details.")
    if args.fix:
        touched = 0
        decl_changed = 0
        data_removed = 0
        for p in sorted(by_file.keys(), key=lambda x: x.name.lower()):
            out_path = args.out if args.out is not None else None
            old_text = p.read_text(encoding="utf-8", errors="ignore")
            if out_path is not None and args.tee_both:
                print(f"--- original: {p} ---")
                print(old_text, end="")
                if not old_text.endswith("\n"):
                    print("")
            work_path = p
            create_backup = args.backup
            if out_path is not None and p.resolve() != out_path.resolve():
                shutil.copy2(p, out_path)
                work_path = out_path
                create_backup = False
            n_decl_total = 0
            n_data_total = 0
            backup_last: Optional[Path] = None
            for _pass in range(8):
                cands = analyze_file(work_path)
                if not cands:
                    break
                n_decl, n_data, backup = apply_fix_file(
                    work_path,
                    cands,
                    fix_arrays=args.fix_arrays,
                    out_path=out_path if (out_path is not None and work_path.resolve() != out_path.resolve()) else None,
                    create_backup=create_backup,
                )
                create_backup = False
                if backup is not None:
                    backup_last = backup
                if n_decl == 0 and n_data == 0:
                    break
                n_decl_total += n_decl
                n_data_total += n_data
                if out_path is not None:
                    work_path = out_path
            if n_decl_total == 0 and n_data_total == 0:
                if out_path is not None and args.tee:
                    txt = out_path.read_text(encoding="utf-8", errors="ignore")
                    if args.tee_both:
                        print(f"--- transformed: {out_path} ---")
                    print(txt, end="")
                    if not txt.endswith("\n"):
                        print("")
                if args.verbose:
                    print(f"\nNo conservative DATA fixes applied in {p.name}")
                continue
            transformed_changed = True
            transformed_target = out_path if out_path is not None else p
            touched += 1
            decl_changed += n_decl_total
            data_removed += n_data_total
            if out_path is not None and args.tee:
                txt = out_path.read_text(encoding="utf-8", errors="ignore")
                print(f"--- transformed: {out_path} ---")
                print(txt, end="")
                if not txt.endswith("\n"):
                    print("")
            if out_path is not None:
                print(
                    f"\nFixed {p.name}: declarations {n_decl_total}, "
                    f"data lines removed {n_data_total}, wrote {out_path}"
                )
            else:
                print(
                    f"\nFixed {p.name}: declarations {n_decl_total}, data lines removed {n_data_total}, "
                    f"backup {backup_last.name if backup_last else '(none)'}"
                )
        print(
            f"\n--fix summary: files changed {touched}, declaration edits {decl_changed}, "
            f"data lines removed {data_removed}"
        )
        if args.out is not None and touched == 0:
            src = files[0]
            if src.resolve() != args.out.resolve():
                shutil.copy2(src, args.out)
            print(f"No conservative fixes were applicable. Wrote unchanged output to {args.out}")
        if args.fix and args.compiler:
            if not fbuild.run_compiler_command(args.compiler, after_compile_paths, "after-fix", fscan.display_path):
                return 5
        if args.run_both:
            src = files[0]
            ok_orig, orig_out, orig_err = fbuild.compile_and_run_source(
                src,
                label="original",
                quiet_run=args.quiet_run,
                keep_exe=args.keep_exe,
                exe_path=Path(f"{src.stem}_orig.exe"),
            )
            if not ok_orig:
                return 5
        if args.run and transformed_changed and transformed_target is not None:
            ok_xf, xform_out, xform_err = fbuild.compile_and_run_source(
                transformed_target,
                label="transformed",
                quiet_run=args.quiet_run,
                keep_exe=args.keep_exe,
                exe_path=Path(f"{transformed_target.stem}.exe"),
            )
            if not ok_xf:
                return 5
        if args.run_diff:
            if not transformed_changed:
                print("Run diff: SKIP (no transformations applied)")
            else:
                same = (orig_out == xform_out) and (orig_err == xform_err)
                if same:
                    print("Run diff: MATCH")
                else:
                    print("Run diff: DIFF")
                    ob = f"STDOUT:\n{orig_out}\nSTDERR:\n{orig_err}\n"
                    tb = f"STDOUT:\n{xform_out}\nSTDERR:\n{xform_err}\n"
                    for line in difflib.unified_diff(
                        ob.splitlines(), tb.splitlines(), fromfile="original", tofile="transformed", lineterm=""
                    ):
                        print(line)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
