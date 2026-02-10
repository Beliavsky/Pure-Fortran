#!/usr/bin/env python3
"""Advisory checker for Fortran variables that could be named constants."""

from __future__ import annotations

import argparse
import re
import shutil
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Set, Tuple

import fortran_scan as fscan
import xunset

TYPE_DECL_RE = re.compile(
    r"^\s*(integer|real|logical|character|complex|type\b|class\b|procedure\b)",
    re.IGNORECASE,
)
ASSIGN_RE = re.compile(r"^\s*([a-z][a-z0-9_]*(?:\s*(?:\([^)]*\)|%\s*[a-z][a-z0-9_]*))?)\s*=", re.IGNORECASE)
POINTER_ASSIGN_RE = re.compile(r"^\s*([a-z][a-z0-9_]*)\s*=>", re.IGNORECASE)
ALLOCATE_RE = re.compile(r"^\s*(allocate|deallocate|nullify)\s*\((.*)\)\s*$", re.IGNORECASE)
READ_RE = re.compile(r"^\s*read\b", re.IGNORECASE)
DATA_ITEM_RE = re.compile(r"([a-z][a-z0-9_]*(?:\([^/]*\))?)\s*/([^/]*)/", re.IGNORECASE)
CALL_RE = re.compile(r"^\s*call\s+([a-z][a-z0-9_]*)\s*(\((.*)\))?\s*$", re.IGNORECASE)
IDENT_RE = re.compile(r"\b([a-z][a-z0-9_]*)\b", re.IGNORECASE)
INTENT_RE = re.compile(r"\bintent\s*\(\s*(inout|out|in)\s*\)", re.IGNORECASE)
PROC_DECL_RE = re.compile(
    r"^\s*(?:(?:pure|elemental|impure|recursive|module)\s+)*(function|subroutine)\s+([a-z][a-z0-9_]*)\s*(\([^)]*\))?",
    re.IGNORECASE,
)
NONDET_TOKEN_RE = re.compile(
    r"\b(random_number|random_seed|read|open|inquire|close|system_clock|cpu_time|date_and_time|get_command_argument|get_environment_variable|execute_command_line)\b",
    re.IGNORECASE,
)
CALL_LIKE_RE = re.compile(r"\b([a-z][a-z0-9_]*)\s*\(", re.IGNORECASE)
LITERAL_WORDS = {"true", "false", "null"}
BLOCK_START_RE = re.compile(
    r"^\s*(if\s*\(.*\)\s*then\b|do\b|select\s+(case|type|rank)\b|where\b|forall\b|associate\b|block\b|critical\b)",
    re.IGNORECASE,
)
BLOCK_END_RE = re.compile(
    r"^\s*end\s*(if|do|select|where|forall|associate|block|critical)\b",
    re.IGNORECASE,
)


@dataclass
class ProcSignature:
    """Procedure signature known from the same source file."""

    name: str
    dummy_order: List[str]
    dummy_intent: Dict[str, str]


@dataclass
class Candidate:
    """Variable that appears set once and deterministic."""

    path: Path
    unit_kind: str
    unit_name: str
    name: str
    decl_line: int
    first_write_line: int
    first_write_detail: str
    first_write_expr: str
    expr_refs: List[str]
    unit_start: int
    unit_end: int


@dataclass
class Exclusion:
    """Reason a declared local variable was not considered a candidate."""

    path: Path
    unit_kind: str
    unit_name: str
    name: str
    decl_line: int
    reason: str


@dataclass
class FixSkip:
    """Reason a candidate was not auto-fixed."""

    path: Path
    unit_kind: str
    unit_name: str
    name: str
    reason: str


def choose_files(args_files: List[Path], exclude: Iterable[str]) -> List[Path]:
    """Resolve source files from args or current-directory defaults."""
    if args_files:
        files = args_files
    else:
        files = sorted(
            set(Path(".").glob("*.f90")) | set(Path(".").glob("*.F90")),
            key=lambda p: p.name.lower(),
        )
    return fscan.apply_excludes(files, exclude)


def split_top_level_commas(text: str) -> List[str]:
    """Split text on top-level commas only."""
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


def parse_decl_entities(stmt: str) -> List[Tuple[str, bool, bool, str, str]]:
    """Parse declaration entities as (name, has_init, has_shape, init_expr, shape_text)."""
    if "::" not in stmt:
        return []
    rhs = stmt.split("::", 1)[1]
    out: List[Tuple[str, bool, bool, str, str]] = []
    for chunk in split_top_level_commas(rhs):
        text = chunk.strip()
        if not text:
            continue
        m = re.match(r"^([a-z][a-z0-9_]*)\s*(\([^)]*\))?\s*(.*)$", text, re.IGNORECASE)
        if not m:
            continue
        name = m.group(1).lower()
        shape_text = (m.group(2) or "").strip()
        has_shape = bool(shape_text)
        rest = m.group(3).strip()
        init_expr = ""
        has_init = False
        if "=>" in rest:
            has_init = True
            init_expr = rest.split("=>", 1)[1].strip()
        elif "=" in rest:
            has_init = True
            init_expr = rest.split("=", 1)[1].strip()
        out.append((name, has_init, has_shape, init_expr, shape_text))
    return out


def parse_decl_attrs(stmt: str) -> str:
    """Return normalized declaration attribute spec text."""
    if "::" not in stmt:
        return ""
    return stmt.split("::", 1)[0].strip().lower()


def is_simple_name(expr: str) -> Optional[str]:
    """Return lowercased variable name if expr is a simple scalar variable reference."""
    m = re.match(r"^\s*([a-z][a-z0-9_]*)\s*$", expr, re.IGNORECASE)
    if not m:
        return None
    return m.group(1).lower()


def first_identifier(expr: str) -> Optional[str]:
    """Extract first identifier from expression."""
    return fscan.base_identifier(expr)


def is_nondeterministic(text: str) -> bool:
    """Check whether statement/expression includes known nondeterministic sources."""
    return NONDET_TOKEN_RE.search(text or "") is not None


def strip_quoted_text(text: str) -> str:
    """Replace quoted string content with spaces for token scanning."""
    out: List[str] = []
    in_single = False
    in_double = False
    for ch in text:
        if ch == "'" and not in_double:
            in_single = not in_single
            out.append(" ")
        elif ch == '"' and not in_single:
            in_double = not in_double
            out.append(" ")
        elif in_single or in_double:
            out.append(" ")
        else:
            out.append(ch)
    return "".join(out)


def expr_refs(expr: str) -> Set[str]:
    """Extract identifier references from an expression, excluding literals/implied-do vars."""
    text = strip_quoted_text((expr or "").strip().lower())
    implied_do_vars: Set[str] = set(m.group(1).lower() for m in re.finditer(r"(?:\(|,)\s*([a-z][a-z0-9_]*)\s*=", text))
    refs: Set[str] = set()
    for m in IDENT_RE.finditer(text):
        n = m.group(1).lower()
        if n not in LITERAL_WORDS and n not in implied_do_vars:
            refs.add(n)
    return refs


def is_parameter_safe_expr(expr: str, known_params: Set[str]) -> Tuple[bool, str]:
    """Conservative check for RHS suitability as a PARAMETER initializer."""
    text = strip_quoted_text((expr or "").strip().lower())
    if not text:
        return False, "missing initializer expression"

    if CALL_LIKE_RE.search(text):
        return False, "contains function call"

    refs = expr_refs(text)
    bad = sorted(n for n in refs if n not in known_params)
    if bad:
        return False, f"depends on non-parameter name(s): {', '.join(bad)}"
    return True, ""


def unwrap_single_line_if(stmt: str) -> Tuple[str, bool]:
    """Return inner statement for IF(cond) stmt form and whether conditional."""
    s = stmt.lstrip()
    if not s.lower().startswith("if"):
        return stmt, False
    m = re.match(r"^\s*if\s*\(", s, re.IGNORECASE)
    if not m:
        return stmt, False
    pos = s.find("(")
    if pos < 0:
        return stmt, False
    depth = 0
    end_pos = -1
    in_single = False
    in_double = False
    for i in range(pos, len(s)):
        ch = s[i]
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
                    end_pos = i
                    break
    if end_pos < 0:
        return stmt, False
    tail = s[end_pos + 1 :].strip()
    if not tail:
        return stmt, False
    if tail.lower().startswith("then"):
        return stmt, False
    return tail, True


def split_code_comment(raw_line: str) -> Tuple[str, str]:
    """Split one source line into code and trailing comment text."""
    in_single = False
    in_double = False
    for i, ch in enumerate(raw_line):
        if ch == "'" and not in_double:
            in_single = not in_single
        elif ch == '"' and not in_single:
            in_double = not in_double
        elif ch == "!" and not in_single and not in_double:
            return raw_line[:i], raw_line[i:]
    return raw_line, ""


def add_parameter_attr(left_spec: str) -> str:
    """Insert PARAMETER attribute into declaration spec if absent."""
    parts = split_top_level_commas(left_spec)
    has_parameter = any(p.strip().lower() == "parameter" for p in parts)
    if has_parameter:
        return ", ".join(p.strip() for p in parts if p.strip())
    if not parts:
        return "parameter"
    return ", ".join([parts[0].strip(), "parameter"] + [p.strip() for p in parts[1:] if p.strip()])


def remove_nonparameter_attrs(left_spec: str) -> str:
    """Remove attributes incompatible with PARAMETER."""
    parts = split_top_level_commas(left_spec)
    banned = {"allocatable", "pointer", "target", "save"}
    kept: List[str] = []
    for p in parts:
        t = p.strip()
        if not t:
            continue
        if t.lower() in banned:
            continue
        kept.append(t)
    return ", ".join(kept)


def parse_decl_chunks(rhs: str) -> List[Tuple[str, str]]:
    """Parse declaration RHS into (name, original_chunk) entities."""
    out: List[Tuple[str, str]] = []
    for chunk in split_top_level_commas(rhs):
        text = chunk.strip()
        if not text:
            continue
        m = re.match(r"^([a-z][a-z0-9_]*)", text, re.IGNORECASE)
        if not m:
            continue
        out.append((m.group(1).lower(), text))
    return out


def find_decl_line_for_name(lines: List[str], start_idx: int, end_idx: int, name: str) -> Optional[int]:
    """Find declaration line index for one name inside the given span."""
    lo = max(0, start_idx)
    hi = min(len(lines) - 1, end_idx)
    target = name.lower()
    for idx in range(lo, hi + 1):
        code, _comment = split_code_comment(lines[idx])
        low = code.strip().lower()
        if not low or "::" not in low:
            continue
        if not TYPE_DECL_RE.match(low):
            continue
        _left, rhs = low.split("::", 1)
        entities = parse_decl_chunks(rhs)
        if any(n == target for n, _chunk in entities):
            return idx
    return None


def infer_rank1_constructor_len(expr: str) -> Optional[int]:
    """Infer element count for a rank-1 array constructor like [1,2,3]."""
    s = (expr or "").strip()
    if not (s.startswith("[") and s.endswith("]")):
        return None
    inner = s[1:-1].strip()
    if not inner:
        return 0
    items = split_top_level_commas(inner)
    if any(":" in it for it in items):
        return None
    return len(items)


def make_backup_path(path: Path) -> Path:
    """Create a non-overwriting backup path next to the target file."""
    base = Path(str(path) + ".bak")
    if not base.exists():
        return base
    idx = 1
    while True:
        cand = Path(f"{path}.bak{idx}")
        if not cand.exists():
            return cand
        idx += 1


def parse_proc_signatures(finfo: fscan.SourceFileInfo) -> Dict[str, List[ProcSignature]]:
    """Collect local procedure dummy order and INTENT declarations."""
    by_name: Dict[str, List[ProcSignature]] = {}
    for p in finfo.procedures:
        header = finfo.lines[p.start - 1] if 1 <= p.start <= len(finfo.lines) else ""
        m = PROC_DECL_RE.match(fscan.strip_comment(header).strip().lower())
        if not m:
            continue
        arg_text = m.group(3)
        dummy_order: List[str] = []
        if arg_text:
            inner = arg_text.strip()[1:-1].strip()
            if inner:
                for a in split_top_level_commas(inner):
                    name = is_simple_name(a)
                    if name:
                        dummy_order.append(name)
        dummy_intent: Dict[str, str] = {}
        for _, stmt in p.body:
            low = stmt.strip().lower()
            if not TYPE_DECL_RE.match(low):
                continue
            m_int = INTENT_RE.search(low)
            if not m_int:
                continue
            intent = m_int.group(1).lower()
            for n in fscan.parse_declared_names_from_decl(low):
                dummy_intent[n] = intent
        sig = ProcSignature(name=p.name.lower(), dummy_order=dummy_order, dummy_intent=dummy_intent)
        by_name.setdefault(sig.name, []).append(sig)
    return by_name


def unit_key(kind: str, name: str, start: int) -> Tuple[str, str, int]:
    """Build a stable key for a program unit."""
    return (kind.lower(), name.lower(), start)


def collect_descendant_writes(
    finfo: fscan.SourceFileInfo,
) -> Dict[Tuple[str, str, int], Dict[str, int]]:
    """Collect writes in contained procedures and map them to each ancestor procedure."""
    if not finfo.procedures:
        return {}

    procs = sorted(finfo.procedures, key=lambda p: (p.start, p.end))
    n = len(procs)
    parent_idx: List[int] = [-1] * n
    children: List[List[int]] = [[] for _ in range(n)]
    for i, p in enumerate(procs):
        best = -1
        best_span = 10**18
        for j, q in enumerate(procs):
            if i == j:
                continue
            if q.start < p.start and q.end >= p.end:
                span = q.end - q.start
                if span < best_span:
                    best = j
                    best_span = span
        parent_idx[i] = best
        if best >= 0:
            children[best].append(i)

    local_declared: List[Set[str]] = []
    free_writes: List[Dict[str, int]] = []
    for p in procs:
        declared: Set[str] = set(p.dummy_names)
        writes: Dict[str, int] = {}
        for _ln, stmt in p.body:
            low = stmt.strip().lower()
            if not low:
                continue
            active_stmt, _ = unwrap_single_line_if(low)
            if TYPE_DECL_RE.match(active_stmt) and "::" in active_stmt:
                for name, _has_init, _has_shape, _init, _shape_text in parse_decl_entities(active_stmt):
                    declared.add(name)
                continue
            m_assign = ASSIGN_RE.match(active_stmt)
            if m_assign:
                lhs = m_assign.group(1).strip()
                base = first_identifier(lhs)
                if base and base not in declared:
                    writes[base] = writes.get(base, 0) + 1
        local_declared.append(declared)
        free_writes.append(writes)

    descendant_for_idx: List[Dict[str, int]] = [dict() for _ in range(n)]

    def dfs(idx: int) -> Dict[str, int]:
        accum: Dict[str, int] = {}
        for cidx in children[idx]:
            child_total = dfs(cidx)
            for name, cnt in child_total.items():
                accum[name] = accum.get(name, 0) + cnt
        descendant_for_idx[idx] = accum
        own_total: Dict[str, int] = dict(free_writes[idx])
        for name, cnt in accum.items():
            own_total[name] = own_total.get(name, 0) + cnt
        return own_total

    roots = [i for i, pidx in enumerate(parent_idx) if pidx < 0]
    for ridx in roots:
        dfs(ridx)

    out: Dict[Tuple[str, str, int], Dict[str, int]] = {}
    for i, p in enumerate(procs):
        out[unit_key(p.kind, p.name, p.start)] = descendant_for_idx[i]
    return out


def map_actuals_to_dummies(sig: ProcSignature, args: List[str]) -> Dict[str, str]:
    """Map dummy name to actual argument text from one call."""
    mapping: Dict[str, str] = {}
    pos = 0
    for arg in args:
        if "=" in arg:
            lhs, rhs = arg.split("=", 1)
            key = lhs.strip().lower()
            val = rhs.strip()
            if re.match(r"^[a-z][a-z0-9_]*$", key):
                mapping[key] = val
            continue
        if pos < len(sig.dummy_order):
            mapping[sig.dummy_order[pos]] = arg.strip()
        pos += 1
    return mapping


def analyze_unit(
    unit: xunset.Unit,
    proc_sigs: Dict[str, List[ProcSignature]],
    descendant_writes: Optional[Dict[str, int]] = None,
    allow_alloc_promotion: bool = False,
) -> Tuple[List[Candidate], List[Exclusion]]:
    """Analyze one unit and return candidates and exclusions."""
    locals_decl_line: Dict[str, int] = {}
    local_ok: Dict[str, bool] = {}
    exclusions: Dict[str, str] = {}
    writes: Dict[str, int] = {}
    first_write_line: Dict[str, int] = {}
    first_write_detail: Dict[str, str] = {}
    first_write_deterministic: Dict[str, bool] = {}
    first_write_expr: Dict[str, str] = {}
    first_write_in_control: Dict[str, bool] = {}
    declared_parameters: Set[str] = set()
    parameter_decl_line: Dict[str, int] = {}
    control_depth = 0

    for d in unit.dummy_names:
        exclusions[d] = "dummy argument"

    for ln, stmt in unit.body:
        low = stmt.strip().lower()
        if not low:
            continue
        if BLOCK_END_RE.match(low):
            control_depth = max(0, control_depth - 1)
        active_stmt, in_inline_if = unwrap_single_line_if(low)
        is_conditional_stmt = (control_depth > 0) or in_inline_if

        if TYPE_DECL_RE.match(active_stmt) and "::" in active_stmt:
            attrs = parse_decl_attrs(active_stmt)
            declared = parse_decl_entities(active_stmt)
            for name, has_init, has_shape, init_expr, shape_text in declared:
                if name not in locals_decl_line:
                    locals_decl_line[name] = ln
                if "parameter" in attrs:
                    exclusions[name] = "already PARAMETER"
                    local_ok[name] = False
                    declared_parameters.add(name)
                    parameter_decl_line[name] = ln
                elif "pointer" in attrs or "target" in attrs:
                    exclusions[name] = "pointer/target declaration"
                    local_ok[name] = False
                elif "allocatable" in attrs:
                    # Optional promotion path, gated by CLI flag.
                    if not allow_alloc_promotion:
                        exclusions[name] = "allocatable declaration (enable with --fix-alloc)"
                        local_ok[name] = False
                    elif not (has_shape and shape_text == "(:)"):
                        exclusions[name] = "allocatable declaration is not rank-1 deferred-shape"
                        local_ok[name] = False
                    else:
                        local_ok.setdefault(name, True)
                elif has_shape and (":" in shape_text or "*" in shape_text):
                    exclusions[name] = "array declaration is not explicit-shape"
                    local_ok[name] = False
                else:
                    local_ok.setdefault(name, True)

                if has_init:
                    writes[name] = writes.get(name, 0) + 1
                    if name not in first_write_line:
                        first_write_line[name] = ln
                        first_write_detail[name] = "declaration initialization"
                        first_write_deterministic[name] = not is_nondeterministic(init_expr)
                        first_write_expr[name] = init_expr
                        first_write_in_control[name] = is_conditional_stmt
            if BLOCK_START_RE.match(low):
                control_depth += 1
            continue

        if active_stmt.startswith("use ") or active_stmt.startswith("implicit ") or active_stmt.startswith("contains"):
            if BLOCK_START_RE.match(low):
                control_depth += 1
            continue

        if active_stmt.startswith("data "):
            tail = active_stmt[4:].strip()
            for m_data in DATA_ITEM_RE.finditer(tail):
                var_spec = m_data.group(1).strip()
                data_vals = m_data.group(2).strip()
                n = first_identifier(var_spec)
                if not n:
                    continue
                writes[n] = writes.get(n, 0) + 1
                if n not in first_write_line:
                    first_write_line[n] = ln
                    first_write_detail[n] = "data statement"
                    first_write_deterministic[n] = True
                    first_write_expr[n] = f"[{data_vals}]"
                    first_write_in_control[n] = is_conditional_stmt
            if BLOCK_START_RE.match(low):
                control_depth += 1
            continue

        m_ptr = POINTER_ASSIGN_RE.match(active_stmt)
        if m_ptr:
            n = m_ptr.group(1).lower()
            writes[n] = writes.get(n, 0) + 1
            if n not in first_write_line:
                first_write_line[n] = ln
                first_write_detail[n] = "pointer assignment"
                first_write_deterministic[n] = False
                first_write_in_control[n] = is_conditional_stmt
            if BLOCK_START_RE.match(low):
                control_depth += 1
            continue

        m_alloc = ALLOCATE_RE.match(active_stmt)
        if m_alloc:
            args = split_top_level_commas(m_alloc.group(2))
            for a in args:
                n = first_identifier(a)
                if not n:
                    continue
                writes[n] = writes.get(n, 0) + 1
                if n not in first_write_line:
                    first_write_line[n] = ln
                    first_write_detail[n] = f"{m_alloc.group(1).lower()} statement"
                    first_write_deterministic[n] = False
                    first_write_in_control[n] = is_conditional_stmt
            if BLOCK_START_RE.match(low):
                control_depth += 1
            continue

        if READ_RE.match(active_stmt):
            after = active_stmt
            if ")" in active_stmt:
                after = active_stmt.rsplit(")", 1)[1]
            for a in split_top_level_commas(after):
                n = first_identifier(a)
                if not n:
                    continue
                writes[n] = writes.get(n, 0) + 1
                if n not in first_write_line:
                    first_write_line[n] = ln
                    first_write_detail[n] = "read statement"
                    first_write_deterministic[n] = False
                    first_write_in_control[n] = is_conditional_stmt
            if BLOCK_START_RE.match(low):
                control_depth += 1
            continue

        m_call = CALL_RE.match(active_stmt)
        if m_call:
            call_name = m_call.group(1).lower()
            call_args_text = m_call.group(3) or ""
            call_args = split_top_level_commas(call_args_text) if call_args_text.strip() else []

            if call_name == "random_number" and call_args:
                n = is_simple_name(call_args[0])
                if n:
                    writes[n] = writes.get(n, 0) + 1
                    if n not in first_write_line:
                        first_write_line[n] = ln
                        first_write_detail[n] = "random_number output"
                        first_write_deterministic[n] = False
                        first_write_in_control[n] = is_conditional_stmt

            if call_name in proc_sigs and len(proc_sigs[call_name]) == 1:
                sig = proc_sigs[call_name][0]
                mapping = map_actuals_to_dummies(sig, call_args)
                for dummy, intent in sig.dummy_intent.items():
                    if intent not in {"out", "inout"}:
                        continue
                    actual = mapping.get(dummy, "")
                    n = is_simple_name(actual)
                    if not n:
                        continue
                    writes[n] = writes.get(n, 0) + 1
                    if n not in first_write_line:
                        first_write_line[n] = ln
                        first_write_detail[n] = f"CALL {call_name} actual for INTENT({intent.upper()})"
                        first_write_deterministic[n] = False
                        first_write_in_control[n] = is_conditional_stmt
            if BLOCK_START_RE.match(low):
                control_depth += 1
            continue

        m_assign = ASSIGN_RE.match(active_stmt)
        if m_assign:
            lhs = m_assign.group(1).strip()
            rhs = active_stmt.split("=", 1)[1] if "=" in active_stmt else ""
            base = first_identifier(lhs)
            if base:
                writes[base] = writes.get(base, 0) + 1
                if base not in first_write_line:
                    first_write_line[base] = ln
                    first_write_detail[base] = "assignment"
                    first_write_deterministic[base] = not is_nondeterministic(rhs)
                    first_write_expr[base] = rhs
                    first_write_in_control[base] = is_conditional_stmt
            if BLOCK_START_RE.match(low):
                control_depth += 1
            continue

        if BLOCK_START_RE.match(low):
            control_depth += 1

    out_candidates: List[Candidate] = []
    out_exclusions: List[Exclusion] = []
    prelim_ok: Set[str] = set()
    desc = descendant_writes or {}

    for name, decl_line in sorted(locals_decl_line.items(), key=lambda kv: kv[1]):
        if name in unit.dummy_names:
            reason = "dummy argument"
        elif not local_ok.get(name, False):
            reason = exclusions.get(name, "unsupported declaration")
        elif desc.get(name, 0) > 0:
            reason = f"assigned in internal procedure ({desc.get(name, 0)} write(s))"
        elif writes.get(name, 0) == 0:
            reason = "never assigned"
        elif writes.get(name, 0) > 1:
            reason = f"assigned {writes.get(name, 0)} times"
        elif first_write_in_control.get(name, False):
            reason = "first set occurs inside control-flow block"
        elif not first_write_deterministic.get(name, False):
            reason = f"first set is non-deterministic ({first_write_detail.get(name, 'unknown')})"
        else:
            prelim_ok.add(name)
            continue

        out_exclusions.append(
            Exclusion(
                path=unit.path,
                unit_kind=unit.kind,
                unit_name=unit.name,
                name=name,
                decl_line=decl_line,
                reason=reason,
            )
        )

    accepted: Set[str] = set()
    known_params = set(declared_parameters)
    pending = sorted(prelim_ok, key=lambda n: first_write_line.get(n, locals_decl_line.get(n, 10**9)))
    pending_reason: Dict[str, str] = {}
    while True:
        progressed = False
        next_pending: List[str] = []
        for name in pending:
            expr = first_write_expr.get(name, "")
            ok, why = is_parameter_safe_expr(expr, known_params)
            if ok:
                accepted.add(name)
                known_params.add(name)
                parameter_decl_line[name] = locals_decl_line.get(name, 10**9)
                progressed = True
            else:
                pending_reason[name] = why
                next_pending.append(name)
        pending = next_pending
        if not progressed:
            break

    for name in sorted(accepted, key=lambda n: first_write_line.get(n, locals_decl_line.get(n, 10**9))):
        decl_line = locals_decl_line[name]
        out_candidates.append(
            Candidate(
                    path=unit.path,
                    unit_kind=unit.kind,
                    unit_name=unit.name,
                    name=name,
                    decl_line=decl_line,
                    first_write_line=first_write_line.get(name, decl_line),
                    first_write_detail=first_write_detail.get(name, "assignment"),
                    first_write_expr=first_write_expr.get(name, ""),
                    expr_refs=sorted(expr_refs(first_write_expr.get(name, ""))),
                    unit_start=unit.start,
                    unit_end=unit.end,
                )
            )

    for name in pending:
        decl_line = locals_decl_line[name]
        why = pending_reason.get(name, "unknown reason")
        out_exclusions.append(
            Exclusion(
                path=unit.path,
                unit_kind=unit.kind,
                unit_name=unit.name,
                name=name,
                decl_line=decl_line,
                reason=f"not PARAMETER-safe: {why}",
            )
        )

    return out_candidates, out_exclusions


def apply_fixes_for_file(
    path: Path, candidates: List[Candidate], aggressive: bool = False
) -> Tuple[int, List[FixSkip], Optional[Path]]:
    """Apply PARAMETER fixes to one file and return stats."""
    lines = path.read_text(encoding="utf-8").splitlines()
    edited = False
    backup_path: Optional[Path] = None
    applied = 0
    skipped: List[FixSkip] = []

    # Process by descending first-write line to keep assignment/data line references stable.
    ordered = sorted(candidates, key=lambda c: (c.first_write_line, c.decl_line, c.name), reverse=True)
    for c in ordered:
        d_idx = c.decl_line - 1
        if d_idx < 0 or d_idx >= len(lines):
            skipped.append(FixSkip(c.path, c.unit_kind, c.unit_name, c.name, "declaration line out of range"))
            continue

        decl_raw = lines[d_idx]
        decl_code, decl_comment = split_code_comment(decl_raw)
        if "::" not in decl_code:
            skipped.append(FixSkip(c.path, c.unit_kind, c.unit_name, c.name, "declaration lacks :: form"))
            continue
        left, rhs = decl_code.split("::", 1)
        entities = parse_decl_chunks(rhs)
        if not entities:
            skipped.append(FixSkip(c.path, c.unit_kind, c.unit_name, c.name, "unable to parse declaration entities"))
            continue
        if c.name not in [n for n, _ in entities]:
            skipped.append(FixSkip(c.path, c.unit_kind, c.unit_name, c.name, "candidate not found on declaration line"))
            continue

        target_chunk = ""
        other_chunks: List[str] = []
        for n, chunk in entities:
            if n == c.name and not target_chunk:
                target_chunk = chunk
            else:
                other_chunks.append(chunk)
        if not target_chunk:
            skipped.append(FixSkip(c.path, c.unit_kind, c.unit_name, c.name, "ambiguous declaration entity"))
            continue
        if len(entities) != 1 and not aggressive:
            skipped.append(
                FixSkip(c.path, c.unit_kind, c.unit_name, c.name, "declaration has multiple entities; split manually first")
            )
            continue

        expr = c.first_write_expr.strip()
        if not expr:
            skipped.append(FixSkip(c.path, c.unit_kind, c.unit_name, c.name, "missing first-write expression"))
            continue

        # For executable assignment fixes, remove only simple standalone assignment lines.
        if c.first_write_detail == "assignment":
            a_idx = c.first_write_line - 1
            if a_idx < 0 or a_idx >= len(lines):
                skipped.append(FixSkip(c.path, c.unit_kind, c.unit_name, c.name, "assignment line out of range"))
                continue
            assign_raw = lines[a_idx]
            assign_code, _ = split_code_comment(assign_raw)
            if ";" in assign_code:
                skipped.append(FixSkip(c.path, c.unit_kind, c.unit_name, c.name, "assignment shares line with other statements"))
                continue
            m = ASSIGN_RE.match(assign_code.strip().lower())
            lhs_name = first_identifier(m.group(1)) if m else None
            if not m or lhs_name != c.name:
                skipped.append(FixSkip(c.path, c.unit_kind, c.unit_name, c.name, "first assignment is not simple variable assignment"))
                continue

        indent = re.match(r"^\s*", decl_raw).group(0) if decl_raw else ""
        left_clean = remove_nonparameter_attrs(left.strip())
        left_new = add_parameter_attr(left_clean)
        target_new = re.sub(r"=\s*.*$", "", target_chunk).strip()
        target_new = re.sub(r"=>\s*.*$", "", target_new).strip()
        if "%" in target_new:
            skipped.append(FixSkip(c.path, c.unit_kind, c.unit_name, c.name, "declaration entity is not a simple scalar name"))
            continue
        if "(:)" in target_new:
            nlen = infer_rank1_constructor_len(expr)
            if nlen is None:
                skipped.append(
                    FixSkip(
                        c.path,
                        c.unit_kind,
                        c.unit_name,
                        c.name,
                        "cannot infer fixed size for allocatable array initializer",
                    )
                )
                continue
            target_new = target_new.replace("(:)", f"({nlen})")
        param_decl_line = f"{indent}{left_new} :: {target_new} = {expr}"

        if backup_path is None:
            backup_path = make_backup_path(path)
            shutil.copy2(path, backup_path)

        inserted_line = False
        cand_idx = d_idx
        if len(entities) == 1:
            lines[d_idx] = f"{param_decl_line}{decl_comment}"
        else:
            lines[d_idx] = f"{indent}{left.strip()} :: {', '.join(other_chunks)}{decl_comment}"
            lines.insert(d_idx + 1, param_decl_line)
            inserted_line = True
            cand_idx = d_idx + 1
        if c.first_write_detail in {"assignment", "data statement"}:
            a_idx = c.first_write_line - 1
            if inserted_line and a_idx > d_idx:
                a_idx += 1
            if a_idx != d_idx:
                del lines[a_idx]
                if a_idx < cand_idx:
                    cand_idx -= 1

        # Reorder generated declaration so referenced names are declared first.
        if c.expr_refs:
            span_start = max(0, c.unit_start - 1)
            span_end = min(len(lines) - 1, c.unit_end - 1)
            max_dep_idx = -1
            for ref in c.expr_refs:
                if ref == c.name:
                    continue
                ridx = find_decl_line_for_name(lines, span_start, span_end, ref)
                if ridx is not None:
                    max_dep_idx = max(max_dep_idx, ridx)
            if max_dep_idx >= cand_idx:
                moved = lines.pop(cand_idx)
                if max_dep_idx > cand_idx:
                    max_dep_idx -= 1
                lines.insert(max_dep_idx + 1, moved)
        edited = True
        applied += 1

    if edited:
        text = "\n".join(lines)
        if path.read_text(encoding="utf-8").endswith("\n"):
            text += "\n"
        path.write_text(text, encoding="utf-8")
    return applied, skipped, backup_path


def analyze_file(path: Path, allow_alloc_promotion: bool) -> Tuple[List[Candidate], List[Exclusion]]:
    """Analyze one source file and return candidates/exclusions."""
    infos, any_missing = fscan.load_source_files([path])
    if not infos or any_missing:
        return [], []
    finfo = infos[0]
    proc_sigs = parse_proc_signatures(finfo)
    desc_writes_map = collect_descendant_writes(finfo)
    candidates: List[Candidate] = []
    exclusions: List[Exclusion] = []
    for unit in xunset.collect_units(finfo):
        dkey = unit_key(unit.kind, unit.name, unit.start)
        cands, excls = analyze_unit(
            unit,
            proc_sigs,
            descendant_writes=desc_writes_map.get(dkey),
            allow_alloc_promotion=allow_alloc_promotion,
        )
        candidates.extend(cands)
        exclusions.extend(excls)
    return candidates, exclusions


def main() -> int:
    """Run candidate constant analysis on selected Fortran files."""
    parser = argparse.ArgumentParser(
        description="Find Fortran local variables that look like constant candidates"
    )
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--verbose", action="store_true", help="Also print excluded locals and reasons")
    parser.add_argument("--fix", action="store_true", help="Rewrite safe candidates as PARAMETER with file backups")
    parser.add_argument(
        "--fix-all",
        action="store_true",
        help="Aggressive fix mode: also split multi-entity declarations when rewriting PARAMETER candidates",
    )
    parser.add_argument(
        "--fix-alloc",
        action="store_true",
        help="Allow allocatable rank-1 deferred-shape arrays to be promoted to PARAMETER when safe",
    )
    args = parser.parse_args()

    files = choose_files(args.fortran_files, args.exclude)
    if not files:
        print("No source files remain after applying --exclude filters.")
        return 2

    infos, any_missing = fscan.load_source_files(files)
    if not infos:
        return 2 if any_missing else 1

    ordered_infos, _ = fscan.order_files_least_dependent(infos)

    all_candidates: List[Candidate] = []
    all_exclusions: List[Exclusion] = []
    for finfo in ordered_infos:
        cands, excls = analyze_file(finfo.path, allow_alloc_promotion=args.fix_alloc)
        all_candidates.extend(cands)
        all_exclusions.extend(excls)

    if not all_candidates:
        print("No constant candidates found.")
    else:
        all_candidates.sort(key=lambda c: (c.path.name.lower(), c.first_write_line, c.name))
        print(f"{len(all_candidates)} constant candidate(s):")
        for c in all_candidates:
            print(
                f"{c.path.name}:{c.first_write_line} {c.unit_kind} {c.unit_name} {c.name} "
                f"(decl@{c.decl_line}, first set: {c.first_write_detail})"
            )

    if args.verbose:
        all_exclusions.sort(key=lambda e: (e.path.name.lower(), e.decl_line, e.name))
        if all_exclusions:
            print("\nExcluded locals:")
            for e in all_exclusions:
                print(f"{e.path.name}:{e.decl_line} {e.unit_kind} {e.unit_name} {e.name} - {e.reason}")

    do_fix = args.fix or args.fix_all
    if do_fix and all_candidates:
        by_file: Dict[Path, List[Candidate]] = {}
        for c in all_candidates:
            by_file.setdefault(c.path, []).append(c)
        total_applied = 0
        total_skipped = 0
        for path in sorted(by_file.keys(), key=lambda p: p.name.lower()):
            file_applied = 0
            file_skipped: List[FixSkip] = []
            backup_name = ""
            max_iter = 100
            for _iter in range(max_iter):
                iter_candidates, _iter_exclusions = analyze_file(path, allow_alloc_promotion=args.fix_alloc)
                if not iter_candidates:
                    break
                applied, skipped, backup = apply_fixes_for_file(path, iter_candidates, aggressive=args.fix_all)
                file_applied += applied
                file_skipped.extend(skipped)
                if backup is not None and not backup_name:
                    backup_name = backup.name
                if applied == 0:
                    break
            total_applied += file_applied
            total_skipped += len(file_skipped)
            if backup_name:
                print(f"\nFixed {path.name}: applied {file_applied}, backup {backup_name}")
            elif file_applied == 0:
                print(f"\nNo fixes applied to {path.name}")
            if args.verbose and file_skipped:
                for s in file_skipped:
                    print(f"{path.name} {s.unit_kind} {s.unit_name} {s.name} - fix skipped: {s.reason}")
        mode = "--fix-all" if args.fix_all else "--fix"
        print(f"\n{mode} summary: applied {total_applied}, skipped {total_skipped}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
