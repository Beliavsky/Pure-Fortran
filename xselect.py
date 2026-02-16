#!/usr/bin/env python3
"""Suggest/fix IF/ELSE IF chains replaceable by SELECT CASE."""

from __future__ import annotations

import argparse
import difflib
import re
import shutil
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, List, Optional, Tuple

import cli_paths as cpaths
import fortran_build as fbuild
import fortran_scan as fscan

IF_THEN_RE = re.compile(r"^\s*if\s*\((.+)\)\s*then\s*$", re.IGNORECASE)
ELSE_IF_RE = re.compile(
    r"^\s*(?:else\s*if|elseif)\s*\((.+)\)\s*then\s*$",
    re.IGNORECASE,
)
ELSE_RE = re.compile(r"^\s*else\s*$", re.IGNORECASE)
END_IF_RE = re.compile(r"^\s*end\s*if\b|^\s*endif\b", re.IGNORECASE)
EQ_RE = re.compile(r"^\s*(.+?)\s*(==|\.eq\.)\s*(.+?)\s*$", re.IGNORECASE)
CMP_RE = re.compile(
    r"^\s*(.+?)\s*(<=|>=|<|>|\.lt\.|\.le\.|\.gt\.|\.ge\.)\s*(.+?)\s*$",
    re.IGNORECASE,
)
ANY_ARRAY_EQ_RE = re.compile(
    r"^\s*any\s*\(\s*([a-z][a-z0-9_]*)\s*(?:==|\.eq\.)\s*\[(.+)\]\s*\)\s*$",
    re.IGNORECASE,
)

SIMPLE_NAME_RE = re.compile(r"^\s*([a-z][a-z0-9_]*)\s*$", re.IGNORECASE)
NUM_LITERAL_RE = re.compile(
    r"^\s*[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[deq][+-]?\d+)?(?:_[a-z0-9_]+)?\s*$",
    re.IGNORECASE,
)
LOGICAL_LITERAL_RE = re.compile(r"^\s*\.(?:true|false)\.\s*$", re.IGNORECASE)
STR_LITERAL_RE = re.compile(r"^\s*(?:'(?:''|[^'])*'|\"(?:\"\"|[^\"])*\")\s*$")
INT_LITERAL_RE = re.compile(r"^\s*([+-]?\d+)(?:_[a-z0-9_]+)?\s*$", re.IGNORECASE)
NAME_TOKEN_RE = re.compile(r"\b([a-z][a-z0-9_]*)\b", re.IGNORECASE)
SIMPLE_ASSIGN_RE = re.compile(r"^\s*([a-z][a-z0-9_]*)\s*=\s*(.+?)\s*$", re.IGNORECASE)
BEGIN_TAG = "! xselect-begin"
END_TAG = "! xselect-end"


@dataclass
class Finding:
    path: Path
    start_line: int
    end_line: int
    selector: str
    cases: List[Tuple[str, int, int]]  # (case_expr, body_start_line, body_end_line)
    default_body: Optional[Tuple[int, int]]
    suggestion: str


def choose_files(args_files: List[Path], exclude: Iterable[str]) -> List[Path]:
    if args_files:
        files = cpaths.expand_path_args(args_files)
    else:
        files = sorted(
            set(Path(".").glob("*.f90")) | set(Path(".").glob("*.F90")),
            key=lambda p: p.name.lower(),
        )
    return fscan.apply_excludes(files, exclude)


def iter_fortran_statements_with_span(lines: Iterable[str]) -> List[Tuple[int, int, str]]:
    """Return semicolon-split statements as (start_line, end_line, statement_text)."""
    out: List[Tuple[int, int, str]] = []
    cur_parts: List[str] = []
    cur_start: Optional[int] = None
    need_more = False
    last_lineno = 0

    for lineno, raw in enumerate(lines, start=1):
        last_lineno = lineno
        code = fscan.strip_comment(raw).rstrip("\r\n")
        seg = code.rstrip()
        if not seg and not need_more:
            continue
        if cur_start is None:
            cur_start = lineno
        if cur_parts:
            lead = seg.lstrip()
            if lead.startswith("&"):
                seg = lead[1:].lstrip()
        seg = seg.rstrip()
        has_trailing_cont = seg.endswith("&")
        if has_trailing_cont:
            seg = seg[:-1].rstrip()
        if seg:
            cur_parts.append(seg)
        need_more = has_trailing_cont
        if need_more:
            continue
        joined = " ".join(cur_parts).strip()
        if joined and cur_start is not None:
            for stmt in fscan.split_fortran_statements(joined):
                if stmt:
                    out.append((cur_start, lineno, stmt))
        cur_parts = []
        cur_start = None

    if cur_parts and cur_start is not None:
        joined = " ".join(cur_parts).strip()
        if joined:
            for stmt in fscan.split_fortran_statements(joined):
                if stmt:
                    out.append((cur_start, last_lineno, stmt))
    return out


def make_backup_path(path: Path) -> Path:
    base = Path(str(path) + ".bak")
    if not base.exists():
        return base
    i = 1
    while True:
        cand = Path(f"{path}.bak{i}")
        if not cand.exists():
            return cand
        i += 1


def split_top_level_commas(text: str) -> List[str]:
    """Split text on top-level commas only (ignore commas in parens/quotes)."""
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


def split_top_level_or(text: str) -> List[str]:
    """Split expression on top-level .or. connectors."""
    out: List[str] = []
    cur: List[str] = []
    depth = 0
    in_single = False
    in_double = False
    i = 0
    s = text
    while i < len(s):
        ch = s[i]
        if ch == "'" and not in_double:
            if in_single and i + 1 < len(s) and s[i + 1] == "'":
                cur.append("''")
                i += 2
                continue
            in_single = not in_single
            cur.append(ch)
            i += 1
            continue
        if ch == '"' and not in_single:
            if in_double and i + 1 < len(s) and s[i + 1] == '"':
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
            if depth == 0 and s[i : i + 4].lower() == ".or.":
                out.append("".join(cur).strip())
                cur = []
                i += 4
                continue
        cur.append(ch)
        i += 1
    tail = "".join(cur).strip()
    if tail:
        out.append(tail)
    return out


def split_top_level_and(text: str) -> List[str]:
    """Split expression on top-level .and. connectors."""
    out: List[str] = []
    cur: List[str] = []
    depth = 0
    in_single = False
    in_double = False
    i = 0
    s = text
    while i < len(s):
        ch = s[i]
        if ch == "'" and not in_double:
            if in_single and i + 1 < len(s) and s[i + 1] == "'":
                cur.append("''")
                i += 2
                continue
            in_single = not in_single
            cur.append(ch)
            i += 1
            continue
        if ch == '"' and not in_single:
            if in_double and i + 1 < len(s) and s[i + 1] == '"':
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
            if depth == 0 and s[i : i + 5].lower() == ".and.":
                out.append("".join(cur).strip())
                cur = []
                i += 5
                continue
        cur.append(ch)
        i += 1
    tail = "".join(cur).strip()
    if tail:
        out.append(tail)
    return out


def split_body_eol(raw: str) -> Tuple[str, str]:
    if raw.endswith("\r\n"):
        return raw[:-2], "\r\n"
    if raw.endswith("\n"):
        return raw[:-1], "\n"
    return raw, ""


def strip_outer_parens(expr: str) -> str:
    """Strip balanced outer parentheses repeatedly."""
    s = expr.strip()
    while s.startswith("(") and s.endswith(")"):
        depth = 0
        in_single = False
        in_double = False
        ok = True
        for i, ch in enumerate(s):
            if ch == "'" and not in_double:
                in_single = not in_single
                continue
            if ch == '"' and not in_single:
                in_double = not in_double
                continue
            if in_single or in_double:
                continue
            if ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
                if depth == 0 and i != len(s) - 1:
                    ok = False
                    break
                if depth < 0:
                    ok = False
                    break
        if not ok or depth != 0:
            break
        s = s[1:-1].strip()
    return s


def normalize_expr(expr: str) -> str:
    return re.sub(r"\s+", "", expr.strip().lower())


def parse_int_case_interval(expr: str) -> Optional[Tuple[Optional[int], Optional[int]]]:
    """Parse integer CASE label item into [low, high] interval (inclusive)."""
    s = expr.strip()
    m_rng = re.match(
        r"^\s*([+-]?\d+(?:_[a-z0-9_]+)?)?\s*:\s*([+-]?\d+(?:_[a-z0-9_]+)?)?\s*$",
        s,
        re.IGNORECASE,
    )
    if m_rng:
        lo_raw = m_rng.group(1)
        hi_raw = m_rng.group(2)
        lo = parse_int_literal(lo_raw) if lo_raw is not None else None
        hi = parse_int_literal(hi_raw) if hi_raw is not None else None
        if lo_raw is not None and lo is None:
            return None
        if hi_raw is not None and hi is None:
            return None
        if lo is not None and hi is not None and lo > hi:
            return None
        return lo, hi
    iv = parse_int_literal(s)
    if iv is None:
        return None
    return iv, iv


def int_intervals_overlap(
    a: Tuple[Optional[int], Optional[int]], b: Tuple[Optional[int], Optional[int]]
) -> bool:
    alo, ahi = a
    blo, bhi = b
    if ahi is not None and blo is not None and ahi < blo:
        return False
    if bhi is not None and alo is not None and bhi < alo:
        return False
    return True


def is_blank_or_comment(raw: str) -> bool:
    body, _eol = split_body_eol(raw)
    st = body.strip()
    return (st == "") or st.startswith("!")


def strip_inline_comment(stmt: str) -> str:
    in_single = False
    in_double = False
    out: List[str] = []
    i = 0
    while i < len(stmt):
        ch = stmt[i]
        if ch == "'" and not in_double:
            if in_single and i + 1 < len(stmt) and stmt[i + 1] == "'":
                out.append("''")
                i += 2
                continue
            in_single = not in_single
            out.append(ch)
            i += 1
            continue
        if ch == '"' and not in_single:
            if in_double and i + 1 < len(stmt) and stmt[i + 1] == '"':
                out.append('""')
                i += 2
                continue
            in_double = not in_double
            out.append(ch)
            i += 1
            continue
        if ch == "!" and not in_single and not in_double:
            break
        out.append(ch)
        i += 1
    return "".join(out).rstrip()


def parse_simple_assignment(stmt: str) -> Optional[Tuple[str, str]]:
    code = strip_inline_comment(stmt).strip()
    if not code:
        return None
    m = SIMPLE_ASSIGN_RE.match(code)
    if not m:
        return None
    lhs = m.group(1).strip()
    rhs = m.group(2).strip()
    if lhs.lower() == "if":
        return None
    return lhs, rhs


def case_single_assignment_target(raw_lines: List[str], finding: Finding) -> Optional[str]:
    lhs_common: Optional[str] = None
    for _expr, bstart, bend in finding.cases:
        stmts: List[str] = []
        for raw in raw_lines[bstart - 1 : bend]:
            if is_blank_or_comment(raw):
                continue
            stmts.append(split_body_eol(raw)[0])
        if len(stmts) != 1:
            return None
        parsed = parse_simple_assignment(stmts[0])
        if parsed is None:
            return None
        lhs, _rhs = parsed
        if lhs_common is None:
            lhs_common = lhs
        elif lhs_common.lower() != lhs.lower():
            return None
    return lhs_common


def find_preceding_assignment(
    raw_lines: List[str], start_line: int, lhs: str
) -> Optional[Tuple[int, str]]:
    idx = start_line - 2
    while idx >= 0 and is_blank_or_comment(raw_lines[idx]):
        idx -= 1
    if idx < 0:
        return None
    parsed = parse_simple_assignment(split_body_eol(raw_lines[idx])[0])
    if parsed is None:
        return None
    lhs0, rhs0 = parsed
    if lhs0.lower() != lhs.lower():
        return None
    return idx, rhs0


def is_case_expr_safe(expr: str) -> bool:
    s = expr.strip()
    return bool(
        INT_LITERAL_RE.match(s)
        or LOGICAL_LITERAL_RE.match(s)
        or STR_LITERAL_RE.match(s)
        or SIMPLE_NAME_RE.match(s)
    )


def parse_simple_eq_selector_case(cond: str) -> Optional[Tuple[str, str]]:
    c = strip_outer_parens(cond)
    m = EQ_RE.match(c)
    if not m:
        return None
    lhs = m.group(1).strip()
    rhs = m.group(3).strip()
    ml = SIMPLE_NAME_RE.match(lhs)
    mr = SIMPLE_NAME_RE.match(rhs)
    if ml and not mr:
        selector = ml.group(1)
        case_expr = rhs
    elif mr and not ml:
        selector = mr.group(1)
        case_expr = lhs
    else:
        return None
    if not is_case_expr_safe(case_expr):
        return None
    toks = {t.lower() for t in NAME_TOKEN_RE.findall(case_expr)}
    if selector.lower() in toks:
        return None
    return selector, case_expr


def parse_int_literal(expr: str) -> Optional[int]:
    m = INT_LITERAL_RE.match(expr.strip())
    if not m:
        return None
    try:
        return int(m.group(1))
    except ValueError:
        return None


def canonical_relop(op: str) -> str:
    """Map symbolic/dotted relational operators to symbolic forms."""
    o = "".join(op.lower().split())
    if o == ".lt.":
        return "<"
    if o == ".le.":
        return "<="
    if o == ".gt.":
        return ">"
    if o == ".ge.":
        return ">="
    return o


def invert_cmp(op: str) -> str:
    return {"<": ">", ">": "<", "<=": ">=", ">=": "<="}[op]


def parse_selector_int_range_case(cond: str) -> Optional[Tuple[str, str]]:
    parts = split_top_level_and(strip_outer_parens(cond))
    if not parts:
        return None

    low: Optional[int] = None
    high: Optional[int] = None
    selector: Optional[str] = None

    for p in parts:
        ps = strip_outer_parens(p)
        m = CMP_RE.match(ps)
        if not m:
            m_eq = EQ_RE.match(ps)
            if not m_eq:
                return None
            lhs = m_eq.group(1).strip()
            rhs = m_eq.group(3).strip()
            ml = SIMPLE_NAME_RE.match(lhs)
            mr = SIMPLE_NAME_RE.match(rhs)
            if ml and not mr:
                sel = ml.group(1)
                val = parse_int_literal(rhs)
            elif mr and not ml:
                sel = mr.group(1)
                val = parse_int_literal(lhs)
            else:
                return None
            if val is None:
                return None
            if selector is None:
                selector = sel
            elif selector.lower() != sel.lower():
                return None
            low = val if low is None else max(low, val)
            high = val if high is None else min(high, val)
            continue

        lhs = m.group(1).strip()
        op = canonical_relop(m.group(2))
        rhs = m.group(3).strip()
        ml = SIMPLE_NAME_RE.match(lhs)
        mr = SIMPLE_NAME_RE.match(rhs)
        sel = None
        val = None
        rel = op
        if ml and not mr:
            sel = ml.group(1)
            val = parse_int_literal(rhs)
        elif mr and not ml:
            sel = mr.group(1)
            val = parse_int_literal(lhs)
            rel = invert_cmp(op)
        else:
            return None
        if val is None:
            return None
        if selector is None:
            selector = sel
        elif selector.lower() != sel.lower():
            return None

        if rel == ">=":
            low = val if low is None else max(low, val)
        elif rel == ">":
            v = val + 1
            low = v if low is None else max(low, v)
        elif rel == "<=":
            high = val if high is None else min(high, val)
        elif rel == "<":
            v = val - 1
            high = v if high is None else min(high, v)
        else:
            return None

    if selector is None:
        return None
    if low is not None and high is not None and low > high:
        return None
    if low is None and high is None:
        return None
    if low is None:
        label = f":{high}"
    elif high is None:
        label = f"{low}:"
    elif low == high:
        label = str(low)
    else:
        label = f"{low}:{high}"
    return selector, label


def parse_selector_cases(cond: str) -> Optional[Tuple[str, List[str]]]:
    cond_s = strip_outer_parens(cond)
    m_any = ANY_ARRAY_EQ_RE.match(cond_s)
    if m_any:
        selector = m_any.group(1)
        raw_list = m_any.group(2)
        items = split_top_level_commas(raw_list)
        if not items:
            return None
        cases: List[str] = []
        for it in items:
            expr = it.strip()
            if not is_case_expr_safe(expr):
                return None
            toks = {t.lower() for t in NAME_TOKEN_RE.findall(expr)}
            if selector.lower() in toks:
                return None
            cases.append(expr)
        return selector, cases

    parsed_range = parse_selector_int_range_case(cond)
    if parsed_range is not None:
        sel, label = parsed_range
        return sel, [label]

    parts = split_top_level_or(cond_s)
    if not parts:
        return None
    selector: Optional[str] = None
    exprs: List[str] = []
    for p in parts:
        parsed = parse_simple_eq_selector_case(p)
        if parsed is None:
            return None
        sel, ex = parsed
        if selector is None:
            selector = sel
        elif selector.lower() != sel.lower():
            return None
        exprs.append(ex)
    if selector is None:
        return None
    return selector, exprs


def build_suggestion(selector: str, cases: List[Tuple[str, int, int]], has_default: bool) -> str:
    parts = [f"select case ({selector})"]
    for expr, _bs, _be in cases:
        parts.append(f"  case ({expr})")
        parts.append("    ...")
    parts.append("  case default")
    parts.append("    ...")
    parts.append("end select")
    return "\n".join(parts)


def analyze_file(path: Path) -> List[Finding]:
    infos, any_missing = fscan.load_source_files([path])
    if not infos or any_missing:
        return []
    finfo = infos[0]
    raw_lines = path.read_text(encoding="utf-8", errors="ignore").splitlines(keepends=True)
    stmts = iter_fortran_statements_with_span(finfo.parsed_lines)
    stmt_count_by_line: dict[int, int] = {}
    for ln, _lend, _s in stmts:
        stmt_count_by_line[ln] = stmt_count_by_line.get(ln, 0) + 1

    out: List[Finding] = []
    n = len(stmts)
    i = 0
    while i < n:
        ln_if, ln_if_end, s_if = stmts[i]
        m_if = IF_THEN_RE.match(s_if.strip())
        if not m_if:
            i += 1
            continue
        if stmt_count_by_line.get(ln_if, 0) != 1:
            i += 1
            continue
        parsed0 = parse_selector_cases(m_if.group(1))
        if parsed0 is None:
            i += 1
            continue
        selector, case0_list = parsed0
        selector_norm = selector.lower()
        controls: List[Tuple[str, int, Optional[List[str]]]] = [("if", i, case0_list)]
        depth = 0
        k = i + 1
        end_idx = -1
        bad = False
        saw_else = False
        while k < n:
            ln, _ln_end, s = stmts[k]
            st = s.strip()
            if IF_THEN_RE.match(st):
                depth += 1
                k += 1
                continue
            if END_IF_RE.match(st):
                if depth == 0:
                    end_idx = k
                    break
                depth -= 1
                k += 1
                continue
            if depth == 0:
                m_elif = ELSE_IF_RE.match(st)
                if m_elif:
                    if saw_else:
                        bad = True
                        break
                    if stmt_count_by_line.get(ln, 0) != 1:
                        bad = True
                        break
                    parsed_n = parse_selector_cases(m_elif.group(1))
                    if parsed_n is None:
                        bad = True
                        break
                    sel_n, case_n_list = parsed_n
                    if sel_n.lower() != selector_norm:
                        bad = True
                        break
                    controls.append(("elseif", k, case_n_list))
                    k += 1
                    continue
                if ELSE_RE.match(st):
                    if saw_else:
                        bad = True
                        break
                    if stmt_count_by_line.get(ln, 0) != 1:
                        bad = True
                        break
                    saw_else = True
                    controls.append(("else", k, None))
                    k += 1
                    continue
            k += 1

        if bad or end_idx < 0:
            i += 1
            continue
        ln_end = stmts[end_idx][0]
        if stmt_count_by_line.get(ln_end, 0) != 1:
            i += 1
            continue

        cases: List[Tuple[str, int, int]] = []
        default_body: Optional[Tuple[int, int]] = None
        case_norms = set()
        int_intervals: List[Tuple[Optional[int], Optional[int]]] = []
        ok = True
        for cidx, (kind, sidx, cexpr) in enumerate(controls):
            ln_ctrl_end = stmts[sidx][1]
            next_line = ln_end
            if cidx + 1 < len(controls):
                next_line = stmts[controls[cidx + 1][1]][0]
            bstart = ln_ctrl_end + 1
            bend = next_line - 1
            if bstart > bend:
                ok = False
                break
            if kind in {"if", "elseif"}:
                assert cexpr is not None
                exprs = cexpr
                for ex in exprs:
                    key = normalize_expr(ex)
                    if key in case_norms:
                        ok = False
                        break
                    iv = parse_int_case_interval(ex)
                    if iv is not None:
                        for piv in int_intervals:
                            if int_intervals_overlap(iv, piv):
                                ok = False
                                break
                        if not ok:
                            break
                        int_intervals.append(iv)
                    case_norms.add(key)
                if not ok:
                    break
                if len(exprs) == 1:
                    case_label = exprs[0]
                else:
                    case_label = ", ".join(exprs)
                cases.append((case_label, bstart, bend))
            else:
                default_body = (bstart, bend)
        if not ok or len(cases) < 2:
            i += 1
            continue

        out.append(
            Finding(
                path=path,
                start_line=ln_if,
                end_line=ln_end,
                selector=selector,
                cases=cases,
                default_body=default_body,
                suggestion=build_suggestion(selector, cases, default_body is not None),
            )
        )
        i = end_idx + 1
    return out


def build_replacement(
    raw_lines: List[str],
    finding: Finding,
    nl: str,
    synthetic_default: Optional[Tuple[str, str]] = None,
) -> List[str]:
    indent = re.match(r"^\s*", split_body_eol(raw_lines[finding.start_line - 1])[0]).group(0)
    out: List[str] = [f"{indent}select case ({finding.selector}){nl}"]

    # Consolidate case labels that have identical bodies (adjacent or not).
    grouped: List[Tuple[List[str], List[str]]] = []
    for expr, bstart, bend in finding.cases:
        labels = [e.strip() for e in split_top_level_commas(expr) if e.strip()]
        body = raw_lines[bstart - 1 : bend]
        merged = False
        for existing_labels, existing_body in grouped:
            if existing_body == body:
                for lb in labels:
                    if lb not in existing_labels:
                        existing_labels.append(lb)
                merged = True
                break
        if not merged:
            grouped.append((labels, body))

    for labels, body in grouped:
        out.append(f"{indent}case ({', '.join(labels)}){nl}")
        out.extend(body)

    if finding.default_body is not None:
        out.append(f"{indent}case default{nl}")
        bstart, bend = finding.default_body
        out.extend(raw_lines[bstart - 1 : bend])
    else:
        # Keep an explicit default branch for readability while preserving behavior.
        out.append(f"{indent}case default{nl}")
        if synthetic_default is not None:
            lhs, rhs = synthetic_default
            out.append(f"{indent}  {lhs} = {rhs}{nl}")
        else:
            out.append(f"{indent}  continue{nl}")
    out.append(f"{indent}end select{nl}")
    return out


def apply_fix(
    path: Path,
    findings: List[Finding],
    out_path: Optional[Path] = None,
    create_backup: bool = True,
) -> Tuple[int, Optional[Path]]:
    if not findings:
        if out_path is not None:
            out_path.write_text(path.read_text(encoding="utf-8", errors="ignore"), encoding="utf-8")
        return 0, None

    raw_lines = path.read_text(encoding="utf-8", errors="ignore").splitlines(keepends=True)
    nl = "\n"
    if raw_lines:
        _b, eol = split_body_eol(raw_lines[0])
        nl = eol or "\n"

    backup: Optional[Path] = None
    changed = 0
    for f in sorted(findings, key=lambda x: x.start_line, reverse=True):
        start_idx = f.start_line - 1
        synth_default: Optional[Tuple[str, str]] = None
        if f.default_body is None:
            lhs = case_single_assignment_target(raw_lines, f)
            if lhs is not None:
                prev = find_preceding_assignment(raw_lines, f.start_line, lhs)
                if prev is not None:
                    prev_idx, rhs = prev
                    start_idx = prev_idx
                    synth_default = (lhs, rhs)
        repl = build_replacement(raw_lines, f, nl, synthetic_default=synth_default)
        raw_lines[start_idx : f.end_line] = repl
        changed += 1

    text_new = "".join(raw_lines)
    if out_path is not None:
        out_path.write_text(text_new, encoding="utf-8")
    else:
        if create_backup:
            backup = make_backup_path(path)
            shutil.copy2(path, backup)
        path.write_text(text_new, encoding="utf-8")
    return changed, backup


def annotate_file(path: Path, findings: List[Finding], *, backup: bool = True) -> Tuple[int, Optional[Path]]:
    if not findings:
        return 0, None
    lines = path.read_text(encoding="utf-8", errors="ignore").splitlines(keepends=True)
    inserted = 0
    for f in sorted(findings, key=lambda x: (x.start_line, x.end_line), reverse=True):
        sidx = f.start_line - 1
        eidx = f.end_line - 1
        if sidx < 0 or eidx < 0 or sidx >= len(lines) or eidx >= len(lines) or sidx > eidx:
            continue
        raw_s = lines[sidx]
        raw_e = lines[eidx]
        indent_s = re.match(r"^\s*", raw_s).group(0) if raw_s else ""
        indent_e = re.match(r"^\s*", raw_e).group(0) if raw_e else ""
        eol_s = "\r\n" if raw_s.endswith("\r\n") else ("\n" if raw_s.endswith("\n") else "\n")
        eol_e = "\r\n" if raw_e.endswith("\r\n") else ("\n" if raw_e.endswith("\n") else "\n")
        if sidx - 1 >= 0 and lines[sidx - 1].strip().lower() == BEGIN_TAG.lower():
            continue
        begin_msg = f"{indent_s}{BEGIN_TAG}{eol_s}"
        end_msg = f"{indent_e}{END_TAG}{eol_e}"
        sugg_lines = f.suggestion.splitlines()
        sugg_msgs = [f"{indent_e}! {sl}  !! suggested replacement by xselect.py{eol_e}" for sl in sugg_lines]
        lines.insert(eidx + 1, end_msg)
        insert_at = eidx + 2
        for sm in sugg_msgs:
            lines.insert(insert_at, sm)
            insert_at += 1
        lines.insert(sidx, begin_msg)
        inserted += 2 + len(sugg_msgs)

    if inserted == 0:
        return 0, None
    backup_path: Optional[Path] = None
    if backup:
        backup_path = make_backup_path(path)
        shutil.copy2(path, backup_path)
    path.write_text("".join(lines), encoding="utf-8")
    return inserted, backup_path


def show_diff(old_text: str, new_text: str, path: Path) -> None:
    diff = difflib.unified_diff(
        old_text.splitlines(keepends=True),
        new_text.splitlines(keepends=True),
        fromfile=str(path),
        tofile=str(path),
    )
    txt = "".join(diff).rstrip()
    if txt:
        print(txt)


def main() -> int:
    ap = argparse.ArgumentParser(description="Suggest/fix IF chains replaceable by SELECT CASE.")
    ap.add_argument("files", nargs="*", type=Path)
    ap.add_argument("--fix", action="store_true", help="Apply fixes in place.")
    ap.add_argument("--annotate", action="store_true", help="Insert annotated suggestion blocks.")
    ap.add_argument("--out", type=Path, help="Write fixed output to separate file (single input file only).")
    ap.add_argument("--backup", dest="backup", action="store_true", default=True)
    ap.add_argument("--no-backup", dest="backup", action="store_false")
    ap.add_argument("--diff", action="store_true", help="Print unified diff for changed files.")
    ap.add_argument("--compiler", type=str, help='Compile check command (supports "{files}").')
    ap.add_argument("--tee", action="store_true")
    ap.add_argument("--tee-both", action="store_true")
    ap.add_argument("--run", action="store_true")
    ap.add_argument("--run-both", action="store_true")
    ap.add_argument("--run-diff", action="store_true")
    ap.add_argument("--quiet-run", action="store_true")
    ap.add_argument("--keep-exe", action="store_true")
    ap.add_argument("--exclude", action="append", default=[])
    ap.add_argument("--verbose", action="store_true")
    ap.add_argument("--limit", type=int, help="Maximum number of files to process.")
    ap.add_argument("--git", action="store_true", help="Commit changed files.")
    args = ap.parse_args()

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
    if args.fix and args.annotate:
        print("--fix and --annotate are mutually exclusive.")
        return 2
    if args.out and len(args.files) != 1:
        print("--out supports exactly one input file.")
        return 2
    if args.tee and args.out is None:
        print("--tee requires --out.")
        return 2
    if args.run and len(args.files) != 1:
        print("--run/--run-both/--run-diff require exactly one input file.")
        return 2

    files = choose_files(list(args.files), args.exclude)
    if not files:
        print("No Fortran files found.")
        return 2

    if args.limit is not None:
        if args.limit < 1:
            print("--limit must be >= 1.")
            return 2
        files = files[: args.limit]

    baseline_ok = True
    if args.fix and args.compiler:
        baseline_ok = fbuild.run_compiler_command(args.compiler, files, "baseline", fscan.display_path)
        if not baseline_ok:
            return 1

    all_findings: List[Finding] = []
    for p in files:
        fnds = analyze_file(p)
        all_findings.extend(fnds)
        if args.verbose and fnds:
            print(f"{fscan.display_path(p)}: {len(fnds)} candidate(s)")
            for f in fnds:
                print(f"  {f.start_line}-{f.end_line} selector {f.selector}")
                print(f"    suggest:\n{f.suggestion}")

    orig_out = ""
    orig_err = ""
    xform_out = ""
    xform_err = ""
    transformed_changed = False
    transformed_target: Optional[Path] = None
    if not all_findings:
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
                return 1
        if args.run_diff:
            print("Run diff: SKIP (no transformations suggested)")
        print("No SELECT CASE replacement candidates found.")
        return 0

    if not args.fix and not args.annotate:
        print(f"{len(all_findings)} SELECT CASE replacement candidate(s).")
        for f in all_findings:
            print(f"{fscan.display_path(f.path)}:{f.start_line}-{f.end_line} selector={f.selector}")
            if args.verbose:
                print(f"  suggest:\n{f.suggestion}")
        return 0

    changed_files: List[Path] = []
    backup_pairs: List[Tuple[Path, Path]] = []
    by_file: dict[Path, List[Finding]] = {}
    for f in all_findings:
        by_file.setdefault(f.path, []).append(f)

    if args.fix:
        for p in files:
            fnds = by_file.get(p, [])
            if not fnds:
                continue
            old_text = p.read_text(encoding="utf-8", errors="ignore")
            out_path = args.out if args.out is not None else None
            if out_path is not None and args.tee_both:
                print(f"--- original: {p} ---")
                print(old_text, end="")
                if not old_text.endswith("\n"):
                    print("")
            changed, backup = apply_fix(p, fnds, out_path=out_path, create_backup=args.backup)
            if changed > 0:
                transformed_changed = True
                new_path = out_path if out_path is not None else p
                transformed_target = new_path
                new_text = new_path.read_text(encoding="utf-8", errors="ignore")
                if args.diff:
                    show_diff(old_text, new_text, p)
                changed_files.append(new_path)
                if backup is not None:
                    backup_pairs.append((p, backup))
                if out_path is not None and args.tee:
                    print(f"--- transformed: {out_path} ---")
                    print(new_text, end="")
                    if not new_text.endswith("\n"):
                        print("")
                if not args.verbose:
                    if out_path is None:
                        print(f"Fixed {fscan.display_path(p)}: replaced {changed} block(s), backup {backup.name if backup else '(none)'}")
                    else:
                        print(f"Fixed {fscan.display_path(p)}: replaced {changed} block(s), wrote {out_path}")
            elif out_path is not None and args.tee:
                new_text = out_path.read_text(encoding="utf-8", errors="ignore")
                if args.tee_both:
                    print(f"--- transformed: {out_path} ---")
                print(new_text, end="")
                if not new_text.endswith("\n"):
                    print("")

        if args.compiler and changed_files:
            ok = fbuild.run_compiler_command(args.compiler, changed_files, "after-fix", fscan.display_path)
            if not ok:
                if args.out is None:
                    fbuild.rollback_backups(backup_pairs, fscan.display_path)
                return 1

        if args.git and changed_files:
            fbuild.git_commit_files(changed_files, "xselect: replace eligible IF chains with SELECT CASE", fscan.display_path)
        print(f"\n--fix summary: files changed {len(changed_files)}, replaced {len(all_findings)}")
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
                return 1
        if args.run and transformed_changed and transformed_target is not None:
            ok_xf, xform_out, xform_err = fbuild.compile_and_run_source(
                transformed_target,
                label="transformed",
                quiet_run=args.quiet_run,
                keep_exe=args.keep_exe,
                exe_path=Path(f"{transformed_target.stem}.exe"),
            )
            if not ok_xf:
                return 1
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
    elif args.annotate:
        touched = 0
        inserted_total = 0
        for p in files:
            fnds = by_file.get(p, [])
            if not fnds:
                continue
            inserted, backup = annotate_file(p, fnds, backup=args.backup)
            inserted_total += inserted
            if inserted > 0:
                touched += 1
                if not args.verbose:
                    print(
                        f"Annotated {fscan.display_path(p)}: inserted {inserted}, backup {backup.name if backup else '(none)'}"
                    )
        print(f"\n--annotate summary: files changed {touched}, inserted {inserted_total}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
