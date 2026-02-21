#!/usr/bin/env python3
"""xf2c.py: small Fortran->C transpiler for a practical subset."""

from __future__ import annotations

import argparse
import glob
import re
import subprocess
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Optional
import fortran_scan as fscan


@dataclass
class Var:
    ctype: str
    is_array: bool = False
    dim: Optional[str] = None
    is_allocatable: bool = False
    intent: Optional[str] = None
    is_external: bool = False
    is_save: bool = False
    optional: bool = False
    is_param: bool = False
    init: Optional[str] = None
    comment: Optional[str] = None


def _strip_comment(line: str) -> str:
    # Use shared quote-aware Fortran comment stripping.
    try:
        return fscan.strip_comment(line)
    except Exception:
        i = line.find("!")
        return line[:i] if i >= 0 else line


def _split_leading_paren_group(stmt: str, keyword: str) -> Optional[tuple[str, str]]:
    """Split `keyword(<group>)<rest>` with quote-aware balanced parens."""
    m = re.match(rf"^\s*{re.escape(keyword)}\s*\(", stmt, re.IGNORECASE)
    if not m:
        return None
    i = m.end() - 1  # points at '('
    depth = 0
    in_single = False
    in_double = False
    start = i + 1
    j = i
    while j < len(stmt):
        ch = stmt[j]
        if ch == "'" and not in_double:
            if in_single and j + 1 < len(stmt) and stmt[j + 1] == "'":
                j += 2
                continue
            in_single = not in_single
            j += 1
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            j += 1
            continue
        if not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
                if depth == 0:
                    inner = stmt[start:j]
                    rest = stmt[j + 1 :]
                    return inner, rest
        j += 1
    return None


def _extract_fortran_comment(line: str) -> Optional[str]:
    in_single = False
    in_double = False
    for i, ch in enumerate(line):
        if ch == "'" and not in_double:
            in_single = not in_single
        elif ch == '"' and not in_single:
            in_double = not in_double
        elif ch == "!" and not in_single and not in_double:
            txt = line[i + 1 :].strip()
            return txt or None
    return None


def _first_unit_doc_comment(unit: Dict[str, object]) -> Optional[str]:
    body_line_nos: List[int] = list(unit.get("body_line_nos", []))
    source_lines: List[str] = list(unit.get("source_lines", []))
    if not body_line_nos or not source_lines:
        return None
    # Prefer standalone comment lines between header and first body statement.
    header_ln = int(unit.get("header_line_no", 1))
    body_start_ln = int(unit.get("body_start_line_no", body_line_nos[0]))
    for ln in range(max(1, header_ln + 1), min(body_start_ln, len(source_lines) + 1)):
        raw = source_lines[ln - 1].strip()
        if not raw:
            continue
        if raw.startswith("!"):
            txt = raw[1:].strip()
            if txt:
                return txt
            continue
        break
    for ln in body_line_nos:
        if not (1 <= ln <= len(source_lines)):
            continue
        raw = source_lines[ln - 1]
        cmt = _extract_fortran_comment(raw)
        if cmt:
            return cmt
        if _strip_comment(raw).strip():
            # Stop at first real statement/declaration if no leading comment.
            break
    return None


def _as_c_inline_comment(text: Optional[str]) -> str:
    if not text:
        return ""
    safe = text.replace("*/", "* /").strip()
    if not safe:
        return ""
    return f" /* {safe} */"


def _fortran_to_c_real_type(text: str) -> str:
    # If code defines dp from kind(1.0d0), use double.
    if re.search(r"kind\s*\(\s*1\.0d0\s*\)", text, re.IGNORECASE):
        return "double"
    return "float"


def _extract_kind_alias_c_types(text: str) -> Dict[str, str]:
    """Extract simple Fortran kind aliases and map to C float/double.

    Examples:
    - integer, parameter :: sp = kind(1.0), dp = kind(1.0d0)
    - integer, parameter :: qp = dp
    """
    out: Dict[str, str] = {}
    lines = text.splitlines()
    decl_re = re.compile(r"^\s*integer\s*,\s*parameter\s*::\s*(.+)$", re.IGNORECASE)
    k_single = re.compile(r"(?i)^\s*kind\s*\(\s*1(?:\.0*)?(?:e[+\-]?\d+)?\s*\)\s*$")
    k_double = re.compile(r"(?i)^\s*kind\s*\(\s*1(?:\.0*)?d[+\-]?\d+\s*\)\s*$")

    pending_alias: Dict[str, str] = {}
    for raw in lines:
        code = _strip_comment(raw).strip()
        if not code:
            continue
        m = decl_re.match(code)
        if not m:
            continue
        rhs_all = m.group(1).strip()
        for ent in [x.strip() for x in _split_args_top_level(rhs_all) if x.strip()]:
            if "=" not in ent:
                continue
            lhs, rhs = [x.strip() for x in ent.split("=", 1)]
            key = lhs.lower()
            rl = rhs.lower()
            if k_single.match(rl):
                out[key] = "float"
            elif k_double.match(rl):
                out[key] = "double"
            elif re.match(r"^[a-z_]\w*$", rl, re.IGNORECASE):
                pending_alias[key] = rl

    # Resolve simple alias chains.
    changed = True
    while changed and pending_alias:
        changed = False
        for k, alias in list(pending_alias.items()):
            if alias in out:
                out[k] = out[alias]
                del pending_alias[k]
                changed = True
    return out


def _eval_int_expr(expr: str) -> Optional[int]:
    s = expr.strip()
    if not re.fullmatch(r"[0-9+\-*/() \t*]+", s):
        return None
    try:
        v = eval(s, {"__builtins__": None}, {})
    except Exception:
        return None
    if isinstance(v, int):
        return v
    return None


def _simplify_int_expr_text(expr: str) -> str:
    v = _eval_int_expr(expr)
    return str(v) if v is not None else expr


def _replace_pow(expr: str) -> str:
    # Conservative repeated replacement for simple operands.
    var = r"[a-z_]\w*(?:\[[^\[\]]+\])*"
    num = r"[0-9]+(?:\.[0-9]*)?(?:[eEdD][+\-]?[0-9]+)?"
    par = r"\([^()]+\)"
    pat = re.compile(
        rf"({var}|{par}|{num})\s*\*\*\s*({var}|{par}|{num})",
        re.IGNORECASE,
    )
    prev = None
    out = expr
    while out != prev:
        prev = out
        out = pat.sub(r"pow(\1, \2)", out)
    return out


def _split_args_top_level(text: str) -> List[str]:
    out: List[str] = []
    cur: List[str] = []
    depth = 0
    bdepth = 0
    in_single = False
    in_double = False
    i = 0
    while i < len(text):
        ch = text[i]
        if ch == "'" and not in_double:
            in_single = not in_single
            cur.append(ch)
            i += 1
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            cur.append(ch)
            i += 1
            continue
        if not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")" and depth > 0:
                depth -= 1
            elif ch == "[":
                bdepth += 1
            elif ch == "]" and bdepth > 0:
                bdepth -= 1
            elif ch == "," and depth == 0 and bdepth == 0:
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


def _split_concat_top_level(text: str) -> List[str]:
    out: List[str] = []
    cur: List[str] = []
    depth = 0
    in_single = False
    in_double = False
    i = 0
    while i < len(text):
        ch = text[i]
        if ch == "'" and not in_double:
            in_single = not in_single
            cur.append(ch)
            i += 1
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            cur.append(ch)
            i += 1
            continue
        if not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")" and depth > 0:
                depth -= 1
            elif ch == "/" and i + 1 < len(text) and text[i + 1] == "/" and depth == 0:
                part = "".join(cur).strip()
                if part:
                    out.append(part)
                cur = []
                i += 2
                continue
        cur.append(ch)
        i += 1
    tail = "".join(cur).strip()
    if tail:
        out.append(tail)
    return out


def _split_colon_top_level(text: str) -> List[str]:
    out: List[str] = []
    cur: List[str] = []
    depth = 0
    bdepth = 0
    in_single = False
    in_double = False
    i = 0
    while i < len(text):
        ch = text[i]
        if ch == "'" and not in_double:
            if in_single and i + 1 < len(text) and text[i + 1] == "'":
                cur.append(ch)
                cur.append(ch)
                i += 2
                continue
            in_single = not in_single
            cur.append(ch)
            i += 1
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            cur.append(ch)
            i += 1
            continue
        if not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")" and depth > 0:
                depth -= 1
            elif ch == "[":
                bdepth += 1
            elif ch == "]" and bdepth > 0:
                bdepth -= 1
            elif ch == ":" and depth == 0 and bdepth == 0:
                out.append("".join(cur).strip())
                cur = []
                i += 1
                continue
        cur.append(ch)
        i += 1
    out.append("".join(cur).strip())
    return out


def _dim_parts(dim: Optional[str]) -> List[str]:
    if not dim:
        return []
    return [p.strip() for p in _split_args_top_level(dim) if p.strip()]


def _is_assumed_shape(dim: Optional[str]) -> bool:
    return any(p == ":" for p in _dim_parts(dim))


def _dim_lb_expr(
    part: str,
    vars_map: Dict[str, Var],
    real_type: str,
    byref_scalars: Optional[set[str]] = None,
    assumed_extents: Optional[Dict[str, List[str]]] = None,
) -> str:
    p = part.strip()
    if p == ":":
        return "1"
    if ":" not in p:
        return "1"
    sp = _split_colon_top_level(p)
    lo = (sp[0] if len(sp) >= 1 else "").strip() or "1"
    return _convert_expr(lo, vars_map, real_type, byref_scalars, assumed_extents)


def _dim_extent_expr(
    part: str,
    vars_map: Dict[str, Var],
    real_type: str,
    byref_scalars: Optional[set[str]] = None,
    assumed_extents: Optional[Dict[str, List[str]]] = None,
) -> str:
    p = part.strip()
    if p == ":":
        return "1"
    if ":" not in p:
        return _convert_expr(p, vars_map, real_type, byref_scalars, assumed_extents)
    sp = _split_colon_top_level(p)
    lo = (sp[0] if len(sp) >= 1 else "").strip() or "1"
    hi = (sp[1] if len(sp) >= 2 else "").strip() or lo
    clo = _convert_expr(lo, vars_map, real_type, byref_scalars, assumed_extents)
    chi = _convert_expr(hi, vars_map, real_type, byref_scalars, assumed_extents)
    return f"(({chi}) - ({clo}) + 1)"


def _extent_param_names(
    arg_name: str,
    rank: int,
    *,
    use_simple_n: bool = False,
) -> List[str]:
    if rank <= 0:
        return []
    if rank == 1:
        return ["n"] if use_simple_n else [f"n_{arg_name}"]
    return [f"n_{arg_name}_{k+1}" for k in range(rank)]


def _dim_product_expr(
    dim: Optional[str],
    vars_map: Dict[str, Var],
    real_type: str,
    byref_scalars: Optional[set[str]] = None,
    assumed_extents: Optional[Dict[str, List[str]]] = None,
) -> str:
    parts = _dim_parts(dim)
    if not parts:
        return "1"
    conv = [_dim_extent_expr(p, vars_map, real_type, byref_scalars, assumed_extents) for p in parts]
    if len(conv) == 1:
        return conv[0]
    return "(" + " * ".join(conv) + ")"


def _dim_product_from_parts(
    parts: List[str],
    vars_map: Dict[str, Var],
    real_type: str,
    byref_scalars: Optional[set[str]] = None,
    assumed_extents: Optional[Dict[str, List[str]]] = None,
) -> str:
    if not parts:
        return "1"
    conv = [_dim_extent_expr(p, vars_map, real_type, byref_scalars, assumed_extents) for p in parts]
    if len(conv) == 1:
        return conv[0]
    return "(" + " * ".join(conv) + ")"


def _fortran_sub_to_linear(idx_parts: List[str], dim_parts: List[str], vars_map: Dict[str, Var], real_type: str, byref_scalars: Optional[set[str]] = None) -> str:
    """Map Fortran subscripts (1-based, column-major) to 0-based linear C index."""
    if len(idx_parts) != len(dim_parts) or not idx_parts:
        return "0"
    lb0 = _dim_lb_expr(dim_parts[0], vars_map, real_type, byref_scalars)
    idx0 = f"({_convert_expr(idx_parts[0], vars_map, real_type, byref_scalars)} - ({lb0}))"
    stride = _dim_extent_expr(dim_parts[0], vars_map, real_type, byref_scalars)
    expr = idx0
    for k in range(1, len(idx_parts)):
        lbk = _dim_lb_expr(dim_parts[k], vars_map, real_type, byref_scalars)
        ik = f"({_convert_expr(idx_parts[k], vars_map, real_type, byref_scalars)} - ({lbk}))"
        expr = f"({expr} + ({stride}) * {ik})"
        if k < len(dim_parts) - 1:
            dk = _dim_extent_expr(dim_parts[k], vars_map, real_type, byref_scalars)
            stride = f"(({stride}) * ({dk}))"
    return expr


def _resolved_dim_parts(
    v: Var,
    var_name: str,
    assumed_extents: Optional[Dict[str, List[str]]],
) -> List[str]:
    dparts = _dim_parts(v.dim)
    if not dparts:
        return []
    if assumed_extents and var_name.lower() in assumed_extents:
        exts = assumed_extents[var_name.lower()]
        out: List[str] = []
        ei = 0
        for d in dparts:
            if d == ":":
                out.append(exts[ei] if ei < len(exts) else "1")
                ei += 1
            else:
                out.append(d)
        return out
    if v.is_allocatable and any(d == ":" for d in dparts):
        exts = _alloc_extent_names(var_name, len(dparts))
        out: List[str] = []
        ei = 0
        for d in dparts:
            if d == ":":
                out.append(exts[ei] if ei < len(exts) else "1")
                ei += 1
            else:
                out.append(d)
        return out
    return dparts


def _alloc_len_name(name: str) -> str:
    return f"__n_{name.lower()}"


def _alloc_extent_names(name: str, rank: int) -> List[str]:
    if rank <= 1:
        return [_alloc_len_name(name)]
    base = name.lower()
    return [f"__n_{base}_{k+1}" for k in range(rank)]


def _rewrite_assumed_shape_calls(
    expr: str,
    vars_map: Dict[str, Var],
    real_type: str,
    byref_scalars: Optional[set[str]],
    assumed_extents: Optional[Dict[str, List[str]]],
    proc_arg_extent_names: Optional[Dict[str, List[List[str]]]],
) -> str:
    if not proc_arg_extent_names:
        return expr
    out: List[str] = []
    i = 0
    n = len(expr)
    while i < n:
        ch = expr[i]
        if not (ch.isalpha() or ch == "_"):
            out.append(ch)
            i += 1
            continue
        j = i + 1
        while j < n and (expr[j].isalnum() or expr[j] == "_"):
            j += 1
        name = expr[i:j]
        k = j
        while k < n and expr[k].isspace():
            k += 1
        if k >= n or expr[k] != "(":
            out.append(name)
            i = j
            continue
        depth = 0
        p = k
        while p < n:
            c = expr[p]
            if c == "(":
                depth += 1
            elif c == ")":
                depth -= 1
                if depth == 0:
                    break
            p += 1
        if p >= n:
            out.append(name)
            i = j
            continue
        inner = expr[k + 1 : p]
        callee = name.lower()
        ex_lists = proc_arg_extent_names.get(callee, [])
        if not any(ex_lists):
            out.append(expr[i : p + 1])
            i = p + 1
            continue
        raw_args = _split_args_top_level(inner) if inner.strip() else []
        new_args: List[str] = []
        for ai, a in enumerate(raw_args):
            exts = ex_lists[ai] if ai < len(ex_lists) else []
            if exts:
                m_id = re.match(r"^\s*([a-z_]\w*)\s*$", a, re.IGNORECASE)
                if m_id:
                    nm = m_id.group(1).lower()
                    vv = vars_map.get(nm)
                    if vv is not None and vv.is_array:
                        dps = _resolved_dim_parts(vv, nm, assumed_extents)
                        if len(dps) >= len(exts):
                            for d in dps[: len(exts)]:
                                new_args.append(_convert_expr(d, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names))
                        else:
                            new_args.extend(["1"] * len(exts))
                    else:
                        new_args.extend(["1"] * len(exts))
                else:
                    new_args.extend(["1"] * len(exts))
            new_args.append(a.strip())
        out.append(f"{name}({', '.join(new_args)})")
        i = p + 1
    return "".join(out)


def _convert_expr(
    expr: str,
    vars_map: Dict[str, Var],
    real_type: str,
    byref_scalars: Optional[set[str]] = None,
    assumed_extents: Optional[Dict[str, List[str]]] = None,
    proc_arg_extent_names: Optional[Dict[str, List[List[str]]]] = None,
) -> str:
    out = expr.strip()
    # Convert Fortran single-quoted strings to C double-quoted strings.
    out = re.sub(r"'([^']*)'", lambda m: '"' + m.group(1).replace('"', '\\"') + '"', out)
    # Remove kind suffixes only from numeric literals, not identifiers.
    out = re.sub(
        r"(?i)\b(([0-9]+(?:\.[0-9]*)?|\.[0-9]+)(?:[ed][+\-]?[0-9]+)?)_(?:dp|sp)\b",
        r"\1",
        out,
    )
    out = re.sub(r"(?i)\bkind\s*\(\s*[^)]*[d][+\-]?\d+\s*\)", "8", out)
    out = re.sub(r"(?i)\bkind\s*\(\s*1(?:\.0*)?(?:[e][+\-]?0)?\s*\)", "4", out)
    out = re.sub(r"([0-9])d([+\-]?[0-9]+)", r"\1e\2", out, flags=re.IGNORECASE)
    out = re.sub(
        r"(?i)\breal\s*\(\s*([^,]+)\s*,\s*kind\s*=\s*(?:dp|sp)\s*\)",
        rf"(({real_type}) (\1))",
        out,
    )
    out = re.sub(r"(?i)\bint8\b", "1", out)
    out = re.sub(r"(?i)\bint16\b", "2", out)
    out = re.sub(r"(?i)\bint32\b", "4", out)
    out = re.sub(r"(?i)\bint64\b", "8", out)
    out = re.sub(r"(?i)\breal32\b", "4", out)
    out = re.sub(r"(?i)\breal64\b", "8", out)
    out = re.sub(r"(?i)\breal128\b", "16", out)
    out = re.sub(r"(?i)\.and\.", "&&", out)
    out = re.sub(r"(?i)\.or\.", "||", out)
    out = re.sub(r"(?i)\.not\.", "!", out)
    out = re.sub(r"(?i)\.true\.", "1", out)
    out = re.sub(r"(?i)\.false\.", "0", out)
    out = re.sub(r"(?i)\bpresent\s*\(\s*([a-z_]\w*)\s*\)", r"(\1 != NULL)", out)
    out = _rewrite_assumed_shape_calls(
        out, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names
    )
    out = re.sub(r'(?i)\b([a-z_]\w*)\s*==\s*("[^"]*")', r"strcmp(\1, \2) == 0", out)
    out = re.sub(r'(?i)\b([a-z_]\w*)\s*!=\s*("[^"]*")', r"strcmp(\1, \2) != 0", out)
    out = re.sub(r"(?i)\bepsilon\s*\(\s*[^)]+\s*\)", "DBL_EPSILON" if real_type == "double" else "FLT_EPSILON", out)

    # SUM lowering for simple whole-array forms: sum(x), sum(x2d)
    def _sum_repl(m: re.Match[str]) -> str:
        arr = m.group(1).lower()
        v = vars_map.get(arr)
        if v is None or not v.is_array:
            return m.group(0)
        dparts = _resolved_dim_parts(v, arr, assumed_extents)
        rank = max(1, len(dparts))
        cty = (v.ctype or real_type).lower()
        if cty == "double":
            suf = "double"
        elif cty == "int":
            suf = "int"
        else:
            suf = "float"
        if rank >= 2 and len(dparts) >= 2:
            d1 = _convert_expr(dparts[0], vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
            d2 = _convert_expr(dparts[1], vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
            return f"sum_2d_{suf}({d1}, {d2}, {arr})"
        n1 = _dim_product_expr(v.dim, vars_map, real_type, byref_scalars, assumed_extents)
        return f"sum_1d_{suf}({n1}, {arr})"

    out = re.sub(r"(?i)\bsum\s*\(\s*([a-z_]\w*)\s*\)", _sum_repl, out)

    # PRODUCT lowering for simple whole-array forms: product(x), product(x2d)
    def _prod_repl(m: re.Match[str]) -> str:
        arr = m.group(1).lower()
        v = vars_map.get(arr)
        if v is None or not v.is_array:
            return m.group(0)
        dparts = _resolved_dim_parts(v, arr, assumed_extents)
        rank = max(1, len(dparts))
        cty = (v.ctype or real_type).lower()
        suf = "double" if cty == "double" else "float"
        if rank >= 2 and len(dparts) >= 2:
            d1 = _convert_expr(dparts[0], vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
            d2 = _convert_expr(dparts[1], vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
            return f"product_2d_{suf}({d1}, {d2}, {arr})"
        n1 = _dim_product_expr(v.dim, vars_map, real_type, byref_scalars, assumed_extents)
        return f"product_1d_{suf}({n1}, {arr})"

    out = re.sub(r"(?i)\bproduct\s*\(\s*([a-z_]\w*)\s*\)", _prod_repl, out)
    # MINVAL/MAXVAL lowering for simple whole-array forms.
    def _minmax_repl(m: re.Match[str], kind: str) -> str:
        arr = m.group(1).lower()
        v = vars_map.get(arr)
        if v is None or not v.is_array:
            return m.group(0)
        n1 = _dim_product_expr(v.dim, vars_map, real_type, byref_scalars, assumed_extents)
        cty = (v.ctype or real_type).lower()
        if cty == "double":
            suf = "double"
        elif cty == "int":
            suf = "int"
        else:
            suf = "float"
        return f"{kind}_1d_{suf}({n1}, {arr})"

    out = re.sub(r"(?i)\bminval\s*\(\s*([a-z_]\w*)\s*\)", lambda m: _minmax_repl(m, "minval"), out)
    out = re.sub(r"(?i)\bmaxval\s*\(\s*([a-z_]\w*)\s*\)", lambda m: _minmax_repl(m, "maxval"), out)
    out = _replace_pow(out)
    # SIZE lowering for assumed-shape (and known-shape) arrays.
    def _size_repl(m: re.Match[str]) -> str:
        arr = m.group(1).lower()
        dim_txt = (m.group(2) or "").strip()
        dim_no = None
        if dim_txt:
            try:
                dim_no = int(dim_txt)
            except Exception:
                dim_no = None
        # Prefer explicit assumed-shape extents for dummy arrays.
        if assumed_extents and arr in assumed_extents and assumed_extents[arr]:
            exts = assumed_extents[arr]
            if dim_no is not None and 1 <= dim_no <= len(exts):
                return exts[dim_no - 1]
            if len(exts) == 1:
                return exts[0]
            return "(" + " * ".join(exts) + ")"
        v = vars_map.get(arr)
        if v is not None and v.is_array:
            if v.is_allocatable:
                dps_alloc = _resolved_dim_parts(v, arr, assumed_extents)
                if dim_no is not None and 1 <= dim_no <= len(dps_alloc):
                    return dps_alloc[dim_no - 1]
                if dim_no is None:
                    return _dim_product_from_parts(dps_alloc, vars_map, real_type, byref_scalars, assumed_extents)
            dps = _dim_parts(v.dim)
            if dim_no is not None and 1 <= dim_no <= len(dps):
                return _convert_expr(dps[dim_no - 1], vars_map, real_type, byref_scalars, assumed_extents)
            return _dim_product_expr(v.dim, vars_map, real_type, byref_scalars)
        return m.group(0)

    out = re.sub(
        r"(?i)\bsize\s*\(\s*([a-z_]\w*)\s*(?:,\s*([0-9]+)\s*)?\)",
        _size_repl,
        out,
    )
    out = re.sub(r"(?i)\babs\s*\(", "fabsf(" if real_type == "float" else "fabs(", out)
    if real_type == "float":
        out = re.sub(r"(?i)\bmin\s*\(", "fminf(", out)
        out = re.sub(r"(?i)\bmax\s*\(", "fmaxf(", out)
    else:
        out = re.sub(r"(?i)\bmin\s*\(", "fmin(", out)
        out = re.sub(r"(?i)\bmax\s*\(", "fmax(", out)

    # Array indexing: x(i) -> x[(i)-1] for known rank-1 arrays.
    for name, v in vars_map.items():
        if not v.is_array:
            continue
        pat = re.compile(rf"\b{re.escape(name)}\s*\(\s*([^)]+?)\s*\)", re.IGNORECASE)
        def _arr_idx(m: re.Match[str]) -> str:
            idx_raw = m.group(1).strip()
            idx_parts = _split_args_top_level(idx_raw)
            dparts = _resolved_dim_parts(v, name, assumed_extents)
            if len(idx_parts) <= 1:
                i_expr = _convert_expr(
                    idx_parts[0] if idx_parts else idx_raw,
                    vars_map,
                    real_type,
                    byref_scalars,
                    assumed_extents,
                )
                i_expr = _simplify_int_expr_text(i_expr.strip())
                iv = _eval_int_expr(i_expr)
                lb1 = _dim_lb_expr(dparts[0], vars_map, real_type, byref_scalars, assumed_extents) if dparts else "1"
                if iv is not None:
                    lbv = _eval_int_expr(lb1)
                    if lbv is not None:
                        return f"{name}[{iv - lbv}]"
                return f"{name}[({i_expr}) - ({lb1})]"
            # Multi-dimensional: flatten in Fortran column-major order.
            if len(dparts) == len(idx_parts):
                lin = _fortran_sub_to_linear(idx_parts, dparts, vars_map, real_type, byref_scalars)
                return f"{name}[{lin}]"
            # Fallback: keep first index behavior if rank information mismatches.
            i_expr = _convert_expr(idx_parts[0], vars_map, real_type, byref_scalars, assumed_extents)
            lb1 = _dim_lb_expr(dparts[0], vars_map, real_type, byref_scalars, assumed_extents) if dparts else "1"
            return f"{name}[({i_expr}) - ({lb1})]"
        out = pat.sub(_arr_idx, out)
    if byref_scalars:
        for nm in sorted(byref_scalars, key=len, reverse=True):
            out = re.sub(rf"\b{re.escape(nm)}\b", f"*{nm}", out)
    # Keep present(...) lowering stable even when optional scalars are byref-dereferenced.
    out = re.sub(r"\*\s*([a-z_]\w*)\s*!=\s*NULL", r"\1 != NULL", out, flags=re.IGNORECASE)
    out = re.sub(r"NULL\s*!=\s*\*\s*([a-z_]\w*)", r"NULL != \1", out, flags=re.IGNORECASE)
    out = re.sub(r"\*\s*([a-z_]\w*)\s*==\s*NULL", r"\1 == NULL", out, flags=re.IGNORECASE)
    out = re.sub(r"NULL\s*==\s*\*\s*([a-z_]\w*)", r"NULL == \1", out, flags=re.IGNORECASE)
    return out


def _parse_decls(
    unit: Dict[str, object],
    real_type: str,
    kind_ctype_map: Optional[Dict[str, str]] = None,
) -> Dict[str, Var]:
    vars_map: Dict[str, Var] = {}
    body_line_nos: List[int] = list(unit.get("body_line_nos", []))
    source_lines: List[str] = list(unit.get("source_lines", []))
    for idx, raw in enumerate(unit["body_lines"]):
        code = _strip_comment(raw).strip()
        inline_comment = None
        if idx < len(body_line_nos):
            ln = body_line_nos[idx]
            if 1 <= ln <= len(source_lines):
                inline_comment = _extract_fortran_comment(source_lines[ln - 1])
        if not code:
            continue
        is_optional = bool(re.search(r"(?i),\s*optional\b", code))
        code_no_opt = re.sub(r"(?i),\s*optional\b", "", code)
        code_int_norm = re.sub(
            r"(?i)^integer\s*\(\s*kind\s*=\s*[^)]+\s*\)",
            "integer",
            code_no_opt,
        )
        m_int = re.match(
            r"^integer(?:\s*,\s*intent\s*\(\s*(in|out|inout)\s*\))?(\s*,\s*parameter)?\s*::\s*(.+)$",
            code_int_norm,
            re.IGNORECASE,
        )
        m_int_attr = re.match(
            r"^integer\s*(?:,\s*([^:]+))?\s*::\s*(.+)$",
            code_int_norm,
            re.IGNORECASE,
        )
        m_int_no_colon = re.match(
            r"^integer(?:\s*,\s*intent\s*\(\s*(in|out|inout)\s*\))?(\s*,\s*parameter)?\s+(.+)$",
            code_int_norm,
            re.IGNORECASE,
        )
        if m_int:
            intent = m_int.group(1).lower() if m_int.group(1) else None
            is_param = bool(m_int.group(2))
            for ent in [x.strip() for x in _split_args_top_level(m_int.group(3)) if x.strip()]:
                if "=" in ent:
                    nm, init = [x.strip() for x in ent.split("=", 1)]
                else:
                    nm, init = ent, None
                ma = re.match(r"^([a-z_]\w*)\s*\(\s*(.+)\s*\)$", nm, re.IGNORECASE)
                if ma:
                    nma = ma.group(1)
                    dim = ma.group(2)
                    vars_map[nma.lower()] = Var(
                        "int",
                        is_array=True,
                        dim=dim,
                        is_param=is_param,
                        init=init,
                        intent=intent,
                        optional=is_optional,
                        comment=inline_comment,
                    )
                else:
                    vars_map[nm.lower()] = Var("int", is_param=is_param, init=init, intent=intent, optional=is_optional, comment=inline_comment)
            continue
        if m_int_attr:
            attrs = (m_int_attr.group(1) or "").lower()
            ents = m_int_attr.group(2)
            m_intent = re.search(r"intent\s*\(\s*(in|out|inout)\s*\)", attrs, re.IGNORECASE)
            intent = m_intent.group(1).lower() if m_intent else None
            is_param = "parameter" in attrs
            is_save = "save" in attrs
            is_alloc = "allocatable" in attrs
            m_dim_attr = re.search(r"dimension\s*\(\s*([^)]+)\s*\)", attrs, re.IGNORECASE)
            dim_attr = m_dim_attr.group(1).strip() if m_dim_attr else None
            for ent in [x.strip() for x in _split_args_top_level(ents) if x.strip()]:
                if "=" in ent:
                    nm, init = [x.strip() for x in ent.split("=", 1)]
                else:
                    nm, init = ent, None
                ma = re.match(r"^([a-z_]\w*)\s*\(\s*(.+)\s*\)$", nm, re.IGNORECASE)
                if ma:
                    nma = ma.group(1)
                    dim = ma.group(2)
                    vars_map[nma.lower()] = Var(
                        "int",
                        is_array=True,
                        dim=dim,
                        is_allocatable=is_alloc,
                        is_param=is_param,
                        is_save=is_save,
                        init=init,
                        intent=intent,
                        optional=is_optional,
                        comment=inline_comment,
                    )
                elif dim_attr:
                    vars_map[nm.lower()] = Var(
                        "int",
                        is_array=True,
                        dim=dim_attr,
                        is_allocatable=is_alloc,
                        is_param=is_param,
                        is_save=is_save,
                        init=init,
                        intent=intent,
                        optional=is_optional,
                        comment=inline_comment,
                    )
                else:
                    vars_map[nm.lower()] = Var(
                        "int",
                        is_param=is_param,
                        is_save=is_save,
                        init=init,
                        intent=intent,
                        optional=is_optional,
                        comment=inline_comment,
                    )
            continue
        if m_int_no_colon and "::" not in code_no_opt:
            intent = m_int_no_colon.group(1).lower() if m_int_no_colon.group(1) else None
            is_param = bool(m_int_no_colon.group(2))
            for ent in [x.strip() for x in _split_args_top_level(m_int_no_colon.group(3)) if x.strip()]:
                if "=" in ent:
                    nm, init = [x.strip() for x in ent.split("=", 1)]
                else:
                    nm, init = ent, None
                ma = re.match(r"^([a-z_]\w*)\s*\(\s*(.+)\s*\)$", nm, re.IGNORECASE)
                if ma:
                    nma = ma.group(1)
                    dim = ma.group(2)
                    vars_map[nma.lower()] = Var(
                        "int",
                        is_array=True,
                        dim=dim,
                        is_param=is_param,
                        init=init,
                        intent=intent,
                        optional=is_optional,
                        comment=inline_comment,
                    )
                else:
                    vars_map[nm.lower()] = Var("int", is_param=is_param, init=init, intent=intent, optional=is_optional, comment=inline_comment)
            continue
        m_real = re.match(
            r"^real\s*\(\s*kind\s*=\s*([a-z_]\w*|[0-9]+)\s*\)(?:\s*,\s*intent\s*\(\s*(in|out|inout)\s*\))?\s*::\s*(.+)$",
            code_no_opt,
            re.IGNORECASE,
        )
        m_real_attr = re.match(
            r"^real\s*\(\s*kind\s*=\s*([a-z_]\w*|[0-9]+)\s*\)\s*(?:,\s*([^:]+))?\s*::\s*(.+)$",
            code_no_opt,
            re.IGNORECASE,
        )
        if not m_real:
            m_real = re.match(
                r"^real(?:\s*,\s*intent\s*\(\s*(in|out|inout)\s*\))?\s*,\s*kind\s*=\s*([a-z_]\w*|[0-9]+)\s*::\s*(.+)$",
                code_no_opt,
                re.IGNORECASE,
            )
        m_real_kind_no_colon = re.match(
            r"^real\s*\(\s*kind\s*=\s*([a-z_]\w*|[0-9]+)\s*\)\s+(.+)$",
            code_no_opt,
            re.IGNORECASE,
        )
        m_real_bare = re.match(
            r"^real(?!\s*\()(?:\s*,\s*intent\s*\(\s*(in|out|inout)\s*\))?\s*::\s*(.+)$",
            code_no_opt,
            re.IGNORECASE,
        )
        m_real_bare_attr = re.match(
            r"^real(?!\s*\()\s*(?:,\s*([^:]+))?\s*::\s*(.+)$",
            code_no_opt,
            re.IGNORECASE,
        )
        m_real_bare_no_colon = re.match(
            r"^real(?!\s*\()(?:\s*,\s*intent\s*\(\s*(in|out|inout)\s*\))?\s+(.+)$",
            code_no_opt,
            re.IGNORECASE,
        )
        m_dprec = re.match(
            r"^double\s+precision(?:\s*,\s*intent\s*\(\s*(in|out|inout)\s*\))?\s*::\s*(.+)$",
            code_no_opt,
            re.IGNORECASE,
        )
        m_dprec_no_colon = re.match(
            r"^double\s+precision(?:\s*,\s*intent\s*\(\s*(in|out|inout)\s*\))?\s+(.+)$",
            code_no_opt,
            re.IGNORECASE,
        )
        m_char = re.match(
            r"^character\s*\(\s*len\s*=\s*[^)]*\)\s*(?:,\s*intent\s*\(\s*(in|out|inout)\s*\))?\s*::\s*(.+)$",
            code_no_opt,
            re.IGNORECASE,
        )
        m_logical = re.match(
            r"^logical(?:\s*,\s*intent\s*\(\s*(in|out|inout)\s*\))?\s*::\s*(.+)$",
            code_no_opt,
            re.IGNORECASE,
        )
        m_logical_attr = re.match(
            r"^logical\s*(?:,\s*([^:]+))?\s*::\s*(.+)$",
            code_no_opt,
            re.IGNORECASE,
        )
        m_logical_no_colon = re.match(
            r"^logical(?:\s*,\s*intent\s*\(\s*(in|out|inout)\s*\))?\s+(.+)$",
            code_no_opt,
            re.IGNORECASE,
        )
        m_char_no_colon = re.match(
            r"^character\s*\(\s*len\s*=\s*[^)]*\)\s*(?:,\s*intent\s*\(\s*(in|out|inout)\s*\))?\s+(.+)$",
            code_no_opt,
            re.IGNORECASE,
        )
        if m_real or m_real_attr:
            # Normalize group layout across the two accepted real-decl forms.
            m_use = m_real if m_real is not None else m_real_attr
            assert m_use is not None
            if m_use.re.pattern.startswith("^real\\s*\\("):
                kind_tok = (m_use.group(1) or "").strip().lower()
                attrs = (m_use.group(2) or "").lower()
                m_intent = re.search(r"intent\s*\(\s*(in|out|inout)\s*\)", attrs, re.IGNORECASE)
                intent = m_intent.group(1).lower() if m_intent else None
                ents = m_use.group(3)
                is_external = "external" in attrs
                is_alloc = "allocatable" in attrs
                m_dim_attr = re.search(r"dimension\s*\(\s*([^)]+)\s*\)", attrs, re.IGNORECASE)
                dim_attr = m_dim_attr.group(1).strip() if m_dim_attr else None
            else:
                kind_tok = (m_use.group(2) or "").strip().lower()
                intent = m_use.group(1).lower() if m_use.group(1) else None
                ents = m_use.group(3)
                is_external = False
                is_alloc = False
                dim_attr = None
            kind_ct = real_type
            if kind_tok:
                if kind_ctype_map and kind_tok in kind_ctype_map:
                    kind_ct = kind_ctype_map[kind_tok]
                elif kind_tok.isdigit():
                    # Heuristic for numeric kind IDs.
                    kind_ct = "double" if int(kind_tok) >= 8 else "float"
            for ent in [x.strip() for x in _split_args_top_level(ents) if x.strip()]:
                ma = re.match(r"^([a-z_]\w*)\s*\(\s*(.+)\s*\)$", lhs, re.IGNORECASE)
                if ma:
                    nm = ma.group(1)
                    dim = ma.group(2).strip()
                    vars_map[nm.lower()] = Var(
                        kind_ct,
                        is_array=True,
                        dim=dim,
                        intent=intent,
                        is_external=is_external,
                        is_allocatable=is_alloc,
                        optional=is_optional,
                        comment=inline_comment,
                    )
                elif dim_attr:
                    vars_map[ent.lower()] = Var(
                        kind_ct,
                        is_array=True,
                        dim=dim_attr,
                        intent=intent,
                        is_external=is_external,
                        is_allocatable=is_alloc,
                        optional=is_optional,
                        comment=inline_comment,
                    )
                else:
                    vars_map[ent.lower()] = Var(
                        kind_ct,
                        intent=intent,
                        is_external=is_external,
                        is_allocatable=is_alloc,
                        optional=is_optional,
                        comment=inline_comment,
                    )
            continue
        if m_real_kind_no_colon and "::" not in code_no_opt:
            kind_tok = (m_real_kind_no_colon.group(1) or "").strip().lower()
            ents = m_real_kind_no_colon.group(2)
            kind_ct = real_type
            if kind_tok:
                if kind_ctype_map and kind_tok in kind_ctype_map:
                    kind_ct = kind_ctype_map[kind_tok]
                elif kind_tok.isdigit():
                    kind_ct = "double" if int(kind_tok) >= 8 else "float"
            for ent in [x.strip() for x in _split_args_top_level(ents) if x.strip()]:
                if "=" in ent and "=>" not in ent:
                    lhs, init = [x.strip() for x in ent.split("=", 1)]
                else:
                    lhs, init = ent, None
                ma = re.match(r"^([a-z_]\w*)\s*\(\s*(.+)\s*\)$", lhs, re.IGNORECASE)
                if ma:
                    nm = ma.group(1)
                    dim = ma.group(2).strip()
                    vars_map[nm.lower()] = Var(kind_ct, is_array=True, dim=dim, init=init, optional=is_optional, comment=inline_comment)
                else:
                    vars_map[lhs.lower()] = Var(kind_ct, init=init, optional=is_optional, comment=inline_comment)
            continue
        m_external = re.match(r"^external\s+(.+)$", code_no_opt, re.IGNORECASE)
        if m_external:
            for ent in [x.strip() for x in _split_args_top_level(m_external.group(1)) if x.strip()]:
                nm = ent.lower()
                v0 = vars_map.get(nm)
                if v0 is None:
                    vars_map[nm] = Var(real_type, is_external=True, optional=is_optional, comment=inline_comment)
                else:
                    v0.is_external = True
            continue
        if m_logical or m_logical_attr:
            m_use = m_logical if m_logical is not None else m_logical_attr
            assert m_use is not None
            if m_use.re.pattern.startswith("^logical(?:\\s*,\\s*intent"):
                intent = m_use.group(1).lower() if m_use.group(1) else None
                ents = m_use.group(2)
                is_external = False
                is_alloc = False
                dim_attr = None
            else:
                attrs = (m_use.group(1) or "").lower()
                ents = m_use.group(2)
                m_intent = re.search(r"intent\s*\(\s*(in|out|inout)\s*\)", attrs, re.IGNORECASE)
                intent = m_intent.group(1).lower() if m_intent else None
                is_external = "external" in attrs
                is_alloc = "allocatable" in attrs
                m_dim_attr = re.search(r"dimension\s*\(\s*([^)]+)\s*\)", attrs, re.IGNORECASE)
                dim_attr = m_dim_attr.group(1).strip() if m_dim_attr else None
            for ent in [x.strip() for x in _split_args_top_level(ents) if x.strip()]:
                if "=" in ent and "=>" not in ent:
                    lhs, init = [x.strip() for x in ent.split("=", 1)]
                else:
                    lhs, init = ent, None
                ma = re.match(r"^([a-z_]\w*)\s*\(\s*(.+)\s*\)$", lhs, re.IGNORECASE)
                if ma:
                    nm = ma.group(1)
                    dim = ma.group(2).strip()
                    vars_map[nm.lower()] = Var("int", is_array=True, dim=dim, init=init, intent=intent, is_external=is_external, is_allocatable=is_alloc, optional=is_optional, comment=inline_comment)
                elif dim_attr:
                    vars_map[lhs.lower()] = Var("int", is_array=True, dim=dim_attr, init=init, intent=intent, is_external=is_external, is_allocatable=is_alloc, optional=is_optional, comment=inline_comment)
                else:
                    vars_map[lhs.lower()] = Var("int", init=init, intent=intent, is_external=is_external, is_allocatable=is_alloc, optional=is_optional, comment=inline_comment)
            continue
        if m_logical_no_colon and "::" not in code_no_opt:
            intent = m_logical_no_colon.group(1).lower() if m_logical_no_colon.group(1) else None
            ents = m_logical_no_colon.group(2)
            for ent in [x.strip() for x in _split_args_top_level(ents) if x.strip()]:
                if "=" in ent and "=>" not in ent:
                    lhs, init = [x.strip() for x in ent.split("=", 1)]
                else:
                    lhs, init = ent, None
                ma = re.match(r"^([a-z_]\w*)\s*\(\s*(.+)\s*\)$", lhs, re.IGNORECASE)
                if ma:
                    nm = ma.group(1)
                    dim = ma.group(2).strip()
                    vars_map[nm.lower()] = Var("int", is_array=True, dim=dim, init=init, intent=intent, optional=is_optional, comment=inline_comment)
                else:
                    vars_map[lhs.lower()] = Var("int", init=init, intent=intent, optional=is_optional, comment=inline_comment)
            continue
        if m_real_bare:
            intent = m_real_bare.group(1).lower() if m_real_bare.group(1) else None
            ents = m_real_bare.group(2)
            for ent in [x.strip() for x in _split_args_top_level(ents) if x.strip()]:
                if "=" in ent and "=>" not in ent:
                    lhs, init = [x.strip() for x in ent.split("=", 1)]
                else:
                    lhs, init = ent, None
                ma = re.match(r"^([a-z_]\w*)\s*\(\s*(.+)\s*\)$", lhs, re.IGNORECASE)
                if ma:
                    nm = ma.group(1)
                    dim = ma.group(2).strip()
                    vars_map[nm.lower()] = Var(real_type, is_array=True, dim=dim, init=init, intent=intent, optional=is_optional, comment=inline_comment)
                else:
                    vars_map[lhs.lower()] = Var(real_type, init=init, intent=intent, optional=is_optional, comment=inline_comment)
            continue
        if m_real_bare_attr:
            attrs = (m_real_bare_attr.group(1) or "").lower()
            ents = m_real_bare_attr.group(2)
            m_intent = re.search(r"intent\s*\(\s*(in|out|inout)\s*\)", attrs, re.IGNORECASE)
            intent = m_intent.group(1).lower() if m_intent else None
            is_external = "external" in attrs
            is_alloc = "allocatable" in attrs
            m_dim_attr = re.search(r"dimension\s*\(\s*([^)]+)\s*\)", attrs, re.IGNORECASE)
            dim_attr = m_dim_attr.group(1).strip() if m_dim_attr else None
            for ent in [x.strip() for x in _split_args_top_level(ents) if x.strip()]:
                if "=" in ent and "=>" not in ent:
                    lhs, init = [x.strip() for x in ent.split("=", 1)]
                else:
                    lhs, init = ent, None
                ma = re.match(r"^([a-z_]\w*)\s*\(\s*(.+)\s*\)$", ent, re.IGNORECASE)
                if ma:
                    nm = ma.group(1)
                    dim = ma.group(2).strip()
                    vars_map[nm.lower()] = Var(
                        real_type,
                        is_array=True,
                        dim=dim,
                        init=init,
                        is_allocatable=is_alloc,
                        intent=intent,
                        is_external=is_external,
                        optional=is_optional,
                        comment=inline_comment,
                    )
                else:
                    nm = lhs
                    if dim_attr:
                        vars_map[nm.lower()] = Var(
                            real_type,
                            is_array=True,
                            dim=dim_attr,
                            init=init,
                            is_allocatable=is_alloc,
                            intent=intent,
                            is_external=is_external,
                            optional=is_optional,
                            comment=inline_comment,
                        )
                    else:
                        vars_map[nm.lower()] = Var(
                            real_type,
                            init=init,
                            is_allocatable=is_alloc,
                            intent=intent,
                            is_external=is_external,
                            optional=is_optional,
                            comment=inline_comment,
                        )
            continue
        if m_real_bare_no_colon and "::" not in code_no_opt:
            intent = m_real_bare_no_colon.group(1).lower() if m_real_bare_no_colon.group(1) else None
            ents = m_real_bare_no_colon.group(2)
            for ent in [x.strip() for x in _split_args_top_level(ents) if x.strip()]:
                if "=" in ent and "=>" not in ent:
                    lhs, init = [x.strip() for x in ent.split("=", 1)]
                else:
                    lhs, init = ent, None
                ma = re.match(r"^([a-z_]\w*)\s*\(\s*(.+)\s*\)$", lhs, re.IGNORECASE)
                if ma:
                    nm = ma.group(1)
                    dim = ma.group(2).strip()
                    vars_map[nm.lower()] = Var(real_type, is_array=True, dim=dim, init=init, intent=intent, optional=is_optional, comment=inline_comment)
                else:
                    vars_map[lhs.lower()] = Var(real_type, init=init, intent=intent, optional=is_optional, comment=inline_comment)
            continue
        if m_dprec:
            intent = m_dprec.group(1).lower() if m_dprec.group(1) else None
            ents = m_dprec.group(2)
            for ent in [x.strip() for x in _split_args_top_level(ents) if x.strip()]:
                ma = re.match(r"^([a-z_]\w*)\s*\(\s*(.+)\s*\)$", ent, re.IGNORECASE)
                if ma:
                    nm = ma.group(1)
                    dim = ma.group(2).strip()
                    vars_map[nm.lower()] = Var("double", is_array=True, dim=dim, intent=intent, optional=is_optional, comment=inline_comment)
                else:
                    vars_map[ent.lower()] = Var("double", intent=intent, optional=is_optional, comment=inline_comment)
            continue
        if m_dprec_no_colon and "::" not in code_no_opt:
            intent = m_dprec_no_colon.group(1).lower() if m_dprec_no_colon.group(1) else None
            ents = m_dprec_no_colon.group(2)
            for ent in [x.strip() for x in _split_args_top_level(ents) if x.strip()]:
                ma = re.match(r"^([a-z_]\w*)\s*\(\s*(.+)\s*\)$", ent, re.IGNORECASE)
                if ma:
                    nm = ma.group(1)
                    dim = ma.group(2).strip()
                    vars_map[nm.lower()] = Var("double", is_array=True, dim=dim, intent=intent, optional=is_optional, comment=inline_comment)
                else:
                    vars_map[ent.lower()] = Var("double", intent=intent, optional=is_optional, comment=inline_comment)
            continue
        if m_char:
            intent = m_char.group(1).lower() if m_char.group(1) else None
            ents = m_char.group(2)
            for ent in [x.strip() for x in _split_args_top_level(ents) if x.strip()]:
                vars_map[ent.lower()] = Var("char *", intent=intent, optional=is_optional, comment=inline_comment)
            continue
        if m_char_no_colon and "::" not in code_no_opt:
            intent = m_char_no_colon.group(1).lower() if m_char_no_colon.group(1) else None
            ents = m_char_no_colon.group(2)
            for ent in [x.strip() for x in _split_args_top_level(ents) if x.strip()]:
                vars_map[ent.lower()] = Var("char *", intent=intent, optional=is_optional, comment=inline_comment)
            continue
    return vars_map


def _emit_decl(nm: str, v: Var, vars_map: Dict[str, Var], real_type: str, for_main: bool, as_arg: bool = False) -> str:
    if as_arg:
        if v.is_external:
            return f"{v.ctype} (*{nm})(...)"
        if v.is_array:
            if v.intent == "in":
                return f"const {v.ctype} *{nm}"
            return f"{v.ctype} *{nm}"
        if v.optional:
            if v.intent == "in":
                return f"const {v.ctype} *{nm}"
            return f"{v.ctype} *{nm}"
        if v.intent in {"out", "inout"}:
            return f"{v.ctype} *{nm}"
        return f"const {v.ctype} {nm}"
    # In Fortran procedures, local variables with declaration-time initialization
    # have implicit SAVE semantics.
    implicit_save_init = (v.init is not None) and (not for_main)
    prefix = "static " if (v.is_save or implicit_save_init) else ""
    if v.is_param:
        raw_init = (v.init or "0").replace("_dp", "").replace("_DP", "")
        val = _eval_int_expr(raw_init)
        init = _convert_expr(v.init or "0", vars_map, real_type)
        if val is not None:
            return f"{prefix}const int {nm} = {val};"
        return f"{prefix}const int {nm} = {init};"
    if v.is_external:
        return f"{prefix}{v.ctype} (*{nm})(...);"
    if v.is_array:
        if for_main:
            if v.is_allocatable:
                return f"{prefix}{v.ctype} *{nm} = NULL;"
            return f"{prefix}{v.ctype} *{nm};"
        dim = _dim_product_expr(v.dim or "1", vars_map, real_type)
        return f"{prefix}{v.ctype} {nm}[{dim}];"
    if v.init is not None:
        init = _convert_expr(v.init, vars_map, real_type)
        return f"{prefix}{v.ctype} {nm} = {init};"
    return f"{prefix}{v.ctype} {nm};"


def _fold_zero_init_to_decl(lines: List[str], real_type: str) -> List[str]:
    """Fold `x = 0.0*;` immediately after declaration into `type x = 0.0*;`.

    Conservative:
    - declaration line exactly `<indent><float|double> name;`
    - next non-comment/non-blank statement is `name = 0.0...;`
    """
    out = list(lines)
    decl_re = re.compile(r"^(\s*)(float|double)\s+([a-z_]\w*)\s*;\s*$", re.IGNORECASE)
    zero_re = re.compile(r"^\s*([a-z_]\w*)\s*=\s*0(?:\.0+)?(?:[eE][+\-]?\d+)?(?:f)?\s*;\s*$", re.IGNORECASE)
    i = 0
    while i < len(out):
        m = decl_re.match(out[i])
        if not m:
            i += 1
            continue
        indent, cty, nm = m.group(1), m.group(2), m.group(3)
        j = i + 1
        while j < len(out):
            s = out[j].strip()
            if not s or s.startswith("/*") or s.startswith("//"):
                j += 1
                continue
            break
        if j >= len(out):
            i += 1
            continue
        z = zero_re.match(out[j])
        if not z or z.group(1).lower() != nm.lower():
            i += 1
            continue
        zero_lit = "0.0f" if cty.lower() == "float" else "0.0"
        out[i] = f"{indent}{cty} {nm} = {zero_lit};"
        out[j] = ""
        i = j + 1
    return [ln for ln in out if ln != ""]


def _compound_assign_style(lines: List[str]) -> List[str]:
    """Rewrite `x = x +/- expr;` into `x += expr;` / `x -= expr;`."""
    out: List[str] = []
    pat = re.compile(
        r"^(\s*)([a-z_]\w*(?:\[[^\]]+\])?)\s*=\s*\2\s*([+\-])\s*(.+?)\s*;\s*$",
        re.IGNORECASE,
    )
    for ln in lines:
        m = pat.match(ln)
        if not m:
            out.append(ln)
            continue
        indent, lhs, op, rhs = m.group(1), m.group(2), m.group(3), m.group(4)
        out.append(f"{indent}{lhs} {op}= {rhs};")
    return out


def _coalesce_adjacent_c_declarations(lines: List[str]) -> List[str]:
    """Coalesce adjacent simple declarations with same type and indent.

    Example:
      double a;
      double b;
    -> double a, b;
    """
    out: List[str] = []
    i = 0
    decl_re = re.compile(
        r"^(\s*)((?:const\s+)?(?:int|float|double))\s+([a-z_]\w*)\s*;\s*$",
        re.IGNORECASE,
    )
    ptr_decl_re = re.compile(
        r"^(\s*)((?:const\s+)?(?:int|float|double))\s*\*\s*([a-z_]\w*)\s*;\s*$",
        re.IGNORECASE,
    )
    init_decl_re = re.compile(
        r"^(\s*)((?:const\s+)?(?:int|float|double))\s+([a-z_]\w*)\s*=\s*(.+?)\s*;\s*$",
        re.IGNORECASE,
    )
    ptr_init_decl_re = re.compile(
        r"^(\s*)((?:const\s+)?(?:int|float|double))\s*\*\s*([a-z_]\w*)\s*=\s*(.+?)\s*;\s*$",
        re.IGNORECASE,
    )
    while i < len(lines):
        m = decl_re.match(lines[i])
        if not m:
            m_ptr = ptr_decl_re.match(lines[i])
            if m_ptr:
                indent = m_ptr.group(1)
                cty = m_ptr.group(2).strip()
                names = [m_ptr.group(3)]
                j = i + 1
                while j < len(lines):
                    mj = ptr_decl_re.match(lines[j])
                    if not mj:
                        break
                    if mj.group(1) != indent or mj.group(2).strip().lower() != cty.lower():
                        break
                    names.append(mj.group(3))
                    j += 1
                if len(names) == 1:
                    out.append(lines[i])
                else:
                    out.append(f"{indent}{cty} *{', *'.join(names)};")
                i = j
                continue

            m_init = init_decl_re.match(lines[i])
            if not m_init:
                m_ptr_init = ptr_init_decl_re.match(lines[i])
                if m_ptr_init:
                    indent = m_ptr_init.group(1)
                    cty = m_ptr_init.group(2).strip()
                    parts = [f"*{m_ptr_init.group(3)} = {m_ptr_init.group(4)}"]
                    j = i + 1
                    while j < len(lines):
                        mj = ptr_init_decl_re.match(lines[j])
                        if not mj:
                            break
                        if mj.group(1) != indent or mj.group(2).strip().lower() != cty.lower():
                            break
                        parts.append(f"*{mj.group(3)} = {mj.group(4)}")
                        j += 1
                    if len(parts) == 1:
                        out.append(lines[i])
                    else:
                        out.append(f"{indent}{cty} {', '.join(parts)};")
                    i = j
                    continue
                out.append(lines[i])
                i += 1
                continue
            indent = m_init.group(1)
            cty = m_init.group(2).strip()
            parts = [f"{m_init.group(3)} = {m_init.group(4)}"]
            j = i + 1
            while j < len(lines):
                mj = init_decl_re.match(lines[j])
                if not mj:
                    break
                if mj.group(1) != indent or mj.group(2).strip().lower() != cty.lower():
                    break
                parts.append(f"{mj.group(3)} = {mj.group(4)}")
                j += 1
            if len(parts) == 1:
                out.append(lines[i])
            else:
                out.append(f"{indent}{cty} {', '.join(parts)};")
            i = j
            continue
        indent = m.group(1)
        cty = m.group(2).strip()
        names = [m.group(3)]
        j = i + 1
        while j < len(lines):
            mj = decl_re.match(lines[j])
            if not mj:
                break
            if mj.group(1) != indent or mj.group(2).strip().lower() != cty.lower():
                break
            names.append(mj.group(3))
            j += 1
        if len(names) == 1:
            out.append(lines[i])
        else:
            out.append(f"{indent}{cty} {', '.join(names)};")
        i = j
    return out


def _collapse_one_line_blocks(lines: List[str]) -> List[str]:
    """Collapse simple braced `for`/`if` blocks with one statement to one-line form."""
    out = list(lines)
    i = 0
    while i < len(out):
        hdr = out[i]
        m = re.match(r"^(\s*)(for\s*\(.+\)|if\s*\(.+\))\s*\{\s*$", hdr)
        if not m:
            i += 1
            continue
        indent = m.group(1)
        header = m.group(2)
        j = i + 1
        while j < len(out) and not out[j].strip():
            j += 1
        if j >= len(out):
            i += 1
            continue
        body = out[j]
        body_strip = body.strip()
        if body_strip.startswith("/*") or body_strip.startswith("//"):
            i += 1
            continue
        if body_strip.endswith("{") or body_strip == "}":
            i += 1
            continue
        if not body_strip.endswith(";"):
            i += 1
            continue
        k = j + 1
        while k < len(out) and not out[k].strip():
            k += 1
        if k >= len(out) or out[k].strip() != "}":
            i += 1
            continue
        # Preserve body text as-is (trim leading/trailing spaces only).
        out[i] = f"{indent}{header} {body_strip}"
        out[j] = ""
        out[k] = ""
        i = k + 1
    return [ln for ln in out if ln != ""]


def _rewrite_zero_based_loop_style(lines: List[str]) -> List[str]:
    """Rewrite safe C loop/index patterns to cleaner 0-based style.

    Patterns (safe, conservative):
    - for (i = 1; i <= n; ++i) {
      ...
      x[i - 1]
      ...
      }
    becomes
    - for (i = 0; i < n; ++i) {
      ...
      x[i]
      ...
      }

    - for (i = 2; i <= n; ++i) {
      ...
      x[i - 1]
      ...
      }
    becomes
    - for (i = 1; i < n; ++i) {
      ...
      x[i]
      ...
      }

    Only applied when the loop body uses loop variable `i` exclusively as `i - 1`.
    """
    out = list(lines)
    i = 0
    while i < len(out):
        m1 = re.match(
            r"^(\s*)for\s*\(\s*((?:int\s+)?)\s*([a-z_]\w*)\s*=\s*1\s*;\s*\3\s*<=\s*([a-z_]\w*|\d+)\s*;\s*\+\+\3\s*\)\s*\{\s*$",
            out[i],
            re.IGNORECASE,
        )
        m2 = re.match(
            r"^(\s*)for\s*\(\s*((?:int\s+)?)\s*([a-z_]\w*)\s*=\s*2\s*;\s*\3\s*<=\s*([a-z_]\w*|\d+)\s*;\s*\+\+\3\s*\)\s*\{\s*$",
            out[i],
            re.IGNORECASE,
        )
        if not m1 and not m2:
            i += 1
            continue
        m = m1 if m1 else m2
        assert m is not None
        indent = m.group(1)
        int_kw = m.group(2) or ""
        ivar = m.group(3)
        hi = m.group(4)
        start_val = 1 if m1 else 2
        end = i + 1
        while end < len(out):
            if re.match(rf"^{re.escape(indent)}}}\s*$", out[end]):
                break
            end += 1
        if end >= len(out):
            i += 1
            continue

        body = out[i + 1 : end]
        body_txt = "\n".join(body)
        if not re.search(rf"\[\s*{re.escape(ivar)}\s*-\s*1\s*\]", body_txt):
            i = end + 1
            continue
        # If loop var appears in body in any way other than `i - 1`, skip rewrite.
        raw_uses = re.findall(rf"\b{re.escape(ivar)}\b", body_txt)
        minus_ones = re.findall(rf"\b{re.escape(ivar)}\s*-\s*1\b", body_txt)
        if len(raw_uses) != len(minus_ones):
            i = end + 1
            continue

        if start_val == 1:
            out[i] = f"{indent}for ({int_kw}{ivar} = 0; {ivar} < {hi}; ++{ivar}) {{"
        else:
            out[i] = f"{indent}for ({int_kw}{ivar} = 1; {ivar} < {hi}; ++{ivar}) {{"
        for k in range(i + 1, end):
            out[k] = re.sub(
                rf"\[\s*{re.escape(ivar)}\s*-\s*1\s*\]",
                f"[{ivar}]",
                out[k],
            )
        i = end + 1
    return out


def _use_block_scoped_loop_indices(lines: List[str]) -> List[str]:
    """Prefer `for (int i = ...)` when loop index is loop-local.

    Conservative rules per index variable:
    - declaration line exists exactly as `int <name>;`
    - all uses of `<name>` are inside loops headed by `for (<name> = ... )`
    """
    out = list(lines)
    decl_re = re.compile(r"^(\s*)int\s+([a-z_]\w*)\s*;\s*$", re.IGNORECASE)

    decls: Dict[str, int] = {}
    for idx, ln in enumerate(out):
        m = decl_re.match(ln)
        if m:
            decls[m.group(2)] = idx

    if not decls:
        return out

    for var, decl_idx in list(decls.items()):
        header_re = re.compile(rf"^\s*for\s*\(\s*{re.escape(var)}\s*=", re.IGNORECASE)
        token_re = re.compile(rf"\b{re.escape(var)}\b", re.IGNORECASE)
        loop_ranges: List[tuple[int, int]] = []
        i = 0
        while i < len(out):
            code = out[i].split("//", 1)[0]
            if not header_re.match(code):
                i += 1
                continue
            # Find loop end by brace depth.
            depth = code.count("{") - code.count("}")
            j = i + 1
            while j < len(out):
                c = out[j].split("//", 1)[0]
                depth += c.count("{") - c.count("}")
                if depth <= 0:
                    break
                j += 1
            if j >= len(out):
                break
            loop_ranges.append((i, j))
            i = j + 1
        if not loop_ranges:
            continue

        uses: List[int] = []
        outside_use = False
        for i, ln in enumerate(out):
            if i == decl_idx:
                continue
            # Strip line comments.
            code = ln.split("//", 1)[0]
            if not code.strip():
                continue
            if token_re.search(code) is None:
                continue
            uses.append(i)
            in_any = any(a <= i <= b for a, b in loop_ranges)
            if not in_any:
                outside_use = True
                break
        if outside_use or not uses:
            continue
        # Rewrite loop headers.
        for a, _b in loop_ranges:
            out[a] = re.sub(
                rf"^(\s*)for\s*\(\s*{re.escape(var)}\s*=",
                rf"\1for (int {var} =",
                out[a],
            )
        # Drop standalone declaration.
        out[decl_idx] = ""

    return [ln for ln in out if ln != ""]


def _inline_simple_int_aliases(lines: List[str]) -> List[str]:
    """Inline simple aliases like `int n; n = n_x;` within one emitted unit."""
    out = list(lines)
    decl_re = re.compile(r"^\s*int\s+([a-z_]\w*)\s*;\s*$", re.IGNORECASE)
    asn_re = re.compile(r"^\s*([a-z_]\w*)\s*=\s*([a-z_]\w*)\s*;\s*$", re.IGNORECASE)

    decl_idx: Dict[str, int] = {}
    for i, ln in enumerate(out):
        m = decl_re.match(ln)
        if m:
            decl_idx[m.group(1)] = i

    remove_idx: Set[int] = set()
    for i, ln in enumerate(out):
        m = asn_re.match(ln)
        if not m:
            continue
        lhs = m.group(1)
        rhs = m.group(2)
        if lhs == rhs:
            continue
        di = decl_idx.get(lhs)
        if di is None or di > i:
            continue
        # Reject if LHS is assigned elsewhere.
        reassigned = False
        for j, lj in enumerate(out):
            if j == i:
                continue
            m2 = re.match(rf"^\s*{re.escape(lhs)}\s*=", lj)
            if m2:
                reassigned = True
                break
        if reassigned:
            continue
        # Replace remaining uses.
        pat = re.compile(rf"\b{re.escape(lhs)}\b")
        for j in range(i + 1, len(out)):
            out[j] = pat.sub(rhs, out[j])
        remove_idx.add(di)
        remove_idx.add(i)

    if not remove_idx:
        return out
    return [ln for idx, ln in enumerate(out) if idx not in remove_idx]


def _prefer_simple_n_extent_name(lines: List[str], assumed_extents: Dict[str, List[str]]) -> List[str]:
    """Rename one generated rank-1 extent name to `n` when unambiguous."""
    exts = [e for vals in assumed_extents.values() for e in vals]
    if len(exts) != 1:
        return lines
    old = exts[0]
    if old == "n":
        return lines
    # Only rename when no standalone `n` symbol exists already.
    n_tok = re.compile(r"\bn\b")
    old_tok = re.compile(rf"\b{re.escape(old)}\b")
    if any(n_tok.search(ln) for ln in lines):
        return lines
    return [old_tok.sub("n", ln) for ln in lines]


def _drop_redundant_outer_parens(expr: str) -> str:
    s = expr.strip()
    if len(s) >= 2 and s[0] == "(" and s[-1] == ")":
        depth = 0
        ok = True
        for i, ch in enumerate(s):
            if ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
                if depth == 0 and i != len(s) - 1:
                    ok = False
                    break
        if ok:
            return s[1:-1].strip()
    return s


def _inject_runtime_helpers(lines: List[str]) -> List[str]:
    text = "\n".join(lines)
    need_sum_1d_float = "sum_1d_float(" in text
    need_sum_2d_float = "sum_2d_float(" in text
    need_sum_1d_double = "sum_1d_double(" in text
    need_sum_2d_double = "sum_2d_double(" in text
    need_sum_1d_int = "sum_1d_int(" in text
    need_sum_2d_int = "sum_2d_int(" in text
    need_product_1d_float = "product_1d_float(" in text
    need_product_2d_float = "product_2d_float(" in text
    need_product_1d_double = "product_1d_double(" in text
    need_product_2d_double = "product_2d_double(" in text
    need_fill_rand_1d_float = "fill_rand_1d_float(" in text
    need_fill_rand_2d_float = "fill_rand_2d_float(" in text
    need_fill_rand_3d_float = "fill_rand_3d_float(" in text
    need_fill_rand_1d_double = "fill_rand_1d_double(" in text
    need_fill_rand_2d_double = "fill_rand_2d_double(" in text
    need_fill_rand_3d_double = "fill_rand_3d_double(" in text
    need_minval_1d_float = "minval_1d_float(" in text
    need_minval_1d_double = "minval_1d_double(" in text
    need_minval_1d_int = "minval_1d_int(" in text
    need_maxval_1d_float = "maxval_1d_float(" in text
    need_maxval_1d_double = "maxval_1d_double(" in text
    need_maxval_1d_int = "maxval_1d_int(" in text
    if not (
        need_sum_1d_float
        or need_sum_2d_float
        or need_sum_1d_double
        or need_sum_2d_double
        or need_sum_1d_int
        or need_sum_2d_int
        or need_product_1d_float
        or need_product_2d_float
        or need_product_1d_double
        or need_product_2d_double
        or need_fill_rand_1d_float
        or need_fill_rand_2d_float
        or need_fill_rand_3d_float
        or need_fill_rand_1d_double
        or need_fill_rand_2d_double
        or need_fill_rand_3d_double
        or need_minval_1d_float
        or need_minval_1d_double
        or need_minval_1d_int
        or need_maxval_1d_float
        or need_maxval_1d_double
        or need_maxval_1d_int
    ):
        return lines

    helpers: List[str] = []
    if need_sum_1d_float:
        helpers.extend(
            [
                "static float sum_1d_float(const int n, const float *x) {",
                "   float s = 0.0f;",
                "   for (int i = 0; i < n; ++i) s += x[i];",
                "   return s;",
                "}",
                "",
            ]
        )
    if need_sum_2d_float:
        helpers.extend(
            [
                "static float sum_2d_float(const int n1, const int n2, const float *x) {",
                "   float s = 0.0f;",
                "   for (int i = 0; i < n1 * n2; ++i) s += x[i];",
                "   return s;",
                "}",
                "",
            ]
        )
    if need_sum_1d_double:
        helpers.extend(
            [
                "static double sum_1d_double(const int n, const double *x) {",
                "   double s = 0.0;",
                "   for (int i = 0; i < n; ++i) s += x[i];",
                "   return s;",
                "}",
                "",
            ]
        )
    if need_sum_2d_double:
        helpers.extend(
            [
                "static double sum_2d_double(const int n1, const int n2, const double *x) {",
                "   double s = 0.0;",
                "   for (int i = 0; i < n1 * n2; ++i) s += x[i];",
                "   return s;",
                "}",
                "",
            ]
        )
    if need_sum_1d_int:
        helpers.extend(
            [
                "static int sum_1d_int(const int n, const int *x) {",
                "   int s = 0;",
                "   for (int i = 0; i < n; ++i) s += x[i];",
                "   return s;",
                "}",
                "",
            ]
        )
    if need_sum_2d_int:
        helpers.extend(
            [
                "static int sum_2d_int(const int n1, const int n2, const int *x) {",
                "   int s = 0;",
                "   for (int i = 0; i < n1 * n2; ++i) s += x[i];",
                "   return s;",
                "}",
                "",
            ]
        )
    if need_product_1d_float:
        helpers.extend(
            [
                "static float product_1d_float(const int n, const float *x) {",
                "   float p = 1.0f;",
                "   for (int i = 0; i < n; ++i) p *= x[i];",
                "   return p;",
                "}",
                "",
            ]
        )
    if need_product_2d_float:
        helpers.extend(
            [
                "static float product_2d_float(const int n1, const int n2, const float *x) {",
                "   float p = 1.0f;",
                "   for (int i = 0; i < n1 * n2; ++i) p *= x[i];",
                "   return p;",
                "}",
                "",
            ]
        )
    if need_product_1d_double:
        helpers.extend(
            [
                "static double product_1d_double(const int n, const double *x) {",
                "   double p = 1.0;",
                "   for (int i = 0; i < n; ++i) p *= x[i];",
                "   return p;",
                "}",
                "",
            ]
        )
    if need_product_2d_double:
        helpers.extend(
            [
                "static double product_2d_double(const int n1, const int n2, const double *x) {",
                "   double p = 1.0;",
                "   for (int i = 0; i < n1 * n2; ++i) p *= x[i];",
                "   return p;",
                "}",
                "",
            ]
        )
    if need_fill_rand_1d_float:
        helpers.extend(
            [
                "static void fill_rand_1d_float(const int n, float *x) {",
                "   for (int i = 0; i < n; ++i) x[i] = (float)rand() / (float)RAND_MAX;",
                "}",
                "",
            ]
        )
    if need_fill_rand_2d_float:
        helpers.extend(
            [
                "static void fill_rand_2d_float(const int n1, const int n2, float *x) {",
                "   for (int i = 0; i < n1 * n2; ++i) x[i] = (float)rand() / (float)RAND_MAX;",
                "}",
                "",
            ]
        )
    if need_fill_rand_3d_float:
        helpers.extend(
            [
                "static void fill_rand_3d_float(const int n1, const int n2, const int n3, float *x) {",
                "   for (int i = 0; i < n1 * n2 * n3; ++i) x[i] = (float)rand() / (float)RAND_MAX;",
                "}",
                "",
            ]
        )
    if need_fill_rand_1d_double:
        helpers.extend(
            [
                "static void fill_rand_1d_double(const int n, double *x) {",
                "   for (int i = 0; i < n; ++i) x[i] = (double)rand() / (double)RAND_MAX;",
                "}",
                "",
            ]
        )
    if need_fill_rand_2d_double:
        helpers.extend(
            [
                "static void fill_rand_2d_double(const int n1, const int n2, double *x) {",
                "   for (int i = 0; i < n1 * n2; ++i) x[i] = (double)rand() / (double)RAND_MAX;",
                "}",
                "",
            ]
        )
    if need_fill_rand_3d_double:
        helpers.extend(
            [
                "static void fill_rand_3d_double(const int n1, const int n2, const int n3, double *x) {",
                "   for (int i = 0; i < n1 * n2 * n3; ++i) x[i] = (double)rand() / (double)RAND_MAX;",
                "}",
                "",
            ]
        )
    if need_minval_1d_float:
        helpers.extend(
            [
                "static float minval_1d_float(const int n, const float *x) {",
                "   if (n <= 0) return 0.0f;",
                "   float m = x[0];",
                "   for (int i = 1; i < n; ++i) if (x[i] < m) m = x[i];",
                "   return m;",
                "}",
                "",
            ]
        )
    if need_minval_1d_double:
        helpers.extend(
            [
                "static double minval_1d_double(const int n, const double *x) {",
                "   if (n <= 0) return 0.0;",
                "   double m = x[0];",
                "   for (int i = 1; i < n; ++i) if (x[i] < m) m = x[i];",
                "   return m;",
                "}",
                "",
            ]
        )
    if need_minval_1d_int:
        helpers.extend(
            [
                "static int minval_1d_int(const int n, const int *x) {",
                "   if (n <= 0) return 0;",
                "   int m = x[0];",
                "   for (int i = 1; i < n; ++i) if (x[i] < m) m = x[i];",
                "   return m;",
                "}",
                "",
            ]
        )
    if need_maxval_1d_float:
        helpers.extend(
            [
                "static float maxval_1d_float(const int n, const float *x) {",
                "   if (n <= 0) return 0.0f;",
                "   float m = x[0];",
                "   for (int i = 1; i < n; ++i) if (x[i] > m) m = x[i];",
                "   return m;",
                "}",
                "",
            ]
        )
    if need_maxval_1d_double:
        helpers.extend(
            [
                "static double maxval_1d_double(const int n, const double *x) {",
                "   if (n <= 0) return 0.0;",
                "   double m = x[0];",
                "   for (int i = 1; i < n; ++i) if (x[i] > m) m = x[i];",
                "   return m;",
                "}",
                "",
            ]
        )
    if need_maxval_1d_int:
        helpers.extend(
            [
                "static int maxval_1d_int(const int n, const int *x) {",
                "   if (n <= 0) return 0;",
                "   int m = x[0];",
                "   for (int i = 1; i < n; ++i) if (x[i] > m) m = x[i];",
                "   return m;",
                "}",
                "",
            ]
        )

    insert_at = 0
    while insert_at < len(lines) and lines[insert_at].startswith("#include"):
        insert_at += 1
    if insert_at < len(lines) and lines[insert_at].strip() == "":
        insert_at += 1
    return lines[:insert_at] + helpers + lines[insert_at:]


def _transpile_unit(
    unit: Dict[str, object],
    real_type: str,
    kind_ctype_map: Dict[str, str],
    proc_arg_modes: Dict[str, List[str]],
    proc_arg_optional: Dict[str, List[bool]],
    proc_arg_ctypes: Dict[str, List[str]],
    proc_arg_is_array: Dict[str, List[bool]],
    proc_arg_assumed_ranks: Dict[str, List[int]],
    proc_arg_extent_names: Dict[str, List[List[str]]],
    array_result_funcs: set[str],
    vars_map_override: Optional[Dict[str, Var]] = None,
    *,
    one_line: bool = False,
    annotate: bool = False,
) -> List[str]:
    out: List[str] = []
    vars_map = dict(vars_map_override) if vars_map_override is not None else _parse_decls(unit, real_type, kind_ctype_map)
    assumed_extents: Dict[str, List[str]] = {}
    indent = 0
    body_line_nos: List[int] = list(unit.get("body_line_nos", []))
    source_lines: List[str] = list(unit.get("source_lines", []))

    byref_scalars: set[str] = set()

    unit_name_l = str(unit.get("name", "")).lower()
    implicit_result_name = "__result"
    use_implicit_result = bool(unit["kind"] == "function" and not (unit.get("result") or "").strip())

    if unit["kind"] == "function":
        ret_name = (unit.get("result") or "").lower()
        ret_name_c = ret_name if ret_name else implicit_result_name
        ret_lookup = ret_name if ret_name else unit_name_l
        ret_var = vars_map.get(ret_lookup, Var(real_type))
        ret_is_array = bool(ret_var.is_array)
        args = []
        proc_name = str(unit.get("name", "")).lower()
        extent_lists = proc_arg_extent_names.get(proc_name, [])
        for idx, a in enumerate(unit.get("args", [])):
            av = vars_map.get(a.lower(), Var("int"))
            exts = extent_lists[idx] if idx < len(extent_lists) else []
            if exts:
                args.extend([f"const int {nm}" for nm in exts])
            args.append(_emit_decl(a, av, vars_map, real_type, False, as_arg=True))
            if av.is_array and _is_assumed_shape(av.dim):
                exts = extent_lists[idx] if idx < len(extent_lists) else _extent_param_names(a.lower(), 1)
                assumed_extents[a.lower()] = exts
            if (not av.is_array) and (not av.is_external) and (av.intent in {"out", "inout"} or av.optional):
                byref_scalars.add(a.lower())
        ret_decl = f"{ret_var.ctype} *" if ret_is_array else f"{ret_var.ctype} "
        out.append(f"{ret_decl}{unit['name']}({', '.join(args)}) {{")
        indent = 3
        doc = _first_unit_doc_comment(unit)
        if doc:
            out.append(" " * indent + f"/* {doc} */")
        for a in unit.get("args", []):
            av = vars_map.get(a.lower())
            if av is not None and av.comment:
                out.append(" " * indent + f"/* {a}: {av.comment} */")
        for arr_name, exts in assumed_extents.items():
            for k, en in enumerate(exts, start=1):
                out.append(" " * indent + f"/* {en}: extent of {arr_name} (dimension {k}) */")
        # Declare function result explicitly.
        if ret_name_c:
            if ret_is_array:
                dim = _dim_product_expr(
                    ret_var.dim or "1",
                    vars_map,
                    real_type,
                    byref_scalars,
                    assumed_extents,
                )
                out.append(
                    " " * indent
                    + f"{ret_var.ctype} *{ret_name_c} = ({ret_var.ctype}*) malloc(sizeof({ret_var.ctype}) * {_drop_redundant_outer_parens(dim)});"
                )
                out.append(" " * indent + f"if (!{ret_name_c}) return NULL;")
            else:
                out.append(" " * indent + f"{ret_var.ctype} {ret_name_c};")
        for nm, v in vars_map.items():
            if nm in {a.lower() for a in unit.get("args", [])}:
                continue
            if nm == ret_name or (not ret_name and nm == unit_name_l):
                continue
            if v.comment:
                out.append(" " * indent + f"/* {nm}: {v.comment} */")
            out.append(" " * indent + _emit_decl(nm, v, vars_map, real_type, False))
            if v.is_allocatable and v.is_array:
                for en in _alloc_extent_names(nm, max(1, len(_dim_parts(v.dim)))):
                    out.append(" " * indent + f"int {en} = 0;")
    elif unit["kind"] == "subroutine":
        args = []
        proc_name = str(unit.get("name", "")).lower()
        extent_lists = proc_arg_extent_names.get(proc_name, [])
        for idx, a in enumerate(unit.get("args", [])):
            av = vars_map.get(a.lower(), Var("int"))
            exts = extent_lists[idx] if idx < len(extent_lists) else []
            if exts:
                args.extend([f"const int {nm}" for nm in exts])
            args.append(_emit_decl(a, av, vars_map, real_type, False, as_arg=True))
            if av.is_array and _is_assumed_shape(av.dim):
                exts = extent_lists[idx] if idx < len(extent_lists) else _extent_param_names(a.lower(), 1)
                assumed_extents[a.lower()] = exts
            if (not av.is_array) and (not av.is_external) and (av.intent in {"out", "inout"} or av.optional):
                byref_scalars.add(a.lower())
        out.append(f"void {unit['name']}({', '.join(args)}) {{")
        indent = 3
        doc = _first_unit_doc_comment(unit)
        if doc:
            out.append(" " * indent + f"/* {doc} */")
        for a in unit.get("args", []):
            av = vars_map.get(a.lower())
            if av is not None and av.comment:
                out.append(" " * indent + f"/* {a}: {av.comment} */")
        for arr_name, exts in assumed_extents.items():
            for k, en in enumerate(exts, start=1):
                out.append(" " * indent + f"/* {en}: extent of {arr_name} (dimension {k}) */")
        for nm, v in vars_map.items():
            if nm in {a.lower() for a in unit.get("args", [])}:
                continue
            if v.comment:
                out.append(" " * indent + f"/* {nm}: {v.comment} */")
            out.append(" " * indent + _emit_decl(nm, v, vars_map, real_type, False))
            if v.is_allocatable and v.is_array:
                for en in _alloc_extent_names(nm, max(1, len(_dim_parts(v.dim)))):
                    out.append(" " * indent + f"int {en} = 0;")
    else:
        out.append("int main(void) {")
        indent = 3
        doc = _first_unit_doc_comment(unit)
        if doc:
            out.append(" " * indent + f"/* {doc} */")
        saw_random = any(re.match(r"^\s*call\s+random_number\s*\(", _strip_comment(ln).strip(), re.IGNORECASE) for ln in unit["body_lines"])
        if saw_random:
            out.append(" " * indent + "srand(1);")
        for nm, v in vars_map.items():
            if v.comment:
                out.append(" " * indent + f"/* {nm}: {v.comment} */")
            out.append(" " * indent + _emit_decl(nm, v, vars_map, real_type, True))
            if v.is_allocatable and v.is_array:
                for en in _alloc_extent_names(nm, max(1, len(_dim_parts(v.dim)))):
                    out.append(" " * indent + f"int {en} = 0;")
        for nm, v in vars_map.items():
            if v.is_array:
                if v.is_allocatable or _is_assumed_shape(v.dim):
                    continue
                dim = _dim_product_expr(v.dim or "1", vars_map, real_type)
                out.append(" " * indent + f"{nm} = ({v.ctype}*) malloc(sizeof({v.ctype}) * {_drop_redundant_outer_parens(dim)});")
                out.append(" " * indent + f"if (!{nm}) return 1;")
                if v.init is not None:
                    m_ctor_init = re.match(r"^\[\s*(.*)\s*\]$", v.init.strip())
                    if m_ctor_init:
                        items = [x.strip() for x in _split_args_top_level(m_ctor_init.group(1)) if x.strip()]
                        for k, it in enumerate(items):
                            cv = _convert_expr(it, vars_map, real_type)
                            out.append(" " * indent + f"{nm}[{k}] = {cv};")

    loop_stack: List[str] = []
    select_stack: List[Dict[str, bool]] = []
    if_stack: List[Dict[str, bool]] = []
    last_comment_lineno: Optional[int] = None

    def _convert_optional_call_expr(callee: str, raw_args: List[str]) -> str:
        cl = callee.lower()
        modes = proc_arg_modes.get(cl, [])
        opts = proc_arg_optional.get(cl, [])
        ctypes = proc_arg_ctypes.get(cl, [])
        is_arr = proc_arg_is_array.get(cl, [])
        cargs: List[str] = []
        n_expected = max(len(modes), len(opts), len(ctypes), len(is_arr))
        for k in range(n_expected):
            mode = modes[k] if k < len(modes) else "value"
            opt = opts[k] if k < len(opts) else False
            cty = ctypes[k] if k < len(ctypes) else real_type
            arrk = is_arr[k] if k < len(is_arr) else False
            if k >= len(raw_args):
                cargs.append("NULL" if opt else "0")
                continue
            a = raw_args[k]
            cexpr = _convert_expr(a, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
            if mode == "value":
                cargs.append(cexpr)
                continue
            m_id = re.match(r"^\s*([a-z_]\w*)\s*$", a, re.IGNORECASE)
            if m_id:
                nm = m_id.group(1).lower()
                vv = vars_map.get(nm)
                if vv is not None:
                    if vv.is_array:
                        cargs.append(nm)
                    else:
                        cargs.append(f"&{nm}")
                    continue
            if opt and (not arrk):
                cargs.append(f"&(({cty}){{{cexpr}}})")
            else:
                cargs.append(f"&({cexpr})")
        return f"{callee}({', '.join(cargs)})"

    def _close_select_case_if_open() -> None:
        nonlocal indent
        if not select_stack:
            return
        top = select_stack[-1]
        if not top.get("case_open", False):
            return
        if not top.get("current_default", False):
            out.append(" " * indent + "break;")
        indent = max(indent - 3, 0)
        out.append(" " * indent + "}")
        top["case_open"] = False
        top["current_default"] = False

    def _emit_fortran_annot(code_line: str) -> None:
        if not annotate:
            return
        safe = code_line.replace("*/", "* /")
        out.append(" " * indent + f"/* f90: {safe} */")

    for idx, raw in enumerate(unit["body_lines"]):
        code = _strip_comment(raw).strip()
        if not code:
            continue
        low = code.lower()

        if low in {"implicit none", "contains"} or low.startswith("use "):
            continue
        if re.match(r"^(integer(?:\s*\([^)]*\))?|real|double\s+precision|character|logical)\b", low):
            continue
        if not (
            low.startswith("end ")
            or low in {"else", "end", "contains"}
            or low.startswith("case ")
            or low.startswith("case(")
            or low.startswith("case default")
        ):
            _emit_fortran_annot(code)
        if idx < len(body_line_nos):
            ln = body_line_nos[idx]
            if 1 <= ln <= len(source_lines) and ln != last_comment_lineno:
                cmt = _extract_fortran_comment(source_lines[ln - 1])
                if cmt:
                    out.append(" " * indent + f"/* {cmt} */")
                    last_comment_lineno = ln

        m_do = re.match(r"^do\s+([a-z_]\w*)\s*=\s*([^,]+)\s*,\s*([^,]+)(?:\s*,\s*([^,]+))?$", code, re.IGNORECASE)
        if m_do:
            v = m_do.group(1).strip()
            lo = _convert_expr(m_do.group(2).strip(), vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
            hi = _convert_expr(m_do.group(3).strip(), vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
            st = _convert_expr((m_do.group(4) or "1").strip(), vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
            if st.strip() == "1":
                out.append(" " * indent + f"for ({v} = {lo}; {v} <= {hi}; ++{v}) {{")
            else:
                out.append(" " * indent + f"for ({v} = {lo}; {v} <= {hi}; {v} += {st}) {{")
            indent += 3
            loop_stack.append(v)
            continue
        m_alloc = re.match(r"^allocate\s*\(\s*([a-z_]\w*)\s*\(\s*(.+)\s*\)\s*\)\s*$", code, re.IGNORECASE)
        if m_alloc:
            an = m_alloc.group(1).lower()
            av = vars_map.get(an)
            if av is not None and av.is_array and av.is_allocatable:
                shp_items = [x.strip() for x in _split_args_top_level(m_alloc.group(2)) if x.strip()]
                rank = max(1, len(_dim_parts(av.dim)))
                if len(shp_items) == rank:
                    shp_c = [_convert_expr(s, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names) for s in shp_items]
                    n_total = _dim_product_from_parts(shp_c, vars_map, real_type, byref_scalars, assumed_extents)
                    out.append(" " * indent + f"if ({an}) free({an});")
                    out.append(" " * indent + f"{an} = ({av.ctype}*) malloc(sizeof({av.ctype}) * {n_total});")
                    out.append(" " * indent + f"if (!{an} && ({n_total}) > 0) return 1;")
                    for k, en in enumerate(_alloc_extent_names(an, rank)):
                        out.append(" " * indent + f"{en} = {shp_c[k]};")
                    continue
            out.append(" " * indent + f"/* unsupported: {code} */")
            continue
        m_dealloc = re.match(r"^deallocate\s*\(\s*([a-z_]\w*)\s*\)\s*$", code, re.IGNORECASE)
        if m_dealloc:
            an = m_dealloc.group(1).lower()
            av = vars_map.get(an)
            if av is not None and av.is_array and av.is_allocatable:
                out.append(" " * indent + f"if ({an}) free({an});")
                out.append(" " * indent + f"{an} = NULL;")
                for en in _alloc_extent_names(an, max(1, len(_dim_parts(av.dim)))):
                    out.append(" " * indent + f"{en} = 0;")
                continue
            out.append(" " * indent + f"/* unsupported: {code} */")
            continue
        if low == "do":
            out.append(" " * indent + "for (;;) {")
            indent += 3
            loop_stack.append("")
            continue
        if low == "end do":
            indent = max(indent - 3, 0)
            out.append(" " * indent + "}")
            if loop_stack:
                loop_stack.pop()
            continue
        if low == "exit":
            out.append(" " * indent + "break;")
            continue
        if low == "return":
            if unit["kind"] == "function":
                if unit.get("result"):
                    out.append(" " * indent + f"return {unit['result']};")
                else:
                    out.append(" " * indent + f"return {implicit_result_name};")
            elif unit["kind"] == "subroutine":
                out.append(" " * indent + "return;")
            else:
                out.append(" " * indent + "return 0;")
            continue
        m_stop = re.match(r"^stop(?:\s*\(\s*([^)]*)\s*\)|\s+(.+))?\s*$", code, re.IGNORECASE)
        if m_stop:
            code_arg = (m_stop.group(1) if m_stop.group(1) is not None else m_stop.group(2)) or ""
            code_arg = code_arg.strip()
            if not code_arg:
                c_stop = "0"
            else:
                # Integer stop codes map directly; strings map to failure.
                if (code_arg.startswith('"') and code_arg.endswith('"')) or (code_arg.startswith("'") and code_arg.endswith("'")):
                    c_stop = "1"
                else:
                    c_stop = _convert_expr(
                        code_arg,
                        vars_map,
                        real_type,
                        byref_scalars,
                        assumed_extents,
                        proc_arg_extent_names,
                    )
            if unit["kind"] == "program":
                out.append(" " * indent + f"return {c_stop};")
            else:
                out.append(" " * indent + f"exit({c_stop});")
            continue

        m_select = re.match(r"^select\s+case\s*\((.+)\)\s*$", code, re.IGNORECASE)
        if m_select:
            sel = _convert_expr(
                m_select.group(1).strip(),
                vars_map,
                real_type,
                byref_scalars,
                assumed_extents,
                proc_arg_extent_names,
            )
            out.append(" " * indent + f"switch ({sel}) {{")
            indent += 3
            select_stack.append({"case_open": False, "current_default": False})
            continue

        if re.match(r"^end\s+select\s*$", code, re.IGNORECASE):
            if not select_stack:
                out.append(" " * indent + f"/* unsupported: {code} */")
                continue
            _close_select_case_if_open()
            indent = max(indent - 3, 0)
            out.append(" " * indent + "}")
            select_stack.pop()
            continue

        m_case = re.match(r"^case\s*\((.+)\)\s*$", code, re.IGNORECASE)
        if m_case:
            if not select_stack:
                out.append(" " * indent + f"/* unsupported: {code} */")
                continue
            _close_select_case_if_open()
            sel_list = [x.strip() for x in _split_args_top_level(m_case.group(1)) if x.strip()]
            if any(":" in s for s in sel_list):
                out.append(" " * indent + f"/* unsupported range-case: {code} */")
                continue
            for s in sel_list:
                cv = _convert_expr(s, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                out.append(" " * indent + f"case {cv}:")
            out.append(" " * indent + "{")
            indent += 3
            select_stack[-1]["case_open"] = True
            select_stack[-1]["current_default"] = False
            continue

        if re.match(r"^case\s+default\s*$", code, re.IGNORECASE):
            if not select_stack:
                out.append(" " * indent + f"/* unsupported: {code} */")
                continue
            _close_select_case_if_open()
            out.append(" " * indent + "default:")
            out.append(" " * indent + "{")
            indent += 3
            select_stack[-1]["case_open"] = True
            select_stack[-1]["current_default"] = True
            continue

        m_call_rn = re.match(r"^call\s+random_number\s*\(\s*(.*)\s*\)\s*$", code, re.IGNORECASE)
        if m_call_rn:
            target_raw = m_call_rn.group(1).strip()
            m_id = re.match(r"^([a-z_]\w*)$", target_raw, re.IGNORECASE)
            if m_id:
                arr = m_id.group(1).lower()
                av = vars_map.get(arr)
                if av and av.is_array:
                    dparts = _resolved_dim_parts(av, arr, assumed_extents)
                    rank = max(1, len(dparts))
                    cty = (av.ctype or real_type).lower()
                    suf = "double" if cty == "double" else "float"
                    if rank >= 3 and len(dparts) >= 3:
                        d1 = _dim_extent_expr(dparts[0], vars_map, real_type, byref_scalars, assumed_extents)
                        d2 = _dim_extent_expr(dparts[1], vars_map, real_type, byref_scalars, assumed_extents)
                        d3 = _dim_extent_expr(dparts[2], vars_map, real_type, byref_scalars, assumed_extents)
                        out.append(" " * indent + f"fill_rand_3d_{suf}({d1}, {d2}, {d3}, {arr});")
                    elif rank >= 2 and len(dparts) >= 2:
                        d1 = _dim_extent_expr(dparts[0], vars_map, real_type, byref_scalars, assumed_extents)
                        d2 = _dim_extent_expr(dparts[1], vars_map, real_type, byref_scalars, assumed_extents)
                        out.append(" " * indent + f"fill_rand_2d_{suf}({d1}, {d2}, {arr});")
                    else:
                        n1 = _dim_product_expr(av.dim or "1", vars_map, real_type, byref_scalars, assumed_extents)
                        out.append(" " * indent + f"fill_rand_1d_{suf}({n1}, {arr});")
                else:
                    out.append(" " * indent + "/* unsupported random_number target */")
                continue
            # Scalar/element target, e.g. random_number(x(i))
            target_c = _convert_expr(target_raw, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
            cty = real_type
            toks = re.findall(r"\b([a-z_]\w*)\b", target_raw, flags=re.IGNORECASE)
            for t in toks:
                vv = vars_map.get(t.lower())
                if vv is not None:
                    cty = vv.ctype or real_type
                    break
            if cty == "double":
                out.append(" " * indent + f"{target_c} = (double)rand() / (double)RAND_MAX;")
            else:
                out.append(" " * indent + f"{target_c} = (float)rand() / (float)RAND_MAX;")
            continue
        m_call_rs = re.match(r"^call\s+random_seed(?:\s*\((.*)\))?\s*$", code, re.IGNORECASE)
        if m_call_rs:
            args_txt = (m_call_rs.group(1) or "").strip()
            if not args_txt:
                # srand(1) is already emitted in main when random is used.
                continue
            size_m = re.search(r"(?i)\bsize\s*=\s*([a-z_]\w*)\b", args_txt)
            if size_m:
                nm = size_m.group(1).lower()
                if nm in vars_map:
                    out.append(" " * indent + f"{nm} = 1;")
                    continue
            # Ignore put/get keyword forms for now.
            continue

        m_if_block = re.match(r"^if\s*\((.+)\)\s*then\s*$", code, re.IGNORECASE)
        if m_if_block:
            cond = _convert_expr(
                m_if_block.group(1).strip(),
                vars_map,
                real_type,
                byref_scalars,
                assumed_extents,
                proc_arg_extent_names,
            )
            out.append(" " * indent + f"if ({cond}) {{")
            indent += 3
            if_stack.append({"branch_open": True})
            continue

        m_else_if_block = re.match(r"^else\s+if\s*\((.+)\)\s*then\s*$", code, re.IGNORECASE)
        if m_else_if_block:
            if not if_stack:
                out.append(" " * indent + f"/* unsupported: {code} */")
                continue
            if if_stack[-1].get("branch_open", False):
                indent = max(indent - 3, 0)
                out.append(" " * indent + "}")
            cond = _convert_expr(
                m_else_if_block.group(1).strip(),
                vars_map,
                real_type,
                byref_scalars,
                assumed_extents,
                proc_arg_extent_names,
            )
            out.append(" " * indent + f"else if ({cond}) {{")
            indent += 3
            if_stack[-1]["branch_open"] = True
            continue

        if low == "else":
            if not if_stack:
                out.append(" " * indent + f"/* unsupported: {code} */")
                continue
            if if_stack[-1].get("branch_open", False):
                indent = max(indent - 3, 0)
                out.append(" " * indent + "}")
            out.append(" " * indent + "else {")
            indent += 3
            if_stack[-1]["branch_open"] = True
            continue

        if re.match(r"^end\s+if\s*$", code, re.IGNORECASE):
            if not if_stack:
                out.append(" " * indent + f"/* unsupported: {code} */")
                continue
            if if_stack[-1].get("branch_open", False):
                indent = max(indent - 3, 0)
                out.append(" " * indent + "}")
            if_stack.pop()
            continue

        m_if_ret = re.match(r"^if\s*\((.+)\)\s*return\s*$", code, re.IGNORECASE)
        if m_if_ret:
            cond = _convert_expr(m_if_ret.group(1).strip(), vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
            out.append(" " * indent + f"if ({cond}) return;")
            continue
        m_if_inline = re.match(r"^if\s*\((.+)\)\s*(.+)$", code, re.IGNORECASE)
        if m_if_inline:
            cond = _convert_expr(m_if_inline.group(1).strip(), vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
            tail = m_if_inline.group(2).strip()
            m_tail_asn = re.match(r"^(.+?)\s*=\s*(.+)$", tail, re.IGNORECASE)
            m_tail_call = re.match(r"^call\s+([a-z_]\w*)\s*\((.*)\)\s*$", tail, re.IGNORECASE)
            if m_tail_asn:
                lhs_raw = m_tail_asn.group(1).strip()
                rhs_raw = m_tail_asn.group(2).strip()
                if use_implicit_result and lhs_raw.lower() == unit_name_l:
                    lhs_raw = implicit_result_name
                lhs = _convert_expr(lhs_raw, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                m_rhs_any_call = re.match(r"^\s*([a-z_]\w*)\s*\((.*)\)\s*$", rhs_raw, re.IGNORECASE)
                if m_rhs_any_call and any(proc_arg_optional.get(m_rhs_any_call.group(1).lower(), [])):
                    args_rhs = _split_args_top_level(m_rhs_any_call.group(2).strip()) if m_rhs_any_call.group(2).strip() else []
                    rhs = _convert_optional_call_expr(m_rhs_any_call.group(1), args_rhs)
                else:
                    rhs = _convert_expr(rhs_raw, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                out.append(" " * indent + f"if ({cond}) {lhs} = {rhs};")
                continue
            if m_tail_call:
                callee = m_tail_call.group(1)
                fargs = _split_args_top_level(m_tail_call.group(2).strip()) if m_tail_call.group(2).strip() else []
                modes = proc_arg_modes.get(callee.lower(), [])
                opts = proc_arg_optional.get(callee.lower(), [])
                extent_lists = proc_arg_extent_names.get(callee.lower(), [])
                cargs: List[str] = []
                n_expected = max(len(modes), len(opts))
                for k in range(n_expected):
                    if k >= len(fargs):
                        cargs.append("NULL" if (k < len(opts) and opts[k]) else "0")
                        continue
                    a = fargs[k]
                    exts = extent_lists[k] if k < len(extent_lists) else []
                    if exts:
                        m_id0 = re.match(r"^\s*([a-z_]\w*)\s*$", a, re.IGNORECASE)
                        if m_id0:
                            nm0 = m_id0.group(1).lower()
                            vv0 = vars_map.get(nm0)
                            if vv0 is not None and vv0.is_array:
                                dps0 = _dim_parts(vv0.dim)
                                if len(dps0) >= len(exts):
                                    for d in dps0[: len(exts)]:
                                        cargs.append(_convert_expr(d, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names))
                                else:
                                    cargs.extend(["1"] * len(exts))
                            else:
                                cargs.extend(["1"] * len(exts))
                        else:
                            cargs.extend(["1"] * len(exts))
                    mode = modes[k] if k < len(modes) else "value"
                    cexpr = _convert_expr(a, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                    if mode == "value":
                        cargs.append(cexpr)
                    else:
                        m_id = re.match(r"^\s*([a-z_]\w*)\s*$", a, re.IGNORECASE)
                        if m_id:
                            nm = m_id.group(1).lower()
                            vv = vars_map.get(nm)
                            if vv is not None:
                                cargs.append(nm if vv.is_array else f"&{nm}")
                            else:
                                cargs.append(f"&({cexpr})")
                        else:
                            cargs.append(f"&({cexpr})")
                out.append(" " * indent + f"if ({cond}) {callee}({', '.join(cargs)});")
                continue
            out.append(" " * indent + f"/* unsupported: {code} */")
            continue
        m_call = re.match(r"^call\s+([a-z_]\w*)\s*\((.*)\)\s*$", code, re.IGNORECASE)
        if m_call:
            callee = m_call.group(1)
            fargs = _split_args_top_level(m_call.group(2).strip()) if m_call.group(2).strip() else []
            modes = proc_arg_modes.get(callee.lower(), [])
            opts = proc_arg_optional.get(callee.lower(), [])
            extent_lists = proc_arg_extent_names.get(callee.lower(), [])
            cargs: List[str] = []
            n_expected = max(len(modes), len(opts))
            for k in range(n_expected):
                if k >= len(fargs):
                    if k < len(opts) and opts[k]:
                        cargs.append("NULL")
                        continue
                    cargs.append("0")
                    continue
                a = fargs[k]
                exts = extent_lists[k] if k < len(extent_lists) else []
                if exts:
                    m_id0 = re.match(r"^\s*([a-z_]\w*)\s*$", a, re.IGNORECASE)
                    if m_id0:
                        nm0 = m_id0.group(1).lower()
                        vv0 = vars_map.get(nm0)
                        if vv0 is not None and vv0.is_array:
                            dps0 = _dim_parts(vv0.dim)
                            if len(dps0) >= len(exts):
                                for d in dps0[: len(exts)]:
                                    cargs.append(_convert_expr(d, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names))
                            else:
                                cargs.extend(["1"] * len(exts))
                        else:
                            cargs.extend(["1"] * len(exts))
                    else:
                        cargs.extend(["1"] * len(exts))
                mode = modes[k] if k < len(modes) else "value"
                cexpr = _convert_expr(a, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                if mode == "value":
                    cargs.append(cexpr)
                else:
                    m_id = re.match(r"^\s*([a-z_]\w*)\s*$", a, re.IGNORECASE)
                    if m_id:
                        nm = m_id.group(1).lower()
                        vv = vars_map.get(nm)
                        if vv is not None:
                            if vv.is_array or nm in byref_scalars:
                                cargs.append(nm)
                            else:
                                cargs.append(f"&{nm}")
                        else:
                            cargs.append(f"&({cexpr})")
                    else:
                        cargs.append(f"&({cexpr})")
            out.append(" " * indent + f"{callee}({', '.join(cargs)});")
            continue

        m_print = re.match(r"^print\s*\*\s*,\s*(.+)$", code, re.IGNORECASE)
        if m_print:
            raw_items = [x.strip() for x in _split_args_top_level(m_print.group(1)) if x.strip()]
            items: List[str] = []
            for rit in raw_items:
                parts = _split_concat_top_level(rit)
                if len(parts) > 1:
                    items.extend(parts)
                else:
                    items.append(rit)
            if len(items) == 1:
                m_vsub1 = re.match(r"^([a-z_]\w*)\s*\(\s*(.+)\s*\)\s*$", items[0], re.IGNORECASE)
                if m_vsub1:
                    an = m_vsub1.group(1).lower()
                    av = vars_map.get(an)
                    inner = m_vsub1.group(2).strip()
                    if av is not None and av.is_array and len(_resolved_dim_parts(av, an, assumed_extents)) == 1:
                        m_ctor_idx = re.match(r"^\[\s*(.*)\s*\]$", inner)
                        cty = (av.ctype or real_type).lower()
                        efmt = "%d" if cty == "int" else "%g"
                        if m_ctor_idx:
                            idx_items = [x.strip() for x in _split_args_top_level(m_ctor_idx.group(1)) if x.strip()]
                            out.append(" " * indent + "{")
                            out.append(" " * (indent + 3) + "int __first = 1;")
                            for iv in idx_items:
                                civ = _convert_expr(iv, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                out.append(" " * (indent + 3) + f'printf("%s{efmt}", __first ? "" : " ", {an}[({civ}) - 1]);')
                                out.append(" " * (indent + 3) + "__first = 0;")
                            out.append(" " * (indent + 3) + 'printf("\\n");')
                            out.append(" " * indent + "}")
                            continue
                        m_idx_arr = re.match(r"^([a-z_]\w*)$", inner, re.IGNORECASE)
                        if m_idx_arr:
                            idxn = m_idx_arr.group(1).lower()
                            ivv = vars_map.get(idxn)
                            if ivv is not None and ivv.is_array:
                                if ivv.is_allocatable:
                                    nidx = _dim_product_from_parts(
                                        _resolved_dim_parts(ivv, idxn, assumed_extents),
                                        vars_map,
                                        real_type,
                                        byref_scalars,
                                        assumed_extents,
                                    )
                                else:
                                    nidx = _dim_product_expr(ivv.dim or "1", vars_map, real_type, byref_scalars, assumed_extents)
                                out.append(" " * indent + f"for (int i_pr = 0; i_pr < {nidx}; ++i_pr) {{")
                                out.append(" " * (indent + 3) + f"int __iv = {idxn}[i_pr];")
                                out.append(" " * (indent + 3) + f'printf("{efmt}%s", {an}[__iv - 1], (i_pr + 1 < {nidx}) ? " " : "\\n");')
                                out.append(" " * indent + "}")
                                continue
                m_sec = re.match(r"^([a-z_]\w*)\s*\((.+)\)\s*$", items[0], re.IGNORECASE)
                if m_sec:
                    an = m_sec.group(1).lower()
                    av = vars_map.get(an)
                    if av is not None and av.is_array:
                        idx_parts = _split_args_top_level(m_sec.group(2))
                        dparts = _resolved_dim_parts(av, an, assumed_extents)
                        rank = len(dparts)
                        if rank in {2, 3} and len(idx_parts) == rank:
                            cty = (av.ctype or real_type).lower()
                            efmt = "%d" if cty == "int" else "%g"
                            dimc = [
                                _convert_expr(dparts[k], vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                for k in range(rank)
                            ]

                            def _parse_dim_spec(txt: str, dflt_hi: str) -> dict:
                                t = txt.strip()
                                m_ctor = re.match(r"^\[\s*(.*)\s*\]$", t)
                                if m_ctor:
                                    vals = [x.strip() for x in _split_args_top_level(m_ctor.group(1)) if x.strip()]
                                    cvals = [
                                        _convert_expr(vv, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                        for vv in vals
                                    ]
                                    return {"kind": "vec", "vals": cvals}
                                if ":" in t:
                                    sp = _split_colon_top_level(t)
                                    lo = _convert_expr((sp[0] or "1").strip(), vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                    hi = _convert_expr((sp[1] or dflt_hi).strip(), vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                    st = _convert_expr((sp[2] if len(sp) == 3 else "1").strip(), vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                    return {"kind": "sec", "lo": lo, "hi": hi, "st": st}
                                val = _convert_expr(t, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                return {"kind": "scalar", "val": val}

                            specs = [_parse_dim_spec(idx_parts[k], dimc[k]) for k in range(rank)]
                            if any(sp["kind"] != "scalar" for sp in specs):
                                out.append(" " * indent + "{")
                                out.append(" " * (indent + 3) + "int __first = 1;")
                                idx_map: Dict[int, str] = {}

                                def _lin_expr() -> str:
                                    i1 = idx_map.get(1, "1")
                                    lin = f"(({i1}) - 1)"
                                    if rank >= 2:
                                        i2 = idx_map.get(2, "1")
                                        lin = f"{lin} + ({dimc[0]}) * (({i2}) - 1)"
                                    if rank >= 3:
                                        i3 = idx_map.get(3, "1")
                                        lin = f"{lin} + ({dimc[0]}) * ({dimc[1]}) * (({i3}) - 1)"
                                    return lin

                                def _emit_dim(dim_k: int, ind: int) -> None:
                                    if dim_k < 1:
                                        lin = _lin_expr()
                                        out.append(" " * ind + f'printf("%s{efmt}", __first ? "" : " ", {an}[{lin}]);')
                                        out.append(" " * ind + "__first = 0;")
                                        return
                                    sp = specs[dim_k - 1]
                                    if sp["kind"] == "scalar":
                                        idx_map[dim_k] = sp["val"]
                                        _emit_dim(dim_k - 1, ind)
                                        return
                                    if sp["kind"] == "vec":
                                        for vv in sp["vals"]:
                                            idx_map[dim_k] = vv
                                            _emit_dim(dim_k - 1, ind)
                                        return
                                    vnm = f"__i{dim_k}"
                                    idx_map[dim_k] = vnm
                                    out.append(" " * ind + f"for (int {vnm} = {sp['lo']}; ({sp['st']}) > 0 ? {vnm} <= {sp['hi']} : {vnm} >= {sp['hi']}; {vnm} += {sp['st']}) {{")
                                    _emit_dim(dim_k - 1, ind + 3)
                                    out.append(" " * ind + "}")

                                _emit_dim(rank, indent + 3)
                                out.append(" " * (indent + 3) + 'printf("\\n");')
                                out.append(" " * indent + "}")
                                continue
                        if len(idx_parts) == 2 and len(dparts) >= 2:
                            d1 = _convert_expr(dparts[0], vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                            d2 = _convert_expr(dparts[1], vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                            cty = (av.ctype or real_type).lower()
                            efmt = "%d" if cty == "int" else "%g"
                            m_v0_ctor = re.match(r"^\[\s*(.*)\s*\]$", idx_parts[0].strip())
                            m_v1_ctor = re.match(r"^\[\s*(.*)\s*\]$", idx_parts[1].strip())
                            if m_v0_ctor or m_v1_ctor:
                                def _dim_span(idx_txt: str, dflt_hi: str) -> tuple[str, str, str]:
                                    idx_txt = idx_txt.strip()
                                    if ":" in idx_txt:
                                        sp = _split_colon_top_level(idx_txt)
                                        lo = _convert_expr((sp[0] or "1").strip(), vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                        hi = _convert_expr((sp[1] or dflt_hi).strip(), vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                        st = _convert_expr((sp[2] if len(sp) == 3 else "1").strip(), vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                        return lo, hi, st
                                    s = _convert_expr(idx_txt, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                    return s, s, "1"
                                out.append(" " * indent + "{")
                                out.append(" " * (indent + 3) + "int __first = 1;")
                                if m_v0_ctor and m_v1_ctor:
                                    v0 = [x.strip() for x in _split_args_top_level(m_v0_ctor.group(1)) if x.strip()]
                                    v1 = [x.strip() for x in _split_args_top_level(m_v1_ctor.group(1)) if x.strip()]
                                    for jv in v1:
                                        cj = _convert_expr(jv, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                        for iv in v0:
                                            ci = _convert_expr(iv, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                            out.append(" " * (indent + 3) + f'printf("%s{efmt}", __first ? "" : " ", {an}[(({ci}) - 1) + ({d1}) * (({cj}) - 1)]);')
                                            out.append(" " * (indent + 3) + "__first = 0;")
                                elif m_v0_ctor:
                                    v0 = [x.strip() for x in _split_args_top_level(m_v0_ctor.group(1)) if x.strip()]
                                    j_lo, j_hi, j_st = _dim_span(idx_parts[1], d2)
                                    out.append(" " * (indent + 3) + f"for (int j_pr = {j_lo}; ({j_st}) > 0 ? j_pr <= {j_hi} : j_pr >= {j_hi}; j_pr += {j_st}) {{")
                                    for iv in v0:
                                        ci = _convert_expr(iv, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                        out.append(" " * (indent + 6) + f'printf("%s{efmt}", __first ? "" : " ", {an}[(({ci}) - 1) + ({d1}) * (j_pr - 1)]);')
                                        out.append(" " * (indent + 6) + "__first = 0;")
                                    out.append(" " * (indent + 3) + "}")
                                else:
                                    i_lo, i_hi, i_st = _dim_span(idx_parts[0], d1)
                                    v1 = [x.strip() for x in _split_args_top_level(m_v1_ctor.group(1)) if x.strip()]
                                    for jv in v1:
                                        cj = _convert_expr(jv, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                        out.append(" " * (indent + 3) + f"for (int i_pr = {i_lo}; ({i_st}) > 0 ? i_pr <= {i_hi} : i_pr >= {i_hi}; i_pr += {i_st}) {{")
                                        out.append(" " * (indent + 6) + f'printf("%s{efmt}", __first ? "" : " ", {an}[(i_pr - 1) + ({d1}) * (({cj}) - 1)]);')
                                        out.append(" " * (indent + 6) + "__first = 0;")
                                        out.append(" " * (indent + 3) + "}")
                                out.append(" " * (indent + 3) + 'printf("\\n");')
                                out.append(" " * indent + "}")
                                continue
                            sec0 = ":" in idx_parts[0]
                            sec1 = ":" in idx_parts[1]
                            if sec0 or sec1:
                                if sec0:
                                    sp0 = _split_colon_top_level(idx_parts[0].strip())
                                    if len(sp0) not in {2, 3}:
                                        sp0 = [idx_parts[0].strip(), idx_parts[0].strip(), "1"]
                                else:
                                    sp0 = [idx_parts[0].strip(), idx_parts[0].strip(), "1"]
                                if sec1:
                                    sp1 = _split_colon_top_level(idx_parts[1].strip())
                                    if len(sp1) not in {2, 3}:
                                        sp1 = [idx_parts[1].strip(), idx_parts[1].strip(), "1"]
                                else:
                                    sp1 = [idx_parts[1].strip(), idx_parts[1].strip(), "1"]
                                i1_lo = _convert_expr((sp0[0] or "1").strip(), vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                i1_hi = _convert_expr((sp0[1] or d1).strip(), vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                i1_st = _convert_expr((sp0[2] if len(sp0) == 3 else "1").strip(), vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                i2_lo = _convert_expr((sp1[0] or "1").strip(), vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                i2_hi = _convert_expr((sp1[1] or d2).strip(), vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                i2_st = _convert_expr((sp1[2] if len(sp1) == 3 else "1").strip(), vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                out.append(" " * indent + "{")
                                out.append(" " * (indent + 3) + "int __first = 1;")
                                out.append(" " * (indent + 3) + f"for (int j_pr = {i2_lo}; ({i2_st}) > 0 ? j_pr <= {i2_hi} : j_pr >= {i2_hi}; j_pr += {i2_st}) {{")
                                out.append(" " * (indent + 6) + f"for (int i_pr = {i1_lo}; ({i1_st}) > 0 ? i_pr <= {i1_hi} : i_pr >= {i1_hi}; i_pr += {i1_st}) {{")
                                out.append(" " * (indent + 9) + f"int __lin = (i_pr - 1) + ({d1}) * (j_pr - 1);")
                                out.append(" " * (indent + 9) + f'printf("%s{efmt}", __first ? "" : " ", {an}[__lin]);')
                                out.append(" " * (indent + 9) + "__first = 0;")
                                out.append(" " * (indent + 6) + "}")
                                out.append(" " * (indent + 3) + "}")
                                out.append(" " * (indent + 3) + 'printf("\\n");')
                                out.append(" " * indent + "}")
                                continue
                m_arr = re.match(r"^([a-z_]\w*)$", items[0], re.IGNORECASE)
                if m_arr:
                    an = m_arr.group(1).lower()
                    av = vars_map.get(an)
                    if av is not None and av.is_array:
                        if av.is_allocatable:
                            npr = _dim_product_from_parts(
                                _resolved_dim_parts(av, an, assumed_extents),
                                vars_map,
                                real_type,
                                byref_scalars,
                                assumed_extents,
                            )
                        else:
                            npr = _dim_product_expr(av.dim or "1", vars_map, real_type, byref_scalars, assumed_extents)
                        cty = (av.ctype or real_type).lower()
                        efmt = "%d" if cty == "int" else "%g"
                        out.append(" " * indent + f"for (int i_pr = 0; i_pr < {npr}; ++i_pr) {{")
                        out.append(" " * (indent + 3) + f'printf("{efmt}%s", {an}[i_pr], (i_pr + 1 < {npr}) ? " " : "\\n");')
                        out.append(" " * indent + "}")
                        continue
                # Array expression print, e.g. print*, 10*x
                expr0 = items[0]
                m_pack_call = re.match(r"^pack\s*\((.*)\)\s*$", expr0, re.IGNORECASE)
                if m_pack_call:
                    pargs = [x.strip() for x in _split_args_top_level(m_pack_call.group(1)) if x.strip()]
                    if len(pargs) >= 2:
                        m_arr = re.match(r"^([a-z_]\w*)$", pargs[0], re.IGNORECASE)
                        m_mask = re.match(r"^([a-z_]\w*)$", pargs[1], re.IGNORECASE)
                        if m_arr and m_mask:
                            an = m_arr.group(1).lower()
                            mn = m_mask.group(1).lower()
                            av = vars_map.get(an)
                            mv = vars_map.get(mn)
                            if av is not None and mv is not None and av.is_array and mv.is_array:
                                npr = _dim_product_expr(av.dim or "1", vars_map, real_type, byref_scalars, assumed_extents)
                                cty = (av.ctype or real_type).lower()
                                efmt = "%d" if cty == "int" else "%g"
                                out.append(" " * indent + "{")
                                out.append(" " * (indent + 3) + "int __first = 1;")
                                out.append(" " * (indent + 3) + f"for (int i_pr = 0; i_pr < {npr}; ++i_pr) {{")
                                out.append(" " * (indent + 6) + f"if ({mn}[i_pr]) {{")
                                out.append(" " * (indent + 9) + f'printf("%s{efmt}", __first ? "" : " ", {an}[i_pr]);')
                                out.append(" " * (indent + 9) + "__first = 0;")
                                out.append(" " * (indent + 6) + "}")
                                out.append(" " * (indent + 3) + "}")
                                out.append(" " * (indent + 3) + 'printf("\\n");')
                                out.append(" " * indent + "}")
                                continue
                m_sum_call = re.match(r"^sum\s*\((.*)\)\s*$", expr0, re.IGNORECASE)
                if m_sum_call:
                    sargs = [x.strip() for x in _split_args_top_level(m_sum_call.group(1)) if x.strip()]
                    if sargs:
                        arg0 = sargs[0]
                        dim_no = None
                        mask_arg: Optional[str] = None
                        if len(sargs) >= 2:
                            s1 = sargs[1]
                            m_dim_kw = re.match(r"(?i)^dim\s*=\s*([0-9]+)$", s1)
                            m_mask_kw = re.match(r"(?i)^mask\s*=\s*(.+)$", s1)
                            if m_dim_kw:
                                dim_no = int(m_dim_kw.group(1))
                            elif m_mask_kw:
                                mask_arg = m_mask_kw.group(1).strip()
                            elif re.fullmatch(r"[0-9]+", s1):
                                dim_no = int(s1)
                            else:
                                mask_arg = s1
                        # Generic scalar SUM over an array expression, with optional MASK.
                        if dim_no is None:
                            arr_tokens_0 = [t for t in sorted({t.lower() for t in re.findall(r"\b[a-z_]\w*\b", _strip_comment(arg0), flags=re.IGNORECASE)}) if t in vars_map and vars_map[t].is_array]
                            arr_tokens_m = []
                            if mask_arg:
                                arr_tokens_m = [t for t in sorted({t.lower() for t in re.findall(r"\b[a-z_]\w*\b", _strip_comment(mask_arg), flags=re.IGNORECASE)}) if t in vars_map and vars_map[t].is_array]
                            all_arrs = arr_tokens_0 + [a for a in arr_tokens_m if a not in arr_tokens_0]
                            if all_arrs:
                                base = vars_map.get(all_arrs[0])
                                assert base is not None
                                compatible = all((vars_map.get(a) is not None and vars_map.get(a).dim == base.dim) for a in all_arrs)
                                if compatible:
                                    npr = _dim_product_expr(base.dim or "1", vars_map, real_type, byref_scalars, assumed_extents)
                                    expr_i = arg0
                                    for a in sorted(all_arrs, key=len, reverse=True):
                                        expr_i = re.sub(rf"\b{re.escape(a)}\b", f"{a}[i_pr]", expr_i, flags=re.IGNORECASE)
                                    mask_i = "1"
                                    if mask_arg:
                                        mask_i = mask_arg
                                        for a in sorted(all_arrs, key=len, reverse=True):
                                            mask_i = re.sub(rf"\b{re.escape(a)}\b", f"{a}[i_pr]", mask_i, flags=re.IGNORECASE)
                                    cexpr_i = _convert_expr(expr_i, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                    cmask_i = _convert_expr(mask_i, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                    cty = (base.ctype or real_type).lower()
                                    efmt = "%d" if cty == "int" else "%g"
                                    out.append(" " * indent + "{")
                                    out.append(" " * (indent + 3) + f"{base.ctype} __sum = 0;")
                                    out.append(" " * (indent + 3) + f"for (int i_pr = 0; i_pr < {npr}; ++i_pr) {{")
                                    out.append(" " * (indent + 6) + f"if ({cmask_i}) __sum += {cexpr_i};")
                                    out.append(" " * (indent + 3) + "}")
                                    out.append(" " * (indent + 3) + f'printf("{efmt}\\n", __sum);')
                                    out.append(" " * indent + "}")
                                    continue
                        m_arr0 = re.match(r"^([a-z_]\w*)$", sargs[0], re.IGNORECASE)
                        if m_arr0:
                            an = m_arr0.group(1).lower()
                            av = vars_map.get(an)
                            if av is not None and av.is_array:
                                dparts = _resolved_dim_parts(av, an, assumed_extents)
                                rank = len(dparts)
                                cty = (av.ctype or real_type).lower()
                                efmt = "%d" if cty == "int" else "%g"
                                if mask_arg is not None and dim_no is None:
                                    npr = _dim_product_expr(av.dim or "1", vars_map, real_type, byref_scalars, assumed_extents)
                                    mexpr = mask_arg
                                    for arrn, vv in vars_map.items():
                                        if vv.is_array and vv.dim == av.dim and re.search(rf"\b{re.escape(arrn)}\b", mexpr, re.IGNORECASE):
                                            mexpr = re.sub(rf"\b{re.escape(arrn)}\b", f"{arrn}[i_pr]", mexpr, flags=re.IGNORECASE)
                                    cmask = _convert_expr(mexpr, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                    out.append(" " * indent + "{")
                                    out.append(" " * (indent + 3) + f"{av.ctype} __sum = 0;")
                                    out.append(" " * (indent + 3) + f"for (int i_pr = 0; i_pr < {npr}; ++i_pr) {{")
                                    out.append(" " * (indent + 6) + f"if ({cmask}) __sum += {an}[i_pr];")
                                    out.append(" " * (indent + 3) + "}")
                                    out.append(" " * (indent + 3) + f'printf("{efmt}\\n", __sum);')
                                    out.append(" " * indent + "}")
                                    continue
                                if dim_no is None:
                                    csum = _convert_expr(expr0, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                    out.append(" " * indent + f'printf("{efmt}\\n", {csum});')
                                    continue
                                if rank in {2, 3} and 1 <= dim_no <= rank:
                                    d = [
                                        _convert_expr(dparts[k], vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                        for k in range(rank)
                                    ]
                                    out.append(" " * indent + "{")
                                    out.append(" " * (indent + 3) + "int __first = 1;")
                                    out.append(" " * (indent + 3) + f"{av.ctype} __sum;")
                                    if rank == 2 and dim_no == 1:
                                        out.append(" " * (indent + 3) + f"for (int j = 1; j <= {d[1]}; ++j) {{")
                                        out.append(" " * (indent + 6) + "__sum = 0;")
                                        out.append(" " * (indent + 6) + f"for (int i = 1; i <= {d[0]}; ++i) __sum += {an}[(i - 1) + ({d[0]}) * (j - 1)];")
                                        out.append(" " * (indent + 6) + f'printf("%s{efmt}", __first ? "" : " ", __sum);')
                                        out.append(" " * (indent + 6) + "__first = 0;")
                                        out.append(" " * (indent + 3) + "}")
                                    elif rank == 2 and dim_no == 2:
                                        out.append(" " * (indent + 3) + f"for (int i = 1; i <= {d[0]}; ++i) {{")
                                        out.append(" " * (indent + 6) + "__sum = 0;")
                                        out.append(" " * (indent + 6) + f"for (int j = 1; j <= {d[1]}; ++j) __sum += {an}[(i - 1) + ({d[0]}) * (j - 1)];")
                                        out.append(" " * (indent + 6) + f'printf("%s{efmt}", __first ? "" : " ", __sum);')
                                        out.append(" " * (indent + 6) + "__first = 0;")
                                        out.append(" " * (indent + 3) + "}")
                                    elif rank == 3 and dim_no == 1:
                                        out.append(" " * (indent + 3) + f"for (int k = 1; k <= {d[2]}; ++k) {{")
                                        out.append(" " * (indent + 6) + f"for (int j = 1; j <= {d[1]}; ++j) {{")
                                        out.append(" " * (indent + 9) + "__sum = 0;")
                                        out.append(" " * (indent + 9) + f"for (int i = 1; i <= {d[0]}; ++i) __sum += {an}[(i - 1) + ({d[0]}) * (j - 1) + ({d[0]}) * ({d[1]}) * (k - 1)];")
                                        out.append(" " * (indent + 9) + f'printf("%s{efmt}", __first ? "" : " ", __sum);')
                                        out.append(" " * (indent + 9) + "__first = 0;")
                                        out.append(" " * (indent + 6) + "}")
                                        out.append(" " * (indent + 3) + "}")
                                    elif rank == 3 and dim_no == 2:
                                        out.append(" " * (indent + 3) + f"for (int k = 1; k <= {d[2]}; ++k) {{")
                                        out.append(" " * (indent + 6) + f"for (int i = 1; i <= {d[0]}; ++i) {{")
                                        out.append(" " * (indent + 9) + "__sum = 0;")
                                        out.append(" " * (indent + 9) + f"for (int j = 1; j <= {d[1]}; ++j) __sum += {an}[(i - 1) + ({d[0]}) * (j - 1) + ({d[0]}) * ({d[1]}) * (k - 1)];")
                                        out.append(" " * (indent + 9) + f'printf("%s{efmt}", __first ? "" : " ", __sum);')
                                        out.append(" " * (indent + 9) + "__first = 0;")
                                        out.append(" " * (indent + 6) + "}")
                                        out.append(" " * (indent + 3) + "}")
                                    elif rank == 3 and dim_no == 3:
                                        out.append(" " * (indent + 3) + f"for (int j = 1; j <= {d[1]}; ++j) {{")
                                        out.append(" " * (indent + 6) + f"for (int i = 1; i <= {d[0]}; ++i) {{")
                                        out.append(" " * (indent + 9) + "__sum = 0;")
                                        out.append(" " * (indent + 9) + f"for (int k = 1; k <= {d[2]}; ++k) __sum += {an}[(i - 1) + ({d[0]}) * (j - 1) + ({d[0]}) * ({d[1]}) * (k - 1)];")
                                        out.append(" " * (indent + 9) + f'printf("%s{efmt}", __first ? "" : " ", __sum);')
                                        out.append(" " * (indent + 9) + "__first = 0;")
                                        out.append(" " * (indent + 6) + "}")
                                        out.append(" " * (indent + 3) + "}")
                                    out.append(" " * (indent + 3) + 'printf("\\n");')
                                    out.append(" " * indent + "}")
                                    continue
                m_expr_vsub = re.match(r"^(.*?)([a-z_]\w*)\s*\(\s*(.+)\s*\)(.*)$", expr0, re.IGNORECASE)
                if m_expr_vsub:
                    pre = m_expr_vsub.group(1)
                    an = m_expr_vsub.group(2).lower()
                    inner = m_expr_vsub.group(3).strip()
                    post = m_expr_vsub.group(4) or ""
                    av = vars_map.get(an)
                    if av is not None and av.is_array:
                        idx_parts = _split_args_top_level(inner)
                        dparts = _resolved_dim_parts(av, an, assumed_extents)
                        rank = len(dparts)
                        if rank in {2, 3} and len(idx_parts) == rank:
                            dimc = [
                                _convert_expr(dparts[k], vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                for k in range(rank)
                            ]

                            def _parse_dim_spec(txt: str, dflt_hi: str) -> dict:
                                t = txt.strip()
                                m_ctor = re.match(r"^\[\s*(.*)\s*\]$", t)
                                if m_ctor:
                                    vals = [x.strip() for x in _split_args_top_level(m_ctor.group(1)) if x.strip()]
                                    cvals = [
                                        _convert_expr(vv, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                        for vv in vals
                                    ]
                                    return {"kind": "vec", "vals": cvals}
                                if ":" in t:
                                    sp = _split_colon_top_level(t)
                                    lo = _convert_expr((sp[0] or "1").strip(), vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                    hi = _convert_expr((sp[1] or dflt_hi).strip(), vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                    st = _convert_expr((sp[2] if len(sp) == 3 else "1").strip(), vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                    return {"kind": "sec", "lo": lo, "hi": hi, "st": st}
                                val = _convert_expr(t, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                return {"kind": "scalar", "val": val}

                            specs = [_parse_dim_spec(idx_parts[k], dimc[k]) for k in range(rank)]
                            if any(sp["kind"] != "scalar" for sp in specs):
                                expr_elem = f"{pre}__elem{post}"
                                cexpr = _convert_expr(expr_elem, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                out.append(" " * indent + "{")
                                out.append(" " * (indent + 3) + "int __first = 1;")
                                out.append(" " * (indent + 3) + f"{av.ctype} __elem;")

                                idx_map: Dict[int, str] = {}

                                def _lin_expr() -> str:
                                    i1 = idx_map.get(1, "1")
                                    lin = f"(({i1}) - 1)"
                                    if rank >= 2:
                                        i2 = idx_map.get(2, "1")
                                        lin = f"{lin} + ({dimc[0]}) * (({i2}) - 1)"
                                    if rank >= 3:
                                        i3 = idx_map.get(3, "1")
                                        lin = f"{lin} + ({dimc[0]}) * ({dimc[1]}) * (({i3}) - 1)"
                                    return lin

                                def _emit_dim(dim_k: int, ind: int) -> None:
                                    if dim_k < 1:
                                        lin = _lin_expr()
                                        out.append(" " * ind + f"__elem = {an}[{lin}];")
                                        out.append(" " * ind + f'printf("%s%g", __first ? "" : " ", {cexpr});')
                                        out.append(" " * ind + "__first = 0;")
                                        return
                                    sp = specs[dim_k - 1]
                                    if sp["kind"] == "scalar":
                                        idx_map[dim_k] = sp["val"]
                                        _emit_dim(dim_k - 1, ind)
                                        return
                                    if sp["kind"] == "vec":
                                        for vv in sp["vals"]:
                                            idx_map[dim_k] = vv
                                            _emit_dim(dim_k - 1, ind)
                                        return
                                    vnm = f"__i{dim_k}"
                                    idx_map[dim_k] = vnm
                                    out.append(" " * ind + f"for (int {vnm} = {sp['lo']}; ({sp['st']}) > 0 ? {vnm} <= {sp['hi']} : {vnm} >= {sp['hi']}; {vnm} += {sp['st']}) {{")
                                    _emit_dim(dim_k - 1, ind + 3)
                                    out.append(" " * ind + "}")

                                _emit_dim(rank, indent + 3)
                                out.append(" " * (indent + 3) + 'printf("\\n");')
                                out.append(" " * indent + "}")
                                continue
                toks0 = {t.lower() for t in re.findall(r"\b[a-z_]\w*\b", _strip_comment(expr0), flags=re.IGNORECASE)}
                arrs0 = [t for t in sorted(toks0) if t in vars_map and vars_map[t].is_array]
                if arrs0 and not any(re.search(rf"\b{re.escape(a)}\s*\(", expr0, flags=re.IGNORECASE) for a in arrs0):
                    base = vars_map.get(arrs0[0])
                    compatible = base is not None and all((vars_map.get(a) is not None and vars_map.get(a).dim == base.dim) for a in arrs0)
                    if compatible and base is not None:
                        if base.is_allocatable and len(_dim_parts(base.dim)) == 1:
                            npr = _alloc_len_name(arrs0[0])
                        else:
                            npr = _dim_product_expr(base.dim or "1", vars_map, real_type, byref_scalars, assumed_extents)
                        expr_i = expr0
                        for a in sorted(arrs0, key=len, reverse=True):
                            expr_i = re.sub(rf"\b{re.escape(a)}\b", f"{a}[i_pr]", expr_i, flags=re.IGNORECASE)
                        cit = _convert_expr(expr_i, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                        out.append(" " * indent + f"for (int i_pr = 0; i_pr < {npr}; ++i_pr) {{")
                        out.append(" " * (indent + 3) + f'printf("%g%s", {cit}, (i_pr + 1 < {npr}) ? " " : "\\n");')
                        out.append(" " * indent + "}")
                        continue
            # Mixed list with one or more whole-array variables: print on one line.
            arr_items: List[tuple[int, str, Var]] = []
            for ii, it in enumerate(items):
                m_ai = re.match(r"^([a-z_]\w*)$", it, re.IGNORECASE)
                if not m_ai:
                    continue
                an = m_ai.group(1).lower()
                av = vars_map.get(an)
                if av is not None and av.is_array:
                    arr_items.append((ii, an, av))
            if arr_items:
                out.append(" " * indent + "{")
                out.append(" " * (indent + 3) + "int __first_pr = 1;")
                arr_pos = {ii for ii, _, _ in arr_items}
                for ii, it in enumerate(items):
                    if ii in arr_pos:
                        an = [a for j, a, _ in arr_items if j == ii][0]
                        av = vars_map.get(an)
                        assert av is not None
                        if av.is_allocatable:
                            npr = _dim_product_from_parts(
                                _resolved_dim_parts(av, an, assumed_extents),
                                vars_map,
                                real_type,
                                byref_scalars,
                                assumed_extents,
                            )
                        else:
                            npr = _dim_product_expr(av.dim or "1", vars_map, real_type, byref_scalars, assumed_extents)
                        cty = (av.ctype or real_type).lower()
                        efmt = "%d" if cty == "int" else "%g"
                        out.append(" " * (indent + 3) + f"for (int i_pr = 0; i_pr < {npr}; ++i_pr) {{")
                        out.append(" " * (indent + 6) + f'printf("%s{efmt}", __first_pr ? "" : " ", {an}[i_pr]);')
                        out.append(" " * (indent + 6) + "__first_pr = 0;")
                        out.append(" " * (indent + 3) + "}")
                    else:
                        if (it.startswith('"') and it.endswith('"')) or (it.startswith("'") and it.endswith("'")):
                            content = it[1:-1].replace("\\", "\\\\").replace('"', '\\"')
                            out.append(" " * (indent + 3) + f'printf("%s%s", __first_pr ? "" : " ", "{content}");')
                            out.append(" " * (indent + 3) + "__first_pr = 0;")
                        else:
                            cit = _convert_expr(it, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                            out.append(" " * (indent + 3) + f'printf("%s%g", __first_pr ? "" : " ", {cit});')
                            out.append(" " * (indent + 3) + "__first_pr = 0;")
                out.append(" " * (indent + 3) + 'printf("\\n");')
                out.append(" " * indent + "}")
                continue
            fmts: List[str] = []
            cargs: List[str] = []
            for it in items:
                if (it.startswith('"') and it.endswith('"')) or (it.startswith("'") and it.endswith("'")):
                    content = it[1:-1]
                    # Fortran strings are mostly literal; preserve backslashes.
                    content = content.replace("\\", "\\\\")
                    content = content.replace('"', '\\"')
                    lit = f'"{content}"'
                    cargs.append(lit)
                    fmts.append("%s")
                    continue
                m_it_call = re.match(r"^\s*([a-z_]\w*)\s*\((.*)\)\s*$", it, re.IGNORECASE)
                if m_it_call and any(proc_arg_optional.get(m_it_call.group(1).lower(), [])):
                    args_it = _split_args_top_level(m_it_call.group(2).strip()) if m_it_call.group(2).strip() else []
                    cit = _convert_optional_call_expr(m_it_call.group(1), args_it)
                else:
                    cit = _convert_expr(it, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                cargs.append(cit)
                mvar = re.match(r"^([a-z_]\w*)$", it, re.IGNORECASE)
                if mvar:
                    cty = vars_map.get(mvar.group(1).lower(), Var("int")).ctype.lower()
                    if "char" in cty:
                        fmts.append("%s")
                    elif cty != "int":
                        fmts.append("%g")
                    else:
                        fmts.append("%d")
                    continue
                if re.match(r"^[+\-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+\-]?\d+)?$", cit):
                    fmts.append("%g")
                else:
                    fmts.append("%g")
            if cargs:
                out.append(" " * indent + f'printf("{" ".join(fmts)}\\n", {", ".join(cargs)});')
            else:
                out.append(" " * indent + 'printf("\\n");')
            continue

        parsed_write = _split_leading_paren_group(code, "write")
        if parsed_write:
            ctl = parsed_write[0].strip()
            tail = (parsed_write[1] or "").strip()
            # Support formatted WRITE to stdout: write(*,'(...)') items
            if ctl.startswith("*"):
                if tail.startswith(","):
                    tail = tail[1:].strip()
                if not tail:
                    out.append(" " * indent + 'printf("\\n");')
                    continue
                items = [x.strip() for x in _split_args_top_level(tail) if x.strip()]
                if len(items) == 1:
                    expr0 = items[0]
                    m_pack_call = re.match(r"^pack\s*\((.*)\)\s*$", expr0, re.IGNORECASE)
                    if m_pack_call:
                        pargs = [x.strip() for x in _split_args_top_level(m_pack_call.group(1)) if x.strip()]
                        if len(pargs) >= 2:
                            m_arr = re.match(r"^([a-z_]\w*)$", pargs[0], re.IGNORECASE)
                            m_mask = re.match(r"^([a-z_]\w*)$", pargs[1], re.IGNORECASE)
                            if m_arr and m_mask:
                                an = m_arr.group(1).lower()
                                mn = m_mask.group(1).lower()
                                av = vars_map.get(an)
                                mv = vars_map.get(mn)
                                if av is not None and mv is not None and av.is_array and mv.is_array:
                                    npr = _dim_product_expr(av.dim or "1", vars_map, real_type, byref_scalars, assumed_extents)
                                    cty = (av.ctype or real_type).lower()
                                    efmt = "%d" if cty == "int" else "%g"
                                    out.append(" " * indent + "{")
                                    out.append(" " * (indent + 3) + "int __first = 1;")
                                    out.append(" " * (indent + 3) + f"for (int i_wr = 0; i_wr < {npr}; ++i_wr) {{")
                                    out.append(" " * (indent + 6) + f"if ({mn}[i_wr]) {{")
                                    out.append(" " * (indent + 9) + f'printf("%s{efmt}", __first ? "" : " ", {an}[i_wr]);')
                                    out.append(" " * (indent + 9) + "__first = 0;")
                                    out.append(" " * (indent + 6) + "}")
                                    out.append(" " * (indent + 3) + "}")
                                    out.append(" " * (indent + 3) + 'printf("\\n");')
                                    out.append(" " * indent + "}")
                                    continue
                    m_sum_call = re.match(r"^sum\s*\((.*)\)\s*$", expr0, re.IGNORECASE)
                    if m_sum_call:
                        sargs = [x.strip() for x in _split_args_top_level(m_sum_call.group(1)) if x.strip()]
                        if sargs:
                            arg0 = sargs[0]
                            dim_no = None
                            mask_arg: Optional[str] = None
                            if len(sargs) >= 2:
                                s1 = sargs[1]
                                m_dim_kw = re.match(r"(?i)^dim\s*=\s*([0-9]+)$", s1)
                                m_mask_kw = re.match(r"(?i)^mask\s*=\s*(.+)$", s1)
                                if m_dim_kw:
                                    dim_no = int(m_dim_kw.group(1))
                                elif m_mask_kw:
                                    mask_arg = m_mask_kw.group(1).strip()
                                elif re.fullmatch(r"[0-9]+", s1):
                                    dim_no = int(s1)
                                else:
                                    mask_arg = s1
                            if dim_no is None:
                                arr_tokens_0 = [t for t in sorted({t.lower() for t in re.findall(r"\b[a-z_]\w*\b", _strip_comment(arg0), flags=re.IGNORECASE)}) if t in vars_map and vars_map[t].is_array]
                                arr_tokens_m = []
                                if mask_arg:
                                    arr_tokens_m = [t for t in sorted({t.lower() for t in re.findall(r"\b[a-z_]\w*\b", _strip_comment(mask_arg), flags=re.IGNORECASE)}) if t in vars_map and vars_map[t].is_array]
                                all_arrs = arr_tokens_0 + [a for a in arr_tokens_m if a not in arr_tokens_0]
                                if all_arrs:
                                    base = vars_map.get(all_arrs[0])
                                    assert base is not None
                                    compatible = all((vars_map.get(a) is not None and vars_map.get(a).dim == base.dim) for a in all_arrs)
                                    if compatible:
                                        npr = _dim_product_expr(base.dim or "1", vars_map, real_type, byref_scalars, assumed_extents)
                                        expr_i = arg0
                                        for a in sorted(all_arrs, key=len, reverse=True):
                                            expr_i = re.sub(rf"\b{re.escape(a)}\b", f"{a}[i_wr]", expr_i, flags=re.IGNORECASE)
                                        mask_i = "1"
                                        if mask_arg:
                                            mask_i = mask_arg
                                            for a in sorted(all_arrs, key=len, reverse=True):
                                                mask_i = re.sub(rf"\b{re.escape(a)}\b", f"{a}[i_wr]", mask_i, flags=re.IGNORECASE)
                                        cexpr_i = _convert_expr(expr_i, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                        cmask_i = _convert_expr(mask_i, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                        cty = (base.ctype or real_type).lower()
                                        efmt = "%d" if cty == "int" else "%g"
                                        out.append(" " * indent + "{")
                                        out.append(" " * (indent + 3) + f"{base.ctype} __sum = 0;")
                                        out.append(" " * (indent + 3) + f"for (int i_wr = 0; i_wr < {npr}; ++i_wr) {{")
                                        out.append(" " * (indent + 6) + f"if ({cmask_i}) __sum += {cexpr_i};")
                                        out.append(" " * (indent + 3) + "}")
                                        out.append(" " * (indent + 3) + f'printf("{efmt}\\n", __sum);')
                                        out.append(" " * indent + "}")
                                        continue
                            m_arr0 = re.match(r"^([a-z_]\w*)$", sargs[0], re.IGNORECASE)
                            if m_arr0:
                                an = m_arr0.group(1).lower()
                                av = vars_map.get(an)
                                if av is not None and av.is_array:
                                    dparts = _resolved_dim_parts(av, an, assumed_extents)
                                    rank = len(dparts)
                                    cty = (av.ctype or real_type).lower()
                                    efmt = "%d" if cty == "int" else "%g"
                                    if mask_arg is not None and dim_no is None:
                                        npr = _dim_product_expr(av.dim or "1", vars_map, real_type, byref_scalars, assumed_extents)
                                        mexpr = mask_arg
                                        for arrn, vv in vars_map.items():
                                            if vv.is_array and vv.dim == av.dim and re.search(rf"\b{re.escape(arrn)}\b", mexpr, re.IGNORECASE):
                                                mexpr = re.sub(rf"\b{re.escape(arrn)}\b", f"{arrn}[i_wr]", mexpr, flags=re.IGNORECASE)
                                        cmask = _convert_expr(mexpr, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                        out.append(" " * indent + "{")
                                        out.append(" " * (indent + 3) + f"{av.ctype} __sum = 0;")
                                        out.append(" " * (indent + 3) + f"for (int i_wr = 0; i_wr < {npr}; ++i_wr) {{")
                                        out.append(" " * (indent + 6) + f"if ({cmask}) __sum += {an}[i_wr];")
                                        out.append(" " * (indent + 3) + "}")
                                        out.append(" " * (indent + 3) + f'printf("{efmt}\\n", __sum);')
                                        out.append(" " * indent + "}")
                                        continue
                                    if dim_no is None:
                                        csum = _convert_expr(expr0, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                        out.append(" " * indent + f'printf("{efmt}\\n", {csum});')
                                        continue
                                    if rank in {2, 3} and 1 <= dim_no <= rank:
                                        d = [
                                            _convert_expr(dparts[k], vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                            for k in range(rank)
                                        ]
                                        out.append(" " * indent + "{")
                                        out.append(" " * (indent + 3) + "int __first = 1;")
                                        out.append(" " * (indent + 3) + f"{av.ctype} __sum;")
                                        if rank == 2 and dim_no == 1:
                                            out.append(" " * (indent + 3) + f"for (int j = 1; j <= {d[1]}; ++j) {{")
                                            out.append(" " * (indent + 6) + "__sum = 0;")
                                            out.append(" " * (indent + 6) + f"for (int i = 1; i <= {d[0]}; ++i) __sum += {an}[(i - 1) + ({d[0]}) * (j - 1)];")
                                            out.append(" " * (indent + 6) + f'printf("%s{efmt}", __first ? "" : " ", __sum);')
                                            out.append(" " * (indent + 6) + "__first = 0;")
                                            out.append(" " * (indent + 3) + "}")
                                        elif rank == 2 and dim_no == 2:
                                            out.append(" " * (indent + 3) + f"for (int i = 1; i <= {d[0]}; ++i) {{")
                                            out.append(" " * (indent + 6) + "__sum = 0;")
                                            out.append(" " * (indent + 6) + f"for (int j = 1; j <= {d[1]}; ++j) __sum += {an}[(i - 1) + ({d[0]}) * (j - 1)];")
                                            out.append(" " * (indent + 6) + f'printf("%s{efmt}", __first ? "" : " ", __sum);')
                                            out.append(" " * (indent + 6) + "__first = 0;")
                                            out.append(" " * (indent + 3) + "}")
                                        elif rank == 3 and dim_no == 1:
                                            out.append(" " * (indent + 3) + f"for (int k = 1; k <= {d[2]}; ++k) {{")
                                            out.append(" " * (indent + 6) + f"for (int j = 1; j <= {d[1]}; ++j) {{")
                                            out.append(" " * (indent + 9) + "__sum = 0;")
                                            out.append(" " * (indent + 9) + f"for (int i = 1; i <= {d[0]}; ++i) __sum += {an}[(i - 1) + ({d[0]}) * (j - 1) + ({d[0]}) * ({d[1]}) * (k - 1)];")
                                            out.append(" " * (indent + 9) + f'printf("%s{efmt}", __first ? "" : " ", __sum);')
                                            out.append(" " * (indent + 9) + "__first = 0;")
                                            out.append(" " * (indent + 6) + "}")
                                            out.append(" " * (indent + 3) + "}")
                                        elif rank == 3 and dim_no == 2:
                                            out.append(" " * (indent + 3) + f"for (int k = 1; k <= {d[2]}; ++k) {{")
                                            out.append(" " * (indent + 6) + f"for (int i = 1; i <= {d[0]}; ++i) {{")
                                            out.append(" " * (indent + 9) + "__sum = 0;")
                                            out.append(" " * (indent + 9) + f"for (int j = 1; j <= {d[1]}; ++j) __sum += {an}[(i - 1) + ({d[0]}) * (j - 1) + ({d[0]}) * ({d[1]}) * (k - 1)];")
                                            out.append(" " * (indent + 9) + f'printf("%s{efmt}", __first ? "" : " ", __sum);')
                                            out.append(" " * (indent + 9) + "__first = 0;")
                                            out.append(" " * (indent + 6) + "}")
                                            out.append(" " * (indent + 3) + "}")
                                        elif rank == 3 and dim_no == 3:
                                            out.append(" " * (indent + 3) + f"for (int j = 1; j <= {d[1]}; ++j) {{")
                                            out.append(" " * (indent + 6) + f"for (int i = 1; i <= {d[0]}; ++i) {{")
                                            out.append(" " * (indent + 9) + "__sum = 0;")
                                            out.append(" " * (indent + 9) + f"for (int k = 1; k <= {d[2]}; ++k) __sum += {an}[(i - 1) + ({d[0]}) * (j - 1) + ({d[0]}) * ({d[1]}) * (k - 1)];")
                                            out.append(" " * (indent + 9) + f'printf("%s{efmt}", __first ? "" : " ", __sum);')
                                            out.append(" " * (indent + 9) + "__first = 0;")
                                            out.append(" " * (indent + 6) + "}")
                                            out.append(" " * (indent + 3) + "}")
                                        out.append(" " * (indent + 3) + 'printf("\\n");')
                                        out.append(" " * indent + "}")
                                        continue
                    m_vsub1 = re.match(r"^([a-z_]\w*)\s*\(\s*(.+)\s*\)\s*$", items[0], re.IGNORECASE)
                    if m_vsub1:
                        an = m_vsub1.group(1).lower()
                        av = vars_map.get(an)
                        inner = m_vsub1.group(2).strip()
                        if av is not None and av.is_array and len(_resolved_dim_parts(av, an, assumed_extents)) == 1:
                            m_ctor_idx = re.match(r"^\[\s*(.*)\s*\]$", inner)
                            cty = (av.ctype or real_type).lower()
                            efmt = "%d" if cty == "int" else "%g"
                            if m_ctor_idx:
                                idx_items = [x.strip() for x in _split_args_top_level(m_ctor_idx.group(1)) if x.strip()]
                                out.append(" " * indent + "{")
                                out.append(" " * (indent + 3) + "int __first = 1;")
                                for iv in idx_items:
                                    civ = _convert_expr(iv, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                    out.append(" " * (indent + 3) + f'printf("%s{efmt}", __first ? "" : " ", {an}[({civ}) - 1]);')
                                    out.append(" " * (indent + 3) + "__first = 0;")
                                out.append(" " * (indent + 3) + 'printf("\\n");')
                                out.append(" " * indent + "}")
                                continue
                            m_idx_arr = re.match(r"^([a-z_]\w*)$", inner, re.IGNORECASE)
                            if m_idx_arr:
                                idxn = m_idx_arr.group(1).lower()
                                ivv = vars_map.get(idxn)
                                if ivv is not None and ivv.is_array:
                                    if ivv.is_allocatable:
                                        nidx = _dim_product_from_parts(
                                            _resolved_dim_parts(ivv, idxn, assumed_extents),
                                            vars_map,
                                            real_type,
                                            byref_scalars,
                                            assumed_extents,
                                        )
                                    else:
                                        nidx = _dim_product_expr(ivv.dim or "1", vars_map, real_type, byref_scalars, assumed_extents)
                                    out.append(" " * indent + f"for (int i_wr = 0; i_wr < {nidx}; ++i_wr) {{")
                                    out.append(" " * (indent + 3) + f"int __iv = {idxn}[i_wr];")
                                    out.append(" " * (indent + 3) + f'printf("{efmt}%s", {an}[__iv - 1], (i_wr + 1 < {nidx}) ? " " : "\\n");')
                                    out.append(" " * indent + "}")
                                    continue
                    m_sec = re.match(r"^([a-z_]\w*)\s*\((.+)\)\s*$", items[0], re.IGNORECASE)
                    if m_sec:
                        an = m_sec.group(1).lower()
                        av = vars_map.get(an)
                        if av is not None and av.is_array:
                            idx_parts = _split_args_top_level(m_sec.group(2))
                            dparts = _resolved_dim_parts(av, an, assumed_extents)
                            rank = len(dparts)
                            if rank in {2, 3} and len(idx_parts) == rank:
                                cty = (av.ctype or real_type).lower()
                                efmt = "%d" if cty == "int" else "%g"
                                dimc = [
                                    _convert_expr(dparts[k], vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                    for k in range(rank)
                                ]

                                def _parse_dim_spec_w(txt: str, dflt_hi: str) -> dict:
                                    t = txt.strip()
                                    m_ctor = re.match(r"^\[\s*(.*)\s*\]$", t)
                                    if m_ctor:
                                        vals = [x.strip() for x in _split_args_top_level(m_ctor.group(1)) if x.strip()]
                                        cvals = [
                                            _convert_expr(vv, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                            for vv in vals
                                        ]
                                        return {"kind": "vec", "vals": cvals}
                                    if ":" in t:
                                        sp = _split_colon_top_level(t)
                                        lo = _convert_expr((sp[0] or "1").strip(), vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                        hi = _convert_expr((sp[1] or dflt_hi).strip(), vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                        st = _convert_expr((sp[2] if len(sp) == 3 else "1").strip(), vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                        return {"kind": "sec", "lo": lo, "hi": hi, "st": st}
                                    val = _convert_expr(t, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                    return {"kind": "scalar", "val": val}

                                specs = [_parse_dim_spec_w(idx_parts[k], dimc[k]) for k in range(rank)]
                                if any(sp["kind"] != "scalar" for sp in specs):
                                    out.append(" " * indent + "{")
                                    out.append(" " * (indent + 3) + "int __first = 1;")
                                    idx_map: Dict[int, str] = {}

                                    def _lin_expr_w() -> str:
                                        i1 = idx_map.get(1, "1")
                                        lin = f"(({i1}) - 1)"
                                        if rank >= 2:
                                            i2 = idx_map.get(2, "1")
                                            lin = f"{lin} + ({dimc[0]}) * (({i2}) - 1)"
                                        if rank >= 3:
                                            i3 = idx_map.get(3, "1")
                                            lin = f"{lin} + ({dimc[0]}) * ({dimc[1]}) * (({i3}) - 1)"
                                        return lin

                                    def _emit_dim_w(dim_k: int, ind: int) -> None:
                                        if dim_k < 1:
                                            lin = _lin_expr_w()
                                            out.append(" " * ind + f'printf("%s{efmt}", __first ? "" : " ", {an}[{lin}]);')
                                            out.append(" " * ind + "__first = 0;")
                                            return
                                        sp = specs[dim_k - 1]
                                        if sp["kind"] == "scalar":
                                            idx_map[dim_k] = sp["val"]
                                            _emit_dim_w(dim_k - 1, ind)
                                            return
                                        if sp["kind"] == "vec":
                                            for vv in sp["vals"]:
                                                idx_map[dim_k] = vv
                                                _emit_dim_w(dim_k - 1, ind)
                                            return
                                        vnm = f"__i{dim_k}"
                                        idx_map[dim_k] = vnm
                                        out.append(" " * ind + f"for (int {vnm} = {sp['lo']}; ({sp['st']}) > 0 ? {vnm} <= {sp['hi']} : {vnm} >= {sp['hi']}; {vnm} += {sp['st']}) {{")
                                        _emit_dim_w(dim_k - 1, ind + 3)
                                        out.append(" " * ind + "}")

                                    _emit_dim_w(rank, indent + 3)
                                    out.append(" " * (indent + 3) + 'printf("\\n");')
                                    out.append(" " * indent + "}")
                                    continue
                            if len(idx_parts) == 2 and len(dparts) >= 2:
                                d1 = _convert_expr(dparts[0], vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                d2 = _convert_expr(dparts[1], vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                cty = (av.ctype or real_type).lower()
                                efmt = "%d" if cty == "int" else "%g"
                                sec0 = ":" in idx_parts[0]
                                sec1 = ":" in idx_parts[1]
                                if sec0 or sec1:
                                    if sec0:
                                        sp0 = _split_colon_top_level(idx_parts[0].strip())
                                        if len(sp0) not in {2, 3}:
                                            sp0 = [idx_parts[0].strip(), idx_parts[0].strip(), "1"]
                                    else:
                                        sp0 = [idx_parts[0].strip(), idx_parts[0].strip(), "1"]
                                    if sec1:
                                        sp1 = _split_colon_top_level(idx_parts[1].strip())
                                        if len(sp1) not in {2, 3}:
                                            sp1 = [idx_parts[1].strip(), idx_parts[1].strip(), "1"]
                                    else:
                                        sp1 = [idx_parts[1].strip(), idx_parts[1].strip(), "1"]
                                    i1_lo = _convert_expr((sp0[0] or "1").strip(), vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                    i1_hi = _convert_expr((sp0[1] or d1).strip(), vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                    i1_st = _convert_expr((sp0[2] if len(sp0) == 3 else "1").strip(), vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                    i2_lo = _convert_expr((sp1[0] or "1").strip(), vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                    i2_hi = _convert_expr((sp1[1] or d2).strip(), vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                    i2_st = _convert_expr((sp1[2] if len(sp1) == 3 else "1").strip(), vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                    out.append(" " * indent + "{")
                                    out.append(" " * (indent + 3) + "int __first = 1;")
                                    out.append(" " * (indent + 3) + f"for (int j_wr = {i2_lo}; ({i2_st}) > 0 ? j_wr <= {i2_hi} : j_wr >= {i2_hi}; j_wr += {i2_st}) {{")
                                    out.append(" " * (indent + 6) + f"for (int i_wr = {i1_lo}; ({i1_st}) > 0 ? i_wr <= {i1_hi} : i_wr >= {i1_hi}; i_wr += {i1_st}) {{")
                                    out.append(" " * (indent + 9) + f"int __lin = (i_wr - 1) + ({d1}) * (j_wr - 1);")
                                    out.append(" " * (indent + 9) + f'printf("%s{efmt}", __first ? "" : " ", {an}[__lin]);')
                                    out.append(" " * (indent + 9) + "__first = 0;")
                                    out.append(" " * (indent + 6) + "}")
                                    out.append(" " * (indent + 3) + "}")
                                    out.append(" " * (indent + 3) + 'printf("\\n");')
                                    out.append(" " * indent + "}")
                                    continue
                    m_arr = re.match(r"^([a-z_]\w*)$", items[0], re.IGNORECASE)
                    if m_arr:
                        an = m_arr.group(1).lower()
                        av = vars_map.get(an)
                        if av is not None and av.is_array:
                            if av.is_allocatable:
                                npr = _dim_product_from_parts(
                                    _resolved_dim_parts(av, an, assumed_extents),
                                    vars_map,
                                    real_type,
                                    byref_scalars,
                                    assumed_extents,
                                )
                            else:
                                npr = _dim_product_expr(av.dim or "1", vars_map, real_type, byref_scalars, assumed_extents)
                            cty = (av.ctype or real_type).lower()
                            efmt = "%d" if cty == "int" else "%g"
                            out.append(" " * indent + f"for (int i_wr = 0; i_wr < {npr}; ++i_wr) {{")
                            out.append(" " * (indent + 3) + f'printf("{efmt}%s", {an}[i_wr], (i_wr + 1 < {npr}) ? " " : "\\n");')
                            out.append(" " * indent + "}")
                            continue
                    expr0 = items[0]
                    toks0 = {t.lower() for t in re.findall(r"\b[a-z_]\w*\b", _strip_comment(expr0), flags=re.IGNORECASE)}
                    arrs0 = [t for t in sorted(toks0) if t in vars_map and vars_map[t].is_array]
                    if arrs0 and not any(re.search(rf"\b{re.escape(a)}\s*\(", expr0, flags=re.IGNORECASE) for a in arrs0):
                        base = vars_map.get(arrs0[0])
                        compatible = base is not None and all((vars_map.get(a) is not None and vars_map.get(a).dim == base.dim) for a in arrs0)
                        if compatible and base is not None:
                            if base.is_allocatable and len(_dim_parts(base.dim)) == 1:
                                npr = _alloc_len_name(arrs0[0])
                            else:
                                npr = _dim_product_expr(base.dim or "1", vars_map, real_type, byref_scalars, assumed_extents)
                            expr_i = expr0
                            for a in sorted(arrs0, key=len, reverse=True):
                                expr_i = re.sub(rf"\b{re.escape(a)}\b", f"{a}[i_wr]", expr_i, flags=re.IGNORECASE)
                            cit = _convert_expr(expr_i, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                            out.append(" " * indent + f"for (int i_wr = 0; i_wr < {npr}; ++i_wr) {{")
                            out.append(" " * (indent + 3) + f'printf("%g%s", {cit}, (i_wr + 1 < {npr}) ? " " : "\\n");')
                            out.append(" " * indent + "}")
                            continue
                fmts: List[str] = []
                cargs: List[str] = []
                for it in items:
                    if (it.startswith('"') and it.endswith('"')) or (it.startswith("'") and it.endswith("'")):
                        content = it[1:-1].replace("\\", "\\\\").replace('"', '\\"')
                        cargs.append(f'"{content}"')
                        fmts.append("%s")
                        continue
                    cit = _convert_expr(it, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                    cargs.append(cit)
                    mvar = re.match(r"^([a-z_]\w*)$", it, re.IGNORECASE)
                    if mvar:
                        cty = vars_map.get(mvar.group(1).lower(), Var("int")).ctype.lower()
                        fmts.append("%g" if cty != "int" else "%d")
                    elif re.match(r"^[+\-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+\-]?\d+)?$", cit):
                        fmts.append("%g")
                    else:
                        fmts.append("%g")
                out.append(" " * indent + f'printf("{" ".join(fmts)}\\n", {", ".join(cargs)});')
                continue
            out.append(" " * indent + f"/* unsupported: {code} */")
            continue

        m_asn = re.match(r"^(.+?)\s*=\s*(.+)$", code, re.IGNORECASE)
        if m_asn:
            lhs_raw = m_asn.group(1).strip()
            rhs_raw = m_asn.group(2).strip()
            if use_implicit_result and lhs_raw.lower() == unit_name_l:
                lhs_raw = implicit_result_name
            m_lhs_whole_ctor = re.match(r"^([a-z_]\w*)$", lhs_raw, re.IGNORECASE)
            m_rhs_ctor = re.match(r"^\[\s*(.*)\s*\]\s*$", rhs_raw)
            if m_lhs_whole_ctor and m_rhs_ctor:
                lhs_nm_ctor = m_lhs_whole_ctor.group(1).lower()
                lv_ctor = vars_map.get(lhs_nm_ctor)
                if lv_ctor is not None and lv_ctor.is_array:
                    ctor_inner = m_rhs_ctor.group(1).strip()
                    ctor_items = _split_args_top_level(ctor_inner) if ctor_inner else []
                    n_ctor = len([x for x in ctor_items if x.strip()])
                    if lv_ctor.is_allocatable:
                        out.append(" " * indent + f"if ({lhs_nm_ctor}) free({lhs_nm_ctor});")
                        out.append(" " * indent + f"{lhs_nm_ctor} = ({lv_ctor.ctype}*) malloc(sizeof({lv_ctor.ctype}) * {n_ctor});")
                        out.append(" " * indent + f"if (!{lhs_nm_ctor} && {n_ctor} > 0) return 1;")
                        if len(_dim_parts(lv_ctor.dim)) == 1:
                            out.append(" " * indent + f"{_alloc_len_name(lhs_nm_ctor)} = {n_ctor};")
                    for k, it in enumerate([x.strip() for x in ctor_items if x.strip()]):
                        cv = _convert_expr(it, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                        out.append(" " * indent + f"{lhs_nm_ctor}[{k}] = {cv};")
                    continue
            # x = reshape([..], [..]) for allocatable arrays.
            m_lhs_whole = re.match(r"^([a-z_]\w*)$", lhs_raw, re.IGNORECASE)
            m_rhs_reshape = re.match(r"^reshape\s*\((.*)\)\s*$", rhs_raw, re.IGNORECASE)
            if m_lhs_whole and m_rhs_reshape:
                lhs_nm_r = m_lhs_whole.group(1).lower()
                lv_r = vars_map.get(lhs_nm_r)
                if lv_r is not None and lv_r.is_array and lv_r.is_allocatable:
                    rargs = _split_args_top_level(m_rhs_reshape.group(1))
                    if len(rargs) >= 2:
                        src_raw = rargs[0].strip()
                        shp_raw = rargs[1].strip()
                        pad_raw = ""
                        if len(rargs) >= 3:
                            third = rargs[2].strip()
                            m_pad_kw = re.match(r"(?i)^pad\s*=\s*(.+)$", third)
                            pad_raw = (m_pad_kw.group(1) if m_pad_kw else third).strip()
                        m_src_ctor = re.match(r"^\[\s*(.*)\s*\]$", src_raw)
                        m_shp_ctor = re.match(r"^\[\s*(.*)\s*\]$", shp_raw)
                        if m_src_ctor and m_shp_ctor:
                            src_items = [x.strip() for x in _split_args_top_level(m_src_ctor.group(1)) if x.strip()]
                            shp_items = [x.strip() for x in _split_args_top_level(m_shp_ctor.group(1)) if x.strip()]
                            pad_items: List[str] = []
                            if pad_raw:
                                m_pad_ctor = re.match(r"^\[\s*(.*)\s*\]$", pad_raw)
                                if m_pad_ctor:
                                    pad_items = [x.strip() for x in _split_args_top_level(m_pad_ctor.group(1)) if x.strip()]
                                else:
                                    pad_items = [pad_raw]
                            rank_lhs = max(1, len(_dim_parts(lv_r.dim)))
                            if len(shp_items) == rank_lhs:
                                shp_c = [_convert_expr(s, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names) for s in shp_items]
                                n_total = _dim_product_from_parts(shp_c, vars_map, real_type, byref_scalars, assumed_extents)
                                exts = _alloc_extent_names(lhs_nm_r, rank_lhs)
                                out.append(" " * indent + f"if ({lhs_nm_r}) free({lhs_nm_r});")
                                out.append(" " * indent + f"{lhs_nm_r} = ({lv_r.ctype}*) malloc(sizeof({lv_r.ctype}) * {n_total});")
                                out.append(" " * indent + f"if (!{lhs_nm_r} && ({n_total}) > 0) return 1;")
                                for k, en in enumerate(exts):
                                    val = shp_c[k] if k < len(shp_c) else "1"
                                    out.append(" " * indent + f"{en} = {val};")
                                src_n = len(src_items)
                                if src_n > 0:
                                    for k in range(src_n):
                                        cv = _convert_expr(src_items[k], vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                        out.append(" " * indent + f"if ({k} < ({n_total})) {lhs_nm_r}[{k}] = {cv};")
                                    pad_n = len(pad_items)
                                    if pad_n > 0:
                                        for pk, pit in enumerate(pad_items):
                                            cp = _convert_expr(pit, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                            out.append(" " * indent + f"{lv_r.ctype} __pad_{pk} = {cp};")
                                        if pad_n == 1:
                                            out.append(" " * indent + f"for (int i_fill = {src_n}; i_fill < ({n_total}); ++i_fill) {lhs_nm_r}[i_fill] = __pad_0;")
                                        else:
                                            out.append(" " * indent + f"for (int i_fill = {src_n}; i_fill < ({n_total}); ++i_fill) {{")
                                            out.append(" " * (indent + 3) + f"switch ((i_fill - {src_n}) % {pad_n}) {{")
                                            for pk in range(pad_n):
                                                out.append(" " * (indent + 3) + f"case {pk}: {lhs_nm_r}[i_fill] = __pad_{pk}; break;")
                                            out.append(" " * (indent + 3) + "default: break;")
                                            out.append(" " * (indent + 3) + "}")
                                            out.append(" " * indent + "}")
                                    elif src_n < 64:
                                        # No PAD supplied: cycle source values.
                                        out.append(" " * indent + f"for (int i_fill = {src_n}; i_fill < ({n_total}); ++i_fill) {{")
                                        out.append(" " * (indent + 3) + f"{lhs_nm_r}[i_fill] = {lhs_nm_r}[i_fill % {src_n}];")
                                        out.append(" " * indent + "}")
                                    else:
                                        out.append(" " * indent + f"for (int i_fill = {src_n}; i_fill < ({n_total}); ++i_fill) {lhs_nm_r}[i_fill] = {lhs_nm_r}[i_fill % {src_n}];")
                                continue
            # Whole allocatable 1D assignment from section expression:
            # x = f(a(l:u:s), ...)
            m_lhs_whole_sec_expr = re.match(r"^([a-z_]\w*)$", lhs_raw, re.IGNORECASE)
            if m_lhs_whole_sec_expr:
                lhs_nm = m_lhs_whole_sec_expr.group(1).lower()
                lv0 = vars_map.get(lhs_nm)
                if lv0 is not None and lv0.is_array and lv0.is_allocatable and len(_dim_parts(lv0.dim)) == 1:
                    def _rewrite_section_expr(expr: str) -> tuple[str, List[tuple[str, str, str, str]]]:
                        out_chars: List[str] = []
                        sections: List[tuple[str, str, str, str]] = []
                        i = 0
                        n = len(expr)
                        while i < n:
                            ch = expr[i]
                            if ch.isalpha() or ch == "_":
                                j = i + 1
                                while j < n and (expr[j].isalnum() or expr[j] == "_"):
                                    j += 1
                                name = expr[i:j]
                                k = j
                                while k < n and expr[k].isspace():
                                    k += 1
                                if k < n and expr[k] == "(":
                                    depth = 0
                                    in_s = False
                                    in_d = False
                                    p = k
                                    endp = -1
                                    while p < n:
                                        c = expr[p]
                                        if c == "'" and not in_d:
                                            if in_s and p + 1 < n and expr[p + 1] == "'":
                                                p += 2
                                                continue
                                            in_s = not in_s
                                            p += 1
                                            continue
                                        if c == '"' and not in_s:
                                            in_d = not in_d
                                            p += 1
                                            continue
                                        if not in_s and not in_d:
                                            if c == "(":
                                                depth += 1
                                            elif c == ")":
                                                depth -= 1
                                                if depth == 0:
                                                    endp = p
                                                    break
                                        p += 1
                                    if endp != -1:
                                        inside = expr[k + 1 : endp]
                                        parts = _split_colon_top_level(inside)
                                        if len(parts) in {2, 3}:
                                            arr = name.lower()
                                            vv = vars_map.get(arr)
                                            if vv is not None and vv.is_array and len(_dim_parts(vv.dim)) == 1:
                                                d0 = _dim_parts(vv.dim)[0] if _dim_parts(vv.dim) else "1"
                                                lo_raw = (parts[0] or "").strip() or "1"
                                                hi_raw = (parts[1] or "").strip()
                                                st_raw = (parts[2] if len(parts) == 3 else "").strip() or "1"
                                                lo = _convert_expr(lo_raw, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                                if hi_raw:
                                                    hi = _convert_expr(hi_raw, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                                else:
                                                    if vv.is_allocatable:
                                                        hi = _alloc_len_name(arr)
                                                    else:
                                                        hi = _convert_expr(d0, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                                st = _convert_expr(st_raw, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                                idx = f"(({lo}) + (p_fill * ({st})))"
                                                out_chars.append(f"{arr}[{idx} - 1]")
                                                sections.append((arr, lo, hi, st))
                                                i = endp + 1
                                                continue
                                out_chars.append(ch)
                                i += 1
                                continue
                            out_chars.append(ch)
                            i += 1
                        return "".join(out_chars), sections

                    rhs_expr_raw, sec_infos = _rewrite_section_expr(rhs_raw)
                    if sec_infos:
                        rhs_tokens = {t.lower() for t in re.findall(r"\b[a-z_]\w*\b", _strip_comment(rhs_raw), flags=re.IGNORECASE)}
                        unresolved_whole = False
                        for an in sorted(rhs_tokens):
                            vv = vars_map.get(an)
                            if vv is None or (not vv.is_array):
                                continue
                            if re.search(rf"\b{re.escape(an)}\s*\(", rhs_raw, flags=re.IGNORECASE):
                                continue
                            unresolved_whole = True
                            break
                        if not unresolved_whole:
                            arr0, lo0, hi0, st0 = sec_infos[0]
                            vv0 = vars_map.get(arr0)
                            if vv0 is not None and vv0.is_array and len(_dim_parts(vv0.dim)) == 1:
                                rhs = _convert_expr(rhs_expr_raw, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                                nname = _alloc_len_name(lhs_nm)
                                self_ref = lhs_nm in rhs_tokens
                                out.append(" " * indent + "{")
                                out.append(" " * (indent + 3) + "int __n_tmp = 0;")
                                out.append(" " * (indent + 3) + f"if ({st0} > 0) {{")
                                out.append(" " * (indent + 6) + f"for (int i_fill = {lo0}; i_fill <= {hi0}; i_fill += {st0}) ++__n_tmp;")
                                out.append(" " * (indent + 3) + "} else {")
                                out.append(" " * (indent + 6) + f"for (int i_fill = {lo0}; i_fill >= {hi0}; i_fill += {st0}) ++__n_tmp;")
                                out.append(" " * (indent + 3) + "}")
                                if self_ref:
                                    out.append(" " * (indent + 3) + f"{lv0.ctype} *__rhs_tmp = ({lv0.ctype}*) malloc(sizeof({lv0.ctype}) * __n_tmp);")
                                    out.append(" " * (indent + 3) + "if (!__rhs_tmp && __n_tmp > 0) return 1;")
                                    out.append(" " * (indent + 3) + "for (int p_fill = 0; p_fill < __n_tmp; ++p_fill) {")
                                    out.append(" " * (indent + 6) + f"__rhs_tmp[p_fill] = {rhs};")
                                    out.append(" " * (indent + 3) + "}")
                                    out.append(" " * (indent + 3) + f"if ({lhs_nm}) free({lhs_nm});")
                                    out.append(" " * (indent + 3) + f"{lhs_nm} = __rhs_tmp;")
                                    out.append(" " * (indent + 3) + f"{nname} = __n_tmp;")
                                else:
                                    out.append(" " * (indent + 3) + f"if ({lhs_nm}) free({lhs_nm});")
                                    out.append(" " * (indent + 3) + f"{lhs_nm} = ({lv0.ctype}*) malloc(sizeof({lv0.ctype}) * __n_tmp);")
                                    out.append(" " * (indent + 3) + f"if (!{lhs_nm} && __n_tmp > 0) return 1;")
                                    out.append(" " * (indent + 3) + f"{nname} = __n_tmp;")
                                    out.append(" " * (indent + 3) + "for (int p_fill = 0; p_fill < __n_tmp; ++p_fill) {")
                                    out.append(" " * (indent + 6) + f"{lhs_nm}[p_fill] = {rhs};")
                                    out.append(" " * (indent + 3) + "}")
                                out.append(" " * indent + "}")
                                continue
            m_lhs_sec = re.match(
                r"^([a-z_]\w*)\s*\(\s*([^:,\)]*)\s*:\s*([^:,\)]*)(?:\s*:\s*([^)]+))?\s*\)$",
                lhs_raw,
                re.IGNORECASE,
            )
            if m_lhs_sec:
                lhs_nm_sec = m_lhs_sec.group(1).lower()
                lv_sec = vars_map.get(lhs_nm_sec)
                if lv_sec is not None and lv_sec.is_array and len(_dim_parts(lv_sec.dim)) == 1:
                    d0 = _dim_parts(lv_sec.dim)[0] if _dim_parts(lv_sec.dim) else "1"
                    lo_raw = (m_lhs_sec.group(2) or "").strip() or "1"
                    hi_raw = (m_lhs_sec.group(3) or "").strip() or d0
                    st_raw = (m_lhs_sec.group(4) or "").strip() or "1"
                    lo = _convert_expr(lo_raw, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                    hi = _convert_expr(hi_raw, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                    st = _convert_expr(st_raw, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                    rhs_scan = _strip_comment(rhs_raw)
                    rhs_tokens = {t.lower() for t in re.findall(r"\b[a-z_]\w*\b", rhs_scan, flags=re.IGNORECASE)}

                    rhs_expr_raw = rhs_raw
                    # Map RHS 1D sections to element access driven by section position p_fill.
                    sec_pat = re.compile(
                        r"\b([a-z_]\w*)\s*\(\s*([^():,\)]*)\s*:\s*([^():,\)]*)(?:\s*:\s*([^():,\)]+))?\s*\)",
                        re.IGNORECASE,
                    )

                    def _rhs_sec_repl(mm: re.Match[str]) -> str:
                        arr = mm.group(1).lower()
                        vv = vars_map.get(arr)
                        if vv is None or (not vv.is_array) or len(_dim_parts(vv.dim)) != 1:
                            return mm.group(0)
                        rdim0 = _dim_parts(vv.dim)[0] if _dim_parts(vv.dim) else "1"
                        rlo_raw = (mm.group(2) or "").strip() or "1"
                        _rhi_raw = (mm.group(3) or "").strip() or rdim0
                        rst_raw = (mm.group(4) or "").strip() or "1"
                        rlo = _convert_expr(rlo_raw, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                        rst = _convert_expr(rst_raw, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                        idx = f"(({rlo}) + (p_fill * ({rst})))"
                        return f"{arr}[{idx} - 1]"

                    rhs_expr_raw = sec_pat.sub(_rhs_sec_repl, rhs_expr_raw)
                    # Reject unresolved whole-array operands for section assignment.
                    unresolved_whole_array = False
                    for an in sorted(rhs_tokens):
                        vv = vars_map.get(an)
                        if vv is None or (not vv.is_array):
                            continue
                        if re.search(rf"\b{re.escape(an)}\s*\(", rhs_raw, flags=re.IGNORECASE):
                            continue
                        unresolved_whole_array = True
                        break
                    if not unresolved_whole_array:
                        rhs = _convert_expr(rhs_expr_raw, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
                        out.append(" " * indent + f"if ({st} > 0) {{")
                        out.append(" " * (indent + 3) + f"for (int i_fill = {lo}, p_fill = 0; i_fill <= {hi}; i_fill += {st}, ++p_fill) {{")
                        out.append(" " * (indent + 6) + f"{lhs_nm_sec}[i_fill - 1] = {rhs};")
                        out.append(" " * (indent + 3) + "}")
                        out.append(" " * indent + "} else {")
                        out.append(" " * (indent + 3) + f"for (int i_fill = {lo}, p_fill = 0; i_fill >= {hi}; i_fill += {st}, ++p_fill) {{")
                        out.append(" " * (indent + 6) + f"{lhs_nm_sec}[i_fill - 1] = {rhs};")
                        out.append(" " * (indent + 3) + "}")
                        out.append(" " * indent + "}")
                        continue
            m_lhs_whole_arr = re.match(r"^([a-z_]\w*)$", lhs_raw, re.IGNORECASE)
            if m_lhs_whole_arr:
                lhs_nm_arr = m_lhs_whole_arr.group(1).lower()
                lv_arr = vars_map.get(lhs_nm_arr)
                if lv_arr is not None and lv_arr.is_array:
                    # Whole-array assignment lowering.
                    rhs_scan = _strip_comment(rhs_raw)
                    rhs_tokens = {t.lower() for t in re.findall(r"\b[a-z_]\w*\b", rhs_scan, flags=re.IGNORECASE)}
                    rhs_array_names = [tok for tok in sorted(rhs_tokens) if tok in vars_map and vars_map[tok].is_array]
                    rhs_uses_array = len(rhs_array_names) > 0
                    if not rhs_uses_array:
                        # Scalar-to-whole-array assignment.
                        rhs = _convert_expr(
                            rhs_raw,
                            vars_map,
                            real_type,
                            byref_scalars,
                            assumed_extents,
                            proc_arg_extent_names,
                        )
                        if lv_arr.is_allocatable and len(_dim_parts(lv_arr.dim)) == 1:
                            nfill = _alloc_len_name(lhs_nm_arr)
                        else:
                            nfill = _dim_product_expr(
                                lv_arr.dim or "1",
                                vars_map,
                                real_type,
                                byref_scalars,
                                assumed_extents,
                            )
                        out.append(" " * indent + f"for (int i_fill = 0; i_fill < {nfill}; ++i_fill) {{")
                        out.append(" " * (indent + 3) + f"{lhs_nm_arr}[i_fill] = {rhs};")
                        out.append(" " * indent + "}")
                        continue
                    # Elementwise array-expression assignment (e.g. y = 2*x, y = x + z).
                    # Conservative: skip if array operands are explicitly subscripted in RHS.
                    explicit_subscript = any(
                        re.search(rf"\b{re.escape(an)}\s*\(", rhs_raw, flags=re.IGNORECASE)
                        for an in rhs_array_names
                    )
                    lhs_shape = tuple(p.replace(" ", "").lower() for p in _dim_parts(lv_arr.dim))
                    same_shape = all(
                        (
                            vars_map.get(an) is not None
                            and tuple(p.replace(" ", "").lower() for p in _dim_parts(vars_map.get(an).dim)) == lhs_shape
                        )
                        for an in rhs_array_names
                    )
                    if (not explicit_subscript) and same_shape:
                        rhs_elem = rhs_raw
                        # Replace array variables with element access, longest first.
                        for an in sorted(rhs_array_names, key=len, reverse=True):
                            rhs_elem = re.sub(
                                rf"\b{re.escape(an)}\b",
                                f"{an}[i_fill]",
                                rhs_elem,
                                flags=re.IGNORECASE,
                            )
                        rhs = _convert_expr(
                            rhs_elem,
                            vars_map,
                            real_type,
                            byref_scalars,
                            assumed_extents,
                            proc_arg_extent_names,
                        )
                        if lv_arr.is_allocatable and len(_dim_parts(lv_arr.dim)) == 1:
                            nfill = _alloc_len_name(lhs_nm_arr)
                        else:
                            nfill = _dim_product_expr(
                                lv_arr.dim or "1",
                                vars_map,
                                real_type,
                                byref_scalars,
                                assumed_extents,
                            )
                        out.append(" " * indent + f"for (int i_fill = 0; i_fill < {nfill}; ++i_fill) {{")
                        out.append(" " * (indent + 3) + f"{lhs_nm_arr}[i_fill] = {rhs};")
                        out.append(" " * indent + "}")
                        continue
            m_lhs_name = re.match(r"^([a-z_]\w*)$", lhs_raw, re.IGNORECASE)
            m_rhs_call = re.match(r"^([a-z_]\w*)\s*\((.*)\)\s*$", rhs_raw, re.IGNORECASE)
            if m_lhs_name and m_rhs_call:
                lhs_nm = m_lhs_name.group(1).lower()
                callee = m_rhs_call.group(1).lower()
                lv = vars_map.get(lhs_nm)
                if lv is not None and lv.is_array and callee in array_result_funcs:
                    raw_args = _split_args_top_level(m_rhs_call.group(2).strip()) if m_rhs_call.group(2).strip() else []
                    cargs: List[str] = []
                    extent_lists = proc_arg_extent_names.get(callee, [])
                    for k, a in enumerate(raw_args):
                        exts = extent_lists[k] if k < len(extent_lists) else []
                        if exts:
                            m_id = re.match(r"^\s*([a-z_]\w*)\s*$", a, re.IGNORECASE)
                            if m_id:
                                nm = m_id.group(1).lower()
                                vv = vars_map.get(nm)
                                if vv is not None and vv.is_array:
                                    dps = _dim_parts(vv.dim)
                                    if len(dps) >= len(exts):
                                        for d in dps[: len(exts)]:
                                            cargs.append(_convert_expr(d, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names))
                                    else:
                                        cargs.extend(["1"] * len(exts))
                                else:
                                    cargs.extend(["1"] * len(exts))
                            else:
                                cargs.extend(["1"] * len(exts))
                        cargs.append(_convert_expr(a, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names))
                    tmp = f"__tmp_{callee}"
                    dim = _dim_product_expr(lv.dim or "1", vars_map, real_type, byref_scalars)
                    out.append(
                        " " * indent + f"{lv.ctype} *{tmp} = {callee}({', '.join(cargs)});"
                    )
                    out.append(" " * indent + f"for (int i_copy = 0; i_copy < {dim}; ++i_copy) {{")
                    out.append(" " * (indent + 3) + f"{lhs_nm}[i_copy] = {tmp}[i_copy];")
                    out.append(" " * indent + "}")
                    out.append(" " * indent + f"free({tmp});")
                    continue
            lhs = _convert_expr(lhs_raw, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
            m_rhs_any_call = re.match(r"^\s*([a-z_]\w*)\s*\((.*)\)\s*$", rhs_raw, re.IGNORECASE)
            if m_rhs_any_call and any(proc_arg_optional.get(m_rhs_any_call.group(1).lower(), [])):
                args_rhs = _split_args_top_level(m_rhs_any_call.group(2).strip()) if m_rhs_any_call.group(2).strip() else []
                rhs = _convert_optional_call_expr(m_rhs_any_call.group(1), args_rhs)
            else:
                rhs = _convert_expr(rhs_raw, vars_map, real_type, byref_scalars, assumed_extents, proc_arg_extent_names)
            out.append(" " * indent + f"{lhs} = {rhs};")
            continue

        out.append(" " * indent + f"/* unsupported: {code} */")

    if unit["kind"] == "function":
        if unit.get("result"):
            out.append(" " * indent + f"return {unit['result']};")
        else:
            out.append(" " * indent + f"return {implicit_result_name};")
        out.append("}")
    elif unit["kind"] == "subroutine":
        out.append("}")
    else:
        for nm, v in vars_map.items():
            if v.is_array:
                out.append(" " * indent + f"free({nm});")
        has_terminal_return = False
        for j in range(len(out) - 1, -1, -1):
            s = out[j].strip()
            if not s or s.startswith("/*"):
                continue
            has_terminal_return = s.startswith("return ")
            break
        if not has_terminal_return:
            out.append(" " * indent + "return 0;")
        out.append("}")
    out = _rewrite_zero_based_loop_style(out)
    out = _use_block_scoped_loop_indices(out)
    out = _inline_simple_int_aliases(out)
    out = _prefer_simple_n_extent_name(out, assumed_extents)
    out = _fold_zero_init_to_decl(out, real_type)
    out = _coalesce_adjacent_c_declarations(out)
    out = _compound_assign_style(out)
    if one_line:
        out = _collapse_one_line_blocks(out)
    out.append("")
    return out


def transpile_fortran_to_c(
    text: str, *, one_line: bool = False, validate: bool = True, annotate: bool = False
) -> str:
    if validate:
        basic_errors = fscan.validate_fortran_basic_statements(text)
        if basic_errors:
            msg = "\n".join(basic_errors[:20])
            if len(basic_errors) > 20:
                msg += f"\n... and {len(basic_errors)-20} more"
            raise ValueError(msg)

    real_type = _fortran_to_c_real_type(text)
    kind_ctype_map = _extract_kind_alias_c_types(text)
    units = fscan.split_fortran_units_simple(text)
    known_proc_names = {str(u.get("name", "")).lower() for u in units}
    if validate:
        errors = fscan.find_implicit_none_undeclared_identifiers(
            text, known_procedure_names=known_proc_names
        )
        if errors:
            msg = "\n".join(errors[:20])
            if len(errors) > 20:
                msg += f"\n... and {len(errors)-20} more"
            raise ValueError(f"Implicit-none validation failed:\n{msg}")

    out: List[str] = [
        "#include <stdio.h>",
        "#include <stdlib.h>",
        "#include <math.h>",
        "#include <float.h>",
        "#include <string.h>",
        "",
    ]
    decl_maps: List[Dict[str, Var]] = [_parse_decls(u, real_type, kind_ctype_map) for u in units]
    # Infer simple dummy-argument traits from usage:
    # - mutated scalar dummy -> inout
    # - scalar dummy used as callee (f(...)) -> external procedure argument
    for u, vmap in zip(units, decl_maps):
        if u.get("kind") not in {"function", "subroutine"}:
            continue
        args_l = [str(a).lower() for a in u.get("args", [])]
        body = [str(s) for s in u.get("body_lines", [])]
        for a in args_l:
            vv = vmap.get(a)
            if vv is None:
                continue
            if vv.intent is None:
                asn_pat = re.compile(rf"^\s*{re.escape(a)}(?:\s*\([^)]*\))?\s*=", re.IGNORECASE)
                mut_pat = re.compile(rf"^\s*{re.escape(a)}\s*[\+\-\*/]=", re.IGNORECASE)
                for s in body:
                    code = _strip_comment(s).strip()
                    if asn_pat.match(code) or mut_pat.match(code):
                        vv.intent = "inout"
                        break
            if (not vv.is_array) and (not vv.is_external):
                callee_pat = re.compile(rf"\b{re.escape(a)}\s*\(", re.IGNORECASE)
                for s in body:
                    code = _strip_comment(s).strip()
                    if not callee_pat.search(code):
                        continue
                    # Skip plain assignment to a(...)=... indexing forms.
                    if re.match(rf"^\s*{re.escape(a)}\s*\(", code, re.IGNORECASE) and "=" in code:
                        continue
                    vv.is_external = True
                    break
    known_proc_names = {str(u.get("name", "")).lower() for u in units if u.get("kind") in {"function", "subroutine"}}
    # Declarations like `real :: f` can denote external function type.
    # If such a name is used as `f(...)`, do not emit it as a local variable.
    for u, vmap in zip(units, decl_maps):
        called_names: set[str] = set()
        for stmt in u.get("body_lines", []):
            code = _strip_comment(str(stmt)).strip()
            for m in re.finditer(r"\b([a-z_]\w*)\s*\(", code, flags=re.IGNORECASE):
                called_names.add(m.group(1).lower())
        arg_names = {str(a).lower() for a in u.get("args", [])}
        unit_name = str(u.get("name", "")).lower()
        for nm in list(vmap.keys()):
            vv = vmap.get(nm)
            if vv is None:
                continue
            if nm in arg_names:
                continue
            # Preserve function result naming patterns in function units.
            if u.get("kind") == "function" and (nm == unit_name or nm == str(u.get("result", "")).lower()):
                continue
            if nm in known_proc_names and nm in called_names and (not vv.is_array) and (not vv.is_param):
                del vmap[nm]
    proc_arg_modes: Dict[str, List[str]] = {}
    proc_arg_optional: Dict[str, List[bool]] = {}
    proc_arg_ctypes: Dict[str, List[str]] = {}
    proc_arg_is_array: Dict[str, List[bool]] = {}
    proc_arg_assumed_ranks: Dict[str, List[int]] = {}
    proc_arg_extent_names: Dict[str, List[List[str]]] = {}
    array_result_funcs: set[str] = set()
    for u, vmap in zip(units, decl_maps):
        if u.get("kind") not in {"function", "subroutine"}:
            continue
        modes: List[str] = []
        optionals: List[bool] = []
        ctypes: List[str] = []
        is_arrays: List[bool] = []
        assumed_ranks: List[int] = []
        extent_names_per_arg: List[List[str]] = []
        arg_names_lower = [str(a).lower() for a in u.get("args", [])]
        assumed_rank1_count = 0
        for a in arg_names_lower:
            av0 = vmap.get(a, Var("int"))
            if av0.is_array and _is_assumed_shape(av0.dim) and max(1, len(_dim_parts(av0.dim))) == 1:
                assumed_rank1_count += 1
        for a in u.get("args", []):
            av = vmap.get(str(a).lower(), Var("int"))
            optionals.append(bool(av.optional))
            ctypes.append(av.ctype)
            is_arrays.append(bool(av.is_array))
            if av.is_array:
                modes.append("ptr")
                if _is_assumed_shape(av.dim):
                    rank = max(1, len(_dim_parts(av.dim)))
                    assumed_ranks.append(rank)
                    use_simple_n = (
                        rank == 1
                        and assumed_rank1_count == 1
                        and "n" not in arg_names_lower
                        and "n" not in vmap
                    )
                    extent_names_per_arg.append(
                        _extent_param_names(str(a).lower(), rank, use_simple_n=use_simple_n)
                    )
                else:
                    assumed_ranks.append(0)
                    extent_names_per_arg.append([])
            elif av.is_external:
                modes.append("value")
                assumed_ranks.append(0)
                extent_names_per_arg.append([])
            elif av.intent == "in":
                modes.append("ptr" if av.optional else "value")
                assumed_ranks.append(0)
                extent_names_per_arg.append([])
            elif av.intent in {"out", "inout"}:
                modes.append("ptr")
                assumed_ranks.append(0)
                extent_names_per_arg.append([])
            else:
                modes.append("ptr" if av.optional else "value")
                assumed_ranks.append(0)
                extent_names_per_arg.append([])
        proc_arg_modes[str(u.get("name", "")).lower()] = modes
        proc_arg_optional[str(u.get("name", "")).lower()] = optionals
        proc_arg_ctypes[str(u.get("name", "")).lower()] = ctypes
        proc_arg_is_array[str(u.get("name", "")).lower()] = is_arrays
        proc_arg_assumed_ranks[str(u.get("name", "")).lower()] = assumed_ranks
        proc_arg_extent_names[str(u.get("name", "")).lower()] = extent_names_per_arg
        if u.get("kind") == "function":
            ret_name = str(u.get("result") or "").lower()
            rv = vmap.get(ret_name)
            if rv is not None and rv.is_array:
                array_result_funcs.add(str(u.get("name", "")).lower())

    # Emit forward declarations so calls compile even when definitions appear later.
    for u, vmap in zip(units, decl_maps):
        if u.get("kind") == "function":
            ret_name = (u.get("result") or "").lower()
            ret_lookup = ret_name if ret_name else str(u.get("name", "")).lower()
            ret_var = vmap.get(ret_lookup, Var(real_type))
            args: List[str] = []
            proc_name = str(u.get("name", "")).lower()
            extent_lists = proc_arg_extent_names.get(proc_name, [])
            flat_exts = [e for lst in extent_lists for e in lst]
            proto_simple_n = (len(flat_exts) == 1 and flat_exts[0] != "n")
            for idx, a in enumerate(u.get("args", [])):
                av = vmap.get(str(a).lower(), Var("int"))
                exts = extent_lists[idx] if idx < len(extent_lists) else []
                if exts:
                    use_exts = (["n"] if (proto_simple_n and len(exts) == 1) else exts)
                    args.extend([f"const int {nm}" for nm in use_exts])
                args.append(_emit_decl(str(a), av, vmap, real_type, False, as_arg=True))
            cmt = _as_c_inline_comment(_first_unit_doc_comment(u))
            ret_decl = f"{ret_var.ctype} *" if ret_var.is_array else f"{ret_var.ctype} "
            out.append(f"{ret_decl}{u['name']}({', '.join(args)});{cmt}")
        elif u.get("kind") == "subroutine":
            args = []
            proc_name = str(u.get("name", "")).lower()
            extent_lists = proc_arg_extent_names.get(proc_name, [])
            flat_exts = [e for lst in extent_lists for e in lst]
            proto_simple_n = (len(flat_exts) == 1 and flat_exts[0] != "n")
            for idx, a in enumerate(u.get("args", [])):
                av = vmap.get(str(a).lower(), Var("int"))
                exts = extent_lists[idx] if idx < len(extent_lists) else []
                if exts:
                    use_exts = (["n"] if (proto_simple_n and len(exts) == 1) else exts)
                    args.extend([f"const int {nm}" for nm in use_exts])
                args.append(_emit_decl(str(a), av, vmap, real_type, False, as_arg=True))
            cmt = _as_c_inline_comment(_first_unit_doc_comment(u))
            out.append(f"void {u['name']}({', '.join(args)});{cmt}")
    if any(u.get("kind") in {"function", "subroutine"} for u in units):
        out.append("")

    for u, vmap in zip(units, decl_maps):
        out.extend(
            _transpile_unit(
                u,
            real_type,
            kind_ctype_map,
            proc_arg_modes,
            proc_arg_optional,
            proc_arg_ctypes,
            proc_arg_is_array,
            proc_arg_assumed_ranks,
            proc_arg_extent_names,
            array_result_funcs,
            vmap,
            one_line=one_line,
            annotate=annotate,
            )
        )
    out = _inject_runtime_helpers(out)
    if not units:
        out.extend(["int main(void) {", "   return 0;", "}"])
    return "\n".join(out).rstrip() + "\n"


def _print_summary_table(rows: List[Dict[str, object]]) -> None:
    if not rows:
        print("No files processed.")
        return

    def _btxt(v: object) -> str:
        if v is True:
            return "True"
        if v is False:
            return "False"
        return ""

    headers = ["source", "c_source", "compile_f90", "compile_c"]
    rendered: List[List[str]] = []
    for r in rows:
        rendered.append([
            str(r.get("source", "")),
            str(r.get("c_source", "")),
            _btxt(r.get("compile_f90")),
            _btxt(r.get("compile_c")),
        ])

    widths = [len(h) for h in headers]
    for vals in rendered:
        for i, v in enumerate(vals):
            if len(v) > widths[i]:
                widths[i] = len(v)

    print("")
    print("--summary:")
    print("  ".join(h.ljust(widths[i]) for i, h in enumerate(headers)))
    for vals in rendered:
        print("  ".join(vals[i].ljust(widths[i]) for i in range(len(headers))))


def main() -> int:
    ap = argparse.ArgumentParser(description="Small Fortran-to-C transpiler.")
    ap.add_argument("fortran_files", nargs="+", help="Input free-form Fortran source file(s) or glob pattern(s).")
    ap.add_argument("--out", default="temp.c", help="Output C file (default: temp.c).")
    ap.add_argument("--out-dir", default="", help="Output directory used with --mode-each.")
    ap.add_argument("--tee", action="store_true", help="Print generated C source.")
    ap.add_argument("--compile", action="store_true", help="Compile generated C source (no run).")
    ap.add_argument("--compile-c", action="store_true", help="Compile generated C source with -c only (no link).")
    ap.add_argument("--run", action="store_true", help="Compile/run generated C source.")
    ap.add_argument("--run-both", action="store_true", help="Build/run original Fortran source and generated C source.")
    ap.add_argument("--compile-both", action="store_true", help="Build (without running) original Fortran source and generated C source.")
    ap.add_argument("--compile-both-c", action="store_true", help="Compile original Fortran and generated C with -c only (no link).")
    ap.add_argument("--one-line", action="store_true", help="Collapse simple one-statement for/if blocks to one line.")
    ap.add_argument("--annotate", action="store_true", help="Insert C comments with original Fortran statements before translated code.")
    ap.add_argument("--mode-each", action="store_true", help="Process each input file independently (required for multiple inputs).")
    ap.add_argument("--summary", action="store_true", help="Print tabular per-file build summary.")
    ap.add_argument("--no-validate", action="store_true", help="Skip Fortran pre-validation checks before transpilation.")
    args = ap.parse_args()
    if args.run_both:
        args.run = True

    def _expand_inputs(raws: List[str]) -> List[Path]:
        out: List[Path] = []
        seen: set[str] = set()
        for raw in raws:
            has_glob = any(ch in raw for ch in "*?[]")
            matches = sorted(glob.glob(raw))
            if has_glob:
                if not matches:
                    print(f"Missing file: {raw}")
                    continue
                for m in matches:
                    p = Path(m)
                    if not p.is_file():
                        continue
                    k = str(p.resolve()).lower()
                    if k in seen:
                        continue
                    seen.add(k)
                    out.append(p)
                continue
            p = Path(raw)
            if not p.exists() or not p.is_file():
                print(f"Missing file: {p}")
                continue
            k = str(p.resolve()).lower()
            if k in seen:
                continue
            seen.add(k)
            out.append(p)
        return out

    src_paths = _expand_inputs(args.fortran_files)
    if not src_paths:
        return 1
    if len(src_paths) > 1 and not args.mode_each:
        print("Multiple input files require --mode-each.")
        return 1

    do_build_fortran = bool(args.compile_both or args.compile_both_c or args.run_both)
    do_build_c = bool(args.run or args.compile_both or args.compile_both_c or args.compile or args.compile_c)
    do_run_fortran = bool(args.run_both)
    do_run_c = bool(args.run)
    force_f_compile_only = bool(args.compile_both_c)
    force_c_compile_only = bool(args.compile_c or args.compile_both_c)

    def _build_and_run(label: str, build_cmd: List[str], exe_path: Path) -> int:
        print(f"Build ({label}):", " ".join(build_cmd))
        cp = subprocess.run(build_cmd, capture_output=True, text=True)
        if cp.returncode != 0:
            print(f"Build ({label}): FAIL")
            if cp.stdout.strip():
                print(cp.stdout.rstrip())
            if cp.stderr.strip():
                print(cp.stderr.rstrip())
            return 1
        print(f"Build ({label}): PASS")
        rp = subprocess.run([str(exe_path)], capture_output=True, text=True)
        if rp.returncode != 0:
            print(f"Run ({label}): FAIL (exit {rp.returncode})")
            if rp.stdout.strip():
                print(rp.stdout.rstrip())
            if rp.stderr.strip():
                print(rp.stderr.rstrip())
            return 1
        print(f"Run ({label}): PASS")
        if rp.stdout.strip():
            print(rp.stdout.rstrip())
        if rp.stderr.strip():
            print(rp.stderr.rstrip())
        return 0

    def _build_only(label: str, build_cmd: List[str]) -> int:
        print(f"Build ({label}):", " ".join(build_cmd))
        cp = subprocess.run(build_cmd, capture_output=True, text=True)
        if cp.returncode != 0:
            print(f"Build ({label}): FAIL")
            if cp.stdout.strip():
                print(cp.stdout.rstrip())
            if cp.stderr.strip():
                print(cp.stderr.rstrip())
            return 1
        print(f"Build ({label}): PASS")
        return 0

    out_dir = Path(args.out_dir) if args.out_dir else None
    if out_dir is not None:
        out_dir.mkdir(parents=True, exist_ok=True)

    def _out_path_for(src_path: Path, multi_mode: bool) -> Path:
        if multi_mode:
            if out_dir is not None:
                return out_dir / f"{src_path.stem}.c"
            return Path(f"{src_path.stem}.c")
        if out_dir is not None:
            return out_dir / Path(args.out).name
        return Path(args.out)

    had_error = False
    summary_rows: List[Dict[str, object]] = []
    multi_mode = args.mode_each and len(src_paths) >= 1
    for src_path in src_paths:
        row: Dict[str, object] = {
            "source": str(src_path),
            "c_source": "",
            "compile_f90": (False if do_build_fortran else None),
            "compile_c": (False if do_build_c else None),
        }
        src_text = src_path.read_text(encoding="utf-8", errors="ignore")
        src_units = fscan.split_fortran_units_simple(src_text)
        has_program_unit = any(u.get("kind") == "program" for u in src_units)

        if do_build_fortran:
            if has_program_unit and not force_f_compile_only:
                f_exe = src_path.with_suffix(".orig.exe")
                f_build_cmd = ["gfortran", str(src_path), "-o", str(f_exe)]
            else:
                f_obj = src_path.with_suffix(".orig.o")
                f_build_cmd = ["gfortran", "-c", str(src_path), "-o", str(f_obj)]
            if do_run_fortran and has_program_unit:
                rc = _build_and_run("original-fortran", f_build_cmd, f_exe)
            else:
                rc = _build_only("original-fortran", f_build_cmd)
            row["compile_f90"] = (rc == 0)
            if rc != 0:
                had_error = True
                summary_rows.append(row)
                if not multi_mode:
                    if args.summary:
                        _print_summary_table(summary_rows)
                    return rc
                continue

        try:
            c_src = transpile_fortran_to_c(
                src_text,
                one_line=args.one_line,
                validate=(not args.no_validate),
                annotate=args.annotate,
            )
        except ValueError as e:
            print(f"{src_path}: {e}")
            row["compile_c"] = False if do_build_c else row["compile_c"]
            summary_rows.append(row)
            had_error = True
            if not multi_mode:
                if args.summary:
                    _print_summary_table(summary_rows)
                return 1
            continue
        out_path = _out_path_for(src_path, multi_mode=(len(src_paths) > 1 or args.mode_each))
        out_path.write_text(c_src, encoding="utf-8")
        print(f"Wrote {out_path}")
        row["c_source"] = str(out_path)
        if args.tee:
            print(c_src, end="")

        if do_build_c:
            if has_program_unit and not force_c_compile_only:
                exe = out_path.with_suffix(".exe")
                c_build_cmd = ["gcc", str(out_path), "-lm", "-o", str(exe)]
            else:
                c_obj = out_path.with_suffix(".o")
                c_build_cmd = ["gcc", "-c", str(out_path), "-o", str(c_obj)]
            if do_run_c and has_program_unit:
                rc = _build_and_run("transformed-c", c_build_cmd, exe)
            else:
                rc = _build_only("transformed-c", c_build_cmd)
            row["compile_c"] = (rc == 0)
            if rc != 0:
                had_error = True
                summary_rows.append(row)
                if not multi_mode:
                    if args.summary:
                        _print_summary_table(summary_rows)
                    return rc
                continue
        summary_rows.append(row)

    if args.summary:
        _print_summary_table(summary_rows)
    return 1 if had_error else 0


if __name__ == "__main__":
    raise SystemExit(main())
