#!/usr/bin/env python3
"""Partial R-to-Fortran transpiler (numeric subset).

This is a pragmatic first pass analogous in workflow to xp2f.py:
- transpile an R script to free-form Fortran
- optionally compile/run Fortran
- optionally run original R via `rscript`
- optionally compare outputs
"""

from __future__ import annotations

import argparse
import difflib
import re
import shlex
import subprocess
import sys
import time
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path

import fortran_scan as fscan


@dataclass
class Assign:
    name: str
    expr: str
    comment: str = ""


@dataclass
class PrintStmt:
    args: list[str]
    comment: str = ""


@dataclass
class ForStmt:
    var: str
    iter_expr: str
    body: list[object]


@dataclass
class IfStmt:
    cond: str
    then_body: list[object]
    else_body: list[object]


@dataclass
class CallStmt:
    name: str
    args: list[str]
    comment: str = ""


@dataclass
class ExprStmt:
    expr: str
    comment: str = ""


@dataclass
class FuncDef:
    name: str
    args: list[str]
    defaults: dict[str, str]
    body: list[object]


def helper_modules_from_files(paths: list[Path]) -> set[str]:
    """Extract top-level module names from helper Fortran files."""
    mods: set[str] = set()
    m_re = re.compile(r"^\s*module\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
    end_re = re.compile(r"^\s*end\s+module\b", re.IGNORECASE)
    proc_re = re.compile(r"^\s*module\s+procedure\b", re.IGNORECASE)
    for p in paths:
        try:
            txt = p.read_text(encoding="utf-8")
        except Exception:
            txt = p.read_text(encoding="utf-8", errors="replace")
        for ln in txt.splitlines():
            s = ln.strip()
            if not s:
                continue
            if end_re.match(s) or proc_re.match(s):
                continue
            m = m_re.match(s)
            if m:
                mods.add(m.group(1).lower())
    return mods


@dataclass
class ListReturnSpec:
    fn_name: str
    root_fields: dict[str, object]
    nested_types: dict[tuple[str, ...], dict[str, object]]


def _split_top_level_else(text: str) -> tuple[str, str] | None:
    """Split `A else B` at top level, outside strings/parentheses."""
    in_single = False
    in_double = False
    esc = False
    depth = 0
    i = 0
    while i < len(text):
        ch = text[i]
        if esc:
            esc = False
            i += 1
            continue
        if ch == "\\":
            esc = True
            i += 1
            continue
        if ch == "'" and not in_double:
            in_single = not in_single
            i += 1
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            i += 1
            continue
        if not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")" and depth > 0:
                depth -= 1
            elif depth == 0 and text[i : i + 5] == " else":
                left = text[:i].strip()
                right = text[i + 5 :].strip()
                if left and right:
                    return left, right
        i += 1
    return None


def _parse_if_head(line: str) -> tuple[str, str] | None:
    s = line.strip()
    if not s.startswith("if"):
        return None
    m = re.match(r"^if\s*\(", s)
    if not m:
        return None
    i = m.end() - 1
    depth = 0
    in_single = False
    in_double = False
    esc = False
    j = i
    while j < len(s):
        ch = s[j]
        if esc:
            esc = False
            j += 1
            continue
        if ch == "\\":
            esc = True
            j += 1
            continue
        if ch == "'" and not in_double:
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
                    return s[i + 1 : j].strip(), s[j + 1 :].strip()
        j += 1
    return None


def parse_call_text(txt: str) -> tuple[str, list[str], dict[str, str]] | None:
    s = txt.strip()
    m = re.match(r"^([A-Za-z]\w*(?:\.[A-Za-z]\w*)*)\s*\((.*)\)\s*$", s)
    if not m:
        return None
    nm = m.group(1)
    inner = m.group(2).strip()
    parts = split_top_level_commas(inner) if inner else []
    pos: list[str] = []
    kw: dict[str, str] = {}
    for p in parts:
        ma = re.match(r"^([A-Za-z]\w*)\s*=\s*(.+)$", p.strip())
        if ma:
            kw[ma.group(1)] = ma.group(2).strip()
        else:
            pos.append(p.strip())
    return nm, pos, kw


def _fortran_str_literal(raw: str) -> str:
    txt = raw.replace('"', '""')
    return f'"{txt}"'


def _dequote_string_literal(s: str) -> str | None:
    t = s.strip()
    if len(t) >= 2 and ((t[0] == '"' and t[-1] == '"') or (t[0] == "'" and t[-1] == "'")):
        return t[1:-1]
    return None


def _split_sprintf_format(fmt: str) -> tuple[list[str], int]:
    # Split on printf-like conversion specs and return literal pieces + count(specs).
    # Supports common specs used by these scripts (e.g. %d, %.6f, %g).
    spec_re = re.compile(r"%(?:[-+ 0#]*)(?:\d+)?(?:\.\d+)?[a-zA-Z]")
    pieces: list[str] = []
    last = 0
    nspec = 0
    for m in spec_re.finditer(fmt):
        pieces.append(fmt[last : m.start()])
        last = m.end()
        nspec += 1
    pieces.append(fmt[last:])
    return pieces, nspec


def _replace_balanced_func_calls(expr: str, fname: str, repl_fn) -> str:
    """Replace `fname(<arg>)` calls using balanced-parentheses parsing."""
    out: list[str] = []
    i = 0
    n = len(expr)
    fnlow = fname.lower()
    while i < n:
        m = re.search(rf"\b{re.escape(fname)}\b", expr[i:], re.IGNORECASE)
        if m is None:
            out.append(expr[i:])
            break
        s0 = i + m.start()
        e0 = i + m.end()
        out.append(expr[i:s0])
        j = e0
        while j < n and expr[j].isspace():
            j += 1
        if j >= n or expr[j] != "(":
            out.append(expr[s0:e0])
            i = e0
            continue
        depth = 0
        in_single = False
        in_double = False
        k = j
        close = -1
        while k < n:
            ch = expr[k]
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
                        close = k
                        break
            k += 1
        if close < 0:
            out.append(expr[s0:])
            break
        inner = expr[j + 1 : close]
        out.append(repl_fn(inner))
        i = close + 1
    return "".join(out)


def _display_expr_to_fortran(expr: str) -> str:
    """Lower display-oriented R wrappers (sprintf/paste) to printable Fortran expr."""
    s = expr.strip()
    cinfo = parse_call_text(s)
    if cinfo is not None:
        nm, pos, kw = cinfo
        low = nm.lower()
        if low == "sprintf":
            if len(pos) >= 2:
                return r_expr_to_fortran(pos[1])
            if len(pos) == 1:
                return r_expr_to_fortran(pos[0])
        if low == "paste":
            # Common case: paste(sprintf(fmt, arr), collapse=" ")
            if pos:
                inner = pos[0].strip()
                c2 = parse_call_text(inner)
                if c2 is not None and c2[0].lower() == "sprintf":
                    p2 = c2[1]
                    if len(p2) >= 2:
                        return r_expr_to_fortran(p2[1])
                return r_expr_to_fortran(inner)
    return r_expr_to_fortran(s)


def _sprintf_arg_items(expr: str) -> list[str] | None:
    """Lower sprintf(fmt, ...) into printable Fortran item expressions."""
    ci = parse_call_text(expr.strip())
    if ci is None or ci[0].lower() != "sprintf":
        return None
    pos = ci[1]
    if not pos:
        return []
    fmt_raw = _dequote_string_literal(pos[0])
    vals = [r_expr_to_fortran(a) for a in pos[1:]]
    if fmt_raw is None:
        return vals
    pieces, nspec = _split_sprintf_format(fmt_raw)
    out_items: list[str] = []
    nuse = min(nspec, len(vals))
    for i in range(nuse + 1):
        lit = pieces[i].replace("\\n", "").replace("\\t", " ")
        if lit:
            out_items.append(_fortran_str_literal(lit))
        if i < nuse:
            out_items.append(vals[i])
    if nuse < len(vals):
        out_items.extend(vals[nuse:])
    return out_items


def strip_r_comment(line: str) -> str:
    out = []
    in_single = False
    in_double = False
    esc = False
    for ch in line:
        if esc:
            out.append(ch)
            esc = False
            continue
        if ch == "\\":
            out.append(ch)
            esc = True
            continue
        if ch == "'" and not in_double:
            in_single = not in_single
            out.append(ch)
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            out.append(ch)
            continue
        if ch == "#" and not in_single and not in_double:
            break
        out.append(ch)
    return "".join(out).rstrip()


def split_r_code_comment(line: str) -> tuple[str, str]:
    """Split R source line into code and trailing `#` comment (outside strings)."""
    out = []
    in_single = False
    in_double = False
    esc = False
    for i, ch in enumerate(line):
        if esc:
            out.append(ch)
            esc = False
            continue
        if ch == "\\":
            out.append(ch)
            esc = True
            continue
        if ch == "'" and not in_double:
            in_single = not in_single
            out.append(ch)
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            out.append(ch)
            continue
        if ch == "#" and not in_single and not in_double:
            return "".join(out), line[i + 1 :]
        out.append(ch)
    return "".join(out), ""


def extract_r_top_comments(src: str) -> list[str]:
    """Collect leading top-of-file R comments (before first code statement)."""
    out: list[str] = []
    seen_code = False
    for raw in src.splitlines():
        code, cmt = split_r_code_comment(raw)
        tcode = code.strip()
        tcmt = cmt.strip()
        if tcode:
            seen_code = True
            break
        if tcmt:
            if tcmt.startswith("!"):
                continue
            out.append(tcmt)
    dedup: list[str] = []
    prev = None
    for c in out:
        if c != prev:
            dedup.append(c)
        prev = c
    return dedup


def _normalize_r_code_key(code: str) -> str:
    return re.sub(r"\s+", " ", code.strip())


def build_r_comment_lookup(src: str) -> dict[str, list[str]]:
    out: dict[str, list[str]] = {}
    pending: list[str] = []
    seen_code = False
    for raw in src.splitlines():
        code, cmt = split_r_code_comment(raw)
        tcode = code.strip()
        tcmt = cmt.strip()
        if not tcode:
            if tcmt:
                if seen_code:
                    pending.append(tcmt)
            continue
        seen_code = True
        key = _normalize_r_code_key(tcode)
        if pending:
            out.setdefault(key, []).append(" | ".join(pending))
            pending = []
        if tcmt:
            out.setdefault(key, []).append(tcmt)
    return out


def pop_comment_for_code(code: str, lookup: dict[str, list[str]] | None) -> str:
    if not lookup:
        return ""
    key = _normalize_r_code_key(code)
    arr = lookup.get(key)
    if not arr:
        return ""
    c = arr.pop(0).strip()
    if not arr:
        lookup.pop(key, None)
    return c


def split_top_level_commas(s: str) -> list[str]:
    out: list[str] = []
    cur: list[str] = []
    depth = 0
    in_single = False
    in_double = False
    esc = False
    for ch in s:
        if esc:
            cur.append(ch)
            esc = False
            continue
        if ch == "\\":
            cur.append(ch)
            esc = True
            continue
        if ch == "'" and not in_double:
            in_single = not in_single
            cur.append(ch)
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            cur.append(ch)
            continue
        if not in_single and not in_double:
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


def split_top_level_semicolons(s: str) -> list[str]:
    out: list[str] = []
    cur: list[str] = []
    depth = 0
    in_single = False
    in_double = False
    esc = False
    for ch in s:
        if esc:
            cur.append(ch)
            esc = False
            continue
        if ch == "\\":
            cur.append(ch)
            esc = True
            continue
        if ch == "'" and not in_double:
            in_single = not in_single
            cur.append(ch)
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            cur.append(ch)
            continue
        if not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")" and depth > 0:
                depth -= 1
            elif ch == ";" and depth == 0:
                out.append("".join(cur).strip())
                cur = []
                continue
        cur.append(ch)
    tail = "".join(cur).strip()
    if tail:
        out.append(tail)
    return out


def preprocess_r_lines(src: str) -> list[str]:
    lines0 = [strip_r_comment(ln) for ln in src.splitlines()]
    lines0 = [ln for ln in lines0 if ln.strip()]
    # Join multiline statements by balanced parentheses.
    joined: list[str] = []
    cur = ""
    depth = 0
    in_single = False
    in_double = False
    for ln in lines0:
        txt = ln.strip()
        if not cur:
            cur = txt
        else:
            cur = cur + " " + txt
        i = 0
        while i < len(txt):
            ch = txt[i]
            if ch == "'" and not in_double:
                in_single = not in_single
            elif ch == '"' and not in_single:
                in_double = not in_double
            elif not in_single and not in_double:
                if ch == "(":
                    depth += 1
                elif ch == ")" and depth > 0:
                    depth -= 1
            i += 1
        if depth == 0 and not in_single and not in_double:
            joined.append(cur)
            cur = ""
    if cur.strip():
        joined.append(cur)
    lines0 = joined
    out: list[str] = []
    for ln in lines0:
        # split top-level semicolon-separated statements
        semis = split_top_level_semicolons(ln) if ";" in ln else [ln]
        for ln_part in semis:
            cur = ln_part
            # make braces standalone tokens to simplify parsing
            while "{" in cur or "}" in cur:
                i_open = cur.find("{") if "{" in cur else 10**9
                i_close = cur.find("}") if "}" in cur else 10**9
                i = min(i_open, i_close)
                if i == 10**9:
                    break
                left = cur[:i].strip()
                br = cur[i]
                right = cur[i + 1 :].strip()
                if left:
                    out.append(left)
                out.append(br)
                cur = right
            if cur.strip():
                out.append(cur.strip())
    return out


def parse_single_statement(ln: str, *, comment_lookup: dict[str, list[str]] | None = None) -> object:
    ln = ln.strip()
    cmt = pop_comment_for_code(ln, comment_lookup)
    if ln == "break":
        return ExprStmt(expr="break", comment=cmt)
    if ln == "next":
        return ExprStmt(expr="next", comment=cmt)
    if ln.startswith("function("):
        raise NotImplementedError("nested/anonymous function definitions not supported")
    if ln.startswith("print(") and ln.endswith(")"):
        inner = ln[len("print(") : -1].strip()
        args = split_top_level_commas(inner) if inner else []
        return PrintStmt(args=args, comment=cmt)
    m_asn = re.match(r"^([A-Za-z]\w*)\s*(<-|=)\s*(.+)$", ln)
    if m_asn:
        rhs = m_asn.group(3).strip()
        return Assign(name=m_asn.group(1), expr=rhs, comment=cmt)
    m_asn_any = re.match(
        r"^([A-Za-z]\w*(?:\[[^\]]+\])?(?:\$[A-Za-z]\w*(?:\[[^\]]+\])?)*)\s*(<-|=)\s*(.+)$",
        ln,
    )
    if m_asn_any:
        # Keep non-simple LHS assignments as generic expr statements.
        return ExprStmt(expr=ln, comment=cmt)
    cinfo = parse_call_text(ln)
    if cinfo is not None:
        nm, pos, kw = cinfo
        args = list(pos) + [f"{k}={v}" for k, v in kw.items()]
        if nm.lower() in {"print", "stopifnot", "set.seed", "cat", "stop", "writelines", "write.table"}:
            return CallStmt(name=nm, args=args, comment=cmt)
        return ExprStmt(expr=ln, comment=cmt)
    return ExprStmt(expr=ln, comment=cmt)


def parse_block(
    lines: list[str],
    i0: int = 0,
    *,
    stop_at_rbrace: bool = False,
    comment_lookup: dict[str, list[str]] | None = None,
) -> tuple[list[object], int]:
    stmts: list[object] = []
    i = i0
    while i < len(lines):
        ln = lines[i].strip()
        if ln == "}":
            if stop_at_rbrace:
                return stmts, i + 1
            i += 1
            continue
        if ln == "{":
            i += 1
            continue

        m_for = re.match(r"^for\s*\(\s*([A-Za-z]\w*)\s+in\s+(.+)\s*\)\s*$", ln)
        if m_for:
            var = m_for.group(1)
            itexpr = m_for.group(2).strip()
            i += 1
            if i >= len(lines) or lines[i].strip() != "{":
                raise NotImplementedError("for requires braced body in this subset")
            body, i = parse_block(lines, i + 1, stop_at_rbrace=True, comment_lookup=comment_lookup)
            stmts.append(ForStmt(var=var, iter_expr=itexpr, body=body))
            continue

        m_fn = re.match(r"^([A-Za-z]\w*)\s*(?:<-|=)\s*function\s*\((.*)\)\s*$", ln)
        if m_fn:
            fname = m_fn.group(1)
            arg_txt = m_fn.group(2).strip()
            args: list[str] = []
            defaults: dict[str, str] = {}
            if arg_txt:
                for part in split_top_level_commas(arg_txt):
                    m_ap = re.match(r"^([A-Za-z]\w*)\s*=\s*(.+)$", part.strip())
                    if m_ap:
                        anm = m_ap.group(1)
                        args.append(anm)
                        defaults[anm] = m_ap.group(2).strip()
                    else:
                        args.append(part.strip())
            i += 1
            if i >= len(lines) or lines[i].strip() != "{":
                raise NotImplementedError("function requires braced body in this subset")
            body, i = parse_block(lines, i + 1, stop_at_rbrace=True, comment_lookup=comment_lookup)
            stmts.append(FuncDef(name=fname, args=args, defaults=defaults, body=body))
            continue

        ih = _parse_if_head(ln)
        if ih is not None:
            cond, tail = ih
            then_body: list[object] = []
            else_body: list[object] = []
            if tail:
                split_tail = _split_top_level_else(" " + tail)
                if split_tail is not None:
                    then_body = [parse_single_statement(split_tail[0], comment_lookup=comment_lookup)]
                    else_body = [parse_single_statement(split_tail[1], comment_lookup=comment_lookup)]
                    i += 1
                else:
                    then_body = [parse_single_statement(tail, comment_lookup=comment_lookup)]
                    i += 1
            else:
                i += 1
                if i < len(lines) and lines[i].strip() == "{":
                    then_body, i = parse_block(lines, i + 1, stop_at_rbrace=True, comment_lookup=comment_lookup)
                else:
                    # accept single next-statement body without braces
                    if i >= len(lines):
                        raise NotImplementedError("if missing body")
                    then_body = [parse_single_statement(lines[i], comment_lookup=comment_lookup)]
                    i += 1
                if i < len(lines) and lines[i].strip() == "else":
                    i += 1
                    if i < len(lines) and lines[i].strip() == "{":
                        else_body, i = parse_block(lines, i + 1, stop_at_rbrace=True, comment_lookup=comment_lookup)
                    else:
                        if i >= len(lines):
                            raise NotImplementedError("else missing body")
                        else_body = [parse_single_statement(lines[i], comment_lookup=comment_lookup)]
                        i += 1
            stmts.append(IfStmt(cond=cond, then_body=then_body, else_body=else_body))
            continue

        try:
            st = parse_single_statement(ln, comment_lookup=comment_lookup)
            stmts.append(st)
            i += 1
            continue
        except NotImplementedError as e:
            if "function definitions not yet supported" in str(e):
                raise
            pass

        raise NotImplementedError(f"unrecognized statement: {ln}")
    return stmts, i


def infer_assigned_names(stmts: list[object], out: dict[str, int] | None = None) -> dict[str, int]:
    if out is None:
        out = {}
    for st in stmts:
        if isinstance(st, Assign):
            out[st.name] = out.get(st.name, 0) + 1
        elif isinstance(st, ForStmt):
            out[st.var] = out.get(st.var, 0) + 1
            infer_assigned_names(st.body, out)
        elif isinstance(st, IfStmt):
            infer_assigned_names(st.then_body, out)
            infer_assigned_names(st.else_body, out)
        elif isinstance(st, FuncDef):
            # separate scope
            continue
    return out


def _is_int_literal(txt: str) -> bool:
    t = txt.strip()
    return re.match(r"^[+-]?\d+[lL]?(?:_[A-Za-z]\w*)?$", t) is not None


def _is_real_literal(txt: str) -> bool:
    t = txt.strip()
    return (
        re.match(r"^[+-]?\d+\.\d*([eE][+-]?\d+)?(?:_[A-Za-z]\w*)?$", t) is not None
        or re.match(r"^[+-]?\d+[eE][+-]?\d+(?:_[A-Za-z]\w*)?$", t) is not None
    )


def _is_integer_arith_expr(txt: str) -> bool:
    """Conservative integer-only arithmetic expression checker (R syntax)."""
    t = txt.strip()
    if not t:
        return False
    if any(ch.isalpha() for ch in t):
        return False
    if "." in t:
        return False
    if re.search(r"\d[eE][+-]?\d", t):
        return False
    if re.search(r"[^0-9\+\-\*/\^\(\)\s]", t):
        return False
    return re.search(r"\d", t) is not None


def _contains_name(expr: str, name: str) -> bool:
    return re.search(rf"\b{re.escape(name)}\b", expr) is not None


def _ifelse_integer_coded(rhs: str) -> bool:
    m = re.match(r"^ifelse\s*\(\s*.+\s*,\s*([^,]+)\s*,\s*([^)]+)\s*\)\s*$", rhs.strip())
    if not m:
        return False
    a = m.group(1).strip()
    b = m.group(2).strip()
    return _is_int_literal(a) and _is_int_literal(b)


def classify_vars(
    stmts: list[object], assign_counts: dict[str, int], known_arrays: set[str] | None = None
) -> tuple[set[str], set[str], set[str], set[str], dict[str, str]]:
    ints: set[str] = set()
    real_scalars: set[str] = set()
    int_arrays: set[str] = set()
    real_arrays: set[str] = set()
    params: dict[str, str] = {}
    known_arrays = set(known_arrays or set())

    def mark_array_uses(txt: str) -> None:
        for m in re.finditer(r"\b([A-Za-z]\w*)\s*\[", txt):
            nm = m.group(1)
            known_arrays.add(nm)
            real_arrays.add(nm)
            int_arrays.discard(nm)
            real_scalars.discard(nm)
            ints.discard(nm)
            params.pop(nm, None)

    def walk(ss: list[object]) -> None:
        for st in ss:
            if isinstance(st, ForStmt):
                it = st.iter_expr.strip()
                mark_array_uses(it)
                if re.match(r"^seq_len\s*\(", it) or re.match(r"^.+:.+$", it):
                    ints.add(st.var)
                    real_scalars.discard(st.var)
                    real_arrays.discard(st.var)
                elif re.match(r"^[A-Za-z]\w*$", it):
                    real_scalars.add(st.var)
                    ints.discard(st.var)
                else:
                    ints.add(st.var)
                walk(st.body)
            elif isinstance(st, IfStmt):
                mark_array_uses(st.cond)
                walk(st.then_body)
                walk(st.else_body)
            elif isinstance(st, Assign):
                rhs = st.expr.strip()
                mark_array_uses(rhs)
                if _ifelse_integer_coded(rhs):
                    int_arrays.add(st.name)
                    known_arrays.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    real_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                elif rhs.lower().startswith("sample.int("):
                    int_arrays.add(st.name)
                    known_arrays.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    real_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                elif rhs.startswith("c(") or rhs.startswith("runif(") or rhs.startswith("rnorm(") or rhs.startswith("ifelse("):
                    real_arrays.add(st.name)
                    known_arrays.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    int_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                elif rhs.lower().startswith("scan("):
                    real_arrays.add(st.name)
                    known_arrays.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    int_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                elif re.match(r"^(length|size)\s*\(", rhs):
                    ints.add(st.name)
                    params.pop(st.name, None)
                    known_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                    int_arrays.discard(st.name)
                    real_arrays.discard(st.name)
                elif re.match(r"^(sum|mean|sd|sqrt|max|min)\s*\(", rhs):
                    real_scalars.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    known_arrays.discard(st.name)
                    int_arrays.discard(st.name)
                    real_arrays.discard(st.name)
                elif any(re.search(rf"\b{re.escape(a)}\b", rhs) for a in known_arrays):
                    real_arrays.add(st.name)
                    known_arrays.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    int_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                elif _is_int_literal(rhs):
                    # Do not force integer typing for variables already inferred real.
                    if st.name in real_scalars or st.name in real_arrays:
                        pass
                    elif assign_counts.get(st.name, 0) == 1:
                        params[st.name] = rhs
                    else:
                        ints.add(st.name)
                        known_arrays.discard(st.name)
                        int_arrays.discard(st.name)
                elif _is_integer_arith_expr(rhs):
                    if st.name in real_scalars or st.name in real_arrays:
                        pass
                    elif assign_counts.get(st.name, 0) == 1:
                        params[st.name] = r_expr_to_fortran(rhs)
                    else:
                        ints.add(st.name)
                        known_arrays.discard(st.name)
                        int_arrays.discard(st.name)
                        real_arrays.discard(st.name)
                        params.pop(st.name, None)
                else:
                    real_scalars.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    known_arrays.discard(st.name)
                    int_arrays.discard(st.name)
                    real_arrays.discard(st.name)

    walk(stmts)
    # move params out of scalar var declarations
    for p in params:
        ints.discard(p)
        real_scalars.discard(p)
        int_arrays.discard(p)
        real_arrays.discard(p)
    return ints, real_scalars, int_arrays, real_arrays, params


def infer_arg_rank(fn: FuncDef, arg: str) -> int:
    pats = [
        re.compile(rf"\bsize\s*\(\s*{re.escape(arg)}\b"),
        re.compile(rf"\bsum\s*\(\s*{re.escape(arg)}\b"),
        re.compile(rf"\bmean\s*\(\s*{re.escape(arg)}\b"),
        re.compile(rf"\b{re.escape(arg)}\s*\["),
    ]

    def _scan(ss: list[object]) -> bool:
        for st in ss:
            if isinstance(st, Assign):
                txt = st.expr
            elif isinstance(st, IfStmt):
                txt = st.cond
            elif isinstance(st, CallStmt):
                txt = ", ".join(st.args)
            elif isinstance(st, ExprStmt):
                txt = st.expr
            elif isinstance(st, ForStmt):
                txt = st.iter_expr
            else:
                txt = ""
            if any(p.search(txt) for p in pats):
                return True
            if isinstance(st, ForStmt):
                if _scan(st.body):
                    return True
            elif isinstance(st, IfStmt):
                if _scan(st.then_body) or _scan(st.else_body):
                    return True
        return False

    return 1 if _scan(fn.body) else 0


def infer_written_args(fn: FuncDef) -> set[str]:
    """Conservatively infer function arguments written in the function body."""
    written: set[str] = set()
    argset = set(fn.args)

    def walk(ss: list[object]) -> None:
        for st in ss:
            if isinstance(st, Assign):
                if st.name in argset:
                    rhs = st.expr.strip()
                    # Ignore normalization identities that transpile away
                    # (e.g., mu <- as.numeric(mu)).
                    if r_expr_to_fortran(rhs) != st.name:
                        written.add(st.name)
            elif isinstance(st, ForStmt):
                if st.var in argset:
                    written.add(st.var)
                walk(st.body)
            elif isinstance(st, IfStmt):
                walk(st.then_body)
                walk(st.else_body)

    walk(fn.body)
    return written


def _replace_idents(expr: str, mapping: dict[str, str]) -> str:
    if not mapping:
        return expr
    out = expr
    for old in sorted(mapping.keys(), key=len, reverse=True):
        out = re.sub(rf"\b{re.escape(old)}\b", mapping[old], out)
    return out


def _stmt_uses_name(st: object, name: str) -> int:
    pat = re.compile(rf"\b{re.escape(name)}\b")
    if isinstance(st, Assign):
        return len(pat.findall(st.expr))
    if isinstance(st, PrintStmt):
        return sum(len(pat.findall(a)) for a in st.args)
    if isinstance(st, CallStmt):
        return sum(len(pat.findall(a)) for a in st.args)
    if isinstance(st, ExprStmt):
        return len(pat.findall(st.expr))
    if isinstance(st, ForStmt):
        n = len(pat.findall(st.iter_expr))
        for b in st.body:
            n += _stmt_uses_name(b, name)
        return n
    if isinstance(st, IfStmt):
        n = len(pat.findall(st.cond))
        for b in st.then_body:
            n += _stmt_uses_name(b, name)
        for b in st.else_body:
            n += _stmt_uses_name(b, name)
        return n
    return 0


def _stmt_writes_name(st: object, name: str) -> bool:
    if isinstance(st, Assign):
        return st.name == name
    if isinstance(st, ForStmt):
        if st.var == name:
            return True
        return any(_stmt_writes_name(b, name) for b in st.body)
    if isinstance(st, IfStmt):
        return any(_stmt_writes_name(b, name) for b in st.then_body) or any(
            _stmt_writes_name(b, name) for b in st.else_body
        )
    return False


def _replace_name_in_stmt(st: object, name: str, repl: str) -> object:
    r = repl.strip()
    simple_f_re = re.compile(r"^[a-z][a-z0-9_]*(?:%[a-z][a-z0-9_]*|\([^()]*\))*$", re.IGNORECASE)
    simple_r_re = re.compile(r"^[a-z][a-z0-9_]*(?:\$[a-z][a-z0-9_]*|\[[^\[\]]*\]|\([^()]*\))*$", re.IGNORECASE)
    if simple_f_re.fullmatch(r) or simple_r_re.fullmatch(r):
        return _rename_stmt_obj(st, {name: r})
    return _rename_stmt_obj(st, {name: f"({r})"})


def _is_inline_temp_rhs(expr: str) -> bool:
    t = expr.strip()
    if not t:
        return False
    # Keep named constants/constructor candidates as explicit declarations.
    if _is_int_literal(t) or _is_real_literal(t) or t in {"TRUE", "FALSE"}:
        return False
    if t.startswith("c(") or (t.startswith("[") and t.endswith("]")):
        return False
    if (t.startswith('"') and t.endswith('"')) or (t.startswith("'") and t.endswith("'")):
        return False
    return True


def inline_single_use_temporaries(stmts: list[object]) -> list[object]:
    """Inline simple single-use temporaries (`t = expr`) into their sole later use."""
    out = list(stmts)
    changed = True
    while changed:
        changed = False
        i = 0
        while i < len(out):
            st = out[i]
            if isinstance(st, ForStmt):
                nb = inline_single_use_temporaries(st.body)
                if nb != st.body:
                    out[i] = ForStmt(var=st.var, iter_expr=st.iter_expr, body=nb)
                    changed = True
                i += 1
                continue
            if isinstance(st, IfStmt):
                nt = inline_single_use_temporaries(st.then_body)
                ne = inline_single_use_temporaries(st.else_body)
                if nt != st.then_body or ne != st.else_body:
                    out[i] = IfStmt(cond=st.cond, then_body=nt, else_body=ne)
                    changed = True
                i += 1
                continue
            if not isinstance(st, Assign):
                i += 1
                continue
            name = st.name.strip()
            if not re.match(r"^[a-z][a-z0-9_]*$", name, re.IGNORECASE):
                i += 1
                continue
            if not _is_inline_temp_rhs(st.expr):
                i += 1
                continue
            total_uses = sum(_stmt_uses_name(sj, name) for sj in out[i + 1 :])
            if total_uses != 1:
                i += 1
                continue
            use_j = -1
            blocked = False
            for j in range(i + 1, len(out)):
                if _stmt_writes_name(out[j], name):
                    blocked = True
                    break
                if _stmt_uses_name(out[j], name) > 0:
                    use_j = j
                    break
            if blocked or use_j < 0:
                i += 1
                continue
            out[use_j] = _replace_name_in_stmt(out[use_j], name, st.expr)
            del out[i]
            changed = True
            break
    return out


def _rename_stmt_obj(st: object, mapping: dict[str, str]) -> object:
    if not mapping:
        return st
    if isinstance(st, Assign):
        return Assign(
            name=mapping.get(st.name, st.name),
            expr=_replace_idents(st.expr, mapping),
            comment=st.comment,
        )
    if isinstance(st, PrintStmt):
        return PrintStmt(
            args=[_replace_idents(a, mapping) for a in st.args],
            comment=st.comment,
        )
    if isinstance(st, ForStmt):
        return ForStmt(
            var=mapping.get(st.var, st.var),
            iter_expr=_replace_idents(st.iter_expr, mapping),
            body=[_rename_stmt_obj(s, mapping) for s in st.body],
        )
    if isinstance(st, IfStmt):
        return IfStmt(
            cond=_replace_idents(st.cond, mapping),
            then_body=[_rename_stmt_obj(s, mapping) for s in st.then_body],
            else_body=[_rename_stmt_obj(s, mapping) for s in st.else_body],
        )
    if isinstance(st, CallStmt):
        return CallStmt(
            name=st.name,
            args=[_replace_idents(a, mapping) for a in st.args],
            comment=st.comment,
        )
    if isinstance(st, ExprStmt):
        return ExprStmt(expr=_replace_idents(st.expr, mapping), comment=st.comment)
    return st


def _stmt_tree_has_side_effect_ops(ss: list[object]) -> bool:
    """Conservative impurity test for R-subset function bodies."""
    bad_call_names = {"set.seed", "cat", "print"}

    def walk(stmts: list[object]) -> bool:
        for st in stmts:
            if isinstance(st, PrintStmt):
                return True
            if isinstance(st, CallStmt):
                nm = st.name.lower()
                if nm in bad_call_names:
                    return True
            if isinstance(st, Assign):
                rhs = st.expr.lower()
                if "runif(" in rhs or "rnorm(" in rhs:
                    return True
            if isinstance(st, ExprStmt):
                ex = st.expr.lower()
                if "runif(" in ex or "rnorm(" in ex:
                    return True
            if isinstance(st, ForStmt):
                if walk(st.body):
                    return True
            elif isinstance(st, IfStmt):
                if walk(st.then_body) or walk(st.else_body):
                    return True
        return False

    return walk(ss)


def _cond_identifiers(expr: str) -> set[str]:
    """Collect identifier-like tokens from an R condition expression."""
    out: set[str] = set()
    for m in re.finditer(r"\b([A-Za-z_]\w*)\b", expr):
        out.add(m.group(1).lower())
    return out


def _is_hoistable_stopifnot_stmt(st: object, allowed_names: set[str]) -> bool:
    """True when a top-level stopifnot can be hoisted before body code."""
    if not isinstance(st, CallStmt):
        return False
    if st.name.lower() != "stopifnot":
        return False
    intr_names = {
        "length",
        "size",
        "all",
        "any",
        "sum",
        "mean",
        "sd",
        "sqrt",
        "max",
        "min",
        "abs",
        "floor",
        "ceiling",
        "log",
        "exp",
        "sin",
        "cos",
        "tan",
        "asin",
        "acos",
        "atan",
        "is",
        "finite",
        "null",
        "true",
        "false",
        "na_real_",
        "int",
        "real",
        "nrow",
        "ncol",
        "dp",
        "real64",
    }
    for a in st.args:
        ids = _cond_identifiers(a)
        bad = {x for x in ids if x not in allowed_names and x not in intr_names}
        if bad:
            return False
    return True


def _looks_integer_fortran_expr(expr: str) -> bool:
    t = expr.strip()
    if not t:
        return False
    if _is_int_literal(t):
        return True
    if re.match(r"^[A-Za-z]\w*$", t):
        return True
    if re.match(r"^size\s*\(.+\)$", t, re.IGNORECASE):
        return True
    if re.match(r"^int\s*\(.+\)$", t, re.IGNORECASE):
        return True
    return False


def _int_bound_expr(expr: str) -> str:
    t = expr.strip()
    m = re.match(r"^int\s*\((.+)\)$", t, re.IGNORECASE)
    if m:
        return m.group(1).strip()
    if _looks_integer_fortran_expr(t):
        return t
    return f"int({t})"


def _negate_simple_relational_expr(expr_f: str) -> str | None:
    """Return negated relational expression for simple `lhs op rhs` forms."""
    s = fscan.strip_redundant_outer_parens_expr(expr_f.strip())
    # Split at top-level relational operator only (outside parentheses/strings).
    ops = [">=", "<=", "==", "/=", ">", "<", ".ge.", ".le.", ".eq.", ".ne.", ".gt.", ".lt."]
    in_single = False
    in_double = False
    depth = 0
    lhs = ""
    rhs = ""
    op_found: str | None = None
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
        if not in_single and not in_double:
            if ch == "(":
                depth += 1
                i += 1
                continue
            if ch == ")" and depth > 0:
                depth -= 1
                i += 1
                continue
            if depth == 0:
                low_rest = s[i:].lower()
                hit = None
                for op in ops:
                    if low_rest.startswith(op):
                        hit = op
                        break
                if hit is not None:
                    lhs = s[:i].strip()
                    rhs = s[i + len(hit) :].strip()
                    op_found = hit
                    break
        i += 1

    if not lhs or not rhs or op_found is None:
        return None
    op = op_found.lower()
    inv = {
        ">=": "<",
        "<=": ">",
        ">": "<=",
        "<": ">=",
        "==": "/=",
        "/=": "==",
        ".ge.": ".lt.",
        ".le.": ".gt.",
        ".gt.": ".le.",
        ".lt.": ".ge.",
        ".eq.": ".ne.",
        ".ne.": ".eq.",
    }.get(op)
    if inv is None:
        return None
    return f"{lhs} {inv} {rhs}"


def _fortran_error_msg(text: str) -> str:
    """Build safe Fortran double-quoted error text literal."""
    t = " ".join(text.strip().split())
    t = t.replace('"', '""')
    return f'"{t}"'


def _is_simple_value_for_merge(expr_f: str) -> bool:
    """True when expression is a simple value (literal or variable reference)."""
    t = expr_f.strip()
    if not t:
        return False
    if _is_int_literal(t) or _is_real_literal(t):
        return True
    if re.match(r"^\.(true|false)\.$", t, re.IGNORECASE):
        return True
    # Simple variable or component, optionally with one index list: a, a%b, a(i), a%b(i,j)
    if re.match(r"^[A-Za-z]\w*(?:%[A-Za-z]\w*)*(?:\([^()]*\))?$", t):
        return True
    return False


def _parse_list_constructor(expr: str) -> dict[str, object] | None:
    s = expr.strip()
    if not (s.startswith("list(") and s.endswith(")")):
        return None
    inner = s[len("list(") : -1].strip()
    out: dict[str, object] = {}
    if not inner:
        return out
    for p in split_top_level_commas(inner):
        m = re.match(r"^([A-Za-z]\w*)\s*=\s*(.+)$", p.strip())
        if not m:
            return None
        k = m.group(1)
        vtxt = m.group(2).strip()
        nested = _parse_list_constructor(vtxt)
        out[k] = nested if nested is not None else vtxt
    return out


def _collect_nested_types(fn_name: str, fields: dict[str, object], path: tuple[str, ...] = ()) -> dict[tuple[str, ...], dict[str, object]]:
    out: dict[tuple[str, ...], dict[str, object]] = {}
    out[path] = fields
    for k, v in fields.items():
        if isinstance(v, dict):
            out.update(_collect_nested_types(fn_name, v, path + (k,)))
    return out


def _type_name_for_path(fn_name: str, path: tuple[str, ...]) -> str:
    if not path:
        return f"{fn_name}_result_t"
    return f"{fn_name}_{'_'.join(path)}_t"


def _list_return_specs(funcs: list[FuncDef]) -> dict[str, ListReturnSpec]:
    specs: dict[str, ListReturnSpec] = {}
    for fn in funcs:
        if not fn.body:
            continue
        last = fn.body[-1]
        if not isinstance(last, ExprStmt):
            continue
        fields = _parse_list_constructor(last.expr)
        if fields is None:
            continue
        specs[fn.name] = ListReturnSpec(
            fn_name=fn.name,
            root_fields=fields,
            nested_types=_collect_nested_types(fn.name, fields),
        )
    return specs


def r_expr_to_fortran(expr: str) -> str:
    s = expr.strip()
    # lm accessors / summary fields (subset)
    s = re.sub(r"\bsummary\s*\(\s*([A-Za-z]\w*)\s*\)\s*\$\s*r\.squared\b", r"\1%r_squared", s)
    s = re.sub(r"\bsummary\s*\(\s*([A-Za-z]\w*)\s*\)\s*\$\s*adj\.r\.squared\b", r"\1%adj_r_squared", s)
    s = re.sub(r"\bsummary\s*\(\s*([A-Za-z]\w*)\s*\)\s*\$\s*sigma\b", r"\1%sigma", s)
    s = re.sub(r"\bcoef\s*\(\s*([A-Za-z]\w*)\s*\)", r"\1%coef", s)
    s = re.sub(r"\bresiduals\s*\(\s*([A-Za-z]\w*)\s*\)", r"\1%resid", s)

    def _normalize_numeric_args(inner: str) -> str:
        parts = split_top_level_commas(inner)
        if not parts:
            return inner
        has_nonint = any(not _is_int_literal(p.strip()) for p in parts)
        outp: list[str] = []
        for p in parts:
            t = p.strip()
            if has_nonint and _is_int_literal(t):
                outp.append(f"{t}.0_dp")
            else:
                outp.append(t)
        return ", ".join(outp)

    s = re.sub(r"\bTRUE\b", ".true.", s)
    s = re.sub(r"\bFALSE\b", ".false.", s)
    s = re.sub(r"\b(\d+)[lL]\b", r"\1", s)
    s = s.replace("&&", ".and.")
    s = s.replace("||", ".or.")
    s = re.sub(r"!\s*(?!=)", ".not. ", s)
    s = s.replace("^", "**")
    # ifelse(a,b,c) -> merge(b,c,a)
    s = re.sub(r"\bifelse\s*\((.+?),(.+?),(.+?)\)", r"merge(\2,\3,\1)", s)
    # basic helpers
    s = re.sub(r"\blength\s*\(\s*([A-Za-z]\w*)\s*\)", r"size(\1)", s)
    s = re.sub(r"\bnrow\s*\(\s*([A-Za-z]\w*)\s*\)", r"size(\1, 1)", s)
    s = re.sub(r"\bncol\s*\(\s*([A-Za-z]\w*)\s*\)", r"size(\1, 2)", s)
    s = _replace_balanced_func_calls(s, "as.matrix", lambda inner: inner.strip())
    s = re.sub(r"\bas\.numeric\s*\(\s*([A-Za-z]\w*)\s*\)", r"\1", s)
    s = re.sub(r"\bis\.finite\s*\(\s*([A-Za-z]\w*)\s*\)", r"ieee_is_finite(\1)", s)
    s = re.sub(r"\bis\.null\s*\(\s*([A-Za-z]\w*)\s*\)", r"(\1 == -1)", s)
    s = re.sub(r"\bNULL\b", "-1", s)
    s = re.sub(r"\bNaN\b", "ieee_value(0.0_dp, ieee_quiet_nan)", s)
    s = re.sub(r"\bNA_real_\b", "ieee_value(0.0_dp, ieee_quiet_nan)", s)
    s = _replace_balanced_func_calls(
        s,
        "mean",
        lambda inner: f"(sum({inner})/real(size({inner}), kind=dp))",
    )
    s = _replace_balanced_func_calls(
        s,
        "max",
        lambda inner: (
            f"maxval({_normalize_numeric_args(inner)})"
            if len(split_top_level_commas(inner)) == 1
            else f"max({_normalize_numeric_args(inner)})"
        ),
    )
    s = _replace_balanced_func_calls(
        s,
        "min",
        lambda inner: (
            f"minval({_normalize_numeric_args(inner)})"
            if len(split_top_level_commas(inner)) == 1
            else f"min({_normalize_numeric_args(inner)})"
        ),
    )
    s = _replace_balanced_func_calls(
        s,
        "pmax",
        lambda inner: f"max({_normalize_numeric_args(inner)})",
    )
    s = _replace_balanced_func_calls(
        s,
        "pmin",
        lambda inner: f"min({_normalize_numeric_args(inner)})",
    )
    s = _replace_balanced_func_calls(
        s,
        "dnorm",
        lambda inner: "dnorm(" + re.sub(r"\blog\s*=", "log_=", inner) + ")",
    )
    # c(...) -> [...] (also for nested occurrences).
    def _repl_c(inner: str) -> str:
        parts = split_top_level_commas(inner.strip())
        vals = []
        for p in parts:
            t = p.strip()
            if _is_int_literal(t):
                vals.append(f"{t}.0_dp")
            elif _is_real_literal(t) and "_dp" not in t:
                vals.append(f"{t}_dp")
            else:
                vals.append(t)
        return "[" + ", ".join(vals) + "]"
    s = _replace_balanced_func_calls(s, "c", _repl_c)
    # decorate bare real literals
    s = re.sub(r"(?<![\w.])(\d+\.\d*([eE][+-]?\d+)?|\d+[eE][+-]?\d+)(?![\w.])", r"\1_dp", s)
    # R list member access: a$b$c -> a%b%c
    s = s.replace("$", "%")
    # R indexing: a[1] -> a(1), a%b[2] -> a%b(2)
    idx_pat = re.compile(r"([A-Za-z]\w*(?:%[A-Za-z]\w*)*)\s*\[([^\[\]]+)\]")
    prev = None
    while prev != s:
        prev = s
        s = idx_pat.sub(lambda m: f"{m.group(1)}({m.group(2).strip()})", s)
    # R empty subscript positions: a[,j] -> a(:,j), a[i,] -> a(i,:)
    s = re.sub(r"\(\s*,", "(:,", s)
    s = re.sub(r",\s*\)", ",:)", s)
    return s


class FEmit:
    def __init__(self) -> None:
        self.lines: list[str] = []
        self.ind = 0

    def w(self, s: str = "") -> None:
        self.lines.append(" " * self.ind + s)

    def push(self) -> None:
        self.ind += 3

    def pop(self) -> None:
        self.ind = max(0, self.ind - 3)

    def text(self) -> str:
        return "\n".join(self.lines) + "\n"


def emit_stmts(
    o: FEmit,
    stmts: list[object],
    need_rnorm: dict[str, bool],
    params: set[str],
    alloc_seen: set[str] | None = None,
    helper_ctx: dict[str, object] | None = None,
) -> None:
    if alloc_seen is None:
        alloc_seen = set()
    has_r_mod = bool(helper_ctx and helper_ctx.get("has_r_mod"))
    need_r_mod: set[str] = set()
    lm_terms_by_fit: dict[str, list[str]] = {}
    if helper_ctx is not None:
        nr = helper_ctx.get("need_r_mod")
        if isinstance(nr, set):
            need_r_mod = nr
        lmd = helper_ctx.get("lm_terms_by_fit")
        if isinstance(lmd, dict):
            lm_terms_by_fit = lmd

    def _emit_alloc_1d(name: str, extent: str) -> None:
        if name in alloc_seen:
            o.w(f"if (allocated({name})) deallocate({name})")
        o.w(f"allocate({name}({extent}))")
        alloc_seen.add(name)

    def _wstmt(stmt_line: str, cmt: str) -> None:
        t = (cmt or "").strip()
        if t:
            o.w("! " + t)
        o.w(stmt_line)

    def _rewrite_predict_expr(expr: str) -> str:
        c = parse_call_text(expr.strip())
        if c is None or c[0].lower() != "predict":
            return expr
        _np, posp, kwp = c
        fit_nm = posp[0].strip() if posp else kwp.get("object", "").strip()
        newd = kwp.get("newdata", "").strip()
        if not fit_nm or not newd:
            return expr
        terms = lm_terms_by_fit.get(fit_nm)
        if not terms:
            return expr
        cols = ", ".join(r_expr_to_fortran(f"{newd}_{t}") for t in terms)
        first = r_expr_to_fortran(f"{newd}_{terms[0]}")
        p = len(terms)
        return f"lm_predict_general({fit_nm}, reshape([{cols}], [size({first}), {p}]))"

    for st in stmts:
        if isinstance(st, Assign):
            if st.name in params:
                # Already emitted as named constant parameter.
                continue
            rhs = st.expr.strip()
            c_lm = parse_call_text(rhs)
            if c_lm is not None and c_lm[0].lower() == "lm":
                _nm_lm, pos_lm, kw_lm = c_lm
                form = pos_lm[0].strip() if pos_lm else kw_lm.get("formula", "").strip()
                m_form = re.match(r"^([A-Za-z]\w*)\s*~\s*(.+)$", form)
                if not m_form:
                    raise NotImplementedError("lm requires formula like y ~ x1 + x2 + ... in this subset")
                yv = r_expr_to_fortran(m_form.group(1).strip())
                rhs_terms = m_form.group(2).strip()
                terms = [t.strip() for t in split_top_level_commas(rhs_terms.replace("+", ",")) if t.strip()]
                if not terms:
                    raise NotImplementedError("lm formula requires at least one predictor")
                lm_terms_by_fit[st.name] = terms
                p = len(terms)
                o.w("block")
                o.push()
                o.w("integer :: n_lm, p_lm")
                o.w("real(kind=dp), allocatable :: x_lm(:,:)")
                o.w(f"n_lm = size({yv})")
                o.w(f"p_lm = {p}")
                o.w("allocate(x_lm(n_lm, p_lm))")
                for j, tnm in enumerate(terms, start=1):
                    tv = r_expr_to_fortran(tnm)
                    o.w(f"x_lm(:, {j}) = {tv}")
                o.w(f"{st.name} = lm_fit_general({yv}, x_lm)")
                o.pop()
                o.w("end block")
                if helper_ctx is not None:
                    helper_ctx["need_lm"] = True
                continue
            c_pred = parse_call_text(rhs)
            if c_pred is not None and c_pred[0].lower() == "predict":
                _np, posp, kwp = c_pred
                fit_nm = posp[0].strip() if posp else kwp.get("object", "").strip()
                newd = kwp.get("newdata", "").strip()
                if not fit_nm or not newd:
                    raise NotImplementedError("predict requires object and newdata in this subset")
                terms = lm_terms_by_fit.get(fit_nm)
                if not terms:
                    raise NotImplementedError("predict requires preceding lm fit with known predictor terms")
                p = len(terms)
                o.w("block")
                o.push()
                o.w("integer :: n_pr, p_pr")
                o.w("real(kind=dp), allocatable :: x_pr(:,:)")
                first = r_expr_to_fortran(f"{newd}_{terms[0]}")
                o.w(f"n_pr = size({first})")
                o.w(f"p_pr = {p}")
                o.w("allocate(x_pr(n_pr, p_pr))")
                for j, tnm in enumerate(terms, start=1):
                    tv = r_expr_to_fortran(f"{newd}_{tnm}")
                    o.w(f"x_pr(:, {j}) = {tv}")
                o.w(f"{st.name} = lm_predict_general({fit_nm}, x_pr)")
                o.pop()
                o.w("end block")
                if helper_ctx is not None:
                    helper_ctx["need_lm"] = True
                continue
            m_mat = re.match(r"^matrix\s*\((.*)\)\s*$", rhs, re.IGNORECASE)
            if m_mat:
                cinfo_m = parse_call_text("matrix(" + m_mat.group(1).strip() + ")")
                if cinfo_m is not None:
                    _nmm, posm, kwm = cinfo_m
                    data_src = posm[0] if posm else kwm.get("data", "")
                    nrow_src = kwm.get("nrow")
                    ncol_src = kwm.get("ncol")
                    if nrow_src is None or ncol_src is None:
                        raise NotImplementedError("matrix(...) requires nrow and ncol in this subset")
                    nr_f = _int_bound_expr(r_expr_to_fortran(nrow_src))
                    nc_f = _int_bound_expr(r_expr_to_fortran(ncol_src))
                    d_ci = parse_call_text(data_src.strip())
                    if d_ci is not None and d_ci[0].lower() == "rnorm":
                        nsrc = d_ci[1][0] if d_ci[1] else d_ci[2].get("n")
                        if nsrc is None:
                            raise NotImplementedError("matrix(rnorm(...)) requires n argument")
                        n_f = _int_bound_expr(r_expr_to_fortran(nsrc))
                        o.w("block")
                        o.push()
                        o.w("real(kind=dp), allocatable :: tmp_m(:)")
                        o.w(f"call rnorm_vec({n_f}, tmp_m)")
                        o.w(f"{st.name} = reshape(tmp_m, [{nr_f}, {nc_f}])")
                        o.pop()
                        o.w("end block")
                        need_rnorm["used"] = True
                        continue
                    # Generic fallback for matrix(data, nrow=, ncol=)
                    data_f = r_expr_to_fortran(data_src)
                    o.w(f"{st.name} = reshape({data_f}, [{nr_f}, {nc_f}])")
                    continue
            m_asm_rt = re.match(
                r"^as\.matrix\s*\(\s*\(?\s*read\.table\s*\((.*)\)\s*\)?\s*\)\s*$",
                rhs,
                re.IGNORECASE,
            )
            if m_asm_rt:
                cinfo_rt = parse_call_text("read.table(" + m_asm_rt.group(1).strip() + ")")
                if cinfo_rt is None:
                    raise NotImplementedError("read.table parse failure")
                _nrt, prt, kwrt = cinfo_rt
                if prt:
                    path_src = prt[0]
                elif "file" in kwrt:
                    path_src = kwrt["file"]
                else:
                    raise NotImplementedError("read.table requires file argument in this subset")
                path_f = r_expr_to_fortran(path_src)
                o.w(f"call read_table_real_matrix({path_f}, {st.name})")
                if helper_ctx is not None:
                    helper_ctx["need_table_reader"] = True
                continue
            m_rt = re.match(r"^read\.table\s*\((.*)\)\s*$", rhs, re.IGNORECASE)
            if m_rt:
                cinfo_rt = parse_call_text("read.table(" + m_rt.group(1).strip() + ")")
                if cinfo_rt is None:
                    raise NotImplementedError("read.table parse failure")
                _nrt, prt, kwrt = cinfo_rt
                if prt:
                    path_src = prt[0]
                elif "file" in kwrt:
                    path_src = kwrt["file"]
                else:
                    raise NotImplementedError("read.table requires file argument in this subset")
                path_f = r_expr_to_fortran(path_src)
                o.w(f"call read_table_real_matrix({path_f}, {st.name})")
                if helper_ctx is not None:
                    helper_ctx["need_table_reader"] = True
                continue
            cinfo_rhs = parse_call_text(rhs)
            if cinfo_rhs is not None and cinfo_rhs[0].lower() == "scan":
                _nm, pos, kw = cinfo_rhs
                if pos:
                    path_src = pos[0]
                elif "file" in kw:
                    path_src = kw["file"]
                else:
                    raise NotImplementedError("scan requires file/path argument in this subset")
                path_f = r_expr_to_fortran(path_src)
                o.w(f"call read_real_vector({path_f}, {st.name})")
                if helper_ctx is not None:
                    helper_ctx["need_scan_reader"] = True
                continue
            # Inline rnorm(...) used inside arithmetic expressions.
            m_rn_inline = re.search(r"\brnorm\s*\(([^()]*)\)", rhs, re.IGNORECASE)
            if m_rn_inline:
                rn_call = "rnorm(" + m_rn_inline.group(1).strip() + ")"
                c_rn_i = parse_call_text(rn_call)
                if c_rn_i is not None:
                    _nn, pos_i, kw_i = c_rn_i
                    n_src = pos_i[0] if pos_i else kw_i.get("n", "")
                    n_f = _int_bound_expr(r_expr_to_fortran(n_src))
                    mean_f = r_expr_to_fortran(kw_i.get("mean", "0.0"))
                    sd_f = r_expr_to_fortran(kw_i.get("sd", "1.0"))
                    o.w("block")
                    o.push()
                    o.w("real(kind=dp), allocatable :: rn_tmp(:)")
                    o.w(f"call rnorm_vec({n_f}, rn_tmp)")
                    if mean_f != "0.0_dp" or sd_f != "1.0_dp":
                        o.w(f"rn_tmp = ({mean_f}) + ({sd_f}) * rn_tmp")
                    rhs_i = rhs.replace(rn_call, "rn_tmp")
                    rhs_f_i = r_expr_to_fortran(rhs_i)
                    _wstmt(f"{st.name} = {rhs_f_i}", st.comment)
                    o.pop()
                    o.w("end block")
                    need_rnorm["used"] = True
                    continue
            rhs_f = r_expr_to_fortran(_rewrite_predict_expr(rhs))
            if rhs_f == st.name:
                # identity cast/normalization (e.g. x <- as.numeric(x))
                continue
            m_pack = re.match(rf"^{re.escape(st.name)}\s*\[\s*(.+)\s*\]\s*$", rhs)
            if m_pack:
                inner = m_pack.group(1).strip()
                if ":" in inner:
                    sec = r_expr_to_fortran(inner)
                    o.w(f"{st.name} = {st.name}({sec})")
                else:
                    mask = r_expr_to_fortran(inner)
                    o.w(f"{st.name} = pack({st.name}, {mask})")
                continue
            m_if_runif = re.match(r"^ifelse\s*\(\s*runif\((.+)\)\s*<\s*(.+)\s*,\s*(.+)\s*,\s*(.+)\s*\)\s*$", rhs)
            if m_if_runif:
                n = r_expr_to_fortran(m_if_runif.group(1).strip())
                nb = _int_bound_expr(n)
                p = r_expr_to_fortran(m_if_runif.group(2).strip())
                a = r_expr_to_fortran(m_if_runif.group(3).strip())
                b = r_expr_to_fortran(m_if_runif.group(4).strip())
                if has_r_mod and a == "1" and b == "2":
                    o.w(f"{st.name} = random_choice2_prob({nb}, {p})")
                    need_r_mod.add("random_choice2_prob")
                    continue
                _emit_alloc_1d(st.name, nb)
                o.w("block")
                o.push()
                o.w("integer :: i_rf")
                o.w("real(kind=dp) :: u_rf")
                o.w(f"do i_rf = 1, {nb}")
                o.push()
                o.w("call random_number(u_rf)")
                if _is_simple_value_for_merge(a) and _is_simple_value_for_merge(b):
                    o.w(f"{st.name}(i_rf) = merge({a}, {b}, u_rf < {p})")
                else:
                    o.w(f"if (u_rf < {p}) then")
                    o.push()
                    o.w(f"{st.name}(i_rf) = {a}")
                    o.pop()
                    o.w("else")
                    o.push()
                    o.w(f"{st.name}(i_rf) = {b}")
                    o.pop()
                    o.w("end if")
                o.pop()
                o.w("end do")
                o.pop()
                o.w("end block")
                continue
            cinfo = parse_call_text(rhs)
            if cinfo is not None and cinfo[0].lower() == "sample.int":
                if not has_r_mod:
                    raise NotImplementedError("sample.int requires helper module r_mod")
                _nm, pos, kw = cinfo
                if pos:
                    n_src = pos[0]
                elif "n" in kw:
                    n_src = kw["n"]
                else:
                    raise NotImplementedError("sample.int requires first argument n")
                n_f = _int_bound_expr(r_expr_to_fortran(n_src))
                size_f = _int_bound_expr(r_expr_to_fortran(kw.get("size", n_src)))
                rep_f = r_expr_to_fortran(kw.get("replace", "FALSE"))
                prob_src = kw.get("prob")
                if prob_src is not None:
                    prob_f = r_expr_to_fortran(prob_src)
                    o.w(f"{st.name} = sample_int({n_f}, size_={size_f}, replace={rep_f}, prob={prob_f})")
                else:
                    o.w(f"{st.name} = sample_int({n_f}, size_={size_f}, replace={rep_f})")
                need_r_mod.add("sample_int")
                continue
            if rhs.startswith("runif(") and rhs.endswith(")"):
                c_ru = parse_call_text(rhs)
                if c_ru is not None:
                    _nru, pos_ru, kw_ru = c_ru
                    n_src = pos_ru[0] if pos_ru else kw_ru.get("n", "")
                    n_f = _int_bound_expr(r_expr_to_fortran(n_src))
                    if len(pos_ru) >= 3:
                        a_f = r_expr_to_fortran(pos_ru[1])
                        b_f = r_expr_to_fortran(pos_ru[2])
                    else:
                        a_f = r_expr_to_fortran(kw_ru.get("min", "0.0"))
                        b_f = r_expr_to_fortran(kw_ru.get("max", "1.0"))
                    if has_r_mod:
                        o.w(f"{st.name} = ({a_f}) + (({b_f}) - ({a_f})) * runif_vec({n_f})")
                        need_r_mod.add("runif_vec")
                    else:
                        _emit_alloc_1d(st.name, n_f)
                        o.w(f"call random_number({st.name})")
                        o.w(f"{st.name} = ({a_f}) + (({b_f}) - ({a_f})) * {st.name}")
                    continue
                n = r_expr_to_fortran(rhs[len("runif(") : -1])
                nb = _int_bound_expr(n)
                if has_r_mod:
                    o.w(f"{st.name} = runif_vec({nb})")
                    need_r_mod.add("runif_vec")
                else:
                    _emit_alloc_1d(st.name, nb)
                    o.w(f"call random_number({st.name})")
            elif re.match(r"^rnorm\s*\(", rhs):
                # Special-case: rnorm(n, mean = mu[z], sd = sigma[z])
                m_rmix = re.match(
                    r"^rnorm\s*\(\s*([^,]+)\s*,\s*mean\s*=\s*([A-Za-z]\w*)\s*\[\s*([A-Za-z]\w*)\s*\]\s*,\s*sd\s*=\s*([A-Za-z]\w*)\s*\[\s*([A-Za-z]\w*)\s*\]\s*\)\s*$",
                    rhs,
                )
                if m_rmix:
                    n = r_expr_to_fortran(m_rmix.group(1).strip())
                    nb = _int_bound_expr(n)
                    mu = m_rmix.group(2)
                    z1 = m_rmix.group(3)
                    sd = m_rmix.group(4)
                    z2 = m_rmix.group(5)
                    if z1 != z2:
                        raise NotImplementedError("rnorm mean/sd index variables must match")
                    z = z1
                    if has_r_mod:
                        o.w(f"{st.name} = {mu}({z}) + {sd}({z}) * rnorm_vec({nb})")
                        need_r_mod.add("rnorm_vec")
                        continue
                    _emit_alloc_1d(st.name, nb)
                    o.w("block")
                    o.push()
                    o.w("integer :: i_rg, k_rg")
                    o.w("real(kind=dp) :: u1_rg, u2_rg, g_rg")
                    o.w(f"do i_rg = 1, {nb}")
                    o.push()
                    o.w("call random_number(u1_rg)")
                    o.w("call random_number(u2_rg)")
                    o.w("if (u1_rg <= tiny(1.0_dp)) cycle")
                    o.w("g_rg = sqrt(-2.0_dp * log(u1_rg)) * cos(2.0_dp * acos(-1.0_dp) * u2_rg)")
                    o.w(f"k_rg = int({z}(i_rg))")
                    o.w(f"{st.name}(i_rg) = {mu}(k_rg) + {sd}(k_rg) * g_rg")
                    o.pop()
                    o.w("end do")
                    o.pop()
                    o.w("end block")
                    continue
                # fallback simple rnorm(n)
                m_rn = re.match(r"^rnorm\s*\(\s*([^)]+)\s*\)\s*$", rhs)
                if m_rn:
                    n = r_expr_to_fortran(m_rn.group(1))
                    if has_r_mod:
                        o.w(f"{st.name} = rnorm_vec({_int_bound_expr(n)})")
                        need_r_mod.add("rnorm_vec")
                    else:
                        o.w(f"call rnorm_vec({_int_bound_expr(n)}, {st.name})")
                        need_rnorm["used"] = True
                    continue
                c_rn = parse_call_text(rhs)
                if c_rn is not None:
                    _nrn, pos_rn, kw_rn = c_rn
                    n_src = pos_rn[0] if pos_rn else kw_rn.get("n", "")
                    n_f = _int_bound_expr(r_expr_to_fortran(n_src))
                    mean_f = r_expr_to_fortran(kw_rn.get("mean", "0.0"))
                    sd_f = r_expr_to_fortran(kw_rn.get("sd", "1.0"))
                    if has_r_mod:
                        o.w(f"{st.name} = ({mean_f}) + ({sd_f}) * rnorm_vec({n_f})")
                        need_r_mod.add("rnorm_vec")
                    else:
                        o.w(f"call rnorm_vec({n_f}, {st.name})")
                        o.w(f"{st.name} = ({mean_f}) + ({sd_f}) * {st.name}")
                        need_rnorm["used"] = True
                    continue
                raise NotImplementedError(f"unsupported rnorm form: {rhs}")
            elif rhs.startswith("rnorm(") and rhs.endswith(")"):
                n = r_expr_to_fortran(rhs[len("rnorm(") : -1])
                if has_r_mod:
                    o.w(f"{st.name} = rnorm_vec({_int_bound_expr(n)})")
                    need_r_mod.add("rnorm_vec")
                else:
                    o.w(f"call rnorm_vec({_int_bound_expr(n)}, {st.name})")
                    need_rnorm["used"] = True
            else:
                _wstmt(f"{st.name} = {rhs_f}", st.comment)
        elif isinstance(st, PrintStmt):
            if st.args:
                if len(st.args) == 1:
                    one = st.args[0].strip()
                    m_sum = re.match(r"^summary\s*\(\s*([A-Za-z]\w*)\s*\)\s*$", one, re.IGNORECASE)
                    if m_sum:
                        _wstmt(f"call print_lm_summary({m_sum.group(1)})", st.comment)
                        if helper_ctx is not None:
                            helper_ctx["need_lm"] = True
                        continue
                    m_coef = re.match(r"^coef\s*\(\s*([A-Za-z]\w*)\s*\)\s*$", one, re.IGNORECASE)
                    if m_coef:
                        fit_nm = m_coef.group(1)
                        terms = lm_terms_by_fit.get(fit_nm, [])
                        if terms:
                            max_len = max(1, max(len(t) for t in terms))
                            terms_lit = ", ".join(f'"{t}"' for t in terms)
                            _wstmt(
                                f'call print_lm_coef_rstyle({fit_nm}, [character(len={max_len}) :: {terms_lit}])',
                                st.comment,
                            )
                        else:
                            _wstmt(f"call print_lm_coef_rstyle({fit_nm})", st.comment)
                        if helper_ctx is not None:
                            helper_ctx["need_lm"] = True
                        continue
                _wstmt("print *, " + ", ".join(r_expr_to_fortran(_rewrite_predict_expr(a)) for a in st.args), st.comment)
            else:
                _wstmt("print *", st.comment)
        elif isinstance(st, CallStmt):
            nm = st.name.lower()
            if nm == "stop":
                if st.args:
                    msg = _dequote_string_literal(st.args[0].strip())
                    if msg is None:
                        msg = st.args[0].strip()
                    o.w(f"error stop {_fortran_error_msg(str(msg))}")
                else:
                    o.w('error stop "stop requested"')
                continue
            if nm == "stopifnot":
                if not st.args:
                    continue
                for a in st.args:
                    cond = fscan.strip_redundant_outer_parens_expr(r_expr_to_fortran(a))
                    neg = _negate_simple_relational_expr(cond)
                    if neg is not None:
                        msg = _fortran_error_msg(f"error: need {cond}")
                        o.w(f"if ({neg}) error stop {msg}")
                    else:
                        msg = _fortran_error_msg(f"error: need {cond}")
                        o.w(f"if (.not. ({cond})) error stop {msg}")
                continue
            if nm == "set.seed":
                # First pass: set RNG to deterministic state when called.
                _wstmt("call random_seed()", st.comment)
                continue
            if nm == "cat":
                if st.args:
                    out_items: list[str] = []
                    for a in st.args:
                        at = a.strip()
                        if at in {'"\\n"', "'\\n'"}:
                            continue
                        sp_items = _sprintf_arg_items(at)
                        if sp_items is not None:
                            out_items.extend(sp_items)
                            continue
                        lit = _dequote_string_literal(at)
                        if lit is not None:
                            lit2 = lit.replace("\\n", "").replace("\\t", " ")
                            if lit2.endswith("="):
                                lit2 = lit2 + " "
                            if lit2.endswith(":"):
                                lit2 = lit2 + " "
                            if lit2:
                                out_items.append(_fortran_str_literal(lit2))
                            continue
                        out_items.append(_display_expr_to_fortran(a))
                    if out_items:
                        _wstmt("print *, " + ", ".join(out_items), st.comment)
                    else:
                        _wstmt("print *", st.comment)
                else:
                    _wstmt("print *", st.comment)
                continue
            if nm == "writelines":
                call_text = f"{st.name}(" + ", ".join(st.args) + ")"
                cinfo = parse_call_text(call_text)
                if cinfo is None:
                    raise NotImplementedError("writeLines parse failure")
                _nmc, pos, kw = cinfo
                if pos:
                    data_src = pos[0]
                elif "text" in kw:
                    data_src = kw["text"]
                else:
                    raise NotImplementedError("writeLines requires text/data argument")
                con_src = kw.get("con", '"out.txt"')
                data_f = r_expr_to_fortran(data_src)
                write_fmt = "(g0.17)"
                fmt_ci = parse_call_text(data_src)
                if fmt_ci is not None and fmt_ci[0].lower() == "format":
                    _fnm_fmt, pos_fmt, kw_fmt = fmt_ci
                    if pos_fmt:
                        data_f = r_expr_to_fortran(pos_fmt[0])
                    dsrc = kw_fmt.get("digits")
                    if dsrc is not None and _is_int_literal(dsrc.strip()):
                        d = int(dsrc.strip())
                        if d < 1:
                            d = 1
                        if d > 30:
                            d = 30
                        write_fmt = f"(g0.{d})"
                con_f = r_expr_to_fortran(con_src)
                o.w("block")
                o.push()
                o.w("integer :: fp, i_wl")
                o.w(f'open(newunit=fp, file={con_f}, status="replace", action="write")')
                o.w(f"if (size({data_f}) > 0) then")
                o.push()
                o.w(f"do i_wl = 1, size({data_f})")
                o.push()
                o.w(f'write(fp, "{write_fmt}") {data_f}(i_wl)')
                o.pop()
                o.w("end do")
                o.pop()
                o.w("end if")
                o.w("close(fp)")
                o.pop()
                o.w("end block")
                continue
            if nm == "write.table":
                call_text = f"{st.name}(" + ", ".join(st.args) + ")"
                cinfo = parse_call_text(call_text)
                if cinfo is None:
                    raise NotImplementedError("write.table parse failure")
                _nmc, pos, kw = cinfo
                if pos:
                    data_src = pos[0]
                elif "x" in kw:
                    data_src = kw["x"]
                else:
                    raise NotImplementedError("write.table requires first argument x")
                file_src = kw.get("file")
                if file_src is None:
                    raise NotImplementedError("write.table requires file= argument")
                data_f = r_expr_to_fortran(data_src)
                file_f = r_expr_to_fortran(file_src)
                o.w(f"call write_table_real_matrix({file_f}, {data_f})")
                if helper_ctx is not None:
                    helper_ctx["need_table_writer"] = True
                continue
            raise NotImplementedError(f"unsupported call statement: {st.name}")
        elif isinstance(st, ForStmt):
            it = st.iter_expr.strip()
            m_colon = re.match(r"^(.+):(.+)$", it)
            m_seq_len = re.match(r"^seq_len\s*\((.+)\)$", it)
            if m_seq_len:
                n = r_expr_to_fortran(m_seq_len.group(1).strip())
                o.w(f"do {st.var} = 1, {_int_bound_expr(n)}")
            elif m_colon:
                a = r_expr_to_fortran(m_colon.group(1).strip())
                b = r_expr_to_fortran(m_colon.group(2).strip())
                o.w(f"do {st.var} = int({a}), int({b})")
            elif re.match(r"^[A-Za-z]\w*$", it):
                arr = it
                idx = f"i_{st.var}"
                o.w("block")
                o.push()
                o.w(f"integer :: {idx}")
                o.w(f"do {idx} = 1, size({arr})")
                o.push()
                o.w(f"{st.var} = {arr}({idx})")
                emit_stmts(o, st.body, need_rnorm, params, alloc_seen, helper_ctx)
                o.pop()
                o.w("end do")
                o.pop()
                o.w("end block")
                continue
            else:
                raise NotImplementedError(f"unsupported for iterator: {it}")
            o.push()
            emit_stmts(o, st.body, need_rnorm, params, alloc_seen, helper_ctx)
            o.pop()
            o.w("end do")
        elif isinstance(st, IfStmt):
            # Prefer MERGE for simple same-target conditional assignment.
            if (
                len(st.then_body) == 1
                and len(st.else_body) == 1
                and isinstance(st.then_body[0], Assign)
                and isinstance(st.else_body[0], Assign)
            ):
                a_then = st.then_body[0]
                a_else = st.else_body[0]
                if a_then.name == a_else.name:
                    rhs_t = r_expr_to_fortran(a_then.expr)
                    rhs_e = r_expr_to_fortran(a_else.expr)
                    if _is_simple_value_for_merge(rhs_t) and _is_simple_value_for_merge(rhs_e):
                        cond_f = r_expr_to_fortran(st.cond)
                        o.w(f"{a_then.name} = merge({rhs_t}, {rhs_e}, {cond_f})")
                        continue

            o.w(f"if ({r_expr_to_fortran(st.cond)}) then")
            o.push()
            emit_stmts(o, st.then_body, need_rnorm, params, alloc_seen, helper_ctx)
            o.pop()
            if st.else_body:
                o.w("else")
                o.push()
                emit_stmts(o, st.else_body, need_rnorm, params, alloc_seen, helper_ctx)
                o.pop()
            o.w("end if")
        elif isinstance(st, ExprStmt):
            if st.expr.strip() == "break":
                o.w("exit")
                continue
            if st.expr.strip() == "next":
                o.w("cycle")
                continue
            m_asn = re.match(r"^(.+?)\s*(<-|=)\s*(.+)$", st.expr.strip())
            if m_asn:
                lhs = r_expr_to_fortran(m_asn.group(1).strip())
                rhs = r_expr_to_fortran(m_asn.group(3).strip())
                _wstmt(f"{lhs} = {rhs}", st.comment)
                continue
            raise NotImplementedError(f"unsupported expression statement: {st.expr}")
        else:
            raise NotImplementedError(f"unsupported statement: {type(st).__name__}")


def _expr_kind_simple(expr: str) -> str:
    t = expr.strip()
    if _is_int_literal(t):
        return "int"
    if _is_real_literal(t):
        return "real"
    if t in {"TRUE", "FALSE"}:
        return "logical"
    return "real"


def emit_function(
    o: FEmit,
    fn: FuncDef,
    list_specs: dict[str, ListReturnSpec],
    helper_ctx: dict[str, object] | None = None,
) -> bool:
    if not fn.body:
        raise NotImplementedError(f"empty function body not supported: {fn.name}")
    last = fn.body[-1]
    if not isinstance(last, ExprStmt):
        raise NotImplementedError(f"function '{fn.name}' requires final expression return in this subset")
    list_spec = list_specs.get(fn.name)
    need_rnorm_local = {"used": False}
    can_be_pure = not _stmt_tree_has_side_effect_ops(fn.body[:-1])

    rk = _expr_kind_simple(last.expr)
    rdecl = "real(kind=dp)"
    if list_spec is None:
        if rk == "int":
            rdecl = "integer"
        elif rk == "logical":
            rdecl = "logical"
    else:
        rdecl = f"type({_type_name_for_path(fn.name, ())})"
    rname = f"{fn.name}_result"
    pref = "pure " if can_be_pure else ""
    o.w(f"{pref}function {fn.name}({', '.join(fn.args)}) result({rname})")
    # argument declarations (first-pass heuristics)
    arg_rank = {a: infer_arg_rank(fn, a) for a in fn.args}
    written_args = infer_written_args(fn)
    arg_type: dict[str, str] = {}
    arg_local_map: dict[str, str] = {}
    arg_local_decl_lines: list[str] = []
    arg_local_init_lines: list[str] = []
    for a in fn.args:
        dflt = fn.defaults.get(a, "")
        intent = "in"
        if a in {"n", "seed", "max_iter", "it"}:
            o.w(f"integer, intent(in) :: {a}")
            arg_type[a] = "integer"
            continue
        if arg_rank.get(a, 0) >= 1:
            o.w(f"real(kind=dp), intent({intent}) :: {a}(:)")
            arg_type[a] = "real_array"
            continue
        if dflt.startswith("c("):
            o.w(f"real(kind=dp), intent({intent}) :: {a}(:)")
            arg_type[a] = "real_array"
        elif dflt.strip().upper() == "NULL":
            o.w(f"integer, intent(in) :: {a}")
            arg_type[a] = "integer"
        elif _is_int_literal(dflt):
            o.w(f"integer, intent(in) :: {a}")
            arg_type[a] = "integer"
        elif dflt in {"TRUE", "FALSE"}:
            o.w(f"logical, intent(in) :: {a}")
            arg_type[a] = "logical"
        else:
            o.w(f"real(kind=dp), intent({intent}) :: {a}")
            arg_type[a] = "real"
    o.w(f"{rdecl} :: {rname}")

    for a in fn.args:
        if a not in written_args:
            continue
        loc = f"{a}_wrk"
        arg_local_map[a] = loc
        t = arg_type.get(a, "real")
        if t == "integer":
            arg_local_decl_lines.append(f"integer :: {loc}")
            arg_local_init_lines.append(f"{loc} = {a}")
        elif t == "logical":
            arg_local_decl_lines.append(f"logical :: {loc}")
            arg_local_init_lines.append(f"{loc} = {a}")
        elif t == "real_array":
            arg_local_decl_lines.append(f"real(kind=dp), allocatable :: {loc}(:)")
            arg_local_init_lines.append(f"{loc} = {a}")
        else:
            arg_local_decl_lines.append(f"real(kind=dp) :: {loc}")
            arg_local_init_lines.append(f"{loc} = {a}")

    for ln in arg_local_decl_lines:
        o.w(ln)

    body_no_ret = fn.body[:-1]
    body_use = [_rename_stmt_obj(st, arg_local_map) for st in body_no_ret] if arg_local_map else body_no_ret
    if body_no_ret:
        hoisted_checks: list[object] = []
        body_rest: list[object] = []
        allowed_names = {a.lower() for a in fn.args} | {v.lower() for v in arg_local_map.values()}
        for st in body_use:
            if _is_hoistable_stopifnot_stmt(st, allowed_names):
                hoisted_checks.append(st)
            else:
                body_rest.append(st)

        known_arrays = {a for a in fn.args if arg_rank.get(a, 0) >= 1}
        known_arrays |= {arg_local_map[a] for a in fn.args if arg_rank.get(a, 0) >= 1 and a in arg_local_map}
        ints, real_scalars, int_arrays, real_arrays, params = classify_vars(
            body_use, infer_assigned_names(body_use), known_arrays=known_arrays
        )
        for a in fn.args:
            ints.discard(a)
            real_scalars.discard(a)
            int_arrays.discard(a)
            real_arrays.discard(a)
            params.pop(a, None)
        for loc in arg_local_map.values():
            ints.discard(loc)
            real_scalars.discard(loc)
            int_arrays.discard(loc)
            real_arrays.discard(loc)
            params.pop(loc, None)
        for p, v in sorted(params.items()):
            o.w(f"integer, parameter :: {p} = {v}")
        if ints:
            o.w("integer :: " + ", ".join(sorted(ints)))
        if int_arrays:
            o.w("integer, allocatable :: " + ", ".join(f"{x}(:)" for x in sorted(int_arrays)))
        if real_arrays:
            o.w("real(kind=dp), allocatable :: " + ", ".join(f"{x}(:)" for x in sorted(real_arrays)))
        if real_scalars:
            o.w("real(kind=dp) :: " + ", ".join(sorted(real_scalars)))
        for ln in arg_local_init_lines:
            o.w(ln)
        emit_stmts(o, hoisted_checks + body_rest, need_rnorm_local, set(params.keys()), helper_ctx=helper_ctx)
    elif arg_local_init_lines:
        for ln in arg_local_init_lines:
            o.w(ln)

    if list_spec is None:
        ret_expr = _replace_idents(last.expr, arg_local_map) if arg_local_map else last.expr
        o.w(f"{rname} = {r_expr_to_fortran(ret_expr)}")
    else:
        def _emit_assign(prefix: str, fields: dict[str, object]) -> None:
            for k, v in fields.items():
                if isinstance(v, dict):
                    _emit_assign(f"{prefix}%{k}", v)
                else:
                    vv = _replace_idents(str(v), arg_local_map) if arg_local_map else str(v)
                    o.w(f"{prefix}%{k} = {r_expr_to_fortran(vv)}")
        _emit_assign(rname, list_spec.root_fields)
    o.w(f"end function {fn.name}")
    return bool(need_rnorm_local["used"])


def infer_function_integer_names(fn: FuncDef) -> set[str]:
    """Infer names that are likely integer-typed within one function scope."""
    ints: set[str] = set()
    for a in fn.args:
        dflt = fn.defaults.get(a, "").strip()
        if a in {"n", "seed", "max_iter", "it"} or _is_int_literal(dflt) or dflt.upper() == "NULL":
            ints.add(a)
    body_no_ret = fn.body[:-1] if fn.body else []
    if body_no_ret:
        known_arrays = {a for a in fn.args if infer_arg_rank(fn, a) >= 1}
        b_ints, _b_real_scalars, _b_int_arrays, _b_real_arrays, b_params = classify_vars(
            body_no_ret, infer_assigned_names(body_no_ret), known_arrays=known_arrays
        )
        ints.update(b_ints)
        ints.update(b_params.keys())
    return ints


def infer_function_integer_array_names(fn: FuncDef) -> set[str]:
    """Infer local names that are likely integer arrays within one function scope."""
    int_arrays: set[str] = set()
    body_no_ret = fn.body[:-1] if fn.body else []
    if body_no_ret:
        known_arrays = {a for a in fn.args if infer_arg_rank(fn, a) >= 1}
        _b_ints, _b_real_scalars, b_int_arrays, _b_real_arrays, _b_params = classify_vars(
            body_no_ret, infer_assigned_names(body_no_ret), known_arrays=known_arrays
        )
        int_arrays.update(b_int_arrays)
    return int_arrays


def _rewrite_named_calls(
    expr: str,
    fn_arg_order: dict[str, list[str]],
    fn_arg_defaults: dict[str, dict[str, str]],
) -> str:
    cinfo = parse_call_text(expr)
    if cinfo is None:
        return expr
    nm, pos, kw = cinfo
    order = fn_arg_order.get(nm)
    if order is None or not kw:
        return expr
    defaults = fn_arg_defaults.get(nm, {})
    vals: list[str] = []
    ip = 0
    for anm in order:
        if ip < len(pos):
            vals.append(pos[ip])
            ip += 1
        elif anm in kw:
            vals.append(f"{anm} = {kw[anm]}")
        elif anm in defaults:
            vals.append(f"{anm} = {defaults[anm]}")
        else:
            # keep placeholder name if not provided
            vals.append(anm)
    return f"{nm}(" + ", ".join(vals) + ")"


def _fortran_ident(name: str) -> str:
    s = re.sub(r"[^A-Za-z0-9_]", "_", name.strip())
    if not s:
        s = "main"
    if not re.match(r"[A-Za-z]", s[0]):
        s = "x_" + s
    return s


def _module_name_from_stem(stem: str) -> str:
    base = _fortran_ident(stem)
    if base.lower().startswith("x") and len(base) > 1:
        base = base[1:]
    return _fortran_ident(base + "_mod")


def _infer_literal_array_parameter(rhs: str) -> tuple[str, int, str] | None:
    """Infer array-parameter declaration info from a literal constructor RHS.

    Returns `(kind, n, expr_f)` where kind is `"integer"` or `"real"`.
    """
    expr_f = r_expr_to_fortran(rhs.strip())
    t = expr_f.strip()
    if not (t.startswith("[") and t.endswith("]")):
        return None
    inner = t[1:-1].strip()
    if not inner:
        return None
    vals = [x.strip() for x in split_top_level_commas(inner) if x.strip()]
    if not vals:
        return None
    all_int = True
    all_num = True
    for v in vals:
        if _is_int_literal(v):
            continue
        if _is_real_literal(v):
            all_int = False
            continue
        all_num = False
        break
    if not all_num:
        return None
    kind = "integer" if all_int else "real"
    return kind, len(vals), expr_f


def infer_main_array_params(stmts: list[object], assign_counts: dict[str, int]) -> dict[str, tuple[str, int, str]]:
    """Find conservative top-level named-constant array candidates."""
    out: dict[str, tuple[str, int, str]] = {}
    for st in stmts:
        if not isinstance(st, Assign):
            continue
        if assign_counts.get(st.name, 0) != 1:
            continue
        rhs = st.expr.strip()
        if not (rhs.startswith("c(") or rhs.startswith("[") or rhs.startswith("array(")):
            continue
        info = _infer_literal_array_parameter(rhs)
        if info is None:
            continue
        out[st.name] = info
    return out


def infer_main_character_scalars(stmts: list[object]) -> set[str]:
    """Find scalar vars assigned from quoted string literals in main statements."""
    out: set[str] = set()
    for st in stmts:
        if not isinstance(st, Assign):
            continue
        rhs = st.expr.strip()
        if _dequote_string_literal(rhs) is not None:
            out.add(st.name)
    return out


def infer_main_real_matrices(stmts: list[object]) -> set[str]:
    """Find variables that should be declared as rank-2 real allocatables."""
    out: set[str] = set()

    def _scan_text(txt: str) -> None:
        for m in re.finditer(r"\b(?:nrow|ncol)\s*\(\s*([A-Za-z]\w*)\s*\)", txt):
            out.add(m.group(1))
        m_wr = re.match(r"^\s*write\.table\s*\((.*)\)\s*$", txt, re.IGNORECASE)
        if m_wr:
            cinfo = parse_call_text("write.table(" + m_wr.group(1).strip() + ")")
            if cinfo is not None:
                _nm, pos, _kw = cinfo
                if pos:
                    p0 = pos[0].strip()
                    if re.match(r"^[A-Za-z]\w*$", p0):
                        out.add(p0)

    for st in stmts:
        if isinstance(st, Assign):
            rhs = st.expr.strip()
            low = rhs.lower()
            if low.startswith("matrix("):
                out.add(st.name)
            if low.startswith("read.table("):
                out.add(st.name)
            if "read.table(" in low and "as.matrix(" in low:
                out.add(st.name)
            _scan_text(rhs)
        elif isinstance(st, CallStmt):
            _scan_text(f"{st.name}(" + ", ".join(st.args) + ")")
        elif isinstance(st, ExprStmt):
            _scan_text(st.expr)
        elif isinstance(st, IfStmt):
            _scan_text(st.cond)
            for b in st.then_body + st.else_body:
                if isinstance(b, (Assign, CallStmt, ExprStmt)):
                    _scan_text(b.expr if isinstance(b, ExprStmt) else (b.name + "(" + ", ".join(b.args) + ")" if isinstance(b, CallStmt) else b.expr))
    return out


def expand_data_frame_assignments(stmts: list[object]) -> list[object]:
    """Expand `a <- data.frame(x1=..., x2=...)` into scalar/array assignments.

    Emits assignments to `a_x1`, `a_x2`, ... so later calls can reference fields.
    """
    out: list[object] = []
    for st in stmts:
        if not isinstance(st, Assign):
            out.append(st)
            continue
        rhs = st.expr.strip()
        cinfo = parse_call_text(rhs)
        if cinfo is None or cinfo[0].lower() != "data.frame":
            out.append(st)
            continue
        _nm, _pos, kw = cinfo
        if not kw:
            # Keep fallback if shape is not understood.
            out.append(st)
            continue
        for k, v in kw.items():
            out.append(Assign(name=f"{st.name}_{k}", expr=v, comment=st.comment))
    return out


def transpile_r_to_fortran(src: str, stem: str, helper_modules: set[str] | None = None) -> str:
    unit_name = _fortran_ident(stem)
    module_name = _module_name_from_stem(stem)
    comment_lookup = build_r_comment_lookup(src)
    lines = preprocess_r_lines(src)
    stmts, i = parse_block(lines, 0, comment_lookup=comment_lookup)
    if i != len(lines):
        raise NotImplementedError("could not parse full source")

    funcs = [s for s in stmts if isinstance(s, FuncDef)]
    main_stmts = [s for s in stmts if not isinstance(s, FuncDef)]
    main_stmts = expand_data_frame_assignments(main_stmts)
    fn_arg_order = {f.name: list(f.args) for f in funcs}
    fn_arg_defaults = {f.name: dict(f.defaults) for f in funcs}
    for f in funcs:
        for st in f.body:
            if isinstance(st, Assign):
                st.expr = _rewrite_named_calls(st.expr, fn_arg_order, fn_arg_defaults)
            elif isinstance(st, ExprStmt):
                st.expr = _rewrite_named_calls(st.expr, fn_arg_order, fn_arg_defaults)
    for st in main_stmts:
        if isinstance(st, Assign):
            st.expr = _rewrite_named_calls(st.expr, fn_arg_order, fn_arg_defaults)
        elif isinstance(st, ExprStmt):
            st.expr = _rewrite_named_calls(st.expr, fn_arg_order, fn_arg_defaults)
    main_stmts = inline_single_use_temporaries(main_stmts)
    list_specs = _list_return_specs(funcs)
    fn_int_names: dict[str, set[str]] = {f.name: infer_function_integer_names(f) for f in funcs}
    fn_int_array_names: dict[str, set[str]] = {f.name: infer_function_integer_array_names(f) for f in funcs}
    helper_modules = set(m.lower() for m in (helper_modules or set()))
    helper_ctx_mod: dict[str, object] = {
        "has_r_mod": ("r_mod" in helper_modules),
        "need_r_mod": set(),
        "need_lm": False,
        "lm_terms_by_fit": {},
    }
    helper_ctx_main: dict[str, object] = {
        "has_r_mod": ("r_mod" in helper_modules),
        "need_r_mod": set(),
        "need_scan_reader": False,
        "need_table_reader": False,
        "need_table_writer": False,
        "need_lm": False,
        "lm_terms_by_fit": {},
    }

    assign_counts = infer_assigned_names(main_stmts)
    ints, real_scalars, int_arrays, real_arrays, params = classify_vars(main_stmts, assign_counts)
    array_params = infer_main_array_params(main_stmts, assign_counts)
    char_scalars = infer_main_character_scalars(main_stmts)
    real_matrices = infer_main_real_matrices(main_stmts)

    # Main program declarations/body (without header/footer).
    pbody = FEmit()
    int_param_pairs: list[tuple[str, str]] = sorted((p, v) for p, v in params.items())
    # Reuse named size constants when multiple array-parameters share extent.
    size_groups: dict[int, list[str]] = {}
    for nm, (_knd, nsz, _expr_f) in array_params.items():
        size_groups.setdefault(nsz, []).append(nm)
    used_names = set(params.keys()) | set(array_params.keys()) | ints | real_scalars | int_arrays | real_arrays
    size_name_for_n: dict[int, str] = {}
    for nsz, names in sorted(size_groups.items()):
        if len(names) < 2:
            continue
        base = "n_param"
        cand = base
        k = 2
        while cand in used_names:
            cand = f"{base}_{k}"
            k += 1
        used_names.add(cand)
        size_name_for_n[nsz] = cand
        int_param_pairs.append((cand, str(nsz)))
    if int_param_pairs:
        rhs = ", ".join(f"{k} = {v}" for k, v in int_param_pairs)
        pbody.w(f"integer, parameter :: {rhs}")
    for nm, (knd, nsz, expr_f) in sorted(array_params.items()):
        n_decl = size_name_for_n.get(nsz, str(nsz))
        if knd == "integer":
            pbody.w(f"integer, parameter :: {nm}({n_decl}) = {expr_f}")
        else:
            pbody.w(f"real(kind=dp), parameter :: {nm}({n_decl}) = {expr_f}")

    # Variables assigned from list-return function calls.
    list_vars: dict[str, str] = {}
    lm_vars: set[str] = set()
    call_pat = re.compile(r"^([A-Za-z]\w*)\s*\(")
    for st in main_stmts:
        if isinstance(st, Assign):
            if re.match(
                r"^lm\s*\(\s*([A-Za-z]\w*)\s*~\s*([A-Za-z]\w*)\s*\+\s*([A-Za-z]\w*)(?:\s*,\s*.*)?\)\s*$",
                st.expr.strip(),
                re.IGNORECASE,
            ):
                lm_vars.add(st.name)
                helper_ctx_main["need_lm"] = True
            m = call_pat.match(st.expr.strip())
            if not m:
                continue
            fnm = m.group(1)
            if fnm in list_specs:
                list_vars[st.name] = _type_name_for_path(fnm, ())
    for nm in list_vars:
        ints.discard(nm)
        real_scalars.discard(nm)
        int_arrays.discard(nm)
        real_arrays.discard(nm)
    for nm in lm_vars:
        ints.discard(nm)
        real_scalars.discard(nm)
        int_arrays.discard(nm)
        real_arrays.discard(nm)
        params.pop(nm, None)
    for nm in array_params:
        ints.discard(nm)
        real_scalars.discard(nm)
        int_arrays.discard(nm)
        real_arrays.discard(nm)
    for nm in char_scalars:
        ints.discard(nm)
        real_scalars.discard(nm)
        int_arrays.discard(nm)
        real_arrays.discard(nm)
        params.pop(nm, None)
    for nm in real_matrices:
        ints.discard(nm)
        real_scalars.discard(nm)
        int_arrays.discard(nm)
        real_arrays.discard(nm)
        params.pop(nm, None)
    if ints:
        pbody.w("integer :: " + ", ".join(sorted(ints)))
    if int_arrays:
        pbody.w("integer, allocatable :: " + ", ".join(f"{x}(:)" for x in sorted(int_arrays)))
    if real_arrays:
        pbody.w("real(kind=dp), allocatable :: " + ", ".join(f"{x}(:)" for x in sorted(real_arrays)))
    if real_scalars:
        pbody.w("real(kind=dp) :: " + ", ".join(sorted(real_scalars)))
    if real_matrices:
        pbody.w("real(kind=dp), allocatable :: " + ", ".join(f"{x}(:,:)" for x in sorted(real_matrices)))
    if char_scalars:
        pbody.w("character(len=:), allocatable :: " + ", ".join(sorted(char_scalars)))
    if list_vars:
        for nm, tn in sorted(list_vars.items()):
            pbody.w(f"type({tn}) :: {nm}")
    if lm_vars:
        for nm in sorted(lm_vars):
            pbody.w(f"type(lm_fit_t) :: {nm}")
    pbody.w("")

    need_rnorm_main = {"used": False}
    emit_stmts(
        pbody,
        main_stmts,
        need_rnorm_main,
        set(params.keys()) | set(array_params.keys()),
        helper_ctx=helper_ctx_main,
    )

    # Module procedures body (without header/footer).
    mprocs = FEmit()
    fn_needs_rnorm = False
    for fn in funcs:
        fn_needs_rnorm = emit_function(mprocs, fn, list_specs, helper_ctx=helper_ctx_mod) or fn_needs_rnorm
        mprocs.w("")
    has_r_mod_main = bool(helper_ctx_main.get("has_r_mod"))
    emit_local_rnorm = (need_rnorm_main["used"] or fn_needs_rnorm) and (not has_r_mod_main)
    if emit_local_rnorm:
        mprocs.w("subroutine rnorm_vec(n, x)")
        mprocs.w("integer, intent(in) :: n")
        mprocs.w("real(kind=dp), allocatable, intent(inout) :: x(:)")
        mprocs.w("integer :: i")
        mprocs.w("real(kind=dp) :: u1, u2, r, t")
        mprocs.w("if (allocated(x)) deallocate(x)")
        mprocs.w("allocate(x(n))")
        mprocs.w("i = 1")
        mprocs.w("do while (i <= n)")
        mprocs.push()
        mprocs.w("call random_number(u1)")
        mprocs.w("call random_number(u2)")
        mprocs.w("if (u1 <= tiny(1.0_dp)) cycle")
        mprocs.w("r = sqrt(-2.0_dp * log(u1))")
        mprocs.w("t = 2.0_dp * acos(-1.0_dp) * u2")
        mprocs.w("x(i) = r * cos(t)")
        mprocs.w("if (i + 1 <= n) x(i + 1) = r * sin(t)")
        mprocs.w("i = i + 2")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("end subroutine rnorm_vec")
        mprocs.w("")
    emit_local_scan_reader = bool(helper_ctx_main.get("need_scan_reader")) and (not has_r_mod_main)
    if emit_local_scan_reader:
        mprocs.w("subroutine read_real_vector(file_path, x)")
        mprocs.w("character(len=*), intent(in) :: file_path")
        mprocs.w("real(kind=dp), allocatable, intent(out) :: x(:)")
        mprocs.w("integer :: fp, ios, n, cap, new_cap")
        mprocs.w("real(kind=dp) :: v")
        mprocs.w("n = 0")
        mprocs.w("cap = 0")
        mprocs.w('open(newunit=fp, file=file_path, status="old", action="read")')
        mprocs.w("do")
        mprocs.push()
        mprocs.w("read(fp, *, iostat=ios) v")
        mprocs.w("if (ios /= 0) exit")
        mprocs.w("if (n == cap) then")
        mprocs.push()
        mprocs.w("new_cap = merge(1024, 2 * cap, cap == 0)")
        mprocs.w("block")
        mprocs.push()
        mprocs.w("real(kind=dp), allocatable :: tmp(:)")
        mprocs.w("allocate(tmp(new_cap))")
        mprocs.w("if (allocated(x) .and. n > 0) tmp(1:n) = x(1:n)")
        mprocs.w("call move_alloc(tmp, x)")
        mprocs.pop()
        mprocs.w("end block")
        mprocs.w("cap = new_cap")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("n = n + 1")
        mprocs.w("x(n) = v")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("close(fp)")
        mprocs.w("if (n == 0) then")
        mprocs.push()
        mprocs.w("allocate(x(0))")
        mprocs.pop()
        mprocs.w("else if (n < size(x)) then")
        mprocs.push()
        mprocs.w("x = x(1:n)")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("end subroutine read_real_vector")
        mprocs.w("")
    emit_local_table_reader = bool(helper_ctx_main.get("need_table_reader")) and (not has_r_mod_main)
    emit_local_table_writer = bool(helper_ctx_main.get("need_table_writer")) and (not has_r_mod_main)
    if emit_local_table_reader or emit_local_table_writer:
        mprocs.w("pure integer function count_ws_tokens(line) result(n_tok)")
        mprocs.w("character(len=*), intent(in) :: line")
        mprocs.w("integer :: i, n")
        mprocs.w("logical :: in_tok")
        mprocs.w("n = len_trim(line)")
        mprocs.w("n_tok = 0")
        mprocs.w("in_tok = .false.")
        mprocs.w("do i = 1, n")
        mprocs.push()
        mprocs.w("if (line(i:i) /= ' ' .and. line(i:i) /= char(9)) then")
        mprocs.push()
        mprocs.w("if (.not. in_tok) then")
        mprocs.push()
        mprocs.w("n_tok = n_tok + 1")
        mprocs.w("in_tok = .true.")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.pop()
        mprocs.w("else")
        mprocs.push()
        mprocs.w("in_tok = .false.")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("end function count_ws_tokens")
        mprocs.w("")
    if emit_local_table_reader:
        mprocs.w("subroutine read_table_real_matrix(file_path, x)")
        mprocs.w("character(len=*), intent(in) :: file_path")
        mprocs.w("real(kind=dp), allocatable, intent(out) :: x(:,:)")
        mprocs.w("integer :: fp, ios, nrow, ncol, i")
        mprocs.w("character(len=4096) :: line")
        mprocs.w("nrow = 0")
        mprocs.w("ncol = 0")
        mprocs.w('open(newunit=fp, file=file_path, status="old", action="read")')
        mprocs.w("do")
        mprocs.push()
        mprocs.w("read(fp, '(A)', iostat=ios) line")
        mprocs.w("if (ios /= 0) exit")
        mprocs.w("if (len_trim(line) == 0) cycle")
        mprocs.w("nrow = nrow + 1")
        mprocs.w("if (ncol == 0) ncol = count_ws_tokens(line)")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("if (nrow <= 0 .or. ncol <= 0) then")
        mprocs.push()
        mprocs.w("allocate(x(0,0))")
        mprocs.w("close(fp)")
        mprocs.w("return")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("allocate(x(nrow, ncol))")
        mprocs.w("rewind(fp)")
        mprocs.w("i = 0")
        mprocs.w("do")
        mprocs.push()
        mprocs.w("read(fp, '(A)', iostat=ios) line")
        mprocs.w("if (ios /= 0) exit")
        mprocs.w("if (len_trim(line) == 0) cycle")
        mprocs.w("i = i + 1")
        mprocs.w("read(line, *) x(i, 1:ncol)")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("close(fp)")
        mprocs.w("end subroutine read_table_real_matrix")
        mprocs.w("")
    if emit_local_table_writer:
        mprocs.w("subroutine write_table_real_matrix(file_path, x)")
        mprocs.w("character(len=*), intent(in) :: file_path")
        mprocs.w("real(kind=dp), intent(in) :: x(:,:)")
        mprocs.w("integer :: fp, i")
        mprocs.w('open(newunit=fp, file=file_path, status="replace", action="write")')
        mprocs.w("do i = 1, size(x, 1)")
        mprocs.push()
        mprocs.w("write(fp, *) x(i, 1:size(x, 2))")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("close(fp)")
        mprocs.w("end subroutine write_table_real_matrix")
        mprocs.w("")
    emit_local_lm = (bool(helper_ctx_main.get("need_lm")) or bool(helper_ctx_mod.get("need_lm"))) and (not has_r_mod_main)
    if emit_local_lm:
        mprocs.w("subroutine print_lm_coef_rstyle(fit, term_names)")
        mprocs.w("type(lm_fit_t), intent(in) :: fit")
        mprocs.w("character(len=*), intent(in), optional :: term_names(:)")
        mprocs.w("integer :: j, p")
        mprocs.w("character(len=32) :: lbl")
        mprocs.w("p = max(0, size(fit%coef) - 1)")
        mprocs.w("write(*,'(a14)', advance='no') '(Intercept)'")
        mprocs.w("do j = 1, p")
        mprocs.push()
        mprocs.w("if (present(term_names) .and. size(term_names) >= j) then")
        mprocs.push()
        mprocs.w("write(*,'(a14)', advance='no') trim(term_names(j))")
        mprocs.pop()
        mprocs.w("else")
        mprocs.push()
        mprocs.w("write(lbl,'(a,i0)') 'x', j")
        mprocs.w("write(*,'(a14)', advance='no') trim(lbl)")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("write(*,*)")
        mprocs.w("if (size(fit%coef) > 0) then")
        mprocs.push()
        mprocs.w("write(*,'(*(f14.7))') fit%coef")
        mprocs.pop()
        mprocs.w("else")
        mprocs.push()
        mprocs.w("write(*,*)")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("end subroutine print_lm_coef_rstyle")
        mprocs.w("")
        mprocs.w("pure function lm_predict_general(fit, xpred) result(yhat)")
        mprocs.w("type(lm_fit_t), intent(in) :: fit")
        mprocs.w("real(kind=dp), intent(in) :: xpred(:,:)")
        mprocs.w("real(kind=dp), allocatable :: yhat(:)")
        mprocs.w("integer :: p")
        mprocs.w("p = size(xpred, 2)")
        mprocs.w("if (size(fit%coef) /= p + 1) error stop \"error: predictor count mismatch\"")
        mprocs.w("allocate(yhat(size(xpred, 1)))")
        mprocs.w("yhat = fit%coef(1) + matmul(xpred, fit%coef(2:p+1))")
        mprocs.w("end function lm_predict_general")
        mprocs.w("")
        mprocs.w("subroutine solve_linear(a, b, x, ok)")
        mprocs.w("real(kind=dp), intent(inout) :: a(:,:)")
        mprocs.w("real(kind=dp), intent(inout) :: b(:)")
        mprocs.w("real(kind=dp), intent(out) :: x(:)")
        mprocs.w("logical, intent(out) :: ok")
        mprocs.w("integer :: i, j, k, p, n")
        mprocs.w("real(kind=dp) :: piv, fac, t")
        mprocs.w("ok = .true.")
        mprocs.w("n = size(b)")
        mprocs.w("if (size(a,1) /= n .or. size(a,2) /= n .or. size(x) /= n) then")
        mprocs.push()
        mprocs.w("ok = .false.")
        mprocs.w("x = 0.0_dp")
        mprocs.w("return")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("do k = 1, n")
        mprocs.push()
        mprocs.w("p = k")
        mprocs.w("piv = abs(a(k,k))")
        mprocs.w("do i = k + 1, n")
        mprocs.push()
        mprocs.w("if (abs(a(i,k)) > piv) then")
        mprocs.push()
        mprocs.w("p = i")
        mprocs.w("piv = abs(a(i,k))")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("if (piv <= tiny(1.0_dp)) then")
        mprocs.push()
        mprocs.w("ok = .false.")
        mprocs.w("x = 0.0_dp")
        mprocs.w("return")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("if (p /= k) then")
        mprocs.push()
        mprocs.w("do j = k, n")
        mprocs.push()
        mprocs.w("t = a(k,j)")
        mprocs.w("a(k,j) = a(p,j)")
        mprocs.w("a(p,j) = t")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("t = b(k)")
        mprocs.w("b(k) = b(p)")
        mprocs.w("b(p) = t")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("do i = k + 1, n")
        mprocs.push()
        mprocs.w("fac = a(i,k) / a(k,k)")
        mprocs.w("a(i,k:n) = a(i,k:n) - fac * a(k,k:n)")
        mprocs.w("b(i) = b(i) - fac * b(k)")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("x(n) = b(n) / a(n,n)")
        mprocs.w("do i = n - 1, 1, -1")
        mprocs.push()
        mprocs.w("if (i < n) then")
        mprocs.push()
        mprocs.w("x(i) = (b(i) - sum(a(i,i+1:n) * x(i+1:n))) / a(i,i)")
        mprocs.pop()
        mprocs.w("else")
        mprocs.push()
        mprocs.w("x(i) = b(i) / a(i,i)")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("end subroutine solve_linear")
        mprocs.w("")
        mprocs.w("function lm_fit_general(y, xpred) result(fit)")
        mprocs.w("real(kind=dp), intent(in) :: y(:)")
        mprocs.w("real(kind=dp), intent(in) :: xpred(:,:)")
        mprocs.w("type(lm_fit_t) :: fit")
        mprocs.w("integer :: i, j, n, p, k, dof")
        mprocs.w("real(kind=dp), allocatable :: a(:,:), b(:), beta(:)")
        mprocs.w("real(kind=dp) :: ybar, sse, sst")
        mprocs.w("logical :: ok")
        mprocs.w("if (size(y) /= size(xpred,1)) then")
        mprocs.push()
        mprocs.w("error stop \"error: need size(y) == size(xpred,1)\"")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("n = size(y)")
        mprocs.w("p = size(xpred,2)")
        mprocs.w("k = p + 1")
        mprocs.w("if (n < k) error stop \"error: need n >= number of parameters\"")
        mprocs.w("allocate(a(k,k), b(k), beta(k))")
        mprocs.w("a = 0.0_dp")
        mprocs.w("b = 0.0_dp")
        mprocs.w("a(1,1) = n")
        mprocs.w("b(1) = sum(y)")
        mprocs.w("do j = 1, p")
        mprocs.push()
        mprocs.w("a(1,j+1) = sum(xpred(:,j))")
        mprocs.w("a(j+1,1) = a(1,j+1)")
        mprocs.w("b(j+1) = sum(xpred(:,j) * y)")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("do i = 1, p")
        mprocs.push()
        mprocs.w("do j = i, p")
        mprocs.push()
        mprocs.w("a(i+1,j+1) = sum(xpred(:,i) * xpred(:,j))")
        mprocs.w("a(j+1,i+1) = a(i+1,j+1)")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("call solve_linear(a, b, beta, ok)")
        mprocs.w("if (.not. ok) error stop \"error: singular normal equations\"")
        mprocs.w("fit%coef = beta")
        mprocs.w("fit%fitted = beta(1) + matmul(xpred, beta(2:k))")
        mprocs.w("fit%resid = y - fit%fitted")
        mprocs.w("sse = sum(fit%resid**2)")
        mprocs.w("ybar = sum(y) / n")
        mprocs.w("sst = sum((y - ybar)**2)")
        mprocs.w("if (sst > 0.0_dp) then")
        mprocs.push()
        mprocs.w("fit%r_squared = 1.0_dp - sse / sst")
        mprocs.pop()
        mprocs.w("else")
        mprocs.push()
        mprocs.w("fit%r_squared = 0.0_dp")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("dof = max(1, n - k)")
        mprocs.w("fit%sigma = sqrt(sse / dof)")
        mprocs.w("fit%adj_r_squared = 1.0_dp - (1.0_dp - fit%r_squared) * (n - 1) / dof")
        mprocs.w("end function lm_fit_general")
        mprocs.w("")
        mprocs.w("subroutine print_lm_summary(fit)")
        mprocs.w("type(lm_fit_t), intent(in) :: fit")
        mprocs.w("write(*,'(a)') 'lm summary:'")
        mprocs.w("write(*,'(a,*(1x,g0))') 'coef:', fit%coef")
        mprocs.w("write(*,'(a,g0)') 'sigma:', fit%sigma")
        mprocs.w("write(*,'(a,g0)') 'r.squared:', fit%r_squared")
        mprocs.w("write(*,'(a,g0)') 'adj.r.squared:', fit%adj_r_squared")
        mprocs.w("end subroutine print_lm_summary")
        mprocs.w("")

    helper_names = {
        "runif_vec",
        "rnorm_vec",
        "random_choice2_prob",
        "sample_int",
        "quantile",
        "dnorm",
        "tail",
        "cbind2",
        "cbind",
        "numeric",
        "pmax",
        "sd",
        "read_real_vector",
        "read_table_real_matrix",
        "write_table_real_matrix",
        "lm_fit_general",
        "lm_predict_general",
        "print_lm_summary",
        "print_lm_coef_rstyle",
        "lm_fit_t",
    }
    mod_needed: set[str] = set()
    main_needed: set[str] = set()
    nr_mod = helper_ctx_mod.get("need_r_mod")
    nr_main = helper_ctx_main.get("need_r_mod")
    if isinstance(nr_mod, set):
        mod_needed.update(nr_mod)
    if isinstance(nr_main, set):
        main_needed.update(nr_main)
    mod_text_now = "\n".join(mprocs.lines)
    main_text_now = "\n".join(pbody.lines)
    for hn in helper_names:
        if re.search(rf"\b{re.escape(hn)}\s*\(", mod_text_now):
            mod_needed.add(hn)
        if re.search(rf"\b{re.escape(hn)}\s*\(", main_text_now):
            main_needed.add(hn)
    if re.search(r"\btype\s*\(\s*lm_fit_t\s*\)", mod_text_now, re.IGNORECASE):
        mod_needed.add("lm_fit_t")
    if re.search(r"\btype\s*\(\s*lm_fit_t\s*\)", main_text_now, re.IGNORECASE):
        main_needed.add("lm_fit_t")

    o = FEmit()
    o.w(f"module {module_name}")
    o.w("use, intrinsic :: iso_fortran_env, only: real64")
    o.w("use, intrinsic :: ieee_arithmetic, only: ieee_is_finite, ieee_value, ieee_quiet_nan")
    if ("r_mod" in helper_modules) and mod_needed:
        o.w("use r_mod, only: " + ", ".join(sorted(mod_needed)))
    o.w("implicit none")
    o.w("integer, parameter :: dp = real64")
    need_lm = bool(helper_ctx_main.get("need_lm")) or bool(helper_ctx_mod.get("need_lm"))
    if need_lm and ("r_mod" not in helper_modules):
        o.w("type :: lm_fit_t")
        o.push()
        o.w("real(kind=dp), allocatable :: coef(:), fitted(:), resid(:)")
        o.w("real(kind=dp) :: sigma, r_squared, adj_r_squared")
        o.pop()
        o.w("end type lm_fit_t")
    # Derived types for list-return functions.
    emitted_types: set[str] = set()
    for fn_name, spec in list_specs.items():
        paths = sorted(spec.nested_types.keys(), key=lambda p: len(p), reverse=True)
        for path in paths:
            tname = _type_name_for_path(fn_name, path)
            if tname in emitted_types:
                continue
            emitted_types.add(tname)
            fields = spec.nested_types[path]
            o.w(f"type :: {tname}")
            o.push()
            for k, v in fields.items():
                if isinstance(v, dict):
                    nt = _type_name_for_path(fn_name, path + (k,))
                    o.w(f"type({nt}) :: {k}")
                else:
                    txt = str(v).strip()
                    fn_ints = fn_int_names.get(fn_name, set())
                    fn_int_arrays = fn_int_array_names.get(fn_name, set())
                    if _is_int_literal(txt):
                        o.w(f"integer :: {k}")
                    elif txt in {"TRUE", "FALSE"}:
                        o.w(f"logical :: {k}")
                    elif re.match(r"^[A-Za-z]\w*$", txt) and txt in fn_ints:
                        o.w(f"integer :: {k}")
                    elif re.match(r"^[A-Za-z]\w*$", txt) and txt in fn_int_arrays:
                        o.w(f"integer, allocatable :: {k}(:)")
                    elif txt.startswith("cbind(") or txt.startswith("cbind2("):
                        o.w(f"real(kind=dp), allocatable :: {k}(:,:)")
                    elif k in {"pi", "mu", "sigma", "x", "z", "resp", "cluster", "loglik"}:
                        o.w(f"real(kind=dp), allocatable :: {k}(:)")
                    elif txt.startswith("c(") or txt.startswith("[") or txt.startswith("runif(") or txt.startswith("rnorm("):
                        o.w(f"real(kind=dp), allocatable :: {k}(:)")
                    else:
                        # If expression looks like integer index/length use integer, else real.
                        if re.match(r"^[A-Za-z]\w*$", txt):
                            o.w(f"real(kind=dp) :: {k}")
                        else:
                            o.w(f"real(kind=dp) :: {k}")
            o.pop()
            o.w(f"end type {tname}")
    need_contains = bool(funcs) or emit_local_rnorm or emit_local_scan_reader or emit_local_table_reader or emit_local_table_writer or emit_local_lm
    if need_contains:
        o.w("")
        o.w("contains")
        o.w("")
        o.lines.extend(mprocs.lines)
    o.w(f"end module {module_name}")
    o.w("")
    o.w(f"program {unit_name}")
    o.w(f"use {module_name}")
    o.w("use, intrinsic :: ieee_arithmetic, only: ieee_is_finite, ieee_value, ieee_quiet_nan")
    if ("r_mod" in helper_modules) and main_needed:
        o.w("use r_mod, only: " + ", ".join(sorted(main_needed)))
    o.w("implicit none")
    o.lines.extend(pbody.lines)
    o.w(f"end program {unit_name}")
    return o.text()


def _norm_output(s: str) -> list[str]:
    lines = s.replace("\r\n", "\n").replace("\r", "\n").split("\n")
    lines = [" ".join(ln.split()) for ln in lines]
    while lines and lines[-1] == "":
        lines.pop()
    return lines


def normalize_fortran_lines(lines: list[str], max_consecutive_blank: int = 1) -> list[str]:
    out: list[str] = []
    blank_run = 0
    for ln in lines:
        s = ln.rstrip()
        if s == "":
            blank_run += 1
            if blank_run <= max_consecutive_blank:
                out.append("")
        else:
            blank_run = 0
            out.append(s)
    # Trim leading/trailing blank lines.
    while out and out[0] == "":
        out.pop(0)
    while out and out[-1] == "":
        out.pop()
    return out


def mark_pure_with_xpure(lines: list[str]) -> list[str]:
    """Mark likely PURE procedures using xpure.py analysis logic."""
    try:
        import xpure  # local tool module in this project
    except Exception:
        return lines

    try:
        result = xpure.analyze_lines(
            lines,
            external_name_status=None,
            generic_interfaces=None,
            strict_unknown_calls=False,
            conservative_call_block=True,
        )
    except Exception:
        return lines

    cand_ids = {(p.name.lower(), int(p.start)) for p in result.candidates}
    if not cand_ids:
        return lines

    out = list(lines)
    try:
        procs = fscan.parse_procedures(lines)
    except Exception:
        return lines

    for p in procs:
        key = (p.name.lower(), int(p.start))
        if key not in cand_ids:
            continue
        idx = p.start - 1
        if idx < 0 or idx >= len(out):
            continue
        new_line, changed = xpure.add_pure_to_declaration(out[idx])
        if changed:
            out[idx] = new_line
    return out


def _run_capture(cmd: list[str]) -> subprocess.CompletedProcess[str]:
    """Run command with robust text decoding on Windows."""
    return subprocess.run(cmd, capture_output=True, text=True, encoding="utf-8", errors="replace")


def _print_captured(cp: subprocess.CompletedProcess[str], normalize_num_output: bool = False) -> None:
    out = cp.stdout or ""
    err = cp.stderr or ""
    if normalize_num_output:
        out = fscan.normalize_numeric_leading_zeros_text(out)
        err = fscan.normalize_numeric_leading_zeros_text(err)
    if out.strip():
        txt = out.rstrip()
        try:
            print(txt)
        except UnicodeEncodeError:
            sys.stdout.buffer.write((txt + "\n").encode("utf-8", errors="replace"))
            sys.stdout.flush()
    if err.strip():
        txt = err.rstrip()
        try:
            print(txt)
        except UnicodeEncodeError:
            sys.stdout.buffer.write((txt + "\n").encode("utf-8", errors="replace"))
            sys.stdout.flush()


def main() -> int:
    ap = argparse.ArgumentParser(description="Partial R-to-Fortran transpiler")
    ap.add_argument("input_r", help="input .R/.r source file")
    ap.add_argument("helpers", nargs="*", help="optional helper Fortran source files (modules)")
    ap.add_argument("--out", help="output .f90 path (default: <input>_r.f90)")
    ap.add_argument("--compile", action="store_true", help="compile transpiled Fortran")
    ap.add_argument("--run", action="store_true", help="compile and run transpiled Fortran")
    ap.add_argument("--run-both", action="store_true", help="run original R and transpiled Fortran")
    ap.add_argument("--run-diff", action="store_true", help="run both and compare outputs")
    ap.add_argument("--time", action="store_true", help="time transpile/compile/run (implies --run)")
    ap.add_argument("--time-both", action="store_true", help="time both original R and transpiled Fortran (implies --run-diff)")
    ap.add_argument("--tee", action="store_true", help="print transpiled source; in run mode also prints transformed output")
    ap.add_argument("--tee-both", action="store_true", help="print original + transpiled source; in run-both mode prints both outputs")
    ap.add_argument(
        "--if-const-aggressive",
        action="store_true",
        help="aggressively fold compile-time constant IF conditions (default folds only literal .true./.false. forms)",
    )
    ap.add_argument(
        "--real-print-fmt",
        default="f0.6",
        help='format descriptor used for real expressions when rewriting `print *` (default: "f0.6")',
    )
    ap.add_argument(
        "--no-format-print",
        action="store_true",
        help="do not rewrite list-directed `print *` to explicit `write` formats",
    )
    ap.add_argument("--compiler", default="gfortran -O3 -march=native -flto -Wfatal-errors", help='compiler command, e.g. "gfortran -O2 -Wall"')
    ap.add_argument("--rscript", default="rscript", help="command to run R scripts")
    ap.add_argument(
        "--normalize-num-output",
        action="store_true",
        help="normalize Fortran run output numeric tokens like .5/-.5 to 0.5/-0.5",
    )
    args = ap.parse_args()

    if args.time_both:
        args.run_diff = True
    if args.run_diff:
        args.run_both = True
    if args.run_both:
        args.run = True
        args.compile = True
    if args.time_both:
        args.time = True
    if args.time:
        args.run = True
    if args.tee_both:
        args.tee = True

    in_path = Path(args.input_r)
    if not in_path.exists():
        print(f"Missing file: {in_path}")
        return 1
    helper_paths = [Path(h) for h in args.helpers]
    for hp in helper_paths:
        if not hp.exists():
            print(f"Missing helper file: {hp}")
            return 1
    out_path = Path(args.out) if args.out else in_path.with_name(f"{in_path.stem}_r.f90")
    helper_modules = helper_modules_from_files(helper_paths)

    timings: dict[str, float] = {}
    r_run = None

    if args.time_both or args.run_both:
        cmd = [args.rscript, str(in_path)]
        t0 = time.perf_counter() if args.time_both else None
        r_run = _run_capture(cmd)
        if args.time_both:
            timings["r_run"] = time.perf_counter() - t0
        print("Run (r):", " ".join(cmd))
        if r_run.returncode != 0:
            print(f"Run (r): FAIL (exit {r_run.returncode})")
            _print_captured(r_run)
            return r_run.returncode
        print("Run (r): PASS")
        _print_captured(r_run)

    t0 = time.perf_counter()
    src = in_path.read_text(encoding="utf-8")
    try:
        f90 = transpile_r_to_fortran(src, in_path.stem, helper_modules=helper_modules)
    except NotImplementedError as e:
        print(f"Transpile: FAIL ({e})")
        return 1
    # Reuse shared Fortran cleanup for redundant int(...) casts.
    f90_lines = f90.splitlines()
    f90_lines = fscan.remove_redundant_int_casts(f90_lines)
    f90_lines = fscan.simplify_real_int_casts_in_mixed_expr(f90_lines)
    f90_lines = fscan.simplify_size_expressions(f90_lines)
    f90_lines = fscan.propagate_array_size_aliases(f90_lines)
    f90_lines = fscan.propagate_cached_size_values(f90_lines)
    f90_lines = fscan.simplify_redundant_parens_in_lines(f90_lines)
    f90_lines = fscan.simplify_do_bounds_parens(f90_lines)
    f90_lines = fscan.simplify_negated_relational_conditions_in_lines(f90_lines)
    f90_lines = fscan.simplify_constant_if_blocks(f90_lines, aggressive=args.if_const_aggressive)
    f90_lines = mark_pure_with_xpure(f90_lines)
    f90_lines = fscan.collapse_single_stmt_if_blocks(f90_lines)
    f90_lines, _na_changes, _na_unavail = fscan.rewrite_named_arguments_in_lines(f90_lines, max_positional=3)
    if not args.no_format_print:
        f90_lines = fscan.rewrite_list_directed_print_reals(f90_lines, real_fmt=args.real_print_fmt)
    f90_lines = fscan.compact_repeated_edit_descriptors(f90_lines)
    f90_lines = fscan.coalesce_simple_declarations(f90_lines, max_len=80)
    f90_lines = fscan.wrap_long_declaration_lines(f90_lines, max_len=80)
    f90_lines = normalize_fortran_lines(f90_lines, max_consecutive_blank=1)
    f90_lines = fscan.wrap_long_fortran_lines(f90_lines, max_len=80)
    f90 = "\n".join(f90_lines) + ("\n" if f90.endswith("\n") else "")
    stamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    r_comments = extract_r_top_comments(src)
    migrated_block = ""
    if r_comments:
        migrated_block = "\n".join(f"! {c}" for c in r_comments) + "\n"
    f90 = f"! transpiled by xr2f.py from {in_path.name} on {stamp}\n" + migrated_block + f90
    out_path.write_text(f90, encoding="utf-8")
    timings["transpile"] = time.perf_counter() - t0
    print(f"wrote {out_path}")

    if args.tee_both:
        print(f"--- original: {in_path} ---")
        print(src.rstrip())
    if args.tee:
        print(f"--- transpiled: {out_path} ---")
        print(f90.rstrip())

    if args.compile or args.run:
        cparts = shlex.split(args.compiler)
        exe = out_path.with_suffix(".exe")
        cmd = cparts + [str(h) for h in helper_paths] + [str(out_path)]
        if args.run:
            cmd += ["-o", str(exe)]
        if args.time:
            print("Compile options:", " ".join(cparts[1:]) if len(cparts) > 1 else "<none>")
        print("Build:", " ".join(cmd))
        t0 = time.perf_counter()
        cp = _run_capture(cmd)
        timings["compile"] = time.perf_counter() - t0
        if cp.returncode != 0:
            print(f"Build: FAIL (exit {cp.returncode})")
            _print_captured(cp)
            return cp.returncode
        print("Build: PASS")

        if args.run:
            t0 = time.perf_counter()
            frun = _run_capture([str(exe)])
            timings["fortran_run"] = time.perf_counter() - t0
            if frun.returncode != 0:
                print(f"Run: FAIL (exit {frun.returncode})")
                _print_captured(frun)
                return frun.returncode
            print("Run: PASS")
            _print_captured(frun, normalize_num_output=args.normalize_num_output)

            if args.run_diff and r_run is not None:
                r_lines = _norm_output((r_run.stdout or "") + (("\n" + r_run.stderr) if r_run.stderr else ""))
                f_blob = (frun.stdout or "") + (("\n" + frun.stderr) if frun.stderr else "")
                if args.normalize_num_output:
                    f_blob = fscan.normalize_numeric_leading_zeros_text(f_blob)
                f_lines = _norm_output(f_blob)
                if r_lines == f_lines:
                    print("Run diff: MATCH")
                else:
                    print("Run diff: DIFF")
                    first = None
                    nmin = min(len(r_lines), len(f_lines))
                    for i in range(nmin):
                        if r_lines[i] != f_lines[i]:
                            first = i
                            break
                    if first is None:
                        first = nmin
                    print(f"  first mismatch line: {first + 1}")
                    if first < len(r_lines):
                        print(f"  r      : {r_lines[first]}")
                    if first < len(f_lines):
                        print(f"  fortran: {f_lines[first]}")
                    for dl in difflib.unified_diff(r_lines, f_lines, fromfile="r", tofile="fortran", n=1):
                        print(dl)
                        if dl.startswith("@@"):
                            break

    if args.time:
        base = timings.get("r_run", 0.0)
        rows = []
        if "r_run" in timings:
            rows.append(("r run", timings["r_run"]))
        rows.append(("transpile", timings.get("transpile", 0.0)))
        if "compile" in timings:
            rows.append(("compile", timings["compile"]))
        if "fortran_run" in timings:
            rows.append(("fortran run", timings["fortran_run"]))
        rows.append(
            (
                "fortran total",
                timings.get("compile", 0.0) + timings.get("fortran_run", 0.0),
            )
        )
        stage_w = max(len("stage"), max(len(n) for n, _ in rows))
        sec_w = max(len("seconds"), max(len(f"{v:.6f}") for _, v in rows))
        print("")
        print("Timing summary (seconds):")
        print(f"  {'stage':<{stage_w}}  {'seconds':>{sec_w}}    ratio(vs r run)")
        for n, v in rows:
            ratio = f"{(v / base):.6f}" if base > 0 else "n/a"
            print(f"  {n:<{stage_w}}  {v:>{sec_w}.6f}    {ratio}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
