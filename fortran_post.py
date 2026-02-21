#!/usr/bin/env python3
"""Shared post-processing rewrites for generated Fortran code."""

from __future__ import annotations

import re
from typing import List

import fortran_scan as fscan
import xunused


def ensure_function_result_syntax(lines: List[str]) -> List[str]:
    """Ensure function signatures use explicit RESULT(...) syntax.

    Conservative rewrite:
    - If a function header has no RESULT(...), append `result(<name>_result)`.
    - Inside that function body, rewrite simple result assignments
      `name = expr` -> `<name>_result = expr`.
    """
    out = list(lines)
    hdr_re = re.compile(
        r"^(?P<indent>\s*)(?P<prefix>.*?\bfunction\s+)(?P<name>[a-z][a-z0-9_]*)\s*\((?P<args>[^)]*)\)\s*$",
        re.IGNORECASE,
    )
    end_re = re.compile(r"^\s*end\s+function\b", re.IGNORECASE)

    i = 0
    while i < len(out):
        raw = out[i]
        code, comment = xunused.split_code_comment(raw.rstrip("\r\n"))
        if re.match(r"^\s*end\s+function\b", code, re.IGNORECASE):
            i += 1
            continue
        m = hdr_re.match(code)
        if m is None:
            i += 1
            continue
        if re.search(r"\bresult\s*\(", code, re.IGNORECASE):
            i += 1
            continue
        fname = m.group("name")
        res = f"{fname}_result"
        eol = "\n" if raw.endswith("\n") else ""
        out[i] = f"{code} result({res}){comment}{eol}"

        # Rewrite plain result assignments in this function body.
        assign_re = re.compile(rf"^(\s*){re.escape(fname)}(\s*=.*)$", re.IGNORECASE)
        j = i + 1
        while j < len(out):
            code_j, comment_j = xunused.split_code_comment(out[j].rstrip("\r\n"))
            if end_re.match(code_j):
                break
            m_asg = assign_re.match(code_j)
            if m_asg is not None:
                eol_j = "\n" if out[j].endswith("\n") else ""
                out[j] = f"{m_asg.group(1)}{res}{m_asg.group(2)}{comment_j}{eol_j}"
            j += 1
        i = j + 1
    return out


def inline_temp_into_function_result(lines: List[str]) -> List[str]:
    """Inline local temporary variables into function RESULT variables.

    Pattern handled (conservative):
    - function header has `result(res)`
    - executable statement `res = tmp`
    - `tmp` is a declared local (non-dummy) and appears only in executable code
    Then rewrite executable references of `tmp` to `res`, drop `res = tmp`,
    and remove `tmp` from declarations.
    """
    out = list(lines)
    f_start_re = re.compile(r"^\s*(?:pure\s+)?function\s+[a-z][a-z0-9_]*\s*\([^)]*\)\s*result\s*\(\s*([a-z][a-z0-9_]*)\s*\)", re.IGNORECASE)
    f_end_re = re.compile(r"^\s*end\s+function\b", re.IGNORECASE)
    assign_re = re.compile(r"^\s*([a-z][a-z0-9_]*)\s*=\s*([a-z][a-z0-9_]*)\s*$", re.IGNORECASE)
    ident_re = re.compile(r"\b[a-z][a-z0-9_]*\b", re.IGNORECASE)

    i = 0
    while i < len(out):
        code_i = fscan.strip_comment(out[i]).strip()
        m_start = f_start_re.match(code_i)
        if m_start is None:
            i += 1
            continue
        res = m_start.group(1).lower()
        j = i + 1
        while j < len(out):
            if f_end_re.match(fscan.strip_comment(out[j]).strip()):
                break
            j += 1
        if j >= len(out):
            break

        # collect dummy names from declarations with intent(...)
        dummies = set()
        for k in range(i + 1, j):
            code_k = fscan.strip_comment(out[k]).strip()
            if "::" in code_k and "intent(" in code_k.lower():
                dummies.update(fscan.parse_declared_names_from_decl(code_k))

        # candidate result assignment
        cand_line = None
        cand_tmp = None
        for k in range(i + 1, j):
            code_k = fscan.strip_comment(out[k]).strip()
            if not code_k or "::" in code_k:
                continue
            m_as = assign_re.match(code_k)
            if m_as is None:
                continue
            lhs = m_as.group(1).lower()
            rhs = m_as.group(2).lower()
            if lhs == res and rhs != res and rhs not in dummies:
                cand_line = k
                cand_tmp = rhs
        if cand_line is None or cand_tmp is None:
            i = j + 1
            continue

        tmp = cand_tmp
        pat_tmp = re.compile(rf"\b{re.escape(tmp)}\b", re.IGNORECASE)
        # Must have a declaration for tmp.
        decl_idxs = []
        for k in range(i + 1, j):
            code_k, _ = xunused.split_code_comment(out[k].rstrip("\r\n"))
            if "::" in code_k and pat_tmp.search(code_k):
                decl_idxs.append(k)
        if not decl_idxs:
            i = j + 1
            continue

        # Ensure tmp only appears in executable statements (or its declarations).
        bad_use = False
        exec_use_count = 0
        for k in range(i + 1, j):
            code_k, _ = xunused.split_code_comment(out[k].rstrip("\r\n"))
            if not pat_tmp.search(code_k):
                continue
            if k in decl_idxs:
                continue
            if "::" in code_k:
                bad_use = True
                break
            exec_use_count += len([m for m in ident_re.finditer(code_k) if m.group(0).lower() == tmp])
        if bad_use or exec_use_count == 0:
            i = j + 1
            continue

        # Rewrite executable occurrences tmp -> res.
        for k in range(i + 1, j):
            if k in decl_idxs:
                continue
            code_k, comment_k = xunused.split_code_comment(out[k].rstrip("\r\n"))
            if not pat_tmp.search(code_k):
                continue
            new_code_k = pat_tmp.sub(res, code_k)
            eol_k = xunused.get_eol(out[k]) or "\n"
            out[k] = f"{new_code_k}{comment_k}{eol_k}"

        # Remove trivial self-assignment result = result.
        code_c, _ = xunused.split_code_comment(out[cand_line].rstrip("\r\n"))
        if re.match(rf"^\s*{re.escape(res)}\s*=\s*{re.escape(res)}\s*$", code_c, re.IGNORECASE):
            out[cand_line] = ""

        # Remove tmp from declarations when unused.
        still_used = False
        for k in range(i + 1, j):
            if k in decl_idxs:
                continue
            code_k = fscan.strip_comment(out[k])
            if any(m.group(0).lower() == tmp for m in ident_re.finditer(code_k)):
                still_used = True
                break
        if not still_used:
            for d in decl_idxs:
                new_ln, _changed = xunused.rewrite_decl_remove_names(out[d], {tmp})
                out[d] = "" if new_ln is None else new_ln

        i = j + 1

    return [ln for ln in out if ln != ""]


def remove_redundant_self_assignments(lines: List[str]) -> List[str]:
    """Remove trivial self-assignments like `x = x`."""
    out: List[str] = []
    assign_re = re.compile(r"^\s*([a-z][a-z0-9_]*)\s*=\s*([a-z][a-z0-9_]*)\s*$", re.IGNORECASE)
    for raw in lines:
        code, comment = xunused.split_code_comment(raw.rstrip("\r\n"))
        m = assign_re.match(code.strip())
        if m is not None and m.group(1).lower() == m.group(2).lower():
            continue
        out.append(raw)
    return out


def remove_pre_overwrite_assignments(lines: List[str], *, lookahead: int = 8) -> List[str]:
    """Remove scalar assignments overwritten before any intervening read.

    Conservative rule:
    - current line is `name = expr` (scalar lhs)
    - within the next `lookahead` executable statements, first mention of
      `name` is another assignment to `name`
    """
    out = list(lines)
    assign_re = re.compile(r"^\s*([a-z][a-z0-9_]*)\s*=\s*[^=].*$", re.IGNORECASE)
    barrier_re = re.compile(
        r"^\s*(if\b|else\b|end\s*if\b|do\b|end\s*do\b|select\b|case\b|end\s*select\b|where\b|end\s*where\b|block\b|end\s*block\b)",
        re.IGNORECASE,
    )
    ident_tpl = r"\b{nm}\b"

    i = 0
    while i < len(out):
        code_i, _com_i = xunused.split_code_comment(out[i].rstrip("\r\n"))
        m = assign_re.match(code_i.strip())
        if m is None:
            i += 1
            continue
        v = m.group(1).lower()
        pat = re.compile(ident_tpl.format(nm=re.escape(v)), re.IGNORECASE)
        replaced = False
        seen_exec = 0
        j = i + 1
        while j < len(out) and seen_exec < lookahead:
            code_j, _com_j = xunused.split_code_comment(out[j].rstrip("\r\n"))
            s = code_j.strip()
            if not s:
                j += 1
                continue
            if barrier_re.match(s):
                break
            seen_exec += 1
            if not pat.search(code_j):
                j += 1
                continue
            m2 = assign_re.match(s)
            if m2 is not None and m2.group(1).lower() == v:
                out[i] = ""
                replaced = True
            break
        if replaced:
            i = j
        else:
            i += 1
    return [ln for ln in out if ln != ""]


def remove_redundant_zero_before_reduction(lines: List[str]) -> List[str]:
    """Drop `v = 0` immediately before `v = sum(...)`/`product(...)`/`norm2(...)`."""
    out = list(lines)
    zero_re = re.compile(r"^\s*([a-z][a-z0-9_]*)\s*=\s*[+-]?0(?:\.0+)?(?:_[a-z0-9_]+)?\s*$", re.IGNORECASE)
    red_re = re.compile(
        r"^\s*([a-z][a-z0-9_]*)\s*=\s*(?:sum|product|norm2)\s*\(",
        re.IGNORECASE,
    )
    ident_tpl = r"\b{nm}\b"
    i = 0
    while i < len(out):
        c0, _ = xunused.split_code_comment(out[i].rstrip("\r\n"))
        m0 = zero_re.match(c0.strip())
        if m0 is None:
            i += 1
            continue
        v = m0.group(1).lower()
        pat_v = re.compile(ident_tpl.format(nm=re.escape(v)), re.IGNORECASE)
        j = i + 1
        steps = 0
        while j < len(out) and steps < 10:
            cj, _ = xunused.split_code_comment(out[j].rstrip("\r\n"))
            s = cj.strip()
            if not s:
                j += 1
                continue
            steps += 1
            if not pat_v.search(cj):
                j += 1
                continue
            mj = red_re.match(s)
            if mj is not None and mj.group(1).lower() == v:
                out[i] = ""
            break
        i += 1
    return [ln for ln in out if ln != ""]


def hoist_repeated_size_calls(lines: List[str], *, min_uses: int = 3) -> List[str]:
    """Hoist repeated `size(arr)` calls to integer locals within each unit.

    Example:
      ... size(x) ... size(x) ... size(x)
    becomes:
      integer :: nx
      nx = size(x)
      ... nx ... nx ... nx
    """
    out = list(lines)
    unit_start_re = re.compile(
        r"^\s*(?:[a-z][a-z0-9_()\s=,:]*\s+)?(?:function|subroutine)\b|^\s*program\b",
        re.IGNORECASE,
    )
    unit_end_re = re.compile(r"^\s*end\s+(?:function|subroutine|program)\b", re.IGNORECASE)
    declish_re = re.compile(
        r"^\s*(?:implicit\b|use\b|integer\b|real\b|logical\b|character\b|complex\b|type\b|class\b|procedure\b|save\b|parameter\b|external\b|intrinsic\b|common\b|equivalence\b|dimension\b)",
        re.IGNORECASE,
    )
    size_re = re.compile(r"\bsize\s*\(\s*([a-z][a-z0-9_]*)\s*\)", re.IGNORECASE)

    i = 0
    while i < len(out):
        if not unit_start_re.match(fscan.strip_comment(out[i]).strip()):
            i += 1
            continue
        us = i
        ue = i + 1
        while ue < len(out) and not unit_end_re.match(fscan.strip_comment(out[ue]).strip()):
            ue += 1
        if ue >= len(out):
            break

        # find executable start
        k = us + 1
        while k < ue and (not out[k].strip() or out[k].lstrip().startswith("!")):
            k += 1
        while k < ue:
            s = fscan.strip_comment(out[k]).strip()
            if not s or s.startswith("!") or declish_re.match(s):
                k += 1
                continue
            break
        exec_start = k
        if exec_start >= ue:
            i = ue + 1
            continue

        # Track declaration section bounds and last integer declaration line.
        decl_start = us + 1
        while decl_start < ue and (not out[decl_start].strip() or out[decl_start].lstrip().startswith("!")):
            decl_start += 1
        decl_end = exec_start
        last_int_decl: Optional[int] = None
        for di in range(decl_start, decl_end):
            code_di = fscan.strip_comment(out[di]).strip()
            if "::" in code_di and re.match(r"^\s*integer\b", code_di, re.IGNORECASE):
                last_int_decl = di

        # collect dummies where early SIZE() is unsafe:
        # allocatable + intent(out) starts unallocated on entry.
        unsafe_size_arrays: Set[str] = set()
        for di in range(decl_start, decl_end):
            code_di = fscan.strip_comment(out[di]).strip()
            if "::" not in code_di:
                continue
            low_di = code_di.lower()
            if "allocatable" not in low_di or "intent(out)" not in low_di:
                continue
            rhs = code_di.split("::", 1)[1]
            for part in rhs.split(","):
                name_part = part.split("(", 1)[0].strip()
                if not name_part:
                    continue
                mname = re.match(r"^[a-z][a-z0-9_]*$", name_part, re.IGNORECASE)
                if mname is not None:
                    unsafe_size_arrays.add(name_part.lower())

        # collect counts from executable region
        counts: Dict[str, int] = {}
        for li in range(exec_start, ue):
            code = fscan.strip_comment(out[li])
            for m in size_re.finditer(code):
                arr = m.group(1).lower()
                counts[arr] = counts.get(arr, 0) + 1
        targets = [a for a, c in counts.items() if c >= min_uses and a not in unsafe_size_arrays]
        if not targets:
            i = ue + 1
            continue

        # existing identifiers to avoid collisions
        used_names: Set[str] = set()
        ident_re = re.compile(r"\b([a-z][a-z0-9_]*)\b", re.IGNORECASE)
        for li in range(us, ue + 1):
            code = fscan.strip_comment(out[li])
            for m in ident_re.finditer(code):
                used_names.add(m.group(1).lower())

        decl_insert = (last_int_decl + 1) if last_int_decl is not None else exec_start
        stmt_insert = exec_start
        new_lines: List[str] = []
        rename: Dict[str, str] = {}
        for arr in targets:
            base = f"n{arr}"
            v = base
            while v in used_names:
                v = v + "_"
            used_names.add(v)
            rename[arr] = v
            new_lines.append(f"integer :: {v}\n")
        for arr, v in rename.items():
            new_lines.append(f"{v} = size({arr})\n")

        # split declaration insertion from assignment insertion
        decl_lines: List[str] = [ln for ln in new_lines if ln.lower().lstrip().startswith("integer ::")]
        asn_lines: List[str] = [ln for ln in new_lines if ln not in decl_lines]

        out[decl_insert:decl_insert] = decl_lines
        shift_decl = len(decl_lines)
        ue += shift_decl
        if decl_insert <= stmt_insert:
            stmt_insert += shift_decl

        out[stmt_insert:stmt_insert] = asn_lines
        shift_asn = len(asn_lines)
        ue += shift_asn
        stmt_start = stmt_insert + shift_asn

        # rewrite executable uses
        for li in range(stmt_start, ue):
            raw = out[li]
            code, comment = xunused.split_code_comment(raw.rstrip("\r\n"))
            if not code.strip():
                continue
            for arr, v in rename.items():
                code = re.sub(
                    rf"\bsize\s*\(\s*{re.escape(arr)}\s*\)",
                    v,
                    code,
                    flags=re.IGNORECASE,
                )
            eol = xunused.get_eol(raw) or "\n"
            out[li] = f"{code}{comment}{eol}"

        i = ue + 1
    return out


def tighten_size_alias_nonpositive_guards(lines: List[str]) -> List[str]:
    """Rewrite `if (n <= 0)` -> `if (n == 0)` for `n = size(...)` aliases."""
    out = list(lines)
    unit_start_re = re.compile(
        r"^\s*(?:[a-z][a-z0-9_()\s=,:]*\s+)?(?:function|subroutine)\b|^\s*program\b",
        re.IGNORECASE,
    )
    unit_end_re = re.compile(r"^\s*end\s+(?:function|subroutine|program)\b", re.IGNORECASE)
    size_asn_re = re.compile(
        r"^\s*([a-z][a-z0-9_]*)\s*=\s*size\s*\(\s*[a-z][a-z0-9_]*\s*\)\s*$",
        re.IGNORECASE,
    )
    if_le0_re = re.compile(
        r"^(?P<prefix>\s*if\s*\(\s*)(?P<name>[a-z][a-z0-9_]*)\s*<=\s*0\s*(?P<suffix>\)\s*(?:then\b.*|return\b.*)?)$",
        re.IGNORECASE,
    )

    i = 0
    while i < len(out):
        if not unit_start_re.match(fscan.strip_comment(out[i]).strip()):
            i += 1
            continue
        us = i
        ue = i + 1
        while ue < len(out) and not unit_end_re.match(fscan.strip_comment(out[ue]).strip()):
            ue += 1
        if ue >= len(out):
            break

        aliases: Set[str] = set()
        for k in range(us + 1, ue):
            code = fscan.strip_comment(out[k]).strip()
            m = size_asn_re.match(code)
            if m is not None:
                aliases.add(m.group(1).lower())

        if aliases:
            for k in range(us + 1, ue):
                raw = out[k]
                code, comment = xunused.split_code_comment(raw.rstrip("\r\n"))
                m = if_le0_re.match(code)
                if m is None:
                    continue
                nm = m.group("name").lower()
                if nm not in aliases:
                    continue
                eol = xunused.get_eol(raw) or "\n"
                out[k] = f"{m.group('prefix')}{m.group('name')} == 0{m.group('suffix')}{comment}{eol}"
        i = ue + 1

    return out


def normalize_shifted_index_loops(lines: List[str]) -> List[str]:
    """Rewrite shifted-index loops to idiomatic 1-based forms.

    Conservative rewrite: only applies when, inside the loop body, every
    occurrence of loop variable `i` appears as `i+1` (with optional spaces).

    Supported header rewrites:
    - `do i = 1, n-1` -> `do i = 2, n`
    - `do i = 0, n-1` -> `do i = 1, n`
    """
    out = list(lines)
    do_re_1 = re.compile(
        r"^(?P<indent>\s*)do\s+(?P<ivar>[a-z][a-z0-9_]*)\s*=\s*1\s*,\s*(?P<ubexpr>.+?)\s*-\s*1\s*$",
        re.IGNORECASE,
    )
    do_re_0 = re.compile(
        r"^(?P<indent>\s*)do\s+(?P<ivar>[a-z][a-z0-9_]*)\s*=\s*0\s*,\s*(?P<ubexpr>.+?)\s*-\s*1\s*$",
        re.IGNORECASE,
    )
    end_do_re = re.compile(r"^\s*end\s*do\s*$", re.IGNORECASE)

    i = 0
    while i < len(out):
        code_i = fscan.strip_comment(out[i]).strip()
        m = do_re_1.match(code_i)
        new_lb = "2"
        if m is None:
            m = do_re_0.match(code_i)
            new_lb = "1"
        if m is None:
            i += 1
            continue
        ivar = m.group("ivar")
        ub = (m.group("ubexpr") or "").strip()

        # Find matching END DO at same nesting depth.
        depth = 1
        j = i + 1
        while j < len(out):
            c = fscan.strip_comment(out[j]).strip()
            if re.match(r"^\s*do\b", c, re.IGNORECASE):
                depth += 1
            elif end_do_re.match(c):
                depth -= 1
                if depth == 0:
                    break
            j += 1
        if j >= len(out):
            break

        # Check loop body usage of ivar.
        pat_plus = re.compile(rf"\b{re.escape(ivar)}\s*\+\s*1\b", re.IGNORECASE)
        pat_id = re.compile(rf"\b{re.escape(ivar)}\b", re.IGNORECASE)
        saw_plus = False
        safe = True
        for k in range(i + 1, j):
            code_k, _comment_k = xunused.split_code_comment(out[k].rstrip("\r\n"))
            if not code_k.strip():
                continue
            if pat_plus.search(code_k):
                saw_plus = True
            code_removed = pat_plus.sub("", code_k)
            if pat_id.search(code_removed):
                safe = False
                break
        if not safe or not saw_plus:
            i = j + 1
            continue

        # Rewrite header and body.
        eol = "\n" if out[i].endswith("\n") else ""
        out[i] = f"{m.group('indent')}do {ivar} = {new_lb}, {ub}{eol}"
        for k in range(i + 1, j):
            code_k, comment_k = xunused.split_code_comment(out[k].rstrip("\r\n"))
            if not code_k.strip():
                continue
            new_code = pat_plus.sub(ivar, code_k)
            eol_k = "\n" if out[k].endswith("\n") else ""
            out[k] = f"{new_code}{comment_k}{eol_k}"
        i = j + 1
    return out


def hoist_repeated_open_file_literals(lines: List[str], *, min_count: int = 2) -> List[str]:
    """Hoist repeated OPEN(file="...") literals into named character parameters.

    Conservative scope:
    - per program/subroutine/function unit
    - only string literals in `file="..."` specifiers
    - only when the same literal appears at least `min_count` times in that unit
    """
    out = list(lines)
    unit_start_re = re.compile(
        r"^\s*(?:[a-z][a-z0-9_()\s=,:]*\s+)?(?:function|subroutine)\b|^\s*program\b",
        re.IGNORECASE,
    )
    unit_end_re = re.compile(r"^\s*end\s+(?:function|subroutine|program)\b", re.IGNORECASE)
    open_file_re = re.compile(r'(\bfile\s*=\s*)(?P<q>["\'])(?P<val>[^"\']+)(?P=q)', re.IGNORECASE)

    i = 0
    while i < len(out):
        if not unit_start_re.match(fscan.strip_comment(out[i]).strip()):
            i += 1
            continue
        us = i
        ue = i
        j = i + 1
        while j < len(out):
            if unit_end_re.match(fscan.strip_comment(out[j]).strip()):
                ue = j
                break
            j += 1
        if ue == us:
            i += 1
            continue

        # find spec-part insertion point
        ins = us + 1
        while ins < ue:
            s = fscan.strip_comment(out[ins]).strip()
            if not s:
                ins += 1
                continue
            if re.match(r"^\s*use\b", s, re.IGNORECASE) or re.match(r"^\s*implicit\b", s, re.IGNORECASE):
                ins += 1
                continue
            if "::" in s:
                ins += 1
                continue
            break

        # collect current names to avoid collisions
        used_names = set()
        for k in range(us, ue + 1):
            used_names.update(fscan.parse_declared_names_from_decl(fscan.strip_comment(out[k])))

        # collect literal counts
        counts = {}
        for k in range(us, ue + 1):
            code = fscan.strip_comment(out[k])
            if "open(" not in code.lower():
                continue
            for m in open_file_re.finditer(code):
                v = m.group("val")
                counts[v] = counts.get(v, 0) + 1
        targets = [v for v, c in counts.items() if c >= min_count]
        if not targets:
            i = ue + 1
            continue

        lit_to_name = {}
        for lit in targets:
            base = re.sub(r"[^a-z0-9]+", "_", lit.lower()).strip("_")
            if not base:
                base = "file"
            cand = f"{base}_path"
            n = 2
            while cand in used_names:
                cand = f"{base}_path_{n}"
                n += 1
            used_names.add(cand)
            lit_to_name[lit] = cand

        # rewrite open(file="...") occurrences
        for k in range(us, ue + 1):
            code, comment = xunused.split_code_comment(out[k].rstrip("\r\n"))
            if "open(" not in code.lower():
                continue
            changed = False

            def _repl(m: re.Match[str]) -> str:
                nonlocal changed
                lit = m.group("val")
                name = lit_to_name.get(lit)
                if name is None:
                    return m.group(0)
                changed = True
                return f"{m.group(1)}{name}"

            new_code = open_file_re.sub(_repl, code)
            if changed:
                eol = "\n" if out[k].endswith("\n") else ""
                out[k] = f"{new_code}{comment}{eol}"

        # insert parameter declarations
        decls = []
        for lit in targets:
            nm = lit_to_name[lit]
            decls.append(f'character(len=*), parameter :: {nm} = "{lit}"\n')
        out[ins:ins] = decls
        i = ue + 1 + len(decls)
    return out


def remove_unused_local_declarations(lines: List[str]) -> List[str]:
    """Remove unused local declaration entities conservatively."""
    out = list(lines)
    unit_start_re = re.compile(
        r"^\s*(?:[a-z][a-z0-9_()\s=,:]*\s+)?(?:function|subroutine)\b|^\s*program\b",
        re.IGNORECASE,
    )
    unit_end_re = re.compile(r"^\s*end\s+(?:function|subroutine|program)\b", re.IGNORECASE)
    ident_re = re.compile(r"\b([a-z][a-z0-9_]*)\b", re.IGNORECASE)

    i = 0
    while i < len(out):
        if not unit_start_re.match(fscan.strip_comment(out[i]).strip()):
            i += 1
            continue
        us = i
        j = i + 1
        while j < len(out):
            if unit_end_re.match(fscan.strip_comment(out[j]).strip()):
                break
            j += 1
        if j >= len(out):
            break
        ue = j

        decl_by_line = {}
        for k in range(us + 1, ue):
            code = fscan.strip_comment(out[k]).strip()
            if "::" not in code:
                continue
            if not re.match(
                r"^(integer|real|logical|character|complex|type\s*\(|class\s*\(|procedure\b)",
                code,
                re.IGNORECASE,
            ):
                continue
            lhs = code.split("::", 1)[0].lower()
            if "parameter" in lhs or "intent(" in lhs:
                continue
            names = fscan.parse_declared_names_from_decl(code)
            if names:
                decl_by_line[k] = names

        if not decl_by_line:
            i = ue + 1
            continue

        uses = {}
        for nset in decl_by_line.values():
            for n in nset:
                uses[n] = 0

        for k in range(us + 1, ue):
            code = fscan.strip_comment(out[k])
            if k in decl_by_line:
                continue
            for m in ident_re.finditer(code):
                n = m.group(1).lower()
                if n in uses:
                    uses[n] += 1

        for k, names in decl_by_line.items():
            rem = {n for n in names if uses.get(n, 0) == 0}
            if not rem:
                continue
            new_ln, _changed = xunused.rewrite_decl_remove_names(out[k], rem)
            out[k] = "" if new_ln is None else new_ln

        i = ue + 1

    return [ln for ln in out if ln != ""]


def _split_top_level_csv(text: str) -> List[str]:
    out: List[str] = []
    cur: List[str] = []
    depth = 0
    in_s = False
    in_d = False
    for ch in text:
        if ch == "'" and not in_d:
            in_s = not in_s
        elif ch == '"' and not in_s:
            in_d = not in_d
        elif not in_s and not in_d:
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


def remove_redundant_size_dummy_args(lines: List[str]) -> List[str]:
    """Remove redundant integer size dummies for assumed-shape args.

    Example: `function f(x, n)` or `subroutine s(x, n)` with `x(:)` and
    `integer, intent(in) :: n`, where `n` is only used as a size proxy, is
    rewritten to omit `n` and uses become `size(x)`.
    """
    out = list(lines)
    p_hdr_re = re.compile(
        r"^(?P<indent>\s*)(?P<prefix>(?:(?:pure|elemental|impure|recursive|module)\s+)*(?:function|subroutine)\s+)(?P<name>[a-z][a-z0-9_]*)\s*\((?P<args>[^)]*)\)(?P<rest>.*)$",
        re.IGNORECASE,
    )
    p_end_re = re.compile(r"^\s*end\s+(?:function|subroutine)\b", re.IGNORECASE)

    i = 0
    while i < len(out):
        code_i, comment_i = xunused.split_code_comment(out[i].rstrip("\r\n"))
        mh = p_hdr_re.match(code_i.strip())
        if mh is None:
            i += 1
            continue
        fname = mh.group("name").lower()
        args = _split_top_level_csv(mh.group("args"))
        if len(args) < 2:
            i += 1
            continue
        j = i + 1
        while j < len(out):
            if p_end_re.match(fscan.strip_comment(out[j]).strip()):
                break
            j += 1
        if j >= len(out):
            break

        # collect declarations
        decl_lines = {}
        for k in range(i + 1, j):
            code_k = fscan.strip_comment(out[k]).strip()
            if "::" not in code_k:
                continue
            for n in fscan.parse_declared_names_from_decl(code_k):
                decl_lines[n] = k

        # try each integer intent(in) dummy as redundant size arg for some array arg
        removed = False
        int_in_dummies: List[str] = []
        for a in args:
            al = a.lower()
            dk0 = decl_lines.get(al)
            if dk0 is None:
                continue
            dcode0 = fscan.strip_comment(out[dk0]).lower()
            if "integer" in dcode0 and "intent(in)" in dcode0:
                int_in_dummies.append(al)
        # Conservative: only rewrite when there is exactly one integer size-like
        # dummy. Multiple integer dummies often encode independent extents.
        if len(int_in_dummies) != 1:
            i = j + 1
            continue
        for nd in list(args):
            nlow = nd.lower()
            dk = decl_lines.get(nlow)
            if dk is None:
                continue
            dcode = fscan.strip_comment(out[dk]).lower()
            if "integer" not in dcode or "intent(in)" not in dcode:
                continue
            # find assumed-shape dummy args. This rewrite is only safe when there
            # is exactly one such arg (avoid flattened multi-array kernels where
            # size dummies encode matrix extents independently).
            arr_candidates: List[str] = []
            for a in args:
                al = a.lower()
                if al == nlow:
                    continue
                da = decl_lines.get(al)
                if da is None:
                    continue
                acode = fscan.strip_comment(out[da]).lower()
                if re.search(rf"\b{re.escape(al)}\s*\(\s*:\s*\)", acode):
                    arr_candidates.append(al)
            if len(arr_candidates) != 1:
                continue
            arr = arr_candidates[0]

            # ensure n is not used in declarations other than its own
            bad = False
            for k in range(i + 1, j):
                if k == dk:
                    continue
                c = fscan.strip_comment(out[k])
                if "::" in c and re.search(rf"\b{re.escape(nlow)}\b", c, re.IGNORECASE):
                    bad = True
                    break
            if bad:
                continue

            # rewrite executable uses n -> size(arr)
            pat_n = re.compile(rf"\b{re.escape(nlow)}\b", re.IGNORECASE)
            for k in range(i + 1, j):
                c, com = xunused.split_code_comment(out[k].rstrip("\r\n"))
                if "::" in c:
                    continue
                if not pat_n.search(c):
                    continue
                c = pat_n.sub(f"size({arr})", c)
                eol = "\n" if out[k].endswith("\n") else ""
                out[k] = f"{c}{com}{eol}"

            # remove n from header arg list
            new_args = [a for a in args if a.lower() != nlow]
            eol = "\n" if out[i].endswith("\n") else ""
            stripped = code_i.strip()
            # rebuild from match parts on stripped header
            out[i] = f"{mh.group('indent')}{mh.group('prefix')}{mh.group('name')}({', '.join(new_args)}){mh.group('rest')}{comment_i}{eol}"

            # remove n from declaration line
            new_ln, _chg = xunused.rewrite_decl_remove_names(out[dk], {nlow})
            out[dk] = "" if new_ln is None else new_ln

            # rewrite call sites and expression invocations globally (simple)
            call_pat = re.compile(rf"\b{re.escape(fname)}\s*\(([^()]*)\)", re.IGNORECASE)
            n_idx = [idx for idx, a in enumerate(args) if a.lower() == nlow][0]
            for k in range(len(out)):
                if k >= i and k <= j:
                    continue
                c, com = xunused.split_code_comment(out[k].rstrip("\r\n"))
                if fname not in c.lower():
                    continue

                def _repl(m: re.Match[str]) -> str:
                    argv = _split_top_level_csv(m.group(1))
                    if len(argv) <= n_idx:
                        return m.group(0)
                    del argv[n_idx]
                    return f"{fname}({', '.join(argv)})"

                nc = call_pat.sub(_repl, c)
                if nc != c:
                    eol2 = "\n" if out[k].endswith("\n") else ""
                    out[k] = f"{nc}{com}{eol2}"

            removed = True
            break
        if removed:
            out = [ln for ln in out if ln != ""]
            # restart scan conservatively
            i = 0
            continue
        i = j + 1
    return out


def promote_pure_scalar_subroutines_to_elemental(lines: List[str]) -> List[str]:
    """Promote `pure subroutine` to `elemental subroutine` when all dummies are scalar.

    Conservative rules:
    - unit declaration must explicitly contain both `pure` and `subroutine`
    - every dummy in signature must be declared in the unit
    - no dummy is declared as an array via `dimension(...)` attribute or `name(...)`
    """
    out = list(lines)
    p_start_re = re.compile(
        r"^(?P<indent>\s*)(?P<prefix>(?:(?:pure|elemental|impure|recursive|module)\s+)*subroutine\s+)(?P<name>[a-z][a-z0-9_]*)\s*\((?P<args>[^)]*)\)(?P<rest>.*)$",
        re.IGNORECASE,
    )
    p_end_re = re.compile(r"^\s*end\s+subroutine\b", re.IGNORECASE)
    for i, ln in enumerate(out):
        code = fscan.strip_comment(ln).strip()
        m = p_start_re.match(code)
        if not m:
            continue
        prefix = m.group("prefix")
        low_prefix = prefix.lower()
        if "pure" not in low_prefix or "elemental" in low_prefix:
            continue
        args_txt = m.group("args").strip()
        if not args_txt:
            continue
        dummies = [a.strip().lower() for a in args_txt.split(",") if a.strip()]
        if not dummies:
            continue
        j = i + 1
        while j < len(out):
            code_j = fscan.strip_comment(out[j]).strip()
            if p_end_re.match(code_j):
                break
            j += 1
        if j >= len(out):
            continue

        seen: Dict[str, bool] = {}
        scalar_ok = True
        decl_re = re.compile(r"^\s*([^!]*?)::\s*(.+)$")
        for k in range(i + 1, j):
            code_k = fscan.strip_comment(out[k]).strip()
            md = decl_re.match(code_k)
            if not md:
                continue
            lhs = md.group(1).lower()
            rhs = md.group(2)
            has_dim_attr = "dimension(" in lhs
            ents = [e.strip() for e in rhs.split(",") if e.strip()]
            for ent in ents:
                mname = re.match(r"^\s*([a-z][a-z0-9_]*)", ent, re.IGNORECASE)
                if not mname:
                    continue
                nm = mname.group(1).lower()
                if nm not in dummies:
                    continue
                is_array = has_dim_attr or ("(" in ent and ")" in ent)
                seen[nm] = True
                if is_array:
                    scalar_ok = False
        if not scalar_ok:
            continue
        if any(nm not in seen for nm in dummies):
            continue

        # apply on full line to preserve original spacing/comments
        line = out[i]
        out[i] = re.sub(r"\bpure\s+subroutine\b", "elemental subroutine", line, count=1, flags=re.IGNORECASE)
    return out
