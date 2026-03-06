#!/usr/bin/env python3
"""Shared post-processing rewrites for generated Fortran code."""

from __future__ import annotations

import re
from typing import List

import fortran_scan as fscan
import xunused

# Internal style toggles (kept local to post-processing behavior).
ENABLE_SAFE_SQUARE_REWRITE = True


def simplify_do_while_true(lines: List[str]) -> List[str]:
    """Rewrite unconditional `do while (.true.)` loops to plain `do`.

    Conservative rewrite:
    - Match only exact `.true.` condition (case-insensitive, optional spaces/parens).
    - Preserve indentation and trailing comments.
    """
    out = list(lines)
    pat = re.compile(r"^(?P<indent>\s*)do\s+while\s*\(\s*\.true\.\s*\)\s*$", re.IGNORECASE)
    for i, ln in enumerate(out):
        code, comment = xunused.split_code_comment(ln.rstrip("\r\n"))
        m = pat.match(code)
        if not m:
            continue
        eol = xunused.get_eol(ln) or ("\n" if ln.endswith("\n") else "")
        out[i] = f"{m.group('indent')}do{comment}{eol}"
    return out


def collapse_single_stmt_if_blocks(lines: List[str]) -> List[str]:
    """Collapse 3-line IF blocks with one executable body statement.

    This keeps the behavior of the standalone `xone_line_if.py` workflow,
    but as a reusable post-processing step.
    """
    return fscan.collapse_single_stmt_if_blocks(lines)


def ensure_blank_line_between_module_procedures(lines: List[str]) -> List[str]:
    """Ensure at least one blank line between consecutive module procedures."""
    out: List[str] = []
    module_start_re = re.compile(r"^\s*module\b(?!\s+procedure\b)", re.IGNORECASE)
    module_end_re = re.compile(r"^\s*end\s+module\b", re.IGNORECASE)
    contains_re = re.compile(r"^\s*contains\s*$", re.IGNORECASE)
    proc_hdr_re = re.compile(
        r"^\s*(?:(?:pure|elemental|impure|recursive|module)\s+)*(?:function|subroutine)\b",
        re.IGNORECASE,
    )
    proc_end_re = re.compile(r"^\s*end\s+(?:function|subroutine)\b", re.IGNORECASE)

    in_module = False
    in_contains = False
    blank_line = "\n" if any(("\n" in ln) or ("\r" in ln) for ln in lines) else ""

    def _code_only(ln: str) -> str:
        return fscan.strip_comment(ln).strip()

    def _is_blank(ln: str) -> bool:
        return not ln.strip()

    for i, ln in enumerate(lines):
        code = _code_only(ln)
        low = code.lower()

        if module_start_re.match(code) and not low.startswith("end module"):
            in_module = True
            in_contains = False
        elif in_module and contains_re.match(code):
            in_contains = True
        elif in_module and module_end_re.match(code):
            in_module = False
            in_contains = False

        out.append(ln)

        if not (in_module and in_contains and proc_end_re.match(code)):
            continue

        # Check whether the next procedure header appears without any blank
        # separating lines (comments-only lines do not count as blank).
        j = i + 1
        saw_blank = False
        while j < len(lines):
            code_j = _code_only(lines[j])
            if _is_blank(lines[j]):
                saw_blank = True
                j += 1
                continue
            if not code_j:
                j += 1
                continue
            break
        if j < len(lines):
            code_j = _code_only(lines[j])
            if proc_hdr_re.match(code_j) and not saw_blank:
                out.append(blank_line)

    return out


def ensure_blank_line_between_program_units(lines: List[str]) -> List[str]:
    """Ensure at least one blank line between adjacent top-level modules/programs."""
    out: List[str] = []
    re_end_unit = re.compile(r"^\s*end\s+(module|program)\b", flags=re.IGNORECASE)
    re_start_unit = re.compile(r"^\s*(module|program)\b", flags=re.IGNORECASE)
    i = 0
    n = len(lines)
    while i < n:
        ln = lines[i]
        out.append(ln)
        if re_end_unit.match(ln):
            j = i + 1
            while j < n and lines[j].strip() == "":
                j += 1
            if j < n and re_start_unit.match(lines[j]):
                if not out or out[-1].strip() != "":
                    out.append("\n" if any(("\n" in x) or ("\r" in x) for x in lines) else "")
        i += 1
    return out


def simplify_norm2_patterns(lines: List[str]) -> List[str]:
    """Rewrite simple Euclidean norm patterns to `norm2(...)`.

    Conservative rewrite:
    - `sqrt(sum(v**2))` -> `norm2(v)`
    - `sqrt(sum((v)**2))` -> `norm2(v)`
    where `v` is a simple identifier.
    """
    out: List[str] = []
    pat = re.compile(
        r"\bsqrt\s*\(\s*sum\s*\(\s*\(?\s*([A-Za-z_][A-Za-z0-9_]*)\s*\)?\s*\*\*\s*2\s*\)\s*\)",
        re.IGNORECASE,
    )
    p_start_re = re.compile(
        r"^\s*(?:(?:pure|elemental|impure|recursive|module)\s+)*(?:function|subroutine)\b",
        re.IGNORECASE,
    )
    p_end_re = re.compile(r"^\s*end\s+(?:function|subroutine)\b", re.IGNORECASE)
    decl_re = re.compile(r"^\s*([^!]*?)::\s*(.+)$")

    def _split_decl_entities(rhs: str) -> List[str]:
        parts: List[str] = []
        cur: List[str] = []
        depth = 0
        for ch in rhs:
            if ch == "(":
                depth += 1
            elif ch == ")":
                depth = max(0, depth - 1)
            if ch == "," and depth == 0:
                parts.append("".join(cur).strip())
                cur = []
            else:
                cur.append(ch)
        if cur:
            parts.append("".join(cur).strip())
        return [p for p in parts if p]

    in_proc = False
    array_names: set[str] = set()
    for raw in lines:
        code, comment = xunused.split_code_comment(raw.rstrip("\r\n"))
        eol = xunused.get_eol(raw) or ("\n" if raw.endswith("\n") else "")
        if p_start_re.match(code):
            in_proc = True
            array_names = set()
        if in_proc:
            md = decl_re.match(code)
            if md:
                lhs = md.group(1).lower()
                rhs = md.group(2)
                has_dim_attr = "dimension(" in lhs
                for ent in _split_decl_entities(rhs):
                    mname = re.match(r"\s*([A-Za-z_][A-Za-z0-9_]*)", ent)
                    if not mname:
                        continue
                    nm = mname.group(1).lower()
                    if has_dim_attr or ("(" in ent and ")" in ent):
                        array_names.add(nm)

        def _repl(m: re.Match[str]) -> str:
            nm = m.group(1).lower()
            if in_proc and (nm in array_names):
                return f"norm2({m.group(1)})"
            return m.group(0)

        code = pat.sub(_repl, code)
        out.append(f"{code}{comment}{eol}")
        if p_end_re.match(code):
            in_proc = False
            array_names = set()
    return out


def rewrite_named_arguments(lines: List[str], *, max_positional: int = 3) -> List[str]:
    """Rewrite eligible positional call arguments into named arguments."""
    out, _nchg, _missing = fscan.rewrite_named_arguments_in_lines(
        lines,
        max_positional=max_positional,
        name_optional=False,
        name_after_positional_limit=False,
    )
    return out


def wrap_long_lines(lines: List[str], *, max_len: int = 80) -> List[str]:
    """Wrap long Fortran statements using continuation lines."""
    return fscan.wrap_long_fortran_lines(lines, max_len=max_len)


def tighten_unary_minus_literal_spacing(lines: List[str]) -> List[str]:
    """Collapse spaced unary-minus numeric literals, e.g. `- 1.2_dp` -> `-1.2_dp`."""
    out: List[str] = []
    # unary contexts: start-of-code, after opener/delimiter/operator
    pat = re.compile(
        r"(^|[=(,\[*/+\-]\s*)-\s+"
        r"((?:\d+\.\d*|\.\d+|\d+)(?:[eEdD][+\-]?\d+)?(?:_[A-Za-z][A-Za-z0-9_]*)?)"
    )
    for raw in lines:
        code, comment = xunused.split_code_comment(raw.rstrip("\r\n"))
        eol = xunused.get_eol(raw) or ("\n" if raw.endswith("\n") else "")
        code = pat.sub(lambda m: f"{m.group(1)}-{m.group(2)}", code)
        out.append(f"{code}{comment}{eol}")
    return out


def normalize_delimiter_inner_spacing(lines: List[str]) -> List[str]:
    """Remove unnecessary spaces just inside () and [] outside strings/comments."""
    out: List[str] = []
    for raw in lines:
        code, comment = xunused.split_code_comment(raw.rstrip("\r\n"))
        eol = xunused.get_eol(raw) or ("\n" if raw.endswith("\n") else "")
        chars = list(code)
        i = 0
        in_single = False
        in_double = False
        cleaned: List[str] = []
        while i < len(chars):
            ch = chars[i]
            if ch == "'" and not in_double:
                in_single = not in_single
                cleaned.append(ch)
                i += 1
                continue
            if ch == '"' and not in_single:
                in_double = not in_double
                cleaned.append(ch)
                i += 1
                continue
            if in_single or in_double:
                cleaned.append(ch)
                i += 1
                continue
            if ch in "([":
                cleaned.append(ch)
                i += 1
                # drop spaces/tabs after opener
                while i < len(chars) and chars[i] in {" ", "\t"}:
                    i += 1
                continue
            if ch in ")]":
                # trim spaces/tabs before closer
                while cleaned and cleaned[-1] in {" ", "\t"}:
                    cleaned.pop()
                cleaned.append(ch)
                i += 1
                continue
            cleaned.append(ch)
            i += 1
        out.append("".join(cleaned) + comment + eol)
    return out


def hoist_module_use_only_imports(lines: List[str]) -> List[str]:
    """Hoist simple procedure-level `use mod, only: ...` into module scope."""
    out = list(lines)
    mod_start_re = re.compile(r"^\s*module\s+([a-z][a-z0-9_]*)\b(?!\s*procedure\b)", re.IGNORECASE)
    mod_end_re = re.compile(r"^\s*end\s+module\b", re.IGNORECASE)
    contains_re = re.compile(r"^\s*contains\s*$", re.IGNORECASE)
    proc_start_re = re.compile(
        r"^\s*(?:(?:pure|elemental|impure|recursive|module)\s+)*(?:function|subroutine)\b",
        re.IGNORECASE,
    )
    proc_end_re = re.compile(r"^\s*end\s+(?:function|subroutine)\b", re.IGNORECASE)
    use_only_re = re.compile(
        r"^\s*use\s*(?:,\s*(?P<intrinsic>intrinsic|non_intrinsic)\s*)?"
        r"(?:::)?\s*(?P<mod>[a-z][a-z0-9_]*)\s*,\s*only\s*:\s*(?P<syms>.+?)\s*$",
        re.IGNORECASE,
    )
    implicit_none_re = re.compile(r"^\s*implicit\s+none\b", re.IGNORECASE)

    def _parse_syms(s: str) -> List[str]:
        parts = [p.strip() for p in s.split(",")]
        out_syms: List[str] = []
        for p in parts:
            if not p:
                continue
            # Skip renames/operators conservatively.
            if "=>" in p or "operator(" in p.lower() or "assignment(" in p.lower():
                return []
            if not re.match(r"^[a-z][a-z0-9_]*$", p, re.IGNORECASE):
                return []
            out_syms.append(p)
        return out_syms

    i = 0
    n = len(out)
    while i < n:
        m0 = mod_start_re.match(fscan.strip_comment(out[i]).strip())
        if not m0:
            i += 1
            continue
        j = i + 1
        contains_idx = -1
        while j < n:
            code_j = fscan.strip_comment(out[j]).strip()
            if contains_re.match(code_j):
                contains_idx = j
            if mod_end_re.match(code_j):
                break
            j += 1
        if j >= n or contains_idx < 0:
            i = j + 1
            continue
        mod_end = j

        # Existing module-level use-only imports.
        module_use_lines: Dict[str, int] = {}
        module_syms: Dict[str, set[str]] = {}
        use_indent = None
        insert_idx = None
        for k in range(i + 1, contains_idx):
            code_k, _ = xunused.split_code_comment(out[k].rstrip("\r\n"))
            mk = use_only_re.match(code_k.strip())
            if mk is not None and mk.group("intrinsic") is None:
                mod_nm = mk.group("mod").lower()
                syms = _parse_syms(mk.group("syms"))
                if syms:
                    module_use_lines[mod_nm] = k
                    module_syms.setdefault(mod_nm, set()).update(s.lower() for s in syms)
                    if use_indent is None:
                        use_indent = re.match(r"^(\s*)", code_k).group(1)
            if insert_idx is None and implicit_none_re.match(code_k):
                insert_idx = k
        if use_indent is None:
            use_indent = re.match(r"^(\s*)", out[contains_idx]).group(1)
        if insert_idx is None:
            insert_idx = contains_idx

        # Scan procedures for hoistable use-only lines.
        hoist: Dict[str, set[str]] = {}
        remove_lines: set[int] = set()
        p = contains_idx + 1
        while p < mod_end:
            code_p = fscan.strip_comment(out[p]).strip()
            if not proc_start_re.match(code_p):
                p += 1
                continue
            q = p + 1
            while q < mod_end:
                code_q, _ = xunused.split_code_comment(out[q].rstrip("\r\n"))
                mq = use_only_re.match(code_q.strip())
                if mq is not None and mq.group("intrinsic") is None:
                    mod_nm = mq.group("mod").lower()
                    syms = _parse_syms(mq.group("syms"))
                    if syms:
                        hoist.setdefault(mod_nm, set()).update(s.lower() for s in syms)
                        remove_lines.add(q)
                if proc_end_re.match(fscan.strip_comment(out[q]).strip()):
                    break
                q += 1
            p = q + 1

        if not hoist:
            i = mod_end + 1
            continue

        # Apply merged module use lists.
        for mod_nm, syms in hoist.items():
            merged = sorted(module_syms.get(mod_nm, set()).union(syms))
            if mod_nm in module_use_lines:
                k = module_use_lines[mod_nm]
                eol = xunused.get_eol(out[k]) or ("\n" if out[k].endswith("\n") else "")
                out[k] = f"{use_indent}use {mod_nm}, only: {', '.join(merged)}{eol}"
            else:
                eol = "\n" if any("\n" in ln or "\r" in ln for ln in out) else ""
                out.insert(insert_idx, f"{use_indent}use {mod_nm}, only: {', '.join(merged)}{eol}")
                # Shift pending line numbers after insertion.
                remove_lines = {ln + 1 if ln >= insert_idx else ln for ln in remove_lines}
                insert_idx += 1
                contains_idx += 1
                mod_end += 1

        # Remove hoisted procedure-local lines.
        for ln in sorted(remove_lines, reverse=True):
            del out[ln]
            if ln <= mod_end:
                mod_end -= 1

        n = len(out)
        i = mod_end + 1

    return out


def apply_xindent_defaults(lines: List[str], *, max_len: int = 80) -> List[str]:
    """Apply xindent.py default indentation/wrapping policy to emitted lines."""
    plain_lines = [ln.rstrip("\r\n") for ln in lines]
    src = ("\n".join(plain_lines) + "\n") if plain_lines else ""
    src_lines = src.splitlines()
    dst = fscan.indent_fortran_blocks(
        src,
        indent_step=3,
        indent_proc=False,
        indent_module=False,
        indent_program=False,
        indent_contains=False,
    )
    dst_lines = dst.splitlines()

    def _max_leading_spaces(ls: List[str]) -> int:
        m = 0
        for ln in ls:
            if not ln:
                continue
            n = len(ln) - len(ln.lstrip(" "))
            if n > m:
                m = n
        return m

    src_max = _max_leading_spaces(src_lines)
    dst_max = _max_leading_spaces(dst_lines)
    if dst_max > max(512, src_max + 120):
        dst_lines = src_lines

    # Match xindent.py low-risk wrapping behavior.
    decl_like_re = re.compile(
        r"^\s*(integer|real|logical|complex|character|type\s*\([^)]+\))\b.*::",
        re.IGNORECASE,
    )
    use_like_re = re.compile(r"^\s*use\b", re.IGNORECASE)
    wrapped: List[str] = []
    for ln in dst_lines:
        if len(ln) <= max_len:
            wrapped.append(ln)
            continue
        if ("!" in ln) or ("'" in ln) or ('"' in ln) or ("//" in ln) or ("&" in ln):
            wrapped.append(ln)
            continue
        if ("**" in ln) or (";" in ln):
            wrapped.append(ln)
            continue
        if decl_like_re.match(ln):
            wrapped.extend(fscan.wrap_long_declaration_lines([ln], max_len=max_len))
            continue
        if use_like_re.match(ln):
            wrapped.extend(fscan.wrap_long_fortran_lines([ln], max_len=max_len))
            continue
        wrapped.append(ln)
    return wrapped


def simplify_bfgs_rank1_update(lines: List[str]) -> List[str]:
    """Simplify over-expanded scalar-broadcast terms in BFGS rank-1 updates."""
    out: List[str] = []
    term_outer_ss = (
        "spread(s, dim=2, ncopies=size(s)) * spread(s, dim=1, ncopies=size(s))"
    )
    term_outer_hys = (
        "spread(hy, dim=2, ncopies=size(s)) * spread(s, dim=1, ncopies=size(hy)) + "
        "spread(s, dim=2, ncopies=size(hy)) * spread(hy, dim=1, ncopies=size(s))"
    )
    pat1 = (
        "spread((ys + real(matmul(y, hy), kind=dp)) / ys**2, dim=1, "
        f"ncopies=size({term_outer_ss},1)) * {term_outer_ss}"
    )
    rep1 = f"((ys + dot_product(y, hy)) / (ys * ys)) * {term_outer_ss}"
    pat2 = f"({term_outer_hys}) / spread(ys, dim=1, ncopies=size(({term_outer_hys}),1))"
    rep2 = f"({term_outer_hys}) / ys"

    for raw in lines:
        code, comment = xunused.split_code_comment(raw.rstrip("\r\n"))
        eol = xunused.get_eol(raw) or ("\n" if raw.endswith("\n") else "")
        if pat1 in code:
            code = code.replace(pat1, rep1)
        if pat2 in code:
            code = code.replace(pat2, rep2)
        out.append(f"{code}{comment}{eol}")
    return out


def simplify_redundant_parentheses(lines: List[str]) -> List[str]:
    """Conservative wrapper for unneeded-parentheses cleanup.

    Delegates to shared scan-time logic so rewrite behavior stays centralized.
    """
    out = fscan.simplify_redundant_parens_in_lines(lines)

    def _split_top_level_add_ops(expr: str):
        def _is_exp_sign(pos: int) -> bool:
            # Detect signs in scientific notation, e.g. 1e-16 or 1d+03.
            j = pos - 1
            while j >= 0 and expr[j].isspace():
                j -= 1
            if j < 1 or expr[j] not in "eEdD":
                return False
            k = j - 1
            while k >= 0 and expr[k].isspace():
                k -= 1
            if k < 0 or not (expr[k].isdigit() or expr[k] == "."):
                return False
            t = pos + 1
            while t < len(expr) and expr[t].isspace():
                t += 1
            return t < len(expr) and expr[t].isdigit()

        terms: List[str] = []
        ops: List[str] = []
        cur: List[str] = []
        depth = 0
        in_single = False
        in_double = False
        i = 0
        while i < len(expr):
            ch = expr[i]
            if ch == "'" and not in_double:
                if in_single and i + 1 < len(expr) and expr[i + 1] == "'":
                    cur.append(expr[i : i + 2]); i += 2; continue
                in_single = not in_single
                cur.append(ch); i += 1; continue
            if ch == '"' and not in_single:
                if in_double and i + 1 < len(expr) and expr[i + 1] == '"':
                    cur.append(expr[i : i + 2]); i += 2; continue
                in_double = not in_double
                cur.append(ch); i += 1; continue
            if in_single or in_double:
                cur.append(ch); i += 1; continue
            if ch == "(":
                depth += 1; cur.append(ch); i += 1; continue
            if ch == ")":
                depth = max(0, depth - 1); cur.append(ch); i += 1; continue
            if ch in "+-" and depth == 0:
                if _is_exp_sign(i):
                    cur.append(ch); i += 1; continue
                # Treat as binary op only when flanked by expression text.
                left = "".join(cur).rstrip()
                j = i + 1
                while j < len(expr) and expr[j].isspace():
                    j += 1
                if left and j < len(expr):
                    terms.append("".join(cur).strip())
                    ops.append(ch)
                    cur = []
                    i += 1
                    continue
            cur.append(ch)
            i += 1
        terms.append("".join(cur).strip())
        return terms, ops

    def _has_top_level_binary_add_sub(expr: str) -> bool:
        depth = 0
        in_single = False
        in_double = False
        prev_sig = ""
        i = 0
        while i < len(expr):
            ch = expr[i]
            if ch == "'" and not in_double:
                if in_single and i + 1 < len(expr) and expr[i + 1] == "'":
                    i += 2
                    continue
                in_single = not in_single
                i += 1
                continue
            if ch == '"' and not in_single:
                if in_double and i + 1 < len(expr) and expr[i + 1] == '"':
                    i += 2
                    continue
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
            if ch == ")":
                depth = max(0, depth - 1)
                i += 1
                continue
            if depth == 0 and ch in "+-":
                # Unary sign is allowed; detect binary +/-
                if prev_sig and prev_sig not in "(*/+-=,:":  # binary op
                    return True
            if not ch.isspace():
                prev_sig = ch
            i += 1
        return False

    def _has_top_level_comma(expr: str) -> bool:
        depth = 0
        in_single = False
        in_double = False
        i = 0
        while i < len(expr):
            ch = expr[i]
            if ch == "'" and not in_double:
                if in_single and i + 1 < len(expr) and expr[i + 1] == "'":
                    i += 2
                    continue
                in_single = not in_single
                i += 1
                continue
            if ch == '"' and not in_single:
                if in_double and i + 1 < len(expr) and expr[i + 1] == '"':
                    i += 2
                    continue
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
            if ch == ")":
                depth = max(0, depth - 1)
                i += 1
                continue
            if depth == 0 and ch == ",":
                return True
            i += 1
        return False

    def _strip_simple_parenthesized_sections(expr: str) -> str:
        # Remove grouping parens only when the inner expression has no top-level
        # binary +/- and no top-level comma, and context is not a call/index.
        s = expr
        while True:
            changed = False
            stack: List[int] = []
            pairs: List[tuple[int, int]] = []
            in_single = False
            in_double = False
            i = 0
            while i < len(s):
                ch = s[i]
                if ch == "'" and not in_double:
                    if in_single and i + 1 < len(s) and s[i + 1] == "'":
                        i += 2
                        continue
                    in_single = not in_single
                    i += 1
                    continue
                if ch == '"' and not in_single:
                    if in_double and i + 1 < len(s) and s[i + 1] == '"':
                        i += 2
                        continue
                    in_double = not in_double
                    i += 1
                    continue
                if in_single or in_double:
                    i += 1
                    continue
                if ch == "(":
                    stack.append(i)
                elif ch == ")" and stack:
                    pairs.append((stack.pop(), i))
                i += 1
            for lpos, rpos in reversed(pairs):
                inner = s[lpos + 1 : rpos].strip()
                if not inner:
                    continue
                prev = s[lpos - 1] if lpos > 0 else ""
                next_ch = s[rpos + 1] if rpos + 1 < len(s) else ""
                # likely call/index context: name(...), arr(...), dt%comp(...)
                if prev.isalnum() or prev in "_%)]":
                    continue
                # likely intrinsic operator call style with no separator
                if next_ch and (next_ch.isalnum() or next_ch == "_"):
                    continue
                if _has_top_level_comma(inner):
                    continue
                if _has_top_level_binary_add_sub(inner):
                    continue
                s = s[:lpos] + inner + s[rpos + 1 :]
                changed = True
                break
            if not changed:
                break
        return s

    def _split_top_level_relop(expr: str):
        # Return (lhs, op, rhs) for a top-level relational operator if present.
        ops = ["<=", ">=", "==", "/=", "<", ">"]
        depth = 0
        in_single = False
        in_double = False
        i = 0
        while i < len(expr):
            ch = expr[i]
            if ch == "'" and not in_double:
                if in_single and i + 1 < len(expr) and expr[i + 1] == "'":
                    i += 2
                    continue
                in_single = not in_single
                i += 1
                continue
            if ch == '"' and not in_single:
                if in_double and i + 1 < len(expr) and expr[i + 1] == '"':
                    i += 2
                    continue
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
            if ch == ")":
                depth = max(0, depth - 1)
                i += 1
                continue
            if depth == 0:
                for op in ops:
                    if expr.startswith(op, i):
                        lhs = expr[:i].rstrip()
                        rhs = expr[i + len(op) :].lstrip()
                        return lhs, op, rhs
            i += 1
        return None

    def _rewrite_safe_mul_self_to_pow2(expr: str) -> str:
        # Conservative: only rewrite simple scalar-like atoms, e.g. x*x, obj%v*obj%v.
        # Do not touch array refs/calls, character literals, or complex syntax.
        if not ENABLE_SAFE_SQUARE_REWRITE:
            return expr
        atom = r"[A-Za-z_][A-Za-z0-9_]*(?:%[A-Za-z_][A-Za-z0-9_]*)*"
        pat = re.compile(
            rf"(?<![A-Za-z0-9_%])(?P<a>{atom})\s*(?<!\*)\*(?!\*)\s*(?P=a)(?![A-Za-z0-9_%(])"
        )

        def _sub_outside_quotes(text: str) -> str:
            out_parts: List[str] = []
            cur: List[str] = []
            in_single = False
            in_double = False
            i = 0
            while i < len(text):
                ch = text[i]
                if ch == "'" and not in_double:
                    if in_single and i + 1 < len(text) and text[i + 1] == "'":
                        # Doubled quote inside string
                        cur.append(text[i : i + 2])
                        i += 2
                        continue
                    if in_single:
                        cur.append(ch)
                        out_parts.append("".join(cur))
                        cur = []
                        in_single = False
                    else:
                        # Flush code segment before entering quote.
                        code_seg = "".join(cur)
                        out_parts.append(pat.sub(lambda m: f"{m.group('a')}**2", code_seg))
                        cur = [ch]
                        in_single = True
                    i += 1
                    continue
                if ch == '"' and not in_single:
                    if in_double and i + 1 < len(text) and text[i + 1] == '"':
                        cur.append(text[i : i + 2])
                        i += 2
                        continue
                    if in_double:
                        cur.append(ch)
                        out_parts.append("".join(cur))
                        cur = []
                        in_double = False
                    else:
                        code_seg = "".join(cur)
                        out_parts.append(pat.sub(lambda m: f"{m.group('a')}**2", code_seg))
                        cur = [ch]
                        in_double = True
                    i += 1
                    continue
                cur.append(ch)
                i += 1
            tail = "".join(cur)
            if in_single or in_double:
                out_parts.append(tail)
            else:
                out_parts.append(pat.sub(lambda m: f"{m.group('a')}**2", tail))
            return "".join(out_parts)

        return _sub_outside_quotes(expr)

    cleaned: List[str] = []
    for ln in out:
        code, comment = xunused.split_code_comment(ln.rstrip("\r\n"))
        eol = xunused.get_eol(ln) or ("\n" if ln.endswith("\n") else "")
        m_asn = re.match(r"^(\s*[^=]+?=\s*)(.+)$", code)
        if m_asn and "::" not in code and "=>" not in code and not re.match(r"^\s*(if|do|where|forall|select)\b", code, re.IGNORECASE):
            lhs = m_asn.group(1)
            rhs = m_asn.group(2).rstrip()
            rhs = fscan.strip_redundant_outer_parens_expr(rhs)
            rhs = _strip_simple_parenthesized_sections(rhs)
            rhs = _rewrite_safe_mul_self_to_pow2(rhs)
            terms, ops = _split_top_level_add_ops(rhs)
            if len(terms) > 1:
                terms = [fscan.strip_redundant_outer_parens_expr(t) for t in terms]
                rhs2 = terms[0]
                for op, t in zip(ops, terms[1:]):
                    rhs2 += f" {op} {t}"
                rhs = rhs2
            code = lhs + rhs
        else:
            # Also simplify redundant parentheses inside IF conditions.
            m_if = re.match(r"^(\s*if\s*\()(.+)(\)\s*(?:then|return|cycle|exit|call\b.*)?)$", code, re.IGNORECASE)
            if m_if:
                pre = m_if.group(1)
                cond = m_if.group(2).rstrip()
                post = m_if.group(3)
                cond = fscan.strip_redundant_outer_parens_expr(cond)
                cond = _strip_simple_parenthesized_sections(cond)
                cond = _rewrite_safe_mul_self_to_pow2(cond)
                terms, ops = _split_top_level_add_ops(cond)
                if len(terms) > 1:
                    terms = [fscan.strip_redundant_outer_parens_expr(t) for t in terms]
                    cond2 = terms[0]
                    for op, t in zip(ops, terms[1:]):
                        cond2 += f" {op} {t}"
                    cond = cond2
                rel = _split_top_level_relop(cond)
                if rel is not None:
                    lhs_rel, op_rel, rhs_rel = rel
                    lhs_rel = fscan.strip_redundant_outer_parens_expr(lhs_rel)
                    rhs_rel = fscan.strip_redundant_outer_parens_expr(rhs_rel)
                    cond = f"{lhs_rel} {op_rel} {rhs_rel}"
                code = f"{pre}{cond}{post}"
        cleaned.append(f"{code}{comment}{eol}")
    return cleaned


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
