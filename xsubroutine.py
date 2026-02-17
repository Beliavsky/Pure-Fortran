#!/usr/bin/env python3
"""Extract a marked code block into a subroutine in a separate module."""

from __future__ import annotations

import argparse
import re
import subprocess
from pathlib import Path
from typing import Dict, List, Optional, Set, Tuple

from fortran_semantics import infer_intent_from_access, is_pure_eligible_block

BEGIN_RE = re.compile(
    r"^\s*!+\s*begin\s+(?:(?:(?P<mod>[a-z][a-z0-9_]*)::)?(?P<kind>subroutine|function)\s+(?P<name>[a-z][a-z0-9_]*)|(?P<legacy_mod>[a-z][a-z0-9_]*)::(?P<legacy_name>[a-z][a-z0-9_]*))\s*$",
    re.IGNORECASE,
)
END_RE = re.compile(
    r"^\s*!+\s*end\s+(?:(?:(?P<mod>[a-z][a-z0-9_]*)::)?(?P<kind>subroutine|function)\s+(?P<name>[a-z][a-z0-9_]*)|(?P<legacy_mod>[a-z][a-z0-9_]*)::(?P<legacy_name>[a-z][a-z0-9_]*))\s*$",
    re.IGNORECASE,
)
UNIT_START_RE = re.compile(r"^\s*(program|subroutine|function)\b", re.IGNORECASE)
UNIT_END_RE = re.compile(
    r"^\s*end\s*$|^\s*end\s+(?:program|subroutine|function)(?:\s+[a-z][a-z0-9_]*)?\s*$",
    re.IGNORECASE,
)
DECL_RE = re.compile(
    r"^\s*(integer|real|logical|character|complex|double\s+precision|type\b|class\b|procedure\b)",
    re.IGNORECASE,
)
IDENT_RE = re.compile(r"\b([a-z][a-z0-9_]*)\b", re.IGNORECASE)
ASSIGN_RE = re.compile(r"^\s*([a-z][a-z0-9_]*(?:\s*\([^=]*\))?)\s*=", re.IGNORECASE)
DO_ITER_RE = re.compile(r"^\s*do\b(?:\s+\d+\s+)?\s*([a-z][a-z0-9_]*)\s*=", re.IGNORECASE)
USE_RE = re.compile(r"^\s*use\s+([a-z][a-z0-9_]*)\s*(?:,\s*only\s*:\s*(.*))?$", re.IGNORECASE)
IF_THEN_RE = re.compile(r"^\s*if\s*\(.*\)\s*then\b", re.IGNORECASE)
ELSE_IF_RE = re.compile(r"^\s*else\s*if\s*\(.*\)\s*then\b", re.IGNORECASE)
ELSE_RE = re.compile(r"^\s*else\b", re.IGNORECASE)
END_IF_RE = re.compile(r"^\s*(end\s*if|endif)\b", re.IGNORECASE)
DO_START_RE = re.compile(r"^\s*do\b", re.IGNORECASE)
END_DO_RE = re.compile(r"^\s*end\s*do\b", re.IGNORECASE)
SELECT_START_RE = re.compile(r"^\s*select\s+case\b", re.IGNORECASE)
CASE_RE = re.compile(r"^\s*case\b", re.IGNORECASE)
END_SELECT_RE = re.compile(r"^\s*end\s*select\b", re.IGNORECASE)
UNSAFE_BLOCK_STMT_RE = re.compile(
    r"^\s*(contains\b|end\s+(program|subroutine|function|module)\b|return\b|stop\b|error\s+stop\b|cycle\b|exit\b|go\s*to\b|goto\b)",
    re.IGNORECASE,
)
KEYWORDS = {
    "if",
    "then",
    "else",
    "end",
    "do",
    "call",
    "print",
    "write",
    "read",
    "stop",
    "return",
    "contains",
    "module",
    "program",
    "subroutine",
    "function",
    "implicit",
    "none",
    "use",
    "only",
    "result",
    "allocate",
    "deallocate",
}


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


def parse_header_args(line: str) -> Set[str]:
    code = strip_comment(line).strip().lower()
    m = re.match(r"^\s*(?:subroutine|function|program)\s+[a-z][a-z0-9_]*\s*\(([^)]*)\)", code, re.IGNORECASE)
    if not m:
        return set()
    return {a.strip().lower() for a in split_top_level_commas(m.group(1)) if a.strip()}


def parse_decl_line(code: str) -> Optional[Tuple[str, List[str]]]:
    if "::" not in code:
        return None
    lhs, rhs = code.split("::", 1)
    ents = split_top_level_commas(rhs)
    if not ents:
        return None
    return lhs.strip(), ents


def decl_tail_reads(code: str) -> Set[str]:
    """Return identifiers read in declaration tails (dims/init), excluding declared entity names."""
    out: Set[str] = set()
    parsed = parse_decl_line(code)
    if not parsed:
        return out
    _spec, ents = parsed
    declared = {entity_name(e) for e in ents if entity_name(e)}
    for ent in ents:
        m = re.match(r"^\s*([a-z][a-z0-9_]*)(.*)$", ent.strip(), re.IGNORECASE)
        if not m:
            continue
        tail = m.group(2)
        for n in extract_identifiers(tail):
            if n not in declared:
                out.add(n)
    return out


def entity_name(ent: str) -> Optional[str]:
    m = re.match(r"^\s*([a-z][a-z0-9_]*)", ent, re.IGNORECASE)
    if not m:
        return None
    return m.group(1).lower()


def sanitize_entity_for_arg(ent: str) -> str:
    t = ent.strip()
    if "=" in t and "=>" not in t:
        t = t.split("=", 1)[0].strip()
    return t


def to_assumed_shape_entity(ent: str) -> str:
    """Convert explicit-shape dummy array entity to assumed-shape when feasible."""
    t = sanitize_entity_for_arg(ent)
    m = re.match(r"^([a-z][a-z0-9_]*)\s*\(([^)]*)\)\s*$", t, re.IGNORECASE)
    if not m:
        return t
    name = m.group(1)
    dims = [d.strip() for d in split_top_level_commas(m.group(2)) if d.strip()]
    if not dims:
        return t
    # Keep assumed-size arrays unchanged.
    if any("*" in d for d in dims):
        return t
    return f"{name}({', '.join(':' for _ in dims)})"


def reindent_subroutine_body(lines: List[str]) -> List[str]:
    """Normalize indentation: only control blocks are indented by 3 spaces."""
    out: List[str] = []
    indent = 0
    for raw in lines:
        eol = "\n"
        if raw.endswith("\r\n"):
            eol = "\r\n"
        elif raw.endswith("\n"):
            eol = "\n"
        code = raw.rstrip("\r\n")
        stripped = code.strip()
        if not stripped:
            out.append(eol)
            continue
        low = strip_comment(stripped).strip().lower()

        is_dedent = (
            low.startswith("end if")
            or low == "endif"
            or low.startswith("end do")
            or low.startswith("end select")
            or low.startswith("case ")
            or low.startswith("case(")
            or low.startswith("case default")
            or low.startswith("else")
            or low.startswith("elseif ")
            or low.startswith("else if")
        )
        if is_dedent:
            indent = max(0, indent - 1)

        out.append(f"{' ' * (3 * indent)}{stripped}{eol}")

        is_start = (
            (
                low.startswith("if")
                and low.endswith("then")
                and not low.startswith("else if")
                and not low.startswith("elseif")
            )
            or low.startswith("do ")
            or low == "do"
            or low.startswith("select case")
            or low.startswith("case ")
            or low.startswith("case(")
            or low.startswith("case default")
            or low.startswith("else")
            or low.startswith("elseif ")
            or low.startswith("else if")
        )
        if is_start:
            indent += 1
    return out


def extent_refs_from_entity(ent: str) -> List[Tuple[str, int]]:
    """Return (name, dim_index) symbols used as explicit extents in one entity."""
    out: List[Tuple[str, int]] = []
    t = sanitize_entity_for_arg(ent)
    m = re.match(r"^([a-z][a-z0-9_]*)\s*\(([^)]*)\)\s*$", t, re.IGNORECASE)
    if not m:
        return out
    dims = [d.strip() for d in split_top_level_commas(m.group(2)) if d.strip()]
    for i, d in enumerate(dims, start=1):
        if re.fullmatch(r"[a-z][a-z0-9_]*", d, re.IGNORECASE):
            out.append((d.lower(), i))
    return out


def apply_symbol_replacements(lines: List[str], repl: Dict[str, str]) -> List[str]:
    """Replace whole-word symbols in code portion of lines, preserving comments."""
    if not repl:
        return lines
    out: List[str] = []
    for raw in lines:
        eol = "\n"
        if raw.endswith("\r\n"):
            eol = "\r\n"
        elif raw.endswith("\n"):
            eol = "\n"
        body = raw.rstrip("\r\n")
        code = strip_comment(body)
        comment = body[len(code) :]
        new_code = code
        for old, new in repl.items():
            new_code = re.sub(rf"\b{re.escape(old)}\b", new, new_code)
        out.append(new_code + comment + eol)
    return out


def parse_assignment_lhs_rhs(code: str) -> Optional[Tuple[str, str, str]]:
    m = re.match(
        r"^\s*([a-z][a-z0-9_]*(?:\s*%\s*[a-z][a-z0-9_]*)?(?:\s*\([^=]*\))?)\s*=\s*(.+?)\s*$",
        code,
        re.IGNORECASE,
    )
    if not m:
        return None
    lhs = m.group(1).strip()
    rhs = m.group(2).strip()
    b = re.match(r"^\s*([a-z][a-z0-9_]*)", lhs, re.IGNORECASE)
    if not b:
        return None
    return lhs, b.group(1).lower(), rhs


def token_occurrences(code: str, name: str) -> int:
    return len(re.findall(rf"\b{re.escape(name)}\b", code, flags=re.IGNORECASE))


def has_markers(lines: List[str]) -> bool:
    for ln in lines:
        t = ln.rstrip("\r\n")
        if BEGIN_RE.match(t) or END_RE.match(t):
            return True
    return False


def find_loop_candidate(lines: List[str], min_body_lines: int) -> Optional[Tuple[int, int]]:
    """Find first DO...END DO whose nonblank body length is >= min_body_lines."""
    stack: List[int] = []
    for i, ln in enumerate(lines):
        code = strip_comment(ln).strip().lower()
        if not code:
            continue
        if DO_START_RE.match(code):
            stack.append(i)
            continue
        if END_DO_RE.match(code) and stack:
            s = stack.pop()
            body_count = 0
            for j in range(s + 1, i):
                cj = strip_comment(lines[j]).strip()
                if cj:
                    body_count += 1
            if body_count >= min_body_lines:
                return s, i
    return None


def auto_insert_loop_markers(lines: List[str], min_body_lines: int) -> Optional[List[str]]:
    hit = find_loop_candidate(lines, min_body_lines)
    if hit is None:
        return None
    s, e = hit
    name = f"loop_block_{s + 1}"
    out = list(lines)
    begin_line = f"!! begin subroutine {name}\n"
    end_line = f"!! end subroutine {name}\n"
    out.insert(s, begin_line)
    out.insert(e + 2, end_line)
    return out


def entity_rank(ent: str) -> int:
    t = sanitize_entity_for_arg(ent)
    m = re.match(r"^([a-z][a-z0-9_]*)\s*\(([^)]*)\)\s*$", t, re.IGNORECASE)
    if not m:
        return 0
    dims = [d.strip() for d in split_top_level_commas(m.group(2)) if d.strip()]
    return len(dims)


def norm_expr(text: str) -> str:
    return re.sub(r"\s+", "", text.strip().lower())


def norm_dims(text: str) -> List[str]:
    return [norm_expr(d) for d in split_top_level_commas(text)]


def maybe_specialize_section_arg(
    arg_name: str, arg_entity: str, block_lines: List[str]
) -> Optional[Tuple[str, str, List[str]]]:
    """
    If arg_name (rank>1) is used only as one consistent rank-1 section, return:
      (dummy_entity_override, call_expr_override, rewritten_block_lines)
    else None.
    """
    rank = entity_rank(arg_entity)
    if rank <= 1:
        return None

    use_paren_re = re.compile(rf"\b{re.escape(arg_name)}\s*\(([^)]*)\)", re.IGNORECASE)
    bare_re = re.compile(rf"\b{re.escape(arg_name)}\b(?!\s*\()", re.IGNORECASE)
    inquiry_prefix_re = re.compile(r"(size|lbound|ubound|shape)\s*\(\s*$", re.IGNORECASE)

    # Reject if bare name appears outside simple inquiry-intrinsic argument contexts.
    for ln in block_lines:
        code = strip_comment(ln)
        for mb in bare_re.finditer(code):
            s = mb.start()
            before = code[max(0, s - 32) : s]
            if inquiry_prefix_re.search(before):
                continue
            return None

    captured_dims: List[List[str]] = []
    captured_raw: Optional[List[str]] = None
    for ln in block_lines:
        code = strip_comment(ln)
        for m in use_paren_re.finditer(code):
            dims_raw = [d.strip() for d in split_top_level_commas(m.group(1)) if d.strip()]
            if len(dims_raw) != rank:
                return None
            dims_norm = [norm_expr(d) for d in dims_raw]
            if captured_raw is None:
                captured_raw = dims_raw
            captured_dims.append(dims_norm)

    if not captured_dims or captured_raw is None:
        return None

    first = captured_dims[0]
    for d in captured_dims[1:]:
        if d != first:
            return None

    n_colon = sum(1 for d in first if d == ":")
    if n_colon != 1:
        return None

    target_norm = first
    call_expr = f"{arg_name}({','.join(captured_raw)})"
    dummy_entity = f"{arg_name}(:)"

    rewritten: List[str] = []
    for raw in block_lines:
        eol = "\n"
        if raw.endswith("\r\n"):
            eol = "\r\n"
        elif raw.endswith("\n"):
            eol = "\n"
        body = raw.rstrip("\r\n")
        code = strip_comment(body)
        comment = body[len(code) :]

        def repl(m: re.Match[str]) -> str:
            dims = norm_dims(m.group(1))
            if dims == target_norm:
                return arg_name
            return m.group(0)

        new_code = use_paren_re.sub(repl, code)
        rewritten.append(new_code + comment + eol)

    return dummy_entity, call_expr, rewritten


def sanitize_spec_for_arg(spec: str) -> str:
    parts = [p.strip() for p in split_top_level_commas(spec)]
    kept: List[str] = []
    for p in parts:
        pl = p.lower()
        if pl.startswith("intent"):
            continue
        if pl == "parameter":
            continue
        if pl == "optional":
            continue
        kept.append(p)
    return ", ".join(kept)


def add_intent(spec: str, intent: str) -> str:
    if re.search(r"\bintent\s*\(", spec, re.IGNORECASE):
        return spec
    return f"{spec}, intent({intent})"


def extract_identifiers(code: str) -> List[str]:
    out: List[str] = []
    seen: Set[str] = set()
    text = strip_comment(code)
    for m in IDENT_RE.finditer(text.lower()):
        n = m.group(1)
        if n in KEYWORDS:
            continue
        if n not in seen:
            seen.add(n)
            out.append(n)
    return out


def parse_call_name_args(code: str) -> Optional[Tuple[str, List[str]]]:
    m = re.match(r"^\s*call\s+([a-z][a-z0-9_]*)\s*\((.*)\)\s*$", code, re.IGNORECASE)
    if not m:
        return None
    name = m.group(1).lower()
    args = [a.strip() for a in split_top_level_commas(m.group(2)) if a.strip()]
    return name, args


def read_write_sets(lines: List[str]) -> Tuple[Set[str], Set[str], List[str]]:
    reads: Set[str] = set()
    writes: Set[str] = set()
    order: List[str] = []
    seen_order: Set[str] = set()
    for ln in lines:
        code = strip_comment(ln).strip()
        if not code:
            continue
        call_info = parse_call_name_args(code)
        if call_info is not None:
            cname, cargs = call_info
            if cname == "random_number" and cargs:
                b = re.match(r"^\s*([a-z][a-z0-9_]*)", cargs[0], re.IGNORECASE)
                if b:
                    writes.add(b.group(1).lower())
            for a in cargs:
                for n in extract_identifiers(a):
                    reads.add(n)
                    if n not in seen_order:
                        seen_order.add(n)
                        order.append(n)
            continue
        m_do = DO_ITER_RE.match(code)
        if m_do:
            iv = m_do.group(1).lower()
            writes.add(iv)
            if iv not in seen_order:
                seen_order.add(iv)
                order.append(iv)
            rhs = code.split("=", 1)[1] if "=" in code else ""
            for n in extract_identifiers(rhs):
                if n == iv:
                    continue
                reads.add(n)
                if n not in seen_order:
                    seen_order.add(n)
                    order.append(n)
            continue
        m_as = ASSIGN_RE.match(code)
        if m_as:
            lhs = m_as.group(1)
            lhs_base = re.match(r"^\s*([a-z][a-z0-9_]*)", lhs, re.IGNORECASE)
            if lhs_base:
                writes.add(lhs_base.group(1).lower())
            rhs = code.split("=", 1)[1]
            for n in extract_identifiers(rhs):
                reads.add(n)
                if n not in seen_order:
                    seen_order.add(n)
                    order.append(n)
            # indexes in lhs are reads
            if "(" in lhs and ")" in lhs:
                for n in extract_identifiers(lhs):
                    if lhs_base and n == lhs_base.group(1).lower():
                        continue
                    reads.add(n)
                    if n not in seen_order:
                        seen_order.add(n)
                        order.append(n)
            continue
        for n in extract_identifiers(code):
            reads.add(n)
            if n not in seen_order:
                seen_order.add(n)
                order.append(n)
    for n in writes:
        if n not in seen_order:
            order.append(n)
    return reads, writes, order


def first_access_kind(lines: List[str]) -> Dict[str, str]:
    """Return first access kind per symbol in block: 'r' or 'w'."""
    first: Dict[str, str] = {}
    for ln in lines:
        code = strip_comment(ln).strip()
        if not code:
            continue
        call_info = parse_call_name_args(code)
        if call_info is not None:
            cname, cargs = call_info
            if cname == "random_number" and cargs:
                b = re.match(r"^\s*([a-z][a-z0-9_]*)", cargs[0], re.IGNORECASE)
                if b:
                    first.setdefault(b.group(1).lower(), "w")
            for a in cargs:
                for n in extract_identifiers(a):
                    first.setdefault(n, "r")
            continue
        m_do = DO_ITER_RE.match(code)
        if m_do:
            iv = m_do.group(1).lower()
            rhs = code.split("=", 1)[1] if "=" in code else ""
            for n in extract_identifiers(rhs):
                if n == iv:
                    continue
                first.setdefault(n, "r")
            first.setdefault(iv, "w")
            continue
        m_as = ASSIGN_RE.match(code)
        if m_as:
            lhs = m_as.group(1)
            lhs_base_m = re.match(r"^\s*([a-z][a-z0-9_]*)", lhs, re.IGNORECASE)
            lhs_base = lhs_base_m.group(1).lower() if lhs_base_m else ""
            # reads from RHS and lhs indices
            rhs = code.split("=", 1)[1]
            for n in extract_identifiers(rhs):
                first.setdefault(n, "r")
            if "(" in lhs and ")" in lhs:
                for n in extract_identifiers(lhs):
                    if lhs_base and n == lhs_base:
                        continue
                    first.setdefault(n, "r")
            if lhs_base:
                first.setdefault(lhs_base, "w")
            continue
        for n in extract_identifiers(code):
            first.setdefault(n, "r")
    return first


def parse_use_line(code: str) -> Optional[Tuple[str, Optional[List[str]], str]]:
    m = USE_RE.match(code.strip())
    if not m:
        return None
    mod = m.group(1).lower()
    only_raw = m.group(2)
    if only_raw is None:
        return mod, None, ""
    names = [x.strip() for x in split_top_level_commas(only_raw) if x.strip()]
    return mod, names, m.group(1)


def parse_marker_fields(m: re.Match[str]) -> Tuple[Optional[str], str, str]:
    legacy_mod = m.group("legacy_mod")
    legacy_name = m.group("legacy_name")
    if legacy_name is not None:
        return legacy_mod.lower(), "subroutine", legacy_name.lower()
    mod = m.group("mod")
    kind = m.group("kind")
    name = m.group("name")
    if kind is None or name is None:
        raise ValueError("Marker parse failure")
    return (mod.lower() if mod else None), kind.lower(), name.lower()


def collect_existing_modules(lines: List[str]) -> Set[str]:
    mods: Set[str] = set()
    for ln in lines:
        code = strip_comment(ln).strip()
        mm = re.match(r"^\s*module\s+([a-z][a-z0-9_]*)\b", code, re.IGNORECASE)
        if not mm:
            continue
        if re.match(r"^\s*module\s+procedure\b", code, re.IGNORECASE):
            continue
        mods.add(mm.group(1).lower())
    return mods


def collect_declared_variable_names(lines: List[str]) -> Set[str]:
    out: Set[str] = set()
    for ln in lines:
        code = strip_comment(ln).strip()
        if not code or not DECL_RE.match(code):
            continue
        parsed = parse_decl_line(code)
        if not parsed:
            continue
        _spec, ents = parsed
        for ent in ents:
            n = entity_name(ent)
            if n:
                out.add(n)
    # Also include dummy names in procedure/program headers.
    for ln in lines:
        out.update(parse_header_args(ln))
    return out


def choose_default_module_name(existing_modules: Set[str], existing_vars: Set[str]) -> str:
    i = 0
    while True:
        cand = "m" if i == 0 else f"m{i}"
        if cand not in existing_modules and cand not in existing_vars:
            return cand
        i += 1


def extract_marked_block(lines: List[str]) -> Tuple[int, int, Optional[str], str, str]:
    begin_hits: List[Tuple[int, Optional[str], str, str]] = []
    end_hits: List[Tuple[int, Optional[str], str, str]] = []
    for i, ln in enumerate(lines):
        t = ln.rstrip("\r\n")
        mb = BEGIN_RE.match(t)
        if mb:
            mod, kind, name = parse_marker_fields(mb)
            begin_hits.append((i, mod, kind, name))
        me = END_RE.match(t)
        if me:
            mod, kind, name = parse_marker_fields(me)
            end_hits.append((i, mod, kind, name))

    if not begin_hits:
        raise ValueError("No begin marker found (expected: !! begin module::subroutine)")
    if not end_hits:
        raise ValueError("No end marker found (expected: !! end module::subroutine)")
    if len(begin_hits) != 1 or len(end_hits) != 1:
        raise ValueError("Exactly one begin marker and one end marker are required per file")

    bidx, mod_b, kind_b, name_b = begin_hits[0]
    eidx, mod_e, kind_e, name_e = end_hits[0]
    if eidx <= bidx:
        raise ValueError("End marker must appear after begin marker")
    if kind_b != kind_e:
        raise ValueError("Begin/end marker procedure kinds must match (subroutine/function)")
    if name_b != name_e:
        raise ValueError("Begin/end marker procedure names must match")
    if (mod_b is None) != (mod_e is None):
        raise ValueError("Begin/end markers must either both specify module prefix or both omit it")
    if mod_b is not None and mod_b != mod_e:
        raise ValueError("Begin/end marker module names must match")
    return bidx, eidx, mod_b, kind_b, name_b


def find_host_unit(lines: List[str], bidx: int) -> Tuple[int, int]:
    s = -1
    for i in range(bidx, -1, -1):
        if UNIT_START_RE.match(strip_comment(lines[i]).strip()):
            s = i
            break
    if s < 0:
        raise ValueError("Could not locate containing program/subroutine/function start")
    t = -1
    for i in range(bidx, len(lines)):
        if UNIT_END_RE.match(strip_comment(lines[i]).strip()):
            t = i
            break
    if t < 0:
        raise ValueError("Could not locate containing program/subroutine/function end")
    return s, t


def control_delta(code_low: str) -> int:
    """Approximate control nesting delta for one line."""
    if END_IF_RE.match(code_low) or END_DO_RE.match(code_low) or END_SELECT_RE.match(code_low):
        return -1
    if ELSE_RE.match(code_low) and not ELSE_IF_RE.match(code_low):
        return 0
    if CASE_RE.match(code_low):
        return 0
    if IF_THEN_RE.match(code_low) or DO_START_RE.match(code_low) or SELECT_START_RE.match(code_low):
        return 1
    return 0


def control_depth_at(lines: List[str], start: int, at: int) -> int:
    """Compute control depth within one host unit at line index 'at' (exclusive)."""
    d = 0
    for i in range(start, at):
        low = strip_comment(lines[i]).strip().lower()
        if not low:
            continue
        delta = control_delta(low)
        d += delta
        if d < 0:
            d = 0
    return d


def first_executable_index(lines: List[str], s: int, t: int) -> Optional[int]:
    """Return first executable statement index inside host unit body."""
    for i in range(s + 1, t):
        low = strip_comment(lines[i]).strip().lower()
        if not low:
            continue
        if low.startswith("use ") or low.startswith("implicit ") or DECL_RE.match(low):
            continue
        return i
    return None


def validate_marked_region(lines: List[str], bidx: int, eidx: int, s: int, t: int) -> None:
    """Validate marker placement and block safety constraints."""
    if bidx <= s or eidx >= t:
        raise ValueError("Markers must be inside a single host unit body")
    if eidx > t:
        raise ValueError("End marker is outside the host unit containing begin marker")

    first_exec = first_executable_index(lines, s, t)
    if first_exec is not None and bidx < first_exec:
        # Allow marker immediately before executable region (or separated by blank/comment lines).
        # Reject only if declaration/use/implicit lines still appear after marker.
        for i in range(bidx + 1, first_exec):
            low = strip_comment(lines[i]).strip().lower()
            if not low:
                continue
            if low.startswith("use ") or low.startswith("implicit ") or DECL_RE.match(low):
                raise ValueError("Begin marker must be in executable section (not in declaration section)")

    d_begin = control_depth_at(lines, s + 1, bidx)
    d_end = control_depth_at(lines, s + 1, eidx)
    if d_begin != d_end:
        raise ValueError("Begin/end markers must be at the same control-block nesting depth")

    if eidx == bidx + 1:
        raise ValueError("Marked block is empty")

    def code_without_comment(i: int) -> str:
        return strip_comment(lines[i]).rstrip("\r\n")

    def starts_with_continuation(i: int) -> bool:
        return code_without_comment(i).lstrip().startswith("&")

    def ends_with_continuation(i: int) -> bool:
        return code_without_comment(i).rstrip().endswith("&")

    # Conservative boundary checks so extraction does not split continued statements.
    if bidx - 1 >= s + 1 and ends_with_continuation(bidx - 1):
        raise ValueError("Begin marker cannot split a continued statement")
    if bidx + 1 < eidx and starts_with_continuation(bidx + 1):
        raise ValueError("Begin marker cannot precede a continuation line")
    if eidx - 1 > bidx and ends_with_continuation(eidx - 1):
        raise ValueError("End marker cannot follow a continued statement line")
    if eidx + 1 < t and starts_with_continuation(eidx + 1):
        raise ValueError("End marker cannot split a continued statement")

    # Enforce no nested markers and reject unsafe statements in block.
    for i in range(bidx + 1, eidx):
        txt = lines[i].rstrip("\r\n")
        if BEGIN_RE.match(txt) or END_RE.match(txt):
            raise ValueError("Nested begin/end markers are not allowed")
        low = strip_comment(lines[i]).strip().lower()
        if not low:
            continue
        if UNSAFE_BLOCK_STMT_RE.match(low):
            raise ValueError(f"Unsafe statement inside marked block at line {i + 1}: {low}")


def rewrite_decl_remove_names(raw_line: str, remove: Set[str]) -> Tuple[Optional[str], bool]:
    code = strip_comment(raw_line).rstrip("\r\n")
    comment = raw_line.rstrip("\r\n")[len(code) :]
    parsed = parse_decl_line(code)
    if not parsed:
        return raw_line, False
    spec, ents = parsed
    kept: List[str] = []
    changed = False
    for ent in ents:
        n = entity_name(ent)
        if n and n in remove:
            changed = True
            continue
        kept.append(ent.strip())
    if not changed:
        return raw_line, False
    if not kept:
        return None, True
    eol = "\n"
    if raw_line.endswith("\r\n"):
        eol = "\r\n"
    elif raw_line.endswith("\n"):
        eol = "\n"
    return f"{spec} :: {', '.join(kept)}{comment}{eol}", True


def main() -> int:
    parser = argparse.ArgumentParser(description="Extract !! begin m::sub .. !! end m::sub block to module subroutine")
    parser.add_argument("fortran_file", type=Path)
    parser.add_argument("--out", type=Path, default=None, help="Output file path (default: temp.f90)")
    parser.add_argument("--out-dir", type=Path, help="Write output file to this directory using input basename")
    parser.add_argument(
        "--check-compile",
        action="store_true",
        help="Compile original first, then transformed output (stop if original fails)",
    )
    parser.add_argument(
        "--compiler-cmd",
        type=str,
        default=None,
        help="Compiler command template (implies --check-compile). Supports {file}, {files}, and {exe}.",
    )
    parser.add_argument("--run", action="store_true", help="Build and run transformed source")
    parser.add_argument("--run-both", action="store_true", help="Build and run both baseline and transformed source")
    parser.add_argument("--inline", action="store_true", help="Inline extracted function call at single use site when safe")
    parser.add_argument(
        "--loop-lines",
        type=int,
        default=None,
        help="If no markers are present, auto-extract first DO...END DO block with at least N nonblank body lines",
    )
    parser.add_argument("--verbose", action="store_true")
    args = parser.parse_args()
    if args.out is not None and args.out_dir is not None:
        print("--out and --out-dir are mutually exclusive.")
        return 2
    if args.run_both:
        args.run = True
    if args.compiler_cmd is not None:
        args.check_compile = True

    src = args.fortran_file
    if not src.exists():
        print(f"Source not found: {src}")
        return 2
    if args.out_dir is not None:
        if args.out_dir.exists() and not args.out_dir.is_dir():
            print("--out-dir exists but is not a directory.")
            return 2
        args.out_dir.mkdir(parents=True, exist_ok=True)
    out_path = args.out
    if out_path is None:
        out_path = (args.out_dir / src.name) if args.out_dir is not None else Path("temp.f90")

    raw = src.read_text(encoding="utf-8-sig", errors="ignore").splitlines(keepends=True)
    if args.loop_lines is not None:
        if args.loop_lines < 1:
            print("--loop-lines must be >= 1")
            return 2
        if not has_markers(raw):
            auto = auto_insert_loop_markers(raw, args.loop_lines)
            if auto is None:
                print(f"No DO...END DO block with at least {args.loop_lines} nonblank body lines found.")
                return 1
            raw = auto

    default_compile = "gfortran -c {file}"
    default_build = "gfortran {file} -o {exe}"
    if args.run:
        compile_template = args.compiler_cmd or default_build
        if "{exe}" not in compile_template:
            compile_template = f"{compile_template} -o {{exe}}"
    else:
        compile_template = args.compiler_cmd or default_compile

    def run_compile(label: str, file_path: Path) -> bool:
        cmd = compile_template.format(file=str(file_path), files=str(file_path), exe="")
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

    def build_and_run(label: str, file_path: Path, exe_suffix: str) -> bool:
        exe_path = Path(f"{file_path.stem}_{exe_suffix}.exe")
        cmd = compile_template.format(file=str(file_path), files=str(file_path), exe=str(exe_path))
        print(f"Build exe ({label}): {cmd}")
        cp = subprocess.run(cmd, shell=True, capture_output=True, text=True)
        if cp.returncode != 0:
            print(f"Build exe ({label}): FAIL (exit {cp.returncode})")
            if cp.stdout:
                print(cp.stdout.rstrip())
            if cp.stderr:
                print(cp.stderr.rstrip())
            return False
        print(f"Build exe ({label}): PASS")
        print(f"Run ({label}): {exe_path}")
        rp = subprocess.run(str(exe_path), shell=True, capture_output=True, text=True)
        if rp.returncode != 0:
            print(f"Run ({label}): FAIL (exit {rp.returncode})")
            if rp.stdout:
                print(rp.stdout.rstrip())
            if rp.stderr:
                print(rp.stderr.rstrip())
            return False
        print(f"Run ({label}): PASS")
        if rp.stdout:
            print(rp.stdout.rstrip())
        if rp.stderr:
            print(rp.stderr.rstrip())
        return True

    if args.run_both:
        if not build_and_run("baseline", src, "orig"):
            print("Baseline build/run failed. Stopping without transformation.")
            return 5
    elif args.check_compile:
        if not run_compile("baseline", src):
            print("Baseline compile failed. Stopping without transformation.")
            return 5

    try:
        bidx, eidx, module_hint, proc_kind, sub_name = extract_marked_block(raw)
    except ValueError as ex:
        print(str(ex))
        return 2

    existing_modules = collect_existing_modules(raw)
    existing_vars = collect_declared_variable_names(raw)
    if module_hint is not None:
        module_name = module_hint
        if module_name == "m" and (module_name in existing_modules or module_name in existing_vars):
            print("Explicit module 'm' requested, but module/variable name 'm' already exists.")
            return 3
        if module_name in existing_modules:
            print(f"Module {module_name} already exists; cannot auto-create duplicate module.")
            return 3
        if module_name in existing_vars:
            print(f"Module name {module_name} conflicts with an existing variable name.")
            return 3
    else:
        module_name = choose_default_module_name(existing_modules, existing_vars)

    try:
        host_start, host_end = find_host_unit(raw, bidx)
    except ValueError as ex:
        print(str(ex))
        return 2
    if eidx > host_end:
        print("End marker is outside the host unit containing begin marker.")
        return 2
    try:
        validate_marked_region(raw, bidx, eidx, host_start, host_end)
    except ValueError as ex:
        print(str(ex))
        return 2
    host_lines = raw[host_start : host_end + 1]
    lb = bidx - host_start
    le = eidx - host_start

    block_lines = host_lines[lb + 1 : le]
    fn_return_lhs_expr = ""
    fn_return_base = ""
    fn_return_decl_spec = ""
    if proc_kind == "function":
        last_exec = -1
        for i in range(len(block_lines) - 1, -1, -1):
            c = strip_comment(block_lines[i]).strip()
            if c:
                last_exec = i
                break
        if last_exec < 0:
            print("Function extraction block is empty.")
            return 2
        parsed_last = parse_assignment_lhs_rhs(strip_comment(block_lines[last_exec]).strip())
        if parsed_last is None:
            print("For function extraction, the last non-empty line before marker end must be an assignment.")
            return 2
        fn_return_lhs_expr, fn_return_base, _fn_return_rhs = parsed_last
    reads, writes, use_order = read_write_sets(block_lines)
    first_access = first_access_kind(block_lines)

    # gather declarations in host
    decl_by_name: Dict[str, Tuple[int, str, str]] = {}
    for i, ln in enumerate(host_lines):
        code = strip_comment(ln).strip()
        if not DECL_RE.match(code.lower()):
            continue
        parsed = parse_decl_line(code)
        if not parsed:
            continue
        spec, ents = parsed
        for ent in ents:
            n = entity_name(ent)
            if n and n not in decl_by_name:
                decl_by_name[n] = (i, spec, ent.strip())

    if proc_kind == "function":
        if fn_return_base not in decl_by_name:
            print(f"Function return variable '{fn_return_base}' is not declared in host scope.")
            return 2
        _ri, fn_return_decl_spec, fn_return_decl_ent = decl_by_name[fn_return_base]
        if "(" in sanitize_entity_for_arg(fn_return_decl_ent):
            print(
                f"Function return variable '{fn_return_base}' appears to be non-scalar; only scalar returns are supported."
            )
            return 2

        # If variables assigned before the return-assignment line are used after marker end,
        # function extraction would silently hide those side effects.
        pre_lhs: Set[str] = set()
        for i, bl in enumerate(block_lines):
            if i == last_exec:
                continue
            c = strip_comment(bl).strip()
            if not c:
                continue
            p = parse_assignment_lhs_rhs(c)
            if p is None:
                continue
            _lhs_expr, lhs_base, _rhs = p
            if lhs_base == fn_return_base:
                continue
            pre_lhs.add(lhs_base)
        used_after: Set[str] = set()
        for ol in host_lines[le + 1 :]:
            c = strip_comment(ol).strip()
            if not c:
                continue
            if DECL_RE.match(c.lower()):
                used_after.update(decl_tail_reads(c))
            else:
                used_after.update(extract_identifiers(c))
        offenders = sorted(pre_lhs & used_after)
        if offenders:
            print(
                "Function extraction would hide assignments needed later; variables used after marker end: "
                + ", ".join(offenders)
            )
            return 2

    dummy_names = parse_header_args(host_lines[0])

    # names referenced outside block in host unit
    outside_lines = host_lines[: lb + 1] + host_lines[le:]
    outside_usage: Set[str] = set()
    for ol in outside_lines:
        c = strip_comment(ol).strip()
        if not c:
            continue
        if DECL_RE.match(c.lower()):
            outside_usage.update(decl_tail_reads(c))
        else:
            outside_usage.update(extract_identifiers(c))

    used_declared = [n for n in use_order if n in decl_by_name]
    args_names: List[str] = []
    locals_names: List[str] = []
    for n in used_declared:
        if n in args_names or n in locals_names:
            continue
        outside_used = n in outside_usage
        first_is_read = first_access.get(n) == "r"
        if n in dummy_names or outside_used or first_is_read:
            args_names.append(n)
        else:
            locals_names.append(n)

    if proc_kind == "function" and fn_return_base:
        args_names = [a for a in args_names if a != fn_return_base]
        locals_names = [l for l in locals_names if l != fn_return_base]

    # Simplify scalar extent arguments by deriving them from assumed-shape arrays.
    # Example: x(n) + arg n => use size(x) inside subroutine and drop n from args.
    arg_set = set(args_names)
    repl_map: Dict[str, str] = {}
    for n in list(args_names):
        info_n = decl_by_name.get(n)
        if info_n is None:
            continue
        _idx_n, spec_n, ent_n = info_n
        ent_n_clean = sanitize_entity_for_arg(ent_n)
        if "(" in ent_n_clean and ")" in ent_n_clean:
            continue  # only scalar candidates
        if "integer" not in spec_n.lower():
            continue
        if n in writes:
            continue

        replacement = ""
        for a in args_names:
            if a == n:
                continue
            info_a = decl_by_name.get(a)
            if info_a is None:
                continue
            _idx_a, _spec_a, ent_a = info_a
            rank_a = entity_rank(ent_a)
            for sym, dim_idx in extent_refs_from_entity(ent_a):
                if sym != n:
                    continue
                replacement = f"size({a})" if (dim_idx == 1 and rank_a == 1) else f"size({a}, {dim_idx})"
                break
            if replacement:
                break
        if replacement:
            repl_map[n] = replacement

    if repl_map:
        args_names = [a for a in args_names if a not in repl_map]
        arg_set = set(args_names)
        block_lines = apply_symbol_replacements(block_lines, repl_map)
        reads, writes, use_order = read_write_sets(block_lines)
        first_access = first_access_kind(block_lines)

    # Specialize higher-rank arguments used only as a consistent rank-1 section
    # (e.g., x(:,icol)) into a rank-1 dummy x(:), and pass that section at call site.
    arg_entity_override: Dict[str, str] = {}
    call_arg_override: Dict[str, str] = {}
    for n in args_names:
        info_n = decl_by_name.get(n)
        if info_n is None:
            continue
        _idx_n, _spec_n, ent_n = info_n
        sp = maybe_specialize_section_arg(n, ent_n, block_lines)
        if sp is None:
            continue
        dummy_ent, call_expr, rewritten = sp
        arg_entity_override[n] = dummy_ent
        call_arg_override[n] = call_expr
        block_lines = rewritten
        reads, writes, use_order = read_write_sets(block_lines)
        first_access = first_access_kind(block_lines)

    # Drop arguments that became unused after rewrites/specialization.
    used_now = set(use_order)
    args_names = [a for a in args_names if a in used_now]

    # build procedure declarations
    arg_decl_lines: List[str] = []
    for n in args_names:
        if n not in decl_by_name:
            continue
        _idx, spec, ent = decl_by_name[n]
        spec2 = sanitize_spec_for_arg(spec)
        intent = infer_intent_from_access(
            is_read=(n in reads),
            is_written=(n in writes),
            first_access=first_access.get(n),
        )
        spec2 = add_intent(spec2, intent)
        ent_out = arg_entity_override.get(n, to_assumed_shape_entity(ent))
        arg_decl_lines.append(f"{spec2} :: {ent_out}")

    local_decl_lines: List[str] = []
    moved_locals: Set[str] = set()
    for n in locals_names:
        if n not in decl_by_name:
            continue
        _idx, spec, ent = decl_by_name[n]
        local_decl_lines.append(f"{spec} :: {ent}")
        moved_locals.add(n)

    # infer required USE statements for new module from host use lines
    needed_names: Set[str] = set(extract_identifiers("\n".join(block_lines + arg_decl_lines + local_decl_lines)))
    sub_use_lines: List[str] = []
    for ln in host_lines:
        code = strip_comment(ln).strip()
        pu = parse_use_line(code)
        if pu is None:
            continue
        mod, only_list, _mod_spelling = pu
        if mod.lower() == module_name.lower():
            continue
        if only_list is None:
            # Keep generic USE conservatively.
            sub_use_lines.append(code)
            continue
        keep = [n for n in only_list if entity_name(n) or True]
        keep2 = [n for n in keep if re.search(rf"\b{re.escape(n.split('=>')[-1].strip().lower())}\b", " ".join(sorted(needed_names)))]
        if keep2:
            sub_use_lines.append(f"use {mod}, only: {', '.join(keep2)}")

    # de-duplicate use lines in order
    sub_use_unique: List[str] = []
    seen_use: Set[str] = set()
    for u in sub_use_lines:
        ul = u.lower()
        if ul not in seen_use:
            seen_use.add(ul)
            sub_use_unique.append(u)

    indent = re.match(r"^\s*", host_lines[lb]).group(0)
    proc_args = ", ".join(args_names)
    call_args_list = [call_arg_override.get(a, a) for a in args_names]
    call_args = ", ".join(call_args_list)
    if proc_kind == "function":
        call_line = (
            f"{indent}{fn_return_lhs_expr} = {sub_name}({call_args})\n"
            if call_args
            else f"{indent}{fn_return_lhs_expr} = {sub_name}()\n"
        )
    else:
        call_line = f"{indent}call {sub_name}({call_args})\n" if call_args else f"{indent}call {sub_name}()\n"

    # remove moved locals from host declarations
    for n in sorted(moved_locals):
        info = decl_by_name.get(n)
        if info is None:
            continue
        li = info[0]
        new_line, _changed = rewrite_decl_remove_names(host_lines[li], {n})
        if new_line is None:
            host_lines[li] = ""
        else:
            host_lines[li] = new_line

    # Replace marked block with call line, or inline function call when safe and requested.
    replacement_lines: List[str] = [call_line]
    inlined = False
    if args.inline and proc_kind == "function":
        tail = host_lines[le + 1 :]
        occ_total = 0
        use_line_idx = -1
        for i, ln in enumerate(tail):
            code = strip_comment(ln).strip()
            if not code:
                continue
            if DECL_RE.match(code.lower()):
                continue
            c = token_occurrences(code, fn_return_base)
            if c:
                occ_total += c
                if use_line_idx < 0 and c == 1:
                    use_line_idx = i
                else:
                    use_line_idx = -1
        if occ_total == 1 and use_line_idx >= 0:
            code_full = tail[use_line_idx]
            body = code_full.rstrip("\r\n")
            code = strip_comment(body)
            comment = body[len(code) :]
            eol = "\n"
            if code_full.endswith("\r\n"):
                eol = "\r\n"
            repl_expr = f"{sub_name}({call_args})" if call_args else f"{sub_name}()"
            new_code = re.sub(rf"\b{re.escape(fn_return_base)}\b", repl_expr, code, count=1, flags=re.IGNORECASE)
            tail[use_line_idx] = new_code + comment + eol
            host_lines = host_lines[:lb] + tail
            replacement_lines = []
            inlined = True
    if not inlined:
        host_lines = host_lines[:lb] + replacement_lines + host_lines[le + 1 :]

    # If function result variable is no longer used after inlining, remove its declaration.
    if inlined and proc_kind == "function":
        still_used = False
        for ln in host_lines:
            c = strip_comment(ln).strip()
            if not c:
                continue
            if DECL_RE.match(c.lower()):
                if fn_return_base in decl_tail_reads(c):
                    still_used = True
                    break
                continue
            if re.search(rf"\b{re.escape(fn_return_base)}\b", c, re.IGNORECASE):
                still_used = True
                break
        if not still_used and fn_return_base in decl_by_name:
            li = decl_by_name[fn_return_base][0]
            new_line, _changed = rewrite_decl_remove_names(host_lines[li], {fn_return_base})
            if new_line is None:
                host_lines[li] = ""
            else:
                host_lines[li] = new_line

    # ensure host uses new module
    has_use_m = any(re.match(rf"^\s*use\s+{re.escape(module_name)}\b", strip_comment(ln).strip(), re.IGNORECASE) for ln in host_lines)
    if not has_use_m:
        insert_at = 1
        for i in range(1, len(host_lines)):
            c = strip_comment(host_lines[i]).strip().lower()
            if not c:
                continue
            if c.startswith("use "):
                insert_at = i + 1
                continue
            break
        host_indent = re.match(r"^\s*", host_lines[0]).group(0)
        host_lines.insert(insert_at, f"{host_indent}use {module_name}, only: {sub_name}\n")

    # compose extracted-procedure module
    sub_lines: List[str] = []
    sub_lines.append(f"module {module_name}\n")
    sub_lines.append("implicit none\n")
    sub_lines.append("contains\n")
    pure_prefix = "pure " if is_pure_eligible_block(block_lines, strip_comment=strip_comment) else ""
    if proc_kind == "function":
        sub_lines.append(
            f"{pure_prefix}function {sub_name}({proc_args}) result({fn_return_base})\n"
            if proc_args
            else f"{pure_prefix}function {sub_name}() result({fn_return_base})\n"
        )
    else:
        sub_lines.append(
            f"{pure_prefix}subroutine {sub_name}({proc_args})\n" if proc_args else f"{pure_prefix}subroutine {sub_name}()\n"
        )
    for u in sub_use_unique:
        sub_lines.append(f"{u}\n")
    for d in arg_decl_lines:
        sub_lines.append(f"{d}\n")
    if proc_kind == "function":
        sub_lines.append(f"{sanitize_spec_for_arg(fn_return_decl_spec)} :: {fn_return_base}\n")
    for d in local_decl_lines:
        sub_lines.append(f"{d}\n")
    for ln in reindent_subroutine_body(block_lines):
        sub_lines.append(ln)
    if proc_kind == "function":
        sub_lines.append(f"end function {sub_name}\n")
    else:
        sub_lines.append(f"end subroutine {sub_name}\n")
    sub_lines.append(f"end module {module_name}\n")
    sub_lines.append("\n")

    # rebuild full file with module inserted before host unit
    out_lines = raw[:host_start] + sub_lines + host_lines + raw[host_end + 1 :]

    out_path.write_text("".join(out_lines), encoding="utf-8", newline="")

    if args.verbose:
        print(f"Extracted block {module_name}::{sub_name}")
        print(f"Arguments: {', '.join(args_names) if args_names else '(none)'}")
        print(f"Locals moved: {', '.join(sorted(moved_locals)) if moved_locals else '(none)'}")
        print(f"Wrote {out_path}")
    else:
        print(f"Wrote {out_path}")
    if args.run:
        if not build_and_run("after-fix", out_path, "new"):
            return 5
    elif args.check_compile:
        if not run_compile("after-fix", out_path):
            return 5
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
