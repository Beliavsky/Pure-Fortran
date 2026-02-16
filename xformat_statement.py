#!/usr/bin/env python3
"""Suggest/fix replacing labeled FORMAT statements with format strings/constants."""

from __future__ import annotations

import argparse
import difflib
import re
import shutil
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Tuple

import cli_paths as cpaths
import fortran_build as fbuild
import fortran_scan as fscan

FORMAT_STMT_RE = re.compile(r"^\s*(\d+)\s*format\s*(\(.+\))\s*$", re.IGNORECASE)
PRINT_RE = re.compile(r"^\s*print\b\s*(.*)$", re.IGNORECASE)
IMPLICIT_NONE_RE = re.compile(r"^\s*implicit\s+none\b", re.IGNORECASE)
IMPLICIT_RE = re.compile(r"^\s*implicit\b", re.IGNORECASE)
ASSIGN_LABEL_RE = re.compile(r"^\s*assign\s+(\d+)\s+to\b", re.IGNORECASE)
NAME_TOKEN_RE = re.compile(r"\b([a-z][a-z0-9_]*)\b", re.IGNORECASE)
HEADER_STMT_RE = re.compile(
    r"^\s*(program|module|subroutine|function|block\s+data)\b",
    re.IGNORECASE,
)
END_HEADER_RE = re.compile(
    r"^\s*end\s*(program|module|subroutine|function|block\s*data)?\b",
    re.IGNORECASE,
)
DECL_PREFIX_RE = re.compile(
    r"^\s*(use|implicit|parameter|integer|real|double\s+precision|complex|logical|character|type|class|"
    r"dimension|common|equivalence|external|intrinsic|save|data|namelist|public|private|procedure)\b",
    re.IGNORECASE,
)
PROC_START_RE = re.compile(
    r"^\s*(?:recursive\s+|pure\s+|impure\s+|elemental\s+|module\s+)*"
    r"(program|module|subroutine|function|block\s*data)\b",
    re.IGNORECASE,
)
TYPED_FUNC_RE = re.compile(
    r"^\s*(?:type\s*\([^)]+\)|double\s+precision|real|integer|complex|logical|character)"
    r"(?:\s*\([^)]+\))?\s+function\b",
    re.IGNORECASE,
)
END_PROC_RE = re.compile(
    r"^\s*end\s*$|^\s*end\s+(?:program|module|subroutine|function|block\s*data)\b",
    re.IGNORECASE,
)


@dataclass
class FormatDef:
    label: str
    fmt: str
    line_start: int
    line_end: int
    scope: str


@dataclass
class UseRewrite:
    line_start: int
    line_end: int
    old_stmt: str
    new_stmt: str
    label: str
    scope: str


@dataclass
class ScopeInfo:
    key: str
    start: int
    end: int


@dataclass
class Finding:
    path: Path
    line: int
    label: str
    replacement: str
    stmt: str


def make_backup_path(path: Path) -> Path:
    base = Path(str(path) + ".bak")
    if not base.exists():
        return base
    idx = 1
    while True:
        cand = Path(f"{path}.bak{idx}")
        if not cand.exists():
            return cand
        idx += 1


def choose_files(args_files: List[Path], exclude: Iterable[str]) -> List[Path]:
    if args_files:
        files = cpaths.expand_path_args(args_files)
    else:
        files = sorted(
            set(Path(".").glob("*.f90")) | set(Path(".").glob("*.F90")),
            key=lambda p: p.name.lower(),
        )
    return fscan.apply_excludes(files, exclude)


def split_top_level_commas(text: str) -> List[str]:
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


def split_top_level_equals(text: str) -> Optional[Tuple[str, str]]:
    depth = 0
    in_single = False
    in_double = False
    i = 0
    while i < len(text):
        ch = text[i]
        if ch == "'" and not in_double:
            if in_single and i + 1 < len(text) and text[i + 1] == "'":
                i += 2
                continue
            in_single = not in_single
            i += 1
            continue
        if ch == '"' and not in_single:
            if in_double and i + 1 < len(text) and text[i + 1] == '"':
                i += 2
                continue
            in_double = not in_double
            i += 1
            continue
        if not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")" and depth > 0:
                depth -= 1
            elif ch == "=" and depth == 0:
                return text[:i].strip(), text[i + 1 :].strip()
        i += 1
    return None


def find_matching_paren(text: str, start_idx: int) -> int:
    """Return index of matching ')' for '(' at start_idx, else -1."""
    depth = 0
    in_single = False
    in_double = False
    i = start_idx
    while i < len(text):
        ch = text[i]
        if ch == "'" and not in_double:
            if in_single and i + 1 < len(text) and text[i + 1] == "'":
                i += 2
                continue
            in_single = not in_single
            i += 1
            continue
        if ch == '"' and not in_single:
            if in_double and i + 1 < len(text) and text[i + 1] == '"':
                i += 2
                continue
            in_double = not in_double
            i += 1
            continue
        if not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
                if depth == 0:
                    return i
        i += 1
    return -1


def split_stmt_label_prefix(stmt: str) -> Tuple[str, str]:
    """Split optional numeric statement label prefix from a statement."""
    m = re.match(r"^(\s*\d+\s+)(.*)$", stmt)
    if not m:
        return "", stmt
    return m.group(1), m.group(2)


def parse_write_read_stmt(stmt: str) -> Optional[Tuple[str, str, str]]:
    """Parse WRITE/READ statement into (kw, control, tail)."""
    _lbl, body = split_stmt_label_prefix(stmt)
    s = body.strip()
    low = s.lower()
    if low.startswith("write"):
        kw = "write"
        klen = 5
    elif low.startswith("read"):
        kw = "read"
        klen = 4
    else:
        return None
    i = klen
    while i < len(s) and s[i].isspace():
        i += 1
    if i >= len(s) or s[i] != "(":
        return None
    j = find_matching_paren(s, i)
    if j < 0:
        return None
    control = s[i + 1 : j]
    tail = s[j + 1 :].strip()
    return kw, control, tail


def split_inline_if_stmt(stmt: str) -> Optional[Tuple[str, str]]:
    """Split inline IF statement into ('if (...)', tail_stmt)."""
    s = stmt.strip()
    low = s.lower()
    if not low.startswith("if"):
        return None
    i = low.find("(")
    if i < 0:
        return None
    j = find_matching_paren(s, i)
    if j < 0:
        return None
    tail = s[j + 1 :].strip()
    if not tail:
        return None
    if tail.lower().endswith("then"):
        return None
    head = s[: j + 1].strip()
    return head, tail


def strip_comment_from_line(raw: str) -> str:
    out: List[str] = []
    in_single = False
    in_double = False
    i = 0
    while i < len(raw):
        ch = raw[i]
        if ch == "'" and not in_double:
            if in_single and i + 1 < len(raw) and raw[i + 1] == "'":
                out.append("''")
                i += 2
                continue
            in_single = not in_single
            out.append(ch)
            i += 1
            continue
        if ch == '"' and not in_single:
            if in_double and i + 1 < len(raw) and raw[i + 1] == '"':
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
    return "".join(out)


def split_eol(raw: str) -> Tuple[str, str]:
    if raw.endswith("\r\n"):
        return raw[:-2], "\r\n"
    if raw.endswith("\n"):
        return raw[:-1], "\n"
    return raw, ""


def iter_statements_with_span(lines: List[str]) -> List[Tuple[int, int, str]]:
    out: List[Tuple[int, int, str]] = []
    cur_parts: List[str] = []
    cur_start: Optional[int] = None
    need_more = False
    last_lineno = 0

    for lineno, raw in enumerate(lines, start=1):
        last_lineno = lineno
        code = strip_comment_from_line(raw).rstrip("\r\n")
        seg = code.rstrip()
        if not seg and not need_more:
            continue

        if cur_start is None:
            cur_start = lineno

        if cur_parts:
            lead = seg.lstrip()
            if lead.startswith("&"):
                seg = lead[1:].lstrip()

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


def proc_scopes(lines: List[str]) -> List[ScopeInfo]:
    scopes: List[ScopeInfo] = [ScopeInfo("__global__", 1, len(lines))]
    stmts = iter_statements_with_span(lines)
    stack: List[Tuple[str, int, int]] = []  # (kind, start_line, ordinal)
    ordinal = 0

    for st, en, stmt in stmts:
        s = stmt.strip().lstrip("\ufeff")
        low = s.lower()
        is_start = PROC_START_RE.match(s) is not None or TYPED_FUNC_RE.match(s) is not None
        is_end = END_PROC_RE.match(s) is not None

        if is_start and not low.startswith("end"):
            m_name = re.search(r"\b(program|module|subroutine|function|block\s*data)\s+([a-z][a-z0-9_]*)", s, re.IGNORECASE)
            if m_name:
                kind = m_name.group(1).lower().replace(" ", "")
                name = m_name.group(2).lower()
            elif TYPED_FUNC_RE.match(s):
                kind = "function"
                m_fn = re.search(r"\bfunction\s+([a-z][a-z0-9_]*)", s, re.IGNORECASE)
                name = m_fn.group(1).lower() if m_fn else f"anon{ordinal+1}"
            else:
                kind = "proc"
                name = f"anon{ordinal+1}"
            ordinal += 1
            stack.append((f"{kind}:{name}", st, ordinal))
            continue

        if is_end and stack:
            kind_name, sline, idx = stack.pop()
            scopes.append(ScopeInfo(f"{kind_name}:{idx}", sline, en))

    # Any unterminated scopes extend to EOF.
    for kind_name, sline, idx in stack:
        scopes.append(ScopeInfo(f"{kind_name}:{idx}", sline, len(lines)))
    return scopes


def scope_for_line(scopes: List[ScopeInfo], line: int) -> str:
    picked = scopes[0]
    for s in scopes[1:]:
        if s.start <= line <= s.end:
            if (s.end - s.start) <= (picked.end - picked.start):
                picked = s
    return picked.key


def is_int_label(tok: str) -> bool:
    return re.fullmatch(r"\d+", tok.strip()) is not None


def rewrite_write_read(stmt: str, label_to_repl: Dict[str, str]) -> Optional[Tuple[str, str]]:
    stmt_prefix, stmt_body = split_stmt_label_prefix(stmt)
    parsed_stmt = parse_write_read_stmt(stmt_body)
    if parsed_stmt is None:
        inl = split_inline_if_stmt(stmt_body)
        if inl is not None:
            head, tail = inl
            out = rewrite_write_read(tail, label_to_repl)
            if out is None:
                out = rewrite_print(tail, label_to_repl)
            if out is None:
                return None
            label, rewritten_tail = out
            return label, f"{stmt_prefix}{head} {rewritten_tail}"
        return None
    kw, control, tail = parsed_stmt
    parts = split_top_level_commas(control)
    if not parts:
        return None

    fmt_idx: Optional[int] = None
    fmt_is_keyword = False
    fmt_label: Optional[str] = None

    for i, p in enumerate(parts):
        kv = split_top_level_equals(p)
        if kv is None:
            continue
        k, v = kv
        if k.strip().lower() == "fmt":
            vv = v.strip()
            if is_int_label(vv):
                fmt_idx = i
                fmt_is_keyword = True
                fmt_label = vv
            break

    if fmt_idx is None and len(parts) >= 2:
        cand = parts[1].strip()
        if is_int_label(cand):
            fmt_idx = 1
            fmt_is_keyword = False
            fmt_label = cand

    if fmt_idx is None or fmt_label is None:
        return None
    repl = label_to_repl.get(fmt_label)
    if repl is None:
        return None

    if fmt_is_keyword:
        k, _v = split_top_level_equals(parts[fmt_idx])  # type: ignore[misc]
        parts[fmt_idx] = f"{k}={repl}"
    else:
        parts[fmt_idx] = repl

    new_stmt = f"{kw}({', '.join(parts)})"
    if tail.strip():
        new_stmt = f"{new_stmt} {tail.strip()}"
    new_stmt = f"{stmt_prefix}{new_stmt}"
    return fmt_label, new_stmt


def rewrite_print(stmt: str, label_to_repl: Dict[str, str]) -> Optional[Tuple[str, str]]:
    stmt_prefix, stmt_body = split_stmt_label_prefix(stmt)
    m = PRINT_RE.match(stmt_body.strip())
    if not m:
        return None
    rest = m.group(1).strip()
    if not rest:
        return None
    parts = split_top_level_commas(rest)
    if not parts:
        return None
    fmt_tok = parts[0].strip()
    if not is_int_label(fmt_tok):
        return None
    repl = label_to_repl.get(fmt_tok)
    if repl is None:
        return None
    parts[0] = repl
    new_stmt = f"{stmt_prefix}print {', '.join(parts)}"
    return fmt_tok, new_stmt


def extract_format_label_usage(stmt: str) -> Optional[str]:
    """Return numeric FORMAT label used by READ/WRITE/PRINT, if any."""
    _stmt_prefix, stmt_body = split_stmt_label_prefix(stmt)
    s0 = stmt_body.strip()
    low = s0.lower()
    if low.startswith("if"):
        i = low.find("(")
        if i >= 0:
            depth = 0
            in_single = False
            in_double = False
            j = i
            while j < len(s0):
                ch = s0[j]
                if ch == "'" and not in_double:
                    if in_single and j + 1 < len(s0) and s0[j + 1] == "'":
                        j += 2
                        continue
                    in_single = not in_single
                    j += 1
                    continue
                if ch == '"' and not in_single:
                    if in_double and j + 1 < len(s0) and s0[j + 1] == '"':
                        j += 2
                        continue
                    in_double = not in_double
                    j += 1
                    continue
                if not in_single and not in_double:
                    if ch == "(":
                        depth += 1
                    elif ch == ")":
                        depth -= 1
                        if depth == 0:
                            tail = s0[j + 1 :].strip()
                            # Inline IF statement (not IF...THEN block header)
                            if tail and not tail.lower().endswith("then"):
                                return extract_format_label_usage(tail)
                            break
                j += 1

    parsed_stmt = parse_write_read_stmt(stmt_body)
    if parsed_stmt is not None:
        _kw, control, _tail = parsed_stmt
        parts = split_top_level_commas(control)
        if not parts:
            return None
        for p in parts:
            kv = split_top_level_equals(p)
            if kv is None:
                continue
            k, v = kv
            if k.strip().lower() == "fmt":
                vv = v.strip()
                if is_int_label(vv):
                    return vv
                return None
        if len(parts) >= 2:
            cand = parts[1].strip()
            if is_int_label(cand):
                return cand
        return None

    m2 = PRINT_RE.match(stmt_body.strip())
    if m2:
        rest = m2.group(1).strip()
        if not rest:
            return None
        parts = split_top_level_commas(rest)
        if not parts:
            return None
        cand = parts[0].strip()
        if is_int_label(cand):
            return cand
    return None


def mk_const_name(label: str, used_names: set[str]) -> str:
    base = f"fmt_{label}"
    name = base
    n = 1
    while name.lower() in used_names:
        name = f"{base}_{n}"
        n += 1
    used_names.add(name.lower())
    return name


def fmt_to_char_literal(fmt: str) -> str:
    return '"' + fmt.replace('"', '""') + '"'


def is_decl_stmt(stmt: str) -> bool:
    st = stmt.strip().lstrip("\ufeff")
    if not st:
        return True
    # Strip leading numeric label if present in fixed-form style.
    st = re.sub(r"^\s*\d+\s+", "", st, count=1)
    if DECL_PREFIX_RE.match(st):
        return True
    # Attribute-only continuation declarations are uncommon; keep conservative.
    return False


def find_insertion_line(lines: List[str], scope: ScopeInfo, stmts: List[Tuple[int, int, str]]) -> int:
    n = len(lines)
    if n == 0:
        return 1
    s0 = max(1, min(scope.start, n))
    e0 = max(s0, min(scope.end, n))
    scope_stmts = [(st, en, ss) for st, en, ss in stmts if s0 <= st <= e0]
    if not scope_stmts:
        return min(n + 1, s0 + 1)

    # Skip the scope header statement itself (SUBROUTINE/FUNCTION/PROGRAM/MODULE).
    start_idx = 0
    if HEADER_STMT_RE.match(scope_stmts[0][2].strip()) and not END_HEADER_RE.match(scope_stmts[0][2].strip()):
        start_idx = 1

    insert_after = scope_stmts[0][1]
    first_exec_start: Optional[int] = None
    for st, en, ss in scope_stmts[start_idx:]:
        sstrip = ss.strip().lstrip("\ufeff")
        if END_HEADER_RE.match(sstrip):
            break
        if is_decl_stmt(sstrip):
            insert_after = en
            continue
        first_exec_start = st
        break

    if first_exec_start is not None:
        return max(1, min(n + 1, first_exec_start))
    return max(1, min(n + 1, insert_after + 1))


def analyze_file(
    path: Path, style: str = "auto"
) -> Tuple[List[Finding], Dict[Tuple[int, int], str], Dict[str, List[Tuple[str, str]]], List[int]]:
    raw_lines = path.read_text(encoding="utf-8", errors="ignore").splitlines(keepends=True)
    stmts = iter_statements_with_span(raw_lines)
    stmt_count_by_line: Dict[int, int] = {}
    for s, _e, _t in stmts:
        stmt_count_by_line[s] = stmt_count_by_line.get(s, 0) + 1

    scopes = proc_scopes(raw_lines)
    scope_map: Dict[str, ScopeInfo] = {s.key: s for s in scopes}

    format_defs: Dict[Tuple[str, str], FormatDef] = {}
    protected_labels: set[Tuple[str, str]] = set()
    used_names_by_scope: Dict[str, set[str]] = {}
    used_names_global: set[str] = set()
    for sline, _eline, stmt in stmts:
        scope = scope_for_line(scopes, sline)
        names = used_names_by_scope.setdefault(scope, set())
        for tok in NAME_TOKEN_RE.findall(stmt):
            low = tok.lower()
            names.add(low)
            used_names_global.add(low)

    for sline, eline, stmt in stmts:
        scope = scope_for_line(scopes, sline)
        m_asg = ASSIGN_LABEL_RE.match(stmt.strip())
        if m_asg:
            protected_labels.add((scope, m_asg.group(1)))
        m = FORMAT_STMT_RE.match(stmt.strip())
        if not m:
            continue
        if stmt_count_by_line.get(sline, 0) != 1:
            continue
        label = m.group(1)
        fmt = m.group(2).strip()
        format_defs[(scope, label)] = FormatDef(label=label, fmt=fmt, line_start=sline, line_end=eline, scope=scope)

    findings: List[Finding] = []
    rewrites: Dict[Tuple[int, int], str] = {}
    label_use_count: Dict[Tuple[str, str], int] = {}
    used_keys: set[Tuple[str, str]] = set()
    label_ref_count: Dict[Tuple[str, str], int] = {}
    use_mode_by_key: Dict[Tuple[str, str], str] = {}

    # Count all numeric FORMAT-label references in READ/WRITE/PRINT across all statements,
    # including continued statements, so we never remove a still-referenced FORMAT label.
    for sline, _eline, stmt in stmts:
        scope = scope_for_line(scopes, sline)
        ulabel = extract_format_label_usage(stmt)
        if ulabel is not None:
            key = (scope, ulabel)
            label_ref_count[key] = label_ref_count.get(key, 0) + 1

    # Decide per-label mode for auto style:
    # const if uses >= 2, otherwise inline.
    if style == "auto":
        for key, nuse in label_ref_count.items():
            use_mode_by_key[key] = "const" if nuse >= 2 else "inline"

    for sline, eline, stmt in stmts:
        if stmt_count_by_line.get(sline, 0) != 1:
            continue
        scope = scope_for_line(scopes, sline)
        # Build replacement map lazily from available format labels in this scope.
        local_map: Dict[str, str] = {}
        for (sc, lb), fd in format_defs.items():
            if sc != scope:
                continue
            key = (scope, lb)
            mode = style
            if style == "auto":
                mode = use_mode_by_key.get(key, "inline")
            if mode == "inline" or (mode == "const" and scope == "__global__"):
                local_map[lb] = fmt_to_char_literal(fd.fmt)
            else:
                # placeholder; finalized after counting uses
                local_map[lb] = f"__CONST__{lb}"

        out_wr = rewrite_write_read(stmt, local_map)
        out_pr = rewrite_print(stmt, local_map)
        out = out_wr if out_wr is not None else out_pr
        if out is None:
            continue
        label, new_stmt = out
        used_keys.add((scope, label))
        mode_used = style
        if style == "auto":
            mode_used = use_mode_by_key.get((scope, label), "inline")
        # Count all successfully rewritten label-uses, regardless of inline/const mode.
        key_used = (scope, label)
        label_use_count[key_used] = label_use_count.get(key_used, 0) + 1
        if mode_used == "const" and scope != "__global__":
            # postpone replacing placeholder with real name
            key = (scope, label)
        rewrites[(sline, eline)] = new_stmt
        findings.append(
            Finding(
                path=path,
                line=sline,
                label=label,
                replacement=new_stmt,
                stmt=stmt.strip(),
            )
        )

    const_decls: Dict[str, List[Tuple[str, str]]] = {}
    format_lines_to_remove: List[int] = []

    if style in {"const", "auto"} and findings:
        const_name_by_key: Dict[Tuple[str, str], str] = {}
        for key, count in label_use_count.items():
            if count <= 0:
                continue
            scope, label = key
            cname = mk_const_name(label, used_names_global)
            const_name_by_key[key] = cname
            used_names_by_scope.setdefault(scope, set()).add(cname.lower())

        # finalize rewrites with real const names
        for sline, eline, stmt in stmts:
            key_stmt = (sline, eline)
            if key_stmt not in rewrites:
                continue
            scope = scope_for_line(scopes, sline)
            local_map: Dict[str, str] = {}
            for (sc, lb), _fd in format_defs.items():
                if sc != scope:
                    continue
                key = (scope, lb)
                mode = style
                if style == "auto":
                    mode = use_mode_by_key.get(key, "inline")
                if mode == "inline" or scope == "__global__":
                    fd = format_defs[(sc, lb)]
                    local_map[lb] = fmt_to_char_literal(fd.fmt)
                    continue
                cname = const_name_by_key.get(key)
                if cname is not None:
                    local_map[lb] = cname
            out = rewrite_write_read(stmt, local_map)
            if out is None:
                out = rewrite_print(stmt, local_map)
            if out is not None:
                _lbl, new_stmt = out
                rewrites[key_stmt] = new_stmt

        # build const declarations and removable format lines
        for (scope, label), fd in format_defs.items():
            key = (scope, label)
            mode = style
            if style == "auto":
                mode = use_mode_by_key.get(key, "inline")
            cname = const_name_by_key.get(key) if mode == "const" else None
            if cname is None:
                # Global-scope fallback uses inline replacement; still remove used FORMAT labels.
                if (
                    key in used_keys
                    and key not in protected_labels
                    and label_use_count.get(key, 0) == label_ref_count.get(key, 0)
                ):
                    format_lines_to_remove.extend(range(fd.line_start, fd.line_end + 1))
                continue
            const_decls.setdefault(scope, []).append((cname, fmt_to_char_literal(fd.fmt)))
            if (
                key in used_keys
                and key not in protected_labels
                and label_use_count.get(key, 0) == label_ref_count.get(key, 0)
            ):
                format_lines_to_remove.extend(range(fd.line_start, fd.line_end + 1))
    elif style == "inline":
        # inline style: remove format lines only when referenced and replaced
        used = {f.label for f in findings}
        for (scope, label), fd in format_defs.items():
            key = (scope, label)
            if (
                label in used
                and key not in protected_labels
                and label_use_count.get(key, 0) == label_ref_count.get(key, 0)
            ):
                format_lines_to_remove.extend(range(fd.line_start, fd.line_end + 1))

    # Scope order stable by source order
    ordered_const_decls: Dict[str, List[Tuple[str, str]]] = {}
    for s in scopes:
        if s.key in const_decls:
            ordered_const_decls[s.key] = const_decls[s.key]

    # Also remove unreferenced FORMAT statements (dead labels), unless protected.
    for key, fd in format_defs.items():
        if key in protected_labels:
            continue
        if label_ref_count.get(key, 0) == 0:
            format_lines_to_remove.extend(range(fd.line_start, fd.line_end + 1))

    # Ensure removed format lines correspond to rewritten labels in same scope.
    rm_set: List[int] = sorted(set(format_lines_to_remove))

    return findings, rewrites, ordered_const_decls, rm_set


def apply_fix(
    path: Path,
    rewrites: Dict[Tuple[int, int], str],
    const_decls: Dict[str, List[Tuple[str, str]]],
    remove_lines: List[int],
    *,
    out_path: Optional[Path] = None,
    create_backup: bool = True,
) -> Tuple[int, Optional[Path]]:
    raw_lines = path.read_text(encoding="utf-8", errors="ignore").splitlines(keepends=True)

    # Replace rewritten statements (single-line or continued).
    changed = 0
    for (line_start, line_end), new_stmt in sorted(rewrites.items(), key=lambda kv: kv[0][0], reverse=True):
        i0 = line_start - 1
        i1 = line_end - 1
        if i0 < 0 or i0 >= len(raw_lines):
            continue
        if i1 < i0:
            continue
        i1 = min(i1, len(raw_lines) - 1)
        body, eol = split_eol(raw_lines[i0])
        indent = re.match(r"^\s*", body).group(0) if body else ""
        first = f"{indent}{new_stmt}{eol}"
        span_len = i1 - i0 + 1
        if span_len <= 1:
            raw_lines[i0] = first
        else:
            filler = [f"{indent}! xformat_statement: rewritten continuation{eol}" for _ in range(span_len - 1)]
            raw_lines[i0 : i1 + 1] = [first] + filler
        changed += 1

    # Remove format lines (line numbers are based on original source; rewrites do not shift lines).
    for line_no in sorted(set(remove_lines), reverse=True):
        idx = line_no - 1
        if 0 <= idx < len(raw_lines):
            del raw_lines[idx]
            changed += 1

    # Recompute scope/stmt views after line deletions, then insert declarations safely.
    scopes = proc_scopes(raw_lines)
    scope_map = {s.key: s for s in scopes}
    stmts = iter_statements_with_span(raw_lines)

    # Insert constant declarations.
    inserts: List[Tuple[int, List[str]]] = []
    for scope_key, decls in const_decls.items():
        scope = scope_map.get(scope_key)
        if scope is None or not decls:
            continue
        ins_line = find_insertion_line(raw_lines, scope, stmts)
        idx = max(0, min(len(raw_lines), ins_line - 1))
        # Use indentation from target line if available.
        indent = ""
        if 0 <= idx < len(raw_lines):
            b, _e = split_eol(raw_lines[idx])
            indent = re.match(r"^\s*", b).group(0) if b else ""
        lines: List[str] = []
        for name, lit in decls:
            lines.append(f"{indent}character(len=*), parameter :: {name} = {lit}\n")
        inserts.append((idx, lines))

    for idx, lines in sorted(inserts, key=lambda x: x[0], reverse=True):
        raw_lines[idx:idx] = lines
        changed += len(lines)

    if changed == 0:
        if out_path is not None:
            out_path.write_text(path.read_text(encoding="utf-8", errors="ignore"), encoding="utf-8")
        return 0, None

    backup: Optional[Path] = None
    target = out_path if out_path is not None else path
    if out_path is None and create_backup:
        backup = make_backup_path(path)
        shutil.copy2(path, backup)
    target.write_text("".join(raw_lines), encoding="utf-8")
    return changed, backup


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
    ap = argparse.ArgumentParser(description="Suggest/fix replacing FORMAT statements with format strings/constants.")
    ap.add_argument("files", nargs="*", type=Path)
    ap.add_argument("--exclude", action="append", default=[])
    ap.add_argument("--verbose", action="store_true")
    ap.add_argument("--fix", action="store_true", help="Apply fixes in place.")
    ap.add_argument("--out", type=Path, help="Write fixed output to separate file (single input file only).")
    ap.add_argument(
        "--style",
        choices=["auto", "const", "inline"],
        default="auto",
        help="Replacement style: auto=const when uses>=2 else inline.",
    )
    ap.add_argument("--backup", dest="backup", action="store_true", default=True)
    ap.add_argument("--no-backup", dest="backup", action="store_false")
    ap.add_argument("--diff", action="store_true")
    ap.add_argument("--compiler", type=str, help='Compile check command (supports "{files}").')
    ap.add_argument("--tee", action="store_true")
    ap.add_argument("--tee-both", action="store_true")
    ap.add_argument("--run", action="store_true")
    ap.add_argument("--run-both", action="store_true")
    ap.add_argument("--run-diff", action="store_true")
    ap.add_argument("--quiet-run", action="store_true")
    ap.add_argument("--keep-exe", action="store_true")
    ap.add_argument("--limit", type=int, help="Maximum number of files to process")
    ap.add_argument("--git", action="store_true", help="Commit changed files")
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
    if args.out is not None and len(args.files) != 1:
        print("--out supports exactly one input file.")
        return 2
    if args.tee and args.out is None:
        print("--tee requires --out.")
        return 2
    if args.run and len(args.files) != 1:
        print("--run/--run-both/--run-diff require exactly one input file.")
        return 2
    if args.diff and not args.fix:
        print("--diff requires --fix.")
        return 2
    if args.compiler and not args.fix:
        print("--compiler requires --fix.")
        return 2

    files = choose_files(list(args.files), args.exclude)
    if not files:
        print("No source files remain after applying --exclude filters.")
        return 2
    if args.limit is not None:
        if args.limit < 1:
            print("--limit must be >= 1.")
            return 2
        files = files[: args.limit]

    baseline_compile_paths = files
    after_compile_paths = [args.out] if (args.fix and args.out is not None) else files
    if args.fix and args.compiler:
        if not fbuild.run_compiler_command(args.compiler, baseline_compile_paths, "baseline", fscan.display_path):
            return 5

    all_findings: List[Finding] = []
    per_file_data: Dict[Path, Tuple[Dict[int, str], Dict[str, List[Tuple[str, str]]], List[int]]] = {}
    for p in files:
        findings, rewrites, const_decls, remove_lines = analyze_file(p, style=args.style)
        all_findings.extend(findings)
        per_file_data[p] = (rewrites, const_decls, remove_lines)
        if args.verbose and findings:
            print(f"{fscan.display_path(p)}: {len(findings)} candidate(s)")
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
                return 5
        if args.run_diff:
            print("Run diff: SKIP (no transformations suggested)")
        print("No FORMAT-statement replacement candidates found.")
        return 0

    if not args.fix:
        print(f"{len(all_findings)} FORMAT-statement replacement candidate(s).")
        for f in all_findings:
            print(f"{fscan.display_path(f.path)}:{f.line} label {f.label} -> {f.replacement}")
            if args.verbose:
                print(f"  {f.stmt}")
        return 0

    changed_files: List[Path] = []
    backup_pairs: List[Tuple[Path, Path]] = []
    total_changes = 0

    for p in files:
        rewrites, const_decls, remove_lines = per_file_data[p]
        if not rewrites and not const_decls and not remove_lines:
            continue
        old_text = p.read_text(encoding="utf-8", errors="ignore")
        out_path = args.out if args.out is not None else None
        if out_path is not None and args.tee_both:
            print(f"--- original: {p} ---")
            print(old_text, end="")
            if not old_text.endswith("\n"):
                print("")
        changed, backup = apply_fix(
            p,
            rewrites,
            const_decls,
            remove_lines,
            out_path=out_path,
            create_backup=args.backup,
        )
        if changed > 0:
            transformed_changed = True
            total_changes += changed
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
                    print(f"Fixed {fscan.display_path(p)}: edits {changed}, backup {backup.name if backup else '(none)'}")
                else:
                    print(f"Fixed {fscan.display_path(p)}: edits {changed}, wrote {out_path}")
        elif out_path is not None and args.tee:
            new_text = out_path.read_text(encoding="utf-8", errors="ignore")
            if args.tee_both:
                print(f"--- transformed: {out_path} ---")
            print(new_text, end="")
            if not new_text.endswith("\n"):
                print("")

    if args.compiler and changed_files:
        ok = fbuild.run_compiler_command(args.compiler, after_compile_paths, "after-fix", fscan.display_path)
        if not ok:
            if args.out is None:
                fbuild.rollback_backups(backup_pairs, fscan.display_path)
            return 5

    if args.git and changed_files:
        fbuild.git_commit_files(
            changed_files,
            f"xformat_statement: replace FORMAT labels with {args.style} format specs",
            fscan.display_path,
        )

    print(f"\n--fix summary: files changed {len(changed_files)}, edits {total_changes}")
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
