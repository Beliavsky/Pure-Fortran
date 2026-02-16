#!/usr/bin/env python3
"""Suggest/fix removing simple GOTO statements when feasible (phase 1)."""

from __future__ import annotations

import argparse
import difflib
import re
import shlex
import shutil
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Set, Tuple

import cli_paths as cpaths
import fortran_build as fbuild
import fortran_scan as fscan

LABEL_LINE_RE = re.compile(r"^(?P<indent>\s*)(?P<label>\d+)(?P<sep>\s+)(?P<stmt>.*)$")
GOTO_RE = re.compile(r"^\s*go\s*to\s+(?P<label>\d+)\s*$", re.IGNORECASE)
IF_GOTO_RE = re.compile(
    r"^\s*(?:(?P<label>\d+)(?P<sep>\s+))?if\s*\((?P<cond>.+)\)\s*go\s*to\s+(?P<target>\d+)\s*$",
    re.IGNORECASE,
)
ASSIGNED_GOTO_RE = re.compile(
    r"^\s*go\s*to\s+\((?P<labels>[^)]*)\)\s*,\s*[a-z][a-z0-9_]*\s*$",
    re.IGNORECASE,
)
ARITH_IF_RE = re.compile(
    r"^\s*if\s*\(.+\)\s*(?P<l1>\d+)\s*,\s*(?P<l2>\d+)\s*,\s*(?P<l3>\d+)\s*$",
    re.IGNORECASE,
)
DO_LABEL_RE = re.compile(r"^\s*do\s+(?P<label>\d+)\s+.+$", re.IGNORECASE)
ASSIGN_RE = re.compile(r"^\s*assign\s+(?P<label>\d+)\s+to\s+[a-z][a-z0-9_]*\s*$", re.IGNORECASE)
READ_WRITE_RE = re.compile(r"^\s*(read|write)\s*\((?P<ctl>.+)\)\s*(?P<tail>.*)$", re.IGNORECASE)
PRINT_RE = re.compile(r"^\s*print\b\s*(?P<rest>.*)$", re.IGNORECASE)
PROGRAM_START_RE = re.compile(r"^\s*program\b", re.IGNORECASE)
MODULE_START_RE = re.compile(r"^\s*module\b", re.IGNORECASE)
MODULE_PROCEDURE_RE = re.compile(r"^\s*module\s+procedure\b", re.IGNORECASE)


@dataclass
class Finding:
    path: Path
    line: int
    kind: str
    suggest: str


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


def split_eol(raw: str) -> Tuple[str, str]:
    if raw.endswith("\r\n"):
        return raw[:-2], "\r\n"
    if raw.endswith("\n"):
        return raw[:-1], "\n"
    return raw, ""


def split_code_comment(line_noeol: str) -> Tuple[str, str]:
    in_single = False
    in_double = False
    i = 0
    while i < len(line_noeol):
        ch = line_noeol[i]
        if ch == "'" and not in_double:
            if in_single and i + 1 < len(line_noeol) and line_noeol[i + 1] == "'":
                i += 2
                continue
            in_single = not in_single
            i += 1
            continue
        if ch == '"' and not in_single:
            if in_double and i + 1 < len(line_noeol) and line_noeol[i + 1] == '"':
                i += 2
                continue
            in_double = not in_double
            i += 1
            continue
        if ch == "!" and not in_single and not in_double:
            return line_noeol[:i], line_noeol[i:]
        i += 1
    return line_noeol, ""


def parse_labeled_code(code: str) -> Tuple[str, Optional[str], str, str]:
    m = LABEL_LINE_RE.match(code)
    if not m:
        indent = re.match(r"^\s*", code).group(0) if code else ""
        return indent, None, "", code
    return m.group("indent"), m.group("label"), m.group("sep"), m.group("stmt")


def choose_files(args_files: List[Path], exclude: Iterable[str]) -> List[Path]:
    if args_files:
        files = cpaths.expand_path_args(args_files)
    else:
        files = sorted(set(Path(".").glob("*.f90")) | set(Path(".").glob("*.F90")), key=lambda p: p.name.lower())
    return fscan.apply_excludes(files, exclude)


def load_codes(path: Path) -> List[List[Path]]:
    out: List[List[Path]] = []
    for raw in path.read_text(encoding="utf-8", errors="ignore").splitlines():
        line = raw.strip()
        if not line or line.startswith("#"):
            continue
        toks = shlex.split(line, posix=False)
        if toks:
            out.append([Path(tok) for tok in toks])
    return out


def resolve_code_entries(entries: List[List[Path]], code_dir: Optional[Path]) -> List[List[Path]]:
    if code_dir is None:
        return entries
    out: List[List[Path]] = []
    for g in entries:
        out.append([p if p.is_absolute() else (code_dir / p) for p in g])
    return out


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


def negate_condition(cond: str) -> Optional[str]:
    """Return a simplified negation for simple relational conditions."""
    s = cond.strip()
    m = re.match(
        r"^(?P<a>.+?)\s*(?P<op><=|>=|/=|==|<|>|\.le\.|\.ge\.|\.ne\.|\.eq\.|\.lt\.|\.gt\.)\s*(?P<b>.+)$",
        s,
        re.IGNORECASE,
    )
    if not m:
        return None
    a = m.group("a").strip()
    b = m.group("b").strip()
    op_raw = m.group("op")
    op = op_raw.lower()
    map_sym = {
        "<": ">=",
        "<=": ">",
        ">": "<=",
        ">=": "<",
        "==": "/=",
        "/=": "==",
        ".lt.": ".ge.",
        ".le.": ".gt.",
        ".gt.": ".le.",
        ".ge.": ".lt.",
        ".eq.": ".ne.",
        ".ne.": ".eq.",
    }
    nop = map_sym.get(op)
    if nop is None:
        return None
    return f"{a} {nop} {b}"


def find_matching_paren(text: str, start_idx: int) -> int:
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


def parse_computed_goto(stmt: str) -> Optional[Tuple[List[str], str]]:
    s = stmt.strip()
    low = s.lower()
    if low.startswith("goto"):
        rest = s[4:].lstrip()
    elif low.startswith("go to"):
        rest = s[5:].lstrip()
    else:
        return None
    if not rest.startswith("("):
        return None
    j = find_matching_paren(rest, 0)
    if j < 0:
        return None
    labels_txt = rest[1:j]
    tail = rest[j + 1 :].strip()
    if not tail.startswith(","):
        return None
    expr = tail[1:].strip()
    if not expr:
        return None
    labels = [t.strip() for t in labels_txt.split(",") if re.fullmatch(r"\d+", t.strip())]
    if not labels:
        return None
    return labels, expr


def collect_referenced_labels(lines: List[str]) -> Set[str]:
    refs: Set[str] = set()
    for raw in lines:
        body, _eol = split_eol(raw)
        code, _comment = split_code_comment(body)
        _i, _lbl, _s, stmt = parse_labeled_code(code)
        s = stmt.strip()

        mg = GOTO_RE.match(s)
        if mg:
            refs.add(mg.group("label"))

        mif = IF_GOTO_RE.match(s)
        if mif:
            refs.add(mif.group("target"))

        mag = ASSIGNED_GOTO_RE.match(s)
        if mag:
            for tok in mag.group("labels").split(","):
                t = tok.strip()
                if re.fullmatch(r"\d+", t):
                    refs.add(t)

        ma = ARITH_IF_RE.match(s)
        if ma:
            refs.update({ma.group("l1"), ma.group("l2"), ma.group("l3")})

        md = DO_LABEL_RE.match(s)
        if md:
            refs.add(md.group("label"))

        mas = ASSIGN_RE.match(s)
        if mas:
            refs.add(mas.group("label"))

        mrw = READ_WRITE_RE.match(s)
        if mrw:
            ctl = mrw.group("ctl")
            parts = split_top_level_commas(ctl)
            if len(parts) >= 2:
                cand = parts[1].strip()
                if re.fullmatch(r"\d+", cand):
                    refs.add(cand)

        mp = PRINT_RE.match(s)
        if mp:
            rest = mp.group("rest").strip()
            parts = split_top_level_commas(rest)
            if parts and re.fullmatch(r"\d+", parts[0].strip()):
                refs.add(parts[0].strip())
    return refs


def build_label_ref_counts(lines: List[str], scope_ids: List[int]) -> Dict[Tuple[int, str], int]:
    """Count numeric label references keyed by (scope_id, label)."""
    counts: Dict[Tuple[int, str], int] = {}
    for i, raw in enumerate(lines, start=1):
        body, _eol = split_eol(raw)
        code, _comment = split_code_comment(body)
        _ind, _lbl, _sep, stmt = parse_labeled_code(code)
        s = stmt.strip()
        if not s:
            continue
        sid = scope_ids[i]

        def bump(label: str) -> None:
            key = (sid, label)
            counts[key] = counts.get(key, 0) + 1

        mg = GOTO_RE.match(s)
        if mg:
            bump(mg.group("label"))

        mif = IF_GOTO_RE.match(s)
        if mif:
            bump(mif.group("target"))

        mag = ASSIGNED_GOTO_RE.match(s)
        if mag:
            for tok in mag.group("labels").split(","):
                t = tok.strip()
                if re.fullmatch(r"\d+", t):
                    bump(t)

        ma = ARITH_IF_RE.match(s)
        if ma:
            bump(ma.group("l1"))
            bump(ma.group("l2"))
            bump(ma.group("l3"))

        md = DO_LABEL_RE.match(s)
        if md:
            bump(md.group("label"))

        mas = ASSIGN_RE.match(s)
        if mas:
            bump(mas.group("label"))

        mrw = READ_WRITE_RE.match(s)
        if mrw:
            ctl = mrw.group("ctl")
            parts = split_top_level_commas(ctl)
            if len(parts) >= 2:
                cand = parts[1].strip()
                if re.fullmatch(r"\d+", cand):
                    bump(cand)

        mp = PRINT_RE.match(s)
        if mp:
            rest = mp.group("rest").strip()
            parts = split_top_level_commas(rest)
            if parts and re.fullmatch(r"\d+", parts[0].strip()):
                bump(parts[0].strip())
    return counts


def find_statement_lines(lines: List[str]) -> List[int]:
    out: List[int] = []
    for i, raw in enumerate(lines, start=1):
        body, _eol = split_eol(raw)
        code, _comment = split_code_comment(body)
        if code.strip():
            out.append(i)
    return out


def build_scope_ids(lines: List[str]) -> List[int]:
    """Return 1-based scope id per line (0 = global/non-procedure)."""
    n = len(lines)
    ids = [0] * (n + 1)
    procs = fscan.parse_procedures(lines)
    sid = 0
    for p in procs:
        sid += 1
        s = max(1, min(n, p.start))
        e = max(s, min(n, p.end))
        for ln in range(s, e + 1):
            ids[ln] = sid
    return ids


def build_label_def_map(lines: List[str], scope_ids: List[int]) -> Dict[Tuple[int, str], int]:
    out: Dict[Tuple[int, str], int] = {}
    for i, raw in enumerate(lines, start=1):
        body, _eol = split_eol(raw)
        code, _comment = split_code_comment(body)
        _indent, lbl, _sep, _stmt = parse_labeled_code(code)
        if lbl is None:
            continue
        key = (scope_ids[i], lbl)
        if key not in out:
            out[key] = i
    return out


def rewrite_file(raw_lines: List[str], path: Path, *, fix_level: int) -> Tuple[List[str], List[Finding], int]:
    lines = list(raw_lines)
    scope_ids = build_scope_ids(lines)
    findings: List[Finding] = []
    edits = 0

    # Phase 2 (fix_level >= 2): computed GO TO -> SELECT CASE dispatch.
    if fix_level >= 2:
        expanded: List[str] = []
        for i, raw in enumerate(lines, start=1):
            body, eol = split_eol(raw)
            code, comment = split_code_comment(body)
            indent, lbl, sep, stmt = parse_labeled_code(code)
            parsed = parse_computed_goto(stmt)
            if parsed is None:
                expanded.append(raw)
                continue
            labels, expr = parsed
            block: List[str] = [f"{indent}select case ({expr})"]
            for k, lb in enumerate(labels, start=1):
                block.append(f"{indent}case ({k})")
                block.append(f"{indent}   go to {lb}")
            block.append(f"{indent}case default")
            block.append(f"{indent}   continue")
            block.append(f"{indent}end select")
            if lbl is not None:
                block[0] = f"{indent}{lbl}{sep}{block[0].lstrip()}"
            if comment:
                block[0] = f"{block[0]} {comment.strip()}"
            for ln in block:
                expanded.append(f"{ln}{eol or '\n'}")
            findings.append(Finding(path=path, line=i, kind="computed_goto", suggest=f"select case({expr})"))
            edits += 1
        lines = expanded
        scope_ids = build_scope_ids(lines)

    # Pass A: collapse goto chains.
    label_map = build_label_def_map(lines, scope_ids)
    for i in range(1, len(lines) + 1):
        body, eol = split_eol(lines[i - 1])
        code, comment = split_code_comment(body)
        indent, lbl, sep, stmt = parse_labeled_code(code)
        mg = GOTO_RE.match(stmt.strip())
        if not mg:
            continue
        target = mg.group("label")
        tline = label_map.get((scope_ids[i], target))
        if tline is None:
            continue
        t_body, _teol = split_eol(lines[tline - 1])
        t_code, _tc = split_code_comment(t_body)
        _ti, _tl, _ts, t_stmt = parse_labeled_code(t_code)
        mg2 = GOTO_RE.match(t_stmt.strip())
        if not mg2:
            continue
        target2 = mg2.group("label")
        if target2 == target:
            continue
        new_stmt = f"go to {target2}"
        new_code = f"{indent}{lbl}{sep}{new_stmt}" if lbl is not None else f"{indent}{new_stmt}"
        lines[i - 1] = f"{new_code}{comment}{eol}"
        findings.append(Finding(path=path, line=i, kind="goto_chain", suggest=new_stmt))
        edits += 1

    # Pass B: remove goto-to-next-statement.
    stmt_lines = find_statement_lines(lines)
    scope_ids = build_scope_ids(lines)
    label_map = build_label_def_map(lines, scope_ids)
    stmt_pos: Dict[int, int] = {ln: idx for idx, ln in enumerate(stmt_lines)}
    i = 1
    while i <= len(lines):
        body, _eol = split_eol(lines[i - 1])
        code, _comment = split_code_comment(body)
        _in, this_lbl, _sp, stmt = parse_labeled_code(code)
        mg = GOTO_RE.match(stmt.strip())
        if not mg:
            i += 1
            continue
        # Never delete a statement that also defines a numeric label.
        if this_lbl is not None:
            i += 1
            continue
        target = mg.group("label")
        this_pos = stmt_pos.get(i)
        if this_pos is None:
            i += 1
            continue
        if this_pos + 1 >= len(stmt_lines):
            i += 1
            continue
        next_stmt_line = stmt_lines[this_pos + 1]
        tline = label_map.get((scope_ids[i], target))
        if tline is not None and tline == next_stmt_line:
            findings.append(Finding(path=path, line=i, kind="goto_next", suggest="remove goto"))
            lines.pop(i - 1)
            edits += 1
            # Recompute maps after structural edit.
            stmt_lines = find_statement_lines(lines)
            scope_ids = build_scope_ids(lines)
            label_map = build_label_def_map(lines, scope_ids)
            stmt_pos = {ln: idx for idx, ln in enumerate(stmt_lines)}
            continue
        i += 1

    # Pass C: simplify if(cond) goto L; goto M; L:
    # Keep this conservative: only for simple single-line statements (no '&').
    i = 1
    while i + 2 <= len(lines):
        b1, e1 = split_eol(lines[i - 1])
        c1, cm1 = split_code_comment(b1)
        ind1, lb1, sp1, st1 = parse_labeled_code(c1)
        if "&" in c1:
            i += 1
            continue
        m_if = IF_GOTO_RE.match(st1.strip())
        if not m_if:
            i += 1
            continue
        cond = m_if.group("cond").strip()
        lt = m_if.group("target")

        b2, _e2 = split_eol(lines[i])
        c2, _cm2 = split_code_comment(b2)
        if "&" in c2:
            i += 1
            continue
        _i2, lb2, _sp2, st2 = parse_labeled_code(c2)
        # Don't remove middle line if it defines a label; other gotos may target it.
        if lb2 is not None:
            i += 1
            continue
        m_g2 = GOTO_RE.match(st2.strip())
        if not m_g2:
            i += 1
            continue
        mt = m_g2.group("label")

        b3, _e3 = split_eol(lines[i + 1])
        c3, _cm3 = split_code_comment(b3)
        if "&" in c3:
            i += 1
            continue
        _i3, lb3, _sp3, _st3 = parse_labeled_code(c3)
        if lb3 != lt:
            i += 1
            continue

        ncond = negate_condition(cond)
        if ncond is None:
            new_stmt = f"if (.not.({cond})) go to {mt}"
        else:
            new_stmt = f"if ({ncond}) go to {mt}"
        new_code = f"{ind1}{lb1}{sp1}{new_stmt}" if lb1 is not None else f"{ind1}{new_stmt}"
        lines[i - 1] = f"{new_code}{cm1}{e1}"
        lines.pop(i)  # remove unconditional goto line
        findings.append(Finding(path=path, line=i, kind="if_goto_goto", suggest=new_stmt))
        edits += 1
        continue

    # Pass D: goto L / if(cond) goto L where L: return  => return / if(cond) return.
    scope_ids = build_scope_ids(lines)
    label_map = build_label_def_map(lines, scope_ids)
    for i in range(1, len(lines) + 1):
        body, eol = split_eol(lines[i - 1])
        code, comment = split_code_comment(body)
        indent, lbl, sep, stmt = parse_labeled_code(code)
        s = stmt.strip()
        new_stmt: Optional[str] = None
        m_if = IF_GOTO_RE.match(s)
        if m_if:
            target = m_if.group("target")
            tline = label_map.get((scope_ids[i], target))
            if tline is not None:
                t_body, _teol = split_eol(lines[tline - 1])
                t_code, _tcm = split_code_comment(t_body)
                _ti, _tl, _ts, t_stmt = parse_labeled_code(t_code)
                if t_stmt.strip().lower() == "return":
                    new_stmt = f"if ({m_if.group('cond').strip()}) return"
        else:
            m_go = GOTO_RE.match(s)
            if m_go:
                target = m_go.group("label")
                tline = label_map.get((scope_ids[i], target))
                if tline is not None:
                    t_body, _teol = split_eol(lines[tline - 1])
                    t_code, _tcm = split_code_comment(t_body)
                    _ti, _tl, _ts, t_stmt = parse_labeled_code(t_code)
                    if t_stmt.strip().lower() == "return":
                        new_stmt = "return"
        if new_stmt is None:
            continue
        new_code = f"{indent}{lbl}{sep}{new_stmt}" if lbl is not None else f"{indent}{new_stmt}"
        lines[i - 1] = f"{new_code}{comment}{eol}"
        findings.append(Finding(path=path, line=i, kind="goto_return", suggest=new_stmt))
        edits += 1

    # Pass E: forward-guard block
    # if (cond) goto L
    #   ... unlabeled statements ...
    # L continue
    # =>
    # if (.not.(cond)) then
    #   ...
    # end if
    i = 1
    while i <= len(lines):
        body, eol = split_eol(lines[i - 1])
        code, comment = split_code_comment(body)
        indent, lbl, sep, stmt = parse_labeled_code(code)
        if "&" in code:
            i += 1
            continue
        m_if = IF_GOTO_RE.match(stmt.strip())
        if not m_if:
            i += 1
            continue
        cond = m_if.group("cond").strip()
        target = m_if.group("target")
        scope_ids = build_scope_ids(lines)
        label_map = build_label_def_map(lines, scope_ids)
        ref_counts = build_label_ref_counts(lines, scope_ids)
        tline = label_map.get((scope_ids[i], target))
        if tline is None or tline <= i:
            i += 1
            continue
        if ref_counts.get((scope_ids[i], target), 0) != 1:
            i += 1
            continue
        t_body, t_eol = split_eol(lines[tline - 1])
        t_code, _tcomment = split_code_comment(t_body)
        _ti, tlbl, _ts, t_stmt = parse_labeled_code(t_code)
        if tlbl != target or t_stmt.strip().lower() != "continue":
            i += 1
            continue
        safe_block = True
        structural_start_re = re.compile(
            r"^\s*(end\s+do|enddo|end\s+if|endif|else\b|else\s*if\b|elseif\b|case\b|select\s+case\b|contains\b)\b",
            re.IGNORECASE,
        )
        for j in range(i + 1, tline):
            bj, _ej = split_eol(lines[j - 1])
            cj, _cmj = split_code_comment(bj)
            _ij, jlbl, _sj, stj = parse_labeled_code(cj)
            if jlbl is not None or "&" in cj:
                safe_block = False
                break
            if structural_start_re.match(stj.strip()):
                safe_block = False
                break
        if not safe_block:
            i += 1
            continue
        ncond = negate_condition(cond)
        if ncond is None:
            new_stmt = f"if (.not.({cond})) then"
        else:
            new_stmt = f"if ({ncond}) then"
        new_code = f"{indent}{lbl}{sep}{new_stmt}" if lbl is not None else f"{indent}{new_stmt}"
        lines[i - 1] = f"{new_code}{comment}{eol}"
        # Indent enclosed body by 3 spaces for readability/structure.
        for j in range(i + 1, tline):
            bj, ej = split_eol(lines[j - 1])
            lines[j - 1] = f"   {bj}{ej}"
        lines[tline - 1] = f"{indent}end if{t_eol}"
        findings.append(Finding(path=path, line=i, kind="if_goto_block", suggest=new_stmt))
        edits += 1
        i += 1

    # Pass F (fix_level >= 3): two-way branch shaping
    # if (cond) goto Ltrue
    #   A...
    #   goto Lend
    # Ltrue continue
    #   B...
    # Lend continue
    # =>
    # if (.not.(cond)) then
    #   A...
    # else
    #   B...
    # end if
    if fix_level >= 3:
        i = 1
        structural_start_re = re.compile(
            r"^\s*(end\s+do|enddo|end\s+if|endif|else\b|else\s*if\b|elseif\b|case\b|select\s+case\b|contains\b)\b",
            re.IGNORECASE,
        )
        while i <= len(lines):
            body, eol = split_eol(lines[i - 1])
            code, comment = split_code_comment(body)
            indent, lbl, sep, stmt = parse_labeled_code(code)
            if "&" in code:
                i += 1
                continue
            m_if = IF_GOTO_RE.match(stmt.strip())
            if not m_if:
                i += 1
                continue
            cond = m_if.group("cond").strip()
            l_true = m_if.group("target")

            scope_ids = build_scope_ids(lines)
            label_map = build_label_def_map(lines, scope_ids)
            ref_counts = build_label_ref_counts(lines, scope_ids)
            sid = scope_ids[i]
            tline = label_map.get((sid, l_true))
            if tline is None or tline <= i:
                i += 1
                continue
            if ref_counts.get((sid, l_true), 0) != 1:
                i += 1
                continue

            # Find the unconditional goto to branch end in the false block.
            gline = None
            l_end = None
            for j in range(i + 1, tline):
                bj, _ej = split_eol(lines[j - 1])
                cj, _cmj = split_code_comment(bj)
                _ij, jlbl, _sj, stj = parse_labeled_code(cj)
                if jlbl is not None or "&" in cj or structural_start_re.match(stj.strip()):
                    gline = None
                    break
                mg = GOTO_RE.match(stj.strip())
                if mg:
                    gline = j
                    l_end = mg.group("label")
                    break
            if gline is None or l_end is None:
                i += 1
                continue

            endline = label_map.get((sid, l_end))
            if endline is None or endline <= tline:
                i += 1
                continue
            if ref_counts.get((sid, l_end), 0) != 1:
                i += 1
                continue

            t_body, _teol = split_eol(lines[tline - 1])
            t_code, _tcm = split_code_comment(t_body)
            _ti, tlbl, _ts, t_stmt = parse_labeled_code(t_code)
            if tlbl != l_true or t_stmt.strip().lower() != "continue":
                i += 1
                continue

            e_body, e_eol = split_eol(lines[endline - 1])
            e_code, _ecm = split_code_comment(e_body)
            _ei, elbl, _es, e_stmt = parse_labeled_code(e_code)
            if elbl != l_end or e_stmt.strip().lower() != "continue":
                i += 1
                continue

            safe = True
            for j in list(range(i + 1, gline)) + list(range(tline + 1, endline)):
                bj, _ej = split_eol(lines[j - 1])
                cj, _cmj = split_code_comment(bj)
                _ij, jlbl, _sj, stj = parse_labeled_code(cj)
                if jlbl is not None or "&" in cj or structural_start_re.match(stj.strip()):
                    safe = False
                    break
            if not safe:
                i += 1
                continue

            ncond = negate_condition(cond)
            if ncond is None:
                if_stmt = f"if (.not.({cond})) then"
            else:
                if_stmt = f"if ({ncond}) then"
            if_code = f"{indent}{lbl}{sep}{if_stmt}" if lbl is not None else f"{indent}{if_stmt}"
            lines[i - 1] = f"{if_code}{comment}{eol}"
            lines[gline - 1] = f"{indent}else{eol}"
            lines[tline - 1] = f""
            lines[endline - 1] = f"{indent}end if{e_eol}"

            # Indent false and true blocks.
            for j in range(i + 1, gline):
                bj, ej = split_eol(lines[j - 1])
                if bj.strip():
                    lines[j - 1] = f"   {bj}{ej}"
            for j in range(gline + 1, endline):
                if j == tline:
                    continue
                bj, ej = split_eol(lines[j - 1])
                if bj.strip():
                    lines[j - 1] = f"   {bj}{ej}"

            findings.append(Finding(path=path, line=i, kind="if_goto_else", suggest=if_stmt))
            edits += 1
            i = endline + 1

    # NOTE: label removal disabled in phase 1.
    # Removing labels safely requires full fixed/free-form continuation awareness
    # and robust label-reference analysis (FORMAT/END/ERR/DO targets, etc.).

    return lines, findings, edits


def count_goto_like(raw_lines: List[str]) -> int:
    """Count goto-like control-flow statements in source lines."""
    n = 0
    for raw in raw_lines:
        body, _eol = split_eol(raw)
        code, _comment = split_code_comment(body)
        _indent, _lbl, _sep, stmt = parse_labeled_code(code)
        s = stmt.strip()
        if not s:
            continue
        if GOTO_RE.match(s) or IF_GOTO_RE.match(s) or ASSIGNED_GOTO_RE.match(s):
            n += 1
            continue
        if parse_computed_goto(s) is not None:
            n += 1
            continue
    return n


def apply_fix(path: Path, new_text: str, *, out_path: Optional[Path], create_backup: bool) -> Tuple[int, Optional[Path]]:
    old_text = path.read_text(encoding="utf-8", errors="ignore")
    if old_text == new_text:
        if out_path is not None:
            out_path.write_text(old_text, encoding="utf-8")
        return 0, None
    backup: Optional[Path] = None
    target = out_path if out_path is not None else path
    if out_path is None and create_backup:
        backup = make_backup_path(path)
        shutil.copy2(path, backup)
    target.write_text(new_text, encoding="utf-8")
    return 1, backup


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


def out_path_for_source(out_dir: Path, src: Path) -> Path:
    return out_dir / src.name


def default_extract_output_path(src: Path) -> Path:
    """Choose default extract output path: <stem>_goto.f90, then _goto1, _goto2, ..."""
    parent = src.parent if str(src.parent) else Path(".")
    base = parent / f"{src.stem}_goto{src.suffix}"
    if not base.exists():
        return base
    i = 1
    while True:
        cand = parent / f"{src.stem}_goto{i}{src.suffix}"
        if not cand.exists():
            return cand
        i += 1


def file_contains_program_or_module(raw_lines: List[str]) -> bool:
    for _ln, stmt in fscan.iter_fortran_statements(raw_lines):
        s = stmt.strip()
        if MODULE_PROCEDURE_RE.match(s):
            continue
        if PROGRAM_START_RE.match(s) or MODULE_START_RE.match(s):
            return True
    return False


def procedure_has_goto(proc: fscan.Procedure) -> bool:
    for _ln, stmt in proc.body:
        s = stmt.strip()
        if GOTO_RE.match(s) or IF_GOTO_RE.match(s) or ASSIGNED_GOTO_RE.match(s):
            return True
        if parse_computed_goto(s) is not None:
            return True
    return False


def extract_goto_procedures(raw_lines: List[str]) -> Tuple[List[str], int]:
    procs = fscan.parse_procedures(raw_lines)
    out: List[str] = []
    count = 0
    for p in procs:
        if not procedure_has_goto(p):
            continue
        s = max(1, min(len(raw_lines), p.start))
        e = max(s, min(len(raw_lines), p.end))
        out.extend(raw_lines[s - 1 : e])
        if out and not out[-1].endswith("\n"):
            out[-1] = out[-1] + "\n"
        out.append("\n")
        count += 1
    return out, count


def main() -> int:
    argv = sys.argv[1:]
    normalized_argv: List[str] = []
    i = 0
    while i < len(argv):
        tok = argv[i]
        if tok == "--fix" and i + 1 < len(argv) and re.fullmatch(r"[123]", argv[i + 1]):
            normalized_argv.extend(["--fix-level", argv[i + 1], "--fix"])
            i += 2
            continue
        normalized_argv.append(tok)
        i += 1

    ap = argparse.ArgumentParser(description="Suggest/fix removing simple GOTO statements (phase 1).")
    ap.add_argument("--codes", type=Path, help="Path to source list (first path per line is used).")
    ap.add_argument("--code-dir", type=Path, help="Base directory for relative paths in --codes.")
    ap.add_argument("files", nargs="*", type=Path)
    ap.add_argument("--exclude", action="append", default=[])
    ap.add_argument("--verbose", action="store_true")
    ap.add_argument("--find", action="store_true", help="List files containing GO TO statements.")
    ap.add_argument("--extract-goto", action="store_true", help="Extract procedures containing GO TO into *_goto*.f90 files.")
    ap.add_argument("--fix", action="store_true", help="Apply fixes (default level 1).")
    ap.add_argument(
        "--fix-level",
        type=int,
        choices=[1, 2, 3],
        help="Fix aggressiveness: 1 conservative, 2 includes computed-goto rewrite, 3 aggressive structured rewrites.",
    )
    ap.add_argument("--fix-all", action="store_true", help="Enable phase-2 rewrites (equivalent to --fix-level 2).")
    ap.add_argument("--out", type=Path)
    ap.add_argument("--out-dir", type=Path)
    ap.add_argument("--backup", dest="backup", action="store_true", default=True)
    ap.add_argument("--no-backup", dest="backup", action="store_false")
    ap.add_argument("--diff", action="store_true")
    ap.add_argument("--compiler", type=str)
    ap.add_argument("--tee", action="store_true")
    ap.add_argument("--tee-both", action="store_true")
    ap.add_argument("--run", action="store_true")
    ap.add_argument("--run-both", action="store_true")
    ap.add_argument("--run-diff", action="store_true")
    ap.add_argument("--quiet-run", action="store_true")
    ap.add_argument("--keep-exe", action="store_true")
    ap.add_argument("--limit", type=int)
    args = ap.parse_args(normalized_argv)

    fix_level = 0
    if args.fix:
        fix_level = 1
    if args.run_diff:
        args.run_both = True
    if args.run_both:
        args.run = True
    if args.tee_both:
        args.tee = True
    if args.run and args.out is None:
        args.out = Path("temp.f90")
    if args.fix_level is not None:
        fix_level = max(fix_level, args.fix_level)
    if args.out is not None or args.out_dir is not None:
        fix_level = max(fix_level, 1)
    if args.fix_all:
        fix_level = max(fix_level, 2)
    if args.out is not None and args.out_dir is not None:
        print("Use either --out or --out-dir, not both.")
        return 2

    if args.files:
        input_paths = args.files
        if args.code_dir is not None:
            input_paths = [p if p.is_absolute() else (args.code_dir / p) for p in args.files]
        files = choose_files(input_paths, args.exclude)
    elif args.codes is not None:
        entries = resolve_code_entries(load_codes(args.codes), args.code_dir)
        firsts: List[Path] = []
        seen = set()
        for g in entries:
            if not g:
                continue
            p = g[0]
            k = str(p).lower()
            if k in seen:
                continue
            seen.add(k)
            firsts.append(p)
        files = fscan.apply_excludes(firsts, args.exclude)
    else:
        files = choose_files([], args.exclude)

    if args.out is not None and len(files) != 1:
        print("--out supports exactly one input file.")
        return 2
    if args.tee and args.out is None:
        print("--tee requires --out.")
        return 2
    if args.run and len(files) != 1:
        print("--run/--run-both/--run-diff require exactly one input file.")
        return 2
    if args.run and args.out_dir is not None:
        print("--run/--run-both/--run-diff do not support --out-dir.")
        return 2
    if args.extract_goto and args.out is not None and len(files) != 1:
        print("--out supports exactly one input file in --extract-goto mode.")
        return 2
    if args.out_dir is not None:
        if args.out_dir.exists() and not args.out_dir.is_dir():
            print("--out-dir exists but is not a directory.")
            return 2
        args.out_dir.mkdir(parents=True, exist_ok=True)

    if not files:
        print("No Fortran files found.")
        return 0

    if args.extract_goto:
        extracted_files = 0
        extracted_procs = 0
        for p in files:
            raw_lines = p.read_text(encoding="utf-8", errors="ignore").splitlines(keepends=True)
            if file_contains_program_or_module(raw_lines):
                print(f"Skipping {fscan.display_path(p)}: contains PROGRAM/MODULE (extract mode supports procedure-only files).")
                continue
            extracted_lines, nproc = extract_goto_procedures(raw_lines)
            if nproc == 0:
                if args.verbose:
                    print(f"{fscan.display_path(p)}: no procedures with goto")
                continue
            if args.out is not None and len(files) == 1:
                outp = args.out
            elif args.out_dir is not None:
                outp = out_path_for_source(args.out_dir, p.with_name(f"{p.stem}_goto{p.suffix}"))
            else:
                outp = default_extract_output_path(p)
            outp.write_text("".join(extracted_lines), encoding="utf-8")
            print(f"Wrote {fscan.display_path(outp)} ({nproc} procedure(s))")
            extracted_files += 1
            extracted_procs += nproc
        print(f"\n--extract-goto summary: files written {extracted_files}, procedures extracted {extracted_procs}")
        return 0

    if args.find:
        hits = 0
        for p in files:
            text = p.read_text(encoding="utf-8", errors="ignore")
            found = False
            for raw in text.splitlines():
                code, _cm = split_code_comment(raw)
                _i, _l, _s, stmt = parse_labeled_code(code)
                if GOTO_RE.match(stmt.strip()) or IF_GOTO_RE.match(stmt.strip()) or ASSIGNED_GOTO_RE.match(stmt.strip()):
                    found = True
                    break
            if found:
                print(fscan.display_path(p))
                hits += 1
        if args.verbose:
            print(f"\n--find summary: files with goto {hits}")
        return 0

    if args.compiler:
        if not fbuild.run_compiler_command(args.compiler, files, "baseline", fscan.display_path):
            return 1

    per_new: Dict[Path, str] = {}
    per_edits: Dict[Path, int] = {}
    per_gotos_before: Dict[Path, int] = {}
    per_gotos_after: Dict[Path, int] = {}
    all_findings: List[Finding] = []
    orig_out = ""
    orig_err = ""
    xform_out = ""
    xform_err = ""
    transformed_changed = False
    transformed_target: Optional[Path] = None
    for p in files:
        old = p.read_text(encoding="utf-8", errors="ignore")
        old_lines = old.splitlines(keepends=True)
        new_lines, findings, edits = rewrite_file(old_lines, p, fix_level=fix_level)
        old_gotos = count_goto_like(old_lines)
        new_gotos = count_goto_like(new_lines)
        # Prefer transformations that reduce goto-like statements.
        if fix_level >= 2:
            if new_gotos >= old_gotos:
                alt_lines, alt_findings, alt_edits = rewrite_file(old_lines, p, fix_level=1)
                alt_gotos = count_goto_like(alt_lines)
                if alt_gotos < new_gotos:
                    new_lines, findings, edits = alt_lines, alt_findings, alt_edits
                    new_gotos = alt_gotos
        per_new[p] = "".join(new_lines)
        per_edits[p] = edits
        per_gotos_before[p] = old_gotos
        per_gotos_after[p] = new_gotos
        all_findings.extend(findings)

    if args.limit and args.limit > 0 and len(all_findings) > args.limit:
        all_findings = all_findings[: args.limit]

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
        print("No GOTO cleanup candidates found.")
        if fix_level > 0 and args.out is not None:
            p = files[0]
            args.out.write_text(per_new[p], encoding="utf-8")
            print(f"Wrote unchanged output to {fscan.display_path(args.out)}")
        return 0

    if args.verbose:
        print(f"{len(all_findings)} replacement candidate(s).")
        for f in all_findings:
            print(f"{fscan.display_path(f.path)}:{f.line} {f.kind}")
            print(f"  suggest: {f.suggest}")
    else:
        counts: Dict[Path, int] = {}
        for f in all_findings:
            counts[f.path] = counts.get(f.path, 0) + 1
        for p, n in counts.items():
            print(f"{fscan.display_path(p)}: {n} candidate(s)")

    if fix_level == 0:
        return 0

    changed_files = 0
    total_edits = 0
    total_gotos_before = 0
    total_gotos_after = 0
    out_files: List[Path] = []
    for p in files:
        old = p.read_text(encoding="utf-8", errors="ignore")
        new = per_new[p]
        if args.diff and old != new:
            show_diff(old, new, p)
        out_path: Optional[Path] = None
        if args.out is not None and p == files[0]:
            out_path = args.out
        elif args.out_dir is not None:
            out_path = out_path_for_source(args.out_dir, p)
        if out_path is not None and args.tee_both:
            print(f"--- original: {p} ---")
            print(old, end="")
            if not old.endswith("\n"):
                print("")
        changed, backup = apply_fix(p, new, out_path=out_path, create_backup=args.backup)
        if out_path is not None:
            out_files.append(out_path)
        if changed:
            transformed_changed = True
            transformed_target = out_path if out_path is not None else p
            changed_files += 1
            total_edits += per_edits.get(p, 0)
            gb = per_gotos_before.get(p, 0)
            ga = per_gotos_after.get(p, 0)
            total_gotos_before += gb
            total_gotos_after += ga
            if out_path is not None:
                if args.tee:
                    print(f"--- transformed: {out_path} ---")
                    print(new, end="")
                    if not new.endswith("\n"):
                        print("")
                print(
                    f"Fixed {fscan.display_path(p)}: edits {per_edits.get(p, 0)}, "
                    f"gotos {gb} -> {ga}, wrote {fscan.display_path(out_path)}"
                )
            else:
                msg = (
                    f"Fixed {fscan.display_path(p)}: edits {per_edits.get(p, 0)}, "
                    f"gotos {gb} -> {ga}"
                )
                if backup is not None:
                    msg += f", backup {fscan.display_path(backup)}"
                print(msg)
        elif out_path is not None and args.tee:
            if args.tee_both:
                print(f"--- transformed: {out_path} ---")
            print(new, end="")
            if not new.endswith("\n"):
                print("")

    if args.compiler:
        if args.out is not None:
            comp_files = [args.out]
        elif args.out_dir is not None:
            comp_files = out_files if out_files else [out_path_for_source(args.out_dir, p) for p in files]
        else:
            comp_files = files
        ok = fbuild.run_compiler_command(args.compiler, comp_files, "after-fix", fscan.display_path)
        if not ok:
            return 1

    if changed_files:
        print(
            f"\n--goto summary: files changed {changed_files}, "
            f"gotos {total_gotos_before} -> {total_gotos_after} "
            f"(delta {total_gotos_after - total_gotos_before:+d})"
        )
    print(f"--fix summary: files changed {changed_files}, edits {total_edits}")
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
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
