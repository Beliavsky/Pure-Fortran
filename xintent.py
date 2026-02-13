#!/usr/bin/env python3
"""Suggest/mark missing INTENT(IN) for Fortran dummy arguments (conservative)."""

from __future__ import annotations

import argparse
import difflib
import fortran_build as fbuild
import re
import shutil
import subprocess
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Optional, Set, Tuple

import cli_paths as cpaths
import fortran_scan as fscan

TYPE_DECL_RE = re.compile(
    r"^\s*(integer|real|logical|character|complex|double\s+precision|type\b|class\b|procedure\b)",
    re.IGNORECASE,
)
NO_COLON_DECL_RE = re.compile(
    r"^\s*(?P<spec>(?:integer|real|logical|complex|character|double\s+precision)\s*(?:\([^)]*\))?"
    r"|type\s*\([^)]*\)|class\s*\([^)]*\))\s*(?P<rhs>[a-z][a-z0-9_]*(?:.*))$",
    re.IGNORECASE,
)
ASSIGN_RE = re.compile(
    r"^\s*(?:\d+\s+)?([a-z][a-z0-9_]*(?:\s*%\s*[a-z][a-z0-9_]*)?)\s*(?:\([^)]*\))?\s*=",
    re.IGNORECASE,
)
ALLOC_DEALLOC_RE = re.compile(r"^\s*(allocate|deallocate)\s*\((.+)\)\s*$", re.IGNORECASE)
CALL_RE = re.compile(r"\bcall\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
EXTERNAL_STMT_RE = re.compile(r"^\s*external\b(.*)$", re.IGNORECASE)
WRITE_START_RE = re.compile(r"^\s*write\s*\(", re.IGNORECASE)
READ_START_RE = re.compile(r"^\s*read\b", re.IGNORECASE)
DO_ITER_RE = re.compile(r"^\s*do\b(?:\s+\d+\s+)?\s*([a-z][a-z0-9_]*)\s*=", re.IGNORECASE)
INTERFACE_START_RE = re.compile(r"^\s*(abstract\s+)?interface\b", re.IGNORECASE)
END_INTERFACE_RE = re.compile(r"^\s*end\s+interface\b", re.IGNORECASE)
PROC_HEADER_ARGS_RE = re.compile(
    r"^\s*(?:(?:pure|elemental|impure|recursive|module)\s+)*(?:function|subroutine)\s+"
    r"[a-z][a-z0-9_]*\s*\(([^)]*)\)",
    re.IGNORECASE,
)


@dataclass
class IntentSuggestion:
    filename: str
    proc_kind: str
    proc_name: str
    proc_start: int
    dummy: str
    decl_line: int
    fixable: bool
    intent: str


@dataclass
class CallSignature:
    """Interprocedural call signature for one procedure name."""

    ordered_dummies: List[str]
    intent_by_dummy: Dict[str, str]  # in/out/inout


def add_summary_item(summary: Dict[Tuple[str, str], List[str]], s: IntentSuggestion) -> None:
    """Add a deduplicated procedure:dummy entry to a summary bucket."""
    key = (s.filename, s.proc_kind)
    token = f"{s.proc_name}:{s.dummy}"
    bucket = summary.setdefault(key, [])
    if token.lower() not in {x.lower() for x in bucket}:
        bucket.append(token)


def print_summary(summary: Dict[Tuple[str, str], List[str]], label: str) -> None:
    """Print grouped function/subroutine intent summary lines."""
    if not summary:
        return
    n_functions = sum(len(v) for (fname, kind), v in summary.items() if kind == "function")
    n_subroutines = sum(len(v) for (fname, kind), v in summary.items() if kind == "subroutine")
    nfiles = len({fname for (fname, _k) in summary.keys()})
    fun_word = "function" if n_functions == 1 else "functions"
    sub_word = "subroutine" if n_subroutines == 1 else "subroutines"
    file_word = "source file" if nfiles == 1 else "source files"
    print(
        f"\nSummary of {n_functions} {fun_word} and {n_subroutines} {sub_word} "
        f"{label} in {nfiles} {file_word}:"
    )
    for (fname, kind) in sorted(summary.keys(), key=lambda x: (x[0].lower(), x[1])):
        items = summary[(fname, kind)]
        if not items:
            continue
        label_kind = kind if len(items) == 1 else f"{kind}s"
        print(f"{fname} {len(items)} {label_kind}: {' '.join(items)}")


def maybe_git_commit(
    do_git: bool,
    changed_files: Set[Path],
    changed_summary: Dict[Tuple[str, str], List[str]],
    intent_label: str,
) -> None:
    """Create a git commit for changed files when --git is enabled."""
    if not do_git or not changed_files:
        return
    n_functions = sum(len(v) for (fname, kind), v in changed_summary.items() if kind == "function")
    n_subroutines = sum(len(v) for (fname, kind), v in changed_summary.items() if kind == "subroutine")
    n_files = len({fname for (fname, _k) in changed_summary.keys()})
    msg = (
        f"intent: mark {intent_label} on {n_functions} function arg(s) and "
        f"{n_subroutines} subroutine arg(s) in {n_files} file(s)"
    )
    fbuild.git_commit_files(sorted(changed_files, key=lambda p: str(p).lower()), msg, fscan.display_path)


def save_transformed_copy(path: Path, save_dir: Path) -> Path:
    """Save a transformed source snapshot into save_dir with a collision-safe filename."""
    resolved = str(path.resolve())
    stem = re.sub(r"[^A-Za-z0-9._-]+", "_", resolved).strip("_")
    if not stem:
        stem = path.name
    out_name = stem
    if not out_name.lower().endswith(path.suffix.lower()):
        out_name += path.suffix
    out_path = save_dir / out_name
    save_dir.mkdir(parents=True, exist_ok=True)
    shutil.copy2(path, out_path)
    return out_path


def quote_cmd_arg(arg: str) -> str:
    """Quote one shell argument for safe compiler command construction."""
    return fbuild.quote_cmd_arg(arg)


def run_compiler_command(command: str, files: List[Path], phase: str) -> bool:
    """Run the configured compiler command and report pass/fail status."""
    return fbuild.run_compiler_command(command, files, phase, fscan.display_path)


def split_code_comment(line: str) -> Tuple[str, str]:
    """Split a line into code and trailing comment parts."""
    in_single = False
    in_double = False
    for i, ch in enumerate(line):
        if ch == "'" and not in_double:
            in_single = not in_single
        elif ch == '"' and not in_single:
            in_double = not in_double
        elif ch == "!" and not in_single and not in_double:
            return line[:i], line[i:]
    return line, ""


def strip_statement_label(stmt: str) -> str:
    """Strip a leading numeric Fortran statement label from one statement string."""
    return re.sub(r"^\s*\d+\s+", "", stmt, count=1)


def strip_one_line_if_prefix(stmt: str) -> str:
    """Strip a leading one-line IF(condition) prefix and return trailing statement text."""
    s = stmt.lstrip()
    if not s.lower().startswith("if"):
        return stmt
    m = re.match(r"^if\s*\(", s, re.IGNORECASE)
    if not m:
        return stmt
    i = m.end() - 1  # index at '('
    depth = 0
    in_single = False
    in_double = False
    for j in range(i, len(s)):
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


def strip_one_line_where_prefix(stmt: str) -> str:
    """Strip a leading one-line WHERE(condition) prefix and return trailing statement text."""
    s = stmt.lstrip()
    if not s.lower().startswith("where"):
        return stmt
    m = re.match(r"^where\s*\(", s, re.IGNORECASE)
    if not m:
        return stmt
    i = m.end() - 1  # index at '('
    depth = 0
    in_single = False
    in_double = False
    for j in range(i, len(s)):
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
                    return tail if tail else stmt
    return stmt


def split_assignment(stmt: str) -> Optional[Tuple[str, str]]:
    """Split a statement into assignment lhs/rhs when it has a true '=' operator."""
    s = stmt.strip()
    if not s:
        return None

    in_single = False
    in_double = False
    depth = 0
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
            continue
        if ch == ")" and depth > 0:
            depth -= 1
            continue
        if ch != "=" or depth != 0:
            continue

        prev = s[i - 1] if i > 0 else ""
        nxt = s[i + 1] if i + 1 < len(s) else ""
        # Exclude relational/equality/pointer operators.
        if prev in {"<", ">", "/", "="}:
            continue
        if nxt == "=":
            continue

        lhs = s[:i].strip()
        rhs = s[i + 1 :].strip()
        if not lhs or not rhs:
            return None
        return lhs, rhs
    return None


def get_eol(line: str) -> str:
    """Return the end-of-line sequence used by a line."""
    if line.endswith("\r\n"):
        return "\r\n"
    if line.endswith("\n"):
        return "\n"
    return ""


def add_intent_attr(line: str, intent: str) -> Tuple[str, bool]:
    """Insert an INTENT attribute into a declaration line when eligible."""
    intent_text = "in out" if intent == "inout" else intent
    code, comment = split_code_comment(line)
    if "::" not in code:
        return line, False
    lhs, rhs = code.split("::", 1)
    if "intent(" in lhs.lower() or re.search(r"\bvalue\b", lhs.lower()):
        return line, False
    new_code = lhs.rstrip() + f", intent({intent_text}) ::" + rhs
    return new_code + comment, True


def split_decl_entities(rhs: str) -> List[str]:
    """Split declaration RHS entities on top-level commas."""
    parts: List[str] = []
    cur: List[str] = []
    depth = 0
    for ch in rhs:
        if ch == "(":
            depth += 1
        elif ch == ")" and depth > 0:
            depth -= 1
        if ch == "," and depth == 0:
            parts.append("".join(cur).strip())
            cur = []
        else:
            cur.append(ch)
    tail = "".join(cur).strip()
    if tail:
        parts.append(tail)
    return parts


def top_level_equals(text: str) -> Optional[Tuple[str, str]]:
    """Split KEY=VALUE at top level (outside strings/parentheses)."""
    in_single = False
    in_double = False
    depth = 0
    for i, ch in enumerate(text):
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
            continue
        if ch == ")" and depth > 0:
            depth -= 1
            continue
        if ch == "=" and depth == 0:
            prev = text[i - 1] if i > 0 else ""
            nxt = text[i + 1] if i + 1 < len(text) else ""
            if prev in {"<", ">", "/", "="} or nxt == "=":
                continue
            return text[:i].strip(), text[i + 1 :].strip()
    return None


def extract_write_unit_base(stmt: str) -> Optional[str]:
    """Return base identifier of WRITE unit/internal-file target, if any."""
    s = stmt.strip()
    if not WRITE_START_RE.match(s):
        return None
    pos = s.lower().find("write")
    lpar = s.find("(", pos)
    if lpar < 0:
        return None
    depth = 0
    in_single = False
    in_double = False
    rpar = -1
    for i in range(lpar, len(s)):
        ch = s[i]
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
            continue
        if ch == ")":
            depth -= 1
            if depth == 0:
                rpar = i
                break
    if rpar < 0:
        return None
    ctl = s[lpar + 1 : rpar].strip()
    if not ctl:
        return None
    parts = split_decl_entities(ctl)
    if not parts:
        return None
    first = parts[0].strip()
    kv = top_level_equals(first)
    unit_expr = ""
    if kv is not None and kv[0].lower() == "unit":
        unit_expr = kv[1]
    elif kv is None:
        unit_expr = first
    else:
        # keyword form without positional unit first; try explicit unit= later.
        for part in parts[1:]:
            kv2 = top_level_equals(part.strip())
            if kv2 is not None and kv2[0].lower() == "unit":
                unit_expr = kv2[1]
                break
    if not unit_expr:
        return None
    return fscan.base_identifier(unit_expr)


def extract_read_iolist_bases(stmt: str) -> Set[str]:
    """Return base identifiers written by READ via its input item list."""
    s = stmt.strip()
    if not READ_START_RE.match(s):
        return set()
    low = s.lower()
    pos = low.find("read")
    tail = s[pos + 4 :].lstrip()
    if not tail:
        return set()

    iolist = ""
    if tail.startswith("("):
        depth = 0
        in_single = False
        in_double = False
        rpar = -1
        for i, ch in enumerate(tail):
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
                continue
            if ch == ")":
                depth -= 1
                if depth == 0:
                    rpar = i
                    break
        if rpar < 0:
            return set()
        iolist = tail[rpar + 1 :].strip()
    elif tail.startswith("*"):
        comma = tail.find(",")
        if comma < 0:
            return set()
        iolist = tail[comma + 1 :].strip()
    else:
        # Unhandled legacy form; do not guess.
        return set()

    if not iolist:
        return set()

    def implied_do_target_base(item: str) -> Optional[str]:
        t = item.strip()
        if not t.startswith("(") or not t.endswith(")"):
            return None
        inner = t[1:-1].strip()
        if not inner:
            return None
        depth = 0
        in_single = False
        in_double = False
        comma = -1
        for i, ch in enumerate(inner):
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
                continue
            if ch == ")" and depth > 0:
                depth -= 1
                continue
            if ch == "," and depth == 0:
                comma = i
                break
        if comma < 0:
            return None
        expr = inner[:comma].strip()
        if not expr:
            return None
        return fscan.base_identifier(expr)

    out: Set[str] = set()
    for item in split_decl_entities(iolist):
        base = fscan.base_identifier(item)
        if not base:
            base = implied_do_target_base(item)
        if base:
            out.add(base)
    return out


def extract_read_control_write_bases(stmt: str) -> Set[str]:
    """Return base identifiers written by READ control specifiers (IOSTAT/IOMSG/SIZE)."""
    s = stmt.strip()
    if not READ_START_RE.match(s):
        return set()
    low = s.lower()
    pos = low.find("read")
    tail = s[pos + 4 :].lstrip()
    if not tail.startswith("("):
        return set()

    depth = 0
    in_single = False
    in_double = False
    rpar = -1
    for i, ch in enumerate(tail):
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
            continue
        if ch == ")":
            depth -= 1
            if depth == 0:
                rpar = i
                break
    if rpar < 0:
        return set()

    ctl = tail[1:rpar].strip()
    if not ctl:
        return set()
    out: Set[str] = set()
    for part in split_decl_entities(ctl):
        kv = top_level_equals(part.strip())
        if kv is None:
            continue
        key = kv[0].lower()
        if key in {"iostat", "iomsg", "size"}:
            base = fscan.base_identifier(kv[1])
            if base:
                out.add(base)
    return out


def parse_external_names(stmt: str) -> Set[str]:
    """Parse names listed in an EXTERNAL statement."""
    low = stmt.lower().strip()
    m = EXTERNAL_STMT_RE.match(low)
    if not m:
        return set()
    rest = m.group(1).strip()
    if not rest:
        return set()
    if rest.startswith("::"):
        rest = rest[2:].strip()
    elif rest.startswith(","):
        rest = rest[1:].strip()
    out: Set[str] = set()
    for tok in split_decl_entities(rest):
        mm = re.match(r"^\s*([a-z][a-z0-9_]*)", tok, re.IGNORECASE)
        if mm:
            out.add(mm.group(1).lower())
    return out


def parse_declared_names_any(code_line: str) -> Set[str]:
    """Parse declared entity names from declaration lines with or without ::."""
    if "::" in code_line:
        return fscan.parse_declared_names_from_decl(code_line)
    m = NO_COLON_DECL_RE.match(code_line.strip())
    if not m:
        return set()
    out: Set[str] = set()
    for ent in split_decl_entities(m.group("rhs").strip()):
        mm = re.match(r"^\s*([a-z][a-z0-9_]*)", ent, re.IGNORECASE)
        if mm:
            out.add(mm.group(1).lower())
    return out


def parse_declared_array_flags(code_line: str) -> Dict[str, bool]:
    """Parse declared entities and whether each is array-like."""
    out: Dict[str, bool] = {}
    if "::" in code_line:
        lhs, _rhs = code_line.split("::", 1)
        has_dim_attr = "dimension" in lhs.lower()
        for name, has_inline_array in fscan.parse_declared_entities(code_line):
            out[name] = has_inline_array or has_dim_attr
        return out

    m = NO_COLON_DECL_RE.match(code_line.strip())
    if not m:
        return out
    has_dim_attr = "dimension" in m.group("spec").lower()
    for ent in split_decl_entities(m.group("rhs")):
        mm = re.match(r"^\s*([a-z][a-z0-9_]*)\s*(\()?", ent, re.IGNORECASE)
        if not mm:
            continue
        out[mm.group(1).lower()] = (mm.group(2) is not None) or has_dim_attr
    return out


def parse_decl_intent(code_line: str) -> Optional[str]:
    """Extract intent from declaration attributes; value implies read-only."""
    low = code_line.lower()
    if re.search(r"\bvalue\b", low):
        return "in"
    m = re.search(r"\bintent\s*\(\s*(in\s*out|inout|in|out)\s*\)", low)
    if not m:
        return None
    tok = re.sub(r"\s+", "", m.group(1).lower())
    if tok == "in":
        return "in"
    if tok == "out":
        return "out"
    return "inout"


def parse_proc_header_ordered_dummies(stmt: str) -> List[str]:
    """Parse ordered dummy names from a procedure header statement."""
    m = PROC_HEADER_ARGS_RE.match(stmt.strip())
    if not m:
        return []
    raw = m.group(1).strip()
    if not raw:
        return []
    out: List[str] = []
    for tok in split_decl_entities(raw):
        mm = re.match(r"^\s*([a-z][a-z0-9_]*)\s*$", tok, re.IGNORECASE)
        if mm:
            out.append(mm.group(1).lower())
    return out


def collect_proc_call_signatures(ordered_files: List[fscan.SourceFileInfo]) -> Dict[str, Optional[CallSignature]]:
    """Collect callee dummy intent signatures; mark ambiguous names as None."""
    global_map: Dict[str, Optional[CallSignature]] = {}
    for finfo in ordered_files:
        sorted_procs = sorted(finfo.procedures, key=lambda p: p.start)
        all_stmts = fscan.iter_fortran_statements(finfo.parsed_lines)
        for pidx, proc in enumerate(sorted_procs):
            if not proc.dummy_names:
                continue
            next_start = sorted_procs[pidx + 1].start if pidx + 1 < len(sorted_procs) else (len(finfo.parsed_lines) + 1)
            body_stmts = statements_in_range(all_stmts, proc.start, next_start)
            start_stmt = ""
            for ln, stmt in body_stmts:
                if ln == proc.start:
                    start_stmt = stmt
                    break
            ordered_dummies = parse_proc_header_ordered_dummies(start_stmt)
            if not ordered_dummies:
                ordered_dummies = sorted(proc.dummy_names)

            intent_by_dummy: Dict[str, str] = {}
            for _ln, code in body_stmts:
                low = code.lower().strip()
                if not low or not TYPE_DECL_RE.match(low):
                    continue
                declared = parse_declared_names_any(low)
                if not declared:
                    continue
                intent = parse_decl_intent(low)
                if intent is None:
                    continue
                for d in ordered_dummies:
                    if d in declared and d not in intent_by_dummy:
                        intent_by_dummy[d] = intent

            sig = CallSignature(ordered_dummies=ordered_dummies, intent_by_dummy=intent_by_dummy)
            key = proc.name.lower()
            prev = global_map.get(key)
            if prev is None and key in global_map:
                continue
            if prev is None:
                global_map[key] = sig
                continue
            if prev.ordered_dummies != sig.ordered_dummies or prev.intent_by_dummy != sig.intent_by_dummy:
                global_map[key] = None
    return global_map


def collect_provisional_proc_intents(ordered_files: List[fscan.SourceFileInfo]) -> Dict[str, Dict[str, str]]:
    """Collect provisional per-procedure dummy intents inferred in current source state."""
    out: Dict[str, Dict[str, str]] = {}
    for finfo in ordered_files:
        sug_in = analyze_intent_suggestions(finfo, target_intent="in", call_sigs=None, interproc=False)
        sug_out = analyze_intent_suggestions(finfo, target_intent="out", call_sigs=None, interproc=False)
        for s in sug_in + sug_out:
            pkey = s.proc_name.lower()
            bucket = out.setdefault(pkey, {})
            cur = bucket.get(s.dummy.lower())
            if cur is None:
                bucket[s.dummy.lower()] = s.intent
            elif cur != s.intent:
                # Conflicting provisional intents are treated as unknown.
                bucket.pop(s.dummy.lower(), None)
    return out


def parse_call_name_and_args(stmt: str) -> Optional[Tuple[str, List[str]]]:
    """Parse CALL name and top-level actual argument list."""
    m = re.search(r"\bcall\s+([a-z][a-z0-9_]*)\b", stmt, re.IGNORECASE)
    if not m:
        return None
    name = m.group(1).lower()
    tail = stmt[m.end() :]
    lpar = tail.find("(")
    if lpar < 0:
        return name, []
    i0 = lpar
    depth = 0
    in_single = False
    in_double = False
    i1 = -1
    for i, ch in enumerate(tail[i0:], start=i0):
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
            continue
        if ch == ")":
            depth -= 1
            if depth == 0:
                i1 = i
                break
    if i1 < 0:
        return name, []
    inside = tail[i0 + 1 : i1].strip()
    if not inside:
        return name, []
    return name, split_decl_entities(inside)


def iter_function_invocations(stmt: str) -> List[Tuple[str, List[str]]]:
    """Find function-like invocations NAME(...) within a statement."""
    out: List[Tuple[str, List[str]]] = []
    s = stmt
    n = len(s)
    i = 0
    in_single = False
    in_double = False
    while i < n:
        ch = s[i]
        if ch == "'" and not in_double:
            in_single = not in_single
            i += 1
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            i += 1
            continue
        if in_single or in_double:
            i += 1
            continue
        if ch.isalpha():
            j = i + 1
            while j < n and (s[j].isalnum() or s[j] == "_"):
                j += 1
            name = s[i:j].lower()
            k = j
            while k < n and s[k].isspace():
                k += 1
            if k < n and s[k] == "(":
                depth = 0
                in_s2 = False
                in_d2 = False
                endp = -1
                m = k
                while m < n:
                    c2 = s[m]
                    if c2 == "'" and not in_d2:
                        in_s2 = not in_s2
                        m += 1
                        continue
                    if c2 == '"' and not in_s2:
                        in_d2 = not in_d2
                        m += 1
                        continue
                    if in_s2 or in_d2:
                        m += 1
                        continue
                    if c2 == "(":
                        depth += 1
                    elif c2 == ")":
                        depth -= 1
                        if depth == 0:
                            endp = m
                            break
                    m += 1
                if endp > k:
                    inner = s[k + 1 : endp].strip()
                    args = split_decl_entities(inner) if inner else []
                    if inner:
                        out.extend(iter_function_invocations(inner))
                    out.append((name, args))
                    i = endp + 1
                    continue
            i = j
            continue
        i += 1
    return out


def resolve_call_formal_intent(
    sig: CallSignature,
    actual: str,
    index: int,
) -> Optional[str]:
    """Resolve formal dummy intent for one actual arg by keyword or position."""
    kv = top_level_equals(actual)
    if kv is not None:
        formal = kv[0].strip().lower()
        if formal in sig.intent_by_dummy:
            return sig.intent_by_dummy[formal]
        return None
    if index >= len(sig.ordered_dummies):
        return None
    formal = sig.ordered_dummies[index]
    return sig.intent_by_dummy.get(formal)


def is_definitely_nonvariable_actual(expr: str) -> bool:
    """Return True when an actual argument is definitely not a variable designator."""
    s = expr.strip()
    if not s:
        return True
    low = s.lower()
    if "'" in s or '"' in s:
        return True
    if re.match(r"^[+-]?(\d+(\.\d*)?|\.\d+)([deq][+-]?\d+)?(_[a-z0-9_]+)?$", low):
        return True
    if low in {".true.", ".false.", ".null."}:
        return True

    depth = 0
    in_single = False
    in_double = False
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
        if in_single or in_double:
            i += 1
            continue
        if ch == "(":
            depth += 1
            i += 1
            continue
        if ch == ")" and depth > 0:
            depth -= 1
            i += 1
            continue
        if depth == 0:
            if ch in "+-*/":
                return True
            if ch == ",":
                return True
            if i + 1 < len(s) and s[i : i + 2] in {"//", "**", "==", "/=", "<=", ">="}:
                return True
            if ch in "<>":
                return True
        i += 1

    var_designator_re = re.compile(
        r"^\s*[a-z][a-z0-9_]*(?:\s*%\s*[a-z][a-z0-9_]*)*(?:\s*\([^)]*\))?\s*$",
        re.IGNORECASE,
    )
    if var_designator_re.match(s):
        return False
    return fscan.base_identifier(s) == ""


def collect_nonvariable_actual_formals(
    ordered_files: List[fscan.SourceFileInfo],
    call_sigs: Dict[str, Optional[CallSignature]],
) -> Dict[str, Set[str]]:
    """Collect formals that are passed definitely non-variable actuals somewhere."""
    out: Dict[str, Set[str]] = {}
    for finfo in ordered_files:
        all_stmts = fscan.iter_fortran_statements(finfo.parsed_lines)
        for _ln, code in all_stmts:
            low = code.lower()
            low_nolab = strip_statement_label(low)
            stmt = strip_one_line_where_prefix(strip_one_line_if_prefix(low_nolab))
            stmt_s = stmt.strip().lower()
            if not stmt_s:
                continue
            if is_declaration_statement(stmt_s):
                continue
            if re.match(
                r"^(subroutine|function|program|module|contains|end\b|use\b|implicit\b|interface\b|type\b|class\b|procedure\b)",
                stmt_s,
            ):
                continue

            invocations: List[Tuple[str, List[str]]] = []
            parsed_call = parse_call_name_and_args(stmt)
            if parsed_call is not None:
                invocations.append(parsed_call)
            invocations.extend(iter_function_invocations(stmt))

            for pname, actuals in invocations:
                sig = call_sigs.get(pname)
                if not isinstance(sig, CallSignature):
                    continue
                for ai, act in enumerate(actuals):
                    expr = top_level_equals(act)[1] if top_level_equals(act) is not None else act
                    if not is_definitely_nonvariable_actual(expr):
                        continue
                    kv = top_level_equals(act)
                    if kv is not None:
                        formal = kv[0].strip().lower()
                    elif ai < len(sig.ordered_dummies):
                        formal = sig.ordered_dummies[ai]
                    else:
                        continue
                    if formal:
                        out.setdefault(pname, set()).add(formal)
    return out


def collect_observed_actual_formals(
    ordered_files: List[fscan.SourceFileInfo],
    call_sigs: Dict[str, Optional[CallSignature]],
) -> Dict[str, Set[str]]:
    """Collect formals that are observed at least once at call sites."""
    out: Dict[str, Set[str]] = {}
    for finfo in ordered_files:
        all_stmts = fscan.iter_fortran_statements(finfo.parsed_lines)
        for _ln, code in all_stmts:
            low = code.lower()
            low_nolab = strip_statement_label(low)
            stmt = strip_one_line_where_prefix(strip_one_line_if_prefix(low_nolab))
            stmt_s = stmt.strip().lower()
            if not stmt_s:
                continue
            if is_declaration_statement(stmt_s):
                continue
            if re.match(
                r"^(subroutine|function|program|module|contains|end\b|use\b|implicit\b|interface\b|type\b|class\b|procedure\b)",
                stmt_s,
            ):
                continue

            invocations: List[Tuple[str, List[str]]] = []
            parsed_call = parse_call_name_and_args(stmt)
            if parsed_call is not None:
                invocations.append(parsed_call)
            invocations.extend(iter_function_invocations(stmt))

            for pname, actuals in invocations:
                sig = call_sigs.get(pname)
                if not isinstance(sig, CallSignature):
                    continue
                for ai, act in enumerate(actuals):
                    kv = top_level_equals(act)
                    if kv is not None:
                        formal = kv[0].strip().lower()
                    elif ai < len(sig.ordered_dummies):
                        formal = sig.ordered_dummies[ai]
                    else:
                        continue
                    if formal:
                        out.setdefault(pname, set()).add(formal)
    return out


def is_declaration_statement(stmt: str) -> bool:
    """Return whether stmt is a declaration statement."""
    s = stmt.strip().lower()
    if not s:
        return False
    # Avoid false positive on assignments like "type = 3".
    if re.match(r"^(type|class|procedure)\s*=", s):
        return False
    if "::" in s:
        return TYPE_DECL_RE.match(s) is not None
    return NO_COLON_DECL_RE.match(s) is not None


def statements_in_range(
    all_stmts: List[Tuple[int, str]],
    start_line: int,
    end_exclusive: int,
) -> List[Tuple[int, str]]:
    """Return statements with start_line <= line < end_exclusive."""
    return [(ln, stmt) for ln, stmt in all_stmts if start_line <= ln < end_exclusive]


def rewrite_semicolon_line_with_intents(line: str, intents_by_name: Dict[str, str]) -> Tuple[List[str], bool]:
    """Rewrite declaration statements on a semicolon line by expanding into separate lines."""
    eol = "\n"
    if line.endswith("\r\n"):
        eol = "\r\n"
    elif line.endswith("\n"):
        eol = "\n"
    code, comment = split_code_comment(line.rstrip("\r\n"))
    stmts = fscan.split_fortran_statements(code)
    if len(stmts) <= 1:
        return [line], False

    out_lines: List[str] = []
    changed = False
    for stmt in stmts:
        new_lines, ok = rewrite_decl_line_with_intents(stmt, intents_by_name)
        if ok:
            changed = True
            for nl in new_lines:
                out_lines.append(nl.rstrip("\r\n") + eol)
        else:
            out_lines.append(stmt.rstrip() + eol)

    if changed and comment:
        out_lines[-1] = out_lines[-1].rstrip("\r\n") + comment + eol
    return out_lines, changed


def collect_continued_statement(lines: List[str], start_idx: int) -> Tuple[int, str, bool]:
    """Collect a continuation statement block and return (end_idx, joined_code, used_continuation)."""
    parts: List[str] = []
    i = start_idx
    used = False
    while i < len(lines):
        raw = lines[i].rstrip("\r\n")
        code, _comment = split_code_comment(raw)
        seg = code.rstrip()
        if i > start_idx:
            lead = seg.lstrip()
            if lead.startswith("&"):
                seg = lead[1:].lstrip()
        has_cont = seg.endswith("&")
        if has_cont:
            seg = seg[:-1].rstrip()
            used = True
        if seg.strip():
            parts.append(seg.strip())
        i += 1
        if has_cont:
            continue
        if i < len(lines):
            next_code, _ = split_code_comment(lines[i].rstrip("\r\n"))
            if next_code.lstrip().startswith("&"):
                used = True
                continue
        break
    return i, " ".join(parts).strip(), used


def rewrite_decl_line_with_intents(line: str, intents_by_name: Dict[str, str]) -> Tuple[List[str], bool]:
    """Rewrite one declaration line to add intents to targeted entities."""
    code, comment = split_code_comment(line)
    lhs = ""
    rhs = ""
    if "::" in code:
        lhs, rhs = code.split("::", 1)
    else:
        m_no = NO_COLON_DECL_RE.match(code.strip())
        if not m_no:
            return [line], False
        lhs = m_no.group("spec")
        rhs = m_no.group("rhs")

    lhs_low = lhs.lower()
    if "intent(" in lhs_low or re.search(r"\bvalue\b", lhs_low):
        return [line], False

    entities = split_decl_entities(rhs)
    if not entities:
        return [line], False

    targeted: List[Tuple[str, str]] = []
    remaining: List[str] = []
    for ent in entities:
        m = re.match(r"^\s*([a-z][a-z0-9_]*)", ent, re.IGNORECASE)
        if not m:
            remaining.append(ent)
            continue
        name = m.group(1).lower()
        if name in intents_by_name:
            targeted.append((ent, intents_by_name[name]))
        else:
            remaining.append(ent)

    if not targeted:
        return [line], False

    out_lines: List[str] = []
    base_lhs = lhs.rstrip()

    for ent, intent in targeted:
        intent_text = "in out" if intent == "inout" else intent
        out_lines.append(f"{base_lhs}, intent({intent_text}) :: {ent}\n")

    if remaining:
        out_lines.append(f"{base_lhs} :: {', '.join(remaining)}")
        if comment:
            out_lines[-1] = out_lines[-1] + comment
        out_lines[-1] += "\n"
    elif comment:
        # keep trailing comment on last emitted line
        out_lines[-1] = out_lines[-1].rstrip("\n") + comment + "\n"

    return out_lines, True


def analyze_intent_suggestions(
    finfo: fscan.SourceFileInfo,
    target_intent: str = "in",
    call_sigs: Optional[Dict[str, Optional[CallSignature]]] = None,
    interproc: bool = False,
    nonvariable_actual_formals: Optional[Dict[str, Set[str]]] = None,
    observed_actual_formals: Optional[Dict[str, Set[str]]] = None,
) -> List[IntentSuggestion]:
    """Suggest dummy arguments that can be marked with the target intent."""
    out: List[IntentSuggestion] = []
    sorted_procs = sorted(finfo.procedures, key=lambda p: p.start)
    all_stmts = fscan.iter_fortran_statements(finfo.parsed_lines)

    for pidx, proc in enumerate(sorted_procs):
        if not proc.dummy_names:
            continue
        next_start = sorted_procs[pidx + 1].start if pidx + 1 < len(sorted_procs) else (len(finfo.parsed_lines) + 1)
        body_stmts = statements_in_range(all_stmts, proc.start, next_start)

        local_names: Set[str] = set(proc.dummy_names)
        if proc.result_name:
            local_names.add(proc.result_name)
        external_dummies: Set[str] = set()
        dummy_is_array: Dict[str, bool] = {d: False for d in proc.dummy_names}
        dummy_is_character: Dict[str, bool] = {d: False for d in proc.dummy_names}

        # dummy -> declaration metadata
        decl: Dict[str, Tuple[int, bool, bool, bool]] = {}
        # line, has_intent_or_value, has_alloc_ptr, fixable

        for ln, code in body_stmts:
            low = code.lower().strip()
            external_dummies.update(parse_external_names(low) & set(proc.dummy_names))
            if not low or not TYPE_DECL_RE.match(low):
                continue
            if low.startswith("procedure"):
                continue
            declared = parse_declared_names_any(low)
            arr_flags = parse_declared_array_flags(low)
            local_names.update(declared)
            has_external = re.search(r"\bexternal\b", low) is not None
            has_intent_or_value = ("intent(" in low) or (re.search(r"\bvalue\b", low) is not None) or has_external
            has_alloc_ptr = ("allocatable" in low) or (re.search(r"\bpointer\b", low) is not None)
            is_character_decl = re.search(r"^\s*character\b", low) is not None
            for d in proc.dummy_names:
                if d in declared and d not in decl:
                    decl[d] = (ln, has_intent_or_value, has_alloc_ptr, True)
                if d in declared and is_character_decl:
                    dummy_is_character[d] = True
                if d in arr_flags and arr_flags[d]:
                    dummy_is_array[d] = True

        probable_proc_dummies: Set[str] = set()
        for _ln, code in body_stmts:
            low = code.lower().strip()
            if not low or is_declaration_statement(low) or low.startswith("procedure"):
                continue
            for d in proc.dummy_names:
                if dummy_is_array.get(d, False):
                    continue
                if re.search(rf"\b{re.escape(d)}\s*\(", low):
                    probable_proc_dummies.add(d)

        writes: Set[str] = set()
        maybe_written_via_call: Set[str] = set()
        reads: Set[str] = set()
        spec_reads: Set[str] = set()
        const_like: Set[str] = set()
        first_event: Dict[str, Tuple[int, str]] = {}
        for ln, code in body_stmts:
            low = code.lower()
            low_nolab = strip_statement_label(low)
            low_exec = strip_one_line_if_prefix(low_nolab)
            low_exec = strip_one_line_where_prefix(low_exec)
            if is_declaration_statement(low_exec):
                declared_here = parse_declared_names_any(low_exec)
                rhs_decl = ""
                if "::" in low_exec:
                    rhs_decl = low_exec.split("::", 1)[1]
                else:
                    m_no = NO_COLON_DECL_RE.match(low_exec.strip())
                    if m_no:
                        rhs_decl = m_no.group("rhs")
                for d in proc.dummy_names:
                    if d in declared_here:
                        # Dummy may still appear in shape/init expressions of other
                        # entities in the same declaration statement.
                        if rhs_decl:
                            for ent in split_decl_entities(rhs_decl):
                                mm_ent = re.match(r"^\s*([a-z][a-z0-9_]*)", ent, re.IGNORECASE)
                                if mm_ent and mm_ent.group(1).lower() == d:
                                    continue
                                if re.search(rf"\b{re.escape(d)}\b", ent):
                                    spec_reads.add(d)
                                    if d not in first_event:
                                        first_event[d] = (ln, "read")
                                    break
                        for ent in split_decl_entities(low_exec.split("::", 1)[1] if "::" in low_exec else low_exec):
                            mm = re.match(r"^\s*([a-z][a-z0-9_]*)", ent, re.IGNORECASE)
                            if mm and mm.group(1).lower() == d and "=" in ent and "=>" not in ent:
                                const_like.add(d)
                                if d not in first_event:
                                    first_event[d] = (ln, "read")
                                break
                for d in proc.dummy_names:
                    if d in declared_here:
                        continue
                    if re.search(rf"\b{re.escape(d)}\b", low_exec):
                        spec_reads.add(d)
                        if d not in first_event:
                            first_event[d] = (ln, "read")
                continue
            asn = split_assignment(low_exec)
            if asn is not None:
                lhs, rhs = asn
                lhs_base = fscan.base_identifier(lhs)
                if lhs_base in proc.dummy_names:
                    writes.add(lhs_base)
                    if lhs_base not in first_event:
                        first_event[lhs_base] = (ln, "write")
                for d in proc.dummy_names:
                    if re.search(rf"\b{re.escape(d)}\b", rhs):
                        reads.add(d)
                        if d not in first_event:
                            first_event[d] = (ln, "read")
                if interproc and call_sigs is not None:
                    for fname, fargs in iter_function_invocations(rhs):
                        fsig = call_sigs.get(fname)
                        if not isinstance(fsig, CallSignature):
                            continue
                        for ai, act in enumerate(fargs):
                            expr = top_level_equals(act)[1] if top_level_equals(act) is not None else act
                            d = fscan.base_identifier(expr)
                            if d not in proc.dummy_names:
                                continue
                            fint = resolve_call_formal_intent(fsig, act, ai)
                            if fint == "in":
                                reads.add(d)
                                if d not in first_event:
                                    first_event[d] = (ln, "read")
                            else:
                                maybe_written_via_call.add(d)
                                if d not in first_event:
                                    first_event[d] = (ln, "call")

            m_alloc = ALLOC_DEALLOC_RE.match(low_exec)
            if m_alloc:
                first_obj = m_alloc.group(2).split(",", 1)[0].strip()
                obj = fscan.base_identifier(first_obj)
                if obj in proc.dummy_names:
                    writes.add(obj)
                    if obj not in first_event:
                        first_event[obj] = (ln, "write")

            if READ_START_RE.match(low_exec):
                for obj in extract_read_iolist_bases(low_exec):
                    if obj in proc.dummy_names:
                        writes.add(obj)
                        if obj not in first_event:
                            first_event[obj] = (ln, "write")
                for obj in extract_read_control_write_bases(low_exec):
                    if obj in proc.dummy_names:
                        writes.add(obj)
                        if obj not in first_event:
                            first_event[obj] = (ln, "write")

            if "=>" in low_exec:
                for d in proc.dummy_names:
                    if re.search(rf"^\s*{re.escape(d)}\s*=>", low_exec):
                        writes.add(d)
                        if d not in first_event:
                            first_event[d] = (ln, "write")

            if CALL_RE.search(low_exec):
                parsed_call = parse_call_name_and_args(low_exec)
                if parsed_call is None:
                    parsed_call = ("", [])
                callee, actuals = parsed_call
                callee_sig = call_sigs.get(callee) if (interproc and call_sigs is not None and callee) else None
                for ai, act in enumerate(actuals):
                    expr = top_level_equals(act)[1] if top_level_equals(act) is not None else act
                    d = fscan.base_identifier(expr)
                    if d not in proc.dummy_names:
                        continue
                    if interproc and isinstance(callee_sig, CallSignature):
                        fint = resolve_call_formal_intent(callee_sig, act, ai)
                        if fint == "in":
                            reads.add(d)
                            if d not in first_event:
                                first_event[d] = (ln, "read")
                            continue
                    maybe_written_via_call.add(d)
                    if d not in first_event:
                        first_event[d] = (ln, "call")
                if not actuals:
                    for d in proc.dummy_names:
                        if re.search(rf"\b{re.escape(d)}\b", low_exec):
                            maybe_written_via_call.add(d)
                            if d not in first_event:
                                first_event[d] = (ln, "call")

            if interproc and call_sigs is not None:
                for fname, fargs in iter_function_invocations(low_exec):
                    fsig = call_sigs.get(fname)
                    if not isinstance(fsig, CallSignature):
                        continue
                    for ai, act in enumerate(fargs):
                        expr = top_level_equals(act)[1] if top_level_equals(act) is not None else act
                        d = fscan.base_identifier(expr)
                        if d not in proc.dummy_names:
                            continue
                        fint = resolve_call_formal_intent(fsig, act, ai)
                        if fint == "in":
                            reads.add(d)
                            if d not in first_event:
                                first_event[d] = (ln, "read")
                        else:
                            maybe_written_via_call.add(d)
                            if d not in first_event:
                                first_event[d] = (ln, "call")

            write_unit = extract_write_unit_base(low_exec)
            if write_unit in proc.dummy_names and dummy_is_character.get(write_unit, False):
                writes.add(write_unit)
                if write_unit not in first_event:
                    first_event[write_unit] = (ln, "write")

            m_do = DO_ITER_RE.match(low_exec)
            if m_do:
                it_name = m_do.group(1).lower()
                if it_name in proc.dummy_names:
                    writes.add(it_name)
                    if it_name not in first_event:
                        first_event[it_name] = (ln, "write")

        for d in sorted(proc.dummy_names):
            if d in external_dummies or d in probable_proc_dummies:
                continue
            meta = decl.get(d)
            if meta is None:
                continue
            decl_line, has_intent_or_value, has_alloc_ptr, fixable = meta
            if has_intent_or_value or has_alloc_ptr:
                continue
            if target_intent == "in":
                if d in writes or d in maybe_written_via_call:
                    continue
            elif target_intent == "out":
                # Conservative INTENT(OUT): must be written and not read/called before first write.
                if d not in writes:
                    continue
                if nonvariable_actual_formals is not None:
                    if d in nonvariable_actual_formals.get(proc.name.lower(), set()):
                        continue
                if d in const_like:
                    continue
                if d in spec_reads:
                    continue
                ev = first_event.get(d)
                if ev is None or ev[1] != "write":
                    continue
                if d in maybe_written_via_call:
                    continue
                if d in reads and (d not in writes or ev[1] != "write"):
                    continue
            elif target_intent == "inout":
                # Conservative INTENT(IN OUT): written, and evidence of input usage.
                if d not in writes:
                    continue
                # Only mark IN OUT when this formal is actually observed at calls
                # in analyzed files, to avoid tightening interfaces blindly.
                if observed_actual_formals is None:
                    continue
                if d not in observed_actual_formals.get(proc.name.lower(), set()):
                    continue
                if nonvariable_actual_formals is not None:
                    if d in nonvariable_actual_formals.get(proc.name.lower(), set()):
                        continue
                if d in const_like:
                    continue
                if d in maybe_written_via_call:
                    continue
                # Require explicit read-like evidence in the procedure.
                if d not in reads and d not in spec_reads:
                    continue
            else:
                continue
            out.append(
                IntentSuggestion(
                    filename=fscan.display_path(finfo.path),
                    proc_kind=proc.kind,
                    proc_name=proc.name,
                    proc_start=proc.start,
                    dummy=d,
                    decl_line=decl_line,
                    fixable=fixable,
                    intent=target_intent,
                )
            )

    return out


def collect_missing_intent_args(finfo: fscan.SourceFileInfo) -> List[IntentSuggestion]:
    """Collect dummies still lacking INTENT/VALUE annotations."""
    out: List[IntentSuggestion] = []
    sorted_procs = sorted(finfo.procedures, key=lambda p: p.start)
    all_stmts = fscan.iter_fortran_statements(finfo.parsed_lines)
    for pidx, proc in enumerate(sorted_procs):
        if not proc.dummy_names:
            continue
        next_start = sorted_procs[pidx + 1].start if pidx + 1 < len(sorted_procs) else (len(finfo.parsed_lines) + 1)
        body_stmts = statements_in_range(all_stmts, proc.start, next_start)
        decl: Dict[str, Tuple[int, bool]] = {}
        external_dummies: Set[str] = set()
        dummy_is_array: Dict[str, bool] = {d: False for d in proc.dummy_names}
        for ln, code in body_stmts:
            low = code.lower().strip()
            external_dummies.update(parse_external_names(low) & set(proc.dummy_names))
            if not low or not TYPE_DECL_RE.match(low):
                continue
            if low.startswith("procedure"):
                continue
            declared = parse_declared_names_any(low)
            arr_flags = parse_declared_array_flags(low)
            has_external = re.search(r"\bexternal\b", low) is not None
            has_intent_or_value = ("intent(" in low) or (re.search(r"\bvalue\b", low) is not None) or has_external
            for d in proc.dummy_names:
                if d in declared and d not in decl:
                    decl[d] = (ln, has_intent_or_value)
                if d in arr_flags and arr_flags[d]:
                    dummy_is_array[d] = True

        probable_proc_dummies: Set[str] = set()
        for _ln, code in body_stmts:
            low = code.lower().strip()
            if not low or is_declaration_statement(low) or low.startswith("procedure"):
                continue
            for d in proc.dummy_names:
                if dummy_is_array.get(d, False):
                    continue
                if re.search(rf"\b{re.escape(d)}\s*\(", low):
                    probable_proc_dummies.add(d)

        for d in sorted(proc.dummy_names):
            if d in external_dummies or d in probable_proc_dummies:
                continue
            meta = decl.get(d)
            if meta is None:
                continue
            decl_line, has_intent_or_value = meta
            if has_intent_or_value:
                continue
            out.append(
                IntentSuggestion(
                    filename=fscan.display_path(finfo.path),
                    proc_kind=proc.kind,
                    proc_name=proc.name,
                    proc_start=proc.start,
                    dummy=d,
                    decl_line=decl_line,
                    fixable=False,
                    intent="",
                )
            )
    return out


def apply_fix(
    finfo: fscan.SourceFileInfo,
    suggestions: List[IntentSuggestion],
    backup: bool,
    show_diff: bool,
    out_path: Optional[Path] = None,
) -> Tuple[int, Optional[Path], List[IntentSuggestion]]:
    """Apply intent suggestions to a file and optionally create a backup."""
    if not suggestions:
        return 0, None, []

    line_to_suggestions: Dict[int, List[IntentSuggestion]] = {}
    for s in suggestions:
        if not s.fixable:
            continue
        line_to_suggestions.setdefault(s.decl_line, [])
        if all(x.dummy.lower() != s.dummy.lower() for x in line_to_suggestions[s.decl_line]):
            line_to_suggestions[s.decl_line].append(s)

    if not line_to_suggestions:
        return 0, None, []

    updated = finfo.lines[:]
    changed = 0
    changed_items: List[IntentSuggestion] = []
    line_offset = 0
    for ln, slist in sorted(line_to_suggestions.items()):
        idx = ln - 1
        idx_adj = idx + line_offset
        if idx_adj < 0 or idx_adj >= len(updated):
            continue
        intents_by_name = {s.dummy.lower(): s.intent for s in slist}
        new_lines, ok = rewrite_decl_line_with_intents(updated[idx_adj], intents_by_name)
        if not ok and ";" in updated[idx_adj]:
            new_lines, ok = rewrite_semicolon_line_with_intents(updated[idx_adj], intents_by_name)
        if not ok:
            end_idx, joined, used_cont = collect_continued_statement(updated, idx_adj)
            if used_cont and joined:
                new_lines, ok = rewrite_decl_line_with_intents(joined, intents_by_name)
                if ok:
                    eol = get_eol(updated[idx_adj])
                    repl = [ln.rstrip("\r\n") + eol for ln in new_lines]
                    updated[idx_adj:end_idx] = repl
                    line_offset += len(repl) - (end_idx - idx_adj)
                    changed += len(intents_by_name)
                    changed_items.extend(slist)
                    continue
        if ok:
            updated[idx_adj:idx_adj + 1] = new_lines
            line_offset += len(new_lines) - 1
            changed += len(intents_by_name)
            changed_items.extend(slist)

    if changed == 0:
        return 0, None, []

    updated = reorder_arg_decls_before_locals(updated)

    if show_diff:
        diff_to = str(out_path) if out_path is not None else str(finfo.path)
        diff = difflib.unified_diff(
            finfo.lines,
            updated,
            fromfile=str(finfo.path),
            tofile=diff_to,
            lineterm="",
        )
        print("\nProposed diff:")
        for line in diff:
            print(line)

    backup_path: Optional[Path] = None
    if backup and out_path is None:
        backup_path = finfo.path.with_name(finfo.path.name + ".bak")
        shutil.copy2(finfo.path, backup_path)
        print(f"\nBackup written: {backup_path}")

    target = out_path if out_path is not None else finfo.path
    with target.open("w", encoding="utf-8", newline="") as f:
        f.write("".join(updated))
    return changed, backup_path, changed_items


def reorder_arg_decls_before_locals(lines: List[str]) -> List[str]:
    """Reorder simple declaration lines so dummy-arg declarations come before locals."""
    parsed_lines = [ln.rstrip("\r\n") for ln in lines]
    procs = sorted(fscan.parse_procedures(parsed_lines), key=lambda p: p.start)
    if not procs:
        return lines
    stmts = fscan.iter_fortran_statements(parsed_lines)
    out = lines[:]

    for pidx, proc in enumerate(procs):
        if not proc.dummy_names:
            continue
        ordered_dummies = parse_proc_header_ordered_dummies(parsed_lines[proc.start - 1])
        if not ordered_dummies:
            ordered_dummies = sorted(proc.dummy_names)
        dummy_pos = {name: i for i, name in enumerate(ordered_dummies)}
        next_start = procs[pidx + 1].start if pidx + 1 < len(procs) else (len(parsed_lines) + 1)
        body = statements_in_range(stmts, proc.start, next_start)
        decl_idxs: List[int] = []
        arg_line_keys: Dict[int, int] = {}
        fixed_idxs: List[int] = []
        interface_depth = 0
        in_spec_part = True

        for ln, stmt in body:
            low = stmt.lower().strip()
            if INTERFACE_START_RE.match(low):
                interface_depth += 1
                continue
            if END_INTERFACE_RE.match(low):
                if interface_depth > 0:
                    interface_depth -= 1
                continue
            if interface_depth > 0:
                continue
            if not in_spec_part:
                continue
            if not (
                is_declaration_statement(low)
                or low.startswith("implicit ")
                or low.startswith("use ")
                or low.startswith("import ")
            ):
                in_spec_part = False
                continue
            if not low or not TYPE_DECL_RE.match(low) or low.startswith("procedure"):
                continue
            idx = ln - 1
            if idx < 0 or idx >= len(out):
                continue
            raw = out[idx].rstrip("\r\n")
            # Skip complex physical lines; keep reordering conservative.
            if ";" in raw or raw.rstrip().endswith("&") or raw.lstrip().startswith("&"):
                continue
            declared = parse_declared_names_any(low)
            if not declared:
                continue
            decl_idxs.append(idx)
            if re.search(r"\bparameter\b", low):
                fixed_idxs.append(idx)
                continue
            arg_names = [d for d in declared if d in dummy_pos]
            if arg_names:
                arg_line_keys[idx] = min(dummy_pos[d] for d in arg_names)

        if not arg_line_keys:
            continue
        arg_idxs = [idx for idx in decl_idxs if idx in arg_line_keys and idx not in fixed_idxs]
        local_idxs = [idx for idx in decl_idxs if idx not in arg_line_keys and idx not in fixed_idxs]

        # Keep relative order for lines with the same argument position key.
        arg_sorted = sorted(arg_idxs, key=lambda idx: (arg_line_keys[idx], idx))
        fixed_order = [idx for idx in decl_idxs if idx in fixed_idxs]
        want_order = fixed_order + arg_sorted + local_idxs
        have_order = decl_idxs
        if want_order == have_order:
            continue
        repl = [out[i] for i in want_order]
        for i, idx in enumerate(have_order):
            out[idx] = repl[i]

    return out


def rollback_backups(backup_pairs: List[Tuple[Path, Path]]) -> None:
    """Restore files from backups after compile validation fails."""
    fbuild.rollback_backups(backup_pairs, fscan.display_path)


def main() -> int:
    """Parse CLI arguments, run intent analysis/fixes, and print summaries."""
    parser = argparse.ArgumentParser(description="Suggest or add INTENT(IN) for dummy args")
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument(
        "--exclude",
        action="append",
        default=[],
        help="Glob pattern to exclude files (can be repeated)",
    )
    parser.add_argument("--fix", action="store_true", help="Apply INTENT(IN) edits for safe suggestions")
    parser.add_argument("--diff", action="store_true", help="Show unified diff before writing")
    parser.add_argument("--backup", dest="backup", action="store_true", default=True)
    parser.add_argument("--no-backup", dest="backup", action="store_false")
    parser.add_argument("--compiler", type=str, help="Compile command; supports {files} placeholder")
    parser.add_argument("--verbose", action="store_true", help="Show per-file details")
    parser.add_argument("--git", action="store_true", help="Commit changed files to git after successful run")
    parser.add_argument("--out", type=Path, help="With --fix, write transformed output to this file (single input)")
    parser.add_argument("--save-dir", type=Path, help="With --fix, save transformed source snapshots to this directory")
    parser.add_argument(
        "--warn-missing-intent",
        action="store_true",
        help="After final pass, report dummy arguments still lacking INTENT/VALUE",
    )
    parser.add_argument(
        "--suggest-intent-out",
        action="store_true",
        help="Suggest/apply INTENT(OUT) (default is INTENT(IN))",
    )
    parser.add_argument(
        "--suggest-intent-inout",
        action="store_true",
        help="Suggest/apply INTENT(IN OUT)",
    )
    parser.add_argument(
        "--iterate",
        action="store_true",
        help="With --fix, repeat analyze/fix passes until no more changes",
    )
    parser.add_argument(
        "--max-iter",
        type=int,
        default=10,
        help="Maximum iterations for --iterate (default: 10)",
    )
    parser.add_argument(
        "--interproc",
        action="store_true",
        help=(
            "Use conservative interprocedural call intent analysis: "
            "CALL arguments mapped to known callee INTENT(IN)/VALUE are treated as read-only"
        ),
    )
    parser.add_argument("--limit", type=int, help="Maximum number of source files to process")
    args = parser.parse_args()
    if args.out is not None:
        args.fix = True
    if args.iterate and not args.fix:
        print("--iterate requires --fix.")
        return 3
    if args.save_dir and not args.fix:
        print("--save-dir requires --fix.")
        return 3
    if args.max_iter < 1:
        print("--max-iter must be >= 1.")
        return 3
    if args.limit is not None and args.limit < 1:
        print("--limit must be >= 1.")
        return 3
    if args.out is not None and args.iterate:
        print("--out is not supported with --iterate.")
        return 3
    if args.out is not None and args.git:
        print("--out is not supported with --git.")
        return 3

    args.fortran_files = cpaths.expand_path_args(args.fortran_files)
    if not args.fortran_files:
        args.fortran_files = sorted(
            set(Path(".").glob("*.f90")) | set(Path(".").glob("*.F90")),
            key=lambda p: p.name.lower(),
        )
        if not args.fortran_files:
            print("No source files provided and no .f90/.F90 files found in current directory.")
            return 2
    args.fortran_files = fscan.apply_excludes(args.fortran_files, args.exclude)
    if args.limit is not None:
        args.fortran_files = args.fortran_files[: args.limit]
    if not args.fortran_files:
        print("No source files remain after applying --exclude filters.")
        return 2
    if args.out is not None and len(args.fortran_files) != 1:
        print("--out requires exactly one input source file.")
        return 2

    changed_summary: Dict[Tuple[str, str], List[str]] = {}
    changed_files: Set[Path] = set()
    suggest_summary_last: Dict[Tuple[str, str], List[str]] = {}
    max_passes = args.max_iter if args.iterate else 1
    did_baseline_compile = False
    baseline_failed = False

    for it in range(1, max_passes + 1):
        source_files, any_missing = fscan.load_source_files(args.fortran_files)
        if not source_files:
            return 2 if any_missing else 1

        ordered_files, _ = fscan.order_files_least_dependent(source_files)
        if it == 1 and len(ordered_files) > 1:
            print("Processing order: " + " ".join(fscan.display_path(f.path) for f in ordered_files))

        compile_paths = [f.path for f in ordered_files]
        after_compile_paths = compile_paths
        if args.compiler:
            compile_paths, _ = fscan.build_compile_closure(ordered_files)
            after_compile_paths = compile_paths
            if args.out is not None and args.fix:
                after_compile_paths = [args.out]
            if args.fix and not did_baseline_compile:
                if not run_compiler_command(args.compiler, compile_paths, phase="baseline"):
                    baseline_failed = True
                    print("Baseline compile failed; continuing to attempt intent fixes.")
                did_baseline_compile = True

        pass_suggest_summary: Dict[Tuple[str, str], List[str]] = {}
        backup_pairs: List[Tuple[Path, Path]] = []
        pass_changed = 0
        need_call_sigs = args.interproc or args.suggest_intent_inout
        call_sigs = collect_proc_call_signatures(ordered_files) if need_call_sigs else None
        nonvariable_actual_formals: Optional[Dict[str, Set[str]]] = None
        observed_actual_formals: Optional[Dict[str, Set[str]]] = None
        if call_sigs is not None:
            nonvariable_actual_formals = collect_nonvariable_actual_formals(ordered_files, call_sigs)
            observed_actual_formals = collect_observed_actual_formals(ordered_files, call_sigs)

        for finfo in ordered_files:
            suggestions_in = analyze_intent_suggestions(
                finfo,
                target_intent="in",
                call_sigs=call_sigs,
                interproc=args.interproc,
                nonvariable_actual_formals=nonvariable_actual_formals,
                observed_actual_formals=observed_actual_formals,
            )
            suggestions_out: List[IntentSuggestion] = []
            if args.suggest_intent_out:
                suggestions_out = analyze_intent_suggestions(
                    finfo,
                    target_intent="out",
                    call_sigs=call_sigs,
                    interproc=args.interproc,
                    nonvariable_actual_formals=nonvariable_actual_formals,
                    observed_actual_formals=observed_actual_formals,
                )
            suggestions_inout: List[IntentSuggestion] = []
            if args.suggest_intent_inout:
                suggestions_inout = analyze_intent_suggestions(
                    finfo,
                    target_intent="inout",
                    call_sigs=call_sigs,
                    interproc=args.interproc,
                    nonvariable_actual_formals=nonvariable_actual_formals,
                    observed_actual_formals=observed_actual_formals,
                )
            suggestions = suggestions_in + suggestions_out + suggestions_inout
            if args.verbose:
                print(f"File: {fscan.display_path(finfo.path)}")
                if args.suggest_intent_out or args.suggest_intent_inout:
                    print(f"\n{len(suggestions)} Likely INTENT(IN/OUT) suggestions:")
                else:
                    print(f"\n{len(suggestions)} Likely INTENT(IN) suggestions:")
                for s in suggestions:
                    mark = "" if s.fixable else " (manual)"
                    print(f"  - {s.proc_kind} {s.proc_name}:{s.dummy} [line {s.decl_line}]{mark}")

            for s in suggestions:
                add_summary_item(pass_suggest_summary, s)

            if not args.fix:
                continue

            changed, backup_path, changed_items = apply_fix(
                finfo,
                suggestions,
                backup=args.backup,
                show_diff=args.diff,
                out_path=args.out,
            )
            pass_changed += changed
            if backup_path:
                backup_pairs.append((finfo.path, backup_path))
            if changed > 0:
                for s in changed_items:
                    add_summary_item(changed_summary, s)
                changed_files.add(finfo.path)
                if args.save_dir:
                    out_path = save_transformed_copy(finfo.path, args.save_dir)
                    if args.verbose:
                        print(f"Saved transformed copy: {out_path}")
                if args.suggest_intent_out or args.suggest_intent_inout:
                    print(f"\nApplied INTENT(IN/OUT) to {changed} declaration(s).")
                else:
                    print(f"\nApplied INTENT(IN) to {changed} declaration(s).")

        if args.compiler:
            phase = "after-fix" if args.fix else "current"
            check_paths = after_compile_paths if args.fix else compile_paths
            if not run_compiler_command(args.compiler, check_paths, phase=phase):
                if args.fix and args.backup and backup_pairs:
                    rollback_backups(backup_pairs)
                return 5

        if not args.fix:
            suggest_summary_last = pass_suggest_summary
            break

        if not args.iterate:
            break
        if pass_changed == 0:
            break
        if it == max_passes:
            print(f"Reached max iterations ({args.max_iter}).")

    if args.fix:
        maybe_git_commit(
            args.git,
            changed_files,
            changed_summary,
            "intent(in/out)" if (args.suggest_intent_out or args.suggest_intent_inout) else "intent(in)",
        )
        if args.suggest_intent_out or args.suggest_intent_inout:
            print_summary(changed_summary, label="with arguments marked intent(in/out)")
        else:
            print_summary(changed_summary, label="with arguments marked intent(in)")
        if baseline_failed:
            print("\nNote: baseline compile failed before fixes; after-fix compile checks passed.")
    else:
        if args.suggest_intent_out or args.suggest_intent_inout:
            print_summary(suggest_summary_last, label="likely needing intent(in/out)")
        else:
            print_summary(suggest_summary_last, label="likely needing intent(in)")

    if args.warn_missing_intent:
        source_files, any_missing = fscan.load_source_files(args.fortran_files)
        if not source_files:
            return 2 if any_missing else 1
        final_ordered, _ = fscan.order_files_least_dependent(source_files)
        missing_summary: Dict[Tuple[str, str], List[str]] = {}
        missing_count = 0
        for finfo in final_ordered:
            missing = collect_missing_intent_args(finfo)
            for s in missing:
                add_summary_item(missing_summary, s)
                missing_count += 1
        if missing_count > 0:
            print_summary(missing_summary, label="with arguments still missing intent/value")
        else:
            print("\nNo procedure arguments are missing intent/value.")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
