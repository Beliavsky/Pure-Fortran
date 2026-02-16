#!/usr/bin/env python3
"""Wrap eligible explicit-shape module procedures with assumed-shape interfaces."""

from __future__ import annotations

import argparse
import difflib
import re
import shutil
import textwrap
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Set, Tuple

import cli_paths as cpaths
import fortran_build as fbuild
import fortran_scan as fscan

MODULE_START_RE = re.compile(r"^\s*module\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
MODULE_PROCEDURE_RE = re.compile(r"^\s*module\s+procedure\b", re.IGNORECASE)
END_MODULE_RE = re.compile(r"^\s*end\s+module\b", re.IGNORECASE)
CONTAINS_RE = re.compile(r"^\s*contains\b", re.IGNORECASE)
SUBR_START_RE = re.compile(
    r"^\s*(?P<prefix>(?:(?:pure|elemental|impure|recursive|module)\s+)*)subroutine\s+"
    r"(?P<name>[a-z][a-z0-9_]*)\s*\((?P<args>[^)]*)\)",
    re.IGNORECASE,
)
FUNC_START_RE = re.compile(
    r"^\s*(?P<prefix>(?:(?:pure|elemental|impure|recursive|module)\s+)*)function\s+"
    r"(?P<name>[a-z][a-z0-9_]*)\s*\((?P<args>[^)]*)\)\s*(?:result\s*\(\s*(?P<res>[a-z][a-z0-9_]*)\s*\))?",
    re.IGNORECASE,
)
DECL_RE = re.compile(r"^(?P<lhs>.+?)::(?P<rhs>.+)$")
IDENT_RE = re.compile(r"^[a-z][a-z0-9_]*$", re.IGNORECASE)
OLD_DECL_RE = re.compile(
    r"^\s*(?P<type>"
    r"integer(?:\s*\([^)]*\))?|"
    r"real(?:\s*\([^)]*\))?|"
    r"logical(?:\s*\([^)]*\))?|"
    r"complex(?:\s*\([^)]*\))?|"
    r"double\s+precision|"
    r"character(?:\s*\([^)]*\))?|"
    r"type\s*\([^)]*\))"
    r"(?P<attrs>\s*(?:,\s*[^,]+)*)\s+"
    r"(?P<rhs>.+)$",
    re.IGNORECASE,
)


@dataclass
class ModuleInfo:
    name: str
    start: int
    contains_line: Optional[int]
    end: int


@dataclass
class DummyInfo:
    name: str
    decl_lhs: str
    is_array: bool
    dims: List[str]


@dataclass
class ProcWrapPlan:
    module_name: str
    proc_kind: str
    proc_name: str
    public_name: str
    proc_start: int
    proc_end: int
    new_explicit_name: str
    wrapper_name: str
    original_args: List[str]
    wrapper_args: List[str]
    removed_dim_args: List[str]
    dummies: Dict[str, DummyInfo]
    bound_exprs: Dict[str, List[str]]
    result_name: Optional[str] = None
    result_decl_lhs: Optional[str] = None
    result_is_array: bool = False
    result_dims: Optional[List[str]] = None
    has_result_clause: bool = False
    header_comments: Optional[List[str]] = None
    copied_param_decls: Optional[List[str]] = None
    intent_promote_lines: Optional[Dict[str, int]] = None
    dummy_accept_expr: Optional[Dict[str, bool]] = None


def make_backup_path(path: Path) -> Path:
    p = Path(str(path) + ".bak")
    if not p.exists():
        return p
    i = 1
    while True:
        q = Path(f"{path}.bak{i}")
        if not q.exists():
            return q
        i += 1


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


def parse_entity(ent: str) -> Tuple[str, bool, List[str]]:
    s = ent.strip()
    if "=" in s and "=>" not in s:
        s = s.split("=", 1)[0].strip()
    m = re.match(r"^([a-z][a-z0-9_]*)\s*(?:\((.*)\))?$", s, re.IGNORECASE)
    if not m:
        return "", False, []
    nm = m.group(1).lower()
    inner = m.group(2)
    if inner is None:
        return nm, False, []
    dims = [d.strip() for d in split_top_level_commas(inner)]
    return nm, True, dims


def parse_decl_stmt(stmt: str) -> Optional[Tuple[str, str]]:
    """Parse declaration statement into (lhs, rhs), supporting with/without ::."""
    m = DECL_RE.match(stmt)
    if m:
        return m.group("lhs").strip(), m.group("rhs").strip()
    m2 = OLD_DECL_RE.match(stmt.strip())
    if not m2:
        return None
    lhs = m2.group("type").strip()
    attrs = (m2.group("attrs") or "").strip()
    if attrs:
        lhs = f"{lhs}{attrs}"
    rhs = m2.group("rhs").strip()
    # Skip obvious non-declaration constructs that can look similar.
    if "(" in lhs and lhs.lower().startswith("type(") and ")" not in lhs:
        return None
    return lhs, rhs


def split_code_comment(line: str) -> Tuple[str, str]:
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


def add_intent_in_to_decl_line(raw_line: str) -> str:
    code, comment = split_code_comment(raw_line.rstrip("\n"))
    if "intent(" in code.lower().replace(" ", ""):
        return raw_line
    if "::" in code:
        lhs, rhs = code.split("::", 1)
        lhs_new = lhs.rstrip() + ", intent(in) "
        out = f"{lhs_new}::{rhs}"
    else:
        m = OLD_DECL_RE.match(code.strip())
        if not m:
            return raw_line
        # Preserve leading indentation.
        lead = re.match(r"^\s*", code).group(0)
        typ = m.group("type").strip()
        attrs = (m.group("attrs") or "").strip()
        rhs = m.group("rhs")
        lhs_new = typ
        if attrs:
            lhs_new += attrs
        lhs_new += ", intent(in)"
        out = f"{lead}{lhs_new} :: {rhs}"
    if comment:
        out += comment
    return out + ("\n" if raw_line.endswith("\n") else "")


def find_promotable_decl_line(lines: List[str], proc: fscan.Procedure, name: str) -> Optional[int]:
    nm = name.lower()
    for i in range(proc.start, proc.end + 1):
        code = fscan.strip_comment(lines[i - 1]).strip()
        if not code:
            continue
        parsed = parse_decl_stmt(code)
        if parsed is None:
            continue
        lhs, rhs = parsed
        if "integer" not in lhs.lower():
            continue
        if "intent(" in lhs.lower().replace(" ", ""):
            continue
        ents = split_top_level_commas(rhs)
        names = []
        for ent in ents:
            nme, _arr, _dims = parse_entity(ent)
            if nme:
                names.append(nme)
        if len(names) == 1 and names[0] == nm:
            return i
    return None


def collect_called_names(lines: List[str]) -> Set[str]:
    out: Set[str] = set()
    call_re = re.compile(r"^\s*call\s+([a-z][a-z0-9_]*)\s*\(", re.IGNORECASE)
    for _ln, stmt in fscan.iter_fortran_statements(lines):
        m = call_re.match(stmt.strip())
        if m:
            out.add(m.group(1).lower())
    return out


def rewrite_calls_to_explicit(lines: List[str], name_map: Dict[str, str]) -> List[str]:
    """Rewrite in-file CALL targets to renamed explicit-shape procedure names."""
    if not name_map:
        return lines
    out: List[str] = []
    pats = {
        old: re.compile(rf"\bcall\s+{re.escape(old)}\s*\(", re.IGNORECASE)
        for old in name_map
    }
    for raw in lines:
        line = raw.rstrip("\n")
        code, comment = split_code_comment(line)
        new_code = code
        for old, pat in pats.items():
            new_code = pat.sub(f"call {name_map[old]}(", new_code)
        out.append(new_code + comment + ("\n" if raw.endswith("\n") else ""))
    return out


def format_assumed_decl(di: DummyInfo) -> str:
    if di.is_array:
        rank = len(di.dims)
        dims = ", ".join(":" for _ in range(rank))
        shape_annot = ", ".join(di.dims)
        return f"{di.decl_lhs} :: {di.name}({dims}) ! ({shape_annot})\n"
    return f"{di.decl_lhs} :: {di.name}\n"


def find_single_decl_line(lines: List[str], proc_start: int, proc_end: int, name: str) -> Optional[int]:
    nm = name.lower()
    hit: Optional[int] = None
    for i in range(proc_start, proc_end + 1):
        code = fscan.strip_comment(lines[i - 1]).strip()
        if not code:
            continue
        parsed = parse_decl_stmt(code)
        if parsed is None:
            continue
        _lhs, rhs = parsed
        ents = []
        for ent in split_top_level_commas(rhs):
            nme, _arr, _dims = parse_entity(ent)
            if nme:
                ents.append(nme)
        if len(ents) == 1 and ents[0] == nm:
            if hit is not None:
                return None
            hit = i
    return hit


def is_spec_stmt_text(line: str) -> bool:
    s = fscan.strip_comment(line).strip().lower()
    if not s:
        return True
    # Continuation lines may belong to declaration/spec statements.
    if s.startswith("&"):
        return True
    if parse_decl_stmt(s) is not None:
        return True
    for kw in (
        "implicit ",
        "use ",
        "import ",
        "save ",
        "data ",
        "common ",
        "equivalence ",
        "external ",
        "intrinsic ",
        "namelist ",
        "parameter ",
        "public ",
        "private ",
    ):
        if s.startswith(kw):
            return True
    return False


def first_executable_line(lines: List[str], start_line: int) -> int:
    """Return 1-based line index of first executable statement in lines, or end+1."""
    in_cont = False
    stmt_is_spec = True
    for i, raw in enumerate(lines, start=start_line):
        code = fscan.strip_comment(raw).rstrip()
        stripped = code.strip()
        if not stripped:
            continue
        if not in_cont:
            stmt_is_spec = is_spec_stmt_text(raw)
            if not stmt_is_spec:
                return i
        in_cont = code.endswith("&")
    return start_line + len(lines)


def consolidate_simple_integer_decls(lines: List[str]) -> List[str]:
    """Merge simple `integer :: a, b` declaration lines in a block."""
    names: List[str] = []
    seen: Set[str] = set()
    remove_idx: Set[int] = set()
    first_idx: Optional[int] = None
    indent = ""
    for i, ln in enumerate(lines):
        s = fscan.strip_comment(ln).strip()
        parsed = parse_decl_stmt(s) if s else None
        if parsed is None:
            continue
        lhs, rhs = parsed
        if lhs.strip().lower() != "integer":
            continue
        ents = split_top_level_commas(rhs)
        local_names: List[str] = []
        ok = True
        for ent in ents:
            if "=" in ent and "=>" not in ent:
                ok = False
                break
            nm, is_arr, _dims = parse_entity(ent)
            if not nm or is_arr:
                ok = False
                break
            local_names.append(nm)
        if not ok:
            continue
        if first_idx is None:
            first_idx = i
            indent = re.match(r"^\s*", ln).group(0)
        remove_idx.add(i)
        for nm in local_names:
            if nm not in seen:
                seen.add(nm)
                names.append(nm)
    if first_idx is None or not names or len(remove_idx) <= 1:
        return lines

    out: List[str] = []
    for i, ln in enumerate(lines):
        if i == first_idx:
            out.append(f"{indent}integer :: {', '.join(names)}\n")
        if i in remove_idx:
            continue
        out.append(ln)
    return out


def apply_replace_in_place(lines: List[str], plan: ProcWrapPlan) -> bool:
    work = list(lines)

    # Header: keep name, switch argument list to wrapper args.
    hdr = work[plan.proc_start - 1].rstrip("\n")
    code, comment = split_code_comment(hdr)
    if plan.proc_kind == "function":
        if plan.result_name and plan.result_name != plan.proc_name:
            new_code = f"function {plan.proc_name}({', '.join(plan.wrapper_args)}) result({plan.result_name})"
        else:
            new_code = f"function {plan.proc_name}({', '.join(plan.wrapper_args)})"
    else:
        new_code = f"subroutine {plan.proc_name}({', '.join(plan.wrapper_args)})"
    work[plan.proc_start - 1] = new_code + comment + ("\n" if work[plan.proc_start - 1].endswith("\n") else "")

    # Rewrite declaration section in place (supports multi-entity declaration lines).
    targets_wrapper = set(plan.wrapper_args)
    targets_removed = set(plan.removed_dim_args)
    target_result = plan.result_name if (plan.proc_kind == "function" and plan.result_name) else None
    found: Set[str] = set()
    reordered_wrapper_decls: List[str] = []
    reordered_removed_decls: List[str] = []
    reordered_result_decl: List[str] = []

    rebuilt: List[str] = []
    for i in range(plan.proc_start, plan.proc_end - 1):
        raw = work[i]
        stripped = fscan.strip_comment(raw).strip()
        parsed = parse_decl_stmt(stripped) if stripped else None
        if parsed is None:
            rebuilt.append(raw)
            continue
        lhs, rhs = parsed
        ents = split_top_level_commas(rhs)
        keep_ents: List[str] = []
        add_lines: List[str] = []
        for ent in ents:
            nm, is_arr, dims = parse_entity(ent)
            if not nm:
                keep_ents.append(ent)
                continue
            if nm in targets_wrapper:
                found.add(nm)
                reordered_wrapper_decls.append(format_assumed_decl(plan.dummies[nm]))
                continue
            if nm in targets_removed:
                found.add(nm)
                reordered_removed_decls.append(f"integer :: {nm}\n")
                continue
            if target_result is not None and nm == target_result and plan.result_decl_lhs:
                found.add(nm)
                if plan.result_is_array and plan.result_dims:
                    rdims: List[str] = []
                    for d in plan.result_dims:
                        key = d.strip().lower()
                        if key in plan.removed_dim_args:
                            exprs = plan.bound_exprs.get(key, [])
                            rdims.append(exprs[0] if exprs else d)
                        else:
                            rdims.append(d)
                    reordered_result_decl = [f"{plan.result_decl_lhs} :: {target_result}({', '.join(rdims)})\n"]
                else:
                    reordered_result_decl = [f"{plan.result_decl_lhs} :: {target_result}\n"]
                continue
            if is_arr and any(expr_refs_any_name(d, targets_removed) for d in dims):
                # Local/spec declarations depending on removed size dummies are unsafe in in-place mode.
                return False
            if expr_refs_any_name(ent, targets_removed):
                # Catch declarations where removed dummies appear in attributes (for example bounds/len).
                return False
            keep_ents.append(ent)

        if keep_ents:
            rebuilt.append(f"{lhs} :: {', '.join(e.strip() for e in keep_ents if e.strip())}\n")
        rebuilt.extend(add_lines)

    # Ensure all required target declarations were found/replaced.
    required = set(plan.wrapper_args) | set(plan.removed_dim_args)
    if target_result is not None:
        required.add(target_result)
    if not required.issubset(found):
        return False

    # Ensure declarations appear in preferred order:
    # dummy args first, function result next (for functions), then local inferred sizes.
    filtered_rebuilt: List[str] = []
    for ln in rebuilt:
        s = fscan.strip_comment(ln).strip()
        parsed = parse_decl_stmt(s) if s else None
        if parsed is None:
            filtered_rebuilt.append(ln)
            continue
        _lhs, rhs = parsed
        ents = [parse_entity(ent)[0] for ent in split_top_level_commas(rhs)]
        ents = [e for e in ents if e]
        if not ents:
            filtered_rebuilt.append(ln)
            continue
        if any(e in targets_wrapper for e in ents):
            continue
        if any(e in targets_removed for e in ents):
            continue
        if target_result is not None and any(e == target_result for e in ents):
            continue
        filtered_rebuilt.append(ln)

    # Preserve original header comments immediately after signature.
    header_block: List[str] = []
    if plan.header_comments:
        header_block = list(plan.header_comments)
        # Remove the first contiguous comment/blank block from filtered content if present.
        k = 0
        while k < len(filtered_rebuilt):
            s = filtered_rebuilt[k].strip()
            if not s or s.startswith("!"):
                k += 1
                continue
            break
        if k > 0:
            filtered_rebuilt = filtered_rebuilt[k:]

    decl_prefix: List[str] = []
    decl_prefix.extend(reordered_wrapper_decls)
    if reordered_result_decl:
        decl_prefix.extend(reordered_result_decl)
    decl_prefix.extend(reordered_removed_decls)
    # Keep header comments right after signature, but place new declarations
    # after implicit/use/import and parameter declarations.
    insert_pos = 0
    for i, ln in enumerate(filtered_rebuilt):
        s = fscan.strip_comment(ln).strip().lower()
        if not s:
            insert_pos = i + 1
            continue
        if s.startswith("!"):
            insert_pos = i + 1
            continue
        if s.startswith("implicit ") or s.startswith("use ") or s.startswith("import "):
            insert_pos = i + 1
            continue
        parsed = parse_decl_stmt(s)
        if parsed is not None:
            lhs, _rhs = parsed
            if "parameter" in lhs.lower():
                insert_pos = i + 1
                continue
        break
    rebuilt = header_block + filtered_rebuilt[:insert_pos] + decl_prefix + filtered_rebuilt[insert_pos:]

    # Consolidate simple local integer declarations.
    first_exec = first_executable_line(rebuilt, 1)
    split_at = min(len(rebuilt), max(0, first_exec - 1))
    spec_part = consolidate_simple_integer_decls(rebuilt[:split_at])
    rebuilt = spec_part + rebuilt[split_at:]

    # Replace interior procedure lines (excluding header and end line).
    work[plan.proc_start : plan.proc_end - 1] = rebuilt
    delta_mid = len(rebuilt) - (plan.proc_end - plan.proc_start - 1)
    plan.proc_end += delta_mid

    # Insert size checks before first executable statement.
    body = work[plan.proc_start:plan.proc_end - 1]
    insert_at = first_executable_line(body, plan.proc_start + 1)
    checks: List[str] = []
    for b in plan.removed_dim_args:
        exprs = plan.bound_exprs.get(b, [])
        if not exprs:
            continue
        checks.append(f"{b} = {exprs[0]}\n")
        for ex in exprs[1:]:
            checks.append(f'if ({ex} /= {b}) error stop "{plan.proc_name}: inconsistent dimension {b}"\n')
    if checks:
        work[insert_at - 1:insert_at - 1] = checks
        delta = len(checks)
        # Shift following plan line ranges handled by caller.
        plan.proc_end += delta
    lines[:] = work
    return True


def extract_kind_len_names(lhs: str) -> Set[str]:
    out: Set[str] = set()
    for m in re.finditer(r"\b(?:kind|len)\s*=\s*([a-z][a-z0-9_]*)\b", lhs, re.IGNORECASE):
        out.add(m.group(1).lower())
    return out


def expr_refs_any_name(expr: str, names: Set[str]) -> bool:
    e = expr.lower()
    for nm in names:
        if re.search(rf"(?<![a-z0-9_]){re.escape(nm)}(?![a-z0-9_])", e):
            return True
    return False


def scan_modules(lines: List[str]) -> List[ModuleInfo]:
    mods: List[ModuleInfo] = []
    stack: List[Tuple[str, int, Optional[int]]] = []
    for i, raw in enumerate(lines, start=1):
        code = fscan.strip_comment(raw).strip()
        if not code:
            continue
        if MODULE_PROCEDURE_RE.match(code):
            continue
        m = MODULE_START_RE.match(code)
        if m and "procedure" not in code.lower():
            stack.append((m.group(1).lower(), i, None))
            continue
        if stack and CONTAINS_RE.match(code):
            name, st, c = stack[-1]
            if c is None:
                stack[-1] = (name, st, i)
            continue
        if END_MODULE_RE.match(code):
            if not stack:
                continue
            name, st, c = stack.pop()
            mods.append(ModuleInfo(name=name, start=st, contains_line=c, end=i))
    return mods


def module_for_proc(proc: fscan.Procedure, mods: List[ModuleInfo]) -> Optional[ModuleInfo]:
    for m in mods:
        if proc.start > m.start and proc.end < m.end:
            return m
    return None


def collect_proc_statements(lines: List[str], proc: fscan.Procedure) -> List[Tuple[int, str]]:
    out: List[Tuple[int, str]] = []
    for ln, stmt in fscan.iter_fortran_statements(lines):
        if proc.start <= ln <= proc.end:
            out.append((ln, stmt))
    return out


def has_scalar_element_call_actual(stmts: List[Tuple[int, str]], array_dummy_names: Set[str]) -> bool:
    """Detect CALL actuals like x(i) / x(i,j) for array dummies (no colon)."""
    if not array_dummy_names:
        return False
    call_re = re.compile(r"^\s*call\s+[a-z][a-z0-9_]*\s*\((.*)\)\s*$", re.IGNORECASE)
    for _ln, stmt in stmts:
        m = call_re.match(stmt.strip())
        if not m:
            continue
        args_txt = m.group(1).strip()
        if not args_txt:
            continue
        for arg in split_top_level_commas(args_txt):
            a = arg.strip()
            mm = re.match(r"^([a-z][a-z0-9_]*)\s*\((.*)\)$", a, re.IGNORECASE)
            if not mm:
                continue
            nm = mm.group(1).lower()
            inner = mm.group(2)
            if nm in array_dummy_names and ":" not in inner:
                return True
    return False


def is_name_assigned(stmts: List[Tuple[int, str]], name: str) -> bool:
    """Conservative check: whether a dummy appears to be assigned/defined."""
    n = re.escape(name.lower())
    assign_re = re.compile(rf"^\s*(?:\d+\s+)?{n}\b\s*(?:=|\()", re.IGNORECASE)
    do_var_re = re.compile(rf"^\s*do\b[^!]*\b{n}\s*=", re.IGNORECASE)
    read_re = re.compile(r"^\s*read\s*\(", re.IGNORECASE)
    call_re = re.compile(r"^\s*call\s+[a-z][a-z0-9_]*\s*\((.*)\)\s*$", re.IGNORECASE)
    for _ln, stmt in stmts:
        s = stmt.strip()
        if not s:
            continue
        if assign_re.match(s):
            return True
        if do_var_re.match(s):
            return True
        if read_re.match(s) and re.search(rf"\b{n}\b", s, re.IGNORECASE):
            return True
        mcall = call_re.match(s)
        if mcall:
            args_txt = mcall.group(1).strip()
            if args_txt:
                for arg in split_top_level_commas(args_txt):
                    a = arg.strip()
                    if not a:
                        continue
                    if "=" in a and "=>" not in a:
                        _k, a = [p.strip() for p in a.split("=", 1)]
                    base = fscan.base_identifier(a)
                    if base and base.lower() == name.lower():
                        # Conservative: actual args may be defined in called routine.
                        return True
    return False


def collect_header_comments(lines: List[str], proc: fscan.Procedure) -> List[str]:
    """Collect contiguous comment lines immediately after procedure signature."""
    out: List[str] = []
    i = proc.start
    while i < proc.end:
        raw = lines[i]
        stripped = raw.strip()
        if not stripped:
            # Preserve blank lines only if we already captured at least one comment.
            if out:
                out.append(raw if raw.endswith("\n") else raw + "\n")
            i += 1
            continue
        if stripped.startswith("!"):
            out.append(raw if raw.endswith("\n") else raw + "\n")
            i += 1
            continue
        break
    # Trim trailing blank lines in copied comment block.
    while out and not out[-1].strip():
        out.pop()
    return out


def build_wrap_plan(
    lines: List[str],
    proc: fscan.Procedure,
    mod: ModuleInfo,
    *,
    infer_intent_in: bool = False,
    replace: bool = False,
) -> Optional[ProcWrapPlan]:
    if proc.kind.lower() not in {"subroutine", "function"}:
        return None
    header_line = lines[proc.start - 1]
    header = fscan.strip_comment(header_line).strip()
    hm = SUBR_START_RE.match(header) if proc.kind.lower() == "subroutine" else FUNC_START_RE.match(header)
    if not hm:
        return None
    name = hm.group("name").lower()
    args_txt = hm.group("args").strip()
    if not args_txt:
        return None
    args = [a.strip().lower() for a in split_top_level_commas(args_txt) if a.strip()]
    if not args or not all(IDENT_RE.match(a) for a in args):
        return None

    stmts = collect_proc_statements(lines, proc)
    header_comments = collect_header_comments(lines, proc)
    dummies: Dict[str, DummyInfo] = {}
    syms: Dict[str, DummyInfo] = {}
    param_decl_by_name: Dict[str, str] = {}
    for _ln, stmt in stmts:
        parsed_decl = parse_decl_stmt(stmt)
        if parsed_decl is None:
            continue
        lhs, rhs = parsed_decl
        for ent in split_top_level_commas(rhs):
            nm, is_arr, dims = parse_entity(ent)
            if not nm:
                continue
            if nm not in syms:
                syms[nm] = DummyInfo(name=nm, decl_lhs=lhs, is_array=is_arr, dims=dims)
            if nm in args and nm not in dummies:
                dummies[nm] = DummyInfo(name=nm, decl_lhs=lhs, is_array=is_arr, dims=dims)
            if "parameter" in lhs.lower():
                # Keep exact declaration statement text for wrapper reuse.
                param_decl_by_name[nm] = f"{stmt}\n"

    if any(a not in dummies for a in args):
        return None

    bound_exprs: Dict[str, List[str]] = {}
    has_explicit = False
    converted_array_args: Set[str] = set()
    for a in args:
        di = dummies[a]
        if not di.is_array:
            continue
        if not di.dims:
            continue
        all_ident = True
        for k, d in enumerate(di.dims, start=1):
            dm = d.strip().lower()
            if not IDENT_RE.match(dm):
                all_ident = False
                break
            if dm not in dummies:
                all_ident = False
                break
            if len(di.dims) == 1 and k == 1:
                bound_exprs.setdefault(dm, []).append(f"size({a})")
            else:
                bound_exprs.setdefault(dm, []).append(f"size({a}, {k})")
        if all_ident:
            has_explicit = True
            converted_array_args.add(a)

    if not has_explicit:
        return None

    if replace and has_scalar_element_call_actual(stmts, converted_array_args):
        return None

    removed: List[str] = []
    promote_lines: Dict[str, int] = {}
    for nm, exprs in bound_exprs.items():
        di = dummies[nm]
        if di.is_array:
            continue
        if "integer" not in di.decl_lhs.lower():
            continue
        has_intent_in = "intent(in)" in di.decl_lhs.lower().replace(" ", "")
        if not has_intent_in:
            if not infer_intent_in:
                continue
            if is_name_assigned(stmts, nm):
                continue
            pline = find_promotable_decl_line(lines, proc, nm)
            if pline is None:
                continue
            promote_lines[nm] = pline
        removed.append(nm)
    removed = [a for a in args if a in removed]
    if not removed:
        return None

    wrapper_args = [a for a in args if a not in removed]
    if not wrapper_args:
        return None

    for w in wrapper_args:
        if dummies[w].decl_lhs.lower().find("optional") >= 0:
            return None

    result_name: Optional[str] = None
    result_decl_lhs: Optional[str] = None
    result_is_array = False
    result_dims: Optional[List[str]] = None
    if proc.kind.lower() == "function":
        has_result_clause = bool(hm.groupdict().get("res"))
        rn = (hm.group("res").lower() if has_result_clause else None) or proc.result_name or name
        rdi = syms.get(rn)
        if rdi is None:
            return None
        if rdi.decl_lhs.lower().find("optional") >= 0:
            return None
        result_name = rn
        result_decl_lhs = rdi.decl_lhs
        result_is_array = rdi.is_array
        result_dims = list(rdi.dims)
    else:
        has_result_clause = False

    # Copy local parameter declarations needed for kind/len references used in wrapper declarations.
    needed_param_names: Set[str] = set()
    for a in wrapper_args:
        needed_param_names |= extract_kind_len_names(dummies[a].decl_lhs)
    if result_decl_lhs:
        needed_param_names |= extract_kind_len_names(result_decl_lhs)
    copied_param_decls: List[str] = []
    for nm in sorted(needed_param_names):
        decl = param_decl_by_name.get(nm)
        if decl:
            copied_param_decls.append(decl)

    # For call-site adaptation in set-level rewriting:
    # expression actuals like [x] are safe only for input-only dummies.
    dummy_accept_expr: Dict[str, bool] = {}
    for a in args:
        di = dummies.get(a)
        if di is None:
            continue
        lhs_nospace = di.decl_lhs.lower().replace(" ", "")
        if "intent(inout)" in lhs_nospace or "intent(out)" in lhs_nospace:
            dummy_accept_expr[a] = False
            continue
        if "intent(in)" in lhs_nospace:
            dummy_accept_expr[a] = True
            continue
        # No explicit INTENT: conservative local inference (shared with intent workflow)
        # Treat as input-only only when it is not assigned/defined in this procedure.
        dummy_accept_expr[a] = not is_name_assigned(stmts, a)

    return ProcWrapPlan(
        module_name=mod.name,
        proc_kind=proc.kind.lower(),
        proc_name=name,
        public_name=name if replace else f"{name}_assumed_shape",
        proc_start=proc.start,
        proc_end=proc.end,
        new_explicit_name=f"{name}_explicit_shape",
        wrapper_name=name if replace else f"{name}_assumed_shape",
        original_args=args,
        wrapper_args=wrapper_args,
        removed_dim_args=removed,
        dummies=dummies,
        bound_exprs=bound_exprs,
        result_name=result_name,
        result_decl_lhs=result_decl_lhs,
        result_is_array=result_is_array,
        result_dims=result_dims,
        has_result_clause=has_result_clause,
        header_comments=header_comments,
        copied_param_decls=copied_param_decls,
        intent_promote_lines=promote_lines,
        dummy_accept_expr=dummy_accept_expr,
    )


def wrapper_lines(plan: ProcWrapPlan) -> List[str]:
    out: List[str] = []
    arglist = ", ".join(plan.wrapper_args)
    if plan.proc_kind == "function":
        if not plan.result_name or not plan.result_decl_lhs:
            return out
        out.append(f"function {plan.public_name}({arglist}) result({plan.result_name})\n")
    else:
        out.append(f"subroutine {plan.public_name}({arglist})\n")
    if plan.header_comments:
        out.extend(plan.header_comments)
    if plan.copied_param_decls:
        out.extend(plan.copied_param_decls)

    for a in plan.wrapper_args:
        di = plan.dummies[a]
        lhs = di.decl_lhs
        if di.is_array:
            rank = len(di.dims)
            dims = ", ".join([":" for _ in range(rank)])
            shape_annot = ", ".join(di.dims)
            out.append(f"{lhs} :: {a}({dims}) ! ({shape_annot})\n")
        else:
            out.append(f"{lhs} :: {a}\n")

    if plan.proc_kind == "function" and plan.result_name and plan.result_decl_lhs:
        if plan.result_is_array and plan.result_dims:
            rdims: List[str] = []
            for d in plan.result_dims:
                key = d.strip().lower()
                if key in plan.removed_dim_args:
                    exprs = plan.bound_exprs.get(key, [])
                    rdims.append(exprs[0] if exprs else d)
                else:
                    rdims.append(d)
            out.append(f"{plan.result_decl_lhs} :: {plan.result_name}({', '.join(rdims)})\n")
        else:
            out.append(f"{plan.result_decl_lhs} :: {plan.result_name}\n")
    for b in plan.removed_dim_args:
        out.append(f"integer :: {b}\n")

    for b in plan.removed_dim_args:
        exprs = plan.bound_exprs.get(b, [])
        if not exprs:
            continue
        out.append(f"{b} = {exprs[0]}\n")
        for ex in exprs[1:]:
            out.append(f'if ({ex} /= {b}) error stop "{plan.wrapper_name}: inconsistent dimension {b}"\n')

    call_args: List[str] = []
    for a in plan.original_args:
        if a in plan.removed_dim_args:
            call_args.append(a)
        else:
            call_args.append(a)
    if plan.proc_kind == "function" and plan.result_name:
        out.append(f"{plan.result_name} = {plan.new_explicit_name}({', '.join(call_args)})\n")
        out.append(f"end function {plan.public_name}\n")
    else:
        out.append(f"call {plan.new_explicit_name}({', '.join(call_args)})\n")
        out.append(f"end subroutine {plan.public_name}\n")
    out.append("\n")
    return out


def interface_lines(plan: ProcWrapPlan) -> List[str]:
    return [
        f"interface {plan.proc_name}\n",
        f"   module procedure {plan.new_explicit_name}, {plan.wrapper_name}\n",
        "end interface\n",
    ]


def apply_plan(lines: List[str], plan: ProcWrapPlan, module_infos: List[ModuleInfo], plans_by_module: Dict[str, List[ProcWrapPlan]]) -> List[str]:
    new_lines = list(lines)

    # Rename original subroutine line.
    idx = plan.proc_start - 1
    pat = re.compile(rf"\bsubroutine\s+{re.escape(plan.proc_name)}\b", re.IGNORECASE)
    new_lines[idx] = pat.sub(f"subroutine {plan.new_explicit_name}", new_lines[idx], count=1)

    # Defer interface/wrapper insertion to aggregate pass.
    return new_lines


def apply_all(lines: List[str], plans: List[ProcWrapPlan], module_infos: List[ModuleInfo], *, replace: bool = False) -> List[str]:
    new_lines = list(lines)
    if replace:
        ordered = sorted(plans, key=lambda p: p.proc_start)
        shift = 0
        for p in ordered:
            p.proc_start += shift
            p.proc_end += shift
            old_end = p.proc_end
            ok = apply_replace_in_place(new_lines, p)
            if not ok:
                continue
            shift += (p.proc_end - old_end)
        return new_lines

    name_map: Dict[str, str] = {p.proc_name: p.new_explicit_name for p in plans}
    new_lines = rewrite_calls_to_explicit(new_lines, name_map)
    for p in plans:
        # Promote inferred dimension args to INTENT(IN) in the explicit-shape procedure declarations.
        if p.intent_promote_lines:
            for _nm, ln in p.intent_promote_lines.items():
                if 1 <= ln <= len(new_lines):
                    new_lines[ln - 1] = add_intent_in_to_decl_line(new_lines[ln - 1])
        idx = p.proc_start - 1
        kind_word = "function" if p.proc_kind == "function" else "subroutine"
        pat = re.compile(rf"\b{kind_word}\s+{re.escape(p.proc_name)}\b", re.IGNORECASE)
        if p.proc_kind == "function":
            line = new_lines[idx]
            code, comment = split_code_comment(line.rstrip("\n"))
            new_code = pat.sub(f"{kind_word} {p.new_explicit_name}", code, count=1)
            if not p.has_result_clause:
                new_code = f"{new_code} result({p.proc_name})"
            new_lines[idx] = new_code + comment + ("\n" if line.endswith("\n") else "")
        else:
            new_lines[idx] = pat.sub(f"{kind_word} {p.new_explicit_name}", new_lines[idx], count=1)
        end_pat = re.compile(rf"^(\s*end\s+{kind_word})\s+{re.escape(p.proc_name)}\b", re.IGNORECASE)
        for j in range(p.proc_end - 1, p.proc_start - 1, -1):
            code = fscan.strip_comment(new_lines[j]).strip()
            if end_pat.match(code):
                new_lines[j] = end_pat.sub(rf"\1 {p.new_explicit_name}", new_lines[j], count=1)
                break

    plans_by_module: Dict[str, List[ProcWrapPlan]] = {}
    for p in plans:
        plans_by_module.setdefault(p.module_name, []).append(p)

    inserts: Dict[int, List[str]] = {}

    for m in module_infos:
        mplans = plans_by_module.get(m.name, [])
        if not mplans:
            continue
        if m.contains_line is None:
            continue

        # Interface blocks before CONTAINS (skip in --replace mode).
        if not replace:
            iface: List[str] = []
            for p in mplans:
                iface.extend(interface_lines(p))
            inserts.setdefault(m.contains_line, []).extend(iface)

        # Place each wrapper immediately before the explicit-shape procedure it wraps.
        for p in mplans:
            wraps = wrapper_lines(p)
            need_leading_blank = True
            if p.proc_start > 1 and not new_lines[p.proc_start - 2].strip():
                need_leading_blank = False
            if wraps and wraps[0].strip() and need_leading_blank:
                wraps = ["\n"] + wraps
            inserts.setdefault(p.proc_start, []).extend(wraps)

    if not inserts:
        return new_lines

    out: List[str] = []
    for i, ln in enumerate(new_lines, start=1):
        if i in inserts:
            out.extend(inserts[i])
        out.append(ln)
    return out


def apply_all_replace(
    lines: List[str],
    plans: List[ProcWrapPlan],
) -> Tuple[List[str], List[ProcWrapPlan]]:
    new_lines = list(lines)
    ordered = sorted(plans, key=lambda p: p.proc_start)
    shift = 0
    applied: List[ProcWrapPlan] = []
    for p in ordered:
        p.proc_start += shift
        p.proc_end += shift
        old_end = p.proc_end
        ok = apply_replace_in_place(new_lines, p)
        if not ok:
            continue
        applied.append(p)
        shift += (p.proc_end - old_end)
    return new_lines, applied


def add_replace_notes(
    lines: List[str],
    module_infos: List[ModuleInfo],
    applied_plans: List[ProcWrapPlan],
) -> List[str]:
    if not applied_plans:
        return lines
    out = list(lines)
    by_mod: Dict[str, List[ProcWrapPlan]] = {}
    for p in applied_plans:
        by_mod.setdefault(p.module_name, []).append(p)

    # Process from bottom to top so line insertions/removals do not disturb
    # not-yet-processed module line numbers.
    mods = sorted(module_infos, key=lambda m: m.start, reverse=True)
    for m in mods:
        mplans = by_mod.get(m.name, [])
        if not mplans:
            continue
        mplans = sorted(mplans, key=lambda p: p.proc_start)
        names: List[str] = []
        seen: Set[str] = set()
        for p in mplans:
            if p.proc_name not in seen:
                seen.add(p.proc_name)
                names.append(p.proc_name)
        if not names:
            continue

        mod_lo = m.start - 1
        mod_hi = (m.end - 1) if (m.end - 1) < len(out) else len(out) - 1
        marker = "! xassumed_shape --replace modified "

        # Remove previous note block if present in this module.
        i = mod_lo
        while i <= mod_hi and i < len(out):
            s = out[i].strip().lower()
            if s.startswith(marker):
                j = i + 1
                while j <= mod_hi and j < len(out):
                    sj = out[j].strip()
                    if sj.startswith("!   "):
                        j += 1
                        continue
                    break
                del out[i:j]
                mod_hi -= (j - i)
                break
            i += 1

        insert_at = mod_lo + 1
        scan_hi = (m.contains_line - 1) if m.contains_line is not None else mod_hi
        if scan_hi > mod_hi:
            scan_hi = mod_hi
        for i in range(mod_lo, max(mod_lo, scan_hi) + 1):
            if i >= len(out):
                break
            code = fscan.strip_comment(out[i]).strip().lower()
            if code.startswith("implicit none"):
                insert_at = i + 1
                break

        lead = f"! xassumed_shape --replace modified {len(names)} procedures:"
        name_txt = ", ".join(names)
        wrapped = textwrap.wrap(name_txt, width=86, break_long_words=False, break_on_hyphens=False)
        note_lines = [lead + "\n"]
        if wrapped:
            note_lines.extend([f"!   {w}\n" for w in wrapped])
        out[insert_at:insert_at] = note_lines
    return out


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


def choose_files(args_files: List[Path], exclude: Iterable[str]) -> List[Path]:
    if args_files:
        files = cpaths.expand_path_args(args_files)
    else:
        files = sorted(set(Path('.').glob('*.f90')) | set(Path('.').glob('*.F90')), key=lambda p: p.name.lower())
    return fscan.apply_excludes(files, exclude)


def apply_fix(path: Path, new_text: str, *, out_path: Optional[Path], create_backup: bool) -> Tuple[int, Optional[Path]]:
    old_text = path.read_text(encoding='utf-8', errors='ignore')
    if old_text == new_text:
        if out_path is not None:
            out_path.write_text(old_text, encoding='utf-8')
        return 0, None
    backup: Optional[Path] = None
    target = out_path if out_path is not None else path
    if out_path is None and create_backup:
        backup = make_backup_path(path)
        shutil.copy2(path, backup)
    target.write_text(new_text, encoding='utf-8')
    return 1, backup


def load_codes(path: Path) -> List[List[Path]]:
    out: List[List[Path]] = []
    for raw in path.read_text(encoding='utf-8', errors='ignore').splitlines():
        line = raw.strip()
        if not line or line.startswith('#'):
            continue
        out.append([Path(tok) for tok in line.split()])
    return out


def resolve_code_entries(entries: List[List[Path]], code_dir: Optional[Path]) -> List[List[Path]]:
    if code_dir is None:
        return entries
    return [[p if p.is_absolute() else (code_dir / p) for p in g] for g in entries]


def main() -> int:
    ap = argparse.ArgumentParser(description='Suggest/fix wrappers for explicit-shape module procedures.')
    ap.add_argument('--codes', type=Path, help='Path to source list (first path per line is used).')
    ap.add_argument('--code-dir', type=Path, help='Base directory for relative paths in --codes.')
    ap.add_argument('files', nargs='*', type=Path)
    ap.add_argument('--exclude', action='append', default=[])
    ap.add_argument('--verbose', action='store_true')
    ap.add_argument('--fix', action='store_true')
    ap.add_argument('--out', type=Path)
    ap.add_argument('--out-dir', type=Path)
    ap.add_argument('--backup', dest='backup', action='store_true', default=True)
    ap.add_argument('--no-backup', dest='backup', action='store_false')
    ap.add_argument('--diff', action='store_true')
    ap.add_argument('--infer-intent-in', action='store_true', help='Infer scalar INTEGER dimension args as input if read-only when INTENT(IN) is missing.')
    ap.add_argument('--replace', action='store_true', help='Replace original procedure name with assumed-shape wrapper (skip generic interface).')
    ap.add_argument('--replace-note', action='store_true', help='In --replace mode, add a module-top comment listing modified procedures.')
    ap.add_argument('--compiler', type=str)
    args = ap.parse_args()

    if args.out is not None or args.out_dir is not None:
        args.fix = True
    if args.replace_note and not args.replace:
        print('--replace-note is only used with --replace.')
        return 2
    if args.out is not None and args.out_dir is not None:
        print('Use either --out or --out-dir, not both.')
        return 2

    if args.files:
        input_paths = args.files
        if args.code_dir is not None:
            input_paths = [p if p.is_absolute() else (args.code_dir / p) for p in input_paths]
        files = choose_files(input_paths, args.exclude)
    elif args.codes is not None:
        entries = resolve_code_entries(load_codes(args.codes), args.code_dir)
        firsts: List[Path] = []
        seen: Set[str] = set()
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
        print('--out supports exactly one input file.')
        return 2
    if args.out_dir is not None:
        if args.out_dir.exists() and not args.out_dir.is_dir():
            print('--out-dir exists but is not a directory.')
            return 2
        args.out_dir.mkdir(parents=True, exist_ok=True)

    if not files:
        print('No Fortran files found.')
        return 0

    if args.compiler:
        if not fbuild.run_compiler_command(args.compiler, files, 'baseline', fscan.display_path):
            return 1

    per_new: Dict[Path, str] = {}
    per_count: Dict[Path, int] = {}
    all_plans: List[Tuple[Path, ProcWrapPlan]] = []

    for p in files:
        text = p.read_text(encoding='utf-8', errors='ignore')
        lines = text.splitlines(keepends=True)
        mods = scan_modules(lines)
        procs = fscan.parse_procedures(lines)
        called_names = collect_called_names(lines)
        plans: List[ProcWrapPlan] = []
        for pr in procs:
            mod = module_for_proc(pr, mods)
            if mod is None:
                continue
            plan = build_wrap_plan(lines, pr, mod, infer_intent_in=args.infer_intent_in, replace=args.replace)
            if plan is not None:
                if args.replace and plan.proc_name in called_names:
                    continue
                plans.append(plan)
        applied_plans: List[ProcWrapPlan] = []
        if plans:
            if args.replace:
                new_lines, applied_plans = apply_all_replace(lines, plans)
                if args.replace_note:
                    new_lines = add_replace_notes(new_lines, scan_modules(new_lines), applied_plans)
            else:
                new_lines = apply_all(lines, plans, mods, replace=False)
                applied_plans = plans
        else:
            new_lines = lines
        per_new[p] = ''.join(new_lines)
        per_count[p] = len(applied_plans)
        for plan in applied_plans:
            all_plans.append((p, plan))

    if not all_plans:
        print('No explicit-shape wrapper candidates found.')
        if args.fix and args.out is not None:
            p = files[0]
            args.out.write_text(per_new[p], encoding='utf-8')
            print(f'Wrote unchanged output to {fscan.display_path(args.out)}')
        return 0

    if args.verbose:
        print(f'{len(all_plans)} wrapper candidate(s).')
        for p, pl in all_plans:
            print(f"{fscan.display_path(p)}:{pl.proc_start} {pl.proc_kind} {pl.proc_name}")
            if args.replace:
                print(f"  suggest: replace {pl.proc_name} in place with assumed-shape arguments")
            else:
                print(f"  suggest: interface {pl.proc_name} -> {pl.new_explicit_name}, {pl.wrapper_name}")
    else:
        for p, n in per_count.items():
            if n:
                print(f"{fscan.display_path(p)}: {n} candidate(s)")

    if not args.fix:
        return 0

    changed_files = 0
    total_wrapped = 0
    out_files: List[Path] = []
    for p in files:
        old = p.read_text(encoding='utf-8', errors='ignore')
        new = per_new[p]
        if args.diff and old != new:
            show_diff(old, new, p)
        out_path: Optional[Path] = None
        if args.out is not None and p == files[0]:
            out_path = args.out
        elif args.out_dir is not None:
            out_path = args.out_dir / p.name
        changed, backup = apply_fix(p, new, out_path=out_path, create_backup=args.backup)
        if out_path is not None:
            out_files.append(out_path)
        if changed:
            changed_files += 1
            total_wrapped += per_count.get(p, 0)
            if out_path is not None:
                print(f"Fixed {fscan.display_path(p)}: wrapped {per_count.get(p, 0)}, wrote {fscan.display_path(out_path)}")
            else:
                msg = f"Fixed {fscan.display_path(p)}: wrapped {per_count.get(p, 0)}"
                if backup is not None:
                    msg += f", backup {fscan.display_path(backup)}"
                print(msg)

    if args.compiler:
        if args.out is not None:
            comp_files = [args.out]
        elif args.out_dir is not None:
            comp_files = out_files if out_files else [args.out_dir / p.name for p in files]
        else:
            comp_files = files
        if not fbuild.run_compiler_command(args.compiler, comp_files, 'after-fix', fscan.display_path):
            return 1

    print(f"\n--fix summary: files changed {changed_files}, wrapped {total_wrapped}")
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
