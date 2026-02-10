#!/usr/bin/env python3
"""
Compare two Fortran module source files.

modes
-----
1) two-file compare (default): compare file1 vs file2

2) one-file compare vs HEAD:
   if only one source file is provided, compare it to the last committed
   version at HEAD for that same repo path.

3) history mode:
   --history compares each committed version to the previously committed one
   (adjacent commits from `git log --follow`), and also compares working tree
   vs HEAD as the first entry.

procedure comparison
--------------------
default:
  - exact text compare (includes comments), ignoring only line-ending style

--semantic:
  - compare normalized statement streams:
      * join continuations
      * split semicolons
      * strip comments
      * lowercase
      * collapse whitespace

output
------
by default, sections with no matches are not printed

--diff-procs:
  - print unified diffs for common procedures reported as different

accessibility (before contains)
-------------------------------
compares:
  - default accessibility (public vs private)
  - explicit public/private lists
  - effective public api diffs (procedures, variables, parameters)

history output
--------------
--history prints a summary line per pair; use --history-names to include names.
"""

from __future__ import annotations

import argparse
import difflib
import re
import subprocess
import sys
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, List, Optional, Set, Tuple

import cli_paths as cpaths
import fortran_scan as fscan


# -------------------------
# regex
# -------------------------
module_start_re = re.compile(r"^\s*module\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
module_end_re = re.compile(r"^\s*end\s+module\b", re.IGNORECASE)
contains_re = re.compile(r"^\s*contains\b", re.IGNORECASE)

interface_start_re = re.compile(r"^\s*(abstract\s+)?interface\b", re.IGNORECASE)
interface_end_re = re.compile(r"^\s*end\s+interface\b", re.IGNORECASE)

typedef_start_re = re.compile(r"^\s*type\b(?!\s*\()", re.IGNORECASE)  # "type :: name", not "type(name)"
typedef_end_re = re.compile(r"^\s*end\s+type\b", re.IGNORECASE)

use_re = re.compile(
    r"^\s*use\b(?:\s*,\s*(?:non_intrinsic|intrinsic)\s*)?(?:\s*::\s*|\s+)([a-z][a-z0-9_]*)(.*)$",
    re.IGNORECASE,
)

proc_sub_re = re.compile(r"\bsubroutine\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
proc_fun_re = re.compile(r"\bfunction\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)

data_decl_start_re = re.compile(
    r"^\s*(integer|real|logical|complex|character|double\s+precision|type\s*\(|class\s*\()",
    re.IGNORECASE,
)

access_stmt_re = re.compile(r"^\s*(public|private)\b(.*)$", re.IGNORECASE)
decl_public_attr_re = re.compile(r"(?<![a-z])public(?![a-z])", re.IGNORECASE)
decl_private_attr_re = re.compile(r"(?<![a-z])private(?![a-z])", re.IGNORECASE)


# -------------------------
# dataclasses
# -------------------------
@dataclass
class module_block:
    name: str
    start_line: int
    end_line: int
    contains_line: Optional[int]


@dataclass
class use_info:
    used_modules: Set[str] = field(default_factory=set)
    wildcard_modules: Set[str] = field(default_factory=set)
    only_imports: Dict[str, Set[str]] = field(default_factory=dict)  # module -> local names
    renames: Dict[str, Dict[str, str]] = field(default_factory=dict)  # module -> {local: remote}


@dataclass
class proc_info:
    name: str
    kind: str
    start_line: int
    end_line: int
    lines: List[str]


@dataclass
class access_info:
    default_private: bool = False
    explicit_public: Set[str] = field(default_factory=set)
    explicit_private: Set[str] = field(default_factory=set)
    decl_public: Set[str] = field(default_factory=set)
    decl_private: Set[str] = field(default_factory=set)


@dataclass
class compare_result:
    module_name: str

    # raw objects (for diffs)
    procs1: Dict[str, List[proc_info]]
    procs2: Dict[str, List[proc_info]]

    # procedure name sets
    proc_names1: Set[str]
    proc_names2: Set[str]

    procs_only1: List[str]
    procs_only2: List[str]
    procs_changed: List[str]
    ambiguous: List[str]

    # module entities
    vars1: Set[str]
    vars2: Set[str]
    const1: Set[str]
    const2: Set[str]

    vars_only1: List[str]
    vars_only2: List[str]
    consts_only1: List[str]
    consts_only2: List[str]

    # use diffs
    uses1: use_info
    uses2: use_info
    use_mods_only1: List[str]
    use_mods_only2: List[str]
    use_only_diff_lines: List[str]

    # accessibility
    access1: access_info
    access2: access_info
    access_lines: List[str]

    # effective public api
    pub_procs1: Set[str]
    pub_procs2: Set[str]
    pub_vars1: Set[str]
    pub_vars2: Set[str]
    pub_consts1: Set[str]
    pub_consts2: Set[str]


# -------------------------
# basic helpers
# -------------------------
def read_lines_noeol_text(text: str) -> List[str]:
    lines = text.splitlines(False)
    if lines and lines[0].startswith("\ufeff"):
        lines[0] = lines[0][1:]
    return lines


def read_lines_noeol(path: Path) -> List[str]:
    return read_lines_noeol_text(path.read_text(encoding="utf-8", errors="ignore"))


def split_top_level_commas(text: str) -> List[str]:
    out: List[str] = []
    cur: List[str] = []
    depth = 0
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
            elif ch == "," and depth == 0:
                out.append("".join(cur).strip())
                cur = []
                continue
        cur.append(ch)
    tail = "".join(cur).strip()
    if tail:
        out.append(tail)
    return out


def print_list_section(title: str, items: List[str], indent: str = "  ") -> None:
    if not items:
        return
    print(title)
    for x in items:
        print(f"{indent}{x}")
    print()


# -------------------------
# module scanning
# -------------------------
def find_modules(lines: List[str]) -> List[module_block]:
    mods: List[module_block] = []
    i = 0
    while i < len(lines):
        code = fscan.strip_comment(lines[i]).strip()
        low = code.lower()
        m = module_start_re.match(low)
        if m:
            toks = low.split()
            if len(toks) >= 2 and toks[1] == "procedure":
                i += 1
                continue
            name = m.group(1).lower()
            start_line = i + 1
            contains_line: Optional[int] = None
            j = i + 1
            end_line = len(lines)
            while j < len(lines):
                c = fscan.strip_comment(lines[j]).strip()
                l = c.lower()
                if contains_line is None and contains_re.match(l):
                    contains_line = j + 1
                if module_end_re.match(l):
                    end_line = j + 1
                    break
                j += 1
            mods.append(module_block(name=name, start_line=start_line, end_line=end_line, contains_line=contains_line))
            i = end_line
            continue
        i += 1
    return mods


def select_module(
    mods1: List[module_block],
    mods2: List[module_block],
    requested: Optional[str],
) -> Tuple[module_block, module_block, str]:
    if requested:
        r = requested.lower()
        a = next((m for m in mods1 if m.name == r), None)
        b = next((m for m in mods2 if m.name == r), None)
        if a is None or b is None:
            raise SystemExit(f"requested module '{requested}' not found in both inputs")
        return a, b, r

    if len(mods1) == 1 and len(mods2) == 1:
        return mods1[0], mods2[0], mods1[0].name

    names1 = {m.name for m in mods1}
    names2 = {m.name for m in mods2}
    common = sorted(names1 & names2)
    if common:
        chosen = common[0]
        a = next(m for m in mods1 if m.name == chosen)
        b = next(m for m in mods2 if m.name == chosen)
        return a, b, chosen

    if not mods1 or not mods2:
        raise SystemExit("no module definitions found in one or both inputs")
    return mods1[0], mods2[0], mods1[0].name


def get_module_by_name(mods: List[module_block], name: str) -> Optional[module_block]:
    n = name.lower()
    return next((m for m in mods if m.name == n), None)


def iter_statements_with_offset(lines_slice: List[str], offset0: int) -> List[Tuple[int, str]]:
    out: List[Tuple[int, str]] = []
    for rel_lineno, stmt in fscan.iter_fortran_statements(lines_slice):
        out.append((offset0 + rel_lineno, stmt))
    return out


# -------------------------
# parse use
# -------------------------
def parse_use_info(lines: List[str], mod: module_block) -> use_info:
    ui = use_info()

    pre_end = (mod.contains_line - 1) if mod.contains_line else (mod.end_line - 1)
    start0 = mod.start_line
    if start0 >= pre_end:
        return ui

    slice0 = start0
    slice1 = pre_end
    stmts = iter_statements_with_offset(lines[slice0:slice1], offset0=slice0)

    interface_depth = 0
    typedef_depth = 0

    for _lineno, stmt in stmts:
        low = stmt.strip().lower()
        if not low:
            continue

        if interface_start_re.match(low):
            interface_depth += 1
            continue
        if interface_end_re.match(low):
            if interface_depth > 0:
                interface_depth -= 1
            continue

        if interface_depth == 0:
            if typedef_start_re.match(low) and "::" in low and not low.startswith(("type(", "type (", "type is")):
                typedef_depth += 1
                continue
            if typedef_end_re.match(low) and typedef_depth > 0:
                typedef_depth -= 1
                continue

        if interface_depth > 0 or typedef_depth > 0:
            continue

        m = use_re.match(low)
        if not m:
            continue

        modname = m.group(1).lower()
        rest = m.group(2) or ""
        ui.used_modules.add(modname)

        m_only = re.search(r"\bonly\s*:\s*(.*)$", rest, re.IGNORECASE)
        if not m_only:
            ui.wildcard_modules.add(modname)
            continue

        rhs = m_only.group(1).strip()
        ui.only_imports.setdefault(modname, set())
        ui.renames.setdefault(modname, {})

        if not rhs:
            continue

        for tok in split_top_level_commas(rhs):
            t = tok.strip()
            if not t:
                continue
            tl = t.lower()
            if tl.startswith(("operator(", "assignment(")):
                continue
            if "=>" in t:
                left, right = t.split("=>", 1)
                local = left.strip().lower()
                remote = right.strip().lower()
                if re.match(r"^[a-z][a-z0-9_]*$", local, re.IGNORECASE) and re.match(r"^[a-z][a-z0-9_]*$", remote, re.IGNORECASE):
                    ui.only_imports[modname].add(local)
                    ui.renames[modname][local] = remote
            else:
                name = t.strip().lower()
                if re.match(r"^[a-z][a-z0-9_]*$", name, re.IGNORECASE):
                    ui.only_imports[modname].add(name)

    return ui


# -------------------------
# parse module vars/params
# -------------------------
def parse_module_entities(lines: List[str], mod: module_block) -> Tuple[Set[str], Set[str]]:
    vars_set: Set[str] = set()
    const_set: Set[str] = set()

    pre_end = (mod.contains_line - 1) if mod.contains_line else (mod.end_line - 1)
    start0 = mod.start_line
    if start0 >= pre_end:
        return vars_set, const_set

    slice0 = start0
    slice1 = pre_end
    stmts = iter_statements_with_offset(lines[slice0:slice1], offset0=slice0)

    interface_depth = 0
    typedef_depth = 0

    for _lineno, stmt in stmts:
        low = stmt.strip().lower()
        if not low:
            continue

        if interface_start_re.match(low):
            interface_depth += 1
            continue
        if interface_end_re.match(low):
            if interface_depth > 0:
                interface_depth -= 1
            continue

        if interface_depth == 0:
            if typedef_start_re.match(low) and "::" in low and not low.startswith(("type(", "type (", "type is")):
                typedef_depth += 1
                continue
            if typedef_end_re.match(low) and typedef_depth > 0:
                typedef_depth -= 1
                continue

        if interface_depth > 0 or typedef_depth > 0:
            continue

        if "::" not in low:
            continue
        if not data_decl_start_re.match(low):
            continue
        if low.lstrip().startswith("procedure"):
            continue

        lhs, rhs = low.split("::", 1)
        is_parameter = re.search(r"(?<![a-z])parameter(?![a-z])", lhs, re.IGNORECASE) is not None

        for chunk in split_top_level_commas(rhs):
            text = chunk.strip()
            if not text:
                continue
            if "=>" in text:
                text = text.split("=>", 1)[0].strip()
            elif "=" in text:
                text = text.split("=", 1)[0].strip()
            m = re.match(r"^([a-z][a-z0-9_]*)", text, re.IGNORECASE)
            if not m:
                continue
            name = m.group(1).lower()
            if is_parameter:
                const_set.add(name)
            else:
                vars_set.add(name)

    return vars_set, const_set


# -------------------------
# parse access / public api
# -------------------------
def parse_access_stmt_names(code: str, keyword: str) -> Optional[List[str]]:
    m = access_stmt_re.match(code.strip())
    if not m or m.group(1).lower() != keyword.lower():
        return None
    rest = (m.group(2) or "").strip()
    if not rest:
        return []
    if rest.startswith("::"):
        rest = rest[2:].strip()
    elif rest.startswith(","):
        rest = rest[1:].strip()
    if not rest:
        return []
    names: List[str] = []
    for tok in split_top_level_commas(rest):
        t = tok.strip()
        if not t:
            continue
        if "=>" in t:
            t = t.split("=>", 1)[1].strip()
        m_name = re.match(r"^([a-z][a-z0-9_]*)$", t, re.IGNORECASE)
        if m_name:
            names.append(m_name.group(1).lower())
    return names


def parse_access_info(lines: List[str], mod: module_block) -> access_info:
    ai = access_info()

    pre_end = (mod.contains_line - 1) if mod.contains_line else (mod.end_line - 1)
    start0 = mod.start_line
    if start0 >= pre_end:
        return ai

    slice0 = start0
    slice1 = pre_end
    stmts = iter_statements_with_offset(lines[slice0:slice1], offset0=slice0)

    interface_depth = 0
    typedef_depth = 0

    for _lineno, stmt in stmts:
        low = stmt.strip().lower()
        if not low:
            continue

        if interface_start_re.match(low):
            interface_depth += 1
            continue
        if interface_end_re.match(low):
            if interface_depth > 0:
                interface_depth -= 1
            continue

        if interface_depth == 0:
            if typedef_start_re.match(low) and "::" in low and not low.startswith(("type(", "type (", "type is")):
                typedef_depth += 1
                continue
            if typedef_end_re.match(low) and typedef_depth > 0:
                typedef_depth -= 1
                continue

        if interface_depth > 0 or typedef_depth > 0:
            continue

        pub = parse_access_stmt_names(low, "public")
        if pub is not None:
            if not pub:
                ai.default_private = False
            else:
                ai.explicit_public.update(pub)
            continue

        prv = parse_access_stmt_names(low, "private")
        if prv is not None:
            if not prv:
                ai.default_private = True
            else:
                ai.explicit_private.update(prv)
            continue

        if "::" not in low:
            continue
        if not data_decl_start_re.match(low):
            continue
        if low.lstrip().startswith("procedure"):
            continue

        lhs, rhs = low.split("::", 1)
        has_pub = decl_public_attr_re.search(lhs) is not None
        has_prv = decl_private_attr_re.search(lhs) is not None
        if not has_pub and not has_prv:
            continue

        for chunk in split_top_level_commas(rhs):
            text = chunk.strip()
            if not text:
                continue
            if "=>" in text:
                text = text.split("=>", 1)[0].strip()
            elif "=" in text:
                text = text.split("=", 1)[0].strip()
            m_name = re.match(r"^([a-z][a-z0-9_]*)", text, re.IGNORECASE)
            if not m_name:
                continue
            name = m_name.group(1).lower()
            if has_prv:
                ai.decl_private.add(name)
            if has_pub:
                ai.decl_public.add(name)

    return ai


def is_public_name(name: str, ai: access_info) -> bool:
    n = name.lower()
    if n in ai.explicit_private:
        return False
    if n in ai.explicit_public:
        return True
    if n in ai.decl_private:
        return False
    if n in ai.decl_public:
        return True
    return not ai.default_private


# -------------------------
# procedures
# -------------------------
def is_proc_end_for_top(low: str, kind: str, name: str) -> bool:
    toks = low.split()
    if not toks or toks[0] != "end":
        return False
    if len(toks) == 1:
        return True
    if toks[1] in {"subroutine", "function"}:
        if toks[1] != kind:
            return False
        return True
    if toks[1] == name:
        return True
    return False


def extract_module_procedures(lines: List[str], mod: module_block) -> Dict[str, List[proc_info]]:
    out: Dict[str, List[proc_info]] = {}
    if not mod.contains_line:
        return out

    end0 = mod.end_line - 1
    s0 = mod.contains_line + 1
    e0 = end0
    if s0 > e0:
        return out

    slice0_0b = s0 - 1
    slice1_0b_excl = e0
    stmts = iter_statements_with_offset(lines[slice0_0b:slice1_0b_excl], offset0=slice0_0b)

    interface_depth = 0
    stack: List[Tuple[str, str, int, bool]] = []  # (kind, name, start_line, is_top_level)

    for lineno, stmt in stmts:
        low = stmt.strip().lower()
        if not low:
            continue

        if interface_start_re.match(low):
            interface_depth += 1
            continue
        if interface_end_re.match(low):
            if interface_depth > 0:
                interface_depth -= 1
            continue
        if interface_depth > 0:
            continue

        if not low.startswith("end"):
            msub = proc_sub_re.search(low)
            if msub:
                nm = msub.group(1).lower()
                is_top = len(stack) == 0
                stack.append(("subroutine", nm, lineno, is_top))
                continue
            mfun = proc_fun_re.search(low)
            if mfun:
                nm = mfun.group(1).lower()
                is_top = len(stack) == 0
                stack.append(("function", nm, lineno, is_top))
                continue

        if stack and low.startswith("end"):
            kind, nm, st, is_top = stack[-1]
            if is_proc_end_for_top(low, kind, nm):
                stack.pop()
                if is_top:
                    end_line = lineno
                    block_lines = lines[st - 1 : end_line]
                    pi = proc_info(name=nm, kind=kind, start_line=st, end_line=end_line, lines=block_lines)
                    out.setdefault(nm, []).append(pi)

    while stack:
        kind, nm, st, is_top = stack.pop()
        if is_top:
            end_line = mod.end_line - 1
            block_lines = lines[st - 1 : end_line]
            pi = proc_info(name=nm, kind=kind, start_line=st, end_line=end_line, lines=block_lines)
            out.setdefault(nm, []).append(pi)

    return out


def canonical_statements(proc_lines: List[str]) -> List[str]:
    out: List[str] = []
    for _lineno, stmt in fscan.iter_fortran_statements(proc_lines):
        code = fscan.strip_comment(stmt).strip()
        if not code:
            continue
        code = code.lower()
        code = " ".join(code.split())
        if code:
            out.append(code)
    return out


# -------------------------
# diff printing
# -------------------------
def print_unified_diff(
    name: str,
    a: proc_info,
    b: proc_info,
    label1: str,
    label2: str,
    n_context: int,
    max_lines: int,
) -> None:
    fromfile = f"{label1}::{name} (l{a.start_line}-l{a.end_line})"
    tofile = f"{label2}::{name} (l{b.start_line}-l{b.end_line})"

    diff_iter = difflib.unified_diff(
        a.lines,
        b.lines,
        fromfile=fromfile,
        tofile=tofile,
        n=n_context,
        lineterm="",
    )
    diff_lines = list(diff_iter)
    if not diff_lines:
        return

    print(f"diff for {name}:")
    if max_lines > 0 and len(diff_lines) > max_lines:
        for ln in diff_lines[:max_lines]:
            print(ln)
        print(f"... diff truncated ({len(diff_lines)} total lines, showing {max_lines})")
    else:
        for ln in diff_lines:
            print(ln)
    print()


# -------------------------
# core compare computation
# -------------------------
def compute_compare(
    lines1: List[str],
    lines2: List[str],
    module_name: str,
    mod1: module_block,
    mod2: module_block,
    semantic: bool,
) -> compare_result:
    uses1 = parse_use_info(lines1, mod1)
    uses2 = parse_use_info(lines2, mod2)

    vars1, const1 = parse_module_entities(lines1, mod1)
    vars2, const2 = parse_module_entities(lines2, mod2)

    access1 = parse_access_info(lines1, mod1)
    access2 = parse_access_info(lines2, mod2)

    procs1 = extract_module_procedures(lines1, mod1)
    procs2 = extract_module_procedures(lines2, mod2)

    proc_names1 = set(procs1.keys())
    proc_names2 = set(procs2.keys())

    procs_only1 = sorted(proc_names1 - proc_names2)
    procs_only2 = sorted(proc_names2 - proc_names1)
    common = sorted(proc_names1 & proc_names2)

    procs_changed: List[str] = []
    ambiguous: List[str] = []

    for nm in common:
        a = procs1.get(nm, [])
        b = procs2.get(nm, [])
        if len(a) != 1 or len(b) != 1:
            ambiguous.append(nm)
            continue

        if semantic:
            ca = canonical_statements(a[0].lines)
            cb = canonical_statements(b[0].lines)
            if ca != cb:
                procs_changed.append(nm)
        else:
            if "\n".join(a[0].lines) != "\n".join(b[0].lines):
                procs_changed.append(nm)

    vars_only1 = sorted(vars1 - vars2)
    vars_only2 = sorted(vars2 - vars1)
    consts_only1 = sorted(const1 - const2)
    consts_only2 = sorted(const2 - const1)

    use_mods_only1 = sorted(uses1.used_modules - uses2.used_modules)
    use_mods_only2 = sorted(uses2.used_modules - uses1.used_modules)

    use_only_diff_lines: List[str] = []
    common_used = sorted(uses1.used_modules & uses2.used_modules)
    for m in common_used:
        w1 = m in uses1.wildcard_modules
        w2 = m in uses2.wildcard_modules
        only1_set = uses1.only_imports.get(m, set())
        only2_set = uses2.only_imports.get(m, set())

        if w1 != w2:
            if w1 and not w2:
                use_only_diff_lines.append(f"  {m}: file1 uses wildcard 'use {m}', file2 uses only")
            elif w2 and not w1:
                use_only_diff_lines.append(f"  {m}: file2 uses wildcard 'use {m}', file1 uses only")

        if (not w1) and (not w2):
            a = sorted(only1_set - only2_set)
            b = sorted(only2_set - only1_set)
            if a or b:
                use_only_diff_lines.append(f"  {m}:")
                use_only_diff_lines.append(f"    only in file1: {', '.join(a) if a else '(none)'}")
                use_only_diff_lines.append(f"    only in file2: {', '.join(b) if b else '(none)'}")

            r1 = uses1.renames.get(m, {})
            r2 = uses2.renames.get(m, {})
            s1 = {f"{k}=>{v}" for k, v in r1.items()}
            s2 = {f"{k}=>{v}" for k, v in r2.items()}
            ra = sorted(s1 - s2)
            rb = sorted(s2 - s1)
            if ra or rb:
                use_only_diff_lines.append(f"  {m} renames:")
                use_only_diff_lines.append(f"    only in file1: {', '.join(ra) if ra else '(none)'}")
                use_only_diff_lines.append(f"    only in file2: {', '.join(rb) if rb else '(none)'}")

    pub_procs1 = {n for n in proc_names1 if is_public_name(n, access1)}
    pub_procs2 = {n for n in proc_names2 if is_public_name(n, access2)}
    pub_vars1 = {n for n in vars1 if is_public_name(n, access1)}
    pub_vars2 = {n for n in vars2 if is_public_name(n, access2)}
    pub_consts1 = {n for n in const1 if is_public_name(n, access1)}
    pub_consts2 = {n for n in const2 if is_public_name(n, access2)}

    access_lines: List[str] = []
    if access1.default_private != access2.default_private:
        d1 = "private" if access1.default_private else "public"
        d2 = "private" if access2.default_private else "public"
        access_lines.append(f"default accessibility differs: file1 default {d1}, file2 default {d2}")

    ep_only1 = sorted(access1.explicit_public - access2.explicit_public)
    ep_only2 = sorted(access2.explicit_public - access1.explicit_public)
    if ep_only1 or ep_only2:
        access_lines.append("explicit public access list differences:")
        if ep_only1:
            access_lines.append(f"  only in file1: {', '.join(ep_only1)}")
        if ep_only2:
            access_lines.append(f"  only in file2: {', '.join(ep_only2)}")

    er_only1 = sorted(access1.explicit_private - access2.explicit_private)
    er_only2 = sorted(access2.explicit_private - access1.explicit_private)
    if er_only1 or er_only2:
        access_lines.append("explicit private access list differences:")
        if er_only1:
            access_lines.append(f"  only in file1: {', '.join(er_only1)}")
        if er_only2:
            access_lines.append(f"  only in file2: {', '.join(er_only2)}")

    ppub_only1 = sorted(pub_procs1 - pub_procs2)
    ppub_only2 = sorted(pub_procs2 - pub_procs1)
    vpub_only1 = sorted(pub_vars1 - pub_vars2)
    vpub_only2 = sorted(pub_vars2 - pub_vars1)
    cpub_only1 = sorted(pub_consts1 - pub_consts2)
    cpub_only2 = sorted(pub_consts2 - pub_consts1)

    if ppub_only1 or ppub_only2 or vpub_only1 or vpub_only2 or cpub_only1 or cpub_only2:
        access_lines.append("effective public api differences:")
        if ppub_only1:
            access_lines.append(f"  public procedures only in file1: {', '.join(ppub_only1)}")
        if ppub_only2:
            access_lines.append(f"  public procedures only in file2: {', '.join(ppub_only2)}")
        if vpub_only1:
            access_lines.append(f"  public variables only in file1: {', '.join(vpub_only1)}")
        if vpub_only2:
            access_lines.append(f"  public variables only in file2: {', '.join(vpub_only2)}")
        if cpub_only1:
            access_lines.append(f"  public parameters only in file1: {', '.join(cpub_only1)}")
        if cpub_only2:
            access_lines.append(f"  public parameters only in file2: {', '.join(cpub_only2)}")

    return compare_result(
        module_name=module_name,
        procs1=procs1,
        procs2=procs2,
        proc_names1=proc_names1,
        proc_names2=proc_names2,
        procs_only1=procs_only1,
        procs_only2=procs_only2,
        procs_changed=sorted(procs_changed),
        ambiguous=sorted(ambiguous),
        vars1=vars1,
        vars2=vars2,
        const1=const1,
        const2=const2,
        vars_only1=vars_only1,
        vars_only2=vars_only2,
        consts_only1=consts_only1,
        consts_only2=consts_only2,
        uses1=uses1,
        uses2=uses2,
        use_mods_only1=use_mods_only1,
        use_mods_only2=use_mods_only2,
        use_only_diff_lines=use_only_diff_lines,
        access1=access1,
        access2=access2,
        access_lines=access_lines,
        pub_procs1=pub_procs1,
        pub_procs2=pub_procs2,
        pub_vars1=pub_vars1,
        pub_vars2=pub_vars2,
        pub_consts1=pub_consts1,
        pub_consts2=pub_consts2,
    )


def print_compare(
    res: compare_result,
    label1: str,
    label2: str,
    semantic: bool,
    diff_procs: bool,
    diff_context: int,
    diff_max_lines: int,
) -> int:
    rc = 0
    printed_any = False

    print(f"module compared: {res.module_name}")
    print(f"file1: {label1}")
    print(f"file2: {label2}")
    print()

    if res.procs_only1:
        print_list_section("procedures only in file1:", res.procs_only1)
        printed_any = True
        rc = 1
    if res.procs_only2:
        print_list_section("procedures only in file2:", res.procs_only2)
        printed_any = True
        rc = 1

    if res.procs_changed:
        printed_any = True
        rc = 1
        if semantic:
            print("procedures in both but semantically different (normalized statements differ):")
        else:
            print("procedures in both but different (exact text differs):")
        for nm in res.procs_changed:
            a = res.procs1[nm][0]
            b = res.procs2[nm][0]
            print(f"  {nm}  (file1: l{a.start_line}-l{a.end_line}, file2: l{b.start_line}-l{b.end_line})")
        print()

        if diff_procs:
            for nm in res.procs_changed:
                a = res.procs1[nm][0]
                b = res.procs2[nm][0]
                print_unified_diff(nm, a, b, label1, label2, diff_context, diff_max_lines)

    if res.ambiguous:
        printed_any = True
        rc = 1
        print("procedures with ambiguous duplicates (name appears multiple times in a file):")
        for nm in res.ambiguous:
            print(f"  {nm}")
            for pi in res.procs1.get(nm, []):
                print(f"    file1: {pi.kind} l{pi.start_line}-l{pi.end_line}")
            for pi in res.procs2.get(nm, []):
                print(f"    file2: {pi.kind} l{pi.start_line}-l{pi.end_line}")
        print()

    if res.vars_only1:
        print_list_section("module variables only in file1:", res.vars_only1)
        printed_any = True
        rc = 1
    if res.vars_only2:
        print_list_section("module variables only in file2:", res.vars_only2)
        printed_any = True
        rc = 1
    if res.consts_only1:
        print_list_section("module constants (parameter) only in file1:", res.consts_only1)
        printed_any = True
        rc = 1
    if res.consts_only2:
        print_list_section("module constants (parameter) only in file2:", res.consts_only2)
        printed_any = True
        rc = 1

    if res.use_mods_only1:
        print_list_section("use modules (before contains) only in file1:", res.use_mods_only1)
        printed_any = True
        rc = 1
    if res.use_mods_only2:
        print_list_section("use modules (before contains) only in file2:", res.use_mods_only2)
        printed_any = True
        rc = 1

    if res.use_only_diff_lines:
        printed_any = True
        rc = 1
        print("use, only imported-entity differences (before contains):")
        for ln in res.use_only_diff_lines:
            print(ln)
        print()

    if res.access_lines:
        printed_any = True
        rc = 1
        print("accessibility differences (before contains):")
        for ln in res.access_lines:
            print(ln)
        print()

    if not printed_any:
        print("no differences found")

    return rc


# -------------------------
# git helpers
# -------------------------
def run_git(repo_root: Path, args: List[str]) -> subprocess.CompletedProcess:
    return subprocess.run(
        ["git", "-C", str(repo_root)] + args,
        capture_output=True,
        text=True,
    )


def get_repo_root_for_path(p: Path) -> Path:
    probe_dir = p.parent if p.is_file() else p
    cp = subprocess.run(
        ["git", "-C", str(probe_dir), "rev-parse", "--show-toplevel"],
        capture_output=True,
        text=True,
    )
    if cp.returncode != 0:
        raise SystemExit(f"not a git repository (cannot find repo root for {p})")
    return Path(cp.stdout.strip())


def repo_relpath(repo_root: Path, p: Path) -> str:
    rp = p.resolve()
    try:
        rel = rp.relative_to(repo_root.resolve())
    except Exception:
        raise SystemExit(f"file is not inside repo root: {p}")
    return rel.as_posix()


def git_show_file_text(repo_root: Path, rev: str, relpath: str) -> str:
    cp = run_git(repo_root, ["show", f"{rev}:{relpath}"])
    if cp.returncode != 0:
        msg = cp.stderr.strip() or cp.stdout.strip()
        raise SystemExit(f"git show failed for {rev}:{relpath}\n{msg}")
    return cp.stdout


def git_log_commits(repo_root: Path, relpath: str, max_commits: int) -> List[str]:
    args = ["log", "--follow", "--format=%H"]
    if max_commits > 0:
        args += ["-n", str(max_commits)]
    args += ["--", relpath]
    cp = run_git(repo_root, args)
    if cp.returncode != 0:
        msg = cp.stderr.strip() or cp.stdout.strip()
        raise SystemExit(f"git log failed for {relpath}\n{msg}")
    commits = [x.strip() for x in cp.stdout.splitlines() if x.strip()]
    return commits


def git_commit_desc(repo_root: Path, commit: str) -> str:
    cp = run_git(repo_root, ["show", "-s", "--format=%h %ad %s", "--date=short", commit])
    if cp.returncode != 0:
        return commit[:12]
    return cp.stdout.strip()


# -------------------------
# history summary printing
# -------------------------
def history_summary_line(res: compare_result) -> str:
    added = len(res.procs_only1)   # newer only
    removed = len(res.procs_only2) # older only (since we treat file1=newer, file2=older)
    changed = len(res.procs_changed)

    v_added = len(res.vars_only1)
    v_removed = len(res.vars_only2)

    c_added = len(res.consts_only1)
    c_removed = len(res.consts_only2)

    u_added = len(res.use_mods_only1)
    u_removed = len(res.use_mods_only2)

    pubp_added = len(res.pub_procs1 - res.pub_procs2)
    pubp_removed = len(res.pub_procs2 - res.pub_procs1)

    pubv_added = len(res.pub_vars1 - res.pub_vars2)
    pubv_removed = len(res.pub_vars2 - res.pub_vars1)

    pubc_added = len(res.pub_consts1 - res.pub_consts2)
    pubc_removed = len(res.pub_consts2 - res.pub_consts1)

    acc = ""
    if res.access1.default_private != res.access2.default_private:
        acc = " default_access_changed"

    return (
        f"procs +{added} -{removed} ~{changed}; "
        f"vars +{v_added} -{v_removed}; "
        f"params +{c_added} -{c_removed}; "
        f"use +{u_added} -{u_removed}; "
        f"public(procs +{pubp_added} -{pubp_removed}, vars +{pubv_added} -{pubv_removed}, params +{pubc_added} -{pubc_removed})"
        f"{acc}"
    )


def history_names_block(res: compare_result) -> None:
    # lists only when non-empty
    if res.procs_only1:
        print(f"  procs added: {', '.join(res.procs_only1)}")
    if res.procs_only2:
        print(f"  procs removed: {', '.join(res.procs_only2)}")
    if res.procs_changed:
        print(f"  procs changed: {', '.join(res.procs_changed)}")

    pubp_added = sorted(res.pub_procs1 - res.pub_procs2)
    pubp_removed = sorted(res.pub_procs2 - res.pub_procs1)
    pubv_added = sorted(res.pub_vars1 - res.pub_vars2)
    pubv_removed = sorted(res.pub_vars2 - res.pub_vars1)
    pubc_added = sorted(res.pub_consts1 - res.pub_consts2)
    pubc_removed = sorted(res.pub_consts2 - res.pub_consts1)

    if pubp_added:
        print(f"  public procs added: {', '.join(pubp_added)}")
    if pubp_removed:
        print(f"  public procs removed: {', '.join(pubp_removed)}")
    if pubv_added:
        print(f"  public vars added: {', '.join(pubv_added)}")
    if pubv_removed:
        print(f"  public vars removed: {', '.join(pubv_removed)}")
    if pubc_added:
        print(f"  public params added: {', '.join(pubc_added)}")
    if pubc_removed:
        print(f"  public params removed: {', '.join(pubc_removed)}")

    if res.vars_only1:
        print(f"  vars added: {', '.join(res.vars_only1)}")
    if res.vars_only2:
        print(f"  vars removed: {', '.join(res.vars_only2)}")

    if res.consts_only1:
        print(f"  params added: {', '.join(res.consts_only1)}")
    if res.consts_only2:
        print(f"  params removed: {', '.join(res.consts_only2)}")

    if res.use_mods_only1:
        print(f"  use modules added: {', '.join(res.use_mods_only1)}")
    if res.use_mods_only2:
        print(f"  use modules removed: {', '.join(res.use_mods_only2)}")


# -------------------------
# drivers
# -------------------------
def compare_two_sources(
    label1: str,
    lines1: List[str],
    label2: str,
    lines2: List[str],
    requested_module: Optional[str],
    semantic: bool,
    diff_procs: bool,
    diff_context: int,
    diff_max_lines: int,
) -> int:
    mods1 = find_modules(lines1)
    mods2 = find_modules(lines2)
    mod1, mod2, modname = select_module(mods1, mods2, requested_module)

    res = compute_compare(lines1, lines2, modname, mod1, mod2, semantic)
    return print_compare(res, label1, label2, semantic, diff_procs, diff_context, diff_max_lines)


def history_mode(
    file_path: Path,
    requested_module: Optional[str],
    semantic: bool,
    history_names: bool,
    history_diff_procs: bool,
    diff_context: int,
    diff_max_lines: int,
    max_commits: int,
) -> int:
    if not file_path.exists():
        raise SystemExit(f"file not found: {file_path}")

    repo_root = get_repo_root_for_path(file_path)
    rel = repo_relpath(repo_root, file_path)

    work_lines = read_lines_noeol(file_path)
    head_lines = read_lines_noeol_text(git_show_file_text(repo_root, "HEAD", rel))

    # choose module name once (from worktree vs head), unless user forced it
    mods_work = find_modules(work_lines)
    mods_head = find_modules(head_lines)
    mod_work, mod_head, modname = select_module(mods_work, mods_head, requested_module)

    print(f"history for: {file_path}")
    print(f"repo path: {rel}")
    print(f"module: {modname}")
    print()

    # first: worktree vs head
    res_wh = compute_compare(work_lines, head_lines, modname, mod_work, mod_head, semantic)
    print(f"worktree vs HEAD: {history_summary_line(res_wh)}")
    if history_names:
        history_names_block(res_wh)
    if history_diff_procs and res_wh.procs_changed:
        # diffs for changed procs between worktree and head
        for nm in res_wh.procs_changed:
            a = res_wh.procs1[nm][0]
            b = res_wh.procs2[nm][0]
            print_unified_diff(nm, a, b, f"worktree:{rel}", f"HEAD:{rel}", diff_context, diff_max_lines)
    print()

    # now commit-to-previous-commit
    commits = git_log_commits(repo_root, rel, max_commits)
    if len(commits) < 2:
        print("no commit history pairs found for this file")
        return 0 if (not res_wh.procs_only1 and not res_wh.procs_only2 and not res_wh.procs_changed and not res_wh.vars_only1 and not res_wh.vars_only2 and not res_wh.consts_only1 and not res_wh.consts_only2 and not res_wh.use_mods_only1 and not res_wh.use_mods_only2 and not res_wh.use_only_diff_lines and not res_wh.access_lines) else 1

    rc = 1 if (
        res_wh.procs_only1 or res_wh.procs_only2 or res_wh.procs_changed or
        res_wh.vars_only1 or res_wh.vars_only2 or res_wh.consts_only1 or res_wh.consts_only2 or
        res_wh.use_mods_only1 or res_wh.use_mods_only2 or res_wh.use_only_diff_lines or res_wh.access_lines
    ) else 0

    # commits are newest->oldest; compare commit[i] (newer) vs commit[i+1] (older)
    for i in range(len(commits) - 1):
        newer = commits[i]
        older = commits[i + 1]

        newer_desc = git_commit_desc(repo_root, newer)
        older_desc = git_commit_desc(repo_root, older)

        newer_lines = read_lines_noeol_text(git_show_file_text(repo_root, newer, rel))
        older_lines = read_lines_noeol_text(git_show_file_text(repo_root, older, rel))

        mods_new = find_modules(newer_lines)
        mods_old = find_modules(older_lines)
        mn = get_module_by_name(mods_new, modname)
        mo = get_module_by_name(mods_old, modname)
        if mn is None or mo is None:
            print(f"{newer_desc} vs {older_desc}: module '{modname}' not found in one of the versions (skipping)")
            print()
            continue

        res = compute_compare(newer_lines, older_lines, modname, mn, mo, semantic)
        line = history_summary_line(res)
        print(f"{newer_desc} vs prev {older_desc}: {line}")
        if history_names:
            history_names_block(res)
        if history_diff_procs and res.procs_changed:
            for nm in res.procs_changed:
                a = res.procs1[nm][0]
                b = res.procs2[nm][0]
                print_unified_diff(nm, a, b, f"{newer_desc}:{rel}", f"{older_desc}:{rel}", diff_context, diff_max_lines)
        print()

        if (
            res.procs_only1 or res.procs_only2 or res.procs_changed or
            res.vars_only1 or res.vars_only2 or res.consts_only1 or res.consts_only2 or
            res.use_mods_only1 or res.use_mods_only2 or res.use_only_diff_lines or res.access_lines
        ):
            rc = 1

    return rc


def one_file_vs_head(
    file_path: Path,
    requested_module: Optional[str],
    semantic: bool,
    diff_procs: bool,
    diff_context: int,
    diff_max_lines: int,
) -> int:
    if not file_path.exists():
        raise SystemExit(f"file not found: {file_path}")

    repo_root = get_repo_root_for_path(file_path)
    rel = repo_relpath(repo_root, file_path)

    work_lines = read_lines_noeol(file_path)
    head_text = git_show_file_text(repo_root, "HEAD", rel)
    head_lines = read_lines_noeol_text(head_text)

    label1 = f"worktree:{file_path}"
    label2 = f"HEAD:{rel}"

    return compare_two_sources(
        label1, work_lines,
        label2, head_lines,
        requested_module,
        semantic,
        diff_procs,
        diff_context,
        diff_max_lines,
    )


# -------------------------
# main
# -------------------------
def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("files", nargs="+", type=Path, help="one file (compare vs HEAD) or two files (direct compare)")
    ap.add_argument("--module", default=None, help="module name to compare (optional)")

    ap.add_argument(
        "--semantic",
        action="store_true",
        help="compare common procedures by normalized statements (ignore comments/whitespace/case/continuations/;)",
    )

    ap.add_argument(
        "--diff-procs",
        action="store_true",
        help="print unified diffs for each common procedure reported as different (two-file or worktree-vs-head only)",
    )
    ap.add_argument("--diff-context", type=int, default=3, help="unified diff context lines (default 3)")
    ap.add_argument("--diff-max-lines", type=int, default=0, help="max diff lines to print per procedure (0 means no limit)")

    ap.add_argument(
        "--history",
        action="store_true",
        help="history mode: compare worktree vs HEAD, then each commit vs previous commit for this file",
    )
    ap.add_argument(
        "--max-commits",
        type=int,
        default=25,
        help="max commits to pull from git log for history mode (default 25, 0 means all)",
    )
    ap.add_argument(
        "--history-names",
        action="store_true",
        help="in history mode, also print the names of added/removed/changed items",
    )
    ap.add_argument(
        "--history-diff-procs",
        action="store_true",
        help="in history mode, also print unified diffs for changed procedures for each pair (can be large)",
    )

    args = ap.parse_args()
    args.files = cpaths.expand_path_args(args.files)

    if len(args.files) not in (1, 2):
        raise SystemExit("provide one file (compare vs HEAD) or two files (direct compare)")

    if len(args.files) == 2:
        if args.history:
            raise SystemExit("--history requires exactly one file argument")
        f1, f2 = args.files
        if not f1.exists():
            raise SystemExit(f"file not found: {f1}")
        if not f2.exists():
            raise SystemExit(f"file not found: {f2}")

        lines1 = read_lines_noeol(f1)
        lines2 = read_lines_noeol(f2)

        return compare_two_sources(
            str(f1), lines1,
            str(f2), lines2,
            args.module,
            args.semantic,
            args.diff_procs,
            args.diff_context,
            args.diff_max_lines,
        )

    # one file
    f = args.files[0]
    if args.history:
        return history_mode(
            f,
            args.module,
            args.semantic,
            args.history_names,
            args.history_diff_procs,
            args.diff_context,
            args.diff_max_lines,
            args.max_commits,
        )

    return one_file_vs_head(
        f,
        args.module,
        args.semantic,
        args.diff_procs,
        args.diff_context,
        args.diff_max_lines,
    )


if __name__ == "__main__":
    sys.exit(main())
