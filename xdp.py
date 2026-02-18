#!/usr/bin/env python3
"""Suggest/fix default real declarations and real literals to use dp kind."""

from __future__ import annotations

import argparse
import difflib
import re
import shutil
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Set, Tuple

import cli_paths as cpaths
import fortran_build as fbuild
import fortran_scan as fscan
import xunset

REAL_DECL_RE = re.compile(r"^(\s*)real(\s*)(.*)$", re.IGNORECASE)
REAL_FUNC_RE = re.compile(r"^\s*real\s+function\b", re.IGNORECASE)
REAL_LITERAL_RE = re.compile(
    r"(?<![A-Za-z0-9_])"
    r"((?:(?:\d+\.\d*|\.\d+|\d+\.)(?:[eEdDqQ][+-]?\d+)?|\d+[eEdDqQ][+-]?\d+))"
    r"(?![A-Za-z0-9_])"
    r"(?!_)"
)
MODULE_START_RE = re.compile(r"^\s*module\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
END_MODULE_RE = re.compile(r"^\s*end\s*module\b", re.IGNORECASE)
CONTAINS_RE = re.compile(r"^\s*contains\b", re.IGNORECASE)
PROGRAM_START_RE = re.compile(r"^\s*program\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
FORMAT_STMT_RE = re.compile(r"^\s*(?:\d+\s+)?format\s*\(", re.IGNORECASE)
CALL_LIKE_RE = re.compile(r"\b([a-z][a-z0-9_]*)\s*\(", re.IGNORECASE)
ETIME_CALL_RE = re.compile(r"\betime\s*\(\s*([a-z][a-z0-9_]*)\s*\)", re.IGNORECASE)

@dataclass
class Finding:
    """One rewrite candidate."""

    path: Path
    line: int
    original: str
    suggestion: str


@dataclass
class ModuleInfo:
    """One module block span in physical line numbers (1-based inclusive)."""

    name: str
    start: int
    end: int
    contains: Optional[int]
    has_dp: bool


@dataclass
class UnitInfo:
    """Procedure/program unit span for inserting local dp parameter when needed."""

    kind: str
    start: int
    end: int
    insert_line: int
    has_dp: bool
    has_dp_symbol: bool
    has_use_stmt: bool
    use_imports_dp: bool
    kind_name: str
    etime_keep_real4: Set[str]
    external_names: Set[str]


DECL_START_RE = re.compile(
    r"^\s*(?:integer|real|double\s+precision|complex|logical|character|type\s*\(|class\s*\()\b",
    re.IGNORECASE,
)


def has_nonparam_dp_symbol_decl(stmt: str) -> bool:
    """True when a declaration statement appears to declare symbol named dp."""
    low = stmt.strip().lower()
    if not DECL_START_RE.match(low):
        return False
    if is_kind_decl_line(stmt, "dp"):
        return False
    # Conservative check: if declaration contains standalone dp token.
    return re.search(r"\bdp\b", low) is not None


def choose_files(args_files: List[Path], exclude: Iterable[str]) -> List[Path]:
    """Resolve input sources; default to *.f90 in cwd."""
    if args_files:
        files = cpaths.expand_source_inputs(args_files)
    else:
        files = sorted(
            set(Path(".").glob("*.f90")) | set(Path(".").glob("*.F90")),
            key=lambda p: p.name.lower(),
        )
    return fscan.apply_excludes(files, exclude)


def make_backup_path(path: Path) -> Path:
    """Create non-overwriting backup path."""
    base = Path(str(path) + ".bak")
    if not base.exists():
        return base
    idx = 1
    while True:
        cand = Path(f"{path}.bak{idx}")
        if not cand.exists():
            return cand
        idx += 1


def split_code_comment(line: str) -> Tuple[str, str]:
    """Split code and trailing comment, respecting quoted strings."""
    in_single = False
    in_double = False
    i = 0
    while i < len(line):
        ch = line[i]
        if ch == "'" and not in_double:
            if in_single and i + 1 < len(line) and line[i + 1] == "'":
                i += 2
                continue
            in_single = not in_single
            i += 1
            continue
        if ch == '"' and not in_single:
            if in_double and i + 1 < len(line) and line[i + 1] == '"':
                i += 2
                continue
            in_double = not in_double
            i += 1
            continue
        if ch == "!" and not in_single and not in_double:
            return line[:i], line[i:]
        i += 1
    return line, ""


def is_kind_decl_line(code: str, kind_name: str = "dp") -> bool:
    """Whether this line declares kind_name parameter itself."""
    low = code.lower()
    if "parameter" not in low:
        return False
    if kind_name.lower() not in low:
        return False
    if "kind(" in low and re.search(rf"\b{re.escape(kind_name.lower())}\b", low):
        return True
    return False


def scan_modules(lines: List[str]) -> List[ModuleInfo]:
    """Scan module spans and whether they already declare dp parameter."""
    mods: List[ModuleInfo] = []
    stack: List[Dict[str, object]] = []
    for lineno, stmt in fscan.iter_fortran_statements(lines):
        low = stmt.strip().lower()
        m = MODULE_START_RE.match(low)
        if m:
            toks = low.split()
            if len(toks) >= 2 and toks[1] != "procedure":
                stack.append(
                    {
                        "name": m.group(1).lower(),
                        "start": lineno,
                        "contains": None,
                        "has_dp": False,
                    }
                )
            continue
        if not stack:
            continue
        top = stack[-1]
        if CONTAINS_RE.match(low):
            if top["contains"] is None:
                top["contains"] = lineno
            continue
        if END_MODULE_RE.match(low):
            top2 = stack.pop()
            mods.append(
                ModuleInfo(
                    name=str(top2["name"]),
                    start=int(top2["start"]),
                    end=lineno,
                    contains=(None if top2["contains"] is None else int(top2["contains"])),
                    has_dp=bool(top2["has_dp"]),
                )
            )
            continue
        if low == "end":
            # Legacy style may close module with bare END.
            top2 = stack.pop()
            mods.append(
                ModuleInfo(
                    name=str(top2["name"]),
                    start=int(top2["start"]),
                    end=lineno,
                    contains=(None if top2["contains"] is None else int(top2["contains"])),
                    has_dp=bool(top2["has_dp"]),
                )
            )
            continue
        # Detect dp only in module specification part (before CONTAINS).
        if top["contains"] is None and is_kind_decl_line(stmt, "dp"):
            top["has_dp"] = True
    return mods


def module_index_for_lines(lines: List[str]) -> Dict[int, int]:
    """Map physical line number -> module index from scan_modules()."""
    mods = scan_modules(lines)
    idx: Dict[int, int] = {}
    for mi, m in enumerate(mods):
        for ln in range(m.start, m.end + 1):
            idx[ln] = mi
    return idx


def module_insert_line(lines: List[str], mod: ModuleInfo) -> int:
    """Compute where to insert module-level dp declaration (1-based line)."""
    limit = mod.contains if mod.contains is not None else mod.end
    ins = mod.start + 1
    for ln, stmt in fscan.iter_fortran_statements(lines):
        if ln <= mod.start:
            continue
        if ln >= limit:
            break
        low = stmt.strip().lower()
        if low.startswith("use ") or low.startswith("use,") or low.startswith("implicit"):
            ins = ln + 1
            continue
        break
    return ins


def is_rewrite_eligible_file(lines: List[str]) -> bool:
    """True if file defines an explicit PROGRAM or at least one MODULE."""
    for _ln, stmt in fscan.iter_fortran_statements(lines):
        low = stmt.strip().lower()
        if PROGRAM_START_RE.match(low):
            return True
        m = MODULE_START_RE.match(low)
        if m:
            toks = low.split()
            if len(toks) >= 2 and toks[1] != "procedure":
                return True
    return False


def split_top_level_commas(text: str) -> List[str]:
    """Split by top-level commas only."""
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


def declared_entity_names_from_real_decl(code: str) -> Set[str]:
    """Best-effort extraction of declared entity names from a REAL declaration line."""
    out: Set[str] = set()
    m = REAL_DECL_RE.match(code)
    if not m:
        return out
    rest = m.group(3).strip()
    if not rest:
        return out
    if "::" in rest:
        ent_text = rest.split("::", 1)[1].strip()
    else:
        ent_text = rest
    for part in split_top_level_commas(ent_text):
        p = part.strip()
        if not p:
            continue
        lhs = p.split("=", 1)[0].strip()
        mm = re.match(r"^([a-z][a-z0-9_]*)\b", lhs, re.IGNORECASE)
        if mm:
            out.add(mm.group(1).lower())
    return out


def external_names_from_stmt(stmt: str) -> Set[str]:
    """Extract names listed on an EXTERNAL declaration statement."""
    s = stmt.strip()
    m = re.match(r"(?i)^external\b(.*)$", s)
    if not m:
        return set()
    tail = m.group(1).strip()
    if tail.startswith("::"):
        tail = tail[2:].strip()
    out: Set[str] = set()
    for part in split_top_level_commas(tail):
        p = part.strip()
        mm = re.match(r"^([a-z][a-z0-9_]*)\b", p, re.IGNORECASE)
        if mm:
            out.add(mm.group(1).lower())
    return out


def rewrite_real_intrinsic_calls(code: str, kind_name: str) -> Tuple[str, int]:
    """Rewrite real(expr) calls to include kind=<kind_name> outside strings."""
    out: List[str] = []
    i = 0
    n = len(code)
    changes = 0
    in_single = False
    in_double = False
    while i < n:
        ch = code[i]
        if ch == "'" and not in_double:
            if in_single and i + 1 < n and code[i + 1] == "'":
                out.append("''")
                i += 2
                continue
            in_single = not in_single
            out.append(ch)
            i += 1
            continue
        if ch == '"' and not in_single:
            if in_double and i + 1 < n and code[i + 1] == '"':
                out.append('""')
                i += 2
                continue
            in_double = not in_double
            out.append(ch)
            i += 1
            continue
        if in_single or in_double:
            out.append(ch)
            i += 1
            continue

        # Match token REAL followed by optional whitespace and '('.
        if code[i : i + 4].lower() == "real":
            prev = code[i - 1] if i > 0 else ""
            if prev and (prev.isalnum() or prev == "_"):
                out.append(ch)
                i += 1
                continue
            j = i + 4
            while j < n and code[j].isspace():
                j += 1
            if j >= n or code[j] != "(":
                out.append(ch)
                i += 1
                continue
            # Parse matching paren.
            k = j + 1
            depth = 1
            s_in = False
            d_in = False
            while k < n and depth > 0:
                c2 = code[k]
                if c2 == "'" and not d_in:
                    if s_in and k + 1 < n and code[k + 1] == "'":
                        k += 2
                        continue
                    s_in = not s_in
                    k += 1
                    continue
                if c2 == '"' and not s_in:
                    if d_in and k + 1 < n and code[k + 1] == '"':
                        k += 2
                        continue
                    d_in = not d_in
                    k += 1
                    continue
                if not s_in and not d_in:
                    if c2 == "(":
                        depth += 1
                    elif c2 == ")":
                        depth -= 1
                k += 1
            if depth != 0:
                out.append(ch)
                i += 1
                continue
            inner = code[j + 1 : k - 1]
            parts = split_top_level_commas(inner)
            if not parts:
                out.append(code[i:k])
                i = k
                continue
            done_kind = False
            for idx, p in enumerate(parts):
                if re.match(r"^\s*kind\s*=", p, re.IGNORECASE):
                    parts[idx] = f"kind={kind_name}"
                    done_kind = True
            if not done_kind:
                if len(parts) == 1:
                    parts.append(f"kind={kind_name}")
                elif len(parts) == 2:
                    # REAL(A, KIND) positional form: normalize second arg.
                    parts[1] = f"kind={kind_name}"
                else:
                    out.append(code[i:k])
                    i = k
                    continue
            out.append(code[i:j + 1] + ", ".join(parts) + ")")
            changes += 1
            i = k
            continue

        out.append(ch)
        i += 1
    return "".join(out), changes


def rewrite_default_real_decl(
    code: str, kind_name: str, keep_real4: Optional[Set[str]] = None
) -> Tuple[str, bool]:
    """Rewrite default REAL declaration head to REAL(kind=<kind_name>)."""
    keep = keep_real4 or set()
    if REAL_FUNC_RE.match(code):
        return code, False
    m = REAL_DECL_RE.match(code)
    if not m:
        return code, False
    names = declared_entity_names_from_real_decl(code)
    if names and any(n in keep for n in names):
        return code, False
    indent, _ws, rest = m.group(1), m.group(2), m.group(3)
    r = rest.lstrip()
    if not r:
        return f"{indent}real(kind={kind_name})", True
    # Skip explicit-kind/oldstyle forms and typed function headers.
    if r.startswith("(") or r.startswith("*"):
        return code, False
    if re.match(r"^function\b", r, re.IGNORECASE):
        return code, False
    if "::" in r:
        new_code = f"{indent}real(kind={kind_name}) {r}"
    else:
        # No-colon declaration: add :: when rewriting.
        rr = r.strip()
        if rr.startswith(","):
            # Try to split known attribute list from entity list.
            m2 = re.match(
                r"^("
                r"(?:\s*,\s*(?:dimension\s*\([^)]*\)|intent\s*\([^)]*\)|save|parameter|"
                r"allocatable|pointer|target|optional|value|volatile|asynchronous|external|"
                r"intrinsic|public|private|contiguous))*"
                r")\s*(.+)$",
                rr,
                re.IGNORECASE,
            )
            if m2 and m2.group(2):
                attrs = m2.group(1)
                ents = m2.group(2).strip()
                new_code = f"{indent}real(kind={kind_name}){attrs} :: {ents}"
            else:
                new_code = f"{indent}real(kind={kind_name}) :: {rr.lstrip(',').strip()}"
        else:
            new_code = f"{indent}real(kind={kind_name}) :: {rr}"
    # Normalize spacing before :: and commas where possible.
    new_code = re.sub(r"\s+::", " ::", new_code)
    new_code = re.sub(r"\s+,", ",", new_code)
    return new_code, (new_code != code)


def _rewrite_literals_outside_strings(code: str, kind_name: str) -> Tuple[str, int]:
    """Rewrite real literals to _<kind_name> outside quoted strings."""

    def repl(m: re.Match[str]) -> str:
        tok = m.group(1)
        tok2 = re.sub(r"[dDqQ]", "e", tok)
        mz = re.match(r"^(.+)[eE]([+-]?\d+)$", tok2)
        if mz:
            try:
                if int(mz.group(2)) == 0:
                    tok2 = mz.group(1)
            except ValueError:
                pass
        return tok2 + f"_{kind_name}"

    out: List[str] = []
    cur: List[str] = []
    in_single = False
    in_double = False
    changes = 0

    def flush_cur() -> None:
        nonlocal changes
        if not cur:
            return
        chunk = "".join(cur)
        new_chunk, n = REAL_LITERAL_RE.subn(repl, chunk)
        out.append(new_chunk)
        changes += n
        cur.clear()

    i = 0
    while i < len(code):
        ch = code[i]
        if ch == "'" and not in_double:
            if in_single and i + 1 < len(code) and code[i + 1] == "'":
                out.append("''")
                i += 2
                continue
            if not in_single:
                flush_cur()
            in_single = not in_single
            out.append(ch)
            i += 1
            continue
        if ch == '"' and not in_single:
            if in_double and i + 1 < len(code) and code[i + 1] == '"':
                out.append('""')
                i += 2
                continue
            if not in_double:
                flush_cur()
            in_double = not in_double
            out.append(ch)
            i += 1
            continue
        if in_single or in_double:
            out.append(ch)
        else:
            cur.append(ch)
        i += 1

    flush_cur()
    return "".join(out), changes


def rewrite_code_line(
    code: str, kind_name: str, keep_real4: Optional[Set[str]] = None
) -> Tuple[str, bool]:
    """Apply declaration and literal rewrites to one code fragment."""
    m_decl = REAL_DECL_RE.match(code)
    if m_decl:
        rdecl = m_decl.group(3).lstrip()
        # Explicit-kind/type declaration forms like REAL(PR) / REAL*8 are
        # declarations, not intrinsic real(...) calls; leave them untouched.
        if rdecl.startswith("(") or rdecl.startswith("*"):
            return code, False
    c1, dchg = rewrite_default_real_decl(code, kind_name, keep_real4)
    if FORMAT_STMT_RE.match(c1.strip()):
        return c1, dchg
    if is_kind_decl_line(c1, kind_name):
        return c1, dchg
    c2, rcg = rewrite_real_intrinsic_calls(c1, kind_name)
    # Keep literals inside KIND(...) expressions unchanged; converting
    # kind(1.0d0) -> kind(1.0_dp) can require dp before it is declared.
    if re.search(r"(?i)\bkind\s*\(", c2):
        return c2, (dchg or rcg > 0)
    c3, lchg = _rewrite_literals_outside_strings(c2, kind_name)
    return c3, (dchg or rcg > 0 or lchg > 0)


def statement_span(lines: List[str], start_idx: int) -> Tuple[int, int]:
    """Return [start, end) physical-line span for a continued free-form statement."""
    n = len(lines)
    if start_idx < 0 or start_idx >= n:
        return start_idx, start_idx
    i = start_idx
    pending = True
    saw_code = False
    while i < n and pending:
        raw = lines[i].rstrip("\r\n")
        code, _ = split_code_comment(raw)
        s = code.strip()
        if s:
            saw_code = True
            pending = code.rstrip().endswith("&")
        else:
            pending = saw_code and pending
        i += 1
        if not pending:
            break
    return start_idx, i


def format_statement_line_mask(lines: List[str]) -> Set[int]:
    """Return physical line numbers that belong to FORMAT statements."""
    mask: Set[int] = set()
    for ln, stmt in fscan.iter_fortran_statements(lines):
        if not FORMAT_STMT_RE.match(stmt.strip()):
            continue
        s0, s1 = statement_span(lines, ln - 1)
        for i in range(s0, s1):
            mask.add(i + 1)
    return mask


def interface_statement_line_mask(lines: List[str]) -> Set[int]:
    """Return physical line numbers that belong to INTERFACE blocks."""
    mask: Set[int] = set()
    depth = 0
    for ln, stmt in fscan.iter_fortran_statements(lines):
        low = stmt.strip().lower()
        starts = low.startswith("interface")
        ends = low.startswith("end interface")
        s0, s1 = statement_span(lines, ln - 1)
        if starts:
            depth += 1
        if depth > 0:
            for i in range(s0, s1):
                mask.add(i + 1)
        if ends and depth > 0:
            depth -= 1
    return mask


def explicit_real_decl_statement_mask(lines: List[str]) -> Set[int]:
    """Return physical line numbers that belong to explicit REAL decl statements."""
    mask: Set[int] = set()
    for ln, stmt in fscan.iter_fortran_statements(lines):
        s = stmt.strip()
        m = REAL_DECL_RE.match(s)
        if not m:
            continue
        rest = m.group(3).lstrip()
        if not (rest.startswith("(") or rest.startswith("*")):
            continue
        s0, s1 = statement_span(lines, ln - 1)
        for i in range(s0, s1):
            mask.add(i + 1)
    return mask


def unit_index_for_lines(path: Path) -> Dict[int, int]:
    """Map physical line -> unit index for procedures/program units in file."""
    infos, any_missing = fscan.load_source_files([path])
    if not infos or any_missing:
        return {}
    finfo = infos[0]
    units = xunset.collect_units(finfo)
    out: Dict[int, int] = {}
    for ui, u in enumerate(units):
        for ln in range(u.start, u.end + 1):
            out[ln] = ui
    return out


def scan_units(path: Path) -> List[UnitInfo]:
    """Collect unit info for dp insertion outside modules."""
    infos, any_missing = fscan.load_source_files([path])
    if not infos or any_missing:
        return []
    finfo = infos[0]
    units = xunset.collect_units(finfo)
    out: List[UnitInfo] = []
    for u in units:
        has_dp = any(is_kind_decl_line(stmt, "dp") for _ln, stmt in u.body)
        has_dp_symbol = any(has_nonparam_dp_symbol_decl(stmt) for _ln, stmt in u.body)
        use_stmts = [
            stmt.strip().lower()
            for _ln, stmt in u.body
            if stmt.strip().lower().startswith("use ") or stmt.strip().lower().startswith("use,")
        ]
        has_use_stmt = bool(use_stmts)
        # Heuristic: treat dp as imported only when it is named in USE text.
        use_imports_dp = any(re.search(r"\bdp\b", s) for s in use_stmts)
        etime_keep_real4: Set[str] = set()
        external_names: Set[str] = set()
        for _ln, stmt in u.body:
            for m_etime in ETIME_CALL_RE.finditer(stmt):
                etime_keep_real4.add("etime")
                etime_keep_real4.add(m_etime.group(1).lower())
            external_names.update(external_names_from_stmt(stmt))
        kind_name = "dp"
        if has_dp_symbol and not has_dp:
            candidate = "dp__"
            blob = "\n".join(stmt.lower() for _ln, stmt in u.body)
            while re.search(rf"\b{re.escape(candidate.lower())}\b", blob):
                candidate += "_"
            kind_name = candidate
        insert_line = u.start + 1
        if u.body:
            insert_line = u.body[0][0]
            if u.kind.lower() == "program":
                # For main programs, prefer inserting immediately after the
                # leading IMPLICIT NONE (after any initial USE lines).
                for ln, stmt in u.body:
                    low = stmt.strip().lower()
                    if low.startswith("use ") or low.startswith("use,"):
                        insert_line = ln + 1
                        continue
                    if re.match(r"^implicit\s+none\b", low):
                        insert_line = ln + 1
                    break
            else:
                for ln, stmt in u.body:
                    low = stmt.strip().lower()
                    if low.startswith("use ") or low.startswith("use,") or low.startswith("implicit"):
                        insert_line = ln + 1
                        continue
                    break
        out.append(
            UnitInfo(
                kind=u.kind.lower(),
                start=u.start,
                end=u.end,
                insert_line=insert_line,
                has_dp=has_dp,
                has_dp_symbol=has_dp_symbol,
                has_use_stmt=has_use_stmt,
                use_imports_dp=use_imports_dp,
                kind_name=kind_name,
                etime_keep_real4=etime_keep_real4,
                external_names=external_names,
            )
        )
    return out


def analyze_file(path: Path) -> List[Finding]:
    """Collect rewrite findings in one file."""
    lines = path.read_text(encoding="utf-8", errors="ignore").splitlines(keepends=True)
    if not is_rewrite_eligible_file(lines):
        return []
    fmt_mask = format_statement_line_mask(lines)
    iface_mask = interface_statement_line_mask(lines)
    explicit_real_decl_mask = explicit_real_decl_statement_mask(lines)
    line_to_mod = module_index_for_lines(lines)
    units = scan_units(path)
    line_to_unit = unit_index_for_lines(path)
    findings: List[Finding] = []
    for i, raw in enumerate(lines):
        if (i + 1) in iface_mask:
            continue
        if (i + 1) in explicit_real_decl_mask:
            continue
        kind_name = "dp"
        keep_real4: Set[str] = set()
        ui = line_to_unit.get(i + 1)
        if ui is None:
            continue
        if ui is not None and 0 <= ui < len(units):
            u = units[ui]
            if u.kind in {"subroutine", "function"} and (u.start not in line_to_mod):
                continue
            if u.external_names:
                continue
            kind_name = u.kind_name
            keep_real4 = set(u.etime_keep_real4)
            keep_real4.update(u.external_names)
        body = raw.rstrip("\r\n")
        eol = raw[len(body) :]
        code, comment = split_code_comment(body)
        if (i + 1) in fmt_mask:
            c1, dchg = rewrite_default_real_decl(code, kind_name, keep_real4)
            new_code, changed = c1, dchg
        else:
            new_code, changed = rewrite_code_line(code, kind_name, keep_real4)
        if not changed:
            continue
        findings.append(
            Finding(
                path=path,
                line=i + 1,
                original=code.rstrip(),
                suggestion=(new_code + comment + eol).rstrip("\r\n"),
            )
        )
    return findings


def annotate_file(path: Path, findings: List[Finding], *, backup: bool = True) -> Tuple[int, Optional[Path]]:
    """Insert suggestion comments after matching lines."""
    if not findings:
        return 0, None
    lines = path.read_text(encoding="utf-8", errors="ignore").splitlines(keepends=True)
    inserts: List[Tuple[int, str]] = []
    by_line: Dict[int, Finding] = {f.line: f for f in findings}
    for ln, f in by_line.items():
        idx = ln - 1
        if idx < 0 or idx >= len(lines):
            continue
        raw = lines[idx]
        indent = re.match(r"^\s*", raw).group(0) if raw else ""
        eol = "\r\n" if raw.endswith("\r\n") else ("\n" if raw.endswith("\n") else "\n")
        msg = f"{indent}! {f.suggestion}  !! suggested by xdp.py{eol}"
        nxt = idx + 1
        if 0 <= nxt < len(lines) and lines[nxt].strip().lower() == msg.strip().lower():
            continue
        inserts.append((idx + 1, msg))

    if not inserts:
        return 0, None
    backup_path: Optional[Path] = None
    if backup:
        backup_path = make_backup_path(path)
        shutil.copy2(path, backup_path)
    for at, msg in sorted(inserts, key=lambda x: x[0], reverse=True):
        lines.insert(at, msg)
    path.write_text("".join(lines), encoding="utf-8")
    return len(inserts), backup_path


def apply_fix_file(
    path: Path,
    findings: List[Finding],
    *,
    annotate: bool = False,
    out_path: Optional[Path] = None,
    create_backup: bool = True,
    dp_expr: str = "kind(1.0d0)",
) -> Tuple[int, Optional[Path]]:
    """Apply rewrites in-place or to out_path."""
    if not findings:
        return 0, None
    by_line: Dict[int, Finding] = {f.line: f for f in findings}
    lines = path.read_text(encoding="utf-8", errors="ignore").splitlines(keepends=True)
    modules = scan_modules(lines)
    line_to_mod = module_index_for_lines(lines)
    units = scan_units(path)
    line_to_unit = unit_index_for_lines(path)
    fmt_mask = format_statement_line_mask(lines)
    iface_mask = interface_statement_line_mask(lines)
    explicit_real_decl_mask = explicit_real_decl_statement_mask(lines)
    touched_mods: Set[int] = set()
    touched_units: Set[int] = set()
    changed = 0
    for i, raw in enumerate(lines):
        if (i + 1) in iface_mask:
            continue
        if (i + 1) in explicit_real_decl_mask:
            continue
        kind_name = "dp"
        keep_real4: Set[str] = set()
        ui = line_to_unit.get(i + 1)
        if ui is None:
            continue
        if ui is not None and 0 <= ui < len(units):
            u = units[ui]
            if u.kind in {"subroutine", "function"} and (u.start not in line_to_mod):
                continue
            if u.external_names:
                continue
            kind_name = u.kind_name
            keep_real4 = set(u.etime_keep_real4)
            keep_real4.update(u.external_names)
        f = by_line.get(i + 1)
        if f is None:
            continue
        body = raw.rstrip("\r\n")
        eol = raw[len(body) :]
        code, comment = split_code_comment(body)
        if (i + 1) in fmt_mask:
            c1, dchg = rewrite_default_real_decl(code, kind_name, keep_real4)
            new_code, ok = c1, dchg
        else:
            new_code, ok = rewrite_code_line(code, kind_name, keep_real4)
        if not ok:
            continue
        suffix = "  !! changed by xdp.py" if annotate else ""
        trailing = f" {comment.strip()}" if comment.strip() else ""
        lines[i] = f"{new_code}{suffix}{trailing}{eol}"
        changed += 1
        mi = line_to_mod.get(i + 1)
        if mi is not None:
            touched_mods.add(mi)
        ui = line_to_unit.get(i + 1)
        if ui is not None:
            touched_units.add(ui)

    if changed > 0:
        # Insert dp declaration in touched scopes that do not already define it.
        inserts: List[Tuple[int, str]] = []
        # 1) Module scope declarations.
        for mi in sorted(touched_mods):
            if mi < 0 or mi >= len(modules):
                continue
            m = modules[mi]
            if m.has_dp:
                continue
            insert_at = module_insert_line(lines, m) - 1
            if insert_at < 0:
                insert_at = 0
            if insert_at >= len(lines):
                insert_at = len(lines)
            # Indent one level inside module.
            mod_line = lines[m.start - 1] if 0 <= (m.start - 1) < len(lines) else ""
            base_indent = re.match(r"^\s*", mod_line).group(0) if mod_line else ""
            eol = "\n"
            if 0 <= insert_at < len(lines):
                ref = lines[insert_at]
                if ref.endswith("\r\n"):
                    eol = "\r\n"
                elif ref.endswith("\n"):
                    eol = "\n"
            dp_line = f"{base_indent}  integer, parameter :: dp = {dp_expr}{eol}"
            inserts.append((insert_at, dp_line))
            changed += 1
        # 2) Non-module procedure/program units.
        for ui in sorted(touched_units):
            if ui < 0 or ui >= len(units):
                continue
            u = units[ui]
            if u.has_dp:
                continue
            # Prefer imported dp from USE-associated modules when present.
            if u.use_imports_dp and u.kind_name == "dp":
                continue
            # Skip units inside module ranges: module-level dp suffices.
            if u.start in line_to_mod and u.kind_name == "dp":
                continue
            insert_at = max(0, min(len(lines), u.insert_line - 1))
            ref_line_no = u.insert_line
            base_indent = ""
            if 0 <= (ref_line_no - 1) < len(lines):
                base_indent = re.match(r"^\s*", lines[ref_line_no - 1]).group(0)
            eol = "\n"
            if 0 <= insert_at < len(lines):
                ref = lines[insert_at]
                if ref.endswith("\r\n"):
                    eol = "\r\n"
                elif ref.endswith("\n"):
                    eol = "\n"
            dp_line = f"{base_indent}integer, parameter :: {u.kind_name} = {dp_expr}{eol}"
            inserts.append((insert_at, dp_line))
            changed += 1
        for at, text in sorted(inserts, key=lambda x: x[0], reverse=True):
            lines.insert(at, text)

    if changed == 0:
        return 0, None
    backup: Optional[Path] = None
    target = out_path if out_path is not None else path
    if out_path is None and create_backup:
        backup = make_backup_path(path)
        shutil.copy2(path, backup)
    target.write_text("".join(lines), encoding="utf-8")
    return changed, backup


def main() -> int:
    """CLI entrypoint."""
    parser = argparse.ArgumentParser(description="Suggest/fix default real declarations and real literals to dp")
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--verbose", action="store_true", help="Print suggested rewritten lines")
    parser.add_argument("--fix", action="store_true", help="Rewrite default real declarations/literals to dp")
    parser.add_argument("--out", type=Path, help="With --fix, write transformed output to this file (single input)")
    parser.add_argument("--out-dir", type=Path, help="With --fix, write transformed outputs to this directory")
    parser.add_argument("--backup", dest="backup", action="store_true", default=True)
    parser.add_argument("--no-backup", dest="backup", action="store_false")
    parser.add_argument("--annotate", action="store_true", help="Insert suggestion comments (or changed tags with --fix)")
    parser.add_argument("--diff", action="store_true", help="With --fix, print unified diffs for changed files")
    parser.add_argument("--compiler", type=str, help="Compile command for baseline/after-fix validation")
    parser.add_argument("--tee", action="store_true")
    parser.add_argument("--tee-both", action="store_true")
    parser.add_argument("--run", action="store_true")
    parser.add_argument("--run-both", action="store_true")
    parser.add_argument("--run-diff", action="store_true")
    parser.add_argument("--quiet-run", action="store_true")
    parser.add_argument("--keep-exe", action="store_true")
    parser.add_argument("--dp", type=str, default="kind(1.0d0)", help="Kind expression for inserted kind constants (default: kind(1.0d0))")
    parser.add_argument("--limit", type=int, help="Maximum number of source files to process")
    args = parser.parse_args()

    if args.limit is not None and args.limit < 1:
        print("--limit must be >= 1.")
        return 2
    if args.run_diff:
        args.run_both = True
    if args.run_both:
        args.run = True
    if args.tee_both:
        args.tee = True
    if args.out is not None or args.out_dir is not None:
        args.fix = True
    if args.run and args.out is None and args.out_dir is None:
        args.out = Path("temp.f90")
    if args.out is not None and args.out_dir is not None:
        print("Use only one of --out or --out-dir.")
        return 2
    if args.diff and not args.fix:
        print("--diff requires --fix.")
        return 2
    if args.compiler and not args.fix:
        print("--compiler requires --fix.")
        return 2
    if args.tee and args.out is None and args.out_dir is None:
        print("--tee requires --out or --out-dir.")
        return 2
    if not args.dp or not args.dp.strip():
        print("--dp must be a non-empty expression, e.g. --dp \"kind(1.0d0)\".")
        return 2

    files = choose_files(args.fortran_files, args.exclude)
    if args.limit is not None:
        files = files[: args.limit]
    if not files:
        print("No source files remain after applying --exclude filters.")
        return 2
    if args.out is not None and len(files) != 1:
        print("--out requires exactly one input source file.")
        return 2
    if args.out_dir is not None:
        if args.out_dir.exists() and not args.out_dir.is_dir():
            print("--out-dir exists but is not a directory.")
            return 2
        args.out_dir.mkdir(parents=True, exist_ok=True)
    if args.run and len(files) != 1:
        print("--run/--run-both/--run-diff require exactly one input source file.")
        return 2

    if args.fix and args.out is not None:
        compile_paths = [args.out]
    elif args.fix and args.out_dir is not None:
        compile_paths = [args.out_dir / p.name for p in files]
    else:
        compile_paths = files
    if args.fix and args.compiler:
        if not fbuild.run_compiler_command(args.compiler, files, "baseline", fscan.display_path):
            return 5

    findings: List[Finding] = []
    by_file: Dict[Path, List[Finding]] = {}
    for p in files:
        fnds = analyze_file(p)
        if not fnds:
            continue
        by_file[p] = fnds
        findings.extend(fnds)
    orig_out = ""
    orig_err = ""
    xform_out = ""
    xform_err = ""
    transformed_changed = False
    transformed_target: Optional[Path] = None

    if not findings:
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
        if args.out is not None or args.out_dir is not None:
            src = files[0]
            target = args.out if args.out is not None else (args.out_dir / src.name)
            if src.resolve() != target.resolve():
                shutil.copy2(src, target)
            if args.tee:
                txt = target.read_text(encoding="utf-8", errors="ignore")
                if args.tee_both:
                    print(f"--- original: {src} ---")
                    print(src.read_text(encoding="utf-8", errors="ignore"), end="")
                    if not txt.endswith("\n"):
                        print("")
                    print(f"--- transformed: {target} ---")
                print(txt, end="")
                if not txt.endswith("\n"):
                    print("")
            print(f"No dp rewrite candidates found. Wrote unchanged output to {target}")
            return 0
        print("No dp rewrite candidates found.")
        return 0

    findings.sort(key=lambda f: (f.path.name.lower(), f.line))
    print(f"{len(findings)} dp rewrite candidate(s).")
    if args.verbose:
        for f in findings:
            print(f"{f.path.name}:{f.line}")
            print(f"  suggest: {f.suggestion}")
    else:
        by_name: Dict[str, int] = {}
        for f in findings:
            by_name[f.path.name] = by_name.get(f.path.name, 0) + 1
        for fn in sorted(by_name.keys(), key=str.lower):
            print(f"{fn}: {by_name[fn]}")

    if args.fix:
        touched = 0
        total = 0
        for p in sorted(by_file.keys(), key=lambda x: x.name.lower()):
            before = p.read_text(encoding="utf-8", errors="ignore")
            out_path = args.out if args.out is not None else (args.out_dir / p.name if args.out_dir is not None else None)
            if out_path is not None and args.tee_both:
                print(f"--- original: {p} ---")
                print(before, end="")
                if not before.endswith("\n"):
                    print("")
            n, backup = apply_fix_file(
                p,
                by_file[p],
                annotate=args.annotate,
                out_path=out_path,
                create_backup=args.backup,
                dp_expr=args.dp.strip(),
            )
            total += n
            if n > 0:
                transformed_changed = True
                touched += 1
                transformed_target = out_path if out_path is not None else p
                if out_path is not None and args.tee:
                    txt = out_path.read_text(encoding="utf-8", errors="ignore")
                    print(f"--- transformed: {out_path} ---")
                    print(txt, end="")
                    if not txt.endswith("\n"):
                        print("")
                if out_path is not None:
                    print(f"\nFixed {p.name}: replaced {n}, wrote {out_path}")
                else:
                    print(f"\nFixed {p.name}: replaced {n}, backup {backup.name if backup else '(none)'}")
                if args.diff:
                    after = (out_path if out_path is not None else p).read_text(encoding="utf-8", errors="ignore")
                    diff_lines = difflib.unified_diff(
                        before.splitlines(),
                        after.splitlines(),
                        fromfile=f"a/{p.name}",
                        tofile=f"b/{(out_path.name if out_path is not None else p.name)}",
                        lineterm="",
                    )
                    print("")
                    for dl in diff_lines:
                        print(dl)
            elif args.verbose:
                print(f"\nNo fixes applied in {p.name}")
            elif out_path is not None and args.tee:
                txt = out_path.read_text(encoding="utf-8", errors="ignore")
                if args.tee_both:
                    print(f"--- transformed: {out_path} ---")
                print(txt, end="")
                if not txt.endswith("\n"):
                    print("")
        print(f"\n--fix summary: files changed {touched}, replaced {total}")
    elif args.annotate:
        touched = 0
        total = 0
        for p in sorted(by_file.keys(), key=lambda x: x.name.lower()):
            n, backup = annotate_file(p, by_file[p], backup=args.backup)
            total += n
            if n > 0:
                touched += 1
                print(f"\nAnnotated {p.name}: inserted {n}, backup {backup.name if backup else '(none)'}")
            elif args.verbose:
                print(f"\nNo annotations inserted in {p.name}")
        print(f"\n--annotate summary: files changed {touched}, inserted {total}")

    if args.fix and args.compiler:
        if not fbuild.run_compiler_command(args.compiler, compile_paths, "after-fix", fscan.display_path):
            return 5
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
