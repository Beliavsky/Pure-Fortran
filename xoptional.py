#!/usr/bin/env python3
"""Advisory checker for OPTIONAL dummy arguments used without PRESENT guards."""

from __future__ import annotations

import argparse
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Set, Tuple

import cli_paths as cpaths
import fortran_scan as fscan

TYPE_DECL_RE = re.compile(
    r"^\s*(integer|real|logical|character|complex|double\s+precision|type\b|class\b|procedure\b)",
    re.IGNORECASE,
)
NO_COLON_DECL_RE = re.compile(
    r"^\s*(?P<spec>(?:integer|real|logical|complex|character|double\s+precision)\s*(?:\([^)]*\))?"
    r"|type\s*\([^)]*\)|class\s*\([^)]*\))\s+(?P<rhs>.+)$",
    re.IGNORECASE,
)
IDENT_RE = re.compile(r"\b([a-z][a-z0-9_]*)\b", re.IGNORECASE)
IF_THEN_RE = re.compile(r"^\s*if\s*\(.*\)\s*then\b", re.IGNORECASE)
ELSE_IF_RE = re.compile(r"^\s*else\s*if\s*\(.*\)\s*then\b", re.IGNORECASE)
ELSE_RE = re.compile(r"^\s*else\b", re.IGNORECASE)
END_IF_RE = re.compile(r"^\s*(end\s*if|endif)\b", re.IGNORECASE)
PRESENT_CALL_RE = re.compile(r"\bpresent\s*\(\s*([a-z][a-z0-9_]*)\s*\)", re.IGNORECASE)
PROC_HEADER_RE = re.compile(
    r"^\s*(?:(?:pure|elemental|impure|recursive|module)\s+)*(function|subroutine)\s+"
    r"([a-z][a-z0-9_]*)\s*\(([^)]*)\)",
    re.IGNORECASE,
)
CALL_RE = re.compile(r"^\s*call\s+([a-z][a-z0-9_]*)\s*(?:\((.*)\))?\s*$", re.IGNORECASE)


@dataclass
class Issue:
    path: Path
    proc_kind: str
    proc_name: str
    line: int
    arg: str
    certainty: str  # definite|possible
    detail: str


@dataclass
class ProcMeta:
    """Procedure metadata used for optional-argument forwarding checks."""

    ordered_dummies: List[str]
    optional_dummies: Set[str]


def choose_files(args_files: List[Path], exclude: Iterable[str]) -> List[Path]:
    """Resolve source files from args or current-directory defaults."""
    if args_files:
        files = cpaths.expand_path_args(args_files)
    else:
        files = sorted(set(Path(".").glob("*.f90")) | set(Path(".").glob("*.F90")), key=lambda p: p.name.lower())
    return fscan.apply_excludes(files, exclude)


def split_top_level_commas(text: str) -> List[str]:
    """Split one string by commas at top level."""
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
    """Strip trailing Fortran comment, respecting quoted strings."""
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


def parse_optional_decl_entities(stmt: str) -> Set[str]:
    """Return OPTIONAL entity names declared on one declaration statement."""
    code = stmt.strip()
    spec = ""
    rhs = ""
    if "::" in code:
        spec, rhs = code.split("::", 1)
    else:
        m = NO_COLON_DECL_RE.match(code)
        if not m:
            return set()
        spec = m.group("spec")
        rhs = m.group("rhs")
    if "optional" not in spec.lower():
        return set()
    out: Set[str] = set()
    for chunk in split_top_level_commas(rhs):
        m = re.match(r"^\s*([a-z][a-z0-9_]*)", chunk, re.IGNORECASE)
        if m:
            out.add(m.group(1).lower())
    return out


def build_proc_meta(finfo: fscan.SourceFileInfo) -> Dict[str, ProcMeta]:
    """Build per-procedure ordered dummies and optional dummy names for one file."""
    ordered_by_name: Dict[str, List[str]] = {}
    for _ln, stmt in fscan.iter_fortran_statements(finfo.parsed_lines):
        low = strip_comment(stmt).strip().lower()
        m = PROC_HEADER_RE.match(low)
        if not m:
            continue
        name = m.group(2).lower()
        args = [a.strip().lower() for a in split_top_level_commas(m.group(3) or "") if a.strip()]
        ordered_by_name[name] = args

    optional_by_name: Dict[str, Set[str]] = {}
    for p in finfo.procedures:
        opts: Set[str] = set()
        for _ln, stmt in p.body:
            low = strip_comment(stmt).strip().lower()
            if TYPE_DECL_RE.match(low):
                opts.update(parse_optional_decl_entities(low))
        optional_by_name[p.name.lower()] = opts

    meta: Dict[str, ProcMeta] = {}
    for p in finfo.procedures:
        name = p.name.lower()
        meta[name] = ProcMeta(
            ordered_dummies=ordered_by_name.get(name, []),
            optional_dummies=optional_by_name.get(name, set()),
        )
    return meta


def extract_if_condition(stmt: str) -> str:
    """Extract condition text from IF(... ) THEN / one-line IF."""
    s = stmt.strip()
    m = re.match(r"^if\s*\(", s, re.IGNORECASE)
    if not m:
        return ""
    i = m.end() - 1
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
                    return s[i + 1 : j].strip()
    return ""


def split_one_line_if(stmt: str) -> Optional[Tuple[str, str]]:
    """Split one-line IF into (cond, tail) or return None."""
    s = stmt.strip()
    if not s.lower().startswith("if"):
        return None
    cond = extract_if_condition(s)
    if not cond:
        return None
    # find the matching close parenthesis location again
    m = re.match(r"^if\s*\(", s, re.IGNORECASE)
    if not m:
        return None
    i = m.end() - 1
    depth = 0
    in_single = False
    in_double = False
    close = -1
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
                    close = j
                    break
    if close < 0:
        return None
    tail = s[close + 1 :].strip()
    if not tail or tail.lower().startswith("then"):
        return None
    return cond, tail


def top_level_split_op(expr: str, op: str) -> List[str]:
    """Split expression on top-level operator tokens like .and. / .or."""
    low = expr.lower()
    out: List[str] = []
    cur: List[str] = []
    depth = 0
    in_single = False
    in_double = False
    i = 0
    while i < len(expr):
        ch = expr[i]
        if ch == "'" and not in_double:
            in_single = not in_single
            cur.append(ch)
            i += 1
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            cur.append(ch)
            i += 1
            continue
        if not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")" and depth > 0:
                depth -= 1
            if depth == 0 and low.startswith(op, i):
                out.append("".join(cur).strip())
                cur = []
                i += len(op)
                continue
        cur.append(ch)
        i += 1
    tail = "".join(cur).strip()
    if tail:
        out.append(tail)
    return out


def peel_parens(s: str) -> str:
    """Strip one or more outer parenthesis layers."""
    t = s.strip()
    while t.startswith("(") and t.endswith(")"):
        depth = 0
        ok = True
        for i, ch in enumerate(t):
            if ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
                if depth == 0 and i != len(t) - 1:
                    ok = False
                    break
        if ok:
            t = t[1:-1].strip()
        else:
            break
    return t


def cond_present_sets(cond: str) -> Tuple[Set[str], Set[str], bool]:
    """Return (true_guards, false_guards, has_present_anywhere).

    Conservative: only .and. conjunctions of PRESENT/NOT PRESENT atoms produce guarantees.
    """
    cond_l = cond.lower()
    has_present = "present" in cond_l
    # .or. breaks certainty in this simple model.
    if ".or." in cond_l:
        return set(), set(), has_present

    true_set: Set[str] = set()
    false_set: Set[str] = set()
    for term in top_level_split_op(cond, ".and."):
        t = peel_parens(term).strip()
        if not t:
            continue
        m_pos = re.fullmatch(r"present\s*\(\s*([a-z][a-z0-9_]*)\s*\)", t, re.IGNORECASE)
        if m_pos:
            true_set.add(m_pos.group(1).lower())
            continue
        m_neg = re.fullmatch(r"\.not\.\s*present\s*\(\s*([a-z][a-z0-9_]*)\s*\)", t, re.IGNORECASE)
        if m_neg:
            false_set.add(m_neg.group(1).lower())
            continue
    return true_set, false_set, has_present


def present_args_in_expr(expr: str) -> Set[str]:
    """Return optional arg names referenced in PRESENT(arg) calls."""
    out: Set[str] = set()
    for m in PRESENT_CALL_RE.finditer(expr):
        out.add(m.group(1).lower())
    return out


def strip_present_calls(expr: str) -> str:
    """Remove PRESENT(arg) calls from expression text."""
    return PRESENT_CALL_RE.sub(" ", expr)


def extract_optional_uses(expr: str, optional_args: Set[str]) -> Set[str]:
    """Extract optional arg names used as values (excluding PRESENT(arg) calls)."""
    text = strip_present_calls(expr)
    out: Set[str] = set()
    for m in IDENT_RE.finditer(text.lower()):
        n = m.group(1).lower()
        if n in optional_args:
            out.add(n)
    return out


def call_forwarded_optional_safe_uses(
    stmt: str,
    opt_args: Set[str],
    proc_meta: Dict[str, ProcMeta],
) -> Set[str]:
    """Return optional arg names that are safely forwarded to OPTIONAL formals in CALLs."""
    m = CALL_RE.match(stmt.strip())
    if not m:
        return set()
    callee = m.group(1).lower()
    args_text = (m.group(2) or "").strip()
    if not args_text:
        return set()
    meta = proc_meta.get(callee)
    if meta is None:
        return set()

    actuals = split_top_level_commas(args_text)
    safe: Set[str] = set()
    pos = 0
    for raw in actuals:
        t = raw.strip()
        if not t:
            pos += 1
            continue
        formal = ""
        actual_expr = t
        if "=" in t and "=>" not in t:
            k, v = t.split("=", 1)
            formal = k.strip().lower()
            actual_expr = v.strip()
        else:
            if pos < len(meta.ordered_dummies):
                formal = meta.ordered_dummies[pos]
            pos += 1
        if not formal or formal not in meta.optional_dummies:
            continue
        for n in extract_optional_uses(actual_expr, opt_args):
            safe.add(n)
    return safe


def call_forwarded_optional_unknown_uses(
    stmt: str,
    opt_args: Set[str],
    proc_meta: Dict[str, ProcMeta],
) -> Set[str]:
    """Return optional arg names forwarded in CALLs whose callee signature is unknown."""
    m = CALL_RE.match(stmt.strip())
    if not m:
        return set()
    callee = m.group(1).lower()
    if callee in proc_meta:
        return set()
    args_text = (m.group(2) or "").strip()
    if not args_text:
        return set()
    out: Set[str] = set()
    for raw in split_top_level_commas(args_text):
        t = raw.strip()
        if not t:
            continue
        actual_expr = t
        if "=" in t and "=>" not in t:
            _k, v = t.split("=", 1)
            actual_expr = v.strip()
        out.update(extract_optional_uses(actual_expr, opt_args))
    return out


def analyze_procedure_optional_use(path: Path, proc: fscan.Procedure, proc_meta: Dict[str, ProcMeta]) -> List[Issue]:
    """Analyze one procedure for optional argument uses lacking PRESENT guards."""
    issues: List[Issue] = []
    opt_args: Set[str] = set()

    for ln, stmt in proc.body:
        low = strip_comment(stmt).strip().lower()
        if not low:
            continue
        if TYPE_DECL_RE.match(low):
            opt_args.update(parse_optional_decl_entities(low))

    if not opt_args:
        return issues

    guard_stack: List[Dict[str, object]] = []
    guard_present: Set[str] = set()

    for ln, stmt in proc.body:
        code = strip_comment(stmt).strip()
        low = code.lower()
        if not low:
            continue
        if TYPE_DECL_RE.match(low):
            continue

        one_if = split_one_line_if(code)
        if one_if is not None:
            cond, tail = one_if
            tset, _fset, has_present = cond_present_sets(cond)
            local_guard = set(guard_present) | (tset & opt_args)
            uses = extract_optional_uses(tail, opt_args)
            uses -= call_forwarded_optional_safe_uses(tail, opt_args, proc_meta)
            unknown_forward = call_forwarded_optional_unknown_uses(tail, opt_args, proc_meta)
            for a in sorted(uses):
                if a in local_guard:
                    continue
                if a in unknown_forward:
                    cert = "possible"
                    detail = (
                        "optional argument may be forwarded to external/unknown callee "
                        "without visible PRESENT() guarantee"
                    )
                elif has_present and a in present_args_in_expr(cond):
                    cert = "possible"
                    detail = (
                        "optional argument may be used without guaranteed PRESENT() guard "
                        "(condition may not ensure presence)"
                    )
                else:
                    cert = "definite"
                    detail = "optional argument may be used without guaranteed PRESENT() guard"
                issues.append(
                    Issue(
                        path=path,
                        proc_kind=proc.kind.lower(),
                        proc_name=proc.name.lower(),
                        line=ln,
                        arg=a,
                        certainty=cert,
                        detail=detail,
                    )
                )
            continue

        if IF_THEN_RE.match(low):
            cond = extract_if_condition(code)
            tset, _fset, _hasp = cond_present_sets(cond)
            pre = set(guard_present)
            guard_stack.append({"pre": pre})
            guard_present = pre | (tset & opt_args)
            continue

        if ELSE_IF_RE.match(low):
            if guard_stack:
                pre = set(guard_stack[-1]["pre"])  # type: ignore[index]
                cond = extract_if_condition(code)
                tset, _fset, _hasp = cond_present_sets(cond)
                guard_present = pre | (tset & opt_args)
            continue

        if ELSE_RE.match(low) and not ELSE_IF_RE.match(low):
            if guard_stack:
                pre = set(guard_stack[-1]["pre"])  # type: ignore[index]
                guard_present = pre
            continue

        if END_IF_RE.match(low):
            if guard_stack:
                frame = guard_stack.pop()
                guard_present = set(frame["pre"])  # type: ignore[index]
            continue

        uses = extract_optional_uses(code, opt_args)
        uses -= call_forwarded_optional_safe_uses(code, opt_args, proc_meta)
        unknown_forward = call_forwarded_optional_unknown_uses(code, opt_args, proc_meta)
        for a in sorted(uses):
            if a in guard_present:
                continue
            if a in unknown_forward:
                issues.append(
                    Issue(
                        path=path,
                        proc_kind=proc.kind.lower(),
                        proc_name=proc.name.lower(),
                        line=ln,
                        arg=a,
                        certainty="possible",
                        detail=(
                            "optional argument may be forwarded to external/unknown callee "
                            "without visible PRESENT() guarantee"
                        ),
                    )
                )
                continue
            issues.append(
                Issue(
                    path=path,
                    proc_kind=proc.kind.lower(),
                    proc_name=proc.name.lower(),
                    line=ln,
                    arg=a,
                    certainty="definite",
                    detail="optional argument may be used without guaranteed PRESENT() guard",
                )
            )

    return issues


def main() -> int:
    """Run optional-argument guard analysis and print findings."""
    parser = argparse.ArgumentParser(
        description="Advisory checker for OPTIONAL dummy arguments possibly used without PRESENT guards"
    )
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--verbose", action="store_true", help="Print all findings")
    args = parser.parse_args()

    files = choose_files(args.fortran_files, args.exclude)
    if not files:
        print("No source files remain after applying --exclude filters.")
        return 2

    infos, any_missing = fscan.load_source_files(files)
    if not infos:
        return 2 if any_missing else 1

    ordered, _ = fscan.order_files_least_dependent(infos)
    issues: List[Issue] = []
    for finfo in ordered:
        proc_meta = build_proc_meta(finfo)
        for p in finfo.procedures:
            issues.extend(analyze_procedure_optional_use(finfo.path, p, proc_meta))

    if not issues:
        print("No likely optional-argument guard findings.")
        return 0

    issues.sort(key=lambda x: (x.path.name.lower(), x.line, x.proc_kind, x.proc_name, x.arg))
    by_file: Dict[str, int] = {}
    n_def = 0
    n_poss = 0
    for i in issues:
        by_file[i.path.name] = by_file.get(i.path.name, 0) + 1
        if i.certainty == "definite":
            n_def += 1
        else:
            n_poss += 1

    print(
        f"{len(issues)} optional-argument guard finding(s) in {len(by_file)} file(s) "
        f"(definite {n_def}, possible {n_poss})."
    )

    if args.verbose:
        for i in issues:
            print(
                f"{i.path.name}:{i.line} {i.proc_kind} {i.proc_name} {i.arg} "
                f"[{i.certainty}] - {i.detail}"
            )
    else:
        for fname in sorted(by_file.keys(), key=str.lower):
            print(f"{fname}: {by_file[fname]}")
        first = issues[0]
        print(
            f"\nFirst finding: {first.path.name}:{first.line} {first.proc_kind} {first.proc_name} "
            f"{first.arg} [{first.certainty}] - {first.detail}"
        )
        print("Run with --verbose to list all findings.")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
