#!/usr/bin/env python3
"""Advisory detector for potentially redundant repeated calculations in Fortran."""

from __future__ import annotations

import argparse
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Set, Tuple

import cli_paths as cpaths
import fortran_scan as fscan

ASSIGN_RE = re.compile(r"^\s*([a-z][a-z0-9_]*)\s*(?:\([^)]*\))?\s*=", re.IGNORECASE)
PRINT_WRITE_RE = re.compile(r"^\s*(print|write)\b", re.IGNORECASE)
CONTROL_BARRIER_RE = re.compile(
    r"^\s*(if\b|else\b|elseif\b|end\s+if\b|do\b|end\s+do\b|select\b|case\b|where\b|end\s+where\b|forall\b|"
    r"contains\b|call\b|goto\b|\d+\s+continue\b)",
    re.IGNORECASE,
)
UNIT_START_RE = re.compile(
    r"^\s*(?:(?:pure|elemental|impure|recursive|module)\s+)*(function|subroutine)\b|^\s*(program|module)\b",
    re.IGNORECASE,
)
UNIT_END_RE = re.compile(r"^\s*end(?:\s+(function|subroutine|program|module))?\b", re.IGNORECASE)
CALL_LIKE_RE = re.compile(r"\b([a-z][a-z0-9_]*)\s*\(", re.IGNORECASE)
IDENT_RE = re.compile(r"\b([a-z][a-z0-9_]*)\b", re.IGNORECASE)
NON_EXPR_IF_RE = re.compile(r"^\s*if\s*\(.*\)\s*then\b", re.IGNORECASE)
LITERAL_WORDS = {"true", "false", "null"}
INTRINSIC_SAFE = {
    "abs",
    "acos",
    "asin",
    "atan",
    "atan2",
    "cos",
    "cosh",
    "exp",
    "log",
    "log10",
    "max",
    "min",
    "mod",
    "nint",
    "real",
    "sin",
    "sinh",
    "sqrt",
    "sum",
    "tan",
    "tanh",
}


@dataclass
class Finding:
    """One advisory finding."""

    path: Path
    rule: str
    line1: int
    line2: int
    expr: str
    confidence: str
    detail: str


@dataclass
class ExprSeen:
    """State for one seen expression in a basic block."""

    line: int
    raw_expr: str
    refs: Set[str]
    confidence: str
    has_user_call: bool
    assigned_since: Set[str]


def choose_files(args_files: List[Path], exclude: Iterable[str]) -> List[Path]:
    """Resolve source files from args or current-directory defaults."""
    if args_files:
        files = cpaths.expand_path_args(args_files)
    else:
        files = sorted(
            set(Path(".").glob("*.f90")) | set(Path(".").glob("*.F90")),
            key=lambda p: p.name.lower(),
        )
    return fscan.apply_excludes(files, exclude)


def split_top_level_commas(text: str) -> List[str]:
    """Split text on top-level commas only."""
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


def remove_ws_outside_quotes(text: str) -> str:
    """Remove whitespace outside quotes."""
    out: List[str] = []
    in_single = False
    in_double = False
    for ch in text:
        if ch == "'" and not in_double:
            in_single = not in_single
            out.append(ch)
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            out.append(ch)
            continue
        if ch.isspace() and not in_single and not in_double:
            continue
        out.append(ch)
    return "".join(out)


def normalize_expr(expr: str) -> str:
    """Normalize expression text for matching."""
    return remove_ws_outside_quotes(expr.strip().lower())


def extract_if_condition(stmt: str) -> Optional[str]:
    """Extract IF(condition) text from an IF statement."""
    s = stmt.strip()
    low = s.lower()
    if not low.startswith("if"):
        return None
    pos = s.find("(")
    if pos < 0:
        return None
    depth = 0
    in_single = False
    in_double = False
    end_pos = -1
    for i in range(pos, len(s)):
        ch = s[i]
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
                    end_pos = i
                    break
    if end_pos < 0:
        return None
    return s[pos + 1 : end_pos]


def expr_refs(expr: str) -> Set[str]:
    """Extract identifier-like references from expression."""
    text = normalize_expr(expr)
    implied_do_vars: Set[str] = set(m.group(1).lower() for m in re.finditer(r"(?:\(|,)\s*([a-z][a-z0-9_]*)\s*=", text))
    refs: Set[str] = set()
    for m in IDENT_RE.finditer(text):
        n = m.group(1).lower()
        if n not in LITERAL_WORDS and n not in implied_do_vars:
            refs.add(n)
    return refs


def call_profile(expr: str) -> Tuple[bool, bool]:
    """Return (has_any_call_like, has_user_call_like)."""
    any_call = False
    user_call = False
    text = normalize_expr(expr)
    for m in CALL_LIKE_RE.finditer(text):
        any_call = True
        name = m.group(1).lower()
        if name not in INTRINSIC_SAFE:
            user_call = True
    return any_call, user_call


def is_literal_only_expr(expr: str) -> bool:
    """Return True when expression appears to be literal-only (no identifiers/calls)."""
    text = normalize_expr(expr)
    # Strip quoted strings.
    out: List[str] = []
    in_single = False
    in_double = False
    for ch in text:
        if ch == "'" and not in_double:
            in_single = not in_single
            out.append(" ")
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            out.append(" ")
            continue
        if in_single or in_double:
            out.append(" ")
            continue
        out.append(ch)
    stripped = "".join(out)
    ids = [m.group(1).lower() for m in IDENT_RE.finditer(stripped)]
    if not ids:
        # No identifier means likely numeric literal expression.
        return True
    # Only logical/null literal words are present.
    return all(n in LITERAL_WORDS for n in ids)


def is_single_identifier_expr(expr: str) -> bool:
    """Return True if expression is exactly one identifier token."""
    text = normalize_expr(expr)
    return re.match(r"^[a-z][a-z0-9_]*$", text, re.IGNORECASE) is not None


def is_empty_array_constructor_expr(expr: str) -> bool:
    """Return True for empty array constructors, typed or untyped."""
    text = normalize_expr(expr)
    if not (text.startswith("[") and text.endswith("]")):
        return False
    inner = text[1:-1].strip()
    if not inner:
        return True
    if "::" in inner:
        tail = inner.split("::", 1)[1].strip()
        return tail == ""
    return False


def extract_expressions(stmt: str) -> List[str]:
    """Extract candidate expressions from one statement."""
    out: List[str] = []
    low = stmt.strip().lower()
    m_asg = ASSIGN_RE.match(low)
    if m_asg and "=>" not in low:
        if "=" in stmt:
            rhs = stmt.split("=", 1)[1].strip()
            if rhs:
                out.append(rhs)
    if PRINT_WRITE_RE.match(low):
        # Heuristic: expression list after first comma.
        parts = split_top_level_commas(stmt)
        if len(parts) >= 2:
            for p in parts[1:]:
                t = p.strip()
                if t:
                    out.append(t)
    return out


def stmt_writes(stmt: str) -> Set[str]:
    """Extract simple variables written by the statement."""
    out: Set[str] = set()
    low = stmt.strip().lower()
    m_asg = ASSIGN_RE.match(low)
    if m_asg and "=>" not in low:
        out.add(m_asg.group(1).lower())
    return out


def analyze_file(path: Path) -> List[Finding]:
    """Analyze one source file for repeated patterns."""
    infos, any_missing = fscan.load_source_files([path])
    if not infos or any_missing:
        return []
    finfo = infos[0]
    findings: List[Finding] = []

    prev_if: Optional[Tuple[int, str]] = None
    seen: Dict[str, ExprSeen] = {}

    for lineno, stmt in fscan.iter_fortran_statements(finfo.parsed_lines):
        stripped = stmt.strip()
        low = stripped.lower()
        if not low:
            continue

        if UNIT_START_RE.match(low) or UNIT_END_RE.match(low):
            prev_if = None
            seen = {}
            continue

        cond = extract_if_condition(stripped)
        if cond is not None:
            norm_cond = normalize_expr(cond)
            if prev_if is not None and prev_if[1] == norm_cond:
                findings.append(
                    Finding(
                        path=path,
                        rule="repeated_if_condition",
                        line1=prev_if[0],
                        line2=lineno,
                        expr=cond.strip(),
                        confidence="high",
                        detail="consecutive IF statements test equivalent conditions",
                    )
                )
            prev_if = (lineno, norm_cond)
        else:
            prev_if = None

        if CONTROL_BARRIER_RE.match(low):
            seen = {}
            continue
        if NON_EXPR_IF_RE.match(low):
            seen = {}
            continue

        exprs = extract_expressions(stmt)
        writes_now = stmt_writes(stmt)

        for e in exprs:
            norm = normalize_expr(e)
            refs = expr_refs(e)
            has_any_call, has_user_call = call_profile(e)
            if has_user_call:
                continue
            if is_literal_only_expr(e):
                continue
            if is_single_identifier_expr(e):
                continue
            if is_empty_array_constructor_expr(e):
                continue
            conf = "medium" if has_any_call else "high"
            prior = seen.get(norm)
            if prior is not None:
                if prior.line == lineno:
                    # Ignore repeats created by semicolon-split statements on the same source line.
                    pass
                if prior.assigned_since & refs:
                    # Inputs changed between repeats.
                    pass
                elif prior.line != lineno:
                    findings.append(
                        Finding(
                            path=path,
                            rule="repeated_expression",
                            line1=prior.line,
                            line2=lineno,
                            expr=e.strip(),
                            confidence=conf if prior.confidence == conf else "medium",
                            detail="same normalized expression in same straight-line block with no intervening writes to inputs",
                        )
                    )
            seen[norm] = ExprSeen(
                line=lineno,
                raw_expr=e.strip(),
                refs=refs,
                confidence=conf,
                has_user_call=has_user_call,
                assigned_since=set(),
            )

        if writes_now:
            for s in seen.values():
                s.assigned_since.update(writes_now)

    return findings


def main() -> int:
    """Run repeated-calculation advisory checks."""
    parser = argparse.ArgumentParser(
        description="Advisory finder for repeated IF conditions and repeated expressions in straight-line blocks"
    )
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--verbose", action="store_true", help="Print all findings (default prints summary + first)")
    args = parser.parse_args()

    files = choose_files(args.fortran_files, args.exclude)
    if not files:
        print("No source files remain after applying --exclude filters.")
        return 2

    all_findings: List[Finding] = []
    for p in files:
        all_findings.extend(analyze_file(p))

    if not all_findings:
        print("No repeated-calculation findings.")
        return 0

    all_findings.sort(key=lambda f: (f.path.name.lower(), f.line1, f.line2, f.rule))
    print(f"{len(all_findings)} repeated-calculation finding(s).")
    if args.verbose:
        for f in all_findings:
            print(
                f"{f.path.name}:{f.line1},{f.line2} {f.rule} [{f.confidence}] "
                f"expr=({f.expr}) - {f.detail}"
            )
    else:
        by_file: Dict[str, int] = {}
        for f in all_findings:
            by_file[f.path.name] = by_file.get(f.path.name, 0) + 1
        for fname in sorted(by_file.keys(), key=str.lower):
            print(f"{fname}: {by_file[fname]}")
        first = all_findings[0]
        print(
            f"\nFirst finding: {first.path.name}:{first.line1},{first.line2} "
            f"{first.rule} [{first.confidence}] expr=({first.expr})"
        )
        print("Run with --verbose to list all findings.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
