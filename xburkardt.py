#!/usr/bin/env python3
"""Add INTENT(IN/OUT) using Burkardt-style parameter comment annotations."""

from __future__ import annotations

import argparse
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Set, Tuple

import cli_paths as cpaths
import fortran_scan as fscan

TYPE_DECL_RE = re.compile(
    r"^\s*(integer|real|logical|character|complex|type\b|class\b|procedure\b)",
    re.IGNORECASE,
)
NO_COLON_DECL_RE = re.compile(
    r"^\s*(?P<spec>(?:integer|real|logical|complex|character|double\s+precision)\s*(?:\([^)]*\))?"
    r"|type\s*\([^)]*\)|class\s*\([^)]*\)|procedure\s*\([^)]*\))\s+(?P<rhs>.+)$",
    re.IGNORECASE,
)


@dataclass
class Change:
    """One applied declaration change."""

    path: Path
    proc_kind: str
    proc_name: str
    line: int
    detail: str


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


def split_code_comment(line: str) -> Tuple[str, str]:
    """Split one source line into code and trailing comment parts."""
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


def split_decl_entities(rhs: str) -> List[str]:
    """Split declaration entity list on top-level commas."""
    out: List[str] = []
    cur: List[str] = []
    depth = 0
    in_single = False
    in_double = False
    for ch in rhs:
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


def leading_name(text: str) -> Optional[str]:
    """Extract leading identifier from a declaration entity chunk."""
    m = re.match(r"^\s*([a-z][a-z0-9_]*)", text, re.IGNORECASE)
    if not m:
        return None
    return m.group(1).lower()


def extract_comment_intents(lines: List[str], start_idx: int, dummy_names: Set[str]) -> Dict[str, str]:
    """Extract dummy -> intent(in/out) map from initial Burkardt parameter comments."""
    wanted = {d.lower() for d in dummy_names}
    seen: Dict[str, str] = {}
    conflicted: Set[str] = set()

    i = start_idx + 1
    while i < len(lines):
        stripped = lines[i].strip()
        if not stripped:
            i += 1
            continue
        if not stripped.startswith("!"):
            break
        text = stripped[1:].strip().lower()
        direction = ""
        payload = ""
        m_io = re.match(r"^(input\s*/\s*output)\s*,\s*(.*)$", text)
        m_in = re.match(r"^(input)\s*,\s*(.*)$", text)
        m_out = re.match(r"^(output)\s*,\s*(.*)$", text)
        if m_io:
            direction = "in out"
            payload = m_io.group(2)
        elif m_in:
            direction = "in"
            payload = m_in.group(2)
        elif m_out:
            direction = "out"
            payload = m_out.group(2)
        else:
            i += 1
            continue

        payload = re.sub(
            r"^(integer|real|logical|character|complex|double\s+precision"
            r"|type\s*\([^)]*\)|class\s*\([^)]*\)|procedure\s*\([^)]*\))\s+",
            "",
            payload,
            flags=re.IGNORECASE,
        )
        for chunk in split_decl_entities(payload):
            nm = leading_name(chunk)
            if not nm:
                break
            if nm not in wanted:
                continue
            d = nm
            prev = seen.get(d)
            if prev is None:
                seen[d] = direction
            elif prev != direction:
                conflicted.add(d)
        i += 1

    return {d: v for d, v in seen.items() if d not in conflicted}


def rewrite_decl_line_with_intents(
    raw_line: str,
    intents_by_name: Dict[str, str],
) -> Tuple[List[str], bool, List[Tuple[str, str]]]:
    """Rewrite one declaration line by adding INTENT for targeted entities."""
    if ";" in raw_line or "&" in raw_line:
        return [raw_line], False, []

    eol = "\n"
    if raw_line.endswith("\r\n"):
        eol = "\r\n"
    elif raw_line.endswith("\n"):
        eol = "\n"

    code, comment = split_code_comment(raw_line.rstrip("\r\n"))
    if not TYPE_DECL_RE.match(code.strip()):
        return [raw_line], False, []

    m_existing_intent = re.search(r"\bintent\s*\(\s*(in\s*out|inout|in|out)\s*\)", code, re.IGNORECASE)
    if re.search(r"\bvalue\b", code, re.IGNORECASE):
        return [raw_line], False, []

    lhs = ""
    rhs = ""
    if "::" in code:
        lhs, rhs = code.split("::", 1)
    else:
        m_no = NO_COLON_DECL_RE.match(code.strip())
        if not m_no:
            return [raw_line], False, []
        lhs = m_no.group("spec")
        rhs = m_no.group("rhs")

    entities = split_decl_entities(rhs)
    if not entities:
        return [raw_line], False, []

    targeted: List[Tuple[str, str, str]] = []
    remaining: List[str] = []
    for ent in entities:
        nm = leading_name(ent)
        if not nm:
            remaining.append(ent)
            continue
        intent = intents_by_name.get(nm)
        if intent in {"in", "out", "in out"}:
            targeted.append((ent, nm, intent))
        else:
            remaining.append(ent)

    if not targeted:
        return [raw_line], False, []

    # If declaration already has INTENT(...), only upgrade IN -> IN OUT when needed.
    if m_existing_intent is not None:
        existing = m_existing_intent.group(1).lower().replace("inout", "in out")
        wants_inout = any(intent == "in out" for _ent, _nm, intent in targeted)
        if existing == "in" and wants_inout:
            new_code = re.sub(
                r"\bintent\s*\(\s*in\s*\)",
                "intent(in out)",
                code,
                count=1,
                flags=re.IGNORECASE,
            )
            applied = [(nm, "in out") for _ent, nm, intent in targeted if intent == "in out"]
            return [new_code + comment + eol], True, applied
        return [raw_line], False, []

    base = lhs.rstrip()
    out_lines: List[str] = []
    applied: List[Tuple[str, str]] = []
    for ent, nm, intent in targeted:
        out_lines.append(f"{base}, intent({intent}) :: {ent}{eol}")
        applied.append((nm, intent))

    if remaining:
        tail = f"{base} :: {', '.join(remaining)}"
        if comment:
            tail += comment
        out_lines.append(tail + eol)
    elif comment:
        out_lines[-1] = out_lines[-1].rstrip("\r\n") + comment + eol
    return out_lines, True, applied


def apply_file(path: Path, verbose: bool) -> Tuple[int, List[Change]]:
    """Apply Burkardt comment-derived intent edits to one file."""
    infos, any_missing = fscan.load_source_files([path])
    if not infos or any_missing:
        return 0, []
    finfo = infos[0]
    lines = path.read_text(encoding="utf-8", errors="ignore").splitlines(keepends=True)
    if not lines:
        return 0, []

    changes: List[Change] = []
    offset = 0
    for proc in sorted(finfo.procedures, key=lambda p: p.start):
        if not proc.dummy_names:
            continue

        start_idx = proc.start - 1 + offset
        if start_idx < 0 or start_idx >= len(lines):
            continue
        intents = extract_comment_intents(lines, start_idx, set(proc.dummy_names))
        if not intents:
            continue

        for ln, _stmt in sorted(proc.body, key=lambda x: x[0]):
            idx = ln - 1 + offset
            if idx < 0 or idx >= len(lines):
                continue
            new_lines, did_change, applied = rewrite_decl_line_with_intents(lines[idx], intents)
            if not did_change:
                continue

            lines[idx:idx + 1] = new_lines
            offset += len(new_lines) - 1
            if verbose:
                toks = ", ".join(f"{n}->intent({it})" for n, it in applied)
                changes.append(
                    Change(
                        path=path,
                        proc_kind=proc.kind,
                        proc_name=proc.name,
                        line=idx + 1,
                        detail=toks,
                    )
                )

    new_text = "".join(lines)
    old_text = path.read_text(encoding="utf-8", errors="ignore")
    if new_text == old_text:
        return 0, changes
    path.write_text(new_text, encoding="utf-8", newline="")
    return 1, changes


def main() -> int:
    """Run Burkardt comment-driven intent insertion on selected files."""
    parser = argparse.ArgumentParser(
        description="Add INTENT(IN/OUT) from Burkardt-style 'Input/Output' parameter comments"
    )
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--verbose", action="store_true", help="List each declaration change made")
    args = parser.parse_args()

    files = choose_files(args.fortran_files, args.exclude)
    if not files:
        print("No source files remain after applying --exclude filters.")
        return 2

    touched = 0
    all_changes: List[Change] = []
    for p in files:
        changed, changes = apply_file(p, verbose=args.verbose)
        touched += changed
        all_changes.extend(changes)

    if args.verbose and all_changes:
        for c in all_changes:
            print(f"{c.path.name}:{c.line} {c.proc_kind} {c.proc_name} {c.detail}")

    print(f"Updated {touched} file(s).")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
