#!/usr/bin/env python3
"""Print long-option summaries for x*.py scripts."""

from __future__ import annotations

import argparse
import ast
from pathlib import Path
from typing import List, Set, Tuple

import cli_paths as cpaths


def choose_files(args_files: List[Path]) -> List[Path]:
    """Resolve input scripts; default to all x*.py files in current directory."""
    if args_files:
        files = cpaths.expand_path_args(args_files)
    else:
        files = sorted(Path(".").glob("x*.py"), key=lambda p: p.name.lower())
    out: List[Path] = []
    for p in files:
        if p.is_file() and p.suffix.lower() == ".py" and p.name.lower().startswith("x"):
            out.append(p)
    return sorted(set(out), key=lambda p: p.name.lower())


def split_targets_and_filters(tokens: List[str]) -> Tuple[List[Path], List[str]]:
    """Split CLI tokens into script targets and option-name filters."""
    scripts: List[Path] = []
    filters: List[str] = []
    for tok in tokens:
        st = tok.strip()
        if not st:
            continue
        if st.lower().endswith(".py"):
            scripts.append(Path(st))
            continue
        # Option filter mode accepts both "out" and "--out".
        if st.startswith("--"):
            st = st[2:]
        elif st.startswith("-"):
            st = st[1:]
        st = st.strip().lower()
        if st:
            filters.append(st)
    return scripts, sorted(set(filters))


def collect_long_options(path: Path) -> Set[str]:
    """Extract long option names from argparse add_argument calls."""
    text = path.read_text(encoding="utf-8", errors="ignore")
    # Be tolerant of BOM in source files.
    if "\ufeff" in text:
        text = text.replace("\ufeff", "")
    tree = ast.parse(text, filename=str(path))
    opts: Set[str] = set()
    for node in ast.walk(tree):
        if not isinstance(node, ast.Call):
            continue
        func = node.func
        if not (isinstance(func, ast.Attribute) and func.attr == "add_argument"):
            continue
        for arg in node.args:
            if isinstance(arg, ast.Constant) and isinstance(arg.value, str):
                s = arg.value.strip()
                if s.startswith("--") and len(s) > 2:
                    opts.add(s[2:])
    return opts


def summarize(path: Path) -> str:
    """Return one-line summary in the requested compact format."""
    opts = sorted(collect_long_options(path))
    if opts:
        return f"{path.name} {len(opts)} {' '.join(opts)}"
    return f"{path.name} 0"


def main() -> int:
    """Parse args and print option summaries."""
    parser = argparse.ArgumentParser(description="List long options used by x*.py scripts")
    parser.add_argument("items", nargs="*")
    args, unknown = parser.parse_known_args()
    items = list(args.items) + list(unknown)

    scripts, filters = split_targets_and_filters(items)
    files = choose_files(scripts)
    if not files:
        print("No x*.py scripts found.")
        return 2

    bad = 0
    matched = 0
    for p in files:
        try:
            opts = collect_long_options(p)
            if filters and not all(f in opts for f in filters):
                continue
            matched += 1
            opts_sorted = sorted(opts)
            if opts_sorted:
                print(f"{p.name} {len(opts_sorted)} {' '.join(opts_sorted)}")
            else:
                print(f"{p.name} 0")
        except SyntaxError as exc:
            bad += 1
            print(f"{p.name} parse-error {exc.msg}")

    if filters and matched == 0 and bad == 0:
        print("No scripts matched the requested option filter(s).")
        return 2

    return 1 if bad else 0


if __name__ == "__main__":
    raise SystemExit(main())
