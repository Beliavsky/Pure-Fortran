#!/usr/bin/env python3
"""Print long-option summaries for x*.py scripts."""

from __future__ import annotations

import argparse
import ast
from pathlib import Path
from typing import Iterable, List, Set

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
    parser.add_argument("scripts", type=Path, nargs="*")
    args = parser.parse_args()

    files = choose_files(args.scripts)
    if not files:
        print("No x*.py scripts found.")
        return 2

    bad = 0
    for p in files:
        try:
            print(summarize(p))
        except SyntaxError as exc:
            bad += 1
            print(f"{p.name} parse-error {exc.msg}")

    return 1 if bad else 0


if __name__ == "__main__":
    raise SystemExit(main())
