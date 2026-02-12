#!/usr/bin/env python3
"""Compare local *.py files against a reference Pure-Fortran checkout."""

from __future__ import annotations

import argparse
import hashlib
import re
from pathlib import Path
from typing import Dict, Iterable, List, Set


def list_py_files(path: Path) -> Dict[str, Path]:
    """Return mapping of lowercase filename -> full path for top-level *.py files."""
    out: Dict[str, Path] = {}
    for p in sorted(path.glob("*.py"), key=lambda q: q.name.lower()):
        out[p.name.lower()] = p
    return out


def find_readme(repo_dir: Path, explicit: Path | None) -> Path:
    """Return README path, preferring explicit path if provided."""
    if explicit is not None:
        return explicit
    candidates = [
        "README.md",
        "README.rst",
        "README.txt",
        "README",
        "readme.md",
        "readme.rst",
        "readme.txt",
        "readme",
    ]
    for name in candidates:
        p = repo_dir / name
        if p.exists():
            return p
    raise FileNotFoundError(f"No README file found in {repo_dir}")


def mentioned_py_names(readme_path: Path) -> Set[str]:
    """Extract referenced *.py filenames from README text."""
    text = readme_path.read_text(encoding="utf-8", errors="ignore")
    names = set()
    for m in re.finditer(r"\b([A-Za-z0-9_.-]+\.py)\b", text, flags=re.IGNORECASE):
        names.add(m.group(1).lower())
    return names


def sha256_file(path: Path) -> str:
    """Compute SHA-256 digest of a file."""
    h = hashlib.sha256()
    with path.open("rb") as f:
        for chunk in iter(lambda: f.read(1024 * 1024), b""):
            h.update(chunk)
    return h.hexdigest()


def print_list(title: str, items: Iterable[str]) -> None:
    """Print one titled list."""
    vals = sorted(set(items), key=str.lower)
    print(title)
    if not vals:
        print("  (none)")
        return
    for name in vals:
        print(f"  {name}")


def main() -> int:
    """CLI entrypoint."""
    parser = argparse.ArgumentParser(
        description=(
            "List local *.py files not in Pure-Fortran README, "
            "missing from Pure-Fortran directory, and differing from matching files."
        )
    )
    parser.add_argument(
        "--local-dir",
        type=Path,
        default=Path("."),
        help="Directory containing local *.py files (default: current directory)",
    )
    parser.add_argument(
        "--repo-dir",
        type=Path,
        default=Path(r"c:\python\public_domain\github\Pure-Fortran"),
        help=r"Reference Pure-Fortran directory (default: c:\python\public_domain\github\Pure-Fortran)",
    )
    parser.add_argument(
        "--readme",
        type=Path,
        help="Optional explicit README path (default: auto-detect in --repo-dir)",
    )
    args = parser.parse_args()

    local_dir = args.local_dir.resolve()
    repo_dir = args.repo_dir.resolve()
    if not local_dir.exists() or not local_dir.is_dir():
        print(f"Local directory not found: {local_dir}")
        return 2
    if not repo_dir.exists() or not repo_dir.is_dir():
        print(f"Repo directory not found: {repo_dir}")
        return 2

    readme_path = find_readme(repo_dir, args.readme.resolve() if args.readme else None)
    if not readme_path.exists():
        print(f"README not found: {readme_path}")
        return 2

    local_files = list_py_files(local_dir)
    repo_files = list_py_files(repo_dir)
    readme_names = mentioned_py_names(readme_path)

    local_names = set(local_files.keys())
    repo_names = set(repo_files.keys())

    not_in_readme = sorted(local_names - readme_names)
    not_in_repo = sorted(local_names - repo_names)

    differs: List[str] = []
    for name in sorted(local_names & repo_names):
        if sha256_file(local_files[name]) != sha256_file(repo_files[name]):
            differs.append(name)

    print(f"Local directory: {local_dir}")
    print(f"Repo directory:  {repo_dir}")
    print(f"README:          {readme_path}")
    print("")
    print_list("Local *.py files not mentioned in README:", not_in_readme)
    print("")
    print_list("Local *.py files not found in repo directory:", not_in_repo)
    print("")
    print_list("Local *.py files that differ from repo versions:", differs)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
