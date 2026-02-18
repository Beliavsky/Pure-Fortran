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


def normalized_text(path: Path) -> str:
    """Read file text with normalized newlines and stripped leading BOM."""
    text = path.read_text(encoding="utf-8", errors="ignore")
    text = text.replace("\r\n", "\n").replace("\r", "\n")
    if text.startswith("\ufeff"):
        text = text[1:]
    return text


def files_differ(local_path: Path, repo_path: Path, *, mode: str) -> bool:
    """Compare files in requested mode."""
    if mode == "bytes":
        return sha256_file(local_path) != sha256_file(repo_path)
    return normalized_text(local_path) != normalized_text(repo_path)


def print_list(title: str, items: Iterable[str], *, flat: bool = False) -> None:
    """Print one titled list."""
    vals = sorted(set(items), key=str.lower)
    if flat:
        print(title)
        if not vals:
            print("(none)")
        else:
            print(" ".join(f'"{v}"' for v in vals))
        return
    print(title)
    if not vals:
        print("  (none)")
        return
    for name in vals:
        print(f"  {name}")


def module_name_from_file(filename: str) -> str:
    """Return lowercase module stem for a Python filename."""
    return Path(filename).stem.lower()


def imported_local_files(path: Path, module_to_file: Dict[str, str]) -> Set[str]:
    """Return local *.py filenames imported by one file (best-effort static parse)."""
    imports: Set[str] = set()
    text = path.read_text(encoding="utf-8", errors="ignore")
    for raw in text.splitlines():
        code = raw.split("#", 1)[0].strip()
        if not code:
            continue
        m_from = re.match(r"^from\s+([A-Za-z_][A-Za-z0-9_]*)\s+import\b", code)
        if m_from:
            mod = m_from.group(1).lower()
            if mod in module_to_file:
                imports.add(module_to_file[mod])
            continue
        m_imp = re.match(r"^import\s+(.+)$", code)
        if m_imp:
            parts = [p.strip() for p in m_imp.group(1).split(",")]
            for part in parts:
                m_part = re.match(r"^([A-Za-z_][A-Za-z0-9_]*)(?:\s+as\s+[A-Za-z_][A-Za-z0-9_]*)?$", part)
                if not m_part:
                    continue
                mod = m_part.group(1).lower()
                if mod in module_to_file:
                    imports.add(module_to_file[mod])
    return imports


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
    parser.add_argument(
        "--compare-mode",
        choices=["text", "bytes"],
        default="text",
        help="File comparison mode for 'differ' list (default: text; bytes is strict hash compare)",
    )
    parser.add_argument(
        "--flat",
        action="store_true",
        help="Print each output group on one line",
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

    module_to_file = {module_name_from_file(name): name for name in local_names}
    imported_by: Dict[str, Set[str]] = {name: set() for name in local_names}
    for src_name, src_path in local_files.items():
        for dep_name in imported_local_files(src_path, module_to_file):
            imported_by.setdefault(dep_name, set()).add(src_name)

    def is_ship_candidate(name: str) -> bool:
        # Heuristic: files likely intended for upload/distribution.
        return (
            name in repo_names
            or name in readme_names
            or name.startswith("x")
            or name.startswith("fortran_")
            or name == "cli_paths.py"
        )

    missing_required: List[str] = []
    missing_optional: List[str] = []
    missing_required_reasons: Dict[str, List[str]] = {}
    for name in not_in_repo:
        importers = sorted(n for n in imported_by.get(name, set()) if is_ship_candidate(n))
        if importers:
            missing_required.append(name)
            missing_required_reasons[name] = importers
        else:
            missing_optional.append(name)

    differs: List[str] = []
    for name in sorted(local_names & repo_names):
        if files_differ(local_files[name], repo_files[name], mode=args.compare_mode):
            differs.append(name)

    print(f"Local directory: {local_dir}")
    print(f"Repo directory:  {repo_dir}")
    print(f"README:          {readme_path}")
    print("")
    print_list("Local *.py files not mentioned in README:", not_in_readme, flat=args.flat)
    print("")
    print_list("Local *.py files not found in repo directory:", not_in_repo, flat=args.flat)
    print("")
    print_list(
        "Subset of local *.py files not in repo that are required by upload-worthy files:",
        missing_required,
        flat=args.flat,
    )
    if args.flat:
        reason_parts = [
            f"{name} <- {', '.join(missing_required_reasons.get(name, []))}"
            for name in sorted(missing_required)
        ]
        if reason_parts:
            print("Import reasons: " + " | ".join(reason_parts))
        else:
            print("Import reasons: (none)")
    else:
        for name in sorted(missing_required):
            print(f"  -> {name} imported by: {', '.join(missing_required_reasons.get(name, []))}")
    print("")
    print_list(
        "Subset of local *.py files not in repo that appear standalone/debug (no ship-file imports):",
        missing_optional,
        flat=args.flat,
    )
    print("")
    print_list("Local *.py files that differ from repo versions:", differs, flat=args.flat)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
