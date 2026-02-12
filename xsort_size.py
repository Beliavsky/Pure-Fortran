#!/usr/bin/env python3
"""Sort a newline-delimited file list by file size (ascending)."""

from __future__ import annotations

import argparse
from pathlib import Path
from typing import List, Tuple


def load_entries(path: Path) -> List[Path]:
    """Load non-empty, non-comment file paths from a list file."""
    out: List[Path] = []
    for raw in path.read_text(encoding="utf-8", errors="ignore").splitlines():
        line = raw.strip()
        if not line or line.startswith("#"):
            continue
        out.append(Path(line))
    return out


def default_output_path(input_path: Path) -> Path:
    """Build default output path next to input list."""
    return input_path.with_name(f"{input_path.stem}_size_sorted{input_path.suffix or '.txt'}")


def main() -> int:
    """Sort input list by source file size and write output list."""
    parser = argparse.ArgumentParser(
        description="Sort list of file paths in ascending order of file size."
    )
    parser.add_argument("list_file", type=Path, help="Input file list (e.g., codes.txt)")
    parser.add_argument(
        "--output",
        type=Path,
        help="Output list path (default: <input>_size_sorted.txt)",
    )
    args = parser.parse_args()

    if not args.list_file.exists():
        print(f"Input list not found: {args.list_file}")
        return 2

    entries = load_entries(args.list_file)
    if not entries:
        print(f"No file entries found in {args.list_file}")
        return 2

    sized: List[Tuple[int, Path]] = []
    missing = 0
    for p in entries:
        if not p.exists():
            missing += 1
            continue
        sized.append((p.stat().st_size, p))

    if not sized:
        print("No existing files to sort.")
        return 2

    sized.sort(key=lambda x: (x[0], str(x[1]).lower()))
    out_path = args.output or default_output_path(args.list_file)
    out_lines = [str(p) for _size, p in sized]
    out_path.write_text("\n".join(out_lines) + "\n", encoding="utf-8")

    print(f"Wrote {len(sized)} sorted entries to {out_path}")
    if missing:
        print(f"Skipped {missing} missing file(s).")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
