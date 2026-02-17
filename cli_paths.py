#!/usr/bin/env python3
"""Generic CLI path argument helpers."""

from __future__ import annotations

from glob import glob
from pathlib import Path
from typing import Iterable, List, Sequence


def expand_path_args(paths: Iterable[Path]) -> List[Path]:
    """Expand wildcard path arguments (for Windows shells that do not expand globs)."""
    out: List[Path] = []
    for p in paths:
        raw = str(p)
        if any(ch in raw for ch in "*?[]"):
            matches = sorted(glob(raw), key=lambda x: x.lower())
            if matches:
                out.extend(Path(m) for m in matches)
            else:
                # Keep unmatched pattern so existing missing-file reporting remains intact.
                out.append(p)
        else:
            out.append(p)
    return out


def expand_source_inputs(
    paths: Iterable[Path],
    *,
    extensions: Sequence[str] = (".f90", ".F90"),
) -> List[Path]:
    """Expand CLI path args and treat directories as source globs.

    - Wildcards are expanded via ``expand_path_args``.
    - Directory entries are expanded to matching files by extension.
    - File entries (or unmatched wildcard tokens) are preserved.
    - Results are de-duplicated and sorted stably for deterministic processing.
    """
    expanded = expand_path_args(paths)
    out: set[Path] = set()
    for p in expanded:
        if p.is_dir():
            for ext in extensions:
                out.update(p.glob(f"*{ext}"))
        else:
            out.add(p)
    return sorted(out, key=lambda p: (p.name.lower(), str(p).lower()))
