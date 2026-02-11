#!/usr/bin/env python3
"""Generic CLI path argument helpers."""

from __future__ import annotations

from glob import glob
from pathlib import Path
from typing import Iterable, List


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
