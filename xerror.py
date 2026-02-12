#!/usr/bin/env python3
"""Summarize warning/error kinds from xcompile.py output logs."""

from __future__ import annotations

import argparse
import re
from collections import defaultdict
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, Optional, Set, Tuple


COMPILE_RE = re.compile(r"^\[\d+\]\s+Compiling\s+(.+?)\s*$")
LOC_RE = re.compile(r"^(.+):(\d+):(\d+):\s*$")
DIAG_RE = re.compile(r"^\s*(Fatal Error|Error|Warning):\s*(.+?)\s*$", re.IGNORECASE)


@dataclass
class KindStats:
    """Aggregated counts for one normalized diagnostic kind."""

    severity: str
    kind: str
    occurrences: int = 0
    files: Set[str] = field(default_factory=set)
    lines: Set[Tuple[str, int]] = field(default_factory=set)


def normalize_kind(text: str) -> str:
    """Normalize message text so similar diagnostics collapse to one kind."""
    out = text.strip()
    out = re.sub(r"\s+at\s+\(\d+\)", "", out, flags=re.IGNORECASE)
    out = re.sub(r"\(\d+\)", "(#)", out)
    out = re.sub(r"'[^']*'", "'<id>'", out)
    out = re.sub(r'"[^"]*"', '"<id>"', out)
    out = re.sub(r"\b\d+\b", "<n>", out)
    out = re.sub(r"\s+", " ", out).strip()
    return out


def parse_log(path: Path) -> Dict[Tuple[str, str], KindStats]:
    """Parse xcompile output and return aggregated stats by (severity, kind)."""
    stats: Dict[Tuple[str, str], KindStats] = {}
    current_compile_file: Optional[str] = None
    last_loc_file: Optional[str] = None
    last_loc_line: Optional[int] = None

    for raw in path.read_text(encoding="utf-8", errors="ignore").splitlines():
        m_compile = COMPILE_RE.match(raw)
        if m_compile:
            current_compile_file = m_compile.group(1).strip()
            last_loc_file = None
            last_loc_line = None
            continue

        m_loc = LOC_RE.match(raw)
        if m_loc:
            last_loc_file = m_loc.group(1).strip()
            last_loc_line = int(m_loc.group(2))
            continue

        m_diag = DIAG_RE.match(raw)
        if not m_diag:
            continue

        sev = m_diag.group(1).lower()
        sev = "error" if sev == "fatal error" else sev
        kind = normalize_kind(m_diag.group(2))
        key = (sev, kind)
        if key not in stats:
            stats[key] = KindStats(severity=sev, kind=kind)
        s = stats[key]
        s.occurrences += 1

        file_for_diag = last_loc_file if last_loc_file else current_compile_file
        if file_for_diag:
            s.files.add(file_for_diag)
        if last_loc_file and last_loc_line is not None:
            s.lines.add((last_loc_file, last_loc_line))

    return stats


def main() -> int:
    """CLI entrypoint."""
    parser = argparse.ArgumentParser(
        description="Summarize warning/error kinds from xcompile.py output logs"
    )
    parser.add_argument("logfile", nargs="?", type=Path, default=Path("burkardt_errors.txt"))
    args = parser.parse_args()

    if not args.logfile.exists():
        print(f"Log file not found: {args.logfile}")
        return 2

    stats = parse_log(args.logfile)
    if not stats:
        print("No warnings/errors found.")
        return 0

    rows = list(stats.values())
    rows.sort(key=lambda s: (0 if s.severity == "error" else 1, -len(s.files), -len(s.lines), -s.occurrences, s.kind))

    print("severity files lines hits kind")
    for s in rows:
        print(f"{s.severity} {len(s.files)} {len(s.lines)} {s.occurrences} {s.kind}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
