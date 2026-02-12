#!/usr/bin/env python3
"""Check whether wrapping out-of-module procedures into a module compiles."""

from __future__ import annotations

import argparse
import glob
import re
import shlex
import subprocess
import zlib
from dataclasses import dataclass
from pathlib import Path
from typing import List, Optional, Sequence, Set, Tuple

import fortran_scan as fscan


PROC_START_RE = re.compile(
    r"^\s*(?P<prefix>(?:(?:pure|elemental|impure|recursive|module)\s+)*)"
    r"(?P<kind>function|subroutine)\s+"
    r"(?P<name>[a-z][a-z0-9_]*)\b",
    re.IGNORECASE,
)
MODULE_START_RE = re.compile(r"^\s*module\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
MODULE_PROC_RE = re.compile(r"^\s*module\s+procedure\b", re.IGNORECASE)
END_MODULE_RE = re.compile(r"^\s*end\s*module\b", re.IGNORECASE)
INTERFACE_START_RE = re.compile(r"^\s*(abstract\s+)?interface\b", re.IGNORECASE)
END_INTERFACE_RE = re.compile(r"^\s*end\s+interface\b", re.IGNORECASE)


@dataclass
class FileResult:
    """Per-file result."""

    source: Path
    has_outside_procs: bool
    module_name: Optional[str] = None
    compile_ok: Optional[bool] = None
    stderr: str = ""
    stdout: str = ""
    outside_count: int = 0


def expand_inputs(specs: Sequence[str]) -> List[Path]:
    """Expand file/glob inputs to a de-duplicated list of paths."""
    out: List[Path] = []
    seen: Set[str] = set()
    for spec in specs:
        matches = glob.glob(spec, recursive=True)
        if not matches:
            matches = [spec]
        for m in matches:
            p = Path(m)
            key = str(p.resolve()).lower() if p.exists() else str(p).lower()
            if key in seen:
                continue
            seen.add(key)
            out.append(p)
    out.sort(key=lambda p: str(p).lower())
    return out


def load_codes(path: Path) -> List[str]:
    """Load file paths/patterns from a text list (one entry per line)."""
    out: List[str] = []
    for raw in path.read_text(encoding="utf-8", errors="ignore").splitlines():
        line = raw.strip()
        if not line or line.startswith("#"):
            continue
        out.append(line)
    return out


def choose_module_name(lines: List[str], source: Path) -> str:
    """Choose a deterministic module name not present in the file."""
    used: Set[str] = set()
    for raw in lines:
        m = MODULE_START_RE.match(fscan.strip_comment(raw).strip())
        if m and not MODULE_PROC_RE.match(raw):
            used.add(m.group(1).lower())
    seed = zlib.crc32(str(source).encode("utf-8")) % 1_000_000
    for i in range(1000):
        name = f"m{(seed + i) % 1_000_000:06d}"
        if name.lower() not in used:
            return name
    return "m999999"


def is_proc_end(stmt_low: str, expected_kind: str) -> bool:
    """Return True if statement ends a procedure of the expected kind."""
    s = stmt_low.strip()
    if s == "end":
        return True
    if s in {"end function", "end subroutine"}:
        return s.endswith(expected_kind)
    if s == "endfunction":
        return expected_kind == "function"
    if s == "endsubroutine":
        return expected_kind == "subroutine"
    return False


def outside_proc_ranges(lines: List[str]) -> List[Tuple[int, int]]:
    """Return 1-based inclusive start/end ranges for procedures outside modules."""
    module_depth = 0
    interface_depth = 0
    stack: List[Tuple[str, int, bool]] = []  # (kind, start_line, in_module)
    out: List[Tuple[int, int]] = []

    for lineno, stmt in fscan.iter_fortran_statements(lines):
        low = stmt.strip().lower()
        if not low:
            continue

        if INTERFACE_START_RE.match(low):
            interface_depth += 1
            continue
        if END_INTERFACE_RE.match(low):
            if interface_depth > 0:
                interface_depth -= 1
            continue

        if interface_depth == 0:
            if END_MODULE_RE.match(low):
                if module_depth > 0:
                    module_depth -= 1
                continue
            m_mod = MODULE_START_RE.match(low)
            if m_mod and not MODULE_PROC_RE.match(low) and not low.startswith("end"):
                module_depth += 1
                continue

            m_proc = PROC_START_RE.match(low)
            if m_proc:
                kind = m_proc.group("kind").lower()
                stack.append((kind, lineno, module_depth > 0))
                continue

            if stack:
                kind, start, in_module = stack[-1]
                if is_proc_end(low, kind):
                    stack.pop()
                    if not in_module:
                        out.append((start, lineno))

    while stack:
        kind, start, in_module = stack.pop()
        if not in_module:
            out.append((start, len(lines)))
    return out


def build_wrapped_text(lines: List[str], source: Path) -> Tuple[str, Optional[str], int]:
    """Build wrapped text if needed; returns text, module name, outside count."""
    ranges = outside_proc_ranges(lines)
    if not ranges:
        text = "".join(lines)
        if text and not text.endswith("\n"):
            text += "\n"
        return text, None, 0

    first = min(s for s, _e in ranges)
    last = max(e for _s, e in ranges)
    module_name = choose_module_name(lines, source)
    nl = "\r\n" if any(ln.endswith("\r\n") for ln in lines) else "\n"

    out: List[str] = []
    out.extend(lines[: first - 1])
    out.append(f"module {module_name}{nl}")
    out.append(f"contains{nl}")
    out.extend(lines[first - 1 : last])
    if not out[-1].endswith(("\n", "\r\n")):
        out[-1] = out[-1] + nl
    out.append(f"end module {module_name}{nl}")
    out.extend(lines[last:])

    text = "".join(out)
    if text and not text.endswith("\n"):
        text += nl
    return text, module_name, len(ranges)


def command_from_template(template: str, file_path: Path) -> List[str]:
    """Build argv from command template and file path."""
    if "{file}" in template:
        return shlex.split(template.replace("{file}", str(file_path)), posix=False)
    argv = shlex.split(template, posix=False)
    argv.append(str(file_path))
    return argv


def compile_check(cmd_template: str, file_path: Path) -> subprocess.CompletedProcess[str]:
    """Compile-check one file path."""
    argv = command_from_template(cmd_template, file_path)
    return subprocess.run(argv, capture_output=True, text=True)


def process_file(source: Path, temp_path: Path, compile_cmd: str) -> FileResult:
    """Process one source file and return informational result."""
    if not source.exists():
        return FileResult(source=source, has_outside_procs=False, compile_ok=False, stderr="file not found")
    text = source.read_text(encoding="utf-8", errors="ignore")
    lines = text.splitlines(keepends=True)
    wrapped, module_name, outside_count = build_wrapped_text(lines, source)
    temp_path.write_text(wrapped, encoding="utf-8", newline="")
    proc = compile_check(compile_cmd, temp_path)
    return FileResult(
        source=source,
        has_outside_procs=(module_name is not None),
        module_name=module_name,
        compile_ok=(proc.returncode == 0),
        stderr=(proc.stderr or "").rstrip(),
        stdout=(proc.stdout or "").rstrip(),
        outside_count=outside_count,
    )


def main() -> int:
    """CLI entrypoint."""
    parser = argparse.ArgumentParser(
        description=(
            "Informational check: wrap procedures outside modules in a temporary module, "
            "write to temp.f90, and compile-check."
        )
    )
    parser.add_argument("inputs", nargs="*", help="Input files and/or glob patterns")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument(
        "--codes",
        type=Path,
        help="Optional path to list file with input files/globs (one per line)",
    )
    parser.add_argument("--temp", type=Path, default=Path("temp.f90"), help="Temporary output path (default: temp.f90)")
    parser.add_argument(
        "--compile-cmd",
        type=str,
        default="gfortran -c {file}",
        help="Compile command template (default: gfortran -c {file})",
    )
    parser.add_argument("--limit", type=int, help="Maximum number of input files to check")
    parser.add_argument("--verbose", action="store_true", help="Print compiler output for each file")
    args = parser.parse_args()
    if args.limit is not None and args.limit < 1:
        print("--limit must be >= 1.")
        return 2

    specs: List[str] = list(args.inputs)
    if args.codes:
        if not args.codes.exists():
            print(f"Codes list not found: {args.codes}")
            return 2
        specs.extend(load_codes(args.codes))
    if not specs:
        print("Provide at least one input path/glob or --codes list.")
        return 2

    paths = expand_inputs(specs)
    if args.exclude:
        paths = fscan.apply_excludes(paths, args.exclude)
    if args.limit is not None:
        paths = paths[: args.limit]
    if not paths:
        print("No input files matched.")
        return 2

    checked = 0
    wrapped = 0
    compiled_ok = 0
    for src in paths:
        res = process_file(src, args.temp, args.compile_cmd)
        checked += 1
        status = "OK" if res.compile_ok else "FAIL"
        if res.has_outside_procs:
            wrapped += 1
            print(
                f"{status}: {src} -> wrapped {res.outside_count} outside procedure(s) "
                f"in module {res.module_name}; wrote {args.temp}"
            )
        else:
            print(f"{status}: {src} -> no outside procedures; copied/checked via {args.temp}")
        if res.compile_ok:
            compiled_ok += 1
        if args.verbose:
            if res.stdout:
                print("stdout:")
                print(res.stdout)
            if res.stderr:
                print("stderr:")
                print(res.stderr)
        elif not res.compile_ok and res.stderr:
            print("stderr:")
            print(res.stderr)

    print("")
    print(f"Checked: {checked}")
    print(f"Had outside procedures: {wrapped}")
    print(f"Compile OK: {compiled_ok}")
    print(f"Compile failed: {checked - compiled_ok}")
    return 0 if compiled_ok == checked else 1


if __name__ == "__main__":
    raise SystemExit(main())
