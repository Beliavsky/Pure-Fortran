#!/usr/bin/env python3
"""Round-trip test Fortran transforms via xarray.py and xto_loop.py.

For each input source file, this script can run:
1) xarray -> xto_loop, then semantic-compare original vs round-tripped file
2) xto_loop -> xarray, then semantic-compare original vs round-tripped file
"""

from __future__ import annotations

import argparse
import re
import shutil
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, List, Optional, Sequence, Tuple

import cli_paths as cpaths
import fortran_scan as fscan


Direction = str


@dataclass
class RoundtripResult:
    source: Path
    direction: Direction
    ok_tools: bool
    ok_semantic: Optional[bool]
    step1: Optional[Path]
    step2: Optional[Path]
    out_final: Optional[Path]
    error: str = ""


def choose_files(args_files: Iterable[Path], exclude: Iterable[str]) -> List[Path]:
    files = cpaths.expand_source_inputs(args_files)
    return fscan.apply_excludes(files, exclude)


def run_cmd(cmd: Sequence[str]) -> Tuple[bool, str]:
    proc = subprocess.run(cmd, text=True, capture_output=True)
    out = (proc.stdout or "") + (proc.stderr or "")
    return proc.returncode == 0, out


def _tokenize_output(text: str) -> List[str]:
    return re.findall(r"[^\s]+", text)


def _parse_float_token(tok: str) -> Optional[float]:
    t = tok.strip()
    if not t:
        return None
    t = re.sub(r"(?i)(\d)d([+-]?\d+)", r"\1e\2", t)
    try:
        return float(t)
    except ValueError:
        return None


def _outputs_match(a: str, b: str, *, rtol: float = 1e-6, atol: float = 1e-9) -> bool:
    ta = _tokenize_output(a)
    tb = _tokenize_output(b)
    if len(ta) != len(tb):
        return False
    for xa, xb in zip(ta, tb):
        fa = _parse_float_token(xa)
        fb = _parse_float_token(xb)
        if fa is None or fb is None:
            if xa != xb:
                return False
            continue
        diff = abs(fa - fb)
        scale = max(abs(fa), abs(fb))
        if diff > atol + rtol * scale:
            return False
    return True


def run_tool(
    tool: str,
    src: Path,
    out: Path,
    *,
    inline: bool,
    no_annotate: bool,
    verbose: bool,
) -> Tuple[bool, str]:
    cmd: List[str] = [sys.executable, tool, str(src), "--out", str(out), "--no-trace"]
    if no_annotate:
        cmd.append("--no-annotate")
    if tool.lower() == "xarray.py" and inline:
        cmd.append("--inline")
    if verbose:
        print(" ".join(cmd))
    ok, out_txt = run_cmd(cmd)
    out_low = out_txt.lower()
    no_candidates = (
        "no array-operation replacement candidates found" in out_low
        or "no array-operation-to-loop replacement candidates found" in out_low
        or "no loopification candidate(s)" in out_low
        or "no loopification candidates found" in out_low
    )
    if ok and (not out.exists() or no_candidates):
        # Some tools report "no candidates" and do not emit/refresh --out.
        shutil.copy2(src, out)
    return ok, out_txt


def semantic_compare(src: Path, dst: Path, *, verbose: bool) -> Tuple[bool, str]:
    cmd = [sys.executable, "xdiff.py", str(src), str(dst), "--semantic"]
    if verbose:
        print(" ".join(cmd))
    ok, out = run_cmd(cmd)
    if ok:
        return True, out
    # Fallback to behavior equivalence for semantic-text mismatches.
    ok_run, run_out = run_compare_outputs(src, dst)
    msg = out
    if run_out:
        msg = f"{msg}\n{run_out}"
    return ok_run, msg


def _split_semicolons(stmt: str) -> List[str]:
    out: List[str] = []
    cur: List[str] = []
    depth = 0
    in_single = False
    in_double = False
    for ch in stmt:
        if ch == "'" and not in_double:
            in_single = not in_single
        elif ch == '"' and not in_single:
            in_double = not in_double
        elif not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")" and depth > 0:
                depth -= 1
            elif ch == ";" and depth == 0:
                part = "".join(cur).strip()
                if part:
                    out.append(part)
                cur = []
                continue
        cur.append(ch)
    tail = "".join(cur).strip()
    if tail:
        out.append(tail)
    return out


def _normalize_semantic_text(text: str) -> List[str]:
    lines = text.splitlines()
    out: List[str] = []
    acc = ""
    for raw in lines:
        code = fscan.strip_comment(raw).rstrip()
        if not code:
            continue
        code = code.lower()
        if code.startswith("&"):
            code = code[1:].lstrip()
        cont = code.endswith("&")
        if cont:
            code = code[:-1].rstrip()
        if acc:
            acc = f"{acc} {code}".strip()
        else:
            acc = code.strip()
        if cont:
            continue
        for stmt in _split_semicolons(acc):
            stmt_n = re.sub(r"\s+", " ", stmt).strip()
            if stmt_n:
                out.append(stmt_n)
        acc = ""
    if acc:
        stmt_n = re.sub(r"\s+", " ", acc).strip()
        if stmt_n:
            out.append(stmt_n)
    return out


def semantic_compare_fallback(src: Path, dst: Path) -> bool:
    a = src.read_text(encoding="utf-8", errors="ignore")
    b = dst.read_text(encoding="utf-8", errors="ignore")
    return _normalize_semantic_text(a) == _normalize_semantic_text(b)


def run_compare_outputs(src: Path, dst: Path) -> Tuple[bool, str]:
    """Fallback equivalence check: compile+run both and compare outputs."""
    exe_src = src.with_suffix(".roundtrip_src.exe")
    exe_dst = dst.with_suffix(".roundtrip_dst.exe")

    ok, out = run_cmd(["gfortran", str(src), "-o", str(exe_src)])
    if not ok:
        return False, f"compile fallback failed for source\n{out}"
    ok, out2 = run_cmd(["gfortran", str(dst), "-o", str(exe_dst)])
    if not ok:
        return False, f"compile fallback failed for roundtrip\n{out2}"
    ok, run_src = run_cmd([str(exe_src)])
    if not ok:
        return False, f"run fallback failed for source\n{run_src}"
    ok, run_dst = run_cmd([str(exe_dst)])
    if not ok:
        return False, f"run fallback failed for roundtrip\n{run_dst}"
    return _outputs_match(run_src, run_dst), ""


def run_direction(
    src: Path,
    direction: Direction,
    work_dir: Path,
    *,
    inline: bool,
    no_annotate: bool,
    verbose: bool,
) -> RoundtripResult:
    stem = src.stem
    step1 = work_dir / f"{stem}.{direction}.step1.f90"
    step2 = work_dir / f"{stem}.{direction}.step2.f90"

    if direction == "array-loop":
        first = "xarray.py"
        second = "xto_loop.py"
    else:
        first = "xto_loop.py"
        second = "xarray.py"

    ok1, out1 = run_tool(
        first, src, step1, inline=inline, no_annotate=no_annotate, verbose=verbose
    )
    if not ok1:
        return RoundtripResult(
            source=src,
            direction=direction,
            ok_tools=False,
            ok_semantic=None,
            step1=None,
            step2=None,
            out_final=None,
            error=f"{first} failed\n{out1}",
        )

    ok2, out2 = run_tool(
        second, step1, step2, inline=inline, no_annotate=no_annotate, verbose=verbose
    )
    if not ok2:
        return RoundtripResult(
            source=src,
            direction=direction,
            ok_tools=False,
            ok_semantic=None,
            step1=step1,
            step2=step2,
            out_final=step2,
            error=f"{second} failed\n{out2}",
        )

    ok_sem, out_sem = semantic_compare(src, step2, verbose=verbose)
    return RoundtripResult(
        source=src,
        direction=direction,
        ok_tools=True,
        ok_semantic=ok_sem,
        step1=step1,
        step2=step2,
        out_final=step2,
        error="" if ok_sem else out_sem,
    )


def tee_file(path: Path, label: str) -> None:
    if not path.exists():
        return
    print(f"--- {label}: {fscan.display_path(path)} ---")
    print(path.read_text(encoding="utf-8", errors="ignore"))


def main() -> int:
    ap = argparse.ArgumentParser(description="Round-trip semantic tester for xarray.py and xto_loop.py")
    ap.add_argument("fortran_files", nargs="+", type=Path)
    ap.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    ap.add_argument("--limit", type=int, help="Maximum number of source files to process")
    ap.add_argument(
        "--direction",
        choices=("array-loop", "loop-array", "both"),
        default="both",
        help="Round-trip direction(s) to test",
    )
    ap.add_argument(
        "--work-dir",
        type=Path,
        default=Path("roundtrip_tmp"),
        help="Directory for intermediate/generated files",
    )
    ap.add_argument(
        "--inline",
        action="store_true",
        help="Pass --inline to xarray.py (off by default for safer round-trip checks)",
    )
    ap.add_argument(
        "--no-inline",
        action="store_true",
        help=argparse.SUPPRESS,
    )
    ap.add_argument(
        "--annotate",
        action="store_true",
        help="Allow tool annotations (default passes --no-annotate)",
    )
    ap.add_argument("-tee", "--tee", action="store_true", help="Print step1 and step2 source files")
    ap.add_argument("--tee-all", action="store_true", help="Print original, step1, and step2 source files")
    ap.add_argument("--verbose", action="store_true", help="Print tool commands")
    args = ap.parse_args()

    files = choose_files(args.fortran_files, args.exclude)
    if args.limit is not None:
        if args.limit < 1:
            print("--limit must be >= 1")
            return 2
        files = files[: args.limit]
    if not files:
        print("No source files remain after applying filters.")
        return 2

    args.work_dir.mkdir(parents=True, exist_ok=True)

    directions: List[Direction]
    if args.direction == "both":
        directions = ["array-loop", "loop-array"]
    else:
        directions = [args.direction]

    results: List[RoundtripResult] = []
    for src in files:
        for d in directions:
            res = run_direction(
                src,
                d,
                args.work_dir,
                inline=(args.inline and not args.no_inline),
                no_annotate=not args.annotate,
                verbose=args.verbose,
            )
            results.append(res)
            status = "PASS" if (res.ok_tools and res.ok_semantic) else "FAIL"
            print(f"{fscan.display_path(src)} [{d}] {status}")
            if args.tee_all:
                tee_file(src, "original")
            if args.tee or args.tee_all:
                if res.step1 is not None:
                    tee_file(res.step1, "step1")
                if res.step2 is not None:
                    tee_file(res.step2, "step2")
            if not res.ok_tools:
                print("  tool failure")
            elif res.ok_semantic is False:
                print("  semantic mismatch")

    fails = [r for r in results if not (r.ok_tools and r.ok_semantic)]
    print(f"\nSummary: total {len(results)}, pass {len(results) - len(fails)}, fail {len(fails)}")
    if fails:
        print("\nFailures:")
        for r in fails:
            print(f"- {fscan.display_path(r.source)} [{r.direction}]")
            if r.out_final is not None:
                print(f"  final: {fscan.display_path(r.out_final)}")
            if r.error:
                err = r.error.strip().splitlines()
                preview = "\n".join(err[:8])
                print(preview)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
