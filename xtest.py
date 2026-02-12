#!/usr/bin/env python3
"""Regression harness for Fortran transform commands."""

from __future__ import annotations

import argparse
from datetime import datetime, timezone
import glob
import json
import shlex
import shutil
import subprocess
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Optional, Tuple


def load_codes(path: Path) -> List[Path]:
    """Load source file paths from a text list (one path per line)."""
    out: List[Path] = []
    for raw in path.read_text(encoding="utf-8", errors="ignore").splitlines():
        line = raw.strip()
        if not line or line.startswith("#"):
            continue
        out.append(Path(line))
    return out


def expand_inputs(specs: List[str]) -> List[Path]:
    """Expand explicit files/globs into a de-duplicated source list."""
    out: List[Path] = []
    seen = set()
    for spec in specs:
        matches = glob.glob(spec, recursive=True)
        if matches:
            for m in matches:
                p = Path(m)
                key = normalize_path_text(p).lower()
                if key not in seen:
                    seen.add(key)
                    out.append(p)
            continue
        p = Path(spec)
        key = normalize_path_text(p).lower()
        if key not in seen:
            seen.add(key)
            out.append(p)
    out.sort(key=lambda p: normalize_path_text(p).lower())
    return out


def command_from_template(template: str, file_path: Path) -> List[str]:
    """Build argv from a command template and target file path."""
    if "{file}" in template:
        cmd = template.replace("{file}", str(file_path))
        return shlex.split(cmd, posix=False)

    argv = shlex.split(template, posix=False)
    file_token = str(file_path).lower()
    if not any(tok.lower() == file_token for tok in argv):
        argv.append(str(file_path))
    return argv


@dataclass
class CmdResult:
    """Result of running an external command."""

    returncode: int
    stdout: str
    stderr: str
    timed_out: bool = False


def run_cmd(argv: List[str], timeout: Optional[float] = None) -> CmdResult:
    """Run one command and capture stdout/stderr text."""
    try:
        proc = subprocess.run(argv, capture_output=True, text=True, timeout=timeout)
        return CmdResult(proc.returncode, proc.stdout or "", proc.stderr or "", timed_out=False)
    except subprocess.TimeoutExpired as exc:
        out = exc.stdout if isinstance(exc.stdout, str) else ""
        err = exc.stderr if isinstance(exc.stderr, str) else ""
        msg = f"Command timed out after {timeout} second(s): {' '.join(argv)}"
        if err:
            err = err.rstrip() + "\n" + msg + "\n"
        else:
            err = msg + "\n"
        return CmdResult(124, out, err, timed_out=True)


def show_output(prefix: str, proc: CmdResult) -> None:
    """Print command output in a compact labeled form."""
    if proc.stdout.strip():
        print(f"{prefix} stdout:")
        print(proc.stdout.rstrip())
    if proc.stderr.strip():
        print(f"{prefix} stderr:")
        print(proc.stderr.rstrip())


def normalize_path_text(path: Path) -> str:
    """Return a normalized absolute path string for stable state comparisons."""
    try:
        return str(path.resolve())
    except OSError:
        return str(path)


def load_state(path: Path) -> Dict[str, object]:
    """Load state JSON if present, else return empty dict."""
    if not path.exists():
        return {}
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except Exception:
        return {}


def save_state(path: Path, state: Dict[str, object]) -> None:
    """Write harness state JSON atomically-ish."""
    path.write_text(json.dumps(state, indent=2, sort_keys=True), encoding="utf-8")


def find_index_by_path(sources: List[Path], path_text: str) -> Optional[int]:
    """Find index of a source path by normalized absolute path text."""
    want = normalize_path_text(Path(path_text))
    for i, src in enumerate(sources):
        if normalize_path_text(src) == want:
            return i
    return None


def write_fail_log(
    path: Path,
    *,
    failure_kind: str,
    source: Path,
    temp: Path,
    before_copy: Path,
    command: List[str],
    proc: CmdResult,
) -> None:
    """Write detailed failure context for copy/paste debugging."""
    ts = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
    lines = [
        f"timestamp_utc: {ts}",
        f"failure_kind: {failure_kind}",
        f"source: {normalize_path_text(source)}",
        f"temp: {normalize_path_text(temp)}",
        f"pre_transform_snapshot: {normalize_path_text(before_copy)}",
        f"command: {' '.join(command)}",
        f"return_code: {proc.returncode}",
        "",
        "stderr:",
        proc.stderr.rstrip(),
        "",
        "stdout:",
        proc.stdout.rstrip(),
        "",
    ]
    path.write_text("\n".join(lines), encoding="utf-8")


def wait_for_signal_file(path: Path, poll_seconds: float = 2.0) -> None:
    """Wait until signal file is created/modified, then clear it."""
    prior_exists = path.exists()
    prior_mtime = path.stat().st_mtime if prior_exists else None
    print(f"Waiting for signal file update: {path}")
    print("Touch/create the file after fixing the issue to continue...")
    while True:
        now_exists = path.exists()
        if now_exists:
            now_mtime = path.stat().st_mtime
            if (not prior_exists) or (prior_mtime is None) or (now_mtime > prior_mtime):
                try:
                    path.unlink()
                except OSError:
                    pass
                print("Signal received; continuing.")
                return
        time.sleep(poll_seconds)


def parse_ok_codes(spec: str) -> List[int]:
    """Parse comma-separated integer exit codes."""
    out: List[int] = []
    for tok in spec.split(","):
        t = tok.strip()
        if not t:
            continue
        out.append(int(t))
    if not out:
        raise ValueError("empty ok-code list")
    return sorted(set(out))


def default_transform_ok_codes(transform_cmd: str) -> List[int]:
    """Return default accepted transform exit codes for known tools."""
    cmd_l = transform_cmd.lower()
    if "xpure.py" in cmd_l:
        return [0, 1]
    return [0]


def default_transform_timeout(transform_cmd: str) -> Optional[float]:
    """Return default timeout (seconds) for known transforms, else None."""
    cmd_l = transform_cmd.lower()
    if "xpure.py" in cmd_l:
        return 120.0
    return None


def run_once(args: argparse.Namespace, *, resume: bool) -> int:
    """Run one full pass over the source list and return process-style exit code."""
    source_desc = ""
    if args.inputs:
        sources = expand_inputs(args.inputs)
        source_desc = f"command-line inputs ({len(args.inputs)} pattern(s))"
    else:
        if not args.codes.exists():
            print(f"Codes list not found: {args.codes}")
            return 2
        sources = load_codes(args.codes)
        source_desc = str(args.codes)
    if not sources:
        if args.inputs:
            print("No source entries found from command-line inputs.")
        else:
            print(f"No source entries found in {args.codes}")
        return 2

    start_idx = 0
    state = load_state(args.state_file)
    if resume:
        status = str(state.get("status", ""))
        failed_source = str(state.get("failed_source", ""))
        next_index = state.get("next_index")

        if failed_source:
            idx = find_index_by_path(sources, failed_source)
            if idx is not None:
                start_idx = idx
        elif isinstance(next_index, int) and 0 <= next_index < len(sources):
            start_idx = next_index

        if start_idx > 0:
            print(f"Resuming from entry {start_idx + 1}: {sources[start_idx]}")
        elif status:
            print("Resume requested but no matching prior failing file found; starting at first entry.")

    print(f"Loaded {len(sources)} code path(s) from {source_desc}")
    tested = 0
    baseline_failed = 0
    processed_entries = 0
    limit_reached = False

    for idx, src in enumerate(sources[start_idx:], start=start_idx):
        if args.limit is not None and processed_entries >= args.limit:
            limit_reached = True
            break
        processed_entries += 1
        if not src.exists():
            print(f"\nSkipping missing source: {src}")
            save_state(
                args.state_file,
                {
                    "status": "skipped_missing",
                    "codes_file": normalize_path_text(args.codes) if not args.inputs else "",
                    "input_specs": list(args.inputs) if args.inputs else [],
                    "compile_cmd": args.compile_cmd,
                    "transform_cmd": args.transform_cmd,
                    "last_source": normalize_path_text(src),
                    "next_index": idx + 1,
                },
            )
            continue

        tested += 1
        print(f"\n[{tested}] Testing {src}")
        shutil.copy2(src, args.temp)

        base_cmd = command_from_template(args.compile_cmd, args.temp)
        base = run_cmd(base_cmd, timeout=args.compile_timeout)
        if base.returncode != 0:
            baseline_failed += 1
            print("Baseline compile failed, moving to next file.")
            if base.timed_out:
                print("Baseline compile timed out, moving to next file.")
            show_output("baseline", base)
            save_state(
                args.state_file,
                {
                    "status": "baseline_fail",
                    "codes_file": normalize_path_text(args.codes) if not args.inputs else "",
                    "input_specs": list(args.inputs) if args.inputs else [],
                    "compile_cmd": args.compile_cmd,
                    "transform_cmd": args.transform_cmd,
                    "failed_source": normalize_path_text(src),
                    "last_source": normalize_path_text(src),
                    "next_index": idx + 1,
                },
            )
            continue

        shutil.copy2(args.temp, args.before_copy)

        transform_cmd = command_from_template(args.transform_cmd, args.temp)
        tx = run_cmd(transform_cmd, timeout=args.transform_timeout)
        if tx.returncode not in args.transform_ok_codes:
            print("Transform command failed. Stopping.")
            print("Source retained at:", args.temp)
            print("Pre-transform snapshot:", args.before_copy)
            if tx.timed_out:
                print("Transform command timed out.")
            show_output("transform", tx)
            save_state(
                args.state_file,
                {
                    "status": "transform_fail",
                    "codes_file": normalize_path_text(args.codes) if not args.inputs else "",
                    "input_specs": list(args.inputs) if args.inputs else [],
                    "compile_cmd": args.compile_cmd,
                    "transform_cmd": args.transform_cmd,
                    "transform_ok_codes": args.transform_ok_codes,
                    "failed_source": normalize_path_text(src),
                    "last_source": normalize_path_text(src),
                    "next_index": idx,
                },
            )
            if args.fail_log:
                write_fail_log(
                    args.fail_log,
                    failure_kind="transform_fail",
                    source=src,
                    temp=args.temp,
                    before_copy=args.before_copy,
                    command=transform_cmd,
                    proc=tx,
                )
                print(f"Failure log written: {args.fail_log}")
            return 3

        post_cmd = command_from_template(args.compile_cmd, args.temp)
        post = run_cmd(post_cmd, timeout=args.compile_timeout)
        if post.returncode != 0:
            print("Post-transform compile failed. Stopping.")
            print("Source retained at:", args.temp)
            print("Pre-transform snapshot:", args.before_copy)
            print("Failing original source:", src)
            if post.timed_out:
                print("Post-transform compile timed out.")
            show_output("post", post)
            save_state(
                args.state_file,
                {
                    "status": "post_compile_fail",
                    "codes_file": normalize_path_text(args.codes) if not args.inputs else "",
                    "input_specs": list(args.inputs) if args.inputs else [],
                    "compile_cmd": args.compile_cmd,
                    "transform_cmd": args.transform_cmd,
                    "failed_source": normalize_path_text(src),
                    "last_source": normalize_path_text(src),
                    "next_index": idx,
                },
            )
            if args.fail_log:
                write_fail_log(
                    args.fail_log,
                    failure_kind="post_compile_fail",
                    source=src,
                    temp=args.temp,
                    before_copy=args.before_copy,
                    command=post_cmd,
                    proc=post,
                )
                print(f"Failure log written: {args.fail_log}")
            return 1

        save_state(
            args.state_file,
                {
                    "status": "ok_progress",
                    "codes_file": normalize_path_text(args.codes) if not args.inputs else "",
                    "input_specs": list(args.inputs) if args.inputs else [],
                    "compile_cmd": args.compile_cmd,
                    "transform_cmd": args.transform_cmd,
                "last_source": normalize_path_text(src),
                "next_index": idx + 1,
            },
        )

    if limit_reached:
        print(f"\nStopped after {processed_entries} file(s) due to --limit {args.limit}.")
        save_state(
            args.state_file,
            {
                "status": "limit_reached",
                "codes_file": normalize_path_text(args.codes) if not args.inputs else "",
                "input_specs": list(args.inputs) if args.inputs else [],
                "compile_cmd": args.compile_cmd,
                "transform_cmd": args.transform_cmd,
                "last_source": normalize_path_text(sources[min(start_idx + processed_entries - 1, len(sources) - 1)]),
                "next_index": min(start_idx + processed_entries, len(sources)),
            },
        )
        return 0

    print(
        f"\nDone. Tested {tested} existing file(s). "
        f"Baseline failed for {baseline_failed} file(s). No post-transform compile failures."
    )
    save_state(
        args.state_file,
        {
            "status": "ok_complete",
            "codes_file": normalize_path_text(args.codes) if not args.inputs else "",
            "input_specs": list(args.inputs) if args.inputs else [],
            "compile_cmd": args.compile_cmd,
            "transform_cmd": args.transform_cmd,
            "last_source": normalize_path_text(sources[-1]) if sources else "",
            "next_index": len(sources),
        },
    )
    return 0


def main() -> int:
    """Run baseline/transform/post-transform compile checks over a file list."""
    parser = argparse.ArgumentParser(
        description=(
            "Copy each listed Fortran file to temp file, compile baseline, run transform command, "
            "compile again, and stop on first post-transform compile failure."
        )
    )
    parser.add_argument(
        "--codes",
        type=Path,
        default=Path("codes.txt"),
        help="Path to file list (default: codes.txt)",
    )
    parser.add_argument(
        "--temp",
        type=Path,
        default=Path("temp.f90"),
        help="Temporary source file path (default: temp.f90)",
    )
    parser.add_argument(
        "--before-copy",
        type=Path,
        default=Path("temp_before_fix.f90"),
        help="Snapshot of temp source before transform (default: temp_before_fix.f90)",
    )
    parser.add_argument(
        "--compile-cmd",
        type=str,
        default="gfortran -c {file} -o temp.o",
        help="Compile command template; use {file} placeholder (default: gfortran -c {file} -o temp.o)",
    )
    parser.add_argument(
        "--transform-cmd",
        type=str,
        required=True,
        help=(
            "Transform command template; may include {file}. "
            "If omitted, temp file path is appended automatically."
        ),
    )
    parser.add_argument(
        "--transform-ok-codes",
        type=str,
        help="Comma-separated transform exit codes treated as success (tool-aware default)",
    )
    parser.add_argument(
        "--transform-timeout",
        type=float,
        help="Transform timeout in seconds (tool-aware default if omitted, use 0 for no timeout)",
    )
    parser.add_argument(
        "--compile-timeout",
        type=float,
        default=60.0,
        help="Compile timeout in seconds (default: 60, use 0 for no timeout)",
    )
    parser.add_argument(
        "--resume",
        action="store_true",
        help="Resume from first failing file recorded in prior run state",
    )
    parser.add_argument(
        "--state-file",
        type=Path,
        default=Path(".xtest_state.json"),
        help="Progress state JSON path for --resume (default: .xtest_state.json)",
    )
    parser.add_argument(
        "--loop",
        action="store_true",
        help="Repeat runs after failures (use with --resume and optionally --wait-file)",
    )
    parser.add_argument(
        "--max-loop",
        type=int,
        default=1,
        help="Maximum loop attempts in --loop mode (default: 1)",
    )
    parser.add_argument(
        "--limit",
        type=int,
        help="Maximum number of source entries to process this run (applies after --resume)",
    )
    parser.add_argument(
        "--fail-log",
        type=Path,
        default=Path("xtest_fail.log"),
        help="Path to write last stopping failure details (default: xtest_fail.log)",
    )
    parser.add_argument(
        "--wait-file",
        type=Path,
        help="In --loop mode, wait for this file to be touched/created before retrying",
    )
    parser.add_argument(
        "inputs",
        nargs="*",
        help="Optional explicit source files/glob patterns (overrides --codes list)",
    )
    args = parser.parse_args()
    if args.max_loop < 1:
        print("--max-loop must be >= 1.")
        return 2
    if args.limit is not None and args.limit < 1:
        print("--limit must be >= 1.")
        return 2
    if args.transform_ok_codes is None:
        args.transform_ok_codes = default_transform_ok_codes(args.transform_cmd)
    else:
        try:
            args.transform_ok_codes = parse_ok_codes(args.transform_ok_codes)
        except Exception:
            print("--transform-ok-codes must be a comma-separated list of integers, e.g. 0,1")
            return 2
    if args.transform_timeout is None:
        args.transform_timeout = default_transform_timeout(args.transform_cmd)
    elif args.transform_timeout <= 0:
        args.transform_timeout = None
    if args.compile_timeout is not None and args.compile_timeout <= 0:
        args.compile_timeout = None

    if not args.loop:
        return run_once(args, resume=args.resume)

    run_count = 0
    resume_flag = args.resume
    last_code = 0
    while run_count < args.max_loop:
        run_count += 1
        print(f"\n=== xtest loop {run_count}/{args.max_loop} ===")
        last_code = run_once(args, resume=resume_flag)
        if last_code == 0:
            print("Loop mode: run completed successfully.")
            return 0

        if run_count >= args.max_loop:
            print("Loop mode: max-loop reached; stopping.")
            return last_code

        if args.wait_file is not None:
            wait_for_signal_file(args.wait_file)
            resume_flag = True
            continue

        print("Loop mode: failure encountered and no --wait-file set; stopping to avoid runaway retries.")
        return last_code

    return last_code


if __name__ == "__main__":
    raise SystemExit(main())
