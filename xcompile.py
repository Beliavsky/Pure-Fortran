#!/usr/bin/env python3
"""Compile-only harness for batches of Fortran source files."""

from __future__ import annotations

import argparse
from datetime import datetime, timezone
import glob
import json
import shlex
import subprocess
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Optional


def load_codes(path: Path) -> List[Path]:
    """Load source file paths from a text list (one path per line)."""
    out: List[Path] = []
    for raw in path.read_text(encoding="utf-8", errors="ignore").splitlines():
        line = raw.strip().lstrip("\ufeff")
        if not line or line.startswith("#"):
            continue
        out.append(Path(line))
    return out


def normalize_path_text(path: Path) -> str:
    """Return a normalized absolute path string for stable state comparisons."""
    try:
        return str(path.resolve())
    except OSError:
        return str(path)


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


def load_state(path: Path) -> Dict[str, object]:
    """Load state JSON if present, else return empty dict."""
    if not path.exists():
        return {}
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except Exception:
        return {}


def save_state(path: Path, state: Dict[str, object]) -> None:
    """Write harness state JSON."""
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
    source: Path,
    command: List[str],
    proc: CmdResult,
) -> None:
    """Write detailed failure context for copy/paste debugging."""
    ts = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
    lines = [
        f"timestamp_utc: {ts}",
        "failure_kind: compile_fail",
        f"source: {normalize_path_text(source)}",
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
    compiled_ok = 0
    compile_failed = 0
    missing = 0
    limit_reached = False
    first_failed_source: Optional[str] = None

    for idx, src in enumerate(sources[start_idx:], start=start_idx):
        if args.limit is not None and tested >= args.limit:
            limit_reached = True
            break

        if not src.exists():
            print(f"\nSkipping missing source: {src}")
            missing += 1
            save_state(
                args.state_file,
                {
                    "status": "missing_source",
                    "codes_file": normalize_path_text(args.codes) if not args.inputs else "",
                    "compile_cmd": args.compile_cmd,
                    "source_count": len(sources),
                    "tested": tested,
                    "compiled_ok": compiled_ok,
                    "compile_failed": compile_failed,
                    "missing": missing,
                    "next_index": idx + 1,
                    "failed_source": "",
                    "last_source": normalize_path_text(src),
                },
            )
            continue

        tested += 1
        print(f"\n[{tested}] Compiling {src}")
        cmd = command_from_template(args.compile_cmd, src)
        res = run_cmd(cmd, timeout=args.compile_timeout)
        if res.returncode != 0:
            compile_failed += 1
            if first_failed_source is None:
                first_failed_source = normalize_path_text(src)
            print("Compile failed.")
            if res.timed_out:
                print("Compile timed out.")
            show_output("compile", res)
            write_fail_log(args.fail_log, source=src, command=cmd, proc=res)
            print(f"Failure log written: {args.fail_log}")
            if args.fast_fail:
                print("Stopping due to --fast-fail.")
                save_state(
                    args.state_file,
                    {
                        "status": "compile_fail",
                        "codes_file": normalize_path_text(args.codes) if not args.inputs else "",
                        "compile_cmd": args.compile_cmd,
                        "source_count": len(sources),
                        "tested": tested,
                        "compiled_ok": compiled_ok,
                        "compile_failed": compile_failed,
                        "missing": missing,
                        "next_index": idx,
                        "failed_source": normalize_path_text(src),
                        "last_source": normalize_path_text(src),
                    },
                )
                return 1
            save_state(
                args.state_file,
                {
                    "status": "compile_fail_continue",
                    "codes_file": normalize_path_text(args.codes) if not args.inputs else "",
                    "compile_cmd": args.compile_cmd,
                    "source_count": len(sources),
                    "tested": tested,
                    "compiled_ok": compiled_ok,
                    "compile_failed": compile_failed,
                    "missing": missing,
                    "next_index": idx + 1,
                    "failed_source": first_failed_source or "",
                    "last_source": normalize_path_text(src),
                },
            )
            continue

        compiled_ok += 1
        save_state(
            args.state_file,
            {
                "status": "ok",
                "codes_file": normalize_path_text(args.codes) if not args.inputs else "",
                "compile_cmd": args.compile_cmd,
                "source_count": len(sources),
                "tested": tested,
                "compiled_ok": compiled_ok,
                "compile_failed": compile_failed,
                "missing": missing,
                "next_index": idx + 1,
                "failed_source": "",
                "last_source": normalize_path_text(src),
            },
        )

    if limit_reached:
        print(f"\nStopped after {tested} file(s) due to --limit {args.limit}.")
        print(f"Chunk summary: compiled OK {compiled_ok}, failed {compile_failed}, missing {missing}")
        if missing:
            print(f"Skipped missing: {missing}")
        save_state(
            args.state_file,
            {
                "status": "limit_reached",
                "codes_file": normalize_path_text(args.codes) if not args.inputs else "",
                "compile_cmd": args.compile_cmd,
                "source_count": len(sources),
                "tested": tested,
                "compiled_ok": compiled_ok,
                "compile_failed": compile_failed,
                "missing": missing,
                "next_index": start_idx + tested + missing,
                "failed_source": first_failed_source or "",
                "last_source": normalize_path_text(sources[min(start_idx + tested + missing - 1, len(sources) - 1)]),
            },
        )
        return 1 if compile_failed > 0 else 0

    if compile_failed == 0:
        print(f"\nSuccess: compiled {compiled_ok} file(s) without compile failures.")
    else:
        print(
            f"\nCompleted with failures: compiled {compiled_ok} file(s), "
            f"failed {compile_failed} file(s)."
        )
    print(f"Run summary: tested {tested}, compiled OK {compiled_ok}, failed {compile_failed}, missing {missing}")
    if missing:
        print(f"Skipped missing: {missing}")
    save_state(
        args.state_file,
        {
            "status": "completed_with_failures" if compile_failed > 0 else "completed",
            "codes_file": normalize_path_text(args.codes) if not args.inputs else "",
            "compile_cmd": args.compile_cmd,
            "source_count": len(sources),
            "tested": tested,
            "compiled_ok": compiled_ok,
            "compile_failed": compile_failed,
            "missing": missing,
            "next_index": len(sources),
            "failed_source": first_failed_source or "",
            "last_source": normalize_path_text(sources[-1]),
        },
    )
    return 1 if compile_failed > 0 else 0


def main() -> int:
    """Compile Fortran file lists with optional fast-fail mode."""
    parser = argparse.ArgumentParser(
        description="Compile each listed Fortran file; continue by default, or stop on first failure with --fast-fail.",
        allow_abbrev=False,
    )
    parser.add_argument(
        "--codes",
        type=Path,
        default=Path("codes.txt"),
        help="Path to file list (default: codes.txt)",
    )
    parser.add_argument(
        "--compile-cmd",
        type=str,
        default="gfortran -c {file}",
        help="Compile command template; use {file} placeholder (default: gfortran -c {file})",
    )
    parser.add_argument(
        "--compile-timeout",
        type=float,
        default=None,
        help="Timeout in seconds for each compile command (default: no timeout)",
    )
    parser.add_argument(
        "--fast-fail",
        action="store_true",
        help="Stop on first compile failure (default is continue through all files)",
    )
    parser.add_argument(
        "--limit",
        type=int,
        help="Maximum number of files to process in this run (applies after --resume)",
    )
    parser.add_argument(
        "--resume",
        action="store_true",
        help="Resume from first failing file recorded in prior run state",
    )
    parser.add_argument(
        "--state-file",
        type=Path,
        default=Path(".xcompile_state.json"),
        help="Progress state JSON path for --resume (default: .xcompile_state.json)",
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
        "--fail-log",
        type=Path,
        default=Path("xcompile_fail.log"),
        help="Path to failure log (default: xcompile_fail.log)",
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
    if args.compile_timeout is not None and args.compile_timeout <= 0:
        args.compile_timeout = None

    if not args.loop:
        return run_once(args, resume=args.resume)

    run_count = 0
    resume_flag = args.resume
    last_code = 1
    while run_count < args.max_loop:
        run_count += 1
        print(f"\n=== xcompile loop {run_count}/{args.max_loop} ===")
        last_code = run_once(args, resume=resume_flag)
        if last_code == 0:
            print("Loop mode: completed successfully.")
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
