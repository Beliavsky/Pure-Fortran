#!/usr/bin/env python3
"""Build Fortran executable sets listed one set per line."""

from __future__ import annotations

import argparse
import json
import shlex
import subprocess
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Optional

SILENT_PROGRESS_PER_LINE = 20


@dataclass
class CmdResult:
    returncode: int
    stdout: str
    stderr: str
    timed_out: bool = False


def load_code_sets(path: Path) -> List[List[Path]]:
    out: List[List[Path]] = []
    for raw in path.read_text(encoding="utf-8", errors="ignore").splitlines():
        line = raw.strip()
        if not line or line.startswith("#"):
            continue
        toks = shlex.split(line, posix=False)
        if not toks:
            continue
        out.append([Path(tok) for tok in toks])
    return out


def resolve_sets(groups: List[List[Path]], code_dir: Optional[Path]) -> List[List[Path]]:
    if code_dir is None:
        return groups
    out: List[List[Path]] = []
    for group in groups:
        out.append([p if p.is_absolute() else (code_dir / p) for p in group])
    return out


def load_state(path: Path) -> Dict[str, object]:
    if not path.exists():
        return {}
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except Exception:
        return {}


def save_state(path: Path, state: Dict[str, object]) -> None:
    path.write_text(json.dumps(state, indent=2, sort_keys=True), encoding="utf-8")


def build_cmd(template: str, files: List[Path], exe_path: Path) -> List[str]:
    argv = shlex.split(template, posix=False)
    out: List[str] = []
    seen_files = False
    seen_file = False
    seen_exe = False

    for tok in argv:
        if tok == "{files}":
            out.extend(str(p) for p in files)
            seen_files = True
            continue
        if tok == "{file}":
            out.append(str(files[0]))
            seen_file = True
            continue
        if tok == "{exe}":
            out.append(str(exe_path))
            seen_exe = True
            continue
        if "{file}" in tok:
            out.append(tok.replace("{file}", str(files[0])))
            seen_file = True
            continue
        if "{exe}" in tok:
            out.append(tok.replace("{exe}", str(exe_path)))
            seen_exe = True
            continue
        out.append(tok)

    if not seen_files and not seen_file:
        out.extend(str(p) for p in files)

    has_o = any(tok == "-o" for tok in out)
    if not seen_exe and not has_o:
        out.extend(["-o", str(exe_path)])
    return out


def run_cmd(argv: List[str], timeout: Optional[float]) -> CmdResult:
    try:
        cp = subprocess.run(argv, capture_output=True, text=True, errors="replace", timeout=timeout)
        return CmdResult(cp.returncode, cp.stdout or "", cp.stderr or "", timed_out=False)
    except subprocess.TimeoutExpired as exc:
        out = exc.stdout if isinstance(exc.stdout, str) else ""
        err = exc.stderr if isinstance(exc.stderr, str) else ""
        msg = f"Command timed out after {timeout} second(s): {' '.join(argv)}"
        err = (err.rstrip() + "\n" if err else "") + msg + "\n"
        return CmdResult(124, out, err, timed_out=True)


def write_failure_log(path: Path, index: int, files: List[Path], cmd: List[str], res: CmdResult) -> None:
    lines = [
        f"index: {index}",
        f"files: {' '.join(str(p) for p in files)}",
        f"command: {' '.join(cmd)}",
        f"returncode: {res.returncode}",
        "",
        "stderr:",
        res.stderr.rstrip(),
        "",
        "stdout:",
        res.stdout.rstrip(),
        "",
    ]
    path.write_text("\n".join(lines), encoding="utf-8")


def main() -> int:
    ap = argparse.ArgumentParser(description="Build executable for each source-file set in --codes.")
    ap.add_argument("--codes", type=Path, required=True, help="Text file: one source set per line.")
    ap.add_argument("--code-dir", type=Path, default=None, help="Base directory for relative paths in --codes.")
    ap.add_argument(
        "--compile-cmd",
        type=str,
        default="gfortran -w -Wfatal-errors {files}",
        help='Build command template; supports {files}, {file}, {exe}.',
    )
    ap.add_argument("--exe-dir", type=Path, default=Path("build_exe"), help="Directory for produced executables.")
    ap.add_argument("--state-file", type=Path, default=Path("xbuild_state.json"), help="State file for --resume.")
    ap.add_argument("--fail-log", type=Path, default=Path("xbuild_fail.log"), help="Failure log path.")
    ap.add_argument("--timeout", type=float, default=None, help="Per-build timeout in seconds.")
    ap.add_argument("--resume", action="store_true", help="Resume from --state-file.")
    ap.add_argument("--silent", action="store_true", help="Print compact progress; details only on failures.")
    ap.add_argument("--maxfail", type=int, default=1, help="Stop after this many failures (0 = no limit).")
    args = ap.parse_args()

    if not args.codes.exists():
        print(f"Codes list not found: {args.codes}")
        return 2

    sets = resolve_sets(load_code_sets(args.codes), args.code_dir)
    if not sets:
        print(f"No source sets loaded from {args.codes}")
        return 2

    args.exe_dir.mkdir(parents=True, exist_ok=True)
    start_idx = 0
    if args.resume:
        st = load_state(args.state_file)
        if isinstance(st.get("last_index"), int):
            start_idx = max(0, int(st["last_index"]) + 1)

    failures = 0
    silent_col = 0
    for i in range(start_idx, len(sets)):
        idx1 = i + 1
        files = sets[i]
        files_str = " ".join(str(p) for p in files)
        if not args.silent:
            print(f"[{idx1}] Building {files_str}")
        else:
            print(f"{idx1}", end=" ", flush=True)
            silent_col += 1
            if silent_col >= SILENT_PROGRESS_PER_LINE:
                print()
                silent_col = 0

        missing = [p for p in files if not p.exists()]
        if missing:
            if args.silent and silent_col:
                print()
                silent_col = 0
            print(f"[{idx1}] Missing file(s):")
            for p in missing:
                print(f"  {p}")
            failures += 1
            save_state(args.state_file, {"last_index": i, "failed": True})
            if args.maxfail > 0 and failures >= args.maxfail:
                print(f"Reached --maxfail {args.maxfail}. Stopping.")
                return 1
            continue

        exe_name = f"{files[-1].stem}.exe"
        exe_path = args.exe_dir / exe_name
        cmd = build_cmd(args.compile_cmd, files, exe_path)
        res = run_cmd(cmd, args.timeout)

        if res.returncode != 0:
            if args.silent and silent_col:
                print()
                silent_col = 0
            print(f"[{idx1}] Build failed.")
            print(f"Source set: {files_str}")
            print("build stderr:")
            if res.stderr.strip():
                print(res.stderr.rstrip())
            elif res.stdout.strip():
                print(res.stdout.rstrip())
            write_failure_log(args.fail_log, idx1, files, cmd, res)
            print(f"Failure log written: {args.fail_log}")
            failures += 1
            save_state(args.state_file, {"last_index": i, "failed": True})
            if args.maxfail > 0 and failures >= args.maxfail:
                print(f"Reached --maxfail {args.maxfail}. Stopping.")
                return 1
            continue

        save_state(args.state_file, {"last_index": i, "failed": False})

    if args.silent and silent_col:
        print()
    print(f"Completed {len(sets) - start_idx} set(s). Failures: {failures}.")
    if failures == 0:
        print("All builds passed.")
        return 0
    return 1


if __name__ == "__main__":
    raise SystemExit(main())

