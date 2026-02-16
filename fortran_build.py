#!/usr/bin/env python3
"""Shared build/compile helpers for Fortran tooling."""

from __future__ import annotations

import subprocess
import re
from pathlib import Path
from typing import Callable, List, Optional, Tuple


def quote_cmd_arg(arg: str) -> str:
    """Quote one shell argument for Windows-safe command composition."""
    return subprocess.list2cmdline([arg])


def run_compiler_command(
    command: str,
    files: List[Path],
    phase: str,
    display_path: Callable[[Path], str],
) -> bool:
    """Run a compile command for the given files and print pass/fail output."""
    file_args = " ".join(quote_cmd_arg(str(p)) for p in files)
    file_args_display = " ".join(quote_cmd_arg(display_path(p)) for p in files)
    if "{files}" in command:
        cmd = command.replace("{files}", file_args)
        cmd_display = command.replace("{files}", file_args_display)
    else:
        cmd = f"{command} {file_args}".strip()
        cmd_display = f"{command} {file_args_display}".strip()

    print(f"Compile ({phase}): {cmd_display}")
    proc = subprocess.run(cmd, shell=True, capture_output=True, text=True)
    if proc.returncode == 0:
        print(f"Compile ({phase}): PASS")
        return True

    print(f"Compile ({phase}): FAIL (exit {proc.returncode})")
    if proc.stdout:
        print(proc.stdout.rstrip())
    if proc.stderr:
        print(format_linker_stderr(proc.stderr).rstrip())
    return False


def compile_and_run_source(
    source: Path,
    *,
    label: str,
    quiet_run: bool = False,
    keep_exe: bool = False,
    exe_path: Optional[Path] = None,
) -> Tuple[bool, str, str]:
    """Compile one Fortran source with gfortran, run it, and return (ok, stdout, stderr)."""
    exe = exe_path if exe_path is not None else Path(f"{source.stem}.exe")
    compile_cmd = ["gfortran", str(source), "-o", str(exe)]
    print(f"Build ({label}): {' '.join(quote_cmd_arg(x) for x in compile_cmd)}")
    cp = subprocess.run(compile_cmd, capture_output=True, text=True)
    if cp.returncode != 0:
        print(f"Build ({label}): FAIL (exit {cp.returncode})")
        if cp.stdout:
            print(cp.stdout.rstrip())
        if cp.stderr:
            print(format_linker_stderr(cp.stderr).rstrip())
        return False, "", ""
    print(f"Build ({label}): PASS")
    print(f"Run ({label}): {quote_cmd_arg(str(exe))}")
    rp = subprocess.run([str(exe)], capture_output=True, text=True)
    try:
        if rp.returncode != 0:
            print(f"Run ({label}): FAIL (exit {rp.returncode})")
            if rp.stdout:
                print(rp.stdout.rstrip())
            if rp.stderr:
                print(rp.stderr.rstrip())
            return False, rp.stdout or "", rp.stderr or ""
        print(f"Run ({label}): PASS")
        if not quiet_run:
            if rp.stdout:
                print(rp.stdout.rstrip())
            if rp.stderr:
                print(rp.stderr.rstrip())
        return True, rp.stdout or "", rp.stderr or ""
    finally:
        if not keep_exe:
            try:
                exe.unlink(missing_ok=True)
            except Exception:
                pass


def format_linker_stderr(stderr: str) -> str:
    """Compact very long linker diagnostics while keeping key symbols."""
    lines = stderr.splitlines()
    out: List[str] = []
    undef_re = re.compile(r"undefined reference to [`']([^`']+)[`']", re.IGNORECASE)
    for ln in lines:
        m = undef_re.search(ln)
        if m:
            out.append(f"undefined reference to '{m.group(1)}'")
        else:
            out.append(ln)
    return "\n".join(out)


def rollback_backups(
    backup_pairs: List[Tuple[Path, Path]],
    display_path: Callable[[Path], str],
) -> None:
    """Restore original files from backup paths after a failed transformation."""
    if not backup_pairs:
        return
    print("Rolling back modified files from backups...")
    for original, backup in backup_pairs:
        if backup.exists():
            import shutil

            shutil.copy2(backup, original)
            print(f"Restored: {display_path(original)}")


def git_commit_files(
    files: List[Path],
    message: str,
    display_path: Callable[[Path], str],
) -> bool:
    """Stage selected files and create a git commit with the provided message."""
    if not files:
        return False
    uniq = []
    seen = set()
    for f in files:
        k = str(f.resolve()).lower()
        if k in seen:
            continue
        seen.add(k)
        uniq.append(f)

    inside = subprocess.run(
        ["git", "rev-parse", "--is-inside-work-tree"],
        capture_output=True,
        text=True,
    )
    if inside.returncode != 0 or inside.stdout.strip().lower() != "true":
        print("Skipping --git: not inside a git work tree.")
        return False

    add_cmd = ["git", "add", "--"] + [str(p) for p in uniq]
    add_proc = subprocess.run(add_cmd, capture_output=True, text=True)
    if add_proc.returncode != 0:
        print("Skipping --git: git add failed.")
        if add_proc.stderr:
            print(add_proc.stderr.rstrip())
        return False

    staged_check = subprocess.run(
        ["git", "diff", "--cached", "--quiet", "--"] + [str(p) for p in uniq]
    )
    if staged_check.returncode == 0:
        print("Skipping --git: no staged changes for selected files.")
        return False

    commit_proc = subprocess.run(
        ["git", "commit", "-m", message],
        capture_output=True,
        text=True,
    )
    if commit_proc.returncode != 0:
        print("Skipping --git: git commit failed.")
        if commit_proc.stderr:
            print(commit_proc.stderr.rstrip())
        return False

    print(f"Created git commit: {message}")
    if commit_proc.stdout:
        # Print first non-empty summary line.
        for line in commit_proc.stdout.splitlines():
            if line.strip():
                print(line.strip())
                break
    return True
