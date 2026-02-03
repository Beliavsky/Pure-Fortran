#!/usr/bin/env python3
"""Shared build/compile helpers for Fortran tooling."""

from __future__ import annotations

import subprocess
from pathlib import Path
from typing import Callable, List, Optional, Tuple


def quote_cmd_arg(arg: str) -> str:
    return subprocess.list2cmdline([arg])


def run_compiler_command(
    command: str,
    files: List[Path],
    phase: str,
    display_path: Callable[[Path], str],
) -> bool:
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
        print(proc.stderr.rstrip())
    return False


def rollback_backups(
    backup_pairs: List[Tuple[Path, Path]],
    display_path: Callable[[Path], str],
) -> None:
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
