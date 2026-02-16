#!/usr/bin/env python3
"""Regression harness for Fortran transform commands."""

from __future__ import annotations

import argparse
from datetime import datetime, timezone
import glob
import json
import time
import shlex
import re
import shutil
import subprocess
import difflib
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Optional, Tuple

SILENT_PROGRESS_PER_LINE = 20


def load_codes(path: Path) -> List[List[Path]]:
    """Load source entries from a text list (one or more paths per line)."""
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


def load_codes_with_lines(path: Path) -> List[Tuple[int, List[Path]]]:
    """Load source entries with original 1-based line numbers from codes file."""
    out: List[Tuple[int, List[Path]]] = []
    for lineno, raw in enumerate(path.read_text(encoding="utf-8", errors="ignore").splitlines(), start=1):
        line = raw.strip()
        if not line or line.startswith("#"):
            continue
        toks = shlex.split(line, posix=False)
        if not toks:
            continue
        out.append((lineno, [Path(tok) for tok in toks]))
    return out


def resolve_code_entries(entries: List[List[Path]], code_dir: Optional[Path]) -> List[List[Path]]:
    """Resolve relative code entries against --code-dir when provided."""
    if code_dir is None:
        return entries
    base = code_dir
    out: List[List[Path]] = []
    for group in entries:
        resolved_group: List[Path] = []
        for p in group:
            resolved_group.append(p if p.is_absolute() else (base / p))
        out.append(resolved_group)
    return out


def baseline_ok_list_path(codes_path: Path) -> Path:
    """Return baseline-ok list path derived from codes list path."""
    return codes_path.with_name(f"{codes_path.stem}_baseline_ok{codes_path.suffix}")


def load_baseline_ok(path: Path) -> Tuple[List[str], set]:
    """Load baseline-ok file preserving order and return (ordered, seen-lower)."""
    ordered: List[str] = []
    seen_lower = set()
    if not path.exists():
        return ordered, seen_lower
    for raw in path.read_text(encoding="utf-8", errors="ignore").splitlines():
        line = raw.strip()
        if not line or line.startswith("#"):
            continue
        key = line.lower()
        if key in seen_lower:
            continue
        seen_lower.add(key)
        ordered.append(line)
    return ordered, seen_lower


def write_baseline_ok(path: Path, entries: List[str]) -> None:
    """Write baseline-ok entries one path per line."""
    text = ""
    if entries:
        text = "\n".join(entries) + "\n"
    path.write_text(text, encoding="utf-8")


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


def command_from_template(template: str, file_paths: List[Path]) -> List[str]:
    """Build argv from a command template and target file set."""
    argv = shlex.split(template, posix=False)
    if not file_paths:
        return argv

    primary = file_paths[0]
    out: List[str] = []
    seen_placeholder = False
    for tok in argv:
        if tok == "{files}":
            out.extend(str(p) for p in file_paths)
            seen_placeholder = True
            continue
        if tok == "{file}":
            out.append(str(primary))
            seen_placeholder = True
            continue
        if "{file}" in tok:
            out.append(tok.replace("{file}", str(primary)))
            seen_placeholder = True
            continue
        out.append(tok)

    if not seen_placeholder:
        for p in file_paths:
            file_token = str(p).lower()
            if not any(tok.lower() == file_token for tok in out):
                out.append(str(p))
    return out


@dataclass
class CmdResult:
    """Result of running an external command."""

    returncode: int
    stdout: str
    stderr: str
    timed_out: bool = False
    elapsed_sec: float = 0.0


def run_cmd(argv: List[str], timeout: Optional[float] = None) -> CmdResult:
    """Run one command and capture stdout/stderr text."""
    t0 = time.perf_counter()
    try:
        proc = subprocess.run(argv, capture_output=True, text=True, timeout=timeout)
        return CmdResult(
            proc.returncode,
            proc.stdout or "",
            proc.stderr or "",
            timed_out=False,
            elapsed_sec=time.perf_counter() - t0,
        )
    except subprocess.TimeoutExpired as exc:
        out = exc.stdout if isinstance(exc.stdout, str) else ""
        err = exc.stderr if isinstance(exc.stderr, str) else ""
        msg = f"Command timed out after {timeout} second(s): {' '.join(argv)}"
        if err:
            err = err.rstrip() + "\n" + msg + "\n"
        else:
            err = msg + "\n"
        return CmdResult(124, out, err, timed_out=True, elapsed_sec=time.perf_counter() - t0)


def count_text_lines(text: str) -> int:
    """Return number of logical lines in text."""
    return len(text.splitlines())


def changed_original_line_count(before_text: str, after_text: str) -> int:
    """Count number of original lines touched by non-equal diff hunks."""
    a = before_text.splitlines()
    b = after_text.splitlines()
    sm = difflib.SequenceMatcher(a=a, b=b)
    changed = 0
    for tag, i1, i2, _j1, _j2 in sm.get_opcodes():
        if tag != "equal":
            changed += (i2 - i1)
    return changed


def print_lines_dataframe(rows: List[Dict[str, object]]) -> None:
    """Print per-file line metrics as a pandas DataFrame (fallback to plain text)."""
    if not rows:
        print("Line metrics: no processed files.")
        return
    cols = [
        "index",
        "source",
        "orig_lines",
        "transformed_lines",
        "delta_lines",
        "changed_orig_lines",
    ]
    try:
        import pandas as pd  # type: ignore

        df = pd.DataFrame(rows)
        for c in cols:
            if c not in df.columns:
                df[c] = None
        df = df[cols]
        print("\nLine metrics:")
        print(df.to_string(index=False))
    except Exception:
        print("\nLine metrics (pandas unavailable; plain text):")
        print("index source orig_lines transformed_lines delta_lines changed_orig_lines")
        for r in rows:
            print(
                f"{r.get('index','')} {r.get('source','')} {r.get('orig_lines','')} "
                f"{r.get('transformed_lines','')} {r.get('delta_lines','')} {r.get('changed_orig_lines','')}"
            )


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


def copy_transformed_output(
    *,
    out_dir: Path,
    temp_path: Path,
    source_paths: List[Path],
    noclobber: bool,
) -> Tuple[bool, str]:
    """Copy transformed temp output to out-dir/<source-basename>.f90."""
    if not temp_path.exists():
        return False, f"transformed temp file not found: {temp_path}"
    src0 = source_paths[0] if source_paths else Path("unknown.f90")
    dest = out_dir / src0.name
    if noclobber and dest.exists():
        return False, f"skip (noclobber): {dest}"
    out_dir.mkdir(parents=True, exist_ok=True)
    shutil.copy2(temp_path, dest)
    return True, str(dest)


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


def find_index_by_path(sources: List[List[Path]], path_text: str) -> Optional[int]:
    """Find index by matching the first path of each source entry."""
    want = normalize_path_text(Path(path_text))
    for i, srcs in enumerate(sources):
        if not srcs:
            continue
        if normalize_path_text(srcs[0]) == want:
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


def transform_cmd_requests_verbose(transform_cmd: str) -> bool:
    """Whether transform command template appears to request verbose output."""
    try:
        toks = shlex.split(transform_cmd, posix=False)
    except Exception:
        toks = transform_cmd.split()
    for tok in toks:
        t = tok.strip().lower()
        if t == "--verbose" or t.startswith("--verbose="):
            return True
    return False


def run_once(args: argparse.Namespace, *, resume: bool) -> int:
    """Run one full pass over the source list and return process-style exit code."""
    source_desc = ""
    source_line_numbers: Optional[List[int]] = None
    if args.inputs:
        sources = [[p] for p in expand_inputs(args.inputs)]
        source_desc = f"command-line inputs ({len(args.inputs)} pattern(s))"
    else:
        if not args.codes.exists():
            print(f"Codes list not found: {args.codes}")
            return 2
        entries = load_codes_with_lines(args.codes)
        source_line_numbers = [ln for ln, _grp in entries]
        sources = resolve_code_entries([grp for _ln, grp in entries], args.code_dir)
        source_desc = str(args.codes)
    orig_sources_count = len(sources)
    if not sources:
        if args.inputs:
            print("No source entries found from command-line inputs.")
        else:
            print(f"No source entries found in {args.codes}")
        return 2
    if args.reverse:
        sources = list(reversed(sources))
        if source_line_numbers is not None:
            source_line_numbers = list(reversed(source_line_numbers))
    if args.start is not None:
        start_spec = str(args.start).strip()
        if re.match(r"^\d+$", start_spec):
            start_1 = int(start_spec)
            if source_line_numbers is not None:
                start_idx_by_line = None
                for i, ln in enumerate(source_line_numbers):
                    if ln >= start_1:
                        start_idx_by_line = i
                        break
                if start_idx_by_line is None:
                    print(f"--start line {start_1} is past end of {args.codes}. Nothing to do.")
                    return 0
                sources = sources[start_idx_by_line:]
                source_line_numbers = source_line_numbers[start_idx_by_line:]
            else:
                if start_1 > len(sources):
                    print(f"--start {start_1} is past available entries ({len(sources)}). Nothing to do.")
                    return 0
                sources = sources[start_1 - 1 :]
        else:
            match_idx = None
            needle = start_spec.lower()
            for i, grp in enumerate(sources):
                if not grp:
                    continue
                first = str(grp[0])
                if first.lower().startswith(needle) or grp[0].name.lower().startswith(needle):
                    match_idx = i
                    break
            if match_idx is None:
                print(f'No codes entry starts with "{start_spec}".')
                return 2
            sources = sources[match_idx:]

    start_idx = 0
    state = load_state(args.state_file)
    if resume:
        if args.start is not None:
            if not args.silent:
                print("Resume note: --start is set; ignoring resume position from state.")
        else:
            status = str(state.get("status", ""))
            failed_source = str(state.get("failed_source", ""))
            next_index = state.get("next_index")
            filtered_run = bool(args.reverse or args.start is not None)

            if failed_source:
                idx = find_index_by_path(sources, failed_source)
                if idx is not None:
                    start_idx = idx
            elif (not filtered_run) and isinstance(next_index, int) and 0 <= next_index < len(sources):
                start_idx = next_index

            if start_idx > 0:
                if not args.silent:
                    print(f"Resuming from entry {start_idx + 1}: {' '.join(str(p) for p in sources[start_idx])}")
            elif filtered_run and failed_source:
                if not args.silent:
                    print("Resume note: prior failed source is outside current filtered selection; starting at filtered start.")
            elif status:
                if not args.silent:
                    print("Resume requested but no matching prior failing file found; starting at first entry.")

    if not args.silent:
        if len(sources) != orig_sources_count:
            print(
                f"Loaded {len(sources)} code entr{'y' if len(sources)==1 else 'ies'} from {source_desc} "
                f"(filtered from {orig_sources_count})."
            )
        else:
            print(f"Loaded {len(sources)} code entr{'y' if len(sources)==1 else 'ies'} from {source_desc}")
    tested = 0
    baseline_failed = 0
    post_failed = 0
    processed_entries = 0
    limit_reached = False
    silent_progress_open = False
    silent_progress_in_line = 0
    run_t0 = time.perf_counter()
    t_baseline = 0.0
    t_transform = 0.0
    t_post = 0.0
    n_baseline = 0
    n_transform = 0
    n_post = 0
    line_rows: List[Dict[str, object]] = []
    echo_transform_output = transform_cmd_requests_verbose(args.transform_cmd)
    baseline_ok_out: Optional[Path] = None
    baseline_ok_entries: List[str] = []
    baseline_ok_seen_lower = set()
    if not args.inputs:
        baseline_ok_out = baseline_ok_list_path(args.codes)
        baseline_ok_entries, baseline_ok_seen_lower = load_baseline_ok(baseline_ok_out)

    def flush_silent_progress_line() -> None:
        nonlocal silent_progress_open, silent_progress_in_line
        if args.silent and silent_progress_open:
            print("")
            silent_progress_open = False
            silent_progress_in_line = 0

    def show_problem_source() -> None:
        flush_silent_progress_line()
        if args.silent:
            print(f"[{tested}] {' '.join(str(p) for p in srcs)}")

    def maybe_record_baseline_ok(entry_paths: List[Path]) -> None:
        nonlocal baseline_ok_entries, baseline_ok_seen_lower
        if baseline_ok_out is None:
            return
        text = " ".join(normalize_path_text(p) for p in entry_paths)
        key = text.lower()
        if key in baseline_ok_seen_lower:
            return
        baseline_ok_seen_lower.add(key)
        baseline_ok_entries.append(text)

    def persist_baseline_ok() -> None:
        if baseline_ok_out is None:
            return
        write_baseline_ok(baseline_ok_out, baseline_ok_entries)

    def print_time_summary() -> None:
        if args.notime:
            return
        total = time.perf_counter() - run_t0
        print(
            (
                f"Time: total {total:.2f}s | "
                f"baseline {t_baseline:.2f}s ({n_baseline}) | "
                f"transform {t_transform:.2f}s ({n_transform}) | "
                f"post {t_post:.2f}s ({n_post})"
            )
        )

    def print_optional_lines_summary() -> None:
        if args.lines:
            print_lines_dataframe(line_rows)

    for idx, srcs in enumerate(sources[start_idx:], start=start_idx):
        src = srcs[0] if srcs else Path("")
        src_display = " ".join(str(p) for p in srcs)
        if args.limit is not None and processed_entries >= args.limit:
            limit_reached = True
            break
        processed_entries += 1
        if not srcs or any(not p.exists() for p in srcs):
            print(f"\nSkipping missing source entry: {src_display}")
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
        if not args.silent:
            print(f"\n[{tested}] Testing {src_display}")
        else:
            print(f"{tested} ", end="", flush=True)
            silent_progress_open = True
            silent_progress_in_line += 1
            if silent_progress_in_line >= SILENT_PROGRESS_PER_LINE:
                print("")
                silent_progress_open = False
                silent_progress_in_line = 0
        is_set = len(srcs) > 1
        if is_set:
            base_cmd = command_from_template(args.compile_cmd, srcs)
            orig_lines = 0
            orig_text = ""
        else:
            shutil.copy2(src, args.temp)
            base_cmd = command_from_template(args.compile_cmd, [args.temp])
        base = run_cmd(base_cmd, timeout=args.compile_timeout)
        t_baseline += base.elapsed_sec
        n_baseline += 1
        if not is_set:
            try:
                orig_text = src.read_text(encoding="utf-8", errors="ignore")
                orig_lines = count_text_lines(orig_text)
            except Exception:
                orig_text = ""
                orig_lines = 0
        if base.returncode != 0:
            baseline_failed += 1
            line_rows.append(
                {
                    "index": tested,
                    "source": src_display,
                    "orig_lines": orig_lines,
                    "transformed_lines": None,
                    "delta_lines": None,
                    "changed_orig_lines": None,
                }
            )
            if not args.silent:
                show_problem_source()
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
        maybe_record_baseline_ok(srcs)

        if not is_set:
            shutil.copy2(args.temp, args.before_copy)

        transform_cmd = command_from_template(args.transform_cmd, srcs if is_set else [args.temp])
        tx = run_cmd(transform_cmd, timeout=args.transform_timeout)
        t_transform += tx.elapsed_sec
        n_transform += 1
        transformed_text = ""
        transformed_lines: Optional[int] = None
        changed_orig = None
        delta_lines = None
        if not is_set:
            transformed_text = args.temp.read_text(encoding="utf-8", errors="ignore") if args.temp.exists() else ""
            transformed_lines = count_text_lines(transformed_text) if transformed_text else 0
            changed_orig = (
                changed_original_line_count(orig_text, transformed_text)
                if (orig_text and transformed_text)
                else None
            )
            delta_lines = (transformed_lines - orig_lines) if transformed_text else None
        line_rows.append(
            {
                "index": tested,
                "source": src_display,
                "orig_lines": orig_lines,
                "transformed_lines": transformed_lines,
                "delta_lines": delta_lines,
                "changed_orig_lines": changed_orig,
            }
        )
        if tx.returncode not in args.transform_ok_codes:
            if not args.silent:
                show_problem_source()
                print("Transform command failed. Stopping.")
                if not is_set:
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
                if not args.silent:
                    print(f"Failure log written: {args.fail_log}")
            persist_baseline_ok()
            print_time_summary()
            print_optional_lines_summary()
            return 3
        if echo_transform_output and (tx.stdout.strip() or tx.stderr.strip()):
            if args.silent:
                flush_silent_progress_line()
            print(f"Transform output for {src_display}:")
            show_output("transform", tx)
        if args.out_dir is not None:
            copied, msg = copy_transformed_output(
                out_dir=args.out_dir,
                temp_path=args.temp,
                source_paths=srcs,
                noclobber=args.noclobber,
            )
            if not copied:
                if not args.silent:
                    print(f"Out copy: {msg}")
            else:
                if not args.silent:
                    print(f"Out copy: {msg}")

        post_cmd = command_from_template(args.compile_cmd, srcs if is_set else [args.temp])
        post = run_cmd(post_cmd, timeout=args.compile_timeout)
        t_post += post.elapsed_sec
        n_post += 1
        if post.returncode != 0:
            post_failed += 1
            show_problem_source()
            print("Post-transform compile failed.")
            if not is_set:
                print("Source retained at:", args.temp)
                print("Pre-transform snapshot:", args.before_copy)
            print("Failing original source:", src_display)
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
            maxfail_unlimited = args.maxfail == 0
            maxfail_reached = (not maxfail_unlimited) and (post_failed >= args.maxfail)
            if maxfail_reached:
                print(f"Reached --maxfail {args.maxfail}. Stopping.")
                persist_baseline_ok()
                print_time_summary()
                print_optional_lines_summary()
                return 1
            save_state(
                args.state_file,
                {
                    "status": "post_compile_fail_continue",
                    "codes_file": normalize_path_text(args.codes) if not args.inputs else "",
                    "input_specs": list(args.inputs) if args.inputs else [],
                    "compile_cmd": args.compile_cmd,
                    "transform_cmd": args.transform_cmd,
                    "failed_source": normalize_path_text(src),
                    "last_source": normalize_path_text(src),
                    "next_index": idx + 1,
                    "post_failures": post_failed,
                },
            )
            continue

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
        flush_silent_progress_line()
        if not args.silent:
            print(f"\nStopped after {processed_entries} file(s) due to --limit {args.limit}.")
            if post_failed > 0:
                print(f"Post-transform compile failures encountered: {post_failed}")
        save_state(
            args.state_file,
            {
                "status": "limit_reached",
                "codes_file": normalize_path_text(args.codes) if not args.inputs else "",
                "input_specs": list(args.inputs) if args.inputs else [],
                "compile_cmd": args.compile_cmd,
                "transform_cmd": args.transform_cmd,
                "last_source": normalize_path_text(sources[min(start_idx + processed_entries - 1, len(sources) - 1)][0]),
                "next_index": min(start_idx + processed_entries, len(sources)),
            },
        )
        persist_baseline_ok()
        print_time_summary()
        print_optional_lines_summary()
        return 1 if post_failed > 0 else 0

    if not args.silent:
        print(
            f"\nDone. Tested {tested} existing file(s). "
            f"Baseline failed for {baseline_failed} file(s). "
            f"Post-transform compile failed for {post_failed} file(s)."
        )
    else:
        flush_silent_progress_line()
    save_state(
        args.state_file,
        {
            "status": "ok_complete",
            "codes_file": normalize_path_text(args.codes) if not args.inputs else "",
            "input_specs": list(args.inputs) if args.inputs else [],
            "compile_cmd": args.compile_cmd,
            "transform_cmd": args.transform_cmd,
            "last_source": normalize_path_text(sources[-1][0]) if sources and sources[-1] else "",
            "next_index": len(sources),
            "post_failures": post_failed,
        },
    )
    persist_baseline_ok()
    print_time_summary()
    print_optional_lines_summary()
    return 1 if post_failed > 0 else 0


def main() -> int:
    """Run baseline/transform/post-transform compile checks over a file list."""
    parser = argparse.ArgumentParser(
        description=(
            "Copy each listed Fortran file to temp file, compile baseline, run transform command, "
            "compile again, and stop when --maxfail post-transform compile failures are reached."
        )
    )
    parser.add_argument(
        "--codes",
        type=Path,
        default=Path("codes.txt"),
        help="Path to source list (default: codes.txt). Each line may contain one or more file paths.",
    )
    parser.add_argument(
        "--code-dir",
        type=Path,
        help="Base directory for relative paths in --codes entries.",
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
        help="Compile command template; use {file} or {files} placeholder (default: gfortran -c {file} -o temp.o)",
    )
    parser.add_argument(
        "--transform-cmd",
        type=str,
        required=True,
        help=(
            "Transform command template; may include {file}. "
            "Use {files} for multi-file source entries. "
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
        "--start",
        type=str,
        help=(
            "Start point (applied after --reverse, before --resume): "
            "either numeric start (codes-file line number when using --codes; entry index for positional inputs) "
            "or first-file prefix (for example: foo.f90)"
        ),
    )
    parser.add_argument(
        "--reverse",
        action="store_true",
        help="Process source entries in reverse order",
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
        "--maxfail",
        type=int,
        default=1,
        help="Maximum post-transform compile failures before stopping (0 = unlimited, default: 1)",
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
        "--silent",
        action="store_true",
        help="Suppress routine progress/success output; print only problems/failures",
    )
    parser.add_argument(
        "--out-dir",
        type=Path,
        help=(
            "Directory to save each successful transformed output as <source-basename>.f90. "
            "Must not be the current directory."
        ),
    )
    parser.add_argument(
        "--noclobber",
        action="store_true",
        help="With --out-dir, do not overwrite existing saved transformed files.",
    )
    parser.add_argument(
        "--notime",
        action="store_true",
        help="Disable elapsed-time summary output",
    )
    parser.add_argument(
        "--lines",
        action="store_true",
        help="Print per-file line-change metrics at end as a pandas DataFrame",
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
    if args.start is not None:
        s = str(args.start).strip()
        if not s:
            print("--start must not be empty.")
            return 2
        if re.match(r"^\d+$", s) and int(s) < 1:
            print("--start numeric value must be >= 1.")
            return 2
    if args.maxfail < 0:
        print("--maxfail must be >= 0.")
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
    if args.out_dir is not None:
        cwd_res = Path(".").resolve()
        out_res = args.out_dir.resolve()
        if out_res == cwd_res:
            print("--out-dir must not be the current directory.")
            return 2
        if args.out_dir.exists() and not args.out_dir.is_dir():
            print("--out-dir exists but is not a directory.")
            return 2

    if not args.loop:
        return run_once(args, resume=args.resume)

    run_count = 0
    resume_flag = args.resume
    last_code = 0
    while run_count < args.max_loop:
        run_count += 1
        if not args.silent:
            print(f"\n=== xtest loop {run_count}/{args.max_loop} ===")
        last_code = run_once(args, resume=resume_flag)
        if last_code == 0:
            if not args.silent:
                print("Loop mode: run completed successfully.")
            return 0

        if run_count >= args.max_loop:
            if not args.silent:
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
