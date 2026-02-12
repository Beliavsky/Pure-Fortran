#!/usr/bin/env python3
"""Convert fixed-form Fortran .f sources to free-form .f90 sources."""

from __future__ import annotations

import argparse
from dataclasses import dataclass
import json
from pathlib import Path
import shlex
import subprocess
import re
from typing import Dict, List, Optional, Tuple


@dataclass
class PendingStmt:
    """Track one in-progress fixed-form statement across continuations."""

    label: str
    parts: List[str]


STATE_VERSION = 1


def collect_inputs(path: Path, recursive: bool) -> List[Path]:
    """Collect .f inputs from a file or directory path."""
    if path.is_file():
        if path.suffix.lower() != ".f":
            raise ValueError(f"Input file is not .f: {path}")
        return [path]
    if path.is_dir():
        files = list(path.rglob("*.f") if recursive else path.glob("*.f"))
        return sorted(files, key=lambda p: str(p).lower())
    raise ValueError(f"Input path does not exist: {path}")


def to_output_path(src: Path) -> Path:
    """Map foo.f -> foo.f90 in same directory."""
    return src.with_suffix(".f90")


def normalize_comment_line(raw: str) -> str:
    """Convert fixed-form comment markers in column 1 to free-form ! comments."""
    if not raw:
        return "!"
    first = raw[0]
    tail = raw[1:].rstrip("\r\n")
    if first == "!":
        return "!" + tail
    return "!" + tail


def normalize_exponent_blanks(code: str) -> str:
    """Normalize numeric exponent blanks such as '-1. D0' -> '-1.D0' outside strings."""
    def _fix_chunk(chunk: str) -> str:
        chunk = re.sub(
            r"(?i)([0-9.])\s+([deq])\s*([+-]?)\s*(\d+)",
            r"\1\2\3\4",
            chunk,
        )
        chunk = re.sub(
            r"(?i)\.(and|or|not|eqv|neqv|eq|ne|lt|le|gt|ge)\s+\.",
            lambda m: f".{m.group(1)}.",
            chunk,
        )
        chunk = re.sub(
            r"(?i)\.\s+(and|or|not|eqv|neqv|eq|ne|lt|le|gt|ge)\.",
            lambda m: f".{m.group(1)}.",
            chunk,
        )
        # Some fixed-form sources compact typed function headers, e.g.
        # "characterfunctionx(i)". Free form requires separators.
        chunk = re.sub(
            r"(?i)^(\s*)character\s*\*\s*(\d+)\s*function\s*([a-z_]\w*)",
            r"\1character*\2 function \3",
            chunk,
        )
        chunk = re.sub(
            r"(?i)^(\s*character\s*\*\s*)([0-9 ]+)([a-z_]\w*)\s*$",
            lambda m: f"{m.group(1)}{re.sub(r'\s+', '', m.group(2))} {m.group(3)}",
            chunk,
        )
        chunk = re.sub(
            r"(?i)^(\s*)double\s*precision\s*function\s*([a-z_]\w*)",
            r"\1double precision function \2",
            chunk,
        )
        chunk = re.sub(
            r"(?i)^(\s*)doubleprecisionfunction\s*([a-z_]\w*)",
            r"\1double precision function \2",
            chunk,
        )
        chunk = re.sub(
            r"(?i)^(\s*(?:character\s*\*\s*(?:\d+|\([^)]*\))|character|"
            r"integer|real|logical|complex|double\s*precision))\s*function([a-z_]\w*)",
            r"\1 function \2",
            chunk,
        )
        chunk = re.sub(
            r"(?i)^(\s*)(character|integer|real|logical|complex)\s*function\s*([a-z_]\w*)",
            r"\1\2 function \3",
            chunk,
        )
        chunk = re.sub(
            r"(?i)^(\s*)(character|integer|real|logical|complex)function\s*([a-z_]\w*)",
            r"\1\2 function \3",
            chunk,
        )
        type_prefix = (
            r"(?:character\s*\*\s*(?:\d+|\([^)]*\))|character|"
            r"integer\s*\*\s*\d+|integer|real\s*\*\s*\d+|real|"
            r"logical\s*\*\s*\d+|logical|complex\s*\*\s*\d+|complex|"
            r"double\s*precision)"
        )
        chunk = re.sub(
            rf"(?i)^(\s*)({type_prefix})\s*([a-z_])",
            lambda m: f"{m.group(1)}{m.group(2)} {m.group(3)}",
            chunk,
        )
        m_do = re.match(r"(?i)^(\s*)do\s*(\d+)\s*([a-z_]\w*)\s*=\s*(.+)$", chunk)
        if m_do:
            if "," in m_do.group(4):
                chunk = f"{m_do.group(1)}do {m_do.group(2)} {m_do.group(3)} = {m_do.group(4)}"
            else:
                chunk = f"{m_do.group(1)}do{m_do.group(2)}{m_do.group(3)} = {m_do.group(4)}"
        chunk = re.sub(
            r"(?i)^(\s*)do\s*(\d+)\s*while\s*(\(.*\))\s*$",
            r"\1do \2 while \3",
            chunk,
        )
        chunk = re.sub(
            r"(?i)^(\s*)dowhile\s*(\(.*\))\s*$",
            r"\1do while \2",
            chunk,
        )
        m_if_call = re.match(r"(?i)^(\s*if\s*\([^)]*\))\s*call([a-z_]\w*)\s*$", chunk)
        if m_if_call and "=" not in chunk:
            chunk = f"{m_if_call.group(1)} call {m_if_call.group(2)}"
        chunk = re.sub(
            r"(?i)^(\s*if\s*\([^)]*\)\s*)(goto|read|print|pause|stop|rewind|backspace|endfile)\s*(\d+)\s*$",
            r"\1\2 \3",
            chunk,
        )
        chunk = re.sub(
            r"(?i)^(\s*if\s*\([^)]*\)\s*)(read|print)\s*(\d+)(\s*,.*)$",
            r"\1\2 \3\4",
            chunk,
        )
        chunk = re.sub(
            r"(?i)^(\s*if\s*\([^)]*\)\s*)return\s*(\d+)\s*$",
            r"\1return \2",
            chunk,
        )
        chunk = re.sub(
            r"(?i)^(\s*)assign\s*(\d+)\s*to\s*([a-z_]\w*)\s*$",
            r"\1assign \2 to \3",
            chunk,
        )
        chunk = re.sub(
            r"(?i)^(\s*\d*\s*)f\s*o\s*r\s*m\s*a\s*t\s*(\()",
            r"\1format \2",
            chunk,
        )
        chunk = re.sub(
            r"(?i)^(\s*)data([a-z_]\w*)(\s*/.*)$",
            r"\1data \2\3",
            chunk,
        )
        chunk = re.sub(
            r"(?i)^(\s*)dimension([a-z_]\w*)(\s*\(.*)$",
            r"\1dimension \2\3",
            chunk,
        )
        chunk = re.sub(
            r"(?i)^(\s*)external([a-z_]\w*)\s*$",
            r"\1external \2",
            chunk,
        )
        chunk = re.sub(
            r"(?i)^(\s*)save([a-z_]\w*)\s*$",
            r"\1save \2",
            chunk,
        )
        chunk = re.sub(
            r"(?i)^(\s*)intrinsic([a-z_]\w*)\s*$",
            r"\1intrinsic \2",
            chunk,
        )
        chunk = re.sub(
            r"(?i)^(\s*)implicit(real|integer|logical|complex|character|double\s*precision)(\s*\(.*)$",
            r"\1implicit \2\3",
            chunk,
        )
        chunk = re.sub(
            r"(?i)^(\s*)implicitnone(\s*)$",
            r"\1implicit none\2",
            chunk,
        )
        chunk = re.sub(
            r"(?i)^(\s*)blockdata(\s*)$",
            r"\1block data\2",
            chunk,
        )
        chunk = re.sub(
            r"(?i)^(\s*)blockdata([a-z_]\w*)\s*$",
            r"\1block data \2",
            chunk,
        )
        return chunk

    out: List[str] = []
    cur: List[str] = []
    in_single = False
    in_double = False
    for ch in code:
        if ch == "'" and not in_double:
            if not in_single:
                out.append(_fix_chunk("".join(cur)))
                cur = []
            in_single = not in_single
            out.append(ch)
            continue
        if ch == '"' and not in_single:
            if not in_double:
                out.append(_fix_chunk("".join(cur)))
                cur = []
            in_double = not in_double
            out.append(ch)
            continue
        if in_single or in_double:
            out.append(ch)
        else:
            cur.append(ch)
    if cur:
        out.append(_fix_chunk("".join(cur)))
    return "".join(out)


def flush_stmt(pending: Optional[PendingStmt], out_lines: List[str]) -> Optional[PendingStmt]:
    """Flush a pending statement into output lines."""
    if pending is None:
        return None
    parts = [p.rstrip() for p in pending.parts if p is not None]
    if not parts:
        return None

    # Some fixed-form sources rely on extension behavior for EXTERNAL/INTRINSIC
    # name lists split across continuation lines without a comma. In free form,
    # that can make the first line syntactically complete before '&'. Insert
    # a missing comma at continuation boundaries when both sides are names.
    head = parts[0].lstrip().upper()
    is_name_list_stmt = head.startswith("EXTERNAL") or head.startswith("INTRINSIC")
    if is_name_list_stmt:
        fixed_parts: List[str] = [parts[0]]
        for nxt in parts[1:]:
            prev = fixed_parts[-1]
            prev_r = prev.rstrip()
            nxt_l = nxt.lstrip()
            if (
                prev_r
                and nxt_l
                and re.search(r"[A-Z0-9_]$", prev_r, flags=re.IGNORECASE)
                and re.match(r"[A-Z_]", nxt_l, flags=re.IGNORECASE)
                and not prev_r.endswith(",")
            ):
                fixed_parts[-1] = prev_r + ","
            fixed_parts.append(nxt)
        parts = fixed_parts

    # Fixed form can continue in the middle of identifiers/numbers. Free form
    # cannot split lexical tokens across continuation lines, so merge such
    # fragments before emitting '&' continuations.
    merged_parts: List[str] = [parts[0]]
    for nxt in parts[1:]:
        prev_r = merged_parts[-1].rstrip()
        nxt_l = nxt.lstrip()
        if (
            prev_r
            and nxt_l
            and re.search(r"[A-Z0-9_]$", prev_r, flags=re.IGNORECASE)
            and re.match(r"[A-Z0-9_]", nxt_l, flags=re.IGNORECASE)
        ):
            merged_parts[-1] = prev_r + nxt_l
        else:
            merged_parts.append(nxt_l)
    parts = merged_parts

    if len(parts) == 1:
        body = parts[0].rstrip()
        if not body:
            return None
        if pending.label:
            out_lines.append(f"{pending.label} {body.lstrip()}")
        else:
            out_lines.append(body)
        return None

    first = parts[0].rstrip()
    if pending.label:
        out_lines.append(f"{pending.label} {first.lstrip()} &")
    else:
        out_lines.append(f"{first} &")

    for mid in parts[1:-1]:
        out_lines.append(f"&{mid.lstrip()} &")
    out_lines.append(f"&{parts[-1].lstrip()}")
    return None


def convert_text(text: str) -> str:
    """Convert fixed-form source text to free-form source text."""
    out_lines: List[str] = []
    pending: Optional[PendingStmt] = None
    deferred_comments: List[str] = []

    def flush_pending() -> None:
        nonlocal pending
        pending = flush_stmt(pending, out_lines)
        if deferred_comments:
            out_lines.extend(deferred_comments)
            deferred_comments.clear()

    for raw in text.splitlines():
        if raw.strip() == "":
            flush_pending()
            out_lines.append("")
            continue

        # Fixed-form full-line comments in column 1.
        if raw and raw[0] in {"c", "C", "*", "!"}:
            comment = normalize_comment_line(raw)
            if pending is None:
                out_lines.append(comment)
            else:
                # In fixed form, comment lines may appear between continuation
                # records without ending the statement.
                deferred_comments.append(comment)
            continue

        # Some sources place '!' in the fixed-form label field (columns 1-5).
        # Treat that as a full comment line.
        if "!" in raw[:5]:
            bang = raw[:5].find("!")
            if raw[:bang].strip() == "":
                flush_pending()
                out_lines.append("!" + raw[bang + 1 :].rstrip("\r\n"))
                continue

        # Preprocessor/directive lines in column 1.
        if raw and raw[0] == "#":
            flush_pending()
            out_lines.append(raw.rstrip("\r\n"))
            continue

        # Fixed-form fields.
        label = raw[:5].strip() if len(raw) >= 5 else raw.strip()
        cont_col = raw[5] if len(raw) >= 6 else " "
        code = (raw[6:72] if len(raw) > 6 else "").rstrip()
        code = normalize_exponent_blanks(code)
        is_cont = (cont_col not in {" ", "0"})

        if code.lstrip().startswith("!"):
            flush_pending()
            out_lines.append(code.lstrip())
            continue

        if is_cont:
            if pending is None:
                pending = PendingStmt(label="", parts=[code])
            else:
                pending.parts.append(code)
            continue

        flush_pending()
        pending = PendingStmt(label=label, parts=[code])

    flush_pending()
    return "\n".join(out_lines) + "\n"


def convert_file(src: Path, dst: Path) -> None:
    """Convert one fixed-form file to free-form file."""
    text = src.read_text(encoding="utf-8", errors="ignore")
    converted = convert_text(text)
    dst.write_text(converted, encoding="utf-8", newline="\n")


def command_from_template(template: str, file_path: Path) -> List[str]:
    """Build argv from a command template and target file path."""
    if "{file}" in template:
        return shlex.split(template.replace("{file}", str(file_path)), posix=False)
    argv = shlex.split(template, posix=False)
    argv.append(str(file_path))
    return argv


def run_cmd(argv: List[str]) -> subprocess.CompletedProcess[str]:
    """Run one shell command and capture text output."""
    return subprocess.run(argv, capture_output=True, text=True)


def file_fingerprint(path: Path) -> str:
    """Return a cheap fingerprint for change detection."""
    st = path.stat()
    return f"{st.st_size}:{st.st_mtime_ns}"


def normalize_key(path: Path) -> str:
    """Normalize a path as key in state on case-insensitive filesystems."""
    return str(path.resolve()).lower()


def load_state(path: Path) -> Dict[str, object]:
    """Load persisted run state or return empty state."""
    if not path.exists():
        return {"version": STATE_VERSION, "files": {}, "last_fail": None}
    try:
        data = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        return {"version": STATE_VERSION, "files": {}, "last_fail": None}
    if not isinstance(data, dict):
        return {"version": STATE_VERSION, "files": {}, "last_fail": None}
    files = data.get("files", {})
    if not isinstance(files, dict):
        files = {}
    return {
        "version": data.get("version", STATE_VERSION),
        "files": files,
        "last_fail": data.get("last_fail"),
    }


def save_state(path: Path, state: Dict[str, object]) -> None:
    """Persist run state."""
    path.write_text(json.dumps(state, indent=2, sort_keys=True), encoding="utf-8")


def can_skip_known_ok(entry: Dict[str, object], check_mode: str) -> bool:
    """Whether a prior success entry is strong enough to skip current check mode."""
    if entry.get("status") != "ok":
        return False
    prev_mode = str(entry.get("check_mode", ""))
    if check_mode == "converted-only":
        return prev_mode in {"converted-only", "regression"}
    return prev_mode == "regression"


def main() -> int:
    """CLI entrypoint."""
    parser = argparse.ArgumentParser(description="Convert .f fixed-form Fortran files to .f90 free-form files")
    parser.add_argument("path", type=Path, help="Input .f file or directory containing .f files")
    parser.add_argument("--recursive", action="store_true", help="When input is a directory, include subdirectories")
    parser.add_argument("--verbose", action="store_true", help="Print extra per-file check details")
    parser.add_argument("--overwrite", action="store_true", help="Allow overwriting existing .f90 outputs")
    parser.add_argument(
        "--check",
        action="store_true",
        help="Compile-check original .f and converted .f90 and report regressions",
    )
    parser.add_argument(
        "--compile-cmd",
        type=str,
        default="gfortran -c {file}",
        help="Compile command template used by --check (default: gfortran -c {file})",
    )
    parser.add_argument(
        "--check-mode",
        choices=["regression", "converted-only"],
        default="regression",
        help="With --check: 'regression' compiles both .f and .f90, 'converted-only' compiles only .f90",
    )
    parser.add_argument(
        "--fail-fast",
        action="store_true",
        help="With --check, stop on first regression where .f compiles and .f90 fails",
    )
    parser.add_argument(
        "--state-file",
        type=Path,
        default=Path(".xfree_state.json"),
        help="JSON file for incremental state (default: .xfree_state.json)",
    )
    parser.add_argument(
        "--resume",
        action="store_true",
        help="Resume from last failing file recorded in --state-file",
    )
    parser.add_argument(
        "--limit",
        type=int,
        help="Maximum number of input .f files to process this run (applies after --resume)",
    )
    parser.add_argument(
        "--skip-known-ok",
        action="store_true",
        help="Skip unchanged files already marked OK in --state-file",
    )
    parser.add_argument(
        "--no-skip-known-ok",
        action="store_true",
        help="Disable skipping unchanged known-OK files",
    )
    args = parser.parse_args()
    if args.fail_fast and not args.check:
        print("--fail-fast requires --check.")
        return 2
    if args.no_skip_known_ok and args.skip_known_ok:
        print("Use only one of --skip-known-ok / --no-skip-known-ok.")
        return 2
    if args.limit is not None and args.limit < 1:
        print("--limit must be >= 1.")
        return 2

    try:
        inputs = collect_inputs(args.path, args.recursive)
    except ValueError as exc:
        print(str(exc))
        return 2

    if not inputs:
        print("No .f files found.")
        return 2

    outputs = [(src, to_output_path(src)) for src in inputs]
    existing = [dst for _src, dst in outputs if dst.exists()]
    if existing and not args.overwrite:
        print("Refusing to overwrite existing .f90 output(s):")
        for p in existing:
            print(p)
        print("Use --overwrite to replace them.")
        return 2

    state_exists = args.state_file.exists()
    state = load_state(args.state_file)
    state_files = state.get("files")
    if not isinstance(state_files, dict):
        state_files = {}
        state["files"] = state_files

    skip_known_ok = False
    if args.check:
        if args.no_skip_known_ok:
            skip_known_ok = False
        elif args.skip_known_ok:
            skip_known_ok = True
        else:
            skip_known_ok = state_exists

    start_index = 0
    if args.resume:
        last_fail = state.get("last_fail")
        if isinstance(last_fail, str) and last_fail:
            key_to_index = {normalize_key(src): i for i, (src, _dst) in enumerate(outputs)}
            idx = key_to_index.get(last_fail.lower())
            if idx is not None:
                start_index = idx
                print(f"Resuming from entry {idx + 1}: {outputs[idx][0]}")
            else:
                print("Resume target from state was not found in current input set; starting from first file.")

    converted = 0
    regressions = 0
    checked = 0
    originals_compiled = 0
    converted_failed = 0
    skipped_known_ok = 0
    failures = 0
    processed = 0
    limit_reached = False
    for src, dst in outputs[start_index:]:
        if args.limit is not None and processed >= args.limit:
            limit_reached = True
            break
        processed += 1
        src_key = normalize_key(src)
        src_fp = file_fingerprint(src)
        prev = state_files.get(src_key)
        if not isinstance(prev, dict):
            prev = {}

        if args.check and skip_known_ok and prev.get("fingerprint") == src_fp and can_skip_known_ok(prev, args.check_mode):
            skipped_known_ok += 1
            print(f"Skipped (known OK): {src}")
            continue

        convert_file(src, dst)
        converted += 1
        print(f"Converted: {src} -> {dst}")
        state_files[src_key] = {
            "source": str(src),
            "dest": str(dst),
            "fingerprint": src_fp,
            "status": "converted",
            "check_mode": args.check_mode if args.check else None,
        }
        if not args.check:
            continue
        checked += 1
        conv_cmd = command_from_template(args.compile_cmd, dst)
        if args.verbose:
            print(f"Check converted: {' '.join(conv_cmd)}")
        conv = run_cmd(conv_cmd)
        if args.check_mode == "regression":
            orig_cmd = command_from_template(args.compile_cmd, src)
            if args.verbose:
                print(f"Check original : {' '.join(orig_cmd)}")
            orig = run_cmd(orig_cmd)
            if orig.returncode == 0:
                originals_compiled += 1
        else:
            orig = None

        if conv.returncode != 0:
            converted_failed += 1
            failures += 1

        failed = False
        if args.check_mode == "regression":
            if orig is not None and orig.returncode == 0 and conv.returncode != 0:
                failed = True
                regressions += 1
                print("")
                print(f"CHECK regression: {src} compiles, but {dst} does not.")
                print("converted stderr:")
                print(conv.stderr.rstrip())
                if conv.stdout.strip():
                    print("converted stdout:")
                    print(conv.stdout.rstrip())
        else:
            if conv.returncode != 0:
                failed = True
                print("")
                print(f"CHECK failure: converted file did not compile: {dst}")
                print("converted stderr:")
                print(conv.stderr.rstrip())
                if conv.stdout.strip():
                    print("converted stdout:")
                    print(conv.stdout.rstrip())

        if failed:
            state["last_fail"] = src_key
            state_files[src_key] = {
                "source": str(src),
                "dest": str(dst),
                "fingerprint": src_fp,
                "status": "fail",
                "check_mode": args.check_mode,
            }
            save_state(args.state_file, state)
            if args.fail_fast:
                print(
                    "Fail-fast: stopping at first "
                    + ("regression." if args.check_mode == "regression" else "converted-file compile failure.")
                )
                print(f"Done. Converted {converted} file(s).")
                if skipped_known_ok:
                    print(f"Skipped known OK unchanged: {skipped_known_ok}")
                print(f"Check summary: checked {checked} file(s).")
                if args.check_mode == "regression":
                    print(f"Original .f compiled: {originals_compiled}")
                    print(f"Converted .f90 failed to compile: {converted_failed}")
                    print(f"Regressions (.f compiled but .f90 failed): {regressions}")
                else:
                    print(f"Converted .f90 failed to compile: {converted_failed}")
                return 1
        else:
            state_files[src_key] = {
                "source": str(src),
                "dest": str(dst),
                "fingerprint": src_fp,
                "status": "ok",
                "check_mode": args.check_mode,
            }

    if limit_reached:
        print(f"Stopped after {processed} file(s) due to --limit {args.limit}.")
    print(f"Done. Converted {converted} file(s).")
    if skipped_known_ok:
        print(f"Skipped known OK unchanged: {skipped_known_ok}")
    if args.check:
        print(f"Check summary: checked {checked} file(s).")
        if args.check_mode == "regression":
            print(f"Original .f compiled: {originals_compiled}")
        print(f"Converted .f90 failed to compile: {converted_failed}")
        if args.check_mode == "regression":
            print(f"Regressions (.f compiled but .f90 failed): {regressions}")

    state["last_fail"] = None
    save_state(args.state_file, state)
    if args.check_mode == "regression":
        return 1 if regressions > 0 else 0
    return 1 if failures > 0 else 0


if __name__ == "__main__":
    raise SystemExit(main())
