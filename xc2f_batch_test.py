#!/usr/bin/env python3
"""Batch test C->Fortran conversion viability for jburkardt-style pairs.

Workflow per case (foo_prb.c):
1) Find paired library source foo.c in same directory.
2) Compile baseline C executable from [foo.c, foo_prb.c].
3) If baseline compile fails, skip conversion for this case.
4) Concatenate [foo.c + foo_prb.c], run xc2f.py on merged source.
5) Compile generated Fortran to executable.
6) Record status and summarize.
"""

from __future__ import annotations

import argparse
import csv
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Optional, Set


@dataclass
class CaseResult:
    case: str
    directory: str
    lib_c: str
    prb_c: str
    baseline_ok: bool
    baseline_rc: int
    xc2f_ok: bool
    fortran_ok: bool
    status: str
    reason: str = ""


def run_cmd(cmd: List[str], cwd: Optional[Path] = None) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        cmd,
        cwd=str(cwd) if cwd is not None else None,
        text=True,
        capture_output=True,
        encoding="utf-8",
        errors="ignore",
    )


def find_prb_files(root: Path) -> List[Path]:
    return sorted(root.rglob("*_prb.c"))


def paired_lib(prb: Path) -> Path:
    base = prb.stem
    if not base.endswith("_prb"):
        return prb.with_name(base + ".c")
    return prb.with_name(base[:-4] + ".c")


def ensure_dir(p: Path) -> None:
    p.mkdir(parents=True, exist_ok=True)


def case_total_size(prb: Path) -> int:
    """Approximate case size as bytes in prb + paired library file."""
    lib = paired_lib(prb)
    sz = 0
    try:
        sz += prb.stat().st_size
    except OSError:
        pass
    try:
        sz += lib.stat().st_size
    except OSError:
        # Keep missing-pair cases at the end when sorting by size.
        sz += 10**18
    return sz


def extract_local_includes(text: str) -> Set[str]:
    out: Set[str] = set()
    for ln in text.splitlines():
        s = ln.strip()
        if not s.startswith("#"):
            continue
        if "include" not in s:
            continue
        # #include "foo.h"
        i = s.find('"')
        if i < 0:
            continue
        j = s.find('"', i + 1)
        if j < 0:
            continue
        hdr = s[i + 1 : j].strip()
        if hdr:
            out.add(hdr)
    return out


def resolve_include_dirs(root: Path, lib: Path, prb: Path, cache: Dict[str, List[Path]]) -> List[Path]:
    dirs: List[Path] = []
    seen: Set[str] = set()

    def _add(p: Path) -> None:
        k = str(p.resolve()).lower()
        if k in seen:
            return
        seen.add(k)
        dirs.append(p)

    # Always include local dirs and root.
    _add(lib.parent)
    _add(prb.parent)
    _add(root)

    txt = lib.read_text(encoding="utf-8", errors="ignore") + "\n" + prb.read_text(encoding="utf-8", errors="ignore")
    headers = extract_local_includes(txt)
    for hdr in headers:
        if hdr in cache:
            for p in cache[hdr]:
                _add(p)
            continue
        found_dirs: List[Path] = []
        # Limit search breadth: first few matches are enough for include path.
        for p in root.rglob(hdr):
            found_dirs.append(p.parent)
            if len(found_dirs) >= 4:
                break
        cache[hdr] = found_dirs
        for p in found_dirs:
            _add(p)
    return dirs


def find_pycparser_fake_libc_include() -> Optional[Path]:
    try:
        import pycparser  # type: ignore

        base = Path(pycparser.__file__).resolve().parent
        cand1 = base / "utils" / "fake_libc_include"
        cand2 = base.parent / "utils" / "fake_libc_include"
        if cand1.exists():
            return cand1
        if cand2.exists():
            return cand2
    except Exception:
        return None
    return None


def sanitize_preprocessed_for_pycparser(text: str) -> str:
    """Drop preprocessed lines that frequently break pycparser on Windows GCC headers."""
    out: List[str] = []
    for ln in text.splitlines():
        low = ln.lower()
        if "__gnuc_va_list" in low or "__builtin_va_list" in low:
            continue
        ln = ln.replace("__volatile__", "")
        out.append(ln)
    return "\n".join(out) + ("\n" if text.endswith("\n") else "")


def main() -> int:
    ap = argparse.ArgumentParser(description="Batch test xc2f.py on *_prb.c / *.c pairs.")
    ap.add_argument(
        "root",
        nargs="?",
        default=r"c:\c\public_domain\github\jburkardt-c",
        help="Root directory containing C subdirectories (default: jburkardt-c path).",
    )
    ap.add_argument("--out-dir", default="xc2f_batch_out", help="Directory for generated artifacts.")
    ap.add_argument("--report", default="xc2f_batch_report.csv", help="CSV report filename.")
    ap.add_argument("--limit", type=int, default=0, help="Max number of *_prb.c cases (0 = all).")
    ap.add_argument(
        "--sort-size",
        action="store_true",
        help="Sort cases by ascending total C source size (paired .c + _prb.c).",
    )
    ap.add_argument("--verbose", action="store_true", help="Print compiler/transpiler stderr on failures.")
    ap.add_argument("--keep-artifacts", action="store_true", help="Keep merged C and generated Fortran files.")
    ap.add_argument("--array-inline", action="store_true", help="Pass --array-inline to xc2f.py.")
    args = ap.parse_args()

    root = Path(args.root)
    out_dir = Path(args.out_dir)
    ensure_dir(out_dir)

    prb_files = find_prb_files(root)
    if args.sort_size:
        prb_files = sorted(prb_files, key=case_total_size)
    if args.limit > 0:
        prb_files = prb_files[: args.limit]
    total = len(prb_files)
    if total == 0:
        print(f"No *_prb.c files found under {root}")
        return 0

    xc2f_script = Path(__file__).with_name("xc2f.py")
    if not xc2f_script.exists():
        print(f"Missing script: {xc2f_script}")
        return 1

    results: List[CaseResult] = []
    include_cache: Dict[str, List[Path]] = {}
    fake_libc = find_pycparser_fake_libc_include()
    for idx, prb in enumerate(prb_files, start=1):
        lib = paired_lib(prb)
        case_name = str(prb.relative_to(root)).replace("\\", "/")
        case_tag = prb.parent.name + "__" + prb.stem
        case_dir = out_dir / case_tag
        ensure_dir(case_dir)

        print(f"[{idx}/{total}] {case_name}")

        if not lib.exists():
            results.append(
                CaseResult(
                    case=case_name,
                    directory=str(prb.parent),
                    lib_c=str(lib),
                    prb_c=str(prb),
                    baseline_ok=False,
                    baseline_rc=1,
                    xc2f_ok=False,
                    fortran_ok=False,
                    status="missing_pair",
                    reason="paired library .c not found",
                )
            )
            continue

        include_dirs = resolve_include_dirs(root, lib, prb, include_cache)
        inc_flags: List[str] = []
        for d in include_dirs:
            inc_flags.extend(["-I", str(d)])

        # 1) Baseline C compile gate
        c_exe = case_dir / "baseline.exe"
        c_cmd = ["gcc", *inc_flags, str(lib), str(prb), "-lm", "-o", str(c_exe)]
        c_proc = run_cmd(c_cmd)
        if c_proc.returncode != 0:
            if args.verbose:
                print(c_proc.stderr.strip())
            results.append(
                CaseResult(
                    case=case_name,
                    directory=str(prb.parent),
                    lib_c=str(lib),
                    prb_c=str(prb),
                    baseline_ok=False,
                    baseline_rc=c_proc.returncode,
                    xc2f_ok=False,
                    fortran_ok=False,
                    status="skip_baseline_fail",
                    reason="baseline C compile failed",
                )
            )
            continue

        # 2) Merge C sources for single-file transpilation
        merged_c = case_dir / "merged.c"
        merged_text = lib.read_text(encoding="utf-8", errors="ignore") + "\n\n" + prb.read_text(
            encoding="utf-8", errors="ignore"
        )
        merged_c.write_text(merged_text, encoding="utf-8")

        # Preprocess merged C to improve pycparser compatibility.
        pre_c = case_dir / "merged_pp.c"
        # Prefer pycparser-friendly preprocessed output.
        pp_cmd = [
            "gcc",
            "-E",
            "-P",
            "-D__attribute__(x)=",
            "-D__extension__=",
            "-D__volatile__=",
            "-D__inline__=inline",
            "-D__asm__(x)=",
            "-D__restrict=",
        ]
        if fake_libc is not None:
            pp_cmd.extend(["-nostdinc", "-I", str(fake_libc)])
        pp_cmd.extend(inc_flags)
        pp_cmd.extend([str(merged_c), "-o", str(pre_c)])
        pp_proc = run_cmd(pp_cmd)
        if pp_proc.returncode != 0:
            if args.verbose:
                print(pp_proc.stderr.strip())
            results.append(
                CaseResult(
                    case=case_name,
                    directory=str(prb.parent),
                    lib_c=str(lib),
                    prb_c=str(prb),
                    baseline_ok=True,
                    baseline_rc=0,
                    xc2f_ok=False,
                    fortran_ok=False,
                    status="fail_preprocess",
                    reason="C preprocess failed",
                )
            )
            continue
        # Sanitize problematic GCC preprocessed constructs for pycparser.
        pre_text = pre_c.read_text(encoding="utf-8", errors="ignore")
        pre_text = sanitize_preprocessed_for_pycparser(pre_text)
        pre_c.write_text(pre_text, encoding="utf-8")

        # 3) Transpile
        f90 = case_dir / "merged.f90"
        xc_cmd = [sys.executable, str(xc2f_script), str(pre_c), "--out", str(f90)]
        if args.array_inline:
            xc_cmd.append("--array-inline")
        xc_proc = run_cmd(xc_cmd)
        if xc_proc.returncode != 0:
            if args.verbose:
                print(xc_proc.stderr.strip())
            results.append(
                CaseResult(
                    case=case_name,
                    directory=str(prb.parent),
                    lib_c=str(lib),
                    prb_c=str(prb),
                    baseline_ok=True,
                    baseline_rc=0,
                    xc2f_ok=False,
                    fortran_ok=False,
                    status="fail_xc2f",
                    reason="xc2f.py failed",
                )
            )
            continue

        # 4) Compile generated Fortran
        f_exe = case_dir / "converted.exe"
        f_cmd = ["gfortran", str(f90), "-o", str(f_exe)]
        f_proc = run_cmd(f_cmd)
        if f_proc.returncode != 0:
            if args.verbose:
                print(f_proc.stderr.strip())
            results.append(
                CaseResult(
                    case=case_name,
                    directory=str(prb.parent),
                    lib_c=str(lib),
                    prb_c=str(prb),
                    baseline_ok=True,
                    baseline_rc=0,
                    xc2f_ok=True,
                    fortran_ok=False,
                    status="fail_fortran_compile",
                    reason="generated Fortran compile failed",
                )
            )
            continue

        results.append(
            CaseResult(
                case=case_name,
                directory=str(prb.parent),
                lib_c=str(lib),
                prb_c=str(prb),
                baseline_ok=True,
                baseline_rc=0,
                xc2f_ok=True,
                fortran_ok=True,
                status="pass",
            )
        )

        if not args.keep_artifacts:
            for p in (merged_c, pre_c, f90):
                if p.exists():
                    p.unlink()

    report_path = out_dir / args.report
    with report_path.open("w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(
            [
                "case",
                "directory",
                "lib_c",
                "prb_c",
                "baseline_ok",
                "baseline_rc",
                "xc2f_ok",
                "fortran_ok",
                "status",
                "reason",
            ]
        )
        for r in results:
            w.writerow(
                [
                    r.case,
                    r.directory,
                    r.lib_c,
                    r.prb_c,
                    r.baseline_ok,
                    r.baseline_rc,
                    r.xc2f_ok,
                    r.fortran_ok,
                    r.status,
                    r.reason,
                ]
            )

    n_total = len(results)
    n_pair_missing = sum(1 for r in results if r.status == "missing_pair")
    n_baseline_fail = sum(1 for r in results if r.status == "skip_baseline_fail")
    n_baseline_ok = sum(1 for r in results if r.baseline_ok)
    n_pre_fail = sum(1 for r in results if r.status == "fail_preprocess")
    n_xc2f_fail = sum(1 for r in results if r.status == "fail_xc2f")
    n_fcompile_fail = sum(1 for r in results if r.status == "fail_fortran_compile")
    n_pass = sum(1 for r in results if r.status == "pass")

    print("")
    print("Summary:")
    print(f"total_cases: {n_total}")
    print(f"missing_pair: {n_pair_missing}")
    print(f"baseline_compile_fail: {n_baseline_fail}")
    print(f"baseline_compile_pass: {n_baseline_ok}")
    print(f"preprocess_fail: {n_pre_fail}")
    print(f"xc2f_fail: {n_xc2f_fail}")
    print(f"fortran_compile_fail: {n_fcompile_fail}")
    print(f"fortran_compile_pass: {n_pass}")
    if n_baseline_ok > 0:
        pct = 100.0 * n_pass / n_baseline_ok
        print(f"pass_rate_given_baseline_ok: {pct:.1f}%")
    print(f"report: {report_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
