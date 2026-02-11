#!/usr/bin/env python3
"""Warn about variable-like names that collide with Fortran keywords or intrinsics."""

from __future__ import annotations

import argparse
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, List, Set

import cli_paths as cpaths
import fortran_scan as fscan

TYPE_DECL_RE = re.compile(
    r"^\s*(integer|real|logical|character|complex|type\b|class\b|procedure\b)",
    re.IGNORECASE,
)

# Conservative keyword set (core language and common specifiers/construct words).
FORTRAN_KEYWORDS: Set[str] = {
    "abstract", "allocatable", "allocate", "assignment", "associate", "asynchronous",
    "backspace", "bind", "block", "blockdata", "call", "case", "class", "close",
    "codimension", "common", "complex", "concurrent", "contains", "continue", "critical",
    "cycle", "data", "deallocate", "default", "deferred", "dimension", "do", "double",
    "doubleprecision", "elemental", "else", "elseif", "elsewhere", "end", "endassociate",
    "endblock", "endcritical", "endenum", "endfile", "endforall", "endfunction",
    "endif", "endinterface", "endmodule", "endprocedure", "endprogram", "endselect",
    "endsubmodule", "endsubroutine", "endtype", "endwhere", "entry", "enum", "enumerator",
    "equivalence", "error", "exit", "extends", "external", "file", "final", "flush",
    "forall", "format", "function", "generic", "go", "goto", "if", "implicit", "import",
    "in", "include", "inout", "integer", "intent", "interface", "intrinsic", "inquire",
    "kind", "len", "lock", "logical", "module", "namelist", "none", "non_intrinsic",
    "nopass", "nullify", "only", "open", "operator", "optional", "out", "parameter",
    "pass", "pause", "pointer", "precision", "print", "private", "procedure", "program",
    "protected", "public", "pure", "read", "real", "recursive", "result", "return",
    "rewind", "save", "select", "selectcase", "selecttype", "sequence", "stop",
    "submodule", "subroutine", "sync", "target", "then", "to", "type", "unlock", "use",
    "value", "volatile", "wait", "where", "while", "write",
}

# Broad intrinsic-procedure set used for shadowing warnings.
FORTRAN_INTRINSICS: Set[str] = {
    "abs", "acos", "acosh", "adjustl", "adjustr", "aimag", "aint", "all", "allocated",
    "anint", "any", "asin", "asinh", "associated", "atan", "atan2", "atanh", "bessel_j0",
    "bessel_j1", "bessel_jn", "bessel_y0", "bessel_y1", "bessel_yn", "bit_size", "btest",
    "ceiling", "char", "cmplx", "command_argument_count", "compiler_options",
    "compiler_version", "conjg", "cos", "cosh", "count", "cpu_time", "cshift", "date_and_time",
    "dble", "digits", "dim", "dot_product", "dprod", "eoshift", "epsilon", "erf", "erfc",
    "erfc_scaled", "event_query", "execute_command_line", "exp", "exponent", "extends_type_of",
    "findloc", "floor", "fraction", "gamma", "get_command", "get_command_argument",
    "get_environment_variable", "huge", "hypot", "iachar", "iall", "iand", "iany", "ibclr",
    "ibits", "ibset", "ichar", "ieee_get_flag", "ieee_get_halting_mode", "ieee_get_status",
    "ieee_get_underflow_mode", "ieee_is_finite", "ieee_is_nan", "ieee_is_normal",
    "ieee_next_after", "ieee_rem", "ieee_selected_real_kind", "ieee_set_flag",
    "ieee_set_halting_mode", "ieee_set_status", "ieee_set_underflow_mode", "ieor", "index",
    "int", "ior", "iparity", "is_contiguous", "is_iostat_end", "is_iostat_eor", "ishft",
    "ishftc", "kind", "lbound", "leadz", "len", "len_trim", "lge", "lgt", "lle", "llt",
    "log", "log10", "log_gamma", "logical", "maskl", "maskr", "matmul", "max", "maxexponent",
    "maxloc", "maxval", "merge", "merge_bits", "min", "minexponent", "minloc", "minval",
    "mod", "modulo", "move_alloc", "mvbits", "nearest", "new_line", "nint", "norm2", "not",
    "null", "num_images", "pack", "parity", "popcnt", "poppar", "precision", "present",
    "product", "radix", "random_init", "random_number", "random_seed", "range", "real",
    "reduce", "repeat", "reshape", "rrspacing", "same_type_as", "scale", "scan",
    "selected_char_kind", "selected_int_kind", "selected_real_kind", "set_exponent", "shape",
    "shifta", "shiftl", "shiftr", "sign", "sin", "sinh", "size", "spacing", "spread", "sqrt",
    "storage_size", "sum", "system_clock", "tan", "tanh", "this_image", "tiny", "trailz",
    "transfer", "transpose", "trim", "ubound", "unpack", "verify", "xor",
}


@dataclass
class Finding:
    """One naming-collision finding."""

    path: Path
    line: int
    name: str
    context: str
    kind: str  # keyword | intrinsic | keyword+intrinsic
    stmt: str


def choose_files(args_files: List[Path], exclude: Iterable[str]) -> List[Path]:
    """Resolve source files from args or current-directory defaults."""
    if args_files:
        files = cpaths.expand_path_args(args_files)
    else:
        files = sorted(
            set(Path(".").glob("*.f90")) | set(Path(".").glob("*.F90")),
            key=lambda p: p.name.lower(),
        )
    return fscan.apply_excludes(files, exclude)


def classify_name(name: str) -> str:
    """Return collision class for a name, or empty string when none."""
    n = name.lower()
    is_kw = n in FORTRAN_KEYWORDS
    is_intr = n in FORTRAN_INTRINSICS
    if is_kw and is_intr:
        return "keyword+intrinsic"
    if is_kw:
        return "keyword"
    if is_intr:
        return "intrinsic"
    return ""


def analyze_file(path: Path) -> List[Finding]:
    """Analyze one file for problematic names."""
    infos, any_missing = fscan.load_source_files([path])
    if not infos or any_missing:
        return []
    finfo = infos[0]
    findings: List[Finding] = []
    seen = set()

    # Declarations (locals/module-scope/program-scope).
    for ln, stmt in fscan.iter_fortran_statements(finfo.parsed_lines):
        low = stmt.strip().lower()
        if not low or not TYPE_DECL_RE.match(low) or "::" not in low:
            continue
        for n in sorted(fscan.parse_declared_names_from_decl(low)):
            c = classify_name(n)
            if not c:
                continue
            key = (ln, n, "declaration")
            if key in seen:
                continue
            seen.add(key)
            findings.append(
                Finding(
                    path=path,
                    line=ln,
                    name=n,
                    context="declaration",
                    kind=c,
                    stmt=stmt.strip(),
                )
            )

    # Procedure dummies/results.
    for p in finfo.procedures:
        for d in sorted(p.dummy_names):
            c = classify_name(d)
            if not c:
                continue
            key = (p.start, d, f"{p.kind} dummy")
            if key in seen:
                continue
            seen.add(key)
            findings.append(
                Finding(
                    path=path,
                    line=p.start,
                    name=d,
                    context=f"{p.kind} dummy",
                    kind=c,
                    stmt=f"{p.kind} {p.name}",
                )
            )
        rname = (p.result_name or "").lower()
        if rname:
            c = classify_name(rname)
            if c:
                key = (p.start, rname, "function result")
                if key not in seen:
                    seen.add(key)
                    findings.append(
                        Finding(
                            path=path,
                            line=p.start,
                            name=rname,
                            context="function result",
                            kind=c,
                            stmt=f"{p.kind} {p.name}",
                        )
                    )
    return findings


def main() -> int:
    """Run naming-collision advisory checks."""
    parser = argparse.ArgumentParser(
        description="Warn about names colliding with Fortran keywords/intrinsics"
    )
    parser.add_argument("fortran_files", type=Path, nargs="*")
    parser.add_argument("--exclude", action="append", default=[], help="Glob pattern to exclude files")
    parser.add_argument("--verbose", action="store_true", help="Print offending statements and contexts")
    args = parser.parse_args()

    files = choose_files(args.fortran_files, args.exclude)
    if not files:
        print("No source files remain after applying --exclude filters.")
        return 2

    findings: List[Finding] = []
    for p in files:
        findings.extend(analyze_file(p))

    if not findings:
        print("No keyword/intrinsic naming collisions found.")
        return 0

    findings.sort(key=lambda f: (f.path.name.lower(), f.line, f.name))
    print(f"{len(findings)} naming-collision finding(s).")
    if args.verbose:
        for f in findings:
            print(f"{f.path.name}:{f.line} {f.name} [{f.kind}] context={f.context}")
            print(f"  {f.stmt}")
    else:
        by_file: dict[str, int] = {}
        for f in findings:
            by_file[f.path.name] = by_file.get(f.path.name, 0) + 1
        for fn in sorted(by_file.keys(), key=str.lower):
            print(f"{fn}: {by_file[fn]}")
        first = findings[0]
        print(
            f"\nFirst finding: {first.path.name}:{first.line} "
            f"{first.name} [{first.kind}] ({first.context})"
        )
        print("Run with --verbose to list all findings and statements.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
