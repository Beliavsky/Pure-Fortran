#!/usr/bin/env python3
from __future__ import annotations

import argparse
import difflib
import re
import shutil
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Set, Tuple

import fortran_build as fbuild
import fortran_scan as fscan
import xassumed_shape as xas


@dataclass
class ProcSig:
    module_name: str
    proc_name: str
    drop_positions: List[int]  # 0-based in original arg list
    drop_names: Set[str]
    dummy_ranks: Dict[int, int]  # original arg index -> rank (0 scalar)
    dummy_accept_expr: Dict[int, bool]  # original arg index -> safe to pass expression actual


USE_RE = re.compile(r"^\s*use\s+([a-z][a-z0-9_]*)\s*(?:,\s*only\s*:\s*(.*))?$", re.IGNORECASE)
ASSIGN_KW_RE = re.compile(r"^\s*[a-z][a-z0-9_]*\s*=", re.IGNORECASE)
CALL_RE = re.compile(r"^\s*call\s+([a-z][a-z0-9_]*)\s*\((.*)\)\s*$", re.IGNORECASE)


def split_code_comment(line: str) -> Tuple[str, str]:
    in_s = False
    in_d = False
    i = 0
    while i < len(line):
        ch = line[i]
        if ch == "'" and not in_d:
            in_s = not in_s
        elif ch == '"' and not in_s:
            in_d = not in_d
        elif ch == "!" and not in_s and not in_d:
            return line[:i], line[i:]
        i += 1
    return line, ""


def show_diff(old_text: str, new_text: str, path: Path) -> None:
    diff = difflib.unified_diff(
        old_text.splitlines(keepends=True),
        new_text.splitlines(keepends=True),
        fromfile=str(path),
        tofile=str(path),
    )
    txt = "".join(diff).rstrip()
    if txt:
        print(txt)


def load_codes(path: Path) -> List[List[Path]]:
    out: List[List[Path]] = []
    for raw in path.read_text(encoding="utf-8", errors="ignore").splitlines():
        line = raw.strip()
        if not line or line.startswith("#"):
            continue
        out.append([Path(tok) for tok in line.split()])
    return out


def resolve_code_entries(entries: List[List[Path]], code_dir: Optional[Path]) -> List[List[Path]]:
    if code_dir is None:
        return entries
    return [[p if p.is_absolute() else (code_dir / p) for p in g] for g in entries]


def parse_use_map(lines: List[str]) -> Dict[str, Optional[Set[str]]]:
    use_map: Dict[str, Optional[Set[str]]] = {}
    for _ln, stmt in fscan.iter_fortran_statements(lines):
        s = stmt.strip()
        m = USE_RE.match(s)
        if not m:
            continue
        mod = m.group(1).lower()
        only_txt = m.group(2)
        if only_txt is None:
            use_map[mod] = None
            continue
        names: Set[str] = set()
        for tok in xas.split_top_level_commas(only_txt):
            t = tok.strip().lower()
            if not t:
                continue
            if "=>" in t:
                left, right = [p.strip() for p in t.split("=>", 1)]
                if right:
                    names.add(right)
                elif left:
                    names.add(left)
            else:
                names.add(t)
        if mod not in use_map:
            use_map[mod] = names
            continue
        prev = use_map[mod]
        if prev is None:
            continue
        use_map[mod] = prev | names
    return use_map


def parse_matching_paren(s: str, lpar: int) -> int:
    depth = 0
    in_s = False
    in_d = False
    i = lpar
    while i < len(s):
        ch = s[i]
        if ch == "'" and not in_d:
            in_s = not in_s
        elif ch == '"' and not in_s:
            in_d = not in_d
        elif not in_s and not in_d:
            if ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
                if depth == 0:
                    return i
        i += 1
    return -1


def declared_ranks(lines: List[str]) -> Dict[str, int]:
    ranks: Dict[str, int] = {}
    def lhs_dimension_rank(lhs: str) -> int:
        m = re.search(r"\bdimension\s*\(([^)]*)\)", lhs, re.IGNORECASE)
        if not m:
            return 0
        inner = m.group(1).strip()
        if not inner:
            return 0
        return max(1, len(xas.split_top_level_commas(inner)))
    for _ln, stmt in fscan.iter_fortran_statements(lines):
        parsed = xas.parse_decl_stmt(stmt)
        if parsed is None:
            continue
        lhs, rhs = parsed
        lhs_rank = lhs_dimension_rank(lhs)
        for ent in xas.split_top_level_commas(rhs):
            nm, is_arr, dims = xas.parse_entity(ent)
            if not nm:
                continue
            if is_arr:
                ranks[nm] = max(1, len(dims))
            elif lhs_rank > 0:
                ranks[nm] = lhs_rank
            elif nm not in ranks:
                ranks[nm] = 0
    return ranks


def drop_args_and_adapt(
    arg_txt: str,
    sig: ProcSig,
    ranks: Dict[str, int],
) -> Tuple[str, int]:
    args = xas.split_top_level_commas(arg_txt)
    if not args:
        return arg_txt, 0
    keep: List[str] = []
    changed = 0
    for i, a in enumerate(args):
        t = a.strip()
        if i in sig.drop_positions:
            changed += 1
            continue
        if "=" in t and "=>" not in t:
            k, v = [p.strip() for p in t.split("=", 1)]
            if k.lower() in sig.drop_names:
                changed += 1
                continue
            dr = sig.dummy_ranks.get(i, 0)
            accept_expr = sig.dummy_accept_expr.get(i, True)
            if dr == 1 and accept_expr and _is_bare_name(v):
                rk = ranks.get(v.lower())
                if rk is not None and rk > 1:
                    keep.append(f"{k}=[{v}]")
                    changed += 1
                    continue
        else:
            dr = sig.dummy_ranks.get(i, 0)
            accept_expr = sig.dummy_accept_expr.get(i, True)
            if dr == 1 and accept_expr and _is_bare_name(t):
                rk = ranks.get(t.lower())
                if rk is not None and rk > 1:
                    keep.append(f"[{t}]")
                    changed += 1
                    continue
        keep.append(a)
    return ", ".join(keep), changed


def rewrite_stmt_invocations(stmt: str, name_to_sig: Dict[str, ProcSig], ranks: Dict[str, int]) -> Tuple[str, int]:
    changed = 0
    out = stmt
    for name, sig in name_to_sig.items():
        i = 0
        low = out.lower()
        while i < len(out):
            j = low.find(name, i)
            if j < 0:
                break
            before = low[j - 1] if j > 0 else " "
            after_idx = j + len(name)
            after = low[after_idx] if after_idx < len(out) else " "
            if (before.isalnum() or before == "_") or (after.isalnum() or after == "_"):
                i = j + 1
                continue
            k = after_idx
            while k < len(out) and out[k].isspace():
                k += 1
            if k >= len(out) or out[k] != "(":
                i = j + 1
                continue
            # Skip declarations/interfaces and type-bound forms.
            left = low[:j].rstrip()
            if left.endswith("%"):
                i = j + 1
                continue
            left_word = left.split()[-1] if left.split() else ""
            if left_word in {"subroutine", "function", "procedure", "interface", "module", "use"}:
                i = j + 1
                continue
            if ASSIGN_KW_RE.match(low):
                # still may contain function call on RHS; allow
                pass
            rpar = parse_matching_paren(out, k)
            if rpar < 0:
                i = j + 1
                continue
            old_arg_txt = out[k + 1:rpar]
            new_arg_txt, n_changes = drop_args_and_adapt(old_arg_txt, sig, ranks)
            if n_changes > 0 and new_arg_txt != old_arg_txt:
                out = out[:k + 1] + new_arg_txt + out[rpar:]
                low = out.lower()
                changed += n_changes
                i = k + 1 + len(new_arg_txt) + 1
            else:
                i = rpar + 1
    return out, changed


def rewrite_file_callers_active(lines: List[str], active: Dict[str, ProcSig]) -> Tuple[List[str], int]:
    if not active:
        return lines, 0

    def iter_statement_ranges(src_lines: List[str]) -> List[Tuple[int, int, str]]:
        out_ranges: List[Tuple[int, int, str]] = []
        cur_parts: List[str] = []
        cur_start: Optional[int] = None
        cur_end: Optional[int] = None
        need_more = False

        for lineno, raw in enumerate(src_lines, start=1):
            code = fscan.strip_comment(raw).rstrip("\r\n")
            seg = code.rstrip()
            if not seg and not need_more:
                continue
            if cur_start is None:
                cur_start = lineno
            cur_end = lineno

            if cur_parts:
                lead = seg.lstrip()
                if lead.startswith("&"):
                    seg = lead[1:].lstrip()

            seg = seg.rstrip()
            has_trailing_cont = seg.endswith("&")
            if has_trailing_cont:
                seg = seg[:-1].rstrip()

            if seg:
                cur_parts.append(seg)

            need_more = has_trailing_cont
            if need_more:
                continue

            joined = " ".join(cur_parts).strip()
            if joined and cur_start is not None and cur_end is not None:
                for stmt in fscan.split_fortran_statements(joined):
                    if stmt:
                        out_ranges.append((cur_start, cur_end, stmt))
            cur_parts = []
            cur_start = None
            cur_end = None

        if cur_parts and cur_start is not None and cur_end is not None:
            joined = " ".join(cur_parts).strip()
            for stmt in fscan.split_fortran_statements(joined):
                if stmt:
                    out_ranges.append((cur_start, cur_end, stmt))
        return out_ranges

    out = list(lines)
    ranks = declared_ranks(lines)
    total = 0
    edits: List[Tuple[int, int, str]] = []
    for ln1, ln2, stmt in iter_statement_ranges(lines):
        new_stmt, n = rewrite_stmt_invocations(stmt, active, ranks)
        if n <= 0 or new_stmt == stmt:
            continue
        raw0 = out[ln1 - 1]
        indent = re.match(r"^\s*", raw0).group(0) if raw0 is not None else ""
        comment = ""
        nl = "\n"
        if ln1 == ln2:
            code0, cmt0 = split_code_comment(raw0.rstrip("\n"))
            if code0.strip() == stmt.strip():
                comment = cmt0
            nl = "\n" if raw0.endswith("\n") else ""
        edits.append((ln1, ln2, f"{indent}{new_stmt}{comment}{nl}"))
        total += n

    for ln1, ln2, repl in reversed(edits):
        out[ln1 - 1:ln2] = [repl]
    return out, total


def rewrite_file_callers(
    lines: List[str],
    available: Dict[str, Dict[str, ProcSig]],
    *,
    skip_names: Optional[Set[str]] = None,
) -> Tuple[List[str], int]:
    use_map = parse_use_map(lines)
    active: Dict[str, ProcSig] = {}
    skip = {n.lower() for n in (skip_names or set())}
    for mod, only_set in use_map.items():
        procs = available.get(mod, {})
        if not procs:
            continue
        if only_set is None:
            for pn, sig in procs.items():
                if pn.lower() in skip:
                    continue
                active[pn] = sig
        else:
            for pn in only_set:
                if pn.lower() in skip:
                    continue
                sig = procs.get(pn)
                if sig is not None:
                    active[pn] = sig
    return rewrite_file_callers_active(lines, active)


def _is_literal_one(expr: str) -> bool:
    s = expr.strip().lower()
    s = s.replace(" ", "")
    # Accept common forms: 1, +1, 1.0, 1.0d0, 1_rk, 1.0_rk
    if re.fullmatch(r"\+?1(?:\.0*)?(?:[de][\+\-]?\d+)?(?:_[a-z0-9_]+)?", s):
        return True
    return False


def _is_bare_name(expr: str) -> bool:
    return re.fullmatch(r"[a-z][a-z0-9_]*", expr.strip(), re.IGNORECASE) is not None


def has_sequence_association_risk(
    plan: xas.ProcWrapPlan,
    set_lines: Dict[Path, List[str]],
) -> bool:
    """Conservative guard for legacy calls that rely on explicit-shape sequence association.

    Example risk:
      subroutine foo(m,n,x)  ! x(m,n)
      call foo(m,1,xvec)     ! xvec rank-1; valid legacy, invalid assumed-shape
    """
    if plan.proc_kind != "subroutine":
        return False
    removed = set(plan.removed_dim_args)
    if not removed:
        return False

    # Build per-dummy metadata from original signature.
    rank_sensitive: List[Tuple[int, int, List[int], bool, bool]] = []
    for nm, di in plan.dummies.items():
        if not di.is_array or len(di.dims) < 1:
            continue
        try:
            dummy_pos = plan.original_args.index(nm)
        except ValueError:
            continue
        dim_positions: List[int] = []
        has_star_dim = any(d.strip() == "*" for d in di.dims)
        for d in di.dims:
            key = d.strip().lower()
            if key in removed:
                try:
                    dim_positions.append(plan.original_args.index(key))
                except ValueError:
                    pass
        if dim_positions or has_star_dim:
            accept_expr = True
            if plan.dummy_accept_expr is not None and nm in plan.dummy_accept_expr:
                accept_expr = bool(plan.dummy_accept_expr[nm])
            rank_sensitive.append((dummy_pos, len(di.dims), dim_positions, has_star_dim, accept_expr))
    # Skip when an array dummy uses a non-trivial bound expression
    # involving removed dimension args (for example u(nx*ny)); this often
    # indicates flattened-storage call patterns that assumed-shape may break.
    for nm, di in plan.dummies.items():
        if not di.is_array:
            continue
        for d in di.dims:
            ds = d.strip().lower()
            if not ds:
                continue
            if not xas.expr_refs_any_name(ds, removed):
                continue
            if not xas.IDENT_RE.match(ds):
                return True

    if not rank_sensitive:
        return False

    pname = plan.proc_name.lower()
    for lines in set_lines.values():
        ranks = declared_ranks(lines)
        for _ln, stmt in fscan.iter_fortran_statements(lines):
            m = CALL_RE.match(stmt.strip())
            if not m:
                continue
            if m.group(1).lower() != pname:
                continue
            args = [a.strip() for a in xas.split_top_level_commas(m.group(2))]
            if not args:
                continue
            for dummy_pos, needed_rank, dim_positions, _has_star_dim, accept_expr in rank_sensitive:
                if dummy_pos >= len(args):
                    continue
                arr_actual = args[dummy_pos]
                if not _is_bare_name(arr_actual):
                    continue
                rk = ranks.get(arr_actual.strip().lower())
                if rk is not None:
                    if needed_rank > 0 and rk < needed_rank:
                        return True
                    if needed_rank == 1 and rk > 1 and not accept_expr:
                        return True
                    if needed_rank > 1 and rk > needed_rank:
                        return True
                for dp in dim_positions:
                    if dp >= len(args):
                        continue
                    if _is_literal_one(args[dp]):
                        return True
    return False


def plan_to_sig(p: xas.ProcWrapPlan) -> ProcSig:
    drops: List[int] = []
    drop_names = set(p.removed_dim_args)
    for i, a in enumerate(p.original_args):
        if a in drop_names:
            drops.append(i)
    dranks: Dict[int, int] = {}
    daccept: Dict[int, bool] = {}
    for i, a in enumerate(p.original_args):
        di = p.dummies.get(a)
        if di is None or not di.is_array:
            dranks[i] = 0
            daccept[i] = True
        else:
            dranks[i] = len(di.dims)
            if p.dummy_accept_expr is not None and a in p.dummy_accept_expr:
                daccept[i] = bool(p.dummy_accept_expr[a])
            else:
                lhs_nospace = di.decl_lhs.lower().replace(" ", "")
                has_out = "intent(out)" in lhs_nospace
                has_inout = "intent(inout)" in lhs_nospace
                daccept[i] = not (has_out or has_inout)
    return ProcSig(
        module_name=p.module_name,
        proc_name=p.proc_name,
        drop_positions=drops,
        drop_names=drop_names,
        dummy_ranks=dranks,
        dummy_accept_expr=daccept,
    )


def apply_fix(path: Path, new_text: str, *, out_path: Optional[Path], create_backup: bool) -> Tuple[int, Optional[Path]]:
    old_text = path.read_text(encoding="utf-8", errors="ignore")
    if old_text == new_text:
        if out_path is not None:
            out_path.write_text(old_text, encoding="utf-8")
        return 0, None
    backup: Optional[Path] = None
    target = out_path if out_path is not None else path
    if out_path is None and create_backup:
        backup = xas.make_backup_path(path)
        shutil.copy2(path, backup)
    target.write_text(new_text, encoding="utf-8")
    return 1, backup


def main() -> int:
    ap = argparse.ArgumentParser(
        description="Convert explicit-shape module procedures across a file set and update later callers."
        ,
        allow_abbrev=False
    )
    ap.add_argument("files", nargs="*", type=Path)
    ap.add_argument("--codes", type=Path)
    ap.add_argument("--code-dir", type=Path)
    ap.add_argument("--start", type=int, default=1, help="1-based set index to start from.")
    ap.add_argument("--limit", type=int, help="Maximum number of sets to process.")
    ap.add_argument("--resume", action="store_true", help="Resume from saved set index in --state-file.")
    ap.add_argument("--state-file", type=Path, default=Path(".xassumed_shape_set.state"))
    ap.add_argument("--exclude", action="append", default=[])
    ap.add_argument("--verbose", action="store_true")
    ap.add_argument("--fix", action="store_true")
    ap.add_argument("--replace", action="store_true", help="Enable in-place explicit->assumed shape replacement.")
    ap.add_argument("--out", type=Path)
    ap.add_argument("--out-dir", type=Path)
    ap.add_argument("--backup", dest="backup", action="store_true", default=True)
    ap.add_argument("--no-backup", dest="backup", action="store_false")
    ap.add_argument("--diff", action="store_true")
    ap.add_argument("--infer-intent-in", dest="infer_intent_in", action="store_true", default=True,
                    help="Infer scalar INTEGER dimension args as input when INTENT(IN) is missing (default: on).")
    ap.add_argument("--no-infer-intent-in", dest="infer_intent_in", action="store_false",
                    help="Disable intent inference for dimension args.")
    ap.add_argument("--replace-note", action="store_true")
    ap.add_argument("--compiler", type=str)
    ap.add_argument("--fail-fast", action="store_true", help="Stop on first compile/link failure (default: keep going).")
    ap.add_argument("--include-no-module", action="store_true",
                    help="Include sets that define no modules (default: skipped).")
    ap.add_argument("--tee", action="store_true")
    ap.add_argument("--tee-both", action="store_true")
    ap.add_argument("--run", action="store_true")
    ap.add_argument("--run-both", action="store_true")
    ap.add_argument("--run-diff", action="store_true")
    ap.add_argument("--quiet-run", action="store_true")
    ap.add_argument("--keep-exe", action="store_true")
    args = ap.parse_args()

    if args.run_diff:
        args.run_both = True
    if args.run_both:
        args.run = True
    if args.tee_both:
        args.tee = True
    if args.replace:
        args.fix = True
    if args.run:
        args.fix = True
    if args.out is not None:
        args.fix = True
    if args.out_dir is not None:
        args.fix = True
    if args.replace_note and not args.replace:
        print("--replace-note requires --replace.")
        return 2
    if args.out is not None and args.out_dir is not None:
        print("Use either --out or --out-dir, not both.")
        return 2
    if args.out_dir is not None:
        if args.out_dir.exists() and not args.out_dir.is_dir():
            print("--out-dir exists but is not a directory.")
            return 2
        args.out_dir.mkdir(parents=True, exist_ok=True)

    if args.files:
        input_paths = args.files
        if args.code_dir is not None:
            input_paths = [p if p.is_absolute() else (args.code_dir / p) for p in input_paths]
        files = fscan.apply_excludes(input_paths, args.exclude)
        entries = [[p] for p in files]
    elif args.codes is not None:
        raw_entries = resolve_code_entries(load_codes(args.codes), args.code_dir)
        entries: List[List[Path]] = []
        for g in raw_entries:
            gg = fscan.apply_excludes(g, args.exclude)
            if gg:
                entries.append(gg)
    else:
        print("No input files specified.")
        return 2

    # By default, skip sets that have no module definitions at all.
    skipped_no_module = 0
    if not args.include_no_module:
        keep_entries: List[List[Path]] = []
        has_module_cache: Dict[Path, bool] = {}
        for g in entries:
            set_has_module = False
            for p in g:
                pp = p.resolve()
                hm = has_module_cache.get(pp)
                if hm is None:
                    try:
                        infos, any_missing = fscan.load_source_files([pp])
                        hm = (not any_missing) and bool(infos) and bool(infos[0].defined_modules)
                    except Exception:
                        hm = True
                    has_module_cache[pp] = hm
                if hm:
                    set_has_module = True
                    break
            if set_has_module:
                keep_entries.append(g)
            else:
                skipped_no_module += 1
        entries = keep_entries

    if args.start < 1:
        print("--start must be >= 1.")
        return 2
    if args.limit is not None and args.limit < 1:
        print("--limit must be >= 1.")
        return 2

    resume_start = 1
    if args.resume and args.state_file.exists():
        try:
            txt = args.state_file.read_text(encoding="utf-8", errors="ignore").strip()
            resume_start = max(1, int(txt))
        except Exception:
            resume_start = 1

    if args.resume and args.start != 1:
        print("Resume note: --start is set; ignoring resume position from state.")
    effective_start = args.start if args.start != 1 else resume_start

    if effective_start > len(entries):
        print(f"--start {effective_start} is beyond available sets ({len(entries)}).")
        return 2

    selected_entries = entries[effective_start - 1 :]
    if args.limit is not None:
        selected_entries = selected_entries[: args.limit]

    if not selected_entries:
        print("No Fortran files found.")
        return 0
    if args.out is not None:
        if len(selected_entries) != 1 or len(selected_entries[0]) != 1:
            print("--out supports exactly one input file.")
            return 2
    if args.tee and not args.fix:
        print("--tee/--tee-both require --fix mode.")
        return 2

    if skipped_no_module and args.verbose:
        print(f"Skipped no-module sets: {skipped_no_module}")

    compile_fail_baseline = 0
    compile_fail_after = 0
    all_module_changes = 0
    all_call_changes = 0
    changed_files = 0
    run_sets = 0
    run_match = 0
    run_diff = 0
    run_skip = 0
    run_fail = 0
    processed_sets = 0
    stop_early = False

    for rel_idx, g in enumerate(selected_entries, start=1):
        tag = f"[{rel_idx}/{len(selected_entries)}]"
        baseline_ok = True
        baseline_run_ok = True
        orig_out = ""
        orig_err = ""
        xform_out = ""
        xform_err = ""
        if args.compiler:
            baseline_ok = fbuild.run_compiler_command(args.compiler, g, f"baseline {tag}", fscan.display_path)
            if not baseline_ok:
                compile_fail_baseline += 1
                if args.fail_fast:
                    stop_early = True
        if baseline_ok and args.run_both:
            main_stem = g[-1].stem if g else "set"
            exe_orig = Path(f"{rel_idx:04d}_{main_stem}_orig.exe")
            baseline_run_ok, orig_out, orig_err = fbuild.compile_and_run_source(
                g[-1] if len(g) == 1 else g[-1],
                label=f"original {tag}",
                quiet_run=args.quiet_run,
                keep_exe=args.keep_exe,
                exe_path=exe_orig,
            ) if len(g) == 1 else _compile_and_run_set(
                g,
                label=f"original {tag}",
                exe_path=exe_orig,
                quiet_run=args.quiet_run,
                keep_exe=args.keep_exe,
            )
            if not baseline_run_ok and args.fail_fast:
                stop_early = True

        # Build and apply plans set-local so callers are rewritten within the set.
        per_new: Dict[Path, str] = {}
        per_mod_count: Dict[Path, int] = {}
        per_call_count: Dict[Path, int] = {}
        available: Dict[str, Dict[str, ProcSig]] = {}

        if not baseline_ok:
            print(f"Skip set {tag}: baseline compile failed.")
        elif not stop_early:
            set_lines: Dict[Path, List[str]] = {}
            for pp in g:
                txt = pp.read_text(encoding="utf-8", errors="ignore")
                set_lines[pp] = txt.splitlines(keepends=True)
            for p in g:
                lines = set_lines[p]
                mods = xas.scan_modules(lines)
                procs = fscan.parse_procedures(lines)

                plans: List[xas.ProcWrapPlan] = []
                for pr in procs:
                    mod = xas.module_for_proc(pr, mods)
                    if mod is None:
                        continue
                    plan = xas.build_wrap_plan(
                        lines, pr, mod, infer_intent_in=args.infer_intent_in, replace=args.replace
                    )
                    if plan is None:
                        continue
                    if has_sequence_association_risk(plan, set_lines):
                        continue
                    plans.append(plan)

                new_lines = lines
                applied: List[xas.ProcWrapPlan] = []
                if plans:
                    new_lines, applied = xas.apply_all_replace(new_lines, plans)
                    if args.replace_note:
                        new_lines = xas.add_replace_notes(new_lines, xas.scan_modules(new_lines), applied)

                # Rewrite calls to procedures replaced in this same file/module.
                local_active: Dict[str, ProcSig] = {pl.proc_name: plan_to_sig(pl) for pl in applied}
                new_lines, local_call_n = rewrite_file_callers_active(new_lines, local_active)

                for pl in applied:
                    available.setdefault(pl.module_name, {})[pl.proc_name] = plan_to_sig(pl)

                new_lines, use_call_n = rewrite_file_callers(
                    new_lines, available, skip_names=set(local_active.keys())
                )
                call_n = local_call_n + use_call_n
                per_new[p] = "".join(new_lines)
                per_mod_count[p] = len(applied)
                per_call_count[p] = call_n
                all_module_changes += len(applied)
                all_call_changes += call_n

            if args.verbose:
                set_mod = sum(per_mod_count.values())
                set_call = sum(per_call_count.values())
                print(f"set {tag}: module procedures converted {set_mod}, caller invocations rewritten {set_call}")
            else:
                for p in g:
                    mc = per_mod_count.get(p, 0)
                    cc = per_call_count.get(p, 0)
                    if mc or cc:
                        print(f"{fscan.display_path(p)}: module {mc}, callers {cc}")

            if args.fix:
                transformed_set: List[Path] = []
                set_changed = False
                for p in g:
                    old = p.read_text(encoding="utf-8", errors="ignore")
                    new = per_new.get(p, old)
                    if args.diff and old != new:
                        show_diff(old, new, p)
                    out_path: Optional[Path] = None
                    if args.out is not None:
                        out_path = args.out
                        transformed_set.append(out_path)
                    elif args.out_dir is not None:
                        out_path = args.out_dir / p.name
                        transformed_set.append(out_path)
                    else:
                        transformed_set.append(p)
                    changed, backup = apply_fix(p, new, out_path=out_path, create_backup=args.backup)
                    if changed:
                        set_changed = True
                        changed_files += 1
                        if out_path is not None:
                            print(f"Fixed {fscan.display_path(p)}: wrote {fscan.display_path(out_path)}")
                        else:
                            msg = f"Fixed {fscan.display_path(p)}"
                            if backup is not None:
                                msg += f", backup {fscan.display_path(backup)}"
                            print(msg)
                    if args.tee:
                        if args.tee_both:
                            print(f"--- original: {fscan.display_path(p)} ---")
                            print(old, end="")
                            if not old.endswith("\n"):
                                print("")
                        tpath = out_path if out_path is not None else p
                        ttxt = tpath.read_text(encoding="utf-8", errors="ignore")
                        print(f"--- transformed: {fscan.display_path(tpath)} ---")
                        print(ttxt, end="")
                        if not ttxt.endswith("\n"):
                            print("")

                if args.compiler:
                    after_ok = fbuild.run_compiler_command(args.compiler, transformed_set, f"after-fix {tag}", fscan.display_path)
                    if not after_ok:
                        compile_fail_after += 1
                        if args.fail_fast:
                            stop_early = True
                if args.run and baseline_run_ok:
                    run_sets += 1
                    if not set_changed:
                        if args.run_diff:
                            print(f"Run diff {tag}: SKIP (no transformations applied)")
                        run_skip += 1
                    else:
                        main_stem = transformed_set[-1].stem if transformed_set else (g[-1].stem if g else "set")
                        exe_new = Path(f"{rel_idx:04d}_{main_stem}_trans.exe")
                        ok_run, xform_out, xform_err = _compile_and_run_set(
                            transformed_set,
                            label=f"transformed {tag}",
                            exe_path=exe_new,
                            quiet_run=args.quiet_run,
                            keep_exe=args.keep_exe,
                        )
                        if not ok_run:
                            run_fail += 1
                            if args.fail_fast:
                                stop_early = True
                        elif args.run_diff:
                            same = (orig_out == xform_out) and (orig_err == xform_err)
                            if same:
                                run_match += 1
                                print(f"Run diff {tag}: MATCH")
                            else:
                                run_diff += 1
                                print(f"Run diff {tag}: DIFF")
                                ob = f"STDOUT:\n{orig_out}\nSTDERR:\n{orig_err}\n"
                                tb = f"STDOUT:\n{xform_out}\nSTDERR:\n{xform_err}\n"
                                for line in difflib.unified_diff(
                                    ob.splitlines(),
                                    tb.splitlines(),
                                    fromfile=f"original {tag}",
                                    tofile=f"transformed {tag}",
                                    lineterm="",
                                ):
                                    print(line)
        processed_sets = rel_idx
        if stop_early:
            break
        print("")

    if args.fix:
        print(
            f"\n--fix summary: files changed {changed_files}, module procedures converted {all_module_changes}, "
            f"caller invocations rewritten {all_call_changes}"
        )
    else:
        print(
            f"\n--summary: module procedures converted {all_module_changes}, "
            f"caller invocations rewritten {all_call_changes}"
        )
    if args.compiler:
        print(
            f"--compile summary: baseline failures {compile_fail_baseline}, "
            f"after-fix failures {compile_fail_after}"
        )
    if args.run:
        print(
            f"--run summary: sets attempted {run_sets}, failures {run_fail}, "
            f"diff match {run_match}, diff mismatch {run_diff}, diff skipped {run_skip}"
        )

    # Save next 1-based set index for --resume.
    try:
        next_set_index = effective_start + processed_sets
        args.state_file.write_text(str(next_set_index) + "\n", encoding="utf-8")
    except Exception:
        pass
    if stop_early:
        return 1
    if compile_fail_baseline or compile_fail_after:
        return 1
    return 0


def _compile_and_run_set(
    files: List[Path],
    *,
    label: str,
    exe_path: Path,
    quiet_run: bool,
    keep_exe: bool,
) -> Tuple[bool, str, str]:
    cmd = ["gfortran"] + [str(p) for p in files] + ["-o", str(exe_path)]
    print(f"Build ({label}): {' '.join(fbuild.quote_cmd_arg(x) for x in cmd)}")
    import subprocess

    cp = subprocess.run(cmd, capture_output=True, text=True)
    if cp.returncode != 0:
        print(f"Build ({label}): FAIL (exit {cp.returncode})")
        if cp.stdout:
            print(cp.stdout.rstrip())
        if cp.stderr:
            print(fbuild.format_linker_stderr(cp.stderr).rstrip())
        return False, "", ""
    print(f"Build ({label}): PASS")
    print(f"Run ({label}): {fbuild.quote_cmd_arg(str(exe_path))}")
    rp = subprocess.run([str(exe_path)], capture_output=True, text=True)
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
                exe_path.unlink(missing_ok=True)
            except Exception:
                pass


if __name__ == "__main__":
    raise SystemExit(main())
