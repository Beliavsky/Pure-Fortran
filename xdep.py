#!/usr/bin/env python3
"""Find source dependencies for a main program from USE-module closures."""

from __future__ import annotations

import argparse
import glob
import json
import re
import subprocess
from pathlib import Path
from typing import Dict, List, Optional, Set, Tuple

import fortran_build as fbuild
import fortran_scan as fscan


PROGRAM_START_RE = re.compile(r"^\s*(?:\d+\s+)?program\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
END_PROGRAM_RE = re.compile(r"^\s*(?:\d+\s+)?end\s+program\b", re.IGNORECASE)
CACHE_DEFAULT = Path(".xdep_cache.json")


def cache_key(path: Path) -> str:
    return str(path.resolve()).lower()


def load_cache(path: Path) -> Dict[str, List[str]]:
    if not path.exists():
        return {}
    try:
        raw = json.loads(path.read_text(encoding="utf-8", errors="ignore"))
    except Exception:
        return {}
    if not isinstance(raw, dict):
        return {}
    out: Dict[str, List[str]] = {}
    for k, v in raw.items():
        if not isinstance(k, str) or not isinstance(v, list):
            continue
        vv: List[str] = []
        for item in v:
            if isinstance(item, str):
                vv.append(item)
        out[k] = vv
    return out


def save_cache(path: Path, data: Dict[str, List[str]]) -> None:
    path.write_text(json.dumps(data, indent=2) + "\n", encoding="utf-8")


def find_main_program_names(lines: List[str]) -> List[str]:
    out: List[str] = []
    for _ln, stmt in fscan.iter_fortran_statements(lines):
        s = stmt.strip()
        if not s:
            continue
        if END_PROGRAM_RE.match(s):
            continue
        m = PROGRAM_START_RE.match(s)
        if m:
            out.append(m.group(1).lower())
    return out


def build_module_provider_map(infos: List[fscan.SourceFileInfo]) -> Dict[str, List[Path]]:
    providers: Dict[str, List[Path]] = {}
    for info in infos:
        for mod in info.defined_modules:
            providers.setdefault(mod, []).append(info.path.resolve())
    return providers


def resolve_closure(
    root: Path,
    by_path: Dict[Path, fscan.SourceFileInfo],
    providers: Dict[str, List[Path]],
) -> Tuple[List[Path], Set[str], Optional[Tuple[str, List[Path]]]]:
    unresolved: Set[str] = set()
    needed: Set[Path] = {root}
    queue: List[Path] = [root]

    while queue:
        p = queue.pop(0)
        info = by_path.get(p)
        if info is None:
            continue
        for mod in sorted(info.used_modules):
            provs = providers.get(mod, [])
            if not provs:
                unresolved.add(mod)
                continue
            if len(provs) > 1:
                return sorted(needed, key=lambda x: str(x).lower()), unresolved, (mod, sorted(provs))
            prov = provs[0]
            if prov not in needed:
                needed.add(prov)
                queue.append(prov)

    needed_infos = [by_path[p] for p in needed if p in by_path]
    ordered_infos, _had_cycle = fscan.order_files_least_dependent(needed_infos)
    ordered = [f.path for f in ordered_infos]
    return ordered, unresolved, None


def main() -> int:
    ap = argparse.ArgumentParser(description="Find source files needed by a main program using USE-module closure.")
    ap.add_argument("target", type=Path, help="Main source file, or directory (process all files with one main PROGRAM).")
    ap.add_argument("--out", type=Path, help="Write ordered dependency list to this file.")
    ap.add_argument("--verbose", action="store_true")
    ap.add_argument("--refresh", action="store_true", help="Force dependency search and refresh cache.")
    ap.add_argument("--cache-file", type=Path, default=CACHE_DEFAULT, help="Dependency cache path (default: .xdep_cache.json).")
    ap.add_argument("--compiler", type=str, help="Compiler executable/prefix for compile check (shorthand for --compile-cmd '<compiler> -c {files}').")
    ap.add_argument("--compile-cmd", type=str, help="Optional compile command template over resolved files.")
    ap.add_argument("--build", action="store_true", help="Build executable with gfortran using incremental object recompilation.")
    ap.add_argument("--rebuild", action="store_true", help="Force recompilation of all objects before linking (implies --build).")
    ap.add_argument("--run", action="store_true", help="Build executable with gfortran from resolved files and run it.")
    args = ap.parse_args()
    if args.run:
        args.build = True
    if args.rebuild:
        args.build = True

    target = args.target
    target_str = str(target)
    has_glob = any(ch in target_str for ch in ["*", "?", "["])

    def process_one(
        root_file: Path,
        cache: Dict[str, List[str]],
        shared_by_path: Optional[Dict[Path, fscan.SourceFileInfo]] = None,
        shared_providers: Optional[Dict[str, List[Path]]] = None,
        shared_src_dir: Optional[Path] = None,
    ) -> int:
        root = root_file.resolve()
        root_text = root.read_text(encoding="utf-8", errors="ignore")
        root_lines = root_text.splitlines()
        mains = find_main_program_names(root_lines)
        if len(mains) != 1:
            if len(mains) == 0:
                print(f"{fscan.display_path(root)} does not contain a main program.")
            else:
                print(f"{fscan.display_path(root)} contains multiple main program units: {', '.join(mains)}")
            return 2

        src_dir = shared_src_dir if shared_src_dir is not None else root.parent
        ckey = cache_key(root)
        ordered: List[Path] = []
        used_cache = False
        if not args.refresh and ckey in cache:
            ordered = [Path(s) for s in cache[ckey]]
            used_cache = True
        else:
            if shared_by_path is not None and shared_providers is not None:
                by_path = shared_by_path
                providers = shared_providers
            else:
                candidates = sorted(set(src_dir.glob("*.f90")) | set(src_dir.glob("*.F90")), key=lambda p: p.name.lower())
                infos, any_missing = fscan.load_source_files(candidates)
                if any_missing:
                    return 2
                by_path = {i.path.resolve(): i for i in infos}
                providers = build_module_provider_map(infos)
            if root not in by_path:
                print(f"Could not load source metadata for {fscan.display_path(root)}")
                return 2
            ordered, unresolved, dup = resolve_closure(root, by_path, providers)

            if dup is not None:
                mod, provs = dup
                print(f"Module multiply defined: {mod}")
                for p in provs:
                    print(f"  {fscan.display_path(p)}")
                return 2

            if unresolved:
                print("Unresolved used module(s): " + ", ".join(sorted(unresolved)))
                return 2
            cache[ckey] = [str(p) for p in ordered]

        dep_line = " ".join(fbuild.quote_cmd_arg(str(p)) for p in ordered)

        if args.verbose:
            print(f"Main program: {mains[0]}")
            if used_cache:
                print(f"Dependencies loaded from cache: {fscan.display_path(args.cache_file)}")
            else:
                print(f"Directory scanned: {fscan.display_path(src_dir)}")
            print(f"Resolved files: {len(ordered)}")
            print(f"Dependencies: {dep_line}")
        else:
            print(dep_line)

        if args.out is not None:
            args.out.write_text(dep_line + "\n", encoding="utf-8")
            if args.verbose:
                print(f"Wrote dependency list: {fscan.display_path(args.out)}")

        compile_cmd = args.compile_cmd
        if compile_cmd is None and args.compiler:
            compile_cmd = f"{args.compiler} -c {{files}}"

        if compile_cmd:
            if not fbuild.run_compiler_command(compile_cmd, ordered, "resolved-set", fscan.display_path):
                return 1

        exe = root.with_suffix(".exe")
        if args.build:
            objects = [p.with_suffix(".o") for p in ordered]
            rebuilt_any = False
            for src, obj in zip(ordered, objects):
                needs_compile = args.rebuild or (not obj.exists())
                if (not needs_compile) and src.exists() and obj.exists():
                    try:
                        needs_compile = obj.stat().st_mtime < src.stat().st_mtime
                    except OSError:
                        needs_compile = True
                if not needs_compile:
                    continue
                ccmd = f"gfortran -c {fbuild.quote_cmd_arg(str(src))} -o {fbuild.quote_cmd_arg(str(obj))}"
                cdisp = f"gfortran -c {fbuild.quote_cmd_arg(fscan.display_path(src))} -o {fbuild.quote_cmd_arg(fscan.display_path(obj))}"
                print(f"Compile (build): {cdisp}")
                cp = subprocess.run(ccmd, shell=True, capture_output=True, text=True)
                if cp.returncode != 0:
                    print(f"Compile (build): FAIL (exit {cp.returncode})")
                    if cp.stdout:
                        print(cp.stdout.rstrip())
                    if cp.stderr:
                        print(cp.stderr.rstrip())
                    return 1
                print("Compile (build): PASS")
                rebuilt_any = True

            needs_link = rebuilt_any or (not exe.exists())
            if (not needs_link) and exe.exists():
                try:
                    exe_m = exe.stat().st_mtime
                    needs_link = any((not o.exists()) or (o.stat().st_mtime > exe_m) for o in objects)
                except OSError:
                    needs_link = True
            if needs_link:
                obj_args = " ".join(fbuild.quote_cmd_arg(str(o)) for o in objects)
                lcmd = f"gfortran {obj_args} -o {fbuild.quote_cmd_arg(str(exe))}"
                ldisp = f"gfortran {' '.join(fbuild.quote_cmd_arg(fscan.display_path(o)) for o in objects)} -o {fbuild.quote_cmd_arg(fscan.display_path(exe))}"
                print(f"Link (build): {ldisp}")
                lp = subprocess.run(lcmd, shell=True, capture_output=True, text=True)
                if lp.returncode != 0:
                    print(f"Link (build): FAIL (exit {lp.returncode})")
                    if lp.stdout:
                        print(lp.stdout.rstrip())
                    if lp.stderr:
                        print(lp.stderr.rstrip())
                    return 1
                print("Link (build): PASS")
            elif args.verbose:
                print("Link (build): SKIP (up to date)")

        if args.run:
            run_cmd = fbuild.quote_cmd_arg(str(exe))
            print(f"Run: {fbuild.quote_cmd_arg(fscan.display_path(exe))}")
            rp = subprocess.run(run_cmd, shell=True, capture_output=True, text=True)
            if rp.returncode != 0:
                print(f"Run: FAIL (exit {rp.returncode})")
                if rp.stdout:
                    print(rp.stdout.rstrip())
                if rp.stderr:
                    print(rp.stderr.rstrip())
                return 1
            print("Run: PASS")
            if rp.stdout:
                print(rp.stdout.rstrip())
            if rp.stderr:
                print(rp.stderr.rstrip())
        return 0

    cache = load_cache(args.cache_file)
    dirty_cache = False
    if has_glob:
        if args.out is not None:
            print("--out is only supported for single-file mode.")
            return 2
        matches = sorted(
            [Path(p) for p in glob.glob(target_str)],
            key=lambda p: p.name.lower(),
        )
        files = [p for p in matches if p.is_file() and p.suffix.lower() == ".f90"]
        if not files:
            print(f"No files matched pattern: {fscan.display_path(target)}")
            return 2
        roots: List[Path] = []
        for p in files:
            txt = p.read_text(encoding="utf-8", errors="ignore")
            mains = find_main_program_names(txt.splitlines())
            if len(mains) == 1:
                roots.append(p)
        if not roots:
            print(f"No main-program source files matched pattern: {fscan.display_path(target)}")
            return 2
        # Build shared indexes once per parent directory.
        by_dir: Dict[Path, Dict[Path, fscan.SourceFileInfo]] = {}
        prov_by_dir: Dict[Path, Dict[str, List[Path]]] = {}
        for f in roots:
            d = f.resolve().parent
            if d in by_dir:
                continue
            candidates = sorted(set(d.glob("*.f90")) | set(d.glob("*.F90")), key=lambda p: p.name.lower())
            infos, any_missing = fscan.load_source_files(candidates)
            if any_missing:
                return 2
            by_dir[d] = {i.path.resolve(): i for i in infos}
            prov_by_dir[d] = build_module_provider_map(infos)

        for i, r in enumerate(roots, start=1):
            d = r.resolve().parent
            if args.verbose:
                if i > 1:
                    print("")
                print(f"[{i}/{len(roots)}] {fscan.display_path(r)}")
            before = dict(cache)
            rc = process_one(
                r,
                cache,
                shared_by_path=by_dir[d],
                shared_providers=prov_by_dir[d],
                shared_src_dir=d,
            )
            if rc != 0:
                return rc
            if cache != before:
                dirty_cache = True
    elif target.is_dir():
        if args.out is not None:
            print("--out is only supported for single-file mode.")
            return 2
        files = sorted(set(target.glob("*.f90")) | set(target.glob("*.F90")), key=lambda p: p.name.lower())
        # Build a shared directory index once; reused for each uncached main.
        infos, any_missing = fscan.load_source_files(files)
        if any_missing:
            return 2
        shared_by_path: Dict[Path, fscan.SourceFileInfo] = {i.path.resolve(): i for i in infos}
        shared_providers = build_module_provider_map(infos)
        roots: List[Path] = []
        for p in files:
            txt = p.read_text(encoding="utf-8", errors="ignore")
            mains = find_main_program_names(txt.splitlines())
            if len(mains) == 1:
                roots.append(p)
        if not roots:
            print(f"No main-program source files found in {fscan.display_path(target)}.")
            return 2
        for i, r in enumerate(roots, start=1):
            if args.verbose:
                if i > 1:
                    print("")
                print(f"[{i}/{len(roots)}] {fscan.display_path(r)}")
            before = dict(cache)
            rc = process_one(
                r,
                cache,
                shared_by_path=shared_by_path,
                shared_providers=shared_providers,
                shared_src_dir=target.resolve(),
            )
            if rc != 0:
                return rc
            if cache != before:
                dirty_cache = True
    else:
        if not target.exists():
            print(f"File not found: {fscan.display_path(target)}")
            return 2
        before = dict(cache)
        rc = process_one(target, cache)
        if rc != 0:
            return rc
        if cache != before:
            dirty_cache = True

    if dirty_cache:
        try:
            save_cache(args.cache_file, cache)
        except Exception:
            pass
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
