#!/usr/bin/env python3
"""Lift internal Fortran procedures into module procedures.

This module is reusable by other scripts via `transform_text()`/`transform_file()`.
It handles:
1) internal procedures of a main program: creates a sibling module before program
2) internal procedures of module procedures: promotes to same enclosing module
"""

from __future__ import annotations

import argparse
import re
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, List, Optional, Sequence, Tuple

import fortran_scan as fscan


UNIT_PROC_START_RE = re.compile(
    r"^\s*(?:(?:pure|elemental|impure|recursive|module)\s+)*(?:"
    r"(?:integer|real(?:\s*\([^)]*\))?|logical|character(?:\s*\([^)]*\))?|"
    r"complex(?:\s*\([^)]*\))?|double\s+precision|type\s*\([^)]*\)|class\s*\([^)]*\))\s+)?"
    r"(function|subroutine)\s+([a-z][a-z0-9_]*)\b",
    re.IGNORECASE,
)
UNIT_MODULE_START_RE = re.compile(r"^\s*module\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
UNIT_PROGRAM_START_RE = re.compile(r"^\s*program\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
END_MODULE_RE = re.compile(r"^\s*end\s+module(?:\s+[a-z][a-z0-9_]*)?\b|\s*end\s*$", re.IGNORECASE)
END_PROGRAM_RE = re.compile(r"^\s*end\s+program(?:\s+[a-z][a-z0-9_]*)?\b|\s*end\s*$", re.IGNORECASE)
END_PROC_RE = re.compile(r"^\s*end\s+(function|subroutine)\b|\s*end\s*$", re.IGNORECASE)
CONTAINS_RE = re.compile(r"^\s*contains\b", re.IGNORECASE)
USE_RE = re.compile(
    r"^\s*use\b(?:\s*,\s*(?:non_intrinsic|intrinsic)\s*)?(?:\s*::\s*|\s+)([a-z][a-z0-9_]*)",
    re.IGNORECASE,
)
PARAM_DECL_RE = re.compile(
    r"^\s*(?:integer|real|logical|character|complex|double\s+precision|type\s*\([^)]*\)|class\s*\([^)]*\))\b"
    r"[^!]*\bparameter\b",
    re.IGNORECASE,
)
TYPE_BLOCK_START_RE = re.compile(r"^\s*type\b(?!\s*\()", re.IGNORECASE)
TYPE_BLOCK_END_RE = re.compile(r"^\s*end\s+type\b", re.IGNORECASE)


@dataclass
class UnitNode:
    kind: str
    name: str
    start: int
    end: int = -1
    contains_line: Optional[int] = None
    parent_index: Optional[int] = None
    children: List[int] = field(default_factory=list)


@dataclass
class TransformResult:
    text: str
    changed: bool
    created_modules: List[str]
    lifted_procedures: List[str]
    warnings: List[str]


def _parse_unit_tree(lines: Sequence[str]) -> List[UnitNode]:
    nodes: List[UnitNode] = []
    stack: List[int] = []
    interface_depth = 0
    for lineno, stmt in fscan.iter_fortran_statements(lines):
        low = fscan.strip_comment(stmt).strip().lower()
        if not low:
            continue
        if re.match(r"^\s*(abstract\s+)?interface\b", low):
            interface_depth += 1
            continue
        if re.match(r"^\s*end\s+interface\b", low):
            if interface_depth > 0:
                interface_depth -= 1
            continue
        if interface_depth > 0:
            continue
        if CONTAINS_RE.match(low):
            if stack:
                nodes[stack[-1]].contains_line = lineno
            continue

        m_mod = UNIT_MODULE_START_RE.match(low)
        if m_mod and not low.startswith("module procedure"):
            parent = stack[-1] if stack else None
            idx = len(nodes)
            nodes.append(UnitNode(kind="module", name=m_mod.group(1).lower(), start=lineno, parent_index=parent))
            if parent is not None:
                nodes[parent].children.append(idx)
            stack.append(idx)
            continue
        m_prog = UNIT_PROGRAM_START_RE.match(low)
        if m_prog:
            parent = stack[-1] if stack else None
            idx = len(nodes)
            nodes.append(UnitNode(kind="program", name=m_prog.group(1).lower(), start=lineno, parent_index=parent))
            if parent is not None:
                nodes[parent].children.append(idx)
            stack.append(idx)
            continue
        m_proc = UNIT_PROC_START_RE.match(low)
        if m_proc:
            parent = stack[-1] if stack else None
            idx = len(nodes)
            nodes.append(UnitNode(kind=m_proc.group(1).lower(), name=m_proc.group(2).lower(), start=lineno, parent_index=parent))
            if parent is not None:
                nodes[parent].children.append(idx)
            stack.append(idx)
            continue

        if stack:
            top = nodes[stack[-1]]
            if top.kind == "module" and END_MODULE_RE.match(low):
                top.end = lineno
                stack.pop()
            elif top.kind == "program" and END_PROGRAM_RE.match(low):
                top.end = lineno
                stack.pop()
            elif top.kind in {"function", "subroutine"} and END_PROC_RE.match(low):
                top.end = lineno
                stack.pop()

    total = len(lines)
    while stack:
        idx = stack.pop()
        nodes[idx].end = total
    return nodes


def _collect_program_module_spec(lines: Sequence[str], prog: UnitNode) -> Tuple[List[str], List[str], List[Tuple[int, int]]]:
    """Collect declarations to keep lifted program internals compilable."""
    out: List[str] = []
    type_names: List[str] = []
    type_ranges: List[Tuple[int, int]] = []
    if prog.contains_line is None:
        return out, type_names, type_ranges
    start_i = prog.start
    stop_i = max(prog.start, prog.contains_line - 1)
    i = start_i
    while i < stop_i:
        raw = lines[i]
        code = fscan.strip_comment(raw).strip()
        low = code.lower()
        if not code:
            i += 1
            continue
        if TYPE_BLOCK_START_RE.match(low) and ("::" in low):
            block_start = i + 1  # to 1-based
            m_tn = re.match(r"^\s*type\b[^:]*::\s*([a-z][a-z0-9_]*)\b", low, re.IGNORECASE)
            if m_tn:
                type_names.append(m_tn.group(1).lower())
            while i < stop_i:
                out.append(lines[i])
                if TYPE_BLOCK_END_RE.match(fscan.strip_comment(lines[i]).strip().lower()):
                    type_ranges.append((block_start, i + 1))
                    i += 1
                    break
                i += 1
            continue
        if USE_RE.match(low) or low == "implicit none" or PARAM_DECL_RE.match(low):
            out.append(lines[i])
        i += 1
    return out, type_names, type_ranges


def _insert_use_line(
    program_lines: List[str],
    module_name: str,
    import_names: List[str],
    prog_start: int,
    prog_end: int,
) -> List[str]:
    use_line = f"   use {module_name}, only: {', '.join(sorted(set(import_names)))}\n"
    insert_after = prog_start
    for i in range(prog_start + 1, prog_end):
        if USE_RE.match(fscan.strip_comment(program_lines[i]).strip().lower()):
            insert_after = i
    program_lines.insert(insert_after + 1, use_line)
    return program_lines


def transform_text(text: str, *, module_suffix: str = "_internal_mod") -> TransformResult:
    lines = text.splitlines(keepends=True)
    plain = [ln.rstrip("\r\n") for ln in lines]
    nodes = _parse_unit_tree(plain)
    if not nodes:
        return TransformResult(text=text, changed=False, created_modules=[], lifted_procedures=[], warnings=[])

    warnings: List[str] = []
    created_modules: List[str] = []
    lifted: List[str] = []

    remove_ranges: List[Tuple[int, int]] = []
    module_insertions: Dict[int, List[str]] = {}
    prepend_blocks: List[List[str]] = []
    extra_use_inserts: List[Tuple[int, str, List[str], int]] = []

    # Promote internal procedures of module procedures.
    for idx, n in enumerate(nodes):
        if n.kind not in {"function", "subroutine"} or n.contains_line is None:
            continue
        parent_idx = n.parent_index
        if parent_idx is None:
            continue
        parent = nodes[parent_idx]
        child_procs = [nodes[c] for c in n.children if nodes[c].kind in {"function", "subroutine"}]
        if not child_procs:
            continue
        if parent.kind == "module":
            remove_ranges.append((n.contains_line, n.end - 1))
            end_i = max(parent.end - 1, 0)
            module_insertions.setdefault(end_i, []).append("\n")
            for c in child_procs:
                snippet = lines[c.start - 1 : c.end]
                module_insertions[end_i].extend(snippet)
                if snippet and not snippet[-1].endswith("\n"):
                    module_insertions[end_i].append("\n")
                lifted.append(c.name)
                warnings.append(
                    f"promoted {c.name} to module {parent.name}; verify no host-variable capture from {n.name}"
                )

    # Lift internal procedures of programs to a new module before the program.
    for n in nodes:
        if n.kind != "program" or n.contains_line is None:
            continue
        child_procs = [nodes[c] for c in n.children if nodes[c].kind in {"function", "subroutine"}]
        if not child_procs:
            continue
        new_mod = f"{n.name}{module_suffix}".lower()
        created_modules.append(new_mod)
        proc_names = [c.name for c in child_procs]
        lifted.extend(proc_names)
        remove_ranges.append((n.contains_line, n.end - 1))
        spec_lines, type_names, type_ranges = _collect_program_module_spec(lines, n)
        remove_ranges.extend(type_ranges)
        mod_block: List[str] = [f"module {new_mod}\n"]
        if spec_lines:
            mod_block.extend(spec_lines)
            if not spec_lines[-1].endswith("\n"):
                mod_block.append("\n")
            if spec_lines[-1].strip():
                mod_block.append("\n")
        else:
            mod_block.append("   implicit none\n\n")
        mod_block.append("contains\n\n")
        for c in child_procs:
            snippet = lines[c.start - 1 : c.end]
            mod_block.extend(snippet)
            if snippet and not snippet[-1].endswith("\n"):
                mod_block.append("\n")
            mod_block.append("\n")
        mod_block.append(f"end module {new_mod}\n\n")
        prepend_blocks.append(mod_block)
        extra_use_inserts.append((n.start - 1, new_mod, proc_names + type_names, n.contains_line - 1))

    if not remove_ranges and not prepend_blocks and not module_insertions:
        return TransformResult(text=text, changed=False, created_modules=[], lifted_procedures=[], warnings=[])

    # Apply line removals (1-based inclusive ranges).
    kill = [False] * len(lines)
    for a, b in remove_ranges:
        lo = max(1, a)
        hi = min(len(lines), b)
        for i in range(lo - 1, hi):
            kill[i] = True

    out_lines = [ln for i, ln in enumerate(lines) if not kill[i]]

    # Re-parse after removals to update indices for use insertions.
    out_plain = [ln.rstrip("\r\n") for ln in out_lines]
    out_nodes = _parse_unit_tree(out_plain)
    prog_by_name = {n.name: n for n in out_nodes if n.kind == "program"}

    for _old_start, mod_name, proc_names, _old_contains in extra_use_inserts:
        # insert only if target program still exists
        # module names are derived from program names so reverse map directly
        prog_name = mod_name[: -len(module_suffix)] if mod_name.endswith(module_suffix) else mod_name
        p = prog_by_name.get(prog_name)
        if p is None:
            continue
        out_lines = _insert_use_line(out_lines, mod_name, proc_names, p.start - 1, p.end - 1)
        out_plain = [ln.rstrip("\r\n") for ln in out_lines]
        out_nodes = _parse_unit_tree(out_plain)
        prog_by_name = {n.name: n for n in out_nodes if n.kind == "program"}

    # Insert promoted procedures before end module lines.
    if module_insertions:
        out_plain = [ln.rstrip("\r\n") for ln in out_lines]
        out_nodes = _parse_unit_tree(out_plain)
        by_mod_end: Dict[str, int] = {}
        for n in out_nodes:
            if n.kind == "module":
                by_mod_end[n.name] = n.end - 1
        # map insertion snippets by module name from original mapping line hint
        mod_snippets: Dict[str, List[str]] = {}
        for idx, n in enumerate(nodes):
            if n.kind not in {"function", "subroutine"} or n.contains_line is None:
                continue
            pidx = n.parent_index
            if pidx is None:
                continue
            parent = nodes[pidx]
            if parent.kind != "module":
                continue
            child_procs = [nodes[c] for c in n.children if nodes[c].kind in {"function", "subroutine"}]
            if not child_procs:
                continue
            blk = mod_snippets.setdefault(parent.name, ["\n"])
            for c in child_procs:
                snippet = lines[c.start - 1 : c.end]
                blk.extend(snippet)
                if snippet and not snippet[-1].endswith("\n"):
                    blk.append("\n")
                blk.append("\n")
        if mod_snippets:
            rebuilt: List[str] = []
            for i, ln in enumerate(out_lines):
                inserted = False
                for mod_name, end_idx in by_mod_end.items():
                    if i == end_idx and mod_name in mod_snippets:
                        rebuilt.extend(mod_snippets[mod_name])
                        inserted = True
                        break
                rebuilt.append(ln)
                if inserted:
                    pass
            out_lines = rebuilt

    if prepend_blocks:
        merged: List[str] = []
        for blk in prepend_blocks:
            merged.extend(blk)
        merged.extend(out_lines)
        out_lines = merged

    new_text = "".join(out_lines)
    return TransformResult(
        text=new_text,
        changed=(new_text != text),
        created_modules=created_modules,
        lifted_procedures=sorted(set(lifted)),
        warnings=warnings,
    )


def transform_file(src: Path, dst: Optional[Path] = None, *, in_place: bool = False) -> TransformResult:
    text = src.read_text(encoding="utf-8")
    res = transform_text(text)
    out_path = src if in_place else (dst if dst is not None else src.with_name(f"{src.stem}_lift.f90"))
    out_path.write_text(res.text, encoding="utf-8")
    return res


def _main() -> int:
    ap = argparse.ArgumentParser(description="Lift internal Fortran procedures to module procedures")
    ap.add_argument("fortran_file", type=Path)
    ap.add_argument("--out", type=Path, default=None, help="output .f90 path")
    ap.add_argument("--in-place", action="store_true", help="rewrite input file in place")
    args = ap.parse_args()

    src = args.fortran_file
    if not src.exists():
        print(f"File not found: {src}")
        return 2
    res = transform_file(src, dst=args.out, in_place=args.in_place)
    if not res.changed:
        print("No internal procedures needed lifting.")
        return 0
    if res.created_modules:
        print("Created modules:", ", ".join(res.created_modules))
    if res.lifted_procedures:
        print("Lifted procedures:", ", ".join(res.lifted_procedures))
    for w in res.warnings:
        print("warning:", w)
    return 0


if __name__ == "__main__":
    raise SystemExit(_main())
