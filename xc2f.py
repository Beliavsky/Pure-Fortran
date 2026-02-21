#!/usr/bin/env python3
"""xc2f.py: small C->Fortran transpiler for a practical C subset.

Current focus: enough coverage to translate xfactors.c into compilable Fortran.
"""

from __future__ import annotations

import argparse
import difflib
import re
import subprocess
import sys
import tempfile
import shutil
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Optional, Set, Tuple
from collections import defaultdict

from pycparser import c_ast, c_parser
import fortran_scan as fscan
import fortran_post as fpost
import fortran_refactor as fref
import xadvance
import xalloc_assign
import xunused
import xpure


PRELUDE = """
typedef unsigned long size_t;
typedef int FILE;
typedef signed char int8_t;
typedef unsigned char uint8_t;
typedef short int16_t;
typedef unsigned short uint16_t;
typedef int int32_t;
typedef unsigned int uint32_t;
typedef long long int64_t;
typedef unsigned long long uint64_t;
typedef long time_t;
struct tm;
void *malloc(size_t);
void *realloc(void *, size_t);
void free(void *);
double sqrt(double);
double floor(double);
time_t time(time_t *);
struct tm *localtime(const time_t *);
size_t strftime(char *, size_t, const char *, const struct tm *);
FILE *fopen(const char *, const char *);
int fclose(FILE *);
int fprintf(FILE *, const char *, ...);
int fscanf(FILE *, const char *, ...);
int printf(const char *, ...);
""".strip()

DEFAULT_GFORTRAN_FLAGS = [
    "-O0",
    "-Wall",
    "-Werror=unused-parameter",
    "-Werror=unused-variable",
    "-Werror=unused-function",
    "-Wno-maybe-uninitialized",
    "-Wno-surprising",
    "-fbounds-check",
    "-static",
    "-fbacktrace",
    "-g",
    "-fmodule-private",
]


@dataclass
class VarInfo:
    ftype: str
    alloc: bool = False


@dataclass
class StructDef:
    name: str
    fields: List[Tuple[str, str]]


_STRUCT_TYPEDEFS: Set[str] = set()


def strip_preprocessor_and_comments(text: str) -> str:
    lines = []
    in_block = False
    i = 0
    while i < len(text):
        if not in_block and text[i : i + 2] == "/*":
            in_block = True
            i += 2
            continue
        if in_block and text[i : i + 2] == "*/":
            in_block = False
            i += 2
            continue
        if not in_block:
            lines.append(text[i])
        i += 1
    s = "".join(lines)
    out = []
    for line in s.splitlines():
        t = line.strip()
        if t.startswith("#"):
            continue
        if "//" in line:
            line = line.split("//", 1)[0]
        out.append(line)
    return "\n".join(out)


def strip_preprocessor_only(text: str) -> str:
    """Remove preprocessor lines but keep comments."""
    out = []
    for line in text.splitlines():
        if line.strip().startswith("#"):
            continue
        out.append(line)
    return "\n".join(out)


def is_language_specific_comment(s: str) -> bool:
    low = s.lower()
    banned = (
        "malloc", "free", "calloc", "realloc", "pointer", "null",
        "size_t", "printf", "scanf",
    )
    return any(tok in low for tok in banned)


def extract_preserved_comments(text: str) -> Dict[int, List[str]]:
    """Extract non-language-specific comments keyed by source line number."""
    out: Dict[int, List[str]] = defaultdict(list)
    lines = text.splitlines()
    in_block = False
    block_start_line = 0
    block_buf: List[str] = []

    def _flush_block(line_no: int) -> None:
        nonlocal block_buf
        for raw in block_buf:
            t = raw.strip()
            if not t:
                continue
            if t.startswith("*"):
                t = t[1:].strip()
            if t and not is_language_specific_comment(t):
                out[line_no].append(t)
        block_buf = []

    for i, raw in enumerate(lines, start=1):
        line = raw
        if in_block:
            if "*/" in line:
                before, _after = line.split("*/", 1)
                block_buf.append(before)
                _flush_block(i)
                in_block = False
            else:
                block_buf.append(line)
            continue

        if "/*" in line:
            before, after = line.split("/*", 1)
            if "*/" in after:
                mid, _tail = after.split("*/", 1)
                t = mid.strip().lstrip("*").strip()
                # Skip trailing prototype comments such as:
                #   float f(...); /* comment */
                if ");" in before:
                    t = ""
                if t and not is_language_specific_comment(t):
                    out[i].append(t)
            else:
                in_block = True
                block_start_line = i
                block_buf = [after]
            # handle // on prefix if present
            if "//" in before:
                c = before.split("//", 1)[1].strip()
                if c and not is_language_specific_comment(c):
                    out[i].append(c)
            continue

        if "//" in line:
            c = line.split("//", 1)[1].strip()
            if c and not is_language_specific_comment(c):
                out[i].append(c)

    if in_block and block_buf:
        _flush_block(block_start_line or 1)
    return out


def extract_c_function_arg_comments(text: str) -> Dict[str, Dict[str, str]]:
    """Extract per-function argument comments of form `name: doc` from C.

    Looks in the contiguous comment block immediately after a function
    definition header line.
    """
    out: Dict[str, Dict[str, str]] = {}
    lines = text.splitlines()
    head_re = re.compile(r"^\s*[a-z_][\w\s\*]*\b([a-z_]\w*)\s*\([^;]*\)\s*\{\s*$", re.IGNORECASE)
    cmt_re = re.compile(r"^\s*/\*\s*(.*?)\s*\*/\s*$")
    arg_re = re.compile(r"^([a-z_]\w*)\s*:\s*(.+)$", re.IGNORECASE)

    i = 0
    while i < len(lines):
        m = head_re.match(lines[i])
        if not m:
            i += 1
            continue
        fname = m.group(1).lower()
        amap: Dict[str, str] = {}
        j = i + 1
        while j < len(lines):
            s = lines[j].strip()
            if not s:
                j += 1
                continue
            mc = cmt_re.match(lines[j])
            if not mc:
                break
            txt = mc.group(1).strip()
            ma = arg_re.match(txt)
            if ma:
                amap[ma.group(1).lower()] = ma.group(2).strip()
            j += 1
        if amap:
            out[fname] = amap
        i = j
    return out


def extract_c_function_header_comments(text: str) -> Dict[str, str]:
    """Extract first non-arg comment from each C function's leading comment block."""
    out: Dict[str, str] = {}
    lines = text.splitlines()
    head_re = re.compile(r"^\s*[a-z_][\w\s\*]*\b([a-z_]\w*)\s*\([^;]*\)\s*\{\s*$", re.IGNORECASE)
    cmt_re = re.compile(r"^\s*/\*\s*(.*?)\s*\*/\s*$")
    arg_re = re.compile(r"^[a-z_]\w*\s*:\s*", re.IGNORECASE)
    i = 0
    while i < len(lines):
        m = head_re.match(lines[i])
        if not m:
            i += 1
            continue
        fname = m.group(1).lower()
        j = i + 1
        while j < len(lines):
            s = lines[j].strip()
            if not s:
                j += 1
                continue
            mc = cmt_re.match(lines[j])
            if not mc:
                break
            txt = mc.group(1).strip()
            if txt and not arg_re.match(txt):
                out[fname] = txt
                break
            j += 1
        i = j
    return out


def c_to_ftype(type_decl: c_ast.Node) -> Tuple[str, bool]:
    node = type_decl
    alloc = False
    ptr_depth = 0
    while isinstance(node, (c_ast.TypeDecl, c_ast.PtrDecl, c_ast.ArrayDecl)):
        if isinstance(node, c_ast.PtrDecl):
            alloc = True
            ptr_depth += 1
            node = node.type
        elif isinstance(node, c_ast.ArrayDecl):
            node = node.type
        else:
            node = node.type
    if isinstance(node, c_ast.IdentifierType):
        names = [n.lower() for n in node.names]
        if "file" in names:
            return "integer", False
        if "void" in names:
            return "void", alloc
        if "char" in names:
            return "character(len=*)", False
        if "double" in names:
            return "real(kind=dp)", alloc
        if "float" in names:
            return "real(kind=sp)", alloc
        if len(names) == 1 and names[0] in _STRUCT_TYPEDEFS:
            return f"type({names[0]})", alloc
        return "integer", alloc
    if isinstance(node, c_ast.Struct):
        sname = (node.name or "").lower()
        if sname:
            return f"type({sname})", alloc
    return "integer", alloc


def type_is_ptr_or_array(type_decl: c_ast.Node) -> bool:
    node = type_decl
    while node is not None:
        if isinstance(node, (c_ast.PtrDecl, c_ast.ArrayDecl)):
            return True
        node = getattr(node, "type", None)
    return False


def type_has_const(type_decl: c_ast.Node) -> bool:
    """Return True if C declaration type carries a const qualifier."""
    node = type_decl
    while node is not None:
        quals = getattr(node, "quals", None)
        if quals and any(str(q).lower() == "const" for q in quals):
            return True
        node = getattr(node, "type", None)
    return False


def gather_decls(node: c_ast.Node, out: Dict[str, VarInfo]) -> None:
    if isinstance(node, c_ast.Decl):
        if isinstance(node.type, c_ast.FuncDecl):
            return
        ftype, alloc = c_to_ftype(node.type)
        if node.name:
            out[node.name] = VarInfo(ftype=ftype, alloc=alloc)
    for _, child in node.children():
        gather_decls(child, out)


def collect_struct_typedefs(ast: c_ast.FileAST) -> Dict[str, StructDef]:
    """Collect `typedef struct { ... } name_t;` definitions."""
    out: Dict[str, StructDef] = {}
    for ext in ast.ext:
        if not isinstance(ext, c_ast.Typedef):
            continue
        t = ext.type
        if not isinstance(t, c_ast.TypeDecl):
            continue
        s = t.type
        if not isinstance(s, c_ast.Struct):
            continue
        tname = ext.name
        if not tname:
            continue
        fields: List[Tuple[str, str]] = []
        for d in s.decls or []:
            if not isinstance(d, c_ast.Decl) or not d.name:
                continue
            ftype, _alloc = c_to_ftype(d.type)
            fields.append((d.name, ftype))
        out[tname.lower()] = StructDef(name=tname.lower(), fields=fields)
    return out


def get_id_name(node: c_ast.Node) -> Optional[str]:
    if isinstance(node, c_ast.ID):
        return node.name
    return None


def has_array_ref_of(node: c_ast.Node, name: str) -> bool:
    """True if subtree contains ArrayRef whose base is identifier `name`."""
    target = name.lower()
    if isinstance(node, c_ast.ArrayRef):
        base = node.name
        if isinstance(base, c_ast.ID) and base.name.lower() == target:
            return True
    for _k, child in node.children():
        if isinstance(child, c_ast.Node) and has_array_ref_of(child, name):
            return True
    return False


def proc_docline(name: str, unit_kind: str) -> str:
    low = name.lower()
    if low.startswith("minmax"):
        return "compute minimum and maximum values"
    if low.startswith("mean"):
        return "compute mean value(s)"
    if low.startswith("sum"):
        return "compute sum value(s)"
    if low.startswith("read"):
        return "read values from input"
    if low.startswith("write"):
        return "write values to output"
    if low.startswith("matmul"):
        return "compute matrix product"
    if low.startswith("factor"):
        return "compute integer factors"
    if unit_kind == "function":
        return f"compute {name}"
    return f"perform {name}"


def arg_docline(name: str, ftype: str, intent: Optional[str] = None, is_array: Optional[bool] = None) -> Optional[str]:
    n = name.lower()
    if n in {"fp", "unit"}:
        return "I/O unit number"
    if n in {"n", "m", "k"}:
        return "problem size"
    if n in {"i", "j", "t"}:
        return "loop index"
    if n in {"x", "y", "z"}:
        if is_array is True:
            return "data array"
        if is_array is False:
            return "data scalar value"
        return "data value"
    if n in {"a", "b", "c"}:
        if is_array is True:
            if intent == "intent(in)":
                return "input array"
            if intent == "intent(out)":
                return "output array"
            return "input/output array"
        if is_array is False:
            if intent == "intent(in)":
                return "input scalar coefficient"
            if intent == "intent(out)":
                return "output scalar"
            if intent == "intent(inout)":
                return "input/output scalar"
            return "scalar value"
    if n in {"v"}:
        return "scalar value"
    if n in {"cap"}:
        return "allocated capacity"
    if n in {"new_cap"}:
        return "new capacity value"
    if n in {"out"}:
        return "output array"
    if "type(" in ftype.lower():
        return "derived-type argument"
    return None


def add_inline_comment(line: str, comment: Optional[str]) -> str:
    if not comment:
        return line
    code, existing = xunused.split_code_comment(line.rstrip("\r\n"))
    if existing.strip():
        return f"{code.rstrip()} {existing.lstrip()}"
    return f"{code} ! {comment}"


class Emitter:
    def __init__(
        self,
        comment_map: Optional[Dict[int, List[str]]] = None,
        line_offset: int = 0,
        array_result_funcs: Optional[Dict[str, int]] = None,
    ) -> None:
        self.lines: List[str] = []
        self.indent = 0
        self.arrays_1d: Set[str] = set()
        self.comment_map: Dict[int, List[str]] = comment_map or {}
        self.line_offset = line_offset
        self.comment_cursor = 1
        self.array_result_funcs: Dict[str, int] = array_result_funcs or {}
        self.array_result_name: Optional[str] = None
        self.array_result_tmp_alias: Optional[str] = None
        self.auto_alloc_assigned: Set[str] = set()
        self.pointer_like_names: Set[str] = set()
        self.var_infos: Dict[str, VarInfo] = {}

    def _is_sizeof_node(self, n: c_ast.Node) -> bool:
        return isinstance(n, c_ast.UnaryOp) and n.op == "sizeof"

    def _mul_terms(self, n: c_ast.Node) -> List[c_ast.Node]:
        if isinstance(n, c_ast.BinaryOp) and n.op == "*":
            return self._mul_terms(n.left) + self._mul_terms(n.right)
        return [n]

    def _malloc_count_expr(self, n: c_ast.Node) -> str:
        """Extract element-count expression from malloc/realloc byte expression."""
        terms = self._mul_terms(n)
        kept = [t for t in terms if not self._is_sizeof_node(t)]
        if not kept:
            return "1"
        parts = [self.expr(t) for t in kept]
        if len(parts) == 1:
            return parts[0]
        return " * ".join(parts)

    def emit(self, line: str = "") -> None:
        self.lines.append(" " * self.indent + line)

    def simp(self, expr: str) -> str:
        # Keep generated arithmetic fully parenthesized to avoid precedence
        # changes (for example: ss/(n-1) must not become ss/n-1).
        return expr.strip()

    def _is_pointer_like_id(self, n: c_ast.Node) -> bool:
        return isinstance(n, c_ast.ID) and n.name.lower() in self.pointer_like_names

    def _is_repeatable_term(self, n: c_ast.Node) -> bool:
        """True when term can be safely duplicated (no side effects)."""
        return isinstance(n, (c_ast.ID, c_ast.ArrayRef, c_ast.StructRef, c_ast.Constant))

    def emit_comments_to(self, src_line: int) -> None:
        """Emit preserved comments up to and including source line."""
        if src_line < self.comment_cursor:
            return
        for ln in range(self.comment_cursor, src_line + 1):
            for c in self.comment_map.get(ln, []):
                self.emit(f"! {c}")
        self.comment_cursor = src_line + 1

    def emit_leading_comments_before(self, src_line: int, *, window: int = 40) -> None:
        """Emit nearby leading comments (before signature) immediately after signature."""
        for c in self.pop_leading_comments_before(src_line, window=window):
            self.emit(f"! {c}")
        if self.comment_cursor <= src_line:
            self.comment_cursor = src_line + 1

    def pop_leading_comments_before(self, src_line: int, *, window: int = 12) -> List[str]:
        """Collect and remove nearby leading comments (before signature).

        Only keeps the nearest contiguous comment cluster before `src_line`
        to avoid pulling comments from prior procedures.
        """
        lo = max(1, src_line - window)
        lines_with_comments = [ln for ln in range(lo, src_line + 1) if self.comment_map.get(ln)]
        if not lines_with_comments:
            return []
        # Keep only the last contiguous cluster.
        cluster: List[int] = [lines_with_comments[-1]]
        for ln in reversed(lines_with_comments[:-1]):
            if cluster[0] - ln <= 1:
                cluster.insert(0, ln)
            else:
                break
        pending: List[str] = []
        for ln in cluster:
            vals = self.comment_map.get(ln, [])
            if vals:
                pending.extend(vals)
                self.comment_map[ln] = []
        return pending

    def expr(self, n: c_ast.Node) -> str:
        if isinstance(n, c_ast.Constant):
            if n.type == "string":
                return n.value
            txt = n.value
            # Normalize C float suffixes (e.g. 0.0f, 1e-3F) to Fortran-real literals.
            txt = re.sub(r"(?i)^([+-]?(?:\d+\.\d*|\.\d+|\d+)(?:[e][+-]?\d+)?)f$", r"\1", txt)
            return txt
        if isinstance(n, c_ast.ID):
            if n.name == "NULL":
                return "0"
            if n.name == "RAND_MAX":
                return "2147483647"
            if n.name == "NAN":
                return "ieee_value(0.0_dp, ieee_quiet_nan)"
            if n.name in {"INFINITY", "HUGE_VAL"}:
                return "ieee_value(0.0_dp, ieee_positive_inf)"
            if self.array_result_name is not None and self.array_result_tmp_alias is not None:
                if n.name == self.array_result_tmp_alias:
                    return self.array_result_name
            return n.name
        if isinstance(n, c_ast.Cast):
            target, _ = c_to_ftype(n.to_type)
            inner = self.expr(n.expr)
            tl = target.lower()
            if tl.startswith("integer"):
                return self.simp(f"int({inner})")
            if tl.startswith("real"):
                mk = re.search(r"kind\s*=\s*([a-z_]\w*)", tl)
                kname = mk.group(1) if mk else "dp"
                return self.simp(f"real({inner}, kind={kname})")
            return inner
        if isinstance(n, c_ast.UnaryOp):
            op = n.op
            if op == "sizeof":
                # Size information is typically consumed in malloc/realloc byte counts.
                # Keep lowering conservative for now.
                return "1"
            if op == "&":
                return self.expr(n.expr)
            if op == "*":
                return self.expr(n.expr)
            if op == "p++" or op == "p--":
                return self.expr(n.expr)
            if op == "-":
                return self.simp(f"-({self.expr(n.expr)})")
            if op == "+":
                return self.simp(f"+({self.expr(n.expr)})")
            if op == "!":
                if isinstance(n.expr, c_ast.ID):
                    return self.simp(f"({self.expr(n.expr)} == 0)")
                return self.simp(f".not. ({self.expr(n.expr)})")
        if isinstance(n, c_ast.BinaryOp):
            op = n.op
            # C pointer arithmetic used as "tail pointer" argument: a + k
            if op == "+":
                if self._is_pointer_like_id(n.left):
                    base = self.expr(n.left)
                    off = self.expr(n.right)
                    if off.strip() in {"0", "+0", "0L", "0l"}:
                        return base
                    return f"{base}(({off})+1:)"
                if self._is_pointer_like_id(n.right):
                    base = self.expr(n.right)
                    off = self.expr(n.left)
                    if off.strip() in {"0", "+0", "0L", "0l"}:
                        return base
                    return f"{base}(({off})+1:)"
            op_map = {
                "&&": ".and.",
                "||": ".or.",
                "==": "==",
                "!=": "/=",
                "<": "<",
                "<=": "<=",
                ">": ">",
                ">=": ">=",
                "+": "+",
                "-": "-",
                "*": "*",
                "/": "/",
                "%": "mod",
            }
            if op == "*" and self._is_repeatable_term(n.left) and self._is_repeatable_term(n.right):
                ltxt = self.expr(n.left)
                rtxt = self.expr(n.right)
                if re.sub(r"\s+", "", ltxt).lower() == re.sub(r"\s+", "", rtxt).lower():
                    return self.simp(f"({ltxt}**2)")
            if op == "%":
                return self.simp(f"mod({self.expr(n.left)}, {self.expr(n.right)})")
            return self.simp(f"({self.expr(n.left)} {op_map.get(op, op)} {self.expr(n.right)})")
        if isinstance(n, c_ast.InitList):
            vals = n.exprs or []
            return "[" + ", ".join(self.expr(v) for v in vals) + "]"
        if isinstance(n, c_ast.TernaryOp):
            return f"merge({self.expr(n.iftrue)}, {self.expr(n.iffalse)}, {self.expr(n.cond)})"
        if isinstance(n, c_ast.ArrayRef):
            idx = n.subscript
            # C pointer indexing: (a + off)[i] => a(off + i + 1)
            if isinstance(n.name, c_ast.BinaryOp) and n.name.op == "+":
                if self._is_pointer_like_id(n.name.left):
                    base = self.expr(n.name.left)
                    off = self.expr(n.name.right)
                    iexpr = self.expr(idx)
                    return f"{base}(({off}) + ({iexpr})+1)"
                if self._is_pointer_like_id(n.name.right):
                    base = self.expr(n.name.right)
                    off = self.expr(n.name.left)
                    iexpr = self.expr(idx)
                    return f"{base}(({off}) + ({iexpr})+1)"
            name = self.expr(n.name)
            if isinstance(idx, c_ast.UnaryOp) and idx.op == "p++":
                return f"{name}({self.expr(idx.expr)}+1)"
            return f"{name}({self.expr(idx)}+1)"
        if isinstance(n, c_ast.FuncCall):
            fname = self.expr(n.name)
            args = []
            if n.args is not None:
                args = [self.expr(a) for a in n.args.exprs]
            if fname in {"fminf", "fmin"} and len(args) >= 2:
                return self.simp(f"min({args[0]}, {args[1]})")
            if fname in {"fmaxf", "fmax"} and len(args) >= 2:
                return self.simp(f"max({args[0]}, {args[1]})")
            if fname == "sqrt":
                a0 = args[0]
                if re.match(r"^\s*real\s*\(", a0, re.IGNORECASE):
                    return self.simp(f"sqrt({a0})")
                return self.simp(f"sqrt(real({a0}, kind=dp))")
            if fname == "floor":
                return self.simp(f"floor({args[0]})")
            if fname == "printf":
                return "__PRINTF__"
            if fname in self.array_result_funcs:
                out_idx = self.array_result_funcs[fname]
                if 0 <= out_idx < len(args):
                    args = [a for i, a in enumerate(args) if i != out_idx]
            return f"{fname}({', '.join(args)})"
        if isinstance(n, c_ast.StructRef):
            base = self.expr(n.name)
            field = self.expr(n.field)
            return f"{base}%{field}"
        raise NotImplementedError(f"Unsupported expr: {type(n).__name__}")

    def emit_printf(self, fc: c_ast.FuncCall) -> bool:
        args = fc.args.exprs if fc.args is not None else []
        if not args:
            self.emit("! approximated printf format by xc2f.py")
            self.emit("write(*,*)")
            return True
        fmt_node = args[0]
        if not isinstance(fmt_node, c_ast.Constant) or fmt_node.type != "string":
            self.emit("! approximated printf format by xc2f.py")
            self.emit(f"write(*,*) {', '.join(self.expr(a) for a in args)}")
            return True
        fmt = fmt_node.value.strip('"')
        vals = args[1:]
        if fmt == "%d:" and len(vals) == 1:
            self.emit(f'write(*,"(i0,a)", advance="no") {self.expr(vals[0])}, ":"')
            return True
        if fmt == " %d" and len(vals) == 1:
            self.emit(f'write(*,"(a,i0)", advance="no") " ", {self.expr(vals[0])}')
            return True
        if fmt == "\\n":
            self.emit("write(*,*)")
            return True
        if fmt in ("%g\\n", "%f\\n", "%lf\\n") and len(vals) == 1:
            self.emit(f"write(*,*) {self.expr(vals[0])}")
            return True
        if fmt == "min=%g max=%g\\n" and len(vals) == 2:
            self.emit(f'write(*,\'("min=",g0," max=",g0)\') {self.expr(vals[0])}, {self.expr(vals[1])}')
            return True
        # Fallback for unsupported formats: preserve data output list-directed.
        self.emit(f'! approximated printf format by xc2f.py: "{fmt}"')
        if vals:
            self.emit(f"write(*,*) {', '.join(self.expr(v) for v in vals)}")
        else:
            self.emit("write(*,*)")
        return False

    def emit_decl(self, name: str, info: VarInfo, params: Set[str], ret_name: Optional[str]) -> None:
        if name in params:
            return
        if ret_name is not None and name == ret_name:
            return
        if info.alloc:
            self.emit(f"{info.ftype}, allocatable :: {name}(:)")
            self.arrays_1d.add(name)
        else:
            self.emit(f"{info.ftype} :: {name}")

    def emit_decl_grouped(self, decls: Dict[str, VarInfo], params: Set[str], ret_name: Optional[str]) -> None:
        """Emit declarations with non-allocatable entities first, then allocatables."""
        items = [(n, info) for n, info in decls.items() if n not in params and not (ret_name is not None and n == ret_name)]
        type_order = {"integer": 0, "logical": 1, "real(kind=sp)": 2, "real(kind=dp)": 3, "real(kind=real64)": 3, "character": 4}
        items.sort(key=lambda x: (1 if x[1].alloc else 0, type_order.get(x[1].ftype.lower(), 50), x[0].lower()))
        for n, info in items:
            self.emit_decl(n, info, params=params, ret_name=ret_name)

    def emit_for(self, st: c_ast.For) -> None:
        # Infinite loop: for(;;) { ... }
        if st.init is None and st.cond is None and st.next is None:
            self.emit("do")
            self.indent += 3
            self.emit_stmt(st.stmt)
            self.indent -= 3
            self.emit("end do")
            return

        if not isinstance(st.cond, c_ast.BinaryOp):
            raise NotImplementedError("Only simple for cond supported")
        if isinstance(st.init, c_ast.Assignment):
            var = self.expr(st.init.lvalue)
            lb = self.expr(st.init.rvalue)
        elif isinstance(st.init, c_ast.DeclList) and len(st.init.decls) == 1:
            d = st.init.decls[0]
            var = d.name
            lb = self.expr(d.init)
        else:
            raise NotImplementedError("Unsupported for init")

        var_l = var.strip().lower()

        # Normalize C loop condition so either `i < ub` or reversed `lb <= i` work.
        cond_op = st.cond.op
        if isinstance(st.cond.left, c_ast.ID) and self.expr(st.cond.left).strip().lower() == var_l:
            bound_expr = self.expr(st.cond.right)
        elif isinstance(st.cond.right, c_ast.ID) and self.expr(st.cond.right).strip().lower() == var_l:
            bound_expr = self.expr(st.cond.left)
            invert = {"<": ">", "<=": ">=", ">": "<", ">=": "<="}
            if cond_op in invert:
                cond_op = invert[cond_op]
            else:
                raise NotImplementedError("Unsupported for condition operator")
        else:
            raise NotImplementedError("Only simple for cond supported")

        ub = bound_expr
        step = None
        if cond_op in ("<", ">"):
            if cond_op == "<":
                ub = f"({ub})-1"
            else:
                ub = f"({ub})+1"

        def _const_int_text(n: c_ast.Node) -> Optional[str]:
            if isinstance(n, c_ast.Constant):
                txt = n.value.strip()
                if re.match(r"^[+-]?\d+$", txt):
                    return txt
            return None

        def _same_var(n: c_ast.Node) -> bool:
            return isinstance(n, c_ast.ID) and self.expr(n).strip().lower() == var_l

        if isinstance(st.next, c_ast.UnaryOp) and st.next.op in ("p++", "++"):
            step = None
        elif isinstance(st.next, c_ast.UnaryOp) and st.next.op in ("p--", "--"):
            step = "-1"
        elif isinstance(st.next, c_ast.Assignment) and isinstance(st.next.lvalue, c_ast.ID) and self.expr(st.next.lvalue).strip().lower() == var_l:
            # += / -= forms
            if st.next.op in ("+=", "-="):
                c = _const_int_text(st.next.rvalue)
                if c is not None:
                    if st.next.op == "+=":
                        step = None if c == "1" else c
                    else:
                        step = "-1" if c == "1" else f"-({c})"
                else:
                    # non-constant step expression
                    rhs = self.expr(st.next.rvalue)
                    step = rhs if st.next.op == "+=" else f"-({rhs})"
            elif st.next.op == "=" and isinstance(st.next.rvalue, c_ast.BinaryOp) and st.next.rvalue.op in ("+", "-"):
                # i = i +/- c, i = c + i
                bop = st.next.rvalue.op
                l, r = st.next.rvalue.left, st.next.rvalue.right
                if _same_var(l):
                    c = _const_int_text(r)
                    if c is not None:
                        if bop == "+":
                            step = None if c == "1" else c
                        else:
                            step = "-1" if c == "1" else f"-({c})"
                    else:
                        rr = self.expr(r)
                        step = rr if bop == "+" else f"-({rr})"
                elif bop == "+" and _same_var(r):
                    c = _const_int_text(l)
                    if c is not None:
                        step = None if c == "1" else c
                    else:
                        step = self.expr(l)
                else:
                    raise NotImplementedError("Unsupported for step")
            else:
                raise NotImplementedError("Unsupported for step")
        else:
            raise NotImplementedError("Unsupported for step")

        if step is None:
            self.emit(f"do {var} = {lb}, {ub}")
        else:
            self.emit(f"do {var} = {lb}, {ub}, {step}")
        self.indent += 3
        self.emit_stmt(st.stmt)
        self.indent -= 3
        self.emit("end do")

    def emit_assignment(self, st: c_ast.Assignment, *, array_result_name: Optional[str] = None) -> None:
        if st.op != "=":
            lhs = self.expr(st.lvalue)
            rhs = self.simp(self.expr(st.rvalue))
            op_map = {"+=": "+", "-=": "-", "*=": "*", "/=": "/"}
            bop = op_map.get(st.op)
            if bop is None:
                raise NotImplementedError(f"Unsupported assignment op {st.op}")
            self.emit(f"{lhs} = {lhs} {bop} {rhs}")
            return

        # Special: a[k++] = expr;
        if isinstance(st.lvalue, c_ast.ArrayRef) and isinstance(st.lvalue.subscript, c_ast.UnaryOp) and st.lvalue.subscript.op == "p++":
            arr = self.expr(st.lvalue.name)
            k = self.expr(st.lvalue.subscript.expr)
            self.emit(f"{arr}({k}+1) = {self.simp(self.expr(st.rvalue))}")
            self.emit(f"{k} = {k} + 1")
            return

        # Special malloc -> allocate
        if isinstance(st.rvalue, c_ast.Cast) and isinstance(st.rvalue.expr, c_ast.FuncCall):
            fc = st.rvalue.expr
            if isinstance(fc.name, c_ast.ID) and fc.name.name == "realloc" and fc.args is not None and len(fc.args.exprs) >= 2:
                lhs = self.expr(st.lvalue)
                size_arg = fc.args.exprs[1]
                n = self._malloc_count_expr(size_arg)
                vinfo = self.var_infos.get(lhs.lower())
                tmp_name = f"{lhs}_tmp"
                elem_type = vinfo.ftype if vinfo is not None else "real(kind=dp)"
                self.emit("block")
                self.indent += 3
                self.emit(f"{elem_type}, allocatable :: {tmp_name}(:)")
                self.emit(f"allocate({tmp_name}({n}))")
                self.emit(f"if (allocated({lhs})) {tmp_name}(1:min(size({lhs}), {n})) = {lhs}(1:min(size({lhs}), {n}))")
                self.emit(f"call move_alloc({tmp_name}, {lhs})")
                self.indent -= 3
                self.emit("end block")
                return
            if isinstance(fc.name, c_ast.ID) and fc.name.name == "malloc" and fc.args is not None and fc.args.exprs:
                arg = fc.args.exprs[0]
                n = self._malloc_count_expr(arg)
                lhs = self.expr(st.lvalue)
                self.emit(f"allocate({lhs}({n}))")
                return
        # fopen -> open(newunit=...)
        if isinstance(st.rvalue, c_ast.FuncCall) and isinstance(st.rvalue.name, c_ast.ID) and st.rvalue.name.name == "fopen":
            args = st.rvalue.args.exprs if st.rvalue.args is not None else []
            if len(args) >= 2:
                lhs = self.expr(st.lvalue)
                fexpr = self.expr(args[0])
                mode = self.expr(args[1]).strip().strip('"').strip("'").lower()
                if "w" in mode:
                    self.emit(f'open(newunit={lhs}, file={fexpr}, status="replace", action="write")')
                elif "r" in mode:
                    self.emit(f'open(newunit={lhs}, file={fexpr}, status="old", action="read")')
                else:
                    self.emit(f'open(newunit={lhs}, file={fexpr})')
                return

        # Special array-result function calls:
        # nf = factors(n, f)  ->  f = factors(n); nf = size(f)
        if isinstance(st.rvalue, c_ast.FuncCall) and isinstance(st.rvalue.name, c_ast.ID):
            fname = st.rvalue.name.name
            if fname in self.array_result_funcs and st.rvalue.args is not None:
                args = [self.expr(a) for a in st.rvalue.args.exprs]
                out_idx = self.array_result_funcs[fname]
                if 0 <= out_idx < len(args):
                    out_var = args[out_idx]
                    call_args = [a for i, a in enumerate(args) if i != out_idx]
                    lhs = self.expr(st.lvalue)
                    self.emit(f"{out_var} = {fname}({', '.join(call_args)})")
                    self.auto_alloc_assigned.add(out_var.lower())
                    self.emit(f"{lhs} = size({out_var})")
                    return

        # rand() scaling idiom -> Fortran random_number(lhs)
        if (
            isinstance(st.rvalue, c_ast.BinaryOp)
            and st.rvalue.op == "/"
            and isinstance(st.rvalue.left, c_ast.Cast)
            and isinstance(st.rvalue.left.expr, c_ast.FuncCall)
            and isinstance(st.rvalue.left.expr.name, c_ast.ID)
            and st.rvalue.left.expr.name.name == "rand"
        ):
            self.emit(f"call random_number({self.expr(st.lvalue)})")
            return

        self.emit(f"{self.expr(st.lvalue)} = {self.simp(self.expr(st.rvalue))}")

    def _unwrap_single_stmt(self, node: Optional[c_ast.Node]) -> Optional[c_ast.Node]:
        if node is None:
            return None
        if isinstance(node, c_ast.Compound):
            items = [b for b in (node.block_items or []) if b is not None]
            if len(items) != 1:
                return None
            return items[0]
        return node

    def _is_simple_value_expr(self, n: c_ast.Node) -> bool:
        if isinstance(n, (c_ast.ID, c_ast.Constant)):
            return True
        if isinstance(n, c_ast.UnaryOp) and n.op in ("+", "-"):
            return isinstance(n.expr, (c_ast.ID, c_ast.Constant))
        return False

    def _extract_simple_update(self, st: c_ast.Node) -> Optional[Tuple[str, str, str]]:
        if not isinstance(st, c_ast.Assignment):
            return None
        lhs = self.expr(st.lvalue)
        if st.op in ("+=", "-=", "*=", "/="):
            if not self._is_simple_value_expr(st.rvalue):
                return None
            op = st.op[0]
            return lhs, op, self.simp(self.expr(st.rvalue))
        if st.op != "=":
            return None
        if not isinstance(st.rvalue, c_ast.BinaryOp):
            return None
        bop = st.rvalue.op
        if bop not in ("+", "-", "*", "/"):
            return None
        left = self.expr(st.rvalue.left)
        right = self.expr(st.rvalue.right)
        if bop in ("+", "*"):
            if left == lhs and self._is_simple_value_expr(st.rvalue.right):
                return lhs, bop, self.simp(right)
            if right == lhs and self._is_simple_value_expr(st.rvalue.left):
                return lhs, bop, self.simp(left)
            return None
        if left == lhs and self._is_simple_value_expr(st.rvalue.right):
            return lhs, bop, self.simp(right)
        return None

    def _emit_if_as_merge_update(self, st: c_ast.If) -> bool:
        if st.iffalse is None:
            return False
        t_stmt = self._unwrap_single_stmt(st.iftrue)
        f_stmt = self._unwrap_single_stmt(st.iffalse)
        if t_stmt is None or f_stmt is None:
            return False
        t_upd = self._extract_simple_update(t_stmt)
        f_upd = self._extract_simple_update(f_stmt)
        if t_upd is None or f_upd is None:
            return False
        lhs_t, op_t, val_t = t_upd
        lhs_f, op_f, val_f = f_upd
        if lhs_t != lhs_f or op_t != op_f:
            return False
        cond = self.simp(self.expr(st.cond))
        self.emit(f"{lhs_t} = {lhs_t} {op_t} merge({val_t}, {val_f}, {cond})")
        return True

    def _is_pointer_nullish_cond(self, node: c_ast.Node) -> bool:
        if isinstance(node, c_ast.UnaryOp) and node.op == "!":
            return isinstance(node.expr, c_ast.ID) and node.expr.name.lower() in self.pointer_like_names
        if isinstance(node, c_ast.BinaryOp) and node.op in ("||", "&&"):
            return self._is_pointer_nullish_cond(node.left) and self._is_pointer_nullish_cond(node.right)
        if isinstance(node, c_ast.BinaryOp) and node.op in ("==", "!="):
            lid = node.left if isinstance(node.left, c_ast.ID) else None
            rid = node.right if isinstance(node.right, c_ast.ID) else None
            lzero = isinstance(node.left, c_ast.Constant) and node.left.value in {"0", "0L"}
            rzero = isinstance(node.right, c_ast.Constant) and node.right.value in {"0", "0L"}
            if lid and rzero and lid.name.lower() in self.pointer_like_names:
                return True
            if rid and lzero and rid.name.lower() in self.pointer_like_names:
                return True
        return False

    def emit_if(self, st: c_ast.If, ret_name: Optional[str], array_result_name: Optional[str] = None) -> None:
        # suppress C pointer/null checks that are not needed after signature lowering
        if isinstance(st.cond, c_ast.BinaryOp) and st.cond.op in ("==", "!="):
            l_is_null = isinstance(st.cond.left, c_ast.ID) and st.cond.left.name == "NULL"
            r_is_null = isinstance(st.cond.right, c_ast.ID) and st.cond.right.name == "NULL"
            if l_is_null or r_is_null:
                return
        if self._is_pointer_nullish_cond(st.cond):
            return
        if self._emit_if_as_merge_update(st):
            return
        self.emit(f"if ({self.simp(self.expr(st.cond))}) then")
        self.indent += 3
        self.emit_stmt(st.iftrue, ret_name=ret_name, array_result_name=array_result_name)
        self.indent -= 3
        if st.iffalse is not None:
            self.emit("else")
            self.indent += 3
            self.emit_stmt(st.iffalse, ret_name=ret_name, array_result_name=array_result_name)
            self.indent -= 3
        self.emit("end if")

    def emit_stmt(
        self,
        st: c_ast.Node,
        ret_name: Optional[str] = None,
        array_result_name: Optional[str] = None,
    ) -> None:
        if st is None:
            return
        coord = getattr(st, "coord", None)
        if coord is not None and getattr(coord, "line", None):
            src_line = int(coord.line) - self.line_offset
            if src_line >= 1:
                # Avoid importing unrelated inter-procedural comments from C
                # statement coordinates; keep documentation at procedure level.
                self.comment_cursor = max(self.comment_cursor, src_line + 1)
        if isinstance(st, c_ast.Compound):
            for b in st.block_items or []:
                self.emit_stmt(b, ret_name=ret_name, array_result_name=array_result_name)
            return
        if isinstance(st, c_ast.Decl):
            # declarations already hoisted
            if st.init is not None:
                if isinstance(st.init, c_ast.Constant) and st.init.value == "NULL":
                    return
                if isinstance(st.init, c_ast.ID) and st.init.name == "NULL":
                    return
                # Declaration-initialized malloc -> allocate
                if isinstance(st.init, c_ast.Cast) and isinstance(st.init.expr, c_ast.FuncCall):
                    fc = st.init.expr
                    if isinstance(fc.name, c_ast.ID) and fc.name.name == "malloc" and fc.args is not None and fc.args.exprs:
                        arg = fc.args.exprs[0]
                        n = self._malloc_count_expr(arg)
                        self.emit(f"allocate({st.name}({n}))")
                        return
                if isinstance(st.init, c_ast.FuncCall) and isinstance(st.init.name, c_ast.ID):
                    fname = st.init.name.name
                    if fname in self.array_result_funcs and st.init.args is not None:
                        args = [self.expr(a) for a in st.init.args.exprs]
                        out_idx = self.array_result_funcs[fname]
                        if 0 <= out_idx < len(args):
                            out_var = args[out_idx]
                            call_args = [a for i, a in enumerate(args) if i != out_idx]
                            self.emit(f"{out_var} = {fname}({', '.join(call_args)})")
                            self.auto_alloc_assigned.add(out_var.lower())
                            self.emit(f"{st.name} = size({out_var})")
                            return
                self.emit(f"{st.name} = {self.expr(st.init)}")
            return
        if isinstance(st, c_ast.Assignment):
            # suppress *out = NULL
            if isinstance(st.lvalue, c_ast.UnaryOp) and st.lvalue.op == "*" and isinstance(st.rvalue, c_ast.ID) and st.rvalue.name == "NULL":
                return
            if isinstance(st.rvalue, c_ast.Constant) and st.rvalue.value == "NULL":
                return
            if isinstance(st.rvalue, c_ast.ID) and st.rvalue.name == "NULL":
                return
            if self.array_result_name is not None:
                if isinstance(st.lvalue, c_ast.ID) and st.lvalue.name == self.array_result_name:
                    if isinstance(st.rvalue, c_ast.ID) and self.expr(st.rvalue).strip() == self.array_result_name:
                        return
            if isinstance(st.lvalue, c_ast.UnaryOp) and st.lvalue.op == "*" and isinstance(st.lvalue.expr, c_ast.ID):
                if self.array_result_name is not None and st.lvalue.expr.name == self.array_result_name:
                    if isinstance(st.rvalue, c_ast.ID) and self.expr(st.rvalue).strip() == self.array_result_name:
                        return
            self.emit_assignment(st, array_result_name=array_result_name)
            return
        if isinstance(st, c_ast.Return):
            if st.expr is None:
                self.emit("return")
            elif array_result_name is not None:
                expr_txt = self.simp(self.expr(st.expr)).strip().lower()
                if expr_txt in {"0", "0.0", "0.0d0", "0.0d+0"}:
                    self.emit(f"allocate({array_result_name}(0))")
                self.emit("return")
            elif ret_name is None:
                self.emit("stop")
            else:
                self.emit(f"{ret_name} = {self.expr(st.expr)}")
                self.emit("return")
            return
        if isinstance(st, c_ast.If):
            self.emit_if(st, ret_name=ret_name, array_result_name=array_result_name)
            return
        if isinstance(st, c_ast.For):
            self.emit_for(st)
            return
        if isinstance(st, c_ast.While):
            # while (fscanf(fp, "...", &v) == 1) { ... } -> read loop with iostat
            cond = st.cond
            if (
                isinstance(cond, c_ast.BinaryOp)
                and cond.op == "=="
                and isinstance(cond.right, c_ast.Constant)
                and cond.right.value == "1"
                and isinstance(cond.left, c_ast.FuncCall)
                and isinstance(cond.left.name, c_ast.ID)
                and cond.left.name.name == "fscanf"
                and cond.left.args is not None
                and len(cond.left.args.exprs) >= 3
            ):
                fp_expr = self.expr(cond.left.args.exprs[0])
                read_arg = cond.left.args.exprs[2]
                read_target = self.expr(read_arg)
                self.emit("block")
                self.indent += 3
                self.emit("integer :: ios")
                self.emit("do")
                self.indent += 3
                self.emit(f"read({fp_expr}, *, iostat=ios) {read_target}")
                self.emit("if (ios /= 0) exit")
                self.emit_stmt(st.stmt, ret_name=ret_name, array_result_name=array_result_name)
                self.indent -= 3
                self.emit("end do")
                self.indent -= 3
                self.emit("end block")
                return
            self.emit(f"do while ({self.simp(self.expr(st.cond))})")
            self.indent += 3
            self.emit_stmt(st.stmt, ret_name=ret_name, array_result_name=array_result_name)
            self.indent -= 3
            self.emit("end do")
            return
        if isinstance(st, c_ast.FuncCall):
            if isinstance(st.name, c_ast.ID) and st.name.name == "printf":
                self.emit_printf(st)
                return
            if isinstance(st.name, c_ast.ID) and st.name.name == "puts":
                args = st.args.exprs if st.args is not None else []
                if len(args) == 1:
                    a0 = args[0]
                    if (
                        isinstance(a0, c_ast.Constant)
                        and a0.type == "string"
                        and a0.value.strip() in {'""', 'L""', 'u8""', 'u""', 'U""'}
                    ):
                        self.emit("print *")
                    else:
                        self.emit(f"write(*,'(a)') {self.expr(a0)}")
                else:
                    self.emit("write(*,*)")
                return
            if isinstance(st.name, c_ast.ID) and st.name.name == "srand":
                # deterministic seed handled by generated Fortran runtime defaults
                return
            if isinstance(st.name, c_ast.ID) and st.name.name == "fclose":
                args = st.args.exprs if st.args is not None else []
                if len(args) == 1:
                    self.emit(f"close({self.expr(args[0])})")
                return
            if isinstance(st.name, c_ast.ID) and st.name.name == "fprintf":
                args = st.args.exprs if st.args is not None else []
                if len(args) >= 3:
                    self.emit(f"write({self.expr(args[0])},*) {self.expr(args[2])}")
                    return
            if isinstance(st.name, c_ast.ID) and st.name.name == "free":
                args = st.args.exprs if st.args is not None else []
                if len(args) == 1:
                    v = self.expr(args[0])
                    if v.lower() in self.auto_alloc_assigned:
                        return
                    self.emit(f"if (allocated({v})) deallocate({v})")
                return
            self.emit(f"call {self.expr(st.name)}({', '.join(self.expr(a) for a in (st.args.exprs if st.args else []))})")
            return
        if isinstance(st, c_ast.UnaryOp):
            if st.op == "p++":
                v = self.expr(st.expr)
                self.emit(f"{v} = {v} + 1")
                return
            if st.op == "p--":
                v = self.expr(st.expr)
                self.emit(f"{v} = {v} - 1")
                return
        if isinstance(st, c_ast.BinaryOp) and st.op == ",":
            # Comma expression used as statement: evaluate both sides for side effects.
            self.emit_stmt(st.left, ret_name=ret_name, array_result_name=array_result_name)
            self.emit_stmt(st.right, ret_name=ret_name, array_result_name=array_result_name)
            return
        if isinstance(st, c_ast.EmptyStatement):
            return
        if isinstance(st, c_ast.Break):
            self.emit("exit")
            return
        if isinstance(st, c_ast.Continue):
            self.emit("cycle")
            return
        raise NotImplementedError(f"Unsupported stmt: {type(st).__name__}")


def emit_function(
    fn: c_ast.FuncDef,
    em: Emitter,
    *,
    main_use_names: Optional[List[str]] = None,
    c_arg_comments_by_func: Optional[Dict[str, Dict[str, str]]] = None,
    c_header_comments_by_func: Optional[Dict[str, str]] = None,
) -> None:
    decl = fn.decl
    fdecl = decl.type
    if not isinstance(fdecl, c_ast.FuncDecl):
        raise NotImplementedError("Expected FuncDecl")
    name = decl.name
    c_arg_comments = (c_arg_comments_by_func or {}).get(name.lower(), {})
    c_header_comment = (c_header_comments_by_func or {}).get(name.lower())
    low_name = name.lower()
    if "min_max" in low_name and (not c_header_comment or "sum" in c_header_comment.lower()):
        c_header_comment = "return min and max of vector"
    if "sum_vec" in low_name and (not c_header_comment or "min and max" in c_header_comment.lower()):
        c_header_comment = "return sum of vector"
    fn_src_line: Optional[int] = None
    if decl.coord is not None and getattr(decl.coord, "line", None):
        src_line = int(decl.coord.line) - em.line_offset
        if src_line >= 1:
            fn_src_line = src_line

    params: List[c_ast.Decl] = []
    if fdecl.args is not None:
        params = [p for p in fdecl.args.params if isinstance(p, c_ast.Decl)]
    need_ieee_consts = ast_uses_any_id(fn.body, {"NAN", "INFINITY", "HUGE_VAL"})

    if name == "main":
        em.emit("program main")
        if main_use_names:
            em.emit(f"use xc2f_mod, only: {', '.join(main_use_names)}")
        em.emit("use, intrinsic :: iso_fortran_env, only: real64")
        if need_ieee_consts:
            em.emit("use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_positive_inf")
        em.emit("implicit none")

        locals_map: Dict[str, VarInfo] = {}
        gather_decls(fn.body, locals_map)
        em.var_infos = {k.lower(): v for k, v in locals_map.items()}
        for n, info in locals_map.items():
            if info.alloc:
                em.pointer_like_names.add(n.lower())
        em.indent = 0
        em.emit_decl_grouped(locals_map, params=set(), ret_name=None)
        em.emit_stmt(fn.body, ret_name=None)
        em.emit("end program main")
        em.pointer_like_names.clear()
        em.var_infos = {}
        return

    # non-main -> Fortran function
    ret_ftype, _ = c_to_ftype(fdecl.type)
    out_idx = em.array_result_funcs.get(name, None)
    out_param_name: Optional[str] = None
    out_c_param_name: Optional[str] = None
    pnames: List[str] = []
    for idx, p in enumerate(params):
        if out_idx is not None and idx == out_idx:
            out_c_param_name = p.name
            out_param_name = p.name
            continue
        pnames.append(p.name)

    result_name_for_body: Optional[str] = None
    result_decl_ftype: Optional[str] = None
    if out_idx is not None:
        em.emit(f"function {name}({', '.join(pnames)}) result({out_param_name})")
        unit_kind = "function"
        result_name_for_body = out_param_name
    elif ret_ftype.lower() == "void":
        em.emit(f"subroutine {name}({', '.join(pnames)})")
        unit_kind = "subroutine"
    else:
        result_name_for_body = f"{name}_result"
        em.emit(f"function {name}({', '.join(pnames)}) result({result_name_for_body})")
        unit_kind = "function"
        result_decl_ftype = ret_ftype
    em.emit(f"! {proc_docline(name, unit_kind)}")
    if c_header_comment:
        em.lines[-1] = f"! {c_header_comment}"
    if fn_src_line is not None:
        comments = em.pop_leading_comments_before(fn_src_line)
        filtered: List[str] = []
        seen: Set[str] = set()
        for c in comments:
            c0 = c.strip()
            if not c0:
                continue
            key = c0.lower()
            if key in seen:
                continue
            seen.add(key)
            filtered.append(c0)
        header_doc: Optional[str] = None
        arg_comment_re = re.compile(r"^[a-z_]\w*\s*:\s*", re.IGNORECASE)
        for c0 in filtered:
            if not arg_comment_re.match(c0):
                header_doc = c0
                break
        param_comment_map: Dict[str, str] = {}
        for c0 in filtered:
            mpc = re.match(r"^([a-z_]\w*)\s*:\s*(.+)$", c0, re.IGNORECASE)
            if not mpc:
                continue
            pname = mpc.group(1).lower()
            ptxt = mpc.group(2).strip()
            if ptxt and pname not in param_comment_map:
                param_comment_map[pname] = ptxt
        if header_doc and not c_header_comment:
            # Replace generic auto-doc with source comment when available.
            if em.lines and em.lines[-1].lstrip().startswith("!"):
                em.lines[-1] = f"! {header_doc}"
        # Keep generated procedure docline only; imported C comments tend to be
        # noisy/redundant after transpilation round-trips.
        comments = []
        if out_idx is not None:
            rewritten: List[str] = []
            for c in comments:
                low = c.lower()
                if "return value is" in low and "number of factors" in low:
                    rewritten.append("- return value is the factors array (size(...) gives count; empty on error)")
                else:
                    rewritten.append(c)
            comments = rewritten
        for c in comments:
            em.emit(f"! {c}")
        if em.comment_cursor <= fn_src_line:
            em.comment_cursor = fn_src_line + 1
    else:
        param_comment_map = {}
    em.emit("use, intrinsic :: iso_fortran_env, only: real64")
    if need_ieee_consts:
        em.emit("use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_positive_inf")

    param_set = set(pnames)
    # params
    for idx, p in enumerate(params):
        if out_idx is not None and idx == out_idx:
            continue
        p_ft, p_alloc = c_to_ftype(p.type)
        p_ptr_or_arr = type_is_ptr_or_array(p.type)
        p_const = type_has_const(p.type)
        if p_const:
            intent = "intent(in)"
        else:
            has_read, has_write = _scan_dummy_usage(fn.body, p.name)
            first_read, first_write = _dummy_first_read_write_line(fn.body, p.name)
            if has_write and (not has_read or (first_write is not None and (first_read is None or first_write <= first_read))):
                intent = "intent(out)"
            elif has_write:
                intent = "intent(inout)"
            else:
                intent = "intent(in)"
        is_array_dummy = has_array_ref_of(fn.body, p.name)
        inline_doc = c_arg_comments.get(p.name.lower()) or param_comment_map.get(p.name.lower()) or arg_docline(
            p.name, p_ft, intent=intent, is_array=is_array_dummy
        )
        if p_ptr_or_arr and is_array_dummy:
            em.emit(add_inline_comment(f"{p_ft}, {intent} :: {p.name}(:)", inline_doc))
            em.pointer_like_names.add(p.name.lower())
        else:
            em.emit(add_inline_comment(f"{p_ft}, {intent} :: {p.name}", inline_doc))
    if out_idx is not None and out_param_name is not None:
        em.emit(add_inline_comment(f"integer, allocatable :: {out_param_name}(:)", arg_docline(out_param_name, "integer")))
        em.array_result_name = out_param_name
    elif unit_kind == "function" and result_name_for_body is not None and result_decl_ftype is not None:
        em.emit(f"{result_decl_ftype} :: {result_name_for_body}")

    # locals
    locals_map: Dict[str, VarInfo] = {}
    gather_decls(fn.body, locals_map)
    em.var_infos = {k.lower(): v for k, v in locals_map.items()}
    for n, info in locals_map.items():
        if info.alloc:
            em.pointer_like_names.add(n.lower())
    if out_idx is not None and out_param_name is not None:
        # If function body includes `*out = tmp` and tmp is allocatable local,
        # alias tmp -> out and avoid declaring tmp.
        body_items = fn.body.block_items or []
        for st in body_items:
            if isinstance(st, c_ast.Assignment) and st.op == "=":
                l = st.lvalue
                r = st.rvalue
                if isinstance(l, c_ast.UnaryOp) and l.op == "*" and isinstance(l.expr, c_ast.ID):
                    if out_c_param_name is not None and l.expr.name == out_c_param_name and isinstance(r, c_ast.ID):
                        tmp = r.name
                        info = locals_map.get(tmp)
                        if info is not None and info.alloc:
                            em.array_result_tmp_alias = tmp
                            del locals_map[tmp]
                            break
    em.emit_decl_grouped(locals_map, params=param_set, ret_name=result_name_for_body if unit_kind == "function" else None)

    # body
    em.emit_stmt(
        fn.body,
        ret_name=result_name_for_body if unit_kind == "function" else None,
        array_result_name=out_param_name if out_idx is not None else None,
    )
    if unit_kind == "subroutine":
        em.emit(f"end subroutine {name}")
    else:
        em.emit(f"end function {name}")
    em.array_result_name = None
    em.array_result_tmp_alias = None
    em.pointer_like_names.clear()
    em.var_infos = {}


def collect_called_names(node: c_ast.Node, names: Set[str]) -> Set[str]:
    """Collect function-call names that match `names` from an AST subtree."""
    found: Set[str] = set()
    if isinstance(node, c_ast.FuncCall) and isinstance(node.name, c_ast.ID):
        n = node.name.name
        if n in names:
            found.add(n)
    for _k, child in node.children():
        if isinstance(child, c_ast.Node):
            found.update(collect_called_names(child, names))
    return found


def ast_uses_any_id(node: c_ast.Node, names: Set[str]) -> bool:
    """True if subtree references any identifier in `names`."""
    if isinstance(node, c_ast.ID) and node.name in names:
        return True
    for _k, child in node.children():
        if isinstance(child, c_ast.Node) and ast_uses_any_id(child, names):
            return True
    return False


def _is_dummy_target_expr(expr: c_ast.Node, pname: str) -> bool:
    """True when AST expr refers to dummy argument pname or *pname."""
    if isinstance(expr, c_ast.ID):
        return expr.name == pname
    if isinstance(expr, c_ast.ArrayRef):
        return _is_dummy_target_expr(expr.name, pname)
    if isinstance(expr, c_ast.StructRef):
        return _is_dummy_target_expr(expr.name, pname)
    if isinstance(expr, c_ast.UnaryOp) and expr.op == "*" and isinstance(expr.expr, c_ast.ID):
        return expr.expr.name == pname
    return False


def _scan_dummy_usage(node: c_ast.Node, pname: str, *, write_ctx: bool = False) -> Tuple[bool, bool]:
    """Return (has_read, has_write) usage for dummy pname in subtree."""
    has_read = False
    has_write = False

    if isinstance(node, c_ast.Assignment):
        if _is_dummy_target_expr(node.lvalue, pname):
            has_write = True
        r_read, r_write = _scan_dummy_usage(node.rvalue, pname, write_ctx=False)
        has_read = has_read or r_read
        has_write = has_write or r_write
        # lvalue subscripts/struct refs may contain reads
        l_read, l_write = _scan_dummy_usage(node.lvalue, pname, write_ctx=True)
        has_read = has_read or l_read
        has_write = has_write or l_write
        return has_read, has_write

    if isinstance(node, c_ast.UnaryOp) and node.op in ("p++", "p--", "++", "--"):
        if _is_dummy_target_expr(node.expr, pname):
            return True, True

    if _is_dummy_target_expr(node, pname):
        if write_ctx:
            return False, False
        return True, False

    for _k, child in node.children():
        if isinstance(child, c_ast.Node):
            c_read, c_write = _scan_dummy_usage(child, pname, write_ctx=write_ctx)
            has_read = has_read or c_read
            has_write = has_write or c_write
    return has_read, has_write


def _dummy_first_read_write_line(
    node: c_ast.Node,
    pname: str,
    *,
    write_ctx: bool = False,
) -> Tuple[Optional[int], Optional[int]]:
    """Return earliest (read_line, write_line) for dummy pname in subtree."""
    first_read: Optional[int] = None
    first_write: Optional[int] = None

    def _line_of(n: c_ast.Node) -> Optional[int]:
        c = getattr(n, "coord", None)
        if c is None:
            return None
        return getattr(c, "line", None)

    if isinstance(node, c_ast.Assignment):
        r_read, r_write = _dummy_first_read_write_line(node.rvalue, pname, write_ctx=False)
        l_read, l_write = _dummy_first_read_write_line(node.lvalue, pname, write_ctx=True)
        for v in (r_read, l_read):
            if v is not None and (first_read is None or v < first_read):
                first_read = v
        for v in (r_write, l_write):
            if v is not None and (first_write is None or v < first_write):
                first_write = v
        if _is_dummy_target_expr(node.lvalue, pname):
            ln = _line_of(node)
            if ln is not None and (first_write is None or ln < first_write):
                first_write = ln
        return first_read, first_write

    if isinstance(node, c_ast.UnaryOp) and node.op in ("p++", "p--", "++", "--"):
        if _is_dummy_target_expr(node.expr, pname):
            ln = _line_of(node)
            return ln, ln

    if _is_dummy_target_expr(node, pname):
        if write_ctx:
            return None, None
        ln = _line_of(node)
        return ln, None

    for _k, child in node.children():
        if isinstance(child, c_ast.Node):
            c_read, c_write = _dummy_first_read_write_line(child, pname, write_ctx=write_ctx)
            if c_read is not None and (first_read is None or c_read < first_read):
                first_read = c_read
            if c_write is not None and (first_write is None or c_write < first_write):
                first_write = c_write
    return first_read, first_write


def transpile_c_to_fortran(text: str, *, refactor: bool = False, raw: bool = False) -> str:
    no_pp = strip_preprocessor_only(text)
    comments = extract_preserved_comments(no_pp)
    c_arg_comments_by_func = extract_c_function_arg_comments(no_pp)
    c_header_comments_by_func = extract_c_function_header_comments(no_pp)
    src = strip_preprocessor_and_comments(text)
    parser = c_parser.CParser()
    ast = parser.parse(PRELUDE + "\n" + src)
    real_prec = _detect_c_real_precision(src)
    dp_init = "kind(1.0)" if real_prec == "single" else "kind(1.0d0)"
    struct_defs = collect_struct_typedefs(ast)
    global _STRUCT_TYPEDEFS
    _STRUCT_TYPEDEFS = set(struct_defs.keys())

    line_offset = PRELUDE.count("\n") + 1
    array_result_funcs: Dict[str, int] = {}
    for ext in ast.ext:
        if not isinstance(ext, c_ast.FuncDef):
            continue
        decl = ext.decl
        fdecl = decl.type
        if not isinstance(fdecl, c_ast.FuncDecl):
            continue
        ret_ftype, _ = c_to_ftype(fdecl.type)
        if ret_ftype.lower() != "integer":
            continue
        if fdecl.args is None:
            continue
        params = [p for p in fdecl.args.params if isinstance(p, c_ast.Decl)]
        for idx, p in enumerate(params):
            _pft, p_alloc = c_to_ftype(p.type)
            if p_alloc and p.name and p.name.lower() == "out":
                array_result_funcs[decl.name] = idx
                break

    em = Emitter(comment_map=comments, line_offset=line_offset, array_result_funcs=array_result_funcs)
    funcs = [e for e in ast.ext if isinstance(e, c_ast.FuncDef) and e.decl.name != "main"]
    mains = [e for e in ast.ext if isinstance(e, c_ast.FuncDef) and e.decl.name == "main"]
    module_proc_names: Set[str] = {e.decl.name for e in funcs}
    main_called_module_names: Set[str] = set()
    main_needed_types: Set[str] = set()
    for m in mains:
        main_called_module_names.update(collect_called_names(m, module_proc_names))
        lmap: Dict[str, VarInfo] = {}
        gather_decls(m.body, lmap)
        for _n, info in lmap.items():
            mty = re.match(r"^\s*type\s*\(\s*([a-z][a-z0-9_]*)\s*\)\s*$", info.ftype, re.IGNORECASE)
            if mty:
                tname = mty.group(1).lower()
                if tname in struct_defs:
                    main_needed_types.add(tname)

    if funcs:
        em.emit("module xc2f_mod")
        em.emit("implicit none")
        em.emit("private")
        if mains:
            publics = sorted(set(main_called_module_names) | set(main_needed_types))
        else:
            publics = sorted(module_proc_names)
        publics.extend(sorted(struct_defs.keys()))
        publics = sorted(set(publics))
        if publics:
            em.emit(f"public :: {', '.join(publics)}")
        if struct_defs:
            em.emit("")
            for sname in sorted(struct_defs.keys()):
                sdef = struct_defs[sname]
                em.emit(f"type :: {sdef.name}")
                for fname, ftype in sdef.fields:
                    em.emit(f"   {ftype} :: {fname}")
                em.emit(f"end type {sdef.name}")
                em.emit("")
        em.emit("contains")
        em.emit("")
        for ext in funcs:
            emit_function(
                ext,
                em,
                c_arg_comments_by_func=c_arg_comments_by_func,
                c_header_comments_by_func=c_header_comments_by_func,
            )
            em.emit("")
        em.emit("end module xc2f_mod")
        em.emit("")

    for ext in mains:
        emit_function(
            ext,
            em,
            main_use_names=sorted(set(main_called_module_names) | set(main_needed_types)),
            c_arg_comments_by_func=c_arg_comments_by_func,
            c_header_comments_by_func=c_header_comments_by_func,
        )
        em.emit("")
    lines = [ln + "\n" for ln in em.lines]
    if raw:
        # Raw mode: return direct lowering output with minimal wrapping only.
        out_text = "".join(lines).rstrip() + "\n"
        return out_text

    lines = fscan.demote_fixed_size_single_allocatables(lines)
    lines = _coalesce_simple_declarations_preserve_intent(lines)
    lines = fscan.coalesce_adjacent_allocate_statements(lines, max_len=80)
    lines = move_decl_comments_to_after_signature(lines)
    lines = move_inits_below_early_guard(lines)
    lines = fscan.promote_scalar_constants_to_parameters(lines)
    lines = remove_redundant_final_return(lines)
    lines = remove_redundant_final_stop(lines)
    lines = fscan.remove_redundant_tail_deallocations(lines)
    lines = collapse_noadvance_integer_print_loops(lines)
    lines = apply_dead_store_cleanup(lines)
    lines = fscan.inline_temp_assign_into_immediate_use(lines, require_write_stmt=True)
    lines = inline_single_use_temp_assignments(lines)
    lines = fpost.inline_temp_into_function_result(lines)
    lines = fpost.remove_redundant_self_assignments(lines)
    lines = fpost.normalize_shifted_index_loops(lines)
    lines = fpost.remove_redundant_size_dummy_args(lines)
    lines = fpost.hoist_repeated_open_file_literals(lines)
    lines = apply_dead_store_cleanup(lines)
    lines = fscan.prune_unused_use_only_lines(lines)
    lines = fscan.avoid_reserved_identifier_definitions(lines)
    lines = fscan.simplify_redundant_parens_in_lines(lines)
    lines = fscan.simplify_integer_arithmetic_in_lines(lines)
    lines = fscan.collapse_single_stmt_if_blocks(lines)
    lines = fscan.collapse_random_number_element_loops(lines)
    lines = fscan.coalesce_contiguous_scalar_assignments_to_constructor(lines)
    lines = _remove_arg_style_doc_comments(lines)
    lines = add_pure_when_possible(lines)
    lines = fpost.promote_pure_scalar_subroutines_to_elemental(lines)
    out_text = "".join(lines).rstrip() + "\n"
    if array_result_funcs:
        out_text = out_text.replace(
            "! - return value is the number of factors (0 on error)\n",
            "! - return value is the factors array (size(...) gives count; empty on error)\n",
        )
    out_text, _n_alloc_assign = xalloc_assign.rewrite_text_allocation_on_assignment(out_text)
    out_text, _n_adv = xadvance.rewrite_text_collapse_nonadv_write_loops(out_text)
    out_lines = out_text.splitlines(keepends=True)
    out_lines = fscan.remove_redundant_int_casts(out_lines)
    out_lines = fscan.remove_redundant_real_casts(out_lines)
    out_lines = rewrite_realloc_assign_patterns(out_lines)
    out_lines = fscan.compact_consecutive_constructor_literals_to_implied_do(out_lines, min_items=4)
    out_lines = fscan.normalize_identifier_case_to_declarations(out_lines)
    out_lines = fscan.demote_fixed_size_single_allocatables(out_lines)
    out_lines = fscan.suffix_real_literals_with_kind(out_lines, kind_name="dp")
    out_lines = collapse_int_array_write_implied_do(out_lines)
    out_lines = fpost.ensure_function_result_syntax(out_lines)
    out_lines = fpost.remove_unused_local_declarations(out_lines)
    out_lines = fscan.ensure_space_before_inline_comments(out_lines)
    out_text = "".join(out_lines)
    out_text = _normalize_kind_intrinsic_literals(out_text)
    out_text = apply_shared_kind_module_dp(out_text, dp_init=dp_init)
    if refactor:
        out_text = fref.refactor_long_main_blocks_to_module_subroutines(out_text)
        out_text = "".join(apply_dead_store_cleanup(out_text.splitlines(keepends=True)))
        out_text = "".join(fscan.coalesce_simple_declarations(out_text.splitlines(keepends=True)))
        out_text = "".join(fpost.remove_unused_local_declarations(out_text.splitlines(keepends=True)))
    out_text = "".join(fscan.prune_unused_use_only_lines(out_text.splitlines(keepends=True)))
    out_text = "".join(fscan.simplify_redundant_parens_in_lines(out_text.splitlines(keepends=True)))
    out_text = "".join(fscan.simplify_do_bounds_parens(out_text.splitlines(keepends=True)))
    out_text = "".join(fscan.ensure_space_before_inline_comments(out_text.splitlines(keepends=True)))
    out_text = _normalize_kind_intrinsic_literals(out_text)
    return out_text


def apply_xarray_postprocess(text: str, *, inline: bool = False) -> str:
    """Run xarray.py as an optional final post-pass on generated Fortran."""
    xarray_path = Path(__file__).with_name("xarray.py")
    if not xarray_path.exists():
        return text
    with tempfile.TemporaryDirectory(prefix="xc2f_array_") as td:
        tdir = Path(td)
        src = tdir / "in.f90"
        out = tdir / "out.f90"
        src.write_text(text, encoding="utf-8")
        cmd = [
            sys.executable,
            str(xarray_path),
            str(src),
            "--fix",
            "--out",
            str(out),
            "--no-trace",
            "--no-annotate",
        ]
        if inline:
            cmd.append("--inline")
        proc = subprocess.run(cmd, capture_output=True, text=True)
        if proc.returncode != 0:
            return text
        if out.exists():
            return out.read_text(encoding="utf-8", errors="ignore")
        return text


def apply_xno_variable_postprocess(text: str) -> str:
    """Run xno_variable.py optional post-pass on generated Fortran."""
    tool_path = Path(__file__).with_name("xno_variable.py")
    if not tool_path.exists():
        return text
    with tempfile.TemporaryDirectory(prefix="xc2f_inline_") as td:
        tdir = Path(td)
        src = tdir / "in.f90"
        out = tdir / "out.f90"
        src.write_text(text, encoding="utf-8")
        cmd = [
            sys.executable,
            str(tool_path),
            str(src),
            "--fix",
            "--out",
            str(out),
            "--no-backup",
        ]
        proc = subprocess.run(cmd, capture_output=True, text=True)
        if proc.returncode != 0:
            return text
        if out.exists():
            return out.read_text(encoding="utf-8", errors="ignore")
        return text


def _build_and_run(
    source: Path,
    *,
    compiler: str,
    exe_path: Path,
    label: str,
    extra_args: Optional[List[str]] = None,
) -> Tuple[bool, str, str, bool]:
    if shutil.which(compiler) is None:
        print(f"Run ({label}): SKIP ({compiler} not found)")
        return False, "", "", False
    extra = extra_args or []
    cmd = [compiler, str(source), *extra, "-o", str(exe_path)]
    print(f"Build ({label}): {' '.join(cmd)}")
    cp = subprocess.run(cmd, capture_output=True, text=True)
    if cp.returncode != 0:
        print(f"Build ({label}): FAIL (exit {cp.returncode})")
        if cp.stdout:
            print(cp.stdout.rstrip())
        if cp.stderr:
            print(cp.stderr.rstrip())
        return False, cp.stdout or "", cp.stderr or "", False
    print(f"Build ({label}): PASS")
    print(f"Run ({label}): {exe_path}")
    rp = subprocess.run([str(exe_path)], capture_output=True, text=True)
    if rp.returncode != 0:
        print(f"Run ({label}): FAIL (exit {rp.returncode})")
        if rp.stdout:
            print(rp.stdout.rstrip())
        if rp.stderr:
            print(rp.stderr.rstrip())
        return False, rp.stdout or "", rp.stderr or "", True
    print(f"Run ({label}): PASS")
    if rp.stdout:
        print(rp.stdout.rstrip())
    if rp.stderr:
        print(rp.stderr.rstrip())
    return True, rp.stdout or "", rp.stderr or "", True


def _build_and_run_c_many(
    sources: List[Path],
    *,
    exe_path: Path,
    label: str,
) -> Tuple[bool, str, str, bool]:
    if shutil.which("gcc") is None:
        print(f"Run ({label}): SKIP (gcc not found)")
        return False, "", "", False
    cmd = ["gcc", *[str(p) for p in sources], "-lm", "-o", str(exe_path)]
    print(f"Build ({label}): {' '.join(cmd)}")
    cp = subprocess.run(cmd, capture_output=True, text=True)
    if cp.returncode != 0:
        print(f"Build ({label}): FAIL (exit {cp.returncode})")
        if cp.stdout:
            print(cp.stdout.rstrip())
        if cp.stderr:
            print(cp.stderr.rstrip())
        return False, cp.stdout or "", cp.stderr or "", False
    print(f"Build ({label}): PASS")
    print(f"Run ({label}): {exe_path}")
    rp = subprocess.run([str(exe_path)], capture_output=True, text=True)
    if rp.returncode != 0:
        print(f"Run ({label}): FAIL (exit {rp.returncode})")
        if rp.stdout:
            print(rp.stdout.rstrip())
        if rp.stderr:
            print(rp.stderr.rstrip())
        return False, rp.stdout or "", rp.stderr or "", True
    print(f"Run ({label}): PASS")
    if rp.stdout:
        print(rp.stdout.rstrip())
    if rp.stderr:
        print(rp.stderr.rstrip())
    return True, rp.stdout or "", rp.stderr or "", True


def _build_only_cmd(cmd: List[str], *, label: str) -> bool:
    print(f"Build ({label}): {' '.join(cmd)}")
    cp = subprocess.run(cmd, capture_output=True, text=True)
    if cp.returncode != 0:
        print(f"Build ({label}): FAIL (exit {cp.returncode})")
        if cp.stdout:
            print(cp.stdout.rstrip())
        if cp.stderr:
            print(cp.stderr.rstrip())
        return False
    print(f"Build ({label}): PASS")
    return True


def _build_c_many_only(
    sources: List[Path],
    *,
    exe_path: Path,
    label: str,
    compile_only: bool,
) -> bool:
    if shutil.which("gcc") is None:
        print(f"Build ({label}): SKIP (gcc not found)")
        return False
    if compile_only:
        ok = True
        for src in sources:
            obj = src.with_suffix(".orig.o")
            cmd = ["gcc", "-c", str(src), "-o", str(obj)]
            ok = _build_only_cmd(cmd, label=label) and ok
        return ok
    cmd = ["gcc", *[str(p) for p in sources], "-lm", "-o", str(exe_path)]
    return _build_only_cmd(cmd, label=label)


def _time_executable(exe_path: Path, *, label: str, reps: int = 3) -> Optional[float]:
    """Return best wall time over reps in seconds, or None on failure."""
    best: Optional[float] = None
    for _ in range(max(1, reps)):
        t0 = time.perf_counter()
        rp = subprocess.run([str(exe_path)], capture_output=True, text=True)
        dt = time.perf_counter() - t0
        if rp.returncode != 0:
            print(f"Time ({label}): FAIL (exit {rp.returncode})")
            if rp.stdout:
                print(rp.stdout.rstrip())
            if rp.stderr:
                print(rp.stderr.rstrip())
            return None
        if best is None or dt < best:
            best = dt
    if best is not None:
        print(f"Time ({label}): best {best:.6f} s over {max(1, reps)} run(s)")
    return best


def apply_shared_kind_module_dp(text: str, *, dp_init: str = "kind(1.0d0)") -> str:
    """Use a shared kind_mod(sp,dp) when both module and main are present.

    Rewrites generated code to:
    - add module kind_mod with dp parameter
    - replace kind=real64 with kind=dp
    - replace/remove direct iso_fortran_env(real64) USE lines
    """
    lines = text.splitlines(keepends=True)
    if not lines:
        return text

    has_kind_mod = any(re.match(r"^\s*module\s+kind_mod\b", ln, re.IGNORECASE) for ln in lines)
    if has_kind_mod:
        return text

    mod_idx = next((i for i, ln in enumerate(lines) if re.match(r"^\s*module\s+xc2f_mod\b", ln, re.IGNORECASE)), None)
    main_idx = next((i for i, ln in enumerate(lines) if re.match(r"^\s*program\s+main\b", ln, re.IGNORECASE)), None)
    if mod_idx is None or main_idx is None:
        return text
    wants_sp = bool(re.search(r"\bkind\s*=\s*sp\b|_sp\b", text, re.IGNORECASE))
    wants_dp = bool(re.search(r"\bkind\s*=\s*dp\b|_dp\b|\breal64\b", text, re.IGNORECASE))
    if not wants_sp and not wants_dp:
        return text

    # Rewrite kinds.
    lines = [re.sub(r"\bkind\s*=\s*real64\b", "kind=dp", ln) for ln in lines]
    # Rewrite literal suffixes introduced before shared kind module insertion.
    lines = [re.sub(r"(?i)\b([0-9]+(?:\.[0-9]*)?(?:[eEdD][+\-]?[0-9]+)?)_real64\b", r"\1_dp", ln) for ln in lines]

    # Remove direct real64 USE lines.
    use_real64_re = re.compile(r"^\s*use\s*,\s*intrinsic\s*::\s*iso_fortran_env\s*,\s*only\s*:\s*real64\s*$", re.IGNORECASE)
    lines = [ln for ln in lines if not use_real64_re.match(ln.strip())]

    use_syms = []
    if wants_sp:
        use_syms.append("sp")
    if wants_dp:
        use_syms.append("dp")
    use_line = f"use kind_mod, only: {', '.join(use_syms)}\n"

    # Add `use kind_mod, only: ...` in xc2f_mod spec part.
    if mod_idx is not None:
        ins_mod = mod_idx + 1
        if ins_mod <= len(lines):
            lines.insert(ins_mod, use_line)

    # Add `use kind_mod, only: ...` in main program after any use lines.
    main_idx = next((i for i, ln in enumerate(lines) if re.match(r"^\s*program\s+main\b", ln, re.IGNORECASE)), None)
    if main_idx is not None:
        ins_main = main_idx + 1
        while ins_main < len(lines):
            s = lines[ins_main].strip()
            if not s:
                ins_main += 1
                continue
            if re.match(r"^\s*use\b", s, re.IGNORECASE):
                ins_main += 1
                continue
            break
        lines.insert(ins_main, use_line)

    kind_mod_block = [
        "module kind_mod\n",
        "implicit none\n",
        "private\n",
        "public :: sp, dp\n",
        "integer, parameter :: sp = kind(1.0)\n",
        f"integer, parameter :: dp = {dp_init}\n",
        "end module kind_mod\n",
        "\n",
    ]
    lines = kind_mod_block + lines
    return "".join(lines)


def _norm_expr_token(expr: str) -> str:
    s = expr.strip()
    while len(s) >= 2 and s[0] == "(" and s[-1] == ")":
        depth = 0
        ok = True
        for i, ch in enumerate(s):
            if ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
                if depth == 0 and i != len(s) - 1:
                    ok = False
                    break
        if not ok:
            break
        s = s[1:-1].strip()
    return re.sub(r"\s+", "", s).lower()


def _remove_arg_style_doc_comments(lines: List[str]) -> List[str]:
    """Drop C-prototype-style doc comments promoted into Fortran.

    Removes lines like:
      ! x: vector
      ! n: extent of x (dimension 1)
    because inline dummy comments are generated separately.
    """
    out: List[str] = []
    arg_cmt_re = re.compile(r"^\s*!\s*[a-z_]\w*\s*:\s*", re.IGNORECASE)
    for ln in lines:
        if arg_cmt_re.match(ln):
            continue
        out.append(ln)
    return out


def _coalesce_simple_declarations_preserve_intent(lines: List[str]) -> List[str]:
    """Coalesce declarations, but keep dummy-arg `intent(...)` lines untouched."""
    out: List[str] = []
    buf: List[str] = []
    intent_re = re.compile(r"\bintent\s*\(", re.IGNORECASE)
    for ln in lines:
        if intent_re.search(ln):
            if buf:
                out.extend(fscan.coalesce_simple_declarations(buf))
                buf = []
            out.append(ln)
        else:
            buf.append(ln)
    if buf:
        out.extend(fscan.coalesce_simple_declarations(buf))
    return out


def _detect_c_real_precision(src_no_comments: str) -> str:
    """Return `single` for float-only C code, otherwise `double`."""
    if re.search(r"\bdouble\b", src_no_comments):
        return "double"
    if re.search(r"\bfloat\b", src_no_comments):
        return "single"
    return "double"


def _normalize_kind_intrinsic_literals(text: str) -> str:
    """Keep `kind(1.0)` / `kind(1.0d0)` free of `_dp` suffixes."""
    return re.sub(r"(?i)\bkind\s*\(\s*([0-9]+(?:\.[0-9]*)?(?:[ed][+\-]?[0-9]+)?)_(?:dp|sp)\s*\)", r"kind(\1)", text)


def collapse_int_array_write_implied_do(lines: List[str]) -> List[str]:
    """Collapse specific implied-do integer print pattern to whole-array print.

    Target:
      write (*, "(*(a,i0))") (" ", int(C(i)), i = 1, UB)
    ->
      write (*, "(*(i0, 1x))") int(C)
    when UB matches a prior allocate(C(int(UB))) in the same unit.
    """
    out = list(lines)
    unit_start_re = re.compile(
        r"^\s*(?:[a-z][a-z0-9_()\s=,:]*\s+)?(?:function|subroutine)\b|^\s*program\b",
        re.IGNORECASE,
    )
    unit_end_re = re.compile(r"^\s*end\s+(?:function|subroutine|program)\b", re.IGNORECASE)
    alloc_re = re.compile(
        r"^\s*allocate\s*\(\s*([a-z][a-z0-9_]*)\s*\(\s*(?:int\s*\((.+)\)|(.+))\s*\)\s*(?:,.*)?\)\s*$",
        re.IGNORECASE,
    )
    decl_rank1_re = re.compile(r"^\s*real\b.*\ballocatable\b.*::\s*(.+)$", re.IGNORECASE)
    write_re = re.compile(
        r'^\s*write\s*\(\s*(.+?)\s*,\s*"?\'' r'?\(\*\(a\s*,\s*i0\)\)"?\'' r'?\s*\)\s*'
        r'\(\s*" "\s*,\s*int\s*\(\s*([a-z][a-z0-9_]*)\s*\(\s*([a-z][a-z0-9_]*)\s*\)\s*\)\s*,\s*'
        r'([a-z][a-z0-9_]*)\s*=\s*1\s*,\s*(.+)\)\s*$',
        re.IGNORECASE,
    )

    i = 0
    while i < len(out):
        if not unit_start_re.match(out[i].strip()):
            i += 1
            continue
        u_start = i
        j = i + 1
        while j < len(out) and not unit_end_re.match(out[j].strip()):
            j += 1
        u_end = j

        rank1_names: Set[str] = set()
        alloc_extent: Dict[str, str] = {}
        for k in range(u_start, u_end):
            code, _comment = fscan._split_code_comment(out[k].rstrip("\r\n"))  # type: ignore[attr-defined]
            mdecl = decl_rank1_re.match(code.strip())
            if mdecl:
                for ent in fscan._split_top_level_commas(mdecl.group(1)):  # type: ignore[attr-defined]
                    mname = re.match(r"^\s*([a-z][a-z0-9_]*)\s*\(\s*:\s*\)\s*$", ent.strip(), re.IGNORECASE)
                    if mname:
                        rank1_names.add(mname.group(1).lower())
            ma = alloc_re.match(code.strip())
            if ma:
                ext = ma.group(2) if ma.group(2) is not None else ma.group(3)
                alloc_extent[ma.group(1).lower()] = _norm_expr_token(ext)

        for k in range(u_start, u_end):
            raw = out[k]
            eol = "\r\n" if raw.endswith("\r\n") else ("\n" if raw.endswith("\n") else "\n")
            code, comment = fscan._split_code_comment(raw.rstrip("\r\n"))  # type: ignore[attr-defined]
            m = write_re.match(code.strip())
            if not m:
                continue
            unit_expr, arr, idx1, idx2, ub = m.group(1), m.group(2), m.group(3), m.group(4), m.group(5)
            if idx1.lower() != idx2.lower():
                continue
            arr_l = arr.lower()
            if arr_l not in rank1_names:
                continue
            ub_n = _norm_expr_token(ub)
            ext_n = alloc_extent.get(arr_l)
            if ext_n is None or ub_n != ext_n:
                continue
            indent = re.match(r"^\s*", code).group(0) if code else ""
            out[k] = f'{indent}write ({unit_expr}, "(*(i0, 1x))") int({arr}){comment}{eol}'

        i = u_end + 1
    return out


def rewrite_realloc_assign_patterns(lines: List[str]) -> List[str]:
    """Rewrite `x = int(realloc(x, n))` to allocatable growth via move_alloc."""
    out = list(lines)
    unit_start_re = re.compile(
        r"^\s*(?:[a-z][a-z0-9_()\s=,:]*\s+)?(?:function|subroutine)\b|^\s*program\b",
        re.IGNORECASE,
    )
    unit_end_re = re.compile(r"^\s*end\s+(?:function|subroutine|program)\b", re.IGNORECASE)
    declish_re = re.compile(
        r"^\s*(?:implicit\b|use\b|integer\b|real\b|logical\b|character\b|complex\b|type\b|class\b|procedure\b|save\b|parameter\b|external\b|intrinsic\b|common\b|equivalence\b|dimension\b)",
        re.IGNORECASE,
    )
    asn_re = re.compile(
        r"^\s*([a-z][a-z0-9_]*)\s*=\s*int\s*\(\s*realloc\s*\(\s*([a-z][a-z0-9_]*)\s*,\s*(.+)\)\s*\)\s*$",
        re.IGNORECASE,
    )
    i = 0
    while i < len(out):
        if not unit_start_re.match(out[i].strip()):
            i += 1
            continue
        u_start = i
        j = i + 1
        while j < len(out) and not unit_end_re.match(out[j].strip()):
            j += 1
        u_end = j

        # Collect allocatable declaration type for 1D vars.
        var_type: Dict[str, str] = {}
        for k in range(u_start + 1, u_end):
            code, _comment = fscan._split_code_comment(out[k].rstrip("\r\n"))  # type: ignore[attr-defined]
            if "::" not in code or "allocatable" not in code.lower():
                continue
            lhs, rhs = code.split("::", 1)
            spec = lhs.strip()
            for ent in fscan._split_top_level_commas(rhs):  # type: ignore[attr-defined]
                m = re.match(r"^\s*([a-z][a-z0-9_]*)\s*\(\s*:\s*\)\s*$", ent.strip(), re.IGNORECASE)
                if m:
                    var_type[m.group(1).lower()] = spec

        for k in range(u_start, u_end):
            raw = out[k]
            eol = "\r\n" if raw.endswith("\r\n") else ("\n" if raw.endswith("\n") else "\n")
            code, comment = fscan._split_code_comment(raw.rstrip("\r\n"))  # type: ignore[attr-defined]
            m = asn_re.match(code.strip())
            if not m:
                continue
            lhs = m.group(1)
            arg1 = m.group(2)
            n_expr = m.group(3).strip()
            if lhs.lower() != arg1.lower():
                continue
            if lhs.lower() not in var_type:
                continue
            n_expr = re.sub(r"\s*\*\s*1\s*$", "", n_expr)
            spec = var_type[lhs.lower()]
            indent = re.match(r"^\s*", code).group(0) if code else ""
            tmp = f"{lhs}_tmp"
            repl = [
                f"{indent}block{eol}",
                f"{indent}   {spec} :: {tmp}(:){eol}",
                f"{indent}   allocate({tmp}({n_expr})){eol}",
                f"{indent}   if (allocated({lhs})) {tmp}(1:min(size({lhs}), {n_expr})) = {lhs}(1:min(size({lhs}), {n_expr})){eol}",
                f"{indent}   call move_alloc({tmp}, {lhs}){eol}",
                f"{indent}end block{comment}{eol}",
            ]
            out[k : k + 1] = repl
            u_end += len(repl) - 1
            k += len(repl) - 1
        i = u_end + 1
    return out


def move_decl_comments_to_after_signature(lines: List[str]) -> List[str]:
    """Move comments in declaration section to immediately after unit signature."""
    out = list(lines)
    unit_start_re = re.compile(
        r"^\s*(?:[a-z][a-z0-9_()\s=,:]*\s+)?(?:function|subroutine)\b|^\s*program\b",
        re.IGNORECASE,
    )
    declish_re = re.compile(
        r"^\s*(?:implicit\b|use\b|integer\b|real\b|logical\b|character\b|complex\b|type\b|class\b|procedure\b|save\b|parameter\b|external\b|intrinsic\b|common\b|equivalence\b|dimension\b)",
        re.IGNORECASE,
    )
    i = 0
    while i < len(out):
        if not unit_start_re.match(out[i].strip()):
            i += 1
            continue
        sig_i = i
        j = i + 1
        comment_idx: List[int] = []
        while j < len(out):
            s = out[j].strip()
            if not s:
                j += 1
                continue
            if s.startswith("!"):
                comment_idx.append(j)
                j += 1
                continue
            if declish_re.match(s):
                j += 1
                continue
            break
        if comment_idx:
            comments = [out[k] for k in comment_idx]
            for k in reversed(comment_idx):
                del out[k]
            insert_at = sig_i + 1
            for c in comments:
                out.insert(insert_at, c)
                insert_at += 1
        i = j
    return out


def remove_redundant_final_return(lines: List[str]) -> List[str]:
    """Drop `return` when it appears immediately before `end function`."""
    out = list(lines)
    i = 0
    while i < len(out):
        if out[i].strip().lower().startswith("end function"):
            j = i - 1
            while j >= 0 and not out[j].strip():
                j -= 1
            if j >= 0 and out[j].strip().lower() == "return":
                del out[j]
                i -= 1
        i += 1
    return out


def remove_redundant_final_stop(lines: List[str]) -> List[str]:
    """Drop `stop` when it appears immediately before `end program`."""
    out = list(lines)
    i = 0
    while i < len(out):
        if out[i].strip().lower().startswith("end program"):
            j = i - 1
            while j >= 0 and not out[j].strip():
                j -= 1
            if j >= 0 and out[j].strip().lower() == "stop":
                del out[j]
                i -= 1
        i += 1
    return out


def move_inits_below_early_guard(lines: List[str]) -> List[str]:
    """Move simple initializations below an early validity guard when safe."""
    out = list(lines)
    unit_start_re = re.compile(
        r"^\s*(?:[a-z][a-z0-9_()\s=,:]*\s+)?(?:function|subroutine)\b|^\s*program\b",
        re.IGNORECASE,
    )
    declish_re = re.compile(
        r"^\s*(?:implicit\b|use\b|integer\b|real\b|logical\b|character\b|complex\b|type\b|class\b|procedure\b|save\b|parameter\b|external\b|intrinsic\b|common\b|equivalence\b|dimension\b)",
        re.IGNORECASE,
    )
    simple_init_re = re.compile(r"^\s*([a-z_]\w*)\s*=\s*[^=].*$", re.IGNORECASE)
    if_start_re = re.compile(r"^\s*if\s*\((.*)\)\s*then\s*$", re.IGNORECASE)
    return_re = re.compile(r"^\s*return\s*$", re.IGNORECASE)
    end_if_re = re.compile(r"^\s*end\s*if\b", re.IGNORECASE)
    token_re = re.compile(r"[a-z_]\w*", re.IGNORECASE)

    i = 0
    while i < len(out):
        if not unit_start_re.match(out[i].strip()):
            i += 1
            continue
        j = i + 1
        while j < len(out):
            s = out[j].strip()
            if not s:
                j += 1
                continue
            if s.startswith("!"):
                j += 1
                continue
            if declish_re.match(s):
                j += 1
                continue
            break
        exec_start = j
        init_idx: List[int] = []
        init_names: List[str] = []
        k = exec_start
        while k < len(out):
            s = out[k].strip()
            if not s or s.startswith("!"):
                k += 1
                continue
            m_init = simple_init_re.match(s)
            if not m_init:
                break
            init_idx.append(k)
            init_names.append(m_init.group(1).lower())
            k += 1
        if not init_idx:
            i = exec_start + 1
            continue
        g = k
        while g < len(out) and (not out[g].strip() or out[g].lstrip().startswith("!")):
            g += 1
        if g >= len(out):
            i = g
            continue
        m_if = if_start_re.match(out[g].strip())
        if not m_if:
            i = g + 1
            continue
        cond_text = m_if.group(1)
        depth = 1
        h = g + 1
        saw_return = False
        while h < len(out):
            sh = out[h].strip()
            if if_start_re.match(sh):
                depth += 1
            elif end_if_re.match(sh):
                depth -= 1
                if depth == 0:
                    break
            elif depth == 1 and return_re.match(sh):
                saw_return = True
            h += 1
        if h >= len(out) or not saw_return:
            i = g + 1
            continue
        guard_text = cond_text + "\n" + "".join(out[g + 1 : h])
        guard_tokens = {t.lower() for t in token_re.findall(guard_text)}
        if any(name in guard_tokens for name in init_names):
            i = h + 1
            continue
        moved = [out[idx] for idx in init_idx]
        for idx in reversed(init_idx):
            del out[idx]
        insert_at = h - len(init_idx) + 1
        for line in moved:
            out.insert(insert_at, line)
            insert_at += 1
        i = insert_at
    return out


def collapse_noadvance_integer_print_loops(lines: List[str]) -> List[str]:
    """Collapse common no-advance integer print loops into unlimited-format write."""
    out: List[str] = []
    removed_loop_vars: Set[str] = set()
    i = 0
    re_head = re.compile(
        r'^(?P<indent>\s*)write\(\*,\s*"\(i0,a\)",\s*advance="no"\)\s*(?P<head>[a-z_]\w*)\s*,\s*":"\s*$',
        re.IGNORECASE,
    )
    re_do = re.compile(
        r'^\s*do\s+(?P<iv>[a-z_]\w*)\s*=\s*0\s*,\s*\((?P<nf>[a-z_]\w*)\)\s*-\s*1\s*$',
        re.IGNORECASE,
    )
    re_item = re.compile(
        r'^\s*write\(\*,\s*"\(a,i0\)",\s*advance="no"\)\s*" "\s*,\s*(?P<arr>[a-z_]\w*)\((?P<iv2>[a-z_]\w*)\s*\+\s*1\)\s*$',
        re.IGNORECASE,
    )
    re_enddo = re.compile(r"^\s*end\s*do\s*$", re.IGNORECASE)
    re_nl = re.compile(r"^\s*write\(\*,\*\)\s*$", re.IGNORECASE)
    while i < len(lines):
        m1 = re_head.match(lines[i].rstrip("\n"))
        if m1 and i + 4 < len(lines):
            m2 = re_do.match(lines[i + 1].rstrip("\n"))
            if m2:
                iv = m2.group("iv")
                m3 = re_item.match(lines[i + 2].rstrip("\n"))
                if (
                    m3
                    and m3.group("iv2").lower() == iv.lower()
                    and re_enddo.match(lines[i + 3].rstrip("\n"))
                    and re_nl.match(lines[i + 4].rstrip("\n"))
                ):
                    indent = m1.group("indent")
                    head = m1.group("head")
                    arr = m3.group("arr")
                    out.append(f'{indent}write(*,"(i0,a,*(1x,i0))") {head}, ":", {arr}\n')
                    removed_loop_vars.add(iv.lower())
                    i += 5
                    continue
        out.append(lines[i])
        i += 1
    if not removed_loop_vars:
        return out

    unit_start_re = re.compile(
        r"^\s*(?:[a-z][a-z0-9_()\s=,:]*\s+)?(?:function|subroutine)\b|^\s*program\b",
        re.IGNORECASE,
    )
    unit_end_re = re.compile(r"^\s*end\s+(?:function|subroutine|program)\b", re.IGNORECASE)

    remove_by_line: Dict[int, Set[str]] = {}
    for v in removed_loop_vars:
        pat = re.compile(rf"\b{re.escape(v)}\b", re.IGNORECASE)
        decl_idxs: List[int] = []
        for idx, ln in enumerate(out):
            code, _comment = xunused.split_code_comment(ln.rstrip("\r\n"))
            if "::" in code and pat.search(code):
                decl_idxs.append(idx)
        for didx in decl_idxs:
            s = didx
            while s >= 0:
                code_s, _ = xunused.split_code_comment(out[s].rstrip("\r\n"))
                if unit_start_re.match(code_s.strip()):
                    break
                s -= 1
            if s < 0:
                continue
            t = didx
            while t < len(out):
                code_t, _ = xunused.split_code_comment(out[t].rstrip("\r\n"))
                if unit_end_re.match(code_t.strip()):
                    break
                t += 1
            if t >= len(out):
                continue
            used_elsewhere = False
            for k in range(s, t + 1):
                code_k, _ = xunused.split_code_comment(out[k].rstrip("\r\n"))
                if "::" in code_k and pat.search(code_k):
                    continue
                if pat.search(code_k):
                    used_elsewhere = True
                    break
            if not used_elsewhere:
                remove_by_line.setdefault(didx, set()).add(v)

    if not remove_by_line:
        return out
    cleaned: List[str] = []
    for idx, ln in enumerate(out):
        remove_here = remove_by_line.get(idx)
        if not remove_here:
            cleaned.append(ln)
            continue
        new_ln, _changed = xunused.rewrite_decl_remove_names(ln, remove_here)
        if new_ln is not None:
            cleaned.append(new_ln)
    return cleaned


def apply_dead_store_cleanup(lines: List[str]) -> List[str]:
    """Remove conservative set-but-never-read locals and their safe writes."""
    edits = fscan.find_set_but_never_read_local_edits([ln.rstrip("\r\n") for ln in lines])
    if not edits.decl_remove_by_line and not edits.remove_stmt_lines:
        return lines
    out: List[str] = []
    for idx1, ln in enumerate(lines, start=1):
        if idx1 in edits.remove_stmt_lines:
            continue
        remove_here = edits.decl_remove_by_line.get(idx1)
        if not remove_here:
            out.append(ln)
            continue
        new_ln, _changed = xunused.rewrite_decl_remove_names(ln, remove_here)
        if new_ln is not None:
            out.append(new_ln)
    return out


def add_pure_when_possible(lines: List[str]) -> List[str]:
    """Mark procedures PURE when xpure analyzer classifies them as candidates."""
    parsed = [ln.rstrip("\r\n") for ln in lines]
    sanitized: List[str] = []
    if_then_re = re.compile(r"^\s*(?:else\s+)?if\s*\((.*)\)\s*then\b", re.IGNORECASE)
    for s in parsed:
        m = if_then_re.match(s)
        if not m:
            sanitized.append(s)
            continue
        cond = m.group(1)
        # Avoid xpure's assignment-regex false positives on IF conditions.
        cond2 = cond.replace("==", ".eq.").replace("/=", ".ne.").replace(">=", ".ge.").replace("<=", ".le.")
        sanitized.append(s[: m.start(1)] + cond2 + s[m.end(1) :])
    result = xpure.analyze_lines(sanitized, strict_unknown_calls=False)
    if not result.candidates:
        return lines
    updated = list(lines)
    for proc in result.candidates:
        idx = proc.start - 1
        xpure.apply_decl_edit_at_or_continuation(updated, idx, xpure.add_pure_to_declaration)
    return updated


def inline_single_use_temp_assignments(lines: List[str]) -> List[str]:
    """Inline very-local temp assignments used once in the immediate next statement.

    Conservative scope:
    - assignment form: `name = expr` on one line (no ';' / '&')
    - next nonblank/comment statement uses `name` exactly once
    - no other uses of `name` in the containing unit
    """
    out = list(lines)
    unit_start_re = re.compile(
        r"^\s*(?:[a-z][a-z0-9_()\s=,:]*\s+)?(?:function|subroutine)\b|^\s*program\b",
        re.IGNORECASE,
    )
    unit_end_re = re.compile(r"^\s*end\s+(?:function|subroutine|program)\b", re.IGNORECASE)
    assign_re = re.compile(r"^\s*([a-z][a-z0-9_]*)\s*=\s*(.+)$", re.IGNORECASE)
    ident_re = re.compile(r"[a-z][a-z0-9_]*", re.IGNORECASE)

    unit_ranges: List[Tuple[int, int]] = []
    s: Optional[int] = None
    for i, raw in enumerate(out):
        code = fscan.strip_comment(raw).strip()
        if not code:
            continue
        if s is None and unit_start_re.match(code):
            s = i
            continue
        if s is not None and unit_end_re.match(code):
            unit_ranges.append((s, i))
            s = None
    if s is not None:
        unit_ranges.append((s, len(out) - 1))

    for us, ue in unit_ranges:
        removed_vars: Set[str] = set()
        i = us
        while i <= ue:
            raw = out[i]
            code, _comment = xunused.split_code_comment(raw.rstrip("\r\n"))
            stmt = code.strip()
            m_as = assign_re.match(stmt)
            if not m_as or ";" in stmt or "&" in stmt:
                i += 1
                continue
            var = m_as.group(1).lower()
            rhs = m_as.group(2).strip()
            if any(tok.group(0).lower() == var for tok in ident_re.finditer(rhs)):
                i += 1
                continue
            # immediate next nonblank/noncomment statement
            j = i + 1
            while j <= ue:
                code_j = fscan.strip_comment(out[j]).strip()
                if code_j:
                    break
                j += 1
            if j > ue:
                i += 1
                continue
            code_j, comment_j = xunused.split_code_comment(out[j].rstrip("\r\n"))
            occ_j = [m for m in ident_re.finditer(code_j) if m.group(0).lower() == var]
            if len(occ_j) != 1:
                i += 1
                continue
            # ensure no other use in unit
            use_count = 0
            for k in range(us, ue + 1):
                code_k, _ = xunused.split_code_comment(out[k].rstrip("\r\n"))
                if k == i:
                    continue
                if "::" in code_k:
                    continue
                use_count += sum(1 for mm in ident_re.finditer(code_k) if mm.group(0).lower() == var)
            if use_count != 1:
                i += 1
                continue
            m0 = occ_j[0]
            new_code_j = f"{code_j[:m0.start()]}{rhs}{code_j[m0.end():]}"
            eol_j = xunused.get_eol(out[j]) or "\n"
            out[j] = f"{new_code_j}{comment_j}{eol_j}"
            # drop assignment line
            out[i] = ""
            removed_vars.add(var)
            i = j + 1

        if removed_vars:
            # remove now-unused declaration entities in this unit
            for k in range(us, ue + 1):
                code_k, _ = xunused.split_code_comment(out[k].rstrip("\r\n"))
                if "::" not in code_k:
                    continue
                present = False
                for v in removed_vars:
                    if any(mm.group(0).lower() == v for mm in ident_re.finditer(code_k)):
                        present = True
                        break
                if not present:
                    continue
                still_used: Set[str] = set()
                for v in removed_vars:
                    for kk in range(us, ue + 1):
                        if kk == k:
                            continue
                        code_kk, _ = xunused.split_code_comment(out[kk].rstrip("\r\n"))
                        if any(mm.group(0).lower() == v for mm in ident_re.finditer(code_kk)):
                            still_used.add(v)
                            break
                to_remove = removed_vars - still_used
                if not to_remove:
                    continue
                new_ln, _changed = xunused.rewrite_decl_remove_names(out[k], to_remove)
                out[k] = "" if new_ln is None else new_ln

    return [ln for ln in out if ln != ""]


def main() -> int:
    ap = argparse.ArgumentParser(description="Partial C to Fortran transpiler")
    ap.add_argument("c_files", type=Path, nargs="+")
    ap.add_argument("--mode", choices=("each", "combined"), default="each", help="Interpret multiple input C files as separate translations (each) or one program (combined)")
    ap.add_argument("--out", type=Path, default=None)
    ap.add_argument("--out-dir", type=Path, default=None, help="Output directory for --mode each")
    ap.add_argument("--tee", action="store_true", help="Print generated Fortran")
    ap.add_argument("--raw", action="store_true", help="Emit raw transpilation output (skip optional post-processing)")
    ap.add_argument("--refactor", action="store_true", help="Extract long main-program blocks into module procedures")
    ap.add_argument("--array", action="store_true", help="Post-process generated Fortran with xarray.py")
    ap.add_argument("--array-inline", action="store_true", help="With --array, enable xarray inline post-pass")
    ap.add_argument("--inline-temp", action="store_true", help="Post-process generated Fortran with xno_variable.py")
    ap.add_argument("--run", action="store_true", help="Build and run generated Fortran output")
    ap.add_argument("--run-both", action="store_true", help="Build/run original C source and generated Fortran output")
    ap.add_argument("--compile-c", action="store_true", help="Compile generated Fortran with -c only (no link/run)")
    ap.add_argument("--compile-both", action="store_true", help="Build original C source and generated Fortran output (no run)")
    ap.add_argument("--compile-both-c", action="store_true", help="Compile original C and generated Fortran with -c only (no link/run)")
    ap.add_argument("--run-diff", action="store_true", help="With --run-both, diff stdout/stderr between C and Fortran runs")
    ap.add_argument("--time-both", action="store_true", help="With --run-both, time both executables and report speed ratio")
    ap.add_argument("--maxfail", type=int, default=None, help="Stop after N cases where C builds but generated Fortran does not")
    args = ap.parse_args()
    if args.out is not None and args.out_dir is not None:
        print("Use only one of --out or --out-dir.")
        return 2
    if args.mode == "combined" and args.out_dir is not None:
        print("--out-dir is only supported with --mode each.")
        return 2
    if args.mode == "each" and len(args.c_files) > 1 and args.out is not None:
        print("--out with multiple input files requires --mode combined or a single input file.")
        return 2
    if args.array_inline:
        args.array = True
    if args.maxfail is not None:
        if args.maxfail < 1:
            print("--maxfail must be >= 1.")
            return 2
        if not (args.run_both or args.compile_both or args.compile_both_c):
            print("--maxfail requires --run-both, --compile-both, or --compile-both-c.")
            return 2
    if args.raw:
        if args.refactor or args.array or args.array_inline or args.inline_temp:
            print("Note: --raw disables --refactor, --array/--array-inline, and --inline-temp.")
        # Raw mode bypasses optional post-processing/refactoring passes.
        args.refactor = False
        args.array = False
        args.array_inline = False
        args.inline_temp = False
    if args.run_diff or args.time_both:
        args.run_both = True
    if args.run_both:
        args.run = True

    def _transpile_text(text: str) -> str:
        fsrc_loc = transpile_c_to_fortran(text, refactor=args.refactor, raw=args.raw)
        if args.array:
            fsrc_loc = apply_xarray_postprocess(fsrc_loc, inline=args.array_inline)
        if args.inline_temp:
            fsrc_loc = apply_xno_variable_postprocess(fsrc_loc)
        post_lines_loc = fsrc_loc.splitlines(keepends=True)
        post_lines_loc = apply_dead_store_cleanup(post_lines_loc)
        post_lines_loc = fpost.remove_redundant_zero_before_reduction(post_lines_loc)
        post_lines_loc = fpost.remove_pre_overwrite_assignments(post_lines_loc)
        post_lines_loc = fpost.hoist_repeated_size_calls(post_lines_loc, min_uses=3)
        post_lines_loc = fpost.tighten_size_alias_nonpositive_guards(post_lines_loc)
        post_lines_loc = inline_single_use_temp_assignments(post_lines_loc)
        post_lines_loc = fpost.remove_unused_local_declarations(post_lines_loc)
        post_lines_loc = _coalesce_simple_declarations_preserve_intent(post_lines_loc)
        post_lines_loc = fscan.remove_redundant_int_casts(post_lines_loc)
        post_lines_loc = fscan.remove_redundant_real_casts(post_lines_loc)
        post_lines_loc = fscan.suffix_real_literals_with_kind(post_lines_loc, kind_name="dp")
        post_lines_loc = fscan.collapse_single_stmt_if_blocks(post_lines_loc)
        post_lines_loc = _remove_arg_style_doc_comments(post_lines_loc)
        fsrc_loc = _normalize_kind_intrinsic_literals("".join(post_lines_loc))
        if args.array:
            fsrc_loc = apply_xarray_postprocess(fsrc_loc, inline=args.array_inline)
            post_lines_loc = fsrc_loc.splitlines(keepends=True)
            post_lines_loc = apply_dead_store_cleanup(post_lines_loc)
            post_lines_loc = fpost.remove_redundant_zero_before_reduction(post_lines_loc)
            post_lines_loc = fpost.remove_pre_overwrite_assignments(post_lines_loc)
            post_lines_loc = fpost.tighten_size_alias_nonpositive_guards(post_lines_loc)
            post_lines_loc = inline_single_use_temp_assignments(post_lines_loc)
            post_lines_loc = fpost.remove_unused_local_declarations(post_lines_loc)
            post_lines_loc = _coalesce_simple_declarations_preserve_intent(post_lines_loc)
            post_lines_loc = fscan.remove_redundant_int_casts(post_lines_loc)
            post_lines_loc = fscan.remove_redundant_real_casts(post_lines_loc)
            post_lines_loc = fscan.suffix_real_literals_with_kind(post_lines_loc, kind_name="dp")
            post_lines_loc = fscan.collapse_single_stmt_if_blocks(post_lines_loc)
            post_lines_loc = _remove_arg_style_doc_comments(post_lines_loc)
            fsrc_loc = _normalize_kind_intrinsic_literals("".join(post_lines_loc))
        return fsrc_loc

    if args.mode == "combined":
        texts = [p.read_text(encoding="utf-8", errors="ignore") for p in args.c_files]
        text = "\n\n".join(texts)
        fsrc = _transpile_text(text)
        out_path = args.out if args.out is not None else Path("temp.f90")
        out_path.write_text(fsrc, encoding="utf-8")
        print(f"Wrote {out_path}")
        if args.tee:
            print(fsrc, end="")
        orig_ok = False
        orig_out = ""
        orig_err = ""
        orig_build_ok = False
        c_exe = out_path.with_name(f"{out_path.stem}.orig.exe")
        if args.run_both:
            orig_ok, orig_out, orig_err, orig_build_ok = _build_and_run_c_many(
                args.c_files,
                exe_path=c_exe,
                label="original-c",
            )
        elif args.compile_both or args.compile_both_c:
            orig_build_ok = _build_c_many_only(
                args.c_files,
                exe_path=c_exe,
                label="original-c",
                compile_only=bool(args.compile_both_c),
            )
        new_ok = False
        new_out = ""
        new_err = ""
        new_build_ok = False
        f_exe = out_path.with_suffix(".exe")
        if args.run:
            new_ok, new_out, new_err, new_build_ok = _build_and_run(
                out_path,
                compiler="gfortran",
                exe_path=f_exe,
                label="transformed-fortran",
                extra_args=DEFAULT_GFORTRAN_FLAGS,
            )
        elif args.compile_both_c or args.compile_c:
            f_obj = out_path.with_suffix(".o")
            cmd_f = ["gfortran", "-c", str(out_path), *DEFAULT_GFORTRAN_FLAGS, "-o", str(f_obj)]
            new_build_ok = _build_only_cmd(cmd_f, label="transformed-fortran")
        elif args.compile_both:
            cmd_f = ["gfortran", str(out_path), *DEFAULT_GFORTRAN_FLAGS, "-o", str(f_exe)]
            new_build_ok = _build_only_cmd(cmd_f, label="transformed-fortran")
        if args.maxfail is not None and orig_build_ok and not new_build_ok:
            print(f"Reached maxfail={args.maxfail} (combined case where C built and Fortran did not).")
        if args.run_diff and args.run_both and orig_ok and new_ok:
            if (orig_out == new_out) and (orig_err == new_err):
                print("Run diff: MATCH")
            else:
                print("Run diff: DIFF")
                old = f"STDOUT:\n{orig_out}\nSTDERR:\n{orig_err}\n"
                new = f"STDOUT:\n{new_out}\nSTDERR:\n{new_err}\n"
                for ln in difflib.unified_diff(
                    old.splitlines(),
                    new.splitlines(),
                    fromfile="original-c",
                    tofile="transformed-fortran",
                    lineterm="",
                ):
                    print(ln)
        if args.time_both and args.run_both and orig_ok and new_ok:
            t_c = _time_executable(c_exe, label="original-c")
            t_f = _time_executable(f_exe, label="transformed-fortran")
            if t_c is not None and t_f is not None and t_c > 0:
                print(f"Time ratio (fortran/c): {t_f / t_c:.3f}")
        return 0

    # mode each
    if args.out_dir is not None:
        args.out_dir.mkdir(parents=True, exist_ok=True)
    fail_count = 0
    for c_file in args.c_files:
        text = c_file.read_text(encoding="utf-8", errors="ignore")
        fsrc = _transpile_text(text)
        if args.out_dir is not None:
            out_path = args.out_dir / f"{c_file.stem}.f90"
        elif args.out is not None:
            out_path = args.out
        elif len(args.c_files) == 1:
            out_path = Path("temp.f90")
        else:
            out_path = Path(f"{c_file.stem}.f90")
        out_path.write_text(fsrc, encoding="utf-8")
        print(f"Wrote {out_path}")
        if args.tee:
            print(fsrc, end="")
        orig_ok = False
        orig_out = ""
        orig_err = ""
        orig_build_ok = False
        c_exe = c_file.with_suffix(".orig.exe")
        if args.run_both:
            orig_ok, orig_out, orig_err, orig_build_ok = _build_and_run(
                c_file,
                compiler="gcc",
                exe_path=c_exe,
                label="original-c",
                extra_args=["-lm"],
            )
        elif args.compile_both or args.compile_both_c:
            if args.compile_both_c:
                c_obj = c_file.with_suffix(".orig.o")
                cmd_c = ["gcc", "-c", str(c_file), "-o", str(c_obj)]
            else:
                cmd_c = ["gcc", str(c_file), "-lm", "-o", str(c_exe)]
            orig_build_ok = _build_only_cmd(cmd_c, label="original-c")
        new_ok = False
        new_out = ""
        new_err = ""
        new_build_ok = False
        f_exe = out_path.with_suffix(".exe")
        if args.run:
            new_ok, new_out, new_err, new_build_ok = _build_and_run(
                out_path,
                compiler="gfortran",
                exe_path=f_exe,
                label="transformed-fortran",
                extra_args=DEFAULT_GFORTRAN_FLAGS,
            )
        elif args.compile_both_c or args.compile_c:
            f_obj = out_path.with_suffix(".o")
            cmd_f = ["gfortran", "-c", str(out_path), *DEFAULT_GFORTRAN_FLAGS, "-o", str(f_obj)]
            new_build_ok = _build_only_cmd(cmd_f, label="transformed-fortran")
        elif args.compile_both:
            cmd_f = ["gfortran", str(out_path), *DEFAULT_GFORTRAN_FLAGS, "-o", str(f_exe)]
            new_build_ok = _build_only_cmd(cmd_f, label="transformed-fortran")
        if args.maxfail is not None and orig_build_ok and not new_build_ok:
            fail_count += 1
            if fail_count >= args.maxfail:
                print(f"Stopped at maxfail={args.maxfail} (C built, Fortran did not).")
                break
        if args.run_diff and args.run_both and orig_ok and new_ok:
            if (orig_out == new_out) and (orig_err == new_err):
                print(f"Run diff ({c_file.name}): MATCH")
            else:
                print(f"Run diff ({c_file.name}): DIFF")
                old = f"STDOUT:\n{orig_out}\nSTDERR:\n{orig_err}\n"
                new = f"STDOUT:\n{new_out}\nSTDERR:\n{new_err}\n"
                for ln in difflib.unified_diff(
                    old.splitlines(),
                    new.splitlines(),
                    fromfile="original-c",
                    tofile="transformed-fortran",
                    lineterm="",
                ):
                    print(ln)
        if args.time_both and args.run_both and orig_ok and new_ok:
            t_c = _time_executable(c_exe, label="original-c")
            t_f = _time_executable(f_exe, label="transformed-fortran")
            if t_c is not None and t_f is not None and t_c > 0:
                print(f"Time ratio (fortran/c): {t_f / t_c:.3f}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
