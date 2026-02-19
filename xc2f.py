#!/usr/bin/env python3
"""xc2f.py: small C->Fortran transpiler for a practical C subset.

Current focus: enough coverage to translate xfactors.c into compilable Fortran.
"""

from __future__ import annotations

import argparse
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Optional, Set, Tuple
from collections import defaultdict

from pycparser import c_ast, c_parser
import fortran_scan as fscan
import xalloc_assign
import xunused
import xpure


PRELUDE = """
typedef unsigned long size_t;
void *malloc(size_t);
void free(void *);
double sqrt(double);
double floor(double);
int printf(const char *, ...);
""".strip()


@dataclass
class VarInfo:
    ftype: str  # 'integer' | 'real'
    alloc: bool = False


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


def c_to_ftype(type_decl: c_ast.Node) -> Tuple[str, bool]:
    node = type_decl
    alloc = False
    while isinstance(node, (c_ast.TypeDecl, c_ast.PtrDecl)):
        if isinstance(node, c_ast.PtrDecl):
            alloc = True
            node = node.type
        else:
            node = node.type
    if isinstance(node, c_ast.IdentifierType):
        names = [n.lower() for n in node.names]
        if "void" in names:
            return "void", alloc
        if "double" in names or "float" in names:
            return "real(kind=real64)", alloc
        return "integer", alloc
    return "integer", alloc


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


def get_id_name(node: c_ast.Node) -> Optional[str]:
    if isinstance(node, c_ast.ID):
        return node.name
    return None


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

    def emit(self, line: str = "") -> None:
        self.lines.append(" " * self.indent + line)

    def simp(self, expr: str) -> str:
        return fscan.strip_redundant_outer_parens_expr(expr)

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

    def pop_leading_comments_before(self, src_line: int, *, window: int = 40) -> List[str]:
        """Collect and remove nearby leading comments (before signature)."""
        lo = max(1, src_line - window)
        pending: List[str] = []
        for ln in range(lo, src_line + 1):
            vals = self.comment_map.get(ln, [])
            if vals:
                pending.extend(vals)
                self.comment_map[ln] = []
        return pending

    def expr(self, n: c_ast.Node) -> str:
        if isinstance(n, c_ast.Constant):
            if n.type == "string":
                return n.value
            return n.value
        if isinstance(n, c_ast.ID):
            if n.name == "NULL":
                return "0"
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
                return self.simp(f"real({inner}, kind=real64)")
            return inner
        if isinstance(n, c_ast.UnaryOp):
            op = n.op
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
                return self.simp(f".not. ({self.expr(n.expr)})")
        if isinstance(n, c_ast.BinaryOp):
            op = n.op
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
            if op == "%":
                return self.simp(f"mod({self.expr(n.left)}, {self.expr(n.right)})")
            return self.simp(f"({self.expr(n.left)} {op_map.get(op, op)} {self.expr(n.right)})")
        if isinstance(n, c_ast.TernaryOp):
            return f"merge({self.expr(n.iftrue)}, {self.expr(n.iffalse)}, {self.expr(n.cond)})"
        if isinstance(n, c_ast.ArrayRef):
            name = self.expr(n.name)
            idx = n.subscript
            if isinstance(idx, c_ast.UnaryOp) and idx.op == "p++":
                return f"{name}({self.expr(idx.expr)}+1)"
            return f"{name}({self.expr(idx)}+1)"
        if isinstance(n, c_ast.FuncCall):
            fname = self.expr(n.name)
            args = []
            if n.args is not None:
                args = [self.expr(a) for a in n.args.exprs]
            if fname == "sqrt":
                return self.simp(f"sqrt(real({args[0]}, kind=real64))")
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
            return self.expr(n.name)
        raise NotImplementedError(f"Unsupported expr: {type(n).__name__}")

    def emit_printf(self, fc: c_ast.FuncCall) -> bool:
        args = fc.args.exprs if fc.args is not None else []
        if not args:
            return False
        fmt_node = args[0]
        if not isinstance(fmt_node, c_ast.Constant) or fmt_node.type != "string":
            return False
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
        items.sort(key=lambda x: (1 if x[1].alloc else 0, x[0].lower()))
        for n, info in items:
            self.emit_decl(n, info, params=params, ret_name=ret_name)

    def emit_for(self, st: c_ast.For) -> None:
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

        ub = self.expr(st.cond.right)
        step = None
        if st.cond.op in ("<", ">"):
            if st.cond.op == "<":
                ub = f"({ub})-1"
            else:
                ub = f"({ub})+1"

        if isinstance(st.next, c_ast.UnaryOp) and st.next.op == "p++":
            step = None
        elif isinstance(st.next, c_ast.UnaryOp) and st.next.op == "p--":
            step = "-1"
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
            if isinstance(fc.name, c_ast.ID) and fc.name.name == "malloc" and fc.args is not None and fc.args.exprs:
                arg = fc.args.exprs[0]
                # expect (size_t)cnt_total * sizeof(int)
                if isinstance(arg, c_ast.BinaryOp) and arg.op == "*":
                    n = self.expr(arg.left)
                else:
                    n = self.expr(arg)
                lhs = self.expr(st.lvalue)
                self.emit(f"allocate({lhs}({n}))")
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
                self.emit_comments_to(src_line)
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
                        if isinstance(arg, c_ast.BinaryOp) and arg.op == "*":
                            n = self.expr(arg.left)
                        else:
                            n = self.expr(arg)
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
        if isinstance(st, c_ast.FuncCall):
            if isinstance(st.name, c_ast.ID) and st.name.name == "printf":
                if not self.emit_printf(st):
                    self.emit("! unsupported printf")
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
        raise NotImplementedError(f"Unsupported stmt: {type(st).__name__}")


def emit_function(fn: c_ast.FuncDef, em: Emitter, *, main_use_names: Optional[List[str]] = None) -> None:
    decl = fn.decl
    fdecl = decl.type
    if not isinstance(fdecl, c_ast.FuncDecl):
        raise NotImplementedError("Expected FuncDecl")
    name = decl.name
    fn_src_line: Optional[int] = None
    if decl.coord is not None and getattr(decl.coord, "line", None):
        src_line = int(decl.coord.line) - em.line_offset
        if src_line >= 1:
            fn_src_line = src_line

    params: List[c_ast.Decl] = []
    if fdecl.args is not None:
        params = [p for p in fdecl.args.params if isinstance(p, c_ast.Decl)]

    if name == "main":
        em.emit("program main")
        if main_use_names:
            em.emit(f"use xc2f_mod, only: {', '.join(main_use_names)}")
        em.emit("use, intrinsic :: iso_fortran_env, only: real64")
        em.emit("implicit none")

        locals_map: Dict[str, VarInfo] = {}
        gather_decls(fn.body, locals_map)
        for n, info in locals_map.items():
            if info.alloc:
                em.pointer_like_names.add(n.lower())
        em.indent = 0
        em.emit_decl_grouped(locals_map, params=set(), ret_name=None)
        em.emit_stmt(fn.body, ret_name=None)
        em.emit("end program main")
        em.pointer_like_names.clear()
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

    if out_idx is not None:
        em.emit(f"function {name}({', '.join(pnames)}) result({out_param_name})")
        unit_kind = "function"
    elif ret_ftype.lower() == "void":
        em.emit(f"subroutine {name}({', '.join(pnames)})")
        unit_kind = "subroutine"
    else:
        em.emit(f"{ret_ftype} function {name}({', '.join(pnames)})")
        unit_kind = "function"
    if fn_src_line is not None:
        comments = em.pop_leading_comments_before(fn_src_line)
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
    em.emit("use, intrinsic :: iso_fortran_env, only: real64")

    param_set = set(pnames)
    # params
    for idx, p in enumerate(params):
        if out_idx is not None and idx == out_idx:
            continue
        p_ft, p_alloc = c_to_ftype(p.type)
        p_const = type_has_const(p.type)
        intent = "intent(in)" if (p_const or not p_alloc) else "intent(inout)"
        if p_alloc:
            em.emit(f"{p_ft}, {intent} :: {p.name}(:)")
            em.pointer_like_names.add(p.name.lower())
        else:
            em.emit(f"{p_ft}, {intent} :: {p.name}")
    if out_idx is not None and out_param_name is not None:
        em.emit(f"integer, allocatable :: {out_param_name}(:)")
        em.array_result_name = out_param_name

    # locals
    locals_map: Dict[str, VarInfo] = {}
    gather_decls(fn.body, locals_map)
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
    em.emit_decl_grouped(locals_map, params=param_set, ret_name=name if unit_kind == "function" else None)

    # body
    em.emit_stmt(fn.body, ret_name=name if unit_kind == "function" else None, array_result_name=out_param_name if out_idx is not None else None)
    if unit_kind == "subroutine":
        em.emit(f"end subroutine {name}")
    else:
        em.emit(f"end function {name}")
    em.array_result_name = None
    em.array_result_tmp_alias = None
    em.pointer_like_names.clear()


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


def transpile_c_to_fortran(text: str) -> str:
    no_pp = strip_preprocessor_only(text)
    comments = extract_preserved_comments(no_pp)
    src = strip_preprocessor_and_comments(text)
    parser = c_parser.CParser()
    ast = parser.parse(PRELUDE + "\n" + src)

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
    for m in mains:
        main_called_module_names.update(collect_called_names(m, module_proc_names))

    if funcs:
        em.emit("module xc2f_mod")
        em.emit("implicit none")
        em.emit("private")
        if mains:
            publics = sorted(main_called_module_names)
        else:
            publics = sorted(module_proc_names)
        if publics:
            em.emit(f"public :: {', '.join(publics)}")
        em.emit("contains")
        em.emit("")
        for ext in funcs:
            emit_function(ext, em)
            em.emit("")
        em.emit("end module xc2f_mod")
        em.emit("")

    for ext in mains:
        emit_function(ext, em, main_use_names=sorted(main_called_module_names))
        em.emit("")
    lines = [ln + "\n" for ln in em.lines]
    lines = fscan.coalesce_simple_declarations(lines)
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
    lines = apply_dead_store_cleanup(lines)
    lines = fscan.prune_unused_use_only_lines(lines)
    lines = fscan.avoid_reserved_identifier_definitions(lines)
    lines = fscan.simplify_redundant_parens_in_lines(lines)
    lines = fscan.simplify_integer_arithmetic_in_lines(lines)
    lines = fscan.coalesce_contiguous_scalar_assignments_to_constructor(lines)
    lines = add_pure_when_possible(lines)
    out_text = "".join(lines).rstrip() + "\n"
    if array_result_funcs:
        out_text = out_text.replace(
            "! - return value is the number of factors (0 on error)\n",
            "! - return value is the factors array (size(...) gives count; empty on error)\n",
        )
    out_text, _n_alloc_assign = xalloc_assign.rewrite_text_allocation_on_assignment(out_text)
    out_text = apply_shared_kind_module_dp(out_text)
    return out_text


def apply_shared_kind_module_dp(text: str) -> str:
    """Use a shared kind_mod(dp=real64) when both module and main are present.

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
    if "real64" not in text:
        return text

    # Rewrite kinds.
    lines = [re.sub(r"\bkind\s*=\s*real64\b", "kind=dp", ln) for ln in lines]

    # Remove direct real64 USE lines.
    use_real64_re = re.compile(r"^\s*use\s*,\s*intrinsic\s*::\s*iso_fortran_env\s*,\s*only\s*:\s*real64\s*$", re.IGNORECASE)
    lines = [ln for ln in lines if not use_real64_re.match(ln.strip())]

    # Add `use kind_mod, only: dp` in xc2f_mod spec part.
    if mod_idx is not None:
        ins_mod = mod_idx + 1
        if ins_mod <= len(lines):
            lines.insert(ins_mod, "use kind_mod, only: dp\n")

    # Add `use kind_mod, only: dp` in main program after any use lines.
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
        lines.insert(ins_main, "use kind_mod, only: dp\n")

    kind_mod_block = [
        "module kind_mod\n",
        "use, intrinsic :: iso_fortran_env, only: real64\n",
        "implicit none\n",
        "integer, parameter :: dp = real64\n",
        "end module kind_mod\n",
        "\n",
    ]
    lines = kind_mod_block + lines
    return "".join(lines)


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
    ap.add_argument("c_file", type=Path)
    ap.add_argument("--out", type=Path, default=Path("temp.f90"))
    ap.add_argument("--tee", action="store_true", help="Print generated Fortran")
    args = ap.parse_args()

    text = args.c_file.read_text(encoding="utf-8", errors="ignore")
    fsrc = transpile_c_to_fortran(text)
    args.out.write_text(fsrc, encoding="utf-8")
    print(f"Wrote {args.out}")
    if args.tee:
        print(fsrc, end="")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
