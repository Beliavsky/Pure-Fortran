#!/usr/bin/env python3
"""
oct2f90.py - a minimal Octave -> Fortran transpiler for a small subset.

Goal:
- Translate a simple Octave/Matlab-style *script* (.m) into a Fortran main program
  that can be linked with your helper modules (funcs_mod, linear_algebra_mod).

Supported subset (by design):
- comments starting with % (outside strings)
- statement separators: newline or ';'
- assignments:   x = expr
- indexed assignment: x(i) = expr, a(i,j) = expr
- function-call statements: disp(expr), printf(\"...\")
- if/elseif/else/end blocks (simple conditions)
- for/end blocks: for i = a:b  or  for i = a:s:b
- expressions with:
    literals (numbers, \"strings\"), variables,
    function calls, indexing, array literals [ ... ], ranges a:b or a:s:b,
    operators: + - * / ^  and elementwise: .* ./ .^
    comparisons: < <= > >= == ~=
    logical: && ||, unary ~, unary +/-
- array literals:
    [1 2 3] or [1,2,3] => vector
    [1 2; 3 4]        => matrix (row-major in source, converted to column-major for Fortran reshape)

Not supported (kept out of scope for a first transpiler):
- user-defined Octave functions
- slicing with ':' inside indexing (A(:,2), A(1:end), etc.)
- anonymous functions, cell arrays, structs
- broadcasting / implicit expansion
- logical-mask assignment x(mask)=...
- complex literals like 1+2i (but complex can appear from eig())

Runtime assumptions:
- You have funcs_mod with dp and Octave-like helpers (linspace, zeros, ones, eye, diag, reshape_oct, sum_oct, ...)
- You have linear_algebra_mod with inv, pinv, det, rank, trace, norm, eig, svd, qr, lu, chol (function forms)

Usage:
  python oct2f90.py input.m -o output.f90
"""

from __future__ import annotations
import argparse
import os
import re
from dataclasses import dataclass
from typing import List, Optional, Tuple, Dict, Any

# -------------------------
# tokenizer
# -------------------------

@dataclass
class Tok:
    kind: str
    val: str
    pos: int

_token_re = re.compile(
    r"""
    (?P<ws>\s+)|
    (?P<number>(?:\d+\.\d*|\d*\.\d+|\d+)(?:[eE][+\-]?\d+)?)|
    (?P<string>\"(?:\\.|[^\"\\])*\")|
    (?P<op>\.\^|\.\*|\.\/|>=|<=|==|~=|&&|\|\||[+\-*/^<>=(),;\[\]:']|~)|
    (?P<name>[A-Za-z_]\w*)
    """,
    re.VERBOSE,
)

def tokenize(s: str) -> List[Tok]:
    out: List[Tok] = []
    i = 0
    while i < len(s):
        m = _token_re.match(s, i)
        if not m:
            raise SyntaxError(f"unexpected character at {i}: {s[i:i+20]!r}")
        if m.lastgroup == "ws":
            i = m.end()
            continue
        kind = m.lastgroup or ""
        val = m.group(kind)
        out.append(Tok(kind, val, i))
        i = m.end()
    out.append(Tok("eof", "", len(s)))
    return out

# -------------------------
# AST nodes
# -------------------------

@dataclass
class Expr: ...

@dataclass
class Num(Expr):
    text: str

@dataclass
class Str(Expr):
    text: str  # includes quotes

@dataclass
class Name(Expr):
    id: str

@dataclass
class BoolLit(Expr):
    val: bool

@dataclass
class Call(Expr):
    fn: str
    args: List[Expr]

@dataclass
class Index(Expr):
    base: str
    subs: List[Expr]  # no ':' allowed in this subset

@dataclass
class ArrayLit(Expr):
    rows: List[List[Expr]]  # rows of entries

@dataclass
class Range(Expr):
    a: Expr
    b: Expr
    step: Optional[Expr] = None  # if provided: a:step:b

@dataclass
class Unary(Expr):
    op: str
    x: Expr

@dataclass
class Bin(Expr):
    op: str
    a: Expr
    b: Expr

@dataclass
class PostTranspose(Expr):
    x: Expr

# -------------------------
# Parser (Pratt)
# -------------------------

class Parser:
    def __init__(self, toks: List[Tok]):
        self.toks = toks
        self.i = 0

    def cur(self) -> Tok:
        return self.toks[self.i]

    def eat(self, kind: Optional[str] = None, val: Optional[str] = None) -> Tok:
        t = self.cur()
        if kind and t.kind != kind:
            raise SyntaxError(f"expected {kind}, got {t.kind} at {t.pos}")
        if val and t.val != val:
            raise SyntaxError(f"expected {val!r}, got {t.val!r} at {t.pos}")
        self.i += 1
        return t

    def parse_expr(self, rbp: int = 0) -> Expr:
        t = self.cur()
        self.i += 1
        left = self.nud(t)

        while self.cur().kind == "op" and self.cur().val == "'":
            self.eat("op", "'")
            left = PostTranspose(left)

        while rbp < self.lbp(self.cur()):
            t = self.cur()
            self.i += 1
            left = self.led(t, left)

            while self.cur().kind == "op" and self.cur().val == "'":
                self.eat("op", "'")
                left = PostTranspose(left)

        return left

    def nud(self, t: Tok) -> Expr:
        if t.kind == "number":
            return Num(t.val)
        if t.kind == "string":
            return Str(t.val)
        if t.kind == "name":
            low = t.val.lower()
            if low == "true":
                return BoolLit(True)
            if low == "false":
                return BoolLit(False)
            if self.cur().kind == "op" and self.cur().val == "(":
                self.eat("op", "(")
                args = self.parse_arglist(")")
                self.eat("op", ")")
                return Call(t.val, args)
            return Name(t.val)
        if t.kind == "op" and t.val in ("+", "-", "~"):
            x = self.parse_expr(70)
            return Unary(t.val, x)
        if t.kind == "op" and t.val == "(":
            x = self.parse_expr(0)
            self.eat("op", ")")
            return x
        if t.kind == "op" and t.val == "[":
            rows = self.parse_array_lit()
            return ArrayLit(rows)
        raise SyntaxError(f"bad token in expression: {t.kind} {t.val!r} at {t.pos}")

    def parse_array_lit(self) -> List[List[Expr]]:
        rows: List[List[Expr]] = []
        currow: List[Expr] = []
        while True:
            if self.cur().kind == "op" and self.cur().val == "]":
                self.eat("op", "]")
                if currow or not rows:
                    rows.append(currow)
                if len(rows) == 1 and len(rows[0]) == 0:
                    rows = [[]]
                return rows

            if self.cur().kind == "op" and self.cur().val == ";":
                self.eat("op", ";")
                rows.append(currow)
                currow = []
                continue

            if self.cur().kind == "op" and self.cur().val == ",":
                self.eat("op", ",")
                continue

            currow.append(self.parse_expr(0))

    def parse_arglist(self, until: str) -> List[Expr]:
        args: List[Expr] = []
        if self.cur().kind == "op" and self.cur().val == until:
            return args
        while True:
            args.append(self.parse_expr(0))
            if self.cur().kind == "op" and self.cur().val == ",":
                self.eat("op", ",")
                continue
            break
        return args

    def lbp(self, t: Tok) -> int:
        if t.kind != "op":
            return 0
        op = t.val
        if op == ":":
            return 10
        if op in ("||",):
            return 20
        if op in ("&&",):
            return 30
        if op in ("==", "~=", "<", ">", "<=", ">="):
            return 40
        if op in ("+", "-"):
            return 50
        if op in ("*", "/", ".*", "./"):
            return 60
        if op in ("^", ".^"):
            return 80
        return 0

    def led(self, t: Tok, left: Expr) -> Expr:
        op = t.val
        if op == ":":
            right = self.parse_expr(10)
            if self.cur().kind == "op" and self.cur().val == ":":
                self.eat("op", ":")
                right2 = self.parse_expr(10)
                return Range(left, right2, step=right)
            return Range(left, right, step=None)

        if op in ("+", "-", "*", "/", ".*", "./", "^", ".^",
                  "==", "~=", "<", "<=", ">", ">=", "&&", "||"):
            rbp = self.lbp(Tok("op", op, 0))
            if op in ("^", ".^"):
                rbp -= 1
            right = self.parse_expr(rbp)
            return Bin(op, left, right)

        raise SyntaxError(f"unknown operator {op!r}")

# -------------------------
# statements
# -------------------------

@dataclass
class Stmt: ...

@dataclass
class Assign(Stmt):
    lhs: Any  # Name | Index | list[str]
    rhs: Expr

@dataclass
class CallStmt(Stmt):
    call: Call

@dataclass
class IfStmt(Stmt):
    cond: Expr
    then_body: List[Stmt]
    elifs: List[Tuple[Expr, List[Stmt]]]
    else_body: List[Stmt]

@dataclass
class ForStmt(Stmt):
    var: str
    rng: Range
    body: List[Stmt]

@dataclass
class BreakStmt(Stmt):
    pass

@dataclass
class ContinueStmt(Stmt):
    pass

# -------------------------
# preprocessing
# -------------------------

def strip_comments(line: str) -> str:
    out = []
    in_str = False
    esc = False
    for ch in line:
        if in_str:
            out.append(ch)
            if esc:
                esc = False
            elif ch == "\\":
                esc = True
            elif ch == '"':
                in_str = False
            continue
        if ch == '"':
            in_str = True
            out.append(ch)
            continue
        if ch == "%":
            break
        out.append(ch)
    return "".join(out)

def split_statements(text: str) -> List[str]:
    stmts: List[str] = []
    buf = []
    depth_paren = 0
    depth_brack = 0
    in_str = False
    esc = False

    def flush():
        s = "".join(buf).strip()
        if s:
            stmts.append(s)
        buf.clear()

    for ch in text:
        if in_str:
            buf.append(ch)
            if esc:
                esc = False
            elif ch == "\\":
                esc = True
            elif ch == '"':
                in_str = False
            continue
        if ch == '"':
            in_str = True
            buf.append(ch)
            continue
        if ch == "(":
            depth_paren += 1
            buf.append(ch)
            continue
        if ch == ")":
            depth_paren = max(0, depth_paren - 1)
            buf.append(ch)
            continue
        if ch == "[":
            depth_brack += 1
            buf.append(ch)
            continue
        if ch == "]":
            depth_brack = max(0, depth_brack - 1)
            buf.append(ch)
            continue
        if ch in ("\n", ";") and depth_paren == 0 and depth_brack == 0:
            flush()
            continue
        buf.append(ch)

    flush()
    return stmts

def parse_expr(text: str) -> Expr:
    toks = tokenize(text)
    p = Parser(toks)
    e = p.parse_expr(0)
    if p.cur().kind != "eof":
        raise SyntaxError(f"trailing tokens after expression near pos {p.cur().pos}")
    return e

def parse_stmt_list(stmts: List[str], k0: int = 0, stop: Optional[set] = None) -> Tuple[List[Stmt], int]:
    # Parse a list of statements until:
    # - an explicit "end" token (consumed), or
    # - a token in stop (not consumed)
    if stop is None:
        stop = set()

    out: List[Stmt] = []
    k = k0
    while k < len(stmts):
        s = stmts[k].strip()
        if not s:
            k += 1
            continue

        low = s.lower()

        if low in stop:
            return out, k

        if low == "end":
            return out, k + 1

        if low == "break":
            out.append(BreakStmt())
            k += 1
            continue

        if low == "continue":
            out.append(ContinueStmt())
            k += 1
            continue

        if low.startswith("for "):
            m = re.match(r"for\s+([A-Za-z_]\w*)\s*=\s*(.*)$", s, flags=re.IGNORECASE)
            if not m:
                raise SyntaxError(f"bad for syntax: {s}")
            var = m.group(1)
            rng = parse_expr(m.group(2))
            if not isinstance(rng, Range):
                raise SyntaxError("for-loop requires a range a:b or a:s:b in this subset")
            body, k2 = parse_stmt_list(stmts, k + 1)
            out.append(ForStmt(var, rng, body))
            k = k2
            continue

        if low.startswith("if "):
            cond = parse_expr(s[2:].strip())

            then_body, k2 = parse_stmt_list(stmts, k + 1, stop={"elseif", "else", "end"})

            elifs: List[Tuple[Expr, List[Stmt]]] = []
            else_body: List[Stmt] = []

            k = k2
            while True:
                if k >= len(stmts):
                    raise SyntaxError("unterminated if block (missing end)")
                t = stmts[k].strip()
                tl = t.lower()
                if tl.startswith("elseif "):
                    c = parse_expr(t[6:].strip())
                    b, k3 = parse_stmt_list(stmts, k + 1, stop={"elseif", "else", "end"})
                    elifs.append((c, b))
                    k = k3
                    continue
                if tl == "else":
                    b, k3 = parse_stmt_list(stmts, k + 1, stop={"end"})
                    else_body = b
                    k = k3
                    # next token must be end, consumed below
                    if k >= len(stmts) or stmts[k].strip().lower() != "end":
                        raise SyntaxError("unterminated else block (missing end)")
                    k += 1
                    break
                if tl == "end":
                    k += 1
                    break
                raise SyntaxError(f"unexpected token in if chain: {t}")
            out.append(IfStmt(cond, then_body, elifs, else_body))
            continue

        if s.startswith("["):
            m = re.match(r"^\[\s*([^\]]+)\s*\]\s*=\s*(.*)$", s)
            if not m:
                raise SyntaxError(f"bad multiple assignment: {s}")
            lhs_list = [x.strip() for x in m.group(1).split(",")]
            rhs = parse_expr(m.group(2))
            out.append(Assign(lhs_list, rhs))
            k += 1
            continue

        if "=" in s and not re.match(r"^\s*==", s):
            parts = re.split(r"(?<![<>=~])=(?!=)", s, maxsplit=1)
            if len(parts) == 2:
                lhs_txt = parts[0].strip()
                rhs_txt = parts[1].strip()
                lhs_expr = parse_expr(lhs_txt)
                if isinstance(lhs_expr, Name):
                    lhs = lhs_expr
                elif isinstance(lhs_expr, Call):
                    lhs = Index(lhs_expr.fn, lhs_expr.args)
                else:
                    raise SyntaxError("lhs must be a variable or indexed variable in this subset")
                rhs = parse_expr(rhs_txt)
                out.append(Assign(lhs, rhs))
                k += 1
                continue

        e = parse_expr(s)
        if isinstance(e, Call):
            out.append(CallStmt(e))
            k += 1
            continue

        raise SyntaxError(f"unsupported statement: {s}")

    return out, k

# -------------------------
# type/rank inference
# -------------------------

@dataclass
class Sym:
    ftype: str   # real | integer | logical | complex
    rank: int    # 0,1,2
    alloc: bool = False

class Infer:
    def __init__(self):
        self.syms: Dict[str, Sym] = {}
        self.used_la = False
        # Octave/Matlab predefined constants we support.
        # Note: we treat these names as constants (not user variables) in this subset.
        self.const_types = {
            "pi": Sym("real", 0, alloc=False),
            "e": Sym("real", 0, alloc=False),
            "eps": Sym("real", 0, alloc=False),
            "realmax": Sym("real", 0, alloc=False),
            "realmin": Sym("real", 0, alloc=False),
            "intmax": Sym("integer", 0, alloc=False),
            "intmin": Sym("integer", 0, alloc=False),
            "inf": Sym("real", 0, alloc=False),
            "nan": Sym("real", 0, alloc=False),
        }

        self.builtin = {
            "linspace": ("funcs", [2]),
            "logspace": ("funcs", [2]),
            "zeros": ("funcs", "all"),
            "ones": ("funcs", "all"),
            "eye": ("funcs", "all"),
            "diag": ("funcs", []),
            "reshape": ("funcs", [1,2]),
            "reshape_oct": ("funcs", [1,2]),
            "sum": ("funcs", []),
            "sum_oct": ("funcs", []),
            "mean": ("funcs", []),
            "disp": ("io", []),
            "printf": ("io", []),

            "inv": ("la", []),
            "pinv": ("la", []),
            "det": ("la", []),
            "rank": ("la", []),
            "trace": ("la", []),
            "norm": ("la", []),
            "eig": ("la", []),
            "svd": ("la", []),
            "qr": ("la", []),
            "lu": ("la", []),
            "chol": ("la", []),
        }

    def ensure(self, name: str, ftype: str, rank: int, alloc: bool):
        cur = self.syms.get(name)
        if cur is None:
            self.syms[name] = Sym(ftype, rank, alloc)
            return

        if cur.ftype != ftype:
            # Precedence rules for mixed usage:
            # - complex dominates everything
            # - logical dominates numeric
            # - integer dominates real (dims/loop vars must be INTEGER in Fortran)
            #   and we can cast integer -> real(kind=dp) at use sites.
            types = {cur.ftype, ftype}
            if "complex" in types:
                cur.ftype = "complex"
            elif "logical" in types:
                cur.ftype = "logical"
            elif "integer" in types:
                cur.ftype = "integer"
            elif "real" in types:
                cur.ftype = "real"
            else:
                cur.ftype = ftype

        cur.rank = max(cur.rank, rank)
        cur.alloc = cur.alloc or alloc

    def mark_integer_expr(self, e: Expr):
        if isinstance(e, Name):
            self.ensure(e.id, "integer", 0, alloc=False)

    def expr_type(self, e: Expr) -> Sym:
        if isinstance(e, Num):
            return Sym("real", 0, alloc=False)
        if isinstance(e, Str):
            return Sym("char", 0, alloc=False)
        if isinstance(e, BoolLit):
            return Sym("logical", 0, alloc=False)
        if isinstance(e, Name):
            k = e.id.lower()
            if k in self.const_types:
                return self.const_types[k]
            return self.syms.get(e.id, Sym("real", 0, alloc=False))
        if isinstance(e, Unary):
            t = self.expr_type(e.x)
            if e.op == "~":
                return Sym("logical", t.rank, alloc=t.rank > 0)
            return t
        if isinstance(e, PostTranspose):
            t = self.expr_type(e.x)
            return Sym(t.ftype, t.rank, alloc=t.rank > 0)
        if isinstance(e, Range):
            self.mark_integer_expr(e.a)
            self.mark_integer_expr(e.b)
            if e.step is not None:
                self.mark_integer_expr(e.step)
            return Sym("real", 1, alloc=True)
        if isinstance(e, ArrayLit):
            r = len(e.rows)
            if r <= 1:
                return Sym("real", 1, alloc=True)
            return Sym("real", 2, alloc=True)
        if isinstance(e, Index):
            base = self.syms.get(e.base, Sym("real", max(0, len(e.subs)), alloc=True))
            return Sym(base.ftype, 0, alloc=False)
        if isinstance(e, Call):
            fn = e.fn
            cat, intpos = self.builtin.get(fn, ("user", []))
            if cat == "la":
                self.used_la = True
            if intpos == "all":
                for a in e.args:
                    self.mark_integer_expr(a)
            elif isinstance(intpos, list):
                for p in intpos:
                    if p < len(e.args):
                        self.mark_integer_expr(e.args[p])

            if fn in ("zeros", "ones", "eye", "inv", "pinv", "qr", "lu", "chol"):
                if fn in ("zeros", "ones", "eye") and len(e.args) >= 2:
                    return Sym("real", 2, alloc=True)
                if fn in ("zeros", "ones", "eye") and len(e.args) == 1:
                    # Octave/Matlab: zeros(n), ones(n), eye(n) => n-by-n
                    return Sym("real", 2, alloc=True)
                return Sym("real", 2, alloc=True)
            if fn in ("linspace", "logspace", "svd"):
                return Sym("real", 1, alloc=True)
            if fn in ("reshape", "reshape_oct"):
                # reshape_oct(x, n) -> 1d; reshape_oct(x, m, n) -> 2d
                if len(e.args) <= 2:
                    return Sym("real", 1, alloc=True)
                return Sym("real", 2, alloc=True)
            if fn == "diag" and len(e.args) == 1:
                a0 = self.expr_type(e.args[0])
                return Sym("real", 2 if a0.rank == 1 else 1, alloc=True)
            if fn in ("sum", "sum_oct"):
                a0 = self.expr_type(e.args[0]) if e.args else Sym("real", 0, False)
                if a0.rank == 2:
                    return Sym("real", 1, alloc=True)
                return Sym("real", 0 if a0.rank == 0 else 1, alloc=a0.rank > 0)
            if fn == "mean":
                a0 = self.expr_type(e.args[0]) if e.args else Sym("real", 0, False)
                if a0.rank == 2:
                    return Sym("real", 1, alloc=True)
                return Sym("real", 0, alloc=False)
            if fn in ("det", "trace", "norm"):
                return Sym("real", 0, alloc=False)
            if fn == "rank":
                return Sym("integer", 0, alloc=False)
            if fn == "eig":
                return Sym("complex", 1, alloc=True)
            return Sym("real", 0, alloc=False)
        if isinstance(e, Bin):
            ta = self.expr_type(e.a)
            tb = self.expr_type(e.b)
            if e.op in ("==", "~=", "<", "<=", ">", ">="):
                return Sym("logical", max(ta.rank, tb.rank), alloc=max(ta.rank, tb.rank) > 0)
            if e.op in ("&&", "||"):
                return Sym("logical", 0, alloc=False)
            ftype = "complex" if (ta.ftype == "complex" or tb.ftype == "complex") else "real"
            rank = max(ta.rank, tb.rank)
            return Sym(ftype, rank, alloc=rank > 0)
        return Sym("real", 0, alloc=False)

# -------------------------
# Fortran emission
# -------------------------

class Emitter:
    def __init__(self, infer: Infer):
        self.infer = infer
        self.lines: List[str] = []
        self.ind = 0
        self.fn_map = {
            "reshape": "reshape_oct",
            "sum": "sum_oct",
        }

    def emit(self, s: str):
        self.lines.append(("   " * self.ind) + s)

    def num_to_fortran(self, t: str) -> str:
        if "_" in t:
            return t
        if "." in t or "e" in t.lower():
            return f"{t}_dp"
        return t

    def _is_int_literal(self, t: str) -> bool:
        if "_" in t:
            return False
        tl = t.lower()
        if "." in tl or "e" in tl:
            return False
        return re.fullmatch(r"[+\-]?\d+", t) is not None

    def _coerce_real_dp(self, ex: Expr, sx: str) -> str:
        # Convert integer-looking literals/variables to real(kind=dp) where Octave semantics expect doubles.
        if isinstance(ex, Num) and self._is_int_literal(ex.text):
            return f"{ex.text}.0_dp"
        if isinstance(ex, Unary) and ex.op in ("+", "-") and isinstance(ex.x, Num) and self._is_int_literal(ex.x.text):
            sign = "-" if ex.op == "-" else ""
            return f"{sign}{ex.x.text}.0_dp"
        if isinstance(ex, Name):
            sym = self.infer.syms.get(ex.id)
            if sym is not None and sym.ftype == "integer":
                return f"real({ex.id}, kind=dp)"
        return sx

    def emit_real_dp_expr(self, e: Expr) -> str:
        sx, _ = self.emit_expr(e)
        return self._coerce_real_dp(e, sx)

    def emit_int_expr(self, e: Expr) -> str:
        # Ensure an INTEGER expression for Fortran DO bounds/steps.
        if isinstance(e, Num):
            txt = e.text.strip()
            if self._is_int_literal(txt):
                return txt
            sx, _ = self.emit_expr(e)
            return f"int({sx})"
        if isinstance(e, Unary) and e.op in ("+", "-") and isinstance(e.x, Num) and self._is_int_literal(e.x.text):
            sign = "-" if e.op == "-" else ""
            return f"{sign}{e.x.text}"
        if isinstance(e, Call):
            fn = e.fn.lower()
            if fn == "floor" and len(e.args) == 1:
                inner = self.emit_real_dp_expr(e.args[0])
                return f"int(floor({inner}))"
            if fn == "sqrt" and len(e.args) == 1:
                inner = self.emit_real_dp_expr(e.args[0])
                return f"int(sqrt({inner}))"
        sx, t = self.emit_expr(e)
        if t.ftype == "integer":
            return sx
        return f"int({sx})"

    def emit_expr(self, e: Expr) -> Tuple[str, Sym]:
        inf = self.infer
        t = inf.expr_type(e)

        if isinstance(e, Num):
            return self.num_to_fortran(e.text), t
        if isinstance(e, Str):
            return e.text, t
        if isinstance(e, BoolLit):
            return (".true." if e.val else ".false."), Sym("logical", 0, alloc=False)
        if isinstance(e, Name):
            k = e.id.lower()
            if k == "pi":
                return "acos(-1.0_dp)", Sym("real", 0, alloc=False)
            if k == "e":
                return "exp(1.0_dp)", Sym("real", 0, alloc=False)
            if k == "eps":
                return "epsilon(1.0_dp)", Sym("real", 0, alloc=False)
            if k == "realmax":
                return "huge(1.0_dp)", Sym("real", 0, alloc=False)
            if k == "realmin":
                return "tiny(1.0_dp)", Sym("real", 0, alloc=False)
            if k == "intmax":
                return "huge(0)", Sym("integer", 0, alloc=False)
            if k == "intmin":
                return "(-huge(0) - 1)", Sym("integer", 0, alloc=False)
            if k == "inf":
                return "huge(1.0_dp)", Sym("real", 0, alloc=False)
            if k == "nan":
                return "(0.0_dp/0.0_dp)", Sym("real", 0, alloc=False)
            return e.id, t
        if isinstance(e, Unary):
            x, tx = self.emit_expr(e.x)
            if e.op == "~":
                return f".not. ({x})", Sym("logical", tx.rank, alloc=tx.rank > 0)
            if e.op == "+":
                return f"+({x})", tx
            if e.op == "-":
                return f"-({x})", tx
            raise SyntaxError(f"unsupported unary {e.op}")
        if isinstance(e, PostTranspose):
            x, tx = self.emit_expr(e.x)
            return f"transpose({x})", tx
        if isinstance(e, Range):
            a, _ = self.emit_expr(e.a)
            b, _ = self.emit_expr(e.b)
            a = self._coerce_real_dp(e.a, a)
            b = self._coerce_real_dp(e.b, b)
            if e.step is None:
                return f"colon_dp({a}, {b})", t
            st, _ = self.emit_expr(e.step)
            st = self._coerce_real_dp(e.step, st)
            return f"colon_dp({a}, {st}, {b})", t
        if isinstance(e, ArrayLit):
            rows = e.rows
            r = len(rows)
            c = len(rows[0]) if r > 0 else 0
            if r <= 1:
                items = []
                for ex in (rows[0] if rows else []):
                    sx, _ = self.emit_expr(ex)
                    sx = self._coerce_real_dp(ex, sx)
                    items.append(sx)
                return f"[{', '.join(items)}]", Sym("real", 1, alloc=True)
            flat = []
            for j in range(c):
                for i in range(r):
                    ex = rows[i][j]
                    sx, _ = self.emit_expr(ex)
                    sx = self._coerce_real_dp(ex, sx)
                    flat.append(sx)
            return f"reshape([{', '.join(flat)}], [{r}, {c}])", Sym("real", 2, alloc=True)
        if isinstance(e, Index):
            # Octave-style indexing:
            # - For vectors: x(i)
            # - For matrices: x(i,j)
            # - Linear indexing into matrices: x(k) uses column-major order.
            sym = self.infer.syms.get(e.base)
            if sym is not None and sym.rank == 2 and len(e.subs) == 1:
                kexpr, _ = self.emit_expr(e.subs[0])
                nrow = f"size({e.base}, 1)"
                r = f"(1 + mod(({kexpr}) - 1, {nrow}))"
                c = f"(1 + (({kexpr}) - 1) / {nrow})"
                return f"{e.base}({r}, {c})", Sym(sym.ftype, 0, alloc=False)

            subs = []
            for ex in e.subs:
                sx, _ = self.emit_expr(ex)
                subs.append(sx)
            return f"{e.base}({', '.join(subs)})", t
        if isinstance(e, Call):
            fn = e.fn
            # unknown -> treat as indexing
            if fn not in self.infer.builtin and fn not in self.fn_map:
                # Octave allows linear indexing into matrices with a single subscript.
                # For rank-2 variables, map x(k) to x(r,c) in column-major order.
                sym = self.infer.syms.get(fn)
                if sym is not None and sym.rank == 2 and len(e.args) == 1:
                    kexpr, _ = self.emit_expr(e.args[0])
                    # r = 1 + mod(k-1, nrow), c = 1 + (k-1)/nrow
                    nrow = f"size({fn}, 1)"
                    r = f"(1 + mod(({kexpr}) - 1, {nrow}))"
                    c = f"(1 + (({kexpr}) - 1) / {nrow})"
                    return f"{fn}({r}, {c})", Sym(sym.ftype, 0, alloc=False)

                lfn = fn.lower()
                if lfn in ("sqrt", "floor") and len(e.args) == 1:
                    sx0, _ = self.emit_expr(e.args[0])
                    sx0 = self._coerce_real_dp(e.args[0], sx0)
                    if lfn == "sqrt":
                        return f"sqrt({sx0})", Sym("real", 0, alloc=False)
                    else:
                        return f"floor({sx0})", Sym("real", 0, alloc=False)

                subs = []
                for ex in e.args:
                    sx, _ = self.emit_expr(ex)
                    subs.append(sx)
                return f"{fn}({', '.join(subs)})", t

                        # minimal printf/disp handled at stmt level
            f = self.fn_map.get(fn, fn)

            # Octave/Matlab: zeros(n) and ones(n) produce n-by-n
            # Our funcs_mod provides zeros(m,n)/ones(m,n), so duplicate the dim.
            if fn in ("zeros", "ones") and len(e.args) == 1:
                a0, _ = self.emit_expr(e.args[0])
                return f"{f}({a0}, {a0})", Sym("real", 2, alloc=True)

            args = []
            for k, ex in enumerate(e.args):
                sx, _ = self.emit_expr(ex)
                # Octave uses doubles for numeric literals; coerce where the callee expects reals.
                if fn in ("linspace", "logspace") and k in (0, 1):
                    sx = self._coerce_real_dp(ex, sx)
                args.append(sx)
            return f"{f}({', '.join(args)})", t

        if isinstance(e, Bin):
            a, ta = self.emit_expr(e.a)
            b, tb = self.emit_expr(e.b)
            op = e.op

            if op == "&&":
                return f"(({a}) .and. ({b}))", Sym("logical", 0, alloc=False)
            if op == "||":
                return f"(({a}) .or. ({b}))", Sym("logical", 0, alloc=False)

            op2 = "/=" if op == "~=" else op
            if op2 in ("==", "/=", "<", "<=", ">", ">="):
                return f"({a} {op2} {b})", Sym("logical", max(ta.rank, tb.rank), alloc=max(ta.rank, tb.rank) > 0)

            if op in (".*", "./"):
                op3 = "*" if op == ".*" else "/"
                return f"({a} {op3} {b})", t
            if op in (".^", "^"):
                return f"({a} ** {b})", t

            if op in ("+", "-"):
                return f"({a} {op} {b})", t
            if op in ("*", "/"):
                if op == "/":
                    return f"({a} / {b})", t
                ra = ta.rank
                rb = tb.rank
                if ra == 0 or rb == 0:
                    return f"({a} * {b})", t
                if ra == 1 and rb == 1:
                    raise SyntaxError("vector*vector unsupported; use elementwise .* or implement dot()")
                return f"matmul({a}, {b})", t

            raise SyntaxError(f"unsupported binary op {op}")

        raise SyntaxError("unknown expression node")

    def emit_stmt(self, st: Stmt):
        inf = self.infer
        if isinstance(st, Assign):
            if isinstance(st.lhs, list):
                raise SyntaxError("multiple assignment not enabled in this minimal subset")
            rhs, tr = self.emit_expr(st.rhs)

            if isinstance(st.lhs, Name):
                lhs = st.lhs.id
                if tr.ftype == "char":
                    raise SyntaxError("string assignment not supported")
                inf.ensure(lhs, tr.ftype, tr.rank, alloc=(tr.rank > 0))
                self.emit(f"{lhs} = {rhs}")
                return

            if isinstance(st.lhs, Index):
                lhs, _ = self.emit_expr(st.lhs)
                inf.ensure(st.lhs.base, "real" if tr.ftype != "complex" else "complex", max(1, len(st.lhs.subs)), alloc=True)
                self.emit(f"{lhs} = {rhs}")
                return

            raise SyntaxError("bad assignment lhs")

        if isinstance(st, CallStmt):
            c = st.call
            fn = c.fn.lower()
            if fn == "disp":
                if len(c.args) != 1:
                    raise SyntaxError("disp(x) only")
                x, _ = self.emit_expr(c.args[0])
                self.emit(f"call disp({x})")
                return
            if fn == "printf":
                # Minimal support:
                # - printf("literal")
                # - printf("%d\n", x)  (integer)
                # - printf("%f\n", x)  (real)
                if len(c.args) == 1 and isinstance(c.args[0], Str):
                    s2 = c.args[0].text.replace("\\n", "")
                    self.emit(f'write(*,"(a)") {s2}')
                    return
                if len(c.args) == 2 and isinstance(c.args[0], Str):
                    fmt = c.args[0].text
                    x1, t1 = self.emit_expr(c.args[1])
                    if "%d" in fmt:
                        self.emit(f'write(*,"(i0)") {x1}')
                        return
                    if "%f" in fmt or "%g" in fmt:
                        xr = self.emit_real_dp_expr(c.args[1])
                        self.emit(f'write(*,"(g0)") {xr}')
                        return
                raise SyntaxError('printf supported only as printf("literal") or printf("%d", x)')
            raise SyntaxError(f"unsupported call statement: {c.fn}(...)")

        if isinstance(st, ForStmt):
            inf.ensure(st.var, "integer", 0, alloc=False)
            inf.mark_integer_expr(st.rng.a)
            inf.mark_integer_expr(st.rng.b)
            if st.rng.step is not None:
                inf.mark_integer_expr(st.rng.step)

            a = self.emit_int_expr(st.rng.a)
            b = self.emit_int_expr(st.rng.b)
            if st.rng.step is None:
                self.emit(f"do {st.var} = {a}, {b}")
            else:
                stp = self.emit_int_expr(st.rng.step)
                self.emit(f"do {st.var} = {a}, {b}, {stp}")
            self.ind += 1
            for s in st.body:
                self.emit_stmt(s)
            self.ind -= 1
            self.emit("end do")
            return

        if isinstance(st, IfStmt):
            c, _ = self.emit_expr(st.cond)
            self.emit(f"if ({c}) then")
            self.ind += 1
            for s in st.then_body:
                self.emit_stmt(s)
            self.ind -= 1
            for cond, body in st.elifs:
                cc, _ = self.emit_expr(cond)
                self.emit(f"else if ({cc}) then")
                self.ind += 1
                for s in body:
                    self.emit_stmt(s)
                self.ind -= 1
            if st.else_body:
                self.emit("else")
                self.ind += 1
                for s in st.else_body:
                    self.emit_stmt(s)
                self.ind -= 1
            self.emit("end if")
            return

        if isinstance(st, BreakStmt):
            self.emit("exit")
            return

        if isinstance(st, ContinueStmt):
            self.emit("cycle")
            return

        raise SyntaxError("unknown statement type")

def gen_runtime_module() -> str:
    return '''module oct_runtime_mod
use funcs_mod, only: dp
implicit none

interface disp
   module procedure disp_real0, disp_real1, disp_real2
   module procedure disp_int0
end interface

interface colon_dp
   module procedure colon_dp2, colon_dp3
end interface

contains

subroutine disp_real0(x)
real(kind=dp), intent(in) :: x
write(*,"(f0.6)") x
end subroutine disp_real0

subroutine disp_int0(i)
integer, intent(in) :: i
write(*,"(i0)") i
end subroutine disp_int0

subroutine disp_real1(x)
real(kind=dp), intent(in) :: x(:)
if (size(x) == 0) then
   write(*,"(a)") "(empty)"
else
   write(*,"(*(f0.6,1x))") x
end if
end subroutine disp_real1

subroutine disp_real2(a)
real(kind=dp), intent(in) :: a(:,:)
integer :: i
if (size(a,1) == 0 .or. size(a,2) == 0) then
   write(*,"(a)") "(empty)"
   return
end if
do i = 1, size(a,1)
   write(*,"(*(f0.6,1x))") a(i,:)
end do
end subroutine disp_real2

function colon_dp2(a, b) result(x)
real(kind=dp), intent(in) :: a, b
real(kind=dp), allocatable :: x(:)
integer :: n, i
real(kind=dp) :: step

if (b >= a) then
   n = int(b - a) + 1
   step = 1.0_dp
else
   n = int(a - b) + 1
   step = -1.0_dp
end if
allocate(x(n))
do i = 1, n
   x(i) = a + real(i-1,kind=dp)*step
end do
end function colon_dp2

function colon_dp3(a, s, b) result(x)
real(kind=dp), intent(in) :: a, s, b
real(kind=dp), allocatable :: x(:)
integer :: n, i
real(kind=dp) :: step

step = s
if (step == 0.0_dp) error stop "colon_dp: step cannot be 0"

n = int((b - a)/step) + 1
if (n < 0) n = 0
allocate(x(max(0,n)))
do i = 1, size(x)
   x(i) = a + real(i-1,kind=dp)*step
end do
end function colon_dp3

end module oct_runtime_mod
'''

def render_fortran(program_name: str, infer: Infer, body_lines: List[str]) -> str:
    uses = []
    uses.append("use funcs_mod")
    uses.append("use oct_runtime_mod, only: disp, colon_dp")
    if infer.used_la:
        uses.append('use linear_algebra_mod, only: inv, pinv, det, rank, trace, norm, eig, svd, qr, lu, chol')

    decl = []
    for name in sorted(infer.syms.keys()):
        s = infer.syms[name]
        if s.ftype == "char":
            continue
        if s.ftype == "integer":
            decl.append(f"integer :: {name}")
        elif s.ftype == "complex":
            if s.rank == 0:
                decl.append(f"complex(kind=dp) :: {name}")
            elif s.rank == 1:
                decl.append(f"complex(kind=dp), allocatable :: {name}(:)")
            else:
                decl.append(f"complex(kind=dp), allocatable :: {name}(:,:)")
        elif s.ftype == "logical":
            if s.rank == 0:
                decl.append(f"logical :: {name}")
            elif s.rank == 1:
                decl.append(f"logical, allocatable :: {name}(:)")
            else:
                decl.append(f"logical, allocatable :: {name}(:,:)")
        else:
            if s.rank == 0:
                decl.append(f"real(kind=dp) :: {name}")
            elif s.rank == 1:
                decl.append(f"real(kind=dp), allocatable :: {name}(:)")
            else:
                decl.append(f"real(kind=dp), allocatable :: {name}(:,:)")

    parts = []
    parts.append(gen_runtime_module())
    parts.append("")
    parts.append(f"program {program_name}")
    for u in uses:
        parts.append(u)
    parts.append("implicit none")
    parts.append("")
    if decl:
        parts.extend(decl)
        parts.append("")
    parts.extend(body_lines)
    parts.append("")
    parts.append(f"end program {program_name}")
    parts.append("")
    return "\n".join(parts)

def transpile(text: str, program_name: str) -> str:
    lines = []
    for line in text.splitlines():
        lines.append(strip_comments(line))
    cleaned = "\n".join(lines)
    stmts_txt = split_statements(cleaned)

    stmts, k = parse_stmt_list(stmts_txt, 0)
    if k != len(stmts_txt):
        raise SyntaxError("extra tokens after top-level parse")

    infer = Infer()
    em = Emitter(infer)

    for st in stmts:
        em.emit_stmt(st)

    return render_fortran(program_name, infer, em.lines)

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("input", help="Octave .m file")
    ap.add_argument("-o", "--output", help="Output Fortran .f90")
    ap.add_argument("--name", help="Fortran program name (default: file stem)")
    args = ap.parse_args()

    with open(args.input, "r", encoding="utf-8") as f:
        text = f.read()

    stem = os.path.splitext(os.path.basename(args.input))[0]
    pname = args.name or re.sub(r"[^A-Za-z0-9_]", "_", stem)
    f90 = transpile(text, pname)

    out = args.output or (stem + ".f90")
    with open(out, "w", encoding="utf-8") as f:
        f.write(f90)

    print(out)

if __name__ == "__main__":
    main()
