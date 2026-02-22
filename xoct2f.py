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
    (?P<op>\.\^|\.\*|\.\/|\.\'|>=|<=|==|~=|&&|\|\||[+\-*/^<>=(),;\[\]:'\\]|~)|
    (?P<name>[A-Za-z_]\w*)
    """,
    re.VERBOSE,
)

def tokenize(s: str) -> List[Tok]:
    out: List[Tok] = []
    i = 0
    while i < len(s):
        # fast whitespace skip
        if s[i].isspace():
            i += 1
            continue

        # manual string scan so backslashes inside strings never conflict with operators
        if s[i] == '"':
            j = i + 1
            while j < len(s):
                if s[j] == '\\':
                    j += 2
                    continue
                if s[j] == '"':
                    j += 1
                    break
                j += 1
            if j > len(s) or s[j-1] != '"':
                raise SyntaxError(f"unterminated string starting at {i}")
            out.append(Tok("string", s[i:j], i))
            i = j
            continue

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


def find_assignment_eq(s: str) -> Optional[int]:
    """Return index of assignment '=' not inside strings, excluding '==', '<=', '>=', '~='."""
    in_str = False
    i = 0
    while i < len(s):
        ch = s[i]
        if in_str:
            if ch == '\\':
                i += 2
                continue
            if ch == '"':
                in_str = False
            i += 1
            continue
        else:
            if ch == '"':
                in_str = True
                i += 1
                continue
            if ch == '=':
                prev = s[i-1] if i > 0 else ''
                nxt = s[i+1] if i+1 < len(s) else ''
                if prev in "<>~=:":
                    i += 1
                    continue
                if nxt == '=':
                    i += 1
                    continue
                return i
            i += 1
    return None

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

        while self.cur().kind == "op" and self.cur().val in ("'", ".'"):
            if self.cur().val == "'":
                self.eat("op", "'")
            else:
                self.eat("op", ".'")
            left = PostTranspose(left)

        while rbp < self.lbp(self.cur()):
            t = self.cur()
            self.i += 1
            left = self.led(t, left)

            while self.cur().kind == "op" and self.cur().val in ("'", ".'"):
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
        if op in ("*", "/", "\\", ".*", "./"):
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

        if op in ("+", "-", "*", "/", "\\", ".*", "./", "^", ".^",
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
    lhs: Any  # Name | Index
    rhs: Expr
    suppress: bool = True

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
        if depth_paren == 0 and depth_brack == 0:
            if ch == ";":
                buf.append(ch)
                flush()
                continue
            if ch == "\n":
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
        suppress = False
        if s.endswith(";"):
            suppress = True
            s = s[:-1].rstrip()
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
            out.append(Assign(lhs_list, rhs, suppress=suppress))
            k += 1
            continue

        eq = find_assignment_eq(s)
        if eq is not None:
            lhs_txt = s[:eq].strip()
            rhs_txt = s[eq+1:].strip()
            lhs_expr = parse_expr(lhs_txt)
            if isinstance(lhs_expr, Name):
                lhs = lhs_expr
            elif isinstance(lhs_expr, Call):
                lhs = Index(lhs_expr.fn, lhs_expr.args)
            else:
                raise SyntaxError("lhs must be a variable or indexed variable in this subset")
            rhs = parse_expr(rhs_txt)
            out.append(Assign(lhs, rhs, suppress=suppress))
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
        self.used_mldivide = False
        self.used_mrdivide = False
        self.used_mpower = False
        self.used_spower = False
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
            "rand": ("rt", "all"),
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
            if fn == "rand":
                # rand() -> scalar, rand(n) -> n-by-n, rand(m,n) -> m-by-n, rand(m,n,p) -> m-by-n-by-p
                if len(e.args) == 0:
                    return Sym("real", 0, alloc=False)
                if len(e.args) == 1:
                    self.mark_integer_expr(e.args[0])
                    return Sym("real", 2, alloc=True)
                if len(e.args) == 2:
                    self.mark_integer_expr(e.args[0]); self.mark_integer_expr(e.args[1])
                    return Sym("real", 2, alloc=True)
                if len(e.args) == 3:
                    self.mark_integer_expr(e.args[0]); self.mark_integer_expr(e.args[1]); self.mark_integer_expr(e.args[2])
                    return Sym("real", 3, alloc=True)
                error_stop = True
                return Sym("real", 0, alloc=False)
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

            if e.op == "\\":
                # Octave left-division: A \ b  (solve linear system / least squares)
                self.used_mldivide = True
                self.used_la = True
                if ta.rank == 2 and tb.rank == 1:
                    return Sym("real", 1, alloc=True)
                if ta.rank == 2 and tb.rank == 2:
                    return Sym("real", 2, alloc=True)
                return Sym("real", max(ta.rank, tb.rank), alloc=max(ta.rank, tb.rank) > 0)

            if e.op == "/":
                # Octave right-division: A / B is matrix division when both are matrices.
                if ta.rank == 2 and tb.rank == 2:
                    self.used_mrdivide = True
                    self.used_la = True
                    return Sym("real", 2, alloc=True)

            if e.op == "^":
                # Octave matrix power when left operand is a matrix and exponent is scalar
                if ta.rank == 2 and tb.rank == 0:
                    self.used_mpower = True
                    self.used_la = True
                    # exponent should be integer-valued
                    self.mark_integer_expr(e.b)
                    return Sym("real", 2, alloc=True)

            if e.op == "^":
                # Octave scalar power with matrix exponent: s ^ A = expm(log(s) * A)
                if ta.rank == 0 and tb.rank == 2:
                    self.used_spower = True
                    self.used_la = True
                    return Sym("real", 2, alloc=True)

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

            # Octave/Matlab: rand(n) produces n-by-n.
            # Provide rand(m,n) and rand(m,n,p) via oct_runtime_mod.
            if fn == "rand" and len(e.args) == 1:
                a0, _ = self.emit_expr(e.args[0])
                return f"rand({a0}, {a0})", Sym("real", 2, alloc=True)

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
            if op == ".^":
                return f"({a} ** {b})", t
            if op == "^":
                # Octave: matrix power when A is a matrix and exponent is scalar
                if ta.rank == 2 and tb.rank == 0:
                    b_int = b if tb.ftype == "integer" else f"int({b})"
                    return f"mpower({a}, {b_int})", t
                # Octave: scalar power with matrix exponent: s ^ A
                if ta.rank == 0 and tb.rank == 2:
                    a_real = f"real({a}, kind=dp)"
                    return f"spower({a_real}, {b})", t
                return f"({a} ** {b})", t

            if op in ("+", "-"):
                return f"({a} {op} {b})", t

            if op == "\\":
                return f"mldivide({a}, {b})", t
            if op in ("*", "/"):
                if op == "/":
                    if ta.rank == 2 and tb.rank == 2:
                        return f"mrdivide({a}, {b})", t
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
                if not st.suppress:
                    self.emit(f'call disp("{lhs} =")')
                    if tr.rank > 0:
                        self.emit('call disp("")')
                    self.emit(f"call disp({lhs})")
                    if tr.rank > 0:
                        self.emit('call disp("")')
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

def gen_runtime_module(infer: Infer) -> str:
    lines = []
    lines.append("module oct_runtime_mod")
    lines.append("use funcs_mod, only: dp")
    lines.append("use, intrinsic :: iso_fortran_env, only: int64")
    if infer.used_mldivide or infer.used_mrdivide or infer.used_mpower:
        lines.append("use linear_algebra_mod, only: inv, pinv")
    lines.append("implicit none")
    lines.append("")
    lines.append("logical :: disp_int_like_default = .true.")
    lines.append("real(kind=dp) :: disp_int_like_tol = 1000.0_dp*epsilon(1.0_dp)")
    lines.append("")
    lines.append("interface disp")
    lines.append("   module procedure disp_real0, disp_real1, disp_real2, disp_real3")
    lines.append("   module procedure disp_int0")
    lines.append("   module procedure disp_char0")
    lines.append("end interface")
    lines.append("")
    lines.append("interface colon_dp")
    lines.append("   module procedure colon_dp2, colon_dp3")
    lines.append("end interface")
    lines.append("")
    lines.append("interface rand")
    lines.append("   module procedure rand_2d, rand_3d")
    lines.append("end interface")
    if infer.used_mldivide:
        lines.append("")
        lines.append("interface mldivide")
        lines.append("   module procedure mldivide_mat_vec, mldivide_mat_mat")
        lines.append("end interface")
    if infer.used_mrdivide:
        lines.append("")
        lines.append("interface mrdivide")
        lines.append("   module procedure mrdivide_mat_mat")
        lines.append("end interface")
    if infer.used_mpower:
        lines.append("")
        lines.append("interface mpower")
        lines.append("   module procedure mpower_mat_int")
        lines.append("end interface")
    if infer.used_spower:
        lines.append("")
        lines.append("interface spower")
        lines.append("   module procedure spower_scalar_mat")
        lines.append("end interface")
    lines.append("")
    lines.append("contains")
    lines.append("")
    lines.append("subroutine set_disp_int_like(flag)")
    lines.append("logical, intent(in) :: flag")
    lines.append("disp_int_like_default = flag")
    lines.append("end subroutine set_disp_int_like")
    lines.append("")
    lines.append("subroutine set_disp_int_like_tol(tol)")
    lines.append("real(kind=dp), intent(in) :: tol")
    lines.append("disp_int_like_tol = max(0.0_dp, tol)")
    lines.append("end subroutine set_disp_int_like_tol")
    lines.append("")
    lines.append("logical function isfinite_dp(x) result(ok)")
    lines.append("real(kind=dp), intent(in) :: x")
    lines.append("ok = (x == x) .and. (abs(x) <= huge(x))")
    lines.append("end function isfinite_dp")
    lines.append("")
    lines.append("subroutine disp_real0(x)")
    lines.append("real(kind=dp), intent(in) :: x")
    lines.append('write(*,"(f0.6)") x')
    lines.append("end subroutine disp_real0")
    lines.append("")
    lines.append("subroutine disp_int0(i)")
    lines.append("integer, intent(in) :: i")
    lines.append('write(*,"(i0)") i')
    lines.append("end subroutine disp_int0")
    lines.append("")
    lines.append("subroutine disp_char0(s)")
    lines.append("character(len=*), intent(in) :: s")
    lines.append('write(*,"(a)") s')
    lines.append("end subroutine disp_char0")
    lines.append("")
    lines.append("subroutine disp_real1(x, int_like)")
    lines.append("real(kind=dp), intent(in) :: x(:)")
    lines.append("logical, intent(in), optional :: int_like")
    lines.append("logical :: use_int_like, all_int")
    lines.append("integer :: i")
    lines.append("integer(kind=int64) :: k")
    lines.append("real(kind=dp) :: r, tol")
    lines.append("use_int_like = disp_int_like_default")
    lines.append("if (present(int_like)) use_int_like = int_like")
    lines.append("if (size(x) == 0) then")
    lines.append('   write(*,"(a)") "(empty)"')
    lines.append("   return")
    lines.append("end if")
    lines.append("all_int = .false.")
    lines.append("if (use_int_like) then")
    lines.append("   all_int = .true.")
    lines.append("   do i = 1, size(x)")
    lines.append("      r = x(i)")
    lines.append("      if (.not. isfinite_dp(r)) then")
    lines.append("         all_int = .false.; exit")
    lines.append("      end if")
    lines.append("      if (abs(r) > real(huge(0_int64), kind=dp)) then")
    lines.append("         all_int = .false.; exit")
    lines.append("      end if")
    lines.append("      k = nint(r, kind=int64)")
    lines.append("      tol = disp_int_like_tol * max(1.0_dp, abs(r))")
    lines.append("      if (abs(r - real(k, kind=dp)) > tol) then")
    lines.append("         all_int = .false.; exit")
    lines.append("      end if")
    lines.append("   end do")
    lines.append("end if")
    lines.append("if (all_int) then")
    lines.append("   do i = 1, size(x)")
    lines.append("      k = nint(x(i), kind=int64)")
    lines.append('      write(*,"(i0)", advance="no") k')
    lines.append('      if (i < size(x)) write(*,"(a)", advance="no") " "')
    lines.append("   end do")
    lines.append("   write(*,*)")
    lines.append("else")
    lines.append('   write(*,"(*(f0.6,1x))") x')
    lines.append("end if")
    lines.append("end subroutine disp_real1")
    lines.append("")
    lines.append("subroutine disp_real2(a, int_like)")
    lines.append("real(kind=dp), intent(in) :: a(:,:)")
    lines.append("logical, intent(in), optional :: int_like")
    lines.append("logical :: use_int_like, all_int")
    lines.append("integer :: i, j")
    lines.append("integer(kind=int64) :: k")
    lines.append("real(kind=dp) :: r, tol")
    lines.append("use_int_like = disp_int_like_default")
    lines.append("if (present(int_like)) use_int_like = int_like")
    lines.append("if (size(a,1) == 0 .or. size(a,2) == 0) then")
    lines.append('   write(*,"(a)") "(empty)"')
    lines.append("   return")
    lines.append("end if")
    lines.append("all_int = .false.")
    lines.append("if (use_int_like) then")
    lines.append("   all_int = .true.")
    lines.append("   do i = 1, size(a,1)")
    lines.append("      do j = 1, size(a,2)")
    lines.append("         r = a(i,j)")
    lines.append("         if (.not. isfinite_dp(r)) then")
    lines.append("            all_int = .false.; exit")
    lines.append("         end if")
    lines.append("         if (abs(r) > real(huge(0_int64), kind=dp)) then")
    lines.append("            all_int = .false.; exit")
    lines.append("         end if")
    lines.append("         k = nint(r, kind=int64)")
    lines.append("         tol = disp_int_like_tol * max(1.0_dp, abs(r))")
    lines.append("         if (abs(r - real(k, kind=dp)) > tol) then")
    lines.append("            all_int = .false.; exit")
    lines.append("         end if")
    lines.append("      end do")
    lines.append("      if (.not. all_int) exit")
    lines.append("   end do")
    lines.append("end if")
    lines.append("if (all_int) then")
    lines.append("   do i = 1, size(a,1)")
    lines.append("      do j = 1, size(a,2)")
    lines.append("         k = nint(a(i,j), kind=int64)")
    lines.append('         write(*,"(i0)", advance="no") k')
    lines.append('         if (j < size(a,2)) write(*,"(a)", advance="no") " "')
    lines.append("      end do")
    lines.append("      write(*,*)")
    lines.append("   end do")
    lines.append("else")
    lines.append("   do i = 1, size(a,1)")
    lines.append('      write(*,"(*(f0.6,1x))") a(i,:)')
    lines.append("   end do")
    lines.append("end if")
    lines.append("end subroutine disp_real2")
    lines.append("")
    lines.append("subroutine disp_real3(a, int_like)")
    lines.append("real(kind=dp), intent(in) :: a(:,:,:)")
    lines.append("logical, intent(in), optional :: int_like")
    lines.append("logical :: use_int_like, all_int")
    lines.append("integer :: i, j, k3")
    lines.append("integer(kind=int64) :: k")
    lines.append("real(kind=dp) :: r, tol")
    lines.append("use_int_like = disp_int_like_default")
    lines.append("if (present(int_like)) use_int_like = int_like")
    lines.append("if (size(a,1) == 0 .or. size(a,2) == 0 .or. size(a,3) == 0) then")
    lines.append('   write(*,"(a)") "(empty)"')
    lines.append("   return")
    lines.append("end if")
    lines.append("all_int = .false.")
    lines.append("if (use_int_like) then")
    lines.append("   all_int = .true.")
    lines.append("   do k3 = 1, size(a,3)")
    lines.append("      do i = 1, size(a,1)")
    lines.append("         do j = 1, size(a,2)")
    lines.append("            r = a(i,j,k3)")
    lines.append("            if (.not. isfinite_dp(r)) then")
    lines.append("               all_int = .false.; exit")
    lines.append("            end if")
    lines.append("            if (abs(r) > real(huge(0_int64), kind=dp)) then")
    lines.append("               all_int = .false.; exit")
    lines.append("            end if")
    lines.append("            k = nint(r, kind=int64)")
    lines.append("            tol = disp_int_like_tol * max(1.0_dp, abs(r))")
    lines.append("            if (abs(r - real(k, kind=dp)) > tol) then")
    lines.append("               all_int = .false.; exit")
    lines.append("            end if")
    lines.append("         end do")
    lines.append("         if (.not. all_int) exit")
    lines.append("      end do")
    lines.append("      if (.not. all_int) exit")
    lines.append("   end do")
    lines.append("end if")
    lines.append("do k3 = 1, size(a,3)")
    lines.append('   write(*,"(a,i0,a)") "(:,:,", k3, "):"')
    lines.append("   if (all_int) then")
    lines.append("      do i = 1, size(a,1)")
    lines.append("         do j = 1, size(a,2)")
    lines.append("            k = nint(a(i,j,k3), kind=int64)")
    lines.append('            write(*,"(i0)", advance="no") k')
    lines.append('            if (j < size(a,2)) write(*,"(a)", advance="no") " "')
    lines.append("         end do")
    lines.append("         write(*,*)")
    lines.append("      end do")
    lines.append("   else")
    lines.append("      do i = 1, size(a,1)")
    lines.append('         write(*,"(*(f0.6,1x))") a(i,:,k3)')
    lines.append("      end do")
    lines.append("   end if")
    lines.append("end do")
    lines.append("end subroutine disp_real3")
    lines.append("")
    lines.append("function colon_dp2(a, b) result(x)")
    lines.append("real(kind=dp), intent(in) :: a, b")
    lines.append("real(kind=dp), allocatable :: x(:)")
    lines.append("integer :: n, i")
    lines.append("if (b < a) then")
    lines.append("   n = 0")
    lines.append("else")
    lines.append("   n = int(floor(b - a)) + 1")
    lines.append("end if")
    lines.append("allocate(x(n))")
    lines.append("if (n > 0) x = [(a + real(i-1,kind=dp), i=1,n)]")
    lines.append("end function colon_dp2")
    lines.append("")
    lines.append("function colon_dp3(a, step, b) result(x)")
    lines.append("real(kind=dp), intent(in) :: a, step, b")
    lines.append("real(kind=dp), allocatable :: x(:)")
    lines.append("integer :: n, i")
    lines.append("if (step == 0.0_dp) then")
    lines.append('   error stop "colon_dp: step must be nonzero"')
    lines.append("end if")
    lines.append("if ((step > 0.0_dp .and. b < a) .or. (step < 0.0_dp .and. b > a)) then")
    lines.append("   n = 0")
    lines.append("else")
    lines.append("   n = int(floor((b - a)/step)) + 1")
    lines.append("end if")
    lines.append("allocate(x(n))")
    lines.append("if (n > 0) x = [(a + real(i-1,kind=dp)*step, i=1,n)]")
    lines.append("end function colon_dp3")
    lines.append("")
    lines.append("function rand_2d(m, n) result(a)")
    lines.append("integer, intent(in) :: m, n")
    lines.append("real(kind=dp), allocatable :: a(:,:)")
    lines.append('if (m < 0 .or. n < 0) error stop "rand: dims must be >= 0"')
    lines.append("allocate(a(m, n))")
    lines.append("call random_number(a)")
    lines.append("end function rand_2d")
    lines.append("")
    lines.append("function rand_3d(m, n, p) result(a)")
    lines.append("integer, intent(in) :: m, n, p")
    lines.append("real(kind=dp), allocatable :: a(:,:,:)")
    lines.append('if (m < 0 .or. n < 0 .or. p < 0) error stop "rand: dims must be >= 0"')
    lines.append("allocate(a(m, n, p))")
    lines.append("call random_number(a)")
    lines.append("end function rand_3d")
    if infer.used_mldivide:
        lines.append("")
        lines.append("function mldivide_mat_vec(a, b) result(x)")
        lines.append("real(kind=dp), intent(in) :: a(:,:), b(:)")
        lines.append("real(kind=dp), allocatable :: x(:)")
        lines.append("if (size(a,1) == size(a,2)) then")
        lines.append("   x = matmul(inv(a), b)")
        lines.append("else")
        lines.append("   x = matmul(pinv(a), b)")
        lines.append("end if")
        lines.append("end function mldivide_mat_vec")
        lines.append("")
        lines.append("function mldivide_mat_mat(a, b) result(x)")
        lines.append("real(kind=dp), intent(in) :: a(:,:), b(:,:)")
        lines.append("real(kind=dp), allocatable :: x(:,:)")
        lines.append("if (size(a,1) == size(a,2)) then")
        lines.append("   x = matmul(inv(a), b)")
        lines.append("else")
        lines.append("   x = matmul(pinv(a), b)")
        lines.append("end if")
        lines.append("end function mldivide_mat_mat")
    if infer.used_mrdivide:
        lines.append("")
        lines.append("function mrdivide_mat_mat(a, b) result(x)")
        lines.append("real(kind=dp), intent(in) :: a(:,:), b(:,:)")
        lines.append("real(kind=dp), allocatable :: x(:,:)")
        lines.append("if (size(b,1) == size(b,2)) then")
        lines.append("   x = matmul(a, inv(b))")
        lines.append("else")
        lines.append("   x = matmul(a, pinv(b))")
        lines.append("end if")
        lines.append("end function mrdivide_mat_mat")

    if infer.used_mpower:
        lines.append("")
        lines.append("function mpower_mat_int(a, n) result(b)")
        lines.append("real(kind=dp), intent(in) :: a(:,:)")
        lines.append("integer, intent(in) :: n")
        lines.append("real(kind=dp), allocatable :: b(:,:)")
        lines.append("real(kind=dp), allocatable :: base(:,:)")
        lines.append("integer :: k, m")
        lines.append("m = size(a,1)")
        lines.append("if (size(a,2) /= m) error stop \"mpower: matrix must be square\"")
        lines.append("if (n == 0) then")
        lines.append("   allocate(b(m,m))")
        lines.append("   b = 0.0_dp")
        lines.append("   do k = 1, m")
        lines.append("      b(k,k) = 1.0_dp")
        lines.append("   end do")
        lines.append("   return")
        lines.append("end if")
        lines.append("if (n > 0) then")
        lines.append("   base = a")
        lines.append("else")
        lines.append("   base = inv(a)")
        lines.append("end if")
        lines.append("allocate(b(m,m))")
        lines.append("b = base")
        lines.append("do k = 2, abs(n)")
        lines.append("   b = matmul(b, base)")
        lines.append("end do")
        lines.append("end function mpower_mat_int")

    if infer.used_spower:
        lines.append("")
        lines.append("function spower_scalar_mat(s, a) result(b)")
        lines.append("real(kind=dp), intent(in) :: s")
        lines.append("real(kind=dp), intent(in) :: a(:,:)")
        lines.append("real(kind=dp), allocatable :: b(:,:)")
        lines.append("real(kind=dp), allocatable :: term(:,:), x(:,:), i2(:,:)")
        lines.append("real(kind=dp) :: tol, scale")
        lines.append("integer :: k, m, km")
        lines.append("m = size(a,1)")
        lines.append("if (size(a,2) /= m) error stop \"spower: matrix must be square\"")
        lines.append("if (s <= 0.0_dp) error stop \"spower: base must be > 0 for real result\"")
        lines.append("x = log(s) * a")
        lines.append("allocate(i2(m,m))")
        lines.append("i2 = 0.0_dp")
        lines.append("do k = 1, m")
        lines.append("   i2(k,k) = 1.0_dp")
        lines.append("end do")
        lines.append("allocate(b(m,m))")
        lines.append("b = i2")
        lines.append("term = i2")
        lines.append("tol = 1.0e-12_dp")
        lines.append("km = 200")
        lines.append("do k = 1, km")
        lines.append("   term = matmul(term, x) / real(k, kind=dp)")
        lines.append("   b = b + term")
        lines.append("   if (maxval(abs(term)) < tol) exit")
        lines.append("end do")
        lines.append("end function spower_scalar_mat")

    lines.append("")
    lines.append("end module oct_runtime_mod")
    return "\n".join(lines)

def render_fortran(program_name: str, infer: Infer, body_lines: List[str], force_real_disp: bool = False) -> str:
    uses = []
    uses.append("use funcs_mod")
    uses.append("use oct_runtime_mod, only: disp, colon_dp, rand, set_disp_int_like, set_disp_int_like_tol" + (", mldivide" if infer.used_mldivide else "") + (", mrdivide" if infer.used_mrdivide else "") + (", mpower" if infer.used_mpower else "") + (", spower" if infer.used_spower else ""))
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
            elif s.rank == 2:
                decl.append(f"complex(kind=dp), allocatable :: {name}(:,:)")
            else:
                decl.append(f"complex(kind=dp), allocatable :: {name}(:,:,:)")
        elif s.ftype == "logical":
            if s.rank == 0:
                decl.append(f"logical :: {name}")
            elif s.rank == 1:
                decl.append(f"logical, allocatable :: {name}(:)")
            elif s.rank == 2:
                decl.append(f"logical, allocatable :: {name}(:,:)")
            else:
                decl.append(f"logical, allocatable :: {name}(:,:,:)")
        else:
            if s.rank == 0:
                decl.append(f"real(kind=dp) :: {name}")
            elif s.rank == 1:
                decl.append(f"real(kind=dp), allocatable :: {name}(:)")
            elif s.rank == 2:
                decl.append(f"real(kind=dp), allocatable :: {name}(:,:)")
            else:
                decl.append(f"real(kind=dp), allocatable :: {name}(:,:,:)")

    parts = []
    parts.append(gen_runtime_module(infer))
    parts.append("")
    parts.append(f"program {program_name}")
    for u in uses:
        parts.append(u)
    parts.append("implicit none")
    parts.append("")
    if decl:
        parts.extend(decl)
        parts.append("")
    if force_real_disp:
        parts.append("call set_disp_int_like(.false.)")
        parts.append("")
    parts.extend(body_lines)
    parts.append("")
    parts.append(f"end program {program_name}")
    parts.append("")
    return "\n".join(parts)

def transpile(text: str, program_name: str, force_real_disp: bool = False) -> str:
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

    return render_fortran(program_name, infer, em.lines, force_real_disp=force_real_disp)

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("input", help="Octave .m file")
    ap.add_argument("-o", "--output", help="Output Fortran .f90")
    ap.add_argument("--name", help="Fortran program name (default: file stem)")
    ap.add_argument("--disp-real", action="store_true", help="Always print real arrays as real (disable integer-like printing)")
    args = ap.parse_args()

    with open(args.input, "r", encoding="utf-8") as f:
        text = f.read()

    stem = os.path.splitext(os.path.basename(args.input))[0]
    pname = args.name or re.sub(r"[^A-Za-z0-9_]", "_", stem)
    f90 = transpile(text, pname, force_real_disp=args.disp_real)

    out = args.output or (stem + ".f90")
    with open(out, "w", encoding="utf-8") as f:
        f.write(f90)

    print(out)

if __name__ == "__main__":
    main()
