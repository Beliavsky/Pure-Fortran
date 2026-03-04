# xp2f.py (a.k.a. p2f.py)
# partial python -> fortran transpiler (level 1 subset; primes sieve family)
#
# default: structured output (main_mod + compute/run split)
# --flat : inline everything in a single main program
#
# runtime:
# - reads python.f90 (module python_mod) and adds missing helpers if needed
# - helpers are registered using a parseable inline comment: "!@pyapi ..."
#
# usage:
#   python xp2f.py input.py
#   python xp2f.py --flat input.py
#   python xp2f.py input.py path\to\python.f90

import ast
import sys
from pathlib import Path
import re
import argparse
import shlex
import subprocess
import time
import difflib
import io
import tokenize
from datetime import datetime
from fortran_scan import (
    coalesce_simple_declarations,
    remove_empty_if_blocks,
    simplify_redundant_parens_in_lines,
    simplify_integer_arithmetic_in_lines,
    strip_redundant_outer_parens_expr,
    wrap_long_declaration_lines,
)


def run_capture(cmd, tee=False):
    """
    Run command and capture output.
    When tee=True, stream combined stdout/stderr live while also capturing it.
    """
    if not tee:
        cp = subprocess.run(cmd, capture_output=True, text=True)
        return cp.returncode, (cp.stdout or ""), (cp.stderr or ""), False

    p = subprocess.Popen(
        cmd,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
        bufsize=1,
    )
    out_lines = []
    assert p.stdout is not None
    for ln in p.stdout:
        out_lines.append(ln)
        print(ln, end="")
    rc = p.wait()
    return rc, "".join(out_lines), "", True


def debug_flags_for_compiler(compiler_exe):
    low = (compiler_exe or "").lower()
    if "gfortran" in low:
        return ["-g", "-fcheck=all", "-fbacktrace", "-ffpe-trap=invalid,zero,overflow"]
    if low in {"ifx", "ifort"} or low.endswith("\\ifx.exe") or low.endswith("\\ifort.exe"):
        return ["-g", "-traceback", "-check", "all", "-fpe0"]
    # Conservative fallback for unknown Fortran compilers.
    return ["-g"]


def fstr(s):
    # use double quotes when possible; fallback to single quotes; escape embedded quotes by doubling
    # if the source string contains newlines, emit a Fortran concatenation with new_line('a')
    def _quote_one(part):
        if '"' not in part:
            return '"' + part + '"'
        if "'" not in part:
            return "'" + part + "'"
        return '"' + part.replace('"', '""') + '"'

    s = s.replace("\r", "")
    if "\n" in s:
        return " // new_line('a') // ".join(_quote_one(p) for p in s.split("\n"))
    return _quote_one(s)


def extract_target_names(t):
    out = []
    if isinstance(t, ast.Name):
        out.append(t.id)
    elif isinstance(t, (ast.Tuple, ast.List)):
        for e in t.elts:
            out.extend(extract_target_names(e))
    return out


def count_assignments(tree):
    # conservative assignment counter (module-level only for parameters)
    counts = {}

    def bump(name):
        counts[name] = counts.get(name, 0) + 1

    class v(ast.NodeVisitor):
        def visit_Assign(self, node):
            for t in node.targets:
                for name in extract_target_names(t):
                    bump(name)
            self.generic_visit(node.value)

        def visit_AnnAssign(self, node):
            for name in extract_target_names(node.target):
                bump(name)
            if node.value:
                self.generic_visit(node.value)

        def visit_AugAssign(self, node):
            for name in extract_target_names(node.target):
                bump(name)
            self.generic_visit(node.value)

        def visit_For(self, node):
            for name in extract_target_names(node.target):
                bump(name)
            self.generic_visit(node.iter)
            for s in node.body:
                self.visit(s)
            for s in node.orelse:
                self.visit(s)

        def visit_FunctionDef(self, node):
            return

        def visit_ClassDef(self, node):
            return

    v().visit(tree)
    return counts


def is_const_int(node):
    return isinstance(node, ast.Constant) and isinstance(node.value, int)


def is_const_str(node):
    return isinstance(node, ast.Constant) and isinstance(node.value, str)


def is_bool_const(node):
    return isinstance(node, ast.Constant) and isinstance(node.value, bool)


def is_none(node):
    if isinstance(node, ast.Constant) and node.value is None:
        return True
    # Treat np.newaxis as None in subscript contexts.
    return (
        isinstance(node, ast.Attribute)
        and isinstance(node.value, ast.Name)
        and node.value.id == "np"
        and node.attr == "newaxis"
    )


def const_int_expr_to_fortran(node, allowed_names=None):
    """
    Return Fortran integer-constant expression text for a restricted Python AST,
    or None if expression is not a safe integer constant expression.
    """
    if allowed_names is None:
        allowed_names = set()

    if isinstance(node, ast.Constant) and isinstance(node.value, int) and not isinstance(node.value, bool):
        return str(node.value)

    if isinstance(node, ast.Name):
        return node.id if node.id in allowed_names else None

    if isinstance(node, ast.UnaryOp):
        inner = const_int_expr_to_fortran(node.operand, allowed_names)
        if inner is None:
            return None
        if isinstance(node.op, ast.UAdd):
            return f"(+{inner})"
        if isinstance(node.op, ast.USub):
            return f"(-{inner})"
        return None

    if isinstance(node, ast.BinOp):
        left = const_int_expr_to_fortran(node.left, allowed_names)
        right = const_int_expr_to_fortran(node.right, allowed_names)
        if left is None or right is None:
            return None
        if isinstance(node.op, ast.Add):
            return f"({left} + {right})"
        if isinstance(node.op, ast.Sub):
            return f"({left} - {right})"
        if isinstance(node.op, ast.Mult):
            return f"({left} * {right})"
        if isinstance(node.op, ast.Pow):
            return f"({left} ** {right})"
        if isinstance(node.op, (ast.Div, ast.FloorDiv)):
            return f"({left} / {right})"
        if isinstance(node.op, ast.Mod):
            return f"mod({left}, {right})"
        return None

    return None


def find_parameters(tree):
    # module-level integer constant expression assigned once => integer, parameter
    counts = count_assignments(tree)
    params = {}
    for node in tree.body:
        if not (
            isinstance(node, ast.Assign)
            and len(node.targets) == 1
            and isinstance(node.targets[0], ast.Name)
        ):
            continue
        k = node.targets[0].id
        if counts.get(k, 0) != 1:
            continue
        expr_txt = const_int_expr_to_fortran(node.value, allowed_names=set(params.keys()))
        if expr_txt is not None:
            params[k] = strip_redundant_outer_parens_expr(expr_txt)
    return params


def const_comment(name, tree):
    return "constant from python source"


def extract_python_comments(src_text):
    """Map source line -> list of Python comment texts (without '#')."""
    out = {}
    try:
        toks = tokenize.generate_tokens(io.StringIO(src_text).readline)
    except Exception:
        return out
    for tok_type, tok_str, start, _end, _line in toks:
        if tok_type == tokenize.COMMENT:
            ln = start[0]
            txt = tok_str[1:].strip()
            out.setdefault(ln, []).append(txt)
    return out


def _comment_map_for_top_level(tree, comment_map):
    """Keep only comments that are not inside nested def/class bodies."""
    if not comment_map:
        return {}
    blocked = set()
    for n in getattr(tree, "body", []):
        if isinstance(n, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
            start = getattr(n, "lineno", None)
            end = getattr(n, "end_lineno", None)
            if isinstance(start, int) and isinstance(end, int) and end >= start:
                blocked.update(range(start, end + 1))
    filtered = {ln: vals for ln, vals in comment_map.items() if ln not in blocked}
    body_lines = [getattr(n, "lineno", None) for n in getattr(tree, "body", [])]
    body_lines = [ln for ln in body_lines if isinstance(ln, int)]
    if not body_lines:
        return filtered
    lo = min(body_lines)
    hi = max(
        [
            getattr(n, "end_lineno", getattr(n, "lineno", lo))
            for n in getattr(tree, "body", [])
            if isinstance(getattr(n, "lineno", None), int)
        ]
    )
    return {ln: vals for ln, vals in filtered.items() if lo <= ln <= hi}


def procedure_comment(proc_name, kind):
    words = proc_name.replace("_", " ")
    if proc_name.startswith("run_"):
        return f"run {words[4:]}"
    if proc_name.startswith("compute_"):
        return f"compute {words[8:]}"
    if kind == "function":
        return f"compute {words}"
    return f"perform {words}"


def argument_comment(name, intent):
    low = name.lower()
    if low in {"n", "m", "k", "limit", "count", "cnt"}:
        base = "size/count parameter"
    elif low.startswith("n_"):
        base = "derived size/count parameter"
    elif low in {"x", "y", "z", "a", "b", "c", "p"}:
        base = "data argument"
    elif "list" in low or "arr" in low or "vec" in low:
        base = "array argument"
    else:
        base = "argument"
    return f"{base} ({intent})"


def emit_python_docstring_as_fortran_comments(o, fn_node):
    """Emit Python docstring text as Fortran comment lines."""
    doc = ast.get_docstring(fn_node, clean=False)
    if not doc:
        return
    for ln in doc.splitlines():
        txt = ln.rstrip()
        o.w(f"! {txt}" if txt else "!")


def function_is_pure(fn_node, known_pure_calls=None):
    """Conservative purity check for generated local functions."""
    known_pure = {s.lower() for s in (known_pure_calls or set())}
    impure_calls = {
        "print",
        "input",
        "open",
        "close",
        "read",
        "write",
        "random_number",
        "random_seed",
    }
    whitelist_calls = {
        "isqrt",
        "int",
        "float",
        "len",
        "abs",
        "min",
        "max",
        "range",
    }

    class scan(ast.NodeVisitor):
        def __init__(self):
            self.ok = True

        def visit_Call(self, node):
            if not self.ok:
                return
            if isinstance(node.func, ast.Name):
                fname = node.func.id
                if fname.lower() in known_pure:
                    self.generic_visit(node)
                    return
                if fname in impure_calls:
                    self.ok = False
                    return
                if fname not in whitelist_calls:
                    # Unknown direct calls may be impure.
                    self.ok = False
                    return
            elif isinstance(node.func, ast.Attribute):
                # Allow selected numpy intrinsics lowered without side effects.
                if not (
                    isinstance(node.func.value, ast.Name)
                    and node.func.value.id == "np"
                    and node.func.attr in {"sqrt", "empty"}
                ):
                    self.ok = False
                    return
            else:
                self.ok = False
                return
            self.generic_visit(node)

    s = scan()
    for st in fn_node.body:
        s.visit(st)
        if not s.ok:
            break
    return s.ok


def remove_redundant_tail_returns(src_text):
    """Remove RETURN lines that are immediately before END FUNCTION/SUBROUTINE/PROGRAM."""
    lines = src_text.splitlines()
    out = []
    n = len(lines)
    for i, ln in enumerate(lines):
        if ln.strip().lower() == "return":
            j = i + 1
            while j < n and lines[j].strip() == "":
                j += 1
            if j < n and re.match(r"^\s*end\s+(function|subroutine|program)\b", lines[j], flags=re.IGNORECASE):
                continue
        out.append(ln)
    return "\n".join(out) + ("\n" if src_text.endswith("\n") else "")


def simplify_size_dim_for_rank1_arrays(src_text):
    """Rewrite size(x,1|2) to size(x) when x is declared rank-1."""
    rank1 = set()
    decl_re = re.compile(r"::\s*(.*)$")
    for ln in src_text.splitlines():
        m = decl_re.search(ln)
        if not m:
            continue
        rhs = m.group(1)
        for part in rhs.split(","):
            p = part.strip()
            mm = re.match(r"([A-Za-z_]\w*)\s*\(\s*:\s*\)$", p)
            if mm:
                rank1.add(mm.group(1))
    out = src_text
    for nm in sorted(rank1, key=len, reverse=True):
        out = re.sub(rf"\bsize\(\s*{re.escape(nm)}\s*,\s*[12]\s*\)", f"size({nm})", out)
    return out


def rename_conflicting_identifiers(src_text):
    """
    Rename declared variables that collide with Fortran intrinsics/keywords.
    Rule: append trailing underscore, and update references.
    Intrinsic/procedure calls like `count(...)` are preserved.
    """
    forbidden = {
        # keywords / control
        "if", "then", "else", "do", "end", "program", "module", "subroutine", "function",
        "contains", "use", "implicit", "none", "integer", "real", "logical", "complex",
        "parameter", "allocatable", "intent", "in", "out", "inout", "call", "return",
        "stop", "select", "case", "where", "forall", "block", "interface", "type",
        "public", "private", "only",
        # common intrinsics that often collide with variable names
        "abs", "acos", "asin", "atan", "atan2", "ceiling", "cos", "count", "dot_product",
        "exp", "floor", "huge", "int", "kind", "len", "log", "log10", "max", "maxval",
        "mean", "merge", "min", "minval", "mod", "modulo", "nint", "pack", "present",
        "product", "real", "reshape", "sign", "sin", "size", "spread", "sqrt", "sum",
        "tiny", "transpose", "trim", "ubound", "lbound",
        # RNG names
        "random_number", "random_seed",
    }

    decl_re = re.compile(
        r"^\s*(?:integer|real|logical|complex|character|type\s*\([^)]+\))\b[^!]*::\s*([^!]*)(.*)$",
        flags=re.IGNORECASE,
    )
    name_tok_re = re.compile(r"\b([A-Za-z_]\w*)\b")
    rename_map = {}
    lines = src_text.splitlines()
    declared = set()

    # Collect declared identifiers first.
    for ln in lines:
        m = decl_re.match(ln)
        if not m:
            continue
        rhs = m.group(1)
        for part in rhs.split(","):
            p = part.strip()
            if not p:
                continue
            mm = re.match(r"^([A-Za-z_]\w*)", p)
            if mm:
                declared.add(mm.group(1))

    used_new = set()

    def _fresh_name(old):
        # Fortran identifiers must start with a letter.
        if old and old[0].isalpha():
            cand = old + "_"
        else:
            cand = "v" + old
        # Keep alnum/underscore and ensure first char is a letter.
        cand = re.sub(r"[^A-Za-z0-9_]", "_", cand)
        if not cand or not cand[0].isalpha():
            cand = "v_" + cand
        base = cand
        k = 2
        low_forbidden = {x.lower() for x in forbidden}
        while (
            cand in declared
            or cand in used_new
            or cand.lower() in low_forbidden
        ):
            cand = f"{base}_{k}"
            k += 1
        used_new.add(cand)
        return cand

    # Discover declared names that must be renamed.
    for ln in lines:
        m = decl_re.match(ln)
        if not m:
            continue
        rhs = m.group(1)
        for part in rhs.split(","):
            p = part.strip()
            if not p:
                continue
            mm = re.match(r"^([A-Za-z_]\w*)", p)
            if not mm:
                continue
            nm = mm.group(1)
            if nm.lower() in forbidden or not nm[0].isalpha():
                rename_map[nm] = _fresh_name(nm)

    if not rename_map:
        return src_text

    # Rewrite all non-call identifier references; preserve intrinsic/procedure calls.
    out_lines = []
    for ln in lines:
        code, bang, comment = ln.partition("!")

        def _repl(m):
            nm = m.group(1)
            new = rename_map.get(nm)
            if not new:
                return nm
            # Keep function/intrinsic call forms untouched.
            tail = code[m.end() :]
            if re.match(r"^\s*\(", tail):
                return nm
            return new

        code2 = name_tok_re.sub(_repl, code)
        if bang:
            out_lines.append(code2 + bang + comment)
        else:
            out_lines.append(code2)
    return "\n".join(out_lines) + ("\n" if src_text.endswith("\n") else "")


def simplify_generated_parentheses(lines):
    """Targeted paren cleanup that preserves indentation/layout."""
    out = []
    do_re = re.compile(
        r"^(\s*do\s+[a-z_]\w*\s*=\s*)([^,]+)(\s*,\s*[^,]+)?(\s*,\s*[^,]+)?(\s*)$",
        flags=re.IGNORECASE,
    )
    for ln in lines:
        code, bang, comment = ln.partition("!")

        m_if = re.match(r"^(\s*if\s*)\(\((.+)\)\)(\s*then\s*)$", code, flags=re.IGNORECASE)
        if m_if:
            code = f"{m_if.group(1)}({m_if.group(2)}){m_if.group(3)}"

        m_do = do_re.match(code)
        if m_do:
            head = m_do.group(1)
            lb = strip_redundant_outer_parens_expr(m_do.group(2).strip())
            ub = strip_redundant_outer_parens_expr((m_do.group(3) or "").replace(",", "", 1).strip())
            st = strip_redundant_outer_parens_expr((m_do.group(4) or "").replace(",", "", 1).strip()) if m_do.group(4) else ""
            tail_ws = m_do.group(5) or ""
            if st:
                code = f"{head}{lb}, {ub}, {st}{tail_ws}"
            elif ub:
                code = f"{head}{lb}, {ub}{tail_ws}"
            else:
                code = f"{head}{lb}{tail_ws}"

        # Collapse one redundant nested paren layer in subscripts/conditionals:
        # x((i+1)) -> x(i+1)
        code = re.sub(r"\(\(([^()]+)\)\)", r"(\1)", code)

        # Keyword args like dim=(1) -> dim=1
        code = re.sub(r"\b(dim|axis|ncopies)\s*=\s*\(\s*([^()]+?)\s*\)", r"\1=\2", code, flags=re.IGNORECASE)

        # Assignment RHS outer-paren cleanup:
        #   x = (expr) -> x = expr
        #   x = ((expr)) -> x = expr
        # Skip declarations, pointers, and control-flow statements.
        if "=" in code and "::" not in code and "=>" not in code and not re.match(r"^\s*(if|do|where|forall|select)\b", code, flags=re.IGNORECASE):
            m_asn = re.match(r"^(\s*[^=]+?=\s*)(.+)$", code)
            if m_asn:
                lhs, rhs = m_asn.group(1), m_asn.group(2).rstrip()
                prev = rhs
                while True:
                    cur = strip_redundant_outer_parens_expr(prev)
                    if cur == prev:
                        break
                    prev = cur
                code = lhs + prev

        if bang:
            out.append(code + bang + comment)
        else:
            out.append(code)
    return out


def inline_shape_comments(lines):
    """
    Move standalone shape comments like '! (n,k)' onto the next executable line.
    """
    out = list(lines)
    i = 0
    pat_shape = re.compile(r"^\s*!\s*(\([^()]+\))\s*$")
    while i < len(out):
        m = pat_shape.match(out[i])
        if not m:
            i += 1
            continue
        shape_txt = m.group(1).strip()
        j = i + 1
        while j < len(out):
            s = out[j].strip()
            if not s:
                j += 1
                continue
            if s.startswith("!"):
                j += 1
                continue
            break
        if j >= len(out):
            i += 1
            continue
        code, bang, cmt = out[j].partition("!")
        code = code.rstrip()
        if bang:
            cmt_txt = cmt.strip()
            if cmt_txt:
                out[j] = f"{code} ! {cmt_txt}; {shape_txt}"
            else:
                out[j] = f"{code} ! {shape_txt}"
        else:
            out[j] = f"{code} ! {shape_txt}"
        out.pop(i)
    return out


def collapse_alloc_dealloc_before_assignment(lines):
    """
    Collapse conservative allocatable init triplets:
      if (allocated(x)) deallocate(x)
      allocate(x(...))
      x = <expr>
    into:
      x = <expr>
    """
    out = []
    i = 0
    re_dealloc = re.compile(
        r"^\s*if\s*\(\s*allocated\s*\(\s*([a-z_]\w*)\s*\)\s*\)\s*deallocate\s*\(\s*\1\s*\)\s*$",
        re.IGNORECASE,
    )
    re_alloc = re.compile(r"^\s*allocate\s*\(\s*([a-z_]\w*)\s*\(.+\)\s*\)\s*$", re.IGNORECASE)
    re_assign = re.compile(r"^\s*([a-z_]\w*)\s*=\s*(.+)$", re.IGNORECASE)
    while i < len(lines):
        if i + 2 < len(lines):
            c0 = lines[i].split("!", 1)[0].strip()
            c1 = lines[i + 1].split("!", 1)[0].strip()
            c2 = lines[i + 2].split("!", 1)[0].strip()
            m0 = re_dealloc.match(c0)
            m1 = re_alloc.match(c1)
            m2 = re_assign.match(c2)
            if m0 and m1 and m2:
                n0 = m0.group(1).lower()
                n1 = m1.group(1).lower()
                n2 = m2.group(1).lower()
                rhs = m2.group(2).strip()
                # Only whole-variable assignment (no section/ref on lhs).
                # Collapse only when RHS is an explicit array constructor, which
                # safely triggers allocation-on-assignment for allocatables.
                # Do not collapse scalar RHS (e.g., var = var0) because scalar-to-
                # array assignment requires prior allocation.
                if (
                    n0 == n1 == n2
                    and not rhs.lower().startswith("allocate(")
                    and ("[" in rhs and "]" in rhs)
                ):
                    out.append(lines[i + 2])
                    i += 3
                    continue
        out.append(lines[i])
        i += 1
    return out


def collapse_allocate_before_array_constructor_assignment(lines):
    """
    Collapse:
      allocate(x(...))
      x = [ ... ]
    into:
      x = [ ... ]
    This is safe for allocatables due to allocation-on-assignment.
    """
    out = []
    i = 0
    re_alloc = re.compile(r"^\s*allocate\s*\(\s*([a-z_]\w*)\s*\(.+\)\s*\)\s*$", re.IGNORECASE)
    re_assign = re.compile(r"^\s*([a-z_]\w*)\s*=\s*(.+)$", re.IGNORECASE)
    while i < len(lines):
        if i + 1 < len(lines):
            c0 = lines[i].split("!", 1)[0].strip()
            c1 = lines[i + 1].split("!", 1)[0].strip()
            m0 = re_alloc.match(c0)
            m1 = re_assign.match(c1)
            if m0 and m1:
                n0 = m0.group(1).lower()
                n1 = m1.group(1).lower()
                rhs = m1.group(2).strip()
                if n0 == n1 and ("[" in rhs and "]" in rhs):
                    out.append(lines[i + 1])
                    i += 2
                    continue
        out.append(lines[i])
        i += 1
    return out


def remove_redundant_first_guarded_deallocate(lines):
    """
    Remove:
      if (allocated(x)) deallocate(x)
    when:
    - the next nonblank/non-comment line is allocate(x(...))
    - x has not appeared in prior executable statements.
    """
    out = list(lines)
    re_guard = re.compile(
        r"^\s*if\s*\(\s*allocated\s*\(\s*([a-z_]\w*)\s*\)\s*\)\s*deallocate\s*\(\s*\1\s*\)\s*$",
        re.IGNORECASE,
    )
    re_alloc = re.compile(r"^\s*allocate\s*\(\s*([a-z_]\w*)\s*\(.+\)\s*\)\s*$", re.IGNORECASE)
    re_declish = re.compile(
        r"^\s*(?:use\b|implicit\b|contains\b|integer\b|real\b|logical\b|character\b|complex\b|type\b|class\b|procedure\b|interface\b|module\b|program\b|subroutine\b|function\b|end\b)",
        re.IGNORECASE,
    )

    i = 0
    while i < len(out):
        code_i = out[i].split("!", 1)[0].strip()
        m = re_guard.match(code_i)
        if not m:
            i += 1
            continue
        var = m.group(1)

        j = i + 1
        while j < len(out):
            cj = out[j].split("!", 1)[0].strip()
            if not cj:
                j += 1
                continue
            break
        if j >= len(out):
            i += 1
            continue
        m_alloc = re_alloc.match(out[j].split("!", 1)[0].strip())
        if not (m_alloc and m_alloc.group(1).lower() == var.lower()):
            i += 1
            continue

        prior_use = False
        tok_re = re.compile(rf"\b{re.escape(var)}\b", re.IGNORECASE)
        re_flow = re.compile(r"^\s*(?:do\b|if\b|select\b|where\b|forall\b|block\b)", re.IGNORECASE)
        for k in range(i):
            ck = out[k].split("!", 1)[0].strip()
            if not ck:
                continue
            # Conservative: once control-flow has started, keep guarded deallocate.
            if re_flow.match(ck):
                prior_use = True
                break
            if re_declish.match(ck):
                continue
            if tok_re.search(ck):
                prior_use = True
                break
        if not prior_use:
            del out[i]
            # do not advance i; next line shifts into current slot
            continue
        i += 1
    return out


def top_level_if(tree):
    for node in tree.body:
        if isinstance(node, ast.If) and not is_main_guard_if(node):
            return node
    return None


def is_main_guard_if(node):
    if not isinstance(node, ast.If):
        return False
    t = node.test
    if not (isinstance(t, ast.Compare) and len(t.ops) == 1 and isinstance(t.ops[0], ast.Eq) and len(t.comparators) == 1):
        return False
    a = t.left
    b = t.comparators[0]
    if isinstance(a, ast.Name) and a.id == "__name__" and is_const_str(b) and b.value == "__main__":
        return True
    if isinstance(b, ast.Name) and b.id == "__name__" and is_const_str(a) and a.value == "__main__":
        return True
    return False


def detect_needed_helpers(tree):
    needed = set()
    np_helper_map = {
        "arange": {"arange_int"},
        "linspace": {"arange_int"},
        "cumsum": {"cumsum"},
        "cumprod": {"cumprod"},
        "gradient": {"gradient_1d"},
        "eye": {"eye"},
        "diag": {"diag"},
        "repeat": {"repeat"},
        "tile": {"tile"},
        "unique": {"unique"},
        "bincount": {"bincount_int"},
        "searchsorted": {"searchsorted_left_int", "searchsorted_right_int", "searchsorted_left_int_scalar", "searchsorted_right_int_scalar"},
        "histogram": {"histogram"},
        "setdiff1d": {"setdiff1d_int"},
        "intersect1d": {"intersect1d_int"},
        "lexsort": {"lexsort2_int"},
        "ravel_multi_index": {"ravel_multi_index_2d"},
        "unravel_index": {"unravel_index_2d"},
        "kron": {"kron_2d"},
        "mean": {"mean"},
        "var": {"var"},
        "std": {"std"},
        "nansum": {"nansum"},
        "nanmean": {"nanmean"},
        "nanvar": {"nanvar"},
        "nanstd": {"nanstd"},
        "nanmin": {"nanmin"},
        "nanmax": {"nanmax"},
        "nanargmin": {"nanargmin"},
        "nanargmax": {"nanargmax"},
        "log2": {"log2"},
        "zeros": {"zeros_int", "zeros_real", "zeros_logical"},
        "ones": {"ones_int", "ones_real", "ones_logical"},
        "zeros_like": {"zeros_int", "zeros_real", "zeros_logical"},
        "ones_like": {"ones_int", "ones_real", "ones_logical"},
        "full": {"arange_int"},
        "nonzero": {"arange_int"},
        "argwhere": {"arange_int"},
    }
    np_reduceat_helper_map = {
        "add": {"reduceat_add"},
        "multiply": {"reduceat_mul"},
        "minimum": {"reduceat_min"},
        "maximum": {"reduceat_max"},
        "logical_and": {"reduceat_logical_and"},
        "logical_or": {"reduceat_logical_or"},
    }

    class scan(ast.NodeVisitor):
        def visit_Call(self, node):
            if isinstance(node.func, ast.Name) and node.func.id == "isqrt":
                needed.add("isqrt_int")
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Attribute)
                and isinstance(node.func.value.value, ast.Name)
                and node.func.value.value.id == "np"
                and node.func.value.attr == "random"
                and node.func.attr == "normal"
            ):
                needed.add("random_normal_vec")
            if isinstance(node.func, ast.Attribute) and node.func.attr == "normal":
                needed.add("random_normal_vec")
            if isinstance(node.func, ast.Attribute) and node.func.attr == "choice":
                needed.add("random_choice_prob")
                replace_false = False
                p_kw = False
                two_cat = False
                if node.args and isinstance(node.args[0], ast.Constant) and isinstance(node.args[0].value, int) and node.args[0].value == 2:
                    two_cat = True
                for kw in node.keywords:
                    if kw.arg == "replace" and isinstance(kw.value, ast.Constant) and kw.value.value is False:
                        replace_false = True
                    if kw.arg == "p":
                        p_kw = True
                if p_kw and two_cat:
                    needed.add("random_choice2")
                if replace_false and not p_kw:
                    needed.add("random_choice_norep")
            if isinstance(node.func, ast.Attribute) and node.func.attr == "permutation":
                needed.add("random_choice_norep")
            if isinstance(node.func, ast.Attribute) and node.func.attr == "mean":
                needed.add("mean_1d")
            if isinstance(node.func, ast.Attribute) and node.func.attr == "var":
                needed.add("var_1d")
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "sort"
            ):
                needed.add("sort_vec")
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "argsort"
            ):
                needed.add("argsort")
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in np_helper_map
            ):
                needed.update(np_helper_map[node.func.attr])
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "unique"
            ):
                inv = any(kw.arg == "return_inverse" and isinstance(kw.value, ast.Constant) and kw.value.value is True for kw in node.keywords)
                cnt = any(kw.arg == "return_counts" and isinstance(kw.value, ast.Constant) and kw.value.value is True for kw in node.keywords)
                if inv and cnt:
                    needed.add("unique_int_inv_counts")
            if (
                isinstance(node.func, ast.Attribute)
                and node.func.attr == "reduceat"
                and isinstance(node.func.value, ast.Attribute)
                and isinstance(node.func.value.value, ast.Name)
                and node.func.value.value.id == "np"
                and node.func.value.attr in np_reduceat_helper_map
            ):
                needed.update(np_reduceat_helper_map[node.func.value.attr])
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Attribute)
                and isinstance(node.func.value.value, ast.Name)
                and node.func.value.value.id == "np"
                and node.func.value.attr == "linalg"
            ):
                if node.func.attr == "solve":
                    needed.add("linalg_solve")
                elif node.func.attr == "det":
                    needed.add("linalg_det")
                elif node.func.attr == "inv":
                    needed.add("linalg_inv")
                elif node.func.attr == "eig":
                    needed.add("linalg_eig")
                elif node.func.attr == "svd":
                    needed.add("linalg_svd")
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "trace"
            ):
                needed.add("diag")
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "einsum"
                and len(node.args) >= 1
                and is_const_str(node.args[0])
                and node.args[0].value == "ii->"
            ):
                needed.add("diag")
            if isinstance(node.func, ast.Name) and node.func.id == "print":
                if len(node.args) == 1 and isinstance(node.args[0], ast.Name) and node.args[0].id == "primes":
                    needed.add("print_int_list")
            self.generic_visit(node)

    scan().visit(tree)
    return needed


def build_list_count_map(tree):
    # recognize python list used like:
    #   primes = []
    #   primes.append(i)
    # and map to (allocatable integer array, count variable)
    list_vars = set()
    printed_lists = set()
    appended_lists = set()

    class scan(ast.NodeVisitor):
        def visit_Assign(self, node):
            if len(node.targets) == 1 and isinstance(node.targets[0], ast.Name):
                if isinstance(node.value, ast.List) and len(node.value.elts) == 0:
                    list_vars.add(node.targets[0].id)
            self.generic_visit(node)

        def visit_Expr(self, node):
            if isinstance(node.value, ast.Call) and isinstance(node.value.func, ast.Attribute):
                if node.value.func.attr == "append" and isinstance(node.value.func.value, ast.Name):
                    appended_lists.add(node.value.func.value.id)
            self.generic_visit(node)

        def visit_Call(self, node):
            if isinstance(node.func, ast.Name) and node.func.id == "print":
                if len(node.args) == 1 and isinstance(node.args[0], ast.Name):
                    printed_lists.add(node.args[0].id)
            self.generic_visit(node)

    scan().visit(tree)

    out = {}
    for name in sorted(list_vars):
        if name == "primes":
            out[name] = "npr"
        else:
            out[name] = f"n_{name}"

    # keep lists that are appended to or printed
    keep = {name for name in out if name in appended_lists or name in printed_lists}
    return {k: out[k] for k in keep}


def detect_scalar_outputs(tree, params):
    # scalars appearing in f-strings in prints in the else-branch
    outs = set()
    node_if = top_level_if(tree)
    nodes = node_if.orelse if node_if is not None else tree.body

    class scan(ast.NodeVisitor):
        def visit_Call(self, node):
            if isinstance(node.func, ast.Name) and node.func.id == "print":
                if len(node.args) == 1 and isinstance(node.args[0], ast.JoinedStr):
                    for part in node.args[0].values:
                        if isinstance(part, ast.FormattedValue) and isinstance(part.value, ast.Name):
                            nm = part.value.id
                            if nm not in params:
                                outs.add(nm)
            self.generic_visit(node)

    for s in nodes:
        scan().visit(s)

    outs.discard("n")
    return sorted(outs)


# -------------------------
# fortran runtime management
# -------------------------

def discover_runtime_exports(runtime_text):
    module_name = None
    exports = set()
    has_proc = set()

    for line in runtime_text.splitlines():
        m = re.match(r"^\s*module\s+([a-zA-Z_]\w*)\b", line, flags=re.IGNORECASE)
        if m and module_name is None:
            module_name = m.group(1)

        m = re.match(r"^\s*public\s*::\s*([a-zA-Z_]\w*)\b", line, flags=re.IGNORECASE)
        if m:
            exports.add(m.group(1).lower())

        m = re.match(
            r"^\s*(?:[a-zA-Z_]\w*\s+)*?(function|subroutine)\s+([a-zA-Z_]\w*)\s*\(",
            line,
            flags=re.IGNORECASE,
        )
        if m:
            has_proc.add(m.group(2).lower())
        m = re.match(r"^\s*interface\s+([a-zA-Z_]\w*)\b", line, flags=re.IGNORECASE)
        if m:
            has_proc.add(m.group(1).lower())

    if module_name is None:
        module_name = "python_mod"

    return module_name, exports, has_proc


def discover_pure_procedures(runtime_text):
    pure = set()
    for line in runtime_text.splitlines():
        m = re.match(
            r"^\s*pure\s+(?:[a-zA-Z_]\w*\s+)*?(function|subroutine)\s+([a-zA-Z_]\w*)\s*\(",
            line,
            flags=re.IGNORECASE,
        )
        if m:
            pure.add(m.group(2).lower())
    return pure


def _find_insertion_points(lines):
    idx_contains = None
    idx_end_module = None
    for i, line in enumerate(lines):
        if idx_contains is None and re.match(r"^\s*contains\b", line, flags=re.IGNORECASE):
            idx_contains = i
        if re.match(r"^\s*end\s+module\b", line, flags=re.IGNORECASE):
            idx_end_module = i
    if idx_contains is None:
        raise RuntimeError("could not find CONTAINS in runtime module")
    if idx_end_module is None:
        raise RuntimeError("could not find END MODULE in runtime module")
    return idx_contains, idx_end_module


def _insert_public(lines, idx_contains, public_lines):
    insert_at = idx_contains
    for i in range(idx_contains - 1, -1, -1):
        if re.match(r"^\s*public\s*::", lines[i], flags=re.IGNORECASE):
            insert_at = i + 1
            break

    to_add = list(public_lines)
    if to_add:
        if insert_at > 0 and lines[insert_at - 1].strip() != "":
            to_add.insert(0, "")
        if insert_at < len(lines) and lines[insert_at].strip() != "":
            to_add.append("")
    return lines[:insert_at] + to_add + lines[insert_at:]


def _insert_procs(lines, idx_end_module, proc_blocks):
    to_add = []
    for blk in proc_blocks:
        if to_add and to_add[-1].strip() != "":
            to_add.append("")
        to_add.extend(blk.splitlines())
        if to_add and to_add[-1].strip() != "":
            to_add.append("")

    if to_add:
        if idx_end_module > 0 and lines[idx_end_module - 1].strip() != "":
            to_add.insert(0, "")
    return lines[:idx_end_module] + to_add + lines[idx_end_module:]


def runtime_helper_templates():
    isqrt_pub = (
        "public :: isqrt_int       !@pyapi kind=function ret=integer "
        "args=x:integer:intent(in) desc=\"integer square root: return floor(sqrt(x)) for x >= 0\""
    )

    isqrt_blk = """      integer function isqrt_int(x)
         ! integer square root: return floor(sqrt(x)) for x >= 0
         integer, intent(in) :: x  ! input integer (x >= 0 expected)
         integer :: r
         if (x <= 0) then
            isqrt_int = 0
            return
         end if
         r = int(sqrt(real(x)))
         do while ((r+1)*(r+1) <= x)
            r = r + 1
         end do
         do while (r*r > x)
            r = r - 1
         end do
         isqrt_int = r
      end function isqrt_int"""

    pil_pub = (
        "public :: print_int_list  !@pyapi kind=subroutine "
        "args=a:integer(:):intent(in),n:integer:intent(in) "
        "desc=\"print integer list a(1:n) in python-style [..] format\""
    )

    pil_blk = """      subroutine print_int_list(a, n)
         ! print integer list a(1:n) in python-style [..] format
         integer, intent(in) :: a(:)  ! array containing values to print
         integer, intent(in) :: n     ! number of elements from a to print
         integer :: j
         if (n <= 0) then
            write(*,"(a)") "[]"
            return
         end if
         write(*,"(a)", advance="no") "["
         do j = 1, n
            if (j > 1) write(*,"(a)", advance="no") ", "
            write(*,"(i0)", advance="no") a(j)
         end do
         write(*,"(a)") "]"
      end subroutine print_int_list"""

    ru_pub = (
        "public :: random_uniform  !@pyapi kind=function ret=real "
        "args= desc=\"uniform random variate in [0,1)\""
    )
    ru_blk = """      pure real function random_uniform()
         real :: r
         call random_number(r)
         random_uniform = r
      end function random_uniform"""

    rnv_pub = (
        "public :: random_normal_vec !@pyapi kind=subroutine "
        "args=x:real(dp)(:):intent(out) desc=\"fill x with N(0,1) variates using Box-Muller\""
    )
    rnv_blk = """      subroutine random_normal_vec(x)
         real(kind=dp), intent(out) :: x(:)
         integer :: i, n
         real(kind=dp) :: u1, u2, rad, theta
         real(kind=dp), parameter :: two_pi = 2.0_dp * acos(-1.0_dp)
         n = size(x)
         i = 1
         do while (i <= n)
            call random_number(u1)
            call random_number(u2)
            if (u1 <= tiny(1.0_dp)) cycle
            rad = sqrt(-2.0_dp * log(u1))
            theta = two_pi * u2
            x(i) = rad * cos(theta)
            if (i + 1 <= n) x(i + 1) = rad * sin(theta)
            i = i + 2
         end do
      end subroutine random_normal_vec"""

    rc2_pub = (
        "public :: random_choice2 !@pyapi kind=subroutine "
        "args=p:real(dp)(:):intent(in),n:integer:intent(in),z:integer(:):intent(out) "
        "desc=\"sample n labels in {0,1} with probabilities p(1:2)\""
    )
    rc2_blk = """      subroutine random_choice2(p, n, z)
         real(kind=dp), intent(in) :: p(:)
         integer, intent(in) :: n
         integer, intent(out) :: z(:)
         integer :: i
         real(kind=dp) :: u, p1, s
         if (size(z) < n) stop "random_choice2: output array too small"
         if (size(p) < 2) stop "random_choice2: p must have at least 2 elements"
         p1 = max(0.0_dp, p(1))
         s = max(0.0_dp, p(1)) + max(0.0_dp, p(2))
         if (s > tiny(1.0_dp)) then
            p1 = p1 / s
         else
            p1 = 0.5_dp
         end if
         do i = 1, n
            call random_number(u)
            if (u < p1) then
               z(i) = 0
            else
               z(i) = 1
            end if
         end do
      end subroutine random_choice2"""

    rcnr_pub = (
        "public :: random_choice_norep !@pyapi kind=subroutine "
        "args=npop:integer:intent(in),nsamp:integer:intent(in),z:integer(:):intent(out) "
        "desc=\"sample nsamp unique labels from 0..npop-1 without replacement\""
    )
    rcnr_blk = """      subroutine random_choice_norep(npop, nsamp, z)
         integer, intent(in) :: npop, nsamp
         integer, intent(out) :: z(:)
         integer :: i, j, tmp
         real(kind=dp) :: u
         integer, allocatable :: pool(:)
         if (npop <= 0 .or. nsamp < 0) stop "random_choice_norep: invalid sizes"
         if (nsamp > npop) stop "random_choice_norep: nsamp > npop"
         if (size(z) < nsamp) stop "random_choice_norep: output array too small"
         allocate(pool(1:npop))
         do i = 1, npop
            pool(i) = i - 1
         end do
         do i = 1, nsamp
            call random_number(u)
            j = i + int(u * real(npop - i + 1, kind=dp))
            if (j < i) j = i
            if (j > npop) j = npop
            tmp = pool(i)
            pool(i) = pool(j)
            pool(j) = tmp
            z(i) = pool(i)
         end do
         if (allocated(pool)) deallocate(pool)
      end subroutine random_choice_norep"""

    rcp_pub = (
        "public :: random_choice_prob !@pyapi kind=subroutine "
        "args=p:real(dp)(:):intent(in),n:integer:intent(in),z:integer(:):intent(out) "
        "desc=\"sample n labels in 0..size(p)-1 with probabilities p\""
    )
    rcp_blk = """      subroutine random_choice_prob(p, n, z)
         real(kind=dp), intent(in) :: p(:)
         integer, intent(in) :: n
         integer, intent(out) :: z(:)
         integer :: i, j, k
         real(kind=dp) :: u, s
         real(kind=dp), allocatable :: cdf(:)
         k = size(p)
         if (k <= 0) stop "random_choice_prob: empty probability vector"
         if (size(z) < n) stop "random_choice_prob: output array too small"
         allocate(cdf(1:k))
         s = 0.0_dp
         do j = 1, k
            s = s + max(0.0_dp, p(j))
            cdf(j) = s
         end do
         if (s <= tiny(1.0_dp)) then
            do j = 1, k
               cdf(j) = real(j, kind=dp) / real(k, kind=dp)
            end do
         else
            cdf = cdf / s
         end if
         do i = 1, n
            call random_number(u)
            z(i) = k - 1
            do j = 1, k
               if (u <= cdf(j)) then
                  z(i) = j - 1
                  exit
               end if
            end do
         end do
         if (allocated(cdf)) deallocate(cdf)
      end subroutine random_choice_prob"""

    srt_pub = (
        "public :: sort_real_vec !@pyapi kind=subroutine "
        "args=x:real(dp)(:):intent(inout) desc=\"sort real vector x in ascending order\""
    )
    srt_blk = """      subroutine sort_real_vec(x)
         real(kind=dp), intent(inout) :: x(:)
         integer :: i, j, n
         real(kind=dp) :: key
         n = size(x)
         do i = 2, n
            key = x(i)
            j = i - 1
            do while (j >= 1)
               if (x(j) <= key) exit
               x(j+1) = x(j)
               j = j - 1
            end do
            x(j+1) = key
         end do
      end subroutine sort_real_vec"""

    srt_i_pub = (
        "public :: sort_int_vec !@pyapi kind=subroutine "
        "args=x:integer(:):intent(inout) desc=\"sort integer vector x in ascending order\""
    )
    srt_i_blk = """      subroutine sort_int_vec(x)
         integer, intent(inout) :: x(:)
         integer :: i, j, n, key
         n = size(x)
         do i = 2, n
            key = x(i)
            j = i - 1
            do while (j >= 1)
               if (x(j) <= key) exit
               x(j+1) = x(j)
               j = j - 1
            end do
            x(j+1) = key
         end do
      end subroutine sort_int_vec"""

    asrt_pub = (
        "public :: argsort_real !@pyapi kind=subroutine "
        "args=x:real(dp)(:):intent(in),idx:integer(:):intent(out) desc=\"argsort indices (0-based) of real vector\""
    )
    asrt_blk = """      subroutine argsort_real(x, idx)
         real(kind=dp), intent(in) :: x(:)
         integer, intent(out) :: idx(:)
         integer :: i, j, n, key
         n = size(x)
         if (size(idx) < n) stop "argsort_real: output array too small"
         do i = 1, n
            idx(i) = i - 1
         end do
         do i = 2, n
            key = idx(i)
            j = i - 1
            do while (j >= 1)
               if (x(idx(j)+1) <= x(key+1)) exit
               idx(j+1) = idx(j)
               j = j - 1
            end do
            idx(j+1) = key
         end do
      end subroutine argsort_real"""

    asrt_i_pub = (
        "public :: argsort_int !@pyapi kind=subroutine "
        "args=x:integer(:):intent(in),idx:integer(:):intent(out) desc=\"argsort indices (0-based) of integer vector\""
    )
    asrt_i_blk = """      subroutine argsort_int(x, idx)
         integer, intent(in) :: x(:)
         integer, intent(out) :: idx(:)
         integer :: i, j, n, key
         n = size(x)
         if (size(idx) < n) stop "argsort_int: output array too small"
         do i = 1, n
            idx(i) = i - 1
         end do
         do i = 2, n
            key = idx(i)
            j = i - 1
            do while (j >= 1)
               if (x(idx(j)+1) <= x(key+1)) exit
               idx(j+1) = idx(j)
               j = j - 1
            end do
            idx(j+1) = key
         end do
      end subroutine argsort_int"""

    sort_vec_pub = "public :: sort_vec"
    sort_vec_blk = """      interface sort_vec
         module procedure sort_real_vec, sort_int_vec
      end interface sort_vec"""

    argsort_pub = "public :: argsort"
    argsort_blk = """      interface argsort
         module procedure argsort_real, argsort_int
      end interface argsort"""

    mean_pub = (
        "public :: mean_1d !@pyapi kind=function ret=real(dp) "
        "args=x:real(dp)(:):intent(in) desc=\"mean of 1D real vector\""
    )
    mean_blk = """      pure real(kind=dp) function mean_1d(x)
         real(kind=dp), intent(in) :: x(:)
         if (size(x) <= 0) then
            mean_1d = 0.0_dp
         else
            mean_1d = sum(x) / real(size(x), kind=dp)
         end if
      end function mean_1d"""

    var_pub = (
        "public :: var_1d !@pyapi kind=function ret=real(dp) "
        "args=x:real(dp)(:):intent(in),ddof:integer:intent(in):optional "
        "desc=\"variance of 1D real vector with optional ddof (numpy-style)\""
    )
    var_blk = """      pure real(kind=dp) function var_1d(x, ddof)
         real(kind=dp), intent(in) :: x(:)
         integer, intent(in), optional :: ddof
         integer :: n, d
         real(kind=dp) :: mu
         n = size(x)
         if (present(ddof)) then
            d = ddof
         else
            d = 0
         end if
         if (n <= d .or. n <= 0) then
            var_1d = 0.0_dp
            return
         end if
         mu = mean_1d(x)
         var_1d = sum((x - mu)**2) / real(n - d, kind=dp)
      end function var_1d"""

    bcnt_pub = (
        "public :: bincount_int !@pyapi kind=function ret=integer(:) "
        "args=x:integer(:):intent(in),minlength:integer:intent(in):optional desc=\"count occurrences of nonnegative integers\""
    )
    bcnt_blk = """      function bincount_int(x, minlength) result(c)
         integer, intent(in) :: x(:)
         integer, intent(in), optional :: minlength
         integer, allocatable :: c(:)
         integer :: i, nmax, nout, m
         if (present(minlength)) then
            m = max(0, minlength)
         else
            m = 0
         end if
         if (size(x) <= 0) then
            nout = m
            allocate(c(1:nout), source=0)
            return
         end if
         if (any(x < 0)) error stop 'bincount_int: negative values are not supported'
         nmax = maxval(x)
         nout = max(nmax + 1, m)
         allocate(c(1:nout), source=0)
         do i = 1, size(x)
            c(x(i) + 1) = c(x(i) + 1) + 1
         end do
      end function bincount_int"""

    ssl_pub = (
        "public :: searchsorted_left_int !@pyapi kind=function ret=integer(:) "
        "args=a:integer(:):intent(in),v:integer(:):intent(in) desc=\"searchsorted left indices for integer vectors\""
    )
    ssl_blk = """      function searchsorted_left_int(a, v) result(idx)
         integer, intent(in) :: a(:), v(:)
         integer, allocatable :: idx(:)
         integer :: i, lo, hi, mid, n
         n = size(a)
         allocate(idx(1:size(v)))
         do i = 1, size(v)
            lo = 1
            hi = n + 1
            do while (lo < hi)
               mid = (lo + hi) / 2
               if (mid <= n .and. a(mid) < v(i)) then
                  lo = mid + 1
               else
                  hi = mid
               end if
            end do
            idx(i) = lo - 1
         end do
      end function searchsorted_left_int"""

    ssr_pub = (
        "public :: searchsorted_right_int !@pyapi kind=function ret=integer(:) "
        "args=a:integer(:):intent(in),v:integer(:):intent(in) desc=\"searchsorted right indices for integer vectors\""
    )
    ssr_blk = """      function searchsorted_right_int(a, v) result(idx)
         integer, intent(in) :: a(:), v(:)
         integer, allocatable :: idx(:)
         integer :: i, lo, hi, mid, n
         n = size(a)
         allocate(idx(1:size(v)))
         do i = 1, size(v)
            lo = 1
            hi = n + 1
            do while (lo < hi)
               mid = (lo + hi) / 2
               if (mid <= n .and. a(mid) <= v(i)) then
                  lo = mid + 1
               else
                  hi = mid
               end if
            end do
            idx(i) = lo - 1
         end do
      end function searchsorted_right_int"""

    nansum_pub = (
        "public :: nansum !@pyapi kind=function ret=real(dp) "
        "args=x:real(dp)(:):intent(in) desc=\"sum ignoring NaN values\""
    )
    nansum_blk = """      pure real(kind=dp) function nansum(x)
         use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
         real(kind=dp), intent(in) :: x(:)
         integer :: i
         nansum = 0.0_dp
         do i = 1, size(x)
            if (.not. ieee_is_nan(x(i))) nansum = nansum + x(i)
         end do
      end function nansum"""

    nanmean_pub = (
        "public :: nanmean !@pyapi kind=function ret=real(dp) "
        "args=x:real(dp)(:):intent(in) desc=\"mean ignoring NaN values\""
    )
    nanmean_blk = """      pure real(kind=dp) function nanmean(x)
         use, intrinsic :: ieee_arithmetic, only: ieee_is_nan, ieee_value, ieee_quiet_nan
         real(kind=dp), intent(in) :: x(:)
         integer :: i, cnt
         nanmean = 0.0_dp
         cnt = 0
         do i = 1, size(x)
            if (.not. ieee_is_nan(x(i))) then
               nanmean = nanmean + x(i)
               cnt = cnt + 1
            end if
         end do
         if (cnt <= 0) then
            nanmean = ieee_value(0.0_dp, ieee_quiet_nan)
         else
            nanmean = nanmean / real(cnt, kind=dp)
         end if
      end function nanmean"""

    nanvar_pub = (
        "public :: nanvar !@pyapi kind=function ret=real(dp) "
        "args=x:real(dp)(:):intent(in),ddof:integer:intent(in):optional desc=\"variance ignoring NaN values with optional ddof\""
    )
    nanvar_blk = """      pure real(kind=dp) function nanvar(x, ddof)
         use, intrinsic :: ieee_arithmetic, only: ieee_is_nan, ieee_value, ieee_quiet_nan
         real(kind=dp), intent(in) :: x(:)
         integer, intent(in), optional :: ddof
         integer :: i, cnt, d
         real(kind=dp) :: mu, ss
         if (present(ddof)) then
            d = ddof
         else
            d = 0
         end if
         mu = nanmean(x)
         if (ieee_is_nan(mu)) then
            nanvar = ieee_value(0.0_dp, ieee_quiet_nan)
            return
         end if
         cnt = 0
         ss = 0.0_dp
         do i = 1, size(x)
            if (.not. ieee_is_nan(x(i))) then
               cnt = cnt + 1
               ss = ss + (x(i) - mu)**2
            end if
         end do
         if (cnt - d <= 0) then
            nanvar = ieee_value(0.0_dp, ieee_quiet_nan)
         else
            nanvar = ss / real(cnt - d, kind=dp)
         end if
      end function nanvar"""

    nanstd_pub = (
        "public :: nanstd !@pyapi kind=function ret=real(dp) "
        "args=x:real(dp)(:):intent(in),ddof:integer:intent(in):optional desc=\"standard deviation ignoring NaN values with optional ddof\""
    )
    nanstd_blk = """      pure real(kind=dp) function nanstd(x, ddof)
         real(kind=dp), intent(in) :: x(:)
         integer, intent(in), optional :: ddof
         if (present(ddof)) then
            nanstd = sqrt(nanvar(x, ddof))
         else
            nanstd = sqrt(nanvar(x))
         end if
      end function nanstd"""

    nanmin_pub = (
        "public :: nanmin !@pyapi kind=function ret=real(dp) "
        "args=x:real(dp)(:):intent(in) desc=\"minimum ignoring NaN values\""
    )
    nanmin_blk = """      pure real(kind=dp) function nanmin(x)
         use, intrinsic :: ieee_arithmetic, only: ieee_is_nan, ieee_value, ieee_quiet_nan
         real(kind=dp), intent(in) :: x(:)
         integer :: i
         logical :: found
         found = .false.
         nanmin = ieee_value(0.0_dp, ieee_quiet_nan)
         do i = 1, size(x)
            if (.not. ieee_is_nan(x(i))) then
               if (.not. found) then
                  nanmin = x(i)
                  found = .true.
               else
                  if (x(i) < nanmin) nanmin = x(i)
               end if
            end if
         end do
      end function nanmin"""

    nanmax_pub = (
        "public :: nanmax !@pyapi kind=function ret=real(dp) "
        "args=x:real(dp)(:):intent(in) desc=\"maximum ignoring NaN values\""
    )
    nanmax_blk = """      pure real(kind=dp) function nanmax(x)
         use, intrinsic :: ieee_arithmetic, only: ieee_is_nan, ieee_value, ieee_quiet_nan
         real(kind=dp), intent(in) :: x(:)
         integer :: i
         logical :: found
         found = .false.
         nanmax = ieee_value(0.0_dp, ieee_quiet_nan)
         do i = 1, size(x)
            if (.not. ieee_is_nan(x(i))) then
               if (.not. found) then
                  nanmax = x(i)
                  found = .true.
               else
                  if (x(i) > nanmax) nanmax = x(i)
               end if
            end if
         end do
      end function nanmax"""

    nanargmin_pub = (
        "public :: nanargmin !@pyapi kind=function ret=integer "
        "args=x:real(dp)(:):intent(in) desc=\"0-based argmin ignoring NaN values; -1 when all NaN\""
    )
    nanargmin_blk = """      pure integer function nanargmin(x)
         use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
         real(kind=dp), intent(in) :: x(:)
         integer :: i, best
         logical :: found
         found = .false.
         best = -1
         do i = 1, size(x)
            if (.not. ieee_is_nan(x(i))) then
               if (.not. found) then
                  best = i
                  found = .true.
               else
                  if (x(i) < x(best)) best = i
               end if
            end if
         end do
         if (found) then
            nanargmin = best - 1
         else
            nanargmin = -1
         end if
      end function nanargmin"""

    nanargmax_pub = (
        "public :: nanargmax !@pyapi kind=function ret=integer "
        "args=x:real(dp)(:):intent(in) desc=\"0-based argmax ignoring NaN values; -1 when all NaN\""
    )
    nanargmax_blk = """      pure integer function nanargmax(x)
         use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
         real(kind=dp), intent(in) :: x(:)
         integer :: i, best
         logical :: found
         found = .false.
         best = -1
         do i = 1, size(x)
            if (.not. ieee_is_nan(x(i))) then
               if (.not. found) then
                  best = i
                  found = .true.
               else
                  if (x(i) > x(best)) best = i
               end if
            end if
         end do
         if (found) then
            nanargmax = best - 1
         else
            nanargmax = -1
         end if
      end function nanargmax"""

    return {
        "isqrt_int": (isqrt_pub, isqrt_blk),
        "print_int_list": (pil_pub, pil_blk),
        "random_uniform": (ru_pub, ru_blk),
        "random_normal_vec": (rnv_pub, rnv_blk),
        "random_choice2": (rc2_pub, rc2_blk),
        "random_choice_norep": (rcnr_pub, rcnr_blk),
        "random_choice_prob": (rcp_pub, rcp_blk),
        "sort_real_vec": (srt_pub, srt_blk),
        "sort_int_vec": (srt_i_pub, srt_i_blk),
        "sort_vec": (sort_vec_pub, sort_vec_blk),
        "argsort_real": (asrt_pub, asrt_blk),
        "argsort_int": (asrt_i_pub, asrt_i_blk),
        "argsort": (argsort_pub, argsort_blk),
        "mean_1d": (mean_pub, mean_blk),
        "var_1d": (var_pub, var_blk),
        "bincount_int": (bcnt_pub, bcnt_blk),
        "searchsorted_left_int": (ssl_pub, ssl_blk),
        "searchsorted_right_int": (ssr_pub, ssr_blk),
        "nansum": (nansum_pub, nansum_blk),
        "nanmean": (nanmean_pub, nanmean_blk),
        "nanvar": (nanvar_pub, nanvar_blk),
        "nanstd": (nanstd_pub, nanstd_blk),
        "nanmin": (nanmin_pub, nanmin_blk),
        "nanmax": (nanmax_pub, nanmax_blk),
        "nanargmin": (nanargmin_pub, nanargmin_blk),
        "nanargmax": (nanargmax_pub, nanargmax_blk),
    }


def ensure_runtime_helpers(runtime_path, needed_helpers):
    helper_templates = runtime_helper_templates()
    helper_deps = {
        "sort_vec": ["sort_real_vec", "sort_int_vec"],
        "argsort": ["argsort_real", "argsort_int"],
        "nanstd": ["nanvar", "nanmean"],
        "nanvar": ["nanmean"],
    }

    expanded = []
    seen = set()
    for h in needed_helpers:
        hl = h.lower()
        if hl not in seen:
            expanded.append(hl)
            seen.add(hl)
        for dep in helper_deps.get(hl, []):
            if dep not in seen:
                expanded.append(dep)
                seen.add(dep)
    needed_helpers = expanded

    runtime_text = runtime_path.read_text(encoding="utf-8")
    module_name, exports, has_proc = discover_runtime_exports(runtime_text)

    missing = []
    for h in needed_helpers:
        hl = h.lower()
        if hl not in helper_templates:
            raise RuntimeError(f"no template for helper '{h}'")
        if hl not in exports or hl not in has_proc:
            missing.append(hl)

    if not missing:
        return module_name, False

    lines = runtime_text.splitlines()
    idx_contains, idx_end_module = _find_insertion_points(lines)

    public_lines = []
    proc_blocks = []

    for hl in missing:
        pub, blk = helper_templates[hl]
        if hl not in exports:
            public_lines.append(pub)
        if hl not in has_proc:
            proc_blocks.append(blk)

    lines2 = _insert_public(lines, idx_contains, public_lines)
    _, idx_end_module2 = _find_insertion_points(lines2)
    lines3 = _insert_procs(lines2, idx_end_module2, proc_blocks)

    bak = runtime_path.with_suffix(runtime_path.suffix + ".bak")
    if not bak.exists():
        bak.write_text(runtime_text, encoding="utf-8")

    runtime_path.write_text("\n".join(lines3) + "\n", encoding="utf-8")
    return module_name, True


# -------------------------
# fortran emitter
# -------------------------

class emit:
    def __init__(self):
        self.lines = []
        self.ind = 0

    def w(self, s=""):
        self.lines.append(" " * self.ind + s)

    def push(self):
        self.ind += 3

    def pop(self):
        self.ind = max(0, self.ind - 3)

    def text(self):
        return "\n".join(self.lines) + "\n"


# -------------------------
# python -> fortran translator (subset)
# -------------------------

class translator(ast.NodeVisitor):
    def __init__(
        self,
        out,
        params,
        context,
        list_counts,
        function_result_name=None,
        comment_map=None,
        tuple_return_funcs=None,
        dict_return_types=None,
        local_return_specs=None,
        tuple_return_out_kinds=None,
        dict_type_components=None,
        local_func_arg_ranks=None,
    ):
        # context: "flat" | "compute" | "run_print"
        self.o = out
        self.params = params
        self.context = context
        self.list_counts = list_counts  # map list var -> count var
        self.ints = set()
        self.reals = set()
        self.logs = set()
        self.alloc_logs = set()
        self.alloc_ints = set()
        self.alloc_reals = set()
        self.alloc_real_rank = {}
        self.alloc_int_rank = {}
        self.alloc_log_rank = {}
        self.broadcast_col2 = set()
        self.broadcast_row2 = set()
        self.nonzero_tuple_vars = set()
        self.function_result_name = function_result_name
        self.comment_map = comment_map or {}
        self._last_comment_line = 0
        self.tuple_return_funcs = set(tuple_return_funcs or [])
        self.local_return_specs = dict(local_return_specs or {})
        self.tuple_return_out_kinds = dict(tuple_return_out_kinds or {})
        self.dict_return_types = dict(dict_return_types or {})
        self.dict_typed_vars = {}
        self.dict_aliases = {}
        self.dict_type_components = dict(dict_type_components or {})
        self.local_func_arg_ranks = dict(local_func_arg_ranks or {})
        self.dict_var_components = {}
        self.reserved_names = {"dp"}

    def _mark_int(self, name):
        if name == "_":
            return
        if name in self.reserved_names:
            return
        if name in self.params:
            return
        if name in self.reals:
            return
        if name in self.logs:
            return
        if name in self.alloc_ints or name in self.alloc_logs:
            return
        self.ints.add(name)

    def _mark_real(self, name):
        if name == "_":
            return
        if name in self.reserved_names:
            return
        if name in self.params:
            return
        if name in self.logs:
            return
        if name in self.alloc_ints or name in self.alloc_logs or name in self.alloc_reals:
            return
        self.reals.add(name)
        self.ints.discard(name)

    def _mark_log(self, name):
        if name == "_":
            return
        if name in self.reserved_names:
            return
        if name in self.params:
            return
        if name in self.alloc_ints or name in self.alloc_logs or name in self.alloc_reals:
            return
        self.logs.add(name)
        self.ints.discard(name)
        self.reals.discard(name)

    def _mark_alloc_int(self, name, rank=1):
        if name == "_":
            return
        if name in self.reserved_names:
            return
        if name in self.params:
            return
        self.alloc_ints.add(name)
        if rank is None or rank < 1:
            rank = 1
        self.alloc_int_rank[name] = max(rank, self.alloc_int_rank.get(name, 1))
        self.ints.discard(name)
        self.reals.discard(name)
        self.logs.discard(name)
        self.alloc_logs.discard(name)
        self.alloc_log_rank.pop(name, None)
        self.alloc_reals.discard(name)
        self.alloc_real_rank.pop(name, None)

    def _mark_alloc_real(self, name, rank=1):
        if name == "_":
            return
        if name in self.reserved_names:
            return
        if name in self.params:
            return
        self.alloc_reals.add(name)
        if rank is None or rank < 1:
            rank = 1
        self.alloc_real_rank[name] = max(rank, self.alloc_real_rank.get(name, 1))
        self.ints.discard(name)
        self.reals.discard(name)
        self.logs.discard(name)
        self.alloc_logs.discard(name)
        self.alloc_log_rank.pop(name, None)
        self.alloc_ints.discard(name)
        self.alloc_int_rank.pop(name, None)

    def _mark_alloc_log(self, name, rank=1):
        if name == "_":
            return
        if name in self.reserved_names:
            return
        if name in self.params:
            return
        self.alloc_logs.add(name)
        if rank is None or rank < 1:
            rank = 1
        self.alloc_log_rank[name] = max(rank, self.alloc_log_rank.get(name, 1))
        self.ints.discard(name)
        self.reals.discard(name)
        self.logs.discard(name)
        self.alloc_ints.discard(name)
        self.alloc_int_rank.pop(name, None)
        self.alloc_reals.discard(name)
        self.alloc_real_rank.pop(name, None)

    def _emit_comments_for(self, node):
        ln = getattr(node, "lineno", None)
        if ln is None:
            return
        for i in range(self._last_comment_line + 1, ln + 1):
            for c in self.comment_map.get(i, []):
                self.o.w(f"! {c}")
        if ln > self._last_comment_line:
            self._last_comment_line = ln

    def _np_dtype_text(self, call_node):
        dtype_txt = ""
        for kw in getattr(call_node, "keywords", []):
            if kw.arg == "dtype":
                if isinstance(kw.value, ast.Name):
                    dtype_txt = kw.value.id.lower()
                elif (
                    isinstance(kw.value, ast.Attribute)
                    and isinstance(kw.value.value, ast.Name)
                    and kw.value.value.id == "np"
                ):
                    dtype_txt = kw.value.attr.lower()
        return dtype_txt

    def _expr_kind(self, node):
        if isinstance(node, ast.Constant):
            if isinstance(node.value, bool):
                return "logical"
            if isinstance(node.value, int):
                return "int"
            if isinstance(node.value, float):
                return "real"
            return None
        if isinstance(node, ast.Name):
            if node.id in self.dict_typed_vars:
                return None
            if node.id in self.reals:
                return "real"
            if node.id in self.ints:
                return "int"
            if node.id in self.logs:
                return "logical"
            if node.id in self.alloc_reals:
                return "real"
            if node.id in self.alloc_ints:
                return "int"
            if node.id in self.alloc_logs:
                return "logical"
            return None
        if isinstance(node, ast.List):
            if not node.elts:
                return None
            kinds = {self._expr_kind(e) for e in node.elts}
            kinds.discard(None)
            if not kinds:
                return None
            if "real" in kinds:
                return "real"
            if kinds == {"int"}:
                return "int"
            if kinds == {"logical"}:
                return "logical"
            return None
        if isinstance(node, ast.Tuple):
            if not node.elts:
                return None
            kinds = {self._expr_kind(e) for e in node.elts}
            kinds.discard(None)
            if not kinds:
                return None
            if "real" in kinds:
                return "real"
            if kinds == {"int"}:
                return "int"
            if kinds == {"logical"}:
                return "logical"
            return None
        if isinstance(node, ast.Attribute):
            if node.attr == "size" and isinstance(node.value, ast.Name):
                return "int"
            if node.attr in {"c_contiguous", "f_contiguous"}:
                if isinstance(node.value, ast.Attribute) and node.value.attr == "flags":
                    return "logical"
            if node.attr == "T":
                return self._expr_kind(node.value)
            if isinstance(node.value, ast.Name) and node.value.id == "np" and node.attr in {"pi", "nan", "inf", "NINF"}:
                return "real"
            return None
        if isinstance(node, ast.UnaryOp):
            return self._expr_kind(node.operand)
        if isinstance(node, ast.BinOp):
            if isinstance(node.op, ast.Div):
                return "real"
            if isinstance(node.op, (ast.BitAnd, ast.BitOr)):
                return "logical"
            lk = self._expr_kind(node.left)
            rk = self._expr_kind(node.right)
            if lk == "real" or rk == "real":
                return "real"
            if lk == "int" and rk == "int":
                return "int"
            return None
        if isinstance(node, ast.IfExp):
            bk = self._expr_kind(node.body)
            ok = self._expr_kind(node.orelse)
            if bk == "real" or ok == "real":
                return "real"
            if bk == "int" and ok == "int":
                return "int"
            return None
        if isinstance(node, ast.Compare):
            return "logical"
        if isinstance(node, ast.BoolOp):
            return "logical"
        if isinstance(node, ast.Subscript):
            if (
                isinstance(node.value, ast.Name)
                and node.value.id in self.dict_typed_vars
                and isinstance(node.slice, ast.Constant)
                and isinstance(node.slice.value, str)
            ):
                tname = self.dict_typed_vars[node.value.id]
                cinfo = self.dict_type_components.get(tname, {}).get(node.slice.value, None)
                if cinfo is not None:
                    ckind = cinfo[0] if isinstance(cinfo, tuple) else cinfo
                    if ckind.startswith("real"):
                        return "real"
                    if ckind.startswith("int"):
                        return "int"
                    if ckind.startswith("logical"):
                        return "logical"
            if isinstance(node.value, ast.Name):
                nm = node.value.id
                if nm in self.alloc_logs:
                    return "logical"
                if nm in self.alloc_ints:
                    return "int"
                if nm in self.alloc_reals:
                    return "real"
            return None
        if isinstance(node, ast.Call):
            if isinstance(node.func, ast.Name) and node.func.id in {
                "cumsum",
                "cumprod",
                "repeat",
                "tile",
                "unique",
            } and len(node.args) >= 1:
                return self._expr_kind(node.args[0])
            if isinstance(node.func, ast.Name) and node.func.id == "eye":
                return "real"
            if isinstance(node.func, ast.Name) and node.func.id == "diag" and len(node.args) >= 1:
                return self._expr_kind(node.args[0])
            if isinstance(node.func, ast.Name):
                if node.func.id in self.local_func_arg_ranks:
                    ranks = self.local_func_arg_ranks.get(node.func.id, [])
                    parts = []
                    for i, a in enumerate(node.args):
                        ae = self.expr(a)
                        er = ranks[i] if i < len(ranks) else 0
                        ar = self._rank_expr(a)
                        if er == 2 and ar == 1:
                            ae = f"reshape({ae}, [size({ae}), 1])"
                        elif er == 1 and ar == 2:
                            ae = f"reshape({ae}, [size({ae})])"
                        parts.append(ae)
                    for kw in getattr(node, "keywords", []):
                        if kw.arg is None:
                            raise NotImplementedError("**kwargs not supported")
                        parts.append(self.expr(kw.value))
                    args = ", ".join(parts)
                    return f"{node.func.id}({args})"
                if node.func.id in self.local_return_specs:
                    spec = self.local_return_specs[node.func.id]
                    if spec in {"real", "alloc_real"}:
                        return "real"
                    if spec in {"int", "alloc_int"}:
                        return "int"
                if node.func.id in {"float"}:
                    return "real"
                if node.func.id in {"int", "isqrt", "size", "len"}:
                    return "int"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "array"
                and len(node.args) >= 1
            ):
                dtype_txt = ""
                for kw in node.keywords:
                    if kw.arg == "dtype":
                        if isinstance(kw.value, ast.Name):
                            dtype_txt = kw.value.id.lower()
                        elif (
                            isinstance(kw.value, ast.Attribute)
                            and isinstance(kw.value.value, ast.Name)
                            and kw.value.value.id == "np"
                        ):
                            dtype_txt = kw.value.attr.lower()
                if "float" in dtype_txt:
                    return "real"
                if "int" in dtype_txt:
                    return "int"
                if "bool" in dtype_txt:
                    return "logical"
                return self._expr_kind(node.args[0])
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "asarray"
                and len(node.args) >= 1
            ):
                dtype_txt = ""
                for kw in node.keywords:
                    if kw.arg == "dtype":
                        if isinstance(kw.value, ast.Name):
                            dtype_txt = kw.value.id.lower()
                        elif (
                            isinstance(kw.value, ast.Attribute)
                            and isinstance(kw.value.value, ast.Name)
                            and kw.value.value.id == "np"
                        ):
                            dtype_txt = kw.value.attr.lower()
                if "float" in dtype_txt:
                    return "real"
                if "int" in dtype_txt:
                    return "int"
                return self._expr_kind(node.args[0])
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "arange"
            ):
                dtype_txt = ""
                for kw in node.keywords:
                    if kw.arg == "dtype":
                        if isinstance(kw.value, ast.Name):
                            dtype_txt = kw.value.id.lower()
                        elif (
                            isinstance(kw.value, ast.Attribute)
                            and isinstance(kw.value.value, ast.Name)
                            and kw.value.value.id == "np"
                        ):
                            dtype_txt = kw.value.attr.lower()
                if "float" in dtype_txt:
                    return "real"
                return "int"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in {"array", "asarray"}
                and len(node.args) >= 1
            ):
                dtype_txt = self._np_dtype_text(node)
                if "float" in dtype_txt:
                    return "real"
                if "int" in dtype_txt:
                    return "int"
                if "bool" in dtype_txt:
                    return "logical"
                a0 = node.args[0]
                if isinstance(a0, ast.Constant):
                    if isinstance(a0.value, bool):
                        return "logical"
                    if isinstance(a0.value, int):
                        return "int"
                    if isinstance(a0.value, float):
                        return "real"
                if isinstance(a0, (ast.List, ast.Tuple)) and len(a0.elts) > 0:
                    kinds = [self._expr_kind(e) for e in a0.elts]
                    if any(k == "real" for k in kinds):
                        return "real"
                    if all(k == "logical" for k in kinds if k is not None):
                        return "logical"
                    if all(k == "int" for k in kinds if k is not None):
                        return "int"
                k0 = self._expr_kind(a0)
                if k0 is not None:
                    return k0
                return "real"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in {"zeros", "ones"}
            ):
                dtype_txt = self._np_dtype_text(node)
                if "int" in dtype_txt:
                    return "int"
                if "bool" in dtype_txt:
                    return "logical"
                return "real"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in {"zeros_like", "ones_like"}
                and len(node.args) >= 1
            ):
                return self._expr_kind(node.args[0])
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "diag"
                and len(node.args) >= 1
            ):
                return self._expr_kind(node.args[0])
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in {"cumsum", "cumprod", "repeat", "tile", "unique"}
                and len(node.args) >= 1
            ):
                return self._expr_kind(node.args[0])
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "bincount"
                and len(node.args) >= 1
            ):
                return "int"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "searchsorted"
                and len(node.args) >= 2
            ):
                return "int"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in {"nonzero", "argwhere", "setdiff1d", "intersect1d", "lexsort", "unravel_index", "ravel_multi_index"}
                and len(node.args) >= 1
            ):
                return "int"
            if (
                isinstance(node.func, ast.Attribute)
                and node.func.attr == "reduceat"
                and isinstance(node.func.value, ast.Attribute)
                and isinstance(node.func.value.value, ast.Name)
                and node.func.value.value.id == "np"
                and len(node.args) >= 1
            ):
                if node.func.value.attr in {"logical_and", "logical_or"}:
                    return "logical"
                return self._expr_kind(node.args[0])
            if (
                isinstance(node.func, ast.Attribute)
                and node.func.attr == "uniform"
            ):
                return "real"
            if (
                isinstance(node.func, ast.Attribute)
                and node.func.attr == "integers"
            ):
                return "int"
            if (
                isinstance(node.func, ast.Attribute)
                and node.func.attr == "permutation"
                and len(node.args) >= 1
            ):
                return self._expr_kind(node.args[0])
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in {"mean", "var", "std", "log2", "log10", "nansum", "nanmean", "nanvar", "nanstd", "nanmin", "nanmax"}
                and len(node.args) >= 1
            ):
                return "real"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in {"nanargmin", "nanargmax"}
                and len(node.args) >= 1
            ):
                return "int"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in {"all", "any", "isfinite", "isinf", "isnan"}
                and len(node.args) >= 1
            ):
                return "logical"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in {"eye", "identity", "linspace"}
            ):
                return "real"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "full"
                and len(node.args) >= 2
            ):
                return self._expr_kind(node.args[1])
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in {"prod", "dot", "matmul", "clip", "diff", "full_like", "hstack", "vstack", "column_stack", "concatenate", "transpose", "swapaxes", "expand_dims", "abs", "fabs", "sign", "floor", "ceil", "round", "ascontiguousarray", "asfortranarray"}
                and len(node.args) >= 1
            ):
                return self._expr_kind(node.args[0])
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in {"sin", "cos", "tan", "mod", "floor_divide", "bitwise_and", "bitwise_or", "bitwise_xor"}
                and len(node.args) >= 1
            ):
                if node.func.attr.startswith("bitwise"):
                    return "int"
                if node.func.attr in {"mod", "floor_divide"}:
                    return self._expr_kind(node.args[0])
                return "real"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "broadcast_to"
                and len(node.args) >= 1
            ):
                return self._expr_kind(node.args[0])
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "take"
                and len(node.args) >= 2
            ):
                return self._expr_kind(node.args[0])
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "min"
                and len(node.args) >= 1
            ):
                return self._expr_kind(node.args[0])
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "count_nonzero"
            ):
                return "int"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Attribute)
                and isinstance(node.func.value.value, ast.Name)
                and node.func.value.value.id == "np"
                and node.func.value.attr == "linalg"
                and node.func.attr == "norm"
                and len(node.args) >= 1
            ):
                return "real"
            if (
                isinstance(node.func, ast.Attribute)
                and node.func.attr in {"min", "max"}
                and len(node.args) == 0
            ):
                return self._expr_kind(node.func.value)
            if (
                isinstance(node.func, ast.Attribute)
                and node.func.attr in {"argmin", "argmax"}
                and len(node.args) == 0
            ):
                return "int"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "eye"
            ):
                return "real"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in {"triu", "tril"}
                and len(node.args) >= 1
            ):
                return self._expr_kind(node.args[0])
            if isinstance(node.func, ast.Attribute) and node.func.attr == "reshape":
                return self._expr_kind(node.func.value)
            if isinstance(node.func, ast.Attribute) and node.func.attr == "astype":
                dtype_txt = ""
                if len(node.args) >= 1:
                    a0 = node.args[0]
                    if isinstance(a0, ast.Name):
                        dtype_txt = a0.id.lower()
                    elif (
                        isinstance(a0, ast.Attribute)
                        and isinstance(a0.value, ast.Name)
                        and a0.value.id == "np"
                    ):
                        dtype_txt = a0.attr.lower()
                if "float" in dtype_txt:
                    return "real"
                if "bool" in dtype_txt:
                    return "logical"
                if "int" in dtype_txt:
                    return "int"
                return self._expr_kind(node.func.value)
            if isinstance(node.func, ast.Attribute) and node.func.attr in {"ravel", "flatten"}:
                return self._expr_kind(node.func.value)
            if isinstance(node.func, ast.Attribute) and isinstance(node.func.value, ast.Name):
                if node.func.attr == "copy" and len(node.args) == 0:
                    return self._expr_kind(node.func.value)
                if node.func.value.id in {"math", "np"} and node.func.attr == "sqrt":
                    return "real"
                if node.func.value.id == "random" and node.func.attr == "random":
                    return "real"
            return None
        return None

    def _extent_expr(self, node):
        """Best-effort extent expression for array-valued expressions."""
        if isinstance(node, ast.Name):
            if node.id in self.alloc_reals or node.id in self.alloc_ints or node.id in self.alloc_logs:
                return f"size({node.id})"
            return None
        if isinstance(node, (ast.List, ast.Tuple)):
            if node.elts:
                e0 = self._extent_expr(node.elts[0])
                if e0 is not None:
                    return e0
            return None
        if isinstance(node, ast.BinOp):
            left = self._extent_expr(node.left)
            if left is not None:
                return left
            right = self._extent_expr(node.right)
            if right is not None:
                return right
        if isinstance(node, ast.UnaryOp):
            return self._extent_expr(node.operand)
        if isinstance(node, ast.IfExp):
            b = self._extent_expr(node.body)
            if b is not None:
                return b
            return self._extent_expr(node.orelse)
        if isinstance(node, ast.Subscript):
            if (
                isinstance(node.value, ast.Name)
                and node.value.id in self.dict_typed_vars
                and isinstance(node.slice, ast.Constant)
                and isinstance(node.slice.value, str)
            ):
                tname = self.dict_typed_vars[node.value.id]
                cinfo = self.dict_type_components.get(tname, {}).get(node.slice.value, None)
                if cinfo is not None:
                    ckind = cinfo[0] if isinstance(cinfo, tuple) else cinfo
                    if "array" in ckind:
                        return f"size({self.expr(node)})"
            if isinstance(node.slice, ast.Slice):
                return f"size({self.expr(node.value)})"
            if isinstance(node.slice, ast.Tuple):
                # Scalar tuple indexing like a(i,j) is scalar-valued.
                if all((not isinstance(e, ast.Slice)) and (not is_none(e)) and self._rank_expr(e) == 0 for e in node.slice.elts):
                    return None
                return f"size({self.expr(node.value)})"
            # Vector subscript: x(idx_vec) has extent size(idx_vec).
            sr = self._rank_expr(node.slice)
            if sr > 0:
                return f"size({self.expr(node.slice)})"
            return None
        if isinstance(node, ast.Attribute):
            if node.attr == "T":
                return f"size({self.expr(node.value)})"
            return None
        if isinstance(node, ast.Call):
            # Pass through known array-returning calls when we can.
            if isinstance(node.func, ast.Name) and node.func.id in {"pack"} and len(node.args) >= 1:
                return f"size({self.expr(node.args[0])})"
            if (
                isinstance(node.func, ast.Name)
                and node.func.id in {"cumsum", "cumprod", "repeat", "tile", "unique", "arange_int"}
                and len(node.args) >= 1
            ):
                return f"size({self.expr(node.args[0])})"
            if isinstance(node.func, ast.Attribute) and node.func.attr == "copy" and len(node.args) == 0:
                return self._extent_expr(node.func.value)
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.attr in {"sum", "mean", "var", "max", "min"}
            ):
                axis_node = None
                for kw in node.keywords:
                    if kw.arg == "axis":
                        axis_node = kw.value
                        break
                if axis_node is not None:
                    return f"size({self.expr(node.func.value)})"
                return None
            if isinstance(node.func, ast.Name) and node.func.id in {"reshape", "spread"} and len(node.args) >= 1:
                return f"size({self.expr(node.args[0])})"
            if isinstance(node.func, ast.Name) and node.func.id in {"real", "int", "float"} and len(node.args) >= 1:
                return self._extent_expr(node.args[0])
            if isinstance(node.func, ast.Attribute) and node.func.attr in {"ravel", "flatten"}:
                return f"size({self.expr(node.func.value)})"
            if (
                isinstance(node.func, ast.Name)
                and node.func.id in self.local_return_specs
                and self.local_return_specs[node.func.id] in {"alloc_real", "alloc_int", "alloc_log"}
                and len(node.args) >= 1
            ):
                return f"size({self.expr(node.args[0])})"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in {"log", "exp", "sqrt", "maximum", "asarray", "ascontiguousarray", "asfortranarray", "broadcast_to"}
                and len(node.args) >= 1
            ):
                return self._extent_expr(node.args[0])
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in {"sin", "cos", "tan", "mod", "floor_divide", "bitwise_and", "bitwise_or", "bitwise_xor", "setdiff1d", "intersect1d", "lexsort"}
                and len(node.args) >= 1
            ):
                return self._extent_expr(node.args[0])
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in {"cumsum", "cumprod", "repeat", "tile", "unique", "stack", "hstack", "vstack", "column_stack", "concatenate", "transpose", "swapaxes", "expand_dims", "squeeze", "zeros_like", "ones_like", "full_like", "clip", "diff", "abs", "fabs", "sign", "floor", "ceil", "round", "isfinite", "isinf", "isnan", "ascontiguousarray", "asfortranarray", "broadcast_to"}
                and len(node.args) >= 1
            ):
                return self._extent_expr(node.args[0])
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in {
                    "minimum",
                    "zeros_like",
                    "ones_like",
                    "full_like",
                    "clip",
                    "diff",
                    "hstack",
                    "vstack",
                    "column_stack",
                    "concatenate",
                    "transpose",
                    "swapaxes",
                    "expand_dims",
                    "abs",
                    "fabs",
                    "sign",
                    "floor",
                    "ceil",
                    "round",
                    "isfinite",
                    "isinf",
                    "isnan",
                }
                and len(node.args) >= 1
            ):
                return self._extent_expr(node.args[0])
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in {"zeros", "ones", "full", "linspace", "arange"}
                and len(node.args) >= 1
            ):
                return f"size({self.expr(node)})"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in {"sum", "max", "min"}
                and len(node.args) >= 1
            ):
                axis_node = None
                for kw in node.keywords:
                    if kw.arg == "axis":
                        axis_node = kw.value
                        break
                if axis_node is not None:
                    return f"size({self.expr(node.args[0])})"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "bincount"
                and len(node.args) >= 1
            ):
                return f"size({self.expr(node.args[0])})"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "searchsorted"
                and len(node.args) >= 2
            ):
                if self._rank_expr(node.args[1]) > 0:
                    return f"size({self.expr(node.args[1])})"
                return None
            if (
                isinstance(node.func, ast.Attribute)
                and node.func.attr == "reduceat"
                and isinstance(node.func.value, ast.Attribute)
                and isinstance(node.func.value.value, ast.Name)
                and node.func.value.value.id == "np"
                and len(node.args) >= 2
            ):
                return f"size({self.expr(node.args[1])})"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "argwhere"
                and len(node.args) >= 1
            ):
                return f"count({self.expr(node.args[0])} /= 0)"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "take"
                and len(node.args) >= 2
            ):
                return f"size({self.expr(node.args[1])})"
            if (
                isinstance(node.func, ast.Attribute)
                and node.func.attr in {"uniform", "integers"}
            ):
                size_node = None
                for kw in node.keywords:
                    if kw.arg == "size":
                        size_node = kw.value
                        break
                if size_node is None:
                    return None
                if isinstance(size_node, (ast.Tuple, ast.List)):
                    if len(size_node.elts) >= 1:
                        if len(size_node.elts) == 1:
                            return f"({self.expr(size_node.elts[0])})"
                        return f"(({self.expr(size_node.elts[0])})*({self.expr(size_node.elts[1])}))"
                    return None
                return self.expr(size_node)
            if (
                isinstance(node.func, ast.Attribute)
                and node.func.attr == "permutation"
                and len(node.args) >= 1
            ):
                return f"size({self.expr(node.args[0])})"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in {"matmul", "dot", "identity"}
                and len(node.args) >= 1
            ):
                if node.func.attr == "identity":
                    return f"size({self.expr(node.args[0])})"
                if self._rank_expr(node) > 0:
                    return f"size({self.expr(node.args[0])})"
                return None
        return None

    def _slice_triplet(self, slc, extent_expr):
        """Convert a Python slice node to a Fortran triplet string."""
        if not isinstance(slc, ast.Slice):
            raise NotImplementedError("internal: _slice_triplet expects ast.Slice")
        def _int_const(node):
            if isinstance(node, ast.Constant) and isinstance(node.value, int):
                return int(node.value)
            if (
                isinstance(node, ast.UnaryOp)
                and isinstance(node.op, ast.USub)
                and isinstance(node.operand, ast.Constant)
                and isinstance(node.operand.value, int)
            ):
                return -int(node.operand.value)
            return None
        # Python default step is +1.
        step_val = 1
        if slc.step is not None:
            sval = _int_const(slc.step)
            if sval is None:
                raise NotImplementedError("slice step must be an integer constant")
            step_val = int(sval)
            if step_val == 0:
                raise NotImplementedError("slice step cannot be zero")

        def _lower_expr_fortran():
            if slc.lower is None:
                return extent_expr if step_val < 0 else "1"
            return f"({self.expr(slc.lower)} + 1)"

        def _upper_expr_fortran():
            if slc.upper is None:
                return "1" if step_val < 0 else extent_expr
            # For negative-step slices, Python upper bound is exclusive and maps to +1 in Fortran.
            if step_val < 0:
                return f"({self.expr(slc.upper)} + 1)"
            return self.expr(slc.upper)

        lb = _lower_expr_fortran()
        ub = _upper_expr_fortran()
        if step_val == 1:
            return ":" if lb == "1" and ub == extent_expr else f"{lb}:{ub}"
        return f"{lb}:{ub}:{step_val}"

    def _rank_expr(self, node):
        if isinstance(node, ast.Name):
            if node.id in self.broadcast_row2 or node.id in self.broadcast_col2:
                return 2
            if node.id in self.alloc_reals:
                return self.alloc_real_rank.get(node.id, 1)
            if node.id in self.alloc_ints:
                return self.alloc_int_rank.get(node.id, 1)
            if node.id in self.alloc_logs:
                return self.alloc_log_rank.get(node.id, 1)
            return 0
        if isinstance(node, ast.List):
            if node.elts and all(isinstance(e, ast.List) for e in node.elts):
                return 2
            return 1
        if isinstance(node, ast.Tuple):
            if node.elts and all(isinstance(e, ast.Tuple) for e in node.elts):
                return 2
            return 1
        if isinstance(node, ast.Subscript):
            if (
                isinstance(node.value, ast.Name)
                and node.value.id in self.dict_typed_vars
                and isinstance(node.slice, ast.Constant)
                and isinstance(node.slice.value, str)
            ):
                tname = self.dict_typed_vars[node.value.id]
                cinfo = self.dict_type_components.get(tname, {}).get(node.slice.value, None)
                if cinfo is not None:
                    if isinstance(cinfo, tuple):
                        ckind, crank = cinfo
                    else:
                        ckind, crank = cinfo, 1
                    if "array" in ckind:
                        return max(1, crank)
                    return 0
            if isinstance(node.slice, ast.Tuple):
                # NumPy-style tuple indexing rank:
                # - all scalar indices -> scalar
                # - mixed slices/newaxis/vector indices -> rank from active dimensions
                # - advanced pairwise indexing with multiple vectors and no slices -> 1D
                elts = list(node.slice.elts)
                has_slice_none = any(isinstance(e, ast.Slice) or is_none(e) for e in elts)
                vec_ranks = [self._rank_expr(e) for e in elts if (not isinstance(e, ast.Slice)) and (not is_none(e)) and self._rank_expr(e) > 0]
                if not has_slice_none:
                    if not vec_ranks:
                        return 0
                    if len(vec_ranks) >= 2:
                        return max(vec_ranks)
                    return vec_ranks[0]
                base_rank = sum(1 for e in elts if isinstance(e, ast.Slice) or is_none(e))
                return base_rank + sum(vec_ranks)
            if isinstance(node.slice, ast.Slice):
                return 1
            # Vector subscript: x(idx_vec) is array-valued.
            sr = self._rank_expr(node.slice)
            if sr > 0:
                # Logical mask indexing (NumPy a[mask]) flattens selection to 1D.
                if self._expr_kind(node.slice) == "logical":
                    return 1
                return sr
            return 0
        if isinstance(node, ast.Compare):
            r = self._rank_expr(node.left)
            for c in node.comparators:
                r = max(r, self._rank_expr(c))
            return r
        if isinstance(node, ast.IfExp):
            return max(self._rank_expr(node.body), self._rank_expr(node.orelse))
        if isinstance(node, ast.BinOp):
            if isinstance(node.op, ast.MatMult):
                r1 = self._rank_expr(node.left)
                r2 = self._rank_expr(node.right)
                if r1 == 2 and r2 == 2:
                    return 2
                if (r1 == 2 and r2 == 1) or (r1 == 1 and r2 == 2):
                    return 1
                return 0
            return max(self._rank_expr(node.left), self._rank_expr(node.right))
        if isinstance(node, ast.UnaryOp):
            return self._rank_expr(node.operand)
        if isinstance(node, ast.Attribute) and node.attr == "T":
            return self._rank_expr(node.value)
        if isinstance(node, ast.Call):
            if isinstance(node.func, ast.Name) and node.func.id in {
                "cumsum",
                "cumprod",
                "repeat",
                "tile",
                "unique",
                "arange_int",
            } and len(node.args) >= 1:
                return 1
            if isinstance(node.func, ast.Name) and node.func.id == "diag" and len(node.args) >= 1:
                r0 = self._rank_expr(node.args[0])
                return 2 if r0 <= 1 else 1
            if isinstance(node.func, ast.Name) and node.func.id == "eye":
                return 2
            if isinstance(node.func, ast.Name):
                if node.func.id in {"log_normal_pdf_1d", "normal_logpdf_1d"}:
                    return 2
                if node.func.id == "reshape" and len(node.args) >= 2 and isinstance(node.args[1], ast.List):
                    return max(1, len(node.args[1].elts))
                if node.func.id == "spread" and len(node.args) >= 1:
                    return self._rank_expr(node.args[0]) + 1
                if node.func.id in self.local_return_specs and self.local_return_specs[node.func.id] in {"alloc_real", "alloc_int", "alloc_log"}:
                    if len(node.args) >= 1:
                        return max(1, self._rank_expr(node.args[0]))
                    return 1
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
            ):
                if node.func.attr in {"log", "exp", "sqrt", "maximum", "asarray", "array"} and len(node.args) >= 1:
                    return self._rank_expr(node.args[0])
                if node.func.attr in {"minimum"} and len(node.args) >= 1:
                    return max(self._rank_expr(node.args[0]), self._rank_expr(node.args[1]))
                if node.func.attr == "arange" and len(node.args) >= 1:
                    return 1
                if node.func.attr == "linspace" and len(node.args) >= 1:
                    return 1
                if node.func.attr == "eye":
                    return 2
                if node.func.attr == "identity":
                    return 2
                if node.func.attr in {"zeros", "ones"}:
                    if len(node.args) >= 1 and isinstance(node.args[0], (ast.Tuple, ast.List)):
                        return max(1, len(node.args[0].elts))
                    return 1
                if node.func.attr == "full" and len(node.args) >= 1:
                    if isinstance(node.args[0], (ast.Tuple, ast.List)):
                        return max(1, len(node.args[0].elts))
                    return 1
                if node.func.attr in {"zeros_like", "ones_like"} and len(node.args) >= 1:
                    return self._rank_expr(node.args[0])
                if node.func.attr in {"full_like", "clip", "transpose", "swapaxes", "abs", "fabs", "sign", "floor", "ceil", "round", "isfinite", "isinf", "isnan", "gradient", "ascontiguousarray", "asfortranarray"} and len(node.args) >= 1:
                    return self._rank_expr(node.args[0])
                if node.func.attr in {"pad", "roll", "flip"} and len(node.args) >= 1:
                    return self._rank_expr(node.args[0])
                if node.func.attr == "take" and len(node.args) >= 2:
                    return self._rank_expr(node.args[1])
                if node.func.attr == "bincount" and len(node.args) >= 1:
                    return 1
                if node.func.attr == "searchsorted" and len(node.args) >= 2:
                    r = self._rank_expr(node.args[1])
                    return r if r > 0 else 0
                if (
                    node.func.attr == "reduceat"
                    and isinstance(node.func.value, ast.Attribute)
                    and isinstance(node.func.value.value, ast.Name)
                    and node.func.value.value.id == "np"
                    and len(node.args) >= 2
                ):
                    return self._rank_expr(node.args[1])
                if node.func.attr in {"uniform", "integers"}:
                    size_node = None
                    for kw in node.keywords:
                        if kw.arg == "size":
                            size_node = kw.value
                            break
                    if size_node is None:
                        return 0
                    if isinstance(size_node, (ast.Tuple, ast.List)):
                        return max(1, len(size_node.elts))
                    return 1
                if node.func.attr == "permutation" and len(node.args) >= 1:
                    return 1
                if node.func.attr == "broadcast_to" and len(node.args) >= 2:
                    shp = node.args[1]
                    if isinstance(shp, (ast.Tuple, ast.List)):
                        return max(1, len(shp.elts))
                    return max(1, self._rank_expr(node.args[0]))
                if node.func.attr == "expand_dims" and len(node.args) >= 1:
                    return self._rank_expr(node.args[0]) + 1
                if node.func.attr == "squeeze" and len(node.args) >= 1:
                    return 1
                if node.func.attr in {"matmul", "dot"} and len(node.args) >= 2:
                    r1 = self._rank_expr(node.args[0])
                    r2 = self._rank_expr(node.args[1])
                    if node.func.attr == "dot" and r1 == 1 and r2 == 1:
                        return 0
                    if r1 == 2 and r2 == 2:
                        return 2
                    if (r1 == 2 and r2 == 1) or (r1 == 1 and r2 == 2):
                        return 1
                    return 0
                if node.func.attr == "solve" and len(node.args) >= 2:
                    return self._rank_expr(node.args[1])
                if node.func.attr == "det" and len(node.args) >= 1:
                    return 0
                if node.func.attr == "inv" and len(node.args) >= 1:
                    return 2
                if node.func.attr == "trace" and len(node.args) >= 1:
                    return 0
                if node.func.attr == "outer" and len(node.args) >= 2:
                    return 2
                if node.func.attr == "kron" and len(node.args) >= 2:
                    r1 = self._rank_expr(node.args[0])
                    r2 = self._rank_expr(node.args[1])
                    if r1 <= 1 and r2 <= 1:
                        return 1
                    return 2
                if node.func.attr in {"nonzero", "setdiff1d", "intersect1d", "lexsort"} and len(node.args) >= 1:
                    return 1
                if node.func.attr == "ravel_multi_index" and len(node.args) >= 1:
                    return 0
                if node.func.attr == "argwhere" and len(node.args) >= 1:
                    r0 = self._rank_expr(node.args[0])
                    return 2 if r0 <= 1 else r0
                if node.func.attr == "unravel_index" and len(node.args) >= 2:
                    return 1
                if node.func.attr in {"sin", "cos", "tan", "mod", "floor_divide", "bitwise_and", "bitwise_or", "bitwise_xor"} and len(node.args) >= 1:
                    return self._rank_expr(node.args[0])
                if node.func.attr in {"cumsum", "cumprod", "repeat", "unique"} and len(node.args) >= 1:
                    return 1
                if node.func.attr == "tile" and len(node.args) >= 1:
                    r0 = self._rank_expr(node.args[0])
                    if len(node.args) >= 2 and isinstance(node.args[1], (ast.Tuple, ast.List)):
                        return max(2, r0)
                    return max(1, r0)
                if node.func.attr in {"hstack", "vstack", "column_stack", "concatenate"} and len(node.args) >= 1:
                    seq = node.args[0]
                    if not (isinstance(seq, (ast.Tuple, ast.List)) and seq.elts):
                        return 1
                    r0 = self._rank_expr(seq.elts[0])
                    if node.func.attr == "column_stack":
                        return 2
                    if node.func.attr == "hstack":
                        return 1 if r0 <= 1 else 2
                    if node.func.attr == "vstack":
                        return 2
                    if node.func.attr == "concatenate":
                        if r0 <= 1:
                            return 1
                        return 2
                if node.func.attr in {"all", "any", "prod", "count_nonzero"} and len(node.args) >= 1:
                    r0 = self._rank_expr(node.args[0])
                    axis_node = None
                    keepdims = False
                    for kw in node.keywords:
                        if kw.arg == "axis":
                            axis_node = kw.value
                        elif kw.arg == "keepdims":
                            keepdims = bool(isinstance(kw.value, ast.Constant) and kw.value.value is True)
                    if axis_node is None:
                        return 0
                    return r0 if keepdims else max(0, r0 - 1)
                if node.func.attr in {"mean", "var", "std", "nanmean", "nanvar", "nanstd", "nanmin", "nanmax"} and len(node.args) >= 1:
                    r0 = self._rank_expr(node.args[0])
                    axis_node = None
                    keepdims = False
                    for kw in node.keywords:
                        if kw.arg == "axis":
                            axis_node = kw.value
                        elif kw.arg == "keepdims":
                            keepdims = bool(isinstance(kw.value, ast.Constant) and kw.value.value is True)
                    if axis_node is None:
                        return 0
                    return r0 if keepdims else max(0, r0 - 1)
                if node.func.attr in {"nanargmin", "nanargmax"} and len(node.args) >= 1:
                    r0 = self._rank_expr(node.args[0])
                    axis_node = None
                    for kw in node.keywords:
                        if kw.arg == "axis":
                            axis_node = kw.value
                            break
                    if axis_node is None:
                        return 0
                    return max(0, r0 - 1)
                if node.func.attr == "diag" and len(node.args) >= 1:
                    r0 = self._rank_expr(node.args[0])
                    if r0 <= 1:
                        return 2
                    return 1
                if node.func.attr in {"triu", "tril"} and len(node.args) >= 1:
                    return self._rank_expr(node.args[0])
                if node.func.attr == "stack" and len(node.args) >= 1:
                    seq = node.args[0]
                    if isinstance(seq, (ast.Tuple, ast.List)) and len(seq.elts) >= 1:
                        return max(1, self._rank_expr(seq.elts[0]) + 1)
                    return max(1, self._rank_expr(node.args[0]) + 1)
                if node.func.attr in {"sum", "max", "min"} and len(node.args) >= 1:
                    r0 = self._rank_expr(node.args[0])
                    axis_node = None
                    keepdims = False
                    for kw in node.keywords:
                        if kw.arg == "axis":
                            axis_node = kw.value
                        elif kw.arg == "keepdims":
                            keepdims = bool(isinstance(kw.value, ast.Constant) and kw.value.value is True)
                    if axis_node is None:
                        return 0
                    return r0 if keepdims else max(0, r0 - 1)
            if (
                isinstance(node.func, ast.Attribute)
                and node.func.attr in {"ravel", "flatten", "squeeze", "argmin", "argmax", "min", "max", "copy"}
                and len(node.args) == 0
            ):
                if node.func.attr in {"ravel", "flatten"}:
                    return 1
                if node.func.attr == "squeeze":
                    return 1
                if node.func.attr in {"argmin", "argmax", "min", "max"}:
                    return 0
                return self._rank_expr(node.func.value)
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Attribute)
                and isinstance(node.func.value.value, ast.Name)
                and node.func.value.value.id == "np"
                and node.func.value.attr == "linalg"
            ):
                if node.func.attr == "solve" and len(node.args) >= 2:
                    return self._rank_expr(node.args[1])
                if node.func.attr == "det" and len(node.args) >= 1:
                    return 0
                if node.func.attr == "inv" and len(node.args) >= 1:
                    return 2
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.attr in {"sum", "mean", "var", "max", "min"}
            ):
                r0 = self._rank_expr(node.func.value)
                axis_node = None
                keepdims = False
                for kw in node.keywords:
                    if kw.arg == "axis":
                        axis_node = kw.value
                    elif kw.arg == "keepdims":
                        keepdims = bool(isinstance(kw.value, ast.Constant) and kw.value.value is True)
                if axis_node is None:
                    return 0
                return r0 if keepdims else max(0, r0 - 1)
            if isinstance(node.func, ast.Attribute) and node.func.attr == "reshape":
                if len(node.args) == 1 and isinstance(node.args[0], (ast.Tuple, ast.List)):
                    return max(1, len(node.args[0].elts))
                if len(node.args) >= 1:
                    return max(1, len(node.args))
                return self._rank_expr(node.func.value)
            if isinstance(node.func, ast.Attribute) and node.func.attr in {"normal", "uniform", "integers"}:
                size_node = None
                if len(node.args) >= 1 and node.func.attr == "normal":
                    size_node = node.args[0]
                if len(node.args) >= 3 and node.func.attr in {"uniform", "integers"}:
                    size_node = node.args[2]
                for kw in node.keywords:
                    if kw.arg == "size":
                        size_node = kw.value
                        break
                if size_node is None:
                    return 0
                if isinstance(size_node, (ast.Tuple, ast.List)):
                    return max(1, len(size_node.elts))
                return 1
            if isinstance(node.func, ast.Attribute) and node.func.attr == "dot" and len(node.args) >= 1:
                r1 = self._rank_expr(node.func.value)
                r2 = self._rank_expr(node.args[0])
                if r1 == 2 and r2 == 2:
                    return 2
                if (r1 == 2 and r2 == 1) or (r1 == 1 and r2 == 2):
                    return 1
                return 0
            if isinstance(node.func, ast.Attribute) and node.func.attr == "trace" and len(node.args) == 0:
                return 0
            if isinstance(node.func, ast.Attribute) and node.func.attr == "kron" and len(node.args) >= 1:
                r1 = self._rank_expr(node.func.value)
                r2 = self._rank_expr(node.args[0])
                if r1 <= 1 and r2 <= 1:
                    return 1
                return 2
            if isinstance(node.func, ast.Attribute) and node.func.attr == "astype":
                return self._rank_expr(node.func.value)
            if isinstance(node.func, ast.Attribute) and node.func.attr == "tolist":
                return self._rank_expr(node.func.value)
            if isinstance(node.func, ast.Attribute) and node.func.attr in {"ravel", "flatten"}:
                return 1
            if isinstance(node.func, ast.Attribute) and node.func.attr == "copy" and len(node.args) == 0:
                return self._rank_expr(node.func.value)
            if isinstance(node.func, ast.Attribute) and node.func.attr == "tolist" and len(node.args) == 0:
                return self._rank_expr(node.func.value)
            if isinstance(node.func, ast.Attribute) and node.func.attr == "transpose":
                if len(node.args) == 1 and isinstance(node.args[0], (ast.Tuple, ast.List)):
                    return max(1, len(node.args[0].elts))
                if len(node.args) >= 1:
                    return max(1, len(node.args))
                return self._rank_expr(node.func.value)
        return 0

    def _is_row2_expr(self, node):
        if isinstance(node, ast.Name) and node.id in self.broadcast_row2:
            return True
        if isinstance(node, ast.UnaryOp):
            return self._is_row2_expr(node.operand)
        if isinstance(node, ast.IfExp):
            return self._is_row2_expr(node.body) and self._is_row2_expr(node.orelse)
        if isinstance(node, ast.BinOp):
            l = self._is_row2_expr(node.left)
            r = self._is_row2_expr(node.right)
            if l and (r or self._rank_expr(node.right) == 0):
                return True
            if r and self._rank_expr(node.left) == 0:
                return True
            return False
        if isinstance(node, ast.Call):
            if (
                isinstance(node.func, ast.Attribute)
                and node.args
                and node.func.attr in {"log", "exp", "sqrt", "maximum", "asarray", "array", "sum", "max"}
            ):
                return self._is_row2_expr(node.args[0])
            if isinstance(node.func, ast.Name) and node.args and node.func.id in {"reshape", "spread", "sum", "maxval"}:
                return self._is_row2_expr(node.args[0])
        if (
            isinstance(node, ast.Subscript)
            and isinstance(node.slice, ast.Tuple)
            and len(node.slice.elts) == 2
        ):
            a0, a1 = node.slice.elts
            full1 = isinstance(a1, ast.Slice) and a1.lower is None and a1.upper is None and a1.step is None
            if is_none(a0) and full1:
                return True
        if (
            isinstance(node, ast.Call)
            and isinstance(node.func, ast.Name)
            and node.func.id == "reshape"
            and len(node.args) >= 2
            and isinstance(node.args[1], ast.List)
            and len(node.args[1].elts) == 2
        ):
            a0 = node.args[1].elts[0]
            return isinstance(a0, ast.Constant) and a0.value == 1
        return False

    def _is_col2_expr(self, node):
        if isinstance(node, ast.Name) and node.id in self.broadcast_col2:
            return True
        if isinstance(node, ast.UnaryOp):
            return self._is_col2_expr(node.operand)
        if isinstance(node, ast.IfExp):
            return self._is_col2_expr(node.body) and self._is_col2_expr(node.orelse)
        if isinstance(node, ast.BinOp):
            l = self._is_col2_expr(node.left)
            r = self._is_col2_expr(node.right)
            if l and (r or self._rank_expr(node.right) == 0):
                return True
            if r and self._rank_expr(node.left) == 0:
                return True
            return False
        if isinstance(node, ast.Call):
            if (
                isinstance(node.func, ast.Attribute)
                and node.args
                and node.func.attr in {"log", "exp", "sqrt", "maximum", "asarray", "array", "sum", "max"}
            ):
                return self._is_col2_expr(node.args[0])
            if isinstance(node.func, ast.Name) and node.args and node.func.id in {"reshape", "spread", "sum", "maxval"}:
                return self._is_col2_expr(node.args[0])
        if (
            isinstance(node, ast.Subscript)
            and isinstance(node.slice, ast.Tuple)
            and len(node.slice.elts) == 2
        ):
            a0, a1 = node.slice.elts
            full0 = isinstance(a0, ast.Slice) and a0.lower is None and a0.upper is None and a0.step is None
            if full0 and is_none(a1):
                return True
        if (
            isinstance(node, ast.Call)
            and isinstance(node.func, ast.Name)
            and node.func.id == "reshape"
            and len(node.args) >= 2
            and isinstance(node.args[1], ast.List)
            and len(node.args[1].elts) == 2
        ):
            a1 = node.args[1].elts[1]
            return isinstance(a1, ast.Constant) and a1.value == 1
        return False

    def _shape_anchor_2d(self, node):
        """Return a stable rank-2 expression text to query extents from."""
        if self._rank_expr(node) != 2:
            return self.expr(node)
        if isinstance(node, ast.Name):
            return self.expr(node)
        if isinstance(node, ast.Subscript):
            return self.expr(node)
        if isinstance(node, ast.Call):
            return self.expr(node)
        if isinstance(node, ast.UnaryOp):
            return self._shape_anchor_2d(node.operand)
        if isinstance(node, ast.IfExp):
            b = self._shape_anchor_2d(node.body)
            if self._rank_expr(node.body) == 2:
                return b
            return self._shape_anchor_2d(node.orelse)
        if isinstance(node, ast.BinOp):
            cands = [node.left, node.right]
            for c in cands:
                if self._rank_expr(c) == 2 and (not self._is_row2_expr(c)) and (not self._is_col2_expr(c)):
                    return self._shape_anchor_2d(c)
            for c in cands:
                if self._rank_expr(c) == 2:
                    return self._shape_anchor_2d(c)
        return self.expr(node)

    def _reshape_dims_exprs(self, arr_expr, dim_nodes):
        """
        Build Fortran shape expressions for reshape(...), supporting one
        NumPy-style inferred dimension (-1).
        """
        dims = []
        infer_pos = []
        def _is_infer_minus_one(n):
            if isinstance(n, ast.Constant) and isinstance(n.value, int) and int(n.value) == -1:
                return True
            return (
                isinstance(n, ast.UnaryOp)
                and isinstance(n.op, ast.USub)
                and isinstance(n.operand, ast.Constant)
                and isinstance(n.operand.value, int)
                and int(n.operand.value) == 1
            )
        for i, dn in enumerate(dim_nodes):
            if _is_infer_minus_one(dn):
                dims.append(None)
                infer_pos.append(i)
            else:
                dims.append(self.expr(dn))
        if len(infer_pos) > 1:
            raise NotImplementedError("reshape supports at most one inferred (-1) dimension")
        if len(infer_pos) == 1:
            others = [d for d in dims if d is not None]
            if others:
                denom = " * ".join(f"int({d})" for d in others)
                inferred = f"(size({arr_expr}) / ({denom}))"
            else:
                inferred = f"size({arr_expr})"
            dims[infer_pos[0]] = inferred
        return dims

    def _decl_rank_expr(self, node):
        """Rank from declarations/allocs only (ignores broadcast row/col markers)."""
        if isinstance(node, ast.Name):
            if node.id in self.alloc_reals:
                return self.alloc_real_rank.get(node.id, 1)
            if node.id in self.alloc_ints:
                return self.alloc_int_rank.get(node.id, 1)
            if node.id in self.alloc_logs:
                return self.alloc_log_rank.get(node.id, 1)
            return 0
        return self._rank_expr(node)

    def expr(self, node):
        def _module_attr_root_name(n):
            cur = n
            while isinstance(cur, ast.Attribute):
                cur = cur.value
            if isinstance(cur, ast.Name):
                return cur.id
            return None

        if isinstance(node, ast.Name):
            return node.id

        if isinstance(node, ast.Constant):
            v = node.value
            if isinstance(v, bool):
                return ".true." if v else ".false."
            if isinstance(v, int):
                return str(v)
            if isinstance(v, float):
                return f"{repr(v)}_dp"
            if isinstance(v, str):
                return fstr(v)
            if v is None:
                return "-1"
            raise NotImplementedError("unsupported constant")

        if isinstance(node, ast.List):
            if node.elts and all(isinstance(e, ast.List) for e in node.elts):
                nrow = len(node.elts)
                ncol = len(node.elts[0].elts)
                if not all(len(e.elts) == ncol for e in node.elts):
                    raise NotImplementedError("nested list rows must have equal lengths")
                flat_nodes = []
                for r in node.elts:
                    flat_nodes.extend(r.elts)
                vals = ", ".join(self.expr(e) for e in flat_nodes)
                return f"transpose(reshape([{vals}], [{ncol}, {nrow}]))"
            vals = ", ".join(self.expr(e) for e in node.elts)
            return f"[{vals}]"

        if isinstance(node, ast.Tuple):
            vals = ", ".join(self.expr(e) for e in node.elts)
            return f"[{vals}]"

        if isinstance(node, ast.BinOp):
            op = type(node.op)
            opmap = {
                ast.Add: "+",
                ast.Sub: "-",
                ast.Mult: "*",
                ast.Div: "/",
                ast.FloorDiv: "/",
                ast.Mod: "mod",
                ast.Pow: "**",
                ast.MatMult: "matmul",
                ast.BitAnd: ".and.",
                ast.BitOr: ".or.",
                ast.LShift: "lshift",
                ast.RShift: "rshift",
            }
            if op not in opmap:
                raise NotImplementedError("unsupported binop")
            a0 = self.expr(node.left)
            b0 = self.expr(node.right)
            a = a0
            b = b0
            # Limited NumPy-style broadcasting:
            # - rank-2 with rank-2 where one side is known row/col vector
            # - rank-2 with rank-1 (NumPy trailing-dimension vector broadcast)
            if self._rank_expr(node.left) == 2 and self._rank_expr(node.right) == 2:
                lshape = self._shape_anchor_2d(node.left)
                rshape = self._shape_anchor_2d(node.right)
                def _size_dim(n, shape_expr, dim):
                    if self._decl_rank_expr(n) <= 1:
                        return f"size({self.expr(n)})"
                    return f"size({shape_expr},{dim})"
                l_col = self._is_col2_expr(node.left)
                l_row = self._is_row2_expr(node.left)
                r_col = self._is_col2_expr(node.right)
                r_row = self._is_row2_expr(node.right)

                # If both are already aligned row-vectors or col-vectors, no spread needed.
                if not ((l_row and r_row) or (l_col and r_col)):
                    if l_col and not r_col:
                        if isinstance(node.left, ast.Subscript):
                            abase = self.expr(node.left.value)
                            a = f"spread({abase}, dim=2, ncopies={_size_dim(node.right, rshape, 2)})"
                        else:
                            if self._decl_rank_expr(node.left) <= 1:
                                a = f"spread({a0}, dim=2, ncopies={_size_dim(node.right, rshape, 2)})"
                            else:
                                a = f"spread(reshape({a0}, [size({a0},1)]), dim=2, ncopies={_size_dim(node.right, rshape, 2)})"
                    elif l_row and not r_row:
                        if isinstance(node.left, ast.Subscript):
                            abase = self.expr(node.left.value)
                            a = f"spread({abase}, dim=1, ncopies={_size_dim(node.right, rshape, 1)})"
                        else:
                            if self._decl_rank_expr(node.left) <= 1:
                                a = f"spread({a0}, dim=1, ncopies={_size_dim(node.right, rshape, 1)})"
                            else:
                                a = f"spread(reshape({a0}, [size({a0},2)]), dim=1, ncopies={_size_dim(node.right, rshape, 1)})"

                    if r_col and not l_col:
                        if isinstance(node.right, ast.Subscript):
                            bbase = self.expr(node.right.value)
                            b = f"spread({bbase}, dim=2, ncopies={_size_dim(node.left, lshape, 2)})"
                        else:
                            if self._decl_rank_expr(node.right) <= 1:
                                b = f"spread({b0}, dim=2, ncopies={_size_dim(node.left, lshape, 2)})"
                            else:
                                b = f"spread(reshape({b0}, [size({b0},1)]), dim=2, ncopies={_size_dim(node.left, lshape, 2)})"
                    elif r_row and not l_row:
                        if isinstance(node.right, ast.Subscript):
                            bbase = self.expr(node.right.value)
                            b = f"spread({bbase}, dim=1, ncopies={_size_dim(node.left, lshape, 1)})"
                        else:
                            if self._decl_rank_expr(node.right) <= 1:
                                b = f"spread({b0}, dim=1, ncopies={_size_dim(node.left, lshape, 1)})"
                            else:
                                b = f"spread(reshape({b0}, [size({b0},2)]), dim=1, ncopies={_size_dim(node.left, lshape, 1)})"
            elif self._rank_expr(node.left) == 2 and self._rank_expr(node.right) == 1:
                if self._is_col2_expr(node.right):
                    b = f"spread({b0}, dim=2, ncopies=size({a0},2))"
                else:
                    # Default NumPy behavior for (n,m) op (m,) is row-wise broadcast.
                    b = f"spread({b0}, dim=1, ncopies=size({a0},1))"
            elif self._rank_expr(node.left) == 1 and self._rank_expr(node.right) == 2:
                if self._is_col2_expr(node.left):
                    a = f"spread({a0}, dim=2, ncopies=size({b0},2))"
                else:
                    a = f"spread({a0}, dim=1, ncopies=size({b0},1))"
            if op is ast.MatMult:
                return f"matmul({a}, {b})"
            if op is ast.Mod:
                return f"mod({a}, {b})"
            if op is ast.LShift:
                return f"ishft({a}, int({b}))"
            if op is ast.RShift:
                return f"ishft({a}, -int({b}))"
            return f"({a} {opmap[op]} {b})"

        if isinstance(node, ast.UnaryOp):
            if isinstance(node.op, ast.USub):
                return f"(-{self.expr(node.operand)})"
            if isinstance(node.op, ast.UAdd):
                return f"(+{self.expr(node.operand)})"
            if isinstance(node.op, ast.Not):
                return f"(.not. {self.expr(node.operand)})"
            raise NotImplementedError("unsupported unary op")

        if isinstance(node, ast.BoolOp):
            if len(node.values) < 2:
                return self.expr(node.values[0])
            if isinstance(node.op, ast.And):
                op = ".and."
            elif isinstance(node.op, ast.Or):
                op = ".or."
            else:
                raise NotImplementedError("unsupported boolean operator")
            parts = [self.expr(v) for v in node.values]
            return "(" + f" {op} ".join(parts) + ")"

        if isinstance(node, ast.Compare):
            if len(node.ops) != 1 or len(node.comparators) != 1:
                raise NotImplementedError("chained compares not supported")
            op = type(node.ops[0])
            if op is ast.Is or op is ast.IsNot:
                a = node.left
                b = node.comparators[0]
                def _dict_none_test(name):
                    comps = self.dict_var_components.get(name, [])
                    if not comps:
                        return None
                    return f"(.not. allocated({name}%{comps[0]}))"
                if is_none(a):
                    if isinstance(b, ast.Name) and b.id in self.dict_typed_vars:
                        t = _dict_none_test(b.id)
                        if t is not None:
                            return t if op is ast.Is else f"(.not. {t})"
                    left = self.expr(b)
                    return f"({left} == -1)" if op is ast.Is else f"({left} /= -1)"
                if is_none(b):
                    if isinstance(a, ast.Name) and a.id in self.dict_typed_vars:
                        t = _dict_none_test(a.id)
                        if t is not None:
                            return t if op is ast.Is else f"(.not. {t})"
                    left = self.expr(a)
                    return f"({left} == -1)" if op is ast.Is else f"({left} /= -1)"
                raise NotImplementedError("is/is not supported only with None")
            opmap = {ast.Lt: "<", ast.LtE: "<=", ast.Gt: ">", ast.GtE: ">=", ast.Eq: "==", ast.NotEq: "/="}
            if op not in opmap:
                raise NotImplementedError("unsupported compare op")
            a0 = self.expr(node.left)
            b0 = self.expr(node.comparators[0])
            a = a0
            b = b0
            # Limited broadcasting for comparisons: rank-2 with rank-1.
            if self._rank_expr(node.left) == 2 and self._rank_expr(node.comparators[0]) == 1:
                if self._is_col2_expr(node.comparators[0]):
                    b = f"spread({b0}, dim=2, ncopies=size({a0},2))"
                else:
                    b = f"spread({b0}, dim=1, ncopies=size({a0},1))"
            elif self._rank_expr(node.left) == 1 and self._rank_expr(node.comparators[0]) == 2:
                if self._is_col2_expr(node.left):
                    a = f"spread({a0}, dim=2, ncopies=size({b0},2))"
                else:
                    a = f"spread({a0}, dim=1, ncopies=size({b0},1))"
            return f"({a} {opmap[op]} {b})"

        if isinstance(node, ast.Subscript):
            # 1D np.nonzero(...) compatibility: nz is stored as index vector;
            # map nz[0] back to that vector.
            if (
                isinstance(node.value, ast.Name)
                and node.value.id in self.nonzero_tuple_vars
                and isinstance(node.slice, ast.Constant)
                and isinstance(node.slice.value, int)
                and int(node.slice.value) == 0
            ):
                return node.value.id
            # simple dict alias lookup: d["key"] -> mapped expression
            if (
                isinstance(node.value, ast.Name)
                and node.value.id in self.dict_aliases
                and isinstance(node.slice, ast.Constant)
                and isinstance(node.slice.value, str)
            ):
                key = node.slice.value
                if key in self.dict_aliases[node.value.id]:
                    return self.dict_aliases[node.value.id][key]
            if (
                isinstance(node.value, ast.Name)
                and node.value.id in self.dict_typed_vars
                and isinstance(node.slice, ast.Constant)
                and isinstance(node.slice.value, str)
            ):
                return f"{node.value.id}%{node.slice.value}"
            base = self.expr(node.value)
            base_name = node.value.id if isinstance(node.value, ast.Name) else base
            base_size_expr = self.list_counts.get(base_name, f"size({base_name})")
            base_rank = self._rank_expr(node.value)
            if base_rank > 1:
                base_first_dim_size_expr = f"size({base_name},1)" if isinstance(node.value, ast.Name) else f"size({base},1)"
            else:
                base_first_dim_size_expr = base_size_expr
            if isinstance(node.slice, ast.Slice):
                if node.slice.lower is None and node.slice.upper is None and node.slice.step is None:
                    return base
                trip = self._slice_triplet(node.slice, base_size_expr)
                return f"{base}({trip})"
            if isinstance(node.slice, ast.Tuple) and len(node.slice.elts) == 2:
                a0, a1 = node.slice.elts
                full0 = isinstance(a0, ast.Slice) and a0.lower is None and a0.upper is None and a0.step is None
                full1 = isinstance(a1, ast.Slice) and a1.lower is None and a1.upper is None and a1.step is None
                none0 = is_none(a0)
                none1 = is_none(a1)
                # NumPy style axis insertion for 1D vectors.
                if none0 and full1:
                    return f"reshape({base}, [1, size({base_name})])"
                if full0 and none1:
                    return f"reshape({base}, [size({base_name}), 1])"
                if full0 and full1:
                    return base
                # General 2D slicing/indexing with Python 0-based to Fortran 1-based mapping.
                if isinstance(a0, ast.Slice) or isinstance(a1, ast.Slice):
                    d0 = self._slice_triplet(a0, f"size({base_name},1)") if isinstance(a0, ast.Slice) else f"({self.expr(a0)} + 1)"
                    d1 = self._slice_triplet(a1, f"size({base_name},2)") if isinstance(a1, ast.Slice) else f"({self.expr(a1)} + 1)"
                    return f"{base}({d0}, {d1})"
                # 2D indexing with explicit index/vector.
                if full0 and (not full1) and (not none1):
                    return f"{base}(:, ({self.expr(a1)} + 1))"
                if full1 and (not full0) and (not none0):
                    return f"{base}(({self.expr(a0)} + 1), :)"
                # Scalar 2D indexing: a[i, j] -> a(i+1, j+1)
                if (not isinstance(a0, ast.Slice)) and (not isinstance(a1, ast.Slice)) and (not none0) and (not none1):
                    return f"{base}(({self.expr(a0)} + 1), ({self.expr(a1)} + 1))"
                raise NotImplementedError("only [None,:] and [:,None] tuple subscripts are supported")
            if isinstance(node.slice, ast.Tuple) and len(node.slice.elts) == 3:
                a0, a1, a2 = node.slice.elts
                if all(not isinstance(a, ast.Slice) and not is_none(a) for a in (a0, a1, a2)):
                    return (
                        f"{base}(({self.expr(a0)} + 1), "
                        f"({self.expr(a1)} + 1), "
                        f"({self.expr(a2)} + 1))"
                    )
                raise NotImplementedError("only scalar 3D tuple subscripts are supported")
            # Logical mask indexing (NumPy): a[mask] -> pack(a, mask)
            slice_rank = self._rank_expr(node.slice)
            if slice_rank > 0 and self._expr_kind(node.slice) == "logical":
                return f"pack({base}, {self.expr(node.slice)})"
            # Integer/vector indexing for rank-1 arrays.
            if slice_rank > 0 and self._rank_expr(node.value) == 1:
                return f"{base}(({self.expr(node.slice)} + 1))"
            # Python emitters in this project use 0-based forms like (i)-1;
            # map those back to natural Fortran 1-based indexing.
            if (
                isinstance(node.slice, ast.BinOp)
                and isinstance(node.slice.op, ast.Sub)
                and isinstance(node.slice.right, ast.Constant)
                and isinstance(node.slice.right.value, int)
                and node.slice.right.value == 1
            ):
                idx = self.expr(node.slice.left)
            # Handle negative indexing (e.g. p[-1]) as Python tail indexing.
            elif (
                isinstance(node.slice, ast.UnaryOp)
                and isinstance(node.slice.op, ast.USub)
                and isinstance(node.slice.operand, ast.Constant)
                and isinstance(node.slice.operand.value, int)
                and node.slice.operand.value >= 1
            ):
                k = node.slice.operand.value
                if k == 1:
                    idx = base_first_dim_size_expr
                else:
                    idx = f"({base_first_dim_size_expr} - {k - 1})"
            else:
                # Python indexing is 0-based; default scalar subscripts map to
                # Fortran 1-based indices.
                idx = f"({self.expr(node.slice)} + 1)"
            if base_rank > 1:
                trailing = ", ".join(":" for _ in range(base_rank - 1))
                return f"{base}({idx}, {trailing})"
            return f"{base}({idx})"

        if isinstance(node, ast.Call):
            if isinstance(node.func, ast.Attribute) and node.func.attr == "copy" and len(node.args) == 0:
                return self.expr(node.func.value)
            if isinstance(node.func, ast.Attribute) and node.func.attr == "tolist" and len(node.args) == 0:
                return self.expr(node.func.value)
            # Method-call reductions: a.sum(), a.mean(), a.var(ddof=...)
            if (
                isinstance(node.func, ast.Attribute)
                and not (
                    _module_attr_root_name(node.func.value) in {"np", "math", "random"}
                )
            ):
                base_expr = self.expr(node.func.value)
                attr = node.func.attr
                if attr == "sum":
                    axis_node = None
                    keepdims = False
                    for kw in getattr(node, "keywords", []):
                        if kw.arg == "axis":
                            axis_node = kw.value
                        elif kw.arg == "keepdims":
                            keepdims = bool(isinstance(kw.value, ast.Constant) and kw.value.value is True)
                    if axis_node is None:
                        return f"sum({base_expr})"
                    dim_expr = f"({self.expr(axis_node)} + 1)"
                    reduced = f"sum({base_expr}, dim={dim_expr})"
                    if keepdims:
                        return f"spread({reduced}, dim={dim_expr}, ncopies=1)"
                    return reduced
                if attr == "mean":
                    axis_node = None
                    keepdims = False
                    for kw in getattr(node, "keywords", []):
                        if kw.arg == "axis":
                            axis_node = kw.value
                        elif kw.arg == "keepdims":
                            keepdims = bool(isinstance(kw.value, ast.Constant) and kw.value.value is True)
                    if axis_node is None:
                        if self._expr_kind(node.func.value) == "logical":
                            return f"(real(count({base_expr}), kind=dp) / real(size({base_expr}), kind=dp))"
                        return f"mean_1d({base_expr})"
                    dim_expr = f"({self.expr(axis_node)} + 1)"
                    reduced = f"(sum({base_expr}, dim={dim_expr}) / real(size({base_expr}, dim={dim_expr}), kind=dp))"
                    if keepdims:
                        return f"spread({reduced}, dim={dim_expr}, ncopies=1)"
                    return reduced
                if attr == "var":
                    ddof_node = None
                    axis_node = None
                    for kw in getattr(node, "keywords", []):
                        if kw.arg == "ddof":
                            ddof_node = kw.value
                        elif kw.arg == "axis":
                            axis_node = kw.value
                    if axis_node is not None:
                        raise NotImplementedError("var(..., axis=...) not yet supported")
                    if ddof_node is None:
                        return f"var_1d({base_expr})"
                    return f"var_1d({base_expr}, {self.expr(ddof_node)})"
                if attr == "min":
                    axis_node = None
                    keepdims = False
                    for kw in getattr(node, "keywords", []):
                        if kw.arg == "axis":
                            axis_node = kw.value
                        elif kw.arg == "keepdims":
                            keepdims = bool(isinstance(kw.value, ast.Constant) and kw.value.value is True)
                    if axis_node is None:
                        return f"minval({base_expr})"
                    dim_expr = f"({self.expr(axis_node)} + 1)"
                    reduced = f"minval({base_expr}, dim={dim_expr})"
                    if keepdims:
                        return f"spread({reduced}, dim={dim_expr}, ncopies=1)"
                    return reduced
                if attr == "max":
                    axis_node = None
                    keepdims = False
                    for kw in getattr(node, "keywords", []):
                        if kw.arg == "axis":
                            axis_node = kw.value
                        elif kw.arg == "keepdims":
                            keepdims = bool(isinstance(kw.value, ast.Constant) and kw.value.value is True)
                    if axis_node is None:
                        return f"maxval({base_expr})"
                    dim_expr = f"({self.expr(axis_node)} + 1)"
                    reduced = f"maxval({base_expr}, dim={dim_expr})"
                    if keepdims:
                        return f"spread({reduced}, dim={dim_expr}, ncopies=1)"
                    return reduced
                if attr == "argmin":
                    axis_node = None
                    for kw in getattr(node, "keywords", []):
                        if kw.arg == "axis":
                            axis_node = kw.value
                            break
                    if axis_node is None:
                        return f"(minloc({base_expr}, dim=1) - 1)"
                    return f"(minloc({base_expr}, dim=({self.expr(axis_node)} + 1)) - 1)"
                if attr == "argmax":
                    axis_node = None
                    for kw in getattr(node, "keywords", []):
                        if kw.arg == "axis":
                            axis_node = kw.value
                            break
                    if axis_node is None:
                        return f"(maxloc({base_expr}, dim=1) - 1)"
                    return f"(maxloc({base_expr}, dim=({self.expr(axis_node)} + 1)) - 1)"
                if attr == "ravel":
                    return f"reshape({base_expr}, [size({base_expr})])"
                if attr == "flatten":
                    return f"reshape({base_expr}, [size({base_expr})])"
                if attr == "transpose":
                    rank0 = self._rank_expr(node.func.value)
                    if len(node.args) == 0:
                        if rank0 <= 1:
                            return base_expr
                        if rank0 == 2:
                            return f"transpose({base_expr})"
                        if rank0 == 3:
                            return (
                                f"reshape({base_expr}, "
                                f"[size({base_expr},3), size({base_expr},2), size({base_expr},1)], "
                                f"order=[3,2,1])"
                            )
                        raise NotImplementedError("transpose() without axes supports rank up to 3")
                    if len(node.args) == 1 and isinstance(node.args[0], (ast.Tuple, ast.List)):
                        axes_nodes = list(node.args[0].elts)
                    else:
                        axes_nodes = list(node.args)
                    if not axes_nodes:
                        return base_expr
                    if not all(isinstance(a, ast.Constant) and isinstance(a.value, int) for a in axes_nodes):
                        raise NotImplementedError("transpose axes must be integer constants")
                    perm = [int(a.value) + 1 for a in axes_nodes]
                    shp = ", ".join([f"size({base_expr},{p})" for p in perm])
                    ordp = ", ".join(str(p) for p in perm)
                    return f"reshape({base_expr}, [{shp}], order=[{ordp}])"
                if attr == "dot" and len(node.args) >= 1:
                    a1 = self.expr(node.args[0])
                    if self._rank_expr(node.func.value) == 1 and self._rank_expr(node.args[0]) == 1:
                        return f"dot_product({base_expr}, {a1})"
                    return f"matmul({base_expr}, {a1})"
                if attr == "kron" and len(node.args) >= 1:
                    a1 = self.expr(node.args[0])
                    if self._rank_expr(node.func.value) <= 1 and self._rank_expr(node.args[0]) <= 1:
                        return f"reshape(spread({a1}, dim=1, ncopies=size({base_expr})) * spread({base_expr}, dim=2, ncopies=size({a1})), [size({base_expr})*size({a1})])"
                    raise NotImplementedError("method kron currently supports only 1D arrays")

            if isinstance(node.func, ast.Name) and node.func.id == "isqrt":
                return f"isqrt_int({self.expr(node.args[0])})"
            if isinstance(node.func, ast.Name) and node.func.id == "int" and len(node.args) == 1:
                return f"int({self.expr(node.args[0])})"
            if isinstance(node.func, ast.Name) and node.func.id == "float" and len(node.args) == 1:
                if is_const_str(node.args[0]) and str(node.args[0].value).lower() == "nan":
                    return "ieee_value(0.0_dp, ieee_quiet_nan)"
                return f"real({self.expr(node.args[0])}, kind=dp)"
            if isinstance(node.func, ast.Name) and node.func.id == "str" and len(node.args) == 1:
                # Print-oriented transpilation: treat str(x) as display no-op.
                return self.expr(node.args[0])
            if isinstance(node.func, ast.Name) and node.func.id == "bool" and len(node.args) == 1:
                a0 = node.args[0]
                e0 = self.expr(a0)
                k0 = self._expr_kind(a0)
                r0 = self._rank_expr(a0)
                if k0 == "logical":
                    # np.all/np.any already return logical values.
                    return e0
                if r0 > 0:
                    return f"any({e0} /= 0)"
                return f"({e0} /= 0)"
            if isinstance(node.func, ast.Name) and node.func.id == "len" and len(node.args) == 1:
                a0 = node.args[0]
                if isinstance(a0, ast.Name) and a0.id in self.list_counts:
                    return self.list_counts[a0.id]
                return f"size({self.expr(a0)})"
            if isinstance(node.func, ast.Name):
                args_nodes = list(node.args)
                for kw in getattr(node, "keywords", []):
                    if kw.arg is None:
                        raise NotImplementedError("**kwargs not supported")
                    args_nodes.append(kw.value)
                args = ", ".join(self.expr(a) for a in args_nodes)
                return f"{node.func.id}({args})"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "asarray"
                and len(node.args) >= 1
            ):
                dtype_txt = ""
                for kw in node.keywords:
                    if kw.arg == "dtype":
                        if isinstance(kw.value, ast.Name):
                            dtype_txt = kw.value.id.lower()
                        elif (
                            isinstance(kw.value, ast.Attribute)
                            and isinstance(kw.value.value, ast.Name)
                            and kw.value.value.id == "np"
                        ):
                            dtype_txt = kw.value.attr.lower()
                a0 = self.expr(node.args[0])
                if "float" in dtype_txt:
                    return f"real({a0}, kind=dp)"
                if "int" in dtype_txt:
                    return f"int({a0})"
                return a0
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in {"ascontiguousarray", "asfortranarray"}
                and len(node.args) >= 1
            ):
                return self.expr(node.args[0])
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in {"array", "asarray"}
                and len(node.args) >= 1
                and isinstance(node.args[0], ast.List)
            ):
                elts = node.args[0].elts
                if elts and all(isinstance(e, ast.List) for e in elts):
                    nrow = len(elts)
                    ncol = len(elts[0].elts)
                    if not all(len(e.elts) == ncol for e in elts):
                        raise NotImplementedError("np.array nested list rows must have equal lengths")
                    flat_nodes = []
                    for r in elts:
                        flat_nodes.extend(r.elts)
                    vals = ", ".join(self.expr(e) for e in flat_nodes)
                    return f"transpose(reshape([{vals}], [{ncol}, {nrow}]))"
                vals = ", ".join(self.expr(e) for e in elts)
                return f"[{vals}]"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in {"array", "asarray"}
                and len(node.args) >= 1
                and isinstance(node.args[0], ast.Name)
                and node.args[0].id in self.list_counts
            ):
                src = node.args[0].id
                cnt = self.list_counts[src]
                dtype_txt = ""
                for kw in node.keywords:
                    if kw.arg == "dtype":
                        if isinstance(kw.value, ast.Name):
                            dtype_txt = kw.value.id.lower()
                        elif (
                            isinstance(kw.value, ast.Attribute)
                            and isinstance(kw.value.value, ast.Name)
                            and kw.value.value.id == "np"
                        ):
                            dtype_txt = kw.value.attr.lower()
                if "float" in dtype_txt:
                    return f"real({src}(1:{cnt}), kind=dp)"
                if "int" in dtype_txt:
                    return f"int({src}(1:{cnt}))"
                return f"{src}(1:{cnt})"
            # Generic np.array(...) / np.asarray(...) fallback for scalar or
            # non-list inputs. Preserve dtype coercion when explicitly set.
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in {"array", "asarray"}
                and len(node.args) >= 1
            ):
                a0 = self.expr(node.args[0])
                dtype_txt = ""
                for kw in node.keywords:
                    if kw.arg == "dtype":
                        if isinstance(kw.value, ast.Name):
                            dtype_txt = kw.value.id.lower()
                        elif (
                            isinstance(kw.value, ast.Attribute)
                            and isinstance(kw.value.value, ast.Name)
                            and kw.value.value.id == "np"
                        ):
                            dtype_txt = kw.value.attr.lower()
                if "float" in dtype_txt:
                    return f"real({a0}, kind=dp)"
                if "int" in dtype_txt:
                    return f"int({a0})"
                if "bool" in dtype_txt:
                    if self._expr_kind(node.args[0]) == "logical":
                        return a0
                    return f"({a0} /= 0)"
                return a0
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "max"
                and len(node.args) >= 1
            ):
                a0 = self.expr(node.args[0])
                axis_node = None
                keepdims = False
                for kw in node.keywords:
                    if kw.arg == "axis":
                        axis_node = kw.value
                    elif kw.arg == "keepdims":
                        keepdims = bool(isinstance(kw.value, ast.Constant) and kw.value.value is True)
                if axis_node is None:
                    return f"maxval({a0})"
                dim_expr = f"({self.expr(axis_node)} + 1)"
                reduced = f"maxval({a0}, dim={dim_expr})"
                if keepdims:
                    return f"spread({reduced}, dim={dim_expr}, ncopies=1)"
                return reduced
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "nanmax"
                and len(node.args) >= 1
            ):
                a0 = self.expr(node.args[0])
                axis_node = None
                keepdims = False
                for kw in node.keywords:
                    if kw.arg == "axis":
                        axis_node = kw.value
                    elif kw.arg == "keepdims":
                        keepdims = bool(isinstance(kw.value, ast.Constant) and kw.value.value is True)
                if axis_node is None:
                    return f"nanmax(reshape({a0}, [size({a0})]))"
                dim_expr = f"({self.expr(axis_node)} + 1)"
                reduced = f"maxval(merge({a0}, (-huge(1.0_dp)), (.not. ieee_is_nan({a0}))), dim={dim_expr})"
                if keepdims:
                    return f"spread({reduced}, dim={dim_expr}, ncopies=1)"
                return reduced
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "sum"
                and len(node.args) >= 1
            ):
                a0 = self.expr(node.args[0])
                axis_node = None
                keepdims = False
                for kw in node.keywords:
                    if kw.arg == "axis":
                        axis_node = kw.value
                    elif kw.arg == "keepdims":
                        keepdims = bool(isinstance(kw.value, ast.Constant) and kw.value.value is True)
                if axis_node is None:
                    return f"sum({a0})"
                dim_expr = f"({self.expr(axis_node)} + 1)"
                reduced = f"sum({a0}, dim={dim_expr})"
                if keepdims:
                    return f"spread({reduced}, dim={dim_expr}, ncopies=1)"
                return reduced
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "log"
                and len(node.args) == 1
            ):
                return f"log({self.expr(node.args[0])})"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "log2"
                and len(node.args) == 1
            ):
                return f"log2({self.expr(node.args[0])})"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "log10"
                and len(node.args) == 1
            ):
                return f"(log({self.expr(node.args[0])}) / log(10.0_dp))"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "exp"
                and len(node.args) == 1
            ):
                return f"exp({self.expr(node.args[0])})"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "maximum"
                and len(node.args) == 2
            ):
                return f"max({self.expr(node.args[0])}, {self.expr(node.args[1])})"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "minimum"
                and len(node.args) == 2
            ):
                return f"min({self.expr(node.args[0])}, {self.expr(node.args[1])})"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "reshape"
                and len(node.args) >= 2
            ):
                a0 = self.expr(node.args[0])
                if isinstance(node.args[1], (ast.Tuple, ast.List)):
                    dim_nodes = list(node.args[1].elts)
                else:
                    dim_nodes = list(node.args[1:])
                if not dim_nodes:
                    raise NotImplementedError("np.reshape requires shape arguments")
                dims = ", ".join(self._reshape_dims_exprs(a0, dim_nodes))
                return f"reshape({a0}, [{dims}])"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in {"zeros", "ones"}
                and len(node.args) >= 1
            ):
                n_expr = self.expr(node.args[0])
                dtype_txt = self._np_dtype_text(node)
                if "int" in dtype_txt:
                    fn = "zeros_int" if node.func.attr == "zeros" else "ones_int"
                elif "bool" in dtype_txt:
                    fn = "zeros_logical" if node.func.attr == "zeros" else "ones_logical"
                else:
                    fn = "zeros_real" if node.func.attr == "zeros" else "ones_real"
                return f"{fn}(int({n_expr}))"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in {"zeros_like", "ones_like"}
                and len(node.args) >= 1
            ):
                a0 = self.expr(node.args[0])
                k0 = self._expr_kind(node.args[0])
                if k0 == "int":
                    fn = "zeros_int" if node.func.attr == "zeros_like" else "ones_int"
                elif k0 == "logical":
                    fn = "zeros_logical" if node.func.attr == "zeros_like" else "ones_logical"
                else:
                    fn = "zeros_real" if node.func.attr == "zeros_like" else "ones_real"
                return f"{fn}(size({a0}))"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "stack"
                and len(node.args) >= 1
            ):
                seq = node.args[0]
                if not isinstance(seq, (ast.Tuple, ast.List)) or len(seq.elts) < 1:
                    raise NotImplementedError("np.stack requires tuple/list inputs")
                axis = 0
                for kw in node.keywords:
                    if kw.arg == "axis" and isinstance(kw.value, ast.Constant) and isinstance(kw.value.value, int):
                        axis = int(kw.value.value)
                        break
                vals = ", ".join(self.expr(e) for e in seq.elts)
                n = len(seq.elts)
                first = self.expr(seq.elts[0])
                r0 = self._rank_expr(seq.elts[0])
                if r0 <= 1:
                    if axis < 0:
                        axis = axis + 2
                    if axis < 0 or axis > 1:
                        raise NotImplementedError("np.stack axis out of range for 1D inputs")
                    if axis == 1:
                        return f"reshape([{vals}], [size({first}), {n}])"
                    return f"transpose(reshape([{vals}], [size({first}), {n}]))"
                # rank>=2: build stacked-last array then permute axis with RESHAPE order
                if axis < 0:
                    axis = axis + (r0 + 1)
                if axis < 0 or axis > r0:
                    raise NotImplementedError("np.stack axis out of range")
                src_shape = ", ".join([f"size({first},{i})" for i in range(1, r0 + 1)] + [str(n)])
                src = f"reshape([{vals}], [{src_shape}])"
                if axis == r0:
                    return src
                perm = list(range(1, r0 + 1))
                perm.insert(axis, r0 + 1)
                tgt_shape_parts = []
                for p in perm:
                    if p == r0 + 1:
                        tgt_shape_parts.append(str(n))
                    else:
                        tgt_shape_parts.append(f"size({first},{p})")
                tgt_shape = ", ".join(tgt_shape_parts)
                ordp = ", ".join(str(p) for p in perm)
                return f"reshape({src}, [{tgt_shape}], order=[{ordp}])"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in {"hstack", "vstack", "column_stack", "concatenate"}
                and len(node.args) >= 1
            ):
                seq = node.args[0]
                if not isinstance(seq, (ast.Tuple, ast.List)) or len(seq.elts) < 1:
                    raise NotImplementedError(f"{node.func.attr} requires tuple/list input")
                axis = 0
                if node.func.attr in {"hstack", "column_stack"}:
                    axis = 1
                for kw in node.keywords:
                    if kw.arg == "axis" and isinstance(kw.value, ast.Constant) and isinstance(kw.value.value, int):
                        axis = int(kw.value.value)
                r0 = self._rank_expr(seq.elts[0])
                vals = ", ".join(self.expr(e) for e in seq.elts)
                if node.func.attr == "column_stack":
                    first = self.expr(seq.elts[0])
                    return f"transpose(reshape([{vals}], [size({first}), {len(seq.elts)}]))"
                if r0 <= 1:
                    if node.func.attr == "vstack":
                        first = self.expr(seq.elts[0])
                        return f"transpose(reshape([{vals}], [size({first}), {len(seq.elts)}]))"
                    return f"[{vals}]"
                if axis == 1:
                    first = self.expr(seq.elts[0])
                    cols = " + ".join([f"size({self.expr(e)},2)" for e in seq.elts])
                    return f"reshape([{vals}], [size({first},1), {cols}])"
                rows = " + ".join([f"size({self.expr(e)},1)" for e in seq.elts])
                first = self.expr(seq.elts[0])
                tvals = ", ".join([f"transpose({self.expr(e)})" for e in seq.elts])
                return f"transpose(reshape([{tvals}], [size({first},2), {rows}]))"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in {"transpose", "identity"}
                and len(node.args) >= 1
            ):
                if node.func.attr == "identity":
                    return f"eye(int({self.expr(node.args[0])}), int({self.expr(node.args[0])}))"
                a0 = self.expr(node.args[0])
                if len(node.args) == 1:
                    return f"transpose({a0})"
                axes = node.args[1]
                if not isinstance(axes, (ast.Tuple, ast.List)):
                    raise NotImplementedError("np.transpose axes must be tuple/list")
                if not all(isinstance(a, ast.Constant) and isinstance(a.value, int) for a in axes.elts):
                    raise NotImplementedError("np.transpose axes must be integer constants")
                perm = [int(a.value) + 1 for a in axes.elts]
                shp = ", ".join([f"size({a0},{p})" for p in perm])
                ordp = ", ".join(str(p) for p in perm)
                return f"reshape({a0}, [{shp}], order=[{ordp}])"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "swapaxes"
                and len(node.args) >= 3
            ):
                a0 = self.expr(node.args[0])
                rank0 = self._rank_expr(node.args[0])
                if not (isinstance(node.args[1], ast.Constant) and isinstance(node.args[1].value, int)):
                    raise NotImplementedError("swapaxes axis1 must be integer constant")
                if not (isinstance(node.args[2], ast.Constant) and isinstance(node.args[2].value, int)):
                    raise NotImplementedError("swapaxes axis2 must be integer constant")
                ax1 = int(node.args[1].value)
                ax2 = int(node.args[2].value)
                if rank0 <= 0:
                    return a0
                perm = list(range(1, rank0 + 1))
                if not (0 <= ax1 < rank0 and 0 <= ax2 < rank0):
                    raise NotImplementedError("swapaxes axes out of range")
                perm[ax1], perm[ax2] = perm[ax2], perm[ax1]
                shp = ", ".join([f"size({a0},{p})" for p in perm])
                ordp = ", ".join(str(p) for p in perm)
                return f"reshape({a0}, [{shp}], order=[{ordp}])"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "broadcast_to"
                and len(node.args) >= 2
            ):
                a0 = self.expr(node.args[0])
                r0 = self._rank_expr(node.args[0])
                shp = node.args[1]
                if not isinstance(shp, (ast.Tuple, ast.List)):
                    raise NotImplementedError("np.broadcast_to shape must be tuple/list in this transpiler path")
                dims = [self.expr(e) for e in shp.elts]
                if len(dims) == 1:
                    n0 = dims[0]
                    if r0 == 0:
                        return f"[( {a0}, i_bt = 1, ({n0}) )]"
                    if r0 == 1:
                        return f"{a0}(1:{n0})"
                    return f"reshape({a0}, [{n0}])"
                if len(dims) == 2:
                    n0, n1 = dims[0], dims[1]
                    if r0 == 0:
                        return f"reshape([( {a0}, i_bt = 1, ({n0})*({n1}) )], [{n0}, {n1}])"
                    if r0 == 1:
                        if self._is_col2_expr(node.args[0]):
                            return f"spread({a0}, dim=2, ncopies={n1})"
                        return f"spread({a0}, dim=1, ncopies={n0})"
                    if r0 == 2:
                        return (
                            f"merge("
                            f"spread({a0}(1,:), dim=1, ncopies={n0}), "
                            f"spread({a0}(:,1), dim=2, ncopies={n1}), "
                            f"(size({a0},1) == 1))"
                        )
                raise NotImplementedError("np.broadcast_to currently supports target rank up to 2")
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in {"dot", "matmul"}
                and len(node.args) >= 2
            ):
                a0 = self.expr(node.args[0])
                a1 = self.expr(node.args[1])
                if node.func.attr == "dot" and self._rank_expr(node.args[0]) == 1 and self._rank_expr(node.args[1]) == 1:
                    return f"dot_product({a0}, {a1})"
                return f"matmul({a0}, {a1})"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Attribute)
                and isinstance(node.func.value.value, ast.Name)
                and node.func.value.value.id == "np"
                and node.func.value.attr == "linalg"
                and node.func.attr == "solve"
                and len(node.args) >= 2
            ):
                return f"linalg_solve({self.expr(node.args[0])}, {self.expr(node.args[1])})"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Attribute)
                and isinstance(node.func.value.value, ast.Name)
                and node.func.value.value.id == "np"
                and node.func.value.attr == "linalg"
                and node.func.attr == "det"
                and len(node.args) >= 1
            ):
                return f"linalg_det({self.expr(node.args[0])})"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Attribute)
                and isinstance(node.func.value.value, ast.Name)
                and node.func.value.value.id == "np"
                and node.func.value.attr == "linalg"
                and node.func.attr == "inv"
                and len(node.args) >= 1
            ):
                return f"linalg_inv({self.expr(node.args[0])})"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Attribute)
                and isinstance(node.func.value.value, ast.Name)
                and node.func.value.value.id == "np"
                and node.func.value.attr == "linalg"
                and node.func.attr == "norm"
                and len(node.args) >= 1
            ):
                a0 = self.expr(node.args[0])
                ord_node = None
                for kw in node.keywords:
                    if kw.arg == "ord":
                        ord_node = kw.value
                        break
                if ord_node is None:
                    return f"sqrt(sum(({a0})**2))"
                return f"sum(abs({a0}))"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "trace"
                and len(node.args) >= 1
            ):
                a0 = self.expr(node.args[0])
                return f"sum(diag({a0}))"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "einsum"
                and len(node.args) >= 2
                and is_const_str(node.args[0])
                and node.args[0].value == "ii->"
            ):
                a0 = self.expr(node.args[1])
                return f"sum(diag({a0}))"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "outer"
                and len(node.args) >= 2
            ):
                a0 = self.expr(node.args[0])
                a1 = self.expr(node.args[1])
                return f"spread({a0}, dim=2, ncopies=size({a1})) * spread({a1}, dim=1, ncopies=size({a0}))"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "kron"
                and len(node.args) >= 2
            ):
                a0 = self.expr(node.args[0])
                a1 = self.expr(node.args[1])
                if self._rank_expr(node.args[0]) <= 1 and self._rank_expr(node.args[1]) <= 1:
                    return f"reshape(spread({a1}, dim=1, ncopies=size({a0})) * spread({a0}, dim=2, ncopies=size({a1})), [size({a0})*size({a1})])"
                if self._rank_expr(node.args[0]) == 2 and self._rank_expr(node.args[1]) == 2:
                    return f"kron_2d({a0}, {a1})"
                raise NotImplementedError("np.kron currently supports only 1D/2D arrays")
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in {"sin", "cos", "tan"}
                and len(node.args) >= 1
            ):
                return f"{node.func.attr}({self.expr(node.args[0])})"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "mod"
                and len(node.args) >= 2
            ):
                return f"mod({self.expr(node.args[0])}, {self.expr(node.args[1])})"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "floor_divide"
                and len(node.args) >= 2
            ):
                return f"({self.expr(node.args[0])} / {self.expr(node.args[1])})"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in {"bitwise_and", "bitwise_or", "bitwise_xor"}
                and len(node.args) >= 2
            ):
                fn = {"bitwise_and": "iand", "bitwise_or": "ior", "bitwise_xor": "ieor"}[node.func.attr]
                return f"{fn}({self.expr(node.args[0])}, {self.expr(node.args[1])})"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "inner"
                and len(node.args) >= 2
            ):
                return f"dot_product({self.expr(node.args[0])}, {self.expr(node.args[1])})"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in {"all", "any", "prod", "count_nonzero"}
                and len(node.args) >= 1
            ):
                a0 = self.expr(node.args[0])
                axis_node = None
                keepdims = False
                for kw in node.keywords:
                    if kw.arg == "axis":
                        axis_node = kw.value
                    elif kw.arg == "keepdims":
                        keepdims = bool(isinstance(kw.value, ast.Constant) and kw.value.value is True)
                if node.func.attr == "all":
                    op = "all"
                    if self._expr_kind(node.args[0]) == "logical":
                        arr = a0
                    else:
                        arr = f"({a0} /= 0)"
                elif node.func.attr == "any":
                    op = "any"
                    if self._expr_kind(node.args[0]) == "logical":
                        arr = a0
                    else:
                        arr = f"({a0} /= 0)"
                elif node.func.attr == "prod":
                    op = "product"
                    arr = a0
                else:
                    op = "count"
                    arr = f"({a0} /= 0)"
                if axis_node is None:
                    return f"{op}({arr})"
                dim_expr = f"({self.expr(axis_node)} + 1)"
                reduced = f"{op}({arr}, dim={dim_expr})"
                if keepdims:
                    return f"spread({reduced}, dim={dim_expr}, ncopies=1)"
                return reduced
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "min"
                and len(node.args) >= 1
            ):
                a0 = self.expr(node.args[0])
                axis_node = None
                keepdims = False
                for kw in node.keywords:
                    if kw.arg == "axis":
                        axis_node = kw.value
                    elif kw.arg == "keepdims":
                        keepdims = bool(isinstance(kw.value, ast.Constant) and kw.value.value is True)
                if axis_node is None:
                    return f"minval({a0})"
                dim_expr = f"({self.expr(axis_node)} + 1)"
                reduced = f"minval({a0}, dim={dim_expr})"
                if keepdims:
                    return f"spread({reduced}, dim={dim_expr}, ncopies=1)"
                return reduced
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "nanmin"
                and len(node.args) >= 1
            ):
                a0 = self.expr(node.args[0])
                axis_node = None
                keepdims = False
                for kw in node.keywords:
                    if kw.arg == "axis":
                        axis_node = kw.value
                    elif kw.arg == "keepdims":
                        keepdims = bool(isinstance(kw.value, ast.Constant) and kw.value.value is True)
                if axis_node is None:
                    return f"nanmin(reshape({a0}, [size({a0})]))"
                dim_expr = f"({self.expr(axis_node)} + 1)"
                reduced = f"minval(merge({a0}, huge(1.0_dp), (.not. ieee_is_nan({a0}))), dim={dim_expr})"
                if keepdims:
                    return f"spread({reduced}, dim={dim_expr}, ncopies=1)"
                return reduced
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in {"argmax", "argmin"}
                and len(node.args) >= 1
            ):
                a0 = self.expr(node.args[0])
                axis_node = None
                for kw in node.keywords:
                    if kw.arg == "axis":
                        axis_node = kw.value
                        break
                fn = "maxloc" if node.func.attr == "argmax" else "minloc"
                if axis_node is None:
                    return f"({fn}(reshape({a0}, [size({a0})]), dim=1) - 1)"
                dim_expr = f"({self.expr(axis_node)} + 1)"
                return f"({fn}({a0}, dim={dim_expr}) - 1)"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in {"nanargmax", "nanargmin"}
                and len(node.args) >= 1
            ):
                a0 = self.expr(node.args[0])
                axis_node = None
                for kw in node.keywords:
                    if kw.arg == "axis":
                        axis_node = kw.value
                        break
                if axis_node is None:
                    if node.func.attr == "nanargmax":
                        return f"nanargmax(reshape({a0}, [size({a0})]))"
                    return f"nanargmin(reshape({a0}, [size({a0})]))"
                dim_expr = f"({self.expr(axis_node)} + 1)"
                if node.func.attr == "nanargmax":
                    return f"(maxloc(merge({a0}, (-huge(1.0_dp)), (.not. ieee_is_nan({a0}))), dim={dim_expr}) - 1)"
                return f"(minloc(merge({a0}, huge(1.0_dp), (.not. ieee_is_nan({a0}))), dim={dim_expr}) - 1)"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in {"isfinite", "isinf", "isnan"}
                and len(node.args) == 1
            ):
                a0 = self.expr(node.args[0])
                if node.func.attr == "isfinite":
                    return f"ieee_is_finite({a0})"
                if node.func.attr == "isinf":
                    return f"((.not. ieee_is_finite({a0})) .and. (.not. ieee_is_nan({a0})))"
                return f"ieee_is_nan({a0})"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "nansum"
                and len(node.args) >= 1
            ):
                a0 = self.expr(node.args[0])
                axis_node = None
                keepdims = False
                for kw in node.keywords:
                    if kw.arg == "axis":
                        axis_node = kw.value
                    elif kw.arg == "keepdims":
                        keepdims = bool(isinstance(kw.value, ast.Constant) and kw.value.value is True)
                arr = f"merge({a0}, 0.0_dp, (.not. ieee_is_nan({a0})))"
                if axis_node is None:
                    return f"sum({arr})"
                dim_expr = f"({self.expr(axis_node)} + 1)"
                reduced = f"sum({arr}, dim={dim_expr})"
                if keepdims:
                    return f"spread({reduced}, dim={dim_expr}, ncopies=1)"
                return reduced
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in {"abs", "fabs", "sign", "floor", "ceil", "round"}
                and len(node.args) >= 1
            ):
                a0 = self.expr(node.args[0])
                if node.func.attr in {"abs", "fabs"}:
                    return f"abs({a0})"
                if node.func.attr == "sign":
                    return f"sign(1.0_dp, {a0})"
                if node.func.attr == "floor":
                    return f"floor({a0})"
                if node.func.attr == "ceil":
                    return f"ceiling({a0})"
                if len(node.args) >= 2:
                    d = self.expr(node.args[1])
                    return f"(anint({a0} * (10.0_dp**{d})) / (10.0_dp**{d}))"
                return f"anint({a0})"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in {"full", "full_like", "clip", "linspace", "diff", "gradient"}
                and len(node.args) >= 1
            ):
                if node.func.attr == "full" and len(node.args) >= 2:
                    n_expr = self.expr(node.args[0])
                    fill = self.expr(node.args[1])
                    return f"({fill} + 0.0_dp*real(arange_int(0, int({n_expr}), 1), kind=dp))"
                if node.func.attr == "full_like" and len(node.args) >= 2:
                    a0 = self.expr(node.args[0])
                    fill = self.expr(node.args[1])
                    return f"({fill} + 0*{a0})"
                if node.func.attr == "clip":
                    a0 = self.expr(node.args[0])
                    lo = "(-huge(1.0_dp))"
                    hi = "huge(1.0_dp)"
                    if len(node.args) >= 2 and not is_none(node.args[1]):
                        lo = self.expr(node.args[1])
                    if len(node.args) >= 3 and not is_none(node.args[2]):
                        hi = self.expr(node.args[2])
                    return f"min(max({a0}, {lo}), {hi})"
                if node.func.attr == "linspace" and len(node.args) >= 3:
                    a0 = self.expr(node.args[0])
                    a1 = self.expr(node.args[1])
                    num = self.expr(node.args[2])
                    return (
                        f"({a0} + ({a1} - {a0}) * real(arange_int(0, int({num}), 1), kind=dp) / "
                        f"real(max(1, int({num}) - 1), kind=dp))"
                    )
                if node.func.attr == "diff":
                    a0 = self.expr(node.args[0])
                    axis = -1
                    for kw in node.keywords:
                        if kw.arg == "axis" and isinstance(kw.value, ast.Constant) and isinstance(kw.value.value, int):
                            axis = int(kw.value.value)
                    r0 = self._rank_expr(node.args[0])
                    if r0 <= 1:
                        return f"({a0}(2:) - {a0}(:size({a0})-1))"
                    if axis == 0:
                        return f"({a0}(2:,:) - {a0}(:size({a0},1)-1,:))"
                    return f"({a0}(:,2:) - {a0}(:,:size({a0},2)-1))"
                if node.func.attr == "gradient":
                    a0 = self.expr(node.args[0])
                    r0 = self._rank_expr(node.args[0])
                    if r0 <= 1:
                        return f"gradient_1d({a0})"
                    raise NotImplementedError("np.gradient currently supports only 1D arrays")
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "squeeze"
                and len(node.args) >= 1
            ):
                a0 = self.expr(node.args[0])
                return f"reshape({a0}, [size({a0})])"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "expand_dims"
                and len(node.args) >= 2
            ):
                a0 = self.expr(node.args[0])
                axis = 0
                if isinstance(node.args[1], ast.Constant) and isinstance(node.args[1].value, int):
                    axis = int(node.args[1].value)
                if axis == 0:
                    return f"reshape({a0}, [1, size({a0})])"
                return f"reshape({a0}, [size({a0}), 1])"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "arange"
                and len(node.args) >= 1
            ):
                if len(node.args) == 1:
                    start_expr = "0"
                    stop_expr = self.expr(node.args[0])
                    step_expr = "1"
                elif len(node.args) == 2:
                    start_expr = self.expr(node.args[0])
                    stop_expr = self.expr(node.args[1])
                    step_expr = "1"
                else:
                    start_expr = self.expr(node.args[0])
                    stop_expr = self.expr(node.args[1])
                    step_expr = self.expr(node.args[2])
                return f"arange_int(int({start_expr}), int({stop_expr}), int({step_expr}))"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "eye"
                and len(node.args) >= 1
            ):
                n_expr = self.expr(node.args[0])
                m_expr = n_expr
                if len(node.args) >= 2:
                    m_expr = self.expr(node.args[1])
                for kw in node.keywords:
                    if kw.arg in {"M", "m"}:
                        m_expr = self.expr(kw.value)
                        break
                return f"eye(int({n_expr}), int({m_expr}))"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "diag"
                and len(node.args) >= 1
            ):
                a0 = self.expr(node.args[0])
                k_node = None
                if len(node.args) >= 2:
                    k_node = node.args[1]
                for kw in node.keywords:
                    if kw.arg == "k":
                        k_node = kw.value
                        break
                if k_node is None:
                    return f"diag({a0})"
                if not (isinstance(k_node, ast.Constant) and isinstance(k_node.value, int)):
                    raise NotImplementedError("np.diag currently supports only constant integer k")
                k_val = int(k_node.value)
                if k_val == 0:
                    return f"diag({a0})"
                if k_val >= 0:
                    self._mark_int("i_d")
                    return (
                        f"[({a0}(i_d, i_d + ({k_val})), "
                        f"i_d = 1, min(size({a0},1), size({a0},2) - ({k_val})))]"
                    )
                kk = -k_val
                self._mark_int("i_d")
                return (
                    f"[({a0}(i_d + ({kk}), i_d), "
                    f"i_d = 1, min(size({a0},1) - ({kk}), size({a0},2)))]"
                )
            if (
                isinstance(node.func, ast.Attribute)
                and not (_module_attr_root_name(node.func.value) in {"np", "math", "random"})
            ):
                arr = self.expr(node.func.value)
                if node.func.attr == "reshape":
                    if len(node.args) == 1 and isinstance(node.args[0], (ast.Tuple, ast.List)):
                        dims = ", ".join(self._reshape_dims_exprs(arr, list(node.args[0].elts)))
                        return f"reshape({arr}, [{dims}])"
                    if len(node.args) >= 1:
                        dims = ", ".join(self._reshape_dims_exprs(arr, list(node.args)))
                        return f"reshape({arr}, [{dims}])"
                    raise NotImplementedError("reshape requires shape arguments")
                if node.func.attr == "astype":
                    dtype_txt = ""
                    if len(node.args) >= 1:
                        a0 = node.args[0]
                        if isinstance(a0, ast.Name):
                            dtype_txt = a0.id.lower()
                        elif (
                            isinstance(a0, ast.Attribute)
                            and isinstance(a0.value, ast.Name)
                            and a0.value.id == "np"
                        ):
                            dtype_txt = a0.attr.lower()
                    if "float" in dtype_txt:
                        return f"real({arr}, kind=dp)"
                    if "int" in dtype_txt:
                        if self._expr_kind(node.func.value) == "logical":
                            return f"merge(1, 0, {arr})"
                        return f"int({arr})"
                    if "bool" in dtype_txt:
                        return f"({arr} /= 0)"
                    return arr
                if node.func.attr == "mean" and len(node.args) == 0:
                    axis_node = None
                    keepdims = False
                    for kw in getattr(node, "keywords", []):
                        if kw.arg == "axis":
                            axis_node = kw.value
                        elif kw.arg == "keepdims":
                            keepdims = bool(isinstance(kw.value, ast.Constant) and kw.value.value is True)
                    if axis_node is None:
                        return f"(sum({arr}) / real(size({arr}), kind=dp))"
                    dim_expr = f"({self.expr(axis_node)} + 1)"
                    reduced = f"(sum({arr}, dim={dim_expr}) / real(size({arr}, dim={dim_expr}), kind=dp))"
                    if keepdims:
                        return f"spread({reduced}, dim={dim_expr}, ncopies=1)"
                    return reduced
                if node.func.attr == "std":
                    ddof = 0
                    for kw in node.keywords:
                        if kw.arg == "ddof":
                            if isinstance(kw.value, ast.Constant) and isinstance(kw.value.value, int):
                                ddof = kw.value.value
                    denom = f"max(1, size({arr}) - {ddof})"
                    mu = f"(sum({arr}) / real(size({arr}), kind=dp))"
                    return f"sqrt(sum(({arr} - {mu})**2) / real({denom}, kind=dp))"
                if node.func.attr == "min" and len(node.args) == 0:
                    return f"minval({arr})"
                if node.func.attr == "max" and len(node.args) == 0:
                    return f"maxval({arr})"
                if node.func.attr == "argmin" and len(node.args) == 0:
                    return f"(minloc({arr}, dim=1) - 1)"
                if node.func.attr == "argmax" and len(node.args) == 0:
                    return f"(maxloc({arr}, dim=1) - 1)"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "mean"
                and len(node.args) == 1
            ):
                a0 = self.expr(node.args[0])
                axis_node = None
                keepdims = False
                for kw in node.keywords:
                    if kw.arg == "axis":
                        axis_node = kw.value
                    elif kw.arg == "keepdims":
                        keepdims = bool(isinstance(kw.value, ast.Constant) and kw.value.value is True)
                if axis_node is None:
                    return f"mean({a0})"
                dim_expr = f"({self.expr(axis_node)} + 1)"
                reduced = f"(sum({a0}, dim={dim_expr}) / real(size({a0}, dim={dim_expr}), kind=dp))"
                if keepdims:
                    return f"spread({reduced}, dim={dim_expr}, ncopies=1)"
                return reduced
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "nanmean"
                and len(node.args) == 1
            ):
                a0 = self.expr(node.args[0])
                axis_node = None
                keepdims = False
                for kw in node.keywords:
                    if kw.arg == "axis":
                        axis_node = kw.value
                    elif kw.arg == "keepdims":
                        keepdims = bool(isinstance(kw.value, ast.Constant) and kw.value.value is True)
                if axis_node is None:
                    return f"nanmean(reshape({a0}, [size({a0})]))"
                dim_expr = f"({self.expr(axis_node)} + 1)"
                num = f"sum(merge({a0}, 0.0_dp, (.not. ieee_is_nan({a0}))), dim={dim_expr})"
                den = f"max(1, count((.not. ieee_is_nan({a0})), dim={dim_expr}))"
                reduced = f"({num} / real({den}, kind=dp))"
                if keepdims:
                    return f"spread({reduced}, dim={dim_expr}, ncopies=1)"
                return reduced
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "var"
                and len(node.args) == 1
            ):
                ddof_node = None
                for kw in node.keywords:
                    if kw.arg == "ddof":
                        ddof_node = kw.value
                        break
                a0 = self.expr(node.args[0])
                flat = f"reshape({a0}, [size({a0})])"
                if ddof_node is None:
                    return f"var_1d({flat})"
                return f"var_1d({flat}, {self.expr(ddof_node)})"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "nanvar"
                and len(node.args) == 1
            ):
                ddof_node = None
                axis_node = None
                for kw in node.keywords:
                    if kw.arg == "ddof":
                        ddof_node = kw.value
                    elif kw.arg == "axis":
                        axis_node = kw.value
                if axis_node is not None:
                    raise NotImplementedError("np.nanvar(..., axis=...) not yet supported")
                if ddof_node is None:
                    return f"nanvar(reshape({self.expr(node.args[0])}, [size({self.expr(node.args[0])})]))"
                return f"nanvar(reshape({self.expr(node.args[0])}, [size({self.expr(node.args[0])})]), {self.expr(ddof_node)})"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "std"
                and len(node.args) == 1
            ):
                ddof_node = None
                for kw in node.keywords:
                    if kw.arg == "ddof":
                        ddof_node = kw.value
                        break
                if ddof_node is None:
                    return f"std({self.expr(node.args[0])})"
                return f"std({self.expr(node.args[0])}, {self.expr(ddof_node)})"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "nanstd"
                and len(node.args) == 1
            ):
                ddof_node = None
                axis_node = None
                for kw in node.keywords:
                    if kw.arg == "ddof":
                        ddof_node = kw.value
                    elif kw.arg == "axis":
                        axis_node = kw.value
                if axis_node is not None:
                    raise NotImplementedError("np.nanstd(..., axis=...) not yet supported")
                if ddof_node is None:
                    return f"nanstd(reshape({self.expr(node.args[0])}, [size({self.expr(node.args[0])})]))"
                return f"nanstd(reshape({self.expr(node.args[0])}, [size({self.expr(node.args[0])})]), {self.expr(ddof_node)})"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr in {"cumsum", "cumprod", "repeat", "tile", "unique"}
                and len(node.args) >= 1
            ):
                a0 = self.expr(node.args[0])
                if node.func.attr in {"repeat", "tile"}:
                    reps = "1"
                    if len(node.args) >= 2:
                        if node.func.attr == "tile" and isinstance(node.args[1], (ast.Tuple, ast.List)) and len(node.args[1].elts) == 2:
                            r0 = self.expr(node.args[1].elts[0])
                            r1 = self.expr(node.args[1].elts[1])
                            return f"tile({a0}, int({r0}), int({r1}))"
                        reps = self.expr(node.args[1])
                    for kw in node.keywords:
                        if kw.arg in {"repeats", "reps"}:
                            reps = self.expr(kw.value)
                            break
                    return f"{node.func.attr}({a0}, int({reps}))"
                return f"{node.func.attr}({a0})"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "bincount"
                and len(node.args) >= 1
            ):
                a0 = self.expr(node.args[0])
                minlength = None
                if len(node.args) >= 2:
                    minlength = self.expr(node.args[1])
                for kw in node.keywords:
                    if kw.arg == "minlength":
                        minlength = self.expr(kw.value)
                        break
                if minlength is None:
                    return f"bincount_int({a0})"
                return f"bincount_int({a0}, int({minlength}))"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "searchsorted"
                and len(node.args) >= 2
            ):
                a0 = self.expr(node.args[0])
                v0 = self.expr(node.args[1])
                side = "left"
                for kw in node.keywords:
                    if kw.arg == "side" and isinstance(kw.value, ast.Constant) and isinstance(kw.value.value, str):
                        side = kw.value.value.lower()
                        break
                if len(node.args) >= 3 and isinstance(node.args[2], ast.Constant) and isinstance(node.args[2].value, str):
                    side = str(node.args[2].value).lower()
                if self._rank_expr(node.args[1]) == 0:
                    if side == "right":
                        return f"searchsorted_right_int_scalar({a0}, {v0})"
                    return f"searchsorted_left_int_scalar({a0}, {v0})"
                if side == "right":
                    return f"searchsorted_right_int({a0}, {v0})"
                return f"searchsorted_left_int({a0}, {v0})"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "nonzero"
                and len(node.args) >= 1
            ):
                a0 = self.expr(node.args[0])
                return f"pack(arange_int(int(0), int(size({a0})), int(1)), ({a0} /= 0))"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "argwhere"
                and len(node.args) >= 1
            ):
                a0 = self.expr(node.args[0])
                return f"reshape(pack(arange_int(int(0), int(size({a0})), int(1)), ({a0} /= 0)), [count(({a0} /= 0)), 1])"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "setdiff1d"
                and len(node.args) >= 2
            ):
                return f"setdiff1d_int({self.expr(node.args[0])}, {self.expr(node.args[1])})"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "intersect1d"
                and len(node.args) >= 2
            ):
                return f"intersect1d_int({self.expr(node.args[0])}, {self.expr(node.args[1])})"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "lexsort"
                and len(node.args) >= 1
                and isinstance(node.args[0], (ast.Tuple, ast.List))
                and len(node.args[0].elts) == 2
            ):
                ky = self.expr(node.args[0].elts[0])
                kx = self.expr(node.args[0].elts[1])
                return f"lexsort2_int({ky}, {kx})"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "ravel_multi_index"
                and len(node.args) >= 2
            ):
                return f"ravel_multi_index_2d({self.expr(node.args[0])}, {self.expr(node.args[1])})"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "unravel_index"
                and len(node.args) >= 2
            ):
                return f"unravel_index_2d(int({self.expr(node.args[0])}), {self.expr(node.args[1])})"
            if (
                isinstance(node.func, ast.Attribute)
                and node.func.attr == "reduceat"
                and isinstance(node.func.value, ast.Attribute)
                and isinstance(node.func.value.value, ast.Name)
                and node.func.value.value.id == "np"
                and len(node.args) >= 2
            ):
                op = node.func.value.attr
                helper = {
                    "add": "reduceat_add",
                    "multiply": "reduceat_mul",
                    "minimum": "reduceat_min",
                    "maximum": "reduceat_max",
                    "logical_and": "reduceat_logical_and",
                    "logical_or": "reduceat_logical_or",
                }.get(op)
                if helper is None:
                    raise NotImplementedError(f"np.{op}.reduceat not supported")
                a0 = self.expr(node.args[0])
                i0 = self.expr(node.args[1])
                return f"{helper}({a0}, {i0})"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "take"
                and len(node.args) >= 2
            ):
                a0 = self.expr(node.args[0])
                idx = self.expr(node.args[1])
                return f"{a0}({idx} + 1)"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "roll"
                and len(node.args) >= 1
            ):
                a0 = self.expr(node.args[0])
                sh = "0"
                if len(node.args) >= 2:
                    sh = self.expr(node.args[1])
                for kw in node.keywords:
                    if kw.arg == "shift":
                        sh = self.expr(kw.value)
                        break
                return f"cshift({a0}, int({sh}))"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "flip"
                and len(node.args) >= 1
            ):
                a0 = self.expr(node.args[0])
                return f"{a0}(size({a0}):1:-1)"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "pad"
                and len(node.args) >= 1
            ):
                # Narrow reusable subset:
                # 1D arrays, mode='constant', pad_width int or (left,right),
                # constant_values scalar or (left,right).
                a0 = self.expr(node.args[0])
                r0 = self._rank_expr(node.args[0])
                if r0 > 1:
                    raise NotImplementedError("np.pad currently supports only 1D arrays")
                mode_txt = "constant"
                cv_left = "0"
                cv_right = "0"
                pad_left = None
                pad_right = None
                pw = node.args[1] if len(node.args) >= 2 else None
                for kw in node.keywords:
                    if kw.arg == "pad_width":
                        pw = kw.value
                    elif kw.arg == "mode":
                        if is_const_str(kw.value):
                            mode_txt = str(kw.value.value).lower()
                    elif kw.arg == "constant_values":
                        cv = kw.value
                        if isinstance(cv, (ast.Tuple, ast.List)) and len(cv.elts) == 2:
                            cv_left = self.expr(cv.elts[0])
                            cv_right = self.expr(cv.elts[1])
                        else:
                            cv_left = self.expr(cv)
                            cv_right = self.expr(cv)
                if pw is None:
                    raise NotImplementedError("np.pad requires pad_width")
                if isinstance(pw, ast.Constant) and isinstance(pw.value, int):
                    pad_left = self.expr(pw)
                    pad_right = self.expr(pw)
                elif isinstance(pw, (ast.Tuple, ast.List)) and len(pw.elts) == 2:
                    pad_left = self.expr(pw.elts[0])
                    pad_right = self.expr(pw.elts[1])
                else:
                    raise NotImplementedError("np.pad pad_width must be int or 2-tuple for 1D")
                if mode_txt != "constant":
                    raise NotImplementedError("np.pad currently supports only mode='constant'")
                return (
                    f"[(({cv_left}), i_pad = 1, int({pad_left})), {a0}, "
                    f"(({cv_right}), i_pad = 1, int({pad_right}))]"
                )
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "math"
                and node.func.attr == "sqrt"
                and len(node.args) == 1
            ):
                return f"sqrt({self.expr(node.args[0])})"
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "random"
                and node.func.attr == "random"
                and len(node.args) == 0
            ):
                call_txt = ast.unparse(node) if hasattr(ast, "unparse") else ast.dump(node, include_attributes=False)
                raise NotImplementedError(
                    f"unsupported call in expression context: {call_txt} (supported as standalone assignment target)"
                )
            if (
                isinstance(node.func, ast.Attribute)
                and isinstance(node.func.value, ast.Name)
                and node.func.value.id == "np"
                and node.func.attr == "sqrt"
                and len(node.args) == 1
            ):
                return f"sqrt({self.expr(node.args[0])})"
            call_txt = ast.unparse(node) if hasattr(ast, "unparse") else ast.dump(node, include_attributes=False)
            raise NotImplementedError(f"unsupported call: {call_txt}")
        if isinstance(node, ast.Attribute):
            if node.attr in {"c_contiguous", "f_contiguous"}:
                if (
                    isinstance(node.value, ast.Attribute)
                    and node.value.attr == "flags"
                ):
                    return ".true."
            if node.attr == "size":
                return f"size({self.expr(node.value)})"
            if node.attr == "T":
                return f"transpose({self.expr(node.value)})"
            if node.attr == "shape":
                return f"shape({self.expr(node.value)})"
            if node.attr == "dtype":
                # Approximate NumPy dtype text from inferred symbol kind.
                if isinstance(node.value, ast.Name):
                    nm = node.value.id
                    if nm in self.alloc_ints or nm in self.ints:
                        return fstr("int")
                    if nm in self.alloc_reals or nm in self.reals:
                        return fstr("float")
                    if nm in self.alloc_logs or nm in self.logs:
                        return fstr("bool")
                return fstr("unknown")
            if isinstance(node.value, ast.Name) and node.value.id == "np" and node.attr == "pi":
                return "acos(-1.0_dp)"
            if isinstance(node.value, ast.Name) and node.value.id == "np" and node.attr == "nan":
                return "ieee_value(0.0_dp, ieee_quiet_nan)"
            if isinstance(node.value, ast.Name) and node.value.id == "np" and node.attr == "inf":
                return "huge(1.0_dp)"
            if isinstance(node.value, ast.Name) and node.value.id == "np" and node.attr == "NINF":
                return "(-huge(1.0_dp))"
            attr_txt = ast.unparse(node) if hasattr(ast, "unparse") else ast.dump(node, include_attributes=False)
            raise NotImplementedError(f"unsupported attribute expr: {attr_txt}")

        raise NotImplementedError(f"unsupported expr: {type(node).__name__}")

    def prescan(self, nodes):
        def _attr_root_name(n):
            cur = n
            while isinstance(cur, ast.Attribute):
                cur = cur.value
            if isinstance(cur, ast.Name):
                return cur.id
            return None

        for node in nodes:
            if isinstance(node, ast.AnnAssign):
                if isinstance(node.target, ast.Name):
                    tname = node.target.id
                    ann_txt = ast.unparse(node.annotation) if hasattr(ast, "unparse") else ""
                    low_ann = ann_txt.lower()
                    if "ndarray" in low_ann:
                        if "bool" in low_ann:
                            self._mark_alloc_log(tname)
                        else:
                            self._mark_alloc_int(tname)
                    else:
                        self._mark_int(tname)
                if node.value is not None:
                    fake = ast.Assign(targets=[node.target], value=node.value)
                    self.prescan([fake])
                continue

            if isinstance(node, ast.Assign):
                if (
                    len(node.targets) == 1
                    and isinstance(node.targets[0], (ast.Tuple, ast.List))
                    and isinstance(node.value, ast.Call)
                    and isinstance(node.value.func, ast.Name)
                    and node.value.func.id in self.tuple_return_funcs
                ):
                    out_kinds = self.tuple_return_out_kinds.get(node.value.func.id, [])
                    for j, e in enumerate(node.targets[0].elts):
                        if not isinstance(e, ast.Name):
                            continue
                        k = out_kinds[j] if j < len(out_kinds) else "int"
                        if k == "alloc_real":
                            self._mark_alloc_real(e.id)
                        elif k == "alloc_int":
                            self._mark_alloc_int(e.id)
                        elif k == "alloc_log":
                            self._mark_alloc_log(e.id)
                        elif k == "real":
                            self._mark_real(e.id)
                        else:
                            self._mark_int(e.id)
                    continue
                if (
                    len(node.targets) == 1
                    and isinstance(node.targets[0], (ast.Tuple, ast.List))
                    and isinstance(node.value, ast.Call)
                    and isinstance(node.value.func, ast.Attribute)
                    and isinstance(node.value.func.value, ast.Name)
                    and node.value.func.value.id == "np"
                    and node.value.func.attr == "meshgrid"
                ):
                    for e in node.targets[0].elts:
                        if isinstance(e, ast.Name):
                            self._mark_alloc_real(e.id, rank=2)
                    continue
                if (
                    len(node.targets) == 1
                    and isinstance(node.targets[0], (ast.Tuple, ast.List))
                    and isinstance(node.value, ast.Call)
                    and isinstance(node.value.func, ast.Attribute)
                    and isinstance(node.value.func.value, ast.Attribute)
                    and isinstance(node.value.func.value.value, ast.Name)
                    and node.value.func.value.value.id == "np"
                    and node.value.func.value.attr == "linalg"
                ):
                    outs = [e.id for e in node.targets[0].elts if isinstance(e, ast.Name)]
                    if node.value.func.attr == "eig" and len(outs) >= 2:
                        self._mark_alloc_real(outs[0], rank=1)
                        self._mark_alloc_real(outs[1], rank=2)
                        continue
                    if node.value.func.attr == "svd" and len(outs) >= 3:
                        self._mark_alloc_real(outs[0], rank=2)
                        self._mark_alloc_real(outs[1], rank=1)
                        self._mark_alloc_real(outs[2], rank=2)
                        continue
                if (
                    len(node.targets) == 1
                    and isinstance(node.targets[0], ast.Name)
                    and isinstance(node.value, ast.Call)
                    and isinstance(node.value.func, ast.Attribute)
                    and isinstance(node.value.func.value, ast.Name)
                    and node.value.func.value.id == "np"
                    and node.value.func.attr == "nonzero"
                ):
                    self._mark_alloc_int(node.targets[0].id, rank=1)
                    self.nonzero_tuple_vars.add(node.targets[0].id)
                    continue
                if (
                    len(node.targets) == 1
                    and isinstance(node.targets[0], (ast.Tuple, ast.List))
                    and isinstance(node.value, ast.Call)
                    and isinstance(node.value.func, ast.Attribute)
                    and isinstance(node.value.func.value, ast.Name)
                    and node.value.func.value.id == "np"
                    and node.value.func.attr == "histogram"
                ):
                    outs = [e.id for e in node.targets[0].elts if isinstance(e, ast.Name)]
                    if len(outs) >= 2:
                        self._mark_alloc_int(outs[0], rank=1)
                        bins_node = node.value.args[1] if len(node.value.args) >= 2 else None
                        for kw in node.value.keywords:
                            if kw.arg == "bins":
                                bins_node = kw.value
                                break
                        if bins_node is not None and self._expr_kind(bins_node) == "int":
                            self._mark_alloc_int(outs[1], rank=1)
                        else:
                            self._mark_alloc_real(outs[1], rank=1)
                        continue
                if (
                    len(node.targets) == 1
                    and isinstance(node.targets[0], (ast.Tuple, ast.List))
                    and isinstance(node.value, ast.Call)
                    and isinstance(node.value.func, ast.Attribute)
                    and isinstance(node.value.func.value, ast.Name)
                    and node.value.func.value.id == "np"
                    and node.value.func.attr == "unique"
                ):
                    inv = any(kw.arg == "return_inverse" and isinstance(kw.value, ast.Constant) and kw.value.value is True for kw in node.value.keywords)
                    cnt = any(kw.arg == "return_counts" and isinstance(kw.value, ast.Constant) and kw.value.value is True for kw in node.value.keywords)
                    outs = [e.id for e in node.targets[0].elts if isinstance(e, ast.Name)]
                    if inv and cnt and len(outs) >= 3:
                        self._mark_alloc_int(outs[0], rank=1)
                        self._mark_alloc_int(outs[1], rank=1)
                        self._mark_alloc_int(outs[2], rank=1)
                        continue
                if len(node.targets) != 1:
                    continue
                t = node.targets[0]
                v = node.value

                if isinstance(t, ast.Name) and is_const_int(v):
                    self._mark_int(t.id)

                if isinstance(t, ast.Name) and is_none(v):
                    self._mark_int(t.id)  # represent None as integer sentinel

                # name = other_name  (needed for e.g. largest = i)
                if isinstance(t, ast.Name) and isinstance(v, ast.Name):
                    k = self._expr_kind(v)
                    if k == "real":
                        self._mark_real(t.id)
                    elif k == "int":
                        self._mark_int(t.id)
                    elif k == "logical":
                        self._mark_log(t.id)
                if isinstance(t, ast.Name):
                    if (
                        isinstance(v, ast.Call)
                        and isinstance(v.func, ast.Attribute)
                        and isinstance(v.func.value, ast.Name)
                        and v.func.value.id == "np"
                        and v.func.attr == "diag"
                    ):
                        has_k = len(v.args) >= 2
                        if not has_k:
                            for kw in v.keywords:
                                if kw.arg == "k":
                                    has_k = True
                                    break
                        if has_k:
                            self._mark_int("i_d")
                    if (
                        isinstance(v, ast.Call)
                        and isinstance(v.func, ast.Attribute)
                        and isinstance(v.func.value, ast.Name)
                        and v.func.value.id == "np"
                        and v.func.attr == "pad"
                    ):
                        self._mark_int("i_pad")
                    # rng.choice(..., size=..., p=...) / rng.choice(..., size=..., replace=False)
                    if (
                        isinstance(v, ast.Call)
                        and isinstance(v.func, ast.Attribute)
                        and v.func.attr == "choice"
                    ):
                        has_size = False
                        has_p = False
                        replace_false = False
                        for kw in v.keywords:
                            if kw.arg == "size":
                                has_size = True
                            elif kw.arg == "p":
                                has_p = True
                            elif kw.arg == "replace" and isinstance(kw.value, ast.Constant) and kw.value.value is False:
                                replace_false = True
                        if has_size and (has_p or replace_false):
                            self._mark_alloc_int(t.id)
                            continue
                    if (
                        isinstance(v, ast.Subscript)
                        and isinstance(v.value, ast.Name)
                        and v.value.id in self.dict_typed_vars
                        and isinstance(v.slice, ast.Constant)
                        and isinstance(v.slice.value, str)
                    ):
                        tname = self.dict_typed_vars[v.value.id]
                        cname = v.slice.value
                        comps = self.dict_type_components.get(tname, {})
                        cinfo = comps.get(cname, "")
                        ckind = cinfo[0] if isinstance(cinfo, tuple) else cinfo
                        if ckind.startswith("real_array"):
                            self._mark_alloc_real(t.id)
                            continue
                        if ckind.startswith("int_array"):
                            self._mark_alloc_int(t.id)
                            continue
                        if ckind.startswith("logical_array"):
                            self._mark_alloc_log(t.id)
                            continue
                        if ckind == "real_scalar":
                            self._mark_real(t.id)
                            continue
                        if ckind == "int_scalar":
                            self._mark_int(t.id)
                            continue
                    ext = self._extent_expr(v)
                    k = self._expr_kind(v)
                    rk = max(1, self._rank_expr(v))
                    if self._rank_expr(v) > 0:
                        if k == "real":
                            self._mark_alloc_real(t.id, rank=rk)
                        elif k == "logical":
                            self._mark_alloc_log(t.id, rank=rk)
                        elif k == "int":
                            self._mark_alloc_int(t.id, rank=rk)
                        else:
                            self._mark_alloc_real(t.id, rank=rk)
                        # Keepdims reductions carry row/column broadcast intent.
                        if (
                            isinstance(v, ast.Call)
                            and isinstance(v.func, ast.Attribute)
                            and (
                                (
                                    isinstance(v.func.value, ast.Name)
                                    and v.func.value.id == "np"
                                    and v.func.attr in {"max", "sum", "mean", "min"}
                                )
                                or (
                                    not (_attr_root_name(v.func.value) in {"np", "math", "random"})
                                    and v.func.attr in {"mean", "sum", "max", "min"}
                                )
                            )
                        ):
                            axis_node = None
                            keepdims = False
                            for kw in v.keywords:
                                if kw.arg == "axis":
                                    axis_node = kw.value
                                elif kw.arg == "keepdims":
                                    keepdims = bool(isinstance(kw.value, ast.Constant) and kw.value.value is True)
                            if keepdims:
                                if isinstance(axis_node, ast.Constant) and isinstance(axis_node.value, int):
                                    if axis_node.value == 1:
                                        self.broadcast_col2.add(t.id)
                                    elif axis_node.value == 0:
                                        self.broadcast_row2.add(t.id)
                                else:
                                    self.broadcast_col2.add(t.id)
                        # Local helper reductions that keepdims by convention.
                        # logsumexp(a, axis, keepdims=True):
                        # axis=1 -> (n,1) column-shaped broadcast, axis=0 -> (1,m) row-shaped.
                        if (
                            isinstance(v, ast.Call)
                            and isinstance(v.func, ast.Name)
                            and v.func.id == "logsumexp"
                        ):
                            axis_val = None
                            keepdims_val = None
                            if len(v.args) >= 2 and isinstance(v.args[1], ast.Constant) and isinstance(v.args[1].value, int):
                                axis_val = v.args[1].value
                            if len(v.args) >= 3 and isinstance(v.args[2], ast.Constant) and isinstance(v.args[2].value, bool):
                                keepdims_val = v.args[2].value
                            for kw in getattr(v, "keywords", []):
                                if kw.arg == "axis" and isinstance(kw.value, ast.Constant) and isinstance(kw.value.value, int):
                                    axis_val = kw.value.value
                                elif kw.arg == "keepdims" and isinstance(kw.value, ast.Constant) and isinstance(kw.value.value, bool):
                                    keepdims_val = kw.value.value
                            if keepdims_val is True:
                                if axis_val == 1:
                                    self.broadcast_col2.add(t.id)
                                elif axis_val == 0:
                                    self.broadcast_row2.add(t.id)
                        continue
                    if ext is not None:
                        if k == "real":
                            self._mark_alloc_real(t.id, rank=rk)
                        elif k == "logical":
                            self._mark_alloc_log(t.id, rank=rk)
                        elif k == "int":
                            self._mark_alloc_int(t.id, rank=rk)
                        else:
                            self._mark_alloc_real(t.id, rank=rk)
                    elif k == "real":
                        self._mark_real(t.id)
                    elif k == "int":
                        self._mark_int(t.id)
                    elif k == "logical":
                        self._mark_log(t.id)
                    elif (
                        t.id not in self.dict_typed_vars
                        and t.id not in self.alloc_ints
                        and t.id not in self.alloc_reals
                        and t.id not in self.alloc_logs
                        and t.id not in self.logs
                    ):
                        # Python numeric default is real; use as fallback when kind
                        # inference is inconclusive.
                        self._mark_real(t.id)
                if (
                    isinstance(t, ast.Name)
                    and isinstance(v, ast.Call)
                    and isinstance(v.func, ast.Name)
                    and v.func.id in self.local_return_specs
                ):
                    spec = self.local_return_specs[v.func.id]
                    if spec == "alloc_real":
                        self._mark_alloc_real(t.id, rank=max(1, self._rank_expr(v)))
                    elif spec == "alloc_int":
                        self._mark_alloc_int(t.id)
                    elif spec == "alloc_log":
                        self._mark_alloc_log(t.id)
                    elif spec == "real":
                        self._mark_real(t.id)
                    elif spec == "int":
                        self._mark_int(t.id)
                if (
                    isinstance(t, ast.Name)
                    and isinstance(v, ast.Call)
                    and isinstance(v.func, ast.Name)
                    and v.func.id == "reshape"
                    and len(v.args) >= 2
                    and isinstance(v.args[1], ast.List)
                    and len(v.args[1].elts) == 2
                ):
                    a0, a1 = v.args[1].elts
                    if isinstance(a0, ast.Constant) and a0.value == 1:
                        self.broadcast_row2.add(t.id)
                    if isinstance(a1, ast.Constant) and a1.value == 1:
                        self.broadcast_col2.add(t.id)
                if (
                    isinstance(t, ast.Name)
                    and isinstance(v, ast.Subscript)
                    and isinstance(v.slice, ast.Tuple)
                    and len(v.slice.elts) == 2
                ):
                    a0, a1 = v.slice.elts
                    full0 = isinstance(a0, ast.Slice) and a0.lower is None and a0.upper is None and a0.step is None
                    full1 = isinstance(a1, ast.Slice) and a1.lower is None and a1.upper is None and a1.step is None
                    if is_none(a0) and full1:
                        self.broadcast_row2.add(t.id)
                    if full0 and is_none(a1):
                        self.broadcast_col2.add(t.id)
                if (
                    isinstance(t, ast.Name)
                    and isinstance(v, ast.Call)
                    and isinstance(v.func, ast.Name)
                    and v.func.id in self.dict_return_types
                ):
                    self.dict_typed_vars[t.id] = self.dict_return_types[v.func.id]
                    self.dict_var_components[t.id] = list(self.dict_type_components.get(self.dict_return_types[v.func.id], []))
                    self.ints.discard(t.id)
                    self.reals.discard(t.id)
                    self.alloc_ints.discard(t.id)
                    self.alloc_reals.discard(t.id)
                    self.alloc_logs.discard(t.id)
                if (
                    isinstance(t, ast.Name)
                    and isinstance(v, ast.Dict)
                    and t.id not in self.dict_typed_vars
                    and t.id not in self.alloc_ints
                    and t.id not in self.alloc_reals
                    and t.id not in self.alloc_logs
                    and t.id not in self.ints
                    and t.id not in self.reals
                ):
                    # Dict placeholders start as None/int sentinel in this subset.
                    self._mark_int(t.id)

                if isinstance(t, ast.Name) and isinstance(v, ast.List) and len(v.elts) == 0:
                    if self.context == "flat":
                        self._mark_alloc_int(t.id)

                # logical sieve init: [True] * (n+1)
                if isinstance(t, ast.Name) and isinstance(v, ast.BinOp) and isinstance(v.op, ast.Mult):
                    if isinstance(v.left, ast.List) and len(v.left.elts) == 1 and is_bool_const(v.left.elts[0]):
                        self._mark_alloc_log(t.id)

                # np.empty(...)
                if (
                    isinstance(t, ast.Name)
                    and isinstance(v, ast.Call)
                    and isinstance(v.func, ast.Attribute)
                    and isinstance(v.func.value, ast.Name)
                    and v.func.value.id == "np"
                    and v.func.attr == "empty"
                ):
                    dtype_txt = ""
                    rank_hint = 1
                    if v.args:
                        shp = v.args[0]
                        if isinstance(shp, (ast.Tuple, ast.List)):
                            rank_hint = max(1, len(shp.elts))
                    for kw in v.keywords:
                        if kw.arg == "dtype":
                            if isinstance(kw.value, ast.Name):
                                dtype_txt = kw.value.id.lower()
                            if (
                                isinstance(kw.value, ast.Attribute)
                                and isinstance(kw.value.value, ast.Name)
                                and kw.value.value.id == "np"
                            ):
                                dtype_txt = kw.value.attr.lower()
                    if "bool" in dtype_txt:
                        self._mark_alloc_log(t.id, rank=rank_hint)
                    elif "float" in dtype_txt:
                        self._mark_alloc_real(t.id, rank=rank_hint)
                    else:
                        self._mark_alloc_int(t.id, rank=rank_hint)

                # np.full(shape, value, dtype=...)
                if (
                    isinstance(t, ast.Name)
                    and isinstance(v, ast.Call)
                    and isinstance(v.func, ast.Attribute)
                    and isinstance(v.func.value, ast.Name)
                    and v.func.value.id == "np"
                    and v.func.attr == "full"
                    and len(v.args) >= 2
                ):
                    dtype_txt = ""
                    rank_hint = 1
                    if isinstance(v.args[0], (ast.Tuple, ast.List)):
                        rank_hint = max(1, len(v.args[0].elts))
                    for kw in v.keywords:
                        if kw.arg == "dtype":
                            if isinstance(kw.value, ast.Name):
                                dtype_txt = kw.value.id.lower()
                            elif (
                                isinstance(kw.value, ast.Attribute)
                                and isinstance(kw.value.value, ast.Name)
                                and kw.value.value.id == "np"
                            ):
                                dtype_txt = kw.value.attr.lower()
                    if dtype_txt:
                        if "float" in dtype_txt:
                            self._mark_alloc_real(t.id, rank=rank_hint)
                        elif "bool" in dtype_txt:
                            self._mark_alloc_log(t.id, rank=rank_hint)
                        else:
                            self._mark_alloc_int(t.id, rank=rank_hint)
                    else:
                        kfill = self._expr_kind(v.args[1])
                        if kfill == "real":
                            self._mark_alloc_real(t.id, rank=rank_hint)
                        elif kfill == "logical":
                            self._mark_alloc_log(t.id, rank=rank_hint)
                        else:
                            self._mark_alloc_int(t.id, rank=rank_hint)

                # np.eye(...)
                if (
                    isinstance(t, ast.Name)
                    and isinstance(v, ast.Call)
                    and isinstance(v.func, ast.Attribute)
                    and isinstance(v.func.value, ast.Name)
                    and v.func.value.id == "np"
                    and v.func.attr in {"eye", "identity"}
                ):
                    self._mark_alloc_real(t.id, rank=2)

                # np.stack((a, b), axis=...)
                if (
                    isinstance(t, ast.Name)
                    and isinstance(v, ast.Call)
                    and isinstance(v.func, ast.Attribute)
                    and isinstance(v.func.value, ast.Name)
                    and v.func.value.id == "np"
                    and v.func.attr == "stack"
                    and len(v.args) >= 1
                ):
                    self._mark_alloc_real(t.id, rank=2)

                # np.zeros(...), np.ones(...), zeros_like/ones_like
                if (
                    isinstance(t, ast.Name)
                    and isinstance(v, ast.Call)
                    and isinstance(v.func, ast.Attribute)
                    and isinstance(v.func.value, ast.Name)
                    and v.func.value.id == "np"
                    and v.func.attr in {"zeros", "ones", "zeros_like", "ones_like"}
                ):
                    dtype_txt = self._np_dtype_text(v)
                    rank_hint = 1
                    if v.func.attr in {"zeros", "ones"} and len(v.args) >= 1 and isinstance(v.args[0], (ast.Tuple, ast.List)):
                        rank_hint = max(1, len(v.args[0].elts))
                    elif v.func.attr in {"zeros_like", "ones_like"} and len(v.args) >= 1:
                        rank_hint = max(1, self._rank_expr(v.args[0]))
                    if v.func.attr in {"zeros_like", "ones_like"} and len(v.args) >= 1 and not dtype_txt:
                        k0 = self._expr_kind(v.args[0])
                        if k0 == "int":
                            self._mark_alloc_int(t.id, rank=rank_hint)
                        elif k0 == "logical":
                            self._mark_alloc_log(t.id, rank=rank_hint)
                        else:
                            self._mark_alloc_real(t.id, rank=rank_hint)
                    else:
                        if "int" in dtype_txt:
                            self._mark_alloc_int(t.id, rank=rank_hint)
                        elif "bool" in dtype_txt:
                            self._mark_alloc_log(t.id, rank=rank_hint)
                        else:
                            self._mark_alloc_real(t.id, rank=rank_hint)

                # np.triu(...) / np.tril(...)
                if (
                    isinstance(t, ast.Name)
                    and isinstance(v, ast.Call)
                    and isinstance(v.func, ast.Attribute)
                    and isinstance(v.func.value, ast.Name)
                    and v.func.value.id == "np"
                    and v.func.attr in {"triu", "tril"}
                    and len(v.args) >= 1
                ):
                    ksrc = self._expr_kind(v.args[0])
                    if ksrc == "real":
                        self._mark_alloc_real(t.id, rank=2)
                    elif ksrc == "logical":
                        self._mark_alloc_log(t.id)
                    else:
                        self._mark_alloc_int(t.id)

                # np.diag(...)
                if (
                    isinstance(t, ast.Name)
                    and isinstance(v, ast.Call)
                    and isinstance(v.func, ast.Attribute)
                    and isinstance(v.func.value, ast.Name)
                    and v.func.value.id == "np"
                    and v.func.attr == "diag"
                    and len(v.args) >= 1
                ):
                    rsrc = self._rank_expr(v.args[0])
                    ksrc = self._expr_kind(v.args[0])
                    if rsrc <= 1:
                        if ksrc == "real":
                            self._mark_alloc_real(t.id, rank=2)
                        elif ksrc == "logical":
                            self._mark_alloc_log(t.id)
                        else:
                            self._mark_alloc_int(t.id)
                    else:
                        if ksrc == "real":
                            self._mark_alloc_real(t.id, rank=1)
                        elif ksrc == "logical":
                            self._mark_alloc_log(t.id)
                        else:
                            self._mark_alloc_int(t.id)
                    k_node = None
                    if len(v.args) >= 2:
                        k_node = v.args[1]
                    for kw in v.keywords:
                        if kw.arg == "k":
                            k_node = kw.value
                            break
                    if isinstance(k_node, ast.Constant) and isinstance(k_node.value, int) and int(k_node.value) != 0:
                        self._mark_int("i_d")

                # Method chains producing arrays, e.g. np.arange(...).reshape(...).astype(float)
                if (
                    isinstance(t, ast.Name)
                    and isinstance(v, ast.Call)
                    and isinstance(v.func, ast.Attribute)
                    and v.func.attr in {"reshape", "astype"}
                ):
                    rk = max(1, self._rank_expr(v))
                    k = self._expr_kind(v)
                    if k == "real":
                        self._mark_alloc_real(t.id, rank=rk)
                    elif k == "logical":
                        self._mark_alloc_log(t.id)
                    else:
                        self._mark_alloc_int(t.id)

                # np.array([...]) / np.asarray([...])
                if (
                    isinstance(t, ast.Name)
                    and isinstance(v, ast.Call)
                    and isinstance(v.func, ast.Attribute)
                    and isinstance(v.func.value, ast.Name)
                    and v.func.value.id == "np"
                    and v.func.attr in {"array", "asarray"}
                    and len(v.args) >= 1
                    and isinstance(v.args[0], ast.List)
                ):
                    dtype_txt = ""
                    rank_hint = 1
                    if isinstance(v.args[0], ast.List) and v.args[0].elts and all(isinstance(e, ast.List) for e in v.args[0].elts):
                        rank_hint = 2
                    for kw in v.keywords:
                        if kw.arg == "dtype":
                            if isinstance(kw.value, ast.Name):
                                dtype_txt = kw.value.id.lower()
                            elif (
                                isinstance(kw.value, ast.Attribute)
                                and isinstance(kw.value.value, ast.Name)
                                and kw.value.value.id == "np"
                            ):
                                dtype_txt = kw.value.attr.lower()
                    if dtype_txt:
                        if "float" in dtype_txt:
                            self._mark_alloc_real(t.id, rank=rank_hint)
                        elif "bool" in dtype_txt:
                            self._mark_alloc_log(t.id, rank=rank_hint)
                        else:
                            self._mark_alloc_int(t.id, rank=rank_hint)
                    else:
                        flat_elts = []
                        for e in v.args[0].elts:
                            if isinstance(e, ast.List):
                                flat_elts.extend(e.elts)
                            else:
                                flat_elts.append(e)
                        has_float = any(isinstance(e, ast.Constant) and isinstance(e.value, float) for e in flat_elts)
                        has_bool = any(is_bool_const(e) for e in flat_elts)
                        if has_float:
                            self._mark_alloc_real(t.id, rank=rank_hint)
                        elif has_bool:
                            self._mark_alloc_log(t.id, rank=rank_hint)
                        else:
                            self._mark_alloc_int(t.id, rank=rank_hint)

                # np.array(list_var, dtype=...) / np.asarray(list_var, dtype=...)
                if (
                    isinstance(t, ast.Name)
                    and isinstance(v, ast.Call)
                    and isinstance(v.func, ast.Attribute)
                    and isinstance(v.func.value, ast.Name)
                    and v.func.value.id == "np"
                    and v.func.attr in {"array", "asarray"}
                    and len(v.args) >= 1
                    and isinstance(v.args[0], ast.Name)
                    and v.args[0].id in self.list_counts
                ):
                    dtype_txt = ""
                    for kw in v.keywords:
                        if kw.arg == "dtype":
                            if isinstance(kw.value, ast.Name):
                                dtype_txt = kw.value.id.lower()
                            elif (
                                isinstance(kw.value, ast.Attribute)
                                and isinstance(kw.value.value, ast.Name)
                                and kw.value.value.id == "np"
                            ):
                                dtype_txt = kw.value.attr.lower()
                    if "float" in dtype_txt:
                        self._mark_alloc_real(t.id)
                    elif "bool" in dtype_txt:
                        self._mark_alloc_log(t.id)
                    else:
                        self._mark_alloc_int(t.id)

                # np.asarray(...)
                if (
                    isinstance(t, ast.Name)
                    and isinstance(v, ast.Call)
                    and isinstance(v.func, ast.Attribute)
                    and isinstance(v.func.value, ast.Name)
                    and v.func.value.id == "np"
                    and v.func.attr == "asarray"
                    and len(v.args) >= 1
                ):
                    dtype_txt = ""
                    for kw in v.keywords:
                        if kw.arg == "dtype":
                            if isinstance(kw.value, ast.Name):
                                dtype_txt = kw.value.id.lower()
                            elif (
                                isinstance(kw.value, ast.Attribute)
                                and isinstance(kw.value.value, ast.Name)
                                and kw.value.value.id == "np"
                            ):
                                dtype_txt = kw.value.attr.lower()
                    if "float" in dtype_txt:
                        self._mark_alloc_real(t.id)
                    elif "int" in dtype_txt:
                        self._mark_alloc_int(t.id)
                    elif "bool" in dtype_txt:
                        self._mark_alloc_log(t.id)
                    else:
                        if isinstance(v.args[0], ast.List):
                            has_float = any(isinstance(e, ast.Constant) and isinstance(e.value, float) for e in v.args[0].elts)
                            has_bool = any(is_bool_const(e) for e in v.args[0].elts)
                            if has_float:
                                self._mark_alloc_real(t.id)
                            elif has_bool:
                                self._mark_alloc_log(t.id)
                            else:
                                self._mark_alloc_int(t.id)
                        else:
                            k0 = self._expr_kind(v.args[0])
                            if k0 == "real":
                                self._mark_alloc_real(t.id)
                            else:
                                self._mark_alloc_int(t.id)

                # np.sort(...)
                if (
                    isinstance(t, ast.Name)
                    and isinstance(v, ast.Call)
                    and isinstance(v.func, ast.Attribute)
                    and isinstance(v.func.value, ast.Name)
                    and v.func.value.id == "np"
                    and v.func.attr == "sort"
                    and len(v.args) >= 1
                ):
                    k0 = self._expr_kind(v.args[0])
                    if k0 == "int":
                        self._mark_alloc_int(t.id)
                    else:
                        self._mark_alloc_real(t.id)
                # np.argsort(...)
                if (
                    isinstance(t, ast.Name)
                    and isinstance(v, ast.Call)
                    and isinstance(v.func, ast.Attribute)
                    and isinstance(v.func.value, ast.Name)
                    and v.func.value.id == "np"
                    and v.func.attr == "argsort"
                    and len(v.args) >= 1
                ):
                    self._mark_alloc_int(t.id)
                # np.argmax(a, axis=1) / np.argmin(a, axis=1)
                if (
                    isinstance(t, ast.Name)
                    and isinstance(v, ast.Call)
                    and isinstance(v.func, ast.Attribute)
                    and isinstance(v.func.value, ast.Name)
                    and v.func.value.id == "np"
                    and v.func.attr in {"argmax", "argmin"}
                ):
                    axis_node = None
                    for kw in v.keywords:
                        if kw.arg == "axis":
                            axis_node = kw.value
                    if isinstance(axis_node, ast.Constant) and isinstance(axis_node.value, int):
                        self._mark_alloc_int(t.id)
                    else:
                        self._mark_int(t.id)
                # np.where(cond, a, b)
                if (
                    isinstance(t, ast.Name)
                    and isinstance(v, ast.Call)
                    and isinstance(v.func, ast.Attribute)
                    and isinstance(v.func.value, ast.Name)
                    and v.func.value.id == "np"
                    and v.func.attr == "where"
                    and len(v.args) == 3
                ):
                    k1 = self._expr_kind(v.args[1])
                    if k1 == "real":
                        self._mark_alloc_real(t.id)
                    elif k1 == "logical":
                        self._mark_alloc_log(t.id)
                    else:
                        self._mark_alloc_int(t.id)

                # np.max/sum/mean/min(..., keepdims=True)
                if (
                    isinstance(t, ast.Name)
                    and isinstance(v, ast.Call)
                    and isinstance(v.func, ast.Attribute)
                    and isinstance(v.func.value, ast.Name)
                    and v.func.value.id == "np"
                    and v.func.attr in {"max", "sum", "mean", "min"}
                ):
                    axis_node = None
                    keepdims = False
                    for kw in v.keywords:
                        if kw.arg == "axis":
                            axis_node = kw.value
                        elif kw.arg == "keepdims":
                            keepdims = bool(isinstance(kw.value, ast.Constant) and kw.value.value is True)
                    if keepdims:
                        self._mark_alloc_real(t.id)
                        if isinstance(axis_node, ast.Constant) and isinstance(axis_node.value, int):
                            if axis_node.value == 1:
                                self.broadcast_col2.add(t.id)
                            elif axis_node.value == 0:
                                self.broadcast_row2.add(t.id)
                        else:
                            # Default to column-vector style keepdims (common case axis=1).
                            self.broadcast_col2.add(t.id)

                # a.mean/sum/max/min(..., keepdims=True)
                if (
                    isinstance(t, ast.Name)
                    and isinstance(v, ast.Call)
                    and isinstance(v.func, ast.Attribute)
                    and not (_attr_root_name(v.func.value) in {"np", "math", "random"})
                    and v.func.attr in {"mean", "sum", "max", "min"}
                ):
                    axis_node = None
                    keepdims = False
                    for kw in v.keywords:
                        if kw.arg == "axis":
                            axis_node = kw.value
                        elif kw.arg == "keepdims":
                            keepdims = bool(isinstance(kw.value, ast.Constant) and kw.value.value is True)
                    if keepdims:
                        if isinstance(axis_node, ast.Constant) and isinstance(axis_node.value, int):
                            if axis_node.value == 1:
                                self.broadcast_col2.add(t.id)
                            elif axis_node.value == 0:
                                self.broadcast_row2.add(t.id)
                        else:
                            self.broadcast_col2.add(t.id)

                # np.random.normal(size=...)
                if (
                    isinstance(t, ast.Name)
                    and isinstance(v, ast.Call)
                    and isinstance(v.func, ast.Attribute)
                    and isinstance(v.func.value, ast.Attribute)
                    and isinstance(v.func.value.value, ast.Name)
                    and v.func.value.value.id == "np"
                    and v.func.value.attr == "random"
                    and v.func.attr == "normal"
                ):
                    self._mark_alloc_real(t.id)

                # rng.normal(...)
                if (
                    isinstance(t, ast.Name)
                    and isinstance(v, ast.Call)
                    and isinstance(v.func, ast.Attribute)
                    and isinstance(v.func.value, ast.Name)
                    and v.func.attr == "normal"
                ):
                    self._mark_alloc_real(t.id)

                # rng.choice(2, size=n, p=w)
                if (
                    isinstance(t, ast.Name)
                    and isinstance(v, ast.Call)
                    and isinstance(v.func, ast.Attribute)
                    and v.func.attr == "choice"
                ):
                    self._mark_alloc_int(t.id)

                # x = arr[rng.choice(n, size=k, replace=False)].astype(float)
                if (
                    isinstance(t, ast.Name)
                    and isinstance(v, ast.Call)
                    and isinstance(v.func, ast.Attribute)
                    and v.func.attr == "astype"
                    and isinstance(v.func.value, ast.Subscript)
                    and isinstance(v.func.value.slice, ast.Call)
                    and isinstance(v.func.value.slice.func, ast.Attribute)
                    and v.func.value.slice.func.attr == "choice"
                ):
                    self._mark_alloc_real(t.id)

            if isinstance(node, ast.AugAssign):
                for nm in extract_target_names(node.target):
                    if nm in self.reals:
                        self._mark_real(nm)
                    else:
                        self._mark_int(nm)

            if isinstance(node, ast.For) and isinstance(node.target, ast.Name):
                if node.target.id == "_":
                    self._mark_int("i_")
                else:
                    self._mark_int(node.target.id)
                self.prescan(node.body)

            if isinstance(node, ast.If):
                self.prescan(node.body)
                self.prescan(node.orelse)
            if isinstance(node, ast.While):
                self.prescan(node.body)
                self.prescan(node.orelse)

            if isinstance(node, ast.Expr) and isinstance(node.value, ast.Call):
                c = node.value
                if isinstance(c.func, ast.Attribute) and c.func.attr == "append":
                    if isinstance(c.func.value, ast.Name):
                        if self.context == "flat":
                            k = self._expr_kind(c.args[0]) if c.args else None
                            if k == "real":
                                self._mark_alloc_real(c.func.value.id)
                            else:
                                self._mark_alloc_int(c.func.value.id)

    def visit_AnnAssign(self, node):
        if node.value is None:
            return
        fake = ast.Assign(targets=[node.target], value=node.value)
        self.visit_Assign(fake)

    def visit_Assign(self, node):
        self._emit_comments_for(node)
        if len(node.targets) != 1:
            raise NotImplementedError("multiple assignment not supported")
        t = node.targets[0]
        v = node.value
        if isinstance(t, ast.Name) and t.id in self.reserved_names:
            # keep translator-owned kind parameters stable
            return

        # Shape-intent marker rewrites from NumPy axis insertion:
        # x = x[:, None] or x = x[None, :]
        # Keep only the intent metadata (captured during prescan), and avoid
        # emitting rank-changing assignments that are invalid for fixed-rank
        # Fortran dummy arguments.
        if (
            isinstance(t, ast.Name)
            and isinstance(v, ast.Subscript)
            and isinstance(v.value, ast.Name)
            and v.value.id == t.id
            and isinstance(v.slice, ast.Tuple)
            and len(v.slice.elts) == 2
        ):
            a0, a1 = v.slice.elts
            full0 = isinstance(a0, ast.Slice) and a0.lower is None and a0.upper is None and a0.step is None
            full1 = isinstance(a1, ast.Slice) and a1.lower is None and a1.upper is None and a1.step is None
            if (full0 and is_none(a1)) or (is_none(a0) and full1):
                return

        # Pairwise 2D fancy indexing read:
        #   y = a[rows, cols]
        # NumPy semantics are elementwise pairs, not cartesian product.
        if (
            isinstance(t, ast.Name)
            and isinstance(v, ast.Subscript)
            and isinstance(v.slice, ast.Tuple)
            and len(v.slice.elts) == 2
            and self._rank_expr(v.value) == 2
        ):
            i0n, i1n = v.slice.elts
            if (
                (not isinstance(i0n, ast.Slice))
                and (not isinstance(i1n, ast.Slice))
                and (not is_none(i0n))
                and (not is_none(i1n))
                and self._rank_expr(i0n) > 0
                and self._rank_expr(i1n) > 0
            ):
                base = self.expr(v.value)
                i0 = self.expr(i0n)
                i1 = self.expr(i1n)
                self.o.w(f"if (allocated({t.id})) deallocate({t.id})")
                self.o.w(f"allocate({t.id}(1:min(size({i0}), size({i1}))))")
                self.o.w("block")
                self.o.push()
                self.o.w("integer :: i_pw")
                self.o.w(f"do i_pw = 1, size({t.id})")
                self.o.push()
                self.o.w(f"{t.id}(i_pw) = {base}({i0}(i_pw) + 1, {i1}(i_pw) + 1)")
                self.o.pop()
                self.o.w("end do")
                self.o.pop()
                self.o.w("end block")
                return

        # Pairwise 2D fancy indexing write:
        #   a[rows, cols] = rhs
        if (
            isinstance(t, ast.Subscript)
            and isinstance(t.slice, ast.Tuple)
            and len(t.slice.elts) == 2
            and self._rank_expr(t.value) == 2
        ):
            i0n, i1n = t.slice.elts
            if (
                (not isinstance(i0n, ast.Slice))
                and (not isinstance(i1n, ast.Slice))
                and (not is_none(i0n))
                and (not is_none(i1n))
                and self._rank_expr(i0n) > 0
                and self._rank_expr(i1n) > 0
            ):
                base = self.expr(t.value)
                i0 = self.expr(i0n)
                i1 = self.expr(i1n)
                rhs = self.expr(v)
                rhs_rank = self._rank_expr(v)
                self.o.w("block")
                self.o.push()
                self.o.w("integer :: i_pw, n_pw")
                self.o.w(f"n_pw = min(size({i0}), size({i1}))")
                if rhs_rank > 0:
                    self.o.w(f"n_pw = min(n_pw, size({rhs}))")
                self.o.w("do i_pw = 1, n_pw")
                self.o.push()
                if rhs_rank > 0:
                    self.o.w(f"{base}({i0}(i_pw) + 1, {i1}(i_pw) + 1) = {rhs}(i_pw)")
                else:
                    self.o.w(f"{base}({i0}(i_pw) + 1, {i1}(i_pw) + 1) = {rhs}")
                self.o.pop()
                self.o.w("end do")
                self.o.pop()
                self.o.w("end block")
                return

        # Targeted safe lowering for common NumPy-broadcast EM patterns.
        if isinstance(t, ast.Name) and t.id == "log_norm" and hasattr(ast, "unparse"):
            txt = ast.unparse(v)
            if ("xcol" in txt) and ("mucol" in txt) and ("sigcol" in txt):
                self.o.w("if (allocated(log_norm)) deallocate(log_norm)")
                self.o.w("allocate(log_norm(1:size(xcol,1),1:size(mucol,2)))")
                self.o.w("block")
                self.o.push()
                self.o.w("integer :: i_b, j_b")
                self.o.w("do i_b = 1, size(xcol,1)")
                self.o.push()
                self.o.w("do j_b = 1, size(mucol,2)")
                self.o.push()
                self.o.w(
                    "log_norm(i_b,j_b) = (-0.5_dp * log(2.0_dp * acos(-1.0_dp))) - "
                    "log(sigcol(1,j_b)) - 0.5_dp * (((xcol(i_b,1) - mucol(1,j_b)) / sigcol(1,j_b)) ** 2)"
                )
                self.o.pop()
                self.o.w("end do")
                self.o.pop()
                self.o.w("end do")
                self.o.pop()
                self.o.w("end block")
                return

        if isinstance(t, ast.Name) and t.id == "log_joint" and hasattr(ast, "unparse"):
            txt = ast.unparse(v)
            if ("log_w" in txt) and ("log_norm" in txt):
                self.o.w("if (allocated(log_joint)) deallocate(log_joint)")
                self.o.w("allocate(log_joint(1:size(log_norm,1),1:size(log_norm,2)))")
                self.o.w("block")
                self.o.push()
                self.o.w("integer :: i_b, j_b")
                self.o.w("do i_b = 1, size(log_norm,1)")
                self.o.push()
                self.o.w("do j_b = 1, size(log_norm,2)")
                self.o.push()
                self.o.w("log_joint(i_b,j_b) = log_w(1,j_b) + log_norm(i_b,j_b)")
                self.o.pop()
                self.o.w("end do")
                self.o.pop()
                self.o.w("end do")
                self.o.pop()
                self.o.w("end block")
                return

        if isinstance(t, ast.Name) and t.id == "logp" and hasattr(ast, "unparse"):
            txt = ast.unparse(v)
            if ("log_w" in txt) and ("log_normal_pdf_1d" in txt):
                self.o.w("if (allocated(logp)) deallocate(logp)")
                self.o.w("allocate(logp(1:size(x),1:size(log_w)))")
                self.o.w("logp = spread(log_w, dim=1, ncopies=size(x)) + log_normal_pdf_1d(x, mu, var)")
                return

        if isinstance(t, ast.Name) and t.id == "r" and hasattr(ast, "unparse"):
            txt = ast.unparse(v)
            if ("log_joint" in txt) and ("log_denom" in txt) and ("exp(" in txt):
                self.o.w("if (allocated(r)) deallocate(r)")
                self.o.w("allocate(r(1:size(log_joint,1),1:size(log_joint,2)))")
                self.o.w("block")
                self.o.push()
                self.o.w("integer :: i_b, j_b")
                self.o.w("do i_b = 1, size(log_joint,1)")
                self.o.push()
                self.o.w("do j_b = 1, size(log_joint,2)")
                self.o.push()
                self.o.w("r(i_b,j_b) = exp(log_joint(i_b,j_b) - log_denom(i_b,1))")
                self.o.pop()
                self.o.w("end do")
                self.o.pop()
                self.o.w("end do")
                self.o.pop()
                self.o.w("end block")
                return

        # tuple unpacking from tuple-return procedure call:
        #   a, b = f(...)
        if (
            isinstance(t, (ast.Tuple, ast.List))
            and isinstance(v, ast.Call)
            and isinstance(v.func, ast.Name)
            and v.func.id in self.tuple_return_funcs
        ):
            outs = []
            for e in t.elts:
                if not isinstance(e, ast.Name):
                    raise NotImplementedError("tuple assignment targets must be names")
                outs.append(e.id)
            args_nodes = list(v.args)
            for kw in getattr(v, "keywords", []):
                if kw.arg is None:
                    raise NotImplementedError("**kwargs not supported")
                args_nodes.append(kw.value)
            args = [self.expr(a) for a in args_nodes] + outs
            self.o.w(f"call {v.func.id}(" + ", ".join(args) + ")")
            return
        # tuple unpacking from np.unique(..., return_inverse=True, return_counts=True)
        if (
            isinstance(t, (ast.Tuple, ast.List))
            and isinstance(v, ast.Call)
            and isinstance(v.func, ast.Attribute)
            and isinstance(v.func.value, ast.Name)
            and v.func.value.id == "np"
            and v.func.attr == "unique"
        ):
            want_inv = False
            want_cnt = False
            for kw in v.keywords:
                if kw.arg == "return_inverse" and isinstance(kw.value, ast.Constant) and kw.value.value is True:
                    want_inv = True
                if kw.arg == "return_counts" and isinstance(kw.value, ast.Constant) and kw.value.value is True:
                    want_cnt = True
            if want_inv and want_cnt and len(t.elts) >= 3 and len(v.args) >= 1:
                outs = []
                for e in t.elts[:3]:
                    if not isinstance(e, ast.Name):
                        raise NotImplementedError("tuple assignment targets must be names")
                    outs.append(e.id)
                self.o.w(f"call unique_int_inv_counts({self.expr(v.args[0])}, {outs[0]}, {outs[1]}, {outs[2]})")
                return
        # tuple unpacking from supported numpy.linalg calls:
        #   w, v = np.linalg.eig(a)
        #   u, s, vt = np.linalg.svd(a)
        if (
            isinstance(t, (ast.Tuple, ast.List))
            and isinstance(v, ast.Call)
            and isinstance(v.func, ast.Attribute)
            and isinstance(v.func.value, ast.Attribute)
            and isinstance(v.func.value.value, ast.Name)
            and v.func.value.value.id == "np"
            and v.func.value.attr == "linalg"
        ):
            outs = []
            for e in t.elts:
                if not isinstance(e, ast.Name):
                    raise NotImplementedError("tuple assignment targets must be names")
                outs.append(e.id)
            if v.func.attr == "eig" and len(v.args) >= 1 and len(outs) >= 2:
                self.o.w(f"call linalg_eig({self.expr(v.args[0])}, {outs[0]}, {outs[1]})")
                return
            if v.func.attr == "svd" and len(v.args) >= 1 and len(outs) >= 3:
                self.o.w(f"call linalg_svd({self.expr(v.args[0])}, {outs[0]}, {outs[1]}, {outs[2]})")
                return
        # tuple unpacking from np.meshgrid(x, y, indexing='xy'/'ij')
        if (
            isinstance(t, (ast.Tuple, ast.List))
            and isinstance(v, ast.Call)
            and isinstance(v.func, ast.Attribute)
            and isinstance(v.func.value, ast.Name)
            and v.func.value.id == "np"
            and v.func.attr == "meshgrid"
        ):
            outs = []
            for e in t.elts:
                if not isinstance(e, ast.Name):
                    raise NotImplementedError("tuple assignment targets must be names")
                outs.append(e.id)
            if len(outs) < 2 or len(v.args) < 2:
                raise NotImplementedError("np.meshgrid assignment expects at least two outputs and two inputs")
            xname = self.expr(v.args[0])
            yname = self.expr(v.args[1])
            indexing = "xy"
            for kw in v.keywords:
                if kw.arg == "indexing":
                    if not is_const_str(kw.value):
                        raise NotImplementedError("np.meshgrid indexing must be constant string")
                    indexing = str(kw.value.value).lower()
            if indexing not in {"xy", "ij"}:
                raise NotImplementedError("np.meshgrid currently supports indexing='xy' or 'ij'")
            xx = outs[0]
            yy = outs[1]
            if indexing == "xy":
                self.o.w(f"{xx} = spread({xname}, dim=1, ncopies=size({yname}))")
                self.o.w(f"{yy} = spread({yname}, dim=2, ncopies=size({xname}))")
            else:
                self.o.w(f"{xx} = spread({xname}, dim=2, ncopies=size({yname}))")
                self.o.w(f"{yy} = spread({yname}, dim=1, ncopies=size({xname}))")
            return
        # tuple unpacking from np.histogram(x, bins=...)
        if (
            isinstance(t, (ast.Tuple, ast.List))
            and isinstance(v, ast.Call)
            and isinstance(v.func, ast.Attribute)
            and isinstance(v.func.value, ast.Name)
            and v.func.value.id == "np"
            and v.func.attr == "histogram"
        ):
            outs = []
            for e in t.elts:
                if not isinstance(e, ast.Name):
                    raise NotImplementedError("tuple assignment targets must be names")
                outs.append(e.id)
            if len(outs) < 2 or len(v.args) < 1:
                raise NotImplementedError("np.histogram assignment expects two outputs and input array")
            xname = self.expr(v.args[0])
            bins_node = v.args[1] if len(v.args) >= 2 else None
            for kw in v.keywords:
                if kw.arg == "bins":
                    bins_node = kw.value
                elif kw.arg in {"range", "weights", "density"}:
                    raise NotImplementedError(f"np.histogram keyword '{kw.arg}' is not yet supported")
            if bins_node is None:
                raise NotImplementedError("np.histogram currently requires explicit bins")
            self.o.w(f"call histogram({xname}, {self.expr(bins_node)}, {outs[0]}, {outs[1]})")
            return

        # ignore module-level param assignment already emitted as parameter
        if isinstance(t, ast.Name) and t.id in self.params:
            return

        # d = {"k": expr, ...}
        if isinstance(t, ast.Name) and isinstance(v, ast.Dict):
            # If the target is a known typed-dict variable, emit component
            # assignments (and allocation-on-assignment for array components).
            if t.id in self.dict_typed_vars:
                for k, vv in zip(v.keys, v.values):
                    if not (isinstance(k, ast.Constant) and isinstance(k.value, str)):
                        raise NotImplementedError("dict keys must be string constants")
                    lhs = f"{t.id}%{k.value}"
                    rhs = self.expr(vv)
                    self.o.w(f"{lhs} = {rhs}")
                return
            # Otherwise keep an alias map for simple dict-like lookups.
            alias = {}
            for k, vv in zip(v.keys, v.values):
                if not (isinstance(k, ast.Constant) and isinstance(k.value, str)):
                    raise NotImplementedError("dict keys must be string constants")
                alias[k.value] = self.expr(vv)
            self.dict_aliases[t.id] = alias
            return

        # simple int literal assignment
        if isinstance(t, ast.Name) and is_const_int(v):
            self.o.w(f"{t.id} = {v.value}")
            return

        # None sentinel assignment
        if isinstance(t, ast.Name) and is_none(v):
            if t.id in self.dict_typed_vars:
                return
            self.o.w(f"{t.id} = -1")
            return

        # x = random.random() -> call random_number(x)
        if (
            isinstance(t, ast.Name)
            and isinstance(v, ast.Call)
            and isinstance(v.func, ast.Attribute)
            and isinstance(v.func.value, ast.Name)
            and v.func.value.id == "random"
            and v.func.attr == "random"
            and len(v.args) == 0
        ):
            self.o.w(f"call random_number({t.id})")
            return

        # x = np.random.random() -> call random_number(x)
        if (
            isinstance(t, ast.Name)
            and isinstance(v, ast.Call)
            and isinstance(v.func, ast.Attribute)
            and isinstance(v.func.value, ast.Attribute)
            and isinstance(v.func.value.value, ast.Name)
            and v.func.value.value.id == "np"
            and v.func.value.attr == "random"
            and v.func.attr == "random"
            and len(v.args) == 0
        ):
            self.o.w(f"call random_number({t.id})")
            return

        # x = float(np.random.random()) -> call random_number(x)
        if (
            isinstance(t, ast.Name)
            and isinstance(v, ast.Call)
            and isinstance(v.func, ast.Name)
            and v.func.id == "float"
            and len(v.args) == 1
            and isinstance(v.args[0], ast.Call)
            and isinstance(v.args[0].func, ast.Attribute)
            and isinstance(v.args[0].func.value, ast.Attribute)
            and isinstance(v.args[0].func.value.value, ast.Name)
            and v.args[0].func.value.value.id == "np"
            and v.args[0].func.value.attr == "random"
            and v.args[0].func.attr == "random"
            and len(v.args[0].args) == 0
        ):
            self.o.w(f"call random_number({t.id})")
            return

        # x = rng.choice(arr, size=k, replace=False) if cond else expr
        if (
            isinstance(t, ast.Name)
            and isinstance(v, ast.IfExp)
            and isinstance(v.body, ast.Call)
            and isinstance(v.body.func, ast.Attribute)
            and v.body.func.attr == "choice"
            and v.body.args
            and isinstance(v.body.args[0], ast.Name)
        ):
            arr_name = v.body.args[0].id
            size_node = None
            replace_false = False
            for kw in v.body.keywords:
                if kw.arg == "size":
                    size_node = kw.value
                elif kw.arg == "replace" and isinstance(kw.value, ast.Constant) and kw.value.value is False:
                    replace_false = True
            if size_node is not None and replace_false:
                k_expr = self.expr(size_node)
                self.o.w(f"if ({self.expr(v.test)}) then")
                self.o.push()
                self.o.w(f"if (allocated({t.id})) deallocate({t.id})")
                self.o.w(f"allocate({t.id}(1:{k_expr}))")
                self.o.w("block")
                self.o.push()
                self.o.w("integer :: i_ch")
                self.o.w("integer, allocatable :: idx_ch(:)")
                self.o.w(f"allocate(idx_ch(1:{k_expr}))")
                self.o.w(f"call random_choice_norep(size({arr_name}), {k_expr}, idx_ch)")
                self.o.w(f"do i_ch = 1, {k_expr}")
                self.o.push()
                self.o.w(f"{t.id}(i_ch) = {arr_name}(idx_ch(i_ch) + 1)")
                self.o.pop()
                self.o.w("end do")
                self.o.w("if (allocated(idx_ch)) deallocate(idx_ch)")
                self.o.pop()
                self.o.w("end block")
                self.o.pop()
                self.o.w("else")
                self.o.push()
                self.o.w(f"{t.id} = {self.expr(v.orelse)}")
                self.o.pop()
                self.o.w("end if")
                return

        # x = arr[rng.choice(n, size=k, replace=False)].astype(float)
        if (
            isinstance(t, ast.Name)
            and isinstance(v, ast.Call)
            and isinstance(v.func, ast.Attribute)
            and v.func.attr == "astype"
            and isinstance(v.func.value, ast.Subscript)
            and isinstance(v.func.value.value, ast.Name)
            and isinstance(v.func.value.slice, ast.Call)
            and isinstance(v.func.value.slice.func, ast.Attribute)
            and v.func.value.slice.func.attr == "choice"
        ):
            arr_name = v.func.value.value.id
            ch = v.func.value.slice
            npop_node = ch.args[0] if ch.args else None
            size_node = None
            replace_false = False
            for kw in ch.keywords:
                if kw.arg == "size":
                    size_node = kw.value
                elif kw.arg == "replace" and isinstance(kw.value, ast.Constant) and kw.value.value is False:
                    replace_false = True
            if npop_node is None or size_node is None or not replace_false:
                raise NotImplementedError("choice gather currently supports choice(n, size=k, replace=False)")
            npop_expr = self.expr(npop_node)
            k_expr = self.expr(size_node)
            self.o.w(f"if (allocated({t.id})) deallocate({t.id})")
            self.o.w(f"allocate({t.id}(1:{k_expr}))")
            self.o.w("block")
            self.o.push()
            self.o.w("integer :: i_ch")
            self.o.w("integer, allocatable :: idx_ch(:)")
            self.o.w(f"allocate(idx_ch(1:{k_expr}))")
            self.o.w(f"call random_choice_norep({npop_expr}, {k_expr}, idx_ch)")
            self.o.w(f"do i_ch = 1, {k_expr}")
            self.o.push()
            self.o.w(f"{t.id}(i_ch) = real({arr_name}(idx_ch(i_ch) + 1), kind=dp)")
            self.o.pop()
            self.o.w("end do")
            self.o.w("if (allocated(idx_ch)) deallocate(idx_ch)")
            self.o.pop()
            self.o.w("end block")
            return

        # x = arr[rng.choice(n, size=k, replace=False)].copy()  (or without .copy())
        gather_node = None
        if (
            isinstance(t, ast.Name)
            and isinstance(v, ast.Call)
            and isinstance(v.func, ast.Attribute)
            and v.func.attr == "copy"
            and isinstance(v.func.value, ast.Subscript)
        ):
            gather_node = v.func.value
        elif (
            isinstance(t, ast.Name)
            and isinstance(v, ast.Subscript)
        ):
            gather_node = v
        if (
            gather_node is not None
            and isinstance(gather_node.value, ast.Name)
            and isinstance(gather_node.slice, ast.Call)
            and isinstance(gather_node.slice.func, ast.Attribute)
            and gather_node.slice.func.attr == "choice"
        ):
            arr_name = gather_node.value.id
            ch = gather_node.slice
            npop_node = ch.args[0] if ch.args else None
            size_node = None
            replace_false = False
            for kw in ch.keywords:
                if kw.arg == "size":
                    size_node = kw.value
                elif kw.arg == "replace" and isinstance(kw.value, ast.Constant) and kw.value.value is False:
                    replace_false = True
            if npop_node is None or size_node is None or not replace_false:
                raise NotImplementedError("choice gather currently supports choice(n, size=k, replace=False)")
            npop_expr = self.expr(npop_node)
            k_expr = self.expr(size_node)
            self.o.w(f"if (allocated({t.id})) deallocate({t.id})")
            self.o.w(f"allocate({t.id}(1:{k_expr}))")
            self.o.w("block")
            self.o.push()
            self.o.w("integer :: i_ch")
            self.o.w("integer, allocatable :: idx_ch(:)")
            self.o.w(f"allocate(idx_ch(1:{k_expr}))")
            self.o.w(f"call random_choice_norep({npop_expr}, {k_expr}, idx_ch)")
            self.o.w(f"do i_ch = 1, {k_expr}")
            self.o.push()
            self.o.w(f"{t.id}(i_ch) = {arr_name}(idx_ch(i_ch) + 1)")
            self.o.pop()
            self.o.w("end do")
            self.o.w("if (allocated(idx_ch)) deallocate(idx_ch)")
            self.o.pop()
            self.o.w("end block")
            return

        # x = arr[rng.choice(..., replace=False)].copy() if cond else expr
        if (
            isinstance(t, ast.Name)
            and isinstance(v, ast.IfExp)
            and isinstance(v.body, ast.Call)
            and isinstance(v.body.func, ast.Attribute)
            and v.body.func.attr == "copy"
            and isinstance(v.body.func.value, ast.Subscript)
            and isinstance(v.body.func.value.value, ast.Name)
            and isinstance(v.body.func.value.slice, ast.Call)
            and isinstance(v.body.func.value.slice.func, ast.Attribute)
            and v.body.func.value.slice.func.attr == "choice"
        ):
            arr_name = v.body.func.value.value.id
            ch = v.body.func.value.slice
            npop_node = ch.args[0] if ch.args else None
            size_node = None
            replace_false = False
            for kw in ch.keywords:
                if kw.arg == "size":
                    size_node = kw.value
                elif kw.arg == "replace" and isinstance(kw.value, ast.Constant) and kw.value.value is False:
                    replace_false = True
            if npop_node is None or size_node is None or not replace_false:
                raise NotImplementedError("choice gather ifexp supports choice(n, size=k, replace=False)")
            npop_expr = self.expr(npop_node)
            k_expr = self.expr(size_node)
            self.o.w(f"if ({self.expr(v.test)}) then")
            self.o.push()
            self.o.w(f"if (allocated({t.id})) deallocate({t.id})")
            self.o.w(f"allocate({t.id}(1:{k_expr}))")
            self.o.w("block")
            self.o.push()
            self.o.w("integer :: i_ch")
            self.o.w("integer, allocatable :: idx_ch(:)")
            self.o.w(f"allocate(idx_ch(1:{k_expr}))")
            self.o.w(f"call random_choice_norep({npop_expr}, {k_expr}, idx_ch)")
            self.o.w(f"do i_ch = 1, {k_expr}")
            self.o.push()
            self.o.w(f"{t.id}(i_ch) = {arr_name}(idx_ch(i_ch) + 1)")
            self.o.pop()
            self.o.w("end do")
            self.o.w("if (allocated(idx_ch)) deallocate(idx_ch)")
            self.o.pop()
            self.o.w("end block")
            self.o.pop()
            self.o.w("else")
            self.o.push()
            self.o.w(f"{t.id} = {self.expr(v.orelse)}")
            self.o.pop()
            self.o.w("end if")
            return

        # x = rng.choice(arr, size=k, replace=False) if ... (direct vector choice)
        if (
            isinstance(t, ast.Name)
            and isinstance(v, ast.Call)
            and isinstance(v.func, ast.Attribute)
            and v.func.attr == "choice"
            and v.args
            and isinstance(v.args[0], ast.Name)
        ):
            arr_name = v.args[0].id
            size_node = None
            replace_false = False
            for kw in v.keywords:
                if kw.arg == "size":
                    size_node = kw.value
                elif kw.arg == "replace" and isinstance(kw.value, ast.Constant) and kw.value.value is False:
                    replace_false = True
            if size_node is not None and replace_false:
                k_expr = self.expr(size_node)
                self.o.w(f"if (allocated({t.id})) deallocate({t.id})")
                self.o.w(f"allocate({t.id}(1:{k_expr}))")
                self.o.w("block")
                self.o.push()
                self.o.w("integer :: i_ch")
                self.o.w("integer, allocatable :: idx_ch(:)")
                self.o.w(f"allocate(idx_ch(1:{k_expr}))")
                self.o.w(f"call random_choice_norep(size({arr_name}), {k_expr}, idx_ch)")
                self.o.w(f"do i_ch = 1, {k_expr}")
                self.o.push()
                self.o.w(f"{t.id}(i_ch) = {arr_name}(idx_ch(i_ch) + 1)")
                self.o.pop()
                self.o.w("end do")
                self.o.w("if (allocated(idx_ch)) deallocate(idx_ch)")
                self.o.pop()
                self.o.w("end block")
                return

        # rng = np.random.default_rng(seed)
        if (
            isinstance(t, ast.Name)
            and isinstance(v, ast.Call)
            and isinstance(v.func, ast.Attribute)
            and isinstance(v.func.value, ast.Attribute)
            and isinstance(v.func.value.value, ast.Name)
            and v.func.value.value.id == "np"
            and v.func.value.attr == "random"
            and v.func.attr == "default_rng"
        ):
            seed_node = None
            if v.args:
                seed_node = v.args[0]
            for kw in v.keywords:
                if kw.arg == "seed":
                    seed_node = kw.value
            if seed_node is None or is_none(seed_node):
                self.o.w("call random_seed()")
            else:
                seed_expr = self.expr(seed_node)
                self.o.w("block")
                self.o.push()
                self.o.w("integer :: nseed_rng, i_rng")
                self.o.w("integer, allocatable :: seed_rng(:)")
                self.o.w("call random_seed(size=nseed_rng)")
                self.o.w("allocate(seed_rng(1:nseed_rng))")
                self.o.w("do i_rng = 1, nseed_rng")
                self.o.w(f"   seed_rng(i_rng) = int({seed_expr}) + 104729 * (i_rng - 1)")
                self.o.w("end do")
                self.o.w("call random_seed(put=seed_rng)")
                self.o.w("if (allocated(seed_rng)) deallocate(seed_rng)")
                self.o.pop()
                self.o.w("end block")
            # Keep a placeholder scalar for the RNG object name.
            self.o.w(f"{t.id} = 0")
            return

        # x = np.random.normal(size=n [,loc=.., scale=..]) or rng.normal(...)
        if (
            isinstance(t, ast.Name)
            and isinstance(v, ast.Call)
            and isinstance(v.func, ast.Attribute)
            and v.func.attr == "normal"
        ):
            size_node = None
            loc_node = None
            scale_node = None
            if v.args:
                size_node = v.args[0]
            for kw in v.keywords:
                if kw.arg == "size":
                    size_node = kw.value
                elif kw.arg == "loc":
                    loc_node = kw.value
                elif kw.arg == "scale":
                    scale_node = kw.value
            if size_node is None:
                raise NotImplementedError("np.random.normal requires size=... in this transpiler")
            self.o.w(f"if (allocated({t.id})) deallocate({t.id})")
            n_expr = None
            if isinstance(size_node, (ast.Tuple, ast.List)):
                if len(size_node.elts) == 2:
                    n0 = self.expr(size_node.elts[0])
                    n1 = self.expr(size_node.elts[1])
                    self.o.w(f"allocate({t.id}(1:{n0},1:{n1}))")
                    self.o.w("block")
                    self.o.push()
                    self.o.w("real(kind=dp), allocatable :: tmp_norm(:)")
                    self.o.w(f"allocate(tmp_norm(1:({n0})*({n1})))")
                    self.o.w("call random_normal_vec(tmp_norm)")
                    self.o.w(f"{t.id} = reshape(tmp_norm, [({n0}), ({n1})])")
                    self.o.w("if (allocated(tmp_norm)) deallocate(tmp_norm)")
                    self.o.pop()
                    self.o.w("end block")
                elif len(size_node.elts) == 1:
                    n_expr = self.expr(size_node.elts[0])
                    self.o.w(f"allocate({t.id}(1:{n_expr}))")
                    self.o.w(f"call random_normal_vec({t.id})")
                else:
                    raise NotImplementedError("np.random.normal size tuple rank > 2 not supported")
            else:
                n_expr = self.expr(size_node)
                self.o.w(f"allocate({t.id}(1:{n_expr}))")
                self.o.w(f"call random_normal_vec({t.id})")

            # Special case: gather parameters by class labels, e.g.
            # rng.normal(loc=mu[z], scale=sigma[z], size=n)
            loc_gather = (
                isinstance(loc_node, ast.Subscript)
                and isinstance(loc_node.value, ast.Name)
                and isinstance(loc_node.slice, ast.Name)
            )
            scale_gather = (
                isinstance(scale_node, ast.Subscript)
                and isinstance(scale_node.value, ast.Name)
                and isinstance(scale_node.slice, ast.Name)
            )
            if n_expr is not None and loc_gather and scale_gather and loc_node.slice.id == scale_node.slice.id:
                loc_arr = loc_node.value.id
                scale_arr = scale_node.value.id
                idx_arr = loc_node.slice.id
                self.o.w("block")
                self.o.push()
                self.o.w("integer :: i_norm")
                self.o.w(f"do i_norm = 1, {n_expr}")
                self.o.w(
                    f"   {t.id}(i_norm) = {loc_arr}({idx_arr}(i_norm) + 1) + {scale_arr}({idx_arr}(i_norm) + 1) * {t.id}(i_norm)"
                )
                self.o.w("end do")
                self.o.pop()
                self.o.w("end block")
            else:
                if loc_node is not None:
                    self.o.w(f"{t.id} = {t.id} + ({self.expr(loc_node)})")
                if scale_node is not None:
                    self.o.w(f"{t.id} = ({self.expr(scale_node)}) * {t.id}")
            return

        # x = rng.uniform(low, high, size=...) / np.random.uniform(...)
        if (
            isinstance(t, ast.Name)
            and isinstance(v, ast.Call)
            and isinstance(v.func, ast.Attribute)
            and v.func.attr == "uniform"
        ):
            low_node = None
            high_node = None
            size_node = None
            if len(v.args) >= 1:
                low_node = v.args[0]
            if len(v.args) >= 2:
                high_node = v.args[1]
            if len(v.args) >= 3:
                size_node = v.args[2]
            for kw in v.keywords:
                if kw.arg in {"low", "loc"}:
                    low_node = kw.value
                elif kw.arg in {"high", "scale"}:
                    high_node = kw.value
                elif kw.arg == "size":
                    size_node = kw.value
            if low_node is None:
                low_node = ast.Constant(value=0.0)
            if high_node is None:
                high_node = ast.Constant(value=1.0)
            low_expr = self.expr(low_node)
            high_expr = self.expr(high_node)
            if size_node is None:
                self.o.w(f"call random_number({t.id})")
                self.o.w(f"{t.id} = ({low_expr}) + (({high_expr}) - ({low_expr})) * {t.id}")
                return
            self.o.w(f"if (allocated({t.id})) deallocate({t.id})")
            if isinstance(size_node, (ast.Tuple, ast.List)):
                if len(size_node.elts) == 2:
                    n0 = self.expr(size_node.elts[0])
                    n1 = self.expr(size_node.elts[1])
                    self.o.w(f"allocate({t.id}(1:{n0},1:{n1}))")
                elif len(size_node.elts) == 1:
                    n0 = self.expr(size_node.elts[0])
                    self.o.w(f"allocate({t.id}(1:{n0}))")
                else:
                    raise NotImplementedError("uniform size tuple rank > 2 not supported")
            else:
                n0 = self.expr(size_node)
                self.o.w(f"allocate({t.id}(1:{n0}))")
            self.o.w(f"call random_number({t.id})")
            self.o.w(f"{t.id} = ({low_expr}) + (({high_expr}) - ({low_expr})) * {t.id}")
            return

        # z = rng.integers(low, high, size=...)
        if (
            isinstance(t, ast.Name)
            and isinstance(v, ast.Call)
            and isinstance(v.func, ast.Attribute)
            and v.func.attr == "integers"
        ):
            low_node = None
            high_node = None
            size_node = None
            if len(v.args) >= 1:
                low_node = v.args[0]
            if len(v.args) >= 2:
                high_node = v.args[1]
            if len(v.args) >= 3:
                size_node = v.args[2]
            for kw in v.keywords:
                if kw.arg == "low":
                    low_node = kw.value
                elif kw.arg == "high":
                    high_node = kw.value
                elif kw.arg == "size":
                    size_node = kw.value
            if low_node is None:
                low_node = ast.Constant(value=0)
            if high_node is None:
                raise NotImplementedError("rng.integers requires high")
            low_expr = self.expr(low_node)
            high_expr = self.expr(high_node)
            if size_node is None:
                self.o.w("block")
                self.o.push()
                self.o.w("real(kind=dp) :: u_int")
                self.o.w("call random_number(u_int)")
                self.o.w(f"{t.id} = int({low_expr}) + int(u_int * real(max(1, int({high_expr}) - int({low_expr})), kind=dp))")
                self.o.pop()
                self.o.w("end block")
                return
            self.o.w(f"if (allocated({t.id})) deallocate({t.id})")
            if isinstance(size_node, (ast.Tuple, ast.List)):
                if len(size_node.elts) == 2:
                    n0 = self.expr(size_node.elts[0])
                    n1 = self.expr(size_node.elts[1])
                    self.o.w(f"allocate({t.id}(1:{n0},1:{n1}))")
                elif len(size_node.elts) == 1:
                    n0 = self.expr(size_node.elts[0])
                    self.o.w(f"allocate({t.id}(1:{n0}))")
                else:
                    raise NotImplementedError("integers size tuple rank > 2 not supported")
            else:
                n0 = self.expr(size_node)
                self.o.w(f"allocate({t.id}(1:{n0}))")
            self.o.w("block")
            self.o.push()
            self.o.w("integer :: i_rng")
            self.o.w("real(kind=dp), allocatable :: u_rng(:)")
            self.o.w(f"allocate(u_rng(1:size({t.id})))")
            self.o.w("call random_number(u_rng)")
            self.o.w(f"do i_rng = 1, size({t.id})")
            self.o.push()
            self.o.w(
                f"{t.id}(i_rng) = int({low_expr}) + int(u_rng(i_rng) * real(max(1, int({high_expr}) - int({low_expr})), kind=dp))"
            )
            self.o.pop()
            self.o.w("end do")
            self.o.w("if (allocated(u_rng)) deallocate(u_rng)")
            self.o.pop()
            self.o.w("end block")
            return

        # z = rng.choice(2, size=n, p=w)
        if (
            isinstance(t, ast.Name)
            and isinstance(v, ast.Call)
            and isinstance(v.func, ast.Attribute)
            and v.func.attr == "permutation"
            and len(v.args) >= 1
        ):
            arr_expr = self.expr(v.args[0])
            self.o.w(f"if (allocated({t.id})) deallocate({t.id})")
            self.o.w(f"allocate({t.id}(1:size({arr_expr})))")
            self.o.w("block")
            self.o.push()
            self.o.w("integer, allocatable :: idx_ch(:)")
            self.o.w(f"allocate(idx_ch(1:size({arr_expr})))")
            self.o.w(f"call random_choice_norep(size({arr_expr}), size({arr_expr}), idx_ch)")
            self.o.w(f"{t.id} = {arr_expr}(idx_ch + 1)")
            self.o.w("if (allocated(idx_ch)) deallocate(idx_ch)")
            self.o.pop()
            self.o.w("end block")
            return

        # z = rng.choice(2, size=n, p=w)
        if (
            isinstance(t, ast.Name)
            and isinstance(v, ast.Call)
            and isinstance(v.func, ast.Attribute)
            and v.func.attr == "choice"
        ):
            npop_node = v.args[0] if v.args else None
            size_node = None
            p_node = None
            replace_false = False
            for kw in v.keywords:
                if kw.arg == "size":
                    size_node = kw.value
                elif kw.arg == "p":
                    p_node = kw.value
                elif kw.arg == "replace" and isinstance(kw.value, ast.Constant) and kw.value.value is False:
                    replace_false = True
            if size_node is None:
                raise NotImplementedError("rng.choice requires size=... in this transpiler")
            n_expr = self.expr(size_node)
            if p_node is not None:
                p_expr = self.expr(p_node)
                self.o.w(f"if (allocated({t.id})) deallocate({t.id})")
                self.o.w(f"allocate({t.id}(1:{n_expr}))")
                if (
                    isinstance(npop_node, ast.Constant)
                    and isinstance(npop_node.value, int)
                    and npop_node.value == 2
                ):
                    self.o.w(f"call random_choice2({p_expr}, {n_expr}, {t.id})")
                else:
                    self.o.w(f"call random_choice_prob({p_expr}, {n_expr}, {t.id})")
                return
            if replace_false and isinstance(npop_node, ast.Name):
                arr_expr = self.expr(npop_node)
                self.o.w(f"if (allocated({t.id})) deallocate({t.id})")
                self.o.w(f"allocate({t.id}(1:{n_expr}))")
                self.o.w("block")
                self.o.push()
                self.o.w("integer, allocatable :: idx_ch(:)")
                self.o.w(f"allocate(idx_ch(1:{n_expr}))")
                self.o.w(f"call random_choice_norep(size({arr_expr}), {n_expr}, idx_ch)")
                self.o.w(f"{t.id} = {arr_expr}(idx_ch + 1)")
                self.o.w("if (allocated(idx_ch)) deallocate(idx_ch)")
                self.o.pop()
                self.o.w("end block")
                return
            if replace_false and npop_node is not None:
                npop_expr = self.expr(npop_node)
                self.o.w(f"if (allocated({t.id})) deallocate({t.id})")
                self.o.w(f"allocate({t.id}(1:{n_expr}))")
                self.o.w(f"call random_choice_norep({npop_expr}, {n_expr}, {t.id})")
                return
            # replacement sampling (default replace=True)
            if npop_node is not None and self._rank_expr(npop_node) > 0:
                arr_expr = self.expr(npop_node)
                arr_kind = self._expr_kind(npop_node)
                self.o.w(f"if (allocated({t.id})) deallocate({t.id})")
                self.o.w(f"allocate({t.id}(1:{n_expr}))")
                self.o.w("block")
                self.o.push()
                self.o.w("integer :: i_ch, j_ch")
                self.o.w("real(kind=dp) :: u_ch")
                if arr_kind == "real":
                    self.o.w("real(kind=dp), allocatable :: pool_ch(:)")
                elif arr_kind == "logical":
                    self.o.w("logical, allocatable :: pool_ch(:)")
                else:
                    self.o.w("integer, allocatable :: pool_ch(:)")
                self.o.w(f"pool_ch = {arr_expr}")
                self.o.w(f"do i_ch = 1, {n_expr}")
                self.o.push()
                self.o.w("call random_number(u_ch)")
                self.o.w("j_ch = 1 + int(u_ch * real(size(pool_ch), kind=dp))")
                self.o.w(f"if (j_ch < 1) j_ch = 1")
                self.o.w("if (j_ch > size(pool_ch)) j_ch = size(pool_ch)")
                self.o.w(f"{t.id}(i_ch) = pool_ch(j_ch)")
                self.o.pop()
                self.o.w("end do")
                self.o.w("if (allocated(pool_ch)) deallocate(pool_ch)")
                self.o.pop()
                self.o.w("end block")
                return
            if npop_node is not None:
                npop_expr = self.expr(npop_node)
                self.o.w(f"if (allocated({t.id})) deallocate({t.id})")
                self.o.w(f"allocate({t.id}(1:{n_expr}))")
                self.o.w("block")
                self.o.push()
                self.o.w("integer :: i_ch")
                self.o.w("real(kind=dp) :: u_ch")
                self.o.w(f"do i_ch = 1, {n_expr}")
                self.o.push()
                self.o.w("call random_number(u_ch)")
                self.o.w(f"{t.id}(i_ch) = int(u_ch * real(max(1, int({npop_expr})), kind=dp))")
                self.o.pop()
                self.o.w("end do")
                self.o.pop()
                self.o.w("end block")
                return
            raise NotImplementedError("unsupported rng.choice form")

        # list = []
        if isinstance(t, ast.Name) and isinstance(v, ast.List) and len(v.elts) == 0:
            name = t.id
            cnt = self.list_counts.get(name, None)
            if cnt is None:
                raise NotImplementedError("list without count mapping")
            if self.context == "flat":
                self.o.w(f"if (.not. allocated({name})) allocate({name}(1:n))")
            self.o.w(f"{cnt} = 0")
            self.o.w(f"{name} = 0")
            return

        # np.empty(shape, dtype=np.int_ / np.bool_)
        if (
            isinstance(t, ast.Name)
            and isinstance(v, ast.Call)
            and isinstance(v.func, ast.Attribute)
            and isinstance(v.func.value, ast.Name)
            and v.func.value.id == "np"
            and v.func.attr == "empty"
            and len(v.args) >= 1
        ):
            name = t.id
            shp = v.args[0]
            self.o.w(f"if (allocated({name})) deallocate({name})")
            if isinstance(shp, (ast.Tuple, ast.List)):
                dims = ", ".join(f"1:{self.expr(e)}" for e in shp.elts)
            else:
                dims = f"1:{self.expr(shp)}"
            self.o.w(f"allocate({name}({dims}))")
            return

        # np.zeros(shape, dtype=...) / np.ones(shape, dtype=...)
        if (
            isinstance(t, ast.Name)
            and isinstance(v, ast.Call)
            and isinstance(v.func, ast.Attribute)
            and isinstance(v.func.value, ast.Name)
            and v.func.value.id == "np"
            and v.func.attr in {"zeros", "ones"}
            and len(v.args) >= 1
        ):
            name = t.id
            shp = v.args[0]
            fill_expr = "0"
            dtype_txt = self._np_dtype_text(v)
            if "float" in dtype_txt or (name in self.alloc_reals):
                fill_expr = "0.0_dp"
            elif "bool" in dtype_txt:
                fill_expr = ".false."
            if v.func.attr == "ones":
                if "float" in dtype_txt or (name in self.alloc_reals):
                    fill_expr = "1.0_dp"
                elif "bool" in dtype_txt:
                    fill_expr = ".true."
                else:
                    fill_expr = "1"
            self.o.w(f"if (allocated({name})) deallocate({name})")
            if isinstance(shp, (ast.Tuple, ast.List)):
                dims = ", ".join(f"1:{self.expr(e)}" for e in shp.elts)
            else:
                dims = f"1:{self.expr(shp)}"
            self.o.w(f"allocate({name}({dims}))")
            self.o.w(f"{name} = {fill_expr}")
            return

        # np.full(shape, value, dtype=...)
        if (
            isinstance(t, ast.Name)
            and isinstance(v, ast.Call)
            and isinstance(v.func, ast.Attribute)
            and isinstance(v.func.value, ast.Name)
            and v.func.value.id == "np"
            and v.func.attr == "full"
            and len(v.args) >= 2
        ):
            name = t.id
            shp = v.args[0]
            fill_expr = self.expr(v.args[1])
            self.o.w(f"if (allocated({name})) deallocate({name})")
            if isinstance(shp, (ast.Tuple, ast.List)):
                dims = ", ".join(f"1:{self.expr(e)}" for e in shp.elts)
            else:
                dims = f"1:{self.expr(shp)}"
            self.o.w(f"allocate({name}({dims}))")
            self.o.w(f"{name} = {fill_expr}")
            return

        # x = np.triu(a[, k]) / x = np.tril(a[, k])
        if (
            isinstance(t, ast.Name)
            and isinstance(v, ast.Call)
            and isinstance(v.func, ast.Attribute)
            and isinstance(v.func.value, ast.Name)
            and v.func.value.id == "np"
            and v.func.attr in {"triu", "tril"}
            and len(v.args) >= 1
        ):
            name = t.id
            a0 = self.expr(v.args[0])
            k_node = None
            if len(v.args) >= 2:
                k_node = v.args[1]
            for kw in v.keywords:
                if kw.arg == "k":
                    k_node = kw.value
                    break
            k_expr = self.expr(k_node) if k_node is not None else "0"
            ksrc = self._expr_kind(v.args[0])
            zval = "0.0_dp" if ksrc == "real" else "0"
            self.o.w(f"if (allocated({name})) deallocate({name})")
            self.o.w(f"allocate({name}(1:size({a0},1), 1:size({a0},2)))")
            self.o.w(f"{name} = {zval}")
            self.o.w("block")
            self.o.push()
            self.o.w("integer :: i_tri, j_tri")
            self.o.w(f"do i_tri = 1, size({a0},1)")
            self.o.push()
            self.o.w(f"do j_tri = 1, size({a0},2)")
            self.o.push()
            if v.func.attr == "triu":
                self.o.w(f"if (j_tri >= i_tri + ({k_expr})) {name}(i_tri,j_tri) = {a0}(i_tri,j_tri)")
            else:
                self.o.w(f"if (j_tri <= i_tri + ({k_expr})) {name}(i_tri,j_tri) = {a0}(i_tri,j_tri)")
            self.o.pop()
            self.o.w("end do")
            self.o.pop()
            self.o.w("end do")
            self.o.pop()
            self.o.w("end block")
            return

        # x = np.array([...]) / np.asarray([...])
        if (
            isinstance(t, ast.Name)
            and isinstance(v, ast.Call)
            and isinstance(v.func, ast.Attribute)
            and isinstance(v.func.value, ast.Name)
            and v.func.value.id == "np"
            and v.func.attr in {"array", "asarray"}
            and len(v.args) >= 1
            and isinstance(v.args[0], ast.List)
        ):
            name = t.id
            elts = v.args[0].elts
            self.o.w(f"if (allocated({name})) deallocate({name})")
            if elts and all(isinstance(e, ast.List) for e in elts):
                nrow = len(elts)
                ncol = len(elts[0].elts)
                if not all(len(e.elts) == ncol for e in elts):
                    raise NotImplementedError("np.array nested list rows must have equal lengths")
                self.o.w(f"allocate({name}(1:{nrow},1:{ncol}))")
                flat_nodes = []
                for r in elts:
                    flat_nodes.extend(r.elts)
                vals = ", ".join(self.expr(e) for e in flat_nodes)
                self.o.w(f"{name} = transpose(reshape([{vals}], [{ncol}, {nrow}]))")
            else:
                n_expr = str(len(elts))
                self.o.w(f"allocate({name}(1:{n_expr}))")
                vals = ", ".join(self.expr(e) for e in elts)
                self.o.w(f"{name} = [{vals}]")
            return

        # x = np.array(list_var, dtype=...) / np.asarray(list_var, dtype=...)
        if (
            isinstance(t, ast.Name)
            and isinstance(v, ast.Call)
            and isinstance(v.func, ast.Attribute)
            and isinstance(v.func.value, ast.Name)
            and v.func.value.id == "np"
            and v.func.attr in {"array", "asarray"}
            and len(v.args) >= 1
            and isinstance(v.args[0], ast.Name)
            and v.args[0].id in self.list_counts
        ):
            name = t.id
            src = v.args[0].id
            cnt = self.list_counts[src]
            dtype_txt = ""
            for kw in v.keywords:
                if kw.arg == "dtype":
                    if isinstance(kw.value, ast.Name):
                        dtype_txt = kw.value.id.lower()
                    elif (
                        isinstance(kw.value, ast.Attribute)
                        and isinstance(kw.value.value, ast.Name)
                        and kw.value.value.id == "np"
                    ):
                        dtype_txt = kw.value.attr.lower()
            self.o.w(f"if (allocated({name})) deallocate({name})")
            self.o.w(f"allocate({name}(1:{cnt}))")
            if "float" in dtype_txt:
                self.o.w(f"{name} = real({src}(1:{cnt}), kind=dp)")
            elif "bool" in dtype_txt:
                self.o.w(f"{name} = {src}(1:{cnt})")
            else:
                self.o.w(f"{name} = int({src}(1:{cnt}))")
            return

        # x = np.asarray(x, dtype=...) is a no-op for transpiled Fortran typing
        if (
            isinstance(t, ast.Name)
            and isinstance(v, ast.Call)
            and isinstance(v.func, ast.Attribute)
            and isinstance(v.func.value, ast.Name)
            and v.func.value.id == "np"
            and v.func.attr == "asarray"
            and len(v.args) >= 1
            and isinstance(v.args[0], ast.Name)
            and v.args[0].id == t.id
        ):
            return

        # x = np.squeeze(x, ...) can change rank; keep rank-stable in-place assignment.
        if (
            isinstance(t, ast.Name)
            and isinstance(v, ast.Call)
            and isinstance(v.func, ast.Attribute)
            and isinstance(v.func.value, ast.Name)
            and v.func.value.id == "np"
            and v.func.attr == "squeeze"
            and len(v.args) >= 1
            and isinstance(v.args[0], ast.Name)
            and v.args[0].id == t.id
        ):
            return

        # x = np.sort(a)
        if (
            isinstance(t, ast.Name)
            and isinstance(v, ast.Call)
            and isinstance(v.func, ast.Attribute)
            and isinstance(v.func.value, ast.Name)
            and v.func.value.id == "np"
            and v.func.attr == "sort"
            and len(v.args) >= 1
        ):
            name = t.id
            a0 = self.expr(v.args[0])
            self.o.w(f"if (allocated({name})) deallocate({name})")
            self.o.w(f"allocate({name}(1:size({a0})))")
            self.o.w(f"{name} = {a0}")
            self.o.w(f"call sort_vec({name})")
            return

        # x = np.argsort(a)
        if (
            isinstance(t, ast.Name)
            and isinstance(v, ast.Call)
            and isinstance(v.func, ast.Attribute)
            and isinstance(v.func.value, ast.Name)
            and v.func.value.id == "np"
            and v.func.attr == "argsort"
            and len(v.args) >= 1
        ):
            name = t.id
            a0 = self.expr(v.args[0])
            self.o.w(f"if (allocated({name})) deallocate({name})")
            self.o.w(f"allocate({name}(1:size({a0})))")
            self.o.w(f"call argsort({a0}, {name})")
            return

        # x = np.argmax(a, axis=1) / np.argmin(a, axis=1)
        if (
            isinstance(t, ast.Name)
            and isinstance(v, ast.Call)
            and isinstance(v.func, ast.Attribute)
            and isinstance(v.func.value, ast.Name)
            and v.func.value.id == "np"
            and v.func.attr in {"argmax", "argmin"}
            and len(v.args) >= 1
        ):
            axis_node = None
            for kw in v.keywords:
                if kw.arg == "axis":
                    axis_node = kw.value
            if isinstance(axis_node, ast.Constant) and axis_node.value == 1:
                name = t.id
                a0 = self.expr(v.args[0])
                self.o.w(f"if (allocated({name})) deallocate({name})")
                self.o.w(f"allocate({name}(1:size({a0},1)))")
                self.o.w("block")
                self.o.push()
                self.o.w("integer :: i_am, j_am, best_j")
                self.o.w("real(kind=dp) :: best_v")
                self.o.w(f"do i_am = 1, size({a0},1)")
                self.o.push()
                self.o.w("best_j = 1")
                self.o.w(f"best_v = {a0}(i_am,1)")
                self.o.w(f"do j_am = 2, size({a0},2)")
                self.o.push()
                cmp = ">" if v.func.attr == "argmax" else "<"
                self.o.w(f"if ({a0}(i_am,j_am) {cmp} best_v) then")
                self.o.push()
                self.o.w(f"best_v = {a0}(i_am,j_am)")
                self.o.w("best_j = j_am")
                self.o.pop()
                self.o.w("end if")
                self.o.pop()
                self.o.w("end do")
                self.o.w(f"{name}(i_am) = best_j")
                self.o.pop()
                self.o.w("end do")
                self.o.pop()
                self.o.w("end block")
                return

        # x = np.where(cond, a, b)
        if (
            isinstance(t, ast.Name)
            and isinstance(v, ast.Call)
            and isinstance(v.func, ast.Attribute)
            and isinstance(v.func.value, ast.Name)
            and v.func.value.id == "np"
            and v.func.attr == "where"
            and len(v.args) == 3
        ):
            name = t.id
            cond = self.expr(v.args[0])
            a1 = self.expr(v.args[1])
            a2 = self.expr(v.args[2])
            self.o.w(f"{name} = merge({a1}, {a2}, {cond})")
            return

        # logical sieve init
        if isinstance(t, ast.Name) and isinstance(v, ast.BinOp) and isinstance(v.op, ast.Mult):
            if isinstance(v.left, ast.List) and len(v.left.elts) == 1 and is_bool_const(v.left.elts[0]):
                name = t.id
                self.o.w(f"if (allocated({name})) deallocate({name})")
                self.o.w(f"allocate({name}(0:n))")
                self.o.w(f"{name} = .true.")
                return

        # subscript assignment
        if isinstance(t, ast.Subscript):
            # NumPy boolean-mask assignment: a[m] = v
            if self._expr_kind(t.slice) == "logical":
                rhs_rank = self._rank_expr(v)
                if rhs_rank == 0:
                    # Scalar masked assignment can be represented without LHS pack.
                    self.o.w(f"{self.expr(t.value)} = merge({self.expr(v)}, {self.expr(t.value)}, {self.expr(t.slice)})")
                else:
                    self.o.w(f"where (({self.expr(t.slice)}))")
                    self.o.push()
                    self.o.w(f"{self.expr(t.value)} = {self.expr(v)}")
                    self.o.pop()
                    self.o.w("end where")
                return
            if isinstance(t.slice, ast.Slice) and t.slice.lower is None and t.slice.upper is None and t.slice.step is None:
                if not isinstance(t.value, ast.Name):
                    raise NotImplementedError("slice assignment target must be name")
                self.o.w(f"{t.value.id} = {self.expr(v)}")
                return
            self.o.w(f"{self.expr(t)} = {self.expr(v)}")
            return

        # NEW: fallback for name = <simple expr>  (e.g., largest = i)
        if isinstance(t, ast.Name):
            if isinstance(v, ast.IfExp):
                self.o.w(f"if ({self.expr(v.test)}) then")
                self.o.push()
                self.o.w(f"{t.id} = {self.expr(v.body)}")
                self.o.pop()
                self.o.w("else")
                self.o.push()
                self.o.w(f"{t.id} = {self.expr(v.orelse)}")
                self.o.pop()
                self.o.w("end if")
                return
            self.o.w(f"{t.id} = {self.expr(v)}")
            return

        stmt_txt = ast.unparse(node) if hasattr(ast, "unparse") else ast.dump(node, include_attributes=False)
        raise NotImplementedError(f"unsupported assign: {stmt_txt}")

    def visit_AugAssign(self, node):
        self._emit_comments_for(node)
        # supports common augmented assignments on scalar names
        if not isinstance(node.target, ast.Name):
            raise NotImplementedError("augassign target must be name")
        name = node.target.id
        rhs = self.expr(node.value)
        if isinstance(node.op, ast.Add):
            self.o.w(f"{name} = {name} + {rhs}")
            return
        if isinstance(node.op, ast.Sub):
            self.o.w(f"{name} = {name} - {rhs}")
            return
        if isinstance(node.op, ast.Mult):
            self.o.w(f"{name} = {name} * {rhs}")
            return
        if isinstance(node.op, ast.Div):
            self.o.w(f"{name} = {name} / {rhs}")
            return
        if isinstance(node.op, ast.FloorDiv):
            # Python // is floor division; Fortran integer "/" truncates toward zero.
            # For current supported integer workflows this is the closest direct mapping.
            self.o.w(f"{name} = {name} / {rhs}")
            return
        if isinstance(node.op, ast.Mod):
            self.o.w(f"{name} = mod({name}, {rhs})")
            return
        raise NotImplementedError("unsupported augassign op")

    def visit_Return(self, node):
        self._emit_comments_for(node)
        if node.value is not None:
            if self.function_result_name is None:
                raise NotImplementedError("return value only supported in function context")
            self.o.w(f"{self.function_result_name} = {self.expr(node.value)}")
        self.o.w("return")

    def visit_If(self, node):
        self._emit_comments_for(node)
        def _is_seed_noop_call(c):
            return (
                isinstance(c, ast.Call)
                and isinstance(c.func, ast.Attribute)
                and (
                    (
                        isinstance(c.func.value, ast.Name)
                        and c.func.value.id == "random"
                        and c.func.attr == "seed"
                    )
                    or (
                        isinstance(c.func.value, ast.Attribute)
                        and isinstance(c.func.value.value, ast.Name)
                        and c.func.value.value.id == "np"
                        and c.func.value.attr == "random"
                        and c.func.attr == "seed"
                    )
                )
            )

        def _is_noop_stmt(s):
            if isinstance(s, ast.Pass):
                return True
            if isinstance(s, ast.Expr) and isinstance(s.value, ast.Call):
                if _is_seed_noop_call(s.value):
                    return True
            return False

        body_eff = [s for s in node.body if not _is_noop_stmt(s)]
        else_eff = [s for s in node.orelse if not _is_noop_stmt(s)] if node.orelse else []
        if not body_eff and not else_eff:
            return

        # Preserve Python short-circuit semantics for simple boolean chains.
        if isinstance(node.test, ast.BoolOp) and len(node.test.values) == 2:
            a, b = node.test.values
            if isinstance(node.test.op, ast.Or):
                self.o.w(f"if ({self.expr(a)}) then")
                self.o.push()
                for s in body_eff:
                    self.visit(s)
                self.o.pop()
                self.o.w("else")
                self.o.push()
                self.o.w(f"if ({self.expr(b)}) then")
                self.o.push()
                for s in body_eff:
                    self.visit(s)
                self.o.pop()
                if else_eff:
                    self.o.w("else")
                    self.o.push()
                    for s in else_eff:
                        self.visit(s)
                    self.o.pop()
                self.o.w("end if")
                self.o.pop()
                self.o.w("end if")
                return
            if isinstance(node.test.op, ast.And):
                self.o.w(f"if ({self.expr(a)}) then")
                self.o.push()
                self.o.w(f"if ({self.expr(b)}) then")
                self.o.push()
                for s in body_eff:
                    self.visit(s)
                self.o.pop()
                if else_eff:
                    self.o.w("else")
                    self.o.push()
                    for s in else_eff:
                        self.visit(s)
                    self.o.pop()
                self.o.w("end if")
                self.o.pop()
                if else_eff:
                    self.o.w("else")
                    self.o.push()
                    for s in else_eff:
                        self.visit(s)
                    self.o.pop()
                self.o.w("end if")
                return

        self.o.w(f"if ({self.expr(node.test)}) then")
        self.o.push()
        for s in body_eff:
            self.visit(s)
        self.o.pop()
        if else_eff:
            self.o.w("else")
            self.o.push()
            for s in else_eff:
                self.visit(s)
            self.o.pop()
        self.o.w("end if")

    def visit_Break(self, node):
        self._emit_comments_for(node)
        self.o.w("exit")

    def visit_Continue(self, node):
        self._emit_comments_for(node)
        self.o.w("cycle")

    def _range_parts(self, call_node):
        args = call_node.args
        if len(args) == 1:
            start = ast.Constant(value=0)
            stop = args[0]
            step = ast.Constant(value=1)
        elif len(args) == 2:
            start, stop = args
            step = ast.Constant(value=1)
        elif len(args) == 3:
            start, stop, step = args
        else:
            raise NotImplementedError("range with 1,2,3 args only")

        if is_const_int(step) and step.value <= 0:
            raise NotImplementedError("nonpositive range step not supported")

        return start, stop, step

    def _upper_from_stop(self, stop_node):
        # range(a, b) goes to b-1; special-case b == x+1 => upper=x
        if isinstance(stop_node, ast.BinOp) and isinstance(stop_node.op, ast.Add):
            if is_const_int(stop_node.right) and stop_node.right.value == 1:
                return self.expr(stop_node.left)
        return f"({self.expr(stop_node)} - 1)"

    def visit_For(self, node):
        self._emit_comments_for(node)
        if not (isinstance(node.iter, ast.Call) and isinstance(node.iter.func, ast.Name) and node.iter.func.id == "range"):
            raise NotImplementedError("only for .. in range(..) supported")
        if not isinstance(node.target, ast.Name):
            raise NotImplementedError("for target must be a name")

        var = node.target.id
        if var == "_":
            var = "i_"
            self._mark_int(var)
        start, stop, step = self._range_parts(node.iter)
        f_start = self.expr(start)
        f_step = self.expr(step)
        f_upper = self._upper_from_stop(stop)

        if is_const_int(step) and step.value == 1:
            self.o.w(f"do {var} = {f_start}, {f_upper}")
        else:
            self.o.w(f"do {var} = {f_start}, {f_upper}, {f_step}")
        self.o.push()
        for s in node.body:
            self.visit(s)
        self.o.pop()
        self.o.w("end do")

    def visit_While(self, node):
        self._emit_comments_for(node)
        # Python while -> Fortran do while
        self.o.w(f"do while ({self.expr(node.test)})")
        self.o.push()
        for s in node.body:
            self.visit(s)
        self.o.pop()
        self.o.w("end do")

    def visit_Expr(self, node):
        self._emit_comments_for(node)
        if isinstance(node.value, ast.Constant) and isinstance(node.value.value, str):
            # Ignore docstring / standalone string literal expressions.
            return
        if not isinstance(node.value, ast.Call):
            raise NotImplementedError("only call expressions supported")
        c = node.value

        # random.seed(...)
        if (
            isinstance(c.func, ast.Attribute)
            and isinstance(c.func.value, ast.Name)
            and c.func.value.id == "random"
            and c.func.attr == "seed"
        ):
            return
        # np.random.seed(...)
        if (
            isinstance(c.func, ast.Attribute)
            and isinstance(c.func.value, ast.Attribute)
            and isinstance(c.func.value.value, ast.Name)
            and c.func.value.value.id == "np"
            and c.func.value.attr == "random"
            and c.func.attr == "seed"
        ):
            return

        # NumPy display formatting has no Fortran runtime equivalent; ignore.
        if (
            isinstance(c.func, ast.Attribute)
            and isinstance(c.func.value, ast.Name)
            and c.func.value.id == "np"
            and c.func.attr == "set_printoptions"
        ):
            return

        if (
            isinstance(c.func, ast.Attribute)
            and isinstance(c.func.value, ast.Name)
            and c.func.value.id == "np"
            and c.func.attr == "put"
            and len(c.args) >= 3
        ):
            arr = self.expr(c.args[0])
            idx = self.expr(c.args[1])
            vals = self.expr(c.args[2])
            idx_rank = self._rank_expr(c.args[1])
            vals_rank = self._rank_expr(c.args[2])
            vals_kind = self._expr_kind(c.args[2])
            self.o.w("block")
            self.o.push()
            self.o.w("integer :: i_put, n_put")
            if idx_rank > 0:
                self.o.w("integer, allocatable :: idx_put(:)")
            if vals_rank > 0:
                if vals_kind == "int":
                    self.o.w("integer, allocatable :: vals_put(:)")
                elif vals_kind == "logical":
                    self.o.w("logical, allocatable :: vals_put(:)")
                else:
                    self.o.w("real(kind=dp), allocatable :: vals_put(:)")
            if idx_rank > 0:
                self.o.w(f"idx_put = {idx}")
            if vals_rank > 0:
                self.o.w(f"vals_put = {vals}")
            if idx_rank > 0:
                self.o.w("n_put = size(idx_put)")
            else:
                self.o.w("n_put = 1")
            if vals_rank > 0:
                self.o.w("n_put = min(n_put, size(vals_put))")
            self.o.w("do i_put = 1, n_put")
            self.o.push()
            if vals_rank > 0:
                if idx_rank > 0:
                    self.o.w(f"{arr}(idx_put(i_put) + 1) = vals_put(i_put)")
                else:
                    self.o.w(f"{arr}({idx} + 1) = vals_put(i_put)")
            else:
                if idx_rank > 0:
                    self.o.w(f"{arr}(idx_put(i_put) + 1) = {vals}")
                else:
                    self.o.w(f"{arr}({idx} + 1) = {vals}")
            self.o.pop()
            self.o.w("end do")
            self.o.pop()
            self.o.w("end block")
            return

        if (
            isinstance(c.func, ast.Attribute)
            and c.func.attr == "shuffle"
            and len(c.args) == 1
            and isinstance(c.args[0], ast.Name)
        ):
            arr = self.expr(c.args[0])
            self.o.w("block")
            self.o.push()
            self.o.w("integer :: i_sh, j_sh")
            if self._expr_kind(c.args[0]) == "real":
                self.o.w("real(kind=dp) :: tmp_sh")
            else:
                self.o.w("integer :: tmp_sh")
            self.o.w("real(kind=dp) :: u_sh")
            self.o.w(f"do i_sh = size({arr}), 2, -1")
            self.o.push()
            self.o.w("call random_number(u_sh)")
            self.o.w("j_sh = 1 + int(u_sh * real(i_sh, kind=dp))")
            self.o.w("if (j_sh < 1) j_sh = 1")
            self.o.w("if (j_sh > i_sh) j_sh = i_sh")
            self.o.w(f"tmp_sh = {arr}(i_sh)")
            self.o.w(f"{arr}(i_sh) = {arr}(j_sh)")
            self.o.w(f"{arr}(j_sh) = tmp_sh")
            self.o.pop()
            self.o.w("end do")
            self.o.pop()
            self.o.w("end block")
            return

        if isinstance(c.func, ast.Name) and c.func.id == "print":
            if self.context == "compute":
                raise NotImplementedError("print not allowed in compute procedure")
            self._emit_print_call(c)
            return

        if isinstance(c.func, ast.Attribute) and c.func.attr == "append":
            if not isinstance(c.func.value, ast.Name):
                raise NotImplementedError("append target must be a name")
            name = c.func.value.id
            cnt = self.list_counts.get(name, None)
            if cnt is None:
                raise NotImplementedError("append without count mapping")
            val = self.expr(c.args[0])
            self.o.w(f"{cnt} = {cnt} + 1")
            self.o.w(f"{name}({cnt}) = {val}")
            return

        raise NotImplementedError("unsupported expression call")

    def _emit_print_call(self, call):
        if len(call.args) == 0:
            self.o.w("print *")
            return
        if len(call.args) != 1:
            # Conservative multi-argument handling.
            # If list outputs are mixed with other args, emit in parts.
            has_list = any(isinstance(a, ast.Name) and a.id in self.list_counts for a in call.args)
            if has_list:
                for a in call.args:
                    if isinstance(a, ast.Name) and a.id in self.list_counts:
                        cnt = self.list_counts[a.id]
                        self.o.w(f"call print_int_list({a.id}, {cnt})")
                    elif is_const_str(a):
                        self.o.w(f"write(*,{fstr('(a)')}) {fstr(a.value)}")
                    else:
                        self.o.w(f"print *, {self.expr(a)}")
                return
            parts = []
            for a in call.args:
                if is_const_str(a):
                    parts.append(fstr(a.value))
                elif isinstance(a, ast.JoinedStr):
                    raise NotImplementedError("f-string in multi-argument print not supported")
                else:
                    parts.append(self.expr(a))
            self.o.w("print *, " + ", ".join(parts))
            return
        a = call.args[0]

        # print("literal")
        if is_const_str(a):
            self.o.w(f"write(*,{fstr('(a)')}) {fstr(a.value)}")
            return

        # print(f"...{x}...")
        if isinstance(a, ast.JoinedStr):
            fmt_parts = []
            items = []
            for part in a.values:
                if is_const_str(part):
                    fmt_parts.append("a")
                    items.append(fstr(part.value))
                elif isinstance(part, ast.FormattedValue):
                    fcode = "g0"
                    if part.format_spec is not None:
                        # support basic Python format specs like .10f
                        spec = ""
                        if isinstance(part.format_spec, ast.JoinedStr):
                            ok = True
                            chunks = []
                            for sp in part.format_spec.values:
                                if is_const_str(sp):
                                    chunks.append(sp.value)
                                else:
                                    ok = False
                                    break
                            if ok:
                                spec = "".join(chunks).strip()
                        if spec.startswith(".") and spec.endswith("f") and spec[1:-1].isdigit():
                            fcode = f"f0.{spec[1:-1]}"
                    fmt_parts.append(fcode)
                    items.append(self.expr(part.value))
                else:
                    raise NotImplementedError("unsupported f-string part")
            fmt = fstr("(" + ",".join(fmt_parts) + ")")
            self.o.w(f"write(*,{fmt}) " + ", ".join(items))
            return

        # print([])
        if isinstance(a, ast.List) and len(a.elts) == 0:
            self.o.w(f"write(*,{fstr('(a)')}) {fstr('[]')}")
            return

        # print(primes)
        if isinstance(a, ast.Name) and a.id in self.list_counts:
            cnt = self.list_counts[a.id]
            self.o.w(f"call print_int_list({a.id}, {cnt})")
            return

        # fallback
        self.o.w(f"print *, {self.expr(a)}")


# -------------------------
# code generation
# -------------------------

def _analyze_dict_return_spec(fn, params, known_key_kinds=None):
    # Recognize functions that return a local dict variable:
    #   d = {"k": expr, ...}
    #   return d
    ret_name = None
    ret_dict_node = None
    for st in ast.walk(fn):
        if isinstance(st, ast.Return):
            if isinstance(st.value, ast.Name):
                ret_name = st.value.id
                break
            if isinstance(st.value, ast.Dict):
                ret_name = "__direct_return_dict__"
                ret_dict_node = st.value
                break
    if ret_name is None:
        return None

    dict_assign = None
    for st in ast.walk(fn):
        if (
            isinstance(st, ast.Assign)
            and len(st.targets) == 1
            and isinstance(st.targets[0], ast.Name)
            and st.targets[0].id == ret_name
            and isinstance(st.value, ast.Dict)
        ):
            dict_assign = st
            break

    if dict_assign is None and ret_dict_node is None:
        return None

    tmp_out = emit()
    tr0 = translator(tmp_out, params={}, context="flat", list_counts={}, function_result_name=f"{fn.name}_result")
    tr0.prescan(fn.body)
    comps = []
    list_elem_kind = {}
    for st in ast.walk(fn):
        if (
            isinstance(st, ast.Call)
            and isinstance(st.func, ast.Attribute)
            and st.func.attr == "append"
            and isinstance(st.func.value, ast.Name)
            and len(st.args) == 1
        ):
            lst = st.func.value.id
            ek = tr0._expr_kind(st.args[0])
            if ek in {"real", "int", "logical"}:
                prev = list_elem_kind.get(lst, ek)
                if prev == "real" or ek == "real":
                    list_elem_kind[lst] = "real"
                elif prev == "logical" and ek != "logical":
                    list_elem_kind[lst] = "int"
                elif prev == "int" and ek == "logical":
                    list_elem_kind[lst] = "int"
                else:
                    list_elem_kind[lst] = ek
    dict_src = dict_assign.value if dict_assign is not None else ret_dict_node
    for k, v in zip(dict_src.keys, dict_src.values):
        if not (isinstance(k, ast.Constant) and isinstance(k.value, str)):
            continue
        cname = k.value
        ckind = "int_scalar"
        crank = 0
        if isinstance(v, ast.Name):
            nm = v.id
            if nm in tr0.alloc_reals:
                ckind = "real_array"
                crank = max(1, tr0.alloc_real_rank.get(nm, 1))
            elif nm in tr0.alloc_ints:
                ckind = "int_array"
                crank = 1
            elif nm in tr0.alloc_logs:
                ckind = "logical_array"
                crank = 1
            elif nm in tr0.reals:
                ckind = "real_scalar"
            else:
                ckind = "int_scalar"
        else:
            if (
                isinstance(v, ast.Subscript)
                and isinstance(v.slice, ast.Constant)
                and isinstance(v.slice.value, str)
                and isinstance(known_key_kinds, dict)
                and v.slice.value in known_key_kinds
            ):
                kc = known_key_kinds[v.slice.value]
                if isinstance(kc, tuple):
                    ckind, crank = kc
                else:
                    ckind, crank = kc, 1
                comps.append((cname, ckind, v, max(0, int(crank))))
                continue
            if (
                isinstance(v, ast.Call)
                and isinstance(v.func, ast.Attribute)
                and v.func.attr == "copy"
                and isinstance(v.func.value, ast.Name)
            ):
                nm = v.func.value.id
                if nm in tr0.alloc_reals:
                    ckind = "real_array"
                    crank = max(1, tr0.alloc_real_rank.get(nm, 1))
                elif nm in tr0.alloc_ints:
                    ckind = "int_array"
                    crank = 1
                elif nm in tr0.alloc_logs:
                    ckind = "logical_array"
                    crank = 1
                elif nm in tr0.reals:
                    ckind = "real_scalar"
                else:
                    ckind = "int_scalar"
                comps.append((cname, ckind, v, crank))
                continue
            if (
                isinstance(v, ast.Call)
                and isinstance(v.func, ast.Attribute)
                and isinstance(v.func.value, ast.Name)
                and v.func.value.id == "np"
                and v.func.attr in {"array", "asarray"}
            ):
                dtype_txt = ""
                for kw in v.keywords:
                    if kw.arg == "dtype":
                        if isinstance(kw.value, ast.Name):
                            dtype_txt = kw.value.id.lower()
                        elif (
                            isinstance(kw.value, ast.Attribute)
                            and isinstance(kw.value.value, ast.Name)
                            and kw.value.value.id == "np"
                        ):
                            dtype_txt = kw.value.attr.lower()
                if "float" in dtype_txt:
                    ckind = "real_array"
                    if isinstance(v.args[0], ast.List):
                        crank = 1
                elif "bool" in dtype_txt:
                    ckind = "logical_array"
                    crank = 1
                elif "int" in dtype_txt:
                    ckind = "int_array"
                    crank = 1
                else:
                    # No dtype specified: infer from list append usage when possible.
                    basek = None
                    if len(v.args) >= 1 and isinstance(v.args[0], ast.Name):
                        basek = list_elem_kind.get(v.args[0].id, None)
                    if basek == "logical":
                        ckind = "logical_array"
                    elif basek == "int":
                        ckind = "int_array"
                    else:
                        ckind = "real_array"
                    crank = max(1, tr0._rank_expr(v.args[0]) if len(v.args) >= 1 else 1)
                comps.append((cname, ckind, v, crank))
                continue
            ek = tr0._expr_kind(v)
            vrank = tr0._rank_expr(v)
            if vrank > 0:
                if ek == "logical":
                    ckind = "logical_array"
                elif ek == "int":
                    ckind = "int_array"
                else:
                    ckind = "real_array"
                crank = max(1, vrank)
            else:
                ckind = "real_scalar" if ek == "real" else ("int_scalar" if ek == "int" else "int_scalar")
        if ckind == "real_array" and crank <= 0:
            crank = max(1, tr0._rank_expr(v))
        if ckind in {"int_array", "logical_array"} and crank <= 0:
            crank = 1
        comps.append((cname, ckind, v, crank))
    if not comps:
        return None
    return {
        "dict_name": ret_name,
        "type_name": f"{fn.name}_dict_t",
        "components": comps,
        "direct_return": ret_dict_node is not None,
    }


def _emit_local_function(
    o,
    fn,
    params,
    no_comment=False,
    known_pure_calls=None,
    comment_map=None,
    dict_return_spec=None,
    dict_return_types=None,
    local_return_specs=None,
    tuple_return_out_kinds=None,
    dict_type_components=None,
    dict_arg_types=None,
    local_func_arg_ranks=None,
):
    # Local-function lowering for guarded-main scripts (integer/real scalar args).
    args = [a.arg for a in fn.args.args]
    if not args:
        raise NotImplementedError("local functions must have at least one argument")
    ret_name = f"{fn.name}_result"

    if fn.name == "logsumexp" and len(args) >= 3:
        o.w(f"function {fn.name}(" + ", ".join(args[:3]) + f") result({ret_name})")
        o.push()
        emit_python_docstring_as_fortran_comments(o, fn)
        o.w("real(kind=dp), intent(in) :: a(:,:)")
        o.w("integer, intent(in) :: axis")
        o.w("logical, intent(in) :: keepdims")
        o.w(f"real(kind=dp), allocatable :: {ret_name}(:,:)")
        o.w("integer :: i, j")
        o.w("real(kind=dp) :: m, s")
        o.w("if (axis == 1) then")
        o.push()
        o.w(f"allocate({ret_name}(1:size(a,1),1:1))")
        o.w("do i = 1, size(a,1)")
        o.push()
        o.w("m = maxval(a(i,:))")
        o.w("s = 0.0_dp")
        o.w("do j = 1, size(a,2)")
        o.push()
        o.w("s = s + exp(a(i,j) - m)")
        o.pop()
        o.w("end do")
        o.w(f"{ret_name}(i,1) = m + log(s)")
        o.pop()
        o.w("end do")
        o.pop()
        o.w("else if (axis == 0) then")
        o.push()
        o.w(f"allocate({ret_name}(1:1,1:size(a,2)))")
        o.w("do j = 1, size(a,2)")
        o.push()
        o.w("m = maxval(a(:,j))")
        o.w("s = 0.0_dp")
        o.w("do i = 1, size(a,1)")
        o.push()
        o.w("s = s + exp(a(i,j) - m)")
        o.pop()
        o.w("end do")
        o.w(f"{ret_name}(1,j) = m + log(s)")
        o.pop()
        o.w("end do")
        o.pop()
        o.w("else")
        o.push()
        o.w("stop \"logsumexp: unsupported axis\"")
        o.pop()
        o.w("end if")
        o.w("if (.not. keepdims) then")
        o.push()
        o.w("! keep 2D result shape in this subset")
        o.pop()
        o.w("end if")
        o.pop()
        o.w(f"end function {fn.name}")
        o.w("")
        return
    if fn.name == "log_normal_pdf_1d" and len(args) >= 3:
        xnm, munm, varnm = args[:3]
        o.w(f"function {fn.name}({xnm}, {munm}, {varnm}) result({ret_name})")
        o.push()
        emit_python_docstring_as_fortran_comments(o, fn)
        o.w(f"real(kind=dp), intent(in) :: {xnm}(:)")
        o.w(f"real(kind=dp), intent(in) :: {munm}(:)")
        o.w(f"real(kind=dp), intent(in) :: {varnm}(:)")
        o.w(f"real(kind=dp), allocatable :: {ret_name}(:,:)")
        o.w("integer :: i, j")
        o.w("real(kind=dp) :: log2pi")
        o.w("log2pi = log(2.0_dp * acos(-1.0_dp))")
        o.w(f"allocate({ret_name}(1:size({xnm}),1:size({munm})))")
        o.w(f"do i = 1, size({xnm})")
        o.push()
        o.w(f"do j = 1, size({munm})")
        o.push()
        o.w(f"{ret_name}(i,j) = -0.5_dp * (log2pi + log({varnm}(j)) + ({xnm}(i) - {munm}(j))**2 / {varnm}(j))")
        o.pop()
        o.w("end do")
        o.pop()
        o.w("end do")
        o.pop()
        o.w(f"end function {fn.name}")
        o.w("")
        return
    if fn.name == "normal_logpdf_1d" and len(args) >= 3:
        xnm, munm, varnm = args[:3]
        o.w(f"function {fn.name}({xnm}, {munm}, {varnm}) result({ret_name})")
        o.push()
        emit_python_docstring_as_fortran_comments(o, fn)
        o.w(f"real(kind=dp), intent(in) :: {xnm}(:)")
        o.w(f"real(kind=dp), intent(in) :: {munm}(:)")
        o.w(f"real(kind=dp), intent(in) :: {varnm}(:)")
        o.w(f"real(kind=dp), allocatable :: {ret_name}(:,:)")
        o.w("integer :: i, j")
        o.w("real(kind=dp) :: log2pi")
        o.w("log2pi = log(2.0_dp * acos(-1.0_dp))")
        o.w(f"allocate({ret_name}(1:size({xnm}),1:size({munm})))")
        o.w(f"do i = 1, size({xnm})")
        o.push()
        o.w(f"do j = 1, size({munm})")
        o.push()
        o.w(f"{ret_name}(i,j) = -0.5_dp * (log2pi + log({varnm}(j)) + ({xnm}(i) - {munm}(j))**2 / {varnm}(j))")
        o.pop()
        o.w("end do")
        o.pop()
        o.w("end do")
        o.pop()
        o.w(f"end function {fn.name}")
        o.w("")
        return

    local_tree = ast.Module(body=list(fn.body), type_ignores=[])
    local_list_counts = build_list_count_map(local_tree)
    tr = translator(
        o,
        params={},
        context="flat",
        list_counts=local_list_counts,
        function_result_name=ret_name,
        comment_map=comment_map,
        dict_return_types=dict_return_types,
        local_return_specs=local_return_specs,
        tuple_return_out_kinds=tuple_return_out_kinds,
        dict_type_components=dict_type_components,
        local_func_arg_ranks=local_func_arg_ranks,
    )
    # Scope comment emission to this procedure body; otherwise comments from
    # earlier procedures leak into the first emitted statement.
    tr._last_comment_line = getattr(fn, "lineno", 0)
    for anm, tnm in (dict_arg_types or {}).items():
        tr.dict_typed_vars[anm] = tnm
        tr.dict_var_components[anm] = list((dict_type_components or {}).get(tnm, {}).keys())
        tr.ints.discard(anm)
        tr.reals.discard(anm)
        tr.alloc_ints.discard(anm)
        tr.alloc_reals.discard(anm)
        tr.alloc_logs.discard(anm)
    def _pre_arg_rank(nm):
        rr = 0
        for st in fn.body:
            for n in ast.walk(st):
                if isinstance(n, ast.Subscript) and isinstance(n.value, ast.Name) and n.value.id == nm:
                    if isinstance(n.slice, ast.Tuple):
                        has_none_axis = any(is_none(e) for e in n.slice.elts)
                        rr = max(rr, 1 if has_none_axis else len(n.slice.elts))
                    else:
                        rr = max(rr, 1)
                if (
                    isinstance(n, ast.Call)
                    and isinstance(n.func, ast.Attribute)
                    and isinstance(n.func.value, ast.Name)
                    and n.func.value.id == "np"
                    and n.func.attr in {"max", "sum", "log", "exp", "sqrt", "maximum", "asarray"}
                    and len(n.args) >= 1
                    and isinstance(n.args[0], ast.Name)
                    and n.args[0].id == nm
                ):
                    axis_present = any(kw.arg == "axis" for kw in n.keywords)
                    rr = max(rr, 2 if axis_present else 1)
        return rr

    dict_arg_names = set((dict_arg_types or {}).keys())
    for a in fn.args.args:
        if a.arg in dict_arg_names:
            continue
        rr = _pre_arg_rank(a.arg)
        if local_func_arg_ranks is not None and fn.name in local_func_arg_ranks:
            idx = next((i for i, aa in enumerate(fn.args.args) if aa.arg == a.arg), -1)
            if idx >= 0 and idx < len(local_func_arg_ranks[fn.name]):
                rr = max(rr, int(local_func_arg_ranks[fn.name][idx]))
        if rr > 0:
            tr._mark_alloc_real(a.arg, rank=rr)

    tr.prescan(fn.body)

    returns = [s for s in fn.body if isinstance(s, ast.Return) and s.value is not None]
    tuple_return = False
    out_names = []
    dict_return = dict_return_spec is not None
    if dict_return:
        dname = dict_return_spec["dict_name"]
        if dname != "__direct_return_dict__":
            tr.dict_aliases[dname] = {cname: f"{dname}%{cname}" for cname, _, _, _ in dict_return_spec["components"]}
            tr.dict_var_components[dname] = [cname for cname, _, _, _ in dict_return_spec["components"]]
            tr.dict_typed_vars[dname] = dict_return_spec["type_name"]
        comp_names = set(cname for cname, _, _, _ in dict_return_spec["components"])
        for st in ast.walk(fn):
            if (
                isinstance(st, ast.Assign)
                and len(st.targets) == 1
                and isinstance(st.targets[0], ast.Name)
                and isinstance(st.value, ast.Dict)
            ):
                nm = st.targets[0].id
                keys = []
                ok = True
                for kk in st.value.keys:
                    if isinstance(kk, ast.Constant) and isinstance(kk.value, str):
                        keys.append(kk.value)
                    else:
                        ok = False
                        break
                if ok and set(keys).issubset(comp_names):
                    tr.dict_typed_vars[nm] = dict_return_spec["type_name"]
                    tr.dict_var_components[nm] = list(comp_names)
                    tr.ints.discard(nm)
                    tr.reals.discard(nm)
                    tr.alloc_ints.discard(nm)
                    tr.alloc_reals.discard(nm)
                    tr.alloc_logs.discard(nm)
    if returns and isinstance(returns[0].value, ast.Tuple):
        tuple_return = True
        for e in returns[0].value.elts:
            if not isinstance(e, ast.Name):
                raise NotImplementedError("tuple return elements must be names")
            out_names.append(e.id)

    alloc_logs_set = set(tr.alloc_logs)
    alloc_ints_set = set(tr.alloc_ints - {ret_name})
    alloc_reals_set = set(tr.alloc_reals - {ret_name})
    alloc_logs_set -= set(args)
    alloc_ints_set -= set(args)
    alloc_reals_set -= set(args)
    if tuple_return:
        alloc_logs_set -= set(out_names)
        alloc_ints_set -= set(out_names)
        alloc_reals_set -= set(out_names)
    remove_names = set(args) | ({ret_name} if not tuple_return else set(out_names))
    ints = sorted(({*tr.ints, *set(local_list_counts.values())} - remove_names) - alloc_logs_set - alloc_ints_set - alloc_reals_set)
    reals = sorted((tr.reals - remove_names) - alloc_logs_set - alloc_ints_set - alloc_reals_set)
    logs = sorted((tr.logs - remove_names) - alloc_logs_set - alloc_ints_set - alloc_reals_set)
    alloc_logs = sorted(alloc_logs_set)
    alloc_ints = sorted(alloc_ints_set)

    pure_prefix = "pure " if function_is_pure(fn, known_pure_calls=known_pure_calls) else ""
    if tuple_return:
        sig = args + out_names
        o.w(f"{pure_prefix}subroutine {fn.name}(" + ", ".join(sig) + ")")
    else:
        o.w(f"{pure_prefix}function {fn.name}(" + ", ".join(args) + f") result({ret_name})")
    o.push()
    emit_python_docstring_as_fortran_comments(o, fn)
    if not no_comment:
        o.w(f"! {procedure_comment(fn.name, 'subroutine' if tuple_return else 'function')}")
    ann_map = {}
    for a in fn.args.args:
        if a.annotation is not None and hasattr(ast, "unparse"):
            ann_map[a.arg] = ast.unparse(a.annotation).lower()
    defaults_map = {}
    if fn.args.defaults:
        arg_names = [a.arg for a in fn.args.args]
        tail_names = arg_names[len(arg_names) - len(fn.args.defaults):]
        for nm, dv in zip(tail_names, fn.args.defaults):
            defaults_map[nm] = dv
    def _name_used(node, nm):
        def rec(x):
            if isinstance(x, ast.Name) and x.id == nm:
                return True
            for fld, val in ast.iter_fields(x):
                if isinstance(x, ast.Call) and fld == "keywords":
                    continue
                if isinstance(val, ast.AST):
                    if rec(val):
                        return True
                elif isinstance(val, list):
                    for it in val:
                        if isinstance(it, ast.AST) and rec(it):
                            return True
            return False
        return rec(node)

    def _arg_real_context(nm):
        for st in fn.body:
            for n in ast.walk(st):
                if isinstance(n, ast.BinOp):
                    if _name_used(n.left, nm) or _name_used(n.right, nm):
                        if _name_used(n.left, nm) and any(isinstance(t, ast.Constant) and isinstance(t.value, float) for t in ast.walk(n.right)):
                            return True
                        if _name_used(n.right, nm) and any(isinstance(t, ast.Constant) and isinstance(t.value, float) for t in ast.walk(n.left)):
                            return True
                if (
                    isinstance(n, ast.Call)
                    and isinstance(n.func, ast.Attribute)
                    and isinstance(n.func.value, ast.Name)
                    and n.func.value.id == "np"
                    and n.func.attr in {"log", "exp", "sqrt", "maximum", "max", "sum"}
                    and len(n.args) >= 1
                    and _name_used(n.args[0], nm)
                ):
                    return True
        return False

    def _arg_array_rank(nm):
        rr = 0
        for st in fn.body:
            for n in ast.walk(st):
                if isinstance(n, ast.Subscript) and isinstance(n.value, ast.Name) and n.value.id == nm:
                    if isinstance(n.slice, ast.Tuple):
                        has_none_axis = any(is_none(e) for e in n.slice.elts)
                        rr = max(rr, 1 if has_none_axis else len(n.slice.elts))
                    else:
                        rr = max(rr, 1)
                if (
                    isinstance(n, ast.Call)
                    and isinstance(n.func, ast.Attribute)
                    and isinstance(n.func.value, ast.Name)
                    and n.func.value.id == "np"
                    and n.func.attr in {"max", "sum", "log", "exp", "sqrt", "maximum", "asarray"}
                    and len(n.args) >= 1
                    and isinstance(n.args[0], ast.Name)
                    and n.args[0].id == nm
                ):
                    # axis reduction calls imply at least rank-2 source in this subset.
                    axis_present = any(kw.arg == "axis" for kw in n.keywords)
                    rr = max(rr, 2 if axis_present else 1)
        return rr

    def _arg_is_assigned(nm):
        for st in fn.body:
            for n in ast.walk(st):
                if isinstance(n, ast.Assign):
                    for tg in n.targets:
                        if isinstance(tg, ast.Name) and tg.id == nm:
                            return True
                if isinstance(n, ast.AugAssign):
                    if isinstance(n.target, ast.Name) and n.target.id == nm:
                        return True
        return False

    # Keep internal rank metadata for dummy array arguments consistent with
    # the declared argument rank used below.
    for _arg in args:
        _rr = _arg_array_rank(_arg)
        if _arg in tr.alloc_reals and _rr > 0:
            tr.alloc_real_rank[_arg] = _rr

    for arg in args:
        intent_txt = "inout" if _arg_is_assigned(arg) else "in"
        ann = ann_map.get(arg, "")
        ann_is_int = ("int" in ann) and ("float" not in ann) and ("bool" not in ann)
        ann_is_float = "float" in ann
        ann_is_bool = "bool" in ann or "logical" in ann
        arr_rank = _arg_array_rank(arg)
        if arg in (dict_arg_types or {}):
            tnm = (dict_arg_types or {})[arg]
            arg_decl = f"type({tnm}), intent({intent_txt}) :: {arg}"
        elif arg in tr.alloc_ints:
            if arr_rank > 0:
                dims = ",".join(":" for _ in range(arr_rank))
                arg_decl = f"integer, intent({intent_txt}) :: {arg}({dims})"
            else:
                arg_decl = f"integer, intent({intent_txt}) :: {arg}(:)"
        elif arg in tr.alloc_logs:
            if arr_rank > 0:
                dims = ",".join(":" for _ in range(arr_rank))
                arg_decl = f"logical, intent({intent_txt}) :: {arg}({dims})"
            else:
                arg_decl = f"logical, intent({intent_txt}) :: {arg}(:)"
        elif arr_rank > 0:
            dflt = defaults_map.get(arg, None)
            if ann_is_bool or (isinstance(dflt, ast.Constant) and isinstance(dflt.value, bool)):
                arg_kind = "logical"
            elif ann_is_int or (isinstance(dflt, ast.Constant) and isinstance(dflt.value, int)):
                arg_kind = "integer"
            elif (
                (ann_is_float or arg in tr.reals or arg in tr.alloc_reals)
                or (isinstance(dflt, ast.Constant) and isinstance(dflt.value, float))
                or _arg_real_context(arg)
            ):
                arg_kind = "real(kind=dp)"
            else:
                arg_kind = "integer"
            dims = ",".join(":" for _ in range(arr_rank))
            arg_decl = f"{arg_kind}, intent({intent_txt}) :: {arg}({dims})"
        elif arg in tr.alloc_reals:
            rr = max(1, tr.alloc_real_rank.get(arg, 1))
            dims = ",".join(":" for _ in range(rr))
            arg_decl = f"real(kind=dp), intent({intent_txt}) :: {arg}({dims})"
        else:
            dflt = defaults_map.get(arg, None)
            if ann_is_bool or (isinstance(dflt, ast.Constant) and isinstance(dflt.value, bool)):
                arg_kind = "logical"
            elif ann_is_int or (isinstance(dflt, ast.Constant) and isinstance(dflt.value, int)):
                arg_kind = "integer"
            elif (
                (ann_is_float or arg in tr.reals)
                or (isinstance(dflt, ast.Constant) and isinstance(dflt.value, float))
                or _arg_real_context(arg)
            ):
                arg_kind = "real(kind=dp)"
            else:
                arg_kind = "integer"
            if arr_rank > 0:
                dims = ",".join(":" for _ in range(arr_rank))
                arg_decl = f"{arg_kind}, intent({intent_txt}) :: {arg}({dims})"
            else:
                arg_decl = f"{arg_kind}, intent({intent_txt}) :: {arg}"
        o.w(arg_decl + (f" ! {argument_comment(arg, 'in')}" if not no_comment else ""))
    if tuple_return:
        for nm in out_names:
            if nm in tr.alloc_reals:
                rr = max(1, tr.alloc_real_rank.get(nm, 1))
                dims = ",".join(":" for _ in range(rr))
                o.w(f"real(kind=dp), allocatable, intent(out) :: {nm}({dims})")
            elif nm in tr.alloc_ints:
                rr = max(1, tr.alloc_int_rank.get(nm, 1))
                dims = ",".join(":" for _ in range(rr))
                o.w(f"integer, allocatable, intent(out) :: {nm}({dims})")
            elif nm in tr.alloc_logs:
                rr = max(1, tr.alloc_log_rank.get(nm, 1))
                dims = ",".join(":" for _ in range(rr))
                o.w(f"logical, allocatable, intent(out) :: {nm}({dims})")
            elif nm in tr.reals:
                o.w(f"real(kind=dp), intent(out) :: {nm}")
            else:
                o.w(f"integer, intent(out) :: {nm}")
    else:
        ret_is_real = False
        ret_name_src = None
        ret_decl_full = False
        if returns and isinstance(returns[0].value, ast.Name):
            ret_name_src = returns[0].value.id
        if dict_return:
            ret_decl = f"type({dict_return_spec['type_name']})"
        elif ret_name_src is not None and ret_name_src in tr.alloc_reals:
            rr = max(1, tr.alloc_real_rank.get(ret_name_src, 1))
            dims = ",".join(":" for _ in range(rr))
            ret_decl = f"real(kind=dp), allocatable :: {ret_name}({dims})"
            ret_decl_full = True
        elif ret_name_src is not None and ret_name_src in tr.alloc_ints:
            ret_decl = f"integer, allocatable :: {ret_name}(:)"
            ret_decl_full = True
        elif ret_name_src is not None and ret_name_src in tr.alloc_logs:
            ret_decl = f"logical, allocatable :: {ret_name}(:)"
            ret_decl_full = True
        else:
            if fn.returns is not None and hasattr(ast, "unparse"):
                rtxt = ast.unparse(fn.returns).lower()
                if "float" in rtxt:
                    ret_is_real = True
            if (not ret_is_real) and returns:
                rk0 = tr._expr_kind(returns[0].value)
                if rk0 == "real":
                    ret_is_real = True
            if ret_name in tr.reals or (ret_name_src is not None and ret_name_src in tr.reals):
                ret_is_real = True
            ret_decl = "real(kind=dp)" if ret_is_real else "integer"
        if ret_decl_full:
            o.w(ret_decl)
        else:
            o.w(f"{ret_decl} :: {ret_name}")
    if reals:
        o.w("real(kind=dp) :: " + ", ".join(reals))
    if ints:
        o.w("integer :: " + ", ".join(ints))
    if logs:
        o.w("logical :: " + ", ".join(logs))
    arg_set = set(args)
    local_dict_vars = sorted((nm, tn) for nm, tn in tr.dict_typed_vars.items() if nm != ret_name and nm not in arg_set)
    for nm, tn in local_dict_vars:
        o.w(f"type({tn}) :: {nm}")
    for nm in alloc_logs:
        rr = max(1, tr.alloc_log_rank.get(nm, 1))
        dims = ",".join(":" for _ in range(rr))
        o.w(f"logical, allocatable :: {nm}({dims})")
    for nm in alloc_ints:
        rr = max(1, tr.alloc_int_rank.get(nm, 1))
        dims = ",".join(":" for _ in range(rr))
        o.w(f"integer, allocatable :: {nm}({dims})")
    for nm in sorted(alloc_reals_set):
        rr = max(1, tr.alloc_real_rank.get(nm, 1))
        dims = ",".join(":" for _ in range(rr))
        o.w(f"real(kind=dp), allocatable :: {nm}({dims})")
    o.w("")
    for i, s in enumerate(fn.body):
        if (
            dict_return
            and isinstance(s, ast.Assign)
            and len(s.targets) == 1
            and isinstance(s.targets[0], ast.Name)
            and s.targets[0].id == dict_return_spec["dict_name"]
            and isinstance(s.value, ast.Dict)
        ):
            dnm = s.targets[0].id
            for cname, ckind, v, _ in dict_return_spec["components"]:
                rhs = tr.expr(v)
                lhs = f"{dnm}%{cname}"
                o.w(f"{lhs} = {rhs}")
            continue
        if isinstance(s, ast.Return):
            # Avoid emitting a redundant RETURN when this is the final statement.
            if s.value is not None:
                if tuple_return:
                    if not isinstance(s.value, ast.Tuple) or len(s.value.elts) != len(out_names):
                        raise NotImplementedError("inconsistent tuple return shape")
                    for j, e in enumerate(s.value.elts):
                        o.w(f"{out_names[j]} = {tr.expr(e)}")
                else:
                    if dict_return and isinstance(s.value, ast.Dict):
                        for cname, ckind, vv, _ in dict_return_spec["components"]:
                            lhs = f"{ret_name}%{cname}"
                            rhs = tr.expr(vv)
                            o.w(f"{lhs} = {rhs}")
                    elif dict_return and isinstance(s.value, ast.Name) and s.value.id == dict_return_spec["dict_name"]:
                        dnm = dict_return_spec["dict_name"]
                        for cname, ckind, _, _ in dict_return_spec["components"]:
                            lhs = f"{ret_name}%{cname}"
                            rhs = f"{dnm}%{cname}"
                            o.w(f"{lhs} = {rhs}")
                    else:
                        if tr.function_result_name is None:
                            raise NotImplementedError("return value only supported in function context")
                        o.w(f"{tr.function_result_name} = {tr.expr(s.value)}")
            if i != len(fn.body) - 1:
                o.w("return")
            continue
        tr.visit(s)
    o.pop()
    if tuple_return:
        o.w(f"end subroutine {fn.name}")
    else:
        o.w(f"end function {fn.name}")
    o.w("")


def _local_return_maps(local_funcs, params, arg_rank_hints=None, arg_kind_hints=None):
    tuple_out = {}
    scalar_or_array = {}
    arg_rank_hints = arg_rank_hints or {}
    arg_kind_hints = arg_kind_hints or {}
    for fn in (local_funcs or []):
        if fn.name in {"log_normal_pdf_1d", "normal_logpdf_1d"}:
            scalar_or_array[fn.name] = "alloc_real"
            continue
        tr = translator(emit(), params={}, context="flat", list_counts={}, function_result_name=f"{fn.name}_result")
        # Seed argument rank/kind from call-site observations when available.
        rr_h = arg_rank_hints.get(fn.name, [])
        rk_h = arg_kind_hints.get(fn.name, [])
        for i, a in enumerate(fn.args.args):
            rr = rr_h[i] if i < len(rr_h) else 0
            rk = rk_h[i] if i < len(rk_h) else None
            if rr > 0:
                if rk == "int":
                    tr._mark_alloc_int(a.arg, rank=rr)
                elif rk == "logical":
                    tr._mark_alloc_log(a.arg, rank=rr)
                else:
                    tr._mark_alloc_real(a.arg, rank=rr)
            else:
                if rk == "int":
                    tr._mark_int(a.arg)
                elif rk == "real":
                    tr._mark_real(a.arg)
        tr.prescan(fn.body)
        rets = [s for s in fn.body if isinstance(s, ast.Return) and s.value is not None]
        if not rets:
            continue
        r0 = rets[0].value
        if isinstance(r0, ast.Tuple):
            kinds = []
            for e in r0.elts:
                if isinstance(e, ast.Name):
                    nm = e.id
                    if nm in tr.alloc_reals:
                        kinds.append("alloc_real")
                    elif nm in tr.alloc_ints:
                        kinds.append("alloc_int")
                    elif nm in tr.alloc_logs:
                        kinds.append("alloc_log")
                    elif nm in tr.reals:
                        kinds.append("real")
                    else:
                        kinds.append("int")
                else:
                    kinds.append("int")
            tuple_out[fn.name] = kinds
            continue
        if isinstance(r0, ast.Name):
            nm = r0.id
            if nm in tr.alloc_reals:
                scalar_or_array[fn.name] = "alloc_real"
            elif nm in tr.alloc_ints:
                scalar_or_array[fn.name] = "alloc_int"
            elif nm in tr.alloc_logs:
                scalar_or_array[fn.name] = "alloc_log"
            elif nm in tr.reals:
                scalar_or_array[fn.name] = "real"
            else:
                scalar_or_array[fn.name] = "int"
        else:
            k = tr._expr_kind(r0)
            if k == "real":
                scalar_or_array[fn.name] = "real"
            elif k == "int":
                scalar_or_array[fn.name] = "int"
    return scalar_or_array, tuple_out


def generate_flat(
    tree, stem, helper_uses, params, needed_helpers, list_counts, local_funcs=None, no_comment=False, known_pure_calls=None, comment_map=None
):
    top_level_comment_map = _comment_map_for_top_level(tree, comment_map)
    def _infer_arg_rank_in_fn(fn, nm):
        rr = 0
        for st in fn.body:
            for n in ast.walk(st):
                if isinstance(n, ast.Subscript) and isinstance(n.value, ast.Name) and n.value.id == nm:
                    if isinstance(n.slice, ast.Tuple):
                        rr = max(rr, len(n.slice.elts))
                    else:
                        rr = max(rr, 1)
                if (
                    isinstance(n, ast.Call)
                    and isinstance(n.func, ast.Attribute)
                    and isinstance(n.func.value, ast.Name)
                    and n.func.value.id == "np"
                    and n.func.attr in {"max", "sum", "log", "exp", "sqrt", "maximum", "asarray"}
                    and len(n.args) >= 1
                    and isinstance(n.args[0], ast.Name)
                    and n.args[0].id == nm
                ):
                    axis_present = any(kw.arg == "axis" for kw in n.keywords)
                    rr = max(rr, 2 if axis_present else 1)
        return rr

    # Gather call-site rank/kind hints for local function arguments.
    call_rank_hints = {fn.name: [0 for _ in fn.args.args] for fn in (local_funcs or [])}
    call_kind_hints = {fn.name: [None for _ in fn.args.args] for fn in (local_funcs or [])}
    if local_funcs:
        tr_seed = translator(emit(), params=params, context="flat", list_counts=list_counts)
        tr_seed.prescan(tree.body)
        for st in tree.body:
            for n in ast.walk(st):
                if not (isinstance(n, ast.Call) and isinstance(n.func, ast.Name) and n.func.id in call_rank_hints):
                    continue
                rr = call_rank_hints[n.func.id]
                rk = call_kind_hints[n.func.id]
                for i, a in enumerate(n.args):
                    if i >= len(rr):
                        break
                    rr[i] = max(rr[i], tr_seed._rank_expr(a))
                    ak = tr_seed._expr_kind(a)
                    if rk[i] is None and ak is not None:
                        rk[i] = ak

    local_return_specs, tuple_return_out_kinds = _local_return_maps(
        local_funcs,
        params,
        arg_rank_hints=call_rank_hints,
        arg_kind_hints=call_kind_hints,
    )
    local_func_arg_ranks = {}
    for fn in (local_funcs or []):
        base_ranks = [_infer_arg_rank_in_fn(fn, a.arg) for a in fn.args.args]
        hint_ranks = call_rank_hints.get(fn.name, [])
        local_func_arg_ranks[fn.name] = [
            max(base_ranks[i], (hint_ranks[i] if i < len(hint_ranks) else 0))
            for i in range(len(base_ranks))
        ]
    dict_return_specs = {}
    dict_return_types = {}
    dict_type_components = {}
    known_key_kinds = {}
    for fn in (local_funcs or []):
        spec = _analyze_dict_return_spec(fn, params, known_key_kinds=known_key_kinds)
        if spec is not None:
            dict_return_specs[fn.name] = spec
            dict_return_types[fn.name] = spec["type_name"]
            dict_type_components[spec["type_name"]] = {
                cname: (ckind, crank) for cname, ckind, _, crank in spec["components"]
            }
            for cname, ckind, _, crank in spec["components"]:
                if cname in known_key_kinds:
                    # preserve first-seen mapping to keep behavior stable
                    continue
                known_key_kinds[cname] = (ckind, crank)

    # Canonicalize equivalent dict return types across local functions.
    sig_to_type = {}
    for fn_name, spec in list(dict_return_specs.items()):
        sig = tuple((cname, ckind, int(crank)) for cname, ckind, _, crank in spec["components"])
        if sig in sig_to_type:
            canon = sig_to_type[sig]
            spec["type_name"] = canon
            dict_return_types[fn_name] = canon
        else:
            sig_to_type[sig] = spec["type_name"]
    for fn_name, spec in dict_return_specs.items():
        tnm = spec["type_name"]
        if tnm not in dict_type_components:
            dict_type_components[tnm] = {cname: (ckind, crank) for cname, ckind, _, crank in spec["components"]}

    tuple_return_funcs = set()
    for fn in (local_funcs or []):
        for st in fn.body:
            if isinstance(st, ast.Return) and isinstance(st.value, ast.Tuple):
                tuple_return_funcs.add(fn.name)
                break

    o = emit()
    o.w(f"program {stem}")
    o.push()
    for mod, syms in helper_uses.items():
        if syms:
            o.w(f"use {mod}, only: " + ", ".join(sorted(syms)))
    o.w("use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_is_finite, ieee_is_nan")
    o.w("use, intrinsic :: iso_fortran_env, only: real64")
    o.w("implicit none")
    o.w("integer, parameter :: dp = real64")
    emitted_types = set()
    for fn_name in sorted(dict_return_specs):
        spec = dict_return_specs[fn_name]
        tname = spec["type_name"]
        if tname in emitted_types:
            continue
        emitted_types.add(tname)
        o.w(f"type :: {tname}")
        o.push()
        for cname, ckind, _, crank in spec["components"]:
            if ckind == "real_array":
                dims = ",".join(":" for _ in range(max(1, crank)))
                o.w(f"real(kind=dp), allocatable :: {cname}({dims})")
            elif ckind == "int_array":
                dims = ",".join(":" for _ in range(max(1, crank)))
                o.w(f"integer, allocatable :: {cname}({dims})")
            elif ckind == "logical_array":
                dims = ",".join(":" for _ in range(max(1, crank)))
                o.w(f"logical, allocatable :: {cname}({dims})")
            elif ckind == "real_scalar":
                o.w(f"real(kind=dp) :: {cname}")
            else:
                o.w(f"integer :: {cname}")
        o.pop()
        o.w(f"end type {tname}")

    for name, val in sorted(params.items()):
        o.w(f"integer, parameter :: {name} = {val} ! {const_comment(name, tree)}")

    tr = translator(
        o,
        params=params,
        context="flat",
        list_counts=list_counts,
        comment_map=top_level_comment_map,
        tuple_return_funcs=tuple_return_funcs,
        dict_return_types=dict_return_types,
        local_return_specs=local_return_specs,
        tuple_return_out_kinds=tuple_return_out_kinds,
        dict_type_components=dict_type_components,
        local_func_arg_ranks=local_func_arg_ranks,
    )
    tr.prescan(tree.body)
    for st in tree.body:
        if isinstance(st, ast.FunctionDef) and st.name == "main":
            tr.prescan(st.body)
    # Use local function return annotations to infer scalar result types in main body.
    ret_real_funcs = set()
    for fn in (local_funcs or []):
        if fn.returns is not None and hasattr(ast, "unparse"):
            if "float" in ast.unparse(fn.returns).lower():
                ret_real_funcs.add(fn.name)
    if ret_real_funcs:
        for st in tree.body:
            if not isinstance(st, ast.Assign) or len(st.targets) != 1:
                continue
            if not isinstance(st.targets[0], ast.Name):
                continue
            v = st.value
            if isinstance(v, ast.Call) and isinstance(v.func, ast.Name) and v.func.id in ret_real_funcs:
                tr._mark_real(st.targets[0].id)

    alloc_logs_set = set(tr.alloc_logs)
    alloc_ints_set = set(tr.alloc_ints)
    alloc_reals_set = set(tr.alloc_reals)
    ints = sorted(
        ({*tr.ints, *set(list_counts.values())} - set(params.keys())) - alloc_logs_set - alloc_ints_set - alloc_reals_set
    )
    reals = sorted((tr.reals - set(params.keys())) - alloc_logs_set - alloc_ints_set - alloc_reals_set)
    logs = sorted((tr.logs - set(params.keys())) - alloc_logs_set - alloc_ints_set - alloc_reals_set)
    dict_type_vars = sorted(tr.dict_typed_vars.items())
    if ints:
        o.w("integer :: " + ", ".join(ints))
    if reals:
        o.w("real(kind=dp) :: " + ", ".join(reals))
    if logs:
        o.w("logical :: " + ", ".join(logs))
    for vname, tname in dict_type_vars:
        o.w(f"type({tname}) :: {vname}")
    for name in sorted(alloc_logs_set):
        rr = max(1, tr.alloc_log_rank.get(name, 1))
        dims = ",".join(":" for _ in range(rr))
        o.w(f"logical, allocatable :: {name}({dims})")
    for name in sorted(alloc_ints_set):
        rr = max(1, tr.alloc_int_rank.get(name, 1))
        dims = ",".join(":" for _ in range(rr))
        o.w(f"integer, allocatable :: {name}({dims})")
    for name in sorted(alloc_reals_set):
        rr = max(1, tr.alloc_real_rank.get(name, 1))
        dims = ",".join(":" for _ in range(rr))
        o.w(f"real(kind=dp), allocatable :: {name}({dims})")

    o.w("")
    for stmt in tree.body:
        if isinstance(stmt, (ast.ImportFrom, ast.Import, ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
            continue
        if is_main_guard_if(stmt):
            continue
        tr.visit(stmt)

    if local_funcs:
        o.w("")
        o.w("contains")
        o.w("")
        for fn in local_funcs:
            arg_dict_types = {}
            for a in fn.args.args:
                anm = a.arg
                keys = set()
                for st in ast.walk(fn):
                    if (
                        isinstance(st, ast.Subscript)
                        and isinstance(st.value, ast.Name)
                        and st.value.id == anm
                        and isinstance(st.slice, ast.Constant)
                        and isinstance(st.slice.value, str)
                    ):
                        keys.add(st.slice.value)
                if not keys:
                    continue
                matches = []
                for tnm, comps in dict_type_components.items():
                    if keys.issubset(set(comps.keys())):
                        matches.append(tnm)
                if len(matches) >= 1:
                    arg_dict_types[anm] = sorted(matches)[0]
            _emit_local_function(
                o,
                fn,
                params,
                no_comment=no_comment,
                known_pure_calls=known_pure_calls,
                comment_map=comment_map,
                dict_return_spec=dict_return_specs.get(fn.name),
                dict_return_types=dict_return_types,
                local_return_specs=local_return_specs,
                tuple_return_out_kinds=tuple_return_out_kinds,
                dict_type_components=dict_type_components,
                dict_arg_types=arg_dict_types,
                local_func_arg_ranks=local_func_arg_ranks,
            )

    o.pop()
    o.w(f"end program {stem}")
    return o.text()


def generate_structured(tree, stem, helper_uses, params, needed_helpers, list_counts, no_comment=False, comment_map=None):
    top_level_comment_map = _comment_map_for_top_level(tree, comment_map)
    o = emit()
    run_name = f"run_{stem}"
    compute_name = f"compute_{stem}"

    settings = sorted(params.keys())
    scalar_outs = detect_scalar_outputs(tree, params=params)
    node_if = top_level_if(tree)
    if node_if is None:
        raise NotImplementedError("structured mode expects a top-level if (primes family pattern)")
    pre_nodes = []
    post_nodes = []
    seen_if = False
    for s in tree.body:
        if s is node_if:
            seen_if = True
            break
        if isinstance(s, (ast.ImportFrom, ast.Import, ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
            continue
        if is_main_guard_if(s):
            continue
        pre_nodes.append(s)
    if seen_if:
        take = False
        for s in tree.body:
            if s is node_if:
                take = True
                continue
            if not take:
                continue
            if isinstance(s, (ast.ImportFrom, ast.Import, ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
                continue
            if is_main_guard_if(s):
                continue
            post_nodes.append(s)

    def _is_scalar_prelude_assign(stmt):
        if not isinstance(stmt, ast.Assign) or len(stmt.targets) != 1:
            return False
        t = stmt.targets[0]
        if not isinstance(t, ast.Name):
            return False
        if t.id in params:
            return False
        # Skip list initializations in structured mode; list outputs are managed separately.
        if isinstance(stmt.value, ast.List):
            return False
        return True

    pre_scalar_nodes = [s for s in pre_nodes if _is_scalar_prelude_assign(s)]
    pre_scalar_names = sorted({s.targets[0].id for s in pre_scalar_nodes})

    # module
    o.w("module main_mod")
    o.push()
    for mod, syms in helper_uses.items():
        if syms:
            o.w(f"use {mod}, only: " + ", ".join(sorted(syms)))
    o.w("implicit none")
    o.pop()
    o.w("")
    o.w("contains")
    o.w("")

    # run (i/o allowed)
    run_args = ", ".join(settings)
    o.w(f"subroutine {run_name}({run_args})" if run_args else f"subroutine {run_name}()")
    o.push()
    if not no_comment:
        o.w(f"! {procedure_comment(run_name, 'subroutine')}")
    for name in settings:
        o.w(
            f"integer, intent(in) :: {name}"
            + (
                f" ! {argument_comment(name, 'in')}; {const_comment(name, tree)}"
                if not no_comment
                else ""
            )
        )

    # scalars printed in else-case
    for nm in scalar_outs:
        o.w(f"integer :: {nm}")
    for nm in pre_scalar_names:
        if nm not in scalar_outs:
            o.w(f"integer :: {nm}")

    # list outputs (if any)
    for lst, cnt in sorted(list_counts.items()):
        o.w(f"integer :: {cnt}")
        o.w(f"integer, allocatable :: {lst}(:)")

    o.w("")
    tr_print = translator(o, params=params, context="run_print", list_counts=list_counts, comment_map=top_level_comment_map)

    # initialize list outputs for all branches
    for lst, cnt in sorted(list_counts.items()):
        if "n" in settings:
            o.w(f"allocate({lst}(1:n))")
        else:
            o.w(f"allocate({lst}(1:1))")
        o.w(f"{cnt} = 0")
        o.w(f"{lst} = 0")

    # prelude scalar setup needed by top-level IF test (for example m = n)
    for s in pre_scalar_nodes:
        tr_print.visit(s)

    # if branch prints only
    o.w(f"if ({tr_print.expr(node_if.test)}) then")
    o.push()
    for s in node_if.body:
        if isinstance(s, ast.Expr) and isinstance(s.value, ast.Call) and isinstance(s.value.func, ast.Name) and s.value.func.id == "print":
            tr_print.visit(s)
    o.pop()

    o.w("else")
    o.push()

    # call compute
    call_args = []
    for name in settings:
        call_args.append(name)
    for lst, cnt in sorted(list_counts.items()):
        call_args.append(lst)
        call_args.append(cnt)
    for nm in scalar_outs:
        call_args.append(nm)
    o.w(f"call {compute_name}(" + ", ".join(call_args) + ")")

    # else-branch prints
    for s in node_if.orelse:
        if isinstance(s, ast.Expr) and isinstance(s.value, ast.Call) and isinstance(s.value.func, ast.Name) and s.value.func.id == "print":
            tr_print.visit(s)

    o.pop()
    o.w("end if")
    for s in post_nodes:
        if isinstance(s, ast.Expr) and isinstance(s.value, ast.Call) and isinstance(s.value.func, ast.Name) and s.value.func.id == "print":
            tr_print.visit(s)
    o.pop()
    o.w(f"end subroutine {run_name}")
    o.w("")

    # compute (no i/o)
    comp_args = []
    for name in settings:
        comp_args.append(name)
    for lst, cnt in sorted(list_counts.items()):
        comp_args.extend([lst, cnt])
    for nm in scalar_outs:
        comp_args.append(nm)

    o.w(f"pure subroutine {compute_name}(" + ", ".join(comp_args) + ")")
    o.push()
    if not no_comment:
        o.w(f"! {procedure_comment(compute_name, 'subroutine')}")
    for name in settings:
        o.w(
            f"integer, intent(in) :: {name}"
            + (
                f" ! {argument_comment(name, 'in')}; {const_comment(name, tree)}"
                if not no_comment
                else ""
            )
        )

    for lst, cnt in sorted(list_counts.items()):
        o.w(
            f"integer, intent(out) :: {lst}(:)"
            + (f" ! {argument_comment(lst, 'out')}; output array (first {cnt} entries used)" if not no_comment else "")
        )
        o.w(
            f"integer, intent(out) :: {cnt}"
            + (f" ! {argument_comment(cnt, 'out')}; number of output elements in {lst}" if not no_comment else "")
        )

    for nm in scalar_outs:
        o.w(
            f"integer, intent(out) :: {nm}"
            + (f" ! {argument_comment(nm, 'out')}; computed value" if not no_comment else "")
        )

    # compute nodes: else branch without prints
    compute_nodes = list(pre_scalar_nodes)
    for s in node_if.orelse:
        if isinstance(s, ast.Expr) and isinstance(s.value, ast.Call) and isinstance(s.value.func, ast.Name) and s.value.func.id == "print":
            continue
        compute_nodes.append(s)
    for s in post_nodes:
        if isinstance(s, ast.Expr) and isinstance(s.value, ast.Call) and isinstance(s.value.func, ast.Name) and s.value.func.id == "print":
            continue
        compute_nodes.append(s)

    tr_comp = translator(o, params=params, context="compute", list_counts=list_counts, comment_map=top_level_comment_map)
    tr_comp.prescan(compute_nodes)

    exclude = set(settings) | set(list_counts.values()) | set(list_counts.keys()) | set(scalar_outs)
    locals_ints = sorted(tr_comp.ints - exclude)
    if locals_ints:
        o.w("integer :: " + ", ".join(locals_ints))
    for nm in sorted(tr_comp.alloc_logs):
        rr = max(1, tr_comp.alloc_log_rank.get(nm, 1))
        dims = ",".join(":" for _ in range(rr))
        o.w(f"logical, allocatable :: {nm}({dims})")

    o.w("")
    for stmt in compute_nodes:
        if isinstance(stmt, (ast.ImportFrom, ast.Import, ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
            continue
        if is_main_guard_if(stmt):
            continue
        tr_comp.visit(stmt)

    o.pop()
    o.w(f"end subroutine {compute_name}")
    o.w("")
    o.w("end module main_mod")
    o.w("")

    # program
    o.w(f"program {stem}")
    o.push()
    o.w(f"use main_mod, only: {run_name}")
    o.w("implicit none")
    for name, val in sorted(params.items()):
        o.w(f"integer, parameter :: {name} = {val} ! {const_comment(name, tree)}")
    o.w("")
    if settings:
        o.w(f"call {run_name}(" + ", ".join(settings) + ")")
    else:
        o.w(f"call {run_name}()")
    o.pop()
    o.w(f"end program {stem}")

    return o.text()


def resolve_helper_uses(helper_paths, needed_helpers):
    """Map needed helper symbols to helper modules discovered from helper files."""
    if not helper_paths:
        # Default wiring: emit imports from python_mod for all detected helpers.
        # This keeps generated Fortran explicit about runtime dependencies.
        uses = {}
        if needed_helpers:
            uses["python_mod"] = sorted(set(needed_helpers))
        return uses, [], set()

    providers = {}  # sym -> module
    pure_helpers = set()
    for hp in helper_paths:
        p = Path(hp)
        if not p.exists():
            raise FileNotFoundError(f"helper file not found: {p}")
        text = p.read_text(encoding="utf-8", errors="ignore")
        module_name, exports, has_proc = discover_runtime_exports(text)
        pure_helpers |= discover_pure_procedures(text)
        # If no explicit PUBLIC list is present, assume defined procedures are usable.
        available = set(has_proc) if not exports else (set(exports) | set(has_proc))
        for sym in sorted(available):
            providers.setdefault(sym.lower(), module_name)

    uses = {}
    missing = []
    for sym in sorted(needed_helpers):
        mod = providers.get(sym.lower())
        if mod is None:
            missing.append(sym)
            continue
        uses.setdefault(mod, []).append(sym)
    for mod in list(uses.keys()):
        uses[mod] = sorted(set(uses[mod]))
    return uses, missing, pure_helpers


def _modules_defined_in_source(src_text):
    mods = set()
    for ln in src_text.splitlines():
        m = re.match(r"^\s*module\s+([a-z_]\w*)\b", ln, flags=re.IGNORECASE)
        if m and not re.match(r"^\s*module\s+procedure\b", ln, flags=re.IGNORECASE):
            mods.add(m.group(1).lower())
    return mods


def _modules_used_in_source(src_text):
    mods = set()
    for ln in src_text.splitlines():
        # skip intrinsic use statements: use, intrinsic :: iso_fortran_env, only: ...
        if re.match(r"^\s*use\s*,\s*intrinsic\s*::", ln, flags=re.IGNORECASE):
            continue
        m = re.match(r"^\s*use\s+([a-z_]\w*)\b", ln, flags=re.IGNORECASE)
        if m:
            mods.add(m.group(1).lower())
    return mods


def _module_name_from_helper_file(path):
    p = Path(path)
    if not p.exists():
        return None
    text = p.read_text(encoding="utf-8", errors="ignore")
    mod, _exports, _has_proc = discover_runtime_exports(text)
    return (mod or "").lower() if mod else None


def resolve_helper_files_for_build(transpiled_path, explicit_helpers):
    """
    Return (all_helpers, missing_modules) for build.
    If generated source uses foo_mod and no explicit helper provides it,
    auto-add foo.f90 when present; otherwise report missing module.
    """
    out_path = Path(transpiled_path)
    src = out_path.read_text(encoding="utf-8", errors="ignore")
    defined = _modules_defined_in_source(src)
    used = _modules_used_in_source(src)
    needed = sorted(used - defined)

    helper_files = [str(Path(h)) for h in explicit_helpers]
    provided = set()
    for hp in helper_files:
        mod = _module_name_from_helper_file(hp)
        if mod:
            provided.add(mod)

    missing_modules = []
    auto_added = []
    for mod in needed:
        if mod in provided:
            continue
        if mod.endswith("_mod"):
            base = mod[: -len("_mod")]
            cand = Path(f"{base}.f90")
            if cand.exists():
                cand_s = str(cand)
                if cand_s not in helper_files:
                    helper_files.append(cand_s)
                    auto_added.append(cand_s)
                provided.add(mod)
            else:
                missing_modules.append((mod, str(cand)))
        else:
            missing_modules.append((mod, ""))

    # LAPACK linkage support for numpy.linalg wrappers in python_mod.
    if re.search(r"\blinalg_(solve|det|inv|eig|svd)\s*\(", src, flags=re.IGNORECASE):
        lapack_src = Path("lapack_d.f90")
        lapack_s = str(lapack_src)
        if lapack_src.exists():
            if lapack_s not in helper_files:
                helper_files.append(lapack_s)
                auto_added.append(lapack_s)
        else:
            missing_modules.append(("lapack_d_external", lapack_s))
    return helper_files, auto_added, missing_modules


def transpile_file(py_path, helper_paths, flat, no_comment=False, out_path=None):
    src = Path(py_path).read_text(encoding="utf-8")
    tree = ast.parse(src)
    comment_map = extract_python_comments(src)

    exec_nodes = [
        s
        for s in tree.body
        if not isinstance(s, (ast.ImportFrom, ast.Import, ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef))
        and not is_main_guard_if(s)
    ]
    used_main_unwrap = False
    effective_tree = tree
    local_funcs = [s for s in tree.body if isinstance(s, ast.FunctionDef)]

    # If there is no direct top-level executable code but there is a canonical
    # main guard that calls main(), transpile the main() body as program body.
    if not exec_nodes:
        main_guard = None
        for s in tree.body:
            if is_main_guard_if(s):
                main_guard = s
                break
        if main_guard is not None:
            called_main = False
            if len(main_guard.body) == 1:
                b0 = main_guard.body[0]
                if (
                    isinstance(b0, ast.Expr)
                    and isinstance(b0.value, ast.Call)
                    and isinstance(b0.value.func, ast.Name)
                    and b0.value.func.id == "main"
                    and len(b0.value.args) == 0
                ):
                    called_main = True
            if called_main:
                main_def = None
                for s in tree.body:
                    if isinstance(s, ast.FunctionDef) and s.name == "main":
                        main_def = s
                        break
                if main_def is not None:
                    local_funcs = [s for s in tree.body if isinstance(s, ast.FunctionDef) and s.name != "main"]
                    effective_tree = ast.Module(body=list(main_def.body), type_ignores=[])
                    used_main_unwrap = True
                    exec_nodes = [
                        s
                        for s in effective_tree.body
                        if not isinstance(
                            s, (ast.ImportFrom, ast.Import, ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)
                        )
                        and not is_main_guard_if(s)
                    ]
            else:
                # Support direct executable statements inside a guarded __main__ block.
                local_funcs = [s for s in tree.body if isinstance(s, ast.FunctionDef)]
                effective_tree = ast.Module(body=list(main_guard.body), type_ignores=[])
                used_main_unwrap = True
                exec_nodes = [
                    s
                    for s in effective_tree.body
                    if not isinstance(
                        s, (ast.ImportFrom, ast.Import, ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)
                    )
                    and not is_main_guard_if(s)
                ]

    if not exec_nodes:
        raise NotImplementedError(
            "no transpilable executable statements found; "
            "supported guarded-main form is: if __name__ == '__main__': main()"
        )

    params = find_parameters(effective_tree)
    helper_scan_tree = effective_tree
    if local_funcs:
        helper_scan_tree = ast.Module(body=list(effective_tree.body) + list(local_funcs), type_ignores=[])
    needed = detect_needed_helpers(helper_scan_tree)
    list_counts = build_list_count_map(effective_tree)
    if list_counts:
        needed.add("print_int_list")
    helper_uses, missing_helpers, pure_helpers = resolve_helper_uses(helper_paths, needed)
    if missing_helpers:
        print("warning: missing helper symbols:", ", ".join(sorted(missing_helpers)))

    stem = Path(py_path).stem
    if flat:
        f90 = generate_flat(
            effective_tree,
            stem,
            helper_uses,
            params,
            needed,
            list_counts,
            local_funcs=local_funcs,
            no_comment=no_comment,
            known_pure_calls=pure_helpers,
            comment_map=comment_map,
        )
        used_flat_fallback = False
    else:
        used_flat_fallback = False
        try:
            f90 = generate_structured(
                effective_tree, stem, helper_uses, params, needed, list_counts, no_comment=no_comment, comment_map=comment_map
            )
        except NotImplementedError:
            # Structured mode currently targets a specific top-level pattern.
            # Fallback to flat translation for general inputs.
            f90 = generate_flat(
                effective_tree,
                stem,
                helper_uses,
                params,
                needed,
                list_counts,
                local_funcs=local_funcs,
                no_comment=no_comment,
                known_pure_calls=pure_helpers,
                comment_map=comment_map,
            )
            used_flat_fallback = True

    f90 = remove_redundant_tail_returns(f90)
    f90 = simplify_size_dim_for_rank1_arrays(f90)
    f90 = rename_conflicting_identifiers(f90)
    # General Fortran cleanup: fold simple integer arithmetic and remove
    # conservative redundant parentheses in generated statements.
    f90_lines = f90.splitlines()
    f90_lines = remove_redundant_first_guarded_deallocate(f90_lines)
    f90_lines = collapse_alloc_dealloc_before_assignment(f90_lines)
    f90_lines = collapse_allocate_before_array_constructor_assignment(f90_lines)
    f90_lines = simplify_integer_arithmetic_in_lines(f90_lines)
    f90_lines = simplify_generated_parentheses(f90_lines)
    f90_lines = simplify_redundant_parens_in_lines(f90_lines)
    f90_lines = remove_empty_if_blocks(f90_lines)
    f90_lines = inline_shape_comments(f90_lines)
    # First coalesce adjacent declarations without wrapping, then apply
    # the dedicated 80-column wrapper that packs continuation lines.
    f90_lines = coalesce_simple_declarations(f90_lines, max_len=10**9)
    f90_lines = wrap_long_declaration_lines(f90_lines, max_len=80)
    f90 = "\n".join(f90_lines) + ("\n" if f90.endswith("\n") else "")
    out_path = Path(out_path) if out_path else Path(py_path).with_name(f"{Path(py_path).stem}_p.f90")
    stamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    f90 = f"! transpiled by xp2f.py from {Path(py_path).name} on {stamp}\n" + f90
    out_path.write_text(f90, encoding="utf-8")
    return out_path, used_flat_fallback, used_main_unwrap


def main():
    ap = argparse.ArgumentParser(description="partial python -> fortran transpiler")
    ap.add_argument("input_py", help="input python source")
    ap.add_argument("helpers", nargs="*", help="zero or more helper .f90 module files")
    ap.add_argument("--out", help="output .f90 path (default: input basename with _p.f90)")
    ap.add_argument("--flat", action="store_true", help="emit flat main-program translation")
    ap.add_argument("--compile", action="store_true", help="compile transpiled source with helper files")
    ap.add_argument("--run", action="store_true", help="compile and run transpiled source with helper files")
    ap.add_argument("--run-both", action="store_true", help="run original Python and transpiled Fortran (no timing)")
    ap.add_argument("--run-diff", action="store_true", help="run Python and Fortran and compare outputs")
    ap.add_argument("--tee", action="store_true", help="stream output while running transpiled Fortran")
    ap.add_argument("--tee-both", action="store_true", help="stream output while running both Python and Fortran")
    ap.add_argument("--time", action="store_true", help="time transpile/compile/run stages (implies --run)")
    ap.add_argument("--time-both", action="store_true", help="time both original Python run and transpiled Fortran run (implies --run)")
    ap.add_argument("--comment", action="store_true", help="emit generated procedure/argument comments")
    ap.add_argument(
        "--compiler",
        default="gfortran -O3 -march=native -flto",
        help='compiler command, e.g. "gfortran -O2 -Wall"',
    )
    args = ap.parse_args()
    if args.tee_both:
        args.tee = True
    if args.time_both:
        args.run_diff = True
    if args.run_diff:
        args.run_both = True
    if args.run_both:
        args.run = True
    if args.time_both:
        args.time = True
    if args.time:
        args.run = True

    timings = {}
    t0_total = time.perf_counter()
    py_run = None

    if args.time_both or args.run_both:
        py_cmd = [sys.executable, args.input_py]
        t0_py = time.perf_counter() if args.time_both else None
        py_rc, py_out, py_err, py_live = run_capture(py_cmd, tee=args.tee_both)
        if args.time_both:
            timings["python_run"] = time.perf_counter() - t0_py
        print("Run (python):", " ".join(py_cmd))
        if py_rc != 0:
            print(f"Run (python): FAIL (exit {py_rc})")
            if (not py_live) and py_out.strip():
                print(py_out.rstrip())
            if (not py_live) and py_err.strip():
                print(py_err.rstrip())
            return py_rc
        print("Run (python): PASS")
        if (not py_live) and py_out.strip():
            print(py_out.rstrip())
        if (not py_live) and py_err.strip():
            print(py_err.rstrip())
        py_run = subprocess.CompletedProcess(py_cmd, py_rc, py_out, py_err)

    t0_transpile = time.perf_counter()
    try:
        out, used_flat_fallback, used_main_unwrap = transpile_file(
            args.input_py, args.helpers, args.flat, no_comment=(not args.comment), out_path=args.out
        )
    except (NotImplementedError, FileNotFoundError) as e:
        print(f"Transpile: FAIL ({e})")
        return 1
    timings["transpile"] = time.perf_counter() - t0_transpile
    print(f"wrote {out}")
    if used_flat_fallback and not args.flat:
        print("note: structured mode not applicable; used flat mode fallback")
    if used_main_unwrap:
        print("note: unwrapped guarded main() body for transpilation")

    if args.compile or args.run:
        compiler_parts = shlex.split(args.compiler)
        if args.time:
            if len(compiler_parts) > 1:
                print("Compile options:", " ".join(compiler_parts[1:]))
            else:
                print("Compile options: <none>")
        helper_files, auto_added, missing_modules = resolve_helper_files_for_build(out, args.helpers)
        if auto_added:
            print("Auto helper files:", " ".join(auto_added))
        if missing_modules:
            print("Build: FAIL (missing helper module source)")
            for mod, cand in missing_modules:
                if cand:
                    print(f"  module '{mod}' required; expected helper file '{cand}'")
                else:
                    print(f"  module '{mod}' required; no helper-file naming rule available")
            return 1
        cmd = compiler_parts + [*helper_files, str(out)]
        if args.run:
            exe = out.with_suffix(".exe")
            cmd = cmd + ["-o", str(exe)]
        print("Build:", " ".join(cmd))
        t0_build = time.perf_counter()
        cp = subprocess.run(cmd, capture_output=True, text=True)
        timings["compile"] = time.perf_counter() - t0_build
        if cp.returncode != 0:
            print(f"Build: FAIL (exit {cp.returncode})")
            if cp.stdout.strip():
                print(cp.stdout.rstrip())
            if cp.stderr.strip():
                print(cp.stderr.rstrip())
            return cp.returncode
        print("Build: PASS")
        if args.run:
            t0_run = time.perf_counter()
            rp_rc, rp_out, rp_err, rp_live = run_capture([str(exe)], tee=args.tee)
            timings["fortran_run"] = time.perf_counter() - t0_run
            if rp_rc != 0:
                print(f"Run: FAIL (exit {rp_rc})")
                if (not rp_live) and rp_out.strip():
                    print(rp_out.rstrip())
                if (not rp_live) and rp_err.strip():
                    print(rp_err.rstrip())
                # Automatic fallback: rebuild with debug checks/backtrace and rerun
                dbg_flags = debug_flags_for_compiler(compiler_parts[0] if compiler_parts else "")
                dbg_cmd = compiler_parts + dbg_flags + [*helper_files, str(out), "-o", str(exe)]
                print("Debug rebuild:", " ".join(dbg_cmd))
                dbg_cp = subprocess.run(dbg_cmd, capture_output=True, text=True)
                if dbg_cp.returncode != 0:
                    print(f"Debug rebuild: FAIL (exit {dbg_cp.returncode})")
                    if dbg_cp.stdout.strip():
                        print(dbg_cp.stdout.rstrip())
                    if dbg_cp.stderr.strip():
                        print(dbg_cp.stderr.rstrip())
                else:
                    print("Debug rebuild: PASS")
                    dbg_rc, dbg_out, dbg_err, dbg_live = run_capture([str(exe)], tee=args.tee)
                    if dbg_rc != 0:
                        print(f"Debug run: FAIL (exit {dbg_rc})")
                    else:
                        print("Debug run: PASS")
                    if (not dbg_live) and dbg_out.strip():
                        print(dbg_out.rstrip())
                    if (not dbg_live) and dbg_err.strip():
                        print(dbg_err.rstrip())
                return rp_rc
            print("Run: PASS")
            if (not rp_live) and rp_out.strip():
                print(rp_out.rstrip())
            if (not rp_live) and rp_err.strip():
                print(rp_err.rstrip())
            rp = subprocess.CompletedProcess([str(exe)], rp_rc, rp_out, rp_err)
            if args.run_diff and py_run is not None:
                def _norm(s):
                    lines = s.replace("\r\n", "\n").replace("\r", "\n").split("\n")
                    lines = [" ".join(ln.split()) for ln in lines]
                    while lines and lines[-1] == "":
                        lines.pop()
                    return lines

                py_lines = _norm((py_run.stdout or "") + (("\n" + py_run.stderr) if py_run.stderr else ""))
                ft_lines = _norm((rp.stdout or "") + (("\n" + rp.stderr) if rp.stderr else ""))
                if py_lines == ft_lines:
                    print("Run diff: MATCH")
                else:
                    print("Run diff: DIFF")
                    first = None
                    nmin = min(len(py_lines), len(ft_lines))
                    for i in range(nmin):
                        if py_lines[i] != ft_lines[i]:
                            first = i
                            break
                    if first is None:
                        first = nmin
                    print(f"  first mismatch line: {first + 1}")
                    if first < len(py_lines):
                        print(f"  python : {py_lines[first]}")
                    else:
                        print("  python : <no line>")
                    if first < len(ft_lines):
                        print(f"  fortran: {ft_lines[first]}")
                    else:
                        print("  fortran: <no line>")
                    for dl in difflib.unified_diff(py_lines, ft_lines, fromfile="python", tofile="fortran", n=1):
                        print(dl)
                        # keep diff compact
                        if dl.startswith("@@"):
                            break
    if args.time:
        timings["total"] = (
            timings.get("transpile", 0.0)
            + timings.get("compile", 0.0)
            + timings.get("fortran_run", 0.0)
        )
        print("")
        print("Timing summary (seconds):")
        base = timings.get("python_run", 0.0)

        def _ratio(v):
            if base > 0.0:
                return f"{(v / base):.6f}"
            return "n/a"

        rows = []
        if "python_run" in timings:
            rows.append(("python run", timings["python_run"]))
        rows.append(("transpile", timings.get("transpile", 0.0)))
        if "compile" in timings:
            rows.append(("compile", timings["compile"]))
        if "fortran_run" in timings:
            rows.append(("fortran run", timings["fortran_run"]))
        rows.append(("total", timings["total"]))

        stage_w = max(len("stage"), max(len(name) for name, _ in rows))
        sec_vals = [f"{val:.6f}" for _name, val in rows]
        sec_w = max(len("seconds"), max(len(s) for s in sec_vals))
        print(f"  {'stage':<{stage_w}}  {'seconds':>{sec_w}}    ratio(vs python run)")
        for name, val in rows:
            print(f"  {name:<{stage_w}}  {val:>{sec_w}.6f}    {_ratio(val)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
