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


def fstr(s):
    # use double quotes when possible; fallback to single quotes; escape embedded quotes by doubling
    if '"' not in s:
        return '"' + s + '"'
    if "'" not in s:
        return "'" + s + "'"
    return '"' + s.replace('"', '""') + '"'


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
    return isinstance(node, ast.Constant) and node.value is None


def find_module_level_int_literals(tree):
    d = {}
    for node in tree.body:
        if (
            isinstance(node, ast.Assign)
            and len(node.targets) == 1
            and isinstance(node.targets[0], ast.Name)
            and is_const_int(node.value)
        ):
            d[node.targets[0].id] = node.value.value
    return d


def find_parameters(tree):
    # module-level int literal assigned once => integer, parameter
    counts = count_assignments(tree)
    d = find_module_level_int_literals(tree)
    params = {}
    for k, v in d.items():
        if counts.get(k, 0) == 1:
            params[k] = v
    return params


def const_comment(name, tree):
    # tiny heuristic; extend as you like
    if name == "n":
        src = ast.unparse(tree) if hasattr(ast, "unparse") else ""
        if "is_prime" in src or "primes" in src:
            return "upper bound for primes"
    return "constant from python source"


def top_level_if(tree):
    for node in tree.body:
        if isinstance(node, ast.If):
            return node
    return None


def detect_needed_helpers(tree):
    needed = set()

    class scan(ast.NodeVisitor):
        def visit_Call(self, node):
            if isinstance(node.func, ast.Name) and node.func.id == "isqrt":
                needed.add("isqrt_int")
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

    if module_name is None:
        module_name = "python_mod"

    return module_name, exports, has_proc


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

    return {
        "isqrt_int": (isqrt_pub, isqrt_blk),
        "print_int_list": (pil_pub, pil_blk),
    }


def ensure_runtime_helpers(runtime_path, needed_helpers):
    helper_templates = runtime_helper_templates()

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
    def __init__(self, out, params, context, list_counts):
        # context: "flat" | "compute" | "run_print"
        self.o = out
        self.params = params
        self.context = context
        self.list_counts = list_counts  # map list var -> count var
        self.ints = set()
        self.alloc_logs = set()
        self.alloc_ints = set()

    def expr(self, node):
        if isinstance(node, ast.Name):
            return node.id

        if isinstance(node, ast.Constant):
            v = node.value
            if isinstance(v, bool):
                return ".true." if v else ".false."
            if isinstance(v, int):
                return str(v)
            if isinstance(v, float):
                return repr(v)
            if isinstance(v, str):
                return fstr(v)
            if v is None:
                return "-1"
            raise NotImplementedError("unsupported constant")

        if isinstance(node, ast.BinOp):
            op = type(node.op)
            opmap = {ast.Add: "+", ast.Sub: "-", ast.Mult: "*", ast.Div: "/", ast.FloorDiv: "/", ast.Mod: "mod", ast.Pow: "**"}
            if op not in opmap:
                raise NotImplementedError("unsupported binop")
            a = self.expr(node.left)
            b = self.expr(node.right)
            if op is ast.Mod:
                return f"mod({a}, {b})"
            return f"({a} {opmap[op]} {b})"

        if isinstance(node, ast.UnaryOp):
            if isinstance(node.op, ast.USub):
                return f"(-{self.expr(node.operand)})"
            if isinstance(node.op, ast.UAdd):
                return f"(+{self.expr(node.operand)})"
            raise NotImplementedError("unsupported unary op")

        if isinstance(node, ast.Compare):
            if len(node.ops) != 1 or len(node.comparators) != 1:
                raise NotImplementedError("chained compares not supported")
            op = type(node.ops[0])
            opmap = {ast.Lt: "<", ast.LtE: "<=", ast.Gt: ">", ast.GtE: ">=", ast.Eq: "==", ast.NotEq: "/="}
            if op not in opmap:
                raise NotImplementedError("unsupported compare op")
            return f"({self.expr(node.left)} {opmap[op]} {self.expr(node.comparators[0])})"

        if isinstance(node, ast.Subscript):
            if not isinstance(node.value, ast.Name):
                raise NotImplementedError("only name[index] supported")
            name = node.value.id
            if isinstance(node.slice, ast.Slice):
                raise NotImplementedError("slices not supported")
            idx = self.expr(node.slice)
            return f"{name}({idx})"

        if isinstance(node, ast.Call):
            if isinstance(node.func, ast.Name) and node.func.id == "isqrt":
                return f"isqrt_int({self.expr(node.args[0])})"
            raise NotImplementedError("unsupported call")

        raise NotImplementedError(f"unsupported expr: {type(node).__name__}")

    def prescan(self, nodes):
        for node in nodes:
            if isinstance(node, ast.Assign):
                if len(node.targets) != 1:
                    continue
                t = node.targets[0]
                v = node.value

                if isinstance(t, ast.Name) and is_const_int(v):
                    if t.id not in self.params:
                        self.ints.add(t.id)

                if isinstance(t, ast.Name) and is_none(v):
                    if t.id not in self.params:
                        self.ints.add(t.id)  # represent None as integer sentinel

                # name = other_name  (needed for e.g. largest = i)
                if isinstance(t, ast.Name) and isinstance(v, ast.Name):
                    if t.id not in self.params:
                        self.ints.add(t.id)

                if isinstance(t, ast.Name) and isinstance(v, ast.List) and len(v.elts) == 0:
                    if self.context == "flat":
                        self.alloc_ints.add(t.id)

                # logical sieve init: [True] * (n+1)
                if isinstance(t, ast.Name) and isinstance(v, ast.BinOp) and isinstance(v.op, ast.Mult):
                    if isinstance(v.left, ast.List) and len(v.left.elts) == 1 and is_bool_const(v.left.elts[0]):
                        self.alloc_logs.add(t.id)

            if isinstance(node, ast.AugAssign):
                for nm in extract_target_names(node.target):
                    if nm not in self.params:
                        self.ints.add(nm)

            if isinstance(node, ast.For) and isinstance(node.target, ast.Name):
                if node.target.id not in self.params:
                    self.ints.add(node.target.id)
                self.prescan(node.body)

            if isinstance(node, ast.If):
                self.prescan(node.body)
                self.prescan(node.orelse)

            if isinstance(node, ast.Expr) and isinstance(node.value, ast.Call):
                c = node.value
                if isinstance(c.func, ast.Attribute) and c.func.attr == "append":
                    if isinstance(c.func.value, ast.Name):
                        if self.context == "flat":
                            self.alloc_ints.add(c.func.value.id)

    def visit_Assign(self, node):
        if len(node.targets) != 1:
            raise NotImplementedError("multiple assignment not supported")
        t = node.targets[0]
        v = node.value

        # ignore module-level param assignment already emitted as parameter
        if isinstance(t, ast.Name) and t.id in self.params and is_const_int(v):
            return

        # simple int literal assignment
        if isinstance(t, ast.Name) and is_const_int(v):
            self.o.w(f"{t.id} = {v.value}")
            return

        # None sentinel assignment
        if isinstance(t, ast.Name) and is_none(v):
            self.o.w(f"{t.id} = -1")
            return

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
            self.o.w(f"{self.expr(t)} = {self.expr(v)}")
            return

        # NEW: fallback for name = <simple expr>  (e.g., largest = i)
        if isinstance(t, ast.Name):
            self.o.w(f"{t.id} = {self.expr(v)}")
            return

        raise NotImplementedError("unsupported assign")

    def visit_AugAssign(self, node):
        # only supports name += expr and name -= expr
        if not isinstance(node.target, ast.Name):
            raise NotImplementedError("augassign target must be name")
        name = node.target.id
        if isinstance(node.op, ast.Add):
            self.o.w(f"{name} = {name} + {self.expr(node.value)}")
            return
        if isinstance(node.op, ast.Sub):
            self.o.w(f"{name} = {name} - {self.expr(node.value)}")
            return
        raise NotImplementedError("unsupported augassign op")

    def visit_If(self, node):
        self.o.w(f"if ({self.expr(node.test)}) then")
        self.o.push()
        for s in node.body:
            self.visit(s)
        self.o.pop()
        if node.orelse:
            self.o.w("else")
            self.o.push()
            for s in node.orelse:
                self.visit(s)
            self.o.pop()
        self.o.w("end if")

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
        if not (isinstance(node.iter, ast.Call) and isinstance(node.iter.func, ast.Name) and node.iter.func.id == "range"):
            raise NotImplementedError("only for .. in range(..) supported")
        if not isinstance(node.target, ast.Name):
            raise NotImplementedError("for target must be a name")

        var = node.target.id
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

    def visit_Expr(self, node):
        if not isinstance(node.value, ast.Call):
            raise NotImplementedError("only call expressions supported")
        c = node.value

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
        if len(call.args) != 1:
            raise NotImplementedError("print with 1 arg only")
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
                    fmt_parts.append("i0")
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

def generate_flat(tree, stem, module_name, params, needed_helpers, list_counts):
    o = emit()
    o.w(f"program {stem}")
    o.push()
    if needed_helpers:
        o.w(f"use {module_name}, only: " + ", ".join(sorted(needed_helpers)))
    o.w("implicit none")

    for name, val in sorted(params.items()):
        o.w(f"integer, parameter :: {name} = {val} ! {const_comment(name, tree)}")

    tr = translator(o, params=params, context="flat", list_counts=list_counts)
    tr.prescan(tree.body)

    ints = sorted({*tr.ints, *set(list_counts.values())} - set(params.keys()))
    if ints:
        o.w("integer :: " + ", ".join(ints))
    for name in sorted(tr.alloc_logs):
        o.w(f"logical, allocatable :: {name}(:)")
    for name in sorted(tr.alloc_ints):
        o.w(f"integer, allocatable :: {name}(:)")

    o.w("")
    for stmt in tree.body:
        if isinstance(stmt, ast.ImportFrom):
            continue
        tr.visit(stmt)

    o.pop()
    o.w(f"end program {stem}")
    return o.text()


def generate_structured(tree, stem, runtime_mod, params, needed_helpers, list_counts):
    o = emit()
    run_name = f"run_{stem}"
    compute_name = f"compute_{stem}"

    settings = sorted(params.keys())
    scalar_outs = detect_scalar_outputs(tree, params=params)
    node_if = top_level_if(tree)
    if node_if is None:
        raise NotImplementedError("structured mode expects a top-level if (primes family pattern)")

    # module
    o.w("module main_mod")
    o.push()
    if needed_helpers:
        o.w(f"use {runtime_mod}, only: " + ", ".join(sorted(needed_helpers)))
    o.w("implicit none")
    o.pop()
    o.w("")
    o.w("contains")
    o.w("")

    # run (i/o allowed)
    run_args = ", ".join(settings)
    o.w(f"subroutine {run_name}({run_args})" if run_args else f"subroutine {run_name}()")
    o.push()
    o.w(f"! driver: translated from {stem}.py (i/o allowed)")
    for name in settings:
        o.w(f"integer, intent(in) :: {name} ! {const_comment(name, tree)}")

    # scalars printed in else-case
    for nm in scalar_outs:
        o.w(f"integer :: {nm}")

    # list outputs (if any)
    for lst, cnt in sorted(list_counts.items()):
        o.w(f"integer :: {cnt}")
        o.w(f"integer, allocatable :: {lst}(:)")

    o.w("")
    tr_print = translator(o, params=params, context="run_print", list_counts=list_counts)

    # if branch prints only
    o.w(f"if ({tr_print.expr(node_if.test)}) then")
    o.push()
    for s in node_if.body:
        if isinstance(s, ast.Expr) and isinstance(s.value, ast.Call) and isinstance(s.value.func, ast.Name) and s.value.func.id == "print":
            tr_print.visit(s)
    o.pop()

    o.w("else")
    o.push()

    # allocate list outputs (if any)
    for lst, cnt in sorted(list_counts.items()):
        if "n" in settings:
            o.w(f"allocate({lst}(1:n))")
        else:
            o.w(f"allocate({lst}(1:1))")
        o.w(f"{cnt} = 0")

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
    o.w(f"! compute: translated from {stem}.py (no i/o)")
    for name in settings:
        o.w(f"integer, intent(in) :: {name} ! {const_comment(name, tree)}")

    for lst, cnt in sorted(list_counts.items()):
        o.w(f"integer, intent(out) :: {lst}(:) ! output array (first {cnt} entries used)")
        o.w(f"integer, intent(out) :: {cnt} ! number of output elements in {lst}")

    for nm in scalar_outs:
        o.w(f"integer, intent(out) :: {nm} ! computed value")

    # compute nodes: else branch without prints
    compute_nodes = []
    for s in node_if.orelse:
        if isinstance(s, ast.Expr) and isinstance(s.value, ast.Call) and isinstance(s.value.func, ast.Name) and s.value.func.id == "print":
            continue
        compute_nodes.append(s)

    tr_comp = translator(o, params=params, context="compute", list_counts=list_counts)
    tr_comp.prescan(compute_nodes)

    exclude = set(settings) | set(list_counts.values()) | set(list_counts.keys()) | set(scalar_outs)
    locals_ints = sorted(tr_comp.ints - exclude)
    if locals_ints:
        o.w("integer :: " + ", ".join(locals_ints))
    for nm in sorted(tr_comp.alloc_logs):
        o.w(f"logical, allocatable :: {nm}(:)")

    o.w("")
    for stmt in compute_nodes:
        if isinstance(stmt, ast.ImportFrom):
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


def transpile_file(py_path, runtime_path, flat):
    src = Path(py_path).read_text(encoding="utf-8")
    tree = ast.parse(src)

    params = find_parameters(tree)
    needed = detect_needed_helpers(tree)
    list_counts = build_list_count_map(tree)

    runtime_mod, updated = ensure_runtime_helpers(Path(runtime_path), needed)

    stem = Path(py_path).stem
    if flat:
        f90 = generate_flat(tree, stem, runtime_mod, params, needed, list_counts)
    else:
        f90 = generate_structured(tree, stem, runtime_mod, params, needed, list_counts)

    out_path = Path(py_path).with_suffix(".f90")
    out_path.write_text(f90, encoding="utf-8")
    return out_path, updated


def main():
    args = sys.argv[1:]
    flat = False
    if "--flat" in args:
        flat = True
        args = [a for a in args if a != "--flat"]

    if len(args) not in (1, 2):
        print("usage: python xp2f.py [--flat] input.py [python.f90]")
        return 2

    py_path = args[0]
    runtime_path = args[1] if len(args) == 2 else "python.f90"

    out, updated = transpile_file(py_path, runtime_path, flat)
    print(f"wrote {out}")
    if updated:
        print(f"updated {runtime_path} (backup: {runtime_path}.bak)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
