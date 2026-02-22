# p2f.py
# partial python -> fortran transpiler for a small subset (enough for primes_up_to_n.py)
#
# behavior:
# - reads runtime module file python.f90 (default ./python.f90)
# - discovers which helper procedures are exported (public :: name)
# - if the translated python needs a helper that is missing, extends python.f90 by:
#     (a) adding a parseable public line:  public :: name  !@pyapi ...
#     (b) adding the procedure implementation before end module
# - writes the translated main program to <input_stem>.f90
#
# usage:
#   python p2f.py input.py
#   python p2f.py input.py path\to\python.f90
#
# then compile:
#   gfortran -std=f2018 python.f90 input.f90

import ast
import sys
from pathlib import Path
import re


# -------------------------
# fortran runtime management
# -------------------------

def discover_runtime_exports(runtime_text):
    # returns:
    #   module_name: str (best guess)
    #   exports: set[str] from "public :: name"
    #   has_proc: set[str] from actual procedure definitions
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

        # function/subroutine definitions (lightweight)
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
    # insert just before CONTAINS, after any existing public lines in that region
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
    # insert before END MODULE
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
    # dict keyed by lowercase helper name -> (public_line, proc_block)

    isqrt_pub = (
        "public :: isqrt_int       !@pyapi kind=function ret=integer "
        "args=x:integer:intent(in) desc=\"integer square root: return floor(sqrt(x)) for x >= 0\""
    )

    isqrt_blk = '''      integer function isqrt_int(x)
         ! integer square root: return floor(sqrt(x)) for x >= 0
         implicit none
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
      end function isqrt_int'''

    pil_pub = (
        "public :: print_int_list  !@pyapi kind=subroutine "
        "args=a:integer(:):intent(in),n:integer:intent(in) "
        "desc=\"print integer list a(1:n) in python-style [..] format\""
    )

    pil_blk = '''      subroutine print_int_list(a, n)
         ! print integer list a(1:n) in python-style [..] format
         implicit none
         integer, intent(in) :: a(:)  ! array containing values to print
         integer, intent(in) :: n     ! number of elements from a to print
         integer :: j
         if (n <= 0) then
            write(*,'(a)') '[]'
            return
         end if
         write(*,'(a)', advance='no') '['
         do j = 1, n
            if (j > 1) write(*,'(a)', advance='no') ', '
            write(*,'(i0)', advance='no') a(j)
         end do
         write(*,'(a)') ']'
      end subroutine print_int_list'''

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
        # require export and definition
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
# transpiler
# -------------------------

def is_const_int(node):
    return isinstance(node, ast.Constant) and isinstance(node.value, int)


def is_const_str(node):
    return isinstance(node, ast.Constant) and isinstance(node.value, str)


def is_bool_const(node):
    return isinstance(node, ast.Constant) and isinstance(node.value, bool)


class emit:
    def __init__(self):
        self.lines = []
        self.ind = 0

    def w(self, s):
        self.lines.append(" " * self.ind + s)

    def push(self):
        self.ind += 3

    def pop(self):
        self.ind = max(0, self.ind - 3)

    def text(self):
        return "\n".join(self.lines) + "\n"


class py2f(ast.NodeVisitor):
    def __init__(self, program_name, module_name, needed_helpers):
        self.o = emit()
        self.program_name = program_name
        self.module_name = module_name
        self.needed_helpers = set(needed_helpers)

        self.ints = set()
        self.alloc_logs = set()
        self.alloc_ints = set()

        self.param_ints = {}
        self._tree = None

    # ---------- expr ----------
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
                return "'" + v.replace("'", "''") + "'"
            raise NotImplementedError("unsupported constant")

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
            }
            if op not in opmap:
                raise NotImplementedError(f"unsupported binop {op.__name__}")

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
            opmap = {
                ast.Lt: "<",
                ast.LtE: "<=",
                ast.Gt: ">",
                ast.GtE: ">=",
                ast.Eq: "==",
                ast.NotEq: "/=",
            }
            if op not in opmap:
                raise NotImplementedError("unsupported compare op")
            return f"({self.expr(node.left)} {opmap[op]} {self.expr(node.comparators[0])})"

        if isinstance(node, ast.Subscript):
            if not isinstance(node.value, ast.Name):
                raise NotImplementedError("only name[index] supported")
            name = node.value.id
            sl = node.slice
            if isinstance(sl, ast.Slice):
                raise NotImplementedError("slices not supported")
            idx = self.expr(sl)
            return f"{name}({idx})"

        if isinstance(node, ast.Call):
            if isinstance(node.func, ast.Name) and node.func.id == "isqrt":
                return f"isqrt_int({self.expr(node.args[0])})"
            raise NotImplementedError("unsupported call")

        raise NotImplementedError("unsupported expr")

    # ---------- helpers ----------
    def simplify_stop_minus_1(self, stop_node):
        if isinstance(stop_node, ast.BinOp) and isinstance(stop_node.op, ast.Add):
            if is_const_int(stop_node.right) and stop_node.right.value == 1:
                return self.expr(stop_node.left)
        return f"({self.expr(stop_node)} - 1)"

    def range_parts(self, call_node):
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

    def prescan(self, tree):
        # keep a handle for heuristic parameter comments
        self._tree = tree

        # top-level constant integer assignments: name = <int>
        top_const = {}
        for stmt in tree.body:
            if isinstance(stmt, ast.Assign) and len(stmt.targets) == 1:
                t = stmt.targets[0]
                if isinstance(t, ast.Name) and is_const_int(stmt.value):
                    top_const[t.id] = stmt.value.value

        # count how many times each name is assigned anywhere (conservative)
        store_counts = {}

        def bump(name):
            store_counts[name] = store_counts.get(name, 0) + 1

        for sub in ast.walk(tree):
            if isinstance(sub, ast.Assign):
                for tgt in sub.targets:
                    if isinstance(tgt, ast.Name):
                        bump(tgt.id)
            elif isinstance(sub, ast.AugAssign):
                if isinstance(sub.target, ast.Name):
                    bump(sub.target.id)
            elif isinstance(sub, ast.AnnAssign):
                if isinstance(sub.target, ast.Name):
                    bump(sub.target.id)
            elif isinstance(sub, ast.For):
                if isinstance(sub.target, ast.Name):
                    bump(sub.target.id)

        # promote to parameters if:
        #   - assigned at top-level to an integer constant
        #   - assigned exactly once in the whole program
        self.param_ints = {name: val for name, val in top_const.items() if store_counts.get(name, 0) == 1}

        # existing lightweight type/alloc inference (tailored to primes example)
        for node in tree.body:
            if isinstance(node, ast.Assign):
                if len(node.targets) != 1:
                    continue
                t = node.targets[0]
                v = node.value

                if isinstance(t, ast.Name) and is_const_int(v):
                    self.ints.add(t.id)

                if isinstance(t, ast.Name) and isinstance(v, ast.List) and len(v.elts) == 0:
                    self.alloc_ints.add(t.id)

                if isinstance(t, ast.Name) and isinstance(v, ast.BinOp) and isinstance(v.op, ast.Mult):
                    if isinstance(v.left, ast.List) and len(v.left.elts) == 1 and is_bool_const(v.left.elts[0]):
                        self.alloc_logs.add(t.id)

            if isinstance(node, ast.If):
                for sub in ast.walk(node):
                    if isinstance(sub, ast.For) and isinstance(sub.target, ast.Name):
                        self.ints.add(sub.target.id)
                    if isinstance(sub, ast.Subscript) and isinstance(sub.value, ast.Name):
                        if sub.value.id == "is_prime":
                            self.alloc_logs.add("is_prime")
                    if isinstance(sub, ast.Expr) and isinstance(sub.value, ast.Call):
                        c = sub.value
                        if isinstance(c.func, ast.Attribute) and c.func.attr == "append":
                            if isinstance(c.func.value, ast.Name):
                                self.alloc_ints.add(c.func.value.id)

    def param_comment(self, name):
        # heuristic, ASCII-only comments for generated parameters
        nm = name.lower()
        if nm == "n":
            # if program appears to be about primes, make the comment specific
            if "prime" in self.program_name.lower():
                return "upper bound for primes"
            if self._tree is not None:
                for sub in ast.walk(self._tree):
                    if isinstance(sub, ast.Name) and sub.id in ("primes", "is_prime"):
                        return "upper bound for primes"
            return "upper bound"
        return "constant from python source"

    # ---------- statements ----------
    def visit_Module(self, node):
        self.prescan(node)

        self.o.w(f"program {self.program_name}")
        self.o.push()

        if self.needed_helpers:
            only = ", ".join(sorted(self.needed_helpers))
            self.o.w(f"use {self.module_name}, only: {only}")

        self.o.w("implicit none")

        if self.param_ints:
            for pname in sorted(self.param_ints):
                pval = self.param_ints[pname]
                cmt = self.param_comment(pname)
                self.o.w(f"integer, parameter :: {pname} = {pval} ! {cmt}")

        ints = sorted((self.ints | {"npr"}) - set(self.param_ints))
        if ints:
            self.o.w("integer :: " + ", ".join(ints))

        if self.alloc_logs:
            for name in sorted(self.alloc_logs):
                self.o.w(f"logical, allocatable :: {name}(:)")
        if self.alloc_ints:
            for name in sorted(self.alloc_ints):
                self.o.w(f"integer, allocatable :: {name}(:)")

        self.o.w("")
        for stmt in node.body:
            self.visit(stmt)

        self.o.pop()
        self.o.w(f"end program {self.program_name}")

    def visit_ImportFrom(self, node):
        return

    def visit_Assign(self, node):
        t = node.targets[0]
        v = node.value

        if isinstance(t, ast.Name) and is_const_int(v):
            if t.id in self.param_ints:
                return
            self.o.w(f"{t.id} = {v.value}")
            return

        if isinstance(t, ast.Name) and isinstance(v, ast.List) and len(v.elts) == 0:
            name = t.id
            self.o.w(f"if (.not. allocated({name})) allocate({name}(1:n))")
            self.o.w("npr = 0")
            return

        if isinstance(t, ast.Name) and isinstance(v, ast.BinOp) and isinstance(v.op, ast.Mult):
            if isinstance(v.left, ast.List) and len(v.left.elts) == 1 and is_bool_const(v.left.elts[0]):
                name = t.id
                self.o.w(f"if (allocated({name})) deallocate({name})")
                self.o.w(f"allocate({name}(0:n))")
                self.o.w(f"{name} = .true.")
                return

        if isinstance(t, ast.Subscript):
            self.o.w(f"{self.expr(t)} = {self.expr(v)}")
            return

        raise NotImplementedError("unsupported assign")

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

    def visit_For(self, node):
        var = node.target.id
        start, stop, step = self.range_parts(node.iter)

        f_start = self.expr(start)
        f_step = self.expr(step)
        f_upper = self.simplify_stop_minus_1(stop)

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
        c = node.value
        if isinstance(c.func, ast.Name) and c.func.id == "print":
            self.emit_print_call(c)
            return

        if isinstance(c.func, ast.Attribute) and c.func.attr == "append":
            arr = c.func.value.id
            val = self.expr(c.args[0])
            self.o.w("npr = npr + 1")
            self.o.w(f"{arr}(npr) = {val}")
            return

        raise NotImplementedError("unsupported expr")

    def emit_print_call(self, call):
        a = call.args[0]

        if isinstance(a, ast.JoinedStr):
            fmt_parts = []
            items = []
            for part in a.values:
                if is_const_str(part):
                    fmt_parts.append("a")
                    items.append("'" + part.value.replace("'", "''") + "'")
                else:
                    fmt_parts.append("i0")
                    items.append(self.expr(part.value))
            fmt = "'" + "(" + ",".join(fmt_parts) + ")" + "'"
            self.o.w(f"write(*,{fmt}) " + ", ".join(items))
            return

        if isinstance(a, ast.List) and len(a.elts) == 0:
            self.o.w("write(*,'(a)') '[]'")
            return

        if isinstance(a, ast.Name) and a.id == "primes":
            self.o.w("call print_int_list(primes, npr)")
            return

        self.o.w(f"print *, {self.expr(a)}")


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


def transpile_file(py_path, runtime_path):
    src = Path(py_path).read_text(encoding="utf-8")
    tree = ast.parse(src)

    needed = detect_needed_helpers(tree)
    module_name, updated = ensure_runtime_helpers(Path(runtime_path), needed)

    prog = Path(py_path).stem
    t = py2f(program_name=prog, module_name=module_name, needed_helpers=needed)
    t.visit(tree)

    out_path = Path(py_path).with_suffix(".f90")
    out_path.write_text(t.o.text(), encoding="utf-8")
    return out_path, updated


def main():
    if len(sys.argv) not in (2, 3):
        print("usage: python p2f.py input.py [python.f90]")
        return 2

    py_path = sys.argv[1]
    runtime_path = sys.argv[2] if len(sys.argv) == 3 else "python.f90"

    out, updated = transpile_file(py_path, runtime_path)
    print(f"wrote {out}")
    if updated:
        print(f"updated {runtime_path} (backup: {runtime_path}.bak)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
