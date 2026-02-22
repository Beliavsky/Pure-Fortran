# minimal python -> fortran transpiler for a small subset (enough for primes_up_to_n.py)

import ast
import sys
from pathlib import Path


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


def is_const_int(node):
    return isinstance(node, ast.Constant) and isinstance(node.value, int)


def is_const_str(node):
    return isinstance(node, ast.Constant) and isinstance(node.value, str)


def is_bool_const(node):
    return isinstance(node, ast.Constant) and isinstance(node.value, bool)


class py2f(ast.NodeVisitor):
    def __init__(self, program_name="xprog"):
        self.o = emit()
        self.program_name = program_name
        self.ints = set()
        self.alloc_logs = set()
        self.alloc_ints = set()
        self.needs_primes_helpers = False

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
            # isqrt(n) -> isqrt_int(n)
            if isinstance(node.func, ast.Name) and node.func.id == "isqrt":
                if len(node.args) != 1:
                    raise NotImplementedError("isqrt with 1 arg only")
                return f"isqrt_int({self.expr(node.args[0])})"

            if isinstance(node.func, ast.Name) and node.func.id == "range":
                raise NotImplementedError("range only supported in for loops")

            raise NotImplementedError(f"unsupported call: {ast.dump(node.func)}")

        if isinstance(node, ast.JoinedStr):
            raise NotImplementedError("f-string only supported inside print(...)")

        raise NotImplementedError(f"unsupported expr: {type(node).__name__}")

    # ---------- helpers ----------
    def simplify_stop_minus_1(self, stop_node):
        # (x + 1) - 1 -> x
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
        # minimal inference tailored to primes example
        for node in tree.body:
            if isinstance(node, ast.Assign):
                if len(node.targets) != 1:
                    continue
                t = node.targets[0]
                v = node.value

                if isinstance(t, ast.Name) and is_const_int(v):
                    self.ints.add(t.id)

                # primes = []
                if isinstance(t, ast.Name) and isinstance(v, ast.List) and len(v.elts) == 0:
                    self.alloc_ints.add(t.id)
                    self.needs_primes_helpers = True

                # is_prime = [True] * (n + 1)
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
                                self.needs_primes_helpers = True

    # ---------- statements ----------
    def visit_Module(self, node):
        self.prescan(node)

        self.o.w(f"program {self.program_name}")
        self.o.push()
        self.o.w("implicit none")

        ints = sorted(self.ints | {"npr"})
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

        self.o.w("")
        self.o.w("contains")
        self.o.push()

        self.emit_isqrt_int()

        if self.needs_primes_helpers:
            self.emit_print_int_list()

        self.o.pop()
        self.o.pop()
        self.o.w(f"end program {self.program_name}")

    def visit_ImportFrom(self, node):
        # ignore "from math import isqrt"
        return

    def visit_Assign(self, node):
        if len(node.targets) != 1:
            raise NotImplementedError("multiple assignment not supported")

        t = node.targets[0]
        v = node.value

        if isinstance(t, ast.Name) and is_const_int(v):
            self.o.w(f"{t.id} = {v.value}")
            return

        # primes = []
        if isinstance(t, ast.Name) and isinstance(v, ast.List) and len(v.elts) == 0:
            name = t.id
            self.o.w(f"if (.not. allocated({name})) allocate({name}(1:n))")
            self.o.w("npr = 0")
            return

        # is_prime = [True] * (n + 1)
        if isinstance(t, ast.Name) and isinstance(v, ast.BinOp) and isinstance(v.op, ast.Mult):
            if isinstance(v.left, ast.List) and len(v.left.elts) == 1 and is_bool_const(v.left.elts[0]):
                name = t.id
                self.o.w(f"if (allocated({name})) deallocate({name})")
                self.o.w(f"allocate({name}(0:n))")
                self.o.w(f"{name} = .true.")
                return

        # subscript assignment
        if isinstance(t, ast.Subscript):
            lhs = self.expr(t)
            rhs = self.expr(v)
            self.o.w(f"{lhs} = {rhs}")
            return

        raise NotImplementedError(f"unsupported assign: {ast.dump(node)}")

    def visit_If(self, node):
        # fortran requires parentheses after if
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
        if not isinstance(node.target, ast.Name):
            raise NotImplementedError("for target must be a name")

        if not (isinstance(node.iter, ast.Call) and isinstance(node.iter.func, ast.Name) and node.iter.func.id == "range"):
            raise NotImplementedError("only for .. in range(..) supported")

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
        if not isinstance(node.value, ast.Call):
            raise NotImplementedError("only call expressions supported")
        c = node.value

        # print(...)
        if isinstance(c.func, ast.Name) and c.func.id == "print":
            self.emit_print_call(c)
            return

        # primes.append(i)
        if isinstance(c.func, ast.Attribute) and c.func.attr == "append":
            if not isinstance(c.func.value, ast.Name):
                raise NotImplementedError("append target must be a name")
            arr = c.func.value.id
            if len(c.args) != 1:
                raise NotImplementedError("append with 1 arg only")
            val = self.expr(c.args[0])
            self.o.w("npr = npr + 1")
            self.o.w(f"{arr}(npr) = {val}")
            return

        raise NotImplementedError("unsupported expression call")

    def emit_print_call(self, call):
        if len(call.args) != 1:
            raise NotImplementedError("print with exactly 1 arg supported here")
        a = call.args[0]

        # print(f"...{n}...")
        if isinstance(a, ast.JoinedStr):
            fmt_parts = []
            items = []
            for part in a.values:
                if is_const_str(part):
                    fmt_parts.append("a")
                    items.append("'" + part.value.replace("'", "''") + "'")
                elif isinstance(part, ast.FormattedValue):
                    fmt_parts.append("i0")
                    items.append(self.expr(part.value))
                else:
                    raise NotImplementedError("unsupported f-string part")
            fmt = "('" + "(" + ",".join(fmt_parts) + ")" + "')"
            # write(*,'(a,i0,a)') 'primes up to ', n, ':'
            self.o.w(f"write(*,{fmt}) " + ", ".join(items))
            return

        # print([])
        if isinstance(a, ast.List) and len(a.elts) == 0:
            self.o.w("write(*,'(a)') '[]'")
            return

        # print(primes)
        if isinstance(a, ast.Name) and a.id == "primes":
            self.o.w("call print_int_list(primes, npr)")
            return

        self.o.w(f"print *, {self.expr(a)}")

    def generic_visit(self, node):
        raise NotImplementedError(f"unsupported syntax: {type(node).__name__}")

    # ---------- helpers emitted into fortran ----------
    def emit_isqrt_int(self):
        self.o.w("integer function isqrt_int(x)")
        self.o.push()
        self.o.w("implicit none")
        self.o.w("integer, intent(in) :: x")
        self.o.w("integer :: r")
        self.o.w("if (x <= 0) then")
        self.o.push()
        self.o.w("isqrt_int = 0")
        self.o.w("return")
        self.o.pop()
        self.o.w("end if")
        self.o.w("r = int(sqrt(real(x)))")
        self.o.w("do while ((r+1)*(r+1) <= x)")
        self.o.push()
        self.o.w("r = r + 1")
        self.o.pop()
        self.o.w("end do")
        self.o.w("do while (r*r > x)")
        self.o.push()
        self.o.w("r = r - 1")
        self.o.pop()
        self.o.w("end do")
        self.o.w("isqrt_int = r")
        self.o.pop()
        self.o.w("end function isqrt_int")
        self.o.w("")

    def emit_print_int_list(self):
        self.o.w("subroutine print_int_list(a, n)")
        self.o.push()
        self.o.w("implicit none")
        self.o.w("integer, intent(in) :: a(:)")
        self.o.w("integer, intent(in) :: n")
        self.o.w("integer :: j")
        self.o.w("if (n <= 0) then")
        self.o.push()
        self.o.w("write(*,'(a)') '[]'")
        self.o.w("return")
        self.o.pop()
        self.o.w("end if")
        self.o.w("write(*,'(a)', advance='no') '['")
        self.o.w("do j = 1, n")
        self.o.push()
        self.o.w("if (j > 1) write(*,'(a)', advance='no') ', '")
        self.o.w("write(*,'(i0)', advance='no') a(j)")
        self.o.pop()
        self.o.w("end do")
        self.o.w("write(*,'(a)') ']'")
        self.o.pop()
        self.o.w("end subroutine print_int_list")
        self.o.w("")


def transpile_file(py_path):
    src = Path(py_path).read_text(encoding="utf-8")
    tree = ast.parse(src)

    prog = Path(py_path).stem
    t = py2f(program_name=prog)
    t.visit(tree)

    out_path = Path(py_path).with_suffix(".f90")
    out_path.write_text(t.o.text(), encoding="utf-8")
    return out_path


def main():
    if len(sys.argv) != 2:
        print("usage: python p2f.py input.py")
        return 2
    out = transpile_file(sys.argv[1])
    print(f"wrote {out}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
