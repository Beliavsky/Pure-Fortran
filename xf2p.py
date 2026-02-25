import re
import sys
import argparse
import subprocess
import shlex
import time
import difflib
from pathlib import Path
from datetime import datetime
from dataclasses import dataclass


def split_fortran_comment(line: str) -> tuple[str, str]:
    in_str = False
    quote = ""
    out: list[str] = []
    for ch in line:
        if in_str:
            out.append(ch)
            if ch == quote:
                in_str = False
        else:
            if ch in ("'", '"'):
                in_str = True
                quote = ch
                out.append(ch)
            elif ch == "!":
                code = "".join(out).rstrip()
                comment = line[len("".join(out)) + 1 :].strip()
                return code, comment
            else:
                out.append(ch)
    return "".join(out).rstrip(), ""


@dataclass
class UseSpec:
    module: str
    only_items: list[str] | None
    intrinsic: bool = False


def _clean_fortran_code_lines(src: str) -> list[str]:
    out: list[str] = []
    for raw in src.splitlines():
        code, _c = split_fortran_comment(raw)
        s = code.strip()
        if s:
            out.append(s)
    return out


def _parse_file_interface(src: str) -> tuple[list[str], list[str], list[UseSpec]]:
    """Return (defined_modules, defined_symbols, use_specs) for one source."""
    code_lines = _clean_fortran_code_lines(src)
    defs: list[str] = []
    mods: list[str] = []
    uses: list[UseSpec] = []

    for s in code_lines:
        sl = s.lower()

        # module declarations (skip 'module procedure')
        mm = re.match(r"^module\s+([a-z_]\w*)\b", sl, re.I)
        if mm and not re.match(r"^module\s+procedure\b", sl, re.I):
            mods.append(mm.group(1))
            continue

        # type declarations
        mt = re.match(r"^type\s*(?:,\s*[^:]*)?::\s*([a-z_]\w*)\b", sl, re.I)
        if mt:
            defs.append(mt.group(1))
            continue

        # function declarations
        mf = re.match(
            r"^(?!\s*end\s+function\b)\s*(?:(?:pure|elemental|recursive)\s+)*(?:\w+(?:\s*\([^)]*\))?\s+)*function\s+([a-z_]\w*)\s*\(",
            sl,
            re.I,
        )
        if mf:
            defs.append(mf.group(1))
            continue

        # subroutine declarations
        ms = re.match(
            r"^(?!\s*end\s+subroutine\b)\s*(?:(?:pure|elemental|recursive)\s+)*subroutine\s+([a-z_]\w*)\s*\(",
            sl,
            re.I,
        )
        if ms:
            defs.append(ms.group(1))
            continue

        # use statements
        mu = re.match(r"^use\s*(?:,\s*intrinsic\s*)?(?:::)?\s*([a-z_]\w*)\s*(.*)$", sl, re.I)
        if mu:
            mod = mu.group(1)
            tail = mu.group(2).strip()
            intrinsic = bool(re.search(r"\bintrinsic\b", sl, re.I))
            only_items: list[str] | None = None
            mo = re.search(r"\bonly\s*:\s*(.+)$", tail, re.I)
            if mo:
                only_raw = mo.group(1).strip()
                only_items = []
                for it in split_args(only_raw):
                    nm = it.strip()
                    if not nm:
                        continue
                    # keep original symbol before rename clause (a => b)
                    if "=>" in nm:
                        nm = nm.split("=>", 1)[1].strip()
                    nm = nm.strip()
                    if re.match(r"^[a-z_]\w*$", nm, re.I):
                        only_items.append(nm)
            uses.append(UseSpec(module=mod, only_items=only_items, intrinsic=intrinsic))
            continue

    return unique_preserve(mods), unique_preserve(defs), uses


def _insert_imports(py_text: str, import_lines: list[str]) -> str:
    if not import_lines:
        return py_text
    lines = py_text.splitlines()
    insert_at = 0
    while insert_at < len(lines):
        s = lines[insert_at].strip()
        if s.startswith("import ") or s.startswith("from "):
            insert_at += 1
            continue
        if s == "":
            insert_at += 1
            break
        break
    merged = lines[:insert_at] + import_lines + ([""] if import_lines and (insert_at < len(lines) and lines[insert_at].strip() != "") else []) + lines[insert_at:]
    return "\n".join(merged).rstrip() + "\n"


def collapse_fortran_continuations(raw_lines: list[tuple[str, str]]) -> list[tuple[str, str]]:
    """Collapse free-form continuation lines joined with trailing/leading '&'."""
    out: list[tuple[str, str]] = []
    i = 0
    n = len(raw_lines)
    while i < n:
        code, comment = raw_lines[i]
        cur_code = code.rstrip()
        cur_comment = comment
        while cur_code.rstrip().endswith("&"):
            cur_code = cur_code.rstrip()
            cur_code = cur_code[:-1].rstrip()
            i += 1
            if i >= n:
                break
            ncode, ncomment = raw_lines[i]
            s = ncode.lstrip()
            if s.startswith("&"):
                s = s[1:].lstrip()
            if cur_code and s:
                cur_code = f"{cur_code} {s}"
            else:
                cur_code = cur_code + s
            if ncomment.strip():
                if cur_comment.strip():
                    cur_comment = f"{cur_comment.strip()} | {ncomment.strip()}"
                else:
                    cur_comment = ncomment
        out.append((cur_code, cur_comment))
        i += 1
    return out


def find_matching_paren(text: str, open_pos: int) -> int:
    depth = 0
    in_str = False
    q = ""
    for p in range(open_pos, len(text)):
        ch = text[p]
        if in_str:
            if ch == q:
                in_str = False
            continue
        if ch in ("'", '"'):
            in_str = True
            q = ch
            continue
        if ch == "(":
            depth += 1
        elif ch == ")":
            depth -= 1
            if depth == 0:
                return p
    return -1


def split_args(s: str) -> list[str]:
    args: list[str] = []
    buf: list[str] = []
    pdepth = 0
    bdepth = 0
    in_str = False
    quote = ""
    for ch in s:
        if in_str:
            buf.append(ch)
            if ch == quote:
                in_str = False
            continue
        if ch in ("'", '"'):
            in_str = True
            quote = ch
            buf.append(ch)
            continue
        if ch == "(":
            pdepth += 1
            buf.append(ch)
            continue
        if ch == ")":
            pdepth = max(0, pdepth - 1)
            buf.append(ch)
            continue
        if ch == "[":
            bdepth += 1
            buf.append(ch)
            continue
        if ch == "]":
            bdepth = max(0, bdepth - 1)
            buf.append(ch)
            continue
        if ch == "," and pdepth == 0 and bdepth == 0:
            arg = "".join(buf).strip()
            if arg:
                args.append(arg)
            buf = []
            continue
        buf.append(ch)
    tail = "".join(buf).strip()
    if tail:
        args.append(tail)
    return args


def unique_preserve(items: list[str]) -> list[str]:
    seen: set[str] = set()
    out: list[str] = []
    for it in items:
        key = it.strip().lower()
        if key in seen:
            continue
        seen.add(key)
        out.append(it)
    return out


def _find_dups_ci(items: list[str]) -> list[str]:
    seen: set[str] = set()
    dups: list[str] = []
    for it in items:
        key = it.strip().lower()
        if key in seen and key not in dups:
            dups.append(key)
        seen.add(key)
    return dups


_type_scalar_hint = {
    "integer": "int",
    "real": "np.float64",
    "logical": "bool",
    "complex": "complex",
}

_type_default_scalar_value = {
    "integer": "0",
    "real": "np.float64(0.0)",
    "logical": "False",
    "complex": "0j",
}

_type_dtype = {
    "integer": "np.int_",
    "real": "np.float64",
    "logical": "np.bool_",
    "complex": "np.complex128",
}

_type_ndarray_hint = {
    "integer": "npt.NDArray[np.int_]",
    "real": "npt.NDArray[np.float64]",
    "logical": "npt.NDArray[np.bool_]",
    "complex": "npt.NDArray[np.complex128]",
}

_LOCAL_RUNTIME_HELPERS = {
    "_f_size",
    "_f_spread",
    "_f_assign_array",
    "mean_1d",
    "var_1d",
    "argsort_real",
    "random_normal_vec",
    "random_choice2",
    "random_choice_prob",
    "random_choice_norep",
}


def infer_function_result_ftype(header: str) -> str | None:
    """Infer scalar Fortran result type from function header when explicit.

    Examples:
      pure integer function f(...)
      real(kind=dp) pure function g(...)
    """
    h = header.strip()
    m = re.search(r"\b(integer|real|logical|complex)\b(?:\s*\([^)]*\))?\b[^!\n]*\bfunction\b", h, re.I)
    if not m:
        return None
    return m.group(1).lower()

_decl_re = re.compile(r"^(integer|real|logical|complex)\b(.*)::(.*)$", re.I)


def parse_decl(line: str):
    m = _decl_re.match(line.strip())
    if not m:
        return None
    ftype = m.group(1).lower()
    attrs = m.group(2).strip()
    rest = m.group(3).strip()
    return ftype, attrs, rest


def parse_decl_items(rest: str):
    items = []
    for part in split_args(rest):
        part = part.strip()
        if not part:
            continue
        init = None
        if "=" in part:
            left, init = part.split("=", 1)
            left = left.strip()
            init = init.strip()
        else:
            left = part.strip()

        shape = None
        mm = re.match(r"^([a-z_]\w*)\s*\(\s*(.+)\s*\)\s*$", left, re.I)
        if mm:
            name = mm.group(1)
            shape = mm.group(2).strip()
        else:
            name = left

        items.append((name, shape, init))
    return items


class basic_f2p:
    def __init__(self) -> None:
        self.out: list[str] = []
        self.indent = 0
        self.seen_parameter = False
        self._code_emit_count = 0
        self._block_code_start: list[int] = []
        self._decl_types: dict[str, str] = {}
        self._decl_array_types: dict[str, str] = {}
        self._subr_sigs: dict[str, dict[str, list[str]]] = {}
        self._current_result_name: str | None = None
        self._derived_types: set[str] = set()

    def _type_hint(self, ftype: str, type_name: str | None = None, is_array: bool = False) -> str:
        if ftype == "type":
            if is_array:
                return "npt.NDArray[object]"
            return type_name if type_name else "SimpleNamespace"
        if is_array:
            return _type_ndarray_hint.get(ftype, "npt.NDArray[np.float64]")
        return _type_scalar_hint.get(ftype, "int")

    def _parse_decl_line(self, s: str):
        pd = parse_decl(s)
        if pd:
            ftype, attrs, rest = pd
            return ftype, None, attrs.lower(), parse_decl_items(rest)
        td = re.match(r"^type\s*\(\s*([a-z_]\w*)\s*\)\s*(.*?)::\s*(.*)$", s, re.I)
        if td:
            return "type", td.group(1), td.group(2).strip().lower(), parse_decl_items(td.group(3).strip())
        return None

    def _validate_unit_symbols(self, unit_kind: str, unit_name: str, args: list[str], body_lines: list[tuple[str, str]]) -> None:
        dups = _find_dups_ci(args)
        if dups:
            raise ValueError(f"{unit_kind} {unit_name}: duplicate formal argument(s): {', '.join(dups)}")

        # Track declarations by lexical scope. A `block ... end block` introduces
        # a nested scope that may legitimately redeclare names used elsewhere.
        # Use a scope-id stack (not just depth), because sibling blocks share depth.
        seen_decl: dict[tuple[tuple[int, ...], str], tuple] = {}
        scope_stack: list[int] = [0]
        next_scope_id = 1
        for i, (code, _comment) in enumerate(body_lines, start=1):
            s = code.strip()
            sl = s.lower()
            if re.match(r"^block\b", sl):
                scope_stack.append(next_scope_id)
                next_scope_id += 1
                continue
            if re.match(r"^end\s+block\b", sl):
                if len(scope_stack) > 1:
                    scope_stack.pop()
                continue
            parsed = self._parse_decl_line(s)
            if not parsed:
                continue
            ftype, type_name, attrs_l, items = parsed
            for name, shape, _init in items:
                key = (tuple(scope_stack), name.lower())
                sig = (
                    ftype,
                    (type_name or "").lower(),
                    shape is not None,
                    (shape or "").replace(" ", "").lower(),
                    attrs_l.replace(" ", ""),
                )
                if key in seen_decl:
                    prev = seen_decl[key]
                    if prev != sig:
                        raise ValueError(
                            f"{unit_kind} {unit_name}: conflicting declaration for '{name}' at body line {i}"
                        )
                    raise ValueError(
                        f"{unit_kind} {unit_name}: duplicate declaration for '{name}' at body line {i}"
                    )
                seen_decl[key] = sig

    def transpile_derived_type(self, header: str, body_lines: list[tuple[str, str]]) -> None:
        h = header.strip()
        m = re.match(r"^type\b(?:\s*,[^:]*)?(?:\s*::\s*|\s+)([a-z_]\w*)\s*$", h, re.I)
        if not m:
            return
        tname = m.group(1)
        self._derived_types.add(tname)

        self.emit("@dataclass")
        self.emit(f"class {tname}:")
        self.indent += 1
        had_field = False
        for code, comment in body_lines:
            s = code.strip()
            pd = parse_decl(s)
            if pd:
                ftype, attrs, rest = pd
                attrs_l = attrs.lower()
                items = parse_decl_items(rest)
                type_name = None
            else:
                td = re.match(r"^type\s*\(\s*([a-z_]\w*)\s*\)\s*(.*?)::\s*(.*)$", s, re.I)
                if not td:
                    continue
                ftype = "type"
                attrs_l = td.group(2).strip().lower()
                items = parse_decl_items(td.group(3).strip())
                type_name = td.group(1)
            for name, shape, init in items:
                had_field = True
                cmt = f"  # {comment.strip()}" if comment.strip() else ""
                if shape is not None or "allocatable" in attrs_l:
                    self.emit(f"{name}: {self._type_hint(ftype, type_name, is_array=True)} | None = None{cmt}")
                    continue
                if ftype == "type":
                    self.emit(f"{name}: {self._type_hint(ftype, type_name, is_array=False)} | None = None{cmt}")
                    continue
                hint = self._type_hint(ftype, None, is_array=False)
                if init is None:
                    default_val = _type_default_scalar_value.get(ftype, "0")
                else:
                    default_val = self.translate_expr(init, set())
                self.emit(f"{name}: {hint} = {default_val}{cmt}")
        if not had_field:
            self.emit("pass")
        self.indent = max(0, self.indent - 1)
        self.emit("")

    @staticmethod
    def _remove_top_level_def(lines: list[str], name: str) -> list[str]:
        start = None
        for i, ln in enumerate(lines):
            if re.match(rf"^def\s+{re.escape(name)}\s*\(", ln):
                start = i
                break
        if start is None:
            return lines
        end = len(lines)
        for j in range(start + 1, len(lines)):
            if re.match(r"^(def|class)\s+\w+\s*\(|^@dataclass\b|^if __name__ == ", lines[j]):
                end = j
                break
        out = lines[:start]
        out.extend(lines[end:])
        while out and out[-1] == "":
            out.pop()
        return out

    @staticmethod
    def _drop_unused_runtime(lines: list[str]) -> list[str]:
        helper_names = [
            "_f_size",
            "_f_spread",
            "_f_assign_array",
            "mean_1d",
            "var_1d",
            "argsort_real",
            "random_normal_vec",
            "random_choice2",
            "random_choice_prob",
            "random_choice_norep",
        ]
        changed = True
        while changed:
            changed = False
            text = "\n".join(lines)
            for name in helper_names:
                # one reference means definition-only; remove it
                if len(re.findall(rf"\b{name}\s*\(", text)) <= 1:
                    new_lines = basic_f2p._remove_top_level_def(lines, name)
                    if len(new_lines) != len(lines):
                        lines = new_lines
                        text = "\n".join(lines)
                        changed = True

        # prune imports that are no longer needed
        def _drop_line(prefix: str) -> None:
            nonlocal lines
            lines = [ln for ln in lines if not ln.startswith(prefix)]

        text = "\n".join(lines)
        if "@dataclass" not in text:
            _drop_line("from dataclasses import dataclass")
        if "SimpleNamespace(" not in text:
            _drop_line("from types import SimpleNamespace")
        text = "\n".join(lines)
        if re.search(r"\bnpt\.", text) is None:
            _drop_line("import numpy.typing as npt")
        text = "\n".join(lines)
        if re.search(r"\bnp\.", text) is None:
            _drop_line("import numpy as np")
        return lines

    def emit(self, s: str = "") -> None:
        if s and not s.lstrip().startswith("#"):
            self._code_emit_count += 1
        self.out.append((" " * 4 * self.indent) + s if s else "")

    def emit_comment(self, c: str) -> None:
        cc = c.strip()
        if cc:
            self.emit(f"# {cc}")

    def _shape_to_py(self, shape: str, arrays_1d: set[str]) -> str:
        dims: list[str] = []
        for d in split_args(shape):
            ds = d.strip()
            if ":" in ds:
                p = ds.split(":", 1)
                lo = p[0].strip()
                hi = p[1].strip()
                if lo and hi:
                    if re.fullmatch(r"[+-]?\d+", lo) and lo == "1":
                        dims.append(self.translate_expr(hi, arrays_1d))
                    else:
                        lo_py = self.translate_expr(lo, arrays_1d)
                        hi_py = self.translate_expr(hi, arrays_1d)
                        dims.append(f"(({hi_py}) - ({lo_py}) + 1)")
                elif hi:
                    dims.append(self.translate_expr(hi, arrays_1d))
                else:
                    dims.append(ds)
            else:
                dims.append(self.translate_expr(ds, arrays_1d))
        if len(dims) == 1:
            return dims[0]
        return "(" + ", ".join(dims) + ")"

    def translate_expr(self, expr: str, arrays_1d: set[str]) -> str:
        s = expr.strip()
        s = s.replace("%", ".")
        had_concat = False

        # Fortran string concatenation operator.
        if "//" in s:
            out_concat: list[str] = []
            i = 0
            in_str = False
            quote = ""
            while i < len(s):
                ch = s[i]
                if in_str:
                    out_concat.append(ch)
                    if ch == quote:
                        in_str = False
                    i += 1
                    continue
                if ch in ("'", '"'):
                    in_str = True
                    quote = ch
                    out_concat.append(ch)
                    i += 1
                    continue
                if i + 1 < len(s) and s[i : i + 2] == "//":
                    out_concat.append(" + ")
                    had_concat = True
                    i += 2
                    continue
                out_concat.append(ch)
                i += 1
            s = "".join(out_concat)
            if had_concat and re.match(r"^\s*\+", s):
                s = '"" ' + s

        # kind(...) used as a kind selector
        # basic rule: if it uses a d exponent constant, treat it as double precision -> 8
        s = re.sub(r"\bkind\s*\(\s*[^)]*[dD][^)]*\)\s*", "8", s, flags=re.I)
        s = re.sub(r"\breal64\b", "8", s, flags=re.I)
        s = re.sub(r"\breal32\b", "4", s, flags=re.I)

        # fortran d exponent literal -> python e exponent literal
        s = re.sub(
            r"(?i)(\d+(?:\.\d*)?|\.\d+)[d]([+-]?\d+)",
            r"\1e\2",
            s,
        )
        # fortran kind suffix literal -> plain python numeric literal
        # e.g. 1.0_dp, 2_ikind, 3.5_rk
        s = re.sub(r"(?i)\b((?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?)\s*_[a-z_]\w*\b", r"\1", s)

        s = re.sub(r"\.true\.", "True", s, flags=re.I)
        s = re.sub(r"\.false\.", "False", s, flags=re.I)
        s = re.sub(r"\.eq\.", " == ", s, flags=re.I)
        s = re.sub(r"\.ne\.", " != ", s, flags=re.I)
        s = re.sub(r"\.lt\.", " < ", s, flags=re.I)
        s = re.sub(r"\.le\.", " <= ", s, flags=re.I)
        s = re.sub(r"\.gt\.", " > ", s, flags=re.I)
        s = re.sub(r"\.ge\.", " >= ", s, flags=re.I)
        s = s.replace("/=", " != ")
        s = re.sub(r"\.and\.", " and ", s, flags=re.I)
        s = re.sub(r"\.or\.", " or ", s, flags=re.I)
        s = re.sub(r"\.not\.", " not ", s, flags=re.I)

        s = re.sub(r"\bsqrt\s*\(", "np.sqrt(", s, flags=re.I)
        s = re.sub(r"\bacos\s*\(", "np.arccos(", s, flags=re.I)
        s = re.sub(r"\bcos\s*\(", "np.cos(", s, flags=re.I)
        s = re.sub(r"\bsin\s*\(", "np.sin(", s, flags=re.I)
        s = re.sub(r"\blog\s*\(", "np.log(", s, flags=re.I)
        s = re.sub(r"\bexp\s*\(", "np.exp(", s, flags=re.I)
        s = re.sub(r"\bmax\s*\(", "np.maximum(", s, flags=re.I)
        s = re.sub(r"\bmin\s*\(", "np.minimum(", s, flags=re.I)
        s = re.sub(r"\breshape\s*\(", "np.reshape(", s, flags=re.I)
        s = re.sub(r"\bspread\s*\(", "_f_spread(", s, flags=re.I)
        s = re.sub(r"\bmod\s*\(", "np.mod(", s, flags=re.I)
        s = re.sub(r"\bmaxval\s*\(", "np.max(", s, flags=re.I)
        s = re.sub(r"\bminval\s*\(", "np.min(", s, flags=re.I)
        s = re.sub(r"\bcount\s*\(", "np.count_nonzero(", s, flags=re.I)
        s = re.sub(r"\breal\s*\(", "np.float64(", s, flags=re.I)
        s = re.sub(r"\bint\s*\(", "int(", s, flags=re.I)
        s = re.sub(r"\bsum\s*\(", "np.sum(", s, flags=re.I)
        s = re.sub(
            r"\ballocated\s*\(\s*([a-z_]\w*)\.([a-z_]\w*)\s*\)",
            r"(hasattr(\1, '\2') and (getattr(\1, '\2') is not None))",
            s,
            flags=re.I,
        )
        s = re.sub(r"\ballocated\s*\(\s*([^)]+?)\s*\)", r"(\1 is not None)", s, flags=re.I)
        s = re.sub(r"\bpresent\s*\(\s*([a-z_]\w*)\s*\)", r"(\1 is not None)", s, flags=re.I)
        s = re.sub(r"\btiny\s*\(\s*[^)]*\)", "np.finfo(float).tiny", s, flags=re.I)
        s = re.sub(r"\bhuge\s*\(\s*[^)]*\)", "np.finfo(float).max", s, flags=re.I)
        s = s.replace("np.np.", "np.")
        s = re.sub(
            r"np\.float64\s*\(\s*(.+?)\s*,\s*kind\s*=\s*[a-z_]\w*\s*\)",
            r"np.asarray(\1, dtype=np.float64)",
            s,
            flags=re.I,
        )

        s = re.sub(r"\bsize\s*\(\s*([a-z_]\w*)\s*\)", r"_f_size(\1)", s, flags=re.I)
        s = re.sub(r"\bsize\s*\(\s*([^)]+)\s*\)", r"_f_size(\1)", s, flags=re.I)
        s = s.replace("np.np.", "np.")

        def _rewrite_np_sum_calls(txt: str) -> str:
            out: list[str] = []
            i = 0
            n = len(txt)
            while i < n:
                k = txt.lower().find("np.sum(", i)
                if k == -1:
                    out.append(txt[i:])
                    break
                out.append(txt[i:k])
                p0 = k + len("np.sum")
                if p0 >= n or txt[p0] != "(":
                    out.append(txt[k:k + 1])
                    i = k + 1
                    continue
                p1 = find_matching_paren(txt, p0)
                if p1 == -1:
                    out.append(txt[k:])
                    break
                inner = txt[p0 + 1 : p1]
                parts = [p.strip() for p in split_args(inner) if p.strip()]
                if not parts:
                    out.append("np.sum()")
                    i = p1 + 1
                    continue
                arr = parts[0]
                dim_expr = None
                mask_expr = None
                for ptxt in parts[1:]:
                    mk = re.match(r"^([a-z_]\w*)\s*=\s*(.+)$", ptxt, re.I)
                    if mk:
                        key = mk.group(1).lower()
                        val = mk.group(2).strip()
                        if key == "dim":
                            dim_expr = val
                        elif key == "mask":
                            mask_expr = val
                    elif dim_expr is None:
                        dim_expr = ptxt
                if mask_expr is not None:
                    arr_eff = f"np.where({mask_expr}, {arr}, 0.0)"
                else:
                    arr_eff = arr
                if dim_expr is not None:
                    repl = f"np.sum({arr_eff}, axis=({dim_expr}) - 1)"
                else:
                    repl = f"np.sum({arr_eff})"
                out.append(repl)
                i = p1 + 1
            return "".join(out)

        s = _rewrite_np_sum_calls(s)

        # 1d array element: a(i) -> a[(i)-1] (assume 1-based Fortran indexing)
        # Use a scanner (not regex) so nested references like x(idx(i)+1) work.
        def _convert_refs(txt: str) -> str:
            out: list[str] = []
            i = 0
            n = len(txt)
            while i < n:
                m = re.match(r"[A-Za-z_]\w*(?:\.[A-Za-z_]\w*)*", txt[i:])
                if not m:
                    out.append(txt[i])
                    i += 1
                    continue
                name = m.group(0)
                j = i + len(name)
                k = j
                while k < n and txt[k].isspace():
                    k += 1
                if k < n and txt[k] == "(":
                    pclose = find_matching_paren(txt, k)
                    if pclose != -1:
                        inner = txt[k + 1 : pclose]
                        inner_py = self.translate_expr(inner, arrays_1d)
                        root = name.split(".", 1)[0].lower()
                        dotted_array_ref = ("." in name) and (root not in {"np", "math", "random"})
                        if name in arrays_1d or dotted_array_ref:
                            if "," in inner:
                                parts = [p.strip() for p in split_args(inner)]
                                idx_parts: list[str] = []
                                for ptxt in parts:
                                    if ptxt == ":":
                                        idx_parts.append(":")
                                    elif ":" in ptxt:
                                        lo, hi = ptxt.split(":", 1)
                                        lo = lo.strip()
                                        hi = hi.strip()
                                        lo_py = self.translate_expr(lo, arrays_1d) if lo else ""
                                        hi_py = self.translate_expr(hi, arrays_1d) if hi else ""
                                        start = f"(int({lo_py}) - 1)" if lo_py else ""
                                        stop = f"int({hi_py})" if hi_py else ""
                                        idx_parts.append(f"{start}:{stop}")
                                    else:
                                        idx_parts.append(f"({self.translate_expr(ptxt, arrays_1d)}) - 1")
                                out.append(f"{name}[{', '.join(idx_parts)}]")
                                i = pclose + 1
                                continue
                            if ":" in inner:
                                lo, hi = inner.split(":", 1)
                                lo = lo.strip()
                                hi = hi.strip()
                                lo_py = self.translate_expr(lo, arrays_1d) if lo else ""
                                hi_py = self.translate_expr(hi, arrays_1d) if hi else ""
                                start = f"(int({lo_py}) - 1)" if lo_py else ""
                                stop = f"int({hi_py})" if hi_py else ""
                                out.append(f"{name}[{start}:{stop}]")
                            else:
                                out.append(f"{name}[({inner_py}) - 1]")
                        else:
                            out.append(f"{name}({inner_py})")
                        i = pclose + 1
                        continue
                out.append(name)
                i = j
            return "".join(out)

        s = _convert_refs(s)
        return s

    def transpile_assignment(self, lhs: str, rhs_py: str, arrays_1d: set[str]) -> None:
        lhs = lhs.replace("%", ".")
        lhs_base = lhs.split(".", 1)[0].strip().lower()
        if self._decl_types.get(lhs_base) == "integer":
            rhs_py = f"int({rhs_py})"
        mname = re.match(r"^\s*([a-z_]\w*)\s*\(", lhs, re.I)
        idx_name = None
        idx = None
        if mname:
            name_try = mname.group(1)
            open_pos = lhs.find("(", mname.end(1))
            if open_pos >= 0:
                close_pos = find_matching_paren(lhs, open_pos)
                if close_pos == len(lhs) - 1:
                    idx_name = name_try
                    idx = lhs[open_pos + 1 : close_pos].strip()
        if idx_name is not None and idx is not None:
            name = idx_name
            if name in arrays_1d:
                if "," in idx:
                    parts = [p.strip() for p in split_args(idx)]
                    idx_parts: list[str] = []
                    for ptxt in parts:
                        if ptxt == ":":
                            idx_parts.append(":")
                        elif ":" in ptxt:
                            lo, hi = ptxt.split(":", 1)
                            lo = lo.strip()
                            hi = hi.strip()
                            lo_py = self.translate_expr(lo, arrays_1d) if lo else ""
                            hi_py = self.translate_expr(hi, arrays_1d) if hi else ""
                            start = f"(int({lo_py}) - 1)" if lo_py else ""
                            stop = f"int({hi_py})" if hi_py else ""
                            idx_parts.append(f"{start}:{stop}")
                        else:
                            idx_parts.append(f"({self.translate_expr(ptxt, arrays_1d)}) - 1")
                    self.emit(f"{name}[{', '.join(idx_parts)}] = {rhs_py}")
                    return
                if ":" in idx:
                    lo, hi = idx.split(":", 1)
                    lo = lo.strip()
                    hi = hi.strip()
                    lo_py = self.translate_expr(lo, arrays_1d) if lo else ""
                    hi_py = self.translate_expr(hi, arrays_1d) if hi else ""
                    start = f"(int({lo_py}) - 1)" if lo_py else ""
                    stop = f"int({hi_py})" if hi_py else ""
                    self.emit(f"{name}[{start}:{stop}] = {rhs_py}")
                else:
                    idx_py = self.translate_expr(idx, arrays_1d)
                    self.emit(f"{name}[({idx_py}) - 1] = {rhs_py}")
                return
        if lhs in arrays_1d:
            self.emit(f"{lhs} = _f_assign_array({lhs}, {rhs_py})")
            return
        self.emit(f"{lhs} = {rhs_py}")

    def transpile_simple_stmt(self, stmt: str, arrays_1d: set[str]) -> None:
        s = stmt.strip()
        if self.handle_exec_line(s, arrays_1d):
            return
        mm = re.match(r'error\s+stop\s+(.+)$', s, re.I)
        if mm:
            self.emit(f"raise RuntimeError({mm.group(1).strip()})")
            return
        if s.lower() == "return":
            if self._current_result_name:
                self.emit(f"return {self._current_result_name}")
            else:
                self.emit("return")
            return
        if "=" in s and "::" not in s:
            lhs, rhs = s.split("=", 1)
            lhs = lhs.strip()
            rhs_py = self.translate_expr(rhs, arrays_1d)
            if lhs in arrays_1d and rhs_py in ("True", "False"):
                self.emit(f"{lhs}[:] = {rhs_py}")
                return
            self.transpile_assignment(lhs, rhs_py, arrays_1d)
        else:
            self.emit("# unsupported inline statement")
            self.emit("pass")

    def emit_parameters_from_decl(self, ftype: str, attrs_l: str, rest: str, arrays_1d: set[str]) -> set[str]:
        names: set[str] = set()
        if "parameter" not in attrs_l:
            return names
        if ftype == "type":
            return names
        for name, shape, init in parse_decl_items(rest):
            if init is None:
                continue
            # parameters are named constants: method (2) -> Final
            hint = _type_scalar_hint.get(ftype, "int")
            val = self.translate_expr(init, arrays_1d)
            self.emit(f"{name}: Final[{hint}] = {val}")
            names.add(name)
        return names

    def emit_var_inits_from_sym(
        self,
        sym: dict[str, dict],
        arrays_1d: set[str],
        parameter_names: set[str],
        skip_names: set[str] | None = None,
    ) -> None:
        # allocate explicit-shape arrays and initialize scalars so python is always valid
        if skip_names is None:
            skip_names = set()
        for name, info in sym.items():
            if name in parameter_names:
                continue
            if name in skip_names:
                continue
            ftype = info["ftype"]
            ftype_name = info.get("type_name")
            is_array = info["is_array"]
            shape = info["shape"]
            init = info["init"]
            alloc = info["alloc"]

            if is_array:
                # only allocate explicit-shape, non-allocatable arrays
                if alloc:
                    self.emit(f"{name} = None")
                    continue
                if shape is None:
                    continue
                if shape.strip() == ":":
                    continue
                if ftype == "type":
                    shape_py = self._shape_to_py(shape, arrays_1d)
                    self.emit(f"{name} = np.empty({shape_py}, dtype=object)")
                    continue
                dtype = _type_dtype[ftype]
                shape_py = self._shape_to_py(shape, arrays_1d)
                hint = _type_ndarray_hint[ftype]
                if init is None:
                    self.emit(f"{name}: {hint} = np.empty({shape_py}, dtype={dtype})")
                else:
                    init_py = self.translate_expr(init, arrays_1d)
                    self.emit(f"{name}: {hint} = np.full({shape_py}, {init_py}, dtype={dtype})")
            else:
                # scalar
                if ftype == "type":
                    cls = ftype_name if ftype_name else "SimpleNamespace"
                    self.emit(f"{name} = {cls}()")
                    continue
                hint = _type_scalar_hint.get(ftype, "int")
                if init is None:
                    default_val = _type_default_scalar_value.get(ftype, "0")
                    self.emit(f"{name}: {hint} = {default_val}")
                else:
                    init_py = self.translate_expr(init, arrays_1d)
                    self.emit(f"{name}: {hint} = {init_py}")

    def handle_exec_line(self, s: str, arrays_1d: set[str]) -> bool:
        sl = s.lower()

        # ignore some non-exec lines
        if sl in ("implicit none", "contains"):
            return True
        if sl.startswith("use "):
            return True
        if sl.startswith("end function") or sl.startswith("end program") or sl.startswith("end module"):
            return True
        if sl == "end":
            return True

        # if (...) then
        mm = re.match(r"if\s*\(\s*(.+)\s*\)\s*then$", s, re.I)
        if mm:
            cond = self.translate_expr(mm.group(1), arrays_1d)
            self.emit(f"if {cond}:")
            self.indent += 1
            self._block_code_start.append(self._code_emit_count)
            return True

        mm = re.match(r"else\s+if\s*\(\s*(.+)\s*\)\s*then$", s, re.I)
        if mm:
            cond = self.translate_expr(mm.group(1), arrays_1d)
            self.indent = max(0, self.indent - 1)
            self.emit(f"elif {cond}:")
            self.indent += 1
            return True

        if sl == "else":
            self.indent = max(0, self.indent - 1)
            self.emit("else:")
            self.indent += 1
            return True

        if sl.startswith("end if"):
            if self._block_code_start:
                start = self._block_code_start.pop()
                if self._code_emit_count == start:
                    self.emit("pass")
            self.indent = max(0, self.indent - 1)
            return True

        # do while (...)
        mm = re.match(r"do\s+while\s*\(\s*(.+)\s*\)$", s, re.I)
        if mm:
            cond = self.translate_expr(mm.group(1), arrays_1d)
            self.emit(f"while {cond}:")
            self.indent += 1
            self._block_code_start.append(self._code_emit_count)
            return True

        # do i = a, b
        mm = re.match(r"do\s+([a-z_]\w*)\s*=\s*(.+?)\s*,\s*(.+)$", s, re.I)
        if mm:
            var = mm.group(1)
            a = self.translate_expr(mm.group(2), arrays_1d)
            b = self.translate_expr(mm.group(3), arrays_1d)
            self.emit(f"for {var} in range({a}, ({b}) + 1):")
            self.indent += 1
            self._block_code_start.append(self._code_emit_count)
            return True

        if sl.startswith("end do"):
            if self._block_code_start:
                start = self._block_code_start.pop()
                if self._code_emit_count == start:
                    self.emit("pass")
            self.indent = max(0, self.indent - 1)
            return True

        # allocate(a(n))
        mm = re.match(r"allocate\s*\(\s*([a-z_]\w*)\s*\(\s*(.+)\s*\)\s*\)\s*$", s, re.I)
        if mm:
            name = mm.group(1)
            sz = self._shape_to_py(mm.group(2), arrays_1d)
            ftype = self._decl_types.get(name.lower(), "")
            if not ftype:
                ftype = self._decl_array_types.get(name.lower(), "")
            if ftype == "integer":
                self.emit(f"{name} = np.empty({sz}, dtype=int)")
            elif ftype == "logical":
                self.emit(f"{name} = np.empty({sz}, dtype=bool)")
            elif ftype == "real":
                self.emit(f"{name} = np.empty({sz}, dtype=np.float64)")
            else:
                self.emit(f"{name} = np.empty({sz})")
            return True

        # open(newunit=fp, file="temp.txt", status="replace", action="write", ...)
        mm = re.match(r"open\s*\(\s*(.+)\s*\)\s*$", s, re.I)
        if mm:
            spec = mm.group(1).strip()
            parts = split_args(spec)
            kws = {}
            for p in parts:
                mk = re.match(r"^\s*([a-z_]\w*)\s*=\s*(.+?)\s*$", p, re.I)
                if mk:
                    kws[mk.group(1).lower()] = mk.group(2).strip()
            newunit = kws.get("newunit", None)
            file_expr = kws.get("file", None)
            status = kws.get("status", None)
            action = kws.get("action", None)
            position = kws.get("position", None)
            if newunit is None or file_expr is None:
                # fallback for unsupported OPEN forms
                self.emit(f"# unsupported open form: {s}")
                return True
            mode = "r"
            st = status.strip().strip("'\"").lower() if status else ""
            act = action.strip().strip("'\"").lower() if action else ""
            pos = position.strip().strip("'\"").lower() if position else ""
            if act == "write" or st == "replace":
                mode = "w"
            if pos == "append":
                mode = "a"
            self.emit(f"{newunit} = open({self.translate_expr(file_expr, arrays_1d)}, {mode!r})")
            return True

        # close(fp)
        mm = re.match(r"close\s*\(\s*(.+?)\s*\)\s*$", s, re.I)
        if mm:
            unit = self.translate_expr(mm.group(1), arrays_1d)
            self.emit(f"{unit}.close()")
            return True

        # call random_number(x)
        mm = re.match(r"call\s+random_number\s*\(\s*([a-z_]\w*)\s*\)\s*$", s, re.I)
        if mm:
            name = mm.group(1)
            if name in arrays_1d:
                self.emit(f"{name} = _f_assign_array({name}, np.random.random(size={name}.shape))")
            else:
                self.emit(f"{name} = np.float64(np.random.random())")
            return True

        # open(newunit=fp, file="temp.txt", status="replace", action="write", ...)
        mm = re.match(r"open\s*\(\s*(.+)\s*\)\s*$", s, re.I)
        if mm:
            spec = mm.group(1).strip()
            parts = split_args(spec)
            kws = {}
            for p in parts:
                mk = re.match(r"^\s*([a-z_]\w*)\s*=\s*(.+?)\s*$", p, re.I)
                if mk:
                    kws[mk.group(1).lower()] = mk.group(2).strip()
            newunit = kws.get("newunit", None)
            file_expr = kws.get("file", None)
            status = kws.get("status", None)
            action = kws.get("action", None)
            position = kws.get("position", None)
            if newunit is None or file_expr is None:
                self.emit(f"# unsupported open form: {s}")
                return True
            mode = "r"
            st = status.strip().strip("'\"").lower() if status else ""
            act = action.strip().strip("'\"").lower() if action else ""
            pos = position.strip().strip("'\"").lower() if position else ""
            if act == "write" or st == "replace":
                mode = "w"
            if pos == "append":
                mode = "a"
            self.emit(f"{newunit} = open({self.translate_expr(file_expr, arrays_1d)}, {mode!r})")
            return True

        # close(fp)
        mm = re.match(r"close\s*\(\s*(.+?)\s*\)\s*$", s, re.I)
        if mm:
            unit = self.translate_expr(mm.group(1), arrays_1d)
            self.emit(f"{unit}.close()")
            return True

        # read(fp,*) a, b, arr(i,:), ...
        mm = re.match(r"read\s*\(\s*([a-z_]\w*)\s*,\s*\*\s*\)\s*(.+)$", s, re.I)
        if mm:
            unit = mm.group(1)
            rest = mm.group(2).strip()
            items = [a.strip() for a in split_args(rest) if a.strip()]
            if len(items) == 1:
                tgt = items[0]
                # row-slice target: mat(i,:)
                mrow = re.match(r"^([a-z_]\w*)\s*\(\s*([^,]+)\s*,\s*:\s*\)\s*$", tgt, re.I)
                if mrow:
                    arr = mrow.group(1)
                    ridx = self.translate_expr(mrow.group(2), arrays_1d)
                    ftype = self._decl_array_types.get(arr.lower(), "")
                    cast = "int" if ftype == "integer" else "float"
                    self.emit(f"__read_vals = [{cast}(v) for v in {unit}.readline().split()]")
                    self.emit(f"{arr}[({ridx}) - 1, :] = np.asarray(__read_vals[:{arr}.shape[1]], dtype={arr}.dtype)")
                    return True
                # scalar target
                lhs = self.translate_expr(tgt, arrays_1d)
                base = re.match(r"^([a-z_]\w*)", tgt, re.I)
                bnm = base.group(1).lower() if base else ""
                ftype = self._decl_types.get(bnm, "")
                cast = "int" if ftype == "integer" else ("bool" if ftype == "logical" else "float")
                self.emit(f"{lhs} = {cast}({unit}.readline().split()[0])")
                return True
            # multiple scalar targets
            self.emit(f"__read_parts = {unit}.readline().split()")
            for i, tgt in enumerate(items):
                lhs = self.translate_expr(tgt, arrays_1d)
                base = re.match(r"^([a-z_]\w*)", tgt, re.I)
                bnm = base.group(1).lower() if base else ""
                ftype = self._decl_types.get(bnm, "")
                cast = "int" if ftype == "integer" else ("bool" if ftype == "logical" else "float")
                self.emit(f"{lhs} = {cast}(__read_parts[{i}])")
            return True

        # call foo(...)
        mm = re.match(r"call\s+([a-z_]\w*)\s*\((.*)\)\s*$", s, re.I)
        if mm:
            cname = mm.group(1)
            cname_l = cname.lower()
            argtxt = mm.group(2).strip()
            args_py: list[str] = []
            kwargs_py: dict[str, str] = {}
            if argtxt:
                for a in split_args(argtxt):
                    a = a.strip()
                    mk = re.match(r"^([a-z_]\w*)\s*=\s*(.+)$", a, re.I)
                    if mk:
                        kwargs_py[mk.group(1).lower()] = self.translate_expr(mk.group(2), arrays_1d)
                    else:
                        args_py.append(self.translate_expr(a, arrays_1d))
            # Fortran random_seed adapters for valid Python
            if cname.lower() == "random_seed":
                if "size" in kwargs_py:
                    self.emit(f"{kwargs_py['size']} = 1")
                elif "put" in kwargs_py:
                    self.emit(f"np.random.seed(int(np.sum({kwargs_py['put']})) % (2**32 - 1))")
                else:
                    self.emit("np.random.seed(None)")
                return True
            merged = list(args_py)
            for k, v in kwargs_py.items():
                merged.append(f"{k}={v}")
            sig = self._subr_sigs.get(cname_l)
            if sig:
                formal_args = sig.get("args", [])
                out_formals = sig.get("out", [])
                if kwargs_py == {} and len(formal_args) > 0 and len(args_py) > len(formal_args):
                    # Robustness for malformed Fortran with duplicated actual arguments.
                    args_py = args_py[: len(formal_args)]
                    merged = list(args_py)
                actual_by_formal: dict[str, str] = {}
                for i, formal in enumerate(formal_args):
                    if i < len(args_py):
                        actual_by_formal[formal] = args_py[i]
                for k, v in kwargs_py.items():
                    actual_by_formal[k] = v
                out_actuals = [actual_by_formal.get(fm, "") for fm in out_formals]
                if out_actuals and all(x != "" for x in out_actuals):
                    self.emit(f"{', '.join(out_actuals)} = {cname}({', '.join(merged)})")
                    return True
            self.emit(f"{cname}({', '.join(merged)})")
            return True

        # return
        if sl == "return":
            if self._current_result_name:
                self.emit(f"return {self._current_result_name}")
            else:
                self.emit("return")
            return True
        if sl == "exit":
            self.emit("break")
            return True
        mm = re.match(r"(?:error\s+)?stop(?:\s+(.+))?$", s, re.I)
        if mm:
            msg = (mm.group(1) or '"stop"').strip()
            self.emit(f"raise RuntimeError({msg})")
            return True

        # print *, ...
        mm = re.match(r"print\s*\*\s*,\s*(.+)$", s, re.I)
        if mm:
            raw_args = [a.strip() for a in split_args(mm.group(1))]
            if len(raw_args) == 1:
                a0 = raw_args[0]
                if re.fullmatch(r"[a-z_]\w*", a0, flags=re.I) and a0.lower() in self._decl_array_types:
                    self.emit(f"print(*np.ravel({a0}, order='F'))")
                    return True
            args2 = []
            for a in raw_args:
                if a.startswith(("'", '"')):
                    args2.append(a)
                else:
                    args2.append(self.translate_expr(a, arrays_1d))
            self.emit(f"print({', '.join(args2)})")
            return True

        # write(*,*) ...
        mm = re.match(r"write\s*\(\s*\*\s*,\s*\*\s*\)\s*(.+)$", s, re.I)
        if mm:
            raw_args = [a.strip() for a in split_args(mm.group(1))]
            if len(raw_args) == 1:
                a0 = raw_args[0]
                if re.fullmatch(r"[a-z_]\w*", a0, flags=re.I) and a0.lower() in self._decl_array_types:
                    self.emit(f"print(*np.ravel({a0}, order='F'))")
                    return True
            args2 = []
            for a in raw_args:
                if a.startswith(("'", '"')):
                    args2.append(a)
                else:
                    args2.append(self.translate_expr(a, arrays_1d))
            self.emit(f"print({', '.join(args2)})")
            return True

        # write(unit,*) ...
        mm = re.match(r"write\s*\(\s*([a-z_]\w*)\s*,\s*\*\s*\)\s*(.*)$", s, re.I)
        if mm:
            unit = mm.group(1)
            rest = mm.group(2).strip()
            if not rest:
                self.emit(f"print(file={unit})")
                return True
            args2 = []
            for a in split_args(rest):
                a = a.strip()
                if a.startswith(("'", '"')):
                    args2.append(a)
                else:
                    args2.append(self.translate_expr(a, arrays_1d))
            self.emit(f"print({', '.join(args2)}, file={unit})")
            return True

        # write(*,"fmt") ...  -> fallback to print(...)
        mm = re.match(r"write\s*\(\s*\*\s*,\s*([\"'].*[\"'])\s*\)\s*(.*)$", s, re.I)
        if mm:
            rest = mm.group(2).strip()
            if not rest:
                self.emit("print()")
                return True
            args2 = []
            for a in split_args(rest):
                a = a.strip()
                if a.startswith(("'", '"')):
                    args2.append(a)
                else:
                    args2.append(self.translate_expr(a, arrays_1d))
            self.emit(f"print({', '.join(args2)})")
            return True

        # generic write(...) ... fallback
        mm = re.match(r"write\s*\(\s*(.*?)\s*\)\s*(.*)$", s, re.I)
        if mm:
            ctl = mm.group(1).strip().lower()
            rest = mm.group(2).strip()
            ctl_parts = split_args(ctl)
            unit_expr = "*"
            if ctl_parts:
                unit_expr = ctl_parts[0].strip()
            args2 = []
            if rest:
                for a in split_args(rest):
                    a = a.strip()
                    if a.startswith(("'", '"')):
                        args2.append(a)
                    else:
                        args2.append(self.translate_expr(a, arrays_1d))
            end_txt = ', end=""' if "advance" in ctl and "'no'" in ctl else ""
            file_txt = ""
            if unit_expr != "*":
                file_txt = f", file={unit_expr}"
            if args2:
                self.emit(f"print({', '.join(args2)}{end_txt}{file_txt})")
            else:
                if file_txt:
                    self.emit(f"print({file_txt[2:]})")
                else:
                    self.emit("print()")
            return True

        # single-line if: if (cond) stmt
        if sl.startswith("if") and not sl.endswith("then"):
            p0 = s.find("(")
            if p0 != -1:
                p1 = find_matching_paren(s, p0)
                if p1 != -1:
                    cond_txt = s[p0 + 1 : p1].strip()
                    stmt = s[p1 + 1 :].strip()
                    if stmt:
                        cond = self.translate_expr(cond_txt, arrays_1d)
                        self.emit(f"if {cond}:")
                        self.indent += 1
                        self.transpile_simple_stmt(stmt, arrays_1d)
                        self.indent = max(0, self.indent - 1)
                        return True

        # assignment
        if "=" in s and "::" not in s:
            lhs, rhs = s.split("=", 1)
            lhs = lhs.strip()
            rhs_py = self.translate_expr(rhs, arrays_1d)
            if lhs in arrays_1d:
                self.emit(f"{lhs} = _f_assign_array({lhs}, {rhs_py})")
                return True
            self.transpile_assignment(lhs, rhs_py, arrays_1d)
            return True

        return False

    def transpile_function(self, header: str, body_lines: list[tuple[str, str]]) -> None:
        hdr = header.strip()
        # tolerate arbitrary prefixes such as:
        # "pure real(kind=dp) function f(...)" or "real(kind=dp) pure function f(...)"
        m = re.search(r"\bfunction\s+(\w+)\s*\(\s*([^\)]*)\s*\)\s*result\s*\(\s*(\w+)\s*\)", hdr, re.I)
        if m:
            fname = m.group(1)
            args = [a.strip() for a in m.group(2).split(",") if a.strip()]
            result_name = m.group(3)
        else:
            m2 = re.search(r"\bfunction\s+(\w+)\s*\(\s*([^\)]*)\s*\)", hdr, re.I)
            if not m2:
                return
            fname = m2.group(1)
            args = [a.strip() for a in m2.group(2).split(",") if a.strip()]
            result_name = fname

        self._validate_unit_symbols("function", fname, args, body_lines)

        sym: dict[str, dict] = {}
        arrays_1d: set[str] = set()
        arg_hints: dict[str, str] = {}
        header_ftype = infer_function_result_ftype(hdr)
        result_hint = _type_scalar_hint.get(header_ftype, "int")
        result_is_scalar = True
        result_is_derived = False
        result_type_name: str | None = None

        # declarations pass
        for code, _comment in body_lines:
            s = code.strip()
            pd = parse_decl(s)
            if pd:
                ftype, attrs, rest = pd
                attrs_l = attrs.lower()
                items = parse_decl_items(rest)
                type_name = None
            else:
                td = re.match(r"^type\s*\(\s*([a-z_]\w*)\s*\)\s*(.*?)::\s*(.*)$", s, re.I)
                if not td:
                    continue
                ftype = "type"
                attrs_l = td.group(2).strip().lower()
                items = parse_decl_items(td.group(3).strip())
                type_name = td.group(1)

            for name, shape, init in items:
                is_array = shape is not None
                is_alloc = "allocatable" in attrs_l
                sym[name] = {
                    "ftype": ftype,
                    "type_name": type_name,
                    "is_array": is_array,
                    "shape": shape,
                    "init": init,
                    "alloc": is_alloc,
                    "attrs_l": attrs_l,
                }
                if is_array:
                    arrays_1d.add(name)

                if name in args and "intent(in" in attrs_l:
                    if is_array:
                        arg_hints[name] = self._type_hint(ftype, type_name, is_array=True)
                    else:
                        arg_hints[name] = self._type_hint(ftype, type_name, is_array=False)

                if name == result_name:
                    if is_array:
                        result_hint = _type_ndarray_hint.get(ftype, "npt.NDArray[np.float64]")
                        result_is_scalar = False
                    else:
                        if ftype == "type":
                            result_hint = self._type_hint(ftype, type_name, is_array=False)
                            result_is_scalar = False
                            result_is_derived = True
                            result_type_name = type_name
                        else:
                            result_hint = _type_scalar_hint[ftype]
                            result_is_scalar = True

        args_annot = []
        for a in args:
            hint = arg_hints.get(a, "int")
            default_none = False
            info = sym.get(a)
            if info and "optional" in info.get("attrs_l", ""):
                default_none = True
            if default_none:
                args_annot.append(f"{a}: {hint} = None")
            else:
                args_annot.append(f"{a}: {hint}")

        self.emit(f"def {fname}({', '.join(args_annot)}) -> {result_hint}:")
        self.indent += 1

        # If the first non-code lines are comments (just after signature),
        # emit them as a Python docstring.
        lead_doc: list[str] = []
        lead_idx = 0
        for code, comment in body_lines:
            if code.strip():
                break
            if comment.strip():
                lead_doc.append(comment.strip())
            lead_idx += 1
        if lead_doc:
            if len(lead_doc) == 1:
                self.emit(f'"""{lead_doc[0]}"""')
            else:
                self.emit('"""')
                for ln in lead_doc:
                    self.emit(ln)
                self.emit('"""')

        # parameters inside function (if any)
        parameter_names: set[str] = set()
        for code, _comment in body_lines:
            s = code.strip()
            pd = parse_decl(s)
            if not pd:
                continue
            ftype, attrs, rest = pd
            attrs_l = attrs.lower()
            if "parameter" in attrs_l:
                parameter_names |= self.emit_parameters_from_decl(ftype, attrs_l, rest, arrays_1d)

        # allocate/init explicit-shape arrays and scalars
        self.emit_var_inits_from_sym(sym, arrays_1d, parameter_names, skip_names=set(args))
        # initialize derived-type/component bases that appear as name%field
        comp_bases: set[str] = set()
        for code, _comment in body_lines:
            for mbase in re.finditer(r"\b([a-z_]\w*)\s*%", code, re.I):
                comp_bases.add(mbase.group(1))
        known_names = {k.lower() for k in sym} | {a.lower() for a in args}
        for b in sorted(comp_bases):
            if b.lower() not in known_names and b not in parameter_names:
                self.emit(f"{b} = SimpleNamespace()")
        result_has_components = any(b.lower() == result_name.lower() for b in comp_bases)
        if (result_has_components or result_is_derived) and result_name.lower() not in {k.lower() for k in sym}:
            cls = result_type_name if result_type_name else "SimpleNamespace"
            self.emit(f"{result_name} = {cls}()")
        self._decl_types = {k.lower(): v["ftype"] for k, v in sym.items()}
        self._decl_array_types = {k.lower(): v["ftype"] for k, v in sym.items() if v.get("is_array")}
        self._decl_types = {k.lower(): v["ftype"] for k, v in sym.items()}
        self._decl_array_types = {k.lower(): v["ftype"] for k, v in sym.items() if v.get("is_array")}

        # exec pass
        prev_result_name = self._current_result_name
        self._current_result_name = result_name
        for idx, (code, comment) in enumerate(body_lines):
            s = code.strip()
            if idx < lead_idx and not s and comment.strip():
                continue
            self.emit_comment(comment)
            if not s:
                continue
            if parse_decl(s):
                continue
            self.handle_exec_line(s, arrays_1d)

        # basic default return (safe)
        self.emit(f"return {result_name}")
        self._current_result_name = prev_result_name
        self.indent = max(0, self.indent - 1)
        self.emit("")

    def transpile_program_body(self, body_lines: list[tuple[str, str]]) -> None:
        # gather decls and arrays
        sym: dict[str, dict] = {}
        arrays_1d: set[str] = set()

        for code, _comment in body_lines:
            s = code.strip()
            if not s:
                continue
            pd = parse_decl(s)
            if not pd:
                continue
            ftype, attrs, rest = pd
            attrs_l = attrs.lower()
            if "parameter" in attrs_l:
                continue
            items = parse_decl_items(rest)
            for name, shape, init in items:
                is_array = shape is not None
                is_alloc = "allocatable" in attrs_l
                sym[name] = {
                    "ftype": ftype,
                    "is_array": is_array,
                    "shape": shape,
                    "init": init,
                    "alloc": is_alloc,
                    "attrs_l": attrs_l,
                }
                if is_array:
                    arrays_1d.add(name)

        # parameters
        parameter_names: set[str] = set()
        for code, _comment in body_lines:
            s = code.strip()
            if not s:
                continue
            pd = parse_decl(s)
            if not pd:
                continue
            ftype, attrs, rest = pd
            attrs_l = attrs.lower()
            if "parameter" in attrs_l:
                parameter_names |= self.emit_parameters_from_decl(ftype, attrs_l, rest, arrays_1d)

        # declare/init variables (explicit-shape arrays + scalars)
        self.emit_var_inits_from_sym(sym, arrays_1d, parameter_names)
        self._decl_types = {k.lower(): v["ftype"] for k, v in sym.items()}
        self._decl_array_types = {k.lower(): v["ftype"] for k, v in sym.items() if v.get("is_array")}

        # exec statements
        for code, comment in body_lines:
            s = code.strip()
            self.emit_comment(comment)
            if not s:
                continue
            if parse_decl(s):
                continue
            self.handle_exec_line(s, arrays_1d)

    def transpile_program(self, body_lines: list[tuple[str, str]]) -> None:
        # Handle internal procedures after "contains".
        main_lines = body_lines
        tail: list[tuple[str, str]] = []
        contains_idx = None
        for i, (code, _comment) in enumerate(body_lines):
            if code.strip().lower() == "contains":
                contains_idx = i
                break
        if contains_idx is not None:
            main_lines = body_lines[:contains_idx]
            tail = body_lines[contains_idx + 1 :]

        # Extract derived types declared in the program declarative part and
        # emit them as top-level dataclasses before internal procedures that may
        # reference those types in annotations.
        filtered_main: list[tuple[str, str]] = []
        i = 0
        nmain = len(main_lines)
        while i < nmain:
            line = main_lines[i][0].strip()
            if re.match(r"^type\b", line, re.I) and not re.match(r"^type\s*\(", line, re.I):
                header = line
                tbody: list[tuple[str, str]] = []
                i += 1
                while i < nmain and not re.match(r"\s*end\s+type\b", main_lines[i][0], re.I):
                    tbody.append(main_lines[i])
                    i += 1
                if i < nmain:
                    i += 1
                self.transpile_derived_type(header, tbody)
                continue
            filtered_main.append(main_lines[i])
            i += 1
        main_lines = filtered_main

        if contains_idx is not None:
            i = 0
            n = len(tail)
            while i < n:
                line = tail[i][0].strip()
                if not line:
                    i += 1
                    continue
                if re.match(r"^(?!\s*end\s+function\b)\s*(?:(?:pure\s+)?\w+(?:\s*\([^)]*\))?\s+)*function\b", line, re.I):
                    header = line
                    fbody: list[tuple[str, str]] = []
                    i += 1
                    while i < n and not re.match(r"\s*end\s+function\b", tail[i][0], re.I):
                        fbody.append(tail[i])
                        i += 1
                    if i < n:
                        i += 1
                    self.transpile_function(header, fbody)
                    continue
                if re.match(r"^(?!\s*end\s+subroutine\b)\s*(?:pure\s+)?subroutine\b", line, re.I):
                    header = line
                    sbody: list[tuple[str, str]] = []
                    i += 1
                    while i < n and not re.match(r"\s*end\s+subroutine\b", tail[i][0], re.I):
                        sbody.append(tail[i])
                        i += 1
                    if i < n:
                        i += 1
                    self.transpile_subroutine(header, sbody)
                    continue
                i += 1

        self.emit("def main() -> None:")
        self.indent += 1
        self.transpile_program_body(main_lines)
        self.indent = max(0, self.indent - 1)
        self.emit("")

    def transpile_subroutine(self, header: str, body_lines: list[tuple[str, str]]) -> None:
        hdr = header.strip()
        m = re.match(r"(?:pure\s+)?subroutine\s+(\w+)\s*\(\s*([^\)]*)\s*\)", hdr, re.I)
        if not m:
            return
        sname = m.group(1)
        args = [a.strip() for a in m.group(2).split(",") if a.strip()]

        self._validate_unit_symbols("subroutine", sname, args, body_lines)

        sym: dict[str, dict] = {}
        arrays_1d: set[str] = set()
        arg_hints: dict[str, str] = {}

        for code, _comment in body_lines:
            s = code.strip()
            pd = parse_decl(s)
            if pd:
                ftype, attrs, rest = pd
                attrs_l = attrs.lower()
                items = parse_decl_items(rest)
                type_name = None
            else:
                td = re.match(r"^type\s*\(\s*([a-z_]\w*)\s*\)\s*(.*?)::\s*(.*)$", s, re.I)
                if not td:
                    continue
                ftype = "type"
                attrs_l = td.group(2).strip().lower()
                items = parse_decl_items(td.group(3).strip())
                type_name = td.group(1)
            for name, shape, init in items:
                is_array = shape is not None
                is_alloc = "allocatable" in attrs_l
                sym[name] = {
                    "ftype": ftype,
                    "type_name": type_name,
                    "is_array": is_array,
                    "shape": shape,
                    "init": init,
                    "alloc": is_alloc,
                    "attrs_l": attrs_l,
                }
                if is_array:
                    arrays_1d.add(name)
                if name in args:
                    if is_array:
                        arg_hints[name] = self._type_hint(ftype, type_name, is_array=True)
                    else:
                        arg_hints[name] = self._type_hint(ftype, type_name, is_array=False)

        args_annot = []
        for a in args:
            hint = arg_hints.get(a, "float")
            default_none = False
            info = sym.get(a)
            if info and "optional" in info.get("attrs_l", ""):
                default_none = True
            if default_none:
                args_annot.append(f"{a}: {hint} = None")
            else:
                args_annot.append(f"{a}: {hint}")
        out_formals: list[str] = []
        for a in args:
            info = sym.get(a)
            if not info:
                continue
            attrs_l = info.get("attrs_l", "")
            if "intent(out" in attrs_l or "intent(inout" in attrs_l:
                out_formals.append(a)
        self._subr_sigs[sname.lower()] = {"args": list(args), "out": list(out_formals)}

        self.emit(f"def {sname}({', '.join(args_annot)}):")
        self.indent += 1

        parameter_names: set[str] = set()
        for code, _comment in body_lines:
            s = code.strip()
            pd = parse_decl(s)
            if not pd:
                continue
            ftype, attrs, rest = pd
            attrs_l = attrs.lower()
            if "parameter" in attrs_l:
                parameter_names |= self.emit_parameters_from_decl(ftype, attrs_l, rest, arrays_1d)

        self.emit_var_inits_from_sym(sym, arrays_1d, parameter_names, skip_names=set(args))
        self._decl_types = {k.lower(): v["ftype"] for k, v in sym.items()}
        self._decl_array_types = {k.lower(): v["ftype"] for k, v in sym.items() if v.get("is_array")}

        for code, comment in body_lines:
            s = code.strip()
            self.emit_comment(comment)
            if not s:
                continue
            if parse_decl(s):
                continue
            self.handle_exec_line(s, arrays_1d)

        if out_formals:
            if len(out_formals) == 1:
                self.emit(f"return {out_formals[0]}")
            else:
                self.emit("return " + ", ".join(out_formals))

        self.indent = max(0, self.indent - 1)
        self.emit("")

    def transpile(self, src: str) -> str:
        raw = [split_fortran_comment(l) for l in src.splitlines()]
        raw = collapse_fortran_continuations(raw)
        self.seen_parameter = any(re.search(r"\bparameter\b", code, re.I) for code, _c in raw)

        self.out = []
        self.indent = 0
        self._code_emit_count = 0
        self._block_code_start = []

        self.emit("import numpy as np")
        self.emit("import numpy.typing as npt")
        self.emit("from dataclasses import dataclass")
        self.emit("from types import SimpleNamespace")
        if self.seen_parameter:
            self.emit("from typing import Final")
        self.emit("")
        self.emit("def _f_size(a, dim=None):")
        self.emit("    arr = np.asarray(a)")
        self.emit("    if dim is None:")
        self.emit("        return arr.size")
        self.emit("    return arr.shape[int(dim) - 1]")
        self.emit("")
        self.emit("def _f_spread(a, dim=None, ncopies=None):")
        self.emit("    arr = np.asarray(a)")
        self.emit("    d = 1 if dim is None else int(dim)")
        self.emit("    ncopy = 1 if ncopies is None else int(ncopies)")
        self.emit("    axis = d - 1")
        self.emit("    ex = np.expand_dims(arr, axis=axis)")
        self.emit("    return np.repeat(ex, ncopy, axis=axis)")
        self.emit("")
        self.emit("def _f_assign_array(lhs, rhs):")
        self.emit("    arr = np.array(rhs, copy=True)")
        self.emit("    if lhs is None:")
        self.emit("        return arr")
        self.emit("    lhs_arr = np.asarray(lhs)")
        self.emit("    if arr.ndim == 0 and lhs_arr.ndim > 0:")
        self.emit("        lhs_arr[...] = arr.item()")
        self.emit("        return lhs")
        self.emit("    if lhs_arr.shape != arr.shape or lhs_arr.dtype != arr.dtype:")
        self.emit("        return arr")
        self.emit("    lhs_arr[...] = arr")
        self.emit("    return lhs")
        self.emit("")
        self.emit("def mean_1d(x):")
        self.emit("    a = np.asarray(x, dtype=np.float64)")
        self.emit("    return np.float64(np.mean(a)) if a.size else np.float64(0.0)")
        self.emit("")
        self.emit("def var_1d(x):")
        self.emit("    a = np.asarray(x, dtype=np.float64)")
        self.emit("    return np.float64(np.var(a, ddof=1)) if a.size > 1 else np.float64(0.0)")
        self.emit("")
        self.emit("def argsort_real(x):")
        self.emit("    return np.argsort(np.asarray(x, dtype=np.float64))")
        self.emit("")
        self.emit("def random_normal_vec(x):")
        self.emit("    return _f_assign_array(x, np.random.normal(size=np.asarray(x).shape))")
        self.emit("")
        self.emit("def random_choice2(weights, n, z=None):")
        self.emit("    p = np.asarray(weights, dtype=np.float64)")
        self.emit("    p = p / np.sum(p)")
        self.emit("    out = np.random.choice(np.arange(p.size), size=int(n), p=p)")
        self.emit("    return _f_assign_array(z, out)")
        self.emit("")
        self.emit("def random_choice_prob(weights, n, z=None):")
        self.emit("    return random_choice2(weights, n, z)")
        self.emit("")
        self.emit("def random_choice_norep(n, k, out=None):")
        self.emit("    vals = np.random.choice(np.arange(int(n)), size=int(k), replace=False)")
        self.emit("    return _f_assign_array(out, vals)")
        self.emit("")

        i = 0
        n = len(raw)

        in_module = False
        found_program = False
        loose_main: list[tuple[str, str]] = []

        while i < n:
            line = raw[i][0].strip()
            if not line:
                self.emit_comment(raw[i][1])
                i += 1
                continue

            if re.match(r"module\b", line, re.I):
                in_module = True
                i += 1
                continue

            if re.match(r"end\s+module\b", line, re.I):
                in_module = False
                i += 1
                continue

            if re.match(r"^type\b", line, re.I) and not re.match(r"^type\s*\(", line, re.I):
                header = line
                tbody: list[tuple[str, str]] = []
                i += 1
                while i < n and not re.match(r"\s*end\s+type\b", raw[i][0], re.I):
                    tbody.append(raw[i])
                    i += 1
                if i < n:
                    i += 1
                self.transpile_derived_type(header, tbody)
                continue

            if re.match(r"^(?!\s*end\s+function\b)\s*(?:(?:pure\s+)?\w+(?:\s*\([^)]*\))?\s+)*function\b", line, re.I):
                header = line
                body: list[tuple[str, str]] = []
                i += 1
                while i < n and not re.match(r"\s*end\s+function\b", raw[i][0], re.I):
                    body.append(raw[i])
                    i += 1
                if i < n:
                    i += 1
                self.transpile_function(header, body)
                continue

            if re.match(r"^(?!\s*end\s+subroutine\b)\s*(?:pure\s+)?subroutine\b", line, re.I):
                header = line
                body: list[tuple[str, str]] = []
                i += 1
                while i < n and not re.match(r"\s*end\s+subroutine\b", raw[i][0], re.I):
                    body.append(raw[i])
                    i += 1
                if i < n:
                    i += 1
                self.transpile_subroutine(header, body)
                continue

            if re.match(r"program\b", line, re.I):
                found_program = True
                body: list[tuple[str, str]] = []
                i += 1
                while i < n and not re.match(r"\s*end\s+program\b", raw[i][0], re.I):
                    body.append(raw[i])
                    i += 1
                if i < n:
                    i += 1
                self.transpile_program(body)
                continue

            if not in_module:
                loose_main.append(raw[i])

            i += 1

        # unnamed main program (no "program" statement)
        if not found_program:
            if any(l.strip() for l in loose_main):
                self.transpile_program(loose_main)

        lines = "\n".join(self.out).rstrip().splitlines()
        lines = self._drop_unused_runtime(lines)
        out_lines: list[str] = []
        i = 0
        while i < len(lines):
            if lines[i].strip() == 'if __name__ == "__main__":':
                if i + 1 < len(lines) and lines[i + 1].strip() == "main()":
                    i += 2
                    continue
            out_lines.append(lines[i])
            i += 1
        has_main = any(ln.startswith("def main(") for ln in out_lines)
        if has_main:
            if out_lines and out_lines[-1].strip() != "":
                out_lines.append("")
            out_lines.append('if __name__ == "__main__":')
            out_lines.append("    main()")
        return "\n".join(out_lines).rstrip() + "\n"


def main() -> int:
    ap = argparse.ArgumentParser(description="Partial Fortran-to-Python transpiler")
    ap.add_argument("input_f90", nargs="+", help="input .f90 source file(s)")
    ap.add_argument("--mode-program", action="store_true", help="treat all input files as one program (default)")
    ap.add_argument("--mode-each", action="store_true", help="transpile each input file independently")
    ap.add_argument("--out", help="output .py path (single target; valid for program mode or one-file each mode)")
    ap.add_argument("--out-dir", help="output directory for --mode-each (default: source file directories)")
    ap.add_argument("--run", action="store_true", help="run translated Python script after writing it")
    ap.add_argument("--compile", action="store_true", help="compile original Fortran source")
    ap.add_argument("--run-both", action="store_true", help="run original Fortran and translated Python (no timing)")
    ap.add_argument("--run-diff", action="store_true", help="run Fortran and Python and compare outputs")
    ap.add_argument("--tee", action="store_true", help="print transformed output (run output, or transpiled source when not running)")
    ap.add_argument("--tee-both", action="store_true", help="print both original and transformed outputs (run output, or source when not running)")
    ap.add_argument("--time", action="store_true", help="time transpile/compile/run stages (implies --run)")
    ap.add_argument("--time-both", action="store_true", help="time both original Fortran and translated Python (implies --run-both)")
    ap.add_argument(
        "--compiler",
        default="gfortran -O3 -march=native -flto",
        help='compiler command, e.g. "gfortran -O2 -Wall"',
    )
    args = ap.parse_args()

    if args.time_both:
        args.run_diff = True
    if args.run_diff:
        args.run_both = True
    if args.run_both:
        args.run = True
        args.compile = True
    if args.time_both:
        args.time = True
    if args.time:
        args.run = True
    if args.tee_both:
        args.tee = True

    show_fortran_output = bool(args.tee_both)
    show_python_output = bool(args.tee or args.tee_both)

    if args.mode_program and args.mode_each:
        print("Transpile: FAIL (choose at most one of --mode-program and --mode-each)")
        return 1
    mode_each = bool(args.mode_each)

    in_paths = [Path(p) for p in args.input_f90]
    for p in in_paths:
        if not p.exists():
            print(f"Missing file: {p}")
            return 1
    if mode_each and args.out and len(in_paths) > 1:
        print("Transpile: FAIL (--out with --mode-each is allowed only for one input file)")
        return 1
    if (not mode_each) and len(in_paths) > 1 and args.out:
        print("Transpile: FAIL (--out is allowed only for a single input in --mode-program)")
        return 1

    def _norm(s: str):
        lines = s.replace("\r\n", "\n").replace("\r", "\n").split("\n")
        lines = [" ".join(ln.split()) for ln in lines]
        while lines and lines[-1] == "":
            lines.pop()
        return lines

    def process_one(src_paths: list[Path], out_path: Path) -> int:
        timings = {}
        ft_run = None
        ft_exe = out_path.with_suffix(".orig.exe")
        compiler_parts = shlex.split(args.compiler)

        if args.compile or args.run_both:
            if args.time:
                if len(compiler_parts) > 1:
                    print("Compile options:", " ".join(compiler_parts[1:]))
                else:
                    print("Compile options: <none>")
            build_cmd = compiler_parts + [str(p) for p in src_paths] + ["-o", str(ft_exe)]
            print("Build (original-fortran):", " ".join(build_cmd))
            t0_build = time.perf_counter()
            cp = subprocess.run(build_cmd, text=True, capture_output=True)
            timings["compile"] = time.perf_counter() - t0_build
            if cp.returncode != 0:
                print(f"Build (original-fortran): FAIL (exit {cp.returncode})")
                if cp.stdout.strip():
                    print(cp.stdout.rstrip())
                if cp.stderr.strip():
                    print(cp.stderr.rstrip())
                return cp.returncode
            print("Build (original-fortran): PASS")

        if args.run_both:
            t0_ft = time.perf_counter()
            ft_run = subprocess.run([str(ft_exe)], text=True, capture_output=True)
            timings["fortran_run"] = time.perf_counter() - t0_ft
            if ft_run.returncode != 0:
                print(f"Run (original-fortran): FAIL (exit {ft_run.returncode})")
                if ft_run.stdout.strip():
                    print(ft_run.stdout.rstrip())
                if ft_run.stderr.strip():
                    print(ft_run.stderr.rstrip())
                return ft_run.returncode
            print("Run (original-fortran): PASS")
            if show_fortran_output and ft_run.stdout.strip():
                print(ft_run.stdout.rstrip())
            if show_fortran_output and ft_run.stderr.strip():
                print(ft_run.stderr.rstrip())

        t0_transpile = time.perf_counter()
        src = "\n\n".join(p.read_text(encoding="utf-8") for p in src_paths)
        t = basic_f2p()
        try:
            py = t.transpile(src)
        except ValueError as e:
            print(f"Transpile: FAIL ({e})")
            return 1
        stamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        src_tag = ", ".join(p.name for p in src_paths)
        py = f"# transpiled by xf2p.py from {src_tag} on {stamp}\n" + py
    out_path.write_text(py, encoding="utf-8")
    if not args.run:
        if args.tee_both:
            try:
                src_text = src_paths[0].read_text(encoding="utf-8") if len(src_paths) == 1 else "\n\n".join(
                    p.read_text(encoding="utf-8") for p in src_paths
                )
                src_tag = src_paths[0].name if len(src_paths) == 1 else ", ".join(p.name for p in src_paths)
                print(f"--- original: {src_tag} ---")
                print(src_text.rstrip())
            except OSError:
                pass
        if args.tee:
            try:
                out_text = out_path.read_text(encoding="utf-8")
                print(f"--- transpiled: {out_path} ---")
                print(out_text.rstrip())
            except OSError:
                pass
        timings["transpile"] = time.perf_counter() - t0_transpile

        if args.run:
            cmd = [sys.executable, str(out_path)]
            t0_py = time.perf_counter()
            rp = subprocess.run(cmd, text=True, capture_output=True)
            timings["python_run"] = time.perf_counter() - t0_py
            if rp.returncode != 0:
                if "SyntaxError" in (rp.stderr or ""):
                    print(f"Run: FAIL (translated Python is invalid: syntax error in {out_path})")
                else:
                    print(f"Run: FAIL (exit {rp.returncode})")
                if rp.stdout.strip():
                    print(rp.stdout.rstrip())
                if rp.stderr.strip():
                    print(rp.stderr.rstrip())
                return rp.returncode
            if args.run_both:
                print("Run (translated-python): PASS")
            if show_python_output and rp.stdout.strip():
                print(rp.stdout.rstrip())
            if show_python_output and rp.stderr.strip():
                print(rp.stderr.rstrip())

            if args.run_diff and ft_run is not None:
                ft_lines = _norm((ft_run.stdout or "") + (("\n" + ft_run.stderr) if ft_run.stderr else ""))
                py_lines = _norm((rp.stdout or "") + (("\n" + rp.stderr) if rp.stderr else ""))
                if ft_lines == py_lines:
                    print("Run diff: MATCH")
                else:
                    print("Run diff: DIFF")
                    first = None
                    nmin = min(len(ft_lines), len(py_lines))
                    for i in range(nmin):
                        if ft_lines[i] != py_lines[i]:
                            first = i
                            break
                    if first is None:
                        first = nmin
                    print(f"  first mismatch line: {first + 1}")
                    if first < len(ft_lines):
                        print(f"  fortran: {ft_lines[first]}")
                    else:
                        print("  fortran: <no line>")
                    if first < len(py_lines):
                        print(f"  python : {py_lines[first]}")
                    else:
                        print("  python : <no line>")
                    for dl in difflib.unified_diff(ft_lines, py_lines, fromfile="fortran", tofile="python", n=1):
                        print(dl)
                        if dl.startswith("@@"):
                            break

        if args.time:
            fortran_total = timings.get("compile", 0.0) + timings.get("fortran_run", 0.0)
            print("")
            print("Timing summary (seconds):")
            base = timings.get("python_run", 0.0)

            def _ratio(v):
                if base > 0.0:
                    return f"{(v / base):.6f}"
                return "n/a"

            rows = []
            rows.append(("transpile", timings.get("transpile", 0.0)))
            if "python_run" in timings:
                rows.append(("python run", timings["python_run"]))
            if "compile" in timings:
                rows.append(("compile", timings["compile"]))
            if "fortran_run" in timings:
                rows.append(("fortran run", timings["fortran_run"]))
            if "compile" in timings or "fortran_run" in timings:
                rows.append(("fortran total", fortran_total))

            print("  stage            seconds    ratio(vs python run)")
            for name, val in rows:
                print(f"  {name:<14} {val:>8.6f}    {_ratio(val)}")
        return 0

    if not mode_each:
        if len(in_paths) == 1:
            out_path = Path(args.out) if args.out else in_paths[-1].with_name(f"{in_paths[-1].stem}_f.py")
            return process_one(in_paths, out_path)

        # Multi-file program mode: emit one Python module per Fortran input file.
        # This preserves separate namespaces and wires USE-based imports.
        timings: dict[str, float] = {}
        ft_run = None
        compiler_parts = shlex.split(args.compiler)
        if args.out_dir:
            ft_exe = Path(args.out_dir) / f"{in_paths[-1].stem}.orig.exe"
        else:
            ft_exe = in_paths[-1].with_suffix(".orig.exe")
        ft_exe.parent.mkdir(parents=True, exist_ok=True)

        if args.compile or args.run_both:
            if args.time:
                if len(compiler_parts) > 1:
                    print("Compile options:", " ".join(compiler_parts[1:]))
                else:
                    print("Compile options: <none>")
            build_cmd = compiler_parts + [str(p) for p in in_paths] + ["-o", str(ft_exe)]
            print("Build (original-fortran):", " ".join(build_cmd))
            t0_build = time.perf_counter()
            cp = subprocess.run(build_cmd, text=True, capture_output=True)
            timings["compile"] = time.perf_counter() - t0_build
            if cp.returncode != 0:
                print(f"Build (original-fortran): FAIL (exit {cp.returncode})")
                if cp.stdout.strip():
                    print(cp.stdout.rstrip())
                if cp.stderr.strip():
                    print(cp.stderr.rstrip())
                return cp.returncode
            print("Build (original-fortran): PASS")

        if args.run_both:
            t0_ft = time.perf_counter()
            ft_run = subprocess.run([str(ft_exe)], text=True, capture_output=True)
            timings["fortran_run"] = time.perf_counter() - t0_ft
            if ft_run.returncode != 0:
                print(f"Run (original-fortran): FAIL (exit {ft_run.returncode})")
                if ft_run.stdout.strip():
                    print(ft_run.stdout.rstrip())
                if ft_run.stderr.strip():
                    print(ft_run.stderr.rstrip())
                return ft_run.returncode
            print("Run (original-fortran): PASS")
            if show_fortran_output and ft_run.stdout.strip():
                print(ft_run.stdout.rstrip())
            if show_fortran_output and ft_run.stderr.strip():
                print(ft_run.stderr.rstrip())

        t0_transpile = time.perf_counter()
        file_src: dict[Path, str] = {p: p.read_text(encoding="utf-8") for p in in_paths}
        file_mods: dict[Path, list[str]] = {}
        file_defs: dict[Path, list[str]] = {}
        file_uses: dict[Path, list[UseSpec]] = {}
        for p in in_paths:
            mods, defs, uses = _parse_file_interface(file_src[p])
            file_mods[p] = mods
            file_defs[p] = defs
            file_uses[p] = uses

        module_provider: dict[str, Path] = {}
        for p in in_paths:
            for m in file_mods[p]:
                module_provider[m.lower()] = p

        generated: dict[Path, Path] = {}
        rc = 0
        for p in in_paths:
            out_path = (Path(args.out_dir) / f"{p.stem}_f.py") if args.out_dir else p.with_name(f"{p.stem}_f.py")
            out_path.parent.mkdir(parents=True, exist_ok=True)
            print(f"[mode-program] {p} -> {out_path}")
            src = file_src[p]
            t = basic_f2p()
            try:
                py = t.transpile(src)
            except ValueError as e:
                print(f"Transpile: FAIL ({e})")
                return 1
            stamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
            py = f"# transpiled by xf2p.py from {p.name} on {stamp}\n" + py
            out_path.write_text(py, encoding="utf-8")
            if not args.run:
                if args.tee_both:
                    print(f"--- original: {p} ---")
                    print(src.rstrip())
                if args.tee:
                    print(f"--- transpiled: {out_path} ---")
                    print(py.rstrip())
            generated[p] = out_path

        # Add inter-file imports according to USE dependencies.
        for p in in_paths:
            out_path = generated[p]
            local_defs = {x.lower() for x in file_defs[p]}
            imports: list[str] = []
            for us in file_uses[p]:
                if us.intrinsic:
                    continue
                provider = module_provider.get(us.module.lower())
                if provider is None or provider == p:
                    continue
                mod_py = generated[provider].stem
                names: list[str]
                if us.only_items is not None:
                    names = [
                        nm
                        for nm in us.only_items
                        if nm.lower() not in local_defs and nm.lower() not in _LOCAL_RUNTIME_HELPERS
                    ]
                else:
                    # import all known symbols from provider, excluding local collisions
                    names = [
                        nm
                        for nm in file_defs[provider]
                        if nm.lower() not in local_defs and nm.lower() not in _LOCAL_RUNTIME_HELPERS
                    ]
                names = unique_preserve(names)
                if not names:
                    continue
                imports.append(f"from {mod_py} import {', '.join(names)}")
            if imports:
                py = out_path.read_text(encoding="utf-8")
                py = _insert_imports(py, unique_preserve(imports))
                out_path.write_text(py, encoding="utf-8")
        timings["transpile"] = time.perf_counter() - t0_transpile

        if args.run:
            main_out = generated[in_paths[-1]]
            cmd = [sys.executable, str(main_out)]
            t0_py = time.perf_counter()
            rp = subprocess.run(cmd, text=True, capture_output=True)
            timings["python_run"] = time.perf_counter() - t0_py
            if rp.returncode != 0:
                if "SyntaxError" in (rp.stderr or ""):
                    print(f"Run: FAIL (translated Python is invalid: syntax error in {main_out})")
                else:
                    print(f"Run: FAIL (exit {rp.returncode})")
                if rp.stdout.strip():
                    print(rp.stdout.rstrip())
                if rp.stderr.strip():
                    print(rp.stderr.rstrip())
                return rp.returncode
            if args.run_both:
                print("Run (translated-python): PASS")
            else:
                print("Run: PASS")
            if show_python_output and rp.stdout.strip():
                print(rp.stdout.rstrip())
            if show_python_output and rp.stderr.strip():
                print(rp.stderr.rstrip())

            if args.run_diff and ft_run is not None:
                ft_lines = _norm((ft_run.stdout or "") + (("\n" + ft_run.stderr) if ft_run.stderr else ""))
                py_lines = _norm((rp.stdout or "") + (("\n" + rp.stderr) if rp.stderr else ""))
                if ft_lines == py_lines:
                    print("Run diff: MATCH")
                else:
                    print("Run diff: DIFF")
                    first = None
                    nmin = min(len(ft_lines), len(py_lines))
                    for i in range(nmin):
                        if ft_lines[i] != py_lines[i]:
                            first = i
                            break
                    if first is None:
                        first = nmin
                    print(f"  first mismatch line: {first + 1}")
                    if first < len(ft_lines):
                        print(f"  fortran: {ft_lines[first]}")
                    else:
                        print("  fortran: <no line>")
                    if first < len(py_lines):
                        print(f"  python : {py_lines[first]}")
                    else:
                        print("  python : <no line>")
                    for dl in difflib.unified_diff(ft_lines, py_lines, fromfile="fortran", tofile="python", n=1):
                        print(dl)
                        if dl.startswith("@@"):
                            break

        if args.time:
            fortran_total = timings.get("compile", 0.0) + timings.get("fortran_run", 0.0)
            print("")
            print("Timing summary (seconds):")
            base = timings.get("python_run", 0.0)

            def _ratio(v):
                if base > 0.0:
                    return f"{(v / base):.6f}"
                return "n/a"

            rows = []
            rows.append(("transpile", timings.get("transpile", 0.0)))
            if "python_run" in timings:
                rows.append(("python run", timings["python_run"]))
            if "compile" in timings:
                rows.append(("compile", timings["compile"]))
            if "fortran_run" in timings:
                rows.append(("fortran run", timings["fortran_run"]))
            if "compile" in timings or "fortran_run" in timings:
                rows.append(("fortran total", fortran_total))

            print("  stage            seconds    ratio(vs python run)")
            for name, val in rows:
                print(f"  {name:<14} {val:>8.6f}    {_ratio(val)}")
        return rc

    rc = 0
    for p in in_paths:
        print(f"[mode-each] {p}")
        if args.out:
            out_path = Path(args.out)
        elif args.out_dir:
            out_path = Path(args.out_dir) / f"{p.stem}_f.py"
        else:
            out_path = p.with_name(f"{p.stem}_f.py")
        out_path.parent.mkdir(parents=True, exist_ok=True)
        rc = process_one([p], out_path)
        if rc != 0:
            break
    return rc


if __name__ == "__main__":
    raise SystemExit(main())
