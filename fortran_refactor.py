#!/usr/bin/env python3
"""Shared Fortran refactoring helpers."""

from __future__ import annotations

import re
from typing import Dict, List, Optional, Set, Tuple

import fortran_scan as fscan
import xunused


def _line_is_decl(line: str) -> bool:
    code = fscan.strip_comment(line).strip()
    if not code:
        return False
    return "::" in code and bool(
        re.match(
            r"^(integer|real|logical|character|complex|type\s*\(|class\s*\(|procedure\b)",
            code,
            re.IGNORECASE,
        )
    )


def _extract_main_decl_map(main_lines: List[str]) -> Tuple[Dict[str, str], Dict[str, Set[str]]]:
    decl_map: Dict[str, str] = {}
    decl_names_map: Dict[str, Set[str]] = {}
    spec_done = False
    for ln in main_lines:
        code = fscan.strip_comment(ln).strip()
        if not code:
            continue
        if code.lower().startswith("program "):
            continue
        if not spec_done and code.lower().startswith("use "):
            continue
        if not spec_done and code.lower().startswith("implicit "):
            continue
        if not spec_done and _line_is_decl(ln):
            names = fscan.parse_declared_names_from_decl(code)
            if not names:
                continue
            decl_names_map[ln] = names
            for n in names:
                decl_map[n] = ln
            continue
        if not spec_done:
            spec_done = True
        if spec_done:
            break
    return decl_map, decl_names_map


def _single_entity_decl_with_intent(line: str, keep_name: str) -> Optional[str]:
    code, comment = xunused.split_code_comment(line.rstrip("\r\n"))
    names = fscan.parse_declared_names_from_decl(code)
    if keep_name.lower() not in names:
        return None
    remove = set(n for n in names if n != keep_name.lower())
    keep_line, _ = xunused.rewrite_decl_remove_names(line, remove)
    if keep_line is None:
        return None
    code2, comment2 = xunused.split_code_comment(keep_line.rstrip("\r\n"))
    if "::" not in code2:
        return None
    lhs, rhs = code2.split("::", 1)
    if "parameter" in lhs.lower():
        return None
    if "intent(" not in lhs.lower():
        lhs = f"{lhs.rstrip()}, intent(inout) "
    return f"{lhs}::{rhs}{comment2}\n"


def _single_entity_decl_with_specific_intent(line: str, keep_name: str, intent: str) -> Optional[str]:
    base = _single_entity_decl_with_intent(line, keep_name)
    if base is None:
        return None
    code, comment = xunused.split_code_comment(base.rstrip("\r\n"))
    if "::" not in code:
        return base
    lhs, rhs = code.split("::", 1)
    # Replace any existing intent(...) with requested one.
    lhs = re.sub(r"\s*,\s*intent\s*\(\s*[a-z]+\s*\)", "", lhs, flags=re.IGNORECASE)
    lhs = f"{lhs.rstrip()}, intent({intent}) "
    return f"{lhs}::{rhs}{comment}\n"


def _decl_is_allocatable(line: str) -> bool:
    code = fscan.strip_comment(line).lower()
    if "::" not in code:
        return False
    lhs = code.split("::", 1)[0]
    return "allocatable" in lhs


def _collect_idents(lines: List[str]) -> List[str]:
    out: List[str] = []
    ident_re = re.compile(r"\b[a-z][a-z0-9_]*\b", re.IGNORECASE)
    for ln in lines:
        code = fscan.strip_comment(ln)
        for m in ident_re.finditer(code):
            out.append(m.group(0).lower())
    return out


def _infer_intent_from_body(arg_name: str, body_lines: List[str]) -> str:
    """Infer conservative dummy INTENT from simple body usage."""
    n = re.escape(arg_name.lower())
    pat_tok = re.compile(rf"\b{n}\b", re.IGNORECASE)
    pat_lhs_assign = re.compile(rf"^\s*{n}(?:\b|\s*\()", re.IGNORECASE)
    pat_read_stmt = re.compile(r"^\s*read\s*\(", re.IGNORECASE)
    pat_call_stmt = re.compile(r"^\s*call\s+[a-z][a-z0-9_]*\s*\(", re.IGNORECASE)

    has_read = False
    has_write = False

    for ln in body_lines:
        code, _comment = xunused.split_code_comment(ln.rstrip("\r\n"))
        low = code.lower()
        if not low.strip():
            continue
        if not pat_tok.search(low):
            continue

        # READ unit is input-use; READ item list is output-use.
        if pat_read_stmt.match(low):
            mclose = low.find(")")
            items = low[mclose + 1 :] if mclose >= 0 else ""
            if pat_tok.search(items):
                has_write = True
            else:
                has_read = True
            continue

        # Assignment LHS -> write; any RHS/reference -> read.
        if "=" in low:
            lhs, rhs = low.split("=", 1)
            if pat_lhs_assign.match(lhs.strip()):
                has_write = True
                if pat_tok.search(rhs):
                    has_read = True
                continue

        # Unknown CALL side effects: be conservative if passed as actual arg.
        if pat_call_stmt.match(low):
            has_read = True
            has_write = True
            continue

        has_read = True

    if has_write and not has_read:
        return "out"
    if has_write and has_read:
        return "inout"
    return "in"


def _first_use_is_write(arg_name: str, body_lines: List[str]) -> bool:
    """Return True if first semantic use of arg in body is a write context."""
    n = re.escape(arg_name.lower())
    pat_tok = re.compile(rf"\b{n}\b", re.IGNORECASE)
    pat_lhs_assign = re.compile(rf"^\s*{n}(?:\b|\s*\()", re.IGNORECASE)
    pat_read_stmt = re.compile(r"^\s*read\s*\(", re.IGNORECASE)
    for ln in body_lines:
        code, _comment = xunused.split_code_comment(ln.rstrip("\r\n"))
        low = code.lower()
        if not low.strip() or not pat_tok.search(low):
            continue
        if "=" in low:
            lhs, rhs = low.split("=", 1)
            if pat_lhs_assign.match(lhs.strip()):
                return True
            if pat_tok.search(rhs):
                return False
        if pat_read_stmt.match(low):
            mclose = low.find(")")
            items = low[mclose + 1 :] if mclose >= 0 else ""
            if pat_tok.search(items):
                return True
        return False
    return False


def _needs_input_value(arg_name: str, body_lines: List[str]) -> bool:
    """True if arg is read before being written in the extracted body."""
    n = re.escape(arg_name.lower())
    pat_tok = re.compile(rf"\b{n}\b", re.IGNORECASE)
    pat_lhs_assign = re.compile(rf"^\s*{n}(?:\b|\s*\()", re.IGNORECASE)
    pat_read_stmt = re.compile(r"^\s*read\s*\(", re.IGNORECASE)
    has_been_written = False

    def _is_only_inquiry_use(code_lower: str) -> bool:
        # Treat ALLOCATED(arg) / SIZE(arg[,...]) as inquiry-only (no caller value needed).
        stripped = re.sub(rf"\ballocated\s*\(\s*{n}\s*\)", "", code_lower, flags=re.IGNORECASE)
        stripped = re.sub(rf"\bsize\s*\(\s*{n}\s*(?:,\s*[^)]*)?\)", "", stripped, flags=re.IGNORECASE)
        return pat_tok.search(stripped) is None
    for ln in body_lines:
        code, _comment = xunused.split_code_comment(ln.rstrip("\r\n"))
        low = code.lower()
        if not low.strip() or not pat_tok.search(low):
            continue

        if _is_only_inquiry_use(low):
            continue
        if re.search(rf"\bif\s*\(\s*allocated\s*\(\s*{n}\s*\)\s*\)", low, re.IGNORECASE):
            # Guarded-by-allocated reads do not require caller-provided value.
            continue

        # move_alloc(src, arg) writes arg.
        if re.search(rf"\bcall\s+move_alloc\s*\(\s*[^,]+,\s*{n}\s*\)", low, re.IGNORECASE):
            has_been_written = True
            continue

        # READ statement: list item is write; other appearance is read.
        if pat_read_stmt.match(low):
            mclose = low.find(")")
            items = low[mclose + 1 :] if mclose >= 0 else ""
            if pat_tok.search(items):
                has_been_written = True
            else:
                if not has_been_written:
                    return True
            continue

        # Assignment statement
        if "=" in low:
            lhs, rhs = low.split("=", 1)
            lhs_has = pat_lhs_assign.match(lhs.strip()) is not None
            rhs_has = pat_tok.search(rhs) is not None
            if rhs_has and not has_been_written:
                return True
            if lhs_has:
                has_been_written = True
            continue

        # Any other use is a read.
        if not has_been_written:
            return True
    return False


def _find_preblock_simple_init(var_name: str, lines: List[str], main_start: int, block_start: int) -> Optional[str]:
    """Find nearest simple assignment `var = expr` before block start."""
    n = re.escape(var_name.lower())
    asg_re = re.compile(rf"^\s*{n}\s*=\s*(.+)$", re.IGNORECASE)
    ident_re = re.compile(r"\b[a-z][a-z0-9_]*\b", re.IGNORECASE)
    for k in range(block_start - 1, main_start, -1):
        code = fscan.strip_comment(lines[k]).strip()
        if not code:
            continue
        m = asg_re.match(code)
        if m is None:
            continue
        rhs = m.group(1).strip()
        # Conservative: only literal-like expressions.
        bad_ident = False
        for tok in ident_re.finditer(rhs):
            bad_ident = True
            break
        if bad_ident:
            return None
        return rhs
    return None


def _can_replace_post_call_count_uses(
    count_name: str,
    arr_name: str,
    lines: List[str],
    start_line: int,
    end_line: int,
) -> bool:
    """True if post-call uses of count can be safely rewritten to size(arr)."""
    n = re.escape(count_name.lower())
    tok_re = re.compile(rf"\b{n}\b", re.IGNORECASE)
    lhs_assign_re = re.compile(rf"^\s*{n}\s*=", re.IGNORECASE)
    for k in range(start_line, end_line + 1):
        code = fscan.strip_comment(lines[k]).strip()
        if not code:
            continue
        if "::" in code:
            continue
        if not tok_re.search(code):
            continue
        if lhs_assign_re.match(code):
            return False
    return True


def _rewrite_post_call_count_uses(
    count_name: str,
    arr_name: str,
    lines: List[str],
    start_line: int,
    end_line: int,
) -> None:
    n = re.escape(count_name.lower())
    tok_re = re.compile(rf"\b{n}\b", re.IGNORECASE)
    for k in range(start_line, end_line + 1):
        code, comment = xunused.split_code_comment(lines[k].rstrip("\r\n"))
        if not code.strip() or "::" in code:
            continue
        if not tok_re.search(code):
            continue
        new_code = tok_re.sub(f"size({arr_name})", code)
        eol = "\n" if lines[k].endswith("\n") else ""
        lines[k] = f"{new_code}{comment}{eol}"


def _collect_existing_proc_names(lines: List[str]) -> Set[str]:
    out: Set[str] = set()
    pat = re.compile(r"^\s*(?:pure\s+)?(?:[a-z][a-z0-9_()\s=,:]*\s+)?(?:subroutine|function)\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
    for ln in lines:
        code = fscan.strip_comment(ln).strip()
        m = pat.match(code)
        if m:
            out.add(m.group(1).lower())
    return out


def _suggest_proc_base_name(body_lines: List[str]) -> str:
    txt = "\n".join(fscan.strip_comment(ln).lower() for ln in body_lines)
    has_read = "read(" in txt
    has_write = "write(" in txt
    has_iostat = "iostat=" in txt
    has_move_alloc = "move_alloc(" in txt
    has_allocate = "allocate(" in txt
    has_min = re.search(r"\bmin\w*\b", txt) is not None
    has_max = re.search(r"\bmax\w*\b", txt) is not None
    has_sum = re.search(r"\bsum\w*\b", txt) is not None

    if has_read and has_iostat:
        return "read_values"
    if has_read:
        return "read_data"
    if has_write:
        return "write_values"
    if has_move_alloc and has_allocate:
        return "grow_buffer"
    if has_min and has_max:
        return "compute_minmax"
    if has_sum:
        return "accumulate_values"
    return "refactor_block"


def _unique_proc_name(base: str, used: Set[str]) -> str:
    name = base
    k = 2
    while name.lower() in used:
        name = f"{base}_{k}"
        k += 1
    used.add(name.lower())
    return name


def _proc_purpose_comment(base: str) -> str:
    if base == "read_values":
        return "read values from input, growing destination array as needed"
    if base == "read_data":
        return "read data from input"
    if base == "write_values":
        return "write values to output"
    if base == "grow_buffer":
        return "grow allocatable buffer while preserving existing values"
    if base == "compute_minmax":
        return "compute minimum and maximum related values"
    if base == "accumulate_values":
        return "accumulate values over the current data set"
    return "extracted block from main program"


def _call_outputs(base: str, ordered_external: List[str]) -> List[str]:
    if base in {"read_values", "read_data"}:
        # Typically `fp` is a unit handle; data outputs are the rest.
        return [a for a in ordered_external if a.lower() != "fp"]
    return list(ordered_external)


def _call_inline_comment(base: str, ordered_external: List[str]) -> str:
    purpose = _proc_purpose_comment(base)
    outs = _call_outputs(base, ordered_external)
    if outs:
        return f"{purpose}; outputs: {', '.join(outs)}"
    return purpose


def _wrap_inline_comment_lines(code_line: str, comment: str, *, indent: str = "", width: int = 80) -> List[str]:
    """Format a code line with inline comment, continuing overflow on next lines."""
    code = code_line.rstrip("\n")
    prefix = f"{code} ! "
    if len(prefix) + len(comment) <= width:
        return [f"{prefix}{comment}\n"]
    # Keep code line with as much comment text as fits.
    room_first = max(0, width - len(prefix))
    words = comment.split()
    first_words: List[str] = []
    cur = 0
    wi = 0
    while wi < len(words):
        w = words[wi]
        add = len(w) + (1 if cur > 0 else 0)
        if cur + add > room_first:
            break
        first_words.append(w)
        cur += add
        wi += 1
    first_text = " ".join(first_words)
    out = [f"{prefix}{first_text}\n"]
    rest_words = words[wi:]
    if not rest_words:
        return out
    cont_prefix = f"{indent}! "
    room = max(1, width - len(cont_prefix))
    line_words: List[str] = []
    cur = 0
    for w in rest_words:
        add = len(w) + (1 if cur > 0 else 0)
        if cur + add > room and line_words:
            out.append(f"{cont_prefix}{' '.join(line_words)}\n")
            line_words = [w]
            cur = len(w)
        else:
            line_words.append(w)
            cur += add
    if line_words:
        out.append(f"{cont_prefix}{' '.join(line_words)}\n")
    return out


def _arg_inline_comment(proc_base: str, arg_name: str) -> Optional[str]:
    n = arg_name.lower()
    if proc_base in {"read_values", "read_data"}:
        if n == "fp":
            return "I/O unit number"
        if n == "v":
            return "value read from input"
        if n == "n":
            return "number of values read"
        if n == "cap":
            return "current allocated capacity"
        if n == "new_cap":
            return "next capacity during growth"
        if n == "x":
            return "values buffer"
    return None


def _with_inline_comment(line: str, comment: Optional[str]) -> str:
    if not comment:
        return line
    code, existing = xunused.split_code_comment(line.rstrip("\r\n"))
    if existing.strip():
        return f"{code}{existing}\n"
    return f"{code} ! {comment}\n"


def refactor_long_main_blocks_to_module_subroutines(text: str, *, min_block_lines: int = 12) -> str:
    lines = text.splitlines(keepends=True)
    if not lines:
        return text
    mod_start = next((i for i, ln in enumerate(lines) if re.match(r"^\s*module\s+xc2f_mod\b", ln, re.IGNORECASE)), None)
    mod_end = next((i for i, ln in enumerate(lines) if re.match(r"^\s*end\s+module\s+xc2f_mod\b", ln, re.IGNORECASE)), None)
    main_start = next((i for i, ln in enumerate(lines) if re.match(r"^\s*program\s+main\b", ln, re.IGNORECASE)), None)
    main_end = next((i for i, ln in enumerate(lines) if re.match(r"^\s*end\s+program\s+main\b", ln, re.IGNORECASE)), None)
    if mod_start is None or mod_end is None or main_start is None or main_end is None or main_start >= main_end:
        return text

    main_lines = lines[main_start:main_end + 1]
    decl_map, _decl_names_map = _extract_main_decl_map(main_lines)
    main_decl_vars = set(decl_map.keys())
    if not main_decl_vars:
        return text

    spans: List[Tuple[int, int]] = []
    depth = 0
    span_start: Optional[int] = None
    for i in range(main_start, main_end + 1):
        code = fscan.strip_comment(lines[i]).strip().lower()
        if code == "block":
            if depth == 0:
                span_start = i
            depth += 1
            continue
        if code.startswith("end block"):
            if depth > 0:
                depth -= 1
                if depth == 0 and span_start is not None:
                    if (i - span_start + 1) >= min_block_lines:
                        spans.append((span_start, i))
                    span_start = None
            continue

    if not spans:
        return text

    generated_subs: List[str] = []
    call_replacements: Dict[int, Tuple[int, List[str]]] = {}
    proc_names: List[str] = []
    used_proc_names: Set[str] = _collect_existing_proc_names(lines)

    for _idx, (bs, be) in enumerate(spans, start=1):
        block_lines = lines[bs:be + 1]
        inner = block_lines[1:-1]
        local_decl_end = 0
        while local_decl_end < len(inner):
            code = fscan.strip_comment(inner[local_decl_end]).strip()
            if not code:
                local_decl_end += 1
                continue
            if _line_is_decl(inner[local_decl_end]):
                local_decl_end += 1
                continue
            break
        local_decl_lines = inner[:local_decl_end]
        body_lines = inner[local_decl_end:]
        local_vars: Set[str] = set()
        for ln in local_decl_lines:
            local_vars.update(fscan.parse_declared_names_from_decl(fscan.strip_comment(ln)))
        used = _collect_idents(body_lines)
        ordered_external: List[str] = []
        seen: Set[str] = set()
        for tok in used:
            if tok in main_decl_vars and tok not in local_vars and tok not in seen:
                ordered_external.append(tok)
                seen.add(tok)
        if not ordered_external:
            continue
        arg_decl_lines: List[str] = []
        local_from_args: List[str] = []
        local_init_lines: List[str] = []
        arg_init_lines: List[str] = []
        call_args: List[str] = []
        trim_array_at_end = False
        bad = False
        for n in ordered_external:
            dline = decl_map.get(n)
            if dline is None:
                bad = True
                break
            intent_n = _infer_intent_from_body(n, body_lines)
            needs_input = _needs_input_value(n, body_lines)
            if _decl_is_allocatable(dline) and not needs_input:
                # Fresh-output allocatable dummies are clearer as INTENT(OUT).
                intent_n = "out"
            # If a variable is only scratch state for the extracted block,
            # keep it local instead of passing it through the call.
            used_after_call = any(
                any(tok.group(0).lower() == n for tok in re.finditer(r"\b[a-z][a-z0-9_]*\b", fscan.strip_comment(lines[k]), re.IGNORECASE))
                for k in range(be + 1, main_end + 1)
            )
            init_expr = _find_preblock_simple_init(n, lines, main_start, bs) if needs_input else None
            if (
                n.lower() == "n"
                and "x" in ordered_external
                and used_after_call
                and (intent_n == "out" or (intent_n == "inout" and init_expr is not None))
                and _can_replace_post_call_count_uses(n, "x", lines, be + 1, main_end)
            ):
                _rewrite_post_call_count_uses(n, "x", lines, be + 1, main_end)
                used_after_call = False
                trim_array_at_end = True
            if used_after_call and intent_n == "inout" and needs_input and init_expr is not None:
                # Incoming value is just a local pre-block initializer:
                # move it into the extracted procedure and expose output-only intent.
                intent_n = "out"
                needs_input = False
            localizable = (
                not used_after_call
                and intent_n in {"inout", "out"}
                and (not needs_input or init_expr is not None)
            )
            if localizable:
                sdecl = _single_entity_decl_with_intent(dline, n)
                if sdecl is None:
                    bad = True
                    break
                s_code, s_comment = xunused.split_code_comment(sdecl.rstrip("\r\n"))
                s_code = re.sub(r"\s*,\s*intent\s*\(\s*[a-z]+\s*\)", "", s_code, flags=re.IGNORECASE)
                local_from_args.append(f"{s_code}{s_comment}\n")
                if init_expr is not None:
                    local_init_lines.append(f"{n} = {init_expr}\n")
                if n.lower() == "n" and "x" in ordered_external:
                    trim_array_at_end = True
                continue
            sdecl = _single_entity_decl_with_specific_intent(dline, n, intent_n)
            if sdecl is None:
                bad = True
                break
            arg_decl_lines.append(sdecl)
            if intent_n == "out" and init_expr is not None:
                arg_init_lines.append(f"{n} = {init_expr}\n")
            call_args.append(n)
        if bad:
            continue

        base = _suggest_proc_base_name(body_lines)
        proc = _unique_proc_name(base, used_proc_names)
        proc_names.append(proc)
        indent = re.match(r"^(\s*)", lines[bs]).group(1)
        call_replacements[bs] = (
            be,
            _wrap_inline_comment_lines(
                f"{indent}call {proc}({', '.join(call_args)})",
                _call_inline_comment(base, call_args),
                indent=indent,
                width=80,
            ),
        )

        generated_subs.append(f"subroutine {proc}({', '.join(call_args)})\n")
        generated_subs.append(f"! {_proc_purpose_comment(base)}\n")
        for n, sdecl in zip(call_args, arg_decl_lines):
            generated_subs.append(_with_inline_comment(sdecl, _arg_inline_comment(base, n)))
        generated_subs.extend(local_from_args)
        for ln in local_decl_lines:
            l2 = ln[3:] if ln.startswith("   ") else ln
            generated_subs.append(l2 if l2.endswith("\n") else f"{l2}\n")
        generated_subs.extend(arg_init_lines)
        generated_subs.extend(local_init_lines)
        for ln in body_lines:
            if ln.startswith("   "):
                generated_subs.append(ln[3:])
            else:
                generated_subs.append(ln)
        if trim_array_at_end:
            generated_subs.append("if (allocated(x)) then\n")
            generated_subs.append("   if (n < size(x)) x = x(1:n)\n")
            generated_subs.append("end if\n")
        generated_subs.append(f"end subroutine {proc}\n")
        generated_subs.append("\n")

    if not generated_subs:
        return text

    rebuilt: List[str] = []
    i = 0
    while i < len(lines):
        repl = call_replacements.get(i)
        if repl is None:
            rebuilt.append(lines[i])
            i += 1
            continue
        end_i, repl_lines = repl
        rebuilt.extend(repl_lines)
        i = end_i + 1
    lines = rebuilt

    mod_end = next((i for i, ln in enumerate(lines) if re.match(r"^\s*end\s+module\s+xc2f_mod\b", ln, re.IGNORECASE)), None)
    if mod_end is None:
        return "".join(lines)
    lines = lines[:mod_end] + generated_subs + lines[mod_end:]

    mod_start2 = next((i for i, ln in enumerate(lines) if re.match(r"^\s*module\s+xc2f_mod\b", ln, re.IGNORECASE)), None)
    mod_end2 = next((i for i, ln in enumerate(lines) if re.match(r"^\s*end\s+module\s+xc2f_mod\b", ln, re.IGNORECASE)), None)
    pub_idx = None
    if mod_start2 is not None and mod_end2 is not None:
        pub_idx = next(
            (
                i
                for i in range(mod_start2 + 1, mod_end2)
                if re.match(r"^\s*public\s*::", lines[i], re.IGNORECASE)
            ),
            None,
        )
    if pub_idx is not None:
        code, comment = xunused.split_code_comment(lines[pub_idx].rstrip("\r\n"))
        lhs, rhs = code.split("::", 1)
        names = [n.strip() for n in rhs.split(",") if n.strip()]
        for p in proc_names:
            if p not in names:
                names.append(p)
        lines[pub_idx] = f"{lhs}:: {', '.join(sorted(set(names), key=str.lower))}{comment}\n"

    for i, ln in enumerate(lines):
        if not re.match(r"^\s*use\s+xc2f_mod\s*,\s*only\s*:", ln, re.IGNORECASE):
            continue
        code, comment = xunused.split_code_comment(ln.rstrip("\r\n"))
        lhs, rhs = code.split(":", 1)
        names = [n.strip() for n in rhs.split(",") if n.strip()]
        for p in proc_names:
            if p not in names:
                names.append(p)
        lines[i] = f"{lhs}: {', '.join(sorted(set(names), key=str.lower))}{comment}\n"
        break

    return "".join(lines)
