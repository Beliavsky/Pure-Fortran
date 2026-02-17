#!/usr/bin/env python3
"""Shared lightweight Fortran semantic helpers used across x*.py tools."""

from __future__ import annotations

import re
from typing import Callable, Iterable, Optional

_OBVIOUS_IMPURE_RE = re.compile(
    r"\b(call|print|read|write|open|close|backspace|rewind|endfile|flush|wait|stop|error\s+stop|pause|allocate|deallocate)\b",
    re.IGNORECASE,
)
_STOP_LIKE_RE = re.compile(r"\bstop\b", re.IGNORECASE)
_ERROR_STOP_RE = re.compile(r"\berror\s+stop\b", re.IGNORECASE)


def infer_intent_from_access(is_read: bool, is_written: bool, first_access: Optional[str] = None) -> str:
    """
    Infer a base INTENT class from local access profile.

    Returns one of: "in", "out", "inout".
    """
    first = (first_access or "").strip().lower()
    if first.startswith("read"):
        first = "r"
    elif first.startswith("write"):
        first = "w"
    if not is_written:
        return "in"
    if not is_read:
        return "out"
    if first == "w":
        return "out"
    return "inout"


def statement_has_obvious_impurity(code_lower: str) -> bool:
    """Return True for statements that are obvious blockers for PURE extraction."""
    return _OBVIOUS_IMPURE_RE.search(code_lower) is not None


def has_stop_without_error_stop(code_lower: str) -> bool:
    """
    Return True when STOP appears and is not part of ERROR STOP.
    """
    return _STOP_LIKE_RE.search(_ERROR_STOP_RE.sub("", code_lower)) is not None


def is_pure_eligible_block(
    lines: Iterable[str],
    strip_comment: Optional[Callable[[str], str]] = None,
) -> bool:
    """Conservative pure-eligibility check over a statement block."""
    strip_fn = strip_comment if strip_comment is not None else (lambda s: s)
    for ln in lines:
        code = strip_fn(ln).strip().lower()
        if not code:
            continue
        if statement_has_obvious_impurity(code):
            return False
    return True

