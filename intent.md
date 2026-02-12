# xintent.py INTENT Rules

This document describes the current conservative rules used by `xintent.py` to decide when a dummy argument can be marked `intent(in)` or `intent(out)`.

## Scope

- Analysis is per procedure (subroutine/function) from `fortran_scan.py` parsing.
- Only declared dummy arguments are considered.
- If a case is ambiguous, the tool prefers no suggestion.

## Dummy eligibility prerequisites

A dummy must satisfy all of the following before intent inference:

1. It has a declaration line in the procedure body.
2. Its declaration does **not** already contain:
   - `intent(...)`, or
   - `value`, or
   - `external` (treated as procedure-like and skipped).
3. Its declaration is **not** `allocatable` or `pointer` (skipped).
4. It is not inferred to be procedure-like by usage:
   - scalar dummy used as `d(...)` in executable code is treated as probable procedure dummy and skipped.

## Read/write event detection

For each eligible dummy, `xintent.py` scans executable statements and records events:

- **Write** if dummy is on assignment LHS:
  - `d = ...`
  - `d(...) = ...`
  - `%` component LHS base is also recognized via base identifier.
- **Read** if dummy token appears on assignment RHS.
- **Write** if dummy appears as first object in `allocate(...)` or `deallocate(...)`.
- **Write** if dummy appears in a `read(...)` statement.
- **Write** for pointer assignment `d => ...`.
- **Maybe-written-via-call** if dummy appears in any `call ...` statement argument list.
- **Write** if dummy is the internal file/unit target in `write(...)`.
- **Write** if dummy is used as a `do` iterator variable.

The tool tracks each dummy's **first event** (`read`, `write`, or `call`) by source line.

Notes:

- Leading statement labels are stripped before analysis.
- One-line `if (...) stmt` and one-line `where (...) stmt` prefixes are stripped so the inner statement is analyzed.

## `intent(in)` rule

A dummy is suggested for `intent(in)` iff:

1. It passes eligibility prerequisites.
2. It has **no write events**.
3. It has **no maybe-written-via-call** event.

Equivalent intuition: appears read-only and not passed to calls in a way that might modify it.

## `intent(out)` rule (`--suggest-intent-out`)

A dummy is suggested for `intent(out)` iff:

1. It passes eligibility prerequisites.
2. It has at least one **write** event.
3. Its **first event is write** (so not read/call before first write).
4. It has no **maybe-written-via-call** event.

Equivalent intuition: value is established by the procedure before any use, and not ambiguous due to call-side effects.

## What `--fix` edits

- Only suggestions marked fixable are edited (the inferred intent suggestions are fixable; reporting from `--warn-missing-intent` is advisory).
- Declaration rewrite supports:
  - `::` declarations,
  - many no-`::` declarations,
  - multi-entity declarations,
  - semicolon-separated declaration statements,
  - continued declaration statements.
- Existing `intent(...)` or `value` declarations are not touched.

## Conservative limitations

- Token-based scans can miss deeper dataflow/aliasing semantics.
- Any dummy seen in call argument context is conservatively blocked for both `intent(in)` and `intent(out)`.
- Procedure-like ambiguities (for example scalar name called as `d(...)`) are skipped.
- Compile validation (`--compiler`) is recommended in fix workflows.


