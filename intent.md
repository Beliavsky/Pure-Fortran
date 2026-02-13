# xintent.py Intent Rules

This document describes the current conservative rules used by `xintent.py` to suggest and apply:

- `intent(in)` (default mode)
- `intent(out)` (`--suggest-intent-out`)
- `intent(in out)` (`--suggest-intent-inout`)

## Scope

- Analysis is per procedure (subroutine/function) parsed via `fortran_scan.py`.
- Only declared dummy arguments are considered.
- If a case is ambiguous, the tool prefers no suggestion.

## Dummy Eligibility Prerequisites

A dummy must satisfy all of the following:

1. It has a declaration in the procedure body.
2. Its declaration does not already contain:
   - `intent(...)`
   - `value`
   - `external` (treated as procedure-like/explicitly excluded)
3. Its declaration is not `allocatable` or `pointer`.
4. It is not inferred procedure-like by usage (for example scalar `d(...)` use in executable code).

## Event Detection

For eligible dummies, `xintent.py` scans statements and records conservative events:

- **Write**
  - assignment LHS (`d = ...`, `d(...) = ...`, component base `d%...`)
  - first object in `allocate(...)` / `deallocate(...)`
  - pointer assignment `d => ...`
  - `do d = ...` iterator
  - `read(...)` i/o list targets (including implied-do targets)
  - `read(..., iostat=..., iomsg=..., size=...)` control targets
  - `write(...)` internal-file unit target only when dummy is `character`
- **Read**
  - assignment RHS token use
  - declaration/spec-expression use (for example bounds using another dummy)
- **Maybe-written-via-call**
  - call/function-actual usage not provably `intent(in)` under interprocedural rules

The tool also tracks each dummy's first observed event (`read`, `write`, or `call`).

## `intent(in)` Rule

A dummy is suggested for `intent(in)` iff:

1. It passes eligibility.
2. It has no write events.
3. It has no maybe-written-via-call event.

## `intent(out)` Rule (`--suggest-intent-out`)

A dummy is suggested for `intent(out)` iff:

1. It passes eligibility.
2. It has at least one write event.
3. Its first event is `write`.
4. It has no maybe-written-via-call event.
5. It is not const-like initialized in declaration.
6. It is not read in declaration/spec expressions.
7. It is not a formal that is observed with non-variable actuals at call sites.
8. If it is read in executable code, there must be a clear top-level initialization write
   before the first read (for example `z = 0; z = z + ...`).

## `intent(in out)` Rule (`--suggest-intent-inout`)

A dummy is suggested for `intent(in out)` iff:

1. It passes eligibility.
2. It has at least one write event.
3. It has read evidence (`reads` or `spec_reads`).
4. It has no maybe-written-via-call event.
5. It is not const-like initialized in declaration.
6. It is not a formal that is observed with non-variable actuals at call sites.
7. If there is a clear top-level initialization write before first read, `intent(out)` is
   preferred and `intent(in out)` is not suggested.

Typical `intent(in out)` pattern accepted:

- read-modify-write use such as `x = alpha*x` or `a = a + 1`.

## Interprocedural Mode (`--interproc`)

`--interproc` improves call argument classification conservatively:

- For `call callee(...)` and function invocations `callee(...)`, if callee signature is known and the matched formal is explicit `intent(in)`/`value`, the actual is treated as read for that call.
- In addition, provisional intra-file `intent(in)` inferences are used conservatively to
  improve read-only call-argument classification.
- Otherwise, actual is treated as maybe-written-via-call.
- Ambiguous/missing callee resolution remains conservative.

## What `--fix` Changes

- Applies only fixable inferred suggestions.
- Rewrites declaration statements (with and without `::`), including multi-entity and continued declarations.
- Uses `intent(in out)` spelling for inout fixes.
- Reorders declarations in specification parts so argument declarations come before locals,
  in argument-list order. `parameter` declarations are kept before args only when needed
  by argument declarations.
- Does not modify lines already containing `intent(...)` or `value`.

## Practical Limitations

- Analysis is token/dataflow heuristic, not full semantic alias analysis.
- Compile validation with `--compiler` is strongly recommended for fix workflows.
