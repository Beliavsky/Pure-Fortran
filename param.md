# xparam.py Rules

This document defines the current rule set used by `xparam.py` to decide whether a Fortran variable can be made a named constant (`parameter`).

## Scope

- Unit types analyzed: `program`, `subroutine`, `function`.
- Inputs: selected `.f90`/`.F90` files.
- Analysis is static and conservative.

## Candidate Eligibility (high level)

A variable is a candidate only if all checks pass:

1. It is a local declared variable in the unit.
2. It is written exactly once.
3. Its first write is not in control flow.
4. Its first write is deterministic.
5. Its initializer expression is PARAMETER-safe.
6. It is not written in any contained internal procedure.

## Declaration Rules

A declared local is excluded immediately if any of the following holds:

- It is a dummy argument.
- It is already declared with `parameter`.
- It is not a simple scalar declaration, including:
  - explicit entity shape (e.g. `x(10)`),
  - `allocatable`,
  - `pointer`,
  - `target`.

## What Counts as a Write

`xparam.py` increments write count for:

- Declaration initialization (`:: x = ...` or pointer init form).
- Assignment (`x = ...`).
- Pointer assignment (`x => ...`).
- `allocate(...)`, `deallocate(...)`, `nullify(...)` on a variable.
- `read` destinations.
- `call random_number(x)` output argument.
- Actual arguments mapped to known same-file dummies with `intent(out)` or `intent(inout)`.
- Assignments in contained internal procedures that target host variables.

A variable must have write count exactly 1.

## Control-Flow Rule

First write must not occur in a control-flow context.

Control-flow contexts include block constructs such as:

- `if ... then ... end if`
- `do ... end do`
- `select case/type/rank ... end select`
- `where ... end where`
- `forall ... end forall`
- `associate ... end associate`
- `block ... end block`
- `critical ... end critical`

Single-line conditional forms are also treated as control flow:

- `if (cond) x = ...`

If first write is in any control-flow context, variable is excluded.

## Determinism Rule

First write is excluded as non-deterministic if it contains any of:

- `random_number`, `random_seed`
- `read`, `open`, `inquire`, `close`
- `system_clock`, `cpu_time`, `date_and_time`
- `get_command_argument`, `get_environment_variable`
- `execute_command_line`

## PARAMETER-Safe Expression Rule

Even if set once and deterministic, the first-write expression must be parameter-safe.

Current conservative checks:

- Reject if expression contains a function call pattern (`name(...)`).
- Collect identifier references (ignoring quoted string contents).
- Every referenced identifier must already be known parameter-like in the same unit analysis order.

This excludes runtime-derived expressions such as:

- `n = size(x)`
- `y = sum(x)/n`

## Internal Procedure Rule

If a local variable in a host procedure is written in any contained internal subroutine/function, it is excluded.

Example: host variable `m` assigned in internal `bar` => cannot be `parameter`.

## Output Categories

- `constant candidate(s)`: variables that pass all rules.
- `Excluded locals` (with `--verbose`): variables rejected with reason text.

## Fix Modes

### `--fix` (conservative)

- Creates backup before first change in a file (`.bak`, `.bak1`, ...).
- Rewrites only safe candidates where declaration line contains exactly one entity.
- Moves first assignment expression to declaration as `parameter` initializer.
- Removes the moved assignment statement line.
- Skips multi-entity declaration lines (e.g. `integer :: i, n`).

### `--fix-all` (aggressive)

Everything in `--fix`, plus:

- Can split multi-entity declarations to extract candidate into its own `parameter` declaration.

Example transformation:

- Before: `integer :: i, n` and `n = 3`
- After: `integer :: i` and `integer, parameter :: n = 3`

## Important Limitations

- This is not full semantic analysis.
- Unknown external procedure effects are not fully modeled unless intent mapping is available in same file.
- PARAMETER-safety uses conservative heuristics; valid Fortran constant expressions may still be rejected.
- Compile validation is not built into `xparam.py`; review and compile after fixes.
