# xfunc_print.py Modes

`xfunc_print.py` flags external output in functions (`print`, `write(*,...)`) and supports optional rewrite strategies.

## Purpose

The tool helps move function-side user output into more explicit control paths.
By default it is advisory-only.

## Detection

Findings are reported for statements inside function bodies that match:

- `print ...`
- `write(*,...) ...`
- `write(unit=*,...) ...`

## CLI

- `python xfunc_print.py [files...]`
- `--exclude` (repeatable glob pattern)
- `--verbose` (print offending statement lines)
- `--diff` (with fix modes, print unified diff)

## Fix Strategies (Mutually Exclusive)

Exactly zero or one of these may be used per run:

- `--fix-msg`
- `--fix-msg-error-stop`
- `--fix-msg-error-stop-block`
- `--fix-unit`
- `--fix-unit-error-stop`
- `--fix-suppress`
- `--fix-error-stop`

Backups are created before edits (`.bak`, `.bak1`, ...).

## Strategy Details

### `--fix-msg`

- Adds optional `character(len=*), intent(out), optional` argument (default base name `msg`, collision-safe suffixes: `msg1`, `msg2`, ...).
- Replaces output statements with guarded assignments:
  - `if (present(msg)) msg = "..."`
- If no message literal is extractable, falls back to storing statement text.

### `--fix-msg-error-stop`

- Same as `--fix-msg`, plus fallback termination when `msg` is absent:
  - `if (present(msg)) then ... else error stop ... end if`

### `--fix-msg-error-stop-block`

- Attempts conservative block-level rewrite for patterns like:
  - `if (...) then`
  - one output statement
  - recovery tail statements (e.g. allocate/return)
  - `end if`
- Rewrites so recovery tail stays in `present(msg)` branch while absent-message path uses `error stop`.
- Falls back to line-level message rewrite when pattern is not safely recognized.

### `--fix-unit`

- Adds optional `integer, intent(in), optional` argument (default base name `out_unit`, collision-safe suffixes).
- Replaces output statements with:
  - `if (present(out_unit)) write(out_unit,'(a)') '...'`

### `--fix-unit-error-stop`

- Same as `--fix-unit`, with absent-unit fallback:
  - `if (present(out_unit)) then ... else error stop ... end if`

### `--fix-suppress`

- Comments out offending output lines:
  - `! xfunc_print suppressed: ...`

### `--fix-error-stop`

- Replaces offending output lines with `error stop`.

## Notes and Caveats

- These are heuristic transformations, not full semantic refactors.
- Multi-statement semicolon lines are skipped by fix modes.
- Function header rewrites are conservative; complex continued headers may be skipped.
- `--fix-msg-error-stop*` / `--fix-unit-error-stop` intentionally alter failure behavior.

## Examples

```bash
python xfunc_print.py stats.f90 --verbose
python xfunc_print.py stats.f90 --fix-msg --diff
python xfunc_print.py stats.f90 --fix-msg-error-stop-block --diff
python xfunc_print.py stats.f90 --fix-unit-error-stop --diff
python xfunc_print.py stats.f90 --fix-suppress --diff
```
