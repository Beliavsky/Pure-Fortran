# Pure-Fortran

Pure-Fortran is a set of conservative Python tools that help modernize and tighten Fortran codebases by adding:

- `INTENT(...)` on dummy arguments
- `PURE` / `ELEMENTAL` on procedures
- `PRIVATE` accessibility inside modules

The tools are designed for iterative use on real projects: suggest changes, apply safe edits, compile, and roll back if needed.

## Motivation

Large Fortran codebases often evolve without complete interface annotations. Missing `intent`, missing `pure`, and broad default `public` visibility make code harder to reason about and harder for compilers to optimize.

Pure-Fortran focuses on practical improvements that are:

1. **Conservative**: static heuristics avoid aggressive unsafe rewrites.
2. **Compile-validated**: in fix mode, tools can verify that code still compiles.
3. **Incremental**: support iterative passes (`--iterate`) to unlock more opportunities as code improves.
4. **Reversible**: backups and rollback behavior reduce risk.

## Project Layout

Main programs:

- `xintent.py` - suggest/apply `intent(in)` and optional `intent(out)`.
- `xpure.py` - suggest/apply `pure`, optionally upgrade to `elemental`.
- `xprivate.py` - suggest/apply module-level `private :: name` restrictions.
- `xintent_pure.py` - pipeline wrapper (`intent -> pure -> optional elemental`).
- `xintent_pure_private.py` - full pipeline wrapper (`intent -> pure -> optional elemental -> private`).
- `xstrip.py` - strip annotations (`intent`, `pure/elemental/impure`, or both) for testing.

Shared support modules:

- `fortran_scan.py` - scanning/parsing helpers, dependency ordering, file selection.
- `fortran_build.py` - compile and rollback helpers, git commit helper.
- `fortran_pipeline.py` - shared pipeline orchestration used by wrappers.

## Requirements

- Python 3.10+
- Optional but recommended: `gfortran` (or another Fortran compiler command) for compile checks
- Optional: Git for `--git`

## General Behavior

Across tools:

- If no file list is provided, tools scan `*.f90` and `*.F90` in the current directory.
- File processing order is dependency-aware (least-dependent first).
- `--exclude` can be repeated to skip files by glob.
- `--backup` (default in fixing tools) writes `.bak` files before first modification.
- `--compiler` enables compile checks; for fix pipelines this is strongly recommended.
- `--git` commits changed files with an auto-generated message.

## Tool-by-Tool

### 1) `xintent.py`

Suggests and optionally applies missing intent attributes on dummy arguments.

Core use cases:

- add `intent(in)` where argument appears read-only
- optionally add `intent(out)` with `--suggest-intent-out`
- warn about unresolved arguments with `--warn-missing-intent`

Typical commands:

```bash
python xintent.py
python xintent.py --fix --iterate --compiler "gfortran -o foo.exe"
python xintent.py --fix --iterate --suggest-intent-out --warn-missing-intent --compiler "gfortran -o foo.exe"
```

Notes:

- Conservative analysis; ambiguous cases are left unchanged.
- Handles multi-entity declarations (for example `integer :: i, j`) when adding intents.

### 2) `xpure.py`

Suggests and optionally applies `pure` to procedures not currently marked `pure`/`elemental`.

Optional elemental mode:

- `--suggest-elemental` suggests/upgrades eligible `pure` procedures to `elemental`.

Typical commands:

```bash
python xpure.py
python xpure.py --fix --iterate --compiler "gfortran -o foo.exe"
python xpure.py --fix --iterate --suggest-elemental --compiler "gfortran -o foo.exe"
```

Notes:

- Uses conservative purity checks (calls, assignments, I/O, control statements, etc.).
- Internal I/O is treated differently from external I/O.
- On compile failure after fix, modified files can be rolled back from backups.

### 3) `xprivate.py`

Suggests and optionally applies `private :: name` inside modules.

It considers module entities, including procedures and module-scope declarations (variables/constants/types/procedures).

Typical commands:

```bash
python xprivate.py
python xprivate.py --fix --iterate --compiler "gfortran -o foo.exe"
```

Notes:

- Suggest mode is conservative around wildcard imports.
- Fix mode is compile-validated and reverts candidate edits that break compilation.

### 4) `xintent_pure.py`

Pipeline wrapper for:

1. `xintent.py` (`--fix --iterate`)
2. `xpure.py` (`--fix --iterate`)
3. optional `xpure.py --suggest-elemental`

Typical command:

```bash
python xintent_pure.py --suggest-intent-out --suggest-elemental --compiler "gfortran -o foo.exe"
```

### 5) `xintent_pure_private.py`

Full pipeline wrapper for:

1. intent
2. pure
3. optional elemental
4. private

Typical command:

```bash
python xintent_pure_private.py --suggest-intent-out --suggest-elemental --compiler "gfortran -o foo.exe"
```

### 6) `xstrip.py`

Utility for test preparation by stripping annotations.

Typical commands:

```bash
python xstrip.py --strip all
python xstrip.py --strip intent --strip-value
python xstrip.py --strip pure
```

By default summary output is off; enable with `--summary`.

## Recommended Transformation Order

If applying all transformations, use this order:

1. `intent` (`xintent.py`)
2. `pure` (`xpure.py`)
3. `elemental` (`xpure.py --suggest-elemental`)
4. `private` (`xprivate.py`)

This order is implemented directly in `xintent_pure_private.py`.

## Safety Model

- **Conservative inference**: if uncertain, tools usually do not edit.
- **Compile checks**: use `--compiler` so edits are validated.
- **Backups**: keep `.bak` files unless `--no-backup` is requested.
- **Rollback**: fix flows attempt restoration when post-edit compilation fails.

## Limitations

These are static heuristics, not full Fortran semantic compilers. Expect some false negatives (and occasional false positives that compile checks catch), especially around:

- indirect/global state effects
- polymorphic dispatch and procedure pointers
- complex host association and interface patterns
- project-specific build systems not represented by a single compiler command

## Practical Workflows

### Conservative audit only

```bash
python xintent.py
python xpure.py
python xprivate.py
```

### Aggressive iterative compile-validated modernization

```bash
python xintent_pure_private.py --suggest-intent-out --suggest-elemental --compiler "gfortran -o foo.exe"
```

### Build command tip

`--compiler` accepts either:

- a command where file paths are appended automatically, or
- a template using `{files}` placeholder.

Examples:

```bash
--compiler "gfortran -o foo.exe"
--compiler "gfortran -J build/mod -o foo.exe {files}"
```

## Git Integration

For tools that modify source, `--git` can auto-commit changed files with an informative message.

Use this only when your working tree state is ready for automatic commits.
