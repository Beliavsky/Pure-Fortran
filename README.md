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
- `xprune.py` - compile-validated pruning of likely unused top-level procedures.
- `ximplicit_none.py` - suggest/apply `implicit none` in program units.
- `xuse_only.py` - suggest/apply `use ..., only: ...` imports from broad `use` statements.
- `xunset.py` - advisory checker for likely use-before-set variables.
- `xunused.py` - advisory checker for likely unused set variables/constants, with optional conservative fix mode.
- `xintent_pure.py` - pipeline wrapper (`intent -> pure -> optional elemental`).
- `xintent_pure_private.py` - full pipeline wrapper (`intent -> pure -> optional elemental -> private`).
- `xstrip.py` - strip annotations (`intent`, `pure/elemental/impure`, or both) for testing.
- `xstrip_implicit_none.py` - strip `implicit none` statements for test-case generation.
- `xstrip_use_only.py` - strip `use ..., only: ...` back to broad `use` for testing.

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

### 5) `xprune.py`

Compile-validated pruning tool that removes likely unused top-level procedures.

By default, it writes a working copy of sources to `pruned/` and applies pruning there.
Use `--in-place` only when you explicitly want to modify the current source tree.

Typical commands:

```bash
python xprune.py --compiler "gfortran -o foo.exe"
python xprune.py --compiler "gfortran -o foo.exe" --out-dir pruned_stats
python xprune.py --compiler "gfortran -o foo.exe" --in-place
```

Notes:

- Pruning is conservative and compile-validated (`baseline`, `trial`, `final` compile passes).
- The tool removes accepted procedures and updates matching `public` lists for removed names.
- If a trial removal breaks compilation, it is reverted immediately.

### 6) `ximplicit_none.py`

Suggests and optionally inserts `implicit none` in:

- each `program`
- each `module` (inserted before `contains` when present)
- external procedures outside modules

It intentionally does not add `implicit none` to procedures contained inside modules, because module-level `implicit none` already covers them.

Typical commands:

```bash
python ximplicit_none.py
python ximplicit_none.py --fix
python ximplicit_none.py --fix --compiler "gfortran -o foo.exe"
```

### 7) `xuse_only.py`

Suggests and optionally rewrites broad imports such as:

- `use foo`

to explicit imports such as:

- `use foo, only: func_a, sub_b`

Typical commands:

```bash
python xuse_only.py
python xuse_only.py --fix
python xuse_only.py --fix --compiler "gfortran -o foo.exe"
```

Notes:

- Conservative analysis across source files using discovered module exports and identifier usage.
- In `--fix` mode, compile checks can be used (`baseline` and `after-fix`) with rollback on failure.
- Renamed imports (`=>`) are handled conservatively and may be skipped.

### 8) `xintent_pure_private.py`

Full pipeline wrapper for:

1. intent
2. pure
3. optional elemental
4. private

Typical command:

```bash
python xintent_pure_private.py --suggest-intent-out --suggest-elemental --compiler "gfortran -o foo.exe"
```

### 9) `xunset.py`

Advisory checker for likely use-before-set variables in procedures/program units.

Typical commands:

```bash
python xunset.py
python xunset.py stats.f90 --verbose
```

Notes:

- Advisory only (no `--fix`).
- Conservative static analysis; best used with manual review.

### 10) `xunused.py`

Advisory checker for likely unused set variables/constants.

Optional modes:

- `--fix`: apply conservative declaration/assignment removals when safe
- `--warn-dead-store`: also report likely dead stores (overwritten-before-read / final unread write)

Typical commands:

```bash
python xunused.py
python xunused.py --warn-dead-store --verbose
python xunused.py --fix --backup
```

Notes:

- `--fix` is conservative by design and skips unsafe edits.
- Dead-store warnings are advisory and branch-aware for `if/else` flows.

### 11) `xstrip.py`

Utility for test preparation by stripping annotations.

Typical commands:

```bash
python xstrip.py --strip all
python xstrip.py --strip intent --strip-value
python xstrip.py --strip pure
```

By default summary output is off; enable with `--summary`.

### 12) `xstrip_implicit_none.py`

Utility to remove `implicit none` statements for testing `ximplicit_none.py`.

Typical commands:

```bash
python xstrip_implicit_none.py --fix
python xstrip_implicit_none.py foo.f90 --fix --diff
```

### 13) `xstrip_use_only.py`

Utility to remove `only:` clauses from `use` statements for testing `xuse_only.py`.

Typical commands:

```bash
python xstrip_use_only.py --fix
python xstrip_use_only.py foo.f90 --fix --diff
```

## Recommended Transformation Order

If applying all transformations, use this order:

1. `implicit none` (`ximplicit_none.py`)
2. `intent` (`xintent.py`)
3. `pure` (`xpure.py`)
4. `elemental` (`xpure.py --suggest-elemental`)
5. `private` (`xprivate.py`)
6. optional structural cleanup: tighten imports/prune (`xuse_only.py`, `xprune.py`)
7. final cleanup/audit: unused values/dead stores (`xunused.py`)

`xintent_pure_private.py` implements the intent/pure/elemental/private subset of this sequence.

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
python ximplicit_none.py
```

### Prune to a separate source tree

```bash
python xprune.py --compiler "gfortran -o foo.exe"
```

This writes pruned sources to `pruned/` by default.

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

## Sample Output

`python xintent_pure_private.py --compiler "gfortran -o foo.exe" --suggest-intent-out --suggest-elemental`
on the source files in the `original` directory gave the source code in the `revised` directory and the terminal
output below.
```
=== Intent Phase ===
Command: c:\Programs\Python313\python.exe xintent.py kind.f90 constants.f90 qsort.f90 util.f90 gnuplot.f90 random.f90 stats.f90 interpret.f90 xinterpret.f90 --fix --iterate --max-iter 10 --compiler gfortran -o foo.exe --backup --suggest-intent-out
Processing order: kind.f90 constants.f90 qsort.f90 util.f90 gnuplot.f90 random.f90 stats.f90 interpret.f90 xinterpret.f90
Compile (baseline): gfortran -o foo.exe kind.f90 constants.f90 qsort.f90 util.f90 gnuplot.f90 random.f90 stats.f90 interpret.f90 xinterpret.f90
Compile (baseline): PASS
Compile (after-fix): gfortran -o foo.exe kind.f90 constants.f90 qsort.f90 util.f90 gnuplot.f90 random.f90 stats.f90 interpret.f90 xinterpret.f90
Compile (after-fix): PASS

=== Pure Phase ===
Command: c:\Programs\Python313\python.exe xpure.py kind.f90 constants.f90 qsort.f90 util.f90 gnuplot.f90 random.f90 stats.f90 interpret.f90 xinterpret.f90 --fix --iterate --max-iter 10 --compiler gfortran -o foo.exe --backup
Processing order: kind.f90 constants.f90 qsort.f90 util.f90 gnuplot.f90 random.f90 stats.f90 interpret.f90 xinterpret.f90
Compile (baseline): gfortran -o foo.exe kind.f90 constants.f90 qsort.f90 util.f90 gnuplot.f90 random.f90 stats.f90 interpret.f90 xinterpret.f90
Compile (baseline): PASS

Backup written: util.f90.bak

Applied PURE to 6 procedure declaration(s).

Backup written: stats.f90.bak

Applied PURE to 17 procedure declaration(s).
Compile (after-fix): gfortran -o foo.exe kind.f90 constants.f90 qsort.f90 util.f90 gnuplot.f90 random.f90 stats.f90 interpret.f90 xinterpret.f90
Compile (after-fix): PASS
Processing order: kind.f90 constants.f90 qsort.f90 util.f90 gnuplot.f90 random.f90 stats.f90 interpret.f90 xinterpret.f90

Backup written: stats.f90.bak

Applied PURE to 2 procedure declaration(s).
Compile (after-fix): gfortran -o foo.exe kind.f90 constants.f90 qsort.f90 util.f90 gnuplot.f90 random.f90 stats.f90 interpret.f90 xinterpret.f90
Compile (after-fix): PASS
Processing order: kind.f90 constants.f90 qsort.f90 util.f90 gnuplot.f90 random.f90 stats.f90 interpret.f90 xinterpret.f90
Compile (after-fix): gfortran -o foo.exe kind.f90 constants.f90 qsort.f90 util.f90 gnuplot.f90 random.f90 stats.f90 interpret.f90 xinterpret.f90
Compile (after-fix): PASS

Summary of 12 functions and 1 subroutine marked pure in 2 source files:
stats.f90 6 functions: arcoef loglik fit_lnorm fit_laplace hyperb_var hyperb_scale_from_sd
stats.f90 1 subroutine: unpack_params
util.f90 6 functions: arange grid replace rep_vec matrix reverse

=== Elemental Phase ===
Command: c:\Programs\Python313\python.exe xpure.py kind.f90 constants.f90 qsort.f90 util.f90 gnuplot.f90 random.f90 stats.f90 interpret.f90 xinterpret.f90 --fix --iterate --max-iter 10 --compiler gfortran -o foo.exe --backup --suggest-elemental
Processing order: kind.f90 constants.f90 qsort.f90 util.f90 gnuplot.f90 random.f90 stats.f90 interpret.f90 xinterpret.f90
Compile (baseline): gfortran -o foo.exe kind.f90 constants.f90 qsort.f90 util.f90 gnuplot.f90 random.f90 stats.f90 interpret.f90 xinterpret.f90
Compile (baseline): PASS

Backup written: stats.f90.bak

Applied ELEMENTAL to 18 procedure declaration(s).

Backup written: interpret.f90.bak

Applied ELEMENTAL to 1 procedure declaration(s).
Compile (after-fix): gfortran -o foo.exe kind.f90 constants.f90 qsort.f90 util.f90 gnuplot.f90 random.f90 stats.f90 interpret.f90 xinterpret.f90
Compile (after-fix): PASS
Processing order: kind.f90 constants.f90 qsort.f90 util.f90 gnuplot.f90 random.f90 stats.f90 interpret.f90 xinterpret.f90
Compile (after-fix): gfortran -o foo.exe kind.f90 constants.f90 qsort.f90 util.f90 gnuplot.f90 random.f90 stats.f90 interpret.f90 xinterpret.f90
Compile (after-fix): PASS

Summary of 19 functions and 0 subroutines marked elemental in 2 source files:
interpret.f90 1 function: lower_str
stats.f90 18 functions: hyperb_pdf_scalar hyperb_int inv_norm besseli0 besseli1 besselk0 besselk1 log1pexp log_beta hyperb_scale_from_sd hyperb_var tcdf betai chisq_cdf gammp gser gcf betacf

=== Private Phase ===
Command: c:\Programs\Python313\python.exe xprivate.py kind.f90 constants.f90 qsort.f90 util.f90 gnuplot.f90 random.f90 stats.f90 interpret.f90 xinterpret.f90 --fix --iterate --max-iter 10 --compiler gfortran -o foo.exe --backup
Processing order: kind.f90 constants.f90 qsort.f90 util.f90 gnuplot.f90 random.f90 stats.f90 interpret.f90 xinterpret.f90
Compile (baseline): gfortran -o foo.exe kind.f90 constants.f90 qsort.f90 util.f90 gnuplot.f90 random.f90 stats.f90 interpret.f90 xinterpret.f90
Compile (baseline): PASS
Backup written: constants.f90.bak
Backup written: gnuplot.f90.bak
Backup written: interpret.f90.bak
Backup written: kind.f90.bak
Backup written: qsort.f90.bak
Backup written: random.f90.bak
Backup written: stats.f90.bak
Backup written: util.f90.bak
Compile (after-fix): gfortran -o foo.exe kind.f90 constants.f90 qsort.f90 util.f90 gnuplot.f90 random.f90 stats.f90 interpret.f90 xinterpret.f90
Compile (after-fix): PASS
```
