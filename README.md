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
- `xpure.py` - suggest/apply `pure`, optionally upgrade to `elemental`, and advisory-suggest `impure elemental`.
- `xprivate.py` - suggest/apply module-level `private :: name` restrictions.
- `xprune.py` - compile-validated pruning of likely unused top-level procedures.
- `ximplicit_none.py` - suggest/apply `implicit none` in program units.
- `xuse_only.py` - suggest/apply `use ..., only: ...` imports from broad `use` statements.
- `xunset.py` - advisory checker for likely use-before-set variables.
- `xunused.py` - advisory checker for likely unused set variables/constants, with optional conservative fix mode.
- `xparam.py` - advisory checker for variables that can be made named constants (`parameter`), with optional fix modes.
- `xrepeated_if.py` - advisory checker/fixer for consecutive repeated IF conditions.
- `xfunc_print.py` - advisory checker/fixer for external output statements inside functions.
- `xnotrim.py` - advisory checker/fixer for likely needless `trim(...)` in string equality/inequality comparisons.
- `xoptval.py` - advisory checker/fixer for optional-argument defaulting patterns replaceable with `stdlib` `optval(...)`.
- `xmerge.py` - advisory checker/fixer for simple `if/else` assignment blocks replaceable with `merge(...)`.
- `xdealloc.py` - advisory checker for early `deallocate(...)` opportunities, with annotation/fix workflows.
- `xformat.py` - advisory checker/fixer for format literals that can be shortened with repeat edit descriptors.
- `xadvance.py` - advisory checker for non-advancing `write` loops that can be collapsed.
- `xnames.py` - advisory checker for variable/procedure names that collide with Fortran keywords/intrinsics.
- `xkind.py` - advisory checker/fixer for hard-coded kind numbers (`_8`, `kind=8`, etc.).
- `xalloc_assign.py` - advisory checker/fixer for redundant `allocate(...)` immediately before whole-array assignment.
- `xset_array.py` - advisory checker for replacing consecutive scalar array-element assignments with one array assignment.
- `xarray.py` - advisory checker for simple loops replaceable by array operations (`sum/product/count` and elementwise forms).
- `xbounds.py` - advisory checker for likely out-of-bounds indexing/slicing patterns.
- `xone_line_if.py` - advisory checker/fixer for collapsible three-line IF blocks (and reverse expansion mode).
- `xlong_lines.py` - advisory checker/fixer for overlong Fortran lines via continuation wrapping.
- `xpower.py` - advisory checker/fixer for repeated multiplication terms (`x*x`) that can be written as powers (`x**2`).
- `xno_variable.py` - advisory checker/fixer for single-use local scalar temporaries that can be inlined.
- `xdealloc_tail.py` - advisory checker for redundant tail `deallocate(...)` of local allocatables.
- `xintent_pure.py` - pipeline wrapper (`intent -> pure -> optional elemental`).
- `xintent_pure_private.py` - full pipeline wrapper (`intent -> pure -> optional elemental -> private`).
- `xstrip.py` - strip annotations (`intent`, `pure/elemental/impure`, or both) for testing.
- `xstrip_implicit_none.py` - strip `implicit none` statements for test-case generation.
- `xstrip_use_only.py` - strip `use ..., only: ...` back to broad `use` for testing.
- `xdiff.py` compares two versions of a Fortran module and reports structural differences, documented [here](/xdiff.md)

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
- `--suggest-impure-elemental` advisories procedures that may be markable `impure elemental` (no fix mode).

Typical commands:

```bash
python xpure.py
python xpure.py --fix --iterate --compiler "gfortran -o foo.exe"
python xpure.py --fix --iterate --suggest-elemental --compiler "gfortran -o foo.exe"
python xpure.py --suggest-impure-elemental
```

Notes:

- Uses conservative purity checks (calls, assignments, I/O, control statements, etc.).
- Internal I/O is treated differently from external I/O.
- On compile failure after fix, modified files can be rolled back from backups.
- `--suggest-impure-elemental` is advisory-only and cannot be combined with `--fix`.

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

### 11) `xparam.py`

Advisory checker for variables that may be converted to named constants (`parameter`).

Optional modes:

- `--fix`: conservative auto-fix for single-entity declarations
- `--annotate`: insert suggestion comments (or tag changed declarations with `--fix`)
- `--fix-all`: more aggressive auto-fix that may split multi-entity declarations
- `--fix-alloc`: allow eligible `allocatable` arrays to be promoted to `parameter`
- `--diff`: with fix modes, print unified diffs for changed files

Typical commands:

```bash
python xparam.py
python xparam.py foo.f90 --verbose
python xparam.py foo.f90 --fix
python xparam.py foo.f90 --annotate
python xparam.py foo.f90 --fix-all
python xparam.py foo.f90 --fix-all --fix-alloc
python xparam.py foo.f90 --fix --annotate --diff
```

Notes:

- Fix modes create backups before editing (`.bak`, `.bak1`, ...).
- `--fix` and `--fix-all` run recursively by default (re-analyze and re-apply) until no more variables can be converted.
- During fixes, generated `parameter` declarations are reordered as needed so dependencies are declared before use.
- By default, `allocatable` declarations are excluded; use `--fix-alloc` to opt in.
- Full rule details are documented in [param.md](param.md).

### 12) `xrepeated_if.py`

Warns about consecutive lines of code (optionally separated by comments) that test the same `if` condition.

Optional mode:

- `--fix`: rewrite repeated single-line IF pairs into one `if (...) then ... end if` block
- `--diff`: with `--fix`, print unified diffs for changed files

Typical commands:

```bash
python xrepeated_if.py
python xrepeated_if.py foo.f90 --verbose
python xrepeated_if.py foo.f90 --fix
python xrepeated_if.py foo.f90 --fix --diff
```

### 13) `xfunc_print.py`

Warns when functions contain external output statements (`print`, `write(*,...)`).

Fix strategy options (mutually exclusive):

- `--fix-msg`
- `--fix-msg-error-stop`
- `--fix-msg-error-stop-block`
- `--fix-unit`
- `--fix-unit-error-stop`
- `--fix-suppress`
- `--fix-error-stop`
- `--diff`: print unified diffs for changed files

Typical commands:

```bash
python xfunc_print.py
python xfunc_print.py foo.f90 --verbose
python xfunc_print.py foo.f90 --fix-msg --diff
python xfunc_print.py foo.f90 --fix-msg-error-stop-block --diff
python xfunc_print.py foo.f90 --fix-unit-error-stop --diff
python xfunc_print.py foo.f90 --fix-suppress --diff
```

Notes:

- Fix strategies are intentionally explicit and mutually exclusive.
- Detailed strategy behavior and caveats are documented in [func_print.md](func_print.md).

### 14) `xnotrim.py`

Warns about likely needless `trim(...)` in string `==`/`/=` (`.eq.`/`.ne.`) comparisons.

Optional modes:

- `--fix`: remove needless `trim(var)` wrappers in high-confidence comparison forms
- `--diff`: with `--fix`, print unified diffs for changed files

Typical commands:

```bash
python xnotrim.py
python xnotrim.py foo.f90 --verbose
python xnotrim.py foo.f90 --fix
python xnotrim.py foo.f90 --fix --diff
```

### 15) `xstrip.py`

Utility for test preparation by stripping annotations.

Typical commands:

```bash
python xstrip.py --strip all
python xstrip.py --strip intent --strip-value
python xstrip.py --strip pure
```

By default summary output is off; enable with `--summary`.

### 16) `xstrip_implicit_none.py`

Utility to remove `implicit none` statements for testing `ximplicit_none.py`.

Typical commands:

```bash
python xstrip_implicit_none.py --fix
python xstrip_implicit_none.py foo.f90 --fix --diff
```

### 17) `xstrip_use_only.py`

Utility to remove `only:` clauses from `use` statements for testing `xuse_only.py`.

Typical commands:

```bash
python xstrip_use_only.py --fix
python xstrip_use_only.py foo.f90 --fix --diff
```

### 18) `xdealloc.py`

Advisory checker for local allocatables that may be safely deallocated earlier.

Optional modes:

- `--annotate`: insert suggestions from current findings.
- `--fix`: convert existing `! deallocate (...) !! suggested by xdealloc.py` comments to active `deallocate (...)` statements.
- `--annotate --fix`: insert active `deallocate (...) !! suggested by xdealloc.py` lines directly from findings.

Typical commands:

```bash
python xdealloc.py
python xdealloc.py foo.f90 --verbose
python xdealloc.py foo.f90 --annotate
python xdealloc.py foo.f90 --fix
python xdealloc.py foo.f90 --annotate --fix
```

Notes:

- Suggestions are conservative and skip common unsafe cases (for example function result variables, loop-local last uses, near-unit-end structural-only tails).
- Edit modes create backups before modifying files (`.bak`, `.bak1`, ...).

### 19) `xformat.py`

Advisory checker/fixer for shorten-able Fortran format literals in `print`/`write` statements.

Optional modes:

- `--fix`: apply conservative rewrites (for example `(a,a,a,i0,a,i0,a)` -> `(3a,2(i0,a))`).
- `--diff`: with `--fix`, print unified diffs for changed files.

Typical commands:

```bash
python xformat.py
python xformat.py foo.f90 --verbose
python xformat.py foo.f90 --fix
python xformat.py foo.f90 --fix --diff
```

Notes:

- Current matching is conservative and focuses on string-literal format specs.
- Fix mode rewrites only unambiguous matches and creates backups before edits.

### 20) `xadvance.py`

Advisory checker for simple indexed `do` loops that perform one non-advancing `write(..., advance="no")` per iteration and can be collapsed.

Optional mode:

- `--annotate`: add suggestion comments after matching loops:
  `! write (...) ...  !! suggested by xadvance.py`

Typical commands:

```bash
python xadvance.py
python xadvance.py foo.f90 --verbose
python xadvance.py foo.f90 --annotate
```

Notes:

- Suggestions use single-write forms with unlimited-repeat formats when a literal format is available (for example `"(*(1x,f12.6))"`).
- Array-section output is suggested when possible; otherwise implied-do output is suggested.
- `--annotate` creates backups before edits.

### 21) `xset_array.py`

Advisory checker for runs of consecutive scalar element assignments that can be replaced by one array-constructor assignment.

Optional mode:

- `--fix`: apply suggested rewrites in-place.
- `--annotate`: insert suggestion comments after the candidate lines (or tag changed lines with `--fix`).
- `--diff`: with `--fix`, print unified diffs for changed files.
- `--vector-subscript`: also suggest/fix non-consecutive element assignments using vector subscripts.

Typical commands:

```bash
python xset_array.py
python xset_array.py foo.f90 --verbose
python xset_array.py foo.f90 --annotate
python xset_array.py foo.f90 --vector-subscript --verbose
python xset_array.py foo.f90 --fix --annotate --diff
```

Notes:

- By default, suggestions use explicit section LHS (for example `x(1:4) = [...]`).
- When full bounds are clearly covered, suggestions use whole-array LHS (for example `x = [...]`).
- Detects vectorizable forms such as `f(a(i))` and suggests `f(a)`/`f(a(lo:hi))` when safe.

### 22) `xarray.py`

Advisory checker for simple `do` loops that can be replaced with array operations.

Optional mode:

- `--fix`: apply suggested rewrites in-place.
- `--annotate`: insert marked replacement blocks and suggested array-operation lines (or tag changed lines with `--fix`).
- `--diff`: with `--fix`, print unified diffs for changed files.

Typical commands:

```bash
python xarray.py
python xarray.py foo.f90 --verbose
python xarray.py foo.f90 --annotate
python xarray.py foo.f90 --fix --annotate --diff
```

Notes:

- Current conservative patterns include:
  - elementwise loops (`c(i)=...`),
  - `sum` reductions,
  - `product` reductions,
  - masked `sum` reductions,
  - `count` patterns,
  - `minval` / `maxval` reductions,
  - `minloc` / `maxloc` index-tracking reductions (strict `<`/`>` patterns).
- Supports simple strided loops (`do i=lb,ub,step`) in supported reduction patterns.
- Avoids recurrence-style false positives (for example `y(i)=y(i-1)+...`).
- Edit modes create backups before edits.

### 23) `xpower.py`

Advisory checker/fixer for repeated multiplication of the same operand (for example `x*x`) that can be rewritten as power form (`x**2`).

Optional modes:

- `--fix`: apply in-place rewrites.
- `--annotate`: with `--fix`, append `!! changed by xpower.py` on changed lines.
- `--diff`: with `--fix`, print unified diffs for changed files.

Typical commands:

```bash
python xpower.py
python xpower.py foo.f90 --verbose
python xpower.py foo.f90 --fix
python xpower.py foo.f90 --fix --annotate --diff
```

### 24) `xno_variable.py`

Advisory checker/fixer for single-use local scalar temporaries that can be inlined.

Optional modes:

- `--fix`: inline safe candidates and remove now-unused declaration entities.
- `--annotate`: with `--fix`, add declaration comments such as `!! x, y removed by xno_variable.py`.

Typical commands:

```bash
python xno_variable.py
python xno_variable.py foo.f90 --verbose
python xno_variable.py foo.f90 --fix
python xno_variable.py foo.f90 --fix --annotate --verbose
```

Notes:

- Includes a safety check that blocks invalid inlining when RHS dependencies are reassigned between temp definition and use (for example swap patterns).
- Fix mode creates backups before edits.

### 25) `xdealloc_tail.py`

Advisory checker for redundant `deallocate(...)` statements on local allocatables near program-unit end.

Optional mode:

- `--annotate`: append inline comments such as `!! xdealloc_tail.py suggests deleting this line`.

Typical commands:

```bash
python xdealloc_tail.py
python xdealloc_tail.py foo.f90 --verbose
python xdealloc_tail.py foo.f90 --annotate
```

Notes:

- Focuses on local allocatables and tail-position deallocations with no meaningful trailing work.
- `--annotate` creates backups before edits.

### 26) `xnames.py`

Advisory checker for identifiers that collide with Fortran keywords or intrinsic names.

Optional mode:

- `--verbose`: print full offending statements/contexts.

Typical commands:

```bash
python xnames.py
python xnames.py foo.f90 --verbose
```

Notes:

- Advisory only (no `--fix`).

### 27) `xkind.py`

Advisory checker/fixer for hard-coded kind numbers.

Optional modes:

- `--fix`: apply in-place rewrites.
- `--annotate`: with `--fix`, append `!! changed by xkind.py` on changed lines.
- `--diff`: with `--fix`, print unified diffs for changed files.

Typical commands:

```bash
python xkind.py
python xkind.py foo.f90 --verbose
python xkind.py foo.f90 --fix --annotate --diff
```

Notes:

- Handles suffix kinds (`0_8`, `1.0_8`), selector kinds (`kind=8`, `real(8)`), and kind-parameter constants (for example `dp = 8` when used in kind selectors).
- In module scope, fix mode can introduce module-level `xkind_*` named kind constants and rewrite literals/selectors to use them.

### 28) `xalloc_assign.py`

Advisory checker/fixer for redundant `allocate(...)` immediately before whole-array assignment.

Optional modes:

- `--fix`: comment out redundant allocate lines.
- `--annotate`: annotate findings; with `--fix`, append `!! commented out by xalloc_assign.py`.
- `--diff`: with `--fix`, print unified diffs for changed files.

Typical commands:

```bash
python xalloc_assign.py
python xalloc_assign.py foo.f90 --verbose
python xalloc_assign.py foo.f90 --fix --annotate --diff
```

Notes:

- Conservative by design (skips unsafe patterns).
- When shape would be lost after removing `allocate(var(1))`, fix mode can rewrite scalar assignment to singleton constructor form (for example `var = [0.0_dp]`).

### 29) `xbounds.py`

Advisory checker for likely out-of-bounds array index/slice patterns.

Optional mode:

- `--verbose`: print statements and diagnostic details.

Typical commands:

```bash
python xbounds.py
python xbounds.py foo.f90 --verbose
```

Notes:

- Advisory only (no `--fix`).
- Uses conservative guard inference (for example `size(...)` checks, alias guards, loop bounds).

### 30) `xone_line_if.py`

Advisory checker/fixer for three-line IF blocks that can be collapsed to one-line IF, with optional reverse expansion.

Optional modes:

- `--fix`: apply rewrite.
- `--annotate`: insert suggestion comments.
- `--diff`: with edits, print unified diffs.
- `--reverse`: operate in reverse (one-line IF -> 3-line block).

Typical commands:

```bash
python xone_line_if.py
python xone_line_if.py foo.f90 --verbose
python xone_line_if.py foo.f90 --fix --diff
python xone_line_if.py foo.f90 --reverse --fix --diff
```

### 31) `xlong_lines.py`

Advisory checker/fixer for lines longer than a configurable maximum (default 100).

Optional modes:

- `--max-len N`: set length threshold (default `100`).
- `--fix`: wrap long lines with continuation (`&`) where safe.
- `--diff` / `-diff`: with `--fix`, print unified diffs.
- `--verbose`: print full overlong lines.

Typical commands:

```bash
python xlong_lines.py
python xlong_lines.py foo.f90 --max-len 100 --verbose
python xlong_lines.py foo.f90 --fix --diff
```

### 32) `xoptval.py`

Advisory checker/fixer for optional-defaulting idioms that can be rewritten with `optval(...)` from `stdlib_optval`.

Optional modes:

- `--fix`: apply in-place rewrites and insert `use stdlib_optval, only: optval` where needed.
- `--annotate`: insert suggestion comments (or add `!! changed by xoptval.py` on changed lines with `--fix`).
- `--diff`: with `--fix`, print unified diffs for changed files.
- `--verbose`: print full findings with source context.

Typical commands:

```bash
python xoptval.py
python xoptval.py foo.f90 --verbose
python xoptval.py foo.f90 --fix --diff
python xoptval.py foo.f90 --fix --annotate --diff
```

Notes:

- Handles common patterns such as:
  - `x = default; if (present(arg)) x = arg`
  - `if (present(arg)) then; x = arg; else; x = default; end if`
- In fix mode, required `use stdlib_optval, only: optval` import is kept in the module/specification part (before `implicit none`).

### 33) `xmerge.py`

Advisory checker/fixer for simple `if/else` assignment blocks that can be replaced by `merge(...)`.

Optional modes:

- `--fix`: apply conservative in-place rewrites.
- `--annotate`: insert suggestion comments (or add `!! changed by xmerge.py` on changed lines with `--fix`).
- `--diff` / `-diff`: with `--fix`, print unified diffs for changed files.
- `--verbose`: print full suggestions.

Typical commands:

```bash
python xmerge.py
python xmerge.py foo.f90 --verbose
python xmerge.py foo.f90 --annotate
python xmerge.py foo.f90 --fix --annotate --diff
```

Notes:

- Matching is intentionally strict: one-statement-per-line, contiguous 5-line `if/else/end if` blocks assigning the same LHS in both branches.
- Rewrites are skipped for known unsafe cases, including:
  - `present(...)`-guarded optionals referenced in either source expression,
  - string-literal branch values with unequal character lengths,
  - complex branch expressions outside the supported simple-source subset.

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
