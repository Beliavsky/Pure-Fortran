# xdiff.py - Fortran module compare tool

xdiff.py compares two versions of a Fortran module and reports structural differences:

- Procedures present in one version but not the other
- Procedures present in both but different (exact or semantic)
- Module variables and module parameters (PARAMETER) added/removed
- USE statement differences (modules used, ONLY imports, renames)
- Accessibility differences (default PUBLIC/PRIVATE, explicit lists, effective public API)
- Optional unified diffs per changed procedure
- Optional history mode (worktree vs HEAD, then commit vs previous commit)

This document describes usage and options.

## Requirements

- Python 3.8+ (recommended: 3.10+)
- The helper module `fortran_scan.py` must be importable (place it in the same directory as xdiff.py).
- For "one file vs HEAD" or "--history" modes:
  - git must be installed and the file must be inside a git repository.

## Basic usage

### Compare two files

```
python xdiff.py m1.f90 m2.f90
```

By default, xdiff.py selects a module to compare using this rule:

- If `--module NAME` is given, compare that module.
- Else, if each file contains exactly one module, compare those.
- Else, if there is a module name common to both files, compare the first common name (alphabetical).
- Else, compare the first module found in each file.

### Compare working tree file vs HEAD

If you provide a single file path, xdiff.py compares the working tree version to `HEAD:path`:

```
python xdiff.py src/my_mod.f90
```

The tool will label inputs as:

- file1: worktree:<absolute_path>
- file2: HEAD:<repo_relative_path>

### History mode (file evolution)

History mode prints a compact summary for:

1) worktree vs HEAD
2) each commit vs the previous commit for that file (from `git log --follow`)

```
python xdiff.py src/my_mod.f90 --history
```

By default, history mode limits the number of commits examined:

- `--max-commits 25` (default)
- `--max-commits 0` means no limit

## Procedure comparison modes

### Exact mode (default)

Two procedures are considered the same only if their extracted text blocks match exactly line-for-line
(including comments). The only thing implicitly ignored is line-ending style.

### Semantic mode

Enable semantic procedure comparison with:

```
python xdiff.py m1.f90 m2.f90 --semantic
```

In semantic mode, a procedure is normalized into a statement stream and then compared. Normalization:

- Join free-form continuation lines
- Split multiple statements separated by semicolons
- Strip comments
- Convert to lowercase
- Collapse whitespace

This is intended to ignore superficial formatting changes while still detecting behavioral changes.

## Unified diffs for procedures

To print unified diffs for each procedure reported as different:

```
python xdiff.py m1.f90 m2.f90 --diff-procs
```

Tuning:

- `--diff-context N` sets the number of context lines (default 3).
- `--diff-max-lines N` truncates each procedure diff to N lines (0 means no limit).

Examples:

```
python xdiff.py m1.f90 m2.f90 --diff-procs --diff-context 5
python xdiff.py m1.f90 m2.f90 --diff-procs --diff-max-lines 400
```

Notes:

- In history mode, diffs are disabled by default because output can be large.
- Use `--history-diff-procs` to enable diffs in history mode.

## Module entities (specification part)

Before the module CONTAINS statement, xdiff.py attempts to extract:

- Module variables (non-PARAMETER declarations)
- Module constants (declarations with PARAMETER)

The extractor is heuristic and focuses on common declaration styles, e.g.:

- `real(dp) :: x, y`
- `integer, parameter :: n = 10`

The tool skips declarations inside INTERFACE blocks and TYPE ... END TYPE blocks in the specification part.

## USE statement differences (before CONTAINS)

xdiff.py compares USE statements before CONTAINS and reports:

- Modules used only in one file vs the other
- For common modules:
  - wildcard USE vs USE, ONLY differences
  - ONLY imported entity differences
  - simple renames of the form `local => remote`

Notes:

- It ignores operator/assignment imports in ONLY lists (e.g. `operator(+)`), because they are harder to model
  consistently and are often rare.

## Accessibility and public API differences

xdiff.py compares accessibility controls before CONTAINS:

- Default accessibility:
  - `private` alone sets default private
  - `public` alone sets default public (also the default Fortran behavior)

- Explicit lists:
  - `public :: a, b`
  - `private :: x, y`

- Declaration attributes (in the declaration prefix before ::), e.g.:
  - `integer, private :: counter`
  - `real, public :: scale`

The effective public-ness is computed using the priority:

1) explicit private list
2) explicit public list
3) declaration private attribute
4) declaration public attribute
5) default (public unless default private)

xdiff.py reports effective public API differences for:

- Procedures
- Module variables
- Module parameters

## History mode output

Enable:

```
python xdiff.py path/to/file.f90 --history
```

Optional details:

- `--history-names` prints the names of added/removed/changed items per pair.
- `--history-diff-procs` prints unified diffs of changed procedures per pair (can be very large).

Example:

```
python xdiff.py src/my_mod.f90 --history --max-commits 50 --history-names
```

The summary line includes counts like:

- procs +A -R ~C (added, removed, changed)
- vars +A -R
- params +A -R
- use +A -R
- public(...) counts for public API changes
- default_access_changed marker when default PUBLIC/PRIVATE changed

## Exit codes

- 0: no differences found
- 1: one or more differences found

In history mode, the exit code is 1 if any pair (including worktree vs HEAD) reports differences.

## Suggested workflows

### Replace git diff when you care about module structure

- Use xdiff.py to see what procedures/exports/imports changed.
- Use `--diff-procs` when you want the line-level patch for specific procedures.

### Track evolution in time

- Use `--history --history-names` to get a structural changelog.
- Add `--semantic` if formatting churn is high.

## Known limitations

- Parsing is heuristic; very complex Fortran (especially heavy preprocessor use) can confuse extraction.
- If a procedure name appears multiple times in a module (duplicates), it is reported as ambiguous.
- Only one module is compared per run unless you add a future --all-modules option.
- The tool does not currently parse:
  - Type-bound procedures inside derived types
  - Generic interfaces in detail (it skips bodies for procedure extraction)

## Option reference

- `--module NAME` : choose module name to compare
- `--semantic` : semantic procedure comparison
- `--diff-procs` : unified diffs for changed procedures
- `--diff-context N` : diff context lines (default 3)
- `--diff-max-lines N` : truncate per-procedure diff (0 means unlimited)

- `--history` : history mode for a single file
- `--max-commits N` : max commits in history mode (default 25, 0 means all)
- `--history-names` : show names in history mode
- `--history-diff-procs` : unified diffs in history mode
