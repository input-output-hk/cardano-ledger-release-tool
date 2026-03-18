# cardano-ledger-release-tool

Utility for managing releases of [cardano-ledger](https://github.com/IntersectMBO/cardano-ledger)

The executable is named `cleret` which is derived from **C**ardano **Le**dger **Re**lease **T**ool. It's also the name of a brand of squeegee, which seems appropriate for a tool that's used to make things squeaky clean.

## Usage

```
Cardano Ledger release tool

Usage: cleret COMMAND

Available options:
  -h,--help                Show this help text
  -V,--version             Show version information

Available commands:
  cabal                    Operations on a Cabal project
  changelogs               Operations on the changelogs of a project
  workflow                 Operations on the GitHub workflows of a Cabal project
```

## Commands

### `changelogs`

```
Usage: cleret changelogs COMMAND

  Operations on the changelogs of a project

Available options:
  -h,--help                Show this help text

Available commands:
  format                   Parse and reformat changelog files
```

#### `changelogs format`

```
Usage: cleret changelogs format [-v|--verbose] [(-i|--inplace) | (-o|--output FILE)]
                                [-b|--bullets CHARS] CHANGELOG ...

  Parse and reformat changelog files

Available options:
  -h,--help                Show this help text
  -v,--verbose             Produce verbose output
  -i,--inplace             Modify files in-place
  -o,--output FILE         Write output to FILE
  -b,--bullets CHARS       Use CHARS for the levels of bullets (default: *-+)
  CHANGELOG ...            Changelog files to process
```

Parse and re-render a changelog, as a form of linting; the output is the canonical representation of the changelog. Using `--inplace` followed by `git diff --exit-code` will determine whether the changelog needs to be changed.

### `workflow`

```
Usage: cleret workflow COMMAND

  Operations on the GitHub workflows of a Cabal project

Available options:
  -h,--help                Show this help text

Available commands:
  check-test-matrix        Check that the test jobs in a GitHub workflow match the
                           tests in a Cabal project
```

#### `workflow check-test-matrix`

```
Usage: cleret workflow check-test-matrix
         [-v|--verbose] [--project DIR] [--workflow FILENAME]

  Check that the test jobs in a GitHub workflow match the tests in a Cabal project

Available options:
  -h,--help                Show this help text
  -v,--verbose             Produce verbose output
  --project DIR            The project directory, or a subdirectory of it
                           (default: .)
  --workflow FILENAME      The workflow file name (relative to .github/workflows)
                           (default: haskell.yml)
```

Outputs the differences between the actual and the expected, and exits with a non-zero status if there are differences.

### `cabal`

```
Usage: cleret cabal COMMAND

  Operations on a Cabal project

Available options:
  -h,--help                Show this help text

Available commands:
  targets                  List the targets in a Cabal project
```

#### `cabal targets`

```
Usage: cleret cabal targets [-v|--verbose] [-p|--project DIR] [-i|--include TYPE]
                            [-x|--exclude TYPE] [PACKAGE ...]

  List the targets in a Cabal project

Available options:
  -h,--help                Show this help text
  -v,--verbose             Produce verbose output
  -p,--project DIR         The project directory, or a subdirectory of it
                           (default: .)
  -i,--include TYPE        Include targets of type TYPE (repeatable; one of: lib,
                           flib, exe, test, bench, setup)
  -x,--exclude TYPE        Exclude targets of type TYPE (repeatable; one of: lib,
                           flib, exe, test, bench, setup)
  PACKAGE ...              Show targets for PACKAGE ... (default: all packages)
```

## Building the Code

During development, use `nix develop` and `cabal build`.

To build a static binary, use `nix build .#static`.

Make sure you have the following in `~/.config/nix/nix.conf` or `/etc/nix/nix.conf`:

```ini
experimental-features = nix-command flakes
accept-flake-config = true
```

## Making a Release

See [RELEASING.md](RELEASING.md).
