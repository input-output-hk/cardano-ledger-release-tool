# cardano-ledger-release-tool

Utility for managing releases of [cardano-ledger](https://github.com/IntersectMBO/cardano-ledger)

The executable is named `cleret` which is derived from **C**ardano **Le**dger **Re**lease **T**ool. It's also the name of a brand of squeegee, which seems appropriate for a tool that's used to make things squeaky clean.

## Usage

```
Cardano Ledger release tool

Usage: cleret [-v|--verbose] COMMAND

Available options:
  -h,--help                Show this help text
  -V,--version             Show version information
  -v,--verbose             Produce verbose output

Available commands:
  changelogs               Parse and lint changelog files
```

## Commands

### `changelogs`

Parse and re-render a changelog, as a form of linting; the output is the canonical representation of the changelog. Using `--inplace` followed by `git diff --exit-code` will determine whether the changelog needs to be changed.

```
Usage: cleret changelogs [(-i|--inplace) | (-o|--output FILE)] [-b|--bullets CHARS]
                         CHANGELOG ...

  Parse and lint changelog files

Available options:
  -h,--help                Show this help text
  -i,--inplace             Modify files in-place
  -o,--output FILE         Write output to FILE
  -b,--bullets CHARS       Use CHARS for the levels of bullets (default: *-+)
  CHANGELOG ...            Changelog files to process
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
