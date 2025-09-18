# cardano-ledger-release-tool

Utility for managing releases of [cardano-ledger](https://github.com/IntersectMBO/cardano-ledger)

```
Cardano Ledger release tool

Usage: cardano-ledger-release-tool [-v|--verbose] COMMAND

Available options:
  -h,--help                Show this help text
  -v,--verbose             Produce verbose output

Available commands:
  changelogs               Parse and lint changelog files
```

The subcommands are described below.

## `changelogs`

Parse and re-render a changelog, as a form of linting; the output is the canonical representation of the changelog. Using `--inplace` followed by `git diff --exit-code` will determine whether the changelog needs to be changed.

```
Usage: cardano-ledger-release-tool changelogs
         [(-i|--inplace) | (-o|--output FILE)] [-b|--bullets CHARS]
         CHANGELOG ...

  Parse and lint changelog files

Available options:
  -h,--help                Show this help text
  -i,--inplace             Modify files in-place
  -o,--output FILE         Write output to FILE
  -b,--bullets CHARS       Use CHARS for the levels of bullets (default: *-+)
  CHANGELOG ...            Changelog files to process
```
