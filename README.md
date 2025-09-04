# cardano-ledger-release-tool

Utilities for managing releases of [cardano-ledger](https://github.com/IntersectMBO/cardano-ledger)

The utilities provided are:

* [changelogs](changelogs): Parses and re-renders a changelog, as a form of linting; the output is the canonical representation of the changelog. A `git diff --exit-code` will show whether the changelog needs to be changed.
