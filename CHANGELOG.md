# Changelog

Version history for `cardano-ledger-release-tool`

## 0.4.0.0

* Add a `failures` group of subcommands

## 0.3.0.0

* Add a `nix` group of subcommands

## 0.2.0.2

* Fix a bug in the GH action that installs cleret

## 0.2.0.1

* Add a reusable GH action that installs cleret

## 0.2.0.0

* Add a `cabal` group of subcommands
* Add a `workflow` group of subcommands
* Make the `changelogs` subcommand a group
* Rename the executable to `cleret`
* Add `-V` as a short-option form of `--version`

## 0.1.1.0

* Add a `--version` option
* Shorten executable name to `clrt`

### changelogs

* Improve error reporting
  - Catch and report IO exceptions
  - Exit with a failure if exceptions or errors occurred
* Stop appending an extra blank line when writing to stdout

## 0.1.0.0

* Restructure as a single `cardano-ledger-release-tool` app with subcommands

## 0.0.0.0

### changelogs

* Initial release of the `changelogs` app
