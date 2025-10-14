# Release Process for `cardano-ledger-release-tool`

1. Bump the version in the Cabal file

2. Add a new section to `CHANGELOG.md`

3. Push the changes to a new PR

4. Merge the PR

5. Create an annotated tag on the tip of `main`

6. Push the tag

7. Check and publish the draft release that CI will create

Ideally, tags should also be signed.
