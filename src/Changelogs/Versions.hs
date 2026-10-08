{-# LANGUAGE OverloadedStrings #-}

{- | Whether a package's cabal version agrees with its @CHANGELOG.md@.

Per @RELEASING.md@, a developer introducing a change bumps the version in both the
@CHANGELOG.md@ and the @.cabal@ file, in the same PR.

The @CHANGELOG.md@ is the authority on the intended version. The developer chooses whether a
change deserves a patch, minor or major bump and records that as the top section, while
@release post@ only ever sets the floor. The cabal version therefore follows the
@CHANGELOG.md@ and must never run ahead of it.

The two are meant to disagree in exactly one state, the one @release post@ creates right after
a release: an empty placeholder section (a lone @*@) on top, with the cabal version left behind
on the released version. That lag is not slack to be taken up, it is required. The placeholder
only reserves the next version number, it does not claim that the version is worth releasing,
and @release check@ offers for release every package whose cabal version is ahead of the one on
CHaP. Moving the cabal version onto an empty section therefore queues up a release with nothing
to show for it. Whatever makes a version releasable, a dependency bounds bump included, earns an
entry in the @CHANGELOG.md@.

The lag can span more than one section when several releases happened without any change in
between.

Only agreement between the two files is checked, not the magnitude of the bump.
-}
module Changelogs.Versions (
  VersionProblem (..),
  checkVersion,
  describeProblem,
  describeChangelogError,
) where

import Changelogs.Core (Changelog, ChangelogError (..), Unreleased (..), releaseVersions, topRelease)
import Common.Version (showVersionText)
import Data.List (find)
import Data.Text (Text)
import Data.Version (Version)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

-- | A way in which a package's cabal version disagrees with its @CHANGELOG.md@.
data VersionProblem
  = {- | A @CHANGELOG.md@ without a single version section leaves nothing to compare the cabal
    version against, so it is a malformed file rather than a package to skip.
    -}
    NoVersionHeading
  | {- | Invariant 1: the top section holds the highest version of all the sections, which is
    to say the sections are in descending order. The cabal version plays no part in this one.

    Versions compare numerically, component by component, so @1.10.0.0@ is above @1.2.3.1@; a
    textual comparison would get this wrong by comparing the @10@ in @1.10@ one character at a
    time.

    Holds the top section's version and a higher one found below it.
    -}
    TopNotHighest Version Version
  | {- | Invariant 2: the cabal version does not exceed the top section's version. The
    @CHANGELOG.md@ decides the intended version, so a cabal version that is ahead of it is
    always wrong.

    Holds the cabal version and the top section's version, which has entries.
    -}
    CabalAhead Version Version
  | {- | Invariant 2 again, when the top section is an empty placeholder. Setting the cabal
    version to the top section would only trade this problem for 'CabalOnPlaceholder', so the
    fix is to record the change in a section for the cabal version, or to set the cabal version
    back to the released version.

    Holds the cabal version and the top section's version, which has no entries.
    -}
    CabalAheadOfPlaceholder Version Version
  | {- | Invariant 3: the cabal version appears as some section in the @CHANGELOG.md@.

    Holds the cabal version.
    -}
    CabalNotInChangelog Version
  | {- | Invariant 4: if the top section has entries, the cabal version matches it.

    By now the cabal version is known to be at or below the top section, so a mismatch always
    means the cabal file is the one lagging behind. Entries under the top section mean the
    developer settled on that version, so the cabal file has to catch up. With a cabal version
    of @1.2.0.0@:

    > ## 1.3.0.0      <- an error, the cabal version must be bumped to 1.3.0.0
    > * Add `foo`

    A lone @*@ is instead the placeholder that @release post@ leaves behind after a release, and
    the cabal version is supposed to stay on the released version until something is actually
    added. Again with a cabal version of @1.2.0.0@:

    > ## 1.3.0.0      <- fine, nothing has been added since 1.2.0.0 was released
    > *
    >
    > ## 1.2.0.0
    > * Add `foo`

    Holds the cabal version and the top section's version.
    -}
    CabalBehind Version Version
  | {- | Invariant 5: if the top section has no entries, the cabal version stays below it.

    The mirror of 'CabalBehind'. An empty top section only reserves the next version number, so
    the cabal version has to stay on the released version below it; otherwise @release check@
    ships a version whose section records no change. With a cabal version of @1.3.0.0@:

    > ## 1.3.0.0      <- an error, nothing is recorded under 1.3.0.0
    > *
    >
    > ## 1.2.0.0
    > * Add `foo`

    The fix is whichever of the two matches what actually happened: leave the cabal version on
    @1.2.0.0@ if nothing has changed since it was released, or record the change under
    @1.3.0.0@ if something has. A dependency bounds bump is the common way to land here and takes
    the second route: it is a real reason to release the package, so it earns a real entry.

    Holds the cabal version, which is also the top section's version.
    -}
    CabalOnPlaceholder Version
  deriving (Eq, Show)

{- | Check a package's cabal version against its @CHANGELOG.md@.

Five invariants are checked, in this order so that the most specific diagnostic wins:

1. The top section holds the highest version of all the sections ('TopNotHighest').
2. The cabal version does not exceed the top section's version ('CabalAhead', or
   'CabalAheadOfPlaceholder' when the top section has no entries).
3. The cabal version appears as some section in the @CHANGELOG.md@ ('CabalNotInChangelog').
4. If the top section has entries, the cabal version matches it ('CabalBehind').
5. If the top section has no entries, the cabal version stays below it ('CabalOnPlaceholder').
-}
checkVersion :: Version -> Changelog -> Maybe VersionProblem
checkVersion cabal changelog = case topRelease changelog of
  Nothing -> Just NoVersionHeading
  Just Unreleased {unreleasedVersion = top, unreleasedIsEmpty = isEmpty}
    | Just higher <- find (> top) versions -> Just $ TopNotHighest top higher
    | cabal > top && isEmpty -> Just $ CabalAheadOfPlaceholder cabal top
    | cabal > top -> Just $ CabalAhead cabal top
    | cabal `notElem` versions -> Just $ CabalNotInChangelog cabal
    | not isEmpty && cabal /= top -> Just $ CabalBehind cabal top
    | isEmpty && cabal == top -> Just $ CabalOnPlaceholder cabal
    | otherwise -> Nothing
 where
  versions = releaseVersions changelog

-- | Describe a problem, given the path of the @CHANGELOG.md@ to show to the user.
describeProblem :: FilePath -> VersionProblem -> Text
describeProblem path problem = case problem of
  NoVersionHeading ->
    "no version heading in " <> cl
  TopNotHighest top higher ->
    "top section of " <> cl <> " is " <> v top <> ", but a later section has a higher version, " <> v higher
  CabalAhead cabal top ->
    "cabal version "
      <> v cabal
      <> " is ahead of "
      <> cl
      <> ", whose most recent section is "
      <> v top
      <> " - set the cabal version to "
      <> v top
      <> ", or add the missing section"
  CabalAheadOfPlaceholder cabal top ->
    "cabal version "
      <> v cabal
      <> " is ahead of "
      <> cl
      <> ", whose most recent section "
      <> v top
      <> " has no entries - add a section for "
      <> v cabal
      <> " recording the change, or set the cabal version back to the version that was released"
  CabalNotInChangelog cabal ->
    "cabal version " <> v cabal <> " has no section in " <> cl
  CabalBehind cabal top ->
    "cabal version is "
      <> v cabal
      <> ", but "
      <> cl
      <> " has entries under "
      <> v top
      <> " - bump the cabal version to "
      <> v top
  CabalOnPlaceholder cabal ->
    "cabal version is "
      <> v cabal
      <> ", but "
      <> cl
      <> " has no entries under "
      <> v cabal
      <> " - either record the change under "
      <> v cabal
      <> ", or leave the cabal version on the version that was released"
 where
  cl = T.pack path
  v = showVersionText

{- | Describe why a @CHANGELOG.md@ could not be read, given its path to show to the user.
Returns 'Nothing' for a missing file, because callers decide what that means.
-}
describeChangelogError :: FilePath -> ChangelogError -> Maybe Text
describeChangelogError path err = case err of
  ChangelogMissing -> Nothing
  ChangelogUnreadable reason -> Just $ "could not read " <> cl <> ": " <> TL.toStrict reason
  ChangelogUnparseable _ -> Just $ "could not parse " <> cl <> " (see `cleret changelogs format`)"
 where
  cl = T.pack path
