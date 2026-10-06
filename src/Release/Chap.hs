{-# LANGUAGE OverloadedStrings #-}

module Release.Chap (
  PackageStatus (..),
  ReleaseInfo (..),
  ensureChapClone,
  getChapVersion,
  readChapMeta,
  classifyPackage,
) where

import Changelogs.Core (Changelog, ChangelogError (..), Unreleased (..), topRelease)
import Changelogs.Versions (checkVersion, describeChangelogError, describeProblem)
import Common.Color (printError, printInfo, printWarning)
import Common.Packages (Package (..), PackageName (..))
import Common.Version (parseVersionText, showVersionText)
import Data.Bifunctor (first)
import Data.Maybe (mapMaybe, maybeToList)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import Data.Version (Version, showVersion)
import Release.Meta (ChapMeta, parseChapMeta)
import System.Directory (doesDirectoryExist, listDirectory, removeDirectoryRecursive)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath ((</>))
import System.Process (callProcess, readProcessWithExitCode)
import UnliftIO.Exception (Exception, displayException, tryAny)

import qualified Data.ByteString as BS
import qualified Data.Text as T

-- Note: we need to store a local copy of CHaP so that we can always quickly
-- query the versions. Querying GitHub doesn't work because they have very
-- strict limits on the amounts of queries per hour, and using a token for this
-- denies the convenience of this setup. A clone is cheap, fast, and local.
chapDir :: FilePath
chapDir = "/tmp/cardano-haskell-packages"

{- | Ensure a shallow clone of CHaP exists at 'chapDir'. Pulls latest if already cloned.
  Detects and recovers from corrupted clones (e.g. macOS /tmp cleanup).
-}
ensureChapClone :: IO ()
ensureChapClone = do
  dirExists <- doesDirectoryExist chapDir
  isRepo <-
    if dirExists
      then do
        (ec, _, _) <- readProcessWithExitCode "git" ["-C", chapDir, "rev-parse", "--git-dir"] ""
        pure (ec == ExitSuccess)
      else pure False

  case (dirExists, isRepo) of
    (True, True) -> do
      printInfo "Updating local CHaP copy..."
      (exitCode, _, err) <- readProcessWithExitCode "git" ["-C", chapDir, "pull", "--ff-only"] ""
      case exitCode of
        ExitSuccess -> pure ()
        ExitFailure _ -> do
          printError $
            "could not update the CHaP clone at "
              <> T.pack chapDir
              <> ": "
              <> T.strip (T.pack err)
              <> " - delete it to re-clone"
          exitFailure
    (True, False) -> do
      printWarning "CHaP clone is corrupted, re-cloning..."
      removeDirectoryRecursive chapDir
      cloneChap
    _ ->
      cloneChap

-- | Clone CHaP from scratch.
cloneChap :: IO ()
cloneChap = do
  printInfo "Cloning CHaP locally..."
  (exitCode, _, err) <-
    readProcessWithExitCode
      "git"
      [ "clone"
      , "--depth"
      , "1"
      , "--filter=blob:none"
      , "--sparse"
      , "https://github.com/IntersectMBO/cardano-haskell-packages.git"
      , chapDir
      ]
      ""
  case exitCode of
    ExitSuccess -> do
      callProcess "git" ["-C", chapDir, "sparse-checkout", "set", "_sources"]
      printInfo "CHaP clone ready."
    ExitFailure _ -> do
      printError $ "Failed to clone CHaP: " <> T.pack err
      exitFailure

-- | Look up the latest version of a package on CHaP.
getChapVersion :: PackageName -> IO (Maybe Version)
getChapVersion (PackageName pkg) = do
  let sourcesDir = chapDir <> "/_sources/" <> T.unpack pkg
  exists <- doesDirectoryExist sourcesDir
  if not exists
    then pure Nothing
    else do
      entries <- listDirectory sourcesDir
      let versions = mapMaybe (parseVersionText . T.pack) entries
      pure $ case versions of
        [] -> Nothing
        vs -> Just (maximum vs)

{- | Read what CHaP recorded about a package version: the commit it was built
from and where the package lived. Messages name the file relative to the
clone.
-}
readChapMeta :: PackageName -> Version -> IO (Either Text ChapMeta)
readChapMeta (PackageName pkg) ver = do
  let relPath = "_sources" </> T.unpack pkg </> showVersion ver </> "meta.toml"
  result <- tryAny $ BS.readFile (chapDir </> relPath)
  pure $ do
    bytes <- first (unreadable relPath) result
    content <- first (unreadable relPath) (decodeUtf8' bytes)
    first ((T.pack relPath <> ": ") <>) (parseChapMeta content)
 where
  unreadable :: Exception e => FilePath -> e -> Text
  unreadable relPath e = "could not read " <> T.pack relPath <> ": " <> T.pack (displayException e)

-- ---------------------------------------------------------------------------
-- Release classification
-- ---------------------------------------------------------------------------

data PackageStatus
  = StatusNew
  | StatusReady
  | -- | Changelog has real unreleased changes ahead of CHaP, but the cabal wasn't bumped to match.
    StatusNeedsBump
  | StatusUpToDate
  deriving (Eq, Show)

data ReleaseInfo = ReleaseInfo
  { riPackage :: Package
  , riReleasedVersion :: Maybe Version
  , riChangelogVersion :: Maybe Version
  , riStatus :: PackageStatus
  , riWarnings :: [Text]
  }
  deriving (Show)

{- | Classify a package's release status by comparing the cabal version, the
version published on CHaP, and the top (unreleased) changelog entry. The
'FilePath' is the path of the changelog to show to the user.
-}
classifyPackage ::
  FilePath ->
  Package ->
  Maybe Version ->
  Either ChangelogError Changelog ->
  ReleaseInfo
classifyPackage changelogPath pkg releasedVersion changelog =
  ReleaseInfo
    { riPackage = pkg
    , riReleasedVersion = releasedVersion
    , riChangelogVersion = changelogVersion
    , riStatus = status
    , riWarnings = warnings
    }
 where
  cabalVer = pkgVersion pkg
  mUnreleased = either (const Nothing) topRelease changelog
  changelogVersion = unreleasedVersion <$> mUnreleased

  -- The changelog's top entry describes an actual, not-yet-released change: it
  -- has content (not the empty placeholder that @release post@ leaves behind)
  -- and its version is ahead of what's published on CHaP.
  changelogAheadOf released = case mUnreleased of
    Just u -> not (unreleasedIsEmpty u) && unreleasedVersion u > released
    Nothing -> False

  status = case releasedVersion of
    Nothing -> StatusNew
    Just released
      -- Every package whose cabal version is ahead of CHaP is offered for
      -- release. That is why the cabal version must stay on the released
      -- version while the top changelog section is still an empty placeholder
      -- (see "Changelogs.Versions").
      | cabalVer > released -> StatusReady
      -- The changelog moved on but the cabal didn't: real changes are staged
      -- for a new version, yet the cabal still matches what's on CHaP.
      | changelogAheadOf released -> StatusNeedsBump
      | otherwise -> StatusUpToDate

  -- A package we're about to publish: New (never released) or Ready (bumped
  -- past what's on CHaP). Both must have a changelog that agrees with the cabal
  -- version (see "Changelogs.Versions").
  releasable = status `elem` [StatusNew, StatusReady]

  warnings = case status of
    -- The changelog has real changes for a new version, but nobody bumped the
    -- cabal to match. Surface it as needing release and force a fix: CHaP keys
    -- releases off the cabal version, so publishing without the bump is a no-op.
    StatusNeedsBump -> case changelogVersion of
      Just clVer ->
        [ "Changelog has unreleased changes for "
            <> showVersionText clVer
            <> " but the .cabal version is still "
            <> showVersionText cabalVer
            <> " — bump the version field in the .cabal file to "
            <> showVersionText clVer
        ]
      Nothing -> []
    _
      | not releasable -> []
      | otherwise -> case changelog of
          Left ChangelogMissing ->
            [ "No changelog entry found (cabal version is "
                <> showVersionText cabalVer
                <> ")"
            ]
          Left err -> maybeToList $ describeChangelogError changelogPath err
          Right cl -> maybeToList $ describeProblem changelogPath <$> checkVersion cabalVer cl
