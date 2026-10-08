{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Release.Command (subcmd) where

import Changelogs.Core (ChangelogError (..), ensureEmptyRelease, readChangelog, writeChangelog)
import Changelogs.Versions (describeChangelogError)
import Common.Color (printBold, printError, printInfo, printSuccess, printWarning)
import Common.Git
import Common.Options (subparsers)
import Common.Packages (Package (..), PackageName (..), describeCabalFileError, isLocalOnly, scanPackages)
import Common.Version (bumpPatch, showVersionText)
import Control.Monad (unless)
import Data.Either (partitionEithers)
import Data.Foldable (for_)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Traversable (for)
import Data.Version (Version, showVersion)
import Options.Applicative
import Release.Chap (
  PackageStatus (..),
  ReleaseInfo (..),
  classifyPackage,
  ensureChapClone,
  getChapVersion,
  readChapMeta,
 )
import Release.Meta (ChapMeta (..), ChapSource (..))
import Release.Tags
import System.Exit (exitFailure)
import System.FilePath (makeRelative, (</>))
import System.IO (hIsTerminalDevice, stdout)
import Text.Printf (printf)

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T

subcmd :: Mod CommandFields (IO ())
subcmd =
  command "release" $
    info
      ( helper
          <*> subparsers
            [ checkCmd
            , postCmd
            ]
      )
      (progDesc "Release workflow for CHaP publishing")

-- ---------------------------------------------------------------------------
-- release check
-- ---------------------------------------------------------------------------

data CheckOptions = CheckOptions
  { checkDryRun :: Bool
  , checkRepoUrl :: Maybe String
  }

checkCmd :: Mod CommandFields (IO ())
checkCmd =
  command "check" $
    info
      ( helper <*> do
          checkDryRun <-
            switch $
              help "Use local git tags instead of querying CHaP API"
                <> long "dry-run"
          checkRepoUrl <-
            optional . strOption $
              help "Override repository URL (auto-detected from git remote)"
                <> long "repo-url"
                <> metavar "URL"
          pure $ runCheck CheckOptions {..}
      )
      ( fullDesc
          <> progDesc "Identify packages needing release and output the CHaP command"
      )

runCheck :: CheckOptions -> IO ()
runCheck CheckOptions {..} = do
  root <- repoRoot
  packages <- scanReleasePackages root
  useColor <- hIsTerminalDevice stdout

  ensureLatestTrunk
  if checkDryRun
    then printInfo "Using local git tags for version comparison (dry-run mode)"
    else ensureChapClone
  putStrLn ""

  -- Gather release info for each package
  infos <- traverse (gatherInfo root checkDryRun) packages

  -- Print table
  printBold $ padRight 30 "Package" <> padRight 12 "Cabal" <> padRight 12 "Released" <> padRight 15 "Status"
  T.putStrLn (T.replicate 69 "─")

  mapM_ (printRow useColor) infos
  putStrLn ""

  -- Print warnings
  let allWarnings = concatMap (\ri -> map ((unPackageName (pkgName (riPackage ri)) <> ": ") <>) (riWarnings ri)) infos
  if null allWarnings
    then pure ()
    else do
      printBold "Errors:"
      mapM_ (\w -> printError ("  " <> w)) allWarnings
      putStrLn ""
      exitFailure

  -- Summary
  let ready = filter (\ri -> riStatus ri `elem` [StatusNew, StatusReady]) infos
  if null ready
    then printInfo "All packages are up to date. Nothing to release."
    else do
      printBold $ T.pack (show (length ready)) <> " package(s) ready for release."
      putStrLn ""

      -- Generate CHaP command
      fullCommit <- getFullCommit "HEAD"
      repoUrl <- detectRepoUrl checkRepoUrl

      printBold "CHaP command (run from cardano-haskell-packages repo):"
      putStrLn ""
      putStr $ "./scripts/add-from-github.sh " <> repoUrl <> " " <> fullCommit <> " \\"
      putStrLn ""

      -- CHaP's add-from-github.sh expects repo-relative package directories
      -- (e.g. "eras/conway/impl"), not package names. pkgDir already holds the
      -- absolute package directory, so makeRelative against the repo root gives
      -- exactly the subdir the script (and each meta.toml) records.
      let readySubdirs = map (makeRelative root . pkgDir . riPackage) ready
          lastIdx = length readySubdirs - 1
      mapM_
        ( \(i, subdir) ->
            if i == lastIdx
              then putStrLn $ "  " <> subdir
              else putStrLn $ "  " <> subdir <> " \\"
        )
        (zip [0 ..] readySubdirs)

      putStrLn ""
      printInfo "After the CHaP PR is merged, run: cleret release post"

{- | Find the packages under the repository root that take part in releases.
Exits if any .cabal file could not be read, before anything is tagged or
printed.
-}
scanReleasePackages :: FilePath -> IO [Package]
scanReleasePackages root = do
  (cabalErrors, packages) <- scanPackages root
  unless (null cabalErrors) $ do
    mapM_ (printError . describeCabalFileError root) cabalErrors
    exitFailure
  pure $ filter (not . isLocalOnly) packages

{- | Fetch origin and insist that HEAD is exactly the tip of its default
branch. @release check@ prints a command that publishes HEAD, and
@release post@ bumps changelogs that may already have been bumped upstream;
from a stale checkout both would report work that is not really pending.
-}
ensureLatestTrunk :: IO ()
ensureLatestTrunk = do
  printInfo "Fetching origin..."
  fetched <- fetchOrigin
  case fetched of
    Left err -> do
      printError $ "git fetch origin failed: " <> T.pack err
      exitFailure
    Right () -> pure ()

  branch <- defaultBranch
  let trunk = "origin/" <> branch
  position <- compareWithRef trunk
  case position of
    Left err -> do
      printError $ "could not compare HEAD with " <> T.pack trunk <> ": " <> T.pack err
      exitFailure
    Right (0, 0) -> pure ()
    Right (behind, ahead) -> do
      printError $
        "HEAD is "
          <> describePosition behind ahead
          <> " "
          <> T.pack trunk
          <> "; this command must run from the latest "
          <> T.pack branch
          <> ". "
          <> (if ahead == 0 then "Run 'git pull --ff-only' and re-run." else "Check it out and re-run.")
      exitFailure
 where
  describePosition behind ahead =
    T.intercalate
      " and "
      ( [T.pack (show behind) <> " commit(s) behind" | behind > 0]
          <> [T.pack (show ahead) <> " commit(s) ahead of" | ahead > 0]
      )

-- | Gather release information for a single package.
gatherInfo :: FilePath -> Bool -> Package -> IO ReleaseInfo
gatherInfo root dryRun pkg = do
  releasedVersion <-
    if dryRun
      then getTagVersion (pkgName pkg)
      else getChapVersion (pkgName pkg)

  let changelogPath = pkgDir pkg </> "CHANGELOG.md"
  changelog <- readChangelog changelogPath

  pure $ classifyPackage (makeRelative root changelogPath) pkg releasedVersion changelog

-- | Detect the repo URL from git remote or use the override.
detectRepoUrl :: Maybe String -> IO String
detectRepoUrl (Just url) = pure url
detectRepoUrl Nothing = do
  mUrl <- getRemoteUrl
  case mUrl of
    Just url -> pure url
    Nothing -> do
      printError "Could not detect repository URL. Use --repo-url to specify."
      exitFailure

-- | Print a table row for a ReleaseInfo.
printRow :: Bool -> ReleaseInfo -> IO ()
printRow useColor ReleaseInfo {..} = do
  let pkg = riPackage
      name = unPackageName (pkgName pkg)
      cabalVer = T.pack $ showVersion (pkgVersion pkg)
      relVer = maybe "-" (T.pack . showVersion) riReleasedVersion
      (statusText, statusColor) = case riStatus of
        StatusNew -> ("● New", if useColor then "\ESC[0;32m" else "") :: (String, String)
        StatusReady -> ("✓ Ready", if useColor then "\ESC[0;32m" else "")
        StatusNeedsBump -> ("⚠ Bump cabal", if useColor then "\ESC[0;33m" else "")
        StatusUpToDate -> ("- Up to date", "")
      resetCode = if null statusColor then "" else "\ESC[0m" :: String
  printf
    "%-30s %-12s %-12s %s%-15s%s\n"
    (T.unpack name)
    (T.unpack cabalVer)
    (T.unpack relVer)
    statusColor
    statusText
    resetCode

-- | Pad a text to a given width on the right.
padRight :: Int -> Text -> Text
padRight n t
  | T.length t >= n = t
  | otherwise = t <> T.replicate (n - T.length t) " "

-- ---------------------------------------------------------------------------
-- release post
-- ---------------------------------------------------------------------------

data PostOptions = PostOptions
  { postApply :: Bool
  , postPush :: Bool
  }

postCmd :: Mod CommandFields (IO ())
postCmd =
  command "post" $
    info
      ( helper <*> do
          postApply <-
            switch $
              help "Actually execute changes (default is dry-run)"
                <> long "apply"
          postPush <-
            switch $
              help "Push tags to origin (only under --apply; otherwise the git push commands are printed)"
                <> long "push"
          pure $ runPost PostOptions {..}
      )
      ( fullDesc
          <> progDesc "Create tags, bump changelogs, and create follow-up PR after CHaP release"
      )

runPost :: PostOptions -> IO ()
runPost PostOptions {..} = do
  let dryRun = not postApply

  root <- repoRoot
  packages <- scanReleasePackages root

  when dryRun $ do
    printBold "DRY RUN - No changes will be made (use --apply to execute)"
    putStrLn ""

  when (dryRun && postPush) $ do
    printWarning "--push is ignored in dry-run mode (only effective under --apply)"
    putStrLn ""

  ensureLatestTrunk
  ensureChapClone
  putStrLn ""

  -- Find all packages published on CHaP
  chapPackages <- findChapPackages packages

  if null chapPackages
    then printInfo "No packages found on CHaP."
    else do
      -- Work out, per package, the commit CHaP recorded and what to do about
      -- its tag. Every problem is reported before anything is changed; the
      -- dry run shows the same analysis.
      (errors, plan) <- planTags chapPackages
      unless (null errors) $ do
        printBold "Errors:"
        mapM_ (\e -> printError ("  " <> e)) errors
        putStrLn ""
        exitFailure

      applyTagPlan dryRun postPush plan
      putStrLn ""

      -- Add an empty changelog entry above the last published version for every
      -- package on CHaP. Packages that already have an unreleased entry above
      -- their CHaP version (stragglers, or work in progress) are skipped, so in
      -- practice this only touches packages whose released version still sits at
      -- the top of the changelog — i.e. exactly the ones just published.
      printBold "Bumping changelogs..."
      allUpdated <- bumpChangelogs dryRun chapPackages
      putStrLn ""

      -- The changed files are left in the working tree: committing them on a
      -- branch and opening the PR is the user's call.
      if null allUpdated
        then printInfo "Everything is up to date."
        else
          if dryRun
            then printBold "DRY RUN complete. Run with --apply to execute."
            else do
              printBold "Changelogs updated. To open the follow-up PR, run:"
              putStrLn "  git checkout -b post-release"
              putStrLn $ "  git add " <> unwords (map (makeRelative root) allUpdated)
              putStrLn "  git commit -m 'Post-release: bump changelogs'"
              putStrLn "  git push -u origin post-release"
              putStrLn "  gh pr create --head post-release"

-- | Find all packages that have a version published on CHaP.
findChapPackages :: [Package] -> IO [(Package, Version)]
findChapPackages packages = do
  results <- traverse checkPackage packages
  pure $ concat results
 where
  checkPackage pkg = do
    mChapVer <- getChapVersion (pkgName pkg)
    case mChapVer of
      Nothing -> pure []
      Just chapVer -> pure [(pkg, chapVer)]

-- | A package version published on CHaP, with the commit CHaP recorded for it.
data ReleasedPackage = ReleasedPackage
  { rpPackage :: Package
  , rpVersion :: Version
  , rpMeta :: ChapMeta
  , rpCommit :: String
  -- ^ The full SHA that CHaP's recorded @rev@ resolves to in this repository.
  }

rpTag :: ReleasedPackage -> String
rpTag rp = tagName (pkgName (rpPackage rp)) (rpVersion rp)

{- | Decide what to do about every package's tag. Reads the commit CHaP
recorded for each package, resolves it (origin has just been fetched, so a
commit that is still unknown is an error), checks that the package really
lived where CHaP says it did at that commit, and compares the existing tags,
local and on origin, against it. Returns all problems found, as messages, and
the action for each package that had none.
-}
planTags :: [(Package, Version)] -> IO ([Text], [(ReleasedPackage, TagAction)])
planTags chapPackages = do
  (metaErrors, recorded) <- partitionEithers <$> for chapPackages readRecorded
  (revErrors, released) <- partitionEithers <$> for recorded resolveRecorded
  (subdirErrors, verified) <- partitionEithers <$> for released checkSubdir

  remote <- lsRemoteTags
  case remote of
    Left err ->
      pure (metaErrors ++ revErrors ++ subdirErrors ++ ["could not list tags on origin: " <> T.pack err], [])
    Right out -> do
      (tagErrors, decided) <- partitionEithers <$> for verified (decide (parseRemoteTags out))
      pure (metaErrors ++ revErrors ++ subdirErrors ++ concat tagErrors, decided)

{- | Read what CHaP recorded for a package version. Which repository CHaP
names is CHaP's business; the commit check in 'checkSubdir' is what proves
the record matches this repository.
-}
readRecorded :: (Package, Version) -> IO (Either Text (Package, Version, ChapMeta, Text))
readRecorded (pkg, ver) = do
  let tag = T.pack (tagName (pkgName pkg) ver)
  result <- readChapMeta (pkgName pkg) ver
  pure $ case result of
    Left err -> Left $ tag <> ": " <> err
    Right meta -> case metaSource meta of
      UrlSource url ->
        Left $ tag <> ": CHaP records a tarball source (" <> url <> "), so there is no commit to tag"
      GitHubSource _ rev -> Right (pkg, ver, meta, rev)

-- | Resolve the recorded commit to a full SHA in this repository.
resolveRecorded :: (Package, Version, ChapMeta, Text) -> IO (Either Text ReleasedPackage)
resolveRecorded (pkg, ver, meta, rev) = do
  result <- resolveCommit (T.unpack rev)
  pure $ case result of
    Right sha -> Right ReleasedPackage {rpPackage = pkg, rpVersion = ver, rpMeta = meta, rpCommit = sha}
    Left err ->
      Left $
        T.pack (tagName (pkgName pkg) ver)
          <> ": commit "
          <> rev
          <> " recorded on CHaP is not in this repository, even after fetching origin ("
          <> T.pack err
          <> ")"

{- | Check that the package's .cabal file exists, at the recorded commit, in
the directory CHaP recorded. That proves CHaP built this package from that
commit, and keeps working if the package has since moved.
-}
checkSubdir :: ReleasedPackage -> IO (Either Text ReleasedPackage)
checkSubdir rp = do
  let cabal = T.unpack (unPackageName (pkgName (rpPackage rp))) <> ".cabal"
      path = maybe cabal (</> cabal) (metaSubdir (rpMeta rp))
  exists <- pathExistsAtCommit (rpCommit rp) path
  pure $
    if exists
      then Right rp
      else
        Left $
          T.pack (rpTag rp)
            <> ": no "
            <> T.pack path
            <> " at commit "
            <> T.pack (take 7 (rpCommit rp))
            <> ", so CHaP's record does not match this package"

-- | Compare the local tag and the tag on origin against the recorded commit.
decide :: Map String String -> ReleasedPackage -> IO (Either [Text] (ReleasedPackage, TagAction))
decide remoteTags rp = do
  let tag = rpTag rp
      remote = maybe TagAbsent TagAt (Map.lookup tag remoteTags)
  local <- maybe TagAbsent TagAt <$> tagTarget tag
  pure $ case decideTag (rpCommit rp) local remote of
    Right tagAction -> Right (rp, tagAction)
    Left problems ->
      Left $ map (describeTagProblem tag (rpCommit rp) (remote == TagAt (rpCommit rp))) problems

-- | Carry out (or, in a dry run, describe) the tag actions.
applyTagPlan :: Bool -> Bool -> [(ReleasedPackage, TagAction)] -> IO ()
applyTagPlan dryRun doPush plan = do
  let withAction a = [rp | (rp, a') <- plan, a' == a]
      fetches = withAction TagFetch
      creates = withAction TagCreate
      toPush = creates ++ withAction TagPushOnly

  if null fetches && null toPush
    then printInfo "All CHaP versions are already tagged at the commits CHaP recorded."
    else do
      unless (null fetches) $ do
        printBold "Fetching tags origin already has at the recorded commit..."
        for_ fetches $ \rp ->
          if dryRun
            then putStrLn $ "  [dry-run] Would fetch tag: " <> rpTag rp
            else do
              fetchTag (rpTag rp)
              printSuccess (T.pack (rpTag rp))
        putStrLn ""

      for_ (NE.groupAllWith rpCommit creates) $ \group -> do
        printBold $
          "Creating tags at "
            <> T.pack (take 7 (rpCommit (NE.head group)))
            <> " ("
            <> T.pack (show (length group))
            <> " package(s))..."
        for_ group (createTagFor dryRun)
        putStrLn ""

      -- Push tags (or print the commands to do so, unless --push was given).
      -- Tags that were fetched are already on origin.
      unless (null toPush) $
        if dryRun
          then do
            printBold "Pushing tags to origin..."
            putStrLn $ "  [dry-run] Would push " <> show (length toPush) <> " tags"
          else
            if doPush
              then do
                printBold "Pushing tags to origin..."
                mapM_ (pushTag . rpTag) toPush
                printSuccess $ "Pushed " <> T.pack (show (length toPush)) <> " tags"
              else do
                printBold "Tags created locally. To push them, run:"
                mapM_ (\rp -> putStrLn $ "  git push origin " <> rpTag rp) toPush

-- | Create a package's tag at the commit CHaP recorded.
createTagFor :: Bool -> ReleasedPackage -> IO ()
createTagFor dryRun rp =
  if dryRun
    then putStrLn $ "  [dry-run] Would create tag: " <> rpTag rp
    else do
      createTag (rpTag rp) (rpCommit rp)
      printSuccess (T.pack (rpTag rp))

-- | Add empty changelog entries for the given packages. Returns updated paths.
bumpChangelogs :: Bool -> [(Package, Version)] -> IO [FilePath]
bumpChangelogs dryRun pkgs = do
  results <- traverse (bumpChangelog dryRun) pkgs
  pure $ concat results

-- | Bump a single package's changelog. Returns the filepath if updated.
bumpChangelog :: Bool -> (Package, Version) -> IO [FilePath]
bumpChangelog dryRun (pkg, releasedVer) = do
  let changelogPath = pkgDir pkg <> "/CHANGELOG.md"
      nextVer = bumpPatch releasedVer

  result <- readChangelog changelogPath
  case result of
    Left ChangelogMissing -> do
      printWarning $ "No CHANGELOG.md for " <> unPackageName (pkgName pkg) <> ", skipping"
      pure []
    Left err -> do
      mapM_
        (\message -> printWarning $ unPackageName (pkgName pkg) <> ": " <> message <> ", skipping")
        (describeChangelogError changelogPath err)
      pure []
    Right cl ->
      case ensureEmptyRelease nextVer cl of
        Nothing -> do
          -- The changelog is already ahead of the last published version. Skip
          -- it unless it has uncommitted changes — e.g. written by an earlier
          -- run that failed before committing — so the commit still picks it up.
          dirty <- hasUncommittedChanges changelogPath
          if not dirty
            then pure []
            else do
              if dryRun
                then putStrLn $ "  [dry-run] Would include uncommitted change: " <> changelogPath
                else putStrLn $ "  " <> T.unpack (unPackageName (pkgName pkg)) <> ": already up to date, including uncommitted change"
              pure [changelogPath]
        Just cl'
          | dryRun -> do
              putStrLn $ "  [dry-run] Would update " <> changelogPath <> " → " <> showVersion nextVer
              pure [changelogPath]
          | otherwise -> do
              writeChangelog changelogPath "*-+" cl'
              printSuccess $ T.pack changelogPath <> " → " <> showVersionText nextVer
              pure [changelogPath]

-- Utility
when :: Bool -> IO () -> IO ()
when True act = act
when False _ = pure ()
