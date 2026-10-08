{-# LANGUAGE OverloadedStrings #-}

module Common.Git (
  repoRoot,
  getCurrentCommit,
  getFullCommit,
  resolveCommit,
  pathExistsAtCommit,
  tagName,
  getTagVersion,
  tagTarget,
  createTag,
  pushTag,
  fetchTag,
  fetchOrigin,
  lsRemoteTags,
  defaultBranch,
  compareWithRef,
  hasUncommittedChanges,
  getRemoteUrl,
) where

import Common.Packages (PackageName (..))
import Common.Version (parseVersionText)
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (mapMaybe)
import Data.Version (Version, showVersion)
import System.Exit (ExitCode (..))
import System.Process (callProcess, readProcess, readProcessWithExitCode)

import qualified Data.Text as T

-- | Get the repository root directory.
repoRoot :: IO FilePath
repoRoot = trim <$> readProcess "git" ["rev-parse", "--show-toplevel"] ""

-- | Get short commit SHA for HEAD.
getCurrentCommit :: IO String
getCurrentCommit = trim <$> readProcess "git" ["rev-parse", "--short", "HEAD"] ""

-- | Get full commit SHA for a ref.
getFullCommit :: String -> IO String
getFullCommit ref = trim <$> readProcess "git" ["rev-parse", ref] ""

{- | Resolve a revision to the full SHA of a commit, peeling tags. The 'Left'
is git's own message, which distinguishes an unknown revision from an
ambiguous abbreviated one.
-}
resolveCommit :: String -> IO (Either String String)
resolveCommit rev = do
  (exitCode, out, err) <-
    readProcessWithExitCode "git" ["rev-parse", "--verify", rev <> "^{commit}"] ""
  pure $ case exitCode of
    ExitSuccess -> Right (trim out)
    ExitFailure _ -> Left (trim err)

-- | Check whether a path exists in the tree of a commit.
pathExistsAtCommit :: String -> FilePath -> IO Bool
pathExistsAtCommit commit path = do
  (exitCode, _, _) <- readProcessWithExitCode "git" ["cat-file", "-e", commit <> ":" <> path] ""
  pure $ exitCode == ExitSuccess

-- | The tag that marks a release of a package: @<pkg>-<version>@.
tagName :: PackageName -> Version -> String
tagName (PackageName pkg) ver = T.unpack pkg <> "-" <> showVersion ver

{- | Get latest tag version for a package prefix.
Returns Nothing if no tags exist matching "<pkg>-*".
-}
getTagVersion :: PackageName -> IO (Maybe Version)
getTagVersion (PackageName pkg) = do
  out <- readProcess "git" ["tag", "-l", T.unpack pkg <> "-*"] ""
  let prefix = T.unpack pkg <> "-"
      versions =
        mapMaybe
          ( \tag ->
              case stripPrefix prefix (trim tag) of
                Just rest -> parseVersionText (T.pack rest)
                Nothing -> Nothing
          )
          (lines out)
  pure $ case versions of
    [] -> Nothing
    vs -> Just (maximum vs)

-- | The full SHA of the commit a local tag points to, if the tag exists.
tagTarget :: String -> IO (Maybe String)
tagTarget tag = either (const Nothing) Just <$> resolveCommit ("refs/tags/" <> tag)

-- | Create an annotated tag at a commit.
createTag :: String -> String -> IO ()
createTag tag commit =
  callProcess "git" ["tag", "-a", tag, "-m", tag, commit]

-- | Push a single tag to origin.
pushTag :: String -> IO ()
pushTag tag =
  callProcess "git" ["push", "origin", tag]

-- | Fetch a single tag from origin.
fetchTag :: String -> IO ()
fetchTag tag =
  callProcess "git" ["fetch", "origin", "tag", tag]

{- | Fetch origin. Tags pointing into the fetched history come along; existing
refs are never moved. The 'Left' is git's error output.
-}
fetchOrigin :: IO (Either String ())
fetchOrigin = do
  (exitCode, _, err) <- readProcessWithExitCode "git" ["fetch", "origin"] ""
  pure $ case exitCode of
    ExitSuccess -> Right ()
    ExitFailure _ -> Left (trim err)

{- | The raw output of @git ls-remote --tags origin@, one @<sha>\\t<ref>@ line
per tag (see "Release.Tags" for its shape). The 'Left' is git's error output.
-}
lsRemoteTags :: IO (Either String String)
lsRemoteTags = do
  (exitCode, out, err) <- readProcessWithExitCode "git" ["ls-remote", "--tags", "origin"] ""
  pure $ case exitCode of
    ExitSuccess -> Right out
    ExitFailure _ -> Left (trim err)

-- | Check whether a path has uncommitted changes (staged, unstaged, or untracked).
hasUncommittedChanges :: FilePath -> IO Bool
hasUncommittedChanges path = do
  out <- readProcess "git" ["status", "--porcelain", "--", path] ""
  pure . not . null $ trim out

{- | The default branch of origin, as recorded in @refs/remotes/origin/HEAD@
by clone; @master@ when that ref is missing.
-}
defaultBranch :: IO String
defaultBranch = do
  (exitCode, out, _) <-
    readProcessWithExitCode "git" ["symbolic-ref", "--quiet", "--short", "refs/remotes/origin/HEAD"] ""
  pure $ case exitCode of
    ExitSuccess | Just branch <- stripPrefix "origin/" (trim out) -> branch
    _ -> "master"

{- | How far HEAD is from a ref, as (commits behind, commits ahead). The
'Left' is git's error output, e.g. for an unknown ref.
-}
compareWithRef :: String -> IO (Either String (Int, Int))
compareWithRef ref = do
  (exitCode, out, err) <-
    readProcessWithExitCode "git" ["rev-list", "--left-right", "--count", ref <> "...HEAD"] ""
  pure $ case exitCode of
    ExitSuccess | [behind, ahead] <- words out -> Right (read behind, read ahead)
    ExitSuccess -> Left ("unexpected output from git rev-list: " <> trim out)
    ExitFailure _ -> Left (trim err)

{- | Get the remote URL for origin, normalizing SSH to HTTPS.
Returns Nothing if no remote is configured.
-}
getRemoteUrl :: IO (Maybe String)
getRemoteUrl = do
  (exitCode, out, _) <- readProcessWithExitCode "git" ["remote", "get-url", "origin"] ""
  case exitCode of
    ExitSuccess -> pure . Just . stripDotGit . normalizeUrl . trim $ out
    ExitFailure _ -> pure Nothing
 where
  normalizeUrl url
    | "git@" `isPrefixOf` url =
        "https://" <> map (\c -> if c == ':' then '/' else c) (drop 4 url)
    | otherwise = url
  stripDotGit url
    | ".git" `isSuffixOf'` url = take (length url - 4) url
    | otherwise = url
  isSuffixOf' suffix str = drop (length str - length suffix) str == suffix

-- | Strip trailing whitespace/newlines from process output.
trim :: String -> String
trim = reverse . dropWhile (`elem` (" \n\r\t" :: String)) . reverse
