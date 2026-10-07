{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Changelogs.Command (subcmd) where

import Changelogs.CheckVersions (checkVersion, describeChangelogError, describeProblem)
import Changelogs.Core (ChangelogError (..), parseChangelog, readChangelog, renderChangelog)
import Common.Options (Options (..), options, subparsers)
import Common.Packages (Package (..), PackageName (..), describeCabalFileError, isLocalOnly, scanPackages)
import Control.Monad (unless, when, (<=<))
import Data.Bitraversable (bitraverse)
import Data.Char (isSpace)
import Data.Either (isLeft)
import Data.Foldable (for_)
import Data.List (dropWhileEnd)
import Data.Maybe (catMaybes)
import Data.Text.Lazy (Text, unpack)
import Data.Traversable (for)
import Options.Applicative
import System.Directory (doesDirectoryExist)
import System.Exit (exitFailure)
import System.FilePath (makeRelative, (</>))
import System.FilePath.Find (fileName, find, (&&?), (/=?), (==?))
import System.IO (hPrint, hPutStrLn, stderr)
import System.Process (readProcess)
import UnliftIO.Exception (tryAny)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

subcmd :: Mod CommandFields (IO ())
subcmd =
  command "changelogs" $
    info
      ( helper
          <*> subparsers
            [ checkVersionsCmd
            , formatChangelogsCmd
            ]
      )
      (progDesc "Operations on the changelogs of a project")

data FormatChangelogsOptions = FormatChangelogsOptions
  { optFilePaths :: [String]
  , optWriteFile :: FilePath -> Text -> IO ()
  , optBulletHierarchy :: Text
  }

modifyInPlace :: FilePath -> Text -> IO ()
modifyInPlace = TL.writeFile

writeToFile :: FilePath -> FilePath -> Text -> IO ()
writeToFile outputFile _sourceFile = TL.writeFile outputFile

writeToStdout :: FilePath -> Text -> IO ()
writeToStdout _sourceFile = TL.putStr

formatChangelogsCmd :: Mod CommandFields (IO ())
formatChangelogsCmd =
  command "format" $
    info
      ( helper <*> do
          optCommon <- options
          optWriteFile <-
            asum
              [ flag' modifyInPlace $
                  help "Modify files in-place"
                    <> short 'i'
                    <> long "inplace"
              , fmap writeToFile . strOption $
                  help "Write output to FILE"
                    <> short 'o'
                    <> long "output"
                    <> metavar "FILE"
              , pure writeToStdout
              ]
          optBulletHierarchy <-
            strOption $
              help "Use CHARS for the levels of bullets"
                <> short 'b'
                <> long "bullets"
                <> metavar "CHARS"
                <> value "*-+"
                <> showDefaultWith unpack
          optFilePaths <-
            some . strArgument $
              help "Changelog files and directories to process"
                <> metavar "(FILE|DIRECTORY) ..."
          pure $ formatChangelogs optCommon FormatChangelogsOptions {..}
      )
      ( fullDesc
          <> progDesc "Parse and reformat changelog files"
          <> footer
            "Directories given as arguments will be searched recursively for \
            \files named \"CHANGELOG.md\". \
            \Directories named \"dist-newstyle\" and \".git\" will be ignored \
            \when searching."
      )

findChangelogs :: FilePath -> IO [FilePath]
findChangelogs fp = do
  let
    notIgnoredDir = fileName /=? "dist-newstyle" &&? fileName /=? ".git"
    isChangelog = fileName ==? "CHANGELOG.md"
  isDir <- doesDirectoryExist fp
  if isDir
    then find notIgnoredDir isChangelog fp
    else pure [fp]

formatChangelogs :: Options -> FormatChangelogsOptions -> IO ()
formatChangelogs Options {..} FormatChangelogsOptions {..} = do
  changelogs <- concat <$> traverse findChangelogs optFilePaths
  failure <- fmap (any isLeft) . for changelogs $ \fp -> do
    bitraverse (hPrint stderr) pure <=< tryAny $ do
      let
        throwError e = errorWithoutStackTrace $ fp <> ": " <> TL.unpack e
        writeLog = optWriteFile fp . renderChangelog optBulletHierarchy
      when (optVerbosity > 0) $
        hPutStrLn stderr $
          "Examining " <> fp
      either throwError writeLog . parseChangelog =<< T.readFile fp
  when failure exitFailure

-- ---------------------------------------------------------------------------
-- changelogs check-versions
-- ---------------------------------------------------------------------------

checkVersionsCmd :: Mod CommandFields (IO ())
checkVersionsCmd =
  command "check-versions" $
    info
      ( helper <*> do
          optProjectDir <-
            optional . strOption $
              help "Check only the packages under DIR (default: the repository root)"
                <> short 'p'
                <> long "project"
                <> metavar "DIR"
          pure $ checkVersions optProjectDir
      )
      (progDesc "Check that cabal package versions match their changelog versions")

checkVersions :: Maybe FilePath -> IO ()
checkVersions optProjectDir = do
  root <- maybe getRepoRoot pure optProjectDir
  (cabalErrors, allPackages) <- scanPackages root
  -- Packages excluded from the release process are versioned 9.9.9.9 and are left alone.
  let packages = filter (not . isLocalOnly) allPackages

  packageErrors <- fmap catMaybes . for packages $ \pkg -> do
    let path = pkgDir pkg </> "CHANGELOG.md"
        displayPath = makeRelative root path
    result <- readChangelog path
    pure . fmap ((unPackageName (pkgName pkg) <> ": ") <>) $ case result of
      -- A package without a CHANGELOG.md is skipped: there is no version to compare against.
      Left ChangelogMissing -> Nothing
      Left e -> describeChangelogError displayPath e
      Right cl -> describeProblem displayPath <$> checkVersion (pkgVersion pkg) cl

  for_ cabalErrors $ printError . describeCabalFileError root
  for_ packageErrors printError

  let errorCount = length cabalErrors + length packageErrors
  unless (errorCount == 0) $ do
    T.putStrLn ""
    T.putStrLn $ T.pack (show errorCount) <> " error(s)"
    T.putStrLn
      "The version must be updated in both the CHANGELOG.md and the .cabal file of every package \
      \affected by a change, in the same PR (see RELEASING.md)."
    exitFailure
 where
  printError = T.hPutStrLn stderr . ("Error: " <>)

-- | Get the repository root directory.
getRepoRoot :: IO FilePath
getRepoRoot = dropWhileEnd isSpace <$> readProcess "git" ["rev-parse", "--show-toplevel"] ""
