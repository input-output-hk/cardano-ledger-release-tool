{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Failures.Extract (extractCmd) where

import Cabal.Plan
import Common.Options (Options (..), options)
import Control.Monad (filterM, guard, unless)
import Data.Traversable (for)
import Failures.LogResults
import Failures.Parse
import Options.Applicative hiding (Failure)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Exit (die)
import System.FilePath ((<.>), (</>))
import System.IO (hPutStrLn, stderr)

import qualified Data.Aeson as JSON
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

data ExtractOptions = ExtractOptions
  { optProjectDir :: FilePath
  , optOutput :: FilePath
  }
  deriving (Show)

extractCmd :: Mod CommandFields (IO ())
extractCmd = do
  command "extract" $
    info
      ( helper <*> do
          optCommon <- options
          optProjectDir <-
            strOption $
              help "The project directory, or a subdirectory of it"
                <> short 'p'
                <> long "project"
                <> metavar "DIR"
                <> value "."
                <> showDefaultWith id
          optOutput <-
            strOption $
              help "Write output to FILE"
                <> short 'o'
                <> long "output"
                <> metavar "FILE"
                <> value "/dev/stdout"
                <> showDefaultWith id
          pure $ extract optCommon ExtractOptions {..}
      )
      (progDesc "Extract failure information from Cabal test logs")

extract :: Options -> ExtractOptions -> IO ()
extract Options {..} ExtractOptions {..} = do
  let
    trace n = if optVerbosity >= n then hPutStrLn stderr else const mempty

  -- Avoid confusing behaviour from `findProjectRoot`
  doesDirectoryExist optProjectDir
    >>= (`unless` die ("Project directory " <> optProjectDir <> " doesn't exist"))

  root <-
    findProjectRoot optProjectDir
      >>= maybe (die $ "Can't find project root in " <> optProjectDir) pure

  trace 2 $ "Examining " <> root

  planLocation <- findPlanJson $ ProjectRelativeToDir root

  trace 2 $ "Plan location: " <> planLocation

  plan <- decodePlanJson planLocation

  suiteLogs <-
    filterM (doesFileExist . snd) $ do
      -- List monad
      unit <- Map.elems $ pjUnits plan
      guard $ uType unit == UnitTypeLocal
      Just dir <- [uDistDir unit]
      comp@(CompNameTest tName) <- Map.keys (uComps unit)
      let
        pId = uPId unit
        PkgId pName _ = pId
        PkgName name = pName
        suite = name <> ":" <> dispCompNameTarget pName comp
        file = dir </> "test" </> T.unpack (dispPkgId pId <> "-" <> tName) <.> "log"
      pure (suite, file)

  trace 1 $ show (length suiteLogs) <> " logs found"

  logSuiteRuns <-
    for suiteLogs $ \(suiteName, file) -> do
      suiteFailures <- do
        trace 2 $ "Examining " <> file
        parseLog file
      pure SuiteRun {..}

  let PkgId _ (Ver logCompilerVersion) = pjCompilerId plan

  JSON.encodeFile optOutput LogResults {..}
