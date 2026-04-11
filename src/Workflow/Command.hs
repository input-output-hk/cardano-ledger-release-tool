{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Workflow.Command (subcmd) where

import Cabal.Plan
import Common.Options (Options (..), options, subparsers)
import Control.Monad (unless, when)
import Data.Bool (bool)
import Data.Foldable (for_)
import Data.List (nub, (\\))
import Data.Text (Text)
import Data.Yaml (Value, decodeFileThrow)
import Lens.Micro ((^..))
import Lens.Micro.Aeson (key, values, _String)
import Options.Applicative
import System.Directory (doesDirectoryExist)
import System.Exit (die, exitFailure)
import System.FilePath ((</>))
import System.IO (stderr)

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

subcmd :: Mod CommandFields (IO ())
subcmd =
  command "workflow" $
    info
      ( helper
          <*> subparsers
            [ checkTestMatrixCmd
            ]
      )
      (progDesc "Operations on the GitHub workflows of a Cabal project")

data CheckTestMatrixOptions = CheckTestMatrixOptions
  { optProjectDir :: FilePath
  , optWorkflowFile :: FilePath
  }
  deriving (Show)

checkTestMatrixCmd :: Mod CommandFields (IO ())
checkTestMatrixCmd =
  command "check-test-matrix" $
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
          optWorkflowFile <-
            strOption $
              help "The workflow file name (relative to .github/workflows)"
                <> short 'w'
                <> long "workflow"
                <> metavar "FILENAME"
                <> value "haskell.yml"
                <> showDefaultWith id
          pure $ checkTestMatrix optCommon CheckTestMatrixOptions {..}
      )
      (progDesc "Check that the test jobs in a GitHub workflow match the tests in a Cabal project")

checkTestMatrix :: Options -> CheckTestMatrixOptions -> IO ()
checkTestMatrix Options {..} CheckTestMatrixOptions {..} = do
  -- Avoid confusing behaviour from `findProjectRoot`
  doesDirectoryExist optProjectDir
    >>= bool (die $ "Project directory " <> optProjectDir <> " doesn't exist") (pure ())

  root <-
    findProjectRoot optProjectDir
      >>= maybe (die $ "Can't find project root in " <> optProjectDir) pure

  plan <- findAndDecodePlanJson $ ProjectRelativeToDir root

  workflow <- decodeFileThrow $ root </> ".github/workflows" </> optWorkflowFile

  -- We use (\\) instead of sets, to catch repeated occurrences
  let expected = planTests plan
      actual = workflowTests workflow
      missing = expected \\ actual
      extra = actual \\ expected

  when optVerbose $ do
    Text.hPutStrLn stderr $ "Cabal:\n  " <> Text.intercalate ", " expected
    Text.hPutStrLn stderr $ "Workflow:\n  " <> Text.intercalate ", " actual

  unless (null missing) $ do
    putStrLn "The following tests are missing from the workflow:"
    for_ missing $ Text.putStrLn . ("* " <>)
  unless (null extra) $ do
    putStrLn "The following tests should not be in the workflow:"
    for_ extra $ Text.putStrLn . ("* " <>)

  unless (null missing && null extra) exitFailure

planTests :: PlanJson -> [Text]
planTests plan =
  let localUnits = filter ((UnitTypeLocal ==) . uType) . Map.elems . pjUnits $ plan
      unitsWithTests = filter (any isTestComp . Map.keys . uComps) localUnits
      isTestComp (CompNameTest _) = True
      isTestComp _ = False
      pIdName (PkgId (PkgName name) _) = name
   in -- Deduplicate the test package names since a package could have multiple suites.
      -- Using `nub` is OK because we're using `\\` above, and the lists are small.
      nub $ pIdName . uPId <$> unitsWithTests

workflowTests :: Value -> [Text]
workflowTests v = v ^.. key "jobs" . key "test" . key "strategy" . key "matrix" . key "package" . values . _String
