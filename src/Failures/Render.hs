{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Failures.Render (renderCmd) where

import Common.Options (Options (..), options)
import Data.Either (partitionEithers)
import Data.Foldable (for_)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Traversable (for)
import Failures.LogResults
import Options.Applicative hiding (Failure)
import System.IO (hPutStrLn, stderr)

import qualified Data.Aeson as JSON
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.IO as T

data RenderOptions = RenderOptions
  { optOutput :: FilePath
  , optInputs :: [FilePath]
  }
  deriving (Show)

renderCmd :: Mod CommandFields (IO ())
renderCmd = do
  command "render" $
    info
      ( helper <*> do
          optCommon <- options
          optOutput <-
            strOption $
              help "Write output to FILE"
                <> short 'o'
                <> long "output"
                <> metavar "FILE"
                <> value "/dev/stdout"
                <> showDefaultWith id
          optInputs <-
            many . strArgument $
              help "JSON files containing failures"
                <> metavar "FILE ..."
          pure $ render optCommon RenderOptions {..}
      )
      (progDesc "Render failure information from Cabal test logs")

render :: Options -> RenderOptions -> IO ()
render Options {..} RenderOptions {..} = do
  let
    trace n = if optVerbosity >= n then hPutStrLn stderr else const mempty

  trace 1 $ show (length optInputs) <> " rendered failures files found"

  (errs, inputs) <-
    fmap partitionEithers $
      for optInputs $ \input -> do
        trace 2 $ "Examining " <> input
        JSON.eitherDecodeFileStrict @LogResults input

  for_ errs $ hPutStrLn stderr

  let
    groupedFailures =
      NE.groupWith fst . sort $
        [ (suiteName, (optionValue failureSelector, logCompilerVersion, optionValue failureSeed))
        | LogResults {..} <- inputs
        , SuiteRun {..} <- logSuiteRuns
        , Failure {..} <- suiteFailures
        ]

  let
    prefix = ["## Test Failures ##"]
    body =
      concat
        [ [ ""
          , "### `" <> suite <> "` ###"
          , ""
          , "| Test                                         | Compiler | Seed     |"
          , "|:-------------------------------------------- |:-------- |:-------- |"
          ]
            <> [ T.unwords ["|", selector, "|", compilerName, "|", seed, "|"]
               | (selector, compiler, seed) <- map snd $ NE.toList g
               , let compilerName = T.intercalate "." $ map (T.pack . show) compiler
               ]
        | g@((suite, _) :| _) <- groupedFailures
        ]

  T.writeFile optOutput . T.unlines $ prefix <> body
