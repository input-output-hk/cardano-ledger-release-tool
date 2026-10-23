{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Failures.Reproduce (reproduceCmd) where

import Common.Options (Options (..), options)
import Control.Monad (unless, when)
import Data.Containers.ListUtils (nubOrd)
import Data.Either (partitionEithers)
import Data.Foldable (for_, toList)
import Data.Function (on)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import Data.Version (Version, makeVersion, parseVersion)
import Failures.LogResults
import Options.Applicative hiding (Failure)
import System.Directory (createDirectoryIfMissing, makeAbsolute, removePathForcibly)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (<.>), (</>))
import System.IO (
  BufferMode (..),
  IOMode (..),
  hPutStrLn,
  hSetBuffering,
  stderr,
  stdout,
  withFile,
 )
import System.Process (CreateProcess (..), StdStream (..), proc, waitForProcess, withCreateProcess)
import Text.ParserCombinators.ReadP (readP_to_S)

import qualified Data.Aeson as JSON
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.IO as T

data ReproduceOptions = ReproduceOptions
  { optNoop :: Bool
  , optNightly :: Bool
  , optCompilerVersion :: Maybe Version
  , optWorkingDirectory :: Maybe FilePath
  , optLogsDirectory :: Maybe FilePath
  , optInputs :: [FilePath]
  }
  deriving (Show)

reproduceCmd :: Mod CommandFields (IO ())
reproduceCmd = do
  command "reproduce" $
    info
      ( helper <*> do
          optCommon <- options
          optNoop <-
            switch $
              help "Just show what would be done"
                <> short 'n'
                <> long "noop"
          optNightly <-
            switch $
              help "Use the nightly version of each test suite"
                <> short 'y'
                <> long "nightly"
          optCompilerVersion <-
            optional . option versionReader $
              help "Select only failures that used compiler version VERSION"
                <> short 'c'
                <> long "compiler"
                <> metavar "VERSION"
          optWorkingDirectory <-
            optional . strOption $
              help "Use DIR as the working directory when running tests"
                <> short 'w'
                <> long "working-directory"
                <> metavar "DIR"
          optLogsDirectory <-
            optional . strOption $
              help "Save log files in DIR"
                <> short 'l'
                <> long "logs"
                <> metavar "DIR"
          optInputs <-
            many . strArgument $
              help "JSON files containing failures"
                <> metavar "FILE ..."
          pure $ reproduce optCommon ReproduceOptions {..}
      )
      (progDesc "Reproduce failures extracted from Cabal test logs")
 where
  versionReader :: ReadM Version
  versionReader = maybeReader $ fmap fst . find (null . snd) . readP_to_S parseVersion

reproduce :: Options -> ReproduceOptions -> IO ()
reproduce Options {..} ReproduceOptions {..} = do
  let
    trace n = if optVerbosity >= n || optNoop then T.hPutStrLn stderr else const mempty

  trace 1 $ T.pack (show $ length optInputs) <> " failures files found"

  (errs, inputs) <-
    fmap partitionEithers $
      for optInputs $ \input -> do
        trace 2 $ "Examining " <> T.pack input
        JSON.eitherDecodeFileStrict @LogResults input

  for_ errs $ hPutStrLn stderr

  logsDirectory <- traverse makeAbsolute optLogsDirectory

  hSetBuffering stdout LineBuffering

  for_ inputs $ \LogResults {..} -> do
    when (maybe True (== makeVersion logCompilerVersion) optCompilerVersion) $ do
      for_ logSuiteRuns $ \SuiteRun {..} -> do
        for_ (NE.groupBy ((==) `on` failureSeed) suiteFailures) $ \group -> do
          let
            Failure {failureSeed = seed} :| _ = group
            selectors = nubOrd . NE.toList $ failureSelector <$> group
            optionArgs Option {..} =
              map T.unpack $
                let
                  modifiedName =
                    fromMaybe optionName $
                      lookup optionName [("-p", "--pattern")]
                 in
                  if "--" `T.isPrefixOf` modifiedName
                    then [modifiedName <> "=" <> optionValue]
                    else [modifiedName, optionValue]
            details = T.unwords [suiteName, "with seed", optionValue seed]

          trace 1 $ "Testing: " <> details

          let
            env = ["NIGHTLY=true" | optNightly]
            cmd = "cabal"
            logPaths =
              [ dir </> T.unpack ("test-" <> suiteName <> "-" <> optionValue seed) <.> "log"
              | dir <- toList logsDirectory
              ]
            args =
              ["test", T.unpack suiteName]
                <> ["--test-option=" <> arg | arg <- optionArgs =<< (seed : selectors)]
                <> ["--test-log=" <> path | path <- logPaths]
            outputArgs =
              [ "-v0"
              , "--test-show-details=never"
              ]

          trace 1 $
            let
              quoteEquals s =
                case T.break (== '=') s of
                  (_, "") ->
                    quote s
                  (pfx, sfx) ->
                    quote pfx <> "=" <> quote (T.drop 1 sfx)
              quote s
                | T.any special s = "\"" <> s <> "\""
                | otherwise = s
              special c = c == ' ' || c == '$' || c == '\''
             in
              T.unwords $ quoteEquals . T.pack <$> env <> (cmd : args)

          unless optNoop $ do
            for_ logPaths $ \path -> do
              createDirectoryIfMissing True $ takeDirectory path
              removePathForcibly path

            exitCode <-
              withFile "/dev/null" ReadWriteMode $ \devNull ->
                withCreateProcess
                  (proc cmd (args <> outputArgs))
                    { cwd = optWorkingDirectory
                    , std_in = UseHandle devNull
                    , std_out = UseHandle devNull
                    , std_err = UseHandle devNull
                    }
                  $ \_ _ _ -> waitForProcess

            when (exitCode == ExitSuccess) $
              trace 0 $
                "Didn't fail: " <> details
