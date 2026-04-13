{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Changelogs.Command (subcmd) where

import Changelogs.Types
import Common.Options (Options (..), options, subparsers)
import Control.Monad (when, (<=<))
import Data.Bitraversable (bitraverse)
import Data.Either (isLeft)
import Data.Text.Lazy (Text, unpack)
import Data.Traversable (for)
import Options.Applicative
import System.Exit (exitFailure)
import System.IO (hPrint, hPutStrLn, stderr)
import UnliftIO.Exception (tryAny)

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

subcmd :: Mod CommandFields (IO ())
subcmd =
  command "changelogs" $
    info
      ( helper
          <*> subparsers
            [ formatChangelogsCmd
            ]
      )
      (progDesc "Operations on the changelogs of a project")

data FormatChangelogsOptions = FormatChangelogsOptions
  { optChangelogs :: [String]
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
          optChangelogs <-
            some . strArgument $
              help "Changelog files to process"
                <> metavar "CHANGELOG ..."
          pure $ formatChangelogs optCommon FormatChangelogsOptions {..}
      )
      (progDesc "Parse and reformat changelog files")

formatChangelogs :: Options -> FormatChangelogsOptions -> IO ()
formatChangelogs Options {..} FormatChangelogsOptions {..} = do
  failure <- fmap (any isLeft) . for optChangelogs $ \fp -> do
    bitraverse (hPrint stderr) pure <=< tryAny $ do
      let
        throwError e = errorWithoutStackTrace $ fp <> ": " <> TL.unpack e
        writeLog = optWriteFile fp . renderChangelog optBulletHierarchy
      when (optVerbosity > 0) $
        hPutStrLn stderr $
          "Examining " <> fp
      either throwError writeLog . parseChangelog =<< TL.readFile fp
  when failure exitFailure
