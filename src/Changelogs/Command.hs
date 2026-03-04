{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Changelogs.Command (subcmd) where

import Changelogs.Types
import Control.Monad (when, (<=<))
import Data.Bitraversable (bitraverse)
import Data.Either (isLeft)
import Data.Functor ((<&>))
import Data.Text.Lazy (Text, unpack)
import Data.Traversable (for)
import Options.Applicative
import System.Exit (exitFailure)
import System.IO (hPrint, stderr)
import UnliftIO.Exception (tryAny)

import qualified Common.Options as Common
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

data Options = Options
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

options :: ParserInfo Options
options =
  info
    ( helper <*> do
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
        pure Options {..}
    )
    (progDesc "Parse and lint changelog files")

subcmd :: Mod CommandFields (Common.Options -> IO ())
subcmd =
  command "changelogs" $
    options <&> \Options {..} Common.Options {} -> do
      failure <- fmap (any isLeft) . for optChangelogs $ \fp -> do
        bitraverse (hPrint stderr) pure <=< tryAny $ do
          let
            throwError e = errorWithoutStackTrace $ fp <> ": " <> TL.unpack e
            writeLog = optWriteFile fp . renderChangelog optBulletHierarchy
          either throwError writeLog . parseChangelog =<< TL.readFile fp
      when failure exitFailure
