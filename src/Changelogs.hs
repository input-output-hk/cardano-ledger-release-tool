{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Changelogs (subcmd) where

import Changelog
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

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Options as Global

data Options = Options
  { optChangelogs :: [String]
  , optWriteFile :: FilePath -> Text -> IO ()
  , optBulletHierarchy :: Text
  }

options :: ParserInfo Options
options =
  info
    ( helper <*> do
        let
          inplaceParser =
            flag' TL.writeFile $
              help "Modify files in-place"
                <> short 'i'
                <> long "inplace"
          fileParser =
            fmap (const . TL.writeFile) . strOption $
              help "Write output to FILE"
                <> short 'o'
                <> long "output"
                <> metavar "FILE"
          stdoutParser =
            -- Write output to stdout
            pure $ const TL.putStr
        optWriteFile <- inplaceParser <|> fileParser <|> stdoutParser
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

subcmd :: Mod CommandFields (Global.Options -> IO ())
subcmd =
  command "changelogs" $
    options <&> \Options {..} Global.Options {} -> do
      failure <- fmap (any isLeft) . for optChangelogs $ \fp -> do
        bitraverse (hPrint stderr) pure <=< tryAny $ do
          let
            throwError e = errorWithoutStackTrace $ fp <> ": " <> TL.unpack e
            writeLog = optWriteFile fp . renderChangelog optBulletHierarchy
          either throwError writeLog . parseChangelog =<< TL.readFile fp
      when failure exitFailure
