{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Changelog

import Data.Foldable (for_)
import Data.Text.Lazy (Text, unpack)
import Options.Applicative
import System.IO (stderr)

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified System.Console.Terminal.Size as TS

data Options = Options
  { optChangelogs :: [String]
  , optWriteFile :: FilePath -> Text -> IO ()
  , optBulletHierarchy :: Text
  }

main :: IO ()
main = do
  cols <- maybe 100 TS.width <$> TS.size

  Options {..} <-
    customExecParser
      (prefs $ columns cols)
      ( info
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
                  pure $ const TL.putStrLn
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
          (fullDesc <> header "Parse and lint changelog files")
      )

  for_ optChangelogs $ \fp -> do
    let
      printError e = TL.hPutStrLn stderr $ TL.pack fp <> ": " <> e
      writeLog = optWriteFile fp . renderChangelog optBulletHierarchy
    either printError writeLog =<< parseChangelogFile fp

parseChangelogFile :: FilePath -> IO (Either Text Changelog)
parseChangelogFile f = parseChangelog <$> TL.readFile f
