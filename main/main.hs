{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable (fold)
import Data.List (intercalate)
import Data.Version (Version, showVersion)
import Options (Options (..))
import Options.Applicative

import qualified Changelogs
import qualified System.Console.Terminal.Size as TS

type Subcommand = Options -> IO ()

subcommands :: [(String, Version, Mod CommandFields Subcommand)]
subcommands =
  [ (Changelogs.name, Changelogs.version, Changelogs.subcmd)
  ]

versionMessage :: String
versionMessage =
  intercalate "\n" $
    [n <> ": " <> showVersion v | (n, v, _) <- subcommands]

main :: IO ()
main = do
  cols <- maybe 100 TS.width <$> TS.size

  (subcmd, opts) <-
    customExecParser
      (prefs $ columns cols)
      ( info
          ( helper <*> simpleVersioner versionMessage <*> do
              optVerbose <-
                switch $
                  help "Produce verbose output"
                    <> short 'v'
                    <> long "verbose"
              subcmd <-
                subparser . fold $
                  [sc | (_, _, sc) <- subcommands]
              pure (subcmd, Options {..})
          )
          (fullDesc <> header "Cardano Ledger release tool")
      )

  subcmd opts
