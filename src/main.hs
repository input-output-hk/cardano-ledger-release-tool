{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Common.Options (Options (..))
import Data.Foldable (fold)
import Data.Version (showVersion)
import Options.Applicative
import PackageInfo_cardano_ledger_release_tool (version)

import qualified Changelogs.Command as Changelogs
import qualified System.Console.Terminal.Size as TS

main :: IO ()
main = do
  cols <- maybe 100 TS.width <$> TS.size

  let
    versionOption v =
      infoOption (showVersion v) $
        help "Show version information"
          <> short 'V'
          <> long "version"
          <> hidden

  (subcmd, opts) <-
    customExecParser
      (prefs $ columns cols)
      ( info
          ( helper <*> versionOption version <*> do
              optVerbose <-
                switch $
                  help "Produce verbose output"
                    <> short 'v'
                    <> long "verbose"
              subcmd <-
                subparser . fold $
                  [ Changelogs.subcmd
                  ]
              pure (subcmd, Options {..})
          )
          (fullDesc <> header "Cardano Ledger release tool")
      )

  subcmd opts
