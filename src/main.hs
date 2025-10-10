{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable (fold)
import Data.Version (showVersion)
import Options (Options (..))
import Options.Applicative
import PackageInfo_cardano_ledger_release_tool (version)

import qualified Changelogs
import qualified System.Console.Terminal.Size as TS

main :: IO ()
main = do
  cols <- maybe 100 TS.width <$> TS.size

  (subcmd, opts) <-
    customExecParser
      (prefs $ columns cols)
      ( info
          ( helper <*> simpleVersioner (showVersion version) <*> do
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
