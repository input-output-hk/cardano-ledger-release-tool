{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable (fold)
import Options (Options (..))
import Options.Applicative

import qualified Changelogs
import qualified System.Console.Terminal.Size as TS

main :: IO ()
main = do
  cols <- maybe 100 TS.width <$> TS.size

  (subcmd, opts) <-
    customExecParser
      (prefs $ columns cols)
      ( info
          ( helper <*> do
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
