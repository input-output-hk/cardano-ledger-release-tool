module Main where

import Common.Options (subparsers)
import Control.Monad (join)
import Data.Version (showVersion)
import Options.Applicative
import PackageInfo_cardano_ledger_release_tool (version)

import qualified Cabal.Command as Cabal
import qualified Changelogs.Command as Changelogs
import qualified System.Console.Terminal.Size as TS
import qualified Workflow.Command as Workflow

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

  join . customExecParser (prefs $ columns cols) $
    info
      ( helper
          <*> versionOption version
          <*> subparsers
            [ Cabal.subcmd
            , Changelogs.subcmd
            , Workflow.subcmd
            ]
      )
      (fullDesc <> header "Cardano Ledger release tool")
