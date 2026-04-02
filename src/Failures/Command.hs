module Failures.Command (subcmd) where

import Common.Options (subparsers)
import Failures.Extract (extractCmd)
import Failures.Render (renderCmd)
import Options.Applicative

subcmd :: Mod CommandFields (IO ())
subcmd =
  command "failures" $
    info
      ( helper
          <*> subparsers
            [ extractCmd
            , renderCmd
            ]
      )
      (progDesc "Examine and summarize failures in Cabal test logs")
