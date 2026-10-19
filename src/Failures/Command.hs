module Failures.Command (subcmd) where

import Common.Options (subparsers)
import Failures.Extract (extractCmd)
import Failures.Render (renderCmd)
import Failures.Reproduce (reproduceCmd)
import Options.Applicative

subcmd :: Mod CommandFields (IO ())
subcmd =
  command "failures" $
    info
      ( helper
          <*> subparsers
            [ extractCmd
            , renderCmd
            , reproduceCmd
            ]
      )
      (progDesc "Examine, summarize and reproduce failures in Cabal test logs")
