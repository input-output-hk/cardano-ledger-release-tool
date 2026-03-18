{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Common.Options where

import Data.Foldable (fold)
import Options.Applicative

newtype Options = Options
  { optVerbose :: Bool
  }
  deriving (Show)

options :: Parser Options
options = do
  optVerbose <-
    switch $
      help "Produce verbose output"
        <> short 'v'
        <> long "verbose"
  pure Options {..}

subparsers :: Foldable t => t (Mod CommandFields a) -> Parser a
subparsers = subparser . fold
