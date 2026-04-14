{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Common.Options where

import Data.Foldable (fold)
import Options.Applicative

newtype Options = Options
  { optVerbosity :: Int
  }
  deriving (Show)

options :: Parser Options
options = do
  optVerbosity <-
    counter $
      help "Increase output verbosity (repeatable)"
        <> short 'v'
        <> long "verbose"
  pure Options {..}

counter :: Mod FlagFields () -> Parser Int
counter = fmap length . many . flag' ()

subparsers :: Foldable t => t (Mod CommandFields a) -> Parser a
subparsers = subparser . fold
