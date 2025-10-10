module Options where

newtype Options = Options
  { optVerbose :: Bool
  }
  deriving (Show)
