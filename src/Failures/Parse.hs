{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Failures.Parse (parseLog) where

import Control.Exception (evaluate)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Failures.LogResults
import Text.Regex.Applicative

import qualified Data.Text as T
import qualified Data.Text.IO as T

cons :: a -> [a] -> [a]
!x `cons` !xs = let !y = x : xs in y

parseLog :: FilePath -> IO [Failure]
parseLog fp = do
  infos <- mapMaybe findInfo . T.lines <$> T.readFile fp
  evaluate $ collectFailures infos

collectFailures :: [ReproInfo] -> [Failure]
collectFailures = go
 where
  go (Seed seed : Selector sel : infs) =
    Failure sel seed `cons` go infs
  go (Selector sel : Seed seed : infs) =
    Failure sel seed `cons` go infs
  go (SelectorAndSeed sel seed : infs) =
    Failure sel seed `cons` go infs
  go (Selector sel : infs) =
    Failure sel def `cons` go infs
  go (Seed seed : infs) =
    Failure def seed `cons` go infs
  go [] = []
  def = Option "" ""

data ReproInfo
  = Selector !Option
  | Seed !Option
  | SelectorAndSeed !Option !Option
  deriving (Eq, Ord, Show)

findInfo :: Text -> Maybe ReproInfo
findInfo = fmap fst . findLongestPrefixWithUncons T.uncons (few anySym *> reproInfo)

reproInfo :: RE Char ReproInfo
reproInfo =
  asum
    [ Selector <$ "Use " <*> (option "-p" <* " '" <*> text <* "'")
    , Seed <$ "Use " <*> (option "--quickcheck-replay" <* "=\"" <*> text <* "\"")
    , SelectorAndSeed
        <$ "To rerun use: "
        <*> (option "--match" <* " \"" <*> text <* "\" ")
        <*> (option "--seed" <* " " <*> text)
    ]
 where
  option name = Option . T.pack <$> name
  text = T.pack <$> few anySym
