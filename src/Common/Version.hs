module Common.Version (
  parseVersionText,
  showVersionText,
) where

import Data.List (find)
import Data.Text (Text)
import Data.Version (Version, parseVersion, showVersion)
import Text.ParserCombinators.ReadP (readP_to_S)

import qualified Data.Text as T

parseVersionText :: Text -> Maybe Version
parseVersionText t =
  fst <$> find (null . snd) (readP_to_S parseVersion (T.unpack t))

showVersionText :: Version -> Text
showVersionText = T.pack . showVersion
