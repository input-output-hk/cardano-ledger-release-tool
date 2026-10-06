module Common.Version (
  bumpPatch,
  parseVersionText,
  showVersionText,
) where

import Data.List (find)
import Data.Text (Text)
import Data.Version (Version (..), parseVersion, showVersion)
import Text.ParserCombinators.ReadP (readP_to_S)

import qualified Data.Text as T

-- | Increment the last component of a version: 1.2.3.4 -> 1.2.3.5
bumpPatch :: Version -> Version
bumpPatch (Version [] tags) = Version [1] tags
bumpPatch (Version branch tags) =
  Version (init branch ++ [last branch + 1]) tags

parseVersionText :: Text -> Maybe Version
parseVersionText t =
  fst <$> find (null . snd) (readP_to_S parseVersion (T.unpack t))

showVersionText :: Version -> Text
showVersionText = T.pack . showVersion
