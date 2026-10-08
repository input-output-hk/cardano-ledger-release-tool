{-# LANGUAGE OverloadedStrings #-}

{- | Parser for the @meta.toml@ that CHaP keeps for every package version in
@_sources/<package>/<version>/@.
-}
module Release.Meta (
  ChapSource (..),
  ChapMeta (..),
  chapMetaCodec,
  parseChapMeta,
) where

import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.Char (isHexDigit)
import Data.Text (Text)
import Toml (TomlCodec, (.=))

import qualified Data.Text as T
import qualified Toml

-- | Where CHaP fetched the package from (foliage's @PackageVersionSource@).
data ChapSource
  = GitHubSource
      { srcRepo :: Text
      -- ^ As written in the file, e.g. @IntersectMBO/cardano-ledger@.
      , srcRev :: Text
      -- ^ 7 to 40 hex digits, lowercased. CHaP has a few abbreviated ones.
      }
  | -- | A tarball; nothing to tag.
    UrlSource Text
  deriving (Eq, Show)

data ChapMeta = ChapMeta
  { metaSource :: ChapSource
  , metaSubdir :: Maybe FilePath
  {- ^ Where the package lived in the repository. 'Nothing' when the file omits
  it, which means the repository root.
  -}
  }
  deriving (Eq, Show)

{- | The fields of @meta.toml@ that matter here. Foliage also decodes
@timestamp@, @revisions@ and @deprecations@; 'Toml.decode' ignores keys the
codec does not mention.
-}
chapMetaCodec :: TomlCodec ChapMeta
chapMetaCodec =
  ChapMeta
    <$> sourceCodec .= metaSource
    <*> Toml.dioptional (Toml.string "subdir") .= metaSubdir

sourceCodec :: TomlCodec ChapSource
sourceCodec =
  Toml.dimatch matchUrl UrlSource (Toml.text "url")
    <|> Toml.dimatch
      matchGitHub
      (uncurry GitHubSource)
      (Toml.table (Toml.pair (Toml.text "repo") (revCodec "rev")) "github")
 where
  matchUrl (UrlSource url) = Just url
  matchUrl _ = Nothing
  matchGitHub (GitHubSource repo rev) = Just (repo, rev)
  matchGitHub _ = Nothing

-- | A commit id: validated and lowercased while decoding.
revCodec :: Toml.Key -> TomlCodec Text
revCodec = Toml.textBy id $ \t ->
  let rev = T.toLower t
   in if T.length rev >= 7 && T.length rev <= 40 && T.all isHexDigit rev
        then Right rev
        else Left ("rev '" <> t <> "' is not a commit id")

{- | Parse the contents of a @meta.toml@. Uses 'Toml.decode', never
'Toml.decodeExact': the files carry keys this codec does not mention.
-}
parseChapMeta :: Text -> Either Text ChapMeta
parseChapMeta = first Toml.prettyTomlDecodeErrors . Toml.decode chapMetaCodec
