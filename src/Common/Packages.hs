{-# LANGUAGE OverloadedStrings #-}

module Common.Packages (
  PackageName (..),
  Package (..),
  CabalFileError,
  describeCabalFileError,
  scanPackages,
  isLocalOnly,
) where

import Common.Version (parseVersionText)
import Control.Monad (guard)
import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import Data.List (isPrefixOf, isSuffixOf, sortOn)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import Data.Version (Version, makeVersion)
import System.Directory (doesDirectoryExist, listDirectory, pathIsSymbolicLink)
import System.FilePath (makeRelative, takeDirectory, (</>))
import UnliftIO.Exception (Exception, displayException, tryAny)

import qualified Data.ByteString as BS
import qualified Data.Text as T

newtype PackageName = PackageName {unPackageName :: Text}
  deriving (Eq, Ord, Show)

data Package = Package
  { pkgName :: PackageName
  , pkgVersion :: Version
  , pkgDir :: FilePath
  }
  deriving (Eq, Show)

-- | A .cabal file that could not be read, with the reason.
data CabalFileError = CabalFileError FilePath Text

describeCabalFileError :: FilePath -> CabalFileError -> Text
describeCabalFileError root (CabalFileError fp reason) =
  T.pack (makeRelative root fp) <> ": " <> reason

{- | Find all packages under a directory by scanning for .cabal files.
Skips dist-newstyle and hidden directories. Returns both success and error cases.
-}
scanPackages :: FilePath -> IO ([CabalFileError], [Package])
scanPackages root = sortPackages . partitionEithers <$> go root
 where
  -- so we have a stable sorting for tests
  sortPackages (errors, packages) =
    ( sortOn (\(CabalFileError fp _) -> fp) errors
    , sortOn pkgName packages
    )

  go dir = do
    entries <- listDirectory dir
    concat <$> traverse (processEntry dir) entries

  processEntry parent entry
    | entry == "dist-newstyle" = pure []
    | "." `isPrefixOf` entry = pure []
    | otherwise = do
        let path = parent </> entry
        isLink <- pathIsSymbolicLink path
        if isLink
          -- Skip symlinks. Their targets are reached via their real paths,
          -- so descending into an alias (e.g. semantics/executable-spec ->
          -- libs/small-steps) would duplicate a package under a non-canonical
          -- directory and make the chosen path depend on traversal order.
          then pure []
          else do
            isDir <- doesDirectoryExist path
            if isDir
              then go path
              else
                if ".cabal" `isSuffixOf` entry
                  then (: []) <$> parseCabalFile path
                  else pure []

parseCabalFile :: FilePath -> IO (Either CabalFileError Package)
parseCabalFile fp = do
  result <- tryAny $ BS.readFile fp
  pure . first (CabalFileError fp) $ do
    bytes <- first unreadable result
    ls <- T.lines <$> first unreadable (decodeUtf8' bytes)
    name <- maybe (Left "no 'name:' field found") Right $ findField "name" ls
    verText <- maybe (Left "no 'version:' field found") Right $ findField "version" ls
    ver <- maybe (Left $ "cannot parse version '" <> verText <> "'") Right $ parseVersionText verText
    pure
      Package
        { pkgName = PackageName name
        , pkgVersion = ver
        , pkgDir = takeDirectory fp
        }
 where
  unreadable :: Exception e => e -> Text
  unreadable e = "could not read: " <> T.pack (displayException e)

-- | Packages with version 9.9.9.9 are local-only and should be skipped for releases and checks.
isLocalOnly :: Package -> Bool
isLocalOnly pkg = pkgVersion pkg == makeVersion [9, 9, 9, 9]

-- | Find the value of the first top-level field with the given (lowercase) name.
findField :: Text -> [Text] -> Maybe Text
findField field = listToMaybe . mapMaybe fieldValue
 where
  fieldValue line = do
    let (name, rest) = T.breakOn ":" line
    guard (T.toLower name == field)
    T.strip <$> T.stripPrefix ":" rest
