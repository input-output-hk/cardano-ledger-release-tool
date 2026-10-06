{-# LANGUAGE OverloadedStrings #-}

module Common.Packages (
  PackageName (..),
  Package (..),
  CabalFileError,
  describeCabalFileError,
  scanPackages,
  isLocalOnly,
  bumpCabalVersion,
) where

import Common.Version (parseVersionText, showVersionText)
import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.List (find, isPrefixOf, isSuffixOf, nubBy, sort, sortOn)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import Data.Version (Version, makeVersion)
import System.Directory (doesDirectoryExist, listDirectory, pathIsSymbolicLink)
import System.FilePath (makeRelative, takeDirectory, (</>))
import UnliftIO.Exception (Exception, displayException, tryAny)

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T

newtype PackageName = PackageName {unPackageName :: Text}
  deriving (Eq, Ord, Show)

data Package = Package
  { pkgName :: PackageName
  , pkgVersion :: Version
  , pkgDir :: FilePath
  }
  deriving (Eq, Show)

instance Ord Package where
  compare a b = compare (pkgName a) (pkgName b)

-- | A .cabal file that could not be read, with the reason.
data CabalFileError = CabalFileError FilePath Text

-- | Describe a 'CabalFileError', with the file's path relative to the given root.
describeCabalFileError :: FilePath -> CabalFileError -> Text
describeCabalFileError root (CabalFileError fp reason) =
  T.pack (makeRelative root fp) <> ": " <> reason

{- | Find all packages under a directory by scanning for .cabal files.
Excludes dist-newstyle. Returns the .cabal files that could not be read, sorted
by path, and the packages, sorted by name.
-}
scanPackages :: FilePath -> IO ([CabalFileError], [Package])
scanPackages root = tidy . partitionEithers <$> go root
 where
  tidy (errors, packages) =
    ( sortOn (\(CabalFileError fp _) -> fp) errors
    , nubBy ((==) `on` pkgName) (sort packages)
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
          then -- Skip symlinks. Their targets are reached via their real paths,
          -- so descending into an alias (e.g. semantics/executable-spec ->
          -- libs/small-steps) would duplicate a package under a non-canonical
          -- directory and make the chosen path depend on traversal order.
            pure []
          else do
            isDir <- doesDirectoryExist path
            if isDir
              then go path
              else
                if ".cabal" `isSuffixOf` entry
                  then (: []) <$> parseCabalFile path
                  else pure []

{- | Parse a .cabal file to extract the package name and version.

@version:@ is mandatory in a cabal file, so failing to find it means the file
has a layout this parser does not handle. Report that rather than silently
dropping the package.
-}
parseCabalFile :: FilePath -> IO (Either CabalFileError Package)
parseCabalFile fp = do
  result <- tryAny $ BS.readFile fp
  pure . first (CabalFileError fp) $ do
    bytes <- first unreadable result
    ls <- T.lines <$> first unreadable (decodeUtf8' bytes)
    name <- maybe (Left "no 'name:' field found") Right $ findField "name:" ls
    verText <- maybe (Left "no 'version:' field found") Right $ findField "version:" ls
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

{- | Whether a line declares a field, given as its lowercase name followed by a colon.
Field names are case-insensitive in cabal.
-}
isField :: Text -> Text -> Bool
isField prefix line = T.toLower (T.take (T.length prefix) line) == prefix

-- | Find the value of the first line declaring a field (see 'isField').
findField :: Text -> [Text] -> Maybe Text
findField prefix = fmap (T.strip . T.drop (T.length prefix)) . find (isField prefix)

-- | Update the version field in a .cabal file. Returns the cabal file path.
bumpCabalVersion :: Package -> Version -> IO FilePath
bumpCabalVersion pkg newVer = do
  let cabalFile = findCabalFile pkg
  content <- T.readFile cabalFile
  let ls = T.lines content
      ls' = map replaceVersion ls
  T.writeFile cabalFile (T.unlines ls')
  pure cabalFile
 where
  replaceVersion line
    | isField "version:" line =
        -- Preserve the whitespace between "version:" and the value
        let prefix = T.takeWhile (/= ':') line <> ":"
            rest = T.drop (T.length prefix) line
            spaces = T.takeWhile (== ' ') rest
         in prefix <> spaces <> showVersionText newVer
    | otherwise = line

-- | Find the .cabal file for a package.
findCabalFile :: Package -> FilePath
findCabalFile pkg =
  pkgDir pkg </> T.unpack (unPackageName (pkgName pkg)) <> ".cabal"
