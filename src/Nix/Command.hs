{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Nix.Command (subcmd) where

import Common.Options (Options (..), options, subparsers)
import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), ToJSON (..), defaultOptions, fieldLabelModifier, genericParseJSON, genericToJSON)
import Data.ByteString (ByteString)
import Data.Char (toLower)
import Data.Foldable (for_)
import Data.List (sortOn, stripPrefix)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Lens.Micro
import Lens.Micro.Aeson
import Lens.Micro.Extras
import Options.Applicative
import System.Directory (createDirectory)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath ((</>))
import System.IO (hFlush, stderr, stdout)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcess, readProcessWithExitCode, showCommandForUser)
import Text.Printf (printf)
import Text.Regex.Applicative

import qualified Data.ByteString as B
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T

subcmd :: Mod CommandFields (IO ())
subcmd =
  command "nix" $
    info
      ( helper
          <*> subparsers
            [ hashesCmd
            ]
      )
      (progDesc "Operations on the nix information of a Cabal project")

data HashesOptions = HashesOptions
  { optPrefetch :: Bool
  , optProject :: FilePath
  , optFlakeLock :: FilePath
  }
  deriving (Show)

hashesCmd :: Mod CommandFields (IO ())
hashesCmd =
  command "hashes" $
    info
      ( helper <*> do
          optCommon <- options
          optPrefetch <-
            switch $
              help "Prefetch inputs to check hash correctness"
                <> short 'p'
                <> long "prefetch"
          optProject <-
            strArgument $
              help "Cabal project file"
                <> metavar "PROJECT-FILE"
                <> value "cabal.project"
                <> showDefaultWith id
          optFlakeLock <-
            strArgument $
              help "Nix flake lock file"
                <> metavar "LOCK-FILE"
                <> value "flake.lock"
                <> showDefaultWith id
          pure $ checkHashes optCommon HashesOptions {..}
      )
      ( fullDesc
          <> progDesc "Check the nix hashes in a flake-enabled Cabal project"
          <> footer
            "Relevant authentication tokens found in the nix configuration will \
            \be used when prefetching, in case any of the inputs are private"
      )

checkHashes :: Options -> HashesOptions -> IO ()
checkHashes Options {..} HashesOptions {..} = do
  srps <- parseSrps <$> T.readFile optProject
  locks <- parseLocks <$> B.readFile optFlakeLock

  when optVerbose $ do
    printf "Found %2d inputs in %s\n" (length srps) optProject
    printf "Found %2d inputs in %s\n" (length locks) optFlakeLock

  failures <- checkInputHashes optVerbose optPrefetch (srps <> locks)

  for_ (sortOn snd failures) $ \(input, failure) -> do
    let urlAndRev = (input ^. inputUrl) <> "@" <> (input ^. inputRev)
    T.hPutStr stderr . T.unlines $
      case failure of
        NoSingleHash hashes ->
          [urlAndRev <> " has " <> (if null hashes then "no hashes" else "multiple hashes:")]
            <> map ("  " <>) hashes
        HashMismatch actual expected ->
          [ "Hash mismatch for " <> urlAndRev <> ":"
          , "  Specified: " <> expected
          , "     Actual: " <> actual
          ]
        PrefetchFailed code cmd args errs ->
          let cmd' = showCommandForUser (T.unpack cmd) (T.unpack <$> args)
           in [ "Prefetching failed for " <> urlAndRev <> ":"
              , "  " <> T.pack (show cmd') <> " exited with " <> T.pack (show code)
              ]
                <> map ("  " <>) (T.lines errs)

  unless (null failures) exitFailure

parseLocks :: ByteString -> [Input]
parseLocks = toListOf $ _Value . key "nodes" . members . key "locked" . _JSON

parseSrps :: Text -> [Input]
parseSrps = snd . foldr go (emptyInput, []) . T.lines
 where
  go l (cur, rest) =
    case T.words l of
      kw : _
        | kw == "source-repository-package" -> (emptyInput, cur : rest)
      kw : val : _
        | kw == "location:" -> (cur & inputUrl .~ val, rest)
        | kw == "tag:" -> (cur & inputRev .~ val, rest)
        | kw == "--sha256:" -> (cur & inputNarHash ?~ val, rest)
      _ -> (cur, rest)

data Failure
  = NoSingleHash ![Text]
  | HashMismatch !Text !Text
  | PrefetchFailed !Int !Text ![Text] !Text
  deriving (Eq, Ord, Show)

checkInputHashes :: Bool -> Bool -> [Input] -> IO [(Input, Failure)]
checkInputHashes verbose prefetch inputs = do
  tokens <-
    if prefetch
      then
        view (key "access-tokens" . key "value" . _JSON)
          <$> readProcess "nix" ["config", "show", "--json"] ""
      else
        pure mempty

  let
    inputCommit Input {..} = (inputType_, inputOwner_, inputRepo_, inputRev_)
    groups = NE.groupAllWith inputCommit inputs
    checkGroup group = do
      let
        input = NE.head group
        hashes = mapMaybe (view inputNarHash) $ NE.toList group
        urlAndRev = (input ^. inputUrl) <> "@" <> (input ^. inputRev)
      when verbose $ do
        T.putStr $ "Checking " <> urlAndRev <> " ... "
        hFlush stdout
      (duration, failures) <-
        timed $
          case hashes of
            [hash] -> if prefetch then checkInputHash tokens input hash else pure []
            _ -> pure [NoSingleHash hashes]
      when verbose $ do
        printf "%.2fs\n" (realToFrac duration :: Double)
        hFlush stdout
      pure $ (input,) <$> failures

  foldMap checkGroup groups

checkInputHash :: Map Text Text -> Input -> Text -> IO [Failure]
checkInputHash tokens inp expectedHash = do
  let
    host = (inp ^. inputType) <> ".com"
    owner = inp ^. inputOwner
    repo = inp ^. inputRepo
    rev = inp ^. inputRev

    bearerAuth t = ["-H", "Authorization: Bearer " <> t]
    privateAuth t = ["-H", "PRIVATE-TOKEN: " <> t]
    githubAuth = bearerAuth
    gitlabAuth t =
      case T.split (== ':') t of
        ["OAuth2", o2] -> bearerAuth o2
        ["PAT", pat] -> privateAuth pat
        [_, _] -> error "Unknown gitlab token type"
        _ -> error "Couldn't parse gitlab token"

    (archiveUrl, authType) =
      case inp ^. inputType of
        "github" ->
          ( T.intercalate
              "/"
              ["https://" <> host, owner, repo, "archive", rev <> ".tar.gz"]
          , githubAuth
          )
        "gitlab" ->
          ( T.intercalate
              "/"
              ["https://" <> host, owner, repo, "-", "archive", rev, repo <> "-" <> rev <> ".tar.gz"]
          , gitlabAuth
          )
        typ ->
          error $ "Unknown input type: " <> T.unpack typ

    auth = maybe [] (map T.unpack . authType) $ Map.lookup host tokens

  result <-
    withSystemTempDirectory "cleret" $ \workdir -> do
      let
        archive = workdir </> "archive.tgz"
        hashdir = workdir </> "content"
      createDirectory hashdir
      runExceptT $ do
        _ <- readProcessExceptT "curl" (auth <> ["-sSfL", "-o", archive, T.unpack archiveUrl]) ""
        _ <- readProcessExceptT "tar" ["-xzf", archive, "-C", hashdir, "--strip-components=1"] ""
        actualHash <- T.strip . T.pack <$> readProcessExceptT "nix" ["hash", "path", hashdir] ""
        pure [HashMismatch actualHash expectedHash | actualHash /= expectedHash]

  let
    prefixes = ["Authorization: Bearer", "PRIVATE-TOKEN:"]
    redact arg =
      fromMaybe arg . listToMaybe $
        [prefix <> " *****" | prefix <- prefixes, prefix `T.isPrefixOf` arg]

  case result of
    Left (code, cmd, args, err) -> pure [PrefetchFailed code (T.pack cmd) (redact . T.pack <$> args) (T.pack err)]
    Right mismatches -> pure mismatches

readProcessExceptT :: String -> [String] -> String -> ExceptT (Int, String, [String], String) IO String
readProcessExceptT cmd args input = do
  (result, out, err) <- liftIO $ readProcessWithExitCode cmd args input
  case result of
    ExitSuccess -> pure out
    ExitFailure code -> throwError (code, cmd, args, err)

timed :: IO a -> IO (NominalDiffTime, a)
timed act = do
  start <- getCurrentTime
  result <- act
  end <- getCurrentTime
  pure (end `diffUTCTime` start, result)

data Input = Input
  { inputType_ :: !Text
  , inputOwner_ :: !Text
  , inputRepo_ :: !Text
  , inputRev_ :: !Text
  , inputNarHash_ :: !(Maybe Text)
  }
  deriving (Eq, Ord, Show, Generic)

inputType :: Lens' Input Text
inputType f s = (\a -> s {inputType_ = a}) <$> f (inputType_ s)

inputOwner :: Lens' Input Text
inputOwner f s = (\a -> s {inputOwner_ = a}) <$> f (inputOwner_ s)

inputRepo :: Lens' Input Text
inputRepo f s = (\a -> s {inputRepo_ = a}) <$> f (inputRepo_ s)

inputRev :: Lens' Input Text
inputRev f s = (\a -> s {inputRev_ = a}) <$> f (inputRev_ s)

inputNarHash :: Lens' Input (Maybe Text)
inputNarHash f s = (\a -> s {inputNarHash_ = a}) <$> f (inputNarHash_ s)

-- This isn't a fully lawful lens because you may not get back out what you put in
-- However, it does normalize URLs
inputUrl :: Lens' Input Text
inputUrl f s = setter s <$> f (getter s)
 where
  getter Input {..} = "https://" <> inputType_ <> ".com/" <> inputOwner_ <> "/" <> inputRepo_ <> ".git"
  re = (,,) <$ "https://" <*> text <* ".com/" <*> text <* "/" <*> text <* ("/" <|> ".git" <|> "")
  setter inp url =
    case match re . T.unpack $ url of
      Just (t, o, r) -> inp {inputType_ = t, inputOwner_ = o, inputRepo_ = r}
      Nothing -> inp
  text = T.pack <$> few anySym

emptyInput :: Input
emptyInput = Input "" "" "" "" Nothing

instance ToJSON Input where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = relabel "input"}

instance FromJSON Input where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = relabel "input"}

relabel :: String -> String -> String
relabel p f = maybe f (_head %~ toLower) $ stripPrefix p =<< (f ^? _init)
