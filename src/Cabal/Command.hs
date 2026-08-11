{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Cabal.Command (subcmd) where

import Cabal.Plan
import Common.Options (Options (..), options, subparsers)
import Control.Concurrent.Async (concurrently)
import Control.Monad (unless, when)
import Data.Aeson (Value, eitherDecodeFileStrict, encodeFile)
import Data.Bool (bool)
import Data.ByteString.Builder (byteString, hPutBuilder)
import Data.Char (toLower, toUpper)
import Data.Function (fix)
import Data.List (intercalate, sort, stripPrefix, (\\))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Traversable (for)
import Lens.Micro ((%~))
import Lens.Micro.Aeson (members, values, _String)
import Options.Applicative
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, makeAbsolute)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..), die, exitFailure)
import System.FilePath (takeDirectory, (<.>), (</>))
import System.IO (
  Handle,
  IOMode (WriteMode),
  hClose,
  hFlush,
  hPutStrLn,
  stderr,
  stdout,
  withBinaryFile,
 )
import System.Process (
  CreateProcess (..),
  StdStream (UseHandle),
  createPipe,
  proc,
  waitForProcess,
  withCreateProcess,
 )
import Text.Read (readMaybe)

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T

subcmd :: Mod CommandFields (IO ())
subcmd =
  command "cabal" $
    info
      ( helper
          <*> subparsers
            [ targetsCmd
            , listBinsCmd
            , relativizeCmd
            , runCmd
            , testCmd
            ]
      )
      (progDesc "Operations on a Cabal project")

compTypeOptions :: Parser [CompType]
compTypeOptions = do
  include <-
    many . option readType $
      help (inExHelp "Include")
        <> short 'i'
        <> long "include"
        <> metavar "TYPE"
  exclude <-
    many . option readType $
      help (inExHelp "Exclude")
        <> short 'x'
        <> long "exclude"
        <> metavar "TYPE"
  pure $ (if null include then allCompTypes else include) \\ exclude
 where
  inExHelp op =
    op
      <> " components of type TYPE (repeatable; one of: "
      <> intercalate ", " (map showType allCompTypes)
      <> ")"
  readType :: ReadM CompType
  readType = maybeReader $ readMaybe . ("CompType" <>) . initial toUpper
  showType :: CompType -> String
  showType = initial toLower . stripPrefix' "CompType" . show
  stripPrefix' p s = fromMaybe s $ stripPrefix p s
  initial f (c : s) = f c : s
  initial _ s = s

projectOption :: Parser FilePath
projectOption =
  strOption $
    help "The project directory, or a subdirectory of it"
      <> short 'p'
      <> long "project"
      <> metavar "DIR"
      <> value "."
      <> showDefaultWith id

data CabalOptions = CabalOptions
  { optProjectDir :: FilePath
  , optNames :: [Text]
  }
  deriving (Show)

cabalOptions :: Parser CabalOptions
cabalOptions = do
  optProjectDir <- projectOption
  optNames <-
    many . strArgument $
      help "Select components named NAME or in package NAME (default: all components)"
        <> metavar "NAME ..."
  pure CabalOptions {..}

targetsCmd :: Mod CommandFields (IO ())
targetsCmd =
  command "targets" $
    info
      (helper <*> (targets <$> options <*> compTypeOptions <*> cabalOptions))
      (progDesc "List the targets in a Cabal project")

listBinsCmd :: Mod CommandFields (IO ())
listBinsCmd =
  command "list-bins" $
    info
      (helper <*> (listBins <$> options <*> compTypeOptions <*> cabalOptions))
      (progDesc "List the binaries in a Cabal project")

relativizeCmd :: Mod CommandFields (IO ())
relativizeCmd =
  command "relativize-plan" $
    info
      (helper <*> (relativize <$> options <*> projectOption))
      (progDesc "Make the file paths in a Cabal plan relative")

runCmd :: Mod CommandFields (IO ())
runCmd =
  command "run" $
    info
      (helper <*> (run <$> options <*> cabalOptions))
      (progDesc "Run the executables in a Cabal project")

testCmd :: Mod CommandFields (IO ())
testCmd =
  command "test" $
    info
      (helper <*> (test <$> options <*> cabalOptions))
      (progDesc "Run the tests in a Cabal project")

targets :: Options -> [CompType] -> CabalOptions -> IO ()
targets optCommon optCompTypes CabalOptions {..} = do
  (_root, plan) <- getProjectPlan optCommon optProjectDir
  T.putStr . T.unlines . sort $
    [ dispCompNameTargetFull pkg comp
    | (PkgId pkg _, comp, _dir, _src, _bin) <- planComponents optNames optCompTypes plan
    ]

listBins :: Options -> [CompType] -> CabalOptions -> IO ()
listBins optCommon optCompTypes CabalOptions {..} = do
  (_root, plan) <- getProjectPlan optCommon optProjectDir
  T.putStr . T.unlines . sort $
    [ T.pack bin
    | (_p, _cn, _dir, _src, bin) <- planBins optNames optCompTypes plan
    ]

relativize :: Options -> FilePath -> IO ()
relativize optCommon projectDir = do
  (rootDir, planFile) <- getProjectPlanFile optCommon projectDir

  plan <- either die pure =<< eitherDecodeFileStrict @Value planFile

  let
    root = T.pack rootDir <> "/"
    dropRoot s = fromMaybe s $ T.stripPrefix root s
    relativizeString = _String %~ dropRoot
    relativizeStrings = relativizeString . (members %~ relativizeStrings) . (values %~ relativizeStrings)

  encodeFile planFile $ relativizeStrings plan

run :: Options -> CabalOptions -> IO ()
run = runComponents [CompTypeExe]

test :: Options -> CabalOptions -> IO ()
test = runComponents [CompTypeTest]

runComponents :: [CompType] -> Options -> CabalOptions -> IO ()
runComponents compTypes optCommon@Options {..} CabalOptions {..} = do
  (rootDir, plan) <- getProjectPlan optCommon optProjectDir
  env <- getEnvironment
  let bins = sort $ planBins optNames compTypes plan

  when (optVerbosity > 0) $ do
    hPutStrLn stderr $ show (length bins) <> " matching binaries"
    hFlush stderr

  failures <- fmap sum . for bins $ \(pkgId, compName, dir, src, bin) -> do
    -- If `bin` and `src` are already absolute, `rootDir` will be ignored
    absBin <- makeAbsolute $ rootDir </> bin
    absSrc <- makeAbsolute $ rootDir </> src
    let
      -- TODO: Figure out how to handle `data-dir` field which isn't surfaced in `plan.json`
      PkgId pkgName _ver = pkgId
      varName = T.unpack $ T.map fixchar (unPkgName pkgName) <> "_datadir"
      fixchar '-' = '_'
      fixchar c = c
      -- TODO: Add other environment variables (eg `_bindir`) if needed
      extraEnv = [(varName, absSrc)]
      cwd = if fst (unCompName compName) `elem` [CompTypeExe, CompTypeSetup] then rootDir else absSrc
      binProc = (proc absBin []) {env = Just $ extraEnv <> env, cwd = Just cwd}
      name = T.unpack $ dispCompNameTargetFull pkgName compName

    unless (optVerbosity == 0 && null (drop 1 bins)) $ do
      hPutStrLn stderr $ "Running " <> name
      hFlush stderr

    binExists <- doesFileExist absBin
    unless binExists $ die $ "Binary missing: " <> absBin

    cwdExists <- doesDirectoryExist cwd
    unless cwdExists $ die $ "Working directory missing: " <> cwd

    exitCode <- case compName of
      CompNameTest testName -> do
        -- Send output to stdout and a file
        -- `dir` will usually be absolute, in which case `(rootDir </>)` is a no-op,
        -- but it's needed for testing because we have to use relative paths there
        let logPath = rootDir </> dir </> "test" </> T.unpack (dispPkgId pkgId <> "-" <> testName) <.> "log"

        createDirectoryIfMissing True $ takeDirectory logPath

        withBinaryFile logPath WriteMode $ \logHandle -> do
          (readEnd, writeEnd) <- createPipe

          let pipedProc = binProc {std_out = UseHandle writeEnd, std_err = UseHandle writeEnd}

          withCreateProcess pipedProc $ \_ _ _ ph -> do
            hClose writeEnd
            snd <$> concurrently (logger readEnd logHandle) (waitForProcess ph)
      _ -> do
        -- Send output to stdout
        withCreateProcess binProc $ \_ _ _ -> waitForProcess

    case exitCode of
      ExitFailure n -> do
        hPutStrLn stderr $ name <> " failed with exit code " <> show n
        hFlush stderr
        pure (1 :: Int)
      ExitSuccess ->
        pure (0 :: Int)

  unless (failures == 0) $ do
    hPutStrLn stderr $ "There were " <> show failures <> " failures"
    exitFailure

logger :: Handle -> Handle -> IO ()
logger pipe file = fix $ \self -> do
  chunk <- BS.hGetSome pipe (64 * 1024)
  unless (BS.null chunk) $ do
    let b = byteString chunk
    hPutBuilder stdout b
    hFlush stdout
    hPutBuilder file b
    self

getProjectPlanFile :: Options -> FilePath -> IO (FilePath, FilePath)
getProjectPlanFile Options {..} projectDir = do
  -- Avoid confusing behaviour from `findProjectRoot`
  doesDirectoryExist projectDir
    >>= bool (die $ "Project directory " <> projectDir <> " doesn't exist") (pure ())

  root <-
    findProjectRoot projectDir
      >>= maybe (die $ "Can't find project root in " <> projectDir) pure

  when (optVerbosity > 0) $
    hPutStrLn stderr $
      "Examining " <> root

  planFile <- findPlanJson $ ProjectRelativeToDir root

  pure (root, planFile)

getProjectPlan :: Options -> FilePath -> IO (FilePath, PlanJson)
getProjectPlan optCommon@Options {..} projectDir = do
  (root, planFile) <- getProjectPlanFile optCommon projectDir

  plan <- decodePlanJson planFile

  when (optVerbosity > 0) $
    hPutStrLn stderr $
      "Plan has " <> show (Map.size $ pjUnitsWithType UnitTypeLocal plan) <> " local units"

  pure (root, plan)

planBins :: [Text] -> [CompType] -> PlanJson -> [(PkgId, CompName, FilePath, FilePath, FilePath)]
planBins names compTypes plan =
  [ (pkgId, compName, dir, src, bin)
  | (pkgId, compName, dir, src, Just bin) <- planComponents names compTypes plan
  ]

planComponents :: [Text] -> [CompType] -> PlanJson -> [(PkgId, CompName, FilePath, FilePath, Maybe FilePath)]
planComponents names compTypes plan =
  let pkgNames = names
      compNames = Just <$> names
   in [ (pkgId, compName, dir, src, bin)
      | unit <- Map.elems $ pjUnitsWithType UnitTypeLocal plan
      , (compName, compInfo) <- Map.toList $ uComps unit
      , fst (unCompName compName) `elem` compTypes
      , let pkgId = uPId unit
      , let (PkgId pkgName _) = pkgId
      , let bin = ciBinFile compInfo
      , null names || unPkgName pkgName `elem` pkgNames || snd (unCompName compName) `elem` compNames
      , Just dir <- [uDistDir unit]
      , Just (LocalUnpackedPackage src) <- [uPkgSrc unit]
      ]

data CompType
  = CompTypeLib
  | CompTypeFlib
  | CompTypeExe
  | CompTypeTest
  | CompTypeBench
  | CompTypeSetup
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

allCompTypes :: [CompType]
allCompTypes = [minBound .. maxBound]

unCompName :: CompName -> (CompType, Maybe Text)
unCompName = \case
  CompNameLib -> (CompTypeLib, Nothing)
  CompNameSubLib n -> (CompTypeLib, Just n)
  CompNameFLib n -> (CompTypeFlib, Just n)
  CompNameExe n -> (CompTypeExe, Just n)
  CompNameTest n -> (CompTypeTest, Just n)
  CompNameBench n -> (CompTypeBench, Just n)
  CompNameSetup -> (CompTypeSetup, Nothing)

dispCompNameTargetFull :: PkgName -> CompName -> Text
dispCompNameTargetFull pkg comp = unPkgName pkg <> ":" <> dispCompNameTarget pkg comp

pjUnitsWithType :: UnitType -> PlanJson -> Map.Map UnitId Unit
pjUnitsWithType t = Map.filter ((t ==) . uType) . pjUnits

unPkgName :: PkgName -> Text
unPkgName (PkgName n) = n
