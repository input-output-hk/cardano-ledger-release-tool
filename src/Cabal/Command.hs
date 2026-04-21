{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Cabal.Command (subcmd) where

import Cabal.Plan
import Common.Options (Options (..), options, subparsers)
import Control.Monad (unless, when)
import Data.Bool (bool)
import Data.Char (toLower, toUpper)
import Data.Foldable (for_)
import Data.List (intercalate, sort, stripPrefix, (\\))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Options.Applicative
import System.Directory (doesDirectoryExist, makeAbsolute)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..), die)
import System.IO (BufferMode (..), hPutStrLn, hSetBuffering, stderr)
import System.Process (CreateProcess (..), proc, waitForProcess, withCreateProcess)
import Text.Read (readMaybe)

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

data CabalOptions = CabalOptions
  { optProjectDir :: FilePath
  , optNames :: [Text]
  }
  deriving (Show)

cabalOptions :: Parser CabalOptions
cabalOptions = do
  optProjectDir <-
    strOption $
      help "The project directory, or a subdirectory of it"
        <> short 'p'
        <> long "project"
        <> metavar "DIR"
        <> value "."
        <> showDefaultWith id
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
targets optCommon optCompTypes optCabal@CabalOptions {..} = do
  (_root, plan) <- getProjectPlan optCommon optCabal
  T.putStr . T.unlines . sort $
    [ dispCompNameTargetFull pkg comp
    | (pkg, comp, _ci, _src) <- planComponents optNames optCompTypes plan
    ]

listBins :: Options -> [CompType] -> CabalOptions -> IO ()
listBins optCommon optCompTypes optCabal@CabalOptions {..} = do
  (_root, plan) <- getProjectPlan optCommon optCabal
  T.putStr . T.unlines . sort $
    [ T.pack bin
    | (_p, _cn, comp, _src) <- planComponents optNames optCompTypes plan
    , Just bin <- [ciBinFile comp]
    ]

run :: Options -> CabalOptions -> IO ()
run = runComponents [CompTypeExe]

test :: Options -> CabalOptions -> IO ()
test = runComponents [CompTypeTest]

runComponents :: [CompType] -> Options -> CabalOptions -> IO ()
runComponents compTypes optCommon@Options {..} optCabal@CabalOptions {..} = do
  hSetBuffering stderr LineBuffering
  (rootDir, plan) <- getProjectPlan optCommon optCabal
  env <- getEnvironment
  let
    bins =
      sort $
        [ (pkgName, compName, bin, src)
        | (pkgName, compName, compInfo, srcLoc) <- planComponents optNames compTypes plan
        , Just bin <- [ciBinFile compInfo]
        , Just (LocalUnpackedPackage src) <- [srcLoc]
        ]
  for_ bins $ \(pkg, comp, bin, src) -> do
    absBin <- makeAbsolute bin
    absSrc <- makeAbsolute src
    let
      -- TODO: Figure out how to handle `data-dir` field which isn't surfaced in `plan.json`
      varName = T.unpack $ T.map fixchar (unPkgName pkg) <> "_datadir"
      fixchar '-' = '_'
      fixchar c = c
      -- TODO: Add other variables (eg `_bindir`) if needed
      extraEnv = [(varName, absSrc)]
      cwd = if fst (unCompName comp) `elem` [CompTypeExe, CompTypeSetup] then rootDir else src
      binProc = (proc absBin []) {env = Just $ extraEnv <> env, cwd = Just cwd}
      name = T.unpack $ dispCompNameTargetFull pkg comp
    unless (optVerbosity == 0 && null (drop 1 bins)) $ do
      hPutStrLn stderr $ "Running " <> name
    withCreateProcess binProc (\_ _ _ -> waitForProcess) >>= \case
      ExitFailure n -> die $ name <> " failed with exit code " <> show n
      ExitSuccess -> pure ()

getProjectPlan :: Options -> CabalOptions -> IO (FilePath, PlanJson)
getProjectPlan Options {..} CabalOptions {..} = do
  -- Avoid confusing behaviour from `findProjectRoot`
  doesDirectoryExist optProjectDir
    >>= bool (die $ "Project directory " <> optProjectDir <> " doesn't exist") (pure ())

  root <-
    findProjectRoot optProjectDir
      >>= maybe (die $ "Can't find project root in " <> optProjectDir) pure

  when (optVerbosity > 0) $
    hPutStrLn stderr $
      "Examining " <> root

  plan <- findAndDecodePlanJson $ ProjectRelativeToDir root

  when (optVerbosity > 0) $
    hPutStrLn stderr $
      "Plan has " <> show (Map.size $ pjUnitsWithType UnitTypeLocal plan) <> " local units"

  pure (root, plan)

planComponents :: [Text] -> [CompType] -> PlanJson -> [(PkgName, CompName, CompInfo, Maybe PkgLoc)]
planComponents names compTypes plan =
  let pkgNames = names
      compNames = Just <$> names
   in [ (pkgName, compName, compInfo, srcLoc)
      | unit <- Map.elems $ pjUnitsWithType UnitTypeLocal plan
      , (compName, compInfo) <- Map.toList $ uComps unit
      , fst (unCompName compName) `elem` compTypes
      , let (PkgId pkgName _) = uPId unit
      , null names || unPkgName pkgName `elem` pkgNames || snd (unCompName compName) `elem` compNames
      , srcLoc <- [uPkgSrc unit]
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
