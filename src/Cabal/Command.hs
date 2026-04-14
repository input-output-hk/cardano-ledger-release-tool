{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Cabal.Command (subcmd) where

import Cabal.Plan
import Common.Options (Options (..), options, subparsers)
import Control.Monad (when)
import Data.Bool (bool)
import Data.Char (toLower, toUpper)
import Data.List (intercalate, stripPrefix, (\\))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Options.Applicative
import System.Directory (doesDirectoryExist)
import System.Exit (die)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

subcmd :: Mod CommandFields (IO ())
subcmd =
  command "cabal" $
    info
      ( helper
          <*> subparsers
            [ targetsCmd
            ]
      )
      (progDesc "Operations on a Cabal project")

data TargetsOptions = TargetsOptions
  { optProjectDir :: FilePath
  , optCompTypes :: [CompType]
  , optPackages :: [Text]
  }
  deriving (Show)

targetsCmd :: Mod CommandFields (IO ())
targetsCmd =
  command "targets" $
    info
      ( helper <*> do
          optCommon <- options
          optProjectDir <-
            strOption $
              help "The project directory, or a subdirectory of it"
                <> short 'p'
                <> long "project"
                <> metavar "DIR"
                <> value "."
                <> showDefaultWith id
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
          optPackages <-
            many . strArgument $
              help "Show targets for PACKAGE ... (default: all packages)"
                <> metavar "PACKAGE ..."
          pure $
            let optCompTypes = (if null include then allCompTypes else include) \\ exclude
             in targets optCommon TargetsOptions {..}
      )
      (progDesc "List the targets in a Cabal project")
 where
  inExHelp op =
    op
      <> " targets of type TYPE (repeatable; one of: "
      <> intercalate ", " (map showType allCompTypes)
      <> ")"
  readType :: ReadM CompType
  readType = maybeReader $ readMaybe . ("CompType" <>) . initial toUpper
  showType :: CompType -> String
  showType = initial toLower . stripPrefix' "CompType" . show
  stripPrefix' p s = fromMaybe s $ stripPrefix p s
  initial f (c : s) = f c : s
  initial _ s = s

targets :: Options -> TargetsOptions -> IO ()
targets Options {..} TargetsOptions {..} = do
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

  Text.putStr . Text.unlines $
    [ dispCompNameTargetFull p c
    | u <- Map.elems $ pjUnitsWithType UnitTypeLocal plan
    , c <- Map.keys $ uComps u
    , compType c `elem` optCompTypes
    , let (PkgId p@(PkgName n) _) = uPId u
    , null optPackages || n `elem` optPackages
    ]

data CompType
  = CompTypeLib
  | CompTypeFlib
  | CompTypeExe
  | CompTypeTest
  | CompTypeBench
  | CompTypeSetup
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

compType :: CompName -> CompType
compType = \case
  CompNameLib -> CompTypeLib
  CompNameSubLib _ -> CompTypeLib
  CompNameFLib _ -> CompTypeFlib
  CompNameExe _ -> CompTypeExe
  CompNameTest _ -> CompTypeTest
  CompNameBench _ -> CompTypeBench
  CompNameSetup -> CompTypeSetup

allCompTypes :: [CompType]
allCompTypes = [minBound .. maxBound]

dispCompNameTargetFull :: PkgName -> CompName -> Text
dispCompNameTargetFull p@(PkgName n) c = n <> ":" <> dispCompNameTarget p c

pjUnitsWithType :: UnitType -> PlanJson -> Map.Map UnitId Unit
pjUnitsWithType t = Map.filter ((t ==) . uType) . pjUnits
