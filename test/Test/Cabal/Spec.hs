{-# LANGUAGE OverloadedStrings #-}

module Test.Cabal.Spec where

import Cabal.Plan (SearchPlanJson (ProjectRelativeToDir), findPlanJson)
import System.FilePath (takeFileName, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess, readProcess)
import Test.Common.Fixture (fixturePath)
import Test.Common.Golden (goldenTest)
import Test.Hspec

import qualified Data.Text as T
import qualified Data.Text.IO as T

spec :: Spec
spec = do
  specify "targets" $ do
    projectDir <- fixturePath "Cabal"
    expected <- fixturePath "Cabal/targets.golden"
    actual <- T.pack <$> readProcess "cleret" ["cabal", "targets", "-p", projectDir] ""
    goldenTest expected actual
  specify "list-bins" $ do
    projectDir <- fixturePath "Cabal"
    expected <- fixturePath "Cabal/list-bins.golden"
    actual <- T.pack <$> readProcess "cleret" ["cabal", "list-bins", "-p", projectDir] ""
    goldenTest expected actual
  specify "relativize-plan" $ do
    projectFixture <- fixturePath "Cabal"
    expected <- fixturePath "Cabal/relativize-plan.golden"
    withTempProject projectFixture $ \projectDir -> do
      planFile <- findPlanJson $ ProjectRelativeToDir projectDir
      let modifyFile fp f = T.writeFile fp . f =<< T.readFile fp
      modifyFile planFile $ T.replace "dist-newstyle" (T.pack $ projectDir </> "dist-newstyle")
      callProcess "cleret" ["cabal", "relativize-plan", "-p", projectDir]
      goldenTest expected =<< T.readFile planFile

withTempProject :: FilePath -> (FilePath -> IO ()) -> IO ()
withTempProject projectFixture action = do
  withSystemTempDirectory "cleret-workdir" $ \workdir -> do
    let projectDir = workdir </> takeFileName projectFixture
    callProcess "cp" ["-r", projectFixture, projectDir]
    callProcess "chmod" ["-R", "u+wx", projectDir] -- Needed for nix
    action projectDir
