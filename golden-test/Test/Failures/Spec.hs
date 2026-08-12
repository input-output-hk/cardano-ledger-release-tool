module Test.Failures.Spec where

import System.FilePath.Find (always, fileName, find, (==?))
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Process (callProcess)
import Test.Common.Fixture (fixturePath)
import Test.Common.Golden (goldenTest)
import Test.Hspec

import qualified Data.Text.IO as T

spec :: Spec
spec = do
  it "extract" $ do
    withSystemTempFile "extract.actual" $ \actual h -> do
      hClose h
      logsDir <- fixturePath "Failures/extract"
      expected <- fixturePath "Failures/extract.golden"
      callProcess "cleret" ["failures", "extract", "-o", actual, "-p", logsDir]
      goldenTest expected =<< T.readFile actual
  it "render" $ do
    withSystemTempFile "render.actual" $ \actual h -> do
      hClose h
      failuresDir <- fixturePath "Failures/render"
      expected <- fixturePath "Failures/render.golden"
      failuresFiles <- find always (fileName ==? "failures.json") failuresDir
      callProcess "cleret" $ ["failures", "render", "-o", actual] <> failuresFiles
      goldenTest expected =<< T.readFile actual
