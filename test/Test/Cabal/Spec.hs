module Test.Cabal.Spec where

import System.Process (readProcess)
import Test.Common.Fixture (fixturePath)
import Test.Common.Golden (goldenTest)
import Test.Hspec

import qualified Data.Text as T

spec :: Spec
spec = do
  it "targets" $ do
    projectDir <- fixturePath "Cabal"
    expected <- fixturePath "Cabal/targets.golden"
    actual <- T.pack <$> readProcess "cleret" ["cabal", "targets", "-p", projectDir] ""
    goldenTest expected actual
