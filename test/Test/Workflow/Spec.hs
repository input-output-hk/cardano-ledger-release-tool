module Test.Workflow.Spec where

import Data.Foldable (for_)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Common.Fixture (fixturePath)
import Test.Common.Golden (goldenTest)
import Test.Hspec

import qualified Data.Text as T

spec :: Spec
spec = do
  describe "check-test-matrix" $ do
    for_ ["", "-missing", "-extra"] $ \suffix -> do
      it (if null suffix then "correct" else dropWhile (== '-') suffix) $ do
        projectDir <- fixturePath "Workflow"
        expected <- fixturePath $ "Workflow/check-test-matrix" <> suffix <> ".golden"
        let workflow = "haskell" <> suffix <> ".yml"
        (code, actual, err) <-
          readProcessWithExitCode "cleret" ["workflow", "check-test-matrix", "-p", projectDir, "-w", workflow] ""
        goldenTest expected (T.pack actual)
        err `shouldSatisfy` null
        if null suffix
          then code `shouldBe` ExitSuccess
          else code `shouldNotBe` ExitSuccess
