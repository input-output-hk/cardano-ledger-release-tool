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
      let
        scenario = if null suffix then "correct" else dropWhile (== '-') suffix
        expectExitCode c = (if scenario == "correct" then shouldBe else shouldNotBe) c ExitSuccess

      specify scenario $ do
        projectDir <- fixturePath "Workflow"
        expected <- fixturePath $ "Workflow/check-test-matrix" <> suffix <> ".golden"
        let workflow = "haskell" <> suffix <> ".yml"

        (code, out, err) <-
          readProcessWithExitCode "cleret" ["workflow", "check-test-matrix", "-p", projectDir, "-w", workflow] ""

        err `shouldSatisfy` null
        goldenTest expected (T.pack out)
        expectExitCode code
