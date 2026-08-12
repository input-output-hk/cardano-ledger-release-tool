module Test.Changelogs.Spec where

import System.Exit (ExitCode (..))
import System.FilePath ((<.>))
import System.Process (readProcessWithExitCode)
import Test.Common.Fixture (fixturePath)
import Test.Common.Golden (goldenTest)
import Test.Hspec

import qualified Data.Text as T

spec :: Spec
spec = do
  specify "format" $ do
    inputFile <- fixturePath "Changelogs/CHANGELOG.md"
    let expected = inputFile <.> "golden"
    (code, out, err) <- readProcessWithExitCode "cleret" ["changelogs", "format", inputFile] ""
    err `shouldSatisfy` null
    goldenTest expected (T.pack out)
    code `shouldBe` ExitSuccess

-- TODO:
--  Markdown parsing failures
--  Version parsing failures
--  Unexpected structure
