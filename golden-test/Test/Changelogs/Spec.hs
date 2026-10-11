module Test.Changelogs.Spec where

import System.Exit (ExitCode (..))
import System.FilePath ((<.>), (</>))
import System.IO (IOMode (..), hPutStr, withBinaryFile)
import System.IO.Temp (withSystemTempDirectory)
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

  describe "check-versions" $ do
    specify "clean" $ do
      projectDir <- fixturePath "Changelogs/check-versions-clean"

      (code, out, err) <-
        readProcessWithExitCode "cleret" ["changelogs", "check-versions", "-p", projectDir] ""

      -- A silent run is the whole point: every package here either matches its
      -- changelog, sits below one or more placeholder entries, has no changelog
      -- at all, or is excluded from releases.
      err `shouldSatisfy` null
      out `shouldSatisfy` null
      code `shouldBe` ExitSuccess

    specify "errors" $ do
      projectDir <- fixturePath "Changelogs/check-versions-errors"
      expected <- fixturePath "Changelogs/check-versions-errors.golden"

      (code, out, err) <-
        readProcessWithExitCode "cleret" ["changelogs", "check-versions", "-p", projectDir] ""

      goldenTest expected (T.pack err)
      out `shouldContain` "RELEASING.md"
      code `shouldNotBe` ExitSuccess

    -- There is no golden file for this one, because the decoder's message varies
    -- between versions of the text library.
    specify "unreadable" $
      withSystemTempDirectory "check-versions" $ \projectDir -> do
        writeFile (projectDir </> "unreadable.cabal") $
          unlines ["cabal-version: 3.12", "name: unreadable", "version: 1.0.0.0"]
        -- A binary handle writes each Char as a single byte, so this ends in 0xff.
        withBinaryFile (projectDir </> "CHANGELOG.md") WriteMode $ \h ->
          hPutStr h "# Version history for `unreadable`\n\n## 1.0.0.0\n\n* First version \xff\n"

        (code, _out, err) <-
          readProcessWithExitCode "cleret" ["changelogs", "check-versions", "-p", projectDir] ""

        err `shouldContain` "could not read"
        code `shouldNotBe` ExitSuccess

-- TODO:
--  Markdown parsing failures
--  Version parsing failures
--  Unexpected structure
