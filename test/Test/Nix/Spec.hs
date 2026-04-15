module Test.Nix.Spec where

import Data.Bifunctor (first)
import Data.Foldable (for_)
import Data.Maybe (isJust)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (addExtension, splitExtension)
import System.Process (readProcessWithExitCode)
import Test.Common.Fixture (fixturePath)
import Test.Common.Golden (goldenTest)
import Test.Hspec

import qualified Data.Text as T

spec :: Spec
spec = do
  inNixSandbox <- runIO $ isJust <$> lookupEnv "NIX_ENFORCE_PURITY"

  describe "hashes" $ do
    for_ ["", "-missing", "-mismatch"] $ \suffix -> do
      let
        withSuffix = fixturePath . uncurry addExtension . first (<> suffix) . splitExtension
        scenario = if null suffix then "correct" else dropWhile (== '-') suffix
        prefetch = ["-p" | scenario == "mismatch"]
        expectExitCode c = (if scenario == "correct" then shouldBe else shouldNotBe) c ExitSuccess

      specify scenario $ do
        if not (null prefetch) && inNixSandbox
          then
            pendingWith "no network access in nix sandbox"
          else do
            lock <- withSuffix "Nix/flake.lock"
            project <- withSuffix "Nix/cabal.project"
            expected <- withSuffix "Nix/hashes.golden"

            (code, out, err) <-
              readProcessWithExitCode "cleret" (["nix", "hashes", project, lock] <> prefetch) ""

            out `shouldSatisfy` null
            goldenTest expected (T.pack err)
            expectExitCode code
