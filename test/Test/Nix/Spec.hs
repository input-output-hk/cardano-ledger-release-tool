module Test.Nix.Spec where

import Control.Monad (when)
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
  describe "hashes" $ do
    for_ ["", "-missing", "-mismatch"] $ \suffix -> do
      let scenario = if null suffix then "correct" else dropWhile (== '-') suffix

      specify scenario $ do
        when (scenario == "mismatch") $ do
          inNixBuild <- isJust <$> lookupEnv "NIX_ENFORCE_PURITY"
          when inNixBuild $
            pendingWith "No network access in pure nix environment"

        let
          withSuffix = fixturePath . uncurry addExtension . first (<> suffix) . splitExtension
          prefetch = ["-p" | scenario == "mismatch"]

        lock <- withSuffix "Nix/flake.lock"
        project <- withSuffix "Nix/cabal.project"
        expected <- withSuffix "Nix/hashes.golden"

        (code, out, err) <-
          readProcessWithExitCode "cleret" (["nix", "hashes", project, lock] <> prefetch) ""

        if scenario == "correct"
          then code `shouldBe` ExitSuccess
          else code `shouldNotBe` ExitSuccess
        out `shouldSatisfy` null
        goldenTest expected (T.pack err)
