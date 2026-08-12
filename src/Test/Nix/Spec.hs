{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Test.Nix.Spec where

import Data.Default
import GHC.Stack
import Lens.Micro
import Nix.Command
import Test.Hspec

checkLens ::
  forall s a.
  (Default s, Eq s, Eq a, Show s, Show a, HasCallStack) =>
  Lens' s a -> s -> a -> Expectation
checkLens l s a = do
  s ^. l `shouldBe` a
  (def & l .~ a) `shouldBe` s

spec :: Spec
spec = do
  describe "urls" $ do
    specify "inputTypeAndHost" $ do
      checkLens
        inputTypeAndHost
        def {inputType_ = "gitlab"}
        ("gitlab", "gitlab.com")
      checkLens
        inputTypeAndHost
        def {inputType_ = "gitlab", inputHost_ = Just "gitlab.haskell.org"}
        ("gitlab", "gitlab.haskell.org")
    specify "inputUrl" $ do
      checkLens
        inputUrl
        def {inputType_ = "gitlab", inputOwner_ = "ghc", inputRepo_ = "head.hackage"}
        "https://gitlab.com/ghc/head.hackage.git"
      checkLens
        inputUrl
        def
          { inputType_ = "gitlab"
          , inputHost_ = Just "gitlab.haskell.org"
          , inputOwner_ = "ghc"
          , inputRepo_ = "head.hackage"
          }
        "https://gitlab.haskell.org/ghc/head.hackage.git"
