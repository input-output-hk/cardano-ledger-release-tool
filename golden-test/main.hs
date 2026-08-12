module Main where

import Test.Hspec

import qualified Test.Cabal.Spec as Cabal
import qualified Test.Changelogs.Spec as Changelogs
import qualified Test.Failures.Spec as Failures
import qualified Test.Nix.Spec as Nix
import qualified Test.Workflow.Spec as Workflow

main :: IO ()
main = hspec $ do
  describe "cabal" Cabal.spec
  describe "changelogs" Changelogs.spec
  describe "failures" Failures.spec
  describe "nix" Nix.spec
  describe "workflow" Workflow.spec
