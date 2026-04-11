module Test.Common.Golden (goldenTest) where

import Data.List (intercalate)
import Data.Text (Text)
import System.Environment (lookupEnv)
import System.FilePath (dropExtension, (<.>))
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..))
import Test.Hspec
import UnliftIO.Exception (catch, catchIO, throwIO)

import qualified Data.Text.IO as T

goldenTest :: HasCallStack => FilePath -> Text -> Expectation
goldenTest goldenPath actualText = do
  update <- lookupEnv "UPDATE_GOLDEN"
  case update of
    Just _ -> do
      T.writeFile goldenPath actualText
    Nothing -> do
      expectedText <- T.readFile goldenPath
      (actualText `shouldBe` expectedText)
        `catch` \(HUnitFailure loc reason) -> do
          let
            actualPath = dropExtension goldenPath <.> "actual"
            msg =
              intercalate
                "\n"
                [ "Actual output is in " <> actualPath
                , "Golden output is in " <> goldenPath
                ]
          T.writeFile actualPath actualText
            `catchIO` mempty
          throwIO $ HUnitFailure loc (addContext msg reason)

addContext :: String -> FailureReason -> FailureReason
addContext msg (ExpectedButGot mPrev e g) = ExpectedButGot (Just msg <> fmap ("\n" <>) mPrev) e g
addContext msg (Reason r) = Reason (msg <> "\n" <> r)
