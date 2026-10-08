{-# LANGUAGE OverloadedStrings #-}

module Common.Color (
  printError,
  printWarning,
  printSuccess,
  printInfo,
  printBold,
) where

import Data.Text (Text)
import System.IO (hIsTerminalDevice, stderr, stdout)

import qualified Data.Text.IO as T

-- | Check if stdout is a terminal (for color auto-detection).
isColorTerminal :: IO Bool
isColorTerminal = hIsTerminalDevice stdout

-- ANSI escape codes
red, green, yellow, blue, bold, reset :: Text
red = "\ESC[0;31m"
green = "\ESC[0;32m"
yellow = "\ESC[0;33m"
blue = "\ESC[0;34m"
bold = "\ESC[1m"
reset = "\ESC[0m"

colorize :: Text -> Text -> IO Text
colorize code txt = do
  isTerm <- isColorTerminal
  pure $ if isTerm then code <> txt <> reset else txt

-- | Print error to stderr in red.
printError :: Text -> IO ()
printError msg = do
  colored <- colorize red ("Error: " <> msg)
  T.hPutStrLn stderr colored

-- | Print warning to stderr in yellow.
printWarning :: Text -> IO ()
printWarning msg = do
  colored <- colorize yellow ("Warning: " <> msg)
  T.hPutStrLn stderr colored

-- | Print success with green checkmark.
printSuccess :: Text -> IO ()
printSuccess msg = do
  colored <- colorize green ("✓ " <> msg)
  T.putStrLn colored

-- | Print info in blue.
printInfo :: Text -> IO ()
printInfo msg = do
  colored <- colorize blue msg
  T.putStrLn colored

-- | Print bold text.
printBold :: Text -> IO ()
printBold msg = do
  colored <- colorize bold msg
  T.putStrLn colored
