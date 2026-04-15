module Test.Common.Fixture where

import Paths_cardano_ledger_release_tool (getDataFileName)
import System.FilePath ((</>))

fixturePath :: FilePath -> IO FilePath
fixturePath relPath = getDataFileName $ "test/fixtures" </> relPath
