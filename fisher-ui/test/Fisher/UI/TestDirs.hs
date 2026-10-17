{-# LANGUAGE TemplateHaskell #-}

-- | Directories the tests read, fixed at compile time.
module Fisher.UI.TestDirs
  ( uiDir
  , fixturesDir
  ) where

import Data.FileEmbed (makeRelativeToProject)
import Language.Haskell.TH.Syntax (lift)

-- | Where @ui/@ is (for the freshness test).
uiDir :: FilePath
uiDir = $(makeRelativeToProject "../ui" >>= lift)

-- | @fisher-ui/test/fixtures@.
fixturesDir :: FilePath
fixturesDir = $(makeRelativeToProject "test/fixtures" >>= lift)
