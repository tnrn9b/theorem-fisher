module Fisher.TestUtil (withTmp, writeLeanRoot) where

import System.Directory (canonicalizePath, createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

-- | A fresh directory, canonical so that comparisons survive /var being a
-- symlink on macOS.
withTmp :: (FilePath -> IO a) -> IO a
withTmp k = withSystemTempDirectory "fisher" (\d -> canonicalizePath d >>= k)

writeLeanRoot :: FilePath -> IO ()
writeLeanRoot p = do
  createDirectoryIfMissing True p
  writeFile (p </> "lakefile.lean") "-- lean project\n"
