-- | Write a file so that a reader sees either the old contents or the new
-- ones, never a partial write: a temporary file beside the target, flushed
-- and synced, then renamed over it. A failure removes the temporary file and
-- re-raises. This is upstream's @atomic_yaml_write@ without the YAML.
module Fisher.Atomic
  ( atomicWriteFile
  , atomicWriteWith
  , secureFile
  , secureDir
  ) where

import Control.Exception (IOException, bracketOnError, try)
import Control.Monad (void)
import qualified Data.ByteString as BS
import System.Directory (createDirectoryIfMissing, removeFile, renameFile)
import System.FilePath (takeDirectory)
import System.IO (Handle, hClose, hFlush, hSetBinaryMode, openTempFile)
import System.Posix.Files (setFileMode)
import System.Posix.IO (closeFd, defaultFileFlags, openFd, OpenMode (WriteOnly))
import System.Posix.Unistd (fileSynchronise)

-- | @atomicWriteFile template path bytes@. The template names the temporary
-- file (@".config_.tmp"@ gives @.config_12345.tmp@), matching the prefixes
-- and suffixes upstream uses so its cleanup globs still apply.
atomicWriteFile :: String -> FilePath -> BS.ByteString -> IO ()
atomicWriteFile template path bytes = atomicWriteWith template path (`BS.hPut` bytes)

-- | The same, with the writer given as an action on the open handle. If it
-- throws, the target is untouched and no temporary file is left behind.
atomicWriteWith :: String -> FilePath -> (Handle -> IO ()) -> IO ()
atomicWriteWith template path write = do
  let dir = takeDirectory path
  createDirectoryIfMissing True dir
  bracketOnError (openTempFile dir template)
    (\(tmp, h) -> hClose h >> void (try (removeFile tmp) :: IO (Either IOException ())))
    (\(tmp, h) -> do
        hSetBinaryMode h True
        write h
        hFlush h
        hClose h
        fd <- openFd tmp WriteOnly defaultFileFlags
        fileSynchronise fd
        closeFd fd
        renameFile tmp path)

-- | Owner read and write only (0600). Failures are ignored, as upstream.
secureFile :: FilePath -> IO ()
secureFile p = void (try (setFileMode p 0o600) :: IO (Either IOException ()))

-- | Owner only (0700). Failures are ignored, as upstream.
secureDir :: FilePath -> IO ()
secureDir p = void (try (setFileMode p 0o700) :: IO (Either IOException ()))
