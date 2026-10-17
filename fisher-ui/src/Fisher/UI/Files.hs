-- | Writing into a project from the UI: the file must be inside the
-- project (not a link, its directory inside the root once links are
-- resolved), and it is written as a new file beside it, renamed over it.
module Fisher.UI.Files
  ( isUnder
  , contained
  , writeOver
  ) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import Data.List (isPrefixOf)
import qualified Data.Text as T
import System.Directory (canonicalizePath, getPermissions, pathIsSymbolicLink, renameFile, setPermissions)
import System.FilePath (addTrailingPathSeparator, takeDirectory, takeFileName, (</>))

import Fisher.UI.Children (newTag)

-- | Whether a directory, links resolved, is the root or below it (the
-- root canonical).
isUnder :: FilePath -> FilePath -> IO Bool
isUnder root0 dir = do
  root <- canonicalizePath root0
  r <- try @SomeException (canonicalizePath dir)
  pure $ case r of
    Right d -> d == root || (addTrailingPathSeparator root) `isPrefixOf` d
    Left _ -> False

-- | Whether a file is in the project: not a link itself, and its
-- directory (links resolved) inside the project root.
contained :: FilePath -> FilePath -> IO Bool
contained root path = do
  link <- either (\(_ :: SomeException) -> False) id <$> try (pathIsSymbolicLink path)
  if link then pure False else isUnder root (takeDirectory path)

-- | Write the bytes over the file (a new file beside it, then renamed in
-- place, its mode kept).
writeOver :: FilePath -> BS.ByteString -> IO ()
writeOver path bytes = do
  tag <- newTag
  let tmp = takeDirectory path </> ("." <> takeFileName path <> ".fisher-ui-" <> T.unpack tag)
  BS.writeFile tmp bytes
  perms <- try @SomeException (getPermissions path)
  either (const (pure ())) (setPermissions tmp) perms
  renameFile tmp path
