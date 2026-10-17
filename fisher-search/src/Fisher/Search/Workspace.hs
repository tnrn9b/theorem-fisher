-- | A Lake project copied for one attempt (or for the swarm's own base):
-- the sources copied (copy-on-write clones on macOS), the project's own
-- build outputs cloned so the copy starts built, the dependency packages
-- shared through a symlink, and a git repository with one commit so
-- workflows that look at history find one.
module Fisher.Search.Workspace
  ( makeWorkspace
  ) where

import Control.Monad (forM_, unless, when)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Info (os)
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)

-- | Copy the project at @root@ to @dest@ (which must not exist), with
-- @text@ as the content of @rel@.
makeWorkspace :: FilePath -> FilePath -> FilePath -> T.Text -> IO ()
makeWorkspace root dest rel text = do
  createDirectoryIfMissing True dest
  entries <- listDirectory root
  forM_ [e | e <- entries, e `notElem` [".lake", ".git"]] $ \e -> copyTree (root </> e) (dest </> e)
  let lake = root </> ".lake"
  hasLake <- doesDirectoryExist lake
  when hasLake $ do
    createDirectoryIfMissing True (dest </> ".lake")
    hasBuild <- doesDirectoryExist (lake </> "build")
    when hasBuild (copyTree (lake </> "build") (dest </> ".lake" </> "build"))
    hasPkgs <- doesDirectoryExist (lake </> "packages")
    when hasPkgs $ do
      target <- canonicalizePath (lake </> "packages")
      createFileLink target (dest </> ".lake" </> "packages")
  BS.writeFile (dest </> rel) (TE.encodeUtf8 text)
  ignore <- doesFileExist (dest </> ".gitignore")
  unless ignore (writeFile (dest </> ".gitignore") ".lake/\n")
  git dest ["init", "-q"]
  git dest ["add", "-A"]
  git dest ["-c", "user.name=Fisher search", "-c", "user.email=search@fisher.invalid", "commit", "-q", "-m", "attempt start"]

-- | @cp -cR@ (APFS clone) on macOS, @cp -R --reflink=auto@ elsewhere.
copyTree :: FilePath -> FilePath -> IO ()
copyTree from to = do
  let args = if os == "darwin" then ["-cR", from, to] else ["-R", "--reflink=auto", from, to]
  (code, _, err) <- readCreateProcessWithExitCode (proc "cp" args) ""
  case code of
    ExitSuccess -> pure ()
    ExitFailure _ -> ioError (userError ("cp " <> unwords args <> ": " <> err))

git :: FilePath -> [String] -> IO ()
git dir args = do
  (code, _, err) <- readCreateProcessWithExitCode ((proc "git" args) {cwd = Just dir}) ""
  case code of
    ExitSuccess -> pure ()
    ExitFailure _ -> ioError (userError ("git " <> unwords args <> ": " <> err))
