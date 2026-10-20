-- | The project session for the one-shot @fisher@ CLI. Upstream keeps the
-- @/project@ override in the REPL's memory; a command that exits after one
-- line has nowhere to keep it, so Fisher keeps it in
-- @$FISHER_HOME/fisher/project-override@ until @fisher project clear@.
module Fisher.Cli.Session
  ( cliSession
  , saveOverride
  , activeCwd
  ) where

import Control.Exception (IOException, try)
import Data.IORef (newIORef)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory, removeFile)
import System.Environment (getEnvironment, lookupEnv)
import System.FilePath ((</>))

import Fisher.Config (loadConfigAt)
import Fisher.Paths (ensureFisherHome)
import Fisher.ProjectCommand (ProjectSession (..))

overrideFile :: FilePath -> FilePath
overrideFile home = home </> "fisher" </> "project-override"

-- | @TERMINAL_CWD@ when set, else the working directory.
activeCwd :: IO FilePath
activeCwd = lookupEnv "TERMINAL_CWD" >>= \case
  Just c | not (null c) -> pure c
  _ -> getCurrentDirectory

cliSession :: IO (FilePath, ProjectSession)
cliSession = do
  home <- ensureFisherHome
  (cfg, _) <- loadConfigAt home
  env <- getEnvironment
  cwd <- activeCwd
  let f = overrideFile home
  exists <- doesFileExist f
  override <- if exists then Just . T.unpack . T.strip <$> TIO.readFile f else pure Nothing
  ref <- newIORef (case override of
    Just p | not (null p) -> Just p
    _ -> Nothing)
  pure (home, ProjectSession cwd cfg env ref)

saveOverride :: FilePath -> Maybe FilePath -> IO ()
saveOverride home = \case
  Just p -> do
    createDirectoryIfMissing True (home </> "fisher")
    TIO.writeFile (overrideFile home) (T.pack p <> "\n")
  Nothing -> do
    _ <- try (removeFile (overrideFile home)) :: IO (Either IOException ())
    pure ()
