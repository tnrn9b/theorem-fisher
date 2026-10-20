-- | The A3 one-shot commands: @sessions@, @config show@, @config edit@ and
-- @version@, wired to the live environment.
module Fisher.Cli.Subcommands
  ( sessionsCli
  , configShowCli
  , configEditCli
  , versionCli
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (getCurrentTimeZone)
import System.Directory (doesFileExist, findExecutable)
import System.Environment (getExecutablePath, lookupEnv)
import System.FilePath (takeDirectory)
import System.IO (hFlush, stdout)
import System.Process (callProcess)

import Fisher.Cli.ConfigShow
import Fisher.Cli.Sessions
import Fisher.Config (defaultConfig, loadConfigAt, saveConfigAt)
import Fisher.EnvVars (getEnvValueAt)
import Fisher.Paths (configPathIn, ensureFisherHome, envPathIn)
import Fisher.Shell.Data (upstreamReleaseDate, upstreamVersion)
import Fisher.Store (storePath)
import Fisher.Swarm (nowSeconds)
import Fisher.Term (detectStyle)

sessionsCli :: SessionsCmd -> IO ()
sessionsCli cmd = do
  home <- ensureFisherHome
  now <- nowSeconds
  tz <- getCurrentTimeZone
  let ask p = do
        TIO.putStr p
        hFlush stdout
        l <- TIO.getLine
        pure (Just l)
  runSessions (SessionsEnv (storePath home) now tz TIO.putStrLn ask) cmd

-- | Process environment first, then the Fisher home's .env (@get_env_value@).
envValue :: FilePath -> T.Text -> IO (Maybe T.Text)
envValue home k = lookupEnv (T.unpack k) >>= \case
  Just v -> pure (Just (T.pack v))
  Nothing -> getEnvValueAt home k

configShowCli :: IO ()
configShowCli = do
  home <- ensureFisherHome
  (cfg, _) <- loadConfigAt home
  st <- detectStyle
  exe <- getExecutablePath
  ls <- showConfigLines (ShowEnv st cfg (configPathIn home) (envPathIn home) (takeDirectory exe) (envValue home))
  mapM_ TIO.putStrLn ls

configEditCli :: IO ()
configEditCli = do
  home <- ensureFisherHome
  let path = configPathIn home
  editConfig
    EditEnv
      { edConfigPath = path
      , edConfigExists = doesFileExist path
      , edWriteDefault = saveConfigAt home defaultConfig
      , edEnv = \k -> fmap T.pack <$> lookupEnv (T.unpack k)
      , edWhich = findExecutable . T.unpack
      , edRun = \e args -> callProcess (T.unpack e) (map T.unpack args)
      , edPut = TIO.putStrLn
      }

versionCli :: IO ()
versionCli = do
  TIO.putStrLn "Fisher 0.2.0"
  TIO.putStrLn ("Compatible with upstream v" <> upstreamVersion <> " (" <> upstreamReleaseDate <> ")")
  exe <- getExecutablePath
  TIO.putStrLn ("Executable: " <> T.pack exe)
