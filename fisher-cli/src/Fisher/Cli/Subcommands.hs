-- | The A3 one-shot commands: @sessions@, @config show@, @config edit@ and
-- @version@, and @runs@, @learn@ and @lessons@, wired to the live
-- environment.
module Fisher.Cli.Subcommands
  ( sessionsCli
  , runsCli
  , learnCli
  , lessonsCli
  , configShowCli
  , configEditCli
  , versionCli
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (getCurrentTimeZone)
import System.Directory (doesFileExist, findExecutable, getCurrentDirectory)
import System.Environment (getExecutablePath, lookupEnv)
import System.FilePath (takeDirectory)
import System.IO (hFlush, stdout)
import System.Process (callProcess)

import Fisher.Cli.ConfigShow
import Fisher.Cli.Learn
import Fisher.Cli.Lessons
import Fisher.Cli.Runs
import Fisher.Cli.Search (cascadeFor, defaultRunArgs, liveJudge, searchTiersConfig)
import Fisher.Cli.Sessions
import Fisher.Config (defaultConfig, loadConfigAt, saveConfigAt)
import Fisher.EnvVars (getEnvValueAt)
import Fisher.Paths (configPathIn, ensureFisherHome, envPathIn)
import Fisher.Runlog (runlogPath)
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

-- | @fisher runs@ over the Fisher home's run log and state.db.
runsCli :: RunsCmd -> IO ()
runsCli cmd = do
  home <- ensureFisherHome
  runRuns (RunsEnv (runlogPath home) (storePath home) nowSeconds TIO.putStrLn) cmd

-- | @fisher learn@ over the Fisher home's run log and policy files, for
-- the working directory's project unless @--project@ names one.
learnCli :: LearnCmd -> IO ()
learnCli cmd = do
  home <- ensureFisherHome
  (cfg, _) <- loadConfigAt home
  cwd <- getCurrentDirectory
  runLearn (LearnEnv (runlogPath home) home cwd cfg nowSeconds TIO.putStrLn) cmd

-- | @fisher lessons@ over the Fisher home's run log and lessons files, for
-- the working directory's project unless @--project@ names one. The judge
-- (built only for @mine@, with the judge on) is the search's: the lemma
-- graph planner's route for the unflagged configuration, none with one
-- warning when it cannot be built.
lessonsCli :: LessonsCmd -> IO ()
lessonsCli cmd = do
  home <- ensureFisherHome
  (cfg, _) <- loadConfigAt home
  cwd <- getCurrentDirectory
  let judge = liveJudge (cascadeFor defaultRunArgs (searchTiersConfig cfg))
  runLessons
    LessonsEnv
      { lsRunlogPath = runlogPath home, lsFisherHome = home, lsCwd = cwd, lsConfig = cfg, lsNow = nowSeconds
      , lsPut = TIO.putStrLn, lsJudge = judge }
    cmd

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
