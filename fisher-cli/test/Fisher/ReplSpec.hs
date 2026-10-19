-- | The fisher shell against upstream's REPL (@repl.json@): each input in
-- each recorded session, with the same world, configuration and swarm,
-- printing what upstream printed, spawning what it spawned and saving what
-- it saved.
module Fisher.ReplSpec (spec) where

import Control.Monad (forM_, unless, when)
import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Time (UTCTime (..), fromGregorian)
import qualified Data.Yaml as Y
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getEnvironment, lookupEnv, setEnv)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO.Temp (withTempDirectory)
import Test.Hspec

import Fisher.A3Golden
import Fisher.A3World
import Fisher.Config (loadConfigAt)
import Fisher.EnvVars (getEnvValueAt)
import Fisher.Project (formatProjectSummary, projectLabel, projectProverRoot, projectRoot)
import Fisher.ProjectCommand (ProjectSession (..), ProjectState (..), handleProjectTokens, projectState)
import Fisher.Shell (resolvedChat, resolvedWorkflow)
import Fisher.Shell.CliConfig (loadCliConfig, saveConfigValueAt)
import Fisher.Shell.Commands
import Fisher.Swarm (SpawnSpec (..))
import Fisher.Term (Style (..), renderText)
import Fisher.Workflow.Command (rewriteForgivingCommand)
import Fisher.Workflow.Plan (LaunchInputs (..), resolveChatRequest, resolveWorkflowRequest)
import Fisher.Workflow.Settings (backendName, normalizeBackendName, supportedBackends)
import Fisher.Workflow.Types (Machine (..), errorMessage)

-- | Lines that name the swarm table: upstream's title says Gauss, and
-- centring a longer name moves it.
titleFix :: Text -> Text
titleFix = T.unlines . map fix . T.lines
  where
    fix l
      | "Workflow Swarm" `T.isInfixOf` l = T.replace "Gauss" "Fisher" (T.strip l)
      | otherwise = l

machine :: FilePath -> Machine
machine gh =
  Machine False (pure (Just "strict mode requires a controlling TTY (No such device or address)")) (pure Nothing)
    (pure (UTCTime (fromGregorian 2026 10 1) 0)) (pure "20261001-120000") gh Nothing (\_ -> pure ())

data Spawned = Spawned [(Text, Text)]

spec :: Spec
spec = do
  cases <- runIO (a3Cases "repl.json")
  describe "the fisher shell, against upstream's REPL" $
    forM_ cases $ \c -> it (T.unpack (str "name" c)) $ withTempDirectory "/tmp" "fa3r-" $ \tmp -> withTempDirectory "/var/tmp" "fa3g-" $ \tmpg -> do
      w <- makeWorld tmp tmpg
      let gh = wFisherHome w
          cwd = wRoot w </> T.unpack (str "cwd" c)
          envExtra = case obj "env" c of
            Object o -> [(K.toString k, T.unpack v) | (k, String v) <- KM.toList o]
            _ -> []
          configPath = gh </> "config.yaml"
      Y.encodeFile configPath (case obj "config" c of Null -> Object KM.empty; v -> v)
      case obj "files" c of
        Object o -> forM_ (KM.toList o) $ \(rel, v) -> case v of
          String s -> do
            createDirectoryIfMissing True (takeDirectory (gh </> K.toString rel))
            TIO.writeFile (gh </> K.toString rel) s
          _ -> pure ()
        _ -> pure ()
      withWorldEnv w envExtra $ do
        (cfg1, _) <- loadConfigAt gh
        (cliCfg, _) <- loadCliConfig configPath
        env <- getEnvironment
        override <- newIORef Nothing
        let session = ProjectSession cwd cfg1 env override
        out <- newIORef []
        tasks <- newIORef []
        allow <- newIORef True
        spawned <- newIORef []
        let put t = modifyIORef out (t :)
            spawn s = do
              ok <- readIORef allow
              n <- length <$> readIORef tasks
              let tid = "af-" <> T.justifyRight 3 '0' (T.pack (show (n + 1)))
              when ok $ do
                modifyIORef tasks (<> [ TaskRow tid (ssDescription s) (ssTheorem s) (ssWorkflowKind s) (ssWorkflowCommand s)
                                          (ssProjectName s) (T.pack (ssProjectRoot s)) (T.pack (ssCwd s)) (ssBackendName s)
                                          "running" (Just 1790000000) Nothing "Waiting" Nothing Nothing Nothing Nothing False ])
                modifyIORef spawned (<> [ [ ("task_id", tid), ("theorem", ssTheorem s), ("description", ssDescription s)
                                          , ("argv0", maybe "" (T.pack . takeFileName . T.unpack) (listToMaybe (ssArgv s)))
                                          , ("cwd", T.pack (ssCwd s)), ("workflow_kind", ssWorkflowKind s)
                                          , ("workflow_command", ssWorkflowCommand s), ("project_name", ssProjectName s)
                                          , ("backend_name", ssBackendName s) ] ])
              pure tid
            inputs active = do
              envMap <- Map.fromList <$> getEnvironment
              pure (LaunchInputs (machine gh) cfg1 envMap True (getEnvValueAt gh) (Just (T.unpack active)))
            deps =
              ShellDeps
                { dOut = Out put, dWidth = pure 80, dAnsi = False, dUnicode = True
                , dRenderTerminal = renderText (Style False True), dFisherHome = gh, dConfigPath = configPath
                , dProject = do
                    ps <- projectState session
                    case stateProject ps of
                      Nothing -> pure (ProjectInfo Nothing (Just (stateSource ps)) (stateError ps))
                      Just p -> do
                        summary <- formatProjectSummary p (Just cwd)
                        pure (ProjectInfo (Just (ProjectView (projectLabel p) (T.pack (projectRoot p)) (T.pack (projectProverRoot p)) summary)) (Just (stateSource ps)) Nothing)
                , dProjectCommand = handleProjectTokens session
                , dHandoffCwd = pure (T.pack cwd)
                , dEnv = \k -> fmap T.pack <$> lookupEnv (T.unpack k)
                , dSetEnv = \k v -> setEnv (T.unpack k) (T.unpack v)
                , dSaveConfigValue = saveConfigValueAt configPath
                , dResolveWorkflow = \cmd active -> do
                    li <- inputs active
                    either (Left . errorMessage) (Right . (`resolvedWorkflow` spawn)) <$> resolveWorkflowRequest li cmd
                , dResolveChat = \payload active -> do
                    li <- inputs active
                    either (Left . errorMessage) (Right . (`resolvedChat` spawn)) <$> resolveChatRequest li payload
                , dNormalizeBackend = fmap backendName . normalizeBackendName
                , dBackends = map backendName supportedBackends
                , dTasks = readIORef tasks
                , dTask = \i -> listToMaybe . filter ((== i) . trId) <$> readIORef tasks
                , dCancel = \i -> do
                    ts <- readIORef tasks
                    if any (\t -> trId t == i && trStatus t == "running") ts
                      then writeIORef tasks [if trId t == i then t {trStatus = "cancelled"} else t | t <- ts] >> pure True
                      else pure False
                , dAttach = \_ -> pure (Right ()), dWaitAttachable = \_ -> pure False
                , dNow = pure 1790000000, dClear = put "<ERASE-SCREEN>"
                , dNewSessionId = pure "20261001_120000_abcdef", dStartedAt = "2026-10-01 12:00:00"
                , dRunQuick = \_ -> pure (Right ""), dForgiving = rewriteForgivingCommand
                , dChat = \_ _ -> pure (), dChatExit = pure False }
        st <- newShellState deps cliCfg "20261001_120000_abcdef"
        let steps = arr "steps" c
            go [] = pure ()
            go (step : rest) = do
              writeIORef out []
              writeIORef spawned []
              let input = str "input" step
                  plainAgent = obj "plain" step == String "agent"
                  wantSpawns = arr "spawned" step
              writeIORef allow (not (null wantSpawns))
              continue <- submit deps st input
              mine <- normalizeWorld w . T.concat . map ((<> "\n") . T.stripEnd) . reverse <$> readIORef out
              let want = str "output" step
                  label = T.unpack input
              unless plainAgent $
                if null wantSpawns && T.length (titleFix mine) > T.length (titleFix want) && "/chat" `T.isPrefixOf` str "dispatched" step
                  then (label, titleFix want `T.isPrefixOf` titleFix mine) `shouldBe` (label, True)
                  else (label, titleFix mine) `shouldBe` (label, titleFix want)
              got <- readIORef spawned
              (label, map (map (fmap (normalizeWorld w))) got)
                `shouldBe` (label, [[(k, str k sp) | k <- ["task_id", "theorem", "description", "argv0", "cwd", "workflow_kind", "workflow_command", "project_name", "backend_name"]] | sp <- wantSpawns])
              case mstr "config_after" step of
                Just yamlText -> do
                  actual <- Y.decodeFileThrow configPath :: IO Value
                  expected <- either (fail . show) pure (Y.decodeEither' (TE.encodeUtf8 yamlText))
                  (label, actual) `shouldBe` (label, expected)
                Nothing -> pure ()
              case mstr "env_backend" step of
                Just b -> do
                  (fmap T.pack <$> lookupEnv "FISHER_AUTOFORMALIZE_BACKEND") `shouldReturn` Just b
                  (fmap T.pack <$> lookupEnv "GAUSS_AUTOFORMALIZE_BACKEND") `shouldReturn` Just b
                Nothing -> pure ()
              if continue then go rest else pure ()
        go steps
        writeIORef out []
        goodbye deps st
        T.concat . map (<> "\n") . reverse <$> readIORef out >>= (`shouldBe` str "goodbye" c)
  where
    _ = (fromMaybe, doesFileExist, Spawned [])
