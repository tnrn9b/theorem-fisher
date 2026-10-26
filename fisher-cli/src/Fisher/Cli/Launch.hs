-- | @fisher plan|launch|chat|prewarm@: managed sessions from the one-shot
-- CLI. @launch@ and @chat@ run the session behind a PTY attached to this
-- terminal until it exits; detaching and reattaching need the long-running
-- fisher shell, which arrives with A3. @launch --print@ runs it headless.
module Fisher.Cli.Launch
  ( liveInputs
  , normalizeWorkflowCommand
  , LaunchFlags (..)
  , launchRequest
  , launchHeadless
  , OwnArgs (..)
  , ownArgsCommand
  , planCommand
  , launchCommand
  , chatCommand
  , prewarmCommand
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, void, when)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BL8

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (getHomeDirectory)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..), exitWith)
import System.IO (stderr)

import Fisher.Cli.Session (cliSession)
import Fisher.Config (loadConfigAt)
import Fisher.EnvVars (getEnvValueAt)
import Fisher.Project (projectLabel, projectProver, projectRoot)
import Fisher.Prover (ProverKind (..))
import Fisher.ProjectCommand (ProjectSession (..), ProjectState (..), projectState)
import Fisher.Runlog (runlogPath)
import Fisher.Swarm
import Fisher.Swarm.Event (Progress (..))
import Fisher.Swarm.Import (importFinished, importRunId, sessionHomeOf)
import Fisher.Cli.Mine (lessonsWarn, mineImported)
import Fisher.Swarm.Types
import Fisher.Workflow.Arxiv (installArxivScript)
import Fisher.Workflow.Command (WorkflowSpec (..), rewriteForgivingCommand)
import Fisher.Workflow.Handoff (HandoffRequest (..), strictModeUnavailableReason)
import Fisher.Workflow.Json (jsonPretty)
import Fisher.Workflow.Plan
import Fisher.Workflow.Run (runHeadless)
import Fisher.Workflow.Types

-- | Inputs for a launch from this process: its environment, the config,
-- the Fisher home's .env for credentials, and the project the session points at
-- (the override, else the one found from the cwd), as upstream's
-- @_active_workflow_cwd@.
liveInputs :: IO LaunchInputs
liveInputs = do
  (home, session) <- cliSession
  (cfg, _) <- loadConfigAt home
  arxiv <- installArxivScript home
  m <- liveMachine home (Just arxiv) strictModeUnavailableReason
  env <- Map.fromList <$> getEnvironment
  st <- projectState session
  let cwd = maybe (sessionCwd session) projectRoot (stateProject st)
  pure (LaunchInputs m cfg env True (getEnvValueAt home) (Just cwd) Nothing)

-- | Accept @prove x@ as well as @/prove x@.
normalizeWorkflowCommand :: [String] -> Text
normalizeWorkflowCommand args =
  let raw = T.unwords (map T.pack args)
   in maybe raw id (rewriteForgivingCommand raw)

-- | What @fisher plan@ and @fisher launch@ take before the command.
data LaunchFlags = LaunchFlags
  { lfModel :: Maybe Text
  -- ^ @--model M@: the model the session's child runs
  , lfPrint :: Bool
  -- ^ @--print@ (@-p@): run the child headless, without a terminal
  }
  deriving (Eq, Show)

-- | The flags anywhere among the words up to a literal @--@, and the
-- command from the rest, with @--prover P@ and @--model M@ put back after
-- it, where the plan (and the Fisher shell's commands) take them. A flag
-- given twice with the same value counts once; two values are refused.
launchRequest :: [String] -> Either Text (LaunchFlags, Text)
launchRequest args0 = go args0 [] False Nothing Nothing
  where
    go ws kept pr model prov = case ws of
      [] -> done kept []
      -- a literal -- ends the flags: the words after it are text, kept
      -- after the flags put back, where the plan stops at -- too and
      -- drops it
      ("--" : rest) -> done kept rest
      (f : rest) | f `elem` ["--print", "-p"] -> go rest kept True model prov
      ("--model" : v : rest) -> setModel v rest
      ["--model"] -> Left modelUsage
      ("--prover" : v : rest) -> setProver v rest
      ["--prover"] -> Left proverUsage
      (a : rest)
        | Just v <- T.stripPrefix "--model=" (T.pack a) -> setModel (T.unpack v) rest
        | Just v <- T.stripPrefix "--prover=" (T.pack a) -> setProver (T.unpack v) rest
        | otherwise -> go rest (a : kept) pr model prov
      where
        done kept' text =
          let command = normalizeWorkflowCommand (reverse kept')
              withFlag name = maybe id (\v c -> c <> " " <> name <> " " <> v)
              flagged = withFlag "--model" model (withFlag "--prover" prov command)
           in Right (LaunchFlags model pr, if null text then flagged else flagged <> " -- " <> T.unwords (map T.pack text))
        setModel v rest = do
          m <- once "--model" modelUsage model v
          go rest kept pr (Just m) prov
        setProver v rest = do
          p <- once "--prover" proverUsage prov v
          go rest kept pr model (Just p)
    once name usage old v0 =
      let v = T.strip (T.pack v0)
       in if T.null v
            then Left usage
            else case old of
              Just o | o /= v -> Left (name <> " given twice: " <> o <> " and " <> v <> ".")
              _ -> Right v
    modelUsage = "--model takes a model name."
    proverUsage = "--prover takes lean4, rocq or isabelle."

usageFail :: Text -> IO a
usageFail e = TIO.hPutStrLn stderr ("(>_<) " <> e) >> exitWith (ExitFailure 2)

fail' :: WorkflowError -> IO a
fail' e = TIO.hPutStrLn stderr ("(>_<) " <> errorMessage e) >> exitWith (ExitFailure 1)

secretish :: String -> Bool
secretish k = any (`T.isInfixOf` T.toUpper (T.pack k)) ["KEY", "TOKEN", "SECRET", "PASSWORD"]

envChanges :: Env -> Env -> Value
envChanges base env =
  object
    [ "set" .= Object (KM.fromList
        [ (K.fromString k, String (if secretish k then "<redacted>" else T.pack v))
        | (k, v) <- Map.toList env, Map.lookup k base /= Just v ])
    , "removed" .= [T.pack k | k <- Map.keys base, Map.notMember k env] ]

requestJson :: Env -> HandoffRequest -> Value
requestJson base r =
  object
    [ "argv" .= hrArgv r, "cwd" .= hrCwd r, "env" .= envChanges base (hrEnv r)
    , "mode" .= hrMode r, "mode_note" .= hrModeNote r, "label" .= hrLabel r, "source" .= hrSource r ]

-- | The subcommands that parse their own flags, wherever they are among
-- the arguments: @check@ ('Fisher.Cli.Check.parseCheckArgs'), @plan@ and
-- @launch@ ('launchRequest'), and @scratch@ ('Fisher.Cli.Scratch'). They bypass optparse, whose subcommands
-- here stop at the first unknown option before a positional argument
-- (@noIntersperse@ replaces @forwardOptions@), so that @fisher check
-- --prover rocq A.v@ and @fisher launch --model M /prove@ ran into
-- @Invalid option@. @plan@ and @launch@ alone or with @-h@ or @--help@
-- first still go to optparse, for its usage text.
data OwnArgs = OwnCheck [String] | OwnPlan [String] | OwnLaunch [String] | OwnScratch [String]
  deriving (Eq, Show)

ownArgsCommand :: [String] -> Maybe OwnArgs
ownArgsCommand = \case
  ("check" : rest) -> Just (OwnCheck rest)
  ("plan" : rest) | wanted rest -> Just (OwnPlan rest)
  ("launch" : rest) | wanted rest -> Just (OwnLaunch rest)
  -- `fisher scratch --list` and `--statement TEXT` start with a flag
  ("scratch" : rest) | wanted rest -> Just (OwnScratch rest)
  _ -> Nothing
  where
    wanted = \case
      (a : _) -> a `notElem` ["-h", "--help"]
      [] -> False

-- | Stage a workflow and print its plan as JSON, without launching it.
-- Credential values in the environment are shown as @<redacted>@.
planCommand :: [String] -> IO ()
planCommand args = do
  (flags, command) <- either usageFail pure (launchRequest args)
  li <- liveInputs
  resolveWorkflowRequest li command >>= \case
    Left e -> fail' e
    Right p0 -> do
      p <- if lfPrint flags then either usageFail pure (headless p0) else pure p0
      BL8.putStrLn . jsonPretty $
        object
          [ "request" .= requestJson (liBaseEnv li) (wpRequest p)
          , "staged_paths" .= Object (KM.fromList [(K.fromText k, String v) | (k, v) <- stagedPaths p])
          , "workflow_kind" .= wsKind (wpSpec p), "backend_command" .= wsBackend (wpSpec p)
          , "project" .= projectLabel (wpProject p) ]

-- | Run the session attached to this terminal until it exits. A workflow
-- session's (backend, session home, cwd) is given so that its session
-- file is imported into the run log when it ends.
runAttached :: LaunchInputs -> Maybe (Text, FilePath, FilePath) -> SpawnSpec -> Text -> IO ()
runAttached li target spec announce = do
  realHome <- getHomeDirectory
  sw <- newSwarm (mIsRoot (liMachine li)) realHome
  t <- spawnInteractive sw spec
  TIO.putStrLn (announce <> " (" <> tiTaskId (taskInfo t) <> ")")
  TIO.putStrLn "This session runs attached until it exits; detach and reattach arrive with the fisher shell."
  io <- terminalAttachIO Nothing
  r <- try (attachTask t io) :: IO (Either SomeException (Either Text Int))
  -- Input closed (or the attach failed) while the session still runs: with
  -- no shell to reattach from, nothing could reach it again, so stop it.
  case r of
    Right (Right n) | n >= 0 -> pure ()
    _ -> void (cancelTask sw (tiTaskId (taskInfo t)))
  v <- waitTask t
  forM_ target $ \(backend, sessionHome, cwd) ->
    (try (fst <$> cliSession) :: IO (Either SomeException FilePath)) >>= \case
      Left e -> TIO.hPutStrLn stderr ("fisher: run log: " <> T.pack (show e))
      Right home -> do
        importFinished (runlogPath home) backend sessionHome cwd v
        -- the lessons' rules over the imported run
        when (backend /= "fisher") $ mineImported lessonsWarn home (liConfig li) (tiProjectRoot (tvInfo v)) (importRunId v)
  TIO.putStrLn ""
  TIO.putStrLn ("Session " <> tiTaskId (tvInfo v) <> " " <> statusName (tvStatus v) <> ": " <> pProgress (tvProgress v)
                  <> maybe "" (" - " <>) (pError (tvProgress v)))
  case r of
    Right (Left e) -> TIO.hPutStrLn stderr e
    Left e -> TIO.hPutStrLn stderr (T.pack (show e))
    _ -> pure ()
  exitWith (if tvStatus v == Complete then ExitSuccess else ExitFailure 1)

-- | The plan with its child in the headless form.
headless :: WorkflowPlan -> Either Text WorkflowPlan
headless p = do
  let r = wpRequest p
  argv <- printArgv (mcBackend (wpContext p)) (hrArgv r)
  pure p {wpRequest = r {hrArgv = argv}}

-- | @fisher launch --print@ with the plan from @resolve@: the planned
-- child in its headless form, stdin from /dev/null, until it exits; its
-- status is the result (2 for a usage error, 1 for a plan that fails, 127
-- when the child cannot start).
launchHeadless :: (Text -> IO (Either WorkflowError WorkflowPlan)) -> [String] -> IO ExitCode
launchHeadless resolve args = case launchRequest args of
  Left e -> TIO.hPutStrLn stderr ("(>_<) " <> e) >> pure (ExitFailure 2)
  Right (_, command) ->
    resolve command >>= \case
      Left e -> TIO.hPutStrLn stderr ("(>_<) " <> errorMessage e) >> pure (ExitFailure 1)
      Right p -> case headless p of
        Left e -> TIO.hPutStrLn stderr ("(>_<) " <> e) >> pure (ExitFailure 2)
        Right hp -> do
          let r = wpRequest hp
              ws = wpSpec hp
          TIO.hPutStrLn stderr ("Running " <> wsKind ws <> " headless for " <> projectLabel (wpProject hp) <> " on " <> mcBackend (wpContext hp) <> ".")
          runHeadless (hrArgv r) (hrCwd r) (hrEnv r) >>= \case
            Left e -> TIO.hPutStrLn stderr ("(>_<) " <> T.unwords (take 1 (hrArgv r)) <> " could not start: " <> e) >> pure (ExitFailure 127)
            Right code -> pure code

-- | Stage a workflow and run it here: attached to this terminal, or with
-- @--print@ headless, stdin from /dev/null, exiting with the child's
-- status (this command's own lines on stderr, so stdout is the child's).
launchCommand :: [String] -> IO ()
launchCommand args = do
  (flags, command) <- either usageFail pure (launchRequest args)
  li <- liveInputs
  if lfPrint flags
    then launchHeadless (resolveWorkflowRequest li) args >>= exitWith
    else resolveWorkflowRequest li command >>= \case
      Left e -> fail' e
      Right p -> do
        let r = wpRequest p
            ws = wpSpec p
            desc = let h = T.strip (T.take 60 (wpInstruction p)) in if T.null h then T.dropWhile (== '/') (wsCanonical ws) else h
            spec = SpawnSpec (if T.null (wpInstruction p) then wsBackend ws else wpInstruction p) desc (hrArgv r) (hrCwd r) (hrEnv r)
                     (wsKind ws) (wsBackend ws) (projectLabel (wpProject p)) (projectRoot (wpProject p)) (mcBackend (wpContext p)) (projectProver (wpProject p))
        runAttached li (Just (mcBackend (wpContext p), sessionHomeOf (mcBackend (wpContext p)) (mcBackendHome (wpContext p)) (hrEnv r), hrCwd r)) spec ("Spawned " <> wsKind ws <> " agent for " <> projectLabel (wpProject p) <> ": " <> desc)

-- | @/chat@: a managed backend session for onboarding and planning.
chatCommand :: [String] -> IO ()
chatCommand args = do
  li <- liveInputs
  let text = T.unwords (map T.pack args)
  resolveChatRequest li text >>= \case
    Left e -> fail' e
    Right p -> do
      let r = cpRequest p
          desc = if T.null (T.strip text) then "managed chat" else T.strip text
          spec = SpawnSpec desc desc (hrArgv r) (hrCwd r) (hrEnv r) "chat" "/chat" "" "" (cpBackend p) Lean4
      runAttached li Nothing spec ("Spawned managed chat session (" <> cpBackend p <> "): " <> desc)

-- | Fetch the lean4-skills checkout, warm the Lean MCP runtime, and set up
-- the Claude plugin in your own profile.
prewarmCommand :: IO ()
prewarmCommand = do
  li <- liveInputs
  prepareManagedRuntimeAssets li >>= \case
    Left e -> fail' e
    Right kvs -> mapM_ (\(k, v) -> TIO.putStrLn (k <> ": " <> (if T.null v then "(none)" else v))) kvs

