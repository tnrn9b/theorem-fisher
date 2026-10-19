-- | @tools/terminal_tool.py@: the @terminal@ tool. A command runs in the
-- task's sandbox (the one the file tools share), after the approval guard;
-- in the foreground with retries, truncation and redaction, or in the
-- background through the process registry.
module Fisher.Agent.Tools.Terminal
  ( TerminalEnv (..)
  , terminalTool
  , terminalToolDef
  , SudoPrompt (..)
  , newSudoPrompt
  , sudoPasswordFor
  , sudoBox
  , ttySudoPrompt
  ) where

import Control.Exception (SomeException, displayException, try)
import Data.IORef
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Fisher.Agent.Tools.Approval
import Fisher.Compat (lookupEnvCompatWith)
import Fisher.Paths (fisherHomeLabelWith)
import Fisher.Agent.Tools.Process
import Fisher.Agent.Tools.Web (toolSchemas)
import Fisher.Py.Re (compileOrDie, search)
import Fisher.Py.Value
import Fisher.Tools.Redact (redactEnabled, redactSensitiveText)
import Fisher.Tools.Registry (ToolCtx (..), ToolDef (..))
import Fisher.Tools.Sandbox (ExecRequest (..), ExecResult (..), Sandbox (..))
import Fisher.Tools.Sandbox.Config (EnvConfig (..), getEnvConfig)

data TerminalEnv = TerminalEnv
  { teGetEnv :: Text -> IO (Maybe Text)
  , teCwd :: IO Text
  -- ^ this process's working directory (the local default)
  , teSandbox :: Text -> IO (Either Text Sandbox)
  , teApproval :: ApprovalEnv
  , teSleep :: Double -> IO ()
  , teProcesses :: ProcessRegistry
  , teFirstUse :: IO ()
  -- ^ the cleanup thread's first pass, which registers the process tool
  , teCheck :: IO Bool
  }

out :: [(Text, PyVal)] -> IO Text
out = pure . dumpsUnicode . PDict

failed :: Text -> IO Text
failed e = out [("output", PStr ""), ("exit_code", PInt (-1)), ("error", PStr ("Failed to execute command: " <> e)), ("status", PStr "error")]

-- | @terminal_tool(command, background, timeout, task_id, workdir,
-- check_interval, pty)@
terminalTool :: TerminalEnv -> ToolCtx -> PyVal -> IO Text
terminalTool env ctx args = do
  cwdNow <- teCwd env
  ec <- getEnvConfig (teGetEnv env) cwdNow
  case ec of
    Left e -> failed e
    Right cfg -> do
      let task = if T.null (tcTaskId ctx) then "default" else tcTaskId ctx
          timeoutArg = case get "timeout" args of
            Just (PInt n) | n /= 0 -> Just n
            Just (PFloat d) | d /= 0 -> Just (round d)
            _ -> Nothing
          effTimeout = fromMaybe (ecTimeout cfg) timeoutArg
          workdir = case get "workdir" args of
            Just (PStr w) | not (T.null w) -> Just w
            _ -> Nothing
          background = maybe False truthy (get "background" args)
      teFirstUse env
      sb <- teSandbox env task
      case sb of
        Left e -> failed e
        Right sandbox -> case get "command" args of
          Just (PStr command) -> do
            verdict <- checkAllCommandGuards (teApproval env) command (ecEnvType cfg)
            if get "approved" verdict == Just (PBool True)
              then
                if background
                  then runBackground env cfg sandbox task command workdir effTimeout
                  else runForeground env sandbox command workdir effTimeout
              else blocked command verdict
          other ->
            -- upstream's retry loop slices the command for its log line
            failed ("'" <> typeName other <> "' object is not subscriptable")
  where
    typeName = \case
      Nothing -> "NoneType"
      Just PNull -> "NoneType"
      Just (PInt _) -> "int"
      Just (PFloat _) -> "float"
      Just (PBool _) -> "bool"
      Just (PList _) -> "list"
      Just (PDict _) -> "dict"
      Just (PStr _) -> "str"
    blocked command v
      | get "status" v == Just (PStr "approval_required") =
          out
            [ ("output", PStr ""), ("exit_code", PInt (-1)), ("error", fromMaybe (PStr "Waiting for user approval") (get "message" v))
            , ("status", PStr "approval_required"), ("command", fromMaybe (PStr command) (get "command" v))
            , ("description", fromMaybe (PStr "command flagged") (get "description" v)), ("pattern_key", fromMaybe (PStr "") (get "pattern_key" v)) ]
      | otherwise =
          let desc = maybe "command flagged" pyStrOf (get "description" v)
              fallback = "Command denied: " <> desc <> ". Use the approval prompt to allow it, or rephrase the command."
           in out [("output", PStr ""), ("exit_code", PInt (-1)), ("error", maybe (PStr fallback) id (get "message" v)), ("status", PStr "blocked")]

runForeground :: TerminalEnv -> Sandbox -> Text -> Maybe Text -> Integer -> IO Text
runForeground env sandbox command workdir effTimeout = go (0 :: Int)
  where
    go retry = do
      r <- try (sbExecute sandbox (ExecRequest command (fromMaybe "" workdir) (Just (fromIntegral effTimeout)) Nothing))
      case r of
        Left (e :: SomeException)
          | "timeout" `T.isInfixOf` T.toLower (T.pack (displayException e)) ->
              out [("output", PStr ""), ("exit_code", PInt 124), ("error", PStr ("Command timed out after " <> T.pack (show effTimeout) <> " seconds"))]
          | retry < 3 -> teSleep env (2 ^ (retry + 1)) >> go (retry + 1)
          | otherwise -> out [("output", PStr ""), ("exit_code", PInt (-1)), ("error", PStr ("Command execution failed: Exception: " <> T.pack (displayException e)))]
        Right res -> do
          gateway <- maybe False (not . T.null) <$> lookupEnvCompatWith (teGetEnv env) "FISHER_GATEWAY_SESSION"
          redact <- redactEnabled <$> lookupEnvCompatWith (teGetEnv env) "FISHER_REDACT_SECRETS"
          let o0 = exOutput res
              sudoBlocked = gateway && any (`T.isInfixOf` o0) ["sudo: a password is required", "sudo: no tty present", "sudo: a terminal is required"]
          home <- if sudoBlocked then T.pack <$> fisherHomeLabelWith (fmap (fmap T.unpack) . teGetEnv env . T.pack) else pure ""
          let o1 = if sudoBlocked
                then o0 <> "\n\n\128161 Tip: To enable sudo over messaging, add SUDO_PASSWORD to " <> home <> "/.env on the agent machine."
                else o0
              o2 = truncateOutput o1
              o3 = if T.null o2 then "" else (if redact then redactSensitiveText else id) (T.strip o2)
          out [("output", PStr o3), ("exit_code", PInt (fromIntegral (exCode res))), ("error", PNull)]

-- | Head 40%, tail 60% of 50,000 characters, and a notice between.
truncateOutput :: Text -> Text
truncateOutput o
  | T.length o <= 50000 = o
  | otherwise =
      let headN = 20000
          tailN = 30000
          omitted = T.length o - headN - tailN
       in T.take headN o <> "\n\n... [OUTPUT TRUNCATED - " <> T.pack (show omitted) <> " chars omitted out of " <> T.pack (show (T.length o)) <> " total] ...\n\n" <> T.takeEnd tailN o

runBackground :: TerminalEnv -> EnvConfig -> Sandbox -> Text -> Text -> Maybe Text -> Integer -> IO Text
runBackground env cfg sandbox task command workdir _effTimeout = do
  key <- fromMaybe "" <$> lookupEnvCompatWith (teGetEnv env) "FISHER_SESSION_KEY"
  let cwd = Just (fromMaybe (ecCwd cfg) workdir)
      pr = teProcesses env
  started <-
    if ecEnvType cfg == "local"
      then spawnLocal pr command cwd task key
      else Right <$> spawnViaSandbox pr sandbox command cwd task key
  case started of
    Left e -> out [("output", PStr ""), ("exit_code", PInt (-1)), ("error", PStr ("Failed to start background process: " <> e))]
    Right (sid, pid) ->
      out ([("output", PStr "Background process started"), ("session_id", PStr sid), ("pid", maybe PNull (PInt . fromIntegral) pid), ("exit_code", PInt 0), ("error", PNull)])

-- | The registry entry; @check_interval@ watchers are added by the handler.
terminalToolDef :: TerminalEnv -> ToolDef
terminalToolDef env =
  let e = fromMaybe (PDict []) (get "terminal" toolSchemas)
   in ToolDef "terminal" "terminal" (fromMaybe PNull (get "schema" e)) (handler env) (Just (teCheck env)) [] (maybe "" pyStrOf (get "emoji" e))

handler :: TerminalEnv -> ToolCtx -> PyVal -> IO Text
handler env ctx args = do
  r <- terminalTool env ctx args
  -- the watcher note and registration for a background start
  case (maybe False truthy (get "background" args), get "check_interval" args, loads' r) of
    (True, Just (PInt ci), Just d@(PDict kvs)) | ci /= 0, Just (PStr sid) <- get "session_id" d -> do
      key <- fromMaybe "" <$> lookupEnvCompatWith (teGetEnv env) "FISHER_SESSION_KEY"
      platform <- fromMaybe "" <$> lookupEnvCompatWith (teGetEnv env) "FISHER_SESSION_PLATFORM"
      chat <- fromMaybe "" <$> lookupEnvCompatWith (teGetEnv env) "FISHER_SESSION_CHAT_ID"
      thread <- fromMaybe "" <$> lookupEnvCompatWith (teGetEnv env) "FISHER_SESSION_THREAD_ID"
      addWatcher (teProcesses env) (PDict [("session_id", PStr sid), ("check_interval", PInt (max 30 ci)), ("session_key", PStr key), ("platform", PStr platform), ("chat_id", PStr chat), ("thread_id", PStr thread)])
      pure (dumpsUnicode (PDict (kvs <> [("check_interval_note", PStr ("Requested " <> T.pack (show ci) <> "s raised to minimum 30s")) | ci < 30])))
    _ -> pure r
  where
    loads' t = either (const Nothing) Just (loads (TE.encodeUtf8 t))

-- ------------------------------------------------------------ sudo

-- | Where the sudo password comes from: @SUDO_PASSWORD@, the session's
-- cached answer, or (interactive) a prompt.
data SudoPrompt = SudoPrompt
  { spGetEnv :: Text -> IO (Maybe Text)
  , spCache :: IORef Text
  , spAsk :: IO Text
  -- ^ the prompt (callback or the box on the terminal); "" to skip
  }

newSudoPrompt :: (Text -> IO (Maybe Text)) -> IO Text -> IO SudoPrompt
newSudoPrompt getEnv ask = SudoPrompt getEnv <$> newIORef "" <*> pure ask

-- | The password for this command ("" when it has no @sudo@ or none is
-- known).
sudoPasswordFor :: SudoPrompt -> Text -> IO Text
sudoPasswordFor sp command
  | not (isJust (search sudoRe command)) = pure ""
  | otherwise = do
      env <- fromMaybe "" <$> spGetEnv sp "SUDO_PASSWORD"
      cached <- readIORef (spCache sp)
      let known = if T.null env then cached else env
      if not (T.null known)
        then pure known
        else do
          interactive <- maybe False (not . T.null) <$> lookupEnvCompatWith (spGetEnv sp) "FISHER_INTERACTIVE"
          if not interactive
            then pure ""
            else do
              pw <- spAsk sp
              if T.null pw then pure "" else writeIORef (spCache sp) pw >> pure pw
  where
    sudoRe = compileOrDie [] "\\bsudo\\b"

-- | @_prompt_for_sudo_password@'s box, before the hidden read.
sudoBox :: Int -> [Text]
sudoBox timeoutSecs =
  [ ""
  , "\9484" <> T.replicate 58 "\9472" <> "\9488"
  , "\9474  \128272 SUDO PASSWORD REQUIRED" <> T.replicate 30 " " <> "\9474"
  , "\9500" <> T.replicate 58 "\9472" <> "\9508"
  , "\9474  Enter password below (input is hidden), or:            \9474"
  , "\9474    \8226 Press Enter to skip (command fails gracefully)     \9474"
  , "\9474    \8226 Wait " <> T.pack (show timeoutSecs) <> "s to auto-skip" <> T.replicate 27 " " <> "\9474"
  , "\9492" <> T.replicate 58 "\9472" <> "\9496"
  , "" ]

-- | The prompt without a UI callback: the box, then a hidden read
-- (@Nothing@ when the 45 seconds ran out; a failed read is an empty
-- answer).
ttySudoPrompt :: (Text -> IO ()) -> IO (Maybe Text) -> IO Text
ttySudoPrompt raw readHidden = do
  mapM_ (\l -> raw (l <> "\n")) (sudoBox 45)
  raw "  Password (hidden): "
  r <- readHidden
  case r of
    Just pw -> do
      raw "\n"
      raw (if T.null pw then "  \9197 Skipped - continuing without sudo\n" else "  \10003 Password received (cached for this session)\n")
      raw "\n"
      pure pw
    Nothing -> do
      raw "\n  \9201 Timeout - continuing without sudo\n"
      raw "    (Press Enter to dismiss)\n"
      raw "\n"
      pure ""
