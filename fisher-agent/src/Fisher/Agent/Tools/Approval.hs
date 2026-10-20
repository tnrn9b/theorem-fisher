-- | @tools/approval.py@ and @tools/tirith_security.py@: the guard every
-- terminal command passes before it runs. Dangerous-pattern detection,
-- per-session and permanent approvals, the approval prompt, smart
-- approval by the auxiliary model, and the tirith scan (run when tirith is
-- installed; Fisher never downloads it).
module Fisher.Agent.Tools.Approval
  ( ApprovalEnv (..)
  , ApprovalState
  , newApprovalState
  , Choice (..)
  , detectDangerousCommand
  , dangerousPatterns
  , checkAllCommandGuards
  , TirithEnv (..)
  , tirithCheck
  , textPrompt
  , approvedSession
  , approvedPermanent
  , pendingApprovals
  , configApprovalMode
  , configSecurity
  , saveAllowlist
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import qualified Fisher.Config as Cfg
import Fisher.Config (loadConfigAt, saveConfigAt)

import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Text.Read (readMaybe)

import Fisher.Agent.Tools.Browser (RunOutcome (..))
import Fisher.Agent.Types
import Fisher.Py.Re (Flag (..), Regex, compileOrDie, search)
import Fisher.Py.Value

-- ------------------------------------------------------------ patterns

dangerousPatterns :: [(Text, Text)]
dangerousPatterns =
  [ ("\\brm\\s+(-[^\\s]*\\s+)*/", "delete in root path")
  , ("\\brm\\s+-[^\\s]*r", "recursive delete")
  , ("\\brm\\s+--recursive\\b", "recursive delete (long flag)")
  , ("\\bchmod\\s+(-[^\\s]*\\s+)*777\\b", "world-writable permissions")
  , ("\\bchmod\\s+--recursive\\b.*777", "recursive world-writable (long flag)")
  , ("\\bchown\\s+(-[^\\s]*)?R\\s+root", "recursive chown to root")
  , ("\\bchown\\s+--recursive\\b.*root", "recursive chown to root (long flag)")
  , ("\\bmkfs\\b", "format filesystem")
  , ("\\bdd\\s+.*if=", "disk copy")
  , (">\\s*/dev/sd", "write to block device")
  , ("\\bDROP\\s+(TABLE|DATABASE)\\b", "SQL DROP")
  , ("\\bDELETE\\s+FROM\\b(?!.*\\bWHERE\\b)", "SQL DELETE without WHERE")
  , ("\\bTRUNCATE\\s+(TABLE)?\\s*\\w", "SQL TRUNCATE")
  , (">\\s*/etc/", "overwrite system config")
  , ("\\bsystemctl\\s+(stop|disable|mask)\\b", "stop/disable system service")
  , ("\\bkill\\s+-9\\s+-1\\b", "kill all processes")
  , ("\\bpkill\\s+-9\\b", "force kill processes")
  , (":\\(\\)\\s*\\{\\s*:\\s*\\|\\s*:\\s*&\\s*\\}\\s*;\\s*:", "fork bomb")
  , ("\\b(bash|sh|zsh)\\s+-c\\s+", "shell command via -c flag")
  , ("\\b(python[23]?|perl|ruby|node)\\s+-[ec]\\s+", "script execution via -e/-c flag")
  , ("\\b(curl|wget)\\b.*\\|\\s*(ba)?sh\\b", "pipe remote content to shell")
  , ("\\b(bash|sh|zsh|ksh)\\s+<\\s*<?\\s*\\(\\s*(curl|wget)\\b", "execute remote script via process substitution")
  , ("\\btee\\b.*(/etc/|/dev/sd|\\.ssh/|\\.fisher/\\.env)", "overwrite system file via tee")
  , ("\\bxargs\\s+.*\\brm\\b", "xargs with rm")
  , ("\\bfind\\b.*-exec\\s+(/\\S*/)?rm\\b", "find -exec rm")
  , ("\\bfind\\b.*-delete\\b", "find -delete")
  ]

-- | @detect_dangerous_command@: the first pattern the lowered command
-- matches (IGNORECASE, DOTALL); its description is also its key.
detectDangerousCommand :: Text -> Maybe Text
detectDangerousCommand cmd =
  case [d | (re, d) <- compiled, isJust (search re (T.toLower cmd))] of
    (d : _) -> Just d
    [] -> Nothing

compiled :: [(Re', Text)]
compiled = [(compileOrDie [IgnoreCase, DotAll] p, d) | (p, d) <- dangerousPatterns]

type Re' = Regex

-- | Every key an approval for this pattern also answers to: its
-- description and the regex-derived key older allowlists used.
aliases :: Text -> S.Set Text
aliases key = fromMaybe (S.singleton key) (M.lookup key aliasMap)

aliasMap :: M.Map Text (S.Set Text)
aliasMap =
  M.fromListWith S.union (concat [[(d, S.fromList [d, l]), (l, S.fromList [l, d])] | (p, d) <- dangerousPatterns, let l = legacyKey p])
  where
    legacyKey p = case T.splitOn "\\b" p of
      (_ : k : _) -> k
      _ -> T.take 20 p

-- ------------------------------------------------------------ state

data ApprovalState = ApprovalState
  { asSession :: IORef (M.Map Text (S.Set Text))
  , asPermanent :: IORef (S.Set Text)
  , asPending :: IORef [(Text, PyVal)]
  }

newApprovalState :: IO ApprovalState
newApprovalState = ApprovalState <$> newIORef M.empty <*> newIORef S.empty <*> newIORef []

approvedSession :: ApprovalState -> IO (M.Map Text (S.Set Text))
approvedSession = readIORef . asSession

approvedPermanent :: ApprovalState -> IO (S.Set Text)
approvedPermanent = readIORef . asPermanent

pendingApprovals :: ApprovalState -> IO [(Text, PyVal)]
pendingApprovals = readIORef . asPending

isApproved :: ApprovalState -> Text -> Text -> IO Bool
isApproved st sessionKey key = do
  perm <- readIORef (asPermanent st)
  sess <- M.findWithDefault S.empty sessionKey <$> readIORef (asSession st)
  let as = aliases key
  pure (any (`S.member` perm) (S.toList as) || any (`S.member` sess) (S.toList as))

approveSession :: ApprovalState -> Text -> Text -> IO ()
approveSession st sk key = modifyIORef (asSession st) (M.insertWith S.union sk (S.singleton key))

-- ------------------------------------------------------------ the guard

data Choice = Once | Session | Always | Deny
  deriving (Eq, Show)

data ApprovalEnv = ApprovalEnv
  { aeGetEnv :: Text -> IO (Maybe Text)
  , aeMode :: IO Text
  -- ^ @approvals.mode@ (manual, smart or off)
  , aeTirith :: Text -> IO (Text, [PyVal], Text)
  -- ^ action, findings, summary
  , aeAux :: AuxLLM
  , aePrompt :: Text -> Text -> Bool -> IO Choice
  -- ^ command, description, whether "always" is offered
  , aeSavePermanent :: S.Set Text -> IO ()
  , aeState :: ApprovalState
  }

envSet :: ApprovalEnv -> Text -> IO Bool
envSet env k = maybe False (not . T.null) <$> aeGetEnv env k

-- | @check_all_command_guards(command, env_type)@
checkAllCommandGuards :: ApprovalEnv -> Text -> Text -> IO PyVal
checkAllCommandGuards env command envType
  | envType `elem` ["docker", "singularity", "modal", "daytona"] = pure approved
  | otherwise = do
      mode <- aeMode env
      yolo <- envSet env "FISHER_YOLO_MODE"
      if yolo || mode == "off"
        then pure approved
        else do
          cli <- envSet env "FISHER_INTERACTIVE"
          gateway <- envSet env "FISHER_GATEWAY_SESSION"
          ask <- envSet env "FISHER_EXEC_ASK"
          if not cli && not gateway && not ask
            then pure approved
            else do
              (action, findings, summary) <- aeTirith env command
              let dangerous = detectDangerousCommand command
              if action == "block"
                then pure (PDict [("approved", PBool False), ("message", PStr ("BLOCKED: Command blocked by security scan (" <> (if T.null summary then "security issue detected" else summary) <> "). Do NOT retry."))])
                else do
                  sk <- maybe "default" id <$> aeGetEnv env "FISHER_SESSION_KEY"
                  tw <-
                    if action == "warn"
                      then do
                        let rule = case findings of
                              (f : _) -> maybe "unknown" pyStrOf (get "rule_id" f)
                              [] -> "unknown"
                            key = "tirith:" <> rule
                            desc = "Security scan: " <> (if T.null summary then "security warning detected" else summary)
                        ok <- isApproved (aeState env) sk key
                        pure [(key, desc, True) | not ok]
                      else pure []
                  dw <- case dangerous of
                    Just d -> do
                      ok <- isApproved (aeState env) sk d
                      pure [(d, d, False) | not ok]
                    Nothing -> pure []
                  let warnings = tw <> dw
                  if null warnings then pure approved else decide sk warnings mode gateway ask
  where
    approved = PDict [("approved", PBool True), ("message", PNull)]
    decide sk warnings mode gateway ask = do
      let combined = T.intercalate "; " [d | (_, d, _) <- warnings]
          wkeys = [k | (k, _, _) <- warnings]
          primary = head wkeys
          hasTirith = or [t | (_, _, t) <- warnings]
      verdict <- if mode == "smart" then smartApprove env command combined else pure "escalate"
      case verdict of
        "approve" -> do
          forM_ wkeys (approveSession (aeState env) sk)
          pure (PDict [("approved", PBool True), ("message", PNull), ("smart_approved", PBool True)])
        "deny" ->
          pure (PDict [("approved", PBool False), ("message", PStr ("BLOCKED by smart approval: " <> combined <> ". The command was assessed as genuinely dangerous. Do NOT retry.")), ("smart_denied", PBool True)])
        _
          | gateway || ask -> do
              modifyIORef (asPending (aeState env)) (\ps -> [(k, v) | (k, v) <- ps, k /= sk] <> [(sk, PDict [("command", PStr command), ("pattern_key", PStr primary), ("pattern_keys", PList (map PStr wkeys)), ("description", PStr combined)])])
              pure $
                PDict
                  [ ("approved", PBool False), ("pattern_key", PStr primary), ("status", PStr "approval_required"), ("command", PStr command)
                  , ("description", PStr combined), ("message", PStr ("\9888\65039 " <> combined <> ". Asking the user for approval...")) ]
          | otherwise -> do
              choice <- aePrompt env command combined (not hasTirith)
              if choice == Deny
                then pure (PDict [("approved", PBool False), ("message", PStr "BLOCKED: User denied. Do NOT retry."), ("pattern_key", PStr primary), ("description", PStr combined)])
                else do
                  forM_ warnings $ \(k, _, isT) ->
                    if choice == Session || (choice == Always && isT)
                      then approveSession (aeState env) sk k
                      else
                        if choice == Always
                          then do
                            approveSession (aeState env) sk k
                            modifyIORef (asPermanent (aeState env)) (S.insert k)
                            readIORef (asPermanent (aeState env)) >>= aeSavePermanent env
                          else pure ()
                  pure approved

-- | @_smart_approve@: approve, deny or escalate.
smartApprove :: ApprovalEnv -> Text -> Text -> IO Text
smartApprove env command description = do
  let prompt =
        "You are a security reviewer for an AI coding agent. A terminal command was flagged by pattern matching as potentially dangerous.\n\nCommand: "
          <> command <> "\nFlagged reason: " <> description
          <> "\n\nAssess the ACTUAL risk of this command. Many flagged commands are false positives \8212 for example, `python -c \"print('hello')\"` is flagged as \"script execution via -c flag\" but is completely harmless.\n\nRules:\n- APPROVE if the command is clearly safe (benign script execution, safe file operations, development tools, package installs, git operations, etc.)\n- DENY if the command could genuinely damage the system (recursive delete of important paths, overwriting system files, fork bombs, wiping disks, dropping databases, etc.)\n- ESCALATE if you're uncertain\n\nRespond with exactly one word: APPROVE, DENY, or ESCALATE"
  r <- aeAux env (auxCall "approval" [PDict [("role", PStr "user"), ("content", PStr prompt)]]) {axMaxTokens = Just 16, axTemperature = Just 0}
  pure $ case r of
    Right resp ->
      let answer = T.toUpper (T.strip (case responseContent resp of PStr s -> s; _ -> ""))
       in if "APPROVE" `T.isInfixOf` answer then "approve" else if "DENY" `T.isInfixOf` answer then "deny" else "escalate"
    Left _ -> "escalate"

-- | @prompt_dangerous_approval@ without a UI callback: the text prompt.
-- @readChoice@ reads one answer (Nothing at end of input or after the 60
-- second wait, with which side said so).
textPrompt :: (Text -> IO ()) -> (Text -> IO (Either Bool Text)) -> Text -> Text -> Bool -> IO Choice
textPrompt say readChoice command description allowPermanent = go (T.length command > 80)
  where
    go truncated = do
      say ""
      say ("  \9888\65039  DANGEROUS COMMAND: " <> description)
      say ("      " <> T.take 80 command <> (if truncated then "..." else ""))
      say ""
      let hint = if truncated then "  |  [v]iew full" else ""
      say (if allowPermanent then "      [o]nce  |  [s]ession  |  [a]lways  |  [d]eny" <> hint else "      [o]nce  |  [s]ession  |  [d]eny" <> hint)
      say ""
      r <- readChoice (if allowPermanent then "      Choice [o/s/a/D]: " else "      Choice [o/s/D]: ")
      case r of
        Left True -> say "\n      \9201 Timeout - denying command" >> done Deny
        Left False -> answer truncated ""
        Right c -> answer truncated (T.toLower (T.strip c))
    answer truncated c
      | c `elem` ["v", "view"] && truncated = do
          say ""
          say "      Full command:"
          say ("      " <> command)
          go False
      | c `elem` ["o", "once"] = say "      \10003 Allowed once" >> done Once
      | c `elem` ["s", "session"] = say "      \10003 Allowed for this session" >> done Session
      | c `elem` ["a", "always"] =
          if allowPermanent then say "      \10003 Added to permanent allowlist" >> done Always else say "      \10003 Allowed for this session" >> done Session
      | otherwise = say "      \10007 Denied" >> done Deny
    done c = say "" >> pure c

-- ------------------------------------------------------------ tirith

data TirithEnv = TirithEnv
  { tiGetEnv :: Text -> IO (Maybe Text)
  , tiSecurity :: IO PyVal
  -- ^ the @security@ section of config.yaml
  , tiWhich :: Text -> IO (Maybe Text)
  , tiIsExecutable :: FilePath -> IO Bool
  , tiExpandUser :: Text -> IO Text
  , tiFisherHome :: FilePath
  , tiRun :: [Text] -> Int -> IO RunOutcome
  }

-- | @check_command_security(command)@: action, findings, summary.
tirithCheck :: TirithEnv -> Text -> IO (Text, [PyVal], Text)
tirithCheck env command = do
  sec <- tiSecurity env
  let cfg k = get k sec
  enabled <- envBool "TIRITH_ENABLED" (maybe True truthy (cfg "tirith_enabled"))
  pathCfg <- maybe (maybe "tirith" pyStrOf (cfg "tirith_path")) id <$> tiGetEnv env "TIRITH_BIN"
  timeout <- envInt "TIRITH_TIMEOUT" (case cfg "tirith_timeout" of Just (PInt n) -> fromIntegral n; _ -> 5)
  failOpen <- envBool "TIRITH_FAIL_OPEN" (maybe True truthy (cfg "tirith_fail_open"))
  if not enabled
    then pure ("allow", [], "")
    else do
      path <- resolvePath pathCfg
      out <- tiRun env [path, "check", "--json", "--non-interactive", "--shell", "posix", "--", command] timeout
      pure $ case out of
        RunFailed e ->
          if failOpen then ("allow", [], "tirith unavailable: " <> e) else ("block", [], "tirith spawn failed (fail-closed): " <> e)
        TimedOut ->
          if failOpen then ("allow", [], "tirith timed out (" <> tshow timeout <> "s)") else ("block", [], "tirith timed out (fail-closed)")
        Ran rc stdout _ ->
          case rc of
            _ | rc `notElem` [0, 1, 2] ->
                if failOpen then ("allow", [], "tirith exit code " <> tshow rc <> " (fail-open)") else ("block", [], "tirith exit code " <> tshow rc <> " (fail-closed)")
            _ ->
              let action = case rc of 0 -> "allow"; 1 -> "block"; _ -> "warn"
                  parsed
                    | T.null (T.strip stdout) = Right (PDict [])
                    | otherwise = case loads (TE.encodeUtf8 stdout) of
                        Right d@(PDict _) -> Right d
                        _ -> Left ()
               in case parsed of
                    Right d ->
                      ( action
                      , take 50 (case get "findings" d of Just (PList fs) -> fs; _ -> [])
                      , T.take 500 (case get "summary" d of Just (PStr s) -> s; _ -> "") )
                    Left () ->
                      (action, [], if action == "block" then "security issue detected (details unavailable)" else if action == "warn" then "security warning detected (details unavailable)" else "")
  where
    tshow = T.pack . show
    envBool k d = maybe d (\v -> T.toLower v `elem` ["1", "true", "yes"]) <$> tiGetEnv env k
    envInt k d = maybe d (\v -> fromMaybe d (readMaybe (T.unpack (T.strip v)))) <$> tiGetEnv env k
    -- the configured path when it was given; else PATH, then the Fisher home's bin
    resolvePath p
      | p /= "tirith" = do
          expanded <- tiExpandUser env p
          ok <- tiIsExecutable env (T.unpack expanded)
          if ok
            then pure expanded
            else fromMaybe expanded <$> tiWhich env expanded
      | otherwise = do
          found <- tiWhich env "tirith"
          case found of
            Just f -> pure f
            Nothing -> do
              let local = tiFisherHome env <> "/bin/tirith"
              ok <- tiIsExecutable env local
              pure (if ok then T.pack local else p)

-- ------------------------------------------------------------ config

-- | @load_config()["approvals"]["mode"]@ (manual when absent; an
-- unquoted YAML @off@ is a boolean, which is not "off", so manual too).
configApprovalMode :: FilePath -> IO Text
configApprovalMode gh = do
  r <- try @SomeException (loadConfigAt gh)
  pure $ case r of
    Right (cfg, _) | Just (A.String m) <- Cfg.getPath ["approvals", "mode"] cfg -> m
    _ -> "manual"

-- | @load_config().get("security", {})@
configSecurity :: FilePath -> IO PyVal
configSecurity gh = do
  r <- try @SomeException (loadConfigAt gh)
  pure $ case r of
    Right (cfg, _) | Just v@(A.Object _) <- Cfg.getPath ["security"] cfg -> fromAeson v
    _ -> PDict []

-- | @save_permanent_allowlist(patterns)@: the whole configuration, with
-- @command_allowlist@ replaced (sorted; upstream writes a set's order).
saveAllowlist :: FilePath -> S.Set Text -> IO ()
saveAllowlist gh patterns = do
  _ <- try @SomeException $ do
    (cfg, _) <- loadConfigAt gh
    let list' = A.Array (V.fromList (map A.String (S.toAscList patterns)))
        cfg' = case cfg of
          A.Object o -> A.Object (KM.insert "command_allowlist" list' o)
          other -> other
    saveConfigAt gh cfg'
  pure ()
