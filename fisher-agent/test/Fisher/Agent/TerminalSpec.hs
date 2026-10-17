-- | 'Fisher.Agent.Tools.Terminal' and 'Fisher.Agent.Tools.Approval'
-- against @tools/terminal_tool.py@ and @tools/approval.py@
-- (@test/golden/b4/terminal.json@): results, what reached the sandbox,
-- the approval prompt and its answers, tirith, smart approval, sudo,
-- sleeps, and the saved allowlist.
module Fisher.Agent.TerminalSpec (spec) where

import Control.Exception (Exception (..), throwIO)
import Control.Monad (forM, forM_)
import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Agent.TestUtil
import Fisher.Agent.Tools.Approval
import Fisher.Agent.Tools.Browser (RunOutcome (..))
import Fisher.Agent.Tools.Process
import Fisher.Agent.Tools.Terminal
import Fisher.Agent.Types
import Fisher.Py.Value
import Fisher.Tools.Registry
import Fisher.Tools.Sandbox (ExecRequest (..), ExecResult (..), Sandbox (..))
import Fisher.Tools.Sandbox.Common (transformSudo)

newtype Raised = Raised String deriving (Show)

instance Exception Raised where
  displayException (Raised s) = s

spec :: Spec
spec = do
  doc <- runIO (loadDoc "test/golden/b4/terminal.json")
  describe "approval.detect_dangerous_command" $
    forM_ (list "detect" doc) $ \case
      PList [PStr cmd, PList [ok, key, _]] -> it (T.unpack (T.take 40 cmd)) $
        detectDangerousCommand cmd `shouldBe` (if ok == PBool True then Just (pyStrOf key) else Nothing)
      _ -> pure ()
  describe "registry entries" $
    it "terminal and process match upstream's" $ do
      st <- newApprovalState
      procs <- newProcessRegistry (ProcessEnv (pure 0) (pure 0) (\_ -> pure ()) (pure "0") (\_ -> pure Nothing) (pure []) (pure "/bin/bash") (pure "/") (pure False) "/nonexistent/processes.json" False)
      let approval = ApprovalEnv (\_ -> pure Nothing) (pure "manual") (\_ -> pure ("allow", [], "")) (\_ -> pure (Left (AuxNoProvider ""))) (\_ _ _ -> pure Deny) (\_ -> pure ()) st
          tenv = TerminalEnv (\_ -> pure Nothing) (pure "/") (\_ -> pure (Left "none")) approval (\_ -> pure ()) procs (pure ()) (pure True)
      forM_ [terminalToolDef tenv, processToolDef procs] $ \td -> do
        let e = field (tdName td) (field "entries" doc)
        PDict [("toolset", PStr (tdToolset td)), ("schema", tdSchema td), ("requires_env", PList (map PStr (tdRequiresEnv td))), ("emoji", PStr (tdEmoji td))]
          `shouldBe` PDict [(k, field k e) | k <- ["toolset", "schema", "requires_env", "emoji"]]
  describe "tools/terminal_tool.py" $
    forM_ (list "cases" doc) $ \c -> it (T.unpack (txt "name" c)) $
      withSystemTempDirectory "fisher-terminal" $ \root -> do
        let gh = root </> "gauss"
            opts = field "opts" c
            optsList k = list k opts
        createDirectoryIfMissing True gh
        case get "config" opts of
          Just (PStr y) -> BS.writeFile (gh </> "config.yaml") (TE.encodeUtf8 y)
          _ -> pure ()
        let envT =
              [(k, v) | (k, PStr v) <- case field "env" opts of PDict kvs -> kvs; _ -> []]
                <> [("TERMINAL_CWD", "/work/project")]
                <> [("GAUSS_INTERACTIVE", "1") | get "interactive" opts == Just (PBool True)]
            getEnv k = pure (lookup k envT)
        execQ <- newIORef (list "exec_answers" c)
        inputs <- newIORef (optsList "inputs")
        tirithQ <- newIORef (optsList "tirith")
        smartQ <- newIORef (optsList "smart")
        sudoQ <- newIORef (optsList "sudo_cb")
        execs <- newIORef []
        creates <- newIORef []
        prompts <- newIORef []
        stdout <- newIORef ""
        tiriths <- newIORef []
        smarts <- newIORef []
        sleeps <- newIORef []
        sandboxes <- newIORef M.empty
        let next q = atomicModifyIORef' q (\case [] -> ([], Nothing); (x : xs) -> (xs, Just x))
            say t = modifyIORef stdout (<> t <> "\n")
            raw t = modifyIORef stdout (<> t)
        sudo <-
          newSudoPrompt getEnv $
            if has "sudo_cb" opts
              then maybe "" pyStrOf <$> next sudoQ
              else ttySudoPrompt raw (pure (Just ""))
        let sandboxFor task = do
              known <- M.lookup task <$> readIORef sandboxes
              case known of
                Just sb -> pure (Right sb)
                Nothing -> do
                  modifyIORef creates (<> [task])
                  case get "create_raises" opts of
                    Just (PStr e) -> pure (Left e)
                    _ -> do
                      let sb =
                            Sandbox
                              { sbKind = "local"
                              , sbExecute = \req -> do
                                  pw <- sudoPasswordFor sudo (erCommand req)
                                  let (cmd, stdin) = transformSudo pw (erCommand req)
                                  modifyIORef execs (<> [PDict [("task", PStr task), ("command", PStr cmd), ("stdin", maybe PNull PStr stdin), ("cwd", if T.null (erCwd req) then PNull else PStr (erCwd req)), ("timeout", maybe PNull (PInt . fromIntegral) (erTimeout req))]])
                                  a <- next execQ
                                  case a of
                                    Nothing -> pure (ExecResult "" 0)
                                    Just ans
                                      | Just (PStr m) <- get "raise" ans -> throwIO (Raised (T.unpack m))
                                      | otherwise -> pure (ExecResult (txt "output" ans) (fromIntegral (int "returncode" ans)))
                              , sbCwd = pure "/work/project", sbDefaultTimeout = 180, sbCleanup = pure () }
                      modifyIORef sandboxes (M.insert task sb)
                      pure (Right sb)
            tirithEnv =
              TirithEnv
                { tiGetEnv = getEnv, tiSecurity = configSecurity gh
                , tiWhich = \n -> pure (case get n (field "which" opts) of Just (PStr p) -> Just p; _ -> Nothing)
                , tiIsExecutable = \_ -> pure False, tiExpandUser = pure, tiGaussHome = gh
                , tiRun = \argv t -> do
                    modifyIORef tiriths (<> [PDict [("argv", PList (map PStr argv)), ("timeout", PInt (fromIntegral t))]])
                    a <- next tirithQ
                    pure $ case a of
                      Nothing -> Ran 0 "" ""
                      Just ans
                        | get "timeout" ans == Just (PBool True) -> TimedOut
                        | get "oserror" ans == Just (PBool True) -> RunFailed ("[Errno 2] No such file or directory: '" <> head argv <> "'")
                        | otherwise -> Ran (fromIntegral (int "rc" ans)) (txt "stdout" ans) "" }
            aux call = do
              modifyIORef smarts (<> [call])
              if get "no_client" opts == Just (PBool True)
                then pure (Left (AuxNoProvider "no client"))
                else do
                  a <- next smartQ
                  pure $ case a of
                    Nothing -> Left (AuxFailed "no answer")
                    Just ans -> case get "raise" ans of
                      Just (PStr m) -> Left (AuxFailed m)
                      _ -> Right (PDict [("choices", PList [PDict [("message", PDict [("content", field "content" ans)])]])])
        st <- newApprovalState
        let approval =
              ApprovalEnv
                { aeGetEnv = getEnv, aeMode = configApprovalMode gh, aeTirith = tirithCheck tirithEnv, aeAux = aux
                , aePrompt = textPrompt say (\p -> do modifyIORef prompts (<> [p]); maybe (Left False) (Right . pyStrOf) <$> next inputs)
                , aeSavePermanent = saveAllowlist gh, aeState = st }
        procs <-
          newProcessRegistry
            (ProcessEnv (pure 0) (pure 0) (\_ -> pure ()) (pure "0") getEnv (pure []) (pure "/bin/bash") (pure "/") (pure False) (gh </> "processes.json") False)
        let tenv =
              TerminalEnv
                { teGetEnv = getEnv, teCwd = pure "/work/project", teSandbox = sandboxFor, teApproval = approval
                , teSleep = \s -> modifyIORef sleeps (<> [s]), teProcesses = procs, teFirstUse = pure (), teCheck = pure True }
        reg <- newRegistry
        register reg (terminalToolDef tenv)
        results <- forM (list "calls" c) $ \call ->
          dispatch reg defaultCtx {tcTaskId = case get "task_id" call of Just (PStr t) -> t; _ -> ""} (txt "tool" call) (field "args" call)
        map PStr results `shouldBe` list "results" c
        es <- readIORef execs
        PList es `shouldBe` PList [PDict [(k, field k e) | k <- ["task", "command", "stdin", "cwd", "timeout"]] | e <- list "execs" c]
        cs <- readIORef creates
        map PStr cs `shouldBe` [field "task_id" x | x <- list "creates" c]
        ps <- readIORef prompts
        map PStr ps `shouldBe` list "prompts" c
        out <- readIORef stdout
        out `shouldBe` txt "stdout" c
        ts <- readIORef tiriths
        PList ts `shouldBe` PList [PDict [(k, field k t) | k <- ["argv", "timeout"]] | t <- list "tirith" c]
        sm <- readIORef smarts
        [PDict [("messages", PList (axMessages a)), ("max_tokens", maybe PNull PInt (axMaxTokens a)), ("temperature", maybe PNull (PInt . round) (axTemperature a))] | a <- sm, get "no_client" opts /= Just (PBool True)]
          `shouldBe` [PDict [(k, field k x) | k <- ["messages", "max_tokens", "temperature"]] | x <- list "smart" c]
        sl <- readIORef sleeps
        sl `shouldBe` [case v of PInt n -> fromIntegral n; PFloat d -> d; _ -> 0 | v <- list "sleeps" c]
        sess <- approvedSession st
        PDict [(k, PList (map PStr (S.toAscList v))) | (k, v) <- M.toAscList sess] `shouldBe` field "session_approved" c
        perm <- approvedPermanent st
        PList (map PStr (S.toAscList perm)) `shouldBe` field "permanent" c
        pend <- pendingApprovals st
        PDict pend `shouldBe` field "pending" c
        exists <- doesFileExist (gh </> "config.yaml")
        cfgAfter <- if exists then Just . TE.decodeUtf8 <$> BS.readFile (gh </> "config.yaml") else pure Nothing
        maybe PNull PStr cfgAfter `shouldBe` field "config_after" c
