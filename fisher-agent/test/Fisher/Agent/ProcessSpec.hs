-- | Background processes and the @process@ tool against
-- @tools/process_registry.py@ (@test/golden/b4/process.json@), on real
-- local bash processes and a scripted non-local sandbox. Pids, uptimes and
-- start times are masked on both sides.
module Fisher.Agent.ProcessSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception (..), throwIO)
import GHC.IO.Encoding (setFileSystemEncoding)
import System.IO (mkTextEncoding)
import Control.Monad (forM_, when)
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Clock (getMonotonicTime)
import Numeric (showHex)
import System.Directory
import System.Environment (getEnvironment)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Agent.TestUtil
import Fisher.Agent.Tools.Approval
import Fisher.Agent.Tools.Process
import Fisher.Agent.Tools.Terminal
import Fisher.Agent.Types
import Fisher.Py.Re (compileOrDie, subText)
import Fisher.Py.Value
import Fisher.Tools.Registry
import Fisher.Tools.Sandbox (ExecRequest (..), ExecResult (..), Sandbox (..))

newtype Raised = Raised String deriving (Show)

instance Exception Raised where
  displayException (Raised s) = s

spec :: Spec
spec = do
  runIO (setFileSystemEncoding =<< mkTextEncoding "UTF-8//ROUNDTRIP")
  cases <- runIO (loadCases "test/golden/b4/process.json")
  describe "tools/process_registry.py" $
    forM_ cases $ \c -> it (T.unpack (txt "name" c)) $
      withSystemTempDirectory "fisher-process" $ \root -> do
        let gh = root </> "gauss"
            work = root </> "work"
            opts = field "opts" c
        createDirectoryIfMissing True gh
        createDirectoryIfMissing True work
        let envT = [(k, v) | (k, PStr v) <- case field "env" opts of PDict kvs -> kvs; _ -> []] <> [("TERMINAL_CWD", T.pack work)]
            getEnv k = pure (lookup k envT)
        uuidN <- newIORef (0 :: Integer)
        execQ <- newIORef (list "execs" opts)
        execs <- newIORef []
        let uuid = do
              modifyIORef uuidN (+ 1)
              n <- readIORef uuidN
              pure (T.justifyRight 32 '0' (T.pack (showHex (n * 0x1F2E3D * 2 ^ (104 :: Int)) "")))
            next q = atomicModifyIORef' q (\case [] -> ([], Nothing); (x : xs) -> (xs, Just x))
            fake =
              Sandbox
                { sbKind = "docker"
                , sbExecute = \req -> do
                    modifyIORef execs (<> [req])
                    a <- next execQ
                    case a of
                      Just ans | Just (PStr m) <- get "raise" ans -> throwIO (Raised (T.unpack m))
                      Just ans -> pure (ExecResult (T.replace "<PID>" "12345" (txt "output" ans)) 0)
                      Nothing -> pure (ExecResult "" 0)
                , sbCwd = pure "/root", sbDefaultTimeout = 180, sbCleanup = pure () }
        environ <- map (\(k, v) -> (T.pack k, T.pack v)) <$> getEnvironment
        procs <-
          newProcessRegistry
            ProcessEnv
              { peNow = realToFrac <$> getPOSIXTime, peMonotonic = getMonotonicTime, peSleep = \s -> threadDelay (round (s * 1e6))
              , peUuidHex = uuid, peGetEnv = getEnv, peEnviron = pure environ, peBash = pure "bash", peCwd = pure work
              , peInterrupted = pure False, peCheckpoint = gh </> "processes.json", pePollRemote = False }
        st <- newApprovalState
        let approval =
              ApprovalEnv getEnv (pure "manual") (\_ -> pure ("allow", [], "")) (\_ -> pure (Left (AuxNoProvider ""))) (\_ _ _ -> pure Deny) (\_ -> pure ()) st
            tenv = TerminalEnv getEnv (pure (T.pack work)) (\_ -> pure (Right fake)) approval (\_ -> pure ()) procs (pure ()) (pure True)
        reg <- newRegistry
        register reg (terminalToolDef tenv)
        register reg (processToolDef procs)
        ids <- newIORef []
        pids <- newIORef []
        results <- newIORef []
        forM_ (list "calls" c) $ \case
          PList [PStr "sleep", PFloat s] -> threadDelay (round (s * 1e6))
          PList (PStr tool : args0 : rest) -> do
            known <- readIORef ids
            let args = case args0 of
                  PDict kvs -> PDict [(k, case v of PStr s | "$" `T.isPrefixOf` s -> PStr (known !! (read (T.unpack (T.drop 1 s)) - 1)); PStr s -> PStr (T.replace "<WORK>" (T.pack work) s); _ -> v) | (k, v) <- kvs]
                  v -> v
                task = case rest of [PStr t] -> t; _ -> ""
            r <- dispatch reg defaultCtx {tcTaskId = task} tool args
            when (tool == "terminal") $ case loads' r of
              Just d | Just (PStr sid) <- get "session_id" d -> do
                modifyIORef ids (<> [sid])
                forM_ (get "pid" d) $ \case PInt p -> modifyIORef pids (<> [p]); _ -> pure ()
              _ -> pure ()
            modifyIORef results (<> [r])
          _ -> pure ()
        threadDelay 200000
        killAll procs
        ps <- readIORef pids
        rs <- readIORef results
        let maskPid t = foldr (\p acc -> subText (compileOrDie [] ("\\b" <> T.pack (show p) <> "\\b")) "<PID>" acc) t ps
            norm t = subText (compileOrDie [] "\"started_at\": \"[0-9T:-]+\"") "\"started_at\": \"<T>\"" (subText (compileOrDie [] "\"uptime_seconds\": [0-9]+") "\"uptime_seconds\": \"<UP>\"" t)
            mask = norm . T.replace (T.pack work) "<WORK>" . maskPid
        map mask rs `shouldBe` map (norm . pyStrOf) (list "results" c)
        es <- readIORef execs
        PList [PDict [("command", PStr (mask (erCommand e))), ("cwd", if T.null (erCwd e) then PNull else PStr (erCwd e)), ("timeout", maybe PNull (PInt . fromIntegral) (erTimeout e))] | e <- es]
          `shouldBe` field "execs" c
        ws <- pendingWatchers procs
        PList ws `shouldBe` field "watchers" c
  where
    loads' t = either (const Nothing) Just (loads (TE.encodeUtf8 t))
