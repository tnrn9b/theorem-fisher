{-# LANGUAGE TemplateHaskell #-}

-- | The REPL a language model's code blocks run in: the Python script in
-- @data/rlm_repl.py@ (see "Fisher.RLM" spec section 3.1 for the protocol),
-- started as a child process and driven over JSON lines on its stdin and
-- stdout. Model calls the code makes come back as @call@ messages and are
-- answered by a 'Callback'. The timeout bounds Python's own time: each wait
-- for the script's next message gets the full allowance, and time spent
-- serving a callback does not count. A block that runs past it, or a script
-- that dies, is killed and restarted with the same context; the block's
-- result then says so.
module Fisher.RLM.Repl
  ( CallKind (..)
  , Callback
  , ReplHandle (..)
  , ReplFactory
  , pythonRepl
  , checkInterpreter
  , pythonNotFound
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (waitCatch, withAsync)
import Control.Concurrent.MVar
import Control.Exception (IOException, SomeException, displayException, mask_, onException, throwIO, try)
import Control.Monad (void, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (toUpper)
import Data.FileEmbed (embedFile)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Fisher.Py.Value (PyVal (..), asList, asText, dumpsAscii, get, loads)
import Fisher.RLM.Parse (BlockResult (..))
import System.Directory (getTemporaryDirectory, removePathForcibly)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (Handle, IOMode (..), hClose, hFlush, hSetBinaryMode, openFile)
import System.IO.Error (ioeGetErrorString, isUserError)
import System.IO.Temp (createTempDirectory)
import System.Posix.Signals (sigKILL, signalProcess)
import System.Process
import System.Timeout (timeout)

-- | Which kind of model call the code made: a plain completion or a
-- recursive run.
data CallKind = CallLlm | CallRlm
  deriving (Eq, Show)

-- | Answer the prompts of one call (with the model the code named, if any):
-- one result per prompt, in order.
type Callback = CallKind -> [Text] -> Maybe Text -> IO [Text]

data ReplHandle = ReplHandle
  { rhExec :: Text -> Callback -> IO BlockResult
  -- ^ run a code block; one at a time (concurrent calls queue)
  , rhClose :: IO ()
  -- ^ stop the process and delete its directory; safe to repeat and safe to
  -- call while a block runs on another thread (that block ends)
  }

-- | Load the context into a new REPL. 'Left' when Python is missing or
-- older than 3.9.
type ReplFactory = PyVal -> IO (Either Text ReplHandle)

scriptBytes :: BS.ByteString
scriptBytes = $(embedFile "data/rlm_repl.py")

-- | A REPL on the given Python executable; the block timeout, in seconds,
-- bounds each stretch of Python time between callbacks.
pythonRepl :: FilePath -> Int -> ReplFactory
pythonRepl py secs ctx = do
  checked <- checkInterpreter py
  case checked of
    Left e -> pure (Left e)
    Right () -> do
      sysTmp <- getTemporaryDirectory
      dir <- createTempDirectory sysTmp "fisher-rlm-"
      BS.writeFile (dir </> "rlm_repl.py") scriptBytes
      started <- startProc py secs dir ctx
      case started of
        Left e -> removePathForcibly dir >> pure (Left e)
        Right p -> do
          st <- newMVar (State (Just p) False)
          lock <- newMVar ()
          let r = Repl py secs dir ctx st lock
          pure (Right (ReplHandle (execBlock r) (closeRepl r)))

-- ---------------------------------------------------------------- interpreter

-- | The refusal when there is no interpreter at the path.
pythonNotFound :: FilePath -> Text
pythonNotFound py = "python3 not found: " <> T.pack py

-- | 'Left' with the reason when the interpreter is missing or older than
-- 3.9.
checkInterpreter :: FilePath -> IO (Either Text ())
checkInterpreter py = do
  env <- filteredEnv
  let run args = readCreateProcessWithExitCode (proc py ("-I" : args)) {env = Just env} ""
  r <- timeout 10000000 (try (run ["-c", "import sys; print(sys.version_info[:2] >= (3, 9))"]))
  case r of
    Nothing -> pure (Left ("python3 version check timed out: " <> T.pack py))
    Just (Left (_ :: IOException)) -> pure (Left (pythonNotFound py))
    Just (Right (code, out, err)) -> case (code, T.strip (T.pack out)) of
      (ExitSuccess, "True") -> pure (Right ())
      (ExitSuccess, "False") -> do
        v <- timeout 10000000 (try (run ["--version"])) :: IO (Maybe (Either IOException (ExitCode, String, String)))
        let ver = case v of
              Just (Right (_, o, e)) -> T.strip (T.pack (o <> e))
              _ -> "unknown version"
        pure (Left ("python3 is older than 3.9: " <> ver))
      _ -> pure (Left ("python3 version check failed: " <> T.strip (T.pack err)))

-- ------------------------------------------------------------------- process

data Proc = Proc
  { pHandle :: ProcessHandle
  , pIn :: Handle
  , pOut :: Handle
  }

data State = State
  { sProc :: Maybe Proc
  -- ^ 'Nothing' after a restart that failed
  , sClosed :: Bool
  }

data Repl = Repl
  { rPy :: FilePath
  , rSecs :: Int
  , rDir :: FilePath
  , rCtx :: PyVal
  , rState :: MVar State
  , rExecLock :: MVar ()
  }

-- | Variables whose names would carry a credential never reach the script.
secretName :: String -> Bool
secretName n =
  any (`isSuffixOf` u) ["_API_KEY", "_TOKEN"]
    || any (`isPrefixOf` u) ["OPENROUTER_", "ANTHROPIC_", "OPENAI_"]
  where
    u = map toUpper n

filteredEnv :: IO [(String, String)]
filteredEnv = filter (not . secretName . fst) <$> getEnvironment

childEnv :: FilePath -> IO [(String, String)]
childEnv dir = do
  env <- filteredEnv
  let ours = ["PYTHONUTF8", "PYTHONIOENCODING", "TMPDIR"]
      kept = [kv | kv@(k, _) <- env, k `notElem` ours]
  pure (kept ++ [("PYTHONUTF8", "1"), ("PYTHONIOENCODING", "utf-8"), ("TMPDIR", dir)])

-- | Start the script in the directory and load the context.
startProc :: FilePath -> Int -> FilePath -> PyVal -> IO (Either Text Proc)
startProc py secs dir ctx = do
  env <- childEnv dir
  logH <- openFile (dir </> "repl.stderr.log") AppendMode
  r <-
    try $
      createProcess
        (proc py ["-I", "-X", "utf8", "rlm_repl.py"])
          { cwd = Just dir
          , env = Just env
          , std_in = CreatePipe
          , std_out = CreatePipe
          , std_err = UseHandle logH
          }
  case r of
    Left (e :: IOException) -> ignoring (hClose logH) >> pure (Left ("could not start " <> T.pack py <> ": " <> T.pack (show e)))
    Right (Just hin, Just hout, _, ph) -> do
      hSetBinaryMode hin True
      hSetBinaryMode hout True
      let p = Proc ph hin hout
      loaded <-
        timeout (secs * 1000000) . try $ do
          sendMsg p (PDict [("op", PStr "load"), ("context", ctx)])
          readMsg p
      case loaded of
        Just (Right m) | get "op" m == Just (PStr "ok") -> pure (Right p)
        Just (Right m) -> killProc p >> pure (Left ("REPL load failed: " <> dumpsAscii m))
        Just (Left (e :: IOException)) -> killProc p >> pure (Left ("REPL load failed: " <> T.pack (show e)))
        Nothing -> killProc p >> pure (Left "REPL load timed out")
    Right _ -> pure (Left "could not open pipes to the REPL")

sendMsg :: Proc -> PyVal -> IO ()
sendMsg p v = do
  BS.hPut (pIn p) (TE.encodeUtf8 (dumpsAscii v) <> "\n")
  hFlush (pIn p)

-- | The next message. End of input is an 'IOException' (EOF); a line that is
-- not JSON is a user error.
readMsg :: Proc -> IO PyVal
readMsg p = do
  line <- BC.hGetLine (pOut p)
  case loads line of
    Right v -> pure v
    Left e -> throwIO (userError ("bad line from the REPL: " <> e))

-- | Wait up to the given milliseconds for the process to exit.
waitUpTo :: Int -> ProcessHandle -> IO (Maybe ExitCode)
waitUpTo ms ph = go (ms `div` 20)
  where
    go n = do
      c <- getProcessExitCode ph
      case c of
        Just _ -> pure c
        Nothing
          | n <= 0 -> pure Nothing
          | otherwise -> threadDelay 20000 >> go (n - 1 :: Int)

-- | Terminate, reap, and close the pipes. Safe to repeat.
killProc :: Proc -> IO ()
killProc p = do
  _ <- try (terminateProcess (pHandle p)) :: IO (Either IOException ())
  exited <- waitUpTo 2000 (pHandle p)
  when (exited == Nothing) $ do
    -- ignored SIGTERM: stop it for good
    pid <- getPid (pHandle p)
    ignoring (maybe (pure ()) (signalProcess sigKILL) pid)
  void (try (waitForProcess (pHandle p)) :: IO (Either IOException ExitCode))
  ignoring (hClose (pIn p))
  ignoring (hClose (pOut p))

ignoring :: IO () -> IO ()
ignoring act = void (try act :: IO (Either IOException ()))

-- ---------------------------------------------------------------------- exec

-- | Send the block and serve its calls until the result arrives. 'Nothing'
-- when the script takes longer than the given seconds to send its next
-- message; the wait restarts after each callback is served.
runBlock :: Int -> Proc -> Text -> Callback -> IO (Maybe BlockResult)
runBlock secs p code cb = do
  sendMsg p (PDict [("op", PStr "exec"), ("code", PStr code)])
  loop
  where
    loop = timeout (secs * 1000000) (readMsg p) >>= maybe (pure Nothing) serve
    serve m =
      case get "op" m >>= asText of
        Just "call" -> do
          kind <- case get "kind" m >>= asText of
            Just "llm" -> pure CallLlm
            Just "rlm" -> pure CallRlm
            _ -> throwIO (userError ("unknown call kind in " <> T.unpack (dumpsAscii m)))
          let prompts = maybe [] (concatMap (maybe [] pure . asText)) (get "prompts" m >>= asList)
              model = get "model" m >>= asText
          res <- withAsync (cb kind prompts model) waitCatch
          let results = case res of
                Right rs
                  | length rs == length prompts -> rs
                  | otherwise ->
                      replicate (length prompts) ("Error: callback returned " <> T.pack (show (length rs)) <> " results for " <> T.pack (show (length prompts)) <> " prompts")
                Left e -> replicate (length prompts) (failed e)
          sendMsg p (PDict [("op", PStr "results"), ("results", PList (map PStr results))])
          loop
        Just "result" -> pure (Just (toResult m))
        _ -> throwIO (userError ("unexpected message from the REPL: " <> T.unpack (dumpsAscii m)))
    failed :: SomeException -> Text
    failed e = "Error: LM query failed - " <> T.pack (displayException e)

toResult :: PyVal -> BlockResult
toResult m =
  BlockResult
    { brStdout = text "stdout"
    , brStderr = text "stderr"
    , brVars = texts "vars"
    , brTypes = texts "types"
    , brFinal = get "final" m >>= asText
    }
  where
    text k = fromMaybe "" (get k m >>= asText)
    texts k = maybe [] (concatMap (maybe [] pure . asText)) (get k m >>= asList)

lostVariables :: Text -> Text
lostVariables why = why <> "; the REPL was restarted and its variables were lost"

blockMessage :: Text -> BlockResult
blockMessage msg = BlockResult "" msg [] [] Nothing

execBlock :: Repl -> Text -> Callback -> IO BlockResult
execBlock r code cb = withMVar (rExecLock r) $ \_ -> do
  cur <- modifyMVar (rState r) $ \s ->
    if sClosed s
      then pure (s, Nothing)
      else case sProc s of
        Just p -> pure (s, Just (Right p))
        Nothing -> do
          started <- startProc (rPy r) (rSecs r) (rDir r) (rCtx r)
          pure (s {sProc = either (const Nothing) Just started}, Just started)
  case cur of
    Nothing -> pure closedResult
    Just (Left e) -> pure (blockMessage ("REPL could not be restarted: " <> e))
    Just (Right p) -> do
      out <-
        try (runBlock (rSecs r) p code cb)
          `onException` abandon p
      case out of
        Right (Just br) -> pure br
        Right Nothing -> recover p ("REPL timed out after " <> T.pack (show (rSecs r)) <> " s")
        Left (e :: IOException)
          | isUserError e -> recover p ("REPL protocol error: " <> T.pack (ioeGetErrorString e))
          | otherwise -> do
              code' <- waitUpTo 2000 (pHandle p)
              recover p ("REPL exited with code " <> maybe "?" exitText code')
  where
    exitText ExitSuccess = "0"
    exitText (ExitFailure n) = T.pack (show n)
    -- An outside interruption leaves the script mid-block: drop it so the
    -- next exec starts a fresh one.
    abandon p = do
      modifyMVar_ (rState r) (\s -> pure s {sProc = Nothing})
      killProc p
    recover p why = do
      killProc p
      restarted <- modifyMVar (rState r) $ \s ->
        if sClosed s
          then pure (s {sProc = Nothing}, Nothing)
          else do
            started <- startProc (rPy r) (rSecs r) (rDir r) (rCtx r)
            pure (s {sProc = either (const Nothing) Just started}, Just started)
      pure $ case restarted of
        Nothing -> closedResult
        Just (Right _) -> blockMessage (lostVariables why)
        Just (Left e) -> blockMessage (why <> "; the REPL could not be restarted: " <> e)

closedResult :: BlockResult
closedResult = blockMessage "REPL closed"

-- --------------------------------------------------------------------- close

closeRepl :: Repl -> IO ()
closeRepl r = mask_ $ do
  first <- modifyMVar (rState r) $ \s ->
    pure (s {sClosed = True, sProc = Nothing}, if sClosed s then Nothing else Just (sProc s))
  case first of
    Nothing -> pure ()
    Just mp -> do
      -- Ask politely only when no block is running; otherwise the kill below
      -- ends that block.
      idle <- tryTakeMVar (rExecLock r)
      case mp of
        Nothing -> pure ()
        Just p -> do
          when (idle /= Nothing) $ do
            ignoring (sendMsg p (PDict [("op", PStr "close")]))
            void (waitUpTo 2000 (pHandle p))
          killProc p
      when (idle /= Nothing) (putMVar (rExecLock r) ())
      removePathForcibly (rDir r)
