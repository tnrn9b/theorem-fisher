-- | @tools/process_registry.py@: background processes started by
-- @terminal(background=true)@, their rolling output, and the @process@
-- tool (list, poll, log, wait, kill, write, submit).
module Fisher.Agent.Tools.Process
  ( ProcessEnv (..)
  , ProcessRegistry
  , newProcessRegistry
  , spawnLocal
  , spawnViaSandbox
  , processTool
  , pendingWatchers
  , addWatcher
  , hasActiveProcesses
  , killAll
  , processToolDef
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM_, unless, when)
import qualified Data.ByteString as BS
import Data.Char (isDigit)
import Data.IORef
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Time (defaultTimeLocale, formatTime, utcToLocalZonedTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.Directory (createDirectoryIfMissing, renameFile)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO
import System.Posix.Signals (sigTERM, signalProcessGroup)
import System.Process

import Fisher.Agent.Tools.Web (toolSchemas)
import Fisher.Py.Value
import Fisher.Tools.Registry (ToolCtx (..), ToolDef (..))
import Fisher.Tools.Sandbox (ExecRequest (..), ExecResult (..), Sandbox (..))
import Fisher.Tools.Sandbox.Common (providerEnvBlocklist)

data ProcessEnv = ProcessEnv
  { peNow :: IO Double
  , peMonotonic :: IO Double
  , peSleep :: Double -> IO ()
  , peUuidHex :: IO Text
  , peGetEnv :: Text -> IO (Maybe Text)
  , peEnviron :: IO [(Text, Text)]
  , peBash :: IO FilePath
  , peCwd :: IO FilePath
  , peInterrupted :: IO Bool
  , peCheckpoint :: FilePath
  -- ^ @$GAUSS_HOME/processes.json@
  , pePollRemote :: Bool
  -- ^ run the 2-second poller for sandboxed processes
  }

data Session = Session
  { sId :: Text
  , sCommand :: Text
  , sTask :: Text
  , sKey :: Text
  , sPid :: Maybe Int
  , sCwd :: Maybe Text
  , sStarted :: Double
  , sExited :: IORef Bool
  , sExit :: IORef (Maybe Int)
  , sOutput :: IORef Text
  , sStdin :: Maybe Handle
  , sProc :: Maybe ProcessHandle
  , sSandbox :: Maybe Sandbox
  }

data ProcessRegistry = ProcessRegistry
  { prEnv :: ProcessEnv
  , prRunning :: MVar [Session]
  , prFinished :: MVar [Session]
  , prWatchers :: IORef [PyVal]
  }

newProcessRegistry :: ProcessEnv -> IO ProcessRegistry
newProcessRegistry env = ProcessRegistry env <$> newMVar [] <*> newMVar [] <*> newIORef []

pendingWatchers :: ProcessRegistry -> IO [PyVal]
pendingWatchers = readIORef . prWatchers

addWatcher :: ProcessRegistry -> PyVal -> IO ()
addWatcher pr w = modifyIORef (prWatchers pr) (<> [w])

maxOutput :: Int
maxOutput = 200000

newSessionRecord :: ProcessRegistry -> Text -> Text -> Text -> Maybe Text -> IO Session
newSessionRecord pr command task key cwd = do
  h <- peUuidHex (prEnv pr)
  now <- peNow (prEnv pr)
  Session ("proc_" <> T.take 12 h) command task key Nothing cwd now <$> newIORef False <*> newIORef Nothing <*> newIORef "" <*> pure Nothing <*> pure Nothing <*> pure Nothing

-- | @_prune_if_needed@ then track as running.
track :: ProcessRegistry -> Session -> IO ()
track pr s = do
  now <- peNow (prEnv pr)
  modifyMVar_ (prFinished pr) (pure . filter (\x -> now - sStarted x <= 1800))
  running <- readMVar (prRunning pr)
  finished <- readMVar (prFinished pr)
  when (length running + length finished >= 64 && not (null finished)) $ do
    let oldest = minimum (map sStarted finished)
        (pre, post) = break ((== oldest) . sStarted) finished
    modifyMVar_ (prFinished pr) (const (pure (pre <> drop 1 post)))
  modifyMVar_ (prRunning pr) (pure . (<> [s]))
  writeCheckpoint pr

moveToFinished :: ProcessRegistry -> Session -> IO ()
moveToFinished pr s = do
  modifyMVar_ (prRunning pr) (pure . filter ((/= sId s) . sId))
  modifyMVar_ (prFinished pr) (\fs -> pure (filter ((/= sId s) . sId) fs <> [s]))
  writeCheckpoint pr

-- | @_write_checkpoint@: the running processes, atomically.
writeCheckpoint :: ProcessRegistry -> IO ()
writeCheckpoint pr = do
  running <- readMVar (prRunning pr)
  entries <- fmap concat . mapM entry $ running
  let path = peCheckpoint (prEnv pr)
  _ <- try @SomeException $ do
    createDirectoryIfMissing True (takeDirectory path)
    let tmp = takeDirectory path </> ".processes_tmp"
    BS.writeFile tmp (TE.encodeUtf8 (dumpsIndent True 2 (PList entries)))
    renameFile tmp path
  pure ()
  where
    entry s = do
      ex <- readIORef (sExited s)
      pure
        [ PDict
            [ ("session_id", PStr (sId s)), ("command", PStr (sCommand s)), ("pid", maybe PNull (PInt . fromIntegral) (sPid s))
            , ("cwd", maybe PNull PStr (sCwd s)), ("started_at", PFloat (sStarted s)), ("task_id", PStr (sTask s)), ("session_key", PStr (sKey s)) ]
        | not ex ]

appendOutput :: Session -> Text -> IO ()
appendOutput s t = modifyIORef' (sOutput s) (\o -> let n = o <> t in if T.length n > maxOutput then T.takeEnd maxOutput n else n)

shellNoise :: [Text]
shellNoise =
  [ "bash: cannot set terminal process group", "bash: no job control in this shell", "no job control in this shell"
  , "cannot set terminal process group", "tcsetattr: Inappropriate ioctl for device" ]

cleanShellNoise :: Text -> Text
cleanShellNoise = T.intercalate "\n" . dropWhileList . T.splitOn "\n"
  where
    dropWhileList (l : ls) | any (`T.isInfixOf` l) shellNoise = dropWhileList ls
    dropWhileList ls = ls

-- | @spawn_local@: @bash -lic command@ in its own session, stdout and
-- stderr on one pipe, stdin open.
spawnLocal :: ProcessRegistry -> Text -> Maybe Text -> Text -> Text -> IO (Either Text (Text, Maybe Int))
spawnLocal pr command cwd0 task key = do
  let env = prEnv pr
  here <- peCwd env
  let cwd = fromMaybe (T.pack here) cwd0
  s0 <- newSessionRecord pr command task key (Just cwd)
  bash <- peBash env
  environ <- peEnviron env
  let forcePrefix = "_GAUSS_FORCE_"
      sanitized = [(k, v) | (k, v) <- environ, not (forcePrefix `T.isPrefixOf` k), not (k `S.member` providerEnvBlocklist)]
      bgEnv = [(k, v) | (k, v) <- sanitized, k /= "PYTHONUNBUFFERED"] <> [("PYTHONUNBUFFERED", "1")]
  r <- try $ do
    (readEnd, writeEnd) <- createPipe
    (Just hin, _, _, ph) <-
      createProcess
        (proc bash ["-lic", T.unpack command])
          { cwd = Just (T.unpack cwd), env = Just [(T.unpack k, T.unpack v) | (k, v) <- bgEnv]
          , std_in = CreatePipe, std_out = UseHandle writeEnd, std_err = UseHandle writeEnd, new_session = True }
    pid <- getPid ph
    pure (readEnd, hin, ph, fmap fromIntegral pid)
  case r of
    Left (e :: SomeException) -> pure (Left (T.pack (displayException e)))
    Right (readEnd, hin, ph, pid) -> do
      hSetBinaryMode readEnd True
      hSetBuffering hin NoBuffering
      hSetEncoding hin utf8
      let s = s0 {sPid = pid, sStdin = Just hin, sProc = Just ph}
      track pr s
      _ <- forkIO (reader s readEnd ph)
      pure (Right (sId s, pid))
  where
    reader s h ph = do
      raw <- newIORef BS.empty
      firstRef <- newIORef True
      let loop = do
            -- read(4096) on a text stream waits for the whole 4096 (or the end)
            chunk <- try @SomeException (BS.hGet h 4096)
            case chunk of
              Right c | not (BS.null c) -> do
                -- decode what has arrived, keeping a cut multibyte character for later
                pending <- readIORef raw
                let bytes = pending <> c
                    (ok, rest) = splitUtf8 bytes
                writeIORef raw rest
                let txt = TE.decodeUtf8With TEE.lenientDecode ok
                first <- readIORef firstRef
                writeIORef firstRef False
                appendOutput s (if first then cleanShellNoise txt else txt)
                loop
              _ -> pure ()
      loop
      leftover <- readIORef raw
      unless (BS.null leftover) (appendOutput s (TE.decodeUtf8With TEE.lenientDecode leftover))
      code <- waitForProcess ph
      writeIORef (sExit s) (Just (case code of ExitSuccess -> 0; ExitFailure n -> n))
      writeIORef (sExited s) True
      moveToFinished pr s

-- | Bytes that decode whole, and a trailing incomplete UTF-8 sequence.
splitUtf8 :: BS.ByteString -> (BS.ByteString, BS.ByteString)
splitUtf8 bs =
  let n = BS.length bs
      tailStart = go (n - 1) (0 :: Int)
      go i k
        | i < 0 || k >= 3 = n
        | otherwise =
            let b = BS.index bs i
             in if b < 0x80 then n
                else if b >= 0xC0
                  then let need = if b >= 0xF0 then 4 else if b >= 0xE0 then 3 else 2
                        in if n - i < need then i else n
                  else go (i - 1) (k + 1)
   in BS.splitAt tailStart bs

shellQuote :: Text -> Text
shellQuote t
  | not (T.null t) && T.all (\c -> c `elem` ("@%+=:,./-_" :: String) || c `elem` ['a' .. 'z'] || c `elem` ['A' .. 'Z'] || isDigit c) t = t
  | otherwise = "'" <> T.replace "'" "'\"'\"'" t <> "'"

-- | @spawn_via_env@: run it in the sandbox under nohup, output to a log.
spawnViaSandbox :: ProcessRegistry -> Sandbox -> Text -> Maybe Text -> Text -> Text -> IO (Text, Maybe Int)
spawnViaSandbox pr sb command cwd task key = do
  s0 <- newSessionRecord pr command task key cwd
  let logPath = "/tmp/gauss_bg_" <> sId s0 <> ".log"
      pidPath = "/tmp/gauss_bg_" <> sId s0 <> ".pid"
      bg = "nohup bash -c " <> shellQuote command <> " > " <> logPath <> " 2>&1 & echo $! > " <> pidPath <> " && cat " <> pidPath
  r <- try (sbExecute sb (ExecRequest bg "" (Just 10) Nothing))
  s <- case r of
    Right res ->
      let pid = case [read (T.unpack l) :: Int | l <- map T.strip (T.lines (T.strip (exOutput res))), not (T.null l), T.all isDigit l] of
            (p : _) -> Just p
            [] -> Nothing
       in pure s0 {sPid = pid, sSandbox = Just sb}
    Left (e :: SomeException) -> do
      writeIORef (sExited s0) True
      writeIORef (sExit s0) (Just (-1))
      writeIORef (sOutput s0) ("Failed to start: " <> T.pack (displayException e))
      pure s0 {sSandbox = Just sb}
  track pr s
  ex <- readIORef (sExited s)
  when (not ex && pePollRemote (prEnv pr)) $ () <$ forkIO (poller s sb logPath pidPath)
  pure (sId s, sPid s)
  where
    poller s sb' logPath pidPath = do
      let loop = do
            ex <- readIORef (sExited s)
            unless ex $ do
              peSleep (prEnv pr) 2
              r <- try $ do
                out <- sbExecute sb' (ExecRequest ("cat " <> logPath <> " 2>/dev/null") "" (Just 10) Nothing)
                unless (T.null (exOutput out)) (writeIORef (sOutput s) (T.takeEnd maxOutput (exOutput out)))
                check <- sbExecute sb' (ExecRequest ("kill -0 $(cat " <> pidPath <> " 2>/dev/null) 2>/dev/null; echo $?") "" (Just 5) Nothing)
                let co = T.strip (exOutput check)
                if not (T.null co) && T.strip (last (T.lines co)) /= "0"
                  then do
                    e <- sbExecute sb' (ExecRequest ("wait $(cat " <> pidPath <> " 2>/dev/null) 2>/dev/null; echo $?") "" (Just 5) Nothing)
                    let es = T.strip (exOutput e)
                        code = case T.lines es of
                          [] -> -1
                          ls -> fromMaybe (-1) (readMaybeInt (T.strip (last ls)))
                    writeIORef (sExit s) (Just code)
                    writeIORef (sExited s) True
                    moveToFinished pr s
                    pure False
                  else pure True
              case r of
                Right True -> loop
                Right False -> pure ()
                Left (_ :: SomeException) -> do
                  writeIORef (sExited s) True
                  writeIORef (sExit s) (Just (-1))
                  moveToFinished pr s
      loop
    readMaybeInt t = if not (T.null t) && T.all (\c -> isDigit c || c == '-') t then Just (read (T.unpack t)) else Nothing

findSession :: ProcessRegistry -> Text -> IO (Maybe Session)
findSession pr i = do
  r <- readMVar (prRunning pr)
  f <- readMVar (prFinished pr)
  pure $ case filter ((== i) . sId) r of
    (s : _) -> Just s
    [] -> case filter ((== i) . sId) f of
      (s : _) -> Just s
      [] -> Nothing

notFound :: Text -> PyVal
notFound i = PDict [("status", PStr "not_found"), ("error", PStr ("No process with ID " <> i))]

statusOf :: Bool -> PyVal
statusOf ex = PStr (if ex then "exited" else "running")

poll :: ProcessRegistry -> Text -> IO PyVal
poll pr i =
  findSession pr i >>= \case
    Nothing -> pure (notFound i)
    Just s -> do
      out <- readIORef (sOutput s)
      ex <- readIORef (sExited s)
      code <- readIORef (sExit s)
      now <- peNow (prEnv pr)
      pure . PDict $
        [ ("session_id", PStr (sId s)), ("command", PStr (sCommand s)), ("status", statusOf ex), ("pid", maybe PNull (PInt . fromIntegral) (sPid s))
        , ("uptime_seconds", PInt (truncate (now - sStarted s))), ("output_preview", PStr (T.takeEnd 1000 out)) ]
          <> [("exit_code", maybe PNull (PInt . fromIntegral) code) | ex]

readLog :: ProcessRegistry -> Text -> Int -> Int -> IO PyVal
readLog pr i offset limit =
  findSession pr i >>= \case
    Nothing -> pure (notFound i)
    Just s -> do
      out <- readIORef (sOutput s)
      ex <- readIORef (sExited s)
      let ls = pyLines out
          total = length ls
          selected
            | offset == 0 && limit > 0 = drop (total - limit) ls
            | otherwise = take limit (drop offset ls)
      pure (PDict [("session_id", PStr (sId s)), ("status", statusOf ex), ("output", PStr (T.intercalate "\n" selected)), ("total_lines", PInt (fromIntegral total)), ("showing", PStr (T.pack (show (length selected)) <> " lines"))])

-- | @str.splitlines()@ for the line breaks a shell produces.
pyLines :: Text -> [Text]
pyLines t
  | T.null t = []
  | otherwise =
      let ls = T.splitOn "\n" (T.replace "\r\n" "\n" t)
       in if last ls == "" then init ls else ls

wait :: ProcessRegistry -> Text -> Maybe Integer -> IO (Either Text PyVal)
wait pr i requested = do
  raw <- fromMaybe "180" <$> peGetEnv (prEnv pr) "TERMINAL_TIMEOUT"
  case reads (T.unpack (T.strip raw)) :: [(Integer, String)] of
    [(maxT, "")] -> do
      let (eff, note) = case requested of
            Just r | r > maxT -> (maxT, Just ("Requested wait of " <> tshow r <> "s was clamped to configured limit of " <> tshow maxT <> "s"))
            Just r | r /= 0 -> (r, Nothing)
            _ -> (maxT, Nothing)
          withNote kvs = PDict (kvs <> [("timeout_note", PStr n) | Just n <- [note]])
      findSession pr i >>= \case
        Nothing -> pure (Right (notFound i))
        Just s -> do
          start <- peMonotonic (prEnv pr)
          let deadline = start + fromIntegral eff
              loop = do
                now <- peMonotonic (prEnv pr)
                if now >= deadline
                  then do
                    out <- readIORef (sOutput s)
                    pure (PDict ([("status", PStr "timeout"), ("output", PStr (T.takeEnd 1000 out))] <> [("timeout_note", PStr (fromMaybe ("Waited " <> tshow eff <> "s, process still running") note))]))
                  else do
                    ex <- readIORef (sExited s)
                    if ex
                      then do
                        code <- readIORef (sExit s)
                        out <- readIORef (sOutput s)
                        pure (withNote [("status", PStr "exited"), ("exit_code", maybe PNull (PInt . fromIntegral) code), ("output", PStr (T.takeEnd 2000 out))])
                      else do
                        intr <- peInterrupted (prEnv pr)
                        if intr
                          then do
                            out <- readIORef (sOutput s)
                            pure (withNote [("status", PStr "interrupted"), ("output", PStr (T.takeEnd 1000 out)), ("note", PStr "User sent a new message -- wait interrupted")])
                          else peSleep (prEnv pr) 1 >> loop
          Right <$> loop
    _ -> pure (Left ("invalid literal for int() with base 10: " <> reprStr raw))
  where
    tshow = T.pack . show

kill :: ProcessRegistry -> Text -> IO PyVal
kill pr i =
  findSession pr i >>= \case
    Nothing -> pure (notFound i)
    Just s -> do
      ex <- readIORef (sExited s)
      if ex
        then do
          code <- readIORef (sExit s)
          pure (PDict [("status", PStr "already_exited"), ("exit_code", maybe PNull (PInt . fromIntegral) code)])
        else do
          r <- try $ case (sProc s, sSandbox s, sPid s) of
            (Just _, _, Just pid) -> do
              k <- try @SomeException (signalProcessGroup sigTERM (fromIntegral pid))
              case (k, sProc s) of
                (Left _, Just ph) -> terminateProcess ph
                _ -> pure ()
            (Nothing, Just sb, Just pid) -> () <$ sbExecute sb (ExecRequest ("kill " <> T.pack (show pid) <> " 2>/dev/null") "" (Just 5) Nothing)
            _ -> pure ()
          case r of
            Left (e :: SomeException) -> pure (PDict [("status", PStr "error"), ("error", PStr (T.pack (displayException e)))])
            Right () -> do
              writeIORef (sExited s) True
              writeIORef (sExit s) (Just (-15))
              moveToFinished pr s
              pure (PDict [("status", PStr "killed"), ("session_id", PStr (sId s))])

writeStdin :: ProcessRegistry -> Text -> Text -> IO PyVal
writeStdin pr i dat =
  findSession pr i >>= \case
    Nothing -> pure (notFound i)
    Just s -> do
      ex <- readIORef (sExited s)
      if ex
        then pure (PDict [("status", PStr "already_exited"), ("error", PStr "Process has already finished")])
        else case sStdin s of
          Nothing -> pure (PDict [("status", PStr "error"), ("error", PStr "Process stdin not available (non-local backend or stdin closed)")])
          Just h -> do
            r <- try (BS.hPut h (TE.encodeUtf8 dat) >> hFlush h)
            pure $ case r of
              Right () -> PDict [("status", PStr "ok"), ("bytes_written", PInt (fromIntegral (T.length dat)))]
              Left (e :: SomeException) -> PDict [("status", PStr "error"), ("error", PStr (T.pack (displayException e)))]

listSessions :: ProcessRegistry -> Maybe Text -> IO PyVal
listSessions pr task = do
  r <- readMVar (prRunning pr)
  f <- readMVar (prFinished pr)
  now <- peNow (prEnv pr)
  let pick = filter (\s -> maybe True (\t -> T.null t || sTask s == t) task) (r <> f)
  entries <- mapM (entry now) pick
  pure (PDict [("processes", PList entries)])
  where
    entry now s = do
      ex <- readIORef (sExited s)
      code <- readIORef (sExit s)
      out <- readIORef (sOutput s)
      started <- utcToLocalZonedTime (posixSecondsToUTCTime (realToFrac (sStarted s)))
      pure . PDict $
        [ ("session_id", PStr (sId s)), ("command", PStr (T.take 200 (sCommand s))), ("cwd", maybe PNull PStr (sCwd s))
        , ("pid", maybe PNull (PInt . fromIntegral) (sPid s)), ("started_at", PStr (T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" started)))
        , ("uptime_seconds", PInt (truncate (now - sStarted s))), ("status", statusOf ex), ("output_preview", PStr (T.takeEnd 200 out)) ]
          <> [("exit_code", maybe PNull (PInt . fromIntegral) code) | ex]

hasActiveProcesses :: ProcessRegistry -> Text -> IO Bool
hasActiveProcesses pr task = do
  r <- readMVar (prRunning pr)
  or <$> mapM (\s -> if sTask s == task then not <$> readIORef (sExited s) else pure False) r

killAll :: ProcessRegistry -> IO ()
killAll pr = do
  r <- readMVar (prRunning pr)
  forM_ r (\s -> kill pr (sId s))

-- | @_handle_process(args, task_id=...)@
processTool :: ProcessRegistry -> ToolCtx -> PyVal -> IO Text
processTool pr ctx args = do
  let action = maybe "" pyStrOf (get "action" args)
      sid = case get "session_id" args of
        Nothing -> ""
        Just PNull -> ""
        Just v -> pyStrOf v
      out = pure . dumpsUnicode
      int' k d = case get k args of Just (PInt n) -> fromIntegral n; _ -> d
  case action of
    "list" -> listSessions pr (if T.null (tcTaskId ctx) then Nothing else Just (tcTaskId ctx)) >>= out
    a | a `elem` ["poll", "log", "wait", "kill", "write", "submit"] ->
      if T.null sid
        then out (PDict [("error", PStr ("session_id is required for " <> a))])
        else case a of
          "poll" -> poll pr sid >>= out
          "log" -> readLog pr sid (int' "offset" 0) (int' "limit" 200) >>= out
          "wait" -> do
            r <- wait pr sid (case get "timeout" args of Just (PInt n) -> Just n; _ -> Nothing)
            case r of
              Right v -> out v
              Left e -> pure (dumpsAscii (PDict [("error", PStr ("Tool execution failed: ValueError: " <> e))]))
          "kill" -> kill pr sid >>= out
          "write" -> writeStdin pr sid (maybe "" pyStrOf (get "data" args)) >>= out
          _ -> writeStdin pr sid (maybe "" pyStrOf (get "data" args) <> "\n") >>= out
    _ -> out (PDict [("error", PStr ("Unknown process action: " <> action <> ". Use: list, poll, log, wait, kill, write, submit"))])

processToolDef :: ProcessRegistry -> ToolDef
processToolDef pr =
  let e = fromMaybe (PDict []) (get "process" toolSchemas)
   in ToolDef "process" "terminal" (fromMaybe PNull (get "schema" e)) (processTool pr) Nothing [] (maybe "" pyStrOf (get "emoji" e))
