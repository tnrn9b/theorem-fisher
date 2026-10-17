-- | @fisher cron@ against @gauss cron@ (@test/golden/e2/cli.json@ and
-- @tick.json@, from tools/corpus/e2.py).
--
-- The CLI: each case's commands in order, the clock moved between them;
-- the printed output (with upstream's pointers to its gateway replaced by
-- Fisher's to @fisher cron serve@), the exit code and jobs.json after each.
--
-- The ticker: jobs made as upstream made them, the clock moved, then
-- @tick@ with the model answering as it answered upstream and the tools
-- giving upstream's results; the requests, the output files (a failed
-- run's up to upstream's Python traceback), jobs.json, the session rows
-- and the agent's printing.
module Fisher.CronCliSpec (spec) where

import Control.Monad (forM_, when)
import qualified Data.ByteString as BS
import Data.IORef
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (LocalTime (..), TimeOfDay (..), fromGregorian, utc)
import Numeric (showHex)
import System.Directory
import System.FileLock (SharedExclusive (..), lockFile, unlockFile)
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Agent.Loop (ToolRunner (..))
import Fisher.Cli.AgentChat (ChatDeps (..))
import Fisher.Cli.Cron
import Fisher.Cron.Dt
import Fisher.Cron.Jobs
import Fisher.Cron.Zone (utcZone)
import Fisher.LLM.Errors (LLMError (..))
import Fisher.LLM.Http
import Fisher.LLM.Sys
import Fisher.LLM.Types
import Fisher.Py.Value
import Fisher.Store (closeStore, openStore, snapshotRows)
import Fisher.Tools.Agent (AgentTools (..), newAgentTools)
import Fisher.Tools.Registry (getToolDefinitions)

txt :: Text -> PyVal -> Text
txt k c = maybe "" pyStrOf (get k c)

fld :: Text -> PyVal -> PyVal
fld k c = fromMaybe PNull (get k c)

lst :: Text -> PyVal -> [PyVal]
lst k c = case get k c of
  Just (PList xs) -> xs
  _ -> []

canon :: PyVal -> PyVal
canon = \case
  PDict kvs -> PDict (sortOn fst [(k, canon v) | (k, v) <- kvs])
  PList xs -> PList (map canon xs)
  v -> v

-- | Upstream's gateway pointers, and Fisher's in their place.
fisherText :: Text -> Text
fisherText =
  T.replace
    "  ⚠  Gateway is not running — jobs won't fire automatically.\n     Start it with: gauss gateway install\n                    sudo gauss gateway install --system  # Linux servers\n"
    "  ⚠  The cron ticker is not running — jobs won't fire automatically.\n     Start it with: fisher cron serve\n"
    . T.replace
      "✗ Gateway is not running — cron jobs will NOT fire\n\n  To enable automatic execution:\n    gauss gateway install    # Install as a user service\n    sudo gauss gateway install --system  # Linux servers: boot-time system service\n    gauss gateway            # Or run in foreground\n"
      "✗ The cron ticker is not running — cron jobs will NOT fire\n\n  To enable automatic execution:\n    fisher cron serve        # Run the ticker (keep it running, e.g. under launchd or systemd)\n"

clockStart :: Dt
clockStart = case fromIsoformat "2026-10-02T04:50:07.123456+00:00" of
  Right d -> astimezone (TzZone utcZone Nothing) d
  Left e -> error (T.unpack e)

cronEnvFor :: FilePath -> IORef Integer -> IORef Integer -> CronEnv
cronEnvFor gauss clock ids =
  CronEnv
    { ceHome = gauss
    , ceNow = do
        us <- readIORef clock
        pure (dtFromInstant (TzZone utcZone Nothing) us)
    , ceLocalOffset = pure 0
    , ceNewHex = do
        n <- atomicModifyIORef' ids (\k -> (k + 1, k + 1))
        pure (T.justifyRight 32 '0' (T.pack (showHex (((0xC0FFEE000000 + n) * 2 ^ (80 :: Int)) + 0xABCDEF) ""))) }

readJobs :: FilePath -> IO PyVal
readJobs gauss = do
  let f = gauss </> "cron" </> "jobs.json"
  ex <- doesFileExist f
  if ex then either fail pure . loads =<< BS.readFile f else pure PNull

outputs :: FilePath -> IO PyVal
outputs gauss = do
  let d = gauss </> "cron" </> "output"
  ex <- doesDirectoryExist d
  if not ex
    then pure (PDict [])
    else do
      jobsDirs <- listDirectory d
      fs <- fmap concat . mapM (\j -> do
        isDir <- doesDirectoryExist (d </> j)
        if not isDir then pure [] else do
          files <- listDirectory (d </> j)
          mapM (\f -> (,) (T.pack (j </> f)) . TE.decodeUtf8 <$> BS.readFile (d </> j </> f)) [f | f <- files, takeExtension f == ".md"]) $ jobsDirs
      pure (PDict (sortOn fst [(k, PStr v) | (k, v) <- fs]))

-- | A failed run's document up to upstream's traceback.
beforeTraceback :: PyVal -> PyVal
beforeTraceback = \case
  PDict kvs -> PDict [(k, PStr (fst (T.breakOn "\n\nTraceback" (fst (T.breakOn "\n\n(no traceback" (pyStrOf v)))))) | (k, v) <- kvs]
  v -> v

newSys :: FilePath -> IORef (M.Map Text Text) -> IORef Double -> IORef Int -> IORef [Text] -> Sys
newSys home env offset uuidN outBuf =
  let clock = (1790000000 +) <$> readIORef offset
   in Sys
        { sysNow = clock, sysMonotonic = (1000 +) <$> readIORef offset, sysSleep = \d -> modifyIORef offset (+ d)
        , sysGetEnv = \k -> M.lookup k <$> readIORef env
        , sysSetEnv = \k v -> modifyIORef env (M.insert k v)
        , sysHttp = \_ -> pure (Left (HttpFail "ConnectError" "no network in tests"))
        , sysOpenBrowser = \_ -> pure False, sysRun = \_ -> pure (Left "no"), sysRunInteractive = \_ -> pure (Left "no")
        , sysWhich = \_ -> pure Nothing, sysPlatform = "linux"
        , sysUuidHex = atomicModifyIORef' uuidN (\n -> (n + 1, T.justifyRight 32 '0' (T.pack (showHex (toInteger (n + 1) * 0x1F2E3D * 2 ^ (104 :: Int)) ""))))
        , sysTimeZoneAt = \_ -> pure utc, sysHome = home, sysGaussHome = home </> ".gauss"
        , sysOut = \t -> modifyIORef outBuf (<> [t]), sysInput = \_ -> pure Nothing, sysGetPass = \_ -> pure Nothing
        , sysStdinTty = pure False, sysStdoutTty = pure False, sysPid = 4242 }

keep :: [Text]
keep = ["authorization", "content-type", "accept", "http-referer", "x-openrouter-title", "x-openrouter-categories"]

reqVal :: HttpReq -> PyVal
reqVal hr =
  canon $ PDict
    [ ("method", PStr (hrMethod hr)), ("url", PStr (hrUrl hr))
    , ("headers", PDict [(k, PStr v) | (k, v) <- hrHeaders hr, k `elem` keep])
    , ("body", fromMaybe PNull (hrBody hr)) ]

goldenReq :: PyVal -> PyVal
goldenReq r =
  canon $ PDict
    [ ("method", fld "method" r), ("url", fld "url" r)
    , ("headers", case fld "headers" r of PDict kvs -> PDict [kv | kv@(k, _) <- kvs, k `elem` keep]; v -> v)
    , ("body", fld "body" r) ]

normalizeOut :: Text -> Text
normalizeOut = T.intercalate "\n" . map (last . T.splitOn "\r") . T.splitOn "\n"

spec :: Spec
spec = do
  cli <- runIO (either fail pure . loads =<< BS.readFile "test/golden/e2/cli.json")
  describe "gauss cron" $
    forM_ (lst "cases" cli) $ \c -> it (T.unpack (txt "name" c)) $ withSystemTempDirectory "fisher-cron" $ \tmp0 -> do
      home <- canonicalizePath tmp0
      let gauss = home </> ".gauss"
      createDirectoryIfMissing True gauss
      env <- newIORef (M.fromList [("HOME", T.pack home), ("NO_COLOR", "1"), ("GAUSS_TIMEZONE", "UTC")])
      offset <- newIORef 0
      uuidN <- newIORef 0
      outBuf <- newIORef []
      clock <- newIORef (dtInstantMicros clockStart)
      ids <- newIORef 0
      let sys = newSys home env offset uuidN outBuf
          deps =
            (defaultCronDeps)
              { crEnv = \_ -> pure (cronEnvFor gauss clock ids)
              , crTicker = \_ -> pure Nothing
              , crErr = \t -> modifyIORef outBuf (<> [t <> "\n"])
              , crReplay = False }
      forM_ (lst "steps" c) $ \st -> case get "advance" st of
        Just (PInt secs) -> modifyIORef clock (+ secs * 1000000)
        _ -> do
          writeIORef outBuf []
          code <- cronMain deps sys (drop 1 [a | PStr a <- lst "argv" st])
          out <- T.concat <$> readIORef outBuf
          PInt (fromIntegral code) `shouldBe` fld "exit" st
          let want = txt "output" st
          when (not (T.null want) || fld "exit" st == PInt 0) $ out `shouldBe` fisherText want
          j <- readJobs gauss
          j `shouldBe` fld "jobs" st
  tickDoc <- runIO (either fail pure . loads =<< BS.readFile "test/golden/e2/tick.json")
  describe "cron.scheduler.tick" $
    forM_ (lst "cases" tickDoc) $ \c -> it (T.unpack (txt "name" c)) $ withSystemTempDirectory "fisher-tick" $ \tmp0 -> do
      home <- canonicalizePath tmp0
      let gauss = home </> ".gauss"
          setup = fld "setup" c
          work = home </> "work"
      createDirectoryIfMissing True (gauss </> "memories")
      createDirectoryIfMissing True work
      BS.writeFile (work </> "README.md") "# Demo\n\nA small project.\n"
      env <- newIORef (M.fromList ([("HOME", T.pack home), ("NO_COLOR", "1"), ("GAUSS_TIMEZONE", "UTC")]
        <> [("OPENROUTER_API_KEY", "sk-or-test-1234567890") | fld "no_key" setup /= PBool True]))
      when (fld "no_key" setup /= PBool True) $ BS.writeFile (gauss </> ".env") "OPENROUTER_API_KEY=sk-or-test-1234567890\n"
      case get "config" setup of
        Just (PStr y) -> BS.writeFile (gauss </> "config.yaml") (TE.encodeUtf8 y)
        _ -> pure ()
      case get "skills" setup of
        Just (PDict kvs) -> forM_ kvs $ \(rel, v) -> do
          let p = gauss </> "skills" </> T.unpack rel
          createDirectoryIfMissing True (takeDirectory p)
          BS.writeFile p (TE.encodeUtf8 (pyStrOf v))
        _ -> pure ()
      offset <- newIORef 0
      uuidN <- newIORef 0
      outBuf <- newIORef []
      clock <- newIORef (dtInstantMicros clockStart)
      ids <- newIORef 0
      reqs <- newIORef []
      queue <- newIORef []
      toolQ <- newIORef []
      let sys = newSys home env offset uuidN outBuf
          cenv = cronEnvFor gauss clock ids
      -- the jobs, as the recorder made them
      forM_ (lst "jobs" setup) $ \case
        PList [PStr sched, PStr prompt, opts] -> do
          r <- createJob cenv
            (createArgs prompt sched)
              { caName = case get "name" opts of Just (PStr n) -> Just n; _ -> Nothing
              , caRepeat = case get "repeat" opts of Just (PInt n) -> Just n; _ -> Nothing
              , caDeliver = case get "deliver" opts of Just (PStr d) -> Just d; _ -> Nothing
              , caSkills = case get "skills" opts of Just (PList ss) -> [s | PStr s <- ss]; _ -> []
              , caModel = case get "model" opts of Just (PStr m) -> Just m; _ -> Nothing }
          case r of
            Right job -> do
              let jid = txt "id" job
              when (get "pause" opts == Just (PBool True)) $ () <$ pauseJob cenv jid Nothing
              when (get "trigger" opts == Just (PBool True)) $ () <$ triggerJob cenv jid
            Left e -> expectationFailure (T.unpack e)
        _ -> pure ()
      case lst "steps" c of
        first : _ -> readJobs gauss >>= (`shouldBe` fld "jobs" first)
        [] -> pure ()
      at <- newAgentTools gauss
      (defs, _) <- getToolDefinitions (atRegistry at) Nothing (Just ["cronjob"])
      let respond hr = do
            modifyIORef reqs (<> [hr])
            if hrMethod hr == "GET"
              then pure (Right (HttpResp 200 [] (TE.encodeUtf8 (dumpsAscii models))))
              else do
                q <- readIORef queue
                case q of
                  [] -> pure (Right (HttpResp 500 [] "{\"error\": {\"message\": \"no canned answer\"}}"))
                  (r : rest) -> do
                    writeIORef queue rest
                    let resp = fld "response" r
                    pure (Right (HttpResp (case fld "status" resp of PInt s -> fromIntegral s; _ -> 200) [] (TE.encodeUtf8 (dumpsAscii (fld "body" resp)))))
          scripted tr =
            tr
              { trCall = \name args _ _ -> do
                  q <- readIORef toolQ
                  case break (\t -> fld "name" t == PStr name && fld "args" t == args) q of
                    (pre, t : post) -> writeIORef toolQ (pre <> post) >> pure (txt "result" t)
                    _ -> pure "{\"error\": \"no recorded result\"}"
              , trReadFilesSummary = \_ -> pure [] }
          chat =
            ChatDeps
              { cdTransport = pure (Transport (\hr _ -> respond hr) (\_ _ _ -> pure (Left ConnectionError)))
              , cdTools = Just (defs, scripted)
              , cdNowLocal = pure (LocalTime (fromGregorian 2026 10 2) (TimeOfDay 4 50 7))
              , cdRuntime = Runtime ((1790000000 +) <$> readIORef offset) (\d -> modifyIORef offset (+ d)) (pure 0)
              , cdChoice = \_ -> pure 0
              , cdAnimate = pure False
              , cdCwd = pure work
              , cdWhich = \_ -> pure Nothing
              , cdBannerInfo = pure ("<WORK>", Nothing, True) }
          deps = (defaultCronDeps) {crChat = chat, crEnv = \_ -> pure cenv, crTicker = \_ -> pure Nothing, crErr = \_ -> pure (), crReplay = False}
      forM_ (drop 1 (lst "steps" c)) $ \st -> case get "advance" st of
        Just (PInt secs) -> modifyIORef clock (+ secs * 1000000)
        _ -> do
          writeIORef reqs []
          writeIORef outBuf []
          writeIORef offset 0
          writeIORef uuidN 0
          writeIORef queue [r | r <- lst "requests" st, fld "method" r /= PStr "GET"]
          writeIORef toolQ (lst "tools" st)
          held <-
            if txt "name" c == "locked"
              then do
                createDirectoryIfMissing True (gauss </> "cron")
                Just <$> lockFile (gauss </> "cron" </> ".tick.lock") Exclusive
              else pure Nothing
          n <- tick deps sys
          mapM_ unlockFile held
          PInt (fromIntegral n) `shouldBe` fld "tick" st
          rs <- readIORef reqs
          map reqVal rs `shouldBe` map goldenReq (lst "requests" st)
          o <- outputs gauss
          beforeTraceback o `shouldBe` beforeTraceback (fld "outputs" st)
          j <- readJobs gauss
          j `shouldBe` fld "jobs" st
          out <- T.concat <$> readIORef outBuf
          normalizeOut (T.replace (T.pack gauss) "<GAUSS_HOME>" out) `shouldBe` normalizeOut (txt "stdout" st)
          case fld "db" st of
            PNull -> pure ()
            db -> do
              stDb <- openStore (gauss </> "state.db")
              (ss, ms) <- snapshotRows stDb
              closeStore stDb
              let pick ks row = PDict [(k, fromMaybe PNull (lookup k row)) | k <- ks]
              PList [pick ["id", "source", "model", "parent_session_id", "message_count", "tool_call_count", "title"] r | r <- sortOn (\row -> maybe "" pyStrOf (lookup "id" row)) ss]
                `shouldBe` fld "sessions" db
              PList [pick ["session_id", "role", "content", "tool_name"] m | m <- ms] `shouldBe` fld "messages" db
  where
    models =
      PDict
        [ ( "data"
          , PList
              [ PDict [("id", PStr "anthropic/claude-opus-4.6"), ("context_length", PInt 1000000), ("name", PStr "Claude Opus 4.6")]
              , PDict [("id", PStr "tiny/model"), ("context_length", PInt 16000), ("name", PStr "Tiny")]
              , PDict [("id", PStr "openai/gpt-5"), ("context_length", PInt 400000)] ] ) ]
