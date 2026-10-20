-- | @fisher acp@ against upstream's @acp@ command (@test/golden/e1/acp.json@,
-- from tools/corpus/e1.py): the scripted client's messages go in one at a
-- time, each request waiting for its answer, and every message the server
-- sends back must be the one upstream sent, in order, with the HTTP
-- answers and tool results upstream got. Lines upstream's agent printed
-- onto stdout (its spinner) are not JSON-RPC and are left out.
module Fisher.AcpSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Monad (forM_, unless, when)
import qualified Data.ByteString as BS
import Data.IORef
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (LocalTime (..), TimeOfDay (..), fromGregorian, utc)
import Numeric (showHex)
import System.Directory
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Hspec

import Fisher.Agent.Loop (ToolRunner (..))
import Fisher.Cli.Acp
import Fisher.Cli.AgentChat (ChatDeps (..))
import Fisher.LLM.Errors (LLMError (..))
import Fisher.LLM.Http
import Fisher.LLM.Sys
import Fisher.LLM.Types
import Fisher.Py.Value
import Fisher.Tools.Agent (AgentTools (..), newAgentTools)
import Fisher.Tools.Registry (getToolDefinitions)

txt :: Text -> PyVal -> Text
txt k c = maybe "" pyStrOf (get k c)

fld :: Text -> PyVal -> PyVal
fld k c = maybe PNull id (get k c)

lst :: Text -> PyVal -> [PyVal]
lst k c = case get k c of
  Just (PList xs) -> xs
  _ -> []

uuidAt :: Int -> Text
uuidAt n = T.justifyRight 32 '0' (T.pack (showHex (toInteger n * 0x1F2E3D * 2 ^ (104 :: Int)) ""))

hex12 :: Integer -> Text
hex12 n = T.justifyRight 12 '0' (T.pack (showHex n ""))

-- | JSON objects compare without their key order.
canon :: PyVal -> PyVal
canon = \case
  PDict kvs -> PDict (sortOn fst [(k, canon v) | (k, v) <- kvs])
  PList xs -> PList (map canon xs)
  v -> v

keep :: [Text]
keep = ["authorization", "content-type", "accept", "http-referer", "x-openrouter-title", "x-openrouter-categories"]

reqVal :: HttpReq -> PyVal
reqVal hr =
  canon $ PDict
    [ ("method", PStr (hrMethod hr)), ("url", PStr (hrUrl hr))
    , ("headers", PDict [(k, PStr v) | (k, v) <- hrHeaders hr, k `elem` keep])
    , ("body", maybe PNull id (hrBody hr)) ]

goldenReq :: PyVal -> PyVal
goldenReq r =
  canon $ PDict
    [ ("method", fld "method" r), ("url", fld "url" r)
    , ("headers", case fld "headers" r of PDict kvs -> PDict [kv | kv@(k, _) <- kvs, k `elem` keep]; v -> v)
    , ("body", fld "body" r) ]

isCancel :: PyVal -> Bool
isCancel m = get "method" m == Just (PStr "session/cancel")

spec :: Spec
spec = do
  doc <- runIO (either fail pure . loads =<< BS.readFile "test/golden/e1/acp.json")
  describe "fisher acp" $
    forM_ (lst "cases" doc) $ \c -> it (T.unpack (txt "name" c)) $ withSystemTempDirectory "fisher-acp" $ \tmp0 -> do
      tmp <- canonicalizePath tmp0
      let sc = fld "scenario" c
          home = tmp </> "home"
          fisher = home </> ".fisher"
          work = tmp </> "work"
          record = fld "record" c
      createDirectoryIfMissing True (fisher </> "memories")
      createDirectoryIfMissing True work
      offset <- newIORef (0 :: Double)
      uuidN <- newIORef (0 :: Int)
      sessN <- newIORef (0 :: Int)
      toolN <- newIORef (0 :: Integer)
      reqs <- newIORef []
      queue <- newIORef [r | r <- lst "requests" record, fld "method" r /= PStr "GET"]
      answers <- newIORef (lst "answers" sc)
      toolQ <- newIORef (lst "tools" record)
      env <- newIORef . M.fromList $
        [("HOME", T.pack home), ("FISHER_TIMEZONE", "UTC")]
          <> [("OPENROUTER_API_KEY", "sk-or-test-1234567890") | fld "no_key" sc /= PBool True]
      -- the connection: lines in, messages out
      inbox <- newChan'
      outs <- newIORef []
      outSignal <- newEmptyMVar
      readsSince <- newIORef (0 :: Int)
      let clock = (1790000000 +) <$> readIORef offset
          sleep d = modifyIORef offset (+ d)
          sys =
            Sys
              { sysNow = clock, sysMonotonic = (1000 +) <$> readIORef offset, sysSleep = sleep
              , sysGetEnv = \k -> M.lookup k <$> readIORef env
              , sysSetEnv = \k v -> modifyIORef env (M.insert k v)
              , sysHttp = \_ -> pure (Left (HttpFail "ConnectError" "no network in tests"))
              , sysOpenBrowser = \_ -> pure False, sysRun = \_ -> pure (Left "no"), sysRunInteractive = \_ -> pure (Left "no")
              , sysWhich = \_ -> pure Nothing, sysPlatform = "linux"
              , sysUuidHex = atomicModifyIORef' uuidN (\n -> (n + 1, uuidAt (n + 1)))
              , sysTimeZoneAt = \_ -> pure utc, sysHome = home, sysFisherHome = fisher
              , sysOut = \_ -> pure (), sysInput = \_ -> pure Nothing, sysGetPass = \_ -> pure Nothing
              , sysStdinTty = pure False, sysStdoutTty = pure False, sysPid = 4242 }
          sub t = T.replace "<WORK>" (T.pack work) t
          mask t = T.replace (T.pack fisher) "<FISHER_HOME>" (T.replace (T.pack work) "<WORK>" t)
          cancelMsg = case filter isCancel [fld "msg" m | m <- lst "messages" c, fld "dir" m == PStr "in"] of
            (m : _) -> m
            [] -> PNull
          respond hr = do
            modifyIORef reqs (<> [hr])
            if hrMethod hr == "GET"
              then pure (Right (HttpResp 200 [] (TE.encodeUtf8 (dumpsAscii (lookupModels record)))))
              else do
                as <- readIORef answers
                case as of
                  (a : rest) -> do
                    writeIORef answers rest
                    when (get "await_cancel" a == Just (PBool True)) $ do
                      -- hand the client's session/cancel to the server and
                      -- wait until it has been read and handled
                      n0 <- readIORef readsSince
                      putChan' inbox (TE.encodeUtf8 (sub (dumpsUnicode cancelMsg)))
                      let wait = do
                            n <- readIORef readsSince
                            unless (n >= n0 + 1) (yieldMs >> wait)
                      _ <- timeout 5000000 wait
                      pure ()
                  [] -> pure ()
                q <- readIORef queue
                case q of
                  [] -> pure (Right (HttpResp 500 [] "{\"error\": {\"message\": \"no canned answer\"}}"))
                  (r : rest) -> do
                    writeIORef queue rest
                    let resp = fld "response" r
                    pure $
                      if get "connect_error" resp == Just (PBool True)
                        then Left ConnectionError
                        else Right (HttpResp (case fld "status" resp of PInt s -> fromIntegral s; _ -> 200) [] (TE.encodeUtf8 (dumpsAscii (fld "body" resp))))
      at <- newAgentTools fisher
      (defs, _) <- getToolDefinitions (atRegistry at) (Just ["fisher-acp"]) Nothing
      let scripted tr =
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
              , cdRuntime = Runtime clock sleep (pure 0)
              , cdChoice = \_ -> pure 0
              , cdAnimate = pure False
              , cdCwd = pure work
              , cdWhich = \_ -> pure Nothing
              , cdBannerInfo = pure ("<WORK>", Nothing, True) }
          deps =
            AcpDeps
              { adChat = chat
              , adSessionUuid = do
                  n <- atomicModifyIORef' sessN (\k -> (k + 1, k + 1))
                  pure ("acb00000-0000-0000-0000-" <> hex12 (toInteger n))
              , adToolCallHex = do
                  n <- atomicModifyIORef' toolN (\k -> (k + 1, k + 1))
                  pure (hex12 (0x7C0000000000 + n) <> T.replicate 20 "0")
              , adLog = \_ -> pure ()
              , adReplay = False }
          io =
            AcpIO
              { aiRead = do
                  modifyIORef readsSince (+ 1)
                  takeChan' inbox
              , aiWrite = \b -> case loads b of
                  Right v -> modifyIORef outs (<> [v]) >> () <$ tryPutMVar outSignal ()
                  Left _ -> pure () }
      server <- newEmptyMVar
      forkIO' (acpServe deps sys io >> putMVar server ())
      let waitFor rid = do
            let loop = do
                  os <- readIORef outs
                  unless (any (\o -> get "id" o == Just rid && not (has "method" o)) os) $ do
                    _ <- timeout 200000 (takeMVar outSignal)
                    loop
            r <- timeout 30000000 loop
            when (r == Nothing) (expectationFailure ("no answer to request " <> T.unpack (pyRepr rid)))
      forM_ [fld "msg" m | m <- lst "messages" c, fld "dir" m == PStr "in", not (isCancel (fld "msg" m))] $ \m -> do
        unless (has "result" m || has "error" m) $ do
          putChan' inbox (TE.encodeUtf8 (sub (dumpsUnicode m)))
          forM_ (if has "id" m && has "method" m then get "id" m else Nothing) waitFor
      -- notifications sent last get a moment, as the recorder gave them
      yieldMs
      closeChan' inbox
      _ <- timeout 10000000 (takeMVar server)
      got <- readIORef outs
      let want = [fld "msg" m | m <- lst "messages" c, fld "dir" m == PStr "out", has "msg" m]
          maskVal v = either (const v) id (loads (TE.encodeUtf8 (mask (dumpsUnicode v))))
      map (canon . maskVal) got `shouldBe` map canon want
      rs <- readIORef reqs
      map reqVal rs `shouldBe` map goldenReq (lst "requests" record)
  where
    -- the model list e1_agent.py answers /models with
    lookupModels _ =
      PDict
        [ ( "data"
          , PList
              [ PDict [("id", PStr "anthropic/claude-opus-4.6"), ("context_length", PInt 1000000), ("name", PStr "Claude Opus 4.6")]
              , PDict [("id", PStr "tiny/model"), ("context_length", PInt 16000), ("name", PStr "Tiny")]
              , PDict [("id", PStr "openai/gpt-5"), ("context_length", PInt 400000)] ] ) ]

-- a small unbounded channel of lines, closable
newtype Chan' = Chan' (MVar ([BS.ByteString], Bool), MVar ())

newChan' :: IO Chan'
newChan' = (\a b -> Chan' (a, b)) <$> newMVar ([], False) <*> newEmptyMVar

putChan' :: Chan' -> BS.ByteString -> IO ()
putChan' (Chan' (v, sig)) x = modifyMVar_ v (\(xs, c) -> pure (xs <> [x], c)) >> () <$ tryPutMVar sig ()

closeChan' :: Chan' -> IO ()
closeChan' (Chan' (v, sig)) = modifyMVar_ v (\(xs, _) -> pure (xs, True)) >> () <$ tryPutMVar sig ()

takeChan' :: Chan' -> IO (Maybe BS.ByteString)
takeChan' ch@(Chan' (v, sig)) = do
  r <- modifyMVar v $ \case
    (x : xs, c) -> pure ((xs, c), Just (Just x))
    ([], True) -> pure (([], True), Just Nothing)
    st -> pure (st, Nothing)
  case r of
    Just x -> pure x
    Nothing -> takeMVar sig >> takeChan' ch

yieldMs :: IO ()
yieldMs = threadDelay 20000

forkIO' :: IO () -> IO ()
forkIO' act = () <$ forkIO act
