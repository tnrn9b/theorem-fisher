-- | @fisher chat --context clm@ (docs/clm.md sections 3, 9 to 11): a
-- scripted turn through the CLI, then what it leaves behind: the context
-- file and @state.json@ under @$GAUSS_HOME/fisher/ctx/<session>/@, the
-- prompt section in the system prompt, the context line on the result,
-- the full transcript in @state.db@, and a replay log that replays bit
-- for bit. Also the flag's errors and the local-backend requirement.
module Fisher.ClmCliSpec (spec, Run (..), runChatWith, runChatWithDeps, completion, capturingStderr) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (LocalTime (..), TimeOfDay (..), fromGregorian, utc)
import Numeric (showHex)
import System.Directory
import System.FilePath
import Control.Exception (finally)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.IO (IOMode (WriteMode), hClose, hFlush, openFile, stderr)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Agent.Loop (ToolRunner (..))
import Fisher.Cli.AgentChat
import Fisher.Cli.Clm (ContextChoice (..), contextChoice, liveHistory)
import Fisher.Agent.Context.Clm (ClmState (..), newClmState)
import Fisher.Agent.Context.Mirror (Turn (..))
import Fisher.LLM.Errors (LLMError (..))
import Fisher.LLM.Http
import Fisher.LLM.Sys
import Fisher.LLM.Types
import Fisher.Py.Value
import Fisher.Store (closeStore, openStore, snapshotRows)
import Fisher.Tools.Agent (AgentTools (..), newAgentTools)
import Fisher.Tools.Registry (getToolDefinitions)

completion :: PyVal -> PyVal
completion msg =
  PDict
    [ ("id", PStr "chatcmpl-1"), ("object", PStr "chat.completion"), ("model", PStr "anthropic/claude-opus-4.6")
    , ("choices", PList [PDict [("index", PInt 0), ("message", msg), ("finish_reason", PStr (if get "tool_calls" msg == Nothing then "stop" else "tool_calls"))]])
    , ("usage", PDict [("prompt_tokens", PInt 100), ("completion_tokens", PInt 10), ("total_tokens", PInt 110)]) ]

messages0 :: [PyVal]
messages0 =
  [ PDict
      [ ("role", PStr "assistant"), ("content", PStr "Reading.")
      , ("tool_calls", PList [PDict [("id", PStr "call_1"), ("type", PStr "function"), ("function", PDict [("name", PStr "read_file"), ("arguments", PStr "{\"path\": \"Math/Foo.lean\"}")])]]) ]
  , PDict [("role", PStr "assistant"), ("content", PStr "It builds.")] ]

answers :: [PyVal]
answers = map completion messages0

-- | A completion with OpenRouter's usage: a cost, and cached and
-- cache-write prompt tokens.
priced :: Maybe Double -> PyVal -> PyVal
priced cost msg =
  set
    "usage"
    ( PDict $
        [ ("prompt_tokens", PInt 100), ("completion_tokens", PInt 10), ("total_tokens", PInt 110)
        , ("prompt_tokens_details", PDict [("cached_tokens", PInt 60), ("cache_write_tokens", PInt 30)]) ]
          <> [("cost", PFloat c) | Just c <- [cost]] )
    (completion msg)

data Run = Run {rCode :: Int, rOut :: Text, rReqs :: [HttpReq], rGauss :: FilePath, rSys :: Sys}

runChat :: FilePath -> [(Text, Text)] -> [Text] -> IO Run
runChat = runChatWith answers

runChatWith :: [PyVal] -> FilePath -> [(Text, Text)] -> [Text] -> IO Run
runChatWith = runChatWithDeps id

-- | 'runChatWith' with the session's dependencies changed first.
runChatWithDeps :: (ChatDeps -> ChatDeps) -> [PyVal] -> FilePath -> [(Text, Text)] -> [Text] -> IO Run
runChatWithDeps change answers' tmp extraEnv argv = do
  let home = tmp </> "home"
      gauss = home </> ".gauss"
      work = tmp </> "work"
  createDirectoryIfMissing True gauss
  createDirectoryIfMissing True work
  offset <- newIORef (0 :: Double)
  uuidN <- newIORef (0 :: Int)
  outBuf <- newIORef []
  reqs <- newIORef []
  queue <- newIORef answers'
  env <-
    newIORef . M.fromList $
      [("HOME", T.pack home), ("COLUMNS", "60"), ("GAUSS_FORCE_PLAIN_OUTPUT", "1"), ("GAUSS_TIMEZONE", "UTC"), ("OPENROUTER_API_KEY", "sk-or-test-1234567890")]
        <> extraEnv
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
          , sysUuidHex = atomicModifyIORef' uuidN (\n -> (n + 1, T.justifyRight 32 '0' (T.pack (showHex (n + 1) ""))))
          , sysTimeZoneAt = \_ -> pure utc, sysHome = home, sysGaussHome = gauss
          , sysOut = \t -> modifyIORef outBuf (<> [t]), sysInput = \_ -> pure Nothing, sysGetPass = \_ -> pure Nothing
          , sysStdinTty = pure False, sysStdoutTty = pure False, sysPid = 4242 }
      respond hr = do
        modifyIORef reqs (<> [hr])
        if hrMethod hr == "GET"
          then pure (Right (HttpResp 200 [] (TE.encodeUtf8 (dumpsAscii (PDict [("data", PList [PDict [("id", PStr "anthropic/claude-opus-4.6"), ("context_length", PInt 200000)]])])))))
          else do
            q <- readIORef queue
            case q of
              [] -> pure (Right (HttpResp 500 [] "{\"error\": {\"message\": \"no canned answer\"}}"))
              (r : rest) -> writeIORef queue rest >> pure (Right (HttpResp 200 [] (TE.encodeUtf8 (dumpsAscii r))))
  at <- newAgentTools gauss
  (defs, _) <- getToolDefinitions (atRegistry at) (Just ["gauss-cli"]) Nothing
  let scripted tr = tr {trCall = \_ _ _ _ -> pure "{\"content\": \"     1|theorem foo : True := trivial\", \"total_lines\": 1}", trReadFilesSummary = \_ -> pure []}
      deps =
        ChatDeps
          { cdTransport = pure (Transport (\hr _ -> respond hr) (\_ _ _ -> pure (Left ConnectionError)))
          , cdTools = Just (defs, scripted)
          , cdNowLocal = pure (LocalTime (fromGregorian 2026 10 6) (TimeOfDay 10 15 0))
          , cdRuntime = Runtime clock sleep (pure 0)
          , cdChoice = \_ -> pure 0
          , cdAnimate = pure False
          , cdCwd = pure work
          , cdWhich = \_ -> pure Nothing
          , cdBannerInfo = pure ("<WORK>", Nothing, True) }
  code <- chatMain (change deps) sys argv
  o <- T.concat <$> readIORef outBuf
  rs <- readIORef reqs
  pure (Run code o rs gauss sys)

bodyOf :: HttpReq -> PyVal
bodyOf = maybe PNull id . hrBody

messagesOf :: HttpReq -> [PyVal]
messagesOf hr = case get "messages" (bodyOf hr) of
  Just (PList ms) -> ms
  _ -> []

textOf :: PyVal -> Text
textOf m = case get "content" m of
  Just (PStr t) -> t
  Just (PList ps) -> T.concat [t | p <- ps, Just (PStr t) <- [get "text" p]]
  _ -> ""

-- | What an action writes to the process's stderr (the CLI's warnings go
-- there, not through Sys), into a file under the given directory.
capturingStderr :: FilePath -> IO a -> IO (a, Text)
capturingStderr dir act = do
  let file = dir </> "stderr.txt"
  saved <- hDuplicate stderr
  h <- openFile file WriteMode
  hFlush stderr
  hDuplicateTo h stderr
  a <- act `finally` (hFlush stderr >> hDuplicateTo saved stderr >> hClose h >> hClose saved)
  t <- TE.decodeUtf8 <$> BS.readFile file
  pure (a, t)

onlySession :: FilePath -> IO Text
onlySession gauss = do
  fs <- listDirectory (gauss </> "fisher" </> "replays")
  case fs of
    [f] -> pure (T.pack (dropExtension f))
    _ -> fail ("replay logs: " <> show fs)

spec :: Spec
spec = do
  describe "fisher chat --context clm" $ do
    it "runs the turn under CLM and leaves the context file, state.json and a replayable log" $ withSystemTempDirectory "fisher-clm-cli" $ \tmp0 -> do
      tmp <- canonicalizePath tmp0
      r <- runChat tmp [] ["-q", "Check that the project builds.", "-Q", "--context", "clm"]
      rCode r `shouldBe` 0
      let ctxRoot = rGauss r </> "fisher" </> "ctx"
      sids <- listDirectory ctxRoot
      length sids `shouldBe` 1
      let sid = head sids
          dir = ctxRoot </> sid
          path = T.pack (dir </> "context.txt")
      doesFileExist (dir </> "context.txt") `shouldReturn` True
      st <- either fail pure . loads =<< BS.readFile (dir </> "state.json")
      case get "turns" st of
        Just (PList ts) -> length ts `shouldBe` 3
        _ -> expectationFailure "no turns in state.json"
      let posts = [h | h <- rReqs r, hrMethod h == "POST"]
      length posts `shouldBe` 2
      let sysText = textOf (head (messagesOf (last posts)))
      sysText `shouldSatisfy` T.isInfixOf ("Your conversation is kept in a context file at " <> path <> ".")
      let lastTool = last [m | m <- messagesOf (last posts), get "role" m == Just (PStr "tool")]
      textOf lastTool `shouldSatisfy` T.isInfixOf "[context "
      -- state.db keeps the transcript: the tool result as the tool gave it
      store <- openStore (rGauss r </> "state.db")
      (_, ms) <- snapshotRows store
      closeStore store
      let contents = [c | m <- ms, Just (PStr c) <- [lookup "content" m]]
      contents `shouldSatisfy` elem "{\"content\": \"     1|theorem foo : True := trivial\", \"total_lines\": 1}"
      -- the replay log has CLM's events, and replays bit for bit
      lg <- BS.readFile (rGauss r </> "fisher" </> "replays" </> (sid <> ".jsonl"))
      let kinds = [maybe "" pyStrOf (get "kind" v) | l <- BC.lines lg, Right v <- [loads l]]
      kinds `shouldSatisfy` (\ks -> all (`elem` ks) ["clm", "clm_write", "clm_read"])
      code <- replayMain (rSys r) [T.pack sid]
      code `shouldBe` 0
    it "rejects an unknown strategy" $ withSystemTempDirectory "fisher-clm-cli" $ \tmp -> do
      r <- runChat tmp [] ["-q", "x", "--context", "summarise"]
      rCode r `shouldBe` 2
      rOut r `shouldSatisfy` T.isInfixOf "argument --context: invalid choice: 'summarise' (choose from 'compress', 'clm', 'rlm')"
    it "needs the local terminal backend" $ withSystemTempDirectory "fisher-clm-cli" $ \tmp -> do
      r <- runChat tmp [("TERMINAL_ENV", "docker")] ["-q", "x", "-Q", "--context", "clm"]
      T.unwords (T.words (rOut r)) `shouldSatisfy` T.isInfixOf "Failed to initialize agent: --context clm needs the local terminal backend for now (TERMINAL_ENV is docker)"
  -- CLM with a store (docs/superpowers/specs/2026-10-14-clm-store-design.md
  -- section 4); the scripted chat's which finds no python3
  describe "fisher chat --context clm with fisher.clm.store" $ do
    let storeOn tmp = do
          createDirectoryIfMissing True (tmp </> "home" </> ".gauss")
          BS.writeFile (tmp </> "home" </> ".gauss" </> "config.yaml") "fisher:\n  clm:\n    store: true\n"
    it "without python3 runs with the store and no recall, warns on stderr, saves store.json and replays" $ withSystemTempDirectory "fisher-clm-store" $ \tmp0 -> do
      tmp <- canonicalizePath tmp0
      storeOn tmp
      (r, err) <- capturingStderr tmp (runChat tmp [] ["-q", "Check that the project builds.", "-Q", "--context", "clm"])
      rCode r `shouldBe` 0
      rOut r `shouldNotSatisfy` T.isInfixOf "works without recall"
      T.lines err `shouldSatisfy` (== 1) . length . filter (== "fisher: python3 not found; CLM's store works without recall")
      sid <- onlySession (rGauss r)
      doesFileExist (rGauss r </> "fisher" </> "ctx" </> T.unpack sid </> "store.json") `shouldReturn` True
      lg <- BS.readFile (rGauss r </> "fisher" </> "replays" </> (T.unpack sid <> ".jsonl"))
      let clms = [v | l <- BC.lines lg, Right v <- [loads l], get "kind" v == Just (PStr "clm")]
      [get "config" c >>= get "store" | c <- clms] `shouldBe` [Just (PBool True)]
      [get "recall" c | c <- clms] `shouldBe` [Nothing]
      replayMain (rSys r) [sid] `shouldReturn` 0
    it "a resume over an invalid store.json warns on stderr, not stdout, and carries on" $ withSystemTempDirectory "fisher-clm-store" $ \tmp0 -> do
      tmp <- canonicalizePath tmp0
      storeOn tmp
      r1 <- runChat tmp [] ["-q", "Check that the project builds.", "-Q", "--context", "clm"]
      rCode r1 `shouldBe` 0
      sid <- onlySession (rGauss r1)
      BS.writeFile (rGauss r1 </> "fisher" </> "ctx" </> T.unpack sid </> "store.json") "{\"items\": [oops"
      (r2, err) <- capturingStderr tmp (runChatWith answers tmp [] ["-q", "Check again.", "-Q", "--context", "clm", "--resume", sid])
      rCode r2 `shouldBe` 0
      rOut r2 `shouldNotSatisfy` T.isInfixOf "store.json is not valid"
      T.lines err `shouldSatisfy` (== 1) . length . filter (== "fisher: store.json is not valid; resuming with an empty store")
      rOut r2 `shouldSatisfy` T.isInfixOf "It builds."
  describe "the choice" $ do
    it "the flag wins over the config" $ do
      contextChoice (Just "compress") (PDict [("fisher", PDict [("context", PStr "clm")])]) `shouldBe` ChooseCompress
      contextChoice (Just "clm") (PDict []) `shouldBe` ChooseClm
    it "the config chooses when there is no flag" $ do
      contextChoice Nothing (PDict [("fisher", PDict [("context", PStr "clm")])]) `shouldBe` ChooseClm
      contextChoice Nothing (PDict []) `shouldBe` ChooseCompress
  describe "resuming" $
    it "the history is the pinned prefix, then the live context" $ do
      let u t = PDict [("role", PStr "user"), ("content", PStr t)]
          a t = PDict [("role", PStr "assistant"), ("content", PStr t)]
          restored = [u "task", a "one", u "two", a "three"]
          st = newClmState {stTurns = [Turn 7 (a "summary")]}
      liveHistory restored st `shouldBe` [u "task", a "summary"]
  describe "the stream-json result (Fisher only)" $ do
    let resultOf r = head [v | l <- T.lines (rOut r), "\"result\"" `T.isInfixOf` l, Right v <- [loads (TE.encodeUtf8 l)], get "type" v == Just (PStr "result")]
        argv = ["-p", "Check that the project builds.", "--output-format", "stream-json"]
    it "sums the provider's cost and the cache tokens" $ withSystemTempDirectory "fisher-cost" $ \tmp -> do
      r <- runChatWith (zipWith priced [Just 0.01, Just 0.02] messages0) tmp [] argv
      let v = resultOf r
      case get "total_cost_usd" v of
        Just (PFloat d) -> abs (d - 0.03) `shouldSatisfy` (< 1e-9)
        other -> expectationFailure ("total_cost_usd: " <> show other)
      (get "usage" v >>= get "cache_read_input_tokens") `shouldBe` Just (PInt 120)
      (get "usage" v >>= get "cache_creation_input_tokens") `shouldBe` Just (PInt 60)
    it "is null when a call reported no cost" $ withSystemTempDirectory "fisher-cost" $ \tmp -> do
      r <- runChatWith (zipWith priced [Just 0.01, Nothing] messages0) tmp [] argv
      get "total_cost_usd" (resultOf r) `shouldBe` Just PNull
  describe "a provider's refusal (Fisher only)" $ do
    let resultOf r = head [v | l <- T.lines (rOut r), "\"result\"" `T.isInfixOf` l, Right v <- [loads (TE.encodeUtf8 l)], get "type" v == Just (PStr "result")]
        argv = ["-p", "Check that the project builds.", "--output-format", "stream-json"]
        refused = completion (PDict [("role", PStr "assistant"), ("content", PNull), ("refusal", PStr "I can't help with that.")])
        posts r = length [hr | hr <- rReqs r, hrMethod hr == "POST"]
    it "is retried once, and an answer after it is the answer" $ withSystemTempDirectory "fisher-refusal" $ \tmp -> do
      r <- runChatWith [head answers, refused, answers !! 1] tmp [] argv
      posts r `shouldBe` 3
      get "result" (resultOf r) `shouldBe` Just (PStr "It builds.")
      get "is_error" (resultOf r) `shouldBe` Just (PBool False)
    it "ends the run as an error the second time, not with the last text" $ withSystemTempDirectory "fisher-refusal" $ \tmp -> do
      r <- runChatWith [head answers, refused, refused] tmp [] argv
      posts r `shouldBe` 3
      get "is_error" (resultOf r) `shouldBe` Just (PBool True)
      get "result" (resultOf r) `shouldNotBe` Just (PStr "Reading.")
      get "error" (resultOf r) `shouldBe` Just (PStr "Provider refused the request: I can't help with that.")
  describe "a call written as text under CLM (Fisher only)" $ do
    let textCall = completion (PDict [("role", PStr "assistant"), ("content", PStr "One left. Read it.\n[call read_file {\"path\": \"Math/Foo.lean\"}]")])
        posts r = [hr | hr <- rReqs r, hrMethod hr == "POST"]
    it "is sent back with a note, and the next answer is the answer" $ withSystemTempDirectory "fisher-textcall" $ \tmp0 -> do
      tmp <- canonicalizePath tmp0
      r <- runChatWith [head answers, textCall, answers !! 1] tmp [] ["-q", "Check that the project builds.", "-Q", "--context", "clm"]
      rCode r `shouldBe` 0
      length (posts r) `shouldBe` 3
      let lastMsgs = messagesOf (last (posts r))
      map textOf lastMsgs `shouldSatisfy` any (T.isInfixOf "written as text")
      rOut r `shouldSatisfy` T.isInfixOf "It builds."
      -- the default guidance follows the prompt section
      textOf (head lastMsgs) `shouldSatisfy` T.isInfixOf "How to keep the context file small while you work:"
    it "is the answer under the compressor, as upstream" $ withSystemTempDirectory "fisher-textcall" $ \tmp -> do
      r <- runChatWith [head answers, textCall] tmp [] ["-q", "Check that the project builds.", "-Q"]
      length (posts r) `shouldBe` 2
