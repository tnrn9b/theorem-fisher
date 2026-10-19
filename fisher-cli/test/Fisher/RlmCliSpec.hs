-- | @fisher rlm@ (Fisher only; docs/superpowers/specs/2026-10-13-rlm-design.md
-- section 5): arguments, contexts, settings, output and exit codes, the
-- replay log and replaying from it, the trajectory log, the interpreter
-- check, and the model adapter over Fisher's LLM client. Most cases run on
-- @--script@ or a canned transport with a stand-in REPL, so they need neither
-- the network nor Python; the ones on the real REPL are pending without
-- @python3@.
module Fisher.RlmCliSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (utc)
import Numeric (showHex)
import System.Directory
import System.FilePath
import System.Posix.Files (setFileMode)
import Test.Hspec

import Fisher.Cli.Rlm
import Fisher.LLM.Errors (LLMError (..))
import Fisher.LLM.Http
import Fisher.LLM.Normalize (Usage (..))
import Fisher.LLM.Sys
import Fisher.LLM.Types
import Fisher.Py.Value
import Fisher.RLM.Parse (BlockResult (..))
import Fisher.RLM.Repl (CallKind (..), Callback, ReplFactory, ReplHandle (..), pythonRepl)
import Fisher.RLM.Types (ModelError (..), Reply (..))
import Fisher.TestUtil (withTmp)

-- ------------------------------------------------------------------ helpers

-- | A chat completion holding this reply, with OpenRouter's cost.
completion :: Text -> PyVal
completion t =
  PDict
    [ ("id", PStr "chatcmpl-1"), ("object", PStr "chat.completion"), ("model", PStr "x/answering-model")
    , ("choices", PList [PDict [("index", PInt 0), ("message", PDict [("role", PStr "assistant"), ("content", PStr t)]), ("finish_reason", PStr "stop")]])
    , ("usage", PDict [("prompt_tokens", PInt 100), ("completion_tokens", PInt 10), ("total_tokens", PInt 110), ("cost", PFloat 0.25)]) ]

ok :: PyVal -> HttpResp
ok v = HttpResp 200 [] (TE.encodeUtf8 (dumpsAscii v))

noCredit :: HttpResp
noCredit = HttpResp 402 [] "{\"error\": {\"message\": \"Insufficient credits\", \"code\": 402}}"

fenced :: Text -> Text
fenced code = "```repl\n" <> code <> "\n```"

-- | A REPL that understands one line per block: @ANSWER text@, @PRINT text@,
-- @CTX@ (prints the context as JSON), @ERROR@, @LLM prompt@ and
-- @LLMS a|b|c@. It records every context it is opened on.
stubRepl :: IORef [PyVal] -> ReplFactory
stubRepl opened ctx = do
  modifyIORef opened (<> [ctx])
  pure (Right (ReplHandle (run ctx) (pure ())))
  where
    run :: PyVal -> Text -> Callback -> IO BlockResult
    run c code cb =
      let (kw, rest0) = T.breakOn " " code
          rest = T.drop 1 rest0
          printed t = BlockResult t "" [] [] Nothing
       in case kw of
            "ANSWER" -> pure (BlockResult "" "" [] [] (Just rest))
            "PRINT" -> pure (printed rest)
            "CTX" -> pure (printed (dumpsUnicode c))
            "ERROR" -> pure (BlockResult "" "\nValueError: boom" [] [] Nothing)
            "LLM" -> printed . T.intercalate "\n" <$> cb CallLlm [rest] Nothing
            "LLMS" -> printed . T.intercalate "\n" <$> cb CallLlm (T.splitOn "|" rest) Nothing
            _ -> pure (printed "")

data Repl = Stub | RealPython

data Setup = Setup
  { suAnswers :: [HttpResp]
  -- ^ the canned transport's answers, in order
  , suStdin :: BS.ByteString
  , suRepl :: Repl
  , suEnv :: [(Text, Text)]
  }

setup :: Setup
setup = Setup [] "" Stub []

data Run = Run
  { rCode :: Int
  , rOut :: Text
  , rErr :: Text
  , rReqs :: [HttpReq]
  , rOpened :: [PyVal]
  , rGauss :: FilePath
  }

runRlm :: Setup -> FilePath -> [Text] -> IO Run
runRlm su tmp argv = do
  let home = tmp </> "home"
      gauss = home </> ".gauss"
  createDirectoryIfMissing True gauss
  uuidN <- newIORef (0 :: Int)
  outBuf <- newIORef []
  errBuf <- newIORef []
  reqs <- newIORef []
  queue <- newIORef (suAnswers su)
  opened <- newIORef []
  env <-
    newIORef . M.fromList $
      [("HOME", T.pack home), ("COLUMNS", "60"), ("GAUSS_FORCE_PLAIN_OUTPUT", "1"), ("OPENROUTER_API_KEY", "sk-or-test-1234567890")]
        <> suEnv su
  let sys =
        Sys
          { sysNow = pure 1790000000, sysMonotonic = pure 1000, sysSleep = \_ -> pure ()
          , sysGetEnv = \k -> M.lookup k <$> readIORef env
          , sysSetEnv = \k v -> modifyIORef env (M.insert k v)
          , sysHttp = \_ -> pure (Left (HttpFail "ConnectError" "no network in tests"))
          , sysOpenBrowser = \_ -> pure False, sysRun = \_ -> pure (Left "no"), sysRunInteractive = \_ -> pure (Left "no")
          , sysWhich = \_ -> pure Nothing, sysPlatform = "linux"
          , sysUuidHex = atomicModifyIORef' uuidN (\n -> (n + 1, T.justifyRight 32 '0' (T.pack (showHex (n + 1) ""))))
          , sysTimeZoneAt = \_ -> pure utc, sysHome = home, sysFisherHome = gauss
          , sysOut = \t -> modifyIORef outBuf (<> [t]), sysInput = \_ -> pure Nothing, sysGetPass = \_ -> pure Nothing
          , sysStdinTty = pure False, sysStdoutTty = pure False, sysPid = 4242 }
      respond hr = do
        modifyIORef reqs (<> [hr])
        atomicModifyIORef' queue $ \case
          [] -> ([], Right (HttpResp 500 [] "{\"error\": {\"message\": \"no canned answer\"}}"))
          r : more -> (more, Right r)
      deps =
        defaultRlmDeps
          { rdTransport = pure (Transport (\hr _ -> respond hr) (\_ _ _ -> pure (Left ConnectionError)))
          , rdRuntime = Runtime (pure 0) (\_ -> pure ()) (pure 0)
          , rdRepl = case suRepl su of
              Stub -> \_ _ -> stubRepl opened
              RealPython -> pythonRepl
          , rdStdin = pure (suStdin su)
          , rdErr = \t -> modifyIORef errBuf (<> [t <> "\n"]) }
  code <- rlmMainWith deps sys argv
  Run code <$> (T.concat <$> readIORef outBuf) <*> (T.concat <$> readIORef errBuf) <*> readIORef reqs <*> readIORef opened <*> pure gauss

-- | Write a file under the directory and give its path.
file :: FilePath -> FilePath -> BS.ByteString -> IO Text
file tmp name bytes = do
  let p = tmp </> name
  createDirectoryIfMissing True (takeDirectory p)
  BS.writeFile p bytes
  pure (T.pack p)

-- | A --script file of these entries.
script :: FilePath -> [PyVal] -> IO Text
script tmp entries = file tmp "script.json" (TE.encodeUtf8 (dumpsAscii (PList entries)))

replies :: [Text] -> [PyVal]
replies = map PStr

jsonOut :: Run -> IO PyVal
jsonOut r = either (\e -> fail ("not JSON: " <> e <> ": " <> T.unpack (rOut r))) pure (loads (TE.encodeUtf8 (rOut r)))

at' :: [Text] -> PyVal -> Maybe PyVal
at' ks v = foldl (\acc k -> acc >>= get k) (Just v) ks

-- | The context the run (not the interpreter check) was opened on.
runContext :: Run -> Maybe PyVal
runContext r = case rOpened r of
  [] -> Nothing
  xs -> Just (last xs)

-- | The message contents of a request body, as text.
bodyMessages :: HttpReq -> [Text]
bodyMessages hr = case hrBody hr >>= get "messages" of
  Just (PList ms) -> map textOf ms
  _ -> []
  where
    textOf m = case get "content" m of
      Just (PStr t) -> t
      Just (PList ps) -> T.concat [t | p <- ps, Just (PStr t) <- [get "text" p]]
      _ -> ""

events :: FilePath -> IO [PyVal]
events p = do
  raw <- BS.readFile p
  pure [v | l <- BC.lines raw, not (BS.null l), Right v <- [loads l]]

kindOf :: PyVal -> Text
kindOf v = maybe "" pyStrOf (get "kind" v)

writeEvents :: FilePath -> [PyVal] -> IO ()
writeEvents p vs = BS.writeFile p (TE.encodeUtf8 (T.concat [dumpsAscii v <> "\n" | v <- vs]))

havePython :: IO Bool
havePython = maybe False (const True) <$> findExecutable "python3"

-- ---------------------------------------------------------------------- spec

spec :: Spec
spec = do
  describe "arguments" $ do
    it "an unknown flag exits 2 with the usage line" $ withTmp $ \tmp -> do
      r <- runRlm setup tmp ["--context", "x", "--bogus"]
      rCode r `shouldBe` 2
      rErr r `shouldSatisfy` T.isInfixOf "usage: fisher rlm [--context FILE|-]..."
      rErr r `shouldSatisfy` T.isInfixOf "fisher rlm: error: unrecognized argument: --bogus"
    it "a flag without its value exits 2" $ withTmp $ \tmp -> do
      r <- runRlm setup tmp ["--context"]
      rCode r `shouldBe` 2
      rErr r `shouldSatisfy` T.isInfixOf "fisher rlm: error: argument --context: expected one argument"
    it "a bad number exits 2" $ withTmp $ \tmp -> do
      r <- runRlm setup tmp ["--context", "x", "--max-depth", "-1"]
      rCode r `shouldBe` 2
      rErr r `shouldSatisfy` T.isInfixOf "argument --max-depth: expected a non-negative integer, got '-1'"
      r1 <- runRlm setup tmp ["--context", "x", "--max-iterations", "0"]
      rCode r1 `shouldBe` 2
      rErr r1 `shouldSatisfy` T.isInfixOf "argument --max-iterations: expected a positive integer, got '0'"
      r2 <- runRlm setup tmp ["--context", "x", "--max-budget", "lots"]
      rCode r2 `shouldBe` 2
      rErr r2 `shouldSatisfy` T.isInfixOf "argument --max-budget: expected a positive number, got 'lots'"
    it "needs a context" $ withTmp $ \tmp -> do
      r <- runRlm setup tmp ["--query", "q"]
      rCode r `shouldBe` 2
      rErr r `shouldSatisfy` T.isInfixOf "fisher rlm: error: at least one --context is required"
    it "--script and --replay do not go together" $ withTmp $ \tmp -> do
      r <- runRlm setup tmp ["--context", "x", "--script", "s", "--replay", "l"]
      rCode r `shouldBe` 2
      rErr r `shouldSatisfy` T.isInfixOf "--script and --replay cannot be used together"
    it "--help says the REPL is not a sandbox" $ withTmp $ \tmp -> do
      r <- runRlm setup tmp ["--help"]
      rCode r `shouldBe` 0
      T.unwords (T.words (rOut r)) `shouldSatisfy` T.isInfixOf "Code the model writes runs as you, in a temporary directory, with your files and network. It is not given your API key."
      rOut r `shouldSatisfy` T.isInfixOf "--record-prompts FILE"

  describe "contexts" $ do
    it "one file is a str, two are a list, a .json file is JSON" $ withTmp $ \tmp -> do
      a <- file tmp "a.txt" "alpha"
      b <- file tmp "b.txt" "beta"
      j <- file tmp "c.json" "{\"k\": [1, 2]}"
      s <- script tmp (replies [fenced "ANSWER ok", fenced "ANSWER ok", fenced "ANSWER ok", fenced "ANSWER ok"])
      r1 <- runRlm setup tmp ["--script", s, "--context", a]
      rCode r1 `shouldBe` 0
      runContext r1 `shouldBe` Just (PStr "alpha")
      r2 <- runRlm setup tmp ["--script", s, "--context", a, "--context", b]
      runContext r2 `shouldBe` Just (PList [PStr "alpha", PStr "beta"])
      r3 <- runRlm setup tmp ["--script", s, "--context", j]
      runContext r3 `shouldBe` Just (PDict [("k", PList [PInt 1, PInt 2])])
      r4 <- runRlm setup tmp ["--script", s, "--context", b, "--context", j]
      runContext r4 `shouldBe` Just (PList [PStr "beta", PDict [("k", PList [PInt 1, PInt 2])]])
    it "- reads stdin, once" $ withTmp $ \tmp -> do
      s <- script tmp (replies [fenced "ANSWER ok"])
      r <- runRlm setup {suStdin = "from stdin"} tmp ["--script", s, "--context", "-"]
      rCode r `shouldBe` 0
      runContext r `shouldBe` Just (PStr "from stdin")
      r2 <- runRlm setup tmp ["--script", s, "--context", "-", "--context", "-"]
      rCode r2 `shouldBe` 2
      rErr r2 `shouldSatisfy` T.isInfixOf "stdin can be read only once"
    it "invalid UTF-8 is read with replacement characters and a note on stderr; NUL bytes stay" $ withTmp $ \tmp -> do
      bad <- file tmp "bad.txt" "ab\xff\xfe"
      nul <- file tmp "nul.txt" "a\NULb"
      s <- script tmp (replies [fenced "ANSWER ok", fenced "ANSWER ok"])
      r <- runRlm setup tmp ["--script", s, "--context", bad]
      rCode r `shouldBe` 0
      runContext r `shouldBe` Just (PStr "ab\xfffd\xfffd")
      rErr r `shouldSatisfy` T.isInfixOf ("fisher rlm: " <> bad <> ": not valid UTF-8; invalid bytes were replaced")
      r2 <- runRlm setup tmp ["--script", s, "--context", nul]
      runContext r2 `shouldBe` Just (PStr "a\NULb")
      rErr r2 `shouldSatisfy` (not . T.isInfixOf "not valid UTF-8")
    it "a missing file or bad JSON exits 1 before any model call" $ withTmp $ \tmp -> do
      j <- file tmp "bad.json" "{nope"
      s <- script tmp (replies [fenced "ANSWER ok"])
      r <- runRlm setup tmp ["--script", s, "--context", T.pack (tmp </> "missing.txt")]
      rCode r `shouldBe` 1
      rErr r `shouldSatisfy` T.isInfixOf "missing.txt"
      r2 <- runRlm setup tmp ["--script", s, "--context", j]
      rCode r2 `shouldBe` 1
      rErr r2 `shouldSatisfy` T.isInfixOf "bad.json: not valid JSON"
      rOpened r2 `shouldBe` []

  describe "output and exit codes" $ do
    it "--json has answer, ending, iterations, usage and log" $ withTmp $ \tmp -> do
      a <- file tmp "a.txt" "alpha"
      s <- script tmp (replies [fenced "PRINT looking", fenced "ANSWER 42"])
      r <- runRlm setup tmp ["--script", s, "--context", a, "--json"]
      rCode r `shouldBe` 0
      v <- jsonOut r
      get "answer" v `shouldBe` Just (PStr "42")
      get "ending" v `shouldBe` Just (PStr "answered")
      get "reason" v `shouldBe` Just PNull
      get "iterations" v `shouldBe` Just (PInt 2)
      at' ["usage", "total_cost_usd"] v `shouldBe` Just (PFloat 0)
      at' ["usage", "models", "scripted", "calls"] v `shouldBe` Just (PInt 2)
      get "log" v `shouldBe` Just PNull
      get "replay_log" v `shouldBe` Just PNull
    it "plain output is the answer and a newline" $ withTmp $ \tmp -> do
      a <- file tmp "a.txt" "alpha"
      s <- script tmp (replies [fenced "ANSWER forty two"])
      r <- runRlm setup tmp ["--script", s, "--context", a]
      rOut r `shouldBe` "forty two\n"
    it "the default answer exits 0" $ withTmp $ \tmp -> do
      a <- file tmp "a.txt" "alpha"
      s <- script tmp (replies ["thinking", "the default"])
      r <- runRlm setup tmp ["--script", s, "--context", a, "--max-iterations", "1", "--json"]
      rCode r `shouldBe` 0
      v <- jsonOut r
      get "ending" v `shouldBe` Just (PStr "default_answer")
      get "answer" v `shouldBe` Just (PStr "the default")
    it "a stop exits 1 with its reason" $ withTmp $ \tmp -> do
      a <- file tmp "a.txt" "alpha"
      s <- script tmp (replies [fenced "PRINT one"])
      r <- runRlm setup tmp ["--script", s, "--context", a, "--json"]
      rCode r `shouldBe` 1
      v <- jsonOut r
      get "ending" v `shouldBe` Just (PStr "stopped")
      get "reason" v `shouldBe` Just (PStr "model error: script: out of replies")
      rErr r `shouldSatisfy` T.isInfixOf "fisher rlm: stopped: model error: script: out of replies"
    it "no credit exits 3" $ withTmp $ \tmp -> do
      a <- file tmp "a.txt" "alpha"
      r <- runRlm setup {suAnswers = [noCredit]} tmp ["--context", a, "--json"]
      rCode r `shouldBe` 3
      v <- jsonOut r
      get "reason" v `shouldBe` Just (PStr "no credit")
      length (rReqs r) `shouldBe` 1

  describe "settings" $ do
    it "come from fisher.rlm in config.yaml, and flags win" $ withTmp $ \tmp -> do
      a <- file tmp "a.txt" "alpha"
      _ <- file tmp "home/.gauss/config.yaml" "fisher:\n  rlm:\n    model: x/from-config\n    max_iterations: 1\n"
      r <- runRlm setup {suAnswers = [ok (completion "talk"), ok (completion "fallback")]} tmp ["--context", a, "--json"]
      rCode r `shouldBe` 0
      [hrBody h >>= get "model" | h <- rReqs r] `shouldBe` replicate 2 (Just (PStr "x/from-config"))
      v <- jsonOut r
      get "ending" v `shouldBe` Just (PStr "default_answer")
      at' ["usage", "models", "x/from-config", "calls"] v `shouldBe` Just (PInt 2)
      at' ["usage", "total_cost_usd"] v `shouldBe` Just (PFloat 0.5)
      r2 <- runRlm setup {suAnswers = map (ok . completion) ["talk", "talk", "fallback"]} tmp ["--context", a, "--model", "x/from-flag", "--max-iterations", "2", "--json"]
      [hrBody h >>= get "model" | h <- rReqs r2] `shouldBe` replicate 3 (Just (PStr "x/from-flag"))
      v2 <- jsonOut r2
      get "iterations" v2 `shouldBe` Just (PInt 2)
    it "--sub-model sends sub-calls to that model; without it they go to the root model" $ withTmp $ \tmp -> do
      a <- file tmp "a.txt" "alpha"
      let answers = map (ok . completion) [fenced "LLM hi", "sub says", fenced "ANSWER done"]
      r <- runRlm setup {suAnswers = answers} tmp ["--context", a, "--model", "x/root", "--sub-model", "x/sub"]
      rCode r `shouldBe` 0
      [hrBody h >>= get "model" | h <- rReqs r] `shouldBe` map (Just . PStr) ["x/root", "x/sub", "x/root"]
      r2 <- runRlm setup {suAnswers = answers} tmp ["--context", a, "--model", "x/root"]
      [hrBody h >>= get "model" | h <- rReqs r2] `shouldBe` map (Just . PStr) ["x/root", "x/root", "x/root"]
    it "a config.yaml that cannot be read warns on stderr and leaves --json clean" $ withTmp $ \tmp -> do
      a <- file tmp "a.txt" "alpha"
      _ <- file tmp "home/.gauss/config.yaml" "fisher: [unclosed\n  rlm: {\n"
      s <- script tmp (replies [fenced "ANSWER ok"])
      r <- runRlm setup tmp ["--script", s, "--context", a, "--json"]
      rCode r `shouldBe` 0
      v <- jsonOut r
      get "answer" v `shouldBe` Just (PStr "ok")
      rErr r `shouldSatisfy` T.isInfixOf "Failed to load config"
    it "in live mode too, a config.yaml that cannot be read leaves --json clean" $ withTmp $ \tmp -> do
      a <- file tmp "a.txt" "alpha"
      _ <- file tmp "home/.gauss/config.yaml" "fisher: [unclosed\n  rlm: {\n"
      r <- runRlm setup {suAnswers = [ok (completion (fenced "ANSWER ok"))]} tmp ["--context", a, "--json"]
      rCode r `shouldBe` 0
      length (rReqs r) `shouldBe` 1
      v <- jsonOut r
      get "answer" v `shouldBe` Just (PStr "ok")
      T.count "Failed to load config" (rErr r) `shouldBe` 1
    it "a bad fisher.rlm section exits 2 with its text" $ withTmp $ \tmp -> do
      a <- file tmp "a.txt" "alpha"
      _ <- file tmp "home/.gauss/config.yaml" "fisher:\n  rlm:\n    max_depth: 0\n"
      r <- runRlm setup tmp ["--context", a]
      rCode r `shouldBe` 2
      rErr r `shouldSatisfy` T.isInfixOf "fisher.rlm: max_depth must be a positive integer"

  describe "scripts and recorded prompts" $ do
    it "--max-depth 0 makes one model call whose single user message is the context file's text" $ withTmp $ \tmp -> do
      a <- file tmp "plain.txt" "the whole context\n\nQuestion: which?"
      s <- script tmp [PStr "1234567"]
      let rec = tmp </> "prompts.json"
      r <- runRlm setup tmp ["--script", s, "--context", a, "--max-depth", "0", "--json", "--record-prompts", T.pack rec]
      rCode r `shouldBe` 0
      v <- jsonOut r
      get "answer" v `shouldBe` Just (PStr "1234567")
      get "ending" v `shouldBe` Just (PStr "answered")
      calls <- either fail pure . loads =<< BS.readFile rec
      calls
        `shouldBe` PList
          [ PDict
              [ ("role", PStr "root"), ("model", PStr "anthropic/claude-sonnet-5.5")
              , ("messages", PList [PDict [("role", PStr "user"), ("content", PStr "the whole context\n\nQuestion: which?")]]) ] ]
    it "{\"sub\": P} answers sub-calls with P and the prompt's first 20 characters; --record-prompts lists every call" $ withTmp $ \tmp -> do
      a <- file tmp "a.txt" "alpha"
      s <- script tmp [PStr (fenced "LLM a prompt longer than twenty characters"), PDict [("sub", PStr "SUB:")], PStr (fenced "ANSWER done")]
      let rec = tmp </> "prompts.json"
      r <- runRlm setup tmp ["--script", s, "--context", a, "--model", "x/root", "--record-prompts", T.pack rec]
      rCode r `shouldBe` 0
      v <- either fail pure . loads =<< BS.readFile rec
      case v of
        PList calls -> do
          map (get "role") calls `shouldBe` map (Just . PStr) ["root", "sub", "root"]
          get "messages" (calls !! 1) `shouldBe` Just (PList [PDict [("role", PStr "user"), ("content", PStr "a prompt longer than twenty characters")]])
          map (get "model") calls `shouldBe` map (Just . PStr) ["x/root", "x/root", "x/root"]
          let lastRoot = case get "messages" (calls !! 2) of Just (PList ms) -> [c | m <- ms, Just (PStr c) <- [get "content" m]]; _ -> []
          T.concat lastRoot `shouldSatisfy` T.isInfixOf "SUB:a prompt longer than"
        _ -> expectationFailure "not a list"
      -- a sub-model, when set, is what sub-calls record
      r2 <- runRlm setup tmp ["--script", s, "--context", a, "--model", "x/root", "--sub-model", "x/sub", "--record-prompts", T.pack rec]
      rCode r2 `shouldBe` 0
      v2 <- either fail pure . loads =<< BS.readFile rec
      case v2 of
        PList calls -> map (get "model") calls `shouldBe` map (Just . PStr) ["x/root", "x/sub", "x/root"]
        _ -> expectationFailure "not a list"
    it "a script with two sub rules is refused" $ withTmp $ \tmp -> do
      a <- file tmp "a.txt" "alpha"
      s <- script tmp [PDict [("sub", PStr "A:")], PStr (fenced "ANSWER x"), PDict [("sub", PStr "B:")]]
      r <- runRlm setup tmp ["--script", s, "--context", a]
      rCode r `shouldBe` 1
      rErr r `shouldSatisfy` T.isInfixOf "script: more than one sub rule"
      rOut r `shouldBe` ""

  describe "the replay log" $ do
    it "holds every model call under $GAUSS_HOME/fisher/replays/" $ withTmp $ \tmp -> do
      a <- file tmp "a.txt" "alpha"
      let answers = map (ok . completion) [fenced "LLMS one|two|three", "s1", "s2", "s3", fenced "ANSWER done"]
      r <- runRlm setup {suAnswers = answers} tmp ["--context", a, "--json"]
      rCode r `shouldBe` 0
      v <- jsonOut r
      lg <- case get "replay_log" v of
        Just (PStr p) -> pure (T.unpack p)
        _ -> fail "no replay_log"
      takeDirectory lg `shouldBe` rGauss r </> "fisher" </> "replays"
      takeFileName lg `shouldSatisfy` ("rlm_" `isPrefixOf`)
      evs <- events lg
      map kindOf evs `shouldBe` ["rlm", "http", "http", "http", "http", "http"]
      get "argv" (head evs) `shouldBe` Just (PList (map PStr ["--context", a, "--json"]))
    it "--replay answers every call from the log by request body, with no network, and the same --json" $ withTmp $ \tmp -> do
      a <- file tmp "a.txt" "alpha"
      let answers = map (ok . completion) [fenced "LLMS one|two|three", "s1", "s2", "s3", fenced "ANSWER done"]
      r1 <- runRlm setup {suAnswers = answers} tmp ["--context", a, "--json"]
      rCode r1 `shouldBe` 0
      v1 <- jsonOut r1
      lg <- case get "replay_log" v1 of
        Just (PStr p) -> pure (T.unpack p)
        _ -> fail "no replay_log"
      -- every exchange in reverse order: matching is by body, not position
      evs <- events lg
      let lg2 = tmp </> "reversed.jsonl"
      writeEvents lg2 (head evs : reverse (tail evs))
      r2 <- runRlm setup tmp ["--context", a, "--json", "--replay", T.pack lg]
      rCode r2 `shouldBe` 0
      rReqs r2 `shouldBe` []
      rOut r2 `shouldBe` rOut r1
      r3 <- runRlm setup tmp ["--context", a, "--json", "--replay", T.pack lg2]
      rCode r3 `shouldBe` 0
      rReqs r3 `shouldBe` []
      v3 <- jsonOut r3
      delete "replay_log" v3 `shouldBe` delete "replay_log" v1
    it "a request not in the log exits 1 naming it" $ withTmp $ \tmp -> do
      a <- file tmp "a.txt" "alpha"
      let answers = map (ok . completion) [fenced "LLM hi", "sub says", fenced "ANSWER done"]
      r1 <- runRlm setup {suAnswers = answers} tmp ["--context", a, "--json"]
      v1 <- jsonOut r1
      lg <- case get "replay_log" v1 of
        Just (PStr p) -> pure (T.unpack p)
        _ -> fail "no replay_log"
      evs <- events lg
      -- drop the sub-call's exchange
      let lg2 = tmp </> "short.jsonl"
      writeEvents lg2 [e | (i, e) <- zip [0 :: Int ..] evs, i /= 2]
      r2 <- runRlm setup tmp ["--context", a, "--replay", T.pack lg2]
      rCode r2 `shouldBe` 1
      rReqs r2 `shouldBe` []
      rErr r2 `shouldSatisfy` T.isInfixOf "replay: no logged response for this request"
      rErr r2 `shouldSatisfy` T.isInfixOf "\"hi\""

  describe "the trajectory log" $
    it "--log-dir D writes one trajectory file" $ withTmp $ \tmp -> do
      a <- file tmp "a.txt" "alpha"
      s <- script tmp (replies [fenced "PRINT one", fenced "ANSWER done"])
      let dir = tmp </> "logs"
      r <- runRlm setup tmp ["--script", s, "--context", a, "--log-dir", T.pack dir, "--json"]
      rCode r `shouldBe` 0
      fs <- listDirectory dir
      length fs `shouldBe` 1
      head fs `shouldSatisfy` (".jsonl" `isSuffixOf`)
      v <- jsonOut r
      get "log" v `shouldBe` Just (PStr (T.pack (dir </> head fs)))
      ls <- BC.lines <$> BS.readFile (dir </> head fs)
      length ls `shouldBe` 3

  describe "the interpreter" $ do
    it "no python3 exits 1 before any model call, saying so" $ withTmp $ \tmp -> do
      a <- file tmp "a.txt" "alpha"
      r <- runRlm setup {suRepl = RealPython, suAnswers = [ok (completion (fenced "ANSWER x"))]} tmp ["--context", a, "--python", T.pack (tmp </> "no-such-python")]
      rCode r `shouldBe` 1
      rErr r `shouldSatisfy` T.isInfixOf "python3 not found"
      rReqs r `shouldBe` []
    it "a python3 older than 3.9 exits 1 before any model call, saying so" $ withTmp $ \tmp -> do
      a <- file tmp "a.txt" "alpha"
      let fake = tmp </> "old-python"
      writeFile fake "#!/bin/sh\ncase \"$2\" in\n  -c) echo False ;;\n  --version) echo 'Python 3.8.1' ;;\nesac\n"
      setFileMode fake 0o755
      r <- runRlm setup {suRepl = RealPython, suAnswers = [ok (completion (fenced "ANSWER x"))]} tmp ["--context", a, "--python", T.pack fake]
      rCode r `shouldBe` 1
      rErr r `shouldSatisfy` T.isInfixOf "older than 3.9: Python 3.8.1"
      rReqs r `shouldBe` []
    it "runs the model's code in the real REPL" $ withTmp $ \tmp -> do
      py <- havePython
      if not py
        then pendingWith "python3 is not installed"
        else do
          a <- file tmp "a.txt" "abc"
          s <- script tmp (replies [fenced "answer['content'] = context.upper()\nanswer['ready'] = True"])
          r <- runRlm setup {suRepl = RealPython} tmp ["--script", s, "--context", a]
          rCode r `shouldBe` 0
          rOut r `shouldBe` "ABC\n"
          -- a NUL byte and a replaced byte reach Python as characters
          oddFile <- file tmp "odd.txt" "a\NULb\xff"
          s2 <- script tmp (replies [fenced "answer['content'] = str([ord(c) for c in context])\nanswer['ready'] = True"])
          r2 <- runRlm setup {suRepl = RealPython} tmp ["--script", s2, "--context", oddFile]
          rCode r2 `shouldBe` 0
          rOut r2 `shouldBe` "[97, 0, 98, 65533]\n"

  describe "the model adapter" $ do
    let base = defaultSettings {sModel = "x/default", sApiKey = "sk-test-1234567890", sSessionId = "fisher-rlm-test"}
        quiet = Runtime (pure 0) (\_ -> pure ()) (pure 0)
        env = Env (pure (T.replicate 32 "0")) (\_ _ -> pure "")
        canned resp = do
          seen <- newIORef []
          let tr = Transport (\hr _ -> modifyIORef seen (<> [hr]) >> pure resp) (\_ _ _ -> pure (Left ConnectionError))
          pure (tr, readIORef seen)
        call resp override = do
          (tr, seen) <- canned resp
          r <- rlmModel quiet tr env base override [("system", "be brief"), ("user", "hello")]
          (,) r <$> seen
    it "sends the override or the default model, without max_tokens" $ do
      (r, sent) <- call (Right (ok (completion "hi"))) Nothing
      r `shouldBe` Right (Reply "hi" (Usage 100 10 110 Nothing Nothing (Just 0.25)) "x/default")
      [hrBody h >>= get "model" | h <- sent] `shouldBe` [Just (PStr "x/default")]
      [maybe False (has "max_tokens") (hrBody h) | h <- sent] `shouldBe` [False]
      map bodyMessages sent `shouldBe` [["be brief", "hello"]]
      (r2, sent2) <- call (Right (ok (completion "hi"))) (Just "x/other")
      fmap rpModel r2 `shouldBe` Right "x/other"
      [hrBody h >>= get "model" | h <- sent2] `shouldBe` [Just (PStr "x/other")]
    it "maps 402 to NoCredit, a refusal to Refused, no content to Empty" $ do
      (r1, _) <- call (Right noCredit) Nothing
      r1 `shouldBe` Left NoCredit
      let refusal =
            PDict
              [ ("choices", PList [PDict [("index", PInt 0), ("message", PDict [("role", PStr "assistant"), ("content", PNull), ("refusal", PStr "I cannot help with that.")]), ("finish_reason", PStr "stop")]]) ]
      (r2, _) <- call (Right (ok refusal)) Nothing
      r2 `shouldBe` Left (Refused "I cannot help with that.")
      (r3, _) <- call (Right (ok (completion ""))) Nothing
      r3 `shouldBe` Left Empty
      (r4, _) <- call (Right (ok (PDict [("choices", PList [])]))) Nothing
      r4 `shouldBe` Left Empty
    it "maps a content_filter finish with no content to Refused, naming the native reason" $ do
      let filtered native =
            PDict
              [ ( "choices"
                , PList [PDict ([("index", PInt 0), ("message", PDict [("role", PStr "assistant"), ("content", PNull), ("refusal", PNull)]), ("finish_reason", PStr "content_filter")] <> [("native_finish_reason", PStr n) | n <- native])] ) ]
      (r1, _) <- call (Right (ok (filtered ["refusal"]))) Nothing
      r1 `shouldBe` Left (Refused "content filter (refusal)")
      (r2, _) <- call (Right (ok (filtered []))) Nothing
      r2 `shouldBe` Left (Refused "content filter")
    it "gives other errors their class and text" $ do
      (r, _) <- call (Right (HttpResp 400 [] "{\"error\": {\"message\": \"bad model\"}}")) Nothing
      case r of
        Left (OtherError t) -> t `shouldSatisfy` T.isPrefixOf "BadRequestError: "
        other -> expectationFailure (show other)
