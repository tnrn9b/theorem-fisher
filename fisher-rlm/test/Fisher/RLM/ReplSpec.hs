module Fisher.RLM.ReplSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.MVar
import Control.Exception (bracket, finally)
import Control.Monad (forever, void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Fisher.Py.Value (PyVal (..))
import Fisher.RLM.Parse (BlockResult (..))
import Fisher.RLM.Repl
import System.Directory (doesDirectoryExist, findExecutable, getPermissions, setPermissions, setOwnerExecutable)
import System.Environment (setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)
import Test.Hspec

noCallback :: Callback
noCallback _ _ _ = pure []

-- | Run a test with a live REPL holding the context "hello"; skipped when
-- python3 is not on the PATH.
withRepl :: Int -> (ReplHandle -> IO ()) -> Expectation
withRepl secs act = do
  found <- findExecutable "python3"
  case found of
    Nothing -> pendingWith "python3 not found"
    Just _ ->
      bracket
        (pythonRepl "python3" secs (PStr "hello") >>= either (\e -> expectationFailure (T.unpack e) >> fail "no repl") pure)
        rhClose
        act

restarted :: Text -> Text
restarted why = why <> "; the REPL was restarted and its variables were lost"

spec :: Spec
spec = describe "Fisher.RLM.Repl" $ do
  it "round-trips a block and keeps variables between blocks" $
    withRepl 30 $ \h -> do
      a <- rhExec h "x = 41" noCallback
      brStdout a `shouldBe` ""
      b <- rhExec h "x += 1\nprint(x, context)" noCallback
      brStdout b `shouldBe` "42 hello\n"
      brVars b `shouldBe` ["answer", "context_0", "context", "x"]
      brTypes b `shouldBe` ["_AnswerDict", "str", "str", "int"]
      brFinal b `shouldBe` Nothing
      c <- rhExec h "answer['content'] = 'done'\nanswer['ready'] = True" noCallback
      brFinal c `shouldBe` Just "done"

  it "answers a batched call and passes kind and model" $
    withRepl 30 $ \h -> do
      seen <- newIORef []
      let cb k ps m = modifyIORef seen ((k, ps, m) :) >> pure ["r1", "r2"]
      r <- rhExec h "print(llm_query_batched(['a', 'b']))\nprint(rlm_query_batched(['c', 'd'], model='m'))" cb
      brStdout r `shouldBe` "['r1', 'r2']\n['r1', 'r2']\n"
      calls <- reverse <$> readIORef seen
      length calls `shouldBe` 2
      calls `shouldBe` [(CallLlm, ["a", "b"], Nothing), (CallRlm, ["c", "d"], Just "m")]

  it "delivers a result with newlines, braces and non-ASCII text intact" $
    withRepl 30 $ \h -> do
      let big = "line1\n{\"a\": [1, 2]}\n}{ \"quoted\" \\ end \233 \8594 \128512"
          cb _ _ _ = pure [big]
      r <- rhExec h "r = llm_query('p')\nprint(len(r), r.count(chr(10)), r.count('{'), ord(r[-1]))" cb
      brStdout r `shouldBe` T.pack (show (T.length big) <> " 2 2 128512\n")

  it "survives a very long context and long output" $ do
    found <- findExecutable "python3"
    case found of
      Nothing -> pendingWith "python3 not found"
      Just _ -> do
        let ctx = PStr (T.replicate 2000000 "ab\n")
        bracket
          (pythonRepl "python3" 60 ctx >>= either (fail . T.unpack) pure)
          rhClose
          $ \h -> do
            r <- rhExec h "print(len(context))\nprint('z' * 1000000)" noCallback
            T.take 8 (brStdout r) `shouldBe` "6000000\n"
            T.length (brStdout r) `shouldBe` 8 + 1000001

  it "times out a busy loop, restarts, and keeps only the context" $
    withRepl 2 $ \h -> do
      _ <- rhExec h "x = 1" noCallback
      r <- rhExec h "while True: pass" noCallback
      r
        `shouldBe` BlockResult
          { brStdout = ""
          , brStderr = "REPL timed out after 2 s; the REPL was restarted and its variables were lost"
          , brVars = []
          , brTypes = []
          , brFinal = Nothing
          }
      n <- rhExec h "print(context)" noCallback
      brStdout n `shouldBe` "hello\n"
      brVars n `shouldBe` ["answer", "context_0", "context"]

  it "does not count the time spent serving a callback against the block" $
    withRepl 2 $ \h -> do
      let slow _ ps _ = threadDelay 3000000 >> pure (map ("r:" <>) ps)
      r <- rhExec h "x = llm_query('a')\ny = llm_query_batched(['b', 'c'])\nprint(x, y)" slow
      r `shouldBe` BlockResult "r:a ['r:b', 'r:c']\n" "" ["answer", "context_0", "context", "x", "y"] ["_AnswerDict", "str", "str", "str", "list"] Nothing

  it "still times out Python that runs past the timeout after a callback" $
    withRepl 2 $ \h -> do
      let fast _ ps _ = pure ps
      r <- rhExec h "llm_query('a')\nwhile True: pass" fast
      brStderr r `shouldBe` restarted "REPL timed out after 2 s"
      n <- rhExec h "print(context)" noCallback
      brStdout n `shouldBe` "hello\n"

  it "restarts when the script exits during a block" $
    withRepl 30 $ \h -> do
      _ <- rhExec h "x = 1" noCallback
      r <- rhExec h "import os\nos._exit(3)" noCallback
      r
        `shouldBe` BlockResult
          { brStdout = ""
          , brStderr = restarted "REPL exited with code 3"
          , brVars = []
          , brTypes = []
          , brFinal = Nothing
          }
      n <- rhExec h "print(context)" noCallback
      brStdout n `shouldBe` "hello\n"
      brVars n `shouldBe` ["answer", "context_0", "context"]

  it "never gives the script an API key or token" $ do
    let names =
          [ "FISHER_TEST_API_KEY"
          , "OPENROUTER_FOO"
          , "ANTHROPIC_FOO"
          , "OPENAI_FOO"
          , "GITHUB_TOKEN"
          ]
    mapM_ (\n -> setEnv n "secret") names
    setEnv "FISHER_TEST_KEEP" "kept"
    (`finally` mapM_ unsetEnv ("FISHER_TEST_KEEP" : names)) $
      withRepl 30 $ \h -> do
        r <-
          rhExec
            h
            "import os\nprint(sorted(k for k in os.environ if k.startswith('FISHER_TEST') or k.endswith('_TOKEN') or k.split('_')[0] in ('OPENROUTER', 'ANTHROPIC', 'OPENAI')))\nprint(os.environ['PYTHONUTF8'], os.environ['PYTHONIOENCODING'])"
            noCallback
        brStdout r `shouldBe` "['FISHER_TEST_KEEP']\n1 utf-8\n"

  it "close leaves no child process, removes the directory, and is idempotent" $
    withRepl 30 $ \h -> do
      r <- rhExec h "import os\nprint(os.getpid())\nprint(os.environ['TMPDIR'])" noCallback
      (pid, dir) <- case T.lines (brStdout r) of
        [a, b] -> pure (a, b)
        other -> fail ("unexpected output " <> show other)
      doesDirectoryExist (T.unpack dir) `shouldReturn` True
      rhClose h
      rhClose h
      (code, _, _) <- readProcessWithExitCode "kill" ["-0", T.unpack pid] ""
      code `shouldNotBe` ExitSuccess
      doesDirectoryExist (T.unpack dir) `shouldReturn` False

  it "close while a block is running ends the block without a restart" $
    withRepl 60 $ \h -> do
      running <- async (rhExec h "while True: pass" noCallback)
      threadDelay 500000
      void (forkIO (rhClose h))
      r <- timeout 15000000 (wait running)
      fmap (T.isPrefixOf "REPL closed" . brStderr) r `shouldBe` Just True
      r2 <- rhExec h "pass" noCallback
      T.isPrefixOf "REPL closed" (brStderr r2) `shouldBe` True

  it "answers a callback that returns the wrong number of results with errors" $
    withRepl 30 $ \h -> do
      r <- rhExec h "print(llm_query_batched(['a', 'b']))" (\_ _ _ -> pure ["only"])
      brStdout r `shouldBe` "['Error: callback returned 1 results for 2 prompts', 'Error: callback returned 1 results for 2 prompts']\n"

  it "runs in UTF-8 mode" $ do
    found <- findExecutable "python3"
    case found of
      Nothing -> pendingWith "python3 not found"
      Just _ -> bracket
        (pythonRepl "python3" 30 (PStr "\233") >>= either (fail . T.unpack) pure)
        rhClose
        $ \h -> do
          r <- rhExec h "print(ord(context[0]))" noCallback
          brStdout r `shouldBe` "233\n"

  it "starts a fresh script after an outside interruption of a block" $
    withRepl 60 $ \h -> do
      cancelled <- newEmptyMVar
      let cb _ _ _ = forever (threadDelay 1000000) `finally` putMVar cancelled ()
      r <- timeout 1000000 (rhExec h "x = 1\nllm_query('a')" cb)
      r `shouldBe` Nothing
      done <- timeout 5000000 (takeMVar cancelled)
      done `shouldBe` Just ()
      n <- rhExec h "print(context)" noCallback
      n `shouldBe` BlockResult "hello\n" "" ["answer", "context_0", "context"] ["_AnswerDict", "str", "str"] Nothing

  describe "a script that ignores SIGTERM" $ do
    let stubborn = "import signal\nsignal.signal(signal.SIGTERM, signal.SIG_IGN)\nwhile True: pass"
    it "is still killed on timeout" $
      withRepl 2 $ \h -> do
        r <- timeout 20000000 (rhExec h stubborn noCallback)
        fmap brStderr r `shouldBe` Just (restarted "REPL timed out after 2 s")
    it "is still killed by close" $
      withRepl 60 $ \h -> do
        running <- async (rhExec h stubborn noCallback)
        threadDelay 1000000
        done <- timeout 20000000 (rhClose h >> wait running)
        fmap (T.isPrefixOf "REPL closed" . brStderr) done `shouldBe` Just True

  describe "the interpreter check" $ do
    it "runs the version check isolated and without credentials" $
      withSystemTempDirectory "fisher-rlm-test" $ \d -> do
        let p = d <> "/fakepython"
            rec = d <> "/seen"
        writeFile p ("#!/bin/sh\necho \"$*\" > " <> rec <> "\nenv >> " <> rec <> "\necho True\n")
        perms <- getPermissions p
        setPermissions p (setOwnerExecutable True perms)
        setEnv "OPENAI_FOO" "secret"
        _ <- pythonRepl p 1 (PStr "c") `finally` unsetEnv "OPENAI_FOO"
        seen <- BC.unpack <$> BS.readFile rec
        take 2 seen `shouldBe` "-I"
        seen `shouldNotContain` "OPENAI_FOO"

    it "reports a missing executable" $
      do
        r <- pythonRepl "no-such-python-xyz" 5 (PStr "c")
        leftOnly r `shouldBe` Left "python3 not found: no-such-python-xyz"

    it "reports an interpreter older than 3.9" $
      withSystemTempDirectory "fisher-rlm-test" $ \d -> do
        let p = d <> "/fakepython"
        writeFile p "#!/bin/sh\ncase \"$*\" in *--version*) echo \"Python 2.7.18\";; *) echo False;; esac\n"
        perms <- getPermissions p
        setPermissions p (setOwnerExecutable True perms)
        r <- pythonRepl p 5 (PStr "c")
        leftOnly r `shouldBe` Left "python3 is older than 3.9: Python 2.7.18"
  where
    leftOnly :: Either Text ReplHandle -> Either Text ()
    leftOnly = fmap (const ())
