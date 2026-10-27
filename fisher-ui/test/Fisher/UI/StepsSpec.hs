-- | The proof canvas through the whole server: a session's steps from its
-- root, a step's text, its verdict computed once in a copy of the project
-- (a fake lake for Lean, a fake fisher check for Rocq) and then cached,
-- and a hand edit handed back through the inbox (a live run) or by
-- resuming the session (no live run, a fake fisher chat).
module Fisher.UI.StepsSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (bracket)
import Control.Monad (forM, forM_)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Database.SQLite.Simple
import Network.HTTP.Types
import Network.Wai (Application, Request (..))
import Network.Wai.Test
import Data.Time.Clock (addUTCTime)
import System.Directory
import System.FilePath (takeDirectory, (</>))
import System.IO (hGetLine)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (..), StdStream (..), createProcess, getPid, proc, terminateProcess, waitForProcess)
import System.Posix.Files (setFileMode)
import Test.Hspec

import Fisher.Agent.Inbox (inboxPath)
import Fisher.Agent.RunLock (runLockPath)
import Fisher.Agent.Steps
import Fisher.UI.Access (Gate (..))
import Fisher.UI.Api.Live
import Fisher.UI.Api.Steps (handMessage, verdictsPath)
import Fisher.UI.Diff (unifiedDiff)
import Fisher.UI.Db (newUiEnv)
import Fisher.UI.FakeFisher (ledgerSchema)
import Fisher.UI.Server (liveApis, serve)

gate :: Gate
gate = Gate "0123456789abcdef0123456789abcdef" 8732

call :: Application -> Method -> B.ByteString -> [Header] -> BL.ByteString -> IO SResponse
call app m p hs body = runSession (srequest (SRequest req body)) app
  where
    req =
      setPath
        defaultRequest
          { requestMethod = m
          , requestHeaderHost = Just "127.0.0.1:8732"
          , requestHeaders =
              hs <> [ ("Host", "127.0.0.1:8732"), (hCookie, "fisher_ui_8732=0123456789abcdef0123456789abcdef")
                    , ("Origin", "http://127.0.0.1:8732"), (hContentType, "application/json") ] }
        p

getJ :: Application -> B.ByteString -> IO (Int, A.Value)
getJ app p = do
  r <- call app methodGet p [] ""
  pure (statusCode (simpleStatus r), maybe A.Null id (A.decode (simpleBody r)))

postJ :: Application -> B.ByteString -> A.Value -> IO (Int, A.Value)
postJ app p v = do
  r <- call app methodPost p [] (A.encode v)
  pure (statusCode (simpleStatus r), maybe A.Null id (A.decode (simpleBody r)))

at :: Text -> A.Value -> Maybe A.Value
at k = \case
  A.Object o -> KM.lookup (K.fromText k) o
  _ -> Nothing

str :: Text -> A.Value -> Maybe Text
str k v = case at k v of
  Just (A.String t) -> Just t
  _ -> Nothing

int :: Text -> A.Value -> Maybe Int
int k v = case at k v of
  Just (A.Number n) -> Just (round n)
  _ -> Nothing

list :: A.Value -> [A.Value]
list = \case
  A.Array xs -> V.toList xs
  _ -> []

-- | A hand back's body, from step 3 (the file on disk at the start).
handBody :: FilePath -> Text -> A.Value
handBody path text = A.object ["path" A..= path, "text" A..= text, "base" A..= fileHash (TE.encodeUtf8 leanA3)]

-- The step texts of the Lean session.
leanA1, leanA2, leanA3, leanA4 :: Text
leanA1 = "theorem a : True := sorry\n"
leanA2 = "theorem a : True := by\n  trivial\n"
leanA3 = "theorem a : True := by\n  BAD\n"
leanA4 = "theorem a : True := by\n  exact trivial\n"

data Env = Env
  { eHome :: FilePath
  , eLean :: FilePath
  , eRocq :: FilePath
  , eCount :: FilePath
  , eLive :: LiveEnv
  , eApp :: Application
  }

-- | FISHER_HOME with sessions R (model example/model-a) and its compression
-- child K, a Lean project (lakefile) and a Rocq project (_CoqProject), a
-- fake lake that records where it ran and fails on BAD, and a fake fisher:
-- @check@ fails a file with Admitted (for the hole) or BAD, @chat@ writes
-- its arguments, cwd and FISHER_UI_LEDGER to files, and anything else
-- (launch) records its tag as R's run in ui_runs and stays up.
withSteps :: (Env -> IO a) -> IO a
withSteps k = withSystemTempDirectory "fisher-ui-steps" $ \tmp0 -> do
  tmp <- canonicalizePath tmp0
  let home = tmp </> "fisher"
      lean = tmp </> "lean"
      rocq = tmp </> "rocq"
      count = tmp </> "lake-runs.txt"
      lake = tmp </> "fake-lake"
      fake = tmp </> "fake-fisher"
  mapM_ (createDirectoryIfMissing True) [home, lean, rocq </> "theories"]
  B.writeFile (lean </> "lakefile.lean") "import Lake\n"
  B.writeFile (rocq </> "_CoqProject") "-R theories Demo\n"
  TIO.writeFile lake $
    T.unlines
      [ "#!/bin/sh"
      , "pwd >> '" <> T.pack count <> "'"
      , "for f in \"$@\"; do :; done"
      , "if grep -q SLOW \"$f\"; then sleep 2; fi"
      , "if grep -q BAD \"$f\"; then echo \"$PWD/$f:2:2: error: unknown tactic\"; exit 1; fi"
      , "exit 0" ]
  TIO.writeFile fake $
    T.unlines
      [ "#!/bin/sh"
      , "case \"$1\" in"
      , "check)"
      , "  if grep -q BAD \"$2\"; then echo \"FAIL $2:r: does not compile\"; exit 1; fi"
      , "  if grep -q Admitted \"$2\"; then echo \"FAIL $2:r: uses Admitted\"; exit 1; fi"
      , "  echo \"ok $2:r\"; exit 0 ;;"
      , "chat)"
      , "  printf '%s\\n' \"$@\" > \"$FISHER_HOME/chat-args.txt\""
      , "  pwd > \"$FISHER_HOME/chat-cwd.txt\"; echo \"$FISHER_UI_LEDGER\" > \"$FISHER_HOME/chat-ledger.txt\""
      , "  echo \"$LEAN_PROJECT_PATH\" > \"$FISHER_HOME/chat-env.txt\""
      , "  sleep 0.3; exit 0 ;;"
      , "*)"
      , "  sqlite3 \"$FISHER_HOME/state.db\" \".timeout 5000\" \"INSERT INTO ui_runs VALUES ('$FISHER_UI_TAG', 'R', $$, 0);\" > /dev/null"
      , "  sleep 30 ;;"
      , "esac" ]
  mapM_ (`setFileMode` 0o755) [lake, fake]
  c <- open (home </> "state.db")
  mapM_ (execute_ c . Query) ledgerSchema
  execute_ c "CREATE TABLE sessions (id TEXT PRIMARY KEY, parent_session_id TEXT, model TEXT, started_at REAL)"
  execute_ c "INSERT INTO sessions VALUES ('R', NULL, 'example/model-a', 1), ('K', 'R', 'example/model-a', 2), ('Q', NULL, NULL, 3), ('S', NULL, NULL, 4), ('Z', NULL, NULL, 5)"
  close c
  let a = lean </> "A.lean"
      rv = rocq </> "theories" </> "R.v"
      step' n p t = step n p t "write_file"
      step n p t tool = Step n ("call_" <> T.pack (show n)) p (fileHash (TE.encodeUtf8 t)) t tool (fromIntegral n)
  -- written out of order: listed in number order
  mapM_ (appendStep (stepsPath home "R")) [step 1 a leanA1 "write_file", step 3 a leanA3 "write_file", step 2 a leanA2 "patch"]
  mapM_ (appendStep (stepsPath home "Q")) [step 1 rv "Theorem r : True.\nAdmitted.\n" "write_file", step 2 rv "Theorem r : True.\nProof. BAD. Qed.\n" "patch"]
  B.writeFile a (TE.encodeUtf8 leanA3)
  -- outside the Lean project: a project of its own, reached by a link to a
  -- file and a link to a directory from inside the Lean project
  let out = tmp </> "outside"
  createDirectoryIfMissing True out
  mapM_ (\(f, t) -> B.writeFile (out </> f) t) [("lakefile.lean", "import Lake\n"), ("B.lean", "outside b\n"), ("T.lean", "outside t\n"), ("C.lean", "outside c\n")]
  createFileLink (out </> "T.lean") (lean </> "L.lean")
  createDirectoryLink out (lean </> "sub")
  mapM_ (appendStep (stepsPath home "Z"))
    [ step' 1 (out </> "B.lean") "outside b\n", step' 2 (lean </> "L.lean") "outside t\n", step' 3 (lean </> "sub" </> "C.lean") "outside c\n" ]
  appendStep (stepsPath home "S") (step' 1 (lean </> "S.lean") "theorem s : True := by\n  SLOW\n")
  c2 <- open (home </> "state.db")
  -- Z's chat ran in the Lean project
  execute c2 "INSERT INTO ui_events VALUES ('Z', 1, 0, 'chat', ?)" (Only (TE.decodeUtf8 (BL.toStrict (A.encode (A.object ["env" A..= A.object ["FISHER_PROJECT_ROOT" A..= lean]])))))
  close c2
  ui <- newUiEnv home (tmp </> "bench") (tmp </> "repo")
  le0 <- newLiveEnv ui fake
  let le = le0 {leGrace = 1000000, leIdle = 1000000, leLake = lake, leCheckSecs = 60}
  r <- k (Env home lean rocq count le (serve gate (liveApis le)))
  stopRuns le
  pure r

-- | Another process (python3, fcntl.lockf: the agent's own lock) holding
-- the file's lock while the action runs, with its pid.
withLockHolder :: FilePath -> (Int -> IO a) -> IO a
withLockHolder p k =
  bracket
    (createProcess (proc "python3" ["-c", "import fcntl,sys,time\nf=open(sys.argv[1],'a')\nfcntl.lockf(f, fcntl.LOCK_EX)\nprint('locked', flush=True)\ntime.sleep(60)\n", p]) {std_out = CreatePipe})
    (\(_, _, _, ph) -> terminateProcess ph >> waitForProcess ph)
    (\(_, mout, _, ph) -> do
        forM_ mout hGetLine
        pid <- getPid ph
        k (maybe 0 fromIntegral pid))

waitFor :: String -> IO Bool -> IO ()
waitFor what p = go (200 :: Int)
  where
    go n = do
      ok <- p
      if ok then pure () else if n > 0 then threadDelay 50000 >> go (n - 1) else fail ("never: " <> what)

spec :: Spec
spec = describe "Fisher.UI.Api.Steps" $ do
  it "lists a session's steps in number order, from its chain's root, without their text" $ withSteps $ \e -> do
    (code, v) <- getJ (eApp e) "/api/sessions/K/steps"
    code `shouldBe` 200
    map (int "seq") (list v) `shouldBe` map Just [1, 2, 3]
    map (str "tool") (list v) `shouldBe` map Just ["write_file", "patch", "write_file"]
    map (str "path") (list v) `shouldBe` replicate 3 (Just (T.pack (eLean e </> "A.lean")))
    map (str "hash") (list v) `shouldBe` map (Just . fileHash . TE.encodeUtf8) [leanA1, leanA2, leanA3]
    map (at "text") (list v) `shouldBe` replicate 3 Nothing
    (_, none) <- getJ (eApp e) "/api/sessions/nosuch/steps"
    none `shouldBe` A.Array mempty

  it "gives a step's text as the step recorded it, its verdict null until a check ran; an unknown step is 404" $ withSteps $ \e -> do
    (code, v) <- getJ (eApp e) "/api/sessions/R/steps/2"
    code `shouldBe` 200
    str "text" v `shouldBe` Just leanA2
    at "verdict" v `shouldBe` Just A.Null
    (missing, _) <- getJ (eApp e) "/api/sessions/R/steps/9"
    missing `shouldBe` 404
    (badN, _) <- getJ (eApp e) "/api/sessions/R/steps/x"
    badN `shouldBe` 400

  it "parses a step log again only when its size or time changed" $ withSteps $ \e -> do
    let p = stepsPath (eHome e) "R"
    (_, a) <- getJ (eApp e) "/api/sessions/R/steps/1"
    str "text" a `shouldBe` Just leanA1
    t <- getModificationTime p
    raw <- B.readFile p
    -- the same size and time: the parsed log is kept
    B.writeFile p (TE.encodeUtf8 (T.replace "sorry" "SORRY" (TE.decodeUtf8 raw)))
    setModificationTime p t
    (_, b) <- getJ (eApp e) "/api/sessions/R/steps/1"
    str "text" b `shouldBe` Just leanA1
    setModificationTime p (addUTCTime 10 t)
    (_, c) <- getJ (eApp e) "/api/sessions/R/steps/1"
    str "text" c `shouldBe` Just "theorem a : True := SORRY\n"

  it "computes a Lean step's verdict once, in a copy of the project, then serves it from the cache" $ withSteps $ \e -> do
    before <- sort <$> listDirectory (eLean e)
    (c1, v1) <- postJ (eApp e) "/api/sessions/R/steps/1/verdict" A.Null
    c1 `shouldBe` 200
    (int "holes" v1, at "ok" v1, at "errors" v1) `shouldBe` (Just 1, Just (A.Bool False), Just (A.Array mempty))
    (_, v3) <- postJ (eApp e) "/api/sessions/R/steps/3/verdict" A.Null
    at "ok" v3 `shouldBe` Just (A.Bool False)
    int "holes" v3 `shouldBe` Just 0
    case map (\case A.String t -> t; _ -> "") (maybe [] list (at "errors" v3)) of
      [err] -> do
        err `shouldSatisfy` T.isInfixOf "unknown tactic"
        err `shouldSatisfy` T.isPrefixOf "A.lean:2:2"
      errs -> expectationFailure ("errors: " <> show errs)
    (_, v2) <- postJ (eApp e) "/api/sessions/R/steps/2/verdict" A.Null
    (int "holes" v2, at "ok" v2, str "prover" v2) `shouldBe` (Just 0, Just (A.Bool True), Just "lean4")
    -- the second time from the cache: lake ran three times, not four
    (_, v2') <- postJ (eApp e) "/api/sessions/R/steps/2/verdict" A.Null
    v2' `shouldBe` v2
    runs <- lines <$> readFile (eCount e)
    length runs `shouldBe` 3
    runs `shouldSatisfy` all (/= eLean e)
    (_, s2) <- getJ (eApp e) "/api/sessions/K/steps/2"
    at "verdict" s2 `shouldBe` Just v2
    doesFileExist (verdictsPath (eHome e) "R") `shouldReturn` True
    -- the project as it was, and no copy left behind
    sort <$> listDirectory (eLean e) `shouldReturn` before
    B.readFile (eLean e </> "A.lean") `shouldReturn` TE.encodeUtf8 leanA3
    listDirectory (eHome e </> "fisher" </> "ui-check") `shouldReturn` []

  it "takes a Rocq step's verdict from fisher check in a copy, a hole not counted as an error" $ withSteps $ \e -> do
    (_, v1) <- postJ (eApp e) "/api/sessions/Q/steps/1/verdict" A.Null
    (int "holes" v1, at "ok" v1, at "errors" v1, str "prover" v1) `shouldBe` (Just 1, Just (A.Bool False), Just (A.Array mempty), Just "rocq")
    (_, v2) <- postJ (eApp e) "/api/sessions/Q/steps/2/verdict" A.Null
    (int "holes" v2, at "ok" v2) `shouldBe` (Just 0, Just (A.Bool False))
    at "errors" v2 `shouldBe` Just (A.toJSON ["FAIL theories/R.v:r: does not compile" :: Text])
    doesFileExist (eRocq e </> "theories" </> "R.v") `shouldReturn` False
    listDirectory (eHome e </> "fisher" </> "ui-check") `shouldReturn` []

  it "hands an edit back to a live run: the file written, a hand step, the change through the inbox" $ withSteps $ \e -> do
    (code, started) <- postJ (eApp e) "/api/sessions" (A.object ["project" A..= eLean e, "command" A..= ("/prove A.lean" :: Text)])
    code `shouldBe` 200
    waitFor "the run drives R" $ do
      (_, lv) <- getJ (eApp e) "/api/sessions/K/live"
      pure (str "tag" lv == str "tag" started && at "running" lv == Just (A.Bool True))
    let path = eLean e </> "A.lean"
    (hc, hv) <- postJ (eApp e) "/api/sessions/K/handback" (handBody path leanA4)
    hc `shouldBe` 200
    (str "delivered" hv, int "seq" hv) `shouldBe` (Just "inbox", Just 4)
    (at "verdict" hv >>= at "ok") `shouldBe` Just (A.Bool True)
    B.readFile path `shouldReturn` TE.encodeUtf8 leanA4
    steps <- readSteps (stepsPath (eHome e) "R")
    [(stSeq s, stTool s, stText s) | s <- steps, stSeq s == 4] `shouldBe` [(4, "hand", leanA4)]
    stHash (last steps) `shouldBe` fileHash (TE.encodeUtf8 leanA4)
    inbox <- B.readFile (inboxPath (eHome e) "R")
    let msg = handMessage path (unifiedDiff path leanA3 leanA4)
    map (A.decodeStrict . TE.encodeUtf8) (T.lines (TE.decodeUtf8 inbox)) `shouldBe` [Just (A.object ["text" A..= msg])]
    msg `shouldSatisfy` T.isPrefixOf ("The user edited " <> T.pack path <> " by hand. The change:\n--- " <> T.pack path)
    msg `shouldSatisfy` T.isInfixOf "\n-  BAD\n+  exact trivial\n"
    msg `shouldSatisfy` T.isSuffixOf "\n+  exact trivial\nContinue from the file as it is now."
    str "message" hv `shouldBe` Just msg
    doesFileExist (eHome e </> "chat-args.txt") `shouldReturn` False

  it "hands an edit back with no live run by resuming the chain's latest session, its model kept, the ledger on" $ withSteps $ \e -> do
    let path = eLean e </> "A.lean"
    (hc, hv) <- postJ (eApp e) "/api/sessions/R/handback" (handBody path leanA4)
    hc `shouldBe` 200
    str "delivered" hv `shouldBe` Just "resume"
    tag <- maybe (fail ("no tag: " <> show hv)) pure (str "tag" hv)
    waitFor "the resume ran" (doesFileExist (eHome e </> "chat-ledger.txt"))
    let msg = handMessage path (unifiedDiff path leanA3 leanA4)
    threadDelay 100000
    args <- T.lines <$> TIO.readFile (eHome e </> "chat-args.txt")
    args `shouldBe` (["chat", "-m", "example/model-a", "-c", "K", "-p"] <> T.lines msg)
    lines <$> readFile (eHome e </> "chat-cwd.txt") `shouldReturn` [eLean e]
    lines <$> readFile (eHome e </> "chat-ledger.txt") `shouldReturn` ["1"]
    (_, lv) <- getJ (eApp e) "/api/sessions/R/live"
    str "tag" lv `shouldBe` Just tag
    doesFileExist (inboxPath (eHome e) "R") `shouldReturn` False

  it "resumes with the flags and variables the session's chat recorded in the ledger" $ withSteps $ \e -> do
    c <- open (eHome e </> "state.db")
    let chatEv = A.object
          [ "model" A..= ("example/model-b" :: Text), "provider" A..= ("openrouter" :: Text), "toolsets" A..= (["fisher-lean", "todo"] :: [Text])
          , "systemPromptFile" A..= ("/x/s.md" :: Text), "mcpConfig" A..= ("/x/m.json" :: Text), "context" A..= A.Null, "maxTurns" A..= A.Null
          , "env" A..= A.object ["LEAN_PROJECT_PATH" A..= eLean e] ]
    execute c "INSERT INTO ui_events VALUES ('R', 1, 0, 'chat', ?), ('R', 2, 0, 'end', '{\"status\":\"stopped\"}')" (Only (TE.decodeUtf8 (BL.toStrict (A.encode chatEv))))
    close c
    let path = eLean e </> "A.lean"
    (hc, hv) <- postJ (eApp e) "/api/sessions/R/handback" (handBody path leanA4)
    hc `shouldBe` 200
    int "after" hv `shouldBe` Just 2
    waitFor "the resume ran" (doesFileExist (eHome e </> "chat-env.txt"))
    threadDelay 100000
    let msg = handMessage path (unifiedDiff path leanA3 leanA4)
    args <- T.lines <$> TIO.readFile (eHome e </> "chat-args.txt")
    args `shouldBe` (["chat", "--provider", "openrouter", "-m", "example/model-b", "-t", "fisher-lean,todo", "--system-prompt-file", "/x/s.md", "--mcp-config", "/x/m.json", "-c", "K", "-p"] <> T.lines msg)
    lines <$> readFile (eHome e </> "chat-env.txt") `shouldReturn` [eLean e]

  it "refuses a file the session never changed, an unchanged text, a relative path and a cross-site page" $ withSteps $ \e -> do
    let path = eLean e </> "A.lean"
    (c1, _) <- postJ (eApp e) "/api/sessions/R/handback" (handBody (eLean e </> "lakefile.lean") "x")
    c1 `shouldBe` 400
    (c2, _) <- postJ (eApp e) "/api/sessions/R/handback" (handBody path leanA3)
    c2 `shouldBe` 400
    (c3, _) <- postJ (eApp e) "/api/sessions/R/handback" (handBody "A.lean" leanA4)
    c3 `shouldBe` 400
    r <- call (eApp e) methodPost "/api/sessions/R/handback" [("Origin", "http://evil.example")] (A.encode (handBody path leanA4))
    statusCode (simpleStatus r) `shouldBe` 403
    B.readFile path `shouldReturn` TE.encodeUtf8 leanA3
    map stSeq <$> readSteps (stepsPath (eHome e) "R") `shouldReturn` [1, 3, 2]

  it "refuses a hand back outside the session's project, to a linked file and through a linked directory" $ withSteps $ \e -> do
    let out = takeDirectory (eLean e) </> "outside"
        body p t = A.object ["path" A..= p, "text" A..= (t :: Text), "base" A..= fileHash (TE.encodeUtf8 t)]
    -- the recorded project wins over the outside directory's own lakefile
    forM_ [(out </> "B.lean", "outside b\n"), (eLean e </> "L.lean", "outside t\n"), (eLean e </> "sub" </> "C.lean", "outside c\n")] $ \(p, t) -> do
      (code, v) <- postJ (eApp e) "/api/sessions/Z/handback" (body p t)
      (p, code) `shouldBe` (p, 400)
      str "error" v `shouldSatisfy` maybe False (T.isInfixOf "project")
    mapM_ (\(f, t) -> B.readFile (out </> f) `shouldReturn` t) [("B.lean", "outside b\n"), ("T.lean", "outside t\n"), ("C.lean", "outside c\n")]
    map stSeq <$> readSteps (stepsPath (eHome e) "Z") `shouldReturn` [1, 2, 3]

  it "answers 409 when the file changed since the step the edit started from" $ withSteps $ \e -> do
    let path = eLean e </> "A.lean"
    (code, _) <- postJ (eApp e) "/api/sessions/R/handback" (A.object ["path" A..= path, "text" A..= leanA4, "base" A..= fileHash (TE.encodeUtf8 leanA1)])
    code `shouldBe` 409
    (missing, _) <- postJ (eApp e) "/api/sessions/R/handback" (A.object ["path" A..= path, "text" A..= leanA4])
    missing `shouldBe` 400
    B.readFile path `shouldReturn` TE.encodeUtf8 leanA3
    map stSeq <$> readSteps (stepsPath (eHome e) "R") `shouldReturn` [1, 3, 2]

  it "refuses a check past four waiting behind the running one (503)" $ withSteps $ \e -> do
    dones <- forM [1 .. 6 :: Int] $ \_ -> do
      v <- newEmptyMVar
      _ <- forkIO (postJ (eApp e) "/api/sessions/S/steps/1/verdict" A.Null >>= putMVar v . fst)
      pure v
    codes <- mapM takeMVar dones
    sort codes `shouldBe` [200, 200, 200, 200, 200, 503]
    (later, _) <- postJ (eApp e) "/api/sessions/S/steps/1/verdict" A.Null
    later `shouldBe` 200

  it "keeps the change in the inbox when the live run ends before reading it, and says so" $ withSteps $ \e -> do
    (_, started) <- postJ (eApp e) "/api/sessions" (A.object ["project" A..= eLean e, "command" A..= ("/prove A.lean" :: Text)])
    waitFor "the run drives R" $ do
      (_, lv) <- getJ (eApp e) "/api/sessions/R/live"
      pure (str "tag" lv == str "tag" started && at "running" lv == Just (A.Bool True))
    (_, hv) <- postJ (eApp e) "/api/sessions/R/handback" (handBody (eLean e </> "A.lean") leanA4)
    str "delivered" hv `shouldBe` Just "inbox"
    (sc, _) <- postJ (eApp e) "/api/sessions/R/stop" A.Null
    sc `shouldBe` 202
    waitFor "the run ended" $ do
      (_, lv) <- getJ (eApp e) "/api/sessions/R/live"
      pure (at "running" lv == Just (A.Bool False))
    (_, lv) <- getJ (eApp e) "/api/sessions/R/live"
    int "inboxPending" lv `shouldBe` Just 1

  it "keeps the change in the inbox when the resume cannot start, and says so" $ withSteps $ \e -> do
    let le = (eLive e) {leFisher = eHome e </> "no-such-fisher"}
        app = serve gate (liveApis le)
    (hc, hv) <- postJ app "/api/sessions/R/handback" (handBody (eLean e </> "A.lean") leanA4)
    hc `shouldBe` 200
    str "delivered" hv `shouldBe` Just "pending"
    str "error" hv `shouldSatisfy` maybe False (not . T.null)
    inbox <- B.readFile (inboxPath (eHome e) "R")
    map (A.decodeStrict . TE.encodeUtf8) (T.lines (TE.decodeUtf8 inbox)) `shouldBe` [Just (A.object ["text" A..= (str "message" hv)])]
    (_, lv) <- getJ app "/api/sessions/R/live"
    int "inboxPending" lv `shouldBe` Just 1

  it "tells a session that runs in another process through its inbox and never resumes it; stop is 409 there, send queues" $ withSteps $ \e -> do
    createDirectoryIfMissing True (stepsDir (eHome e))
    let lockp = runLockPath (eHome e) "R"
    writeFile lockp ""
    withLockHolder lockp $ \pid -> do
      (_, lv) <- getJ (eApp e) "/api/sessions/K/live"
      (int "elsewhere" lv, str "status" lv) `shouldBe` (Just pid, Just "running")
      let path = eLean e </> "A.lean"
      (hc, hv) <- postJ (eApp e) "/api/sessions/K/handback" (handBody path leanA4)
      hc `shouldBe` 200
      (str "delivered" hv, int "elsewhere" hv) `shouldBe` (Just "inbox", Just pid)
      B.readFile path `shouldReturn` TE.encodeUtf8 leanA4
      let msg = handMessage path (unifiedDiff path leanA3 leanA4)
      inbox <- B.readFile (inboxPath (eHome e) "R")
      map (A.decodeStrict . TE.encodeUtf8) (T.lines (TE.decodeUtf8 inbox)) `shouldBe` [Just (A.object ["text" A..= msg])]
      threadDelay 500000
      doesFileExist (eHome e </> "chat-args.txt") `shouldReturn` False
      (sc, sv) <- postJ (eApp e) "/api/sessions/R/stop" A.Null
      sc `shouldBe` 409
      str "error" sv `shouldSatisfy` maybe False (T.isInfixOf "running in another process")
      (mc, mv) <- postJ (eApp e) "/api/sessions/R/message" (A.object ["text" A..= ("Also check B." :: Text)])
      (mc, int "elsewhere" mv) `shouldBe` (200, Just pid)
    -- the lock let go: a hand back resumes again
    (_, hv2) <- postJ (eApp e) "/api/sessions/R/handback" (A.object ["path" A..= (eLean e </> "A.lean"), "text" A..= leanA2, "base" A..= fileHash (TE.encodeUtf8 leanA4)])
    str "delivered" hv2 `shouldBe` Just "resume"

  it "tells the agent the file changed substantially when the diff is over 20,000 characters" $ withSteps $ \e -> do
    let path = eLean e </> "A.lean"
        big = leanA3 <> T.concat [T.pack ("-- line " <> show i <> "\n") | i <- [1 .. 3000 :: Int]]
    (hc, hv) <- postJ (eApp e) "/api/sessions/R/handback" (handBody path big)
    hc `shouldBe` 200
    str "message" hv `shouldBe` Just ("The user edited " <> T.pack path <> " by hand. The change: the file changed substantially; read it. Continue from the file as it is now.")
