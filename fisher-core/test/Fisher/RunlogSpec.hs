module Fisher.RunlogSpec (spec) where

import Control.Concurrent.Async (concurrently, mapConcurrently)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Exception (IOException, bracket, throwIO, try)
import Control.Monad (forM_)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.List (isPrefixOf, sort)
import Database.SQLite.Simple (Only (..), close, execute_, open, query_)
import qualified Data.Text as T
import Data.Time (diffUTCTime, getCurrentTime)
import System.Timeout (timeout)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>))
import Test.Hspec

import Fisher.Runlog
import Fisher.TestUtil (withTmp)

sample :: NewRun
sample =
  NewRun
    { nrId = "run-1"
    , nrKind = SearchAttempt
    , nrParent = Just "search-1"
    , nrProject = Just "/work/proj"
    , nrProver = Just "lean"
    , nrBackend = Just "claude-code"
    , nrModel = Just "opus"
    , nrTier = Just "heavy"
    , nrWorkflow = Just "/prove"
    , nrArgv = ["fisher", "search", "--attempt-minutes", "10"]
    , nrStartedAt = 1000.5
    , nrRawDir = Just "/cache/runs/run-1"
    , nrCapture = Full
    }

lesson :: LessonRow
lesson =
  LessonRow
    { lrId = 0
    , lrProjectKey = "p1"
    , lrKind = "unknown_identifier"
    , lrKey = "unknown_identifier:foo"
    , lrText = "In this project `foo` does not exist."
    , lrStatus = Candidate
    , lrEvidenceRuns = ["r1"]
    , lrAgreements = 0
    , lrFirstSeen = 100.5
    , lrActivatedAt = Nothing
    , lrRetiredAt = Nothing
    , lrRunsActive = []
    , lrHitsWhileActive = 0
    , lrAttemptsWhileActive = 0
    , lrHitsBefore = 2
    , lrAttemptsBefore = 9
    , lrRunsBefore = 5
    , lrBeforeWindow = []
    , lrAgreedAttempts = []
    }

withLog :: (FilePath -> Runlog -> IO a) -> IO a
withLog k = withTmp $ \tmp -> let p = runlogPath tmp in withRunlog p (k p)

field :: T.Text -> Value -> Maybe Value
field k = \case
  Object o -> KM.lookup (K.fromText k) o
  _ -> Nothing

spec :: Spec
spec = do
  it "puts runs.db under the Fisher home" $
    runlogPath "/h" `shouldBe` "/h/fisher/runs.db"

  it "round trips every field of a started run" $ withLog $ \_ rl -> do
    startRun rl sample
    Just r <- getRun rl "run-1"
    rrId r `shouldBe` "run-1"
    rrKind r `shouldBe` SearchAttempt
    rrParent r `shouldBe` Just "search-1"
    rrProject r `shouldBe` Just "/work/proj"
    rrProjectKey r `shouldBe` Just (projectKeyOf "/work/proj")
    rrProver r `shouldBe` Just "lean"
    rrBackend r `shouldBe` Just "claude-code"
    rrModel r `shouldBe` Just "opus"
    rrTier r `shouldBe` Just "heavy"
    rrWorkflow r `shouldBe` Just "/prove"
    rrArgv r `shouldBe` ["fisher", "search", "--attempt-minutes", "10"]
    rrStartedAt r `shouldBe` 1000.5
    rrEndedAt r `shouldBe` Nothing
    rrExit r `shouldBe` Nothing
    rrOutcome r `shouldBe` Nothing
    rrRawDir r `shouldBe` Just "/cache/runs/run-1"
    rrCapture r `shouldBe` Full

  it "numbers events 1, 2, 3 and keeps t" $ withLog $ \_ rl -> do
    startRun rl sample
    forM_ [0.5, 1.5, 2.5] $ \t -> event rl "run-1" t "message" (object ["role" .= ("assistant" :: T.Text), "text" .= ("hi" :: T.Text)])
    evs <- runEvents rl "run-1"
    map (\(s, t, k, _) -> (s, t, k)) evs `shouldBe` [(1, 0.5, "message"), (2, 1.5, "message"), (3, 2.5, "message")]

  it "redacts a secret in args" $ withLog $ \_ rl -> do
    startRun rl sample
    event rl "run-1" 0 "tool_start" (object ["name" .= ("terminal" :: T.Text), "args" .= object ["command" .= ("OPENAI_API_KEY=sk-live-abcdefghijklmnop" :: T.Text)]])
    [(_, _, _, d)] <- runEvents rl "run-1"
    case field "args" d >>= field "command" of
      Just (String cmd) -> cmd `shouldSatisfy` (not . T.isInfixOf "abcdefghijklmnop")
      other -> expectationFailure ("no command string: " <> show other)
    field "name" d `shouldBe` Just (String "terminal")

  it "redacts a secret in a mistake event's lesson" $ withLog $ \_ rl -> do
    startRun rl sample
    event rl "run-1" 0 "mistake" (object ["kind" .= ("judged" :: T.Text), "lesson" .= ("Set OPENAI_API_KEY=sk-live-abcdefghijklmnop first." :: T.Text)])
    [(_, _, _, d)] <- runEvents rl "run-1"
    case field "lesson" d of
      Just (String l) -> l `shouldSatisfy` (not . T.isInfixOf "abcdefghijklmnop")
      other -> expectationFailure ("no lesson string: " <> show other)

  it "caps a long result at 65536 characters plus the suffix" $ withLog $ \_ rl -> do
    startRun rl sample
    event rl "run-1" 0 "tool_end" (object ["result" .= T.replicate 102400 "x"])
    [(_, _, _, d)] <- runEvents rl "run-1"
    field "result" d `shouldBe` Just (String (T.replicate 65536 "x" <> "... [truncated]"))

  it "masks the value after a secret flag in argv" $ withLog $ \_ rl -> do
    startRun rl sample {nrArgv = ["fisher", "--api-key", "hunter2hunter2hunter2", "--token=abc123", "--auth-token", "xyz", "--max-tokens", "100", "-p", "keep"]}
    Just r <- getRun rl "run-1"
    let argv = rrArgv r
    length argv `shouldBe` 10
    T.unwords argv `shouldSatisfy` (\w -> not (any (`T.isInfixOf` w) ["hunter2", "abc123", "xyz"]))
    argv !! 1 `shouldBe` "--api-key"
    argv !! 3 `shouldSatisfy` T.isPrefixOf "--token="
    drop 6 argv `shouldBe` ["--max-tokens", "100", "-p", "keep"]

  it "redacts a bare string or array as event data" $ withLog $ \_ rl -> do
    startRun rl sample
    event rl "run-1" 0 "note" (String "OPENAI_API_KEY=sk-live-abcdefghijklmnop")
    event rl "run-1" 1 "note" (toJSONList ["GITHUB_TOKEN=ghp_abcdefghijklmnop0123"])
    evs <- runEvents rl "run-1"
    concatMap (\(_, _, _, d) -> show d) evs `shouldSatisfy` (\s -> not (any (`T.isInfixOf` T.pack s) ["abcdefghijklmnop", "ghp_abcdefghijklmnop0123"]))

  it "caps a result made of many strings at 64 KB as a whole" $ withLog $ \_ rl -> do
    startRun rl sample
    event rl "run-1" 0 "tool_end" (object ["result" .= toJSONList (replicate 3 (T.replicate 40000 "y")), "text" .= T.replicate 70000 "z"])
    [(_, _, _, d)] <- runEvents rl "run-1"
    case field "result" d of
      Just (String s) -> do
        T.length s `shouldBe` 65536 + T.length "... [truncated]"
        s `shouldSatisfy` T.isSuffixOf "... [truncated]"
      other -> expectationFailure ("result not capped to a string: " <> take 200 (show other))
    fmap (\case String s -> T.length s; _ -> -1) (field "text" d) `shouldBe` Just (65536 + T.length "... [truncated]")

  it "masks every string under a secret-named key, at any depth" $ withLog $ \_ rl -> do
    startRun rl sample
    let secrets = ["a1b2c3d4e5f6a7b8c9d0", "correct-horse-battery", "abcdefghijklmnopqrstuvwxyz", "9f8e7d6c5b4a39281706", "nestedsecretvalue123", "arraysecretvalue456"]
        s :: T.Text -> Value
        s = String
    event rl "run-1" 0 "tool_start" $
      object
        [ "name" .= s "mcp_call"
        , "args" .= object
            [ "api_key" .= s (secrets !! 0)
            , "password" .= s (secrets !! 1)
            , "headers" .= object ["Authorization" .= s ("Bearer " <> secrets !! 2)]
            , "env" .= object ["GLM_API_KEY" .= s (secrets !! 3)]
            , "max_tokens" .= (4096 :: Int)
            , "token_count" .= (17 :: Int)
            ]
        ]
    event rl "run-1" 1 "note" $
      object
        [ "config" .= object ["deep" .= toJSONList' [object ["X-Api-Key" .= s (secrets !! 4)]]]
        , "list" .= toJSONList' [object ["token" .= object ["value" .= s (secrets !! 5)]]]
        , "goal" .= s "g1"
        ]
    evs <- runEvents rl "run-1"
    let shown = T.pack (concatMap (\(_, _, _, d) -> show d) evs)
    forM_ secrets $ \x -> shown `shouldSatisfy` (not . T.isInfixOf x)
    [(_, _, _, d1), (_, _, _, d2)] <- pure evs
    (field "args" d1 >>= field "max_tokens") `shouldBe` Just (Number 4096)
    (field "args" d1 >>= field "token_count") `shouldBe` Just (Number 17)
    (field "args" d1 >>= field "headers" >>= field "Authorization") `shouldBe` Just (String "Bearer ***")
    (field "args" d1 >>= field "api_key") `shouldBe` Just (String "***")
    field "goal" d2 `shouldBe` Just (String "g1")

  it "caps a 1 MB result before redacting it, in under a second" $ withLog $ \_ rl -> do
    startRun rl sample
    let line = "theorem foo : 1 + 1 = 2 := by simp  -- OPENAI_API_KEY=sk-live-abcdefghijklmnop\n"
        big = T.replicate (1048576 `div` T.length line + 1) line
    t0 <- getCurrentTime
    event rl "run-1" 0 "tool_end" (object ["result" .= big])
    t1 <- getCurrentTime
    realToFrac (diffUTCTime t1 t0) `shouldSatisfy` (< (1 :: Double))
    -- a 1 MB args value is capped the same way (untimed)
    event rl "run-1" 1 "tool_start" (object ["args" .= object ["input" .= big]])
    [(_, _, _, d0), (_, _, _, d1)] <- runEvents rl "run-1"
    let fields = \case Object o -> o; _ -> KM.empty
        d = Object (fields d0 <> fields d1)
    -- the masks are shorter than the keys they replace, so a capped value
    -- may come out under the cap; it is still marked as cut
    forM_ [field "result" d, field "args" d] $ \case
      Just (String r) -> do
        T.length r `shouldSatisfy` (<= 65536 + T.length "... [truncated]")
        r `shouldSatisfy` T.isSuffixOf "... [truncated]"
        r `shouldSatisfy` (not . T.isInfixOf "abcdefghijklmnop")
      other -> expectationFailure ("not a capped string: " <> take 200 (show other))

  it "stores no line of a private key block cut by the cap, wherever it starts" $ withLog $ \_ rl -> do
    startRun rl sample
    let bodyLines = [T.pack (take 64 (drop (i `mod` 7) (cycle "MIIEowIBAAKCAQEAx9f3Zq0LmN8vR2tYw4pK1sJ6hB5cD7eF9gH0iJ2kL3mN4oP5qR"))) <> T.pack (show i) | i <- [1 .. 40 :: Int]]
        block = "-----BEGIN RSA PRIVATE KEY-----\n" <> T.intercalate "\n" bodyLines <> "\n-----END RSA PRIVATE KEY-----\n"
    T.length block `shouldSatisfy` (\n -> n > 2600 && n < 2800)
    -- the probe: the block starts just before the old cut (65536 + 1024);
    -- then one starting past any cut margin, which loses its END marker
    forM_ [65000, 73000, 73700] $ \pad -> do
      event rl "run-1" 0 "tool_end" (object ["result" .= (T.replicate pad "x" <> block)])
    evs <- runEvents rl "run-1"
    length evs `shouldBe` 3
    forM_ evs $ \(_, _, _, d) -> case field "result" d of
      Just (String r) -> forM_ bodyLines $ \l -> r `shouldSatisfy` (not . T.isInfixOf (T.take 40 l))
      other -> expectationFailure ("no result string: " <> take 200 (show other))

  it "caps a large structured args value as a whole" $ withLog $ \_ rl -> do
    startRun rl sample
    event rl "run-1" 0 "tool_start" (object ["args" .= toJSONList (replicate 3 (T.replicate 40000 "y"))])
    [(_, _, _, d)] <- runEvents rl "run-1"
    fmap (\case String x -> T.length x; _ -> -1) (field "args" d) `shouldBe` Just (65536 + T.length "... [truncated]")

  it "leaves no transaction open when the work inside one throws" $ withLog $ \_ rl -> do
    startRun rl sample
    r <- try (withWrite rl (\c -> execute_ c "DELETE FROM goals" >> throwIO (userError "boom"))) :: IO (Either IOException ())
    either (const True) (const False) r `shouldBe` True
    upsertGoal rl "run-1" (GoalRow "g1" "s" "eq||Nat|small" "open" Nothing Nothing Nothing)
    map grName <$> runGoals rl "run-1" `shouldReturn` ["g1"]

  it "leaves no transaction open when a write is cancelled at any point" $ withLog $ \_ rl -> do
    startRun rl sample
    forM_ [0 .. 300 :: Int] $ \i -> do
      _ <- timeout (i `mod` 60) (upsertGoal rl "run-1" (GoalRow (T.pack ("g" <> show (i `mod` 3))) "s" "eq||Nat|small" "open" Nothing Nothing Nothing))
      pure ()
    upsertGoal rl "run-1" (GoalRow "last" "s" "eq||Nat|small" "open" Nothing Nothing Nothing)
    (map grName <$> runGoals rl "run-1") `shouldReturn'` "last"

  it "keeps a small structured result as it is" $ withLog $ \_ rl -> do
    startRun rl sample
    let small = object ["ok" .= True, "lines" .= toJSONList ["a", "b"]]
    event rl "run-1" 0 "tool_end" (object ["result" .= small])
    [(_, _, _, d)] <- runEvents rl "run-1"
    field "result" d `shouldBe` Just small

  it "starts a run afresh when its id is started again" $ withLog $ \_ rl -> do
    startRun rl sample
    event rl "run-1" 0 "message" (object ["text" .= ("old" :: T.Text)])
    upsertGoal rl "run-1" (GoalRow "g1" "s" "eq||Nat|small" "closed" Nothing Nothing Nothing)
    endRun rl "run-1" (RunEnd 1010 (Just 0) "closed" Nothing Nothing Nothing Nothing)
    startRun rl sample
    runEvents rl "run-1" `shouldReturn` []
    runGoals rl "run-1" `shouldReturn` []
    Just r <- getRun rl "run-1"
    (rrEndedAt r, rrOutcome r) `shouldBe` (Nothing, Nothing)
    event rl "run-1" 0 "message" (object [])
    map (\(q, _, _, _) -> q) <$> runEvents rl "run-1" `shouldReturn` [1]

  it "records the end of a run" $ withLog $ \_ rl -> do
    startRun rl sample
    endRun rl "run-1" (RunEnd 1010 (Just 0) "closed" (Just 0.25) (Just 100) (Just 20) (Just 3))
    Just r <- getRun rl "run-1"
    (rrEndedAt r, rrExit r, rrOutcome r, rrCostUsd r) `shouldBe` (Just 1010, Just 0, Just "closed", Just 0.25)
    (rrInputTokens r, rrOutputTokens r, rrTurns r) `shouldBe` (Just 100, Just 20, Just 3)

  it "lists runs by kind and project key, newest first" $ withLog $ \_ rl -> do
    startRun rl sample {nrId = "a", nrStartedAt = 1}
    startRun rl sample {nrId = "b", nrStartedAt = 3}
    startRun rl sample {nrId = "c", nrStartedAt = 2, nrKind = LaunchRun}
    startRun rl sample {nrId = "d", nrStartedAt = 4, nrProject = Just "/other"}
    attempts <- listRuns rl Nothing (Just (projectKeyOf "/work/proj")) (Just SearchAttempt) 10
    map rrId attempts `shouldBe` ["b", "a"]
    everything <- listRuns rl Nothing Nothing Nothing 10
    map rrId everything `shouldBe` ["d", "b", "c", "a"]
    recent <- listRuns rl (Just 2.5) Nothing Nothing 10
    map rrId recent `shouldBe` ["d", "b"]

  it "keeps goals and exports a run with its goals and events" $ withLog $ \_ rl -> do
    startRun rl sample
    upsertGoal rl "run-1" (GoalRow "g1" "1 + 1 = 2" "eq||Nat|small" "open" Nothing Nothing Nothing)
    upsertGoal rl "run-1" (GoalRow "g1" "1 + 1 = 2" "eq||Nat|small" "closed" (Just "auto") (Just 1.5) (Just 0))
    event rl "run-1" 0 "message" (object ["text" .= ("ok" :: T.Text)])
    gs <- runGoals rl "run-1"
    map grStatus gs `shouldBe` ["closed"]
    [v] <- exportRuns rl Nothing Nothing
    field "id" v `shouldBe` Just (String "run-1")
    (field "goals" v, fmap (\case Array a -> length a; _ -> -1) (field "events" v))
      `shouldSatisfy` (\(g, e) -> g /= Nothing && e == Just 1)

  it "reads the events of some kinds across runs in one query" $ withLog $ \_ rl -> do
    startRun rl sample
    startRun rl sample {nrId = "run-0"}
    event rl "run-1" 0 "attempt" (object ["goal" .= ("g" :: T.Text)])
    event rl "run-1" 1 "tool_start" (object [])
    event rl "run-0" 2 "auto closed" (object ["goal" .= ("h" :: T.Text)])
    event rl "run-0" 3 "attempt" (object [])
    evs <- runEventsOfKinds rl ["attempt", "auto closed"]
    map (\(r, s, t, k, _) -> (r, s, t, k)) evs
      `shouldBe` [("run-0", 1, 2, "auto closed"), ("run-0", 2, 3, "attempt"), ("run-1", 1, 0, "attempt")]
    [d | ("run-1", _, _, _, d) <- evs] `shouldBe` [object ["goal" .= ("g" :: T.Text)]]
    runEventsOfKinds rl [] `shouldReturn` []

  it "records a policy change" $ withLog $ \_ rl ->
    recordPolicyChange rl 5 (projectKeyOf "/work/proj") "*" "backend" (toJSONList ["codex"]) (toJSONList ["claude-code"]) (object [])

  it "lists a project's policy changes newest first, at most the limit" $ withLog $ \_ rl -> do
    let key = projectKeyOf "/work/proj"
    recordPolicyChange rl 5 key "*" "backends" (toJSONList ["codex"]) (toJSONList ["claude-code"]) (object ["n" .= (1 :: Int)])
    recordPolicyChange rl 7 key "eq||Nat|small" "tiers" (toJSONList ["light"]) (toJSONList ["heavy"]) (object [])
    recordPolicyChange rl 6 key "*" "tactics" (toJSONList []) (toJSONList ["simp"]) (object [])
    recordPolicyChange rl 9 (projectKeyOf "/other") "*" "backends" (toJSONList []) (toJSONList ["fisher"]) (object [])
    cs <- listPolicyChanges rl key 2
    map (\(at, sh, k, _, _, _) -> (at, sh, k)) cs `shouldBe` [(7, "eq||Nat|small", "tiers"), (6, "*", "tactics")]
    allOf <- listPolicyChanges rl key 0
    map (\(at, _, _, _, _, _) -> at) allOf `shouldBe` [7, 6, 5]
    [(_, _, _, old, new, ev)] <- pure (drop 2 allOf)
    (old, new, ev) `shouldBe` (toJSONList ["codex"], toJSONList ["claude-code"], object ["n" .= (1 :: Int)])

  it "lists the projects runs were in, the latest first" $ withLog $ \_ rl -> do
    startRun rl sample {nrId = "a", nrStartedAt = 1}
    startRun rl sample {nrId = "b", nrStartedAt = 3, nrProject = Just "/other"}
    startRun rl sample {nrId = "c", nrStartedAt = 2, nrProject = Nothing}
    runProjects rl `shouldReturn` ["/other", "/work/proj"]

  it "reads the first events of a run up to a limit" $ withLog $ \_ rl -> do
    startRun rl sample
    forM_ [1 .. 5 :: Int] $ \i -> event rl "run-1" (fromIntegral i) "step" (object [])
    evs <- runEventsUpTo rl "run-1" 3
    map (\(s, _, _, _) -> s) evs `shouldBe` [1, 2, 3]

  it "reads through a connection opened elsewhere (read-only), making no schema" $ withTmp $ \tmp -> do
    let p = runlogPath tmp
    withRunlog p (\rl -> startRun rl sample)
    rows <- bracket (open ("file:" <> p <> "?mode=ro")) close $ \c -> do
      rl <- runlogOn c
      listRuns rl Nothing Nothing Nothing 0
    map rrId rows `shouldBe` ["run-1"]

  -- The suite runs with -N2, and both writers wait on one barrier, so the
  -- two connections really contend for the write lock.
  it "takes 100 events from two writers in parallel" $ withTmp $ \tmp -> do
    let p = runlogPath tmp
    withRunlog p $ \a -> withRunlog p $ \b -> do
      startRun a sample
      go <- newEmptyMVar
      done <- mapConcurrently (\rl -> do
        readMVar go
        forM_ [1 .. 50 :: Int] $ \i -> event rl "run-1" (fromIntegral i) "step" (object [])) [a, b]
        `concurrently` putMVar go ()
      length (fst done) `shouldBe` 2
      evs <- runEvents a "run-1"
      sort (map (\(s, _, _, _) -> s) evs) `shouldBe` [1 .. 100]

  -- upsertGoal holds BEGIN IMMEDIATE for its delete and insert: the other
  -- connection's event inserts wait on the busy timeout meanwhile.
  it "writes events while another connection upserts goals" $ withTmp $ \tmp -> do
    let p = runlogPath tmp
    withRunlog p $ \a -> withRunlog p $ \b -> do
      startRun a sample
      go <- newEmptyMVar
      let goals = forM_ [1 .. 50 :: Int] $ \i ->
            upsertGoal a "run-1" (GoalRow (T.pack ("g" <> show (i `mod` 5))) "s" "eq||Nat|small" (T.pack (show i)) Nothing Nothing Nothing)
          events = forM_ [1 .. 50 :: Int] $ \i -> event b "run-1" (fromIntegral i) "step" (object [])
      _ <- concurrently (concurrently (readMVar go >> goals) (readMVar go >> events)) (putMVar go ())
      evs <- runEvents b "run-1"
      map (\(s, _, _, _) -> s) evs `shouldBe` [1 .. 50]
      gs <- runGoals a "run-1"
      length gs `shouldBe` 5

  it "closes once: a second close does nothing and never throws, and a later write is an error" $ withTmp $ \tmp -> do
    let p = runlogPath tmp
    r <- withRunlog p $ \rl -> do
      startRun rl sample
      closeRunlog rl
      closeRunlog rl
      try (event rl "run-1" 0 "late" (object [])) :: IO (Either IOException ())
    either (const True) (const False) r `shouldBe` True
    withRunlog p (\rl -> map rrId <$> listRuns rl Nothing Nothing Nothing 0) `shouldReturn` ["run-1"]

  it "moves a corrupt file aside and starts afresh" $ withTmp $ \tmp -> do
    let p = runlogPath tmp
    withRunlog p (const (pure ()))
    writeFile p "not a database"
    r <- openRunlog p
    case r of
      Left e -> expectationFailure (T.unpack e)
      Right rl -> do
        startRun rl sample
        closeRunlog rl
    names <- listDirectory (tmp </> "fisher")
    filter ("runs.db.broken-" `isPrefixOf`) names `shouldSatisfy` (not . null)
    doesFileExist p `shouldReturn` True

  describe "lessons" $ do
    it "modifies a lesson in one transaction, and two passes at once both land" $ withLog $ \_ rl -> do
      i <- upsertLesson rl lesson
      modifyLesson rl (i + 100) id `shouldReturn` Nothing
      Just m <- modifyLesson rl i (\l -> l {lrText = "changed", lrId = 999})
      (lrId m, lrText m) `shouldBe` (i, "changed")
      _ <- mapConcurrently (\n -> modifyLesson rl i (\l -> l {lrRunsActive = lrRunsActive l <> ["r" <> T.pack (show n)]})) [1 .. 20 :: Int]
      Just l <- getLesson rl i
      sort (lrRunsActive l) `shouldBe` sort ["r" <> T.pack (show n) | n <- [1 .. 20 :: Int]]


    it "modifyLessonE writes only on a Right, deciding on the row as stored" $ withLog $ \_ rl -> do
      i <- upsertLesson rl lesson {lrStatus = Active}
      modifyLessonE rl (i + 100) Right `shouldReturn` Right Nothing
      -- changed under the caller: the function sees the stored row
      setLessonStatus rl i Retired 70
      r <- modifyLessonE rl i (\l -> if lrStatus l == Active then Right l {lrStatus = Pinned} else Left "not active")
      r `shouldBe` Left "not active"
      (fmap lrStatus <$> getLesson rl i) `shouldReturn` Just Retired
      Right (Just m) <- modifyLessonE rl i (\l -> Right l {lrText = "changed", lrId = 999})
      lrId m `shouldBe` i
      lrText m `shouldBe` "changed"
      (fmap lrText <$> getLesson rl i) `shouldReturn` Just "changed"
    it "merges into a stored lesson or inserts the fresh one, one row for two first sightings" $ withLog $ \_ rl -> do
      let addRun r l = l {lrEvidenceRuns = lrEvidenceRuns l <> [r]}
      a <- mergeLesson rl "p1" "k:a" lesson {lrEvidenceRuns = ["r1"]} (addRun "r1")
      (lrProjectKey a, lrKey a, lrEvidenceRuns a) `shouldBe` ("p1", "k:a", ["r1"])
      a' <- mergeLesson rl "p1" "k:a" lesson {lrEvidenceRuns = ["r2"]} (addRun "r2")
      (lrId a', lrEvidenceRuns a') `shouldBe` (lrId a, ["r1", "r2"])
      _ <- concurrently
        (mergeLesson rl "p2" "k:b" lesson {lrEvidenceRuns = ["x"]} (addRun "x"))
        (mergeLesson rl "p2" "k:b" lesson {lrEvidenceRuns = ["y"]} (addRun "y"))
      rows <- listLessons rl (Just "p2") []
      map (sort . lrEvidenceRuns) rows `shouldBe` [["x", "y"]]

    it "stores the before window and reads the before counters as its sums" $ withLog $ \_ rl -> do
      i <- upsertLesson rl lesson {lrBeforeWindow = [("r1", 1, 10), ("r2", 0, 8)]}
      Just l <- getLesson rl i
      lrBeforeWindow l `shouldBe` [("r1", 1, 10), ("r2", 0, 8)]
      (lrRunsBefore l, lrHitsBefore l, lrAttemptsBefore l) `shouldBe` (2, 1, 18)

    it "stores the attempts that agreed" $ withLog $ \_ rl -> do
      i <- upsertLesson rl lesson {lrAgreedAttempts = ["s1-1", "s2-3"], lrAgreements = 2}
      fmap lrAgreedAttempts <$> getLesson rl i `shouldReturn` Just ["s1-1", "s2-3"]

    it "makes no ALTER: an older lessons table without before_window is not changed" $ withTmp $ \tmp -> do
      let p = tmp </> "fisher" </> "runs.db"
      withRunlog p (const (pure ()))
      bracket (open p) close $ \c -> do
        execute_ c "DROP TABLE lessons"
        execute_ c "CREATE TABLE lessons (id INTEGER PRIMARY KEY AUTOINCREMENT, project_key TEXT NOT NULL,\
                   \ kind TEXT NOT NULL, key TEXT NOT NULL, text TEXT NOT NULL, status TEXT NOT NULL, evidence_runs TEXT NOT NULL,\
                   \ agreements INTEGER NOT NULL, first_seen REAL NOT NULL, activated_at REAL, retired_at REAL, runs_active TEXT NOT NULL,\
                   \ hits_while_active INTEGER NOT NULL, attempts_while_active INTEGER NOT NULL, hits_before INTEGER NOT NULL,\
                   \ attempts_before INTEGER NOT NULL, runs_before INTEGER NOT NULL, UNIQUE (project_key, key))"
      withRunlog p $ \rl -> listLessons rl Nothing [] `shouldReturn` []
      bracket (open p) close $ \c -> do
        cols <- query_ c "SELECT name FROM pragma_table_info('lessons')" :: IO [Only T.Text]
        map fromOnly cols `shouldNotContain` ["before_window"]

    it "moves an older judged or self key into its own namespace when opened, once" $ withTmp $ \tmp -> do
      let p = tmp </> "fisher" </> "runs.db"
      ids <- withRunlog p $ \rl -> mapM (upsertLesson rl)
        [ lesson {lrKind = "judged", lrKey = "budget_burn"}
        , lesson {lrKind = "self", lrKey = "self:nat.foo"}
        , lesson {lrKind = "budget_burn", lrKey = "budget_burn", lrProjectKey = "p2"} ]
      -- written before the namespace: put the old key back by hand
      bracket (open p) close $ \c -> execute_ c "UPDATE lessons SET key = 'budget_burn' WHERE kind = 'judged'"
      forM_ [1, 2 :: Int] $ \_ -> withRunlog p $ \rl ->
        mapM (fmap (fmap lrKey) . getLesson rl) ids `shouldReturn` map Just ["judged:budget_burn", "self:nat.foo", "budget_burn"]

    it "folds a bare judged key an older binary wrote beside its prefixed row into that row" $ withTmp $ \tmp -> do
      let p = tmp </> "fisher" </> "runs.db"
      (twin, bare) <- withRunlog p $ \rl -> do
        t <- upsertLesson rl lesson {lrKind = "judged", lrKey = "judged:budget_burn", lrEvidenceRuns = ["r1"], lrAgreements = 1, lrAgreedAttempts = ["a1"]}
        b <- upsertLesson rl lesson {lrKind = "judged", lrKey = "budget_burn", lrEvidenceRuns = ["r1", "r2"], lrAgreements = 2, lrAgreedAttempts = ["a1", "a2"]}
        pure (t, b)
      withRunlog p $ \rl -> do
        getLesson rl bare `shouldReturn` Nothing
        findLesson rl "p1" "budget_burn" `shouldReturn` Nothing
        Just t <- getLesson rl twin
        (lrKey t, lrEvidenceRuns t, lrAgreedAttempts t, lrAgreements t) `shouldBe` ("judged:budget_burn", ["r1", "r2"], ["a1", "a2"], 2)

    it "keeps a pinned or dropped bare row's status when folding it, and its agreements with no attempts recorded" $ withTmp $ \tmp -> do
      let p = tmp </> "fisher" </> "runs.db"
      ids <- withRunlog p $ \rl -> do
        t1 <- upsertLesson rl lesson {lrKind = "judged", lrKey = "judged:a", lrEvidenceRuns = ["r1"], lrAgreements = 1, lrAgreedAttempts = ["a1"]}
        _ <- upsertLesson rl lesson {lrKind = "judged", lrKey = "a", lrStatus = Pinned, lrActivatedAt = Just 7, lrEvidenceRuns = ["r2"], lrAgreements = 3, lrAgreedAttempts = []}
        t2 <- upsertLesson rl lesson {lrKind = "self", lrKey = "self:b", lrStatus = Active, lrActivatedAt = Just 5}
        _ <- upsertLesson rl lesson {lrKind = "self", lrKey = "b", lrStatus = Dropped, lrRetiredAt = Just 9}
        t3 <- upsertLesson rl lesson {lrKind = "judged", lrKey = "judged:c", lrStatus = Pinned, lrActivatedAt = Just 4}
        _ <- upsertLesson rl lesson {lrKind = "judged", lrKey = "c", lrStatus = Candidate}
        pure [t1, t2, t3]
      withRunlog p $ \rl -> do
        [a, b, c] <- mapM (fmap (maybe (error "gone") id) . getLesson rl) ids
        (lrStatus a, lrActivatedAt a, lrAgreements a, lrEvidenceRuns a) `shouldBe` (Pinned, Just 7, 4, ["r1", "r2"])
        (lrStatus b, lrRetiredAt b) `shouldBe` (Dropped, Just 9)
        lrStatus c `shouldBe` Pinned
        length <$> listLessons rl (Just "p1") [] `shouldReturn` 3

    it "inserts a lesson, gets it by id and finds it by project and key" $ withLog $ \_ rl -> do
      i <- upsertLesson rl lesson
      i `shouldSatisfy` (> 0)
      getLesson rl i `shouldReturn` Just lesson {lrId = i}
      findLesson rl "p1" "unknown_identifier:foo" `shouldReturn` Just lesson {lrId = i}
      findLesson rl "p2" "unknown_identifier:foo" `shouldReturn` Nothing
      getLesson rl (i + 100) `shouldReturn` Nothing

    it "lists by project and status, oldest first seen first" $ withLog $ \_ rl -> do
      a <- upsertLesson rl lesson {lrKey = "k:a", lrFirstSeen = 30}
      b <- upsertLesson rl lesson {lrKey = "k:b", lrFirstSeen = 10, lrStatus = Active}
      c <- upsertLesson rl lesson {lrKey = "k:c", lrFirstSeen = 20, lrProjectKey = "*", lrStatus = Pinned}
      map lrId <$> listLessons rl Nothing [] `shouldReturn` [b, c, a]
      map lrId <$> listLessons rl (Just "p1") [] `shouldReturn` [b, a]
      map lrId <$> listLessons rl Nothing [Active, Pinned] `shouldReturn` [b, c]
      map lrId <$> listLessons rl (Just "*") [Candidate] `shouldReturn` []

    it "sets the timestamps a status change needs" $ withLog $ \_ rl -> do
      i <- upsertLesson rl lesson
      setLessonStatus rl i Active 50
      Just a <- getLesson rl i
      (lrStatus a, lrActivatedAt a, lrRetiredAt a) `shouldBe` (Active, Just 50, Nothing)
      setLessonStatus rl i Retired 80
      Just r <- getLesson rl i
      (lrStatus r, lrActivatedAt r, lrRetiredAt r) `shouldBe` (Retired, Just 50, Just 80)
      setLessonStatus rl i Candidate 90
      Just c <- getLesson rl i
      (lrStatus c, lrActivatedAt c, lrRetiredAt c) `shouldBe` (Candidate, Nothing, Nothing)
      setLessonStatus rl i Dropped 95
      Just d <- getLesson rl i
      (lrStatus d, lrRetiredAt d) `shouldBe` (Dropped, Just 95)

    it "updates on a second upsert of the same project and key, keeping the id" $ withLog $ \_ rl -> do
      i <- upsertLesson rl lesson
      j <- upsertLesson rl lesson {lrText = "newer text", lrEvidenceRuns = ["r1", "r2", "r3"]}
      j `shouldBe` i
      Just l <- getLesson rl i
      (lrText l, lrEvidenceRuns l) `shouldBe` ("newer text", ["r1", "r2", "r3"])
      length <$> listLessons rl Nothing [] `shouldReturn` 1
      k <- upsertLesson rl l {lrHitsWhileActive = 4, lrRunsActive = ["r9"]}
      k `shouldBe` i
      fmap (\x -> (lrHitsWhileActive x, lrRunsActive x)) <$> getLesson rl i `shouldReturn` Just (4, ["r9"])

    it "keeps the status, timestamps, runs in force and counters when an insert meets a stored pair" $ withLog $ \_ rl -> do
      i <- upsertLesson rl lesson
      setLessonStatus rl i Active 50
      Just a <- getLesson rl i
      stored <- upsertLesson rl a {lrRunsActive = ["r5"], lrHitsWhileActive = 3, lrAttemptsWhileActive = 7}
      stored `shouldBe` i
      j <- upsertLesson rl lesson {lrText = "fresh text", lrEvidenceRuns = ["r1", "r2"], lrAgreements = 2, lrFirstSeen = 999, lrHitsBefore = 0, lrRunsBefore = 0}
      j `shouldBe` i
      Just l <- getLesson rl i
      (lrText l, lrEvidenceRuns l, lrAgreements l) `shouldBe` ("fresh text", ["r1", "r2"], 2)
      (lrStatus l, lrActivatedAt l, lrFirstSeen l) `shouldBe` (Active, Just 50, 100.5)
      (lrRunsActive l, lrHitsWhileActive l, lrAttemptsWhileActive l, lrHitsBefore l, lrRunsBefore l) `shouldBe` (["r5"], 3, 7, 2, 5)

    it "keeps the timestamps when the status does not change, and pins keeping an earlier activation" $ withLog $ \_ rl -> do
      i <- upsertLesson rl lesson
      setLessonStatus rl i Active 50
      setLessonStatus rl i Active 60
      fmap lrActivatedAt <$> getLesson rl i `shouldReturn` Just (Just 50)
      setLessonStatus rl i Pinned 70
      fmap (\x -> (lrStatus x, lrActivatedAt x, lrRetiredAt x)) <$> getLesson rl i `shouldReturn` Just (Pinned, Just 50, Nothing)
      k <- upsertLesson rl lesson {lrKey = "k:other"}
      setLessonStatus rl k Pinned 75
      fmap lrActivatedAt <$> getLesson rl k `shouldReturn` Just (Just 75)
      setLessonStatus rl i Retired 80
      setLessonStatus rl i Retired 90
      fmap lrRetiredAt <$> getLesson rl i `shouldReturn` Just (Just 80)
      setLessonStatus rl i Dropped 95
      setLessonStatus rl i Dropped 99
      fmap lrRetiredAt <$> getLesson rl i `shouldReturn` Just (Just 95)

    it "deletes every lesson of a project key, of any status, and answers how many" $ withLog $ \_ rl -> do
      a <- upsertLesson rl lesson {lrKey = "k:a"}
      b <- upsertLesson rl lesson {lrKey = "k:b"}
      setLessonStatus rl b Dropped 50
      g <- upsertLesson rl lesson {lrKey = "k:a", lrProjectKey = "*", lrStatus = Active}
      o <- upsertLesson rl lesson {lrKey = "k:a", lrProjectKey = "p2"}
      deleteLessons rl "p1" `shouldReturn` 2
      listLessons rl (Just "p1") [] `shouldReturn` []
      getLesson rl a `shouldReturn` Nothing
      map lrId <$> listLessons rl Nothing [] `shouldReturn` [g, o]
      deleteLessons rl "p1" `shouldReturn` 0
      -- the key can be stored again, as a fresh candidate
      c <- upsertLesson rl lesson {lrKey = "k:b"}
      fmap lrStatus <$> getLesson rl c `shouldReturn` Just Candidate

    it "renders a row as JSON with the status in lowercase" $ do
      let v = lessonRowJson lesson {lrId = 7, lrStatus = Pinned}
      field "status" v `shouldBe` Just (String "pinned")
      field "id" v `shouldBe` Just (Number 7)
      field "evidence_runs" v `shouldBe` Just (toJSONList ["r1"])
      field "agreed_attempts" v `shouldBe` Just (toJSONList ([] :: [T.Text]))

    it "sums the spend of a day, 0 for a day with none" $ withLog $ \_ rl -> do
      addSpend rl "2026-10-25" 0.25
      addSpend rl "2026-10-25" 0.5
      spendOn rl "2026-10-25" `shouldReturn` 0.75
      spendOn rl "2026-10-26" `shouldReturn` 0

    it "reads no lessons from an older runs.db opened read-only" $ withTmp $ \tmp -> do
      let p = tmp </> "old.db"
      bracket (open p) close $ \c ->
        execute_ c "CREATE TABLE runs (id TEXT PRIMARY KEY, kind TEXT NOT NULL)"
      (ls, one, found, spent) <- bracket (open ("file:" <> p <> "?mode=ro")) close $ \c -> do
        execute_ c "PRAGMA query_only = ON"
        rl <- runlogOn c
        (,,,) <$> listLessons rl Nothing [] <*> getLesson rl 1 <*> findLesson rl "p" "k" <*> spendOn rl "2026-10-25"
      (ls, one, found, spent) `shouldBe` ([], Nothing, Nothing, 0)

    it "reads lessons through a query_only connection" $ withTmp $ \tmp -> do
      let p = runlogPath tmp
      i <- withRunlog p (`upsertLesson` lesson)
      ls <- bracket (open ("file:" <> p <> "?mode=ro")) close $ \c -> do
        execute_ c "PRAGMA query_only = ON"
        rl <- runlogOn c
        listLessons rl (Just "p1") [Candidate]
      map lrId ls `shouldBe` [i]
  where
    toJSONList :: [T.Text] -> Value
    toJSONList = Array . foldMap (pure . String)
    toJSONList' :: [Value] -> Value
    toJSONList' = Array . foldMap pure
    shouldReturn' :: IO [T.Text] -> T.Text -> Expectation
    shouldReturn' act x = act >>= (`shouldSatisfy` elem x)
