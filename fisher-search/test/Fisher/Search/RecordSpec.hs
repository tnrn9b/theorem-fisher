-- | 'recordOrchestrator' and 'replayOrchestrator': a run on real threads
-- recorded as a tape and played back gives the same events exactly; every
-- event logged is on the tape and compared on replay; a cap replays as it
-- went (a hit one with the events it logged, one that ran under its
-- recorded seconds), a price as the caller used it; a replay asked for
-- anything else throws 'ReplayDiverged'; the flat search's tape
-- ('AttemptScript') round-trips through JSON, its version checked.
module Fisher.Search.RecordSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (ErrorCall (..), throwIO)
import Data.Aeson (decode, eitherDecode, encode)
import Data.Either (isLeft)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.List (isInfixOf)
import Data.Text (Text)
import Test.Hspec

import Fisher.Search.Child (ChildStats (..))
import Fisher.Search.Controller (ModelFailure (..), Stop (..), StopReason (..))
import Fisher.Search.Orchestrate
import Fisher.Search.Scheduler
import Fisher.Search.SimHarness
import Fisher.Search.Tape

-- | An attempt on a goal and backend, with its id.
attemptOn :: Int -> Text -> Text -> AttemptSpec
attemptOn i g b = AttemptSpec
  { asId = i, asGoal = g, asBackend = b, asStart = "start", asBrief = mempty, asModel = Nothing, asTier = Nothing }

-- | A real orchestrator whose attempts return at once, with this price
-- for every cancelled attempt.
quick :: Maybe Double -> IO AttemptOrchestrator
quick price = realOrchestrator (\_ -> pure (plainResult Nothing Nothing "")) (pure 0) (\_ -> pure ()) (\_ -> pure price)

-- | The recording orchestrator over a real one.
recorded :: AttemptOrchestrator -> IO (AttemptOrchestrator, IO AttemptScript)
recorded = recordOrchestrator attemptKey

-- | The replaying orchestrator, its log dropped.
replayed :: AttemptScript -> IO (AttemptOrchestrator, IO Int)
replayed = replayOrchestrator attemptKey (\_ -> pure ())

-- | The identity scenario on real threads, recorded: its events and tape.
recordIdentity :: IO ([Event], AttemptScript)
recordIdentity = do
  recRef <- newIORef Nothing
  let wrap o = do
        (o', rd) <- recorded o
        writeIORef recRef (Just rd)
        pure o'
  evs <- realSearch wrap identityFile identityConfig (emptyScript {scAttempts = identityEntries})
  rc <- readIORef recRef >>= maybe (fail "the wrapper was not used") id
  pure (evs, rc)

-- | The identity search replayed from a tape: its events and the steps
-- consumed.
replayIdentity :: AttemptScript -> IO ([Event], Int)
replayIdentity rc = do
  countRef <- newIORef (pure 0)
  evs <- searchOn (\logE -> do
    (o, n) <- replayOrchestrator attemptKey logE rc
    writeIORef countRef n
    pure o) identityFile identityConfig
  n <- join' countRef
  pure (evs, n)
  where
    join' r = readIORef r >>= id

diverged :: Selector ReplayDiverged
diverged = const True

stats :: ChildStats
stats = ChildStats
  { csExit = Just 0, csWall = 1.5, csTurns = Just 7, csInputTokens = Just 1200, csOutputTokens = Nothing
  , csCostUsd = Just 0.25, csResult = Just "done", csTools = Map.fromList [("Edit", 3), ("Read", 4)] }

spec :: Spec
spec = describe "recordOrchestrator and replayOrchestrator" $ do
  it "a real run's tape replays to the same events, times included" $ do
    (realEvs, rc) <- recordIdentity
    [k | Spawned k <- rcSteps rc] `shouldBe` [AttemptKey "a" "x" 0, AttemptKey "b" "y" 0, AttemptKey "b" "x" 0]
    [k | Cancelled k <- rcSteps rc] `shouldBe` [AttemptKey "b" "x" 0]
    (evs, n) <- replayIdentity rc
    evs `shouldBe` realEvs
    n `shouldBe` length (rcSteps rc)

  it "a replay asked for a call the tape does not have throws" $ do
    (_, rc) <- recordIdentity
    -- a's attempt fails on the tape now, so the replay retries a where
    -- the run went on with b
    let failA = \case
          Delivered secs k t _ | k == AttemptKey "a" "x" 0 -> Delivered secs k t (Left "boom")
          st -> st
    replayIdentity rc {rcSteps = map failA (rcSteps rc)} `shouldThrow` diverged
    (o, n) <- replayed (Recording tapeVersion [Now 1])
    oSpawn o (attemptOn 1 "a" "x") `shouldThrow` (== ReplayDiverged 0 "now 1.0" "spawn a/x/0")
    n `shouldReturn` 0
    oNow o `shouldReturn` 1
    oNow o `shouldThrow` (== ReplayDiverged 1 "the end of the tape" "now")
    -- the seconds are checked too
    (c, _) <- replayed (Recording tapeVersion [Capped "auto" 2 True []])
    oCapped c "auto" 3 (pure ()) `shouldThrow` (== ReplayDiverged 0 "capped auto 2.0 (hit)" "capped auto 3.0")

  it "a cap that hit replays without running the step; one that ran runs again" $ do
    (o, rd) <- quick Nothing >>= recorded
    oCapped o "auto" 0.05 (threadDelay 1000000) `shouldReturn` Nothing
    oCapped o "repair" 5 (pure (3 :: Int)) `shouldReturn` Just 3
    rc <- rd
    rcSteps rc `shouldBe` [Capped "auto" 0.05 True [], Capped "repair" 5 False []]
    (s, n) <- replayed rc
    ran <- newIORef False
    oCapped s "auto" 0.05 (writeIORef ran True) `shouldReturn` Nothing
    readIORef ran `shouldReturn` False
    oCapped s "repair" 5 (writeIORef ran True >> pure (3 :: Int)) `shouldReturn` Just 3
    readIORef ran `shouldReturn` True
    n `shouldReturn` 2

  it "a cap that hit drops the steps its action took" $ do
    clock <- newIORef (0 :: Double)
    let tick = atomicModifyIORef' clock (\t -> (t + 1, t + 1))
    o0 <- realOrchestrator (\_ -> pure (plainResult Nothing Nothing "")) tick (\_ -> pure ()) (\_ -> pure Nothing)
    (o, rd) <- recorded o0
    oNow o `shouldReturn` 1
    oCapped o "repair" 0.05 (oNow o >> oNow o >> threadDelay 1000000) `shouldReturn` Nothing
    oNow o `shouldReturn` 4
    rc <- rd
    rcSteps rc `shouldBe` [Now 1, Capped "repair" 0.05 True [], Now 4]
    (s, n) <- replayed rc
    ran <- newIORef False
    oNow s `shouldReturn` 1
    oCapped s "repair" 0.05 (writeIORef ran True) `shouldReturn` Nothing
    readIORef ran `shouldReturn` False
    oNow s `shouldReturn` 4
    n `shouldReturn` 3

  it "every logged event is on the tape; a cap that hit carries the events its step logged" $ do
    (o, rd) <- quick Nothing >>= recorded
    oLog o (BudgetSpent 0)
    oCapped o "auto" 0.05 (oLog o (AutoTimeout 1 "g" "auto") >> threadDelay 1000000) `shouldReturn` Nothing
    oCapped o "repair" 5 (oLog o (Repaired 2 1 "g" 2 1 1)) `shouldReturn` Just ()
    rc <- rd
    -- the hit step's events inline; the ran step's follow it as Logged steps
    rcSteps rc `shouldBe`
      [ Logged (BudgetSpent 0), Capped "auto" 0.05 True [AutoTimeout 1 "g" "auto"], Capped "repair" 5 False []
      , Logged (Repaired 2 1 "g" 2 1 1) ]
    logged <- newIORef []
    (s, n) <- replayOrchestrator attemptKey (\e -> modifyIORef' logged (e :)) rc
    ran <- newIORef False
    oLog s (BudgetSpent 0)
    oCapped s "auto" 0.05 (writeIORef ran True) `shouldReturn` Nothing
    readIORef ran `shouldReturn` False
    reverse <$> readIORef logged `shouldReturn` [BudgetSpent 0, AutoTimeout 1 "g" "auto"]
    oCapped s "repair" 5 (oLog s (Repaired 2 1 "g" 2 1 1)) `shouldReturn` Just ()
    reverse <$> readIORef logged `shouldReturn` [BudgetSpent 0, AutoTimeout 1 "g" "auto", Repaired 2 1 "g" 2 1 1]
    n `shouldReturn` 4

  it "a logged event other than the tape's throws, naming both, and is not logged" $ do
    logged <- newIORef []
    (s, n) <- replayOrchestrator attemptKey (\e -> modifyIORef' logged (e :)) (Recording tapeVersion [Logged (BudgetSpent 0)])
    oLog s (BudgetSpent 1) `shouldThrow` (== ReplayDiverged 0 "log BudgetSpent 0.0" "log BudgetSpent 1.0")
    readIORef logged `shouldReturn` []
    n `shouldReturn` 0
    -- an event where the tape has another kind of step
    (c, _) <- replayed (Recording tapeVersion [Now 1])
    oLog c (BudgetSpent 0) `shouldThrow` (== ReplayDiverged 0 "now 1.0" "log BudgetSpent 0.0")

  it "a cap that hit after a nested cap ran carries every event logged inside it" $ do
    (o, rd) <- quick Nothing >>= recorded
    let outer = do
          _ <- oCapped o "inner" 5 (oLog o (AutoTimeout 1 "g" "inner"))
          oLog o (AutoTimeout 2 "g" "outer")
          threadDelay 1000000
    oCapped o "outer" 0.1 outer `shouldReturn` Nothing
    oNow o `shouldReturn` 0
    rc <- rd
    rcSteps rc `shouldBe` [Capped "outer" 0.1 True [AutoTimeout 1 "g" "inner", AutoTimeout 2 "g" "outer"], Now 0]
    logged <- newIORef []
    (s, n) <- replayOrchestrator attemptKey (\e -> modifyIORef' logged (e :)) rc
    ran <- newIORef False
    oCapped s "outer" 0.1 (writeIORef ran True) `shouldReturn` Nothing
    readIORef ran `shouldReturn` False
    reverse <$> readIORef logged `shouldReturn` [AutoTimeout 1 "g" "inner", AutoTimeout 2 "g" "outer"]
    oNow s `shouldReturn` 0
    n `shouldReturn` 2

  it "a cap that ran around a nested cap that hit replays both" $ do
    (o, rd) <- quick Nothing >>= recorded
    let outer = do
          _ <- oCapped o "inner" 0.05 (oLog o (AutoTimeout 1 "g" "inner") >> threadDelay 1000000)
          oLog o (AutoTimeout 2 "g" "outer")
    oCapped o "outer" 5 outer `shouldReturn` Just ()
    rc <- rd
    rcSteps rc `shouldBe`
      [Capped "outer" 5 False [], Capped "inner" 0.05 True [AutoTimeout 1 "g" "inner"], Logged (AutoTimeout 2 "g" "outer")]
    logged <- newIORef []
    (s, n) <- replayOrchestrator attemptKey (\e -> modifyIORef' logged (e :)) rc
    let outer' = do
          _ <- oCapped s "inner" 0.05 (fail "the hit step ran")
          oLog s (AutoTimeout 2 "g" "outer")
    oCapped s "outer" 5 outer' `shouldReturn` Just ()
    reverse <$> readIORef logged `shouldReturn` [AutoTimeout 1 "g" "inner", AutoTimeout 2 "g" "outer"]
    n `shouldReturn` 3

  it "a step that ran and does not end within its recorded seconds on replay throws" $ do
    (s, _) <- replayed (Recording tapeVersion [Capped "repair" 0.05 False []])
    oCapped s "repair" 0.05 (threadDelay 2000000) `shouldThrow`
      (== ReplayDiverged 0 "capped repair 5.0e-2 (ran)" "the step `repair` did not end within its recorded cap")

  it "a price answered Nothing replays as Nothing" $ do
    (o, rd) <- quick Nothing >>= recorded
    let sp = attemptOn 1 "a" "x"
    r <- oSpawn o sp
    oCancel o r
    oPrice o sp `shouldReturn` Nothing
    rc <- rd
    rcSteps rc `shouldBe` [Spawned (AttemptKey "a" "x" 0), Cancelled (AttemptKey "a" "x" 0), Priced (AttemptKey "a" "x" 0) Nothing]
    (s, n) <- replayed rc
    r' <- oSpawn s sp
    oCancel s r'
    oPrice s sp `shouldReturn` Nothing
    n `shouldReturn` 3

  it "a price that throws is recorded as the Nothing its caller used, and replays" $ do
    o0 <- realOrchestrator (\_ -> pure (plainResult Nothing Nothing "")) (pure 0) (\_ -> pure ()) (\_ -> throwIO (ErrorCall "no ledger"))
    (o, rd) <- recorded o0
    let sp = attemptOn 1 "a" "x"
        k = AttemptKey "a" "x" 0
    r <- oSpawn o sp
    oCancel o r
    priceCancelled True o sp `shouldReturn` Nothing
    rc <- rd
    rcSteps rc `shouldBe` [Spawned k, Cancelled k, Priced k Nothing]
    (s, n) <- replayed rc
    r' <- oSpawn s sp
    oCancel s r'
    priceCancelled True s sp `shouldReturn` Nothing
    n `shouldReturn` 3

  it "a tape round-trips through JSON" $ do
    let full = (plainResult (Just "theorem a : A := by\n  simp") (Just stats) "closed")
          { arStop = Just (Stop StallDollars 3 1.5), arDollars = Just 0.5, arLastMessage = Just "stuck" }
        failed = (plainResult Nothing Nothing "") {arFailure = Just RateLimited, arStop = Just (Stop StallTurns 8 0)}
        a = AttemptKey "a" "x" 0
        b = AttemptKey "b" "y" 0
        rc = Recording tapeVersion
          [ Now 0.5, Spawned a, Spawned b, Delivered 8 a 2.5 (Right full), Delivered 5.5 b 3 (Right failed)
          , AwaitTimedOut 1 4, Delivered 4 b 4.25 (Left "boom"), Cancelled b, Capped "auto" 4 True [AutoTimeout 1 "g" "auto"], Capped "repair" 2 False []
          , Priced a (Just 0.125), Priced b Nothing, Logged (AutoTimeout 1 "g" "auto")
          , Logged (Finished 3 2 "b" "y" "failed" (Just stats) (Just 0.5)) ] :: AttemptScript
    decode (encode rc) `shouldBe` Just rc
    decode "{\"akGoal\":\"a\",\"akBackend\":\"x\",\"akIndex\":2}" `shouldBe` Just (AttemptKey "a" "x" 2)

  it "a tape of another version, or of none, does not decode, naming the version" $ do
    let other = eitherDecode "{\"rcVersion\":2,\"rcSteps\":[]}" :: Either String AttemptScript
    either id (const "") other `shouldSatisfy` isInfixOf "a tape of version 2"
    (eitherDecode "{\"rcSteps\":[]}" :: Either String AttemptScript) `shouldSatisfy` isLeft
    (eitherDecode "{\"rcVersion\":1,\"rcSteps\":[]}" :: Either String AttemptScript) `shouldBe` Right (Recording 1 [])
    tapeVersion `shouldBe` 1
