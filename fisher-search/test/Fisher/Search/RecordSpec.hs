-- | 'recordOrchestrator' and 'replayOrchestrator': a run on real threads
-- recorded as a tape and played back gives the same events exactly; a
-- cap replays as it went, a price as it was answered; a replay asked for
-- anything else throws 'ReplayDiverged'; the flat search's tape
-- ('AttemptScript') round-trips through JSON.
module Fisher.Search.RecordSpec (spec) where

import Control.Concurrent (threadDelay)
import Data.Aeson (decode, encode)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Test.Hspec

import Fisher.Search.Child (ChildStats (..))
import Fisher.Search.Controller (ModelFailure (..), Stop (..), StopReason (..))
import Fisher.Search.Orchestrate
import Fisher.Search.Scheduler
import Fisher.Search.SimHarness

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
          Delivered k t _ | k == AttemptKey "a" "x" 0 -> Delivered k t (Left "boom")
          st -> st
    replayIdentity (Recording (map failA (rcSteps rc))) `shouldThrow` diverged
    (o, n) <- replayed (Recording [Now 1])
    oSpawn o (attemptOn 1 "a" "x") `shouldThrow` diverged
    n `shouldReturn` 0
    oNow o `shouldReturn` 1
    oNow o `shouldThrow` diverged

  it "a cap that hit replays without running the step; one that ran runs again" $ do
    (o, rd) <- quick Nothing >>= recorded
    oCapped o "auto" 0.05 (threadDelay 1000000) `shouldReturn` Nothing
    oCapped o "repair" 5 (pure (3 :: Int)) `shouldReturn` Just 3
    rc <- rd
    rcSteps rc `shouldBe` [Capped "auto" 0.05 True, Capped "repair" 5 False]
    (s, n) <- replayed rc
    ran <- newIORef False
    oCapped s "auto" 0.05 (writeIORef ran True) `shouldReturn` Nothing
    readIORef ran `shouldReturn` False
    oCapped s "repair" 5 (writeIORef ran True >> pure (3 :: Int)) `shouldReturn` Just 3
    readIORef ran `shouldReturn` True
    n `shouldReturn` 2

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

  it "a tape round-trips through JSON" $ do
    let full = (plainResult (Just "theorem a : A := by\n  simp") (Just stats) "closed")
          { arStop = Just (Stop StallDollars 3 1.5), arDollars = Just 0.5, arLastMessage = Just "stuck" }
        failed = (plainResult Nothing Nothing "") {arFailure = Just RateLimited, arStop = Just (Stop StallTurns 8 0)}
        a = AttemptKey "a" "x" 0
        b = AttemptKey "b" "y" 0
        rc = Recording
          [ Now 0.5, Spawned a, Spawned b, Delivered a 2.5 (Right full), Delivered b 3 (Right failed)
          , AwaitTimedOut 4, Delivered b 4.25 (Left "boom"), Cancelled b, Capped "auto" 4 True, Capped "repair" 2 False
          , Priced a (Just 0.125), Priced b Nothing ] :: AttemptScript
    decode (encode rc) `shouldBe` Just rc
    decode "{\"akGoal\":\"a\",\"akBackend\":\"x\",\"akIndex\":2}" `shouldBe` Just (AttemptKey "a" "x" 2)
