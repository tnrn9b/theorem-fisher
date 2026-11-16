-- | 'recordOrchestrator': a run on real threads recorded and replayed
-- through the simulator ('recordingScript') gives the same events, a
-- cap that hit is a cap on replay, a price is given back, and the flat
-- search's recording ('AttemptScript') round-trips through JSON.
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

never :: Seconds
never = Seconds (1 / 0)

stats :: ChildStats
stats = ChildStats
  { csExit = Just 0, csWall = 1.5, csTurns = Just 7, csInputTokens = Just 1200, csOutputTokens = Nothing
  , csCostUsd = Just 0.25, csResult = Just "done", csTools = Map.fromList [("Edit", 3), ("Read", 4)] }

spec :: Spec
spec = describe "recordOrchestrator" $ do
  it "records a real run that replays through the simulator to the same events" $ do
    recRef <- newIORef Nothing
    let wrap o = do
          (o', rd) <- recorded o
          writeIORef recRef (Just rd)
          pure o'
    realEvs <- realSearch wrap identityFile identityConfig (emptyScript {scAttempts = identityEntries})
    rc <- readIORef recRef >>= maybe (fail "the wrapper was not used") id
    -- three attempts in spawn order; the last cut by the budget
    [k | (k, _, _) <- rcEntries rc] `shouldBe` [AttemptKey "a" "x" 0, AttemptKey "b" "y" 0, AttemptKey "b" "x" 0]
    [(k, r) | (k, s, r) <- rcEntries rc, s == never] `shouldBe` [(AttemptKey "b" "x" 0, Left "cancelled")]
    [s | (_, Seconds s, _) <- rcEntries rc, not (isInfinite s)] `shouldSatisfy` all (> 0)
    -- the replay runs in the recording's (real) seconds, so with the
    -- real run's budget
    (_, simEvs, tr) <- simSearch identityFile (scaled identityConfig) (recordingScript rc)
    map zeroTimes simEvs `shouldBe` map zeroTimes realEvs
    stCancelled tr `shouldSatisfy` (not . null)
    allNamed tr

  it "records a cap that hit, which is a cap on replay; a step that ran is not recorded" $ do
    (o, rd) <- quick Nothing >>= recorded
    oCapped o "auto" 0.05 (threadDelay 1000000) `shouldReturn` Nothing
    oCapped o "repair" 5 (pure (3 :: Int)) `shouldReturn` Just 3
    rc <- rd
    rcCaps rc `shouldBe` [("auto", 0.05)]
    (s, trace) <- simOrchestrator attemptKey (recordingScript rc)
    oCapped s "auto" 0.05 (pure ()) `shouldReturn` Nothing
    oCapped s "repair" 5 (pure (3 :: Int)) `shouldReturn` Just 3
    stClock <$> trace `shouldReturn` 0.05

  it "records a price given, which is given back on replay" $ do
    (o, rd) <- quick (Just 0.75) >>= recorded
    let sp = attemptOn 1 "a" "x"
    r <- oSpawn o sp
    oCancel o r
    oPrice o sp `shouldReturn` Just 0.75
    rc <- rd
    rcPrices rc `shouldBe` [(AttemptKey "a" "x" 0, 0.75)]
    rcEntries rc `shouldBe` [(AttemptKey "a" "x" 0, never, Left "cancelled")]
    (s, _) <- simOrchestrator attemptKey (recordingScript rc)
    r' <- oSpawn s sp
    oPrice s sp `shouldReturn` Just 0.75
    -- never due, so never delivered
    fmap fst <$> oAwaitAny s 100 [r'] `shouldReturn` Nothing

  it "an attempt script round-trips through JSON" $ do
    let full = (plainResult (Just "theorem a : A := by\n  simp") (Just stats) "closed")
          { arStop = Just (Stop StallDollars 3 1.5), arDollars = Just 0.5, arLastMessage = Just "stuck" }
        failed = (plainResult Nothing Nothing "") {arFailure = Just RateLimited, arStop = Just (Stop StallTurns 8 0)}
        rc = Recording
          { rcEntries =
              [ (AttemptKey "a" "x" 0, Seconds 2.5, Right full)
              , (AttemptKey "a" "y" 0, Seconds 0.25, Right failed)
              , (AttemptKey "b" "x" 0, Seconds 1, Left "boom")
              , (AttemptKey "b" "x" 1, never, Left "cancelled") ]
          , rcCaps = [("auto", 4)]
          , rcPrices = [(AttemptKey "b" "x" 1, 0.125)] } :: AttemptScript
    decode (encode rc) `shouldBe` Just rc
    encode never `shouldBe` "\"never\""
    encode (Seconds 2.5) `shouldBe` "2.5"
    decode "{\"akGoal\":\"a\",\"akBackend\":\"x\",\"akIndex\":2}" `shouldBe` Just (AttemptKey "a" "x" 2)
