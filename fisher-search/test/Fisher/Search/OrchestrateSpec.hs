module Fisher.Search.OrchestrateSpec (spec) where

import Control.Concurrent (forkIO, myThreadId, threadDelay, throwTo)
import Control.Concurrent.MVar
import Control.Exception (AsyncException (..), ErrorCall (..), SomeException, displayException, throwIO, try)
import Data.IORef
import Data.Text (Text)
import GHC.Clock (getMonotonicTime)
import Test.Hspec

import Fisher.Search.Orchestrate
import Fisher.Search.Scheduler (AttemptResult (..), AttemptSpec (..), plainResult)

-- | An attempt on a goal, with its id.
attempt :: Int -> Text -> AttemptSpec
attempt i g = AttemptSpec
  { asId = i, asGoal = g, asBackend = "x", asStart = "start", asBrief = mempty, asModel = Nothing, asTier = Nothing }

-- | The goal @fast@ takes 10 ms and @slow@ 200 ms; each records its goal
-- when it finishes and returns it as its note.
runner :: IORef [Text] -> AttemptSpec -> IO AttemptResult
runner finished sp = do
  threadDelay (if asGoal sp == "fast" then 10000 else 200000)
  atomicModifyIORef' finished (\xs -> (asGoal sp : xs, ()))
  pure (plainResult Nothing Nothing (asGoal sp))

orchestrator :: IO (Orchestrator, IORef [Text])
orchestrator = do
  finished <- newIORef []
  o <- realOrchestrator (runner finished) (pure 0) (\_ -> pure ()) (\_ -> pure Nothing)
  pure (o, finished)

note :: Maybe (Running, Either SomeException AttemptResult) -> Maybe (Running, Either String Text)
note = fmap (\(r, e) -> (r, either (Left . show) (Right . arNote) e))

-- | The action's result and the seconds it took.
timed :: IO a -> IO (a, Double)
timed act = do
  t0 <- getMonotonicTime
  x <- act
  t1 <- getMonotonicTime
  pure (x, t1 - t0)

spec :: Spec
spec = describe "realOrchestrator" $ do
  it "await the first of two" $ do
    (o, _) <- orchestrator
    slow <- oSpawn o (attempt 1 "slow")
    fast <- oSpawn o (attempt 2 "fast")
    (got, secs) <- timed (oAwaitAny o 5 [slow, fast])
    note got `shouldBe` Just (fast, Right "fast")
    secs `shouldSatisfy` (< 0.19)
    -- the slow one is still there and is delivered next
    note <$> oAwaitAny o 5 [slow, fast] `shouldReturn` Just (slow, Right "slow")

  it "cancel the other" $ do
    (o, finished) <- orchestrator
    slow <- oSpawn o (attempt 1 "slow")
    fast <- oSpawn o (attempt 2 "fast")
    note <$> oAwaitAny o 5 [slow, fast] `shouldReturn` Just (fast, Right "fast")
    oCancel o slow
    threadDelay 300000
    readIORef finished `shouldReturn` ["fast"]
    -- the cancelled one is forgotten: awaiting it returns at once
    (got, secs) <- timed (oAwaitAny o 5 [slow])
    note got `shouldBe` Nothing
    secs `shouldSatisfy` (< 0.1)
    -- cancelling again, or cancelling one already delivered, is harmless
    oCancel o slow
    oCancel o fast
    readIORef finished `shouldReturn` ["fast"]

  it "a cap that hits returns Nothing" $ do
    (o, _) <- orchestrator
    (r, secs) <- timed (oCapped o "auto" 0.02 (threadDelay 2000000 >> pure 'x'))
    r `shouldBe` Nothing
    secs `shouldSatisfy` (< 1)
    oCapped o "auto" 1 (pure (3 :: Int)) `shouldReturn` Just 3

  it "a cancelled real attempt is never delivered" $ do
    (o, finished) <- orchestrator
    slow <- oSpawn o (attempt 1 "slow")
    oCancel o slow
    (got, secs) <- timed (oAwaitAny o 5 [slow])
    note got `shouldBe` Nothing
    secs `shouldSatisfy` (< 0.1)
    threadDelay 300000
    readIORef finished `shouldReturn` []

  it "zero seconds delivers an attempt already finished" $ do
    (o, finished) <- orchestrator
    fast <- oSpawn o (attempt 1 "fast")
    threadDelay 100000
    readIORef finished `shouldReturn` ["fast"]
    note <$> oAwaitAny o 0 [fast] `shouldReturn` Just (fast, Right "fast")
    -- delivered, so it is gone
    note <$> oAwaitAny o 0 [fast] `shouldReturn` Nothing

  it "attempts already finished are delivered in list order" $ do
    (o, _) <- orchestrator
    a <- oSpawn o (attempt 1 "fast")
    b <- oSpawn o (attempt 2 "fast")
    threadDelay 100000
    fmap fst <$> oAwaitAny o 0 [b, a] `shouldReturn` Just b
    fmap fst <$> oAwaitAny o 0 [b, a] `shouldReturn` Just a

  it "await on no attempts returns at once" $ do
    (o, _) <- orchestrator
    (got, secs) <- timed (oAwaitAny o 100 [])
    note got `shouldBe` Nothing
    secs `shouldSatisfy` (< 1)

  it "capped rethrows async" $ do
    (o, _) <- orchestrator
    me <- myThreadId
    started <- newEmptyMVar
    _ <- forkIO (takeMVar started >> throwTo me UserInterrupt)
    (r, secs) <- timed (try (oCapped o "auto" 5 (putMVar started () >> threadDelay 5000000)))
    r `shouldBe` (Left UserInterrupt :: Either AsyncException (Maybe ()))
    secs `shouldSatisfy` (< 2)

  it "an attempt that throws is delivered as a Left" $ do
    o <- realOrchestrator (\_ -> threadDelay 10000 >> throwIO (ErrorCall "boom")) (pure 0) (\_ -> pure ()) (\_ -> pure Nothing)
    r <- oSpawn o (attempt 1 "fast")
    got <- oAwaitAny o 5 [r]
    fmap (\(r', e) -> (r', either (Left . displayException) (Right . arNote) e)) got
      `shouldBe` Just (r, Left "boom")

  -- the Running constructor is hidden, so an attempt cancelled twice
  -- stands in for one the orchestrator never issued
  it "cancelling an attempt twice is a no-op" $ do
    (o, finished) <- orchestrator
    slow <- oSpawn o (attempt 1 "slow")
    oCancel o slow
    (_, secs) <- timed (oCancel o slow)
    secs `shouldSatisfy` (< 0.1)
    (got, secs') <- timed (oAwaitAny o 5 [slow])
    note got `shouldBe` Nothing
    secs' `shouldSatisfy` (< 0.1)
    readIORef finished `shouldReturn` []

  it "a price comes from the given function" $ do
    o <- realOrchestrator (\_ -> pure (plainResult Nothing Nothing "")) (pure 0) (\_ -> pure ())
      (\sp -> pure (if asGoal sp == "priced" then Just (fromIntegral (asId sp) / 4) else Nothing))
    oPrice o (attempt 3 "priced") `shouldReturn` Just 0.75
    oPrice o (attempt 3 "free") `shouldReturn` Nothing
