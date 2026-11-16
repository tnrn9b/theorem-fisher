module Fisher.Search.SimOrchestrateSpec (spec) where

import Control.Concurrent (myThreadId)
import Control.Exception (AsyncException (..), ErrorCall (..), SomeException, displayException, throwIO)
import Data.IORef
import Data.List (isInfixOf)
import Data.Text (Text)
import Test.Hspec

import Fisher.Search.Orchestrate
import Fisher.Search.Scheduler (AttemptResult (..), AttemptSpec (..), Event (..), plainResult)

-- | An attempt on a goal and backend, with its id.
attempt :: Int -> Text -> Text -> AttemptSpec
attempt i g b = AttemptSpec
  { asId = i, asGoal = g, asBackend = b, asStart = "start", asBrief = mempty, asModel = Nothing, asTier = Nothing }

-- | A script entry that finishes with this note.
done :: Text -> Entry AttemptSpec AttemptResult
done n = Scripted (\_ -> pure (Right (plainResult Nothing Nothing n)))

-- | A script entry that throws with this text.
boom :: Text -> Entry AttemptSpec AttemptResult
boom t = Scripted (\_ -> pure (Left t))

-- | The simulator over attempts.
sim :: Script AttemptSpec AttemptResult -> IO (AttemptOrchestrator, IO (SimTrace AttemptSpec))
sim = simOrchestrator attemptKey

key :: Text -> Text -> Int -> AttemptKey
key = AttemptKey

-- | What an await delivered: the attempt and its note or exception text.
outcome :: Maybe (Running, Either SomeException AttemptResult) -> Maybe (Running, Either String Text)
outcome = fmap (\(r, e) -> (r, either (Left . displayException) (Right . arNote) e))

spec :: Spec
spec = describe "simOrchestrator" $ do
  it "spawn then await returns the scripted result and moves the clock" $ do
    (o, _) <- sim emptyScript {scAttempts = [(key "a" "x" 0, 5, done "ok")]}
    r <- oSpawn o (attempt 1 "a" "x")
    oNow o `shouldReturn` 0
    got <- oAwaitAny o 10 [r]
    outcome got `shouldBe` Just (r, Right "ok")
    oNow o `shouldReturn` 5

  it "await past the deadline returns Nothing and moves the clock to the deadline" $ do
    (o, _) <- sim emptyScript {scAttempts = [(key "a" "x" 0, 12, done "late")]}
    r <- oSpawn o (attempt 1 "a" "x")
    got <- oAwaitAny o 10 [r]
    outcome got `shouldBe` Nothing
    oNow o `shouldReturn` 10
    got' <- oAwaitAny o 5 [r]
    outcome got' `shouldBe` Just (r, Right "late")
    oNow o `shouldReturn` 12

  it "an attempt due just after the deadline is not delivered even with a step cost" $ do
    (o, _) <- sim emptyScript {scAttempts = [(key "a" "x" 0, 10.5, done "late")], scStepCost = 1}
    r <- oSpawn o (attempt 1 "a" "x")
    outcome <$> oAwaitAny o 10 [r] `shouldReturn` Nothing
    -- a timed out await stops at the deadline, with no step cost
    oNow o `shouldReturn` 10
    -- a delivery charges the step cost after the due time
    outcome <$> oAwaitAny o 5 [r] `shouldReturn` Just (r, Right "late")
    oNow o `shouldReturn` 11.5
    -- with nothing in flight an await returns at once and charges nothing
    outcome <$> oAwaitAny o 5 [r] `shouldReturn` Nothing
    oNow o `shouldReturn` 11.5

  it "negative seconds never move the clock backwards" $ do
    (o, _) <- sim emptyScript {scAttempts = [(key "a" "x" 0, 3, done "ok")], scCaps = [("disprove", 2)]}
    r <- oSpawn o (attempt 1 "a" "x")
    outcome <$> oAwaitAny o (-5) [r] `shouldReturn` Nothing
    oNow o `shouldReturn` 0
    oCapped o "disprove" (-5) (pure 'x') `shouldReturn` Nothing
    oNow o `shouldReturn` 0

  it "two attempts due together" $ do
    (o, _) <- sim emptyScript
      {scAttempts = [(key "a" "x" 0, 3, done "first"), (key "b" "y" 0, 3, done "second")]}
    ra <- oSpawn o (attempt 1 "a" "x")
    rb <- oSpawn o (attempt 2 "b" "y")
    -- list order, as the real side delivers, not spawn order
    outcome <$> oAwaitAny o 10 [rb, ra] `shouldReturn` Just (rb, Right "second")
    outcome <$> oAwaitAny o 10 [rb, ra] `shouldReturn` Just (ra, Right "first")
    oNow o `shouldReturn` 3
    -- both are delivered, so nothing is left to wait for
    outcome <$> oAwaitAny o 10 [rb, ra] `shouldReturn` Nothing
    oNow o `shouldReturn` 3

  it "two attempts already past due are delivered in list order" $ do
    (o, _) <- sim emptyScript
      { scAttempts = [(key "a" "x" 0, 1, done "early"), (key "b" "x" 0, 2, done "later"), (key "c" "x" 0, 9, done "c")]
      , scCaps = [("step", 5)] }
    ra <- oSpawn o (attempt 1 "a" "x")
    rb <- oSpawn o (attempt 2 "b" "x")
    -- a capped step moves the clock to 5, past both due times
    oCapped o "step" 10 (pure ()) `shouldReturn` Nothing
    oNow o `shouldReturn` 5
    -- the later due comes first because it is listed first, as the real
    -- side's poll delivers, and the clock stays
    outcome <$> oAwaitAny o 0 [rb, ra] `shouldReturn` Just (rb, Right "later")
    oNow o `shouldReturn` 5
    outcome <$> oAwaitAny o 0 [rb, ra] `shouldReturn` Just (ra, Right "early")
    oNow o `shouldReturn` 5
    -- with none past due, the earliest to come is waited for
    rc <- oSpawn o (attempt 3 "c" "x")
    outcome <$> oAwaitAny o 20 [rc] `shouldReturn` Just (rc, Right "c")
    oNow o `shouldReturn` 14

  it "a due attempt at deadline zero" $ do
    (o, _) <- sim emptyScript
      {scAttempts = [(key "a" "x" 0, 0, done "now"), (key "b" "x" 0, 4, done "edge")]}
    r <- oSpawn o (attempt 1 "a" "x")
    outcome <$> oAwaitAny o 0 [r] `shouldReturn` Just (r, Right "now")
    oNow o `shouldReturn` 0
    -- due exactly at the deadline is delivered (at or before)
    r' <- oSpawn o (attempt 2 "b" "x")
    outcome <$> oAwaitAny o 4 [r'] `shouldReturn` Just (r', Right "edge")
    oNow o `shouldReturn` 4

  it "cancel removes an attempt and records it" $ do
    (o, trace) <- sim emptyScript
      {scAttempts = [(key "a" "x" 0, 3, done "cancelled"), (key "a" "y" 0, 5, done "kept")]}
    r1 <- oSpawn o (attempt 1 "a" "x")
    r2 <- oSpawn o (attempt 2 "a" "y")
    oCancel o r1
    outcome <$> oAwaitAny o 10 [r1, r2] `shouldReturn` Just (r2, Right "kept")
    oNow o `shouldReturn` 5
    stCancelled <$> trace `shouldReturn` [r1]

  it "cancelling an attempt after it was delivered is not recorded" $ do
    (o, trace) <- sim emptyScript {scAttempts = [(key "a" "x" 0, 2, done "ok")]}
    r <- oSpawn o (attempt 1 "a" "x")
    outcome <$> oAwaitAny o 10 [r] `shouldReturn` Just (r, Right "ok")
    oCancel o r
    stCancelled <$> trace `shouldReturn` []

  it "a cancelled attempt that would have thrown" $ do
    (o, _) <- sim emptyScript
      {scAttempts = [(key "a" "x" 0, 1, boom "boom"), (key "b" "x" 0, 2, boom "bang")]}
    r1 <- oSpawn o (attempt 1 "a" "x")
    oCancel o r1
    outcome <$> oAwaitAny o 10 [r1] `shouldReturn` Nothing
    -- one not cancelled delivers its exception as a Left
    r2 <- oSpawn o (attempt 2 "b" "x")
    outcome <$> oAwaitAny o 10 [r1, r2] `shouldReturn` Just (r2, Left "bang")
    oNow o `shouldReturn` 2

  it "a capped step does not run and charges its seconds" $ do
    (o, _) <- sim emptyScript {scCaps = [("disprove", 3)], scStepCost = 1}
    ran <- newIORef False
    oCapped o "disprove" 10 (writeIORef ran True >> pure 'x') `shouldReturn` Nothing
    readIORef ran `shouldReturn` False
    oNow o `shouldReturn` 3
    -- a cap shorter than the listed seconds charges the cap
    oCapped o "disprove" 2 (writeIORef ran True >> pure 'y') `shouldReturn` Nothing
    readIORef ran `shouldReturn` False
    oNow o `shouldReturn` 5

  it "an unlisted step runs and charges scStepCost" $ do
    (o, _) <- sim emptyScript {scCaps = [("disprove", 3)], scStepCost = 0.5}
    oCapped o "auto" 10 (pure (7 :: Int)) `shouldReturn` Just 7
    oNow o `shouldReturn` 0.5

  it "an unscripted attempt throws at spawn with its key" $ do
    (o, trace) <- sim emptyScript {scAttempts = [(key "a" "x" 0, 1, done "once")]}
    r <- oSpawn o (attempt 1 "a" "x")
    -- a second spawn on the same goal and backend is index 1, not scripted
    let named (ErrorCall m) = all (`isInfixOf` m) ["akGoal = \"a\"", "akBackend = \"x\"", "akIndex = 1"]
    oSpawn o (attempt 2 "a" "x") `shouldThrow` named
    let other (ErrorCall m) = all (`isInfixOf` m) ["akGoal = \"b\"", "akIndex = 0"]
    oSpawn o (attempt 3 "b" "x") `shouldThrow` other
    stSpawned <$> trace `shouldReturn` [attempt 1 "a" "x"]
    outcome <$> oAwaitAny o 10 [r] `shouldReturn` Just (r, Right "once")

  it "scDefault covers an unscripted attempt" $ do
    (o, trace) <- sim emptyScript
      { scAttempts = [(key "a" "x" 0, 2, done "scripted")]
      , scDefault = Just (Scripted (\s -> pure (Right (plainResult Nothing Nothing (asGoal s <> "/" <> asBackend s)))))
      }
    r1 <- oSpawn o (attempt 1 "a" "x")
    r2 <- oSpawn o (attempt 2 "a" "x")
    -- the default takes no virtual time
    outcome <$> oAwaitAny o 10 [r1, r2] `shouldReturn` Just (r2, Right "a/x")
    oNow o `shouldReturn` 0
    outcome <$> oAwaitAny o 10 [r1] `shouldReturn` Just (r1, Right "scripted")
    oNow o `shouldReturn` 2
    -- the trace names the attempt that fell to the default
    stDefaulted <$> trace `shouldReturn` [attempt 2 "a" "x"]

  it "prices come from the script" $ do
    (o, _) <- sim emptyScript
      { scAttempts = [(key "a" "x" 0, 1, done "p"), (key "a" "x" 1, 1, done "q")]
      , scPrices = [(key "a" "x" 0, 0.25)]
      }
    _ <- oSpawn o (attempt 1 "a" "x")
    _ <- oSpawn o (attempt 2 "a" "x")
    oPrice o (attempt 1 "a" "x") `shouldReturn` Just 0.25
    let unpriced (ErrorCall m) = all (`isInfixOf` m) ["akGoal = \"a\"", "akIndex = 1"]
    oPrice o (attempt 2 "a" "x") `shouldThrow` unpriced

  it "the trace holds spawns, cancels, events and the clock" $ do
    (o, trace) <- sim emptyScript
      {scAttempts = [(key "a" "x" 0, 4, done "a"), (key "b" "x" 0, 9, done "b"), (key "c" "x" 0, 9, done "c")]}
    let s1 = attempt 1 "a" "x"
        s2 = attempt 2 "b" "x"
        s3 = attempt 3 "c" "x"
    r1 <- oSpawn o s1
    r2 <- oSpawn o s2
    r3 <- oSpawn o s3
    oLog o (Launched 0 1 "a" "x")
    _ <- oAwaitAny o 10 [r1, r2, r3]
    oCancel o r3
    oCancel o r2
    oLog o (BudgetSpent 4)
    t <- trace
    stSpawned t `shouldBe` [s1, s2, s3]
    stDefaulted t `shouldBe` []
    stCancelled t `shouldBe` [r3, r2]
    stEvents t `shouldBe` [Launched 0 1 "a" "x", BudgetSpent 4]
    stClock t `shouldBe` 4

  it "a Run entry runs at spawn on the caller's thread and is delivered after its seconds" $ do
    ran <- newIORef Nothing
    let job sp = do
          tid <- myThreadId
          writeIORef ran (Just tid)
          pure (plainResult Nothing Nothing ("ran " <> asGoal sp))
    (o, _) <- sim emptyScript {scAttempts = [(key "a" "x" 0, 6, Run job)]}
    me <- myThreadId
    r <- oSpawn o (attempt 1 "a" "x")
    -- the action has run, on this thread, before any await
    readIORef ran `shouldReturn` Just me
    oNow o `shouldReturn` 0
    outcome <$> oAwaitAny o 5 [r] `shouldReturn` Nothing
    oNow o `shouldReturn` 5
    outcome <$> oAwaitAny o 5 [r] `shouldReturn` Just (r, Right "ran a")
    oNow o `shouldReturn` 6

  it "a Run entry's exception is a Left delivered at its time" $ do
    (o, _) <- sim emptyScript {scAttempts = [(key "a" "x" 0, 3, Run (\_ -> ioError (userError "boom")))]}
    r <- oSpawn o (attempt 1 "a" "x")
    outcome <$> oAwaitAny o 2 [r] `shouldReturn` Nothing
    outcome <$> oAwaitAny o 10 [r] `shouldReturn` Just (r, Left "user error (boom)")
    oNow o `shouldReturn` 3

  it "a Run entry's asynchronous exception is rethrown at spawn" $ do
    (o, trace) <- sim emptyScript
      {scAttempts = [(key "a" "x" 0, 3, Run (\_ -> throwIO ThreadKilled)), (key "a" "x" 1, 20, done "later")]}
    oSpawn o (attempt 1 "a" "x") `shouldThrow` (== ThreadKilled)
    -- the job counts as spawned (the next on its key is index 1) and is
    -- not cancelled
    r <- oSpawn o (attempt 2 "a" "x")
    t <- trace
    stSpawned t `shouldBe` [attempt 1 "a" "x", attempt 2 "a" "x"]
    stCancelled t `shouldBe` []
    -- nothing is due for it: past its 3 seconds the await delivers nothing
    -- and stops at the deadline (its handle never reached the caller, so
    -- the await is over the later job)
    outcome <$> oAwaitAny o 5 [r] `shouldReturn` Nothing
    oNow o `shouldReturn` 5

  it "a Scripted function that throws is a Left delivered at its time" $ do
    (o, _) <- sim emptyScript {scAttempts = [(key "a" "x" 0, 2, Scripted (\_ -> ioError (userError "bang")))]}
    r <- oSpawn o (attempt 1 "a" "x")
    outcome <$> oAwaitAny o 1 [r] `shouldReturn` Nothing
    outcome <$> oAwaitAny o 10 [r] `shouldReturn` Just (r, Left "user error (bang)")
    oNow o `shouldReturn` 2
