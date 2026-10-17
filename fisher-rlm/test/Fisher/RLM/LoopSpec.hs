module Fisher.RLM.LoopSpec (spec) where

import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (waitCatch, withAsync)
import Control.Concurrent.MVar
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Fisher.LLM.Normalize (Usage (..))
import Fisher.Py.Value (PyVal (..))
import Fisher.RLM.Config
import Fisher.RLM.Fakes
import Fisher.RLM.Loop (completion)
import Fisher.RLM.Parse (BlockResult (..), Message, formatIteration)
import Fisher.RLM.Prompt
import Fisher.RLM.Repl (CallKind (..), ReplHandle (..))
import Fisher.RLM.Types
import Test.Hspec

ctx :: PyVal
ctx = PStr "the context"

start :: [Message]
start = initialMessages (contextShape ctx) Nothing

run :: RlmEnv -> IO RlmResult
run env = completion env 0 ctx Nothing

-- | The default root model, which also serves sub-calls when no sub-model
-- is set.
dm :: Text
dm = rcModel defaultRlmConfig

noSub :: Model
noSub _ _ = pure (Left (OtherError "no sub-calls expected"))

-- | A model whose every call gives this outcome, counting the calls.
constModel :: Either ModelError Reply -> IO (Model, IO Int)
constModel r = do
  n <- newIORef (0 :: Int)
  pure (\_ _ -> atomicModifyIORef' n (\k -> (k + 1, ())) >> pure r, readIORef n)

-- | A model recording the override it was called with, answering the text.
overrideModel :: Text -> IO (Model, IO [Maybe Text])
overrideModel t = do
  seen <- newIORef []
  pure (\o _ -> atomicModifyIORef' seen (\s -> (s ++ [o], ())) >> pure (Right (reply t)), readIORef seen)

costly :: Double -> Text -> Either ModelError Reply
costly c t = Right (Reply t (usageOf (Just c)) "scripted")

-- | A reply of this many prompt tokens at this cost.
priced :: Integer -> Double -> Text -> Either ModelError Reply
priced p c t = Right (Reply t (Usage p 5 (p + 5) Nothing Nothing (Just c)) "scripted")

-- | The dollars of every call in the run.
spent :: RlmResult -> Double
spent r = sum [fromMaybe 0 (uCost u) | u <- M.elems (rrUsage r)]

-- | The text of a call refused on the budget.
isBudgetRefusal :: Text -> Bool
isBudgetRefusal t = "Error: Budget exhausted (spent $" `T.isPrefixOf` t && " of $" `T.isInfixOf` t && ")" `T.isSuffixOf` t

subCalls :: RlmResult -> [SubCall]
subCalls r = [s | ir <- rrTrajectory r, (_, _, ss) <- irBlocks ir, s <- ss]

stdouts :: RlmResult -> [Text]
stdouts r = [brStdout b | ir <- rrTrajectory r, (_, b, _) <- irBlocks ir]

-- | Track how many are in flight at once: enter, leave, the number in
-- flight, the highest seen, and a hold.
data Gauge = Gauge
  { enter :: IO ()
  , leave :: IO ()
  , live :: IO Int
  , highest :: IO Int
  , hold :: Int -> IO ()
  -- ^ wait until this many have entered in all, or 0.2 s: with no bound
  -- every call is in flight at once, with one the wait runs out
  }

gauge :: IO Gauge
gauge = do
  cur <- newTVarIO (0 :: Int)
  hi <- newTVarIO (0 :: Int)
  entered <- newTVarIO (0 :: Int)
  let enter' = atomically $ do
        n <- (+ 1) <$> readTVar cur
        writeTVar cur n
        modifyTVar' hi (max n)
        modifyTVar' entered (+ 1)
      hold' n = do
        late <- registerDelay 200000
        atomically ((readTVar entered >>= check . (>= n)) `orElse` (readTVar late >>= check))
  pure
    Gauge
      { enter = enter'
      , leave = atomically (modifyTVar' cur (subtract 1))
      , live = readTVarIO cur
      , highest = readTVarIO hi
      , hold = hold'
      }

spec :: Spec
spec = do
  describe "the iteration loop" $ do
    it "answers on turn 3 with the history built turn by turn" $ do
      (root, calls) <- scriptedModel ["thinking", fenced "PRINT hi", fenced "ANSWER 42"]
      r <- run (testEnv defaultRlmConfig root noSub)
      rrEnding r `shouldBe` Answered
      rrIterations r `shouldBe` 3
      rrAnswer r `shouldBe` "42"
      cs <- calls
      length cs `shouldBe` 3
      let c1 = start ++ [turnMessage 0 30]
          c2 = c1 ++ [("assistant", "thinking"), turnMessage 1 30]
          c3 = c2 ++ formatIteration 20000 (fenced "PRINT hi") [BlockResult "hi" "" [] [] Nothing] ++ [turnMessage 2 30]
      cs `shouldBe` [c1, c2, c3]
      last (cs !! 2) `shouldBe` ("user", "Turn 3/30:")
      map irPrompt (rrTrajectory r) `shouldBe` cs

    it "ends at the first block that answers, not showing earlier output or running later blocks" $ do
      let two = "```repl\nPRINT one\n```\n```repl\nANSWER yes\n```\n```repl\nPRINT three\n```"
      (root, calls) <- scriptedModel [two]
      r <- run (testEnv defaultRlmConfig root noSub)
      rrEnding r `shouldBe` Answered
      rrAnswer r `shouldBe` "yes"
      rrIterations r `shouldBe` 1
      length <$> calls `shouldReturn` 1
      [code | ir <- rrTrajectory r, (code, _, _) <- irBlocks ir] `shouldBe` ["PRINT one", "ANSWER yes"]
      map irFinal (rrTrajectory r) `shouldBe` [Just "yes"]

    it "asks for a default answer when the iterations run out" $ do
      (root, calls) <- scriptedModel ["a", "b", "final words"]
      seen <- newIORef []
      let env = (testEnv defaultRlmConfig {rcMaxIterations = 2} root noSub) {reOnIteration = \i _ -> modifyIORef seen (++ [i])}
      r <- run env
      rrEnding r `shouldBe` DefaultAnswer
      rrAnswer r `shouldBe` "final words"
      rrIterations r `shouldBe` 2
      cs <- calls
      length cs `shouldBe` 3
      let c2 = start ++ [turnMessage 0 2, ("assistant", "a"), turnMessage 1 2]
      cs !! 2 `shouldBe` c2 ++ [("assistant", "b"), defaultAnswerMessage]
      last (cs !! 2) `shouldBe` defaultAnswerMessage
      map irFinal (rrTrajectory r) `shouldBe` [Nothing, Nothing, Just "final words"]
      readIORef seen `shouldReturn` [0, 1, 2]

    it "stops when the REPL cannot start, before any model call" $ do
      (root, calls) <- scriptedModel [fenced "ANSWER x"]
      r <- run (testEnv defaultRlmConfig root noSub) {reRepl = \_ -> pure (Left "no python")}
      rrEnding r `shouldBe` Stopped "repl: no python"
      calls `shouldReturn` []

    it "stops with an internal error when the REPL factory throws" $ do
      (root, calls) <- scriptedModel [fenced "ANSWER x"]
      r <- run (testEnv defaultRlmConfig root noSub) {reRepl = \_ -> ioError (userError "disk full")}
      rrEnding r `shouldBe` Stopped "internal: user error (disk full)"
      rrIterations r `shouldBe` 0
      calls `shouldReturn` []

    it "stops with an internal error and the progress so far when a block throws" $ do
      (root, calls) <- scriptedModel ["thinking", fenced "PRINT x", fenced "ANSWER x"]
      let boom _ _ = errorWithoutStackTrace "boom"
      r <- run (testEnv defaultRlmConfig root noSub) {reRepl = fakeRepl boom}
      rrEnding r `shouldBe` Stopped "internal: boom"
      rrAnswer r `shouldBe` "thinking"
      rrIterations r `shouldBe` 1
      length <$> calls `shouldReturn` 2

  describe "the depth fallback" $ do
    let fallback c = do
          (root, calls) <- scriptedModel ["plain answer"]
          r <- completion (testEnv defaultRlmConfig root noSub) 1 c Nothing
          rrEnding r `shouldBe` Answered
          rrAnswer r `shouldBe` "plain answer"
          rrIterations r `shouldBe` 0
          calls
    it "sends a string context as one user message" $
      fallback (PStr "hello") `shouldReturn` [[("user", "hello")]]
    it "sends a list of role and content dicts as those messages" $
      fallback (PList [PDict [("role", PStr "system"), ("content", PStr "s")], PDict [("role", PStr "user"), ("content", PInt 3)]])
        `shouldReturn` [[("system", "s"), ("user", "3")]]
    it "sends anything else as its JSON text" $
      fallback (PDict [("a", PInt 1)]) `shouldReturn` [[("user", "{\"a\": 1}")]]

  describe "sub-calls" $ do
    it "runs rlm_query as a child run one level down when the depth allows" $ do
      (root, calls) <- scriptedModel [fenced "RLM what", fenced "ANSWER kid", fenced "ANSWER done"]
      r <- run (testEnv defaultRlmConfig {rcMaxDepth = 2} root noSub)
      rrEnding r `shouldBe` Answered
      rrAnswer r `shouldBe` "done"
      cs <- calls
      length cs `shouldBe` 3
      cs !! 1 `shouldBe` initialMessages (contextShape (PStr "what")) Nothing ++ [turnMessage 0 30]
      subCalls r `shouldBe` [SubCall CallRlm "what" "kid" 1 dm]
      length (rrTrajectory r) `shouldBe` 2

    it "makes rlm_query a plain sub-model call at the depth limit" $ do
      (root, _) <- scriptedModel [fenced "RLM what", fenced "ANSWER done"]
      (sub, subCs) <- scriptedModel ["plain"]
      r <- run (testEnv defaultRlmConfig root sub)
      rrEnding r `shouldBe` Answered
      subCs `shouldReturn` [[("user", "what")]]
      subCalls r `shouldBe` [SubCall CallRlm "what" "plain" 1 dm]

    it "gives a child the named model as its root model" $ do
      (kid, overrides) <- overrideModel (fenced "ANSWER kid")
      (top, _) <- scriptedModel [fenced "RLM@big what", fenced "ANSWER done"]
      let model o = maybe (top o) (const (kid o)) o
      r <- run (testEnv defaultRlmConfig {rcMaxDepth = 2} model noSub)
      subCalls r `shouldBe` [SubCall CallRlm "what" "kid" 1 "big"]
      overrides `shouldReturn` [Just "big"]

    it "routes llm_query to the sub model with the named model, else the configured one" $ do
      (root, _) <- scriptedModel [fenced "LLM a", fenced "LLM@big b", fenced "ANSWER done"]
      (sub, overrides) <- overrideModel "ok"
      r <- run (testEnv defaultRlmConfig {rcSubModel = Just "small"} root sub)
      rrEnding r `shouldBe` Answered
      overrides `shouldReturn` [Just "small", Just "big"]
      map scModel (subCalls r) `shouldBe` ["small", "big"]

    it "runs a batch in input order with at most max_concurrent in flight" $ do
      g <- gauge
      let prompts = ["p" <> T.pack (show i) | i <- [1 .. 10 :: Int]]
          sub _ msgs = do
            enter g
            hold g 10
            leave g
            pure (Right (reply ("r:" <> snd (head msgs))))
      (root, _) <- scriptedModel [fenced ("LLMS " <> T.intercalate "|" prompts), fenced "ANSWER done"]
      r <- run (testEnv defaultRlmConfig root sub)
      rrEnding r `shouldBe` Answered
      map scResponse (subCalls r) `shouldBe` map ("r:" <>) prompts
      map scPrompt (subCalls r) `shouldBe` prompts
      head (stdouts r) `shouldBe` T.intercalate "\n" (map ("r:" <>) prompts)
      highest g >>= (`shouldSatisfy` (<= 4))

    it "returns a failed llm_query as its error text and carries on" $ do
      (root, calls) <- scriptedModel [fenced "LLM x", fenced "ANSWER done"]
      (sub, _) <- constModel (Left (OtherError "boom"))
      r <- run (testEnv defaultRlmConfig root sub)
      rrEnding r `shouldBe` Answered
      length <$> calls `shouldReturn` 2
      stdouts r `shouldBe` ["Error: LM query failed - boom", ""]
      subCalls r `shouldBe` [SubCall CallLlm "x" "Error: LM query failed - boom" 1 dm]

    it "returns a failed plain call at the depth limit as its error text" $ do
      (root, _) <- scriptedModel [fenced "RLM x", fenced "ANSWER done"]
      (sub, _) <- constModel (Left (Refused "nope"))
      r <- run (testEnv defaultRlmConfig root sub)
      rrEnding r `shouldBe` Answered
      head (stdouts r) `shouldBe` "Error: LM query failed at max depth - refused: nope"

    it "returns a child that runs over the budget in the reference's wording" $ do
      (root, calls) <- scriptedReplies [costly 0 (fenced "RLM q"), costly 0.02 "spend", costly 0 (fenced "ANSWER x")]
      r <- run (testEnv defaultRlmConfig {rcMaxDepth = 2, rcMaxBudget = Just 0.01} root noSub)
      stdouts r `shouldBe` ["Error: Child RLM budget exceeded - budget"]
      rrEnding r `shouldBe` Stopped "budget"
      rrIterations r `shouldBe` 1
      length <$> calls `shouldReturn` 2

    it "returns a child that stops as an error text" $ do
      (root, _) <- scriptedReplies [Right (reply (fenced "RLM x")), Left Empty, Right (reply (fenced "ANSWER done"))]
      r <- run (testEnv defaultRlmConfig {rcMaxDepth = 2} root noSub)
      rrEnding r `shouldBe` Answered
      head (stdouts r) `shouldBe` "Error: Child RLM completion failed - empty"

    it "sums usage per model across root, sub-calls and children" $ do
      let u c = usageOf (Just c)
      (root, _) <-
        scriptedReplies
          [ Right (Reply (fenced "RLM@kid q") (u 0.5) "root")
          , Right (Reply (fenced "LLM x") (u 0.25) "kid")
          , Right (Reply (fenced "ANSWER k") (u 0.25) "kid")
          , Right (Reply (fenced "LLM y") (u 0.5) "root")
          , Right (Reply (fenced "ANSWER done") (u 0.5) "root")
          ]
      (sub, _) <- scriptedReplies (replicate 2 (Right (Reply "s" (u 0.125) "sub")))
      r <- run (testEnv defaultRlmConfig {rcMaxDepth = 2} root sub)
      rrEnding r `shouldBe` Answered
      rrUsage r
        `shouldBe` M.fromList
          [ ("root", Usage 30 15 45 Nothing Nothing (Just 1.5))
          , ("kid", Usage 20 10 30 Nothing Nothing (Just 0.5))
          , ("sub", Usage 20 10 30 Nothing Nothing (Just 0.25))
          ]

  describe "limits" $ do
    it "stops after max_errors consecutive error turns with the newest reply" $ do
      (root, _) <- scriptedModel ["first\n" <> fenced "ERROR", "second\n" <> fenced "ERROR", fenced "ANSWER x"]
      r <- run (testEnv defaultRlmConfig {rcMaxErrors = Just 2} root noSub)
      rrEnding r `shouldBe` Stopped "errors"
      rrAnswer r `shouldBe` "second\n" <> fenced "ERROR"
      rrIterations r `shouldBe` 2

    it "counts only consecutive error turns" $ do
      (root, calls) <- scriptedModel [fenced "ERROR", fenced "PRINT ok", fenced "ERROR", fenced "ERROR", fenced "ANSWER x"]
      r <- run (testEnv defaultRlmConfig {rcMaxErrors = Just 2} root noSub)
      rrEnding r `shouldBe` Stopped "errors"
      length <$> calls `shouldReturn` 4

    it "stops on the budget after the turn that passes it" $ do
      -- priced so the estimate before the second call is far under the budget
      (root, calls) <- scriptedReplies (replicate 5 (priced 1000000 0.006 "spend"))
      r <- run (testEnv defaultRlmConfig {rcMaxBudget = Just 0.01} root noSub)
      rrEnding r `shouldBe` Stopped "budget"
      rrIterations r `shouldBe` 2
      rrAnswer r `shouldBe` "spend"
      length <$> calls `shouldReturn` 2

    it "stops on the timeout" $ do
      (root, calls) <- scriptedModel (replicate 5 "slow")
      clock <- steppingClock 100
      r <- run (testEnv defaultRlmConfig {rcMaxTimeout = Just 150} root noSub) {reClock = clock}
      rrEnding r `shouldBe` Stopped "timeout"
      length <$> calls `shouldReturn` 1

    it "stops on the token limit" $ do
      (root, calls) <- scriptedModel (replicate 5 "talk")
      r <- run (testEnv defaultRlmConfig {rcMaxTokens = Just 20} root noSub)
      rrEnding r `shouldBe` Stopped "tokens"
      length <$> calls `shouldReturn` 2

    it "refuses a child once the budget is spent" $ do
      (root, calls) <- scriptedReplies [costly 0.02 (fenced "RLM q"), costly 0 (fenced "ANSWER x")]
      r <- run (testEnv defaultRlmConfig {rcMaxDepth = 2, rcMaxBudget = Just 0.01} root noSub)
      subCalls r `shouldBe` [SubCall CallRlm "q" "Error: Budget exhausted (spent $0.020000 of $0.010000)" 1 dm]
      rrEnding r `shouldBe` Stopped "budget"
      length <$> calls `shouldReturn` 1

    it "refuses a child once the time is spent" $ do
      (root, calls) <- scriptedModel [fenced "RLM q", fenced "ANSWER x"]
      clock <- steppingClock 100
      r <- run (testEnv defaultRlmConfig {rcMaxDepth = 2, rcMaxTimeout = Just 150} root noSub) {reClock = clock}
      subCalls r `shouldBe` [SubCall CallRlm "q" "Error: Timeout exhausted (200.0s of 150.0s)" 1 dm]
      rrEnding r `shouldBe` Stopped "timeout"
      length <$> calls `shouldReturn` 1

  describe "the budget before each model call" $ do
    it "changes nothing without a budget, priced replies or not" $ do
      (root, calls) <- scriptedReplies [costly 5 "thinking", costly 5 (fenced "PRINT hi"), costly 5 (fenced "ANSWER 42")]
      r <- run (testEnv defaultRlmConfig root noSub)
      rrEnding r `shouldBe` Answered
      rrAnswer r `shouldBe` "42"
      let c1 = start ++ [turnMessage 0 30]
          c2 = c1 ++ [("assistant", "thinking"), turnMessage 1 30]
          c3 = c2 ++ formatIteration 20000 (fenced "PRINT hi") [BlockResult "hi" "" [] [] Nothing] ++ [turnMessage 2 30]
      calls `shouldReturn` [c1, c2, c3]

    it "lets the first call go with nothing spent, even with a tiny budget" $ do
      (root, calls) <- scriptedReplies [costly 1 "looked"]
      r <- run (testEnv defaultRlmConfig {rcMaxBudget = Just 1e-9} root noSub)
      -- the call went; the check after the turn then stops the run
      rrAnswer r `shouldBe` "looked"
      rrEnding r `shouldBe` Stopped "budget"
      length <$> calls `shouldReturn` 1

    it "refuses the sub-calls of a batch that would pass the budget, with no model call and no prefix" $ do
      n <- newIORef (0 :: Int)
      let prompts = ["p" <> T.pack (show i) <> T.replicate (if i < 10 then 398 else 397) "x" | i <- [1 .. 10 :: Int]]
          sub _ _ = atomicModifyIORef' n (\k -> (k + 1, ())) >> pure (priced 100 0.01 "ok")
      -- the root reply is priced at the sub-calls' rate, so every sub-call
      -- reserves its estimate, however the batch is scheduled
      (root, _) <- scriptedReplies [priced 100000 0 (fenced ("LLMS " <> T.intercalate "|" prompts)), costly 0 (fenced "ANSWER done")]
      r <- run (testEnv defaultRlmConfig {rcMaxBudget = Just 0.045} root sub)
      let rs = map scResponse (subCalls r)
          refused = filter isBudgetRefusal rs
      length rs `shouldBe` 10
      refused `shouldSatisfy` (not . null)
      filter (not . isBudgetRefusal) rs `shouldSatisfy` all (== "ok")
      readIORef n `shouldReturn` (10 - length refused)
      spent r `shouldSatisfy` (<= 0.045 + 0.01)
      rrEnding r `shouldBe` Stopped "budget"

    it "refuses in the reference's wording, spent and budget to six places" $ do
      (root, _) <- scriptedModel [fenced ("LLMS " <> T.intercalate "|" (replicate 6 (T.replicate 400 "y"))), fenced "ANSWER done"]
      let sub _ _ = pure (priced 100 0.01 "ok")
      r <- run (testEnv defaultRlmConfig {rcMaxBudget = Just 0.045, rcMaxConcurrent = 1} root sub)
      map scResponse (subCalls r)
        `shouldBe` replicate 4 "ok" ++ replicate 2 "Error: Budget exhausted (spent $0.040000 of $0.045000)"

    it "ends the run as stopped on the budget when a root call would pass it" $ do
      (root, calls) <- scriptedReplies [priced 10 0.001 "looked", costly 0 (fenced "ANSWER x")]
      r <- run (testEnv defaultRlmConfig {rcMaxBudget = Just 0.01} root noSub)
      rrEnding r `shouldBe` Stopped "budget"
      rrAnswer r `shouldBe` "looked"
      rrIterations r `shouldBe` 1
      length <$> calls `shouldReturn` 1

    it "ends the run as stopped on the budget when the default-answer call would pass it" $ do
      (root, calls) <- scriptedReplies [priced 10 0.001 "looked", costly 0 "final"]
      r <- run (testEnv defaultRlmConfig {rcMaxBudget = Just 0.01, rcMaxIterations = 1} root noSub)
      rrEnding r `shouldBe` Stopped "budget"
      rrAnswer r `shouldBe` "looked"
      length <$> calls `shouldReturn` 1

    it "reserves each call's estimate, so concurrent calls cannot all start when only one fits" $ do
      g <- gauge
      n <- newIORef (0 :: Int)
      let prompts = replicate 4 (T.replicate 40000 "z")
          sub _ _ = do
            atomicModifyIORef' n (\k -> (k + 1, ()))
            enter g
            hold g 4
            leave g
            pure (priced 10000 0.01 "ok")
      (root, _) <- scriptedReplies [priced 1000 0.001 (fenced ("LLMS " <> T.intercalate "|" prompts)), costly 0 (fenced "ANSWER done")]
      r <- run (testEnv defaultRlmConfig {rcMaxBudget = Just 0.015, rcMaxConcurrent = 4} root sub)
      highest g `shouldReturn` 1
      readIORef n `shouldReturn` 1
      length (filter isBudgetRefusal (map scResponse (subCalls r))) `shouldBe` 3

    it "releases a call's reservation when the call is cancelled" $ do
      -- a turn whose sub-call is cancelled mid-call, then a root call that
      -- fits only once that sub-call's estimate is released
      entered <- newEmptyMVar
      n <- newIORef (0 :: Int)
      let big = T.replicate 400000 "c"
          sub _ _ = do
            k <- atomicModifyIORef' n (\x -> (x + 1, x))
            if k == 0 then putMVar entered () >> threadDelay maxBound >> pure (Left Empty) else pure (priced 10 0 "ok")
          cancelling code cb = case T.stripPrefix "CANCEL " code of
            Just p -> withAsync (cb CallLlm [p] Nothing) (\_ -> takeMVar entered) >> pure (BlockResult "cancelled" "" [] [] Nothing)
            Nothing -> pyLike code cb
      (root, calls) <- scriptedReplies [priced 1000 0.001 (fenced ("CANCEL " <> big)), costly 0 (fenced "ANSWER done")]
      r <- run (testEnv defaultRlmConfig {rcMaxBudget = Just 0.15} root sub) {reRepl = fakeRepl cancelling}
      readIORef n `shouldReturn` 1
      rrEnding r `shouldBe` Answered
      rrAnswer r `shouldBe` "done"
      length <$> calls `shouldReturn` 2

  describe "model errors" $ do
    let stopsWith e reason = do
          (root, calls) <- scriptedReplies [Right (reply "looked"), Left e, Right (reply (fenced "ANSWER x"))]
          r <- run (testEnv defaultRlmConfig root noSub)
          rrEnding r `shouldBe` Stopped reason
          rrAnswer r `shouldBe` "looked"
          length <$> calls `shouldReturn` 2
    it "stops on a refusal" $ stopsWith (Refused "policy") "refused"
    it "stops on an empty reply" $ stopsWith Empty "empty"
    it "stops on another error, keeping its text" $ stopsWith (OtherError "500") "model error: 500"
    it "stops on no credit with no further calls" $ stopsWith NoCredit "no credit"

    it "stops when the root model throws, keeping what the run has so far" $ do
      (scripted, _) <- scriptedModel ["looked"]
      n <- newIORef (0 :: Int)
      let root o msgs = do
            k <- atomicModifyIORef' n (\c -> (c + 1, c))
            if k == 0 then scripted o msgs else ioError (userError "socket closed")
      r <- run (testEnv defaultRlmConfig root noSub)
      rrEnding r `shouldBe` Stopped "model error: user error (socket closed)"
      rrAnswer r `shouldBe` "looked"
      rrIterations r `shouldBe` 1
      length (rrTrajectory r) `shouldBe` 1
      M.keys (rrUsage r) `shouldBe` ["scripted"]

    it "gives a parent the error of a child whose root model throws" $ do
      (scripted, _) <- scriptedModel [fenced "RLM q", fenced "ANSWER done"]
      let root o msgs
            | msgs == initialMessages (contextShape (PStr "q")) Nothing ++ [turnMessage 0 30] = ioError (userError "boom")
            | otherwise = scripted o msgs
      r <- run (testEnv defaultRlmConfig {rcMaxDepth = 2} root noSub)
      rrEnding r `shouldBe` Answered
      stdouts r `shouldBe` ["Error: Child RLM completion failed - model error: user error (boom)", ""]

    it "stops the whole run when a sub-call has no credit" $ do
      (root, calls) <- scriptedModel [fenced "LLM x", fenced "ANSWER x"]
      (sub, _) <- constModel (Left NoCredit)
      r <- run (testEnv defaultRlmConfig root sub)
      rrEnding r `shouldBe` Stopped "no credit"
      length <$> calls `shouldReturn` 1

    it "stops on no credit even when the REPL catches the callback's failure" $ do
      -- as the Python REPL does: a failed callback becomes the call's result
      let catching code cb = pyLike code (\k ps m -> withAsync (cb k ps m) waitCatch >>= either (const (pure (map (const "Error") ps))) pure)
      (root, calls) <- scriptedModel [fenced "LLM x", fenced "ANSWER x"]
      (sub, _) <- constModel (Left NoCredit)
      r <- run (testEnv defaultRlmConfig root sub) {reRepl = fakeRepl catching}
      rrEnding r `shouldBe` Stopped "no credit"
      length <$> calls `shouldReturn` 1

  describe "batched children (Review Focus 5)" $ do
    let batch kidReply = do
          g <- gauge
          kids <- newIORef (0 :: Int)
          let prompts = T.intercalate "|" ["q" <> T.pack (show i) | i <- [1 .. 10 :: Int]]
              model o _ = case o of
                Nothing -> pure (Right (reply (fenced ("RLMS@kid " <> prompts))))
                Just _ -> do
                  n <- atomicModifyIORef' kids (\k -> (k + 1, k + 1))
                  hold g 10
                  pure (kidReply n)
              factory c
                | c == ctx = pure (Right (ReplHandle pyLike (pure ())))
                | otherwise = do
                    enter g
                    pure (Right (ReplHandle pyLike (leave g)))
          r <- run (testEnv defaultRlmConfig {rcMaxDepth = 2, rcMaxIterations = 1} model noSub) {reRepl = factory}
          (,,,) r <$> live g <*> highest g <*> readIORef kids
    it "keeps at most max_concurrent children alive and closes them all" $ do
      (r, alive, most, kids) <- batch (const (Right (reply (fenced "ANSWER ok"))))
      kids `shouldBe` 10
      map scResponse (subCalls r) `shouldBe` replicate 10 "ok"
      alive `shouldBe` 0
      most `shouldSatisfy` (<= 4)
    it "closes every child when one runs out of credit mid-batch" $ do
      (r, alive, most, _) <- batch (\n -> if n == 6 then Left NoCredit else Right (reply (fenced "ANSWER ok")))
      rrEnding r `shouldBe` Stopped "no credit"
      alive `shouldBe` 0
      most `shouldSatisfy` (<= 4)
