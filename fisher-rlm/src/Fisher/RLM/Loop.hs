-- | The RLM loop: the root model writes code, the REPL runs it, and the
-- code's model calls are served here, as plain calls or as child runs one
-- level down. Ported from RLM.completion, _completion_turn,
-- _check_iteration_limits, _default_answer, _fallback_answer and _subcall in
-- rlm/core/rlm.py, with the call routing of _llm_query and _rlm_query in
-- rlm/environments/local_repl.py (alexzhang13/rlm at
-- d04208afbad29ca675ab13478c40ee8bebc84bfe, MIT; see NOTICE).
module Fisher.RLM.Loop (completion) where

import Control.Applicative ((<|>))
import Control.Concurrent.Async (mapConcurrently, race)
import Control.Concurrent.QSem
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (when)
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Fisher.LLM.Normalize (Usage (..))
import Fisher.Py.Value (PyVal (..), dumpsAscii, get, pyStrOf)
import Fisher.RLM.Config (RlmConfig (..))
import Fisher.RLM.Parse (BlockResult (..), Message, findCodeBlocks, formatIteration)
import Fisher.RLM.Prompt (contextShape, defaultAnswerMessage, initialMessages, turnMessage)
import Fisher.RLM.Repl (CallKind (..), Callback, ReplHandle (..))
import Fisher.RLM.Types
import Text.Printf (printf)

-- | What every run in one tree shares.
data Tree = Tree
  { tUsage :: TVar (M.Map Text Usage)
  , tNoCredit :: TVar Bool
  , tStart :: Double
  , tMaxBudget :: Maybe Double
  , tBudget :: TVar Budget
  }

-- | What the budget check before each call needs besides the dollars spent:
-- the estimates of the calls in flight, and the priced replies so far.
data Budget = Budget
  { bReserved :: Double
  , bPricedCost :: Double
  -- ^ dollars over the replies that reported a cost
  , bPricedPrompt :: Integer
  -- ^ prompt tokens over the same replies
  }

-- | Thrown where a call finds no credit, so the whole tree unwinds.
data NoCreditStop = NoCreditStop
  deriving (Show)

instance Exception NoCreditStop

-- | One run's progress so far, readable after the run is interrupted.
data Progress = Progress
  { pIterations :: Int
  , pTrajectory :: [IterationRecord]
  -- ^ newest first
  , pBest :: Maybe Text
  -- ^ the newest non-empty root reply
  }

-- | Run on the context at the given depth, with an optional root prompt.
-- At or past the depth limit this is one plain call on the context.
completion :: RlmEnv -> Int -> PyVal -> Maybe Text -> IO RlmResult
completion env depth ctx root = do
  t0 <- reClock env
  tree <- Tree <$> newTVarIO M.empty <*> newTVarIO False <*> pure t0 <*> pure (rcMaxBudget (reConfig env)) <*> newTVarIO (Budget 0 0 0)
  progress <- newIORef (Progress 0 [] Nothing)
  -- A sub-call's failure may be caught by the REPL (the Python one turns it
  -- into the call's result), so the flag, not only the exception, stops it.
  -- Any other ordinary exception (the REPL failing to start or to run a
  -- block) ends the run with what it has so far.
  let noCredit = readTVar (tNoCredit tree) >>= check
  out <- try (race (atomically noCredit) (trySync (runAt tree env progress depth ctx root)))
  case out of
    Right (Right (Right r)) -> pure r
    Right (Right (Left e)) -> finish tree progress (Stopped ("internal: " <> T.pack (displayException e)))
    Right (Left ()) -> finish tree progress (Stopped "no credit")
    Left NoCreditStop -> finish tree progress (Stopped "no credit")

-- | The result as the run stands.
finish :: Tree -> IORef Progress -> Ending -> IO RlmResult
finish tree progress ending = do
  p <- readIORef progress
  usage <- readTVarIO (tUsage tree)
  let answer = case ending of
        Stopped _ -> fromMaybe "" (pBest p)
        _ -> ""
  pure (RlmResult answer ending (pIterations p) usage (reverse (pTrajectory p)))

answered :: Tree -> IORef Progress -> Ending -> Text -> IO RlmResult
answered tree progress ending a = (\r -> r {rrAnswer = a}) <$> finish tree progress ending

runAt :: Tree -> RlmEnv -> IORef Progress -> Int -> PyVal -> Maybe Text -> IO RlmResult
runAt tree env progress depth ctx root
  | depth >= rcMaxDepth cfg = fallback tree env progress ctx
  | otherwise =
      bracket (reRepl env ctx) (either (const (pure ())) rhClose) $ \case
        Left e -> finish tree progress (Stopped ("repl: " <> e))
        Right h -> iterateTurns tree env progress depth h (initialMessages (contextShape ctx) root)
  where
    cfg = reConfig env

-- | The repo's _fallback_answer: the context as messages, one call.
fallback :: Tree -> RlmEnv -> IORef Progress -> PyVal -> IO RlmResult
fallback tree env progress ctx = do
  r <- rootCall tree env messages
  case r of
    Left why -> finish tree progress why
    Right rep -> answered tree progress Answered (rpText rep)
  where
    messages = case ctx of
      PStr s -> [("user", s)]
      PList xs@(_ : _) | Just ms <- traverse asMessage xs -> ms
      v -> [("user", dumpsAscii v)]
    asMessage v@(PDict _) = (,) <$> (pyStrOf <$> get "role" v) <*> (pyStrOf <$> get "content" v)
    asMessage _ = Nothing

stopFor :: ModelError -> Ending
stopFor = \case
  Refused _ -> Stopped "refused"
  Empty -> Stopped "empty"
  NoCredit -> Stopped "no credit"
  OtherError t -> Stopped ("model error: " <> t)
  OverBudget _ -> Stopped "budget"

errorText :: ModelError -> Text
errorText = \case
  Refused t -> "refused: " <> t
  Empty -> "empty response"
  NoCredit -> "no credit"
  OtherError t -> t
  OverBudget t -> t

-- | Every model call in the tree goes through here: none starts once credit
-- has run out, and each reply's usage is added to the tree's. Under a
-- budget, a call starts only when the dollars spent, the estimates of the
-- calls in flight and its own estimate fit in it; its estimate is held
-- until it returns. The estimate is a quarter of the messages' characters
-- at the run's dollars per prompt token so far, 0 before any priced reply.
callModel :: Tree -> Model -> Maybe Text -> [Message] -> IO (Either ModelError Reply)
callModel tree model override msgs = do
  out <- readTVarIO (tNoCredit tree)
  when out (throwIO NoCreditStop)
  case tMaxBudget tree of
    Nothing -> call
    -- masked from the reservation until the release is installed, so a
    -- cancellation cannot leave the estimate held
    Just b -> mask $ \restore -> do
      held <- atomically $ do
        spent <- usageCost <$> readTVar (tUsage tree)
        bu <- readTVar (tBudget tree)
        let rate
              | bPricedPrompt bu > 0 = bPricedCost bu / fromIntegral (bPricedPrompt bu)
              | otherwise = 0
            estimate = fromIntegral (sum (map (T.length . snd) msgs)) / 4 * rate
        if spent + bReserved bu + estimate > b
          then pure (Left spent)
          else do
            writeTVar (tBudget tree) bu {bReserved = bReserved bu + estimate}
            pure (Right estimate)
      case held of
        Left spent -> pure (Left (OverBudget (budgetText spent b)))
        Right estimate ->
          restore call `finally` atomically (modifyTVar' (tBudget tree) (\bu -> bu {bReserved = bReserved bu - estimate}))
  where
    call = do
      r <- model override msgs
      case r of
        Left NoCredit -> do
          atomically (writeTVar (tNoCredit tree) True)
          throwIO NoCreditStop
        Left e -> pure (Left e)
        Right rep -> do
          let u = rpUsage rep
          atomically $ do
            modifyTVar' (tUsage tree) (M.insertWith addUsage (rpModel rep) u)
            case uCost u of
              Just c -> modifyTVar' (tBudget tree) (\bu -> bu {bPricedCost = bPricedCost bu + c, bPricedPrompt = bPricedPrompt bu + uPrompt u})
              Nothing -> pure ()
          pure (Right rep)

-- | The refusal of a call or a child run on the budget, as the reference
-- words it.
budgetText :: Double -> Double -> Text
budgetText spent b = T.pack (printf "Error: Budget exhausted (spent $%.6f of $%.6f)" spent b)

-- | A call to this run's root model; a failure, or an ordinary exception
-- from the model, is the ending to stop with.
rootCall :: Tree -> RlmEnv -> [Message] -> IO (Either Ending Reply)
rootCall tree env msgs = do
  r <- trySync (callModel tree (reRoot env) Nothing msgs)
  pure $ case r of
    Right (Right rep) -> Right rep
    Right (Left e) -> Left (stopFor e)
    Left e -> Left (Stopped ("model error: " <> T.pack (displayException e)))

addUsage :: Usage -> Usage -> Usage
addUsage a b =
  Usage
    { uPrompt = uPrompt a + uPrompt b
    , uCompletion = uCompletion a + uCompletion b
    , uTotal = uTotal a + uTotal b
    , uCacheRead = plus (uCacheRead a) (uCacheRead b)
    , uCacheWrite = plus (uCacheWrite a) (uCacheWrite b)
    , uCost = plus (uCost a) (uCost b)
    }
  where
    plus Nothing Nothing = Nothing
    plus x y = Just (fromMaybe 0 x + fromMaybe 0 y)

treeCost :: Tree -> IO Double
treeCost tree = usageCost <$> readTVarIO (tUsage tree)

usageCost :: M.Map Text Usage -> Double
usageCost = sum . map (fromMaybe 0 . uCost) . M.elems

treeTokens :: Tree -> IO Integer
treeTokens tree = sum . map (\u -> uPrompt u + uCompletion u) . M.elems <$> readTVarIO (tUsage tree)

-- --------------------------------------------------------------------- turns

iterateTurns :: Tree -> RlmEnv -> IORef Progress -> Int -> ReplHandle -> [Message] -> IO RlmResult
iterateTurns tree env progress depth h = go 0 0
  where
    cfg = reConfig env
    maxIt = rcMaxIterations cfg
    stop = finish tree progress . Stopped
    overTime now = maybe False (\t -> now - tStart tree > t) (rcMaxTimeout cfg)

    go i errs history
      | i >= maxIt = defaultAnswer history
      | otherwise = do
          before <- reClock env
          if overTime before
            then stop "timeout"
            else do
              let prompt = history ++ [turnMessage i maxIt]
              r <- rootCall tree env prompt
              case r of
                Left why -> finish tree progress why
                Right rep -> turn i errs prompt before (rpText rep)

    turn i errs prompt before text = do
      blocks <- runBlocks tree env depth h (findCodeBlocks text)
      after <- reClock env
      let final = listToMaybe [a | (_, b, _) <- blocks, Just a <- [brFinal b]]
          record = IterationRecord prompt text blocks final (after - before)
      modifyIORef' progress $ \p ->
        p
          { pIterations = i + 1
          , pTrajectory = record : pTrajectory p
          , pBest = if T.null (T.strip text) then pBest p else Just text
          }
      reOnIteration env i record
      let errs' = if any (\(_, b, _) -> not (T.null (brStderr b))) blocks then errs + 1 else 0 :: Int
      limit <- overLimit errs' after
      case (limit, final) of
        (Just why, _) -> stop why
        (Nothing, Just a) -> answered tree progress Answered a
        (Nothing, Nothing) ->
          go (i + 1) errs' (prompt ++ formatIteration (rcTruncate cfg) text [b | (_, b, _) <- blocks])

    -- _check_iteration_limits, with the timeout as well
    overLimit errs now = do
      cost <- treeCost tree
      tokens <- treeTokens tree
      pure . listToMaybe $
        ["errors" | Just m <- [rcMaxErrors cfg], errs >= m]
          ++ ["budget" | Just b <- [rcMaxBudget cfg], cost > b]
          ++ ["timeout" | overTime now]
          ++ ["tokens" | Just n <- [rcMaxTokens cfg], tokens > n]

    defaultAnswer history = do
      let prompt = history ++ [defaultAnswerMessage]
      before <- reClock env
      r <- rootCall tree env prompt
      case r of
        Left why -> finish tree progress why
        Right rep -> do
          after <- reClock env
          let text = rpText rep
              record = IterationRecord prompt text [] (Just text) (after - before)
          modifyIORef' progress (\p -> p {pTrajectory = record : pTrajectory p})
          reOnIteration env maxIt record
          answered tree progress DefaultAnswer text

-- | Run the blocks in order, up to and including the first that answers.
runBlocks :: Tree -> RlmEnv -> Int -> ReplHandle -> [Text] -> IO [(Text, BlockResult, [SubCall])]
runBlocks _ _ _ _ [] = pure []
runBlocks tree env depth h (code : more) = do
  calls <- newIORef []
  res <- rhExec h code (callback tree env depth calls)
  subs <- readIORef calls
  let this = (code, res, subs)
  case brFinal res of
    Just _ -> pure [this]
    Nothing -> (this :) <$> runBlocks tree env depth h more

-- ----------------------------------------------------------------- sub-calls

-- | Serve one call from the code: every prompt concurrently, at most
-- 'rcMaxConcurrent' at once, results in input order. The calls are recorded,
-- in order, for the block's record.
callback :: Tree -> RlmEnv -> Int -> IORef [SubCall] -> Callback
callback tree env depth calls kind prompts override = do
  sem <- newQSem (max 1 (rcMaxConcurrent cfg))
  results <- mapConcurrently (bracket_ (waitQSem sem) (signalQSem sem) . one) prompts
  atomicModifyIORef' calls (\cs -> (cs ++ zipWith (\p r -> SubCall kind p r next used) prompts results, ()))
  pure results
  where
    cfg = reConfig env
    next = depth + 1
    asChild = kind == CallRlm && next < rcMaxDepth cfg
    subModel = override <|> rcSubModel cfg
    -- the model each call is served by, as the trajectory records it
    used = fromMaybe (rcModel cfg) (if asChild then override else subModel)
    one p = case kind of
      CallLlm -> plain "Error: LM query failed - " p
      CallRlm
        | asChild -> child p
        | otherwise -> plain "Error: LM query failed at max depth - " p

    plain prefix p = do
      r <- trySync (callModel tree (reSub env) subModel [("user", p)])
      pure $ case r of
        Right (Right rep) -> rpText rep
        Right (Left (OverBudget t)) -> t
        Right (Left e) -> prefix <> errorText e
        Left e -> prefix <> T.pack (displayException e)

    child p = do
      refused <- exhausted
      case refused of
        Just msg -> pure msg
        Nothing -> do
          progress <- newIORef (Progress 0 [] Nothing)
          let env' =
                env
                  { reRoot = maybe (reRoot env) (\m _ -> reRoot env (Just m)) override
                  , reOnIteration = \_ _ -> pure ()
                  }
          r <- trySync (runAt tree env' progress next (PStr p) Nothing)
          pure $ case r of
            Right res -> case rrEnding res of
              Stopped "budget" -> "Error: Child RLM budget exceeded - budget"
              Stopped why -> "Error: Child RLM completion failed - " <> why
              _ -> rrAnswer res
            Left e -> "Error: RLM query failed - " <> T.pack (displayException e)

    -- what is left of the budget and the time, as _subcall checks before a
    -- child starts
    exhausted = do
      spent <- treeCost tree
      case rcMaxBudget cfg of
        Just b | b - spent <= 0 -> pure (Just (budgetText spent b))
        _ -> case rcMaxTimeout cfg of
          Nothing -> pure Nothing
          Just t -> do
            now <- reClock env
            let elapsed = now - tStart tree
            pure $
              if t - elapsed <= 0
                then Just (T.pack (printf "Error: Timeout exhausted (%.1fs of %.1fs)" elapsed t))
                else Nothing

-- | Catch an action's ordinary failures; running out of credit and
-- cancellation pass through.
trySync :: IO a -> IO (Either SomeException a)
trySync act = do
  r <- try act
  case r of
    Left e
      | Just NoCreditStop <- fromException e -> throwIO e
      | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
    _ -> pure r
