{-# LANGUAGE RankNTypes #-}

-- | The swarm's scheduler: attempts on a file's open goals, run in
-- parallel across backends, each proof checked by the kernel and merged
-- into a shared base that later attempts start from.
--
-- Policy, in order: every open goal gets a first attempt before any gets
-- a second; a retry goes to a backend that has not tried that goal yet,
-- else the one that has tried it least; with slots to spare, a goal
-- already being attempted gets another attempt on a backend not running
-- it; a goal stops after @scMaxAttempts@ finished attempts. When a goal
-- closes, the other attempts on it are cancelled. A proof that depends
-- only on goals still open is merged too and the goal is waiting; it
-- closes when they do. The search ends when nothing can be launched and
-- nothing is running, or at the budget.
--
-- With automation on ('seAuto'), just before a goal's first attempt is
-- launched it gets Auto's tactic portfolio (so the slots fill as each
-- goal's pre-pass ends); a fill the kernel accepts closes the goal with no
-- model. A failed attempt goes to Repair, and a partial proof it keeps
-- becomes the goal's next start. With an 'SmtEnv' ('aeSmt'), Disprove's
-- SMT source runs once after the pre-pass, before the goal's first
-- attempt (one solver process); after a goal's second failure Disprove
-- runs once with plausible (and the SMT source when it did not run
-- before): a certified negation stops the goal, a counterexample goes
-- into its next brief. If the language server fails, the automation is
-- off for the rest of the run.
--
-- With retrieval on ('seRetrieval'), each goal's first launch gets its
-- facts and its offer from the library ("Fisher.Search.Retrieval"), cut
-- off at the env's limit, and every attempt on the goal carries them
-- ahead of any repair or counterexample text. With the library on, the
-- run's new sorry-free lemmas are saved to it at the end.
--
-- With a learned policy ('scGoalPolicy'), a goal with an entry orders its
-- backends, its tiers and its pre-pass tactics by the entry, within the
-- configured arms. Where the backends' order differs from the configured
-- one it is preferred over the spread by usage; a goal marked to explore
-- runs its first attempt on its second backend. Each knob whose order
-- differs is logged once per goal at its first use ('PolicyApplied'), and
-- so is a learned attempt budget below the configured one
-- ('scAttemptSecs'); the tiers' and the budget's are never exploration.
module Fisher.Search.Scheduler
  ( SearchConfig (..)
  , defaultSearchConfig
  , AttemptSpec (..)
  , AttemptResult (..)
  , plainResult
  , priceCancelled
  , SearchEnv (..)
  , GoalStatus (..)
  , GoalState (..)
  , Event (..)
  , AttemptRecord (..)
  , plainRecord
  , SearchReport (..)
  , AutoEnv (..)
  , AutoTrack (..)
  , noAutoTrack
  , trackRan
  , partialBrief
  , suspectBrief
  , stallNote
  , attemptBrief
  , autoStep
  , partialStart
  , keptPartial
  , pickLaunch
  , goalBackends
  , goalTiers
  , goalTactics
  , runSearch
  , runSingle
  , runToEnd
  ) where

import Control.Exception (SomeAsyncException, SomeException, displayException, fromException, mask, onException, throwIO, try)
import Control.Monad (forM_, join, void, when)
import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.IORef
import Data.Char (isDigit)
import Data.List (elemIndex, intersperse, nub, sortOn)
import qualified Data.Map.Strict as Map
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe, isJust, listToMaybe, maybeToList)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (showFFloat)

import Fisher.Learn.Policy (GoalPolicy (..))
import Fisher.Role (PromptPart, static, toPrompt, tool, worker)
import Fisher.Search.Attempt
import Fisher.Search.Auto (AutoConfig (..), AutoFail (..), Hole (..), Lsp (..), autoGoal, holesIn)
import Fisher.Search.Check (AxiomPolicy, LeanRun)
import qualified Fisher.Search.Check as C
import Fisher.Search.Child (ChildStats (..))
import Fisher.Search.Controller (Health, Stop (..), cap2000, emptyHealth, modelFailureText, noteResult, skipped, stopReasonText)
import Fisher.Search.Disprove (Disproof (..), DisproveInfo (..), DisproveSource (..), SmtEnv, disprove, disproveAfterSuspect, disproveWitnessOnly, sourceName)
import Fisher.Search.Headless (nativeBackend)
import Fisher.Search.Lean
import Fisher.Search.Orchestrate (AttemptOrchestrator, Orchestrator (..), ReplayDiverged, Running)
import qualified Fisher.Search.Repair as R
import Fisher.Search.Retrieval
import Fisher.Search.Splice (Spliced (..), splice)
import Fisher.Search.Verify

data SearchConfig = SearchConfig
  { scBackends :: [Text]
  , scSlots :: Int
  , scMaxAttempts :: Int
  , scBudget :: Double
  -- ^ seconds of wall clock for the whole search
  , scParallelPerGoal :: Bool
  , scTiers :: [Text]
  -- ^ the model tiers, cheapest first; empty is the backend's own model
  , scControl :: Bool
  -- ^ the compute controller is on
  , scUntiered :: [Text]
  -- ^ backends outside the tiers (fisher search: the stream backends,
  -- claude-code and codex): their attempts run on the backend's own model,
  -- with no tier, and say nothing of the tiers' health
  , scGoalPolicy :: Map.Map Text GoalPolicy
  -- ^ the learned policy by goal: its order of the backends, the tiers and
  -- the pre-pass's tactics, each within the configured arms (an arm that
  -- is not configured is ignored); a goal with no entry, or a knob with no
  -- configured arm, runs on the configured order
  , scPolicyLogged :: Bool
  -- ^ log a 'PolicyApplied' event, once per goal and knob, where the
  -- policy's order differs from the configured one
  , scAttemptSecs :: Maybe Int
  -- ^ the configured attempt budget in seconds, when the caller applies
  -- the policy's lower one ('gpAttemptSecs'): a goal whose budget is
  -- below it logs a 'PolicyApplied' of knob @attempt_secs@ at its first
  -- launch. The scheduler itself times no attempt.
  }
  deriving (Eq, Show)

defaultSearchConfig :: SearchConfig
defaultSearchConfig = SearchConfig
  { scBackends = ["claude-code", nativeBackend], scSlots = 4, scMaxAttempts = 3, scBudget = 7200, scParallelPerGoal = True
  , scTiers = [], scControl = False, scUntiered = [], scGoalPolicy = Map.empty, scPolicyLogged = True
  , scAttemptSecs = Nothing }

-- | The arms of a knob for a goal: the policy's that are configured, in
-- its order, once each; the configured arms when none is.
policyOrder :: [Text] -> [Text] -> [Text]
policyOrder configured pol = case nub [x | x <- pol, x `elem` configured] of
  [] -> configured
  xs -> xs

-- | A goal's backends, in the order its attempts prefer them.
goalBackends :: SearchConfig -> Text -> [Text]
goalBackends cfg g = maybe (scBackends cfg) (policyOrder (scBackends cfg) . gpBackends) (Map.lookup g (scGoalPolicy cfg))

-- | A goal's tiers, as indices into 'scTiers': the policy's order, then
-- any configured tier it leaves out (so the cascade keeps every tier).
goalTiers :: SearchConfig -> Text -> [Int]
goalTiers cfg g =
  let ts = scTiers cfg
      ord = maybe ts (policyOrder ts . gpTiers) (Map.lookup g (scGoalPolicy cfg))
      is = [i | t <- ord, Just i <- [elemIndex t ts]]
   in is <> [i | i <- [0 .. length ts - 1], i `notElem` is]

-- | A goal's pre-pass tactics within the run's.
goalTactics :: SearchConfig -> Text -> [Text] -> [Text]
goalTactics cfg g tacs = maybe tacs (policyOrder tacs . gpTactics) (Map.lookup g (scGoalPolicy cfg))

-- | The tier for a goal's step over its tier order, as
-- "Fisher.Search.Controller"'s @tierFor@ does over the configured one: start at position @min k (n-1)@, else the next
-- unskipped position above, else the highest unskipped one below.
tierIn :: [Int] -> Health -> Int -> Maybe Int
tierIn ord h k = (ord !!) <$> case [p | p <- [start .. n - 1], ok p] of
  (p : _) -> Just p
  [] -> listToMaybe [p | p <- [start - 1, start - 2 .. 0], ok p]
  where
    n = length ord
    start = max 0 (min k (n - 1))
    ok p = not (skipped h (ord !! p))

data SearchEnv = SearchEnv
  { seOrch :: AttemptOrchestrator
  -- ^ the effects: an attempt spawned runs to the end and cancelling it
  -- must stop it; the clock; the run log; the automation's capped steps;
  -- and a cancelled attempt's dollars (fisher search: its ledger rows),
  -- asked only with the controller on, once the attempt is cancelled
  , seLean :: LeanRun
  , sePolicy :: AxiomPolicy
  , seRoot :: FilePath
  -- ^ the project the checks compile in
  , seRel :: FilePath
  , seBaseChanged :: Text -> IO ()
  , seAuto :: Maybe AutoEnv
  -- ^ the automation pre-pass, Repair and Disprove; Nothing is the search
  -- without them
  , seRetrieval :: Maybe RetrievalEnv
  -- ^ facts and the library; Nothing is the search without them
  }

-- | What the automation needs: its settings, Lean's language server and,
-- for Disprove's SMT source, the solver (Nothing turns that source off).
data AutoEnv = AutoEnv
  { aeConfig :: AutoConfig
  , aeLsp :: Lsp
  , aeSmt :: Maybe SmtEnv
  }

data GoalStatus = Open | Closed | Waiting | GaveUp | Disproved
  deriving (Eq, Show)

data GoalState = GoalState
  { gsStatus :: GoalStatus
  , gsTried :: [Text]
  -- ^ backends of the finished attempts, oldest first
  , gsRunning :: [(Int, Text)]
  , gsBy :: Maybe (Int, Text)
  -- ^ the attempt whose proof closed (or is waiting)
  , gsAt :: Maybe Double
  -- ^ seconds from the start when it closed
  , gsClosedBy :: Maybe Text
  -- ^ who wrote the kept proof: @auto@, @model@ or @model+repair@
  , gsPartial :: Maybe (Text, Text, Text)
  -- ^ from Repair: the failed attempt's start, the repaired file and the
  -- brief listing its holes (lines as in the repaired file)
  , gsDisproof :: Maybe Disproof
  , gsDisproveTried :: Bool
  , gsSmtTried :: Bool
  -- ^ Disprove's SMT source ran before the first attempt
  , gsDisproofSource :: Maybe DisproveSource
  -- ^ where the disproof's counterexample came from
  , gsDisproofWitness :: Maybe Text
  -- ^ for a certified negation, the solver's model whose witness tactic
  -- certified it
  , gsStall :: Maybe PromptPart
  -- ^ the note on a stopped attempt for the goal's next brief
  , gsTiers :: [Int]
  -- ^ the tiers the goal's attempts were launched on, each once, in order
  }
  deriving (Eq, Show)

data AttemptRecord = AttemptRecord
  { arId :: Int
  , arGoal :: Text
  , arBackend :: Text
  , arOutcome :: Text
  , arStarted :: Double
  , arEnded :: Double
  , arChild :: Maybe ChildStats
  , arModel :: Maybe Text
  , arTierOf :: Maybe Int
  , arStopped :: Maybe Stop
  -- ^ the controller stopped the attempt, and why
  , arCost :: Maybe Double
  -- ^ the attempt's dollars as the run reported them, else the child's own
  }
  deriving (Eq, Show)

-- | A record with no controller fields: attempt, goal, backend, outcome,
-- started, ended, stats.
plainRecord :: Int -> Text -> Text -> Text -> Double -> Double -> Maybe ChildStats -> AttemptRecord
plainRecord i g b o s e c = AttemptRecord
  { arId = i, arGoal = g, arBackend = b, arOutcome = o, arStarted = s, arEnded = e, arChild = c
  , arModel = Nothing, arTierOf = Nothing, arStopped = Nothing, arCost = Nothing }

data SearchReport = SearchReport
  { srGoals :: [(Text, GoalState)]
  , srAttempts :: [AttemptRecord]
  , srElapsed :: Double
  , srClosed :: Int
  , srBudgetSpent :: Bool
  , srBase :: Text
  , srAuto :: Bool
  -- ^ the automation was on at the start
  , srRepairs :: Int
  -- ^ repaired attempts Repair kept
  , srAutoRan :: Bool
  -- ^ the automation was on at the start and did not go off before any
  -- of its steps completed
  , srAutoOff :: Maybe Text
  -- ^ why the automation went off during the run
  , srRetrieval :: Maybe RetrievalReport
  -- ^ what retrieval and the library did; Nothing with both off
  , srControl :: Bool
  -- ^ the compute controller was on
  , srEscalations :: Int
  -- ^ the 'Escalated' events logged
  }
  deriving (Eq, Show)

-- | The next attempt to launch, if any: a goal and a backend.
pickLaunch :: SearchConfig -> [Text] -> Map.Map Text GoalState -> Map.Map Text Int -> Maybe (Text, Text)
pickLaunch cfg order goals usage =
  let st g = goals Map.! g
      open = [g | g <- order, Map.member g goals, gsStatus (st g) == Open, length (gsTried (st g)) + length (gsRunning (st g)) < scMaxAttempts cfg]
      idle = [g | g <- open, null (gsRunning (st g))]
      busy =
        [ g | scParallelPerGoal cfg, g <- open, not (null (gsRunning (st g)))
        , length (gsRunning (st g)) < length (goalBackends cfg g) ]
      byAttempts gs = sortOn (\(i, g) -> (length (gsTried (st g)) + length (gsRunning (st g)), i)) (zip [0 :: Int ..] gs)
      choose gs = case byAttempts gs of
        [] -> Nothing
        ((_, g) : _) -> fmap (\b -> (g, b)) (backendFor g)
      -- a goal's backends in its policy's order; where that differs from
      -- the configured order it is preferred over the spread by usage, and
      -- an exploring goal's first attempt runs on its second backend
      backendFor g =
        let s = st g
            cands = goalBackends cfg g
            applied = cands /= scBackends cfg
            explore = maybe False gpExplore (Map.lookup g (scGoalPolicy cfg))
            free = [b | b <- cands, b `notElem` map snd (gsRunning s)]
            score b = (b `elem` gsTried s, length (filter (== b) (gsTried s)), if applied then 0 else Map.findWithDefault 0 b usage)
         in case cands of
              _ : second : _ | explore, null (gsTried s), null (gsRunning s) -> Just second
              _ -> case sortOn (\(i, b) -> (score b, i)) (zip [0 :: Int ..] free) of
                [] -> Nothing
                ((_, b) : _) -> Just b
   in case choose idle of
        Just x -> Just x
        Nothing -> choose busy

data St = St
  { stGoals :: Map.Map Text GoalState
  , stBase :: Text
  , stNext :: Int
  , stUsage :: Map.Map Text Int
  , stRunning :: [(Running, AttemptSpec, Double)]
  , stRecords :: [AttemptRecord]
  , stFromPartial :: [Int]
  -- ^ attempts that started from a repaired partial proof
  , stRepairs :: Int
  , stPrePassed :: Set.Set Text
  -- ^ goals whose pre-pass has run
  , stRetrieved :: Map.Map Text Retrieved
  -- ^ each goal's retrieval, made at its first launch
  , stHelpers :: Map.Map Text [Text]
  -- ^ the helpers merged with each goal's kept proofs
  , stRetried :: Set.Set Text
  -- ^ goals whose cut-off retrieval was made again
  , stHealth :: Health
  -- ^ the tiers' health, run-wide
  , stLastTier :: Map.Map Text Int
  -- ^ each goal's last chosen tier
  , stModelErrors :: Map.Map Text Int
  -- ^ model errors per goal kept out of its attempt count
  , stEscalations :: Int
  -- ^ 'Escalated' events logged
  , stStallUsed :: Map.Map Int PromptPart
  -- ^ the stall note each running attempt took into its brief
  , stPolicyLogged :: Set.Set (Text, Text)
  -- ^ the goals and knobs whose 'PolicyApplied' is logged
  }

-- | The brief for an attempt that starts from a repaired proof: each hole
-- left, its line and its goal state.
partialBrief :: [(Hole, Text)] -> Text
partialBrief gs =
  T.intercalate "\n" $
    "Start from this partial proof; fill these holes:"
      : concat [("- line " <> T.pack (show (hLine h)) <> ":") : map ("    " <>) (T.lines g) | (h, g) <- gs]

-- | A 'partialBrief' with its line numbers moved by @d@.
shiftBrief :: Int -> Text -> Text
shiftBrief 0 b = b
shiftBrief d b = T.intercalate "\n" (map shift (T.splitOn "\n" b))
  where
    shift l = case T.stripPrefix "- line " l of
      Just r
        | (n, ":") <- T.span isDigit r, not (T.null n) -> "- line " <> T.pack (show (read (T.unpack n) + d)) <> ":"
      _ -> l

-- | A 'partialBrief' (as moved by 'shiftBrief') as a prompt part: its
-- heading and line marks are Fisher's, the indented lines Lean's goal
-- states (tool output).
partialPart :: Text -> PromptPart
partialPart b = mconcat (intersperse (static "\n") (map line (T.splitOn "\n" b)))
  where
    line l = maybe (static l) (\g -> static "    " <> tool g) (T.stripPrefix "    " l)

-- | An attempt's brief: the goal's retrieval (already trimmed; empty
-- without it), the repaired proof's holes, the hints, blank lines between,
-- empty parts left out (not trimmed, unlike 'Fisher.Role.parts'). With no
-- retrieval it is today's brief exactly.
attemptBrief :: PromptPart -> PromptPart -> [PromptPart] -> PromptPart
attemptBrief retrieved brief hint = mconcat (intersperse (static "\n\n") (filter (/= mempty) (retrieved : brief : hint)))

-- | The hint from Disprove: Fisher's sentence, then the counterexample
-- (plausible's) or the countermodel (the solver's), tool output.
suspectBrief :: DisproveSource -> Text -> PromptPart
suspectBrief ViaPlausible c = static "A counterexample search (plausible) suggests this goal may be false:\n" <> tool c
suspectBrief ViaSmt c = static "An SMT solver's countermodel suggests this goal may be false:\n" <> tool c

-- | The note on a stopped attempt for the goal's next brief: the stop,
-- then the prover's last errors on the file and the attempt's last
-- message, each capped at 2,000 characters and left out when empty. The
-- sentences are Fisher's, the errors the prover's (tool), the message the
-- worker's.
stallNote :: Stop -> Text -> Maybe Text -> PromptPart
stallNote st errs msg =
  mconcat . intersperse (static "\n\n") $
    static ( "A previous attempt on this goal was stopped after " <> T.pack (show (stTurns st)) <> " turns and $"
        <> T.pack (showFFloat (Just 2) (stDollars st) "") <> " without progress on the file (" <> stopReasonText (stReason st) <> ")." )
      : [static "The prover's last errors on the file:\n" <> tool (cap2000 errs) | not (blank errs)]
      <> [static "Its last message:\n" <> toPrompt (worker (cap2000 m)) | Just m <- [msg], not (blank m)]
  where blank = T.null . T.strip

-- | What the automation did over a run: whether a step completed, and
-- why it went off (with whether a step had completed by then).
data AutoTrack = AutoTrack
  { atStepDone :: Bool
  , atOff :: Maybe (Text, Bool)
  }
  deriving (Eq, Show)

noAutoTrack :: AutoTrack
noAutoTrack = AutoTrack False Nothing

-- | Whether the automation ran: it did not go off before any step completed.
trackRan :: AutoTrack -> Bool
trackRan = maybe True snd . atOff

-- | One automation step: Nothing when the automation is off, the budget
-- is spent, or the server just failed (which turns it off, once, closing
-- the 'Lsp' and calling @off@ with why). The step gets the per-hole cap
-- cut to the budget left and is stopped there as a whole: 'Missed' when
-- it runs out (the cap function, an orchestrator's 'oCapped', under the
-- step's name). Its Lsp calls @timedOut@ when a call is cut off at its
-- cap. The track records a completed step and the server's failure.
autoStep :: (forall b. Text -> Double -> IO b -> IO (Maybe b)) -> Text -> IORef (Maybe AutoEnv) -> IORef AutoTrack -> IO Double -> (Text -> IO ()) -> IO () -> (AutoEnv -> IO (Either AutoFail a)) -> IO (Maybe (Either AutoFail a))
autoStep capped step ref track budgetLeft off timedOut act = readIORef ref >>= \case
  Nothing -> pure Nothing
  Just ae -> do
    left <- budgetLeft
    if left <= 0
      then pure Nothing
      else do
        let ac = aeConfig ae
        r <- capped step left
               (act ae {aeConfig = ac {acSeconds = min (acSeconds ac) left}, aeLsp = (aeLsp ae) {lspTimedOut = timedOut}})
        case r of
          Nothing -> pure (Just (Left Missed))
          Just (Left (LspDown why)) -> do
            writeIORef ref Nothing
            modifyIORef' track (\t -> t {atOff = Just (why, atStepDone t)})
            -- the server is stopped at once, not at the end of the run
            _ <- try @SomeException (lspClose (aeLsp ae))
            off why
            pure Nothing
          Just x -> do
            modifyIORef' track (\t -> t {atStepDone = True})
            pure (Just x)

-- | A goal's repaired partial proof spliced into the current base, when it
-- goes in cleanly (the goal's declaration replaced and nothing else, so a
-- partial that added helpers is not reused): the start and the brief with
-- its lines moved to the base's.
partialStart :: Text -> (Text, Text, Text) -> Text -> Maybe (Text, PromptPart)
partialStart g (st, ag, br) base
  | Right sp <- splice st ag base g
  , Just dA <- findDecl g (declarations ag)
  , Just dB <- findDecl g (declarations base)
  , let lsA = T.splitOn "\n" ag
        lsB = T.splitOn "\n" base
        plain = T.intercalate "\n" (take (dStart dB) lsB <> take (dEnd dA - dStart dA) (drop (dStart dA) lsA) <> drop (dEnd dB) lsB)
  , spText sp == plain =
      Just (spText sp, partialPart (shiftBrief (dStart dB - dStart dA) br))
  | otherwise = Nothing

-- | The partial proof a goal keeps after Repair (goal, the attempt's
-- start, the repair, the one kept so far): the repair when holes are left
-- and fewer than the kept one has, else the kept one.
keptPartial :: Text -> Text -> R.Repaired -> Maybe (Text, Text, Text) -> Maybe (Text, Text, Text)
keptPartial g start rp old
  | R.rpHolesAfter rp > 0 && better = Just (start, R.rpText rp, partialBrief (R.rpGoals rp))
  | otherwise = old
  where
    holesOf txt = maybe 0 (length . holesIn txt) (findDecl g (declarations txt))
    better = maybe True (\(_, o, _) -> R.rpHolesAfter rp < holesOf o) old

-- | Search the open goals of @base0@ until they are all decided or the
-- budget is spent.
runSearch :: SearchConfig -> SearchEnv -> Text -> IO (Either Text SearchReport)
runSearch cfg env base0 = do
  let order = map dName (openGoals base0)
      orch = seOrch env
  types0 <- goalTypes (seLean env) (seRoot env) (seRel env) base0 order
  case types0 of
    Left e -> pure (Left e)
    Right types -> do
      t0 <- oNow orch
      let fresh = GoalState
            { gsStatus = Open, gsTried = [], gsRunning = [], gsBy = Nothing, gsAt = Nothing, gsClosedBy = Nothing
            , gsPartial = Nothing, gsDisproof = Nothing, gsDisproveTried = False, gsSmtTried = False, gsDisproofSource = Nothing, gsDisproofWitness = Nothing, gsStall = Nothing, gsTiers = [] }
      ref <- newIORef St
        { stGoals = Map.fromList [(g, fresh) | g <- order], stBase = base0, stNext = 1, stUsage = Map.empty
        , stRunning = [], stRecords = [], stFromPartial = [], stRepairs = 0, stPrePassed = Set.empty
        , stRetrieved = Map.empty, stHelpers = Map.empty, stRetried = Set.empty
        , stHealth = emptyHealth, stLastTier = Map.empty, stModelErrors = Map.empty, stEscalations = 0
        , stStallUsed = Map.empty, stPolicyLogged = Set.empty }
      autoRef <- newIORef (seAuto env)
      track <- newIORef noAutoTrack
      -- one retriever for the run: its corpora are shared by every goal
      retrieve <- traverse (\re -> newRetriever re (seRel env)) (seRetrieval env)
      -- the harvest into the library: at the end, or on an exception
      -- with the base as it is then; once
      harvestOnce <- once $ do
        s0 <- readIORef ref
        case seRetrieval env of
          Nothing -> pure (0, 0)
          Just re -> saveRun re (seRel env) base0 (stBase s0) [] >>= \case
            Nothing -> pure (0, 0)
            Just r -> do
              t <- subtract t0 <$> oNow orch
              case r of
                Left why -> oLog orch (LibraryProblem t why) >> pure (0, 0)
                Right (l, n) -> oLog orch (LibrarySaved t l n) >> pure (l, n)
      let elapsed = subtract t0 <$> oNow orch
          modifyGoal g f = modifyIORef' ref (\s -> s {stGoals = Map.adjust f g (stGoals s)})
          goalNow g = (Map.! g) . stGoals <$> readIORef ref
          typeOf g = fromMaybe "" (Map.lookup g types)
          budgetLeft = (scBudget cfg -) <$> elapsed
          -- one automation step: Nothing when the automation is off, the
          -- budget is spent, or the server just failed (which turns it off,
          -- once). The step gets the per-hole cap cut to the budget left
          -- and is stopped there as a whole: 'Missed' when it runs out.
          -- @step@ and @g@ name it in a timeout's event.
          withAuto :: Text -> Text -> (AutoEnv -> IO (Either AutoFail a)) -> IO (Maybe (Either AutoFail a))
          withAuto step g = autoStep (oCapped orch) step autoRef track budgetLeft (\why -> elapsed >>= \t -> oLog orch (AutoOff t why))
                              (elapsed >>= \t -> oLog orch (AutoTimeout t g step))
          -- the start and brief of a goal's next attempt: the goal's
          -- retrieval, a repaired partial proof spliced into the current
          -- base when it goes in cleanly (the goal's declaration replaced
          -- and nothing else), a counterexample from Disprove, and the note
          -- on a stopped attempt (cleared at the launch that uses it)
          startFor g gs base retrieved =
            let fromPartial = gsPartial gs >>= \p -> partialStart g p base
                hint = (case gsDisproof gs of
                  Just (DSuspect c) -> [suspectBrief (fromMaybe ViaPlausible (gsDisproofSource gs)) c]
                  _ -> []) <> maybeToList (gsStall gs)
                (start, brief) = fromMaybe (base, mempty) fromPartial
             in (start, attemptBrief retrieved brief hint, isJust fromPartial)
          openStmts = map dStatement (openGoals base0)
          -- a goal's retrieval, made at its first launch; one cut off is
          -- made again, once, at the goal's next launch
          retrievalFor g = case retrieve of
            Nothing -> pure mempty
            Just get -> do
              s0 <- readIORef ref
              let done = Map.lookup g (stRetrieved s0)
                  retried = Set.member g (stRetried s0)
              case done of
                Just rt | not (rtCutOff rt) || retried -> pure (rtBrief rt)
                _ -> do
                  when (isJust done) $ modifyIORef' ref (\x -> x {stRetried = Set.insert g (stRetried x)})
                  let d = findDecl g (declarations base0)
                      q = Query g g (maybe 1 ((+ 1) . dStart) d) (maybe "" dStatement d) (Map.lookup g types) openStmts
                  rt <- get q
                  modifyIORef' ref (\x -> x {stRetrieved = Map.insert g rt (stRetrieved x)})
                  t <- elapsed
                  mapM_ (\re -> when (isJust (reFacts re)) (oLog orch (Facts t g (rtFacts rt) (rtFactsChars rt) (rtCutOff rt)))) (seRetrieval env)
                  mapM_ (\re -> when (isJust (reLibrary re)) (oLog orch (LibraryOffered t g (rtOffered rt) (rtNotes rt) (rtCutOff rt)))) (seRetrieval env)
                  pure (rtBrief rt)
          -- a goal's pre-pass runs just before its first launch; with the
          -- budget spent nothing is launched
          launchAll = do
            s <- readIORef ref
            left <- budgetLeft
            when (length (stRunning s) < scSlots cfg && left > 0) $
              case pickLaunch (launchCfg s) order (stGoals s) (stUsage s) of
                Nothing -> pure ()
                Just (g, _) | not (Set.member g (stPrePassed s)) -> do
                  modifyIORef' ref (\x -> x {stPrePassed = Set.insert g (stPrePassed x)})
                  prePass g
                  earlySmt g
                  launchAll
                Just (g, b) -> do
                  s0 <- readIORef ref
                  -- the goal's tier: its step is its counted attempts; with
                  -- every tier skipped nothing tiered is launched. A backend
                  -- outside the tiers gets none.
                  case (if tiered b then tierOf s0 g else Nothing) of
                    Just Nothing -> pure ()
                    tier -> do
                      retrieved <- retrievalFor g
                      s1 <- readIORef ref
                      t <- elapsed
                      let (start, brief, partial) = startFor g (stGoals s1 Map.! g) (stBase s1) retrieved
                          i = join tier
                          spec = AttemptSpec
                            { asId = stNext s1, asGoal = g, asBackend = b, asStart = start, asBrief = brief
                            , asModel = fmap (scTiers cfg !!) i, asTier = i }
                      a <- oSpawn orch spec
                      writeIORef ref s1
                        { stNext = stNext s1 + 1
                        , stUsage = Map.insertWith (+) b 1 (stUsage s1)
                        , stRunning = (a, spec, t) : stRunning s1
                        , stGoals = Map.adjust (\gs -> gs { gsRunning = (asId spec, b) : gsRunning gs, gsStall = Nothing
                                                          , gsTiers = gsTiers gs <> [k | Just k <- [i], k `notElem` gsTiers gs] }) g (stGoals s1)
                        , stFromPartial = [asId spec | partial] <> stFromPartial s1
                        , stLastTier = maybe id (Map.insert g) i (stLastTier s1)
                        , stStallUsed = maybe id (Map.insert (asId spec)) (gsStall (stGoals s1 Map.! g)) (stStallUsed s1) }
                      oLog orch (Launched t (asId spec) g b)
                      logPolicy g "backends" (scBackends cfg) (goalBackends cfg g) explores
                      forM_ (learnedBudget g) $ \(dflt, secs) ->
                        logPolicy g "attempt_secs" [T.pack (show dflt)] [T.pack (show secs)] (const False)
                      forM_ ((,) <$> i <*> asModel spec) $ \(k, m) -> do
                        oLog orch (TierChosen t (asId spec) g k m)
                        -- exploration is a choice of backend, never of tier
                        logPolicy g "tiers" (scTiers cfg) (map (scTiers cfg !!) (goalTiers cfg g)) (const False)
                        -- any change of a goal's tier is an escalation,
                        -- also one made because a tier was skipped; the
                        -- report's @escalations@ counts these events
                        forM_ (Map.lookup g (stLastTier s1)) $ \k0 ->
                          when (k0 /= k) $ do
                            modifyIORef' ref (\x -> x {stEscalations = stEscalations x + 1})
                            oLog orch (Escalated t g k0 k)
                      launchAll
          -- Nothing with no tiers; Just Nothing with every tier skipped.
          -- A goal with a tier policy climbs its own order.
          tierOf s g
            | null (scTiers cfg) = Nothing
            | otherwise = Just (tierIn (goalTiers cfg g) (stHealth s) (length (gsTried (stGoals s Map.! g))))
          -- whether a goal's attempts are exploration (the policy's flag)
          explores = maybe False gpExplore . flip Map.lookup (scGoalPolicy cfg)
          -- the configured budget and the goal's learned one, when that is lower
          learnedBudget g = do
            dflt <- scAttemptSecs cfg
            secs <- Map.lookup g (scGoalPolicy cfg) >>= gpAttemptSecs
            if secs < dflt then Just (dflt, secs) else Nothing
          -- the policy's order of a knob at its first use for a goal,
          -- logged once and only where it differs from the configured one
          logPolicy g knob dflt applied explore = do
            s <- readIORef ref
            when (scPolicyLogged cfg && applied /= dflt && not (Set.member (g, knob) (stPolicyLogged s))) $ do
              modifyIORef' ref (\x -> x {stPolicyLogged = Set.insert (g, knob) (stPolicyLogged x)})
              t <- elapsed
              oLog orch (PolicyApplied t g knob (T.intercalate "," dflt) (T.intercalate "," applied) (evidenceOf g knob) (explore g))
          -- the policy's evidence for a knob: its part of the goal's
          -- evidence, else all of it
          evidenceOf g knob = case gpEvidence <$> Map.lookup g (scGoalPolicy cfg) of
            Just (Object o) | Just v <- KM.lookup (K.fromText knob) o -> v
            Just v -> v
            Nothing -> Null
          tiered b = b `notElem` scUntiered cfg
          tiersOut s = not (null (scTiers cfg)) && all (skipped (stHealth s)) [0 .. length (scTiers cfg) - 1]
          -- every tier skipped and no backend outside the tiers: nothing can run
          allSkipped s = tiersOut s && all tiered (scBackends cfg)
          -- with every tier skipped only the backends outside the tiers launch
          launchCfg s
            | tiersOut s = cfg {scBackends = filter (not . tiered) (scBackends cfg)}
            | otherwise = cfg
          -- a finished attempt's result for its tier's health; a tier newly
          -- skipped is logged, and with every tier skipped the open goals
          -- nothing runs on give up
          noteTier spec failure = forM_ (asTier spec) $ \i -> do
            s <- readIORef ref
            let (h, newly) = noteResult i failure (stHealth s)
            writeIORef ref s {stHealth = h}
            when newly $ do
              t <- elapsed
              oLog orch (TierSkipped t (scTiers cfg !! i) (maybe "" modelFailureText failure))
              s' <- readIORef ref
              when (allSkipped s') $
                forM_ [g | (g, gs) <- Map.toList (stGoals s'), gsStatus gs == Open, null (gsRunning gs)] giveUp
          giveUp g = do
            modifyGoal g (\x -> x {gsStatus = GaveUp})
            t <- elapsed
            oLog orch (GoalGaveUp t g)
          record spec started outcome stats = record' spec started outcome stats Nothing
          record' spec started outcome stats ar = do
            t <- elapsed
            recordAt' t spec started outcome stats ar
          -- the attempt's record and event, ended at @t@, with its result
          -- when it returned one; the event's dollars only with the
          -- controller on
          recordAt' t spec started outcome stats ar = do
            let rec = (recordOf spec started t outcome stats)
                  {arStopped = ar >>= arStop, arCost = (ar >>= arDollars) <|> (stats >>= csCostUsd)}
            modifyIORef' ref (\s -> s {stRecords = rec : stRecords s})
            oLog orch (Finished t (asId spec) (asGoal spec) (asBackend spec) outcome stats (if scControl cfg then ar >>= arDollars else Nothing))
          recordOf spec started t outcome stats = (plainRecord (asId spec) (asGoal spec) (asBackend spec) outcome started t stats)
            {arModel = asModel spec, arTierOf = asTier spec}
          dropRunning aid = modifyIORef' ref $ \s ->
            s { stRunning = [r | r@(_, sp, _) <- stRunning s, asId sp /= aid]
              , stGoals = Map.map (\gs -> gs {gsRunning = [x | x@(i, _) <- gsRunning gs, i /= aid]}) (stGoals s) }
          priceOf = priceCancelled (scControl cfg) (seOrch env)
          cancelOthers why g = do
            s <- readIORef ref
            forM_ [r | r@(_, sp, _) <- stRunning s, asGoal sp == g] $ \(a, sp, started) -> do
              oCancel orch a
              dropRunning (asId sp)
              d <- priceOf sp
              t <- elapsed
              modifyIORef' ref (\s' -> s' {stRecords = (recordOf sp started t ("cancelled: " <> why) Nothing) {arCost = d} : stRecords s'})
              oLog orch (AttemptCancelled t (asId sp) g d)
          merge merged = do
            modifyIORef' ref (\s -> s {stBase = merged})
            seBaseChanged env merged
          -- goals whose proofs wait on others, checked again after a close
          recheckWaiting = do
            s <- readIORef ref
            let waiting = [g | (g, gs) <- Map.toList (stGoals s), gsStatus gs == Waiting]
            vs <- recheck (seLean env) (sePolicy env) (seRoot env) (seRel env) (stBase s) types waiting
            closedNow <- fmap concat . mapM (\(g, v) -> if v == C.Closed then pure [g] else pure []) $ vs
            t <- elapsed
            forM_ closedNow $ \g -> do
              modifyGoal g (\gs -> gs {gsStatus = Closed, gsAt = Just t})
              oLog orch (GoalClosed t g (maybe 0 fst (gsBy ((stGoals s) Map.! g))))
            when (not (null closedNow)) recheckWaiting
          -- an attempt's proof the kernel kept (closed, or waiting on open goals)
          -- (after the merge, as today: the merge and its seBaseChanged
          -- come before the attempt's record)
          settle spec v t = do
            s <- readIORef ref
            let g = asGoal spec
                closed = v == C.Closed
                by = if asId spec `elem` stFromPartial s then "model+repair" else "model"
            modifyGoal g (\x -> x { gsStatus = if closed then Closed else Waiting, gsBy = Just (asId spec, asBackend spec)
                                  , gsAt = if closed then Just t else Nothing
                                  , gsTried = gsTried x <> [asBackend spec], gsClosedBy = Just by })
            oLog orch ((if closed then GoalClosed else GoalWaiting) t g (asId spec))
            cancelOthers "the goal closed" g
            when closed recheckWaiting
          -- Repair on a failed attempt's file: True when the repair (with
          -- Auto filling every hole it left) closed the goal (or left it
          -- waiting), the attempt recorded by @kept@ with its note first
          tryRepair spec agent kept = do
            let g = asGoal spec
                stmt = maybe "" dStatement (findDecl g (declarations (asStart spec)))
            r <- withAuto "repair" g $ \ae ->
              if acRepair (aeConfig ae) then R.repair (aeConfig ae) (aeLsp ae) elapsed stmt agent g else pure (Right Nothing)
            case r of
              Just (Right (Just rp)) -> do
                t <- elapsed
                oLog orch (Repaired t (asId spec) g (R.rpHolesBefore rp) (R.rpHolesAfter rp) (R.rpAutoClosed rp))
                modifyIORef' ref (\s -> s {stRepairs = stRepairs s + 1})
                modifyGoal g (\x -> x {gsPartial = keptPartial g (asStart spec) rp (gsPartial x)})
                -- with no hole left the repaired file is never a start: it is
                -- checked as this attempt's proof, a close counting as
                -- model+repair
                modifyIORef' ref (\s -> s {stFromPartial = [asId spec | R.rpHolesAfter rp == 0] <> stFromPartial s})
                if R.rpHolesAfter rp > 0
                  then pure False
                  else do
                    s <- readIORef ref
                    o <- verifyAttempt (seLean env) (sePolicy env) (seRoot env) (seRel env) (asStart spec) (R.rpText rp) (stBase s) g (typeOf g)
                    case o of
                      Judged v merged _ | isKept v -> do
                        merge merged
                        kept (if v == C.Closed then "closed by repair" else "waiting after repair") :: IO ()
                        t' <- elapsed
                        settle spec v t'
                        pure True
                      _ -> pure False
              _ -> pure False
          isKept = \case
            C.Rejected _ -> False
            _ -> True
          -- Disprove, once, after a goal's second failure: plausible, and
          -- the SMT source unless it ran before the first attempt. A model
          -- the early step left uncertified (it probes the witness tactic
          -- only) gets 'certifyTactics' here once, whatever plausible says
          tryDisprove g = do
            gs <- goalNow g
            on <- readIORef autoRef
            when (isJust on && gsStatus gs == Open && length (gsTried gs) >= 2 && not (gsDisproveTried gs)) $ do
              modifyGoal g (\x -> x {gsDisproveTried = True})
              let smtSuspect = case (gsDisproof gs, gsDisproofSource gs) of
                    (Just (DSuspect _), Just ViaSmt) -> True
                    _ -> False
              disproveWith (if smtSuspect then disproveAfterSuspect else disprove) g
                ([ViaPlausible] <> [ViaSmt | not (gsSmtTried gs)])
          -- Disprove's SMT source, once, before a goal's first attempt,
          -- when the run has a solver ('aeSmt'); a model is certified by
          -- its witness tactic only, so a goal pays one probe at most here
          earlySmt g = do
            gs <- goalNow g
            on <- readIORef autoRef
            when (gsStatus gs == Open && maybe False (\ae -> isJust (aeSmt ae) && acDisprove (aeConfig ae)) on) $ do
              modifyGoal g (\x -> x {gsSmtTried = True})
              disproveWith disproveWitnessOnly g [ViaSmt]
          -- Disprove ('disprove' or 'disproveWitnessOnly') with these
          -- sources on the current base: an event per source that ran,
          -- then the verdict. A certified negation stops the goal; a
          -- counterexample is kept for its next brief; an unknown keeps a
          -- counterexample found before.
          disproveWith how g sources = do
            base <- stBase <$> readIORef ref
            r <- withAuto "disprove" g $ \ae ->
              if acDisprove (aeConfig ae)
                then how (aeSmt ae) sources (aeConfig ae) (aeLsp ae) (seLean env) (sePolicy env) (seRoot env) (seRel env) base g
                else pure (Left Missed)
            t <- elapsed
            case r of
              Just (Right (d, src, info)) -> do
                forM_ (diSteps info) $ \(s, res) -> oLog orch (DisproveRan t g (sourceName s) res)
                let srcName = maybe "" sourceName src
                case d of
                  DFalse tac -> do
                    modifyGoal g (\x -> x {gsStatus = Disproved, gsDisproof = Just d, gsDisproofSource = src, gsDisproofWitness = diWitness info})
                    oLog orch (GoalDisproved t g "false" tac srcName (diWitness info))
                    cancelOthers "the goal was disproved" g
                  DSuspect c -> do
                    modifyGoal g (\x -> x {gsDisproof = Just d, gsDisproofSource = src, gsDisproofWitness = Nothing})
                    oLog orch (GoalDisproved t g "suspect" c srcName Nothing)
                  DUnknown -> modifyGoal g $ \x -> case gsDisproof x of
                    Just (DSuspect _) -> x
                    _ -> x {gsDisproof = Just DUnknown}
              _ -> pure ()
          failed spec = do
            modifyGoal (asGoal spec) (\gs -> gs {gsTried = gsTried gs <> [asBackend spec]})
            tryDisprove (asGoal spec)
            gaveUpIfDone (asGoal spec)
          -- a goal nothing runs on gives up at its attempts allowed, or with
          -- every tier skipped
          gaveUpIfDone g = do
            s <- readIORef ref
            let gs = stGoals s Map.! g
            when (gsStatus gs == Open && null (gsRunning gs) && (length (gsTried gs) >= scMaxAttempts cfg || allSkipped s)) $
              giveUp g
          -- the pre-pass at a goal: Auto, a fill merged only when the
          -- kernel closes the goal with it
          prePass g = do
            s <- readIORef ref
            t1 <- elapsed
            r <- withAuto "auto" g $ \ae -> do
              let ac = aeConfig ae
                  tacs = goalTactics cfg g (acTactics ac)
              logPolicy g "tactics" (acTactics ac) tacs (const False)
              autoGoal ac {acTactics = tacs} (aeLsp ae) elapsed (stBase s) g
            t2 <- elapsed
            case r of
              Just (Right (filled, snips)) -> do
                o <- verifyAttempt (seLean env) (sePolicy env) (seRoot env) (seRel env) (stBase s) filled (stBase s) g (typeOf g)
                case o of
                  Judged C.Closed merged _ -> do
                    merge merged
                    t3 <- elapsed
                    modifyGoal g (\x -> x {gsStatus = Closed, gsAt = Just t3, gsClosedBy = Just "auto"})
                    oLog orch (AutoClosed t3 g (T.intercalate "; " snips) (t3 - t1))
                    recheckWaiting
                  _ -> pure ()
              Just (Left _) -> oLog orch (AutoMissed t2 g (t2 - t1))
              Nothing -> pure ()
          finish spent = do
            saved <- harvestOnce
            t <- elapsed
            s <- readIORef ref
            tr <- readIORef track
            let finalDecls = declarations (stBase s)
                goalRet g rt =
                  let proof = maybe "" dProof (findDecl g finalDecls)
                      helpers = helperStatements finalDecls (Map.findWithDefault [] g (stHelpers s))
                   in GoalRetrieval (rtFacts rt) (rtOffered rt) (usedOf (rtLemmas rt) proof helpers)
                rr = RetrievalReport [(g, goalRet g rt) | g <- order, Just rt <- [Map.lookup g (stRetrieved s)]] saved
            pure . Right $ SearchReport
              { srGoals = [(g, stGoals s Map.! g) | g <- order]
              , srAttempts = reverse (stRecords s)
              , srElapsed = t
              , srClosed = length [() | gs <- Map.elems (stGoals s), gsStatus gs == Closed]
              , srBudgetSpent = spent
              , srBase = stBase s
              , srAuto = isJust (seAuto env)
              , srRepairs = stRepairs s
              , srAutoRan = isJust (seAuto env) && trackRan tr
              , srAutoOff = fst <$> atOff tr
              , srRetrieval = rr <$ seRetrieval env
              , srControl = scControl cfg
              , srEscalations = stEscalations s }
          loop = budgetLeft >>= \left0 -> if left0 <= 0 then budgetOut else do
            launchAll
            s <- readIORef ref
            left <- budgetLeft
            -- a pre-pass in launchAll may have spent the budget
            if left <= 0 then budgetOut else if null (stRunning s)
              then do
                -- a goal can give up only once nothing runs on it
                forM_ [g | (g, gs) <- Map.toList (stGoals s), gsStatus gs == Open, length (gsTried gs) >= scMaxAttempts cfg] $ \g ->
                  modifyGoal g (\x -> x {gsStatus = GaveUp})
                when (allSkipped s) $
                  forM_ [g | (g, gs) <- Map.toList (stGoals s), gsStatus gs == Open, length (gsTried gs) < scMaxAttempts cfg] giveUp
                finish False
              else do
                  r <- oAwaitAny orch left [a | (a, _, _) <- stRunning s]
                  case r of
                    Nothing -> budgetOut
                    Just (a, res) -> do
                      -- oAwaitAny delivers only what it was given, all running
                      let (spec, started) = case [(sp, st0) | (a', sp, st0) <- stRunning s, a' == a] of
                            x : _ -> x
                            [] -> error ("runSearch: delivered an attempt not running: " <> show a)
                      dropRunning (asId spec)
                      handle spec started res
                      loop
          budgetOut = do
            s <- readIORef ref
            t <- elapsed
            forM_ (stRunning s) $ \(a, sp, started) -> do
              oCancel orch a
              d <- priceOf sp
              modifyIORef' ref (\s' -> s' {stRecords = (recordOf sp started t "cancelled: budget spent" Nothing) {arCost = d} : stRecords s'})
              -- with the controller on, each one is logged with its dollars
              when (scControl cfg) $ oLog orch (AttemptCancelled t (asId sp) (asGoal sp) d)
            modifyIORef' ref (\s' -> s' {stRunning = []})
            oLog orch (BudgetSpent t)
            finish True
          handle :: AttemptSpec -> Double -> Either SomeException AttemptResult -> IO ()
          -- the result judged, then fed to its tier's health (after its
          -- record, so a tier skipped and the goals given up come after it);
          -- an exception says nothing about the model and is not fed
          handle spec started res = do
            judge spec started res
            modifyIORef' ref (\x -> x {stStallUsed = Map.delete (asId spec) (stStallUsed x)})
            case res of
              Right ar -> noteTier spec (arFailure ar)
              Left _ -> pure ()
          judge spec started res = do
            s <- readIORef ref
            let g = asGoal spec
                gs = stGoals s Map.! g
                mar = either (const Nothing) Just res
                -- a failed attempt with a file: Repair, then the failure;
                -- the attempt is recorded once, as Repair's close when it
                -- closed, else with why it failed. A stopped attempt fails
                -- the same way, and its note (the verification's errors,
                -- @errs@) goes into the goal's next brief.
                repairOrFail why errs stats agent = do
                  t <- elapsed
                  let rec note = recordAt' t spec started note stats mar
                  closed <- tryRepair spec agent rec
                  when (not closed) $ do
                    forM_ (mar >>= \ar -> (,) ar <$> arStop ar) $ \(ar, st) ->
                      modifyGoal g (\x -> x {gsStall = Just (stallNote st errs (arLastMessage ar))})
                    rec why
                    failed spec
            -- a stopped attempt is logged before its record, its file
            -- judged as any other's
            forM_ (mar >>= \ar -> arStop ar) $ \st -> do
              t <- elapsed
              oLog orch (AttemptStopped t (asId spec) g (stopReasonText (stReason st)) (stTurns st) (stDollars st))
            case res of
              _ | gsStatus gs `elem` [Closed, Waiting, Disproved] ->
                    record' spec started "discarded: the goal was decided meanwhile" (either (const Nothing) arStats res) mar
              Left e -> record spec started ("error: " <> T.take 300 (T.pack (displayException e))) Nothing >> failed spec
              -- the model failed: neither Repair nor the kernel; kept out of
              -- the goal's attempts three times, then a failed attempt
              Right ar | Just f <- arFailure ar -> do
                record' spec started ("model error: " <> modelFailureText f) (arStats ar) mar
                -- the stall note it carried goes back for the next attempt,
                -- unless a newer one is there
                forM_ (Map.lookup (asId spec) (stStallUsed s)) $ \note ->
                  modifyGoal g (\x -> x {gsStall = gsStall x <|> Just note})
                let n = Map.findWithDefault 0 g (stModelErrors s)
                if n < 3
                  then do
                    modifyIORef' ref (\x -> x {stModelErrors = Map.insert g (n + 1) (stModelErrors x)})
                    gaveUpIfDone g
                  else failed spec
              Right ar -> case arText ar of
                Nothing -> record' spec started ("no file: " <> arNote ar) (arStats ar) mar >> failed spec
                Just agent -> do
                  o <- verifyAttempt (seLean env) (sePolicy env) (seRoot env) (seRel env) (asStart spec) agent (stBase s) g (typeOf g)
                  case o of
                    NotSpliced e -> repairOrFail ("not spliced: " <> T.pack (show e)) (T.pack (show e)) (arStats ar) agent
                    Judged (C.Rejected why) _ _ -> repairOrFail ("rejected: " <> why) why (arStats ar) agent
                    Judged v merged helpers -> do
                      merge merged
                      modifyIORef' ref (\x -> x {stHelpers = Map.insertWith (flip (<>)) g helpers (stHelpers x)})
                      t <- elapsed
                      let note = (if v == C.Closed then "closed" else "waiting on open goals")
                                   <> (if null helpers then "" else " (helpers: " <> T.intercalate ", " helpers <> ")")
                      record' spec started note (arStats ar) mar
                      settle spec v t
          -- on an exception: the harvest, then the attempts stopped
          stopRunning = readIORef ref >>= mapM_ (\(a, _, _) -> oCancel orch a) . stRunning
      harvestOnException (void harvestOnce) stopRunning loop

-- | A cancelled job's dollars (the orchestrator's 'oPrice') with the
-- controller on (the Bool); else, or when the price fails, none. An
-- asynchronous exception, and a replay's divergence, is rethrown.
priceCancelled :: Bool -> Orchestrator spec res -> spec -> IO (Maybe Double)
priceCancelled control orch sp
  | not control = pure Nothing
  | otherwise = try (oPrice orch sp) >>= \case
      Left e | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
      Left e | Just (_ :: ReplayDiverged) <- fromException e -> throwIO e
      Left (_ :: SomeException) -> pure Nothing
      Right d -> pure d

-- | The baseline the gate compares against: one attempt, on one backend,
-- at every open goal of the file at once (its goal is @*@), for the whole
-- budget. Afterwards each goal's proof is taken from its file and checked
-- as the swarm checks one.
runSingle :: SearchEnv -> Text -> Text -> IO (Either Text SearchReport)
runSingle env backend base0 = do
  let order = map dName (openGoals base0)
      orch = seOrch env
  types0 <- goalTypes (seLean env) (seRoot env) (seRel env) base0 order
  case types0 of
    Left e -> pure (Left e)
    Right types -> do
      t0 <- oNow orch
      let spec = AttemptSpec {asId = 1, asGoal = "*", asBackend = backend, asStart = base0, asBrief = mempty, asModel = Nothing, asTier = Nothing}
      oLog orch (Launched 0 1 "*" backend)
      ar <- runToEnd orch spec
      t1 <- subtract t0 <$> oNow orch
      oLog orch (Finished t1 1 "*" backend (arNote ar) (arStats ar) Nothing)
      let fresh = GoalState
            { gsStatus = Open, gsTried = [backend], gsRunning = [], gsBy = Nothing, gsAt = Nothing, gsClosedBy = Nothing
            , gsPartial = Nothing, gsDisproof = Nothing, gsDisproveTried = False, gsSmtTried = False, gsDisproofSource = Nothing, gsDisproofWitness = Nothing, gsStall = Nothing, gsTiers = [] }
          go base acc [] = pure (base, acc)
          go base acc (g : gs) = case arText ar of
            Nothing -> go base ((g, fresh {gsStatus = GaveUp}) : acc) gs
            Just agent -> do
              o <- verifyAttempt (seLean env) (sePolicy env) (seRoot env) (seRel env) base0 agent base g (fromMaybe "" (Map.lookup g types))
              case o of
                Judged v merged _ | isKept v ->
                  go merged ((g, fresh {gsStatus = if v == C.Closed then Closed else Waiting, gsBy = Just (1, backend), gsAt = Just t1}) : acc) gs
                _ -> go base ((g, fresh {gsStatus = GaveUp}) : acc) gs
          isKept = \case
            C.Closed -> True
            C.Waiting -> True
            C.Rejected _ -> False
      (base, states0) <- go base0 [] order
      let waiting = [g | (g, s) <- states0, gsStatus s == Waiting]
      vs <- recheck (seLean env) (sePolicy env) (seRoot env) (seRel env) base types waiting
      let states = [ (g, if (g, C.Closed) `elem` vs then s {gsStatus = Closed} else s) | (g, s) <- reverse states0 ]
          closed = length [() | (_, s) <- states, gsStatus s == Closed]
      forM_ [g | (g, s) <- states, gsStatus s == Closed] $ \g -> oLog orch (GoalClosed t1 g 1)
      seBaseChanged env base
      pure . Right $ SearchReport states [plainRecord 1 "*" backend (arNote ar) 0 t1 (arStats ar)] t1 closed False base False 0 False Nothing Nothing False 0

-- | One attempt spawned and waited for to the end: its result, or its
-- exception rethrown. An exception while waiting (the caller cancelled)
-- cancels the attempt; the spawn and the handler are set up masked, so
-- no asynchronous exception between them leaves the attempt running.
--
-- The caller is the only holder of the 'Running': nobody else cancels or
-- awaits it. If anyone did, 'oAwaitAny' would return Nothing at once and
-- the wait would spin.
runToEnd :: AttemptOrchestrator -> AttemptSpec -> IO AttemptResult
runToEnd orch spec = mask $ \restore -> do
  r <- oSpawn orch spec
  let wait = oAwaitAny orch 86400 [r] >>= \case
        Nothing -> wait
        Just (_, res) -> either throwIO pure res
  restore wait `onException` oCancel orch r
