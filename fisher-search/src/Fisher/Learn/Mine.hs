-- | The mining pass: a run's attempts read back from the run log, their
-- mistakes found by the rules ("Fisher.Learn.Mistakes"), the failures the
-- rules do not explain shown to the judge ("Fisher.Learn.Judge") under
-- the dollar caps, and the lessons' lifecycle ("Fisher.Learn.Lessons")
-- moved on: evidence, the run noted in force, promotion, retirement, the
-- two files regenerated.
--
-- The rules:
--
-- * A run's attempts ('attemptViews'): a search run's are its
--   @search-attempt@ children, each with the goal and shape of its
--   @attempt@ event, its class (the row's outcome) and its outcome text
--   (the search run's last @finished@ event for its number, the number
--   being the id's suffix after @<search run>-@; the class when there is
--   none). Any other run (a launch) is one attempt, the run itself, its
--   outcome text that of its last @outcome@ event. The attempts' own
--   @mistake@ events are left out of what the classifiers see.
-- * Once per attempt: with @again@ False an attempt that already carries
--   a @mistake@ event is not classified again, but the evidence of its
--   rule events is recorded again (a pass stopped between writing the
--   events and recording their evidence loses nothing; for a run already
--   counted it changes nothing, since a run is in @evidence_runs@ once).
--   With @again@ True the rules run again and only the hits not already
--   logged (by kind and name) are written; every hit's evidence is
--   recorded again, which changes nothing for a run already counted. The
--   judge never sees an attempt twice: one with a @judged@ or @self@ event
--   is not judged again, even with @again@.
-- * A hit is a @mistake@ event on the attempt ('mistakeJson' with its
--   @key@, @kind:name@ or the kind alone for a kind with no name), written
--   first, then evidence under that key, the kind's template rendered as
--   the text (no evidence when a value is not a name, 'renderTemplate',
--   or the text does not pass the sanitiser; the hit still counts). The
--   evidence run is the mined run (the search, not the attempt), so three
--   runs promote and three attempts of one run do not.
-- * The residue: an attempt of class @failed@, @timeout@ or @stalled@
--   with no rule hit. With a judge, while the run's judge dollars are under
--   'meRunCap', the day's ('spendOn', the UTC day of 'meNow') under
--   'meDayCap' and the run's judge calls under 'meCallCap', one call
--   ('judgeAttempt': a reply that cannot be read is asked again once)
--   under 'meJudgeTimeout' seconds; every call's dollars go to 'addSpend'
--   (a call that timed out is charged 'judgeCostEstimate'). The judge phase
--   of a pass stops 'meJudgeBudget' seconds after its first call.
--   A verdict, its texts redacted, is evidence for its lesson (@judged@,
--   when the judge was sure enough) and its tell-yourself (@self@), at most
--   one agreement per row for the call, then a @mistake@ event of each
--   kind. A reply that still cannot be read is a @judged@ event with
--   @parsed: false@, so the attempt is not paid for again. No credit, a
--   call that failed (twice) or broke, and a call past its timeout stop the
--   judging for the pass (for 'mineSince', its every run; the time budget
--   too is the whole pass's), with a warning; the attempt is left for a
--   later pass. Every residue attempt not judged counts in 'mrUnjudged'.
-- * Then, once per run: the run noted in force ('noteInForce') with the
--   ids of its last @lessons@ event, its attempts and its hits per key
--   (a run with no attempts is not noted); 'promote'; 'retire';
--   'regenerate', whose failure is a warning.
-- * Passes are serialised per Fisher home by an exclusive lock on
--   @fisher/lessons.lock@; a pass that cannot take it in 30 s does
--   nothing and warns.
--
-- * A failure in one attempt or one step (noting in force, promotion,
--   retirement, the files) is a warning and the rest of the pass goes on.
--   An asynchronous exception (an interrupt) stops the pass and goes on up,
--   the lock released.
--
-- 'mineRun' may still throw (reading the run's attempts can); its callers
-- wrap it so mining never stops a run.
module Fisher.Learn.Mine
  ( MineEnv (..)
  , MineReport (..)
  , mineReportJson
  , mineRun
  , mineRunWithin
  , mineSince
  , attemptViews
  , lessonsShown
  , judgeCostEstimate
  , mineLockPath
  , withMineLock
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeAsyncException, SomeException, displayException, fromException, mask, onException, throwIO, try)
import Control.Monad (forM, forM_, unless, when)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.IORef
import Data.List (find, nub, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing, listToMaybe, mapMaybe)
import Data.Scientific (toBoundedInteger)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import System.Directory (createDirectoryIfMissing)
import System.FileLock (SharedExclusive (..), tryLockFile, unlockFile)
import System.FilePath (takeDirectory, (</>))
import System.Timeout (timeout)
import Text.Read (readMaybe)

import Fisher.Learn.Judge (Verdict (..), judgeAttempt, judgeKey)
import Fisher.Learn.Lessons (Evidence (..), Limits, noteInForce, promote, recordEvidence, regenerate, retire)
import Fisher.Learn.Mistakes (AttemptView (..), Mistake (..), classify, mistakeJson, renderMistakeKind)
import Fisher.Lessons.File (Template (..), renderTemplate)
import Fisher.Lessons.Prompt (PromptLessons (..), promptLessons)
import Fisher.Redact (redactSensitiveText)
import Fisher.Runlog
import Fisher.Search.GraphJobs (Ask, Asked (..))

-- | What a pass needs: the log, the time (seconds since the epoch; its UTC
-- day is the day of the judge's day cap), the Fisher home (the global
-- file, the lock), the project's root (its file) and key, the judge
-- (Nothing: rules only), the per-run and per-day judge caps in dollars,
-- the per-run cap on judge calls, the seconds one judge call and a pass's
-- judge phase may take, the lifecycle's numbers, the templates, and where
-- warnings go.
data MineEnv = MineEnv
  { meRunlog :: Runlog
  , meNow :: Double
  , meFisherHome :: FilePath
  , meRoot :: FilePath
  , meProjectKey :: Text
  , meJudge :: Maybe Ask
  , meRunCap :: Double
  , meDayCap :: Double
  , meCallCap :: Int
  , meJudgeTimeout :: Double
  , meJudgeBudget :: Double
  , meLimits :: Limits
  , meTemplates :: [Template]
  , meWarn :: Text -> IO ()
  }

-- | What a pass did: rule hits written, attempts judged, residue attempts
-- left unjudged, and the ids of the lessons it promoted and retired.
data MineReport = MineReport
  { mrMined :: Int
  , mrJudged :: Int
  , mrUnjudged :: Int
  , mrPromoted :: [Int]
  , mrRetired :: [Int]
  }
  deriving (Eq, Show)

instance Semigroup MineReport where
  a <> b =
    MineReport
      (mrMined a + mrMined b) (mrJudged a + mrJudged b) (mrUnjudged a + mrUnjudged b)
      (nub (mrPromoted a <> mrPromoted b)) (nub (mrRetired a <> mrRetired b))

instance Monoid MineReport where
  mempty = MineReport 0 0 0 [] []

-- | The report's @lessons@ object: @{mined, judged, unjudged, promoted,
-- retired}@, the last two lists of lesson ids.
mineReportJson :: MineReport -> Value
mineReportJson r =
  object
    [ "mined" .= mrMined r, "judged" .= mrJudged r, "unjudged" .= mrUnjudged r
    , "promoted" .= mrPromoted r, "retired" .= mrRetired r ]

-- The lock ---------------------------------------------------------------

-- | @fisherHome/fisher/lessons.lock@.
mineLockPath :: FilePath -> FilePath
mineLockPath home = home </> "fisher" </> "lessons.lock"

-- | Run an action holding the exclusive lock on a file (made when it is
-- not there), waiting for it up to some seconds; Nothing when it could not
-- be taken in time.
withMineLock :: Double -> FilePath -> IO a -> IO (Maybe a)
withMineLock wait path act = do
  createDirectoryIfMissing True (takeDirectory path)
  start <- clock
  let go = do
        got <- mask $ \restore ->
          tryLockFile path Exclusive >>= \case
            Nothing -> pure Nothing
            Just l -> do
              x <- restore act `onException` unlockFile l
              unlockFile l
              pure (Just x)
        case got of
          Just x -> pure (Just x)
          Nothing -> do
            t <- clock
            if t - start >= wait then pure Nothing else threadDelay 100000 >> go
  go
  where
    clock = realToFrac <$> getPOSIXTime :: IO Double

locked :: Double -> MineEnv -> IO MineReport -> IO MineReport
locked wait env act =
  withMineLock wait (mineLockPath (meFisherHome env)) act >>= \case
    Just r -> pure r
    Nothing -> do
      meWarn env ("skipped: another mining pass held " <> T.pack (mineLockPath (meFisherHome env)) <> " for "
                    <> T.pack (show (round wait :: Int)) <> " s")
      pure mempty

-- The pass ---------------------------------------------------------------

-- | Mine a search or launch run (its id); @again@ re-runs the rules on
-- attempts already mined.
mineRun :: MineEnv -> Text -> Bool -> IO MineReport
mineRun = mineRunWithin 30

-- | 'mineRun' waiting for the lock up to some seconds.
mineRunWithin :: Double -> MineEnv -> Text -> Bool -> IO MineReport
mineRunWithin wait env rid again = locked wait env $ do
  p <- newPass
  r <- mineOne env p rid again
  r <$ warnRefused env p

-- | Mine every search and launch run of the env's project started at or
-- after a time (every one with Nothing), oldest first, under one lock.
mineSince :: MineEnv -> Maybe Double -> Bool -> IO MineReport
mineSince env since again = locked 30 env $ do
  let rl = meRunlog env
  runs <- concat <$> forM [SearchRun, LaunchRun] (\k -> listRuns rl since (Just (meProjectKey env)) (Just k) 0)
  p <- newPass
  r <- mconcat <$> mapM (\x -> mineOne env p (rrId x) again) (sortOn (\x -> (rrStartedAt x, rrId x)) runs)
  r <$ warnRefused env p

-- One warning for the pass: how many lesson texts the sanitiser refused.
warnRefused :: MineEnv -> Pass -> IO ()
warnRefused env p = do
  n <- readIORef (pRefused p)
  when (n > 0) $ meWarn env (T.pack (show n) <> " lesson texts refused by the sanitiser")

-- | The dollars a judge call is charged when its cost is not known (an
-- unpriced route, a call stopped at its timeout), toward both caps.
judgeCostEstimate :: Double
judgeCostEstimate = 0.02

-- What a pass carries along (one run, or every run of 'mineSince'):
-- whether the judging stopped (no credit, a broken call, a timeout, the
-- time budget), when its first judge call started, and the kinds already
-- warned of for having no template.
data Pass = Pass
  { pBroke :: IORef Bool
  , pStarted :: IORef (Maybe Double)
  , pNoTemplate :: IORef (Set.Set Text)
  , pRefused :: IORef Int
  -- ^ the lesson texts 'recordEvidence' refused (the sanitiser)
  }

newPass :: IO Pass
newPass = Pass <$> newIORef False <*> newIORef Nothing <*> newIORef Set.empty <*> newIORef 0

-- What the pass over one run carries along: the run's judge dollars and
-- calls (the caps are per run), and the pass's state.
data Judging = Judging
  { jSpent :: IORef Double
  , jCalls :: IORef Int
  , jBroke :: IORef Bool
  , jStarted :: IORef (Maybe Double)
  , jNoTemplate :: IORef (Set.Set Text)
  , jRefused :: IORef Int
  }

-- What an attempt came to: rule hits written, judged, left unjudged, and
-- the keys of every mistake event it now carries.
data Mined = Mined { miRules :: Int, miJudged :: Int, miUnjudged :: Int, miKeys :: [Text] }

mineOne :: MineEnv -> Pass -> Text -> Bool -> IO MineReport
mineOne env pass rid again = do
  let rl = meRunlog env
      lim = meLimits env
      pk = meProjectKey env
  getRun rl rid >>= \case
    Nothing -> pure mempty
    Just _ -> do
      attempts <- attemptsOf rl rid
      st <- (\spent calls -> Judging spent calls (pBroke pass) (pStarted pass) (pNoTemplate pass) (pRefused pass)) <$> newIORef 0 <*> newIORef 0
      mined <- forM attempts $ \(aid, av, prior) ->
        guarded env aid (Mined 0 0 0 []) (mineAttempt env st rid aid av prior again)
      unless (null attempts) . guarded env "noting the run in force" () $ do
        shown <- shownIds rl rid
        let hits = Map.fromListWith (+) [(k, 1) | m <- mined, k <- miKeys m]
        noteInForce rl lim rid shown (length attempts) hits
      promoted <- guarded env "promotion" [] (promote rl (meNow env) lim (meTemplates env) pk)
      forM_ [r | r <- promoted, lrStatus r == Candidate, lrKind r `elem` ["judged", "self"]] $ \r ->
        meWarn env ("lesson " <> T.pack (show (lrId r)) <> " (" <> lrKind r <> ") is no longer shown: it waits for your approval, fisher lessons show "
                      <> T.pack (show (lrId r)) <> ", then fisher lessons pin " <> T.pack (show (lrId r)))
      retired <- guarded env "retirement" [] (retire rl (meNow env) lim pk)
      guarded env "the lessons files" () $
        regenerate rl (meFisherHome env) (meRoot env) pk >>= either (\e -> meWarn env ("the lessons files: " <> e)) pure
      pure
        MineReport
          { mrMined = sum (map miRules mined)
          , mrJudged = sum (map miJudged mined)
          , mrUnjudged = sum (map miUnjudged mined)
          , mrPromoted = [lrId r | r <- promoted, lrStatus r == Active]
          , mrRetired = map lrId retired }

-- One step of a pass: a synchronous exception is a warning (naming what
-- failed) and the step's default, so one bad attempt or step does not stop
-- the rest; an asynchronous one goes on up.
guarded :: MineEnv -> Text -> a -> IO a -> IO a
guarded env what dflt act =
  try act >>= \case
    Right x -> pure x
    Left e
      | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
      | otherwise -> do
          meWarn env (what <> ": " <> T.take 300 (T.pack (displayException (e :: SomeException))))
          pure dflt

-- The ids of the run's last @lessons@ event.
shownIds :: Runlog -> Text -> IO [Int]
shownIds rl rid = do
  es <- runEvents rl rid
  pure $ case reverse [d | (_, _, "lessons", d) <- es] of
    d : _ | Just (Array xs) <- field "ids" d -> [i | Number n <- toList xs, Just i <- [toBoundedInteger n]]
    _ -> []

mineAttempt :: MineEnv -> Judging -> Text -> Text -> AttemptView -> [Value] -> Bool -> IO Mined
mineAttempt env st rid aid av prior again
  | not again && not (null prior) = do
      -- the evidence of the events already written (once per run: a run
      -- already counted changes nothing)
      forM_ prior $ \d ->
        when (kindOf d `notElem` judgeKinds && not (T.null (kindOf d))) $
          ruleEvidence env st rid aid (kindOf d) (nameOf d) (strOf "new" d) (ruleKeyOf (kindOf d) (nameOf d))
      pure (Mined 0 0 0 (nub priorKeys))
  | otherwise = do
      let hits = classify av
          fresh = [m | m <- hits, (renderMistakeKind (mKind m), mName m) `notElem` priorRules]
      -- the events first, then the evidence: a pass stopped between them
      -- records the evidence next time (above)
      forM_ fresh $ \m ->
        event rl aid (timeAt (mSeq m)) "mistake" (withKey (Just (ruleKey m)) (mistakeJson (avGoal av) (avShape av) m))
      forM_ hits $ \m -> ruleEvidence env st rid aid (renderMistakeKind (mKind m)) (mName m) (mNew m) (ruleKey m)
      let newKeys = nub (map ruleKey fresh)
          residue = avClass av `elem` ["failed", "timeout", "stalled"] && null hits && null priorRules && not priorJudged
      if not residue
        then pure (Mined (length fresh) 0 0 (nub (priorKeys <> newKeys)))
        else do
          j <- judgeOne env st rid aid av
          pure $ case j of
            Nothing -> Mined (length fresh) 0 1 (nub (priorKeys <> newKeys))
            Just ks -> Mined (length fresh) 1 0 (nub (priorKeys <> newKeys <> ks))
  where
    rl = meRunlog env
    kindOf d = case field "kind" d of Just (String k) -> k; _ -> ""
    nameOf = strOf "name"
    strOf k d = case field k d of Just (String n) -> Just n; _ -> Nothing
    priorRules = [(kindOf d, nameOf d) | d <- prior, kindOf d `notElem` judgeKinds]
    priorJudged = any ((`elem` judgeKinds) . kindOf) prior
    priorKeys = mapMaybe keyOf prior
    keyOf d
      | kindOf d == "lean3_syntax" = Just "lean3_syntax"
      | otherwise = case field "key" d of
      Just (String k) -> Just k
      _ | kindOf d `notElem` judgeKinds, not (T.null (kindOf d)) -> Just (kindOf d <> maybe "" (":" <>) (nameOf d))
      _ -> Nothing
    timeAt s = maybe lastT (\(_, t, _, _) -> t) (find (\(x, _, _, _) -> x == s) (avEvents av))
    lastT = maybe 0 (\(_, t, _, _) -> t) (listToMaybe (reverse (avEvents av)))

judgeKinds :: [Text]
judgeKinds = ["judged", "self"]

-- | A rule hit's lesson key: @kind:name@, or the kind for a hit with no
-- name. @lean3_syntax@ keys on the kind alone: its template names no
-- marker, so one key per marker would only repeat one line.
ruleKey :: Mistake -> Text
ruleKey m = ruleKeyOf (renderMistakeKind (mKind m)) (mName m)

ruleKeyOf :: Text -> Maybe Text -> Text
ruleKeyOf kind name
  | kind == "lean3_syntax" = kind
  | otherwise = kind <> maybe "" (":" <>) name

-- The evidence of a rule hit (its kind, name, replacement and key) in a
-- run's attempt: the kind's template rendered, when every value is a name.
ruleEvidence :: MineEnv -> Judging -> Text -> Text -> Text -> Maybe Text -> Maybe Text -> Text -> IO ()
ruleEvidence env st rid aid kind name new key = case find ((== kind) . tKind) (meTemplates env) of
  Just t ->
    forM_ (renderTemplate t (Map.fromList ([("name", n) | Just n <- [name]] <> [("new", n) | Just n <- [new]]))) $ \txt ->
      refusedIf st . recordEvidence (meRunlog env) (meNow env) $
        Evidence
          { evRun = rid, evAttempt = aid, evProjectKey = meProjectKey env, evKind = kind, evKey = key
          , evText = txt, evJudged = False }
  Nothing -> do
    first <- atomicModifyIORef' (jNoTemplate st) (\s -> (Set.insert kind s, not (Set.member kind s)))
    when first $ meWarn env ("no lesson template for " <> kind <> "; its hits make no lesson")

-- One judge call for a residue attempt, under the caps, the timeout and
-- the pass's time budget: the keys of the mistake events it wrote (none
-- for a reply that could not be read), or Nothing when the attempt was not
-- judged.
judgeOne :: MineEnv -> Judging -> Text -> Text -> AttemptView -> IO (Maybe [Text])
judgeOne env st rid aid av = case meJudge env of
  Nothing -> pure Nothing
  Just ask -> do
    broke <- readIORef (jBroke st)
    spent <- readIORef (jSpent st)
    calls <- readIORef (jCalls st)
    today <- spendOn rl day
    now <- clock
    started <- readIORef (jStarted st)
    let elapsed = maybe 0 (now -) started
        left = meJudgeBudget env - elapsed
    if broke || spent >= meRunCap env || today >= meDayCap env || calls >= meCallCap env
      then pure Nothing
      else
        if left <= 0
          then stop ("the judge phase reached its time budget of " <> secs (meJudgeBudget env) <> "; the rest of the run is left unjudged")
          else do
            when (isNothing started) $ writeIORef (jStarted st) (Just now)
            modifyIORef' (jCalls st) (+ 1)
            let limit = min (meJudgeTimeout env) left
            timeout (round (limit * 1000000)) (judgeAttempt ask av) >>= \case
              Nothing -> do
                charge judgeCostEstimate
                stop ("the judge took over " <> secs limit <> " on " <> aid <> " (its time limit); the rest of the run is left unjudged")
              Just (NoCredit c) -> do
                charge c
                stop "the judge has no credit; the rest of the run is left unjudged"
              Just (Asked c (Left e))
                | broken e -> do
                    charge c
                    stop ("the judge failed on " <> aid <> ": " <> e <> "; the rest of the run is left unjudged")
                | otherwise -> do
                    charge c
                    meWarn env ("the judge's reply for " <> aid <> " could not be read: " <> e)
                    -- marked judged, so a later pass does not pay for it again
                    event rl aid lastT "mistake" . withKey Nothing . object $
                      base "judged" <> ["parsed" .= False, "error" .= e, "lesson" .= Null]
                    pure (Just [])
              Just (Asked c (Right v)) -> do
                charge c
                Just <$> verdictEvents (redacted v)
  where
    rl = meRunlog env
    day = T.pack (formatTime defaultTimeLocale "%Y-%m-%d" (posixSecondsToUTCTime (realToFrac (meNow env))))
    clock = realToFrac <$> getPOSIXTime :: IO Double
    secs x = T.pack (show (max 1 (round x :: Int))) <> " s"
    stop why = do
      writeIORef (jBroke st) True
      meWarn env why
      pure Nothing
    -- a call that failed (after its retry) or broke, not a reply that
    -- could not be read
    broken e = any (`T.isPrefixOf` e) ["the model call failed", "the call broke"]
    charge c = when (c > 0) $ do
      modifyIORef' (jSpent st) (+ c)
      addSpend rl day c
    redacted v =
      v { vMistake = redactSensitiveText (vMistake v), vLesson = redactSensitiveText <$> vLesson v
        , vTellYourself = redactSensitiveText (vTellYourself v) }
    evidence kind txt = do
      r <- recordEvidence rl (meNow env) Evidence
        { evRun = rid, evAttempt = aid, evProjectKey = meProjectKey env, evKind = kind, evKey = judgeKey kind txt
        , evText = txt, evJudged = True }
      r <$ when (isNothing r) (modifyIORef' (jRefused st) (+ 1))
    lastSeq = maybe 0 (\(s, _, _, _) -> s) (listToMaybe (reverse (avEvents av)))
    lastT = maybe 0 (\(_, t, _, _) -> t) (listToMaybe (reverse (avEvents av)))
    base kind = ["kind" .= (kind :: Text), "seq" .= lastSeq, "goal" .= avGoal av, "shape" .= avShape av]
    -- the evidence first, then the events (which carry the rows' keys; a
    -- text the sanitiser refused has none)
    verdictEvents v = do
      lesson <- maybe (pure Nothing) (evidence "judged") (vLesson v)
      self <- evidence "self" (vTellYourself v)
      event rl aid lastT "mistake" . withKey (lrKey <$> lesson) . object $
        base "judged" <> ["text" .= vMistake v, "lesson" .= vLesson v, "confidence" .= vConfidence v]
      event rl aid lastT "mistake" . withKey (lrKey <$> self) . object $ base "self" <> ["text" .= vTellYourself v]
      pure (nub [lrKey l | Just l <- [lesson, self]])

-- Count a sighting the sanitiser refused.
refusedIf :: Judging -> IO (Maybe LessonRow) -> IO ()
refusedIf st act = act >>= \r -> when (isNothing r) (modifyIORef' (jRefused st) (+ 1))

withKey :: Maybe Text -> Value -> Value
withKey k = \case
  Object o -> Object (KM.insert "key" (maybe Null String k) o)
  v -> v

field :: Text -> Value -> Maybe Value
field k = \case
  Object o -> KM.lookup (K.fromText k) o
  _ -> Nothing

-- The attempts ----------------------------------------------------------

-- | A run's attempts as the classifiers see them, by run id: a search
-- run's children, any other run itself.
attemptViews :: Runlog -> Text -> IO [(Text, AttemptView)]
attemptViews rl rid = map (\(a, v, _) -> (a, v)) <$> attemptsOf rl rid

-- Each attempt with its view and its @mistake@ events' data.
attemptsOf :: Runlog -> Text -> IO [(Text, AttemptView, [Value])]
attemptsOf rl rid =
  getRun rl rid >>= \case
    Nothing -> pure []
    Just run
      | rrKind run == SearchRun -> do
          parent <- runEvents rl rid
          let finished =
                Map.fromList
                  [ (n, o) | (_, _, "finished", d) <- parent
                  , Just (Number x) <- [field "attempt" d], Just n <- [toBoundedInteger x :: Maybe Int]
                  , Just (String o) <- [field "outcome" d] ]
          kids <- filter ((== SearchAttempt) . rrKind) <$> childRuns rl rid
          forM kids $ \k -> do
            es <- runEvents rl (rrId k)
            let n = T.stripPrefix (rid <> "-") (rrId k) >>= readMaybe . T.unpack
                cls = fromMaybe "" (rrOutcome k)
                outcome = fromMaybe cls (n >>= (`Map.lookup` finished))
                att = listToMaybe [d | (_, _, "attempt", d) <- es]
                str key = case att >>= field key of Just (String x) -> x; _ -> ""
            pure (rrId k, view es outcome cls (str "goal") (str "shape"), mistakesOf es)
      | otherwise -> do
          es <- runEvents rl rid
          let cls = fromMaybe "" (rrOutcome run)
              outcome = case reverse [o | (_, _, "outcome", d) <- es, Just (String o) <- [field "outcome" d]] of
                o : _ -> o
                [] -> cls
          pure [(rid, view es outcome cls "" "", mistakesOf es)]
  where
    view es outcome cls goal shape =
      AttemptView
        { avEvents = [e | e@(_, _, k, _) <- es, k /= "mistake"]
        , avOutcomeText = outcome, avClass = cls, avGoal = goal, avShape = shape }
    mistakesOf es = [d | (_, _, "mistake", d) <- es]

-- | The ids of the lessons a run of a project (its root) is shown: the
-- rows whose lines the prompt block keeps ('promptLessons', built from
-- the shown rows of @runs.db@ as every backend's prompt is, after the
-- sanitiser and the caps). A row whose file is foreign is not shown.
lessonsShown :: Runlog -> Bool -> FilePath -> FilePath -> IO [Int]
lessonsShown rl autoJudged home root = plIds <$> promptLessons rl autoJudged home root
