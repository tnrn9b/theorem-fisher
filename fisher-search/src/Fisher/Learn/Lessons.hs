-- | The lessons' lifecycle in @runs.db@: evidence makes candidates,
-- counts promote them, a measured rate retires them, a cap bounds each
-- file, and the two lessons files are regenerated from what is active.
--
-- The rules:
--
-- * Evidence ('recordEvidence'): the text first passes the redactor
--   ('redactSensitiveText') and then 'sanitiseLesson'; a text the
--   sanitiser refuses is not recorded at all. A rule kind keys on the
--   caller's 'evKey' (@kind:name@); a @judged@ or @self@ lesson first looks
--   for a @judged@ or @self@ row of the project (any status, never a rule
--   row) whose text is 'sameLesson', and else keys on 'judgeKey' of its
--   kind and text (@judged:@ or @self:@ first, so never a rule's key). The run is added to @evidence_runs@ when new; a sighting with
--   'evJudged' adds an agreement only when its attempt has not agreed on
--   that row before (@agreed_attempts@), so one judge call is at most one
--   agreement however many of its texts land on the row; a retired or
--   dropped row is left as it is (its key is not proposed again).
-- * Promotion ('promote'): a candidate of a rule kind with 'lPromoteRuns'
--   distinct evidence runs becomes active. A @judged@ or @self@ candidate
--   (model-written text, from transcripts) is never activated by its
--   counts: with 'lPromoteRuns' runs and 'lAgreements' agreements it is
--   'awaitingApproval', and a human pins it (@fisher lessons pin@, the
--   panel's Pin). 'lAutoJudged' (@fisher.lessons.auto_judged@) promotes
--   those on their counts too. With 'lAutoJudged' off, an active (not
--   pinned) @judged@ or @self@ row of the project or @*@ goes back to
--   candidate (so one promoted under the old rule waits for a pin too). A
--   rule kind (never @judged@ or @self@, whatever 'lAutoJudged' says) whose
--   template is global and that is seen (candidate, active or pinned) in
--   'lGlobalProjects' project keys becomes an active row under project key
--   @*@, with the union of the evidence of the rule rows of that key and
--   the newest of their texts (a row of another kind under the key is never
--   counted or copied); the per-project rows stay as they are. While the
--   @*@ row of a key is active, pinned, retired or dropped, a project
--   candidate of that key is not promoted and a project's active row of it
--   is left out of the project file (a retired or dropped global key is
--   not proposed again per project). 'promote' answers the rows whose
--   status differs at the end of the call from its start.
-- * The cap: at most 'lMax' active and pinned lines per file. Over it,
--   the active line whose latest evidence run started first goes back to
--   candidate, with its runs in force and while-active counters cleared,
--   until the file fits (pinned lines count and are never demoted).
-- * In force ('noteInForce'): once per run, idempotent. An active or pinned
--   row the run showed gains the run in @runs_active@ (once) and the run's
--   hits of its key and attempts in the while-active counters. Every
--   candidate of the run's project, and every @*@ candidate, gains the run
--   (once) in its before window ('lrBeforeWindow': run, hits, attempts),
--   which keeps the last 'lRetireRuns' runs; the before counters are the
--   window's sums.
-- * Retirement ('retire'): an active row (never a pinned one) with at
--   least 'lRetireRuns' runs in force and 'lMinBefore' before runs retires
--   when its rate per attempt in force is at least 'lRetireRatio' times
--   its rate before. The rate in force is cumulative over every run in
--   force so far: a lesson kept at its tenth run is checked again after each
--   later run and can retire if its rate climbs.
-- * The files ('regenerate'): the project's active and pinned rows to
--   @.fisher/lessons.md@, the @*@ rows to the global file, oldest first,
--   through 'writeLessonsFile' only. A file with nothing to write that is
--   not there is not made.
-- * The actions by hand ('applyAction', @fisher lessons@ and the web
--   app's Lessons panel): pin makes a candidate or active row pinned
--   (activated now unless it was), a retired one pinned afresh (activated
--   now, its runs in force and while-active counters cleared, so the
--   measurement starts over), and refuses a dropped or pinned one; unpin
--   makes a pinned row active (a @judged@ or @self@ one a candidate, its
--   in-force counters cleared, so it is proposed again until it is pinned)
--   and refuses any other; drop makes any row
--   but a dropped one dropped.
--
-- Every change to a stored row is a read and a write in one transaction
-- ('modifyLesson', 'mergeLesson'), so two mining passes at once never write
-- an older copy of a row over the other's change.
module Fisher.Learn.Lessons
  ( Evidence (..)
  , Limits (..)
  , defaultLimits
  , awaitingApproval
  , recordEvidence
  , promote
  , noteInForce
  , retire
  , regenerate
  , regenerateGlobal
  , fileRows
  , lessonActions
  , actionTarget
  , applyTo
  , applyAction
  , lessonRoot
  , globalKey
  ) where

import Control.Applicative ((<|>))
import Control.Monad (forM, forM_, unless, void, when)
import Data.List (find, nub, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist)

import Fisher.Learn.Judge (judgeKey, sameLesson)
import Fisher.Lessons.Prompt (globalKey, shownRows)
import Fisher.Lessons.File (Template (..), globalLessonsPath, loadTemplates, projectLessonsPath, renderTemplate, sanitiseLesson, writeLessonsFile)
import Fisher.Redact (redactSensitiveText)
import Fisher.Runlog

-- | One sighting of a lesson in a run: the run, the attempt, its project
-- key, the mistake kind (or @judged@, @self@), the key (@kind:name@ for a
-- rule kind; unused for @judged@ and @self@), the rendered lesson text,
-- and whether a judge call produced it (an agreement, once per attempt).
data Evidence = Evidence
  { evRun :: Text
  , evAttempt :: Text
  , evProjectKey :: Text
  , evKind :: Text
  , evKey :: Text
  , evText :: Text
  , evJudged :: Bool
  }
  deriving (Eq, Show)

-- | The numbers of the lifecycle.
data Limits = Limits
  { lPromoteRuns :: Int
  -- ^ distinct evidence runs to promote
  , lAgreements :: Int
  -- ^ judge agreements a @judged@ or @self@ lesson also needs
  , lRetireRuns :: Int
  -- ^ runs in force before the rate is compared, and the before window
  , lRetireRatio :: Double
  -- ^ retire when the rate in force is at least this times the rate before
  , lMinBefore :: Int
  -- ^ before runs the comparison needs
  , lMax :: Int
  -- ^ active and pinned lines per file
  , lGlobalProjects :: Int
  -- ^ project keys a global kind is seen in to become a global lesson
  , lAutoJudged :: Bool
  -- ^ promote @judged@ and @self@ lessons on their counts, as rules are
  -- (else they wait for a pin)
  }
  deriving (Eq, Show)

defaultLimits :: Limits
defaultLimits =
  Limits
    { lPromoteRuns = 3
    , lAgreements = 2
    , lRetireRuns = 10
    , lRetireRatio = 0.8
    , lMinBefore = 5
    , lMax = 12
    , lGlobalProjects = 3
    , lAutoJudged = False
    }


judgeKinds :: [Text]
judgeKinds = ["judged", "self"]

isJudgeKind :: Text -> Bool
isJudgeKind = (`elem` judgeKinds)

-- | Enough evidence to promote: 'lPromoteRuns' distinct runs, and for
-- @judged@ and @self@ also 'lAgreements' agreements.
meetsCounts :: Limits -> LessonRow -> Bool
meetsCounts lim r =
  length (nub (lrEvidenceRuns r)) >= lPromoteRuns lim
    && (not (isJudgeKind (lrKind r)) || lrAgreements r >= lAgreements lim)

-- | A @judged@ or @self@ candidate with the counts that would promote a
-- rule, which waits for a human to pin it (never, with 'lAutoJudged').
awaitingApproval :: Limits -> LessonRow -> Bool
awaitingApproval lim r =
  not (lAutoJudged lim) && lrStatus r == Candidate && isJudgeKind (lrKind r) && meetsCounts lim r

shown :: LessonStatus -> Bool
shown s = s == Active || s == Pinned

closedOff :: LessonStatus -> Bool
closedOff s = s == Retired || s == Dropped

-- Evidence ---------------------------------------------------------------

-- | Record a sighting at a time and answer the row as stored; 'Nothing'
-- (nothing written) when its text, redacted, does not pass
-- 'sanitiseLesson'. A judge result's texts agree once per attempt.
recordEvidence :: Runlog -> Double -> Evidence -> IO (Maybe LessonRow)
recordEvidence rl now ev0 = case sanitiseLesson (redactSensitiveText (evText ev0)) of
  Nothing -> pure Nothing
  Just txt -> Just <$> record ev0 {evText = txt}
  where
    record ev = do
      key <-
        if isJudgeKind (evKind ev)
          then do
            rows <- filter (isJudgeKind . lrKind) <$> listLessons rl (Just (evProjectKey ev)) []
            let own = judgeKey (evKind ev) (evText ev)
            pure (maybe own lrKey (find (\r -> lrKey r == own || sameLesson (lrText r) (evText ev)) rows))
          else pure (evKey ev)
      mergeLesson rl (evProjectKey ev) key (fresh ev key) (merge ev)
    agreed ev = [evAttempt ev | evJudged ev]
    fresh ev key =
      (blank (evProjectKey ev) (evKind ev) key (evText ev) now)
        { lrEvidenceRuns = [evRun ev], lrAgreements = length (agreed ev), lrAgreedAttempts = agreed ev }
    merge ev r
      | closedOff (lrStatus r) = r
      | otherwise =
          let new = [a | a <- agreed ev, a `notElem` lrAgreedAttempts r]
           in r { lrEvidenceRuns = addRun (evRun ev) (lrEvidenceRuns r)
                , lrAgreements = lrAgreements r + length new
                , lrAgreedAttempts = lrAgreedAttempts r <> new }

blank :: Text -> Text -> Text -> Text -> Double -> LessonRow
blank pk kind key txt now =
  LessonRow
    { lrId = 0, lrProjectKey = pk, lrKind = kind, lrKey = key, lrText = txt, lrStatus = Candidate
    , lrEvidenceRuns = [], lrAgreements = 0, lrFirstSeen = now, lrActivatedAt = Nothing, lrRetiredAt = Nothing
    , lrRunsActive = [], lrHitsWhileActive = 0, lrAttemptsWhileActive = 0, lrHitsBefore = 0
    , lrAttemptsBefore = 0, lrRunsBefore = 0, lrBeforeWindow = [], lrAgreedAttempts = [] }

addRun :: Text -> [Text] -> [Text]
addRun x xs = if x `elem` xs then xs else xs <> [x]

-- A row with this before window and the counters that sum it.
withWindow :: [(Text, Int, Int)] -> LessonRow -> LessonRow
withWindow w r =
  r { lrBeforeWindow = w, lrRunsBefore = length w
    , lrHitsBefore = sum [h | (_, h, _) <- w], lrAttemptsBefore = sum [a | (_, _, a) <- w] }

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

-- Status changes, as functions of the stored row (applied by 'modifyLesson').
activate :: Double -> LessonRow -> LessonRow
activate now r = r {lrStatus = Active, lrActivatedAt = Just now, lrRetiredAt = Nothing}

demote :: LessonRow -> LessonRow
demote r =
  r { lrStatus = Candidate, lrActivatedAt = Nothing, lrRetiredAt = Nothing
    , lrRunsActive = [], lrHitsWhileActive = 0, lrAttemptsWhileActive = 0 }

-- Promotion --------------------------------------------------------------

-- | Promote a project's candidates at a time, make the global lessons the
-- evidence calls for, and apply the cap to the project file and the
-- global file. The templates say which kinds may be global. Answers the
-- rows whose status differs at the end of the call from its start, as
-- they now are.
promote :: Runlog -> Double -> Limits -> [Template] -> Text -> IO [LessonRow]
promote rl now lim templates pk = do
  before <- statuses
  unless (lAutoJudged lim) $ do
    shownJudged <- filter (isJudgeKind . lrKind) <$> ((<>) <$> listLessons rl (Just pk) [Active] <*> listLessons rl (Just globalKey) [Active])
    forM_ shownJudged $ \r -> modifyLesson rl (lrId r) (\x -> if lrStatus x == Active && isJudgeKind (lrKind x) then demote x else x)
  promoteGlobal rl now lim templates pk
  blocked <- blockedKeys rl
  rows <- listLessons rl (Just pk) [Candidate]
  forM_ rows $ \r ->
    unless (lrKey r `elem` blocked) . void $
      modifyLesson rl (lrId r) (\x -> if lrStatus x == Candidate && meets x then activate now x else x)
  applyCap rl lim pk
  applyCap rl lim globalKey
  after <- statuses
  let changed = [i | (i, s) <- M.toList after, M.lookup i before /= Just s]
  catMaybes <$> mapM (getLesson rl) changed
  where
    statuses = do
      rs <- (<>) <$> listLessons rl (Just pk) [] <*> listLessons rl (Just globalKey) []
      pure (M.fromList [(lrId r, lrStatus r) | r <- rs])
    meets r = meetsCounts lim r && (lAutoJudged lim || not (isJudgeKind (lrKind r)))

-- The keys whose @*@ row is shown, retired or dropped: no project row of
-- them is promoted or written.
blockedKeys :: Runlog -> IO [Text]
blockedKeys rl = do
  stars <- listLessons rl (Just globalKey) []
  pure [lrKey s | s <- stars, lrStatus s /= Candidate]

-- For each key of the project whose kind is global (a rule kind only,
-- never @judged@ or @self@): when the per-project rule
-- rows of that key (candidate, active or pinned; a row of another kind
-- under the key is never counted nor copied) span enough project keys,
-- the @*@ row is made active (or refreshed, when it is already shown) with
-- the union of their evidence, the sum of their agreements and the newest
-- text; a new one starts from the projects' before windows (the last
-- 'lRetireRuns' runs of them, taken in project order). A retired or
-- dropped @*@ row is left alone.
promoteGlobal :: Runlog -> Double -> Limits -> [Template] -> Text -> IO ()
promoteGlobal rl now lim templates pk = do
  mine <- listLessons rl (Just pk) seen
  let globalKinds = [tKind t | t <- templates, tGlobal t, not (isJudgeKind (tKind t))]
      keys = nub [lrKey r | r <- mine, lrKind r `elem` globalKinds]
  unless (null keys) $ do
    everywhere <- listLessons rl Nothing seen
    forM_ keys $ \k -> do
      let rows = [r | r <- everywhere, lrKey r == k, lrProjectKey r /= globalKey, lrKind r `elem` globalKinds]
          projects = nub (map lrProjectKey rows)
          runs = foldl (flip addRun) [] (concatMap lrEvidenceRuns rows)
      case reverse rows of
        newest : _ | length projects >= lGlobalProjects lim -> do
          let fresh =
                activate now . withWindow (lastN (lRetireRuns lim) (concatMap lrBeforeWindow rows)) $
                  (blank globalKey (lrKind newest) k (lrText newest) now)
                    { lrEvidenceRuns = runs, lrAgreements = sum (map lrAgreements rows) }
              refresh s
                | closedOff (lrStatus s) = s
                | otherwise =
                    (if shown (lrStatus s) then id else activate now)
                      s { lrEvidenceRuns = foldl (flip addRun) (lrEvidenceRuns s) runs
                        , lrAgreements = sum (map lrAgreements rows)
                        , lrText = lrText newest }
          () <$ mergeLesson rl globalKey k fresh refresh
        _ -> pure ()
  where
    seen = [Candidate, Active, Pinned]

-- | The lines a file shows ('shownRows'): for the global file the shown
-- @*@ rows; for a project's, its shown rows less the active ones whose key
-- is blocked. The prompt block is built from the same rows.
fileRows :: Runlog -> Text -> IO [LessonRow]
fileRows = shownRows

applyCap :: Runlog -> Limits -> Text -> IO ()
applyCap rl lim pk = do
  rows <- fileRows rl pk
  let over = length rows - lMax lim
  when (over > 0) $ do
    dated <- forM [r | r <- rows, lrStatus r == Active] $ \r -> do
      t <- latestEvidence rl r
      pure (t, lrId r)
    forM_ (take over (sort dated)) $ \(_, i) ->
      modifyLesson rl i (\x -> if lrStatus x == Active then demote x else x)

-- When a row's latest evidence run started (its first sighting when the
-- run is not logged).
latestEvidence :: Runlog -> LessonRow -> IO Double
latestEvidence rl r = do
  ts <- mapM (fmap (fmap rrStartedAt) . getRun rl) (lrEvidenceRuns r)
  pure (case catMaybes ts of [] -> lrFirstSeen r; xs -> maximum xs)

-- In force ---------------------------------------------------------------

-- | Note a run: its id, the lesson ids it showed, its attempts, and the
-- mistake hits per lesson key in it. Noting a run again changes nothing.
noteInForce :: Runlog -> Limits -> Text -> [Int] -> Int -> Map Text Int -> IO ()
noteInForce rl lim rid ids attempts hits = do
  forM_ (nub ids) $ \i -> modifyLesson rl i inForce
  pk <- (>>= rrProjectKey) <$> getRun rl rid
  own <- maybe (pure []) (\k -> if k == globalKey then pure [] else listLessons rl (Just k) [Candidate]) pk
  stars <- listLessons rl (Just globalKey) [Candidate]
  forM_ (own <> stars) $ \r -> modifyLesson rl (lrId r) window
  where
    hitsOf r = M.findWithDefault 0 (lrKey r) hits
    inForce r
      | shown (lrStatus r) && rid `notElem` lrRunsActive r =
          r { lrRunsActive = lrRunsActive r <> [rid]
            , lrHitsWhileActive = lrHitsWhileActive r + hitsOf r
            , lrAttemptsWhileActive = lrAttemptsWhileActive r + attempts }
      | otherwise = r
    window r
      | lrStatus r /= Candidate || rid `elem` [x | (x, _, _) <- lrBeforeWindow r] = r
      | otherwise = withWindow (lastN (lRetireRuns lim) (lrBeforeWindow r <> [(rid, hitsOf r, attempts)])) r

-- Retirement -------------------------------------------------------------

-- | Retire, at a time, the active rows of a project key (and the global
-- ones) whose rate did not fall; answers them as they now are.
retire :: Runlog -> Double -> Limits -> Text -> IO [LessonRow]
retire rl now lim pk = do
  rows <- (<>) <$> listLessons rl (Just pk) [Active] <*> (if pk == globalKey then pure [] else listLessons rl (Just globalKey) [Active])
  done <- forM [r | r <- rows, due r] $ \r -> do
    m <- modifyLesson rl (lrId r) (\x -> if lrStatus x == Active && due x then x {lrStatus = Retired, lrRetiredAt = Just now} else x)
    pure (case m of Just x | lrStatus x == Retired -> Just x; _ -> Nothing)
  pure (catMaybes done)
  where
    due r =
      length (lrRunsActive r) >= lRetireRuns lim
        && lrRunsBefore r >= lMinBefore lim
        && lrAttemptsBefore r > 0
        && lrAttemptsWhileActive r > 0
        && rate (lrHitsWhileActive r) (lrAttemptsWhileActive r) >= lRetireRatio lim * rate (lrHitsBefore r) (lrAttemptsBefore r)
    rate :: Int -> Int -> Double
    rate h a = fromIntegral h / fromIntegral a

-- The files --------------------------------------------------------------

-- | Write the project file (under the project root) and the global file
-- (under the Fisher home) from the shown rows, after 'globalRuleTexts'. A
-- file that is foreign or cannot be written is left as it is and named in
-- the 'Left', the other is still written.
regenerate :: Runlog -> FilePath -> FilePath -> Text -> IO (Either Text ())
regenerate rl home root pk = do
  globalRuleTexts rl home
  proj <- fileRows rl pk
  glob <- fileRows rl globalKey
  errs <- mapM writeRows [(projectLessonsPath root, proj), (globalLessonsPath home, glob)]
  pure (failures errs)

-- | Only the global file, from the shown @*@ rows (for a change whose
-- project's root is not known).
regenerateGlobal :: Runlog -> FilePath -> IO (Either Text ())
regenerateGlobal rl home = do
  globalRuleTexts rl home
  glob <- fileRows rl globalKey
  failures . pure <$> writeRows (globalLessonsPath home, glob)

-- | The templates are the source of a rule's text: each active or pinned
-- @*@ row of a global rule kind (the only kinds 'promoteGlobal' makes)
-- gets its kind's template (under the Fisher home, 'loadTemplates'; a
-- global kind names nothing, so no placeholder is filled) when that
-- differs, so a text written over it by any other path does not stay.
-- Templates that cannot be read, a kind with no global template, or a
-- rendering the sanitiser refuses leave the row as it is.
globalRuleTexts :: Runlog -> FilePath -> IO ()
globalRuleTexts rl home =
  loadTemplates home >>= \case
    Left _ -> pure ()
    Right ts -> do
      rows <- listLessons rl (Just globalKey) [Active, Pinned]
      forM_ [r | r <- rows, not (isJudgeKind (lrKind r))] $ \r ->
        case [x | t <- ts, tKind t == lrKind r, tGlobal t, Just x <- [renderTemplate t M.empty >>= sanitiseLesson]] of
          x : _ | x /= lrText r -> void (modifyLesson rl (lrId r) (\y -> if lrKind y == lrKind r then y {lrText = x} else y))
          _ -> pure ()

failures :: [Maybe Text] -> Either Text ()
failures errs = case catMaybes errs of
  [] -> Right ()
  es -> Left (T.intercalate "; " es)

writeRows :: (FilePath, [LessonRow]) -> IO (Maybe Text)
writeRows (path, rows) = do
  there <- doesFileExist path
  if null rows && not there
    then pure Nothing
    else either (\e -> Just (T.pack path <> ": " <> e)) (const Nothing) <$> writeLessonsFile path (map lrText rows)

-- By hand -----------------------------------------------------------------

-- | The actions 'applyAction' knows.
lessonActions :: [Text]
lessonActions = ["pin", "unpin", "drop"]

-- | The status an action gives a row of a status, or why it does not
-- apply.
actionTarget :: Text -> LessonStatus -> Either Text LessonStatus
actionTarget action st = case action of
  "pin" -> case st of
    Pinned -> Left "already pinned"
    Dropped -> Left "dropped"
    _ -> Right Pinned
  "unpin" -> if st == Pinned then Right Active else Left "not pinned"
  "drop" -> if st == Dropped then Left "already dropped" else Right Dropped
  _ -> Left ("unknown action " <> action <> " (one of " <> T.intercalate ", " lessonActions <> ")")

-- | An action at a time on a row: the row changed, or why the action does
-- not apply. The timestamps follow 'setLessonStatus': active sets
-- @activated_at@ (kept when it was already active) and clears
-- @retired_at@; pinned keeps an earlier @activated_at@ (else now) and
-- clears @retired_at@, and a retired row is pinned afresh (activated now,
-- its runs in force and while-active counters cleared, so the measurement
-- starts over); dropped sets @retired_at@; an unpinned @judged@ or @self@
-- row is a candidate ('demote').
applyTo :: Text -> Double -> LessonRow -> Either Text LessonRow
applyTo action now l = to . judgedUnpin <$> actionTarget action (lrStatus l)
  where
    -- an unpinned judged or self lesson needs a pin again: a candidate
    judgedUnpin st
      | action == "unpin" && isJudgeKind (lrKind l) && st == Active = Candidate
      | otherwise = st
    was = lrStatus l
    to = \case
      Pinned
        | was == Retired ->
            l { lrStatus = Pinned, lrActivatedAt = Just now, lrRetiredAt = Nothing, lrRunsActive = []
              , lrHitsWhileActive = 0, lrAttemptsWhileActive = 0 }
        | otherwise -> l {lrStatus = Pinned, lrActivatedAt = lrActivatedAt l <|> Just now, lrRetiredAt = Nothing}
      Active -> l {lrStatus = Active, lrActivatedAt = if was == Active then lrActivatedAt l else Just now, lrRetiredAt = Nothing}
      Candidate -> demote l
      st -> l {lrStatus = st, lrRetiredAt = if was == st then lrRetiredAt l else Just now}

-- | Apply an action at a time to a lesson by id, decided and written in
-- one write transaction ('modifyLessonE', 'applyTo'), so a mining pass
-- that changes the row meanwhile is never decided on a stale copy. The
-- row as stored after it, @Right Nothing@ for an unknown id, or why the
-- action does not apply (nothing written). The lessons files are the
-- caller's to regenerate.
applyAction :: Runlog -> Double -> Text -> Int -> IO (Either Text (Maybe LessonRow))
applyAction rl now action i = modifyLessonE rl i (applyTo action now)

-- | The root of a lesson's project, from a logged run of its evidence
-- (Nothing for a global lesson or one with no such run).
lessonRoot :: Runlog -> LessonRow -> IO (Maybe FilePath)
lessonRoot rl l = do
  rs <- forM (lrEvidenceRuns l) (getRun rl)
  pure (listToMaybe [p | Just r <- rs, rrProjectKey r == Just (lrProjectKey l), Just p <- [rrProject r]])
