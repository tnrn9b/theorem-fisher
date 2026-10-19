-- | Upstream's cron/jobs.py: schedules, the next run, and the jobs kept in
-- @cron/jobs.json@ in the Fisher home (with each run's output under
-- @cron/output/JOB/TIMESTAMP.md@). Jobs stay Python dicts ('PyVal'), in
-- upstream's key order, since the file is shared with it.
module Fisher.Cron.Jobs
  ( CronEnv (..)
  , cronDir
  , parseDuration
  , parseSchedule
  , computeNextRun
  , loadJobs
  , saveJobs
  , CreateArgs (..)
  , createArgs
  , createJob
  , getJob
  , listJobs
  , updateJob
  , pauseJob
  , resumeJob
  , triggerJob
  , removeJob
  , markJobRun
  , getDueJobs
  , saveJobOutput
  , normalizeSkills
  , applySkillFields
  , nowIso
  , zoneFor
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (forM)
import qualified Data.ByteString as BS
import Data.Char (isDigit, isSpace)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.FilePath ((</>))
import System.Posix.Files (setFileMode)

import Fisher.Cron.Croniter
import Fisher.Cron.Dt
import Fisher.Py.Value

-- | What the cron code takes from outside.
data CronEnv = CronEnv
  { ceHome :: FilePath
  -- ^ the Fisher home ('Fisher.Paths.getFisherHome')
  , ceNow :: IO Dt
  -- ^ @gauss_time.now()@
  , ceLocalOffset :: IO Int
  -- ^ the offset of @datetime.now().astimezone()@ (system local time)
  , ceNewHex :: IO Text
  -- ^ @uuid.uuid4().hex@
  }

cronDir :: CronEnv -> FilePath
cronDir env = ceHome env </> "cron"

nowIso :: CronEnv -> IO Text
nowIso env = isoformat <$> ceNow env

-- | The zone @gauss_time.now()@ answers in.
zoneFor :: Dt -> Maybe Tz
zoneFor = dtTz

-- ---------------------------------------------------------------- schedules

pyStrip :: Text -> Text
pyStrip = T.strip

-- | @parse_duration(s)@: minutes.
parseDuration :: Text -> Either Text Integer
parseDuration s0 =
  let s = T.toLower (pyStrip s0)
      (num, rest) = T.span isDigit s
      unit = T.dropWhile isSpace rest
      units = ["m", "min", "mins", "minute", "minutes", "h", "hr", "hrs", "hour", "hours", "d", "day", "days"]
      mult = case T.take 1 unit of
        "m" -> 1
        "h" -> 60
        _ -> 1440
   in if not (T.null num) && unit `elem` units
        then Right (read (T.unpack num) * mult)
        else Left ("Invalid duration: '" <> s <> "'. Use format like '30m', '2h', or '1d'")

cronLike :: Text -> Bool
cronLike p = not (T.null p) && T.all (\c -> isDigit c || c `elem` ("*-,/" :: String)) p

isoDateStart :: Text -> Bool
isoDateStart s =
  T.length s >= 10 && T.all isDigit (T.take 4 s) && T.index s 4 == '-' && T.all isDigit (T.take 2 (T.drop 5 s))
    && T.index s 7 == '-' && T.all isDigit (T.take 2 (T.drop 8 s))

-- | @parse_schedule(schedule)@
parseSchedule :: CronEnv -> Text -> IO (Either Text PyVal)
parseSchedule env schedule0 = do
  let schedule = pyStrip schedule0
      original = schedule
      lower = T.toLower schedule
      parts = T.words schedule
  if "every " `T.isPrefixOf` lower
    then pure $ do
      m <- parseDuration (pyStrip (T.drop 6 schedule))
      Right (PDict [("kind", PStr "interval"), ("minutes", PInt m), ("display", PStr ("every " <> T.pack (show m) <> "m"))])
    else
      if length parts >= 5 && all cronLike (take 5 parts)
        then pure $ case expand schedule of
          Left (CronError _ e) -> Left ("Invalid cron expression '" <> schedule <> "': " <> e)
          Right _ -> Right (PDict [("kind", PStr "cron"), ("expr", PStr schedule), ("display", PStr schedule)])
        else
          if "T" `T.isInfixOf` schedule || isoDateStart schedule
            then pure $ case fromIsoformat (T.replace "Z" "+00:00" schedule) of
              Left e -> Left ("Invalid timestamp '" <> schedule <> "': " <> e)
              Right dt -> Right (PDict [("kind", PStr "once"), ("run_at", PStr (isoformat dt)), ("display", PStr ("once at " <> strftimeYmdHm dt))])
            else case parseDuration schedule of
              Right m -> do
                now <- ceNow env
                pure (Right (PDict [("kind", PStr "once"), ("run_at", PStr (isoformat (addMinutes m now))), ("display", PStr ("once in " <> original))]))
              Left _ ->
                pure . Left $
                  "Invalid schedule '" <> original <> "'. Use:\n"
                    <> "  - Duration: '30m', '2h', '1d' (one-shot)\n"
                    <> "  - Interval: 'every 30m', 'every 2h' (recurring)\n"
                    <> "  - Cron: '0 9 * * *' (cron expression)\n"
                    <> "  - Timestamp: '2026-02-03T14:00:00' (one-shot at time)"

-- | @_ensure_aware(dt)@: in the zone of @gauss_time.now()@.
ensureAware :: CronEnv -> Dt -> IO Dt
ensureAware env dt = do
  now <- ceNow env
  let target = fromMaybe (TzFixed 0) (dtTz now)
  case dtTz dt of
    Nothing -> do
      off <- ceLocalOffset env
      pure (astimezone target (replaceTz (Just (TzFixed off)) dt))
    Just _ -> pure (astimezone target dt)

-- | @compute_next_run(schedule, last_run_at)@
computeNextRun :: CronEnv -> PyVal -> Maybe Text -> IO (Either Text (Maybe Text))
computeNextRun env sched lastRun = do
  now <- ceNow env
  case get "kind" sched of
    Just (PStr "once") -> case fromIsoformat (str' "run_at") of
      Left e -> pure (Left e)
      Right d -> do
        runAt <- ensureAware env d
        pure (Right (if dtInstantMicros runAt > dtInstantMicros now then Just (str' "run_at") else Nothing))
    Just (PStr "interval") -> do
      let minutes = case get "minutes" sched of
            Just (PInt m) -> m
            _ -> 0
      case lastRun of
        Just l | not (T.null l) -> case fromIsoformat l of
          Left e -> pure (Left e)
          Right d -> do
            lastA <- ensureAware env d
            pure (Right (Just (isoformat (addMinutes minutes lastA))))
        _ -> pure (Right (Just (isoformat (addMinutes minutes now))))
    Just (PStr "cron") -> pure $ case expand (str' "expr") of
      Left (CronError _ e) -> Left e
      Right ex -> case getNext (newIter ex now) of
        Left (CronError _ e) -> Left e
        Right (d, _) -> Right (Just (isoformat d))
    _ -> pure (Right Nothing)
  where
    str' k = maybe "" pyStrOf (get k sched)

-- ---------------------------------------------------------------- storage

jobsFile :: CronEnv -> FilePath
jobsFile env = cronDir env </> "jobs.json"

ensureDirs :: CronEnv -> IO ()
ensureDirs env = do
  createDirectoryIfMissing True (cronDir env </> "output")
  mapM_ (\d -> try @SomeException (setFileMode d 0o700)) [cronDir env, cronDir env </> "output"]

-- | @load_jobs()@
loadJobs :: CronEnv -> IO [PyVal]
loadJobs env = do
  ensureDirs env
  ex <- doesFileExist (jobsFile env)
  if not ex
    then pure []
    else do
      r <- try @SomeException (BS.readFile (jobsFile env))
      pure $ case r of
        Right b | Right d <- loads b, Just (PList js) <- get "jobs" d -> js
        _ -> []

-- | @save_jobs(jobs)@: written whole, then moved over the old file.
saveJobs :: CronEnv -> [PyVal] -> IO ()
saveJobs env jobs = do
  ensureDirs env
  now <- nowIso env
  let tmp = cronDir env </> ".jobs_fisher.tmp"
  BS.writeFile tmp (TE.encodeUtf8 (dumpsIndent True 2 (PDict [("jobs", PList jobs), ("updated_at", PStr now)])))
  renameFile tmp (jobsFile env)
  _ <- try @SomeException (setFileMode (jobsFile env) 0o600)
  pure ()

-- | @_normalize_skill_list(skill, skills)@
normalizeSkills :: Maybe PyVal -> Maybe PyVal -> [Text]
normalizeSkills skill skills =
  let raw = case skills of
        Nothing -> maybe [] (\s -> [s | truthy s]) skill
        Just PNull -> maybe [] (\s -> [s | truthy s]) skill
        Just (PStr s) -> [PStr s]
        Just (PList xs) -> xs
        Just v -> [v]
      texts = [T.strip (case v of PNull -> ""; _ -> pyStrOf v) | v <- raw]
   in foldl (\acc t -> if T.null t || t `elem` acc then acc else acc <> [t]) [] texts

-- | @_apply_skill_fields(job)@
applySkillFields :: PyVal -> PyVal
applySkillFields job =
  let skills = normalizeSkills (get "skill" job) (get "skills" job)
   in set "skill" (maybe PNull PStr (headMay skills)) (set "skills" (PList (map PStr skills)) job)
  where
    headMay = \case
      x : _ -> Just x
      [] -> Nothing

data CreateArgs = CreateArgs
  { caPrompt :: Text
  , caSchedule :: Text
  , caName :: Maybe Text
  , caRepeat :: Maybe Integer
  , caDeliver :: Maybe Text
  , caOrigin :: Maybe PyVal
  , caSkills :: [Text]
  , caModel :: Maybe Text
  , caProvider :: Maybe Text
  , caBaseUrl :: Maybe Text
  }

createArgs :: Text -> Text -> CreateArgs
createArgs p s = CreateArgs p s Nothing Nothing Nothing Nothing [] Nothing Nothing Nothing

optText :: Maybe Text -> PyVal
optText = maybe PNull PStr

-- | @create_job(...)@
createJob :: CronEnv -> CreateArgs -> IO (Either Text PyVal)
createJob env a = do
  parsed <- parseSchedule env (caSchedule a)
  case parsed of
    Left e -> pure (Left e)
    Right sched -> do
      let isOnce = get "kind" sched == Just (PStr "once")
          repeat' = case caRepeat a of
            Nothing | isOnce -> Just 1
            r -> r
          deliver = fromMaybe (if isJust (caOrigin a) then "origin" else "local") (caDeliver a)
          skills = normalizeSkills Nothing (Just (PList (map PStr (caSkills a))))
          norm f = case fmap T.strip f of
            Just t | not (T.null t) -> Just t
            _ -> Nothing
          baseUrl = case fmap (T.dropWhileEnd (== '/') . T.strip) (caBaseUrl a) of
            Just t | not (T.null t) -> Just t
            _ -> Nothing
          labelSource = if not (T.null (caPrompt a)) then caPrompt a else case skills of
            s : _ -> s
            [] -> "cron job"
          name = case caName a of
            Just n | not (T.null n) -> n
            _ -> T.strip (T.take 50 labelSource)
      hex <- ceNewHex env
      now <- nowIso env
      next <- computeNextRun env sched Nothing
      case next of
        Left e -> pure (Left e)
        Right nextRun -> do
          let job =
                PDict
                  [ ("id", PStr (T.take 12 hex)), ("name", PStr name), ("prompt", PStr (caPrompt a))
                  , ("skills", PList (map PStr skills)), ("skill", maybe PNull PStr (case skills of s : _ -> Just s; [] -> Nothing))
                  , ("model", optText (norm (caModel a))), ("provider", optText (norm (caProvider a))), ("base_url", optText baseUrl)
                  , ("schedule", sched), ("schedule_display", fromMaybe (PStr (caSchedule a)) (get "display" sched))
                  , ("repeat", PDict [("times", maybe PNull PInt repeat'), ("completed", PInt 0)])
                  , ("enabled", PBool True), ("state", PStr "scheduled"), ("paused_at", PNull), ("paused_reason", PNull)
                  , ("created_at", PStr now), ("next_run_at", optText nextRun), ("last_run_at", PNull)
                  , ("last_status", PNull), ("last_error", PNull), ("deliver", PStr deliver), ("origin", fromMaybe PNull (caOrigin a)) ]
          jobs <- loadJobs env
          saveJobs env (jobs <> [job])
          pure (Right job)

jobId :: PyVal -> Text
jobId j = maybe "" pyStrOf (get "id" j)

-- | @get_job(job_id)@
getJob :: CronEnv -> Text -> IO (Maybe PyVal)
getJob env jid = do
  jobs <- loadJobs env
  pure (applySkillFields <$> lookupJob jid jobs)

lookupJob :: Text -> [PyVal] -> Maybe PyVal
lookupJob jid jobs = case [j | j <- jobs, jobId j == jid] of
  j : _ -> Just j
  [] -> Nothing

-- | @list_jobs(include_disabled)@
listJobs :: CronEnv -> Bool -> IO [PyVal]
listJobs env includeDisabled = do
  jobs <- map applySkillFields <$> loadJobs env
  pure (if includeDisabled then jobs else filter enabled jobs)

enabled :: PyVal -> Bool
enabled j = maybe True truthy (get "enabled" j)

-- | @{**job, **updates}@
merge :: PyVal -> [(Text, PyVal)] -> PyVal
merge j kvs = foldl (\acc (k, v) -> set k v acc) j kvs

-- | @update_job(job_id, updates)@
updateJob :: CronEnv -> Text -> [(Text, PyVal)] -> IO (Either Text (Maybe PyVal))
updateJob env jid updates = do
  jobs <- loadJobs env
  case break ((== jid) . jobId) jobs of
    (_, []) -> pure (Right Nothing)
    (pre, job : post) -> do
      let updated0 = applySkillFields (merge job updates)
          has' k = isJust (lookup k updates)
          updated1 =
            if has' "skills" || has' "skill"
              then
                let s = normalizeSkills (get "skill" updated0) (get "skills" updated0)
                 in set "skill" (case s of x : _ -> PStr x; [] -> PNull) (set "skills" (PList (map PStr s)) updated0)
              else updated0
          sched = fromMaybe PNull (get "schedule" updated1)
          paused j = get "state" j == Just (PStr "paused")
      r1 <-
        if has' "schedule"
          then do
            let disp = fromMaybe (fromMaybe (fromMaybe PNull (get "schedule_display" updated1)) (get "display" sched)) (lookup "schedule_display" updates)
                u = set "schedule_display" disp updated1
            if not (paused u)
              then fmap (\n -> set "next_run_at" (optText n) u) <$> computeNextRun env sched Nothing
              else pure (Right u)
          else pure (Right updated1)
      case r1 of
        Left e -> pure (Left e)
        Right u1 -> do
          r2 <-
            if enabled u1 && not (paused u1) && not (maybe False truthy (get "next_run_at" u1))
              then fmap (\n -> set "next_run_at" (optText n) u1) <$> computeNextRun env (fromMaybe PNull (get "schedule" u1)) Nothing
              else pure (Right u1)
          case r2 of
            Left e -> pure (Left e)
            Right u2 -> do
              saveJobs env (pre <> [u2] <> post)
              pure (Right (Just (applySkillFields u2)))

-- | @pause_job(job_id, reason)@
pauseJob :: CronEnv -> Text -> Maybe Text -> IO (Either Text (Maybe PyVal))
pauseJob env jid reason = do
  now <- nowIso env
  updateJob env jid [("enabled", PBool False), ("state", PStr "paused"), ("paused_at", PStr now), ("paused_reason", optText reason)]

-- | @resume_job(job_id)@
resumeJob :: CronEnv -> Text -> IO (Either Text (Maybe PyVal))
resumeJob env jid =
  getJob env jid >>= \case
    Nothing -> pure (Right Nothing)
    Just job -> do
      n <- computeNextRun env (fromMaybe PNull (get "schedule" job)) Nothing
      case n of
        Left e -> pure (Left e)
        Right next -> updateJob env jid [("enabled", PBool True), ("state", PStr "scheduled"), ("paused_at", PNull), ("paused_reason", PNull), ("next_run_at", optText next)]

-- | @trigger_job(job_id)@
triggerJob :: CronEnv -> Text -> IO (Either Text (Maybe PyVal))
triggerJob env jid =
  getJob env jid >>= \case
    Nothing -> pure (Right Nothing)
    Just _ -> do
      now <- nowIso env
      updateJob env jid [("enabled", PBool True), ("state", PStr "scheduled"), ("paused_at", PNull), ("paused_reason", PNull), ("next_run_at", PStr now)]

-- | @remove_job(job_id)@
removeJob :: CronEnv -> Text -> IO Bool
removeJob env jid = do
  jobs <- loadJobs env
  let kept = filter ((/= jid) . jobId) jobs
  if length kept < length jobs then saveJobs env kept >> pure True else pure False

-- | @mark_job_run(job_id, success, error)@
markJobRun :: CronEnv -> Text -> Bool -> Maybe Text -> IO ()
markJobRun env jid ok err = do
  jobs <- loadJobs env
  case break ((== jid) . jobId) jobs of
    (_, []) -> saveJobs env jobs
    (pre, job0 : post) -> do
      now <- nowIso env
      let job1 =
            set "last_error" (if ok then PNull else optText err)
              . set "last_status" (PStr (if ok then "ok" else "error"))
              . set "last_run_at" (PStr now)
              $ job0
          rep = get "repeat" job1
      case rep of
        Just r@(PDict _) | truthy r -> do
          let completed = (case get "completed" r of Just (PInt c) -> c; _ -> 0) + 1
              r' = set "completed" (PInt completed) r
              job2 = set "repeat" r' job1
          case get "times" r' of
            Just (PInt times) | completed >= times -> saveJobs env (pre <> post)
            _ -> finish pre post job2 now
        _ -> finish pre post job1 now
  where
    finish pre post job now = do
      n <- computeNextRun env (fromMaybe PNull (get "schedule" job)) (Just now)
      let next = either (const Nothing) id n
          job' = set "next_run_at" (optText next) job
          job'' = case next of
            Nothing -> set "state" (PStr "completed") (set "enabled" (PBool False) job')
            Just _ | get "state" job' /= Just (PStr "paused") -> set "state" (PStr "scheduled") job'
            _ -> job'
      saveJobs env (pre <> [job''] <> post)

-- | @get_due_jobs()@
getDueJobs :: CronEnv -> IO [PyVal]
getDueJobs env = do
  now <- ceNow env
  jobs <- map applySkillFields <$> loadJobs env
  fmap concat . forM jobs $ \job ->
    if not (enabled job)
      then pure []
      else case get "next_run_at" job of
        Just (PStr nr) | not (T.null nr), Right d <- fromIsoformat nr -> do
          a <- ensureAware env d
          pure [job | dtInstantMicros a <= dtInstantMicros now]
        _ -> pure []

-- | @save_job_output(job_id, output)@: the file's path.
saveJobOutput :: CronEnv -> Text -> Text -> IO FilePath
saveJobOutput env jid output = do
  ensureDirs env
  now <- ceNow env
  let dir = cronDir env </> "output" </> T.unpack jid
      file = dir </> T.unpack (strftimeFile now) <> ".md"
      tmp = dir </> ".output_fisher.tmp"
  createDirectoryIfMissing True dir
  _ <- try @SomeException (setFileMode dir 0o700)
  BS.writeFile tmp (TE.encodeUtf8 output)
  renameFile tmp file
  _ <- try @SomeException (setFileMode file 0o600)
  pure file
