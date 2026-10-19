-- | Upstream's tools/cronjob_tools.py @cronjob(...)@: one action-oriented
-- entry point over the job store, answering in JSON (indent 2), which the
-- @cron@ CLI reads back.
module Fisher.Cron.Tool
  ( cronjob
  , scanCronPrompt
  , repeatDisplay
  , formatJob
  , originFromEnv
  ) where

import Data.Char (ord)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (showHex)

import Fisher.Cron.Jobs
import qualified Fisher.Py.Re as Re
import Fisher.Py.Value

threatPatterns :: [(Text, Text)]
threatPatterns =
  [ ("ignore\\s+(?:\\w+\\s+)*(?:previous|all|above|prior)\\s+(?:\\w+\\s+)*instructions", "prompt_injection")
  , ("do\\s+not\\s+tell\\s+the\\s+user", "deception_hide")
  , ("system\\s+prompt\\s+override", "sys_prompt_override")
  , ("disregard\\s+(your|all|any)\\s+(instructions|rules|guidelines)", "disregard_rules")
  , ("curl\\s+[^\\n]*\\$\\{?\\w*(KEY|TOKEN|SECRET|PASSWORD|CREDENTIAL|API)", "exfil_curl")
  , ("wget\\s+[^\\n]*\\$\\{?\\w*(KEY|TOKEN|SECRET|PASSWORD|CREDENTIAL|API)", "exfil_wget")
  , ("cat\\s+[^\\n]*(\\.env|credentials|\\.netrc|\\.pgpass)", "read_secrets")
  , ("authorized_keys", "ssh_backdoor")
  , ("/etc/sudoers|visudo", "sudoers_mod")
  , ("rm\\s+-rf\\s+/", "destructive_root_rm")
  ]

compiled :: [(Re.Regex, Text)]
compiled = [(Re.compileOrDie [Re.IgnoreCase] p, n) | (p, n) <- threatPatterns]

invisible :: [Char]
invisible = ['\x200b', '\x200c', '\x200d', '\x2060', '\xfeff', '\x202a', '\x202b', '\x202c', '\x202d', '\x202e']

-- | @_scan_cron_prompt(prompt)@: the error, or "".
scanCronPrompt :: Text -> Text
scanCronPrompt p = case [c | c <- invisible, T.any (== c) p] of
  c : _ -> "Blocked: prompt contains invisible unicode U+" <> T.toUpper (T.justifyRight 4 '0' (T.pack (showHex (ord c) ""))) <> " (possible injection)."
  [] -> case [n | (r, n) <- compiled, isJust (Re.search r p)] of
    n : _ -> "Blocked: prompt matches threat pattern '" <> n <> "'. Cron prompts must not contain injection or exfiltration payloads."
    [] -> ""

-- | @_repeat_display(job)@
repeatDisplay :: PyVal -> Text
repeatDisplay job =
  let r = case get "repeat" job of
        Just d@(PDict _) -> d
        _ -> PDict []
      times = get "times" r
      completed = case get "completed" r of
        Just (PInt c) -> c
        _ -> 0
   in case times of
        Nothing -> "forever"
        Just PNull -> "forever"
        Just (PInt 1) -> if completed == 0 then "once" else "1/1"
        Just t -> if completed /= 0 then T.pack (show completed) <> "/" <> pyStrOf t else pyStrOf t <> " times"

canonicalSkills :: Maybe PyVal -> Maybe PyVal -> [Text]
canonicalSkills = normalizeSkills

-- | @_format_job(job)@
formatJob :: PyVal -> PyVal
formatJob job =
  let prompt = maybe "" pyStrOf (get "prompt" job)
      skills = canonicalSkills (get "skill" job) (get "skills" job)
      g k = fromMaybe PNull (get k job)
      enabled = maybe True truthy (get "enabled" job)
   in PDict
        [ ("job_id", g "id"), ("name", g "name"), ("skill", maybe PNull PStr (case skills of s : _ -> Just s; [] -> Nothing))
        , ("skills", PList (map PStr skills))
        , ("prompt_preview", PStr (if T.length prompt > 100 then T.take 100 prompt <> "..." else prompt))
        , ("model", g "model"), ("provider", g "provider"), ("base_url", g "base_url"), ("schedule", g "schedule_display")
        , ("repeat", PStr (repeatDisplay job)), ("deliver", fromMaybe (PStr "local") (get "deliver" job))
        , ("next_run_at", g "next_run_at"), ("last_run_at", g "last_run_at"), ("last_status", g "last_status")
        , ("enabled", PBool enabled), ("state", fromMaybe (PStr (if enabled then "scheduled" else "paused")) (get "state" job))
        , ("paused_at", g "paused_at"), ("paused_reason", g "paused_reason") ]

out :: [(Text, PyVal)] -> Text
out = dumpsIndent True 2 . PDict

failure :: Text -> Text
failure e = out [("success", PBool False), ("error", PStr e)]

-- | @_normalize_optional_job_value(value)@
normOpt :: Bool -> Maybe PyVal -> Maybe Text
normOpt slash = \case
  Nothing -> Nothing
  Just PNull -> Nothing
  Just v ->
    let t = T.strip (pyStrOf v)
        t' = if slash then T.dropWhileEnd (== '/') t else t
     in if T.null t' then Nothing else Just t'

-- | @cronjob(action, ...)@ with the handler's arguments (a dict), and
-- @os.getenv@ for the origin of a job made inside a gateway session.
cronjob :: CronEnv -> (Text -> IO (Maybe Text)) -> PyVal -> IO Text
cronjob env getenv args = do
  let arg k = case get k args of
        Just PNull -> Nothing
        v -> v
      argT k = pyStrOf <$> arg k
      action = fromMaybe "" (argT "action")
      normalized = T.toLower (T.strip action)
      prompt = argT "prompt"
      schedule = argT "schedule"
      jobId = argT "job_id"
  case normalized of
    "create" -> case schedule of
      Nothing -> pure (failure "schedule is required for create")
      Just "" -> pure (failure "schedule is required for create")
      Just sched -> do
        let skills = canonicalSkills (arg "skill") (arg "skills")
            p = fromMaybe "" prompt
        if T.null p && null skills
          then pure (failure "create requires either prompt or at least one skill")
          else do
            let scan = if T.null p then "" else scanCronPrompt p
            if not (T.null scan)
              then pure (failure scan)
              else do
                origin <- originFromEnv getenv
                r <-
                  createJob env
                    (createArgs p sched)
                      { caName = argT "name"
                      , caRepeat = case arg "repeat" of Just (PInt n) -> Just n; _ -> Nothing
                      , caDeliver = argT "deliver"
                      , caOrigin = origin
                      , caSkills = skills
                      , caModel = normOpt False (arg "model")
                      , caProvider = normOpt False (arg "provider")
                      , caBaseUrl = normOpt True (arg "base_url") }
                pure $ case r of
                  Left e -> failure e
                  Right job ->
                    let g k = fromMaybe PNull (get k job)
                     in out
                          [ ("success", PBool True), ("job_id", g "id"), ("name", g "name"), ("skill", g "skill")
                          , ("skills", fromMaybe (PList []) (get "skills" job)), ("schedule", g "schedule_display")
                          , ("repeat", PStr (repeatDisplay job)), ("deliver", fromMaybe (PStr "local") (get "deliver" job))
                          , ("next_run_at", g "next_run_at"), ("job", formatJob job)
                          , ("message", PStr ("Cron job '" <> maybe "" pyStrOf (get "name" job) <> "' created.")) ]
    "list" -> do
      jobs <- listJobs env (maybe False truthy (arg "include_disabled"))
      pure (out [("success", PBool True), ("count", PInt (fromIntegral (length jobs))), ("jobs", PList (map formatJob jobs))])
    _ -> case jobId of
      Nothing -> pure (failure ("job_id is required for action '" <> normalized <> "'"))
      Just "" -> pure (failure ("job_id is required for action '" <> normalized <> "'"))
      Just jid ->
        getJob env jid >>= \case
          Nothing -> pure (failure ("Job with ID '" <> jid <> "' not found. Use cronjob(action='list') to inspect jobs."))
          Just job -> do
            let name = maybe "" pyStrOf (get "name" job)
                done r = case r of
                  Left e -> failure e
                  Right updated -> out [("success", PBool True), ("job", formatJob (fromMaybe PNull updated))]
            case normalized of
              "remove" -> do
                ok <- removeJob env jid
                pure $
                  if not ok
                    then failure ("Failed to remove job '" <> jid <> "'")
                    else
                      out
                        [ ("success", PBool True), ("message", PStr ("Cron job '" <> name <> "' removed."))
                        , ("removed_job", PDict [("id", PStr jid), ("name", fromMaybe PNull (get "name" job)), ("schedule", fromMaybe PNull (get "schedule_display" job))]) ]
              "pause" -> done <$> pauseJob env jid (argT "reason")
              "resume" -> done <$> resumeJob env jid
              a | a `elem` ["run", "run_now", "trigger"] -> done <$> triggerJob env jid
              "update" -> update jid job
              _ -> pure (failure ("Unknown cron action '" <> action <> "'"))
  where
    update jid job = do
      let arg k = case get k args of
            Just PNull -> Nothing
            v -> v
          argT k = pyStrOf <$> arg k
          promptScan = maybe "" scanCronPrompt (argT "prompt")
      if not (T.null promptScan)
        then pure (failure promptScan)
        else do
          let skillsGiven = arg "skills" /= Nothing || arg "skill" /= Nothing
              skills = canonicalSkills (arg "skill") (arg "skills")
              base =
                concat
                  [ maybe [] (\p -> [("prompt", PStr p)]) (argT "prompt")
                  , maybe [] (\n -> [("name", PStr n)]) (argT "name")
                  , maybe [] (\d -> [("deliver", PStr d)]) (argT "deliver")
                  , if skillsGiven then [("skills", PList (map PStr skills)), ("skill", maybe PNull PStr (case skills of s : _ -> Just s; [] -> Nothing))] else []
                  , maybe [] (\_ -> [("model", maybe PNull PStr (normOpt False (arg "model")))]) (arg "model")
                  , maybe [] (\_ -> [("provider", maybe PNull PStr (normOpt False (arg "provider")))]) (arg "provider")
                  , maybe [] (\_ -> [("base_url", maybe PNull PStr (normOpt True (arg "base_url")))]) (arg "base_url")
                  , case arg "repeat" of
                      Just r -> [("repeat", set "times" r (case get "repeat" job of Just d@(PDict _) -> d; _ -> PDict []))]
                      Nothing -> [] ]
          schedR <- case argT "schedule" of
            Nothing -> pure (Right [])
            Just s ->
              parseSchedule env s >>= \case
                Left e -> pure (Left e)
                Right parsed ->
                  pure . Right $
                    [("schedule", parsed), ("schedule_display", fromMaybe (PStr s) (get "display" parsed))]
                      <> (if get "state" job /= Just (PStr "paused") then [("state", PStr "scheduled"), ("enabled", PBool True)] else [])
          case schedR of
            Left e -> pure (failure e)
            Right schedUpdates -> do
              let updates = base <> schedUpdates
              if null updates
                then pure (failure "No updates provided.")
                else do
                  r <- updateJob env jid updates
                  pure $ case r of
                    Left e -> failure e
                    Right updated -> out [("success", PBool True), ("job", formatJob (fromMaybe PNull updated))]

-- | @_origin_from_env()@, each @FISHER_SESSION_X@ read with
-- @GAUSS_SESSION_X@ as its fallback.
originFromEnv :: (Text -> IO (Maybe Text)) -> IO (Maybe PyVal)
originFromEnv getenv0 = do
  p <- getenv "SESSION_PLATFORM"
  c <- getenv "SESSION_CHAT_ID"
  case (p, c) of
    (Just pl, Just ch) | not (T.null pl), not (T.null ch) -> do
      n <- getenv "SESSION_CHAT_NAME"
      th <- getenv "SESSION_THREAD_ID"
      pure (Just (PDict [("platform", PStr pl), ("chat_id", PStr ch), ("chat_name", maybe PNull PStr n), ("thread_id", maybe PNull PStr th)]))
    _ -> pure Nothing
  where
    getenv suffix =
      getenv0 ("FISHER_" <> suffix) >>= \case
        Just v -> pure (Just v)
        Nothing -> getenv0 ("GAUSS_" <> suffix)
