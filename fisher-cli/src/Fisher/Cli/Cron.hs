-- | @fisher cron ...@: upstream's @gauss cron@ (gauss_cli/cron.py over
-- tools/cronjob_tools.py and cron/), plus Fisher's own ticker.
--
-- @list [--all]@, @create|add SCHEDULE [PROMPT] [--name] [--deliver]
-- [--repeat N] [--skill S]...@, @edit JOB [--schedule] [--prompt] [--name]
-- [--deliver] [--repeat] [--skill S]... [--add-skill S]...
-- [--remove-skill S]... [--clear-skills]@, @pause|resume|run|remove JOB@,
-- @status@ and @tick@ are upstream's, with its output. Upstream's jobs
-- fire from the messaging gateway's background ticker, which Fisher does
-- not have: @fisher cron serve [--interval SECONDS]@ is Fisher's ticker
-- (in the foreground; run it under launchd or systemd to keep it), and
-- where upstream points at the gateway, Fisher points at it.
--
-- A due job runs on the native agent (platform @cron@, every toolset but
-- @cronjob@), its session saved in state.db, its output written to
-- @$FISHER_HOME/cron/output/JOB/TIME.md@. Delivery to a chat platform needs
-- upstream's gateway, which Fisher does not port: such a job's result
-- stays in its output file, and a warning says so.
module Fisher.Cli.Cron
  ( CronDeps (..)
  , defaultCronDeps
  , cronMain
  , tick
  , runJob
  , realCronEnv
  , skillView
  ) where

import Control.Concurrent (myThreadId, threadDelay, throwTo)
import Control.Exception (AsyncException (UserInterrupt), SomeException, displayException, finally, fromException, throwIO, try)
import Control.Monad (forM_, forever, unless, when)
import qualified Data.ByteString as BS
import Data.Char (isDigit)
import Data.IORef
import Data.List (isPrefixOf, sort)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Time.LocalTime (getCurrentTimeZone, timeZoneMinutes)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, removeFile)
import System.FileLock (SharedExclusive (..), tryLockFile, unlockFile)
import System.FilePath (takeExtension, takeFileName, (</>))
import System.IO (stderr)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (Handler (Catch), installHandler, nullSignal, sigTERM, signalProcess)

import Fisher.Agent.Loop
import Fisher.Agent.Replay (closeReplayLog, logOutcome, logTurn)
import Fisher.Agent.StoreDB (storeSessionDB)
import Fisher.Cli.AgentChat (ChatDeps (..), defaultChatDeps)
import Fisher.Cli.Native
import Fisher.Cli.Status (sysStyle)
import Fisher.Cron.Dt
import Fisher.Cron.Jobs
import Fisher.Cron.Tool (cronjob)
import Fisher.Cron.Zone
import Fisher.LLM.Auth.Env (loadDotenvIntoSys)
import Fisher.LLM.Auth.Error (formatRuntimeProviderError)
import Fisher.LLM.Auth.PyYaml (parsePyYaml)
import Fisher.LLM.Auth.Resolve (RuntimeRequest (..), resolveRuntimeProvider)
import Fisher.LLM.Sys
import Fisher.LLM.Types
import Fisher.Py.Value
import Fisher.Store (closeStore, openStore)
import Fisher.Term

-- | What @fisher cron@ takes from outside, so the specs can script it.
data CronDeps = CronDeps
  { crChat :: ChatDeps
  , crEnv :: Sys -> IO CronEnv
  , crTicker :: Sys -> IO (Maybe Int)
  -- ^ the PID of a running @fisher cron serve@
  , crErr :: Text -> IO ()
  -- ^ stderr (argparse errors, warnings)
  , crReplay :: Bool
  }

defaultCronDeps :: CronDeps
defaultCronDeps = CronDeps defaultChatDeps realCronEnv tickerPid (TIO.hPutStrLn stderr) True

-- | The zone @gauss_time.now()@ uses: FISHER_TIMEZONE (else GAUSS_TIMEZONE), else config.yaml's
-- @timezone@, else the system's local time (a fixed offset, as
-- @datetime.now().astimezone()@ gives).
realCronEnv :: Sys -> IO CronEnv
realCronEnv sys = do
  fromEnv <- maybe "" T.strip <$> sysGetEnvCompat sys "FISHER_TIMEZONE"
  name <-
    if not (T.null fromEnv)
      then pure fromEnv
      else do
        cfg <- rawConfig sys
        pure $ case get "timezone" cfg of
          Just (PStr t) | not (T.null (T.strip t)) -> T.strip t
          _ -> ""
  tzdir <- fromMaybe "/usr/share/zoneinfo" . fmap T.unpack <$> sysGetEnv sys "TZDIR"
  zone <- if T.null name then pure Nothing else loadZone tzdir name
  let localOffset = (* 60) . timeZoneMinutes <$> getCurrentTimeZone
  pure
    CronEnv
      { ceHome = sysFisherHome sys
      , ceNow = do
          t <- sysNow sys
          tz <- case zone of
            Just z -> pure (TzZone z Nothing)
            Nothing -> TzFixed <$> localOffset
          pure (dtFromInstant tz (round (t * 1000000)))
      , ceLocalOffset = localOffset
      , ceNewHex = sysUuidHex sys }

-- | config.yaml as written (yaml.safe_load), or an empty dict.
rawConfig :: Sys -> IO PyVal
rawConfig sys = do
  let p = sysFisherHome sys </> "config.yaml"
  ex <- doesFileExist p
  if not ex
    then pure (PDict [])
    else do
      r <- try @SomeException (BS.readFile p >>= parsePyYaml)
      pure $ case r of
        Right (Right v@(PDict _)) -> v
        _ -> PDict []

tickerFile :: Sys -> FilePath
tickerFile sys = sysFisherHome sys </> "cron" </> "ticker.pid"

-- | The PID in cron/ticker.pid, when that process is alive.
tickerPid :: Sys -> IO (Maybe Int)
tickerPid sys = do
  ex <- doesFileExist (tickerFile sys)
  if not ex
    then pure Nothing
    else do
      t <- T.strip . TE.decodeUtf8 <$> BS.readFile (tickerFile sys)
      if T.null t || not (T.all isDigit t)
        then pure Nothing
        else do
          let pid = read (T.unpack t) :: Int
          alive <- try @SomeException (signalProcess nullSignal (fromIntegral pid))
          pure (either (const Nothing) (const (Just pid)) alive)

-- ------------------------------------------------------------------ args

data Opt = Flag Text | Value Text | IntValue Text | Append Text

optName :: Opt -> Text
optName = \case
  Flag n -> n
  Value n -> n
  IntValue n -> n
  Append n -> n

data Sub = Sub
  { subProg :: Text
  , subUsage :: Text
  , subPositionals :: [(Text, Bool)]
  , subOpts :: [Opt]
  }

subcommands :: [(Text, Sub)]
subcommands =
  [ ("list", Sub "fisher cron list" "usage: fisher cron list [-h] [--all]\n" [] [Flag "--all"])
  , ("create", createSub), ("add", createSub)
  , ( "edit"
    , Sub "fisher cron edit"
        "usage: fisher cron edit [-h] [--schedule SCHEDULE] [--prompt PROMPT]\n\
        \                        [--name NAME] [--deliver DELIVER] [--repeat REPEAT]\n\
        \                        [--skill SKILLS] [--add-skill ADD_SKILLS]\n\
        \                        [--remove-skill REMOVE_SKILLS] [--clear-skills]\n\
        \                        job_id\n"
        [("job_id", True)]
        [ Value "--schedule", Value "--prompt", Value "--name", Value "--deliver", IntValue "--repeat", Append "--skill"
        , Append "--add-skill", Append "--remove-skill", Flag "--clear-skills" ] )
  , ("pause", jobSub "pause"), ("resume", jobSub "resume"), ("run", jobSub "run")
  , ("remove", jobSub "remove"), ("rm", jobSub "remove"), ("delete", jobSub "remove")
  , ("status", Sub "fisher cron status" "usage: fisher cron status [-h]\n" [] [])
  , ("tick", Sub "fisher cron tick" "usage: fisher cron tick [-h]\n" [] [])
  , ("serve", Sub "fisher cron serve" "usage: fisher cron serve [-h] [--interval SECONDS]\n" [] [IntValue "--interval"])
  ]
  where
    createSub =
      Sub "fisher cron create"
        "usage: fisher cron create [-h] [--name NAME] [--deliver DELIVER]\n\
        \                          [--repeat REPEAT] [--skill SKILLS]\n\
        \                          schedule [prompt]\n"
        [("schedule", True), ("prompt", False)]
        [Value "--name", Value "--deliver", IntValue "--repeat", Append "--skill"]
    jobSub n = Sub ("fisher cron " <> n) ("usage: fisher cron " <> n <> " [-h] job_id\n") [("job_id", True)] []

-- | Parsed: options (an append option once per use), positionals by name.
type Parsed = [(Text, Text)]

parseSub :: Sub -> [Text] -> Either Text Parsed
parseSub sub = go [] []
  where
    err msg = Left (subUsage sub <> subProg sub <> ": error: " <> msg <> "\n")
    go acc pos args = case args of
      [] -> finish acc pos
      a : rest
        | a == "--" -> finish acc (pos <> rest)
        | "--" `T.isPrefixOf` a -> do
            let (name, inline) = case T.breakOn "=" a of
                  (n, "") -> (n, Nothing)
                  (n, v) -> (n, Just (T.drop 1 v))
                exact = [o | o <- subOpts sub, optName o == name]
                prefixed = [o | o <- subOpts sub, T.unpack name `isPrefixOf` T.unpack (optName o)]
            case (exact, prefixed) of
              (o : _, _) -> takeOpt o inline rest
              ([], [o]) -> takeOpt o inline rest
              ([], []) -> err ("unrecognized arguments: " <> T.unwords (a : rest))
              ([], os) -> err ("ambiguous option: " <> name <> " could match " <> T.intercalate ", " (map optName os))
        | otherwise -> go acc (pos <> [a]) rest
      where
        takeOpt o inline rest = case o of
          Flag n -> go (acc <> [(T.drop 2 n, "true")]) pos rest
          _ -> do
            (v, rest') <- case inline of
              Just v -> Right (v, rest)
              Nothing -> case rest of
                v : r | not ("-" `T.isPrefixOf` v) -> Right (v, r)
                _ -> err ("argument " <> optName o <> ": expected one argument")
            case o of
              IntValue n | not (validInt v) -> err ("argument " <> n <> ": invalid int value: '" <> v <> "'")
              _ -> go (acc <> [(T.drop 2 (optName o), v)]) pos rest'
    validInt v = let t = T.strip v in not (T.null t) && T.all isDigit (fromMaybe t (T.stripPrefix "-" t))
    finish acc pos =
      let names = subPositionals sub
          required = [n | (n, True) <- names]
          missing = drop (length pos) required
       in if not (null missing)
            then err ("the following arguments are required: " <> T.intercalate ", " missing)
            else
              if length pos > length names
                then err ("unrecognized arguments: " <> T.unwords (drop (length names) pos))
                else Right (acc <> zip (map fst names) pos)

values :: Text -> Parsed -> [Text]
values k p = [v | (k', v) <- p, k' == k]

value :: Text -> Parsed -> Maybe Text
value k p = case values k p of
  [] -> Nothing
  vs -> Just (last vs)

-- ------------------------------------------------------------------ commands

-- | @fisher cron ...@: the exit code.
cronMain :: CronDeps -> Sys -> [Text] -> IO Int
cronMain deps sys args = do
  env <- crEnv deps sys
  st <- sysStyle sys
  let say t = sysOut sys (t <> "\n")
      paint = color st
  case args of
    [] -> cronList deps sys env say paint False >> pure 0
    sub : rest -> case lookup sub subcommands of
      Nothing -> do
        crErr deps
          ( "usage: fisher cron [-h]\n\
            \                   {list,create,add,edit,pause,resume,run,remove,rm,delete,status,tick}\n\
            \                   ...\n\
            \fisher cron: error: argument cron_command: invalid choice: '" <> sub <> "' (choose from 'list', 'create', 'add', 'edit', 'pause', 'resume', 'run', 'remove', 'rm', 'delete', 'status', 'tick')" )
        pure 2
      Just spec -> case parseSub spec rest of
        Left e -> crErr deps (T.dropWhileEnd (== '\n') e) >> pure 2
        Right p -> case sub of
          "list" -> cronList deps sys env say paint (isJust (value "all" p)) >> pure 0
          "status" -> cronStatus deps sys env say paint >> pure 0
          "tick" -> tick deps sys >> pure 0
          "serve" -> serve deps sys (maybe 60 (read . T.unpack) (value "interval" p))
          s | s `elem` ["create", "add"] -> cronCreate env sys say paint p >> pure 0
          "edit" -> cronEdit env sys say paint p >> pure 0
          "pause" -> jobAction env sys say paint "pause" p "Paused" >> pure 0
          "resume" -> jobAction env sys say paint "resume" p "Resumed" >> pure 0
          "run" -> jobAction env sys say paint "run" p "Triggered" >> pure 0
          _ -> jobAction env sys say paint "remove" p "Removed" >> pure 0

blue :: Colour
blue = "\ESC[34m"

pyStr :: PyVal -> Text
pyStr = pyStrOf

-- | Fisher's lines where upstream points at the gateway: the ticker is
-- @fisher cron serve@.
tickerHint :: CronDeps -> Sys -> (Text -> IO ()) -> ([Colour] -> Text -> Text) -> IO ()
tickerHint deps sys say paint = do
  pid <- crTicker deps sys
  unless (isJust pid) $ do
    say (paint [yellow] "  ⚠  The cron ticker is not running — jobs won't fire automatically.")
    say (paint [dim] "     Start it with: fisher cron serve")
    say ""

cronList :: CronDeps -> Sys -> CronEnv -> (Text -> IO ()) -> ([Colour] -> Text -> Text) -> Bool -> IO ()
cronList deps sys env say paint showAll = do
  jobs <- listJobs env showAll
  if null jobs
    then do
      say (paint [dim] "No scheduled jobs.")
      say (paint [dim] "Create one with 'fisher cron create ...' or the /cron command in chat.")
    else do
      say ""
      say (paint [cyan] "┌─────────────────────────────────────────────────────────────────────────┐")
      say (paint [cyan] "│                         Scheduled Jobs                                  │")
      say (paint [cyan] "└─────────────────────────────────────────────────────────────────────────┘")
      say ""
      forM_ jobs $ \job -> do
        let g k d = fromMaybe d (get k job)
            jid = T.take 8 (pyStr (g "id" (PStr "?")))
            name = pyStr (g "name" (PStr "(unnamed)"))
            schedule = pyStr (g "schedule_display" (fromMaybe (PStr "?") (get "schedule" job >>= get "value")))
            enabled = maybe True truthy (get "enabled" job)
            state = pyStr (g "state" (PStr (if enabled then "scheduled" else "paused")))
            nextRun = pyStr (g "next_run_at" (PStr "?"))
            rep = case get "repeat" job of
              Just r@(PDict _) -> r
              _ -> PDict []
            times = get "times" rep
            completed = maybe "0" pyStr (get "completed" rep)
            repeatStr = case times of
              Just t | truthy t -> completed <> "/" <> pyStr t
              _ -> "∞"
            deliverStr = case g "deliver" (PList [PStr "local"]) of
              PList ds -> T.intercalate ", " (map pyStr ds)
              d -> pyStr d
            skills = case get "skills" job of
              Just (PList ss) | not (null ss) -> map pyStr ss
              _ -> case get "skill" job of
                Just s | truthy s -> [pyStr s]
                _ -> []
            status
              | state == "paused" = paint [yellow] "[paused]"
              | state == "completed" = paint [blue] "[completed]"
              | enabled = paint [green] "[active]"
              | otherwise = paint [red] "[disabled]"
        say ("  " <> paint [yellow] jid <> " " <> status)
        say ("    Name:      " <> name)
        say ("    Schedule:  " <> schedule)
        say ("    Repeat:    " <> repeatStr)
        say ("    Next run:  " <> nextRun)
        say ("    Deliver:   " <> deliverStr)
        unless (null skills) $ say ("    Skills:    " <> T.intercalate ", " skills)
        say ""
      tickerHint deps sys say paint

cronStatus :: CronDeps -> Sys -> CronEnv -> (Text -> IO ()) -> ([Colour] -> Text -> Text) -> IO ()
cronStatus deps sys env say paint = do
  say ""
  crTicker deps sys >>= \case
    Just pid -> do
      say (paint [green] "✓ The cron ticker is running — cron jobs will fire automatically")
      say ("  PID: " <> T.pack (show pid))
    Nothing -> do
      say (paint [red] "✗ The cron ticker is not running — cron jobs will NOT fire")
      say ""
      say "  To enable automatic execution:"
      say "    fisher cron serve        # Run the ticker (keep it running, e.g. under launchd or systemd)"
  say ""
  jobs <- listJobs env False
  if null jobs
    then say "  No active jobs"
    else do
      let nexts = [n | j <- jobs, Just (PStr n) <- [get "next_run_at" j], not (T.null n)]
      say ("  " <> T.pack (show (length jobs)) <> " active job(s)")
      unless (null nexts) $ say ("  Next run: " <> minimum nexts)
  say ""

api :: CronEnv -> Sys -> [(Text, PyVal)] -> IO PyVal
api env sys kvs = do
  out <- cronjob env (sysGetEnv sys) (PDict kvs)
  pure (either (const (PDict [])) id (loads (TE.encodeUtf8 out)))

optT :: Maybe Text -> PyVal
optT = maybe PNull PStr

-- | @_normalize_skills(single, skills)@: Nothing when no --skill was given.
normSkills :: [Text] -> Maybe [Text]
normSkills [] = Nothing
normSkills xs = Just (foldl (\acc t -> let s = T.strip t in if T.null s || s `elem` acc then acc else acc <> [s]) [] xs)

cronCreate :: CronEnv -> Sys -> (Text -> IO ()) -> ([Colour] -> Text -> Text) -> Parsed -> IO ()
cronCreate env sys say paint p = do
  r <-
    api env sys
      [ ("action", PStr "create"), ("schedule", optT (value "schedule" p)), ("prompt", optT (value "prompt" p))
      , ("name", optT (value "name" p)), ("deliver", optT (value "deliver" p))
      , ("repeat", maybe PNull (PInt . read . T.unpack . T.strip) (value "repeat" p))
      , ("skill", PNull), ("skills", maybe PNull (PList . map PStr) (normSkills (values "skill" p))) ]
  if not (maybe False truthy (get "success" r))
    then say (paint [red] ("Failed to create job: " <> maybe "unknown error" pyStr (get "error" r)))
    else do
      say (paint [green] ("Created job: " <> f "job_id" r))
      say ("  Name: " <> f "name" r)
      say ("  Schedule: " <> f "schedule" r)
      case get "skills" r of
        Just (PList ss@(_ : _)) -> say ("  Skills: " <> T.intercalate ", " (map pyStr ss))
        _ -> pure ()
      say ("  Next run: " <> f "next_run_at" r)
  where
    f k v = maybe "None" pyStr (get k v)

cronEdit :: CronEnv -> Sys -> (Text -> IO ()) -> ([Colour] -> Text -> Text) -> Parsed -> IO ()
cronEdit env sys say paint p = do
  let jid = fromMaybe "" (value "job_id" p)
  getJob env jid >>= \case
    Nothing -> say (paint [red] ("Job not found: " <> jid))
    Just job -> do
      let existing = case get "skills" job of
            Just (PList ss) | not (null ss) -> map pyStr ss
            _ -> case get "skill" job of
              Just s | truthy s -> [pyStr s]
              _ -> []
          replacement = normSkills (values "skill" p)
          adds = fromMaybe [] (normSkills (values "add-skill" p))
          removes = fromMaybe [] (normSkills (values "remove-skill" p))
          final
            | isJust (value "clear-skills" p) = Just []
            | Just r <- replacement = Just r
            | not (null adds) || not (null removes) =
                let kept = [s | s <- existing, s `notElem` removes]
                 in Just (kept <> [a | a <- adds, a `notElem` kept])
            | otherwise = Nothing
      r <-
        api env sys
          [ ("action", PStr "update"), ("job_id", PStr jid), ("schedule", optT (value "schedule" p))
          , ("prompt", optT (value "prompt" p)), ("name", optT (value "name" p)), ("deliver", optT (value "deliver" p))
          , ("repeat", maybe PNull (PInt . read . T.unpack . T.strip) (value "repeat" p))
          , ("skills", maybe PNull (PList . map PStr) final) ]
      if not (maybe False truthy (get "success" r))
        then say (paint [red] ("Failed to update job: " <> maybe "unknown error" pyStr (get "error" r)))
        else do
          let u = fromMaybe (PDict []) (get "job" r)
              f k = maybe "None" pyStr (get k u)
          say (paint [green] ("Updated job: " <> f "job_id"))
          say ("  Name: " <> f "name")
          say ("  Schedule: " <> f "schedule")
          case get "skills" u of
            Just (PList ss@(_ : _)) -> say ("  Skills: " <> T.intercalate ", " (map pyStr ss))
            _ -> say "  Skills: none"

jobAction :: CronEnv -> Sys -> (Text -> IO ()) -> ([Colour] -> Text -> Text) -> Text -> Parsed -> Text -> IO ()
jobAction env sys say paint action p verb = do
  let jid = fromMaybe "" (value "job_id" p)
  r <- api env sys [("action", PStr action), ("job_id", PStr jid)]
  if not (maybe False truthy (get "success" r))
    then say (paint [red] ("Failed to " <> action <> " job: " <> maybe "unknown error" pyStr (get "error" r)))
    else do
      let job = case get "job" r of
            Just j | truthy j -> j
            _ -> case get "removed_job" r of
              Just j | truthy j -> j
              _ -> PDict []
      say (paint [green] (verb <> " job: " <> maybe jid pyStr (get "name" job) <> " (" <> jid <> ")"))
      when (action `elem` ["resume", "run"]) $
        case get "job" r >>= get "next_run_at" of
          Just n | truthy n -> say ("  Next run: " <> pyStr n)
          _ -> pure ()
      when (action == "run") $ say "  It will run on the next scheduler tick."

-- ------------------------------------------------------------------ the ticker

-- | @fisher cron serve@: tick now and every interval, until stopped.
serve :: CronDeps -> Sys -> Int -> IO Int
serve deps sys interval = do
  createDirectoryIfMissing True (sysFisherHome sys </> "cron")
  -- SIGTERM (launchd, systemd, kill) ends the loop as Ctrl-C does, so the
  -- PID file goes
  me <- myThreadId
  _ <- installHandler sigTERM (Catch (throwTo me UserInterrupt)) Nothing
  pid <- getProcessID
  BS.writeFile (tickerFile sys) (TE.encodeUtf8 (T.pack (show pid)))
  sysOut sys ("fisher cron ticker running (PID " <> T.pack (show pid) <> ", every " <> T.pack (show interval) <> " s); Ctrl-C to stop\n")
  (forever $ do
      r <- try @SomeException (tick deps sys)
      n <- case r of
        Right k -> pure k
        Left e -> case fromException e of
          Just (a :: AsyncException) -> throwIO a
          Nothing -> crErr deps ("tick failed: " <> T.pack (displayException e)) >> pure 0
      when (n > 0) $ do
        env <- crEnv deps sys
        now <- ceNow env
        sysOut sys (strftimeHms now <> " - ran " <> T.pack (show n) <> " job(s)\n")
      threadDelay (max 1 interval * 1000000))
    `finally` (try @SomeException (removeFile (tickerFile sys)) >> pure ())

-- | @cron.scheduler.tick()@: the jobs run (0 when another tick holds the
-- lock).
tick :: CronDeps -> Sys -> IO Int
tick deps sys = do
  env <- crEnv deps sys
  let dir = cronDir env
  createDirectoryIfMissing True dir
  lock <- tryLockFile (dir </> ".tick.lock") Exclusive
  case lock of
    Nothing -> pure 0
    Just l -> (`finally` unlockFile l) $ do
      due <- getDueJobs env
      -- the tools and the model list are made once, as upstream's modules are
      ref <- newIORef Nothing
      let shared = readIORef ref >>= \case
            Just s -> pure s
            Nothing -> do
              s <- newNativeShared sys (crChat deps) (crErr deps)
              writeIORef ref (Just s)
              pure s
      fmap sum . mapM (runDue env shared) $ due
  where
    runDue env shared job = do
      let jid = maybe "" pyStrOf (get "id" job)
      r <- try @SomeException $ do
        (ok, output, final, err) <- runJob deps sys env shared job
        _ <- saveJobOutput env jid output
        let name = maybe jid pyStrOf (get "name" job)
            content = if ok then final else "⚠️ Cron job '" <> name <> "' failed:\n" <> fromMaybe "" err
        unless (T.null content) (deliver job)
        markJobRun env jid ok err
      case r of
        Right () -> pure (1 :: Int)
        Left e -> do
          crErr deps ("Error processing job " <> jid <> ": " <> errText e)
          markJobRun env jid False (Just (errText e))
          pure 0
    errText e = case fromException e of
      Just (NativeFailure _ t) -> t
      Nothing -> T.pack (displayException e)
    deliver job = do
      let jid = maybe "" pyStrOf (get "id" job)
          target = maybe "local" pyStrOf (get "deliver" job)
      unless (target == "local") $
        crErr deps ("Job '" <> jid <> "': deliver=" <> target <> ", but Fisher has no messaging gateway; the result is in " <> T.pack (sysFisherHome sys </> "cron" </> "output" </> T.unpack jid))

-- | @_build_job_prompt(job)@: Left is the error a missing skill raises.
buildJobPrompt :: Sys -> PyVal -> IO (Either Text Text)
buildJobPrompt sys job = do
  let prompt = maybe "" pyStrOf (get "prompt" job)
      skills = case get "skills" job of
        Just (PList ss) -> [T.strip (pyStrOf s) | s <- ss, not (T.null (T.strip (pyStrOf s)))]
        Just PNull -> legacy
        Nothing -> legacy
        _ -> []
      legacy = case get "skill" job of
        Just s | truthy s -> [T.strip (pyStrOf s)]
        _ -> []
  if null skills
    then pure (Right prompt)
    else go skills [] prompt
  where
    go [] parts prompt =
      pure . Right . T.intercalate "\n" $
        parts <> (if T.null prompt then [] else ["", "The user has provided the following instruction alongside the skill invocation: " <> prompt])
    go (s : rest) parts prompt =
      skillView sys s >>= \case
        Left e -> pure (Left e)
        Right content ->
          go rest
            ( parts <> ["" | not (null parts)]
                <> ["[SYSTEM: The user has invoked the \"" <> s <> "\" skill, indicating they want you to follow its instructions. The full skill content is loaded below.]", "", T.strip content] )
            prompt

-- | @skill_view(name)@'s content (or its error), as cron uses it.
skillView :: Sys -> Text -> IO (Either Text Text)
skillView sys name = do
  let root = sysFisherHome sys </> "skills"
  ex <- doesDirectoryExist root
  if not ex
    then pure (Left "Skills directory does not exist yet. It will be created on first install.")
    else do
      let direct = root </> T.unpack name
      directDir <- (&&) <$> doesDirectoryExist direct <*> doesFileExist (direct </> "SKILL.md")
      directMd <- doesFileExist (direct <> ".md")
      found <-
        if directDir
          then pure (Just (direct </> "SKILL.md"))
          else
            if directMd
              then pure (Just (direct <> ".md"))
              else do
                files <- walk root
                case [f | f <- files, takeFileName f == "SKILL.md", takeFileName (takeDirOf f) == T.unpack name] of
                  f : _ -> pure (Just f)
                  [] -> pure (case [f | f <- files, takeFileName f == T.unpack name <> ".md", takeFileName f /= "SKILL.md"] of f : _ -> Just f; [] -> Nothing)
      case found of
        Nothing -> pure (Left ("Skill '" <> name <> "' not found."))
        Just f -> do
          r <- try @SomeException (BS.readFile f)
          case r of
            Left e -> pure (Left ("Failed to read skill '" <> name <> "': " <> T.pack (displayException e)))
            Right b -> do
              let content = TE.decodeUtf8 b
              ok <- platformOk content
              pure (if ok then Right content else Left ("Skill '" <> name <> "' is not supported on this platform."))
  where
    takeDirOf = reverse . drop 1 . dropWhile (/= '/') . reverse
    walk d = do
      es <- sort <$> listDirectory d
      fmap concat . mapM (\e -> do
        let p = d </> e
        isDir <- doesDirectoryExist p
        if isDir then walk p else pure [p | takeExtension p == ".md"]) $ es
    platformOk content = case T.stripPrefix "---\n" content of
      Nothing -> pure True
      Just rest -> do
        let (front, _) = T.breakOn "\n---" rest
        r <- parsePyYaml (TE.encodeUtf8 front)
        pure $ case r of
          Right fm | Just ps <- get "platforms" fm, truthy ps ->
            let items = case ps of
                  PList xs -> map (T.toLower . T.strip . pyStrOf) xs
                  v -> [T.toLower (T.strip (pyStrOf v))]
                mapped p = fromMaybe p (lookup p [("macos", "darwin"), ("linux", "linux"), ("windows", "win32")])
             in any (\p -> mapped p `T.isPrefixOf` sysPlatform sys) items
          _ -> True

-- | @run_job(job)@: success, the output document, the final response and
-- the error. Raises (as upstream's prompt building does) for a missing
-- skill.
runJob :: CronDeps -> Sys -> CronEnv -> IO NativeShared -> PyVal -> IO (Bool, Text, Text, Maybe Text)
runJob deps sys env shared job = do
  let jid = maybe "" pyStrOf (get "id" job)
      name = maybe "" pyStrOf (get "name" job)
      display = maybe "N/A" pyStrOf (get "schedule_display" job)
  prompt <- buildJobPrompt sys job >>= either (throwIO . NativeFailure "RuntimeError") pure
  r <- try @SomeException $ do
    loadDotenvIntoSys sys
    cfg <- rawConfig sys
    envModel <- sysGetEnvCompat sys "FISHER_MODEL"
    let jobModel = case get "model" job of
          Just (PStr m) | not (T.null m) -> Just m
          _ -> Nothing
        model0 = fromMaybe (fromMaybe "anthropic/claude-opus-4.6" (nonEmpty envModel)) jobModel
        model = case (jobModel, get "model" cfg) of
          (Nothing, Just (PStr m)) -> m
          (Nothing, Just d@(PDict _)) -> maybe model0 pyStrOf (get "default" d)
          _ -> model0
        agentCfg = fromMaybe (PDict []) (get "agent" cfg)
    effortEnv <- fromMaybe "" <$> sysGetEnvCompat sys "FISHER_REASONING_EFFORT"
    let effort = if T.null effortEnv then T.strip (maybe "" pyStrOf (get "reasoning_effort" agentCfg)) else effortEnv
        reasoning
          | not (T.null effort) && T.toLower effort /= "none" =
              if T.toLower effort `elem` ["xhigh", "high", "medium", "low", "minimal"] then Just (PDict [("enabled", PBool True), ("effort", PStr (T.toLower effort))]) else Nothing
          | T.toLower effort == "none" = Just (PDict [("enabled", PBool False)])
          | otherwise = Nothing
    prefillEnv <- fromMaybe "" <$> sysGetEnvCompat sys "FISHER_PREFILL_MESSAGES_FILE"
    let prefillFile = if T.null prefillEnv then maybe "" pyStrOf (get "prefill_messages_file" cfg) else prefillEnv
    prefill <-
      if T.null prefillFile
        then pure []
        else do
          let p0 = T.unpack prefillFile
              p = if "/" `isPrefixOf` p0 then p0 else if "~/" `isPrefixOf` p0 then sysHome sys </> drop 2 p0 else sysFisherHome sys </> p0
          ex <- doesFileExist p
          if not ex then pure [] else do
            b <- BS.readFile p
            pure (case loads b of Right (PList xs) -> xs; _ -> [])
    let maxIter = case (get "max_turns" agentCfg, get "max_turns" cfg) of
          (Just (PInt n), _) | n > 0 -> fromIntegral n
          (_, Just (PInt n)) | n > 0 -> fromIntegral n
          _ -> 90
        pr = fromMaybe (PDict []) (get "provider_routing" cfg)
        strs k = case get k pr of
          Just (PList xs) -> [pyStrOf x | x <- xs]
          _ -> []
        routing = Routing (strs "only") (strs "ignore") (strs "order") (pyStrOf <$> get "sort" pr) False Nothing
    reqProvider <- case get "provider" job of
      Just (PStr p) | not (T.null p) -> pure (Just p)
      _ -> nonEmpty <$> sysGetEnvCompat sys "FISHER_INFERENCE_PROVIDER"
    let baseUrl = case get "base_url" job of
          Just (PStr b) | not (T.null b) -> Just b
          _ -> Nothing
    rt <-
      try @SomeException (resolveRuntimeProvider sys (RuntimeRequest reqProvider Nothing baseUrl)) >>= \case
        Left e -> ioErrorNative "RuntimeError" (formatRuntimeProviderError e)
        Right v -> pure v
    now <- ceNow env
    store <- either (const Nothing) Just <$> try @SomeException (openStore (sysFisherHome sys </> "state.db"))
    let field' k = maybe "" pyStrOf (get k rt)
        sid = "cron_" <> jid <> "_" <> strftimeSessionId now
        spec =
          (nativeSpec "cron" sid)
            { nsModel = model, nsProvider = if T.null (field' "provider") then "openrouter" else field' "provider"
            , nsApiMode = fromMaybe ChatCompletions (parseApiMode (field' "api_mode")), nsBaseUrl = field' "base_url"
            , nsApiKey = field' "api_key", nsBaseUrlGiven = True, nsDisabled = Just ["cronjob"], nsMaxIterations = maxIter
            , nsReasoning = reasoning, nsPrefill = prefill, nsRouting = routing
            , nsDb = fmap (\s -> storeSessionDB s (sysNow sys)) store, nsReplay = crReplay deps, nsOut = sysOut sys }
    (do
        sh <- shared
        (ag, lg) <- buildNative sys (crChat deps) sh spec
        let ri = runInput prompt
        mapM_ (`logTurn` ri) lg
        res <- runConversation ag ri
        mapM_ (`logOutcome` res) lg
        mapM_ closeReplayLog lg
        pure res)
      `finally` mapM_ closeStore store
  now <- ceNow env
  let header = "**Job ID:** " <> jid <> "\n**Run Time:** " <> strftimeYmdHms now <> "\n**Schedule:** " <> display <> "\n\n## Prompt\n\n" <> prompt <> "\n\n"
  pure $ case r of
    Right (RunDone d) ->
      let final = case get "final_response" d of
            Just (PStr s) | not (T.null s) -> s
            _ -> "(No response generated)"
       in (True, "# Cron Job: " <> name <> "\n\n" <> header <> "## Response\n\n" <> final <> "\n", final, Nothing)
    Right (RunRaised cls m) -> failed name header (cls <> ": " <> m)
    Left e -> failed name header (case fromException e of
      Just (NativeFailure cls t) -> cls <> ": " <> t
      Nothing -> "Exception: " <> T.pack (displayException e))
  where
    nonEmpty = \case
      Just t | not (T.null (T.strip t)) -> Just t
      _ -> Nothing
    failed name header msg =
      (False, "# Cron Job: " <> name <> " (FAILED)\n\n" <> header <> "## Error\n\n```\n" <> msg <> "\n\n(no traceback: Fisher is not Python)\n```\n", "", Just msg)
    ioErrorNative :: Text -> Text -> IO a
    ioErrorNative cls t = throwIO (NativeFailure cls t)
