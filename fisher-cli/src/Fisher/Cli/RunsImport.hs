-- | Runs made before the run log, read into it: search and bench run
-- directories ('importSearchDir') and the native agent's chat sessions
-- ('importSessions').
--
-- An imported search is laid out exactly as a live one writes itself
-- ("Fisher.Cli.Search"), so the learned policy's statistics
-- ("Fisher.Learn.Stats") count it: a parent run (kind @search@, or
-- @bench-import@ for a bench tree) with id @import-<16 hex digits of the
-- SHA-256 of the run directory's canonical path>@, its goal rows (name,
-- statement from base.lean, shape, status, closed by, seconds and
-- dollars to the close), its @auto closed@ and @auto missed@ events from
-- events.jsonl, and one @search-attempt@ run per attempt, id
-- @<parent>-<n>@, with its backend, model, tier, outcome class, cost and
-- times, whose first event is @attempt@ @{goal, shape, backend}@. The
-- report has no clock, so the run's start is report.json's modification
-- time less its @elapsed_s@, and each attempt's times are offsets from it.
-- The report records no tactics, so no @portfolio@ event is written. A
-- run whose parent is already logged and ended is skipped; one left half
-- written (no end) is written again.
module Fisher.Cli.RunsImport
  ( importSearchDir
  , reportDirs
  , findReports
  , importSessions
  ) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, bracket, try)
import Control.Monad (filterM, forM, forM_)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.List (nub, sort)
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Database.SQLite.Simple (SQLData (..), close, open, query, query_)
import qualified Data.Vector as V
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, doesPathExist, getModificationTime, listDirectory)
import System.FilePath ((</>))

import Fisher.Cli.Search (attemptEventData, knownStatement, shapeOfKnown, statementOf)
import Fisher.Runlog
import Fisher.Search.Lean (Decl (..), declarations)
import Fisher.Search.Outcome (outcomeClassText)
import Fisher.Sha256 (sha256Hex)

-- | Import every run directory (one holding report.json) at or under the
-- directory, as runs of the given parent kind ('SearchRun', or
-- 'BenchImport' for a bench tree): the runs written, and the directories
-- skipped because their report.json could not be read or parsed (a run
-- already logged counts as neither). 'Left' when the directory does not
-- exist or holds no report.json ('findReports').
importSearchDir :: Runlog -> RunKind -> FilePath -> IO (Either Text (Int, Int))
importSearchDir rl kind dir =
  findReports dir >>= \case
    Left e -> pure (Left e)
    Right dirs -> do
      live <- liveDirs rl
      rs <- mapM (importRun rl kind live) dirs
      pure (Right (length [() | Written <- rs], length [() | Unreadable <- rs]))

-- | The run directories at or under a directory ('reportDirs', to depth
-- 5); 'Left' when the directory does not exist or holds none. Touches no
-- run log, so a caller can check before opening one.
findReports :: FilePath -> IO (Either Text [FilePath])
findReports dir = do
  there <- doesDirectoryExist dir
  if not there
    then pure (Left ("no directory " <> T.pack dir))
    else do
      dirs <- reportDirs 5 dir
      pure (if null dirs then Left ("no report.json under " <> T.pack dir) else Right dirs)

-- The run directories of the searches the run log has from the searches
-- themselves (kind @search@, captured whole), canonical.
liveDirs :: Runlog -> IO [FilePath]
liveDirs rl = do
  rs <- listRuns rl Nothing Nothing (Just SearchRun) 0
  catMaybes <$> mapM canon [d | r <- rs, rrCapture r == Full, Just d <- [rrRawDir r]]
  where
    canon d = either (const Nothing) Just <$> (try (canonicalizePath d) :: IO (Either SomeException FilePath))

-- | The directories at or under a directory, to a depth, that hold a
-- report.json (anything of that name: one that cannot be read is
-- skipped later and counted), in name order; a run directory's own
-- subdirectories are not searched.
reportDirs :: Int -> FilePath -> IO [FilePath]
reportDirs depth dir = do
  isRun <- doesPathExist (dir </> "report.json")
  if isRun
    then pure [dir]
    else
      if depth <= 0
        then pure []
        else do
          names <- either (const []) sort <$> (try (listDirectory dir) :: IO (Either SomeException [FilePath]))
          subs <- filterM doesDirectoryExist (map (dir </>) names)
          concat <$> mapM (reportDirs (depth - 1)) subs

data Imported = Written | Already | Unreadable

-- One run directory: written; already logged (a live search logged it, or
-- it is imported already); or unreadable (its report.json cannot be read
-- or does not parse).
importRun :: Runlog -> RunKind -> [FilePath] -> FilePath -> IO Imported
importRun rl kind live dir0 = do
  dir <- canonicalizePath dir0
  let pid = "import-" <> T.take 16 (sha256Hex (TE.encodeUtf8 (T.pack dir)))
  existing <- getRun rl pid
  read' <- try (BS.readFile (dir </> "report.json")) :: IO (Either SomeException BS.ByteString)
  let decoded = either (const Nothing) A.decodeStrict read'
  case (existing, decoded) of
    _ | dir `elem` live -> pure Already
    (Just r, _) | isJust (rrEndedAt r) -> pure Already
    (_, Just rep@(Object _)) -> do
      mtime <- realToFrac . utcTimeToPOSIXSeconds <$> getModificationTime (dir </> "report.json")
      hasBase <- doesFileExist (dir </> "base.lean")
      base <- if hasBase then TE.decodeUtf8With (\_ _ -> Just '?') <$> BS.readFile (dir </> "base.lean") else pure ""
      autoEvents <- readAutoEvents (dir </> "events.jsonl")
      writeRun rl kind pid dir mtime base autoEvents rep
      pure Written
    _ -> pure Unreadable

-- The @auto closed@ and @auto missed@ lines of events.jsonl: t, kind, data.
readAutoEvents :: FilePath -> IO [(Double, Text, Value)]
readAutoEvents path = do
  there <- doesFileExist path
  if not there
    then pure []
    else do
      ls <- BC.lines <$> BS.readFile path
      pure
        [ (fromMaybe 0 (num "t" v), k, v)
        | l <- ls
        , Just v@(Object _) <- [A.decodeStrict l]
        , Just k <- [str "event" v]
        , k `elem` ["auto closed", "auto missed"] ]

writeRun :: Runlog -> RunKind -> Text -> FilePath -> Double -> Text -> [(Double, Text, Value)] -> Value -> IO ()
writeRun rl kind pid dir mtime base autoEvents rep = do
  let elapsed = fromMaybe (maximum (0 : mapMaybe (num "ended_s") attempts)) (num "elapsed_s" rep)
      start = mtime - elapsed
      project = T.unpack <$> str "project" rep
      stmts = [(dName d, dStatement d) | d <- declarations base]
      attempts = objects "attempts" rep
      goals = [g | g <- objects "goals" rep, str "goal" g /= Just "*"]
      cost a = num "dollars" a <|> (field "stats" a >>= num "cost_usd")
      -- the budget an attempt ran at: the goal's learned one (the learn
      -- block's applied attempt_secs), else the run's attempt_s; none for
      -- the --single baseline's goal * (which the statistics skip)
      budgetOf g
        | g == "*" = Nothing
        | otherwise = (field "learn" rep >>= field "applied" >>= field g >>= int "attempt_secs") <|> int "attempt_s" rep
      -- report.json's backends: {name: [attempts, closes]} (keys in name
      -- order, so in the order the attempts first used them, then the
      -- unused ones), or a list of names
      backends = case field "backends" rep of
        Just (Array xs) -> Just (T.intercalate "," [b | String b <- V.toList xs])
        Just (Object o) ->
          let names = map K.toText (KM.keys o)
              used = nub [b | a <- attempts, Just b <- [str "backend" a], b `elem` names]
           in Just (T.intercalate "," (used <> filter (`notElem` used) (sort names)))
        _ -> Nothing
  -- the attempts first, the parent last: a parent with an end is a run
  -- written whole
  forM_ (zip [1 :: Int ..] attempts) $ \(i, a) -> do
    let n = T.pack (show (fromMaybe i (int "attempt" a)))
        aid = pid <> "-" <> n
        goal = fromMaybe "" (str "goal" a)
        backend = fromMaybe "" (str "backend" a)
        stats = case field "stats" a of
          Just s@(Object _) -> Just s
          _ -> Nothing
        cls = case str "outcome_class" a of
          Just c -> c
          Nothing -> outcomeClassText (fromMaybe "" (str "outcome" a)) (int "exit" <$> stats) (isJust (str "stopped" a))
        tier = (T.pack . show <$> int "tier" a) <|> str "tier" a
    startRun rl
      NewRun
        { nrId = aid, nrKind = SearchAttempt, nrParent = Just pid, nrProject = project, nrProver = Just "lean"
        , nrBackend = str "backend" a, nrModel = str "model" a, nrTier = tier, nrWorkflow = Nothing, nrArgv = []
        , nrStartedAt = start + fromMaybe 0 (num "started_s" a), nrRawDir = Nothing, nrCapture = Imported }
    event rl aid 0 "attempt" (attemptEventData goal (knownStatement stmts goal) backend (budgetOf goal))
    endRun rl aid
      RunEnd
        { reEndedAt = start + fromMaybe 0 (num "ended_s" a <|> num "started_s" a), reExit = stats >>= int "exit"
        , reOutcome = cls, reCostUsd = cost a, reInputTokens = stats >>= int "input_tokens"
        , reOutputTokens = stats >>= int "output_tokens", reTurns = stats >>= int "turns" }
  startRun rl
    NewRun
      { nrId = pid, nrKind = kind, nrParent = Nothing, nrProject = project, nrProver = Just "lean", nrBackend = backends
      , nrModel = Nothing, nrTier = Nothing, nrWorkflow = Nothing, nrArgv = [], nrStartedAt = start
      , nrRawDir = Just dir, nrCapture = Imported }
  forM_ autoEvents $ \(t, k, d) -> event rl pid t k d
  rows <- forM goals $ \g -> do
    let name = fromMaybe "" (str "goal" g)
        stmt = statementOf stmts name
        status = fromMaybe "open" (str "status" g)
        at = if status == "closed" then num "closed_at_s" g else Nothing
        closedBy
          | status == "disproved" = Just "disproved"
          | status == "closed", str "closedBy" g == Just "auto" = Just "auto"
          | status == "closed", Just i <- int "by_attempt" g = Just ("attempt:" <> T.pack (show i))
          | otherwise = Nothing
        dollars t =
          let costs = [cost a | a <- attempts, str "goal" a == Just name, fromMaybe 0 (num "started_s" a) <= t]
           in if null costs then Just 0 else knownSum costs
        row = GoalRow name stmt (shapeOfKnown (knownStatement stmts name)) status closedBy at (at >>= dollars)
    upsertGoal rl pid row
    pure row
  let outcome
        | not (null rows) && all ((== "closed") . grStatus) rows = "closed"
        | field "budget_spent" rep == Just (Bool True) = "budget"
        | otherwise = "failed"
  endRun rl pid (RunEnd (start + elapsed) Nothing outcome (knownSum (map cost attempts)) Nothing Nothing Nothing)

knownSum :: [Maybe Double] -> Maybe Double
knownSum xs = case catMaybes xs of
  [] -> Nothing
  ds -> Just (sum ds)

-- | Every session of a state.db not yet in the run log as a @chat@ run
-- (@chat-<session id>@, as a live chat names its own), its messages as
-- events: a user, assistant or system message is @message@ @{role,
-- text}@, an assistant's tool calls each a @tool_start@ @{name, args,
-- call_id}@, a tool's answer @tool_end@ @{name, call_id, ok, result}@, at
-- its time from the session's start. No cost is known. A session the log
-- already has is skipped: its @chat-<sid>@ run, a cron run under its bare
-- id, a search attempt's @session@ @{sid}@ event, or (for a continuation
-- that compression made) a live run of the first session of its chain.
-- The number of sessions written; 0 (and no file made) when there is no
-- state.db.
importSessions :: Runlog -> FilePath -> IO Int
importSessions rl path = do
  there <- doesFileExist path
  if not there
    then pure 0
    else bracket (open path) close $ \c -> do
      sessions <-
        query_ c "SELECT id, model, started_at, ended_at, end_reason, input_tokens, output_tokens FROM sessions ORDER BY started_at, id"
          :: IO [[SQLData]]
      -- the sessions native search attempts logged under their own ids
      attemptSids <- runEventsOfKinds rl ["session"]
      let logged = [sid | (_, _, _, _, d) <- attemptSids, Just sid <- [str "sid" d]]
      counts <- forM sessions $ \case
        [SQLText sid, model, started, ended, reason, inp, outp] -> do
          let rid = "chat-" <> sid
              t0 = fromMaybe 0 (dbl started)
          root <- rootOf c sid
          -- logged already: as this chat (live or imported), as a cron
          -- run (its bare id), as a search attempt's session, or as part
          -- of the live chat it continues after a compression (the live
          -- run keeps the root's id)
          own <- or <$> mapM (fmap isJust . getRun rl) [rid, sid]
          continued <-
            if root == sid
              then pure False
              else any (\r -> rrCapture r /= Imported) . catMaybes <$> mapM (getRun rl) ["chat-" <> root, root]
          if own || continued || sid `elem` logged || root `elem` logged
            then pure 0
            else do
              msgs <-
                query c "SELECT role, content, tool_call_id, tool_calls, tool_name, timestamp FROM messages WHERE session_id = ? ORDER BY id"
                  [SQLText sid] :: IO [[SQLData]]
              startRun rl
                NewRun
                  { nrId = rid, nrKind = ChatRun, nrParent = Nothing, nrProject = Nothing, nrProver = Nothing
                  , nrBackend = Just "native", nrModel = txt model, nrTier = Nothing, nrWorkflow = Nothing, nrArgv = []
                  , nrStartedAt = t0, nrRawDir = Nothing, nrCapture = Imported }
              forM_ msgs $ \m -> forM_ (messageEvents t0 m) $ \(t, k, d) -> event rl rid t k d
              forM_ (dbl ended) $ \e ->
                endRun rl rid (RunEnd e Nothing (fromMaybe "unknown" (txt reason)) Nothing (int' inp) (int' outp) Nothing)
              pure (1 :: Int)
        _ -> pure 0
      pure (sum counts)
  where
    -- the first session of a compression chain (parent_session_id)
    rootOf c = go (1000 :: Int)
      where
        go n x = do
          rows <- query c "SELECT parent_session_id FROM sessions WHERE id = ?" [SQLText x] :: IO [[SQLData]]
          case rows of
            [SQLText p] : _ | n > 0, not (T.null p), p /= x -> go (n - 1) p
            _ -> pure x
    txt = \case SQLText t -> Just t; _ -> Nothing
    dbl = \case SQLFloat d -> Just d; SQLInteger n -> Just (fromIntegral n); _ -> Nothing
    int' = \case SQLInteger n -> Just (fromIntegral n); _ -> Nothing
    messageEvents t0 = \case
      [role, content, callId, calls, toolName, ts] ->
        let t = max 0 (fromMaybe t0 (dbl ts) - t0)
            text = fromMaybe "" (txt content)
            r = fromMaybe "" (txt role)
         in if r == "tool"
              then [(t, "tool_end", object ["name" .= txt toolName, "call_id" .= txt callId, "ok" .= True, "result" .= text])]
              else
                [(t, "message", object ["role" .= r, "text" .= text]) | not (T.null text) || null (toolCalls calls)]
                  ++ [(t, "tool_start", object ["name" .= n, "args" .= a, "call_id" .= i]) | (n, a, i) <- toolCalls calls]
      _ -> []
    -- an assistant row's tool_calls, OpenAI's shape: name, arguments, id
    toolCalls = \case
      SQLText s | Just (Array xs) <- A.decodeStrict (TE.encodeUtf8 s) ->
        [ (str "name" fn, args, str "id" call)
        | call <- V.toList xs
        , let fn = fromMaybe (object []) (field "function" call)
              args = case field "arguments" fn of
                Just (String j) -> fromMaybe (String j) (A.decodeStrict (TE.encodeUtf8 j))
                Just v -> v
                Nothing -> Null ]
      _ -> []

-- JSON fields ---------------------------------------------------------------

field :: Text -> Value -> Maybe Value
field k = \case
  Object o -> KM.lookup (K.fromText k) o
  _ -> Nothing

str :: Text -> Value -> Maybe Text
str k v = case field k v of
  Just (String t) -> Just t
  _ -> Nothing

num :: Text -> Value -> Maybe Double
num k v = case field k v of
  Just (Number n) -> Just (toRealFloat n)
  _ -> Nothing

int :: Text -> Value -> Maybe Int
int k v = round <$> num k v

objects :: Text -> Value -> [Value]
objects k v = case field k v of
  Just (Array xs) -> [o | o@(Object _) <- V.toList xs]
  _ -> []
