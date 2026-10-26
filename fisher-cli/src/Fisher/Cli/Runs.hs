-- | @fisher runs list|show|export|import|import-sessions@: the run log
-- ("Fisher.Runlog", @$FISHER_HOME/fisher/runs.db@) read back, and runs made
-- before it read in ("Fisher.Cli.RunsImport").
--
-- * @list [--since D] [--project P] [--kind K] [--limit N]@: one line per
--   run, newest first (start, id, kind, backend, outcome, cost); @D@ is a
--   date (@2026-10-01@, UTC) or an age (@7d@, @12h@, @2w@).
-- * @show ID@: the run's row, its goals and its first 20 events. The
--   events are printed from the stored data, which the log redacted as it
--   wrote them.
-- * @export [--project P] FILE@: every run (of the project) as a JSON
--   array, each with its goals and events.
-- * @import [--bench] DIR@: the search run directories at or under DIR
--   (bench arms with @--bench@), laid out as live searches; a report.json
--   that cannot be read is counted as skipped.
-- * @import-sessions@: the Fisher home's state.db sessions as chat runs.
--
-- Reading never makes a run log that is not there.
module Fisher.Cli.Runs
  ( RunsCmd (..)
  , RunsEnv (..)
  , runsParser
  , runRuns
  , parseSince
  , emptyHint
  ) where

import Control.Exception (finally)
import Control.Monad (forM_)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (defaultTimeLocale, formatTime, parseTimeM)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Numeric (showFFloat)
import System.Directory (doesFileExist, makeAbsolute)

import Options.Applicative

import Fisher.Cli.RunsImport (findReports, importSearchDir, importSessions)
import Fisher.Runlog

data RunsCmd
  = RList (Maybe Text) (Maybe Text) (Maybe Text) Int
  -- ^ since, project, kind, limit
  | RShow Text
  | RExport (Maybe Text) FilePath
  -- ^ project, file
  | RImport Bool FilePath
  -- ^ @--bench@, directory
  | RImportSessions
  deriving (Eq, Show)

-- | Where the run log and the session store are, the clock, how to print.
data RunsEnv = RunsEnv
  { reRunlogPath :: FilePath
  , reStorePath :: FilePath
  , reNow :: IO Double
  , rePut :: Text -> IO ()
  }

-- | What an empty run log prints.
emptyHint :: Text
emptyHint = "no runs logged yet: fisher runs import ~/.cache/fisher/search"

runRuns :: RunsEnv -> RunsCmd -> IO ()
runRuns env = \case
  RList since project kind limit -> do
    now <- reNow env
    key <- traverse projectKey project
    case (traverse (parseSince now) since, traverse kindOf kind) of
      (Left e, _) -> put e
      (_, Left e) -> put e
      (Right since', Right kind') -> reading $ \case
        Nothing -> put emptyHint
        Just rl -> do
          rows <- listRuns rl since' key kind' limit
          case rows of
            []
              | (since, project, kind) == (Nothing, Nothing, Nothing) -> put emptyHint
              | otherwise -> put "no runs match"
            _ -> do
              put (row ["started", "id", "kind", "backend", "outcome", "cost"])
              forM_ rows $ \r ->
                put (row [date (rrStartedAt r), rrId r, renderRunKind (rrKind r), dash (rrBackend r), dash (rrOutcome r), dollars (rrCostUsd r)])
  RShow rid -> reading $ \case
    Nothing -> put ("no run " <> rid)
    Just rl ->
      getRun rl rid >>= \case
        Nothing -> put ("no run " <> rid)
        Just r -> do
          goals <- runGoals rl rid
          evs <- runEvents rl rid
          mapM_ put (showRun r goals evs)
  RExport project file -> do
    key <- traverse projectKey project
    vs <- reading $ \case
      Nothing -> pure []
      Just rl -> exportRuns rl Nothing key
    LBS.writeFile file (A.encode vs)
    put ("exported " <> count (length vs) "run" <> " to " <> T.pack file)
  RImport bench dir ->
    -- checked first, so a wrong DIR makes no run log
    findReports dir >>= \case
      Left e -> put e
      Right _ ->
        writing $ \rl ->
          importSearchDir rl (if bench then BenchImport else SearchRun) dir >>= \case
            Left e -> put e
            Right (n, skipped) ->
              put ("imported " <> count n "run" <> " from " <> T.pack dir
                     <> (if skipped > 0 then " (" <> tshow skipped <> " skipped: report.json unreadable)" else ""))
  RImportSessions ->
    writing $ \rl -> do
      n <- importSessions rl (reStorePath env)
      put ("imported " <> count n "session" <> " from " <> T.pack (reStorePath env))
  where
    put = rePut env
    -- the log when there is one (never made here)
    reading :: (Maybe Runlog -> IO a) -> IO a
    reading k = do
      there <- doesFileExist (reRunlogPath env)
      if not there then k Nothing else opened (k . Just) (k Nothing)
    writing k = opened k (pure ())
    opened :: (Runlog -> IO a) -> IO a -> IO a
    opened k failed =
      openRunlog (reRunlogPath env) >>= \case
        Left e -> put ("fisher runs: " <> e) >> failed
        Right rl -> k rl `finally` closeRunlog rl
    projectKey p = projectKeyOf <$> makeAbsolute (T.unpack p)
    kindOf k = maybe (Left ("unknown kind " <> k <> "; one of " <> T.intercalate ", " (map renderRunKind [minBound .. maxBound]))) Right (parseRunKind k)
    row cols = T.stripEnd (T.intercalate "  " (zipWith (\w c -> T.justifyLeft w ' ' c) [16, 40, 14, 20, 10, 0] cols))

-- | A run as lines: its row, its goals, and its first 20 events.
showRun :: RunRow -> [GoalRow] -> [(Int, Double, Text, A.Value)] -> [Text]
showRun r goals evs =
  [ "run " <> rrId r
  , "  kind " <> renderRunKind (rrKind r) <> "  parent " <> dash (rrParent r) <> "  capture " <> renderCapture (rrCapture r)
  , "  project " <> dash (T.pack <$> rrProject r)
  , "  backend " <> dash (rrBackend r) <> "  model " <> dash (rrModel r) <> "  tier " <> dash (rrTier r)
      <> "  workflow " <> dash (rrWorkflow r)
  , "  started " <> date (rrStartedAt r) <> "  ended " <> maybe "-" date (rrEndedAt r)
      <> "  outcome " <> dash (rrOutcome r) <> "  exit " <> dash (tshow <$> rrExit r)
  , "  cost " <> dollars (rrCostUsd r) <> "  tokens " <> dash (tshow <$> rrInputTokens r) <> " in, "
      <> dash (tshow <$> rrOutputTokens r) <> " out  turns " <> dash (tshow <$> rrTurns r) ]
    <> ["  argv " <> T.unwords (rrArgv r) | not (null (rrArgv r))]
    <> ["  raw " <> T.pack d | Just d <- [rrRawDir r]]
    <> [count (length goals) "goal" <> (if null goals then "" else ":")]
    <> [ "  " <> T.intercalate "  " [grName g, grStatus g, dash (grClosedBy g), maybe "-" (\s -> fixed 1 s <> " s") (grSecondsToClose g), dollars (grDollarsToClose g), grShape g]
       | g <- goals ]
    <> [count n "event" <> (if n > 20 then ", the first 20:" else if n > 0 then ":" else "")]
    <> [ "  " <> T.intercalate "  " [tshow s, fixed 1 t, k, clip (TE.decodeUtf8 (LBS.toStrict (A.encode d)))]
       | (s, t, k, d) <- take 20 evs ]
  where
    n = length evs
    clip x = if T.length x > 200 then T.take 200 x <> "..." else x

-- | A @--since@: a date (@YYYY-MM-DD@, UTC midnight) or an age before now
-- (@<n>h@, @<n>d@, @<n>w@), as seconds since the epoch.
parseSince :: Double -> Text -> Either Text Double
parseSince now s
  | Just (digits, unit) <- T.unsnoc s
  , not (T.null digits)
  , T.all isDigit digits
  , Just secs <- lookup unit [('h', 3600), ('d', 86400), ('w', 604800)] =
      Right (now - fromIntegral (read (T.unpack digits) :: Integer) * secs)
  | Just t <- parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack s) :: Maybe UTCTime =
      Right (realToFrac (utcTimeToPOSIXSeconds t))
  | otherwise = Left ("--since takes a date (2026-10-01) or an age (12h, 7d, 2w), not " <> s)

date :: Double -> Text
date t = T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M" (posixSecondsToUTCTime (realToFrac t)))

dash :: Maybe Text -> Text
dash = fromMaybe "-"

dollars :: Maybe Double -> Text
dollars = maybe "-" (\d -> "$" <> fixed 2 d)

fixed :: Int -> Double -> Text
fixed k x = T.pack (showFFloat (Just k) x "")

tshow :: Show a => a -> Text
tshow = T.pack . show

count :: Int -> Text -> Text
count k noun = tshow k <> " " <> noun <> (if k == 1 then "" else "s")

-- | @fisher runs@'s arguments.
runsParser :: Parser RunsCmd
runsParser = hsubparser
  ( command "list"
      (info (RList <$> optional (T.pack <$> strOption (long "since" <> metavar "D" <> help "Runs started since a date (2026-10-01) or an age (12h, 7d, 2w)"))
                   <*> optional (T.pack <$> strOption (long "project" <> metavar "P" <> help "Runs of the project at P"))
                   <*> optional (T.pack <$> strOption (long "kind" <> metavar "K" <> help "Runs of a kind (search, search-attempt, launch, chat, ui-run, cron, bench-import, import)"))
                   <*> option auto (long "limit" <> value 20 <> metavar "N" <> help "At most N runs (0: all)"))
            (progDesc "List logged runs, newest first"))
 <> command "show"
      (info (RShow . T.pack <$> strArgument (metavar "ID"))
            (progDesc "Show a run, its goals and its first 20 events"))
 <> command "export"
      (info (RExport <$> optional (T.pack <$> strOption (long "project" <> metavar "P" <> help "Only the project at P"))
                     <*> strArgument (metavar "FILE"))
            (progDesc "Write the runs, with their goals and events, as a JSON array"))
 <> command "import"
      (info (RImport <$> switch (long "bench" <> help "DIR is a bench tree (its runs are bench-import runs)")
                     <*> strArgument (metavar "DIR"))
            (progDesc "Read search run directories (report.json) at or under DIR into the run log"))
 <> command "import-sessions"
      (info (pure RImportSessions)
            (progDesc "Read the chat sessions of state.db into the run log")) )
