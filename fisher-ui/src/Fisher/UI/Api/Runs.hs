-- | @GET /api/runs@ and @GET /api/runs/:bench@: the benchmark runs under
-- the bench root, as the bench scripts leave them.
--
--   * g and h: @runs/<arm>/<n>/{report.json,events.jsonl,spend.json}@.
--     A goal is solved when its report says closed (the bench's own report
--     judges by @fisher search check@ on base.lean; its gate line is shown
--     as it prints it). Dollars follow each bench's report.py: h, the larger
--     of the /credits spans and fisher's count (the finished attempts in
--     @events*.jsonl@, or report.json's attempts when larger); g, the spans
--     when there are any, else fisher's count.
--   * w: @runs/<arm>/<n>/@ as g and h, each run a whole file: a row per
--     goal of its report (solved when closed, with the goal's retrieval:
--     facts, lemmas offered and used), and @files@, a row per run (closed
--     and total goals, dollars as bench h's, report.json's @facts_mean@,
--     @offered_total@, @used_total@). The retrieval fields are null in a
--     report without them.
--   * c3 (and clm4's c3 runs): @<out>/results.json@ records.
--   * clm4 long runs: @<root>/long-*/usage.json@ with @judge.json@.
--   * d: @judge.json@ beside a search run's report.json.
--   * rlm: @rlm/results.json@ records.
--
-- The gate line of g, h and w is the last line of
-- @python3 -I tools/bench/<b>/report.py --runs DIR@ run from the
-- repository the server started in. It checks proofs and takes minutes, so
-- it runs in the background and is cached by its inputs' sizes and times.
-- One child runs per bench: inputs that change while it runs mark it stale,
-- and it runs again once it ends. A child is killed after 45 minutes
-- ('ueGateTimeout'; bench h's takes about 16 on the Mac), and every child
-- when the server stops ('stopGates'). A report.py run by hand at the same
-- time checks in the same workspaces, and the two can fail each other's
-- checks.
--
-- The gate is not started, nor run again, while the bench itself runs (a
-- run directory with events.jsonl and no report.json): report.py would
-- check proofs in the workspaces the bench is writing, and its answer
-- would be for runs not done. A stale gate run is followed by another only
-- while the page still asks: not when no request asked for that bench's
-- gate in the last 5 minutes ('ueGateQuiet').
--
-- The JSON has @gate@ (the line, or null) and @gateStatus@: @"pending"@,
-- @"failed"@, @"bench in progress"@ or @"ok"@ (null with no gate: not g,
-- h or w, or no script).
module Fisher.UI.Api.Runs
  ( runsApi
  , RunRoot (..)
  , listRuns
  , GoalRow (..)
  , benchGoals
  , benchGate
  , benches
  , stopGates
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (SomeException, try)
import Control.Monad (filterM, forM, forM_, void, when)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Char (isDigit)
import Data.IORef
import Data.List (isPrefixOf, nub, sort, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Scientific (toBoundedInteger, toRealFloat)
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Vector as V
import Network.HTTP.Types (notFound404, ok200)
import Network.Wai (Request (..))
import System.Directory (doesDirectoryExist, doesFileExist, getFileSize, getModificationTime, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (takeFileName, (</>))
import qualified System.IO as IO
import System.Posix.Signals (sigKILL, signalProcessGroup)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createProcess, getPid, proc, waitForProcess)
import System.Timeout (timeout)

import Fisher.UI.Db (Fingerprint, GateRun (..), GateStatus (..), UiEnv (..))
import Fisher.UI.Response (Api, jsonResponse)

benches :: [Text]
benches = ["c3", "clm4", "d", "g", "h", "w", "rlm"]

data RunRoot = RunRoot { rrBench :: Text, rrArm :: Text, rrRoot :: FilePath }
  deriving (Eq, Show)

instance A.ToJSON RunRoot where
  toJSON (RunRoot b a r) = A.object ["bench" A..= b, "arm" A..= a, "root" A..= r]

data GoalRow = GoalRow
  { grGoal :: Text
  , grArm :: Text
  , grSolved :: Bool
  , grTurns :: Maybe Int
  , grDollars :: Maybe Double
  , grClosedBy :: Maybe Text
  , grRetrieval :: Maybe (Int, Int, Int)
  -- ^ bench w: the goal's facts, lemmas offered and lemmas used
  }
  deriving (Eq, Show)

instance A.ToJSON GoalRow where
  toJSON g =
    A.object $
      [ "goal" A..= grGoal g, "arm" A..= grArm g, "solved" A..= grSolved g, "turns" A..= grTurns g
      , "dollars" A..= grDollars g, "closedBy" A..= grClosedBy g ]
        <> case grRetrieval g of
          Nothing -> []
          Just (f, o, u) -> ["facts" A..= f, "offered" A..= o, "used" A..= u]

runsApi :: UiEnv -> Api
runsApi env r = case pathInfo r of
  ["api", "runs"] -> Just . jsonResponse ok200 <$> listRuns (ueBench env)
  ["api", "runs", b]
    | b `notElem` benches -> pure (Just (jsonResponse notFound404 (A.object ["error" A..= ("no such bench" :: Text)])))
    | otherwise -> Just <$> do
        roots <- rootsOf (ueBench env) b
        goals <- concat <$> mapM (benchGoals b) roots
        gate <- benchGate env b
        files <- if b == "w" then concat <$> mapM wFiles roots else pure []
        pure . jsonResponse ok200 $
          A.object $
            [ "files" A..= files | b == "w" ] <>
            [ "bench" A..= b, "arms" A..= nub (map grArm goals <> map rrArm roots), "goals" A..= goals
            , "gate" A..= (case gate of { Just (GateOk t) -> Just t; _ -> Nothing })
            , "gateStatus" A..= fmap statusText gate ]
  _ -> pure Nothing

-- | Every bench's arms, in 'benches' order.
listRuns :: FilePath -> IO [RunRoot]
listRuns bench = concat <$> mapM (rootsOf bench) benches

dirs :: FilePath -> IO [FilePath]
dirs d = do
  yes <- doesDirectoryExist d
  if not yes
    then pure []
    else do
      xs <- either (\(_ :: SomeException) -> []) sort <$> try (listDirectory d)
      filterM doesDirectoryExist [d </> x | x <- xs, not ("." `isPrefixOf` x)]

-- | A bench's arms (g, h, rlm) or run directories (c3, clm4, d).
rootsOf :: FilePath -> Text -> IO [RunRoot]
rootsOf bench b = case b of
  _ | b `elem` ["g", "h", "w"] -> map (\d -> RunRoot b (name d) d) <$> dirs (bench </> T.unpack b </> "runs")
  "c3" -> do
    ds <- dirs (bench </> "c3")
    withFile "results.json" ds
  "clm4" -> do
    ds <- dirs (bench </> "clm4")
    fmap concat . forM ds $ \d -> do
      res <- doesFileExist (d </> "results.json")
      longs <- filter (("long-" `isPrefixOf`) . takeFileName) <$> dirs d
      pure [RunRoot b (name d) d | res || not (null longs)]
  "d" -> do
    let top = bench </> "d"
    ds <- deep 3 top
    withFile "judge.json" ds
  "rlm" -> do
    let p = bench </> "rlm" </> "results.json"
    recs <- records p
    pure [RunRoot b a (bench </> "rlm") | a <- nub (mapMaybe (txt "arm") recs)]
  _ -> pure []
  where
    name = T.pack . takeFileName
    withFile f ds = do
      ok <- filterM (\d -> doesFileExist (d </> f)) ds
      pure [RunRoot b (name d) d | d <- ok]
    deep :: Int -> FilePath -> IO [FilePath]
    deep 0 _ = pure []
    deep n d = do
      xs <- filter ((`notElem` ["twisted", "lean", ".lake", "attempts", "base", "ws"]) . takeFileName) <$> dirs d
      (xs <>) . concat <$> mapM (deep (n - 1)) xs

-- ------------------------------------------------------------------ reading

load :: FilePath -> IO (Maybe A.Value)
load p = either (\(_ :: SomeException) -> Nothing) A.decodeStrict <$> try (B.readFile p)

jsonLines :: FilePath -> IO [A.Value]
jsonLines p = either (\(_ :: SomeException) -> []) (mapMaybe A.decode . BLC.lines . BLC.fromStrict) <$> try (B.readFile p)

at :: [Text] -> A.Value -> Maybe A.Value
at [] v = Just v
at (k : ks) (A.Object o) = KM.lookup (K.fromText k) o >>= at ks
at _ _ = Nothing

txt :: Text -> A.Value -> Maybe Text
txt k v = case at [k] v of
  Just (A.String s) -> Just s
  _ -> Nothing

num :: [Text] -> A.Value -> Maybe Double
num ks v = case at ks v of
  Just (A.Number n) -> Just (toRealFloat n)
  _ -> Nothing

int :: [Text] -> A.Value -> Maybe Int
int ks v = case at ks v of
  Just (A.Number n) -> toBoundedInteger n
  _ -> Nothing

list :: Text -> A.Value -> [A.Value]
list k v = case at [k] v of
  Just (A.Array xs) -> V.toList xs
  _ -> []

records :: FilePath -> IO [A.Value]
records p = maybe [] (\v -> case v of A.Array xs -> V.toList xs; _ -> list "runs" v) <$> load p

-- ------------------------------------------------------------------ goals

benchGoals :: Text -> RunRoot -> IO [GoalRow]
benchGoals b rr = case b of
  _ | b `elem` ["g", "h"] -> do
    ns <- sortOn numKey . filter (all isDigit . takeFileName) <$> dirs (rrRoot rr)
    forM ns (searchGoal b (rrArm rr))
  "w" -> do
    ns <- sortOn numKey . filter (all isDigit . takeFileName) <$> dirs (rrRoot rr)
    concat <$> forM ns (wGoals (rrArm rr))
  "c3" -> c3Goals (rrArm rr) (rrRoot rr)
  "clm4" -> do
    cs <- c3Goals (rrArm rr) (rrRoot rr)
    longs <- filter (("long-" `isPrefixOf`) . takeFileName) <$> dirs (rrRoot rr)
    ls <- forM longs $ \d -> do
      u <- fromMaybe A.Null <$> load (d </> "usage.json")
      j <- load (d </> "judge.json")
      let closed = j >>= int ["closed"]
          total = j >>= int ["total"]
      pure
        GoalRow
          { grGoal = fromMaybe (T.pack (takeFileName d)) (txt "file" u)
          , grArm = rrArm rr <> "/" <> T.pack (takeFileName d)
          , grSolved = maybe False (> 0) closed && closed == total
          , grTurns = int ["turns"] u, grDollars = num ["cost_usd"] u, grClosedBy = Nothing, grRetrieval = Nothing }
    pure (cs <> ls)
  "d" -> do
    j <- fromMaybe A.Null <$> load (rrRoot rr </> "judge.json")
    pure
      [ GoalRow g (rrArm rr) (at ["closed"] e == Just (A.Bool True)) Nothing Nothing Nothing Nothing
      | e <- list "goals" j, Just g <- [txt "goal" e] ]
  "rlm" -> do
    recs <- records (rrRoot rr </> "results.json")
    pure
      [ GoalRow (fromMaybe "" (maybe (txt "task" e) Just (txt "id" e))) (rrArm rr)
          (at ["correct"] e == Just (A.Bool True)) (int ["iterations"] e) (num ["cost"] e) Nothing Nothing
      | e <- recs, txt "arm" e == Just (rrArm rr) ]
  _ -> pure []
  where
    numKey :: FilePath -> Int
    numKey = read . takeFileName

c3Goals :: Text -> FilePath -> IO [GoalRow]
c3Goals root dir = do
  recs <- records (dir </> "results.json")
  pure
    [ GoalRow
        { grGoal = fromMaybe "" (txt "id" e)
        , grArm = root <> "/" <> T.intercalate "-" (mapMaybe (`txt` e) ["backend", "workflow", "context"])
        , grSolved = at ["solved"] e == Just (A.Bool True)
        , grTurns = int ["turns"] e, grDollars = num ["cost_usd"] e, grClosedBy = Nothing, grRetrieval = Nothing }
    | e <- recs ]

-- | One goal of a g or h arm.
searchGoal :: Text -> Text -> FilePath -> IO GoalRow
searchGoal b arm run = do
  rep <- load (run </> "report.json")
  dollars <- runDollars b run rep
  let entry = case rep of
        Just r -> case list "goals" r of
          (e : _) -> Just e
          [] -> Nothing
        Nothing -> Nothing
      treeRoot = rep >>= \r -> case list "tree" r of
        (n : _) -> Just n
        [] -> Nothing
      attempts = maybe [] (list "attempts") rep
      name =
        fromMaybe (T.pack (takeFileName run)) $
          (entry >>= txt "goal") `firstOf` (treeRoot >>= txt "name") `firstOf` (attempts `headOf` txt "goal")
      solved = case entry of
        Just e -> txt "status" e == Just "closed"
        Nothing -> maybe False (\r -> fromMaybe 0 (int ["goals_closed"] r) > 0) rep
      turns = if null attempts then Nothing else Just (sum (mapMaybe (int ["stats", "turns"]) attempts))
  pure
    GoalRow
      { grGoal = name, grArm = arm, grSolved = solved, grTurns = turns
      , grDollars = dollars, grClosedBy = entry >>= txt "closedBy", grRetrieval = Nothing }
  where
    firstOf (Just a) _ = Just a
    firstOf Nothing c = c
    headOf xs f = case mapMaybe f xs of
      (x : _) -> Just x
      [] -> Nothing

-- | A run's dollars as its bench's report.py counts them (Nothing with no
-- report, no credits and no finished attempt): h and w, the larger of the
-- /credits spans and fisher's count (the finished attempts in
-- @events*.jsonl@, or report.json's attempts when larger); g, the spans
-- when there are any, else fisher's count.
runDollars :: Text -> FilePath -> Maybe A.Value -> IO (Maybe Double)
runDollars b run rep = do
  ev <- eventDollars
  credits <- spans <$> load (run </> "spend.json")
  let attempts = maybe [] (list "attempts") rep
      flat = sum (mapMaybe (num ["stats", "cost_usd"]) attempts)
      graph = rep >>= num ["dollars", "total"]
      hLike = b `elem` ["h", "w"]
      fisher
        | hLike = max ev flat
        | otherwise = fromMaybe flat graph
      dollars
        | hLike = max (fromMaybe 0 credits) fisher
        | otherwise = fromMaybe fisher credits
  pure (if rep == Nothing && credits == Nothing && ev == 0 then Nothing else Just dollars)
  where
    eventDollars = do
      fs <- either (\(_ :: SomeException) -> []) sort <$> try (listDirectory run)
      let evs = [run </> f | f <- fs, "events" `isPrefixOf` f, ".jsonl" `T.isSuffixOf` T.pack f]
      sum <$> mapM (fmap (sum . mapMaybe finished) . jsonLines) evs
    finished e
      | txt "event" e == Just "finished" = num ["stats", "cost_usd"] e
      | otherwise = Nothing

-- | The goals of one bench w run (a whole file): its report's goals, with
-- their retrieval fields when the report has them.
wGoals :: Text -> FilePath -> IO [GoalRow]
wGoals arm run = do
  rep <- load (run </> "report.json")
  pure
    [ GoalRow
        { grGoal = g, grArm = arm, grSolved = txt "status" e == Just "closed", grTurns = Nothing, grDollars = Nothing
        , grClosedBy = txt "closedBy" e
        , grRetrieval = (,,) <$> int ["facts"] e <*> count "offered" e <*> count "used" e }
    | Just r <- [rep], e <- list "goals" r, Just g <- [txt "goal" e] ]
  where
    count k e = case at [k] e of
      Just (A.Array xs) -> Just (V.length xs)
      _ -> Nothing

-- | A bench w arm's runs, one row each: @{arm, run, file, closed, total,
-- dollars, factsMean, offered, used}@ (the last three null without
-- retrieval in the report).
wFiles :: RunRoot -> IO [A.Value]
wFiles rr = do
  ns <- sortOn (read . takeFileName :: FilePath -> Int) . filter (all isDigit . takeFileName) <$> dirs (rrRoot rr)
  forM ns $ \run -> do
    rep <- load (run </> "report.json")
    dollars <- runDollars "w" run rep
    evs <- concat <$> (mapM jsonLines . map (run </>) . filter (\f -> "events" `isPrefixOf` f && ".jsonl" `T.isSuffixOf` T.pack f)
             =<< either (\(_ :: SomeException) -> []) sort <$> try (listDirectory run))
    let goals = maybe [] (list "goals") rep
        closedEv = length (nub [g | e <- evs, txt "event" e == Just "closed", Just g <- [txt "goal" e]])
    pure $
      A.object
        [ "arm" A..= rrArm rr, "run" A..= takeFileName run
        , "file" A..= (rep >>= txt "file")
        , "closed" A..= maybe closedEv (\r -> fromMaybe 0 (int ["goals_closed"] r)) rep
        , "total" A..= (if rep == Nothing then Nothing else Just (fromMaybe (length goals) (rep >>= int ["goals_total"])))
        , "dollars" A..= dollars
        , "factsMean" A..= (rep >>= num ["facts_mean"])
        , "offered" A..= (rep >>= int ["offered_total"])
        , "used" A..= (rep >>= int ["used_total"])
        , "report" A..= (rep /= Nothing) ]

-- | The sum of spend.json's complete spans (after not below before), or
-- Nothing with none.
spans :: Maybe A.Value -> Maybe Double
spans = \case
  Nothing -> Nothing
  Just sp ->
    let ss = case at ["spans"] sp of
          Just (A.Array xs) -> V.toList xs
          Just _ -> []
          Nothing -> [sp]
        deltas = [a - bf | s <- ss, Just bf <- [num ["before"] s], Just a <- [num ["after"] s], a >= bf]
     in if null deltas then Nothing else Just (sum deltas)

-- ------------------------------------------------------------------ gate

statusText :: GateStatus -> Text
statusText = \case
  GatePending -> "pending"
  GateFailed -> "failed"
  GateBusy -> "bench in progress"
  GateOk _ -> "ok"

-- | The gate of a g or h bench, Nothing when it has none (another bench,
-- or no report.py or runs). Asking starts report.py when no child runs
-- and the inputs differ from those of the last status; while one runs, a
-- change of the inputs marks it stale instead of starting a second.
benchGate :: UiEnv -> Text -> IO (Maybe GateStatus)
benchGate env b
  | b `notElem` gated = pure Nothing
  | otherwise = do
      let script = ueRepo env </> "tools" </> "bench" </> T.unpack b </> "report.py"
          runs = ueBench env </> T.unpack b </> "runs"
      have <- (&&) <$> doesFileExist script <*> doesDirectoryExist runs
      if not have
        then pure Nothing
        else do
          fp <- fingerprint runs
          busy <- inProgress runs
          t <- nowSecs
          closing <- readIORef (ueClosing env)
          act <- atomicModifyIORef' (ueGates env) $ \m -> case M.lookup b m of
            Just g
              | gRunning g -> (M.insert b (if gFp g == fp then g {gAsked = t} else g {gStale = True, gAsked = t}) m, Left GatePending)
              | busy -> (M.insert b g {gStatus = GateBusy, gFp = fp, gAsked = t} m, Left GateBusy)
              | gFp g == fp -> (M.insert b g {gAsked = t} m, Left (gStatus g))
            _ | busy -> (M.insert b (GateRun fp GateBusy False False Nothing t) m, Left GateBusy)
              | closing -> (m, Left GatePending)
              | otherwise -> (M.insert b (GateRun fp GatePending True False Nothing t) m, Right ())
          case act of
            Left st -> pure (Just st)
            Right () -> do
              void . forkIO $ gateLoop env b script runs
              pure (Just GatePending)

-- | The benches with a gate (report.py).
gated :: [Text]
gated = ["g", "h", "w"]

nowSecs :: IO Double
nowSecs = realToFrac <$> getPOSIXTime

-- | Whether the bench is still running: a run directory with an
-- events.jsonl and no report.json.
inProgress :: FilePath -> IO Bool
inProgress runs = do
  arms <- dirs runs
  ns <- concat <$> mapM dirs arms
  or <$> mapM (\n -> (&&) <$> doesFileExist (n </> "events.jsonl") <*> (not <$> doesFileExist (n </> "report.json"))) ns

-- | Run the bench's report.py until a run ends with its inputs unchanged
-- (a stale run is followed by another while the gate is asked for and the
-- bench is not running), then keep its status.
gateLoop :: UiEnv -> Text -> FilePath -> FilePath -> IO ()
gateLoop env b script runs = go
  where
    go = do
      st <- either (\(_ :: SomeException) -> GateFailed) id <$> try (gateChild env b script runs)
      fp' <- either (\(_ :: SomeException) -> []) id <$> try (fingerprint runs)
      busy <- either (\(_ :: SomeException) -> False) id <$> try (inProgress runs)
      t <- nowSecs
      closing <- readIORef (ueClosing env)
      again <- atomicModifyIORef' (ueGates env) $ \m -> case M.lookup b m of
        Just g
          | gStale g && busy -> (M.insert b g {gFp = fp', gStatus = GateBusy, gRunning = False, gStale = False, gChild = Nothing} m, False)
          | gStale g && not closing && t - gAsked g <= ueGateQuiet env -> (M.insert b g {gFp = fp', gStale = False, gChild = Nothing} m, True)
          -- stale with no one asking: kept for the old inputs, so the
          -- next request (its inputs differ) starts a fresh run
          | otherwise -> (M.insert b g {gStatus = st, gRunning = False, gStale = False, gChild = Nothing} m, False)
        Nothing -> (m, False)
      when again go

-- | One report.py: its last non-empty stdout line when it exits 0 within
-- the timeout, else failed (killed with its process group at the timeout).
gateChild :: UiEnv -> Text -> FilePath -> FilePath -> IO GateStatus
gateChild env b script runs =
  IO.withFile "/dev/null" IO.ReadWriteMode $ \devnull -> do
    (_, mout, _, ph) <-
      createProcess
        (proc "python3" ["-I", script, "--runs", runs])
          { cwd = Just (ueRepo env), std_in = UseHandle devnull, std_out = CreatePipe, std_err = UseHandle devnull
          , create_group = True }
    out <- newEmptyMVar
    forM_ mout $ \h -> forkIO (try (B.hGetContents h) >>= putMVar out . either (\(_ :: SomeException) -> "") id)
    exited <- newEmptyMVar
    _ <- forkIO (try (waitForProcess ph) >>= putMVar exited)
    atomicModifyIORef' (ueGates env) (\m -> (M.adjust (\g -> g {gChild = Just (ph, exited)}) b m, ()))
    -- a stop that came before the child was recorded did not see it
    closing <- readIORef (ueClosing env)
    when closing (killGroup ph)
    done <- timeout (ueGateTimeout env) (readMVar exited)
    case done of
      Nothing -> killGroup ph >> readMVar exited >> pure GateFailed
      Just (Right ExitSuccess) -> do
        o <- timeout 5000000 (readMVar out)
        pure $ case filter (not . T.null) (map T.strip (T.lines (TE.decodeUtf8With TE.lenientDecode (fromMaybe "" o)))) of
          [] -> GateFailed
          ls -> GateOk (last ls)
      Just _ -> pure GateFailed

-- | SIGKILL a gate child's process group (report.py and the checks it ran).
killGroup :: ProcessHandle -> IO ()
killGroup ph = do
  mp <- getPid ph
  forM_ mp $ \pid -> void (try (signalProcessGroup sigKILL pid) :: IO (Either SomeException ()))

-- | Kill every running gate child and wait for each to exit; no child
-- starts after this. The server calls it as it stops.
stopGates :: UiEnv -> IO ()
stopGates env = do
  atomicModifyIORef' (ueClosing env) (const (True, ()))
  kids <- mapMaybe gChild . M.elems <$> readIORef (ueGates env)
  forM_ kids $ \(ph, exited) -> do
    killGroup ph
    void (timeout 5000000 (readMVar exited))

-- | The size and time of every file report.py reads.
fingerprint :: FilePath -> IO Fingerprint
fingerprint runs = do
  arms <- dirs runs
  ns <- concat <$> mapM dirs arms
  fmap concat . forM ns $ \n -> do
    fs <- either (\(_ :: SomeException) -> []) sort <$> try (listDirectory n)
    let wanted = [n </> f | f <- fs, f `elem` ["report.json", "spend.json", "base.lean"] || ("events" `isPrefixOf` f)]
    forM wanted $ \f -> (,,) f <$> getFileSize f <*> getModificationTime f
