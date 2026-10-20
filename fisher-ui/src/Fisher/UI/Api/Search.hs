-- | The goal board (U4): the runs of @fisher search@ (flat and graph),
-- their events as they are written, start, stop, apply and retry.
--
-- A run is a directory as @fisher search@ leaves it (events.jsonl,
-- report.json once it ends, base.lean, graph.json and
-- @whiteboard-*.md@ for a graph run, @attempts/<n>-<goal>-<backend>/@).
-- Its id names where it is:
--
--   * @s.<name>@: @~/.cache/fisher/search/<name>@ (where @fisher search@
--     puts a run by default, and where the runs the page starts go);
--   * @g.<arm>.<n>@, @h.<arm>.<n>@, @w.<arm>.<n>@: bench g's, h's and
--     w's @runs/<arm>/<n>@ under the bench root.
--
-- Routes:
--
--   * @GET /api/search@: @[{run, dir, bench, file, project, mode, started,
--     closed, total, status, running}]@, the latest first. @mode@ is
--     @graph@ or @flat@; @closed@ and @total@ are report.json's goal counts,
--     or counted from the events while it has none; @started@ is POSIX
--     seconds (report.json's time less its elapsed seconds, else the events
--     file's time less its last event's @t@, else the directory's time).
--   * @GET /api/search/:run@: the same with @report@ (report.json or
--     null), @graph@ (graph.json or null), @whiteboards@
--     @[{root, file, text}]@, @goals@ (a flat run's report goals or null),
--     @attempts@ (the attempt directories' names), @auto@ (the automation
--     was asked for), @retrieval@ and @library@ (on: from this server's
--     run, else from the run's facts and library events, else on) and
--     @apply@ @{allowed, reason}@.
--   * @GET /api/search/:run/events@: server-sent events, one per complete
--     line of events.jsonl: @id:@ is the byte offset just past the line,
--     @data:@ the line as written. It starts at @Last-Event-ID@, else
--     @?from=@, else 0 (an offset must be 0 or just past a newline); a
--     last line with no newline yet is held back until it has one. The
--     file is polled every 250 ms. The stream ends with @event: end@,
--     @data: {status}@: once the run has its report (and no run of this
--     server is still writing it), once this server's run exits (read
--     three more times first), or, for a run no process of this server
--     writes, after 'leIdle' (5 minutes) with no new line (status
--     @unknown@). Asked for at the end of a finished run, it answers 204.
--   * @POST /api/search@ @{project, file, graph, auto, retrieval,
--     library}@: runs @fisher search FILE [--graph] [--no-auto]
--     [--no-retrieval] [--no-library] --out DIR@ in the project (retrieval
--     and library default to on: no flag)
--     (FILE relative to it; DIR a new @~/.cache/fisher/search/<stamp>@),
--     stdin from /dev/null, in its own process group ("Fisher.UI.Children"),
--     output to @$FISHER_HOME/fisher/ui-runs/search-<stamp>.log@: @{run}@.
--     A retry of a goal is this on the same file: @fisher search@ has no
--     single-goal flag, so it searches the file as it is now.
--   * @POST /api/search/:run/stop@: the run's process tree stopped
--     (SIGTERM, SIGKILL after 10 s); 202 at once, 409 when no run of this
--     server is going there.
--   * @POST /api/search/:run/apply@: base.lean written over the run's file
--     in its project (a new file beside it, renamed over it), under
--     fisher-search's rule for @--apply@ ('applyAllowed': a graph run only
--     when no lemma from a split is left with its sorry; a flat run
--     always). Refused (409, with the reason) while it runs, without a
--     report, for a file that is a link, outside its project or not a
--     @.lean@ file, and for a bench run (its workspace is the bench's).
--
-- A run's status: @running@, @stopping@ (this server's run); @done@ (it
-- has its report); @stopped@ (stopped here, even when it wrote a report as
-- it stopped: the page shows that report, but the run did not end on its
-- own); @crashed@ (this server's run exited with no report);
-- @incomplete@ (no report and no run of this server).
module Fisher.UI.Api.Search
  ( SearchEnv (..)
  , SearchRun (..)
  , newSearchEnv
  , searchApi
  , stopSearches
  , completeLines
  , graphFromJson
  , needsEvents
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (SomeException, try)
import Control.Monad (forM, unless, void, when)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import Data.Char (isAlphaNum, isDigit)
import qualified Data.IntMap.Strict as IM
import Data.IORef
import Data.List (isPrefixOf, isSuffixOf, nub, sort, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import Data.Ord (Down (..))
import Data.Scientific (toBoundedInteger, toRealFloat)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (getPOSIXTime, utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getZonedTime)
import qualified Data.Vector as V
import Network.HTTP.Types
import Network.Wai
import System.Directory
import System.Exit (ExitCode (..))
import System.FilePath (isAbsolute, makeRelative, normalise, splitDirectories, takeDirectory, takeExtension, (</>))
import System.IO (IOMode (..), SeekMode (..), hFileSize, hSeek, withBinaryFile)
import System.Posix.Files (fileID, getFileStatus)
import System.Posix.Types (FileID)
import Text.Read (readMaybe)

import Fisher.Agent.Steps (fileHash)
import Fisher.Search.Graph (Graph (..), Node (..), NodeStatus (..), Split (..), SplitStatus (..))
import Fisher.Search.Report (applyAllowed, sorryLemmas)
import Fisher.UI.Api.Live (LiveEnv (..), bad, jsonBodyWith, textField)
import Fisher.UI.Children
import Fisher.UI.Db (UiEnv (..))
import Fisher.UI.Files (contained, writeOver)
import Fisher.UI.Response (Api, jsonResponse, pageHeaders)

-- | A search run this server started.
data SearchRun = SearchRun
  { srChild :: Child
  , srExit :: MVar ExitCode
  , srStopping :: IORef Bool
  , srProject :: FilePath
  , srFile :: FilePath
  -- ^ relative to the project
  , srGraph :: Bool
  , srAuto :: Bool
  , srRetrieval :: Bool
  , srLibrary :: Bool
  , srLog :: FilePath
  , srOriginal :: Maybe Text
  -- ^ the file's hash when the run started
  }

data SearchEnv = SearchEnv
  { seLive :: LiveEnv
  -- ^ the fisher to run, the stop grace, the poll and idle times
  , seRoot :: FilePath
  -- ^ @~/.cache/fisher/search@
  , seRuns :: IORef (M.Map Text SearchRun)
  -- ^ by run id
  , seClosing :: IORef Bool
  , seStart :: MVar ()
  -- ^ one start at a time (each takes a new directory)
  , seEvents :: IORef (M.Map FilePath ((Integer, UTCTime), EvSummary))
  -- ^ each events.jsonl's summary, by its size and time
  }

newSearchEnv :: LiveEnv -> FilePath -> IO SearchEnv
newSearchEnv live root = SearchEnv live root <$> newIORef M.empty <*> newIORef False <*> newMVar () <*> newIORef M.empty

ui :: SearchEnv -> UiEnv
ui = leUi . seLive

searchApi :: SearchEnv -> Api
searchApi env r = case (requestMethod r, pathInfo r) of
  (m, ["api", "search"])
    | m == methodGet -> Just . jsonResponse ok200 <$> listAll env
    | m == methodPost -> Just <$> start env r
  (m, ["api", "search", rid]) | m == methodGet -> Just <$> withRun rid (detail env)
  (m, ["api", "search", rid, "events"]) | m == methodGet -> Just <$> withRun rid (events env r)
  (m, ["api", "search", rid, "stop"]) | m == methodPost -> Just <$> withRun rid (stop env)
  (m, ["api", "search", rid, "apply"]) | m == methodPost -> Just <$> withRun rid (apply env r)
  _ -> pure Nothing
  where
    withRun rid k = case parseId rid of
      Nothing -> pure (bad badRequest400 "bad run id")
      Just ref -> do
        found <- locate env ref
        maybe (pure (bad notFound404 "no such run")) k found

-- ------------------------------------------------------------- run ids

data RunRef = RunRef { rfId :: Text, rfBench :: Maybe Text, rfParts :: [Text] }

data Located = Located { lcRef :: RunRef, lcDir :: FilePath }

-- | A name part of an id: letters, digits, @-@ and @_@.
namePart :: Text -> Bool
namePart t = not (T.null t) && T.all (\c -> isAlphaNum c || c == '-' || c == '_') t

parseId :: Text -> Maybe RunRef
parseId rid = case T.splitOn "." rid of
  ["s", n] | namePart n -> Just (RunRef rid Nothing [n])
  [b, arm, n] | b `elem` searchBenches, namePart arm, namePart n -> Just (RunRef rid (Just b) [arm, n])
  _ -> Nothing

dirOf :: SearchEnv -> RunRef -> FilePath
dirOf env ref = case (rfBench ref, rfParts ref) of
  (Nothing, [n]) -> seRoot env </> T.unpack n
  (Just b, [arm, n]) -> ueBench (ui env) </> T.unpack b </> "runs" </> T.unpack arm </> T.unpack n
  _ -> seRoot env </> "-"

-- | The run's directory when it is one: it exists and has events.jsonl or
-- report.json, or a run of this server is there.
locate :: SearchEnv -> RunRef -> IO (Maybe Located)
locate env ref = do
  let d = dirOf env ref
  isDir <- doesDirectoryExist d
  mine <- M.member (rfId ref) <$> readIORef (seRuns env)
  has <- or <$> mapM (doesFileExist . (d </>)) ["events.jsonl", "report.json"]
  pure (if isDir && (has || mine) then Just (Located ref d) else Nothing)

subdirs :: FilePath -> IO [FilePath]
subdirs d = do
  xs <- either (\(_ :: SomeException) -> []) sort <$> try (listDirectory d)
  filterM' [x | x <- xs, not ("." `isPrefixOf` x)]
  where
    filterM' = fmap catMaybes . mapM (\x -> doesDirectoryExist (d </> x) >>= \y -> pure (if y then Just x else Nothing))

-- | The benches whose runs are runs of fisher search (@runs/<arm>/<n>@).
searchBenches :: [Text]
searchBenches = ["g", "h", "w"]

-- | Every run: the search root's, then bench g's, h's and w's.
allRuns :: SearchEnv -> IO [Located]
allRuns env = do
  own <- subdirs (seRoot env)
  benches <- fmap concat . forM searchBenches $ \b -> do
    let runs = ueBench (ui env) </> T.unpack b </> "runs"
    arms <- subdirs runs
    fmap concat . forM arms $ \arm -> map (\n -> (b, arm, n)) <$> subdirs (runs </> arm)
  let refs =
        [RunRef ("s." <> t) Nothing [t] | n <- own, let t = T.pack n, namePart t]
          <> [RunRef (b <> "." <> a <> "." <> n') (Just b) [a, n'] | (b, arm, n) <- benches, let a = T.pack arm; n' = T.pack n, namePart a, namePart n']
  catMaybes <$> mapM (locate env) refs

-- ------------------------------------------------------------- reading

load :: FilePath -> IO (Maybe A.Value)
load p = either (\(_ :: SomeException) -> Nothing) A.decodeStrict <$> try (B.readFile p)

at :: Text -> A.Value -> Maybe A.Value
at k = \case
  A.Object o -> KM.lookup (K.fromText k) o
  _ -> Nothing

txt :: Text -> A.Value -> Maybe Text
txt k v = case at k v of
  Just (A.String s) -> Just s
  _ -> Nothing

int :: Text -> A.Value -> Maybe Int
int k v = case at k v of
  Just (A.Number n) -> toBoundedInteger n
  _ -> Nothing

dbl :: Text -> A.Value -> Maybe Double
dbl k v = case at k v of
  Just (A.Number n) -> Just (toRealFloat n)
  _ -> Nothing

list :: Text -> A.Value -> [A.Value]
list k v = case at k v of
  Just (A.Array xs) -> V.toList xs
  _ -> []

-- | The complete lines of a chunk read at an offset: each with the offset
-- just past its newline. A last line with no newline is left out; blank
-- lines are moved past and not given.
completeLines :: Integer -> B.ByteString -> [(Integer, B.ByteString)]
completeLines off0 chunk = go off0 chunk
  where
    go off s = case BC.elemIndex '\n' s of
      Nothing -> []
      Just i ->
        let l = B.take i s
            off' = off + fromIntegral i + 1
            rest = go off' (B.drop (i + 1) s)
         in if B.null (BC.strip l) then rest else (off', l) : rest

-- | The bytes of the file from an offset (none when it is not there).
readFrom :: FilePath -> Integer -> IO B.ByteString
readFrom p off = either (\(_ :: SomeException) -> "") id <$> try (withBinaryFile p ReadMode $ \h -> do
  size <- hFileSize h
  if size <= off then pure "" else hSeek h AbsoluteSeek off >> B.hGet h (fromIntegral (size - off)))

fileSize :: FilePath -> IO Integer
fileSize p = either (\(_ :: SomeException) -> 0) id <$> try (getFileSize p)

-- | graph.json as fisher-search's graph (its whiteboards left out).
graphFromJson :: A.Value -> Maybe Graph
graphFromJson v = do
  ns <- mapM node (list "nodes" v)
  ss <- mapM split (list "splits" v)
  roots <- mapM (\case A.Number n -> toBoundedInteger n; _ -> Nothing) (list "roots" v)
  pure (Graph (IM.fromList [(nId n, n) | n <- ns]) (IM.fromList [(sId s, s) | s <- ss]) roots IM.empty)
  where
    ints k o = mapM (\case A.Number n -> toBoundedInteger n; _ -> Nothing) (list k o)
    node o = do
      i <- int "id" o
      st <- case txt "status" o of
        Just "open" -> Just NOpen
        Just "proved" -> Just NProved
        Just "failed" -> Just (NFailed (fromMaybe "" (txt "reason" o)))
        _ -> Nothing
      sp <- ints "splits" o
      Just (Node i (fromMaybe "" (txt "name" o)) (fromMaybe "" (txt "key" o)) (fromMaybe i (int "root" o)) (fromMaybe 0 (int "depth" o)) st sp (fromMaybe 0 (int "attempts" o)) Nothing)
    split o = do
      i <- int "id" o
      p <- int "parent" o
      ls <- ints "lemmas" o
      let why = fromMaybe "" (txt "reason" o)
      st <- case txt "status" o of
        Just "proposed" -> Just SProposed
        Just "rejected" -> Just (SRejected why)
        Just "accepted" -> Just SAccepted
        Just "failed" -> Just (SFailed why)
        Just "done" -> Just SDone
        _ -> Nothing
      Just (Split i p ls st)

-- ------------------------------------------------------------- status

ourRun :: SearchEnv -> Text -> IO (Maybe SearchRun)
ourRun env rid = M.lookup rid <$> readIORef (seRuns env)

-- | The status, whether this server's run is going, and the report.
stateOf :: SearchEnv -> Located -> IO (Text, Bool, Maybe A.Value)
stateOf env lc = do
  rep <- load (lcDir lc </> "report.json")
  mine <- ourRun env (rfId (lcRef lc))
  case mine of
    Just run -> do
      ex <- tryReadMVar (srExit run)
      stopping <- readIORef (srStopping run)
      pure $ case ex of
        Nothing -> (if stopping then "stopping" else "running", True, rep)
        Just _
          | stopping -> ("stopped", False, rep)
          | isJust rep -> ("done", False, rep)
          | otherwise -> ("crashed", False, rep)
    Nothing -> pure (if isJust rep then "done" else "incomplete", False, rep)

isGraph :: Maybe SearchRun -> Maybe A.Value -> Bool -> Bool -> Bool
isGraph mine rep nodeAdded hasGraph = case rep >>= txt "mode" of
  Just m -> m == "graph"
  Nothing -> maybe False srGraph mine || hasGraph || nodeAdded

-- | What the list needs from an events file: whether it has a graph's
-- events, the goals closed and in all counted both ways, the last @t@.
data EvSummary = EvSummary
  { esNodes :: Bool, esFlat :: (Int, Int), esGraph :: (Int, Int), esLastT :: Double
  , esFacts :: Bool
  -- ^ a @facts@ event (an index is built for the library too, so
  -- @index built@ alone does not say retrieval was on)
  , esLibrary :: Bool
  -- ^ an event of the library (@library offered@, @saved@, @problem@)
  }

-- | Whether the list must read the events: the report (if any) lacks its
-- mode, goal counts or elapsed seconds.
needsEvents :: Maybe A.Value -> Bool
needsEvents = \case
  Just v -> not (isJust (txt "mode" v) && isJust (int "goals_closed" v) && isJust (int "goals_total" v) && isJust (dbl "elapsed_s" v))
  Nothing -> True

-- | An events file's summary, read again only when its size or time changed.
evSummary :: SearchEnv -> FilePath -> IO EvSummary
evSummary env p = do
  st <- try ((,) <$> getFileSize p <*> getModificationTime p)
  case st of
    Left (_ :: SomeException) -> pure (EvSummary False (0, 0) (0, 0) 0 False False)
    Right k -> do
      cached <- M.lookup p <$> readIORef (seEvents env)
      case cached of
        Just (k', es) | k' == k -> pure es
        _ -> do
          evs <- mapMaybe (A.decodeStrict . snd) . completeLines 0 <$> readFrom p 0
          let es =
                EvSummary
                  (any ((== Just "node added") . txt "event") evs) (counted False evs) (counted True evs)
                  (case reverse (mapMaybe (dbl "t") evs) of { x : _ -> x; [] -> 0 })
                  (named ["facts"] evs) (named ["library offered", "library saved", "library problem"] evs)
          atomicModifyIORef' (seEvents env) (\m -> (M.insert p (k, es) m, ()))
          pure es

named :: [Text] -> [A.Value] -> Bool
named ns = any (\e -> maybe False (`elem` ns) (txt "event" e))

-- | Goals closed and in all, counted from the events (no report yet).
counted :: Bool -> [A.Value] -> (Int, Int)
counted graph evs
  | graph =
      let roots = nub [n | e <- evs, txt "event" e == Just "node added", int "depth" e == Just 0, Just n <- [int "node" e]]
          proved = S.fromList [n | e <- evs, txt "event" e == Just "node proved", Just n <- [int "node" e]]
       in (length (filter (`S.member` proved) roots), length roots)
  | otherwise =
      let goals = nub (mapMaybe (txt "goal") evs)
          closed = S.fromList [g | e <- evs, txt "event" e `elem` [Just "closed", Just "auto closed"], Just g <- [txt "goal" e]]
       in (length (filter (`S.member` closed) goals), length goals)

summary :: SearchEnv -> Located -> IO (A.Object, Maybe A.Value, Bool)
summary env lc = do
  let d = lcDir lc
      ref = lcRef lc
  (status, running, rep) <- stateOf env lc
  mine <- ourRun env (rfId ref)
  -- a finished run's report has all the list shows: its events are not read
  es <- if needsEvents rep then Just <$> evSummary env (d </> "events.jsonl") else pure Nothing
  hasGraph <- doesFileExist (d </> "graph.json")
  started <- startedAt d rep (maybe 0 esLastT es)
  let graph = isGraph mine rep (maybe False esNodes es) hasGraph
      (c, t) = maybe (0, 0) (if graph then esGraph else esFlat) es
      file = (rep >>= txt "file") `orElse` (T.pack . srFile <$> mine)
      project = (rep >>= txt "project") `orElse` (T.pack . srProject <$> mine)
      o =
        KM.fromList
          [ ("run", A.String (rfId ref)), ("dir", A.toJSON d), ("bench", A.toJSON (rfBench ref)), ("file", A.toJSON file)
          , ("project", A.toJSON project), ("mode", A.String (if graph then "graph" else "flat")), ("started", A.toJSON started)
          , ("closed", A.toJSON (maybe c id (rep >>= int "goals_closed"))), ("total", A.toJSON (maybe t id (rep >>= int "goals_total")))
          , ("status", A.String status), ("running", A.Bool running) ]
  pure (o, rep, graph)
  where
    orElse (Just a) _ = Just a
    orElse Nothing b = b

-- | POSIX seconds: report.json's time less its elapsed seconds, else the
-- events file's time less its last event's @t@, else the directory's time.
startedAt :: FilePath -> Maybe A.Value -> Double -> IO Double
startedAt d rep lastT = do
  let mtime p = either (\(_ :: SomeException) -> Nothing) (Just . realToFrac . utcTimeToPOSIXSeconds) <$> try (getModificationTime p)
  rt <- mtime (d </> "report.json")
  et <- mtime (d </> "events.jsonl")
  dt <- fromMaybe 0 <$> mtime d
  pure $ case (rt, rep >>= dbl "elapsed_s", et) of
    (Just r, Just e, _) -> r - e
    (_, _, Just e) -> e - lastT
    _ -> dt

listAll :: SearchEnv -> IO [A.Value]
listAll env = do
  runs <- allRuns env
  rows <- forM runs $ \lc -> (\(o, _, _) -> o) <$> summary env lc
  pure (map A.Object (sortOn (Down . started) rows))
  where
    started o = case KM.lookup "started" o of
      Just (A.Number n) -> toRealFloat n :: Double
      _ -> 0

detail :: SearchEnv -> Located -> IO Response
detail env lc = do
  let d = lcDir lc
  (o, rep, graph) <- summary env lc
  gj <- if graph then load (d </> "graph.json") else pure Nothing
  files <- either (\(_ :: SomeException) -> []) sort <$> try (listDirectory d)
  let boardFiles = [f | f <- files, "whiteboard-" `isPrefixOf` f, ".md" `isSuffixOf` f]
  boards <- forM (sortOn boardRoot boardFiles) $ \f -> do
    t <- either (\(_ :: SomeException) -> "") (TE.decodeUtf8With TEE.lenientDecode) <$> try (B.readFile (d </> f))
    pure (A.object ["root" A..= boardRoot f, "file" A..= f, "text" A..= t])
  -- a graph.json whiteboard with no file of its own
  let fromGraph =
        [ A.object ["root" A..= r, "file" A..= A.Null, "text" A..= t]
        | Just g <- [gj], w <- list "whiteboards" g, Just r <- [int "root" w], Just t <- [txt "text" w]
        , r `notElem` mapMaybe boardRoot boardFiles ]
  attempts <- subdirs (d </> "attempts")
  mine <- ourRun env (rfId (lcRef lc))
  ap <- applyCheck env lc False
  es <- evSummary env (d </> "events.jsonl")
  let auto = case rep of
        Just v -> at "auto_requested" v == Just (A.Bool True)
        Nothing -> maybe False srAuto mine
      -- this server's run knows its flags; else the events tell (a run
      -- with the facts, or the library, writes their events for every
      -- goal: one of them alone means the other was off); else on (no
      -- flag passed, as for a run from before retrieval)
      retrieval = maybe (esFacts es || not (esLibrary es)) srRetrieval mine
      library = maybe (esLibrary es || not (esFacts es)) srLibrary mine
      goals = if graph then A.Null else maybe A.Null (\v -> maybe A.Null (const (A.toJSON (list "goals" v))) (at "goals" v)) rep
      more =
        [ ("report", maybe A.Null id rep), ("graph", maybe A.Null id gj), ("whiteboards", A.toJSON (boards <> fromGraph))
        , ("goals", goals), ("attempts", A.toJSON (sortOn attemptKey attempts)), ("auto", A.Bool auto)
        , ("retrieval", A.Bool retrieval), ("library", A.Bool library)
        , ("apply", A.object (either (\(why, changed) -> ["allowed" A..= False, "reason" A..= why, "changed" A..= changed]) (const ["allowed" A..= True, "reason" A..= A.Null, "changed" A..= False]) ap)) ]
  pure (jsonResponse ok200 (A.Object (KM.union (KM.fromList more) o)))
  where
    boardRoot :: FilePath -> Maybe Int
    boardRoot f = readMaybe (takeWhile isDigit (drop (length ("whiteboard-" :: String)) f))
    attemptKey :: FilePath -> (Int, FilePath)
    attemptKey a = (fromMaybe 0 (readMaybe (takeWhile isDigit a)), a)

-- ------------------------------------------------------------- stream

-- | Where a stream starts: Last-Event-ID, else ?from=, else 0.
cursorOf :: Request -> Maybe Integer
cursorOf r = case lookup "Last-Event-ID" (requestHeaders r) of
  Just h -> readMaybe (BC.unpack h)
  Nothing -> case lookup "from" (queryString r) of
    Just (Just v) -> readMaybe (BC.unpack v)
    Just Nothing -> Nothing
    Nothing -> Just 0

-- | Whether the offset starts a line of the file (0, or just past a newline).
atLineStart :: FilePath -> Integer -> IO Bool
atLineStart p off
  | off == 0 = pure True
  | off < 0 = pure False
  | otherwise = do
      size <- fileSize p
      if off > size
        then pure False
        else (== "\n") <$> either (\(_ :: SomeException) -> "") id <$> try (withBinaryFile p ReadMode $ \h -> hSeek h AbsoluteSeek (off - 1) >> B.hGet h 1)

-- | The file's inode, when it is there.
inodeOf :: FilePath -> IO (Maybe FileID)
inodeOf p = either (\(_ :: SomeException) -> Nothing) (Just . fileID) <$> try (getFileStatus p)

-- | The bytes of the file just before an offset (at most 4096).
bytesBefore :: FilePath -> Integer -> IO B.ByteString
bytesBefore p c
  | c <= 0 = pure ""
  | otherwise = do
      let from = max 0 (c - 4096)
      either (\(_ :: SomeException) -> "") id <$> try (withBinaryFile p ReadMode $ \h -> hSeek h AbsoluteSeek from >> B.hGet h (fromIntegral (c - from)))

-- | What the stream has seen of the file: its inode, the cursor and the
-- bytes just before it. The file is intact while its inode is the same,
-- it is no shorter than the cursor and those bytes are still there; else
-- it was truncated or replaced (rewritten).
data Seen = Seen { snInode :: Maybe FileID, snCursor :: Integer, snTail :: B.ByteString }

intact :: FilePath -> Seen -> IO (Bool, Maybe FileID)
intact p sn = do
  ino <- inodeOf p
  size <- fileSize p
  before <- bytesBefore p (snCursor sn)
  let sameFile = case (snInode sn, ino) of
        (Just a, Just b) -> a == b
        (Just _, Nothing) -> False
        (Nothing, _) -> True
      tailOk = B.length before >= B.length (snTail sn) && B.drop (B.length before - B.length (snTail sn)) before == snTail sn
  pure (sameFile && size >= snCursor sn && tailOk, ino)

-- | Seconds since the events file (else the run directory) last changed.
stillFor :: FilePath -> FilePath -> IO Double
stillFor p d = do
  now <- realToFrac <$> getPOSIXTime
  t <- try (getModificationTime p) >>= \case
    Right x -> pure (Just x)
    Left (_ :: SomeException) -> either (\(_ :: SomeException) -> Nothing) Just <$> try (getModificationTime d)
  pure (maybe 0 (\x -> now - realToFrac (utcTimeToPOSIXSeconds x)) t)

events :: SearchEnv -> Request -> Located -> IO Response
events env r lc = do
  let p = lcDir lc </> "events.jsonl"
      rid = rfId (lcRef lc)
      live = seLive env
  case cursorOf r of
    Nothing -> pure (bad badRequest400 "the cursor is a byte offset")
    Just c0 -> do
      size0 <- fileSize p
      ok <- if c0 > size0 then pure True else atLineStart p c0
      (_, running0, rep0) <- stateOf env lc
      if not ok
        then pure (bad badRequest400 "the offset is not the start of a line of events.jsonl")
        else
          if isJust rep0 && not running0 && c0 == size0
            then pure (responseLBS noContent204 (("Cache-Control", "no-store") : pageHeaders) "")
            else pure . responseStream ok200 sseHeaders $ \write flush -> do
              write "retry: 1000\n\n" >> flush
              ino0 <- inodeOf p
              tail0 <- bytesBefore p c0
              let send ls = unless (null ls) (mapM_ (\(o, l) -> write ("id: " <> BB.integerDec o <> "\ndata: " <> BB.byteString l <> "\n\n")) ls >> flush)
                  ending st = write ("event: end\ndata: " <> BB.lazyByteString (A.encode (A.object ["status" A..= (st :: Text)])) <> "\n\n") >> flush
                  -- the complete lines past the cursor, once the file is
                  -- checked to be the one read so far (Nothing: rewritten)
                  readSend sn = do
                    (fine, ino) <- intact p sn
                    if not fine
                      then pure Nothing
                      else do
                        chunk <- readFrom p (snCursor sn)
                        let ls = completeLines (snCursor sn) chunk
                        send ls
                        let c' = if null ls then snCursor sn else fst (last ls)
                            seenBytes = snTail sn <> B.take (fromIntegral (c' - snCursor sn)) chunk
                            tl = B.drop (B.length seenBytes - 4096) seenBytes
                        pure (Just (Seen (maybe ino Just (snInode sn)) c' tl, not (null ls)))
                  -- the run's last lines (read again, 200 ms apart, for
                  -- lines written as it went), then its end
                  finish sn k =
                    readSend sn >>= \case
                      Nothing -> ending "rewritten"
                      Just (sn', _)
                        | k > (0 :: Int) -> threadDelay 200000 >> finish sn' (k - 1)
                        | otherwise -> do
                            (st, _, _) <- stateOf env lc
                            ending st
                  loop sn quiet = do
                    -- the state first: a report there now means every line
                    -- is in the file by the time it is read
                    (st, running, rep) <- stateOf env lc
                    mine <- ourRun env rid
                    readSend sn >>= \case
                      Nothing -> ending "rewritten"
                      Just (sn', got) -> do
                        let quiet' = if got then 0 else quiet + lePoll live
                        still <- if isJust mine then pure 0 else stillFor p (lcDir lc)
                        if isJust rep && not running
                          then ending st
                          else case mine of
                            Just _ | not running -> finish sn' 3
                            -- no process of this server writes it: it ends
                            -- once the file has been still for the idle bound
                            Nothing | still * 1000000 >= fromIntegral (leIdle live) -> ending "unknown"
                            _ -> do
                              q <- if quiet' >= leKeepAlive live then write ": keep-alive\n\n" >> flush >> pure 0 else pure quiet'
                              threadDelay (lePoll live)
                              loop sn' q
              if c0 > size0
                then ending "rewritten"
                else loop (Seen ino0 c0 tail0) 0
  where
    sseHeaders =
      [ (hContentType, "text/event-stream; charset=utf-8"), ("Cache-Control", "no-store")
      , ("X-Accel-Buffering", "no") ] <> pageHeaders

-- ------------------------------------------------------------- start

-- | The body as JSON (64 KB at most).
bodyOf :: Request -> IO (Maybe A.Object)
bodyOf = jsonBodyWith 65536

boolField :: A.Key -> A.Object -> Maybe Bool
boolField k o = case KM.lookup k o of
  Just (A.Bool b) -> Just b
  _ -> Nothing

start :: SearchEnv -> Request -> IO Response
start env r = do
  body <- bodyOf r
  case body of
    Just o
      | Just p <- textField "project" o, Just f <- textField "file" o -> do
          let graph = fromMaybe False (boolField "graph" o)
              auto = fromMaybe True (boolField "auto" o)
              flags = Flags graph auto (fromMaybe True (boolField "retrieval" o)) (fromMaybe True (boolField "library" o))
              project = T.unpack p
          isDir <- doesDirectoryExist project
          if not (isAbsolute project) || not isDir
            then pure (bad badRequest400 "the project is not an existing absolute directory")
            else do
              proj <- canonicalizePath project
              let path0 = T.unpack f
                  path = normalise (if isAbsolute path0 then path0 else proj </> path0)
              isFile <- doesFileExist path
              inside <- if isFile then contained proj path else pure False
              rel <- if inside then (\c -> makeRelative proj c) <$> canonicalizePath path else pure ""
              closing <- readIORef (seClosing env)
              case () of
                _ | T.null f || T.any (`elem` ("\r\n\0" :: String)) f -> pure (bad badRequest400 "the file is one path")
                  | not isFile -> pure (bad badRequest400 "no such file")
                  | not inside || ".." `elem` splitDirectories rel || isAbsolute rel -> pure (bad badRequest400 "the file is not inside the project, or is a link")
                  | takeExtension path /= ".lean" -> pure (bad badRequest400 "fisher search takes a Lean file")
                  | otherwise -> lakeRoot path >>= \case
                      Nothing -> pure (bad badRequest400 "no lakefile above the file: fisher search runs in a Lake project (lakefile.lean or lakefile.toml)")
                      Just _ -> go closing proj rel flags
    _ -> pure (bad badRequest400 "send {project, file, graph, auto, retrieval, library} as JSON")
  where
    go closing proj rel flags
      | closing = pure (bad serviceUnavailable503 "fisher ui is stopping")
      | otherwise = launch env proj rel flags

-- | A search run's choices: the lemma graph, the automation, the facts
-- (retrieval) and the lemma library.
data Flags = Flags { fGraph :: Bool, fAuto :: Bool, fRetrieval :: Bool, fLibrary :: Bool }

-- | The flags of @fisher search@ for them (none for the defaults).
flagArgs :: Flags -> [String]
flagArgs f =
  ["--graph" | fGraph f] <> ["--no-auto" | not (fAuto f)] <> ["--no-retrieval" | not (fRetrieval f)] <> ["--no-library" | not (fLibrary f)]

-- | The directory of a lakefile at or above the file's (as fisher search
-- finds its project).
lakeRoot :: FilePath -> IO (Maybe FilePath)
lakeRoot path = walk (takeDirectory path)
  where
    walk d = do
      has <- or <$> mapM (doesFileExist . (d </>)) ["lakefile.toml", "lakefile.lean"]
      if has then pure (Just d) else let up = takeDirectory d in if up == d then pure Nothing else walk up

launch :: SearchEnv -> FilePath -> FilePath -> Flags -> IO Response
launch env proj rel flags = do
  made <- withMVar (seStart env) $ \_ -> do
    stamp <- formatTime defaultTimeLocale "%Y%m%d-%H%M%S" <$> getZonedTime
    createDirectoryIfMissing True (seRoot env)
    name <- fresh stamp (0 :: Int)
    let out = seRoot env </> name
        rid = "s." <> T.pack name
        home = ueHome (ui env)
        logp = home </> "fisher" </> "ui-runs" </> ("search-" <> name <> ".log")
        argv = [leFisher (seLive env), "search", rel] <> flagArgs flags <> ["--out", out]
    createDirectory out
    original <- either (\(_ :: SomeException) -> Nothing) (Just . fileHash) <$> try (B.readFile (proj </> rel))
    res <- try (spawnChild (Just logp) proj argv [("FISHER_HOME", home)])
    case res of
      Left (e :: SomeException) -> pure (Left (T.pack (show e)))
      Right (ch, exited) -> do
        run <- SearchRun ch exited <$> newIORef False <*> pure proj <*> pure rel <*> pure (fGraph flags) <*> pure (fAuto flags) <*> pure (fRetrieval flags) <*> pure (fLibrary flags) <*> pure logp <*> pure original
        atomicModifyIORef' (seRuns env) (\m -> (M.insert rid run m, ()))
        closing <- readIORef (seClosing env)
        when closing (stopRun env run)
        pure (Right rid)
  pure $ case made of
    Left e -> bad internalServerError500 e
    Right rid -> jsonResponse ok200 (A.object ["run" A..= rid])
  where
    fresh stamp n = do
      let name = if n == 0 then stamp else stamp <> "-" <> show n
      taken <- doesPathExist (seRoot env </> name)
      if taken then fresh stamp (n + 1) else pure name

-- ------------------------------------------------------------- stop

stopRun :: SearchEnv -> SearchRun -> IO ()
stopRun env run = do
  atomicModifyIORef' (srStopping run) (const (True, ()))
  void (try (stopChildWith (leGrace (seLive env)) (srChild run)) :: IO (Either SomeException ()))

stop :: SearchEnv -> Located -> IO Response
stop env lc =
  ourRun env (rfId (lcRef lc)) >>= \case
    Nothing -> pure (bad conflict409 "no run of this server is going there")
    Just run -> do
      ex <- tryReadMVar (srExit run)
      if isJust ex
        then pure (bad conflict409 "the run has ended")
        else do
          atomicModifyIORef' (srStopping run) (const (True, ()))
          _ <- forkIO (stopRun env run)
          pure (jsonResponse accepted202 (A.object ["status" A..= ("stopping" :: Text)]))

-- | Stop every search run still going and wait for them; none starts
-- after this. The server calls it as it stops.
stopSearches :: SearchEnv -> IO ()
stopSearches env = do
  atomicModifyIORef' (seClosing env) (const (True, ()))
  runs <- M.elems <$> readIORef (seRuns env)
  dones <- forM runs $ \run -> do
    done <- newEmptyMVar
    ex <- tryReadMVar (srExit run)
    if isJust ex then putMVar done () else void (forkIO (stopRun env run >> putMVar done ()))
    pure done
  mapM_ takeMVar dones

-- ------------------------------------------------------------- apply

-- | Where apply would write and what, or why it may not (and whether
-- the only reason is that the file changed since the run started, which
-- @force@ overrides). The refusals that need nothing read come first.
applyCheck :: SearchEnv -> Located -> Bool -> IO (Either (Text, Bool) (FilePath, B.ByteString))
applyCheck env lc force = do
  let d = lcDir lc
      no why = pure (Left (why, False))
  (_, running, rep) <- stateOf env lc
  case rep of
    _ | isJust (rfBench (lcRef lc)) -> no "a bench run's workspace is the bench's; apply is for runs of fisher search"
      | running -> no "the run is still going"
    Nothing -> no "the run has no report.json (it has not finished)"
    Just v -> case (txt "project" v, txt "file" v) of
      (Nothing, _) -> no "the report names no project"
      (_, Nothing) -> no "the report names no file"
      (Just p, Just f)
        | isAbsolute (T.unpack f) || ".." `elem` splitDirectories (T.unpack f) || takeExtension (T.unpack f) /= ".lean" ->
            no "the run's file is not a Lean file inside its project"
        | otherwise -> do
            let proj0 = T.unpack p
                target = proj0 </> T.unpack f
            projOk <- doesDirectoryExist proj0
            exists <- doesFileExist target
            inside <- if projOk && exists then contained proj0 target else pure False
            baseR <- try (B.readFile (d </> "base.lean"))
            case baseR of
              _ | not projOk || not exists -> no ("the run's file " <> T.pack target <> " is not there")
                | not inside -> no "the run's file is not inside its project, or is a link"
              Left (_ :: SomeException) -> no "the run has no base.lean"
              Right base -> do
                rule <-
                  if txt "mode" v == Just "graph"
                    then do
                      gj <- load (d </> "graph.json")
                      let baseT = TE.decodeUtf8With TEE.lenientDecode base
                      pure $ case gj >>= graphFromJson of
                        Nothing -> Just "the run's graph.json could not be read"
                        Just g
                          | applyAllowed g baseT -> Nothing
                          | otherwise ->
                              let left = sorryLemmas g baseT
                               in Just ("not applied: " <> T.pack (show (length left)) <> " lemmas from splits are still sorry (" <> T.intercalate ", " left <> ")")
                    else pure Nothing
                case rule of
                  Just why -> no why
                  Nothing
                    | force -> pure (Right (target, base))
                    | otherwise -> do
                        changed <- changedSince env lc v target
                        pure (maybe (Right (target, base)) (\why -> Left (why, True)) changed)

-- | Why the file is not the one the run started on, if it is not: its
-- text differs from the one this server's run started on, or it was
-- written after the run started.
changedSince :: SearchEnv -> Located -> A.Value -> FilePath -> IO (Maybe Text)
changedSince env lc rep target = do
  mine <- ourRun env (rfId (lcRef lc))
  now <- either (\(_ :: SomeException) -> Nothing) (Just . fileHash) <$> try (B.readFile target)
  mt <- either (\(_ :: SomeException) -> Nothing) (Just . realToFrac . utcTimeToPOSIXSeconds) <$> try (getModificationTime target)
  started <- case mine of
    Just run -> pure (chStarted (srChild run))
    Nothing -> startedAt (lcDir lc) (Just rep) 0
  let name = T.pack (takeFileNameOf target)
  pure $ case (mine >>= srOriginal, now, mt) of
    (Just o, Just h, _) | o /= h -> Just (name <> " is not the text the run started on (it changed since)")
    (_, _, Just m) | m > started -> Just (name <> " was written after the run started")
    _ -> Nothing
  where
    takeFileNameOf = reverse . takeWhile (/= '/') . reverse

apply :: SearchEnv -> Request -> Located -> IO Response
apply env r lc = do
  body <- bodyOf r
  let force = maybe False (fromMaybe False . boolField "force") body
  applyCheck env lc force >>= \case
    Left (why, changed) -> pure (jsonResponse conflict409 (A.object ["error" A..= why, "changed" A..= changed]))
    Right (target, base) -> do
      r <- try (writeOver target base)
      pure $ case r of
        Left (e :: SomeException) -> bad internalServerError500 (T.pack (show e))
        Right () -> jsonResponse ok200 (A.object ["written" A..= target, "bytes" A..= B.length base])
