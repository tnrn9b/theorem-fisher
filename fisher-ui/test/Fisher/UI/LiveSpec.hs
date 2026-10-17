-- | Live sessions through the whole server: the ledger as a stream that
-- resumes after Last-Event-ID, and runs of a fake fisher (a shell script
-- that writes ui_runs and ui_events with sqlite3 over one connection, as
-- the agent does) that finish, crash, ignore SIGTERM and take a message.
module Fisher.UI.LiveSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, bracket, finally, try)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Database.SQLite.Simple
import Network.HTTP.Types
import Network.Wai (Application, Request (..))
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wai.Test
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.IO (hGetLine)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, terminateProcess, waitForProcess)
import System.Posix.Files (setFileMode)
import System.Posix.Signals (nullSignal, sigKILL, signalProcess)
import System.Timeout (timeout)
import Test.Hspec

import Fisher.UI.Access (Gate (..))
import Fisher.UI.Api.Live
import Fisher.UI.Db (UiEnv (..), newUiEnv)
import Fisher.UI.FakeFisher (fakeScript, ledgerSchema)
import Fisher.UI.Server (liveApis, serve)

gate :: Gate
gate = Gate "0123456789abcdef0123456789abcdef" 8732

call :: Application -> Method -> B.ByteString -> [Header] -> BL.ByteString -> IO SResponse
call app m p hs body = runSession (srequest (SRequest req body)) app
  where
    req =
      setPath
        defaultRequest
          { requestMethod = m
          , requestHeaderHost = Just "127.0.0.1:8732"
          , requestHeaders =
              -- the test's own headers first: the first of a name is the one read
              hs <> [ ("Host", "127.0.0.1:8732"), (hCookie, "fisher_ui_8732=0123456789abcdef0123456789abcdef")
                    , ("Origin", "http://127.0.0.1:8732") ] }
        p

getJ :: Application -> B.ByteString -> IO (Int, A.Value)
getJ app p = do
  r <- call app methodGet p [] ""
  pure (statusCode (simpleStatus r), maybe A.Null id (A.decode (simpleBody r)))

postJ :: Application -> B.ByteString -> A.Value -> IO (Int, A.Value)
postJ app p v = do
  r <- call app methodPost p [(hContentType, "application/json")] (A.encode v)
  pure (statusCode (simpleStatus r), maybe A.Null id (A.decode (simpleBody r)))

str :: Text -> A.Value -> Maybe Text
str k = \case
  A.Object o | Just (A.String t) <- KM.lookup (K.fromText k) o -> Just t
  _ -> Nothing

-- | The stream's events: (id, kind, data).
sse :: BL.ByteString -> [(Int, Text, A.Value)]
sse body = mapMaybe event (splitOn "\n\n" (BLC.unpack body))
  where
    event block =
      let ls = lines block
          ids = [read (drop 4 l) | l <- ls, take 4 l == "id: "]
          ds = [drop 6 l | l <- ls, take 6 l == "data: "]
       in case (ids, ds) of
            ([n], [d]) | Just v <- A.decode (BLC.pack d) -> Just (n, maybe "" id (str "kind" v), dataOf v)
            _ -> Nothing
    dataOf = \case
      A.Object o | Just d <- KM.lookup "data" o -> d
      _ -> A.Null
    splitOn sep s = go s
      where
        go x = case breakOn x of
          (a, Nothing) -> [a]
          (a, Just rest) -> a : go rest
        breakOn x
          | null x = ("", Nothing)
          | take (length sep) x == sep = ("", Just (drop (length sep) x))
          | otherwise = let (a, r) = breakOn (drop 1 x) in (take 1 x <> a, r)

-- | Another process (python3, fcntl.lockf: the agent's own lock) holding
-- the file's lock while the action runs.
withLockHolder :: FilePath -> IO a -> IO a
withLockHolder p k =
  bracket
    (createProcess (proc "python3" ["-c", "import fcntl,sys,time\nf=open(sys.argv[1],'a')\nfcntl.lockf(f, fcntl.LOCK_EX)\nprint('locked', flush=True)\ntime.sleep(60)\n", p]) {std_out = CreatePipe})
    (\(_, _, _, ph) -> terminateProcess ph >> waitForProcess ph)
    (\(_, mout, _, _) -> mapM_ hGetLine mout >> k)

stream :: Application -> B.ByteString -> [Header] -> IO (Int, [(Int, Text, A.Value)])
stream app p hs = do
  r <- timeout 30000000 (call app methodGet p hs "")
  case r of
    Nothing -> fail ("the stream of " <> BC.unpack p <> " did not end")
    Just x -> pure (statusCode (simpleStatus x), sse (simpleBody x))

-- | Read a stream over a real socket until event n has come (and
-- whatever came with it), then hang up: a page that goes away mid-run.
readUntil :: Int -> B.ByteString -> Int -> IO [(Int, Text, A.Value)]
readUntil port path n =
  bracket connect NS.close $ \s -> do
    NSB.sendAll s ("GET " <> path <> " HTTP/1.0\r\nHost: 127.0.0.1:8732\r\nCookie: fisher_ui_8732=0123456789abcdef0123456789abcdef\r\n\r\n")
    let go acc = do
          let body = snd (B.breakSubstring "\r\n\r\n" acc)
              evs = sse (BL.fromStrict (B.drop 4 body))
          if any (\(k, _, _) -> k >= n) evs
            then pure evs
            else do
              c <- NSB.recv s 4096
              if B.null c then pure evs else go (acc <> c)
    r <- timeout 20000000 (go "")
    maybe (fail "the stream never reached the event") pure r
  where
    connect = do
      s <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
      NS.connect s (NS.SockAddrInet (fromIntegral port) (NS.tupleToHostAddress (127, 0, 0, 1)))
      pure s

ids :: [(Int, Text, A.Value)] -> [Int]
ids = map (\(n, _, _) -> n)

kinds :: [(Int, Text, A.Value)] -> [Text]
kinds = map (\(_, k, _) -> k)

schema :: [Text]
schema = ledgerSchema <> ["CREATE TABLE IF NOT EXISTS sessions (id TEXT PRIMARY KEY, parent_session_id TEXT)"]

-- | A GAUSS_HOME, a project directory and the live server over them, with
-- a fake fisher whose run is the given shell text and a 1 s stop grace.
withLive :: Text -> (FilePath -> FilePath -> LiveEnv -> Application -> IO a) -> IO a
withLive body k = withSystemTempDirectory "fisher-ui-live" $ \tmp0 -> do
  tmp <- canonicalizePath tmp0
  let home = tmp </> "gauss"
      proj = tmp </> "proj"
      fake = tmp </> "fake-fisher"
  mapM_ (createDirectoryIfMissing True) [home, proj]
  TIO.writeFile fake (fakeScript True body)
  setFileMode fake 0o755
  ui <- newUiEnv home (tmp </> "bench") (tmp </> "repo")
  le0 <- newLiveEnv ui fake
  let le = le0 {leGrace = 1000000, leIdle = 1000000}
  r <- k home proj le (serve gate (liveApis le))
  stopRuns le
  pure r

-- | Poll a tag until its session is known.
sessionOf :: Application -> Text -> IO Text
sessionOf app tag = go (200 :: Int)
  where
    go n = do
      (_, v) <- getJ app ("/api/tags/" <> TE.encodeUtf8 tag)
      case str "session" v of
        Just s -> pure s
        Nothing | n > 0 -> threadDelay 50000 >> go (n - 1)
        Nothing -> fail ("tag " <> T.unpack tag <> " never got a session: " <> show v)

-- | Poll a tag until its status is the one wanted (up to 10 s).
statusBecomes :: Application -> Text -> Text -> IO A.Value
statusBecomes app tag want = go (200 :: Int)
  where
    go n = do
      (_, v) <- getJ app ("/api/tags/" <> TE.encodeUtf8 tag)
      if str "status" v == Just want
        then pure v
        else if n > 0 then threadDelay 50000 >> go (n - 1) else fail ("tag " <> T.unpack tag <> " never became " <> T.unpack want <> ": " <> show v)

startRun :: Application -> FilePath -> IO Text
startRun app proj = startWith app proj "/prove theories/Eval.v"

startWith :: Application -> FilePath -> Text -> IO Text
startWith app proj cmd = do
  (code, v) <- postJ app "/api/sessions" (A.object ["project" A..= proj, "command" A..= cmd])
  code `shouldBe` 200
  maybe (fail ("no tag: " <> show v)) pure (str "tag" v)

alive :: Int -> IO Bool
alive p = either (\(_ :: SomeException) -> False) (const True) <$> try (signalProcess nullSignal (fromIntegral p))

spec :: Spec
spec = describe "Fisher.UI.Api.Live" $ do
  describe "the event stream" $ do
    let fixture k = withLive "exit 0" $ \home _ le app -> do
          c <- open (home </> "state.db")
          mapM_ (execute_ c . Query) schema
          execute_ c "INSERT INTO sessions VALUES ('L', NULL), ('L2', 'L')"
          -- written out of order: the stream is in number order
          execute_ c "INSERT INTO ui_events VALUES ('L', 3, 0, 'tool_end', '{\"name\":\"read_file\"}'), ('L', 1, 0, 'thinking', '{\"label\":\"x\"}'),\
                     \ ('L', 5, 0, 'end', '{\"status\":\"completed\"}'), ('L', 2, 0, 'tool_start', '{\"name\":\"read_file\"}'),\
                     \ ('L', 4, 0, 'message', '{\"role\":\"assistant\",\"text\":\"done\"}'), ('other', 1, 0, 'end', '{}')"
          close c
          k le app
    it "sends the ledger in number order, ending with the end event" $ fixture $ \_ app -> do
      (code, evs) <- stream app "/api/sessions/L/events" []
      code `shouldBe` 200
      ids evs `shouldBe` [1, 2, 3, 4, 5]
      kinds evs `shouldBe` ["thinking", "tool_start", "tool_end", "message", "end"]
    it "resumes after Last-Event-ID (or ?after=) with no gap and no duplicate" $ fixture $ \_ app -> do
      (_, a) <- stream app "/api/sessions/L/events" []
      (_, b) <- stream app "/api/sessions/L/events" [("Last-Event-ID", "3")]
      ids b `shouldBe` [4, 5]
      ids (take 3 a <> b) `shouldBe` ids a
      (_, c) <- stream app "/api/sessions/L/events?after=3" []
      ids c `shouldBe` [4, 5]
      -- the header wins over the query, as on a reconnect of a page opened with ?after=
      (_, d) <- stream app "/api/sessions/L/events?after=1" [("Last-Event-ID", "4")]
      ids d `shouldBe` [5]
    it "answers 204 past the end, so the browser does not reconnect" $ fixture $ \_ app -> do
      (code, evs) <- stream app "/api/sessions/L/events" [("Last-Event-ID", "5")]
      code `shouldBe` 204
      evs `shouldBe` []
    it "answers 204 at once for a session with no ledger events that no run of this server drives" $ fixture $ \_ app -> do
      t0 <- getCurrentTime
      (code, evs) <- stream app "/api/sessions/nothing-here/events" []
      t1 <- getCurrentTime
      (code, evs) `shouldBe` (204, [])
      realToFrac (diffUTCTime t1 t0) `shouldSatisfy` (< (0.5 :: Double))
    it "keeps waiting (no early 204) for a session with no ledger events whose run lock another process holds" $ fixture $ \le app -> do
      let home = ueHome (leUi le)
      createDirectoryIfMissing True (home </> "fisher" </> "steps")
      let lockp = home </> "fisher" </> "steps" </> "elsewhere-1.lock"
      writeFile lockp ""
      withLockHolder lockp $ do
        t0 <- getCurrentTime
        (code, evs) <- stream app "/api/sessions/elsewhere-1/events" []
        t1 <- getCurrentTime
        code `shouldBe` 200
        -- the made-up end after the idle bound (1 s, counted in 250 ms polls)
        kinds evs `shouldBe` ["end"]
        realToFrac (diffUTCTime t1 t0) `shouldSatisfy` (>= (0.5 :: Double))
    it "streams a compression child's events from its root" $ fixture $ \_ app -> do
      (_, evs) <- stream app "/api/sessions/L2/events" []
      ids evs `shouldBe` [1, 2, 3, 4, 5]

  it "starts a run that finishes: a tag, the session it became, its events and the end" $
    withLive "{ hdr; sleep 0.3; ev 1 tool_start '{\"name\":\"read_file\"}'; sleep 0.3; ev 2 tool_end '{\"name\":\"read_file\",\"ok\":true}'; sleep 0.3; ev 3 message '{\"role\":\"assistant\",\"text\":\"done\"}'; ev 4 end '{\"status\":\"completed\"}'; } | sql; exit 0" $ \home proj _ app -> do
      tag <- startRun app proj
      sid <- sessionOf app tag
      sid `shouldBe` ("s_" <> tag)
      (_, evs) <- stream app ("/api/sessions/" <> TE.encodeUtf8 sid <> "/events") []
      ids evs `shouldBe` [1, 2, 3, 4]
      kinds evs `shouldBe` ["tool_start", "tool_end", "message", "end"]
      v <- statusBecomes app tag "completed"
      KM.lookup "exit" (case v of A.Object o -> o; _ -> mempty) `shouldBe` Just (A.Number 0)
      -- fisher launch --print <command>, in the project, with the ledger on
      lines <$> readFile (home </> T.unpack ("args-" <> tag <> ".txt")) `shouldReturn` ["launch", "--print", "/prove theories/Eval.v"]
      lines <$> readFile (home </> T.unpack ("cwd-" <> tag <> ".txt")) `shouldReturn` [proj]
      lines <$> readFile (home </> T.unpack ("ledger-" <> tag <> ".txt")) `shouldReturn` ["1"]
      (_, live) <- getJ app ("/api/sessions/c_" <> TE.encodeUtf8 tag <> "/live")
      str "root" live `shouldBe` Just sid
      str "tag" live `shouldBe` Just tag

  it "marks a run that crashes (exit 2, no end event) crashed, ends its stream, and keeps serving" $
    withLive "{ hdr; ev 1 tool_start '{\"name\":\"terminal\"}'; ev 2 tool_end '{\"name\":\"terminal\"}'; } | sql; sleep 0.5; exit 2" $ \_ proj _ app -> do
      tag <- startRun app proj
      sid <- sessionOf app tag
      (_, evs) <- stream app ("/api/sessions/" <> TE.encodeUtf8 sid <> "/events") []
      ids evs `shouldBe` [1, 2, 3]
      kinds evs `shouldBe` ["tool_start", "tool_end", "end"]
      let (_, _, endData) = last evs
      str "status" endData `shouldBe` Just "crashed"
      _ <- statusBecomes app tag "crashed"
      (ping, _) <- getJ app "/api/ping"
      ping `shouldBe` 200
      -- the made-up end is the end: asked for past it, 204
      (code, _) <- stream app ("/api/sessions/" <> TE.encodeUtf8 sid <> "/events") [("Last-Event-ID", "3")]
      code `shouldBe` 204

  it "stops a run that ignores SIGTERM after the grace, its children too, and takes a message first" $
    withLive "trap '' TERM; { hdr; ev 1 tool_start '{\"name\":\"terminal\"}'; } | sql; sleep 300 & echo $! > \"$GAUSS_HOME/kid.pid\"; wait; wait; wait" $ \home proj _ app -> do
      tag <- startRun app proj
      sid <- sessionOf app tag
      -- a message to the compression child goes to the root's inbox
      (mc, mv) <- postJ app ("/api/sessions/c_" <> TE.encodeUtf8 tag <> "/message") (A.object ["text" A..= ("try induction" :: Text)])
      mc `shouldBe` 200
      str "session" mv `shouldBe` Just sid
      ls <- BC.lines <$> B.readFile (home </> "fisher" </> "inbox" </> T.unpack (sid <> ".jsonl"))
      length ls `shouldBe` 1
      (A.decodeStrict (head ls) >>= str "text") `shouldBe` Just "try induction"
      (ec, _) <- postJ app ("/api/sessions/" <> TE.encodeUtf8 sid <> "/message") (A.object ["text" A..= ("  " :: Text)])
      ec `shouldBe` 400
      kid <- waitPid (home </> "kid.pid")
      t0 <- getCurrentTime
      (sc, _) <- postJ app ("/api/sessions/" <> TE.encodeUtf8 sid <> "/stop") (A.object [])
      sc `shouldBe` 202
      _ <- statusBecomes app tag "stopped"
      t1 <- getCurrentTime
      realToFrac (diffUTCTime t1 t0) `shouldSatisfy` (>= (1 :: Double))
      realToFrac (diffUTCTime t1 t0) `shouldSatisfy` (< (8 :: Double))
      goneSoon kid `shouldReturn` True
      (_, evs) <- stream app ("/api/sessions/" <> TE.encodeUtf8 sid <> "/events") []
      kinds evs `shouldBe` ["tool_start", "end"]
      let (_, _, endData) = last evs
      str "status" endData `shouldBe` Just "stopped"
      -- no live run: a message is refused
      (lc, _) <- postJ app ("/api/sessions/" <> TE.encodeUtf8 sid <> "/message") (A.object ["text" A..= ("late" :: Text)])
      lc `shouldBe` 409

  it "ends a stream with a made-up end after the idle bound when no run of this server drives it, and a reconnect then gets 204" $
    withLive "exit 0" $ \home _ _ app -> do
      c <- open (home </> "state.db")
      mapM_ (execute_ c . Query) schema
      execute_ c "INSERT INTO ui_events VALUES ('I', 1, 0, 'tool_start', '{\"name\":\"t\"}'), ('I', 2, 0, 'tool_end', '{\"name\":\"t\"}')"
      close c
      t0 <- getCurrentTime
      (_, evs) <- stream app "/api/sessions/I/events" []
      t1 <- getCurrentTime
      ids evs `shouldBe` [1, 2, 3]
      kinds evs `shouldBe` ["tool_start", "tool_end", "end"]
      let (_, _, endData) = last evs
      str "status" endData `shouldBe` Just "unknown"
      KM.lookup "made" (case endData of A.Object o -> o; _ -> mempty) `shouldBe` Just (A.Bool True)
      realToFrac (diffUTCTime t1 t0) `shouldSatisfy` (>= (1 :: Double))
      (code, _) <- stream app "/api/sessions/I/events" [("Last-Event-ID", "3")]
      code `shouldBe` 204

  it "waits for an end event written just after the run exits instead of making up a crash" $
    withLive "{ hdr; ev 1 tool_start '{\"name\":\"t\"}'; } | sql; ( sleep 0.2; ev 2 end '{\"status\":\"completed\"}' | sql ) & exit 0" $ \_ proj _ app -> do
      tag <- startRun app proj
      sid <- sessionOf app tag
      (_, evs) <- stream app ("/api/sessions/" <> TE.encodeUtf8 sid <> "/events") []
      kinds evs `shouldBe` ["tool_start", "end"]
      let (_, _, endData) = last evs
      str "status" endData `shouldBe` Just "completed"
      _ <- statusBecomes app tag "completed"
      pure ()

  it "resumes a live run's stream read partway, after its Last-Event-ID, with no gap or duplicate" $
    withLive "{ hdr; for i in 1 2 3 4 5 6; do sleep 0.25; ev $i tool_start '{\"name\":\"t\"}'; done; sleep 0.25; ev 7 end '{\"status\":\"completed\"}'; } | sql; exit 0" $ \_ proj _ app -> do
      tag <- startRun app proj
      sid <- sessionOf app tag
      let path = "/api/sessions/" <> TE.encodeUtf8 sid <> "/events"
      first <- testWithApplication (pure app) $ \port -> readUntil port path 3
      ids first `shouldSatisfy` (\xs -> take 3 xs == [1, 2, 3])
      let lastSeen = last (ids first)
      lastSeen `shouldSatisfy` (< 7)
      (_, rest) <- stream app path [("Last-Event-ID", BC.pack (show lastSeen))]
      ids first <> ids rest `shouldBe` [1 .. 7]
      kinds rest `shouldSatisfy` ((== "end") . last)

  it "stops a run's grandchild that left its process group for a session of its own" $
    withLive "trap '' TERM; { hdr; ev 1 tool_start '{\"name\":\"t\"}'; } | sql; perl -MPOSIX -e 'POSIX::setsid(); open(F, \">$ENV{GAUSS_HOME}/gkid.pid\"); print F $$; close F; exec \"sleep\", \"300\"' & wait; wait" $ \home proj _ app -> do
      tag <- startRun app proj
      sid <- sessionOf app tag
      kid <- waitPid (home </> "gkid.pid")
      (do (sc, _) <- postJ app ("/api/sessions/" <> TE.encodeUtf8 sid <> "/stop") (A.object [])
          sc `shouldBe` 202
          _ <- statusBecomes app tag "stopped"
          goneSoon kid `shouldReturn` True)
        `finally` (try (signalProcess sigKILL (fromIntegral kid)) :: IO (Either SomeException ()))

  it "passes the command as one argument, its spacing kept" $ withLive "exit 0" $ \home proj _ app -> do
    tag <- startWith app proj "  /prove  theories/Eval.v   --model m "
    _ <- statusBecomes app tag "crashed"
    lines <$> readFile (home </> T.unpack ("args-" <> tag <> ".txt")) `shouldReturn` ["launch", "--print", "/prove  theories/Eval.v   --model m"]

  it "refuses a start from a cross-site page (403)" $ withLive "exit 0" $ \_ proj _ app -> do
    r <- call app methodPost "/api/sessions" [("Origin", "http://evil.example"), (hContentType, "application/json")]
           (A.encode (A.object ["project" A..= proj, "command" A..= ("/prove x" :: Text)]))
    statusCode (simpleStatus r) `shouldBe` 403

  it "refuses a start without an existing absolute project or a Fisher command" $ withLive "exit 0" $ \_ proj _ app -> do
    (a, _) <- postJ app "/api/sessions" (A.object ["project" A..= ("rel/dir" :: Text), "command" A..= ("/prove x" :: Text)])
    a `shouldBe` 400
    (b, _) <- postJ app "/api/sessions" (A.object ["project" A..= (proj </> "missing"), "command" A..= ("/prove x" :: Text)])
    b `shouldBe` 400
    (c, _) <- postJ app "/api/sessions" (A.object ["project" A..= proj, "command" A..= ("prove x\n/rm" :: Text)])
    c `shouldBe` 400
    (d, _) <- getJ app "/api/tags/nosuchtag"
    d `shouldBe` 404
  where
    waitPid f = go (100 :: Int)
      where
        go n = do
          here <- doesFileExist f
          s <- if here then readFile f else pure ""
          case reads s of
            [(p, _)] -> pure (p :: Int)
            _ | n > 0 -> threadDelay 50000 >> go (n - 1)
            _ -> fail "no pid"
    goneSoon p = go (60 :: Int)
      where
        go n = alive p >>= \case
          False -> pure True
          True | n > 0 -> threadDelay 50000 >> go (n - 1)
          True -> pure False
