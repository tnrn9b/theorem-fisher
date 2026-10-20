-- | The goal board's routes through the whole server: search runs (a
-- copy of the synthetic runs in test/fixtures/search, and the copied bench
-- g graph and h auto runs), their events as a stream whose cursor is a
-- byte offset, apply under fisher-search's rule, and runs of a fake
-- fisher search that grow their events file, finish and stop.
module Fisher.UI.SearchSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forM_, void)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), diffUTCTime, getCurrentTime)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Network.HTTP.Types
import Network.Wai (Application, Request (..))
import Network.Wai.Test
import System.Directory
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (createSymbolicLink, setFileMode)
import System.Process (readProcess)
import System.Timeout (timeout)
import Test.Hspec

import Fisher.UI.Access (Gate (..))
import Fisher.UI.Api.Live (LiveEnv (..), newLiveEnv, stopRuns)
import Fisher.UI.Api.Search
import Fisher.UI.Api.Templates (newCreateEnv)
import Fisher.UI.Db (newUiEnv)
import Fisher.UI.Server (allApis, serve)
import Fisher.UI.TestDirs (fixturesDir)

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

key :: Text -> A.Value -> A.Value
key k = \case
  A.Object o -> maybe A.Null id (KM.lookup (K.fromText k) o)
  _ -> A.Null

arr :: A.Value -> [A.Value]
arr = \case
  A.Array xs -> V.toList xs
  _ -> []

txt :: A.Value -> Text
txt = \case
  A.String t -> t
  _ -> ""

num :: A.Value -> Double
num = \case
  A.Number n -> toRealFloat n
  _ -> -1

-- | The stream: the (id, data line) of each message, and the end event's
-- data when there is one.
data Stream = Stream { stEvents :: [(Integer, B.ByteString)], stEnd :: Maybe A.Value }
  deriving (Show)

parseStream :: BL.ByteString -> Stream
parseStream body =
  let blocks = splitBlocks (BL.toStrict body)
      one b =
        let ls = BC.lines b
            field f = [B.drop (B.length f) l | l <- ls, f `B.isPrefixOf` l]
         in (field "event: ", field "id: ", field "data: ")
      parsed = map one blocks
   in Stream
        [(read (BC.unpack i), d) | ([], [i], [d]) <- parsed]
        (case [d | (["end"], _, [d]) <- parsed] of
           d : _ -> A.decodeStrict d
           [] -> Nothing)
  where
    splitBlocks s = case B.breakSubstring "\n\n" s of
      (a, rest) | B.null rest -> [a | not (B.null a)]
                | otherwise -> a : splitBlocks (B.drop 2 rest)

stream :: Application -> B.ByteString -> [Header] -> IO (Int, Stream)
stream app p hs = do
  r <- timeout 30000000 (call app methodGet p hs "")
  case r of
    Nothing -> fail ("the stream of " <> BC.unpack p <> " did not end")
    Just x -> pure (statusCode (simpleStatus x), parseStream (simpleBody x))

graphRun, flatRun :: B.ByteString
graphRun = "s.20261016-090000"
flatRun = "s.20261016-080000"

-- | The files the Lean project starts with (the runs' files, holes open).
projectFiles :: [(FilePath, Text)]
projectFiles =
  [ ("lakefile.lean", "import Lake\n")
  , ("Demo/Graph.lean", "namespace Demo\n\ntheorem main_thm : 1 + 1 = 2 := by\n  sorry\n\ntheorem side_thm : 2 + 2 = 4 := by\n  sorry\n\nend Demo\n")
  , ("Demo/Flat.lean", "namespace Demo\n\ntheorem a : True := by\n  sorry\n\nend Demo\n") ]

-- | A search root with a copy of the fixture runs (their project set to a
-- scratch Lean project), the bench fixtures, and the server over them with
-- a fake fisher whose body is the given shell text.
withSearch :: Text -> (FilePath -> FilePath -> SearchEnv -> Application -> IO a) -> IO a
withSearch body k = withSystemTempDirectory "fisher-ui-search" $ \tmp0 -> do
  tmp <- canonicalizePath tmp0
  let root = tmp </> "search"
      proj = tmp </> "proj"
      home = tmp </> "fisher"
      fake = tmp </> "fake-fisher"
  mapM_ (createDirectoryIfMissing True) [root, home, proj </> "Demo"]
  forM_ projectFiles $ \(f, t) -> TIO.writeFile (proj </> f) t >> setModificationTime (proj </> f) old
  _ <- readProcess "cp" ["-R", fixturesDir </> "search" </> ".", root] ""
  forM_ ["20261016-090000", "20261016-080000"] $ \d -> do
    let p = root </> d </> "report.json"
    t <- TIO.readFile p
    TIO.writeFile p (T.replace "\"/fixture/lean\"" (T.pack (show proj)) t)
  TIO.writeFile fake ("#!/bin/sh\nprintf '%s\\n' \"$@\" > \"$FISHER_HOME/search-args.txt\"\npwd > \"$FISHER_HOME/search-cwd.txt\"\n" <> body)
  setFileMode fake 0o755
  ui <- newUiEnv home (fixturesDir </> "bench") (tmp </> "repo")
  le0 <- newLiveEnv ui fake
  let le = le0 {leGrace = 1000000, leIdle = 1500000}
  se <- newSearchEnv le root
  ce <- newCreateEnv le
  r <- k tmp proj se (serve gate (allApis le se ce))
  stopSearches se
  stopRuns le
  pure r

-- | A time well before the fixture runs: a project file not touched since.
old :: UTCTime
old = UTCTime (fromGregorian 2026 10 1) 0

runOf :: Text -> A.Value -> Maybe A.Value
runOf rid v = case [x | x <- arr v, key "run" x == A.String rid] of
  x : _ -> Just x
  [] -> Nothing

-- | Poll the run until its status is the one wanted (up to 10 s).
statusBecomes :: Application -> Text -> Text -> IO A.Value
statusBecomes app rid want = go (200 :: Int)
  where
    go n = do
      (_, v) <- getJ app ("/api/search/" <> TE.encodeUtf8 rid)
      if key "status" v == A.String want
        then pure v
        else if n > 0 then threadDelay 50000 >> go (n - 1) else fail ("run " <> T.unpack rid <> " never became " <> T.unpack want <> ": " <> show (key "status" v))

eventNames :: Stream -> [Text]
eventNames s = mapMaybe (\(_, d) -> A.decodeStrict d >>= \v -> case key "event" v of A.String t -> Just t; _ -> Nothing) (stEvents s)

spec :: Spec
spec = describe "Fisher.UI.Api.Search" $ do
  describe "completeLines" $ do
    it "gives each whole line with the offset after it, and holds back a last line with no newline yet" $
      completeLines 10 "{\"a\":1}\n{\"b\":2}\n{\"c\":" `shouldBe` [(18, "{\"a\":1}"), (26, "{\"b\":2}")]
    it "skips blank lines but moves past them" $
      completeLines 0 "x\n\ny\n" `shouldBe` [(2, "x"), (5, "y")]

  describe "GET /api/search" $
    it "lists the search root's runs and the bench g, h and w runs, with file, mode and counts" $ withSearch "exit 0" $ \_ _ _ app -> do
      (code, v) <- getJ app "/api/search"
      code `shouldBe` 200
      let row rid = maybe (fail ("no run " <> T.unpack rid)) pure (runOf rid v)
      g <- row "s.20261016-090000"
      f <- row "s.20261016-080000"
      bg <- row "g.graph.5"
      bh <- row "h.auto.4"
      (key "mode" g, key "file" g, key "closed" g, key "total" g) `shouldBe` (A.String "graph", A.String "Demo/Graph.lean", A.Number 1, A.Number 2)
      (key "mode" f, key "file" f, key "closed" f, key "total" f) `shouldBe` (A.String "flat", A.String "Demo/Flat.lean", A.Number 2, A.Number 4)
      (key "mode" bg, key "bench" bg, key "file" bg, key "closed" bg) `shouldBe` (A.String "graph", A.String "g", A.String "Step/Relay.lean", A.Number 1)
      (key "mode" bh, key "bench" bh, key "file" bh, key "total" bh) `shouldBe` (A.String "flat", A.String "h", A.String "Step/Graft.lean", A.Number 1)
      key "status" g `shouldBe` A.String "done"
      bw <- row "w.B.4"
      (key "mode" bw, key "bench" bw, key "file" bw, key "closed" bw, key "total" bw) `shouldBe` (A.String "flat", A.String "w", A.String "Step/Latency.lean", A.Number 2, A.Number 3)
      -- every g, h and w run the fixtures have (g flat 8 + 5-unparsed, g graph the same, h 16, w 3)
      length [x | x <- arr v, key "bench" x /= A.Null] `shouldBe` 37
      num (key "started" g) `shouldSatisfy` (> 0)

  describe "GET /api/search (counts)" $ do
    it "needs the events only when the report lacks the counts or elapsed_s" $ do
      let rep = A.object ["goals_closed" A..= (1 :: Int), "goals_total" A..= (2 :: Int), "elapsed_s" A..= (3 :: Double), "mode" A..= ("swarm" :: Text)]
      needsEvents (Just rep) `shouldBe` False
      needsEvents (Just (A.object ["goals_closed" A..= (1 :: Int), "goals_total" A..= (2 :: Int)])) `shouldBe` True
      needsEvents Nothing `shouldBe` True
    it "counts a run without a report from its events, again once they grow" $ withSearch "exit 0" $ \tmp _ _ app -> do
      let dir = tmp </> "search" </> "20261016-110000"
      createDirectoryIfMissing True dir
      B.writeFile (dir </> "events.jsonl") "{\"event\":\"auto closed\",\"goal\":\"Demo.a\",\"seconds\":1,\"snippets\":\"simp\",\"t\":1}\n"
      (_, v1) <- getJ app "/api/search"
      fmap (\x -> (key "closed" x, key "total" x)) (runOf "s.20261016-110000" v1) `shouldBe` Just (A.Number 1, A.Number 1)
      B.appendFile (dir </> "events.jsonl") "{\"event\":\"auto missed\",\"goal\":\"Demo.b\",\"seconds\":1,\"t\":2}\n"
      (_, v2) <- getJ app "/api/search"
      fmap (\x -> (key "closed" x, key "total" x)) (runOf "s.20261016-110000" v2) `shouldBe` Just (A.Number 1, A.Number 2)

  describe "GET /api/search/:run" $ do
    it "gives graph null for a graph.json cut short, and apply says it could not read it" $ withSearch "exit 0" $ \tmp _ _ app -> do
      let gp = tmp </> "search" </> "20261016-090000" </> "graph.json"
      raw <- B.readFile gp
      B.writeFile gp (B.take (B.length raw `div` 2) raw)
      (code, v) <- getJ app ("/api/search/" <> graphRun)
      code `shouldBe` 200
      (key "graph" v, key "mode" v, key "status" v) `shouldBe` (A.Null, A.String "graph", A.String "done")
      txt (key "reason" (key "apply" v)) `shouldSatisfy` ("graph.json" `T.isInfixOf`)
    it "gives a graph run's nodes, splits (a split's lemmas under their parent) and whiteboards" $ withSearch "exit 0" $ \_ _ _ app -> do
      (code, v) <- getJ app ("/api/search/" <> graphRun)
      code `shouldBe` 200
      let gr = key "graph" v
      [(key "id" n, key "name" n, key "status" n) | n <- arr (key "nodes" gr)]
        `shouldBe` [ (A.Number 1, A.String "Demo.main_thm", A.String "open"), (A.Number 2, A.String "Demo.side_thm", A.String "proved")
                   , (A.Number 3, A.String "Demo.main_thm_l1", A.String "proved"), (A.Number 4, A.String "Demo.main_thm_l2", A.String "failed") ]
      [(key "id" s, key "parent" s, key "lemmas" s, key "status" s) | s <- arr (key "splits" gr)]
        `shouldBe` [ (A.Number 1, A.Number 1, A.toJSON [3 :: Int, 4], A.String "accepted")
                   , (A.Number 2, A.Number 1, A.toJSON ([] :: [Int]), A.String "rejected") ]
      wb1 <- TIO.readFile (fixturesDir </> "search" </> "20261016-090000" </> "whiteboard-1-Demo_main_thm.md")
      [(key "root" w, key "file" w, key "text" w) | w <- arr (key "whiteboards" v)]
        `shouldBe` [ (A.Number 1, A.String "whiteboard-1-Demo_main_thm.md", A.String wb1)
                   , (A.Number 2, A.String "whiteboard-2-Demo_side_thm.md", A.String "# Plan for Demo.side_thm\n- Demo.side_thm (proved)") ]
      (key "mode" v, key "status" v, key "auto" v) `shouldBe` (A.String "graph", A.String "done", A.Bool True)
      key "attempts" v `shouldBe` A.toJSON ["1-Demo_main_thm-fisher", "2-Demo_side_thm-fisher" :: Text]
      key "goals_closed" (key "report" v) `shouldBe` A.Number 1
    it "gives the bench g graph run's graph and whiteboard" $ withSearch "exit 0" $ \_ _ _ app -> do
      (_, v) <- getJ app "/api/search/g.graph.5"
      [(key "name" n, key "status" n) | n <- arr (key "nodes" (key "graph" v))] `shouldBe` [(A.String "Step.Relay.fireCand_third", A.String "proved")]
      map (key "text") (arr (key "whiteboards" v)) `shouldBe` [A.String "# Plan for Step.Relay.fireCand_third\n- Step.Relay.fireCand_third (proved)"]
      key "model" (key "report" v) `shouldBe` A.String "example/model-a"
    it "gives a flat run's goals with their status, closedBy and disproof" $ withSearch "exit 0" $ \_ _ _ app -> do
      (code, v) <- getJ app ("/api/search/" <> flatRun)
      code `shouldBe` 200
      key "graph" v `shouldBe` A.Null
      [(key "goal" g, key "status" g, key "closedBy" g) | g <- arr (key "goals" v)]
        `shouldBe` [ (A.String "Demo.a", A.String "closed", A.String "auto"), (A.String "Demo.b", A.String "closed", A.String "model+repair")
                   , (A.String "Demo.c", A.String "disproved", A.Null), (A.String "Demo.d", A.String "gave up", A.Null) ]
      key "verdict" (key "disproof" (arr (key "goals" v) !! 2)) `shouldBe` A.String "false"
      (_, h) <- getJ app "/api/search/h.auto.4"
      [(key "goal" g, key "status" g, key "closedBy" g) | g <- arr (key "goals" h)]
        `shouldBe` [(A.String "Step.Graft.runS_graft", A.String "closed", A.String "model")]
    it "refuses an unknown run (404) and a bad id (400)" $ withSearch "exit 0" $ \_ _ _ app -> do
      fst <$> getJ app "/api/search/s.20990101-000000" `shouldReturn` 404
      fst <$> getJ app "/api/search/g.graph.99" `shouldReturn` 404
      fst <$> getJ app "/api/search/x.y" `shouldReturn` 400
      fst <$> getJ app "/api/search/s..." `shouldReturn` 400

  describe "GET /api/search/:run/events" $ do
    it "streams each line with its end offset as the id, ends with an end event, and resumes after an offset without repeating a line" $
      withSearch "exit 0" $ \tmp _ _ app -> do
        raw <- B.readFile (tmp </> "search" </> "20261016-080000" </> "events.jsonl")
        (code, full) <- stream app ("/api/search/" <> flatRun <> "/events") []
        code `shouldBe` 200
        map snd (stEvents full) `shouldBe` BC.lines raw
        map fst (stEvents full) `shouldBe` scanl1 (+) (map (fromIntegral . (+ 1) . B.length) (BC.lines raw))
        fmap (key "status") (stEnd full) `shouldBe` Just (A.String "done")
        let (_, cut) = stEvents full !! 2
            off = fst (stEvents full !! 2)
        (_, rest) <- stream app ("/api/search/" <> flatRun <> "/events") [("Last-Event-ID", BC.pack (show off))]
        stEvents rest `shouldBe` drop 3 (stEvents full)
        snd (head (stEvents rest)) `shouldNotBe` cut
        (_, viaQuery) <- stream app ("/api/search/" <> flatRun <> "/events?from=" <> BC.pack (show off)) []
        stEvents viaQuery `shouldBe` drop 3 (stEvents full)
        -- past the end of a finished run: 204 so the browser stops
        r <- call app methodGet ("/api/search/" <> flatRun <> "/events") [("Last-Event-ID", BC.pack (show (B.length raw)))] ""
        statusCode (simpleStatus r) `shouldBe` 204
        -- an offset inside a line is refused
        r2 <- call app methodGet ("/api/search/" <> flatRun <> "/events?from=5") [] ""
        statusCode (simpleStatus r2) `shouldBe` 400
    it "does not send a partial last line until its newline is written" $ withSearch "exit 0" $ \tmp _ _ app -> do
      let dir = tmp </> "search" </> "20261016-100000"
          l1 = "{\"event\":\"auto missed\",\"goal\":\"Demo.a\",\"seconds\":1,\"t\":1}"
          l2 = "{\"attempt\":1,\"backend\":\"fisher\",\"event\":\"launched\",\"goal\":\"Demo.a\",\"t\":1}"
          l3a = "{\"attempt\":1,\"event\":\"clo"
          l3b = "sed\",\"goal\":\"Demo.a\",\"t\":9}"
      createDirectoryIfMissing True dir
      B.writeFile (dir </> "events.jsonl") (l1 <> "\n" <> l2 <> "\n" <> l3a)
      _ <- forkIO $ threadDelay 1000000 >> B.appendFile (dir </> "events.jsonl") (l3b <> "\n")
      (_, s) <- stream app "/api/search/s.20261016-100000/events" []
      map snd (stEvents s) `shouldBe` [l1, l2, l3a <> l3b]
      -- no run of this server and no report: it ends after the idle bound
      fmap (key "status") (stEnd s) `shouldBe` Just (A.String "unknown")

    it "ends with rewritten when the file is truncated and written again in place, sending nothing of the new text" $ withSearch "exit 0" $ \tmp _ _ app -> do
      let dir = tmp </> "search" </> "20261016-120000"
          p = dir </> "events.jsonl"
          ls = ["{\"event\":\"auto missed\",\"goal\":\"Demo." <> BC.pack [c] <> "\",\"seconds\":1,\"t\":1}" | c <- "abc"]
      createDirectoryIfMissing True dir
      B.writeFile p (BC.unlines ls)
      _ <- forkIO $ do
        threadDelay 800000
        -- in place: the same inode, longer than before, a last line half written
        B.writeFile p ("{\"event\":\"auto missed\",\"goal\":\"Other.long_name_padded_past_the_old_end_long_name_padded_past_the_old_end_long_name_padded_past_the_old_end_long_name_padded_past_the_old_end_\",\"seconds\":1,\"t\":5}\n{\"event\":\"auto missed\",\"goal\":\"Other.second\",\"seconds\":1,\"t\":6}\n{\"fragm")
      (_, s1) <- stream app "/api/search/s.20261016-120000/events" []
      map snd (stEvents s1) `shouldBe` ls
      fmap (key "status") (stEnd s1) `shouldBe` Just (A.String "rewritten")
    it "ends with rewritten when the file is replaced by another (a new inode)" $ withSearch "exit 0" $ \tmp _ _ app -> do
      let dir = tmp </> "search" </> "20261016-130000"
          p = dir </> "events.jsonl"
          l1 = "{\"event\":\"auto missed\",\"goal\":\"Demo.a\",\"seconds\":1,\"t\":1}"
      createDirectoryIfMissing True dir
      B.writeFile p (l1 <> "\n")
      _ <- forkIO $ do
        threadDelay 800000
        B.writeFile (dir </> "new.jsonl") (l1 <> "\n" <> "{\"event\":\"gave up\",\"goal\":\"Demo.a\",\"t\":2}\n")
        renameFile (dir </> "new.jsonl") p
      (_, s1) <- stream app "/api/search/s.20261016-130000/events" []
      map snd (stEvents s1) `shouldBe` [l1]
      fmap (key "status") (stEnd s1) `shouldBe` Just (A.String "rewritten")
    it "starts a reconnect past the end of a file that was cut short with rewritten" $ withSearch "exit 0" $ \tmp _ _ app -> do
      let dir = tmp </> "search" </> "20261016-140000"
      createDirectoryIfMissing True dir
      B.writeFile (dir </> "events.jsonl") "{\"event\":\"gave up\",\"goal\":\"Demo.a\",\"t\":2}\n"
      (code, s1) <- stream app "/api/search/s.20261016-140000/events" [("Last-Event-ID", "5000")]
      code `shouldBe` 200
      (stEvents s1, fmap (key "status") (stEnd s1)) `shouldBe` ([], Just (A.String "rewritten"))
    it "ends a stream of a run no process of this server writes at once when its events file has been still for the idle bound" $
      withSearch "exit 0" $ \tmp _ _ app -> do
        let dir = tmp </> "search" </> "20261016-150000"
            p = dir </> "events.jsonl"
        createDirectoryIfMissing True dir
        B.writeFile p "{\"event\":\"gave up\",\"goal\":\"Demo.a\",\"t\":2}\n"
        setModificationTime p old
        t0 <- getCurrentTime
        (_, s1) <- stream app "/api/search/s.20261016-150000/events" []
        t1 <- getCurrentTime
        (length (stEvents s1), fmap (key "status") (stEnd s1)) `shouldBe` (1, Just (A.String "unknown"))
        diffUTCTime t1 t0 `shouldSatisfy` (< 1)

  describe "POST /api/search/:run/apply" $ do
    it "refuses a file changed since the run started (409, changed) unless forced" $ withSearch "exit 0" $ \tmp proj _ app -> do
      let target = proj </> "Demo" </> "Flat.lean"
      TIO.writeFile target "namespace Demo\n\ntheorem a : True := by\n  trivial\n\nend Demo\n"
      (code, v) <- postJ app ("/api/search/" <> flatRun <> "/apply") (A.object [])
      code `shouldBe` 409
      key "changed" v `shouldBe` A.Bool True
      TIO.readFile target >>= (`shouldSatisfy` ("trivial" `T.isInfixOf`))
      (_, d) <- getJ app ("/api/search/" <> flatRun)
      (key "allowed" (key "apply" d), key "changed" (key "apply" d)) `shouldBe` (A.Bool False, A.Bool True)
      (code2, _) <- postJ app ("/api/search/" <> flatRun <> "/apply") (A.object ["force" A..= True])
      code2 `shouldBe` 200
      base <- B.readFile (tmp </> "search" </> "20261016-080000" </> "base.lean")
      B.readFile target `shouldReturn` base
    it "refuses a file of this server's run whose text differs from the one it started on, even with an old time, unless forced" $
      withSearch finishing $ \tmp proj _ app -> do
        let target = proj </> "Demo" </> "Flat.lean"
        (_, v) <- postJ app "/api/search" (A.object ["project" A..= proj, "file" A..= ("Demo/Flat.lean" :: Text), "graph" A..= False, "auto" A..= True])
        let rid = txt (key "run" v)
        _ <- statusBecomes app rid "done"
        TIO.writeFile target "edited\n"
        setModificationTime target old
        (code, e) <- postJ app ("/api/search/" <> TE.encodeUtf8 rid <> "/apply") (A.object [])
        (code, key "changed" e) `shouldBe` (409, A.Bool True)
        TIO.readFile target `shouldReturn` "edited\n"
        fst <$> postJ app ("/api/search/" <> TE.encodeUtf8 rid <> "/apply") (A.object ["force" A..= True]) `shouldReturn` 200
        TIO.readFile target `shouldReturn` "merged\n"
        void (pure tmp)
    it "refuses a graph run whose root is left open with a lemma from its split still sorry, the file unchanged" $
      withSearch "exit 0" $ \_ proj _ app -> do
        before <- B.readFile (proj </> "Demo" </> "Graph.lean")
        (code, v) <- postJ app ("/api/search/" <> graphRun <> "/apply") (A.object [])
        code `shouldBe` 409
        txt (key "error" v) `shouldSatisfy` ("Demo.main_thm_l2" `T.isInfixOf`)
        txt (key "error" v) `shouldSatisfy` ("sorry" `T.isInfixOf`)
        B.readFile (proj </> "Demo" </> "Graph.lean") `shouldReturn` before
        (_, d) <- getJ app ("/api/search/" <> graphRun)
        key "allowed" (key "apply" d) `shouldBe` A.Bool False
    it "writes a flat run's merged file over its file in the project, by a rename" $ withSearch "exit 0" $ \tmp proj _ app -> do
      base <- B.readFile (tmp </> "search" </> "20261016-080000" </> "base.lean")
      (_, d) <- getJ app ("/api/search/" <> flatRun)
      key "allowed" (key "apply" d) `shouldBe` A.Bool True
      (code, v) <- postJ app ("/api/search/" <> flatRun <> "/apply") (A.object [])
      code `shouldBe` 200
      key "written" v `shouldBe` A.String (T.pack (proj </> "Demo" </> "Flat.lean"))
      B.readFile (proj </> "Demo" </> "Flat.lean") `shouldReturn` base
      sort <$> listDirectory (proj </> "Demo") `shouldReturn` ["Flat.lean", "Graph.lean"]
    it "writes a graph run's merged file once no lemma from a split is left sorry" $ withSearch "exit 0" $ \tmp proj _ app -> do
      let b = tmp </> "search" </> "20261016-090000" </> "base.lean"
      t <- TIO.readFile b
      TIO.writeFile b (T.replace "theorem main_thm_l2 : 0 = 1 := by\n  sorry" "theorem main_thm_l2 : 0 = 1 := by\n  decide" t)
      (code, _) <- postJ app ("/api/search/" <> graphRun <> "/apply") (A.object [])
      code `shouldBe` 200
      B.readFile (proj </> "Demo" </> "Graph.lean") >>= (`shouldSatisfy` ("decide" `B.isInfixOf`))
    it "refuses a file that is a link, and any bench run (its workspace is the bench's)" $ withSearch "exit 0" $ \tmp proj _ app -> do
      let target = proj </> "Demo" </> "Flat.lean"
      renameFile target (tmp </> "Flat.lean")
      createSymbolicLink (tmp </> "Flat.lean") target
      (code, _) <- postJ app ("/api/search/" <> flatRun <> "/apply") (A.object [])
      code `shouldBe` 409
      B.readFile (tmp </> "Flat.lean") `shouldReturn` TE.encodeUtf8 (snd (projectFiles !! 2))
      fst <$> postJ app "/api/search/h.auto.4/apply" (A.object []) `shouldReturn` 409
    it "refuses a cross-site page (403)" $ withSearch "exit 0" $ \_ proj _ app -> do
      r <- call app methodPost ("/api/search/" <> flatRun <> "/apply") [("Origin", "http://evil.example"), (hContentType, "application/json")] "{}"
      statusCode (simpleStatus r) `shouldBe` 403
      TIO.readFile (proj </> "Demo" </> "Flat.lean") `shouldReturn` snd (projectFiles !! 2)

  describe "POST /api/search" $ do
    it "runs fisher search FILE --graph --no-auto --out DIR in the project, streams its growing events, and is done once it writes its report" $
      withSearch growing $ \tmp proj se app -> do
        (code, v) <- postJ app "/api/search" (A.object ["project" A..= proj, "file" A..= ("Demo/Graph.lean" :: Text), "graph" A..= True, "auto" A..= False])
        code `shouldBe` 200
        let rid = txt (key "run" v)
        rid `shouldSatisfy` ("s." `T.isPrefixOf`)
        let out = seRoot se </> T.unpack (T.drop 2 rid)
        doesDirectoryExist out `shouldReturn` True
        waitFile (tmp </> "fisher" </> "search-args.txt")
        lines <$> readFile (tmp </> "fisher" </> "search-args.txt") `shouldReturn` ["search", "Demo/Graph.lean", "--graph", "--no-auto", "--out", out]
        readFile (tmp </> "fisher" </> "search-cwd.txt") `shouldReturn` (proj <> "\n")
        (_, l) <- getJ app "/api/search"
        fmap (key "status") (runOf rid l) `shouldBe` Just (A.String "running")
        (_, s) <- stream app ("/api/search/" <> TE.encodeUtf8 rid <> "/events") []
        eventNames s `shouldBe` ["node added", "launched", "finished", "node proved"]
        fmap (key "status") (stEnd s) `shouldBe` Just (A.String "done")
        d <- statusBecomes app rid "done"
        (key "running" d, key "goals_closed" (key "report" d)) `shouldBe` (A.Bool False, A.Number 1)
    it "passes no --graph and no --no-auto for a flat run with the automation, and a retry (the same file again) is a new run" $
      withSearch "exit 0" $ \tmp proj _ app -> do
        (_, v1) <- postJ app "/api/search" (A.object ["project" A..= proj, "file" A..= (proj </> "Demo" </> "Flat.lean"), "graph" A..= False, "auto" A..= True])
        waitFile (tmp </> "fisher" </> "search-args.txt")
        (_, v2) <- postJ app "/api/search" (A.object ["project" A..= proj, "file" A..= ("Demo/Flat.lean" :: Text), "graph" A..= False, "auto" A..= True])
        key "run" v1 `shouldNotBe` key "run" v2
        threadDelay 300000
        args <- lines <$> readFile (tmp </> "fisher" </> "search-args.txt")
        take 3 args `shouldBe` ["search", "Demo/Flat.lean", "--out"]
        length args `shouldBe` 4
        -- the fake exits 0 with no report: crashed
        void (statusBecomes app (txt (key "run" v2)) "crashed")
    it "passes --no-retrieval and --no-library when the page turns them off, and the run's detail says so" $
      withSearch "sleep 1" $ \tmp proj _ app -> do
        (code, v) <- postJ app "/api/search" (A.object ["project" A..= proj, "file" A..= ("Demo/Flat.lean" :: Text), "graph" A..= False, "auto" A..= True, "retrieval" A..= False, "library" A..= False])
        code `shouldBe` 200
        waitFile (tmp </> "fisher" </> "search-args.txt")
        args <- lines <$> readFile (tmp </> "fisher" </> "search-args.txt")
        take 4 args `shouldBe` ["search", "Demo/Flat.lean", "--no-retrieval", "--no-library"]
        (_, d) <- getJ app ("/api/search/" <> TE.encodeUtf8 (txt (key "run" v)))
        (key "retrieval" d, key "library" d) `shouldBe` (A.Bool False, A.Bool False)
    it "reads a run's retrieval and library from its events: facts events for retrieval, library events for the library" $ withSearch "exit 0" $ \tmp _ _ app -> do
      (_, b) <- getJ app "/api/search/w.B.4"
      (key "retrieval" b, key "library" b) `shouldBe` (A.Bool True, A.Bool False)
      (_, c) <- getJ app "/api/search/w.C.4"
      (key "retrieval" c, key "library" c) `shouldBe` (A.Bool True, A.Bool True)
      -- an index built for the library alone is not retrieval
      let d = tmp </> "search" </> "20261016-100000"
      createDirectoryIfMissing True d
      B.writeFile (d </> "events.jsonl") "{\"event\":\"index built\",\"t\":0.5,\"files\":4,\"declarations\":80,\"seconds\":1}\n{\"event\":\"library offered\",\"t\":1,\"goal\":\"Demo.a\",\"lemmas\":[],\"notes\":0}\n"
      (_, l) <- getJ app "/api/search/s.20261016-100000"
      (key "retrieval" l, key "library" l) `shouldBe` (A.Bool False, A.Bool True)
    it "stops a run (its process tree) and says stopped" $ withSearch "trap '' TERM; sleep 300 & wait" $ \tmp proj _ app -> do
      (_, v) <- postJ app "/api/search" (A.object ["project" A..= proj, "file" A..= ("Demo/Flat.lean" :: Text), "graph" A..= False, "auto" A..= True])
      let rid = txt (key "run" v)
      waitFile (tmp </> "fisher" </> "search-args.txt")
      (code, _) <- postJ app ("/api/search/" <> TE.encodeUtf8 rid <> "/stop") (A.object [])
      code `shouldBe` 202
      _ <- statusBecomes app rid "stopped"
      (_, s) <- stream app ("/api/search/" <> TE.encodeUtf8 rid <> "/events") []
      fmap (key "status") (stEnd s) `shouldBe` Just (A.String "stopped")
      fst <$> postJ app ("/api/search/" <> TE.encodeUtf8 rid <> "/stop") (A.object []) `shouldReturn` 409
      fst <$> postJ app ("/api/search/" <> flatRun <> "/stop") (A.object []) `shouldReturn` 409
    it "refuses a file with no lakefile above it (400 naming the lakefile)" $ withSearch "exit 0" $ \tmp _ _ app -> do
      createDirectoryIfMissing True (tmp </> "bare" </> "Demo")
      writeFile (tmp </> "bare" </> "Demo" </> "X.lean") "theorem x : True := trivial\n"
      (code, v) <- postJ app "/api/search" (A.object ["project" A..= (tmp </> "bare"), "file" A..= ("Demo/X.lean" :: Text), "graph" A..= False, "auto" A..= True])
      code `shouldBe` 400
      txt (key "error" v) `shouldSatisfy` ("lakefile" `T.isInfixOf`)
      doesFileExist (tmp </> "fisher" </> "search-args.txt") `shouldReturn` False
    it "refuses a relative project, a file outside the project or missing, and a file that is not Lean" $ withSearch "exit 0" $ \tmp proj _ app -> do
      writeFile (tmp </> "Out.lean") "theorem x : True := trivial\n"
      writeFile (proj </> "notes.txt") "x\n"
      let try1 p f = fst <$> postJ app "/api/search" (A.object ["project" A..= p, "file" A..= f, "graph" A..= False, "auto" A..= True])
      try1 ("proj" :: Text) ("Demo/Flat.lean" :: Text) `shouldReturn` 400
      try1 (T.pack proj) (T.pack (tmp </> "Out.lean")) `shouldReturn` 400
      try1 (T.pack proj) ("../Out.lean" :: Text) `shouldReturn` 400
      try1 (T.pack proj) ("Demo/None.lean" :: Text) `shouldReturn` 400
      try1 (T.pack proj) ("notes.txt" :: Text) `shouldReturn` 400
      doesFileExist (tmp </> "fisher" </> "search-args.txt") `shouldReturn` False
  where
    waitFile p = do
      ok <- timeout 10000000 (let go = doesFileExist p >>= \y -> if y then threadDelay 100000 else threadDelay 50000 >> go in go)
      maybe (fail ("no " <> p)) pure ok
    -- a flat run that writes a report and a merged file, then exits
    finishing =
      T.unlines
        [ "while [ \"$1\" != \"--out\" ]; do shift; done; out=\"$2\"; mkdir -p \"$out\""
        , "printf '%s\\n' '{\"event\":\"auto closed\",\"goal\":\"Demo.a\",\"seconds\":1,\"snippets\":\"simp\",\"t\":1}' > \"$out/events.jsonl\""
        , "printf 'merged\\n' > \"$out/base.lean\""
        , "printf '{\"mode\":\"swarm\",\"file\":\"Demo/Flat.lean\",\"project\":\"%s\",\"elapsed_s\":1,\"goals_total\":1,\"goals_closed\":1,\"goals\":[],\"attempts\":[]}\\n' \"$(pwd)\" > \"$out/report.json\"" ]
    -- a graph run that writes its events a line at a time, the last in two
    -- pieces, then its report
    growing =
      T.unlines
        [ "while [ \"$1\" != \"--out\" ]; do shift; done; out=\"$2\"; mkdir -p \"$out\"; e=\"$out/events.jsonl\""
        , "printf '%s\\n' '{\"depth\":0,\"event\":\"node added\",\"name\":\"Demo.main_thm\",\"node\":1,\"t\":0}' >> \"$e\"; sleep 0.3"
        , "printf '%s\\n' '{\"attempt\":1,\"event\":\"launched\",\"node\":1,\"t\":0}' >> \"$e\"; sleep 0.3"
        , "printf '%s' '{\"attempt\":1,\"event\":\"finished\",' >> \"$e\"; sleep 0.5"
        , "printf '%s\\n' '\"node\":1,\"outcome\":\"closed\",\"t\":2}' >> \"$e\"; sleep 0.3"
        , "printf '%s\\n' '{\"event\":\"node proved\",\"node\":1,\"t\":2}' >> \"$e\"; sleep 0.2"
        , "printf '%s\\n' '{\"mode\":\"graph\",\"file\":\"Demo/Graph.lean\",\"goals_total\":1,\"goals_closed\":1,\"tree\":[],\"attempts\":[]}' > \"$out/report.json\"" ]
