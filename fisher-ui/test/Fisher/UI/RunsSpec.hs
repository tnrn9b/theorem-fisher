module Fisher.UI.RunsSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (forM_)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.IORef (readIORef)
import Data.List (sort)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Signals (nullSignal, signalProcess)
import Test.Hspec

import Fisher.UI.Api.Runs (stopGates)
import Fisher.UI.Db
import Fisher.UI.Fixture (getJson)
import Fisher.UI.TestDirs (fixturesDir)

key :: T.Text -> A.Value -> A.Value
key k = \case
  A.Object o -> maybe A.Null id (KM.lookup (K.fromText k) o)
  _ -> A.Null

arr :: A.Value -> [A.Value]
arr = \case
  A.Array xs -> V.toList xs
  _ -> []

num :: A.Value -> Double
num = \case
  A.Number n -> realToFrac n
  _ -> -1

bench :: FilePath
bench = fixturesDir </> "bench"

-- | report.py's numbers for the copied runs (tools/bench/h/report.py
-- goal_dollars; g/report.py goal_dollars): arm, n, goal, dollars, turns.
hExpected :: [(T.Text, Int, T.Text, Double, Int)]
hExpected =
  [ ("noauto", 1, "Step.Diamond.fireDiamond_of'", 0.8983304999999999, 18)
  , ("noauto", 2, "Step.Graft.spliced_run", 1.0884246, 28)
  , ("noauto", 3, "Step.Graft.runBody_subst", 0.8278810000000001, 29)
  , ("noauto", 4, "Step.Graft.runS_graft", 1.2393723999999997, 28)
  , ("noauto", 5, "Step.Diamond.twoFire_joint", 1.1011251000000002, 25)
  , ("noauto", 6, "Step.Cone.rel_kept", 0.8933558000000001, 21)
  , ("noauto", 7, "Step.SymExpr.evalPure_typed", 0.4378116, 15)
  , ("noauto", 8, "Step.Frame.runBody_split", 0.4892814, 16)
  , ("auto", 1, "Step.Diamond.fireDiamond_of'", 0.8827010000000001, 19)
  , ("auto", 2, "Step.Graft.spliced_run", 1.061965, 25)
  , ("auto", 3, "Step.Graft.runBody_subst", 0.8108757999999999, 28)
  , ("auto", 4, "Step.Graft.runS_graft", 1.1700381000000002, 28)
  , ("auto", 5, "Step.Diamond.twoFire_joint", 1.0260106, 22)
  , ("auto", 6, "Step.Cone.rel_kept", 0.9525895999999999, 22)
  , ("auto", 7, "Step.SymExpr.evalPure_typed", 0.4440062, 16)
  , ("auto", 8, "Step.Frame.runBody_split", 0.4222138, 15) ]

gExpected :: [(T.Text, T.Text, Double)]
gExpected =
  [ ("flat", "Step.Summary.machineClose_of_bodyClose", 0.5520478999999909)
  , ("flat", "Channels.ObfsSummary.good_eq", 1.0728743999999892)
  , ("graph", "Step.Summary.machineClose_of_bodyClose", 0.546283399999993)
  , ("graph", "Step.Relay.Setup.graftCtx", 0.35534890000002406) ]

goalsOf :: A.Value -> T.Text -> [A.Value]
goalsOf v a = [g | g <- arr (key "goals" v), key "arm" g == A.String a]

-- | Copy a fixture bench dir (files two levels down) to a scratch root.
copyBench :: FilePath -> FilePath -> IO ()
copyBench from to = do
  arms <- listDirectory from
  forM_ arms $ \a -> do
    ns <- listDirectory (from </> a)
    forM_ ns $ \n -> do
      createDirectoryIfMissing True (to </> a </> n)
      fs <- listDirectory (from </> a </> n)
      forM_ fs $ \f -> B.readFile (from </> a </> n </> f) >>= B.writeFile (to </> a </> n </> f)

-- | A stub report.py that records its pid in @<bench>/h/starts@ and
-- prints @gate N@ (N the runs so far) once @<bench>/h/release@ exists
-- (it gives up after a minute, so a stub left by a failed test ends).
blockingStub :: B.ByteString
blockingStub =
  "import os, sys, time\n\
  \top = os.path.dirname(sys.argv[2])\n\
  \with open(os.path.join(top, 'starts'), 'a') as f: f.write('%d\\n' % os.getpid())\n\
  \n = len(open(os.path.join(top, 'starts')).read().split())\n\
  \end = time.time() + 60\n\
  \while not os.path.exists(os.path.join(top, 'release')):\n\
  \    if time.time() > end: sys.exit(2)\n\
  \    time.sleep(0.05)\n\
  \print('gate %d' % n)\n"

-- | A scratch bench root with a copy of the h runs and a repo whose
-- tools/bench/h/report.py is the given script; the gates are stopped after.
withGate :: B.ByteString -> (FilePath -> UiEnv -> IO a) -> IO a
withGate script k = withSystemTempDirectory "fisher-ui" $ \tmp -> do
  copyBench (bench </> "h" </> "runs") (tmp </> "h" </> "runs")
  let repo = tmp </> "repo"
  createDirectoryIfMissing True (repo </> "tools" </> "bench" </> "h")
  B.writeFile (repo </> "tools" </> "bench" </> "h" </> "report.py") script
  bracket (newUiEnv "/nonexistent" tmp repo) stopGates (k tmp)

starts :: FilePath -> IO [Int]
starts tmp = do
  let f = tmp </> "h" </> "starts"
  here <- doesFileExist f
  if here then map read . lines . BC.unpack <$> B.readFile f else pure []

alive :: Int -> IO Bool
alive pid = either (\(_ :: SomeException) -> False) (const True) <$> try (signalProcess nullSignal (fromIntegral pid))

statusOf :: UiEnv -> (A.Value -> Bool) -> IO Bool
statusOf env ok = ok . key "gateStatus" . snd <$> getJson env "/api/runs/h"

-- | Poll for up to five seconds.
waitFor :: IO Bool -> IO Bool
waitFor p = go (50 :: Int)
  where
    go 0 = p
    go n = p >>= \yes -> if yes then pure True else threadDelay 100000 >> go (n - 1)

spec :: Spec
spec = do
  describe "GET /api/runs" $ do
    it "lists each bench's arms with their root" $ do
      env <- newUiEnv "/nonexistent" bench "/nonexistent"
      (code, v) <- getJson env "/api/runs"
      code `shouldBe` 200
      [(key "bench" x, key "arm" x) | x <- arr v]
        `shouldBe` [(A.String b, A.String a) | (b, a) <- [("g", "flat"), ("g", "graph"), ("h", "auto"), ("h", "noauto"), ("w", "A"), ("w", "B"), ("w", "C")]]
      key "root" (head (arr v)) `shouldBe` A.String (T.pack (bench </> "g" </> "runs" </> "flat"))
    it "a missing bench root gives an empty list" $ withSystemTempDirectory "fisher-ui" $ \tmp -> do
      env <- newUiEnv tmp (tmp </> "bench") tmp
      snd <$> getJson env "/api/runs" `shouldReturn` A.Array mempty
      (code, v) <- getJson env "/api/runs/h"
      code `shouldBe` 200
      v `shouldBe` A.object ["bench" A..= ("h" :: T.Text), "arms" A..= ([] :: [T.Text]), "goals" A..= ([] :: [A.Value]), "gate" A..= A.Null, "gateStatus" A..= A.Null]
    it "an unknown bench is 404" $ do
      env <- newUiEnv "/nonexistent" bench "/nonexistent"
      fst <$> getJson env "/api/runs/zz" `shouldReturn` 404

  describe "GET /api/runs/h" $ do
    it "gives 8 goals per arm with closedBy, turns and dollars as max(credits, fisher)" $ do
      env <- newUiEnv "/nonexistent" bench "/nonexistent"
      (code, v) <- getJson env "/api/runs/h"
      code `shouldBe` 200
      key "bench" v `shouldBe` A.String "h"
      sort (arr (key "arms" v)) `shouldBe` map A.String ["auto", "noauto"]
      length (goalsOf v "auto") `shouldBe` 8
      length (goalsOf v "noauto") `shouldBe` 8
      forM_ hExpected $ \(arm, n, goal, dollars, turns) -> do
        let g = goalsOf v arm !! (n - 1)
        key "goal" g `shouldBe` A.String goal
        key "solved" g `shouldBe` A.Bool True
        key "turns" g `shouldBe` A.Number (fromIntegral turns)
        num (key "dollars" g) `shouldSatisfy` (\d -> abs (d - dollars) < 1e-9)
        key "closedBy" g `shouldBe` (if arm == "auto" then A.String "model" else A.Null)
      key "gate" v `shouldBe` A.Null
    it "takes the credits when they are the larger count" $ withSystemTempDirectory "fisher-ui" $ \tmp -> do
      let runs = tmp </> "h" </> "runs"
      copyBench (bench </> "h" </> "runs") runs
      B.writeFile (runs </> "auto" </> "1" </> "spend.json") "{\"spans\": [{\"before\": 10.0, \"after\": 15.0}, {\"before\": 20.0, \"after\": null}]}"
      env <- newUiEnv "/nonexistent" tmp "/nonexistent"
      (_, v) <- getJson env "/api/runs/h"
      num (key "dollars" (head (goalsOf v "auto"))) `shouldSatisfy` (\d -> abs (d - 5) < 1e-9)
      num (key "dollars" (goalsOf v "auto" !! 1)) `shouldSatisfy` (\d -> abs (d - 1.061965) < 1e-9)
    it "shows the last line report.py prints as the gate" $ withSystemTempDirectory "fisher-ui" $ \tmp -> do
      let repo = tmp </> "repo"
      createDirectoryIfMissing True (repo </> "tools" </> "bench" </> "h")
      B.writeFile (repo </> "tools" </> "bench" </> "h" </> "report.py") $
        "import sys\n\
        \assert sys.argv[1] == '--runs' and sys.argv[2].endswith('/h/runs'), sys.argv\n\
        \print('# H')\nprint('')\nprint('auto closes more, or as many for at least 25% fewer dollars: PASS')\nprint('')\n"
      env <- newUiEnv "/nonexistent" bench repo
      let poll :: Int -> IO A.Value
          poll 0 = pure A.Null
          poll k = do
            (_, v) <- getJson env "/api/runs/h"
            if key "gate" v /= A.Null then pure (key "gate" v) else threadDelay 100000 >> poll (k - 1)
      poll 100 `shouldReturn` A.String "auto closes more, or as many for at least 25% fewer dollars: PASS"
      (_, v) <- getJson env "/api/runs/h"
      key "gateStatus" v `shouldBe` A.String "ok"
      stopGates env
    it "a report.py that exits nonzero is failed, not pending" $ withGate "import sys\nsys.exit(1)\n" $ \_ env -> do
      (_, v0) <- getJson env "/api/runs/h"
      key "gateStatus" v0 `shouldBe` A.String "pending"
      waitFor (statusOf env (/= A.String "pending")) `shouldReturn` True
      (_, v) <- getJson env "/api/runs/h"
      key "gate" v `shouldBe` A.Null
      key "gateStatus" v `shouldBe` A.String "failed"
    it "runs one report.py per bench: inputs changed during a run start no second child, it reruns once after" $
      withGate blockingStub $ \tmp env -> do
        (_, v0) <- getJson env "/api/runs/h"
        key "gateStatus" v0 `shouldBe` A.String "pending"
        waitFor ((== 1) . length <$> starts tmp) `shouldReturn` True
        B.writeFile (tmp </> "h" </> "runs" </> "auto" </> "1" </> "events-more.jsonl") ""
        forM_ [1 :: Int, 2, 3] $ \_ -> do
          (_, v) <- getJson env "/api/runs/h"
          key "gate" v `shouldBe` A.Null
          key "gateStatus" v `shouldBe` A.String "pending"
          threadDelay 200000
        length <$> starts tmp `shouldReturn` 1
        B.writeFile (tmp </> "h" </> "release") ""
        waitFor (statusOf env (== A.String "ok")) `shouldReturn` True
        (_, v) <- getJson env "/api/runs/h"
        key "gate" v `shouldBe` A.String "gate 2"
        length <$> starts tmp `shouldReturn` 2
    it "does not start report.py while the bench runs (a run with events and no report), and starts it once the run is done" $
      withGate blockingStub $ \tmp env -> do
        let run9 = tmp </> "h" </> "runs" </> "auto" </> "9"
        createDirectoryIfMissing True run9
        B.writeFile (run9 </> "events.jsonl") "{\"event\":\"launched\",\"goal\":\"G\",\"t\":1}\n"
        forM_ [1 :: Int, 2, 3] $ \_ -> do
          (_, v) <- getJson env "/api/runs/h"
          (key "gate" v, key "gateStatus" v) `shouldBe` (A.Null, A.String "bench in progress")
          threadDelay 200000
        starts tmp `shouldReturn` []
        B.writeFile (run9 </> "report.json") "{\"goals\": []}"
        (_, v) <- getJson env "/api/runs/h"
        key "gateStatus" v `shouldBe` A.String "pending"
        waitFor ((== 1) . length <$> starts tmp) `shouldReturn` True
        B.writeFile (tmp </> "h" </> "release") ""
        waitFor (statusOf env (== A.String "ok")) `shouldReturn` True
    it "does not run a stale gate again when the bench started running meanwhile" $
      withGate blockingStub $ \tmp env -> do
        _ <- getJson env "/api/runs/h"
        waitFor ((== 1) . length <$> starts tmp) `shouldReturn` True
        let run9 = tmp </> "h" </> "runs" </> "auto" </> "9"
        createDirectoryIfMissing True run9
        B.writeFile (run9 </> "events.jsonl") "{\"event\":\"launched\",\"goal\":\"G\",\"t\":1}\n"
        (_, v) <- getJson env "/api/runs/h"
        key "gateStatus" v `shouldBe` A.String "pending"
        B.writeFile (tmp </> "h" </> "release") ""
        waitFor (statusOf env (== A.String "bench in progress")) `shouldReturn` True
        threadDelay 500000
        length <$> starts tmp `shouldReturn` 1
    it "does not chain stale reruns when no request asked for the gate in the quiet time" $
      withGate blockingStub $ \tmp env0 -> do
        let env = env0 {ueGateQuiet = 0.3}
            gateRun = M.lookup "h" <$> readIORef (ueGates env)
        _ <- getJson env "/api/runs/h"
        waitFor ((== 1) . length <$> starts tmp) `shouldReturn` True
        B.writeFile (tmp </> "h" </> "runs" </> "auto" </> "1" </> "events-more.jsonl") ""
        (_, v) <- getJson env "/api/runs/h"
        key "gateStatus" v `shouldBe` A.String "pending"
        -- nobody asks again: the stale run ends with no second one
        threadDelay 800000
        B.writeFile (tmp </> "h" </> "release") ""
        waitFor (maybe False (not . gRunning) <$> gateRun) `shouldReturn` True
        threadDelay 500000
        length <$> starts tmp `shouldReturn` 1
        fmap gStatus <$> gateRun `shouldReturn` Just (GateOk "gate 1")
        -- the next request finds the inputs changed and starts a fresh run
        (_, v2) <- getJson env "/api/runs/h"
        key "gateStatus" v2 `shouldBe` A.String "pending"
        waitFor ((== 2) . length <$> starts tmp) `shouldReturn` True
    it "stopping the server kills a running report.py" $ withGate blockingStub $ \tmp env -> do
      _ <- getJson env "/api/runs/h"
      waitFor ((== 1) . length <$> starts tmp) `shouldReturn` True
      [pid] <- starts tmp
      alive pid `shouldReturn` True
      stopGates env
      waitFor (not <$> alive pid) `shouldReturn` True
    it "kills report.py when it runs past the timeout, and the gate is failed" $ withGate blockingStub $ \tmp env0 -> do
      let env = env0 {ueGateTimeout = 500000}
      _ <- getJson env "/api/runs/h"
      waitFor ((== 1) . length <$> starts tmp) `shouldReturn` True
      [pid] <- starts tmp
      waitFor (statusOf env (== A.String "failed")) `shouldReturn` True
      waitFor (not <$> alive pid) `shouldReturn` True

  describe "GET /api/runs/w" $
    it "gives a row per goal with its retrieval, and a row per file with closed, dollars as bench h's and the report's retrieval totals" $ do
      env <- newUiEnv "/nonexistent" bench "/nonexistent"
      (code, v) <- getJson env "/api/runs/w"
      code `shouldBe` 200
      sort (arr (key "arms" v)) `shouldBe` map A.String ["A", "B", "C"]
      [(key "goal" g, key "solved" g) | g <- goalsOf v "B"]
        `shouldBe` [(A.String "Step.Latency.a", A.Bool True), (A.String "Step.Latency.b", A.Bool True), (A.String "Step.Latency.c", A.Bool False)]
      -- B ran with --no-library: facts, nothing offered or used
      [(key "facts" g, key "offered" g, key "used" g) | g <- goalsOf v "B"]
        `shouldBe` [(A.Number 20, A.Number 0, A.Number 0), (A.Number 12, A.Number 0, A.Number 0), (A.Number 8, A.Number 0, A.Number 0)]
      -- C with the library too
      [(key "facts" g, key "offered" g, key "used" g) | g <- goalsOf v "C"]
        `shouldBe` [(A.Number 20, A.Number 2, A.Number 1), (A.Number 12, A.Number 1, A.Number 0), (A.Number 8, A.Number 0, A.Number 0)]
      -- a report without retrieval has no retrieval fields
      [key "facts" g | g <- goalsOf v "A"] `shouldBe` replicate 3 A.Null
      let files = arr (key "files" v)
          fileOf a = head [f | f <- files, key "arm" f == A.String a]
      map (\f -> (key "arm" f, key "run" f, key "file" f, key "closed" f, key "total" f)) files
        `shouldBe` [ (A.String "A", A.String "4", A.String "Step/Latency.lean", A.Number 1, A.Number 3)
                   , (A.String "B", A.String "4", A.String "Step/Latency.lean", A.Number 2, A.Number 3)
                   , (A.String "C", A.String "4", A.String "Step/Latency.lean", A.Number 2, A.Number 3) ]
      -- the larger of the credits (2.0) and fisher's count (1.25); then of 0.5 and 0.75
      num (key "dollars" (fileOf "A")) `shouldSatisfy` (\d -> abs (d - 2) < 1e-9)
      num (key "dollars" (fileOf "B")) `shouldSatisfy` (\d -> abs (d - 0.75) < 1e-9)
      (key "factsMean" (fileOf "B"), key "offered" (fileOf "B"), key "used" (fileOf "B")) `shouldBe` (A.Number 13.3, A.Number 0, A.Number 0)
      (key "factsMean" (fileOf "C"), key "offered" (fileOf "C"), key "used" (fileOf "C")) `shouldBe` (A.Number 13.3, A.Number 3, A.Number 1)
      (key "factsMean" (fileOf "A"), key "offered" (fileOf "A")) `shouldBe` (A.Null, A.Null)

  describe "GET /api/runs/g" $
    it "gives the flat and graph arms, dollars from the credits spans" $ do
      env <- newUiEnv "/nonexistent" bench "/nonexistent"
      (_, v) <- getJson env "/api/runs/g"
      length (goalsOf v "flat") `shouldBe` 8
      length (goalsOf v "graph") `shouldBe` 8
      forM_ gExpected $ \(arm, goal, dollars) -> do
        let gs = [g | g <- goalsOf v arm, key "goal" g == A.String goal]
        map (\g -> abs (num (key "dollars" g) - dollars) < 1e-9) gs `shouldBe` [True]
      map (key "solved") (goalsOf v "graph") `shouldBe` replicate 8 (A.Bool True)
