module Fisher.UI.SessionsSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (bracket, try)
import Control.Monad (forM_)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.IORef
import Data.List (sort)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Database.SQLite.Simple
import GHC.Stats (RTSStats (..), getRTSStats, getRTSStatsEnabled)
import System.Directory (canonicalizePath, getFileSize, listDirectory)
import System.Mem (performMajorGC)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Timeout (timeout)
import Test.Hspec

import Fisher.UI.Api.Sessions (SessionInfo (..), listSessions)
import Fisher.UI.Db
import Fisher.UI.Fixture

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

near :: Double -> Double -> Bool
near a b = abs (a - b) < 1e-9

byId :: T.Text -> A.Value -> A.Value
byId i v = case [x | x <- arr v, key "id" x == A.String i] of
  (x : _) -> x
  [] -> A.Null

spec :: Spec
spec = do
  describe "GET /api/sessions" $ do
    it "lists the three sessions, newest first, with the exact fields" $ withFixture $ \fx -> do
      (code, v) <- getJson (fxEnv fx) "/api/sessions"
      code `shouldBe` 200
      map (key "id") (arr v) `shouldBe` map A.String [s3, s2, s1]
      forM_ (arr v) $ \x -> case x of
        A.Object o -> sort (map K.toText (KM.keys o)) `shouldBe` sort ["id", "title", "project", "prover", "model", "strategy", "started", "turns", "dollars", "result"]
        _ -> expectationFailure "not an object"
      lean <- canonicalizePath (fxLean fx)
      rocq <- canonicalizePath (fxRocq fx)
      let a = byId s1 v
          b = byId s2 v
          c = byId s3 v
      key "title" a `shouldBe` A.String "Lean session"
      key "project" a `shouldBe` A.String (T.pack lean)
      key "prover" a `shouldBe` A.String "lean4"
      key "model" a `shouldBe` A.String "example/model-a"
      key "strategy" a `shouldBe` A.String "clm"
      key "started" a `shouldBe` A.Number 1790000000
      key "turns" a `shouldBe` A.Number 2
      num (key "dollars" a) `shouldSatisfy` near 0.035
      key "result" a `shouldBe` A.String "completed"
      key "project" b `shouldBe` A.String (T.pack rocq)
      key "prover" b `shouldBe` A.String "rocq"
      key "strategy" b `shouldBe` A.String "compress"
      key "result" b `shouldBe` A.String "compression"
      -- the child: its root's project and strategy, no dollars of its own
      key "prover" c `shouldBe` A.String "rocq"
      key "strategy" c `shouldBe` A.String "compress"
      key "dollars" c `shouldBe` A.Number 0
      key "title" c `shouldBe` A.Null
    it "filters by prover, model, strategy and project" $ withFixture $ \fx -> do
      let ids p = map (key "id") . arr . snd <$> getJson (fxEnv fx) p
      ids "/api/sessions?prover=rocq" `shouldReturn` map A.String [s3, s2]
      ids "/api/sessions?prover=lean4" `shouldReturn` [A.String s1]
      ids "/api/sessions?model=openai%2Fgpt-5" `shouldReturn` map A.String [s3, s2]
      ids "/api/sessions?strategy=clm" `shouldReturn` [A.String s1]
      ids "/api/sessions?strategy=compress&prover=rocq" `shouldReturn` map A.String [s3, s2]
      ids "/api/sessions?prover=isabelle" `shouldReturn` []
      rocq <- canonicalizePath (fxRocq fx)
      ids ("/api/sessions?project=" <> TE.encodeUtf8 (T.pack rocq)) `shouldReturn` map A.String [s3, s2]
      ids "/api/sessions?model=" `shouldReturn` map A.String [s3, s2, s1]
    it "takes a query value that is not UTF-8 as matching nothing, not as an error" $ withFixture $ \fx -> do
      (code, v) <- getJson (fxEnv fx) "/api/sessions?model=%FF%FE"
      (code, v) `shouldBe` (200, A.Array mempty)
    it "an empty FISHER_HOME gives an empty list" $ withSystemTempDirectory "fisher-ui" $ \tmp -> do
      env <- newUiEnv tmp tmp tmp
      snd <$> getJson env "/api/sessions" `shouldReturn` A.Array mempty

  describe "GET /api/sessions/:id" $ do
    it "gives the messages in order with tool calls decoded, the cost and no children" $ withFixture $ \fx -> do
      (code, v) <- getJson (fxEnv fx) ("/api/sessions/" <> TE.encodeUtf8 s1)
      code `shouldBe` 200
      key "id" (key "session" v) `shouldBe` A.String s1
      let ms = arr (key "messages" v)
      map (key "role") ms `shouldBe` map A.String ["user", "assistant", "tool", "assistant"]
      map (key "content") ms `shouldBe` map A.String ["prove t", "", "theorem t", "done"]
      case ms of
        [_, call, tool, _] -> do
          key "name" (key "function" (head (arr (key "toolCalls" call)))) `shouldBe` A.String "read_file"
          key "toolName" tool `shouldBe` A.String "read_file"
          key "toolCallId" tool `shouldBe` A.String "call_1"
          key "toolCalls" tool `shouldBe` A.Null
          key "at" tool `shouldBe` A.Number 1790000003
        _ -> expectationFailure "four messages"
      num (key "dollars" (key "cost" v)) `shouldSatisfy` near 0.035
      key "input" (key "cost" v) `shouldBe` A.Number 300
      key "output" (key "cost" v) `shouldBe` A.Number 30
      key "cacheRead" (key "cost" v) `shouldBe` A.Number 120
      key "children" v `shouldBe` A.Array mempty
    it "names the compression children" $ withFixture $ \fx -> do
      (_, v) <- getJson (fxEnv fx) ("/api/sessions/" <> TE.encodeUtf8 s2)
      key "children" v `shouldBe` A.Array (V.fromList [A.String s3])
      num (key "dollars" (key "cost" v)) `shouldSatisfy` near 0.5
    it "an unknown id is 404 and a path-like id 400" $ withFixture $ \fx -> do
      fst <$> getJson (fxEnv fx) "/api/sessions/nope" `shouldReturn` 404
      fst <$> getJson (fxEnv fx) "/api/sessions/..%2f..%2fetc" `shouldReturn` 400

  describe "sessionCost" $ do
    it "reads a log once for concurrent requests" $ withFixture $ \fx -> do
      let p = replayLogPath (fxHome fx) s1
      nReads <- newIORef (0 :: Int)
      release <- newEmptyMVar
      lc <- newLogCache (\f -> atomicModifyIORef' nReads (\k -> (k + 1, ())) >> readMVar release >> BL.readFile f)
      outs <- mapM (const newEmptyMVar) [1 :: Int, 2, 3]
      forM_ outs $ \o -> forkIO (sessionCostWith lc p >>= putMVar o)
      threadDelay 200000
      putMVar release ()
      cs <- mapM takeMVar outs
      readIORef nReads `shouldReturn` 1
      map costDollars cs `shouldSatisfy` all (near 0.035)
    it "keeps the heap small while it reads a 60 MB log" $ withSystemTempDirectory "fisher-ui" $ \tmp -> do
      getRTSStatsEnabled `shouldReturn` True
      let p = tmp </> "big.jsonl"
      bigLog p
      performMajorGC
      lc <- newLogCache BL.readFile
      c <- sessionCostWith lc p
      costDollars c `shouldSatisfy` (\d -> abs (d - 10) < 1e-6)
      performMajorGC
      st <- getRTSStats
      -- the whole file held at once would be over 60 MB live
      max_live_bytes st `shouldSatisfy` (< 24000000)
    it "sums usage.cost over the http events" $ withFixture $ \fx -> do
      c <- sessionCost (replayLogPath (fxHome fx) s1)
      costDollars c `shouldSatisfy` near 0.035
    it "reads a 60 MB log once for two calls, and again when it grows" $ withSystemTempDirectory "fisher-ui" $ \tmp -> do
      let p = tmp </> "big.jsonl"
          n = 10000 :: Int
      bigLog p
      nReads <- newIORef (0 :: Int)
      lc <- newLogCache (\f -> modifyIORef' nReads (+ 1) >> BL.readFile f)
      a <- sessionCostWith lc p
      b <- sessionCostWith lc p
      readIORef nReads `shouldReturn` 1
      a `shouldBe` b
      costDollars a `shouldSatisfy` (\d -> abs (d - 10) < 1e-6)
      costInput a `shouldBe` 100 * fromIntegral n
      BL.appendFile p (httpLine False 0.5 <> "\n")
      c <- sessionCostWith lc p
      readIORef nReads `shouldReturn` 2
      costDollars c `shouldSatisfy` (\d -> abs (d - 10.5) < 1e-6)

  describe "state.db while the agent writes" $ do
    it "leaves a closed WAL database's directory as it was (no -wal or -shm)" $ withFixture $ \fx -> do
      let db = fxHome fx </> "state.db"
      listed <- sort <$> listDirectory (fxHome fx)
      listed `shouldNotContain` ["state.db-wal"]
      -- the header's read and write versions are 2: WAL
      B.take 2 . B.drop 18 <$> B.readFile db `shouldReturn` B.pack [2, 2]
      rows <- withRo db allSessions
      length rows `shouldBe` 3
      sort <$> listDirectory (fxHome fx) `shouldReturn` listed
    it "reads within 2 s while a writer holds a WAL transaction, and sees the row after the commit" $ withFixture $ \fx -> do
      let db = fxHome fx </> "state.db"
          env = fxEnv fx
      bracket (open db) close $ \w -> do
        execute_ w "PRAGMA busy_timeout = 10000"
        query_ w "PRAGMA journal_mode" `shouldReturn` [Only ("wal" :: T.Text)]
        execute_ w "BEGIN IMMEDIATE"
        execute w "INSERT INTO sessions (id, source, started_at) VALUES (?, 'cli', ?)" ("20261002_000000_dddddd" :: T.Text, 1790100000 :: Double)
        during <- timeout 2000000 (listSessions env Nothing Nothing Nothing Nothing)
        fmap (map (srId . siRow)) during `shouldBe` Just [s3, s2, s1]
        execute_ w "COMMIT"
      committed <- listSessions env Nothing Nothing Nothing Nothing
      map (srId . siRow) committed `shouldBe` ["20261002_000000_dddddd", s3, s2, s1]
    it "retries while the database is locked and returns once it is free" $ withFixture $ \fx -> do
      let db = fxHome fx </> "state.db"
      w <- open db
      execute_ w "PRAGMA locking_mode = EXCLUSIVE"
      execute_ w "BEGIN EXCLUSIVE"
      execute w "INSERT INTO sessions (id, source, started_at) VALUES (?, 'cli', ?)" ("20261002_000000_eeeeee" :: T.Text, 1790100000 :: Double)
      done <- newEmptyMVar
      t0 <- getCurrentTime
      _ <- forkIO $ do
        r <- try (timeout 3000000 (withRo db allSessions))
        t1 <- getCurrentTime
        putMVar done (r, diffUTCTime t1 t0)
      threadDelay 500000
      execute_ w "COMMIT"
      close w
      (r, waited) <- takeMVar done
      -- it waited for the lock rather than reading past it
      waited `shouldSatisfy` (\d -> d >= 0.45 && d < 2)
      case r of
        Right (Just rows) -> length rows `shouldBe` 4
        Right Nothing -> expectationFailure "the read did not return"
        Left (e :: SQLError) -> expectationFailure ("the read failed: " <> show e)
    it "opens state.db read-only" $ withFixture $ \fx -> do
      r <- try (withRo (fxHome fx </> "state.db") (\c -> execute_ c "DELETE FROM messages"))
      case r of
        Left (_ :: SQLError) -> pure ()
        Right () -> expectationFailure "a write went through"
      withRo (fxHome fx </> "state.db") (\c -> query_ c "SELECT COUNT(*) FROM messages") `shouldReturn` [Only (8 :: Int)]

-- | A 60 MB log of 10000 padded prompts and 10000 priced responses
-- ($0.001, 100 input tokens each), written as it is generated.
bigLog :: FilePath -> IO ()
bigLog p = do
  let pad = logLine "prompt" [("text", A.String (T.replicate 6000 "x"))]
  BL.writeFile p (BLC.unlines (concat (replicate 10000 [pad, httpLine False 0.001])))
  getFileSize p >>= (`shouldSatisfy` (> 60000000))
