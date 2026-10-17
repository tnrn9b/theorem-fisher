-- | The attempt watcher: turns from the streams, the ledger read live from
-- a real state.db (stop, no stop, a locked database, three failed reads),
-- the dollars, and model failures from a finished attempt's files.
module Fisher.Cli.WatchSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (async, cancel)
import Control.Exception (SomeException, finally, try)
import Control.Monad (forM_)
import Data.Aeson (Value, decodeStrict, object, (.=))
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.SQLite.Simple (Connection, Only (..), Query, close, execute, execute_, open, query_)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (createDirectoryIfMissing, createDirectoryLink)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Hspec

import Fisher.Cli.Watch
import Fisher.Search.Controller

ledgerSchema :: [Query]
ledgerSchema =
  [ "CREATE TABLE IF NOT EXISTS ui_events (session_id TEXT NOT NULL, seq INTEGER NOT NULL, at REAL NOT NULL,\
    \ kind TEXT NOT NULL, data TEXT NOT NULL, PRIMARY KEY (session_id, seq))"
  , "CREATE TABLE IF NOT EXISTS ui_runs (tag TEXT PRIMARY KEY, session_id TEXT NOT NULL, pid INTEGER NOT NULL, started REAL NOT NULL)" ]

-- | A temporary state.db in WAL mode, as the agent opens it, with the run
-- of tag @t1@ as session @s1@; the writer stays open for the test.
withLedger :: (FilePath -> FilePath -> Connection -> IO a) -> IO a
withLedger k = withSystemTempDirectory "watch" $ \dir -> do
  let db = dir </> "state.db"
  c <- open db
  execute_ c "PRAGMA busy_timeout = 10000"
  _ <- query_ c "PRAGMA journal_mode=WAL" :: IO [Only Text]
  mapM_ (execute_ c) ledgerSchema
  execute c "INSERT INTO ui_runs (tag, session_id, pid, started) VALUES (?, ?, ?, ?)" ("t1" :: Text, "s1" :: Text, 1 :: Int, 0 :: Double)
  r <- k dir db c
  close c
  pure r

-- | The ledger's next row for session s1.
emitRow :: Connection -> Text -> Value -> IO ()
emitRow c kind v =
  execute c "INSERT INTO ui_events (session_id, seq, at, kind, data) SELECT ?, COALESCE(MAX(seq), 0) + 1, 0, ?, ? FROM ui_events WHERE session_id = ?"
    ("s1" :: Text, kind, TE.decodeUtf8 (BL.toStrict (A.encode v)), "s1" :: Text)

-- | One model call as the agent records it: a cost row (unpriced here) and
-- the assistant message (the real agent writes messages at session end).
assistant :: Connection -> IO ()
assistant c = do
  emitRow c "cost" (object ["dollars" .= A.Null, "input" .= (10 :: Int), "output" .= (5 :: Int)])
  emitRow c "message" (object ["role" .= ("assistant" :: Text), "text" .= ("thinking aloud" :: Text)])

stepOn :: Connection -> FilePath -> IO ()
stepOn c target = emitRow c "step" (object ["seq" .= (1 :: Int), "path" .= target, "hash" .= ("h" :: Text)])

costRow :: Connection -> Maybe Double -> IO ()
costRow c d = emitRow c "cost" (object ["dollars" .= d, "input" .= (10 :: Int), "output" .= (5 :: Int)])

linesOf :: [BS.ByteString] -> [Value]
linesOf = mapMaybe decodeStrict

stopOf :: Either Watched a -> Maybe Stop
stopOf = either wStop (const Nothing)

spec :: Spec
spec = do
  describe "streamSignals" $ do
    it "claude-code: two lines with one message.id are one turn" $ do
      let ls = linesOf
            [ "{\"type\":\"assistant\",\"message\":{\"id\":\"m1\",\"content\":[{\"type\":\"text\",\"text\":\"a\"}]}}"
            , "{\"type\":\"assistant\",\"message\":{\"id\":\"m1\",\"content\":[{\"type\":\"tool_use\",\"name\":\"Read\"}]}}"
            , "{\"type\":\"user\",\"message\":{\"content\":[]}}"
            , "{\"type\":\"assistant\",\"message\":{\"id\":\"m2\",\"content\":[]}}" ]
      streamSignals "claude-code" ls `shouldBe` [Turn, Turn]
    it "codex: item.completed agent messages and reasoning are turns, other items not" $ do
      let ls = linesOf
            [ "{\"type\":\"item.completed\",\"item\":{\"type\":\"agent_message\",\"text\":\"hi\"}}"
            , "{\"type\":\"item.completed\",\"item\":{\"type\":\"command_execution\"}}"
            , "{\"type\":\"item.started\",\"item\":{\"type\":\"agent_message\"}}"
            , "{\"type\":\"item.completed\",\"item\":{\"type\":\"reasoning\",\"text\":\"hm\"}}" ]
      streamSignals "codex" ls `shouldBe` [Turn, Turn]
    it "native: each model call (a cost row) is a turn" $
      streamSignals "gauss" (linesOf ["{\"type\":\"assistant\",\"message\":{}}", "{\"type\":\"assistant\",\"message\":{}}", "{\"type\":\"result\"}"])
        `shouldBe` [Turn, Turn]

  describe "watchChild on the ledger" $ do
    it "stops a child that talks six turns without a step, within 12 s" $ withLedger $ \dir db c -> do
      let target = dir </> "ws" </> "F.lean"
      mapM_ (const (assistant c)) [1 :: Int .. 6]
      r <- timeout (12 * 1000000) (watchChild (StallRule 3 Nothing) True (FromLedger db "t1" target) (threadDelay (60 * 1000000)))
      case r of
        Nothing -> expectationFailure "no verdict within 12 s"
        Just res -> fmap (\s -> (stReason s, stTurns s)) (stopOf res) `shouldBe` Just (StallTurns, 6)
    it "lets a child that steps on the target after every two messages run to its end" $ withLedger $ \dir db c -> do
      let target = dir </> "ws" </> "F.lean"
          -- six messages by the end, four before the first poll: without
          -- the steps the poll at 5 s would stop it
          twoThenStep = assistant c >> assistant c >> stepOn c target
          child = do
            twoThenStep
            threadDelay 1000000
            twoThenStep
            threadDelay 5500000
            twoThenStep
            threadDelay 2000000
            pure (7 :: Int)
      r <- watchChild (StallRule 3 Nothing) True (FromLedger db "t1" target) child
      either (const Nothing) (Just . fst) r `shouldBe` Just 7
    it "a step on another file is no change" $ withLedger $ \dir db c -> do
      let target = dir </> "ws" </> "F.lean"
      assistant c >> assistant c >> stepOn c (dir </> "ws" </> "Other.lean") >> mapM_ (const (assistant c)) [1 :: Int .. 4]
      r <- watchChildWith defaultWatchOpts {woInterval = 0.3} (StallRule 3 Nothing) True (FromLedger db "t1" target) (threadDelay (20 * 1000000))
      fmap stTurns (stopOf r) `shouldBe` Just 6
    it "with early stop off it never stops, and still prices the attempt" $ withLedger $ \dir db c -> do
      let target = dir </> "ws" </> "F.lean"
      mapM_ (const (assistant c)) [1 :: Int .. 5]
      costRow c (Just 0.5)
      r <- watchChildWith defaultWatchOpts {woInterval = 0.3} (StallRule 3 Nothing) False (FromLedger db "t1" target) (threadDelay 1000000)
      fmap (wDollars . snd) (either (const Nothing) Just r) `shouldBe` Just (Just 0.5)
    it "wDollars sums the cost rows; a cost without dollars adds nothing" $ withLedger $ \dir db c -> do
      costRow c (Just 0.25) >> assistant c >> costRow c Nothing >> costRow c (Just 0.5)
      r <- watchChildWith defaultWatchOpts {woInterval = 0.3} (StallRule 8 (Just 1.5)) True (FromLedger db "t1" (dir </> "F.lean")) (threadDelay 1000000)
      case r of
        Right (_, w) -> do
          wDollars w `shouldBe` Just 0.75
          wLast w `shouldBe` Just "thinking aloud"
        Left w -> expectationFailure ("stopped: " <> show (wStop w))
    it "no cost rows with a number is no dollars" $ withLedger $ \dir db c -> do
      costRow c Nothing >> assistant c
      r <- watchChildWith defaultWatchOpts {woInterval = 0.3} (StallRule 8 Nothing) True (FromLedger db "t1" (dir </> "F.lean")) (threadDelay 600000)
      fmap (wDollars . snd) (either (const Nothing) Just r) `shouldBe` Just Nothing
    it "a tag not in ui_runs yet is no rows, not an error, for the first three polls" $ withLedger $ \dir db _ -> do
      unread <- newIORef (0 :: Int)
      r <- watchChildWith defaultWatchOpts {woInterval = 0.2, woUnread = \_ -> modifyIORef' unread (+ 1)}
             (StallRule 3 Nothing) True (FromLedger db "not-yet" (dir </> "F.lean")) (threadDelay 500000)
      either (const Nothing) (Just . wUnread . snd) r `shouldBe` Just Nothing
      readIORef unread `shouldReturn` 0
    it "a session that never appears: the sixth poll goes to the stream, whose turns stop the child" $ withLedger $ \dir db _ -> do
      let stream = dir </> "stream.jsonl"
      unread <- newIORef ([] :: [Text])
      let child = do
            BS.writeFile stream (BC.unlines (replicate 6 "{\"type\":\"assistant\",\"message\":{\"content\":[]}}"))
            threadDelay (20 * 1000000)
      t0 <- getCurrentTime
      r <- timeout (10 * 1000000) $
             watchChildWith defaultWatchOpts {woInterval = 0.2, woUnread = \t -> modifyIORef' unread (t :), woStream = Just stream}
               (StallRule 3 Nothing) True (FromLedger db "never" (dir </> "F.lean")) child
      t1 <- getCurrentTime
      fmap (fmap stTurns . stopOf) r `shouldBe` Just (Just 6)
      ws <- readIORef unread
      length ws `shouldBe` 1
      map (T.isInfixOf "no session for never") ws `shouldBe` [True]
      fmap wUnread (either Just (const Nothing) =<< r) `shouldBe` Just (Just (head ws))
      -- six polls of 0.2 s, not the first
      diffUTCTime t1 t0 `shouldSatisfy` (>= 1.1)
    it "a ui_runs row started before the attempt is another run's: its rows are never read" $ withLedger $ \dir db c -> do
      -- t1 was recorded at time 0 (an older run with the same tag)
      mapM_ (const (assistant c)) [1 :: Int, 2, 3]
      costRow c (Just 0.5)
      now <- realToFrac <$> getPOSIXTime
      r <- watchChildWith defaultWatchOpts {woInterval = 0.2, woSince = now} (StallRule 3 Nothing) True (FromLedger db "t1" (dir </> "F.lean")) (threadDelay 500000)
      fmap (wDollars . snd) (either (const Nothing) Just r) `shouldBe` Just Nothing
    it "a ui_runs row started at or after the attempt's start is read" $ withLedger $ \dir db c -> do
      now <- realToFrac <$> getPOSIXTime
      execute c "INSERT INTO ui_runs (tag, session_id, pid, started) VALUES (?, ?, ?, ?)" ("t2" :: Text, "s1" :: Text, 3 :: Int, now + 0.01 :: Double)
      mapM_ (const (assistant c)) [1 :: Int .. 6]
      r <- timeout (5 * 1000000) (watchChildWith defaultWatchOpts {woInterval = 0.2, woSince = now} (StallRule 3 Nothing) True (FromLedger db "t2" (dir </> "F.lean")) (threadDelay (20 * 1000000)))
      fmap (fmap stTurns . stopOf) r `shouldBe` Just (Just 6)
    it "a database locked by a write transaction during one poll: no stop, no crash" $ withLedger $ \dir db c -> do
      let target = dir </> "ws" </> "F.lean"
      assistant c >> assistant c
      costRow c (Just 0.1)
      -- the agent's own connection takes the database exclusively (readers
      -- get SQLITE_BUSY) with a write open from 1 s to 10 s, across the
      -- first poll and its two seconds of retries, then commits and lets go
      locked <- newEmptyMVar
      released <- newEmptyMVar
      _ <- forkIO $ do
        threadDelay 1000000
        held <- try $ do
          _ <- query_ c "PRAGMA locking_mode = EXCLUSIVE" :: IO [Only Text]
          execute_ c "BEGIN EXCLUSIVE"
          execute c "INSERT INTO ui_runs (tag, session_id, pid, started) VALUES (?, ?, ?, ?)" ("other" :: Text, "s2" :: Text, 2 :: Int, 0 :: Double)
        putMVar locked (either (\e -> Just (show (e :: SomeException))) (const Nothing) held)
        threadDelay 9000000
        _ <- try @SomeException $ do
          execute_ c "COMMIT"
          _ <- query_ c "PRAGMA locking_mode = NORMAL" :: IO [Only Text]
          _ <- query_ c "SELECT COUNT(*) FROM ui_runs" :: IO [Only Int]
          pure ()
        putMVar released ()
      -- the lock really stops a reader
      threadDelay 1500000
      probe <- try @SomeException (do
        rc <- open ("file:" <> db <> "?mode=ro")
        n <- query_ rc "SELECT COUNT(*) FROM ui_events" :: IO [Only Int]
        close rc
        pure n)
      either (const True) (const False) probe `shouldBe` True
      r <- watchChild (StallRule 3 Nothing) True (FromLedger db "t1" target) (threadDelay 10000000)
      takeMVar locked `shouldReturn` Nothing
      takeMVar released
      case r of
        Right (_, w) -> wDollars w `shouldBe` Just 0.1
        Left w -> expectationFailure ("stopped: " <> show (wStop w))
    it "three failed reads in a row: one ledger unread, then the stream" $ withSystemTempDirectory "watch" $ \dir -> do
      let db = dir </> "state.db"
          stream = dir </> "stream.jsonl"
      BS.writeFile db "this is not a database"
      BS.writeFile stream ""
      unread <- newIORef ([] :: [Text])
      let child = do
            threadDelay 1200000
            BS.appendFile stream (BC.unlines (replicate 6 "{\"type\":\"assistant\",\"message\":{\"content\":[]}}"))
            threadDelay (20 * 1000000)
      r <- watchChildWith defaultWatchOpts {woInterval = 0.2, woUnread = \t -> modifyIORef' unread (t :), woStream = Just stream}
             (StallRule 3 Nothing) True (FromLedger db "t1" (dir </> "F.lean")) child
      length <$> readIORef unread `shouldReturn` 1
      fmap stTurns (stopOf r) `shouldBe` Just 6

    it "a step through a symlinked workspace is a change to the target" $ withLedger $ \dir db c -> do
      createDirectoryIfMissing True (dir </> "real")
      BS.writeFile (dir </> "real" </> "F.lean") "theorem t : True := sorry\n"
      createDirectoryLink (dir </> "real") (dir </> "link")
      let twoThenStep p = assistant c >> assistant c >> stepOn c p
          child p = twoThenStep p >> twoThenStep p >> threadDelay 1000000 >> twoThenStep p >> threadDelay 600000
          watch target p = watchChildWith defaultWatchOpts {woInterval = 0.3} (StallRule 3 Nothing) True (FromLedger db "t1" target) (child p)
      -- the agent records the link, the target is the real path
      r1 <- watch (dir </> "real" </> "F.lean") (dir </> "link" </> "F.lean")
      either (const False) (const True) r1 `shouldBe` True
      -- and the other way round
      r2 <- watch (dir </> "link" </> "F.lean") (dir </> "real" </> "F.lean")
      either (const False) (const True) r2 `shouldBe` True
    it "a child that ends while the stopping poll runs is not stopped" $ withLedger $ \dir db c -> do
      mapM_ (const (assistant c)) [1 :: Int, 2, 3]
      -- the first poll (0.3 s) waits on the lock until 1 s; the child ends at 0.6 s
      locked <- newEmptyMVar
      _ <- forkIO $ do
        _ <- query_ c "PRAGMA locking_mode = EXCLUSIVE" :: IO [Only Text]
        execute_ c "BEGIN EXCLUSIVE"
        execute c "INSERT INTO ui_runs (tag, session_id, pid, started) VALUES (?, ?, ?, ?)" ("other" :: Text, "s2" :: Text, 2 :: Int, 0 :: Double)
        putMVar locked ()
        threadDelay 1000000
        execute_ c "COMMIT"
        _ <- query_ c "PRAGMA locking_mode = NORMAL" :: IO [Only Text]
        _ <- query_ c "SELECT COUNT(*) FROM ui_runs" :: IO [Only Int]
        pure ()
      takeMVar locked
      r <- watchChildWith defaultWatchOpts {woInterval = 0.3} (StallRule 3 Nothing) True (FromLedger db "t1" (dir </> "F.lean"))
             (threadDelay 600000 >> pure (5 :: Int))
      either (const Nothing) (Just . fst) r `shouldBe` Just 5

  describe "ledgerDollars" $ do
    it "sums the tag's cost rows; no session, an older session or no number is none" $ withLedger $ \_ db c -> do
      costRow c (Just 0.25) >> assistant c >> costRow c Nothing >> costRow c (Just 0.5)
      ledgerDollars db "t1" 0 `shouldReturn` Just 0.75
      ledgerDollars db "nobody" 0 `shouldReturn` Nothing
      now <- realToFrac <$> getPOSIXTime
      ledgerDollars db "t1" now `shouldReturn` Nothing
    it "a session with no priced cost row is none" $ withLedger $ \_ db c -> do
      costRow c Nothing >> assistant c
      ledgerDollars db "t1" 0 `shouldReturn` Nothing

  describe "watchChild on a stream" $ do
    it "cancelling the watching thread returns promptly" $ withSystemTempDirectory "watch" $ \dir -> do
      let stream = dir </> "stream.jsonl"
          target = dir </> "F.lean"
      BS.writeFile target "theorem t : True := sorry\n"
      -- a large stream, so most of each poll is spent reading it
      BS.writeFile stream (BS.replicate (8 * 1024 * 1024) 120)
      childGone <- newEmptyMVar
      a <- async (watchChildWith defaultWatchOpts {woInterval = 0.01} (StallRule 1000000 Nothing) True (FromStream stream "claude-code" target)
                    (threadDelay (60 * 1000000) `finally` putMVar childGone ()))
      threadDelay 500000
      t0 <- getCurrentTime
      done <- timeout 2000000 (cancel a >> takeMVar childGone)
      t1 <- getCurrentTime
      done `shouldBe` Just ()
      diffUTCTime t1 t0 `shouldSatisfy` (< 2)
    it "claude-code: six distinct messages without a change to the target stop it" $ withSystemTempDirectory "watch" $ \dir -> do
      let stream = dir </> "stream.jsonl"
          target = dir </> "F.lean"
      BS.writeFile target "theorem t : True := sorry\n"
      BS.writeFile stream ""
      let child = do
            forM_ ["m1", "m1", "m2", "m3", "m4", "m5", "m6"] $ \i -> do
              BS.appendFile stream ("{\"type\":\"assistant\",\"message\":{\"id\":\"" <> i <> "\",\"content\":[{\"type\":\"text\",\"text\":\"look " <> i <> "\"}]}}\n")
              threadDelay 100000
            threadDelay (20 * 1000000)
      r <- watchChildWith defaultWatchOpts {woInterval = 0.2} (StallRule 3 Nothing) True (FromStream stream "claude-code" target) child
      fmap stTurns (stopOf r) `shouldBe` Just 6
      either wLast (const Nothing) r `shouldBe` Just "look m6"
    it "claude-code: an edit to the target between polls resets the turns" $ withSystemTempDirectory "watch" $ \dir -> do
      let stream = dir </> "stream.jsonl"
          target = dir </> "F.lean"
      BS.writeFile target "theorem t : True := sorry\n"
      BS.writeFile stream ""
      let msg i = BS.appendFile stream ("{\"type\":\"assistant\",\"message\":{\"id\":\"" <> i <> "\",\"content\":[]}}\n")
          child = do
            msg "m1" >> msg "m2"
            threadDelay 500000
            BS.writeFile target "theorem t : True := trivial\n"
            msg "m3" >> msg "m4"
            threadDelay 500000
            pure ()
      r <- watchChildWith defaultWatchOpts {woInterval = 0.2} (StallRule 3 Nothing) True (FromStream stream "claude-code" target) child
      either (const False) (const True) r `shouldBe` True

  describe "failureOf" $ do
    let files dir out err = BS.writeFile (dir </> "stream.jsonl") out >> BS.writeFile (dir </> "stderr.txt") err
        failure dir = failureOf (dir </> "stream.jsonl") (dir </> "stderr.txt")
    it "a result line with a rate limit error is RateLimited" $ withSystemTempDirectory "watch" $ \dir -> do
      files dir "{\"type\":\"assistant\",\"message\":{}}\n{\"type\":\"result\",\"subtype\":\"error_during_execution\",\"is_error\":true,\"error\":\"RateLimitError: 429\",\"result\":\"\"}\n" ""
      failure dir (Just 1) `shouldReturn` Just RateLimited
    it "a result line decides: stderr is not read beside it" $ withSystemTempDirectory "watch" $ \dir -> do
      files dir "{\"type\":\"result\",\"subtype\":\"error_during_execution\",\"is_error\":true,\"error\":\"TypeError: x\"}\n" "HTTP 429 rate limit\n"
      failure dir (Just 1) `shouldReturn` Nothing
    it "a successful result is no failure, whatever stderr says of retries" $ withSystemTempDirectory "watch" $ \dir -> do
      files dir "{\"type\":\"result\",\"subtype\":\"success\",\"is_error\":false,\"result\":\"done\"}\n" "retrying after 429\n"
      failure dir (Just 0) `shouldReturn` Nothing
    it "a successful result whose text says 404 is no failure" $ withSystemTempDirectory "watch" $ \dir -> do
      files dir "{\"type\":\"result\",\"subtype\":\"success\",\"is_error\":false,\"result\":\"The lemma at line 404 is not available in Mathlib; proved it directly.\"}\n" ""
      failure dir (Just 0) `shouldReturn` Nothing
    it "a result's text is read only when it is an error" $ withSystemTempDirectory "watch" $ \dir -> do
      -- not a success, but neither is_error nor an error subtype: the text is the model's
      files dir "{\"type\":\"result\",\"subtype\":\"interrupted\",\"is_error\":false,\"result\":\"HTTP 404 is not available\"}\n" ""
      failure dir (Just 1) `shouldReturn` Nothing
      -- the error field is always read
      files dir "{\"type\":\"result\",\"subtype\":\"interrupted\",\"is_error\":false,\"error\":\"model_not_found\",\"result\":\"\"}\n" ""
      failure dir (Just 1) `shouldReturn` Just Unavailable
      -- an error subtype: the text too
      files dir "{\"type\":\"result\",\"subtype\":\"error_during_execution\",\"result\":\"HTTP 404: model is not available\"}\n" ""
      failure dir (Just 1) `shouldReturn` Just Unavailable
    it "no result line and a non-zero exit: stderr decides" $ withSystemTempDirectory "watch" $ \dir -> do
      files dir "" "Error: model_not_found: example/model-a\n"
      failure dir (Just 1) `shouldReturn` Just Unavailable
    it "no result line and exit 0, or killed at the limit: stderr is not read" $ withSystemTempDirectory "watch" $ \dir -> do
      files dir "" "Error: model_not_found: example/model-a\n"
      failure dir (Just 0) `shouldReturn` Nothing
      failure dir Nothing `shouldReturn` Nothing
    it "missing files are no failure" $ withSystemTempDirectory "watch" $ \dir ->
      failure dir (Just 1) `shouldReturn` Nothing
