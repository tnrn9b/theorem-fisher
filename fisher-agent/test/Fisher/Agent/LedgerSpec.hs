-- | The event ledger (Fisher UI, task 4): numbered events per session in
-- state.db, carried on across a reopen, and the runs' tags. Needs sqlite
-- (the Mac's stack test; the cloud's stand-in cannot run it).
module Fisher.Agent.LedgerSpec (spec) where

import Control.Concurrent.Async (forConcurrently_)
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.List (sort)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.SQLite.Simple (Only (..), close, open, query)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Agent.Ledger
import Fisher.Agent.Loop (AgentResult (..), Callbacks (..), noCallbacks)
import Fisher.Agent.Steps
import Fisher.Agent.UiHooks (endStatus, newUiHooks, uiCallbacks, usageOfBody)
import Fisher.Py.Value

-- | The rows as the UI server reads them (this package only writes).
ledgerEvents :: FilePath -> Text -> Int -> IO [(Int, Text, A.Value)]
ledgerEvents db sid n0 = do
  c <- open db
  rows <- query c "SELECT seq, kind, data FROM ui_events WHERE session_id = ? AND seq > ? ORDER BY seq" (sid, n0) :: IO [(Int, Text, Text)]
  close c
  pure [(n, k, fromJust (A.decodeStrict (TE.encodeUtf8 d))) | (n, k, d) <- rows]

ledgerRun :: FilePath -> Text -> IO (Maybe (Text, Int))
ledgerRun db tag = do
  c <- open db
  rows <- query c "SELECT session_id, pid FROM ui_runs WHERE tag = ?" (Only tag)
  close c
  pure (case rows of (r : _) -> Just r; [] -> Nothing)

ev :: Text -> [(A.Key, A.Value)] -> UiEvent
ev k kvs = UiEvent k (A.object kvs)

spec :: Spec
spec = do
  describe "Fisher.Agent.Ledger" $ do
    it "numbers each session's events from 1 with no gaps, also after a reopen" $ withSystemTempDirectory "fisher-ledger" $ \tmp -> do
      let db = tmp </> "state.db"
      a <- openLedger db "s1"
      b <- openLedger db "s2"
      mapM (emit a) [ev "tool_start" [("name", A.String "read_file")], ev "tool_end" [("ok", A.Bool True)]] `shouldReturn` [1, 2]
      emit b (ev "thinking" [("label", A.String "hm")]) `shouldReturn` 1
      closeLedger a
      a2 <- openLedger db "s1"
      emit a2 (ev "end" [("status", A.String "completed")]) `shouldReturn` 3
      closeLedger a2
      closeLedger b
      es <- ledgerEvents db "s1" 0
      [(n, k) | (n, k, _) <- es] `shouldBe` [(1, "tool_start"), (2, "tool_end"), (3, "end")]
      [v | (3, _, v) <- es] `shouldBe` [A.object [("status", A.String "completed")]]
      map (\(n, _, _) -> n) <$> ledgerEvents db "s1" 1 `shouldReturn` [2, 3]
    it "gives two writers of one session (two handles) one shared sequence with no gap or duplicate" $ withSystemTempDirectory "fisher-ledger" $ \tmp -> do
      let db = tmp </> "state.db"
          n = 50 :: Int
      a <- openLedger db "s1"
      b <- openLedger db "s1"
      forConcurrently_ [1 .. 2 * n] $ \i ->
        emit (if even i then a else b) (ev "tool_start" [("i", A.toJSON i)])
      closeLedger a
      closeLedger b
      es <- ledgerEvents db "s1" 0
      [k | (k, _, _) <- es] `shouldBe` [1 .. 2 * n]
      sort [A.encode v | (_, _, v) <- es] `shouldBe` sort [A.encode (A.object [("i", A.toJSON i)]) | i <- [1 .. 2 * n]]
    it "keeps a tag's first run when a second process records the same tag" $ withSystemTempDirectory "fisher-ledger" $ \tmp -> do
      let db = tmp </> "state.db"
      l <- openLedger db "s1"
      recordRun l "tag-1" 1
      l2 <- openLedger db "s2"
      recordRun l2 "tag-1" 2
      closeLedger l
      closeLedger l2
      ledgerRun db "tag-1" `shouldReturn` Just ("s1", 1)
    it "maps a run's tag to its session" $ withSystemTempDirectory "fisher-ledger" $ \tmp -> do
      let db = tmp </> "state.db"
      l <- openLedger db "s9"
      recordRun l "tag-1" 4242
      closeLedger l
      ledgerRun db "tag-1" `shouldReturn` Just ("s9", 4242)
      ledgerRun db "other" `shouldReturn` Nothing
  describe "Fisher.Agent.UiHooks" $ do
    it "numbers a concurrent batch's steps once each, one whole line per step" $ withSystemTempDirectory "fisher-uihooks" $ \tmp -> do
      let log' = tmp </> "steps.jsonl"
          n = 40 :: Int
          name i = tmp </> ("F" <> show i <> ".lean")
          body i = T.replicate 2000 (T.pack (show i)) <> "\n"
      h <- newUiHooks Nothing log' (\_ p -> pure (T.unpack p)) Nothing
      done <- maybe (fail "no cbToolDone") pure (cbToolDone (uiCallbacks h noCallbacks))
      forConcurrently_ [1 .. n] $ \i -> do
        BS.writeFile (name i) (TE.encodeUtf8 (body i))
        done "task" (T.pack ("call_" <> show i)) "write_file" (PDict [("path", PStr (T.pack (name i)))]) (Right "{\"bytes_written\": 1, \"dirs_created\": false}")
      raw <- BS.readFile log'
      length (BC.lines raw) `shouldBe` n
      steps <- readSteps log'
      sort (map stSeq steps) `shouldBe` [1 .. n]
      map stSeq steps `shouldBe` [1 .. n]
      [(stCallId s, stText s) | s <- steps, stCallId s == "call_7"] `shouldBe` [("call_7", body (7 :: Int))]
    it "numbers its next step after one appended from outside (the UI's hand edit)" $ withSystemTempDirectory "fisher-uihooks" $ \tmp -> do
      let log' = tmp </> "steps.jsonl"
          file = tmp </> "A.lean"
      appendStep log' (Step 1 "call_0" file (fileHash "a") "a" "write_file" 1)
      h <- newUiHooks Nothing log' (\_ p -> pure (T.unpack p)) Nothing
      done <- maybe (fail "no cbToolDone") pure (cbToolDone (uiCallbacks h noCallbacks))
      let tool cid = done "task" cid "write_file" (PDict [("path", PStr (T.pack file))]) (Right "{\"bytes_written\": 1, \"dirs_created\": false}")
      BS.writeFile file "b"
      tool "call_1"
      -- the UI appends a hand step while the agent runs
      _ <- appendNextStep log' (\k -> Step k ("hand_" <> T.pack (show k)) file (fileHash "c") "c" "hand" 3)
      BS.writeFile file "d"
      tool "call_2"
      tool "call_3"
      steps <- readSteps log'
      [(stSeq s', stCallId s') | s' <- steps] `shouldBe` [(1, "call_0"), (2, "call_1"), (3, "hand_3"), (4, "call_2"), (5, "call_3")]
    it "numbers concurrent agent and outside appends once each, [1..n] in the log's order" $ withSystemTempDirectory "fisher-uihooks" $ \tmp -> do
      let log' = tmp </> "steps.jsonl"
          n = 30 :: Int
          name i = tmp </> ("G" <> show i <> ".lean")
      h <- newUiHooks Nothing log' (\_ p -> pure (T.unpack p)) Nothing
      done <- maybe (fail "no cbToolDone") pure (cbToolDone (uiCallbacks h noCallbacks))
      forConcurrently_ [1 .. 2 * n] $ \i ->
        if even i
          then do
            BS.writeFile (name i) (TE.encodeUtf8 (T.replicate 3000 "x"))
            done "task" (T.pack ("call_" <> show i)) "write_file" (PDict [("path", PStr (T.pack (name i)))]) (Right "{\"bytes_written\": 1, \"dirs_created\": false}")
          else () <$ appendNextStep log' (\k -> Step k ("hand_" <> T.pack (show i)) (name i) (fileHash "y") (T.replicate 3000 "y") "hand" 0)
      raw <- BS.readFile log'
      length (BC.lines raw) `shouldBe` 2 * n
      map stSeq <$> readSteps log' `shouldReturn` [1 .. 2 * n]
      lastSeq log' `shouldReturn` 2 * n
    it "reads usage from a reply and from a stream" $ do
      usageOfBody "{\"usage\": {\"prompt_tokens\": 100, \"completion_tokens\": 10, \"cost\": 0.25}}" `shouldBe` Just (Just 0.25, 100, 10)
      usageOfBody "data: {\"choices\": []}\n\ndata: {\"usage\": {\"prompt_tokens\": 7, \"completion_tokens\": 3}}\n\ndata: [DONE]\n" `shouldBe` Just (Nothing, 7, 3)
      usageOfBody "data: {\"type\": \"message_start\", \"message\": {\"usage\": {\"input_tokens\": 9, \"output_tokens\": 1}}}\ndata: {\"type\": \"message_delta\", \"usage\": {\"output_tokens\": 5}}\n" `shouldBe` Just (Nothing, 9, 5)
      usageOfBody "{\"choices\": []}" `shouldBe` Nothing
    it "names how a turn ended" $ do
      endStatus (Just (RunDone (PDict [("completed", PBool True)]))) `shouldBe` "completed"
      endStatus (Just (RunDone (PDict [("completed", PBool False), ("interrupted", PBool True)]))) `shouldBe` "interrupted"
      endStatus (Just (RunDone (PDict [("completed", PBool False)]))) `shouldBe` "failed"
      endStatus (Just (RunRaised "X" "y")) `shouldBe` "error"
      endStatus Nothing `shouldBe` "error"
