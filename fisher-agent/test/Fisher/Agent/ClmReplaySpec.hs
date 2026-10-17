-- | CLM sessions replay bit for bit (docs/clm.md section 9): each
-- scenario of test/golden/clm/scenarios.json runs with a replay log, and
-- 'replayFile' runs it again from the log; a log whose context file was
-- changed is refused. Also @state.json@'s round trip and the rollback a
-- provider's over-long-request error causes.
module Fisher.Agent.ClmReplaySpec (spec) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (LocalTime (..), TimeOfDay (..), fromGregorian)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Agent.AuxSpec (testSys)
import Fisher.Agent.ClmHarness (runScenarioWith)
import Fisher.Agent.Context.Budget (pathAliases, samePath)
import Fisher.Agent.Context.Clm
import Fisher.Agent.Context.Mirror (Turn (..))
import Fisher.Agent.Loop (batchWritesSameFile, hasTextCall, responseRefusal)
import Fisher.Agent.Replay
import Fisher.Agent.TestUtil
import Fisher.Py.Value

record :: FilePath -> PyVal -> IO FilePath
record dir sc = do
  lg <- openReplayLog dir "clm"
  _ <- runScenarioWith (Just lg) sc
  closeReplayLog lg
  pure (replayPath dir "clm")

replay :: FilePath -> FilePath -> IO ReplayReport
replay dir path = do
  let base = ReplayBase (testSys dir [] (\_ -> pure ())) (LocalTime (fromGregorian 2026 10 6) (TimeOfDay 10 15 0)) (pure (PDict [])) (const Nothing)
  replayFile base path >>= either (fail . T.unpack) pure

-- | The log with the first event of a kind changed.
tamper :: T.Text -> (PyVal -> PyVal) -> FilePath -> IO ()
tamper kind f path = do
  ls <- BC.lines <$> BS.readFile path
  let go done [] = ([], done)
      go done (l : rest) = case loads l of
        Right v | not done, get "kind" v == Just (PStr kind) ->
          let (rest', d) = go True rest in (TE.encodeUtf8 (dumpsAscii (f v)) : rest', d)
        _ -> let (rest', d) = go done rest in (l : rest', d)
      (ls', changed) = go False ls
  if changed then BS.writeFile path (BC.unlines ls') else fail ("no " <> T.unpack kind <> " event")

spec :: Spec
spec = do
  cases <- runIO (loadCases "test/golden/clm/scenarios.json")
  describe "CLM sessions replay (scenarios.json)" $
    forM_ cases $ \sc -> it (show (txt "name" sc)) $ withSystemTempDirectory "clm-replay" $ \dir -> do
      path <- record dir sc
      r <- replay dir path
      rpMismatches r `shouldBe` []
  let named n = head [c | c <- cases, txt "name" c == n]
  describe "a changed log is refused" $ do
    it "a context file read that differs" $ withSystemTempDirectory "clm-replay" $ \dir -> do
      path <- record dir (named "flatten-in-loop")
      -- the second read-back is the model's edit: give it different text
      lsBefore <- BC.lines <$> BS.readFile path
      let readsLogged = [l | l <- lsBefore, Right v <- [loads l], get "kind" v == Just (PStr "clm_read")]
      length readsLogged `shouldSatisfy` (>= 2)
      let change v = case get "file" v of
            Just (PStr t) -> set "file" (PStr (T.replace "one sorry" "two sorries" t)) v
            _ -> v
      -- change the read that carries the edit (the second clm_read)
      ls <- BC.lines <$> BS.readFile path
      let step (n, acc) l = case loads l of
            Right v | get "kind" v == Just (PStr "clm_read") -> (n + 1, acc <> [if n == (1 :: Int) then TE.encodeUtf8 (dumpsAscii (change v)) else l])
            _ -> (n, acc <> [l])
      BS.writeFile path (BC.unlines (snd (foldl step (0, []) ls)))
      r <- replay dir path
      rpMismatches r `shouldNotBe` []
    it "a context file write that differs" $ withSystemTempDirectory "clm-replay" $ \dir -> do
      path <- record dir (named "flatten-in-loop")
      tamper "clm_write" (\v -> set "text" (PStr (maybe "" pyStrOf (get "text" v) <> "x")) v) path
      r <- replay dir path
      rpMismatches r `shouldNotBe` []
  describe "state.json" $
    it "round-trips" $ do
      let st =
            newClmState
              { stTurns = [Turn 3 (PDict [("role", PStr "user"), ("content", PStr "hi")])], stCounter = 7
              , stArmed = S.fromList [75], stFlushed = True, stShortened = S.fromList [4], stSeen = [3, 4]
              , stNoteId = Just 5, stRollbacks = 2, stLastMirror = Just "# Fisher context file\n" }
      stateFromPy (stateToPy st) `shouldBe` Just st {stIters = 0}
  describe "a provider's over-long-request error" $ do
    let asst = PDict [("role", PStr "assistant"), ("content", PStr "ok"), ("tool_calls", PList [PDict [("id", PStr "call_1"), ("type", PStr "function"), ("function", PDict [("name", PStr "terminal"), ("arguments", PStr "{}")])]])]
        tool = PDict [("role", PStr "tool"), ("content", PStr "out"), ("tool_call_id", PStr "call_1")]
        st0 = newClmState {stTurns = [Turn 1 (PDict [("role", PStr "user"), ("content", PStr "more")]), Turn 2 asst, Turn 3 tool], stCounter = 4}
    it "drops the newest turn and adds the note" $
      case overflowRollback defaultClmConfig "/ctx/c.txt" Nothing st0 of
        Right st -> do
          map turnId (stTurns st) `shouldBe` [1, 4]
          stNoteId st `shouldBe` Just 4
          T.isPrefixOf "[Context over budget: the last turn (terminal, " (maybe "" pyStrOf (get "content" (turnMsg (last (stTurns st))))) `shouldBe` True
        Left e -> expectationFailure (T.unpack e)
    it "replaces its own note and drops the turn before it" $
      case overflowRollback defaultClmConfig "/ctx/c.txt" Nothing st0 >>= overflowRollback defaultClmConfig "/ctx/c.txt" Nothing of
        Right st -> map turnId (stTurns st) `shouldBe` [4]
        Left e -> expectationFailure (T.unpack e)
    it "gives up when there is nothing left to drop" $
      overflowRollback defaultClmConfig "/ctx/c.txt" Nothing newClmState `shouldSatisfy` either (const True) (const False)
  describe "the context file under another spelling (macOS's /private/var)" $ do
    let ctx = "/var/folders/ab/T/g/fisher/ctx/s1/context.txt"
        call name p = PDict [("id", PStr "c1"), ("type", PStr "function"), ("function", PDict [("name", PStr name), ("arguments", PStr ("{\"path\": \"" <> p <> "\"}"))])]
    it "the same file is recognised" $ do
      samePath ctx ("/private" <> ctx) `shouldBe` True
      samePath ctx "/var/folders//ab/./T/g/fisher/ctx/s1/context.txt" `shouldBe` True
      samePath ctx "/var/folders/ab/T/g/fisher/ctx/s1/other.txt" `shouldBe` False
      samePath "/private/Users/x" "/Users/x" `shouldBe` False
    it "a read through /private counts as a read of the context file" $
      crMirrorRead (classifyCall ctx (call "read_file" ("/private" <> ctx)) "{\"content\": \"x\"}") `shouldBe` True
    it "a terminal command naming the context file's directory is a context call" $ do
      let shell cmd = PDict [("id", PStr "c2"), ("type", PStr "function"), ("function", PDict [("name", PStr "terminal"), ("arguments", PStr ("{\"command\": \"" <> cmd <> "\"}"))])]
      crContext (classifyCall ctx (shell "wc -c /private/var/folders/ab/T/g/fisher/ctx/s1/context.txt") "{\"output\": \"1\", \"exit_code\": 0}") `shouldBe` True
      crContext (classifyCall ctx (shell "lake build") "{\"output\": \"ok\", \"exit_code\": 0}") `shouldBe` False
    it "the file tools get every spelling" $
      pathAliases ctx `shouldBe` [ctx, ctx, "/private" <> ctx]
  describe "a batch with a write and any other call runs in order (Fisher only)" $ do
    let patchOn p = ("patch", PDict [("mode", PStr "replace"), ("path", PStr p), ("old_string", PStr "a"), ("new_string", PStr "b")])
        readOf p = ("read_file", PDict [("path", PStr p)])
        term = ("terminal", PDict [("command", PStr "lake build")])
    it "two patches on one file" $
      batchWritesSameFile "/w" [patchOn "/w/Foo.lean", patchOn "Foo.lean"] `shouldBe` True
    it "a read and a write of one file" $
      batchWritesSameFile "/w" [readOf "/private/var/x/Foo.lean", patchOn "/var/x/Foo.lean"] `shouldBe` True
    it "batches without a write stay parallel" $ do
      batchWritesSameFile "/w" [readOf "/w/A.lean", readOf "/w/A.lean"] `shouldBe` False
      batchWritesSameFile "/w" [term, readOf "/w/A.lean"] `shouldBe` False
      batchWritesSameFile "/w" [patchOn "/w/A.lean"] `shouldBe` False
    it "a write next to any other call, which may build, check or read it" $ do
      batchWritesSameFile "/w" [patchOn "/w/A.lean", term] `shouldBe` True
      batchWritesSameFile "/w" [patchOn "/w/A.lean", patchOn "/w/B.lean"] `shouldBe` True
      batchWritesSameFile "/w" [("write_file", PDict [("path", PStr "B.lean"), ("content", PStr "x")]), ("execute_code", PDict [("code", PStr "print(1)")])] `shouldBe` True
      batchWritesSameFile "/w" [patchOn "/w/A.lean", ("mcp_lean_lsp_lean_diagnostic_messages", PDict [("file_path", PStr "/w/A.lean")])] `shouldBe` True
  describe "a call written as text (Fisher only)" $ do
    it "is a line in the context file's call notation" $ do
      hasTextCall "Three left. Read them.\n[call read_file {\"path\": \"/w/A.lean\", \"offset\": 372}]" `shouldBe` True
      hasTextCall "  [call terminal {\"command\": \"lake build\"}]  " `shouldBe` True
    it "is not prose that mentions one" $ do
      hasTextCall "All 12 theorems are proved." `shouldBe` False
      hasTextCall "I used [call read_file] earlier." `shouldBe` False
      hasTextCall "[call ]" `shouldBe` False
  describe "the CLM defaults (Fisher only)" $ do
    it "rule 3 is off and the default guidance is tools/clm/skill.md" $ do
      ccKeepUnread defaultClmConfig `shouldBe` False
      f <- TE.decodeUtf8 <$> BS.readFile "../tools/clm/skill.md"
      defaultSkill `shouldBe` T.strip f
    it "the config turns rule 3 on and the guidance off" $ do
      ccKeepUnread (clmConfigFromPy (Just (PDict [("keep_unread", PBool True)]))) `shouldBe` True
      ccSkill (clmConfigFromPy Nothing) `shouldBe` defaultSkill
      clmConfigToPy defaultClmConfig `shouldBe` clmConfigToPy (clmConfigFromPy Nothing)
  describe "a provider's refusal (Fisher only)" $ do
    let resp msg = PDict [("choices", PList [PDict [("message", msg)]])]
    it "is read from the first choice's message" $ do
      responseRefusal (resp (PDict [("content", PNull), ("refusal", PStr " no. ")])) `shouldBe` Just "no."
      responseRefusal (resp (PDict [("content", PStr "ok"), ("refusal", PNull)])) `shouldBe` Nothing
      responseRefusal (resp (PDict [("refusal", PStr "")])) `shouldBe` Nothing
