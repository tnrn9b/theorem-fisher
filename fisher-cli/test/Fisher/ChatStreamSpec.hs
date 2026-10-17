-- | @fisher chat --output-format stream-json@ as the launch's native child
-- runs it (@-q@, not @-Q@): every stdout line is one JSON object and the
-- stream ends with one @result@ event; and the exit summary's tool calls,
-- each executed call counted once (Fisher only: upstream counts the tool
-- results plus the assistant messages that carry calls).
module Fisher.ChatStreamSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Cli.AgentChat (exitSummaryLines)
import Control.Exception (ErrorCall (..), throwIO)

import Fisher.Cli.AgentChat (ChatDeps (..))
import Fisher.ClmCliSpec (Run (..), capturingStderr, completion, runChatWith, runChatWithDeps)
import Fisher.LLM.Http (Transport (..))
import Fisher.LLM.Types (HttpReq (..))
import Fisher.Py.Value
import Fisher.Skin (defaultSkin)

call :: Text -> Text -> PyVal
call i name = PDict [("id", PStr i), ("type", PStr "function"), ("function", PDict [("name", PStr name), ("arguments", PStr "{\"path\": \"Math/Foo.lean\"}")])]

calling :: [PyVal] -> PyVal
calling cs = PDict [("role", PStr "assistant"), ("content", PStr "Reading."), ("tool_calls", PList cs)]

answer :: Text -> PyVal
answer t = PDict [("role", PStr "assistant"), ("content", PStr t)]

-- | Three model turns: two calls at once, then one, then the answer.
turns :: [PyVal]
turns = [calling [call "c1" "read_file", call "c2" "read_file"], calling [call "c3" "read_file"], answer "It builds."]

jsonLines :: Text -> [Either String PyVal]
jsonLines out = [either (Left . (T.unpack l <>) . (": " <>) . show) Right (loads (TE.encodeUtf8 l)) | l <- T.lines out]

resultEvents :: [PyVal] -> [PyVal]
resultEvents vs = [v | v <- vs, get "type" v == Just (PStr "result")]

spec :: Spec
spec = do
  describe "stream-json under -q" $ do
    let argv = ["-q", "Check that the project builds.", "--output-format", "stream-json"]
    it "prints only JSON objects on stdout and ends with one result event" $ withSystemTempDirectory "fisher-stream" $ \tmp -> do
      (r, err) <- capturingStderr tmp (runChatWith (map completion turns) tmp [] argv)
      rCode r `shouldBe` 0
      let parsed = jsonLines (rOut r)
      [e | Left e <- parsed] `shouldBe` []
      let vs = [v | Right v <- parsed]
      all (\case PDict _ -> True; _ -> False) vs `shouldBe` True
      length (resultEvents vs) `shouldBe` 1
      get "type" (last vs) `shouldBe` Just (PStr "result")
      let res = last vs
      get "subtype" res `shouldBe` Just (PStr "success")
      get "is_error" res `shouldBe` Just (PBool False)
      get "num_turns" res `shouldBe` Just (PInt 3)
      get "num_tool_calls" res `shouldBe` Just (PInt 3)
      get "result" res `shouldBe` Just (PStr "It builds.")
      (get "usage" res >>= get "input_tokens") `shouldBe` Just (PInt 300)
      -- the human text is on stderr: the exit summary among it
      err `shouldSatisfy` T.isInfixOf "(1 user, 3 tool calls)"

    it "ends with an error result when the run fails" $ withSystemTempDirectory "fisher-stream" $ \tmp -> do
      (r, _) <- capturingStderr tmp (runChatWith [] tmp [] argv)
      let parsed = jsonLines (rOut r)
      [e | Left e <- parsed] `shouldBe` []
      let vs = [v | Right v <- parsed]
      length (resultEvents vs) `shouldBe` 1
      get "type" (last vs) `shouldBe` Just (PStr "result")
      get "is_error" (last vs) `shouldBe` Just (PBool True)

    it "has the same shape under -p" $ withSystemTempDirectory "fisher-stream" $ \tmp -> do
      (r, _) <- capturingStderr tmp (runChatWith (map completion turns) tmp [] ["-p", "Check that the project builds.", "--output-format", "stream-json"])
      let parsed = jsonLines (rOut r)
      [e | Left e <- parsed] `shouldBe` []
      let vs = [v | Right v <- parsed]
      length (resultEvents vs) `shouldBe` 1
      get "num_tool_calls" (last vs) `shouldBe` Just (PInt 3)

  describe "-p without stream-json (Fisher only)" $ do
    it "prints only the answer and the session id on stdout, the progress on stderr" $ withSystemTempDirectory "fisher-print" $ \tmp -> do
      (r, err) <- capturingStderr tmp (runChatWith (map completion turns) tmp [] ["-p", "Check that the project builds."])
      rCode r `shouldBe` 0
      filter (not . T.null) (T.lines (rOut r)) `shouldBe` ["It builds.", "session_id: 20261006_101500_000000"]
      err `shouldSatisfy` T.isInfixOf "Math/Foo.lean"
    it "keeps upstream's stdout under -Q -q" $ withSystemTempDirectory "fisher-print" $ \tmp -> do
      (r, _) <- capturingStderr tmp (runChatWith (map completion turns) tmp [] ["-Q", "-q", "Check that the project builds."])
      rOut r `shouldSatisfy` T.isInfixOf "Math/Foo.lean"

  describe "the exit status (Fisher only: under -p or stream-json)" $ do
    let prompt = "Check that the project builds."
        -- every model call raises, which the loop does not catch
        raising d = d {cdTransport = (\t -> t {tRequest = \hr x -> if hrMethod hr == "POST" then throwIO (ErrorCall "boom") else tRequest t hr x}) <$> cdTransport d}
    it "is 0 when the run succeeds under -p" $ withSystemTempDirectory "fisher-exit" $ \tmp -> do
      (r, _) <- capturingStderr tmp (runChatWith (map completion turns) tmp [] ["-p", prompt])
      rCode r `shouldBe` 0
    it "is 1 when the run fails under -p" $ withSystemTempDirectory "fisher-exit" $ \tmp -> do
      (r, _) <- capturingStderr tmp (runChatWith [] tmp [] ["-p", prompt])
      rCode r `shouldBe` 1
    it "is 1 when the run fails under -p with stream-json, and so is the result" $ withSystemTempDirectory "fisher-exit" $ \tmp -> do
      (r, _) <- capturingStderr tmp (runChatWith [] tmp [] ["-p", prompt, "--output-format", "stream-json"])
      rCode r `shouldBe` 1
      let vs = [v | Right v <- jsonLines (rOut r)]
      get "is_error" (last vs) `shouldBe` Just (PBool True)
    it "is 1 when the run fails under -q with stream-json" $ withSystemTempDirectory "fisher-exit" $ \tmp -> do
      (r, _) <- capturingStderr tmp (runChatWith [] tmp [] ["-q", prompt, "--output-format", "stream-json"])
      rCode r `shouldBe` 1
    it "stays upstream's 0 under -Q -q" $ withSystemTempDirectory "fisher-exit" $ \tmp -> do
      (r, _) <- capturingStderr tmp (runChatWith [] tmp [] ["-Q", "-q", prompt])
      rCode r `shouldBe` 0
    it "ends the stream with an error result when the run raises under -p" $ withSystemTempDirectory "fisher-exit" $ \tmp -> do
      (r, _) <- capturingStderr tmp (runChatWithDeps raising (map completion turns) tmp [] ["-p", prompt, "--output-format", "stream-json"])
      let parsed = jsonLines (rOut r)
      [e | Left e <- parsed] `shouldBe` []
      let vs = [v | Right v <- parsed]
      length (resultEvents vs) `shouldBe` 1
      get "type" (last vs) `shouldBe` Just (PStr "result")
      get "is_error" (last vs) `shouldBe` Just (PBool True)
      fmap (T.isInfixOf "boom" . pyStrOf) (get "error" (last vs)) `shouldBe` Just True
      rCode r `shouldBe` 1

  describe "the exit summary's tool calls" $ do
    -- the shape of a recorded session: four calls over three assistant
    -- messages, so upstream's count is 7
    let tool i = PDict [("role", PStr "tool"), ("content", PStr "{\"content\": \"ok\"}"), ("tool_call_id", PStr i)]
        user = PDict [("role", PStr "user"), ("content", PStr "Prove it.")]
        recorded =
          [ user, calling [call "a" "read_file", call "b" "read_file"], tool "a", tool "b"
          , calling [call "c" "patch"], tool "c", calling [call "d" "terminal"], tool "d", answer "Done." ]
        summary h = last (exitSummaryLines defaultSkin "s1" 5 h)
    it "counts each executed call once" $
      summary recorded `shouldBe` "Messages:       9 (1 user, 4 tool calls)"
    it "leaves out the calls an interrupt skipped" $ do
      let skipped = PDict [("role", PStr "tool"), ("content", PStr "[Tool execution skipped \8212 patch was not started. User sent a new message]"), ("tool_call_id", PStr "e")]
          cancelled = PDict [("role", PStr "tool"), ("content", PStr "[Tool execution cancelled \8212 patch was skipped due to user interrupt]"), ("tool_call_id", PStr "f")]
      summary (recorded <> [calling [call "e" "patch", call "f" "patch"], skipped, cancelled])
        `shouldBe` "Messages:       12 (1 user, 4 tool calls)"
    it "leaves out the results the loop writes for a call to an unknown tool" $ do
      let unknown = PDict [("role", PStr "tool"), ("content", PStr "Tool 'reed_file' does not exist. Available tools: patch, read_file"), ("tool_call_id", PStr "g")]
          sibling = PDict [("role", PStr "tool"), ("content", PStr "Skipped: another tool call in this turn used an invalid name. Please retry this tool call."), ("tool_call_id", PStr "h")]
          -- a real result that happens to start the same way still counts
          real = PDict [("role", PStr "tool"), ("content", PStr "Tool 'x' output follows"), ("tool_call_id", PStr "i")]
      summary (recorded <> [calling [call "g" "reed_file", call "h" "read_file"], unknown, sibling, calling [call "i" "terminal"], real])
        `shouldBe` "Messages:       14 (1 user, 5 tool calls)"
