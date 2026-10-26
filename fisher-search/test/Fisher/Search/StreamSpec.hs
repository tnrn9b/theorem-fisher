module Fisher.Search.StreamSpec (spec) where

import Control.Exception (evaluate)
import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import System.FilePath ((</>))
import Test.Hspec

import Fisher.Search.Stream

fixture :: FilePath -> IO BS.ByteString
fixture name = BS.readFile ("test/fixtures/stream" </> name)

field :: Text -> StreamEvent -> Maybe Value
field k e = case stData e of
  Object o -> KM.lookup (K.fromText k) o
  _ -> Nothing

kinds :: [StreamEvent] -> [Text]
kinds = map stKind

of' :: Text -> [StreamEvent] -> [StreamEvent]
of' k = filter ((== k) . stKind)

spec :: Spec
spec = do
  describe "streamEvents claude-code" $ do
    it "maps text, tool use, tool result and the result line" $ do
      es <- streamEvents "claude-code" <$> fixture "claude.jsonl"
      kinds es `shouldBe` ["message", "tool_start", "tool_end", "message", "cost", "outcome"]
    it "fills each event's data" $ do
      es <- streamEvents "claude-code" <$> fixture "claude.jsonl"
      [m1, _] <- pure (of' "message" es)
      [ts] <- pure (of' "tool_start" es)
      [te] <- pure (of' "tool_end" es)
      [c] <- pure (of' "cost" es)
      [o] <- pure (of' "outcome" es)
      field "role" m1 `shouldBe` Just (String "assistant")
      field "text" m1 `shouldBe` Just (String "I will read the file first.")
      field "name" ts `shouldBe` Just (String "Read")
      field "call_id" ts `shouldBe` Just (String "toolu_01")
      field "args" ts `shouldBe` Just (Object (KM.fromList [("file_path", String "/work/p/Main.lean")]))
      field "ok" te `shouldBe` Just (Bool True)
      field "call_id" te `shouldBe` Just (String "toolu_01")
      field "name" te `shouldBe` Just (String "Read")
      field "result" te `shouldBe` Just (String "theorem main : 1 = 1 := by sorry")
      field "dollars" c `shouldBe` Just (Number 0.0123)
      field "input" c `shouldBe` Just (Number 1500)
      field "output" c `shouldBe` Just (Number 200)
      field "outcome" o `shouldBe` Just (String "completed")
      map stT es `shouldBe` map (const Nothing) es
    it "a failed tool and a string result" $ do
      let l = "{\"type\":\"user\",\"message\":{\"content\":[{\"type\":\"tool_result\",\"tool_use_id\":\"toolu_9\",\"content\":\"no such file\",\"is_error\":true}]}}"
      [te] <- pure (streamEvents "claude-code" l)
      field "ok" te `shouldBe` Just (Bool False)
      field "result" te `shouldBe` Just (String "no such file")
      field "name" te `shouldBe` Just Null
    it "an error result is an error outcome" $ do
      let l = "{\"type\":\"result\",\"subtype\":\"error_max_turns\",\"is_error\":true,\"session_id\":\"s\"}"
      [field "outcome" e | e <- of' "outcome" (streamEvents "claude-code" l)] `shouldBe` [Just (String "error")]
  describe "streamEvents codex" $ do
    it "gives one tool start and end pair for a command" $ do
      es <- streamEvents "codex" <$> fixture "codex.jsonl"
      kinds (filter ((`elem` ["tool_start", "tool_end"]) . stKind) es) `shouldBe` ["tool_start", "tool_end"]
      [ts] <- pure (of' "tool_start" es)
      [te] <- pure (of' "tool_end" es)
      field "call_id" ts `shouldBe` Just (String "item_1")
      field "call_id" te `shouldBe` Just (String "item_1")
      field "ok" te `shouldBe` Just (Bool True)
      field "result" te `shouldBe` Just (String "Build completed successfully.")
    it "the agent message and the turn's usage" $ do
      es <- streamEvents "codex" <$> fixture "codex.jsonl"
      kinds es `shouldBe` ["tool_start", "tool_end", "message", "cost"]
      [c] <- pure (of' "cost" es)
      field "input" c `shouldBe` Just (Number 900)
      field "output" c `shouldBe` Just (Number 60)
      field "dollars" c `shouldBe` Just Null
    it "a started command is not started twice" $ do
      let ls =
            BC.unlines
              [ "{\"type\":\"item.started\",\"item\":{\"id\":\"c1\",\"type\":\"command_execution\",\"command\":\"ls\",\"aggregated_output\":\"\",\"exit_code\":null,\"status\":\"in_progress\"}}"
              , "{\"type\":\"item.completed\",\"item\":{\"id\":\"c1\",\"type\":\"command_execution\",\"command\":\"ls\",\"aggregated_output\":\"x\",\"exit_code\":2,\"status\":\"failed\"}}"
              ]
          es = streamEvents "codex" ls
      kinds es `shouldBe` ["tool_start", "tool_end"]
      [field "ok" e | e <- of' "tool_end" es] `shouldBe` [Just (Bool False)]
    it "a command without an id gets a synthetic one" $ do
      let ls =
            BC.unlines
              [ "{\"type\":\"item.started\",\"item\":{\"type\":\"command_execution\",\"command\":\"ls\",\"status\":\"in_progress\"}}"
              , "{\"type\":\"item.completed\",\"item\":{\"type\":\"command_execution\",\"command\":\"ls\",\"aggregated_output\":\"a\",\"exit_code\":0}}"
              , "{\"type\":\"item.completed\",\"item\":{\"type\":\"command_execution\",\"command\":\"pwd\",\"aggregated_output\":\"/\",\"exit_code\":0}}"
              ]
          es = streamEvents "codex" ls
      kinds es `shouldBe` ["tool_start", "tool_end", "tool_start", "tool_end"]
      [field "call_id" e | e <- es] `shouldBe` map (Just . String) ["codex-1", "codex-1", "codex-2", "codex-2"]
  describe "streamEvents fisher (native)" $
    it "gives the tool start and the cost" $ do
      es <- streamEvents "fisher" <$> fixture "native.jsonl"
      kinds es `shouldBe` ["tool_start", "cost"]
      [field "dollars" e | e <- of' "cost" es] `shouldBe` [Just (Number 0.002)]
  describe "streamEvents on damaged input" $ do
    it "the truncated fixture gives the events before the cut" $ do
      es <- streamEvents "claude-code" <$> fixture "truncated.jsonl"
      _ <- evaluate (length (show (map stData es)))
      kinds es `shouldBe` ["message", "tool_start", "tool_end", "message"]
    it "skips undecodable lines anywhere and never throws" $ do
      let ls =
            BC.unlines
              [ "not json"
              , "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"a\"}]}}"
              , "\255\254{"
              , "[1,2]"
              , "{\"type\":\"assistant\",\"message\":{\"content\":\"plain\"}}"
              , "{\"type\":\"user\",\"message\":{\"content\":[{\"type\":\"tool_result\"}]}}"
              , "{\"type\":\"result\",\"usage\":7}"
              , "{\"type\":\"item.completed\",\"item\":3}"
              , "{\"type\":"
              ]
      mapM_ (\b -> evaluate (length (show [(stKind e, stData e) | e <- streamEvents b ls]))) ["claude-code", "codex", "fisher", "other"]
      take 1 (kinds (streamEvents "claude-code" ls)) `shouldBe` ["message"]
      streamEvents "claude-code" "" `shouldBe` []
  describe "streamSessionId" $ do
    it "reads the result line's session id" $
      (streamSessionId <$> fixture "claude.jsonl") `shouldReturn` Just "sess-1"
    it "falls back to the init line when the result is cut" $
      (streamSessionId <$> fixture "truncated.jsonl") `shouldReturn` Just "sess-1"
    it "is Nothing for a stream without one" $
      (streamSessionId <$> fixture "codex.jsonl") `shouldReturn` Nothing
