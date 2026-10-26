module Fisher.Learn.MistakesSpec (spec) where

import Control.Exception (evaluate)
import Data.Aeson (Value (..), eitherDecodeStrict, object, withObject, (.:), (.=))
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>))
import Test.Hspec

import Fisher.Learn.Mistakes

-- | One attempt's events from a fixture file of run-log event lines.
fixture :: FilePath -> IO [(Int, Double, Text, Value)]
fixture name = do
  bs <- BS.readFile ("test/fixtures/mistakes" </> name)
  either fail pure (mapM line (filter (not . BS.null) (BC.lines bs)))
  where
    line l = eitherDecodeStrict l >>= parseEither (withObject "event" (\o -> (,,,) <$> o .: "seq" <*> o .: "t" <*> o .: "kind" <*> o .: "data"))

view :: [(Int, Double, Text, Value)] -> Text -> Text -> AttemptView
view es outcome cls = AttemptView {avEvents = es, avOutcomeText = outcome, avClass = cls, avGoal = "foo", avShape = "arith"}

classifyFixture :: FilePath -> Text -> Text -> IO [Mistake]
classifyFixture name outcome cls = do
  es <- fixture name
  pure (classify (view es outcome cls))

kindsOf :: [Mistake] -> [MistakeKind]
kindsOf = map mKind

-- | Events built in the test: a tool call and its end.
call :: Int -> Text -> Value -> Text -> [(Int, Double, Text, Value)]
call s name args result =
  [ (s, fromIntegral s, "tool_start", object ["name" .= name, "args" .= args, "call_id" .= cid])
  , (s + 1, fromIntegral s + 0.5, "tool_end", object ["name" .= name, "call_id" .= cid, "ok" .= True, "ms" .= (5 :: Int), "result" .= result, "preview" .= T.take 20 result])
  ]
  where cid = "c" <> T.pack (show s)

diagResult :: Text -> Text
diagResult msg = "{\"success\":false,\"timed_out\":false,\"items\":[{\"severity\":\"error\",\"message\":\"" <> msg <> "\",\"line\":1,\"column\":0,\"category\":null,\"hint\":null}]}"

forced :: [Mistake] -> IO [Mistake]
forced ms = evaluate (length (show ms)) >> pure ms

spec :: Spec
spec = do
  describe "one fixture per kind" $ do
    it "unknown_identifier carries the name, and two equal diagnostics give one hit" $ do
      ms <- classifyFixture "unknown_identifier.jsonl" "" "failed"
      map (\m -> (mKind m, mName m)) ms `shouldBe` [(UnknownIdentifier, Just "Nat.foo_bar")]
      map mSeq ms `shouldBe` [3]
    it "deprecated carries the old and the new name" $ do
      ms <- classifyFixture "deprecated.jsonl" "" "failed"
      map (\m -> (mKind m, mName m, mNew m)) ms `shouldBe` [(Deprecated, Just "Array.le_trans'", Just "Array.le_trans")]
    it "lean3_syntax carries the marker" $ do
      ms <- classifyFixture "lean3_syntax.jsonl" "" "failed"
      map (\m -> (mKind m, mName m)) ms `shouldBe` [(Lean3Syntax, Just "λ x,")]
    it "sorry_left from the outcome" $ do
      ms <- classifyFixture "sorry_left.jsonl" "rejected: still sorry" "failed"
      kindsOf ms `shouldBe` [SorryLeft]
      kindsOf <$> classifyFixture "sorry_left.jsonl" "rejected: the proof uses sorry" "failed" `shouldReturn` [SorryLeft]
    it "statement_changed from the outcome" $ do
      ms <- classifyFixture "statement_changed.jsonl" "rejected: the statement elaborates to a different type" "failed"
      kindsOf ms `shouldBe` [StatementChanged]
    it "axiom_used carries the first axiom" $ do
      ms <- classifyFixture "axiom_used.jsonl" "rejected: axioms beyond the allowed: Classical.choice, propext" "failed"
      map (\m -> (mKind m, mName m)) ms `shouldBe` [(AxiomUsed, Just "Classical.choice")]
    it "timeout_loop after three timed-out run_code or multi_attempt calls" $ do
      ms <- classifyFixture "timeout_loop.jsonl" "" "failed"
      kindsOf ms `shouldBe` [TimeoutLoop]
      map mSeq ms `shouldBe` [6]
    it "repeated_call on the same tool with the same args three times" $ do
      ms <- classifyFixture "repeated_call.jsonl" "" "failed"
      kindsOf ms `shouldBe` [RepeatedCall]
      map mSeq ms `shouldBe` [5]
    it "no_check_before_finish when no check follows the last write" $ do
      ms <- classifyFixture "no_check_before_finish.jsonl" "" "failed"
      kindsOf ms `shouldBe` [NoCheckBeforeFinish]
      map mSeq ms `shouldBe` [3]
    it "budget_burn on a stalled attempt that wrote nothing" $ do
      ms <- classifyFixture "budget_burn.jsonl" "" "stalled"
      kindsOf ms `shouldBe` [BudgetBurn]
      kindsOf <$> classifyFixture "budget_burn.jsonl" "" "failed" `shouldReturn` []
    it "a clean closed attempt with a check after its write gives nothing" $
      classifyFixture "negatives.jsonl" "closed" "closed" `shouldReturn` []

  describe "robustness" $ do
    it "a tool failure text gives no hit" $ do
      ms <- forced (classify (view (call 1 "mcp_lean_lsp_lean_run_code" (object ["code" .= ("x" :: Text)]) "Error executing tool lean_run_code: boom") "" "failed"))
      ms `shouldBe` []
    it "a 70000-character non-JSON result gives no hit and no exception" $ do
      let big = "{\"items\": [" <> T.replicate 70000 "a"
      ms <- forced (classify (view (call 1 "mcp_lean_lsp_lean_diagnostic_messages" Null big <> call 3 "lean_run_code" (String big) big) "" "failed"))
      ms `shouldBe` []
    it "missing fields and odd data shapes do not throw" $ do
      let es =
            [ (1, 0, "tool_end", Null), (2, 0, "tool_start", String "x"), (3, 0, "tool_end", object ["name" .= (3 :: Int)])
            , (4, 0, "tool_end", object ["name" .= ("lean_run_code" :: Text), "result" .= object []])
            , (5, 0, "message", Array mempty)
            , (6, 0, "tool_end", object ["name" .= ("lean_build" :: Text), "result" .= ("unknown identifier '" :: Text)])
            , (7, 0, "tool_start", object ["name" .= ("write_file" :: Text)]) ]
      ms <- forced (classify (view es "rejected: axioms beyond the allowed: " "failed"))
      kindsOf ms `shouldBe` [NoCheckBeforeFinish, AxiomUsed]
    it "matches the tool name by suffix for both prefixes" $
      mapM_
        ( \n -> do
            let ms = classify (view (call 1 n Null (diagResult "unknown identifier 'Nat.foo'")) "" "failed")
            map (\m -> (mKind m, mName m)) ms `shouldBe` [(UnknownIdentifier, Just "Nat.foo")]
        )
        ["mcp__lean-lsp__lean_run_code", "mcp_lean_lsp_lean_run_code"]
    it "two unknown identifier diagnostics of one name give one hit" $ do
      let es = call 1 "lean_diagnostic_messages" Null (diagResult "unknown identifier 'Nat.foo'") <> call 3 "lean_run_code" Null (diagResult "unknown identifier 'Nat.foo'")
      map mName (classify (view es "" "failed")) `shouldBe` [Just "Nat.foo"]
    it "scans a non-JSON result as text, and unknown constant counts" $ do
      let es = call 1 "lean_build" Null "error: Foo.lean:3:2: unknown constant 'Foo.bar'\nerror: build failed"
      map (\m -> (mKind m, mName m)) (classify (view es "" "failed")) `shouldBe` [(UnknownIdentifier, Just "Foo.bar")]
    it "keeps a trailing prime of a name" $ do
      let es = call 1 "lean_run_code" Null (diagResult "unknown identifier 'le_trans''")
      map mName (classify (view es "" "failed")) `shouldBe` [Just "le_trans'"]

  describe "lean 3 markers" $ do
    let marker code = [n | Mistake Lean3Syntax n _ _ <- classify (view (call 1 "write_file" (object ["path" .= ("A.lean" :: Text), "content" .= (code :: Text)]) "ok" <> call 3 "lean_build" Null "ok") "" "failed")]
    it "finds each marker" $ do
      marker "theorem a : P :=\nbegin\n  simp\nend" `shouldBe` [Just "begin ... end"]
      marker "  assume h," `shouldBe` [Just "assume"]
      marker "  cases h with a b," `shouldBe` [Just "cases h with"]
      marker "#check nat.succ_le_iff" `shouldBe` [Just "#check nat."]
    it "leaves Lean 4 forms alone" $ do
      marker "example := fun ⟨a, b⟩ => a" `shouldBe` []
      marker "example := λ ⟨a, b⟩ ↦ a" `shouldBe` []
      marker "  cases h with\n  | inl h => exact h" `shouldBe` []
      marker "  rcases h with ⟨a, b⟩" `shouldBe` []
      marker "  cases' h with a b" `shouldBe` []
      marker "#check Nat.succ_le_iff\nend Foo" `shouldBe` []
      marker "theorem a : P := by {\n  simp }" `shouldBe` []
      marker "theorem a : True := by { trivial }" `shouldBe` []
    it "does not read the text an Edit replaces" $ do
      let es = call 1 "Edit" (object ["file_path" .= ("A.lean" :: Text), "old_string" .= ("begin\n simp\nend" :: Text), "new_string" .= ("by simp" :: Text)]) "ok" <> call 3 "lean_build" Null "ok"
      classify (view es "" "failed") `shouldBe` []

  describe "false positives the review probed" $ do
    let w = call 1 "Write" (object ["file_path" .= ("A.lean" :: Text), "content" .= ("x" :: Text)]) "ok"
        chk = call 3 "lean_diagnostic_messages" (object ["file_path" .= ("A.lean" :: Text)]) "{\"items\":[]}"
        todo s = call s "TodoWrite" (object ["todos" .= ([] :: [Text])]) "ok"
        named ms = map (\m -> (mKind m, mName m)) ms
    it "TodoWrite is not a write" $ do
      classify (view (w <> chk <> todo 5) "" "failed") `shouldBe` []
      kindsOf (classify (view (todo 1) "" "stalled")) `shouldBe` [BudgetBurn]
    it "a local name is not an unknown identifier" $ do
      classify (view (call 1 "lean_run_code" Null (diagResult "unknown identifier 'h'")) "" "failed") `shouldBe` []
      let unknown x = map mName (classify (view (call 1 "lean_run_code" Null (diagResult ("unknown identifier '" <> x <> "'"))) "" "failed"))
      mapM_ (\x -> unknown x `shouldBe` []) ["h_pos", "hx_le", "ih_n", "this_1", "hk'", "hn_succ'"]
      unknown "half_pos" `shouldBe` [Just "half_pos"]
      unknown "hx.le" `shouldBe` [Just "hx.le"]
    it "keeps the prime before other punctuation" $ do
      map mName (classify (view (call 1 "lean_build" Null "error: unknown identifier 'le_foo'': foo") "" "failed")) `shouldBe` [Just "le_foo'"]
      map mName (classify (view (call 1 "lean_build" Null "unknown identifier 'le_trans''.") "" "failed")) `shouldBe` [Just "le_trans'"]
    it "a #check probe is not an unknown identifier" $
      classify (view (call 1 "lean_run_code" (object ["code" .= ("#check Nat.foo" :: Text)]) (diagResult "unknown constant 'Nat.foo'")) "" "failed") `shouldBe` []
    it "a multi_attempt where another snippet worked is not an unknown identifier" $ do
      let multi r = call 1 "lean_multi_attempt" (object ["snippets" .= (["exact Nat.foo", "simp"] :: [Text])]) r
          diagOf m = "[{\"severity\":\"error\",\"message\":\"" <> m <> "\"}]"
          item sn ds = "{\"snippet\":\"" <> sn <> "\",\"goals\":[],\"diagnostics\":" <> ds <> "}"
      classify (view (multi ("{\"items\":[" <> item "exact Nat.foo" (diagOf "unknown identifier 'Nat.foo'") <> "," <> item "simp" "[]" <> "]}")) "" "closed") `shouldBe` []
      named (classify (view (multi ("{\"items\":[" <> item "exact Nat.foo" (diagOf "unknown identifier 'Nat.foo'") <> "," <> item "simp" (diagOf "simp made no progress") <> "]}")) "" "failed"))
        `shouldBe` [(UnknownIdentifier, Just "Nat.foo")]
    it "a deprecation the attempt's code does not use is not counted" $
      classify (view (call 1 "lean_build" Null "{\"success\":true,\"diagnostics\":[{\"severity\":\"warning\",\"message\":\"`Other.thing` has been deprecated: use `Other.thing2` instead\"}]}") "" "failed") `shouldBe` []
    it "an attempt cut off right after a write is not blamed for the check" $
      classify (view w "" "timeout") `shouldBe` []
    it "Lean's heartbeat timeout counts as a timeout" $ do
      let hb s = call s "lean_run_code" (object ["code" .= ("example := by simp " <> T.pack (show s) :: Text)]) (diagResult "(deterministic) timeout at `whnf`, maximum number of heartbeats (200000) has been reached")
      kindsOf (classify (view (concatMap hb [1, 3, 5]) "" "failed")) `shouldBe` [TimeoutLoop]
    it "a failed write is not a write" $ do
      let failedEdit = [(1, 1, "tool_start", object ["name" .= ("Edit" :: Text), "args" .= object [], "call_id" .= ("x" :: Text)]), (2, 2, "tool_end", object ["name" .= ("Edit" :: Text), "call_id" .= ("x" :: Text), "ok" .= False, "result" .= ("old_string not found" :: Text)])]
      kindsOf (classify (view failedEdit "" "stalled")) `shouldBe` [BudgetBurn]

  describe "toolCalls" $ do
    it "pairs a native start with no call_id to the next end of its name" $ do
      let start s a = (s, fromIntegral s, "tool_start", object ["name" .= ("lean_run_code" :: Text), "args" .= object ["code" .= (a :: Text)]])
          end s i = (s, fromIntegral s, "tool_end", object ["name" .= ("lean_run_code" :: Text), "call_id" .= (i :: Text), "result" .= ("ok" :: Text)])
          cs = toolCalls [start 1 "a", end 2 "k1", start 3 "b", end 4 "k2"]
      map (\c -> (tcSeq c, fmap fst (tcEnd c))) cs `shouldBe` [(1, Just 2), (3, Just 4)]
    it "pairs by call_id when the start carries one, in any order" $ do
      let start s i = (s, fromIntegral s, "tool_start", object ["name" .= ("lean_run_code" :: Text), "args" .= object [], "call_id" .= (i :: Text)])
          end s i = (s, fromIntegral s, "tool_end", object ["name" .= ("lean_run_code" :: Text), "call_id" .= (i :: Text), "result" .= ("ok" :: Text)])
          cs = toolCalls [start 1 "k1", start 2 "k2", end 3 "k2", end 4 "k1"]
      map (\c -> (tcSeq c, fmap fst (tcEnd c))) cs `shouldBe` [(1, Just 4), (2, Just 3)]
    it "pairs no end when several starts of the name with no call_id are open" $ do
      let start s a = (s, fromIntegral s, "tool_start", object ["name" .= ("lean_run_code" :: Text), "args" .= object ["code" .= (a :: Text)]])
          end s i = (s, fromIntegral s, "tool_end", object ["name" .= ("lean_run_code" :: Text), "call_id" .= (i :: Text), "result" .= ("ok" :: Text)])
          cs = toolCalls [start 1 "a", start 2 "b", end 3 "k2", end 4 "k1", start 5 "c", end 6 "k3"]
      map (\c -> (tcSeq c, isNothingArgs c, fmap fst (tcEnd c))) cs
        `shouldBe` [(1, False, Nothing), (2, False, Nothing), (3, True, Just 3), (4, True, Just 4), (5, False, Just 6)]
    it "a concurrent #check probe is not paired with the wrong call" $ do
      -- two native lean_run_code calls at once; the probe's end comes back second
      let es =
            [ (1, 1, "tool_start", object ["name" .= ("mcp_lean_lsp_lean_run_code" :: Text), "args" .= object ["code" .= ("#check Nat.foo" :: Text)]])
            , (2, 2, "tool_start", object ["name" .= ("mcp_lean_lsp_lean_run_code" :: Text), "args" .= object ["code" .= ("example := Nat.foo" :: Text)]])
            , (3, 3, "tool_end", object ["name" .= ("mcp_lean_lsp_lean_run_code" :: Text), "call_id" .= ("k2" :: Text), "result" .= diagResult "unknown identifier 'Nat.foo'"])
            , (4, 4, "tool_end", object ["name" .= ("mcp_lean_lsp_lean_run_code" :: Text), "call_id" .= ("k1" :: Text), "result" .= diagResult "unknown constant 'Nat.foo'"]) ]
      map (\m -> (mKind m, mName m)) (classify (view es "" "failed")) `shouldBe` [(UnknownIdentifier, Just "Nat.foo")]
    it "an end with no start is a call with no args" $ do
      let cs = toolCalls [(5, 5, "tool_end", object ["name" .= ("lean_build" :: Text), "call_id" .= ("z" :: Text), "result" .= ("ok" :: Text)])]
      map (\c -> (tcSeq c, tcName c, isNothingArgs c, fmap fst (tcEnd c))) cs `shouldBe` [(5, "lean_build", True, Just 5)]
    it "a native failed write is seen through the pairing" $ do
      let es = [(1, 1, "tool_start", object ["name" .= ("patch" :: Text), "args" .= object ["path" .= ("A.lean" :: Text)]]), (2, 2, "tool_end", object ["name" .= ("patch" :: Text), "call_id" .= ("k9" :: Text), "ok" .= False, "result" .= ("no match" :: Text)])]
      kindsOf (classify (view es "" "stalled")) `shouldBe` [BudgetBurn]

  describe "smt_check" $ do
    let smt s res = call s "smt_check" (object ["query" .= ("(assert (> x " <> T.pack (show s) <> "))" :: Text)]) ("{\"result\": \"" <> res <> "\", \"model\": [], \"notes\": []}")
    it "smt_unknown_loop on three unknowns" $ do
      kindsOf (classify (view (smt 1 "unknown" <> smt 3 "unknown" <> smt 5 "unknown") "" "failed")) `shouldBe` [SmtUnknownLoop]
      map mName (classify (view (smt 1 "unknown" <> smt 3 "error" <> smt 5 "unknown") "" "failed")) `shouldBe` [Nothing]
      map mSeq (classify (view (smt 1 "error" <> smt 3 "error" <> smt 5 "error" <> smt 7 "unknown") "" "failed")) `shouldBe` [6]
    it "not on two" $
      classify (view (smt 1 "unknown" <> smt 3 "unknown") "" "failed") `shouldBe` []
    it "not when a sat breaks the run" $ do
      classify (view (smt 1 "unknown" <> smt 3 "unknown" <> smt 5 "sat" <> smt 7 "unknown") "" "failed") `shouldBe` []
      classify (view (smt 1 "unknown" <> smt 3 "unsat" <> smt 5 "unknown" <> smt 7 "error") "" "failed") `shouldBe` []

  describe "repeated calls" $
    it "a check repeated after each write is not a repeat" $ do
      let diag s = call s "lean_diagnostic_messages" (object ["file_path" .= ("A.lean" :: Text)]) "{\"items\":[]}"
          write s c = call s "Write" (object ["file_path" .= ("A.lean" :: Text), "content" .= (c :: Text)]) "ok"
          es = write 1 "a" <> diag 3 <> write 5 "b" <> diag 7 <> write 9 "c" <> diag 11
      classify (view es "" "failed") `shouldBe` []

  describe "mistakeJson" $
    it "writes the mistake event data" $
      mistakeJson "g" "sh" (Mistake Deprecated (Just "a") (Just "b") 4)
        `shouldBe` object ["kind" .= ("deprecated" :: Text), "name" .= ("a" :: Text), "new" .= ("b" :: Text), "seq" .= (4 :: Int), "goal" .= ("g" :: Text), "shape" .= ("sh" :: Text)]

  describe "renderMistakeKind" $
    it "gives the snake_case names and parses them back" $ do
      map renderMistakeKind [minBound .. maxBound]
        `shouldBe` ["unknown_identifier", "deprecated", "lean3_syntax", "sorry_left", "statement_changed", "axiom_used", "timeout_loop", "repeated_call", "no_check_before_finish", "budget_burn", "smt_unknown_loop"]
      map (parseMistakeKind . renderMistakeKind) [minBound .. maxBound] `shouldBe` map Just [minBound .. maxBound :: MistakeKind]

isNothingArgs :: ToolCall -> Bool
isNothingArgs c = case tcArgs c of
  Nothing -> True
  Just _ -> False
