{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-- | Golden tests of the RLM offload cut, the tail cut and the fixed texts.
module Fisher.Agent.RlmOffloadSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Fisher.Agent.Context.Rlm
import Fisher.Py.Value

-- | Message n: odd n a user message, even n an assistant message.
plain :: Int -> PyVal
plain n = obj [("role", PStr (if odd n then "user" else "assistant")), ("content", PStr ("m" <> T.pack (show n)))]

plains :: [Int] -> [PyVal]
plains = map plain

calls :: Text -> [(Text, Text)] -> PyVal
calls txt cs =
  obj
    [ ("role", PStr "assistant")
    , ("content", PStr txt)
    , ( "tool_calls"
      , PList [obj [("id", PStr i), ("type", PStr "function"), ("function", obj [("name", PStr nm), ("arguments", PStr "{\"x\": 1}")])] | (i, nm) <- cs] ) ]

result :: Text -> Text -> PyVal
result i c = obj [("role", PStr "tool"), ("tool_call_id", PStr i), ("content", PStr c)]

contents :: [PyVal] -> [Text]
contents = map (maybe "" pyStrOf . get "content")

idxs :: RlmState -> [Int]
idxs = map stIndex . rsStore

spec :: Spec
spec = do
  describe "offload" $ do
    it "moves the middle of 12 plain messages behind a stub" $ do
      let Just o = offload 3 4 emptyRlmState (plains [1 .. 12])
          ms = ofMessages o
      length ms `shouldBe` 8
      take 3 ms `shouldBe` plains [1 .. 3]
      ms !! 3 `shouldBe` stubMessage (4, 8) 5 (sum (map (T.length . renderTurn) (rsStore (ofState o))))
      drop 4 ms `shouldBe` plains [9 .. 12]
      idxs (ofState o) `shouldBe` [4 .. 8]
      rsRange (ofState o) `shouldBe` Just (4, 8)
      rsOffloads (ofState o) `shouldBe` 1
      ofMoved o `shouldBe` (4, 8)
      map stMessage (rsStore (ofState o)) `shouldBe` plains [4 .. 8]
    it "extends the head past the tool results of a call at its end" $ do
      let h = plains [1, 2] <> [calls "c" [("a", "t1"), ("b", "t2")], result "a" "ra", result "b" "rb"] <> plains [6 .. 14]
          Just o = offload 3 4 emptyRlmState h
      take 5 (ofMessages o) `shouldBe` take 5 h
      idxs (ofState o) `shouldBe` [6 .. 10]
      isStub (ofMessages o !! 5) `shouldBe` True
    it "moves the tail start back to the call its results answer" $ do
      let h = plains [1 .. 7] <> [calls "c" [("a", "t1"), ("b", "t2")], result "a" "ra", result "b" "rb"] <> plains [11, 12]
          Just o = offload 3 4 emptyRlmState h
      drop 4 (ofMessages o) `shouldBe` drop 7 h
      idxs (ofState o) `shouldBe` [4 .. 7]
    it "names tool results from the call in the middle" $ do
      let h = plains [1 .. 3] <> [calls "c" [("a", "grep")], result "a" "ra"] <> plains [6 .. 12]
          Just o = offload 3 4 emptyRlmState h
      map stTool (rsStore (ofState o)) `shouldBe` [Nothing, Just "grep", Nothing, Nothing, Nothing]
    it "uses unknown for a result with no call" $ do
      let h = plains [1 .. 4] <> [result "zz" "r"] <> plains [6 .. 12]
          Just o = offload 3 4 emptyRlmState h
      map stTool (rsStore (ofState o)) `shouldContain` [Just "unknown"]
    it "a second offload merges into one stub with exact indices" $ do
      let Just o1 = offload 3 4 emptyRlmState (plains [1 .. 12])
          h2 = ofMessages o1 <> plains [13 .. 22]
          Just o2 = offload 3 4 (ofState o1) h2
          ms = ofMessages o2
      length (filter isStub ms) `shouldBe` 1
      rsRange (ofState o2) `shouldBe` Just (4, 18)
      rsOffloads (ofState o2) `shouldBe` 2
      idxs (ofState o2) `shouldBe` [4 .. 18]
      ofMoved o2 `shouldBe` (9, 18)
      take 3 ms `shouldBe` plains [1 .. 3]
      drop 4 ms `shouldBe` plains [19 .. 22]
      length ms `shouldBe` 8
      T.isInfixOf "Turns 4 to 18 of this session (15 messages," (head (contents [ms !! 3])) `shouldBe` True
    it "lists as new only the turns not already stored by a tail cut" $ do
      let big = result "a" (T.replicate 30000 "x")
          (h', st) = cutTail emptyRlmState (plains [1 .. 3] <> [calls "c" [("a", "cat")], big])
          Just o = offload 3 4 st (h' <> plains [6 .. 12])
      ofMoved o `shouldBe` (4, 8)
      map stIndex (ofNew o) `shouldBe` [4, 6, 7, 8]
      idxs (ofState o) `shouldBe` [4, 5, 6, 7, 8]
      stMessage (rsStore (ofState o) !! 1) `shouldBe` big
    it "gives Nothing when the middle is empty" $
      fmap (const ()) (offload 3 4 emptyRlmState (plains [1 .. 7])) `shouldBe` Nothing
  describe "cutTail" $ do
    it "cuts a 50,000 character tool result in the tail and stores the full message" $ do
      let big = result "a" (T.replicate 50000 "x")
          h = plains [1 .. 3] <> [calls "c" [("a", "cat")], big]
          (h', st') = cutTail emptyRlmState h
          Just c = get "content" (last h')
      take 4 h' `shouldBe` take 4 h
      case c of
        PStr s -> do
          T.take 2000 s `shouldBe` T.replicate 2000 "x"
          T.drop 2000 s `shouldBe` "\n... (48,000 more characters; the full output is in recall's store as turn 5)"
        _ -> expectationFailure "content is not a string"
      map stIndex (rsStore st') `shouldBe` [5]
      stMessage (head (rsStore st')) `shouldBe` big
      stTool (head (rsStore st')) `shouldBe` Just "cat"
    it "leaves short results and results outside the last 4 alone" $ do
      let big = result "a" (T.replicate 30000 "y")
          h = [big] <> plains [2 .. 6]
      cutTail emptyRlmState h `shouldBe` (h, emptyRlmState)
  describe "texts" $ do
    it "stubMessage matches the spec example" $
      get "content" (stubMessage (4, 37) 34 212400)
        `shouldBe` Just (PStr "Turns 4 to 37 of this session (34 messages, 212,400 characters) were moved out of the context to save space. Nothing was summarised or lost. To get anything from them, such as an earlier tool output, a decision, or a proof attempt, call recall with a specific question.")
    it "isStub recognises the stub only" $ do
      isStub (stubMessage (4, 8) 5 100) `shouldBe` True
      isStub (plain 1) `shouldBe` False
    it "renders turns" $ do
      renderTurn (StoredTurn 7 "user" Nothing (plain 1)) `shouldBe` "[turn 7, user]\nm1"
      renderTurn (StoredTurn 8 "tool" (Just "grep") (result "a" "out")) `shouldBe` "[turn 8, tool grep]\nout"
      renderTurn (StoredTurn 9 "assistant" Nothing (calls "go" [("a", "grep")]))
        `shouldBe` "[turn 9, assistant]\ngo\ncall grep {\"x\": 1}"
    it "recallSchema names recall with one required question" $ do
      getPath ["function", "name"] recallSchema `shouldBe` Just (PStr "recall")
      getPath ["function", "parameters", "required"] recallSchema `shouldBe` Just (PList [PStr "question"])
  describe "liveHistory" $ do
    it "gives the head, the stub and the messages after the stored range" $ do
      let full = plains [1 .. 40]
          st = RlmState [StoredTurn i (if odd i then "user" else "assistant") Nothing (plain i) | i <- [4 .. 30]] 1 (Just (4, 30))
          ms = liveHistory st full
      take 3 ms `shouldBe` plains [1 .. 3]
      isStub (ms !! 3) `shouldBe` True
      drop 4 ms `shouldBe` plains [31 .. 40]
      liveHistory emptyRlmState full `shouldBe` full
    it "re-applies a tail cut's preview when nothing was offloaded" $ do
      let big = result "a" (T.replicate 50000 "x")
          h = plains [1 .. 3] <> [calls "c" [("a", "cat")], big]
          (h', st') = cutTail emptyRlmState h
      liveHistory st' h `shouldBe` h'
    it "re-applies the preview to cut turns in the head and after the stored range, not inside it" $ do
      let big i = result i (T.replicate 30000 i)
          h0 = plains [1, 2] <> [calls "c" [("p", "cat")], big "p"]
          (h0', st0) = cutTail emptyRlmState h0
          h1 = h0' <> plains [5 .. 12]
          Just o = offload 4 4 st0 h1
          h2 = ofMessages o <> [calls "d" [("q", "cat")], big "q"]
          (h2', st2) = cutTail (ofState o) h2
          full = h0 <> plains [5 .. 12] <> [calls "d" [("q", "cat")], big "q"]
      idxs st2 `shouldBe` [4, 5, 6, 7, 8, 14]
      rsRange st2 `shouldBe` Just (5, 8)
      liveHistory st2 full `shouldBe` h2'
  describe "state codec" $ do
    it "round-trips" $ do
      let Just o = offload 3 4 emptyRlmState (plains [1 .. 3] <> [calls "c" [("a", "grep")], result "a" "r"] <> plains [6 .. 12])
      stateFromPy (stateToPy (ofState o)) `shouldBe` Just (ofState o)
      stateFromPy (stateToPy emptyRlmState) `shouldBe` Just emptyRlmState
    it "rejects junk" $ stateFromPy (PStr "x") `shouldBe` Nothing
