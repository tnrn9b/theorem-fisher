-- | The CLM store, pure part: spill, the handle line, read_stored slices, the
-- rollback note, persistence, the two config keys, and the archive.
module Fisher.Agent.ClmStoreSpec (spec) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Fisher.Agent.Context.Budget (readStub, shortenWrites)
import Fisher.Agent.Context.Clm
import Fisher.Agent.Context.ClmStore
import Fisher.Agent.Context.Mirror (Turn (..), tokens)
import Fisher.Agent.Context.Recall
import Fisher.Agent.Context.Rlm (emptyStoreText)
import Fisher.Py.Value

toolMsg :: Text -> PyVal
toolMsg t = obj [("role", PStr "tool"), ("tool_call_id", PStr "c1"), ("content", PStr t)]

callOf :: Text -> Text -> PyVal
callOf name args =
  obj [("id", PStr "c1"), ("type", PStr "function"), ("function", obj [("name", PStr name), ("arguments", PStr args)])]

-- | A text of n lines, each "line K" padded to w characters.
linesOf :: Int -> Int -> Text
linesOf n w = T.intercalate "\n" [T.justifyLeft w 'x' ("line " <> T.pack (show k) <> " ") | k <- [1 .. n]]

-- limit 1000 tokens, share 0.25: threshold 250 tokens
limit :: Integer
limit = 1000

share :: Double
share = 0.25

ctxPath :: Text
ctxPath = "/w/ctx/c.txt"

run :: Store -> [(PyVal, Maybe PyVal, Text, Maybe Int)] -> ([(PyVal, Text)], Store, [Item])
run st = spill limit share ctxPath 10 st

-- | A text whose tool message has exactly n tokens.
withTokens :: Integer -> Text
withTokens n = go 0
  where
    go k = let t = T.replicate k "a" in if tokens [toolMsg t] >= n then t else go (k + 1)

storeOf :: [Text] -> Store
storeOf ts =
  Store
    [Item i Spilled Nothing Nothing (Just (20 + i)) (toolMsg t) | (i, t) <- zip [1 ..] ts]
    (length ts + 1)
    (S.fromList [21 .. 20 + length ts])

itemPy :: Int -> PyVal
itemPy i = obj [("id", PInt (fromIntegral i)), ("kind", PStr "spilled"), ("message", toolMsg "z")]

spec :: Spec
spec = do
  describe "spill" $ do
    it "keeps a result of exactly the threshold" $ do
      let t = withTokens 250
      tokens [toolMsg t] `shouldBe` 250
      let (out, st, new) = run emptyStore [(toolMsg t, Nothing, t, Nothing)]
      out `shouldBe` [(toolMsg t, t)]
      new `shouldBe` []
      st `shouldBe` emptyStore
    it "spills one token over" $ do
      let t = withTokens 251
          (out, st, new) = run emptyStore [(toolMsg t, Nothing, t, Nothing)]
      map itId new `shouldBe` [1]
      length (sItems st) `shouldBe` 1
      map (T.isInfixOf "[stored as S1: " . snd) out `shouldBe` [True]
    it "previews whole lines within 10% of the limit" $ do
      let t = linesOf 100 40
          (out, _, _) = run emptyStore [(toolMsg t, Nothing, t, Nothing)]
          pv = snd (head out)
          ls = T.lines pv
          body = init ls
      length body `shouldSatisfy` (> 0)
      body `shouldBe` take (length body) (T.lines t)
      tokens [PStr (T.intercalate "\n" body)] `shouldSatisfy` (<= 100)
      tokens [PStr (T.intercalate "\n" (take (length body + 1) (T.lines t)))] `shouldSatisfy` (> 100)
      get "content" (fst (head out)) `shouldBe` Just (PStr pv)
    it "writes the handle line as the spec has it" $ do
      -- 411 lines of 40 characters, then a last line making 18,400 characters in all
      let ls = replicate 411 (T.replicate 40 "x") <> [T.replicate (18400 - 411 * 41) "x"]
          t = T.intercalate "\n" ls
      T.length t `shouldBe` 18400
      length (T.lines t) `shouldBe` 412
      let st0 = Store [] 7 S.empty
          (_, _, itms) = spill 100000 0.0001 ctxPath 0 st0 [(toolMsg t, Nothing, t, Nothing)]
      length itms `shouldBe` 1
      handleLine (head itms) `shouldBe` "[stored as S7: 18,400 characters, 412 lines; read_stored(\"S7\", from_line, to_line) or recall(question)]"
    it "counts wrapped lines in the handle" $ do
      let t = T.replicate 18400 "y"
          st0 = Store [] 1 S.empty
          (_, _, itms) = spill 100000 0.0001 ctxPath 0 st0 [(toolMsg t, Nothing, t, Nothing)]
      handleLine (head itms) `shouldBe` "[stored as S1: 18,400 characters, 10 lines; read_stored(\"S1\", from_line, to_line) or recall(question)]"
    it "never spills a read of the context file or its backup" $ do
      let t = withTokens 400
          rd p = callOf "read_file" ("{\"path\": \"" <> p <> "\"}")
          (out, _, new) = run emptyStore
            [ (toolMsg t, Just (rd ctxPath), t, Nothing)
            , (toolMsg t, Just (rd "/w/ctx/c.prev.txt"), t, Nothing)
            , (toolMsg t, Just (rd "/w/ctx//./c.txt"), t, Nothing) ]
      new `shouldBe` []
      map snd out `shouldBe` [t, t, t]
    it "never spills a result of read_stored or recall" $ do
      let t = withTokens 400
          (out, st, new) = run emptyStore
            [ (toolMsg t, Just (callOf "read_stored" "{\"id\": \"S1\", \"from_line\": 1, \"to_line\": 9}"), t, Nothing)
            , (toolMsg t, Just (callOf "recall" "{\"question\": \"why?\"}"), t, Nothing)
            , (set "name" (PStr "recall") (toolMsg t), Nothing, t, Nothing) ]
      new `shouldBe` []
      st `shouldBe` emptyStore
      map snd out `shouldBe` [t, t, t]
    it "spills a read of another file, and records the tool name" $ do
      let t = withTokens 400
          (_, _, new) = run emptyStore [(toolMsg t, Just (callOf "read_file" "{\"path\": \"/w/A.lean\"}"), t, Nothing)]
      map itTool new `shouldBe` [Just "read_file"]
    it "spills three large results as S1, S2, S3, at consecutive positions" $ do
      let t = withTokens 400
          (_, st, new) = run emptyStore [(toolMsg t, Nothing, t, Nothing) | _ <- [1 :: Int, 2, 3]]
      map (itemName . itId) new `shouldBe` ["S1", "S2", "S3"]
      map itOrigin new `shouldBe` [Just 10, Just 11, Just 12]
      map itKind new `shouldBe` [Spilled, Spilled, Spilled]
      sNext st `shouldBe` 4
      sOrigins st `shouldBe` S.fromList [10, 11, 12]
    it "stores the original message unchanged" $ do
      let t = withTokens 400
          (_, _, itms) = run emptyStore [(toolMsg t, Nothing, t, Nothing)]
      itMessage (head itms) `shouldBe` toolMsg t
    it "does not store a result whose position is already stored" $ do
      let t = withTokens 400
          st0 = emptyStore {sOrigins = S.fromList [5], sNext = 3}
          (out, st, new) = run st0 [(toolMsg t, Nothing, t, Just 5), (toolMsg t, Nothing, t, Just 6)]
      map itOrigin new `shouldBe` [Just 6]
      map itId new `shouldBe` [3]
      snd (head out) `shouldBe` t
      sOrigins st `shouldBe` S.fromList [5, 6]
  describe "readStored" $ do
    let ten = T.intercalate "\n" ["l" <> T.pack (show k) | k <- [1 .. 10 :: Int]]
        st1 = storeOf [ten]
    it "gives lines 3 to 5" $
      readStored 1000 st1 "S1" 3 5 `shouldBe` "l3\nl4\nl5"
    it "clips to_line past the end" $
      readStored 1000 st1 "S1" 8 99 `shouldBe` "l8\nl9\nl10"
    it "gives an empty slice with a reason for a bad range" $ do
      let msg = "(no lines: from_line must be at least 1 and not after to_line)"
      readStored 1000 st1 "S1" 5 3 `shouldBe` msg
      readStored 1000 st1 "S1" 0 3 `shouldBe` msg
      readStored 1000 st1 "S1" (-2) 3 `shouldBe` msg
      readStored 1000 st1 "S1" 2 0 `shouldBe` msg
    it "says so when from_line is past the end" $
      readStored 1000 st1 "S1" 11 20 `shouldBe` "(no lines: S1 has 10 lines)"
    it "cuts a slice over 25% of the limit and names the next range" $ do
      let big = linesOf 200 40
          st = storeOf [big]
          out = readStored 1000 st "S1" 1 200
          ls = T.lines out
          body = init ls
          n = length body
      n `shouldSatisfy` (< 200)
      body `shouldBe` take n (T.lines big)
      tokens [PStr (T.intercalate "\n" body)] `shouldSatisfy` (<= 250)
      last ls `shouldBe` "... (" <> T.pack (show (200 - n)) <> " more lines; read_stored(\"S1\", " <> T.pack (show (n + 1)) <> ", 200) continues)"
    it "reads one 30,000-character line in pieces until the end" $ do
      let big = T.replicate 30000 "q"
          st = storeOf [big]
          go k acc =
            let out = readStored 1000 st "S1" k 15
                ls = T.lines out
             in if "... (" `T.isPrefixOf` last ls
                  then go (k + length ls - 1) (acc <> T.concat (init ls))
                  else acc <> T.concat ls
      go 1 "" `shouldBe` big
      let first = readStored 1000 st "S1" 1 15
      T.lines first `shouldBe` [T.replicate 2000 "q", "... (14 more lines; read_stored(\"S1\", 2, 15) continues)"]
    it "reports an unknown id with the current range" $ do
      let st = storeOf (replicate 14 "x")
      readStored 1000 st "S99" 1 5 `shouldBe` "Error: no stored item S99 (the store holds S1 to S14)."
      readStored 1000 st "nonsense" 1 5 `shouldBe` "Error: no stored item nonsense (the store holds S1 to S14)."
    it "gives the empty-store text for an empty store" $
      readStored 1000 emptyStore "S1" 1 5 `shouldBe` emptyStoreText
  describe "texts" $ do
    it "names the stored range of dropped turns" $ do
      storedRangeNote [9, 10, 11] `shouldBe` " The dropped turns are stored as S9 to S11."
      storedRangeNote [9] `shouldBe` " The dropped turns are stored as S9."
      storedRangeNote [] `shouldBe` ""
    it "names several items as runs, with commas and a last and" $ do
      storedRangeNote [3, 7, 9, 10, 11] `shouldBe` " The dropped turns are stored as S3, S7 and S9 to S11."
      storedRangeNote [3, 5] `shouldBe` " The dropped turns are stored as S3 and S5."
      storedRangeNote [11, 9, 10, 9] `shouldBe` " The dropped turns are stored as S9 to S11."
      storedRangeNote [1, 2, 4, 5] `shouldBe` " The dropped turns are stored as S1 to S2 and S4 to S5."
    it "reads the names back from a note" $ do
      noteStoredIds ("[Context over budget: ...]" <> storedRangeNote [3, 7, 9, 10, 11]) `shouldBe` [3, 7, 9, 10, 11]
      noteStoredIds ("[Context over budget: ...]" <> storedRangeNote [9]) `shouldBe` [9]
      noteStoredIds "[Context over budget: ...]" `shouldBe` []
    it "renders an item with its name and tool" $ do
      let (_, _, itms) = run emptyStore [(toolMsg (withTokens 400), Just (callOf "lean_build" "{}"), withTokens 400, Nothing)]
      length itms `shouldBe` 1
      T.takeWhile (/= '\n') (renderItem (head itms)) `shouldBe` "[S1, tool lean_build]"
      renderItem (Item 2 Archived (Just 4) Nothing Nothing (obj [("role", PStr "assistant"), ("content", PStr "hi")]))
        `shouldBe` "[S2, assistant]\nhi"
    it "has the prompt paragraph and a read_stored schema" $ do
      storePromptSection `shouldBe` "Large tool outputs and anything you cut from the context are kept in a store, so nothing is lost. read_stored gives exact lines of a stored item (S1, S2, ...); recall answers a question about all of them. The context budget is never a reason to stop: store, read back or recall what you need, and keep working until the task is done."
      (get "function" readStoredSchema >>= get "name") `shouldBe` Just (PStr "read_stored")
  describe "persistence" $ do
    it "round-trips a store" $ do
      let s = (storeOf [withTokens 400, "short"]) {sNext = 9}
          s' = s {sItems = sItems s <> [Item 8 Archived (Just 3) Nothing Nothing (toolMsg "z")], sOrigins = S.insert 2 (sOrigins s)}
      storeFromPy (storeToPy s') `shouldBe` Just s'
      storeFromPy (storeToPy emptyStore) `shouldBe` Just emptyStore
    it "refuses an invalid value" $ do
      storeFromPy (PStr "x") `shouldBe` Nothing
      fmap sNext (storeFromPy (PDict [("next", PInt 1), ("origins", PList []), ("items", PList [itemPy 5])])) `shouldBe` Just 6
      storeFromPy (PDict [("items", PInt 3)]) `shouldBe` Nothing
      storeFromPy (PDict [("next", PInt 1), ("origins", PList []), ("items", PList [PInt 1])]) `shouldBe` Nothing
  describe "config" $ do
    it "reads store and spill" $ do
      let c = clmConfigFromPy (Just (obj [("store", PBool True), ("spill", PFloat 0.5)]))
      (ccStore c, ccSpill c) `shouldBe` (True, 0.5)
    it "falls back to 0.25 for an invalid spill" $ do
      ccSpill (clmConfigFromPy (Just (obj [("spill", PInt 0)]))) `shouldBe` 0.25
      ccSpill (clmConfigFromPy (Just (obj [("spill", PInt 2)]))) `shouldBe` 0.25
      ccSpill (clmConfigFromPy (Just (obj [("spill", PFloat 0)]))) `shouldBe` 0.25
      ccSpill (clmConfigFromPy (Just (obj [("spill", PFloat 1.5)]))) `shouldBe` 0.25
      ccSpill (clmConfigFromPy (Just (obj [("spill", PInt 1)]))) `shouldBe` 1
    it "defaults to store off and parses a logged config without the keys" $ do
      (ccStore defaultClmConfig, ccSpill defaultClmConfig) `shouldBe` (False, 0.25)
      let c = clmConfigFromPy (Just (clmConfigToPy defaultClmConfig))
      (ccStore c, ccSpill c) `shouldBe` (False, 0.25)
    it "reads store_prompt: default True, false gives False, a non-bool keeps True" $ do
      ccStorePrompt defaultClmConfig `shouldBe` True
      ccStorePrompt (clmConfigFromPy Nothing) `shouldBe` True
      ccStorePrompt (clmConfigFromPy (Just (obj [("store_prompt", PBool False)]))) `shouldBe` False
      ccStorePrompt (clmConfigFromPy (Just (obj [("store_prompt", PBool True)]))) `shouldBe` True
      ccStorePrompt (clmConfigFromPy (Just (obj [("store_prompt", PStr "false")]))) `shouldBe` True
      ccStorePrompt (clmConfigFromPy (Just (obj [("store_prompt", PInt 0)]))) `shouldBe` True
    it "logs store_prompt only with the store on and the prompt off, and a log without the key replays as True" $ do
      get "store_prompt" (clmConfigToPy defaultClmConfig) `shouldBe` Nothing
      get "store_prompt" (clmConfigToPy defaultClmConfig {ccStore = True}) `shouldBe` Nothing
      get "store_prompt" (clmConfigToPy defaultClmConfig {ccStorePrompt = False}) `shouldBe` Nothing
      let off = defaultClmConfig {ccStore = True, ccStorePrompt = False}
      get "store_prompt" (clmConfigToPy off) `shouldBe` Just (PBool False)
      clmConfigFromPy (Just (clmConfigToPy off)) `shouldBe` off
      ccStorePrompt (clmConfigFromPy (Just (clmConfigToPy defaultClmConfig {ccStore = True}))) `shouldBe` True
    it "round-trips through clmConfigToPy" $ do
      let c = defaultClmConfig {ccStore = True, ccSpill = 0.5}
      clmConfigFromPy (Just (clmConfigToPy c)) `shouldBe` c
  describe "recallTexts" $
    it "gives the empty-store text for no texts" $ do
      r <- recallTexts (error "unused") [] "q"
      rcText r `shouldBe` emptyStoreText
  describe "archive" archiveSpec
  describe "afterBatch with spilled results" $ do
    let cfg = defaultClmConfig {ccBudget = 2000, ccReserve = 0, ccSkill = ""}
        a = asstM "Building." (callOf "terminal" "{}")
        long = T.replicate 3000 "x"
        bi sp =
          BatchIn
            { biConfig = cfg, biPath = ctxPath, biSession = "s", biState = newClmState, biMirror = ""
            , biFile = Nothing, biAssistant = a, biTools = [(toolMsg long, long)], biTodo = Nothing, biSpilled = sp }
        liveTool o = [m | Turn _ m <- stTurns (boState o), get "role" m == Just (PStr "tool")]
    it "does not cut a spilled result's text" $ do
      let o = afterBatch (bi (S.fromList [0]))
      map (T.takeWhile (/= '\n') . maybe "" pyStrOf . get "content") (liveTool o) `shouldBe` [long]
      boDropped o `shouldBe` []
    it "still cuts a result that was not spilled" $ do
      let o = afterBatch (bi S.empty)
      map (T.isInfixOf "characters cut" . maybe "" pyStrOf . get "content") (liveTool o) `shouldBe` [True]

-- ------------------------------------------------------------------ archive

userM :: Text -> PyVal
userM t = obj [("role", PStr "user"), ("content", PStr t)]

asstM :: Text -> PyVal -> PyVal
asstM t c = obj [("role", PStr "assistant"), ("content", PStr t), ("tool_calls", PList [c])]

u0, a1, t2 :: PyVal
u0 = userM "Prove foo."
a1 = asstM "Building." (callOf "terminal" "{\"command\": \"lake build\"}")
t2 = toolMsg "build output"

tr3 :: [PyVal]
tr3 = [u0, a1, t2]

org3 :: M.Map Int Int
org3 = M.fromList [(1, 0), (2, 1), (3, 2)]

before3 :: [Turn]
before3 = [Turn 1 u0, Turn 2 a1, Turn 3 t2]

-- | (position, turn, tool, why) of each new item, and the store's positions.
summary :: (Store, [(Item, Text)]) -> ([(Maybe Int, Maybe Int, Maybe Text, Text)], [Int])
summary (st, its) = ([(itOrigin i, itTurn i, itTool i, w) | (i, w) <- its], S.toList (sOrigins st))

archiveSpec :: Spec
archiveSpec = do
  it "stores turns the model deleted, once each" $ do
    let r = archiveWhy ctxPath tr3 org3 before3 [Turn 1 u0] [] 3 emptyStore
    summary r `shouldBe` ([(Just 1, Just 2, Nothing, "deleted"), (Just 2, Just 3, Just "terminal", "deleted")], [1, 2])
    map (itKind . fst) (snd r) `shouldBe` [Archived, Archived]
    map (itMessage . fst) (snd r) `shouldBe` [a1, t2]
    map (itId . fst) (snd r) `shouldBe` [1, 2]
    -- the same batch again stores nothing more
    let (st1, _) = r
        r2 = archiveWhy ctxPath tr3 org3 [Turn 1 u0] [Turn 1 u0] [] 3 st1
    snd r2 `shouldBe` []
    fst r2 `shouldBe` st1
  it "stores the original of a turn the model rewrote" $ do
    let r = archiveWhy ctxPath tr3 org3 before3 [Turn 1 (userM "the task, in short"), Turn 2 a1, Turn 3 t2] [] 3 emptyStore
    summary r `shouldBe` ([(Just 0, Just 1, Nothing, "rewritten")], [0])
    map (itMessage . fst) (snd r) `shouldBe` [u0]
  it "stores the original of a read CLM shortened" $ do
    let rd = asstM "" (callOf "read_file" ("{\"path\": \"" <> ctxPath <> "\"}"))
        full = toolMsg "<<< turn 1 user >>>\nProve foo.\n"
        tr = [u0, rd, full]
        stub = set "content" (PStr (readStub "<<< turn 1 user >>>\nProve foo.\n")) full
        r = archiveWhy ctxPath tr org3 [Turn 1 u0, Turn 2 rd, Turn 3 stub] [Turn 1 u0, Turn 2 rd, Turn 3 stub] [] 3 emptyStore
    summary r `shouldBe` ([(Just 2, Just 3, Just "read_file", "shortened")], [2])
    map (itMessage . fst) (snd r) `shouldBe` [full]
  it "stores the original of a result CLM cut" $ do
    let cut = set "content" (PStr "build [... 3 characters cut ...] output") t2
        r = archiveWhy ctxPath tr3 org3 [Turn 1 u0] [Turn 1 u0, Turn 2 a1, Turn 3 cut] [] 3 emptyStore
    summary r `shouldBe` ([(Just 2, Just 3, Just "terminal", "cut")], [2])
  it "stores the units a rollback dropped, from this batch and from the context" $ do
    let a5 = asstM "Again." (callOf "terminal" "{\"command\": \"lake env\"}")
        t6 = toolMsg "env output"
        tr = tr3 <> [a5, t6]
        note = userM "[Context over budget: ...]"
        r = archiveWhy ctxPath tr org3 before3 [Turn 1 u0, Turn 4 note] [[a1, t2], [a5, t6]] 3 emptyStore
    summary r
      `shouldBe` ( [ (Just 1, Just 2, Nothing, "rolled_back"), (Just 2, Just 3, Just "terminal", "rolled_back")
                   , (Just 3, Nothing, Nothing, "rolled_back"), (Just 4, Nothing, Just "terminal", "rolled_back") ]
                 , [1, 2, 3, 4] )
    map (itMessage . fst) (snd r) `shouldBe` [a1, t2, a5, t6]
  it "stores this batch's dropped unit even when its live copy was cut" $ do
    let cut = set "content" (PStr "b [...] t") t2
        r = archiveWhy ctxPath tr3 (M.fromList [(1, 0)]) [Turn 1 u0] [Turn 1 u0, Turn 2 (userM "note")] [[a1, cut]] 1 emptyStore
    summary r `shouldBe` ([(Just 1, Nothing, Nothing, "rolled_back"), (Just 2, Nothing, Just "terminal", "rolled_back")], [1, 2])
    map (itMessage . fst) (snd r) `shouldBe` [a1, t2]
  it "does not store a spilled result again when its preview turn is deleted" $ do
    let st0 = emptyStore {sItems = [Item 1 Spilled Nothing (Just "terminal") (Just 2) t2], sNext = 2, sOrigins = S.fromList [2]}
        r = archiveWhy ctxPath tr3 org3 before3 [Turn 1 u0] [] 3 st0
    summary r `shouldBe` ([(Just 1, Just 2, Nothing, "deleted")], [1, 2])
    map (itId . fst) (snd r) `shouldBe` [2]
  it "never stores a turn the model wrote, or a rollback note" $ do
    let flat = userM "Done so far: foo is proved."
        note = userM "[Context over budget: ...]"
        r = archiveWhy ctxPath tr3 org3 (before3 <> [Turn 9 flat, Turn 10 note]) before3 [] 3 emptyStore
    summary r `shouldBe` ([], [])
  it "takes a result with CLM's context line as live in full" $ do
    let withLine = set "content" (PStr "build output\n\n[context 120 of 5,952 tokens (2%)]") t2
        r = archiveWhy ctxPath tr3 org3 before3 [Turn 1 u0, Turn 2 a1, Turn 3 withLine] [] 3 emptyStore
    summary r `shouldBe` ([], [])
    liveInFull ctxPath t2 withLine `shouldBe` True
    liveInFull ctxPath t2 (set "content" (PStr "build output\n\nmore") t2) `shouldBe` False
  it "takes an assistant whose writes of the context file were shortened as live in full" $ do
    let w = asstM "Compacting." (callOf "write_file" ("{\"path\": \"" <> ctxPath <> "\", \"content\": \"a long new context\"}"))
        short = shortenWrites ctxPath w
        tr = [u0, w, t2]
        r = archiveWhy ctxPath tr org3 [Turn 1 u0] [Turn 1 u0, Turn 2 short, Turn 3 t2] [] 3 emptyStore
    short `shouldNotBe` w
    summary r `shouldBe` ([], [])
  it "applies shortenWrites only for the context file's path" $ do
    let w = asstM "Writing." (callOf "write_file" "{\"path\": \"/w/A.lean\", \"content\": \"theorem foo\"}")
        short = shortenWrites "/w/A.lean" w
        r = archiveWhy ctxPath [u0, w, t2] org3 [Turn 1 u0] [Turn 1 u0, Turn 2 short, Turn 3 t2] [] 3 emptyStore
    short `shouldNotBe` w
    summary r `shouldBe` ([(Just 1, Just 2, Nothing, "rewritten")], [1])
  it "labels a turn the edit rewrote and the rollback dropped rolled_back" $ do
    let flat = userM "summary of the build"
        note = userM "[Context over budget: ...]"
        (st, its, named) = archiveStep ctxPath tr3 org3 before3 [Turn 1 u0, Turn 4 note] [[flat], [t2]] (S.fromList [2, 3]) 3 emptyStore
    [(itOrigin i, w) | (i, w) <- its] `shouldBe` [(Just 1, "rolled_back"), (Just 2, "rolled_back")]
    named `shouldBe` [1, 2]
    S.toList (sOrigins st) `shouldBe` [1, 2]
  it "names already stored items that hold dropped originals" $ do
    let preview = set "content" (PStr "build\n[stored as S1: ...]") t2
        st0 = emptyStore {sItems = [Item 1 Spilled Nothing (Just "terminal") (Just 2) t2], sNext = 2, sOrigins = S.fromList [2]}
        note = userM "[Context over budget: ...]"
        (_, its, named) = archiveStep ctxPath tr3 org3 [Turn 1 u0, Turn 2 a1, Turn 3 preview] [Turn 1 u0, Turn 4 note] [[a1, preview]] (S.fromList [2, 3]) 3 st0
    map (itId . fst) its `shouldBe` [2]
    named `shouldBe` [1, 2]
  it "does not name an item the edit deleted between dropped turns" $ do
    let u3 = userM "a note of mine"
        a5 = asstM "Again." (callOf "terminal" "{}")
        t6 = toolMsg "again"
        tr = [u0, a1, t2, u3, a5, t6]
        org = M.fromList (zip [1 ..] [0 .. 5])
        bs = zipWith Turn [1 ..] tr
        note = userM "[Context over budget: ...]"
        (_, its, named) = archiveStep ctxPath tr org bs [Turn 1 u0, Turn 7 note] [[a1, t2], [a5, t6]] (S.fromList [2, 3, 5, 6]) 6 emptyStore
    [(itId i, w) | (i, w) <- its] `shouldBe` [(1, "rolled_back"), (2, "rolled_back"), (3, "deleted"), (4, "rolled_back"), (5, "rolled_back")]
    named `shouldBe` [1, 2, 4, 5]
  it "archives an overflow rollback and ends its note with the stored items" $ do
    let st0 = newClmState {stTurns = before3, stCounter = 4}
    case overflowRollback defaultClmConfig ctxPath Nothing st0 of
      Left e -> expectationFailure (T.unpack e)
      Right st1 -> do
        let (st2, store, its) = archiveOverflow ctxPath tr3 org3 st0 st1 emptyStore
        [(itOrigin i, w) | (i, w) <- its] `shouldBe` [(Just 1, "rolled_back"), (Just 2, "rolled_back")]
        S.toList (sOrigins store) `shouldBe` [1, 2]
        let noteText = [maybe "" pyStrOf (get "content" m) | Turn i m <- stTurns st2, Just i == stNoteId st2]
        map (" The dropped turns are stored as S1 to S2." `T.isSuffixOf`) noteText `shouldBe` [True]
  it "a replaced rollback note names what the note before it named" $ do
    let a5 = asstM "Again." (callOf "terminal" "{}")
        t6 = toolMsg "again"
        tr = [u0, a1, t2, userM "old note", a5, t6]
        oldNote = userM ("[Context over budget: ...]" <> storedRangeNote [1, 2])
        newNote = userM "[Context over budget: the last turn ...]"
        st0 = emptyStore {sItems = [Item 1 Archived Nothing Nothing (Just 1) a1, Item 2 Archived Nothing (Just "terminal") (Just 2) t2], sNext = 3, sOrigins = S.fromList [1, 2]}
        bs = [Turn 1 u0, Turn 4 oldNote]
        out =
          BatchOut
            { boState = newClmState {stTurns = [Turn 1 u0, Turn 4 newNote], stCounter = 5, stNoteId = Just 4, stRollbacks = 2}
            , boOutcome = RolledBack, boFlags = [], boBackup = Nothing, boFlush = False
            , boDropped = [[a5, t6]], boDroppedTurns = [] }
        (out', _, _, its) = archiveBatch ctxPath tr (M.fromList [(1, 0)]) 4 bs out st0
        noteText = [maybe "" pyStrOf (get "content" m) | Turn i m <- stTurns (boState out'), i == 4]
    map (itId . fst) its `shouldBe` [3, 4]
    noteText `shouldBe` ["[Context over budget: the last turn ...] The dropped turns are stored as S1 to S4."]
  it "archive is archiveWhy without the reasons" $ do
    let (st, its) = archive ctxPath tr3 org3 before3 [Turn 1 u0] [] 3 emptyStore
    (st, its) `shouldBe` (let (s, xs) = archiveWhy ctxPath tr3 org3 before3 [Turn 1 u0] [] 3 emptyStore in (s, map fst xs))
