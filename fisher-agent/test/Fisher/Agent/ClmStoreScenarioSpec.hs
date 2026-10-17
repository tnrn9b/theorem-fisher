-- | Whole CLM runs with the store on (docs/superpowers/specs/2026-10-14-clm-store-design.md
-- sections 3.1 and 3.2), through CLM's scenario harness (Fisher.Agent.ClmHarness),
-- at a budget of 8,000 tokens (the default reserve: a limit of 5,952):
--
-- * spill-continues: three results of 5,400 characters fill most of the
--   context, then a 30,000-character result. With the store off its cut
--   still does not fit, and the batch rolls back until max_rollbacks stops
--   the session; with the store on it is spilled and the next request has
--   its preview and handle.
-- * three-spills: three 20,000-character results in one batch spill as S1
--   to S3.
-- * rollback-stored: five 5,000-character results in one batch go over the
--   budget; the rolled-back unit is stored and the note ends with its range.
--
-- With the store off each script gives exactly what CLM gave before the
-- store (test/golden/clm/store-off.json, recorded from the code before it).
--
-- The tools and the prompt (spec 3.3 and 3.4), with every request's tools
-- and system prompt recorded:
--
-- * store-tools: a 20,000-character result spills as S1; then read_stored,
--   recall (on the harness's fake RLM), and both in one batch.
-- * store-errors: read_stored with missing or wrongly typed arguments, and
--   an unknown id.
-- * small: nothing is ever stored, so every request is plain CLM's.
module Fisher.Agent.ClmStoreScenarioSpec (spec, scenarios, toolScenarios, storeScenarios2, storeOn) where

import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Fisher.Agent.ClmHarness (runScenario)
import Fisher.Agent.Context.ClmStore (noteStoredIds, readStoredSchema, storePromptSection)
import Fisher.Agent.Context.Mirror (render)
import Fisher.Agent.Context.Rlm (emptyStoreText, recallSchema)
import Fisher.Agent.TestUtil
import Fisher.Py.Value

sid :: Text
sid = "20261014_090000_cd34ef"

-- | A text of exactly n characters: numbered lines of 60 characters.
outText :: Text -> Int -> Text
outText tag n =
  T.take n (T.intercalate "\n" [T.justifyLeft 59 '.' (tag <> " line " <> T.pack (show k) <> " ") | k <- [1 .. n `div` 60 + 1]])

call :: Text -> Text -> Text -> PyVal
call i cmd res =
  obj [("id", PStr i), ("name", PStr "terminal"), ("arguments", obj [("command", PStr cmd)]), ("result", PStr res)]

answer :: Text -> [PyVal] -> PyVal
answer c cs = obj ([("content", PStr c)] <> [("tool_calls", PList cs) | not (null cs)])

scenario :: Text -> Text -> [(Text, PyVal)] -> [PyVal] -> PyVal
scenario name about cfg as =
  obj
    [ ("name", PStr name), ("about", PStr about)
    , ("config", obj (("budget", PInt 8000) : cfg))
    , ("session_id", PStr sid), ("mirror_path", PStr ("/ctx/" <> sid <> "/context.txt"))
    , ("task", PStr "Prove the twelve lemmas in Step/Kernel.lean.")
    , ("answers", PList as) ]

-- | The scripts, with the store off.
scenarios :: [PyVal]
scenarios =
  [ scenario
      "spill-continues"
      "a 30,000-character result after three of 5,400: rolls back to max_rollbacks with the store off, spills with it on"
      [("max_rollbacks", PInt 1)]
      ( [answer ("Step " <> k <> ".") [call ("call_" <> k) ("step " <> k) (outText ("s" <> k) 5400)] | k <- ["1", "2", "3"]]
          <> [ answer "Building." [call "call_4" "lake build" big]
             , answer "Building again." [call "call_5" "lake build" big]
             , answer "Done." [] ] )
  , scenario
      "three-spills"
      "three 20,000-character results in one batch"
      []
      [ answer "Three builds." [call ("call_" <> k) "lake build" (outText "b" 20000) | k <- ["1", "2", "3"]]
      , answer "Done." [] ]
  , scenario
      "rollback-stored"
      "five 5,000-character results in one batch: over the budget, rolled back"
      []
      [ answer "Five reads." [call ("call_" <> k) "cat part" (outText "p" 5000) | k <- ["1", "2", "3", "4", "5"]]
      , answer "Done." [] ]
  ]
  where
    big = outText "build" 30000

-- | A call the loop answers itself (read_stored, recall): no scripted result.
local :: Text -> Text -> PyVal -> PyVal
local i n a = obj [("id", PStr i), ("name", PStr n), ("arguments", a)]

readArgs :: Text -> Integer -> Integer -> PyVal
readArgs i a b = obj [("id", PStr i), ("from_line", PInt a), ("to_line", PInt b)]

question :: Text -> PyVal
question q = obj [("question", PStr q)]

-- | An answer of the agent's model, priced at a cent.
priced :: PyVal -> PyVal
priced = set "cost" (PFloat 0.01)

-- | Recall's root model: a reply with a repl block holding this code (the
-- fake RLM answers @answer CODE@), priced at a quarter.
root :: Text -> PyVal
root code = obj [("content", PStr ("```repl\n" <> code <> "\n```")), ("cost", PFloat 0.25)]

built :: Text
built = outText "b" 20000

-- | The scripts for the tools and the prompt (the store on, requests
-- recorded).
toolScenarios :: [PyVal]
toolScenarios =
  map
    (set "requests" (PBool True) . storeOn)
    [ scenario
        "store-tools"
        "a spill, then read_stored, recall, and both in one batch"
        []
        [ priced (answer "Build." [call "call_1" "lake build" built])
        , priced (answer "Read." [local "r1" "read_stored" (readArgs "S1" 1 3)])
        , priced (answer "Ask." [local "q1" "recall" (question "What failed?")])
        , root "B"
        , priced (answer "Both." [local "r2" "read_stored" (readArgs "S1" 2 2), local "q2" "recall" (question "Again?")])
        , root "C"
        , priced (answer "Done." []) ]
    , scenario
        "store-errors"
        "read_stored with bad arguments and an unknown id"
        []
        [ answer "Build." [call "call_1" "lake build" built]
        , answer
            "Read."
            [ local "e1" "read_stored" (obj [("id", PStr "S1")])
            , local "e2" "read_stored" (obj [("id", PInt 1), ("from_line", PInt 1), ("to_line", PInt 2)])
            , local "e3" "read_stored" (obj [("id", PStr "S1"), ("from_line", PStr "1"), ("to_line", PInt 2)])
            , local "e4" "read_stored" (readArgs "S9" 1 2)
            , local "e5" "recall" (obj []) ]
        , answer "Done." [] ]
    , scenario
        "small"
        "one small result: nothing is stored"
        []
        [answer "Step." [call "call_1" "step" (outText "s" 200)], answer "Done." []]
    , scenario
        "store-spill-low"
        "spill 0.05: a long read_stored slice and a long recall answer stay in the context in full"
        [("spill", PFloat 0.05)]
        [ answer "Build." [call "call_1" "lake build" built]
        , answer "Read." [local "r1" "read_stored" (readArgs "S1" 1 200)]
        , answer "Ask." [local "q1" "recall" (question "Everything?")]
        , root longCode
        , answer "Done." [] ]
    , scenario
        "store-overflow"
        "the provider finds the request too long: the dropped turn is the first item"
        []
        [ answer "Step." [call "call_1" "step" (outText "s" 2000)]
        , obj [("http_error", PInt 400), ("body", PStr "{\"error\": {\"message\": \"This model's maximum context length is 400000 tokens.\"}}")]
        , answer "Done." [] ]
    , scenario
        "store-empty"
        "read_stored and recall before any item"
        []
        [ answer "Read." [local "r1" "read_stored" (readArgs "S1" 1 2), local "q1" "recall" (question "Anything?")]
        , answer "Done." [] ] ]

-- | The resume and rewrite scripts (the store on).
--
-- * resume-rollback: three 20,000-character results (spilled, at
--   positions 2 to 4) and four of 5,000 in one batch go over the budget
--   and roll back.
-- * rewrite-deleted: an empty reply after a batch ends the run on the
--   empty-response fallback, which rewrites the batch's assistant text;
--   in a second run the model deletes every turn.
storeScenarios2 :: [PyVal]
storeScenarios2 =
  map
    storeOn
    [ scenario
        "resume-rollback"
        "three spilled results and four of 5,000 characters in one batch: over the budget, rolled back"
        []
        [ answer
            "Seven."
            ( [call ("call_" <> k) "lake build" (outText "b" 20000) | k <- ["1", "2", "3"]]
                <> [call ("call_" <> k) "cat part" (outText "p" 5000) | k <- ["4", "5", "6", "7"]] )
        , answer "Done." [] ]
    , set "next_task" (PStr "Drop the old turns.") $
        scenario
          "rewrite-deleted"
          "the empty-response fallback rewrites the assistant text; a later run deletes that turn"
          []
          [ answer "Build first." [call "call_1" "lake build" (outText "b" 300)]
          , answer "" []
          , answer "Drop." [writeCall "w1" (render sid [])]
          , answer "Done." [] ] ]

-- | A write of the context file that leaves it as the given text.
writeCall :: Text -> Text -> PyVal
writeCall i t =
  obj
    [ ("id", PStr i), ("name", PStr "write_file")
    , ("arguments", obj [("path", PStr ("/ctx/" <> sid <> "/context.txt")), ("content", PStr t)])
    , ("result", PStr "{\"bytes_written\": 1}"), ("mirror_after", PStr t) ]

-- | A store an earlier process saved: S1 to S3, from its positions 2 to 4.
loaded123 :: PyVal
loaded123 =
  obj
    [ ("next", PInt 4), ("origins", PList (map PInt [1, 2, 3, 4]))
    , ( "items"
      , PList
          [ obj [("id", PInt i), ("kind", PStr "spilled"), ("origin", PInt (i + 1)), ("message", obj [("role", PStr "tool"), ("content", PStr "old")])]
          | i <- [1, 2, 3] ] ) ]

-- | An item record with an origin.
isJustOrigin :: [(Text, PyVal)] -> Bool
isJustOrigin kvs = maybe False (/= PNull) (lookup "origin" kvs)

-- | A recall answer of over 2,000 characters (on one line).
longCode :: Text
longCode = T.unwords (replicate 300 "lemma")

storeOn :: PyVal -> PyVal
storeOn sc = set "config" (set "store" (PBool True) (field "config" sc)) sc

storeOff :: PyVal -> PyVal
storeOff sc = set "config" (set "store" (PBool False) (field "config" sc)) sc

named :: Text -> PyVal
named n = head [sc | sc <- scenarios <> toolScenarios <> storeScenarios2, txt "name" sc == n]

-- | The agent's requests (recall's carry no tools key).
agentRequests :: PyVal -> [PyVal]
agentRequests got = [r | r <- list "requests" got, field "tools" r /= PNull]

-- | Tool call i's result in the messages of call k's request.
resultIn :: Int -> Text -> PyVal -> Maybe Text
resultIn k i got = case [contentOf m | m <- contextAt k got, txt "role" m == "tool", txt "tool_call_id" m == i] of
  (t : _) -> Just t
  [] -> Nothing

-- | A result without CLM's context line (a blank line, then a bracketed
-- line).
bare :: Text -> Text
bare = fst . T.breakOn "\n\n["

costOf :: PyVal -> Maybe Integer
costOf got = case field "cost" got of
  PFloat d -> Just (round (d * 1000))
  _ -> Nothing

-- | The messages of call k's request (1-based).
contextAt :: Int -> PyVal -> [PyVal]
contextAt k got = list "context" (list "steps" got !! (k - 1))

contentOf :: PyVal -> Text
contentOf m = txt "content" m

-- | The start of item i's handle line, for a result of the given characters.
handle :: Int -> Text -> Text
handle i chars = "[stored as S" <> T.pack (show i) <> ": " <> chars <> " characters, "

spec :: Spec
spec = describe "CLM with a store: scenarios" $ do
  golden <- runIO (loadCases "test/golden/clm/store-off.json")
  describe "with the store off" $
    forM_ scenarios $ \sc -> it (T.unpack (txt "name" sc) <> ": the outcome CLM gave before the store") $ do
      got <- runScenario sc
      let want = head ([field "outcome" g | g <- golden, txt "name" g == txt "name" sc] <> [PNull])
      shouldMatch "outcome" got want
  describe "with the store on" $ do
    it "spill-continues: stopped by max_rollbacks with the store off" $ do
      off <- runScenario (named "spill-continues")
      get "error" (field "result" off)
        `shouldBe` Just (PStr "Context budget exceeded: 1 rollbacks in a row did not bring the context under 5,952 tokens.")
      length (list "steps" off) `shouldBe` 5
    it "spill-continues: the 30,000-character result is spilled and the batch continues" $ do
      got <- runScenario (storeOn (named "spill-continues"))
      let step4 = list "steps" got !! 3
      get "rollback" step4 `shouldBe` Nothing
      get "stop" step4 `shouldBe` Nothing
      list "spills" got `shouldSatisfy` (\s -> take 1 s == [PInt 1])
      let ms = contextAt 5 got
          results = [m | m <- ms, txt "role" m == "tool", txt "tool_call_id" m == "call_4"]
      length results `shouldBe` 1
      let c = contentOf (head results)
          big = outText "build" 30000
      T.takeWhile (/= '\n') c `shouldBe` T.takeWhile (/= '\n') big
      handle 1 "30,000" `T.isInfixOf` c `shouldBe` True
      T.length c `shouldSatisfy` (< 3000)
      get "completed" (field "result" got) `shouldBe` Just (PBool True)
    it "three-spills: three large results in one batch spill as S1 to S3" $ do
      got <- runScenario (storeOn (named "three-spills"))
      list "spills" got `shouldBe` [PInt 1, PInt 2, PInt 3]
      let ms = contextAt 2 got
          results = [contentOf m | m <- ms, txt "role" m == "tool"]
      length results `shouldBe` 3
      [handle i "20,000" `T.isInfixOf` r | (i, r) <- zip [1 ..] results] `shouldBe` [True, True, True]
      [T.unpack (txt "kind" it') | it' <- list "items" (field "store" got)] `shouldBe` ["spilled", "spilled", "spilled"]
      get "completed" (field "result" got) `shouldBe` Just (PBool True)
    it "a loaded store's positions belong to the earlier process: new originals there are spilled" $ do
      got <- runScenario (set "store_load" loaded123 (storeOn (named "three-spills")))
      list "spills" got `shouldBe` [PInt 4, PInt 5, PInt 6]
      [txt "kind" it' | it' <- list "items" (field "store" got)] `shouldBe` replicate 6 "spilled"
      list "origins" (field "store" got) `shouldBe` map PInt [2, 3, 4]
    it "after a resume, a rollback's note names only this process's items, not loaded ones at the same positions" $ do
      got <- runScenario (set "store_load" loaded123 (named "resume-rollback"))
      get "rollback" (head (list "steps" got)) `shouldBe` Just (PInt 1)
      list "spills" got `shouldBe` [PInt 4, PInt 5, PInt 6]
      let note = contentOf (last (contextAt 2 got))
          ids = noteStoredIds note
      "[Context over budget: " `T.isPrefixOf` note `shouldBe` True
      ids `shouldBe` [4 .. 11]
      " The dropped turns are stored as S4 to S11." `T.isSuffixOf` note `shouldBe` True
      [i | PDict kvs <- list "items" (field "store" got), Just (PInt i) <- [lookup "id" kvs], i <= 3, isJustOrigin kvs] `shouldBe` []
    it "a loop-side rewrite keeps the original: the fallback's rewritten assistant text, deleted in a later run, is in the store" $ do
      got <- runScenario (named "rewrite-deleted")
      get "completed" (field "result" got) `shouldBe` Just (PBool True)
      [m | t <- list "final_context" got, let m = field "message" t, txt "content" m == "Build first."] `shouldBe` []
      let stored = [field "message" i | i <- list "items" (field "store" got)]
      [txt "role" m | m <- stored, txt "content" m == "Build first."] `shouldBe` ["assistant"]
      [w | a <- list "archives" got, PStr w <- [field "why" a]] `shouldSatisfy` elem "rewritten"
    it "rollback-stored: the dropped unit is stored and the note ends with its range" $ do
      got <- runScenario (storeOn (named "rollback-stored"))
      get "rollback" (head (list "steps" got)) `shouldBe` Just (PInt 1)
      list "spills" got `shouldBe` []
      list "archives" got `shouldBe` [obj [("ids", PList (map PInt [1 .. 6])), ("why", PStr "rolled_back")]]
      let note = last (contextAt 2 got)
      txt "role" note `shouldBe` "user"
      "[Context over budget: " `T.isPrefixOf` contentOf note `shouldBe` True
      " The dropped turns are stored as S1 to S6." `T.isSuffixOf` contentOf note `shouldBe` True
      [txt "kind" it' | it' <- list "items" (field "store" got)] `shouldBe` replicate 6 "archived"
  describe "with the store on: read_stored, recall and the prompt" $ do
    it "before any item, every request's tools and system prompt are plain CLM's" $ do
      on <- runScenario (named "small")
      off <- runScenario (storeOff (named "small"))
      list "items" (field "store" on) `shouldBe` []
      length (list "requests" on) `shouldBe` 2
      list "requests" on `shouldBe` list "requests" off
      tools <- runScenario (named "store-tools")
      toolsOff <- runScenario (storeOff (named "store-tools"))
      take 1 (list "requests" tools) `shouldBe` take 1 (list "requests" toolsOff)
    it "after the first spill, the next request offers read_stored and recall and its system prompt ends with the store's section" $ do
      got <- runScenario (named "store-tools")
      off <- runScenario (storeOff (named "store-tools"))
      let plain = head (list "requests" off)
          later = drop 1 (agentRequests got)
      length later `shouldBe` 4
      forM_ later $ \r -> do
        list "tools" r `shouldBe` list "tools" plain <> [readStoredSchema, recallSchema]
        txt "system" r `shouldBe` txt "system" plain <> "\n\n" <> storePromptSection
    it "with store_prompt false, the tools are offered after the first spill but the system prompt has no store section" $ do
      got <- runScenario (set "config" (set "store_prompt" (PBool False) (field "config" (named "store-tools"))) (named "store-tools"))
      off <- runScenario (storeOff (named "store-tools"))
      let plain = head (list "requests" off)
          later = drop 1 (agentRequests got)
      length later `shouldBe` 4
      forM_ later $ \r -> do
        list "tools" r `shouldBe` list "tools" plain <> [readStoredSchema, recallSchema]
        txt "system" r `shouldBe` txt "system" plain
        storePromptSection `T.isInfixOf` txt "system" r `shouldBe` False
      list "spills" got `shouldBe` [PInt 1]
    it "read_stored gives the item's lines, with no model call" $ do
      got <- runScenario (named "store-tools")
      list "spills" got `shouldBe` [PInt 1]
      fmap bare (resultIn 3 "r1" got) `shouldBe` Just (T.intercalate "\n" (take 3 (T.lines built)))
      -- the agent's first three calls, then recall's root: none between
      [field "tools" r == PNull | r <- take 4 (list "requests" got)] `shouldBe` [False, False, False, True]
    it "recall answers from the fake RLM over the rendered items, and its cost is in the session's" $ do
      got <- runScenario (named "store-tools")
      fmap bare (resultIn 4 "q1" got) `shouldBe` Just "answer B"
      [field "tools" r == PNull | r <- list "requests" got] `shouldBe` [False, False, False, True, False, True, False]
      costOf got `shouldBe` Just (round ((5 * 0.01 + 2 * 0.25 :: Double) * 1000))
      get "completed" (field "result" got) `shouldBe` Just (PBool True)
    it "read_stored and recall in one batch both answer" $ do
      got <- runScenario (named "store-tools")
      fmap bare (resultIn 5 "r2" got) `shouldBe` Just (T.lines built !! 1)
      fmap bare (resultIn 5 "q2" got) `shouldBe` Just "answer C"
    it "read_stored with missing or wrongly typed arguments, or an unknown id, answers with an error text" $ do
      got <- runScenario (named "store-errors")
      let bad = "Error: read_stored needs an id (a string such as \"S3\") and from_line and to_line (integers)."
      map (\i -> fmap bare (resultIn 3 i got)) ["e1", "e2", "e3", "e4", "e5"]
        `shouldBe` map Just [bad, bad, bad, "Error: no stored item S9 (the store holds S1 to S1).", "Error: recall needs a question (a string)."]
      length (list "requests" got) `shouldBe` 3
    it "without recall's setup, read_stored alone is offered and a recall call is refused as an unknown tool" $ do
      let sc = named "store-errors"
      got <- runScenario (set "config" (set "recall" (PBool False) (field "config" sc)) sc)
      off <- runScenario (storeOff sc)
      let plain = head (list "requests" off)
      map (list "tools") (drop 1 (agentRequests got)) `shouldSatisfy` all (== (list "tools" plain <> [readStoredSchema]))
      fmap (T.isPrefixOf "Tool 'recall' does not exist. ") (resultIn 3 "e5" got) `shouldBe` Just True
    it "with the store off, read_stored and recall are refused as unknown tools" $ do
      got <- runScenario (storeOff (named "store-errors"))
      fmap (T.isPrefixOf "Tool 'read_stored' does not exist. ") (resultIn 3 "e4" got) `shouldBe` Just True
      fmap (T.isPrefixOf "Tool 'recall' does not exist. ") (resultIn 3 "e5" got) `shouldBe` Just True
      map (list "tools") (agentRequests got) `shouldSatisfy` all (\ts -> readStoredSchema `notElem` ts && recallSchema `notElem` ts)
    it "never spills a read_stored slice or a recall answer: with spill 0.05 both stay in the context in full" $ do
      got <- runScenario (named "store-spill-low")
      list "spills" got `shouldBe` [PInt 1]
      let slice = maybe "" bare (resultIn 3 "r1" got)
          (body, more) = (init (T.lines slice), last (T.lines slice))
      length body `shouldSatisfy` (> 20)
      body `shouldBe` take (length body) (T.lines built)
      ("more lines; read_stored(\"S1\", " `T.isInfixOf` more) `shouldBe` True
      "[stored as S" `T.isInfixOf` slice `shouldBe` False
      fmap bare (resultIn 4 "r1" got) `shouldBe` Just slice
      fmap bare (resultIn 4 "q1" got) `shouldBe` Just ("answer " <> longCode)
      get "completed" (field "result" got) `shouldBe` Just (PBool True)
    it "the first item stored by an overflow rollback rebuilds the prompt and offers the tools" $ do
      got <- runScenario (named "store-overflow")
      off <- runScenario (storeOff (named "store-overflow"))
      let plain = head (list "requests" off)
          rs = agentRequests got
      length rs `shouldBe` 3
      list "archives" got `shouldSatisfy` (not . null)
      take 2 rs `shouldBe` take 2 (list "requests" off)
      list "tools" (rs !! 2) `shouldBe` list "tools" plain <> [readStoredSchema, recallSchema]
      txt "system" (rs !! 2) `shouldBe` txt "system" plain <> "\n\n" <> storePromptSection
    it "read_stored and recall over an empty store give the empty-store text, with no model call" $ do
      got <- runScenario (named "store-empty")
      off <- runScenario (storeOff (named "store-empty"))
      map (\i -> fmap bare (resultIn 2 i got)) ["r1", "q1"] `shouldBe` [Just emptyStoreText, Just emptyStoreText]
      length (list "requests" got) `shouldBe` 2
      map (list "tools") (list "requests" got) `shouldBe` replicate 2 (list "tools" (head (list "requests" off)))
