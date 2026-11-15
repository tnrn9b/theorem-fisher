{-# LANGUAGE DataKinds #-}

module Fisher.Search.GraphRunSpec (spec) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Role (Said, partText, static, toPrompt, tool)
import Fisher.Role.Internal (said')
import Fisher.Search.Auto (AutoConfig (..), defaultAutoConfig)
import Fisher.Search.Check (AxiomPolicy (..), LeanRun (..), standardAxioms)
import qualified Fisher.Search.Check as C
import Fisher.Search.Controller (ModelFailure (..), Stop (..), StopReason (..))
import Fisher.Search.FakeLsp (fake0, fakeLsp)
import Fisher.Search.Graph
import Fisher.Search.GraphHarness
import Fisher.Search.GraphRun
import Fisher.Search.Lean
import Fisher.Search.Report (graphEventJson, graphReportJson)
import Fisher.Search.Scheduler
import Fisher.Search.Scripted (accept, plan)
import Fisher.Search.Verify (Outcome (..), goalTypes, verifyAttempt)

-- | 'keyLean' printing each checked name as Lean does for one with universe
-- parameters: @\@A.l.{u_1} : type@.
univLean :: LeanRun
univLean = LeanRun $ \root rel -> do
  t <- TE.decodeUtf8 <$> BS.readFile (root </> rel)
  (code, out) <- runLean keyLean root rel
  let checked = [T.strip (T.drop (T.length "#check @") l) | l <- T.lines t, "#check @" `T.isPrefixOf` l]
      fix l = case T.breakOn " : " l of
        (n, rest) | n `elem` checked, not (T.null rest) -> "@" <> n <> ".{u_1}" <> rest
        _ -> l
  pure (code, T.unlines (map fix (T.lines out)))

-- | 'keyLean' that refuses to compile a proof holding LATE when r1 is checked
-- and no lemma is left open: a sketch that passes its check and fails on recheck.
lateLean :: LeanRun
lateLean = LeanRun $ \root rel -> do
  t <- TE.decodeUtf8 <$> BS.readFile (root </> rel)
  (code, out) <- runLean keyLean root rel
  let ds = declarations (fst (T.breakOn "\n\n#print axioms" t))
      late = [d | d <- ds, "LATE" `T.isInfixOf` dProof d]
      checksR1 = "#print axioms r1\n" `T.isInfixOf` t
  pure $ if checksR1 && not (null late) && not (any (\d -> isOpen d && dName d /= "r1") ds)
    then (1, T.pack rel <> ":1:1: error: LATE\n" <> out)
    else (code, out)

-- | The verdict on a root as `fisher search check` gives it: the final base
-- as the agent's file, spliced into the original and checked.
judge :: LeanRun -> Text -> Text -> Text -> IO C.Verdict
judge leanRun original final goal = withSystemTempDirectory "judge" $ \root -> do
  BS.writeFile (root </> "F.lean") (TE.encodeUtf8 original)
  let pol = AxiomPolicy standardAxioms False
  tys <- goalTypes leanRun root "F.lean" original [goal]
  case tys of
    Left e -> fail (T.unpack e)
    Right m -> verifyAttempt leanRun pol root "F.lean" original final original goal (Map.findWithDefault "" goal m) >>= \case
      Judged v _ _ -> pure v
      NotSpliced e -> pure (C.Rejected (T.pack (show e)))

spec :: Spec
spec = runGraphSpec >> controllerSpec >> failureNoteSpec

-- | A failure line's note keeps the bytes of the whole note put on one line,
-- whatever constructor takes its rest.
failureNoteSpec :: Spec
failureNoteSpec = describe "failureNote" $ do
  it "a Lean error as tool output: the bytes of the note on one line" $ do
    let errs = "F.lean:4:2: error: unknown identifier 'foo'\n  at   this\n\ttactic   failed  "
    partText (failureNote "split failed: " tool errs)
      `shouldBe` "split failed: F.lean:4:2: error: unknown identifier 'foo' at this tactic failed"
    partText (failureNote "rejected on recheck: " tool errs)
      `shouldBe` "rejected on recheck: F.lean:4:2: error: unknown identifier 'foo' at this tactic failed"
    partText (failureNote "split failed: " tool (T.replicate 400 "e"))
      `shouldBe` T.take 300 ("split failed: " <> T.replicate 400 "e")
  it "a Fisher sentence, and a blank rest" $ do
    partText (failureNote "split failed: " static "the sketch does not close with its lemmas proved")
      `shouldBe` "split failed: the sketch does not close with its lemmas proved"
    partText (failureNote "" static "waits on lemmas that did not close") `shouldBe` "waits on lemmas that did not close"
    partText (failureNote "split failed: " tool "  \n ") `shouldBe` "split failed:"
  it "whitespace at the prefix's end and the rest's start becomes one space" $ do
    partText (failureNote "FALSE: " static "  \n claim  with\truns \n") `shouldBe` "FALSE: claim with runs"
    partText (failureNote "  lead " static "x") `shouldBe` "lead x"
  it "a note quoting the verifier keeps the bytes of the whole note on one line" $ do
    let oneLine = T.take 300 . T.unwords . T.words
        note claim why = [Piece static "FALSE: ", Piece static claim, Piece static " (not confirmed: ", Quote (said' why :: Said 'Verifier), Piece static ")"]
        whole claim why = oneLine ("FALSE: " <> claim <> " (not confirmed: " <> why <> ")")
        cases =
          [ ("n = 0", "n = 1 works"), ("n = 0", "  spaced \n  out  "), ("n = 0", ""), ("n = 0", "   ")
          , ("n = 0", T.replicate 400 "w"), (T.replicate 270 "c", "a b c d e f g h"), (T.replicate 280 "c", "ab cd")
          , ("claim  ", "  why") ]
    [partText (notePieces (note cl why)) | (cl, why) <- cases] `shouldBe` [whole cl why | (cl, why) <- cases]
    partText (notePieces [Piece static "split rejected: ", Quote (said' "  too\thard \n" :: Said 'Verifier)]) `shouldBe` "split rejected: too hard"
  it "a job's error as a part keeps the bytes of the whole note on one line" $ do
    -- a planner's refused reply, as planJob's error carries it: Fisher's
    -- sentence with the planner's line in it
    let planner = said' "stray   words\tthe planner\n wrote" :: Said 'Planner
        err = static "text outside declarations: " <> toPrompt planner
        oneLine = T.take 300 . T.unwords . T.words
    partText (notePieces [Piece static "split failed: ", Part err])
      `shouldBe` "split failed: text outside declarations: stray words the planner wrote"
    let long = static "the model call failed: " <> tool (T.replicate 400 "e")
    partText (notePieces [Piece static "split failed: ", Part long]) `shouldBe` oneLine ("split failed: " <> partText long)
    partText (notePieces [Piece static "FALSE: ", Piece static "n = 0", Piece static " (not checked: ", Part (static "the call broke: " <> tool "  boom \n"), Piece static ")"])
      `shouldBe` "FALSE: n = 0 (not checked: the call broke: boom )"
    partText (notePieces [Piece static "split rejected: ", Part (static "  cycle: " <> toPrompt (said' "a" :: Said 'Planner))]) `shouldBe` "split rejected: cycle: a"

runGraphSpec :: Spec
runGraphSpec = describe "runGraph" $ do
  it "0: the first events are one NodeAdded per root, depth 0, in file order" $ do
    o <- runG cfg twoRoots [("r1", [ok]), ("r2", [ok])] []
    take 2 (oEvents o) `shouldBe` [NodeAdded 0 1 "r1" 0, NodeAdded 0 2 "r2" 0]
    length [() | NodeAdded _ _ _ 0 <- oEvents o] `shouldBe` 2

  it "1: closes a root by its direct attempt without a model call" $ do
    o <- runG cfg oneRoot [("r1", [ok])] []
    oAsks o `shouldBe` []
    statusOf o "r1" `shouldBe` NProved
    grClosedRoots (oRep o) `shouldBe` 1
    grStopped (oRep o) `shouldBe` Nothing
    length (openGoals (grBase (oRep o))) `shouldBe` 0

  it "2: splits a failed root into two lemmas proved in parallel, then closes it" $ do
    o <- runG cfg oneRoot
      [("r1", [bad]), ("l1", [(w "exact done") {rDelay = 20}]), ("l2", [(w "exact done") {rDelay = 300}])]
      [say Planner (plan [("l1", "P"), ("l2", "Q")] "theorem r1 : A" "exact combine l1 l2"), say Verifier accept]
    tags o `shouldBe`
      [ "launch r1", "end r1", "proposed r1", "accepted r1", "added l1", "added l2"
      , "launch l1", "launch l2", "end l1", "proved l1", "end l2", "proved l2", "proved r1" ]
    grClosedRoots (oRep o) `shouldBe` 1
    length (openGoals (grBase (oRep o))) `shouldBe` 0
    map fst (oAsks o) `shouldBe` [Planner, Verifier]
    sStatus (splitOf o "r1" 0) `shouldBe` SDone
    let briefOf g = fromMaybe "" (lookup g (oBriefs o))
    briefOf "l1" `shouldSatisfy` T.isInfixOf "only these lemmas of the current plan, even where they still end in `sorry`: `l2`."
    briefOf "r1" `shouldSatisfy` T.isInfixOf "Use no declaration that still ends in `sorry`."

  it "3: two roots naming the same lemma key share one node, proved once" $ do
    o <- runG cfg twoRoots
      [("r1", [bad]), ("r2", [bad {rDelay = 300}]), ("l", [ok {rDelay = 900}])]
      [ sayTo Planner (toNode "r1") (plan [("l", "P")] "theorem r1 : A" "exact use l")
      , sayTo Planner (toNode "r2") (plan [("m", "P")] "theorem r2 : B" "exact use m")
      , say Verifier accept, say Verifier accept ]
    IM.size (gNodes (grGraph (oRep o))) `shouldBe` 3
    count "shared l" (tags o) `shouldBe` 1
    count "proved l" (tags o) `shouldBe` 1
    count "l" (oLaunched o) `shouldBe` 1
    grClosedRoots (oRep o) `shouldBe` 2
    T.isInfixOf "lemma m : P := by exact l" (grBase (oRep o)) `shouldBe` True

  it "4: a lemma already proved is reused, with no worker on it" $ do
    o <- runG cfg twoRoots
      [("r1", [bad]), ("r2", [bad {rDelay = 600}]), ("l", [ok])]
      [ sayTo Planner (toNode "r1") (plan [("l", "P")] "theorem r1 : A" "exact use l")
      , sayTo Planner (toNode "r2") (plan [("m", "P")] "theorem r2 : B" "exact use m")
      , say Verifier accept, say Verifier accept ]
    count "reused l" (tags o) `shouldBe` 1
    oLaunched o `shouldBe` ["r1", "r2", "l"]
    IM.size (gNodes (grGraph (oRep o))) `shouldBe` 3
    grClosedRoots (oRep o) `shouldBe` 2

  it "5: a rejected split, then an accepted one" $ do
    o <- runG cfg oneRoot
      [("r1", [bad]), ("l2", [ok])]
      [ say Planner (plan [("l1", "P")] "theorem r1 : A" "exact use l1")
      , say Planner (plan [("l2", "Q")] "theorem r1 : A" "exact use l2")
      , say Verifier (reject "too hard"), say Verifier accept ]
    [why | SplitRejected _ 1 why <- oEvents o] `shouldBe` ["too hard"]
    [s | SplitAccepted _ s _ <- oEvents o] `shouldBe` [2]
    statusOf o "r1" `shouldBe` NProved
    lookup 1 (grWhiteboards (oRep o)) `shouldSatisfy` maybe False (T.isInfixOf "r1: split rejected: too hard")

  it "6: a lemma whose attempt and both splits fail fails its split, and the parent splits again" $ do
    o <- runG cfg oneRoot
      [("r1", [bad]), ("l", [bad]), ("k", [ok])]
      ( [ sayTo Planner (toNode "r1") (plan [("l", "P")] "theorem r1 : A" "exact use l")
        , sayTo Planner (toNode "r1") (plan [("k", "Q")] "theorem r1 : A" "exact use k") ]
        <> replicate 4 (sayTo Planner (toNode "l") garbage)
        <> [say Verifier accept, say Verifier accept] )
    count "split failed l" (tags o) `shouldBe` 2
    statusOf o "l" `shouldBe` NFailed "nothing left to try"
    [why | SplitFailed _ 1 why <- oEvents o] `shouldBe` ["lemma l failed"]
    ["failed l", "split failed r1", "proposed r1"] `shouldSatisfy` (`infixOf` tags o)
    statusOf o "r1" `shouldBe` NProved
    declsNamed o "l" `shouldBe` []
    T.isInfixOf "sorry" (grBase (oRep o)) `shouldBe` False
    judge keyLean oneRoot (grBase (oRep o)) "r1" `shouldReturn` C.Closed

  it "7: a node at the depth limit gets two attempts and no split" $ do
    o <- runG cfg {gcMaxSplits = 1} oneRoot
      [("r1", [bad]), ("a", [bad]), ("b", [bad]), ("c", [bad, bad])]
      [ sayTo Planner (toNode "r1") (plan [("a", "P")] "theorem r1 : A" "exact use a")
      , sayTo Planner (toNode "a") (plan [("b", "Q")] "lemma a : P" "exact use b")
      , sayTo Planner (toNode "b") (plan [("c", "R")] "lemma b : Q" "exact use c")
      , say Verifier accept, say Verifier accept, say Verifier accept ]
    nDepth (nodeBy o "c") `shouldBe` 3
    count "c" (oLaunched o) `shouldBe` 2
    count "proposed c" (tags o) `shouldBe` 0
    map (statusOf o) ["c", "b", "a", "r1"] `shouldBe` replicate 4 (NFailed "nothing left to try")
    grClosedRoots (oRep o) `shouldBe` 0

  it "8: a confirmed FALSE fails the lemma at once" $ do
    o <- runG cfg {gcMaxSplits = 1} oneRoot
      [("r1", [bad]), ("l", [bad {rFinal = Just "tried\nFALSE: l fails at 0"}])]
      [ say Planner (plan [("l", "P")] "theorem r1 : A" "exact use l")
      , say Verifier accept
      , sayTo Verifier "fails at 0" "{\"false\": true, \"reason\": \"right\"}" ]
    [c | FalseLemma _ _ c <- oEvents o] `shouldBe` ["l fails at 0"]
    ["false l", "failed l"] `shouldSatisfy` (`infixOf` tags o)
    [why | NodeFailed _ i why <- oEvents o, i == nId (nodeBy o "l")] `shouldBe` ["false: l fails at 0"]
    statusOf o "l" `shouldBe` NFailed "false: l fails at 0"
    count "l" (oLaunched o) `shouldBe` 1
    count "proposed l" (tags o) `shouldBe` 0
    statusOf o "r1" `shouldBe` NFailed "nothing left to try"

  it "9: the dollar cap reached by the planner's reply does not start the verifier" $ do
    o <- runG cfg {gcCapUsd = 1} oneRoot
      [("r1", [bad {rCost = 0.25}])]
      [Reply Planner Nothing (Right (plan [("l", "P")] "theorem r1 : A" "exact use l", 2)) 0, say Verifier accept]
    map fst (oAsks o) `shouldBe` [Planner]
    grStopped (oRep o) `shouldBe` Just "dollars"
    Map.lookup Planner (grDollars (oRep o)) `shouldBe` Just 2
    Map.lookup Worker (grDollars (oRep o)) `shouldBe` Just 0.25
    count "stopped dollars" (tags o) `shouldBe` 1

  it "10: two roots proposing the same lemma at once end with one node" $ do
    o <- runG cfg twoRoots
      [("r1", [bad]), ("r2", [bad]), ("l", [ok {rDelay = 900}])]
      [ Reply Planner (Just (toNode "r1")) (Right (plan [("l", "P")] "theorem r1 : A" "exact use l", 0)) 100
      , Reply Planner (Just (toNode "r2")) (Right (plan [("l", "P")] "theorem r2 : B" "exact use l", 0)) 100
      , Reply Verifier (Just "theorem r1") (Right (accept, 0)) 0
      , Reply Verifier (Just "theorem r2") (Right (accept, 0)) 400 ]
    let ts = tags o
    precedes ts "proposed r2" "accepted r1" `shouldBe` True
    precedes ts "accepted r1" "accepted r2" `shouldBe` True
    length [() | n <- IM.elems (gNodes (grGraph (oRep o))), nName n == "l"] `shouldBe` 1
    IM.size (gNodes (grGraph (oRep o))) `shouldBe` 3
    count "shared l" ts `shouldBe` 1
    grClosedRoots (oRep o) `shouldBe` 2
    length [() | d <- declarations (grBase (oRep o)), dName d == "l"] `shouldBe` 1

  it "11: a sketch that fails its check is repaired on the second try" $ do
    o <- runG cfg oneRoot
      [("r1", [bad]), ("l", [ok])]
      [ say Planner (plan [("l", "P")] "theorem r1 : A" "BAD use l")
      , say Planner (plan [("l", "P")] "theorem r1 : A" "exact use l")
      , say Verifier accept ]
    let planners = [ms | (Planner, ms) <- oAsks o]
    length planners `shouldBe` 2
    map fst (last planners) `shouldBe` ["system", "user", "assistant", "user"]
    snd (last planners !! 2) `shouldBe` plan [("l", "P")] "theorem r1 : A" "BAD use l"
    (snd (last (last planners))) `shouldSatisfy` T.isPrefixOf "Lean reported:"
    (snd (last (last planners))) `shouldSatisfy` T.isInfixOf "error: BAD"
    count "proposed r1" (tags o) `shouldBe` 1
    statusOf o "r1" `shouldBe` NProved

  it "12: no credit ends the run at once" $ do
    o <- runG cfg twoRoots
      [("r1", [bad]), ("r2", [ok {rDelay = 60000}])]
      [Reply Planner Nothing (Left AskNoCredit) 0]
    grStopped (oRep o) `shouldBe` Just "credit"
    count "stopped credit" (tags o) `shouldBe` 1
    statusOf o "r2" `shouldBe` NOpen
    [arOutcome a | a <- grAttempts (oRep o), arGoal a == "r2"] `shouldBe` ["cancelled: credit"]

  it "13: stops at the time budget and cancels what runs" $ do
    o <- runG cfg {gcBudget = 50} twoRoots
      [("r1", [bad {rClock = 100}]), ("r2", [ok {rDelay = 60000}])]
      []
    grStopped (oRep o) `shouldBe` Just "time"
    [arOutcome a | a <- grAttempts (oRep o), arGoal a == "r2"] `shouldSatisfy` all (T.isPrefixOf "cancelled")
    oAsks o `shouldBe` []

  it "14: a closing sketch merges the node's proof without its unused lemmas" $ do
    o <- runG cfg oneRoot
      [("r1", [bad])]
      [say Planner (plan [("l", "P")] "theorem r1 : A" "exact done")]
    statusOf o "r1" `shouldBe` NProved
    map fst (oAsks o) `shouldBe` [Planner]
    [dName d | d <- declarations (grBase (oRep o))] `shouldBe` ["r1"]
    T.isInfixOf "sorry" (grBase (oRep o)) `shouldBe` False

  it "15: a node waiting on a goal that fails goes on to propose a split" $ do
    o <- runG cfg twoRoots
      [("r1", [w "exact use r2"]), ("r2", [bad {rDelay = 150}]), ("k", [ok])]
      ( replicate 4 (sayTo Planner (toNode "r2") garbage)
        <> [ sayTo Planner (toNode "r1") (plan [("k", "Q")] "theorem r1 : A" "exact use k"), say Verifier accept ] )
    statusOf o "r2" `shouldBe` NFailed "nothing left to try"
    precedes (tags o) "end r1" "failed r2" `shouldBe` True
    ["failed r2", "proposed r1"] `shouldSatisfy` (`infixOf` tags o)
    nAttempts (nodeBy o "r1") `shouldBe` 1
    lookup 1 (grWhiteboards (oRep o)) `shouldSatisfy` maybe False (T.isInfixOf "r1: waits on lemmas that did not close")
    statusOf o "r1" `shouldBe` NProved

  it "16: dollars of a paid call are counted when the next call throws" $ do
    o <- runG cfg {gcMaxSplits = 1} oneRoot
      [("r1", [bad])]
      [ Reply Planner Nothing (Right (garbage, 1.5)) 0
      , Reply Planner Nothing (Left (AskFailed "THROW")) 0 ]
    Map.lookup Planner (grDollars (oRep o)) `shouldBe` Just 1.5
    count "split failed r1" (tags o) `shouldBe` 1
    statusOf o "r1" `shouldBe` NFailed "nothing left to try"

  it "17: a sketch accepted after the base moved is checked again and merged into the new base" $ do
    o <- runG cfg twoRoots
      [("r1", [bad]), ("r2", [ok {rDelay = 150}]), ("l", [ok])]
      [ Reply Planner Nothing (Right (plan [("l", "P")] "theorem r1 : A" "exact use l", 0)) 400
      , say Verifier accept ]
    let sketchChecks = [p | p <- oProbes o, "#print axioms r1\n" `T.isInfixOf` p, "#print axioms l\n" `T.isInfixOf` p]
    length sketchChecks `shouldBe` 2
    T.isInfixOf "theorem r2 : B := by\n  exact done" (last sketchChecks) `shouldBe` True
    let base = grBase (oRep o)
    T.isInfixOf "theorem r2 : B := by\n  exact done" base `shouldBe` True
    T.isInfixOf "exact use l" base `shouldBe` True
    grClosedRoots (oRep o) `shouldBe` 2

  it "18: a split whose lemmas are proved but whose sketch is then rejected fails, keeping its lemmas" $ do
    o <- runGWith lateLean cfg {gcMaxSplits = 1} oneRoot
      [("r1", [bad]), ("l", [ok {rDelay = 200}])]
      [say Planner (plan [("l", "P")] "theorem r1 : A" "exact use l LATE"), say Verifier accept]
    statusOf o "l" `shouldBe` NProved
    sStatus (splitOf o "r1" 0) `shouldBe` SFailed "the sketch does not close with its lemmas proved"
    statusOf o "r1" `shouldBe` NFailed "nothing left to try"
    map (isOpen) (declsNamed o "l") `shouldBe` [False]

  it "18b: a split whose sketch still waits on another plan lemma stays pending and closes with it" $ do
    o <- runG cfg oneRoot
      [("r1", [bad]), ("a", [bad]), ("b", [ok {rDelay = 800}]), ("c", [ok]), ("d", [ok])]
      [ sayTo Planner (toNode "r1") (plan [("a", "P"), ("b", "Q")] "theorem r1 : A" "exact use a b")
      , sayTo Planner (toNode "a") (plan [("c", "R"), ("d", "S")] "lemma a : P" "exact use c d b")
      , say Verifier accept, say Verifier accept ]
    let ts = tags o
    precedes ts "proved c" "proved b" `shouldBe` True
    precedes ts "proved d" "proved b" `shouldBe` True
    count "split failed a" ts `shouldBe` 0
    sStatus (splitOf o "a" 0) `shouldBe` SDone
    statusOf o "a" `shouldBe` NProved
    statusOf o "r1" `shouldBe` NProved
    sStatus (splitOf o "r1" 0) `shouldBe` SDone

  it "19: a planner for a lemma is told which lemmas of the plan it may use" $ do
    o <- runG cfg {gcMaxSplits = 1} oneRoot
      [("r1", [bad]), ("a", [bad]), ("b", [ok {rDelay = 300}])]
      [ sayTo Planner (toNode "r1") (plan [("a", "P"), ("b", "Q")] "theorem r1 : A" "exact use a b")
      , sayTo Planner (toNode "a") garbage, sayTo Planner (toNode "a") garbage
      , say Verifier accept ]
    let firstA = head [ms | (Planner, ms) <- oAsks o, any (T.isInfixOf (toNode "a") . snd) ms]
    snd (firstA !! 1) `shouldSatisfy` T.isInfixOf "only these lemmas of the current plan, even where they still end in `sorry`: `b`."

  it "20: lemma nodes are named by their qualified names, not by the printed lead" $ do
    let file = T.unlines ["import Mathlib", "", "namespace A", "", "theorem r1 : X := by", "  sorry", "", "end A"]
    o <- runGWith univLean cfg file
      [("A.r1", [bad]), ("A.l", [ok])]
      -- the fake Lean sees a use only by the full name
      [say Planner (plan [("l", "P")] "theorem r1 : X" "exact use A.l"), say Verifier accept]
    [nName n | n <- IM.elems (gNodes (grGraph (oRep o)))] `shouldBe` ["A.r1", "A.l"]
    nKey (nodeBy o "A.l") `shouldBe` "P :="
    nKey (nodeBy o "A.r1") `shouldBe` "X :="
    statusOf o "A.r1" `shouldBe` NProved

  it "21: a planner reply with no blocks, and a verifier reply with no JSON, are each asked again with the reason" $ do
    o <- runG cfg oneRoot
      [("r1", [bad]), ("l", [ok])]
      [ say Planner garbage, say Planner (plan [("l", "P")] "theorem r1 : A" "exact use l")
      , say Verifier "Looks fine to me.", say Verifier accept ]
    let planners = [ms | (Planner, ms) <- oAsks o]
        verifiers = [ms | (Verifier, ms) <- oAsks o]
    map (map fst) planners `shouldBe` [["system", "user"], ["system", "user", "assistant", "user"]]
    snd (last planners !! 2) `shouldBe` garbage
    snd (last (last planners)) `shouldSatisfy` T.isPrefixOf "Your reply could not be used: the reply needs a blueprint block and a lean block"
    map (map fst) verifiers `shouldBe` [["system", "user"], ["system", "user", "assistant", "user"]]
    snd (last verifiers !! 2) `shouldBe` "Looks fine to me."
    snd (last (last verifiers)) `shouldSatisfy` T.isPrefixOf "Your reply could not be used: the verdict is not the JSON asked for"
    statusOf o "r1" `shouldBe` NProved

  it "22: a cap reached with no root left open is not the stop reason" $ do
    o <- runG cfg {gcMaxSplits = 0, gcCapUsd = 1} oneRoot
      [("r1", [bad {rFinal = Just "FALSE: A fails", rCost = 2}])]
      []
    oAsks o `shouldBe` []
    statusOf o "r1" `shouldBe` NFailed "nothing left to try"
    grStopped (oRep o) `shouldBe` Nothing
    count "stopped dollars" (tags o) `shouldBe` 0

-- * The compute controller in graph mode

ma, mb :: Text
ma = "example/model-a"
mb = "example/model-b"

-- | Two tiers, the controller on, no splits: each root gets two counted attempts.
tiered :: GraphConfig
tiered = cfg {gcTiers = [ma, mb], gcControl = True, gcMaxDepth = 0, gcMaxSplits = 0}

encodeT :: A.Value -> Text
encodeT = TE.decodeUtf8 . BL.toStrict . A.encode

field :: Text -> A.Value -> Maybe A.Value
field k = \case
  A.Object o -> KM.lookup (K.fromText k) o
  _ -> Nothing

outcomes :: Out -> [Text]
outcomes o = map arOutcome (grAttempts (oRep o))

stop8 :: Stop
stop8 = Stop StallTurns 8 0.42

controllerSpec :: Spec
controllerSpec = describe "the controller in graph mode" $ do
  it "plannerModel is the last tier when tiers are set, else the model given" $ do
    plannerModel [ma, mb] (Just "x") `shouldBe` Just mb
    plannerModel [] (Just "x") `shouldBe` Just "x"
    plannerModel [] Nothing `shouldBe` Nothing

  it "with no tiers and the controller off, the events and report of case 2 are today's" $ do
    o <- runG cfg oneRoot
      [("r1", [bad]), ("l1", [(w "exact done") {rDelay = 20}]), ("l2", [(w "exact done") {rDelay = 300}])]
      [say Planner (plan [("l1", "P"), ("l2", "Q")] "theorem r1 : A" "exact combine l1 l2"), say Verifier accept]
    map (encodeT . graphEventJson) (oEvents o) `shouldBe` goldenEvents2
    encodeT (graphReportJson [] (oRep o)) `shouldBe` goldenReport2
    map asModel (oSpecs o) `shouldBe` [Nothing, Nothing, Nothing]

  it "a node's workers take tiers a, b, b as a flat goal's attempts do; a model error is not verified and not counted" $ do
    o <- runG tiered oneRoot [("r1", [bad, ok {rFailure = Just RateLimited}, bad])] []
    map asModel (oSpecs o) `shouldBe` [Just ma, Just mb, Just mb]
    map asTier (oSpecs o) `shouldBe` [Just 0, Just 1, Just 1]
    [(i, k, m) | NodeTierChosen _ i _ k m <- oEvents o] `shouldBe` [(1, 0, ma), (2, 1, mb), (3, 1, mb)]
    [(f, t) | NodeEscalated _ _ f t <- oEvents o] `shouldBe` [(0, 1)]
    [() | NodeTierSkipped {} <- oEvents o] `shouldBe` []
    drop 1 (take 2 (outcomes o)) `shouldBe` ["model error: rate limited"]
    nAttempts (nodeBy o "r1") `shouldBe` 2
    statusOf o "r1" `shouldBe` NFailed "nothing left to try"
    map arModel (grAttempts (oRep o)) `shouldBe` [Just ma, Just mb, Just mb]
    map arTierOf (grAttempts (oRep o)) `shouldBe` [Just 0, Just 1, Just 1]

  it "a node's model errors are kept out of its attempts three times, then counted" $ do
    let rl = bad {rFailure = Just RateLimited}
    o <- runG tiered oneRoot [("r1", [rl, rl, bad, rl, rl])] []
    map asModel (oSpecs o) `shouldBe` [Just ma, Just ma, Just ma, Just mb, Just mb]
    count "model error: rate limited" (outcomes o) `shouldBe` 4
    nAttempts (nodeBy o "r1") `shouldBe` 2
    statusOf o "r1" `shouldBe` NFailed "nothing left to try"

  it "a stopped worker is logged, repaired, and its note kept past a model error for the next real worker" $ do
    let auto = defaultAutoConfig {acTactics = ["rfl", "simp_all"]}
        stopped = (w "intro x\n  exact BAD") {rStop = Just stop8, rLast = Just "I keep trying the same tactic."}
    o <- runGAuto keyLean (Just (\tr -> AutoEnv auto (fakeLsp tr fake0) Nothing)) tiered oneRoot
      [("r1", [stopped, bad {rFailure = Just RateLimited}, bad])] []
    [(a, why, turns, d) | NodeStopped _ a _ why turns d <- oEvents o] `shouldBe` [(1, "turns", 8, 0.42)]
    [a | NodeRepaired _ a _ _ _ <- oEvents o] `shouldSatisfy` elem 1
    let firstStop = [i | (i, NodeStopped {}) <- zip [0 :: Int ..] (oEvents o)]
        firstEnd = [i | (i, AttemptEnded _ 1 _ _) <- zip [0 :: Int ..] (oEvents o)]
    (take 1 firstStop < take 1 firstEnd) `shouldBe` True
    case map (partText . asBrief) (oSpecs o) of
      [b1, b2, b3] -> do
        b1 `shouldNotSatisfy` T.isInfixOf "stopped after"
        b2 `shouldSatisfy` T.isInfixOf "A previous attempt on this goal was stopped after 8 turns and $0.42 without progress on the file (turns)."
        b2 `shouldSatisfy` T.isInfixOf "The prover's last errors on the file:"
        b2 `shouldSatisfy` T.isInfixOf "Its last message:\nI keep trying the same tactic."
        -- the model-error worker gave the note back
        b3 `shouldSatisfy` T.isInfixOf "A previous attempt on this goal was stopped after 8 turns"
      bs -> expectationFailure ("briefs: " <> show (length bs))
    map arStopped (grAttempts (oRep o)) `shouldBe` [Just stop8, Nothing, Nothing]

  it "a worker's dollars are its ledger's when known, else the child's" $ do
    o <- runG tiered twoRoots [("r1", [ok {rCost = 9, rDollars = Just 0.42}]), ("r2", [ok {rCost = 0.3}])] []
    Map.lookup Worker (grDollars (oRep o)) `shouldBe` Just 0.72
    [d | RoleCost _ Worker d <- oEvents o] `shouldMatchList` [0.42, 0.3]
    map arCost (grAttempts (oRep o)) `shouldMatchList` [Just 0.42, Just 0.3]

  it "a worker cancelled at the time budget is priced (controller on), counted as worker dollars" $ do
    let price sp = pure (if asGoal sp == "r2" then Just 0.35 else Nothing)
        runs = [("r1", [bad {rClock = 100}]), ("r2", [ok {rDelay = 60000}])]
    o <- runGPriced price (\_ -> pure Nothing) keyLean Nothing tiered {gcBudget = 50} twoRoots runs []
    grStopped (oRep o) `shouldBe` Just "time"
    [(arOutcome a, arCost a) | a <- grAttempts (oRep o), arGoal a == "r2"] `shouldBe` [("cancelled: time", Just 0.35)]
    Map.lookup Worker (grDollars (oRep o)) `shouldBe` Just 0.35
    -- with the controller off the price is not asked
    o2 <- runGPriced price (\_ -> pure Nothing) keyLean Nothing cfg {gcBudget = 50} twoRoots runs []
    [arCost a | a <- grAttempts (oRep o2), arGoal a == "r2"] `shouldBe` [Nothing]

  it "the dollar cap is reached through a worker's ledger dollars" $ do
    o <- runG cfg {gcCapUsd = 1} oneRoot [("r1", [bad {rDollars = Just 2, rCost = 0}])]
      [say Planner (plan [("l1", "P")] "theorem r1 : A" "exact use l1"), say Verifier accept]
    Map.lookup Worker (grDollars (oRep o)) `shouldBe` Just 2
    oAsks o `shouldBe` []
    grStopped (oRep o) `shouldBe` Just "dollars"

  it "a model failure on a worker skips its tier for the whole run" $ do
    o <- runG tiered {gcSlots = 1} twoRoots [("r1", [ok {rFailure = Just Unavailable}, ok]), ("r2", [ok])] []
    [(asGoal s, asModel s) | s <- oSpecs o] `shouldBe` [("r1", Just ma), ("r2", Just mb), ("r1", Just mb)]
    [(m, why) | NodeTierSkipped _ m why <- oEvents o] `shouldBe` [(ma, "unavailable model")]
    take 1 (outcomes o) `shouldBe` ["model error: unavailable model"]
    grClosedRoots (oRep o) `shouldBe` 2

  it "with every tier skipped the open nodes give up and the run ends with no planner call" $ do
    o <- runG cfg {gcTiers = [ma], gcControl = True} oneRoot [("r1", [ok {rFailure = Just NoTools}])] []
    oLaunched o `shouldBe` ["r1"]
    oAsks o `shouldBe` []
    [(m, why) | NodeTierSkipped _ m why <- oEvents o] `shouldBe` [(ma, "no tool support")]
    statusOf o "r1" `shouldBe` NFailed "nothing left to try"

  it "the controller's events and report fields in JSON" $ do
    encodeT (graphEventJson (NodeTierChosen 2 7 4 1 mb)) `shouldBe`
      encodeT (A.object ["event" A..= ("tier chosen" :: Text), "t" A..= (2 :: Double), "attempt" A..= (7 :: Int), "node" A..= (4 :: Int), "tier" A..= (1 :: Int), "model" A..= mb])
    encodeT (graphEventJson (NodeStopped 2 7 4 "turns" 8 0.42)) `shouldBe`
      encodeT (A.object ["event" A..= ("attempt stopped" :: Text), "t" A..= (2 :: Double), "attempt" A..= (7 :: Int), "node" A..= (4 :: Int), "reason" A..= ("turns" :: Text), "turns" A..= (8 :: Int), "dollars" A..= (0.42 :: Double)])
    encodeT (graphEventJson (NodeEscalated 2 4 0 1)) `shouldBe`
      encodeT (A.object ["event" A..= ("escalated" :: Text), "t" A..= (2 :: Double), "node" A..= (4 :: Int), "from" A..= (0 :: Int), "to" A..= (1 :: Int)])
    encodeT (graphEventJson (NodeTierSkipped 2 ma "rate limited")) `shouldBe`
      encodeT (A.object ["event" A..= ("tier skipped" :: Text), "t" A..= (2 :: Double), "model" A..= ma, "reason" A..= ("rate limited" :: Text)])
    o <- runG tiered oneRoot [("r1", [bad {rStop = Just stop8, rDollars = Just 0.5}, ok {rDollars = Just 1.25}])] []
    let rep = graphReportJson [] (oRep o)
    field "escalations" rep `shouldBe` Just (A.Number 1)
    field "stopped_total" rep `shouldBe` Just (A.Number 1)
    field "dollars_by_tier" rep `shouldBe` Just (A.object ["0" A..= (0.5 :: Double), "1" A..= (1.25 :: Double)])
    case field "attempts" rep of
      Just (A.Array as) -> do
        map (field "model") (toList as) `shouldBe` [Just (A.String ma), Just (A.String mb)]
        map (field "tier") (toList as) `shouldBe` [Just (A.Number 0), Just (A.Number 1)]
        map (field "stopped") (toList as) `shouldBe` [Just (A.String "turns"), Just A.Null]
        map (field "dollars") (toList as) `shouldBe` [Just (A.Number 0.5), Just (A.Number 1.25)]
      other -> expectationFailure (show other)
  where
    toList = foldr (:) []


goldenEvents2 :: [Text]
goldenEvents2 =
  [ "{\"depth\":0,\"event\":\"node added\",\"name\":\"r1\",\"node\":1,\"t\":0}"
  , "{\"attempt\":1,\"event\":\"launched\",\"node\":1,\"t\":0}"
  , "{\"event\":\"cost\",\"role\":\"worker\",\"t\":0,\"usd\":0}"
  , "{\"attempt\":1,\"event\":\"finished\",\"node\":1,\"outcome\":\"rejected: does not compile: _fisher_search_F.lean:3:2: error: BAD\",\"t\":0}"
  , "{\"event\":\"split proposed\",\"node\":1,\"split\":1,\"t\":0}"
  , "{\"event\":\"cost\",\"role\":\"planner\",\"t\":0,\"usd\":0}"
  , "{\"event\":\"cost\",\"role\":\"verifier\",\"t\":0,\"usd\":0}"
  , "{\"event\":\"split accepted\",\"lemmas\":[2,3],\"split\":1,\"t\":0}"
  , "{\"depth\":1,\"event\":\"node added\",\"name\":\"l1\",\"node\":2,\"t\":0}"
  , "{\"depth\":1,\"event\":\"node added\",\"name\":\"l2\",\"node\":3,\"t\":0}"
  , "{\"attempt\":2,\"event\":\"launched\",\"node\":2,\"t\":0}"
  , "{\"attempt\":3,\"event\":\"launched\",\"node\":3,\"t\":0}"
  , "{\"event\":\"cost\",\"role\":\"worker\",\"t\":0,\"usd\":0}"
  , "{\"attempt\":2,\"event\":\"finished\",\"node\":2,\"outcome\":\"closed\",\"t\":0}"
  , "{\"event\":\"node proved\",\"node\":2,\"t\":0}"
  , "{\"event\":\"cost\",\"role\":\"worker\",\"t\":0,\"usd\":0}"
  , "{\"attempt\":3,\"event\":\"finished\",\"node\":3,\"outcome\":\"closed\",\"t\":0}"
  , "{\"event\":\"node proved\",\"node\":3,\"t\":0}"
  , "{\"event\":\"node proved\",\"node\":1,\"t\":0}" ]

goldenReport2 :: Text
goldenReport2 =
  "{\"attempts\":[{\"attempt\":1,\"backend\":\"fisher\",\"ended_s\":0,\"goal\":\"r1\",\"outcome\":\"rejected: does not compile: _fisher_search_F.lean:3:2: error: BAD\",\"outcome_class\":\"failed\",\"started_s\":0,\"stats\":{\"cost_usd\":0,\"exit\":0,\"input_tokens\":null,\"output_tokens\":null,\"tools\":{},\"turns\":null,\"wall_s\":0}},{\"attempt\":2,\"backend\":\"fisher\",\"ended_s\":0,\"goal\":\"l1\",\"outcome\":\"closed\",\"outcome_class\":\"closed\",\"started_s\":0,\"stats\":{\"cost_usd\":0,\"exit\":0,\"input_tokens\":null,\"output_tokens\":null,\"tools\":{},\"turns\":null,\"wall_s\":0}},{\"attempt\":3,\"backend\":\"fisher\",\"ended_s\":0,\"goal\":\"l2\",\"outcome\":\"closed\",\"outcome_class\":\"closed\",\"started_s\":0,\"stats\":{\"cost_usd\":0,\"exit\":0,\"input_tokens\":null,\"output_tokens\":null,\"tools\":{},\"turns\":null,\"wall_s\":0}}],\"dollars\":{\"planner\":0,\"total\":0,\"verifier\":0,\"worker\":0},\"elapsed_s\":0,\"goals_closed\":1,\"goals_total\":1,\"nodes_proved\":3,\"nodes_total\":3,\"splits_total\":1,\"stopped\":null,\"tree\":[{\"attempts\":1,\"depth\":0,\"id\":1,\"name\":\"r1\",\"splits\":[{\"id\":1,\"lemmas\":[{\"attempts\":0,\"depth\":1,\"id\":2,\"name\":\"l1\",\"splits\":[],\"status\":\"proved\"},{\"attempts\":0,\"depth\":1,\"id\":3,\"name\":\"l2\",\"splits\":[],\"status\":\"proved\"}],\"status\":\"done\"}],\"status\":\"proved\"}]}"
