module Fisher.Search.ShapeSpec (spec) where

import Control.Exception (evaluate)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Fisher.Search.Shape

-- | Step.Cone.rel_kept from tools/bench/h/goals.json, as dStatement gives it.
relKept :: Text
relKept =
  T.concat
    [ "theorem rel_kept {E : Eval} (hE : LawfulEval E) {Dw : Draw} {n n' : Network}\n"
    , "    {keep : TAName → Prop} {cpl : ChannelCoupling}\n"
    , "    (hwf : WellFormed n) (hwf' : WellFormed n') (hcf' : GuardClockFree n') (hnames : ChannelNaming n)\n"
    , "    (hcone : ConeClosed cpl n keep) (hslice : MachinesTo keep n n') (hbody : NoBodyRecv n)\n"
    , "    (hdisp : LiveDispatch E n keep)\n"
    , "    {g g' : GState} (hrel : Rel E n n' keep g g')\n"
    , "    {c c' : Cand} (hc : c ∈ atMin (networkCands E g n)) (hc' : c' ∈ networkCands E g' n')\n"
    , "    (hkeep : keep c.ta) (hcc : CandSl (Gone n keep) c c') {s s' : Step}\n"
    , "    (hs : c.step = some s) (hs' : c'.step = some s') :\n"
    , "    (fireCand E Dw c s g).map (Except.map (fun x =>\n"
    , "        ({ x.1 with store := overlay (WithClock (KeptCell n')) x.1.store g'.store },\n"
    , "         slChange (WithClock (KeptCell n')) (GoneEnd (Gone n keep)) (g'.atTime c.fire).store x.2)))\n"
    , "      = fireCand E Dw c' s' g'\n"
    , "    ∧ ∀ g₁ Δ, Except.ok (g₁, Δ) ∈ (fireCand E Dw c s g).support →\n"
    , "        Rel E n n' keep g₁ { g₁ with store := overlay (WithClock (KeptCell n')) g₁.store g'.store }\n"
    , "        ∧ liveLocal (GoneEnd (Gone n keep))\n"
    , "            (slChange (WithClock (KeptCell n')) (GoneEnd (Gone n keep)) (g'.atTime c.fire).store Δ)\n"
    , "          = liveLocal (GoneEnd (Gone n keep)) Δ := by"
    ]

spec :: Spec
spec = do
  describe "shapeOf" $ do
    it "strips a leading forall: eq, no heads, small" $
      shapeOf "theorem t : \8704 n : \8469, n * 1 = n := by" `shouldBe` Shape "eq" [] [] "small"
    it "skips the binders and reads heads and namespaces in the type" $
      shapeOf "theorem s (s : Finset \8469) : Finset.sum s id \8804 Nat.choose 5 2 := by"
        `shouldBe` Shape "le" ["Finset.sum", "Nat.choose"] ["Finset", "Nat"] "small"
    it "a leading exists" $
      shConnective (shapeOf "theorem e : \8707 x : \8477, Real.sqrt x = 2 := by") `shouldBe` "exists"
    it "iff before and" $
      shConnective (shapeOf "theorem i (p q : Prop) : p \8743 q \8596 q \8743 p := by") `shouldBe` "iff"
    it "an arrow under a leading exists is not stripped" $
      shConnective (shapeOf "theorem e : \8707 x, P x \8594 x = 0 := by") `shouldBe` "exists"
    it "an operator after a later binder is not top level" $
      shConnective (shapeOf "theorem b : P \8743 \8704 x, x = x := by") `shouldBe` "and"
    it "arrows are stripped to the conclusion" $
      shConnective (shapeOf "lemma l : A \8594 B \8594 C := by") `shouldBe` "other"
    it "the conclusion after the last arrow decides" $
      shConnective (shapeOf "theorem h (a b : \8469) : a = b \8594 b \8804 a := by") `shouldBe` "le"
    it "an operator inside parentheses is not top level" $
      shConnective (shapeOf "theorem p : f (a = b) := by") `shouldBe` "other"
    it "the empty string" $
      shapeOf "" `shouldBe` Shape "other" [] [] "small"
    it "heads are the first three dotted identifiers, deduplicated; namespaces are all of them" $
      shapeOf "theorem m : Nat.succ a = Nat.succ b + Int.toNat c + List.length l + Finset.card s := by"
        `shouldBe` Shape "eq" ["Nat.succ", "Int.toNat", "List.length"] ["Finset", "Int", "List", "Nat"] "small"
    it "projections on local names are not heads" $
      shapeOf "theorem t (g : G) (x : A \215 B) : g.store c = Foo.bar x.1 := by" `shouldBe` Shape "eq" ["Foo.bar"] ["Foo"] "small"
    it "a dotted Mathlib-style name with several segments is a head" $
      shapeOf "theorem t : Step.Diamond.foo a = n.succ := by" `shouldBe` Shape "eq" ["Step.Diamond.foo"] ["Step"] "small"
    it "ge is le and gt is lt" $ do
      shConnective (shapeOf "theorem t : a \8805 b := by") `shouldBe` "le"
      shConnective (shapeOf "theorem t : a >= b := by") `shouldBe` "le"
      shConnective (shapeOf "theorem t : a > b := by") `shouldBe` "lt"
    it "a binder inside brackets does not hide the top-level operator" $ do
      let conn = shConnective . shapeOf
      conn "theorem t : Set.image (fun x => x + 1) s = t := by" `shouldBe` "eq"
      conn "theorem t : Finset.sum s (fun i => f i) = Nat.choose 5 2 := by" `shouldBe` "eq"
      conn "theorem t : (\8704 x, p x) \8743 q := by" `shouldBe` "and"
      conn "theorem t : (\8707 x, p x) \8596 q := by" `shouldBe` "iff"
      conn "theorem t : (\8704 x \8712 s, p x) \8594 a = b := by" `shouldBe` "eq"
    it "a membership in a binder is not mem" $ do
      let conn = shConnective . shapeOf
      conn "theorem t : \8704 x \8712 s, p := by" `shouldBe` "other"
      conn "theorem t : \8707 x \8712 s, p := by" `shouldBe` "exists"
      conn "theorem t : p \8594 \8704 x \8712 s, q x = 0 := by" `shouldBe` "eq"
    it "size boundaries at 19, 20, 60 and 61 words" $ do
      let sized n = shSize (shapeOf ("theorem w : " <> T.unwords (replicate (n - 2) "a") <> " = b := by"))
      map sized [19, 20, 60, 61] `shouldBe` ["small", "medium", "medium", "large"]
    it "a long bench statement is large" $
      shSize (shapeOf relKept) `shouldBe` "large"
    it "a medium statement" $
      shSize (shapeOf ("theorem w : " <> T.unwords (replicate 30 "a") <> " = b := by")) `shouldBe` "medium"
    it "never throws" $ do
      let inputs =
            [ ""
            , ":"
            , ":= by"
            , "theorem"
            , "theorem x :"
            , "theorem x : \8704"
            , "theorem x : \8704 a b,"
            , "theorem x : \8707"
            , "theorem x : \8704 \10627 a \10628, fun y => y = a := by"
            , "theorem x : {y | y \8712 s} = s := by"
            , "theorem x : (((( := by"
            , "theorem x : )))) \8594 \8594 := by"
            , "theorem x : a.b. = .c := by"
            , "theorem x : \"str : \\\" = \" = s -- c : d\n := by"
            , "theorem x /- a : b -/ : \945\8321 \8800 \946\8322 := by"
            , "\955 x, x"
            , "theorem x : a \8594"
            , "\8594"
            , T.replicate 2000 "("
            ]
      mapM_ (\s -> evaluate (T.length (renderShape (shapeOf s)))) inputs
  describe "renderShape and parseShape" $ do
    it "renders the canonical string" $
      renderShape (Shape "le" ["Finset.sum", "Nat.choose"] ["Finset", "Nat"] "medium")
        `shouldBe` "le|Finset.sum,Nat.choose|Finset,Nat|medium"
    it "renders empty lists as empty fields" $
      renderShape (Shape "eq" [] [] "small") `shouldBe` "eq|||small"
    it "round trips" $ do
      let shapes =
            [ Shape "le" ["Finset.sum", "Nat.choose"] ["Finset", "Nat"] "medium"
            , Shape "eq" [] [] "small"
            , shapeOf relKept
            ]
      map (parseShape . renderShape) shapes `shouldBe` map Just shapes
    it "rejects what is not a shape" $ do
      parseShape "" `shouldBe` Nothing
      parseShape "le|a|b" `shouldBe` Nothing
      parseShape "le|a|b|huge" `shouldBe` Nothing
      parseShape "nope|||small" `shouldBe` Nothing
