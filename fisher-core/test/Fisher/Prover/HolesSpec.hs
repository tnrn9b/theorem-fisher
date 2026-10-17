module Fisher.Prover.HolesSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck

import Fisher.Prover (Prover (..), prover)
import Fisher.Prover.Holes
import Fisher.Prover.Kind

-- | (line, column, word, enclosing declaration)
at :: [Hole] -> [(Int, Int, Text, Maybe Text)]
at = map (\h -> (hLine h, hColumn h, hWord h, hDecl h))

wordsOf :: [Hole] -> [Text]
wordsOf = map hWord

spec :: Spec
spec = do
  describe "Lean" $ do
    let h = holes Lean4
    it "finds sorry and admit with line, column and the theorem" $
      at (h "theorem foo : 1 = 1 := by\n  sorry\n\nlemma bar : True := by admit\n")
        `shouldBe` [(2, 3, "sorry", Just "foo"), (4, 24, "admit", Just "bar")]
    it "ignores line comments, nested block comments and strings" $
      h "-- sorry\n/- a /- sorry -/ sorry -/\ndef s := \"sorry \\\" sorry\"\nexample : True := trivial\n" `shouldBe` []
    it "does not take identifier parts or a quote character for a hole" $
      h "def a := sorryAx\ndef b := Nat.sorry\ndef c := '\"'\ndef d := mysorry\ntheorem t := sorry\n"
        `shouldBe` [Hole 5 14 "sorry" (Just "t")]
    it "sees code after a doc comment" $
      wordsOf (h "/-- doc sorry -/\ntheorem t : True := by sorry") `shouldBe` ["sorry"]
    it "leaves hDecl empty for a hole outside a theorem" $
      at (h "def f : Nat := sorry\n") `shouldBe` [(1, 16, "sorry", Nothing)]
    it "does not attribute a hole in a def to the theorem before it" $
      at (h "theorem t : True := trivial\ndef f : Nat := sorry\n") `shouldBe` [(2, 16, "sorry", Nothing)]

  describe "Rocq" $ do
    let h = holes Rocq
    it "finds Admitted., admit and give_up with line, column and the lemma" $
      at (h "Lemma foo : 1 = 1.\nProof.\n  admit.\nAdmitted.\n\nTheorem bar : True.\nProof. give_up. Abort.\n")
        `shouldBe` [(3, 3, "admit", Just "foo"), (4, 1, "Admitted.", Just "foo"), (7, 8, "give_up", Just "bar")]
    it "names the other declaration keywords" $
      map hDecl (h "Proposition p : True. Admitted.\nCorollary c : True. Admitted.\nFact f : True. Admitted.\nRemark r : True. Admitted.\nExample e : True. Admitted.\n")
        `shouldBe` map Just ["p", "c", "f", "r", "e"]
    it "ignores a comment holding Admitted. (Review Focus)" $
      h "(* Admitted. *)\nLemma a : True. Proof. exact I. Qed.\n" `shouldBe` []
    it "ignores Admitted. inside a string inside a comment (Review Focus)" $
      h "(* \"(* Admitted. *)\" *)\nLemma a : True. Proof. exact I. Qed.\n" `shouldBe` []
    it "ignores a string in a comment that holds a comment closer (Review Focus)" $
      wordsOf (h "(* \"*)\" Admitted. *)\nLemma a : True. Admitted.\n") `shouldBe` ["Admitted."]
    it "ignores a nested comment (Review Focus)" $
      h "(* a (* b *) Admitted. *)\nLemma a : True. Proof. exact I. Qed.\n" `shouldBe` []
    it "reads the comment after a nested one as code once it closes" $
      wordsOf (h "(* a (* b *) c *) Admitted.\n") `shouldBe` ["Admitted."]
    it "ignores strings, with doubled quotes as escapes" $
      h "Definition s := \"say \"\"Admitted.\"\" admit\".\n" `shouldBe` []
    it "does not take Admitted as part of an identifier" $
      h "Lemma NotAdmitted. Admitted_x. Foo.Admitted. admitted. Admitted\n" `shouldBe` []
    it "needs the full stop after Admitted" $
      h "Lemma l : True. Admitted\n" `shouldBe` []

  describe "Isabelle" $ do
    let h = holes Isabelle
    it "finds sorry and oops with line, column and the lemma" $
      at (h "theory T imports Main begin\nlemma foo: \"x = x\"\n  sorry\n\ntheorem bar [simp]: \"y = y\"\n  oops\nend\n")
        `shouldBe` [(3, 3, "sorry", Just "foo"), (6, 3, "oops", Just "bar")]
    it "names a lemma with a locale context and skips anonymous ones" $
      map hDecl (h "lemma (in loc) l1: \"a\" sorry\nlemma \"b\" sorry\nlemma assumes \"c\" shows \"d\" sorry\nschematic_goal g: \"?x\" sorry\ncorollary c1: \"e\" oops\nproposition p1: \"f\" sorry\n")
        `shouldBe` [Just "l1", Nothing, Nothing, Just "g", Just "c1", Just "p1"]
    it "ignores nested comments, strings and cartouches" $
      h "(* sorry (* sorry *) sorry *)\nlemma a: \"\\<open>sorry\\<close> sorry\" by simp\n\\<open>sorry \\<open>oops\\<close> sorry\\<close>\n" `shouldBe` []
    it "ignores a comment line" $
      h "lemma a: \"x\" by simp \\<comment> sorry oops\nlemma b: \"y\" by simp\n" `shouldBe` []
    it "ignores a comment with a cartouche argument on later lines" $
      h "lemma a: \"x\" by simp \\<comment> \\<open>so\nsorry\\<close>\n" `shouldBe` []
    it "ignores sorry in a cartouche nested in a text block, and finds one in a lemma (Review Focus)" $
      at (h "text \\<open>A note: \\<open>lemma x sorry\\<close> and sorry here.\\<close>\nlemma foo: \"x = x\"\n  sorry\n")
        `shouldBe` [(3, 3, "sorry", Just "foo")]
    it "ignores txt blocks and the unicode cartouche" $
      h "lemma a: \"x\"\n  txt \\<open>sorry\\<close>\n  txt \8249sorry\8250\n  by simp\n" `shouldBe` []
    it "does not treat a symbol name as a hole" $
      h "lemma a: \"\\<sorry>\" by simp\n" `shouldBe` []

  describe "pvHoles" $
    it "is the scanner of the prover's kind" $
      map (\k -> pvHoles (prover k) "sorry Admitted. oops admit") [Lean4, Rocq, Isabelle]
        `shouldBe` [ holes Lean4 "sorry Admitted. oops admit"
                   , holes Rocq "sorry Admitted. oops admit"
                   , holes Isabelle "sorry Admitted. oops admit" ]

  describe "commenting out" $ modifyMaxSuccess (const 2000) $ do
    let prop kind wrap frags = forAll (genText frags) $ \(pre, mid, post) ->
          let keep = pre <> "\n" <> post
              wrapped = pre <> "\n" <> wrap mid <> "\n" <> post
              sig = map (\x -> (hWord x, hDecl x)) . holes kind
           in sig wrapped === sig keep
    it "Lean: wrapping any text in a comment adds no hole and keeps the others" $
      prop Lean4 (\t -> "/- " <> t <> " -/") leanFrags
    it "Rocq: wrapping any text in a comment adds no hole and keeps the others" $
      prop Rocq (\t -> "(* " <> t <> " *)") rocqFrags
    it "Isabelle: wrapping any text in a comment adds no hole and keeps the others" $
      prop Isabelle (\t -> "(* " <> t <> " *)") isabelleFrags
    it "Isabelle: wrapping any text in a cartouche adds no hole and keeps the others" $
      prop Isabelle (\t -> "text \\<open>" <> t <> "\\<close>") isabelleFrags

-- | Texts made of self-contained fragments (balanced comments, strings and
-- cartouches), so that a wrapper around them stays well formed.
genText :: [Text] -> Gen (Text, Text, Text)
genText frags = (,,) <$> block <*> block <*> block
  where
    block = T.intercalate "\n" <$> listOf line
    line = T.unwords <$> listOf1 (elements frags)

leanFrags, rocqFrags, isabelleFrags :: [Text]
leanFrags =
  [ "sorry", "admit", "theorem foo :", "lemma bar :", "def f", "x := by", "\"sorry\"", "-- sorry", "/- sorry -/"
  , "/- a /- sorry -/ b -/", "'\"'", "sorryAx", "example" ]
rocqFrags =
  [ "Admitted.", "admit.", "admit", "give_up.", "Lemma foo : True.", "Theorem bar.", "Definition d.", "Proof.", "Qed."
  , "\"Admitted.\"", "\"say \"\"admit\"\"\"", "(* Admitted. *)", "(* a (* b *) admit *)", "(* \"*)\" *)", "NotAdmitted." ]
isabelleFrags =
  [ "sorry", "oops", "lemma foo:", "theorem bar [simp]:", "lemma \"x\"", "by simp", "\"sorry\"", "(* sorry *)"
  , "(* a (* sorry *) b *)", "\\<open>sorry\\<close>", "\\<open>a \\<open>oops\\<close> b\\<close>", "text \\<open>sorry\\<close>"
  , "txt \\<open>oops\\<close>", "definition d" ]
