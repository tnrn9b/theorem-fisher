module Fisher.Search.SketchSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Data.Maybe (fromMaybe)
import Fisher.Search.Check (AxiomPolicy (..), LeanRun (..), standardAxioms)
import Fisher.Search.Lean (declarations, dStart, findDecl)
import Fisher.Search.FakeLean (fakeLean)
import Fisher.Search.Sketch

base :: Text
base = T.intercalate "\n"
  [ "import Mathlib"
  , ""
  , "namespace Foo"
  , ""
  , "theorem old : O := by"
  , "  sorry"
  , ""
  , "/-- the main one -/"
  , "theorem main (x : Nat) : P x := by"
  , "  sorry"
  , ""
  , "end Foo"
  ]

good :: Text
good = T.intercalate "\n"
  [ "lemma aux1 (x : Nat) : Q x := by sorry"
  , ""
  , "lemma aux2 (x : Nat) : R x :="
  , "  sorry"
  , ""
  , "theorem main (x : Nat) : P x := by"
  , "  exact combine (aux1 x) (aux2 x)"
  ]

parse :: Text -> Either SketchError Sketch
parse = parseSketch base "Foo.main"

spec :: Spec
spec = do
  describe "parseSketch" $ do
    it "reads a good two-lemma sketch" $
      parse good `shouldBe` Right (Sketch
        [ ("aux1", "lemma aux1 (x : Nat) : Q x := by sorry")
        , ("aux2", "lemma aux2 (x : Nat) : R x :=\n  sorry") ]
        "theorem main (x : Nat) : P x := by\n  exact combine (aux1 x) (aux2 x)")
    it "wants the node" $
      parse "lemma aux1 (x : Nat) : Q x := by sorry" `shouldBe` Left NoNodeDecl
    it "refuses a changed statement" $
      parse (T.replace "P x" "P (x + 1)" good) `shouldBe` Left NodeStatementChanged
    it "refuses a lemma with a proof" $
      parse (T.replace "by sorry" "by simp" good) `shouldBe` Left (LemmaNotOpen "aux1")
    it "refuses two lemmas of one name" $
      parse (T.replace "aux2" "aux1" good) `shouldBe` Left (DuplicateName "aux1")
    it "refuses a name the base has, qualified by the node's namespace" $
      parse (T.replace "aux2" "old" good) `shouldBe` Left (NameClash "Foo.old")
    it "refuses an axiom" $
      parse ("axiom bad : False\n\n" <> good) `shouldBe` Left (DeniedLine "axiom bad : False")
    it "refuses stray text" $
      parse ("by the way\n\n" <> good) `shouldBe` Left (ExtraText "by the way")
    it "lets open ... in stand above a lemma" $
      fmap (map fst . skLemmas) (parse ("open Nat in\n" <> good)) `shouldBe` Right ["aux1", "aux2"]
    it "keeps an open ... in line with its lemma" $
      fmap (map snd . skLemmas) (parse (T.replace "lemma aux2" "open Nat in\nlemma aux2" good))
        `shouldBe` Right ["lemma aux1 (x : Nat) : Q x := by sorry", "open Nat in\nlemma aux2 (x : Nat) : R x :=\n  sorry"]
    it "does not take main_aux for the node" $
      fmap (map fst . skLemmas) (parse (T.replace "theorem main" "lemma main_aux : S := by sorry\n\ntheorem main" good))
        `shouldBe` Right ["aux1", "aux2", "main_aux"]
    it "does not take Other.main for the node" $
      parse (T.replace "theorem main" "lemma Other.main : S := by sorry\n\ntheorem main" good)
        `shouldSatisfy` either (const False) (\sk -> map fst (skLemmas sk) == ["aux1", "aux2", "Other.main"] && "theorem main" `T.isPrefixOf` skNode sk)
    it "takes a lemma whose sorry is on the next line as open" $
      fmap (map fst . skLemmas) (parse (T.replace "theorem main" "lemma aux3 : S := by\n  sorry\n\ntheorem main" good))
        `shouldBe` Right ["aux1", "aux2", "aux3"]
    it "finds a duplicate after qualifying" $
      parse (T.replace "lemma aux2" "lemma Foo.aux1" good) `shouldBe` Left (DuplicateName "Foo.aux1")
    it "refuses a dangling open ... in" $
      parse (good <> "\n\nopen Nat in") `shouldBe` Left (ExtraText "open Nat in")
  describe "applySketch" $ do
    it "keeps the base node's attributes and puts a sketch open ... in above the doc comment" $ do
      let base2 = T.replace "theorem main" "@[simp] theorem main" base
          sk = T.replace "theorem main" "open Nat in\ntheorem main" good
      (parseSketch base2 "Foo.main" sk >>= applySketch base2 "Foo.main") `shouldBe` Right (T.intercalate "\n"
        [ "import Mathlib", "", "namespace Foo", "", "theorem old : O := by", "  sorry", ""
        , "lemma aux1 (x : Nat) : Q x := by sorry", ""
        , "lemma aux2 (x : Nat) : R x :=", "  sorry", ""
        , "open Nat in"
        , "/-- the main one -/"
        , "@[simp] theorem main (x : Nat) : P x := by"
        , "  exact combine (aux1 x) (aux2 x)"
        , ""
        , "end Foo" ])
    it "keeps attributes when the sketch has none" $ do
      let base2 = T.replace "theorem main" "@[simp] private theorem main" base
      fmap (T.isInfixOf "@[simp] private theorem main (x : Nat) : P x := by\n  exact combine") (parseSketch base2 "Foo.main" good >>= applySketch base2 "Foo.main")
        `shouldBe` Right True
    it "puts the lemmas above the node's doc comment and replaces the node" $
      (parse good >>= applySketch base "Foo.main") `shouldBe` Right (T.intercalate "\n"
        [ "import Mathlib"
        , ""
        , "namespace Foo"
        , ""
        , "theorem old : O := by"
        , "  sorry"
        , ""
        , "lemma aux1 (x : Nat) : Q x := by sorry"
        , ""
        , "lemma aux2 (x : Nat) : R x :="
        , "  sorry"
        , ""
        , "/-- the main one -/"
        , "theorem main (x : Nat) : P x := by"
        , "  exact combine (aux1 x) (aux2 x)"
        , ""
        , "end Foo" ])
  describe "aliasLemma" $ do
    it "replaces the proof by an exact" $
      fmap (aliasLemma "aux2" "Foo.other") (parse good) `shouldBe` Right (Sketch
        [ ("aux1", "lemma aux1 (x : Nat) : Q x := by sorry")
        , ("aux2", "lemma aux2 (x : Nat) : R x := by exact Foo.other") ]
        "theorem main (x : Nat) : P x := by\n  exact combine (aux1 x) (aux2 x)")
    it "keeps a one-line statement and its doc comment" $
      fmap (aliasLemma "aux1" "t") (parseSketch base "Foo.main" ("/-- d -/\nlemma aux1 : Q := sorry\n\n" <> T.unlines (drop 5 (T.lines good))))
        `shouldSatisfy` either (const False) (\s -> lookup "aux1" (skLemmas s) == Just "/-- d -/\nlemma aux1 : Q := by exact t")
    it "aliases a lemma with a multi-line statement" $
      fmap (aliasLemma "aux1" "t") (parseSketch base "Foo.main" (T.replace "lemma aux1 (x : Nat) : Q x := by sorry" "lemma aux1 (x : Nat)\n    (h : x > 0) : Q x := by sorry" good))
        `shouldSatisfy` either (const False) (\s -> lookup "aux1" (skLemmas s) == Just "lemma aux1 (x : Nat)\n    (h : x > 0) : Q x := by exact t")
  describe "typeKey" $ do
    it "drops either prefix" $ do
      typeKey "f" "f : A -> B" `shouldBe` "A -> B"
      typeKey "f" "@f : A -> B" `shouldBe` "A -> B"
    it "keeps a multi-line type whole" $
      typeKey "f" "@f : A ->\n  B ->\n  C" `shouldBe` "A ->\n  B ->\n  C"
    it "leaves a type without the prefix" $
      typeKey "f" "g : A" `shouldBe` "g : A"
    it "drops a universe suffix on the name" $ do
      typeKey "A.l" "@A.l.{u_1} : P" `shouldBe` "P"
      typeKey "A.l" "A.l.{u_1, v} : P" `shouldBe` "P"
  describe "removeDecls" $ do
    let file = T.intercalate "\n"
          [ "import Mathlib", "", "namespace Foo", ""
          , "/-- a dead one -/", "@[simp]", "lemma dead : D := by", "  sorry", ""
          , "open Nat in", "lemma dead2 : E := sorry", ""
          , "theorem main : P := by", "  exact keep", "", "end Foo" ]
    it "removes a declaration with its doc comment, attributes and one blank line" $
      removeDecls ["Foo.dead"] file `shouldBe` T.intercalate "\n"
        [ "import Mathlib", "", "namespace Foo", ""
        , "open Nat in", "lemma dead2 : E := sorry", ""
        , "theorem main : P := by", "  exact keep", "", "end Foo" ]
    it "removes an `open ... in` line above it, and several at once" $
      removeDecls ["Foo.dead2", "Foo.dead"] file `shouldBe` T.intercalate "\n"
        [ "import Mathlib", "", "namespace Foo", "", "theorem main : P := by", "  exact keep", "", "end Foo" ]
    it "leaves the file alone for a name not in it" $ do
      removeDecls ["dead", "Foo.nope"] file `shouldBe` file
      removeDecls [] file `shouldBe` file
  describe "mentions" $ do
    it "finds a name as a whole identifier, bare or qualified" $ do
      mentions "Foo.l" "exact l" `shouldBe` True
      mentions "Foo.l" "exact Foo.l x" `shouldBe` True
      mentions "Foo.l" "exact (l.symm)" `shouldBe` True
    it "not inside a longer identifier" $ do
      mentions "Foo.l" "exact l2 x" `shouldBe` False
      mentions "Foo.l" "exact al" `shouldBe` False
      mentions "Foo.l" "exact l'" `shouldBe` False
  describe "checkSketch" $ do
    let pol = AxiomPolicy standardAxioms False
        nodeTy = "Foo.main : theorem main (x : Nat) : P x :="
        run applied lemmas = withSystemTempDirectory "sk" $ \root -> do
          BS.writeFile (root </> "F.lean") (TE.encodeUtf8 base)
          checkSketch fakeLean pol root "F.lean" applied "Foo.main" nodeTy lemmas
        applied0 = either (error . show) id (parse good >>= applySketch base "Foo.main")
    it "passes, giving each lemma's printed type" $
      run applied0 ["aux1", "aux2"] `shouldReturn`
        Right (Map.fromList [("aux1", "Foo.aux1 : lemma aux1 (x : Nat) : Q x :="), ("aux2", "Foo.aux2 : lemma aux2 (x : Nat) : R x :=")])
    it "fails when it does not compile" $ do
      r <- run (T.replace "exact combine" "BAD combine" applied0) ["aux1", "aux2"]
      r `shouldSatisfy` either (T.isInfixOf "BAD") (const False)
    it "fails when the node is not waiting" $ do
      r <- run (T.replace "exact combine (aux1 x) (aux2 x)" "sorry" applied0) ["aux1", "aux2"]
      r `shouldBe` Left "still sorry"
    it "fails when a lemma has no printed type" $
      run applied0 ["aux1", "nope"] `shouldReturn` Left "no type printed for nope"
    it "refuses a node whose proof uses sorry itself" $ do
      let lr = LeanRun $ \root rel -> do
            (c, o) <- runLean fakeLean root rel
            t <- TE.decodeUtf8 <$> BS.readFile (root </> rel)
            let d = fromMaybe (error "no node") (findDecl "Foo.main" (declarations (fst (T.breakOn "\n\n#print axioms" t))))
            pure (c, T.pack (rel <> ":" <> show (dStart d + 1) <> ":8: warning: declaration uses `sorry`\n") <> o)
      r <- withSystemTempDirectory "sk" $ \root -> do
        BS.writeFile (root </> "F.lean") (TE.encodeUtf8 base)
        checkSketch lr pol root "F.lean" applied0 "Foo.main" nodeTy ["aux1", "aux2"]
      r `shouldSatisfy` either (T.isInfixOf "uses sorry") (const False)
