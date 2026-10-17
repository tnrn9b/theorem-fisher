module Fisher.Search.LeanSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Fisher.Search.Lean

file :: T.Text
file = T.unlines
  [ "import Mathlib"
  , ""
  , "namespace Foo"
  , ""
  , "/-- A doc"
  , "    over two lines. -/"
  , "@[simp] theorem one (n : Nat) :"
  , "    n + 0 = n := by"
  , "  sorry"
  , ""
  , "section Bar"
  , "private lemma two : True := by"
  , "  trivial"
  , "end Bar"
  , ""
  , "theorem three (h : a := b) : 1 = 1 := sorry"
  , ""
  , "theorem _root_.four : 2 = 2 := by sorry"
  , ""
  , "theorem five : (3 : Nat) = 3 :="
  , "  rfl"
  , "end Foo"
  , ""
  , "namespace A.B"
  , "theorem six : True := by"
  , "  -- a comment"
  , "  sorry"
  , "  "
  , "end A.B"
  , "theorem seven : True := by"
  , "  exact trivial"
  ]

spec :: Spec
spec = describe "the Lean scanner" $ do
  it "does not cut a statement at a := inside a multi-line structure update" $ do
    let src = T.unlines
          [ "theorem third (g : Nat) :"
          , "    f g = (h g).map (fun x =>"
          , "      { x with pre := 1,"
          , "               post := 2 }) := by"
          , "  sorry" ]
    map dName (openGoals src) `shouldBe` ["third"]
    map statementHead (declarations src) `shouldSatisfy` all (T.isInfixOf "post := 2")
  let ds = declarations file
      byName n = maybe (error (T.unpack n)) id (findDecl n ds)
  it "names declarations through namespaces, sections and _root_" $
    map dName ds `shouldBe` ["Foo.one", "Foo.two", "Foo.three", "four", "Foo.five", "A.B.six", "seven"]
  it "finds the open goals, tactic and term" $
    map dName (openGoals file) `shouldBe` ["Foo.one", "Foo.three", "four", "A.B.six"]
  it "keeps the statement through := by, across lines" $ do
    dStatement (byName "Foo.one") `shouldBe` "theorem one (n : Nat) :\n    n + 0 = n := by"
    statementHead (byName "Foo.one") `shouldBe` "theorem one (n : Nat) :\n    n + 0 = n :="
  it "skips := inside brackets" $
    dStatement (byName "Foo.three") `shouldBe` "theorem three (h : a := b) : 1 = 1 :="
  it "places the doc comment and attributes" $ do
    let d = byName "Foo.one"
    (dDocStart d, dStart d, dStmtEnd d, dEnd d) `shouldBe` (4, 6, 7, 9)
  it "marks private declarations" $
    dPrivate (byName "Foo.two") `shouldBe` True
  it "trims trailing blank lines off a proof and ignores comments" $ do
    let d = byName "A.B.six"
    dEnd d `shouldBe` 27
    proofCode d `shouldBe` ["sorry"]
  it "reads a term proof on the next line" $
    proofCode (byName "Foo.five") `shouldBe` ["rfl"]
