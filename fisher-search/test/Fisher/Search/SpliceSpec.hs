module Fisher.Search.SpliceSpec (spec) where

import Data.List (foldl')
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Fisher.Search.Lean
import Fisher.Search.Splice

start :: T.Text
start = T.unlines
  [ "import Mathlib"
  , ""
  , "def f (n : Nat) : Nat := n + 1"
  , ""
  , "/-- a -/"
  , "theorem a : f 0 = 1 := by"
  , "  sorry"
  , ""
  , "theorem b : f 1 = 2 := by"
  , "  sorry"
  , ""
  , "theorem c : f 2 = 3 := by"
  , "  sorry"
  ]

edit :: T.Text -> T.Text -> T.Text -> T.Text
edit name proof = replaceProof name proof

-- | The file with one theorem's sorry replaced.
replaceProof :: T.Text -> T.Text -> T.Text -> T.Text
replaceProof name proof txt =
  let ls = T.splitOn "\n" txt
      d = maybe (error ("no " <> T.unpack name)) id (findDecl name (declarations txt))
   in T.intercalate "\n" (take (dStmtEnd d + 1) ls <> ["  " <> proof] <> drop (dEnd d) ls)

isDenied :: Either SpliceError Spliced -> Bool
isDenied = either (\case Denied _ -> True; _ -> False) (const False)

ok :: Either SpliceError Spliced -> T.Text
ok = either (error . show) spText

spec :: Spec
spec = describe "splicing a proof into the base" $ do
  it "puts the goal's proof into the base" $ do
    let agent = edit "b" "rfl" start
    ok (splice start agent start "b") `shouldBe` agent
  it "applies to a base that moved on" $ do
    let base = edit "a" "decide" start
        agent = edit "b" "rfl" start
    ok (splice start agent base "b") `shouldBe` edit "b" "rfl" base
  it "drops the agent's edits to other goals" $ do
    let agent = edit "c" "simp" (edit "b" "rfl" start)
    ok (splice start agent start "b") `shouldBe` edit "b" "rfl" start
  it "puts added helpers just above the goal's doc comment" $ do
    let agent = T.replace "/-- a -/" "theorem helper : 1 = 1 := rfl\n\n/-- a -/" (edit "a" "exact helper ▸ rfl" start)
        r = splice start agent start "a"
    fmap spHelpers r `shouldBe` Right ["helper"]
    let out = ok r
    T.isInfixOf "theorem helper : 1 = 1 := rfl\n\n/-- a -/\ntheorem a" out `shouldBe` True
  it "moves a helper the agent put elsewhere to above the goal" $ do
    let agent = edit "c" "exact h3" start <> "\ntheorem h3 : f 2 = 3 := rfl\n"
        out = ok (splice start agent start "c")
    T.breakOn "theorem h3" out `shouldSatisfy` (\(pre, _) -> not (T.isInfixOf "theorem c" pre))
  it "refuses a changed statement" $
    splice start (T.replace "f 1 = 2" "f 1 = f 1" (edit "b" "rfl" start)) start "b" `shouldBe` Left StatementChanged
  it "accepts := where the original had := by" $ do
    let agent = T.replace "theorem b : f 1 = 2 := by\n  sorry" "theorem b : f 1 = 2 :=\n  rfl" start
    ok (splice start agent start "b") `shouldBe` agent
  it "refuses a proof that is still sorry" $
    splice start start start "b" `shouldBe` Left TargetStillOpen
  it "refuses a change outside the declarations" $
    splice start (T.replace "n + 1" "n + 2" (edit "b" "rfl" start)) start "b"
      `shouldSatisfy` either (\case OutsideChanged _ -> True; _ -> False) (const False)
  it "refuses a removed declaration" $
    splice start (T.replace "theorem c : f 2 = 3 := by\n  sorry" "" (edit "b" "rfl" start)) start "b"
      `shouldSatisfy` either (\case OutsideChanged _ -> True; _ -> False) (const False)
  it "refuses a helper whose name the base already has" $ do
    let base = T.replace "/-- a -/" "theorem helper : 2 = 2 := rfl\n\n/-- a -/" start
        agent = T.replace "theorem b" "theorem helper : 1 = 1 := rfl\n\ntheorem b" (edit "b" "rfl" start)
    splice start agent base "b" `shouldBe` Left (NameClash "helper")
  it "does not add a helper the base already has word for word" $ do
    let helper = "theorem helper : 1 = 1 := rfl"
        agent = T.replace "theorem b" (helper <> "\n\ntheorem b") (edit "c" "exact helper ▸ rfl" (edit "b" "exact helper ▸ rfl" start))
        first = ok (splice start agent start "b")
        twice = ok (splice start agent first "c")
    length (T.breakOnAll helper twice) `shouldBe` 1
  it "refuses an added axiom, #exit and kernel-off options" $ do
    let with l = T.replace "theorem b" (l <> "\n\ntheorem b") (edit "b" "rfl" start)
    splice start (with "axiom cheat : False") start "b" `shouldSatisfy` isDenied
    splice start (with "#exit") start "b" `shouldSatisfy` isDenied
    splice start (with "set_option debug.skipKernelTC true") start "b" `shouldSatisfy` isDenied
    splice start (edit "b" "set_option debug.skipKernelTC true in rfl" start) start "b" `shouldSatisfy` isDenied
  it "refuses a helper that uses sorry (the agent's own new goal)" $ do
    let agent = edit "b" "rfl" start <> "\ntheorem other : 2 = 2 := by\n  sorry\n"
    splice start agent start "b" `shouldSatisfy` isDenied
    splice start (edit "b" "rfl" start <> "\ntheorem other : 2 = 2 := by\n  exact (by admit)\n") start "b" `shouldSatisfy` isDenied
  it "keeps set_option ... in above the goal" $ do
    let agent = T.replace "theorem b" "set_option maxHeartbeats 400000 in\ntheorem b" (edit "b" "rfl" start)
    ok (splice start agent start "b") `shouldSatisfy` T.isInfixOf "set_option maxHeartbeats 400000 in\ntheorem b"
  it "merges attempts on different goals in any order to the same file" $
    property $ \(Blind order) ->
      let goals = ["a", "b", "c"]
          proofs = zip goals ["decide", "rfl", "simp [f]"]
          attempts = [(g, edit g p start) | (g, p) <- proofs]
          perm = permute order attempts
          final = foldl' (\base (g, agent) -> ok (splice start agent base g)) start perm
          expected = foldl' (\t (g, p) -> edit g p t) start proofs
       in final === expected

-- | A permutation of a list from a seed.
permute :: [Int] -> [a] -> [a]
permute seeds xs = go (cycle (if null seeds then [0] else seeds)) xs
  where
    go _ [] = []
    go (s : ss) ys = let i = s `mod` length ys in ys !! i : go ss (take i ys <> drop (i + 1) ys)
    go [] ys = ys
