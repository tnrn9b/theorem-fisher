module Fisher.Search.FactsSpec (spec) where

import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Search.Facts
import Fisher.Search.Index

fixture :: FilePath
fixture = "test/fixtures/index-proj"

withIndex :: (Index -> IO a) -> IO a
withIndex k = withSystemTempDirectory "facts" $ \tmp -> do
  (ix, _) <- buildIndex fixture (tmp </> "index.json")
  k ix

names :: [Fact] -> [Text]
names = map (eName . fEntry)

tagged :: [Fact] -> [(Text, Text)]
tagged = map (\f -> (eName (fEntry f), fWhy f))

whys :: Text -> [Fact] -> [Text]
whys w = names . filter ((== w) . fWhy)

-- | A synthetic one-module index from (name, statement, line).
synthetic :: [(Text, Text, Int)] -> Index
synthetic ds = Index es (M.fromList [("A", [])]) (M.fromList (zip (map entryKey es) [0 ..]))
  where
    es = [Entry n KTheorem s "A.lean" l "A" [] False | (n, s, l) <- ds]

spec :: Spec
spec = do
  describe "ppConstants" $ do
    it "strips '@' and universe levels, keeps dotted and private names, drops numerals" $
      ppConstants "@Eq.{u+1, max v w} Nat (@Proj.bump.{0} c) (OfNat.ofNat 3) (_private.Proj.Top.0.twin) x.1"
        `shouldBe` ["Eq", "Nat", "Proj.bump", "c", "OfNat.ofNat", "_private.Proj.Top.0.twin", "x"]
    it "leaves out the names bound by binders" $
      ppConstants "∀ (bump double : Nat) {α : Type} [inst : Sized α], @Eq.{1} Nat bump (Proj.double double)"
        `shouldBe` ["Nat", "Type", "Sized", "Eq", "Proj.double"]

  describe "factsFor on the fixture project" $ do
    let cfg = defaultFacts
    it "gives the direct constants first, by exact index name, then one hop, then keyword matches" $
      withIndex $ \ix -> do
        let ty = "∀ (bump : Nat) (c : Proj.Counter), @Eq.{1} Prop (@Proj.Extra.uses_inner.{0} c) (Counter.value_mk bump)"
            fs = factsFor cfg ix Nothing "Proj/Top.lean" 10 "theorem top_no_open : base = 3 :=" (Just ty)
        take 3 (tagged fs)
          `shouldBe` [("Proj.Counter", "uses"), ("Proj.Extra.uses_inner", "uses"), ("Proj.Inner.bump_pos", "one hop")]
        -- a binder and a short name shared with project declarations are not facts
        whys "uses" fs `shouldBe` ["Proj.Counter", "Proj.Extra.uses_inner"]
        map fWhy (drop 3 fs) `shouldSatisfy` all (== "matches")
        whys "matches" fs `shouldSatisfy` (\ms -> "Proj.Extra.base" `elem` ms && "Proj.Extra.base_eq" `elem` ms && "top_uses" `elem` ms)
        length (names fs) `shouldBe` length (M.keys (M.fromList [(n, ()) | n <- names fs]))

    it "orders keyword facts by BM25" $
      withIndex $ \ix -> do
        let fs = factsFor cfg ix Nothing "Proj/Top.lean" 10 "theorem q : base = 3 :=" Nothing
        -- term frequency of "base" 3, 2, 1; same_line and through_option are below the goal
        tagged fs `shouldBe` [("Proj.Extra.base_eq", "matches"), ("Proj.Extra.base", "matches"), ("top_uses", "matches")]

    it "with no type gives keyword facts only" $
      withIndex $ \ix -> do
        let fs = factsFor cfg ix Nothing "Proj/Top.lean" 10 "theorem top_no_open : base = 3 :=" Nothing
        fs `shouldSatisfy` (not . null)
        map fWhy fs `shouldSatisfy` all (== "matches")

    it "never offers the goal itself or anything below it in the same file" $
      withIndex $ \ix -> do
        let ty = "∀ (n : Nat) (k : Proj.Counter), @Eq.{1} Nat (Proj.Counter.value (Proj.Counter.mk n)) (Proj.spin (Proj.nonrec_id n)) ∧ Proj.Counter.value_mk = top_level_fact"
            fs = factsFor cfg ix Nothing "Proj/Basic.lean" 61 "theorem Counter.value_mk (n : Nat) : (Counter.mk n).value = n :=" (Just ty)
        whys "uses" fs `shouldBe` ["Proj.Counter"]
        names fs `shouldSatisfy` all (`notElem` ["Proj.Counter.value_mk", "Proj.nonrec_id", "Proj.spin", "top_level_fact"])
        map (eLine . fEntry) fs `shouldSatisfy` all (< 61)

    it "never offers a module outside the import closure" $
      withIndex $ \ix -> do
        let fs = factsFor cfg ix Nothing "Proj/Use.lean" 23 "theorem uses_inner (c : Counter) : top full same line base =" (Just "top_full ∧ same_line ∧ Proj.Extra.base")
        whys "uses" fs `shouldBe` ["Proj.Extra.base"]
        map (eFile . fEntry) fs `shouldSatisfy` all (/= "Proj/Top.lean")

    it "offers a private declaration only in its own file" $
      withIndex $ \ix -> do
        let ty = "@Eq.{0} Prop _private.Proj.Basic.0.Proj.hidden_helper True"
            here = factsFor cfg ix Nothing "Proj/Basic.lean" 61 "theorem value_mk : hidden helper" (Just ty)
            there = factsFor cfg ix Nothing "Proj/Use.lean" 11 "theorem sees_private : hidden helper" (Just ty)
            top = factsFor cfg ix Nothing "Proj/Top.lean" 26 "theorem mathlib_name : twin" Nothing
        whys "uses" here `shouldBe` ["Proj.hidden_helper"]
        names there `shouldSatisfy` notElem "Proj.hidden_helper"
        [eFile (fEntry f) | f <- top, eName (fEntry f) == "twin"] `shouldBe` ["Proj/Top.lean"]

    it "in a file importing nothing from the project gives keyword facts from above the goal only" $
      withIndex $ \ix -> do
        let fs = factsFor cfg ix Nothing "Proj/Basic.lean" 42 "theorem bump_value : (bump c).value = c.value + 1 :=" (Just "@Eq.{1} Nat (Nat.succ n) (HAdd.hAdd n 1)")
        fs `shouldSatisfy` (not . null)
        whys "uses" fs `shouldBe` []
        whys "one hop" fs `shouldBe` []
        map (\f -> (eFile (fEntry f), eLine (fEntry f) < 42)) fs `shouldSatisfy` all (== ("Proj/Basic.lean", True))
        names fs `shouldSatisfy` elem "Proj.bump"

    it "finds nothing for a file outside the index" $
      withIndex $ \ix ->
        factsFor cfg ix Nothing "Other/Nowhere.lean" 5 "theorem bump : base" (Just "Proj.bump") `shouldBe` []

  describe "keyword score" $ do
    it "keeps a keyword fact only at 0.4 of the top keyword score or more" $ do
      let ix = synthetic ([("rare", "theorem rare : zeta = zeta :=", 1)] <> [("c" <> T.pack (show i), "theorem c" <> T.pack (show i) <> " : foo = foo :=", 1 + i) | i <- [1 .. 8 :: Int]])
      fcKeywordRatio defaultFacts `shouldBe` 0.4
      names (factsFor defaultFacts ix Nothing "A.lean" 100 "theorem goal : zeta = foo :=" Nothing) `shouldBe` ["rare"]
      length (factsFor defaultFacts {fcKeywordRatio = 0} ix Nothing "A.lean" 100 "theorem goal : zeta = foo :=" Nothing) `shouldBe` 9

  describe "goal module" $ do
    -- module B lives in A/B.lean (srcDir A); module A.B lives in src/A/B.lean
    let e n f m l = Entry n KTheorem ("theorem " <> n <> " : foo :=") f l m [] False
        es = [e "inA" "src/A/B.lean" "A.B" 1, e "inB" "A/B.lean" "B" 1]
        ix = Index es (M.fromList [("A.B", []), ("B", [])]) (M.fromList (zip (map entryKey es) [0 ..]))
        go mm file = names (factsFor defaultFacts ix mm file 100 "theorem goal : foo :=" Nothing)
    it "uses the module given exactly" $ do
      go (Just "B") "whatever.lean" `shouldBe` ["inB"]
      go (Just "A.B") "A/B.lean" `shouldBe` ["inA"]
      go (Just "Missing") "A/B.lean" `shouldBe` []
    it "without one, matches the file exactly against the entries' files before the suffix match" $ do
      go Nothing "A/B.lean" `shouldBe` ["inB"]
      go Nothing "src/A/B.lean" `shouldBe` ["inA"]
      go Nothing "/abs/proj/src/A/B.lean" `shouldBe` ["inA"]

  describe "caps" $ do
    it "holds the 20-entry cap" $ do
      let ix = synthetic [("t" <> T.pack (show i), "theorem t" <> T.pack (show i) <> " : foo = foo :=", i) | i <- [1 .. 30 :: Int]]
          fs = factsFor defaultFacts ix Nothing "A.lean" 100 "theorem goal : foo = foo :=" Nothing
      length fs `shouldBe` 20
    it "holds the 8,000-character cap, dropping whole entries" $ do
      let long i = "theorem t" <> T.pack (show i) <> " : foo = " <> T.replicate 480 "x" <> " :="
          ix = synthetic [("t" <> T.pack (show i), long i, i) | i <- [1 .. 30 :: Int]]
          fs = factsFor defaultFacts ix Nothing "A.lean" 100 "theorem goal : foo :=" Nothing
          body = factsBrief fs
          entryChars = T.length body - T.length "Relevant project facts:\n"
          oneMore = factsFor defaultFacts {fcChars = 1000000} ix Nothing "A.lean" 100 "theorem goal : foo :=" Nothing
      length fs `shouldSatisfy` (\n -> n > 0 && n < 20)
      entryChars `shouldSatisfy` (<= 8000)
      T.length (factsBrief (take (length fs + 1) oneMore)) - T.length "Relevant project facts:\n" `shouldSatisfy` (> 8000)
      fs `shouldBe` take (length fs) oneMore
      [l | l <- drop 1 (T.lines body), not (" (A.lean:" `T.isInfixOf` l && ")" `T.isSuffixOf` l)] `shouldBe` []

  describe "header-only kinds" $ do
    it "take at most 5 of the entries; theorems fill the rest in order" $ do
      let kinds = cycle [KStructure, KInductive, KClass, KAbbrev]
          heads = [Entry ("S" <> T.pack (show i)) k ("structure S" <> T.pack (show i) <> " where") "A.lean" i "A" [] False | (i, k) <- zip [1 .. 10 :: Int] kinds]
          thms = [Entry ("t" <> T.pack (show i)) KTheorem ("theorem t" <> T.pack (show i) <> " : foo = foo :=") "A.lean" (10 + i) "A" [] False | i <- [1 .. 10 :: Int]]
          defs = [Entry "d1" KDef "def d1 : foo := foo" "A.lean" 30 "A" [] False]
          es = heads <> thms <> defs
          ix = Index es (M.fromList [("A", [])]) (M.fromList (zip (map entryKey es) [0 ..]))
          ty = T.intercalate " -> " ["S" <> T.pack (show i) | i <- [1 .. 10 :: Int]] <> " -> d1 = d1"
          fs = factsFor defaultFacts ix Nothing "A.lean" 100 "theorem goal : foo = foo :=" (Just ty)
      fcHeaderMax defaultFacts `shouldBe` 5
      take 6 (names fs) `shouldBe` ["S1", "S2", "S3", "S4", "S5", "d1"]
      length [() | f <- fs, eKind (fEntry f) `elem` [KStructure, KInductive, KClass, KAbbrev]] `shouldBe` 5
      sort (drop 6 (names fs)) `shouldBe` sort ["t" <> T.pack (show i) | i <- [1 .. 10 :: Int]]
    it "the cap counts across the tiers" $ do
      let e n k l = Entry n k (kindText k <> " " <> n <> " foo where") "A.lean" l "A" [] False
          es = [e ("S" <> T.pack (show i)) KStructure i | i <- [1 .. 8 :: Int]]
          ix = Index es (M.fromList [("A", [])]) (M.fromList (zip (map entryKey es) [0 ..]))
          fs = factsFor defaultFacts ix Nothing "A.lean" 100 "theorem goal : foo :=" (Just "S1 -> S2 -> S3 -> foo")
      -- three direct, then keyword matches: five in all
      length fs `shouldBe` 5

  describe "factsBrief" $ do
    it "is empty when nothing was found" $ factsBrief [] `shouldBe` ""
    it "renders a header and one line per fact" $ do
      let e = Entry "Proj.Inner.bump_pos" KLemma "lemma bump_pos (c : Counter) :\n    0 < (bump c).value :=" "Proj/Basic.lean" 48 "Proj.Basic" [] False
      factsBrief [Fact e "uses"]
        `shouldBe` "Relevant project facts:\n- Proj.Inner.bump_pos : lemma bump_pos (c : Counter) : 0 < (bump c).value (Proj/Basic.lean:48)\n"
