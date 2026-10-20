module Fisher.Search.LibrarySpec (spec) where

import Control.Concurrent.Async (forConcurrently_)
import Data.Aeson (decodeStrict)
import qualified Data.ByteString as BS
import Data.Char (isHexDigit)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Search.Index
import Fisher.Search.Library

-- | A synthetic index: Proj.Basic declares bump and base; the target
-- file Proj/Top.lean (importing it) declares the two goals and a helper.
ix0 :: Index
ix0 = mkIndex
  [ ent "Proj.bump" KDef "def bump (n : Nat) : Nat :=" "Proj/Basic.lean" 3 "Proj.Basic"
  , ent "Proj.base" KDef "def base : Nat :=" "Proj/Basic.lean" 5 "Proj.Basic"
  , ent "Proj.spin" KDef "def spin (n : Nat) : Nat :=" "Proj/Basic.lean" 7 "Proj.Basic"
  , ent "Proj.goal_one" KTheorem "theorem goal_one (n : Nat) : bump n > 0 :=" "Proj/Top.lean" 5 "Proj.Top"
  , ent "Proj.goal_two" KTheorem "theorem goal_two : base = 3 :=" "Proj/Top.lean" 8 "Proj.Top"
  , ent "Proj.in_file" KTheorem "theorem in_file : spin 0 = 0 :=" "Proj/Top.lean" 11 "Proj.Top"
  , (ent "Proj.secret" KDef "private def secret : Nat :=" "Proj/Basic.lean" 9 "Proj.Basic") {ePrivate = True}
  , ent "Proj.far" KDef "def far : Nat :=" "Proj/Far.lean" 1 "Proj.Far"
  ]

ent :: Text -> Kind -> Text -> FilePath -> Int -> Text -> Entry
ent n k s f l m = Entry n k s f l m [] False

mkIndex :: [Entry] -> Index
mkIndex es =
  Index es (M.fromList [("Proj.Basic", []), ("Proj.Top", ["Proj.Basic"]), ("Proj.Far", [])]) (M.fromList (zip (map entryKey es) [0 ..]))

start :: Text
start = T.unlines
  [ "import Proj.Basic"
  , ""
  , "namespace Proj"
  , ""
  , "theorem goal_one (n : Nat) : bump n > 0 := by"
  , "  sorry"
  , ""
  , "theorem goal_two : base = 3 := by"
  , "  sorry"
  , ""
  , "theorem in_file : spin 0 = 0 := by"
  , "  rfl"
  , ""
  , "end Proj"
  ]

merged :: Text
merged = T.unlines
  [ "import Proj.Basic"
  , ""
  , "namespace Proj"
  , ""
  , "theorem bump_helper (n : Nat) : bump n = n + 1 := by"
  , "  simp [bump]"
  , ""
  , "theorem bad_helper : base = 3 := by"
  , "  sorry"
  , ""
  , "theorem goal_one (n : Nat) : bump n > 0 := by"
  , "  rw [bump_helper]"
  , "  omega"
  , ""
  , "theorem goal_two : base = 3 := by"
  , "  sorry"
  , ""
  , "theorem in_file : spin 0 = 0 := by"
  , "  rfl"
  , ""
  , "end Proj"
  ]

lemma :: Text -> Text -> [Text] -> Lemma
lemma n s us = Lemma
  { lmName = n, lmStatement = s, lmProof = "  simp", lmFile = "Proj/Other.lean", lmModule = "Proj.Other"
  , lmRun = "/runs/0", lmDate = "2026-10-16T00:00:00Z", lmUses = us, lmProofUses = [] }

note :: Text -> FilePath -> Note
note t f = Note {ntText = t, ntFile = f, ntGoal = "Proj.g", ntRun = "/runs/0"}

goalStmt :: Text
goalStmt = "theorem goal_one (n : Nat) : bump n > 0 := by"

openStmts :: [Text]
openStmts = [goalStmt, "theorem goal_two : base = 3 := by"]

offer' :: [Text] -> Text -> ([Lemma], [Note]) -> ([Lemma], [Note])
offer' cs g = offer ix0 "Proj/Top.lean" cs g openStmts

spec :: Spec
spec = do
  describe "libraryKey" $ do
    it "is the first 16 hex characters of the sha256 of the path" $
      libraryKey "/home/u/proj" `shouldBe` "9c09f8055a26c277"
    it "is stable and differs between paths" $ do
      libraryKey "/a/b" `shouldBe` libraryKey "/a/b"
      libraryKey "/a/b/" `shouldBe` libraryKey "/a/b"
      libraryKey "/a/b" `shouldNotBe` libraryKey "/a/c"
      T.length (libraryKey "/a/b") `shouldBe` 16
      T.unpack (libraryKey "/a/b") `shouldSatisfy` all isHexDigit
  describe "libraryDir" $
    it "is FISHER_HOME/fisher/library/<key>" $
      libraryDir "/g" "/home/u/proj" `shouldBe` "/g/fisher/library/9c09f8055a26c277"

  describe "harvest" $ do
    let (ls, ns) = harvest ix0 "Proj/Top.lean" start merged "/runs/1"
    it "saves a new sorry-free helper with its statement, proof, module and uses" $ do
      map lmName ls `shouldBe` ["Proj.bump_helper"]
      case ls of
        [l] -> do
          lmStatement l `shouldBe` "theorem bump_helper (n : Nat) : bump n = n + 1 := by"
          lmProof l `shouldBe` "  simp [bump]"
          lmFile l `shouldBe` "Proj/Top.lean"
          lmModule l `shouldBe` "Proj.Top"
          lmRun l `shouldBe` "/runs/1"
          lmUses l `shouldBe` ["Proj.bump"]
        _ -> expectationFailure "one lemma"
      ns `shouldBe` []
    it "saves nothing when nothing is new" $
      fst (harvest ix0 "Proj/Top.lean" merged merged "/runs/1") `shouldBe` []
    it "treats a renamed, moved or reordered old declaration as old" $ do
      let moved = T.unlines
            [ "import Proj.Basic"
            , ""
            , "theorem in_file_renamed : spin 0 = 0 := by"
            , "  rfl"
            , ""
            , "namespace Proj"
            , ""
            , "namespace Inner"
            , ""
            , "theorem goal_two :  base = 3 := by"
            , "  sorry"
            , ""
            , "end Inner"
            , ""
            , "theorem goal_one' (n : Nat) : bump n > 0 := by"
            , "  omega"
            , ""
            , "theorem bump_helper (n : Nat) : bump n = n + 1 := by"
            , "  simp [bump]"
            , ""
            , "end Proj"
            ]
      map lmName (fst (harvest ix0 "Proj/Top.lean" start moved "/runs/1")) `shouldBe` ["Proj.bump_helper"]
    it "skips a helper using sorryAx" $ do
      let ax = T.replace "  simp [bump]" "  exact sorryAx _" merged
      fst (harvest ix0 "Proj/Top.lean" start ax "/runs/1") `shouldBe` []
    it "skips everything when the merged text does not scan" $
      fst (harvest ix0 "Proj/Top.lean" start (merged <> "\n/- an open comment\n") "/runs/1") `shouldBe` []

  describe "harvest and proof constants" $ do
    let withHelpers hs = T.replace "theorem goal_one" (T.intercalate "\n" hs <> "\ntheorem goal_one") merged
    it "records the project constants a proof uses" $
      map lmProofUses (fst (harvest ix0 "Proj/Top.lean" start merged "/runs/1")) `shouldBe` [["Proj.bump"]]
    it "saves a helper using a helper with the dependency recorded" $ do
      let m = withHelpers
            [ "theorem h1 : spin 1 = spin 1 := by", "  rfl", ""
            , "theorem h2 : bump 0 > 0 := by", "  have := h1", "  simp [bump]", "" ]
          ls = fst (harvest ix0 "Proj/Top.lean" start m "/runs/1")
      map lmName ls `shouldBe` ["Proj.bump_helper", "Proj.h1", "Proj.h2"]
      map lmProofUses ls `shouldBe` [["Proj.bump"], [], ["Proj.h1", "Proj.bump"]]
    it "drops a split lemma whose proof uses a sibling that is still sorry, and what uses it" $ do
      let m = withHelpers
            [ "theorem s2 : bump 1 = 2 := by", "  sorry", ""
            , "theorem s1 : bump 1 > 0 := by", "  rw [s2]", "  omega", ""
            , "theorem s0 : bump 1 > 0 ∧ True := by", "  exact ⟨s1, trivial⟩", "" ]
      map lmName (fst (harvest ix0 "Proj/Top.lean" start m "/runs/1")) `shouldBe` ["Proj.bump_helper"]
    it "reads an old library line without proof constants" $
      fmap lmProofUses (decodeStrict "{\"name\":\"P.a\",\"statement\":\"theorem a : True :=\",\"proof\":\" trivial\",\"file\":\"F.lean\",\"module\":\"F\",\"run\":\"r\",\"date\":\"d\",\"uses\":[]}")
        `shouldBe` Just []

  describe "harvestNotes" $ do
    it "keeps each text once, whitespace normalised and capped" $ do
      let ns = harvestNotes "Proj/Top.lean" "/runs/1"
                 [("Proj.a", "tried  simp\nand omega"), ("Proj.b", "tried simp and omega"), ("Proj.c", "  "), ("Proj.d", T.replicate 400 "x")]
      map ntText ns `shouldBe` ["tried simp and omega", T.replicate 300 "x"]
      map ntGoal ns `shouldBe` ["Proj.a", "Proj.d"]
      map ntFile ns `shouldBe` ["Proj/Top.lean", "Proj/Top.lean"]
    it "reads the failure lines of a whiteboard" $
      whiteboardFailures "# Plan for main\n- main (open)\n\n## Failed\nmain: split failed: no\na: NOTE text: with colon\n"
        `shouldBe` [("main", "split failed: no"), ("a", "NOTE text: with colon")]

  describe "offer" $ do
    let bumpL = lemma "Proj.bump_eq" "theorem bump_eq (n : Nat) : bump n = n + 1 := by" ["Proj.bump"]
    it "requires a shared project constant" $ do
      fst (offer' ["Nat", "Proj.base"] goalStmt ([bumpL], [])) `shouldBe` []
      fst (offer' ["Nat", "Proj.bump"] goalStmt ([bumpL], [])) `shouldBe` [bumpL]
    it "ranks by BM25 plus one per shared constant" $ do
      let two = lemma "Proj.two" "theorem two : spin 1 = base := by" ["Proj.spin", "Proj.base"]
          oneKw = lemma "Proj.oneKw" "theorem oneKw : base = base := by" ["Proj.base"]
          oneNo = lemma "Proj.oneNo" "theorem oneNo : spin 2 = 2 := by" ["Proj.spin"]
          r = offer' ["Proj.spin", "Proj.base"] "theorem q : base = 3 := by" ([oneNo, oneKw, two], [])
      -- two shares 2 and matches "base"; oneKw shares 1 and matches; oneNo shares 1 only
      map lmName (fst r) `shouldBe` ["Proj.two", "Proj.oneKw", "Proj.oneNo"]
    it "caps at 5 lemmas and 3 notes" $ do
      let ls = [lemma ("Proj.l" <> T.pack (show i)) ("theorem l" <> T.pack (show i) <> " : bump " <> T.pack (show i) <> " > 0 := by") ["Proj.bump"] | i <- [1 .. 7 :: Int]]
          ns = [note ("bump goal note " <> T.pack (show i)) "Proj/Top.lean" | i <- [1 .. 5 :: Int]]
          (ol, on) = offer' ["Proj.bump"] goalStmt (ls, ns)
      length ol `shouldBe` 5
      length on `shouldBe` 3
    it "skips a lemma whose uses are no longer all in the index" $ do
      let stale = lemma "Proj.stale" "theorem stale : bump 1 = gone 1 := by" ["Proj.bump", "Proj.gone"]
      fst (offer' ["Proj.bump"] goalStmt ([stale, bumpL], [])) `shouldBe` [bumpL]
    it "skips a lemma whose normalised statement equals an open goal's" $ do
      let same = lemma "Proj.aux" "theorem aux (n : Nat) :\n    bump n  >  0 :=  by" ["Proj.bump"]
      fst (offer' ["Proj.bump"] goalStmt ([same, bumpL], [])) `shouldBe` [bumpL]
    it "skips a lemma already declared in the target file" $ do
      let inFile = lemma "Proj.in_file" "theorem in_file : spin 0 = 0 ∧ bump 0 = 1 := by" ["Proj.spin", "Proj.bump"]
      fst (offer' ["Proj.bump", "Proj.spin"] goalStmt ([inFile, bumpL], [])) `shouldBe` [bumpL]
    it "offers a lemma using a private constant only in that constant's file" $ do
      let sec = lemma "Proj.sec_eq" "theorem sec_eq : secret = bump 0 := by" ["_private.Proj.Basic.0.Proj.secret", "Proj.bump"]
      fst (offer' ["Proj.bump"] goalStmt ([sec], [])) `shouldBe` []
      fst (offer ix0 "Proj/Basic.lean" ["Proj.bump"] goalStmt openStmts ([sec], [])) `shouldBe` [sec]
    it "does not offer a lemma using a module the target does not import" $ do
      let farL = lemma "Proj.far_eq" "theorem far_eq : far = bump 0 := by" ["Proj.far", "Proj.bump"]
      fst (offer' ["Proj.bump"] goalStmt ([farL, bumpL], [])) `shouldBe` [bumpL]
    it "never offers a lemma whose proof uses sorry, admit or sorryAx" $ do
      let adm = (lemma "Proj.adm" "theorem adm : bump 2 = 3 := by" ["Proj.bump"]) {lmProof = "  admit"}
          ax = (lemma "Proj.ax" "theorem ax : bump 3 = 4 := by" ["Proj.bump"]) {lmProof = "  exact sorryAx _"}
      fst (offer' ["Proj.bump"] goalStmt ([adm, ax, bumpL], [])) `shouldBe` [bumpL]
    it "does not offer a lemma whose proof uses a module the target does not import" $ do
      let farP = (lemma "Proj.farp" "theorem farp : bump 5 > 0 := by" ["Proj.bump"]) {lmProofUses = ["Proj.far"]}
          okP = (lemma "Proj.okp" "theorem okp : bump 6 > 0 := by" ["Proj.bump"]) {lmProofUses = ["Proj.base"]}
      fst (offer' ["Proj.bump"] goalStmt ([farP, okP], [])) `shouldBe` [okP]
    it "offers a lemma whose proof uses another library lemma together with it, that one first" $ do
      let h1 = (lemma "Proj.h1" "theorem h1 : spin 1 = spin 1 := by" ["Proj.spin"]) {lmProofUses = []}
          h2 = (lemma "Proj.h2" "theorem h2 : bump 0 > 0 := by" ["Proj.bump"]) {lmProofUses = ["Proj.h1", "Proj.bump"]}
      fst (offer' ["Proj.bump"] goalStmt ([h2, h1], [])) `shouldBe` [h1, h2]
      -- without it in the library, not at all
      fst (offer' ["Proj.bump"] goalStmt ([h2], [])) `shouldBe` []
      -- the brief shows the dependency first and never the lemma alone
      let b = libraryBrief ([h1, h2], [])
      T.breakOn "-- Proj.h1 (" b `shouldSatisfy` ((< T.length (fst (T.breakOn "-- Proj.h2 (" b))) . T.length . fst)
      libraryBrief ([h1 {lmProof = "  " <> T.replicate 1600 "x"}, h2], []) `shouldBe` ""
    it "offers one lemma per statement" $ do
      let a1 = lemma "Proj.a1" "theorem a1 : bump 7 > 0 := by" ["Proj.bump"]
          a2 = lemma "Proj.a2" "theorem a2 :  bump 7 > 0 := by" ["Proj.bump"]
      fst (offer' ["Proj.bump"] goalStmt ([a1, a2], [])) `shouldBe` [a1]
    it "does not offer a dependency stating an open goal, nor what needs it" $ do
      let dep = lemma "Proj.dep" "theorem dep : base = 3 := by" ["Proj.base"]
          user = (lemma "Proj.user" "theorem user : bump 0 > 0 := by" ["Proj.bump"]) {lmProofUses = ["Proj.dep"]}
      fst (offer' ["Proj.bump"] goalStmt ([user, dep], [])) `shouldBe` []
    it "offers notes by BM25, the same file first" $ do
      let far = note "bump bump goal one stuck" "Proj/Elsewhere.lean"
          near = note "bump stuck" "Proj/Top.lean"
          off = note "unrelated words only" "Proj/Top.lean"
      snd (offer' ["Proj.bump"] goalStmt ([], [far, off, near])) `shouldBe` [near, far]

  describe "libraryBrief" $ do
    it "is empty for an empty offer" $ libraryBrief ([], []) `shouldBe` ""
    it "gives each lemma's statement and proof and the notes" $ do
      let b = libraryBrief ([lemma "Proj.bump_eq" "theorem bump_eq (n : Nat) : bump n = n + 1 := by" ["Proj.bump"]], [note "omega closes it" "Proj/Top.lean"])
      b `shouldSatisfy` T.isPrefixOf "Lemmas proved in earlier runs (paste one above the goal as a helper if it helps):"
      b `shouldSatisfy` T.isInfixOf "theorem bump_eq (n : Nat) : bump n = n + 1 := by\n  simp"
      b `shouldSatisfy` T.isInfixOf "omega closes it"
    it "skips a lemma over 1,500 characters and stops past 6,000 in all" $ do
      let big = (lemma "Proj.big" "theorem big : bump 0 = 1 := by" ["Proj.bump"]) {lmProof = "  " <> T.replicate 1500 "x"}
          mid i = (lemma ("Proj.mid" <> T.pack (show i)) ("theorem mid" <> T.pack (show i) <> " : bump 0 = 1 := by") ["Proj.bump"]) {lmProof = "  " <> T.replicate 1400 "y"}
          b = libraryBrief (big : map mid [1 .. 5 :: Int], [])
      b `shouldSatisfy` (not . T.isInfixOf "Proj.big")
      map (\i -> ("Proj.mid" <> T.pack (show i)) `T.isInfixOf` b) [1 .. 5 :: Int] `shouldBe` [True, True, True, True, False]
    it "is empty when every lemma is too long" $ do
      let big = (lemma "Proj.big" "theorem big : bump 0 = 1 := by" ["Proj.bump"]) {lmProof = "  " <> T.replicate 1500 "x"}
      libraryBrief ([big], []) `shouldBe` ""

  describe "storage" $ do
    let l1 = lemma "Proj.a" "theorem a : bump 0 = 1 := by" ["Proj.bump"]
        l2 = lemma "Proj.b" "theorem b : bump 1 = 2 := by" ["Proj.bump"]
        l1ws = l1 {lmStatement = "theorem a :  bump 0 = 1 :=\n  by", lmRun = "/runs/9"}
        l1other = l1 {lmStatement = "theorem a : bump 0 = 2 := by"}
    it "writes root.txt on the first save and dedupes by name and statement across saves" $
      withSystemTempDirectory "lib" $ \tmp -> do
        let dir = tmp </> "lib"
        saveLibrary dir "/home/u/proj" [l1, l2] [note "n1" "F.lean"] `shouldReturn` (2, 1)
        BS.readFile (dir </> "root.txt") `shouldReturn` "/home/u/proj\n"
        saveLibrary dir "/home/u/proj" [l1, l1ws, l1other] [note "n1" "G.lean", note "n2" "F.lean"] `shouldReturn` (1, 1)
        (ls, ns) <- loadLibrary dir
        map lmStatement ls `shouldBe` map lmStatement [l1, l2, l1other]
        map ntText ns `shouldBe` ["n1", "n2"]
    it "does not save a renamed copy of a lemma's statement" $
      withSystemTempDirectory "lib" $ \tmp -> do
        _ <- saveLibrary tmp "/r" [l1] []
        saveLibrary tmp "/r" [l1 {lmName = "Proj.a_aux", lmStatement = "theorem a_aux :\n  bump 0 = 1 := by"}] [] `shouldReturn` (0, 0)
        saveLibrary tmp "/r" [l2, l2 {lmName = "Proj.b'", lmStatement = "theorem b' : bump 1 = 2 := by"}] [] `shouldReturn` (1, 0)
    it "stamps a lemma with no date" $
      withSystemTempDirectory "lib" $ \tmp -> do
        _ <- saveLibrary tmp "/r" [l1 {lmDate = ""}] []
        (ls, _) <- loadLibrary tmp
        map (T.length . lmDate) ls `shouldSatisfy` all (>= 10)
    it "loads nothing from a missing directory" $
      withSystemTempDirectory "lib" $ \tmp ->
        loadLibrary (tmp </> "none") `shouldReturn` ([], [])
    it "drops only a trailing line cut off mid-append" $
      withSystemTempDirectory "lib" $ \tmp -> do
        _ <- saveLibrary tmp "/r" [l1] []
        BS.appendFile (tmp </> "lemmas.jsonl") "{\"name\":\"Proj.cut"
        fmap (map lmName . fst) (loadLibrary tmp) `shouldReturn` ["Proj.a"]
        doesFileExist (tmp </> "lemmas.jsonl.bad") `shouldReturn` False
        saveLibrary tmp "/r" [l2] [] `shouldReturn` (1, 0)
        fmap (map lmName . fst) (loadLibrary tmp) `shouldReturn` ["Proj.a", "Proj.b"]
    it "caps notes at 300 characters on load" $
      withSystemTempDirectory "lib" $ \tmp -> do
        _ <- saveLibrary tmp "/r" [] [note "short" "F.lean"]
        BS.appendFile (tmp </> "notes.jsonl") (BS.concat ["{\"text\":\"", BS.replicate 500 120, "\",\"file\":\"F.lean\",\"goal\":\"g\",\"run\":\"r\"}\n"])
        fmap (map (T.length . ntText) . snd) (loadLibrary tmp) `shouldReturn` [5, 300]
    it "sets a corrupted lemmas.jsonl aside and returns the rest" $
      withSystemTempDirectory "lib" $ \tmp -> do
        _ <- saveLibrary tmp "/r" [l1] [note "kept" "F.lean"]
        good <- BS.readFile (tmp </> "lemmas.jsonl")
        -- a bad line with a good one after it is not a cut-off append
        BS.appendFile (tmp </> "lemmas.jsonl") ("{not json\n" <> good)
        (ls, ns) <- loadLibrary tmp
        ls `shouldBe` []
        map ntText ns `shouldBe` ["kept"]
        doesFileExist (tmp </> "lemmas.jsonl.bad") `shouldReturn` True
        saveLibrary tmp "/r" [l2] [] `shouldReturn` (1, 0)
        fmap (map lmName . fst) (loadLibrary tmp) `shouldReturn` ["Proj.b"]
    it "loses no line to concurrent saves" $
      withSystemTempDirectory "lib" $ \tmp -> do
        let batch i = [lemma ("Proj.c" <> T.pack (show i) <> "_" <> T.pack (show j)) ("theorem c : bump " <> T.pack (show j) <> " = " <> T.pack (show i) <> " := by") ["Proj.bump"] | j <- [1 .. 5 :: Int]]
        forConcurrently_ [1 .. 16 :: Int] $ \i -> saveLibrary tmp "/r" (batch i) [note ("note " <> T.pack (show i)) "F.lean"]
        (ls, ns) <- loadLibrary tmp
        length ls `shouldBe` 80
        length ns `shouldBe` 16
