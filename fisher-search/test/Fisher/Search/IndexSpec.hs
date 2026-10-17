module Fisher.Search.IndexSpec (spec) where

import Control.Monad (filterM, forM_, unless)
import qualified Data.ByteString as BS
import Data.IORef
import Data.List (find, isInfixOf, isPrefixOf, sort)
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock (addUTCTime)
import System.Directory
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Search.Index
import Fisher.Search.Lean (Decl (..), declarations)

fixture :: FilePath
fixture = "test/fixtures/index-proj"

copyTree :: FilePath -> FilePath -> IO ()
copyTree from to = do
  createDirectoryIfMissing True to
  xs <- listDirectory from
  forM_ xs $ \x -> do
    isDir <- doesDirectoryExist (from </> x)
    if isDir then copyTree (from </> x) (to </> x) else copyFile (from </> x) (to </> x)

-- | A fresh copy of the fixture, its cache path, and a reader that records
-- which files it read.
withProject :: (FilePath -> FilePath -> IORef [FilePath] -> IO a) -> IO a
withProject k = withSystemTempDirectory "index-proj" $ \tmp -> do
  let root = tmp </> "proj"
  copyTree fixture root
  reads' <- newIORef []
  k root (tmp </> "cache" </> "index.json") reads'

counting :: IORef [FilePath] -> FilePath -> IO BS.ByteString
counting ref p = modifyIORef ref (p :) >> BS.readFile p

build :: FilePath -> FilePath -> IORef [FilePath] -> IO (Index, [(FilePath, Text)], [FilePath])
build root cache ref = do
  writeIORef ref []
  (ix, skipped) <- buildIndexWith (counting ref) root cache
  rs <- readIORef ref
  pure (ix, skipped, sort (map (makeRelative root) rs))

readUtf8 :: FilePath -> IO Text
readUtf8 p = decodeUtf8 <$> BS.readFile p

entry :: Index -> Text -> Maybe Entry
entry ix nm = find ((== nm) . eName) (ixEntries ix)

-- | The 1-based line of the first line of a fixture file containing a needle.
lineOf :: FilePath -> Text -> IO Int
lineOf file needle = do
  src <- readUtf8 (fixture </> file)
  case [i | (i, l) <- zip [1 ..] (T.lines src), needle `T.isInfixOf` l] of
    (i : _) -> pure i
    [] -> fail ("needle not in fixture: " <> T.unpack needle)

usesOf :: Index -> Text -> [Text]
usesOf ix nm = maybe ["<missing " <> nm <> ">"] eUses (entry ix nm)

spec :: Spec
spec = do
  describe "moduleOf" $ do
    it "turns a path under the root into a dotted module name" $ do
      moduleOf "/r" "/r/Proj/Basic.lean" `shouldBe` "Proj.Basic"
      moduleOf "/r/" "/r/Proj/Sub/Deep.lean" `shouldBe` "Proj.Sub.Deep"
      moduleOf "/r" "Top.lean" `shouldBe` "Top"
      moduleOf "/r" "./Proj/Use.lean" `shouldBe` "Proj.Use"

  describe "buildIndex on the fixture project" $ do
    it "indexes every kind with its fully qualified name, line and module" $
      withProject $ \root cache _ -> do
        (ix, _) <- buildIndex root cache
        let expected =
              [ ("Proj.Counter", KStructure, "Proj/Basic.lean", "structure Counter where")
              , ("Proj.Color", KInductive, "Proj/Basic.lean", "inductive Color where")
              , ("Proj.Tree", KInductive, "Proj/Basic.lean", "inductive Tree : Type")
              , ("Proj.Sized", KClass, "Proj/Basic.lean", "class Sized")
              , ("Proj.bump", KDef, "Proj/Basic.lean", "def bump")
              , ("Proj.greeting", KDef, "Proj/Basic.lean", "def greeting")
              , ("Proj.Count", KAbbrev, "Proj/Basic.lean", "abbrev Count")
              , ("Proj.instSizedCounter", KInstance, "Proj/Basic.lean", "instance instSizedCounter")
              , ("Proj.magic", KAxiom, "Proj/Basic.lean", "axiom magic")
              , ("Proj.secret", KOpaque, "Proj/Basic.lean", "opaque secret")
              , ("Proj.bump_value", KTheorem, "Proj/Basic.lean", "theorem bump_value")
              , ("Proj.Inner.bump_pos", KLemma, "Proj/Basic.lean", "lemma bump_pos")
              , ("Proj.hidden_helper", KTheorem, "Proj/Basic.lean", "theorem hidden_helper")
              , ("Proj.double", KDef, "Proj/Basic.lean", "def double")
              , ("Proj.Counter.value_mk", KTheorem, "Proj/Basic.lean", "protected theorem Counter.value_mk")
              , ("Proj.nonrec_id", KDef, "Proj/Basic.lean", "nonrec def nonrec_id")
              , ("Proj.spin", KDef, "Proj/Basic.lean", "partial def spin")
              , ("same_line", KTheorem, "Proj/Top.lean", "theorem same_line")
              , ("through_option", KTheorem, "Proj/Top.lean", "theorem through_option")
              , ("option_same_line", KTheorem, "Proj/Top.lean", "theorem option_same_line")
              , ("top_level_fact", KTheorem, "Proj/Basic.lean", "theorem _root_.top_level_fact")
              , ("counter_open", KTheorem, "Proj/Use.lean", "theorem counter_open")
              , ("Proj.Extra.base", KDef, "Proj/Use.lean", "def base")
              , ("Proj.Extra.base_eq", KTheorem, "Proj/Use.lean", "theorem base_eq")
              , ("top_uses", KTheorem, "Proj/Top.lean", "theorem top_uses")
              , ("long_statement", KTheorem, "Proj/Top.lean", "theorem long_statement")
              ]
        forM_ expected $ \(nm, k, file, needle) -> do
          ln <- lineOf file needle
          let e = entry ix nm
          fmap (\x -> (eKind x, eFile x, eLine x, eModule x)) e
            `shouldBe` Just (k, file, ln, moduleOf root (root </> file))

    it "takes the statement from the keyword through := or where, capped at 1,500 characters" $
      withProject $ \root cache _ -> do
        (ix, _) <- buildIndex root cache
        let st nm = eStatement <$> entry ix nm
        st "Proj.Inner.bump_pos" `shouldBe` Just "lemma bump_pos (c : Counter) :\n    0 < (bump c).value :="
        st "Proj.Counter" `shouldBe` Just "structure Counter where"
        st "Proj.Color" `shouldBe` Just "inductive Color where"
        st "Proj.Tree" `shouldBe` Just "inductive Tree : Type"
        st "Proj.Sized" `shouldBe` Just "class Sized (α : Type) where"
        st "Proj.double" `shouldBe` Just "def double : Nat → Nat"
        st "Proj.magic" `shouldBe` Just "axiom magic : ∀ n : Nat, n = n"
        st "Proj.secret" `shouldBe` Just "opaque secret : Nat"
        st "Proj.greeting" `shouldBe` Just "def greeting : String :="
        st "Proj.instSizedCounter" `shouldBe` Just "instance instSizedCounter : Sized Counter where"
        st "Proj.bump_value" `shouldBe` Just "theorem bump_value : (bump c).value = c.value + 1 :="
        st "counter_open" `shouldBe` Just "theorem counter_open (c : Counter) : -- not Proj.secret\n    bump c = bump c :="
        st "Proj.Counter.value_mk" `shouldBe` Just "theorem Counter.value_mk (n : Nat) : (Counter.mk n).value = n :="
        st "same_line" `shouldBe` Just "theorem same_line : base = 3 :="
        fmap T.length (st "long_statement") `shouldBe` Just 1500
        fmap (T.isPrefixOf "theorem long_statement : True ∧ True") (st "long_statement") `shouldBe` Just True

    it "skips commented-out declarations and unnamed instances" $
      withProject $ \root cache _ -> do
        (ix, _) <- buildIndex root cache
        entry ix "Proj.fake_in_comment" `shouldBe` Nothing
        entry ix "fake_in_comment" `shouldBe` Nothing
        filter ((== KInstance) . eKind) (ixEntries ix) `shouldSatisfy` ((== 1) . length)

    it "agrees with Lean.declarations on the theorems and lemmas (eLine is dStart + 1)" $
      withProject $ \root cache _ -> do
        (ix, _) <- buildIndex root cache
        forM_ ["Proj/Basic.lean", "Proj/Use.lean", "Proj/Top.lean"] $ \file -> do
          src <- readUtf8 (root </> file)
          let ours = [(eName e, eLine e) | e <- ixEntries ix, eFile e == file, eKind e `elem` [KTheorem, KLemma]]
          -- Lean.hs does not see comments, so it reports the theorem inside the /-! -/ block,
          -- and it misses a keyword after "open ... in" or "set_option ... in" on the same line
          let sameLine = ["same_line", "option_same_line"]
          filter ((`notElem` sameLine) . fst) ours
            `shouldBe` [(dName d, dStart d + 1) | d <- declarations src, dName d /= "fake_in_comment"]

    it "skips .lake and the lakefile" $
      withProject $ \root cache _ -> do
        (ix, _) <- buildIndex root cache
        entry ix "dep_fact" `shouldBe` Nothing
        filter ((".lake" `isInfixOf`) . eFile) (ixEntries ix) `shouldBe` []
        M.keys (ixImports ix) `shouldBe` ["Proj.Basic", "Proj.Top", "Proj.Use"]

    it "skips symlinked directories, hidden directories and hidden files" $
      withProject $ \root cache _ -> do
        createDirectoryLink (root </> "Proj") (root </> "Linked")
        createDirectoryIfMissing True (root </> ".git" </> "x")
        BS.writeFile (root </> ".git" </> "x" </> "Git.lean") "theorem in_git : True := trivial\n"
        BS.writeFile (root </> "Proj" </> ".Hidden.lean") "theorem in_hidden : True := trivial\n"
        (ix, _) <- buildIndex root cache
        filter (\e -> not ("Proj/" `isPrefixOf` eFile e)) (ixEntries ix) `shouldBe` []
        entry ix "in_git" `shouldBe` Nothing
        entry ix "in_hidden" `shouldBe` Nothing

    it "skips a file that does not parse with a reason and indexes the rest" $
      withProject $ \root cache _ -> do
        (ix, skipped) <- buildIndex root cache
        map fst skipped `shouldBe` ["Proj/Bad.lean"]
        forM_ skipped $ \(_, why) -> why `shouldSatisfy` T.isInfixOf "comment"
        entry ix "bad_one" `shouldBe` Nothing
        entry ix "bad_two" `shouldBe` Nothing
        entry ix "Proj.bump" `shouldSatisfy` (not . isNothing)
        entry ix "top_full" `shouldSatisfy` (not . isNothing)

    it "skips a file that is not UTF-8" $
      withProject $ \root cache _ -> do
        BS.writeFile (root </> "Proj" </> "Latin.lean") (BS.pack [0x74, 0x68, 0xff, 0xfe, 0x0a])
        (_, skipped) <- buildIndex root cache
        map fst skipped `shouldBe` ["Proj/Bad.lean", "Proj/Latin.lean"]

    it "records imports and the transitive project import closure" $
      withProject $ \root cache _ -> do
        (ix, _) <- buildIndex root cache
        M.lookup "Proj.Use" (ixImports ix) `shouldBe` Just ["Mathlib.Tactic", "Proj.Basic"]
        M.lookup "Proj.Top" (ixImports ix) `shouldBe` Just ["Proj.Use", "Mathlib.Order.Basic"]
        importClosure ix "Proj.Top" `shouldBe` Set.fromList ["Proj.Top", "Proj.Use", "Proj.Basic"]
        importClosure ix "Proj.Use" `shouldBe` Set.fromList ["Proj.Use", "Proj.Basic"]
        importClosure ix "Proj.Basic" `shouldBe` Set.fromList ["Proj.Basic"]
        importClosure ix "Mathlib.Tactic" `shouldBe` Set.empty

    it "maps every name to its position in the entries" $
      withProject $ \root cache _ -> do
        (ix, _) <- buildIndex root cache
        forM_ (M.toList (ixByName ix)) $ \(k, i) -> entryKey (ixEntries ix !! i) `shouldBe` k
        M.size (ixByName ix) `shouldBe` length (ixEntries ix)

  describe "the cache" $ do
    it "a second build with no file changed reads no .lean file and gives the same index" $
      withProject $ \root cache ref -> do
        (ix1, sk1, r1) <- build root cache ref
        r1 `shouldBe` ["Proj/Bad.lean", "Proj/Basic.lean", "Proj/Top.lean", "Proj/Use.lean"]
        doesFileExist cache `shouldReturn` True
        (ix2, sk2, r2) <- build root cache ref
        r2 `shouldBe` []
        ix2 `shouldBe` ix1
        sk2 `shouldBe` sk1

    it "re-reads a touched file alone" $
      withProject $ \root cache ref -> do
        _ <- build root cache ref
        let use = root </> "Proj" </> "Use.lean"
        t <- getModificationTime use
        setModificationTime use (addUTCTime 60 t)
        (_, _, r) <- build root cache ref
        r `shouldBe` ["Proj/Use.lean"]

    it "picks up an edited file and drops a deleted one" $
      withProject $ \root cache ref -> do
        _ <- build root cache ref
        BS.appendFile (root </> "Proj" </> "Top.lean") (encodeUtf8 "\ntheorem appended : True := trivial\n")
        removeFile (root </> "Proj" </> "Bad.lean")
        (ix, sk, r) <- build root cache ref
        r `shouldBe` ["Proj/Top.lean"]
        sk `shouldBe` []
        fmap eFile (entry ix "appended") `shouldBe` Just "Proj/Top.lean"

    it "rebuilds from scratch when the cache is valid JSON of another version" $
      withProject $ \root cache ref -> do
        (ix1, _, _) <- build root cache ref
        old <- readUtf8 cache
        let bumped = T.replace "\"version\":" "\"version\":9" old
        bumped `shouldNotBe` old
        BS.writeFile cache (encodeUtf8 bumped)
        (ix2, _, r) <- build root cache ref
        length r `shouldBe` 4
        ix2 `shouldBe` ix1

    it "leaves no temporary file beside the cache" $
      withProject $ \root cache ref -> do
        _ <- build root cache ref
        BS.appendFile (root </> "Proj" </> "Top.lean") (encodeUtf8 "\ntheorem more : True := trivial\n")
        _ <- build root cache ref
        listDirectory (takeDirectory cache) `shouldReturn` ["index.json"]

    it "rebuilds from scratch when the cache file is corrupt" $
      withProject $ \root cache ref -> do
        (ix1, _, _) <- build root cache ref
        writeFile cache "{not json"
        (ix2, _, r) <- build root cache ref
        length r `shouldBe` 4
        ix2 `shouldBe` ix1

  describe "eUses" $ do
    it "lists project names by full name" $
      withProject $ \root cache _ -> do
        (ix, _) <- buildIndex root cache
        usesOf ix "top_full" `shouldBe` ["Proj.Counter", "Proj.bump"]

    it "lists names visible through open, leaving out comments" $
      withProject $ \root cache _ -> do
        (ix, _) <- buildIndex root cache
        usesOf ix "counter_open" `shouldBe` ["Proj.Counter", "Proj.bump"]

    it "lists names visible through the enclosing namespace, dotted names to their longest index prefix" $
      withProject $ \root cache _ -> do
        (ix, _) <- buildIndex root cache
        usesOf ix "Proj.Extra.base_eq" `shouldBe` ["Proj.Extra.base"]
        usesOf ix "Proj.Extra.dotted" `shouldBe` ["Proj.Counter", "Proj.Color", "Proj.bump"]
        usesOf ix "Proj.Extra.uses_inner" `shouldBe` ["Proj.Counter", "Proj.Inner.bump_pos"]

    it "applies open ... in to the next declaration only" $
      withProject $ \root cache _ -> do
        (ix, _) <- buildIndex root cache
        usesOf ix "top_uses" `shouldBe` ["Proj.Extra.base"]
        usesOf ix "top_no_open" `shouldBe` []

    it "applies open ... in on the declaration's own line, and through a set_option ... in line" $
      withProject $ \root cache _ -> do
        (ix, _) <- buildIndex root cache
        usesOf ix "same_line" `shouldBe` ["Proj.Extra.base"]
        usesOf ix "through_option" `shouldBe` ["Proj.Extra.base"]
        usesOf ix "option_same_line" `shouldBe` []

    it "keys private declarations by their module and never resolves another file's private name" $
      withProject $ \root cache _ -> do
        (ix, _) <- buildIndex root cache
        let twins = [e | e <- ixEntries ix, eName e == "twin"]
        map (\e -> (eFile e, ePrivate e)) twins `shouldBe` [("Proj/Top.lean", True), ("Proj/Use.lean", True)]
        M.lookup "_private.Proj.Top.0.twin" (ixByName ix) `shouldSatisfy` (not . isNothing)
        M.lookup "_private.Proj.Use.0.twin" (ixByName ix) `shouldSatisfy` (not . isNothing)
        M.lookup "twin" (ixByName ix) `shouldBe` Nothing
        M.lookup "_private.Proj.Basic.0.Proj.hidden_helper" (ixByName ix) `shouldSatisfy` (not . isNothing)
        fmap ePrivate (entry ix "Proj.bump") `shouldBe` Just False
        usesOf ix "uses_twin" `shouldBe` ["_private.Proj.Top.0.twin"]
        usesOf ix "sees_private" `shouldBe` []
        forM_ (M.toList (ixByName ix)) $ \(k, i) -> entryKey (ixEntries ix !! i) `shouldBe` k

    it "never counts a binder, a Mathlib name or the declaration itself" $
      withProject $ \root cache _ -> do
        (ix, _) <- buildIndex root cache
        usesOf ix "short_names" `shouldBe` []
        usesOf ix "mathlib_name" `shouldBe` []
        usesOf ix "Proj.bump" `shouldBe` ["Proj.Counter"]
        usesOf ix "Proj.double" `shouldBe` []

    it "only ever lists exact index names" $
      withProject $ \root cache _ -> do
        (ix, _) <- buildIndex root cache
        let names = M.keysSet (ixByName ix)
        bad <- filterM (\u -> pure (not (Set.member u names))) (concatMap eUses (ixEntries ix))
        unless (null bad) $ expectationFailure (show bad)

  describe "a Lake package inside a repository" $ do
    let withRepo k = withSystemTempDirectory "index-repo" $ \tmp -> do
          copyTree "test/fixtures/index-repo" (tmp </> "repo")
          k (tmp </> "repo") (tmp </> "cache" </> "index.json")

    it "lakePackageRoot finds the nearest directory holding a lakefile" $
      withRepo $ \repo _ -> do
        lakePackageRoot (repo </> "lean" </> "src" </> "Twist" </> "Main.lean") `shouldReturn` Just (repo </> "lean")
        lakePackageRoot (repo </> "lean" </> "src" </> "Twist" </> "Core") `shouldReturn` Just (repo </> "lean")
        lakePackageRoot (repo </> "lean") `shouldReturn` Just (repo </> "lean")
        lakePackageRoot (repo </> "docs") `shouldReturn` Nothing

    it "names modules from the lean_lib srcDir and skips nested lakefiles and files outside it" $
      withRepo $ \repo cache -> do
        (ix, skipped) <- buildIndex (repo </> "lean") cache
        skipped `shouldBe` []
        [(eName e, eFile e, eModule e) | e <- ixEntries ix]
          `shouldBe` [ ("Twist.seed", "src/Twist/Core/Defs.lean", "Twist.Core.Defs")
                     , ("Twist.seed_eq", "src/Twist/Main.lean", "Twist.Main")
                     ]
        ixImports ix `shouldBe` M.fromList [("Twist.Core.Defs", ["Mathlib.Logic.Basic"]), ("Twist.Main", ["Twist.Core.Defs"])]
        importClosure ix "Twist.Main" `shouldBe` Set.fromList ["Twist.Main", "Twist.Core.Defs"]
        usesOf ix "Twist.seed_eq" `shouldBe` ["Twist.seed"]

    it "reads srcDir from a lakefile.lean too, and defaults to the package root" $
      withSystemTempDirectory "index-lean-lakefile" $ \tmp -> do
        let pkg = tmp </> "pkg"
        createDirectoryIfMissing True (pkg </> "Src" </> "A")
        BS.writeFile (pkg </> "lakefile.lean") "import Lake\nopen Lake DSL\npackage p\nlean_lib A where\n  srcDir := \"Src\"\n"
        BS.writeFile (pkg </> "Src" </> "A" </> "B.lean") "theorem ab : True := trivial\n"
        (ix, _) <- buildIndex pkg (tmp </> "c1.json")
        [(eFile e, eModule e) | e <- ixEntries ix] `shouldBe` [("Src/A/B.lean", "A.B")]
        BS.writeFile (pkg </> "lakefile.lean") "import Lake\nopen Lake DSL\npackage p\nlean_lib A\n"
        (ix2, _) <- buildIndex pkg (tmp </> "c2.json")
        [(eFile e, eModule e) | e <- ixEntries ix2] `shouldBe` [("Src/A/B.lean", "Src.A.B")]
