module Fisher.Search.CheckSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import Control.Concurrent (threadDelay)
import System.Directory (createDirectoryIfMissing, doesFileExist, getPermissions, listDirectory, setOwnerExecutable, setPermissions)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)
import Test.Hspec

import Fisher.Search.Check

file :: T.Text
file = T.unlines
  [ "import Mathlib"
  , "theorem a : 1 = 1 := by"
  , "  rfl"
  , "theorem b : 2 = 2 := by"
  , "  sorry"
  , "theorem c : 3 = 3 := by"
  , "  exact b ▸ rfl"
  ]

-- | A lean that answers with a canned output, and checks what it was given.
fake :: Int -> T.Text -> (T.Text -> IO ()) -> LeanRun
fake code out seen = LeanRun $ \root rel -> do
  BS.readFile (root </> rel) >>= seen . TE.decodeUtf8
  pure (code, out)

outA :: T.Text
outA = T.unlines
  [ "Bench/_fisher_search_X.lean:4:8: warning: declaration uses `sorry`"
  , "'a' depends on axioms: [propext,"
  , " Quot.sound]"
  , "a : @Eq.{1} Nat"
  , "  (@OfNat.ofNat.{0} Nat (nat_lit 1) (instOfNatNat (nat_lit 1)))"
  , "  (@OfNat.ofNat.{0} Nat (nat_lit 1) (instOfNatNat (nat_lit 1)))"
  , "'c' depends on axioms: [sorryAx]"
  , "c : @Eq.{1} Nat three three"
  ]

pol :: AxiomPolicy
pol = AxiomPolicy standardAxioms False

spec :: Spec
spec = describe "the kernel check" $ do
  it "probes from a sibling file it removes, with the commands appended" $
    withSystemTempDirectory "check" $ \root -> do
      pr <- probe (fake 0 outA (\t -> do
              T.isInfixOf "#print axioms a\n\nset_option pp.all true in\n#check @a" t `shouldBe` True
              T.isInfixOf "#check @c" t `shouldBe` True)) root "X.lean" file ["a", "c"]
      listDirectory root `shouldReturn` []
      prSorryLines pr `shouldBe` [4]
      Map.lookup "a" (prAxioms pr) `shouldBe` Just ["propext", "Quot.sound"]
      Map.lookup "a" (prTypes pr) `shouldBe` Just (T.intercalate "\n"
        [ "a : @Eq.{1} Nat"
        , "  (@OfNat.ofNat.{0} Nat (nat_lit 1) (instOfNatNat (nat_lit 1)))"
        , "  (@OfNat.ofNat.{0} Nat (nat_lit 1) (instOfNatNat (nat_lit 1)))" ])
      Map.lookup "c" (prTypes pr) `shouldBe` Just "c : @Eq.{1} Nat three three"
      doesFileExist (root </> "_fisher_search_X.lean") `shouldReturn` False
  it "reads the type of a universe-polymorphic name" $
    withSystemTempDirectory "check" $ \root -> do
      pr <- probe (fake 0 "'A.l' depends on axioms: [propext]\n@A.l.{u_1} : P\n" (const (pure ()))) root "X.lean" file ["A.l"]
      Map.lookup "A.l" (prTypes pr) `shouldBe` Just "@A.l.{u_1} : P"
  let pr = Probe 0 [] [4] (Map.fromList [("a", ["propext"]), ("c", ["sorryAx"])]) (Map.fromList [("a", "a : T"), ("c", "c : U")]) ""
  it "closes a goal with standard axioms and its type" $
    verdict pol file "a" "a : T" pr `shouldBe` Closed
  it "waits on a goal whose sorry comes from an open goal it uses" $
    verdict pol file "c" "c : U" pr `shouldBe` Waiting
  it "rejects a changed type" $
    verdict pol file "a" "a : T'" pr `shouldBe` Rejected "the statement elaborates to a different type"
  it "rejects a goal that is still sorry" $
    verdict pol file "b" "b : V" pr `shouldBe` Rejected "still sorry"
  it "rejects errors" $
    verdict pol file "a" "a : T" pr {prErrors = ["X.lean:3:2: error: unsolved goals"], prExit = 1}
      `shouldBe` Rejected "does not compile: X.lean:3:2: error: unsolved goals"
  it "rejects a sorry outside the open goals" $
    verdict pol file "a" "a : T" pr {prSorryLines = [2]}
      `shouldBe` Rejected "the proof uses sorry"
  it "rejects other axioms, allows native_decide only when asked" $ do
    let nat = pr {prAxioms = Map.fromList [("a", ["propext", "a._native.native_decide.ax_1_1"])]}
    verdict pol file "a" "a : T" nat `shouldBe` Rejected "axioms beyond the allowed: a._native.native_decide.ax_1_1"
    verdict pol {apNativeDecide = True} file "a" "a : T" nat `shouldBe` Closed
    verdict pol {apAllowed = Set.insert "Lean.ofReduceBool" standardAxioms} file "a" "a : T"
      pr {prAxioms = Map.fromList [("a", ["Lean.ofReduceBool"])]} `shouldBe` Closed

  describe "lakeLeanRun" $
    it "kills the whole process group when the caller's timeout fires" $
      withSystemTempDirectory "lake-kill" $ \root -> do
        -- a stand-in lake whose child (the "lean") outlives it unless the group is killed
        let bin = root </> "bin"
        createDirectoryIfMissing True bin
        writeFile (bin </> "lake") "#!/bin/sh\nsleep 600 &\necho $! > child.pid\nwait\n"
        getPermissions (bin </> "lake") >>= setPermissions (bin </> "lake") . setOwnerExecutable True
        r <- timeout 1000000 (runLean (lakeLeanRunWith (bin </> "lake") 600) root "X.lean")
        fmap fst r `shouldBe` Nothing
        pid <- filter (`elem` ['0' .. '9']) <$> readFile (root </> "child.pid")
        pid `shouldNotBe` ""
        threadDelay 300000
        -- gone, or a zombie waiting to be reaped
        (code, out, _) <- readProcessWithExitCode "ps" ["-o", "stat=", "-p", pid] ""
        (code /= ExitSuccess || take 1 (dropWhile (== ' ') out) == "Z") `shouldBe` True
