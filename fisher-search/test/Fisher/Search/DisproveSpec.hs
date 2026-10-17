module Fisher.Search.DisproveSpec (spec) where

import qualified Data.ByteString as BS
import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Data.IORef
import GHC.Clock (getMonotonicTime)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Search.Auto
import qualified Data.Set as Set
import Fisher.Search.Check (AxiomPolicy (..), LeanRun (..), standardAxioms)
import Fisher.Search.Disprove

base :: Text
base = "theorem foo : P := by\n  sorry\n\ntheorem bar : Q := by\n  simp"

errDiag :: Text -> Diag
errDiag = Diag 2 2 SevError

lspWith :: IORef [[Text]] -> Trial -> Lsp
lspWith sent tr = Lsp
  { lspAttempts = \src _ snips -> modifyIORef sent (<> [snips, [src]]) >> pure (Right [tr {tSnippet = s} | s <- snips])
  , lspGoal = \_ _ -> pure (Right Nothing)
  , lspDiagnostics = \_ -> pure (Right [])
  , lspClose = pure ()
  , lspTimedOut = pure () }

found, none :: Trial
found = Trial "" [] [errDiag "Found a counter-example!\nx := 3\ny := 0"]
none = Trial "" [] [errDiag "Unable to find a counter-example"]

-- | A Lean stand-in: records each file it is given; a negation by a
-- tactic in the list compiles clean (with @extra@ for its line), any other
-- has an error. The negation's printed axioms are @axs@.
fakeRunAx :: IORef [Text] -> [Text] -> (Int -> Text) -> [Text] -> LeanRun
fakeRunAx seen oks extra axs = LeanRun $ \root rel -> do
  t <- TE.decodeUtf8 <$> BS.readFile (root </> rel)
  modifyIORef seen (<> [t])
  let ls = T.lines t
      negs = [(i, l) | (i, l) <- zip [1 :: Int ..] ls, "theorem _fisher_neg_" `T.isPrefixOf` l]
      queried = [T.strip (T.drop (T.length "#print axioms ") l) | l <- ls, "#print axioms " `T.isPrefixOf` l]
      axLine n
        | null axs = "'" <> n <> "' does not depend on any axioms"
        | otherwise = "'" <> n <> "' depends on axioms: [" <> T.intercalate ", " axs <> "]"
  pure $ case negs of
    ((i, l) : _)
      | any (\k -> ("by " <> wrapSnippet 200000 k) `T.isSuffixOf` l) oks -> (0, T.unlines (extra i : map axLine queried))
      | otherwise -> (1, T.pack rel <> ":" <> T.pack (show i) <> ":2: error: failed")
    [] -> (0, "")

fakeRun :: IORef [Text] -> [Text] -> (Int -> Text) -> LeanRun
fakeRun seen oks extra = fakeRunAx seen oks extra []

cfg :: AutoConfig
cfg = defaultAutoConfig

pol :: AxiomPolicy
pol = AxiomPolicy standardAxioms False

-- | The negation theorem's number in a probed file.
negNumber :: Text -> Text
negNumber t = T.takeWhile (/= ' ') (snd (T.breakOn "_fisher_neg_" t))

spec :: Spec
spec = do
  describe "parsePlausible" $ do
    it "a counter-example is a suspect holding its lines" $
      parsePlausible found `shouldBe` DSuspect "x := 3\ny := 0"
    it "no counter-example is unknown" $
      parsePlausible none `shouldBe` DUnknown
    it "missing generators is unknown" $
      parsePlausible (Trial "" [] [errDiag "failed to synthesize\n  Plausible.SampleableExt P"]) `shouldBe` DUnknown
  describe "negationTheorem" $
    it "names the type with type_of% in a numbered theorem" $
      negationTheorem 3 "Ns.foo" "decide"
        `shouldBe` "theorem _fisher_neg_3 : ¬ (type_of% @Ns.foo) := by set_option maxHeartbeats 200000 in decide"
  describe "disprove" $ do
    it "certifies with the first tactic that checks, the base cut after the declaration, the negation's axioms printed" $
      withSystemTempDirectory "dis" $ \root -> do
        sent <- newIORef []
        seen <- newIORef []
        r <- disprove cfg (lspWith sent found) (fakeRun seen ["simp_all"] (const "")) pol root "X.lean" base "foo"
        r `shouldBe` Right (DFalse "simp_all")
        -- plausible gets the file cut after the goal
        readIORef sent >>= (`shouldBe` [[wrapSnippet 200000 "plausible"], ["theorem foo : P := by\n  sorry"]])
        ts <- readIORef seen
        length ts `shouldBe` 2
        let t = last ts
            n = negNumber t
        T.isPrefixOf "_fisher_neg_" n `shouldBe` True
        t `shouldBe`
          ("theorem foo : P := by\n  sorry\n\ntheorem " <> n <> " : ¬ (type_of% @foo) := by set_option maxHeartbeats 200000 in simp_all"
            <> "\n\n#print axioms " <> n <> "\n\nset_option pp.all true in\n#check @" <> n <> "\n")
        -- each probe names its negation afresh
        negNumber (head ts) `shouldNotBe` n
    it "names the negation in its namespace" $
      withSystemTempDirectory "dis" $ \root -> do
        sent <- newIORef []
        seen <- newIORef []
        r <- disprove cfg (lspWith sent found) (fakeRun seen ["decide"] (const "")) pol root "X.lean"
               ("namespace Ns\n\n" <> base <> "\n\nend Ns") "Ns.foo"
        r `shouldBe` Right (DFalse "decide")
        ts <- readIORef seen
        any (T.isInfixOf "#print axioms Ns._fisher_neg_") ts `shouldBe` True
        any (T.isInfixOf "end Ns") ts `shouldBe` False
    it "a negation whose axioms hold sorryAx is not a disproof" $
      withSystemTempDirectory "dis" $ \root -> do
        sent <- newIORef []
        seen <- newIORef []
        r <- disprove cfg (lspWith sent found) (fakeRunAx seen ["decide"] (const "") ["propext", "sorryAx"]) pol root "X.lean" base "foo"
        r `shouldBe` Right (DSuspect "x := 3\ny := 0")
    it "a negation on an axiom outside the policy is not a disproof, and is with the axiom allowed" $
      withSystemTempDirectory "dis" $ \root -> do
        sent <- newIORef []
        seen <- newIORef []
        let lean = fakeRunAx seen ["decide"] (const "") ["propext", "Foo.ax"]
        r <- disprove cfg (lspWith sent found) lean pol root "X.lean" base "foo"
        r `shouldBe` Right (DSuspect "x := 3\ny := 0")
        r2 <- disprove cfg (lspWith sent found) lean pol {apAllowed = Set.insert "Foo.ax" standardAxioms} root "X.lean" base "foo"
        r2 `shouldBe` Right (DFalse "decide")
    it "a negation with no axioms printed is not a disproof" $
      withSystemTempDirectory "dis" $ \root -> do
        sent <- newIORef []
        let silent = LeanRun $ \_ _ -> pure (0, "")
        r <- disprove cfg (lspWith sent found) silent pol root "X.lean" base "foo"
        r `shouldBe` Right (DSuspect "x := 3\ny := 0")
    it "a counter-example no tactic certifies stays a suspect" $
      withSystemTempDirectory "dis" $ \root -> do
        sent <- newIORef []
        seen <- newIORef []
        r <- disprove cfg (lspWith sent found) (fakeRun seen [] (const "")) pol root "X.lean" base "foo"
        r `shouldBe` Right (DSuspect "x := 3\ny := 0")
        length <$> readIORef seen >>= (`shouldBe` 4)
    it "no counter-example is unknown and runs no probe" $
      withSystemTempDirectory "dis" $ \root -> do
        sent <- newIORef []
        seen <- newIORef []
        r <- disprove cfg (lspWith sent none) (fakeRun seen ["decide"] (const "")) pol root "X.lean" base "foo"
        r `shouldBe` Right DUnknown
        readIORef seen >>= (`shouldBe` [])
    it "a sorry warning on the negation's line does not certify" $
      withSystemTempDirectory "dis" $ \root -> do
        sent <- newIORef []
        seen <- newIORef []
        let warn i = "X.lean:" <> T.pack (show i) <> ":0: warning: declaration uses 'sorry'"
        r <- disprove cfg (lspWith sent found) (fakeRun seen ["decide"] warn) pol root "X.lean" base "foo"
        r `shouldBe` Right (DSuspect "x := 3\ny := 0")
    it "a sorry warning on another line still certifies" $
      withSystemTempDirectory "dis" $ \root -> do
        sent <- newIORef []
        seen <- newIORef []
        r <- disprove cfg (lspWith sent found) (fakeRun seen ["decide"] (const "X.lean:1:8: warning: declaration uses 'sorry'")) pol root "X.lean" base "foo"
        r `shouldBe` Right (DFalse "decide")
    it "a dead Lsp is LspDown" $
      withSystemTempDirectory "dis" $ \root -> do
        seen <- newIORef []
        let lsp = (lspWith undefined found) {lspAttempts = \_ _ _ -> pure (Left "boom")}
        r <- disprove cfg lsp (fakeRun seen [] (const "")) pol root "X.lean" base "foo"
        r `shouldBe` Left (LspDown "boom")
    it "an unknown goal is missed" $
      withSystemTempDirectory "dis" $ \root -> do
        sent <- newIORef []
        seen <- newIORef []
        r <- disprove cfg (lspWith sent found) (fakeRun seen [] (const "")) pol root "X.lean" base "nope"
        r `shouldBe` Left Missed
    it "a probe that throws counts as a failed tactic" $
      withSystemTempDirectory "dis" $ \root -> do
        sent <- newIORef []
        let boom = LeanRun $ \_ _ -> throwIO (userError "lean gone")
        r <- disprove cfg (lspWith sent found) boom pol root "X.lean" base "foo"
        r `shouldBe` Right (DSuspect "x := 3\ny := 0")
    it "each probe has its own cap: every certify tactic runs even after plausible took its time" $
      withSystemTempDirectory "dis" $ \root -> do
        sent <- newIORef []
        runs <- newIORef (0 :: Int)
        let slowLsp = (lspWith sent found) {lspAttempts = \_ _ snips -> threadDelay 150000 >> pure (Right [found {tSnippet = s} | s <- snips])}
            slow = LeanRun $ \_ _ -> modifyIORef runs (+ 1) >> threadDelay 3000000 >> pure (0, "")
        t0 <- getMonotonicTime
        r <- disprove cfg {acSeconds = 0.2} slowLsp slow pol root "X.lean" base "foo"
        t1 <- getMonotonicTime
        r `shouldBe` Right (DSuspect "x := 3\ny := 0")
        readIORef runs `shouldReturn` 4
        (t1 - t0) `shouldSatisfy` (< 2)
    it "a plausible call cut off at the cap is unknown and reported as a timeout" $
      withSystemTempDirectory "dis" $ \root -> do
        timedOut <- newIORef (0 :: Int)
        seen <- newIORef []
        let hang = (lspWith undefined found) {lspAttempts = \_ _ _ -> threadDelay 3000000 >> pure (Right []), lspTimedOut = modifyIORef timedOut (+ 1)}
        r <- disprove cfg {acSeconds = 0.2} hang (fakeRun seen [] (const "")) pol root "X.lean" base "foo"
        r `shouldBe` Right DUnknown
        readIORef timedOut `shouldReturn` 1
