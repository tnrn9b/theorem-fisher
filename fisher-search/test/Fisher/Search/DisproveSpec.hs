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
import Fisher.Smt (Model (..), SmtResult (..), Solver (..), Sort (..), Value (..))

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

-- | 'disprove' with plausible alone, its verdict only.
plain :: AutoConfig -> Lsp -> LeanRun -> AxiomPolicy -> FilePath -> FilePath -> Text -> Text -> IO (Either AutoFail Disproof)
plain c l lr p r rel b n = fmap (\(d, _, _) -> d) <$> disprove Nothing [ViaPlausible] c l lr p r rel b n

-- | An Lsp whose plausible trial is @pl@ and whose query tactic prints
-- @query@ as smt_show does.
smtLsp :: IORef [[Text]] -> Trial -> Text -> Lsp
smtLsp sent pl query = (lspWith sent pl)
  { lspAttempts = \src _ snips -> do
      modifyIORef sent (<> [snips, [src]])
      pure (Right [ if "plausible" `T.isInfixOf` s then pl {tSnippet = s}
                    else Trial s ["P"] [errDiag "unsolved goals", Diag 2 2 SevInfo ("goal: P\n\nquery:\n" <> query)]
                  | s <- snips ]) }

-- | A solver stand-in recording each query it is given.
fakeSmt :: IORef [Text] -> SmtResult -> SmtEnv
fakeSmt qs r = SmtEnv (Z3 "/nonexistent/z3") 7 "smt_show" (\_ secs q -> modifyIORef qs (<> [T.pack (show secs) <> " " <> q]) >> pure r)

smtBase :: Text
smtBase = "theorem foo : \8704 x y : Int, x * y = x + y := by\n  sorry"

smtQuery :: Text
smtQuery = "(declare-const x Int)\n(declare-const y Int)\n(assert (not (= (* x y) (+ x y))))\n(check-sat)"

counter :: Model
counter = Model [("x", SInt, VInt 1), ("y", SInt, VInt 0)] []

witness :: Text
witness = "push_neg; refine \10216(1 : Int), (0 : Int), ?_\10217; first | decide | norm_num | omega | simp_all"

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
  describe "the smt source" $ do
    it "queryTactic: smt_show, or the trace fallback" $ do
      queryTactic "smt_show" `shouldBe` "smt_show"
      queryTactic "trace" `shouldBe` "set_option trace.smt.translate.query true in smt"
    it "parseSmtQuery from smt_show" $ do
      let tr = Trial "smt_show" ["P"] [errDiag "unsolved goals", Diag 3 2 SevInfo ("goal: \8704 x : Int, x = x\n\nquery:\n" <> smtQuery)]
      parseSmtQuery "smt_show" tr `shouldBe` Just smtQuery
      parseSmtQuery "smt_show" (Trial "smt_show" ["P"] [errDiag "unsolved goals"]) `shouldBe` Nothing
      parseSmtQuery "smt_show" (Trial "smt_show" ["P"] [errDiag ("query:\n" <> smtQuery)]) `shouldBe` Nothing
    it "parseSmtQuery from the trace" $ do
      let tr = Trial "t" [] [Diag 3 2 SevInfo ("[smt.translate.query] " <> smtQuery)]
      parseSmtQuery "trace" tr `shouldBe` Just smtQuery
      parseSmtQuery "trace" (Trial "t" [] [Diag 3 2 SevInfo "[smt.preprocess] after cleanup"]) `shouldBe` Nothing
    it "boundVariables: binders before the colon, then the leading foralls" $ do
      boundVariables "theorem foo (n : Nat) {m : Int} :\n    \8704 x y : Int, x > 0 \8594 \8704 z : Int, P := by"
        `shouldBe` [("n", "Nat"), ("m", "Int"), ("x", "Int"), ("y", "Int")]
      boundVariables "\8704 (a : BitVec 8) (b : Bool), P a b" `shouldBe` [("a", "BitVec 8"), ("b", "Bool")]
      boundVariables "theorem t (f : Nat \8594 Nat) : \8704 x, \8704 y : \8484, f x = x :=" `shouldBe` [("f", "Nat \8594 Nat"), ("x", ""), ("y", "\8484")]
      boundVariables "theorem t [inst : Foo] (x : Nat) : P x := by" `shouldBe` []
      boundVariables "theorem t : 1 + 1 = 2 := by" `shouldBe` []
    it "witnessTactic renders Int Bool BitVec" $ do
      let m = Model [("x", SInt, VInt (-3)), ("|b|", SBool, VBool True), ("v", SBitVec 8, VBitVec 8 31)] []
      witnessTactic m [] [("x", "Int"), ("b", "Bool"), ("v", "BitVec 8")]
        `shouldBe` Just "push_neg; refine \10216(-3 : Int), true, 0x1f#8, ?_\10217; first | decide | norm_num | omega | simp_all"
      witnessTactic m [] [("x", ""), ("b", "Prop")]
        `shouldBe` Just "push_neg; refine \10216(-3), True, ?_\10217; first | decide | norm_num | omega | simp_all"
      -- a variable the model lacks, a value of another sort, no variables
      witnessTactic m [] [("x", "Int"), ("z", "Int")] `shouldBe` Nothing
      witnessTactic m [] [("b", "Int")] `shouldBe` Nothing
      witnessTactic m [] [] `shouldBe` Nothing
      witnessTactic (Model [("r", SOther "Real", VOther "(/ 1.0 2.0)")] []) [] [("r", "Real")] `shouldBe` Nothing
    it "uninterpreted model gives no witness" $
      withSystemTempDirectory "dis" $ \root -> do
        let m = Model [("x", SInt, VInt 1), ("y", SInt, VInt 0)] ["f"]
        witnessTactic m [] [("x", "Int"), ("y", "Int")] `shouldBe` Nothing
        sent <- newIORef []
        seen <- newIORef []
        qs <- newIORef []
        r <- disprove (Just (fakeSmt qs (Sat m))) [ViaSmt] cfg (smtLsp sent none smtQuery) (fakeRun seen [] (const "")) pol root "X.lean" smtBase "foo"
        r `shouldBe` Right (DSuspect "x := 1\ny := 0\nf := (a function)", Just ViaSmt, DisproveInfo [(ViaSmt, "sat")] Nothing)
        ts <- readIORef seen
        any (T.isInfixOf "push_neg") ts `shouldBe` False
        length ts `shouldBe` length certifyTactics
    it "witnessTactic matches lean-smt's inaccessible names" $ do
      let m = Model [("|x\10013|", SInt, VInt 1), ("|y\10013\185|", SInt, VInt (-2))] []
      leanName "|x\10013\185\178|" `shouldBe` "x"
      witnessTactic m [] [("x", "Int"), ("y", "Int")]
        `shouldBe` Just "push_neg; refine \10216(1 : Int), (-2 : Int), ?_\10217; first | decide | norm_num | omega | simp_all"
      boundVariables "theorem t : \8704 x y : Int, x + y = 0 := by" `shouldBe` [("x", "Int"), ("y", "Int")]
    it "witnessTactic falls back to the query's declare-const order" $ do
      let q = "(set-logic ALL)\n(declare-const |a\10013| Int)\n(declare-const b Bool)\n(declare-fun f (Int) Int)\n(assert (not (= |a\10013| 0)))\n(check-sat)\n"
          m = Model [("b", SBool, VBool False), ("|a\10013|", SInt, VInt 4)] []
      declaredConsts q `shouldBe` ["|a\10013|", "b"]
      witnessTactic m (declaredConsts q) [("n", "Int"), ("p", "Bool")]
        `shouldBe` Just "push_neg; refine \10216(4 : Int), false, ?_\10217; first | decide | norm_num | omega | simp_all"
      -- too few constants, or no query: no witness
      witnessTactic m ["|a\10013|"] [("n", "Int"), ("p", "Bool")] `shouldBe` Nothing
      witnessTactic m [] [("n", "Int")] `shouldBe` Nothing
      -- two constants with one Lean name: positional, which here has no constants
      witnessTactic (Model [("|x\10013|", SInt, VInt 1), ("|x\10013\185|", SInt, VInt 2)] []) [] [("x", "Int")] `shouldBe` Nothing
      -- the refusals hold on the positional route too
      witnessTactic (Model [("|k\10013|", SInt, VInt (-1))] []) ["|k\10013|"] [("n", "Nat")] `shouldBe` Nothing
      witnessTactic (Model [("|k\10013|", SInt, VInt 1)] ["g"]) ["|k\10013|"] [("n", "Int")] `shouldBe` Nothing
    it "negative Nat refused" $ do
      witnessTactic (Model [("n", SInt, VInt (-1))] []) [] [("n", "Nat")] `shouldBe` Nothing
      witnessTactic (Model [("n", SInt, VInt (-1))] []) [] [("n", "\8469")] `shouldBe` Nothing
      witnessTactic (Model [("n", SInt, VInt 2)] []) [] [("n", "\8469")]
        `shouldBe` Just "push_neg; refine \10216(2 : Nat), ?_\10217; first | decide | norm_num | omega | simp_all"
    it "a certified model is DFalse via the witness" $
      withSystemTempDirectory "dis" $ \root -> do
        sent <- newIORef []
        seen <- newIORef []
        qs <- newIORef []
        r <- disprove (Just (fakeSmt qs (Sat counter))) [ViaSmt] cfg (smtLsp sent none smtQuery) (fakeRun seen [witness] (const "")) pol root "X.lean" smtBase "foo"
        r `shouldBe` Right (DFalse witness, Just ViaSmt, DisproveInfo [(ViaSmt, "sat")] (Just "x := 1\ny := 0"))
        -- the query tactic at the hole of the file cut after the goal
        readIORef sent >>= (`shouldBe` [[wrapSnippet 200000 "smt_show"], [smtBase]])
        -- the query, prepared (get-model added), under the env's cap
        readIORef qs >>= (`shouldBe` ["7 " <> smtQuery <> "\n(get-model)\n"])
        -- the witness is the first and only probe
        ts <- readIORef seen
        length ts `shouldBe` 1
        T.isInfixOf witness (head ts) `shouldBe` True
    it "the real smt_show shape: inaccessible names, certified via the witness" $
      withSystemTempDirectory "dis" $ \root -> do
        sent <- newIORef []
        seen <- newIORef []
        qs <- newIORef []
        let q = "(declare-const |x\10013| Int)\n(declare-const |y\10013| Int)\n(assert (not (= (* |x\10013| |y\10013|) (+ |x\10013| |y\10013|))))\n(check-sat)"
            m = Model [("|x\10013|", SInt, VInt 1), ("|y\10013|", SInt, VInt 0)] []
        r <- disprove (Just (fakeSmt qs (Sat m))) [ViaSmt] cfg (smtLsp sent none q) (fakeRun seen [witness] (const "")) pol root "X.lean" smtBase "foo"
        fmap (\(d, src, _) -> (d, src)) r `shouldBe` Right (DFalse witness, Just ViaSmt)
        length <$> readIORef seen `shouldReturn` 1
    it "a spurious model stays DSuspect" $
      withSystemTempDirectory "dis" $ \root -> do
        sent <- newIORef []
        seen <- newIORef []
        qs <- newIORef []
        r <- disprove (Just (fakeSmt qs (Sat counter))) [ViaSmt] cfg (smtLsp sent none smtQuery) (fakeRun seen [] (const "")) pol root "X.lean" smtBase "foo"
        r `shouldBe` Right (DSuspect "x := 1\ny := 0", Just ViaSmt, DisproveInfo [(ViaSmt, "sat")] Nothing)
        -- the witness, then every certify tactic
        length <$> readIORef seen >>= (`shouldBe` 1 + length certifyTactics)
    it "a model certified by a plain tactic is DFalse with no witness" $
      withSystemTempDirectory "dis" $ \root -> do
        sent <- newIORef []
        seen <- newIORef []
        qs <- newIORef []
        r <- disprove (Just (fakeSmt qs (Sat counter))) [ViaSmt] cfg (smtLsp sent none smtQuery) (fakeRun seen ["omega"] (const "")) pol root "X.lean" smtBase "foo"
        r `shouldBe` Right (DFalse "omega", Just ViaSmt, DisproveInfo [(ViaSmt, "sat")] Nothing)
    it "unsat is DUnknown with source smt" $
      withSystemTempDirectory "dis" $ \root -> do
        sent <- newIORef []
        seen <- newIORef []
        qs <- newIORef []
        r <- disprove (Just (fakeSmt qs Unsat)) [ViaSmt] cfg (smtLsp sent none smtQuery) (fakeRun seen ["decide"] (const "")) pol root "X.lean" smtBase "foo"
        r `shouldBe` Right (DUnknown, Just ViaSmt, DisproveInfo [(ViaSmt, "unsat")] Nothing)
        readIORef seen >>= (`shouldBe` [])
        -- unknown and a solver error are ignored the same way
        r2 <- disprove (Just (fakeSmt qs (Unknown "timeout"))) [ViaSmt] cfg (smtLsp sent none smtQuery) (fakeRun seen ["decide"] (const "")) pol root "X.lean" smtBase "foo"
        r2 `shouldBe` Right (DUnknown, Just ViaSmt, DisproveInfo [(ViaSmt, "unknown")] Nothing)
        r3 <- disprove (Just (fakeSmt qs (SolverError "boom"))) [ViaSmt] cfg (smtLsp sent none smtQuery) (fakeRun seen ["decide"] (const "")) pol root "X.lean" smtBase "foo"
        r3 `shouldBe` Right (DUnknown, Just ViaSmt, DisproveInfo [(ViaSmt, "error")] Nothing)
        readIORef seen >>= (`shouldBe` [])
    it "no query is none, and the solver is not run" $
      withSystemTempDirectory "dis" $ \root -> do
        sent <- newIORef []
        seen <- newIORef []
        qs <- newIORef []
        r <- disprove (Just (fakeSmt qs Unsat)) [ViaSmt] cfg (lspWith sent none) (fakeRun seen [] (const "")) pol root "X.lean" smtBase "foo"
        r `shouldBe` Right (DUnknown, Just ViaSmt, DisproveInfo [(ViaSmt, "none")] Nothing)
        readIORef qs >>= (`shouldBe` [])
    it "no SmtEnv runs plausible only" $
      withSystemTempDirectory "dis" $ \root -> do
        sent <- newIORef []
        seen <- newIORef []
        r <- disprove Nothing [ViaPlausible, ViaSmt] cfg (smtLsp sent found smtQuery) (fakeRun seen [] (const "")) pol root "X.lean" base "foo"
        r `shouldBe` Right (DSuspect "x := 3\ny := 0", Just ViaPlausible, DisproveInfo [(ViaPlausible, "counterexample")] Nothing)
        readIORef sent >>= (`shouldBe` [[wrapSnippet 200000 "plausible"], ["theorem foo : P := by\n  sorry"]])
    it "after plausible, a model is tried with its witness alone, and the first suspect is kept" $
      withSystemTempDirectory "dis" $ \root -> do
        sent <- newIORef []
        seen <- newIORef []
        qs <- newIORef []
        r <- disprove (Just (fakeSmt qs (Sat counter))) [ViaPlausible, ViaSmt] cfg (smtLsp sent found smtQuery) (fakeRun seen [] (const "")) pol root "X.lean" smtBase "foo"
        r `shouldBe` Right (DSuspect "x := 3\ny := 0", Just ViaPlausible, DisproveInfo [(ViaPlausible, "counterexample"), (ViaSmt, "sat")] Nothing)
        length <$> readIORef seen >>= (`shouldBe` length certifyTactics + 1)
        -- and a witness that certifies wins over the suspect
        seen2 <- newIORef []
        r2 <- disprove (Just (fakeSmt qs (Sat counter))) [ViaPlausible, ViaSmt] cfg (smtLsp sent found smtQuery) (fakeRun seen2 [witness] (const "")) pol root "X.lean" smtBase "foo"
        r2 `shouldBe` Right (DFalse witness, Just ViaSmt, DisproveInfo [(ViaPlausible, "counterexample"), (ViaSmt, "sat")] (Just "x := 1\ny := 0"))
    it "a dead Lsp at the query is LspDown" $
      withSystemTempDirectory "dis" $ \root -> do
        seen <- newIORef []
        qs <- newIORef []
        let lsp = (lspWith undefined found) {lspAttempts = \_ _ _ -> pure (Left "boom")}
        r <- disprove (Just (fakeSmt qs Unsat)) [ViaSmt] cfg lsp (fakeRun seen [] (const "")) pol root "X.lean" smtBase "foo"
        r `shouldBe` Left (LspDown "boom")
  describe "disprove" $ do
    it "certifies with the first tactic that checks, the base cut after the declaration, the negation's axioms printed" $
      withSystemTempDirectory "dis" $ \root -> do
        sent <- newIORef []
        seen <- newIORef []
        r <- plain cfg (lspWith sent found) (fakeRun seen ["simp_all"] (const "")) pol root "X.lean" base "foo"
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
        r <- plain cfg (lspWith sent found) (fakeRun seen ["decide"] (const "")) pol root "X.lean"
               ("namespace Ns\n\n" <> base <> "\n\nend Ns") "Ns.foo"
        r `shouldBe` Right (DFalse "decide")
        ts <- readIORef seen
        any (T.isInfixOf "#print axioms Ns._fisher_neg_") ts `shouldBe` True
        any (T.isInfixOf "end Ns") ts `shouldBe` False
    it "a negation whose axioms hold sorryAx is not a disproof" $
      withSystemTempDirectory "dis" $ \root -> do
        sent <- newIORef []
        seen <- newIORef []
        r <- plain cfg (lspWith sent found) (fakeRunAx seen ["decide"] (const "") ["propext", "sorryAx"]) pol root "X.lean" base "foo"
        r `shouldBe` Right (DSuspect "x := 3\ny := 0")
    it "a negation on an axiom outside the policy is not a disproof, and is with the axiom allowed" $
      withSystemTempDirectory "dis" $ \root -> do
        sent <- newIORef []
        seen <- newIORef []
        let lean = fakeRunAx seen ["decide"] (const "") ["propext", "Foo.ax"]
        r <- plain cfg (lspWith sent found) lean pol root "X.lean" base "foo"
        r `shouldBe` Right (DSuspect "x := 3\ny := 0")
        r2 <- plain cfg (lspWith sent found) lean pol {apAllowed = Set.insert "Foo.ax" standardAxioms} root "X.lean" base "foo"
        r2 `shouldBe` Right (DFalse "decide")
    it "a negation with no axioms printed is not a disproof" $
      withSystemTempDirectory "dis" $ \root -> do
        sent <- newIORef []
        let silent = LeanRun $ \_ _ -> pure (0, "")
        r <- plain cfg (lspWith sent found) silent pol root "X.lean" base "foo"
        r `shouldBe` Right (DSuspect "x := 3\ny := 0")
    it "a counter-example no tactic certifies stays a suspect" $
      withSystemTempDirectory "dis" $ \root -> do
        sent <- newIORef []
        seen <- newIORef []
        r <- plain cfg (lspWith sent found) (fakeRun seen [] (const "")) pol root "X.lean" base "foo"
        r `shouldBe` Right (DSuspect "x := 3\ny := 0")
        length <$> readIORef seen >>= (`shouldBe` 4)
    it "no counter-example is unknown and runs no probe" $
      withSystemTempDirectory "dis" $ \root -> do
        sent <- newIORef []
        seen <- newIORef []
        r <- plain cfg (lspWith sent none) (fakeRun seen ["decide"] (const "")) pol root "X.lean" base "foo"
        r `shouldBe` Right DUnknown
        readIORef seen >>= (`shouldBe` [])
    it "a sorry warning on the negation's line does not certify" $
      withSystemTempDirectory "dis" $ \root -> do
        sent <- newIORef []
        seen <- newIORef []
        let warn i = "X.lean:" <> T.pack (show i) <> ":0: warning: declaration uses 'sorry'"
        r <- plain cfg (lspWith sent found) (fakeRun seen ["decide"] warn) pol root "X.lean" base "foo"
        r `shouldBe` Right (DSuspect "x := 3\ny := 0")
    it "a sorry warning on another line still certifies" $
      withSystemTempDirectory "dis" $ \root -> do
        sent <- newIORef []
        seen <- newIORef []
        r <- plain cfg (lspWith sent found) (fakeRun seen ["decide"] (const "X.lean:1:8: warning: declaration uses 'sorry'")) pol root "X.lean" base "foo"
        r `shouldBe` Right (DFalse "decide")
    it "a dead Lsp is LspDown" $
      withSystemTempDirectory "dis" $ \root -> do
        seen <- newIORef []
        let lsp = (lspWith undefined found) {lspAttempts = \_ _ _ -> pure (Left "boom")}
        r <- plain cfg lsp (fakeRun seen [] (const "")) pol root "X.lean" base "foo"
        r `shouldBe` Left (LspDown "boom")
    it "an unknown goal is missed" $
      withSystemTempDirectory "dis" $ \root -> do
        sent <- newIORef []
        seen <- newIORef []
        r <- plain cfg (lspWith sent found) (fakeRun seen [] (const "")) pol root "X.lean" base "nope"
        r `shouldBe` Left Missed
    it "a probe that throws counts as a failed tactic" $
      withSystemTempDirectory "dis" $ \root -> do
        sent <- newIORef []
        let boom = LeanRun $ \_ _ -> throwIO (userError "lean gone")
        r <- plain cfg (lspWith sent found) boom pol root "X.lean" base "foo"
        r `shouldBe` Right (DSuspect "x := 3\ny := 0")
    it "each probe has its own cap: every certify tactic runs even after plausible took its time" $
      withSystemTempDirectory "dis" $ \root -> do
        sent <- newIORef []
        runs <- newIORef (0 :: Int)
        let slowLsp = (lspWith sent found) {lspAttempts = \_ _ snips -> threadDelay 150000 >> pure (Right [found {tSnippet = s} | s <- snips])}
            slow = LeanRun $ \_ _ -> modifyIORef runs (+ 1) >> threadDelay 3000000 >> pure (0, "")
        t0 <- getMonotonicTime
        r <- plain cfg {acSeconds = 0.2} slowLsp slow pol root "X.lean" base "foo"
        t1 <- getMonotonicTime
        r `shouldBe` Right (DSuspect "x := 3\ny := 0")
        readIORef runs `shouldReturn` 4
        (t1 - t0) `shouldSatisfy` (< 2)
    it "a plausible call cut off at the cap is unknown and reported as a timeout" $
      withSystemTempDirectory "dis" $ \root -> do
        timedOut <- newIORef (0 :: Int)
        seen <- newIORef []
        let hang = (lspWith undefined found) {lspAttempts = \_ _ _ -> threadDelay 3000000 >> pure (Right []), lspTimedOut = modifyIORef timedOut (+ 1)}
        r <- plain cfg {acSeconds = 0.2} hang (fakeRun seen [] (const "")) pol root "X.lean" base "foo"
        r `shouldBe` Right DUnknown
        readIORef timedOut `shouldReturn` 1
