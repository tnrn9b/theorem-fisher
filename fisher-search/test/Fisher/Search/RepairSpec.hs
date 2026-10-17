module Fisher.Search.RepairSpec (spec) where

import Control.Concurrent (threadDelay)
import Data.IORef
import Data.Text (Text)
import GHC.Clock (getMonotonicTime)
import qualified Data.Text as T
import Test.Hspec

import Fisher.Search.Auto
import Fisher.Search.Lean
import Fisher.Search.Repair

declOf :: Text -> Decl
declOf src = case declarations src of
  (d : _) -> d
  [] -> error "no declaration in fixture"

-- | A fake Lsp: diagnostics from a function of the file, Auto closing a
-- hole on the given lines, a goal naming the hole's line.
fakeLsp :: IORef Int -> (Text -> [Diag]) -> [Int] -> Lsp
fakeLsp calls diags closeAt =
  Lsp
    { lspAttempts = \_ h snips ->
        pure (Right [Trial s (if hLine h `elem` closeAt then [] else ["P"]) [] | s <- snips])
    , lspGoal = \_ h -> pure (Right (Just ("goal at " <> T.pack (show (hLine h)))))
    , lspDiagnostics = \src -> modifyIORef calls (+ 1) >> pure (Right (diags src))
    , lspClose = pure ()
    , lspTimedOut = pure ()
    }

err :: Int -> Diag
err l = Diag l 2 SevError "type mismatch"

-- | Errors on every line of the file starting with the prefix.
errorsAt :: Text -> Text -> [Diag]
errorsAt pre src = [err i | (i, l) <- zip [1 ..] (T.splitOn "\n" src), pre `T.isPrefixOf` l]

-- | Errors on the first line starting with the prefix.
firstErrorAt :: Text -> Text -> [Diag]
firstErrorAt pre = take 1 . errorsAt pre

cfg :: AutoConfig
cfg = defaultAutoConfig {acTactics = ["rfl"]}

clock :: IO Double
clock = pure 0

run :: (Text -> [Diag]) -> [Int] -> Text -> Text -> IO (Either AutoFail (Maybe Repaired), Int)
run diags closeAt stmt src = do
  calls <- newIORef 0
  r <- repair cfg (fakeLsp calls diags closeAt) clock stmt src "t"
  n <- readIORef calls
  pure (r, n)

stmtT :: Text
stmtT = "theorem t : P := by"

spec :: Spec
spec = do
  describe "tacticUnit" $ do
    it "a plain tactic line and its deeper continuation" $
      tacticUnit ["theorem t : P := by", "  intro x", "  apply foo", "    bar", "    baz", "  exact h"] 2 3
        `shouldBe` (3, 6)
    it "inside a have ... := by: the have's body" $
      tacticUnit ["theorem t : P := by", "  have h : Q := by", "    simp", "    exact bad", "  exact h"] 2 4
        `shouldBe` (3, 5)
    it "inside a bullet branch: the branch body" $
      tacticUnit ["theorem t : P := by", "  constructor", "  · simp", "    exact bad", "  · rfl"] 2 4
        `shouldBe` (3, 5)
    it "inside a case: the case body" $
      tacticUnit ["theorem t : P := by", "  cases h", "  case inl h =>", "    simp", "    exact bad", "  case inr h => rfl"] 2 5
        `shouldBe` (4, 6)
    it "the last line of the proof alone" $
      tacticUnit ["theorem t : P := by", "  intro x", "  exact h"] 2 3 `shouldBe` (3, 4)
    it "a continuation line belongs to its tactic" $
      tacticUnit ["theorem t : P := by", "  apply foo", "    bar", "  exact h"] 2 3 `shouldBe` (2, 4)
    it "stops at the next line at the same depth and leaves trailing blanks out" $
      tacticUnit ["theorem t : P := by", "  apply foo", "    bar", "", "  exact h"] 2 2 `shouldBe` (2, 4)

  describe "sorryUnits" $ do
    it "replaces a unit with one sorry at its indentation" $ do
      let src = "theorem t : P := by\n  intro x\n  have h : Q := by\n    simp\n    exact bad\n  exact h"
      sorryUnits src (declOf src) [5]
        `shouldBe` "theorem t : P := by\n  intro x\n  have h : Q := by\n    sorry\n  exact h"
    it "collapses overlapping units to the outer one" $ do
      let src = "theorem t : P := by\n  apply foo\n    bar\n    baz\n  exact h"
      sorryUnits src (declOf src) [2, 3, 4] `shouldBe` "theorem t : P := by\n  sorry\n  exact h"
    it "keeps a bullet header" $ do
      let src = "theorem t : P ∧ Q := by\n  constructor\n  · simp\n    exact bad\n  · rfl"
      sorryUnits src (declOf src) [4] `shouldBe` "theorem t : P ∧ Q := by\n  constructor\n  · sorry\n  · rfl"
    it "replaces several units" $ do
      let src = "theorem t : P := by\n  intro x\n  simp\n  omega\n  exact h"
      sorryUnits src (declOf src) [3, 5] `shouldBe` "theorem t : P := by\n  intro x\n  sorry\n  omega\n  sorry"
    it "a case unit's sorry is a tactic hole" $ do
      let src = "theorem t : P ∨ Q → R := by\n  intro h\n  cases h\n  case inl h => simp\n    exact bad\n  case inr h => rfl"
          out = sorryUnits src (declOf src) [5]
      out `shouldBe` "theorem t : P ∨ Q → R := by\n  intro h\n  cases h\n  case inl h => sorry\n  case inr h => rfl"
      holesIn out (declOf out) `shouldBe` [Hole 4 16 False]
    it "a | alternative's sorry is a tactic hole" $ do
      let src = "theorem t : P ∨ Q → R := by\n  intro h\n  cases h with\n  | inl h => simp\n    exact bad\n  | inr h => rfl"
          out = sorryUnits src (declOf src) [5]
      out `shouldBe` "theorem t : P ∨ Q → R := by\n  intro h\n  cases h with\n  | inl h => sorry\n  | inr h => rfl"
      holesIn out (declOf out) `shouldBe` [Hole 4 13 False]
    it "ignores lines outside the proof" $ do
      let src = "theorem t : P := by\n  simp\n\ntheorem u : Q := by\n  simp"
      sorryUnits src (declOf src) [1, 5] `shouldBe` src

  describe "repair" $ do
    let file =
          T.intercalate
            "\n"
            [ "theorem t : P := by"
            , "  intro x"
            , "  have h : Q := by"
            , "    simp"
            , "    exact bad"
            , "  constructor"
            , "  · linarith"
            , "  · exact h"
            ]
        roundOne src = if "exact bad" `T.isInfixOf` src then [err 5, err 7] else []
    it "sorries the failing units, then Auto fills what it can" $ do
      (r, n) <- run roundOne [4] stmtT file
      n `shouldBe` 3
      r `shouldBe` Right
        ( Just
            Repaired
              { rpText =
                  T.intercalate
                    "\n"
                    [ "theorem t : P := by"
                    , "  intro x"
                    , "  have h : Q := by"
                    , "    set_option maxHeartbeats 200000 in rfl"
                    , "  constructor"
                    , "  · sorry"
                    , "  · exact h"
                    ]
              , rpHolesBefore = 2
              , rpHolesAfter = 1
              , rpAutoClosed = 1
              , rpGoals = [(Hole 6 4 False, "goal at 6")]
              }
        )
    it "rpGoals holds every remaining hole with its goal" $ do
      (r, _) <- run roundOne [] stmtT file
      fmap (fmap rpGoals) r `shouldBe` Right (Just [(Hole 4 4 False, "goal at 4"), (Hole 6 4 False, "goal at 6")])
      fmap (fmap rpAutoClosed) r `shouldBe` Right (Just 0)
    it "Auto runs on every hole, the agent's own sorries included" $ do
      let src = "theorem t : P := by\n  constructor\n  · sorry\n  · simp"
      (r, _) <- run (firstErrorAt "  · simp") [3, 4] stmtT src
      fmap (fmap (\x -> (rpHolesBefore x, rpAutoClosed x, rpHolesAfter x))) r `shouldBe` Right (Just (2, 2, 0))
    it "drops the result when the final check finds an error in the target" $ do
      let diags s = (if "exact bad" `T.isInfixOf` s then [err 5, err 7] else []) <> [err 4 | "maxHeartbeats" `T.isInfixOf` s]
      (r, n) <- run diags [4] stmtT file
      r `shouldBe` Right Nothing
      n `shouldBe` 3
    it "the final check ignores errors outside the target" $ do
      let src = file <> "\n\ntheorem u : Q := by\n  simp"
          diags s = err 11 : (if "exact bad" `T.isInfixOf` s then [err 5, err 7] else [])
      (r, _) <- run diags [4] stmtT src
      fmap (fmap rpAutoClosed) r `shouldBe` Right (Just 1)
    let tacs k = T.intercalate "\n" (stmtT : ["  tac" <> T.pack (show i) | i <- [1 .. k :: Int]] <> ["  exact h"])
    it "allows four rounds" $ do
      (r, n) <- run (firstErrorAt "  tac") [] stmtT (tacs 4)
      n `shouldBe` 6
      fmap (fmap rpHolesBefore) r `shouldBe` Right (Just 4)
    it "gives up when errors remain after four rounds" $ do
      (r, n) <- run (firstErrorAt "  tac") [] stmtT (tacs 5)
      n `shouldBe` 5
      r `shouldBe` Right Nothing
    it "gives up when errors stay put" $ do
      (r, _) <- run (const [err 2]) [] stmtT "theorem t : P := by\n  simp\n  exact h"
      r `shouldBe` Right Nothing
    it "keeps five holes" $ do
      (r, _) <- run (\s -> if "  tac" `T.isInfixOf` s then errorsAt "  tac" s else []) [] stmtT (tacs 5)
      fmap (fmap rpHolesBefore) r `shouldBe` Right (Just 5)
    it "drops six holes" $ do
      (r, _) <- run (\s -> if "  tac" `T.isInfixOf` s then errorsAt "  tac" s else []) [] stmtT (tacs 6)
      r `shouldBe` Right Nothing
    it "drops a proof of only sorry" $ do
      (r, _) <- run (\s -> if "simp" `T.isInfixOf` s then [err 2, err 3] else []) [] stmtT "theorem t : P := by\n  simp\n  exact h"
      r `shouldBe` Right Nothing
    it "drops a changed statement without asking Lean" $ do
      (r, n) <- run roundOne [] "theorem t : Q := by" file
      r `shouldBe` Right Nothing
      n `shouldBe` 0
    it "ignores errors outside the target" $ do
      let src = "theorem t : P := by\n  simp\n\ntheorem u : Q := by\n  simp"
      (r, n) <- run (const [err 5]) [] stmtT src
      r `shouldBe` Right Nothing
      n `shouldBe` 1
    it "repairs the target while ignoring another declaration's errors" $ do
      let src = "theorem t : P := by\n  intro x\n  simp\n\ntheorem u : Q := by\n  simp"
      (r, _) <- run (\s -> err 6 : [err 3 | "intro x\n  simp" `T.isInfixOf` s]) [] stmtT src
      fmap (fmap rpText) r `shouldBe` Right (Just "theorem t : P := by\n  intro x\n  sorry\n\ntheorem u : Q := by\n  simp")
    it "sends the Lsp each file only up to the end of the target" $ do
      sent <- newIORef []
      calls <- newIORef 0
      let src = "theorem t : P := by\n  intro x\n  simp\n\ntheorem u : Q := by\n  simp"
          base = fakeLsp calls (\s -> [err 3 | "intro x\n  simp" `T.isInfixOf` s]) []
          lsp = base { lspDiagnostics = \f -> modifyIORef sent (<> [f]) >> lspDiagnostics base f
                     , lspGoal = \f hh -> modifyIORef sent (<> [f]) >> lspGoal base f hh
                     , lspAttempts = \f hh ss -> modifyIORef sent (<> [f]) >> lspAttempts base f hh ss }
      r <- repair cfg lsp clock stmtT src "t"
      fmap (fmap rpText) r `shouldBe` Right (Just "theorem t : P := by\n  intro x\n  sorry\n\ntheorem u : Q := by\n  simp")
      texts <- readIORef sent
      length texts `shouldSatisfy` (>= 4)
      filter (T.isInfixOf "theorem u") texts `shouldBe` []
    it "stops at its wall cap with nothing repaired, and says it timed out" $ do
      calls <- newIORef 0
      timedOut <- newIORef (0 :: Int)
      let base = fakeLsp calls (errorsAt "  simp") []
          slow = base { lspDiagnostics = \f -> threadDelay 400000 >> lspDiagnostics base f
                      , lspTimedOut = modifyIORef timedOut (+ 1) }
      t0 <- getMonotonicTime
      r <- repair cfg {acRepairSeconds = 0.6} slow getMonotonicTime stmtT "theorem t : P := by\n  intro x\n  simp" "t"
      t1 <- getMonotonicTime
      r `shouldBe` Right Nothing
      (t1 - t0) `shouldSatisfy` (< 1.2)
      -- the first call answered, the second cut off by the cap
      readIORef calls `shouldReturn` 1
      readIORef timedOut `shouldReturn` 1
    it "gives up when an error sits on the statement" $ do
      (r, _) <- run (\s -> err 1 : [err 3 | "simp" `T.isInfixOf` s]) [] stmtT "theorem t : P := by\n  intro x\n  simp"
      r `shouldBe` Right Nothing
    it "drops a unit left with no goals" $ do
      let src = "theorem t : P := by\n  intro x\n  simp\n  exact h"
          diags s
            | "simp" `T.isInfixOf` s = [err 3]
            | "exact h" `T.isInfixOf` s = [Diag 4 2 SevError "no goals to be proved"]
            | otherwise = []
      (r, _) <- run diags [] stmtT src
      fmap (fmap rpText) r `shouldBe` Right (Just "theorem t : P := by\n  intro x\n  sorry")
    it "a no-goals error on the first line of a have body sorries the body" $ do
      let src = "theorem t : P := by\n  intro x\n  have h : Q := by\n    simp\n  exact h"
          diags s = [Diag 4 4 SevError "no goals to be proved" | "simp" `T.isInfixOf` s]
      (r, _) <- run diags [] stmtT src
      fmap (fmap rpText) r `shouldBe` Right (Just "theorem t : P := by\n  intro x\n  have h : Q := by\n    sorry\n  exact h")
    it "a dead Lsp is LspDown" $ do
      calls <- newIORef 0
      let lsp = (fakeLsp calls (const []) []) {lspDiagnostics = \_ -> pure (Left "gone")}
      r <- repair cfg lsp clock stmtT file "t"
      r `shouldBe` Left (LspDown "gone")
    it "an Lsp exception is LspDown" $ do
      calls <- newIORef 0
      let lsp = (fakeLsp calls (const []) []) {lspDiagnostics = \_ -> ioError (userError "boom")}
      r <- repair cfg lsp clock stmtT file "t"
      case r of
        Left (LspDown _) -> pure ()
        other -> expectationFailure (show other)
    it "an unknown goal gives Nothing" $ do
      calls <- newIORef 0
      r <- repair cfg (fakeLsp calls roundOne []) clock stmtT file "nope"
      r `shouldBe` Right Nothing
    it "the round constants" $ (maxRepairRounds, maxRepairHoles) `shouldBe` (4, 5)
