module Fisher.Search.AutoSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Clock (getMonotonicTime)
import Test.Hspec

import Fisher.Search.Auto
import Fisher.Search.Lean

declOf :: Text -> Decl
declOf src = case declarations src of
  (d : _) -> d
  [] -> error "no declaration in fixture"

holes :: Text -> [Hole]
holes src = holesIn src (declOf src)

-- | A fake Lsp: records each snippet list sent and answers with 'answer'.
fakeLsp :: IORef [[Text]] -> (Text -> IO (Either Text Trial)) -> Lsp
fakeLsp sent answer =
  Lsp
    { lspAttempts = \_ _ snips -> do
        modifyIORef sent (<> [snips])
        rs <- mapM answer snips
        pure (sequence rs)
    , lspGoal = \_ _ -> pure (Right Nothing)
    , lspDiagnostics = \_ -> pure (Right [])
    , lspClose = pure ()
    , lspTimedOut = pure ()
    }

-- | Closes a snippet whose tactic is one of the given ones.
closing :: [Text] -> Text -> IO (Either Text Trial)
closing oks s =
  pure . Right $
    if any (\t -> wrapSnippet 200000 t == s) oks
      then Trial s [] []
      else Trial s ["P"] []

fixedClock :: IO Double
fixedClock = pure 0

spec :: Spec
spec = do
  describe "holesIn" $ do
    it "finds a tactic hole" $
      holes "theorem a : P := by\n  intro x\n  sorry" `shouldBe` [Hole 3 2 False]
    it "finds a term hole" $
      map hTerm (holes "theorem b : P := sorry") `shouldBe` [True]
    it "puts a term hole at its column" $
      holes "theorem b : P := sorry" `shouldBe` [Hole 1 17 True]
    it "a term hole on the next line" $
      holes "theorem b : P :=\n  sorry" `shouldBe` [Hole 2 2 True]
    it "exact sorry is a term hole" $
      holes "theorem a : P := by\n  exact sorry" `shouldBe` [Hole 2 8 True]
    it "anonymous constructor holes are term holes" $
      holes "theorem t : P ∧ Q := ⟨sorry, sorry⟩" `shouldBe` [Hole 1 22 True, Hole 1 29 True]
    it "a sorry first on its line under a nested by is a tactic hole" $
      holes "theorem f : P :=\n  foo (by\n    sorry)" `shouldBe` [Hole 3 4 False]
    it "a sorry after a bullet is a tactic hole" $
      holes "theorem g : P := by\n  · sorry" `shouldBe` [Hole 2 4 False]
    it "a sorry after case ... => is a tactic hole" $
      holes "theorem g : P ∨ Q := by\n  cases h\n  case inl h => sorry" `shouldBe` [Hole 3 16 False]
    it "a sorry after ; or <;> is a tactic hole" $
      holes "theorem g : P := by\n  intro x; sorry\n  constructor <;> sorry"
        `shouldBe` [Hole 2 11 False, Hole 3 18 False]
    it "a sorry after a tactic combinator is a tactic hole" $
      mapM_
        (\(pre, col) -> holes ("theorem g : P := by\n  " <> pre <> "sorry") `shouldBe` [Hole 2 col False])
        [ ("first | ", 10), ("first | rfl | ", 16), ("rfl <|> ", 10), ("try ", 6), ("repeat ", 9)
        , ("all_goals ", 12), ("any_goals ", 12), ("focus ", 8), ("on_goal 2 => ", 15) ]
    it "a sorry after <| or || is still a term hole" $
      holes "theorem g : P := by\n  exact f <| sorry\n  exact a || sorry"
        `shouldBe` [Hole 2 13 True, Hole 3 13 True]
    it "a word ending in a combinator name is not a combinator" $
      holes "theorem g : P := by\n  exact retry sorry" `shouldBe` [Hole 2 14 True]
    it "a have's := sorry is a term hole" $
      holes "theorem g : P := by\n  have h : Q := sorry\n  exact h" `shouldBe` [Hole 2 16 True]
    it "skips char literals" $
      holes "theorem c : P := by\n  simp [f '\"', g '\\n', h' 'x']\n  sorry" `shouldBe` [Hole 3 2 False]
    it "inline := by sorry is a tactic hole" $
      holes "theorem a : P := by sorry" `shouldBe` [Hole 1 20 False]
    it "skips sorry in comments and strings" $
      holes
        "theorem c : P := by\n  -- sorry here\n  /- a sorry\n  sorry -/\n  simp [\"sorry\"]\n  sorry"
        `shouldBe` [Hole 6 2 False]
    it "skips identifiers containing sorry" $
      holes "theorem c : P := by\n  exact sorryAx _ Foo.sorry\n  sorry" `shouldBe` [Hole 3 2 False]
    it "gives two holes in line order" $
      holes "theorem d : P ∧ Q := by\n  constructor\n  · sorry\n  · sorry"
        `shouldBe` [Hole 3 4 False, Hole 4 4 False]
    it "counts columns in code points" $
      holes "theorem e : ∀ x : ℕ, x = x := by\n  exact f 𝓝 ∀ sorry" `shouldBe` [Hole 2 14 True]
    it "only looks inside the declaration" $ do
      let src = "theorem a : P := by\n  sorry\n\ntheorem b : Q := by\n  sorry"
      case findDecl "b" (declarations src) of
        Just d -> holesIn src d `shouldBe` [Hole 5 2 False]
        Nothing -> expectationFailure "no b"

  describe "wrapSnippet" $
    it "sets the heartbeats" $
      wrapSnippet 200000 "omega" `shouldBe` "set_option maxHeartbeats 200000 in omega"

  describe "winner" $ do
    it "takes the first trial with no goals and no errors" $
      winner
        [ Trial "rfl" ["P"] []
        , Trial "decide" [] [Diag 1 0 SevError "failed"]
        , Trial "omega" [] [Diag 1 0 SevWarning "unused variable"]
        , Trial "simp_all" [] []
        ]
        `shouldBe` Just "omega"
    it "rejects a sorry warning" $
      winner [Trial "aesop" [] [Diag 1 0 SevWarning "declaration uses 'sorry'"]] `shouldBe` Nothing
    it "reads a Try this suggestion" $
      suggestion (Trial "exact?" [] [Diag 3 2 SevInfo "Try this: exact Nat.le_refl n"])
        `shouldBe` Just "exact Nat.le_refl n"
    it "reads a suggestion from a warning" $
      suggestion (Trial "exact?" [] [Diag 3 2 SevWarning "Try this: exact Nat.le_refl n"])
        `shouldBe` Just "exact Nat.le_refl n"
    it "returns the suggestion in place of exact?" $
      winner [Trial "exact?" [] [Diag 3 2 SevInfo "Try this: exact Nat.le_refl n"]]
        `shouldBe` Just "exact Nat.le_refl n"
    it "keeps the heartbeat wrapper around a suggestion" $
      winner [Trial (wrapSnippet 200000 "exact?") [] [Diag 3 2 SevInfo "Try this: exact h"]]
        `shouldBe` Just (wrapSnippet 200000 "exact h")
    it "is Nothing with no trials" $
      winner [] `shouldBe` Nothing
    -- the messages as Lean 4.33 and Mathlib print them (recorded on the Mac,
    -- 2026-10-15, from exact?, apply? and aesop? in a Mathlib fixture)
    it "reads Lean 4.33's exact? suggestion below the heading, after [apply]" $
      suggestion (Trial "exact?" [] [Diag 4 39 SevInfo "Try this:\n  [apply] exact Nat.add_eq_left.mpr rfl"])
        `shouldBe` Just "exact Nat.add_eq_left.mpr rfl"
    it "reads Lean 4.33's apply? suggestion" $
      suggestion (Trial "apply?" [] [Diag 5 53 SevInfo "Try this:\n  [apply] exact Nat.le_add_right_of_le h"])
        `shouldBe` Just "exact Nat.le_add_right_of_le h"
    it "reads aesop?'s suggestion after a blank line" $
      suggestion (Trial "aesop?" [] [Diag 6 56 SevInfo "Try this:\n\n  [apply]   simp_all only [and_self]"])
        `shouldBe` Just "simp_all only [and_self]"
    it "keeps a suggestion over several lines, its lines aligned" $
      suggestion (Trial "aesop?" [] [Diag 7 63 SevInfo "Try this:\n\n  [apply]     simp_all only [ne_eq, gt_iff_lt]\n    sorry"])
        `shouldBe` Just "simp_all only [ne_eq, gt_iff_lt]\nsorry"
    it "keeps deeper lines of a suggestion deeper" $
      suggestion (Trial "aesop?" [] [Diag 1 0 SevInfo "Try this:\n  [apply] constructor\n  · simp\n  · intro x\n    simp"])
        `shouldBe` Just "constructor\n· simp\n· intro x\n  simp"
    it "an empty suggestion is none" $
      suggestion (Trial "exact?" [] [Diag 1 0 SevInfo "Try this:\n\n"]) `shouldBe` Nothing
    it "puts a Lean 4.33 suggestion in place of exact?, under the heartbeat wrapper" $
      winner [Trial (wrapSnippet 200000 "exact?") [] [Diag 3 2 SevInfo "Try this:\n  [apply] exact Nat.ne_zero_of_lt h"]]
        `shouldBe` Just (wrapSnippet 200000 "exact Nat.ne_zero_of_lt h")
    it "a search tactic left in the fill never wins" $ do
      winner [Trial (wrapSnippet 200000 "exact?") [] []] `shouldBe` Nothing
      winner [Trial "apply?" [] [Diag 1 0 SevInfo "no suggestion here"]] `shouldBe` Nothing
      winner [Trial "aesop?" [] [Diag 1 0 SevInfo "Try this: aesop?"]] `shouldBe` Nothing
      winner [Trial "exact?" [] [], Trial "omega" [] []] `shouldBe` Just "omega"
    it "a multi-line suggestion that holds sorry loses" $
      winner [Trial "aesop?" [] [Diag 7 63 SevInfo "Try this:\n\n  [apply]     simp_all only [ne_eq, gt_iff_lt]\n    sorry"]]
        `shouldBe` Nothing

  describe "fillHole" $ do
    it "replaces exactly the sorry" $
      fillHole "theorem a : P := by\n  intro x\n  sorry\n" (Hole 3 2 False) "omega"
        `shouldBe` "theorem a : P := by\n  intro x\n  omega\n"
    it "replaces the sorry after exact" $
      fillHole "theorem a : P := by\n  exact sorry" (Hole 2 8 True) "h"
        `shouldBe` "theorem a : P := by\n  exact (by h)"
    it "fills a term hole inside a term in parentheses" $
      fillHole "theorem t : P ∧ Q := ⟨sorry, sorry⟩" (Hole 1 29 True) "rfl"
        `shouldBe` "theorem t : P ∧ Q := ⟨sorry, (by rfl)⟩"
    it "fills a have's := sorry with a bare by" $
      fillHole "theorem g : P := by\n  have h : Q := sorry" (Hole 2 16 True) "simp"
        `shouldBe` "theorem g : P := by\n  have h : Q := by simp"
    it "fills a := sorry on the line after the statement with a bare by" $
      fillHole "theorem b : P :=\n  sorry" (Hole 2 2 True) "rfl" `shouldBe` "theorem b : P :=\n  by rfl"
    it "leaves the source alone when the hole is not a sorry" $
      fillHole "theorem a : P := by\n  simp" (Hole 2 2 False) "rfl" `shouldBe` "theorem a : P := by\n  simp"
    it "fills a term hole with by" $
      fillHole "theorem b : P := sorry" (Hole 1 17 True) "omega"
        `shouldBe` "theorem b : P := by omega"
    it "keeps the hole's indentation on continuation lines" $
      fillHole "theorem d : P := by\n  · sorry\n  · rfl" (Hole 2 4 False) "constructor\n· simp\n· omega"
        `shouldBe` "theorem d : P := by\n  · constructor\n    · simp\n    · omega\n  · rfl"
    it "indents a multi-line term fill under its by" $
      fillHole "theorem b : P := sorry" (Hole 1 17 True) "constructor\nall_goals simp"
        `shouldBe` "theorem b : P := by constructor\n                    all_goals simp"
    it "leaves other sorries alone" $
      fillHole "theorem d : P := by\n  · sorry\n  · sorry" (Hole 3 4 False) "rfl"
        `shouldBe` "theorem d : P := by\n  · sorry\n  · rfl"

  describe "fillHoleAt" $ do
    it "gives fillHole's text and where a tactic snippet ends" $
      fillHoleAt "theorem a : P := by\n  sorry\n" (Hole 2 2 False) "omega"
        `shouldBe` Just ("theorem a : P := by\n  omega\n", (1, 7))
    it "ends a term snippet inside its parentheses" $
      fillHoleAt "theorem t : P ∧ Q := ⟨sorry, sorry⟩" (Hole 1 29 True) "rfl"
        `shouldBe` Just ("theorem t : P ∧ Q := ⟨sorry, (by rfl)⟩", (0, 36))
    it "ends a multi-line snippet on its last line" $
      fillHoleAt "theorem d : P := by\n  · sorry\n  · rfl" (Hole 2 4 False) "constructor\n· simp"
        `shouldBe` Just ("theorem d : P := by\n  · constructor\n    · simp\n  · rfl", (2, 10))
    it "is Nothing when the hole is not a sorry" $
      fillHoleAt "theorem a : P := by\n  simp" (Hole 2 2 False) "rfl" `shouldBe` Nothing

  describe "cutAfter" $ do
    let src = "import X\n\nnamespace N\n\ntheorem a : P := by\n  sorry\n\ntheorem b : Q := by\n  simp\n\nend N\n"
    it "keeps the file through the declaration's last line" $
      cutAfter src (declOf src) `shouldBe` "import X\n\nnamespace N\n\ntheorem a : P := by\n  sorry"
    it "cuts at the declaration holding the hole" $ do
      cutAtHole src (Hole 6 2 False) `shouldBe` "import X\n\nnamespace N\n\ntheorem a : P := by\n  sorry"
      cutAtHole src (Hole 9 2 False) `shouldBe` "import X\n\nnamespace N\n\ntheorem a : P := by\n  sorry\n\ntheorem b : Q := by\n  simp"
    it "keeps the whole file for a hole no declaration holds" $
      cutAtHole src (Hole 1 0 False) `shouldBe` src

  describe "autoHole" $ do
    let src = "theorem a : P := by\n  sorry"
        h = Hole 2 2 False
        cfg = defaultAutoConfig
    it "sends one wrapped snippet per call in order and stops at the winner" $ do
      sent <- newIORef []
      r <- autoHole cfg (fakeLsp sent (closing ["omega"])) fixedClock src h
      r `shouldBe` Right (wrapSnippet 200000 "omega")
      readIORef sent
        `shouldReturn` [[wrapSnippet 200000 t] | t <- ["rfl", "decide", "omega"]]
    it "misses after the whole portfolio" $ do
      sent <- newIORef []
      r <- autoHole cfg (fakeLsp sent (closing [])) fixedClock src h
      r `shouldBe` Left Missed
      length <$> readIORef sent `shouldReturn` length defaultTactics
    it "stops sending once the wall cap has passed" $ do
      sent <- newIORef []
      now <- newIORef (0 :: Double)
      let base = fakeLsp sent (closing [])
          lsp = base {lspAttempts = \f hh ss -> modifyIORef now (+ 31) >> lspAttempts base f hh ss}
      r <- autoHole cfg lsp (readIORef now) src h
      r `shouldBe` Left Missed
      readIORef sent `shouldReturn` [[wrapSnippet 200000 t] | t <- ["rfl", "decide"]]
    it "cuts off a call that hangs past the cap" $ do
      sent <- newIORef []
      let base = fakeLsp sent (closing ["rfl"])
          lsp = base {lspAttempts = \f hh ss -> threadDelay 5000000 >> lspAttempts base f hh ss}
      t0 <- getMonotonicTime
      r <- autoHole cfg {acSeconds = 0.3} lsp getMonotonicTime src h
      t1 <- getMonotonicTime
      r `shouldBe` Left Missed
      (t1 - t0) `shouldSatisfy` (< 2)
    it "turns an exception from the Lsp into LspDown" $ do
      let lsp = (fakeLsp undefined (closing [])) {lspAttempts = \_ _ _ -> throwIO (userError "boom")}
      r <- autoHole cfg lsp fixedClock src h
      case r of
        Left (LspDown m) -> T.unpack m `shouldContain` "boom"
        other -> expectationFailure (show other)
    it "reports a dead Lsp" $ do
      let lsp = (fakeLsp undefined (closing [])) {lspAttempts = \_ _ _ -> pure (Left "dead")}
      r <- autoHole cfg lsp fixedClock src h
      r `shouldBe` Left (LspDown "dead")
    it "sends the file only up to the end of the hole's declaration" $ do
      seenSrc <- newIORef []
      sent <- newIORef []
      let base = fakeLsp sent (closing ["rfl"])
          lsp = base {lspAttempts = \f hh ss -> modifyIORef seenSrc (<> [f]) >> lspAttempts base f hh ss}
      r <- autoHole cfg lsp fixedClock (src <> "\n\ntheorem b : Q := by\n  simp\n") h
      r `shouldBe` Right (wrapSnippet 200000 "rfl")
      readIORef seenSrc `shouldReturn` [src]
    it "reports a call cut off at the cap through lspTimedOut" $ do
      timedOut <- newIORef (0 :: Int)
      sent <- newIORef []
      let base = fakeLsp sent (closing ["rfl"])
          lsp = base {lspAttempts = \f hh ss -> threadDelay 5000000 >> lspAttempts base f hh ss, lspTimedOut = modifyIORef timedOut (+ 1)}
      r <- autoHole cfg {acSeconds = 0.2} lsp getMonotonicTime src h
      r `shouldBe` Left Missed
      readIORef timedOut `shouldReturn` 1
    it "uses acTactics" $ do
      sent <- newIORef []
      r <- autoHole cfg {acTactics = ["norm_num", "linarith"]} (fakeLsp sent (closing ["linarith"])) fixedClock src h
      r `shouldBe` Right (wrapSnippet 200000 "linarith")
      readIORef sent `shouldReturn` [[wrapSnippet 200000 "norm_num"], [wrapSnippet 200000 "linarith"]]

  describe "autoGoal" $ do
    let base =
          T.unlines
            [ "theorem other : R := by"
            , "  sorry"
            , ""
            , "theorem d : P ∧ Q := by"
            , "  constructor"
            , "  · sorry"
            , "  · sorry"
            ]
        perHole closes = \_ hole snips -> pure . Right $
          [ if (hLine hole, s) `elem` closes then Trial s [] [] else Trial s ["G"] [] | s <- snips ]
        lspWith closes = (fakeLsp undefined (closing [])) {lspAttempts = perHole closes}
    it "misses when only one hole closes" $ do
      r <- autoGoal defaultAutoConfig (lspWith [(6, wrapSnippet 200000 "rfl")]) fixedClock base "d"
      r `shouldBe` Left Missed
    it "fills every hole when all close" $ do
      let closes = [(6, wrapSnippet 200000 "rfl"), (7, wrapSnippet 200000 "simp_all")]
      r <- autoGoal defaultAutoConfig (lspWith closes) fixedClock base "d"
      r
        `shouldBe` Right
          ( T.unlines
              [ "theorem other : R := by"
              , "  sorry"
              , ""
              , "theorem d : P ∧ Q := by"
              , "  constructor"
              , "  · set_option maxHeartbeats 200000 in rfl"
              , "  · set_option maxHeartbeats 200000 in simp_all"
              ]
          , [wrapSnippet 200000 "rfl", wrapSnippet 200000 "simp_all"]
          )
    it "fills two holes on one line from the right" $ do
      let src = "theorem e : P ∧ Q := by\n  refine ⟨sorry, sorry⟩"
          lsp = (fakeLsp undefined (closing [])) {lspAttempts = \_ hole snips -> pure (Right [Trial (s <> T.pack (show (hCol hole))) [] [] | s <- snips])}
      r <- autoGoal defaultAutoConfig {acTactics = ["rfl"]} lsp fixedClock src "e"
      r `shouldBe` Right
        ( "theorem e : P ∧ Q := by\n  refine ⟨(by set_option maxHeartbeats 200000 in rfl10), (by set_option maxHeartbeats 200000 in rfl17)⟩"
        , [wrapSnippet 200000 "rfl" <> "10", wrapSnippet 200000 "rfl" <> "17"] )
    it "ignores the sorry warning that sibling holes cause" $ do
      let warn = Diag 4 8 SevWarning "declaration uses 'sorry'"
          lsp = (fakeLsp undefined (closing [])) {lspAttempts = \_ _ snips -> pure (Right [Trial s [] [warn] | s <- snips])}
      r <- autoGoal defaultAutoConfig {acTactics = ["rfl"]} lsp fixedClock base "d"
      fmap snd r `shouldBe` Right [wrapSnippet 200000 "rfl", wrapSnippet 200000 "rfl"]
    it "still rejects a sibling-hole trial whose fill is a sorry" $ do
      let warn = Diag 4 8 SevWarning "declaration uses 'sorry'"
          sugg = Diag 6 4 SevInfo "Try this: exact sorry"
          lsp = (fakeLsp undefined (closing [])) {lspAttempts = \_ _ snips -> pure (Right [Trial s [] [warn, sugg] | s <- snips])}
      r <- autoGoal defaultAutoConfig {acTactics = ["exact?"]} lsp fixedClock base "d"
      r `shouldBe` Left Missed
    it "still rejects admit with sibling holes" $ do
      let warn = Diag 4 8 SevWarning "declaration uses 'sorry'"
          lsp = (fakeLsp undefined (closing [])) {lspAttempts = \_ _ snips -> pure (Right [Trial s [] [warn] | s <- snips])}
      r <- autoGoal defaultAutoConfig {acTactics = ["admit"]} lsp fixedClock base "d"
      r `shouldBe` Left Missed
    it "a single hole still loses to the sorry warning" $ do
      let warn = Diag 1 8 SevWarning "declaration uses 'sorry'"
          lsp = (fakeLsp undefined (closing [])) {lspAttempts = \_ _ snips -> pure (Right [Trial s [] [warn] | s <- snips])}
      r <- autoGoal defaultAutoConfig {acTactics = ["rfl"]} lsp fixedClock "theorem a : P := by\n  sorry" "a"
      r `shouldBe` Left Missed
    it "propagates a dead Lsp" $ do
      let lsp = (fakeLsp undefined (closing [])) {lspAttempts = \_ _ _ -> pure (Left "gone")}
      r <- autoGoal defaultAutoConfig lsp fixedClock base "d"
      r `shouldBe` Left (LspDown "gone")
    it "misses an unknown goal" $ do
      r <- autoGoal defaultAutoConfig (lspWith []) fixedClock base "nope"
      r `shouldBe` Left Missed
