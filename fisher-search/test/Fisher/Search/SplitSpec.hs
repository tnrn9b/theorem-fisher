{-# LANGUAGE DataKinds #-}

module Fisher.Search.SplitSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Fisher.Role (Role (..), Said, partText, source, static, toPrompt)
import Fisher.Role.Internal (said')
import Fisher.Search.Golden
import Fisher.Search.GraphJobs (sketchProblem)
import Fisher.Search.Sketch (SketchError (..), parseSketch, sketchErrorText)
import Fisher.Search.Split

-- | A planner's reply, as the role monad's ask gives it.
planner :: Text -> Said 'Planner
planner = said'

spec :: Spec
spec = do
  describe "plannerMessages" $ do
    it "first round" $
      golden "planner-first.txt" (render (plannerMessages "theorem main : P := by sorry" "main" [] (static "# Plan for main\n\n- main (open)") [] mempty Nothing))
    it "repair round with failed attempts" $
      golden "planner-repair.txt" (render (plannerMessages "theorem main : P := by sorry" "main" ["aux", "Foo.b"] (static "# Plan for main\n\n- main (open)")
        ["tried simp, stuck on the second case"] mempty (Just (toPrompt (planner "```lean\nlemma x : foo := by sorry\n```"), LeanErrors "error: unknown identifier 'foo'"))))
    it "repair round after a reply that could not be used" $ do
      let ms = plannerMessages "f" "main" [] (static "wb") [] mempty (Just (toPrompt (planner "no blocks here"), FormatError (static "the reply needs a blueprint block and a lean block")))
      map fst ms `shouldBe` ["system", "user", "assistant", "user"]
      partText (snd (ms !! 2)) `shouldBe` "no blocks here"
      partText (snd (ms !! 3)) `shouldSatisfy` T.isPrefixOf "Your reply could not be used: the reply needs a blueprint block and a lean block"
    it "repair round after a sketch with a denied line: the line is the planner's, the bytes are the sentence's" $ do
      let lean = "axiom cheat : P\ntheorem main : P := cheat"
          reply = planner ("```blueprint\nuse an axiom\n```\n```lean\n" <> lean <> "\n```")
      case parseSketch "theorem main : P := by sorry" "main" lean of
        Left e@(DeniedLine l) -> do
          l `shouldBe` "axiom cheat : P"
          let ms = plannerMessages "f" "main" [] (static "wb") [] mempty (Just (toPrompt reply, FormatError (sketchProblem "main" reply e)))
          partText (snd (ms !! 3)) `shouldBe` ("Your reply could not be used: " <> sketchErrorText e <> "\nReply again with both blocks.")
          partText (snd (ms !! 3)) `shouldSatisfy` T.isInfixOf "turns checks off): axiom cheat : P\n"
        other -> expectationFailure ("expected a denied line, got " <> show other)
  describe "planner retrieval" $
    it "puts the retrieval text after the usable line and before the failed attempts" $ do
      let ms = plannerMessages "f" "main" [] (static "wb") ["stuck"] (source "Relevant project facts:\n- P.bump : def P.bump (n : Nat) : Nat (P/Defs.lean:3)\n") Nothing
          u = partText (snd (ms !! 1))
          at x = T.length (fst (T.breakOn x u))
      u `shouldSatisfy` T.isInfixOf "\n\nRelevant project facts:\n- P.bump : def P.bump (n : Nat) : Nat (P/Defs.lean:3)\n\nDirect attempts"
      at "Besides the new lemmas" `shouldSatisfy` (< at "Relevant project facts:")
  describe "planner notes" $
    it "puts each note on one line, cut to 300 characters" $ do
      let ms = plannerMessages "f" "main" [] (static "wb") ["line one\nline two", T.replicate 400 "z"] mempty Nothing
          u = partText (snd (ms !! 1))
      u `shouldSatisfy` T.isInfixOf "- line one line two\n"
      u `shouldSatisfy` T.isInfixOf ("- " <> T.replicate 300 "z")
      u `shouldSatisfy` (not . T.isInfixOf (T.replicate 301 "z"))
  describe "parsePlan" $ do
    let both = "```blueprint\nlemma a then b\n```\n```lean\nlemma a : A := by sorry\n```"
    it "reads both blocks" $ parsePlan both `shouldBe` Right ("lemma a then b", "lemma a : A := by sorry")
    it "ignores extra prose" $
      parsePlan ("Here you go.\n" <> both <> "\nHope it helps.") `shouldBe` Right ("lemma a then b", "lemma a : A := by sorry")
    it "accepts reverse order" $
      parsePlan "```lean\nX\n```\n```blueprint\nY\n```" `shouldBe` Right ("Y", "X")
    it "takes the first of each tag" $
      parsePlan "```lean\nX\n```\n```lean\nZ\n```\n```blueprint\nY\n```" `shouldBe` Right ("Y", "X")
    it "keeps a nested fenced block inside the blueprint" $
      parsePlan "```blueprint\nExample:\n```lean\nlemma x : X := by sorry\n```\nthen a\n```\n```lean\nREAL\n```"
        `shouldBe` Right ("Example:\n```lean\nlemma x : X := by sorry\n```\nthen a", "REAL")
    it "normalises tag case and lean4" $ do
      parsePlan "```Blueprint\nY\n```\n```Lean4\nX\n```" `shouldBe` Right ("Y", "X")
      parsePlan "```BLUEPRINT\nY\n```\n```lean4\nX\n```" `shouldBe` Right ("Y", "X")
    it "needs both" $ do
      parsePlan "```lean\nX\n```" `shouldBe` Left "the reply needs a blueprint block and a lean block"
      parsePlan "```blueprint\nY\n```" `shouldBe` Left "the reply needs a blueprint block and a lean block"
  where render ms = mconcat [ "[" <> r <> "]\n" <> partText c <> "\n" | (r, c) <- ms ]
