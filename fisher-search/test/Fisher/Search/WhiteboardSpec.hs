module Fisher.Search.WhiteboardSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Fisher.Role (partText, static)
import Fisher.Search.Golden
import Fisher.Search.Graph
import Fisher.Search.Whiteboard

-- | main splits into a, b; a splits into c, and b is shared with c's split; a rejected split is not shown.
tree :: Graph
tree =
  let (r, g1) = addRoot "main" "k-main" emptyGraph
      (s1, g2) = proposeSplit r g1
      (s0, g3) = proposeSplit r (acc s1 [("a", "ka"), ("b", "kb")] g2)
      g4 = rejectSplit s0 "no" g3
      (s2, g5) = proposeSplit 2 g4
      g6 = acc s2 [("c", "kc"), ("b", "kb")] g5
  in markProved 4 g6
  where acc s ls g = either (error . T.unpack) fst (acceptSplit (const True) s ls g)

spec :: Spec
spec = do
  describe "workerEnd" $ do
    it "NOTE last" $ workerEnd (Just "work\nNOTE: stuck on c") "fb" `shouldBe` Note "stuck on c"
    it "FALSE last" $ workerEnd (Just "NOTE: x\nFALSE: n = 0") "fb" `shouldBe` FalseClaim "n = 0"
    it "trailing prose after NOTE" $ workerEnd (Just "NOTE: gave up\nThanks.\n\n") "fb" `shouldBe` Note "gave up"
    it "CRLF" $ workerEnd (Just "a\r\nNOTE: done\r\n") "fb" `shouldBe` Note "done"
    it "indented tag" $ workerEnd (Just "  FALSE:  bad  ") "fb" `shouldBe` FalseClaim "bad"
    it "bare NOTE gives the fallback" $ workerEnd (Just "text\nNOTE:  ") "fb" `shouldBe` Note "fb"
    it "neither" $ workerEnd (Just "just text") "fb" `shouldBe` Note "fb"
    it "nothing" $ workerEnd Nothing "fb" `shouldBe` Note "fb"
    it "a tag in markdown emphasis" $ do
      workerEnd (Just "x\n**NOTE:** tried simp") "fb" `shouldBe` Note "tried simp"
      workerEnd (Just "*NOTE:* tried ring") "fb" `shouldBe` Note "tried ring"
      workerEnd (Just "`NOTE:` stuck") "fb" `shouldBe` Note "stuck"
      workerEnd (Just "**FALSE:** n = 0 fails") "fb" `shouldBe` FalseClaim "n = 0 fails"
      workerEnd (Just "`FALSE:`  n = 1") "fb" `shouldBe` FalseClaim "n = 1"
      workerEnd (Just "**NOTE:**") "fb" `shouldBe` Note "fb"
  describe "whiteboard" $ do
    it "two levels with failures, shared node twice" $
      golden "whiteboard.txt" (partText (whiteboard tree 1 (map static ["a: simp failed", "b: wrong type"])))
    it "no failures" $ T.isInfixOf "## Failed" (partText (whiteboard tree 1 [])) `shouldBe` False
    it "caps at 2000 and keeps the newest failures" $ do
      let fs = [ T.pack (show i) <> ": " <> T.replicate 90 "x" | i <- [100 .. 132 :: Int] ]
          w = partText (whiteboard tree 1 (map static fs))
      T.length (T.intercalate "\n" fs) `shouldSatisfy` (> 3000)
      T.length w `shouldSatisfy` (<= 2000)
      w `shouldSatisfy` T.isInfixOf "132: "
      w `shouldSatisfy` (not . T.isInfixOf "100: ")
    it "cuts a huge tree with a final ..." $ do
      let (_, g1) = addRoot "r" "kr" emptyGraph
          (s, g2) = proposeSplit 1 g1
          names = [ (T.pack ("lemma" <> show i) <> T.replicate 30 "y", T.pack (show i)) | i <- [1 .. 100 :: Int] ]
          g3 = either (error . T.unpack) fst (acceptSplit (const False) s names g2)
          w = partText (whiteboard g3 1 [static "f"])
      T.length w `shouldSatisfy` (<= 2000)
      w `shouldSatisfy` T.isSuffixOf "..."
  describe "workerBrief" $ do
    it "golden" $ golden "brief.txt" (partText (workerBrief "main" "theorem main : P" ["aux", "Foo.b"] (static "# Plan for main\n\n- main (open)")))
    it "with no plan lemmas" $
      partText (workerBrief "main" "theorem main : P" [] (static "wb")) `shouldSatisfy` T.isInfixOf "\nUse no declaration that still ends in `sorry`.\n"
