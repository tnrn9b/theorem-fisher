module Fisher.Search.GraphSpec (spec) where

import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck

import Fisher.Search.Graph

lim :: Limits
lim = Limits 3 3

-- | A root with a proposed-and-accepted split of two fresh lemmas (ids 2 and 3).
twoLemmas :: Graph
twoLemmas =
  let (r, g1) = addRoot "main" "k-main" emptyGraph
      (s, g2) = proposeSplit r g1
  in either (error . T.unpack) fst (acceptSplit (const False) s [("a", "ka"), ("b", "kb")] g2)

-- | twoLemmas, then node 2 split into one lemma (node 4, depth 2); node 3 unattempted.
threeLevels :: Graph
threeLevels =
  let (s, g1) = proposeSplit 2 twoLemmas
  in either (error . T.unpack) fst (acceptSplit (const False) s [("c", "kc")] g1)

-- Random operations ------------------------------------------------------

data Op
  = OAddRoot Int
  | OPropose Int
  | OAccept Bool Int [Int]
  | OReject Int
  | OAttempt Int
  | OProve Int
  | OFail Int
  | OFailSplit Int
  | OClose Int
  deriving (Show)

genOp :: Maybe Bool -> Gen Op
genOp alias = oneof
  [ OAddRoot <$> chooseInt (0, 5)
  , OPropose <$> arbitrary
  , OAccept <$> maybe arbitrary pure alias <*> arbitrary <*> resize 3 (listOf (chooseInt (0, 5)))
  , OReject <$> arbitrary
  , OAttempt <$> arbitrary
  , OProve <$> arbitrary
  , OFail <$> arbitrary
  , OFailSplit <$> arbitrary
  , OClose <$> arbitrary
  ]

pick :: Int -> [a] -> Maybe a
pick _ [] = Nothing
pick i xs = Just (xs !! (abs i `mod` length xs))

key :: Int -> Text
key k = "key" <> T.pack (show k)

-- | Graph plus the ids that were proved by markProved.
type St = (Graph, [NodeId])

step :: St -> Op -> St
step st@(g, pr) = \case
  OAddRoot k -> (snd (addRoot ("r" <> T.pack (show k)) (key k) g), pr)
  OPropose i -> case pick i (IM.keys (gNodes g)) of
    Just n -> (snd (proposeSplit n g), pr)
    Nothing -> st
  OAccept al i ks -> case pick i [ sId s | s <- IM.elems (gSplits g), sStatus s == SProposed ] of
    Just s -> case acceptSplit (const al) s [ ("l" <> T.pack (show k), key k) | k <- ks ] g of
      Right (g', _) -> (g', pr)
      Left _ -> st
    Nothing -> st
  OReject i -> case pick i [ sId s | s <- IM.elems (gSplits g), sStatus s == SProposed ] of
    Just s -> (rejectSplit s "no" g, pr)
    Nothing -> st
  OAttempt i -> case pick i (IM.keys (gNodes g)) of
    Just n -> (attemptDone n g, pr)
    Nothing -> st
  OProve i -> case pick i (IM.keys (gNodes g)) of
    Just n -> (markProved n g, n : pr)
    Nothing -> st
  OFail i -> case pick i (IM.keys (gNodes g)) of
    Just n -> (markFailed n "bad" g, pr)
    Nothing -> st
  OFailSplit i -> case pick i [ sId s | s <- IM.elems (gSplits g), sStatus s == SAccepted ] of
    Just s -> (failSplit s "no" g, pr)
    Nothing -> st
  OClose i -> case pick i [ sId s | s <- IM.elems (gSplits g), sStatus s == SAccepted
                              , all (\l -> fmap nStatus (IM.lookup l (gNodes g)) == Just NProved) (sLemmas s) ] of
    Just s -> (closeSplit s g, pr)
    Nothing -> st

build :: [Op] -> St
build = foldl' step (emptyGraph, [])

ops :: Maybe Bool -> Gen [Op]
ops al = listOf (genOp al)

spec :: Spec
spec = do
  describe "addRoot / acceptSplit" $ do
    it "numbers roots from 1 in order" $ do
      let (a, g1) = addRoot "a" "ka" emptyGraph
          (b, g2) = addRoot "b" "kb" g1
      (a, b, gRoots g2) `shouldBe` (1, 2, [1, 2])
    it "gives new nodes for two fresh lemmas" $ do
      let (r, g1) = addRoot "main" "k" emptyGraph
          (s, g2) = proposeSplit r g1
      fmap snd (acceptSplit (const True) s [("a", "ka"), ("b", "kb")] g2)
        `shouldBe` Right [NewNode 2, NewNode 3]
    it "rejects a lemma whose key is the parent's" $ do
      let (r, g1) = addRoot "main" "k" emptyGraph
          (s, g2) = proposeSplit r g1
      fmap snd (acceptSplit (const True) s [("a", "k")] g2) `shouldBe` Left "cycle: a"
    it "rejects a lemma whose key is an ancestor's" $ do
      let (s2, g1) = proposeSplit 2 twoLemmas
      fmap snd (acceptSplit (const True) s2 [("c", "k-main")] g1) `shouldBe` Left "cycle: c"
    it "rejects a key held by a failed node" $ do
      let g1 = markFailed 2 "no" twoLemmas
          (r, g2) = addRoot "other" "ko" g1
          (s, g3) = proposeSplit r g2
      fmap snd (acceptSplit (const True) s [("again", "ka")] g3)
        `shouldBe` Left "the lemma again already failed"
    it "rejects two lemmas with one key" $ do
      let (r, g1) = addRoot "main" "k" emptyGraph
          (s, g2) = proposeSplit r g1
      fmap snd (acceptSplit (const True) s [("a", "x"), ("b", "x")] g2)
        `shouldBe` Left "duplicate lemma b"
    it "reuses a proved node and shares an open one" $ do
      let g1 = markProved 2 twoLemmas
          (r, g2) = addRoot "other" "ko" g1
          (s, g3) = proposeSplit r g2
      fmap snd (acceptSplit (const True) s [("a'", "ka"), ("b'", "kb")] g3)
        `shouldBe` Right [Reused 2, Shared 3]
    it "makes new nodes when aliasing is impossible (keys then repeat, by design)" $ do
      let (r, g1) = addRoot "other" "ko" twoLemmas
          (s, g2) = proposeSplit r g1
      case acceptSplit (const False) s [("a'", "ka")] g2 of
        Right (g3, links) -> do
          links `shouldBe` [NewNode 5]
          length [ () | n <- IM.elems (gNodes g3), nKey n == "ka" ] `shouldBe` 2
        Left e -> expectationFailure (T.unpack e)
    it "marks the split accepted and lists the linked nodes" $ do
      fmap sStatus (IM.lookup 1 (gSplits twoLemmas)) `shouldBe` Just SAccepted
      fmap sLemmas (IM.lookup 1 (gSplits twoLemmas)) `shouldBe` Just [2, 3]
      fmap nDepth (IM.lookup 2 (gNodes twoLemmas)) `shouldBe` Just 1
      fmap nRoot (IM.lookup 3 (gNodes twoLemmas)) `shouldBe` Just 1

  describe "failSplit / closeSplit" $ do
    it "fails a proposed split with its reason" $ do
      let (r, g1) = addRoot "main" "k" emptyGraph
          (s, g2) = proposeSplit r g1
      fmap sStatus (IM.lookup s (gSplits (failSplit s "no blocks" g2))) `shouldBe` Just (SFailed "no blocks")
    it "closes a split as done" $ do
      let (r, g1) = addRoot "main" "k" emptyGraph
          (s, g2) = proposeSplit r g1
      fmap sStatus (IM.lookup s (gSplits (closeSplit s g2))) `shouldBe` Just SDone
  describe "markFailed / doneSplits" $ do
    it "fails the split holding the node" $ do
      let g = markFailed 2 "no" twoLemmas
      fmap sStatus (IM.lookup 1 (gSplits g)) `shouldBe` Just (SFailed "lemma a failed")
      fmap nStatus (IM.lookup 2 (gNodes g)) `shouldBe` Just (NFailed "no")
    it "reports a split done when all lemmas are proved and the parent is open" $ do
      doneSplits twoLemmas `shouldBe` []
      doneSplits (markProved 3 (markProved 2 twoLemmas)) `shouldBe` [1]

  describe "liveNodes" $ do
    it "holds the roots and the lemmas of accepted splits" $
      liveNodes twoLemmas `shouldBe` IS.fromList [1, 2, 3]
    it "drops the lemmas of a failed split, and theirs" $ do
      liveNodes (failSplit 1 "no" threeLevels) `shouldBe` IS.fromList [1]
      liveNodes (failSplit 2 "no" threeLevels) `shouldBe` IS.fromList [1, 2, 3]
    it "keeps the lemmas of a done split" $
      liveNodes (closeSplit 1 twoLemmas) `shouldBe` IS.fromList [1, 2, 3]
    it "keeps a lemma shared with a live split" $ do
      let (r2, g1) = addRoot "two" "k2" twoLemmas
          (s, g2) = proposeSplit r2 g1
          g3 = either (error . T.unpack) fst (acceptSplit (const True) s [("b'", "kb")] g2)
      liveNodes (failSplit 1 "no" g3) `shouldBe` IS.fromList [1, 3, 4]
    it "gives a dead lemma no action and never calls it stuck" $ do
      let g = failSplit 1 "no" (attemptDone 1 twoLemmas)
      nextAction lim IS.empty g `shouldBe` Just (Propose 1)
      stuck (Limits 3 0) IS.empty (attemptDone 2 (attemptDone 3 (snd (proposeSplit 1 g)))) `shouldBe` []

  describe "nextAction" $ do
    let (r, g0) = addRoot "main" "k" emptyGraph
    it "attempts a fresh root, then proposes" $ do
      nextAction lim IS.empty g0 `shouldBe` Just (Attempt r)
      nextAction lim IS.empty (attemptDone r g0) `shouldBe` Just (Propose r)
    it "does not propose while a split is proposed, or past the split limit" $ do
      let g1 = attemptDone r g0
          (_, g2) = proposeSplit r g1
      nextAction lim IS.empty g2 `shouldBe` Nothing
      nextAction (Limits 3 0) IS.empty g1 `shouldBe` Nothing
    it "leaves a covered node alone and works on its lemmas" $ do
      nextAction lim IS.empty (attemptDone 1 twoLemmas) `shouldBe` Just (Attempt 2)
    it "takes the deepest node, ties by lowest id" $ do
      let (s, g1) = proposeSplit 1 twoLemmas
          g2 = markFailed 2 "x" (markFailed 3 "y" g1)
      -- the old split failed; node 1 is uncovered again and is the only open node
      s `shouldBe` 2
      nextAction lim IS.empty g2 `shouldBe` Just (Attempt 1)
      nextAction lim IS.empty twoLemmas `shouldBe` Just (Attempt 2)
    it "prefers the deeper node inside a root" $
      nextAction lim IS.empty threeLevels `shouldBe` Just (Attempt 4)
    it "skips busy nodes" $
      nextAction lim (IS.fromList [2]) twoLemmas `shouldBe` Just (Attempt 3)
    it "gives two attempts and no proposal at the depth limit" $ do
      let l = Limits 0 3
      nextAction l IS.empty g0 `shouldBe` Just (Attempt r)
      nextAction l IS.empty (attemptDone r g0) `shouldBe` Just (Attempt r)
      nextAction l IS.empty (attemptDone r (attemptDone r g0)) `shouldBe` Nothing
      stuck l IS.empty (attemptDone r (attemptDone r g0)) `shouldBe` [r]
    it "is fair between roots" $ do
      let (_, g1) = addRoot "two" "k2" g0
      nextAction lim IS.empty g1 `shouldBe` Just (Attempt 1)
      nextAction lim IS.empty (recordAction 1 g1) `shouldBe` Just (Attempt 2)
    it "counts actions for the root of a lemma" $ do
      gActions (recordAction 2 twoLemmas) `shouldBe` IM.fromList [(1, 1)]
    it "rejects a lemma whose key is the grandparent's" $ do
      let (s, g1) = proposeSplit 4 threeLevels
      fmap snd (acceptSplit (const True) s [("m", "k-main")] g1) `shouldBe` Left "cycle: m"
      ancestors 4 threeLevels `shouldBe` IS.fromList [1, 2]
    it "does not call a node with a pending proposed split stuck" $ do
      let (r, g0) = addRoot "main" "k" emptyGraph
          (_, g1) = proposeSplit r (attemptDone r g0)
      stuck lim IS.empty g1 `shouldBe` []
    it "calls a node stuck once attempts and splits are used up" $ do
      let (r, g0) = addRoot "main" "k" emptyGraph
          (s, g1) = proposeSplit r (attemptDone r g0)
          g2 = rejectSplit s "no" g1
      stuck (Limits 3 1) IS.empty g2 `shouldBe` [r]
      stuck (Limits 3 2) IS.empty g2 `shouldBe` []
    it "reports ancestors" $
      ancestors 2 twoLemmas `shouldBe` IS.fromList [1]

  describe "properties" $ modifyMaxSuccess (const 5000) $ do
    it "never proves a node without markProved" $ property $ forAll (ops Nothing) $ \os ->
      let (g, pr) = build os
      in all (\n -> nStatus n /= NProved || nId n `elem` pr) (IM.elems (gNodes g))
    it "propagates failure to the splits holding the node" $ property $
      forAll (ops Nothing) $ \os -> forAll arbitrary $ \i ->
        let (g, _) = build os
        in case pick i (IM.keys (gNodes g)) of
             Nothing -> True
             Just n ->
               let g' = markFailed n "bad" g
               -- a done split stays done: its lemmas were proved when it closed
               in all (\s -> n `notElem` sLemmas s || sStatus s /= SAccepted)
                      (IM.elems (gSplits g'))
    it "keeps keys unique among aliased lemma nodes" $ property $ forAll (ops (Just True)) $ \os ->
      let (g, _) = build os
          ks = [ nKey n | n <- IM.elems (gNodes g), nId n `notElem` gRoots g ]
      in length ks == length (IS.toList (IS.fromList (map (read . T.unpack . T.drop 3) ks)))
    it "never accepts a cycle" $ property $ forAll (ops Nothing) $ \os ->
      let (g, _) = build os
      in and [ l `notElem` (sParent s : IS.toList (ancestors (sParent s) g))
             | s <- IM.elems (gSplits g), sStatus s == SAccepted, l <- sLemmas s ]
    it "names only open, idle nodes" $ property $
      forAll (ops Nothing) $ \os -> forAll (listOf (chooseInt (1, 12))) $ \bs ->
        let (g, _) = build os
            busy = IS.fromList bs
        in case nextAction lim busy g of
             Nothing -> True
             Just a ->
               let i = case a of { Attempt x -> x; Propose x -> x }
               in fmap nStatus (IM.lookup i (gNodes g)) == Just NOpen && not (IS.member i busy)
    it "never names a node that is not live" $ property $
      forAll (ops Nothing) $ \os -> forAll (listOf (chooseInt (1, 12))) $ \bs ->
        let (g, _) = build os
        in case nextAction lim (IS.fromList bs) g of
             Nothing -> True
             Just a -> IS.member (case a of { Attempt x -> x; Propose x -> x }) (liveNodes g)
    it "calls only live nodes stuck" $ property $
      forAll (ops Nothing) $ \os ->
        let (g, _) = build os
        in all (`IS.member` liveNodes g) (stuck lim IS.empty g)
    it "is fair between roots" $ property $
      forAll (ops Nothing) $ \os -> forAll (listOf (chooseInt (1, 12))) $ \bs ->
      forAll (listOf (chooseInt (1, 12))) $ \rs ->
        let (g0, _) = build os
            g = foldl' (flip recordAction) g0 rs
            busy = IS.fromList bs
            acts r = IM.findWithDefault 0 r (gActions g)
            hasWork r = nextAction lim (IS.fromList [ nId n | n <- IM.elems (gNodes g), nRoot n /= r ] `IS.union` busy) g /= Nothing
        in case nextAction lim busy g of
             Nothing -> True
             Just a ->
               let i = case a of { Attempt x -> x; Propose x -> x }
                   r0 = maybe 0 nRoot (IM.lookup i (gNodes g))
               in and [ acts r0 <= acts r | r <- gRoots g, hasWork r ]
    it "picks the deepest actionable node of its root" $ property $
      forAll (ops Nothing) $ \os -> forAll (listOf (chooseInt (1, 12))) $ \bs ->
        let (g, _) = build os
            busy = IS.fromList bs
            idOf = \case { Attempt x -> x; Propose x -> x }
            depthOf i = maybe 0 nDepth (IM.lookup i (gNodes g))
            actionable n = nextAction lim (IS.fromList (filter (/= nId n) (IM.keys (gNodes g))) `IS.union` busy) g
                             /= Nothing && not (IS.member (nId n) busy)
        in case nextAction lim busy g of
             Nothing -> True
             Just a ->
               let i = idOf a
                   r0 = maybe 0 nRoot (IM.lookup i (gNodes g))
               in and [ depthOf i >= nDepth n | n <- IM.elems (gNodes g), nRoot n == r0, actionable n ]
    it "reaches every node from a root" $ property $ forAll (ops Nothing) $ \os ->
      let (g, _) = build os
          held = IS.fromList (concatMap sLemmas (IM.elems (gSplits g)))
      in all (\n -> nId n `elem` gRoots g || IS.member (nId n) held) (IM.elems (gNodes g))
