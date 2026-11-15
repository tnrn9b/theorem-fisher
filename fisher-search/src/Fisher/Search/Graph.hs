-- | The lemma graph of a proof search: open goals, the splits proposed for them,
-- and the lemmas those splits introduce. Pure; the driver holds the state.
module Fisher.Search.Graph
  ( NodeId, SplitId
  , NodeStatus (..), SplitStatus (..)
  , Node (..), Split (..), Graph (..), Limits (..), Action (..), Link (..)
  , emptyGraph, addRoot, proposeSplit, rejectSplit, failSplit, closeSplit, acceptSplit
  , AcceptError (..), acceptErrorText, acceptSplitWhy
  , attemptDone, markProved, markFailed, setPartial
  , doneSplits, liveNodes, planLemmas, nextAction, stuck, recordAction, ancestors
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List (sortOn)
import Data.Maybe (listToMaybe)
import Data.Bifunctor (first)
import Data.Text (Text)

type NodeId = Int
type SplitId = Int

data NodeStatus = NOpen | NProved | NFailed Text deriving (Eq, Show)

data SplitStatus = SProposed | SRejected Text | SAccepted | SFailed Text | SDone
  deriving (Eq, Show)

data Node = Node
  { nId :: NodeId, nName :: Text, nKey :: Text, nRoot :: NodeId, nDepth :: Int
  , nStatus :: NodeStatus, nSplits :: [SplitId], nAttempts :: Int
  , nPartial :: Maybe (Text, Text, Text)
  -- ^ from Repair: the failed attempt's start, the repaired file and the
  -- brief listing its holes, for the node's next attempt
  } deriving (Eq, Show)

data Split = Split
  { sId :: SplitId, sParent :: NodeId, sLemmas :: [NodeId], sStatus :: SplitStatus
  } deriving (Eq, Show)

data Graph = Graph
  { gNodes :: IntMap Node, gSplits :: IntMap Split, gRoots :: [NodeId], gActions :: IntMap Int
  } deriving (Eq, Show)

data Limits = Limits { lMaxDepth :: Int, lMaxSplits :: Int } deriving (Eq, Show)

data Action = Attempt NodeId | Propose NodeId deriving (Eq, Show)

data Link = NewNode NodeId | Shared NodeId | Reused NodeId deriving (Eq, Show)

emptyGraph :: Graph
emptyGraph = Graph IM.empty IM.empty [] IM.empty

-- | A new root goal: name, key.
addRoot :: Text -> Text -> Graph -> (NodeId, Graph)
addRoot name key g =
  let i = IM.size (gNodes g) + 1
      n = Node i name key i 0 NOpen [] 0 Nothing
  in (i, g { gNodes = IM.insert i n (gNodes g), gRoots = gRoots g ++ [i] })

proposeSplit :: NodeId -> Graph -> (SplitId, Graph)
proposeSplit nid g =
  let i = IM.size (gSplits g) + 1
      s = Split i nid [] SProposed
  in (i, g { gSplits = IM.insert i s (gSplits g)
           , gNodes = IM.adjust (\n -> n { nSplits = nSplits n ++ [i] }) nid (gNodes g) })

rejectSplit :: SplitId -> Text -> Graph -> Graph
rejectSplit sid why g =
  g { gSplits = IM.adjust (\s -> s { sStatus = SRejected why }) sid (gSplits g) }

-- | A split that could not be used (its sketch failed, or a lemma failed).
failSplit :: SplitId -> Text -> Graph -> Graph
failSplit sid why g = g { gSplits = IM.adjust (\s -> s { sStatus = SFailed why }) sid (gSplits g) }

-- | A split whose sketch closed its node outright.
closeSplit :: SplitId -> Graph -> Graph
closeSplit sid g = g { gSplits = IM.adjust (\s -> s { sStatus = SDone }) sid (gSplits g) }

-- | Why a split could not be accepted; the lemma names are the split's.
data AcceptError = NoSuchSplit | NotProposed | NoSuchParent | Cycle Text | DuplicateLemma Text | LemmaFailed Text
  deriving (Eq, Show)

acceptErrorText :: AcceptError -> Text
acceptErrorText = \case
  NoSuchSplit -> "no such split"
  NotProposed -> "the split is not proposed"
  NoSuchParent -> "no such parent"
  Cycle n -> "cycle: " <> n
  DuplicateLemma n -> "duplicate lemma " <> n
  LemmaFailed n -> "the lemma " <> n <> " already failed"

-- | Accept a proposed split with its lemmas, given as (name, key).
acceptSplit :: (NodeId -> Bool) -> SplitId -> [(Text, Text)] -> Graph -> Either Text (Graph, [Link])
acceptSplit canAlias sid lemmas = first acceptErrorText . acceptSplitWhy canAlias sid lemmas

-- | 'acceptSplit' with why it could not, as data, so a caller can say
-- where the lemma's name came from.
acceptSplitWhy :: (NodeId -> Bool) -> SplitId -> [(Text, Text)] -> Graph -> Either AcceptError (Graph, [Link])
acceptSplitWhy canAlias sid lemmas g0 = case IM.lookup sid (gSplits g0) of
  Nothing -> Left NoSuchSplit
  Just sp
    | sStatus sp /= SProposed -> Left NotProposed
    | otherwise -> case IM.lookup (sParent sp) (gNodes g0) of
        Nothing -> Left NoSuchParent
        Just parent -> do
          let forbidden = nKey parent : [ nKey a | i <- IS.toList (ancestors (nId parent) g0)
                                                 , Just a <- [IM.lookup i (gNodes g0)] ]
          (g1, links) <- go parent forbidden [] g0 [] lemmas
          let ids = map linkId links
              g2 = g1 { gSplits = IM.insert sid sp { sLemmas = ids, sStatus = SAccepted } (gSplits g1) }
          Right (g2, links)
  where
    go _ _ _ g acc [] = Right (g, reverse acc)
    go parent forbidden seen g acc ((name, key) : rest)
      | key `elem` forbidden = Left (Cycle name)
      | key `elem` seen = Left (DuplicateLemma name)
      | otherwise =
          let holders = [ n | n <- IM.elems (gNodes g), nKey n == key ]
              failed = [ () | n <- holders, NFailed _ <- [nStatus n] ]
              proved = [ nId n | n <- holders, nStatus n == NProved, canAlias (nId n) ]
              open = [ nId n | n <- holders, nStatus n == NOpen, canAlias (nId n) ]
          in if not (null failed) then Left (LemmaFailed name)
             else case (proved, open) of
               (n : _, _) -> go parent forbidden (key : seen) g (Reused n : acc) rest
               (_, n : _) -> go parent forbidden (key : seen) g (Shared n : acc) rest
               _ ->
                 let i = IM.size (gNodes g) + 1
                     node = Node i name key (nRoot parent) (nDepth parent + 1) NOpen [] 0 Nothing
                 in go parent forbidden (key : seen) g { gNodes = IM.insert i node (gNodes g) }
                       (NewNode i : acc) rest

linkId :: Link -> NodeId
linkId = \case
  NewNode i -> i
  Shared i -> i
  Reused i -> i

attemptDone :: NodeId -> Graph -> Graph
attemptDone nid g = g { gNodes = IM.adjust (\n -> n { nAttempts = nAttempts n + 1 }) nid (gNodes g) }

-- | Set the partial proof a node's next attempt starts from.
setPartial :: NodeId -> Maybe (Text, Text, Text) -> Graph -> Graph
setPartial nid p g = g { gNodes = IM.adjust (\n -> n { nPartial = p }) nid (gNodes g) }

markProved :: NodeId -> Graph -> Graph
markProved nid g = g { gNodes = IM.adjust (\n -> n { nStatus = NProved }) nid (gNodes g) }

markFailed :: NodeId -> Text -> Graph -> Graph
markFailed nid why g =
  let nm = maybe "" nName (IM.lookup nid (gNodes g))
      hit s = sStatus s == SAccepted && nid `elem` sLemmas s
  in g { gNodes = IM.adjust (\n -> n { nStatus = NFailed why }) nid (gNodes g)
       , gSplits = IM.map (\s -> if hit s then s { sStatus = SFailed ("lemma " <> nm <> " failed") } else s)
                          (gSplits g) }

-- | Accepted splits whose lemmas are all proved while the parent is still open.
doneSplits :: Graph -> [SplitId]
doneSplits g =
  [ sId s | s <- IM.elems (gSplits g), sStatus s == SAccepted
          , all provedN (sLemmas s)
          , maybe False ((== NOpen) . nStatus) (IM.lookup (sParent s) (gNodes g)) ]
  where provedN i = maybe False ((== NProved) . nStatus) (IM.lookup i (gNodes g))

-- | Covered by a live split: some split is accepted and none of its lemmas has failed.
covered :: Graph -> Node -> Bool
covered g n = any live [ s | i <- nSplits n, Just s <- [IM.lookup i (gSplits g)] ]
  where
    live s = sStatus s == SAccepted && not (any failedN (sLemmas s))
    failedN i = case IM.lookup i (gNodes g) of
      Just m | NFailed _ <- nStatus m -> True
      _ -> False

hasProposed :: Graph -> Node -> Bool
hasProposed g n = or [ sStatus s == SProposed | i <- nSplits n, Just s <- [IM.lookup i (gSplits g)] ]

nodeAction :: Limits -> Graph -> Node -> Maybe Action
nodeAction lim g n
  | nAttempts n < attemptsFor = Just (Attempt (nId n))
  | length (nSplits n) < lMaxSplits lim && nDepth n < lMaxDepth lim && not (hasProposed g n) =
      Just (Propose (nId n))
  | otherwise = Nothing
  where attemptsFor = if nDepth n >= lMaxDepth lim then 2 else 1

-- | The nodes the search still works for: the roots, and the lemmas of
-- accepted or done splits whose parent is live. A lemma of a failed or
-- rejected split, and everything below it, is not.
liveNodes :: Graph -> IntSet
liveNodes g = grow (IS.fromList (gRoots g))
  where
    holding = [ sp | sp <- IM.elems (gSplits g), sStatus sp == SAccepted || sStatus sp == SDone ]
    grow live =
      let more = IS.fromList [ l | sp <- holding, IS.member (sParent sp) live, l <- sLemmas sp ]
          next = IS.union live more
      in if next == live then live else grow next

-- | The lemmas of a root's current plan: the nodes reached from it through
-- accepted or done splits, the root left out, in id order.
planLemmas :: Graph -> NodeId -> [NodeId]
planLemmas g r = IS.toList (IS.delete r (reach (IS.singleton r) [r]))
  where
    holding = [ sp | sp <- IM.elems (gSplits g), sStatus sp == SAccepted || sStatus sp == SDone ]
    reach seen [] = seen
    reach seen (x : xs) =
      let new = IS.toList (IS.fromList [ l | sp <- holding, sParent sp == x, l <- sLemmas sp, not (IS.member l seen) ])
      in reach (IS.union seen (IS.fromList new)) (new ++ xs)

-- | Live, open, idle nodes with no live split covering them.
idle :: IntSet -> Graph -> [Node]
idle busy g = [ n | n <- IM.elems (gNodes g), nStatus n == NOpen, IS.member (nId n) live
                  , not (IS.member (nId n) busy), not (covered g n) ]
  where live = liveNodes g

-- | The next action, busy nodes excluded: the root with the fewest recorded actions
-- that has something to do, and inside it the deepest node, ties by lowest id.
nextAction :: Limits -> IntSet -> Graph -> Maybe Action
nextAction lim busy g =
  let cands = [ (n, a) | n <- idle busy g, Just a <- [nodeAction lim g n] ]
      actionsOf r = IM.findWithDefault 0 r (gActions g)
      rootsWith = [ (actionsOf r, pos, r) | (pos, r) <- zip [0 :: Int ..] (gRoots g)
                  , any ((== r) . nRoot . fst) cands ]
  in do
    (_, _, r) <- listToMaybe (sortOn (\(a, p, _) -> (a, p)) rootsWith)
    let inRoot = [ (n, a) | (n, a) <- cands, nRoot n == r ]
    (_, a) <- listToMaybe (sortOn (\(n, _) -> (negate (nDepth n), nId n)) inRoot)
    pure a

-- | Live, open, idle, uncovered nodes with nothing left to try and no proposed split pending.
stuck :: Limits -> IntSet -> Graph -> [NodeId]
stuck lim busy g =
  [ nId n | n <- idle busy g, Nothing <- [nodeAction lim g n], not (hasProposed g n) ]

recordAction :: NodeId -> Graph -> Graph
recordAction nid g = case IM.lookup nid (gNodes g) of
  Nothing -> g
  Just n -> g { gActions = IM.insertWith (+) (nRoot n) 1 (gActions g) }

-- | Every node above this one (through the splits that hold it), not the node itself.
ancestors :: NodeId -> Graph -> IntSet
ancestors nid g = walk IS.empty (parentsOf nid)
  where
    parentsOf i = [ sParent s | s <- IM.elems (gSplits g), i `elem` sLemmas s ]
    walk seen [] = seen
    walk seen (p : ps)
      | IS.member p seen = walk seen ps
      | otherwise = walk (IS.insert p seen) (parentsOf p ++ ps)
