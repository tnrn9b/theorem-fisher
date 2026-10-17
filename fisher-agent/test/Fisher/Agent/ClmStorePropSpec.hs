-- | The CLM store's lossless property (spec 3.2), over CLM's own batch
-- step: random batches of tool results (small, cut, spilled, reads of the
-- context file), random edits of the mirror (keep, delete or rewrite each
-- turn) and the rollbacks the budget forces, run through 'spillBatch',
-- 'afterBatch' and 'archiveBatch' as the loop runs them. After every batch,
-- every transcript message but the rollback notes is live in full or
-- stored, no transcript position is stored twice, and each item holds its
-- original unchanged; and a rollback note names every item holding an
-- original it dropped, and nothing else but what the note it replaced
-- named.
module Fisher.Agent.ClmStorePropSpec (spec) where

import qualified Data.ByteString as BS
import Data.List (nub)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Hspec
import Test.QuickCheck

import Fisher.Agent.Context.Clm
import Fisher.Agent.Context.ClmStore
import Fisher.Agent.Context.Mirror (Turn (..), render)
import Fisher.Py.Value

sid :: Text
sid = "20261014_090000_cd34ef"

path :: Text
path = "/ctx/20261014_090000_cd34ef/context.txt"

data Size = RSmall | RMedium | RLarge | RReadCtx
  deriving (Eq, Show)

data EditOp = Keep | Delete | Rewrite
  deriving (Eq, Show)

-- | One batch: its results' sizes, and the edit made to each block of
-- the mirror (cycled; 'Nothing' leaves the file as written).
data Batch = Batch [Size] (Maybe [EditOp])
  deriving (Show)

data Run = Run Integer Double [Batch]
  deriving (Show)

instance Arbitrary Run where
  arbitrary = do
    budget <- elements [800, 1500, 2500]
    share <- elements [0.25, 0.5, 1.0]
    n <- choose (1, 12)
    bs <- vectorOf n batch
    pure (Run budget share bs)
    where
      batch = do
        k <- choose (1, 3)
        sizes <- vectorOf k (frequency [(4, pure RSmall), (3, pure RMedium), (2, pure RLarge), (1, pure RReadCtx)])
        ed <- frequency [(2, pure Nothing), (3, Just <$> listOf1 (frequency [(5, pure Keep), (2, pure Delete), (2, pure Rewrite)]))]
        pure (Batch sizes ed)
  shrink (Run b s bs) = [Run b s bs' | bs' <- shrinkList (const []) bs, not (null bs')]

data Sim = Sim
  { smTr :: [PyVal]
  , smOrigin :: M.Map Int Int
  , smState :: ClmState
  , smStore :: Store
  , smNotes :: S.Set Int
  -- ^ transcript positions of rollback notes
  , smDone :: Bool
  , smBad :: [String]
  -- ^ rollback notes that named the wrong items
  }

textOf :: Int -> Int -> Text
textOf seed n = T.take n (T.intercalate "\n" [T.justifyLeft 49 '-' ("r" <> T.pack (show seed) <> " " <> T.pack (show k)) | k <- [1 .. n `div` 50 + 1]])

-- | Edit the rendered mirror: blocks are kept, dropped or given a new body.
editMirror :: [EditOp] -> Text -> Text
editMirror ops t =
  let ls = T.splitOn "\n" t
      (pre, rest) = break ("<<< turn " `T.isPrefixOf`) ls
      blocks = splitB rest
      splitB [] = []
      splitB (h : xs) = let (b, more) = break ("<<< turn " `T.isPrefixOf`) xs in (h : b) : splitB more
      one (op, blk) = case op of
        Keep -> blk
        Delete -> []
        Rewrite -> [head blk, "summary: this turn showed what mattered", ""]
   in T.intercalate "\n" (pre <> concatMap one (zip (cycle ops) blocks))

step :: ClmConfig -> Sim -> (Int, Batch) -> Sim
step cfg sm (bn, Batch sizes ed)
  | smDone sm = sm
  | otherwise =
      let st0 = shortenReads path (smState sm)
          mirror = render sid (stTurns st0)
          cid k = "c" <> T.pack (show bn) <> "_" <> T.pack (show k)
          callFor (k, sz) =
            let (name, args) = case sz of
                  RReadCtx -> ("read_file", "{\"path\": \"" <> path <> "\"}")
                  _ -> ("terminal", "{\"command\": \"step " <> T.pack (show bn) <> "\"}")
             in obj [("id", PStr (cid k)), ("type", PStr "function"), ("function", obj [("name", PStr name), ("arguments", PStr args)])]
          ks = zip [0 :: Int ..] sizes
          assistant = obj [("role", PStr "assistant"), ("content", PStr ("batch " <> T.pack (show bn))), ("tool_calls", PList (map callFor ks))]
          lim = fromIntegral (clmLimit cfg)
          resultOf (k, sz) = case sz of
            RSmall -> textOf (bn * 10 + k) 80
            RMedium -> textOf (bn * 10 + k) (lim + lim `div` 3)
            RLarge -> textOf (bn * 10 + k) (lim * 2)
            RReadCtx -> mirror
          tools = [obj [("role", PStr "tool"), ("tool_call_id", PStr (cid k)), ("content", PStr (resultOf (k, sz)))] | (k, sz) <- ks]
          p0 = length (smTr sm)
          tr1 = smTr sm <> (assistant : tools)
          (biT, spilled, store1, _) = spillBatch cfg path (p0 + 1) assistant [(m, txt m) | m <- tools] (smStore sm)
          file = Just (TE.encodeUtf8 (maybe mirror (`editMirror` mirror) ed))
          out0 =
            afterBatch
              BatchIn
                { biConfig = cfg, biPath = path, biSession = sid, biState = st0, biMirror = mirror
                , biFile = file :: Maybe BS.ByteString, biAssistant = assistant, biTools = biT, biTodo = Nothing
                , biSpilled = spilled }
          (out, origin', store2, _) = archiveBatch path tr1 (smOrigin sm) p0 (stTurns st0) out0 store1
          st' = boState out
          -- what the note must name
          droppedPos = S.fromList ([p0 .. length tr1 - 1] <> [p | i <- boDroppedTurns out0, Just p <- [M.lookup i (smOrigin sm)]])
          mustName = S.fromList [itId i | i <- sItems store2, Just p <- [itOrigin i], S.member p droppedPos]
          noteOf ts = [maybe "" pyStrOf (get "content" m) | Turn i m <- ts, Just i == stNoteId st']
          named = S.fromList (concatMap noteStoredIds (noteOf (stTurns st')))
          namedPrev = S.fromList (concatMap noteStoredIds (noteOf (stTurns st0)))
          bad
            | boOutcome out /= RolledBack = []
            | not (mustName `S.isSubsetOf` named) = ["batch " <> show bn <> ": the note names " <> show named <> ", not all of " <> show mustName]
            | not (named `S.isSubsetOf` S.union mustName namedPrev) = ["batch " <> show bn <> ": the note names " <> show named <> " beyond " <> show mustName <> " and " <> show namedPrev]
            | otherwise = []
          (tr2, notes) = case (boOutcome out, stNoteId st') of
            (RolledBack, Just nid) -> (tr1 <> [m | Turn i m <- stTurns st', i == nid], S.insert (length tr1) (smNotes sm))
            _ -> (tr1, smNotes sm)
       in Sim tr2 origin' st' store2 notes (case boOutcome out of Stopped _ -> True; _ -> False) (smBad sm <> bad)
  where
    txt m = maybe "" pyStrOf (get "content" m)

-- | What must hold after a batch: Nothing, or why not.
lossless :: Sim -> Maybe String
lossless sm
  | not (null (smBad sm)) = Just (unwords (smBad sm))
  | not (null missing) = Just ("not live and not stored: positions " <> show missing)
  | length origins /= length (nub origins) = Just ("stored twice: " <> show origins)
  | S.fromList origins /= sOrigins (smStore sm) = Just "sOrigins differs from the items' origins"
  | not (and [itMessage i == smTr sm !! p | i <- sItems (smStore sm), Just p <- [itOrigin i]]) = Just "an item differs from its original"
  | otherwise = Nothing
  where
    live = map turnMsg (stTurns (smState sm))
    origins = [p | i <- sItems (smStore sm), Just p <- [itOrigin i]]
    -- position 0 stands for the pinned task (never a turn)
    missing =
      [ p
      | (p, m) <- zip [0 :: Int ..] (smTr sm)
      , p > 0
      , not (S.member p (smNotes sm))
      , not (S.member p (sOrigins (smStore sm)))
      , not (liveIn path m live) ]

prop_lossless :: Run -> Property
prop_lossless (Run budget share bs) =
  let cfg = defaultClmConfig {ccBudget = budget, ccReserve = 0, ccMaxRollbacks = 2, ccStore = True, ccSpill = share, ccSkill = ""}
      task = obj [("role", PStr "user"), ("content", PStr "the task")]
      sim0 = Sim [task] M.empty newClmState emptyStore S.empty False []
      sims = scanl (step cfg) sim0 (zip [1 ..] bs)
      bad = [(k, e) | (k, s) <- zip [0 :: Int ..] sims, Just e <- [lossless s]]
      final = last sims
      items = sItems (smStore final)
   in classify (any ((== Spilled) . itKind) items) "spilled"
        . classify (any ((== Archived) . itKind) items) "archived"
        . classify (not (S.null (smNotes final))) "rolled back"
        . classify (smDone final) "stopped"
        $ counterexample (show bad) (null bad)

spec :: Spec
spec = describe "CLM store: lossless (5,000 runs)" $
  it "every original is live in full or stored, and stored once" $
    property (withMaxSuccess 5000 prop_lossless)
