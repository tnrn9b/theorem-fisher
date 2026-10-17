-- | The CLM context strategy (docs/clm.md): its configuration, the loop's
-- state, and the pure step that follows each batch of tool calls
-- (section 6 steps 3 to 6, with sections 7 and 8).
module Fisher.Agent.Context.Clm
  ( ClmConfig (..)
  , defaultClmConfig
  , defaultSkill
  , clmLimit
  , MirrorIO (..)
  , ClmEvent (..)
  , ClmSetup (..)
  , ClmState (..)
  , newClmState
  , syncTurns
  , shortenReads
  , CallResult (..)
  , classifyCall
  , BatchIn (..)
  , BatchOut (..)
  , BatchOutcome (..)
  , afterBatch
  , spillBatch
  , archiveBatch
  , archiveOverflow
  , overflowRollback
  , stateToPy
  , stateFromPy
  , clmConfigFromPy
  , clmConfigToPy
  ) where

import qualified Data.ByteString as BS
import Data.List (find)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Agent.Context.Budget
import Fisher.Agent.Context.ClmStore
import Fisher.Agent.Context.EditGate
import Fisher.Agent.Context.Mirror
import Fisher.Agent.Context.Recall (RecallResult)
import Fisher.Py.Value
import Fisher.RLM.Config (RlmConfig)
import Fisher.RLM.Repl (ReplFactory)

data ClmConfig = ClmConfig
  { ccBudget :: Integer
  , ccReserve :: Integer
  , ccGate :: GateMode
  , ccMaxRollbacks :: Int
  , ccKeepUnread :: Bool
  -- ^ rule 3 of section 5: an edit keeps turns the model has not read in
  -- the file; off (the default), the file is taken as written, since every
  -- editable turn was in the prompt the model wrote it from
  , ccSkill :: Text
  -- ^ guidance appended to the prompt section: 'defaultSkill' unless
  -- @fisher.clm.skill@ names a file (its text) or is @none@ (empty)
  , ccStore :: Bool
  -- ^ @fisher.clm.store@: keep spilled results and archived originals in a store
  , ccSpill :: Double
  -- ^ @fisher.clm.spill@, in (0, 1]: a result over this share of the limit is spilled
  , ccStorePrompt :: Bool
  -- ^ @fisher.clm.store_prompt@: with the store on, the system prompt gets
  -- the store's paragraph; only @false@ turns it off (for ablations)
  }
  deriving (Eq, Show)

defaultClmConfig :: ClmConfig
defaultClmConfig = ClmConfig 32000 2048 Fit 50 False defaultSkill False 0.25 True

-- | How to work under the budget, appended to the prompt section; the text
-- of tools/clm/skill.md (a test keeps the two equal).
defaultSkill :: Text
defaultSkill =
  "How to keep the context file small while you work:" <> "\n"
    <> "" <> "\n"
    <> "- Read the whole output of every build or check: do not cut it with head or tail, which can hide the error that matters." <> "\n"
    <> "- Do not search or grep the context file; you can see all of it already. Read it only to compact it." <> "\n"
    <> "- Compact before the budget is nearly full, in one edit: keep your last few turns as they are, and replace each older turn's tool output with one line that says what it showed (for example \"built: 2 errors in foo_bar, line 40\" or \"read Foo.lean lines 1 to 200\"). Keep any text you may reuse, such as a proof that almost worked." <> "\n"
    <> "- If the context is too full to read the file, write it afresh with write_file: a few lines on what is done, what is left and what you are doing now." <> "\n"
    <> "- Keep track of the work with the todo tool, which survives compaction: what is done, what is left, and what you tried on each part that failed. Update it each time you compact." <> "\n"
    <> "- After compacting, go straight back to the task."

clmLimit :: ClmConfig -> Integer
clmLimit c = ccBudget c - ccReserve c

-- | Where the mirror and its backup live, as the loop reaches them.
data MirrorIO = MirrorIO
  { mioPath :: Text
  -- ^ the path the model is given
  , mioWrite :: Text -> IO ()
  , mioRead :: IO (Maybe BS.ByteString)
  , mioWriteBackup :: Text -> IO ()
  }

-- | What the loop reports as it goes (the scenario harness, and the
-- replay log in CLM3).
data ClmEvent
  = EvCall Int PyVal [PyVal]
  -- ^ a model call: its number (0 for the summary request), the pinned
  -- task, the messages after it
  | EvMirrorWrite Text
  | EvMirrorRead (Maybe BS.ByteString)
  | EvBatch [(Text, PyVal)]
  -- ^ the flags and the context line of a batch

data ClmSetup = ClmSetup
  { csConfig :: ClmConfig
  , csIO :: MirrorIO
  , csObserve :: ClmEvent -> IO ()
  , csSessionId :: Text
  , csLoad :: IO (Maybe ClmState)
  -- ^ @state.json@, when a session resumes
  , csSave :: ClmState -> IO ()
  -- ^ written after every batch and every run
  , csStoreLoad :: IO (Maybe Store)
  -- ^ @store.json@, when a session resumes with the store on
  , csStoreSave :: Store -> IO ()
  -- ^ written after every batch, with the store on
  , csOnSpill :: Item -> IO ()
  -- ^ each spilled result, after its batch, with itTurn the CLM turn its
  -- preview is (none when the batch rolled back) (the replay log)
  , csOnArchive :: [Item] -> Text -> IO ()
  -- ^ each batch's archived items, by why: deleted, rewritten, shortened,
  -- cut or rolled_back (the replay log)
  , csRecall :: Maybe (RlmConfig, ReplFactory)
  -- ^ with the store on, recall's configuration and REPL (spec 3.3);
  -- 'Nothing': recall is neither offered nor accepted
  , csOnRecall :: Text -> RecallResult -> IO ()
  -- ^ each recall under the store: its question and result (the replay log)
  }

data ClmState = ClmState
  { stTurns :: [Turn]
  , stCounter :: Int
  , stArmed :: S.Set Int
  , stFlushed :: Bool
  , stShortened :: S.Set Int
  , stSeen :: [Int]
  , stNoteId :: Maybe Int
  , stRollbacks :: Int
  , stIters :: Int
  , stLastMirror :: Maybe Text
  -- ^ the mirror text last written
  }
  deriving (Eq, Show)

newClmState :: ClmState
newClmState = ClmState [] 1 (S.fromList [25, 50, 75]) False S.empty [] Nothing 0 0 Nothing

-- | @state.json@ (section 9).
stateToPy :: ClmState -> PyVal
stateToPy st =
  PDict
    [ ("version", PInt 1)
    , ("turns", PList [PDict [("id", PInt (fromIntegral i)), ("message", m)] | Turn i m <- stTurns st])
    , ("counter", PInt (fromIntegral (stCounter st)))
    , ("armed", ints (S.toList (stArmed st)))
    , ("memory_flushed", PBool (stFlushed st))
    , ("shortened_reads", ints (S.toList (stShortened st)))
    , ("seen", ints (stSeen st))
    , ("note_id", maybe PNull (PInt . fromIntegral) (stNoteId st))
    , ("rollbacks", PInt (fromIntegral (stRollbacks st)))
    , ("mirror", maybe PNull PStr (stLastMirror st)) ]
  where
    ints = PList . map (PInt . fromIntegral)

stateFromPy :: PyVal -> Maybe ClmState
stateFromPy v = do
  PList ts <- get "turns" v
  turns <- mapM (\t -> Turn <$> int' t "id" <*> get "message" t) ts
  counter <- int' v "counter"
  pure
    newClmState
      { stTurns = turns, stCounter = counter
      , stArmed = maybe (stArmed newClmState) S.fromList (ints "armed")
      , stFlushed = get "memory_flushed" v == Just (PBool True)
      , stShortened = maybe S.empty S.fromList (ints "shortened_reads")
      , stSeen = maybe [] id (ints "seen")
      , stNoteId = int' v "note_id"
      , stRollbacks = maybe 0 id (int' v "rollbacks")
      , stLastMirror = case get "mirror" v of Just (PStr t) -> Just t; _ -> Nothing }
  where
    int' d k = case get k d of
      Just (PInt n) -> Just (fromIntegral n)
      _ -> Nothing
    ints k = case get k v of
      Just (PList xs) -> Just [fromIntegral n | PInt n <- xs]
      _ -> Nothing

-- | The @fisher.clm@ section of config.yaml (section 11).
clmConfigFromPy :: Maybe PyVal -> ClmConfig
clmConfigFromPy mv =
  let v = maybe (PDict []) id mv
      num k d = case get k v of
        Just (PInt n) | n > 0 -> n
        _ -> d
   in ClmConfig
        { ccBudget = num "budget" (ccBudget defaultClmConfig)
        , ccReserve = case get "reserve" v of
            Just (PInt n) | n >= 0 -> n
            _ -> ccReserve defaultClmConfig
        , ccGate = if get "gate" v == Just (PStr "shrink") then Shrink else Fit
        , ccMaxRollbacks = fromIntegral (num "max_rollbacks" (fromIntegral (ccMaxRollbacks defaultClmConfig)))
        , ccKeepUnread = get "keep_unread" v == Just (PBool True)
        , ccSkill = case get "skill_text" v of
            Just (PStr t) -> t
            _ -> defaultSkill
        , ccStore = get "store" v == Just (PBool True)
        , ccSpill = case get "spill" v of
            Just (PFloat d) | d > 0 && d <= 1 -> d
            Just (PInt 1) -> 1
            _ -> ccSpill defaultClmConfig
        , ccStorePrompt = get "store_prompt" v /= Just (PBool False) }

clmConfigToPy :: ClmConfig -> PyVal
clmConfigToPy c =
  PDict $
    [ ("budget", PInt (ccBudget c)), ("reserve", PInt (ccReserve c))
    , ("gate", PStr (if ccGate c == Shrink then "shrink" else "fit"))
    , ("max_rollbacks", PInt (fromIntegral (ccMaxRollbacks c))) ]
      <> [("keep_unread", PBool True) | ccKeepUnread c]
      <> [("skill_text", PStr (ccSkill c)) | ccSkill c /= defaultSkill]
      -- with the store on both are logged; with it off neither is, and the
      -- record is as before the store (spill is used only with the store)
      <> [("store", PBool True) | ccStore c]
      <> [("spill", PFloat (ccSpill c)) | ccStore c]
      <> [("store_prompt", PBool False) | ccStore c, not (ccStorePrompt c)]

-- | A provider's over-long-request error, handled as a rollback (section
-- 8): the newest turn goes, and the rollback note says so. Nothing when
-- there is no turn left to drop, or the rollbacks have run out.
overflowRollback :: ClmConfig -> Text -> Maybe Text -> ClmState -> Either Text ClmState
overflowRollback cfg path todo st
  | stRollbacks st >= ccMaxRollbacks cfg = Left (budgetError (ccMaxRollbacks cfg) (clmLimit cfg))
  | otherwise =
      let (ts0, oldNote) = case (reverse (stTurns st), stNoteId st) of
            (Turn i _ : rest, Just n) | i == n -> (reverse rest, Just n)
            _ -> (stTurns st, Nothing)
          msgs = map turnMsg ts0
       in case reverse (units msgs) of
            [] -> Left "Context length exceeded and there is no turn left to drop."
            ((a, b) : _) ->
              let dropped = [take (b - a) (drop a msgs)]
                  kept = take a ts0
                  note = PDict [("role", PStr "user"), ("content", PStr (rollbackNote dropped path todo))]
                  nid = maybe (stCounter st) id oldNote
               in Right
                    st
                      { stTurns = kept <> [Turn nid note], stCounter = maybe (stCounter st + 1) (const (stCounter st)) oldNote, stNoteId = Just nid
                      , stRollbacks = stRollbacks st + 1 }

-- | The turns for the messages after the pinned prefix: those that match
-- the turns already held keep their ids, the rest take fresh ones.
syncTurns :: [PyVal] -> ClmState -> ClmState
syncTurns msgs st =
  let matched = length (takeWhile id (zipWith (\(Turn _ m) x -> m == x) (stTurns st) msgs))
      kept = take matched (stTurns st)
      rest = drop matched msgs
      fresh = zipWith Turn [stCounter st ..] rest
   in st {stTurns = kept <> fresh, stCounter = stCounter st + length rest}

-- | Step 1: earlier reads of the mirror or the backup keep only their length.
shortenReads :: Text -> ClmState -> ClmState
shortenReads path st =
  let readIds =
        S.fromList
          [ i
          | Turn _ m <- stTurns st
          , get "role" m == Just (PStr "assistant")
          , c <- toolCallsOf m
          , (get "function" c >>= get "name") == Just (PStr "read_file")
          , Just p <- [pathArg c]
          , any (samePath p) [path, prevPath path]
          , Just i <- [pyStrOf <$> get "id" c] ]
      go (Turn i m)
        | get "role" m == Just (PStr "tool")
        , Just c <- pyStrOf <$> get "tool_call_id" m
        , S.member c readIds
        , not (S.member i (stShortened st)) =
            (Turn i (set "content" (PStr (readStub (maybe "" pyStrOf (get "content" m)))) m), [i])
        | otherwise = (Turn i m, [])
      (ts, done) = unzip (map go (stTurns st))
   in st {stTurns = ts, stShortened = S.union (stShortened st) (S.fromList (concat done))}

-- | One tool result, as the batch step needs to know it.
data CallResult = CallResult
  { crMirrorRead :: Bool
  -- ^ a successful read of the mirror
  , crPrevRead :: Bool
  -- ^ a successful read of the backup
  , crContext :: Bool
  -- ^ a successful context-only call
  }

-- | A call (from the assistant message) and its result text.
classifyCall :: Text -> PyVal -> Text -> CallResult
classifyCall path c res =
  let name = maybe "" pyStrOf (get "function" c >>= get "name")
      p = pathArg c
      err = isErrorResult res
      is q = maybe False (samePath q) p
      mirrorRead = name == "read_file" && is path
      prevRead = name == "read_file" && is (prevPath path)
      shell = name `elem` ["terminal", "execute_code"] && maybe False (mentionsContext path) (cmdArg c)
      ctxCall = (name `elem` ["read_file", "write_file", "patch"] && is path) || prevRead || shell
   in CallResult (mirrorRead && not err) (prevRead && not err) (ctxCall && not err)

data BatchIn = BatchIn
  { biConfig :: ClmConfig
  , biPath :: Text
  , biSession :: Text
  , biState :: ClmState
  -- ^ after step 1 (reads shortened), with what this batch's reads saw
  , biMirror :: Text
  -- ^ the mirror as written at step 1
  , biFile :: Maybe BS.ByteString
  -- ^ the mirror as read back
  , biAssistant :: PyVal
  , biTools :: [(PyVal, Text)]
  -- ^ each tool message with the result's classification source: the
  -- call it answers is found by id in the assistant message
  , biTodo :: Maybe Text
  , biSpilled :: S.Set Int
  -- ^ indices into 'biTools' of results already spilled (their previews
  -- are not cut again)
  }

data BatchOutcome = Continue | RolledBack | Stopped Text
  deriving (Eq, Show)

data BatchOut = BatchOut
  { boState :: ClmState
  , boOutcome :: BatchOutcome
  , boFlags :: [(Text, PyVal)]
  , boBackup :: Maybe Text
  -- ^ write the backup with this text
  , boFlush :: Bool
  -- ^ run the memory flush
  , boDropped :: [[PyVal]]
  -- ^ the units a rollback dropped (also when the rollbacks ran out and
  -- the session stops); empty otherwise
  , boDroppedTurns :: [Int]
  -- ^ the ids of the context turns among them
  }

-- | Steps 3 to 6 of section 6, after the tools ran.
afterBatch :: BatchIn -> BatchOut
afterBatch bi =
  let cfg = biConfig bi
      limit = clmLimit cfg
      path = biPath bi
      st0 = biState bi
      calls = toolCallsOf (biAssistant bi)
      callFor m = case [c | c <- calls, get "id" c == get "tool_call_id" m] of
        (c : _) -> Just c
        [] -> Nothing
      content m = maybe "" pyStrOf (get "content" m)
      -- step 2's cut, for every result but a successful read of the mirror or the backup
      classified =
        [ (m', cr {crContext = crContext cr && not (isErrorResult final)})
        | (k, (m, res)) <- zip [0 :: Int ..] (biTools bi)
        , let cr0 = maybe (CallResult False False False) (\c -> classifyCall path c res) (callFor m)
              isRead = crMirrorRead cr0 || crPrevRead cr0
              keep = isRead || S.member k (biSpilled bi)
              final = if keep then res else cutResult limit res
              m' = if keep then m else set "content" (PStr final) m
              cr = cr0 {crContext = maybe False (\c -> ctxCallOf path c) (callFor m)} ]
      toolMsgs = map fst classified
      free = not (null classified) && all (crContext . snd) classified
      liveReads = [k | (k, (_, cr)) <- zip [0 :: Int ..] classified, crMirrorRead cr || crPrevRead cr]
      assistant0 = shortenWrites path (biAssistant bi)
      acctLive a = a : [if k `elem` liveReads then set "content" (PStr (readStub (content m))) m else m | (k, m) <- zip [0 ..] toolMsgs]
      -- step 3
      turns0 = stTurns st0
      -- rule 3 off: every turn counts as read, so the file is taken as written
      seen0 = if ccKeepUnread cfg then stSeen st0 else map turnId turns0
      rb = readBack (biSession bi) turns0 (stCounter st0) seen0 (biFile bi)
      before = map turnMsg turns0
      (st1, parts1, editFlag, mergeLive, backup) = case rb of
        NotUtf8 -> (st0, [receiptNotUtf8], Nothing, False, Nothing)
        Edited e ->
          let after = map turnMsg (editTurns e)
           in if accept (ccGate cfg) limit before after (acctLive assistant0)
                then
                  ( st0 {stTurns = editTurns e, stCounter = editNext e, stSeen = S.toList (S.union (S.fromList (stSeen st0)) (S.fromList (editWritten e)))}
                  , [receiptApplied (length before) (length after) (tokens before) (tokens after) (editKept e)]
                  , Just "applied"
                  , editMergeIntoLive e
                  , Just (biMirror bi) )
                else
                  ( st0
                  , [ if ccGate cfg == Shrink
                        then receiptRejectedShrink (tokens before) (tokens after)
                        else receiptRejectedFit (tokens (after <> acctLive assistant0)) limit ]
                  , Just "rejected"
                  , False
                  , Nothing )
        NoEdit k | k > 0 -> (st0, [receiptOnlyKept k], Nothing, False, Nothing)
        _ -> (st0, [], Nothing, False, Nothing)
      editF = [("edit", PStr e) | Just e <- [editFlag]]
      ctxMsgs = map turnMsg (stTurns st1)
      combined = ctxMsgs <> acctLive assistant0
      t = tokens combined
      us = units combined
   in if t > limit
        then
          let keepN = shrinkKeep (length us - 1)
              shrinkKeep k
                | k > 0 && tokens (take (snd (us !! (k - 1))) combined) > limit = shrinkKeep (k - 1)
                | otherwise = k
              dropped = [take (b - a) (drop a combined) | (a, b) <- drop keepN us]
              keptCtx = if keepN > 0 then take (snd (us !! (keepN - 1))) (stTurns st1) else []
              droppedTurns = map turnId (drop (length keptCtx) (stTurns st1))
           in if stRollbacks st1 == ccMaxRollbacks cfg
                then
                  BatchOut
                    (st1 {stTurns = keptCtx})
                    (Stopped (budgetError (ccMaxRollbacks cfg) limit))
                    (editF <> [("stop", PBool True)])
                    backup
                    False
                    dropped
                    droppedTurns
                else
                  let note = PDict [("role", PStr "user"), ("content", PStr (rollbackNote dropped path (biTodo bi)))]
                      (ts, counter, nid, replaced) = case (reverse keptCtx, stNoteId st1) of
                        (Turn i _ : rest, Just n) | i == n -> (reverse rest <> [Turn n note], stCounter st1, n, True)
                        _ -> (keptCtx <> [Turn (stCounter st1) note], stCounter st1 + 1, stCounter st1, False)
                   in BatchOut
                        st1 {stTurns = ts, stCounter = counter, stNoteId = Just nid, stRollbacks = stRollbacks st1 + 1, stIters = stIters st1 - 1}
                        RolledBack
                        (editF <> [("note_replaced", PBool True) | replaced] <> [("rollback", PInt (fromIntegral (length dropped)))])
                        backup
                        False
                        dropped
                        droppedTurns
        else
          let (turns2, assistant1, moved) =
                if mergeLive
                  then case reverse (stTurns st1) of
                    (Turn _ a : rest) -> (reverse rest, into (contentText (get "content" a)) assistant0, True)
                    [] -> (stTurns st1, assistant0, False)
                  else (stTurns st1, assistant0, False)
              ths = thresholds limit
              crossed = [p | (p, th) <- ths, t >= th, S.member p (stArmed st1)]
              rearmed = S.fromList [p | (p, th) <- ths, t < th]
              armed = (S.union (stArmed st1) rearmed) `S.difference` S.fromList crossed
              fired = if null crossed then Nothing else Just (maximum crossed)
              assistants = [m | m <- combined, get "role" m == Just (PStr "assistant")]
              q = maximum (0 : map (\m -> tokens [m]) (lastN 5 assistants))
              urgent = t >= limit * 50 `div` 100 && (limit - t) < max (limit `div` 10) (2 * q)
              nudgePart
                | urgent = [urgentText]
                | Just p <- fired = [nudgeText p]
                | otherwise = []
              nudgeF
                | urgent = [("nudge", PStr "urgent")]
                | Just p <- fired = [("nudge", PInt (fromIntegral p))]
                | otherwise = []
              flush = fired == Just 75 && not (stFlushed st1)
              line = "[" <> T.intercalate "; " (parts1 <> [readout t limit] <> nudgePart) <> "]"
              toolMsgs' = case reverse toolMsgs of
                (l : rest) -> reverse (set "content" (PStr (content l <> "\n\n" <> line)) l : rest)
                [] -> []
              live = assistant1 : toolMsgs'
              n0 = stCounter st1
              ts = turns2 <> zipWith Turn [n0 ..] live
              st2 =
                st1
                  { stTurns = ts, stCounter = n0 + length live, stArmed = armed, stRollbacks = 0
                  , stFlushed = stFlushed st1 || flush
                  , stIters = if free then stIters st1 - 1 else stIters st1 }
           in BatchOut
                st2
                Continue
                ( editF <> [("moved_into_live", PBool True) | moved] <> [("free", PBool True) | free] <> nudgeF
                    <> [("memory_flush", PBool True) | flush] <> [("context_line", PStr line)] )
                backup
                flush
                []
                []
  where
    lastN n xs = drop (length xs - n) xs

-- | Spill a batch's tool results with the store on (spec 3.1): the context
-- file's path, the transcript position of the first tool message, the
-- assistant message (each result's call is found by its id), each tool
-- message with its result text, and the store. Gives 'biTools', 'biSpilled',
-- the store and the new items.
spillBatch :: ClmConfig -> Text -> Int -> PyVal -> [(PyVal, Text)] -> Store -> ([(PyVal, Text)], S.Set Int, Store, [Item])
spillBatch cfg path first assistant tools store =
  let calls = toolCallsOf assistant
      callFor m = find (\c -> get "id" c == get "tool_call_id" m) calls
      ins = [(m, callFor m, res, Just (first + k)) | (k, (m, res)) <- zip [0 ..] tools]
      (outs, store', items) = spill (clmLimit cfg) (ccSpill cfg) path first store ins
      spilled = S.fromList [p - first | Item {itOrigin = Just p} <- items]
   in (outs, spilled, store', items)

-- | After 'afterBatch' with the store on (spec 3.2): record the transcript
-- position of each new live turn in the origin map, archive what is no
-- longer live in full, and end a rollback note with the items that hold
-- the originals it dropped (and, when it replaced the note before it, what
-- that note named). Arguments: the context file's path, the transcript
-- (ending with this batch's messages, the assistant's at the given
-- position), the origin map, the turns before the batch (after step 1),
-- the batch's outcome and the store.
archiveBatch :: Text -> [PyVal] -> M.Map Int Int -> Int -> [Turn] -> BatchOut -> Store -> (BatchOut, M.Map Int Int, Store, [(Item, Text)])
archiveBatch path tr origin p0 before out store =
  let st = boState out
      ts = stTurns st
      nLive = length tr - p0
      origin'
        | boOutcome out == Continue = M.union (M.fromList (zip (map turnId (drop (length ts - nLive) ts)) [p0 ..])) origin
        | otherwise = origin
      (store', items, named) = archiveStep path tr origin' before ts (boDropped out) (S.fromList (boDroppedTurns out)) p0 store
      st'
        | boOutcome out == RolledBack = endNote (storedRangeNote (named <> namedBefore before st)) st
        | otherwise = st
   in (out {boState = st'}, origin', store', items)

-- | The same for an overflow rollback (section 8): the path, the
-- transcript, the origin map, the state before it and after it, and the
-- store.
archiveOverflow :: Text -> [PyVal] -> M.Map Int Int -> ClmState -> ClmState -> Store -> (ClmState, Store, [(Item, Text)])
archiveOverflow path tr origin before after store =
  let kept = S.fromList (map turnId (stTurns after))
      gone = [t | t@(Turn i _) <- stTurns before, not (S.member i kept)]
      (store', items, named) = archiveStep path tr origin (stTurns before) (stTurns after) [map turnMsg gone] (S.fromList (map turnId gone)) (length tr) store
   in (endNote (storedRangeNote (named <> namedBefore (stTurns before) after)) after, store', items)

-- | What the note a new rollback note replaced (same id) named.
namedBefore :: [Turn] -> ClmState -> [Int]
namedBefore before st = concat [noteStoredIds (contentText (get "content" m)) | Turn i m <- before, Just i == stNoteId st]

-- | Append a text to the rollback note's content.
endNote :: Text -> ClmState -> ClmState
endNote t st
  | T.null t = st
  | otherwise =
      st {stTurns = [if Just i == stNoteId st then Turn i (set "content" (PStr (contentText (get "content" m) <> t)) m) else Turn i m | Turn i m <- stTurns st]}

-- | A call to the file tools on the mirror, or a read of the backup.
ctxCallOf :: Text -> PyVal -> Bool
ctxCallOf path c =
  let name = maybe "" pyStrOf (get "function" c >>= get "name")
      p = pathArg c
      is q = maybe False (samePath q) p
   in (name `elem` ["read_file", "write_file", "patch"] && is path)
        || (name == "read_file" && is (prevPath path))
        || (name `elem` ["terminal", "execute_code"] && maybe False (mentionsContext path) (cmdArg c))
