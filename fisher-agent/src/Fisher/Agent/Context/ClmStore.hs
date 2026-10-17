-- | CLM with a store (docs/superpowers/specs/2026-10-14-clm-store-design.md),
-- pure part: the store of items (spilled oversized tool results, archived
-- originals), the spill step, the exact texts, and read_stored's slices.
--
-- This module does not import "Fisher.Agent.Context.Clm" (which imports it),
-- so 'spill' takes the limit and share, not the config.
module Fisher.Agent.Context.ClmStore
  ( ItemKind (..)
  , Item (..)
  , Store (..)
  , emptyStore
  , itemName
  , renderItem
  , itemBody
  , spill
  , archive
  , archiveWhy
  , archiveStep
  , archiveRewritten
  , liveInFull
  , liveIn
  , handleLine
  , previewOf
  , readStored
  , storedRangeNote
  , noteStoredIds
  , storePromptSection
  , readStoredSchema
  , storeToPy
  , storeFromPy
  ) where

import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

import Fisher.Agent.Context.Budget (pathArg, prevPath, samePath, shortenWrites)
import Fisher.Agent.Context.Mirror (Turn (..), toolCallsOf, tokens)
import Fisher.Agent.Context.Rlm (StoredTurn (..), emptyStoreText, renderTurn)
import Fisher.Agent.Types (commas)
import Fisher.Py.Value

data ItemKind = Spilled | Archived
  deriving (Eq, Show)

data Item = Item
  { itId :: Int
  -- ^ shown as @S<n>@, from 1, in order
  , itKind :: ItemKind
  , itTurn :: Maybe Int
  -- ^ the CLM turn id it came from, when it had one
  , itTool :: Maybe Text
  -- ^ the tool name, for a tool result
  , itOrigin :: Maybe Int
  -- ^ the transcript position (0-based) of the original, when known
  , itMessage :: PyVal
  -- ^ the original message, unchanged
  }
  deriving (Eq, Show)

data Store = Store
  { sItems :: [Item]
  , sNext :: Int
  -- ^ the id the next item gets
  , sOrigins :: S.Set Int
  -- ^ transcript positions already stored (no double storage)
  }
  deriving (Eq, Show)

emptyStore :: Store
emptyStore = Store [] 1 S.empty

itemName :: Int -> Text
itemName n = "S" <> T.pack (show n)

-- ------------------------------------------------------------------ rendering

roleOf :: PyVal -> Text
roleOf m = maybe "" pyStrOf (get "role" m)

-- | The item as recall sees it: @[S7, tool NAME]@ or @[S7, assistant]@, then
-- the text, formatted as phase 2's 'renderTurn' does.
renderItem :: Item -> Text
renderItem it =
  let hdr = case roleOf (itMessage it) of
        "tool" -> "[" <> itemName (itId it) <> ", tool " <> fromMaybe "unknown" (itTool it) <> "]"
        r -> "[" <> itemName (itId it) <> ", " <> r <> "]"
      body = itemBody it
   in if T.null body then hdr else hdr <> "\n" <> body

-- | The item's text without its header line.
itemBody :: Item -> Text
itemBody it =
  let rendered = renderTurn (StoredTurn 0 (roleOf m) (itTool it) m)
      m = itMessage it
   in T.drop 1 (T.dropWhile (/= '\n') rendered)

-- ------------------------------------------------------------------ token fit

-- | The longest prefix of the lines whose joined text is within the cap
-- (CLM's estimate); possibly empty.
fitLines :: Integer -> [Text] -> [Text]
fitLines cap ls = adjust (guess 0 0 ls)
  where
    n = length ls
    fits k = tokens [PStr (T.intercalate "\n" (take k ls))] <= cap
    -- a running estimate (repr length is additive per line, up to quoting)
    guess :: Int -> Integer -> [Text] -> Int
    guess k _ [] = k
    guess k acc (l : rest) =
      let acc' = acc + fromIntegral (T.length (pyRepr (PStr l))) - 2 + 2
       in if (acc' + 2) `div` 4 > cap then k else guess (k + 1) acc' rest
    adjust k0 = take (up (down k0)) ls
    down k = if k > 0 && not (fits k) then down (k - 1) else k
    up k = if k < n && fits (k + 1) then up (k + 1) else k

-- | The first lines of the full text that fit in 10% of the limit.
previewOf :: Integer -> Text -> Text
previewOf limit full = T.intercalate "\n" (fitLines (limit `div` 10) (wrapLines full))

-- | The lines of a text as read_stored numbers them: any line over 2,000
-- characters is split into consecutive pieces of at most 2,000.
wrapLines :: Text -> [Text]
wrapLines = concatMap piece . T.lines
  where
    piece l = if T.length l <= 2000 then [l] else T.chunksOf 2000 l

-- | @[stored as S7: 18,400 characters, 412 lines; read_stored("S7", from_line, to_line) or recall(question)]@
handleLine :: Item -> Text
handleLine it =
  let body = itemBody it
      nm = itemName (itId it)
   in "[stored as " <> nm <> ": " <> commas (fromIntegral (T.length body)) <> " characters, "
        <> commas (fromIntegral (length (wrapLines body))) <> " lines; read_stored(\"" <> nm
        <> "\", from_line, to_line) or recall(question)]"

-- ------------------------------------------------------------------ spill

-- | Spill the oversized results of one batch's tool messages.
--
-- Arguments: CLM's limit, the spill share, the context file's path, the first
-- transcript position of the batch's tool messages (used for a message whose
-- own position is 'Nothing': first position plus its index), the store, and
-- per tool message: the message, the call it answers (from the assistant
-- message; gives the tool name and tells a read of the context file or its
-- backup, which is never spilled), the result text and its transcript
-- position. Returns each message with its result text (a preview and the
-- handle line when spilled), the new store and the new items.
--
-- A result whose position is already in 'sOrigins' is left as it is: it is
-- stored, and not stored again. Results of read_stored and recall are never
-- spilled: they come from the store already.
spill ::
  Integer ->
  Double ->
  Text ->
  Int ->
  Store ->
  [(PyVal, Maybe PyVal, Text, Maybe Int)] ->
  ([(PyVal, Text)], Store, [Item])
spill limit share path first st0 = go st0 [] [] . zip [0 :: Int ..]
  where
    threshold = share * fromIntegral limit
    go st outs news [] = (reverse outs, st, reverse news)
    go st outs news ((k, (m, call, res, pos)) : rest) =
      let position = fromMaybe (first + k) pos
          over = fromIntegral (tokens [m]) > threshold
          skip = maybe False readsContext call || S.member position (sOrigins st) || fromStore (toolName m call)
       in if not over || skip
            then go st ((m, res) : outs) news rest
            else
              let it =
                    Item
                      { itId = sNext st
                      , itKind = Spilled
                      , itTurn = Nothing
                      , itTool = toolName m call
                      , itOrigin = Just position
                      , itMessage = m }
                  preview = previewOf limit res
                  shown = if T.null preview then handleLine' else preview <> "\n" <> handleLine'
                  handleLine' = handleFor it res
                  st' = st {sItems = sItems st <> [it], sNext = sNext st + 1, sOrigins = S.insert position (sOrigins st)}
               in go st' ((set "content" (PStr shown) m, shown) : outs) (it : news) rest
    handleFor it res = handleLine it {itMessage = set "content" (PStr res) (itMessage it)}
    fromStore = (`elem` [Just "read_stored", Just "recall"])
    readsContext c =
      let name = maybe "" pyStrOf (get "function" c >>= get "name")
          is q = maybe False (samePath q) (pathArg c)
       in name == "read_file" && (is path || is (prevPath path))
    toolName m call = case (call >>= \c -> get "function" c >>= get "name", get "name" m) of
      (Just n, _) -> Just (pyStrOf n)
      (_, Just n@(PStr _)) -> Just (pyStrOf n)
      _ -> Nothing

-- ------------------------------------------------------------------ archive

-- | Whether a live message holds an original in full (spec 3.2): equal, or
-- equal but for the context line CLM appends to a batch's last result (a
-- blank line, then one bracketed line), or equal but for 'shortenWrites' of
-- the model's own writes of the context file at the given path (that text
-- is the model's, about the context, and is not kept).
liveInFull :: Text -> PyVal -> PyVal -> Bool
liveInFull path orig live = liveIn path orig [live]

-- | Whether some live message holds an original in full ('liveInFull').
liveIn :: Text -> PyVal -> [PyVal] -> Bool
liveIn path orig lives = any (\l -> l `elem` forms || contextLine l) lives
  where
    forms = orig : [f | roleOf orig == "assistant", let f = shortenWrites path orig, f /= orig]
    contextLine live = case (roleOf orig, get "content" orig, get "content" live) of
      ("tool", Just (PStr co), Just (PStr cl))
        | Just rest <- T.stripPrefix (co <> "\n\n[") cl ->
            "]" `T.isSuffixOf` rest && not (T.any (== '\n') rest) && set "content" (PStr co) live == orig
      _ -> False

-- | 'archiveWhy' without the reasons.
archive :: Text -> [PyVal] -> M.Map Int Int -> [Turn] -> [Turn] -> [[PyVal]] -> Int -> Store -> (Store, [Item])
archive path tr origin before after dropped start st =
  let (st', its) = archiveWhy path tr origin before after dropped start st in (st', map fst its)

-- | 'archiveStep' with the dropped turns known only by their messages.
archiveWhy :: Text -> [PyVal] -> M.Map Int Int -> [Turn] -> [Turn] -> [[PyVal]] -> Int -> Store -> (Store, [(Item, Text)])
archiveWhy path tr origin before after dropped start st =
  let (st', its, _) = archiveStep path tr origin before after dropped S.empty start st in (st', its)

-- | The archive step of one batch (spec 3.2). Arguments: the context
-- file's path, the transcript (ending with this batch's messages), the
-- origin map (CLM turn id to transcript position), the turns before the
-- batch and after it, the units a rollback dropped (empty otherwise), the
-- ids of the context turns it dropped (a turn whose message is among the
-- dropped units counts too), the transcript position of this batch's first
-- message, and the store.
--
-- Every original that is no longer live in full ('liveIn' the turns after)
-- is stored once as an 'Archived' item, in transcript order: an
-- origin-mapped turn (before or after) that disappeared (@rolled_back@ when
-- the rollback dropped it, @deleted@ otherwise) or whose live message
-- differs (@shortened@ for a read stub, @cut@ for a result of this batch,
-- @rewritten@ otherwise), and, after a rollback, this batch's messages that
-- no turn holds (@rolled_back@). Turns without an origin (the model's own
-- flat turns, rollback notes) are never stored; positions in 'sOrigins' are
-- skipped.
--
-- Also gives the ids, ascending, of every item that holds an original the
-- rollback dropped, whether stored now or before (a spilled result's item),
-- for the rollback note.
archiveStep ::
  Text -> [PyVal] -> M.Map Int Int -> [Turn] -> [Turn] -> [[PyVal]] -> S.Set Int -> Int -> Store -> (Store, [(Item, Text)], [Int])
archiveStep path tr origin before after dropped droppedIds start st0 =
  let (st', its) = foldl step (st0, []) (M.toAscList cands)
      named = [itId it | it <- sItems st', Just p <- [itOrigin it], S.member p droppedPos]
   in (st', its, S.toAscList (S.fromList named))
  where
    trS = Seq.fromList tr
    n = Seq.length trS
    at p = Seq.index trS p
    beforeM = M.fromList [(i, m) | Turn i m <- before]
    afterM = M.fromList [(i, m) | Turn i m <- after]
    live = map turnMsg after
    flatDropped = concat dropped
    mapped = S.fromList (M.elems origin)
    wasDropped i =
      not (M.member i afterM)
        && (S.member i droppedIds || maybe False (`elem` flatDropped) (M.lookup i beforeM))
    -- position -> (turn, why)
    turnCands =
      M.fromList
        [ (p, (Just i, whyTurn i p))
        | (i, p) <- M.toList origin
        , p >= 0, p < n
        , M.member i beforeM || M.member i afterM ]
    batchPos = if null dropped then [] else [p | p <- [max 0 start .. n - 1], not (S.member p mapped)]
    batchCands = M.fromList [(p, (Nothing, "rolled_back")) | p <- batchPos]
    droppedPos = S.fromList (batchPos <> [p | (p, (_, w)) <- M.toList turnCands, w == "rolled_back"])
    cands = M.union turnCands batchCands
    whyTurn i p = case M.lookup i afterM of
      Nothing -> if wasDropped i then "rolled_back" else "deleted"
      Just m
        | not (M.member i beforeM) -> if roleOf (at p) == "tool" then "cut" else "rewritten"
        | roleOf m == "tool" && "[context file, " `T.isPrefixOf` maybe "" pyStrOf (get "content" m) -> "shortened"
        | otherwise -> "rewritten"
    isLive orig = liveIn path orig live
    step (st, acc) (p, (tid, why))
      | S.member p (sOrigins st) = (st, acc)
      | isLive (at p) = (st, acc)
      | otherwise =
          let orig = at p
              it =
                Item
                  { itId = sNext st, itKind = Archived, itTurn = tid
                  , itTool = if roleOf orig == "tool" then toolAt p orig else Nothing
                  , itOrigin = Just p, itMessage = orig }
           in (st {sItems = sItems st <> [it], sNext = sNext st + 1, sOrigins = S.insert p (sOrigins st)}, acc <> [(it, why)])
    -- the tool name of a result: its call in the nearest earlier assistant
    -- message that has it (the scan stops at the first match), else the
    -- message's own name
    toolAt p m =
      let cid = get "tool_call_id" m
          calls = [c | q <- [p - 1, p - 2 .. 0], let a = at q, roleOf a == "assistant", c <- toolCallsOf a]
       in case find (\c -> get "id" c == cid) calls of
            Just c | Just nm <- get "function" c >>= get "name" -> Just (pyStrOf nm)
            _ -> case get "name" m of
              Just v@(PStr _) -> Just (pyStrOf v)
              _ -> Nothing

-- | A sync that replaced turns (a loop-side rewrite of a message: the turn
-- and every later one take fresh ids): the originals of the replaced turns
-- that have one and are not live in full in the turns after it are stored,
-- as @rewritten@ ('archiveStep' over those turns alone). Arguments: the
-- context file's path, the transcript, the origin map, the replaced turns,
-- the turns after the sync, and the store.
archiveRewritten :: Text -> [PyVal] -> M.Map Int Int -> [Turn] -> [Turn] -> Store -> (Store, [(Item, Text)])
archiveRewritten path tr origin gone after st =
  let origin' = M.restrictKeys origin (S.fromList [i | Turn i _ <- gone])
      (st', its, _) = archiveStep path tr origin' gone after [] S.empty (length tr) st
   in (st', [(it, "rewritten") | (it, _) <- its])

-- ------------------------------------------------------------------ read_stored

-- | @read_stored(id, from_line, to_line)@: limit, store, id text, lines.
readStored :: Integer -> Store -> Text -> Int -> Int -> Text
readStored limit st ident from to
  | null (sItems st) = emptyStoreText
  | otherwise = case mapMaybe pick (sItems st) of
      [] -> unknown
      (it : _) -> slice it
  where
    wanted = parseName ident
    pick it = if Just (itId it) == wanted then Just it else Nothing
    unknown =
      "Error: no stored item " <> ident <> " (the store holds " <> itemName (minimum ids) <> " to " <> itemName (maximum ids) <> ")."
    ids = map itId (sItems st)
    slice it
      | from < 1 || to < 1 || from > to = "(no lines: from_line must be at least 1 and not after to_line)"
      | from > n = "(no lines: " <> itemName (itId it) <> " has " <> T.pack (show n) <> " lines)"
      | otherwise =
          let to' = min to n
              want = take (to' - from + 1) (drop (from - 1) ls)
              kept = fitLines (limit `div` 4) want
              -- a slice always holds at least one (wrapped) line
              kept' = if null kept then take 1 want else kept
              last' = from + length kept' - 1
           in if last' >= to'
                then T.intercalate "\n" kept'
                else
                  T.intercalate "\n" kept' <> "\n... (" <> T.pack (show (to' - last')) <> " more lines; read_stored(\""
                    <> itemName (itId it) <> "\", " <> T.pack (show (last' + 1)) <> ", " <> T.pack (show to') <> ") continues)"
      where
        ls = wrapLines (itemBody it)
        n = length ls

-- | @S7@ to 7.
parseName :: Text -> Maybe Int
parseName t = case T.uncons (T.strip t) of
  Just (c, rest) | c == 'S' || c == 's', Just k <- readMaybe (T.unpack rest), k >= 1, T.all (`elem` ['0' .. '9']) rest -> Just k
  _ -> Nothing

-- ------------------------------------------------------------------ texts

-- | The rollback note's addition naming the items that hold the dropped
-- turns: consecutive ids as ranges, joined with commas and a last "and"
-- (@ The dropped turns are stored as S3, S7 and S9 to S11.@).
storedRangeNote :: [Int] -> Text
storedRangeNote ids0
  | null ids0 = ""
  | otherwise = notePrefix <> joinAnd (map run (runs (S.toAscList (S.fromList ids0)))) <> "."
  where
    runs [] = []
    runs (x : xs) = let (r, rest) = go x x xs in r : runs rest
    go a b (y : ys) | y == b + 1 = go a y ys
    go a b ys = ((a, b), ys)
    run (a, b) = itemName a <> (if a == b then "" else " to " <> itemName b)
    joinAnd [x] = x
    joinAnd xs = T.intercalate ", " (init xs) <> " and " <> last xs

notePrefix :: Text
notePrefix = " The dropped turns are stored as "

-- | The ids a note's 'storedRangeNote' names (none when it has none).
noteStoredIds :: Text -> [Int]
noteStoredIds t = case T.breakOnEnd notePrefix t of
  ("", _) -> []
  (_, rest) ->
    let body = fromMaybe rest (T.stripSuffix "." (T.strip (T.takeWhile (/= '\n') rest)))
        parts = concatMap (T.splitOn " and ") (T.splitOn ", " body)
        one p = case T.splitOn " to " p of
          [a] -> maybe [] pure (parseName a)
          [a, b] | Just x <- parseName a, Just y <- parseName b -> [x .. y]
          _ -> []
     in S.toAscList (S.fromList (concatMap one parts))

storePromptSection :: Text
storePromptSection =
  "Large tool outputs and anything you cut from the context are kept in a store, so nothing is lost. "
    <> "read_stored gives exact lines of a stored item (S1, S2, ...); recall answers a question about all of them. "
    <> "The context budget is never a reason to stop: store, read back or recall what you need, "
    <> "and keep working until the task is done."

readStoredSchema :: PyVal
readStoredSchema =
  obj
    [ ("type", PStr "function")
    , ( "function"
      , obj
          [ ("name", PStr "read_stored")
          , ( "description"
            , PStr "Read lines of a stored item (a large tool output, or a turn cut from the context), exactly as it was. Lines are 1-based and inclusive; line numbers past the end are clipped." )
          , ( "parameters"
            , obj
                [ ("type", PStr "object")
                , ( "properties"
                  , obj
                      [ ("id", obj [("type", PStr "string"), ("description", PStr "The item, such as S7.")])
                      , ("from_line", obj [("type", PStr "integer"), ("description", PStr "First line, from 1.")])
                      , ("to_line", obj [("type", PStr "integer"), ("description", PStr "Last line, inclusive.")]) ] )
                , ("required", PList [PStr "id", PStr "from_line", PStr "to_line"]) ] ) ] ) ]

-- ------------------------------------------------------------------ persistence

storeToPy :: Store -> PyVal
storeToPy s =
  obj
    [ ("next", PInt (fromIntegral (sNext s)))
    , ("origins", PList (map (PInt . fromIntegral) (S.toAscList (sOrigins s))))
    , ("items", PList (map itemToPy (sItems s))) ]
  where
    opt k = maybe [] (\v -> [(k, v)])
    itemToPy it =
      obj $
        [ ("id", PInt (fromIntegral (itId it)))
        , ("kind", PStr (case itKind it of Spilled -> "spilled"; Archived -> "archived")) ]
          <> opt "turn" (PInt . fromIntegral <$> itTurn it)
          <> opt "tool" (PStr <$> itTool it)
          <> opt "origin" (PInt . fromIntegral <$> itOrigin it)
          <> [("message", itMessage it)]

storeFromPy :: PyVal -> Maybe Store
storeFromPy v = do
  PInt nx <- get "next" v
  PList os <- get "origins" v
  PList is <- get "items" v
  origins <- traverse int os
  items <- traverse itemFromPy is
  pure (Store items (maximum (fromIntegral nx : map ((+ 1) . itId) items)) (S.fromList origins))
  where
    int (PInt n) = Just (fromIntegral n)
    int _ = Nothing
    optInt k d = case get k d of
      Nothing -> Just Nothing
      Just (PInt n) -> Just (Just (fromIntegral n))
      Just _ -> Nothing
    itemFromPy d = do
      PInt i <- get "id" d
      kind <- case get "kind" d of
        Just (PStr "spilled") -> Just Spilled
        Just (PStr "archived") -> Just Archived
        _ -> Nothing
      turn <- optInt "turn" d
      tool <- case get "tool" d of
        Nothing -> Just Nothing
        Just (PStr t) -> Just (Just t)
        Just _ -> Nothing
      origin <- optInt "origin" d
      msg <- get "message" d
      pure (Item (fromIntegral i) kind turn tool origin msg)

