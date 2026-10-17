-- | The RLM context strategy, pure part: the store of moved-out turns, the
-- offload cut that moves the middle of the history into it, and the fixed
-- texts (stub, prompt section, recall schema).
module Fisher.Agent.Context.Rlm
  ( StoredTurn (..)
  , RlmState (..)
  , emptyRlmState
  , Offload (..)
  , offload
  , cutTail
  , renderTurn
  , isStub
  , stubMessage
  , promptSection
  , recallSchema
  , emptyStoreText
  , liveHistory
  , stateToPy
  , stateFromPy
  ) where

import Data.List (findIndex, sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Agent.Context.Mirror (contentText, toolCallsOf)
import Fisher.Agent.Types (commas)
import Fisher.Py.Value

-- | A message moved out of the context.
data StoredTurn = StoredTurn
  { stIndex :: Int -- ^ 1-based position in the session's history (after the system prompt)
  , stRole :: Text -- ^ "user" | "assistant" | "tool"
  , stTool :: Maybe Text -- ^ tool name for a tool result
  , stMessage :: PyVal -- ^ the original message, unchanged
  }
  deriving (Eq, Show)

data RlmState = RlmState
  { rsStore :: [StoredTurn] -- ^ in index order
  , rsOffloads :: Int
  , rsRange :: Maybe (Int, Int) -- ^ first and last stored index covered by the stub
  }
  deriving (Eq, Show)

emptyRlmState :: RlmState
emptyRlmState = RlmState [] 0 Nothing

data Offload = Offload
  { ofMessages :: [PyVal]
  , ofState :: RlmState
  , ofMoved :: (Int, Int) -- ^ session indices of the first and last message newly moved
  , ofNew :: [StoredTurn] -- ^ the turns this offload added to the store (not those a cut stored before)
  , ofMovedMessages :: [PyVal] -- ^ the messages newly moved, as the history held them (a cut result as its preview)
  }

-- ------------------------------------------------------------------ texts

stubPrefix, stubPhrase :: Text
stubPrefix = "Turns "
stubPhrase = " were moved out of the context to save space."

stubMessage :: (Int, Int) -> Int -> Int -> PyVal
stubMessage (a, b) n c =
  obj
    [ ("role", PStr "user")
    , ( "content"
      , PStr $
          "Turns " <> tshow a <> " to " <> tshow b <> " of this session (" <> tshow n
            <> " messages, " <> commas (fromIntegral c) <> " characters) were moved out of the context to save space. "
            <> "Nothing was summarised or lost. To get anything from them, such as an earlier tool output, a decision, "
            <> "or a proof attempt, call recall with a specific question." )
    ]

isStub :: PyVal -> Bool
isStub m = case get "content" m of
  Just (PStr s) -> stubPrefix `T.isPrefixOf` s && stubPhrase `T.isInfixOf` s
  _ -> False

promptSection :: Text
promptSection =
  "Earlier turns of this session may have been moved out of the context. The recall tool answers a question about them by searching their full text with a recursive language model. Each call costs model calls, so ask a specific question, and ask again rather than guessing when you need an exact earlier output."

emptyStoreText :: Text
emptyStoreText = "Nothing has been moved out of the context yet."

recallSchema :: PyVal
recallSchema =
  obj
    [ ("type", PStr "function")
    , ( "function"
      , obj
          [ ("name", PStr "recall")
          , ( "description"
            , PStr "Answer a question about the turns of this session that were moved out of the context, by searching their full text. Ask a specific question." )
          , ( "parameters"
            , obj
                [ ("type", PStr "object")
                , ("properties", obj [("question", obj [("type", PStr "string"), ("description", PStr "What you need from the earlier turns.")])])
                , ("required", PList [PStr "question"]) ] ) ] ) ]

renderTurn :: StoredTurn -> Text
renderTurn t =
  T.intercalate "\n" (header : body <> calls)
  where
    m = stMessage t
    k = tshow (stIndex t)
    header = case stRole t of
      "tool" -> "[turn " <> k <> ", tool " <> fromMaybe "unknown" (stTool t) <> "]"
      r -> "[turn " <> k <> ", " <> r <> "]"
    text = contentText (get "content" m)
    body = [text | not (T.null text)]
    calls =
      [ "call " <> fnName c <> " " <> argText c
      | stRole t == "assistant", c <- toolCallsOf m ]
    fnName c = maybe "" pyStrOf (get "function" c >>= get "name")
    argText c = case get "function" c >>= get "arguments" of
      Just (PStr s) -> s
      Just v -> dumpsUnicode v
      Nothing -> ""

-- ------------------------------------------------------------------ helpers

tshow :: Show a => a -> Text
tshow = T.pack . show

roleOf :: PyVal -> Text
roleOf m = maybe "" pyStrOf (get "role" m)

isTool :: PyVal -> Bool
isTool m = roleOf m == "tool"

-- | Tool names by call id, from the store and then the history, in order.
callNames :: RlmState -> [PyVal] -> M.Map Text Text
callNames st hist = foldl add M.empty (map stMessage (rsStore st) <> hist)
  where
    add acc m
      | roleOf m == "assistant" =
          foldl (\a c -> case (get "id" c, get "function" c >>= get "name") of
                    (Just i, Just n) -> M.insert (pyStrOf i) (pyStrOf n) a
                    _ -> a) acc (toolCallsOf m)
      | otherwise = acc

-- | A tool result's name from a call id seen before it, else "unknown".
toolName :: M.Map Text Text -> PyVal -> Maybe Text
toolName names m
  | isTool m = Just (fromMaybe "unknown" (get "tool_call_id" m >>= \i -> M.lookup (pyStrOf i) names))
  | otherwise = Nothing

-- | The session index of the message at a 1-based history position.
sessionIdx :: RlmState -> [PyVal] -> Int -> Int
sessionIdx st hist p = case (rsRange st, findIndex isStub hist) of
  (Just (_, b), Just i) | p > i + 1 -> b + (p - (i + 1))
  _ -> p

mkTurn :: M.Map Text Text -> Int -> PyVal -> StoredTurn
mkTurn names i m = StoredTurn i (roleOf m) (toolName names m) m

-- | Insert turns, keeping index order; an index already stored keeps its entry.
insertTurns :: [StoredTurn] -> [StoredTurn] -> [StoredTurn]
insertTurns old new =
  sortOn stIndex (old <> [t | t <- new, stIndex t `notElem` map stIndex old])

inRange :: (Int, Int) -> StoredTurn -> Bool
inRange (a, b) t = stIndex t >= a && stIndex t <= b

stubFor :: RlmState -> (Int, Int) -> PyVal
stubFor st r =
  let ts = filter (inRange r) (rsStore st)
   in stubMessage r (length ts) (sum (map (T.length . renderTurn) ts))

-- ------------------------------------------------------------------ the cut

-- | Protected head, protected tail, state, current history (no system prompt).
-- The middle moves into the store and a stub takes its place; no assistant
-- call is separated from the tool results that answer it.
offload :: Int -> Int -> RlmState -> [PyVal] -> Maybe Offload
offload h t st hist
  | null fresh = Nothing
  | maybe False (> ts) stubPos = Nothing
  | otherwise =
      Just
        Offload
          { ofMessages = take he hist <> [stubFor st' range'] <> drop ts hist
          , ofState = st'
          , ofMoved = (fst (head freshIdx), fst (last freshIdx))
          , ofNew = [x | x <- rsStore st', stIndex x `notElem` map stIndex (rsStore st)]
          , ofMovedMessages = map snd freshIdx }
  where
    n = length hist
    stubPos = (+ 1) <$> findIndex isStub hist
    he = case stubPos of
      Just sp | sp <= h + 1 -> sp - 1
      _ -> headSnap (max 0 h)
    headSnap k | k < n && isTool (hist !! k) = headSnap (k + 1)
               | otherwise = min k n
    ts = tailSnap (max 0 (n - t))
    tailSnap k | k > 0 && k < n && isTool (hist !! k) = tailSnap (k - 1)
               | otherwise = k
    middle = if ts > he then zip [he + 1 ..] (take (ts - he) (drop he hist)) else []
    stubInMiddle = any (isStub . snd) middle
    freshIdx = [(sessionIdx st hist p, m) | (p, m) <- middle, not (isStub m)]
    fresh = freshIdx
    names = callNames st hist
    a' = case (stubInMiddle, rsRange st) of
      (True, Just (a, _)) -> a
      _ -> fst (head freshIdx)
    range' = (a', fst (last freshIdx))
    st' =
      st
        { rsStore = insertTurns (rsStore st) [mkTurn names i m | (i, m) <- freshIdx]
        , rsOffloads = rsOffloads st + 1
        , rsRange = Just range' }

-- | Spec 3.4: tool results among the last 4 messages longer than 20,000
-- characters keep their first 2,000; the full message goes to the store.
cutTail :: RlmState -> [PyVal] -> ([PyVal], RlmState)
cutTail st hist = (hist', st {rsStore = insertTurns (rsStore st) newTurns})
  where
    n = length hist
    names = callNames st hist
    cut = map step (zip [1 ..] hist)
    step (p, m)
      | p > n - 4 && isTool m && T.length (contentText (get "content" m)) > 20000 =
          let k = sessionIdx st hist p
           in (previewOf k m, Just (mkTurn names k m))
      | otherwise = (m, Nothing)
    hist' = map fst cut
    newTurns = mapMaybe snd cut

-- | A cut tool result as the context keeps it: its first 2,000 characters
-- and a line pointing at its turn in the store (the session index given).
previewOf :: Int -> PyVal -> PyVal
previewOf k m =
  let full = contentText (get "content" m)
      preview =
        T.take 2000 full <> "\n... (" <> commas (fromIntegral (T.length full - 2000))
          <> " more characters; the full output is in recall's store as turn " <> tshow k <> ")"
   in set "content" (PStr preview) m

-- | The full transcript from state.db as the model sees it: head, stub, the
-- messages after the stored range, and every turn a tail cut stored (a
-- stored turn outside the stub's range) back to its preview.
liveHistory :: RlmState -> [PyVal] -> [PyVal]
liveHistory st full = case rsRange st of
  Nothing -> map back indexed
  Just r@(a, b) -> map back (take (a - 1) indexed) <> [stubFor st r] <> map back (drop b indexed)
  where
    indexed = zip [1 ..] full
    cuts = S.fromList [stIndex t | t <- rsStore st, maybe True (\r -> not (inRange r t)) (rsRange st)]
    back (i, m)
      | i `S.member` cuts = previewOf i m
      | otherwise = m

-- ------------------------------------------------------------------ persistence

stateToPy :: RlmState -> PyVal
stateToPy st =
  obj
    [ ("store", PList (map turn (rsStore st)))
    , ("offloads", PInt (fromIntegral (rsOffloads st)))
    , ("range", maybe PNull (\(a, b) -> PList [PInt (fromIntegral a), PInt (fromIntegral b)]) (rsRange st)) ]
  where
    turn t =
      obj
        [ ("index", PInt (fromIntegral (stIndex t)))
        , ("role", PStr (stRole t))
        , ("tool", maybe PNull PStr (stTool t))
        , ("message", stMessage t) ]

stateFromPy :: PyVal -> Maybe RlmState
stateFromPy v = do
  ts <- get "store" v >>= asList >>= mapM turn
  o <- get "offloads" v >>= int
  r <- case get "range" v of
    Just PNull -> Just Nothing
    Just (PList [x, y]) -> do a <- int x; b <- int y; Just (Just (a, b))
    _ -> Nothing
  Just (RlmState ts o r)
  where
    int = \case PInt i -> Just (fromIntegral i); _ -> Nothing
    turn t = do
      i <- get "index" t >>= int
      r <- get "role" t >>= asText
      tool <- case get "tool" t of
        Just PNull -> Just Nothing
        Just (PStr s) -> Just (Just s)
        _ -> Nothing
      m <- get "message" t
      Just (StoredTurn i r tool m)
