-- | QuickCheck properties of the mirror (docs/clm.md section 13, P1 to P5).
module Fisher.Agent.ClmPropSpec (spec) where

import qualified Data.ByteString as BS
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Hspec
import Test.QuickCheck

import Fisher.Agent.Context.Mirror (Edit (..), ReadBack (..), Turn (..), readBack, render)
import Fisher.Py.Value

sid :: Text
sid = "20261006_101500_ab12cd"

-- ---------------------------------------------------------------- generators

-- | Text with the lines the format must survive: header look-alikes,
-- escapes, blank lines, trailing spaces, carriage returns, unicode.
bodyText :: Gen Text
bodyText = T.intercalate "\n" <$> listOf1 line
  where
    line =
      frequency
        [ (6, T.pack <$> listOf (elements (['a' .. 'z'] <> " .:;{}[]\"'")))
        , (1, pure "")
        , (1, pure "   ")
        , (1, (\n -> "<<< turn " <> T.pack (show (n :: Int)) <> " user >>>") <$> choose (0, 30))
        , (1, (\k -> T.replicate k "\\" <> "<<< turn 3 assistant >>>") <$> choose (1, 3))
        , (1, pure "<<< turn x >>>")
        , (1, pure "line with a carriage return\r")
        , (1, pure "théorème ∀ x, x = x")
        , (1, pure "# Fisher context file for session 20261006_101500_ab12cd.") ]

callId :: Int -> Text
callId n = "call_" <> T.pack (show n)

toolCall :: Int -> Text -> Text -> PyVal
toolCall n name args =
  PDict
    [ ("id", PStr (callId n)), ("call_id", PStr (callId n)), ("response_item_id", PStr ("fc_" <> T.pack (show n)))
    , ("type", PStr "function"), ("function", PDict [("name", PStr name), ("arguments", PStr args)]) ]

assistantMsg :: Text -> [PyVal] -> PyVal
assistantMsg c calls =
  PDict $
    [("role", PStr "assistant"), ("content", PStr c), ("reasoning", PNull), ("finish_reason", PStr (if null calls then "stop" else "tool_calls"))]
      <> [("tool_calls", PList calls) | not (null calls)]

-- | A legal list of editable turns: user turns, plain answers, and groups
-- of an assistant with one to three calls followed by their results.
editable :: Gen [Turn]
editable = do
  units <- listOf unit
  let msgs = go 1 units
  pure (zipWith Turn [1 ..] msgs)
  where
    unit = frequency [(2, Left <$> bodyText), (1, Right . Left <$> bodyText), (3, Right . Right <$> ((,) <$> bodyText <*> listOf1 bodyText `suchThat` ((<= 3) . length)))]
    go _ [] = []
    go n (Left t : us) = PDict [("role", PStr "user"), ("content", PStr t)] : go n us
    go n (Right (Left t) : us) = assistantMsg t [] : go n us
    go n (Right (Right (t, results)) : us) =
      let k = length results
          names = take k (cycle ["terminal", "read_file", "search_files"])
          calls = [toolCall (n + i) nm "{\"command\": \"x\"}" | (i, nm) <- zip [0 ..] names]
          tools = [PDict [("role", PStr "tool"), ("content", PStr r), ("tool_call_id", PStr (callId (n + i)))] | (i, r) <- zip [0 ..] results]
       in assistantMsg t calls : tools <> go (n + k) us

-- | An edit of a mirror text, the way a model might make one.
edited :: Text -> Gen Text
edited t = do
  let ls = T.splitOn "\n" t
  ops <- listOf1 (op (length ls))
  pure (T.intercalate "\n" (foldl apply ls ops))
  where
    op n = oneof
      [ Del <$> choose (0, n), Dup <$> choose (0, n), Ins <$> choose (0, n) <*> bodyText
      , Swap <$> choose (0, n) <*> choose (0, n), pure Crlf ]
    apply ls = \case
      Del i -> take i ls <> drop (i + 1) ls
      Dup i -> take (i + 1) ls <> take 1 (drop i ls) <> drop (i + 1) ls
      Ins i x -> take i ls <> T.splitOn "\n" x <> drop i ls
      Swap i j | i < length ls, j < length ls -> [if k == i then ls !! j else if k == j then ls !! i else l | (k, l) <- zip [0 ..] ls]
      Swap _ _ -> ls
      Crlf -> map (<> "\r") ls

data Op = Del Int | Dup Int | Ins Int Text | Swap Int Int | Crlf
  deriving (Show)

-- ---------------------------------------------------------------- legality (P1)

role :: PyVal -> Text
role m = case get "role" m of
  Just (PStr r) -> r
  _ -> ""

callIds :: PyVal -> [PyVal]
callIds m = case get "tool_calls" m of
  Just (PList cs) -> [i | c <- cs, Just i <- [get "id" c]]
  _ -> []

-- | Tool messages only right after the assistant that called them (or its
-- other results); no flattened message is empty; no two adjacent
-- flattened messages share a role; no flattened assistant directly before
-- an assistant with tool calls. Flattened = not one of the originals.
legal :: [PyVal] -> [PyVal] -> Bool
legal originals ms = toolsOk ms && all nonEmpty fl && noAdjacent
  where
    isFlat m = m `notElem` originals
    fl = filter isFlat ms
    nonEmpty m = maybe False (\c -> not (T.null (T.strip (pyStrOf c)))) (get "content" m)
    noAdjacent =
      and
        [ not (isFlat a && isFlat b && role a == role b) && not (isFlat a && role a == "assistant" && role b == "assistant" && not (null (callIds b)))
        | (a, b) <- zip ms (drop 1 ms) ]
    toolsOk = go []
      where
        go _ [] = True
        go open (m : rest)
          | role m == "tool" = maybe False (`elem` open) (get "tool_call_id" m) && go open rest
          | role m == "assistant" = go (callIds m) rest
          | otherwise = go [] rest

result :: [Turn] -> ReadBack -> [Turn]
result old = \case
  Edited e -> editTurns e
  _ -> old

-- | Some of the turns, as the ones the model has seen in the file.
seenOf :: [Turn] -> Gen [Int]
seenOf ts = frequency [(3, pure (map turnId ts)), (2, sublistOf (map turnId ts))]

all' :: [Turn] -> [Int]
all' = map turnId

spec :: Spec
spec = describe "the mirror (properties)" $ do
  it "P1: any edit reads back to a legal list" $
    property $ forAll editable $ \ts -> forAll (edited (render sid ts)) $ \t -> forAll (seenOf ts) $ \seen ->
      let rb = readBack sid ts (length ts + 1) seen (Just (TE.encodeUtf8 t))
       in counterexample (T.unpack t) (legal (map turnMsg ts) (map turnMsg (result ts rb)))
  it "P2: the unedited mirror reads back unchanged" $
    property $ forAll editable $ \ts ->
      readBack sid ts (length ts + 1) (all' ts) (Just (TE.encodeUtf8 (render sid ts))) === NoEdit 0
  it "P3: reading back is idempotent" $
    property $ forAll editable $ \ts -> forAll (edited (render sid ts)) $ \t -> forAll (seenOf ts) $ \seen ->
      case readBack sid ts (length ts + 1) seen (Just (TE.encodeUtf8 t)) of
        Edited e -> readBack sid (editTurns e) (editNext e) (all' (editTurns e)) (Just (TE.encodeUtf8 (render sid (editTurns e)))) === NoEdit 0
        _ -> property True
  it "P4: any body text round-trips through escaping" $
    property $ forAll bodyText $ \b ->
      let ts = [Turn 1 (PDict [("role", PStr "user"), ("content", PStr b)])]
       in readBack sid ts 2 [1] (Just (TE.encodeUtf8 (render sid ts))) === NoEdit 0
  it "P5: ids stay unique, and new ones come from the counter" $
    property $ forAll editable $ \ts -> forAll (edited (render sid ts)) $ \t -> forAll (seenOf ts) $ \seen ->
      let n0 = length ts + 1
       in case readBack sid ts n0 seen (Just (TE.encodeUtf8 t)) of
            Edited e ->
              let ids = map turnId (editTurns e)
                  n = editNext e
               in ids == nub ids && all (\i -> i `elem` map turnId ts || (i >= n0 && i < n)) ids && n >= n0
            _ -> True
  it "a file that is not UTF-8 is rejected" $
    property $ forAll editable $ \ts ->
      readBack sid ts 1 (all' ts) (Just (TE.encodeUtf8 (render sid ts) <> BS.pack [0xff])) === NotUtf8
