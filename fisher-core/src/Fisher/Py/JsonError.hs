-- | The message Python's @json.loads@ raises for text that is not JSON,
-- exactly as @str(JSONDecodeError)@ spells it (CPython 3.11, C scanner):
-- @Expecting ',' delimiter: line 1 column 8 (char 7)@. The agent loop
-- quotes it back to the model when a tool call's arguments do not parse.
module Fisher.Py.JsonError
  ( jsonError
  ) where

import Data.Char (isDigit, isHexDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as V

-- | 'Nothing' when the text parses; otherwise the error message.
jsonError :: Text -> Maybe Text
jsonError t
  | "\xFEFF" `T.isPrefixOf` t = Just (render "Unexpected UTF-8 BOM (decode using utf-8-sig)" 0)
  | otherwise =
      case value (ws 0) of
        Left (Stop p) -> Just (render "Expecting value" p)
        Left (Err m p) -> Just (render m p)
        Right end ->
          let e = ws end
           in if e /= n then Just (render "Extra data" e) else Nothing
  where
    s = V.fromList (T.unpack t)
    n = V.length s
    at i = if i < n then Just (s V.! i) else Nothing
    render m p =
      let before = take p (T.unpack t)
          line = 1 + length (filter (== '\n') before)
          col = case [i | (i, c) <- zip [0 :: Int ..] before, c == '\n'] of
            [] -> p + 1
            is -> p - last is
       in m <> ": line " <> tshow line <> " column " <> tshow col <> " (char " <> tshow p <> ")"
    ws i = case at i of
      Just c | c `elem` (" \t\n\r" :: String) -> ws (i + 1)
      _ -> i
    lit i w = T.unpack w == [c | j <- [i .. i + T.length w - 1], Just c <- [at j]]

    -- scan_once: the end of the value starting at i
    value i = case at i of
      Just '"' -> string (i + 1)
      Just '{' -> object (i + 1)
      Just '[' -> array (i + 1)
      Just 'n' | lit i "null" -> Right (i + 4)
      Just 't' | lit i "true" -> Right (i + 4)
      Just 'f' | lit i "false" -> Right (i + 5)
      Just 'N' | lit i "NaN" -> Right (i + 3)
      Just 'I' | lit i "Infinity" -> Right (i + 8)
      Just '-' | lit i "-Infinity" -> Right (i + 9)
      Just _ -> number i
      Nothing -> Left (Stop i)

    number start =
      let i0 = if at start == Just '-' then start + 1 else start
          intEnd = case at i0 of
            Just c | c >= '1' && c <= '9' -> Just (digits (i0 + 1))
            Just '0' -> Just (i0 + 1)
            _ -> Nothing
       in case intEnd of
            Nothing -> Left (Stop start)
            Just i1 ->
              let i2 = case (at i1, at (i1 + 1)) of
                    (Just '.', Just d) | isDigit d -> digits (i1 + 2)
                    _ -> i1
                  i3 = case at i2 of
                    Just e | e == 'e' || e == 'E' ->
                      let j = if at (i2 + 1) `elem` [Just '+', Just '-'] then i2 + 2 else i2 + 1
                          k = digits j
                       in if k > j then k else i2
                    _ -> i2
               in Right i3
    digits i = case at i of
      Just c | isDigit c -> digits (i + 1)
      _ -> i

    -- scanstring: i is just past the opening quote
    string begin = go begin
      where
        go i = case at i of
          Nothing -> Left (Err "Unterminated string starting at" (begin - 1))
          Just '"' -> Right (i + 1)
          Just '\\' -> case at (i + 1) of
            Nothing -> Left (Err "Unterminated string starting at" (begin - 1))
            Just 'u' -> unicode (i + 2)
            Just c
              | c `elem` ("\"\\/bfnrt" :: String) -> go (i + 2)
              | otherwise -> Left (Err "Invalid \\escape" i)
          Just c
            | c <= '\x1f' -> Left (Err "Invalid control character at" i)
            | otherwise -> go (i + 1)
        -- next: the first hex digit
        unicode next
          | next + 4 >= n = Left (Err "Invalid \\uXXXX escape" (next - 1))
          | not (all isHexDigit [s V.! j | j <- [next .. next + 3]]) = Left (Err "Invalid \\uXXXX escape" (next - 1))
          | otherwise =
              let code = read ("0x" <> [s V.! j | j <- [next .. next + 3]]) :: Int
                  after = next + 4
               in if code >= 0xD800 && code <= 0xDBFF && after + 6 < n && at after == Just '\\' && at (after + 1) == Just 'u'
                    then
                      if all isHexDigit [s V.! j | j <- [after + 2 .. after + 5]]
                        then go (after + 6)
                        else Left (Err "Invalid \\uXXXX escape" (after + 1))
                    else go after

    object i0 =
      let i = ws i0
       in case at i of
            Just '"' -> member i
            Just '}' -> Right (i + 1)
            _ -> Left (Err "Expecting property name enclosed in double quotes" i)
      where
        member i = do
          k <- string (i + 1)
          let c = ws k
          if at c /= Just ':'
            then Left (Err "Expecting ':' delimiter" c)
            else do
              v <- value (ws (c + 1))
              let j = ws v
              case at j of
                Just '}' -> Right (j + 1)
                Just ',' ->
                  let q = ws (j + 1)
                   in if at q == Just '"' then member q else Left (Err "Expecting property name enclosed in double quotes" q)
                _ -> Left (Err "Expecting ',' delimiter" j)

    array i0 =
      let i = ws i0
       in if at i == Just ']' then Right (i + 1) else element i
      where
        element i = do
          v <- value i
          let j = ws v
          case at j of
            Just ']' -> Right (j + 1)
            Just ',' -> element (ws (j + 1))
            _ -> Left (Err "Expecting ',' delimiter" j)

data Failure = Stop Int | Err Text Int

tshow :: Int -> Text
tshow = T.pack . show
