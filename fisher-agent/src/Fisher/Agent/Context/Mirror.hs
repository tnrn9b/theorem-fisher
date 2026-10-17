-- | The mirror: the editable turns written as a text file the model edits,
-- and the read-back that turns an edited file into turns again
-- (docs/clm.md sections 4 and 5; rule numbers are the spec's).
module Fisher.Agent.Context.Mirror
  ( Turn (..)
  , Edit (..)
  , ReadBack (..)
  , render
  , readBack
  , banner
  , bodyOf
  , contentText
  , headerIds
  , tokens
  , units
  , plainAnswer
  , into
  , toolCallsOf
  ) where

import qualified Data.ByteString as BS
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Fisher.Py.Value

-- | An editable message and its id.
data Turn = Turn {turnId :: Int, turnMsg :: PyVal}
  deriving (Eq, Show)

-- | An edit the read-back found.
data Edit = Edit
  { editTurns :: [Turn]
  , editNext :: Int
  -- ^ the id counter after it
  , editKept :: Int
  -- ^ messages kept by rule 3 (the model had not read them)
  , editMergeIntoLive :: Bool
  -- ^ an answer is newly last: it goes into the live assistant
  , editWritten :: [Int]
  -- ^ ids of the messages made from blocks the model wrote
  }
  deriving (Eq, Show)

data ReadBack
  = NoEdit Int
  -- ^ no edit; the messages rule 3 kept
  | Missing
  | NotUtf8
  | Edited Edit
  deriving (Eq, Show)

-- | T(ms) (section 2): Fisher's @estimateMessagesTokensRough@, a quarter
-- of the summed lengths of the messages' Python reprs.
tokens :: [PyVal] -> Integer
tokens ms = sum [fromIntegral (T.length (pyRepr m)) | m <- ms] `div` 4

-- ---------------------------------------------------------------- format (4)

banner :: Text -> [Text]
banner sid =
  [ "# Fisher context file for session " <> sid <> "."
  , "# Each turn starts with a header line. Edit, shorten or delete turns to free context."
  , "# The system prompt and the task are pinned and are not shown here." ]

field :: Text -> PyVal -> Text
field k m = maybe "" pyStrOf (get k m)

role :: PyVal -> Text
role = field "role"

toolCallsOf :: PyVal -> [PyVal]
toolCallsOf m = case get "tool_calls" m of
  Just (PList cs) -> cs
  _ -> []

callId :: PyVal -> Maybe PyVal
callId = get "id"

fnField :: Text -> PyVal -> Text
fnField k c = maybe "" pyStrOf (get "function" c >>= get k)

-- | @content_text@: a message content as text.
contentText :: Maybe PyVal -> Text
contentText = \case
  Nothing -> ""
  Just PNull -> ""
  Just (PStr s) -> s
  Just (PList ps) ->
    T.intercalate "\n" $
      mapMaybe
        ( \p -> case get "type" p of
            Just (PStr "text") -> Just (maybe "" pyStrOf (get "text" p))
            Just (PStr t) | t `elem` ["image_url", "image", "input_image"] -> Just "[image]"
            _ -> Nothing )
        ps
  Just v -> pyStrOf v

-- | A message's body as the mirror writes it.
bodyOf :: PyVal -> Text
bodyOf m =
  let text = contentText (get "content" m)
   in if role m == "assistant"
        then case toolCallsOf m of
          [] -> text
          calls ->
            T.intercalate "\n" ([text | text /= ""] <> ["[call " <> fnField "name" c <> " " <> fnField "arguments" c <> "]" | c <- calls])
        else text

escapedPrefix :: Text -> Bool
escapedPrefix l = let rest = T.dropWhile (== '\\') l in T.length rest < T.length l && "<<< turn " `T.isPrefixOf` rest

escapeLine :: Text -> Text
escapeLine l = if "<<< turn " `T.isPrefixOf` l || escapedPrefix l then "\\" <> l else l

unescapeLine :: Text -> Text
unescapeLine l = if escapedPrefix l then T.drop 1 l else l

toolNames :: [Turn] -> M.Map Text Text
toolNames ts =
  foldl'
    (\acc c -> maybe acc (\i -> M.insert (pyStrOf i) (fnField "name" c) acc) (callId c))
    M.empty
    [c | Turn _ m <- ts, role m == "assistant", c <- toolCallsOf m]

nameOk :: Char -> Bool
nameOk c = isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` ("_.:-" :: String)

headerName :: M.Map Text Text -> PyVal -> Text
headerName names m
  | role m /= "tool" = ""
  | otherwise = case get "tool_call_id" m >>= (\i -> M.lookup (pyStrOf i) names) of
      Just n | not (T.null n) -> T.map (\c -> if nameOk c then c else '_') n
      _ -> ""

-- | The mirror text for the editable turns.
render :: Text -> [Turn] -> Text
render sid ts =
  let names = toolNames ts
      block (Turn i m) =
        let name = headerName names m
         in ["<<< turn " <> T.pack (show i) <> " " <> role m <> (if T.null name then "" else " " <> name) <> " >>>"]
              <> map escapeLine (T.splitOn "\n" (bodyOf m))
              <> [""]
   in T.intercalate "\n" (banner sid <> concatMap block ts) <> "\n"

-- | A header line: its id (0 when over nine digits), role and name.
parseHeader :: Text -> Maybe (Int, Text, Text)
parseHeader line0 = do
  rest0 <- T.stripPrefix "<<< turn " line0
  let (digits, rest1) = T.span isDigit rest0
  case T.uncons digits of
    Just (d, _) | d /= '0' -> Just ()
    _ -> Nothing
  rest2 <- T.stripPrefix " " rest1
  let (r, rest3) = T.break (== ' ') rest2
  if r `elem` ["user", "assistant", "tool"] then Just () else Nothing
  let rest4 = fromMaybe rest3 (T.stripSuffix "\r" rest3)
  body <- T.stripSuffix " >>>" rest4
  name <-
    if T.null body
      then Just ""
      else do
        n <- T.stripPrefix " " body
        if not (T.null n) && T.all nameOk n then Just n else Nothing
  let ident = if T.length digits <= 9 then read (T.unpack digits) else 0
  pure (ident, r, name)

-- | The ids in a mirror's headers.
headerIds :: Text -> [Int]
headerIds t = [i | l <- T.splitOn "\n" t, Just (i, _, _) <- [parseHeader l]]

isWs :: Char -> Bool
isWs c = c `elem` (" \t\n\r\v\f" :: String)

blank :: Text -> Bool
blank = T.all isWs

-- | Rule 4.
trim :: Text -> Text
trim t = T.dropWhileEnd isWs (T.intercalate "\n" (dropWhile blank (T.splitOn "\n" t)))

plainAnswer :: PyVal -> Bool
plainAnswer m = role m == "assistant" && null (toolCallsOf m)

-- | Rule 9: text put before an assistant message's own content.
into :: Text -> PyVal -> PyVal
into text m =
  let own = contentText (get "content" m)
   in set "content" (PStr (if own /= "" then text <> "\n\n" <> own else text)) m

-- | Turns: a group (an assistant with tool calls and the tool messages
-- right after it that answer them), or a single message; as index ranges.
units :: [PyVal] -> [(Int, Int)]
units ms = go 0
  where
    n = length ms
    go i
      | i >= n = []
      | otherwise =
          let m = ms !! i
              calls = if role m == "assistant" then toolCallsOf m else []
           in if null calls
                then (i, i + 1) : go (i + 1)
                else
                  let ids = mapMaybe callId calls
                      j = i + 1 + length (takeWhile (\x -> role x == "tool" && maybe False (`elem` ids) (get "tool_call_id" x)) (drop (i + 1) ms))
                   in (i, j) : go j

-- ---------------------------------------------------------------- read back (5)

data Block = Block
  { bId :: Maybe Int
  , bRole :: Text
  , bName :: Text
  , bLines :: [Text]
  , bKept :: Bool
  }

data Kind = Flat | Plain | Structured
  deriving (Eq)

data Item = Item
  { iKind :: Kind
  , iMsg :: PyVal
  , iBlock :: BlockInfo
  , iId :: Int
  -- ^ for plain and structured
  }

data BlockInfo = BlockInfo {biId :: Maybe Int, biKnown :: Bool, biKept :: Bool}

-- | @read_back@: the editable turns, the id counter, the ids the model has
-- seen in the file, and the file (Nothing: missing).
readBack :: Text -> [Turn] -> Int -> [Int] -> Maybe BS.ByteString -> ReadBack
readBack _ _ _ _ Nothing = Missing
readBack sid editable counter0 seenL (Just bytes) = case TE.decodeUtf8' bytes of
  Left _ -> NotUtf8
  Right text0 ->
    let original = M.fromList [(i, m) | Turn i m <- editable]
        position = M.fromList (zip (map turnId editable) [0 :: Int ..])
        seen = S.fromList seenL
        -- rule 1
        text =
          if T.any (== '\n') text0 && T.count "\r\n" text0 == T.count "\n" text0
            then T.replace "\r\n" "\n" text0
            else text0
        ls0 = T.splitOn "\n" text
        ls = if "\n" `T.isSuffixOf` text then init ls0 else ls0
        -- rule 2
        ban = banner sid
        (preLines, blocks0) = splitBlocks ban ls
        preBlock = [Block Nothing "user" "" preLines False | not (null preLines), trim (T.intercalate "\n" preLines) /= ""]
        fileBlocks = preBlock <> blocks0
        -- rule 3
        present = S.fromList [i | Block (Just i) _ _ _ _ <- fileBlocks]
        names = toolNames editable
        msgs = map turnMsg editable
        keptUnits =
          [ unit
          | (a, b) <- units msgs
          , let unit = take (b - a) (drop a editable)
          , all (\(Turn i _) -> not (S.member i seen) && not (S.member i present)) unit ]
        keptBlocks = [Block (Just i) (role m) (headerName names m) (T.splitOn "\n" (bodyOf m)) True | unit <- keptUnits, Turn i m <- unit]
        kept = length keptBlocks
        blocks = fileBlocks <> keptBlocks
        -- rules 4 and 5
        annotated = annotate S.empty blocks
        annotate _ [] = []
        annotate firsts (b : rest) =
          let first = maybe False (\i -> not (S.member i firsts)) (bId b)
              known = first && maybe False (`M.member` original) (bId b)
              t = trim (T.intercalate "\n" (bLines b))
              unchanged = known && maybe False (\i -> let o = original M.! i in role o == bRole b && t == trim (bodyOf o)) (bId b)
              firsts' = maybe firsts (`S.insert` firsts) (bId b)
           in (b, t, first, known, unchanged) : annotate firsts' rest
        -- rule 6
        (member, groups) = groupsOf editable
        held =
          S.fromList
            [ gid
            | (gid, members) <- groups
            , (k : _) <- [[k | (k, (b, _, first, _, _)) <- zip [0 :: Int ..] annotated, first, bId b == Just gid]]
            , let sq = take (length members) (drop k annotated)
            , length sq == length members
            , and [unchanged && bId b == Just x | ((b, _, _, _, unchanged), x) <- zip sq members] ]
        -- rule 7
        items =
          concat
            [ case () of
                _
                  | unchanged
                  , Just i <- bId b
                  , o <- original M.! i
                  , Nothing <- M.lookup i member
                  , role o /= "tool" ->
                      [Item Plain o info i]
                  | unchanged
                  , Just i <- bId b
                  , Just g <- M.lookup i member
                  , S.member g held ->
                      [Item Structured (original M.! i) info i]
                  | otherwise -> maybe [] (\m -> [Item Flat m info 0]) (flatMsg b t)
            | (b, t, _, known, unchanged) <- annotated
            , let info = BlockInfo (bId b) known (bKept b) ]
        -- rule 8
        merged = foldl' mergeFlat [] items
        mergeFlat acc it = case acc of
          (prev : rest)
            | iKind it == Flat, iKind prev == Flat, role (iMsg prev) == role (iMsg it) ->
                prev {iMsg = PDict [("role", PStr (role (iMsg prev))), ("content", PStr (field "content" (iMsg prev) <> "\n\n" <> field "content" (iMsg it)))]} : rest
          _ -> it : acc
        -- rule 9
        newlyBefore a b = case iKind a of
          Flat -> True
          _ ->
            let k = position M.! iId a
             in not (k + 1 < length editable && turnId (editable !! (k + 1)) == iId b)
        ruled = foldl' rule9 [] (reverse merged)
        rule9 acc it = case acc of
          (a : rest)
            | iKind a `elem` [Flat, Plain]
            , plainAnswer (iMsg a)
            , iKind it == Structured
            , role (iMsg it) == "assistant"
            , newlyBefore a it ->
                it {iMsg = into (contentText (get "content" (iMsg a))) (iMsg it)} : rest
          _ -> it : acc
        final = reverse ruled
        -- rule 10
        (result, counter, written) = foldl' giveId ([], counter0, []) final
        giveId (rs, c, w) it =
          let (nid, c') = case iKind it of
                Flat | biKnown (iBlock it), Just i <- biId (iBlock it) -> (i, c)
                Flat -> (c, c + 1)
                _ -> (iId it, c)
           in (rs <> [Turn nid (iMsg it)], c', if biKept (iBlock it) then w else w <> [nid])
     in if result == editable
          then NoEdit kept
          else
            Edited
              Edit
                { editTurns = result, editNext = counter, editKept = kept
                , editMergeIntoLive = case reverse result of
                    (Turn i m : _) -> plainAnswer m && (null editable || turnId (last editable) /= i)
                    [] -> False
                , editWritten = written }

splitBlocks :: [Text] -> [Text] -> ([Text], [Block])
splitBlocks ban = go [] []
  where
    go pre acc [] = (reverse pre, reverse (map finishB acc))
    go pre acc (l : rest) = case parseHeader l of
      Just (i, r, n) -> go pre (Block (Just i) r n [] False : acc) rest
      Nothing -> case acc of
        [] ->
          if T.dropWhileEnd (== '\r') l `elem` ban
            then go pre acc rest
            else go (unescapeLine l : pre) acc rest
        (b : bs) -> go pre (b {bLines = unescapeLine l : bLines b} : bs) rest
    finishB b = b {bLines = reverse (bLines b)}

flatMsg :: Block -> Text -> Maybe PyVal
flatMsg b t
  | T.null t = Nothing
  | bRole b == "assistant" = Just (PDict [("role", PStr "assistant"), ("content", PStr t)])
  | bRole b == "user" = Just (PDict [("role", PStr "user"), ("content", PStr t)])
  | otherwise =
      let hd = if T.null (bName b) then "Tool result:" else "Result of " <> bName b <> ":"
       in Just (PDict [("role", PStr "user"), ("content", PStr (hd <> "\n" <> t))])

-- | Groups: member id -> group id, and each group's ids in order.
groupsOf :: [Turn] -> (M.Map Int Int, [(Int, [Int])])
groupsOf ts =
  let ms = map turnMsg ts
      gs =
        [ (turnId (ts !! a), map turnId (take (b - a) (drop a ts)))
        | (a, b) <- units ms
        , let m = ms !! a
        , role m == "assistant"
        , not (null (toolCallsOf m)) ]
   in (M.fromList [(x, g) | (g, xs) <- gs, x <- xs], gs)
