-- | A goal's shape key: coarse features of a statement's text that group
-- goals for the learned search policy (the run log's @goals.shape@).
--
-- 'shapeOf' reads the text 'Fisher.Search.Lean.dStatement' gives
-- (@theorem foo binders : TYPE := by@). It is total: any text, a partial
-- declaration or the empty string, gives a shape.
--
-- The type is the text after the first top-level @:@ (outside the
-- balanced binder groups @( ) { } [ ] ⦃ ⦄ ⟨ ⟩@) up to the first top-level
-- @:=@; with no top-level @:@ the whole text is the type. From it:
--
-- * the connective: leading @∀ ... ,@ binders and @A →@ hypotheses are
--   stripped (on the last top-level arrow), then a leading @∃@ gives
--   @exists@, else the first of @↔ = ≠ ≤ < ∧ ∨ ∈@ (in that order) that
--   occurs at top level (@≥@ counts as @≤@ and @>@ as @<@), else
--   @other@. A binder (@∀ ∃ fun λ@) at depth zero
--   after the start reaches to the end, so what follows it is not top level;
-- * the heads: the first three distinct dotted identifiers whose first
--   segment starts with an uppercase letter (Mathlib-style constants such
--   as @Finset.sum@; a projection like @g.store@ or @x.1@ is not one), in
--   order of appearance;
-- * the namespaces: the sorted distinct first segments of every such
--   identifier in the type;
-- * the size: by whitespace-separated words of the type, under 20
--   @small@, at most 60 @medium@, else @large@.
module Fisher.Search.Shape
  ( Shape (..)
  , shapeOf
  , renderShape
  , parseShape
  ) where

import Data.Char (isAlpha, isAlphaNum, isSpace, isUpper)
import Data.List (nub, sort)
import Data.Text (Text)
import qualified Data.Text as T

data Shape = Shape
  { shConnective :: Text
  , shHeads :: [Text]
  , shNamespaces :: [Text]
  , shSize :: Text
  }
  deriving (Eq, Ord, Show)

-- | The canonical string, @le|Finset.sum,Nat.choose|Finset,Nat|medium@.
renderShape :: Shape -> Text
renderShape (Shape c hs ns s) = T.intercalate "|" [c, T.intercalate "," hs, T.intercalate "," ns, s]

-- | The inverse of 'renderShape' on its image; Nothing for anything else.
parseShape :: Text -> Maybe Shape
parseShape t = case T.splitOn "|" t of
  [c, hs, ns, s]
    | c `elem` connectives, s `elem` ["small", "medium", "large"] -> Just (Shape c (list hs) (list ns) s)
  _ -> Nothing
  where
    list x = if T.null x then [] else T.splitOn "," x

connectives :: [Text]
connectives = ["eq", "ne", "le", "lt", "iff", "exists", "and", "or", "mem", "other"]

shapeOf :: Text -> Shape
shapeOf stmt =
  let ty = typeTokens (depths (tokenize stmt))
      tyText = case (ty, reverse ty) of
        ((_, o0, _) : _, (_, o1, l1) : _) -> T.take (o1 + l1 - o0) (T.drop o0 stmt)
        _ -> ""
      dotted = [x | (Tok Ident x, _, _) <- ty, T.any (== '.') x, maybe False (isUpper . fst) (T.uncons x)]
      heads = take 3 (nub dotted)
      spaces = sort (nub [T.takeWhile (/= '.') x | x <- dotted])
      n = length (T.words tyText)
      size
        | n < 20 = "small"
        | n <= 60 = "medium"
        | otherwise = "large"
   in Shape (connective (strip (rebase ty))) heads spaces size

-- * Tokens

data Kind = Ident | Sym | Lit
  deriving (Eq, Show)

data Tok = Tok Kind Text
  deriving (Eq, Show)

-- | A token, its offset in the text and its length.
type Located = (Tok, Int, Int)

-- | Identifiers, string literals, and symbols (the multi-character
-- operators below as one token, else one character). Comments and
-- whitespace are dropped.
tokenize :: Text -> [Located]
tokenize = go 0
  where
    go !o s = case T.uncons s of
      Nothing -> []
      Just (c, rest)
        | isSpace c -> go (o + 1) rest
        | "--" `T.isPrefixOf` s ->
            let (cm, s') = T.break (== '\n') s
             in go (o + T.length cm) s'
        | "/-" `T.isPrefixOf` s ->
            let n = blockComment 0 0 s
             in go (o + n) (T.drop n s)
        | c == '"' ->
            let n = stringLit 1 rest
             in (Tok Lit (T.take n s), o, n) : go (o + n) (T.drop n s)
        | identStart c ->
            let n = identLen s
             in (Tok Ident (T.take n s), o, n) : go (o + n) (T.drop n s)
        | Just op <- multi s ->
            let n = T.length op
             in (Tok Sym op, o, n) : go (o + n) (T.drop n s)
        | otherwise -> (Tok Sym (T.singleton c), o, 1) : go (o + 1) rest
    multi s = case filter (`T.isPrefixOf` s) operators of
      op : _ -> Just op
      [] -> Nothing
    -- nested block comments; the length consumed, to the end when unclosed
    blockComment :: Int -> Int -> Text -> Int
    blockComment !d !n s
      | T.null s = n
      | "/-" `T.isPrefixOf` s = blockComment (d + 1) (n + 2) (T.drop 2 s)
      | "-/" `T.isPrefixOf` s = if d <= 1 then n + 2 else blockComment (d - 1) (n + 2) (T.drop 2 s)
      | otherwise = blockComment d (n + 1) (T.drop 1 s)
    -- the length of a string literal from after its opening quote
    stringLit :: Int -> Text -> Int
    stringLit !n s = case T.uncons s of
      Nothing -> n
      Just ('"', _) -> n + 1
      Just ('\\', s') -> case T.uncons s' of
        Nothing -> n + 1
        Just (_, s'') -> stringLit (n + 2) s''
      Just (_, s') -> stringLit (n + 1) s'

-- | Longest first, so that @<->@ wins over @<-@ and @<@.
operators :: [Text]
operators = ["<->", ":=", "::", "<=", ">=", "->", "=>", "!=", "==", "/\\", "\\/", "\8707!"]

identStart :: Char -> Bool
identStart c = (isAlpha c && c /= '\955') || c == '_'

identChar :: Char -> Bool
identChar c = (isAlphaNum c && c /= '\955') || c `elem` ("_'!?" :: String)

-- | An identifier's length: identifier characters, and a @.@ when another
-- identifier character follows it (so @Finset.sum@ and @h.1@ are one,
-- and a trailing dot is not part of it).
identLen :: Text -> Int
identLen = go 0
  where
    go !n s = case T.uncons s of
      Just (c, rest)
        | identChar c -> go (n + 1) rest
        | c == '.', Just (d, _) <- T.uncons rest, identChar d -> go (n + 1) rest
      _ -> n

-- * Structure

opens, closes :: [Text]
opens = ["(", "{", "[", "\10627", "\10216"]
closes = [")", "}", "]", "\10628", "\10217"]

-- | Each token with the bracket depth it sits at (an opening or closing
-- bracket is at the outer depth); never below zero.
depths :: [Located] -> [(Located, Int)]
depths = go 0
  where
    go _ [] = []
    go !d (l@(Tok k x, _, _) : ls)
      | k == Sym, x `elem` opens = (l, d) : go (d + 1) ls
      | k == Sym, x `elem` closes = let d' = max 0 (d - 1) in (l, d') : go d' ls
      | otherwise = (l, d) : go d ls

-- | The type's tokens with their depths.
typeTokens :: [(Located, Int)] -> [Located]
typeTokens ts =
  let afterColon = case break (\(l, d) -> d == 0 && sym ":" l) ts of
        (_, _ : rest) -> rest
        (whole, []) -> whole
      body = takeWhile (\(l, d) -> not (d == 0 && sym ":=" l)) afterColon
   in map fst body

sym :: Text -> Located -> Bool
sym x (Tok Sym y, _, _) = x == y
sym _ _ = False

-- | A simplified token for the connective: its text and its depth
-- relative to the type's start.
type Item = (Text, Int)

rebase :: [Located] -> [Item]
rebase ls = [(x, d) | ((Tok _ x, _, _), d) <- depths ls]

isBinder :: Text -> Bool
isBinder x = x `elem` ["\8704", "forall", "\8707", "\8707!", "exists", "fun", "\955", "\928"]

-- | A binder at depth zero, whose body reaches to the end of the type (a
-- binder inside brackets ends with its bracket).
binderAt0 :: Item -> Bool
binderAt0 (y, d) = d == 0 && isBinder y

isArrow :: Text -> Bool
isArrow x = x `elem` ["\8594", "->"]

-- | The top-level part of a type: its depth-zero items before any
-- depth-zero binder after the first.
topLevel :: [Item] -> [Text]
topLevel [] = []
topLevel ((x, d) : rest) = [x | d == 0] ++ [y | (y, 0) <- takeWhile (not . binderAt0) rest]

-- | Strip leading universal binders and hypotheses (not under a leading
-- @∃@ or @fun@, whose body reaches to the end).
strip :: [Item] -> [Item]
strip items = case items of
  (x, _) : _ | x `elem` ["\8707", "\8707!", "exists", "fun", "\955"] -> items
  (x, 0) : rest
    | x `elem` ["\8704", "forall", "\928"] -> case break (\(y, d) -> d == 0 && y == ",") rest of
        (_, _ : body) -> strip body
        (_, []) -> []
  _ -> case lastArrow items of
    Just i -> strip (drop (i + 1) items)
    Nothing -> items

-- | The index of the last top-level arrow, before any depth-zero binder
-- after the first.
lastArrow :: [Item] -> Maybe Int
lastArrow items =
  let scope = case items of
        [] -> []
        i0 : rest -> (0, i0) : takeWhile (not . binderAt0 . snd) (zip [1 ..] rest)
      hits = [i | (i, (x, 0)) <- scope, isArrow x]
   in if null hits then Nothing else Just (last hits)

connective :: [Item] -> Text
connective items = case items of
  (x, 0) : _ | x `elem` ["\8707", "\8707!", "exists"] -> "exists"
  _ ->
    let top = topLevel items
        pick = [name | (ops, name) <- table, any (`elem` top) ops]
     in case pick of
          name : _ -> name
          [] -> "other"
  where
    table =
      [ (["\8596", "<->"], "iff")
      , (["="], "eq")
      , (["\8800", "!="], "ne")
      , (["\8804", "<=", "\8805", ">="], "le")
      , (["<", ">"], "lt")
      , (["\8743", "/\\"], "and")
      , (["\8744", "\\/"], "or")
      , (["\8712"], "mem")
      ]
