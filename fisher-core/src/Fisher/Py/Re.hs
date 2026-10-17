-- | A backtracking engine for the subset of Python's @re@ syntax that
-- upstream's patterns use, with Python's matching order: leftmost match,
-- alternatives tried left to right, greedy and lazy quantifiers, capture
-- groups numbered by their opening parenthesis, backreferences, fixed-width
-- lookbehind, lookahead, and @re.IGNORECASE@. Substitution follows Python
-- 3.7+: an empty match is allowed right after a non-empty one, but never
-- twice at the same position.
--
-- @re.DOTALL@ too. Not supported (no upstream pattern needs them): named
-- groups, inline flags, MULTILINE, VERBOSE, conditional groups, possessive
-- quantifiers.
module Fisher.Py.Re
  ( Regex
  , Flag (..)
  , Match (..)
  , compile
  , compileOrDie
  , search
  , searchFrom
  , matchStart
  , fullMatch
  , findAll
  , sub
  , subText
  , group
  , groupOr
  , groupCount
  ) where

import Data.Char (isAlphaNum, isDigit, isSpace, toLower, toUpper)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U

data Flag = IgnoreCase | DotAll
  deriving (Eq, Show)

data Item = IChar Char | IRange Char Char | IClass (Char -> Bool)

data Node
  = NChar Char
  | NAny
  | NSet Bool [Item]
  | NStart
  | NEnd
  | NWordB Bool
  | NGroup (Maybe Int) Node
  | NSeq [Node]
  | NAlt [Node]
  | NRep Int (Maybe Int) Bool Node
  | NLook Bool Bool Node -- ahead?, negated?
  | NBack Int

data Regex = Regex
  { rxNode :: Node
  , rxGroups :: Int
  , rxIcase :: Bool
  , rxFirst :: Maybe (Char -> Bool)
  , rxSource :: Text
  }

instance Show Regex where
  show r = "Regex " <> show (rxSource r)

data Match = Match
  { mStart :: !Int
  , mEnd :: !Int
  , mGroups :: IM.IntMap (Int, Int)
  , mSubject :: Text
  }

-- | Group @n@ (0 is the whole match); 'Nothing' when it did not take part.
group :: Int -> Match -> Maybe Text
group 0 m = Just (slice (mSubject m) (mStart m) (mEnd m))
group n m = (\(s, e) -> slice (mSubject m) s e) <$> IM.lookup n (mGroups m)

groupOr :: Text -> Int -> Match -> Text
groupOr d n m = fromMaybe d (group n m)

-- | Number of capture groups in the pattern.
groupCount :: Regex -> Int
groupCount = rxGroups

slice :: Text -> Int -> Int -> Text
slice t s e = T.take (e - s) (T.drop s t)

compileOrDie :: [Flag] -> Text -> Regex
compileOrDie fl src = either (\e -> error ("Fisher.Py.Re: " <> e <> " in " <> show src)) id (compile fl src)

compile :: [Flag] -> Text -> Either String Regex
compile fl src = do
  (node, rest, n) <- parseAlt (T.unpack src) 1
  case rest of
    [] -> pure ()
    (c : _) -> Left ("unexpected " <> show c)
  let ic = IgnoreCase `elem` fl
      node' = if DotAll `elem` fl then dotAll node else node
  pure (Regex node' (n - 1) ic (firstSet ic node') src)

-- | @re.DOTALL@: @.@ matches a newline too (as the empty negated set).
dotAll :: Node -> Node
dotAll = \case
  NAny -> NSet True []
  NGroup g x -> NGroup g (dotAll x)
  NSeq xs -> NSeq (map dotAll xs)
  NAlt xs -> NAlt (map dotAll xs)
  NRep lo hi lazy x -> NRep lo hi lazy (dotAll x)
  NLook a ng x -> NLook a ng (dotAll x)
  x -> x

-- Parser ------------------------------------------------------------------

type P a = String -> Int -> Either String (a, String, Int)

parseAlt :: P Node
parseAlt s n = do
  (first, rest, n1) <- parseSeq s n
  goAlt [first] rest n1
  where
    goAlt acc ('|' : r) k = do
      (nxt, r', k') <- parseSeq r k
      goAlt (nxt : acc) r' k'
    goAlt [one] r k = pure (one, r, k)
    goAlt acc r k = pure (NAlt (reverse acc), r, k)

parseSeq :: P Node
parseSeq = go []
  where
    go acc s n = case s of
      [] -> done acc s n
      ('|' : _) -> done acc s n
      (')' : _) -> done acc s n
      _ -> do
        (atom, r, n') <- parseAtom s n
        (q, r', n'') <- parseQuant atom r n'
        go (q : acc) r' n''
    done [one] s n = pure (one, s, n)
    done acc s n = pure (NSeq (reverse acc), s, n)

parseQuant :: Node -> P Node
parseQuant atom s n = case s of
  ('*' : r) -> lazy 0 Nothing r
  ('+' : r) -> lazy 1 Nothing r
  ('?' : r) -> lazy 0 (Just 1) r
  ('{' : r) | Just (lo, hi, r') <- braces r -> lazy lo hi r'
  _ -> pure (atom, s, n)
  where
    lazy lo hi ('?' : r) = pure (NRep lo hi False atom, r, n)
    lazy lo hi r = pure (NRep lo hi True atom, r, n)
    braces r =
      let (a, r1) = span isDigit r
       in case r1 of
            ('}' : r2) | not (null a) -> Just (read a, Just (read a), r2)
            (',' : r2) ->
              let (b, r3) = span isDigit r2
               in case r3 of
                    ('}' : r4) -> Just (if null a then 0 else read a, if null b then Nothing else Just (read b), r4)
                    _ -> Nothing
            _ -> Nothing

parseAtom :: P Node
parseAtom s n = case s of
  ('(' : '?' : ':' : r) -> grp Nothing r n
  ('(' : '?' : '=' : r) -> look True False r
  ('(' : '?' : '!' : r) -> look True True r
  ('(' : '?' : '<' : '=' : r) -> look False False r
  ('(' : '?' : '<' : '!' : r) -> look False True r
  ('(' : '?' : _) -> Left "unsupported group syntax"
  ('(' : r) -> grp (Just n) r (n + 1)
  ('[' : r) -> parseClass r n
  ('.' : r) -> pure (NAny, r, n)
  ('^' : r) -> pure (NStart, r, n)
  ('$' : r) -> pure (NEnd, r, n)
  ('\\' : r) -> parseEscape r n
  (c : r) -> pure (NChar c, r, n)
  [] -> Left "unexpected end"
  where
    grp num r k = do
      (inner, r', k') <- parseAlt r k
      case r' of
        (')' : r'') -> pure (NGroup num inner, r'', k')
        _ -> Left "missing )"
    look ahead neg r = do
      (inner, r', k') <- parseAlt r n
      case r' of
        (')' : r'') -> pure (NLook ahead neg inner, r'', k')
        _ -> Left "missing )"

parseEscape :: P Node
parseEscape s n = case s of
  (c : r) | isDigit c && c /= '0' -> pure (NBack (read [c]), r, n)
  ('b' : r) -> pure (NWordB True, r, n)
  ('B' : r) -> pure (NWordB False, r, n)
  ('A' : r) -> pure (NStart, r, n)
  ('Z' : r) -> pure (NEnd, r, n) -- close enough for upstream's use (none)
  (c : r) -> case escClass c of
    Just item -> pure (NSet False [item], r, n)
    Nothing -> pure (NChar (escChar c), r, n)
  [] -> Left "trailing backslash"

escClass :: Char -> Maybe Item
escClass = \case
  's' -> Just (IClass pySpace)
  'S' -> Just (IClass (not . pySpace))
  'd' -> Just (IClass isDigit)
  'D' -> Just (IClass (not . isDigit))
  'w' -> Just (IClass isWord)
  'W' -> Just (IClass (not . isWord))
  _ -> Nothing

escChar :: Char -> Char
escChar = \case
  'n' -> '\n'
  't' -> '\t'
  'r' -> '\r'
  'f' -> '\f'
  'v' -> '\v'
  c -> c

-- | Python's str @\\s@: Unicode whitespace plus the ASCII separators.
pySpace :: Char -> Bool
pySpace c = isSpace c || c `elem` ("\x1c\x1d\x1e\x1f" :: String)

isWord :: Char -> Bool
isWord c = isAlphaNum c || c == '_'

parseClass :: P Node
parseClass s0 n = do
  let (neg, s1) = case s0 of
        ('^' : r) -> (True, r)
        r -> (False, r)
  -- a ']' straight after '[' or '[^' is a literal
  (items, rest) <- case s1 of
    (']' : r) -> items' [IChar ']'] r
    r -> items' [] r
  pure (NSet neg (reverse items), rest, n)
  where
    items' acc = \case
      (']' : r) -> Right (acc, r)
      [] -> Left "unterminated ["
      s -> do
        (lo, r) <- one s
        case (lo, r) of
          (Left c, '-' : r') | (h : _) <- r', h /= ']' -> do
            (hi, r'') <- one r'
            case hi of
              Left d -> items' (IRange c d : acc) r''
              Right it -> items' (it : IChar '-' : IChar c : acc) r''
          (Left c, _) -> items' (IChar c : acc) r
          (Right it, _) -> items' (it : acc) r
    one = \case
      ('\\' : c : r) -> pure (maybe (Left (escChar c)) Right (escClass c), r)
      (c : r) -> pure (Left c, r)
      [] -> Left "unterminated ["

-- Matching ----------------------------------------------------------------

type Caps = IM.IntMap (Int, Int)

data Env = Env {eV :: U.Vector Char, eLen :: !Int, eIc :: !Bool}

charEq :: Bool -> Char -> Char -> Bool
charEq ic a b = a == b || (ic && (toLower a == toLower b || toUpper a == toUpper b))

inSet :: Bool -> [Item] -> Char -> Bool
inSet ic items c = any hit items || (ic && (any (\i -> hitC i (toLower c)) items || any (\i -> hitC i (toUpper c)) items))
  where
    hit i = hitC i c
    hitC (IChar d) x = d == x
    hitC (IRange a b) x = a <= x && x <= b
    hitC (IClass f) x = f x

-- | Continuation-passing backtracking: @run env node pos caps k@ tries every way
-- @node@ can match at @pos@, in Python's preference order, calling @k@.
run :: Env -> Node -> Int -> Caps -> (Int -> Caps -> Maybe r) -> Maybe r
run env node i caps k = case node of
  NChar c
    | i < eLen env && charEq (eIc env) c (eV env U.! i) -> k (i + 1) caps
    | otherwise -> Nothing
  NAny
    | i < eLen env && eV env U.! i /= '\n' -> k (i + 1) caps
    | otherwise -> Nothing
  NSet neg items
    | i < eLen env && inSet (eIc env) items (eV env U.! i) /= neg -> k (i + 1) caps
    | otherwise -> Nothing
  NStart -> if i == 0 then k i caps else Nothing
  NEnd ->
    if i == eLen env || (i == eLen env - 1 && eV env U.! i == '\n')
      then k i caps
      else Nothing
  NWordB want ->
    let before = i > 0 && isWord (eV env U.! (i - 1))
        after = i < eLen env && isWord (eV env U.! i)
     in if (before /= after) == want then k i caps else Nothing
  NGroup Nothing inner -> run env inner i caps k
  NGroup (Just g) inner -> run env inner i caps (\j caps' -> k j (IM.insert g (i, j) caps'))
  NSeq nodes -> seqM nodes i caps
    where
      seqM [] j c = k j c
      seqM (x : xs) j c = run env x j c (\j' c' -> seqM xs j' c')
  NAlt alts -> firstJust [run env a i caps k | a <- alts]
  NRep lo hi greedy inner -> rep lo hi greedy inner 0 i caps
    where
      rep l h g x count j c =
        let canStop = count >= l
            canMore = maybe True (count <) h
            more = if canMore then run env x j c (\j' c' -> if j' == j && count >= l then Nothing else rep l h g x (count + 1) j' c') else Nothing
            stop = if canStop then k j c else Nothing
         in if g then orElse more stop else orElse stop more
  NLook True neg inner ->
    case run env inner i caps (\_ c' -> Just c') of
      Just c' | not neg -> k i c'
      Nothing | neg -> k i caps
      _ -> Nothing
  NLook False neg inner ->
    let w = width inner
        ok = case w of
          Just wd | i - wd >= 0 -> run env inner (i - wd) caps (\j c' -> if j == i then Just c' else Nothing)
          _ -> Nothing
     in case ok of
          Just c' | not neg -> k i c'
          Nothing | neg -> k i caps
          _ -> Nothing
  NBack g -> case IM.lookup g caps of
    Nothing -> Nothing
    Just (s, e) ->
      let len = e - s
          okAt = i + len <= eLen env && and [charEq (eIc env) (eV env U.! (s + d)) (eV env U.! (i + d)) | d <- [0 .. len - 1]]
       in if okAt then k (i + len) caps else Nothing

orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just a) _ = Just a
orElse Nothing b = b

firstJust :: [Maybe a] -> Maybe a
firstJust = foldr orElse Nothing

-- | Fixed width of a lookbehind body.
width :: Node -> Maybe Int
width = \case
  NChar _ -> Just 1
  NAny -> Just 1
  NSet _ _ -> Just 1
  NStart -> Just 0
  NEnd -> Just 0
  NWordB _ -> Just 0
  NGroup _ x -> width x
  NSeq xs -> sum <$> traverse width xs
  NAlt xs -> case traverse width xs of
    Just (w : ws) | all (== w) ws -> Just w
    _ -> Nothing
  NRep lo (Just hi) _ x | lo == hi -> (* lo) <$> width x
  NRep {} -> Nothing
  NLook {} -> Just 0
  NBack _ -> Nothing

-- | A predicate every match's first character satisfies, when the pattern
-- cannot match empty; used to skip start positions quickly.
firstSet :: Bool -> Node -> Maybe (Char -> Bool)
firstSet ic = go
  where
    go = \case
      NChar c -> Just (charEq ic c)
      NSet neg items -> Just (\x -> inSet ic items x /= neg)
      NAny -> Just (/= '\n')
      NGroup _ x -> go x
      NSeq xs -> goSeq xs
      NAlt xs -> do
        fs <- traverse go xs
        pure (\c -> any ($ c) fs)
      NRep lo _ _ x | lo >= 1 -> go x
      _ -> Nothing
    goSeq = \case
      [] -> Nothing
      (NLook {} : rest) -> goSeq rest
      (x : _) -> go x

envOf :: Regex -> Text -> Env
envOf rx t = let v = U.fromList (T.unpack t) in Env v (U.length v) (rxIcase rx)

-- | @re.search@ from position @from@ (character index).
searchFrom :: Regex -> Int -> Text -> Maybe Match
searchFrom rx from t = searchEnv rx (envOf rx t) t from False

searchEnv :: Regex -> Env -> Text -> Int -> Bool -> Maybe Match
searchEnv rx env t from mustAdvance = go from
  where
    n = eLen env
    go i
      | i > n = Nothing
      | Just f <- rxFirst rx, i < n, not (f (eV env U.! i)) = go (i + 1)
      | Just _ <- rxFirst rx, i >= n = Nothing
      | otherwise =
          case run env (rxNode rx) i IM.empty (\j caps -> if mustAdvance && i == from && j == i then Nothing else Just (j, caps)) of
            Just (j, caps) -> Just (Match i j caps t)
            Nothing -> go (i + 1)

search :: Regex -> Text -> Maybe Match
search rx = searchFrom rx 0

-- | @re.match@: anchored at the start, not at the end.
matchStart :: Regex -> Text -> Maybe Match
matchStart rx t =
  let env = envOf rx t
   in (\(j, caps) -> Match 0 j caps t) <$> run env (rxNode rx) 0 IM.empty (\j caps -> Just (j, caps))

-- | @re.fullmatch@.
fullMatch :: Regex -> Text -> Maybe Match
fullMatch rx t =
  let env = envOf rx t
   in (\(j, caps) -> Match 0 j caps t) <$> run env (rxNode rx) 0 IM.empty (\j caps -> if j == eLen env then Just (j, caps) else Nothing)

-- | Every non-overlapping match, left to right (@re.finditer@).
findAll :: Regex -> Text -> [Match]
findAll rx t = go 0 False
  where
    env = envOf rx t
    go pos adv = case searchEnv rx env t pos adv of
      Nothing -> []
      Just mt -> mt : go (mEnd mt) (mEnd mt == mStart mt)

-- | @re.sub(pattern, fn, text)@.
sub :: Regex -> (Match -> Text) -> Text -> Text
sub rx f t = T.concat (go 0 t (findAll rx t))
  where
    -- @remaining@ is the subject from character @pos@ on, so each piece is
    -- cut in time proportional to its own length.
    go _ remaining [] = [remaining]
    go pos remaining (mt : rest) =
      let (before, fromMatch) = T.splitAt (mStart mt - pos) remaining
          after = T.drop (mEnd mt - mStart mt) fromMatch
       in before : f mt : go (mEnd mt) after rest

-- | @re.sub@ with a fixed replacement (no group references).
subText :: Regex -> Text -> Text -> Text
subText rx r = sub rx (const r)
