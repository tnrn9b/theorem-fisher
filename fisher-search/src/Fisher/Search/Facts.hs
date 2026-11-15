{-# LANGUAGE MultiWayIf #-}

-- | The project facts offered for one goal: the project constants its
-- @pp.all@ type uses, the project constants their statements use (one
-- hop), then BM25 keyword matches against the goal's statement. Only
-- declarations the goal's file can see are offered (the same file above
-- the goal, or a module in the file's import closure; a private one only
-- in its own file), in that order, deduplicated and capped.
module Fisher.Search.Facts
  ( FactsConfig (..)
  , defaultFacts
  , headerOnly
  , Fact (..)
  , ppConstants
  , factsFor
  , factsBrief
  , factLine
  , goalModule
  ) where

import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.Containers.ListUtils (nubOrd)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (joinPath, normalise, splitDirectories)

import Fisher.Role (PromptPart, source, static)
import Fisher.Search.Bm25 (corpus, rank, tokens)
import Fisher.Search.Index

data FactsConfig = FactsConfig
  { fcMax :: Int
  -- ^ at most this many facts
  , fcChars :: Int
  -- ^ at most this many characters of rendered fact lines ('factLine',
  -- newlines included; the header is not counted)
  , fcKeywordRatio :: Double
  -- ^ a keyword fact is kept only when its BM25 score is at least this
  -- fraction of the best visible keyword score
  , fcHeaderMax :: Int
  -- ^ at most this many facts of the header-only kinds ('headerOnly':
  -- structure, inductive, class, abbrev), across all tiers; the slots
  -- they would take go to the next facts in order
  }
  deriving (Eq, Show)

-- | 20 entries, 8,000 characters, keyword scores at 0.4 of the best, at
-- most 5 header-only entries.
defaultFacts :: FactsConfig
defaultFacts = FactsConfig {fcMax = 20, fcChars = 8000, fcKeywordRatio = 0.4, fcHeaderMax = 5}

-- | Kinds whose statement is only a header (the name and its
-- parameters), so a fact of them says little on its own.
headerOnly :: Kind -> Bool
headerOnly k = k `elem` [KStructure, KInductive, KClass, KAbbrev]

data Fact = Fact
  { fEntry :: Entry
  , fWhy :: Text
  -- ^ "uses", "one hop" or "matches"
  }
  deriving (Eq, Show)

-- | The identifiers of a @pp.all@ type in order of first mention, with a
-- leading @\@@ and universe levels (@.{u}@) stripped. A dotted name stops
-- before its first numeric component (@x.1@ gives @x@), except Lean's
-- private names (@_private.M.0.n@), which are kept whole. Names bound by
-- a binder in the type (@(x y : T)@, @{α : Type}@, @[inst : C]@,
-- @⦃h : P⦄@, @let x : T := v@) are left out wherever they occur: in
-- @pp.all@ output a constant is never written as a bare bound name.
ppConstants :: Text -> [Text]
ppConstants ty = nubOrd [i | i <- ids, Set.notMember i bound]
  where
    toks = lexPp (stripUniverses ty)
    ids = mapMaybe ident toks
    bound = Set.fromList (binders toks)

-- Tokens of a pp.all type: identifiers (with their '@' if written) and
-- single punctuation characters; whitespace dropped.
data Tok = Ident Text | Punct Char
  deriving (Eq, Show)

lexPp :: Text -> [Tok]
lexPp t = case T.uncons t of
  Nothing -> []
  Just (c, rest)
    | isSpace c -> lexPp rest
    | c == '«' ->
        let (body, after) = T.breakOn "»" rest
         in Ident ("«" <> body <> "»") : lexPp (T.drop 1 after)
    | c == '@' || identStart c ->
        let (w, after) = T.span identChar rest in Ident (T.cons c w) : lexPp after
    | otherwise -> Punct c : lexPp rest

identStart :: Char -> Bool
identStart c = isAlpha c || c == '_'

identChar :: Char -> Bool
identChar c = isAlphaNum c || c `elem` ("_'!?.«»" :: String)

-- | Remove every @.{...}@ (universe levels never hold braces).
stripUniverses :: Text -> Text
stripUniverses t = case T.breakOn ".{" t of
  (before, after)
    | T.null after -> before
    | otherwise -> before <> stripUniverses (T.drop 1 (T.dropWhile (/= '}') after))

-- | The constant an identifier token names, if any.
ident :: Tok -> Maybe Text
ident = \case
  Punct _ -> Nothing
  Ident w0 ->
    let w = T.dropWhileEnd (== '.') (T.dropWhile (== '@') w0)
     in if
          | T.null w -> Nothing
          | "_private." `T.isPrefixOf` w -> Just w
          | otherwise ->
              let comps = takeWhile okComp (T.splitOn "." w)
               in if null comps then Nothing else Just (T.intercalate "." comps)
  where
    okComp c = maybe False (\(h, _) -> identStart h || h == '«') (T.uncons c)

-- | Names bound by binders: a run of plain identifiers right after an
-- opening bracket or @let@/@have@ and followed by a lone @:@.
binders :: [Tok] -> [Text]
binders = \case
  [] -> []
  (opener : rest)
    | opens opener ->
        let (run, after) = span plain rest
         in case after of
              (Punct ':' : more) | not (startsAssign more), not (null run) -> mapMaybe ident run <> binders after
              _ -> binders rest
    | otherwise -> binders rest
  where
    opens = \case
      Punct c -> c `elem` ("({[⦃" :: String)
      Ident w -> w `elem` ["let", "have"]
    plain = \case
      Ident w -> not ("@" `T.isPrefixOf` w) && not ("." `T.isInfixOf` w)
      Punct _ -> False
    startsAssign = \case
      (Punct '=' : _) -> True
      _ -> False

-- | The facts for a goal, from the index, the goal's module when the
-- caller knows it (@moduleOf srcRoot file@; used exactly, and a module
-- not in the index gets no facts), the goal's file (relative to the
-- package root, like 'eFile', or absolute; used to find the module when
-- none is given, see 'goalModule'), the 1-based line of its
-- keyword, its statement as written (the BM25 query) and its @pp.all@
-- type when one was printed. A file whose module is not in the index
-- gets no facts. Partially applied to the index, the BM25 corpus is
-- built once.
factsFor :: FactsConfig -> Index -> Maybe Text -> FilePath -> Int -> Text -> Maybe Text -> [Fact]
factsFor cfg ix = go
  where
    entries = M.fromList (zip [0 :: Int ..] (ixEntries ix))
    corp = corpus [(i, tokens (eName e <> " " <> eStatement e)) | (i, e) <- M.toList entries]
    go mmod file line stmt mty = case maybe (goalModule ix file) known mmod of
      Nothing -> []
      Just gm ->
        let closure = importClosure ix gm
            visible e
              | eModule e == gm = eLine e < line
              | otherwise = not (ePrivate e) && Set.member (eModule e) closure
            byKey k = [(i, e) | Just i <- [M.lookup k (ixByName ix)], Just e <- [M.lookup i entries], visible e]
            direct = nubOn fst (concatMap byKey (maybe [] ppConstants mty))
            hop = nubOn fst (concatMap (concatMap byKey . eUses . snd) direct)
            query = filter (`Set.notMember` declWords) (tokens stmt)
            kw0 = [(sc, (i, e)) | (i, sc) <- rank corp query, Just e <- [M.lookup i entries], visible e]
            kw = case kw0 of
              [] -> []
              ((top, _) : _) -> [p | (sc, p) <- kw0, sc >= fcKeywordRatio cfg * top]
            ordered =
              nubOn (fst . fst) $
                [(p, "uses") | p <- direct] <> [(p, "one hop") | p <- hop] <> [(p, "matches") | p <- kw]
         in capped cfg [Fact e why | ((_, e), why) <- ordered]
    known m = if M.member m (ixImports ix) then Just m else Nothing

-- | Declaration keywords and modifiers: every goal statement starts with
-- one, so they would match every declaration of that kind.
declWords :: Set.Set Text
declWords =
  Set.fromList
    ( concatMap (tokens . kindText) [minBound .. maxBound]
        <> ["private", "protected", "noncomputable", "nonrec", "partial", "unsafe", "scoped", "local"]
    )

nubOn :: Ord b => (a -> b) -> [a] -> [a]
nubOn f = go Set.empty
  where
    go _ [] = []
    go seen (x : xs)
      | Set.member (f x) seen = go seen xs
      | otherwise = x : go (Set.insert (f x) seen) xs

-- | Keep facts in order while both caps hold; the first that would break
-- one ends the list, so whole entries are dropped. A header-only fact
-- past 'fcHeaderMax' of them is skipped and the next one considered.
capped :: FactsConfig -> [Fact] -> [Fact]
capped cfg = go 0 0 0
  where
    go :: Int -> Int -> Int -> [Fact] -> [Fact]
    go _ _ _ [] = []
    go n heads chars (f : fs)
      | n >= fcMax cfg = []
      | header && heads >= fcHeaderMax cfg = go n heads chars fs
      | chars' > fcChars cfg = []
      | otherwise = f : go (n + 1) (if header then heads + 1 else heads) chars' fs
      where
        header = headerOnly (eKind (fEntry f))
        chars' = chars + T.length (factLine f) + 1

-- | The module of the goal's file when the caller gave none: the module
-- of an entry whose 'eFile' is exactly the file (relative to the package
-- root), else, as a fallback, the indexed module whose path (dots as
-- directories, plus @.lean@) is the longest trailing part of the file's
-- path.
goalModule :: Index -> FilePath -> Maybe Text
goalModule ix file =
  case [eModule e | e <- ixEntries ix, splitDirectories (normalise (eFile e)) == dirs] of
    (m : _) -> Just m
    [] ->
      listToMaybe
        (sortOn (negate . length . T.splitOn ".") [m | m <- M.keys (ixImports ix), modPath m `suffixOf` dirs])
  where
    dirs = filter (`notElem` [".", "/"]) (splitDirectories (normalise file))
    modPath m = splitDirectories (joinPath (map T.unpack (T.splitOn "." m)) <> ".lean")
    suffixOf xs ys = length xs <= length ys && drop (length ys - length xs) ys == xs

-- | One fact: @- name : statement (file:line)@, the statement on one line
-- (whitespace runs collapsed) and without its trailing @:=@.
factLine :: Fact -> Text
factLine (Fact e _) = "- " <> eName e <> " : " <> oneLine (eStatement e) <> " (" <> T.pack (eFile e) <> ":" <> T.pack (show (eLine e)) <> ")"
  where
    oneLine s =
      let w = T.unwords (T.words s)
       in maybe w T.stripEnd (T.stripSuffix ":=" w)

-- | The facts section for a brief: empty when there are none. The facts
-- are the project's (source), the heading Fisher's.
factsBrief :: [Fact] -> PromptPart
factsBrief [] = mempty
factsBrief fs = static "Relevant project facts:\n" <> mconcat [source (factLine f) <> static "\n" | f <- fs]
