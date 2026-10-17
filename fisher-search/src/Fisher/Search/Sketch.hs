-- | The planner's sketch for a split: new open lemmas followed by the
-- node's declaration with a proof that uses them. Parsing checks its shape,
-- applying merges it into the base above the node, and checking compiles it.
module Fisher.Search.Sketch
  ( Sketch (..)
  , SketchError (..)
  , parseSketch
  , applySketch
  , aliasLemma
  , checkSketch
  , checkSketchVerdict
  , typeKey
  , sketchErrorText
  , nsPrefix
  , qualify
  , removeDecls
  , mentions
  ) where

import Data.Char (isAlphaNum)
import Data.List (find)
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Search.Check
import Fisher.Search.Lean
import Fisher.Search.Splice (deniedLine)

data Sketch = Sketch
  { skLemmas :: [(Text, Text)]
  -- ^ (bare name, declaration text), in order
  , skNode :: Text
  -- ^ the node's declaration text, from its keyword line
  }
  deriving (Eq, Show)

data SketchError
  = NoNodeDecl
  | NodeStatementChanged
  | LemmaNotOpen Text
  | DuplicateName Text
  | NameClash Text
  | DeniedLine Text
  | ExtraText Text
  deriving (Eq, Show)

sketchErrorText :: SketchError -> Text
sketchErrorText = \case
  NoNodeDecl -> "the sketch has no declaration of the node"
  NodeStatementChanged -> "the node's statement was changed; it must stay exactly as it is"
  LemmaNotOpen n -> "the lemma " <> n <> " has a proof; each new lemma must end in `:= by sorry` or `:= sorry`"
  DuplicateName n -> "the name " <> n <> " is used for two lemmas"
  NameClash n -> "the name " <> n <> " is already in the file"
  DeniedLine l -> "a line is not allowed (axiom, #exit or an option that turns checks off): " <> l
  ExtraText l -> "text outside declarations: " <> l

lastComponent :: Text -> Text
lastComponent = last . T.splitOn "."

-- | Everything before the last dot-separated component, with its dot
-- ("" for a bare name).
nsPrefix :: Text -> Text
nsPrefix n = T.take (T.length n - T.length (lastComponent n)) n

qualify :: Text -> Text -> Text
qualify pre n
  | T.null pre || pre `T.isPrefixOf` n = n
  | otherwise = pre <> n

isOpenIn :: Text -> Bool
isOpenIn l = let t = T.strip l in "open" `T.isPrefixOf` t && " in" `T.isSuffixOf` t

parseSketch :: Text -> Text -> Text -> Either SketchError Sketch
parseSketch base nodeName block = do
  let ls = T.splitOn "\n" block
      ds = declarations block
      covered i = any (\d -> i >= dDocStart d && i < dEnd d) ds
  case filter deniedLine ls of
    (l : _) -> Left (DeniedLine (T.take 120 (T.strip l)))
    [] -> Right ()
  let loose = [(i, l) | (i, l) <- zip [0 :: Int ..] ls, not (covered i), not (T.null (T.strip l))]
      looseIx = map fst loose
      nextNonBlank i = find (\j -> not (T.null (T.strip (ls !! j)))) [i + 1 .. length ls - 1]
      -- an `open ... in` line must lead to a declaration (through more such lines)
      attached i = case nextNonBlank i of
        Nothing -> False
        Just j -> any ((== j) . dDocStart) ds || (j `elem` looseIx && isOpenIn (ls !! j) && attached j)
  case [l | (i, l) <- loose, not (isOpenIn l && attached i)] of
    (l : _) -> Left (ExtraText (T.take 120 (T.strip l)))
    [] -> Right ()
  let pre = nsPrefix nodeName
      q d = qualify pre (dName d)
  node <- maybe (Left NoNodeDecl) Right (find ((== nodeName) . q) ds)
  baseNode <- maybe (Left NoNodeDecl) Right (findDecl nodeName (declarations base))
  if statementHead node /= statementHead baseNode then Left NodeStatementChanged else Right ()
  let lemmas = [d | d <- ds, dStart d /= dStart node]
      baseNames = Set.fromList (map dName (declarations base))
  mapM_ (\d -> if isOpen d then Right () else Left (LemmaNotOpen (dName d))) lemmas
  let dupes = [dName d | (i, d) <- zip [0 :: Int ..] lemmas, q d `elem` map q (take i lemmas)]
  case dupes of
    (n : _) -> Left (DuplicateName n)
    [] -> Right ()
  case [q d | d <- lemmas, q d `Set.member` baseNames] of
    (n : _) -> Left (NameClash n)
    [] -> Right ()
  -- `open ... in` lines directly above a declaration travel with it
  let openAbove d = [l | (i, l) <- loose, i < dDocStart d, i >= prevEnd d]
      prevEnd d = maximum (0 : [dEnd e | e <- ds, dEnd e <= dDocStart d])
      textOf d = T.intercalate "\n" (openAbove d <> take (dEnd d - dDocStart d) (drop (dDocStart d) ls))
      nodeText = T.intercalate "\n" (openAbove node <> take (dEnd node - dStart node) (drop (dStart node) ls))
  Right (Sketch [(dName d, textOf d) | d <- lemmas] nodeText)

applySketch :: Text -> Text -> Sketch -> Either SketchError Text
applySketch base nodeName sk = do
  node <- maybe (Left NoNodeDecl) Right (findDecl nodeName (declarations base))
  let ls = T.splitOn "\n" base
      (basePrefix, _) = keywordSplit (T.intercalate "\n" (drop (dStart node) (take (dEnd node) ls)))
      skLs = T.splitOn "\n" (skNode sk)
      (opens, nodeLs) = span isOpenIn skLs
      (_, fromKeyword) = keywordSplit (T.intercalate "\n" nodeLs)
      nodeText = T.splitOn "\n" (basePrefix <> fromKeyword)
      replaced = take (dStart node) ls <> nodeText <> drop (dEnd node) ls
      insertion = concat [T.splitOn "\n" t <> [""] | (_, t) <- skLemmas sk] <> opens
  Right (T.intercalate "\n" (take (dDocStart node) replaced <> insertion <> drop (dDocStart node) replaced))

-- | A declaration's text split before its keyword: the attributes and
-- modifiers in front of it, and the rest.
keywordSplit :: Text -> (Text, Text)
keywordSplit t = case declarations t of
  (d : _) | dStart d == 0 -> T.breakOn (dStatement d) t
  _ -> ("", t)

-- | Make a lemma's proof @by exact target@, keeping its statement.
aliasLemma :: Text -> Text -> Sketch -> Sketch
aliasLemma name target sk = sk {skLemmas = map f (skLemmas sk)}
  where
    f (n, t)
      | n == name = (n, alias t)
      | otherwise = (n, t)
    alias t = case declarations t of
      (d : _) ->
        let ls = T.splitOn "\n" t
            (pre, _) = T.breakOn (dStatement d) (T.intercalate "\n" (drop (dStart d) (take (dStmtEnd d + 1) ls)))
         in T.intercalate "\n" (take (dStart d) ls <> [pre <> statementHead d <> " by exact " <> target])
      [] -> t

-- | The printed type without its @name :@ or @\@name :@ lead, the name
-- with or without a universe suffix @.{u_1}@.
typeKey :: Text -> Text -> Text
typeKey name ty =
  case [r | p <- [name, "@" <> name], Just r0 <- [T.stripPrefix p ty], Just r <- [T.stripPrefix " : " (dropUniverses r0)]] of
    (r : _) -> T.strip r
    [] -> T.strip ty
  where
    dropUniverses r = case T.stripPrefix ".{" r of
      Just u | (_, rest) <- T.breakOn "}" u, not (T.null rest) -> T.drop 1 rest
      _ -> r

-- | Remove the named declarations (fully qualified names) from a file, each
-- with its doc comment and attributes, the @open ... in@ lines directly
-- above it and one blank line after it. Names not in the file are ignored.
removeDecls :: [Text] -> Text -> Text
removeDecls names src
  | IS.null drop' = src
  | otherwise = T.intercalate "\n" [l | (i, l) <- zip [0 ..] ls, not (IS.member i drop')]
  where
    ls = T.splitOn "\n" src
    n = length ls
    blank i = i < n && T.null (T.strip (ls !! i))
    opensAbove i = if i > 0 && isOpenIn (ls !! (i - 1)) then opensAbove (i - 1) else i
    range d = [opensAbove (dDocStart d) .. dEnd d - 1] <> [dEnd d | blank (dEnd d)]
    drop' = IS.fromList (concat [range d | d <- declarations src, dName d `elem` names])

-- | Whether a text names a declaration: some identifier in it has the
-- declaration's last name as one of its dot-separated parts (so @l@,
-- @Foo.l@ and @l.symm@ all name @Foo.l@). Errs on the side of yes.
mentions :: Text -> Text -> Bool
mentions name text = any (elem (lastComponent name) . T.splitOn ".") idents
  where
    idents = filter (not . T.null) (T.split (not . identChar) text)
    identChar c = isAlphaNum c || c `elem` ("_'.!?" :: String)

-- | Compile the merged text and read it: it must compile, the node must be
-- waiting (or closed) on the lemmas and each lemma must have a printed type.
-- Lemma names may be bare or already qualified; the result is keyed by the
-- names as given.
checkSketch :: LeanRun -> AxiomPolicy -> FilePath -> FilePath -> Text -> Text -> Text -> [Text]
            -> IO (Either Text (Map.Map Text Text))
checkSketch lr pol root rel applied nodeName nodeType lemmaNames =
  fmap snd <$> checkSketchVerdict lr pol root rel applied nodeName nodeType lemmaNames

-- | 'checkSketch' with the node's verdict ('Closed' or 'Waiting') beside the types.
checkSketchVerdict :: LeanRun -> AxiomPolicy -> FilePath -> FilePath -> Text -> Text -> Text -> [Text]
                   -> IO (Either Text (Verdict, Map.Map Text Text))
checkSketchVerdict lr pol root rel applied nodeName nodeType lemmaNames = do
  let pre = nsPrefix nodeName
      qualified = [(n, qualify pre n) | n <- lemmaNames]
  pr <- probe lr root rel applied (nodeName : map snd qualified)
  pure $ case verdict pol applied nodeName nodeType pr of
    Rejected r -> Left r
    v -> fmap ((,) v . Map.fromList) $ mapM
      (\(n, q) -> maybe (Left ("no type printed for " <> n)) (\t -> Right (n, t)) (Map.lookup q (prTypes pr)))
      qualified
