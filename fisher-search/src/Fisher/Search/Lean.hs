-- | A light scanner for the theorems of a Lean file: where each one
-- starts, where its statement ends and what its proof is, with names
-- qualified by the enclosing namespaces. Enough to find the open goals
-- (a proof that is just @sorry@) and to cut one declaration out of a file
-- and into another; Lean itself judges whether the result is a proof.
--
-- A declaration is a line starting (after attributes and modifiers) with
-- @theorem@ or @lemma@. Its statement runs to the first line containing
-- @:=@ followed by nothing, @by@, or a proof on the same line; its proof is
-- what follows @:=@ (and @by@) on that line and the indented or blank lines
-- below, up to the next line starting in column 0, trailing blank lines
-- left out.
module Fisher.Search.Lean
  ( Decl (..)
  , declarations
  , openGoals
  , isOpen
  , findDecl
  , statementHead
  , proofCode
  ) where

import Data.Char (isSpace)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T

data Decl = Decl
  { dName :: Text
  -- ^ fully qualified
  , dPrivate :: Bool
  , dDocStart :: Int
  -- ^ first line of the doc comment and attributes above it (0-based)
  , dStart :: Int
  -- ^ the line with the keyword
  , dStmtEnd :: Int
  -- ^ the line where the statement ends (holds the @:=@)
  , dEnd :: Int
  -- ^ one past the last line of the proof
  , dStatement :: Text
  -- ^ from the keyword through @:=@ (and @ by@ when there), as written
  , dProof :: Text
  -- ^ the proof, from after @:=@ / @:= by@ to the end
  }
  deriving (Eq, Show)

data Scope = Ns Text | Sec Text

-- | Every theorem and lemma of a file, in order.
declarations :: Text -> [Decl]
declarations src = go [] 0
  where
    ls = T.splitOn "\n" src
    n = length ls
    line i = ls !! i
    go :: [Scope] -> Int -> [Decl]
    go stack i
      | i >= n = []
      | otherwise =
          let l = line i
              ws = T.words l
           in case ws of
                ("namespace" : name : _) -> go (reverse (map Ns (T.splitOn "." name)) <> stack) (i + 1)
                (s : rest) | s == "section" || (s == "noncomputable" && take 1 rest == ["section"]) ->
                  let nm = case dropWhile (== "section") (if s == "noncomputable" then rest else s : rest) of
                        (x : _) -> x
                        [] -> ""
                   in go (Sec nm : stack) (i + 1)
                ("end" : rest) | startsAtColumn0 l -> go (closeScope (take 1 rest) stack) (i + 1)
                _ | startsAtColumn0 l, Just (kwCol, priv, name) <- declHead l ->
                      case statementEnd i kwCol of
                        Nothing -> go stack (i + 1)
                        Just (j, cut) ->
                          let stmt = T.intercalate "\n" (take (j - i + 1) (drop i ls))
                              stmt' = T.drop kwCol stmt
                              stmtText = T.take (T.length stmt' - (T.length (line j) - cut)) stmt'
                              inline = T.strip (T.drop cut (line j))
                              k0 = bodyEnd (j + 1)
                              k = trimBlank (j + 1) k0
                              body = take (k - j - 1) (drop (j + 1) ls)
                              proof = T.intercalate "\n" ([inline | not (T.null inline)] <> body)
                              nss = [x | Ns x <- reverse stack]
                              q = case T.stripPrefix "_root_." name of
                                Just r -> r
                                Nothing -> T.intercalate "." (nss <> [name])
                              d = Decl q priv (docStart i) i j k stmtText proof
                           in d : go stack (max k (j + 1))
                _ -> go stack (i + 1)
    closeScope name stack = case (name, stack) of
      ([], Sec _ : rest) -> rest
      ([], _) -> stack
      ([nm], Sec s : rest) | s == nm -> rest
      ([nm], _) -> drop (length (T.splitOn "." nm)) stack
      _ -> stack
    -- the statement ends on the first line from i holding ":=" at the end,
    -- or ":= by" at the end, or ":= by ..." / ":= term" (a one-line proof);
    -- a later column-0 line first means it is not a declaration we can cut
    -- (brackets opened on one line of the statement stay open on the next,
    -- so a `:=` inside a multi-line `{ x with f := ... }` does not count)
    statementEnd i kwCol = loop 0 i
      where
        loop depth j
          | j >= n = Nothing
          | j > i && startsAtColumn0 (line j) = Nothing
          | otherwise = case assignScan depth (if j == i then T.drop kwCol (line j) else line j) of
              Right c -> Just (j, c + (if j == i then kwCol else 0))
              Left depth' -> loop depth' (j + 1)
    bodyEnd k
      | k >= n = n
      | let l = line k, not (T.null l), not (isSpace (T.head l)) = k
      | otherwise = bodyEnd (k + 1)
    trimBlank lo k
      | k > lo && T.null (T.strip (line (k - 1))) = trimBlank lo (k - 1)
      | otherwise = k
    -- doc comments and attribute lines directly above the keyword line
    docStart i = up i
      where
        up j
          | j > 0, isAttr (line (j - 1)) = up (j - 1)
          | j > 0, T.isSuffixOf "-/" (T.stripEnd (line (j - 1))) = case findDocOpen (j - 1) of
              Just o -> up o
              Nothing -> j
          | otherwise = j
        findDocOpen j
          | j < 0 = Nothing
          | "/--" `T.isPrefixOf` line j || "/-!" `T.isPrefixOf` line j = Just j
          | j < i - 400 = Nothing
          | otherwise = findDocOpen (j - 1)
        isAttr l = "@[" `T.isPrefixOf` l && "]" `T.isSuffixOf` T.stripEnd l

startsAtColumn0 :: Text -> Bool
startsAtColumn0 l = not (T.null l) && not (isSpace (T.head l))

-- | The keyword's column, whether the declaration is private, and its
-- name, for a line opening a theorem or lemma.
declHead :: Text -> Maybe (Int, Bool, Text)
declHead l0 = goAttrs 0 l0
  where
    goAttrs off l
      | "@[" `T.isPrefixOf` l =
          let (attr, rest) = T.breakOn "]" l
              rest' = T.drop 1 rest
              sp = T.length (T.takeWhile isSpace rest')
           in if T.null rest then Nothing else goMods (off + T.length attr + 1 + sp) False (T.drop sp rest')
      | otherwise = goMods off False l
    goMods off priv l =
      let (w, rest) = T.break isSpace l
          sp = T.length (T.takeWhile isSpace rest)
       in case w of
            _ | w `elem` ["private", "protected", "noncomputable", "nonrec"] ->
                  goMods (off + T.length w + sp) (priv || w == "private") (T.drop sp rest)
            _ | w `elem` ["theorem", "lemma"] ->
                  let nm = T.takeWhile (\c -> not (isSpace c) && c `notElem` ("({[:" :: String)) (T.drop sp rest)
                   in if T.null nm then Nothing else Just (off, priv, nm)
            _ -> Nothing

-- | Where the statement stops in a line: just past @:=@, or past @:= by@
-- when @by@ follows. Only an @:=@ outside brackets and not part of a
-- longer operator counts.
assignCut :: Text -> Maybe Int
assignCut = either (const Nothing) Just . assignScan 0

-- | 'assignCut' from a bracket depth carried over from earlier lines:
-- @Right@ the cut, or @Left@ the depth at the end of the line.
assignScan :: Int -> Text -> Either Int Int
assignScan depth0 l = scan 0 depth0 (T.unpack l)
  where
    scan _ depth [] = Left depth
    scan i depth s@(c : rest)
      | "--" `startsWith` s = Left depth
      | c `elem` ("([{⟨" :: String) = scan (i + 1) (depth + 1) rest
      | c `elem` (")]}⟩" :: String) = scan (i + 1) (max 0 (depth - 1)) rest
      | depth == 0, ":=" `startsWith` s, not (prevOp i), not (nextOp (drop 2 s)) =
          let after = drop 2 s
              sp = length (takeWhile isSpace after)
              rest2 = drop sp after
           in if "by" `startsWith` rest2 && (null (drop 2 rest2) || isSpace (head (drop 2 rest2)))
                then Right (i + 2 + sp + 2)
                else Right (i + 2)
      | otherwise = scan (i + 1) depth rest
    startsWith p s = take (length p) s == p
    prevOp i = i > 0 && T.index l (i - 1) `elem` (":=<>!|&" :: String)
    nextOp s = case s of
      (x : _) -> x `elem` ("=:" :: String)
      [] -> False

-- | The proof with comments and blank lines left out.
proofCode :: Decl -> [Text]
proofCode d =
  [ s | l <- T.splitOn "\n" (dProof d), let s = T.strip (fst (T.breakOn "--" l)), not (T.null s) ]

-- | A proof that is just @sorry@ (tactic or term).
isOpen :: Decl -> Bool
isOpen d = proofCode d == ["sorry"]

-- | The open goals of a file, private ones included.
openGoals :: Text -> [Decl]
openGoals = filter isOpen . declarations

findDecl :: Text -> [Decl] -> Maybe Decl
findDecl nm = find ((== nm) . dName)

-- | The statement without a trailing @by@: a term-mode proof may write
-- @:=@ where the original had @:= by@.
statementHead :: Decl -> Text
statementHead d =
  let s = T.stripEnd (dStatement d)
   in maybe s T.stripEnd (T.stripSuffix "by" s >>= \r -> if ":=" `T.isSuffixOf` T.stripEnd r then Just r else Nothing)
