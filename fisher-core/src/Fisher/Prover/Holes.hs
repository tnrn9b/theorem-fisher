-- | Where a source file leaves a goal unproved: @sorry@ and @admit@ in Lean,
-- @Admitted.@, @admit@ and @give_up@ in Rocq, @sorry@ and @oops@ in
-- Isabelle. One small character state machine per prover family lexes the
-- text into words, skipping what is not code (comments, strings,
-- cartouches), so a marker inside any of those is not a hole.
module Fisher.Prover.Holes
  ( Hole (..)
  , holes
  , declarations
  , holeWords
  , eventHoleWords
  ) where

import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Prover.Kind (ProverKind (..))

-- | An unproved goal: where its marker is (1-based line and column, in
-- characters), the marker, and the theorem or lemma it sits in, if any.
data Hole = Hole
  { hLine :: Int
  , hColumn :: Int
  , hWord :: Text
  , hDecl :: Maybe Text
  }
  deriving (Eq, Show)

-- | The markers the scanner reports, as 'hWord' spells them.
holeWords :: ProverKind -> [Text]
holeWords = \case
  Lean4 -> ["sorry", "admit"]
  Rocq -> ["Admitted.", "admit", "give_up"]
  Isabelle -> ["sorry", "oops"]

-- | The lower-case words a tool result is searched for when guessing that
-- a session left a hole. Lean keeps the literal @sorry@ it always used.
eventHoleWords :: ProverKind -> [Text]
eventHoleWords = \case
  Lean4 -> ["sorry"]
  Rocq -> ["admit", "give_up"]
  Isabelle -> ["sorry", "oops"]

-- | The unproved goals of a source text, in order.
holes :: ProverKind -> Text -> [Hole]
holes kind = walk kind Nothing . lexer (lang kind) . T.unpack

-- | The theorems a source text declares, with the line each starts on, in
-- order: the file outline @fisher check@ checks when given no targets. A
-- Rocq name inside @Module M. ... End M.@ (or @Module Import M.@, @Module
-- Export M.@) is qualified as @M.name@ (a @Section@ does not qualify; a
-- @Module Type@ declares no constants; @Declare Module@ opens nothing).
declarations :: ProverKind -> Text -> [(Int, Text)]
declarations kind = go [] . lexer (lang kind) . T.unpack
  where
    go _ [] = []
    go st (Word l _ w : rest)
      | w `elem` declKeys kind, (Just n, rest') <- declName kind rest =
          if Nothing `elem` st then go st rest' else (l, qualify st n) : go st rest'
      | kind == Rocq, w == "Declare", (Word _ _ "Module" : r) <- rest = go st (dropWhile (/= Sym '.') r)
      | kind == Rocq, w == "Module" = case rest of
          Word _ _ "Type" : Word _ _ _ : r | opens r -> go (Nothing : st) r
          Word _ _ ie : Word _ _ m : r | ie `elem` ["Import", "Export"], opens r -> go (Just (T.pack m) : st) r
          Word _ _ m : r | m `notElem` ["Import", "Export"], opens r -> go (Just (T.pack m) : st) r
          _ -> go st rest
      | kind == Rocq, w == "Section", Word _ _ _ : r <- rest = go (Just "" : st) r
      | kind == Rocq, w == "End", Word _ _ _ : r <- rest = go (drop 1 st) r
    go st (_ : rest) = go st rest

    -- @Module M := ...@ defines a module in one sentence and opens nothing
    opens r = not (assigns (takeWhile (/= Sym '.') r))
    assigns (Sym ':' : Sym '=' : _) = True
    assigns (_ : r) = assigns r
    assigns [] = False

    qualify st n = T.intercalate "." ([m | Just m <- reverse st, not (T.null m)] ++ [n])

-- Lexing ------------------------------------------------------------------

data Tok = Word !Int !Int String | Sym Char | Lit
  deriving (Eq, Show)

data StrEsc = Backslash | Doubled

data Lang = Lang
  { lnLine :: Maybe String        -- ^ line comment opener
  , lnBlock :: Maybe (String, String) -- ^ nesting block comment
  , lnStrInComment :: Bool        -- ^ strings are lexed inside block comments
  , lnEsc :: StrEsc
  , lnCarts :: [(String, String)] -- ^ cartouches (nesting)
  , lnCharLit :: Bool             -- ^ @'x'@ is a character literal
  , lnSymbols :: Bool             -- ^ @\<name>@ symbols
  , lnComment :: Bool             -- ^ @\<comment>@ runs to the end of the line
  }

lang :: ProverKind -> Lang
lang = \case
  Lean4 -> Lang (Just "--") (Just ("/-", "-/")) False Backslash [] True False False
  Rocq -> Lang Nothing (Just ("(*", "*)")) True Doubled [] False False False
  Isabelle ->
    Lang Nothing (Just ("(*", "*)")) False Backslash
      [("\\<open>", "\\<close>"), ("\8249", "\8250"), ("{*", "*}")] False True True

-- | Remaining text with the position of its first character.
data P = P !Int !Int String

adv :: Int -> P -> P
adv 0 p = p
adv _ p@(P _ _ []) = p
adv n (P l c (x : xs))
  | x == '\n' = adv (n - 1) (P (l + 1) 1 xs)
  | otherwise = adv (n - 1) (P l (c + 1) xs)

isIdStart, isIdChar :: Char -> Bool
isIdStart c = isAlphaNum c || c == '_'
isIdChar c = isAlphaNum c || c == '_' || c == '\''

lexer :: Lang -> String -> [Tok]
lexer lg = go . P 1 1
  where
    go p@(P l c s) = case s of
      [] -> []
      (x : rest)
        | Just o <- lnLine lg, o `isPrefixOf` s -> go (toEol p)
        | Just (o, cl) <- lnBlock lg, o `isPrefixOf` s -> go (block (o, cl) 1 (adv (length o) p))
        | lnComment lg, "\\<comment>" `isPrefixOf` s ->
            let q = adv (length ("\\<comment>" :: String)) p
                P _ _ t = spaces q
             in if startsArg t then go q else go (toEol q)
        | lnSymbols lg, "\\<" `isPrefixOf` s, (o, cl) : _ <- [k | k@(o', _) <- lnCarts lg, o' `isPrefixOf` s] ->
            Lit : go (cart (o, cl) 1 (adv (length o) p))
        | lnSymbols lg, "\\<" `isPrefixOf` s ->
            let q = adv 2 p
                P _ _ t = q
                n = length (takeWhile (\y -> y /= '>' && not (isSpace y)) t)
             in Sym '\\' : go (adv (n + 1) q)
        | (o, cl) : _ <- [k | k@(o', _) <- lnCarts lg, o' `isPrefixOf` s] ->
            Lit : go (cart (o, cl) 1 (adv (length o) p))
        | x == '"' -> Lit : go (str (adv 1 p))
        | lnCharLit lg, x == '\'', Just n <- charLen rest -> Lit : go (adv n p)
        | isIdStart x ->
            let w = ident s
             in Word l c w : go (adv (length w) p)
        | isSpace x -> go (adv 1 p)
        | otherwise -> Sym x : go (adv 1 p)

    startsArg t = any (`isPrefixOf` t) (map fst (lnCarts lg)) || "\"" `isPrefixOf` t

    spaces p@(P _ _ (y : _)) | isSpace y = spaces (adv 1 p)
    spaces p = p

    toEol p@(P _ _ s) = case s of
      [] -> p
      ('\n' : _) -> p
      _ -> toEol (adv 1 p)

    -- Inside a nesting block comment; strings are lexed when the prover does.
    block (o, cl) d p@(P _ _ s)
      | null s = p
      | cl `isPrefixOf` s = if d == (1 :: Int) then adv (length cl) p else block (o, cl) (d - 1) (adv (length cl) p)
      | o `isPrefixOf` s = block (o, cl) (d + 1) (adv (length o) p)
      | lnStrInComment lg, "\"" `isPrefixOf` s = block (o, cl) d (str (adv 1 p))
      | otherwise = block (o, cl) d (adv 1 p)

    cart (o, cl) d p@(P _ _ s)
      | null s = p
      | cl `isPrefixOf` s = if d == (1 :: Int) then adv (length cl) p else cart (o, cl) (d - 1) (adv (length cl) p)
      | o `isPrefixOf` s = cart (o, cl) (d + 1) (adv (length o) p)
      | otherwise = cart (o, cl) d (adv 1 p)

    -- Just after the opening quote.
    str p@(P _ _ s) = case s of
      [] -> p
      ('\\' : _) | Backslash <- lnEsc lg -> str (adv 2 p)
      ('"' : '"' : _) | Doubled <- lnEsc lg -> str (adv 2 p)
      ('"' : _) -> adv 1 p
      _ -> str (adv 1 p)

-- | The length of a Lean character literal after its opening quote, with
-- the quotes: @'x'@ is 3, @'\n'@ and @'\x41'@ run to the closing quote.
charLen :: String -> Maybe Int
charLen = \case
  ('\\' : r) -> case break (== '\'') (take 10 r) of
    (a, '\'' : _) -> Just (length a + 3)
    _ -> Nothing
  [_, '\''] -> Just 3
  (_ : '\'' : _) -> Just 3
  _ -> Nothing

-- | A word, with dots joined when an identifier character follows.
ident :: String -> String
ident s = case span isIdChar s of
  (a, '.' : r@(y : _)) | isIdStart y && (isAlpha y || y == '_') -> a ++ "." ++ ident r
  (a, _) -> a

-- Reading the words ---------------------------------------------------------

-- | Words that open a declaration whose name is the next word, per prover.
declKeys :: ProverKind -> [String]
declKeys = \case
  Lean4 -> ["theorem", "lemma", "corollary", "proposition"]
  Rocq -> ["Lemma", "Theorem", "Proposition", "Corollary", "Fact", "Remark", "Example"]
  Isabelle -> ["theorem", "lemma", "corollary", "proposition", "schematic_goal"]

-- | Words that open something else, ending the enclosing theorem.
resetKeys :: ProverKind -> [String]
resetKeys = \case
  Lean4 -> ["def", "instance", "structure", "inductive", "class", "abbrev", "axiom", "example", "opaque", "namespace", "section", "end"]
  Rocq -> ["Definition", "Fixpoint", "Inductive", "Record", "Instance", "Module", "Section", "End", "Axiom", "Parameter", "Hypothesis"]
  Isabelle -> ["definition", "fun", "function", "primrec", "datatype", "record", "locale", "theory", "instantiation", "abbreviation", "inductive", "axiomatization"]

walk :: ProverKind -> Maybe Text -> [Tok] -> [Hole]
walk kind = go
  where
    go _ [] = []
    go cur (Word l c w : rest)
      | w `elem` declKeys kind = let (nm, rest') = declName kind rest in go nm rest'
      | w `elem` resetKeys kind = go Nothing rest
      | Just hw <- hole w rest = Hole l c hw cur : go cur rest
    go cur (_ : rest) = go cur rest

    hole w rest = case kind of
      Lean4 -> pick ["sorry", "admit"]
      Rocq
        | w == "Admitted", (Sym '.' : _) <- rest -> Just "Admitted."
        | otherwise -> pick ["admit", "give_up"]
      Isabelle -> pick ["sorry", "oops"]
      where
        pick ws = if w `elem` ws then Just (T.pack w) else Nothing

-- | The name after a declaration keyword, and the tokens after it.
declName :: ProverKind -> [Tok] -> (Maybe Text, [Tok])
declName kind ts = case kind of
  Isabelle -> case skipContext ts of
    ts'@(Word _ _ w : nxt : _) | nxt `elem` [Sym ':', Sym '['] -> (Just (T.pack w), drop 1 ts')
    ts' -> (Nothing, ts')
  _ -> case ts of
    Word _ _ w : r -> (Just (T.pack w), r)
    _ -> (Nothing, ts)
  where
    -- @(in locale)@ after the keyword
    skipContext (Sym '(' : r) = drop 1 (dropWhile (/= Sym ')') r)
    skipContext r = r
