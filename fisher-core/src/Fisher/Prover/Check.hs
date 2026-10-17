-- | The pure half of @fisher check@: what it runs and how it reads what
-- comes back, for Rocq and Isabelle (spec section 4.4). Every parser here
-- is tested on outputs recorded from the real tools (fixtures under
-- @fisher-core/test/fixtures/check/@); "Fisher.Cli.Check" runs the tools.
module Fisher.Prover.Check
  ( Target (..)
  , CheckResult (..)
  , parseTarget
  , resultLine
  , declAt
    -- * Rocq
  , rocqFlags
  , rocqProjectFiles
  , relocateFlags
  , rocqModule
  , parseRocqDeps
  , compileOrder
  , parseAssumptions
  , rocqError
  , rocqProblems
  , rocqCheckFile
  , duneTheoryName
    -- * Isabelle
  , RootSession (..)
  , parseRootSessions
  , sessionFor
  , parseIsabelleBuild
  , isabelleSorries
  , parseOracles
  , oracleResults
  , isabelleProblems
  , isabelleLabels
  , isabelleCheckRoot
  , isabelleCheckTheory
  , isabelleStrictArgs
  , isabelleCheckArgs
  , checkSession
  ) where

import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (dropExtension, isAbsolute, splitDirectories, takeBaseName, (</>))
import Text.Read (readMaybe)

-- | A file and, optionally, one declaration in it; without one, every
-- theorem the file declares.
data Target = Target {tFile :: FilePath, tDecl :: Maybe Text}
  deriving (Eq, Show)

data CheckResult = CheckResult {crTarget :: Text, crOk :: Bool, crProblems :: [Text]}
  deriving (Eq, Show)

-- | @FILE:decl@ splits at the last colon when a name follows it.
parseTarget :: String -> Target
parseTarget s = case break (== ':') (reverse s) of
  (d@(_ : _), ':' : f@(_ : _)) | '/' `notElem` d -> Target (reverse f) (Just (T.pack (reverse d)))
  _ -> Target s Nothing

-- | @ok <target>@ or @FAIL <target>: <problems>@.
resultLine :: CheckResult -> Text
resultLine r
  | crOk r = "ok " <> crTarget r
  | otherwise = "FAIL " <> crTarget r <> ": " <> T.intercalate "; " (crProblems r)

-- | The declaration (from 'Fisher.Prover.Holes.declarations') a line falls in.
declAt :: [(Int, Text)] -> Int -> Maybe Text
declAt ds line = snd <$> listToMaybe (reverse [d | d@(l, _) <- sortOn fst ds, l <= line])

-- Rocq ----------------------------------------------------------------------

-- | The words of a @_RocqProject@ / @_CoqProject@: @#@ comments dropped,
-- double quotes grouping.
projectWords :: Text -> [Text]
projectWords = concatMap (wordsQ . T.unpack) . T.lines
  where
    wordsQ s = case dropWhile isSpace s of
      [] -> []
      ('#' : _) -> []
      ('"' : r) -> let (w, r') = break (== '"') r in T.pack w : wordsQ (drop 1 r')
      r -> let (w, r') = break (\c -> isSpace c || c == '#') r in T.pack w : wordsQ r'

-- | (flags, files) of a project file.
projectParts :: Text -> ([Text], [Text])
projectParts = go . projectWords
  where
    go = \case
      [] -> ([], [])
      (f : a : b : r) | f `elem` ["-Q", "-R"] -> let (x, y) = go r in (f : a : b : x, y)
      ("-I" : a : r) -> let (x, y) = go r in ("-I" : a : x, y)
      ("-arg" : _ : r) -> go r
      (w : r)
        | ".v" `T.isSuffixOf` w -> let (x, y) = go r in (x, w : y)
        | otherwise -> go r

-- | The @-Q@, @-R@ and @-I@ flags of a @_RocqProject@ or @_CoqProject@.
rocqFlags :: Text -> [Text]
rocqFlags = fst . projectParts

-- | The @.v@ files a @_RocqProject@ or @_CoqProject@ lists.
rocqProjectFiles :: Text -> [FilePath]
rocqProjectFiles = map T.unpack . snd . projectParts

normDir :: FilePath -> [String]
normDir = filter (`notElem` [".", ""]) . splitDirectories

-- | The logical module name of a source file (relative to the project
-- root) under the flags, and whether a @-Q@ / @-R@ mapping covers it. An
-- uncovered file is compiled under its base name.
rocqModule :: [Text] -> FilePath -> (Text, Bool)
rocqModule flags file =
  case sortOn (\(d, _) -> negate (length d)) [m | m@(d, _) <- maps flags, d `isPrefixOf` dirs] of
    ((d, name) : _) -> (dotted (name ++ drop (length d) dirs ++ [base]), True)
    [] -> (T.pack base, False)
  where
    parts = normDir file
    dirs = take (length parts - 1) parts
    base = takeBaseName file
    dotted = T.intercalate "." . map T.pack . filter (not . null)
    maps = \case
      (f : d : n : r) | f `elem` ["-Q", "-R"] -> (normDir (T.unpack d), splitOnDot n) : maps r
      (_ : r) -> maps r
      [] -> []
    splitOnDot n = filter (not . null) (map T.unpack (T.splitOn "." n))

-- | @rocq dep@ (or @coqdep@) output as each source file's source
-- dependencies within the project.
parseRocqDeps :: Text -> [(FilePath, [FilePath])]
parseRocqDeps = mapMaybe line . T.lines
  where
    line l = case T.breakOn ": " l of
      (lhs, rhs) | not (T.null rhs) -> do
        vo <- listToMaybe (filter (".vo" `T.isSuffixOf`) (T.words lhs))
        let src = toV vo
        pure (T.unpack src, [T.unpack (toV d) | d <- T.words (T.drop 2 rhs), ".vo" `T.isSuffixOf` d, toV d /= src])
      _ -> Nothing
    toV w = T.dropEnd 1 w

-- | The files to compile for these, dependencies first.
compileOrder :: [(FilePath, [FilePath])] -> [FilePath] -> [FilePath]
compileOrder deps = reverse . snd . foldl visit (Set.empty, [])
  where
    m = Map.fromList deps
    visit (seen, acc) f
      | f `Set.member` seen = (seen, acc)
      | otherwise =
          let (seen', acc') = foldl visit (Set.insert f seen, acc) (Map.findWithDefault [] f m)
           in (seen', f : acc')

-- | The output of compiling a file holding @Print Assumptions X.@: @Right
-- []@ when closed, the assumptions (axioms and the rest) otherwise, @Left
-- "missing"@ when X is not found (removed, or ended with @Abort@), and
-- @Left@ the error for any other failure.
parseAssumptions :: Text -> Either Text [Text]
parseAssumptions t
  | any ("was not found in the current environment" `T.isInfixOf`) ls = Left "missing"
  | Just e <- rocqError t = Left e
  | not (null entries) = Right entries
  | any ("Closed under the global context" `T.isInfixOf`) ls = Right []
  | otherwise = Left "no Print Assumptions output"
  where
    ls = T.lines t
    entries = sections ls
    sections = \case
      [] -> []
      (l : r) | isHeader l -> let (es, r') = section r in es ++ sections r'
      (_ : r) -> sections r
    section = \case
      (l : r)
        | Just n <- entry l -> let (es, r') = section r in (n : es, r')
        | " " `T.isPrefixOf` l || "\t" `T.isPrefixOf` l -> section r
      r -> ([], r)
    -- a header is @Axioms:@; @name :@ with the type on the next line (Rocq
    -- wraps a long one so) is an entry
    isHeader l = not (T.null l) && not (isSpace (T.head l)) && ":" `T.isSuffixOf` T.stripEnd l && not (" :" `T.isSuffixOf` T.stripEnd l) && not (" : " `T.isInfixOf` l)
    entry l
      | not (T.null l), not (isSpace (T.head l)), (n, rest) <- T.breakOn " : " l, not (T.null rest), T.all nameChar n, not (T.null n) = Just n
      | not (T.null l), not (isSpace (T.head l)), Just n <- T.stripSuffix " :" (T.stripEnd l), T.all nameChar n, not (T.null n) = Just n
      | otherwise = Nothing
    nameChar c = isAlphaNum c || c `elem` ("_'." :: String)

-- | The first error of a Rocq run, with its line and file.
rocqError :: Text -> Maybe Text
rocqError t = case break ("Error:" `T.isPrefixOf`) (T.lines t) of
  (pre, e : _) -> Just (T.strip (T.drop 6 e) <> location (listToMaybe (reverse pre)))
  _ -> Nothing
  where
    location (Just l)
      | "File \"" `T.isPrefixOf` l
      , (f, r) <- T.breakOn "\"" (T.drop 6 l)
      , Just n <- listToMaybe (drop 1 (T.words (T.takeWhile (/= ',') (T.drop 2 (snd (T.breakOn ", line" r)))))) =
          " (line " <> n <> " of \"" <> f <> "\")"
    location _ = ""

-- | The problems of a Rocq target (its fully qualified name, and the
-- declaration as given) from its 'parseAssumptions' result. An @Admitted@
-- target is listed as an axiom under its own name: Rocq prints the
-- shortest unambiguous suffix of the qualified name (@A.adm@ for
-- @Lib.A.adm@, or @adm@ in the file itself), so the axiom is the target
-- when it is such a suffix, on dot boundaries, at least as long as the
-- declaration.
rocqProblems :: Text -> Text -> Either Text [Text] -> [Text]
rocqProblems full decl = \case
  Right axs -> map one axs
  Left "missing" -> ["missing (not declared, or ended with Abort)"]
  Left e -> [e]
  where
    fullParts = T.splitOn "." full
    declLen = length (T.splitOn "." decl)
    isTarget ax =
      let ps = T.splitOn "." ax
       in length ps >= declLen && ps == drop (length fullParts - length ps) fullParts
    one ax
      | isTarget ax = decl <> " is Admitted (axiom " <> ax <> ")"
      | otherwise = "axiom " <> ax

-- | Project flags for compiling in a copy of the project: @-I@ directories
-- (OCaml plugins, not copied) and @-Q@ / @-R@ directories outside the
-- project become absolute paths into the original.
relocateFlags :: FilePath -> [Text] -> [Text]
relocateFlags root = \case
  (f : d : n : r) | f `elem` ["-Q", "-R"] -> f : outside d : n : relocateFlags root r
  ("-I" : d : r) -> "-I" : absolute d : relocateFlags root r
  (x : r) -> x : relocateFlags root r
  [] -> []
  where
    absolute d = if isAbsolute (T.unpack d) then d else T.pack (root </> T.unpack d)
    outside d = case splitDirectories (T.unpack d) of
      (".." : _) -> absolute d
      _ -> d

-- | The generated file that audits one declaration of a compiled module.
rocqCheckFile :: Text -> Text -> Text
rocqCheckFile m d = "Require " <> m <> ".\nPrint Assumptions " <> m <> "." <> d <> ".\n"

-- | The @(name X)@ of a dune file's @(coq.theory ...)@ or @(rocq.theory ...)@.
duneTheoryName :: Text -> Maybe Text
duneTheoryName t
  | any (`T.isInfixOf` t) ["(coq.theory", "(rocq.theory"] =
      case T.breakOn "(name" t of
        (_, r) | not (T.null r) -> listToMaybe (T.words (T.map (\c -> if c == ')' then ' ' else c) (T.drop 5 r)))
        _ -> Nothing
  | otherwise = Nothing

-- Isabelle ------------------------------------------------------------------

-- | A session of a ROOT file: its name, its directory (from @in DIR@,
-- relative to the ROOT's directory, @""@ for the same one) and its theories.
data RootSession = RootSession {rsName :: Text, rsDir :: FilePath, rsTheories :: [Text]}
  deriving (Eq, Show)

data RTok = RWord Text | RStr Text | RSym Char
  deriving (Eq, Show)

rootTokens :: String -> [RTok]
rootTokens = \case
  [] -> []
  ('(' : '*' : r) -> rootTokens (comment (1 :: Int) r)
  ('"' : r) -> let (w, r') = break (== '"') r in RStr (T.pack w) : rootTokens (drop 1 r')
  s@('\\' : _) | "\\<open>" `isPrefixOf` s -> RStr "" : rootTokens (cart (1 :: Int) (drop 7 s))
  (c : r)
    | isSpace c -> rootTokens r
    | c `elem` ("()[]=+," :: String) -> RSym c : rootTokens r
    | otherwise -> let (w, r') = break stop (c : r) in RWord (T.pack w) : rootTokens r'
  where
    stop x = isSpace x || x `elem` ("()[]=+,\"" :: String)
    comment _ [] = []
    comment d ('*' : ')' : r) = if d == 1 then r else comment (d - 1) r
    comment d ('(' : '*' : r) = comment (d + 1) r
    comment d (_ : r) = comment d r
    cart _ [] = []
    cart d s
      | "\\<close>" `isPrefixOf` s = if d == 1 then drop 8 s else cart (d - 1) (drop 8 s)
      | "\\<open>" `isPrefixOf` s = cart (d + 1) (drop 7 s)
      | otherwise = cart d (drop 1 s)

-- | The sessions of a ROOT file, enough to find a theory's session.
parseRootSessions :: Text -> [RootSession]
parseRootSessions = top . rootTokens . T.unpack
  where
    top = \case
      (RWord "session" : n : r) | Just name <- nameOf n -> header name r
      (_ : r) -> top r
      [] -> []
    header name r0 =
      let r1 = skipGroup '(' ')' r0
          (dir, r2) = case r1 of
            (RWord "in" : d : r) | Just dn <- nameOf d -> (T.unpack dn, r)
            _ -> ("", r1)
          r3 = case r2 of (RSym '=' : r) -> r; r -> r
          (ths, rest) = body False r3
       in RootSession name dir ths : top rest
    body inTh = \case
      [] -> ([], [])
      ts@(RWord w : _) | w `elem` ["session", "chapter"] -> ([], ts)
      ts@(RSym '[' : _) -> body inTh (skipGroup '[' ']' ts)
      ts@(RSym '(' : _) -> body inTh (skipGroup '(' ')' ts)
      (RWord w : r) | w `elem` keywords -> body (w == "theories") r
      (t : r)
        | inTh, Just n <- nameOf t -> let (xs, r') = body inTh r in (n : xs, r')
        | otherwise -> body inTh r
    keywords =
      [ "description", "options", "sessions", "directories", "theories", "document_theories"
      , "document_files", "export_files", "export_classpath" ]
    nameOf = \case RWord w -> Just w; RStr s | not (T.null s) -> Just s; _ -> Nothing
    skipGroup o c (RSym x : r) | x == o = go (1 :: Int) r
      where
        go _ [] = []
        go d (RSym y : r')
          | y == c = if d == 1 then r' else go (d - 1) r'
          | y == o = go (d + 1) r'
        go d (_ : r') = go d r'
    skipGroup _ _ ts = ts

-- | The session and theory base name of a theory file (relative to the
-- ROOT's directory): a theories entry naming its path wins, then one
-- with its base name.
sessionFor :: [RootSession] -> FilePath -> Maybe (Text, Text)
sessionFor ss file = listToMaybe (exact ++ loose)
  where
    parts = normDir (dropExtension file)
    base = T.pack (takeBaseName file)
    rel s = let d = normDir (rsDir s) in if d `isPrefixOf` parts then Just (drop (length d) parts) else Nothing
    exact = [(rsName s, base) | s <- ss, Just r <- [rel s], th <- rsTheories s, normDir (T.unpack th) == r]
    loose = [(rsName s, base) | s <- ss, th <- rsTheories s, T.pack (takeBaseName (T.unpack th)) == base]

starred :: Text -> [Text]
starred = mapMaybe (fmap T.strip . T.stripPrefix "***") . T.lines

-- | Where the strict build met a @sorry@: (file as printed, line).
isabelleSorries :: Text -> [(FilePath, Int)]
isabelleSorries = mapMaybe one . starred
  where
    one l = do
      r <- T.stripPrefix "At command \"sorry\" (line " l
      let (n, r') = T.breakOn " of \"" r
      f <- T.stripSuffix "\")" (T.drop 5 r')
      line <- readMaybe (T.unpack n)
      pure (T.unpack f, line)

-- | The strict build's verdict: @Left@ names each sorry, or the first error.
parseIsabelleBuild :: Text -> Either Text ()
parseIsabelleBuild t
  | not (null sorries) = Left (T.intercalate "; " ["sorry at line " <> tshow n <> " of " <> T.pack f | (f, n) <- sorries])
  | (e : rest) <- errs = Left (e <> at rest)
  | failed = Left "build failed"
  | otherwise = Right ()
  where
    sorries = isabelleSorries t
    errs = dropWhile (\l -> T.null l || "At command" `T.isPrefixOf` l) (starred t)
    failed = any (\l -> "FAILED" `T.isInfixOf` l || "Unfinished session" `T.isPrefixOf` l) (T.lines t)
    at rest = case rest of
      (l : _)
        | Just r <- T.stripPrefix "At command \"" l
        , (_, loc) <- T.breakOn "(line " r
        , not (T.null loc) -> " " <> T.replace "\"" "" loc
      _ -> ""
    tshow = T.pack . show

-- | One result line of the check theory: @FISHER_OK n@ is @Right []@,
-- @FISHER_ORACLES n: a, b@ the oracles, @FISHER_MISSING n@ (or an
-- @Undefined fact@ error) @Left "missing"@.
parseOracles :: Text -> Either Text [Text]
parseOracles l0
  | "FISHER_OK " `T.isPrefixOf` l = Right []
  | Just r <- T.stripPrefix "FISHER_ORACLES " l = Right (filter (not . T.null) (map T.strip (T.splitOn "," (T.drop 2 (snd (T.breakOn ": " r))))))
  | "FISHER_MISSING " `T.isPrefixOf` l || "Undefined fact" `T.isPrefixOf` l = Left "missing"
  | otherwise = Left l
  where
    l = T.strip (maybe l0 id (T.stripPrefix "***" (T.strip l0)))

-- | Every target's result in the check build's output.
oracleResults :: Text -> [(Text, Either Text [Text])]
oracleResults = mapMaybe one . starred
  where
    one l
      | Just r <- T.stripPrefix "FISHER_OK " l = Just (T.strip r, parseOracles l)
      | Just r <- T.stripPrefix "FISHER_ORACLES " l = Just (fst (T.breakOn ": " r), parseOracles l)
      | Just r <- T.stripPrefix "FISHER_MISSING " l = Just (T.strip r, parseOracles l)
      | Just r <- T.stripPrefix "Undefined fact: \"" l = Just (fst (T.breakOn "\"" r), Left "missing")
      | otherwise = Nothing

isabelleProblems :: Either Text [Text] -> [Text]
isabelleProblems = \case
  Right os -> ["oracle " <> o <> (if o == "Pure.skip_proof" then " (sorry)" else "") | o <- os]
  Left "missing" -> ["missing (not declared, or ended with oops)"]
  Left e -> [e]

-- | The name each (theory, fact) target reports under in the check
-- theory: the fact's name, or @theory.name@ when two targets share it.
isabelleLabels :: [(Text, Text)] -> [Text]
isabelleLabels ts = [if length (filter ((== n) . snd) ts) > 1 then th <> "." <> n else n | (th, n) <- ts]

-- | The generated session that audits the targets of one session.
checkSession :: Text
checkSession = "Fisher_Check"

isabelleCheckRoot :: Text -> Text
isabelleCheckRoot parent = "session " <> checkSession <> " = \"" <> parent <> "\" +\n  theories " <> checkSession <> "\n"

-- | The check theory: imports the targets' theories (session-qualified)
-- and, in one ML command, looks up each target, collects its oracles and
-- fails with every result when any target is missing or has one, so that
-- no target hides another (an ML error stops the theory).
isabelleCheckTheory :: [Text] -> [(Text, Text)] -> Text
isabelleCheckTheory imports targets =
  T.unlines
    [ "theory " <> checkSession <> " imports " <> T.unwords (map quote imports) <> " begin"
    , "ML \\<open>"
    , "local"
    , "  fun fisher_lookup ctxt names ="
    , "    (case names of"
    , "      [] => NONE"
    , "    | n :: ns => (case try (Proof_Context.get_thms ctxt) n of NONE => fisher_lookup ctxt ns | r => r))"
    , "  fun fisher_one ctxt (label, names) ="
    , "    (case fisher_lookup ctxt names of"
    , "      NONE => \"FISHER_MISSING \" ^ label"
    , "    | SOME thms =>"
    , "        (case Thm_Deps.all_oracles thms of"
    , "          [] => \"FISHER_OK \" ^ label"
    , "        | os => \"FISHER_ORACLES \" ^ label ^ \": \" ^ commas (map (fn x => fst (fst x)) os)))"
    , "in"
    , "  fun fisher_check ctxt targets ="
    , "    let val rs = map (fisher_one ctxt) targets"
    , "    in if forall (String.isPrefix \"FISHER_OK \") rs then writeln (cat_lines rs) else error (cat_lines rs) end"
    , "end"
    , "\\<close>"
    , "ML \\<open>fisher_check \\<^context> [" <> T.intercalate ", " (zipWith entry (isabelleLabels targets) targets) <> "]\\<close>"
    , "end"
    ]
  where
    quote x = "\"" <> x <> "\""
    entry label (th, n) = "(" <> quote label <> ", [" <> quote (th <> "." <> n) <> ", " <> quote n <> "])"

-- | @isabelle build@ with sorry refused; @-c@ so that a heap built earlier
-- with @quick_and_dirty@ is not reused.
isabelleStrictArgs :: FilePath -> Text -> [String]
isabelleStrictArgs root session = ["build", "-c", "-o", "quick_and_dirty=false", "-d", root, T.unpack session]

-- | The check session's build. After a strict build that passed it keeps
-- @quick_and_dirty=false@, so the parent's heap from that build is reused:
-- other options would rebuild the parent with them, and under
-- @quick_and_dirty=true@ the function package (@fun@, @primrec@) proves its
-- equations with @Pure.skip_proof@, so every theorem about such a
-- definition would show that oracle. After a strict build that failed it
-- sets @quick_and_dirty=true@ (sorry allowed, so the parent builds and each
-- target's oracles show).
isabelleCheckArgs :: Bool -> FilePath -> FilePath -> [String]
isabelleCheckArgs strictOk root tmp =
  ["build", "-c", "-o", "quick_and_dirty=" <> (if strictOk then "false" else "true"), "-d", root, "-d", tmp, T.unpack checkSession]
