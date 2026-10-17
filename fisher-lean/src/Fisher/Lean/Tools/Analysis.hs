-- | lean-lsp-mcp @tools/analysis.py@ (with @attempt_utils.py@,
-- @verify.py@, @minimal_hypotheses.py@): @lean_multi_attempt@,
-- @lean_run_code@, @lean_verify@, @lean_minimal_hypotheses@. Every trial
-- runs in a scratch slot; the user's documents are never edited. The REPL
-- fast path (off unless configured) is not ported: these are the LSP paths.
module Fisher.Lean.Tools.Analysis
  ( multiAttemptTool
  , runCodeTool
  , verifyTool
  , minimalHypothesesTool
  , buildAttemptText
  , validTheoremName
  , classifyAxioms
  , parseAxioms
  , scanWarnings
  , findTheoremBinders
  , theoremDeclared
  , dropBinder
  ) where

import Control.Exception (try)
import Data.Char (isAlphaNum, isSpace)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Lean.Client
import Fisher.Lean.Error
import Fisher.Lean.Scratch
import Fisher.Lean.Session
import Fisher.Lean.Text (readFileContents, splitLines)
import Fisher.Lean.Tool
import Fisher.Lean.Tools.Common
import Fisher.Py.Json (pyFloatRepr)
import Fisher.Py.Value

-- ---------------------------------------------------------------- multi attempt

-- | @build_attempt_text(lines, line_context, target_column, snippet, line)@:
-- (snippet, text, goal line, goal column, line delta).
buildAttemptText :: [Text] -> Text -> Int -> Text -> Int -> (Text, Text, Int, Int, Int)
buildAttemptText ls ctx col snippet line =
  let snip = T.dropWhileEnd (== '\n') snippet
      snipLines = if T.null snip then [""] else T.splitOn "\n" snip
      indent = T.take col ctx
      payload = (indent <> head snipLines) : map (indent <>) (tail snipLines)
      endLine = min (line - 1 + length snipLines) (length ls)
      delta = length payload - (endLine - (line - 1))
      text = T.intercalate "\n" (take (line - 1) ls <> payload <> drop endLine ls) <> "\n"
      goalLine = line - 1 + length payload - 1
      goalCol = T.length (last payload)
   in (snip, text, goalLine, goalCol, delta)

type Identity = [Maybe PyVal]

-- | @diagnostic_identity(d)@ (range preferred).
identity :: PyVal -> Identity
identity d =
  let r = maybe (PDict []) id (rangeOf "range" "fullRange" d)
      sub k = case get k r of Just v | truthy v -> v; _ -> PDict []
   in [ get "line" (sub "start"), get "character" (sub "start"), get "line" (sub "end"), get "character" (sub "end")
      , get "severity" d, get "code" d, get "source" d, get "message" d ]

-- | @shift_baseline_keys(keys, edit_start_line, delta)@.
shiftKeys :: [Identity] -> Int -> Int -> [Identity]
shiftKeys ks start delta
  | delta == 0 = ks
  | otherwise = map shift ks
  where
    shift k = case k of
      (Just (PInt sl) : sc : el : rest)
        | fromIntegral sl >= start ->
            Just (PInt (sl + fromIntegral delta)) : sc : (case el of Just (PInt e) -> Just (PInt (e + fromIntegral delta)); _ -> el) : rest
      _ -> k

multiAttemptTool :: Tool
multiAttemptTool =
  Tool "lean_multi_attempt" "multi_attemptArguments"
    [fileField, ge "1" (field "line" TInt), field "snippets" TList, ge "1" (nullable "column" TInt PNull)]
    run
  where
    run s a = do
      let line = argInt a "line"
      snippets <- mapM snippetText (argList a "snippets")
      rel <- requireClientForFile s (argText a "file_path") Nothing
      c <- getClient s
      openSynced s rel False
      ls <- splitLines <$> content c rel
      ctx <-
        if line < 1 || line > length ls
          then toolError ("Line " <> tshow line <> " out of range (file has " <> tshow (length ls) <> " lines)")
          else pure (ls !! (line - 1))
      col <- case argMaybeInt a "column" of
        Nothing -> pure (maybe 0 id (T.findIndex (not . isSpace) ctx))
        Just k
          | k > T.length ctx + 1 -> toolError ("Column " <> tshow k <> " out of range for line of length " <> tshow (T.length ctx))
          | otherwise -> pure (k - 1)
      base <- try (diagnostics c rel True Nothing False)
      let baseKeys = case base of
            Right r -> Just (map identity (drItems r))
            Left (_ :: LeanError) -> Nothing
          prepared = [buildAttemptText ls ctx col sn line | sn <- snippets]
      pool <- scratchPool s False
      trials <- runTexts pool [t | (_, t, _, _, _) <- prepared] [Just (gl, gc) | (_, _, gl, gc, _) <- prepared] Nothing
      limit <- maxOutputChars s
      let item (sn, _, gl, _, delta) trial =
            let ds = drItems (trDiagnostics trial)
                indexed = zip [0 :: Int ..] ds
                local = [(i, d) | (i, d) <- indexed, keepLocal d]
                keepLocal d = not (null (filterByLineRange [d] (line - 1) gl))
                localIds = Set.fromList (map fst local)
                extra = case baseKeys of
                  Nothing -> []
                  Just bk ->
                    let shifted = shiftKeys bk (line - 1) delta
                     in [d | (i, d) <- indexed, not (Set.member i localIds), identity d `notElem` shifted]
                goals = case trGoal trial of
                  Just g | grStatus g == "goals" -> grGoals g
                  _ -> []
             in PDict
                  [ ("snippet", PStr sn)
                  , ("goals", PList (map (PStr . boundOutput limit) goals))
                  , ("diagnostics", PList [m | Just m <- map (diagnosticMessage limit) (map snd local <> extra)])
                  , ("timed_out", PBool False)
                  , ("proof_status", PNull) ]
      pure (Model (PDict [("items", PList (zipWith item prepared trials))]))
    snippetText = \case
      PStr t -> pure t
      v -> toolError ("1 validation error for multi_attemptArguments\nsnippets.0\n  Input should be a valid string [type=string_type, input_value=" <> pyRepr v <> ", input_type=" <> pyType v <> "]\n    For further information visit https://errors.pydantic.dev/2.13/v/string_type")

pyType :: PyVal -> Text
pyType = \case
  PStr _ -> "str"
  PList _ -> "list"
  PInt _ -> "int"
  PFloat _ -> "float"
  PBool _ -> "bool"
  PDict _ -> "dict"
  PNull -> "NoneType"

-- ---------------------------------------------------------------- run code

runCodeTool :: Tool
runCodeTool =
  Tool "lean_run_code" "run_codeArguments" [field "code" TStr] run
  where
    run s a = do
      bound <- boundProject s
      case bound of
        Nothing -> toolError "No valid Lean project path found. Run another tool first to set it up."
        Just _ -> pure ()
      startupClient s
      pool <- scratchPool s True
      let code = argText a "code"
      trial <- runText pool (if "\n" `T.isSuffixOf` code then code else code <> "\n") Nothing Nothing
      limit <- maxOutputChars s
      let report = trDiagnostics trial
          msgs = [m | Just m <- map (diagnosticMessage limit) (drItems report)]
          errs = any (\m -> get "severity" m == Just (PStr "error")) msgs
      pure (Model (PDict [("success", PBool (not errs && not (drFatal report))), ("timed_out", PBool False), ("diagnostics", PList msgs)]))

-- ---------------------------------------------------------------- verify

identStart, identRest :: Char -> Bool
identStart ch = (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') || ch == '_'
identRest ch = identStart ch || (ch >= '0' && ch <= '9') || ch == '\''

-- | @_validate_theorem_name@'s pattern, ASCII dotted identifiers.
validTheoremName :: Text -> Bool
validTheoremName t = not (T.null t) && all seg (T.splitOn "." t)
  where
    seg x = case T.uncons x of
      Just (h, r) -> identStart h && T.all identRest r
      Nothing -> False

checkTheoremName :: Text -> IO Text
checkTheoremName n
  | validTheoremName n = pure n
  | otherwise = toolError "Invalid theorem name. Use a Lean fully qualified name such as `Namespace.theorem`."

-- | @parse_axioms(diags)@.
parseAxioms :: [PyVal] -> [Text]
parseAxioms ds = concat [axiomsIn (T.replace "\n" " " (msgOf d)) | d <- ds, get "severity" d == Just (PInt 3)]
  where
    axiomsIn msg = case T.breakOn "depends on axioms:" msg of
      (_, r) | not (T.null r) ->
        let after = T.dropWhile isSpace (T.drop (T.length "depends on axioms:") r)
         in case T.uncons after of
              Just ('[', body) ->
                -- (.+?)\] : the shortest non-empty run before a ']'
                case T.breakOn "]" (T.drop 1 body) of
                  (x, rest) | not (T.null rest) && not (T.null body) -> map T.strip (T.splitOn "," (T.take 1 body <> x))
                  _ -> axiomsIn (T.drop 1 r)
              _ -> axiomsIn (T.drop 1 r)
      _ -> []

msgOf :: PyVal -> Text
msgOf d = case get "message" d of Just (PStr m) -> m; Just v -> pyStrOf v; Nothing -> ""

-- | @classify_axioms(axioms)@: the trust verdict and the non-standard ones.
classifyAxioms :: [Text] -> (Text, [Text])
classifyAxioms axs =
  let nonStd = [x | x <- axs, x `notElem` ["propext", "Classical.choice", "Quot.sound"]]
   in if "sorryAx" `elem` axs
        then ("incomplete", nonStd)
        else
          if any (\x -> x `elem` ["Lean.ofReduceBool", "Lean.trustCompiler"] || nativeDecide x) axs
            then ("native", nonStd)
            else if not (null nonStd) then ("custom", nonStd) else ("standard", nonStd)
  where
    -- (?:^|\.)_native\.native_decide\.ax(?:_\d+)*$
    nativeDecide x =
      let marker = "_native.native_decide.ax"
       in or
            [ (i == 0 || T.index x (i - 1) == '.') && suffixOk (T.drop (i + T.length marker) x)
            | i <- positions marker x ]
    suffixOk t
      | T.null t = True
      | Just r <- T.stripPrefix "_" t, let (d, r') = T.span (\ch -> ch >= '0' && ch <= '9') r, not (T.null d) = suffixOk r'
      | otherwise = False

positions :: Text -> Text -> [Int]
positions needle hay = go 0 hay
  where
    go off t = case T.breakOn needle t of
      (a, b) | not (T.null b) -> let i = off + T.length a in i : go (i + 1) (T.drop (T.length a + 1) t)
      _ -> []

-- | One of verify.py's warning patterns, as tokens.
data Tok = WB | L Text | Ws

warningPatterns :: [[Tok]]
warningPatterns =
  [ [L "set_option", Ws, L "debug."]
  , [WB, L "unsafe", WB]
  , [L "@[implemented_by", WB]
  , [L "@[extern", WB]
  , [WB, L "opaque", WB]
  , [L "local", Ws, L "instance", WB]
  , [L "local", Ws, L "notation", WB]
  , [L "local", Ws, L "macro_rules", WB]
  , [L "scoped", Ws, L "notation", WB]
  , [L "scoped", Ws, L "instance", WB]
  , [L "@[csimp", WB]
  , [L "import", Ws, L "Lean.Elab", WB]
  , [L "import", Ws, L "Lean.Meta", WB] ]

-- | The leftmost match of a pattern (@re.search(...).group(0)@).
searchPat :: [Tok] -> Text -> Maybe Text
searchPat toks t = case [(i, e) | i <- [0 .. T.length t], Just e <- [go toks i]] of
  ((i, e) : _) -> Just (T.take (e - i) (T.drop i t))
  [] -> Nothing
  where
    n = T.length t
    wordAt i = i >= 0 && i < n && (isAlphaNum (T.index t i) || T.index t i == '_')
    go [] i = Just i
    go (WB : r) i = if wordAt (i - 1) /= wordAt i then go r i else Nothing
    go (L l : r) i = if l `T.isPrefixOf` T.drop i t then go r (i + T.length l) else Nothing
    go (Ws : r) i =
      let k = T.length (T.takeWhile isSpace (T.drop i t))
       in if k == 0 then Nothing else go r (i + k)

-- | @scan_warnings(path)@ without the @rg@ process: each line that matches
-- a pattern, with the first pattern's match in the stripped line.
scanWarnings :: FilePath -> IO [PyVal]
scanWarnings p = do
  r <- try (readFileContents p)
  pure $ case r of
    Left (_ :: LeanError) -> []
    Right txt ->
      [ PDict [("line", PInt n), ("pattern", PStr m)]
      | (n, l) <- zip [1 ..] (T.splitOn "\n" txt)
      , let st = T.strip l
      , Just m <- [firstJust [searchPat pat st | pat <- warningPatterns]] ]
  where
    firstJust xs = case [x | Just x <- xs] of (x : _) -> Just x; [] -> Nothing

verifyTool :: Tool
verifyTool =
  Tool "lean_verify" "verify_theoremArguments" [fileField, field "theorem_name" TStr, opt "scan_source" TBool (PBool True)] run
  where
    run s a = do
      name <- checkTheoremName (argText a "theorem_name")
      rel <- requireClientForFile s (argText a "file_path") Nothing
      policy <- sessionPolicy s
      resolved <- resolveFilePath s (argText a "file_path")
      absPath <- validatePath policy (maybe (T.unpack (argText a "file_path")) id resolved)
      c <- getClient s
      openSynced s rel False
      orig <- content c rel
      let text = T.dropWhileEnd (== '\n') orig <> "\n\n#print axioms _root_." <> name <> "\n"
          snippetLine = T.count "\n" text - 1
      pool <- scratchPool s True
      r <- try (runText pool text Nothing Nothing)
      trial <- case r of
        Right t -> pure t
        Left (RequestTimeout m) -> toolError ("Axiom check timed out before `#print axioms` produced output: " <> m)
        Left e -> toolError (errorText e)
      let appended = [d | d <- drItems (trDiagnostics trial), maybe (-1) id (intAt ["start", "line"] (maybe (PDict []) id (rangeOf "fullRange" "range" d))) >= snippetLine]
          errs = [msgOf d | d <- appended, get "severity" d == Just (PInt 1)]
      if not (null errs) then toolError ("Axiom check failed: " <> T.intercalate "; " errs) else pure ()
      let axs = parseAxioms appended
      if null axs && not (any (("does not depend on any axioms" `T.isInfixOf`) . msgOf) appended)
        then toolError "Axiom check produced no `#print axioms` output — result is inconclusive (elaboration may not have reached the check)."
        else pure ()
      warnings <-
        if argBool a "scan_source"
          then do
            rg <- leRgAvailable (sessionEnv s)
            if rg then scanWarnings absPath else pure [PDict [("line", PInt 0), ("pattern", PStr "ripgrep (rg) not installed - warnings unavailable")]]
          else pure []
      let (trust, nonStd) = classifyAxioms axs
      pure (Model (PDict [("axioms", PList (map PStr axs)), ("trust", PStr trust), ("non_standard_axioms", PList (map PStr nonStd)), ("warnings", PList warnings)]))

-- ---------------------------------------------------------------- minimal hypotheses

isWordCh :: Char -> Bool
isWordCh ch = isAlphaNum ch || ch == '_'

-- | The end of the first @\\b(theorem|lemma|example|def)\\s+NAME(?![A-Za-z0-9_'])@.
declMatchEnd :: Text -> Text -> Maybe Int
declMatchEnd src name =
  case [e | i <- [0 .. T.length src - 1], Just e <- [at i]] of
    (e : _) -> Just e
    [] -> Nothing
  where
    n = T.length src
    at i
      | i > 0 && isWordCh (T.index src (i - 1)) = Nothing
      | otherwise =
          case [T.length k | k <- ["theorem", "lemma", "example", "def"], k `T.isPrefixOf` T.drop i src] of
            (k : _) ->
              let j = i + k
                  ws = T.length (T.takeWhile isSpace (T.drop j src))
                  e = j + ws + T.length name
               in if ws > 0 && name `T.isPrefixOf` T.drop (j + ws) src && not (e < n && identRest (T.index src e))
                    then Just e
                    else Nothing
            [] -> Nothing

theoremDeclared :: Text -> Text -> Bool
theoremDeclared src name = declMatchEnd src name /= Nothing

-- | @find_theorem_binders(source, name)@: (text, start, end) of each
-- top-level bracket group after the name.
findTheoremBinders :: Text -> Text -> [(Text, Int, Int)]
findTheoremBinders src name = maybe [] (go . skipWs) (declMatchEnd src name)
  where
    n = T.length src
    skipWs p = p + T.length (T.takeWhile (`elem` (" \t\n\r" :: String)) (T.drop p src))
    go p
      | p >= n = []
      | otherwise =
          let ch = T.index src p
           in case lookup ch [('(', ')'), ('{', '}'), ('[', ']')] of
                Just close -> case balanced p ch close of
                  Just e -> (T.take (e + 1 - p) (T.drop p src), p, e + 1) : go (skipWs (e + 1))
                  Nothing -> []
                Nothing -> []
    balanced p open close =
      let step (depth, found) (i, ch)
            | found /= Nothing = (depth, found)
            | ch == open = (depth + 1, Nothing)
            | ch == close = if depth - 1 == 0 then (0, Just i) else (depth - 1, Nothing)
            | otherwise = (depth, Nothing)
       in snd (foldl step (0 :: Int, Nothing) (zip [p ..] (T.unpack (T.drop p src))))

dropBinder :: Text -> Int -> Int -> Text
dropBinder src start end =
  let cut = if start > 0 && T.index src (start - 1) == ' ' then start - 1 else start
   in T.take cut src <> T.drop end src

minimalHypothesesTool :: Tool
minimalHypothesesTool =
  Tool "lean_minimal_hypotheses" "minimal_hypothesesArguments"
    [fileField, field "theorem_name" TStr, le "300.0" (ge "5.0" (opt "inactivity_timeout" TFloat (PFloat 60)))]
    run
  where
    run s a = do
      name <- checkTheoremName (argText a "theorem_name")
      rel <- requireClientForFile s (argText a "file_path") Nothing
      c <- getClient s
      openSynced s rel False
      orig <- content c rel
      let bare = last (T.splitOn "." name)
      if not (theoremDeclared orig bare) then toolError ("Could not find theorem '" <> name <> "' in " <> rel <> ".") else pure ()
      let binders = findTheoremBinders orig bare
          explicit = [b | b@(t, _, _) <- binders, "(" `T.isPrefixOf` t, ":" `T.isInfixOf` t]
          skipped = length binders - length explicit
          result vs = Model (PDict [("theorem_name", PStr name), ("file", PStr rel), ("verdicts", PList vs), ("skipped_implicit", PInt (fromIntegral skipped))])
          timeoutS = argDouble a "inactivity_timeout"
      if null explicit
        then pure (result [])
        else do
          baseline <- diagnostics c rel True (Just timeoutS) False
          let baseKeys = Set.fromList [errorKey d | d <- drItems baseline, get "severity" d == Just (PInt 1)]
          pool <- scratchPool s False
          limit <- maxOutputChars s
          trials <- runTextsEach pool [dropBinder orig st en | (_, st, en) <- explicit] (Just timeoutS)
          let verdict (b, _, _) r =
                let binder = T.strip b
                    mk status breaks detail = PDict [("binder", PStr binder), ("status", PStr status), ("breaks", PList breaks), ("detail", PStr detail)]
                 in case r of
                      Left (RequestTimeout _) -> mk "error" [] ("elaboration timed out after " <> pyFloatRepr timeoutS <> "s")
                      Left e -> mk "error" [] (T.take 200 (errorText e))
                      Right trial ->
                        let new = [d | d <- drItems (trDiagnostics trial), get "severity" d == Just (PInt 1), not (Set.member (errorKey d) baseKeys)]
                         in if null new then mk "removable" [] "" else mk "load-bearing" [m | Just m <- map (diagnosticMessage limit) new] ""
          pure (result (zipWith verdict explicit trials))
    errorKey d =
      let r = case get "fullRange" d of
            Just v | truthy v -> v
            Just _ -> PDict []
            Nothing -> case get "range" d of Just v | truthy v -> v; _ -> PDict []
       in (maybe (-1) id (intAt ["start", "line"] r), maybe (-1) id (intAt ["start", "character"] r), T.take 120 (msgOf d))

