-- | The mistakes agents keep making, found by rules over one attempt's
-- events (the @tool_start@, @tool_end@ and @message@ events of the run log)
-- and its outcome text.
--
-- What counts:
--
-- * Tool names are matched by suffix (@lean_run_code@), since the native
--   agent (@mcp_lean_lsp_<tool>@) and Claude Code (@mcp__lean-lsp__<tool>@)
--   prefix them differently; the suffix is the whole name or follows one of
--   @_ - . : /@, so @TodoWrite@ is not @Write@.
-- * A call is a @tool_start@ paired with its @tool_end@ ('toolCalls'): by
--   @call_id@, else (native runs logged before starts carried one) with the
--   one open start of the same name; with several open, not at all. An end
--   with no start is a call with no arguments.
-- * A write is a write call whose end does not say @ok: false@.
-- * Lean diagnostics are read from the @result@ of a @tool_end@ of a Lean
--   tool (@lean_diagnostic_messages@, @lean_run_code@, @lean_multi_attempt@,
--   @lean_verify@, @lean_build@): every @message@ string of the JSON when the
--   result parses, else the raw text.
-- * An unknown identifier counts only when it looks like a declaration (a
--   dot in it, or an underscore and 4 characters or more), not when a
--   @lean_run_code@ of only @#check@ / @#print@ / @#find@ lines probed it,
--   and not from a @lean_multi_attempt@ where some snippet succeeded.
-- * A deprecation counts only when the old name is in the attempt's own
--   code (its writes and runs).
-- * @no_check_before_finish@ does not apply to an attempt that timed out.
-- * Lean 3 markers are read from the arguments of the writes and of
--   @lean_run_code@ / @lean_multi_attempt@ calls, with line comments cut:
--   every string value except a path, the @old_string@ an edit replaces,
--   and the removed (@-@) lines of a V4A patch.
-- * A @repeated_call@ is the same tool with the same arguments 3 times with
--   no write in between (a check rerun after each edit is not a repeat); a
--   write repeated 3 times anywhere counts. A failed write is an ordinary
--   call.
-- * An @smt_unknown_loop@ is 3 or more @smt_check@ ends in a row whose
--   @result@ is @unknown@ or @error@; it carries no name.
-- * The outcome kinds come from the outcome text; they carry the seq of the
--   attempt's last event.
--
-- No classifier throws: odd shapes, missing fields and non-JSON results
-- simply give no hit.
module Fisher.Learn.Mistakes
  ( MistakeKind (..)
  , renderMistakeKind
  , parseMistakeKind
  , Mistake (..)
  , AttemptView (..)
  , classify
  , mistakeJson
  , resultText
  , ToolCall (..)
  , toolCalls
  ) where

import Data.Aeson (Value (..), decodeStrict, encode, object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum, isAscii, isAsciiLower, isDigit, isSpace)
import Data.Foldable (toList)
import Data.List (sortOn)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

data MistakeKind
  = UnknownIdentifier
  | Deprecated
  | Lean3Syntax
  | SorryLeft
  | StatementChanged
  | AxiomUsed
  | TimeoutLoop
  | RepeatedCall
  | NoCheckBeforeFinish
  | BudgetBurn
  | SmtUnknownLoop
  deriving (Eq, Ord, Show, Enum, Bounded)

renderMistakeKind :: MistakeKind -> Text
renderMistakeKind = \case
  UnknownIdentifier -> "unknown_identifier"
  Deprecated -> "deprecated"
  Lean3Syntax -> "lean3_syntax"
  SorryLeft -> "sorry_left"
  StatementChanged -> "statement_changed"
  AxiomUsed -> "axiom_used"
  TimeoutLoop -> "timeout_loop"
  RepeatedCall -> "repeated_call"
  NoCheckBeforeFinish -> "no_check_before_finish"
  BudgetBurn -> "budget_burn"
  SmtUnknownLoop -> "smt_unknown_loop"

parseMistakeKind :: Text -> Maybe MistakeKind
parseMistakeKind t = lookup t [(renderMistakeKind k, k) | k <- [minBound .. maxBound]]

-- | One hit: the kind, the name it carries (the identifier, the old name,
-- the Lean 3 marker, the axiom), the replacement name of a deprecation, and
-- the seq of the event it was found at.
data Mistake = Mistake { mKind :: MistakeKind, mName :: Maybe Text, mNew :: Maybe Text, mSeq :: Int }
  deriving (Eq, Show)

-- | One attempt: its events as (seq, t, kind, data), its outcome text (for
-- example @rejected: still sorry@), its outcome class (@closed@, @failed@,
-- @stalled@, @timeout@, ...), its goal and its shape.
data AttemptView = AttemptView
  { avEvents :: [(Int, Double, Text, Value)]
  , avOutcomeText :: Text
  , avClass :: Text
  , avGoal :: Text
  , avShape :: Text
  }
  deriving (Show)

-- | The @mistake@ event data: goal, shape, the hit.
mistakeJson :: Text -> Text -> Mistake -> Value
mistakeJson goal shape m =
  object
    [ "kind" .= renderMistakeKind (mKind m), "name" .= mName m, "new" .= mNew m
    , "seq" .= mSeq m, "goal" .= goal, "shape" .= shape ]

-- | Every hit, in seq order, the first of each (kind, name) kept.
classify :: AttemptView -> [Mistake]
classify av = dedup (sortOn mSeq (eventHits <> outcomeHits))
  where
    es = avEvents av
    cs = toolCalls es
    lastSeq = foldr (\(s, _, _, _) m -> max s m) 0 es
    writes = filter isWrite cs
    eventHits =
      concat
        [ diagnosticHits cs
        , lean3Hits cs
        , timeoutHits cs
        , smtUnknownHits cs
        , repeatHits cs
        , if avClass av == "timeout" then [] else noCheckHits cs writes
        , [Mistake BudgetBurn Nothing Nothing lastSeq | avClass av `elem` ["stalled", "timeout"], null writes]
        ]
    outcomeHits = [Mistake k n Nothing lastSeq | (k, n) <- outcomeKinds (avOutcomeText av)]

dedup :: [Mistake] -> [Mistake]
dedup = go Set.empty
  where
    go _ [] = []
    go seen (m : ms)
      | Set.member k seen = go seen ms
      | otherwise = m : go (Set.insert k seen) ms
      where k = (mKind m, mName m)

-- Tools ----------------------------------------------------------------------

diagTools, loopTools, checkTools, writeTools :: [Text]
diagTools = ["lean_diagnostic_messages", "lean_run_code", "lean_multi_attempt", "lean_verify", "lean_build"]
loopTools = ["lean_run_code", "lean_multi_attempt"]
checkTools = ["lean_diagnostic_messages", "lean_verify", "lean_build"]
writeTools = ["write_file", "patch", "Write", "Edit", "MultiEdit"]

isTool :: [Text] -> Text -> Bool
isTool suffixes n = not (T.null n) && any suffix suffixes
  where suffix sfx = n == sfx || any (\sep -> (sep <> sfx) `T.isSuffixOf` n) ["_", "-", ".", ":", "/"]

field :: Text -> Value -> Maybe Value
field k = \case
  Object o -> KM.lookup (K.fromText k) o
  _ -> Nothing

textField :: Text -> Value -> Maybe Text
textField k v = case field k v of
  Just (String s) -> Just s
  _ -> Nothing

toolName :: Value -> Text
toolName = fromMaybe "" . textField "name"

-- | One tool call: the seq and name of its start (of its end, for an end
-- with no start), its args (Nothing for an end with no start), and its end
-- (seq and data) when it has one.
data ToolCall = ToolCall { tcSeq :: Int, tcName :: Text, tcArgs :: Maybe Value, tcEnd :: Maybe (Int, Value) }
  deriving (Show)

-- | The calls of a list of events, in seq order. An end pairs with the
-- start of its @call_id@, else with the one open start of its name that has
-- no @call_id@. When several such starts are open (a concurrent batch of
-- one tool, whose ends come in the order the calls finish) the end pairs
-- with none of them.
toolCalls :: [(Int, Double, Text, Value)] -> [ToolCall]
toolCalls es = IM.elems final
  where
    (final, _, _, _) = foldl step (IM.empty, Map.empty, Map.empty, 0 :: Int) (sortOn (\(s, _, _, _) -> s) es)
    step st@(acc, byId, queues, n) (s, _, k, d) = case k of
      "tool_start" ->
        let c = ToolCall s (toolName d) (Just (fromMaybe Null (field "args" d))) Nothing
            acc' = IM.insert n c acc
         in case callId d of
              Just i -> (acc', Map.insert i n byId, queues, n + 1)
              Nothing -> (acc', byId, Map.insertWith (\(new, _) (old, amb) -> (old <> new, amb)) (toolName d) ([n], False) queues, n + 1)
      "tool_end" ->
        let close ix = IM.adjust (\c -> c {tcEnd = Just (s, d)}) ix acc
            orphan = IM.insert n (ToolCall s (toolName d) Nothing (Just (s, d))) acc
         in case callId d >>= (`Map.lookup` byId) of
              Just ix -> (close ix, Map.delete (fromMaybe "" (callId d)) byId, queues, n)
              Nothing -> case Map.lookup (toolName d) queues of
                Just ([ix], False) -> (close ix, byId, Map.delete (toolName d) queues, n)
                -- several calls of the name open at once: which one ended is
                -- not known, so the end stays alone and one call is taken as
                -- closed; the name stays unpaired until all of them are
                Just (_ : rest, _) ->
                  (orphan, byId, if null rest then Map.delete (toolName d) queues else Map.insert (toolName d) (rest, True) queues, n + 1)
                _ -> (orphan, byId, queues, n + 1)
      _ -> st
    callId d = case textField "call_id" d of
      Just i | not (T.null i) -> Just i
      _ -> Nothing

-- | A write that did not fail.
isWrite :: ToolCall -> Bool
isWrite c = isTool writeTools (tcName c) && not failed
  where
    failed = case tcEnd c of
      Just (_, d) -> field "ok" d == Just (Bool False)
      Nothing -> False

-- | A @tool_end@ event's result as text: the text itself, a JSON value encoded, or
-- the preview.
resultText :: Value -> Text
resultText d = case field "result" d of
  Just (String t) -> t
  Just v | v /= Null -> jsonText v
  _ -> fromMaybe "" (textField "preview" d)

jsonText :: Value -> Text
jsonText = TE.decodeUtf8With TEE.lenientDecode . BL.toStrict . encode

-- | The calls that ended, with the end's seq and result text.
ended :: [ToolCall] -> [(ToolCall, Int, Text)]
ended cs = [(c, s, resultText d) | c <- cs, Just (s, d) <- [tcEnd c]]

-- Diagnostics ----------------------------------------------------------------

diagnosticHits :: [ToolCall] -> [Mistake]
diagnosticHits cs =
  concat
    [ [Mistake UnknownIdentifier (Just x) Nothing s | x <- unknownNames t, declLike x, not (probed c x), not (recovered c r)]
        <> [Mistake Deprecated (Just old) new s | (Just old, new) <- deprecations t, old `T.isInfixOf` ownCode]
    | (c, s, r) <- ended cs, isTool diagTools (tcName c), t <- diagTexts r ]
  where
    ownCode = T.unlines [x | c <- cs, isTool (writeTools <> loopTools) (tcName c), Just a <- [tcArgs c], x <- codeStrings a]
    -- a lemma name, not a local: a dot, or an underscore and 4 characters,
    -- and not a hypothesis name (h_pos, hx_le, ih_n, this_1)
    declLike x = (T.any (== '.') x || (T.any (== '_') x && T.length x >= 4)) && not (hypothesis x)
    hypothesis x = or
      [ rest == "" || ("_" `T.isPrefixOf` rest && T.all (\c -> isAscii c && (isAlphaNum c || c `elem` ("_'" :: String))) rest)
      | p <- ["h", "hx", "hy", "hz", "hn", "hm", "hk", "ih", "this"], Just rest <- [T.stripPrefix p x] ]
    -- a lean_run_code of only #check / #print / #find lines naming x
    probed c x =
      isTool ["lean_run_code"] (tcName c) && case tcArgs c of
        Just a ->
          let code = T.unlines (codeStrings a)
              ls = filter (not . T.null) (map T.strip (T.lines code))
           in not (null ls) && all (\l -> any (`T.isPrefixOf` l) ["#check", "#print", "#find"]) ls && x `T.isInfixOf` code
        Nothing -> False
    -- a lean_multi_attempt where some snippet had no error
    recovered c r = isTool ["lean_multi_attempt"] (tcName c) && case decodeStrict (TE.encodeUtf8 r) of
      Just v | Just (Array items) <- field "items" v -> any clean (toList items)
      _ -> False
    clean item = case field "diagnostics" item of
      Just (Array ds) -> not (any (\d -> textField "severity" d == Just "error") (toList ds))
      Nothing -> True
      _ -> False

-- | The diagnostic messages of a result: every @message@ string of the JSON,
-- else the raw text.
diagTexts :: Text -> [Text]
diagTexts r = case decodeStrict (TE.encodeUtf8 r) of
  Just v@(Object _) -> messages v
  Just v@(Array _) -> messages v
  Just (String s) -> [s]
  _ -> [r]
  where
    messages = \case
      Object o -> concat [if k == "message" then strs v else messages v | (k, v) <- KM.toList o]
      Array a -> concatMap messages (toList a)
      _ -> []
    strs = \case
      String s -> [s]
      v -> messages v

-- | The X of @unknown identifier 'X'@ and @unknown constant 'X'@.
unknownNames :: Text -> [Text]
unknownNames t = concatMap names ["unknown identifier '", "unknown constant '"]
  where
    names pre = mapMaybe (quoted . T.drop (T.length pre) . snd) (T.breakOnAll pre t)
    quoted rest =
      let tok = T.takeWhile nameChar rest
          name = if T.any (== '\'') tok then T.dropEnd 1 (fst (T.breakOnEnd "'" tok)) else tok
       in if T.null name then Nothing else Just (T.take 200 name)
    nameChar c = not (isSpace c) && c `notElem` ("\"\\,;()[]{}`" :: String)

-- | Each @has been deprecated@: the name before it (backticked, else the last
-- word) and the @use Y@ after it on the same line.
deprecations :: Text -> [(Maybe Text, Maybe Text)]
deprecations t = [(oldName before, newName (T.drop (T.length pre) rest)) | (before, rest) <- T.breakOnAll pre t]
  where
    pre = "has been deprecated"
    oldName before =
      let b = T.stripEnd before
       in if "`" `T.isSuffixOf` b
            then nonEmpty (T.takeWhileEnd (\c -> c /= '`' && c /= '\n') (T.dropEnd 1 b))
            else nonEmpty (T.takeWhileEnd isNameChar b)
    newName after =
      let line = T.take 300 (T.takeWhile (/= '\n') after)
          (_, r) = T.breakOn "use " line
          u = T.stripStart (T.drop 4 r)
       in if T.null r
            then Nothing
            else
              if "`" `T.isPrefixOf` u
                then nonEmpty (T.takeWhile (/= '`') (T.drop 1 u))
                else nonEmpty (T.dropWhileEnd (`elem` (".,;:" :: String)) (T.takeWhile (not . isSpace) u))
    nonEmpty x = let y = T.take 200 (T.strip x) in if T.null y then Nothing else Just y
    isNameChar c = isAlphaNum c || c `elem` ("_.'!?" :: String)

-- Lean 3 ---------------------------------------------------------------------

lean3Hits :: [ToolCall] -> [Mistake]
lean3Hits cs =
  [ Mistake Lean3Syntax (Just m) Nothing (tcSeq c)
  | c <- cs, isTool (writeTools <> loopTools) (tcName c), Just a <- [tcArgs c], m <- lean3Markers (T.unlines (codeStrings a)) ]

-- | The code an edit or a run carries: every string value but paths, the
-- text an edit replaces, and the removed lines of a patch.
codeStrings :: Value -> [Text]
codeStrings = \case
  String s -> [s]
  Object o -> concat [inKey k v | (k, v) <- KM.toList o]
  Array a -> concatMap codeStrings (toList a)
  _ -> []
  where
    inKey k v
      | k `elem` ["path", "file_path", "old_string", "old_str", "mode"] = []
      | k == "patch" = map (T.unlines . filter (not . ("-" `T.isPrefixOf`)) . T.lines) (codeStrings v)
      | otherwise = codeStrings v

lean3Markers :: Text -> [Text]
lean3Markers code =
  [m | (m, True) <- [("λ x,", any lambdaComma ls), ("assume", any assume ls), ("cases h with", any casesWith ls), ("begin ... end", beginEnd)]]
    <> Set.toList (Set.fromList (mapMaybe checkNs ls))
  where
    ls = map (fst . T.breakOn "--") (T.lines code)
    -- each occurrence of a word: the text after it, when it stands alone on its left
    after w l = [T.drop (T.length w) r | (b, r) <- T.breakOnAll w l, maybe True (not . identChar . snd) (T.unsnoc b)]
    assume l = any (maybe False (isSpace . fst) . T.uncons) (after "assume" l)
    casesWith l = any casesRest (after "cases" l)
    casesRest r = case T.uncons r of
      Just (c, _) | isSpace c ->
        let (_, w) = T.breakOn " with " r
            next = T.stripStart (T.drop 6 w)
         in not (T.null w) && maybe False ((/= '|') . fst) (T.uncons next)
      _ -> False
    lambdaComma l = any (commaFirst (0 :: Int)) [T.drop 1 r | (_, r) <- T.breakOnAll "λ" l]
    commaFirst depth s
      | "=>" `T.isPrefixOf` s = False
      | otherwise = case T.uncons s of
          Nothing -> False
          Just (c, rest)
            | c == '↦' -> False
            | c == ',' && depth == 0 -> True
            | c `elem` ("([{⟨" :: String) -> commaFirst (depth + 1) rest
            | c `elem` (")]}⟩" :: String) -> depth > 0 && commaFirst (depth - 1) rest
            | otherwise -> commaFirst depth rest
    stripped = map T.strip ls
    beginEnd =
      case break (\l -> l == "begin" || ":= begin" `T.isSuffixOf` l || ":=begin" `T.isSuffixOf` l) stripped of
        (_, _ : rest) -> "end" `elem` rest
        _ -> False
    checkNs l = do
      r <- T.stripPrefix "#check" (T.strip l)
      let r' = T.stripStart r
          ns = T.takeWhile (\c -> isAsciiLower c || isDigit c || c == '_') r'
      (c0, _) <- T.uncons ns
      if isAsciiLower c0 && "." `T.isPrefixOf` T.drop (T.length ns) r' then Just ("#check " <> ns <> ".") else Nothing
    identChar c = isAlphaNum c || c `elem` ("_.'" :: String)

-- Calls ----------------------------------------------------------------------

-- | Three consecutive timed-out results among the run_code and
-- multi_attempt ends.
timeoutHits :: [ToolCall] -> [Mistake]
timeoutHits cs = take 1 (go (0 :: Int) (sortOn fst [(s, r) | (c, s, r) <- ended cs, isTool loopTools (tcName c)]))
  where
    go _ [] = []
    go k ((s, r) : rest)
      | timedOut r = if k + 1 >= 3 then [Mistake TimeoutLoop Nothing Nothing s] else go (k + 1) rest
      | otherwise = go 0 rest

-- | Three consecutive @smt_check@ ends whose result is @unknown@ or
-- @error@.
smtUnknownHits :: [ToolCall] -> [Mistake]
smtUnknownHits cs = take 1 (go (0 :: Int) (sortOn fst [(s, r) | (c, s, r) <- ended cs, isTool ["smt_check"] (tcName c)]))
  where
    go _ [] = []
    go k ((s, r) : rest)
      | undecided r = if k + 1 >= 3 then [Mistake SmtUnknownLoop Nothing Nothing s] else go (k + 1) rest
      | otherwise = go 0 rest

-- | An @smt_check@ result that is @unknown@ or @error@: its JSON's
-- @result@, or for a cut preview the field's text.
undecided :: Text -> Bool
undecided r = case decodeStrict (TE.encodeUtf8 r) >>= textField "result" of
  Just v -> v `elem` ["unknown", "error"]
  Nothing ->
    let flat = T.filter (not . isSpace) r
     in any (`T.isInfixOf` flat) ["\"result\":\"unknown\"", "\"result\":\"error\""]

timedOut :: Text -> Bool
timedOut r =
  let l = T.toLower r
   in any (`T.isInfixOf` l) ["timed out", "diagnostics_timed_out", "deterministic) timeout", "maximum number of heartbeats"]
        || "\"timed_out\":true" `T.isInfixOf` T.filter (not . isSpace) l

-- | The third call of a tool with the same arguments, with no write between
-- (a write counts anywhere).
repeatHits :: [ToolCall] -> [Mistake]
repeatHits = take 1 . go (0 :: Int) Map.empty
  where
    go _ _ [] = []
    go epoch seen (c : rest) = case tcArgs c of
      Nothing -> go epoch seen rest
      Just a ->
        let w = isWrite c
            key = (tcName c, encode a, if w then -1 else epoch)
            n = Map.findWithDefault (0 :: Int) key seen + 1
            hit = [Mistake RepeatedCall Nothing Nothing (tcSeq c) | n == 3]
         in hit <> go (if w then epoch + 1 else epoch) (Map.insert key n seen) rest

-- | A write with no check call after the last one.
noCheckHits :: [ToolCall] -> [ToolCall] -> [Mistake]
noCheckHits cs writes = case reverse writes of
  [] -> []
  (w : _)
    | any (\c -> isTool checkTools (tcName c) && tcSeq c > tcSeq w) cs -> []
    | otherwise -> [Mistake NoCheckBeforeFinish Nothing Nothing (tcSeq w)]

-- Outcome --------------------------------------------------------------------

outcomeKinds :: Text -> [(MistakeKind, Maybe Text)]
outcomeKinds o =
  [(SorryLeft, Nothing) | "still sorry" `T.isInfixOf` o || "uses sorry" `T.isInfixOf` o]
    <> [(StatementChanged, Nothing) | "elaborates to a different type" `T.isInfixOf` o]
    <> [(AxiomUsed, firstAxiom r) | let (_, r) = T.breakOn axioms o, not (T.null r)]
  where
    axioms = "axioms beyond the allowed"
    firstAxiom r =
      let rest = T.stripStart (T.drop (T.length axioms) r)
          list = T.stripStart (fromMaybe rest (T.stripPrefix ":" rest))
          a = T.take 200 (T.strip (T.takeWhile (\c -> c /= ',' && c /= '\n') list))
       in if T.null a then Nothing else Just a
