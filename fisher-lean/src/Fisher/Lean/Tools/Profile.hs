-- | @lean_profile_proof@ (lean-lsp-mcp @profile_utils.py@): the file's
-- header and one theorem written to a temporary file in the project,
-- @lake env lean --profile@ on it, and the trace output folded into
-- per-line and per-category times. The regexes are ported by hand.
module Fisher.Lean.Tools.Profile
  ( profileTool
  , parseOutput
  , extractTheoremSource
  , lineTimes
  , pyRound1
  ) where

import Control.Exception (IOException, finally, try)
import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.List (sortOn)
import qualified Data.ByteString as BS
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import System.Directory (removeFile)
import System.IO (hClose, openTempFile)

import Fisher.Lean.Process (ioErrorText)
import Fisher.Lean.Session
import Fisher.Lean.Text (readTextUtf8, splitLines)
import Fisher.Lean.Tool
import Fisher.Lean.Tools.Common (fileField, tshow)
import Fisher.Py.Json (pyFloatRepr)
import Fisher.Py.Value
import Fisher.Tools.Py (pyFloat)

-- | Python's @round(x, 1)@ (ties to even on the exact binary value).
pyRound1 :: Double -> Double
pyRound1 x
  | isNaN x || isInfinite x = x
  | otherwise = fromRational (fromIntegral (round (toRational x * 10) :: Integer) / 10)

profileTool :: Tool
profileTool =
  Tool "lean_profile_proof" "profile_proofArguments"
    [fileField, ge "1" (field "line" TInt), ge "1" (opt "top_n" TInt (PInt 5)), ge "1" (opt "timeout" TFloat (PFloat 60))]
    run
  where
    run s a = do
      let fp = argText a "file_path"
      bound <- boundProject s
      resolved <- resolveFilePath s fp
      file <- maybe (toolError ("[Errno 2] No such file or directory: '" <> fp <> "'")) pure resolved
      project <- case bound of
        Just p -> pure (Just p)
        Nothing -> inferProjectPath s file
      root <- maybe (toolError "Lean project not found") pure project
      policy <- pathPolicy s root
      path <- validatePath policy file
      Model <$> profileTheorem s path (argInt a "line") root (argDouble a "timeout") (argInt a "top_n")

-- ---------------------------------------------------------------- source

-- | @_DECL_RE@: @^\\s*(?:private\\s+)?(theorem|lemma|def)\\s+(\\S+)@, the name.
declName :: Text -> Maybe Text
declName l =
  let s0 = T.dropWhile isSpace l
      s1 = case T.stripPrefix "private" s0 of
        Just r | Just (ch, _) <- T.uncons r, isSpace ch -> T.dropWhile isSpace r
        _ -> s0
      try' x = case [r | k <- ["theorem", "lemma", "def"], Just r <- [T.stripPrefix k x]] of
        (r : _) | Just (ch, _) <- T.uncons r, isSpace ch -> let nm = T.takeWhile (not . isSpace) (T.dropWhile isSpace r) in if T.null nm then Nothing else Just nm
        _ -> Nothing
   in case try' s1 of
        Just n -> Just n
        Nothing -> if s1 /= s0 then try' s0 else Nothing

headerLine :: Text -> Bool
headerLine l = or [maybe False (isSpace . fst) (T.uncons =<< T.stripPrefix k l) | k <- ["import", "open", "set_option", "universe", "variable"]]

findHeaderEnd :: [Text] -> Int
findHeaderEnd = go 0 False . zip [0 ..]
  where
    go acc _ [] = acc
    go acc inBlock ((i, l) : rest) =
      let s = T.strip l
          b1 = inBlock || "/-" `T.isInfixOf` l
          b2 = if "-/" `T.isInfixOf` l then False else b1
       in if b2 || T.null s || "--" `T.isPrefixOf` s || headerLine l
            then go (i + 1) b2 rest
            else
              if "namespace" `T.isPrefixOf` s || "section" `T.isPrefixOf` s
                then go (i + 1) b2 rest
                else
                  if declName l /= Nothing || any (`T.isPrefixOf` s) ["@[", "private ", "protected "]
                    then acc
                    else go (i + 1) b2 rest

-- | @_extract_theorem_source(lines, target_line)@: source, name, theorem
-- start line in the source.
extractTheoremSource :: [Text] -> Int -> Either Text (Text, Text, Int)
extractTheoremSource ls target = case declName (ls !! (target - 1)) of
  Nothing -> Left ("No theorem/lemma/def at line " <> tshow target)
  Just name ->
    let headerEnd = findHeaderEnd ls
        theoremEnd = case [i | i <- [target .. length ls - 1], declName (ls !! i) /= Nothing] of
          (i : _) -> i
          [] -> length ls
        header = T.intercalate "\n" (take headerEnd ls)
        theorem = T.intercalate "\n" (take (theoremEnd - (target - 1)) (drop (target - 1) ls))
     in Right (header <> "\n\n" <> theorem <> "\n", name, headerEnd + 2)

findProofStart :: [Text] -> Either Text Int
findProofStart ls = case [i + 1 | (i, l) <- zip [0 ..] ls, ":= by" `T.isInfixOf` l || " by" `T.isSuffixOf` T.stripEnd l] of
  (i : _) -> Right i
  [] -> Left "No 'by' proof found in theorem"

proofItems :: [Text] -> Int -> [(Int, Text, Bool)]
proofItems ls start =
  [ (i + 1, T.dropWhile (`elem` ("·*- \t" :: String)) s, T.head s `elem` ("·*-" :: String))
  | i <- [start .. length ls - 1], let s = T.strip (ls !! i), not (T.null s), not ("--" `T.isPrefixOf` s) ]

-- ---------------------------------------------------------------- output

type Trace = (Int, Text, Double, Text)

-- | @_parse_output(output)@: traces (depth, class, ms, message) and the
-- cumulative category times, in order.
parseOutput :: Text -> ([Trace], [(Text, Double)])
parseOutput out = go False (splitLines out) [] []
  where
    go _ [] ts cs = (reverse ts, dedupe (reverse cs))
    go inCum (l : rest) ts cs
      | "cumulative profiling times:" `T.isInfixOf` l = go True rest ts cs
      | inCum = case cumulative l of
          Just c -> go inCum rest ts (c : cs)
          Nothing -> go inCum rest ts cs
      | otherwise = case trace l of
          Just t -> go inCum rest (t : ts) cs
          Nothing -> go inCum rest ts cs
    -- a dict: a later value replaces an earlier one in its first place
    dedupe = foldl (\acc (k, v) -> if any ((== k) . fst) acc then [(k', if k' == k then v else v') | (k', v') <- acc] else acc <> [(k, v)]) []

-- | @_TRACE_RE@: @^(\\s*)\\[([^\\]]+)\\]\\s+\\[([\\d.]+)\\]\\s+(.+)$@.
trace :: Text -> Maybe Trace
trace l = do
  let (ind, r0) = T.span isSpace l
  r1 <- T.stripPrefix "[" r0
  let (cls, r2) = T.break (== ']') r1
  if T.null cls then Nothing else Just ()
  r3 <- T.stripPrefix "]" r2
  let (ws1, r4) = T.span isSpace r3
  if T.null ws1 then Nothing else Just ()
  r5 <- T.stripPrefix "[" r4
  let (num, r6) = T.span (\ch -> isDigit ch || ch == '.') r5
  if T.null num then Nothing else Just ()
  r7 <- T.stripPrefix "]" r6
  let (ws2, msg0) = T.span isSpace r7
  if T.null ws2 then Nothing else Just ()
  msg <- if not (T.null msg0) then Just msg0 else if T.length ws2 >= 2 then Just (T.takeEnd 1 ws2) else Nothing
  secs <- pyFloat num
  pure (T.length ind `div` 2, cls, secs * 1000, msg)

-- | @_CUMULATIVE_RE@: @^\\s+(\\S+(?:\\s+\\S+)*)\\s+([\\d.]+)(ms|s)$@.
cumulative :: Text -> Maybe (Text, Double)
cumulative l = do
  let (ind, r) = T.span isSpace l
  if T.null ind then Nothing else Just ()
  let tok = T.takeWhileEnd (not . isSpace) r
      before = T.dropEnd (T.length tok) r
      (num, unit) = T.span (\ch -> isDigit ch || ch == '.') tok
  if T.null num || unit `notElem` ["ms", "s"] then Nothing else Just ()
  if T.null before || not (isSpace (T.last before)) then Nothing else Just ()
  let cat = T.stripEnd before
  if T.null cat then Nothing else Just ()
  v <- pyFloat num
  pure (cat, v * (if unit == "s" then 1000 else 1))

stripStatus :: Text -> Text
stripStatus t = case [r | p <- ["\x2705", "\x274c", "\x1f4a5"], Just r <- [T.stripPrefix p t]] of
  (r : _) -> T.dropWhile isSpace (maybe r id (T.stripPrefix "\xfe0f" r))
  [] -> t

wordBoundaryMatch :: Text -> Text -> Bool
wordBoundaryMatch name msg =
  let n = T.length msg
      w i = i >= 0 && i < n && (isAlphaNum (T.index msg i) || T.index msg i == '_')
      boundary i = w (i - 1) /= w i
      hits = [i | i <- [0 .. n - T.length name], name `T.isPrefixOf` T.drop i msg]
   in any (\i -> boundary i && boundary (i + T.length name)) hits

-- | @_extract_line_times(traces, name, proof_items)@: per-line ms in first
-- hit order, and the total.
lineTimes :: [Trace] -> Text -> [(Int, Text, Bool)] -> ([(Int, Double)], Double)
lineTimes traces name items = go traces [] 0 0 False Nothing []
  where
    go [] acc total _ _ _ _ = (acc, total)
    go ((depth, cls, ms, msg) : rest) acc total vdepth inValue tdepth used
      | cls == "Elab.definition.value" && wordBoundaryMatch name msg = go rest acc ms depth True tdepth used
      | cls == "Elab.async" && ("proof of " <> name) `T.isInfixOf` msg = go rest acc (max total ms) vdepth inValue tdepth used
      | inValue =
          if depth <= vdepth
            then (acc, total)
            else
              if cls == "Elab.step"
                then
                  let headT = stripStatus (T.strip (head (T.splitOn "\n" msg)))
                   in if not (T.null headT) && not ("expected type:" `T.isPrefixOf` headT)
                        then
                          let td = case tdepth of Just d | d /= 0 -> d; _ -> depth
                           in if depth == td
                                then
                                  let tactic = T.dropWhile (`elem` ("·*- \t" :: String)) headT
                                   in case matchLine tactic (T.null tactic) used of
                                        Just ln -> go rest (addTo ln ms acc) total vdepth inValue (Just td) (ln : used)
                                        Nothing -> go rest acc total vdepth inValue (Just td) used
                                else go rest acc total vdepth inValue (Just td) used
                        else go rest acc total vdepth inValue tdepth used
                else go rest acc total vdepth inValue tdepth used
      | otherwise = go rest acc total vdepth inValue tdepth used
    addTo ln ms acc = if any ((== ln) . fst) acc then [(l, if l == ln then v + ms else v) | (l, v) <- acc] else acc <> [(ln, ms)]
    matchLine tactic isBullet used =
      case [ ln
           | (ln, c, srcBullet) <- items
           , ln `notElem` used
           , (isBullet && srcBullet) || (not isBullet && not (T.null c) && (T.take 25 c `T.isPrefixOf` tactic || T.take 25 tactic `T.isPrefixOf` c)) ] of
        (ln : _) -> Just ln
        [] -> Nothing

isErrorLine :: Text -> Bool
isErrorLine l = any ok (positionsOf "error" l)
  where
    ok i =
      (i == 0 || T.take 2 (T.drop (i - 2) l) == ": " && i >= 2)
        && T.take 1 (T.drop (i + 5) l) `elem` ["(", ":"]
    positionsOf needle hay = [i | i <- [0 .. T.length hay - T.length needle], needle `T.isPrefixOf` T.drop i hay]

profileTheorem :: Session -> FilePath -> Int -> FilePath -> Double -> Int -> IO PyVal
profileTheorem s path line root limit topN = do
  ls <- splitLines <$> readTextUtf8 path
  if line < 1 || line > length ls then toolError ("Line " <> tshow line <> " out of range") else pure ()
  (source, name, srcStart) <- either toolError pure (extractTheoremSource ls line)
  let sourceLines = splitLines source
      offset = line - srcStart
  proofStart <- either toolError pure (findProofStart sourceLines)
  let items = proofItems sourceLines proofStart
  (tmp, h) <- openTempFile root "tmp.lean"
  BS.hPut h (TE.encodeUtf8 source) `finally` hClose h
  resolvedTmp <- resolve tmp
  r <-
    try (leRunProcess (sessionEnv s) root ["lake", "env", "lean", "--profile", "-Dtrace.profiler=true", "-Dtrace.profiler.threshold=0", resolvedTmp] (Just limit))
      `finally` (try @IOException (removeFile tmp) >> pure ())
  out <- case r of
    Left (e :: IOException) -> toolError (ioErrorText "lake" e)
    Right Nothing -> toolError ("Profiling timed out after " <> pyFloatRepr limit <> "s")
    Right (Just (_, bytes)) -> pure (TE.decodeUtf8With TEE.lenientDecode bytes)
  let errs = [T.strip l | l <- splitLines out, isErrorLine l]
  if not (null errs)
    then
      toolError
        ( "Profiled snippet failed to compile (the theorem is extracted standalone with only the file's imports; same-file dependencies are not included): "
            <> T.intercalate " | " (take 5 errs) )
    else pure ()
  let (traces, cum) = parseOutput out
      (times, total) = lineTimes traces name items
      top = take topN (sortOn (Down . snd) [(ln, ms) | (ln, ms) <- times, ms >= total * 0.01])
      cats = [(k, PFloat (pyRound1 v)) | (k, v) <- sortOn (Down . snd) cum, k `notElem` ["import", "initialization", "parsing", "interpretation", "linting"], v >= 1.0]
      lineEntry (ln, ms) =
        PDict
          [ ("line", PInt (fromIntegral (ln + offset)))
          , ("ms", PFloat (pyRound1 ms))
          , ("text", PStr (if ln <= length sourceLines then T.take 60 (T.strip (sourceLines !! (ln - 1))) else "")) ]
  pure (PDict [("ms", PFloat (pyRound1 total)), ("lines", PList (map lineEntry top)), ("categories", PDict cats)])
