-- | @lean_file_outline@ (lean-lsp-mcp @outline_utils.py@): imports, the
-- server's document symbols plus @theorem@/@lemma@/@def@ lines read from
-- top-level namespace bodies, and type signatures from an @#info_trees@
-- copy of the file run in the serial scratch slot. Python's regexes are
-- ported by hand, one function each.
module Fisher.Lean.Tools.Outline
  ( outlineTool
  , stripLineComment
  , scanAssignments
  , signatureText
  , extractDeclarations
  , extractType
  , extractFields
  ) where

import Control.Exception (SomeException, try)
import Data.Char (isAlphaNum, isSpace)
import Data.IORef
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (getFileSize, getModificationTime)
import System.IO.Unsafe (unsafePerformIO)

import Fisher.Lean.Client
import Fisher.Lean.Scratch
import Fisher.Lean.Session
import Fisher.Lean.Text (splitLines)
import Fisher.Lean.Tool
import Fisher.Lean.Tools.Common
import Fisher.Py.Value

outlineTool :: Tool
outlineTool =
  Tool "lean_file_outline" "file_outlineArguments" [fileField, ge "1" (nullable "max_declarations" TInt PNull)] run
  where
    run s a = do
      rel <- requireClientForFile s (argText a "file_path") Nothing
      c <- getClient s
      pool <- scratchPool s True
      Model <$> generateOutline c pool rel (argMaybeInt a "max_declarations")

-- ---------------------------------------------------------------- cache

type CacheKey = (FilePath, Text, Text, Text, Integer)

-- | The process-wide outline cache (32 entries, least recently used out).
outlineCache :: IORef [(CacheKey, PyVal)]
outlineCache = unsafePerformIO (newIORef [])
{-# NOINLINE outlineCache #-}

cacheKey :: Client -> Text -> Text -> IO CacheKey
cacheKey c rel txt = do
  let p = joinPath' (clientProject c) (T.unpack rel)
  r <- try @SomeException ((,) <$> getModificationTime p <*> getFileSize p)
  pure $ case r of
    Right (m, sz) -> (clientProject c, rel, txt, T.pack (show m), sz)
    Left _ -> (clientProject c, rel, txt, "-1", -1)

cacheGet :: CacheKey -> IO (Maybe PyVal)
cacheGet k = atomicModifyIORef' outlineCache $ \es -> case lookup k es of
  Just v -> (filter ((/= k) . fst) es <> [(k, v)], Just v)
  Nothing -> (es, Nothing)

cachePut :: CacheKey -> PyVal -> IO ()
cachePut k v = atomicModifyIORef' outlineCache $ \es ->
  let es' = filter ((/= k) . fst) es <> [(k, v)]
   in (drop (length es' - 32) es', ())

withMax :: Maybe Int -> PyVal -> PyVal
withMax mmax o = case (mmax, get "declarations" o) of
  (Just n, Just (PList ds)) | length ds > n -> set "declarations" (PList (take n ds)) (set "total_declarations" (PInt (fromIntegral (length ds))) o)
  _ -> o

-- ---------------------------------------------------------------- outline

symbolKind :: Integer -> Text
symbolKind k =
  maybe "unknown" id . lookup k $
    zip [1 ..] ["file", "module", "namespace", "package", "class", "method", "property", "field", "constructor", "enum", "interface", "function", "variable", "constant", "string", "number", "boolean", "array", "object", "key", "null", "enumMember", "struct", "event", "operator", "typeParameter"]

isMethod :: PyVal -> Bool
isMethod sym = get "kind" sym `elem` [Just (PInt 6), Just (PStr "method")]

outlineOf :: [PyVal] -> [PyVal] -> Maybe PyVal -> PyVal
outlineOf imports decls total = PDict [("imports", PList imports), ("declarations", PList decls), ("total_declarations", maybe PNull id total)]

generateOutline :: Client -> ScratchPool -> Text -> Maybe Int -> IO PyVal
generateOutline c pool rel mmax = do
  reloadFromDisk c rel False
  txt <- content c rel
  key <- cacheKey c rel txt
  cached <- cacheGet key
  case cached of
    Just o -> pure (withMax mmax o)
    Nothing -> do
      let imports =
            [ PStr imp
            | l <- splitLines txt
            , let st = T.strip l
            , Just imp <- [if "public import " `T.isPrefixOf` st then Just (T.drop 14 st) else if "import " `T.isPrefixOf` st then Just (T.drop 7 st) else Nothing] ]
      syms0 <- documentSymbols c rel True
      let syms = [case get "kind" sym of Just (PInt k) -> set "kind" (PStr (symbolKind k)) sym; _ -> sym | sym <- syms0]
      outline <-
        if null syms && null imports
          then pure (outlineOf [] [] Nothing)
          else do
            let allSyms = flatten txt syms
                lspMethods = [x | x <- allSyms, isMethod x, not (has "_keyword" x)]
            infos <- infoTrees pool txt lspMethods
            let typeSigs = M.fromList [(n, sig) | (n, info) <- M.toList infos, Just sig <- [extractType info n], not (T.null sig)]
                fieldsMap = M.fromList [(n, fs) | (n, info) <- M.toList infos, let fs = extractFields info n, not (null fs)]
                decls = [entry typeSigs fieldsMap x | x <- allSyms, isMethod x || maybe False truthy (get "_keyword" x) || get "kind" x == Just (PStr "namespace")]
            pure (outlineOf imports decls Nothing)
      cachePut key outline
      pure (withMax mmax outline)
  where
    entry typeSigs fieldsMap sym =
      let name = textOf (get "name" sym)
          typeSig = case get "_type" sym of
            Just (PStr t) | not (T.null t) -> t
            _ -> M.findWithDefault "" name typeSigs
          fields = M.findWithDefault [] name fieldsMap
          kw = case get "_keyword" sym of Just (PStr k) -> Just k; _ -> Nothing
          tag = detectTag name (maybe (PStr "") id (get "kind" sym)) typeSig (not (null fields)) kw
          start = maybe 0 id (intAt ["range", "start", "line"] sym) + 1
          end = maybe 0 id (intAt ["range", "end", "line"] sym) + 1
          children =
            [ PDict [("name", PStr f), ("kind", PStr "field"), ("start_line", PInt (fromIntegral start)), ("end_line", PInt (fromIntegral start)), ("type_signature", PStr ft), ("children", PList [])]
            | (f, ft) <- fields ]
       in PDict
            [ ("name", PStr name), ("kind", PStr tag), ("start_line", PInt (fromIntegral start)), ("end_line", PInt (fromIntegral end))
            , ("type_signature", if T.null typeSig then PNull else PStr typeSig), ("children", PList children) ]

textOf :: Maybe PyVal -> Text
textOf = \case
  Just (PStr t) -> t
  Just v -> pyStrOf v
  Nothing -> ""

-- | @_flatten_symbols@: pre-order, with source declarations appended to the
-- children of a (top-level, string-kinded) namespace.
flatten :: Text -> [PyVal] -> [PyVal]
flatten txt = concatMap one
  where
    one sym =
      let children0 = case get "children" sym of Just (PList xs) -> xs; _ -> []
          children
            | not (T.null txt) && get "kind" sym == Just (PStr "namespace") =
                children0 <> extractDeclarations txt (maybe 0 id (intAt ["range", "start", "line"] sym)) (maybe 0 id (intAt ["range", "end", "line"] sym))
            | otherwise = children0
       in sym : flatten txt children

-- | @_detect_tag@.
detectTag :: Text -> PyVal -> Text -> Bool -> Maybe Text -> Text
detectTag name kind typeSig hasFields kw
  | hasFields = if "→" `T.isInfixOf` typeSig then "Class" else "Struct"
  | name == "example" = "Ex"
  | kw `elem` [Just "theorem", Just "lemma"] = "Thm"
  | not (T.null typeSig) && ("∀" `T.isInfixOf` typeSig || "=" `T.isInfixOf` typeSig) = "Thm"
  | not (T.null typeSig) && "→" `T.isInfixOf` replaceOnce " → " "" typeSig = "Thm"
  | kind == PStr "namespace" = "Ns"
  | otherwise = "Def"
  where
    replaceOnce needle rep t = case T.breakOn needle t of
      (a, b) | not (T.null b) -> a <> rep <> T.drop (T.length needle) b
      _ -> t

-- | @_get_info_trees@: each LSP method's name and the info-tree message
-- Lean prints for the @#info_trees in@ line put before it.
infoTrees :: ScratchPool -> Text -> [PyVal] -> IO (M.Map Text Text)
infoTrees _ _ [] = pure M.empty
infoTrees pool txt syms = do
  let sorted = sortOn (\x -> maybe 0 id (intAt ["range", "start", "line"] x)) syms
      inserts = [(maybe 0 id (intAt ["range", "start", "line"] x) + i, textOf (get "name" x)) | (i, x) <- zip [0 ..] sorted]
      ls = foldl (\acc (at, _) -> let (a, b) = splitAt at acc in a <> ["#info_trees in"] <> b) (splitLines txt) inserts
      byLine = M.fromList inserts
  trial <- runText pool (T.intercalate "\n" ls <> "\n") Nothing Nothing
  pure $
    foldl
      (\m d ->
         let r = maybe (PDict []) id (rangeOf "fullRange" "range" d)
          in case intAt ["start", "line"] r of
               Just l | get "severity" d == Just (PInt 3), Just n <- M.lookup l byLine -> M.insert n (textOf (Just (maybe (PStr "") id (get "message" d)))) m
               _ -> m)
      M.empty
      (drItems (trDiagnostics trial))

-- | Every index where @needle@ occurs in @hay@, in order.
occurrences :: Text -> Text -> [Int]
occurrences needle hay = go 0 hay
  where
    go off t = case T.breakOn needle t of
      (a, b) | not (T.null b) -> let i = off + T.length a in i : go (i + 1) (T.drop (T.length a + 1) t)
      _ -> []

-- | @_extract_type@: @  • \\[Term\\] NAME \\(isBinder := true\\) : ([^\@]+) \@@.
extractType :: Text -> Text -> Maybe Text
extractType info name =
  let pre = "  • [Term] " <> name <> " (isBinder := true) : "
      try1 i =
        let rest = T.drop (i + T.length pre) info
            (cap, after) = T.breakOn "@" rest
         in if not (T.null after) && T.length cap >= 2 && T.last cap == ' ' then Just (T.strip (T.init cap)) else Nothing
   in case [x | i <- occurrences pre info, Just x <- [try1 i]] of
        (x : _) -> Just x
        [] -> Nothing

isWord :: Char -> Bool
isWord ch = isAlphaNum ch || ch == '_'

-- | @_extract_fields@: for @NAME.(\\w+)@ then @\@NAME.(\\w+)@, every match of
-- @  • \\[Term\\] PATTERN \\(isBinder := true\\) : (.+?) \@@, cleaned.
extractFields :: Text -> Text -> [(Text, Text)]
extractFields info name = concatMap scan ["  • [Term] " <> name <> ".", "  • [Term] @" <> name <> "."]
  where
    mid = " (isBinder := true) : "
    scan pre = go 0
      where
        go from =
          case [m | i <- occurrences pre info, i >= from, Just m <- [matchAt i]] of
            ((end, f, ty) : _) -> (f, clean ty) : go end
            [] -> []
        matchAt i =
          let afterPre = T.drop (i + T.length pre) info
              (w, r1) = T.span isWord afterPre
           in if T.null w || not (mid `T.isPrefixOf` r1)
                then Nothing
                else
                  let r2 = T.drop (T.length mid) r1
                      line = T.takeWhile (/= '\n') r2
                   in case [k | k <- [1 .. T.length line - 2], " @" `T.isPrefixOf` T.drop k line] of
                        (k : _) -> Just (i + T.length pre + T.length w + T.length mid + k + 2, w, T.take k line)
                        [] -> Nothing
    clean full
      | "]" `T.isInfixOf` full = T.strip (T.dropWhile (`elem` ("→ " :: String)) (snd (T.breakOnEnd "]" full)))
      | " → " `T.isInfixOf` full = T.strip (last (T.splitOn " → " full))
      | otherwise = T.strip full

-- ---------------------------------------------------------------- source declarations

-- | @_strip_line_comment@.
stripLineComment :: Text -> Text
stripLineComment t
  | not ("--" `T.isInfixOf` t) = t
  | otherwise = go False 0
  where
    n = T.length t
    at i = T.index t i
    go inStr i
      | i >= n = t
      | at i == '\\' && inStr = go inStr (i + 2)
      | at i == '"' = go (not inStr) (i + 1)
      | not inStr && at i == '-' && "--" `T.isPrefixOf` T.drop i t && (i == 0 || isSpace (at (i - 1))) = T.stripEnd (T.take i t)
      | otherwise = go inStr (i + 1)

asciiIdent :: Char -> Bool
asciiIdent ch = (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') || (ch >= '0' && ch <= '9') || ch == '_' || ch == '\''

-- | The matches of @_ASSIGNMENT_OR_BINDER_RE@ in order: @(start, isBinder)@.
assignmentMatches :: Text -> [(Int, Bool)]
assignmentMatches t = go 0
  where
    n = T.length t
    go i
      | i >= n = []
      | Just end <- binderAt i = (i, True) : go end
      | ":=" `T.isPrefixOf` T.drop i t = (i, False) : go (i + 2)
      | otherwise = go (i + 1)
    binderAt i
      | i > 0 && asciiIdent (T.index t (i - 1)) = Nothing
      | otherwise =
          let rest = T.drop i t
              kwEnd = [T.length kw | kw <- ["let", "letI", "haveI"], kw `T.isPrefixOf` rest, Just (ch, _) <- [T.uncons (T.drop (T.length kw) rest)], isSpace ch]
           in case kwEnd of
                (k : _) ->
                  let after = T.drop (k + 1) rest
                   in if arrowAhead after then Nothing else Just (i + k + 1)
                [] -> Nothing
    -- (?:(?!:=|;).)*← on one line
    arrowAhead s = case T.uncons s of
      Nothing -> False
      Just (ch, r)
        | ":=" `T.isPrefixOf` s || ch == ';' -> False
        | ch == '←' -> True
        | ch == '\n' -> False
        | otherwise -> arrowAhead r

-- | @_scan_assignments(text, pending)@.
scanAssignments :: Text -> Int -> (Maybe Int, Int)
scanAssignments t = go (assignmentMatches t)
  where
    go [] p = (Nothing, p)
    go ((i, binder) : ms) p
      | binder = go ms (p + 1)
      | p > 0 = go ms (p - 1)
      | otherwise = (Just i, p)

whereAt :: Text -> Maybe Int
whereAt t = case [i | i <- occurrences "where" t, i == 0 || not (asciiIdent (T.index t (i - 1)) || T.index t (i - 1) == '.'), maybe True (not . asciiIdent . fst) (T.uncons (T.drop (i + 5) t))] of
  (i : _) -> Just i
  [] -> Nothing

matchArm :: Text -> Bool
matchArm l = "|" `T.isPrefixOf` l && "=>" `T.isInfixOf` T.drop 1 (T.takeWhile (/= '\n') l)

hashCommand :: Text -> Bool
hashCommand l = case T.unpack (T.take 2 l) of
  ['#', ch] -> (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') || ch == '_'
  _ -> False

nextCommand :: Text -> Bool
nextCommand l
  | "@[" `T.isPrefixOf` l || "/--" `T.isPrefixOf` l = True
  | otherwise = kw (dropMods l)
  where
    mods = ["private", "protected", "noncomputable", "partial", "unsafe", "nonrec"]
    dropMods s = case [r | m <- mods, Just r <- [T.stripPrefix m s], Just (ch, _) <- [T.uncons r], isSpace ch] of
      (r : _) -> dropMods (T.dropWhile isSpace r)
      [] -> s
    kws = ["theorem", "lemma", "def", "abbrev", "instance", "example", "structure", "class", "inductive", "axiom", "opaque", "namespace", "section", "end", "open", "variable", "universe", "attribute", "set_option", "mutual", "macro", "syntax", "notation", "elab"]
    kw s = or [maybe True (not . isWord . fst) (T.uncons r) | k <- kws, Just r <- [T.stripPrefix k s]]

-- | @_signature_text(lines, start, end)@.
signatureText :: [Text] -> Int -> Int -> Maybe Text
signatureText ls start end = go start [] 0
  where
    go index parts pending
      | index >= end = Nothing
      | otherwise =
          let raw = ls !! index
              line0 = stripLineComment (T.strip raw)
              skip = index > start && (T.null line0 || "--" `T.isPrefixOf` line0 || "/-" `T.isPrefixOf` line0)
           in if skip
                then go (index + 1) parts pending
                else
                  if index > start && matchArm line0
                    then Just (T.intercalate " " parts)
                    else
                      if index > start && (nextCommand line0 || hashCommand raw)
                        then Nothing
                        else
                          let wh = whereAt line0
                              line = maybe line0 (`T.take` line0) wh
                              (bodyAt, pending') = scanAssignments line pending
                           in case bodyAt of
                                Just b -> Just (T.intercalate " " (parts <> [T.take b line]))
                                Nothing ->
                                  if isJust wh
                                    then Just (T.intercalate " " (parts <> [line]))
                                    else go (index + 1) (parts <> [line]) pending'

-- | @_extract_declarations(content, start, end)@.
extractDeclarations :: Text -> Int -> Int -> [PyVal]
extractDeclarations txt start end = concatMap one [start .. stop - 1]
  where
    ls = splitLines txt
    stop = min end (length ls)
    one i =
      let line = T.strip (ls !! i)
       in case [k | k <- ["theorem", "lemma", "def"], (k <> " ") `T.isPrefixOf` line] of
            (k : _) -> case T.words (T.strip (T.drop (T.length k) line)) of
              (name : _) | not ("_" `T.isPrefixOf` name) ->
                let typeSig = do
                      header <- signatureText ls i stop
                      let sigPart = T.strip (T.drop (T.length k) (T.strip header))
                      if name `T.isPrefixOf` sigPart then Just (T.strip (T.drop (T.length name) sigPart)) else Nothing
                 in [ PDict
                        [ ("name", PStr name), ("kind", PStr "method")
                        , ("range", PDict [("start", PDict [("line", PInt (fromIntegral i)), ("character", PInt 0)]), ("end", PDict [("line", PInt (fromIntegral i)), ("character", PInt (fromIntegral (T.length (ls !! i))))])])
                        , ("_keyword", PStr k), ("_type", maybe PNull PStr typeSig) ] ]
              _ -> []
            [] -> []
