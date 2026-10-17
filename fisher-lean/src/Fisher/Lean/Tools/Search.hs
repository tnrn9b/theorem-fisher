{-# LANGUAGE MultiWayIf #-}

-- | lean-lsp-mcp @tools/search.py@ (with @search_utils.py@ and
-- @loogle.py@'s remote path): @lean_local_search@ (ripgrep over the
-- project, its packages and the stdlib, plus the server's symbol index),
-- and the remote services @lean_leansearch@, @lean_loogle@,
-- @lean_leanfinder@ and @lean_hammer_premise@, with their client-side rate
-- limits. Local Loogle (@LEAN_LOOGLE_LOCAL@) is not ported.
module Fisher.Lean.Tools.Search
  ( localSearchTool
  , leanSearchTool
  , loogleTool
  , leanFinderTool
  , hammerPremiseTool
  , declLine
  , resolveNamespaces
  , searchPattern
  , sortKey
  , isCompilerHelper
  , ripgrepMessage
  ) where

import Control.Exception (SomeException, try)
import Data.Char (isAlphaNum, isSpace)
import Data.IORef
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import System.FilePath (isAbsolute, (</>))

import Fisher.Lean.Client
import Fisher.Lean.Http
import Fisher.Lean.Session
import Fisher.Lean.Text (pyReEscape, readTextUtf8, splitLines)
import Fisher.Lean.Tool
import Fisher.Lean.Tools.Common
import Fisher.Lean.Transport (encodeCompact)
import Fisher.Py.Value

-- ---------------------------------------------------------------- rate limits

-- | @rate_limited(category, max, per)@: the check before the tool runs.
rateLimited :: Session -> Text -> Integer -> Integer -> IO ()
rateLimited s cat maxN per = do
  now <- floor <$> leNow (sessionEnv s)
  over <- atomicModifyIORef' (sessionRate s) $ \m ->
    let kept = [t | t <- M.findWithDefault [] cat m, t > now - per]
     in if fromIntegral (length kept) >= maxN
          then (M.insert cat kept m, True)
          else (M.insert cat (kept <> [now]) m, False)
  if over
    then toolError ("Tool limit exceeded: " <> tshow maxN <> " requests per " <> tshow per <> " s. Try again later.")
    else pure ()

-- | @is_custom_backend(env, default)@.
customBackend :: Session -> Text -> Text -> IO Bool
customBackend s var def = do
  v <- maybe "" T.strip <$> leGetEnv (sessionEnv s) var
  pure (not (T.null v) && T.dropWhileEnd (== '/') v /= T.dropWhileEnd (== '/') def)

envOr :: Session -> Text -> Text -> IO Text
envOr s var def = maybe def id <$> leGetEnv (sessionEnv s) var

-- ---------------------------------------------------------------- HTTP

-- | @urlopen_json(req, timeout)@: the parsed body, or the exception's text
-- as a tool error.
urlopenJson :: Session -> HttpRequest -> IO PyVal
urlopenJson s rq = do
  out <- leHttp (sessionEnv s) rq
  case out of
    HttpResponse _ _ body -> either (const (toolError (orjsonError body))) pure (loads body)
    _ -> toolError (maybe "" id (outcomeError out))

jsonPost :: Text -> PyVal -> HttpRequest
jsonPost url payload =
  HttpRequest "POST" url [("User-Agent", "lean-lsp-mcp/0.1"), ("Content-Type", "application/json")] (Just (encodeCompact payload)) 10

-- | A model's str field, or pydantic's error for the value given.
strField :: Text -> Text -> PyVal -> IO Text
strField model name = \case
  PStr t -> pure t
  v ->
    toolError
      ( "1 validation error for " <> model <> "\n" <> name <> "\n  Input should be a valid string [type=string_type, input_value="
          <> pyRepr v <> ", input_type=" <> pyTypeOf v <> "]\n    For further information visit https://errors.pydantic.dev/2.13/v/string_type" )

pyTypeOf :: PyVal -> Text
pyTypeOf = \case
  PStr _ -> "str"
  PList _ -> "list"
  PInt _ -> "int"
  PFloat _ -> "float"
  PBool _ -> "bool"
  PDict _ -> "dict"
  PNull -> "NoneType"

-- | @d.get(k, "")@ on a dict, or Python's AttributeError.
getOr :: PyVal -> Text -> PyVal -> IO PyVal
getOr d k def = case d of
  PDict _ -> pure (maybe def id (get k d))
  v -> pyAttrError v "get"

-- ---------------------------------------------------------------- leansearch

leanSearchTool :: Tool
leanSearchTool =
  Tool "lean_leansearch" "leansearchArguments" [field "query" TStr, ge "1" (opt "num_results" TInt (PInt 5))] run
  where
    run s a = do
      rateLimited s "leansearch" 90 30
      let n = argInt a "num_results"
      -- upstream's URL is fixed; FISHER_LEANSEARCH_URL exists only so the
      -- live check can point it at its stub, as c2.py's redirect does
      base <- envOr s "FISHER_LEANSEARCH_URL" "https://leansearch.net"
      results <- urlopenJson s (jsonPost (base <> "/search") (PDict [("num_results", PStr (tshow n)), ("query", PList [PStr (argText a "query")])]))
      let first = case results of
            PList (x : _) -> x
            _ -> PNull
      if not (truthy results) || not (truthy first)
        then pure (Model (PDict [("items", PList [])]))
        else do
          raw <- case first of
            PList xs -> mapM (\r -> maybe (pyKeyError "result") pure (get "result" r)) (take n xs)
            v -> pyAttrError v "__getitem__"
          items <- mapM item raw
          pure (Model (PDict [("items", PList items)]))
    joined k r = case get k r of
      Just (PList xs) -> T.intercalate "." <$> mapM (strField "LeanSearchResult" k) xs
      Just v -> strField "LeanSearchResult" k v
      Nothing -> pyKeyError k
    item r = do
      name <- joined "name" r
      modName <- joined "module_name" r
      let opt' k = case get k r of Just v@(PStr _) -> v; _ -> PNull
      pure (PDict [("name", PStr name), ("module_name", PStr modName), ("kind", opt' "kind"), ("type", opt' "type")])

-- ---------------------------------------------------------------- loogle

loogleTool :: Tool
loogleTool =
  Tool "lean_loogle" "loogleArguments" [field "query" TStr, ge "1" (opt "num_results" TInt (PInt 8))] run
  where
    run s a = do
      let query = argText a "query"
          n = argInt a "num_results"
      custom <- customBackend s "LOOGLE_URL" "https://loogle.lean-lang.org"
      if custom
        then pure ()
        else do
          now <- floor <$> leNow (sessionEnv s)
          over <- atomicModifyIORef' (sessionRate s) $ \m ->
            let kept = [t | t <- M.findWithDefault [] "loogle" m, now - t < 30]
             in if length kept >= 3 then (M.insert "loogle" kept m, True) else (M.insert "loogle" (kept <> [now]) m, False)
          if over then toolError "Rate limit exceeded: 3 requests per 30s. Use --loogle-local to avoid limits." else pure ()
      r <- loogleRemote s query n
      case r of
        Left msg -> toolError msg
        Right items -> pure (Model (PDict [("items", PList items)]))

-- | @loogle_remote(query, n)@: the hits, or the message it returns.
loogleRemote :: Session -> Text -> Int -> IO (Either Text [PyVal])
loogleRemote s query n = do
  base <- envOr s "LOOGLE_URL" "https://loogle.lean-lang.org"
  extra <- leGetEnv (sessionEnv s) "LOOGLE_HEADERS"
  r <- try @SomeException $ do
    extraHeaders <- case extra of
      Just raw | not (T.null raw) -> case loads (TE.encodeUtf8 raw) of
        Right (PDict kvs) -> pure [(k, pyStrOf v) | (k, v) <- kvs]
        Right _ -> toolError "'list' object has no attribute 'keys'"
        Left _ -> toolError (orjsonError (TE.encodeUtf8 raw))
      _ -> pure []
    out <- leHttp (sessionEnv s) (HttpRequest "GET" (base <> "/json?q=" <> pyQuote query) (ordered extraHeaders) Nothing 10)
    results <- case out of
      HttpResponse _ _ body -> either (const (toolError (orjsonError body))) pure (loads body)
      _ -> toolError (maybe "" id (outcomeError out))
    err <- getOr results "error" PNull
    if truthy err
      then do
        sugg <- getOr results "suggestions" PNull
        let ss = case sugg of PList xs -> map pyStrOf xs; _ -> []
            hint = if null ss then "" else " Suggestions: " <> T.intercalate ", " (take 5 ss)
        pure (Left ("loogle query error: " <> pyStrOf err <> hint))
      else do
        hits <- getOr results "hits" PNull
        let hs = case hits of PList xs -> take n xs; _ -> []
        Right <$> mapM hit hs
  pure $ case r of
    Right v -> v
    Left e -> Left ("loogle error:\n" <> exceptionText e)
  where
    -- urllib keeps the header order the dict was built in
    ordered extra' = [("User-Agent", maybe "lean-lsp-mcp/0.1" id (lookup "User-Agent" extra'))] <> [(k, v) | (k, v) <- extra', k /= "User-Agent"]
    hit r = do
      name <- getOr r "name" (PStr "") >>= strField "LoogleResult" "name"
      ty <- getOr r "type" (PStr "") >>= strField "LoogleResult" "type"
      md <- getOr r "module" (PStr "") >>= strField "LoogleResult" "module"
      pure (PDict [("name", PStr name), ("type", PStr ty), ("module", PStr md)])

-- ---------------------------------------------------------------- leanfinder

leanFinderTool :: Tool
leanFinderTool =
  Tool "lean_leanfinder" "leanfinderArguments"
    [field "query" TStr, ge "1" (opt "num_results" TInt (PInt 5)), opt "version" (TLit ["v4.19.0", "v4.24.0", "v4.28.0"]) (PStr "v4.28.0")]
    run
  where
    run s a = do
      rateLimited s "leanfinder" 10 30
      url <- envOr s "LEAN_FINDER_URL" "https://lean-lsp-proxy.leanfinder.workers.dev"
      dat <- urlopenJson s (jsonPost url (PDict [("inputs", PStr (argText a "query")), ("top_k", PInt (fromIntegral (argInt a "num_results"))), ("version", arg a "version")]))
      case dat of
        PDict _ | Just e <- get "error" dat -> toolError (pyStrOf e)
        _ -> pure ()
      results <- getOr dat "results" (PList [])
      items <- case results of
        PList xs -> mapM item xs
        _ -> pure []
      pure (Model (PDict [("items", PList items)]))
    item r = do
      let f k = getOr r k (PStr "") >>= strField "LeanFinderResult" k
      fn <- f "formal_name"
      inf <- f "informal_name"
      kind <- f "kind"
      ty <- f "type"
      desc <- f "informal_description"
      path <- getOr r "path" (PStr "") >>= \case
        PStr p -> pure (T.replace "/" "." p)
        v -> pyAttrError v "replace"
      pure (PDict [("formal_name", PStr fn), ("informal_name", PStr inf), ("kind", PStr kind), ("type", PStr ty), ("informal_description", PStr desc), ("path", PStr path)])

-- ---------------------------------------------------------------- hammer

hammerPremiseTool :: Tool
hammerPremiseTool =
  Tool "lean_hammer_premise" "hammer_premiseArguments"
    [fileField, ge "1" (field "line" TInt), ge "1" (field "column" TInt), ge "1" (opt "num_results" TInt (PInt 32))]
    run
  where
    run s a = do
      custom <- customBackend s "LEAN_HAMMER_URL" "http://leanpremise.net"
      if custom then pure () else rateLimited s "hammer_premise" 6 30
      rel <- requireClientForFile s (argText a "file_path") Nothing
      c <- getClient s
      openSynced s rel False
      let line = argInt a "line"
          col = argInt a "column"
      g <- goal c rel (line - 1) (col - 1) True
      st <- case grGoals g of
        (first : _) | grStatus g == "goals" -> pure first
        _ -> toolError ("No goals found at line " <> tshow line <> ", column " <> tshow col <> " (status: " <> grStatus g <> "). Try a different position.")
      url <- envOr s "LEAN_HAMMER_URL" "http://leanpremise.net"
      results <- urlopenJson s (jsonPost (url <> "/retrieve") (PDict [("state", PStr st), ("new_premises", PList []), ("k", PInt (fromIntegral (argInt a "num_results")))]))
      items <- case results of
        PList xs -> mapM (\r -> maybe (pyKeyError "name") (fmap (\n -> PDict [("name", PStr n)]) . strField "PremiseResult" "name") (get "name" r)) xs
        v -> pyAttrError v "__iter__"
      pure (Model (PDict [("items", PList items)]))

-- ---------------------------------------------------------------- local search

ripgrepMessage :: String -> Text
ripgrepMessage os =
  T.intercalate "\n" $
    [ "ripgrep (rg) was not found on your PATH. The lean_local_search tool uses ripgrep for fast declaration search.", "", "Installation options:" ]
      <> ["  - " <> i | i <- instructions]
      <> ["More installation options: https://github.com/BurntSushi/ripgrep#installation"]
  where
    instructions = case os of
      "darwin" -> ["brew install ripgrep"]
      "linux" -> ["sudo apt-get install ripgrep", "sudo dnf install ripgrep"]
      "mingw32" -> ["winget install BurntSushi.ripgrep.MSVC", "choco install ripgrep"]
      _ -> ["Check alternative installation methods."]

declKeywords, declModifiers :: [Text]
declKeywords = ["theorem", "lemma", "def", "axiom", "class", "instance", "structure", "inductive", "abbrev", "opaque"]
declModifiers = ["public", "protected", "private", "noncomputable", "partial", "unsafe", "scoped", "local"]

declLead :: Text
declLead = "^\\s*(?:@\\[[^\\]]*\\]\\s*)*(?:(?:" <> T.intercalate "|" declModifiers <> ")\\s+)*"

-- | The pattern ripgrep is given for a query.
searchPattern :: Text -> Text
searchPattern query =
  let term = last (T.splitOn "." query)
   in declLead <> "(?:" <> T.intercalate "|" declKeywords <> ")\\s+" <> "(?:[A-Za-z0-9_'.]+\\.)*" <> pyReEscape term <> "[A-Za-z0-9_'.]*(?:\\s|:|$)"

identCh :: Char -> Bool
identCh ch = (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') || (ch >= '0' && ch <= '9') || ch == '_' || ch == '\''

-- | @_DECL_LINE_RE.match(line)@: the keyword and the (dotted) name.
declLine :: Text -> Maybe (Text, Text)
declLine l0 = do
  let l1 = attrs (T.dropWhile isSpace l0)
      l2 = mods l1
  (kind, rest) <- case [(k, r) | k <- declKeywords, Just r <- [T.stripPrefix k l2], Just (ch, _) <- [T.uncons r], isSpace ch] of
    (x : _) -> Just x
    [] -> Nothing
  let r = T.dropWhile isSpace rest
      name = dotted r
  if T.null name then Nothing else Just (kind, name)
  where
    attrs t = case T.stripPrefix "@[" t of
      Just r | (_, after) <- T.break (== ']') r, not (T.null after) -> attrs (T.dropWhile isSpace (T.drop 1 after))
      _ -> t
    mods t = case [r | m <- declModifiers, Just r <- [T.stripPrefix m t], Just (ch, _) <- [T.uncons r], isSpace ch] of
      (r : _) -> mods (T.dropWhile isSpace r)
      [] -> t
    dotted t =
      let seg = T.takeWhile identCh t
          rest = T.drop (T.length seg) t
       in if T.null seg
            then ""
            else case T.uncons rest of
              Just ('.', r) | Just (ch, _) <- T.uncons r, identCh ch -> seg <> "." <> dotted r
              _ -> seg

isWordCh :: Char -> Bool
isWordCh ch = isAlphaNum ch || ch == '_'

-- | @_resolve_namespaces(path, lines)@: the enclosing namespace of each
-- (1-based) line asked about.
resolveNamespaces :: FilePath -> Set.Set Int -> IO (M.Map Int Text)
resolveNamespaces p wanted
  | Set.null wanted = pure M.empty
  | otherwise = do
      r <- try @SomeException (readTextUtf8 p)
      pure $ case r of
        Left _ -> M.empty
        Right txt -> go (zip [1 ..] (take (Set.findMax wanted) (splitLines txt))) [] M.empty
  where
    go [] _ acc = acc
    go ((i, raw) : rest) stack acc =
      let st = T.strip raw
          stack'
            | Just ns <- namespaceOf st = stack <> [Just ns]
            | kw "section" st || kw "mutual" st = stack <> [Nothing]
            | kw "end" st = if null stack then stack else init stack
            | otherwise = stack
          acc' = if Set.member i wanted then M.insert i (T.intercalate "." [x | Just x <- stack']) acc else acc
       in go rest stack' acc'
    kw k t = case T.stripPrefix k t of
      Just r -> maybe True (not . isWordCh . fst) (T.uncons r)
      Nothing -> False
    namespaceOf t = do
      r <- T.stripPrefix "namespace" t
      let (ws, r') = T.span isSpace r
      if T.null ws then Nothing else Just ()
      let name = T.takeWhile (\ch -> isWordCh ch || ch == '.' || ch == '\'') r'
      if T.null name then Nothing else Just name

-- | @_local_search_sort_key(match, query)@ (query case-folded).
sortKey :: Text -> PyVal -> (Int, Int, Int, Text, Text)
sortKey q m =
  let name = str' "name"
      base = last (T.splitOn "." name)
      nf = T.toCaseFold name
      bf = T.toCaseFold base
      rank
        | "." `T.isInfixOf` q =
            if
              | nf == q -> 0
              | q `T.isPrefixOf` nf -> 1
              | q `T.isInfixOf` nf -> 2
              | bf == q -> 3
              | q `T.isPrefixOf` bf -> 4
              | q `T.isInfixOf` bf -> 5
              | otherwise -> 6
        | otherwise =
            if
              | nf == q || bf == q -> 0
              | q `T.isPrefixOf` bf -> 1
              | q `T.isInfixOf` bf -> 2
              | q `T.isPrefixOf` nf -> 3
              | q `T.isInfixOf` nf -> 4
              | otherwise -> 5
      penalty = if ".lake/packages/" `T.isPrefixOf` str' "file" then 1 else 0
   in (rank, penalty, T.length base, base, name)
  where
    str' k = case get k m of Just (PStr t) -> t; _ -> ""

match3 :: Text -> Text -> Text -> PyVal
match3 name kind file = PDict [("name", PStr name), ("kind", PStr kind), ("file", PStr file)]

-- | @lean_local_search(query, limit, root, policy)@.
ripgrepSearch :: Session -> PathPolicy -> Text -> Int -> IO [PyVal]
ripgrepSearch s pol query limit = do
  let root = ppRoot pol
      stdlib = [r | (r, ".lean-stdlib") <- ppRoots pol]
      cmd =
        [ "rg", "--json", "--no-ignore", "--smart-case", "--hidden", "--color", "never", "--no-messages"
        , "-g", "*.lean", "-g", "!.git/**", "-g", "!.lake/build/**", T.unpack (searchPattern query), root ]
          <> stdlib
      breadth = if "." `T.isInfixOf` query then 32 else 8
      maxCandidates = min (max (limit * breadth) limit) 2048
  (code, out, err) <- leRunRg (sessionEnv s) root cmd
  let events = [e | l <- BC.lines out, not (BS.null l), Right e <- [loads l], get "type" e == Just (PStr "match")]
  found <- fmap concat . mapM (one root) $ events
  let (matches, early) = if length found >= maxCandidates then (take maxCandidates found, True) else (found, False)
  if not early && code `notElem` [0, 1] && null matches
    then
      let errText = TE.decodeUtf8With TEE.lenientDecode err
       in toolError ("Search failed: ripgrep exited with code " <> tshow code <> (if T.null errText then "" else "\n" <> errText))
    else qualifyAndRank matches query limit
  where
    one root e = do
      let text = maybe "" pyStrOf (getPath ["data", "lines", "text"] e)
          path = T.unpack (maybe "" pyStrOf (getPath ["data", "path", "text"] e))
          lineNo = maybe 0 id (intAt ["data", "line_number"] e)
      case declLine text of
        Nothing -> pure []
        Just (kind, name) -> do
          absPath <- if isAbsolute path then pure path else resolve (root </> path)
          md <- displayPath pol absPath
          pure $ case md of
            Just d -> [(match3 name kind d, (absPath, lineNo))]
            Nothing -> []

-- | @_qualify_and_rank_matches(matches, locations, query, limit)@.
qualifyAndRank :: [(PyVal, (FilePath, Int))] -> Text -> Int -> IO [PyVal]
qualifyAndRank found query limit = do
  let byFile = M.fromListWith Set.union [(p, Set.singleton l) | (_, (p, l)) <- found]
  nss <- M.traverseWithKey resolveNamespaces byFile
  let qualified =
        [ case M.lookup l (M.findWithDefault M.empty p nss) of
            Just prefix | not (T.null prefix) -> set "name" (PStr (prefix <> "." <> nameOf m)) m
            _ -> m
        | (m, (p, l)) <- found ]
      q = T.toCaseFold query
      kept = if "." `T.isInfixOf` q then [m | m <- qualified, q `T.isInfixOf` T.toCaseFold (nameOf m)] else qualified
      sorted = sortOn (sortKey q) kept
  pure (dedupe Set.empty sorted)
  where
    nameOf m = case get "name" m of Just (PStr t) -> t; _ -> ""
    key m = (nameOf m, maybe "" pyStrOf (get "kind" m), maybe "" pyStrOf (get "file" m))
    dedupe _ [] = []
    dedupe seen (m : rest)
      | Set.member (key m) seen = dedupe seen rest
      | otherwise =
          let seen' = Set.insert (key m) seen
           in if Set.size seen' >= limit then [m] else m : dedupe seen' rest

-- | @_is_compiler_helper(name)@.
isCompilerHelper :: Text -> Bool
isCompilerHelper name =
  let bare = maybe name id (T.stripPrefix "_root_." name)
      leaf0 = last (T.splitOn "." bare)
      leaf = if "«" `T.isPrefixOf` leaf0 && "»" `T.isSuffixOf` leaf0 && T.length leaf0 >= 2 then T.drop 1 (T.dropEnd 1 leaf0) else leaf0
      macro = "_private." `T.isPrefixOf` bare && "_aux_" `T.isPrefixOf` leaf && "_macroRules_" `T.isInfixOf` leaf
      hyg = "._@." `T.isInfixOf` bare && "._hygCtx._hyg." `T.isInfixOf` bare
   in macro || hyg

-- | @workspace_symbol_matches(symbols, policy, query)@.
symbolMatches :: PathPolicy -> Text -> [PyVal] -> IO [PyVal]
symbolMatches pol query syms = fmap concat . mapM one $ syms
  where
    wanted = let q = T.strip query in maybe q id (T.stripPrefix "_root_." q)
    one sym = case get "name" sym of
      Just (PStr name)
        | not (T.null name)
        , not (isCompilerHelper name && maybe name id (T.stripPrefix "_root_." name) /= wanted) ->
            case get "location" sym of
              Just loc@(PDict _) -> case get "path" loc of
                Just (PStr p) | not (T.null p) -> do
                  let local = if isAbsolute (T.unpack p) then T.unpack p else ppRoot pol </> T.unpack p
                  md <- displayPath pol local
                  pure [match3 name "declaration" d | Just d <- [md]]
                _ -> pure []
              _ -> pure []
      _ -> pure []

-- | @merge_local_search_matches(source, index, query, limit)@.
mergeMatches :: [PyVal] -> [PyVal] -> Text -> Int -> [PyVal]
mergeMatches source index query limit =
  let nameOf m = case get "name" m of Just (PStr t) -> t; _ -> ""
      step (acc, seen) m = if Set.member (nameOf m) seen then (acc, seen) else (acc <> [m], Set.insert (nameOf m) seen)
      (merged, _) = foldl step (source, Set.fromList (map nameOf source)) index
   in take limit (sortOn (sortKey (T.toCaseFold query)) merged)

localSearchTool :: Tool
localSearchTool =
  Tool "lean_local_search" "local_searchArguments"
    [field "query" TStr, ge "1" (opt "limit" TInt (PInt 10)), nullable "project_root" TStr PNull]
    run
  where
    run s a = do
      rg <- leRgAvailable (sessionEnv s)
      if rg then pure () else toolError (ripgrepMessage (leOs (sessionEnv s)))
      stored <- boundProject s
      root <- case arg a "project_root" of
        PStr pr | not (T.null pr) -> do
          let p = T.unpack pr
          r <- try @ToolError $ case stored of
            Just st | not (isAbsolute p), take 1 p /= "~" -> bindProject s (st </> p)
            _ -> bindProject s p
          case r of
            Right x -> pure (Just x)
            Left (ToolError m) -> toolError ("Invalid project root '" <> pr <> "': " <> m)
        _ -> pure stored
      rootPath <- maybe (toolError "Lean project path not set. Call a file-based tool first.") pure root
      pol <- pathPolicy s rootPath
      let query = T.strip (argText a "query")
          limit = argInt a "limit"
      source <- ripgrepSearch s pol query limit
      mc <- runningClient s (ppRoot pol)
      (items, status) <- case mc of
        Nothing -> pure (source, "unavailable")
        Just c -> do
          r <- try @SomeException (workspaceSymbol c query (Just limit) 2)
          case r of
            Left _ -> pure (source, "error")
            Right (syms, ready) -> do
              idx <- symbolMatches pol query syms
              pure (mergeMatches source idx query limit, if ready then "consulted" else "warming")
      pure (Model (PDict [("items", PList [PDict [(k, maybe (PStr "") id (get k m)) | k <- ["name", "kind", "file"]] | m <- items]), ("index", PStr status)]))
