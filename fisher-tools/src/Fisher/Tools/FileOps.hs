-- | @tools.file_operations.ShellFileOperations@: read, write, patch and
-- search expressed as shell commands sent to a 'Sandbox', so they work the
-- same on every backend. Each operation sends the commands upstream sends,
-- in the same order, and reads the answers the same way; the results are
-- the dicts upstream's @to_dict()@ returns.
module Fisher.Tools.FileOps
  ( FileOps (..)
  , newFileOps
  , WriteScope (..)
  , homeWriteScope
    -- * Results
  , ReadResult (..)
  , readResultDict
  , emptyRead
  , WriteResult (..)
  , writeResultDict
  , PatchResult (..)
  , patchResultDict
  , emptyPatch
  , SearchMatch (..)
  , SearchResult (..)
  , searchResultDict
  , LintResult (..)
  , lintResultDict
    -- * Operations
  , readFileOp
  , writeFileOp
  , patchReplace
  , patchReplaceExact
  , checkLint
  , searchOp
    -- * Pieces (used by V4A patches and tests)
  , foExec
  , expandPath
  , escapeShellArg
  , addLineNumbers
  , hasCommand
  , unifiedDiffText
  ) where

import Control.Monad (forM)
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Py.Difflib (splitLinesKeepEnds, unifiedDiff)
import Fisher.Py.Re
import Fisher.Py.Value (PyVal (..))
import Fisher.Tools.Fuzzy (exactFindAndReplace, fuzzyFindAndReplace)
import Fisher.Tools.Py
import Fisher.Tools.Sandbox

-- | Where writes may not go: upstream's credential and system deny list,
-- resolved with @realpath@ on this machine.
newtype WriteScope = WriteScope {wsDenied :: Text -> IO Bool}

-- | The deny list built from a home directory, as upstream builds it from
-- @Path.home()@ at import time.
homeWriteScope :: Text -> IO WriteScope
homeWriteScope home = do
  let h = T.unpack home
      exact =
        [ h <> "/.ssh/authorized_keys", h <> "/.ssh/id_rsa", h <> "/.ssh/id_ed25519", h <> "/.ssh/config"
        , h <> "/.gauss/.env", h <> "/.bashrc", h <> "/.zshrc", h <> "/.profile", h <> "/.bash_profile"
        , h <> "/.zprofile", h <> "/.netrc", h <> "/.pgpass", h <> "/.npmrc", h <> "/.pypirc"
        , "/etc/sudoers", "/etc/passwd", "/etc/shadow"
        ]
      prefixes = [h <> "/.ssh", h <> "/.aws", h <> "/.gnupg", h <> "/.kube", "/etc/sudoers.d", "/etc/systemd"]
  exactR <- mapM pyRealpath exact
  prefR <- map (<> "/") <$> mapM pyRealpath prefixes
  pure . WriteScope $ \p -> do
    resolved <- pyRealpath (T.unpack (expandUserHost home p))
    pure (resolved `elem` exactR || any (`isPrefixOfS` resolved) prefR)
  where
    isPrefixOfS a b = take (length a) b == a

-- | @os.path.expanduser@ on the host, for the deny-list check only.
expandUserHost :: Text -> Text -> Text
expandUserHost home p
  | p == "~" = home
  | "~/" `T.isPrefixOf` p = home <> T.drop 1 p
  | otherwise = p

data FileOps = FileOps
  { foSandbox :: Sandbox
  , foCwd :: Text
  , foCmdCache :: IORef (M.Map Text Bool)
  , foScope :: WriteScope
  }

-- | @ShellFileOperations(env)@: the working directory is the sandbox's at
-- construction, or @/@.
newFileOps :: WriteScope -> Sandbox -> IO FileOps
newFileOps scope sb = do
  cwd <- sbCwd sb
  cache <- newIORef M.empty
  pure (FileOps sb (if T.null cwd then "/" else cwd) cache scope)

-- | @_exec@: a timeout is passed only when it is non-zero.
foExec :: FileOps -> Text -> Maybe Int -> Maybe Text -> IO ExecResult
foExec fo cmd t stdin =
  sbExecute (foSandbox fo) (ExecRequest cmd (foCwd fo) (case t of Just n | n /= 0 -> Just n; _ -> Nothing) stdin)

run :: FileOps -> Text -> IO ExecResult
run fo c = foExec fo c Nothing Nothing

hasCommand :: FileOps -> Text -> IO Bool
hasCommand fo cmd = do
  cache <- readIORef (foCmdCache fo)
  case M.lookup cmd cache of
    Just b -> pure b
    Nothing -> do
      r <- run fo ("command -v " <> cmd <> " >/dev/null 2>&1 && echo 'yes'")
      let b = pyStrip (exOutput r) == "yes"
      modifyIORef' (foCmdCache fo) (M.insert cmd b)
      pure b

escapeShellArg :: Text -> Text
escapeShellArg a = "'" <> T.replace "'" "'\"'\"'" a <> "'"

-- | @_expand_path@: @~@ and @~/...@ through the sandbox's @$HOME@, and
-- @~user@ (a validated name) through the sandbox's shell.
expandPath :: FileOps -> Text -> IO Text
expandPath fo path
  | T.null path = pure path
  | "~" `T.isPrefixOf` path = do
      r <- run fo "echo $HOME"
      let home = pyStrip (exOutput r)
      if exCode r == 0 && not (T.null home)
        then
          if path == "~"
            then pure home
            else
              if "~/" `T.isPrefixOf` path
                then pure (home <> T.drop 1 path)
                else do
                  let rest = T.drop 1 path
                      user = fst (T.breakOn "/" rest)
                  if not (T.null user) && isJustMatch (fullMatch userRe user)
                    then do
                      e <- run fo ("echo " <> path)
                      let v = pyStrip (exOutput e)
                      pure (if exCode e == 0 && not (T.null v) then v else path)
                    else pure path
        else pure path
  | otherwise = pure path
  where
    isJustMatch = maybe False (const True)

userRe :: Regex
userRe = compileOrDie [] "[a-zA-Z0-9._-]+"

-- | @_add_line_numbers@.
addLineNumbers :: Text -> Int -> Text
addLineNumbers content start =
  T.intercalate "\n"
    [ T.justifyRight 6 ' ' (T.pack (show i)) <> "|" <> (if T.length l > 2000 then T.take 2000 l <> "... [truncated]" else l)
    | (i, l) <- zip [start ..] (T.splitOn "\n" content)
    ]

unifiedDiffText :: Text -> Text -> Text -> Text
unifiedDiffText old new name = unifiedDiff (splitLinesKeepEnds old) (splitLinesKeepEnds new) ("a/" <> name) ("b/" <> name)

-- ------------------------------------------------------------------ results

data ReadResult = ReadResult
  { rrContent :: Text
  , rrTotalLines :: Integer
  , rrFileSize :: Integer
  , rrTruncated :: Bool
  , rrHint :: Maybe Text
  , rrIsBinary :: Bool
  , rrIsImage :: Bool
  , rrError :: Maybe Text
  , rrSimilar :: [Text]
  }
  deriving (Eq, Show)

emptyRead :: ReadResult
emptyRead = ReadResult "" 0 0 False Nothing False False Nothing []

-- | @ReadResult.to_dict()@: every field that is not None or [] (the
-- base64, mime and dimension fields are only ever None here).
readResultDict :: ReadResult -> [(Text, PyVal)]
readResultDict r =
  [ ("content", PStr (rrContent r))
  , ("total_lines", PInt (rrTotalLines r))
  , ("file_size", PInt (rrFileSize r))
  , ("truncated", PBool (rrTruncated r))
  ]
    <> [("hint", PStr h) | Just h <- [rrHint r]]
    <> [("is_binary", PBool (rrIsBinary r)), ("is_image", PBool (rrIsImage r))]
    <> [("error", PStr e) | Just e <- [rrError r]]
    <> [("similar_files", PList (map PStr (rrSimilar r))) | not (null (rrSimilar r))]

data WriteResult = WriteResult
  { wrBytes :: Integer
  , wrDirsCreated :: Bool
  , wrError :: Maybe Text
  }
  deriving (Eq, Show)

writeResultDict :: WriteResult -> [(Text, PyVal)]
writeResultDict w =
  [("bytes_written", PInt (wrBytes w)), ("dirs_created", PBool (wrDirsCreated w))]
    <> [("error", PStr e) | Just e <- [wrError w]]

data LintResult = LintResult
  { lrSuccess :: Bool
  , lrSkipped :: Bool
  , lrOutput :: Text
  , lrMessage :: Text
  }
  deriving (Eq, Show)

lintResultDict :: LintResult -> PyVal
lintResultDict l
  | lrSkipped l = PDict [("status", PStr "skipped"), ("message", PStr (lrMessage l))]
  | otherwise = PDict [("status", PStr (if lrSuccess l then "ok" else "error")), ("output", PStr (lrOutput l))]

data PatchResult = PatchResult
  { prSuccess :: Bool
  , prDiff :: Text
  , prModified :: [Text]
  , prCreated :: [Text]
  , prDeleted :: [Text]
  , prLint :: Maybe PyVal
  , prError :: Maybe Text
  }
  deriving (Eq, Show)

emptyPatch :: PatchResult
emptyPatch = PatchResult False "" [] [] [] Nothing Nothing

patchErr :: Text -> PatchResult
patchErr e = emptyPatch {prError = Just e}

-- | @PatchResult.to_dict()@: success, then each field that is truthy.
patchResultDict :: PatchResult -> [(Text, PyVal)]
patchResultDict p =
  [("success", PBool (prSuccess p))]
    <> [("diff", PStr (prDiff p)) | not (T.null (prDiff p))]
    <> [("files_modified", strList (prModified p)) | not (null (prModified p))]
    <> [("files_created", strList (prCreated p)) | not (null (prCreated p))]
    <> [("files_deleted", strList (prDeleted p)) | not (null (prDeleted p))]
    <> [("lint", l) | Just l <- [prLint p], truthyDict l]
    <> [("error", PStr e) | Just e <- [prError p], not (T.null e)]
  where
    strList = PList . map PStr
    truthyDict = \case
      PDict [] -> False
      _ -> True

data SearchMatch = SearchMatch {smPath :: Text, smLine :: Integer, smContent :: Text}
  deriving (Eq, Show)

data SearchResult = SearchResult
  { srMatches :: [SearchMatch]
  , srFiles :: [Text]
  , srCounts :: [(Text, Integer)]
  , srTotal :: Integer
  , srTruncated :: Bool
  , srError :: Maybe Text
  }
  deriving (Eq, Show)

emptySearch :: SearchResult
emptySearch = SearchResult [] [] [] 0 False Nothing

searchResultDict :: SearchResult -> [(Text, PyVal)]
searchResultDict s =
  [("total_count", PInt (srTotal s))]
    <> [("matches", PList [PDict [("path", PStr (smPath m)), ("line", PInt (smLine m)), ("content", PStr (smContent m))] | m <- srMatches s]) | not (null (srMatches s))]
    <> [("files", PList (map PStr (srFiles s))) | not (null (srFiles s))]
    <> [("counts", PDict [(k, PInt v) | (k, v) <- srCounts s]) | not (null (srCounts s))]
    <> [("truncated", PBool True) | srTruncated s]
    <> [("error", PStr e) | Just e <- [srError s], not (T.null e)]

-- -------------------------------------------------------------------- read

binaryExtensions, imageExtensions :: [Text]
binaryExtensions =
  [ ".png", ".jpg", ".jpeg", ".gif", ".webp", ".bmp", ".ico", ".tiff", ".tif", ".svg"
  , ".mp3", ".mp4", ".wav", ".avi", ".mov", ".mkv", ".flac", ".ogg", ".webm"
  , ".zip", ".tar", ".gz", ".bz2", ".xz", ".7z", ".rar"
  , ".pdf", ".doc", ".docx", ".xls", ".xlsx", ".ppt", ".pptx"
  , ".exe", ".dll", ".so", ".dylib", ".o", ".a", ".pyc", ".pyo", ".class", ".wasm", ".bin"
  , ".ttf", ".otf", ".woff", ".woff2", ".eot"
  , ".db", ".sqlite", ".sqlite3"
  ]
imageExtensions = [".png", ".jpg", ".jpeg", ".gif", ".webp", ".bmp", ".ico"]

isLikelyBinary :: Text -> Text -> Bool
isLikelyBinary path sample
  | T.toLower (pySplitExt path) `elem` binaryExtensions = True
  | T.null sample = False
  | otherwise =
      let s = T.take 1000 sample
          np = T.length (T.filter (\c -> fromEnum c < 32 && c `notElem` ("\n\r\t" :: String)) s)
       in (fromIntegral np / fromIntegral (min (T.length sample) 1000) :: Double) > 0.30

maxLines :: Integer
maxLines = 2000

-- | @read_file(path, offset, limit)@ on a path already type-checked as a
-- string.
readFileOp :: FileOps -> Text -> Integer -> Integer -> IO ReadResult
readFileOp fo path0 offset limit0 = do
  path <- expandPath fo path0
  let limit = min limit0 maxLines
      q = escapeShellArg path
  st <- run fo ("wc -c < " <> q <> " 2>/dev/null")
  if exCode st /= 0
    then suggestSimilar fo path
    else do
      let size = fromMaybe 0 (pyInt (exOutput st))
      if T.toLower (pySplitExt path) `elem` imageExtensions
        then
          pure
            emptyRead
              { rrIsImage = True
              , rrIsBinary = True
              , rrFileSize = size
              , rrHint = Just "Image file detected. Automatically redirected to vision_analyze tool. Use vision_analyze with this file path to inspect the image contents."
              }
        else do
          sample <- run fo ("head -c 1000 " <> q <> " 2>/dev/null")
          if isLikelyBinary path (exOutput sample)
            then pure emptyRead {rrIsBinary = True, rrFileSize = size, rrError = Just "Binary file - cannot display as text. Use appropriate tools to handle this file type."}
            else do
              let endLine = offset + limit - 1
              r <- run fo ("sed -n '" <> tshow offset <> "," <> tshow endLine <> "p' " <> q)
              if exCode r /= 0
                then pure emptyRead {rrError = Just ("Failed to read file: " <> exOutput r)}
                else do
                  wc <- run fo ("wc -l < " <> q)
                  let total = fromMaybe 0 (pyInt (exOutput wc))
                      truncated = total > endLine
                  pure
                    emptyRead
                      { rrContent = addLineNumbers (exOutput r) (fromIntegral offset)
                      , rrTotalLines = total
                      , rrFileSize = size
                      , rrTruncated = truncated
                      , rrHint =
                          if truncated
                            then Just ("Use offset=" <> tshow (endLine + 1) <> " to continue reading (showing " <> tshow offset <> "-" <> tshow endLine <> " of " <> tshow total <> " lines)")
                            else Nothing
                      }

suggestSimilar :: FileOps -> Text -> IO ReadResult
suggestSimilar fo path = do
  let dir = case pyDirname path of
        "" -> "."
        d -> d
      filename = pyBasename path
  ls <- run fo ("ls -1 " <> escapeShellArg dir <> " 2>/dev/null | head -20")
  let out = pyStrip (exOutput ls)
      files = if exCode ls == 0 && not (T.null out) then T.splitOn "\n" out else []
      target = setOf (T.toLower filename)
      similar =
        [ pyJoin dir f
        | f <- files
        , let common = length (filter (`elem` target) (setOf (T.toLower f)))
        , (fromIntegral common :: Double) >= fromIntegral (T.length filename) * 0.5
        ]
  pure emptyRead {rrError = Just ("File not found: " <> path), rrSimilar = take 5 similar}
  where
    setOf = M.keys . M.fromList . map (\c -> (c, ())) . T.unpack

-- ------------------------------------------------------------------- write

writeFileOp :: FileOps -> Text -> Text -> IO WriteResult
writeFileOp fo path0 content = do
  path <- expandPath fo path0
  denied <- wsDenied (foScope fo) path
  if denied
    then pure (WriteResult 0 False (Just ("Write denied: '" <> path <> "' is a protected system/credential file.")))
    else do
      let parent = pyDirname path
      dirs <-
        if T.null parent
          then pure False
          else (\r -> exCode r == 0) <$> run fo ("mkdir -p " <> escapeShellArg parent)
      w <- foExec fo ("cat > " <> escapeShellArg path) Nothing (Just content)
      if exCode w /= 0
        then pure (WriteResult 0 dirs (Just ("Failed to write file: " <> exOutput w)))
        else do
          st <- run fo ("wc -c < " <> escapeShellArg path <> " 2>/dev/null")
          let n = fromMaybe (utf8Len content) (pyInt (exOutput st))
          pure (WriteResult n dirs Nothing)
  where
    utf8Len = fromIntegral . T.foldl' (\a c -> a + charBytes c) (0 :: Int)
    charBytes c
      | fromEnum c < 0x80 = 1
      | fromEnum c < 0x800 = 2
      | fromEnum c < 0x10000 = 3
      | otherwise = 4

-- ------------------------------------------------------------------- patch

patchReplace :: FileOps -> Text -> Text -> Text -> Bool -> IO PatchResult
patchReplace = patchReplaceWith fuzzyFindAndReplace

-- | Fisher only: a patch that takes exact matches alone.
patchReplaceExact :: FileOps -> Text -> Text -> Text -> Bool -> IO PatchResult
patchReplaceExact = patchReplaceWith exactFindAndReplace

patchReplaceWith :: (Text -> Text -> Text -> Bool -> (Text, Int, Maybe Text)) -> FileOps -> Text -> Text -> Text -> Bool -> IO PatchResult
patchReplaceWith findReplace fo path0 old new replaceAll = do
  path <- expandPath fo path0
  denied <- wsDenied (foScope fo) path
  if denied
    then pure (patchErr ("Write denied: '" <> path <> "' is a protected system/credential file."))
    else do
      r <- run fo ("cat " <> escapeShellArg path <> " 2>/dev/null")
      if exCode r /= 0
        then pure (patchErr ("Failed to read file: " <> path))
        else do
          let content = exOutput r
          case findReplace content old new replaceAll of
            (_, _, Just err) -> pure (patchErr err)
            (_, 0, Nothing) -> pure (patchErr ("Could not find match for old_string in " <> path))
            (newContent, _, Nothing) -> do
              w <- writeFileOp fo path newContent
              case wrError w of
                Just e -> pure (patchErr ("Failed to write changes: " <> e))
                Nothing -> do
                  lint <- checkLint fo path
                  pure
                    emptyPatch
                      { prSuccess = True
                      , prDiff = unifiedDiffText content newContent path
                      , prModified = [path]
                      , prLint = Just (lintResultDict lint)
                      }

linters :: [(Text, Text)]
linters =
  [ (".py", "python -m py_compile {file} 2>&1")
  , (".js", "node --check {file} 2>&1")
  , (".ts", "npx tsc --noEmit {file} 2>&1")
  , (".go", "go vet {file} 2>&1")
  , (".rs", "rustfmt --check {file} 2>&1")
  ]

checkLint :: FileOps -> Text -> IO LintResult
checkLint fo path = do
  let ext = T.toLower (pySplitExt path)
  case lookup ext linters of
    Nothing -> pure (LintResult True True "" ("No linter for " <> ext <> " files"))
    Just tmpl -> do
      let base = head (T.words tmpl)
      ok <- hasCommand fo base
      if not ok
        then pure (LintResult True True "" (base <> " not available"))
        else do
          r <- foExec fo (T.replace "{file}" (escapeShellArg path) tmpl) (Just 30) Nothing
          pure (LintResult (exCode r == 0) False (pyStrip (exOutput r)) "")

-- ------------------------------------------------------------------ search

-- | @search(pattern, path, target, file_glob, limit, offset, output_mode, context)@.
searchOp :: FileOps -> Text -> Text -> Text -> Maybe Text -> Integer -> Integer -> Text -> Integer -> IO SearchResult
searchOp fo pattern path0 target fileGlob limit offset outputMode context = do
  path <- expandPath fo path0
  check <- run fo ("test -e " <> escapeShellArg path <> " && echo exists || echo not_found")
  if "not_found" `T.isInfixOf` exOutput check
    then pure emptySearch {srError = Just ("Path not found: " <> path <> ". Verify the path exists (use 'terminal' to check).")}
    else
      if target == "files"
        then searchFiles fo pattern path limit offset
        else do
          rg <- hasCommand fo "rg"
          if rg
            then searchGrepLike True fo pattern path fileGlob limit offset outputMode context
            else do
              grep <- hasCommand fo "grep"
              if grep
                then searchGrepLike False fo pattern path fileGlob limit offset outputMode context
                else pure emptySearch {srError = Just "Content search requires ripgrep (rg) or grep. Install ripgrep: https://github.com/BurntSushi/ripgrep#installation"}

searchFiles :: FileOps -> Text -> Text -> Integer -> Integer -> IO SearchResult
searchFiles fo pattern path limit offset = do
  hasFind <- hasCommand fo "find"
  if not hasFind
    then pure emptySearch {srError = Just "File search requires 'find' command. On Windows, use Git Bash, WSL, or install Unix tools."}
    else do
      let pat = if not ("**/" `T.isPrefixOf` pattern) && not ("/" `T.isInfixOf` pattern) then pattern else last (T.splitOn "/" pattern)
          base = "find " <> escapeShellArg path <> " -type f -name " <> escapeShellArg pat <> " "
      r0 <- foExec fo (base <> "-printf '%T@ %p\\n' 2>/dev/null | sort -rn | tail -n +" <> tshow (offset + 1) <> " | head -n " <> tshow limit) (Just 60) Nothing
      r <-
        if T.null (pyStrip (exOutput r0))
          then foExec fo (base <> "2>/dev/null | head -n " <> tshow (limit + offset) <> " | tail -n +" <> tshow (offset + 1)) (Just 60) Nothing
          else pure r0
      let files =
            [ case T.breakOn " " line of
                (ts, rest) | not (T.null rest), pyIsDigitStr (T.replace "." "" ts) -> T.drop 1 rest
                _ -> line
            | line <- T.splitOn "\n" (pyStrip (exOutput r))
            , not (T.null line)
            ]
      pure emptySearch {srFiles = files, srTotal = fromIntegral (length files)}

matchRe, ctxRe :: Regex
matchRe = compileOrDie [] "^([A-Za-z]:)?(.*?):(\\d+):(.*)$"
ctxRe = compileOrDie [] "^([A-Za-z]:)?(.*?)-(\\d+)-(.*)$"

searchGrepLike :: Bool -> FileOps -> Text -> Text -> Maybe Text -> Integer -> Integer -> Text -> Integer -> IO SearchResult
searchGrepLike isRg fo pattern path fileGlob limit offset outputMode context = do
  let parts =
        (if isRg then ["rg", "--line-number", "--no-heading", "--with-filename"] else ["grep", "-rnH"])
          <> (if context > 0 then ["-C", tshow context] else [])
          <> maybe [] (\g -> [if isRg then "--glob" else "--include", escapeShellArg g]) (nonEmpty fileGlob)
          <> (case outputMode of "files_only" -> ["-l"]; "count" -> ["-c"]; _ -> [])
          <> [escapeShellArg pattern, escapeShellArg path]
      fetch
        | isRg = if context > 0 then limit + offset + 200 else limit + offset
        | otherwise = limit + offset + (if context > 0 then 200 else 0)
  r <- foExec fo (T.unwords (parts <> ["|", "head", "-n", tshow fetch])) (Just 60) Nothing
  let out = exOutput r
      lns = T.splitOn "\n" (pyStrip out)
  if exCode r == 2 && T.null (pyStrip out)
    then pure emptySearch {srError = Just "Search failed: Search error"}
    else case outputMode of
      "files_only" -> do
        let allFiles = filter (not . T.null) lns
        pure emptySearch {srFiles = pySlice offset limit allFiles, srTotal = fromIntegral (length allFiles)}
      "count" -> do
        let counts = foldl addCount [] lns
        pure emptySearch {srCounts = counts, srTotal = sum (map snd counts)}
      _ -> do
        ms <- fmap concat . forM lns $ \line ->
          if T.null line || line == "--"
            then pure []
            else case matchStart matchRe line of
              Just m -> pure [toMatch m]
              Nothing
                | context > 0, Just m <- matchStart ctxRe line -> pure [toMatch m]
                | otherwise -> pure []
        let total = fromIntegral (length ms)
        pure emptySearch {srMatches = pySlice offset limit ms, srTotal = total, srTruncated = total > offset + limit}
  where
    nonEmpty = \case
      Just g | not (T.null g) -> Just g
      _ -> Nothing
    toMatch m =
      SearchMatch
        (groupOr "" 1 m <> groupOr "" 2 m)
        (fromMaybe 0 (pyInt (groupOr "0" 3 m)))
        (T.take 500 (groupOr "" 4 m))
    addCount acc line
      | ":" `T.isInfixOf` line =
          let (k, v) = T.breakOnEnd ":" line
           in case pyInt v of
                Just n -> upsert (T.dropEnd 1 k) n acc
                Nothing -> acc
      | otherwise = acc
    upsert k v acc = if any ((== k) . fst) acc then [(k', if k' == k then v else v') | (k', v') <- acc] else acc <> [(k, v)]

-- | Python's @xs[offset:offset + limit]@ for non-negative ints, and the
-- negative-index forms the same expression takes for negative ones.
pySlice :: Integer -> Integer -> [a] -> [a]
pySlice offset limit xs =
  let n = fromIntegral (length xs) :: Integer
      norm i = if i < 0 then max 0 (n + i) else min n i
      s = norm offset
      e = norm (offset + limit)
   in if e <= s then [] else take (fromIntegral (e - s)) (drop (fromIntegral s) xs)

tshow :: Show a => a -> Text
tshow = T.pack . show
