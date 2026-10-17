{-# LANGUAGE MultiWayIf #-}

-- | The project index: every declaration of a Lake package's own @.lean@
-- files, with its kind, statement, file, line, module and the project
-- names its statement mentions, plus each file's imports.
--
-- The root given to 'buildIndex' is the Lake package root (the directory
-- holding @lakefile.lean@ or @lakefile.toml@; 'lakePackageRoot' finds it).
-- Files are taken from the @srcDir@ the lakefile names (@srcDir = "..."@ in
-- TOML, @srcDir := "..."@ in Lean), else the root, and module names are
-- relative to that directory. Dot directories and dot files (@.lake@,
-- @.git@), @lake-packages@, symlinked directories and lakefiles at any
-- depth are never indexed.
--
-- The scan is textual, like "Fisher.Search.Lean": comments and string
-- literals are blanked out first, then namespaces, sections, @mutual@ and
-- @open@ are tracked line by line (commands in column 0), and a line whose
-- first word (after attributes and modifiers) is one of the indexed
-- keywords opens a declaration. A file that cannot be read as UTF-8 or
-- leaves a block comment or a string literal open is skipped with a reason
-- and the rest of the project is still indexed.
--
-- Each file's scan is cached in a JSON file keyed by the file's path
-- (relative to the project root) with its mtime and size, so a rebuild
-- reads only the files that changed. Name resolution ('eUses') runs after
-- the whole index is assembled, from the cached scans.
module Fisher.Search.Index
  ( Kind (..)
  , kindText
  , Entry (..)
  , Index (..)
  , statementCap
  , buildIndex
  , buildIndexWith
  , importClosure
  , moduleOf
  , lakePackageRoot
  , entryKey
  , privateKey
  , scanText
  , scanTextWith
  ) where

import Control.Exception (IOException, onException, try)
import Control.Monad (forM, when)
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.Containers.ListUtils (nubOrd)
import Data.List (isPrefixOf, sort)
import Data.Maybe (listToMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.Directory
import System.FilePath
import System.IO (hClose, openBinaryTempFile)

data Kind = KTheorem | KLemma | KDef | KAbbrev | KStructure | KInductive | KClass | KInstance | KAxiom | KOpaque
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The keyword that opens a declaration of this kind.
kindText :: Kind -> Text
kindText = \case
  KTheorem -> "theorem"
  KLemma -> "lemma"
  KDef -> "def"
  KAbbrev -> "abbrev"
  KStructure -> "structure"
  KInductive -> "inductive"
  KClass -> "class"
  KInstance -> "instance"
  KAxiom -> "axiom"
  KOpaque -> "opaque"

data Entry = Entry
  { eName :: Text
  -- ^ fully qualified (namespaces, @_root_.@ honoured); private names unmangled
  , eKind :: Kind
  , eStatement :: Text
  -- ^ from the keyword through @:=@ or @where@ (without a following @by@);
  -- for a pattern-matching definition, an inductive with @|@ lines, an
  -- axiom or an opaque, through the line before the body. At most
  -- 'statementCap' characters.
  , eFile :: FilePath
  -- ^ relative to the package root (so it includes the srcDir)
  , eLine :: Int
  -- ^ 1-based line of the keyword (Lean.hs's 0-based @dStart@ plus one)
  , eModule :: Text
  -- ^ the file's path relative to the srcDir, dotted
  , eUses :: [Text]
  -- ^ project names the statement mentions, in order of first mention:
  -- an identifier resolved to an exact index name as written, under an
  -- enclosing namespace, or under an @open@ in effect; a dotted
  -- identifier counts by its longest prefix that resolves. Never the
  -- declaration itself. Each is an 'ixByName' key ('entryKey'): a private
  -- declaration is only visible from its own file and is listed by its
  -- 'privateKey'.
  , ePrivate :: Bool
  -- ^ declared @private@: callers must not offer it outside its own file
  }
  deriving (Eq, Show)

data Index = Index
  { ixEntries :: [Entry]
  -- ^ by file path, then line
  , ixImports :: Map Text [Text]
  -- ^ every indexed project module to its imports as written (Mathlib and
  -- other packages included)
  , ixByName :: Map Text Int
  -- ^ 'entryKey' to its position in 'ixEntries' (the first, if a key
  -- repeats), so two private declarations of one name in different files
  -- do not collide
  }
  deriving (Eq, Show)

statementCap :: Int
statementCap = 1500

-- | The key of an entry in 'ixByName': its name, or for a private
-- declaration Lean's own private name, @_private.<Module>.0.<name>@.
entryKey :: Entry -> Text
entryKey e = if ePrivate e then privateKey (eModule e) (eName e) else eName e

privateKey :: Text -> Text -> Text
privateKey m nm = "_private." <> m <> ".0." <> nm

-- | The nearest directory at or above the path (a file or a directory)
-- that holds a @lakefile.lean@ or @lakefile.toml@.
lakePackageRoot :: FilePath -> IO (Maybe FilePath)
lakePackageRoot p0 = do
  p <- normalise <$> makeAbsolute p0
  isFile <- doesFileExist p
  up (dropTrailingPathSeparator (if isFile then takeDirectory p else p))
  where
    up d = do
      here <- or <$> mapM (doesFileExist . (d </>)) lakefiles
      let parent = takeDirectory d
      if
        | here -> pure (Just d)
        | parent == d -> pure Nothing
        | otherwise -> up parent

lakefiles :: [FilePath]
lakefiles = ["lakefile.lean", "lakefile.toml"]

-- | The source directory the package's lakefile names, relative to the
-- root, or @""@ when it names none.
lakeSrcDir :: FilePath -> IO FilePath
lakeSrcDir root = do
  srcs <- mapM fromFile lakefiles
  pure (maybe "" (normalise . T.unpack) (listToMaybe (concat srcs)))
  where
    fromFile f = do
      r <- try (BS.readFile (root </> f)) :: IO (Either IOException BS.ByteString)
      pure $ case r of
        Left _ -> []
        Right bs -> [v | l <- T.lines (decodeUtf8With lenientDecode bs), Just v <- [srcDirOn l]]
    srcDirOn l = do
      rest <- T.stripPrefix "srcDir" (T.stripStart l)
      rest' <- T.stripPrefix "=" (T.stripStart rest) `orElse` T.stripPrefix ":=" (T.stripStart rest)
      inner <- T.stripPrefix "\"" (T.stripStart rest')
      let v = T.takeWhile (/= '"') inner
      if T.null v || v == "." then Nothing else Just v
    orElse a b = maybe b Just a

-- | Index the project at the root, using and refreshing the cache file.
-- Returns the index and the skipped files (relative paths) with reasons.
buildIndex :: FilePath -> FilePath -> IO (Index, [(FilePath, Text)])
buildIndex = buildIndexWith BS.readFile

-- | 'buildIndex' with the reader for @.lean@ files given (it receives the
-- absolute path).
buildIndexWith :: (FilePath -> IO BS.ByteString) -> FilePath -> FilePath -> IO (Index, [(FilePath, Text)])
buildIndexWith reader root0 cachePath = do
  root <- makeAbsolute root0
  src <- lakeSrcDir root
  files <- leanFiles root src
  old <- readCache cachePath
  results <- forM files $ \rel -> do
    let path = root </> rel
    st <- try ((,) <$> getModificationTime path <*> getFileSize path) :: IO (Either IOException (UTCTime, Integer))
    case st of
      Left e -> pure (rel, Nothing, Failed ("could not stat: " <> T.pack (show e)))
      Right (t, size) -> do
        let mt = floor (utcTimeToPOSIXSeconds t * 1000000000) :: Integer
        case M.lookup rel old of
          Just ce | ceMtime ce == mt, ceSize ce == size -> pure (rel, Just ce, ceResult ce)
          _ -> do
            r <- try (reader path) :: IO (Either IOException BS.ByteString)
            case r of
              Left e -> pure (rel, Nothing, Failed ("could not read: " <> T.pack (show e)))
              Right bs -> let res = parseFile bs in pure (rel, Just (CacheEntry mt size res), res)
  let new = M.fromList [(rel, ce) | (rel, Just ce, _) <- results]
  when (new /= old) $ writeCache cachePath new
  let ix = assemble (root </> src) root [(rel, res) | (rel, _, res) <- results]
  pure (ix, [(rel, why) | (rel, _, Failed why) <- results])

-- | The project modules a module can see: itself and everything it imports
-- transitively within the project. Empty for a module not in the index.
importClosure :: Index -> Text -> Set Text
importClosure ix m
  | M.notMember m imps = Set.empty
  | otherwise = go (Set.singleton m) [m]
  where
    imps = ixImports ix
    go seen [] = seen
    go seen (x : xs) =
      let next = [y | y <- M.findWithDefault [] x imps, M.member y imps, Set.notMember y seen]
       in go (foldr Set.insert seen next) (next <> xs)

-- | The module name of a file: its path relative to the root (a relative
-- path is taken as already relative), without @.lean@, dots for slashes.
moduleOf :: FilePath -> FilePath -> Text
moduleOf root file =
  let rel
        | isAbsolute file = makeRelative (normalise root) (normalise file)
        | otherwise = normalise file
      noExt = if takeExtension rel == ".lean" then dropExtension rel else rel
   in T.intercalate "." [T.pack d | d <- splitDirectories noExt, d /= ".", d /= "/"]

-- Files ----------------------------------------------------------------

-- | The @.lean@ files under the source directory, relative to the root.
leanFiles :: FilePath -> FilePath -> IO [FilePath]
leanFiles root src = sort <$> walk (if null src then "" else src)
  where
    walk rel = do
      xs <- either (const []) id <$> (try (listDirectory (root </> rel)) :: IO (Either IOException [FilePath]))
      concat <$> forM (sort xs) (\x -> do
        let r = if null rel then x else rel </> x
            p = root </> r
        isLink <- either (const False) id <$> (try (pathIsSymbolicLink p) :: IO (Either IOException Bool))
        isDir <- doesDirectoryExist p
        if isDir
          then if isLink || "." `isPrefixOf` x || x == "lake-packages" then pure [] else walk r
          else pure [r | takeExtension x == ".lean", not ("." `isPrefixOf` x), x `notElem` lakefiles])

-- Cache ----------------------------------------------------------------

data RawDecl = RawDecl
  { rdName :: Text
  , rdKind :: Kind
  , rdStatement :: Text
  , rdLine :: Int
  , rdIdents :: [Text]
  -- ^ identifiers of the statement after the name, comments left out
  , rdNamespace :: [Text]
  , rdOpens :: [Text]
  , rdPrivate :: Bool
  }
  deriving (Eq, Show)

data FileResult = Parsed [Text] [RawDecl] | Failed Text
  deriving (Eq, Show)

data CacheEntry = CacheEntry {ceMtime :: Integer, ceSize :: Integer, ceResult :: FileResult}
  deriving (Eq, Show)

-- | Bump 'scannerVersion' whenever the scan of a file can give a different
-- result, so cached scans from an older scanner are thrown away.
scannerVersion :: Int
scannerVersion = 2

cacheVersion :: Int
cacheVersion = 100 * cacheFormat + scannerVersion
  where
    cacheFormat = 1

instance ToJSON RawDecl where
  toJSON d =
    object
      [ "name" .= rdName d
      , "kind" .= kindText (rdKind d)
      , "statement" .= rdStatement d
      , "line" .= rdLine d
      , "idents" .= rdIdents d
      , "namespace" .= rdNamespace d
      , "opens" .= rdOpens d
      , "private" .= rdPrivate d
      ]

instance FromJSON RawDecl where
  parseJSON = withObject "decl" $ \o ->
    RawDecl
      <$> o .: "name"
      <*> (o .: "kind" >>= kindP)
      <*> o .: "statement"
      <*> o .: "line"
      <*> o .: "idents"
      <*> o .: "namespace"
      <*> o .: "opens"
      <*> o .: "private"
    where
      kindP :: Text -> Parser Kind
      kindP t = maybe (fail "unknown kind") pure (lookup t [(kindText k, k) | k <- [minBound .. maxBound]])

instance ToJSON CacheEntry where
  toJSON c =
    object $
      ["mtime" .= ceMtime c, "size" .= ceSize c] <> case ceResult c of
        Parsed is ds -> ["imports" .= is, "decls" .= ds]
        Failed why -> ["skipped" .= why]

instance FromJSON CacheEntry where
  parseJSON = withObject "file" $ \o -> do
    res <-
      o .:? "skipped" >>= \case
        Just why -> pure (Failed why)
        Nothing -> Parsed <$> o .: "imports" <*> o .: "decls"
    CacheEntry <$> o .: "mtime" <*> o .: "size" <*> pure res

readCache :: FilePath -> IO (Map FilePath CacheEntry)
readCache path = do
  r <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
  pure $ case r of
    Left _ -> M.empty
    Right bs -> case eitherDecodeStrict bs of
      Right (Object o) | Just v <- parseMaybe' o -> v
      _ -> M.empty
  where
    parseMaybe' o = case fromJSON (Object o) of
      Success (CacheFile v fs) | v == cacheVersion -> Just fs
      _ -> Nothing

data CacheFile = CacheFile Int (Map FilePath CacheEntry)

instance FromJSON CacheFile where
  parseJSON = withObject "index" $ \o -> CacheFile <$> o .: "version" <*> o .: "files"

-- | Best effort: a cache that cannot be written only costs a full scan next time.
writeCache :: FilePath -> Map FilePath CacheEntry -> IO ()
writeCache path fs = do
  _ <-
    try
      ( do
          let dir = takeDirectory path
          createDirectoryIfMissing True dir
          (tmp, h) <- openBinaryTempFile dir (takeFileName path <> ".tmp")
          ( do
              LBS.hPut h (encode (object ["version" .= cacheVersion, "files" .= fs]))
              hClose h
              renameFile tmp path
            )
            `onException` (hClose h >> (try (removeFile tmp) :: IO (Either IOException ())))
      ) ::
      IO (Either IOException ())
  pure ()

-- Assembly -------------------------------------------------------------

-- | From the source directory (module names), the package root (file
-- paths) and each file's scan.
assemble :: FilePath -> FilePath -> [(FilePath, FileResult)] -> Index
assemble srcRoot root parsed = Index entries imports byName
  where
    modOf rel = moduleOf srcRoot (root </> rel)
    raws = [(rel, modOf rel, d) | (rel, Parsed _ ds) <- parsed, d <- ds]
    public = Set.fromList [rdName d | (_, _, d) <- raws, not (rdPrivate d)]
    privates = M.fromListWith Set.union [(rel, Set.singleton (rdName d)) | (rel, _, d) <- raws, rdPrivate d]
    entries =
      [ entryOf public (M.findWithDefault Set.empty rel privates) rel m d
      | (rel, m, d) <- raws
      ]
    byName = M.fromListWith (\_ o -> o) (zip (map entryKey entries) [0 ..])
    imports = M.fromList [(modOf rel, is) | (rel, Parsed is _) <- parsed]

-- | An entry from its scan, given the public names of the whole project
-- and the private names of its own file.
entryOf :: Set Text -> Set Text -> FilePath -> Text -> RawDecl -> Entry
entryOf public mine rel m d =
  Entry (rdName d) (rdKind d) (rdStatement d) rel (rdLine d) m (resolve visible key d) (rdPrivate d)
  where
    visible c = Set.member c mine || Set.member c public
    key c = if Set.member c mine then privateKey m c else c

-- | The entries a file of the index would have if its text were the one
-- given (the file relative to the package root, like 'eFile', and its
-- module), names resolved against the rest of the index and the text's
-- own declarations; or why the text does not parse. For a file changed
-- during a run, such as a search's merged file.
scanText :: Index -> FilePath -> Text -> Text -> Either Text [Entry]
scanText ix rel m t = map fst <$> scanTextWith ix rel m t

-- | 'scanText', each entry with a resolver for other text of the same
-- declaration (such as its proof): the index keys (and the text's own
-- declarations' keys) that text's identifiers refer to, read as the
-- statement's are, under the declaration's namespaces and opens.
-- Comments and string literals in that text are left out.
scanTextWith :: Index -> FilePath -> Text -> Text -> Either Text [(Entry, Text -> [Text])]
scanTextWith ix rel m t = case parseFile (encodeUtf8 t) of
  Failed why -> Left why
  Parsed _ ds ->
    let here = normalise rel
        public =
          Set.fromList
            ( [eName e | e <- ixEntries ix, not (ePrivate e), normalise (eFile e) /= here]
                <> [rdName d | d <- ds, not (rdPrivate d)]
            )
        mine = Set.fromList [rdName d | d <- ds, rdPrivate d]
        visible c = Set.member c mine || Set.member c public
        key c = if Set.member c mine then privateKey m c else c
        other d txt =
          let masked = either (const txt) T.pack (maskSource (T.unpack txt))
           in resolve visible key d {rdIdents = identsIn masked}
     in Right [(entryOf public mine rel m d, other d) | d <- ds]

-- | The index keys the statement's identifiers can refer to, given which
-- plain names are visible from the declaration's file and their keys.
resolve :: (Text -> Bool) -> (Text -> Text) -> RawDecl -> [Text]
resolve visible key d = nubOrd [key r | t <- rdIdents d, r <- one t, r /= rdName d]
  where
    ns = rdNamespace d
    prefixes = [T.intercalate "." (take k ns) | k <- [length ns, length ns - 1 .. 1]]
    opens = concat [[p <> "." <> o | p <- prefixes] <> [o] | o <- rdOpens d]
    one t = case T.stripPrefix "_root_." t of
      Just r -> take 1 [c | c <- dotPrefixes r, visible c]
      Nothing ->
        firstNonEmpty
          [ filter visible (nubOrd (c : [p <> "." <> c | p <- prefixes] <> [o <> "." <> c | o <- opens]))
          | c <- dotPrefixes t
          ]
    dotPrefixes t = let cs = T.splitOn "." t in [T.intercalate "." (take k cs) | k <- [length cs, length cs - 1 .. 1]]
    firstNonEmpty xs = case dropWhile null xs of
      (x : _) -> x
      [] -> []

-- Parsing one file -----------------------------------------------------

parseFile :: BS.ByteString -> FileResult
parseFile bs = case decodeUtf8' bs of
  Left _ -> Failed "not valid UTF-8"
  Right t0 ->
    let t = T.replace "\r\n" "\n" (T.dropWhile (== '\xFEFF') t0)
     in case maskSource (T.unpack t) of
          Left why -> Failed why
          Right m ->
            let ols = T.splitOn "\n" t
                mls = T.splitOn "\n" (T.pack m)
             in Parsed (importsOf mls) (scan ols mls)

-- | Blank out comments, string literals and character literals (newlines
-- kept, so lines and columns are unchanged), or say why the file does not
-- parse.
maskSource :: String -> Either Text String
maskSource = go 1 ' '
  where
    go :: Int -> Char -> String -> Either Text String
    go _ _ [] = Right []
    go ln prev s@(c : cs)
      | "--" `isPrefixOf` s =
          let (com, rest) = break (== '\n') s in (blank com <>) <$> go ln ' ' rest
      | "/-" `isPrefixOf` s = case block 1 (drop 2 s) "" of
          Nothing -> Left ("unterminated block comment opened at line " <> T.pack (show ln))
          Just (body, rest) -> (blank ("/-" <> body) <>) <$> go (ln + newlines body) ' ' rest
      | c == '"' = case str cs "" of
          Nothing -> Left ("unterminated string literal at line " <> T.pack (show ln))
          Just (body, rest) -> (blank ('"' : body) <>) <$> go (ln + newlines body) ' ' rest
      | c == '\'', not (identChar prev), Just (body, rest) <- charLit cs = (blank ('\'' : body) <>) <$> go ln ' ' rest
      | otherwise = (c :) <$> go (if c == '\n' then ln + 1 else ln) c cs
    blank = map (\x -> if x == '\n' then '\n' else ' ')
    newlines = length . filter (== '\n')
    -- the comment's text after "/-" through its closing "-/", nesting counted
    block :: Int -> String -> String -> Maybe (String, String)
    block depth s acc = case s of
      [] -> Nothing
      '-' : '/' : r
        | depth == 1 -> Just (reverse ('/' : '-' : acc), r)
        | otherwise -> block (depth - 1) r ('/' : '-' : acc)
      '/' : '-' : r -> block (depth + 1) r ('-' : '/' : acc)
      x : r -> block depth r (x : acc)
    str :: String -> String -> Maybe (String, String)
    str s acc = case s of
      [] -> Nothing
      '\\' : x : r -> str r (x : '\\' : acc)
      '"' : r -> Just (reverse ('"' : acc), r)
      x : r -> str r (x : acc)
    charLit s = case s of
      '\\' : r | (esc, '\'' : rest) <- break (== '\'') r, length esc <= 8, '\n' `notElem` esc -> Just ('\\' : esc <> "'", rest)
      x : '\'' : rest | x /= '\n' -> Just ([x, '\''], rest)
      _ -> Nothing

identChar :: Char -> Bool
identChar c = isAlphaNum c || c `elem` ("_'!?." :: String)

-- | The modules named on @import@ lines (also @public import@, @import all@).
importsOf :: [Text] -> [Text]
importsOf mls =
  concat
    [ filter (/= "all") rest
    | l <- mls
    , startsAtColumn0 l
    , ("import" : rest) <- [dropWhile (`elem` ["public", "private", "meta"]) (T.words l)]
    ]

startsAtColumn0 :: Text -> Bool
startsAtColumn0 l = not (T.null l) && not (isSpace (T.head l))

data Scope = Root | Ns Text | Sec Text | Mutual
  deriving (Eq)

-- | One frame per namespace component, section or mutual block, with the
-- namespaces opened in it.
data Frame = Frame Scope [Text]

-- | The declarations of a file, from its original and masked lines.
scan :: [Text] -> [Text] -> [RawDecl]
scan ols mls = go [Frame Root []] [] (zip3 [0 ..] ols mls)
  where
    go :: [Frame] -> [Text] -> [(Int, Text, Text)] -> [RawDecl]
    go _ _ [] = []
    go stack pending (cur@(i, _, m) : rest)
      | startsAtColumn0 m, ("namespace" : nm : _) <- ws =
          go (reverse [Frame (Ns c) [] | c <- T.splitOn "." nm] <> stack) pending rest
      | startsAtColumn0 m, ("section" : nm) <- dropMods ws =
          go (Frame (Sec (T.unwords (take 1 nm))) [] : stack) pending rest
      | startsAtColumn0 m, ["mutual"] <- ws = go (Frame Mutual [] : stack) pending rest
      | startsAtColumn0 m, ("end" : nm) <- ws = go (close (take 1 nm) stack) pending rest
      | startsAtColumn0 m, (os, c) <- inPrefixes m, c > 0 =
          if T.null (T.strip (T.drop c m))
            then go stack (pending <> os) rest
            else declOr (pending <> os) c
      | startsAtColumn0 m, ("open" : names) <- ws = go (addOpens (openNames names) stack) pending rest
      | otherwise = declOr pending 0
      where
        ws = T.words m
        declOr pend c0 = case declHead c0 m of
          Just h -> decl pend h
          Nothing
            | startsAtColumn0 m, not ("@[" `T.isPrefixOf` m) -> go stack [] rest
            | otherwise -> go stack pend rest
        decl pend (kwCol, kind, priv, mname, after) =
          let here = case mname of
                Nothing -> []
                Just nm0 ->
                  let nss = [x | Frame (Ns x) _ <- reverse stack]
                      nm = case T.stripPrefix "_root_." nm0 of
                        Just r -> r
                        Nothing -> T.intercalate "." (nss <> [nm0])
                      (k, cut) = statementEnd kwCol (map (\(_, _, x) -> x) (cur : rest))
                      stmt = spanText kwCol k cut (map (\(_, x, _) -> x) (cur : rest))
                      mstmt = spanText kwCol k cut (map (\(_, _, x) -> x) (cur : rest))
                      idents = identsIn (T.drop (after - kwCol) mstmt)
                      opens = pend <> concat [os | Frame _ os <- stack]
                   in [RawDecl nm kind (T.take statementCap (T.stripEnd stmt)) (i + 1) idents nss opens priv]
           in here <> go stack [] rest
    -- leading "open X in" and "set_option o v in" commands on a line: the
    -- namespaces they open and the column after the last "in" (0 for none)
    inPrefixes :: Text -> ([Text], Int)
    inPrefixes m = loop [] 0
      where
        loop acc c = case wordsFrom c of
          ((_, w) : more)
            | w `elem` ["open", "set_option"]
            , (before, (ci, _) : _) <- break ((== "in") . snd) more ->
                loop (acc <> (if w == "open" then openNames (map snd before) else [])) (ci + 2)
          _ -> (acc, c)
        wordsFrom c = wordsAt c (T.drop c m)
        wordsAt c t
          | T.null t = []
          | isSpace (T.head t) = let sp = T.takeWhile isSpace t in wordsAt (c + T.length sp) (T.drop (T.length sp) t)
          | otherwise = let w = T.takeWhile (not . isSpace) t in (c, w) : wordsAt (c + T.length w) (T.drop (T.length w) t)
    dropMods = dropWhile (\w -> w `elem` ["noncomputable", "public", "private"] || "@[" `T.isPrefixOf` w)
    openNames = takeWhile (\w -> w `notElem` ["hiding", "renaming", "in"] && not ("(" `T.isPrefixOf` w)) . filter (/= "scoped")
    addOpens os (Frame s o : fs) = Frame s (o <> os) : fs
    addOpens _ [] = []
    close name stack = case (name, stack) of
      (_, [Frame Root _]) -> stack
      ([], Frame (Sec _) _ : fs) -> fs
      ([], Frame Mutual _ : fs) -> fs
      ([], _) -> stack
      ([nm], Frame (Sec s) _ : fs) | s == nm -> fs
      ([nm], _) -> dropNs (length (T.splitOn "." nm)) stack
      _ -> stack
    dropNs :: Int -> [Frame] -> [Frame]
    dropNs 0 fs = fs
    dropNs k (f@(Frame s _) : fs)
      | s == Root = f : fs
      | otherwise = dropNs (k - 1) fs
    dropNs _ [] = []

-- | The text from the keyword (line 0, column kwCol) to the cut on line k.
spanText :: Int -> Int -> Int -> [Text] -> Text
spanText kwCol k cut ls =
  let sel = take (k + 1) ls
      sel' = case reverse sel of
        (l : before) -> reverse before <> [T.take cut l]
        [] -> []
   in T.drop kwCol (T.intercalate "\n" sel')

-- | Where the statement of a declaration stops, over the masked lines
-- from the keyword's line: just past the first @:=@ or @where@ outside
-- brackets, or at the end of the line before a line starting with @|@ or
-- indented no deeper than the keyword's line, or at the end of the file.
-- Returns the line offset and the column on that line.
statementEnd :: Int -> [Text] -> (Int, Int)
statementEnd kwCol mls = loop 0 0 mls
  where
    indent l = T.length (T.takeWhile isSpace l)
    indent0 = maybe 0 indent (safeHead mls)
    safeHead = \case
      (x : _) -> Just x
      [] -> Nothing
    endOf k = (k, maybe 0 T.length (safeHead (drop k mls)))
    loop :: Int -> Int -> [Text] -> (Int, Int)
    loop k _ [] = endOf (max 0 (k - 1))
    loop k depth (l : ls)
      | k > 0, not (T.null (T.strip l)), indent l <= indent0 || (depth == 0 && "|" `T.isPrefixOf` T.stripStart l) =
          endOf (k - 1)
      | otherwise =
          let start = if k == 0 then kwCol else 0
           in case cutIn depth start l of
                Right c -> (k, c)
                Left depth' -> loop (k + 1) depth' ls

-- | Scan one masked line from a column with a bracket depth: @Right@ the
-- column just past @:=@ or @where@ at depth 0, or @Left@ the depth at the end.
cutIn :: Int -> Int -> Text -> Either Int Int
cutIn depth0 start l = go start depth0 (T.unpack (T.drop start l))
  where
    go _ depth [] = Left depth
    go i depth s@(c : rest)
      | c `elem` ("([{⟨⦃" :: String) = go (i + 1) (depth + 1) rest
      | c `elem` (")]}⟩⦄" :: String) = go (i + 1) (max 0 (depth - 1)) rest
      | depth == 0, ":=" `isPrefixOf` s, not (prevIn ":=<>!|&"), not (nextIn (drop 2 s) "=:") = Right (i + 2)
      | depth == 0, "where" `isPrefixOf` s, not (prevIdent i), not (nextIdent (drop 5 s)) = Right (i + 5)
      | otherwise = go (i + 1) depth rest
      where
        prevIn cs = i > 0 && T.index l (i - 1) `elem` (cs :: String)
    nextIn s cs = case s of
      (x : _) -> x `elem` (cs :: String)
      [] -> False
    prevIdent i = i > 0 && identChar (T.index l (i - 1))
    nextIdent = \case
      (x : _) -> identChar x
      [] -> False

-- | The column of the keyword, the kind, the name (Nothing for an
-- unnamed instance or a name we cannot read) and the column after the
-- name, for a masked line opening a declaration.
declHead :: Int -> Text -> Maybe (Int, Kind, Bool, Maybe Text, Int)
declHead c0 l = goAttrs (skipSp c0)
  where
    at c = T.drop c l
    skipSp c = c + T.length (T.takeWhile isSpace (at c))
    word c = T.takeWhile (not . isSpace) (at c)
    goAttrs c
      | "@[" `T.isPrefixOf` at c = closeAt '[' ']' (c + 1) >>= goAttrs . skipSp
      | otherwise = goMods False c
    goMods priv c =
      let w = word c
          c' = skipSp (c + T.length w)
       in if
            | w `elem` mods -> goMods (priv || w == "private") c'
            | w == "class", word c' == "inductive" -> Just (named priv c KClass (skipSp (c' + T.length "inductive")))
            | Just k <- lookup w kws -> Just (named priv c k c')
            | otherwise -> Nothing
    named priv kwc k cn =
      let c = if k == KInstance && "(priority" `T.isPrefixOf` at cn then maybe cn skipSp (closeAt '(' ')' cn) else cn
          nm = T.takeWhile (\ch -> not (isSpace ch) && ch `notElem` ("({[:⦃⟨" :: String)) (at c)
          ok = maybe False (\(h, _) -> isAlpha h || h == '_' || h == '«') (T.uncons nm)
          nm' = fst (T.breakOn ".{" nm)
       in if ok then (kwc, k, priv, Just nm', c + T.length nm) else (kwc, k, priv, Nothing, c)
    -- the column just past the bracket matching the one at column c
    closeAt open shut c = loop (c + 1) (1 :: Int)
      where
        loop i d
          | i >= T.length l = Nothing
          | T.index l i == open = loop (i + 1) (d + 1)
          | T.index l i == shut = if d == 1 then Just (i + 1) else loop (i + 1) (d - 1)
          | otherwise = loop (i + 1) d
    mods = ["private", "protected", "noncomputable", "nonrec", "partial", "unsafe", "scoped", "local"] :: [Text]
    kws = [(kindText k, k) | k <- [minBound .. maxBound]]

-- | The identifiers of masked statement text: dotted names whose
-- components start with a letter or @_@ (a numeric projection ends one),
-- in order of first appearance.
identsIn :: Text -> [Text]
identsIn t = nubOrd [i | w <- T.split (not . identChar) t, Just i <- [clean w]]
  where
    clean w =
      let comps = takeWhile okComp (T.splitOn "." w)
       in if null comps then Nothing else Just (T.intercalate "." comps)
    okComp c = maybe False (\(h, _) -> isAlpha h || h == '_') (T.uncons c)
