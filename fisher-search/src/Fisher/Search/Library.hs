-- | The lemma library: per project, the helper lemmas earlier search runs
-- proved and merged (each checked by the kernel with its goal), and short
-- notes from graph runs, offered to later goals.
--
-- After a run, 'harvest' compares the run's merged file with its starting
-- file and keeps each new theorem or lemma that uses no @sorry@;
-- 'harvestNotes' keeps the graph's failure lines and workers' @NOTE:@
-- lines. 'saveLibrary' appends them to @lemmas.jsonl@ and @notes.jsonl@ in
-- the project's library directory ('libraryDir'), leaving out what is
-- already there, under a lock so two runs of one project lose no line.
-- 'offer' picks the lemmas and notes for one goal and 'libraryBrief'
-- renders them for its brief. An offered lemma pasted above a goal is a
-- helper like any other: the kernel checks it with the goal.
module Fisher.Search.Library
  ( Lemma (..)
  , Note (..)
  , Library
  , lemmaCap
  , noteCap
  , noteChars
  , lemmaChars
  , briefChars
  , libraryKey
  , libraryDir
  , harvest
  , harvestNotes
  , whiteboardFailures
  , saveLibrary
  , loadLibrary
  , loadLibraryReport
  , peekLibrary
  , offer
  , offerer
  , normalStatement
  , libraryBrief
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, bracket, onException, try)
import Control.Monad (unless)
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isSpace)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import GHC.IO.Handle.Lock (LockMode (ExclusiveLock), hTryLock)
import System.Directory
import System.FilePath
import System.IO (Handle, IOMode (ReadWriteMode), hClose, openFile)

import Fisher.LLM.Auth.Sha256 (sha256Hex)
import Fisher.Search.Bm25 (Corpus, corpus, rank, tokens)
import Fisher.Search.Facts (goalModule)
import Fisher.Search.Index
import Fisher.Search.Lean (Decl (..), declarations)
import Fisher.Search.Splice (usesSorry)

data Lemma = Lemma
  { lmName :: Text
  -- ^ fully qualified, as 'declarations' gives it
  , lmStatement :: Text
  -- ^ from the keyword through @:=@ (and @ by@), as written
  , lmProof :: Text
  -- ^ what follows the statement, as 'dProof' gives it
  , lmFile :: FilePath
  -- ^ the file it was merged into, relative to the package root
  , lmModule :: Text
  , lmRun :: FilePath
  -- ^ the run directory
  , lmDate :: Text
  -- ^ UTC, ISO 8601; 'saveLibrary' stamps a lemma that has none
  , lmUses :: [Text]
  -- ^ the project constants its statement uses ('ixByName' keys)
  , lmProofUses :: [Text]
  -- ^ the project constants its proof uses, resolved as the statement's
  -- are: index keys, or the names of lemmas saved with it from the same
  -- run (offered together with it)
  }
  deriving (Eq, Show)

data Note = Note
  { ntText :: Text
  , ntFile :: FilePath
  , ntGoal :: Text
  -- ^ the node or goal the note is about
  , ntRun :: FilePath
  }
  deriving (Eq, Show)

-- | Lemmas and notes, as loaded, harvested or offered.
type Library = ([Lemma], [Note])

-- | At most this many lemmas and notes are offered to one goal.
lemmaCap, noteCap :: Int
lemmaCap = 5
noteCap = 3

-- | A note is kept to this many characters (on harvest and on load).
noteChars :: Int
noteChars = 300

-- | A lemma whose statement plus proof is longer than 'lemmaChars' is
-- never put in a brief, and lemmas stop once their statements and proofs
-- together would pass 'briefChars'.
lemmaChars, briefChars :: Int
lemmaChars = 1500
briefChars = 6000

instance ToJSON Lemma where
  toJSON l =
    object
      [ "name" .= lmName l
      , "statement" .= lmStatement l
      , "proof" .= lmProof l
      , "file" .= lmFile l
      , "module" .= lmModule l
      , "run" .= lmRun l
      , "date" .= lmDate l
      , "uses" .= lmUses l
      , "proof_uses" .= lmProofUses l
      ]

instance FromJSON Lemma where
  parseJSON = withObject "lemma" $ \o ->
    Lemma
      <$> o .: "name"
      <*> o .: "statement"
      <*> o .: "proof"
      <*> o .: "file"
      <*> o .: "module"
      <*> o .: "run"
      <*> o .: "date"
      <*> o .: "uses"
      <*> (o .:? "proof_uses" .!= [])

instance ToJSON Note where
  toJSON n = object ["text" .= ntText n, "file" .= ntFile n, "goal" .= ntGoal n, "run" .= ntRun n]

instance FromJSON Note where
  parseJSON = withObject "note" $ \o -> Note <$> o .: "text" <*> o .: "file" <*> o .: "goal" <*> o .: "run"

-- Where ----------------------------------------------------------------

-- | The first 16 hex characters of the sha256 of the project root's
-- absolute path (UTF-8, normalised, no trailing separator). The caller
-- gives the absolute path of the original project (never an attempt
-- workspace).
libraryKey :: FilePath -> Text
libraryKey root = T.take 16 (sha256Hex (TE.encodeUtf8 (T.pack (canon root))))

canon :: FilePath -> FilePath
canon = dropTrailingPathSeparator . normalise

-- | The project's library directory under a home (@$GAUSS_HOME@):
-- @home/fisher/library/<key>@. (@--library-dir DIR@ names the library
-- directory itself instead.)
libraryDir :: FilePath -> FilePath -> FilePath
libraryDir home root = home </> "fisher" </> "library" </> T.unpack (libraryKey root)

-- Harvest --------------------------------------------------------------

-- | The new sorry-free theorems and lemmas of a run. Index, file
-- (relative to the package root), start text, merged text, run directory.
-- A declaration of the merged text is old when the start text declares
-- its name or its statement ('normalStatement', so a renamed, moved or
-- reordered one stays old); a new one is kept when its statement and
-- proof use no @sorry@, @admit@ or @sorryAx@ and the index's scanner
-- reads it in the merged text. Its uses are the project constants its
-- statement resolves to there, its proof uses those its proof resolves
-- to. A kept lemma whose proof names a declaration of the merged text
-- that is new in this run, or that uses @sorry@, is dropped unless that
-- declaration is itself kept (so a helper using a kept helper stays, with
-- the dependency in its proof uses; one using a sibling still @sorry@ goes,
-- as does what uses it). Nothing when the merged text does not scan. No
-- notes (see 'harvestNotes'); dates are left for 'saveLibrary'.
harvest :: Index -> FilePath -> Text -> Text -> FilePath -> Library
harvest ix file before after run = case scanTextWith ix file modName after of
  Left _ -> ([], [])
  Right es ->
    let scanned = M.fromList [(eName e, (e, res)) | (e, res) <- reverse es]
        afterDecls = declarations after
        keyOf d = maybe (dName d) (entryKey . fst) (M.lookup (dName d) scanned)
        isFresh d = Set.notMember (dName d) oldNames && Set.notMember (normalStatement (dStatement d)) oldStmts
        bad d = cheats (dProof d) || cheats (dStatement d)
        -- declarations a kept proof may name only when they are kept too
        guarded = Set.fromList [keyOf d | d <- afterDecls, isFresh d || bad d]
        cands =
          [ (keyOf d, toLemma d (eUses e) (res (dProof d)))
          | d <- afterDecls, isFresh d, not (bad d)
          , Just (e, res) <- [M.lookup (dName d) scanned] ]
        settle cs =
          let kept = Set.fromList (map fst cs)
              cs' = [c | c@(_, l) <- cs, all (\p -> Set.notMember p guarded || Set.member p kept) (lmProofUses l)]
           in if length cs' == length cs then cs else settle cs'
     in (map snd (settle cands), [])
  where
    olds = declarations before
    oldNames = Set.fromList (map dName olds)
    oldStmts = Set.fromList (map (normalStatement . dStatement) olds)
    modName = fromMaybe (fallbackModule file) (goalModule ix file)
    toLemma d us pus =
      Lemma
        { lmName = dName d
        , lmStatement = dStatement d
        , lmProof = dProof d
        , lmFile = file
        , lmModule = modName
        , lmRun = run
        , lmDate = ""
        , lmUses = us
        , lmProofUses = pus
        }

-- | Text that uses @sorry@, @admit@ or @sorryAx@.
cheats :: Text -> Bool
cheats t = usesSorry t || "sorryAx" `T.isInfixOf` t

-- | A file the index does not know: its path, dotted.
fallbackModule :: FilePath -> Text
fallbackModule = moduleOf ""

-- | Notes from (goal or node, text) pairs: whitespace runs collapsed, cut
-- to 'noteChars', empty ones dropped, each text once (the first kept).
harvestNotes :: FilePath -> FilePath -> [(Text, Text)] -> [Note]
harvestNotes file run items = dedupeNotes Set.empty [Note t file g run | (g, raw) <- items, let t = cleanNote raw, not (T.null t)]

cleanNote :: Text -> Text
cleanNote = T.take noteChars . oneLine

oneLine :: Text -> Text
oneLine = T.unwords . T.words

-- | The (node, note) lines of a whiteboard's @## Failed@ section, as
-- "Fisher.Search.Whiteboard" renders them (@name: note@).
whiteboardFailures :: Text -> [(Text, Text)]
whiteboardFailures board =
  [ (T.strip n, T.strip (T.drop 1 r))
  | l <- drop 1 (dropWhile ((/= "## Failed") . T.strip) (T.lines board))
  , let (n, r) = T.breakOn ":" l
  , not (T.null r)
  , not (T.null (T.strip n))
  ]

-- Storage --------------------------------------------------------------

lemmasFile, notesFile, rootFile, lockFile :: FilePath -> FilePath
lemmasFile d = d </> "lemmas.jsonl"
notesFile d = d </> "notes.jsonl"
rootFile d = d </> "root.txt"
lockFile d = d </> "lock"

-- | Append lemmas and notes to the library directory (created if
-- missing; @root.txt@ naming the project root written on the first save):
-- library directory, project root, lemmas, notes. A lemma already there
-- with the same name and statement (whitespace normalised), or a note
-- with the same text, is left out, as is a repeat within the call.
-- Returns the counts appended. Runs under the directory's lock, so
-- concurrent saves (threads or processes) lose no line. A library file
-- that fails to parse is set aside as in 'loadLibrary'. Throws on an
-- I/O error; the caller decides whether that matters.
saveLibrary :: FilePath -> FilePath -> [Lemma] -> [Note] -> IO (Int, Int)
saveLibrary dir root ls ns = withLock dir $ do
  hasRoot <- doesFileExist (rootFile dir)
  unless hasRoot $ BS.writeFile (rootFile dir) (TE.encodeUtf8 (T.pack root <> "\n"))
  (oldL, _) <- readJsonl (lemmasFile dir) :: IO ([Lemma], Bool)
  (oldN, _) <- readJsonl (notesFile dir) :: IO ([Note], Bool)
  now <- T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" <$> getCurrentTime
  let oldStmts = Set.fromList (map (normalStatement . lmStatement) oldL)
      newL = dedupeStatements oldStmts (dedupeLemmas (Set.fromList (map lemmaKey oldL)) [if T.null (lmDate l) then l {lmDate = now} else l | l <- ls])
      newN = dedupeNotes (Set.fromList (map noteKey oldN)) ns
  appendJsonl (lemmasFile dir) newL
  appendJsonl (notesFile dir) newN
  pure (length newL, length newN)

-- | Everything in a library directory (empty when it does not exist). A
-- file with a line that does not parse is renamed with a @.bad@ suffix,
-- a fresh empty one is started, and that file contributes nothing.
loadLibrary :: FilePath -> IO Library
loadLibrary dir = fst <$> loadLibraryReport dir

-- | 'loadLibrary', also naming the files set aside (their new paths).
loadLibraryReport :: FilePath -> IO (Library, [FilePath])
loadLibraryReport dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure (([], []), [])
    else withLock dir $ do
      (ls, badL) <- readJsonl (lemmasFile dir)
      (ns, badN) <- readJsonl (notesFile dir)
      pure ((ls, [n {ntText = T.take noteChars (ntText n)} | n <- ns]), [p <> ".bad" | (p, True) <- [(lemmasFile dir, badL), (notesFile dir, badN)]])

-- | The library as it is, read without the lock and without writing
-- anything (no directory made, no file set aside or rewritten): lines that
-- do not parse are skipped. For @--print-facts@.
peekLibrary :: FilePath -> IO Library
peekLibrary dir = do
  ls <- peek (lemmasFile dir)
  ns <- peek (notesFile dir)
  pure (ls, [n {ntText = T.take noteChars (ntText n)} | n <- ns])
  where
    peek :: FromJSON a => FilePath -> IO [a]
    peek p = do
      r <- try (BS.readFile p) :: IO (Either IOException BS.ByteString)
      pure $ case r of
        Left _ -> []
        Right bs -> [x | l <- BC.lines bs, Just x <- [decodeStrict' l]]

lemmaKey :: Lemma -> (Text, Text)
lemmaKey l = (lmName l, sha256Hex (TE.encodeUtf8 (oneLine (lmStatement l))))

noteKey :: Note -> Text
noteKey = oneLine . ntText

dedupeLemmas :: Set.Set (Text, Text) -> [Lemma] -> [Lemma]
dedupeLemmas _ [] = []
dedupeLemmas seen (l : rest)
  | Set.member k seen = dedupeLemmas seen rest
  | otherwise = l : dedupeLemmas (Set.insert k seen) rest
  where
    k = lemmaKey l

-- | Lemmas whose statement ('normalStatement') is not seen yet: a renamed
-- copy of a lemma is not saved again.
dedupeStatements :: Set.Set Text -> [Lemma] -> [Lemma]
dedupeStatements _ [] = []
dedupeStatements seen (l : rest)
  | Set.member k seen = dedupeStatements seen rest
  | otherwise = l : dedupeStatements (Set.insert k seen) rest
  where
    k = normalStatement (lmStatement l)

dedupeNotes :: Set.Set Text -> [Note] -> [Note]
dedupeNotes _ [] = []
dedupeNotes seen (n : rest)
  | Set.member k seen = dedupeNotes seen rest
  | otherwise = n : dedupeNotes (Set.insert k seen) rest
  where
    k = noteKey n

-- | The records of a JSON-lines file, and whether it was set aside. A
-- last line that does not parse is taken for an append cut off by a
-- crash: it is dropped and the file rewritten without it. Any other line
-- that does not parse sets the file aside: it is renamed @*.bad@ and an
-- empty one written. A missing file has none. Call under the lock.
readJsonl :: FromJSON a => FilePath -> IO ([a], Bool)
readJsonl p = do
  exists <- doesFileExist p
  if not exists
    then pure ([], False)
    else do
      bs <- BS.readFile p
      let ls = filter (not . BC.all isSpace) (BC.lines bs)
          decoded = map decodeStrict' ls
      case (traverse id decoded, traverse id (dropLast decoded)) of
        (Just xs, _) -> pure (xs, False)
        (Nothing, Just xs) -> do
          BS.writeFile p (BS.concat [l <> "\n" | l <- dropLast ls])
          pure (xs, False)
        (Nothing, Nothing) -> do
          renameFile p (p <> ".bad")
          BS.writeFile p ""
          pure ([], True)
  where
    dropLast xs = take (length xs - 1) xs

appendJsonl :: ToJSON a => FilePath -> [a] -> IO ()
appendJsonl _ [] = pure ()
appendJsonl p xs = BS.appendFile p (BS.concat [LBS.toStrict (encode x) <> "\n" | x <- xs])

-- | Run under the directory's lock (created with the directory). The lock
-- file is held open read-write with an exclusive lock: another process
-- waits on the lock, another thread of this process on GHC's own
-- one-writer rule for the open. Polls every 10 ms, for up to 60 s.
withLock :: FilePath -> IO a -> IO a
withLock dir act = do
  createDirectoryIfMissing True dir
  bracket (acquire (0 :: Int)) hClose (const act)
  where
    acquire n = do
      r <- try (attempt (lockFile dir)) :: IO (Either IOException (Maybe Handle))
      case r of
        Right (Just h) -> pure h
        _
          | n >= 6000 -> ioError (userError ("the library lock " <> lockFile dir <> " stayed taken for 60 s"))
          | otherwise -> threadDelay 10000 >> acquire (n + 1)
    attempt p = do
      h <- openFile p ReadWriteMode
      ok <- hTryLock h ExclusiveLock `onException` hClose h
      if ok then pure (Just h) else hClose h >> pure Nothing

-- Offer ----------------------------------------------------------------

-- | What to offer one goal: index, the goal's file (relative to the
-- package root, like 'eFile'), the goal's constants (as 'ppConstants'
-- gives them; only those in the index count), its statement, the open
-- goals' statements, the library.
--
-- Lemmas: only one whose uses are all still in the index and visible
-- from the goal's file (a private constant only in its own file, any
-- other in the file's module or its import closure), that shares at least
-- one project constant with the goal, whose name the goal's file does not
-- declare, whose statement and proof use no @sorry@, @admit@ or
-- @sorryAx@, and whose statement ('normalStatement') is no open goal's.
-- The caller must include the goal's own statement in the open goals'
-- statements, so a lemma stating the goal itself is never offered.
-- Scored by BM25 of the goal's statement over the lemmas'
-- statements plus 1 for each shared constant, best first (ties by name),
-- one per name, at most 'lemmaCap'.
--
-- Proof constants: each must be visible from the goal's file like the
-- statement's uses, or name another library lemma that passes the same
-- checks (but for the shared constant); that lemma is offered with it,
-- just before it, and counts toward the cap. A lemma whose dependencies
-- do not fit under the cap is left out.
--
-- Notes: those whose text BM25 matches the goal's statement, the goal's
-- file first, then by score, at most 'noteCap'.
offer :: Index -> FilePath -> [Text] -> Text -> [Text] -> Library -> Library
offer ix file goalConsts goalStmt openStmts lib = offerer ix lib file goalConsts goalStmt openStmts

-- | 'offer' with the index and the library first: partially applied, the
-- index's table and the library's BM25 corpora are built once and shared
-- by every goal.
offerer :: Index -> Library -> FilePath -> [Text] -> Text -> [Text] -> Library
offerer ix (ls, ns) = pick
  where
    entries = M.fromList (zip [0 :: Int ..] (ixEntries ix))
    lemmaCorpus = corpus (zip [0 ..] (map (tokens . lmStatement) ls))
    noteCorpus = corpus (zip [0 ..] (map (tokens . ntText) ns))
    notesById = M.fromList (zip [0 :: Int ..] ns)
    pick file goalConsts goalStmt openStmts = offerWith ix entries lemmaCorpus noteCorpus notesById (ls, ns) file goalConsts goalStmt openStmts

offerWith
  :: Index -> M.Map Int Entry -> Corpus -> Corpus -> M.Map Int Note -> Library
  -> FilePath -> [Text] -> Text -> [Text] -> Library
offerWith ix entries lemmaCorpus noteCorpus notesById (ls, _) file goalConsts goalStmt openStmts = (pickLemmas, pickNotes)
  where
    known k = M.member k (ixByName ix)
    closure = maybe Set.empty (importClosure ix) (goalModule ix file)
    visible k = case M.lookup k (ixByName ix) >>= (`M.lookup` entries) of
      Nothing -> False
      Just e
        | ePrivate e -> normalise (eFile e) == here
        | otherwise -> Set.member (eModule e) closure
    goalSet = Set.fromList (filter known goalConsts)
    here = normalise file
    inFile = Set.fromList [eName e | e <- ixEntries ix, normalise (eFile e) == here]
    openSet = Set.fromList (map normalStatement openStmts)
    query = filter (`Set.notMember` declWords) (tokens goalStmt)

    lemmaScores = M.fromList (rank lemmaCorpus query)
    basic l =
      all visible (lmUses l)
        && not (cheats (lmProof l) || cheats (lmStatement l))
        && Set.notMember (lmName l) inFile
        && Set.notMember (normalStatement (lmStatement l)) openSet
    -- library lemmas by name (and by private key), the first of a name
    byName = M.fromListWith (\_ o -> o) (concat [[(lmName l, l), (privateKey (lmModule l) (lmName l), l)] | l <- ls])
    -- the library lemmas a lemma's proof needs first, in order; Nothing
    -- when a proof constant is neither visible nor such a lemma
    deps seen l = concat <$> traverse (need seen) (lmProofUses l)
    need seen p
      | known p = if visible p then Just [] else Nothing
      | Just d <- M.lookup p byName =
          if Set.member (lmName d) seen
            then Just []
            else if basic d then (<> [d]) <$> deps (Set.insert (lmName d) seen) d else Nothing
      | otherwise = Nothing
    candidates =
      [ (negate score, lmName l, ds <> [l])
      | (i, l) <- zip [0 :: Int ..] ls
      , basic l
      , let shared = Set.size (Set.intersection goalSet (Set.fromList (lmUses l)))
      , shared > 0
      , Just ds <- [deps (Set.singleton (lmName l)) l]
      , let score = fromIntegral shared + M.findWithDefault 0 i lemmaScores
      ]
    pickLemmas = fill (Set.empty, Set.empty) [] [g | (_, _, g) <- sortOn (\(s, n, _) -> (s, n)) candidates]
    -- whole groups (dependencies, then the lemma), one lemma per name and
    -- per statement, at most 'lemmaCap' lemmas; a group whose lemma's
    -- statement is already picked under another name is skipped
    fill _ acc [] = acc
    fill seen acc (g : rest)
      | null new || stmtTaken = fill seen acc rest
      | length acc + length new > lemmaCap = fill seen acc rest
      | otherwise = fill (foldr mark seen new) (acc <> new) rest
      where
        new = onePerName seen g
        stmtTaken = case reverse g of
          l : _ -> Set.member (lmName l) (fst seen) == False && Set.member (normalStatement (lmStatement l)) (snd seen)
          [] -> False
    mark l (ns, ss) = (Set.insert (lmName l) ns, Set.insert (normalStatement (lmStatement l)) ss)
    onePerName _ [] = []
    onePerName seen@(ns, ss) (l : rest)
      | Set.member (lmName l) ns || Set.member (normalStatement (lmStatement l)) ss = onePerName seen rest
      | otherwise = l : onePerName (mark l seen) rest

    noteRank = rank noteCorpus query
    ranked = [n | (i, _) <- noteRank, Just n <- [M.lookup i notesById]]
    sameFile n = normalise (ntFile n) == here
    pickNotes = take noteCap (dedupeNotes Set.empty (filter sameFile ranked <> filter (not . sameFile) ranked))

-- | Declaration keywords and modifiers, left out of the query.
declWords :: Set.Set Text
declWords =
  Set.fromList
    ( concatMap (tokens . kindText) [minBound .. maxBound]
        <> ["private", "protected", "noncomputable", "nonrec", "partial", "unsafe", "scoped", "local"]
    )

-- | A statement's proposition for comparison: without attributes,
-- modifiers, the keyword and the name, without a trailing @:=@ or
-- @:= by@, whitespace runs collapsed. Two declarations that state the
-- same thing under different names compare equal.
normalStatement :: Text -> Text
normalStatement s0 = oneLine (stripTail (afterName (dropMods (T.stripStart s0))))
  where
    dropMods s
      | "@[" `T.isPrefixOf` s = dropMods (T.stripStart (T.drop 1 (snd (T.breakOn "]" s))))
      | otherwise =
          let (w, rest) = T.break isSpace s
           in if w `elem` mods then dropMods (T.stripStart rest) else s
    mods = ["private", "protected", "noncomputable", "nonrec", "partial", "unsafe", "scoped", "local"]
    afterName s =
      let (w, rest) = T.break isSpace s
       in if w `elem` ["theorem", "lemma"]
            then T.dropWhile (\c -> not (isSpace c) && c `notElem` ("({[:" :: String)) (T.stripStart rest)
            else s
    stripTail s =
      let t = T.stripEnd s
          noBy = case T.stripSuffix "by" t of
            Just r | ":=" `T.isSuffixOf` T.stripEnd r -> T.stripEnd r
            _ -> t
       in maybe noBy T.stripEnd (T.stripSuffix ":=" noBy)

-- | The library section of a brief: empty when nothing is offered.
libraryBrief :: Library -> Text
libraryBrief (ls, ns) = T.intercalate "\n" (filter (not . T.null) [lemmaPart, notePart])
  where
    offeredNames = Set.fromList (map lmName ls)
    -- a lemma needing another offered one is shown only after it
    shown = fitting Set.empty 0 [l | l <- ls, T.length (source l) <= lemmaChars]
    fitting _ _ [] = []
    fitting names total (l : rest)
      | total' > briefChars = []
      | not (all (`Set.member` names) [p | p <- lmProofUses l, Set.member p offeredNames]) = fitting names total rest
      | otherwise = l : fitting (Set.insert (lmName l) names) total' rest
      where
        total' = total + T.length (source l)
    lemmaPart
      | null shown = ""
      | otherwise =
          "Lemmas proved in earlier runs (paste one above the goal as a helper if it helps):\n"
            <> T.concat [block l | l <- shown]
    block l =
      "```lean\n-- " <> lmName l <> " (" <> T.pack (lmFile l) <> ")" <> needs l <> "\n" <> source l <> "\n```\n"
    needs l = case [p | p <- lmProofUses l, Set.member p offeredNames] of
      [] -> ""
      ps -> "; paste " <> T.intercalate ", " ps <> " above it"
    notePart
      | null ns = ""
      | otherwise = "Notes from earlier runs:\n" <> T.concat ["- " <> ntGoal n <> ": " <> ntText n <> "\n" | n <- ns]

-- | A lemma's text: the statement, then the proof on the same line when
-- it began there, else on the lines below.
source :: Lemma -> Text
source l
  | T.null p = s
  | isSpace (T.head p) = s <> "\n" <> p
  | otherwise = s <> " " <> p
  where
    s = T.stripEnd (lmStatement l)
    p = lmProof l
