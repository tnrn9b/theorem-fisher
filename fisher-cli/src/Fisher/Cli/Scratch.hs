{-# LANGUAGE MultiWayIf #-}
-- | @fisher scratch@: a throwaway Lean environment in seconds. The first
-- scratch for a template and its variables builds a base with the normal
-- create (steps and sample check) under
-- @~/.cache/fisher/scratch/bases/<key>@; every scratch is then a clone of
-- that base (copy on write where the file system has it) under
-- @~/.cache/fisher/scratch/<id>@, with a file to work in next to the
-- template's sample.
--
-- A base is built under the lock @bases/<key>.lock@ (held while it is
-- checked, built and cloned), so two first scratches build it once. A base
-- whose build fails, or that was left half built, is removed and built
-- again next time; one whose build was interrupted (Ctrl-C, or Stop in the
-- UI) is kept, and the next scratch resumes it with @fisher project build@. Cloning a base touches it, so its age is the time since
-- it was last used.
module Fisher.Cli.Scratch
  ( ScratchRequest (..)
  , ScratchDeps (..)
  , defaultScratchDeps
  , scratch
  , scratchWith
  , baseInterrupted
  , baseKey
  , cloneCommand
  , scratchDir
  , listScratch
  , baseRelease
  , pruneScratch
  , scratchMain
  , scratchMainWith
  , scratchUsage
  ) where

import Control.Exception (SomeException, bracket, try)
import Control.Monad (filterM, forM, forM_)
import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.IO as TIO
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import qualified Data.Yaml as Y
import Numeric (showHex)
import System.Directory
  ( copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, doesPathExist, getFileSize
  , getHomeDirectory, getModificationTime, listDirectory, makeAbsolute, pathIsSymbolicLink
  , removeDirectoryRecursive, setModificationTime )
import System.Exit (ExitCode (..))
import System.FileLock (SharedExclusive (..), lockFile, tryLockFile, unlockFile)
import System.FilePath (makeRelative, takeDirectory, takeFileName, (</>))
import qualified System.Info
import System.IO (IOMode (..), hFlush, stdout, withBinaryFile)
import System.Process (readProcessWithExitCode)
import Text.Printf (printf)
import Text.Read (readMaybe)

import Fisher.Cli.Create (buildProject, createProject)
import Fisher.Config (loadConfigAt)
import Fisher.Paths (ensureFisherHome)
import Fisher.ProjectCommand (CreateRequest (..))
import Fisher.Sha256 (sha256Hex)
import Fisher.Template (Template (..), Variable (..), findTemplate, templateRevision)
import Fisher.Template.Run (Event (..), readEvents)

-- | One scratch: the template, its variables, and what to work on (a copy
-- of a file, or a statement as @scratch_goal@; neither gives a file with
-- the sample's imports). 'srYes' trusts a custom template's steps for the
-- base's build.
data ScratchRequest = ScratchRequest
  { srTemplate :: Text
  , srVars :: [(Text, Text)]
  , srFile :: Maybe FilePath
  , srStatement :: Maybe Text
  , srYes :: Bool
  , srTrustRevision :: Maybe Text
  -- ^ @--trust-revision REV@, passed to the base's create
  }
  deriving (Eq, Show)

-- | What a scratch depends on, so tests can stand in for the create.
data ScratchDeps = ScratchDeps
  { sdCreate :: CreateRequest -> IO (Either Text FilePath)
  , sdBuild :: Bool -> FilePath -> IO (Either Text ())
  -- ^ resume an interrupted base (@--yes@, the base)
  , sdConfig :: Value
  , sdOs :: String
  -- ^ as 'System.Info.os': @darwin@, @linux@, ...
  , sdSay :: Text -> IO () }

-- | The real create, the configuration in the Fisher home, this system.
defaultScratchDeps :: IO ScratchDeps
defaultScratchDeps = do
  home <- ensureFisherHome
  (cfg, _) <- loadConfigAt home
  pure ScratchDeps
    { sdCreate = createProject
    , sdBuild = buildProject
    , sdConfig = cfg
    , sdOs = System.Info.os
    , sdSay = \l -> TIO.putStrLn l >> hFlush stdout }

-- | 'scratchWith' the real dependencies: the scratch's root and the file
-- to work on.
scratch :: ScratchRequest -> IO (Either Text (FilePath, FilePath))
scratch r = defaultScratchDeps >>= \d -> scratchWith d r

-- | @~/.cache/fisher/scratch@.
scratchDir :: IO FilePath
scratchDir = (\h -> h </> ".cache" </> "fisher" </> "scratch") <$> getHomeDirectory

basesDir :: FilePath -> FilePath
basesDir root = root </> "bases"

-- | The base's key: the first 16 hex of the sha256 of the template's name,
-- its revision and the variables (in name order; a later value for a name
-- wins).
baseKey :: Template -> Text -> [(Text, Text)] -> Text
baseKey t rev vars =
  T.take 16 . sha256Hex . TE.encodeUtf8 . T.pack $ show (tplName t, rev, Map.toAscList (Map.fromList vars))

-- | The command that clones a base to a target that does not exist yet.
-- Times are kept (@-p@): Lake compares them, and a clone with new ones
-- would rebuild.
cloneCommand :: String -> FilePath -> FilePath -> [String]
cloneCommand os base target = case os of
  "darwin" -> ["cp", "-c", "-R", "-p", base, target]
  "linux" -> ["cp", "--reflink=auto", "-R", "-p", base, target]
  _ -> ["cp", "-R", "-p", base, target]

-- | The project name a scratch's base gets unless @--var project=@ says.
defaultProject :: Text
defaultProject = "Scratch"

scratchWith :: ScratchDeps -> ScratchRequest -> IO (Either Text (FilePath, FilePath))
scratchWith d r = do
  given <- case (srFile r, srStatement r) of
    (Just _, Just _) -> pure (Left "Pass a FILE or --statement, not both.")
    (Just f, Nothing) -> do
      path <- makeAbsolute f
      ok <- doesFileExist path
      pure (if ok then Right (Just (Left path)) else Left ("No such file: " <> T.pack path))
    (Nothing, Just s) -> pure (Just . Right <$> statementProblem s)
    (Nothing, Nothing) -> pure (Right Nothing)
  case given of
    Left e -> pure (Left e)
    Right work -> findTemplate (sdConfig d) (srTemplate r) >>= \case
      Left e -> pure (Left e)
      Right t -> do
        rev <- templateRevision t
        root <- scratchDir
        createDirectoryIfMissing True (basesDir root)
        let user = srVars r
            createVars = user <> [("project", defaultProject) | "project" `notElem` map fst user]
            keyVars = [(varName v, varDefault v) | v <- tplVariables t, varName v /= "project"] <> createVars
            base = basesDir root </> T.unpack (baseKey t rev keyVars)
            lock = base <> ".lock"
            -- clone under a shared lock (clones of one base run at once);
            -- build under the exclusive one, then look again
            attempt :: Int -> IO (Either Text (FilePath, FilePath))
            attempt n = do
              cloned <- bracket (lockFile lock Shared) unlockFile $ \_ ->
                baseReady base >>= \case
                  False -> pure Nothing
                  True -> Just <$> (cloneBase d root base >>= \case
                    Left e -> pure (Left e)
                    Right target -> writeWork base target work)
              case cloned of
                Just x -> pure x
                Nothing
                  | n <= 0 -> pure (Left ("The base " <> T.pack base <> " was removed while it was in use; try again"))
                  | otherwise ->
                      bracket (lockFile lock Exclusive) unlockFile (\_ -> ensureBase d t base createVars (srYes r, srTrustRevision r)) >>= \case
                        Left e -> pure (Left e)
                        Right () -> attempt (n - 1)
        attempt 3

-- | The statement, trimmed; refused when empty or a whole declaration.
statementProblem :: Text -> Either Text Text
statementProblem s0 = case T.words s of
  [] -> Left "--statement is empty"
  (w : _) | w `elem` ["theorem", "lemma"] -> Left "give the statement only, without theorem NAME :"
  _ -> Right s
  where
    s = T.strip s0

-- | What a scratch answers when its base's build was interrupted: the base
-- is kept for the next scratch to resume.
baseInterrupted :: Text
baseInterrupted = "the base build was interrupted; run fisher scratch again to resume"

-- | The base, built if it is not ready: an interrupted one is resumed, a
-- failed or half built one removed and built again. A build that is
-- interrupted keeps the base; one that fails removes it. Called with the
-- base's exclusive lock held.
ensureBase :: ScratchDeps -> Template -> FilePath -> [(Text, Text)] -> (Bool, Maybe Text) -> IO (Either Text ())
ensureBase d t base vars (yes, trustRev) = do
  ready <- baseReady base
  if ready
    then pure (Right ())
    else do
      resumable <- baseStopped base
      -- the line the UI follows the base's events by
      sdSay d ("Building the base for " <> tplName t <> " (once) in " <> T.pack base)
      if resumable
        then do
          sdSay d "  resuming its interrupted build"
          sdBuild d yes base >>= settle
        else do
          exists <- doesPathExist base
          if exists then removeDirectoryRecursive base else pure ()
          sdCreate d CreateRequest
            { crTarget = base, crTemplate = Just (tplName t), crSource = Nothing, crVars = vars
            , crBuild = True, crYes = yes, crName = Nothing, crTrustRevision = trustRev } >>= settle . fmap (const ())
  where
    settle = \case
      Right () -> pure (Right ())
      Left e -> do
        stopped <- baseStopped base
        if stopped
          then pure (Left baseInterrupted)
          else do
            gone <- doesPathExist base
            if gone then removeDirectoryRecursive base else pure ()
            pure (Left e)

-- | Interrupted: the events file ends with @failed@ @interrupted@, and the
-- manifest (which a build resumes from) is there.
baseStopped :: FilePath -> IO Bool
baseStopped base = do
  manifest <- doesFileExist (base </> ".fisher" </> "project.yaml")
  if not manifest then pure False else (\es -> not (null es) && last es == Failed "interrupted") <$> readEvents base

-- | Built: the events file ends with @ready@.
baseReady :: FilePath -> IO Bool
baseReady base = do
  isDir <- doesDirectoryExist base
  if not isDir then pure False else (\es -> not (null es) && last es == Ready) <$> readEvents base

-- | A clone of the base under a new id; the base is touched.
cloneBase :: ScratchDeps -> FilePath -> FilePath -> IO (Either Text FilePath)
cloneBase d root base = do
  target <- freshTarget root (10 :: Int)
  let cmd = cloneCommand (sdOs d) base target
  (code, out, err) <- readProcessWithExitCode (head cmd) (tail cmd) ""
  case code of
    ExitSuccess -> do
      now <- getCurrentTime
      _ <- try (setModificationTime base now) :: IO (Either SomeException ())
      -- the clone kept the base's times; the scratch itself is new
      _ <- try (setModificationTime target now) :: IO (Either SomeException ())
      pure (Right target)
    ExitFailure n -> do
      exists <- doesPathExist target
      if exists then removeDirectoryRecursive target else pure ()
      pure (Left ("Could not clone the base (" <> T.unwords (map T.pack cmd) <> ", exit " <> T.pack (show n) <> "): " <> T.unwords (T.words (T.pack (out <> err)))))
  where
    freshTarget dir tries = do
      now <- getZonedTime
      hex <- randomHex
      let p = dir </> (formatTime defaultTimeLocale "%Y%m%d-%H%M%S" now <> "-" <> hex)
      taken <- doesPathExist p
      if taken && tries > 0 then freshTarget dir (tries - 1) else pure p

-- | Four hex digits from @/dev/urandom@.
randomHex :: IO String
randomHex = do
  bytes <- withBinaryFile "/dev/urandom" ReadMode (`BS.hGet` 2)
  pure (concatMap (\b -> let s = showHex b "" in if length s == 1 then '0' : s else s) (BS.unpack bytes))

-- | The file to work on, next to the sample: a copy of the given file, or
-- @Scratch.lean@ with the sample's imports and the statement, if any.
writeWork :: FilePath -> FilePath -> Maybe (Either FilePath Text) -> IO (Either Text (FilePath, FilePath))
writeWork base target work =
  sampleOf base >>= \case
    Left e -> pure (Left e)
    Right rel -> do
      let dir = takeDirectory (target </> rel)
      case work of
        Just (Left file) -> do
          let dest = dir </> takeFileName file
          copyFile file dest
          recordWork target dest
        _ -> do
          sample <- TE.decodeUtf8With TEE.lenientDecode <$> BS.readFile (target </> rel)
          let imports = headerImports sample
              header = T.unlines imports
              goal s = "theorem scratch_goal :\n    " <> s <> "\n    := by\n  sorry\n"
              body = case work of
                Just (Right s) -> if null imports then goal s else header <> "\n" <> goal s
                _ -> header
              dest = dir </> "Scratch.lean"
          BS.writeFile dest (TE.encodeUtf8 body)
          recordWork target dest
  where
    recordWork root dest = do
      createDirectoryIfMissing True (root </> ".fisher")
      BS.writeFile (workRecord root) (TE.encodeUtf8 (T.pack (makeRelative root dest)))
      pure (Right (root, dest))

-- | Where a scratch keeps the path of its work file (relative to its root).
workRecord :: FilePath -> FilePath
workRecord target = target </> ".fisher" </> "scratch-file"

-- | The @import@ lines of a Lean file's header: up to the first line that
-- is not an import, blank, a comment (@--@ or @/- ... -/@), @prelude@ or
-- @set_option@. Carriage returns are dropped.
headerImports :: Text -> [Text]
headerImports = go . map (T.strip . T.filter (/= '\r')) . T.lines
  where
    go [] = []
    go (l : ls)
      | T.null l || "--" `T.isPrefixOf` l || l == "prelude" || "set_option " `T.isPrefixOf` l = go ls
      | "/-" `T.isPrefixOf` l = go (skipBlock (T.drop 2 l : ls))
      | "import " `T.isPrefixOf` l = l : go ls
      | otherwise = []
    -- past the end of a block comment; the rest of its last line goes on
    skipBlock [] = []
    skipBlock (l : ls) = case T.breakOn "-/" l of
      (_, rest) | not (T.null rest) -> let after = T.strip (T.drop 2 rest) in if T.null after then ls else after : ls
      _ -> skipBlock ls

-- | The sample's path in the base, as its manifest records it.
sampleOf :: FilePath -> IO (Either Text FilePath)
sampleOf base = do
  let mpath = base </> ".fisher" </> "project.yaml"
  Y.decodeFileEither mpath >>= \case
    Right (Object o)
      | Just (Object t) <- KM.lookup "template" o
      , Just (String s) <- KM.lookup "sample" t -> pure (Right (T.unpack s))
    Right _ -> pure (Left ("No template sample recorded in " <> T.pack mpath))
    Left e -> pure (Left ("cannot read " <> T.pack mpath <> ": " <> T.unwords (T.words (T.pack (Y.prettyPrintParseException e)))))

-- | The scratches, then the bases: path, age in days and size in bytes. A
-- scratch's age counts from the newest change to its root, its top-level
-- entries or its work file; a base's from its last clone.
listScratch :: IO [(FilePath, Double, Integer)]
listScratch = do
  root <- scratchDir
  scratches <- dirsIn root
  bases <- dirsIn (basesDir root)
  let ss = filter ((/= "bases") . takeFileName) scratches
  xs <- forM ss $ \p -> (p,,) <$> scratchAge p <*> sizeOf p
  ys <- forM bases $ \p -> (p,,) <$> ageDays p <*> sizeOf p
  pure (xs <> ys)

-- | For a base, the variables it was built with (the releases @latest@
-- resolved to), as @name=value@ but for @project@, from its manifest;
-- empty for a scratch.
baseRelease :: FilePath -> IO Text
baseRelease p
  | takeFileName (takeDirectory p) /= "bases" = pure ""
  | otherwise =
      (Y.decodeFileEither (p </> ".fisher" </> "project.yaml") :: IO (Either Y.ParseException Value)) >>= \case
        Right (Object o)
          | Just (Object t) <- KM.lookup "template" o
          , Just (Object vs) <- KM.lookup "variables" t ->
              pure (T.unwords [K.toText k <> "=" <> v | (k, String v) <- KM.toList vs, k /= "project"])
        _ -> pure ""

-- | Remove the scratches older than the days and, with the flag, the bases
-- older than that too. A base whose lock is held (being built or cloned) is
-- kept. Returns what was removed.
pruneScratch :: Double -> Bool -> IO [FilePath]
pruneScratch days withBases = do
  root <- scratchDir
  scratches <- filter ((/= "bases") . takeFileName) <$> dirsIn root
  oldScratches <- filterM (fmap (> days) . scratchAge) scratches
  mapM_ removeDirectoryRecursive oldScratches
  oldBases <-
    if not withBases
      then pure []
      else do
        bases <- dirsIn (basesDir root)
        old <- filterM (fmap (> days) . ageDays) bases
        fmap concat . forM old $ \b ->
          tryLockFile (b <> ".lock") Exclusive >>= \case
            Nothing -> pure []
            Just l -> do
              removeDirectoryRecursive b
              unlockFile l
              pure [b]
  pure (sort oldScratches <> sort oldBases)

dirsIn :: FilePath -> IO [FilePath]
dirsIn dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure []
    else do
      names <- sort <$> listDirectory dir
      filterM (\p -> (&&) <$> doesDirectoryExist p <*> (not <$> pathIsSymbolicLink p)) (map (dir </>) names)

ageDays :: FilePath -> IO Double
ageDays p = getModificationTime p >>= daysSince

daysSince :: UTCTime -> IO Double
daysSince m = (\now -> realToFrac (diffUTCTime now m) / 86400) <$> getCurrentTime

-- | Days since the newest of: the scratch's root, its top-level entries,
-- its work file.
scratchAge :: FilePath -> IO Double
scratchAge root = do
  entries <- map (root </>) <$> listDirectory root
  work <- try (BS.readFile (workRecord root)) >>= \case
    Right bytes -> pure [root </> T.unpack (T.strip (TE.decodeUtf8With TEE.lenientDecode bytes))]
    Left (_ :: SomeException) -> pure []
  times <- fmap concat . forM (root : entries <> work) $ \p ->
    either (const []) pure <$> (try (getModificationTime p) :: IO (Either SomeException UTCTime))
  if null times then ageDays root else daysSince (maximum times)

-- | Bytes under a directory, not following links.
sizeOf :: FilePath -> IO Integer
sizeOf p = do
  link <- pathIsSymbolicLink p
  isDir <- doesDirectoryExist p
  if
    | link -> pure 0
    | isDir -> listDirectory p >>= fmap sum . mapM (sizeOf . (p </>))
    | otherwise -> either (const 0) id <$> (try (getFileSize p) :: IO (Either SomeException Integer))

scratchUsage :: [Text]
scratchUsage =
  [ "Usage: fisher scratch [--template NAME] [--var NAME=VALUE]... [--yes] [--trust-revision REV] [FILE | --statement TEXT]"
  , "       fisher scratch --list"
  , "       fisher scratch --prune DAYS [--bases]"
  , "  (the template defaults to lean-mathlib; the first scratch for a template builds its base once)" ]

-- | @fisher scratch ARGS@ with the real dependencies; the exit code.
scratchMain :: [String] -> IO Int
scratchMain args = defaultScratchDeps >>= \d -> scratchMainWith d (sdSay d) args

data Mode = Make | List | Prune Double

data Opts = Opts
  { oMode :: Mode, oBases :: Bool, oTemplate :: Maybe Text, oVars :: [(Text, Text)]
  , oYes :: Bool, oStatement :: Maybe Text, oFile :: Maybe FilePath, oTrust :: Maybe Text }

-- | Parse the arguments and run: 0 done, 1 failed, 2 a wrong use, 130
-- the base's build interrupted.
scratchMainWith :: ScratchDeps -> (Text -> IO ()) -> [String] -> IO Int
scratchMainWith d say args = case parse (Opts Make False Nothing [] False Nothing Nothing Nothing) args of
  Left e -> mapM_ say (("(>_<) " <> e) : scratchUsage) >> pure 2
  Right o -> case oMode o of
    List -> do
      xs <- listScratch
      if null xs
        then scratchDir >>= \root -> say ("No scratch environments under " <> T.pack root)
        else forM_ xs $ \(p, age, bytes) -> do
          release <- baseRelease p
          let line = T.pack (printf "%-60s %7.1f days %10.1f MB" p age (fromIntegral bytes / 1048576 :: Double))
          say (if T.null release then line else line <> "  " <> release)
      pure 0
    Prune days -> do
      removed <- pruneScratch days (oBases o)
      if null removed then say "Nothing to prune" else mapM_ (\p -> say ("Removed " <> T.pack p)) removed
      pure 0
    Make ->
      scratchWith d ScratchRequest
        { srTemplate = maybe "lean-mathlib" id (oTemplate o), srVars = oVars o, srFile = oFile o
        , srStatement = oStatement o, srYes = oYes o, srTrustRevision = oTrust o } >>= \case
        Left e -> say ("(>_<) " <> e) >> pure (if e == baseInterrupted then 130 else 1)
        Right (root, file) -> do
          say ("Scratch: " <> T.pack root)
          say ("File: " <> T.pack file)
          pure 0
  where
    parse o = \case
      [] -> check o
      ("--list" : rest) -> parse o {oMode = List} rest
      ("--prune" : n : rest) | Just x <- readMaybe n, x >= (0 :: Double) -> parse o {oMode = Prune x} rest
      ["--prune"] -> Left "`--prune` takes a number of days"
      ("--prune" : n : _) -> Left ("`--prune` takes a number of days; got `" <> T.pack n <> "`")
      ("--bases" : rest) -> parse o {oBases = True} rest
      ("--yes" : rest) -> parse o {oYes = True} rest
      ("--template" : n : rest) -> parse o {oTemplate = Just (T.pack n)} rest
      ("--statement" : s : rest) -> parse o {oStatement = Just (T.pack s)} rest
      ("--trust-revision" : v : rest) -> parse o {oTrust = Just (T.pack v)} rest
      ("--var" : kv : rest) -> case T.breakOn "=" (T.pack kv) of
        (k, v) | not (T.null k), not (T.null v) -> parse o {oVars = oVars o <> [(k, T.drop 1 v)]} rest
        _ -> Left ("`--var` takes NAME=VALUE; got `" <> T.pack kv <> "`")
      [flag] | flag `elem` ["--template", "--statement", "--var", "--trust-revision"] -> Left ("`" <> T.pack flag <> "` takes a value")
      (a : rest)
        | take 1 a == "-" -> Left ("Unknown option `" <> T.pack a <> "`")
        | Nothing <- oFile o -> parse o {oFile = Just a} rest
        | otherwise -> Left "Pass one FILE"
    check o = case oMode o of
      Make
        | oBases o -> Left "`--bases` goes with `--prune`"
        | Just _ <- oFile o, Just _ <- oStatement o -> Left "Pass a FILE or --statement, not both"
        | otherwise -> Right o
      _
        | not (null (oVars o)) || oYes o || oTrust o /= Nothing || oFile o /= Nothing || oStatement o /= Nothing || oTemplate o /= Nothing ->
            Left "`--list` and `--prune` take no other options"
        | List <- oMode o, oBases o -> Left "`--bases` goes with `--prune`"
        | otherwise -> Right o
