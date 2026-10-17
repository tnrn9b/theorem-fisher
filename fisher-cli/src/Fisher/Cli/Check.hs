-- | @fisher check [--prover K] [--isabelle PATH] [FILE[:decl]...]@: decide,
-- with no model involved, whether the targets of a Rocq or Isabelle project
-- are proved (spec section 4.4). One line per target, @ok <target>@ or
-- @FAIL <target>: <problems>@; exit 1 when any fails, 2 when the check
-- cannot run.
--
-- Rocq: in a copy of the project's sources in a temporary directory,
-- compile the targets' files and what they depend on (@rocq dep@ order)
-- with the project's flags, using @rocq compile@ when present, else @coqc@
-- (@dune build@, in dune's own @_build@, for a dune project); then, per
-- target, compile a
-- generated file that requires the module and runs @Print Assumptions@.
-- One generated file per target, because an error (a missing name) stops
-- the compile at the first one.
--
-- Isabelle: per session, @isabelle build -c -o quick_and_dirty=false@ (the
-- @-c@ keeps a heap built with quick_and_dirty from passing), then a
-- generated session @Fisher_Check@ on top of it whose single ML command
-- looks up every target and reports each one's oracles; it fails, listing
-- every result, when any target is missing or has an oracle. The check
-- session builds with @quick_and_dirty=false@ when the strict build passed
-- (reusing its heap) and with @quick_and_dirty=true@ only when it failed.
--
-- Without targets: every theorem the file outline
-- ('Fisher.Prover.Holes.declarations') finds in the project's source files.
-- Generated files go to a temporary directory, never into the project.
module Fisher.Cli.Check
  ( checkMain
  , CheckOptions (..)
  , defaultCheckOptions
  , parseCheckArgs
  , runCheck
  , checkUsage
  ) where

import Control.Exception (SomeException, bracket, try)
import Control.Monad (filterM, forM, forM_, void, when)
import Data.Char (isAlphaNum)
import Data.List (nub, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath
import System.IO (stderr)
import System.Posix.Process (getProcessID)
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)

import Fisher.Project (Project (..), discoverProject)
import Fisher.Prover (ProverKind (..), Prover (..), binaryFix, detectProver, findIsabelle, findProverRoot, prover, proverByName)
import Fisher.Prover.Check
import Fisher.Prover.Holes (declarations)
import Fisher.Workflow.Provers (stageCoqcWrapper, stageCoqdepWrapper)

data CheckOptions = CheckOptions
  { coProver :: Maybe ProverKind
  , coIsabelle :: Maybe FilePath
  , coTargets :: [String]
  }
  deriving (Eq, Show)

defaultCheckOptions :: CheckOptions
defaultCheckOptions = CheckOptions Nothing Nothing []

checkUsage :: Text
checkUsage =
  T.unlines
    [ "usage: fisher check [--prover rocq|isabelle] [--isabelle PATH] [FILE[:decl]...]"
    , ""
    , "Compiles the targets and audits their axioms (Rocq) or oracles (Isabelle)."
    , "Prints `ok <target>` or `FAIL <target>: <problems>` per target and exits 1"
    , "when any fails. Without targets, checks every theorem in the project."
    ]

-- | @Left@ an error, @Right Nothing@ for help.
parseCheckArgs :: [String] -> Either Text (Maybe CheckOptions)
parseCheckArgs = go defaultCheckOptions
  where
    go o = \case
      [] -> Right (Just o {coTargets = reverse (coTargets o)})
      (h : _) | h `elem` ["-h", "--help"] -> Right Nothing
      ("--prover" : k : r) -> case proverByName (T.pack k) of
        Just kind -> go o {coProver = Just kind} r
        Nothing -> Left ("unknown prover " <> T.pack k <> "; use lean4, rocq or isabelle")
      ("--isabelle" : p : r) -> go o {coIsabelle = Just p} r
      (f : _) | f `elem` ["--prover", "--isabelle"] -> Left (T.pack f <> " needs a value")
      ("--" : r) -> Right (Just o {coTargets = reverse (coTargets o) ++ r})
      (f@('-' : _) : _) -> Left ("unknown option " <> T.pack f)
      (t : r) -> go o {coTargets = t : coTargets o} r

checkMain :: [String] -> IO ExitCode
checkMain args = case parseCheckArgs args of
  Left e -> TIO.hPutStrLn stderr e >> TIO.hPutStr stderr checkUsage >> pure (ExitFailure 2)
  Right Nothing -> TIO.putStr checkUsage >> pure ExitSuccess
  Right (Just o) -> do
    cwd <- getCurrentDirectory
    runCheck o cwd >>= \case
      Left e -> TIO.hPutStrLn stderr e >> pure (ExitFailure 2)
      Right [] -> TIO.hPutStrLn stderr "fisher check: no theorems to check" >> pure ExitSuccess
      Right rs -> do
        mapM_ (TIO.putStrLn . resultLine) rs
        pure (if all crOk rs then ExitSuccess else ExitFailure 1)

-- | A target after expansion: its label, its file (absolute, and relative
-- to the project root) and the declaration.
data Item = Item {iLabel :: Text, iAbs :: FilePath, iRel :: FilePath, iDecl :: Text}

-- | Check the targets of the project at or above @cwd@ (or above the first
-- target). @Left@ when the check cannot run at all.
-- @--isabelle@ is looked at on the Isabelle path only.
runCheck :: CheckOptions -> FilePath -> IO (Either Text [CheckResult])
runCheck o cwd = do
  start <- case coTargets o of
    (t : _) -> pure (takeDirectory (cwd </> tFile (parseTarget t)))
    [] -> pure cwd
  locate o start >>= \case
    Left e -> pure (Left e)
    Right (Lean4, _) -> pure (Left "fisher check is for Rocq and Isabelle projects; a Lean project is checked by `lake build`.")
    Right (kind, root) -> do
      (bad, items) <- expand kind root cwd (coTargets o)
      if null items
        then pure (Right bad)
        else fmap (bad ++) <$> case kind of
          Rocq -> checkRocq root items
          _ -> checkIsabelle o root items

-- | The prover and project root.
locate :: CheckOptions -> FilePath -> IO (Either Text (ProverKind, FilePath))
locate o start = case coProver o of
  Just k ->
    findProverRoot k start >>= \case
      Just r -> pure (Right (k, r))
      Nothing -> pure (Left ("no " <> pvName (prover k) <> " project root at or above " <> T.pack start))
  Nothing ->
    try @SomeException (discoverProject start) >>= \case
      Right p -> pure (Right (projectProver p, projectProverRoot p))
      Left _ ->
        detectProver start >>= \case
          Left e -> pure (Left e)
          Right (Just kr) -> pure (Right kr)
          Right Nothing -> pure (Left ("no Rocq or Isabelle project at or above " <> T.pack start <> "; pass --prover rocq|isabelle in a project directory."))

-- | The targets as items, with the results already decided (a file that
-- cannot be read, a name the generated files cannot carry).
expand :: ProverKind -> FilePath -> FilePath -> [String] -> IO ([CheckResult], [Item])
expand kind root cwd = \case
  [] -> do
    files <- sourceFiles kind root
    fmap mconcat . forM files $ \rel -> fileItems False (T.pack rel) (root </> rel) rel
  ts -> fmap mconcat . forM ts $ \t -> do
    let Target f md = parseTarget t
    absf <- canonical (cwd </> f)
    let rel = makeRelative root absf
    case md of
      Just d
        | validName d -> pure ([], [Item (T.pack t) absf rel d])
        | otherwise -> pure ([CheckResult (T.pack t) False ["not a declaration name"]], [])
      Nothing -> fileItems True (T.pack t) absf rel
  where
    -- a file named on the command line must declare something to check
    fileItems named label absf rel =
      try @SomeException (TIO.readFile absf) >>= \case
        Left _ -> pure ([CheckResult label False ["cannot read " <> T.pack absf]], [])
        Right src -> case [Item (label <> ":" <> d) absf rel d | (_, d) <- declarations kind src, validName d] of
          [] | named -> pure ([CheckResult label False ["no theorems found"]], [])
          its -> pure ([], its)
    canonical p = either (const p) id <$> try @SomeException (canonicalizePath p)

validName :: Text -> Bool
validName d = not (T.null d) && T.all (\c -> isAlphaNum c || c `elem` ("_'." :: String)) d

-- | The project's source files, relative to the root: a Rocq project
-- file's list when it has one, else every file of the prover's extension
-- outside hidden and build directories.
sourceFiles :: ProverKind -> FilePath -> IO [FilePath]
sourceFiles kind root = do
  listed <- case kind of
    Rocq -> maybe (pure []) (fmap rocqProjectFiles . TIO.readFile) =<< projectFile root
    _ -> pure []
  if not (null listed)
    then pure (map normalise listed)
    else walkSources kind root

-- | Every file of the prover's extension under the root, outside hidden
-- and build (@_@-prefixed) directories.
walkSources :: ProverKind -> FilePath -> IO [FilePath]
walkSources kind root = sort <$> walk ""
  where
    exts = pvSourceExt (prover kind)
    walk rel = do
      names <- listDirectory (root </> rel)
      fmap concat . forM (sort names) $ \n -> do
        let r = if null rel then n else rel </> n
        isDir <- doesDirectoryExist (root </> r)
        if isDir
          then if take 1 n `elem` [".", "_"] then pure [] else walk r
          else pure [r | takeExtension n `elem` exts]

projectFile :: FilePath -> IO (Maybe FilePath)
projectFile root = do
  fs <- filterM doesFileExist [root </> "_RocqProject", root </> "_CoqProject"]
  pure (case fs of (f : _) -> Just f; [] -> Nothing)

run :: FilePath -> FilePath -> [String] -> IO (ExitCode, Text)
run = runWith Nothing

-- | 'run' in the environment given, else this process's.
runWith :: Maybe [(String, String)] -> FilePath -> FilePath -> [String] -> IO (ExitCode, Text)
runWith environment dir bin args = do
  r <- try @SomeException (readCreateProcessWithExitCode (proc bin args) {cwd = Just dir, env = environment} "")
  pure $ case r of
    Right (code, out, err) -> (code, T.pack out <> T.pack err)
    Left e -> (ExitFailure 127, T.pack (show e))

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir act = do
  base <- getTemporaryDirectory
  pid <- getProcessID
  let pick n = do
        let d = base </> ("fisher-check-" <> show pid <> "-" <> show (n :: Int))
        exists <- doesPathExist d
        if exists then pick (n + 1) else createDirectory d >> pure d
  bracket (pick 0) (\d -> try @SomeException (removeDirectoryRecursive d)) act

failWith :: Text -> Text
failWith out = fromMaybe (T.strip (T.unlines (reverse (take 3 (reverse (T.lines out)))))) (rocqError out)

-- Rocq ----------------------------------------------------------------------

data RocqBin = RocqBin {rbCompile :: (FilePath, [String]), rbDep :: Maybe (FilePath, [String])}

rocqBin :: IO (Maybe RocqBin)
rocqBin =
  findExecutable "rocq" >>= \case
    Just r -> pure (Just (RocqBin (r, ["compile"]) (Just (r, ["dep"]))))
    Nothing ->
      findExecutable "coqc" >>= \case
        Nothing -> pure Nothing
        Just c -> Just . RocqBin (c, []) . fmap (,[]) <$> findExecutable "coqdep"

checkRocq :: FilePath -> [Item] -> IO (Either Text [CheckResult])
checkRocq root items =
  rocqBin >>= \case
    Nothing -> pure (Left ("neither rocq nor coqc is on PATH; " <> binaryFix Rocq "coqc"))
    Just rb -> do
      pf <- projectFile root
      isDune <- doesFileExist (root </> "dune-project")
      if isJust pf || not isDune
        then withTempDir $ \tmp -> do
          -- compile in a copy, so that no .vo, .vos, .vok or .glob lands
          -- in the project (or replaces the user's own)
          let work = tmp </> "project"
              chk = tmp </> "check"
          flags <- relocateFlags root <$> maybe (pure []) (fmap rocqFlags . TIO.readFile) pf
          copyRocqProject root work pf (map iRel items)
          createDirectory chk
          failed <- compileRocq rb work flags (nub (map iRel items))
          Right <$> mapM (auditRocq rb work flags flags failed chk) items
        else do
          (srcFlags, buildFlags) <- duneFlags root
          (code, out) <- withTempDir $ \bin -> do
            env <- duneEnvironment bin
            runWith env root "dune" ["build"]
          let failed =
                if code == ExitSuccess then Map.empty
                else Map.fromList [(iRel i, "dune build failed: " <> failWith out) | i <- items]
          withTempDir $ \tmp -> Right <$> mapM (auditRocq rb root srcFlags buildFlags failed tmp) items

-- | Copy the project file and the Rocq sources (those found, those it
-- lists and the targets' files) into @work@, keeping their places.
copyRocqProject :: FilePath -> FilePath -> Maybe FilePath -> [FilePath] -> IO ()
copyRocqProject root work pf targets = do
  createDirectoryIfMissing True work
  mapM_ (\f -> copyFile f (work </> takeFileName f)) pf
  found <- walkSources Rocq root
  listed <- sourceFiles Rocq root
  forM_ (nub (found ++ listed ++ targets)) $ \rel -> do
    let src = root </> rel
    yes <- doesFileExist src
    when (yes && take 1 (splitDirectories rel) /= [".."] && not (isAbsolute rel)) $ do
      createDirectoryIfMissing True (takeDirectory (work </> rel))
      copyFile src (work </> rel)

-- | Compile the files and their dependencies in order; the problem of each
-- file that did not compile.
compileRocq :: RocqBin -> FilePath -> [Text] -> [FilePath] -> IO (Map.Map FilePath Text)
compileRocq rb root flags targets = do
  files <- sourceFiles Rocq root
  let fl = map T.unpack flags
  deps <- case rbDep rb of
    Nothing -> pure []
    Just (b, a) -> do
      (code, out) <- run root b (a ++ fl ++ nub (files ++ targets))
      pure (if code == ExitSuccess then map norm (parseRocqDeps out) else [])
  let order = compileOrder deps targets
      depsOf f = fromMaybe [] (lookup f deps)
      step failed f = case [d | d <- depsOf f, d `Map.member` failed] of
        (d : _) -> pure (Map.insert f ("not compiled: " <> T.pack d <> " failed to compile") failed)
        [] -> do
          let (b, a) = rbCompile rb
          (code, out) <- run root b (a ++ fl ++ [f])
          pure (if code == ExitSuccess then failed else Map.insert f ("does not compile: " <> failWith out) failed)
  foldlM' step Map.empty order
  where
    norm (f, ds) = (normalise f, map normalise ds)
    foldlM' f z xs = foldl (\acc x -> acc >>= \a -> f a x) (pure z) xs

-- | The environment @dune build@ runs in. dune's coq mode runs @coqc@ and
-- @coqdep@ from PATH; with @rocq@ there and no @coqc@ (Rocq 9 without the
-- compatibility binaries), the launch's coqc wrapper and a coqdep one
-- (for whichever is missing) are staged in @bin@, first on PATH.
-- 'Nothing' (this process's environment) otherwise.
duneEnvironment :: FilePath -> IO (Maybe [(String, String)])
duneEnvironment bin =
  findExecutable "coqc" >>= \case
    Just _ -> pure Nothing
    Nothing ->
      findExecutable "rocq" >>= \case
        Nothing -> pure Nothing
        Just rocq -> do
          _ <- stageCoqcWrapper bin rocq
          findExecutable "coqdep" >>= maybe (void (stageCoqdepWrapper bin rocq)) (const (pure ()))
          environment <- getEnvironment
          let old = fromMaybe "" (lookup "PATH" environment)
              path = if null old then bin else bin <> [searchPathSeparator] <> old
          pure (Just (("PATH", path) : filter ((/= "PATH") . fst) environment))

-- | A dune project's @-Q@ mappings: the source directories (for module
-- names) and the build directories (for loading the compiled modules).
duneFlags :: FilePath -> IO ([Text], [Text])
duneFlags root = do
  dirs <- walkDirs ""
  found <- fmap concat . forM dirs $ \d -> do
    let f = root </> d </> "dune"
    yes <- doesFileExist f
    if not yes then pure [] else maybe [] (\n -> [(d, n)]) . duneTheoryName <$> TIO.readFile f
  pure
    ( concat [["-Q", T.pack (dot d), n] | (d, n) <- found]
    , concat [["-Q", T.pack ("_build" </> "default" </> d), n] | (d, n) <- found] )
  where
    dot d = if null d then "." else d
    walkDirs rel = do
      names <- listDirectory (root </> rel)
      subs <- fmap concat . forM (sort names) $ \n -> do
        let r = if null rel then n else rel </> n
        isDir <- doesDirectoryExist (root </> r)
        if isDir && take 1 n `notElem` [".", "_"] then walkDirs r else pure []
      pure (rel : subs)

auditRocq :: RocqBin -> FilePath -> [Text] -> [Text] -> Map.Map FilePath Text -> FilePath -> Item -> IO CheckResult
auditRocq rb root srcFlags loadFlags failed tmp i = case Map.lookup (iRel i) failed of
  Just p -> pure (CheckResult (iLabel i) False [p])
  Nothing -> do
    let (m, covered) = rocqModule srcFlags (iRel i)
        file = tmp </> "Fisher_Check.v"
        extra = if covered then [] else ["-Q", root </> takeDirectory (iRel i), ""]
        (b, a) = rbCompile rb
    TIO.writeFile file (rocqCheckFile m (iDecl i))
    (_, out) <- run root b (a ++ map T.unpack loadFlags ++ extra ++ [file])
    let ps = rocqProblems (m <> "." <> iDecl i) (iDecl i) (parseAssumptions out)
    pure (CheckResult (iLabel i) (null ps) ps)

-- Isabelle ------------------------------------------------------------------

-- | @--isabelle@ (a path, or a command name looked up on PATH), else
-- 'findIsabelle'.
isabelleBin :: CheckOptions -> IO (Either Text FilePath)
isabelleBin o = case coIsabelle o of
  Just p
    | '/' `notElem` p ->
        maybe (Left ("--isabelle " <> T.pack p <> ": not found on PATH")) Right <$> findExecutable p
    | otherwise -> do
        yes <- doesFileExist p
        pure (if yes then Right p else Left ("--isabelle " <> T.pack p <> ": no such file"))
  Nothing -> do
    -- where the launch and doctor look: PATH, ISABELLE_HOME, the app
    env <- getEnvironment
    maybe (Left notFound) (Right . fst) <$> findIsabelle env
  where
    notFound =
      "isabelle not found on PATH, under ISABELLE_HOME, in Applications or by --isabelle PATH; "
        <> binaryFix Isabelle "isabelle" <> "."

-- | Every session of the project's ROOT and the ROOTs its ROOTS lists, with
-- directories relative to the project root.
rootSessions :: FilePath -> IO [RootSession]
rootSessions root = do
  here <- readRoot ""
  roots <- readMaybe (root </> "ROOTS")
  let dirs = [T.unpack (T.strip l) | l <- maybe [] T.lines roots, not (T.null (T.strip l)), not ("#" `T.isPrefixOf` T.strip l)]
  others <- concat <$> mapM readRoot dirs
  pure (here ++ others)
  where
    readRoot d = do
      t <- readMaybe (root </> d </> "ROOT")
      pure [s {rsDir = normalise (d </> rsDir s)} | s <- maybe [] parseRootSessions t]
    readMaybe f = either (const Nothing) Just <$> try @SomeException (TIO.readFile f)

checkIsabelle :: CheckOptions -> FilePath -> [Item] -> IO (Either Text [CheckResult])
checkIsabelle o root items =
  isabelleBin o >>= \case
    Left e -> pure (Left e)
    Right isa -> do
      sessions <- rootSessions root
      let placed = [(i, sessionFor sessions (iRel i)) | i <- items]
          bySession = Map.fromListWith (flip (++)) [(s, [(i, th)]) | (i, Just (s, th)) <- placed]
          homeless = [CheckResult (iLabel i) False [T.pack (iRel i) <> " is in no session of ROOT"] | (i, Nothing) <- placed]
      checked <- forM (Map.toList bySession) $ \(s, its) -> sessionResults isa root s its
      let byLabel = Map.fromList [(crTarget r, r) | r <- concat checked ++ homeless]
      pure (Right [r | i <- items, Just r <- [Map.lookup (iLabel i) byLabel]])

sessionResults :: FilePath -> FilePath -> Text -> [(Item, Text)] -> IO [CheckResult]
sessionResults isa root session its = do
  home <- getHomeDirectory
  (code, out) <- run root isa (isabelleStrictArgs root session)
  let strict = case parseIsabelleBuild out of
        Right () | code /= ExitSuccess -> Left ("isabelle build exited " <> T.pack (show code))
        r -> r
      sorries = [(expandHome home f, n) | (f, n) <- isabelleSorries out]
  outlines <- forM its $ \(i, _) -> either (const []) (declarations Isabelle) <$> try @SomeException (TIO.readFile (iAbs i))
  checkOut <- withTempDir $ \tmp -> do
    TIO.writeFile (tmp </> "ROOT") (isabelleCheckRoot session)
    let thys = nub [session <> "." <> th | (_, th) <- its]
    TIO.writeFile (tmp </> (T.unpack checkSession <> ".thy")) (isabelleCheckTheory thys [(th, iDecl i) | (i, th) <- its])
    run tmp isa (isabelleCheckArgs (either (const False) (const True) strict) root tmp)
  let labels = isabelleLabels [(th, iDecl i) | (i, th) <- its]
      results = Map.fromList (oracleResults (snd checkOut))
      checkFailed = fst checkOut /= ExitSuccess
      fallback = either id (const "check build failed") (parseIsabelleBuild (snd checkOut))
  pure
    [ CheckResult (iLabel i) (null ps) ps
    | ((i, _), label, outline) <- zip3 its labels outlines
    , let own = ["sorry at line " <> T.pack (show n) | (f, n) <- sorries, sameFile f (iAbs i), declAt outline n == Just (iDecl i)]
          strictP = case strict of
            Left e | null sorries -> ["strict build: " <> e]
            _ -> []
          oracleP
            | not checkFailed = []
            | Just r <- Map.lookup label results = isabelleProblems r
            | otherwise = ["check failed: " <> fallback]
          ps = nub (own ++ strictP ++ oracleP)
    ]
  where
    expandHome home f = case f of
      ('~' : '/' : r) -> home </> r
      _ -> f
    -- the build prints paths with ~ and may resolve links differently
    sameFile a b = equalFilePath (normalise a) (normalise b) || takeFileName a == takeFileName b
