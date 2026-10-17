-- | A Gauss project: a Lean 4, Rocq or Isabelle repository with a
-- @.gauss/project.yaml@ manifest. Discovery, loading, initialization and
-- template population, ported from @gauss_cli/project.py@ at the commit in
-- @UPSTREAM@. The error messages for Lean projects are upstream's, word for
-- word; Rocq and Isabelle (kinds @rocq@ and @isabelle@, with the manifest
-- key @root@) are Fisher's own.
module Fisher.Project
  ( Project (..)
  , ProjectError (..)
  , InitOptions (..)
  , defaultInitOptions
  , projectErrorMessage
  , isCommandError
  , projectLabel
  , isBlueprint
  , schemaVersion
  , templateEnvVar
  , blueprintMarkerFiles
  , isLeanProjectRoot
  , findLeanProjectRoot
  , detectBlueprintMarkers
  , resolveTemplateSource
  , loadProject
  , discoverProject
  , initializeProject
  , populateFromTemplate
  , formatProjectSummary
  , renderManifest
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad (filterM, forM_, unless, when)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.List (elemIndex, isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (formatTime, defaultTimeLocale, getCurrentTime)
import qualified Data.Yaml as Y
import qualified Data.Yaml.Pretty as YP
import System.Directory
  ( canonicalizePath, copyFileWithMetadata, createDirectoryIfMissing
  , doesDirectoryExist, doesFileExist, doesPathExist, listDirectory )
import System.Exit (ExitCode (..))
import System.FilePath
  ( isAbsolute, makeRelative, splitDirectories, takeDirectory, takeFileName, (</>) )
import System.Process.Typed (proc, readProcess)

import Fisher.Config (getPath)
import Fisher.Paths (expandUser)
import Fisher.Prover (Marker (..), Prover (..), ProverKind (..), detectProver, findProverRoot, isProverRoot, markerKinds, prover, proverByName, proverTitle, tieMessage, withArticle)
import Fisher.Py (lookupKey, pyIter, pyRepr, pyStr, pyStrOr, pyTruthy)

-- | Upstream's @GaussProject@. Two projects are equal when every field,
-- the raw manifest included, is equal, as with the frozen dataclass.
data Project = Project
  { projectRoot :: FilePath
  , projectGaussDir :: FilePath
  , projectManifestPath :: FilePath
  , projectName :: Text
  , projectKind :: Text
  , projectSchemaVersion :: Int
  , projectLeanRoot :: FilePath
  -- ^ The prover root, whatever the kind; kept for the Lean-only callers.
  , projectProver :: ProverKind
  , projectProverRoot :: FilePath
  -- ^ The prover project root (@lean_root@ or @root@ in the manifest).
  , projectRuntimeDir :: FilePath
  , projectCacheDir :: FilePath
  , projectWorkflowsDir :: FilePath
  , projectCreatedAt :: Text
  , projectSourceMode :: Text
  , projectTemplateSource :: Text
  , projectBlueprintMarkers :: [Text]
  , projectManifest :: Value
  }
  deriving (Eq, Show)

-- | Upstream's four exception classes. 'ProjectTemplateUnavailable' is a
-- kind of command error there; 'isCommandError' says so here.
data ProjectError
  = ProjectNotFound Text
  | ProjectManifestInvalid Text
  | ProjectCommandFailed Text
  | ProjectTemplateUnavailable Text
  deriving (Eq, Show)

instance Exception ProjectError

projectErrorMessage :: ProjectError -> Text
projectErrorMessage = \case
  ProjectNotFound m -> m
  ProjectManifestInvalid m -> m
  ProjectCommandFailed m -> m
  ProjectTemplateUnavailable m -> m

isCommandError :: ProjectError -> Bool
isCommandError = \case
  ProjectCommandFailed _ -> True
  ProjectTemplateUnavailable _ -> True
  _ -> False

gaussDirName, manifestFileName :: FilePath
gaussDirName = ".gauss"
manifestFileName = "project.yaml"

schemaVersion :: Int
schemaVersion = 1

templateEnvVar :: String
templateEnvVar = "GAUSS_BLUEPRINT_TEMPLATE_SOURCE"

blueprintMarkerFiles :: [FilePath]
blueprintMarkerFiles =
  [ "lean-toolchain"
  , "lakefile.lean"
  , "lakefile.toml"
  , "templates/blueprint.yml"
  , ".github/workflows/gauss.yml"
  ]

-- | @self.name or self.root.name@.
projectLabel :: Project -> Text
projectLabel p
  | T.null (projectName p) = T.pack (takeFileName (projectRoot p))
  | otherwise = projectName p

isBlueprint :: Project -> Bool
isBlueprint = not . null . projectBlueprintMarkers

tshow :: FilePath -> Text
tshow = T.pack

resolve :: FilePath -> IO FilePath
resolve p = expandUser p >>= canonicalizePath

-- | The path and each of its parents, nearest first, as @(p, *p.parents)@.
selfAndParents :: FilePath -> [FilePath]
selfAndParents p =
  let up = takeDirectory p
   in if up == p then [p] else p : selfAndParents up

-- | @Path.relative_to@ as a test: is @child@ the root or under it.
within :: FilePath -> FilePath -> Bool
within root child = splitDirectories root `isPrefixOf` splitDirectories child

isLeanProjectRoot :: FilePath -> IO Bool
isLeanProjectRoot = isProverRoot Lean4

findLeanProjectRoot :: FilePath -> IO (Maybe FilePath)
findLeanProjectRoot = findProverRoot Lean4

detectBlueprintMarkers :: FilePath -> IO [Text]
detectBlueprintMarkers root =
  map T.pack <$> filterM (doesPathExist . (root </>)) blueprintMarkerFiles

-- | The environment variable when set and non-blank, else
-- @gauss.project.template_source@ from the configuration, else empty.
resolveTemplateSource :: Value -> [(String, String)] -> Text
resolveTemplateSource config env =
  let fromEnv = T.strip (T.pack (fromMaybe "" (lookup templateEnvVar env)))
   in if not (T.null fromEnv)
        then fromEnv
        else case getPath ["gauss", "project"] config of
          Just pc@(Object _) -> pyStrOr (lookupKey "template_source" pc)
          _ -> ""

-- | @payload.get(key, default)@: a key that is present, even as null, is
-- returned as it is.
getOr :: Value -> Text -> Value -> Value
getOr dflt k v = fromMaybe dflt (lookupKey k v)

manifestErr :: Text -> IO a
manifestErr = throwIO . ProjectManifestInvalid

resolveRelative :: FilePath -> Value -> Text -> IO FilePath
resolveRelative root value field = do
  let raw = pyStrOr (Just value)
  when (T.null raw) $ manifestErr ("Project manifest field `" <> field <> "` must not be empty.")
  when (isAbsolute (T.unpack raw)) $ manifestErr ("Project manifest field `" <> field <> "` must be relative.")
  resolved <- canonicalizePath (root </> T.unpack raw)
  unless (within root resolved) $
    manifestErr ("Project manifest field `" <> field <> "` points outside the project root.")
  pure resolved

-- | Load from a project root, its @.gauss@ directory, or the manifest
-- itself. Creates the runtime, cache and workflows directories if missing.
loadProject :: FilePath -> IO Project
loadProject target = do
  candidate <- resolve target
  isFile <- doesFileExist candidate
  let (gdir, mpath, root)
        | isFile = (takeDirectory candidate, candidate, takeDirectory (takeDirectory candidate))
        | takeFileName candidate == gaussDirName =
            (candidate, candidate </> manifestFileName, takeDirectory candidate)
        | otherwise =
            (candidate </> gaussDirName, candidate </> gaussDirName </> manifestFileName, candidate)
      mp = tshow mpath
  gdirOk <- doesDirectoryExist gdir
  unless gdirOk $ throwIO (ProjectNotFound ("Gauss project directory not found at " <> tshow gdir <> "."))
  mOk <- doesFileExist mpath
  unless mOk $
    manifestErr ("Found `" <> tshow gdir <> "` but `" <> T.pack manifestFileName <> "` is missing.")
  decoded <- Y.decodeFileEither mpath
  payload0 <- case decoded of
    Left e -> manifestErr ("Failed to read " <> mp <> ": " <> T.pack (Y.prettyPrintParseException e))
    Right v -> pure v
  payload <- case payload0 of
    v | not (pyTruthy v) -> pure (Object KM.empty)
    v@(Object _) -> pure v
    _ -> manifestErr (mp <> " must contain a mapping.")

  let sv = lookupKey "schema_version" payload
      svOk = case sv of
        Just (Number n) -> n == fromIntegral schemaVersion
        Just (Bool True) -> schemaVersion == 1
        _ -> False
  unless svOk $
    manifestErr (mp <> " has schema_version=" <> pyRepr sv <> "; expected " <> T.pack (show schemaVersion) <> ".")

  let name = pyStrOr (lookupKey "name" payload)
  when (T.null name) $ manifestErr (mp <> " is missing a non-empty `name`.")

  let kind = let k = pyStrOr (lookupKey "kind" payload) in if T.null k then "lean4" else k
  pk <- case proverByName kind of
    Just k -> pure k
    Nothing -> manifestErr (mp <> " has unsupported `kind`: " <> pyRepr (Just (String kind)) <> ".")

  -- @lean_root@ and @root@ name the same thing; Lean's own key comes first
  -- for Lean, @root@ for the others. Either alone serves; both must agree.
  let (ownKey, otherKey) = if pk == Lean4 then ("lean_root", "root") else ("root", "lean_root")
      key = case (lookupKey ownKey payload, lookupKey otherKey payload) of
        (Nothing, Just _) -> otherKey
        _ -> ownKey
  proverRoot <- resolveRelative root (getOr (String ".") key payload) key
  case (lookupKey "lean_root" payload, lookupKey "root" payload) of
    (Just _, Just _) -> do
      let other = if key == "root" then "lean_root" else "root"
      otherRoot <- resolveRelative root (getOr (String ".") other payload) other
      when (otherRoot /= proverRoot) $ manifestErr (mp <> " declares lean_root and root differently.")
    _ -> pure ()
  rootOk <- isProverRoot pk proverRoot
  unless rootOk $
    manifestErr
      ( mp <> " declares " <> key <> "=" <> pyRepr (lookupKey key payload)
          <> ", but that directory is not " <> withArticle pk <> " project root." )

  pathsV <- case getOr (Object KM.empty) "paths" payload of
    v@(Object _) -> pure v
    _ -> manifestErr (mp <> " has invalid `paths` metadata.")
  let pathField k = getOr (String (T.pack (gaussDirName </> T.unpack k))) k pathsV
  runtimeDir <- resolveRelative root (pathField "runtime") "paths.runtime"
  cacheDir <- resolveRelative root (pathField "cache") "paths.cache"
  workflowsDir <- resolveRelative root (pathField "workflows") "paths.workflows"

  let section k = case getOr (Object KM.empty) k payload of
        Null -> pure (Object KM.empty)
        v@(Object _) -> pure v
        _ -> manifestErr (mp <> " has invalid `" <> k <> "` metadata.")
  sourceV <- section "source"
  blueprintV <- section "blueprint"

  let markers =
        filter (not . T.null)
          [ T.strip (pyStr m) | m <- pyIter (getOr Null "markers" blueprintV) ]

  forM_ [gdir, runtimeDir, cacheDir, workflowsDir] (createDirectoryIfMissing True)

  let sourceMode = let s = pyStrOr (lookupKey "mode" sourceV) in if T.null s then "init" else s
  pure Project
    { projectRoot = root
    , projectGaussDir = gdir
    , projectManifestPath = mpath
    , projectName = name
    , projectKind = kind
    , projectSchemaVersion = schemaVersion
    , projectLeanRoot = proverRoot
    , projectProver = pk
    , projectProverRoot = proverRoot
    , projectRuntimeDir = runtimeDir
    , projectCacheDir = cacheDir
    , projectWorkflowsDir = workflowsDir
    , projectCreatedAt = pyStrOr (lookupKey "created_at" payload)
    , projectSourceMode = sourceMode
    , projectTemplateSource = pyStrOr (lookupKey "template_source" sourceV)
    , projectBlueprintMarkers = markers
    , projectManifest = payload
    }

-- | The nearest directory at or above @start@ holding a @.gauss@ entry. An
-- entry that is not a valid project is an error, not a reason to keep
-- climbing, as upstream.
discoverProject :: FilePath -> IO Project
discoverProject start = resolve start >>= go . selfAndParents
  where
    go [] =
      throwIO . ProjectNotFound $
        "No active Gauss project found. Use `/project init`, `/project convert`, "
          <> "or `/project use <path>` first."
    go (c : cs) = doesPathExist (c </> gaussDirName) >>= \yes -> if yes then loadProject c else go cs

data InitOptions = InitOptions
  { initName :: Maybe Text
  , initLeanRoot :: Maybe FilePath
  , initSourceMode :: Text
  , initTemplateSource :: Text
  , initBlueprintMarkers :: Maybe [Text]
  , initProver :: Maybe ProverKind
  -- ^ @--prover@: the kind to look for, skipping detection's tie check.
  }

defaultInitOptions :: InitOptions
defaultInitOptions = InitOptions Nothing Nothing "init" "" Nothing Nothing

-- | Key order of the manifest as upstream writes it.
manifestKeyOrder :: [Text]
manifestKeyOrder =
  [ "schema_version", "name", "kind", "lean_root", "root", "created_at", "source", "blueprint", "paths"
  , "mode", "template_source", "markers", "runtime", "cache", "workflows" ]

-- | The manifest as upstream writes it: keys in its order, and the
-- timestamp single-quoted. Unquoted, PyYAML reads @created_at@ back as a
-- datetime and the Python build then prints it with a space for the @T@.
renderManifest :: Value -> BS.ByteString
renderManifest = quoteCreatedAt . YP.encodePretty (YP.setConfCompare order YP.defConfig)
  where
    quoteCreatedAt = BC.unlines . map quoteLine . BC.lines
    quoteLine l = case BC.stripPrefix "created_at: " l of
      Just v | not (BC.null v), BC.head v /= '\'' -> "created_at: '" <> v <> "'"
      _ -> l
    order a b = compare (rank a, a) (rank b, b)
    rank k = fromMaybe (length manifestKeyOrder) (elemIndex k manifestKeyOrder)

-- | Write a manifest under @root@ unless one is there, then load it.
initializeProject :: FilePath -> InitOptions -> IO Project
initializeProject root0 opts = do
  root <- resolve root0
  createDirectoryIfMissing True root
  let mpath = root </> gaussDirName </> manifestFileName
  existing <- doesFileExist mpath
  if existing
    then loadProject root
    else do
      found <- case (initLeanRoot opts, initProver opts) of
        (Just p, k) -> do
          lr <- resolve p
          -- A root given outright: its kind is the one asked for, else the
          -- one its markers show, else Lean (whose check below then fails
          -- as before).
          kind <- case k of
            Just kk -> pure kk
            Nothing -> markerKinds lr >>= \case
              [] -> pure Lean4
              [kk] -> pure kk
              (x : y : _) -> throwIO (ProjectCommandFailed (tieMessage lr x y))
          pure (Just (kind, lr))
        (Nothing, Just k) -> fmap (k,) <$> findProverRoot k root
        (Nothing, Nothing) -> detectProver root >>= either (throwIO . ProjectCommandFailed) pure
      (pk, lr) <- case found of
        Nothing -> throwIO (ProjectCommandFailed (notFound (fromMaybe Lean4 (initProver opts))))
        Just x -> pure x
      let rootWord = if pk == Lean4 then "Lean" else proverTitle pk
      unless (within root lr) $
        throwIO . ProjectCommandFailed $
          rootWord <> " root " <> tshow lr <> " is outside the requested project root " <> tshow root <> "."
      rootOk <- isProverRoot pk lr
      unless rootOk $
        throwIO . ProjectCommandFailed $
          rootWord <> " root " <> tshow lr <> " is missing " <> markerText pk <> "."
      let gdir = root </> gaussDirName
      forM_ [gdir, gdir </> "runtime", gdir </> "cache", gdir </> "workflows"] (createDirectoryIfMissing True)
      markers <- maybe (detectBlueprintMarkers root) pure (initBlueprintMarkers opts)
      now <- getCurrentTime
      let name = T.strip $ case initName opts of
            Just n | not (T.null n) -> n
            _ | not (null (takeFileName root)) -> T.pack (takeFileName root)
            _ -> "gauss-project"
          rel = makeRelative root lr
          mode = let m = T.strip (if T.null (initSourceMode opts) then "init" else initSourceMode opts)
                  in if T.null m then "init" else m
          payload =
            object
              [ "schema_version" .= schemaVersion
              , "name" .= name
              , "kind" .= pvName (prover pk)
              , (if pk == Lean4 then "lean_root" else "root") .= (if null rel then "." else rel)
              , "created_at" .= formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S+00:00" now
              , "source" .= object ["mode" .= mode, "template_source" .= T.strip (initTemplateSource opts)]
              , "blueprint" .= object ["markers" .= markers]
              , "paths" .= object
                  [ "runtime" .= (".gauss/runtime" :: Text)
                  , "cache" .= (".gauss/cache" :: Text)
                  , "workflows" .= (".gauss/workflows" :: Text) ]
              ]
      BS.writeFile mpath (renderManifest payload)
      loadProject root

-- | No root of the kind at or above the directory given to init.
notFound :: ProverKind -> Text
notFound = \case
  Lean4 ->
    "Lean project root not found. `/project init` and `/project convert` "
      <> "work inside Lean 4 repositories (expected `lakefile.lean` or `lakefile.toml`)."
  k ->
    proverTitle k <> " project root not found. `/project init` and `/project convert` "
      <> "work inside " <> proverTitle k <> " repositories (expected " <> markerText k <> ")."

-- | The kind's markers in prose: @`lakefile.lean` or `lakefile.toml`@.
markerText :: ProverKind -> Text
markerText k =
  let names = [ case m of
                  FileMarker f -> "`" <> T.pack f <> "`"
                  DuneMarker -> "a `dune-project` using coq or rocq"
              | m <- pvMarkers (prover k) ]
   in case reverse names of
        [] -> ""
        [one] -> one
        (lastName : rest) -> T.intercalate ", " (reverse rest) <> " or " <> lastName

-- | Fill a new directory from a template: a local git checkout is cloned, a
-- plain local directory is copied, anything else is handed to @git clone@.
populateFromTemplate :: FilePath -> Text -> IO ()
populateFromTemplate target0 source0 = do
  target <- resolve target0
  taken <- doesPathExist target
  when taken $ throwIO (ProjectCommandFailed ("Target path already exists: " <> tshow target))
  let source = T.strip source0
  when (T.null source) $
    throwIO (ProjectTemplateUnavailable "Blueprint project creation is blocked until a template source is configured.")
  sourcePath <- expandUser (T.unpack source)
  present <- doesPathExist sourcePath
  if not present
    then gitClone (T.unpack source) target
    else do
      isDir <- doesDirectoryExist sourcePath
      isRepo <- doesDirectoryExist (sourcePath </> ".git")
      if isDir && isRepo
        then canonicalizePath sourcePath >>= \s -> gitClone s target
        else
          if isDir
            then copyTree sourcePath target
            else throwIO (ProjectCommandFailed ("Template source is not a directory: " <> tshow sourcePath))

-- | @shutil.copytree@ with its defaults: symlinks followed, metadata copied.
copyTree :: FilePath -> FilePath -> IO ()
copyTree src dst = do
  createDirectoryIfMissing True dst
  entries <- listDirectory src
  forM_ entries $ \e -> do
    isDir <- doesDirectoryExist (src </> e)
    if isDir then copyTree (src </> e) (dst </> e) else copyFileWithMetadata (src </> e) (dst </> e)

gitClone :: String -> FilePath -> IO ()
gitClone source target = do
  createDirectoryIfMissing True (takeDirectory target)
  (code, out, err) <- readProcess (proc "git" ["clone", "--depth", "1", source, target])
  case code of
    ExitSuccess -> pure ()
    ExitFailure _ -> do
      let pick = T.strip . T.pack . BL8.unpack
          msg | not (T.null (pick err)) = pick err
              | not (T.null (pick out)) = pick out
              | otherwise = T.pack (show code)
      throwIO . ProjectCommandFailed $
        "Failed to clone template source " <> pyRepr (Just (String (T.pack source))) <> ": " <> msg

-- | @label · cwd:<relative> · blueprint@, the cwd part only when the given
-- directory is strictly inside the project.
formatProjectSummary :: Project -> Maybe FilePath -> IO Text
formatProjectSummary p mcwd = do
  cwdBit <- case mcwd of
    Nothing -> pure []
    Just c -> do
      rc <- canonicalizePath c
      let rel = makeRelative (projectRoot p) rc
      pure ["cwd:" <> T.pack rel | within (projectRoot p) rc, rel `notElem` ["", "."]]
  pure (T.intercalate " · " ([projectLabel p] <> cwdBit <> ["blueprint" | isBlueprint p]))
