-- | The @/project@ command: @status@, @init@, @convert@, @use@, @clear@ and
-- @create@, with upstream's output text (@GaussCLI._handle_project_command@
-- in @cli.py@). The session override lives in a 'ProjectSession' the caller
-- keeps: the REPL for the length of a session, the @fisher@ CLI in a file.
module Fisher.ProjectCommand
  ( ProjectSession (..)
  , ProjectState (..)
  , handleProjectCommand
  , handleProjectTokens
  , projectState
  , activeProjectSummary
  , projectUsage
  , resolveCliPath
  ) where

import Control.Exception (throwIO, try)
import Data.Aeson (Value)
import Data.IORef (IORef, newIORef, modifyIORef', readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (canonicalizePath, doesFileExist)
import System.FilePath (isAbsolute, takeDirectory, takeFileName, (</>))

import Fisher.Compat (projectDirNames)
import Fisher.Paths (expandUser)
import Fisher.Project
import Fisher.Prover (ProverKind (..), findProverRoot, markerKinds, proverByName, proverTitle, tieMessage)
import Fisher.Shlex (shlexSplit)

-- | What @/project@ needs from the session around it.
data ProjectSession = ProjectSession
  { sessionCwd :: FilePath
  -- ^ @TERMINAL_CWD@ or the process's working directory.
  , sessionConfig :: Value
  , sessionEnv :: [(String, String)]
  , sessionOverride :: IORef (Maybe FilePath)
  }

-- | Upstream's @(project, source, error)@.
data ProjectState = ProjectState
  { stateProject :: Maybe Project
  , stateSource :: Text
  , stateError :: Maybe Text
  }

resolveDir :: FilePath -> IO FilePath
resolveDir p = expandUser p >>= canonicalizePath

-- | @_resolve_cli_path@: blank is the session directory, a relative path is
-- taken from it.
resolveCliPath :: ProjectSession -> Maybe Text -> IO FilePath
resolveCliPath s raw = do
  base <- resolveDir (sessionCwd s)
  case T.strip <$> raw of
    Nothing -> pure base
    Just r | T.null r -> pure base
    Just r -> do
      p <- expandUser (T.unpack r)
      canonicalizePath (if isAbsolute p then p else base </> p)

-- | The override when there is one, else discovery from the session cwd.
projectState :: ProjectSession -> IO ProjectState
projectState s = do
  override <- readIORef (sessionOverride s)
  case override of
    Just root -> fromResult "override" <$> try (loadProject root)
    Nothing -> do
      cwd <- resolveDir (sessionCwd s)
      fromResult "ambient" <$> try (discoverProject cwd)
  where
    fromResult src = \case
      Right p -> ProjectState (Just p) src Nothing
      Left e -> ProjectState Nothing src (Just (projectErrorMessage e))

-- | @_active_project_summary@: the status-bar label, @[override]@ marked.
activeProjectSummary :: ProjectSession -> IO (Maybe Text)
activeProjectSummary s = do
  st <- projectState s
  case stateProject st of
    Nothing -> pure Nothing
    Just p -> do
      summary <- formatProjectSummary p (Just (sessionCwd s))
      pure (Just (if stateSource st == "override" then summary <> " [override]" else summary))

projectUsage :: [Text]
projectUsage =
  [ "  Usage:"
  , "    /project status"
  , "    /project init [path] [--name <name>] [--prover lean4|rocq|isabelle]"
  , "    /project convert [path] [--name <name>] [--prover lean4|rocq|isabelle]"
  , "    /project use [path]"
  , "    /project clear"
  , "    /project create <path> [--template-source <source>] [--name <name>]" ]

statusLines :: ProjectSession -> IO [Text]
statusLines s = do
  st <- projectState s
  override <- readIORef (sessionOverride s)
  let header =
        [ ""
        , "  Project Status"
        , "  CWD: " <> T.pack (sessionCwd s)
        , "  Session override: " <> maybe "(none)" T.pack override ]
  pure . (header <>) $ case stateProject st of
    Nothing ->
      [ "  Active project: (none)"
      , "  Detail: " <> maybe "No active Fisher project found." id (stateError st)
      , "  Next: /project init, /project convert, /project create <path>, or /project use <path>" ]
    Just p ->
      [ "  Active project: " <> projectLabel p <> " [" <> stateSource st <> "]"
      , "  Root: " <> T.pack (projectRoot p)
      , "  " <> rootLabel (projectProver p) <> " root: " <> T.pack (projectProverRoot p)
      , "  Manifest: " <> T.pack (projectManifestPath p)
      , "  Source mode: " <> projectSourceMode p ]
        <> ["  Template source: " <> projectTemplateSource p | not (T.null (projectTemplateSource p))]
        <> ["  Blueprint markers: " <> T.intercalate ", " (projectBlueprintMarkers p) | isBlueprint p]

-- | @_parse_project_options@: @--key value@ pairs and the positional rest.
parseOptions :: [Text] -> Either Text ([Text], Map.Map Text Text)
parseOptions = go [] Map.empty
  where
    go pos opts [] = Right (reverse pos, opts)
    go pos opts (t : rest)
      | "--" `T.isPrefixOf` t = case rest of
          (v : rest') -> go pos (Map.insert t v opts) rest'
          [] -> Left ("Missing value for option `" <> t <> "`.")
      | otherwise = go (t : pos) opts rest

-- | The kind as the status and errors name its root: @Lean@, @Rocq@,
-- @Isabelle@ (Lean's line is upstream's @Lean root:@).
rootLabel :: ProverKind -> Text
rootLabel = \case
  Lean4 -> "Lean"
  k -> proverTitle k

-- | The prover root for @init@ and @convert@: the nearest root of the
-- @--prover@ kind when one is named, else the nearest root of any kind.
-- No root is the caller's message, for the kind named or for Lean. A root
-- that already holds a manifest is returned without a kind, tie or not, so
-- that init loads that manifest.
findRoot :: Maybe Text -> FilePath -> (ProverKind -> Text) -> IO (Maybe ProverKind, FilePath)
findRoot named root missing = case named of
  Just n -> case proverByName n of
    Nothing -> throwIO (ProjectCommandFailed ("Unknown prover `" <> n <> "`; expected lean4, rocq or isabelle."))
    Just k -> findProverRoot k root >>= maybe (throwIO (ProjectCommandFailed (missing k))) (pure . (Just k,))
  Nothing -> go (selfAndParents root)
  where
    go [] = throwIO (ProjectCommandFailed (missing Lean4))
    go (d : ds) =
      markerKinds d >>= \case
        [] -> go ds
        kinds -> do
          hasManifest <- or <$> mapM (\n -> doesFileExist (d </> n </> "project.yaml")) projectDirNames
          case kinds of
            _ | hasManifest -> pure (Nothing, d)
            [k] -> pure (Just k, d)
            (a : b : _) -> throwIO (ProjectCommandFailed (tieMessage d a b))
    selfAndParents p = let up = takeDirectory p in if up == p then [p] else p : selfAndParents up

-- | Handle a whole @/project ...@ line, as typed.
handleProjectCommand :: ProjectSession -> Text -> IO [Text]
handleProjectCommand s line = case shlexSplit line of
  Left e -> pure ["(>_<) Could not parse `/project`: " <> e]
  Right parts -> handleProjectTokens s (drop 1 parts)

-- | Handle the words after @/project@. Returns the lines upstream prints.
handleProjectTokens :: ProjectSession -> [Text] -> IO [Text]
handleProjectTokens s tokens = do
  out <- newIORef []
  let say ls = modifyIORef' out (<> ls)
      sub = maybe "status" T.toLower (safeHead tokens)
      args = drop 1 tokens
  r <- try $ do
    (positional, options) <- either (throwIO . ProjectCommandFailed) pure (parseOptions args)
    let firstPos = safeHead positional
        opt k = case Map.lookup k options of
          Just v | not (T.null v) -> Just v
          _ -> Nothing
    case sub of
      _ | sub `elem` ["status", "inspect"] -> statusLines s >>= say
      "clear" -> do
        writeIORef (sessionOverride s) Nothing
        say ["(^_^) Cleared the session project override."]
        statusLines s >>= say
      _ | sub `elem` ["use", "set"] -> do
        target <- resolveCliPath s firstPos
        p <- discoverProject target
        writeIORef (sessionOverride s) (Just (projectRoot p))
        say ["(^_^)b Active project set to: " <> projectLabel p, "  Root: " <> T.pack (projectRoot p)]
      "init" -> do
        root <- resolveCliPath s firstPos
        (kind, lr) <- findRoot (opt "--prover") root
          (\k -> rootLabel k <> " project root not found. `/project init` works inside an existing " <> proverTitle k <> " repository.")
        p <- initializeProject lr defaultInitOptions
          { initName = Map.lookup "--name" options, initSourceMode = "init", initProver = kind }
        writeIORef (sessionOverride s) (Just (projectRoot p))
        say ["(^_^)b Initialized Fisher project: " <> projectLabel p, "  Root: " <> T.pack (projectRoot p)]
      "convert" -> do
        root <- resolveCliPath s firstPos
        (kind, lr) <- findRoot (opt "--prover") root $ \case
          Lean4 -> "Lean project root not found. `/project convert` expects a Lean 4 blueprint repository."
          k -> proverTitle k <> " project root not found. `/project convert` expects a " <> proverTitle k <> " repository."
        markers <- detectBlueprintMarkers lr
        p <- initializeProject lr defaultInitOptions
          { initName = Map.lookup "--name" options
          , initSourceMode = if null markers then "convert" else "convert-blueprint"
          , initBlueprintMarkers = Just markers
          , initProver = kind }
        writeIORef (sessionOverride s) (Just (projectRoot p))
        say ["(^_^)b Registered existing repository as a Fisher project: " <> projectLabel p, "  Root: " <> T.pack (projectRoot p)]
        say ["  Blueprint markers: " <> T.intercalate ", " markers | not (null markers)]
      "create" -> do
        rawTarget <- maybe (throwIO (ProjectCommandFailed "`/project create` requires a target directory path.")) pure firstPos
        target <- resolveCliPath s (Just rawTarget)
        let source = maybe (resolveTemplateSource (sessionConfig s) (sessionEnv s)) id (opt "--template-source")
        if T.null source
          then throwIO . ProjectTemplateUnavailable $
            "Blueprint project creation is blocked until a template source is configured. "
              <> "Set `fisher.project.template_source` or `FISHER_BLUEPRINT_TEMPLATE_SOURCE`, then retry."
          else pure ()
        populateFromTemplate target source
        markers <- detectBlueprintMarkers target
        p <- initializeProject target defaultInitOptions
          { initName = Just (maybe (T.pack (takeFileName target)) id (opt "--name"))
          , initSourceMode = "template"
          , initTemplateSource = source
          , initBlueprintMarkers = Just markers }
        writeIORef (sessionOverride s) (Just (projectRoot p))
        say [ "(^_^)b Created Fisher project from template: " <> projectLabel p
            , "  Root: " <> T.pack (projectRoot p)
            , "  Template source: " <> source ]
      _ -> throwIO (ProjectCommandFailed ("Unknown `/project` subcommand: " <> sub))
  case r of
    Right () -> pure ()
    Left e -> say (("(>_<) " <> projectErrorMessage e) : projectUsage)
  readIORef out
  where
    safeHead = \case
      (x : _) -> Just x
      [] -> Nothing

