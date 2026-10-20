-- | The @/project@ command: @status@, @init@, @convert@, @use@, @clear@ and
-- @create@, with upstream's output text (the CLI class's @_handle_project_command@
-- in @cli.py@), and Fisher's @templates@, @build@ and @create --template@.
-- The session override lives in a 'ProjectSession' the caller keeps: the
-- REPL for the length of a session, the @fisher@ CLI in a file.
--
-- Creating from a template builds and checks the project with the search's
-- Lean probe, which lives above this package, so the session carries the
-- functions that do it ('TemplateOps', filled in by fisher-cli).
module Fisher.ProjectCommand
  ( ProjectSession (..)
  , ProjectState (..)
  , TemplateOps (..)
  , CreateRequest (..)
  , createFromSource
  , templateListLines
  , handleProjectCommand
  , handleProjectTokens
  , handleProjectTokensStatus
  , interruptedLead
  , projectState
  , activeProjectSummary
  , projectUsage
  , resolveCliPath
  ) where

import Control.Exception (throwIO, try)
import Data.Aeson (Value)
import Data.IORef (IORef, newIORef, modifyIORef', readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (canonicalizePath, doesFileExist)
import System.FilePath (isAbsolute, takeDirectory, takeFileName, (</>))

import Fisher.Paths (expandUser, fisherDirName)
import Fisher.Project
import Fisher.Prover (Prover (..), ProverKind (..), findProverRoot, markerKinds, prover, proverByName, proverTitle, tieMessage)
import Fisher.Shlex (shlexSplit)
import Fisher.Template (Origin (..), Template (..), listTemplatesWithNotes)

-- | What creating from a template takes: @create@'s words, parsed.
data CreateRequest = CreateRequest
  { crTarget :: FilePath
  -- ^ absolute
  , crTemplate :: Maybe Text
  -- ^ @--template NAME@
  , crSource :: Maybe Text
  -- ^ @--template-source S@ (or the configured source): with a manifest, an
  -- unnamed template; without one, copied or cloned as before
  , crVars :: [(Text, Text)]
  -- ^ @--var k=v@, in the order given
  , crBuild :: Bool
  -- ^ false for @--no-build@
  , crYes :: Bool
  -- ^ @--yes@: run a custom template's steps without asking
  , crName :: Maybe Text
  -- ^ @--name@
  , crTrustRevision :: Maybe Text
  -- ^ @--trust-revision REV@: run a custom template's steps without asking
  -- when its revision is REV (the one a user confirmed), else stop
  }
  deriving (Eq, Show)

-- | Creating, building and listing templates, as the @fisher@ command line
-- does them. Each returns the lines to print after its own progress lines.
--
-- A create or build that fails returns its @(>_<)@ line; one that fails
-- in a way the old create did (a plain template source) throws
-- 'ProjectError', so the usage follows as before.
data TemplateOps = TemplateOps
  { toCreate :: CreateRequest -> IO [Text]
  , toBuild :: Bool -> FilePath -> IO [Text]
  -- ^ @--yes@, the project
  , toList :: IO [Text]
  }

-- | What @/project@ needs from the session around it.
data ProjectSession = ProjectSession
  { sessionCwd :: FilePath
  -- ^ @TERMINAL_CWD@ or the process's working directory.
  , sessionConfig :: Value
  , sessionEnv :: [(String, String)]
  , sessionOverride :: IORef (Maybe FilePath)
  , sessionTemplates :: Maybe TemplateOps
  -- ^ 'Nothing' where only this package is linked (the web app's server):
  -- @create --template@ and @build@ then say they need the command line
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
  , "    /project create <path> [--template <name>] [--var <k=v>]... [--no-build] [--yes] [--trust-revision <rev>] [--name <name>]"
  , "    /project create <path> --template-source <source> [--name <name>]"
  , "      (with neither, a configured template source, else --template lean-mathlib)"
  , "    /project build [path] [--yes]"
  , "    /project templates" ]

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

-- | The words after the subcommand.
data Parsed = Parsed
  { pPositional :: [Text]
  , pOptions :: Map.Map Text Text
  , pFlags :: [Text]
  , pVars :: [(Text, Text)]
  }

-- | Options that take no value.
booleanFlags :: [Text]
booleanFlags = ["--no-build", "--yes"]

-- | @_parse_project_options@: @--key value@ pairs and the positional rest,
-- with Fisher's flags ('booleanFlags') and the repeatable @--var NAME=VALUE@.
parseOptions :: [Text] -> Either Text Parsed
parseOptions = go (Parsed [] Map.empty [] [])
  where
    go p [] = Right p {pPositional = reverse (pPositional p), pVars = reverse (pVars p)}
    go p (t : rest)
      | t `elem` booleanFlags = go p {pFlags = t : pFlags p} rest
      | t == "--var" = case rest of
          (v : rest') -> case T.breakOn "=" v of
            (k, eqv) | not (T.null k), not (T.null eqv) -> go p {pVars = (k, T.drop 1 eqv) : pVars p} rest'
            _ -> Left ("`--var` takes NAME=VALUE; got `" <> v <> "`.")
          [] -> Left "Missing value for option `--var`."
      | "--" `T.isPrefixOf` t = case rest of
          (v : rest') -> go p {pOptions = Map.insert t v (pOptions p)} rest'
          [] -> Left ("Missing value for option `" <> t <> "`.")
      | otherwise = go p {pPositional = t : pPositional p} rest

-- | @fisher project templates@: the built-ins and the configured ones, with
-- prover and description, then any notes (a configured template replacing a
-- built-in, or failing to load).
templateListLines :: Value -> IO [Text]
templateListLines config = do
  (ts, notes) <- listTemplatesWithNotes config
  let width = maximum (0 : map (T.length . tplName) ts)
      line t =
        "    " <> T.justifyLeft width ' ' (tplName t) <> "  " <> pvName (prover (tplProver t)) <> "  " <> tplDescription t
          <> (if tplName t == "lean-mathlib" then " (the default)" else "")
          <> (case tplOrigin t of
                Configured src -> " [" <> src <> "]"
                _ -> "")
  pure (("  Templates:" : map line ts) <> map ("  Note: " <>) notes)

-- | @create@ from a source with no template manifest, as before templates:
-- the source copied or cloned to the target, then registered. Throws
-- 'ProjectError'.
createFromSource :: FilePath -> Text -> Maybe Text -> IO (Project, [Text])
createFromSource target source name = do
  populateFromTemplate target source
  markers <- detectBlueprintMarkers target
  p <- initializeProject target defaultInitOptions
    { initName = Just (maybe (T.pack (takeFileName target)) id name)
    , initSourceMode = "template"
    , initTemplateSource = source
    , initBlueprintMarkers = Just markers }
  pure
    ( p
    , [ "(^_^)b Created Fisher project from template: " <> projectLabel p
      , "  Root: " <> T.pack (projectRoot p)
      , "  Template source: " <> source ] )

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
          hasManifest <- doesFileExist (d </> fisherDirName </> "project.yaml")
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
handleProjectTokens s tokens = snd <$> handleProjectTokensStatus s tokens

-- | How the message of a create or build stopped by Ctrl-C (or SIGTERM)
-- begins; the project's root follows.
interruptedLead :: Text
interruptedLead = "Interrupted; resume with fisher project build "

-- | 'handleProjectTokens' with the exit status for the command line: 0,
-- 1 when the command failed, 2 when it was used wrongly, 130 when a create
-- or build was interrupted.
handleProjectTokensStatus :: ProjectSession -> [Text] -> IO (Int, [Text])
handleProjectTokensStatus s tokens = do
  out <- newIORef []
  misuse <- newIORef False
  failedOps <- newIORef False
  interrupted <- newIORef False
  let say ls = modifyIORef' out (<> ls)
      usageError :: Text -> IO a
      usageError m = writeIORef misuse True >> throwIO (ProjectCommandFailed m)
      fromOps ls = do
        if any ("(>_<)" `T.isPrefixOf`) ls then writeIORef failedOps True else pure ()
        if any (("(>_<) " <> interruptedLead) `T.isPrefixOf`) ls then writeIORef interrupted True else pure ()
        say ls
      sub = maybe "status" T.toLower (safeHead tokens)
      args = drop 1 tokens
  r <- try $ do
    parsed <- either usageError pure (parseOptions args)
    let positional = pPositional parsed
        options = pOptions parsed
        firstPos = safeHead positional
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
      "templates" -> maybe (templateListLines (sessionConfig s)) toList (sessionTemplates s) >>= say
      "build" -> do
        ops <- needsCli "Building a template project needs the fisher command line."
        target <- resolveCliPath s firstPos
        toBuild ops ("--yes" `elem` pFlags parsed) target >>= fromOps
      "create" -> do
        rawTarget <- maybe (usageError "`/project create` requires a target directory path.") pure firstPos
        target <- resolveCliPath s (Just rawTarget)
        let given = opt "--template-source"
            source = maybe (resolveTemplateSource (sessionConfig s) (sessionEnv s)) id given
            named = opt "--template"
            request src = CreateRequest
              { crTarget = target
              , crTemplate = named
              , crSource = src
              , crVars = pVars parsed
              , crBuild = "--no-build" `notElem` pFlags parsed
              , crYes = "--yes" `elem` pFlags parsed
              , crName = opt "--name"
              , crTrustRevision = opt "--trust-revision" }
        case (sessionTemplates s, named, given) of
          (_, Just _, Just _) -> usageError "Pass `--template` or `--template-source`, not both."
          (Nothing, Just _, _) -> throwIO (ProjectCommandFailed "Creating from a template needs the fisher command line.")
          (Just ops, _, _) -> do
            -- the override follows a project this create made, not one
            -- that was there already
            let manifest = target </> fisherDirName </> "project.yaml"
            before <- doesFileExist manifest
            toCreate ops (request (if isJust named || T.null source then Nothing else Just source)) >>= fromOps
            after <- doesFileExist manifest
            if after && not before then writeIORef (sessionOverride s) (Just target) else pure ()
          (Nothing, Nothing, _) -> do
            if T.null source
              then throwIO . ProjectTemplateUnavailable $
                "Blueprint project creation is blocked until a template source is configured. "
                  <> "Set `fisher.project.template_source` or `FISHER_BLUEPRINT_TEMPLATE_SOURCE`, then retry."
              else pure ()
            (p, ls) <- createFromSource target source (opt "--name")
            writeIORef (sessionOverride s) (Just (projectRoot p))
            say ls
      _ -> usageError ("Unknown `/project` subcommand: " <> sub)
  status <- case r of
    Right () -> do
      stopped <- readIORef interrupted
      f <- readIORef failedOps
      pure (if stopped then 130 else if f then 1 else 0)
    Left e -> do
      say (("(>_<) " <> projectErrorMessage e) : projectUsage)
      (\m -> if m then 2 else 1) <$> readIORef misuse
  (,) status <$> readIORef out
  where
    needsCli msg = maybe (throwIO (ProjectCommandFailed msg)) pure (sessionTemplates s)
    safeHead = \case
      (x : _) -> Just x
      [] -> Nothing

