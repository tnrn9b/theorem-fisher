{-# LANGUAGE MultiWayIf #-}
-- | @fisher project create --template@, @fisher project build@ and the
-- template functions the project command calls ('templateOps'): load the
-- template and its variables, check the target and the tools, ask before a
-- custom template's steps run, write the files and @.fisher/project.yaml@,
-- run the steps, then check the sample with the search's Lean probe.
--
-- The manifest's @template@ key holds what the project was made from (name,
-- revision, variables) and what @fisher project build@ needs to finish it
-- without the template (the rendered steps and the sample's path); the step
-- runner adds @done@ as steps finish.
module Fisher.Cli.Create
  ( -- * Creating and building
    CreateDeps (..)
  , defaultDeps
  , createProject
  , createProjectWith
  , buildProject
  , buildProjectWith
  , templateOps
    -- * The parts
  , preflight
  , targetProblem
  , checkSample
  , checkSampleWith
  , templateResolver
  , sampleTheorems
  , askOn
  , interruptible
  , interruptedMessage
  ) where

import Control.Concurrent (myThreadId, threadDelay, throwTo)
import Control.Exception (AsyncException (..), SomeException, bracket, catchJust, finally, try)
import Control.Monad (forM_, unless, when)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.List (isPrefixOf, isSuffixOf)
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import qualified Data.Yaml as Y
import System.Directory
  ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist, doesPathExist, getTemporaryDirectory
  , listDirectory, makeAbsolute, removeDirectoryRecursive, removeFile )
import System.Environment (getEnvironment)
import System.FilePath (takeDirectory, (</>))
import System.IO (hFlush, stdin, stdout)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (Handler (..), installHandler, sigINT, sigTERM)

import Fisher.Cli.AddSmt (addSmt)
import Fisher.Cli.LakeRun (appendRun, askOn, interruptible, interruptibleWith, lakeErrorLines, probeProblems, removeStaleProbes, runIn)
import Fisher.Config (loadConfigAt)
import Fisher.Paths (ensureFisherHome, expandUser, fisherDirName)
import Fisher.Project (InitOptions (..), Project (..), ProjectError, defaultInitOptions, initializeProject, projectErrorMessage, renderManifest)
import Fisher.ProjectCommand (CreateRequest (..), TemplateOps (..), createFromSource, interruptedLead, templateListLines)
import Fisher.Prover (ProverKind (..), binaryFix, proverByName)
import Fisher.Sha256 (sha256Hex)
import Fisher.Smt.Reservoir (fetchBuilds, pickRevision)
import Fisher.Template
import Fisher.Template.Run
import Fisher.Which (whichEnv)

-- | What creating and building depend on, so tests can stand in for the
-- terminal, the network and Lean.
data CreateDeps = CreateDeps
  { cdSay :: Text -> IO ()
  -- ^ one progress line
  , cdAsk :: Text -> IO Bool
  -- ^ the trust question; 'False' unless answered yes
  , cdConfig :: Value
  , cdEnv :: [(String, String)]
  -- ^ where 'preflight' looks for the tools
  , cdResolver :: Text -> [(Text, Text)] -> IO (Either Text Text)
  -- ^ the latest release of @lean@ or @mathlib@, or the latest @smt@
  -- revision, given the values resolved before it
  , cdCheck :: Template -> FilePath -> IO (Either [Text] ())
  -- ^ the sample check
  , cdTrustFile :: FilePath
  , cdScale :: Int
  -- ^ seconds in one of a step's minutes
  }

-- | The real ones: the configuration in the Fisher home, this process's
-- environment, stdin and stdout, @git ls-remote@ and Reservoir
-- ('templateResolver') and 'checkSample'.
defaultDeps :: IO CreateDeps
defaultDeps = do
  home <- ensureFisherHome
  (cfg, _) <- loadConfigAt home
  env <- getEnvironment
  depsFor cfg env

depsFor :: Value -> [(String, String)] -> IO CreateDeps
depsFor cfg env = do
  tf <- trustFile
  let say l = TIO.putStrLn l >> hFlush stdout
  pure CreateDeps
    { cdSay = say
    , cdAsk = askOn stdin
    , cdConfig = cfg
    , cdEnv = env
    , cdResolver = templateResolver say fetchBuilds
    , cdCheck = checkSample
    , cdTrustFile = tf
    , cdScale = 60 }

-- | What a template's @latest@ resolves to: @lean@ and @mathlib@, the
-- highest release tag ('latestRelease'); @smt@, the newest commit Reservoir
-- built on @leanprover/lean4:@ and the resolved @mathlib@, else @none@ (no
-- lean-smt in the project, whose main may not match that Mathlib) with a
-- warning that names @fisher project add smt@.
templateResolver :: (Text -> IO ()) -> IO (Either Text [(Text, Text)]) -> Text -> [(Text, Text)] -> IO (Either Text Text)
templateResolver say fetch what done
  | what == "smt" = do
      let tc = ("leanprover/lean4:" <>) <$> lookup "mathlib" done
      builds <- fetch
      case pickRevision tc (either (const []) id builds) True of
        Right c -> pure (Right c)
        Left _ -> say (smtNoneWarning builds tc) >> pure (Right "none")
  | otherwise = latestRelease what

-- | The warning when the template's @smt@ resolves to @none@.
smtNoneWarning :: Either Text [(Text, Text)] -> Maybe Text -> Text
smtNoneWarning builds tc = "Warning: " <> why <> "; add it later with fisher project add smt"
  where
    why = case (builds, tc) of
      (Left e, _) -> "could not read Reservoir's builds of lean-smt (" <> e <> ")"
      (_, Just t) -> "lean-smt has no build for " <> t
      (_, Nothing) -> "lean-smt has no build for this toolchain"

-- | The functions the project command calls, over the session's config and
-- environment. A template's progress lines are printed as they happen and
-- the lines returned are only an error's; a plain template source returns
-- the old create's lines, and its errors are thrown as before
-- ('ProjectError'), so the usage follows them.
templateOps :: Value -> [(String, String)] -> TemplateOps
templateOps cfg env = TemplateOps
  { toCreate = \r -> depsFor cfg env >>= \d -> createLines d r >>= \(x, ls) -> pure (ls <> failure x)
  , toBuild = \yes p -> depsFor cfg env >>= \d -> failure <$> buildProjectWith d yes p
  , toList = templateListLines cfg
  , toAddSmt = addSmt }
  where
    failure :: Either Text a -> [Text]
    failure = either (\e -> ["(>_<) " <> e]) (const [])

-- | 'createProjectWith' the real dependencies. Prints its progress and
-- returns the project's root.
createProject :: CreateRequest -> IO (Either Text FilePath)
createProject r = defaultDeps >>= \d -> createProjectWith d r

-- | 'buildProjectWith' the real dependencies; @yes@ runs a custom
-- template's steps without asking.
buildProject :: Bool -> FilePath -> IO (Either Text ())
buildProject yes p = defaultDeps >>= \d -> buildProjectWith d yes p

-- | A template, or a source without a manifest (created as before).
data Chosen = Chosen Template Text | Plain Text

-- | Create, printing the progress; the old create's error is a 'Left' here.
createProjectWith :: CreateDeps -> CreateRequest -> IO (Either Text FilePath)
createProjectWith d r =
  (try (createLines d r) :: IO (Either ProjectError (Either Text FilePath, [Text]))) >>= \case
    Left e -> pure (Left (projectErrorMessage e))
    Right (x, ls) -> mapM_ (cdSay d) ls >> pure x

-- | Create; with the old create's lines for a plain source, whose errors
-- are thrown.
createLines :: CreateDeps -> CreateRequest -> IO (Either Text FilePath, [Text])
createLines d r = do
  target <- makeAbsolute (crTarget r)
  withChosen d r $ \case
    Left e -> pure (Left e, [])
    Right (Plain source) -> do
      (p, ls) <- createFromSource target source (crName r)
      pure (Right (projectRoot p), ls)
    Right (Chosen t sourceText) -> (,[]) <$> fromTemplate d r target t sourceText

-- | The template the request names (default @lean-mathlib@), or its source.
-- A git source is cloned to a temporary directory for the length of the
-- action.
withChosen :: CreateDeps -> CreateRequest -> (Either Text Chosen -> IO a) -> IO a
withChosen d r k = case (crTemplate r, crSource r) of
  (Just n, _) -> findTemplate (cdConfig d) n >>= k . fmap (`Chosen` n)
  (Nothing, Nothing) -> findTemplate (cdConfig d) "lean-mathlib" >>= k . fmap (`Chosen` "lean-mathlib")
  (Nothing, Just src) -> do
    path <- expandUser (T.unpack src)
    isDir <- doesDirectoryExist path
    exists <- doesPathExist path
    if
      | isDir -> do
          hasManifest <- doesFileExist (path </> manifestName)
          if hasManifest then loadTemplate (Source src) path >>= k . fmap (`Chosen` src) else k (Right (Plain src))
      | exists -> k (Right (Plain src))
      | otherwise -> do
          tmp <- getTemporaryDirectory
          pid <- getProcessID
          let dir = tmp </> ("fisher-template-" <> show pid <> "-" <> filter (/= '/') (take 40 (T.unpack (T.takeWhileEnd (/= '/') src))))
          exists' <- doesPathExist dir
          when exists' (removeDirectoryRecursive dir)
          flip finally (try (removeDirectoryRecursive dir) :: IO (Either SomeException ())) $
            shallowClone src dir >>= \case
              -- the old create clones again and reports the failure its way
              Left _ -> k (Right (Plain src))
              Right () -> do
                hasManifest <- doesFileExist (dir </> manifestName)
                if hasManifest then loadTemplate (Source src) dir >>= k . fmap (`Chosen` src) else k (Right (Plain src))

shallowClone :: Text -> FilePath -> IO (Either Text ())
shallowClone url dir = do
  (code, out) <- runIn "git" 600 (takeDirectory dir) ["clone", "--depth", "1", "--", T.unpack url, dir]
  pure (if code == 0 then Right () else Left (T.unwords (T.words out)))

fromTemplate :: CreateDeps -> CreateRequest -> FilePath -> Template -> Text -> IO (Either Text FilePath)
fromTemplate d r target t sourceText =
  targetProblem target >>= \case
    Just e -> pure (Left e)
    Nothing -> preflight (cdEnv d) >>= \case
      Left e | crBuild r -> pure (Left e)
      tools -> do
        either (\e -> cdSay d ("Warning: " <> e)) pure tools
        resolveVariables t target (crVars r) (cdResolver d) >>= \case
          Left e -> pure (Left e)
          Right vals -> case (,) <$> renderSteps t vals <*> renderSample t vals of
            Left e -> pure (Left e)
            Right (steps, sample) -> renderFiles t vals >>= \case
              Left e -> pure (Left e)
              Right files -> do
                trusted <- isTrusted (cdTrustFile d) t
                rev <- templateRevision t
                -- --trust-revision: yes for the revision a user confirmed only
                let confirmed = crTrustRevision r == Just rev
                go <- if trusted || crYes r || confirmed then pure True else maybe (askTrust d t target steps) (const (pure False)) (crTrustRevision r)
                if
                  | not go, Just _ <- crTrustRevision r ->
                      pure (Left ("Template `" <> tplName t <> "`: the template changed since it was confirmed (now at " <> rev <> "); nothing was written. Confirm its steps again."))
                  | not go -> pure (Left ("Template `" <> tplName t <> "` was not trusted; nothing was written. Pass --yes to run its steps."))
                  | otherwise -> do
                    written <- writeProject target t sourceText rev vals steps sample files (crName r)
                    case written of
                      Left e -> pure (Left e)
                      Right () -> do
                        unless trusted (recordTrustRevision (cdTrustFile d) (tplName t) rev)
                        cdSay d ("(^_^)b Wrote " <> T.pack (show (length files)) <> " files from template " <> tplName t <> " to " <> T.pack target)
                        if crBuild r
                          then fmap (const target) <$> runBuild d target t {tplSteps = steps, tplSample = sample}
                          else do
                            cdSay d ("Not built; build it with: fisher project build " <> T.pack target)
                            pure (Right target)

askTrust :: CreateDeps -> Template -> FilePath -> [Step] -> IO Bool
askTrust d t target steps = do
  let origin = case tplOrigin t of
        Configured src -> " (" <> src <> ")"
        Source src -> " (" <> src <> ")"
        BuiltIn -> ""
  cdSay d ("Template `" <> tplName t <> "`" <> origin <> " runs these steps in " <> T.pack target <> ":")
  forM_ steps $ \s -> cdSay d ("  " <> stepName s <> ": " <> T.unwords (stepRun s))
  cdAsk d "Run these steps? [y/N] "

-- | Write the files and the manifest. Directories are made as needed.
writeProject :: FilePath -> Template -> Text -> Text -> [(Text, Text)] -> [Step] -> FilePath -> [(FilePath, BS.ByteString)] -> Maybe Text -> IO (Either Text ())
writeProject target t sourceText rev vals steps sample files name = do
  createDirectoryIfMissing True target
  forM_ files $ \(rel, bytes) -> do
    createDirectoryIfMissing True (takeDirectory (target </> rel))
    BS.writeFile (target </> rel) bytes
  r <- try (initializeProject target defaultInitOptions
    { initName = name
    , initLeanRoot = Just target
    , initProver = Just (tplProver t)
    , initSourceMode = "template"
    , initTemplateSource = sourceText
    , initBlueprintMarkers = Just [] }) :: IO (Either ProjectError Project)
  case r of
    Left e -> pure (Left (projectErrorMessage e))
    Right p -> do
      let mpath = projectManifestPath p
      Y.decodeFileEither mpath >>= \case
        Right (Object o) -> do
          let section = object
                [ "name" .= tplName t
                , "revision" .= rev
                , "origin" .= originName (tplOrigin t)
                , "variables" .= object [K.fromText k .= v | (k, v) <- vals]
                , "steps" .= [object ["name" .= stepName s, "run" .= stepRun s, "timeout_minutes" .= stepTimeoutMinutes s] | s <- steps]
                , "sample" .= T.pack sample ]
          BS.writeFile mpath (renderManifest (Object (KM.insert "template" section o)))
          pure (Right ())
        Right _ -> pure (Left ("cannot read " <> T.pack mpath))
        Left e -> pure (Left ("cannot read " <> T.pack mpath <> ": " <> T.pack (Y.prettyPrintParseException e)))

originName :: Origin -> Text
originName = \case
  BuiltIn -> "builtin"
  Configured _ -> "configured"
  Source _ -> "source"

-- | Nothing when the target can be created in: it does not exist, or it is
-- a directory holding nothing but @.DS_Store@.
targetProblem :: FilePath -> IO (Maybe Text)
targetProblem target = do
  isDir <- doesDirectoryExist target
  exists <- doesPathExist target
  if
    | isDir -> do
        entries <- listDirectory target
        pure (if all (== ".DS_Store") entries then Nothing else Just notEmpty)
    | exists -> pure (Just notEmpty)
    | otherwise -> pure Nothing
  where
    notEmpty = "Target is not empty: " <> T.pack target

-- | @elan@ and @lake@ on the environment's PATH; the first missing one with
-- the fix @fisher doctor@ gives for it.
preflight :: [(String, String)] -> IO (Either Text ())
preflight env = go ["elan", "lake"]
  where
    go [] = pure (Right ())
    go (tool : rest) =
      whichEnv env tool >>= \case
        Just _ -> go rest
        Nothing -> pure (Left (T.pack tool <> " is not on PATH: " <> binaryFix Lean4 (T.pack tool)))

-- | The message for a build stopped by Ctrl-C (or a stop from the UI).
interruptedMessage :: FilePath -> Text
interruptedMessage root = interruptedLead <> T.pack root

-- | Whether a failure is an interrupt's: one is pending, or the failure is
-- a process killed by SIGINT or SIGTERM (the UI's stop signals the step
-- and this process together, and the step can die first), and one comes
-- within two seconds.
interruptComing :: IORef Bool -> Bool -> IO Bool
interruptComing pending signalled = readIORef pending >>= \case
  True -> pure True
  False | signalled -> poll (40 :: Int)
        | otherwise -> pure False
  where
    poll n
      | n <= 0 = readIORef pending
      | otherwise = readIORef pending >>= \p -> if p then pure True else threadDelay 50000 >> poll (n - 1)

-- | Exit codes of a process killed by SIGINT or SIGTERM, as the step runner
-- and 'runIn' report them.
signalExits :: [Int]
signalExits = [-2, -15, 130, 143]

-- | The remaining steps, then the check; the events file gets the check and
-- @ready@ or @failed@. Ctrl-C (or SIGTERM) in either stops it with only
-- @failed@ @interrupted@ (not also the failure it caused), and the project
-- can be built again.
runBuild :: CreateDeps -> FilePath -> Template -> IO (Either Text ())
runBuild d root t = do
  pending <- newIORef False
  r <- catchJust (\e -> if e == UserInterrupt then Just () else Nothing) (Right <$> interruptibleWith pending (body pending)) (\() -> pure (Left ()))
  case r of
    Left () -> do
      writeEvent root (Failed "interrupted")
      pure (Left (interruptedMessage root))
    Right x -> pure x
  where
    -- the interrupt is on its way: wait for it here, where it is caught
    awaitInterrupt fallback = threadDelay 10000000 >> pure fallback
    body pending =
      runStepsWith (cdScale d) root (tplSteps t) (cdSay d . ("  " <>)) >>= \case
        Left e -> do
          evs <- readEvents root
          let signalled = case reverse evs of
                StepEnd _ n _ : _ -> n `elem` signalExits
                _ -> False
          stopped <- interruptComing pending signalled
          if stopped
            then awaitInterrupt (Left e)
            else do
              writeEvent root (Failed e)
              pure (Left e)
        Right () -> do
          cdSay d ("  check: " <> T.pack (tplSample t))
          cdCheck d t root >>= \case
            Left problems -> do
              let signalled = or [("exit " <> T.pack (show n) <> ")") `T.isInfixOf` p || ("exited with " <> T.pack (show n)) `T.isInfixOf` p | p <- problems, n <- signalExits]
              stopped <- interruptComing pending signalled
              let failure = T.intercalate "\n  " $
                    ("The sample check failed for " <> T.pack (tplSample t) <> ":") : problems
                      <> ["see .fisher/create.log; rerun with fisher project build " <> T.pack root]
              if stopped
                then awaitInterrupt (Left failure)
                else do
                  writeEvent root (CheckEnd False problems)
                  writeEvent root (Failed "the sample check failed")
                  pure (Left failure)
            Right () -> do
              writeEvent root (CheckEnd True [])
              writeEvent root Ready
              let sample = T.pack (root </> tplSample t)
              cdSay d ("Ready: " <> T.pack root)
              cdSay d ("Next: fisher search " <> sample <> " or /prove " <> sample)
              pure (Right ())

-- | Run the steps not yet done and the check again, from what the manifest
-- recorded at create. A custom template's steps are asked about as create
-- asks, unless @yes@.
buildProjectWith :: CreateDeps -> Bool -> FilePath -> IO (Either Text ())
buildProjectWith d yes root0 = do
  root <- makeAbsolute root0
  let mpath = root </> fisherDirName </> "project.yaml"
  exists <- doesFileExist mpath
  if not exists
    then pure (Left ("No Fisher project at " <> T.pack root <> "; make one with fisher project create"))
    else Y.decodeFileEither mpath >>= \case
      Left e -> pure (Left ("cannot read .fisher/project.yaml: " <> T.unwords (T.words (T.pack (Y.prettyPrintParseException e)))))
      Right v -> case recorded root v of
        Nothing -> pure (Left (T.pack root <> " was not made from a template; there is nothing to build"))
        Just (t, vals) ->
          preflight (cdEnv d) >>= \case
            Left e -> pure (Left e)
            Right () -> do
              known <- knownSteps d t vals
              let key = "project:" <> T.pack root
                  hash = stepsHash (tplSteps t)
              trusted <- if known then pure True else isTrustedRevision (cdTrustFile d) key hash
              go <- if trusted || yes then pure True else askTrust d t root (tplSteps t)
              if not go
                then pure (Left ("The steps recorded for `" <> tplName t <> "` are not trusted; nothing was run. Pass --yes to run them."))
                else do
                  unless trusted (recordTrustRevision (cdTrustFile d) key hash)
                  runBuild d root t

-- | Whether the recorded steps are a trusted template's own: the recorded
-- name is a template here (built-in or configured), built-in or trusted at
-- its current revision, and its steps rendered with the recorded variables
-- are exactly the recorded ones. What the manifest says of its origin
-- counts for nothing.
knownSteps :: CreateDeps -> Template -> [(Text, Text)] -> IO Bool
knownSteps d t vals =
  findTemplate (cdConfig d) (tplName t) >>= \case
    Left _ -> pure False
    Right tpl
      | renderSteps tpl vals /= Right (tplSteps t) -> pure False
      | otherwise -> isTrusted (cdTrustFile d) tpl

-- | What trust in one project's recorded steps is kept under.
stepsHash :: [Step] -> Text
stepsHash steps = sha256Hex (TE.encodeUtf8 (T.pack (show [(stepName s, stepRun s, stepTimeoutMinutes s) | s <- steps])))

-- | The template a manifest records (name, kind, steps, sample, with the
-- template source as its origin) and its variables.
recorded :: FilePath -> Value -> Maybe (Template, [(Text, Text)])
recorded root = \case
  Object o -> do
    Object tpl <- KM.lookup "template" o
    String name <- KM.lookup "name" tpl
    String sample <- KM.lookup "sample" tpl
    Array steps <- KM.lookup "steps" tpl
    kind <- case KM.lookup "kind" o of
      Just (String k) -> proverByName k
      _ -> Just Lean4
    ss <- mapM step (V.toList steps)
    let text k m = case KM.lookup k m of
          Just (String x) -> x
          _ -> ""
        source = case KM.lookup "source" o of
          Just (Object src) -> text "template_source" src
          _ -> ""
    pure
      ( Template
          { tplName = name, tplProver = kind, tplDescription = "", tplVariables = [], tplFiles = []
          , tplSteps = ss, tplSample = T.unpack sample, tplOrigin = Source source, tplRoot = root }
      , case KM.lookup "variables" tpl of
          Just (Object vs) -> [(K.toText k, x) | (k, String x) <- KM.toList vs]
          _ -> [] )
  _ -> Nothing
  where
    step = \case
      Object s -> do
        String n <- KM.lookup "name" s
        Array run <- KM.lookup "run" s
        argv <- mapM (\case String a -> Just a; _ -> Nothing) (V.toList run)
        let minutes = case KM.lookup "timeout_minutes" s of
              Just (Number m) -> round m
              _ -> defaultTimeoutMinutes
        pure (Step n argv minutes)
      _ -> Nothing

-- | The sample check: @lake build@, then the search's probe of the sample
-- with its theorems: no errors, no @sorry@, no axiom beyond the standard
-- three. The template's 'tplSample' is the path in the project.
checkSample :: Template -> FilePath -> IO (Either [Text] ())
checkSample = checkSampleWith "lake" (30 * 60)

-- | 'checkSample' with the @lake@ to run and a time limit in seconds for
-- each of the two runs.
checkSampleWith :: FilePath -> Int -> Template -> FilePath -> IO (Either [Text] ())
checkSampleWith lake secs t root = do
  removeStaleProbes (takeDirectory (root </> tplSample t))
  (code, out) <- runIn lake secs root ["build"]
  appendRun (logFile root) "check" "lake build" code out
  if code /= 0
    then pure (Left (("lake build failed (exit " <> T.pack (show code) <> "); see .fisher/create.log") : take 5 (lakeErrorLines out)))
    else do
      let rel = tplSample t
      text <- TE.decodeUtf8With TEE.lenientDecode <$> BS.readFile (root </> rel)
      case sampleTheorems text of
        [] -> pure (Left ["no theorem in " <> T.pack rel])
        names -> do
          problems <- probeProblems lake secs (logFile root) "check" root rel text names
          pure (if null problems then Right () else Left problems)

-- | The full names of a file's theorems and lemmas: @theorem NAME@ or
-- @lemma NAME@, after any @\@[...]@ attributes and modifiers (@private@,
-- @protected@, ...), qualified by the @namespace@s open there (@end@ closes
-- the innermost namespace or section; @_root_.@ opts out).
sampleTheorems :: Text -> [Text]
sampleTheorems = go [] . T.lines
  where
    go _ [] = []
    go stack (l : ls) = case T.words l of
      ("namespace" : n : _) -> go (Just n : stack) ls
      ("section" : _) -> go (Nothing : stack) ls
      ("end" : _) -> go (drop 1 stack) ls
      ws -> case declName (dropModifiers (dropAttributes ws)) of
        Just n -> qualify stack n : go stack ls
        Nothing -> go stack ls
    dropAttributes = \case
      (w : rest) | "@[" `T.isPrefixOf` w ->
        if "]" `T.isInfixOf` w then dropAttributes rest else dropAttributes (drop 1 (dropWhile (not . T.isInfixOf "]") rest))
      ws -> ws
    dropModifiers = dropWhile (`elem` ["private", "protected", "noncomputable", "nonrec", "unsafe", "partial"])
    declName = \case
      (kw : n : _) | kw `elem` ["theorem", "lemma"] ->
        let name = T.takeWhile (\c -> c /= ':' && c /= '(' && c /= '{' && c /= '[' && c /= '⦃') n
         in if T.null name then Nothing else Just name
      _ -> Nothing
    qualify stack n = case T.stripPrefix "_root_." n of
      Just bare -> bare
      Nothing -> T.intercalate "." (reverse [x | Just x <- stack] <> [n])
