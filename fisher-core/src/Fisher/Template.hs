{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Templates for new prover environments: the manifest
-- (@fisher-template.yaml@), its variables and their rendering, the built-ins
-- shipped in the binary, and the templates named in the configuration
-- (@fisher.templates.<name>: <path or git URL>@).
--
-- A template is a directory with the manifest at its root. Only the files
-- the manifest lists are copied; @{{var}}@ in their contents, in their paths,
-- in the steps' argv and in the sample path is replaced by the variable's
-- value. The variable @project@ is always present. @{{{{@ renders as a
-- literal @{{@ and @}}}}@ as @}}@, for a file that needs double braces of
-- its own (a Lean set of sets, say). In file contents a line holding only
-- @{{#if var}}@ starts a guarded block that a line holding only @{{/if}}@
-- ends: the lines between are kept unless the variable's value is @none@
-- (or empty), and the two guard lines are always dropped. Guards do not
-- nest. Symbolic links in a template are refused, so nothing outside it is
-- copied.
--
-- The built-ins live under @data/templates/<name>@ in this package. A
-- @__var__@ in a path there stands for @{{var}}@ (braces are glob syntax for
-- Cabal), and they are written out to
-- @$FISHER_HOME/fisher/templates-builtin/<name>-<hash>@ on use (the hash is
-- of the embedded files, so a directory is never changed once written), so
-- every template's 'tplRoot' is a directory.
module Fisher.Template
  ( -- * The model
    Variable (..)
  , Step (..)
  , Template (..)
  , Origin (..)
  , TemplateError (..)
  , manifestName
  , defaultTimeoutMinutes
  , projectPattern
    -- * Loading
  , parseTemplate
  , loadTemplate
  , builtinNames
  , builtinTemplates
  , listTemplates
  , listTemplatesWithNotes
  , findTemplate
  , templateCacheDir
  , cloneStamp
    -- * Variables and rendering
  , resolveVariables
  , reservedProjectNames
  , renderText
  , renderGuards
  , renderFiles
  , renderSteps
  , renderSample
    -- * Releases and revisions
  , latestRelease
  , latestReleaseFrom
  , releaseRepository
  , pickLatest
  , templateRevision
  ) where

import Control.Exception (Exception, IOException, SomeException, throwIO, try)
import Control.Monad (forM, forM_, unless, when)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.FileEmbed (embedDir, makeRelativeToProject)
import Data.List (isPrefixOf, nub, sort, sortOn)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import qualified Data.Scientific as Sci
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (diffUTCTime, getCurrentTime)
import qualified Data.Vector as V
import qualified Data.Yaml as Y
import System.Directory
  ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist, doesPathExist, getHomeDirectory
  , getModificationTime, listDirectory, makeAbsolute, pathIsSymbolicLink, removeDirectoryRecursive
  , renameDirectory )
import System.Exit (ExitCode (..))
import System.Posix.Process (getProcessID)
import System.FilePath (joinPath, splitDirectories, takeDirectory, (</>))
import System.Process.Typed (nullStream, proc, readProcess, setStdin)
import System.Timeout (timeout)

import Fisher.Atomic (atomicWriteFile)
import Fisher.Paths (expandUser, getFisherHome)
import Fisher.Prover (ProverKind (..))
import Fisher.Py.Re (compile, fullMatch)
import Fisher.Sha256 (sha256Hex)

-- * The model

-- | A variable the template's files may name as @{{name}}@. @pattern@ is a
-- regular expression the whole value must match; an empty @default@ for
-- @project@ means the target directory's base name.
data Variable = Variable
  { varName :: Text
  , varDefault :: Text
  , varPattern :: Text
  , varDescription :: Text
  }
  deriving (Eq, Show)

-- | A build step: run in the project directory, with a time limit.
data Step = Step
  { stepName :: Text
  , stepRun :: [Text]
  , stepTimeoutMinutes :: Int
  }
  deriving (Eq, Show)

-- | Where a template came from: shipped in the binary, named in the
-- configuration (the configured path or URL), or given as a source on the
-- command line.
data Origin = BuiltIn | Configured Text | Source Text
  deriving (Eq, Show)

data Template = Template
  { tplName :: Text
  , tplProver :: ProverKind
  , tplDescription :: Text
  , tplVariables :: [Variable]
  , tplFiles :: [FilePath]
  -- ^ the files to copy, relative to 'tplRoot', globs expanded, not rendered
  , tplSteps :: [Step]
  , tplSample :: FilePath
  -- ^ one of 'tplFiles', not rendered
  , tplOrigin :: Origin
  , tplRoot :: FilePath
  -- ^ the directory the files are read from
  }
  deriving (Eq, Show)

-- | A template that cannot be used, as a one-line message.
newtype TemplateError = TemplateError Text
  deriving (Show)

instance Exception TemplateError

manifestName :: FilePath
manifestName = "fisher-template.yaml"

defaultTimeoutMinutes :: Int
defaultTimeoutMinutes = 30

-- | The pattern of @project@ unless the manifest gives its own.
projectPattern :: Text
projectPattern = "[A-Za-z][A-Za-z0-9_]*"

projectVariable :: Variable
projectVariable = Variable "project" "" projectPattern "The project name (default: the target directory's name)"

-- * Placeholders

-- | Text or a @{{name}}@.
data Piece = Lit Text | Ref Text

-- | The text split into literals and placeholders; @{{{{@ and @}}}}@ are
-- the literal @{{@ and @}}@.
pieces :: Text -> [Piece]
pieces t = case T.breakOn "{{" t of
  (pre, rest)
    | T.null rest -> lit pre
    | "{{{{" `T.isPrefixOf` rest -> lit pre <> [Lit "{{"] <> pieces (T.drop 4 rest)
    | otherwise ->
        let after = T.drop 2 rest
            (name, close) = T.span isIdentChar after
         in if isIdent name && "}}" `T.isPrefixOf` close
              then lit pre <> (Ref name : pieces (T.drop 2 close))
              else lit (pre <> "{") <> pieces (T.drop 1 rest)
  where
    lit x = [Lit (T.replace "}}}}" "}}" x) | not (T.null x)]

refs :: Text -> [Text]
refs t = [n | Ref n <- pieces t]

isIdentChar :: Char -> Bool
isIdentChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '_'

isIdent :: Text -> Bool
isIdent n = case T.uncons n of
  Just (c, rest) -> (isAsciiLower c || isAsciiUpper c || c == '_') && T.all isIdentChar rest
  Nothing -> False

-- | Every @{{name}}@ replaced by its value; one with no value is refused.
renderText :: [(Text, Text)] -> Text -> Either Text Text
renderText vals t = T.concat <$> mapM one (pieces t)
  where
    one (Lit s) = Right s
    one (Ref n) = maybe (Left ("No value for `{{" <> n <> "}}`")) Right (lookup n vals)

-- | The guard a line holds: @Just (Just var)@ for @{{#if var}}@, @Just
-- Nothing@ for @{{/if}}@, else Nothing.
guardLine :: Text -> Maybe (Maybe Text)
guardLine l = case T.stripSuffix "}}" =<< T.stripPrefix "{{" (T.strip l) of
  Just "/if" -> Just Nothing
  Just inner
    | Just n <- T.stripPrefix "#if " inner, isIdent (T.strip n) -> Just (Just (T.strip n))
  _ -> Nothing

-- | The variables the guards in a text name.
guardRefs :: Text -> [Text]
guardRefs t = [n | l <- T.splitOn "\n" t, Just (Just n) <- [guardLine l]]

-- | The guarded blocks resolved (see the module header): a block whose
-- variable is @none@ or empty is dropped, the guard lines always. An
-- unclosed block, a stray @{{/if}}@, a nested guard or a variable with no
-- value is refused.
renderGuards :: [(Text, Text)] -> Text -> Either Text Text
renderGuards vals t
  | not (any (isJust . guardLine) ls) = Right t
  | otherwise = T.intercalate "\n" <$> go Nothing ls
  where
    ls = T.splitOn "\n" t
    -- the open block: Nothing outside one, Just keep inside
    go open [] = maybe (Right []) (const (Left "An `{{#if ...}}` block has no `{{/if}}`")) open
    go open (l : rest) = case (guardLine l, open) of
      (Just (Just n), Nothing) -> case lookup n vals of
        Nothing -> Left ("No value for `{{#if " <> n <> "}}`")
        Just v -> go (Just (v `notElem` ["", "none"])) rest
      (Just (Just _), Just _) -> Left "`{{#if ...}}` blocks do not nest"
      (Just Nothing, Nothing) -> Left "`{{/if}}` with no `{{#if ...}}`"
      (Just Nothing, Just _) -> go Nothing rest
      (Nothing, Just False) -> go open rest
      (Nothing, _) -> (l :) <$> go open rest

-- * Parsing

-- | The template at a root, from its manifest's bytes: the fields checked,
-- the listed files expanded and present, and every @{{name}}@ naming a
-- variable. The origin is 'Source' (the root); 'loadTemplate' sets another.
parseTemplate :: FilePath -> ByteString -> IO (Either Text Template)
parseTemplate root bytes = case Y.decodeEither' bytes of
  Left e -> pure (Left (where_ <> ": " <> T.strip (T.pack (Y.prettyPrintParseException e))))
  Right v -> case fields v of
    Left e -> pure (Left (where_ <> ": " <> e))
    Right (mk, patterns, sample) -> do
      expanded <- expandFiles root patterns
      case expanded of
        Left e -> pure (Left (where_ <> ": " <> e))
        Right files -> do
          let t = mk files
          checked <- checkTemplate t sample
          pure (either (Left . ((where_ <> ": ") <>)) Right checked)
  where
    where_ = T.pack (root </> manifestName)
    fields v = do
      o <- case v of
        Object o -> Right o
        _ -> Left "the manifest is not a mapping"
      name <- reqText o "name"
      unless (validName name) $
        Left ("field `name` (" <> name <> ") must be lower case letters, digits and -")
      proverName <- reqText o "prover"
      kind <- case proverName of
        "lean4" -> Right Lean4
        p | p `elem` ["rocq", "isabelle"] -> Left ("field `prover`: " <> p <> " is not supported yet (only lean4 for now)")
        p -> Left ("field `prover`: unknown prover `" <> p <> "` (expected lean4)")
      desc <- optText o "description"
      vars <- variables o
      patterns <- case KM.lookup "files" o of
        Just (Array a) | not (V.null a) -> mapM (asText "files") (V.toList a)
        Just (Array _) -> Left "field `files` is empty"
        Just _ -> Left "field `files` must be a list of paths"
        Nothing -> Left "missing field `files`"
      steps <- case KM.lookup "steps" o of
        Nothing -> Right []
        Just Null -> Right []
        Just (Array a) -> mapM step (V.toList a)
        Just _ -> Left "field `steps` must be a list"
      sample <- reqText o "sample"
      pure
        ( \files -> Template name kind (T.strip desc) vars files steps (T.unpack sample) (Source (T.pack root)) root
        , map T.unpack patterns
        , sample )
    variables o = do
      declared <- case KM.lookup "variables" o of
        Nothing -> Right []
        Just Null -> Right []
        Just (Array a) -> mapM variable (V.toList a)
        Just _ -> Left "field `variables` must be a list"
      let names = map varName declared
      case [n | n <- names, length (filter (== n) names) > 1] of
        (n : _) -> Left ("variable `" <> n <> "` is declared twice")
        [] -> Right ()
      let given = lookup "project" [(varName d, d) | d <- declared]
          project = case given of
            Nothing -> projectVariable
            Just d ->
              projectVariable
                { varPattern = if T.null (varPattern d) then projectPattern else varPattern d
                , varDescription = if T.null (varDescription d) then varDescription projectVariable else varDescription d }
      pure (project : filter ((/= "project") . varName) declared)
    variable = \case
      Object o -> do
        n <- reqText o "name"
        unless (isIdent n) $ Left ("variable name `" <> n <> "` must be letters, digits and _")
        d <- scalarOr o "default"
        p <- optText o "pattern"
        unless (T.null p) $ case compile [] p of
          Left e -> Left ("variable `" <> n <> "`: bad pattern `" <> p <> "` (" <> T.pack e <> ")")
          Right _ -> Right ()
        desc <- optText o "description"
        pure (Variable n d p desc)
      _ -> Left "each entry of `variables` must be a mapping with a `name`"
    step = \case
      Object o -> do
        n <- reqText o "name"
        run <- case KM.lookup "run" o of
          Just (Array a) | not (V.null a) -> mapM (asText ("run` of step `" <> n)) (V.toList a)
          _ -> Left ("step `" <> n <> "`: field `run` must be a non-empty list")
        limit <- case KM.lookup "timeout_minutes" o of
          Nothing -> Right defaultTimeoutMinutes
          Just Null -> Right defaultTimeoutMinutes
          Just (Number x) | Just i <- Sci.toBoundedInteger x, i > 0 -> Right i
          Just _ -> Left ("step `" <> n <> "`: field `timeout_minutes` must be a positive whole number")
        pure (Step n run limit)
      _ -> Left "each entry of `steps` must be a mapping with a `name` and `run`"

reqText :: KM.KeyMap Value -> Text -> Either Text Text
reqText o k = case KM.lookup (K.fromText k) o of
  Just (String s) | not (T.null (T.strip s)) -> Right (T.strip s)
  Just (String _) -> Left ("field `" <> k <> "` is empty")
  Just _ -> Left ("field `" <> k <> "` must be text")
  Nothing -> Left ("missing field `" <> k <> "`")

optText :: KM.KeyMap Value -> Text -> Either Text Text
optText o k = case KM.lookup (K.fromText k) o of
  Just (String s) -> Right s
  Just Null -> Right ""
  Nothing -> Right ""
  Just _ -> Left ("field `" <> k <> "` must be text")

-- | Text, or a number or boolean as YAML wrote it.
scalarOr :: KM.KeyMap Value -> Text -> Either Text Text
scalarOr o k = case KM.lookup (K.fromText k) o of
  Just (String s) -> Right s
  Just (Number x) -> Right (either (T.pack . show) (T.pack . show) (Sci.floatingOrInteger x :: Either Double Integer))
  Just (Bool b) -> Right (if b then "true" else "false")
  Just Null -> Right ""
  Nothing -> Right ""
  Just _ -> Left ("field `" <> k <> "` must be text")

-- | A list entry as text; a number or boolean as YAML wrote it.
asText :: Text -> Value -> Either Text Text
asText k = \case
  String s | not (T.null s) -> Right s
  v@(Number _) -> scalarOr (KM.singleton "v" v) "v"
  v@(Bool _) -> scalarOr (KM.singleton "v" v) "v"
  _ -> Left ("field `" <> k <> "` must hold text entries")

validName :: Text -> Bool
validName n = not (T.null n) && T.all (\c -> isAsciiLower c || isDigit c || c == '-') n

-- | Every @{{name}}@ in the files (their paths and their UTF-8 contents), the
-- steps and the sample names a variable; the sample is one of the files.
checkTemplate :: Template -> Text -> IO (Either Text Template)
checkTemplate t sample = do
  let known = map varName (tplVariables t)
      bad where_ txt = [(n, where_) | n <- refs txt <> guardRefs txt, n `notElem` known]
  fromContents <- forM (tplFiles t) $ \f -> do
    bytes <- BS.readFile (tplRoot t </> f)
    pure $ case TE.decodeUtf8' bytes of
      Right txt -> bad (T.pack f) txt
      Left _ -> []
  let fromPaths = concat [bad ("the path " <> T.pack f) (T.pack f) | f <- tplFiles t]
      fromSteps = concat [bad ("step `" <> stepName s <> "`") a | s <- tplSteps t, a <- stepRun s]
      fromSample = bad "field `sample`" sample
  pure $ case fromPaths <> concat fromContents <> fromSteps <> fromSample of
    ((n, where_) : _) -> Left ("`{{" <> n <> "}}` in " <> where_ <> " names no variable")
    []
      | tplSample t `notElem` tplFiles t -> Left ("field `sample` (" <> sample <> ") is not one of the files")
      | otherwise -> Right t

-- | The listed paths and globs as files under the root, in order, each once.
-- A path naming a directory takes the files under it.
expandFiles :: FilePath -> [FilePath] -> IO (Either Text [FilePath])
expandFiles root patterns = do
  needWalk <- or <$> forM patterns (\p -> if isGlob p then pure True else doesDirectoryExist (root </> stripDot p))
  walked <- if needWalk then walk root else pure (Right [])
  case walked of
    Left e -> pure (Left e)
    Right allFiles -> do
      results <- forM patterns $ \p0 -> do
        let p = stripDot p0
        if escapes p
          then pure (Left ("file `" <> T.pack p0 <> "` in `files` leaves the template"))
          else
            if isGlob p
              then pure $ case filter (globMatch (splitDirectories p) . splitDirectories) allFiles of
                [] -> Left ("pattern `" <> T.pack p0 <> "` in `files` matches no file")
                ms -> Right ms
              else do
                linked <- linkOnPath root p
                isFile <- doesFileExist (root </> p)
                isDir <- doesDirectoryExist (root </> p)
                pure $ case linked of
                  Just l -> Left (symlinkMessage l)
                  Nothing
                    | isFile -> Right [p]
                    | isDir -> case filter ((splitDirectories p `isPrefixOf`) . splitDirectories) allFiles of
                        [] -> Left ("directory `" <> T.pack p0 <> "` in `files` holds no file")
                        ms -> Right ms
                    | otherwise -> Left ("file `" <> T.pack p0 <> "` listed in `files` does not exist")
      pure (nub . concat <$> sequence results)
  where
    stripDot p = if "./" `isPrefixOf` p then stripDot (drop 2 p) else p

escapes :: FilePath -> Bool
escapes p = null p || "/" `isPrefixOf` p || ".." `elem` splitDirectories p

isGlob :: FilePath -> Bool
isGlob = any (`elem` ("*?" :: String))

-- | Glob segments against path segments: @**@ is any number of directories,
-- @*@ and @?@ stay within one name.
globMatch :: [String] -> [String] -> Bool
globMatch [] [] = True
globMatch ("**" : ps) xs = globMatch ps xs || (not (null xs) && globMatch ("**" : ps) (drop 1 xs))
globMatch (p : ps) (x : xs) = nameMatch p x && globMatch ps xs
globMatch _ _ = False

nameMatch :: String -> String -> Bool
nameMatch [] [] = True
nameMatch ('*' : ps) xs = nameMatch ps xs || (not (null xs) && nameMatch ('*' : ps) (drop 1 xs))
nameMatch ('?' : ps) (_ : xs) = nameMatch ps xs
nameMatch (p : ps) (x : xs) = p == x && nameMatch ps xs
nameMatch _ _ = False

-- | The first symbolic link among the path's leading parts and itself.
linkOnPath :: FilePath -> FilePath -> IO (Maybe FilePath)
linkOnPath root p = go [] (splitDirectories p)
  where
    go _ [] = pure Nothing
    go done (x : xs) = do
      let rel = joinPath (done <> [x])
      exists <- doesPathExist (root </> rel) `orLink` (root </> rel)
      isLink <- if exists then pathIsSymbolicLink (root </> rel) else pure False
      if isLink then pure (Just rel) else go (done <> [x]) xs
    -- A dangling link does not "exist", but it is still a link.
    orLink check path = do
      e <- check
      if e then pure True else either (const False) id <$> (try (pathIsSymbolicLink path) :: IO (Either IOException Bool))

symlinkMessage :: FilePath -> Text
symlinkMessage l = "`" <> T.pack l <> "` is a symbolic link; templates may not hold links"

-- | Files under the root, relative and sorted; the manifest and @.git@ left
-- out. A symbolic link anywhere (outside a @.git@) is refused.
walk :: FilePath -> IO (Either Text [FilePath])
walk root = fmap (sort . filter (/= manifestName)) <$> go ""
  where
    go rel = do
      names <- listDirectory (root </> rel)
      fmap (fmap concat . sequence) . forM (sort names) $ \n -> do
        let r = if null rel then n else rel </> n
        isLink <- pathIsSymbolicLink (root </> r)
        isDir <- doesDirectoryExist (root </> r)
        if
          | n == ".git" -> pure (Right [])
          | isLink -> pure (Left (symlinkMessage r))
          | isDir -> go r
          | otherwise -> pure (Right [r])

-- | 'parseTemplate' of the manifest in a directory, with its origin.
loadTemplate :: Origin -> FilePath -> IO (Either Text Template)
loadTemplate origin root = do
  present <- doesFileExist (root </> manifestName)
  if not present
    then pure (Left (T.pack root <> " has no " <> T.pack manifestName))
    else do
      parsed <- BS.readFile (root </> manifestName) >>= parseTemplate root
      pure ((\t -> t {tplOrigin = origin}) <$> parsed)

-- * The built-ins

builtinData :: [(FilePath, ByteString)]
builtinData = $(makeRelativeToProject "data/templates" >>= embedDir)

-- | In the order they are listed.
builtinNames :: [Text]
builtinNames = ["lean", "lean-mathlib"]

-- | @__var__@ in a stored path back to @{{var}}@ (the name letters and
-- digits only).
unescapePath :: Text -> Text
unescapePath t = case T.breakOn "__" t of
  (pre, rest)
    | T.null rest -> pre
    | otherwise ->
        let (name, close) = T.span (\c -> isAsciiLower c || isAsciiUpper c || isDigit c) (T.drop 2 rest)
         in case T.stripPrefix "__" close of
              Just after | not (T.null name) -> pre <> "{{" <> name <> "}}" <> unescapePath after
              _ -> pre <> "_" <> unescapePath (T.drop 1 rest)

-- | The built-ins written out and loaded. Each goes to
-- @$FISHER_HOME/fisher/templates-builtin/<name>-<hash>@, where the hash is
-- the first 8 hex digits of the sha256 of its embedded files: built in a
-- temporary sibling and renamed into place, so a directory that exists is
-- complete and is never written again (another process may be reading it).
-- A rename that finds the directory already there (a race) counts as done.
-- Directories of older versions are left alone.
builtinTemplates :: IO [Template]
builtinTemplates = do
  base <- (</> "fisher" </> "templates-builtin") <$> getFisherHome
  forM builtinNames $ \name -> do
    let prefix = T.unpack name <> "/"
        mine = sortOn fst
          [ (T.unpack (unescapePath (T.pack rel)), bytes)
          | (p, bytes) <- builtinData, Just rel <- [stripPrefixPath prefix p] ]
        digest = T.take 8 (sha256Hex (BS.concat [BC.pack f <> "\0" <> BC.pack (show (BS.length b')) <> "\0" <> b' | (f, b') <- mine]))
        dir = base </> (T.unpack name <> "-" <> T.unpack digest)
    done <- doesDirectoryExist dir
    unless done $ do
      createDirectoryIfMissing True base
      pid <- getProcessID
      stamp <- getPOSIXTime
      let tmp = base </> (".tmp-" <> T.unpack name <> "-" <> T.unpack digest <> "-" <> show pid <> "-" <> show (floor (stamp * 1000000) :: Integer))
      forM_ mine $ \(rel, bytes) -> do
        createDirectoryIfMissing True (takeDirectory (tmp </> rel))
        BS.writeFile (tmp </> rel) bytes
      moved <- try (renameDirectory tmp dir) :: IO (Either IOException ())
      case moved of
        Right () -> pure ()
        Left e -> do
          there <- doesDirectoryExist dir
          _ <- try (removeDirectoryRecursive tmp) :: IO (Either IOException ())
          unless there $ throwIO e
    loadTemplate BuiltIn dir >>= either (throwIO . TemplateError . (("Built-in template " <> name <> " is broken: ") <>)) pure
  where
    stripPrefixPath pre p = let p' = map (\c -> if c == '\\' then '/' else c) p in if pre `isPrefixOf` p' then Just (drop (length pre) p') else Nothing

-- * Configured templates

-- | @fisher.templates@ from the configuration, sorted by name.
configured :: Value -> [(Text, Either Text Text)]
configured config = case config of
  Object top | Just (Object f) <- KM.lookup "fisher" top, Just (Object ts) <- KM.lookup "templates" f ->
    sortOn fst
      [ (K.toText k, case v of
            String s | not (T.null (T.strip s)) -> Right (T.strip s)
            _ -> Left ("fisher.templates." <> K.toText k <> " must be a path or a git URL"))
      | (k, v) <- KM.toList ts ]
  _ -> []

-- | @~/.cache/fisher/templates@, where git templates are cloned.
templateCacheDir :: IO FilePath
templateCacheDir = (\h -> h </> ".cache" </> "fisher" </> "templates") <$> getHomeDirectory

-- | Touched in a clone's @.git@ when it was last fetched.
cloneStamp :: FilePath
cloneStamp = ".git" </> "fisher-fetched"

isGitSource :: Text -> Bool
isGitSource s =
  any (`T.isPrefixOf` s) ["https://", "http://", "ssh://", "git://", "file://", "git@"]
    || ".git" `T.isSuffixOf` T.dropWhileEnd (== '/') s

-- | 'loadConfigured'' with any exception turned into its message.
loadConfigured :: Text -> Text -> IO (Either Text Template)
loadConfigured name src =
  (try (loadConfigured' name src) :: IO (Either SomeException (Either Text Template))) >>= \case
    Left e -> pure (Left ("Template `" <> name <> "`: " <> T.unwords (T.words (T.pack (show e)))))
    Right r -> pure r

loadConfigured' :: Text -> Text -> IO (Either Text Template)
loadConfigured' name src
  | not (validName name) = pure (Left ("Template name `" <> name <> "` must be lower case letters, digits and -"))
  | isGitSource src = do
      url <- T.pack <$> expandUser (T.unpack src)
      cloned <- ensureClone name url
      case cloned of
        Left e -> pure (Left e)
        Right dir -> named <$> loadTemplate (Configured src) dir
  | otherwise = do
      dir <- expandUser (T.unpack src)
      isDir <- doesDirectoryExist dir
      if isDir
        then named <$> loadTemplate (Configured src) dir
        else pure (Left ("Template `" <> name <> "`: " <> src <> " is not a directory"))
  where
    named = fmap (\t -> t {tplName = name})

-- | The clone of a git template, made if missing (or cloned from another
-- URL) and fetched again when the last fetch is more than a day old. A
-- failed refresh keeps the clone there is.
ensureClone :: Text -> Text -> IO (Either Text FilePath)
ensureClone name url = do
  dir <- (</> T.unpack name) <$> templateCacheDir
  hasGit <- doesDirectoryExist (dir </> ".git")
  origin <- if hasGit then git (Just dir) ["remote", "get-url", "origin"] else pure (Left "")
  if hasGit && origin == Right url
    then do
      stale <- isStale (dir </> cloneStamp)
      when stale $ do
        fetched <- git (Just dir) ["fetch", "--depth", "1", "--quiet", "origin"]
        case fetched of
          Right _ -> do
            reset <- git (Just dir) ["reset", "--hard", "--quiet", "FETCH_HEAD"]
            either (const (pure ())) (const (touch (dir </> cloneStamp))) reset
          Left _ -> pure ()
      pure (Right dir)
    else do
      there <- doesPathExist dir
      when there $ removeDirectoryRecursive dir
      createDirectoryIfMissing True (takeDirectory dir)
      r <- git Nothing ["clone", "--depth", "1", "--quiet", "--", T.unpack url, dir]
      case r of
        Left e -> pure (Left ("Failed to clone template " <> name <> ": " <> e))
        Right _ -> touch (dir </> cloneStamp) >> pure (Right dir)
  where
    isStale p = do
      e <- doesFileExist p
      if not e
        then pure True
        else do
          m <- getModificationTime p
          now <- getCurrentTime
          pure (diffUTCTime now m > 86400)
    touch p = atomicWriteFile ".stamp_.tmp" p ""

-- | @git ARGS@ (in a directory), stdin closed, no prompts, under ten
-- minutes: its trimmed stdout, or its message on one line.
git :: Maybe FilePath -> [String] -> IO (Either Text Text)
git dir args = do
  let argv = maybe [] (\d -> ["-C", d]) dir <> args
      cfg = setStdin nullStream (proc "git" argv)
  r <- try (timeout (600 * 1000000) (readProcess cfg)) :: IO (Either SomeException (Maybe (ExitCode, BL.ByteString, BL.ByteString)))
  pure $ case r of
    Left e -> Left (oneLine (T.pack (show e)))
    Right Nothing -> Left "git timed out"
    Right (Just (ExitSuccess, out, _)) -> Right (T.strip (decode out))
    Right (Just (ExitFailure c, out, err)) ->
      let msg = oneLine (decode err)
          msg' = if T.null msg then oneLine (decode out) else msg
       in Left (if T.null msg' then "git exited with " <> T.pack (show c) else msg')
  where
    decode = TE.decodeUtf8With (\_ _ -> Just '\xfffd') . BL.toStrict
    oneLine = T.unwords . filter (not . T.null) . map T.strip . T.lines

-- | Built-ins then configured ones; a configured template with a built-in's
-- name takes its place, with a note. A configured one that cannot be loaded
-- is left out, with its error as a note.
listTemplatesWithNotes :: Value -> IO ([Template], [Text])
listTemplatesWithNotes config = do
  builtins <- builtinTemplates
  loaded <- forM (configured config) $ \(name, src) -> (,) name <$> either (pure . Left) (loadConfigured name) src
  let good = [(n, t) | (n, Right t) <- loaded]
      errors = [e | (_, Left e) <- loaded]
      replaced = [n | (n, _) <- good, n `elem` builtinNames]
      fromBuiltins = [fromMaybe b (lookup (tplName b) good) | b <- builtins]
      others = [t | (n, t) <- good, n `notElem` builtinNames]
      notes = ["Configured template `" <> n <> "` replaces the built-in one." | n <- replaced] <> errors
  pure (fromBuiltins <> others, notes)

listTemplates :: Value -> IO [Template]
listTemplates config = fst <$> listTemplatesWithNotes config

-- | One template by name: a configured one first, else a built-in.
findTemplate :: Value -> Text -> IO (Either Text Template)
findTemplate config name = case lookup name (configured config) of
  Just (Left e) -> pure (Left e)
  Just (Right src) -> loadConfigured name src
  Nothing
    | name `elem` builtinNames -> do
        bs <- builtinTemplates
        pure (maybe (Left (unknown name)) Right (lookup name [(tplName b, b) | b <- bs]))
    | otherwise -> pure (Left (unknown name))
  where
    unknown n =
      "Unknown template `" <> n <> "` (known: "
        <> T.intercalate ", " (nub (builtinNames <> map fst (configured config))) <> ")"

-- * Variables

-- | Project names a new project may not take (compared ignoring case):
-- Lean's own packages and Mathlib with its dependencies. A root package of
-- that name makes Lake see a cycle, and on a file system that ignores case
-- its @.olean@ shadows the real one.
reservedProjectNames :: [Text]
reservedProjectNames =
  [ "lean", "init", "std", "lake", "mathlib", "batteries", "aesop", "qq", "proofwidgets", "plausible"
  , "importgraph", "leansearchclient" ]

-- | The value of every variable, in the template's order: @--var@ pairs
-- (the last one of a name wins), else the default; @project@ defaults to the
-- target's base name. Unknown names and values that do not match are
-- refused before anything is resolved; then a @lean@, @mathlib@ or @smt@
-- of @latest@ is resolved, in the template's order, by the given function
-- (called with the variable's name and the values resolved before it, so
-- @smt@ sees the resolved @mathlib@).
resolveVariables :: Template -> FilePath -> [(Text, Text)] -> (Text -> [(Text, Text)] -> IO (Either Text Text)) -> IO (Either Text [(Text, Text)])
resolveVariables t target0 given resolver = do
 target <- normalisePath <$> makeAbsolute target0
 let base = T.pack (lastSegment target)
     value v = case lookup (varName v) (reverse given) of
       Just x -> (x, True)
       Nothing
         | varName v == "project" && T.null (varDefault v) -> (base, False)
         | otherwise -> (varDefault v, False)
     checked v =
       let (x, passed) = value v
        in case matching v x of
             Right _
               | varName v == "project" && T.toLower x `elem` reservedProjectNames ->
                   Left ("project name " <> x <> " clashes with a Lean or Mathlib package; pass --var project=NAME")
               | otherwise -> Right (v, x)
             Left e
               | varName v == "project" && not passed -> Left (e <> "; pass --var project=NAME")
               | otherwise -> Left e
 case [n | (n, _) <- given, n `notElem` names] of
    (n : _) ->
      pure (Left ("Unknown variable `" <> n <> "` for template `" <> tplName t <> "` (known: " <> T.intercalate ", " names <> ")"))
    [] -> case mapM checked (tplVariables t) of
      Left e -> pure (Left e)
      Right vals -> fmap (fmap reverse) (resolveIn [] vals)
  where
    -- the values so far, latest first
    resolveIn done [] = pure (Right done)
    resolveIn done ((v, x) : rest)
      | x == "latest" && varName v `elem` ["lean", "mathlib", "smt"] =
          resolver (varName v) (reverse done) >>= \case
            Left _
              | varName v == "smt" -> pure (Left "could not resolve the latest smt revision; pass --var smt=main")
              | otherwise -> pure (Left ("could not resolve the latest release; pass --var " <> varName v <> "=v4.x"))
            Right r -> case matching v r of
              Left e -> pure (Left e)
              Right r' -> resolveIn ((varName v, r') : done) rest
      | otherwise = resolveIn ((varName v, x) : done) rest
    names = map varName (tplVariables t)
    -- @.@ dropped and @..@ taken back a level, without touching the disk.
    normalisePath p = joinPath (reverse (foldl step [] (splitDirectories p)))
    step acc "." = acc
    step (x : acc) ".." | x /= "/" = acc
    step acc x = x : acc
    matching v x
      | T.null (varPattern v) = Right x
      | otherwise = case compile [] (varPattern v) of
          Left _ -> Left ("Variable `" <> varName v <> "` has a bad pattern `" <> varPattern v <> "`")
          Right rx
            | isJust (fullMatch rx x) -> Right x
            | otherwise -> Left ("Variable `" <> varName v <> "` is `" <> x <> "`, which does not match `" <> varPattern v <> "`")
    lastSegment p = case reverse (filter (`notElem` ["/", "."]) (splitDirectories p)) of
      (s : _) -> s
      [] -> ""

-- * Rendering

-- | Each listed file's rendered path (relative) and contents. A path that
-- renders to an absolute one, or through @..@, is refused, as are two files
-- rendering to one path. Files that are not UTF-8 are copied as they are.
renderFiles :: Template -> [(Text, Text)] -> IO (Either Text [(FilePath, ByteString)])
renderFiles t vals = do
  rendered <- forM (tplFiles t) $ \f -> case renderPath f of
    Left e -> pure (Left e)
    Right p -> do
      bytes <- BS.readFile (tplRoot t </> f)
      pure $ case TE.decodeUtf8' bytes of
        Left _ -> Right (p, bytes)
        Right txt -> (\c -> (p, TE.encodeUtf8 c)) <$> (renderGuards vals txt >>= renderText vals)
  pure $ do
    out <- sequence rendered
    case [p | (p, n) <- counts (map fst out), n > 1] of
      (p : _) -> Left ("Two template files render to `" <> T.pack p <> "`")
      [] -> Right out
  where
    renderPath f = do
      p <- T.unpack <$> renderText vals (T.pack f)
      let p' = joinPath (filter (/= ".") (splitDirectories p))
      if escapes p || null p'
        then Left ("Template file `" <> T.pack f <> "` renders to `" <> T.pack p <> "`, outside the project")
        else Right p'
    counts xs = [(x, length (filter (== x) xs)) | x <- nub xs]

-- | The steps with their argv rendered.
renderSteps :: Template -> [(Text, Text)] -> Either Text [Step]
renderSteps t vals = forM (tplSteps t) $ \s -> (\r -> s {stepRun = r}) <$> mapM (renderText vals) (stepRun s)

-- | The sample's path in the new project.
renderSample :: Template -> [(Text, Text)] -> Either Text FilePath
renderSample t vals = T.unpack <$> renderText vals (T.pack (tplSample t))

-- * Releases

-- | The repository whose tags name the releases of @lean@ or @mathlib@.
releaseRepository :: Text -> Maybe String
releaseRepository = \case
  "lean" -> Just "https://github.com/leanprover/lean4"
  "mathlib" -> Just "https://github.com/leanprover-community/mathlib4"
  _ -> Nothing

-- | The highest @v4.N.M@ release of @lean@ or @mathlib@.
latestRelease :: Text -> IO (Either Text Text)
latestRelease what = case releaseRepository what of
  Nothing -> pure (Left ("No release source for `" <> what <> "`"))
  Just url -> latestReleaseFrom url

-- | The highest @v4.N.M@ tag of a repository, by @git ls-remote --tags@.
latestReleaseFrom :: String -> IO (Either Text Text)
latestReleaseFrom url = do
  r <- git Nothing ["ls-remote", "--tags", "--refs", "--", url]
  pure $ case r of
    Left e -> Left e
    Right out -> maybe (Left ("No v4.N.M release tag in " <> T.pack url)) Right (pickLatest out)

-- | The highest @v4.N.M@ tag (release candidates and others left out) in
-- @git ls-remote@ output.
pickLatest :: Text -> Maybe Text
pickLatest out = case sortOn fst (mapMaybe version (T.lines out)) of
  [] -> Nothing
  vs -> Just (snd (last vs))
  where
    version line = do
      ref <- case T.splitOn "\t" line of
        [_, r] -> Just (T.strip r)
        _ -> Nothing
      tag <- T.stripPrefix "refs/tags/" ref
      rest <- T.stripPrefix "v4." tag
      case T.splitOn "." rest of
        [n, m] | all (\x -> not (T.null x) && T.all isDigit x) [n, m] -> Just ((read (T.unpack n) :: Int, read (T.unpack m) :: Int), tag)
        _ -> Nothing

-- * Revisions

-- | The template's git HEAD commit when its root is a git checkout with no
-- changes (@git status --porcelain@ empty), else the sha256 of its manifest
-- and listed files.
templateRevision :: Template -> IO Text
templateRevision t = do
  isRepo <- doesPathExist (tplRoot t </> ".git")
  clean <- if isRepo then git (Just (tplRoot t)) ["status", "--porcelain"] else pure (Left "")
  fromGit <- if clean == Right "" then git (Just (tplRoot t)) ["rev-parse", "HEAD"] else pure (Left "")
  case fromGit of
    Right h | not (T.null h) -> pure h
    _ -> do
      parts <- forM (manifestName : tplFiles t) $ \f -> do
        bytes <- either (const "") id <$> (try (BS.readFile (tplRoot t </> f)) :: IO (Either IOException ByteString))
        pure (BC.pack f <> "\0" <> BC.pack (show (BS.length bytes)) <> "\0" <> bytes)
      pure (sha256Hex (BS.concat parts))
