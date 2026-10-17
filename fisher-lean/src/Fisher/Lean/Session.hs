-- | lean-lsp-mcp's session state around the client (@server.py@,
-- @client_utils.py@, @file_utils.py@, @config.py@): the bound project, the
-- shared client per project (started on first use, restarted after it
-- dies), which paths a tool may touch, and how a tool's @file_path@
-- becomes the relative path handed to the client.
module Fisher.Lean.Session
  ( LeanEnv (..)
  , defaultLeanEnv
  , Session
  , sessionEnv
  , newSession
  , ToolError (..)
  , toolError
  , boundProject
  , getClient
  , requireClientForFile
  , setupClientForFile
  , startupClient
  , stopProjectClients
  , openSynced
  , PathPolicy (..)
  , pathPolicy
  , sessionPolicy
  , validatePath
  , resolve
  , policyContains
  , displayPath
  , boundOutput
  , maxOutputChars
  , maxOpenFiles
  , scratchSlots
  , validProject
  , requireProjectPath
  , bindProject
  , inferProjectPath
  , resolveFilePath
  , relPath
  , sessionRuntime
  , Runtime (..)
  , withBuildFlag
  , scratchPool
  , attachClient
  , closeSession
  , runningClient
  , sessionRate
  ) where

import Control.Concurrent.MVar
import qualified Data.ByteString as BS
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Info
import Control.Exception (Exception (..), SomeException, catch, throwIO, try)
import Control.Monad (filterM, forM, forM_, when)
import Data.IORef
import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (isAbsolute, joinPath, splitDirectories, takeDirectory, (</>))
import System.Process (readCreateProcessWithExitCode, proc, cwd)

import Fisher.Lean.Ref (modifyRef)
import Fisher.Lean.Client
import Fisher.Lean.Http (HttpClient)
import Fisher.Lean.HttpReal (realHttpClient)
import Fisher.Lean.Process (ProcessRunner, runMerged, runSplit)
import Fisher.Lean.Scratch
import Fisher.Tools.Py (pyInt)

-- | A tool failure; the text after @"Error executing tool <name>: "@.
newtype ToolError = ToolError Text
  deriving (Show)

instance Exception ToolError where
  displayException (ToolError t) = T.unpack t

toolError :: Text -> IO a
toolError = throwIO . ToolError

data LeanEnv = LeanEnv
  { leGetEnv :: Text -> IO (Maybe Text)
  , leClientEnv :: ClientEnv
  , lePrintPrefix :: FilePath -> IO (Maybe FilePath)
  -- ^ @lean --print-prefix@ run in a project
  , leCwd :: IO FilePath
  , leRequestTimeout :: Double
  -- ^ leanclient's per-request default (300 s)
  , leRgAvailable :: IO Bool
  -- ^ @shutil.which("rg")@ (lean_verify's source scan needs it)
  , leRunProcess :: ProcessRunner
  -- ^ @lake@ for lean_build and lean_profile_proof
  , leRunRg :: FilePath -> [String] -> IO (Int, BS.ByteString, BS.ByteString)
  -- ^ @rg@ for lean_local_search: exit code, stdout, stderr
  , leHttp :: HttpClient
  -- ^ the remote search services
  , leNow :: IO Double
  -- ^ @time.time()@, for the rate limits
  , leOs :: String
  -- ^ @System.Info.os@ (the ripgrep install hint is per platform)
  }

defaultLeanEnv :: LeanEnv
defaultLeanEnv =
  LeanEnv
    { leGetEnv = \k -> fmap T.pack <$> lookupEnv (T.unpack k)
    , leClientEnv = defaultClientEnv
    , lePrintPrefix = \root -> do
        r <- try @SomeException (readCreateProcessWithExitCode (proc "lean" ["--print-prefix"]) {cwd = Just root} "")
        pure $ case r of
          Right (ExitSuccess, out, _) | not (null (strip' out)) -> Just (strip' out)
          _ -> Nothing
    , leCwd = getCurrentDirectory
    , leRequestTimeout = 300
    , leRgAvailable = isJust <$> findExecutable "rg"
    , leRunProcess = runMerged
    , leRunRg = runSplit
    , leHttp = \rq -> sharedHttp >>= \h -> h rq
    , leNow = realToFrac <$> getPOSIXTime
    , leOs = System.Info.os }
  where
    strip' = reverse . dropWhile (`elem` (" \n\r\t" :: String)) . reverse . dropWhile (`elem` (" \n\r\t" :: String))

-- | One project's shared state: the client and the scratch pools' slots.
data Runtime = Runtime
  { rtClient :: Maybe Client
  , rtBuilding :: Bool
  }

data Session = Session
  { sEnv :: LeanEnv
  , sProject :: IORef (Maybe FilePath)
  , sClient :: IORef (Maybe Client)
  , sRuntimes :: IORef (M.Map FilePath Runtime)
  , sOrder :: IORef [FilePath]
  , sProjectCache :: IORef (M.Map FilePath FilePath)
  , sClientLock :: MVar ()
  , sPools :: IORef (M.Map (FilePath, Bool) ScratchPool)
  , sRate :: IORef (M.Map Text [Integer])
  -- ^ the lifespan's @rate_limit@: request times per category
  , sPrefixCache :: IORef (M.Map FilePath (Maybe FilePath))
  }

sessionEnv :: Session -> LeanEnv
sessionEnv = sEnv

-- | The session lifespan: @LEAN_PROJECT_PATH@, when set, must be a
-- project root.
newSession :: LeanEnv -> IO Session
newSession env = do
  raw <- maybe "" T.strip <$> leGetEnv env "LEAN_PROJECT_PATH"
  root <- if T.null raw then pure Nothing else Just <$> requireProjectPath (T.unpack raw)
  Session env <$> newIORef root <*> newIORef Nothing <*> newIORef M.empty <*> newIORef [] <*> newIORef M.empty <*> newMVar () <*> newIORef M.empty <*> newIORef M.empty <*> newIORef M.empty

boundProject :: Session -> IO (Maybe FilePath)
boundProject = readIORef . sProject

sessionRuntime :: Session -> FilePath -> IO (Maybe Runtime)
sessionRuntime s root = M.lookup root <$> readIORef (sRuntimes s)

-- ---------------------------------------------------------------- config

envInt :: Session -> Text -> Integer -> IO Integer
envInt s k d = do
  v <- leGetEnv (sEnv s) k
  pure (fromMaybe d (v >>= pyInt))

maxOutputChars :: Session -> IO Int
maxOutputChars s = fromIntegral . max 0 <$> envInt s "LEAN_MCP_MAX_OUTPUT_CHARS" 6000

maxOpenFiles :: Session -> IO Int
maxOpenFiles s = do
  v <- envInt s "LEAN_LSP_MAX_OPEN_FILES" 4
  pure (if v < 1 then 4 else fromIntegral v)

scratchSlots :: Session -> IO Int
scratchSlots s = fromIntegral . max 1 <$> envInt s "LEAN_MCP_SCRATCH_SLOTS" 1

-- | @bound_output(text)@: the middle elided past the character budget.
boundOutput :: Int -> Text -> Text
boundOutput limit text
  | limit <= 0 || T.length text <= limit = text
  | otherwise =
      let elided = T.length text - limit
          headN = (limit * 2) `div` 3
          tailN = limit - headN
          bounded =
            T.take headN text <> "\n\n[... " <> T.pack (show elided) <> " characters elided; set LEAN_MCP_MAX_OUTPUT_CHARS=0 for the full text ...]\n\n"
              <> T.takeEnd tailN text
       in if T.length bounded < T.length text then bounded else text

-- ---------------------------------------------------------------- projects

-- | @Path.resolve()@ (symlinks followed, missing tail kept).
resolve :: FilePath -> IO FilePath
resolve p = canonicalizePath p `catch` \(_ :: SomeException) -> makeAbsolute p

-- | A directory with @lean-toolchain@ and @lakefile.lean@ or @lakefile.toml@.
validProject :: FilePath -> IO Bool
validProject p = do
  d <- doesDirectoryExist p
  if not d
    then pure False
    else do
      tc <- doesFileExist (p </> "lean-toolchain")
      lf <- (||) <$> doesFileExist (p </> "lakefile.lean") <*> doesFileExist (p </> "lakefile.toml")
      pure (tc && lf)

-- | @require_lean_project_path(p)@.
requireProjectPath :: FilePath -> IO FilePath
requireProjectPath p0 = do
  p <- expandUser p0
  ex <- doesPathExist p
  if not ex
    then throwIO (ToolError ("Lean project path '" <> T.pack p0 <> "' does not exist."))
    else do
      r <- canonicalizePath p
      ok <- validProject r
      if ok
        then pure r
        else throwIO (ToolError ("Lean project path '" <> T.pack r <> "' must contain `lean-toolchain` and either `lakefile.lean` or `lakefile.toml`."))

expandUser :: FilePath -> IO FilePath
expandUser = \case
  '~' : rest | null rest || take 1 rest == "/" -> (<> rest) <$> getHomeDirectory
  p -> pure p

-- | @bind_lean_project_path@: switching projects (stdio only) drops the
-- session's client.
bindProject :: Session -> FilePath -> IO FilePath
bindProject s p = do
  root <- requireProjectPath p
  cur <- readIORef (sProject s)
  when (cur /= Just root) $ do
    transport <- maybe "stdio" (T.toLower . T.strip) <$> leGetEnv (sEnv s) "LEAN_LSP_MCP_ACTIVE_TRANSPORT"
    let t = if T.null transport then "stdio" else transport
    when (isJust cur && t /= "stdio") $
      throwIO (ToolError ("Project switching is disabled for `" <> t <> "` transport. Restart the server with LEAN_PROJECT_PATH set to the desired Lean project root."))
    writeIORef (sProject s) (Just root)
    writeIORef (sClient s) Nothing
  pure root

data PathPolicy = PathPolicy
  { ppRoot :: FilePath
  , ppRoots :: [(FilePath, Text)]
  -- ^ allowed root, display prefix
  }

-- | @build_lean_path_policy(root)@: the project, each of its packages,
-- the toolchain's sources.
pathPolicy :: Session -> FilePath -> IO PathPolicy
pathPolicy s root0 = do
  root <- resolve root0
  let pkgDir = root </> ".lake" </> "packages"
  isPkgs <- doesDirectoryExist pkgDir
  entries <- if isPkgs then sort <$> listDirectory pkgDir else pure []
  pkgs <- fmap catMaybes . forM entries $ \e -> do
    r <- try @SomeException (canonicalizePath (pkgDir </> e))
    ex <- either (const (pure False)) doesPathExist r
    pure $ case r of
      Right p | ex -> Just (p, ".lake/packages/" <> T.pack e)
      _ -> Nothing
  cached <- M.lookup root <$> readIORef (sPrefixCache s)
  prefix <- case cached of
    Just v -> pure v
    Nothing -> do
      v <- lePrintPrefix (sEnv s) root
      modifyRef (sPrefixCache s) (M.insert root v)
      pure v
  stdlib <- case prefix of
    Just pre -> do
      ok <- doesDirectoryExist (pre </> "src")
      pure [(pre </> "src", ".lean-stdlib") | ok]
    Nothing -> pure []
  pure (PathPolicy root ([(root, "")] <> pkgs <> stdlib))

-- | @get_path_policy(ctx)@: the bound project's policy.
sessionPolicy :: Session -> IO PathPolicy
sessionPolicy s = readIORef (sProject s) >>= maybe (toolError "lean project path is not set.") (pathPolicy s)

-- | @policy.validate_path(p)@: the resolved path, inside an allowed root.
validatePath :: PathPolicy -> FilePath -> IO FilePath
validatePath pol p = do
  r <- resolve p
  if any (under r . fst) (ppRoots pol)
    then pure r
    else toolError ("Path '" <> T.pack p <> "' is outside the active Lean project, dependencies, and stdlib roots.")

under :: FilePath -> FilePath -> Bool
under p root = let r = splitDirectories root in take (length r) (splitDirectories p) == r

policyContains :: PathPolicy -> FilePath -> IO Bool
policyContains pol p = do
  r <- resolve p
  pure (any (under r . fst) (ppRoots pol))

-- | @display_path@: the allowed root's prefix and the path under it.
displayPath :: PathPolicy -> FilePath -> IO (Maybe Text)
displayPath pol p = do
  r <- resolve p
  pure $ case [(root, pre) | (root, pre) <- ppRoots pol, under r root] of
    ((root, pre) : _) ->
      let rel = drop (length (splitDirectories root)) (splitDirectories r)
          relS = T.intercalate "/" (map T.pack rel)
       in Just (if null rel then (if T.null pre then "." else pre) else if T.null pre then relS else pre <> "/" <> relS)
    [] -> Nothing

-- | @os.path.relpath(p, root)@ (lexical).
relPath :: FilePath -> FilePath -> Text
relPath p root =
  let ps = splitDirectories p
      rs = splitDirectories root
      common = length (takeWhile id (zipWith (==) ps rs))
      ups = replicate (length rs - common) ".."
      rest = ups <> drop common ps
   in if null rest then "." else T.intercalate "/" (map T.pack rest)

-- | @resolve_file_path@: absolute as given, else against the bound root or
-- the working directory; it must exist.
resolveFilePath :: Session -> Text -> IO (Maybe FilePath)
resolveFilePath s fp = do
  p <- expandUser (T.unpack fp)
  bound <- readIORef (sProject s)
  base <- maybe (leCwd (sEnv s)) pure bound
  let full = if isAbsolute p then p else base </> p
  ex <- doesPathExist full
  if not ex then pure Nothing else Just <$> canonicalizePath full

-- | @infer_project_path(file)@: the bound project when it holds the file
-- and no nearer project does, else the innermost project above it (an
-- outer one when the file sits in its @.lake/packages@); the result is
-- bound.
inferProjectPath :: Session -> FilePath -> IO (Maybe FilePath)
inferProjectPath s file0 = do
  bound <- readIORef (sProject s)
  cwd' <- leCwd (sEnv s)
  let file1 = if isAbsolute file0 then file0 else fromMaybe cwd' bound </> file0
  file <- resolve file1
  isDir <- doesDirectoryExist file
  startDir <- resolve (if isDir then file else takeDirectory file)
  fast <- case bound of
    Just b -> do
      okPol <- try @SomeException (pathPolicy s b)
      case okPol of
        Right pol -> do
          inside <- policyContains pol file
          nearer <- enclosesNearer startDir b
          pure (if inside && not nearer then Just b else Nothing)
        Left _ -> pure Nothing
    Nothing -> pure Nothing
  case fast of
    Just b -> pure (Just b)
    Nothing -> do
      let dirs = ancestors startDir
      cands <- filterM cachedValid dirs
      case pick file cands of
        Nothing -> pure Nothing
        Just root -> Just <$> bindProject s root
  where
    ancestors d = let ps = splitDirectories d in [joinPath (take n ps) | n <- [length ps, length ps - 1 .. 1]]
    cachedValid d = do
      cache <- readIORef (sProjectCache s)
      case M.lookup d cache of
        Just r -> pure (not (null r))
        Nothing -> do
          v <- validProject d
          modifyRef (sProjectCache s) (M.insert d (if v then d else ""))
          pure v
    pick file cands =
      case [c | c <- cands, under file (c </> ".lake" </> "packages")] of
        (c : _) -> Just c
        [] -> case cands of
          (c : _) -> Just c
          [] -> Nothing
    enclosesNearer startDir b = do
      let between = takeWhile (/= b) [d | d <- ancestors startDir, under d b]
          pkg = b </> ".lake" </> "packages"
      or <$> mapM (\d -> if under d pkg then pure False else validProject d) between

-- | @setup_client_for_file@: the client-relative path, or Nothing.
setupClientForFile :: Session -> Text -> IO (Maybe Text)
setupClientForFile s fp = do
  mres <- resolveFilePath s fp
  case mres of
    Nothing -> pure Nothing
    Just resolved -> do
      mproj <- inferProjectPath s resolved
      case mproj of
        Nothing -> pure Nothing
        Just proj -> do
          mpol <- try @SomeException (pathPolicy s proj)
          case mpol of
            Left _ -> pure Nothing
            Right pol -> do
              ok <- policyContains pol resolved
              if not ok
                then pure Nothing
                else do
                  startupClient s
                  pure (Just (relPath resolved (ppRoot pol)))

-- | @require_client_for_file@.
requireClientForFile :: Session -> Text -> Maybe Text -> IO Text
requireClientForFile s fp custom = do
  r <- setupClientForFile s fp
  case r of
    Just rel -> pure rel
    Nothing ->
      toolError
        (fromMaybe ("Invalid Lean file path: '" <> fp <> "' not found in any Lean project (no lean-toolchain ancestor or file does not exist)") custom)

-- | @startup_client@: the project's shared client, started (or restarted
-- after a death) as needed.
startupClient :: Session -> IO ()
startupClient s = withMVar (sClientLock s) $ \_ -> do
  mroot <- readIORef (sProject s)
  root <- maybe (toolError "lean project path is not set.") pure mroot
  rts <- readIORef (sRuntimes s)
  let rt = fromMaybe (Runtime Nothing False) (M.lookup root rts)
  when (rtBuilding rt) $ toolError "A project build is in progress. Retry after the build completes."
  alive <- maybe (pure False) clientAlive (rtClient rt)
  c <-
    if alive
      then pure (maybe (error "unreachable") id (rtClient rt))
      else do
        forM_ (rtClient rt) $ \old -> try @SomeException (closeClient old)
        order <- readIORef (sOrder s)
        when (length (filter (/= root) order) >= 8) $ do
          let oldest = head (filter (/= root) order)
          old <- M.lookup oldest <$> readIORef (sRuntimes s)
          forM_ (old >>= rtClient) $ \oc -> try @SomeException (closeClient oc)
          modifyRef (sRuntimes s) (M.delete oldest)
          modifyRef (sOrder s) (filter (/= oldest))
        n <- maxOpenFiles s
        r <- try (startClient (leClientEnv (sEnv s)) root n (leRequestTimeout (sEnv s)))
        case r of
          Right c -> pure c
          Left (e :: SomeException) ->
            toolError ("Failed to start Lean language server at '" <> T.pack root <> "': " <> T.pack (displayException e))
  modifyRef (sRuntimes s) (M.insert root rt {rtClient = Just c})
  modifyRef (sOrder s) (\o -> filter (/= root) o <> [root])
  writeIORef (sClient s) (Just c)

-- | Close every client of the session's project (a build does this).
stopProjectClients :: Session -> FilePath -> IO ()
stopProjectClients s root = do
  rt <- M.lookup root <$> readIORef (sRuntimes s)
  own <- readIORef (sClient s)
  writeIORef (sClient s) Nothing
  modifyRef (sRuntimes s) (M.adjust (\r -> r {rtClient = Nothing}) root)
  let clients = foldr (\c acc -> if c `elem` acc then acc else c : acc) [] (catMaybes [own, rt >>= rtClient])
  forM_ clients $ \c -> try @SomeException (closeClient c)

withBuildFlag :: Session -> FilePath -> Bool -> IO ()
withBuildFlag s root on = modifyRef (sRuntimes s) (\m -> M.insert root ((fromMaybe (Runtime Nothing False) (M.lookup root m)) {rtBuilding = on}) m)

getClient :: Session -> IO Client
getClient s = readIORef (sClient s) >>= maybe (toolError "Lean client is not running for this project.") pure

-- | @open_synced(rel, wait)@.
openSynced :: Session -> Text -> Bool -> IO ()
openSynced s rel wait = do
  c <- getClient s
  reloadFromDisk c rel wait

-- | @get_scratch_pool@ / @get_serial_scratch_pool@: the project's pool for
-- its running client (a new one after a restart).
scratchPool :: Session -> Bool -> IO ScratchPool
scratchPool s serial = do
  root <- readIORef (sProject s) >>= maybe (toolError "lean project path is not set.") pure
  rt <- M.lookup root <$> readIORef (sRuntimes s)
  c <- maybe (toolError "Lean client is not running for this project.") pure (rt >>= rtClient)
  cur <- M.lookup (root, serial) <$> readIORef (sPools s)
  case cur of
    Just p | poolClient p == c -> pure p
    _ -> do
      size <- if serial then pure 1 else scratchSlots s
      p <- newScratchPool c "" size (if serial then "_mcp_serial" else "_mcp_scratch")
      modifyRef (sPools s) (M.insert (root, serial) p)
      pure p

-- | After a build: the new client is the project's shared client and the
-- session's.
attachClient :: Session -> FilePath -> Client -> IO ()
attachClient s root c = do
  modifyRef (sRuntimes s) (\m -> M.insert root ((fromMaybe (Runtime Nothing False) (M.lookup root m)) {rtClient = Just c}) m)
  modifyRef (sOrder s) (\o -> filter (/= root) o <> [root])
  writeIORef (sClient s) (Just c)

-- | The lifespan's end: every client the session started is closed.
closeSession :: Session -> IO ()
closeSession s = do
  rts <- readIORef (sRuntimes s)
  own <- readIORef (sClient s)
  forM_ (catMaybes (own : map rtClient (M.elems rts))) $ \c -> try @SomeException (closeClient c)

-- | One HTTPS manager for the process, made on first use.
sharedHttp :: IO HttpClient
sharedHttp = readMVar sharedHttpVar

sharedHttpVar :: MVar HttpClient
sharedHttpVar = unsafePerformIO (realHttpClient >>= newMVar)
{-# NOINLINE sharedHttpVar #-}

-- | @running_shared_client(root)@: the project's client if one is up; never
-- starts one.
runningClient :: Session -> FilePath -> IO (Maybe Client)
runningClient s root0 = do
  root <- resolve root0
  rt <- M.lookup root <$> readIORef (sRuntimes s)
  case rt >>= rtClient of
    Just c -> do
      ok <- clientAlive c
      pure (if ok then Just c else Nothing)
    Nothing -> pure Nothing

sessionRate :: Session -> IORef (M.Map Text [Integer])
sessionRate = sRate
