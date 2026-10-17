-- | A client for the Lean 4 language server (leanclient @aio/client.py@ and
-- @aio/document.py@, the client lean-lsp-mcp uses).
--
-- * Documents are tracked per project-relative path: the version sent, the
--   diagnostics published for it, fileProgress, stale-import and crash flags.
-- * Freshness: 'barrier' sends @textDocument/waitForDiagnostics@ for the
--   current version; queries run it first unless told not to.
-- * Positions are 0-based lines and code-point columns; UTF-16 conversion
--   happens at the wire.
-- * Open files are capped (LRU eviction, never in-flight or pinned ones).
-- * A file watcher (when the server registers one) reports @.lean@ and
--   @.ilean@ changes, invalidates barriers and marks other files stale.
module Fisher.Lean.Client
  ( ClientEnv (..)
  , defaultClientEnv
  , Client
  , clientProject
  , clientMaxWorkers
  , startClient
  , closeClient
  , clientAlive
  , content
  , docLines
  , openPaths
  , isOpen
  , openFile
  , updateFile
  , reloadFromDisk
  , closeFile
  , restartFile
  , pinFile
  , barrier
  , GoalResult (..)
  , DiagnosticsReport (..)
  , diagnostics
  , goal
  , termGoal
  , hover
  , completions
  , completionResolve
  , references
  , goto
  , documentSymbols
  , workspaceSymbol
  , codeActions
  , codeActionResolve
  , rpcCall
  , getWidgets
  , getWidgetSource
  , convertLocation
  , pathToUri
  , joinPath'
  ) where

import Control.Concurrent.MVar
import Control.Exception (SomeException, catch, finally, throwIO, try)
import Control.Monad (forM, forM_, unless, void, when)
import Data.IORef
import Data.List (minimumBy, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Clock (getMonotonicTime)
import System.Directory (canonicalizePath, getModificationTime, makeAbsolute)
import System.FilePath (isAbsolute, splitDirectories, takeExtension, (</>))
import System.Posix.Process (getProcessID)

import Fisher.Lean.Ref (modifyRef)
import Fisher.Lean.Error
import Fisher.Lean.Text
import Fisher.Lean.Transport
import Fisher.Lean.Watch (fsWatch)
import Fisher.Lean.Utf16
import Fisher.Py.Value
import qualified Fisher.Py.Re as Re

-- | What the client takes from the world.
data ClientEnv = ClientEnv
  { ceWire :: FilePath -> IO Wire
  -- ^ start the server in the project directory
  , ceWatch :: FilePath -> ([(Int, FilePath)] -> IO ()) -> IO (IO ())
  -- ^ watch a directory tree for @.lean@/@.ilean@ changes: batches of
  -- (change type 1 added, 2 modified, 3 deleted; absolute path); returns
  -- the stop action
  , ceProcessId :: IO Integer
  , ceNow :: IO Double
  -- ^ monotonic seconds
  }

defaultClientEnv :: ClientEnv
defaultClientEnv =
  ClientEnv
    { ceWire = processWire ["lake", "serve", "--", "-Dserver.reportDelayMs=0"]
    , ceWatch = fsWatch
    , ceProcessId = fromIntegral <$> getProcessID
    , ceNow = getMonotonicTime }

data Status = Opening | Live | Crashed | Closed
  deriving (Eq, Show)

data DocRec = DocRec
  { dText :: Text
  , dLines :: [Text]
  , dVersion :: Integer
  , dStatus :: Status
  , dPinned :: Bool
  , dRefcount :: Int
  , dLastUsed :: Double
  , dDiagnostics :: [PyVal]
  , dDiagnosticsVersion :: Maybe Integer
  , dBarrierVersion :: Maybe Integer
  , dProcessing :: [PyVal]
  , dFatal :: Bool
  , dStale :: Bool
  , dRebuildAttempted :: Bool
  }

data Doc = Doc
  { docPath :: Text
  , docUri :: Text
  , docVirtual :: Bool
  , docRec :: IORef DocRec
  }

data Client = Client
  { cProject :: FilePath
  , cMaxWorkers :: Int
  , cTimeout :: Double
  , cEnv :: ClientEnv
  , cTransport :: Transport
  , cDocs :: IORef (M.Map Text Doc)
  , cOpenLock :: MVar ()
  , cRebuildLocks :: IORef (M.Map Text (MVar ()))
  , cRpc :: IORef (M.Map Text (Text, Double))
  , cWatchStop :: IORef (Maybe (IO ()))
  , cStarted :: IORef Bool
  , cLinesCache :: IORef (M.Map FilePath (Text, [Text]))
  }

-- | Identity (Python's @is@).
instance Eq Client where
  a == b = cDocs a == cDocs b

clientProject :: Client -> FilePath
clientProject = cProject

clientMaxWorkers :: Client -> Int
clientMaxWorkers = cMaxWorkers

-- ---------------------------------------------------------------- paths

-- | @Path(a) / b@ as pathlib joins: an absolute @b@ wins; @.@ components
-- and repeated slashes go, @..@ stays.
joinPath' :: FilePath -> FilePath -> FilePath
joinPath' a b =
  let parts = filter (\p -> p /= "." && p /= "/") (splitDirectories (if isAbsolute b then b else a </> b))
   in "/" <> foldr1' parts
  where
    foldr1' [] = ""
    foldr1' ps = foldr1 (\x acc -> x <> "/" <> acc) ps

pathToUri :: FilePath -> Text -> Text
pathToUri project path = pathAsUri (joinPath' project (T.unpack path))

uriToRelpath :: Client -> Text -> Text
uriToRelpath c uri =
  let local = uriToPath uri
      root = splitDirectories (cProject c)
      parts = splitDirectories local
   in if take (length root) parts == root && length parts >= length root
        then case drop (length root) parts of
          [] -> "."
          rest -> T.pack (foldr1 (\x acc -> x <> "/" <> acc) rest)
        else T.pack local

-- ---------------------------------------------------------------- lifecycle

-- | @AsyncLeanLSPClient(project).start()@: the toolchain floor (4.24), the
-- server, @initialize@ (30 s), @initialized@, the file watcher.
startClient :: ClientEnv -> FilePath -> Int -> Double -> IO Client
startClient env project0 maxWorkers reqTimeout = do
  project <- canonicalizePath project0 `catch` \(_ :: SomeException) -> makeAbsolute project0
  checkToolchain project
  docs <- newIORef M.empty
  watchStop <- newIORef Nothing
  cref <- newIORef Nothing
  let onNote method params = readIORef cref >>= mapM_ (\c -> onNotification c method params)
      onReq method params = readIORef cref >>= mapM_ (\c -> onServerRequest c method params)
  wire <- ceWire env project
  tr <- newTransport wire reqTimeout onNote onReq
  c <- Client project maxWorkers reqTimeout env tr docs <$> newMVar () <*> newIORef M.empty <*> newIORef M.empty
    <*> pure watchStop <*> newIORef False <*> newIORef M.empty
  writeIORef cref (Just c)
  pid <- ceProcessId env
  _ <- request tr "initialize"
    (PDict
      [ ("processId", PInt pid)
      , ("rootUri", PStr (pathAsUri (joinPath' project "")))
      , ( "capabilities"
        , PDict
            [ ("workspace", PDict [("didChangeWatchedFiles", PDict [("dynamicRegistration", PBool True)])])
            , ("lean", PDict [("incrementalDiagnosticSupport", PBool True)]) ] )
      , ("initializationOptions", PDict [("editDelay", PInt 1), ("hasWidgets", PBool True)]) ])
    (Just 30)
  notify tr "initialized" (PDict [])
  startWatcher c
  writeIORef (cStarted c) True
  pure c

checkToolchain :: FilePath -> IO ()
checkToolchain project = do
  r <- try @SomeException (readTextUtf8 (project </> "lean-toolchain"))
  case r of
    Left _ -> pure ()
    Right raw -> case Re.search (Re.compileOrDie [] "v(\\d+)\\.(\\d+)") (T.strip raw) of
      Just m | Just a <- Re.group 1 m, Just b <- Re.group 2 m -> do
        let (ma, mi) = (read (T.unpack a) :: Int, read (T.unpack b) :: Int)
        when ((ma, mi) < (4, 24)) $
          throwIO . UnsupportedVersion $
            "Project toolchain is Lean " <> a <> "." <> b
              <> "; leanclient.aio requires >= 4.24. Update lean-toolchain, or pass check_version=False (unsupported)."
      _ -> pure ()

closeClient :: Client -> IO ()
closeClient c = do
  writeIORef (cStarted c) False
  stopWatcher c
  closeTransport (cTransport c)
  writeIORef (cDocs c) M.empty

clientAlive :: Client -> IO Bool
clientAlive c = (&&) <$> readIORef (cStarted c) <*> transportAlive (cTransport c)

-- ---------------------------------------------------------------- notifications

onNotification :: Client -> Text -> PyVal -> IO ()
onNotification c method params = do
  let uri = case get "uri" params of
        Just (PStr u) -> Just u
        Just _ -> Nothing
        Nothing -> case get "textDocument" params >>= get "uri" of
          Just (PStr u) -> Just u
          _ -> Nothing
  docs <- readIORef (cDocs c)
  forM_ (uri >>= \u -> lookupUri docs u) $ \doc -> case method of
    "textDocument/publishDiagnostics" -> atomicModifyIORef' (docRec doc) $ \r ->
      let version = case get "version" params of Just (PInt v) -> Just v; _ -> Nothing
          ds = case get "diagnostics" params of Just (PList xs) -> xs; _ -> []
       in case version of
            Just v | v < dVersion r -> (r, ())
            _ ->
              ( r { dDiagnostics = if get "isIncremental" params == Just (PBool True) then dDiagnostics r <> ds else ds
                  , dDiagnosticsVersion = version }
              , () )
    "$/lean/fileProgress" -> atomicModifyIORef' (docRec doc) $ \r ->
      let ps = case get "processing" params of Just (PList xs) -> xs; _ -> []
       in (r {dProcessing = ps, dFatal = dFatal r || any (\e -> get "kind" e == Just (PInt 2)) ps}, ())
    "$/lean/staleDependency" -> atomicModifyIORef' (docRec doc) (\r -> (r {dStale = True, dRebuildAttempted = False}, ()))
    _ -> pure ()

lookupUri :: M.Map Text Doc -> Text -> Maybe Doc
lookupUri docs u = case [d | d <- M.elems docs, docUri d == u] of
  (d : _) -> Just d
  [] -> Nothing

onServerRequest :: Client -> Text -> PyVal -> IO ()
onServerRequest c method params = case method of
  "client/registerCapability"
    | any watched (listOf "registrations" params) -> startWatcher c
  "client/unregisterCapability"
    | any watched (listOf "unregisterations" params <> (if has "unregisterations" params then [] else listOf "unregistrations" params)) -> stopWatcher c
  _ -> pure ()
  where
    watched r = get "method" r == Just (PStr "workspace/didChangeWatchedFiles")
    listOf k v = case get k v of Just (PList xs) -> xs; _ -> []

startWatcher :: Client -> IO ()
startWatcher c = do
  running <- readIORef (cWatchStop c)
  when (isNothing running) $ do
    stop <- ceWatch (cEnv c) (cProject c) (onChanges c)
    writeIORef (cWatchStop c) (Just stop)

stopWatcher :: Client -> IO ()
stopWatcher c = do
  s <- atomicModifyIORef' (cWatchStop c) (\s -> (Nothing, s))
  forM_ s id

onChanges :: Client -> [(Int, FilePath)] -> IO ()
onChanges c changes = do
  invalidateDiskBarriers c
  uris <- forM [p | (_, p) <- changes, takeExtension p == ".lean"] $ \p -> pathAsUri <$> resolved p
  markOthersStale c uris
  items <- forM (sortOn snd changes) $ \(ty, p) -> do
    u <- pathAsUri <$> resolved p
    pure (PDict [("uri", PStr u), ("type", PInt (fromIntegral ty))])
  void (try @LeanError (notify (cTransport c) "workspace/didChangeWatchedFiles" (PDict [("changes", PList items)])))
  where
    resolved p = canonicalizePath p `catch` \(_ :: SomeException) -> pure p

-- ---------------------------------------------------------------- documents

getDoc :: Client -> Text -> IO Doc
getDoc c path = do
  docs <- readIORef (cDocs c)
  case M.lookup path docs of
    Just d -> pure d
    Nothing -> throwIO (FileNotOpen (path <> " is not open (call open() first)"))

content :: Client -> Text -> IO Text
content c path = getDoc c path >>= fmap dText . readIORef . docRec

docLines :: Client -> Text -> IO [Text]
docLines c path = getDoc c path >>= fmap dLines . readIORef . docRec

openPaths :: Client -> IO [Text]
openPaths c = M.keys <$> readIORef (cDocs c)

isOpen :: Client -> Text -> IO Bool
isOpen c path = do
  docs <- readIORef (cDocs c)
  case M.lookup path docs of
    Nothing -> pure False
    Just d -> (/= Closed) . dStatus <$> readIORef (docRec d)

touch :: Client -> Doc -> IO ()
touch c d = do
  t <- ceNow (cEnv c)
  modifyRef (docRec d) (\r -> r {dLastUsed = t})

newRec :: Double -> Text -> DocRec
newRec t text =
  DocRec text (splitLines text) 1 Opening False 0 t [] Nothing Nothing [] False False False

projectFile :: Client -> Text -> FilePath
projectFile c path = joinPath' (cProject c) (T.unpack path)

-- | @open(path, text=None, wait=True, dependency_build_mode="never")@.
openFile :: Client -> Text -> Maybe Text -> Bool -> Text -> IO ()
openFile c path mtext wait mode = do
  docs <- readIORef (cDocs c)
  existing <- case M.lookup path docs of
    Just d -> do
      r <- readIORef (docRec d)
      pure (if dStatus r /= Closed then Just (d, r) else Nothing)
    Nothing -> pure Nothing
  case existing of
    Just (d, r) -> do
      case mtext of
        Just text | text /= dText r -> updateFile c path text wait
        _ -> when wait (barrier c path Nothing)
      touch c d
    Nothing -> do
      withMVar (cOpenLock c) $ \_ -> do
        evictIfNeeded c
        text <- maybe (readTextUtf8 (projectFile c path)) pure mtext
        t <- ceNow (cEnv c)
        ref <- newIORef (newRec t text)
        let doc = Doc path (pathToUri (cProject c) path) (mtext /= Nothing) ref
        modifyRef (cDocs c) (M.insert path doc)
        notify (cTransport c) "textDocument/didOpen" $
          PDict
            [ ("textDocument", PDict [("uri", PStr (docUri doc)), ("languageId", PStr "lean4"), ("version", PInt 1), ("text", PStr text)])
            , ("dependencyBuildMode", PStr mode) ]
        modifyRef ref (\r -> r {dStatus = Live})
      when wait (barrier c path Nothing)

-- | Scratch slots do not count against the open-file budget.
pinFile :: Client -> Text -> IO ()
pinFile c path = getDoc c path >>= \d -> modifyRef (docRec d) (\r -> r {dPinned = True})

-- | @update(path, text, wait=False)@: a full-text didChange.
updateFile :: Client -> Text -> Text -> Bool -> IO ()
updateFile c path text wait = do
  doc <- getDoc c path
  t <- ceNow (cEnv c)
  v <- atomicModifyIORef' (docRec doc) $ \r ->
    let r' = r { dText = text, dLines = splitLines text, dBarrierVersion = Nothing, dRebuildAttempted = False
               , dVersion = dVersion r + 1, dDiagnosticsVersion = Nothing, dFatal = False, dStatus = Live, dLastUsed = t }
     in (r', dVersion r')
  notify (cTransport c) "textDocument/didChange" $
    PDict [("textDocument", PDict [("uri", PStr (docUri doc)), ("version", PInt v)]), ("contentChanges", PList [PDict [("text", PStr text)]])]
  when wait (barrier c path Nothing)

-- | @reload_from_disk(path, wait=False)@: a changed file is sent as a
-- didChange, every other disk document is marked stale, then didSave.
reloadFromDisk :: Client -> Text -> Bool -> IO ()
reloadFromDisk c path wait = do
  disk <- readTextUtf8 (projectFile c path)
  open' <- isOpen c path
  if not open'
    then openFile c path Nothing wait "never"
    else do
      cur <- content c path
      if disk /= cur
        then do
          updateFile c path disk False
          invalidateDiskBarriers c
          doc <- getDoc c path
          markOthersStale c [docUri doc]
          notify (cTransport c) "textDocument/didSave" (PDict [("textDocument", PDict [("uri", PStr (docUri doc))])])
          when wait (barrier c path Nothing)
        else when wait (barrier c path Nothing)

invalidateDiskBarriers :: Client -> IO ()
invalidateDiskBarriers c = do
  docs <- readIORef (cDocs c)
  forM_ (M.elems docs) $ \d -> unless (docVirtual d) $ modifyRef (docRec d) (\r -> r {dBarrierVersion = Nothing})

-- | Every non-virtual document other than the changed ones is marked stale.
markOthersStale :: Client -> [Text] -> IO ()
markOthersStale c uris = do
  docs <- readIORef (cDocs c)
  forM_ (M.elems docs) $ \d ->
    when (not (docVirtual d) && any (/= docUri d) uris) $
      modifyRef (docRec d) (\r -> r {dStale = True, dRebuildAttempted = False})

closeFile :: Client -> Text -> IO ()
closeFile c path = do
  md <- atomicModifyIORef' (cDocs c) (\m -> (M.delete path m, M.lookup path m))
  forM_ md $ \d -> do
    modifyRef (cRpc c) (M.delete (docUri d))
    modifyRef (docRec d) (\r -> r {dStatus = Closed})
    void (try @LeanError (notify (cTransport c) "textDocument/didClose" (PDict [("textDocument", PDict [("uri", PStr (docUri d))])])))

-- | @restart_file@: didClose + didOpen with the text reread from disk
-- (virtual documents keep theirs) and the version bumped.
restartFile :: Client -> Text -> Bool -> Text -> IO ()
restartFile c path wait mode = do
  open' <- isOpen c path
  if not open'
    then openFile c path Nothing wait mode
    else do
      doc <- getDoc c path
      r0 <- readIORef (docRec doc)
      text <- if docVirtual doc then pure (dText r0) else readTextUtf8 (projectFile c path)
      notify (cTransport c) "textDocument/didClose" (PDict [("textDocument", PDict [("uri", PStr (docUri doc))])])
      modifyRef (cRpc c) (M.delete (docUri doc))
      v <- atomicModifyIORef' (docRec doc) $ \r ->
        let r' = r { dText = text, dLines = splitLines text, dBarrierVersion = Nothing, dVersion = dVersion r + 1, dStatus = Opening
                   , dDiagnostics = [], dDiagnosticsVersion = Nothing, dProcessing = [], dFatal = False, dStale = False
                   , dRebuildAttempted = False }
         in (r', dVersion r')
      notify (cTransport c) "textDocument/didOpen" $
        PDict
          [ ("textDocument", PDict [("uri", PStr (docUri doc)), ("languageId", PStr "lean4"), ("version", PInt v), ("text", PStr text)])
          , ("dependencyBuildMode", PStr mode) ]
      modifyRef (docRec doc) (\r -> r {dStatus = Live})
      when wait (barrier c path Nothing)

evictIfNeeded :: Client -> IO ()
evictIfNeeded c = loop
  where
    loop = do
      docs <- M.elems <$> readIORef (cDocs c)
      recs <- mapM (\d -> (,) d <$> readIORef (docRec d)) docs
      let used = length [() | (_, r) <- recs, not (dPinned r)]
          candidates = [(d, r) | (d, r) <- recs, not (dPinned r), dRefcount r == 0, dStatus r /= Closed]
      when (used >= cMaxWorkers c && not (null candidates)) $ do
        let (victim, _) = minimumBy (comparing (dLastUsed . snd)) candidates
        closeFile c (docPath victim)
        loop

-- ---------------------------------------------------------------- freshness

-- | Resolves once the server has elaborated the current version.
barrier :: Client -> Text -> Maybe Double -> IO ()
barrier c path mtimeout = do
  rebuilt <- rebuildStale c path mtimeout
  unless rebuilt $ do
    barrierOnce c path mtimeout
    void (rebuildStale c path mtimeout)

withRef :: Client -> Doc -> IO a -> IO a
withRef c doc act = do
  modifyRef (docRec doc) (\r -> r {dRefcount = dRefcount r + 1})
  act `finally` do
    modifyRef (docRec doc) (\r -> r {dRefcount = dRefcount r - 1})
    touch c doc

barrierOnce :: Client -> Text -> Maybe Double -> IO ()
barrierOnce c path mtimeout = do
  doc <- getDoc c path
  r0 <- readIORef (docRec doc)
  when (dStatus r0 == Crashed) $ updateFile c path (dText r0) False
  r <- readIORef (docRec doc)
  let v = dVersion r
  case dBarrierVersion r of
    Just bv | bv >= v -> touch c doc
    _ -> withRef c doc $ do
      res <- try (request (cTransport c) "textDocument/waitForDiagnostics" (PDict [("uri", PStr (docUri doc)), ("version", PInt v)]) (Just (fromMaybe (cTimeout c) (nonZero mtimeout))))
      case res of
        Right _ -> modifyRef (docRec doc) (\x -> x {dBarrierVersion = Just (maybe v (max v) (dBarrierVersion x))})
        Left e@(RpcError code msg _)
          | code `elem` [-32901, -32902] || "worker" `T.isInfixOf` T.toLower msg -> do
              modifyRef (docRec doc) (\x -> x {dStatus = Crashed})
              throwIO (WorkerCrashed ("File worker for " <> path <> " died: " <> msg) (docUri doc))
          | otherwise -> throwIO e
        Left e -> throwIO e
  where
    nonZero = \case
      Just t | t /= 0 -> Just t
      _ -> Nothing

rebuildStale :: Client -> Text -> Maybe Double -> IO Bool
rebuildStale c path mtimeout = do
  doc <- getDoc c path
  need <- needsRebuild doc
  if not need
    then pure False
    else do
      lock <- do
        fresh <- newMVar ()
        atomicModifyIORef' (cRebuildLocks c) (\m -> case M.lookup path m of Just l -> (m, l); Nothing -> (M.insert path fresh m, fresh))
      withMVar lock $ \_ -> do
        doc' <- getDoc c path
        need' <- needsRebuild doc'
        if not need'
          then pure False
          else do
            modifyRef (docRec doc') (\r -> r {dRebuildAttempted = True})
            restartFile c path False "once"
            getDoc c path >>= \d -> modifyRef (docRec d) (\r -> r {dRebuildAttempted = True})
            barrierOnce c path mtimeout
            pure True
  where
    needsRebuild doc = do
      r <- readIORef (docRec doc)
      pure $
        not (docVirtual doc) && not (dRebuildAttempted r)
          && (dStale r || any (\d -> "Imports are out of date" `T.isInfixOf` pyStrOf (fromMaybe (PStr "") (get "message" d))) (dDiagnostics r))

-- ---------------------------------------------------------------- queries

position :: [Text] -> Int -> Int -> PyVal
position = positionToUtf16

requestAt :: Client -> Doc -> Text -> Int -> Int -> [(Text, PyVal)] -> Maybe Double -> IO PyVal
requestAt c doc method line col extra mtimeout = do
  ls <- dLines <$> readIORef (docRec doc)
  let params = PDict ([("textDocument", PDict [("uri", PStr (docUri doc))]), ("position", position ls line col)] <> extra)
  withRef c doc $ do
    r <- try (request (cTransport c) method params mtimeout)
    case r of
      Right v -> pure v
      Left (RpcError code msg _) | code `elem` [-32901, -32902] -> do
        modifyRef (docRec doc) (\x -> x {dStatus = Crashed})
        throwIO (WorkerCrashed ("File worker for " <> docPath doc <> " died: " <> msg) (docUri doc))
      Left e -> throwIO e

data DiagnosticsReport = DiagnosticsReport
  { drItems :: [PyVal]
  , drVersion :: Maybe Integer
  , drFatal :: Bool
  , drPartial :: Bool
  , drProcessing :: [PyVal]
  }

-- | @diagnostics(path, fresh=True, timeout=None, partial_ok=False)@.
diagnostics :: Client -> Text -> Bool -> Maybe Double -> Bool -> IO DiagnosticsReport
diagnostics c path fresh mtimeout partialOk = do
  doc <- getDoc c path
  partial <-
    if not fresh
      then pure False
      else do
        r <- try (barrier c path mtimeout)
        case r of
          Right () -> pure False
          Left (RequestTimeout m) -> if partialOk then pure True else throwIO (RequestTimeout m)
          Left e -> throwIO e
  r <- readIORef (docRec doc)
  let ls = dLines r
      conv d = foldl (\acc k -> case get k acc of
                         Just rng | truthy rng -> set k (rangeFromUtf16 ls rng) acc
                         _ -> acc) d ["range", "fullRange"]
      processing = if partial then [rangeFromUtf16 ls rng | e <- dProcessing r, Just rng <- [get "range" e], truthy rng] else []
  pure (DiagnosticsReport (map conv (dDiagnostics r)) (dDiagnosticsVersion r) (dFatal r) partial processing)

data GoalResult = GoalResult
  { grStatus :: Text
  -- ^ "goals", "complete", "no_goal"
  , grGoals :: [Text]
  , grRendered :: Maybe Text
  }

goal :: Client -> Text -> Int -> Int -> Bool -> IO GoalResult
goal c path line col fresh = do
  doc <- getDoc c path
  when fresh (barrier c path Nothing)
  res <- requestAt c doc "$/lean/plainGoal" line col [] Nothing
  let rendered = case get "rendered" res of Just (PStr s) -> Just s; _ -> Nothing
  pure $ case res of
    PNull -> GoalResult "no_goal" [] Nothing
    _ -> case get "goals" res of
      Just (PList gs) | not (null gs) -> GoalResult "goals" [pyStrOf g | g <- gs] rendered
      _ -> GoalResult "complete" [] rendered

termGoal :: Client -> Text -> Int -> Int -> Bool -> IO PyVal
termGoal c path line col fresh = do
  doc <- getDoc c path
  when fresh (barrier c path Nothing)
  requestAt c doc "$/lean/plainTermGoal" line col [] Nothing

hover :: Client -> Text -> Int -> Int -> Bool -> IO PyVal
hover c path line col fresh = do
  doc <- getDoc c path
  when fresh (barrier c path Nothing)
  res <- requestAt c doc "textDocument/hover" line col [] Nothing
  ls <- dLines <$> readIORef (docRec doc)
  pure $ case get "range" res of
    Just rng | truthy res -> set "range" (rangeFromUtf16 ls rng) res
    _ -> res

completions :: Client -> Text -> Int -> Int -> Bool -> IO [PyVal]
completions c path line col fresh = do
  doc <- getDoc c path
  when fresh (barrier c path Nothing)
  res <- requestAt c doc "textDocument/completion" line col [("context", PDict [("triggerKind", PInt 1)])] Nothing
  pure $ case res of
    PNull -> []
    PList xs -> xs
    _ -> case get "items" res of
      Just (PList xs) -> xs
      _ -> []

completionResolve :: Client -> PyVal -> Double -> IO PyVal
completionResolve c item t = do
  res <- request (cTransport c) "completionItem/resolve" item (Just t)
  case res of
    PDict _ -> pure res
    _ -> throwIO (ClientError "completionItem/resolve returned a non-object response")

references :: Client -> Text -> Int -> Int -> Bool -> Maybe Int -> Bool -> IO [PyVal]
references c path line col includeDecl maxResults fresh = do
  doc <- getDoc c path
  when fresh (barrier c path Nothing)
  res <- requestAt c doc "textDocument/references" line col [("context", PDict [("includeDeclaration", PBool includeDecl)])] Nothing
  let locs = case res of PList xs -> xs; _ -> []
      locs' = maybe locs (`take` locs) maxResults
  mapM (convertLocation c) locs'

goto :: Client -> Text -> Text -> Int -> Int -> Bool -> IO [PyVal]
goto c kind path line col fresh = do
  doc <- getDoc c path
  when fresh (barrier c path Nothing)
  res <- requestAt c doc ("textDocument/" <> kind) line col [] Nothing
  case res of
    PNull -> pure []
    PDict _ -> mapM (convertLocation c) [res]
    PList xs -> mapM (convertLocation c) xs
    _ -> pure []

dictList :: PyVal -> [PyVal]
dictList = \case
  PList xs -> [x | x@(PDict _) <- xs]
  _ -> []

documentSymbols :: Client -> Text -> Bool -> IO [PyVal]
documentSymbols c path fresh = do
  doc <- getDoc c path
  when fresh (barrier c path Nothing)
  res <- withRef c doc (request (cTransport c) "textDocument/documentSymbol" (PDict [("textDocument", PDict [("uri", PStr (docUri doc))])]) Nothing)
  pure (dictList res)

-- | @workspace_symbol(query, max_results, wait_for_index=0, timeout)@: the
-- symbols (locations in code points) and whether the .ilean index had
-- finished loading, which nothing here waits for, so it is always False.
workspaceSymbol :: Client -> Text -> Maybe Int -> Double -> IO ([PyVal], Bool)
workspaceSymbol c query maxResults t = do
  res <- request (cTransport c) "workspace/symbol" (PDict [("query", PStr query)]) (Just t)
  let syms = maybe id take maxResults (dictList res)
  out <- mapM (\sym -> case get "location" sym of
                  Just loc | truthy loc -> (\l -> set "location" l sym) <$> convertLocation c loc
                  _ -> pure sym) syms
  pure (out, False)

codeActions :: Client -> Text -> Int -> Int -> Int -> Int -> Bool -> IO [PyVal]
codeActions c path sl sc el ec fresh = do
  doc <- getDoc c path
  when fresh (barrier c path Nothing)
  ls <- dLines <$> readIORef (docRec doc)
  res <- withRef c doc $
    request (cTransport c) "textDocument/codeAction"
      (PDict
        [ ("textDocument", PDict [("uri", PStr (docUri doc))])
        , ("range", PDict [("start", position ls sl sc), ("end", position ls el ec)])
        , ("context", PDict [("diagnostics", PList [])]) ])
      Nothing
  pure (dictList res)

codeActionResolve :: Client -> PyVal -> Double -> IO PyVal
codeActionResolve c action t = do
  res <- request (cTransport c) "codeAction/resolve" action (Just t)
  case res of
    PDict _ -> pure res
    _ -> throwIO (ClientError "codeAction/resolve returned a non-object response")

-- ---------------------------------------------------------------- Lean RPC

rpcCall :: Client -> Text -> Int -> Int -> Text -> PyVal -> Double -> IO PyVal
rpcCall c path line col method params t = do
  doc <- getDoc c path
  session <- rpcSession c doc t
  ls <- dLines <$> readIORef (docRec doc)
  request (cTransport c) "$/lean/rpc/call"
    (PDict
      [ ("textDocument", PDict [("uri", PStr (docUri doc))]), ("position", position ls line col)
      , ("sessionId", PStr session), ("method", PStr method), ("params", params) ])
    (Just t)

rpcSession :: Client -> Doc -> Double -> IO Text
rpcSession c doc t = do
  now' <- ceNow (cEnv c)
  known <- M.lookup (docUri doc) <$> readIORef (cRpc c)
  case known of
    Just (s, at) | now' - at < 20 -> modifyRef (cRpc c) (M.insert (docUri doc) (s, now')) >> pure s
    _ -> do
      res <- request (cTransport c) "$/lean/rpc/connect" (PDict [("uri", PStr (docUri doc))]) (Just t)
      case get "sessionId" res of
        Just (PStr s) -> modifyRef (cRpc c) (M.insert (docUri doc) (s, now')) >> pure s
        _ -> throwIO (ClientError "$/lean/rpc/connect returned no sessionId")

getWidgets :: Client -> Text -> Int -> Int -> Bool -> IO [PyVal]
getWidgets c path line col fresh = do
  when fresh (barrier c path Nothing)
  ls <- docLines c path
  res <- rpcCall c path line col "Lean.Widget.getWidgets"
    (PDict [("line", PInt (fromIntegral line)), ("character", PInt (fromIntegral (codepointToUtf16 (lineAt ls line) col)))]) 20
  pure $ case get "widgets" (if truthy res then res else PDict []) of
    Just (PList ws) -> ws
    _ -> []

getWidgetSource :: Client -> Text -> Int -> Int -> Text -> IO PyVal
getWidgetSource c path line col hash = do
  ls <- docLines c path
  rpcCall c path line col "Lean.Widget.getWidgetSource" (PDict [("pos", position ls line col), ("hash", PStr hash)]) 20

-- ---------------------------------------------------------------- locations

-- | A Location (or LocationLink) with its range in code points (an open
-- document's lines, else the target file from disk) and @path@ added.
convertLocation :: Client -> PyVal -> IO PyVal
convertLocation c loc = do
  let uri = case (get "uri" loc, get "targetUri" loc) of
        (Just (PStr u), _) | not (T.null u) -> u
        (_, Just (PStr u)) -> u
        _ -> ""
      key = if has "range" loc then "range" else "targetRange"
      out = set "path" (PStr (uriToRelpath c uri)) loc
  case get key loc of
    Nothing -> pure out
    Just rng -> do
      docs <- readIORef (cDocs c)
      ls <- case lookupUri docs uri of
        Just d -> dLines <$> readIORef (docRec d)
        Nothing -> diskLines c uri
      let out' = set key (rangeFromUtf16 ls rng) out
      pure $ case get "targetSelectionRange" loc of
        Just sel -> set "targetSelectionRange" (rangeFromUtf16 ls sel) out'
        Nothing -> out'

diskLines :: Client -> Text -> IO [Text]
diskLines c uri = do
  let local = uriToPath uri
  r <- try @SomeException $ do
    mtime <- T.pack . show <$> getModificationTime local
    cache <- readIORef (cLinesCache c)
    case M.lookup local cache of
      Just (m, ls) | m == mtime -> pure ls
      _ -> do
        ls <- splitLines <$> readTextLenient local
        modifyRef (cLinesCache c) (\m -> M.insert local (mtime, ls) (if M.size m > 64 then M.empty else m))
        pure ls
  pure (either (const []) id r)
