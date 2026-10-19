-- | @GET /api/sessions@ (with filters @project@, @prover@, @model@,
-- @strategy@) and @GET /api/sessions/:id@.
--
-- state.db does not record a session's project, so it is found from the
-- replay log: the first absolute path a tool touched that lies in a Fisher
-- project (its manifest's kind is the prover) or in a prover project root
-- (lakefile, _CoqProject, ROOT). The project is that directory's path. A compression child writes no log of its
-- own (the agent keeps logging to the log it opened), so a child takes its
-- project and strategy from its root's log and counts no dollars of its own.
module Fisher.UI.Api.Sessions
  ( sessionsApi
  , SessionInfo (..)
  , sessionInfos
  , sessionInfo
  , logFor
  , chainOf
  , listSessions
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (filterM)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Yaml as Y
import Network.HTTP.Types (badRequest400, notFound404, ok200)
import Network.Wai (Request (..))
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist)
import System.FilePath (takeDirectory, (</>))

import Fisher.Compat (projectDirNames)
import Fisher.Prover (Prover (..), detectProver, prover)
import Fisher.UI.Db
import Fisher.UI.Response (Api, jsonResponse)

data SessionInfo = SessionInfo
  { siRow :: SessionRow
  , siProject :: Maybe Text
  , siProver :: Maybe Text
  , siStrategy :: Text
  , siCost :: Cost
  , siResult :: Maybe Text
  , siLog :: Maybe FilePath
  -- ^ the log read for it (its own, or its root's)
  }

instance A.ToJSON SessionInfo where
  toJSON si =
    let r = siRow si
     in A.object
          [ "id" A..= srId r, "title" A..= srTitle r, "project" A..= siProject si, "prover" A..= siProver si
          , "model" A..= srModel r, "strategy" A..= siStrategy si, "started" A..= srStarted r
          , "turns" A..= srTurns r, "dollars" A..= costDollars (siCost si), "result" A..= siResult si ]

sessionsApi :: UiEnv -> Api
sessionsApi env r = case pathInfo r of
  ["api", "sessions"] -> Just <$> do
    let q k = case lookup k (queryString r) of
          -- bytes that are not UTF-8 (a hand-made URL) match nothing
          Just (Just v) | v /= "" -> Just (TE.decodeUtf8With TEE.lenientDecode v)
          _ -> Nothing
    xs <- listSessions env (q "project") (q "prover") (q "model") (q "strategy")
    pure (jsonResponse ok200 xs)
  ["api", "sessions", sid]
    | not (safeId sid) -> pure (Just (jsonResponse badRequest400 (err "bad session id")))
    | otherwise -> Just <$> detail sid
  _ -> pure Nothing
  where
    detail sid = do
      here <- doesFileExist (stateDb env)
      if not here
        then pure (jsonResponse notFound404 (err "no such session"))
        else withRo (stateDb env) $ \c -> do
          rows <- allSessions c
          case [x | x <- rows, srId x == sid] of
            [] -> pure (jsonResponse notFound404 (err "no such session"))
            (row : _) -> do
              si <- sessionInfo env rows row
              ms <- sessionMessages c sid
              let kids = [srId x | x <- rows, srParent x == Just sid]
              pure . jsonResponse ok200 $
                A.object
                  [ "session" A..= si, "messages" A..= map message ms
                  , "cost" A..= siCost si, "children" A..= kids ]
    err :: Text -> A.Value
    err m = A.object ["error" A..= m]

message :: MessageRow -> A.Value
message m =
  A.object
    [ "role" A..= mrRole m, "content" A..= mrContent m
    , "toolCalls" A..= (mrToolCalls m >>= A.decodeStrict . TE.encodeUtf8 :: Maybe A.Value)
    , "toolName" A..= mrToolName m, "toolCallId" A..= mrToolCallId m, "at" A..= mrAt m ]

-- | Every session with its derived fields, filtered (a filter given is an
-- exact match).
listSessions :: UiEnv -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> IO [SessionInfo]
listSessions env project prov model strategy = do
  here <- doesFileExist (stateDb env)
  if not here
    then pure []
    else do
      rows <- withRo (stateDb env) allSessions
      filter keep <$> sessionInfos env rows
  where
    keep si =
      matches project (siProject si) && matches prov (siProver si)
        && matches model (srModel (siRow si)) && matches strategy (Just (siStrategy si))
    matches want have = maybe True (\w -> have == Just w) want

sessionInfos :: UiEnv -> [SessionRow] -> IO [SessionInfo]
sessionInfos env rows = mapM (sessionInfo env rows) rows

-- | The session's root first, then every session below it.
chainOf :: [SessionRow] -> Text -> [Text]
chainOf rows sid = down (root sid (length rows))
  where
    parentOf s = case [p | x <- rows, srId x == s, Just p <- [srParent x]] of
      (p : _) -> Just p
      [] -> Nothing
    root s n = case parentOf s of
      Just p | n > 0 -> root p (n - 1)
      _ -> s
    down s = s : concatMap down [srId x | x <- rows, srParent x == Just s]

-- | The log for a session: its own, else the nearest ancestor's.
logFor :: UiEnv -> [SessionRow] -> Text -> IO (Maybe (FilePath, Bool))
logFor env rows sid = go sid True (length rows)
  where
    go s own n
      | not (safeId s) = pure Nothing
      | otherwise = do
          let p = replayLogPath (ueHome env) s
          here <- doesFileExist p
          if here
            then pure (Just (p, own))
            else case [q | x <- rows, srId x == s, Just q <- [srParent x]] of
              (q : _) | n > 0 -> go q False (n - 1)
              _ -> pure Nothing

sessionInfo :: UiEnv -> [SessionRow] -> SessionRow -> IO SessionInfo
sessionInfo env rows row = do
  lg <- logFor env rows (srId row)
  s <- maybe (pure emptySummary) (logSummary (ueLogs env) . fst) lg
  proj <- projectOf env (lsPaths s)
  let inChain = isJust (srParent row) || srEndReason row == Just "compression" || any ((== Just (srId row)) . srParent) rows
      strategy = fromMaybe (if inChain then "compress" else "none") (lsStrategy s)
      own = maybe False snd lg
      result = case lsResult s of
        Just x | own -> Just x
        _ -> srEndReason row
  pure
    SessionInfo
      { siRow = row, siProject = T.pack . fst <$> proj, siProver = snd <$> proj, siStrategy = strategy
      , siCost = if own then lsCost s else mempty, siResult = result, siLog = fst <$> lg }

-- | The first path that lies in a project: its root and prover.
projectOf :: UiEnv -> [FilePath] -> IO (Maybe (FilePath, Text))
projectOf env = \case
  [] -> pure Nothing
  (p : ps) -> do
    isDir <- doesDirectoryExist p
    let dir = if isDir then p else takeDirectory p
    found <- cachedDir dir
    maybe (projectOf env ps) (pure . Just) found
  where
    cachedDir dir = do
      known <- M.lookup dir <$> readIORef (ueProjects env)
      case known of
        Just v -> pure v
        Nothing -> do
          v <- lookupDir dir
          modifyIORef' (ueProjects env) (M.insert dir v)
          pure v
    lookupDir dir = do
      exists <- doesDirectoryExist dir
      if not exists
        then pure Nothing
        else do
          start <- canonicalizePath dir
          g <- manifestAbove start
          case g of
            Just v -> pure (Just v)
            Nothing -> do
              d <- try (detectProver start)
              pure $ case d of
                Right (Right (Just (k, root))) -> Just (root, pvName (prover k))
                Left (_ :: SomeException) -> Nothing
                _ -> Nothing

-- | The nearest directory at or above holding @.fisher/project.yaml@ (or,
-- from before the rename, @.gauss/project.yaml@), with
-- the manifest's kind (lean4 when it names none). Read here rather than by
-- 'Fisher.Project.loadProject', which creates the project's runtime
-- directories.
manifestAbove :: FilePath -> IO (Maybe (FilePath, Text))
manifestAbove d = do
  found <- filterM doesFileExist [d </> n </> "project.yaml" | n <- projectDirNames]
  case found of
    m : _ -> do
      v <- either (\(_ :: SomeException) -> Nothing) Just <$> try (Y.decodeFileThrow m)
      let kind = case v of
            Just (A.Object o) | Just (A.String k) <- KM.lookup "kind" o, not (T.null k) -> k
            _ -> "lean4"
      pure (Just (d, kind))
    [] ->
      let up = takeDirectory d
       in if up == d then pure Nothing else manifestAbove up
