{-# LANGUAGE MultiWayIf #-}
-- | The New project view: the templates there are, and making a project
-- (or a scratch environment) from one by running the command line as a
-- child, its progress followed through the project's events file.
--
-- Routes:
--
--   * @GET /api/templates@: @{templates, notes}@. Each template is
--     @{name, prover, description, variables [{name, default, pattern,
--     description}], origin (builtin, configured or source), source (the
--     configured path or URL, else null), steps [{name, run,
--     timeout_minutes}] (as the manifest has them, not rendered), sample,
--     trusted}@; @notes@ are the configured templates that could not be
--     loaded, one line each.
--   * @POST /api/create@ @{template, path | scratch: true, vars, confirm,
--     statement}@: @path@ is the new project's folder, an absolute path
--     that does not exist or is empty (only a @.DS_Store@ in it); @scratch@
--     makes a scratch environment instead (@statement@, optional, is its
--     goal). @vars@ is an object of text values (an empty value means the
--     default). The folder is made canonical through its deepest existing
--     parent (links resolved), and that path is checked, used to tell two
--     creates in one folder apart, and given to the child. The variables are
--     checked here (an unknown name or a value that does not match its
--     pattern is a 400 with the message), @latest@ left for the command line
--     to resolve. A template that is not trusted yet answers 409 @{error,
--     template, steps, revision}@ (the steps rendered, the template's
--     revision) until @confirm@ is that revision; a confirm of another
--     revision (the template changed since it was shown) gets a fresh 409.
--     Then @fisher project create PATH --template NAME [--var K=V ...]
--     --trust-revision REV@ (or @fisher scratch --template NAME [--var K=V
--     ...] [--statement TEXT] --trust-revision REV@), REV the revision just
--     checked: the command line runs the steps only if the template it loads
--     is still at REV, so a change between the check and the load stops it
--     ("the template changed since it was confirmed"). It runs in the Fisher home, stdin
--     from /dev/null, in its own process group, output appended to
--     @$FISHER_HOME/fisher/ui-runs/create-<stamp>.log@: @{id}@.
--   * @GET /api/create/:id@: @{id, template, mode, root, file, status,
--     running, reason, log, events}@. @root@ is the project (or the
--     scratch, once made), @file@ the sample (or the scratch's file)
--     relative to it, @events@ the events file followed.
--   * @GET /api/create/:id/events@: server-sent events, one per complete
--     line of the events file (@<project>/.fisher/create.jsonl@; for a
--     scratch, the base's, once the command line says it is building one):
--     @id:@ is the byte offset just past the line. It starts at
--     @Last-Event-ID@, else @?from=@, else 0 (0 or just past a newline). It
--     ends, once the child has exited, with @event: end@, @data: {status,
--     reason, root, file}@; an offset past the end of the file ends it at
--     once with @rewritten@ (start again from 0). Asked for at the end of a
--     run that has ended, it answers 204.
--   * @POST /api/create/:id/stop@: the child's process tree stopped
--     (SIGTERM, SIGKILL after the grace); 202, or 409 when it has ended.
--   * @POST /api/create/:id/retry@: once it has failed or was stopped,
--     @fisher project build PATH --yes@ (it resumes at the first step not
--     done), or the same create again when the project has no manifest yet
--     (or for a scratch): @{id, from}@, @from@ the offset to follow the
--     events from; 409 while another create of this server runs in the
--     folder. When the command line stopped because the template changed
--     since it was confirmed (nothing was written), the same create would
--     stop again: it answers 409 @{error, template, steps, revision,
--     request}@ as a create does, with the template as it is now and the
--     request to send again (@{template, path | scratch, vars, statement}@)
--     once its steps are confirmed.
--
-- Status: @running@, @stopping@; then @ready@ (the events file ends with
-- @ready@, even when a stop came after it; for a scratch, the command line
-- printed its root and file and exited 0), @stopped@, or @failed@ with the reason (the command line's
-- error, else the failed event's reason, else its exit code).
-- The runs are kept in memory: they are gone when the server stops.
module Fisher.UI.Api.Templates
  ( CreateEnv (..)
  , CreateRun (..)
  , Attempt (..)
  , Mode (..)
  , newCreateEnv
  , templatesApi
  , stopCreates
  , templateJson
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (SomeException, try)
import Control.Monad (forM, unless, void)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getZonedTime)
import Network.HTTP.Types
import Network.Wai
import System.Directory
import System.Exit (ExitCode (..))
import System.FilePath (isAbsolute, joinPath, makeRelative, normalise, splitDirectories, (</>))
import System.IO (IOMode (..), SeekMode (..), hFileSize, hSeek, withBinaryFile)
import Text.Read (readMaybe)

import Fisher.Config (loadConfigAt)
import Fisher.Prover.Kind (ProverKind (..))
import Fisher.Template
import Fisher.Template.Run (Event (..), eventsFile, isTrusted, readEvents)
import Fisher.UI.Api.Live (LiveEnv (..), bad, jsonBodyWith, textField)
import Fisher.UI.Api.Search (completeLines)
import Fisher.UI.Children
import Fisher.UI.Db (UiEnv (..))
import Fisher.UI.Response (Api, jsonResponse, pageHeaders)

-- | Where the run makes its environment: a project folder, or a scratch
-- (whose root the command line chooses).
data Mode = InProject FilePath | InScratch
  deriving (Eq, Show)

-- | One child of a run (the create, then each retry) and the offset in the
-- log where its output starts.
data Attempt = Attempt { atChild :: Child, atExit :: MVar ExitCode, atLogFrom :: Integer }

data CreateRun = CreateRun
  { crMode :: Mode
  , crTemplate :: Text
  , crVarArgs :: [String]
  -- ^ @--var K=V@ for each variable given
  , crStatement :: Maybe Text
  , crRevision :: Text
  -- ^ the template's revision when it was checked (@--trust-revision@)
  , crSample :: Maybe FilePath
  -- ^ the sample relative to the project (a project run)
  , crLog :: FilePath
  , crAttempt :: IORef Attempt
  , crStopping :: IORef Bool
  }

data CreateEnv = CreateEnv
  { ceLive :: LiveEnv
  -- ^ the fisher to run, the grace, the poll and keep-alive times
  , ceRuns :: IORef (M.Map Text CreateRun)
  , ceClosing :: IORef Bool
  , ceStart :: MVar ()
  -- ^ one start (or retry) at a time
  }

newCreateEnv :: LiveEnv -> IO CreateEnv
newCreateEnv live = CreateEnv live <$> newIORef M.empty <*> newIORef False <*> newMVar ()

homeOf :: CreateEnv -> FilePath
homeOf = ueHome . leUi . ceLive

-- | The trust file the child reads (@FISHER_HOME@ is the server's home).
trustPath :: CreateEnv -> FilePath
trustPath env = homeOf env </> "fisher" </> "templates-trusted.json"

templatesApi :: CreateEnv -> Api
templatesApi env r = case (requestMethod r, pathInfo r) of
  (m, ["api", "templates"]) | m == methodGet -> Just <$> listing env
  (m, ["api", "create"]) | m == methodPost -> Just <$> create env r
  (m, ["api", "create", cid]) | m == methodGet -> Just <$> withRun cid (detail env cid)
  (m, ["api", "create", cid, "events"]) | m == methodGet -> Just <$> withRun cid (events env r)
  (m, ["api", "create", cid, "stop"]) | m == methodPost -> Just <$> withRun cid (stop env)
  (m, ["api", "create", cid, "retry"]) | m == methodPost -> Just <$> withRun cid (retry env cid)
  _ -> pure Nothing
  where
    withRun cid k = M.lookup cid <$> readIORef (ceRuns env) >>= maybe (pure (bad notFound404 "no such create run")) k

configOf :: CreateEnv -> IO A.Value
configOf env = either (\(_ :: SomeException) -> A.Null) fst <$> try (loadConfigAt (homeOf env))

-- ------------------------------------------------------------- listing

proverText :: ProverKind -> Text
proverText = \case
  Lean4 -> "lean4"
  Rocq -> "rocq"
  Isabelle -> "isabelle"

stepJson :: Step -> A.Value
stepJson s = A.object ["name" A..= stepName s, "run" A..= stepRun s, "timeout_minutes" A..= stepTimeoutMinutes s]

-- | A template as the listing gives it.
templateJson :: Template -> Bool -> A.Value
templateJson t trusted =
  A.object
    [ "name" A..= tplName t, "prover" A..= proverText (tplProver t), "description" A..= tplDescription t
    , "variables" A..= [ A.object ["name" A..= varName v, "default" A..= varDefault v, "pattern" A..= varPattern v, "description" A..= varDescription v]
                       | v <- tplVariables t ]
    , "origin" A..= (case tplOrigin t of BuiltIn -> "builtin"; Configured _ -> "configured"; Source _ -> "source" :: Text)
    , "source" A..= (case tplOrigin t of BuiltIn -> Nothing; Configured s -> Just s; Source s -> Just s)
    , "steps" A..= map stepJson (tplSteps t), "sample" A..= tplSample t, "trusted" A..= trusted ]

trusted' :: CreateEnv -> Template -> IO Bool
trusted' env t = either (\(_ :: SomeException) -> False) id <$> try (isTrusted (trustPath env) t)

listing :: CreateEnv -> IO Response
listing env = do
  cfg <- configOf env
  r <- try (listTemplatesWithNotes cfg)
  case r of
    Left (e :: SomeException) -> pure (bad internalServerError500 (T.pack (show e)))
    Right (ts, notes) -> do
      rows <- forM ts $ \t -> templateJson t <$> trusted' env t
      pure (jsonResponse ok200 (A.object ["templates" A..= rows, "notes" A..= notes]))

-- ------------------------------------------------------------- create

boolField :: A.Key -> A.Object -> Maybe Bool
boolField k o = case KM.lookup k o of
  Just (A.Bool b) -> Just b
  _ -> Nothing

-- | The variables given: an object of text values, the empty ones left out.
varsOf :: A.Object -> Either Text [(Text, Text)]
varsOf o = case KM.lookup "vars" o of
  Nothing -> Right []
  Just A.Null -> Right []
  Just (A.Object m) -> fmap concat . forM (KM.toList m) $ \(k, v) -> case v of
    A.String s
      | T.null (T.strip s) -> Right []
      | otherwise -> Right [(K.toText k, T.strip s)]
    _ -> Left "vars is an object of text values"
  Just _ -> Left "vars is an object of text values"

usageText :: Text
usageText = "send {template, path | scratch: true, vars, confirm} as JSON"

-- | The folder for a new project, or why it will not do.
checkFolder :: Text -> IO (Either Text FilePath)
checkFolder p0
  | T.null p || T.any (`elem` ("\r\n\0" :: String)) p = pure (Left "the folder is one path")
  | not (isAbsolute (T.unpack p)) = pure (Left ("the folder must be an absolute path (got `" <> p <> "`)"))
  | otherwise = do
      path <- canonicalFolder (T.unpack p)
      isFile <- doesFileExist path
      isDir <- doesDirectoryExist path
      entries <- if isDir then either (\(_ :: SomeException) -> ["?"]) id <$> try (listDirectory path) else pure []
      pure $ case () of
        _ | isFile -> Left ("`" <> p <> "` is a file")
          | any (/= ".DS_Store") entries -> Left ("`" <> p <> "` is not empty; choose a new or empty folder")
          | otherwise -> Right path
  where
    p = T.strip p0

-- | The path through its deepest existing parent made canonical (links
-- resolved), the rest kept as given.
canonicalFolder :: FilePath -> IO FilePath
canonicalFolder path = go (splitDirectories (normalise path)) []
  where
    go parts rest
      | null parts = pure (joinPath ("/" : rest))
      | otherwise = do
          let here = joinPath parts
          there <- doesPathExist here
          if there
            then either (\(_ :: SomeException) -> joinPath (parts <> rest)) (\c -> joinPath (c : rest)) <$> try (canonicalizePath here)
            else go (init parts) (last parts : rest)

create :: CreateEnv -> Request -> IO Response
create env r = do
  body <- jsonBodyWith 65536 r
  case body of
    Nothing -> pure (bad badRequest400 usageText)
    Just o -> case (textField "template" o, varsOf o) of
      (Nothing, _) -> pure (bad badRequest400 usageText)
      (_, Left e) -> pure (bad badRequest400 e)
      (Just name, Right vars) -> do
        let scratch = boolField "scratch" o == Just True
            path = T.strip <$> textField "path" o
            statement = if scratch then mfilter' (not . T.null) (T.strip <$> textField "statement" o) else Nothing
            confirmed = textField "confirm" o
        case (scratch, path) of
          (True, Just p) | not (T.null p) -> pure (bad badRequest400 "give a folder or scratch, not both")
          (True, _) -> go name InScratch vars statement confirmed
          (False, Just p) | not (T.null p) -> checkFolder p >>= either (pure . bad badRequest400) (\d -> go name (InProject d) vars Nothing confirmed)
          (False, _) -> pure (bad badRequest400 "give the project's folder (an absolute path) or scratch: true")
  where
    mfilter' f m = case m of
      Just x | f x -> Just x
      _ -> Nothing
    go name mode vars statement confirmed = do
      cfg <- configOf env
      findTemplate cfg name >>= \case
        Left e -> pure (bad badRequest400 e)
        Right t -> do
          -- a scratch's project is Scratch unless given (as fisher scratch does)
          let target = case mode of
                InProject d -> d
                InScratch -> "/Scratch"
          -- latest is left for the command line to resolve (no network here)
          resolveVariables t target vars (\_ -> pure (Right "latest")) >>= \case
            Left e -> pure (bad badRequest400 e)
            Right vals -> do
              ok <- trusted' env t
              rev <- either (\(e :: SomeException) -> "unreadable: " <> T.pack (show e)) id <$> try (templateRevision t)
              if not ok && confirmed /= Just rev
                then
                  pure $ confirmResponse
                    (case confirmed of
                       Just _ -> "Template `" <> tplName t <> "` changed since its steps were shown; confirm them again"
                       Nothing -> "Template `" <> tplName t <> "`" <> sourceNote t <> " runs these steps in the new project; confirm them to go on")
                    t rev vals []
                else do
                  let sample = case mode of
                        InProject _ -> either (const Nothing) Just (renderSample t vals)
                        InScratch -> Nothing
                      varArgs = concat [["--var", T.unpack (k <> "=" <> v)] | (k, v) <- vars]
                  launch env mode (tplName t) varArgs statement rev sample
    sourceNote t = case tplOrigin t of
      BuiltIn -> ""
      Configured s -> " (" <> s <> ")"
      Source s -> " (" <> s <> ")"

-- | A 409 asking for the steps to be confirmed: the template's steps
-- rendered with the values, and its revision.
confirmResponse :: Text -> Template -> Text -> [(Text, Text)] -> [(K.Key, A.Value)] -> Response
confirmResponse msg t rev vals extra =
  jsonResponse conflict409 . A.object $
    [ "error" A..= msg
    , "template" A..= tplName t
    , "revision" A..= rev
    , "steps" A..= map stepJson (either (const (tplSteps t)) id (renderSteps t vals)) ]
      <> extra

-- | The command line that makes the environment.
createArgv :: CreateEnv -> Mode -> Text -> [String] -> Maybe Text -> Text -> [String]
createArgv env mode name varArgs statement rev = case mode of
  InProject d -> [fisher, "project", "create", d, "--template", T.unpack name] <> varArgs <> trust
  InScratch ->
    [fisher, "scratch", "--template", T.unpack name] <> varArgs
      <> maybe [] (\s -> ["--statement", T.unpack s]) statement <> trust
  where
    fisher = leFisher (ceLive env)
    trust = ["--trust-revision", T.unpack rev]

runArgv :: CreateEnv -> CreateRun -> [String]
runArgv env run = createArgv env (crMode run) (crTemplate run) (crVarArgs run) (crStatement run) (crRevision run)

-- | Start a child in the Fisher home, its output appended to the log.
spawnAttempt :: CreateEnv -> FilePath -> [String] -> IO Attempt
spawnAttempt env logp argv = do
  let home = homeOf env
  from <- fileSize logp
  (ch, exited) <- spawnChild (Just logp) home argv [("FISHER_HOME", home)]
  pure (Attempt ch exited from)

-- | Whether another create of this server is running in the folder.
busyIn :: CreateEnv -> Maybe CreateRun -> Mode -> IO Bool
busyIn env self = \case
  InProject d -> do
    runs <- M.elems <$> readIORef (ceRuns env)
    let others = [x | x <- runs, crMode x == InProject d, maybe True (\me -> crAttempt me /= crAttempt x) self]
    or <$> mapM isRunning others
  InScratch -> pure False

launch :: CreateEnv -> Mode -> Text -> [String] -> Maybe Text -> Text -> Maybe FilePath -> IO Response
launch env mode name varArgs statement rev sample = do
  made <- withMVar (ceStart env) $ \_ -> do
    closing <- readIORef (ceClosing env)
    busy <- busyIn env Nothing mode
    if
      | closing -> pure (Left (serviceUnavailable503, "fisher ui is stopping"))
      | busy -> pure (Left (conflict409, "a create is already going in that folder"))
      | otherwise -> do
          stamp <- formatTime defaultTimeLocale "%Y%m%d-%H%M%S" <$> getZonedTime
          runs <- readIORef (ceRuns env)
          let fresh n = let s = if n == (0 :: Int) then stamp else stamp <> "-" <> show n in if M.member ("c." <> T.pack s) runs then fresh (n + 1) else s
              stampN = fresh 0
              cid = "c." <> T.pack stampN
              logp = homeOf env </> "fisher" </> "ui-runs" </> ("create-" <> stampN <> ".log")
          res <- try (spawnAttempt env logp (createArgv env mode name varArgs statement rev))
          case res of
            Left (e :: SomeException) -> pure (Left (internalServerError500, T.pack (show e)))
            Right at -> do
              run <- CreateRun mode name varArgs statement rev sample logp <$> newIORef at <*> newIORef False
              atomicModifyIORef' (ceRuns env) (\m -> (M.insert cid run m, ()))
              pure (Right cid)
  pure $ case made of
    Left (s, e) -> bad s e
    Right cid -> jsonResponse ok200 (A.object ["id" A..= cid])

isRunning :: CreateRun -> IO Bool
isRunning run = readIORef (crAttempt run) >>= fmap isNothing . tryReadMVar . atExit

-- ------------------------------------------------------------- state

-- | The bytes of the file from an offset (none when it is not there).
readFrom :: FilePath -> Integer -> IO B.ByteString
readFrom p off = either (\(_ :: SomeException) -> "") id <$> try (withBinaryFile p ReadMode $ \h -> do
  size <- hFileSize h
  if size <= off then pure "" else hSeek h AbsoluteSeek off >> B.hGet h (fromIntegral (size - off)))

fileSize :: FilePath -> IO Integer
fileSize p = either (\(_ :: SomeException) -> 0) id <$> try (getFileSize p)

-- | What a run has come to.
data Outcome = Outcome
  { ocStatus :: Text
  , ocRunning :: Bool
  , ocReason :: Maybe Text
  , ocRoot :: Maybe FilePath
  , ocFile :: Maybe FilePath
  -- ^ relative to the root
  , ocEvents :: Maybe FilePath
  }

-- | The current child's output, as lines.
outputOf :: CreateRun -> Attempt -> IO [Text]
outputOf run at = T.lines . TE.decodeUtf8With TEE.lenientDecode <$> readFrom (crLog run) (atLogFrom at)

-- | The value after a prefix on the last line that has it.
lastAfter :: Text -> [Text] -> Maybe Text
lastAfter pre ls = case reverse (mapMaybe (T.stripPrefix pre . T.strip) ls) of
  x : _ -> Just (T.strip x)
  [] -> Nothing

-- | The command line's error: from its last @(>_<)@ line to the end (20
-- lines at most).
errorOf :: [Text] -> Maybe Text
errorOf ls = case break (T.isPrefixOf "(>_<)" . T.strip) (reverse ls) of
  (_, []) -> Nothing
  (after, l : _) ->
    let first = T.strip (fromMaybe l (T.stripPrefix "(>_<)" (T.strip l)))
     in Just (T.intercalate "\n" (take 20 (first : reverse (filter (not . T.null . T.strip) after))))

stateOf :: CreateRun -> IO Outcome
stateOf run = do
  at <- readIORef (crAttempt run)
  ex <- tryReadMVar (atExit at)
  stopping <- readIORef (crStopping run)
  out <- outputOf run at
  let base = case lastAfter "Building the base for " out of
        Just x | " (once) in " `T.isInfixOf` x -> Just (T.unpack (snd (T.breakOnEnd " (once) in " x)))
        _ -> Nothing
      evFile = case crMode run of
        InProject d -> Just (eventsFile d)
        InScratch -> eventsFile <$> base
      (root, file) = case crMode run of
        InProject d -> (Just d, crSample run)
        InScratch ->
          let r0 = T.unpack <$> lastAfter "Scratch: " out
              f0 = T.unpack <$> lastAfter "File: " out
           in (r0, (\r' f' -> if isAbsolute f' then makeRelative r' f' else f') <$> r0 <*> f0)
      exitNote c = "fisher exited" <> case c of
        ExitSuccess -> " before it was ready"
        ExitFailure n -> " with code " <> T.pack (show n)
      failed why = pure (Outcome "failed" False (Just why) root file evFile)
  case ex of
    Nothing -> pure (Outcome (if stopping then "stopping" else "running") True Nothing root file evFile)
    Just c -> do
      -- ready in the events (or a scratch's root and file) wins over a
      -- stop that came after it
      lastEv <- case crMode run of
        InProject d -> either (\(_ :: SomeException) -> Nothing) (\evs -> if null evs then Nothing else Just (last evs)) <$> try (readEvents d)
        InScratch -> pure Nothing
      let scratchReady = crMode run == InScratch && c == ExitSuccess && isJust root && isJust file
      case lastEv of
        Just Ready -> pure (Outcome "ready" False Nothing root file evFile)
        _ | scratchReady -> pure (Outcome "ready" False Nothing root file evFile)
          | stopping -> pure (Outcome "stopped" False (errorOf out) root file evFile)
        Just (Failed why) -> failed (fromMaybe why (errorOf out))
        _ -> failed (fromMaybe (exitNote c) (errorOf out))

detail :: CreateEnv -> Text -> CreateRun -> IO Response
detail _ cid run = do
  o <- stateOf run
  pure . jsonResponse ok200 $
    A.object
      [ "id" A..= cid, "template" A..= crTemplate run
      , "mode" A..= (case crMode run of InProject _ -> "project"; InScratch -> "scratch" :: Text)
      , "root" A..= ocRoot o, "file" A..= ocFile o, "status" A..= ocStatus o, "running" A..= ocRunning o
      , "reason" A..= ocReason o, "log" A..= crLog run, "events" A..= ocEvents o ]

endJson :: Outcome -> A.Value
endJson o = A.object ["status" A..= ocStatus o, "reason" A..= ocReason o, "root" A..= ocRoot o, "file" A..= ocFile o]

-- ------------------------------------------------------------- stream

-- | Where a stream starts: Last-Event-ID, else ?from=, else 0.
cursorOf :: Request -> Maybe Integer
cursorOf r = case lookup "Last-Event-ID" (requestHeaders r) of
  Just h -> readMaybe (BC.unpack h)
  Nothing -> case lookup "from" (queryString r) of
    Just (Just v) -> readMaybe (BC.unpack v)
    Just Nothing -> Nothing
    Nothing -> Just 0

-- | Whether the offset starts a line of the file (0, or just past a newline).
atLineStart :: FilePath -> Integer -> IO Bool
atLineStart p off
  | off == 0 = pure True
  | off < 0 = pure False
  | otherwise = (== "\n") <$> either (\(_ :: SomeException) -> "") id <$> try (withBinaryFile p ReadMode $ \h -> hSeek h AbsoluteSeek (off - 1) >> B.hGet h 1)

events :: CreateEnv -> Request -> CreateRun -> IO Response
events env r run = case cursorOf r of
  Nothing -> pure (bad badRequest400 "the cursor is a byte offset")
  Just c0 -> do
    o0 <- stateOf run
    size0 <- maybe (pure 0) fileSize (ocEvents o0)
    lineOk <- if c0 > size0 then pure True else maybe (pure (c0 == 0)) (`atLineStart` c0) (ocEvents o0)
    if
      | c0 < 0 || not lineOk -> pure (bad badRequest400 "the offset is not the start of a line of the events file")
      | not (ocRunning o0) && c0 == size0 -> pure (responseLBS noContent204 (("Cache-Control", "no-store") : pageHeaders) "")
      | otherwise -> pure . responseStream ok200 sseHeaders $ \write flush -> do
          write "retry: 1000\n\n" >> flush
          let send ls = unless (null ls) (mapM_ (\(off, l) -> write ("id: " <> BB.integerDec off <> "\ndata: " <> BB.byteString l <> "\n\n")) ls >> flush)
              ending v = write ("event: end\ndata: " <> BB.lazyByteString (A.encode v) <> "\n\n") >> flush
              rewritten = ending (A.object ["status" A..= ("rewritten" :: Text)])
              -- the complete lines past the cursor (Nothing: the file is
              -- shorter than what was read of it)
              readSend p c = do
                size <- maybe (pure 0) fileSize p
                if size < c
                  then pure Nothing
                  else do
                    chunk <- maybe (pure "") (`readFrom` c) p
                    let ls = completeLines c chunk
                    send ls
                    pure (Just (if null ls then c else fst (last ls), not (null ls)))
              -- the last lines, read again 200 ms apart, then the end
              finish c k = do
                o <- stateOf run
                readSend (ocEvents o) c >>= \case
                  Nothing -> rewritten
                  Just (c', _)
                    | k > (0 :: Int) -> threadDelay 200000 >> finish c' (k - 1)
                    | otherwise -> ending (endJson o)
              loop c quiet = do
                -- the state first: once the child has exited, every line
                -- is in the file by the time it is read
                o <- stateOf run
                readSend (ocEvents o) c >>= \case
                  Nothing -> rewritten
                  Just (c', got)
                    | not (ocRunning o) -> finish c' 2
                    | otherwise -> do
                        let quiet' = if got then 0 else quiet + lePoll (ceLive env)
                        q <- if quiet' >= leKeepAlive (ceLive env) then write ": keep-alive\n\n" >> flush >> pure 0 else pure quiet'
                        threadDelay (lePoll (ceLive env))
                        loop c' q
          if c0 > size0 then rewritten else loop c0 0
  where
    sseHeaders =
      [ (hContentType, "text/event-stream; charset=utf-8"), ("Cache-Control", "no-store")
      , ("X-Accel-Buffering", "no") ] <> pageHeaders

-- ------------------------------------------------------------- stop, retry

stopRun :: CreateEnv -> CreateRun -> IO ()
stopRun env run = do
  atomicModifyIORef' (crStopping run) (const (True, ()))
  at <- readIORef (crAttempt run)
  void (try (stopChildWith (leGrace (ceLive env)) (atChild at)) :: IO (Either SomeException ()))

stop :: CreateEnv -> CreateRun -> IO Response
stop env run = do
  going <- isRunning run
  if not going
    then pure (bad conflict409 "the create has ended")
    else do
      atomicModifyIORef' (crStopping run) (const (True, ()))
      _ <- forkIO (stopRun env run)
      pure (jsonResponse accepted202 (A.object ["status" A..= ("stopping" :: Text)]))

-- | The variables a run was started with, from its @--var K=V@ pairs.
varsOfRun :: CreateRun -> [(Text, Text)]
varsOfRun run = go (crVarArgs run)
  where
    go = \case
      ("--var" : kv : rest) -> let (k, v) = T.breakOn "=" (T.pack kv) in (k, T.drop 1 v) : go rest
      (_ : rest) -> go rest
      [] -> []

-- | For a run the command line stopped because its template changed since
-- it was confirmed: the 409 that asks to confirm the template as it is now.
reconfirm :: CreateEnv -> CreateRun -> IO Response
reconfirm env run = do
  cfg <- configOf env
  findTemplate cfg (crTemplate run) >>= \case
    Left e -> pure (bad badRequest400 e)
    Right t -> do
      let vars = varsOfRun run
          target = case crMode run of
            InProject d -> d
            InScratch -> "/Scratch"
      vals <- either (const vars) id <$> resolveVariables t target vars (\_ -> pure (Right "latest"))
      rev <- either (\(e :: SomeException) -> "unreadable: " <> T.pack (show e)) id <$> try (templateRevision t)
      let request = A.object $
            [ "template" A..= tplName t, "vars" A..= A.object [K.fromText k A..= v | (k, v) <- vars] ]
              <> (case crMode run of
                    InProject d -> ["path" A..= d]
                    InScratch -> ["scratch" A..= True])
              <> maybe [] (\st -> ["statement" A..= st]) (crStatement run)
      pure (confirmResponse ("Template `" <> tplName t <> "` changed since it was confirmed; confirm its steps again") t rev vals ["request" A..= request])

retry :: CreateEnv -> Text -> CreateRun -> IO Response
retry env cid run = do
  made <- withMVar (ceStart env) $ \_ -> do
    closing <- readIORef (ceClosing env)
    going <- isRunning run
    busy <- busyIn env (Just run) (crMode run)
    o <- stateOf run
    manifest <- case crMode run of
      InProject d -> doesFileExist (d </> ".fisher" </> "project.yaml")
      InScratch -> pure False
    let changed = maybe False ("changed since it was confirmed" `T.isInfixOf`) (ocReason o)
    if
      | closing -> pure (Left (bad serviceUnavailable503 "fisher ui is stopping"))
      | going -> pure (Left (bad conflict409 "the create is still going"))
      | busy -> pure (Left (bad conflict409 "a create is already going in that folder"))
      | ocStatus o == "ready" -> pure (Left (bad conflict409 "it is ready; there is nothing to retry"))
      -- the same create would stop the same way: confirm the template again
      | changed && not manifest -> Left <$> reconfirm env run
      | otherwise -> do
          let argv = if manifest then [leFisher (ceLive env), "project", "build", projectDir, "--yes"] else runArgv env run
              projectDir = case crMode run of
                InProject d -> d
                InScratch -> ""
          -- a project's build appends to its events; a scratch may build
          -- its base again from the start
          from <- case crMode run of
            InProject d -> fileSize (eventsFile d)
            InScratch -> pure 0
          res <- try (spawnAttempt env (crLog run) argv)
          case res of
            Left (e :: SomeException) -> pure (Left (bad internalServerError500 (T.pack (show e))))
            Right at -> do
              writeIORef (crStopping run) False
              writeIORef (crAttempt run) at
              pure (Right from)
  pure $ case made of
    Left resp -> resp
    Right from -> jsonResponse ok200 (A.object ["id" A..= cid, "from" A..= from])

-- | Stop every create still going and wait for them; none starts after
-- this. The server calls it as it stops.
stopCreates :: CreateEnv -> IO ()
stopCreates env = do
  atomicModifyIORef' (ceClosing env) (const (True, ()))
  runs <- M.elems <$> readIORef (ceRuns env)
  dones <- forM runs $ \run -> do
    done <- newEmptyMVar
    going <- isRunning run
    if going then void (forkIO (stopRun env run >> putMVar done ())) else putMVar done ()
    pure done
  mapM_ takeMVar dones
