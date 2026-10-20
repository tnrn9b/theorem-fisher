-- | Running a template's steps in a new project: each step in the project
-- directory with stdin closed, in a process group of its own, under its
-- time limit. Output goes to @.fisher/create.log@, events to
-- @.fisher/create.jsonl@ (one JSON object per line), and the steps that
-- finished are recorded under @template.done@ in @.fisher/project.yaml@, so
-- a second run picks up at the first step not done.
--
-- Trust: a configured template runs its own commands, so it is run only
-- once its name and revision are in the trust file
-- (@$FISHER_HOME/fisher/templates-trusted.json@, a JSON object from
-- template name to revision). Built-ins are always trusted.
module Fisher.Template.Run
  ( -- * Events
    Event (..)
  , eventsFile
  , logFile
  , writeEvent
  , readEvents
  , tailLimit
    -- * Steps
  , runSteps
  , runStepsWith
  , doneSteps
    -- * Trust
  , trustFile
  , isTrusted
  , recordTrust
  , isTrustedRevision
  , recordTrustRevision
  ) where

import Control.Concurrent (forkIO, myThreadId, newEmptyMVar, putMVar, readMVar, threadDelay)
import Control.Exception (IOException, SomeException, mask, onException, try)
import Control.Monad (unless, void)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), (.:), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Vector as V
import qualified Data.Yaml as Y
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO (Handle, IOMode (..), hClose, hFlush, withBinaryFile)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (sigKILL, signalProcessGroup)
import System.Process (CreateProcess (..), StdStream (..), createPipe, createProcess, getPid, getProcessExitCode, proc, waitForProcess, ProcessHandle)
import System.Timeout (timeout)

import Fisher.Paths (fisherDirName, getFisherHome)
import Fisher.Project (renderManifest)
import Fisher.Template (Origin (..), Step (..), Template (..), templateRevision)

-- | What happened while a project was made, in the order it happened.
data Event
  = StepStart Text
  | StepEnd Text Int [Text]
  -- ^ step, exit code (-1 at the time limit), the last 'tailLimit' lines of its output
  | CheckEnd Bool [Text]
  -- ^ the sample check passed, and its problems
  | Ready
  | Failed Text
  deriving (Eq, Show)

instance ToJSON Event where
  toJSON = \case
    StepStart s -> A.object ["event" .= ("step start" :: Text), "step" .= s]
    StepEnd s n t -> A.object ["event" .= ("step end" :: Text), "step" .= s, "exit" .= n, "tail" .= t]
    CheckEnd ok ps -> A.object ["event" .= ("check" :: Text), "ok" .= ok, "problems" .= ps]
    Ready -> A.object ["event" .= ("ready" :: Text)]
    Failed r -> A.object ["event" .= ("failed" :: Text), "reason" .= r]
  -- in the documented key order, which 'toJSON' (a map) would sort
  toEncoding = \case
    StepStart s -> A.pairs ("event" .= ("step start" :: Text) <> "step" .= s)
    StepEnd s n t -> A.pairs ("event" .= ("step end" :: Text) <> "step" .= s <> "exit" .= n <> "tail" .= t)
    CheckEnd ok ps -> A.pairs ("event" .= ("check" :: Text) <> "ok" .= ok <> "problems" .= ps)
    Ready -> A.pairs ("event" .= ("ready" :: Text))
    Failed r -> A.pairs ("event" .= ("failed" :: Text) <> "reason" .= r)

instance FromJSON Event where
  parseJSON = A.withObject "event" $ \o -> (o .: "event" :: AT.Parser Text) >>= \case
    "step start" -> StepStart <$> o .: "step"
    "step end" -> StepEnd <$> o .: "step" <*> o .: "exit" <*> o .: "tail"
    "check" -> CheckEnd <$> o .: "ok" <*> o .: "problems"
    "ready" -> pure Ready
    "failed" -> Failed <$> o .: "reason"
    other -> fail ("unknown event " <> T.unpack other)

-- | @.fisher/create.jsonl@ under a project directory.
eventsFile :: FilePath -> FilePath
eventsFile project = project </> fisherDirName </> "create.jsonl"

-- | @.fisher/create.log@ under a project directory.
logFile :: FilePath -> FilePath
logFile project = project </> fisherDirName </> "create.log"

-- | How many lines of a step's output its end event carries.
tailLimit :: Int
tailLimit = 40

-- | Append one event, as one line, to the project's events file.
writeEvent :: FilePath -> Event -> IO ()
writeEvent project ev = do
  createDirectoryIfMissing True (project </> fisherDirName)
  BS.appendFile (eventsFile project) (BL.toStrict (A.encode ev) <> "\n")

-- | The events written so far (none when there is no file). A line that
-- does not parse (one being written, say) is left out.
readEvents :: FilePath -> IO [Event]
readEvents project = do
  e <- doesFileExist (eventsFile project)
  if not e
    then pure []
    else do
      ls <- BC.lines <$> BS.readFile (eventsFile project)
      pure [ev | l <- ls, Just ev <- [A.decodeStrict l]]

manifestPath :: FilePath -> FilePath
manifestPath project = project </> fisherDirName </> "project.yaml"

-- | The steps recorded done in the project's manifest (@template.done@).
-- A missing manifest has none; one that does not parse, or is not a
-- mapping, is an error.
doneSteps :: FilePath -> IO (Either Text [Text])
doneSteps project = fmap doneIn <$> readManifest project

-- | The manifest as a mapping. Only a missing (or empty) file reads as
-- empty; anything else that is not a mapping is refused, so a manifest
-- that does not parse is never overwritten.
readManifest :: FilePath -> IO (Either Text Value)
readManifest project = do
  e <- doesFileExist (manifestPath project)
  if not e
    then pure (Right (Object mempty))
    else do
      r <- Y.decodeFileEither (manifestPath project) :: IO (Either Y.ParseException Value)
      pure $ case r of
        Right v@(Object _) -> Right v
        Right Null -> Right (Object mempty)
        Right _ -> Left (cannotRead "not a mapping")
        Left err -> Left (cannotRead (T.unwords (T.words (T.pack (Y.prettyPrintParseException err)))))
  where
    cannotRead why = "cannot read .fisher/project.yaml: " <> why

doneIn :: Value -> [Text]
doneIn = \case
  Object o | Just (Object t) <- KM.lookup "template" o, Just (Array a) <- KM.lookup "done" t ->
    [s | String s <- V.toList a]
  _ -> []

-- | Add a step to @template.done@, keeping the rest of the manifest.
markDone :: FilePath -> Text -> IO (Either Text ())
markDone project name = readManifest project >>= traverse (\m -> do
  let done' = doneIn m <> [name | name `notElem` doneIn m]
      top = case m of Object o -> o; _ -> mempty
      tpl = case KM.lookup "template" top of Just (Object t) -> t; _ -> mempty
      m' = Object (KM.insert "template" (Object (KM.insert "done" (toJSON done') tpl)) top)
  writeAtomic (manifestPath project) (renderManifest m'))

writeAtomic :: FilePath -> BS.ByteString -> IO ()
writeAtomic path bytes = do
  createDirectoryIfMissing True (takeDirectory path)
  pid <- getProcessID
  tid <- myThreadId
  let tmp = path <> ".tmp-" <> show pid <> "-" <> filter (/= ' ') (show tid)
  BS.writeFile tmp bytes
  renameFile tmp path

-- | 'runStepsWith' with real minutes.
runSteps :: FilePath -> [Step] -> (Text -> IO ()) -> IO (Either Text ())
runSteps = runStepsWith 60

-- | Run the steps not yet recorded done, in order, in the project
-- directory, stopping at the first that fails. @scale@ is the number of
-- seconds in one of a step's @timeout_minutes@ (60, except in tests). The
-- sink gets one progress line per step, which names the log (the step's
-- output goes only there).
--
-- A step that exits 0 is recorded done. One that exits otherwise, cannot
-- start or reaches its limit gives the message, and the steps after it do
-- not run. When the step ends, any process still in its group is killed;
-- when the calling thread gets an exception (a cancel, Ctrl-C), the group
-- is killed and the exception passes on, the step not recorded done.
runStepsWith :: Int -> FilePath -> [Step] -> (Text -> IO ()) -> IO (Either Text ())
runStepsWith scale project steps sink = do
  createDirectoryIfMissing True (project </> fisherDirName)
  doneSteps project >>= \case
    Left e -> pure (Left e)
    Right done -> go done (zip [1 :: Int ..] steps)
  where
    total = length steps
    go _ [] = pure (Right ())
    go done ((i, st) : rest)
      | stepName st `elem` done = do
          sink (label i st <> ": done before, skipped")
          go done rest
      | otherwise = do
          sink (label i st <> ": " <> T.unwords (stepRun st) <> " (output in .fisher/create.log)")
          r <- runOne scale project st
          case r of
            Right () -> markDone project (stepName st) >>= \case
              Right () -> go (done <> [stepName st]) rest
              Left e -> pure (Left e)
            Left e -> pure (Left e)
    label i st = "step " <> stepName st <> " (" <> T.pack (show i) <> "/" <> T.pack (show total) <> ")"

data Outcome = Exited Int | TimedOut

runOne :: Int -> FilePath -> Step -> IO (Either Text ())
runOne scale project st = case stepRun st of
  [] -> pure (Left ("step " <> name <> " has no command"))
  (cmd : args) -> do
    writeEvent project (StepStart name)
    appendLog ("== step " <> name <> ": " <> T.unwords (stepRun st) <> "\n")
    started <- mask $ \restore -> do
      p <- try (createPipe >>= \(rEnd, wEnd) -> do
                  (_, _, _, ph) <- createProcess (proc (T.unpack cmd) (map T.unpack args))
                    { cwd = Just project, std_in = NoStream, std_out = UseHandle wEnd, std_err = UseHandle wEnd
                    , create_group = True }
                    `onException` mapM_ (\h -> try (hClose h) :: IO (Either SomeException ())) [rEnd, wEnd]
                  pure (rEnd, ph))
      case p of
        Left (e :: IOException) -> pure (Left (T.pack (show e)))
        Right (rEnd, ph) -> do
          pid <- getPid ph
          let killGroup = mapM_ (\g -> void (try (signalProcessGroup sigKILL g) :: IO (Either SomeException ()))) pid
          tailRef <- newIORef (Seq.empty, BS.empty)
          readerDone <- newEmptyMVar
          _ <- forkIO (readOutput rEnd tailRef >> putMVar readerDone ())
          -- polled, not a blocking wait: without the threaded RTS a blocking
          -- waitForProcess cannot be interrupted by the time limit
          r <- restore (timeout (limit * 1000000) (pollExit ph))
                 `onException` (killGroup >> waitForProcess ph)
          outcome <- case r of
            Just c -> pure (Exited (case c of ExitSuccess -> 0; ExitFailure k -> k))
            Nothing -> killGroup >> waitForProcess ph >> pure TimedOut
          -- the leader is reaped but the group lives while any member does (and
          -- its id is not reused until then): kill what the step left behind
          killGroup
          -- the pipe closes once the group is gone; a process that left the
          -- group could keep it open, so the wait is bounded
          _ <- timeout 5000000 (readMVar readerDone)
          ls <- tailLines tailRef
          pure (Right (outcome, ls))
    case started of
      Left e -> do
        appendLog ("== step " <> name <> " could not start: " <> e <> "\n")
        writeEvent project (StepEnd name 127 [e])
        pure (Left ("step " <> name <> " could not start: " <> e))
      Right (Exited 0, ls) -> do
        appendLog ("== step " <> name <> ": exit 0\n")
        writeEvent project (StepEnd name 0 ls)
        pure (Right ())
      Right (Exited n, ls) -> do
        appendLog ("== step " <> name <> ": exit " <> tshow n <> "\n")
        writeEvent project (StepEnd name n ls)
        pure (Left ("step " <> name <> " failed (exit " <> tshow n <> "); see .fisher/create.log"))
      Right (TimedOut, ls) -> do
        let msg = "step " <> name <> " timed out after " <> tshow (stepTimeoutMinutes st) <> " min"
        appendLog ("== " <> msg <> "\n")
        writeEvent project (StepEnd name (-1) ls)
        pure (Left (msg <> "; see .fisher/create.log"))
  where
    name = stepName st
    limit = max 1 (stepTimeoutMinutes st * scale)
    appendLog t = BS.appendFile (logFile project) (TE.encodeUtf8 t)
    -- copy the merged output to the log as it comes, keeping the last lines
    readOutput :: Handle -> IORef (Seq.Seq Text, BS.ByteString) -> IO ()
    readOutput h ref = do
      _ <- try (withBinaryFile (logFile project) AppendMode $ \logH ->
             let loop = do
                   chunk <- BS.hGetSome h 32768
                   unless (BS.null chunk) $ do
                     BS.hPut logH chunk
                     hFlush logH
                     atomicModifyIORef' ref (\s -> (feed chunk s, ()))
                     loop
              in loop) :: IO (Either SomeException ())
      void (try (hClose h) :: IO (Either SomeException ()))

-- | Split a chunk into lines, keeping the last 'tailLimit' whole lines and
-- the partial one (at most 64 KiB of it).
feed :: BS.ByteString -> (Seq.Seq Text, BS.ByteString) -> (Seq.Seq Text, BS.ByteString)
feed chunk (ls, partial) =
  case BC.split '\n' (partial <> chunk) of
    [] -> (ls, BS.empty)
    pieces ->
      let whole = init pieces
          rest = last pieces
          ls' = trimTo tailLimit (foldl (\acc l -> acc Seq.|> decodeLine l) ls whole)
       in (ls', BS.drop (BS.length rest - 65536) rest)

tailLines :: IORef (Seq.Seq Text, BS.ByteString) -> IO [Text]
tailLines ref = do
  (ls, partial) <- readIORef ref
  pure (toList (if BS.null partial then ls else trimTo tailLimit (ls Seq.|> decodeLine partial)))

trimTo :: Int -> Seq.Seq a -> Seq.Seq a
trimTo n s = Seq.drop (Seq.length s - n) s

decodeLine :: BS.ByteString -> Text
decodeLine = T.dropWhileEnd (== '\r') . TE.decodeUtf8With TEE.lenientDecode

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | @$FISHER_HOME/fisher/templates-trusted.json@.
trustFile :: IO FilePath
trustFile = (\h -> h </> "fisher" </> "templates-trusted.json") <$> getFisherHome

-- | The trust file's entries; a missing or unreadable file has none.
readTrust :: FilePath -> IO (KM.KeyMap Value)
readTrust path = do
  r <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
  pure $ case r of
    Right bytes | Just (Object o) <- A.decodeStrict bytes -> o
    _ -> mempty

-- | A built-in, or a template whose current revision is the one recorded
-- for its name.
isTrusted :: FilePath -> Template -> IO Bool
isTrusted path t = case tplOrigin t of
  BuiltIn -> pure True
  _ -> templateRevision t >>= isTrustedRevision path (tplName t)

-- | Record the template's current revision as trusted, keeping the other
-- entries.
recordTrust :: FilePath -> Template -> IO ()
recordTrust path t = templateRevision t >>= recordTrustRevision path (tplName t)

-- | Whether the trust file records this revision for this name (a project's
-- manifest keeps both, so @fisher project build@ can ask without the
-- template).
isTrustedRevision :: FilePath -> Text -> Text -> IO Bool
isTrustedRevision path name rev = do
  m <- readTrust path
  pure (KM.lookup (K.fromText name) m == Just (String rev))

-- | Record a revision as trusted for a name, keeping the other entries.
recordTrustRevision :: FilePath -> Text -> Text -> IO ()
recordTrustRevision path name rev = do
  m <- readTrust path
  writeAtomic path (BL.toStrict (A.encode (Object (KM.insert (K.fromText name) (String rev) m))))

-- | Wait for a process by polling its exit code every 50 ms.
pollExit :: ProcessHandle -> IO ExitCode
pollExit ph = getProcessExitCode ph >>= maybe (threadDelay 50000 >> pollExit ph) pure
