-- | Interactive Claude Code and Codex runs (started from the fisher shell
-- or @fisher launch@), imported into the run log from those tools' own
-- session files when the run ends.
--
-- * Claude Code writes @<home>/.claude/projects/<encoded cwd>/<session>.jsonl@,
--   the cwd encoded with every @/@ replaced by @-@ (newer releases also
--   replace every other character that is not a letter or digit; both
--   spellings are looked up). An @assistant@ line's @text@ blocks are
--   @message {role, text}@ and its @tool_use@ blocks @tool_start {name,
--   args, call_id}@; a @user@ line's @tool_result@ blocks are @tool_end
--   {name, call_id, ok, result}@, as in stream-json.
-- * Codex writes @<CODEX_HOME>/sessions/YYYY/MM/DD/rollout-*.jsonl@. A
--   @response_item@ whose payload is a @function_call@ is a @tool_start@,
--   its @function_call_output@ the @tool_end@, and an assistant @message@ a
--   @message@.
--
-- Every event's @t@ is seconds from the file's first timestamp (a line
-- without one keeps the previous line's; never below 0). A line that is not a JSON object,
-- or has an unexpected shape, is skipped; nothing here throws. The files
-- hold tool inputs and outputs only, and every event is written with
-- 'Fisher.Runlog.event', which redacts and caps it.
module Fisher.Swarm.Import
  ( claudeSessionFile
  , claudeProjectDirs
  , claudeSessionEvents
  , codexSessionFile
  , codexSessionEvents
  , sessionHomeOf
  , importTask
  , importFinished
  , importRunId
  , sessionFileLimit
  ) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_, unless)
import Data.Aeson (Object, Value (..), decodeStrict, object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (isAlphaNum)
import Data.Foldable (toList)
import Data.List (isPrefixOf, isSuffixOf, nub, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (getPOSIXTime, utcTimeToPOSIXSeconds)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import System.Directory (canonicalizePath, doesDirectoryExist, getFileSize, getModificationTime, listDirectory)
import System.FilePath (dropTrailingPathSeparator, normalise, (</>))
import System.IO (IOMode (..), hIsEOF, stderr, withFile)

import Fisher.Runlog
import Fisher.Runlog.Stream (StreamEvent (..))
import Fisher.Swarm.Types

-- * Finding the files

-- | The Claude Code session file of the cwd under the backend home: of
-- those modified at or after the start (seconds since the epoch), the one
-- whose first timestamp is nearest the start, then the newest.
claudeSessionFile :: FilePath -> FilePath -> Double -> IO (Maybe FilePath)
claudeSessionFile home cwd start = do
  cwds <- spellings cwd
  let dirs = nub (concatMap (claudeProjectDirs home) cwds)
  files <- concat <$> mapM (\d -> map (d </>) . filter (".jsonl" `isSuffixOf`) <$> listing d) dirs
  pickFile start (const True) files

-- | Where Claude Code keeps a cwd's sessions under a home: the cwd with
-- every @/@ replaced by @-@, then with every character that is not a
-- letter or digit replaced (the spelling of newer releases).
claudeProjectDirs :: FilePath -> FilePath -> [FilePath]
claudeProjectDirs home cwd =
  [home </> ".claude" </> "projects" </> enc | enc <- nub [map slash cwd, map other cwd]]
  where
    slash c = if c == '/' then '-' else c
    other c = if isAlphaNum c then c else '-'

-- | The Codex rollout of the cwd under @<CODEX_HOME>/sessions@: of those
-- modified at or after the start whose @session_meta@ names the cwd, the
-- one whose first timestamp is nearest the start, then the newest.
codexSessionFile :: FilePath -> FilePath -> Double -> IO (Maybe FilePath)
codexSessionFile codexHome cwd start = do
  files <- walk (4 :: Int) (codexHome </> "sessions")
  mine <- map norm <$> spellings cwd
  metaCwds <- forM files $ \p -> do
    h <- headObjects p
    case listToMaybe [c | o <- h, str "type" o == Just "session_meta", Just pl <- [obj "payload" o], Just c <- [str "cwd" pl]] of
      Nothing -> pure (p, [])
      Just c -> (,) p . map norm <$> spellings (T.unpack c)
  let ours = [p | (p, cs) <- metaCwds, any (`elem` mine) cs]
  pickFile start (`elem` ours) files
  where
    walk depth d = do
      names <- listing d
      fmap concat . forM names $ \n -> do
        let p = d </> n
        isDir <- either (const False) id <$> (try (doesDirectoryExist p) :: IO (Either SomeException Bool))
        if isDir
          then if depth > 0 then walk (depth - 1) p else pure []
          else pure [p | "rollout-" `isPrefixOf` n, ".jsonl" `isSuffixOf` n]

-- | The session home of a backend: @CODEX_HOME@ from the child's
-- environment for @codex@ (else @<backend home>/.codex@), the backend home
-- itself (Claude Code's HOME) otherwise.
sessionHomeOf :: Text -> FilePath -> Map.Map String String -> FilePath
sessionHomeOf backend home env
  | backend == "codex" = case Map.lookup "CODEX_HOME" env of
      Just c | not (all (== ' ') c) -> c
      _ -> home </> ".codex"
  | otherwise = home

-- | A path as given and canonicalized (macOS's @/tmp@ is @/private/tmp@).
spellings :: FilePath -> IO [FilePath]
spellings p = do
  c <- either (const p) id <$> (try (canonicalizePath p) :: IO (Either SomeException FilePath))
  pure (nub [p, c])

norm :: FilePath -> FilePath
norm = dropTrailingPathSeparator . normalise

listing :: FilePath -> IO [FilePath]
listing d = either (const []) id <$> (try (listDirectory d) :: IO (Either SomeException [FilePath]))

-- | The first few lines of a file that are JSON objects; [] on any error.
headObjects :: FilePath -> IO [Object]
headObjects p = either (const []) id <$> (try (withFile p ReadMode (go (8 :: Int))) :: IO (Either SomeException [Object]))
  where
    go 0 _ = pure []
    go n h =
      hIsEOF h >>= \case
        True -> pure []
        False -> do
          l <- BS.hGetLine h
          rest <- go (n - 1) h
          pure ([o | Just (Object o) <- [decodeStrict l]] ++ rest)

pickFile :: Double -> (FilePath -> Bool) -> [FilePath] -> IO (Maybe FilePath)
pickFile start keep files = do
  ranked <- fmap concat . forM (filter keep files) $ \p ->
    (try (getModificationTime p) :: IO (Either SomeException UTCTime)) >>= \case
      Right m | posix m >= start -> do
        h <- headObjects p
        let first = listToMaybe (mapMaybe stamp h)
            dist = abs . subtract start <$> first
        pure [((maybe True (const False) dist, fromMaybe 0 dist, Down (posix m)), p)]
      _ -> pure []
  pure (snd <$> listToMaybe (sortOn fst ranked))

posix :: UTCTime -> Double
posix = realToFrac . utcTimeToPOSIXSeconds

stamp :: Object -> Maybe Double
stamp o = str "timestamp" o >>= iso8601ParseM . T.unpack >>= Just . posix

-- * Reading them

-- | Lines that are JSON objects, each with its @t@: seconds from the
-- file's first timestamp, a line without one keeping the previous @t@.
timedObjects :: BS.ByteString -> [(Double, Object)]
timedObjects bs =
  let objs = [o | l <- BC.lines bs, Just (Object o) <- [decodeStrict l]]
      t0 = listToMaybe (mapMaybe stamp objs)
      go _ [] = []
      go prev (o : os) =
        let t = max 0 (fromMaybe prev ((-) <$> stamp o <*> t0))
         in (t, o) : go t os
   in go 0 objs

at' :: Double -> StreamEvent -> StreamEvent
at' t e = e {stT = Just t}

-- | The events of a Claude Code session file.
claudeSessionEvents :: BS.ByteString -> [StreamEvent]
claudeSessionEvents = go Map.empty . timedObjects
  where
    go _ [] = []
    go names ((t, o) : os) = case str "type" o of
      Just "assistant" ->
        let bs = blocks (obj "message" o >>= KM.lookup "content")
            starts = [(i, n) | b <- bs, str "type" b == Just "tool_use", Just i <- [str "id" b], Just n <- [str "name" b]]
         in map (at' t) (mapMaybe assistantBlock bs) ++ go (Map.union (Map.fromList starts) names) os
      Just "user" ->
        [at' t (toolEnd names b) | b <- blocks (obj "message" o >>= KM.lookup "content"), str "type" b == Just "tool_result"]
          ++ go names os
      _ -> go names os
    assistantBlock b = case str "type" b of
      Just "text" | Just x <- str "text" b, not (T.null (T.strip x)) -> Just (message x)
      Just "tool_use" -> Just (toolStart (txt (str "name" b)) (fromMaybe Null (KM.lookup "input" b)) (txt (str "id" b)))
      _ -> Nothing
    toolEnd names b =
      let cid = str "tool_use_id" b
       in ev "tool_end"
            [ ("name", txt (cid >>= (`Map.lookup` names)))
            , ("call_id", txt cid)
            , ("ok", Bool (KM.lookup "is_error" b /= Just (Bool True)))
            , ("result", String (contentText (KM.lookup "content" b)))
            ]

-- | The events of a Codex rollout file.
codexSessionEvents :: BS.ByteString -> [StreamEvent]
codexSessionEvents = go Map.empty . timedObjects
  where
    go _ [] = []
    go names ((t, o) : os) = case (str "type" o, obj "payload" o) of
      (Just "response_item", Just p) -> case str "type" p of
        Just "message"
          | str "role" p == Just "assistant"
          , let x = contentText (KM.lookup "content" p)
          , not (T.null (T.strip x)) ->
              at' t (message x) : go names os
        Just ty
          | ty `elem` ["function_call", "custom_tool_call"]
          , Just cid <- str "call_id" p ->
              let name = fromMaybe "" (str "name" p)
                  args = arguments (maybe (KM.lookup "input" p) Just (KM.lookup "arguments" p))
               in at' t (toolStart (String name) args (String cid)) : go (Map.insert cid name names) os
          | ty `elem` ["function_call_output", "custom_tool_call_output"] ->
              let cid = str "call_id" p
                  (ok, result) = output (KM.lookup "output" p)
               in at' t (ev "tool_end" [("name", txt (cid >>= (`Map.lookup` names))), ("call_id", txt cid), ("ok", Bool ok), ("result", String result)])
                    : go names os
        _ -> go names os
      _ -> go names os
    -- arguments arrive as a JSON string
    arguments = \case
      Just (String s) -> fromMaybe (String s) (decodeStrict (TE.encodeUtf8 s))
      Just v -> v
      Nothing -> Null
    -- an output is a string, often itself JSON {output, metadata {exit_code}}
    output = \case
      Just (String s) -> case decodeStrict (TE.encodeUtf8 s) of
        Just (Object b) | Just x <- str "output" b -> (exitOk b, x)
        _ -> (True, s)
      Just (Object b) -> (exitOk b, fromMaybe "" (str "output" b <|> str "content" b))
      _ -> (True, "")
    exitOk b = case obj "metadata" b >>= KM.lookup "exit_code" of
      Just (Number n) -> n == 0
      _ -> KM.lookup "success" b /= Just (Bool False)

-- * Writing the run

-- | Log a finished task as a @launch@ run @swarm-<session id>@: the
-- backend's session file's events when one is found (capture
-- @imported@), else the run alone (@shell-only@; a file over
-- 'sessionFileLimit' is not read), then an @outcome@
-- event and the run's end. The home is the session home
-- ('sessionHomeOf'): @codex@ reads the rollouts of the cwd under it as
-- CODEX_HOME; any other backend reads Claude Code's sessions of the cwd
-- under it as HOME. Returns the capture.
importTask :: Runlog -> TaskView -> Text -> FilePath -> FilePath -> IO Capture
importTask rl v backend home cwd = do
  now <- realToFrac <$> getPOSIXTime
  let info = tvInfo v
      rid = importRunId v
      end = fromMaybe now (tvEnd v)
  file <- case tvStart v of
    Nothing -> pure Nothing
    Just s
      | backend == "codex" -> codexSessionFile home cwd s
      | otherwise -> claudeSessionFile home cwd s
  size <- maybe (pure Nothing) (\p -> either (const Nothing) Just <$> (try (getFileSize p) :: IO (Either SomeException Integer))) file
  let tooBig = maybe False (> sessionFileLimit) size
  evs <- case file of
    Just p | not tooBig ->
      (try (BS.readFile p) :: IO (Either SomeException BS.ByteString)) >>= \case
        Left _ -> pure []
        Right bs -> pure (if backend == "codex" then codexSessionEvents bs else claudeSessionEvents bs)
    _ -> pure []
  let capture = if null evs then ShellOnly else Imported
  startRun rl
    NewRun
      { nrId = rid, nrKind = LaunchRun, nrParent = Nothing
      , nrProject = if null (tiProjectRoot info) then Nothing else Just (tiProjectRoot info)
      , nrProver = Nothing, nrBackend = Just backend, nrModel = Nothing, nrTier = Nothing
      , nrWorkflow = if T.null (tiWorkflowKind info) then Nothing else Just (tiWorkflowKind info)
      , nrArgv = [], nrStartedAt = fromMaybe end (tvStart v), nrRawDir = Nothing, nrCapture = capture }
  case size of
    Just n | tooBig ->
      event rl rid 0 "import_skipped"
        (object ["reason" .= ("the session file is " <> T.pack (show n) <> " bytes, over the 64 MB limit" :: Text)])
    _ -> pure ()
  forM_ evs $ \e -> event rl rid (fromMaybe 0 (stT e)) (stKind e) (stData e)
  let outcome = outcomeOf (tvStatus v)
  event rl rid (max 0 (end - fromMaybe end (tvStart v))) "outcome" (object ["outcome" .= outcome, "reason" .= Null])
  endRun rl rid
    RunEnd
      { reEndedAt = end, reExit = Nothing, reOutcome = outcome
      , reCostUsd = Nothing, reInputTokens = Nothing, reOutputTokens = Nothing, reTurns = Nothing }
  pure capture
  where
    outcomeOf = \case
      Complete -> "completed"
      Failed -> "failed"
      Cancelled -> "cancelled"
      Queued -> "cancelled"
      Running -> "cancelled"

-- | The run id a finished task is imported as: @swarm-<session id>@.
importRunId :: TaskView -> Text
importRunId v = "swarm-" <> tiSessionId (tvInfo v)

-- | The largest session file that is read (64 MB). A larger one is not
-- read at all: the run is logged @shell-only@ with an @import_skipped
-- {reason}@ event, so a huge session never stalls the end of a launch.
sessionFileLimit :: Integer
sessionFileLimit = 64 * 1024 * 1024

-- | 'importTask' into the run log at the path, for a finished task of the
-- backend with that session home and cwd. The native backend (@fisher@) logs its
-- own runs and is skipped. Never fails the caller: any error is one
-- @fisher: run log: ...@ line on stderr.
importFinished :: FilePath -> Text -> FilePath -> FilePath -> TaskView -> IO ()
importFinished db backend home cwd v = unless (backend == "fisher") $
  (try (withRunlog db (\rl -> importTask rl v backend home cwd)) :: IO (Either SomeException Capture)) >>= \case
    Left e -> TIO.hPutStrLn stderr ("fisher: run log: " <> T.pack (show e))
    Right _ -> pure ()

-- * Shapes

ev :: Text -> [(K.Key, Value)] -> StreamEvent
ev kind kvs = StreamEvent Nothing kind (object [k .= x | (k, x) <- kvs])

message :: Text -> StreamEvent
message x = ev "message" [("role", String "assistant"), ("text", String x)]

toolStart :: Value -> Value -> Value -> StreamEvent
toolStart name args cid = ev "tool_start" [("name", name), ("args", args), ("call_id", cid)]

-- | A message's content blocks; a plain string is one text block.
blocks :: Maybe Value -> [Object]
blocks = \case
  Just (Array xs) -> [b | Object b <- toList xs]
  Just (String x) -> [KM.fromList [("type", String "text"), ("text", String x)]]
  _ -> []

-- | Content's text: a string, or its text blocks (@text@, @output_text@)
-- one per line.
contentText :: Maybe Value -> Text
contentText = \case
  Just (String x) -> x
  Just (Array xs) ->
    T.intercalate "\n" [x | Object b <- toList xs, str "type" b `elem` map Just ["text", "output_text"], Just x <- [str "text" b]]
  _ -> ""

str :: K.Key -> Object -> Maybe Text
str k o = case KM.lookup k o of
  Just (String x) -> Just x
  _ -> Nothing

obj :: K.Key -> Object -> Maybe Object
obj k o = case KM.lookup k o of
  Just (Object x) -> Just x
  _ -> Nothing

txt :: Maybe Text -> Value
txt = maybe Null String
