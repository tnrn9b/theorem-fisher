-- | The session store upstream keeps in @$GAUSS_HOME/state.db@
-- (@gauss_state.py@'s @SessionDB@): the same SQLite schema, migrations,
-- FTS5 index and queries, so either program reads what the other wrote.
module Fisher.Store
  ( Store
  , storePath
  , openStore
  , closeStore
  , withStore
  , SessionRow
  , createSession
  , endSession
  , appendMessage
  , getSession
  , resolveSessionId
  , rootSessionId
  , sanitizeTitle
  , setSessionTitle
  , listSessionsRich
  , RichSession (..)
  , sessionCount
  , messageCount
  , exportSession
  , exportAll
  , deleteSession
  , pruneSessions
  , sessionColumns
  , messageColumns
    -- * What the agent loop writes
  , createSessionFull
  , updateSystemPrompt
  , updateTokenCounts
  , storedSystemPrompt
  , getSessionTitle
  , allTitles
  , appendMessageFull
  , reopenSession
  , messagesAsConversation
  , snapshotRows
  ) where

import Control.Exception (SomeException, bracket, try)
import Control.Monad (forM, forM_, unless, void, when)
import Data.Char (ord)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField ()
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import Fisher.Py.Json (PyVal (..))
import Fisher.Rich.Cells (isPySpace)

newtype Store = Store Connection

storePath :: FilePath -> FilePath
storePath home = home </> "state.db"

schemaVersion :: Int
schemaVersion = 4

schemaSql :: [Query]
schemaSql =
  [ "CREATE TABLE IF NOT EXISTS schema_version (version INTEGER NOT NULL)"
  , "CREATE TABLE IF NOT EXISTS sessions (id TEXT PRIMARY KEY, source TEXT NOT NULL, user_id TEXT, model TEXT,\
    \ model_config TEXT, system_prompt TEXT, parent_session_id TEXT, started_at REAL NOT NULL, ended_at REAL,\
    \ end_reason TEXT, message_count INTEGER DEFAULT 0, tool_call_count INTEGER DEFAULT 0,\
    \ input_tokens INTEGER DEFAULT 0, output_tokens INTEGER DEFAULT 0, title TEXT,\
    \ FOREIGN KEY (parent_session_id) REFERENCES sessions(id))"
  , "CREATE TABLE IF NOT EXISTS messages (id INTEGER PRIMARY KEY AUTOINCREMENT,\
    \ session_id TEXT NOT NULL REFERENCES sessions(id), role TEXT NOT NULL, content TEXT, tool_call_id TEXT,\
    \ tool_calls TEXT, tool_name TEXT, timestamp REAL NOT NULL, token_count INTEGER, finish_reason TEXT)"
  , "CREATE INDEX IF NOT EXISTS idx_sessions_source ON sessions(source)"
  , "CREATE INDEX IF NOT EXISTS idx_sessions_parent ON sessions(parent_session_id)"
  , "CREATE INDEX IF NOT EXISTS idx_sessions_started ON sessions(started_at DESC)"
  , "CREATE INDEX IF NOT EXISTS idx_messages_session ON messages(session_id, timestamp)" ]

ftsSql :: [Query]
ftsSql =
  [ "CREATE VIRTUAL TABLE IF NOT EXISTS messages_fts USING fts5(content, content=messages, content_rowid=id)"
  , "CREATE TRIGGER IF NOT EXISTS messages_fts_insert AFTER INSERT ON messages BEGIN\
    \ INSERT INTO messages_fts(rowid, content) VALUES (new.id, new.content); END"
  , "CREATE TRIGGER IF NOT EXISTS messages_fts_delete AFTER DELETE ON messages BEGIN\
    \ INSERT INTO messages_fts(messages_fts, rowid, content) VALUES('delete', old.id, old.content); END"
  , "CREATE TRIGGER IF NOT EXISTS messages_fts_update AFTER UPDATE ON messages BEGIN\
    \ INSERT INTO messages_fts(messages_fts, rowid, content) VALUES('delete', old.id, old.content);\
    \ INSERT INTO messages_fts(rowid, content) VALUES (new.id, new.content); END" ]

-- | @SessionDB.__init__@: open (creating the directory), WAL, foreign
-- keys, schema and migrations.
openStore :: FilePath -> IO Store
openStore path = do
  createDirectoryIfMissing True (takeDir path)
  conn <- open path
  execute_ conn "PRAGMA busy_timeout = 10000"
  _ <- query_ conn "PRAGMA journal_mode=WAL" :: IO [Only Text]
  execute_ conn "PRAGMA foreign_keys=ON"
  mapM_ (execute_ conn) schemaSql
  rows <- query_ conn "SELECT version FROM schema_version LIMIT 1" :: IO [Only Int]
  case rows of
    [] -> execute conn "INSERT INTO schema_version (version) VALUES (?)" (Only schemaVersion)
    (Only v : _) -> do
      when (v < 2) $ do
        quietly conn "ALTER TABLE messages ADD COLUMN finish_reason TEXT"
        execute_ conn "UPDATE schema_version SET version = 2"
      when (v < 3) $ do
        quietly conn "ALTER TABLE sessions ADD COLUMN title TEXT"
        execute_ conn "UPDATE schema_version SET version = 3"
      when (v < 4) $ do
        quietly conn "CREATE UNIQUE INDEX IF NOT EXISTS idx_sessions_title_unique ON sessions(title) WHERE title IS NOT NULL"
        execute_ conn "UPDATE schema_version SET version = 4"
  quietly conn "CREATE UNIQUE INDEX IF NOT EXISTS idx_sessions_title_unique ON sessions(title) WHERE title IS NOT NULL"
  hasFts <- try (query_ conn "SELECT * FROM messages_fts LIMIT 0" :: IO [[SQLData]]) :: IO (Either SomeException [[SQLData]])
  case hasFts of
    Left _ -> mapM_ (execute_ conn) ftsSql
    Right _ -> pure ()
  pure (Store conn)
  where
    takeDir p = reverse (drop 1 (dropWhile (/= '/') (reverse p)))
    quietly c q = void (try (execute_ c q) :: IO (Either SomeException ()))

closeStore :: Store -> IO ()
closeStore (Store c) = close c

withStore :: FilePath -> (Store -> IO a) -> IO a
withStore path = bracket (openStore path) closeStore

type SessionRow = [(Text, PyVal)]

sessionColumns, messageColumns :: [Text]
sessionColumns =
  [ "id", "source", "user_id", "model", "model_config", "system_prompt", "parent_session_id", "started_at"
  , "ended_at", "end_reason", "message_count", "tool_call_count", "input_tokens", "output_tokens", "title" ]
messageColumns =
  ["id", "session_id", "role", "content", "tool_call_id", "tool_calls", "tool_name", "timestamp", "token_count", "finish_reason"]

-- A message's tool_calls column holds JSON, which upstream decodes on
-- export; Fisher writes no tool calls yet and exports the text as stored.
pyOf :: SQLData -> PyVal
pyOf = \case
  SQLInteger n -> PInt (fromIntegral n)
  SQLFloat d -> PFloat d
  SQLText t -> PStr t
  SQLNull -> PNull
  SQLBlob _ -> PNull

selectCols :: [Text] -> Text
selectCols = T.intercalate ", "

createSession :: Store -> Double -> Text -> Text -> Maybe Text -> IO ()
createSession (Store c) now sid source model =
  execute c "INSERT INTO sessions (id, source, user_id, model, model_config, system_prompt, parent_session_id, started_at)\
            \ VALUES (?, ?, NULL, ?, NULL, NULL, NULL, ?)" (sid, source, model, now)

endSession :: Store -> Double -> Text -> Text -> IO ()
endSession (Store c) now sid reason = execute c "UPDATE sessions SET ended_at = ?, end_reason = ? WHERE id = ?" (now, reason, sid)

-- | @append_message@ for plain messages (no tool calls): the row, and the
-- session's message count.
appendMessage :: Store -> Double -> Text -> Text -> Maybe Text -> IO ()
appendMessage (Store c) now sid role content = do
  execute c "INSERT INTO messages (session_id, role, content, tool_call_id, tool_calls, tool_name, timestamp, token_count, finish_reason)\
            \ VALUES (?, ?, ?, NULL, NULL, NULL, ?, NULL, NULL)" (sid, role, content, now)
  execute c "UPDATE sessions SET message_count = message_count + 1 WHERE id = ?" (Only sid)

getSession :: Store -> Text -> IO (Maybe SessionRow)
getSession (Store c) sid = do
  rows <- query c (Query ("SELECT " <> selectCols sessionColumns <> " FROM sessions WHERE id = ?")) (Only sid) :: IO [[SQLData]]
  pure (zip sessionColumns . map pyOf <$> listToMaybe rows)

-- | Fisher only: the root of a session's compression chain (its
-- @parent_session_id@s followed up, at most 1000); a session the store
-- does not know is its own root. The UI's step log, ledger and inbox are
-- keyed on it.
rootSessionId :: Store -> Text -> IO Text
rootSessionId (Store c) = go (1000 :: Int)
  where
    go n s = do
      rows <- query c "SELECT parent_session_id FROM sessions WHERE id = ?" (Only s) :: IO [Only (Maybe Text)]
      case rows of
        (Only (Just p) : _) | n > 0, not (T.null p), p /= s -> go (n - 1) p
        _ -> pure s

-- | @resolve_session_id@: an exact id, else a unique prefix.
resolveSessionId :: Store -> Text -> IO (Maybe Text)
resolveSessionId st@(Store c) given = do
  exact <- getSession st given
  case exact of
    Just _ -> pure (Just given)
    Nothing -> do
      let escaped = T.replace "_" "\\_" (T.replace "%" "\\%" (T.replace "\\" "\\\\" given))
      rows <- query c "SELECT id FROM sessions WHERE id LIKE ? ESCAPE '\\' ORDER BY started_at DESC LIMIT 2" (Only (escaped <> "%")) :: IO [Only Text]
      pure $ case rows of
        [Only one] -> Just one
        _ -> Nothing

-- | @sanitize_title@: control and invisible characters out, whitespace
-- collapsed; empty is no title; over 100 characters is an error.
sanitizeTitle :: Maybe Text -> Either Text (Maybe Text)
sanitizeTitle Nothing = Right Nothing
sanitizeTitle (Just t)
  | T.null t = Right Nothing
  | otherwise =
      let asciiCtl ch = let o = ord ch in (o <= 0x08) || o == 0x0b || o == 0x0c || (o >= 0x0e && o <= 0x1f) || o == 0x7f
          invisible ch =
            let o = ord ch
             in (o >= 0x200b && o <= 0x200f) || (o >= 0x2028 && o <= 0x202e) || (o >= 0x2060 && o <= 0x2069)
                  || o == 0xfeff || o == 0xfffc || (o >= 0xfff9 && o <= 0xfffb)
          cleaned0 = T.filter (\ch -> not (asciiCtl ch) && not (invisible ch)) t
          cleaned = T.unwords (T.split isPySpace cleaned0 `without` "")
          n = T.length cleaned
       in if T.null cleaned
            then Right Nothing
            else if n > 100 then Left ("Title too long (" <> T.pack (show n) <> " chars, max 100)") else Right (Just cleaned)
  where
    without xs x = filter (/= x) xs

-- | @set_session_title@: Left on a bad or taken title, else whether the
-- session existed.
setSessionTitle :: Store -> Text -> Text -> IO (Either Text Bool)
setSessionTitle (Store c) sid raw = case sanitizeTitle (Just raw) of
  Left e -> pure (Left e)
  Right title -> do
    conflict <- case title of
      Just tt -> query c "SELECT id FROM sessions WHERE title = ? AND id != ?" (tt, sid) :: IO [Only Text]
      Nothing -> pure []
    case (title, conflict) of
      (Just tt, Only other : _) -> pure (Left ("Title '" <> tt <> "' is already in use by session " <> other))
      _ -> do
        execute c "UPDATE sessions SET title = ? WHERE id = ?" (title, sid)
        Right . (> 0) <$> changes c

data RichSession = RichSession
  { rsId :: Text
  , rsSource :: Text
  , rsTitle :: Maybe Text
  , rsPreview :: Text
  , rsLastActive :: Maybe Double
  }
  deriving (Show)

-- | @list_sessions_rich@: newest first, with a preview of the first user
-- message and the time of the last message.
listSessionsRich :: Store -> Maybe Text -> Int -> IO [RichSession]
listSessionsRich (Store c) source limit = do
  let q =
        "SELECT s.id, s.source, s.title,\
        \ COALESCE((SELECT SUBSTR(REPLACE(REPLACE(m.content, X'0A', ' '), X'0D', ' '), 1, 63) FROM messages m\
        \ WHERE m.session_id = s.id AND m.role = 'user' AND m.content IS NOT NULL ORDER BY m.timestamp, m.id LIMIT 1), '') AS _preview_raw,\
        \ COALESCE((SELECT MAX(m2.timestamp) FROM messages m2 WHERE m2.session_id = s.id), s.started_at) AS last_active\
        \ FROM sessions s "
  rows <- case source of
    Just src -> query c (q <> "WHERE s.source = ? ORDER BY s.started_at DESC LIMIT ? OFFSET 0") (src, limit)
    Nothing -> query c (q <> "ORDER BY s.started_at DESC LIMIT ? OFFSET 0") (Only limit)
  pure
    [ RichSession sid src title (preview raw) lastActive
    | (sid, src, title, raw, lastActive) <- rows :: [(Text, Text, Maybe Text, Text, Maybe Double)] ]
  where
    preview raw =
      let s = T.dropAround isPySpace raw
       in if T.null s then "" else T.take 60 s <> (if T.length s > 60 then "..." else "")

sessionCount :: Store -> Maybe Text -> IO Int
sessionCount (Store c) = \case
  Just src -> fromOnly . head <$> query c "SELECT COUNT(*) FROM sessions WHERE source = ?" (Only src)
  Nothing -> fromOnly . head <$> query_ c "SELECT COUNT(*) FROM sessions"

messageCount :: Store -> IO Int
messageCount (Store c) = fromOnly . head <$> query_ c "SELECT COUNT(*) FROM messages"

messagesOf :: Connection -> Text -> IO [PyVal]
messagesOf c sid = do
  rows <- query c (Query ("SELECT " <> selectCols messageColumns <> " FROM messages WHERE session_id = ? ORDER BY timestamp, id")) (Only sid) :: IO [[SQLData]]
  pure [PDict (zip messageColumns (map pyOf r)) | r <- rows]

-- | @export_session@: the session's row with its messages.
exportSession :: Store -> Text -> IO (Maybe PyVal)
exportSession st@(Store c) sid =
  getSession st sid >>= \case
    Nothing -> pure Nothing
    Just row -> do
      ms <- messagesOf c sid
      pure (Just (PDict (row <> [("messages", PList ms)])))

-- | @export_all@: every session (of a source), newest first, as
-- @search_sessions@ orders them.
exportAll :: Store -> Maybe Text -> IO [PyVal]
exportAll (Store c) source = do
  let q = "SELECT " <> selectCols sessionColumns <> " FROM sessions "
  rows <- case source of
    Just src -> query c (Query (q <> "WHERE source = ? ORDER BY started_at DESC LIMIT ? OFFSET ?")) (src, 100000 :: Int, 0 :: Int)
    Nothing -> query c (Query (q <> "ORDER BY started_at DESC LIMIT ? OFFSET ?")) (100000 :: Int, 0 :: Int) :: IO [[SQLData]]
  forM rows $ \r -> do
    let row = zip sessionColumns (map pyOf r)
        sid = case lookup "id" row of Just (PStr s) -> s; _ -> ""
    ms <- messagesOf c sid
    pure (PDict (row <> [("messages", PList ms)]))

deleteSession :: Store -> Text -> IO Bool
deleteSession (Store c) sid = do
  [Only n] <- query c "SELECT COUNT(*) FROM sessions WHERE id = ?" (Only sid) :: IO [Only Int]
  if n == 0
    then pure False
    else do
      withTransaction c $ do
        execute c "DELETE FROM messages WHERE session_id = ?" (Only sid)
        execute c "DELETE FROM sessions WHERE id = ?" (Only sid)
      pure True

-- | @prune_sessions@: ended sessions started more than @days@ ago.
pruneSessions :: Store -> Double -> Int -> Maybe Text -> IO Int
pruneSessions (Store c) now days source = do
  let cutoff = now - fromIntegral days * 86400
  ids <- case source of
    Just src -> query c "SELECT id FROM sessions WHERE started_at < ? AND ended_at IS NOT NULL AND source = ?" (cutoff, src)
    Nothing -> query c "SELECT id FROM sessions WHERE started_at < ? AND ended_at IS NOT NULL" (Only cutoff) :: IO [Only Text]
  unless (null ids) $
    withTransaction c $ forM_ ids $ \(Only sid) -> do
      execute c "DELETE FROM messages WHERE session_id = ?" (Only sid)
      execute c "DELETE FROM sessions WHERE id = ?" (Only sid)
  pure (length ids)

-- ---------------------------------------------------------------- agent loop

-- | @create_session(session_id, source, model, model_config, parent)@:
-- a plain INSERT (an existing id fails, as upstream).
createSessionFull :: Store -> Double -> Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> IO ()
createSessionFull (Store c) now sid source model modelConfig parent =
  execute c "INSERT INTO sessions (id, source, user_id, model, model_config, system_prompt, parent_session_id, started_at)\
            \ VALUES (?, ?, NULL, ?, ?, NULL, ?, ?)" (sid, source, model, modelConfig, parent, now)

updateSystemPrompt :: Store -> Text -> Text -> IO ()
updateSystemPrompt (Store c) sid p = execute c "UPDATE sessions SET system_prompt = ? WHERE id = ?" (p, sid)

-- | @update_token_counts@: add to the counters, fill the model if unset.
updateTokenCounts :: Store -> Text -> Integer -> Integer -> Maybe Text -> IO ()
updateTokenCounts (Store c) sid i o model =
  execute c "UPDATE sessions SET input_tokens = input_tokens + ?, output_tokens = output_tokens + ?, model = COALESCE(model, ?) WHERE id = ?"
    (i, o, model, sid)

storedSystemPrompt :: Store -> Text -> IO (Maybe Text)
storedSystemPrompt (Store c) sid = do
  rows <- query c "SELECT system_prompt FROM sessions WHERE id = ?" (Only sid) :: IO [Only (Maybe Text)]
  pure $ case rows of
    (Only p : _) -> p
    _ -> Nothing

getSessionTitle :: Store -> Text -> IO (Maybe Text)
getSessionTitle (Store c) sid = do
  rows <- query c "SELECT title FROM sessions WHERE id = ?" (Only sid) :: IO [Only (Maybe Text)]
  pure $ case rows of
    (Only t : _) -> t
    _ -> Nothing

allTitles :: Store -> IO [Text]
allTitles (Store c) = map fromOnly <$> (query_ c "SELECT title FROM sessions WHERE title IS NOT NULL" :: IO [Only Text])

-- | @append_message@: the row (tool calls already JSON), the session's
-- message count, and its tool-call count by the number of calls.
appendMessageFull :: Store -> Double -> Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Int -> Maybe Text -> Maybe Text -> IO ()
appendMessageFull (Store c) now sid role content toolName toolCallsJson nCalls toolCallId finishReason = do
  execute c "INSERT INTO messages (session_id, role, content, tool_call_id, tool_calls, tool_name, timestamp, token_count, finish_reason)\
            \ VALUES (?, ?, ?, ?, ?, ?, ?, NULL, ?)" (sid, role, content, toolCallId, toolCallsJson, toolName, now, finishReason)
  if nCalls > 0
    then execute c "UPDATE sessions SET message_count = message_count + 1, tool_call_count = tool_call_count + ? WHERE id = ?" (nCalls, sid)
    else execute c "UPDATE sessions SET message_count = message_count + 1 WHERE id = ?" (Only sid)

-- | Resuming re-opens a session: @ended_at@ and @end_reason@ cleared.
reopenSession :: Store -> Text -> IO ()
reopenSession (Store c) sid = execute c "UPDATE sessions SET ended_at = NULL, end_reason = NULL WHERE id = ?" (Only sid)

-- | @get_messages_as_conversation@: role and content, plus the tool call
-- id, tool name and (decoded) tool calls when set; the tool calls as the
-- stored JSON text, for the caller to decode.
messagesAsConversation :: Store -> Text -> IO [(Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text)]
messagesAsConversation (Store c) sid =
  query c "SELECT role, content, tool_call_id, tool_calls, tool_name FROM messages WHERE session_id = ? ORDER BY timestamp, id" (Only sid)

-- | Every session and message row, as the B3 corpus records them
-- (timestamps reduced to whether they are set).
snapshotRows :: Store -> IO ([[(Text, PyVal)]], [[(Text, PyVal)]])
snapshotRows (Store c) = do
  ss <- query_ c (Query ("SELECT " <> selectCols sessionColumns <> " FROM sessions ORDER BY started_at, id")) :: IO [[SQLData]]
  ms <- query_ c (Query ("SELECT " <> selectCols messageColumns <> " FROM messages ORDER BY id")) :: IO [[SQLData]]
  let present = \case
        SQLNull -> PBool False
        _ -> PBool True
      srow r = [(k, if k `elem` ["started_at", "ended_at"] then present v else pyOf v) | (k, v) <- zip sessionColumns r]
      mrow r = [(k, if k == "timestamp" then present v else pyOf v) | (k, v) <- zip messageColumns r, k /= "id"]
  pure (map srow ss, map mrow ms)
