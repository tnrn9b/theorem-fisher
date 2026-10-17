-- | Where a conversation is kept: the session database the agent writes
-- through (upstream's @SessionDB@ in @gauss_state.py@, as an interface:
-- the sqlite store in the CLI, an in-memory copy in the specs), and the
-- session log file (@sessions/session_<id>.json@).
module Fisher.Agent.Session
  ( SessionDB (..)
  , memorySessionDB
  , MemoryDB
  , memorySnapshot
  , nextTitleInLineage
  , cleanSessionContent
  , sessionLogEntry
  , isoformat
  ) where

import Control.Exception (throwIO)
import Data.IORef
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (LocalTime, defaultTimeLocale, formatTime)

import qualified Fisher.Py.Re as Re
import Fisher.Py.Value

-- | The operations the agent uses. Each may throw; the agent catches.
data SessionDB = SessionDB
  { dbCreateSession :: Text -> Text -> Maybe Text -> Maybe PyVal -> Maybe Text -> IO ()
  -- ^ id, source, model, model_config, parent id
  , dbEndSession :: Text -> Text -> IO ()
  , dbUpdateSystemPrompt :: Text -> Text -> IO ()
  , dbUpdateTokenCounts :: Text -> Integer -> Integer -> Maybe Text -> IO ()
  , dbStoredSystemPrompt :: Text -> IO (Maybe Text)
  -- ^ @get_session(id)["system_prompt"]@
  , dbGetSessionTitle :: Text -> IO (Maybe Text)
  , dbSetSessionTitle :: Text -> Text -> IO ()
  , dbTitles :: IO [Text]
  -- ^ every title, for @get_next_title_in_lineage@
  , dbAppendMessage :: Text -> PyVal -> IO ()
  -- ^ the session and the message dict (role, content, tool_name,
  -- tool_calls, tool_call_id, finish_reason are read from it)
  }

-- | @get_next_title_in_lineage(base)@ over the titles that exist.
nextTitleInLineage :: [Text] -> Text -> Text
nextTitleInLineage titles baseTitle =
  let base = case Re.fullMatch lineageRx baseTitle >>= Re.group 1 of
        Just b -> b
        Nothing -> baseTitle
      existing = [t | t <- titles, t == base || (base <> " #") `T.isPrefixOf` t]
      nums = [read (T.unpack d) :: Integer | t <- existing, Just d <- [Re.fullMatch numRx t >>= Re.group 1]]
   in if null existing then base else base <> " #" <> T.pack (show (maximum (1 : nums) + 1))
  where
    lineageRx = Re.compileOrDie [] "(.*?) #(\\d+)"
    numRx = Re.compileOrDie [] ".* #(\\d+)"

-- | The database the specs use: the rows sqlite would hold.
data MemoryDB = MemoryDB
  { mdSessions :: IORef [[(Text, PyVal)]]
  , mdMessages :: IORef [[(Text, PyVal)]]
  }

memorySessionDB :: IO (SessionDB, MemoryDB)
memorySessionDB = do
  ss <- newIORef []
  ms <- newIORef []
  let upd sid f = modifyIORef ss (map (\r -> if lookup "id" r == Just (PStr sid) then f r else r))
      setK k v r = [(k', if k' == k then v else v') | (k', v') <- r]
      num k r = case lookup k r of
        Just (PInt n) -> n
        _ -> 0
      db =
        SessionDB
          { dbCreateSession = \sid source model cfg parent -> do
              rows <- readIORef ss
              if any (\r -> lookup "id" r == Just (PStr sid)) rows
                then throwIO (userError "UNIQUE constraint failed: sessions.id")
                else
                  modifyIORef ss (<> [[ ("id", PStr sid), ("source", PStr source), ("user_id", PNull), ("model", maybe PNull PStr model)
                                       , ("model_config", maybe PNull (\c -> if truthy c then PStr (dumpsAscii c) else PNull) cfg)
                                       , ("system_prompt", PNull), ("parent_session_id", maybe PNull PStr parent)
                                       , ("started_at", PBool True), ("ended_at", PBool False), ("end_reason", PNull)
                                       , ("message_count", PInt 0), ("tool_call_count", PInt 0), ("input_tokens", PInt 0)
                                       , ("output_tokens", PInt 0), ("title", PNull) ]])
          , dbEndSession = \sid reason -> upd sid (setK "end_reason" (PStr reason) . setK "ended_at" (PBool True))
          , dbUpdateSystemPrompt = \sid p -> upd sid (setK "system_prompt" (PStr p))
          , dbUpdateTokenCounts = \sid i o model -> upd sid $ \r ->
              let r1 = setK "input_tokens" (PInt (num "input_tokens" r + i)) (setK "output_tokens" (PInt (num "output_tokens" r + o)) r)
               in case lookup "model" r1 of
                    Just PNull -> setK "model" (maybe PNull PStr model) r1
                    _ -> r1
          , dbStoredSystemPrompt = \sid -> do
              rows <- readIORef ss
              pure $ case [r | r <- rows, lookup "id" r == Just (PStr sid)] of
                (r : _) | Just (PStr p) <- lookup "system_prompt" r -> Just p
                _ -> Nothing
          , dbGetSessionTitle = \sid -> do
              rows <- readIORef ss
              pure $ case [r | r <- rows, lookup "id" r == Just (PStr sid)] of
                (r : _) | Just (PStr t) <- lookup "title" r -> Just t
                _ -> Nothing
          , dbSetSessionTitle = \sid t -> upd sid (setK "title" (PStr t))
          , dbTitles = do
              rows <- readIORef ss
              pure [t | r <- rows, Just (PStr t) <- [lookup "title" r]]
          , dbAppendMessage = \sid m -> do
              let g k = maybe PNull id (get k m)
                  calls = case get "tool_calls" m of
                    Just l@(PList xs) | not (null xs) -> Just l
                    _ -> Nothing
              modifyIORef ms (<> [[ ("session_id", PStr sid), ("role", case g "role" of PNull -> PStr "unknown"; v -> v)
                                   , ("content", g "content"), ("tool_call_id", g "tool_call_id")
                                   , ("tool_calls", maybe PNull (PStr . dumpsAscii) calls), ("tool_name", g "tool_name")
                                   , ("timestamp", PBool True), ("token_count", PNull), ("finish_reason", g "finish_reason") ]])
              let k = case calls of
                    Just (PList xs) -> fromIntegral (length xs)
                    _ -> 0
              upd sid (\r -> setK "tool_call_count" (PInt (num "tool_call_count" r + k)) (setK "message_count" (PInt (num "message_count" r + 1)) r))
          }
  pure (db, MemoryDB ss ms)

-- | The rows as the corpus records them: sessions in creation order,
-- messages in insertion order.
memorySnapshot :: MemoryDB -> IO PyVal
memorySnapshot md = do
  ss <- readIORef (mdSessions md)
  ms <- readIORef (mdMessages md)
  pure (PDict [("sessions", PList (map PDict (sortOn (const ()) ss))), ("messages", PList (map PDict ms))])

-- | @_clean_session_content@: scratchpad tags as think tags, and the
-- newlines around think blocks tightened.
cleanSessionContent :: Text -> Text
cleanSessionContent c0 =
  let c1 =
        if "<REASONING_SCRATCHPAD>" `T.isInfixOf` c0
          then T.replace "</REASONING_SCRATCHPAD>" "</think>" (T.replace "<REASONING_SCRATCHPAD>" "<think>" c0)
          else c0
      c2 = Re.sub (Re.compileOrDie [] "\\n+(<think>)") (\_ -> "\n<think>") c1
      c3 = Re.sub (Re.compileOrDie [] "(</think>)\\n+") (\_ -> "</think>\n") c2
   in T.strip c3

-- | @datetime.isoformat()@ for a naive time: microseconds only when not 0.
isoformat :: LocalTime -> Text
isoformat t =
  let base = T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" t)
      micros = T.pack (formatTime defaultTimeLocale "%6q" t)
   in if T.all (== '0') micros then base else base <> "." <> micros

-- | The session log file's contents (@_save_session_log@).
sessionLogEntry :: Text -> Text -> Text -> Text -> LocalTime -> LocalTime -> Text -> [PyVal] -> [PyVal] -> PyVal
sessionLogEntry sid model baseUrl platform start now systemPrompt tools msgs =
  let cleaned =
        [ case (get "role" m, get "content" m) of
            (Just (PStr "assistant"), Just (PStr c)) | not (T.null c) -> set "content" (PStr (cleanSessionContent c)) m
            _ -> m
        | m <- msgs ]
   in PDict
        [ ("session_id", PStr sid), ("model", PStr model), ("base_url", PStr baseUrl), ("platform", PStr platform)
        , ("session_start", PStr (isoformat start)), ("last_updated", PStr (isoformat now))
        , ("system_prompt", PStr systemPrompt), ("tools", PList tools)
        , ("message_count", PInt (fromIntegral (length msgs))), ("messages", PList cleaned) ]
