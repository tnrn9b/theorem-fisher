-- | The session database the CLI gives the agent: @state.db@ through
-- 'Fisher.Store', opened once per agent.
module Fisher.Agent.StoreDB
  ( storeSessionDB
  , storeSnapshot
  , restoreConversation
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Fisher.Agent.Session
import Fisher.Py.Value
import Fisher.Store

storeSessionDB :: Store -> IO Double -> SessionDB
storeSessionDB st clock =
  SessionDB
    { dbCreateSession = \sid source model cfg parent -> do
        t <- clock
        createSessionFull st t sid source model (cfg >>= \c -> if truthy c then Just (dumpsAscii c) else Nothing) parent
    , dbEndSession = \sid reason -> clock >>= \t -> endSession st t sid reason
    , dbUpdateSystemPrompt = updateSystemPrompt st
    , dbUpdateTokenCounts = updateTokenCounts st
    , dbStoredSystemPrompt = storedSystemPrompt st
    , dbGetSessionTitle = getSessionTitle st
    , dbSetSessionTitle = \sid t -> do
        r <- setSessionTitle st sid t
        either (ioError . userError . show) (const (pure ())) r
    , dbTitles = allTitles st
    , dbAppendMessage = \sid m -> do
        t <- clock
        let g k = case get k m of
              Just (PStr s) -> Just s
              Just PNull -> Nothing
              Just v -> Just (pyStrOf v)
              Nothing -> Nothing
            role = case get "role" m of
              Just (PStr r) -> r
              _ -> "unknown"
            (callsJson, n) = case get "tool_calls" m of
              Just l@(PList xs) | not (null xs) -> (Just (dumpsAscii l), length xs)
              _ -> (Nothing, 0)
        appendMessageFull st t sid role (g "content") (g "tool_name") callsJson n (g "tool_call_id") (g "finish_reason")
    }

-- | The rows, in the corpus's form.
storeSnapshot :: Store -> IO PyVal
storeSnapshot st = do
  (ss, ms) <- snapshotRows st
  pure (PDict [("sessions", PList (map PDict ss)), ("messages", PList (map PDict ms))])

-- | @get_messages_as_conversation(session_id)@
restoreConversation :: Store -> Text -> IO [PyVal]
restoreConversation st sid = do
  rows <- messagesAsConversation st sid
  pure
    [ PDict $
        [("role", PStr role), ("content", maybe PNull PStr content)]
          <> [("tool_call_id", PStr i) | Just i <- [callId], not (T.null i)]
          <> [("tool_name", PStr n) | Just n <- [toolName], not (T.null n)]
          <> [("tool_calls", v) | Just j <- [calls], not (T.null j), Right v <- [loads (TE.encodeUtf8 j)]]
    | (role, content, callId, calls, toolName) <- rows ]
