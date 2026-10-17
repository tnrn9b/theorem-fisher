-- | MCP's streamable HTTP transport as the SDK speaks it: each request a
-- POST whose answer is JSON or an event stream, the session id from the
-- initialize answer echoed on every later request with the negotiated
-- protocol version, a GET event stream opened after
-- @notifications/initialized@ (two tries, a second apart), a 404 to a
-- request turned into a "Session terminated" error, any other failure
-- status fatal to the connection, and a DELETE when it closes. The HTTP
-- itself goes through an 'HttpClient', so the logic runs without a network.
module Fisher.Mcp.Http
  ( HttpClient (..)
  , HttpResponse (..)
  , HttpOpts (..)
  , connectHttp
  , taskGroupError
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, void, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Fisher.Mcp.Session (Incoming (..), Transport (..))
import Fisher.Py.Value (PyVal (..), dumpsUnicode, get, loads)

data HttpResponse = HttpResponse
  { hrStatus :: Int
  , hrHeaders :: [(Text, Text)]
  -- ^ names lowercased
  , hrRead :: IO BS.ByteString
  -- ^ the next chunk of the body; empty at the end
  , hrClose :: IO ()
  }

newtype HttpClient = HttpClient
  { hcRequest :: Text -> Text -> [(Text, Text)] -> Maybe BS.ByteString -> IO (Either Text HttpResponse)
  -- ^ method, url, headers (in order; a name may repeat), body
  }

data HttpOpts = HttpOpts
  { hoUrl :: Text
  , hoHeaders :: [(Text, Text)]
  }

-- | What upstream logs when the transport's task group dies.
taskGroupError :: Text
taskGroupError = "unhandled errors in a TaskGroup (1 sub-exception)"

data St = St
  { stSession :: IORef (Maybe Text)
  , stProtocol :: IORef (Maybe Text)
  , stClosed :: IORef Bool
  }

-- | Open the transport (nothing is sent until the first message).
connectHttp :: HttpClient -> HttpOpts -> IO Transport
connectHttp hc o = do
  q <- newTQueueIO
  st <- St <$> newIORef Nothing <*> newIORef Nothing <*> newIORef False
  let headersFor extra = do
        sid <- readIORef (stSession st)
        proto <- readIORef (stProtocol st)
        let mcp =
              [("accept", "application/json, text/event-stream"), ("content-type", "application/json")]
                <> [("mcp-session-id", s) | Just s <- [sid]]
                <> [("mcp-protocol-version", p) | Just p <- [proto]]
            lower = T.toLower . fst
            fromConfig = [(k, v) | (k, v) <- hoHeaders o, T.toLower k `notElem` map lower mcp]
        pure (fromConfig <> mcp <> extra)
      push = atomically . writeTQueue q
      fatal = push (InFatal taskGroupError)
      post msg = do
        hs <- headersFor []
        r <- hcRequest hc "POST" (hoUrl o) hs (Just (TE.encodeUtf8 (dumpsUnicode msg)))
        pure r
      isRequest m = get "method" m /= Nothing && get "id" m /= Nothing
      handleRequest m = void . forkIO $ do
        r <- post m
        case r of
          Left e -> push (InFatal e)
          Right resp -> do
            let status = hrStatus resp
            if status == 202
              then hrClose resp
              else
                if status == 404
                  then do
                    hrClose resp
                    push (InMessage (PDict [("jsonrpc", PStr "2.0"), ("id", maybe PNull id (get "id" m)), ("error", PDict [("code", PInt 32600), ("message", PStr "Session terminated")])]))
                  else
                    if status < 200 || status >= 300
                      then hrClose resp >> fatal
                      else do
                        when (get "method" m == Just (PStr "initialize")) $
                          forM_ (lookup "mcp-session-id" (hrHeaders resp)) (writeIORef (stSession st) . Just)
                        let ctype = maybe "" T.toLower (lookup "content-type" (hrHeaders resp))
                        if "application/json" `T.isPrefixOf` ctype
                          then do
                            body <- readAll resp
                            case loads body of
                              Right v -> do
                                noteProtocol m v
                                push (InMessage v)
                              Left _ -> push InUnparsable
                          else
                            if "text/event-stream" `T.isPrefixOf` ctype
                              then sseEvents resp $ \ev dat -> do
                                if ev == "message" && not (BS.null dat)
                                  then case loads dat of
                                    Right v -> do
                                      noteProtocol m v
                                      push (InMessage v)
                                      pure (get "result" v /= Nothing || get "error" v /= Nothing)
                                    Left _ -> push InUnparsable >> pure False
                                  else pure False
                              else hrClose resp >> push InUnparsable
      noteProtocol m v =
        when (get "method" m == Just (PStr "initialize")) $
          case get "result" v >>= get "protocolVersion" of
            Just (PStr p) -> writeIORef (stProtocol st) (Just p)
            _ -> pure ()
      getStream = void . forkIO $ do
        let attempt (n :: Int) = do
              closed <- readIORef (stClosed st)
              unless (closed || n >= 2) $ do
                hs <- headersFor [("accept", "text/event-stream"), ("cache-control", "no-store")]
                r <- hcRequest hc "GET" (hoUrl o) hs Nothing
                case r of
                  Right resp | hrStatus resp >= 200 && hrStatus resp < 300 -> do
                    sseEvents resp $ \ev dat -> do
                      when (ev == "message" && not (BS.null dat)) $ either (const (pure ())) (push . InMessage) (loads dat)
                      pure False
                    attempt 0
                  Right resp -> hrClose resp >> threadDelay 1000000 >> attempt (n + 1)
                  Left _ -> threadDelay 1000000 >> attempt (n + 1)
        attempt 0
      send m
        | isRequest m = handleRequest m
        | otherwise = do
            r <- post m
            case r of
              Left e -> ioError (userError (T.unpack e))
              Right resp -> do
                hrClose resp
                let status = hrStatus resp
                when (status /= 202 && (status < 200 || status >= 300) && status /= 404) fatal
                when (get "method" m == Just (PStr "notifications/initialized")) $ do
                  sid <- readIORef (stSession st)
                  when (sid /= Nothing) getStream
      close = do
        writeIORef (stClosed st) True
        sid <- readIORef (stSession st)
        when (sid /= Nothing) $ do
          hs <- headersFor []
          r <- try (hcRequest hc "DELETE" (hoUrl o) hs Nothing) :: IO (Either SomeException (Either Text HttpResponse))
          case r of
            Right (Right resp) -> hrClose resp
            _ -> pure ()
  pure (Transport send q close)

readAll :: HttpResponse -> IO BS.ByteString
readAll resp = go []
  where
    go acc = do
      c <- hrRead resp
      if BS.null c then hrClose resp >> pure (BS.concat (reverse acc)) else go (c : acc)

-- | Read server-sent events, calling @k event data@ for each; stop when @k@
-- says so or the stream ends.
sseEvents :: HttpResponse -> (Text -> BS.ByteString -> IO Bool) -> IO ()
sseEvents resp k = go "" "message" [] False
  where
    go buf ev dat started = case BC.elemIndex '\n' buf of
      Just i -> do
        let line0 = BS.take i buf
            line = if BC.isSuffixOf "\r" line0 then BS.init line0 else line0
            rest = BS.drop (i + 1) buf
        if BS.null line
          then
            if started
              then do
                stop <- k ev (BS.intercalate "\n" (reverse dat))
                if stop then hrClose resp else go rest "message" [] False
              else go rest "message" [] False
          else
            let (field, val0) = BC.break (== ':') line
                val = let v = BS.drop 1 val0 in if BC.isPrefixOf " " v then BS.drop 1 v else v
             in case field of
                  "event" -> go rest (TE.decodeUtf8 val) dat True
                  "data" -> go rest ev (val : dat) True
                  _ -> go rest ev dat True
      Nothing -> do
        c <- hrRead resp
        if BS.null c then hrClose resp else go (buf <> c) ev dat started
