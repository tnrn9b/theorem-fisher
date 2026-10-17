-- | A JSON-RPC 2.0 client session over any MCP transport, behaving as the
-- MCP SDK's @ClientSession@ does where upstream can see it: request ids
-- count from 0, responses are matched by id (a string id that spells a
-- number matches the number), requests from the server are answered
-- inline (nothing else on the session is handled meanwhile), and once the
-- transport closes the in-flight requests fail with "Connection closed"
-- and later ones with an empty @ClosedResourceError@.
module Fisher.Mcp.Session
  ( Incoming (..)
  , Transport (..)
  , Session
  , McpFailure (..)
  , failureText
  , ServerRequestHandler
  , newSession
  , request
  , notify
  , sessionClosed
  , sessionFatal
  , sessionClosedSTM
  , closeSession
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (Exception, SomeException, throwIO, try)
import Control.Monad (forM_, void, when)
import Data.Char (isDigit)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Py.Value (PyVal (..), get)

-- | What a transport delivers.
data Incoming
  = InMessage PyVal
  | InUnparsable
  | InClosed
  -- ^ the stream ended (stdio: the server exited)
  | InFatal Text
  -- ^ the transport died with an error (HTTP: a non-2xx answer); the
  -- owner reconnects

data Transport = Transport
  { tSend :: PyVal -> IO ()
  , tIncoming :: TQueue Incoming
  , tClose :: IO ()
  }

-- | Failures as upstream's exception class name and message.
data McpFailure = McpFailure Text Text
  deriving (Show)

instance Exception McpFailure

failureText :: McpFailure -> Text
failureText (McpFailure cls msg) = cls <> ": " <> msg

-- | Answer one request from the server: a result, or (code, message, data).
type ServerRequestHandler = Text -> PyVal -> IO (Either (Integer, Text, Maybe PyVal) PyVal)

data Session = Session
  { sTransport :: Transport
  , sNext :: TVar Integer
  , sPending :: TVar (M.Map Integer (TMVar (Either McpFailure PyVal)))
  , sClosed :: TVar Bool
  , sFatal :: TMVar Text
  }

newSession :: Transport -> ServerRequestHandler -> IO Session
newSession tr handler = do
  s <- Session tr <$> newTVarIO 0 <*> newTVarIO M.empty <*> newTVarIO False <*> newEmptyTMVarIO
  _ <- forkIO (readLoop s handler)
  pure s

readLoop :: Session -> ServerRequestHandler -> IO ()
readLoop s handler = loop
  where
    loop = do
      inc <- atomically (readTQueue (tIncoming (sTransport s)))
      case inc of
        InUnparsable -> loop
        InClosed -> do
          pend <- atomically $ do
            writeTVar (sClosed s) True
            p <- readTVar (sPending s)
            writeTVar (sPending s) M.empty
            pure p
          forM_ (M.elems pend) $ \v -> atomically (void (tryPutTMVar v (Left (McpFailure "McpError" "Connection closed"))))
        InFatal e -> atomically $ do
          writeTVar (sClosed s) True
          void (tryPutTMVar (sFatal s) e)
        InMessage m -> handle m >> loop
    handle m = case (get "method" m, get "id" m) of
      (Just (PStr method), Just rid) -> do
        r <- try (handler method (maybe PNull id (get "params" m)))
        let reply = case r of
              Right (Right res) -> PDict [("jsonrpc", PStr "2.0"), ("id", rid), ("result", res)]
              Right (Left (code, msg, dat)) -> PDict [("jsonrpc", PStr "2.0"), ("id", rid), ("error", PDict ([("code", PInt code), ("message", PStr msg)] <> maybe [] (\d -> [("data", d)]) dat))]
              Left (e :: SomeException) -> PDict [("jsonrpc", PStr "2.0"), ("id", rid), ("error", PDict [("code", PInt 0), ("message", PStr (T.pack (show e)))])]
        void (try (tSend (sTransport s) reply) :: IO (Either SomeException ()))
      (Just _, Nothing) -> pure () -- a notification
      (Nothing, Just rid) -> case normId rid of
        Nothing -> pure ()
        Just n -> do
          slot <- atomically $ do
            p <- readTVar (sPending s)
            writeTVar (sPending s) (M.delete n p)
            pure (M.lookup n p)
          forM_ slot $ \v -> atomically . void . tryPutTMVar v $ case (get "error" m, get "result" m) of
            (Just e, _) -> Left (McpFailure "McpError" (case get "message" e of Just (PStr t) -> t; _ -> ""))
            (_, Just res) -> Right res
            _ -> Right PNull
      _ -> pure ()
    normId = \case
      PInt n -> Just n
      PStr t | not (T.null t), T.all isDigit t -> Just (read (T.unpack t))
      _ -> Nothing

-- | Send a request and wait for its result (callers bound the wait).
request :: Session -> Text -> Maybe PyVal -> IO PyVal
request s method params = do
  closed <- readTVarIO (sClosed s)
  when closed (throwIO (McpFailure "ClosedResourceError" ""))
  (rid, slot) <- atomically $ do
    n <- readTVar (sNext s)
    writeTVar (sNext s) (n + 1)
    v <- newEmptyTMVar
    modifyTVar' (sPending s) (M.insert n v)
    pure (n, v)
  let msg = PDict ([("method", PStr method)] <> maybe [] (\p -> [("params", p)]) params <> [("jsonrpc", PStr "2.0"), ("id", PInt rid)])
  sent <- try (tSend (sTransport s) msg)
  case sent of
    Left (e :: SomeException) -> do
      atomically (modifyTVar' (sPending s) (M.delete rid))
      throwIO (McpFailure "ClosedResourceError" (T.pack (show e)))
    Right () -> atomically (takeTMVar slot) >>= either throwIO pure

notify :: Session -> Text -> Maybe PyVal -> IO ()
notify s method params = do
  closed <- readTVarIO (sClosed s)
  when closed (throwIO (McpFailure "ClosedResourceError" ""))
  tSend (sTransport s) (PDict ([("method", PStr method)] <> maybe [] (\p -> [("params", p)]) params <> [("jsonrpc", PStr "2.0")]))

sessionClosed :: Session -> IO Bool
sessionClosed s = readTVarIO (sClosed s)

-- | Blocks until the session is closed (the server's output ended).
sessionClosedSTM :: Session -> STM ()
sessionClosedSTM s = readTVar (sClosed s) >>= check

-- | Blocks until the transport dies with an error; its message.
sessionFatal :: Session -> STM Text
sessionFatal s = readTMVar (sFatal s)

closeSession :: Session -> IO ()
closeSession s = do
  atomically (writeTVar (sClosed s) True)
  tClose (sTransport s)
