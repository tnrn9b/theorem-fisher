-- | JSON-RPC over a @lake serve@ process (leanclient @aio/transport.py@):
-- Content-Length framing, request ids from 1, every pending request failed
-- when the connection dies, server requests answered (the watcher
-- registration and the refresh requests with @null@, anything else with
-- -32601), and @$/cancelRequest@ on a timeout.
--
-- The byte stream is a 'Wire', so a test can stand in for the server.
module Fisher.Lean.Transport
  ( Wire (..)
  , processWire
  , Transport
  , newTransport
  , transportAlive
  , request
  , notify
  , closeTransport
  , encodeCompact
  , pyFloatSeconds
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (SomeException, catch, onException, throwIO, try)
import Control.Monad (forM_, unless, void, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (isSpace, ord, toLower)
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Numeric (showHex)
import System.IO
import System.Posix.Signals (sigKILL, signalProcessGroup)
import System.Process
import System.Timeout (timeout)

import Fisher.Lean.Ref (modifyRef)
import Fisher.Lean.Error
import Fisher.Py.Json (pyFloatRepr)
import Fisher.Py.Value

-- | One connection's bytes: write a frame, read the next message (or the
-- reason the connection ended), close.
data Wire = Wire
  { wWrite :: BS.ByteString -> IO ()
  , wRead :: IO (Either Text PyVal)
  , wClose :: IO ()
  , wStderr :: IO Text
  }

-- | @orjson.dumps@: compact, non-ASCII kept, control characters escaped.
encodeCompact :: PyVal -> BS.ByteString
encodeCompact = TE.encodeUtf8 . go
  where
    go = \case
      PNull -> "null"
      PBool b -> if b then "true" else "false"
      PInt n -> T.pack (show n)
      PFloat d -> pyFloatRepr d
      PStr s -> qstr s
      PList xs -> "[" <> T.intercalate "," (map go xs) <> "]"
      PDict kvs -> "{" <> T.intercalate "," [qstr k <> ":" <> go v | (k, v) <- kvs] <> "}"
    qstr s = "\"" <> T.concatMap esc s <> "\""
    esc c = case c of
      '"' -> "\\\""
      '\\' -> "\\\\"
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      '\b' -> "\\b"
      '\f' -> "\\f"
      _ | ord c < 0x20 -> "\\u" <> T.justifyRight 4 '0' (T.pack (showHex (ord c) ""))
        | otherwise -> T.singleton c

frame :: PyVal -> BS.ByteString
frame v = let body = encodeCompact v in BC.pack ("Content-Length: " <> show (BS.length body) <> "\r\n\r\n") <> body

-- | @lake serve@ (or any command) in its own process group, in @cwd@;
-- stderr kept as a 64 KiB tail. Closing SIGKILLs the whole group, as
-- leanclient does (no @shutdown@/@exit@).
processWire :: [String] -> FilePath -> IO Wire
processWire cmd cwd = do
  (Just hin, Just hout, Just herr, ph) <-
    createProcess
      (proc (head cmd) (tail cmd))
        { cwd = Just cwd, std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe, new_session = True }
  mapM_ (`hSetBinaryMode` True) [hin, hout, herr]
  hSetBuffering hin (BlockBuffering Nothing)
  tailRef <- newIORef BS.empty
  _ <- forkIO $
    let loop = do
          chunk <- BS.hGetSome herr 4096 `catch` \(_ :: SomeException) -> pure BS.empty
          unless (BS.null chunk) $ do
            modifyRef tailRef (\t -> let t' = t <> chunk in if BS.length t' > 65536 then BS.drop (BS.length t' - 65536) t' else t')
            loop
     in loop
  let readHeaders cl = do
        r <- try (BS.hGetLine hout)
        case r of
          Left (_ :: SomeException) -> pure (Left "Language server closed the connection")
          Right raw -> do
            let line = BC.dropWhile isSpace (BC.dropWhileEnd isSpace raw)
            if BS.null line
              then pure (Right cl)
              else
                if BC.map toLower (BC.take 15 line) == "content-length:"
                  then case BC.readInt (BC.dropWhile isSpace (BC.drop 15 line)) of
                    Just (n, rest) | BS.null (BC.dropWhile isSpace rest) -> readHeaders (Just n)
                    _ -> pure (Left ("Malformed Content-Length header: " <> T.pack (show line)))
                  else readHeaders cl
      readMsg = do
        eof <- hIsEOF hout `catch` \(_ :: SomeException) -> pure True
        if eof
          then pure (Left "Language server closed the connection")
          else
            readHeaders Nothing >>= \case
              Left e -> pure (Left e)
              Right Nothing -> pure (Left "Message without Content-Length header")
              Right (Just n) -> do
                body <- BS.hGet hout n
                if BS.length body < n
                  then pure (Left "Language server closed the connection")
                  else case loads body of
                    Right v -> pure (Right v)
                    Left e -> pure (Left ("Malformed JSON from server: " <> T.pack e))
      close' = do
        _ <- try @SomeException (hClose hin)
        mpid <- getPid ph
        forM_ mpid $ \pid -> void (try @SomeException (signalProcessGroup sigKILL pid))
        _ <- try @SomeException (timeout 5000000 (waitForProcess ph))
        pure ()
  pure
    Wire
      { wWrite = \b -> BS.hPut hin b >> hFlush hin
      , wRead = readMsg
      , wClose = close'
      , wStderr = TE.decodeUtf8With TEE.lenientDecode <$> readIORef tailRef }

-- | @f"{x}s"@ for a float timeout.
pyFloatSeconds :: Double -> Text
pyFloatSeconds = pyFloatRepr

data Transport = Transport
  { tWire :: Wire
  , tIds :: IORef Integer
  , tPending :: IORef (M.Map Integer (MVar (Either LeanError PyVal)))
  , tWriteLock :: MVar ()
  , tDeath :: IORef (Maybe LeanError)
  , tOnNotification :: Text -> PyVal -> IO ()
  , tOnServerRequest :: Text -> PyVal -> IO ()
  , tDefaultTimeout :: Double
  }

ackRequests :: [Text]
ackRequests =
  [ "client/registerCapability", "client/unregisterCapability", "workspace/semanticTokens/refresh"
  , "workspace/inlayHint/refresh", "workspace/codeLens/refresh", "workspace/diagnostic/refresh" ]

-- | Start reading. The callbacks see notifications and acknowledged server
-- requests (their exceptions are swallowed).
newTransport :: Wire -> Double -> (Text -> PyVal -> IO ()) -> (Text -> PyVal -> IO ()) -> IO Transport
newTransport wire defTimeout onNote onReq = do
  t <- Transport wire <$> newIORef 1 <*> newIORef M.empty <*> newMVar () <*> newIORef Nothing <*> pure onNote <*> pure onReq <*> pure defTimeout
  _ <- forkIO (readLoop t)
  pure t

transportAlive :: Transport -> IO Bool
transportAlive t = (== Nothing) . fmap (const ()) <$> readIORef (tDeath t)

die :: Transport -> LeanError -> IO ()
die t e = do
  first <- atomicModifyIORef' (tDeath t) (\d -> case d of Nothing -> (Just e, True); Just _ -> (d, False))
  when first $ do
    pend <- atomicModifyIORef' (tPending t) (\m -> (M.empty, m))
    forM_ (M.elems pend) $ \mv -> void (tryPutMVar mv (Left e))

readLoop :: Transport -> IO ()
readLoop t = do
  r <- try (wRead (tWire t))
  case r of
    Left (e :: SomeException) -> do
      tl <- wStderr (tWire t)
      die t (TransportError ("Reader failed: " <> T.pack (show e)) tl)
    Right (Left msg) -> do
      tl <- wStderr (tWire t)
      die t (TransportError msg tl)
    Right (Right msg) -> do
      dispatch t msg `catch` \(_ :: SomeException) -> pure ()
      readLoop t

dispatch :: Transport -> PyVal -> IO ()
dispatch t msg =
  case (get "method" msg, get "id" msg) of
    (Just (PStr method), Just mid) | mid /= PNull -> void . forkIO $ answer method mid (paramsOf msg)
    (_, Just (PInt mid)) -> do
      mv <- atomicModifyIORef' (tPending t) (\m -> (M.delete mid m, M.lookup mid m))
      forM_ mv $ \v -> void . tryPutMVar v $
        case get "error" msg of
          Just err | has "error" msg ->
            let code = case get "code" err of Just (PInt c) -> c; _ -> 0
                m = case get "message" err of Just (PStr s) -> s; _ -> ""
             in Left (if code == -32800 then RequestCancelled m else RpcError code m (maybe PNull id (get "data" err)))
          _ -> Right (maybe PNull id (get "result" msg))
    (Just (PStr method), _) -> tOnNotification t method (paramsOf msg) `catch` \(_ :: SomeException) -> pure ()
    _ -> pure ()
  where
    paramsOf m = case get "params" m of
      Just p@(PDict _) -> p
      Just PNull -> PDict []
      Nothing -> PDict []
      Just p -> p
    answer method mid params = do
      payload <-
        if method `elem` ackRequests
          then do
            tOnServerRequest t method params `catch` \(_ :: SomeException) -> pure ()
            pure (PDict [("jsonrpc", PStr "2.0"), ("id", mid), ("result", PNull)])
          else pure (PDict [("jsonrpc", PStr "2.0"), ("id", mid), ("error", PDict [("code", PInt (-32601)), ("message", PStr ("Unsupported: " <> method))])])
      void (try @LeanError (write t payload))

write :: Transport -> PyVal -> IO ()
write t payload = do
  d <- readIORef (tDeath t)
  case d of
    Just e -> throwIO e
    Nothing -> withMVar (tWriteLock t) $ \_ -> do
      r <- try (wWrite (tWire t) (frame payload))
      case r of
        Right () -> pure ()
        Left (e :: SomeException) -> do
          tl <- wStderr (tWire t)
          let err = TransportError ("Write failed: " <> T.pack (show e)) tl
          die t err
          throwIO err

notify :: Transport -> Text -> PyVal -> IO ()
notify t method params = write t (PDict [("jsonrpc", PStr "2.0"), ("method", PStr method), ("params", params)])

-- | The @result@ of a request; 'LeanError' on an error answer, a timeout
-- (after @$/cancelRequest@) or a dead connection.
request :: Transport -> Text -> PyVal -> Maybe Double -> IO PyVal
request t method params mtimeout = do
  let secs = maybe (tDefaultTimeout t) id mtimeout
  rid <- atomicModifyIORef' (tIds t) (\i -> (i + 1, i))
  mv <- newEmptyMVar
  modifyRef (tPending t) (M.insert rid mv)
  write t (PDict [("jsonrpc", PStr "2.0"), ("id", PInt rid), ("method", PStr method), ("params", params)])
    `onException` modifyRef (tPending t) (M.delete rid)
  r <- timeout (round (secs * 1e6)) (readMVar mv) `onException` abandon rid
  case r of
    Nothing -> do
      abandon rid
      throwIO (RequestTimeout (method <> " timed out after " <> pyFloatSeconds secs <> "s"))
    Just (Left e) -> throwIO e
    Just (Right v) -> pure v
  where
    abandon rid = do
      modifyRef (tPending t) (M.delete rid)
      alive <- transportAlive t
      when alive $ void (try @LeanError (notify t "$/cancelRequest" (PDict [("id", PInt rid)])))

closeTransport :: Transport -> IO ()
closeTransport t = do
  tl <- wStderr (tWire t)
  die t (TransportError "Transport closed" tl)
  wClose (tWire t)
