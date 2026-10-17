-- | A stand-in for @lake serve@ that replays a recorded transcript.
--
-- Each message Fisher writes is matched with the earliest unconsumed
-- recorded client message of the same method and params (responses to
-- server requests, @workspace/didChangeWatchedFiles@ and @$/cancelRequest@
-- are not matched: their timing is the machine's). A recorded server
-- message is sent once every client message recorded before it has been
-- matched, with a response's id rewritten to the id Fisher used for that
-- request. A message with no recorded counterpart is noted (a request gets
-- an error answer), so a test can name the first divergence.
module Fisher.Lean.FakeLake
  ( FakeLake
  , newFakeLake
  , fakeWire
  , fakeMismatches
  , fakeBlockedOn
  ) where

import Control.Concurrent.STM
import Control.Monad (forM_, unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Lean.Transport (Wire (..), encodeCompact)
import Fisher.Py.Value

data Entry = Entry
  { eIdx :: Int
  , eDir :: Text
  , eMsg :: PyVal
  }

data FakeLake = FakeLake
  { fEntries :: [Entry]
  , fConsumed :: IORef (M.Map Int ())
  , fSent :: IORef (M.Map Int ())
  , fIdMap :: IORef (M.Map Integer PyVal)
  -- ^ recorded request id -> Fisher's id
  , fOut :: IORef (TQueue PyVal)
  -- ^ the newest connection's queue (a restart's old reader must not take
  -- the new server's messages)
  , fMismatch :: IORef [Text]
  , fLock :: TMVar ()
  }

newFakeLake :: [PyVal] -> IO FakeLake
newFakeLake lsp = do
  let es = [Entry i (maybe "" pyStrOf (get "dir" e)) (maybe PNull id (get "msg" e)) | (i, e) <- zip [0 ..] lsp]
  f <- FakeLake es <$> newIORef M.empty <*> newIORef M.empty <*> newIORef M.empty <*> (newTQueueIO >>= newIORef) <*> newIORef [] <*> newTMVarIO ()
  release f
  pure f

fakeMismatches :: FakeLake -> IO [Text]
fakeMismatches = readIORef . fMismatch

-- | The first recorded client message still waited for (what blocks the
-- replay), if any.
fakeBlockedOn :: FakeLake -> IO (Maybe Text)
fakeBlockedOn f = do
  consumed <- readIORef (fConsumed f)
  pure $ case [e | e <- fEntries f, eDir e == "c2s", not (ignorable (eMsg e)), not (M.member (eIdx e) consumed)] of
    (e : _) -> Just (T.take 300 (dumpsUnicode (eMsg e)))
    [] -> Nothing

ignorable :: PyVal -> Bool
ignorable m =
  let method = get "method" m
   in (isJust (get "id" m) && method == Nothing)
        || method == Just (PStr "workspace/didChangeWatchedFiles")
        || method == Just (PStr "$/cancelRequest")

-- | Params as compared: the pid in @initialize@ is not.
norm :: PyVal -> PyVal
norm m = case get "method" m of
  Just (PStr "initialize") -> maybe PNull (set "processId" (PInt 0)) (get "params" m)
  _ -> maybe PNull id (get "params" m)

release :: FakeLake -> IO ()
release f = do
  consumed <- readIORef (fConsumed f)
  sent <- readIORef (fSent f)
  ids <- readIORef (fIdMap f)
  let go _ [] = pure ()
      go blocked (e : rest)
        | eDir e == "c2s" =
            go (blocked || (not (ignorable (eMsg e)) && not (M.member (eIdx e) consumed))) rest
        | blocked || M.member (eIdx e) sent = go blocked rest
        | otherwise = do
            let m = eMsg e
                m' = case (get "method" m, get "id" m) of
                  (Nothing, Just (PInt rid)) -> maybe m (\fid -> set "id" fid m) (M.lookup rid ids)
                  _ -> m
            modifyIORef' (fSent f) (M.insert (eIdx e) ())
            q <- readIORef (fOut f)
            atomically (writeTQueue q m')
            go blocked rest
  go False (fEntries f)

onClientMessage :: FakeLake -> PyVal -> IO ()
onClientMessage f m = unless (ignorable m) $ do
  consumed <- readIORef (fConsumed f)
  let method = get "method" m
      cands = [e | e <- fEntries f, eDir e == "c2s", not (M.member (eIdx e) consumed), get "method" (eMsg e) == method, norm (eMsg e) == norm m]
  case cands of
    (e : _) -> do
      modifyIORef' (fConsumed f) (M.insert (eIdx e) ())
      case (get "id" (eMsg e), get "id" m) of
        (Just (PInt rid), Just fid) -> modifyIORef' (fIdMap f) (M.insert rid fid)
        _ -> pure ()
      release f
    [] -> do
      modifyIORef' (fMismatch f) (<> ["not in the transcript: " <> T.take 400 (dumpsUnicode m)])
      q <- readIORef (fOut f)
      forM_ (get "id" m) $ \fid ->
        atomically . writeTQueue q $
          PDict [("jsonrpc", PStr "2.0"), ("id", fid), ("error", PDict [("code", PInt (-32603)), ("message", PStr ("not in transcript: " <> maybe "" pyStrOf method))])]

-- | A connection to the fake server (each @lake serve@ start gets one; the
-- transcript is shared, so a restart continues it).
fakeWire :: FakeLake -> IO Wire
fakeWire f = do
  buf <- newIORef BS.empty
  q <- newTQueueIO
  writeIORef (fOut f) q
  pure
    Wire
      { wWrite = \bytes -> do
          atomically (takeTMVar (fLock f))
          modifyIORef' buf (<> bytes)
          let drain = do
                b <- readIORef buf
                case parseFrame b of
                  Just (v, rest) -> writeIORef buf rest >> onClientMessage f v >> drain
                  Nothing -> pure ()
          drain
          atomically (putTMVar (fLock f) ())
      , wRead = Right <$> atomically (readTQueue q)
      , wClose = pure ()
      , wStderr = pure "" }
  where
    _ = encodeCompact

parseFrame :: BS.ByteString -> Maybe (PyVal, BS.ByteString)
parseFrame b = do
  let (hdr, rest) = BS.breakSubstring "\r\n\r\n" b
  unless' (BS.null rest)
  n <- fst <$> BC.readInt (BC.dropWhile (== ' ') (BS.drop 15 hdr))
  let body = BS.take n (BS.drop 4 rest)
  if BS.length body < n
    then Nothing
    else case loads body of
      Right v -> Just (v, BS.drop (4 + n) rest)
      Left _ -> Nothing
  where
    unless' c = if c then Nothing else Just ()
