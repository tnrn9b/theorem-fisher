-- | The compute controller's watcher: one attempt's child runs while its
-- progress is polled (every 5 s) from the native agent's ledger in
-- @state.db@ or from the attempt's stream; the stall rule
-- ("Fisher.Search.Controller") stops a child that works without changing
-- its target. It also prices the attempt (the ledger's cost rows), keeps
-- the model's last message, and reads a finished attempt's model failure.
--
-- The ledger reader is fisher-ui's ("Fisher.UI.Db": 'withRo',
-- @ledgerEvents@, @ledgerRun@) in small, read-only and copied, since
-- fisher-cli does not build on fisher-ui. A poll that fails leaves the
-- progress as it was; three in a row switch the attempt to its stream. A
-- tag with no session yet is a good read for three polls; each poll after
-- that fails, so an agent that never records its tag (its ledger did not
-- open, another Fisher home) goes to its stream at the sixth poll (30 s),
-- whose turns are then counted from its start.
module Fisher.Cli.Watch
  ( Source (..)
  , Watched (..)
  , watchChild
  , WatchOpts (..)
  , defaultWatchOpts
  , watchChildWith
  , streamSignals
  , streamLast
  , failureOf
  , readLines
  , ledgerDollars
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (cancel, waitCatch, withAsync)
import qualified Control.Concurrent.Async as Async
import Control.Exception (IOException, SomeAsyncException, SomeException, bracket, displayException, fromException, throwIO, try)
import Data.Aeson (Value (..), decodeStrict)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Data.List (foldl')
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Time.Clock (addUTCTime, getCurrentTime)
import qualified Data.Vector as V
import Database.SQLite.Simple (Connection, Error (..), Only (..), SQLError (..), close, execute_, open, query)
import System.Directory (canonicalizePath, makeAbsolute)
import System.Timeout (timeout)

import Fisher.LLM.Auth.Sha256 (sha256Hex)
import Fisher.Search.Controller
import Fisher.Search.Headless (nativeBackend)

-- | Where an attempt's progress is read.
data Source
  = FromLedger FilePath Text FilePath
  -- ^ @state.db@, the attempt's @FISHER_UI_TAG@, the target's absolute path
  | FromStream FilePath Text FilePath
  -- ^ @stream.jsonl@, the backend, the target's absolute path
  deriving (Eq, Show)

-- | What the watcher saw: the stop (when it stopped the child), the
-- dollars (Nothing when no ledger cost row had a number), the model's last
-- message, and why the ledger was given up (once three reads failed).
data Watched = Watched
  { wStop :: Maybe Stop
  , wDollars :: Maybe Double
  , wLast :: Maybe Text
  , wUnread :: Maybe Text
  }
  deriving (Eq, Show)

data WatchOpts = WatchOpts
  { woInterval :: Double
  -- ^ seconds between polls
  , woUnread :: Text -> IO ()
  -- ^ called once, when the ledger is given up for the stream
  , woStream :: Maybe FilePath
  -- ^ the native attempt's stream, read when the ledger is given up
  , woSince :: Double
  -- ^ the attempt's start (POSIX seconds): a @ui_runs@ row started before
  -- it is another run's, never read
  }

defaultWatchOpts :: WatchOpts
defaultWatchOpts = WatchOpts 5 (\_ -> pure ()) Nothing 0

-- | Run the child, polling every 5 s; with early stop on (the Bool), a
-- stall stops it: the child is cancelled and the result is Left. Else
-- Right with the child's value once it ends (after a last poll).
watchChild :: StallRule -> Bool -> Source -> IO a -> IO (Either Watched (a, Watched))
watchChild = watchChildWith defaultWatchOpts

data St = St
  { sSource :: Source
  , sProgress :: Progress
  , sSeq :: Int
  -- ^ the last ledger number read
  , sSession :: Maybe Text
  , sFails :: Int
  -- ^ ledger reads failed in a row
  , sPriced :: Bool
  , sLast :: Maybe Text
  , sTurns :: Int
  -- ^ the stream's turns counted so far
  , sHash :: Maybe Text
  -- ^ the target's sha256 at the last poll of a stream
  , sUnread :: Maybe Text
  , sNoSession :: Int
  -- ^ polls that found no session for the tag
  }

watchChildWith :: WatchOpts -> StallRule -> Bool -> Source -> IO a -> IO (Either Watched (a, Watched))
watchChildWith opts rule early src0 child = do
  src <- case src0 of
    FromLedger db tag t -> FromLedger db tag <$> canonical t
    FromStream p b t -> FromStream p b <$> canonical t
  st0 <- baseline src (St src emptyProgress 0 Nothing 0 False Nothing 0 Nothing Nothing 0)
  ref <- newIORef st0
  withAsync child $ \a ->
    let done s = \case
          Left e -> throwIO e
          Right x -> pure (Right (x, watched s))
        loop = do
          r <- timeout (max 1 (round (woInterval opts * 1000000))) (waitCatch a)
          poll opts ref
          s <- readIORef ref
          case r of
            Just res -> done s res
            Nothing -> case stallVerdict rule (sProgress s) of
              -- a child that ended while the poll ran is not stopped
              Just st | early -> Async.poll a >>= \case
                Just res -> done s res
                Nothing -> cancel a >> pure (Left (watched s) {wStop = Just st})
              _ -> loop
     in loop

-- | The path with its links resolved (a step's path and the target compare
-- this way); absolute only, when that fails.
canonical :: FilePath -> IO FilePath
canonical p = do
  r <- try (canonicalizePath p) :: IO (Either IOException FilePath)
  case r of
    Right q -> pure q
    Left _ -> either (const p) id <$> (try (makeAbsolute p) :: IO (Either IOException FilePath))

watched :: St -> Watched
watched s = Watched Nothing (if sPriced s then Just (pgTotal (sProgress s)) else Nothing) (sLast s) (sUnread s)

-- | A stream's starting point: its turns so far and the target's hash.
baseline :: Source -> St -> IO St
baseline src s = case src of
  FromLedger {} -> pure s
  FromStream path backend target -> do
    ls <- readLines path
    h <- hashFile target
    pure s {sTurns = turnCount (streamSignals backend ls), sHash = h}

turnCount :: [Signal] -> Int
turnCount sigs = length [() | Turn <- sigs]

poll :: WatchOpts -> IORef St -> IO ()
poll opts ref = do
  s <- readIORef ref
  case sSource s of
    FromLedger db tag target -> do
      r <- try (readLedger db tag (woSince opts) (sSession s) (sSeq s))
      let failedRead why
            | sFails s + 1 >= 3 = do
                let fallback = FromStream (maybe "" id (woStream opts)) nativeBackend target
                woUnread opts why
                s' <- baseline fallback s {sSource = fallback}
                -- with no session ever read, no turn was counted: the
                -- stream's count from its start
                writeIORef ref s' {sUnread = Just why, sTurns = if isJust (sSession s) then sTurns s' else 0}
            | otherwise = writeIORef ref s {sFails = sFails s + 1, sNoSession = sNoSession s + 1}
      case r of
        Left e | isJust (fromException e :: Maybe SomeAsyncException) -> throwIO e
        Left e -> failedRead ("three reads of " <> T.pack db <> " failed in a row; the stream from now on: "
                                <> T.take 300 (T.pack (displayException (e :: SomeException))))
        Right (Nothing, _)
          | sNoSession s >= 3 -> failedRead ("no session for " <> tag <> " in " <> T.pack db <> " after "
                                               <> T.pack (show (sNoSession s + 1)) <> " polls; the stream from now on")
          | otherwise -> writeIORef ref s {sFails = 0, sNoSession = sNoSession s + 1}
        Right (sid, rows0) -> do
          rows <- mapM canonicalStep rows0
          let sigs = ledgerSignals target rows
          writeIORef ref s
            { sProgress = foldl' feed (sProgress s) sigs
            , sSeq = maximum (sSeq s : [n | (n, _, _) <- rows])
            , sSession = sid
            , sFails = 0
            , sPriced = sPriced s || not (null [() | Spent _ <- sigs])
            , sLast = lastAssistant rows <|> sLast s }
    FromStream path backend target -> do
      ls <- readLines path
      h <- hashFile target
      let total = turnCount (streamSignals backend ls)
          new = max 0 (total - sTurns s)
          sigs = replicate new Turn <> [Changed | h /= sHash s]
      writeIORef ref s
        { sProgress = foldl' feed (sProgress s) sigs
        , sTurns = max total (sTurns s)
        , sHash = h
        , sLast = streamLast backend ls <|> sLast s }

-- | A @step@ row with its path canonical.
canonicalStep :: (Int, Text, Value) -> IO (Int, Text, Value)
canonicalStep row = case row of
  (n, "step", Object o) | Just (String p) <- KM.lookup "path" o -> do
    q <- canonical (T.unpack p)
    pure (n, "step", Object (KM.insert "path" (String (T.pack q)) o))
  _ -> pure row

-- ---------------------------------------------------------------- the ledger

-- | The tag's session (known, or looked up in @ui_runs@ among the rows
-- started at or after @since@; Nothing while the agent has not recorded
-- it) and its rows after a number.
readLedger :: FilePath -> Text -> Double -> Maybe Text -> Int -> IO (Maybe Text, [(Int, Text, Value)])
readLedger db tag since known after = withRo db $ \c -> do
  sid <- maybe (ledgerRun c tag since) (pure . Just) known
  case sid of
    Nothing -> pure (Nothing, [])
    Just s -> (,) (Just s) <$> ledgerEvents c s after

-- | The dollars of the tag's ledger cost rows (its session started at or
-- after @since@), for an attempt cancelled before its watcher returned;
-- Nothing with no session or no cost row with a number.
ledgerDollars :: FilePath -> Text -> Double -> IO (Maybe Double)
ledgerDollars db tag since = do
  (sid, rows) <- readLedger db tag since Nothing 0
  let spent = [d | Spent d <- ledgerSignals "" rows]
  pure (if null spent || sid == Nothing then Nothing else Just (sum spent))

-- | Open @path@ read-only and run the action, retrying the whole read on
-- SQLITE_BUSY or SQLITE_LOCKED for up to two seconds (as fisher-ui does).
-- Always @mode=ro@, never @immutable=1@: the agent is writing.
withRo :: FilePath -> (Connection -> IO a) -> IO a
withRo path k = do
  deadline <- addUTCTime 2 <$> getCurrentTime
  let go = do
        r <- try (bracket (open (roUri path)) close (\c -> setup c >> k c))
        case r of
          Right a -> pure a
          Left e
            | sqlError e `elem` [ErrorBusy, ErrorLocked] -> do
                now <- getCurrentTime
                if now >= deadline then throwIO e else threadDelay 50000 >> go
            | otherwise -> throwIO e
  go
  where
    setup c = do
      execute_ c "PRAGMA busy_timeout = 250"
      execute_ c "PRAGMA query_only = ON"

roUri :: FilePath -> String
roUri p = "file:" <> concatMap esc p <> "?mode=ro"
  where
    esc = \case
      '%' -> "%25"
      '?' -> "%3f"
      '#' -> "%23"
      ' ' -> "%20"
      c -> [c]

hasTable :: Connection -> Text -> IO Bool
hasTable c name = not . null <$> (query c "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = ?" (Only name) :: IO [Only Int])

ledgerEvents :: Connection -> Text -> Int -> IO [(Int, Text, Value)]
ledgerEvents c sid after = do
  has <- hasTable c "ui_events"
  if not has
    then pure []
    else do
      rows <- query c "SELECT seq, kind, data FROM ui_events WHERE session_id = ? AND seq > ? ORDER BY seq" (sid, after) :: IO [(Int, Text, Text)]
      pure [(n, kd, v) | (n, kd, d) <- rows, Just v <- [decodeStrict (TE.encodeUtf8 d)]]

ledgerRun :: Connection -> Text -> Double -> IO (Maybe Text)
ledgerRun c tag since = do
  has <- hasTable c "ui_runs"
  if not has
    then pure Nothing
    else listToMaybe . map fromOnly <$> (query c "SELECT session_id FROM ui_runs WHERE tag = ? AND started >= ?" (tag, since) :: IO [Only Text])

-- ---------------------------------------------------------------- streams

-- | Turns in a stream's lines (no file hashing here): claude-code, one per
-- distinct @message.id@ among @assistant@ lines; codex, one per
-- @item.completed@ whose item is an @agent_message@ or @reasoning@; the
-- native agent's stream, one per @assistant@ line.
streamSignals :: Text -> [Value] -> [Signal]
streamSignals backend ls = case backend of
  "claude-code" -> go Set.empty [v | v <- ls, str "type" v == Just "assistant"]
  "codex" -> [Turn | v <- ls, str "type" v == Just "item.completed", (at "item" v >>= str "type") `elem` [Just "agent_message", Just "reasoning"]]
  _ -> [Turn | v <- ls, str "type" v == Just "assistant"]
  where
    go _ [] = []
    go seen (v : vs) = case at "message" v >>= str "id" of
      Just i | Set.member i seen -> go seen vs
             | otherwise -> Turn : go (Set.insert i seen) vs
      Nothing -> Turn : go seen vs

-- | The last assistant text in a stream.
streamLast :: Text -> [Value] -> Maybe Text
streamLast backend ls = listToMaybe (reverse (mapMaybe text ls))
  where
    text v
      | backend == "codex" = case at "item" v of
          Just it | str "type" v == Just "item.completed", str "type" it == Just "agent_message" -> nonEmpty =<< str "text" it
          _ -> Nothing
      | str "type" v == Just "assistant" = case at "message" v >>= at "content" of
          Just (Array cs) -> nonEmpty (T.intercalate "\n" [t | c <- V.toList cs, str "type" c == Just "text", Just t <- [str "text" c]])
          _ -> Nothing
      | otherwise = Nothing
    nonEmpty t = if T.null (T.strip t) then Nothing else Just t

-- | A finished attempt's model failure (not asked of a stopped one), from
-- its stream's final @result@ line when it ended in an error: its @error@,
-- and its @result@ (which can be the model's own prose) only when
-- @is_error@ is true or the subtype starts with @error@; a successful
-- result is no failure. With no result line,
-- and only when the child exited non-zero (the Int, Nothing when it was
-- killed), from the stream's error lines (codex) and stderr.
failureOf :: FilePath -> FilePath -> Maybe Int -> IO (Maybe ModelFailure)
failureOf streamPath stderrPath exit = do
  ls <- readLines streamPath
  case listToMaybe (reverse [v | v <- ls, str "type" v == Just "result"]) of
    Just r
      | failed r -> pure (classifyFailure (T.unlines (mapMaybe (`str` r) ("error" : ["result" | errorResult r]))))
      | otherwise -> pure Nothing
    Nothing
      | Just c <- exit, c /= 0 -> do
          err <- either (const "") (TE.decodeUtf8With TEE.lenientDecode) <$> (try (BS.readFile stderrPath) :: IO (Either IOException BS.ByteString))
          pure (classifyFailure (T.unlines (mapMaybe errorLine ls) <> err))
      | otherwise -> pure Nothing
  where
    errorResult r = at "is_error" r == Just (Bool True) || maybe False ("error" `T.isPrefixOf`) (str "subtype" r)
    failed r = at "is_error" r == Just (Bool True)
      || maybe False (/= "success") (str "subtype" r)
      || maybe False (not . T.null . T.strip) (str "error" r)
    errorLine v = case str "type" v of
      Just "error" -> str "message" v
      Just "turn.failed" -> at "error" v >>= str "message"
      _ -> Nothing

-- | A JSON-lines file's objects; a missing file or a partial line is none.
readLines :: FilePath -> IO [Value]
readLines p = do
  r <- try (BS.readFile p) :: IO (Either IOException BS.ByteString)
  pure $ case r of
    Left _ -> []
    Right bs -> mapMaybe decodeStrict (BC.lines bs)

hashFile :: FilePath -> IO (Maybe Text)
hashFile p = either (const Nothing) (Just . sha256Hex) <$> (try (BS.readFile p) :: IO (Either IOException BS.ByteString))

at :: Text -> Value -> Maybe Value
at k = \case
  Object o -> KM.lookup (K.fromText k) o
  _ -> Nothing

str :: Text -> Value -> Maybe Text
str k v = case at k v of
  Just (String t) -> Just t
  _ -> Nothing
