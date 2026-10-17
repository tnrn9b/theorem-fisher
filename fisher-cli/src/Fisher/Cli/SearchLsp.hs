-- | The search's 'Lsp' over fisher-lean: one Lean language server per run,
-- started in the Lake project it is given (the search's check workspace),
-- and one scratch document in it.
--
-- The search sends each text cut after the declaration it works on, so a
-- trial elaborates only up to its goal. The warm-up elaborates the whole
-- file once: the scratch document then holds every declaration's
-- elaboration, which later trials reuse up to the line they change.
--
-- Each snippet is placed with 'fillHoleAt', exactly as Auto splices a
-- winner, and the whole text is sent to the scratch document (a virtual
-- document: nothing is written to disk). The goal after the snippet is
-- asked where the snippet ends. A trial's diagnostics are, as
-- @lean_multi_attempt@ reports them, those on the snippet's lines and
-- those new against the file without the snippet. Positions go to the
-- client in code points; the client converts them to the UTF-16 columns
-- the server counts in.
--
-- A call cut off by its caller (a timeout) leaves its request cancelled
-- (fisher-lean's transport sends @$/cancelRequest@ when a wait is
-- interrupted) and the scratch document dirty: the next call closes that
-- document and opens a fresh one under a new name, so nothing the server
-- still says about the old one is read. A dead server is never restarted:
-- every call answers 'Left' from then on, and so does every failure, so
-- the search turns the automation off.
module Fisher.Cli.SearchLsp
  ( openLsp
  , openLspWith
  -- * For the specs
  , diagOf
  , localTo
  ) where

import Control.Concurrent.MVar
import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Control.Monad (unless, when)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import System.FilePath ((</>))
import System.Timeout (timeout)

import Fisher.Lean.Client (Client, DiagnosticsReport (..), GoalResult (..), clientAlive)
import Fisher.Lean.Scratch (ScratchPool, TrialResult (..), closePool, newScratchPool, runText)
import Fisher.Lean.Session
import Fisher.Lean.Tool (exceptionText)
import Fisher.Py.Value (PyVal (..), get)
import Fisher.Search.Auto

-- | The language server for a file of a project (the path relative to
-- the root): started, and the file elaborated once (its imports loaded),
-- before the search begins, within the given seconds. 'Left' when it does
-- not start or the warm-up fails or runs out of time (the server is then
-- stopped).
openLsp :: Double -> FilePath -> FilePath -> IO (Either Text Lsp)
openLsp = openLspWith defaultLeanEnv

data St = St
  { stPool :: Maybe ScratchPool
  , stGen :: Int
  , stDirty :: Bool
  , stBase :: Maybe (Text, [PyVal])
  -- ^ the last text elaborated as it is, and its diagnostics
  }

-- | 'openLsp' over a given environment (the specs pass a stand-in server).
openLspWith :: LeanEnv -> Double -> FilePath -> FilePath -> IO (Either Text Lsp)
openLspWith env warmSecs root rel = do
  started <- safely (newSession env) >>= \case
    Left e -> pure (Left e)
    Right s -> do
      -- a failure once the session exists stops whatever it started
      r <- safely $ do
        root' <- bindProject s root
        startupClient s
        c <- getClient s
        pure (s, root', c)
      case r of
        Left _ -> do
          mroot <- boundProject s
          mapM_ (\b -> try @SomeException (stopProjectClients s b)) mroot
          _ <- try @SomeException (closeSession s)
          pure ()
        Right _ -> pure ()
      pure r
  case started of
    Left e -> pure (Left e)
    Right (s, r, c) -> do
      lock <- newMVar ()
      st <- newIORef (St Nothing 0 False Nothing)
      closed <- newIORef False
      let lsp = adapter s r c lock st closed
      src <- safely (TE.decodeUtf8With TEE.lenientDecode <$> BS.readFile (root </> rel))
      let shown = if warmSecs == fromIntegral (round warmSecs :: Int) then show (round warmSecs :: Int) else show warmSecs
          late = "the warm-up took longer than " <> T.pack shown <> " s"
      warm <- case src of
        Left e -> pure (Left e)
        Right t -> maybe (Left late) (fmap (const ())) <$> timeout (max 1 (ceiling (warmSecs * 1000000))) (lspDiagnostics lsp t)
      case warm of
        Left e -> lspClose lsp >> pure (Left e)
        Right () -> pure (Right lsp)

adapter :: Session -> FilePath -> Client -> MVar () -> IORef St -> IORef Bool -> Lsp
adapter s root c lock st closed =
  Lsp
    { lspAttempts = \src h snips -> call $ \pool -> mapM (attempt pool src h) snips
    , lspGoal = \src h -> call $ \pool -> goalAt pool src h
    , lspDiagnostics = \src -> call $ \pool -> mapMaybe diagOf <$> elaborate pool src
    , lspClose = do
        was <- atomicModifyIORef' closed (\b -> (True, b))
        unless was $ do
          p <- stPool <$> readIORef st
          mapM_ (\x -> try @SomeException (closePool x)) p
          _ <- try @SomeException (stopProjectClients s root)
          _ <- try @SomeException (closeSession s)
          pure ()
    , lspTimedOut = pure () }
  where
    -- one call at a time, on a clean scratch document
    call :: (ScratchPool -> IO a) -> IO (Either Text a)
    call act = withMVar lock $ \_ -> safely $ do
      done <- readIORef closed
      when done $ toolError "the Lean server was closed"
      alive <- clientAlive c
      unless alive $ toolError "the Lean server stopped"
      pool <- cleanPool
      modifyIORef' st (\x -> x {stDirty = True})
      a <- act pool
      modifyIORef' st (\x -> x {stDirty = False})
      pure a

    cleanPool = do
      x <- readIORef st
      case stPool x of
        Just p | not (stDirty x) -> pure p
        old -> do
          mapM_ (\p -> try @SomeException (closePool p)) old
          let g = stGen x + 1
          p <- newScratchPool c "" 1 ("_fisher_auto_" <> T.pack (show g))
          writeIORef st (St (Just p) g False Nothing)
          pure p

    -- the diagnostics of a text as it is (the last one is kept)
    elaborate pool src = do
      x <- readIORef st
      case stBase x of
        Just (t, ds) | t == src -> pure ds
        _ -> do
          tr <- runText pool (withNewline src) Nothing Nothing
          let ds = drItems (trDiagnostics tr)
          modifyIORef' st (\y -> y {stBase = Just (src, ds)})
          pure ds

    attempt pool src h sn = case fillHoleAt src h sn of
      Nothing -> toolError ("no sorry at line " <> T.pack (show (hLine h)) <> ", column " <> T.pack (show (hCol h)))
      Just (text, (gl, gc)) -> do
        base <- elaborate pool src
        tr <- runText pool (withNewline text) (Just (gl, gc)) Nothing
        let delta = length (T.splitOn "\n" text) - length (T.splitOn "\n" src)
            first = hLine h - 1
            known = [shift first delta (ident d) | d <- base]
            ds = drItems (trDiagnostics tr)
            kept = [d | d <- ds, localTo first gl d || ident d `notElem` known]
            goals = case trGoal tr of
              Just gr | grStatus gr == "goals" -> grGoals gr
              _ -> []
        pure (Trial sn goals (mapMaybe diagOf kept))

    -- the goal at the hole: at the sorry for a tactic hole; for a term
    -- hole, at a tactic sorry put in its place as Auto would fill it
    goalAt pool src h = do
      let at' = if hTerm h
            then (\(t, (l, col)) -> (t, (l, col - 5))) <$> fillHoleAt src h "sorry"
            else Just (src, (hLine h - 1, hCol h))
      case at' of
        Nothing -> pure Nothing
        Just (text, pos) -> do
          tr <- runText pool (withNewline text) (Just pos) Nothing
          pure $ case trGoal tr of
            Just gr | grStatus gr == "goals", not (null (grGoals gr)) -> Just (T.intercalate "\n\n" (grGoals gr))
            _ -> Nothing

withNewline :: Text -> Text
withNewline t = if "\n" `T.isSuffixOf` t then t else t <> "\n"

-- | Every failure as 'Left' with its text; an asynchronous exception (the
-- caller's timeout) goes on.
safely :: IO a -> IO (Either Text a)
safely act = do
  r <- try act
  case r of
    Right a -> pure (Right a)
    Left e
      | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
      | otherwise -> pure (Left (exceptionText e))

-- | A diagnostic (positions already in code points, 0-based lines) as the
-- search takes it: 1-based line.
diagOf :: PyVal -> Maybe Diag
diagOf d = do
  (l, col) <- start d
  pure (Diag (l + 1) col sev (case get "message" d of Just (PStr m) -> m; _ -> ""))
  where
    sev = case get "severity" d of
      Just (PInt 1) -> SevError
      Just (PInt 2) -> SevWarning
      _ -> SevInfo

start :: PyVal -> Maybe (Int, Int)
start d = do
  r <- rangeOf d
  p <- get "start" r
  (,) <$> intOf (get "line" p) <*> intOf (get "character" p)

endLine :: PyVal -> Maybe Int
endLine d = do
  r <- case get "fullRange" d of
    Just v@(PDict _) -> Just v
    _ -> rangeOf d
  get "end" r >>= intOf . get "line"

rangeOf :: PyVal -> Maybe PyVal
rangeOf d = case get "range" d of
  Just v@(PDict _) -> Just v
  _ -> case get "fullRange" d of
    Just v@(PDict _) -> Just v
    _ -> Nothing

intOf :: Maybe PyVal -> Maybe Int
intOf = \case
  Just (PInt n) -> Just (fromIntegral n)
  _ -> Nothing

-- | Whether a diagnostic touches the 0-based lines from @a@ to @b@.
localTo :: Int -> Int -> PyVal -> Bool
localTo a b d = case start d of
  Just (l, _) -> l <= b && fromMaybe l (endLine d) >= a
  Nothing -> False

type Ident = (Maybe (Int, Int), Maybe Int, Maybe PyVal, Maybe PyVal)

ident :: PyVal -> Ident
ident d = (start d, endLine d, get "severity" d, get "message" d)

-- | A diagnostic of the text without the snippet, moved to where it is
-- with the snippet's extra lines when it starts after the hole's line.
shift :: Int -> Int -> Ident -> Ident
shift first delta k@(st, en, sv, m)
  | delta == 0 = k
  | Just (l, col) <- st, l > first = (Just (l + delta, col), (+ delta) <$> en, sv, m)
  | otherwise = k
