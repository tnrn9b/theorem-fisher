-- | leanclient's @ScratchPool@ (@aio/scratch.py@): virtual documents in the
-- project root, never on disk, opened once with a header and pinned; a
-- trial is a full-text didChange and a barrier, so the user's files are
-- never touched. Free slots are handed out first in, first out.
module Fisher.Lean.Scratch
  ( ScratchPool
  , poolClient
  , newScratchPool
  , TrialResult (..)
  , runText
  , runTexts
  , runTextsEach
  , closePool
  ) where

import Control.Concurrent.Async (mapConcurrently, mapConcurrently_)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (finally, onException, try)
import Control.Monad (forM_, unless)
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Lean.Client
import Fisher.Lean.Error (LeanError)

data ScratchPool = ScratchPool
  { poolClient :: Client
  , spHeader :: Text
  , spPaths :: [Text]
  , spFree :: TQueue Text
  , spWarm :: MVar Bool
  , spTicket :: TVar Int
  -- ^ the next ticket handed out
  , spTurn :: TVar Int
  -- ^ the ticket whose holder takes the next free slot
  }

-- | @ScratchPool(client, header, size, name_prefix)@.
newScratchPool :: Client -> Text -> Int -> Text -> IO ScratchPool
newScratchPool c header size prefix =
  ScratchPool c (T.dropWhileEnd (== '\n') header <> "\n") [prefix <> "_" <> T.pack (show i) <> ".lean" | i <- [0 .. size - 1]]
    <$> newTQueueIO <*> newMVar False <*> newTVarIO 0 <*> newTVarIO 0

warm :: ScratchPool -> IO ()
warm p = modifyMVar_ (spWarm p) $ \done -> do
  unless done $ do
    forM_ (spPaths p) $ \path -> do
      openFile (poolClient p) path (Just (spHeader p)) False "never"
      pinFile (poolClient p) path
    mapConcurrently_ (\path -> barrier (poolClient p) path Nothing) (spPaths p)
    atomically (mapM_ (writeTQueue (spFree p)) (spPaths p))
  pure True

data TrialResult = TrialResult
  { trBody :: Text
  , trDiagnostics :: DiagnosticsReport
  , trGoal :: Maybe GoalResult
  }

-- | @run_text(text, want_goal_at, timeout)@: a whole document in a free
-- slot; diagnostics in document coordinates.
runText :: ScratchPool -> Text -> Maybe (Int, Int) -> Maybe Double -> IO TrialResult
runText p text wantGoal mtimeout = ticket p >>= \t -> runTicket p t text wantGoal mtimeout

-- | A place in line for a slot: callers get slots in the order they took
-- tickets, as asyncio's queue serves its waiters.
ticket :: ScratchPool -> IO Int
ticket p = atomically (stateTVar (spTicket p) (\t -> (t, t + 1)))

runTicket :: ScratchPool -> Int -> Text -> Maybe (Int, Int) -> Maybe Double -> IO TrialResult
runTicket p t text wantGoal mtimeout = do
  -- a failed warm-up gives up its turn so later tickets are not stuck
  warm p `onException` atomically (readTVar (spTurn p) >>= check . (== t) >> writeTVar (spTurn p) (t + 1))
  slot <- atomically $ do
    cur <- readTVar (spTurn p)
    check (cur == t)
    s <- readTQueue (spFree p)
    writeTVar (spTurn p) (t + 1)
    pure s
  let c = poolClient p
  ( do
      updateFile c slot text False
      report <- diagnostics c slot True mtimeout False
      g <- traverse (\(l, col) -> goal c slot l col False) wantGoal
      pure (TrialResult text report g) )
    `finally` atomically (writeTQueue (spFree p) slot)

-- | @run_texts@: concurrently, results in input order.
runTexts :: ScratchPool -> [Text] -> [Maybe (Int, Int)] -> Maybe Double -> IO [TrialResult]
runTexts p texts goals mtimeout = do
  ts <- mapM (const (ticket p)) texts
  mapConcurrently (\(tk, (t, g)) -> runTicket p tk t g mtimeout) (zip ts (zip texts (goals <> repeat Nothing)))

-- | One trial per text, concurrently, each failure kept as its own result
-- (as probes that catch their own errors under @asyncio.gather@).
runTextsEach :: ScratchPool -> [Text] -> Maybe Double -> IO [Either LeanError TrialResult]
runTextsEach p texts mtimeout = do
  ts <- mapM (const (ticket p)) texts
  mapConcurrently (\(tk, t) -> try (runTicket p tk t Nothing mtimeout)) (zip ts texts)

closePool :: ScratchPool -> IO ()
closePool p = do
  forM_ (spPaths p) (closeFile (poolClient p))
  modifyMVar_ (spWarm p) (const (pure False))
  atomically (flushTQueue (spFree p) >> pure ())
