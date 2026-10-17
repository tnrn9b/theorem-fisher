-- | The project file watcher leanclient runs (@watchfiles.awatch(root,
-- debounce=200, step=50)@ filtered to @.lean@ and @.ilean@): recursive,
-- @.lake@ included. Changes are batched as watchfiles does: once one
-- arrives, more are gathered until 50 ms pass with none, or 200 ms since
-- the first; a batch is a set (each (type, path) once).
module Fisher.Lean.Watch
  ( fsWatch
  , Batcher
  , newBatcher
  , pushChange
  , nextBatch
  , changeType
  , watchedFile
  ) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (forever, void)
import Data.List (nub)
import System.FilePath (takeExtension)
import System.FSNotify

-- | watchfiles' Change: 1 added, 2 modified, 3 deleted.
changeType :: Event -> Maybe Int
changeType = \case
  Added {} -> Just 1
  Modified {} -> Just 2
  CloseWrite {} -> Just 2
  Removed {} -> Just 3
  _ -> Nothing

watchedFile :: FilePath -> Bool
watchedFile p = takeExtension p `elem` [".lean", ".ilean"]

newtype Batcher = Batcher (TQueue (Int, FilePath))

newBatcher :: IO Batcher
newBatcher = Batcher <$> newTQueueIO

pushChange :: Batcher -> (Int, FilePath) -> IO ()
pushChange (Batcher q) = atomically . writeTQueue q

-- | One batch: block for the first change, then gather.
nextBatch :: Batcher -> Int -> Int -> IO [(Int, FilePath)]
nextBatch (Batcher q) debounceMs stepMs = do
  first <- atomically (readTQueue q)
  let go acc waited
        | waited >= debounceMs = pure acc
        | otherwise = do
            threadDelay (stepMs * 1000)
            more <- atomically (flushTQueue q)
            if null more then pure acc else go (acc <> more) (waited + stepMs)
  nub <$> go [first] 0

-- | Watch @root@; @onBatch@ gets each batch. Returns the stop action.
fsWatch :: FilePath -> ([(Int, FilePath)] -> IO ()) -> IO (IO ())
fsWatch root onBatch = do
  mgr <- startManager
  b <- newBatcher
  stopListening <-
    watchTree mgr root (\e -> watchedFile (eventPath e) && changeType e /= Nothing) $ \e ->
      mapM_ (\t -> pushChange b (t, eventPath e)) (changeType e)
  worker <- forkIO . forever $ do
    batch <- nextBatch b 200 50
    -- a failing handler must not stop the watcher
    void (try @SomeException (onBatch batch))
  pure $ do
    void (try @SomeException stopListening)
    killThread worker
    void (try @SomeException (stopManager mgr))
