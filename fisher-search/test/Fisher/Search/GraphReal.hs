-- | The graph harness's script on real threads: 'realOrchestrator' over
-- the same 'Script', for the few graph specs that need real concurrency
-- (the simulator runs every job on the caller's thread).
module Fisher.Search.GraphReal
  ( realFor
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (ErrorCall (..), throwIO)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)

import Fisher.Search.GraphHarness (OrchFor)
import Fisher.Search.GraphRun (jobKey)
import Fisher.Search.Orchestrate (Entry (Scripted), ScriptHit (..), realOrchestrator, scriptEntry)
import qualified Fisher.Search.Orchestrate as O

-- | The real orchestrator over the script, read as the simulator reads it
-- ('scriptEntry' over 'jobKey'), given the real milliseconds per virtual
-- second: each job sleeps its seconds so scaled on its own thread, then
-- gives the script's result (or throws its text). The clock is the wall
-- clock. A worker cancelled in its sleep never reaches its entry, so it
-- records none of the harness's bookkeeping (launched, briefs, starts,
-- specs).
realFor :: Double -> OrchFor
realFor msPerSec sc _ = do
  counts <- newIORef (Map.empty :: Map.Map (Text, Text) Int)
  let run job = do
        found <- atomicModifyIORef' counts (\m -> either (\e -> (m, Left e)) (\(hit, m') -> (m', Right hit)) (scriptEntry jobKey sc m job))
        hit <- either (throwIO . ErrorCall . T.unpack) pure found
        threadDelay (round (sdSecs hit * msPerSec * 1000))
        case sdEntry hit of
          Scripted f -> f job >>= either (throwIO . ErrorCall . T.unpack) pure
          O.Run f -> f job
  realOrchestrator run (realToFrac <$> getPOSIXTime) (\_ -> pure ()) (\_ -> pure Nothing)
