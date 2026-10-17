-- | The loop goldens again, with the session rows written to a real
-- @state.db@ through 'Fisher.Agent.StoreDB' (the in-memory database the
-- other spec uses must agree with sqlite).
module Fisher.Agent.StoreLoopSpec (spec) where

import Control.Monad (forM_)
import Data.IORef
import qualified Data.Text as T
import System.FilePath ((</>))
import Test.Hspec

import Fisher.Agent.LoopSpec (runCaseWith)
import Fisher.Agent.StoreDB
import Fisher.Agent.TestUtil
import Fisher.Store (openStore)

spec :: Spec
spec = do
  cases <- runIO (loadCases "test/golden/b3/loop.json")
  describe "AIAgent.run_conversation over state.db" $
    forM_ cases $ \c -> it (T.unpack (txt "name" c)) $
      runCaseWith (\home -> do
        st <- openStore (home </> "state.db")
        -- a clock that moves, so sessions sort by creation as they do upstream
        tick <- newIORef (1790000000 :: Double)
        let clock = atomicModifyIORef' tick (\t -> (t + 1, t))
        pure (storeSessionDB st clock, storeSnapshot st)) Nothing c
