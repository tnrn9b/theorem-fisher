-- | The replay log round trip: each loop golden is run with the log
-- recording, then the log is replayed and every request must come out
-- byte for byte as logged, every tool call and result as logged.
module Fisher.Agent.ReplaySpec (spec) where

import Control.Monad (forM_, unless)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (LocalTime (..), TimeOfDay (..), fromGregorian)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Agent.AuxSpec (testSys)
import Fisher.Agent.LoopSpec (hasInterrupts, runCaseWith)
import Fisher.Agent.Replay
import Fisher.Agent.Session (memorySessionDB, memorySnapshot)
import Fisher.Agent.TestUtil
import Fisher.LLM.Auth.PyYaml (loadConfigOrdered)
import Fisher.Py.Value

spec :: Spec
spec = do
  b3 <- runIO (loadCases "test/golden/b3/loop.json")
  b4 <- runIO (loadCases "test/golden/b4/loop.json")
  let cases = b3
  describe "replay logs" $
    forM_ [c | c <- b3 <> b4, not (hasInterrupts c)] $ \c -> it (T.unpack (txt "name" c)) $
      withSystemTempDirectory "fisher-replay" $ \tmp -> do
        let home = tmp </> "home"
        createDirectoryIfMissing True (home </> ".gauss")
        -- the replay reads the same config.yaml the run did
        case get "config" (field "scenario" c) of
          Just (PStr y) -> BS.writeFile (home </> ".gauss" </> "config.yaml") (TE.encodeUtf8 y)
          _ -> pure ()
        lg <- openReplayLog tmp "case"
        runCaseWith (\_ -> do (db, m) <- memorySessionDB; pure (db, memorySnapshot m)) (Just lg) c
        closeReplayLog lg
        let sys = testSys home [("HOME", T.pack home)] (\_ -> pure ())
            base = ReplayBase sys (LocalTime (fromGregorian 2026 10 2) (TimeOfDay 4 50 7)) (loadConfigOrdered sys) (const Nothing)
        r <- replayFile base (replayPath tmp "case")
        case r of
          Left e -> expectationFailure (T.unpack e)
          Right rep -> unless (null (rpMismatches rep)) $ expectationFailure (T.unpack (T.unlines (rpMismatches rep)))
  -- a resumed session appends a second process to the same log: each
  -- process replays with its own agent (and its own model-metadata fetch)
  describe "a log written by two processes" $
    it "replays both" $
      withSystemTempDirectory "fisher-replay" $ \tmp -> do
        let home = tmp </> "home"
            plain = [c | c <- cases, not (hasInterrupts c)]
        createDirectoryIfMissing True (home </> ".gauss")
        forM_ (take 2 plain) $ \c -> do
          lg <- openReplayLog tmp "two"
          runCaseWith (\_ -> do (db, m) <- memorySessionDB; pure (db, memorySnapshot m)) (Just lg) c
          closeReplayLog lg
        let sys = testSys home [("HOME", T.pack home)] (\_ -> pure ())
            base = ReplayBase sys (LocalTime (fromGregorian 2026 10 2) (TimeOfDay 4 50 7)) (loadConfigOrdered sys) (const Nothing)
        r <- replayFile base (replayPath tmp "two")
        case r of
          Left e -> expectationFailure (T.unpack e)
          Right rep -> do
            unless (null (rpMismatches rep)) $ expectationFailure (T.unpack (T.unlines (rpMismatches rep)))
            rpTurns rep `shouldSatisfy` (>= 2)
