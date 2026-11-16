-- | The simulator's event stream is the real orchestrator's: one scenario
-- run through both, the events compared with their times set to 0. The
-- only Scheduler spec on real threads; its sleeps total well under a
-- second.
module Fisher.Search.SchedulerIdentitySpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (ErrorCall (..), throwIO)
import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Search.Check (AxiomPolicy (..), standardAxioms)
import Fisher.Search.FakeLean (fakeLean)
import Fisher.Search.Orchestrate
import Fisher.Search.Scheduler
import Fisher.Search.SimHarness

file :: Text
file = T.unlines
  [ "import Mathlib", ""
  , "theorem a : A := by", "  sorry", ""
  , "theorem b : B := by", "  sorry" ]

-- | Two goals, three attempts of 2, 4 and 60 seconds: a closes at 2, b's
-- first attempt fails at 4, its second (on the other backend) is cut by
-- the budget at 8. Every two events are at least 2 s (100 ms real) apart.
entries :: [ScriptEntry]
entries =
  [ attempt "a" "x" 2 (proofAttempt "ok")
  , attempt "b" "y" 4 (proofAttempt "exact BAD")
  , attempt "b" "x" 60 (proofAttempt "ok") ]

cfg :: SearchConfig
cfg = defaultSearchConfig {scBackends = ["x", "y"], scSlots = 2, scMaxAttempts = 3, scBudget = 8, scParallelPerGoal = False}

-- | Real milliseconds per virtual second.
msPerSec :: Double
msPerSec = 50

-- | The script as a real run function, read as the simulator reads it
-- ('scriptEntry'): each attempt sleeps its seconds scaled to
-- milliseconds, then returns the script's result (or throws its text).
realRun :: Script -> IO (AttemptSpec -> IO AttemptResult)
realRun sc = do
  counts <- newIORef (Map.empty :: Map.Map (Text, Text) Int)
  pure $ \sp -> do
    found <- atomicModifyIORef' counts (\m -> either (\e -> (m, Left e)) (\(hit, m') -> (m', Right hit)) (scriptEntry sc m sp))
    hit <- either (throwIO . ErrorCall . T.unpack) pure found
    threadDelay (round (sdSecs hit * msPerSec * 1000))
    either (throwIO . ErrorCall . T.unpack) pure (sdResult hit sp)

-- | The same search on the real orchestrator, its budget scaled as the
-- sleeps are: the events, oldest first.
realEvents :: Script -> IO [Event]
realEvents sc = withSystemTempDirectory "real" $ \root -> do
  BS.writeFile (root </> "F.lean") (TE.encodeUtf8 file)
  events <- newIORef []
  run <- realRun sc
  orch <- realOrchestrator run (realToFrac <$> getPOSIXTime) (\e -> atomicModifyIORef' events (\es -> (e : es, ()))) (\_ -> pure Nothing)
  let env = SearchEnv
        { seOrch = orch, seLean = fakeLean, sePolicy = AxiomPolicy standardAxioms False, seRoot = root, seRel = "F.lean"
        , seBaseChanged = \_ -> pure ()
        , seAuto = Nothing, seRetrieval = Nothing }
  r <- runSearch cfg {scBudget = scBudget cfg * msPerSec / 1000} env file
  either (fail . T.unpack) (const (reverse <$> readIORef events)) r

-- | An event with every time in it set to 0.
zeroTimes :: Event -> Event
zeroTimes = \case
  Launched _ i g b -> Launched 0 i g b
  Finished _ i g b o st d -> Finished 0 i g b o st d
  GoalClosed _ g i -> GoalClosed 0 g i
  GoalWaiting _ g i -> GoalWaiting 0 g i
  GoalGaveUp _ g -> GoalGaveUp 0 g
  AttemptCancelled _ i g d -> AttemptCancelled 0 i g d
  BudgetSpent _ -> BudgetSpent 0
  AutoClosed _ g s _ -> AutoClosed 0 g s 0
  AutoMissed _ g _ -> AutoMissed 0 g 0
  Repaired _ i g b a c -> Repaired 0 i g b a c
  GoalDisproved _ g k x s w -> GoalDisproved 0 g k x s w
  DisproveRan _ g s r -> DisproveRan 0 g s r
  AutoOff _ why -> AutoOff 0 why
  AutoTimeout _ g s -> AutoTimeout 0 g s
  IndexBuilt _ f d _ -> IndexBuilt 0 f d 0
  IndexSkipped _ f why -> IndexSkipped 0 f why
  Facts _ g n c cut -> Facts 0 g n c cut
  LibraryOffered _ g ls n cut -> LibraryOffered 0 g ls n cut
  LibrarySaved _ l n -> LibrarySaved 0 l n
  LibraryProblem _ why -> LibraryProblem 0 why
  TierChosen _ i g k m -> TierChosen 0 i g k m
  AttemptStopped _ i g why n d -> AttemptStopped 0 i g why n d
  Escalated _ g a b -> Escalated 0 g a b
  TierSkipped _ m why -> TierSkipped 0 m why
  LedgerUnread _ i why -> LedgerUnread 0 i why
  PolicyApplied _ g k d a ev x -> PolicyApplied 0 g k d a ev x

spec :: Spec
spec = describe "the simulator against the real orchestrator" $
  it "gives the same events, times aside, for the same script" $ do
    let sc = emptyScript {scAttempts = entries}
    (_, simEvs, tr) <- simSearch file cfg sc
    realEvs <- realEvents sc
    map zeroTimes realEvs `shouldBe` map zeroTimes simEvs
    -- the scenario is the one meant: a closes, b fails, then the budget
    [(t, i, g, b) | Launched t i g b <- simEvs] `shouldBe` [(0, 1, "a", "x"), (0, 2, "b", "y"), (4, 3, "b", "x")]
    [(t, g) | GoalClosed t g _ <- simEvs] `shouldBe` [(2, "a")]
    [t | BudgetSpent t <- simEvs] `shouldBe` [8]
    stClock tr `shouldBe` 8
