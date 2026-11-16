-- | The graph search's events on the simulator are those on the real
-- orchestrator: one scenario run through both, the events compared with
-- their times set to 0. The only graph spec of the harness on real
-- threads; its sleeps total well under a second.
module Fisher.Search.GraphIdentitySpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (ErrorCall (..), throwIO)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Test.Hspec

import Fisher.Search.Scheduler (Event (..))
import Fisher.Search.GraphHarness
import Fisher.Search.GraphRun
import Fisher.Search.Orchestrate (Entry (Scripted), ScriptHit (..), realOrchestrator, scriptEntry)
import qualified Fisher.Search.Orchestrate as O
import Fisher.Search.Scripted (accept, plan)

-- | Two roots: r1's worker fails at 2 s, its split into l is accepted at
-- once, l's worker proves l (and so r1) at 4 s; r2's worker would take
-- 60 s and is cancelled at the budget, 8 s. Every two events of different
-- times are at least 2 s (100 ms real) apart.
runs :: [(Text, [Run])]
runs = [("r1", [bad {rClock = 2}]), ("r2", [ok {rClock = 60}]), ("l", [ok {rClock = 2}])]

replies :: [Reply]
replies = [say Planner (plan [("l", "P")] "theorem r1 : A" "exact use l"), say Verifier accept]

gcfg :: GraphConfig
gcfg = cfg {gcBudget = 8}

-- | Real milliseconds per virtual second.
msPerSec :: Double
msPerSec = 50

-- | The real orchestrator over the script, read as the simulator reads it
-- ('scriptEntry' over 'jobKey'): each job sleeps its seconds scaled to
-- milliseconds, then gives the script's result (or throws its text). The
-- clock is the wall clock.
realFor :: OrchFor
realFor sc _ = do
  counts <- newIORef (Map.empty :: Map.Map (Text, Text) Int)
  let run job = do
        found <- atomicModifyIORef' counts (\m -> either (\e -> (m, Left e)) (\(hit, m') -> (m', Right hit)) (scriptEntry jobKey sc m job))
        hit <- either (throwIO . ErrorCall . T.unpack) pure found
        threadDelay (round (sdSecs hit * msPerSec * 1000))
        case sdEntry hit of
          Scripted f -> f job >>= either (throwIO . ErrorCall . T.unpack) pure
          O.Run f -> f job
  realOrchestrator run (realToFrac <$> getPOSIXTime) (\_ -> pure ()) (\_ -> pure Nothing)

-- | A graph event with every time in it set to 0.
zeroG :: GraphEvent -> GraphEvent
zeroG = \case
  NodeAdded _ i n d -> NodeAdded 0 i n d
  NodeShared _ i s -> NodeShared 0 i s
  NodeReused _ i s -> NodeReused 0 i s
  SplitProposed _ s i -> SplitProposed 0 s i
  SplitRejected _ s why -> SplitRejected 0 s why
  SplitAccepted _ s is -> SplitAccepted 0 s is
  SplitFailed _ s why -> SplitFailed 0 s why
  NodeProved _ i -> NodeProved 0 i
  NodeFailed _ i why -> NodeFailed 0 i why
  FalseLemma _ i c -> FalseLemma 0 i c
  AttemptLaunched _ a i -> AttemptLaunched 0 a i
  AttemptEnded _ a i o -> AttemptEnded 0 a i o
  RoleCost _ r d -> RoleCost 0 r d
  GraphStopped _ why -> GraphStopped 0 why
  NodeAutoClosed _ i s -> NodeAutoClosed 0 i s
  NodeRepaired _ a i b af -> NodeRepaired 0 a i b af
  NodeDisproved _ i k x s m -> NodeDisproved 0 i k x s m
  NodeDisproveRan _ i s r -> NodeDisproveRan 0 i s r
  GraphAutoOff _ why -> GraphAutoOff 0 why
  NodeAutoTimeout _ i s -> NodeAutoTimeout 0 i s
  NodeFacts _ i n c cut -> NodeFacts 0 i n c cut
  NodeLibraryOffered _ i ls n cut -> NodeLibraryOffered 0 i ls n cut
  GraphRunEvent e -> GraphRunEvent (zeroRun e)
  NodeTierChosen _ a i k m -> NodeTierChosen 0 a i k m
  NodeStopped _ a i why n d -> NodeStopped 0 a i why n d
  NodeEscalated _ i a b -> NodeEscalated 0 i a b
  NodeTierSkipped _ m why -> NodeTierSkipped 0 m why
  where
    -- the run-level events a graph run logs
    zeroRun = \case
      IndexBuilt _ f d _ -> IndexBuilt 0 f d 0
      IndexSkipped _ f why -> IndexSkipped 0 f why
      LibrarySaved _ l n -> LibrarySaved 0 l n
      LibraryProblem _ why -> LibraryProblem 0 why
      e -> e

spec :: Spec
spec = describe "the graph search on the simulator against the real orchestrator" $
  it "gives the same events, times aside, for the same script" $ do
    sim <- runG gcfg twoRoots runs replies
    real <- runGBy realFor gcfg {gcBudget = gcBudget gcfg * msPerSec / 1000} twoRoots runs replies
    map zeroG (oEvents real) `shouldBe` map zeroG (oEvents sim)
    -- the scenario is the one meant: a split accepted, a worker cancelled at the budget
    let evs = oEvents sim
    [(t, i) | AttemptLaunched t i _ <- evs] `shouldBe` [(0, 1), (0, 2), (2, 3)]
    [t | SplitAccepted t _ _ <- evs] `shouldBe` [2]
    [(t, i) | NodeProved t i <- evs] `shouldBe` [(4, 3), (4, 1)]
    [(t, i, o) | AttemptEnded t i _ o <- evs, i == 2] `shouldBe` [(8, 2, "cancelled: time")]
    [t | GraphStopped t "time" <- evs] `shouldBe` [8]
    grClosedRoots (oRep sim) `shouldBe` 1
