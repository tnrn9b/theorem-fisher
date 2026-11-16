-- | The Scheduler specs' harness over the simulator ('simOrchestrator'):
-- a search in a fresh directory whose attempts follow a 'Script' in
-- virtual seconds, and terse ways to write the script's entries.
module Fisher.Search.SimHarness
  ( SimEnv (..)
  , simEnv
  , withSimEnv
  , simSearchIn
  , simSearchEnv
  , simSearch
  , script
  , ScriptEntry
  , attempt
  , attemptN
  , always
  , inOrder
  , proofAttempt
  , spliceProof
  , sorryAttempt
  , allNamed
  , identityFile
  , identityEntries
  , identityConfig
  , msPerSec
  , realRun
  , scaled
  , realSearch
  , zeroTimes
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (ErrorCall (..), throwIO)
import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Expectation, shouldBe)

import Fisher.Search.Check (AxiomPolicy (..), LeanRun, standardAxioms)
import Fisher.Search.FakeLean (fakeLean)
import Fisher.Search.Lean
import Fisher.Search.Orchestrate
import Fisher.Search.Retrieval (RetrievalEnv)
import Fisher.Search.Scheduler

-- | What a simulated search runs on besides its script.
data SimEnv = SimEnv
  { simFile :: Text
  -- ^ the target file, F.lean
  , simSetup :: FilePath -> Text -> IO ()
  -- ^ writes the project into the fresh root (by default F.lean alone)
  , simLean :: LeanRun
  , simAuto :: Maybe AutoEnv
  , simRetrieval :: FilePath -> IO (Maybe RetrievalEnv)
  -- ^ built in the fresh root
  , simPrice :: Maybe (AttemptSpec -> IO (Maybe Double))
  -- ^ in place of the script's prices
  , simLogHook :: Event -> IO ()
  -- ^ runs after each event is logged
  }

-- | A search of this file with 'fakeLean' and nothing else.
simEnv :: Text -> SimEnv
simEnv file = SimEnv
  { simFile = file
  , simSetup = \root t -> BS.writeFile (root </> "F.lean") (TE.encodeUtf8 t)
  , simLean = fakeLean
  , simAuto = Nothing
  , simRetrieval = \_ -> pure Nothing
  , simPrice = Nothing
  , simLogHook = \_ -> pure ()
  }

-- | The search's env over the simulator in a fresh root, with the root
-- and the trace's reader.
withSimEnv :: SimEnv -> Script AttemptSpec AttemptResult -> (FilePath -> SearchEnv -> IO (SimTrace AttemptSpec) -> IO a) -> IO a
withSimEnv se sc k = withSystemTempDirectory "sim" $ \root -> do
  simSetup se root (simFile se)
  ret <- simRetrieval se root
  (o, trace) <- simOrchestrator attemptKey sc
  let orch = Orchestrator
        { oSpawn = oSpawn o
        , oAwaitAny = oAwaitAny o
        , oCancel = oCancel o
        , oNow = oNow o
        , oLog = \e -> oLog o e >> simLogHook se e
        , oCapped = \name secs act -> oCapped o name secs act
        , oPrice = fromMaybe (oPrice o) (simPrice se)
        }
      env = SearchEnv
        { seOrch = orch, seLean = simLean se, sePolicy = AxiomPolicy standardAxioms False, seRoot = root, seRel = "F.lean"
        , seBaseChanged = \_ -> pure ()
        , seAuto = simAuto se, seRetrieval = ret }
  k root env trace

-- | A search run to its end: the report, the events (oldest first) and
-- the trace, handed on with the root while it still exists.
simSearchIn :: SimEnv -> SearchConfig -> Script AttemptSpec AttemptResult -> (FilePath -> (SearchReport, [Event], SimTrace AttemptSpec) -> IO a) -> IO a
simSearchIn se cfg sc k = withSimEnv se sc $ \root env trace -> do
  r <- runSearch cfg env (simFile se)
  tr <- trace
  rep <- either (fail . T.unpack) pure r
  k root (rep, stEvents tr, tr)

simSearchEnv :: SimEnv -> SearchConfig -> Script AttemptSpec AttemptResult -> IO (SearchReport, [Event], SimTrace AttemptSpec)
simSearchEnv se cfg sc = simSearchIn se cfg sc (\_ r -> pure r)

-- | A search of this file with nothing but the script.
simSearch :: Text -> SearchConfig -> Script AttemptSpec AttemptResult -> IO (SearchReport, [Event], SimTrace AttemptSpec)
simSearch file = simSearchEnv (simEnv file)

-- | One script entry: the attempt, its virtual seconds, its result.
type ScriptEntry = (AttemptKey, Double, Entry AttemptSpec AttemptResult)

-- | A script of these entries, unscripted attempts writing @sorry@ at
-- once (as the specs' fake runs did).
script :: [ScriptEntry] -> Script AttemptSpec AttemptResult
script es = emptyScript {scAttempts = es, scDefault = Just (Scripted (pure . sorryAttempt))}

-- | The first attempt on this goal and backend: it takes these seconds.
attempt :: Text -> Text -> Double -> (AttemptSpec -> Either Text AttemptResult) -> ScriptEntry
attempt g b = attemptN g b 0

-- | The attempt on this goal and backend with this many before it.
attemptN :: Text -> Text -> Int -> Double -> (AttemptSpec -> Either Text AttemptResult) -> ScriptEntry
attemptN g b i secs f = (AttemptKey g b i, secs, Scripted (pure . f))

-- | Every attempt on this goal and backend alike. Only indices 0 to 99
-- are scripted: a hundred and first attempt on the pair falls to the
-- default.
always :: Text -> Text -> Double -> (AttemptSpec -> Either Text AttemptResult) -> [ScriptEntry]
always g b secs f = [attemptN g b i secs f | i <- [0 .. 99]]

-- | A goal's attempts in launch order, each with its backend; the index
-- of each is counted per backend.
inOrder :: Text -> [(Text, Double, AttemptSpec -> Either Text AttemptResult)] -> [ScriptEntry]
inOrder g xs =
  [ attemptN g b (length [() | (b', _, _) <- take n xs, b' == b]) secs f
  | (n, (b, secs, f)) <- zip [0 ..] xs ]

-- | The attempt's start with this proof under the goal's statement.
spliceProof :: Text -> AttemptSpec -> Text
spliceProof proof sp =
  let ls = T.splitOn "\n" (asStart sp)
      d = fromMaybe (error "no goal") (findDecl (asGoal sp) (declarations (asStart sp)))
   in T.intercalate "\n" (take (dStmtEnd d + 1) ls <> ["  " <> proof] <> drop (dEnd d) ls)

-- | An attempt that writes this proof.
proofAttempt :: Text -> AttemptSpec -> Either Text AttemptResult
proofAttempt proof sp = Right (plainResult (Just (spliceProof proof sp)) Nothing "")

-- | No attempt fell to the script's default: for a scenario whose script
-- names every attempt it expects, so a default cannot hide a wrong guess
-- about which goal or backend runs.
allNamed :: SimTrace AttemptSpec -> Expectation
allNamed tr = stDefaulted tr `shouldBe` []

-- | An attempt that gives up, writing @sorry@.
sorryAttempt :: AttemptSpec -> Either Text AttemptResult
sorryAttempt = proofAttempt "sorry"

-- The identity scenario, shared by the identity and record specs, and
-- its run on real threads.

-- | The identity scenario's file: two goals.
identityFile :: Text
identityFile = T.unlines
  [ "import Mathlib", ""
  , "theorem a : A := by", "  sorry", ""
  , "theorem b : B := by", "  sorry" ]

-- | Two goals, three attempts of 2, 4 and 60 seconds: a closes at 2, b's
-- first attempt fails at 4, its second (on the other backend) is cut by
-- the budget at 8. Every two events are at least 2 s (100 ms real) apart.
identityEntries :: [ScriptEntry]
identityEntries =
  [ attempt "a" "x" 2 (proofAttempt "ok")
  , attempt "b" "y" 4 (proofAttempt "exact BAD")
  , attempt "b" "x" 60 (proofAttempt "ok") ]

-- | The identity scenario's configuration, in virtual seconds.
identityConfig :: SearchConfig
identityConfig = defaultSearchConfig {scBackends = ["x", "y"], scSlots = 2, scMaxAttempts = 3, scBudget = 8, scParallelPerGoal = False}

-- | Real milliseconds per virtual second.
msPerSec :: Double
msPerSec = 50

-- | The script as a real run function, read as the simulator reads it
-- ('scriptEntry'): each attempt sleeps its seconds scaled to
-- milliseconds, then returns the script's result (or throws its text).
realRun :: Script AttemptSpec AttemptResult -> IO (AttemptSpec -> IO AttemptResult)
realRun sc = do
  counts <- newIORef (Map.empty :: Map.Map (Text, Text) Int)
  pure $ \sp -> do
    found <- atomicModifyIORef' counts (\m -> either (\e -> (m, Left e)) (\(hit, m') -> (m', Right hit)) (scriptEntry attemptKey sc m sp))
    hit <- either (throwIO . ErrorCall . T.unpack) pure found
    threadDelay (round (sdSecs hit * msPerSec * 1000))
    case sdEntry hit of
      Scripted f -> f sp >>= either (throwIO . ErrorCall . T.unpack) pure
      Run _ -> fail "realRun: a Run entry in the identity script"

-- | A configuration's budget scaled as the real sleeps are, for a run on
-- the real orchestrator.
scaled :: SearchConfig -> SearchConfig
scaled c = c {scBudget = scBudget c * msPerSec / 1000}

-- | The search of this file on the real orchestrator (through the given
-- wrapper), its attempts following the script ('realRun') and its budget
-- scaled ('scaled'): the events, oldest first.
realSearch :: (AttemptOrchestrator -> IO AttemptOrchestrator) -> Text -> SearchConfig -> Script AttemptSpec AttemptResult -> IO [Event]
realSearch wrap file cfg sc = withSystemTempDirectory "real" $ \root -> do
  BS.writeFile (root </> "F.lean") (TE.encodeUtf8 file)
  events <- newIORef []
  run <- realRun sc
  orch <- wrap =<< realOrchestrator run (realToFrac <$> getPOSIXTime) (\e -> atomicModifyIORef' events (\es -> (e : es, ()))) (\_ -> pure Nothing)
  let env = SearchEnv
        { seOrch = orch, seLean = fakeLean, sePolicy = AxiomPolicy standardAxioms False, seRoot = root, seRel = "F.lean"
        , seBaseChanged = \_ -> pure ()
        , seAuto = Nothing, seRetrieval = Nothing }
  r <- runSearch (scaled cfg) env file
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
