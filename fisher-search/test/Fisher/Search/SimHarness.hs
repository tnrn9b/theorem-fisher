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
  ) where

import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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
withSimEnv :: SimEnv -> Script -> (FilePath -> SearchEnv -> IO SimTrace -> IO a) -> IO a
withSimEnv se sc k = withSystemTempDirectory "sim" $ \root -> do
  simSetup se root (simFile se)
  ret <- simRetrieval se root
  (o, trace) <- simOrchestrator sc
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
simSearchIn :: SimEnv -> SearchConfig -> Script -> (FilePath -> (SearchReport, [Event], SimTrace) -> IO a) -> IO a
simSearchIn se cfg sc k = withSimEnv se sc $ \root env trace -> do
  r <- runSearch cfg env (simFile se)
  tr <- trace
  rep <- either (fail . T.unpack) pure r
  k root (rep, stEvents tr, tr)

simSearchEnv :: SimEnv -> SearchConfig -> Script -> IO (SearchReport, [Event], SimTrace)
simSearchEnv se cfg sc = simSearchIn se cfg sc (\_ r -> pure r)

-- | A search of this file with nothing but the script.
simSearch :: Text -> SearchConfig -> Script -> IO (SearchReport, [Event], SimTrace)
simSearch file = simSearchEnv (simEnv file)

-- | One script entry: the attempt, its virtual seconds, its result.
type ScriptEntry = (AttemptKey, Double, AttemptSpec -> Either Text AttemptResult)

-- | A script of these entries, unscripted attempts writing @sorry@ at
-- once (as the specs' fake runs did).
script :: [ScriptEntry] -> Script
script es = emptyScript {scAttempts = es, scDefault = Just sorryAttempt}

-- | The first attempt on this goal and backend: it takes these seconds.
attempt :: Text -> Text -> Double -> (AttemptSpec -> Either Text AttemptResult) -> ScriptEntry
attempt g b = attemptN g b 0

-- | The attempt on this goal and backend with this many before it.
attemptN :: Text -> Text -> Int -> Double -> (AttemptSpec -> Either Text AttemptResult) -> ScriptEntry
attemptN g b i secs f = (AttemptKey g b i, secs, f)

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
allNamed :: SimTrace -> Expectation
allNamed tr = stDefaulted tr `shouldBe` []

-- | An attempt that gives up, writing @sorry@.
sorryAttempt :: AttemptSpec -> Either Text AttemptResult
sorryAttempt = proofAttempt "sorry"
