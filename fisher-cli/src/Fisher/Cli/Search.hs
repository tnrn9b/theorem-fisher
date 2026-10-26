-- | @fisher search ...@: the proof-search swarm (D).
--
-- @fisher search FILE [--backends claude-code,fisher] [--parallel N]
-- [--budget 2h] [--attempt-minutes 20] [--max-turns 40] [--max-attempts 3]
-- [--one-per-goal] [--single BACKEND] [--workflow prove] [--native-decide]
-- [--allow AXIOM]... [--out DIR] [--apply]@ searches the open goals of FILE
-- (a file of a Lake project): attempts in parallel across backends, each
-- in its own copy of the project, each proof checked by the kernel and
-- merged into a shared base. @--single BACKEND@ runs the baseline instead:
-- one child at the whole file for the whole budget. The run directory
-- (default ~/.cache/fisher/search/TIME) holds events.jsonl, report.json,
-- base.lean (the merged file) and each attempt's transcript; @--apply@
-- writes base.lean back over FILE.
--
-- @--graph [--max-depth 3] [--max-splits 2] [--cap-usd 25] [--model M]@
-- runs the lemma-graph search instead ("Fisher.Search.GraphRun"): a goal
-- its attempt cannot close is split into lemmas by a planner model call,
-- reviewed by a verifier call, merged into the base and searched in turn.
-- It runs on the native backend only (fisher), and the planner and verifier calls go to the native agent's provider with
-- @--model@ (default: the configured model). The run directory adds
-- graph.json and a whiteboard per root goal; @--apply@ writes back only
-- when no lemma from a split is left with its sorry.
--
-- Both modes take @[--no-retrieval] [--no-library] [--facts 20]
-- [--facts-chars 8000] [--library-dir DIR]@: before the run the project's
-- declarations are indexed once ("Fisher.Search.Index", cached in the
-- library directory), each goal's brief gets its project facts and the
-- lemmas earlier runs proved, and at the end the run's new lemmas (and in
-- graph mode its notes) go into the library, by default
-- @$FISHER_HOME/fisher/library/<key>@ (@--library-dir DIR@ names the
-- library directory itself; @--library-read-only@ offers from it and
-- saves nothing). @FILE --print-facts@ only prints each open goal's facts
-- and library section, and starts no attempt.
--
-- The compute controller: @[--tiers M1,M2,...] [--no-cascade]
-- [--stall-turns 8] [--stall-dollars 1.50] [--no-early-stop]@. The tiers
-- (the flag, else config @fisher.search.tiers@) are the models a goal's
-- attempts climb, cheapest first; each native attempt runs on its tier's
-- model (its @-m@). The tiers are for native backends only: claude-code and
-- codex attempts run on their own model, take no tier and count in no
-- tier's health. A watcher ("Fisher.Cli.Watch") polls each attempt every 5 s,
-- native attempts from their ledger rows in the Fisher home's @state.db@
-- (@FISHER_UI_LEDGER=1@, @FISHER_UI_TAG=search-<run>-<id>-<attempt>@, the
-- id 8 hex characters fixed once per process, so two runs with one
-- @--out@ name never share a tag), the
-- others from their stream, and stops one that works that many turns (or,
-- native only, dollars) without changing its target (not with
-- @--no-early-stop@, which still prices them). With no tiers set the search
-- is today's, whatever the stall flags say; one tier with
-- @--no-early-stop@, and @--no-cascade@, run one model (the last tier, else
-- @--model@) as today's search.
--
-- The run log (@$FISHER_HOME/fisher/runs.db@, "Fisher.Runlog") gets the
-- search as a run of kind @search@ (id @search-<run directory name>-<run id>@)
-- with every event of events.jsonl, each attempt as a @search-attempt@
-- child (id 'attemptTag', its raw directory the attempt's; a claude-code or
-- codex attempt's stream.jsonl read into its events, a native attempt
-- writing its own under @FISHER_RUN_ID@), each attempt's end with its
-- 'outcomeClass' as soon as the search judges it (on an interrupt, the
-- running ones as cancelled), and one goal row per goal. A run log that cannot be
-- opened or written is one line on stderr; the search goes on. The search's
-- run starts with a @portfolio@ event, @{tactics}@ (the pre-pass's tactics,
-- @[]@ with @--no-auto@ or @--single@, which run no pre-pass), before any
-- auto event.
--
-- The learned policy ("Fisher.Learn.Policy"): at the start of a flat
-- search (not @--single@, not @--graph@) the project's statistics are read
-- from the run log, the policy is computed within the run's backends,
-- tiers, tactics and attempt budget as configured (the flags ignored, so a
-- flagged run neither rewrites the file nor logs a change), written to
-- @$FISHER_HOME/fisher/policy/<project_key>.json@ with each change against
-- the previous file in the log's @policy_changes@, and applied per open goal
-- by its shape: its order of the backends, tiers and tactics, and its
-- attempt budget when below the run's. A flag given (@--backends@,
-- @--tiers@, @--auto-tactics@, @--attempt-minutes@) overrides its knob for
-- every goal. Every eighth logged attempt of a shape explores (the goal's
-- first attempt on its second backend), never with @--backends@.
-- @--no-learn@, or config @fisher.learn.enabled: false@, skips it; so does a
-- run log that cannot be opened, and a previous policy file that cannot be
-- read (@fisher: learn: ignoring <path>: <why>@ on stderr; the fresh policy
-- is still written). The report's @learn@ says which.
--
-- @fisher search check --root R --file F --goal G --agent A [--native]
-- [--allow AXIOM]...@ splices the proof of G from file A (an attempt that
-- started from R/F) into R/F and prints the kernel's verdict as JSON.
module Fisher.Cli.Search
  ( searchMain
  -- * For the specs
  , RunArgs (..)
  , defaultRunArgs
  , parseRun
  , checkGraph
  , withBrief
  , askResult
  , judgeAskResult
  , configuredModel
  , setModel
  , autoConfig
  , serverWanted
  , runUsageText
  , goalTable
  , factsConfig
  , retrievalWanted
  , libraryDirFor
  , buildRetrievalAt
  , termToException
  , exitOnTerminated
  , indexCacheFor
  , Cascade (..)
  , cascadeFor
  , searchTiersConfig
  , modelCommand
  , attemptTag
  , newRunId
  , ledgerEnv
  , attemptWatched
  , attemptDollars
  , searchConfigFor
  , attemptEnv
  , isNative
    -- * The learned policy
  , learnEnabled
  , lessonsEnabled
  , mineSearch
  , liveJudge
  , reportAfterLessons
  , lessonsEnv
  , searchRunId
  , policyDefaults
  , savedDefaults
  , policyShapes
  , lakeRoot
  , searchRunDir
  , goalPolicies
  , withExploration
  , learnOverridden
  , learnPolicy
  , learnApplied
  , portfolioTactics
    -- * The smt arm
  , SmtSetup (..)
  , smtAvailable
  , smtSetup
  , smtSetupWith
  , smtOutput
    -- * The run log
  , SearchLog
  , openSearchLog
  , searchLogOpen
  , searchLogged
  , searchLogEvent
  , searchLogBegin
  , searchLogPortfolio
  , attemptEventData
  , statementOf
  , knownStatement
  , shapeOfKnown
  , searchLogSearchEvent
  , searchLogGraphEvent
  , searchLogInterrupted
  , keepChildEnd
  , finishSearchLog
  , closeSearchLog
  , goalRows
  , eventRow
  , streamTimes
  , attemptRunEnv
  , attemptEnd
  , searchOutcome
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent (myThreadId, throwTo)
import Control.Exception (SomeAsyncException, SomeException, bracket, displayException, finally, fromException, handle, onException, throwIO, try)
import System.Posix.Temp (mkdtemp)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (Handler (Catch), installHandler, sigTERM)
import Control.Monad (forM_, mfilter, unless, void, when)
import Data.Aeson (object, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isSpace)
import Data.Foldable (toList)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types (Pair)
import Data.IORef
import Data.List (isPrefixOf, nub)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import System.Environment (getArgs)
import qualified Data.Text.IO as TIO
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getHomeDirectory, getTemporaryDirectory, makeAbsolute, removeDirectoryRecursive)
import System.Info (os)
import System.Process (proc, readCreateProcessWithExitCode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import System.Exit (exitWith, ExitCode (..))
import System.FilePath (dropTrailingPathSeparator, makeRelative, takeDirectory, takeFileName, (</>))
import System.IO
import Numeric (showFFloat)

import Fisher.Cli.Launch (liveInputs)
import Fisher.Cli.Watch
import Fisher.Cli.Mine (lessonsConfigOn, lessonsEventFrom, lessonsJudgeOn, lessonsWarn, mineEnvFor, withLessons)
import Fisher.Cli.Rlm (liveModelFor)
import Fisher.Cli.SearchLsp (openLsp)
import Fisher.LLM.Auth.Config (loadConfigPy, modelConfig)
import Fisher.LLM.Auth.Sha256 (sha256Hex)
import Fisher.LLM.Normalize (Usage (..))
import Fisher.LLM.Sys (Sys (..))
import Fisher.LLM.SysIO (newSysIO)
import Fisher.Config (getPath)
import Fisher.Paths (fisherDirName, getFisherHome)
import Fisher.Project (InitOptions (..), defaultInitOptions, initializeProject)
import Fisher.Py.Value (PyVal (..), get)
import Fisher.RLM.Types (Model, ModelError (..), Reply (..))
import Fisher.Search.Auto (AutoConfig (..), Lsp (..), defaultAutoConfig, gateTactics)
import Fisher.Search.Check
import Fisher.Search.Disprove (Disproof (..), SmtEnv (..))
import Fisher.Search.Facts (FactsConfig (..), defaultFacts)
import Fisher.Search.Index (Index (..), buildIndex)
import Fisher.Search.Library (libraryDir, libraryKey, loadLibraryReport, peekLibrary)
import Fisher.Search.Retrieval
import Fisher.Search.Child (ChildStats (..), runChild, streamStats)
import Fisher.Agent.UiHooks (runlogGuard, runlogWarn)
import Fisher.Learn.Policy (Defaults (..), GoalPolicy (..), Policy (..), computePolicy, exploreNow, policyDiff, policyFor, policyPath, readPolicy, writePolicy)
import Fisher.Learn.Mine (judgeCostEstimate, mineReportJson, mineRun)
import Fisher.Search.GraphJobs (Ask)
import Fisher.Learn.Stats (Level (..), Stats (..), loadStats, shapeAttempts)
import Fisher.Runlog (Capture (..), GoalRow (..), NewRun (..), RunEnd (..), RunKind (..), RunRow (..), Runlog, closeRunlog, endRun, event, getRun, openRunlog, projectKeyOf, recordPolicyChange, runlogPath, startRun, upsertGoal)
import Fisher.Search.Outcome (outcomeClass)
import Fisher.Search.Shape (renderShape, shapeOf)
import Fisher.Search.Stream (StreamEvent (..), streamEvents)
import Fisher.Search.Controller (StallRule (..), Stop (..), StopReason (..), defaultStallRule, stopReasonText)
import Fisher.Search.Graph (Graph (..), Node (..), NodeId)
import Fisher.Search.GraphRun (AskError (..), GraphConfig (..), GraphEnv (..), GraphEvent (..), GraphReport (..), plannerModel, runGraph)
import Fisher.Search.Headless
import Fisher.Search.Lean (Decl (..), declarations, openGoals)
import Fisher.Search.Report
import Fisher.Search.Scheduler hiding (GoalStatus (..))
import qualified Fisher.Search.Scheduler as S
import Fisher.Search.Splice (SpliceError, applySmtImport, smtKept)
import Fisher.Smt (Solver, SmtConfig (..), findSolver, runQuery, smtConfig, solverName)
import Fisher.Smt.Lake (HeaderKind (..), hasSmt, headerKind, insertImportSmt)
import Fisher.Search.Verify
import Fisher.Search.Workspace (makeWorkspace)
import Fisher.Workflow.Handoff (HandoffRequest (..))
import Fisher.Workflow.Plan (LaunchInputs (..), WorkflowPlan (..), promptLessonsFor, resolveWorkflowRequest)
import Fisher.Workflow.Types (Env, Machine (..), errorMessage)

readUtf8 :: FilePath -> IO T.Text
readUtf8 p = TE.decodeUtf8With TEE.lenientDecode <$> BS.readFile p

usage :: IO a
usage = do
  hPutStrLn stderr "usage: fisher search check --root ROOT --file FILE --goal NAME --agent AGENT_FILE [--native] [--allow AXIOM]..."
  exitWith (ExitFailure 2)

data CheckArgs = CheckArgs
  { caRoot :: Maybe FilePath, caFile :: Maybe FilePath, caGoal :: Maybe T.Text, caAgent :: Maybe FilePath
  , caNative :: Bool, caAllow :: [T.Text], caTimeout :: Int }

parseCheck :: [String] -> CheckArgs -> Maybe CheckArgs
parseCheck args a = case args of
  [] -> Just a
  "--root" : v : r -> parseCheck r a {caRoot = Just v}
  "--file" : v : r -> parseCheck r a {caFile = Just v}
  "--goal" : v : r -> parseCheck r a {caGoal = Just (T.pack v)}
  "--agent" : v : r -> parseCheck r a {caAgent = Just v}
  "--native" : r -> parseCheck r a {caNative = True}
  "--allow" : v : r -> parseCheck r a {caAllow = T.pack v : caAllow a}
  "--timeout" : v : r | [(n, "")] <- reads v -> parseCheck r a {caTimeout = n}
  _ -> Nothing

searchMain :: [String] -> IO ()
searchMain = \case
  "check" : rest -> checkMain rest
  args -> case parseRun args defaultRunArgs of
    Just ra0 | Just f <- raFile ra0 -> case checkGraph ra0 of
      Left e -> hPutStrLn stderr (T.unpack e) >> exitWith (ExitFailure 2)
      Right ra
        | raPrintFacts ra -> printFactsMain ra f
        | otherwise -> exitOnTerminated (runMain ra f)
    _ -> runUsage

checkMain :: [String] -> IO ()
checkMain rest = case parseCheck rest (CheckArgs Nothing Nothing Nothing Nothing False [] 900) of
    Just (CheckArgs (Just root) (Just file) (Just goal) (Just agentPath) native allow secs) -> do
      start <- readUtf8 (root </> file)
      agent <- readUtf8 agentPath
      let lr = lakeLeanRun secs
          pol = AxiomPolicy (Set.union standardAxioms (Set.fromList allow)) native
      types <- goalTypes lr root file start [goal]
      case types of
        Left e -> emit (object ["verdict" .= ("error" :: T.Text), "reason" .= e]) >> exitWith (ExitFailure 1)
        Right ts -> do
          o <- verifyAttempt lr pol root file start agent start goal (Map.findWithDefault "" goal ts)
          emit (outcomeJson o)
          exitWith (case o of Judged Closed _ _ -> ExitSuccess; _ -> ExitFailure 1)
    _ -> usage
  where
    emit = BL8.putStrLn . A.encode

outcomeJson :: Outcome -> A.Value
outcomeJson = \case
  NotSpliced e -> object ["verdict" .= ("not-spliced" :: T.Text), "reason" .= spliceReason e]
  Judged v _ helpers -> object (["verdict" .= verdictName v, "helpers" .= helpers] <> case v of
    Rejected why -> ["reason" .= why]
    _ -> [])

verdictName :: Verdict -> T.Text
verdictName = \case
  Closed -> "closed"
  Waiting -> "waiting"
  Rejected _ -> "rejected"

spliceReason :: SpliceError -> T.Text
spliceReason = T.pack . show

-- ------------------------------------------------------------- the run

data RunArgs = RunArgs
  { raFile :: Maybe FilePath
  , raBackends :: [T.Text]
  , raSlots :: Int
  , raBudget :: Double
  , raAttemptSecs :: Int
  , raMaxTurns :: Int
  , raMaxAttempts :: Int
  , raParallelPerGoal :: Bool
  , raSingle :: Maybe T.Text
  , raWorkflow :: T.Text
  , raNative :: Bool
  , raAllow :: [T.Text]
  , raOut :: Maybe FilePath
  , raApply :: Bool
  , raBackendsGiven :: Bool
  -- ^ @--backends@ was on the command line
  , raGraph :: Bool
  , raMaxDepth :: Int
  , raMaxSplits :: Int
  , raCapUsd :: Double
  , raModel :: Maybe T.Text
  , raGraphFlags :: Bool
  -- ^ one of the four flags above was on the command line
  , raAuto :: Bool
  -- ^ the automation before the models (@--no-auto@ turns it off)
  , raRepair :: Bool
  , raDisprove :: Bool
  , raAutoTactics :: Maybe [T.Text]
  , raAutoSeconds :: Maybe Double
  , raRepairSeconds :: Maybe Double
  , raAutoWarmup :: Double
  -- ^ seconds the language server's warm-up may take
  , raRetrieval :: Bool
  -- ^ project facts in the briefs (@--no-retrieval@ turns them off)
  , raLibrary :: Bool
  -- ^ the lemma library (@--no-library@ turns it off)
  , raFacts :: Int
  , raFactsChars :: Int
  , raLibraryDir :: Maybe FilePath
  -- ^ the library directory itself, in place of the per-project default
  , raLibraryReadOnly :: Bool
  -- ^ offer from the library, never save to it
  , raPrintFacts :: Bool
  -- ^ print each open goal's retrieval and stop
  , raTiers :: Maybe [T.Text]
  -- ^ @--tiers@: the model tiers, cheapest first (else the config's)
  , raCascade :: Bool
  -- ^ @--no-cascade@ turns it off: one model, early stop off
  , raStallTurns :: Int
  , raStallDollars :: Double
  , raEarlyStop :: Bool
  -- ^ @--no-early-stop@ turns it off
  , raAttemptSecsGiven :: Bool
  -- ^ @--attempt-minutes@ was on the command line
  , raLearn :: Bool
  -- ^ the learned policy (@--no-learn@ turns it off)
  , raLessons :: Bool
  -- ^ the lessons: mining after the run and the block in the prompts
  -- (@--no-lessons@ turns them off)
  , raSmt :: Bool
  -- ^ the @smt@ arm is available (no flag: 'smtSetup' sets it from the
  -- project and the config); False drops it from the pre-pass
  }
  deriving (Eq, Show)

defaultRunArgs :: RunArgs
defaultRunArgs = RunArgs Nothing ["claude-code", nativeBackend] 4 7200 1200 40 3 True Nothing "prove" False [] Nothing False
                         False False 3 2 25 Nothing False True True True Nothing Nothing Nothing 600
                         True True (fcMax defaultFacts) (fcChars defaultFacts) Nothing False False
                         Nothing True (srTurns defaultStallRule) (fromMaybe 1.5 (srDollars defaultStallRule)) True
                         False True True True

runUsage :: IO a
runUsage = do
  hPutStrLn stderr runUsageText
  exitWith (ExitFailure 2)

runUsageText :: String
runUsageText = unlines
    [ "usage: fisher search FILE [--backends claude-code,fisher,codex] [--parallel N] [--budget 2h]"
    , "                          [--attempt-minutes 20] [--max-turns 40] [--max-attempts 3] [--one-per-goal]"
    , "                          [--single BACKEND] [--workflow prove|autoprove] [--native-decide] [--allow AXIOM]..."
    , "                          [--out DIR] [--apply] [--model MODEL]"
    , "                          [--tiers M1,M2,...] [--no-cascade] [--stall-turns 8] [--stall-dollars 1.50]"
    , "                          [--no-early-stop] [--no-learn]"
    , "       fisher search FILE --graph [--max-depth 3] [--max-splits 2] [--cap-usd 25] [--model MODEL]"
    , "                          [--backends fisher] [--parallel N] [--budget 2h] [--attempt-minutes 20]"
    , "                          [--max-turns 40] [--native-decide] [--allow AXIOM]... [--out DIR] [--apply]"
    , "                          [--tiers M1,M2,...] [--no-cascade] [--stall-turns 8] [--stall-dollars 1.50]"
    , "                          [--no-early-stop]"
    , "       both take [--auto | --no-auto] [--no-repair] [--no-disprove] [--auto-tactics A,B,...] [--auto-seconds 60]"
    , "                 [--repair-seconds 120] [--auto-warmup-seconds 600]"
    , "                 [--no-retrieval] [--no-library] [--facts N] [--facts-chars N] [--library-dir DIR]"
    , "                 [--library-read-only] [--no-lessons]"
    , "       fisher search FILE --print-facts [--facts N] [--facts-chars N] [--no-library] [--library-dir DIR]"
    , "       fisher search check --root ROOT --file FILE --goal NAME --agent AGENT_FILE [--native] [--allow AXIOM]..." ]

-- | "2h", "90m", "300s" or plain seconds.
duration :: String -> Maybe Double
duration v = case reads v of
  [(n, "")] -> Just n
  [(n, "h")] -> Just (n * 3600)
  [(n, "m")] -> Just (n * 60)
  [(n, "s")] -> Just n
  _ -> Nothing

parseRun :: [String] -> RunArgs -> Maybe RunArgs
parseRun args a = case args of
  [] -> Just a
  "--backends" : v : r -> parseRun r a {raBackends = map T.strip (T.splitOn "," (T.pack v)), raBackendsGiven = True}
  "--parallel" : v : r | [(n, "")] <- reads v, n > 0 -> parseRun r a {raSlots = n}
  "--budget" : v : r | Just d <- duration v -> parseRun r a {raBudget = d}
  "--attempt-minutes" : v : r | Just d <- duration (v <> "m") -> parseRun r a {raAttemptSecs = round d, raAttemptSecsGiven = True}
  "--max-turns" : v : r | [(n, "")] <- reads v -> parseRun r a {raMaxTurns = n}
  "--max-attempts" : v : r | [(n, "")] <- reads v -> parseRun r a {raMaxAttempts = n}
  "--one-per-goal" : r -> parseRun r a {raParallelPerGoal = False}
  "--single" : v : r -> parseRun r a {raSingle = Just (T.pack v)}
  "--workflow" : v : r | v `elem` ["prove", "autoprove"] -> parseRun r a {raWorkflow = T.pack v}
  "--native-decide" : r -> parseRun r a {raNative = True}
  "--allow" : v : r -> parseRun r a {raAllow = T.pack v : raAllow a}
  "--out" : v : r -> parseRun r a {raOut = Just v}
  "--apply" : r -> parseRun r a {raApply = True}
  "--graph" : r -> parseRun r a {raGraph = True}
  "--max-depth" : v : r | [(n, "")] <- reads v, n >= 0 -> parseRun r a {raMaxDepth = n, raGraphFlags = True}
  "--max-splits" : v : r | [(n, "")] <- reads v, n >= 0 -> parseRun r a {raMaxSplits = n, raGraphFlags = True}
  "--cap-usd" : v : r | [(x, "")] <- reads v, x > 0 -> parseRun r a {raCapUsd = x, raGraphFlags = True}
  "--model" : v : r | not (null v) -> parseRun r a {raModel = Just (T.pack v)}
  "--auto" : r -> parseRun r a {raAuto = True}
  "--no-auto" : r -> parseRun r a {raAuto = False}
  "--no-repair" : r -> parseRun r a {raRepair = False}
  "--no-disprove" : r -> parseRun r a {raDisprove = False}
  "--auto-tactics" : v : r
    | let ts = map T.strip (T.splitOn "," (T.pack v)), not (any T.null ts) -> parseRun r a {raAutoTactics = Just ts}
  "--auto-seconds" : v : r | [(x, "")] <- reads v, x > 0 -> parseRun r a {raAutoSeconds = Just x}
  "--repair-seconds" : v : r | [(x, "")] <- reads v, x > 0 -> parseRun r a {raRepairSeconds = Just x}
  "--auto-warmup-seconds" : v : r | [(x, "")] <- reads v, x > 0 -> parseRun r a {raAutoWarmup = x}
  "--no-retrieval" : r -> parseRun r a {raRetrieval = False}
  "--no-library" : r -> parseRun r a {raLibrary = False}
  "--facts" : v : r | [(n, "")] <- reads v, n > 0 -> parseRun r a {raFacts = n}
  "--facts-chars" : v : r | [(n, "")] <- reads v, n > 0 -> parseRun r a {raFactsChars = n}
  "--library-dir" : v : r | not (null v) -> parseRun r a {raLibraryDir = Just v}
  "--library-read-only" : r -> parseRun r a {raLibraryReadOnly = True}
  "--print-facts" : r -> parseRun r a {raPrintFacts = True}
  "--tiers" : v : r
    | let ts = map T.strip (T.splitOn "," (T.pack v)), not (any (\t -> T.null t || T.any isSpace t) ts) -> parseRun r a {raTiers = Just ts}
  "--no-cascade" : r -> parseRun r a {raCascade = False}
  "--stall-turns" : v : r | [(n, "")] <- reads v, n > 0 -> parseRun r a {raStallTurns = n}
  "--stall-dollars" : v : r | [(x, "")] <- reads v, x > 0 -> parseRun r a {raStallDollars = x}
  "--no-early-stop" : r -> parseRun r a {raEarlyStop = False}
  "--no-learn" : r -> parseRun r a {raLearn = False}
  "--no-lessons" : r -> parseRun r a {raLessons = False}
  f : r | not ("--" `isPrefixOf` f), raFile a == Nothing -> parseRun r a {raFile = Just f}
  _ -> Nothing

-- | The run's models and whether the compute controller is on: the tiers
-- (the flag, else the config's), the single model the backends get when no
-- tier names one (native only, as @--model@ is today), the controller, and
-- early stop. The controller is on with two tiers or more, or one tier
-- and early stop on, and never with @--no-cascade@; two tiers or more with
-- @--no-early-stop@ keep it on (escalation, the ledger priced) with no
-- attempt stopped. No tiers is today's search whatever the stall flags
-- say; one tier with @--no-early-stop@, and @--no-cascade@, are today's
-- search on one model (the last tier, else @--model@).
data Cascade = Cascade
  { cTiers :: [T.Text]
  , cModel :: Maybe T.Text
  , cControl :: Bool
  , cEarlyStop :: Bool
  }
  deriving (Eq, Show)

cascadeFor :: RunArgs -> [T.Text] -> Cascade
cascadeFor ra configTiers
  | raCascade ra && (length tiers >= 2 || (length tiers == 1 && raEarlyStop ra)) = Cascade tiers (raModel ra) True (raEarlyStop ra)
  | otherwise = Cascade [] (lastTier <|> raModel ra) False False
  where
    tiers = fromMaybe configTiers (raTiers ra)
    lastTier = if null tiers then Nothing else Just (last tiers)

-- | @fisher.search.tiers@ in the config: a list of model names (blank
-- ones dropped); anything else is no tiers.
searchTiersConfig :: A.Value -> [T.Text]
searchTiersConfig cfg = case getPath ["fisher", "search", "tiers"] cfg of
  Just (A.Array xs) -> [T.strip t | A.String t <- toList xs, not (T.null (T.strip t))]
  _ -> []

-- | The workflow command for a stream backend's attempt, with its tier's
-- model when it has one.
modelCommand :: Maybe T.Text -> T.Text -> T.Text
modelCommand m cmd = maybe cmd (\x -> cmd <> " --model " <> x) m

-- | A native attempt's @FISHER_UI_TAG@: @search-<run directory's
-- name>-<run id>-<attempt>@. The run id ('newRunId') is the process's, so
-- runs that share an @--out@ name (bench items, the probe's goals) never
-- share a tag in the one @state.db@.
attemptTag :: FilePath -> T.Text -> Int -> T.Text
attemptTag runDir runId n = searchRunId runDir runId <> "-" <> T.pack (show n)

-- | The search's run id in the run log: @search-<run directory's
-- name>-<run id>@; each attempt's is it, a dash and the attempt's number
-- ('attemptTag'), which the miner reads back ("Fisher.Learn.Mine").
searchRunId :: FilePath -> T.Text -> T.Text
searchRunId runDir runId = "search-" <> T.pack (takeFileName (dropTrailingPathSeparator runDir)) <> "-" <> runId

-- | The run's id for its ledger tags: 8 hex characters of a hash of the
-- process id and the clock, taken once at the run's start.
newRunId :: IO T.Text
newRunId = do
  pid <- getProcessID
  t <- getPOSIXTime
  pure (T.take 8 (sha256Hex (TE.encodeUtf8 (T.pack (show pid <> " " <> show t)))))

-- | The attempt's environment with the ledger on and its tag, when the
-- controller watches it from the ledger; else as it is.
ledgerEnv :: Bool -> T.Text -> Env -> Env
ledgerEnv on tag env
  | on = Map.insert "FISHER_UI_LEDGER" "1" (Map.insert "FISHER_UI_TAG" (T.unpack tag) env)
  | otherwise = env

-- | Whether an attempt at the goal is watched: the controller on, and not
-- the @--single@ baseline (goal @*@).
attemptWatched :: Cascade -> T.Text -> Bool
attemptWatched c goal = cControl c && goal /= "*"

-- | A watched attempt's dollars: its ledger's; once the ledger was given
-- up for the stream, the larger of the ledger's partial sum and the
-- stream's @cost_usd@ (either one when only it is known).
attemptDollars :: Watched -> Maybe ChildStats -> Maybe Double
attemptDollars w stats = case wUnread w of
  Nothing -> wDollars w
  Just _ -> case (wDollars w, stats >>= csCostUsd) of
    (Just a, Just b) -> Just (max a b)
    (a, b) -> a <|> b

-- | The flat search's settings.
searchConfigFor :: RunArgs -> Cascade -> SearchConfig
searchConfigFor ra c = SearchConfig
  { scBackends = raBackends ra, scSlots = raSlots ra, scMaxAttempts = raMaxAttempts ra, scBudget = raBudget ra
  , scParallelPerGoal = raParallelPerGoal ra, scTiers = cTiers c, scControl = cControl c
  , scUntiered = filter (not . isNative) (raBackends ra)
  , scGoalPolicy = Map.empty, scPolicyLogged = True, scAttemptSecs = Just (raAttemptSecs ra) }

-- | The automation's settings, or Nothing with @--no-auto@ (today's search).
autoConfig :: RunArgs -> Maybe AutoConfig
autoConfig ra
  | not (raAuto ra) = Nothing
  | otherwise =
      Just defaultAutoConfig
        { acTactics = gateTactics (raSmt ra) (fromMaybe (acTactics defaultAutoConfig) (raAutoTactics ra))
        , acSeconds = fromMaybe (acSeconds defaultAutoConfig) (raAutoSeconds ra)
        , acRepair = raRepair ra
        , acDisprove = raDisprove ra
        , acRepairSeconds = fromMaybe (acRepairSeconds defaultAutoConfig) (raRepairSeconds ra) }

-- ------------------------------------------------------------- the learned policy

-- | Whether the run uses the learned policy: not @--no-learn@, and config
-- @fisher.learn.enabled@ not @false@.
learnEnabled :: RunArgs -> A.Value -> Bool
learnEnabled ra cfg = raLearn ra && getPath ["fisher", "learn", "enabled"] cfg /= Just (A.Bool False)

-- | Whether the run has lessons (mining after it, the @lessons@ event, the
-- block in the attempts' prompts): not @--no-lessons@, and config
-- @fisher.lessons.enabled@ not @false@.
lessonsEnabled :: RunArgs -> A.Value -> Bool
lessonsEnabled ra cfg = raLessons ra && lessonsConfigOn cfg

-- | An attempt's environment: with lessons off, @FISHER_LESSONS=0@, which
-- the prompt writers read to leave the lessons block out.
lessonsEnv :: Bool -> Env -> Env
lessonsEnv on = if on then id else Map.insert "FISHER_LESSONS" "0"

-- | The policy's bounds: the run's backends, tiers, pre-pass tactics
-- (none with @--no-auto@) and attempt budget.
policyDefaults :: RunArgs -> Cascade -> Maybe AutoConfig -> Defaults
policyDefaults ra casc ac = Defaults (raBackends ra) (cTiers casc) (maybe [] acTactics ac) (raAttemptSecs ra)

-- | The saved policy's bounds: the unflagged configuration's
-- ('defaultRunArgs' with the config's tiers), so the policy file never
-- depends on a run's flags.
savedDefaults :: A.Value -> Defaults
savedDefaults cfg = policyDefaults unflagged (cascadeFor unflagged (searchTiersConfig cfg)) (autoConfig unflagged)
  where
    unflagged = defaultRunArgs

-- | The shapes a policy is computed over: those of the given statements,
-- the project's shapes in the stats and the previous policy's (so a
-- recompute keeps the shapes it had and 'policyDiff' reports none as
-- removed).
policyShapes :: [T.Text] -> Stats -> Maybe Policy -> [T.Text]
policyShapes stmts st prev =
  nub ( map (renderShape . shapeOf) stmts
          <> [sh | (LShape, sh, _, _) <- Map.keys (stCells st)]
          <> maybe [] (Map.keys . pShapes) prev )

-- | Each goal's entry (goal, statement), by its shape: within the run's
-- backends and tactics, a budget only below the run's, and each knob
-- whose flag was given set to the flag's value ('learnOverridden'): the
-- backends to @--backends@, the tiers to @--tiers@, the tactics to
-- @--auto-tactics@, and no learned budget with @--attempt-minutes@.
goalPolicies :: RunArgs -> Policy -> [(T.Text, T.Text)] -> Map.Map T.Text GoalPolicy
goalPolicies ra pol goals = Map.fromList [(g, override (policyFor pol (renderShape (shapeOf stmt)))) | (g, stmt) <- goals]
  where
    tactics = maybe [] acTactics (autoConfig ra)
    override gp = gp
      { gpBackends = if raBackendsGiven ra then raBackends ra else filter (`elem` raBackends ra) (gpBackends gp)
      , gpTiers = fromMaybe (gpTiers gp) (raTiers ra)
      , gpTactics = fromMaybe (filter (`elem` tactics) (gpTactics gp)) (raAutoTactics ra)
      , gpAttemptSecs = if raAttemptSecsGiven ra then Nothing else mfilter (< raAttemptSecs ra) (gpAttemptSecs gp)
      , gpExplore = gpExplore gp && not (raBackendsGiven ra) }

-- | Each goal's exploration ('exploreNow' of the attempts logged for its
-- shape in the project), with two backends or more and never with
-- @--backends@.
withExploration :: RunArgs -> Stats -> [(T.Text, T.Text)] -> Map.Map T.Text GoalPolicy -> Map.Map T.Text GoalPolicy
withExploration ra st goals = Map.mapWithKey explore
  where
    can = not (raBackendsGiven ra) && length (raBackends ra) >= 2
    explore g gp =
      gp {gpExplore = can && exploreNow (shapeAttempts st (renderShape (shapeOf (statementOf goals g))))}

-- | The knobs whose flags were given, which the policy leaves alone.
learnOverridden :: RunArgs -> [T.Text]
learnOverridden ra =
  [ k | (k, True) <- [("backends", raBackendsGiven ra), ("tiers", isJust (raTiers ra)), ("tactics", isJust (raAutoTactics ra))
                     , ("attempt_secs", raAttemptSecsGiven ra)] ]

-- | The learned policy for a run, given the config, the Fisher home, the
-- project root, the search's log and the open goals (name, statement):
-- the per-goal map for the scheduler and the report's @learn@. When it
-- runs ('learnEnabled', a flat search, the log open) it computes the
-- policy from the log's statistics over the goals' shapes, the shapes the
-- log has seen and the previous file's, writes it, and records each change
-- against the previous file. The saved policy's bounds are the unflagged
-- configuration's ('defaultRunArgs' with the config's tiers): the flags
-- shape only the map the run applies ('goalPolicies'), so a flagged run
-- between two plain ones records no change. A previous file that cannot
-- be read is ignored (one line on stderr): the fresh policy is written but
-- the run takes the defaults. Off, or on any failure (one line on stderr),
-- the map is empty (the defaults) and @learn@ says why; @changes@ counts
-- the change rows actually recorded.
learnPolicy :: RunArgs -> A.Value -> FilePath -> FilePath -> SearchLog -> [(T.Text, T.Text)]
            -> IO (Map.Map T.Text GoalPolicy, A.Value)
learnPolicy ra cfg home root sl goals
  | not (raLearn ra) = off "--no-learn"
  | not (learnEnabled ra cfg) = off "fisher.learn.enabled is false"
  | isJust (raSingle ra) = off "--single runs no policy"
  | raGraph ra = off "the lemma graph runs no policy"
  | otherwise = case slRunlog sl of
      Nothing -> off "the run log could not be opened"
      Just rl -> do
        r <- try @SomeException $ do
          st <- loadStats rl key
          prev <- readPolicy path
          at <- clockNow
          let shapes = policyShapes (map snd goals) st (either (const Nothing) id prev)
              policy = computePolicy at key (savedDefaults cfg) st shapes
              changes = policyDiff (either (const Nothing) id prev) policy
          writePolicy path policy
          recorded <- newIORef (0 :: Int)
          searchLogged sl $ \rl' ->
            forM_ changes $ \(sh, knob, o, n) -> do
              recordPolicyChange rl' at key sh knob o n (evidenceOf policy sh knob)
              modifyIORef' recorded (+ 1)
          n <- readIORef recorded
          pure (st, prev, policy, n)
        case r of
          Left e
            | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
            | otherwise -> do
                let why = "the policy could not be computed: " <> T.take 300 (T.pack (displayException e))
                hPutStrLn stderr ("fisher: learn: " <> T.unpack why)
                off why
          Right (_, Left why, _, n) -> do
            hPutStrLn stderr ("fisher: learn: ignoring " <> T.unpack why)
            pure (Map.empty, object ["enabled" .= False, "reason" .= ("ignoring " <> why), "policy_file" .= path, "changes" .= n])
          Right (st, Right _, policy, n) -> do
            let m = withExploration ra st goals (goalPolicies ra policy goals)
                sc = (searchConfigFor ra (cascadeFor ra (searchTiersConfig cfg))) {scGoalPolicy = m}
            pure
              ( m
              , object
                  [ "enabled" .= True, "policy_file" .= path, "shapes" .= Map.size (Map.delete "*" (pShapes policy))
                  , "overridden" .= learnOverridden ra, "exploration" .= Map.keys (Map.filter gpExplore m), "changes" .= n
                  , "applied" .= learnApplied sc (maybe [] acTactics (autoConfig ra)) m ] )
  where
    key = projectKeyOf root
    path = policyPath home key
    off why = pure (Map.empty, object ["enabled" .= False, "reason" .= (why :: T.Text)])
    -- a change's evidence: the knob's part of the shape's, else all of it
    evidenceOf policy sh knob = case gpEvidence <$> Map.lookup sh (pShapes policy) of
      Just (A.Object o) | Just v <- KM.lookup (K.fromText knob) o -> v
      Just v -> v
      Nothing -> A.Null

-- | The report's per-goal @applied@: for each goal the policy moves, the
-- knobs it moves off the configured ones (the run's search config, its
-- pre-pass tactics) and their values as the scheduler applies them:
-- @{goal: {backends?, tiers?, tactics?, attempt_secs?}}@.
learnApplied :: SearchConfig -> [T.Text] -> Map.Map T.Text GoalPolicy -> A.Value
learnApplied sc tactics m =
  object [K.fromText g .= object kvs | (g, gp) <- Map.toList m, let kvs = knobs g gp, not (null kvs)]
  where
    knobs g gp =
      [ "backends" .= bs | let bs = goalBackends sc' g, bs /= scBackends sc ]
        <> [ "tiers" .= ts | let ts = map (scTiers sc !!) (goalTiers sc' g), ts /= scTiers sc ]
        <> [ "tactics" .= xs | let xs = goalTactics sc' g tactics, xs /= tactics ]
        <> [ "attempt_secs" .= secs | Just secs <- [gpAttemptSecs gp], maybe True (secs <) (scAttemptSecs sc) ]
      where
        sc' = sc {scGoalPolicy = Map.singleton g gp}

-- | The @portfolio@ event's tactics: the pre-pass's, none with
-- @--no-auto@ or @--single@ (which runs no pre-pass).
portfolioTactics :: RunArgs -> [T.Text]
portfolioTactics ra
  | isJust (raSingle ra) = []
  | otherwise = maybe [] acTactics (autoConfig ra)

-- | Whether the @smt@ arm is available to a search in this project: its
-- @lake-manifest.json@ lists lean-smt and @fisher.smt.enabled@ is not
-- false.
smtAvailable :: A.Value -> FilePath -> IO Bool
smtAvailable cfg root = (smtcEnabled (smtConfig cfg) &&) <$> hasSmt root

-- | What the smt arm changes in a search, decided once at its start.
data SmtSetup = SmtSetup
  { ssAvailable :: Bool
  , ssArgs :: RunArgs
  -- ^ the run's arguments with 'raSmt' set and a user @--auto-tactics@
  -- list gated ('gateTactics'), so the pre-pass, the portfolio event and
  -- the learned policy all see the same tactics
  , ssBase :: T.Text
  -- ^ the base text: with the @import Smt@ marker line when the arm is
  -- available and the file does not import Smt
  , ssInserted :: Bool
  -- ^ the marker went in, so the written file keeps it only when a kept
  -- proof uses @smt@ ('smtOutput')
  , ssWarning :: Maybe T.Text
  -- ^ for stderr, when a user list lost @smt@ (and, when that left it
  -- empty, the automation is off)
  , ssFacts :: T.Text
  -- ^ the briefs' smt paragraph ('smtFacts'); empty when not available
  , ssSmt :: Maybe SmtEnv
  -- ^ Disprove's SMT source: Nothing when the arm is not available or no
  -- solver is on PATH
  , ssSolverWarning :: Maybe T.Text
  -- ^ for stderr, once per run: lean-smt is there but no solver, so the
  -- SMT countermodels are off (only when Disprove would run)
  }

-- | The smt arm's setup for a search: the config, the package root, the
-- parsed arguments and the file's text.
smtSetup :: A.Value -> FilePath -> RunArgs -> T.Text -> IO SmtSetup
smtSetup = smtSetupWith findSolver

-- | 'smtSetup' with the solver lookup given.
smtSetupWith :: (SmtConfig -> IO (Maybe Solver)) -> A.Value -> FilePath -> RunArgs -> T.Text -> IO SmtSetup
smtSetupWith lookupSolver cfg root ra original = do
  let sc = smtConfig cfg
  leanSmt <- hasSmt root
  let -- a module file takes no marker line (Lean rejects an import before
      -- @module@), so the arm is off for it
      moduleFile = leanSmt && smtcEnabled sc && headerKind original == Module
      available = leanSmt && smtcEnabled sc && not moduleFile
      given = raAutoTactics ra
      gated = gateTactics available <$> given
      dropped = not available && maybe False (elem "smt") given
      -- a user list of nothing but smt: no pre-pass to run, so no server
      emptied = dropped && gated == Just []
      offTail = if emptied then ", which leaves no tactic: the automation is off (as with --no-auto)" else ""
      why
        | leanSmt = "fisher.smt.enabled is false"
        | otherwise = "the project has no lean-smt (fisher project add smt)"
      base = if available then insertImportSmt original else original
      ra' = ra {raSmt = available, raAutoTactics = gated, raAuto = raAuto ra && not emptied}
  solver <- if available then lookupSolver sc else pure Nothing
  pure SmtSetup
    { ssAvailable = available
    , ssArgs = ra'
    , ssBase = base
    , ssInserted = base /= original
    , ssWarning =
        if moduleFile
          then Just ("smt is not available for a module file: the arm is off for this run"
                     <> (if dropped then "; dropped from --auto-tactics" <> offTail else ""))
        else if not dropped then Nothing
        else Just ("smt is not available here: " <> why <> "; dropped from --auto-tactics" <> offTail)
    , ssFacts = smtFacts available (solverName <$> solver)
    , ssSmt = (\s -> SmtEnv s (smtcTimeoutSecs sc) (smtcQueryTactic sc) runQuery) <$> solver
    , ssSolverWarning =
        if available && null solver && serverWanted ra' && raDisprove ra'
          then Just ("smt: no solver on PATH for fisher.smt.solver " <> smtcSolver sc <> " (z3 or cvc5); the SMT countermodels are off for this run")
          else Nothing }

-- | The merged text as the search writes it at the end (@base.lean@ and
-- the @--apply@ write) and the report's @smt_import@: given whether the
-- marker went in and the base at the start, the marker stays only when a
-- kept proof uses @smt@.
smtOutput :: Bool -> T.Text -> T.Text -> (T.Text, [Pair])
smtOutput inserted start final
  | not inserted = (final, [])
  | otherwise =
      ( applySmtImport start final
      , ["smt_import" .= (if smtKept start final then "kept" else "removed" :: T.Text)] )

-- | The run's @learn@ as one line.
learnLine :: A.Value -> T.Text
learnLine = \case
  A.Object o
    | Just (A.Bool True) <- KM.lookup "enabled" o ->
        "learn: policy " <> str "policy_file" o <> " (" <> num "shapes" o <> " shapes, " <> num "changes" o <> " changes)"
    | otherwise -> "learn: off (" <> str "reason" o <> ")"
  _ -> "learn: off"
  where
    str k o = case KM.lookup k o of
      Just (A.String t) -> t
      _ -> ""
    num k o = case KM.lookup k o of
      Just (A.Number x) -> T.pack (show (round x :: Int))
      _ -> "0"

-- | The facts' settings, or Nothing with @--no-retrieval@.
factsConfig :: RunArgs -> Maybe FactsConfig
factsConfig ra
  | not (raRetrieval ra) = Nothing
  | otherwise = Just defaultFacts {fcMax = raFacts ra, fcChars = raFactsChars ra}

-- | Whether the run builds the index: facts or the library on. With both
-- off the search is today's.
retrievalWanted :: RunArgs -> Bool
retrievalWanted ra = raRetrieval ra || raLibrary ra

-- | The run's retrieval, built once: the project's index (cached in the
-- library directory) and the library as it is now, with the events to
-- log first. Nothing (and why, as an event) when the index cannot be built.
buildRetrieval :: RunArgs -> FilePath -> FilePath -> IO (Maybe RetrievalEnv, [Event])
buildRetrieval ra root runDir = do
  home <- getFisherHome
  buildRetrievalAt False home ra root runDir

-- | 'buildRetrieval' under a given Fisher home. Read-only (for
-- @--print-facts@): nothing is written to the library directory or
-- under the home; the index cache goes to a private temporary directory
-- (mode 700, removed once the index is built), the library is read
-- without its lock ('peekLibrary'), and nothing is harvested.
buildRetrievalAt :: Bool -> FilePath -> RunArgs -> FilePath -> FilePath -> IO (Maybe RetrievalEnv, [Event])
buildRetrievalAt readOnly home ra root runDir = do
  let dir = libraryDirFor ra home root
  t0 <- getPOSIXTime
  built <- try @SomeException $
    if readOnly
      then do
        tmp <- getTemporaryDirectory
        bracket (mkdtemp (tmp </> "fisher-print-facts-")) (\d -> void (try @SomeException (removeDirectoryRecursive d))) $ \d ->
          buildIndex root (d </> "index.json")
      else do
        createDirectoryIfMissing True dir
        buildIndex root (indexCacheFor ra home root)
  t1 <- getPOSIXTime
  case built of
    Left e | isJust (fromException e :: Maybe SomeAsyncException) -> throwIO e
    Left e -> pure (Nothing, [LibraryProblem 0 ("the index could not be built: " <> T.take 300 (T.pack (show e)))])
    Right (ix, skipped) -> do
      let secs = realToFrac (t1 - t0) :: Double
          evs0 = IndexBuilt 0 (Map.size (ixImports ix)) (length (ixEntries ix)) secs : [IndexSkipped 0 f why | (f, why) <- skipped]
      (lib, evs1) <-
        if raLibrary ra && readOnly
          then do
            r <- try @SomeException (peekLibrary dir)
            pure $ case r of
              Left e -> (Just ([], []), [LibraryProblem 0 ("the library could not be read: " <> T.take 300 (T.pack (show e)))])
              Right l -> (Just l, [])
        else if raLibrary ra
          then do
            r <- try @SomeException (loadLibraryReport dir)
            pure $ case r of
              Left e -> (Just ([], []), [LibraryProblem 0 ("the library could not be read: " <> T.take 300 (T.pack (show e)))])
              Right (l, bad) -> (Just l, [LibraryProblem 0 ("set aside " <> T.pack b <> ": it did not parse") | b <- bad])
          else pure (Nothing, [])
      pure (Just (RetrievalEnv ix (factsConfig ra) lib (not (readOnly || raLibraryReadOnly ra)) dir root runDir retrievalCutoff ""), evs0 <> evs1)

-- | The library directory, given the Fisher home and the package root:
-- @--library-dir@ itself (one library for every project run with it, as
-- the bench's workspaces are), else @<home>/fisher/library/<key>@.
libraryDirFor :: RunArgs -> FilePath -> FilePath -> FilePath
libraryDirFor ra home root = fromMaybe (libraryDir home root) (raLibraryDir ra)

-- | The index's cache file: @index.json@ in the project's own library
-- directory; in a shared @--library-dir@, @index-<key>.json@, one per
-- package root, so two workspaces never read each other's scans.
indexCacheFor :: RunArgs -> FilePath -> FilePath -> FilePath
indexCacheFor ra home root = case raLibraryDir ra of
  Nothing -> libraryDir home root </> "index.json"
  Just d -> d </> ("index-" <> T.unpack (libraryKey root) <> ".json")

-- | @FILE --print-facts@: each open goal's retrieval as a brief would
-- carry it, with the goals' types from one compile of the file (keyword
-- facts only when it does not compile). Starts no attempt and saves
-- nothing to the library.
printFactsMain :: RunArgs -> FilePath -> IO ()
printFactsMain ra file0 = do
  file <- makeAbsolute file0
  root <- lakeRoot file >>= maybe (hPutStrLn stderr ("no lakefile above " <> file) >> exitWith (ExitFailure 2)) pure
  let rel = makeRelative root file
  original <- readUtf8 file
  home <- getFisherHome
  (mre, evs) <- buildRetrievalAt True home ra {raRetrieval = True} root ""
  mapM_ (TIO.putStrLn . eventLine) evs
  re <- maybe (hPutStrLn stderr "(>_<) no index" >> exitWith (ExitFailure 1)) pure mre
  let goals = openGoals original
      names = map dName goals
  types <- goalTypes (lakeLeanRun 900) root rel original names
  tys <- case types of
    Left e -> TIO.putStrLn ("no types (keyword facts only): " <> e) >> pure Map.empty
    Right m -> pure m
  retrieveFor <- newRetriever re rel
  let opens = map dStatement goals
  mapM_ (\d -> do
    rt <- retrieveFor (Query (dName d) (dName d) (dStart d + 1) (dStatement d) (Map.lookup (dName d) tys) opens)
    TIO.putStrLn ("== " <> dName d <> " (" <> T.pack rel <> ":" <> T.pack (show (dStart d + 1)) <> "): "
                  <> T.pack (show (rtFacts rt)) <> " facts, " <> T.pack (show (rtFactsChars rt)) <> " chars, lemmas "
                  <> T.pack (show (rtOffered rt)) <> ", " <> T.pack (show (rtNotes rt)) <> " notes"
                  <> (if rtCutOff rt then " (cut off)" else ""))
    TIO.putStrLn (rtBrief rt)
    hFlush stdout) goals

-- | Whether the run starts the automation's language server: auto on, and
-- not @--single@ (the baseline runs no automation).
serverWanted :: RunArgs -> Bool
serverWanted ra = isJust (autoConfig ra) && raSingle ra == Nothing

-- | The backends a run can name: claude-code, codex and the native one.
knownBackends :: [T.Text]
knownBackends = ["claude-code", "codex", nativeBackend]

-- | The rules for a run's flags: every backend named (@--backends@,
-- @--single@) is one of 'knownBackends'; @--graph@ takes neither
-- @--single@ nor @--one-per-goal@, and runs on the native backend only
-- (the backends become @["fisher"]@); its own flags need it.
checkGraph :: RunArgs -> Either T.Text RunArgs
checkGraph ra
  | b : _ <- filter (`notElem` knownBackends) (raBackends ra <> maybe [] pure (raSingle ra)) =
      Left ("unknown backend `" <> b <> "`; the backends are: " <> T.intercalate ", " knownBackends)
  | not (raGraph ra) =
      if raGraphFlags ra then Left "--max-depth, --max-splits and --cap-usd need --graph" else Right ra
  | raSingle ra /= Nothing || not (raParallelPerGoal ra) = Left "--graph does not take --single or --one-per-goal"
  | otherwise = (\bs -> ra {raBackends = bs}) <$> graphBackends (if raBackendsGiven ra then Just (raBackends ra) else Nothing)

-- | The run directory, absolute: @--out@ (from the working directory),
-- else @~/.cache/fisher/search/<stamp>@. Absolute because the run log
-- keeps it as the run's raw directory, which @fisher runs import@ matches
-- from any working directory.
searchRunDir :: FilePath -> String -> Maybe FilePath -> IO FilePath
searchRunDir home stamp out = makeAbsolute (fromMaybe (home </> ".cache" </> "fisher" </> "search" </> stamp) out)

-- | The Lake project a file belongs to: the nearest directory above it with
-- a lakefile.
lakeRoot :: FilePath -> IO (Maybe FilePath)
lakeRoot path = go (takeDirectory path)
  where
    go d = do
      has <- or <$> mapM (doesFileExist . (d </>)) ["lakefile.toml", "lakefile.lean"]
      if has then pure (Just d) else let p = takeDirectory d in if p == d then pure Nothing else go p

-- | From now on SIGTERM throws 'Terminated' (asynchronous, like Ctrl-C's
-- interrupt) to the calling thread, so a run its watcher stops still
-- harvests into the library before it exits ('exitOnTerminated').
termToException :: IO ()
termToException = do
  me <- myThreadId
  void (installHandler sigTERM (Catch (throwTo me Terminated)) Nothing)

-- | 'Terminated' reaching the top exits with code 143, as a process
-- killed by SIGTERM would.
exitOnTerminated :: IO a -> IO a
exitOnTerminated = handle (\Terminated -> exitWith (ExitFailure 143))

runMain :: RunArgs -> FilePath -> IO ()
runMain ra0 file0 = do
  termToException
  file <- makeAbsolute file0
  root <- lakeRoot file >>= maybe (hPutStrLn stderr ("no lakefile above " <> file) >> exitWith (ExitFailure 2)) pure
  let rel = makeRelative root file
  onDisk <- readUtf8 file
  liLive <- liveInputs
  -- the smt arm, decided once: every stage sees the same base text and
  -- tactics
  smt <- smtSetup (liConfig liLive) root ra0 onDisk
  mapM_ (hPutStrLn stderr . T.unpack) (ssWarning smt)
  mapM_ (hPutStrLn stderr . T.unpack) (ssSolverWarning smt)
  let ra = ssArgs smt
      original = ssBase smt
      smtOut = smtOutput (ssInserted smt) original
  stamp <- formatTime defaultTimeLocale "%Y%m%d-%H%M%S" <$> getZonedTime
  home <- getHomeDirectory
  runDir <- searchRunDir home stamp (raOut ra)
  let baseWs = runDir </> "base"
  createDirectoryIfMissing True runDir
  makeWorkspace root baseWs rel original
  -- with lessons off every attempt (and its plan) is told so
  let lessonsOn = lessonsEnabled ra (liConfig liLive)
      liOff = liLive {liBaseEnv = lessonsEnv lessonsOn (liBaseEnv liLive)}
  -- the lessons block, computed once: every attempt's prompt carries it and
  -- the run's lessons event records its ids and hash
  searchLessons <- promptLessonsFor liOff root
  let li0 = liOff {liLessons = searchLessons}
      m = liMachine li0
      casc = cascadeFor ra (searchTiersConfig (liConfig li0))
      mainState = fromMaybe (mFisherHome m </> "autoformalize") (Map.lookup "FISHER_AUTOFORMALIZE_MANAGED_STATE_DIR" (liBaseEnv li0))
  -- stage the workflow assets once, where every attempt copies them from
  prep <- planFor li0 baseWs Nothing (fromMaybe (case raBackends ra of { b : _ -> b; [] -> "claude-code" }) (raSingle ra)) (raWorkflow ra) rel Nothing
  case prep of
    Left e -> hPutStrLn stderr ("(>_<) " <> T.unpack e) >> exitWith (ExitFailure 1)
    Right _ -> pure ()
  counter <- newIORef (0 :: Int)
  runId <- newRunId
  -- the run log: the search's run now, each attempt's as it starts, the
  -- ends and the goals at the end; a failure is one line on stderr
  -- each ledger-watched attempt's tag and start, to price it if cancelled
  ledgerOf <- newIORef (Map.empty :: Map.Map Int (T.Text, Double))
  -- the run's start for the watcher's events, set again as the search starts
  runStart <- newIORef =<< (realToFrac <$> getPOSIXTime :: IO Double)
  args <- getArgs
  t00 <- clockNow
  -- the run directory's name and the run id: two runs with one --out name
  -- are two runs
  let parentId = searchRunId runDir runId
      tagOf = attemptTag runDir runId
      stmts = [(dName d, dStatement d) | d <- declarations original]
  slog <- openSearchLog (mFisherHome m) tagOf (readIORef runStart) NewRun
    { nrId = parentId, nrKind = SearchRun, nrParent = Nothing, nrProject = Just root, nrProver = Just "lean"
    , nrBackend = Just (T.intercalate "," (maybe (raBackends ra) pure (raSingle ra))), nrModel = cModel casc, nrTier = Nothing
    , nrWorkflow = Just (raWorkflow ra), nrArgv = map T.pack args, nrStartedAt = t00, nrRawDir = Just runDir, nrCapture = Full }
  -- from here on an interrupt (a Ctrl-C during the language server's
  -- warm-up or the policy's computation too) ends the search's run as
  -- cancelled, with its running attempts, and the log is closed
  let cancelled = do
        t1 <- clockNow
        searchLogInterrupted slog
        finishSearchLog slog [] [] (RunEnd t1 Nothing "cancelled" Nothing Nothing Nothing Nothing)
  ( do
      -- the pre-pass's tactics, for the misses the statistics charge to them,
      -- before any auto event
      searchLogPortfolio slog (portfolioTactics ra)
      -- the lessons the attempts' prompts carry, the files' hashes and the
      -- skill's revision
      forM_ (if lessonsOn then searchLessons else Nothing) $ \pl -> searchLogged slog $ \rl ->
        lessonsEventFrom pl (mFisherHome m) root (Just (mainState </> "assets")) >>= event rl parentId 0 "lessons"
      (goalPolicy, learnV) <- learnPolicy ra (liConfig li0) (mFisherHome m) root slog
                                [(dName d, dStatement d) | d <- openGoals original]
      TIO.putStrLn (learnLine learnV)
      eventsH <- openFile (runDir </> "events.jsonl") WriteMode
      hSetBuffering eventsH LineBuffering
      let usesNative = "native_decide" `T.isInfixOf` original
          pol = AxiomPolicy (Set.union standardAxioms (Set.fromList (raAllow ra))) (raNative ra || usesNative)
          logE e = do
            BL8.hPutStrLn eventsH (A.encode (eventJson e))
            TIO.putStrLn (eventLine e)
            hFlush stdout
            searchLogSearchEvent slog e
          finish attempts goals outcome cost = do
            t1 <- clockNow
            finishSearchLog slog attempts goals (RunEnd t1 Nothing outcome cost Nothing Nothing Nothing)
          -- after finish: the lessons pass over the run, its report's object
          mineAfter = mineSearch ra casc (liConfig li0) (mFisherHome m) root slog lessonsOn
          runAttempt spec = do
            n <- atomicModifyIORef' counter (\c -> (c + 1, c + 1))
            let safe = T.unpack (T.map (\c -> if c `elem` ("/ ." :: String) then '_' else c) (asGoal spec))
                dir = runDir </> "attempts" </> (show n <> "-" <> take 60 safe <> "-" <> T.unpack (asBackend spec))
                ws = dir </> "ws"
                state = dir </> "managed"
            createDirectoryIfMissing True dir
            makeWorkspace root ws rel (asStart spec)
            hasAssets <- doesDirectoryExist (mainState </> "assets")
            when hasAssets $ do
              createDirectoryIfMissing True state
              _ <- readCreateProcessWithExitCode (proc "cp" (if os == "darwin" then ["-cR", mainState </> "assets", state </> "assets"] else ["-R", mainState </> "assets", state </> "assets"])) ""
              pure ()
            let native = isNative (asBackend spec)
                watching = attemptWatched casc (asGoal spec)
                tag = tagOf (asId spec)
                -- the attempt's budget: the whole search's for the --single
                -- baseline, else the run's, or the policy's when that is lower
                learned = Map.lookup (asGoal spec) goalPolicy >>= gpAttemptSecs
                secs = if asGoal spec == "*" then ceiling (raBudget ra) else maybe (raAttemptSecs ra) (min (raAttemptSecs ra)) learned
                -- the attempt's run, started before its child (which adds to it)
                -- the attempt's run, started before its child (which adds to
                -- it), with its goal in an @attempt@ event; ended when the
                -- search judges it ('searchLogSearchEvent')
                begin argv' = do
                  t <- clockNow
                  searchLogBegin slog (asId spec) NewRun
                    { nrId = tag, nrKind = SearchAttempt, nrParent = Just parentId, nrProject = Just root, nrProver = Just "lean"
                    , nrBackend = Just (asBackend spec), nrModel = if native then asModel spec <|> cModel casc else asModel spec
                    , nrTier = T.pack . show <$> asTier spec, nrWorkflow = Just (raWorkflow ra), nrArgv = argv'
                    , nrStartedAt = t, nrRawDir = Just dir, nrCapture = Full }
                    (attemptEventData (asGoal spec) (knownStatement stmts (asGoal spec)) (asBackend spec) (Just secs))
                unstarted r = begin [] >> pure r
                -- after the child: a managed backend's stream as events (the
                -- native agent writes its own)
                ended res = do
                  let stats = arStats res
                  unless native $ searchLogged slog $ \rl -> do
                    let path = dir </> "stream.jsonl"
                    there <- doesFileExist path
                    bytes <- if there then BS.readFile path else pure ""
                    forM_ (streamTimes (csWall <$> stats) (streamEvents (asBackend spec) bytes)) $ \(t, k, d) -> event rl tag t k d
                  pure res
            planned <- planFor li0 ws (Just state) (asBackend spec) (raWorkflow ra) rel (if native then Nothing else asModel spec)
            case planned of
              Left e -> unstarted (plainResult Nothing Nothing ("plan failed: " <> e))
              Right (argv0, cwd', env) -> case (if native then maybe Right setModel (asModel spec <|> cModel casc) argv0 else Right argv0) of
               Left e -> unstarted (plainResult Nothing Nothing e)
               Right argv -> do
                let extra = if asGoal spec == "*" then singlePrompt rel else withBrief (attemptPrompt (asGoal spec) rel) (asBrief spec)
                case headlessArgv (asBackend spec) argv extra (raMaxTurns ra) of
                  Left e -> unstarted (plainResult Nothing Nothing e)
                  Right hargv -> do
                    logged <- begin hargv
                    let runEnv = lessonsEnv lessonsOn ((if logged then attemptRunEnv tag parentId else id) (ledgerEnv (watching && native) tag env))
                        child = runChild (map T.unpack hargv) cwd' (Map.toList runEnv) secs dir
                        readOut = do
                          out <- try @SomeException (readUtf8 (ws </> rel))
                          BS.writeFile (dir </> "final.lean") (either (const "") TE.encodeUtf8 out)
                          pure (either (const Nothing) Just out)
                        note stats = maybe "killed at the time limit" (\c -> "exit " <> T.pack (show c)) (csExit stats)
                    ended =<< if not watching
                      then do
                        stats <- child
                        out <- readOut
                        pure (plainResult out (Just stats) (note stats))
                      else do
                        target <- makeAbsolute (ws </> rel)
                        t0 <- getPOSIXTime
                        when native $ modifyIORef' ledgerOf (Map.insert (asId spec) (tag, realToFrac t0))
                        let streamPath = dir </> "stream.jsonl"
                            rule = StallRule (raStallTurns ra) (if native then Just (raStallDollars ra) else Nothing)
                            src = if native then FromLedger (mFisherHome m </> "state.db") tag target else FromStream streamPath (asBackend spec) target
                            opts = defaultWatchOpts
                              { woUnread = \why -> do
                                  s0 <- readIORef runStart
                                  t <- realToFrac <$> getPOSIXTime
                                  logE (LedgerUnread (t - s0) (asId spec) why)
                              , woStream = Just streamPath
                              , woSince = realToFrac t0 }
                        r <- watchChildWith opts rule (cEarlyStop casc) src child
                        out <- readOut
                        streamed <- streamLast (if native then nativeBackend else asBackend spec) <$> readLines streamPath
                        let priced w res = res {arDollars = attemptDollars w (arStats res), arLastMessage = wLast w <|> streamed}
                        case r of
                          -- a model failure only of an attempt that ended by itself
                          Right (stats, w) -> do
                            failure <- failureOf streamPath (dir </> "stderr.txt") (csExit stats)
                            pure (priced w (plainResult out (Just stats) (note stats)) {arFailure = failure})
                          Left w -> do
                            t1 <- getPOSIXTime
                            partial <- streamStats <$> BS.readFile streamPath
                            let stats = partial {csExit = Nothing, csWall = realToFrac (t1 - t0)}
                                why = maybe "stopped" (\st -> "stopped: " <> stopReasonText (stReason st)) (wStop w)
                            pure (priced w (plainResult out (Just stats) why) {arStop = wStop w})
          -- a cancelled native attempt's dollars: its ledger's cost rows
          priceOf spec = Map.lookup (asId spec) <$> readIORef ledgerOf >>= \case
            Just (tag, since) -> ledgerDollars (mFisherHome m </> "state.db") tag since
            Nothing -> pure Nothing
          env0 = SearchEnv runAttempt (lakeLeanRun 900) pol baseWs rel (realToFrac <$> getPOSIXTime) logE
                   (\t -> BS.writeFile (runDir </> "base.lean") (TE.encodeUtf8 t) >> BS.writeFile (baseWs </> rel) (TE.encodeUtf8 t))
                   Nothing Nothing priceOf
          cfg = (searchConfigFor ra casc) {scGoalPolicy = goalPolicy}
      TIO.putStrLn ("search " <> T.pack rel <> " in " <> T.pack root <> ": " <> T.pack (show (length (openGoals original)))
                    <> " open goals; run directory " <> T.pack runDir <> (if raGraph ra then " (lemma graph)" else ""))
      -- the automation's language server, in the check workspace (its trials
      -- are virtual documents there; nothing is written); never for --single,
      -- which runs no automation. Its warm-up is bounded and counts toward no
      -- budget.
      started <- case autoConfig ra of
        Just ac | serverWanted ra -> do
          TIO.putStrLn "auto: starting Lean's language server on the file (loading its imports can take a minute)"
          hFlush stdout
          w0 <- getPOSIXTime
          r <- openLsp (raAutoWarmup ra) baseWs rel
          w1 <- getPOSIXTime
          let secs = realToFrac (w1 - w0) :: Double
          TIO.putStrLn ("auto: the warm-up took " <> T.pack (show (round secs :: Int)) <> " s" <> either (const " and failed") (const "") r)
          hFlush stdout
          pure ((\l -> Just (AutoEnv ac l (ssSmt smt), secs)) <$> r)
        _ -> pure (Right Nothing)
      -- the index, built once for the run (not for --single, which has no briefs)
      (retrieval, retrievalEvents) <-
        if retrievalWanted ra && raSingle ra == Nothing then buildRetrieval ra root runDir else pure (Nothing, [])
      writeIORef runStart =<< (realToFrac <$> getPOSIXTime)
      let env1 = env0 {seAuto = either (const Nothing) (fmap fst) started, seRetrieval = (\re -> re {reExtra = ssFacts smt}) <$> retrieval}
          offAtStart = either (\e -> Just ("the Lean server did not start: " <> e)) (const Nothing) started
          autoExtra = case autoConfig ra of
            Nothing -> const []
            Just _ -> \(ran, during) ->
              autoStateJson (offAtStart <|> ("--single runs no automation" <$ raSingle ra)) ran during
                <> either (const []) (maybe [] (\(_, w) -> ["auto_warmup_s" .= (fromIntegral (round (w * 10) :: Int) / 10 :: Double)])) started
      ( if raGraph ra
          then graphMain ra casc file root rel runDir eventsH env1 offAtStart retrievalEvents autoExtra learnV original (searchLogGraphEvent slog) finish mineAfter smtOut
          else swarmMain ra file root rel runDir eventsH env1 cfg offAtStart retrievalEvents autoExtra learnV original stmts finish mineAfter smtOut )
        `finally` mapM_ (lspClose . aeLsp) (seAuto env1)
    ) `onException` cancelled `finally` closeSearchLog slog

swarmMain :: RunArgs -> FilePath -> FilePath -> FilePath -> FilePath -> Handle -> SearchEnv -> SearchConfig -> Maybe T.Text
          -> [Event] -> ((Bool, Maybe T.Text) -> [Pair]) -> A.Value -> T.Text -> [(T.Text, T.Text)] -> Finish -> IO A.Value
          -> (T.Text -> (T.Text, [Pair])) -> IO ()
swarmMain ra file root rel runDir eventsH env0 cfg offAtStart startEvents autoExtra learnV original stmts finish mineAfter smtOut = do
  mapM_ (seLog env0 . AutoOff 0) offAtStart
  mapM_ (seLog env0) startEvents
  r <- case raSingle ra of
    Just b -> runSingle env0 b original
    Nothing -> runSearch cfg env0 original
  hClose eventsH
  case r of
    Left e -> finish [] [] "failed" Nothing >> hPutStrLn stderr ("(>_<) " <> T.unpack e) >> exitWith (ExitFailure 1)
    Right rep -> do
      finish (srAttempts rep) (goalRows stmts rep) (searchOutcome rep)
        (knownSum (map arCost (srAttempts rep)))
      let (written, smtPairs) = smtOut (srBase rep)
          extra lessonsV =
            [ "file" .= rel, "project" .= root, "mode" .= maybe ("swarm" :: T.Text) ("single " <>) (raSingle ra)
            , "backends" .= raBackends ra, "slots" .= raSlots ra, "budget_s" .= raBudget ra
            , "attempt_s" .= raAttemptSecs ra, "max_turns" .= raMaxTurns ra, "learn" .= withLessons learnV lessonsV ]
            <> autoExtra (srAutoRan rep, srAutoOff rep) <> smtPairs
      reportAfterLessons
        (do BS.writeFile (runDir </> "base.lean") (TE.encodeUtf8 written)
            when (raApply ra) $ BS.writeFile file (TE.encodeUtf8 written))
        mineAfter
        (\lv -> BL8.writeFile (runDir </> "report.json") (encodePretty (reportJson (extra lv) rep)))
      when (srAuto rep) $ mapM_ TIO.putStrLn (goalTable rep)
      TIO.putStrLn $ T.pack (show (srClosed rep)) <> " of " <> T.pack (show (length (srGoals rep))) <> " goals closed in "
        <> T.pack (show (round (srElapsed rep) :: Int)) <> " s; report " <> T.pack (runDir </> "report.json")
        <> (if raApply ra then "; written to " <> T.pack file else "; merged file " <> T.pack (runDir </> "base.lean"))

-- | The lemma-graph run: the planner and verifier on the native agent's
-- provider, the graph and whiteboards saved after every job, its events
-- beside the attempts'.
graphMain :: RunArgs -> Cascade -> FilePath -> FilePath -> FilePath -> FilePath -> Handle -> SearchEnv -> Maybe T.Text
          -> [Event] -> ((Bool, Maybe T.Text) -> [Pair]) -> A.Value -> T.Text -> (GraphEvent -> IO ()) -> Finish -> IO A.Value
          -> (T.Text -> (T.Text, [Pair])) -> IO ()
graphMain ra casc file root rel runDir eventsH env0 offAtStart startEvents autoExtra learnV original toRunlog finish mineAfter smtOut = do
  home <- getHomeDirectory
  gh <- getFisherHome
  sys <- newSysIO home gh
  -- the planner's and the verifier's model: the last tier, else --model
  modelName <- case plannerModel (cTiers casc) (cModel casc) of
    Just m -> pure m
    Nothing -> configuredModel <$> loadConfigPy sys {sysOut = \_ -> pure ()}
  built <- liveModelFor sys modelName
  model <- case built of
    Left e -> hPutStrLn stderr ("(>_<) " <> T.unpack e) >> exitWith (ExitFailure 1)
    Right m -> pure (m :: Model)
  let logG e = do
        BL8.hPutStrLn eventsH (A.encode (graphEventJson e))
        TIO.putStrLn (graphEventLine e)
        hFlush stdout
        toRunlog e
      saveG g boards = do
        BL8.writeFile (runDir </> "graph.json") (encodePretty (graphJson g boards))
        mapM_ (\(r, t) -> BS.writeFile (runDir </> whiteboardFile g r) (TE.encodeUtf8 t)) boards
      genv = GraphEnv
        { geSearch = env0
        , geAsk = \_ msgs -> askResult <$> model Nothing msgs
        , geLogG = logG
        , geSave = saveG }
      gcfg = GraphConfig
        { gcSlots = raSlots ra, gcMaxDepth = raMaxDepth ra, gcMaxSplits = raMaxSplits ra, gcCapUsd = raCapUsd ra
        , gcBudget = raBudget ra, gcTiers = cTiers casc, gcControl = cControl casc }
  mapM_ (logG . GraphAutoOff 0) offAtStart
  mapM_ (logG . GraphRunEvent) startEvents
  r <- runGraph gcfg genv original
  hClose eventsH
  case r of
    Left e -> finish [] [] "failed" Nothing >> hPutStrLn stderr ("(>_<) " <> T.unpack e) >> exitWith (ExitFailure 1)
    Right rep -> do
      -- the graph's goals are its roots; the goal rows are the flat search's
      let roots = length (gRoots (grGraph rep))
          outcome
            | roots > 0 && grClosedRoots rep == roots = "closed"
            | isJust (grStopped rep) = "budget"
            | otherwise = "failed"
      finish (grAttempts rep) [] outcome (Just (sum (Map.elems (grDollars rep))))
      let g = grGraph rep
          base = grBase rep
          (written, smtPairs) = smtOut base
          extra lessonsV =
            [ "file" .= rel, "project" .= root, "mode" .= ("graph" :: T.Text)
            , "backends" .= raBackends ra, "slots" .= raSlots ra, "budget_s" .= raBudget ra
            , "attempt_s" .= raAttemptSecs ra, "max_turns" .= raMaxTurns ra
            , "max_depth" .= raMaxDepth ra, "max_splits" .= raMaxSplits ra, "cap_usd" .= raCapUsd ra, "model" .= modelName
            , "learn" .= withLessons learnV lessonsV ]
            <> autoExtra (grAutoRan rep, grAutoOff rep) <> smtPairs
          left = sorryLemmas g base
          applied = raApply ra && null left
          merged = T.pack (runDir </> "base.lean")
      reportAfterLessons
        (do BS.writeFile (runDir </> "base.lean") (TE.encodeUtf8 written)
            when applied $ BS.writeFile file (TE.encodeUtf8 written))
        mineAfter
        (\lv -> BL8.writeFile (runDir </> "report.json") (encodePretty (graphReportJson (extra lv) rep)))
      mapM_ (mapM_ TIO.putStrLn . graphTree g) (gRoots g)
      when (raApply ra && not applied) $
        TIO.putStrLn ("not applied: " <> T.pack (show (length left)) <> " lemmas from splits are still sorry; merged file " <> merged)
      TIO.putStrLn $ T.pack (show (grClosedRoots rep)) <> " of " <> T.pack (show (length (gRoots g))) <> " goals closed in "
        <> T.pack (show (round (grElapsed rep) :: Int)) <> " s, $" <> T.pack (showFFloat (Just 2) (sum (Map.elems (grDollars rep))) "")
        <> maybe "" (\w -> " (stopped: " <> w <> ")") (grStopped rep)
        <> "; report " <> T.pack (runDir </> "report.json")
        <> (if applied then "; written to " <> T.pack file else if raApply ra then "" else "; merged file " <> merged)

-- | The end of a finished search: its outputs (the merged file and the
-- @--apply@ write, which do not wait for the lessons), then the lessons
-- pass, then the report with the pass's object. An interrupt during the
-- pass still writes the report, its lessons @{interrupted: true}@, and
-- then goes on up; any other failure of it is a warning and the report's
-- @{enabled: true, error}@.
reportAfterLessons :: IO () -> IO A.Value -> (A.Value -> IO ()) -> IO ()
reportAfterLessons outputs mine writeReport = do
  outputs
  r <- try @SomeException mine
  case r of
    Right v -> writeReport v
    Left e
      | Just (_ :: SomeAsyncException) <- fromException e -> writeReport (object ["interrupted" .= True]) >> throwIO e
      | otherwise -> do
          let why = "the pass failed: " <> T.take 300 (T.pack (displayException e))
          lessonsWarn why
          writeReport (object ["enabled" .= True, "error" .= why])

-- | The lessons pass after a search (its log, its run): the rules, and
-- the judge (built as the lemma graph's planner is, 'liveJudge') when
-- @fisher.lessons.judge@ is on; the report's @lessons@ object. Off,
-- @{enabled: false, reason}@. On, a pass that could not run or failed is
-- @{enabled: true, error}@ with one @fisher: lessons:@ warning (the log
-- not open, an earlier write to it failed, or the pass's own failure); the
-- search goes on. An interrupt goes on up. A line says the pass started,
-- since it can wait up to 30 s for another pass's lock and for the judge.
mineSearch :: RunArgs -> Cascade -> A.Value -> FilePath -> FilePath -> SearchLog -> Bool -> IO A.Value
mineSearch ra casc cfg home root sl on
  | not on = pure (object ["enabled" .= False, "reason" .= (if raLessons ra then "fisher.lessons.enabled is false" else "--no-lessons" :: T.Text)])
  | otherwise = do
      TIO.putStrLn ("lessons: mining " <> slParent sl <> " (the rules" <> (if lessonsJudgeOn cfg then ", then the judge" else "") <> ")")
      hFlush stdout
      earlier <- readIORef (slWarned sl)
      case slRunlog sl of
        Nothing -> failed "the run log is not open"
        Just _ | earlier -> failed "an earlier write to the run log failed"
        Just rl -> do
          r <- try @SomeException $ do
            judge <- if lessonsJudgeOn cfg then liveJudge casc else pure Nothing
            env <- mineEnvFor lessonsWarn cfg home root judge rl
            mineRun env (slParent sl) False
          case r of
            Right rep -> pure (mineReportJson rep)
            Left e
              | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
              | otherwise -> failed ("the pass failed: " <> T.take 300 (T.pack (displayException e)))
  where
    failed why = do
      lessonsWarn why
      pure (object ["enabled" .= True, "error" .= why])

-- | The judge's model call, on the lemma graph planner's route: the last
-- tier, else @--model@, else the configured model, over 'liveModelFor'.
-- Nothing (and one warning) when it cannot be built.
liveJudge :: Cascade -> IO (Maybe Ask)
liveJudge casc = do
  r <- try @SomeException $ do
    home <- getHomeDirectory
    gh <- getFisherHome
    sys <- newSysIO home gh
    modelName <- case plannerModel (cTiers casc) (cModel casc) of
      Just m -> pure m
      Nothing -> configuredModel <$> loadConfigPy sys {sysOut = \_ -> pure ()}
    liveModelFor sys modelName
  case r of
    Left e
      | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
      | otherwise -> Nothing <$ lessonsWarn ("no judge: " <> T.take 300 (T.pack (displayException e)))
    Right (Left e) -> Nothing <$ lessonsWarn ("no judge: " <> e)
    Right (Right model) -> pure (Just (\_ msgs -> judgeAskResult <$> (model :: Model) Nothing msgs))

-- | The goals at the end of a run with the automation on: name, status,
-- and what the automation did (@closed@, @repair@ when a repaired attempt
-- closed it, @disproved@, @suspect@, or @-@).
goalTable :: SearchReport -> [T.Text]
goalTable rep =
  let rows = ("goal", "status", "auto") : [(g, statusName (gsStatus s), autoOf s) | (g, s) <- srGoals rep]
      w1 = maximum [T.length a | (a, _, _) <- rows]
      w2 = maximum [T.length b | (_, b, _) <- rows]
   in [T.justifyLeft w1 ' ' a <> "  " <> T.justifyLeft w2 ' ' b <> "  " <> c | (a, b, c) <- rows]
  where
    autoOf s = case (gsDisproof s, gsClosedBy s) of
      (Just (DFalse _), _) -> "disproved"
      (_, Just "auto") | closed s -> "closed"
      (_, Just "model+repair") | closed s -> "repair"
      (Just (DSuspect _), _) -> "suspect"
      _ -> "-"
    closed s = case gsStatus s of
      S.Closed -> True
      _ -> False

-- | whiteboard-<n>-<root name>.md
whiteboardFile :: Graph -> NodeId -> FilePath
whiteboardFile g r =
  let name = maybe "" nName (IM.lookup r (gNodes g))
      safe = T.unpack (T.map (\c -> if c `elem` ("/ ." :: String) then '_' else c) name)
   in "whiteboard-" <> show r <> "-" <> take 60 safe <> ".md"

-- | The attempt's prompt, then a blank line and the brief when there is one.
withBrief :: T.Text -> T.Text -> T.Text
withBrief prompt brief
  | T.null (T.strip brief) = prompt
  | otherwise = prompt <> "\n\n" <> brief

-- | A model call as the graph search takes it: the reply's text and its
-- dollars (none reported is 0); 402 is no credit, any other error a failed call.
askResult :: Either ModelError Reply -> Either AskError (T.Text, Double)
askResult = \case
  Right rp -> Right (rpText rp, fromMaybe 0 (uCost (rpUsage rp)))
  Left NoCredit -> Left AskNoCredit
  Left e -> Left (AskFailed (case e of
    Refused why -> "refused: " <> why
    Empty -> "empty reply"
    OtherError why -> why
    OverBudget why -> why))

-- | 'askResult' for the judge: a reply whose cost is not known is charged
-- 'judgeCostEstimate', so an unpriced route still meets the dollar caps.
judgeAskResult :: Either ModelError Reply -> Either AskError (T.Text, Double)
judgeAskResult = \case
  Right rp -> Right (rpText rp, fromMaybe judgeCostEstimate (uCost (rpUsage rp)))
  e -> askResult e

-- | The planned native child with the value after its @-m@ (or @--model@)
-- replaced; an error when it has none.
setModel :: T.Text -> [T.Text] -> Either T.Text [T.Text]
setModel m = go
  where
    go = \case
      f : _ : r | f `elem` ["-m", "--model"] -> Right (f : m : r)
      x : r -> (x :) <$> go r
      [] -> Left "the planned native child has no -m to replace"

-- | The native backend.
isNative :: T.Text -> Bool
isNative = (== nativeBackend)

-- | The native agent's model in config.yaml (@model@, or its @default@),
-- else the agent's own default.
configuredModel :: PyVal -> T.Text
configuredModel cfg = case get "default" (modelConfig cfg) of
  Just (PStr m) | not (T.null (T.strip m)) -> T.strip m
  _ -> "anthropic/claude-opus-4.6"

-- | What the baseline child is told.
singlePrompt :: FilePath -> T.Text
singlePrompt file = T.unwords
  [ "This is an unattended run: nobody will answer questions or approve anything, so never stop to ask."
  , "Prove every theorem in " <> T.pack file <> " whose proof is sorry, one after another, without changing any statement;"
  , "you may add helper lemmas. Stop when they are all proved or you cannot make progress on the rest." ]

-- | The base environment an attempt is planned in: its backend and, when
-- it has one, its managed state directory (over what either had).
attemptEnv :: T.Text -> Maybe FilePath -> Env -> Env
attemptEnv backend state =
  Map.insert "FISHER_AUTOFORMALIZE_BACKEND" (T.unpack backend)
    . maybe id (Map.insert "FISHER_AUTOFORMALIZE_MANAGED_STATE_DIR") state

-- | The headless child for one backend in one workspace: plan the workflow
-- there as `fisher launch` would, with the managed state in @state@ and
-- the model, when one is given, as the command's @--model@.
planFor :: LaunchInputs -> FilePath -> Maybe FilePath -> T.Text -> T.Text -> FilePath -> Maybe T.Text -> IO (Either T.Text ([T.Text], FilePath, Env))
planFor li0 ws state backend workflow rel model = do
  hasProject <- doesDirectoryExist (ws </> fisherDirName)
  unless hasProject (void (initializeProject ws defaultInitOptions {initName = Just "search"}))
  let li = li0 {liBaseEnv = attemptEnv backend state (liBaseEnv li0), liActiveCwd = Just ws}
  r <- resolveWorkflowRequest li (modelCommand model (workflowCommand workflow rel))
  pure $ case r of
    Left e -> Left (errorMessage e)
    Right p -> let q = wpRequest p in Right (hrArgv q, hrCwd q, hrEnv q)

-- ------------------------------------------------------------- the run log

-- | A search's run log: the log (Nothing when it could not be opened, and
-- then nothing is logged), the flag that a write failed (after which
-- nothing more is tried, 'runlogGuard'), the search run's id and start,
-- the attempts' ids, the search's start (the events' clock), whether the
-- run was ended, and what the events tell of the attempts: those started
-- and not yet ended, each one's stop, and the goals closed or disproved.
data SearchLog = SearchLog
  { slRunlog :: Maybe Runlog
  , slWarned :: IORef Bool
  , slParent :: T.Text
  , slStartedAt :: Double
  , slTagOf :: Int -> T.Text
  , slSearchStart :: IO Double
  , slEnded :: IORef Bool
  , slOpen :: IORef (Set.Set Int)
  , slStops :: IORef (Map.Map Int Stop)
  , slSettled :: IORef (Set.Set T.Text)
  }

-- | The end of a search for the run log: its attempts, its goals, its
-- outcome and its dollars.
type Finish = [AttemptRecord] -> [GoalRow] -> T.Text -> Maybe Double -> IO ()

-- | Open the Fisher home's run log and start the search's run there,
-- given the attempts' ids and the search's start. A failure is @fisher:
-- run log: <msg>@ on stderr, once, and the search goes on without the log.
openSearchLog :: FilePath -> (Int -> T.Text) -> IO Double -> NewRun -> IO SearchLog
openSearchLog home tagOf searchStart parent = do
  warned <- newIORef False
  ended <- newIORef False
  open' <- newIORef Set.empty
  stops <- newIORef Map.empty
  settled <- newIORef Set.empty
  opened <- try @SomeException (openRunlog (runlogPath home))
  rl <- case opened of
    Left e
      | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
      | otherwise -> runlogWarn warned (T.pack (displayException e)) >> pure Nothing
    Right (Left e) -> runlogWarn warned e >> pure Nothing
    Right (Right r) -> pure (Just r)
  let sl = SearchLog rl warned (nrId parent) (nrStartedAt parent) tagOf searchStart ended open' stops settled
  searchLogged sl (`startRun` parent)
  pure sl

-- | Whether the log is open.
searchLogOpen :: SearchLog -> Bool
searchLogOpen = isJust . slRunlog

-- | A write to the log, if it is open and no write has failed; a failure
-- is one line on stderr and never reaches the search.
searchLogged :: SearchLog -> (Runlog -> IO ()) -> IO ()
searchLogged sl k = forM_ (slRunlog sl) (\rl -> runlogGuard (slWarned sl) (k rl))

-- | Start an attempt's run (its number, its row) with an @attempt@ event
-- (its data, 'attemptEventData'); whether the row was written, so the
-- child is told its id only then.
searchLogBegin :: SearchLog -> Int -> NewRun -> A.Value -> IO Bool
searchLogBegin sl i nr d = do
  atomicModifyIORef' (slOpen sl) (\x -> (Set.insert i x, ()))
  searchLogged sl (\rl -> startRun rl nr >> event rl (nrId nr) 0 "attempt" d)
  failed <- readIORef (slWarned sl)
  pure (searchLogOpen sl && not failed)

-- | The search's @portfolio@ event, @{tactics}@, on its run at its start.
searchLogPortfolio :: SearchLog -> [T.Text] -> IO ()
searchLogPortfolio sl ts = searchLogged sl (\rl -> event rl (slParent sl) 0 "portfolio" (object ["tactics" .= ts]))

-- | The @attempt@ event's data: the goal, its shape (from the statement;
-- @""@ for an empty one, an attempt of no known statement, which the
-- statistics skip), the backend and, when it is known, the budget in
-- seconds it runs at (@budget_secs@; the policy's budget is computed from
-- the closes at the configured one).
attemptEventData :: T.Text -> T.Text -> T.Text -> Maybe Int -> A.Value
attemptEventData goal stmt backend budget =
  object (["goal" .= goal, "shape" .= shapeOfKnown stmt, "backend" .= backend] <> ["budget_secs" .= b | Just b <- [budget]])

-- | The shape of a statement, @""@ for none.
shapeOfKnown :: T.Text -> T.Text
shapeOfKnown stmt = if T.null (T.strip stmt) then "" else renderShape (shapeOf stmt)

-- | A goal's statement, else its name.
statementOf :: [(T.Text, T.Text)] -> T.Text -> T.Text
statementOf stmts g = fromMaybe g (lookup g stmts)

-- | A goal's statement, else @""@ (the @--single@ baseline's goal @*@, a
-- lemma-graph node: no declaration of the file).
knownStatement :: [(T.Text, T.Text)] -> T.Text -> T.Text
knownStatement stmts g = fromMaybe "" (lookup g stmts)

-- | An event's JSON ('eventJson' or 'graphEventJson') as an event of the
-- search's run, its time moved from the search's start to the run's.
searchLogEvent :: SearchLog -> A.Value -> IO ()
searchLogEvent sl v = do
  s0 <- slSearchStart sl
  let (t, k, d) = eventRow v
  searchLogged sl (\rl -> event rl (slParent sl) (t + s0 - slStartedAt sl) k d)

-- | A search event: written to the search's run, and an attempt ended as
-- soon as the search judges it. @Finished@ ends it with its class (the
-- stop from its @attempt stopped@ event), @cancelled@ ends it as
-- cancelled when its goal was closed, left waiting or disproved (the
-- scheduler cancels the others then) and as budget otherwise (the
-- budget's own cancels), and @budget spent@ ends every attempt still open
-- as budget. The sets are changed atomically: the attempts' threads begin
-- them while the scheduler's thread ends them.
searchLogSearchEvent :: SearchLog -> Event -> IO ()
searchLogSearchEvent sl e = do
  searchLogEvent sl (eventJson e)
  case e of
    AttemptStopped _ i _ why turns d ->
      atomicModifyIORef' (slStops sl) (\x -> (Map.insert i (Stop (if why == "dollars" then StallDollars else StallTurns) turns d) x, ()))
    GoalClosed _ g _ -> settle g
    GoalWaiting _ g _ -> settle g
    GoalDisproved _ g "false" _ _ _ -> settle g
    Finished t i g b o stats d -> do
      stop <- Map.lookup i <$> readIORef (slStops sl)
      endAttemptAt sl ((plainRecord i g b o 0 t stats) {arStopped = stop, arCost = d <|> (stats >>= csCostUsd)})
    AttemptCancelled t i g d -> do
      settled <- Set.member g <$> readIORef (slSettled sl)
      let o = if settled then "cancelled: the goal closed" else "cancelled: budget spent"
      endAttemptAt sl ((plainRecord i g "" o 0 t Nothing) {arCost = d})
    BudgetSpent t -> do
      open' <- Set.toList <$> readIORef (slOpen sl)
      forM_ open' $ \i -> endAttemptAt sl (plainRecord i "" "" "cancelled: budget spent" 0 t Nothing)
    _ -> pure ()
  where
    settle g = atomicModifyIORef' (slSettled sl) (\x -> (Set.insert g x, ()))

-- | A lemma-graph event: written to the search's run, and an attempt
-- ended with its class when it ends (the stop from its @NodeStopped@).
searchLogGraphEvent :: SearchLog -> GraphEvent -> IO ()
searchLogGraphEvent sl e = do
  searchLogEvent sl (graphEventJson e)
  case e of
    NodeStopped _ i _ why turns d ->
      atomicModifyIORef' (slStops sl) (\x -> (Map.insert i (Stop (if why == "dollars" then StallDollars else StallTurns) turns d) x, ()))
    AttemptEnded t i _ o -> do
      stop <- Map.lookup i <$> readIORef (slStops sl)
      endAttemptAt sl ((plainRecord i "" "" o 0 t Nothing) {arStopped = stop})
    _ -> pure ()

-- | On an interrupted search, every attempt started and not ended ends as
-- cancelled, now.
searchLogInterrupted :: SearchLog -> IO ()
searchLogInterrupted sl = do
  s0 <- slSearchStart sl
  now <- clockNow
  open' <- Set.toList <$> readIORef (slOpen sl)
  forM_ open' $ \i -> endAttemptAt sl (plainRecord i "" "" "cancelled: the search was interrupted" 0 (now - s0) Nothing)

-- | End attempt @arId@ from its record ('attemptEnd', its end from the
-- search's start), keeping what the attempt's own end wrote ('keepChildEnd').
endAttemptAt :: SearchLog -> AttemptRecord -> IO ()
endAttemptAt sl a = do
  atomicModifyIORef' (slOpen sl) (\x -> (Set.delete (arId a) x, ()))
  s0 <- slSearchStart sl
  let tag = slTagOf sl (arId a)
  searchLogged sl $ \rl -> do
    row <- getRun rl tag
    endRun rl tag (keepChildEnd row (attemptEnd s0 a))

-- | An attempt's end, keeping the cost, tokens and turns the run's row
-- already holds over the search's. The row's values may be the native
-- child's own end or the search's earlier end of the same attempt (at its
-- @finished@ event); the two cannot be told apart, and today they agree,
-- both coming from the attempt's dollars or its stream's cost. So the
-- final pass is the last word on an attempt's class, exit and end time,
-- and the first written value is kept for its cost, tokens and turns.
keepChildEnd :: Maybe RunRow -> RunEnd -> RunEnd
keepChildEnd row e = case row of
  Nothing -> e
  Just r -> e
    { reCostUsd = rrCostUsd r <|> reCostUsd e
    , reInputTokens = rrInputTokens r <|> reInputTokens e
    , reOutputTokens = rrOutputTokens r <|> reOutputTokens e
    , reTurns = rrTurns r <|> reTurns e }

-- | The search's end, once (a later call does nothing): each attempt's
-- end with its class ('attemptEnd'), the goal rows, and the search's own
-- end. This is the last word on each attempt.
finishSearchLog :: SearchLog -> [AttemptRecord] -> [GoalRow] -> RunEnd -> IO ()
finishSearchLog sl attempts goals end = do
  first <- atomicModifyIORef' (slEnded sl) (\x -> (True, not x))
  when first $ do
    forM_ attempts (endAttemptAt sl)
    searchLogged sl $ \rl -> do
      forM_ goals (upsertGoal rl (slParent sl))
      endRun rl (slParent sl) end

closeSearchLog :: SearchLog -> IO ()
closeSearchLog sl = forM_ (slRunlog sl) (\rl -> void (try @SomeException (closeRunlog rl)))

-- | The goal rows of a finished search, given each declaration's name and
-- statement (none for the @--single@ baseline's goal @*@): the statement
-- (the goal's name when unknown) and its shape (@""@ when unknown, which
-- the statistics skip), the status,
-- who closed it (@auto@, @attempt:<n>@ or @disproved@), the seconds to the
-- close and the dollars the goal's attempts started by then spent (0 with
-- none; Nothing when none of them is priced).
goalRows :: [(T.Text, T.Text)] -> SearchReport -> [GoalRow]
goalRows stmts rep = [row g s | (g, s) <- srGoals rep, g /= "*"]
  where
    row g s =
      let stmt = statementOf stmts g
          at = if gsStatus s == S.Closed then gsAt s else Nothing
       in GoalRow
            { grName = g, grStatement = stmt, grShape = shapeOfKnown (knownStatement stmts g), grStatus = statusName (gsStatus s)
            , grClosedBy = closedBy s, grSecondsToClose = at, grDollarsToClose = at >>= dollars g }
    closedBy s = case gsStatus s of
      S.Disproved -> Just "disproved"
      S.Closed
        | gsClosedBy s == Just "auto" -> Just "auto"
        | Just (i, _) <- gsBy s -> Just ("attempt:" <> T.pack (show i))
      _ -> Nothing
    dollars g at =
      let costs = [arCost a | a <- srAttempts rep, arGoal a == g, arStarted a <= at]
       in if null costs then Just 0 else knownSum costs

-- | The sum of the known values; Nothing when none is known.
knownSum :: [Maybe Double] -> Maybe Double
knownSum xs = case catMaybes xs of
  [] -> Nothing
  ds -> Just (sum ds)

-- | A search event's JSON as a run-log event: its @t@ (0 when absent), its
-- @event@ name as the kind (@event@ when absent) and the object itself.
eventRow :: A.Value -> (Double, T.Text, A.Value)
eventRow v = case v of
  A.Object o ->
    ( case KM.lookup "t" o of
        Just (A.Number n) -> realToFrac n
        _ -> 0
    , case KM.lookup "event" o of
        Just (A.String k) -> k
        _ -> "event"
    , v )
  _ -> (0, "event", v)

-- | A managed backend's stream events with times: an event's own, else
-- the i-th of n at i * wall / n (spread over the attempt's wall time), or
-- 0 when the wall time is unknown.
streamTimes :: Maybe Double -> [StreamEvent] -> [(Double, T.Text, A.Value)]
streamTimes wall evs = zipWith place [1 :: Int ..] evs
  where
    n = length evs
    place i e = (fromMaybe (maybe 0 (\w -> fromIntegral i * w / fromIntegral n) wall) (stT e), stKind e, stData e)

-- | The child's environment with its run in the run log: @FISHER_RUN_ID@
-- (the attempt's id, whose row the parent started) and @FISHER_RUN_PARENT@.
attemptRunEnv :: T.Text -> T.Text -> Env -> Env
attemptRunEnv tag parent = Map.insert "FISHER_RUN_ID" (T.unpack tag) . Map.insert "FISHER_RUN_PARENT" (T.unpack parent)

-- | An attempt's end in the run log, given the search's start: the end,
-- the child's exit, 'outcomeClass', the dollars, tokens and turns.
attemptEnd :: Double -> AttemptRecord -> RunEnd
attemptEnd s0 a =
  let c = arChild a
   in RunEnd (s0 + arEnded a) (c >>= csExit) (outcomeClass a) (arCost a <|> (c >>= csCostUsd))
        (c >>= csInputTokens) (c >>= csOutputTokens) (c >>= csTurns)

-- | A search's outcome: @closed@ when it had goals and closed them all,
-- @budget@ when the budget was spent, else @failed@.
searchOutcome :: SearchReport -> T.Text
searchOutcome rep
  | not (null (srGoals rep)) && all ((== S.Closed) . gsStatus . snd) (srGoals rep) = "closed"
  | srBudgetSpent rep = "budget"
  | otherwise = "failed"

clockNow :: IO Double
clockNow = realToFrac <$> getPOSIXTime
