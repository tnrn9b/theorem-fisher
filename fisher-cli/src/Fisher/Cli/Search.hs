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
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent (myThreadId, throwTo)
import Control.Exception (SomeAsyncException, SomeException, bracket, finally, fromException, handle, throwIO, try)
import System.Posix.Temp (mkdtemp)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (Handler (Catch), installHandler, sigTERM)
import Control.Monad (unless, void, when)
import Data.Aeson (object, (.=))
import qualified Data.Aeson as A
import Data.Char (isSpace)
import Data.Foldable (toList)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types (Pair)
import Data.IORef
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, isJust)
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
import Fisher.Search.Auto (AutoConfig (..), Lsp (..), defaultAutoConfig)
import Fisher.Search.Check
import Fisher.Search.Disprove (Disproof (..))
import Fisher.Search.Facts (FactsConfig (..), defaultFacts)
import Fisher.Search.Index (Index (..), buildIndex)
import Fisher.Search.Library (libraryDir, libraryKey, loadLibraryReport, peekLibrary)
import Fisher.Search.Retrieval
import Fisher.Search.Child (ChildStats (..), runChild, streamStats)
import Fisher.Search.Controller (StallRule (..), Stop (..), defaultStallRule, stopReasonText)
import Fisher.Search.Graph (Graph (..), Node (..), NodeId)
import Fisher.Search.GraphRun (AskError (..), GraphConfig (..), GraphEnv (..), GraphEvent (..), GraphReport (..), plannerModel, runGraph)
import Fisher.Search.Headless
import Fisher.Search.Lean (Decl (..), openGoals)
import Fisher.Search.Report
import Fisher.Search.Scheduler hiding (GoalStatus (..))
import qualified Fisher.Search.Scheduler as S
import Fisher.Search.Splice (SpliceError)
import Fisher.Search.Verify
import Fisher.Search.Workspace (makeWorkspace)
import Fisher.Workflow.Handoff (HandoffRequest (..))
import Fisher.Workflow.Plan (LaunchInputs (..), WorkflowPlan (..), resolveWorkflowRequest)
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
  }
  deriving (Eq, Show)

defaultRunArgs :: RunArgs
defaultRunArgs = RunArgs Nothing ["claude-code", nativeBackend] 4 7200 1200 40 3 True Nothing "prove" False [] Nothing False
                         False False 3 2 25 Nothing False True True True Nothing Nothing Nothing 600
                         True True (fcMax defaultFacts) (fcChars defaultFacts) Nothing False False
                         Nothing True (srTurns defaultStallRule) (fromMaybe 1.5 (srDollars defaultStallRule)) True

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
    , "                          [--no-early-stop]"
    , "       fisher search FILE --graph [--max-depth 3] [--max-splits 2] [--cap-usd 25] [--model MODEL]"
    , "                          [--backends fisher] [--parallel N] [--budget 2h] [--attempt-minutes 20]"
    , "                          [--max-turns 40] [--native-decide] [--allow AXIOM]... [--out DIR] [--apply]"
    , "                          [--tiers M1,M2,...] [--no-cascade] [--stall-turns 8] [--stall-dollars 1.50]"
    , "                          [--no-early-stop]"
    , "       both take [--auto | --no-auto] [--no-repair] [--no-disprove] [--auto-tactics A,B,...] [--auto-seconds 60]"
    , "                 [--repair-seconds 120] [--auto-warmup-seconds 600]"
    , "                 [--no-retrieval] [--no-library] [--facts N] [--facts-chars N] [--library-dir DIR]"
    , "                 [--library-read-only]"
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
  "--attempt-minutes" : v : r | Just d <- duration (v <> "m") -> parseRun r a {raAttemptSecs = round d}
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
attemptTag runDir runId n =
  "search-" <> T.pack (takeFileName (dropTrailingPathSeparator runDir)) <> "-" <> runId <> "-" <> T.pack (show n)

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
  , scUntiered = filter (not . isNative) (raBackends ra) }

-- | The automation's settings, or Nothing with @--no-auto@ (today's search).
autoConfig :: RunArgs -> Maybe AutoConfig
autoConfig ra
  | not (raAuto ra) = Nothing
  | otherwise =
      Just defaultAutoConfig
        { acTactics = fromMaybe (acTactics defaultAutoConfig) (raAutoTactics ra)
        , acSeconds = fromMaybe (acSeconds defaultAutoConfig) (raAutoSeconds ra)
        , acRepair = raRepair ra
        , acDisprove = raDisprove ra
        , acRepairSeconds = fromMaybe (acRepairSeconds defaultAutoConfig) (raRepairSeconds ra) }

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
      pure (Just (RetrievalEnv ix (factsConfig ra) lib (not (readOnly || raLibraryReadOnly ra)) dir root runDir retrievalCutoff), evs0 <> evs1)

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
runMain ra file0 = do
  termToException
  file <- makeAbsolute file0
  root <- lakeRoot file >>= maybe (hPutStrLn stderr ("no lakefile above " <> file) >> exitWith (ExitFailure 2)) pure
  let rel = makeRelative root file
  original <- readUtf8 file
  stamp <- formatTime defaultTimeLocale "%Y%m%d-%H%M%S" <$> getZonedTime
  home <- getHomeDirectory
  let runDir = fromMaybe (home </> ".cache" </> "fisher" </> "search" </> stamp) (raOut ra)
      baseWs = runDir </> "base"
  createDirectoryIfMissing True runDir
  makeWorkspace root baseWs rel original
  li0 <- liveInputs
  let m = liMachine li0
      casc = cascadeFor ra (searchTiersConfig (liConfig li0))
      mainState = fromMaybe (mFisherHome m </> "autoformalize") (Map.lookup "FISHER_AUTOFORMALIZE_MANAGED_STATE_DIR" (liBaseEnv li0))
  -- stage the workflow assets once, where every attempt copies them from
  prep <- planFor li0 baseWs Nothing (fromMaybe (case raBackends ra of { b : _ -> b; [] -> "claude-code" }) (raSingle ra)) (raWorkflow ra) rel Nothing
  case prep of
    Left e -> hPutStrLn stderr ("(>_<) " <> T.unpack e) >> exitWith (ExitFailure 1)
    Right _ -> pure ()
  counter <- newIORef (0 :: Int)
  runId <- newRunId
  -- each ledger-watched attempt's tag and start, to price it if cancelled
  ledgerOf <- newIORef (Map.empty :: Map.Map Int (T.Text, Double))
  -- the run's start for the watcher's events, set again as the search starts
  runStart <- newIORef =<< (realToFrac <$> getPOSIXTime :: IO Double)
  eventsH <- openFile (runDir </> "events.jsonl") WriteMode
  hSetBuffering eventsH LineBuffering
  let usesNative = "native_decide" `T.isInfixOf` original
      pol = AxiomPolicy (Set.union standardAxioms (Set.fromList (raAllow ra))) (raNative ra || usesNative)
      logE e = do
        BL8.hPutStrLn eventsH (A.encode (eventJson e))
        TIO.putStrLn (eventLine e)
        hFlush stdout
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
        planned <- planFor li0 ws (Just state) (asBackend spec) (raWorkflow ra) rel (if native then Nothing else asModel spec)
        case planned of
          Left e -> pure (plainResult Nothing Nothing ("plan failed: " <> e))
          Right (argv0, cwd', env) -> case (if native then maybe Right setModel (asModel spec <|> cModel casc) argv0 else Right argv0) of
           Left e -> pure (plainResult Nothing Nothing e)
           Right argv -> do
            let extra = if asGoal spec == "*" then singlePrompt rel else withBrief (attemptPrompt (asGoal spec) rel) (asBrief spec)
            case headlessArgv (asBackend spec) argv extra (raMaxTurns ra) of
              Left e -> pure (plainResult Nothing Nothing e)
              Right hargv -> do
                let secs = if asGoal spec == "*" then ceiling (raBudget ra) else raAttemptSecs ra
                    tag = attemptTag runDir runId (asId spec)
                    child = runChild (map T.unpack hargv) cwd' (Map.toList (ledgerEnv (watching && native) tag env)) secs dir
                    readOut = do
                      out <- try @SomeException (readUtf8 (ws </> rel))
                      BS.writeFile (dir </> "final.lean") (either (const "") TE.encodeUtf8 out)
                      pure (either (const Nothing) Just out)
                    note stats = maybe "killed at the time limit" (\c -> "exit " <> T.pack (show c)) (csExit stats)
                if not watching
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
      cfg = searchConfigFor ra casc
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
      pure ((\l -> Just (AutoEnv ac l, secs)) <$> r)
    _ -> pure (Right Nothing)
  -- the index, built once for the run (not for --single, which has no briefs)
  (retrieval, retrievalEvents) <-
    if retrievalWanted ra && raSingle ra == Nothing then buildRetrieval ra root runDir else pure (Nothing, [])
  writeIORef runStart =<< (realToFrac <$> getPOSIXTime)
  let env1 = env0 {seAuto = either (const Nothing) (fmap fst) started, seRetrieval = retrieval}
      offAtStart = either (\e -> Just ("the Lean server did not start: " <> e)) (const Nothing) started
      autoExtra = case autoConfig ra of
        Nothing -> const []
        Just _ -> \(ran, during) ->
          autoStateJson (offAtStart <|> ("--single runs no automation" <$ raSingle ra)) ran during
            <> either (const []) (maybe [] (\(_, w) -> ["auto_warmup_s" .= (fromIntegral (round (w * 10) :: Int) / 10 :: Double)])) started
  ( if raGraph ra
      then graphMain ra casc file root rel runDir eventsH env1 offAtStart retrievalEvents autoExtra original
      else swarmMain ra file root rel runDir eventsH env1 cfg offAtStart retrievalEvents autoExtra original )
    `finally` mapM_ (lspClose . aeLsp) (seAuto env1)

swarmMain :: RunArgs -> FilePath -> FilePath -> FilePath -> FilePath -> Handle -> SearchEnv -> SearchConfig -> Maybe T.Text
          -> [Event] -> ((Bool, Maybe T.Text) -> [Pair]) -> T.Text -> IO ()
swarmMain ra file root rel runDir eventsH env0 cfg offAtStart startEvents autoExtra original = do
  mapM_ (seLog env0 . AutoOff 0) offAtStart
  mapM_ (seLog env0) startEvents
  r <- case raSingle ra of
    Just b -> runSingle env0 b original
    Nothing -> runSearch cfg env0 original
  hClose eventsH
  case r of
    Left e -> hPutStrLn stderr ("(>_<) " <> T.unpack e) >> exitWith (ExitFailure 1)
    Right rep -> do
      let extra =
            [ "file" .= rel, "project" .= root, "mode" .= maybe ("swarm" :: T.Text) ("single " <>) (raSingle ra)
            , "backends" .= raBackends ra, "slots" .= raSlots ra, "budget_s" .= raBudget ra
            , "attempt_s" .= raAttemptSecs ra, "max_turns" .= raMaxTurns ra ]
            <> autoExtra (srAutoRan rep, srAutoOff rep)
      BL8.writeFile (runDir </> "report.json") (encodePretty (reportJson extra rep))
      BS.writeFile (runDir </> "base.lean") (TE.encodeUtf8 (srBase rep))
      when (raApply ra) $ BS.writeFile file (TE.encodeUtf8 (srBase rep))
      when (srAuto rep) $ mapM_ TIO.putStrLn (goalTable rep)
      TIO.putStrLn $ T.pack (show (srClosed rep)) <> " of " <> T.pack (show (length (srGoals rep))) <> " goals closed in "
        <> T.pack (show (round (srElapsed rep) :: Int)) <> " s; report " <> T.pack (runDir </> "report.json")
        <> (if raApply ra then "; written to " <> T.pack file else "; merged file " <> T.pack (runDir </> "base.lean"))

-- | The lemma-graph run: the planner and verifier on the native agent's
-- provider, the graph and whiteboards saved after every job, its events
-- beside the attempts'.
graphMain :: RunArgs -> Cascade -> FilePath -> FilePath -> FilePath -> FilePath -> Handle -> SearchEnv -> Maybe T.Text
          -> [Event] -> ((Bool, Maybe T.Text) -> [Pair]) -> T.Text -> IO ()
graphMain ra casc file root rel runDir eventsH env0 offAtStart startEvents autoExtra original = do
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
    Left e -> hPutStrLn stderr ("(>_<) " <> T.unpack e) >> exitWith (ExitFailure 1)
    Right rep -> do
      let g = grGraph rep
          base = grBase rep
          extra =
            [ "file" .= rel, "project" .= root, "mode" .= ("graph" :: T.Text)
            , "backends" .= raBackends ra, "slots" .= raSlots ra, "budget_s" .= raBudget ra
            , "attempt_s" .= raAttemptSecs ra, "max_turns" .= raMaxTurns ra
            , "max_depth" .= raMaxDepth ra, "max_splits" .= raMaxSplits ra, "cap_usd" .= raCapUsd ra, "model" .= modelName ]
            <> autoExtra (grAutoRan rep, grAutoOff rep)
          left = sorryLemmas g base
          applied = raApply ra && null left
          merged = T.pack (runDir </> "base.lean")
      BL8.writeFile (runDir </> "report.json") (encodePretty (graphReportJson extra rep))
      BS.writeFile (runDir </> "base.lean") (TE.encodeUtf8 base)
      when applied $ BS.writeFile file (TE.encodeUtf8 base)
      mapM_ (mapM_ TIO.putStrLn . graphTree g) (gRoots g)
      when (raApply ra && not applied) $
        TIO.putStrLn ("not applied: " <> T.pack (show (length left)) <> " lemmas from splits are still sorry; merged file " <> merged)
      TIO.putStrLn $ T.pack (show (grClosedRoots rep)) <> " of " <> T.pack (show (length (gRoots g))) <> " goals closed in "
        <> T.pack (show (round (grElapsed rep) :: Int)) <> " s, $" <> T.pack (showFFloat (Just 2) (sum (Map.elems (grDollars rep))) "")
        <> maybe "" (\w -> " (stopped: " <> w <> ")") (grStopped rep)
        <> "; report " <> T.pack (runDir </> "report.json")
        <> (if applied then "; written to " <> T.pack file else if raApply ra then "" else "; merged file " <> merged)

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
