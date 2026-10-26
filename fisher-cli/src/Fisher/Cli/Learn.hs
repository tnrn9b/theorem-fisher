-- | @fisher learn stats|show|diff|reset@: the learned search policy
-- ("Fisher.Learn.Policy") and the statistics behind it
-- ("Fisher.Learn.Stats"), for a project: @--project P@, else the Lake
-- project the working directory is in (the working directory itself when
-- none), as @fisher search@ finds it.
--
-- * @stats [--shape S]@: per knob (backends, tiers, tactics) a table of
--   arm, attempts, closes, smoothed rate, dollars per close and closes per
--   hour, at the shape's level (with @--shape@) and the project's; the
--   arms in the order the policy would put them, then any others seen.
-- * @show@: the policy file, each shape's orders and budget with its
--   evidence.
-- * @diff@: what recomputing the policy now would change in the file
--   ('policyDiff'); it writes nothing.
-- * @reset@: deletes the policy file (the next search starts from the
--   configured order and recomputes it).
--
-- The policy's bounds are the unflagged configuration's, as the search
-- saves it ('savedDefaults'). Reading never makes a run log that is not
-- there; an empty one prints the import hint.
module Fisher.Cli.Learn
  ( LearnCmd (..)
  , LearnEnv (..)
  , learnParser
  , runLearn
  , projectRootFor
  ) where

import Control.Exception (finally)
import Control.Monad (forM_)
import Data.Aeson (Value (..))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Numeric (showFFloat)
import System.Directory (doesFileExist, removeFile)
import System.FilePath (isAbsolute, normalise, (</>))

import Options.Applicative

import Fisher.Cli.Runs (emptyHint)
import Fisher.Cli.Search (lakeRoot, policyShapes, savedDefaults)
import Fisher.Learn.Policy
import Fisher.Learn.Stats
import Fisher.Runlog

data LearnCmd
  = LStats (Maybe FilePath) (Maybe Text)
  -- ^ project, shape
  | LShow (Maybe FilePath)
  | LDiff (Maybe FilePath)
  | LReset (Maybe FilePath)
  deriving (Eq, Show)

-- | The run log, the Fisher home (where the policy files are), the
-- working directory, the configuration (for the tiers), the clock, how
-- to print.
data LearnEnv = LearnEnv
  { leRunlogPath :: FilePath
  , leFisherHome :: FilePath
  , leCwd :: FilePath
  , leConfig :: Value
  , leNow :: IO Double
  , lePut :: Text -> IO ()
  }

-- | The project's root: the given path (from the working directory), else
-- the Lake project above the working directory, else the working
-- directory.
projectRootFor :: FilePath -> Maybe FilePath -> IO FilePath
projectRootFor cwd = \case
  Just p -> pure (normalise (if isAbsolute p then p else cwd </> p))
  Nothing -> fromMaybe cwd <$> lakeRoot (cwd </> ".")

runLearn :: LearnEnv -> LearnCmd -> IO ()
runLearn env = \case
  LStats project shape -> do
    root <- projectRootFor (leCwd env) project
    let key = projectKeyOf root
    put ("project " <> T.pack root <> " (" <> key <> ")")
    withLog $ \case
      Nothing -> put tableHeader >> put emptyHint
      Just rl -> do
        anyRun <- listRuns rl Nothing Nothing Nothing 1
        if null anyRun
          then put tableHeader >> put emptyHint
          else do
            st <- loadStats rl key
            at <- leNow env
            let levels = [("shape " <> s, s, LShape) | Just s <- [shape]] <> [("project", "*", LProject)]
                pol = computePolicy at key defaults st (map (\(_, s, _) -> s) levels)
            forM_ [minBound .. maxBound] $ \knob ->
              forM_ levels $ \(label, sh, lvl) -> do
                put ""
                put (renderKnob knob <> " (" <> label <> ")")
                put tableHeader
                case statsTable st lvl sh knob (knobArms knob (policyFor pol sh)) of
                  [] -> put "  (none)"
                  rows -> forM_ rows (put . statsRow)
  LShow project -> do
    root <- projectRootFor (leCwd env) project
    let path = policyPath (leFisherHome env) (projectKeyOf root)
    readPolicy path >>= \case
      Left e -> put ("fisher learn: " <> e)
      Right Nothing -> put "no policy for this project"
      Right (Just p) -> do
        put ("policy " <> T.pack path <> " (computed " <> date (pComputedAt p) <> ")")
        forM_ (Map.toList (pShapes p)) $ \(sh, gp) -> do
          put ""
          put ("shape " <> sh)
          put ("  backends " <> list (gpBackends gp))
          put ("  tiers " <> list (gpTiers gp))
          put ("  tactics " <> list (gpTactics gp))
          put ("  attempt_secs " <> maybe "the configured budget" (T.pack . show) (gpAttemptSecs gp))
          mapM_ put (evidenceLines (gpEvidence gp))
  LDiff project -> do
    root <- projectRootFor (leCwd env) project
    let key = projectKeyOf root
        path = policyPath (leFisherHome env) key
    prev <- readPolicy path >>= \case
      Left e -> put ("fisher learn: ignoring " <> e) >> pure Nothing
      Right p -> pure p
    st <- withLog $ \case
      Nothing -> pure emptyStats
      Just rl -> loadStats rl key
    at <- leNow env
    let new = computePolicy at key defaults st (policyShapes [] st prev)
    case policyDiff prev new of
      [] -> put "no changes"
      changes -> forM_ changes $ \(sh, knob, o, n) -> put (sh <> " " <> knob <> ": " <> json o <> " -> " <> json n)
  LReset project -> do
    root <- projectRootFor (leCwd env) project
    let path = policyPath (leFisherHome env) (projectKeyOf root)
    there <- doesFileExist path
    if there
      then removeFile path >> put ("removed " <> T.pack path)
      else put "no policy"
  where
    put = lePut env
    defaults = savedDefaults (leConfig env)
    -- the log when there is one (never made here); one that cannot be
    -- opened is one line and reads as absent
    withLog :: (Maybe Runlog -> IO a) -> IO a
    withLog k = do
      there <- doesFileExist (leRunlogPath env)
      if not there
        then k Nothing
        else
          openRunlog (leRunlogPath env) >>= \case
            Left e -> put ("fisher learn: " <> e) >> k Nothing
            Right rl -> k (Just rl) `finally` closeRunlog rl

tableHeader :: Text
tableHeader = row ["arm", "attempts", "closes", "rate", "$/close", "closes/h"]

row :: [Text] -> Text
row cols = T.stripEnd ("  " <> T.intercalate "  " (zipWith (\w c -> T.justifyLeft w ' ' c) [24, 9, 7, 6, 8, 0] cols))

-- An arm's row: its cell's attempts and closes, and the smoothed rate,
-- dollars per close and closes per hour of the entry.
statsRow :: ArmRow -> Text
statsRow a =
  row [arArm a, tshow (arAttempts a), tshow (arCloses a), fixed 2 (arRate a), maybe "-" (fixed 2) (arCostPerClose a), fixed 1 (arClosesPerHour a)]

-- The evidence of a shape's entry: per knob and arm, attempts, closes,
-- rate and dollars per close.
evidenceLines :: Value -> [Text]
evidenceLines = \case
  Object knobs ->
    [ "  evidence " <> K.toText k <> " " <> K.toText arm <> ": " <> num "attempts" e 0 <> " attempts, " <> num "closes" e 0
        <> " closes, rate " <> num "rate" e 2 <> ", $/close " <> num "cost_per_close" e 2
    | (k, Object arms) <- KM.toList knobs
    , (arm, e) <- KM.toList arms ]
  _ -> []
  where
    num f e digits = case e of
      Object o | Just (Number x) <- KM.lookup f o -> fixed digits (toRealFloat x)
      _ -> "-"

list :: [Text] -> Text
list = \case
  [] -> "(none)"
  xs -> T.intercalate ", " xs

json :: Value -> Text
json = TE.decodeUtf8 . LBS.toStrict . A.encode

date :: Double -> Text
date t = T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M" (posixSecondsToUTCTime (realToFrac t)))

fixed :: Int -> Double -> Text
fixed k x = T.pack (showFFloat (Just k) x "")

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | @fisher learn@'s arguments.
learnParser :: Parser LearnCmd
learnParser = hsubparser
  ( command "stats"
      (info (LStats <$> project
                    <*> optional (T.pack <$> strOption (long "shape" <> metavar "S" <> help "Also the table at this goal shape")))
            (progDesc "The arms' statistics per knob: attempts, closes, rate, $/close, closes/h"))
 <> command "show" (info (LShow <$> project) (progDesc "The project's learned policy, with its evidence per shape"))
 <> command "diff" (info (LDiff <$> project) (progDesc "What recomputing the policy now would change"))
 <> command "reset" (info (LReset <$> project) (progDesc "Delete the project's policy file")) )
  where
    project = optional (strOption (long "project" <> metavar "P" <> help "The project's root (default: the Lake project of the working directory)"))
