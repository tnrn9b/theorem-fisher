-- | The lessons' mining pass as the commands run it: the config keys
-- (@fisher.lessons.enabled@, @judge@, @judge_cap_usd@,
-- @judge_day_cap_usd@, @judge_calls@, @max@, @auto_judged@), the 'MineEnv' for a project,
-- the pass after an imported launch run (its @lessons@ event, then the
-- rules only), and the @lessons@ event a search writes on its run. Shared by @fisher search@, the shell and @fisher
-- launch@, so it imports none of them.
module Fisher.Cli.Mine
  ( lessonsConfigOn
  , lessonsJudgeOn
  , judgeCaps
  , judgeCalls
  , lessonsLimits
  , mineEnvFor
  , mineImported
  , lessonsEventData
  , lessonsEventFrom
  , withLessons
  , lessonsWarn
  ) where

import Control.Exception (SomeAsyncException, SomeException, displayException, fromException, throwIO, try)
import Control.Monad (unless, void, when)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Scientific (toBoundedInteger, toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

import Fisher.Config (getPath, lessonsAutoJudged)
import Fisher.Learn.Lessons (Limits (..), defaultLimits)
import Fisher.Learn.Mine (MineEnv (..), mineRun)
import Fisher.Lessons.Prompt (PromptLessons (..), promptLessons)
import Fisher.Role (partText)
import Fisher.Sha256 (sha256Hex)
import Fisher.Lessons.File (fileHash, globalLessonsPath, loadTemplates, projectLessonsPath)
import Fisher.Runlog (Runlog, event, runEvents, runlogPath, withRunlog, projectKeyOf)
import Fisher.Search.GraphJobs (Ask)
import Fisher.Workflow.Assets (checkoutRoot, readCheckoutRevision)

-- | @fisher.lessons.enabled@, on unless @false@.
lessonsConfigOn :: Value -> Bool
lessonsConfigOn cfg = getPath ["fisher", "lessons", "enabled"] cfg /= Just (Bool False)

-- | @fisher.lessons.judge@, on unless @false@.
lessonsJudgeOn :: Value -> Bool
lessonsJudgeOn cfg = getPath ["fisher", "lessons", "judge"] cfg /= Just (Bool False)

-- | The judge's caps in dollars, per run and per day: @judge_cap_usd@
-- (0.25) and @judge_day_cap_usd@ (2.00); a value that is not a number of
-- 0 or more is the default.
judgeCaps :: Value -> (Double, Double)
judgeCaps cfg = (dollars "judge_cap_usd" 0.25, dollars "judge_day_cap_usd" 2.0)
  where
    dollars k d = case getPath ["fisher", "lessons", k] cfg of
      Just (Number n) | n >= 0 -> toRealFloat n
      _ -> d

-- | The judge calls a run may make: @judge_calls@ (20), a whole number of
-- 0 or more; anything else is the default. With an unpriced route the
-- dollar caps see only an estimate, so this bounds the calls themselves.
judgeCalls :: Value -> Int
judgeCalls cfg = case getPath ["fisher", "lessons", "judge_calls"] cfg of
  Just (Number n) | Just m <- toBoundedInteger n, m >= 0 -> m
  _ -> 20

-- | The lifecycle's numbers: the defaults, with @fisher.lessons.max@ (a
-- positive integer) as the cap on a file's lines and
-- @fisher.lessons.auto_judged@ (only @true@ turns it on) promoting judged
-- and self lessons on their counts instead of waiting for a pin.
lessonsLimits :: Value -> Limits
lessonsLimits cfg =
  defaultLimits
    { lMax = case getPath ["fisher", "lessons", "max"] cfg of
        Just (Number n) | Just m <- toBoundedInteger n, m > 0 -> m
        _ -> lMax defaultLimits
    , lAutoJudged = lessonsAutoJudged cfg }

-- | One line on stderr: @fisher: lessons: <msg>@.
lessonsWarn :: Text -> IO ()
lessonsWarn msg = void (try @SomeException (hPutStrLn stderr ("fisher: lessons: " <> T.unpack msg)))

-- | The env of a pass now over a project (its root), its warnings to a
-- writer (given the message, without the prefix), with the Fisher home's
-- templates (none, with a warning, when the override cannot be read), the
-- config's caps and limits, a judge or none, 90 s for one judge call and
-- 300 s for the judge phase of a pass.
mineEnvFor :: (Text -> IO ()) -> Value -> FilePath -> FilePath -> Maybe Ask -> Runlog -> IO MineEnv
mineEnvFor warn cfg home root judge rl = do
  now <- realToFrac <$> getPOSIXTime
  templates <- loadTemplates home >>= either (\e -> warn ("no templates: " <> e) >> pure []) pure
  let (runCap, dayCap) = judgeCaps cfg
  pure
    MineEnv
      { meRunlog = rl, meNow = now, meFisherHome = home, meRoot = root, meProjectKey = projectKeyOf root
      , meJudge = judge, meRunCap = runCap, meDayCap = dayCap, meCallCap = judgeCalls cfg
      , meJudgeTimeout = 90, meJudgeBudget = 300, meLimits = lessonsLimits cfg
      , meTemplates = templates, meWarn = warn }

-- | After a launch run was imported (its id, its project root), when the
-- config has lessons on, @FISHER_LESSONS@ is not @0@ and the run has a
-- project: its @lessons@ event ('lessonsEventData' over the files as they
-- are now, best effort, since the run read them when it started; none
-- when the run already has one), then the rules pass over it; warnings to
-- a writer (as 'mineEnvFor'; 'lessonsWarn' on a terminal of its own, the
-- shell's output in the shell). Never fails the caller: a failure is one
-- warning.
mineImported :: (Text -> IO ()) -> FilePath -> Value -> FilePath -> Text -> IO ()
mineImported warn home cfg root rid = do
  envOff <- (== Just "0") <$> lookupEnv "FISHER_LESSONS"
  when (lessonsConfigOn cfg && not envOff && not (null root)) $
    try (withRunlog (runlogPath home) $ \rl -> do
           guardedW "the lessons event" $ do
             es <- runEvents rl rid
             unless (any (\(_, _, k, _) -> k == "lessons") es) $ do
               assets <- assetsDir home
               lessonsEventData rl cfg home root (Just assets) >>= event rl rid 0 "lessons"
           mineEnvFor warn cfg home root Nothing rl >>= \env -> void (mineRun env rid False))
      >>= either failed pure
  where
    failed e
      | Just (_ :: SomeAsyncException) <- fromException e = throwIO e
      | otherwise = warn (T.pack (displayException (e :: SomeException)))
    guardedW what act = try act >>= \case
      Right () -> pure ()
      Left e
        | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
        | otherwise -> warn (what <> ": " <> T.pack (displayException (e :: SomeException)))

-- The managed assets directory, as a search finds it.
assetsDir :: FilePath -> IO FilePath
assetsDir home = do
  st <- lookupEnv "FISHER_AUTOFORMALIZE_MANAGED_STATE_DIR"
  pure (fromMaybe (home </> "autoformalize") st </> "assets")

-- | The @lessons@ event of a run, from the config, the Fisher home, the
-- project root and the managed assets directory (Nothing: no skill
-- revision), with the block built now ('promptLessons'; for an imported
-- run, which read the files when it started, this is best effort). A
-- search uses 'lessonsEventFrom' with the block its attempts carry.
lessonsEventData :: Runlog -> Value -> FilePath -> FilePath -> Maybe FilePath -> IO Value
lessonsEventData rl cfg home root assets = do
  pl <- promptLessons rl (lessonsAutoJudged cfg) home root
  lessonsEventFrom pl home root assets

-- | The @lessons@ event of a run whose prompts carry this block: the ids
-- of the lessons they are shown and the SHA-256 of the block ('plBlock':
-- the shown rows of runs.db and the files' Yours lines), the two files'
-- hashes (null when absent; the source of the Yours lines) and the Lean
-- skill's revision.
lessonsEventFrom :: PromptLessons -> FilePath -> FilePath -> Maybe FilePath -> IO Value
lessonsEventFrom pl home root assets = do
  ph <- fileHash (projectLessonsPath root)
  gh <- fileHash (globalLessonsPath home)
  rev <- maybe (pure Nothing) (readCheckoutRevision . checkoutRoot) assets
  let block = partText (plBlock pl)
  pure (object [ "ids" .= plIds pl, "block_hash" .= (if T.null block then Nothing else Just (sha256Hex (TE.encodeUtf8 block)))
               , "project_hash" .= ph, "global_hash" .= gh, "skill_revision" .= rev ])

-- | The report's @learn@ block with its @lessons@ object (a block that is
-- not an object becomes one).
withLessons :: Value -> Value -> Value
withLessons learn lessons = case learn of
  Object o -> Object (KM.insert "lessons" lessons o)
  _ -> object ["lessons" .= lessons]
