-- | Fisher.Cron against upstream's cron/jobs.py and croniter 6.0.0
-- (@test/golden/e2/schedules.json@ and @croniter.json@, from
-- tools/corpus/e2.py): every schedule parsed, its next run, and the first
-- six fire times of each expression from each start, in three zones.
module Fisher.CronSpec (spec) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.Map.Strict as M
import Numeric (showHex)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Fisher.Cron.Croniter
import Fisher.Cron.Dt
import Fisher.Cron.Jobs
import Fisher.Cron.Tool
import Fisher.Cron.Zone
import Fisher.Py.Value

lst :: Text -> PyVal -> [PyVal]
lst k c = case get k c of
  Just (PList xs) -> xs
  _ -> []

txt :: Text -> PyVal -> Text
txt k c = maybe "" pyStrOf (get k c)

zone :: Text -> IO Zone
zone n = fromMaybe (error ("no zone " <> T.unpack n)) <$> loadZone "/usr/share/zoneinfo" n

-- | An instant (an ISO string with an offset) in a zone, as
-- @datetime.fromisoformat(s).astimezone(ZoneInfo(zone))@.
inZone :: Zone -> Text -> Dt
inZone z s = case fromIsoformat s of
  Right d -> astimezone (TzZone z Nothing) d
  Left e -> error (T.unpack e)

result :: Either Text PyVal -> PyVal
result = \case
  Right v -> PDict [("ok", v)]
  Left e -> PDict [("error", PStr e)]

nextVal :: Either Text (Maybe Text) -> PyVal
nextVal = \case
  Right n -> PDict [("ok", maybe PNull PStr n)]
  Left e -> PDict [("error", PStr e)]

stripType :: PyVal -> PyVal
stripType = \case
  PDict kvs -> PDict [kv | kv@(k, _) <- kvs, k /= "type"]
  v -> v

spec :: Spec
spec = do
  sched <- runIO (either fail pure . loads =<< BS.readFile "test/golden/e2/schedules.json")
  describe "cron/jobs.py parse_duration" $
    forM_ (lst "durations" sched) $ \c -> it (show (txt "input" c)) $
      stripType (fld "result" c) `shouldBe` (case parseDuration (txt "input" c) of
        Right m -> PDict [("ok", PInt m)]
        Left e -> PDict [("error", PStr e)])
  describe "cron/jobs.py parse_schedule and compute_next_run" $
    forM_ (lst "cases" sched) $ \c -> it (T.unpack (txt "zone" c <> " " <> txt "now" c <> " " <> T.pack (show (txt "input" c)))) $ do
      z <- zone (txt "zone" c)
      let env = CronEnv "/nonexistent" (pure (inZone z (txt "now" c))) (pure 0) (pure "0")
      parsed <- parseSchedule env (txt "input" c)
      result parsed `shouldBe` stripType (fld "parse" c)
      case (parsed, get "next" c) of
        (Right s, Just want) -> do
          n <- computeNextRun env s Nothing
          nextVal n `shouldBe` stripType want
          case get "next_after" c of
            Just (PDict kvs) -> forM_ kvs $ \(lastRun, w) -> do
              n2 <- computeNextRun env s (Just lastRun)
              nextVal n2 `shouldBe` stripType w
            _ -> pure ()
        _ -> pure ()
  cron <- runIO (either fail pure . loads =<< BS.readFile "test/golden/e2/croniter.json")
  describe "croniter get_next" $
    forM_ (lst "cases" cron) $ \c -> it (T.unpack (txt "zone" c <> " " <> txt "start" c <> " " <> txt "expr" c)) $ do
      z <- zone (txt "zone" c)
      let start = inZone z (txt "start" c)
          go 0 _ = Right []
          go k i = do
            (d, i') <- getNext i
            (isoformat d :) <$> go (k - 1 :: Int) i'
          got = do
            ex <- expand (txt "expr" c)
            go 6 (newIter ex start)
      case got of
        Right fires -> PList (map PStr fires) `shouldBe` fld "fires" c
        Left (CronError _ e) -> PStr e `shouldBe` fld "error" c
  describe "croniter errors" $
    forM_ (lst "errors" cron) $ \c -> it (T.unpack (txt "expr" c)) $
      case expand (txt "expr" c) of
        Right _ -> get "ok" c `shouldBe` Just (PBool True)
        Left (CronError ty e) -> (PStr ty, PStr e) `shouldBe` (fld "type" c, fld "error" c)
  tool <- runIO (either fail pure . loads =<< BS.readFile "test/golden/e2/tool.json")
  describe "tools/cronjob_tools.py cronjob" $
    forM_ (lst "cases" tool) $ \c -> it (T.unpack (txt "name" c)) $ withSystemTempDirectory "fisher-cron" $ \home -> do
      ids <- newIORef (0 :: Integer)
      envVars <- newIORef M.empty
      let env = testEnv home ids
      forM_ (lst "steps" c) $ \st -> case get "env" st of
        Just (PDict kvs) -> modifyIORef envVars (M.union (M.fromList [(k, pyStrOf v) | (k, v) <- kvs]))
        _ -> do
          r <- cronjob env (\k -> M.lookup k <$> readIORef envVars) (fld "call" st)
          PStr r `shouldBe` fld "result" st
          jobsNow <- readJobs home
          jobsNow `shouldBe` fld "jobs" st
  where
    fld k c = fromMaybe PNull (get k c)

-- | The recorder's clock and uuid counter.
testEnv :: FilePath -> IORef Integer -> CronEnv
testEnv home ids =
  CronEnv
    { ceHome = home
    , ceNow = pure (inZone utcZone "2026-10-02T04:50:07.123456+00:00")
    , ceLocalOffset = pure 0
    , ceNewHex = do
        n <- atomicModifyIORef' ids (\k -> (k + 1, k + 1))
        pure (T.justifyRight 32 '0' (T.pack (showHex (((0xC0FFEE000000 + n) * 2 ^ (80 :: Int)) + 0xABCDEF) ""))) }

readJobs :: FilePath -> IO PyVal
readJobs home = do
  let f = home </> "cron" </> "jobs.json"
  ex <- doesFileExist f
  if ex then either fail pure . loads =<< BS.readFile f else pure PNull
