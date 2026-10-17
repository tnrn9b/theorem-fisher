-- | @fisher ui --prune DAYS@ and where the server finds the repository for
-- the gates.
module Fisher.UI.PruneSpec (spec) where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.IORef
import Data.List (sort)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Data.Text (Text)
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.SQLite.Simple
import System.Directory
import System.FilePath ((</>))
import System.IO (hGetLine)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, terminateProcess, waitForProcess)
import Test.Hspec

import Fisher.Agent.Inbox (inboxDir)
import Fisher.Agent.RunLock (runLockPath)
import Fisher.Agent.Steps (stepsDir)
import Fisher.UI.FakeFisher (ledgerSchema)
import Fisher.UI.Prune
import Fisher.UI.Server (findRepo, uiMain)

-- | Make a file and set its time this many days ago.
oldFile :: Double -> FilePath -> IO ()
oldFile days p = do
  writeFile p "x\n"
  t <- getCurrentTime
  setModificationTime p (addUTCTime (realToFrac (negate days * 86400)) t)

spec :: Spec
spec = do
  describe "fisher ui --prune" $
    it "deletes the ledger rows, step and inbox files of old sessions not running, and old run logs; keeps the rest" $
      withSystemTempDirectory "fisher-ui-prune" $ \tmp -> do
        let home = tmp </> "gauss"
            sd = stepsDir home
            ib = inboxDir home
            logs = home </> "fisher" </> "ui-runs"
        mapM_ (createDirectoryIfMissing True) [sd, ib, logs]
        c <- open (home </> "state.db")
        mapM_ (execute_ c . Query) ledgerSchema
        -- old: 40 days; fresh: now; running: old but its lock held
        nowS <- realToFrac <$> getPOSIXTime :: IO Double
        forM_ [("old", nowS - 40 * 86400), ("fresh", nowS), ("running", nowS - 40 * 86400)] $ \(s, at) ->
          execute c "INSERT INTO ui_events VALUES (?, 1, ?, 'end', '{}')" (s :: Text, at :: Double)
        execute c "INSERT INTO ui_runs VALUES ('t1', 'old', 1, ?), ('t2', 'fresh', 2, ?), ('t3', 'running', 3, ?)" (nowS - 40 * 86400 :: Double, nowS :: Double, nowS - 40 * 86400 :: Double)
        close c
        forM_ ["old", "running"] $ \s -> do
          oldFile 40 (sd </> (s <> ".jsonl"))
          oldFile 40 (sd </> (s <> ".verdicts.json"))
          oldFile 40 (ib </> (s <> ".jsonl"))
        oldFile 40 (sd </> "old.jsonl.lock")
        oldFile 40 (ib </> "old.offset")
        writeFile (sd </> "fresh.jsonl") "x\n"
        -- a session with only old files (no ledger)
        oldFile 40 (sd </> "files-only.jsonl")
        oldFile 40 (logs </> "a.log")
        -- an old log of a run whose session is running stays
        oldFile 40 (logs </> "t3.log")
        writeFile (logs </> "b.log") "x\n"
        writeFile (runLockPath home "running") ""
        -- while it deletes a session's files, prune holds that session's run lock
        heldDuring <- newIORef []
        let during r' = otherCanLock (runLockPath home r') >>= \free -> modifyIORef heldDuring ((r', not free) :)
        r <- withHolder (runLockPath home "running") (pruneUiWith during home 30)
        r `shouldBe` Pruned {prSessions = 2, prRows = 2, prFiles = 6, prLogs = 1}
        sort <$> readIORef heldDuring `shouldReturn` [("files-only", True), ("old", True)]
        -- the run locks stay (a chat may be opening one), released
        sort <$> listDirectory sd `shouldReturn` sort ["fresh.jsonl", "running.jsonl", "running.verdicts.json", "running.lock", "old.lock", "files-only.lock"]
        otherCanLock (runLockPath home "old") `shouldReturn` True
        listDirectory ib `shouldReturn` ["running.jsonl"]
        sort <$> listDirectory logs `shouldReturn` ["b.log", "t3.log"]
        c2 <- open (home </> "state.db")
        evs <- query_ c2 "SELECT session_id FROM ui_events ORDER BY session_id" :: IO [Only Text]
        runs <- query_ c2 "SELECT tag FROM ui_runs" :: IO [Only Text]
        close c2
        map fromOnly evs `shouldBe` ["fresh", "running"]
        sort (map fromOnly runs) `shouldBe` ["t2", "t3"]

  describe "fisher ui --prune DAYS" $
    it "refuses fewer than one day" $ withSystemTempDirectory "fisher-ui-prune" $ \tmp -> do
      forM_ ["0", "0.5", "-3", "x"] $ \d -> uiMain tmp ["--prune", d] `shouldReturn` 1
      uiMain tmp ["--prune", "1"] `shouldReturn` 0

  describe "the repository for the gates" $ do
    it "is the config key fisher.repo when it names a directory" $ withSystemTempDirectory "fisher-ui-repo" $ \tmp -> do
      let home = tmp </> "gauss"
          repo = tmp </> "elsewhere"
      createDirectoryIfMissing True home
      createDirectoryIfMissing True repo
      writeFile (home </> "config.yaml") ("fisher:\n  repo: " <> repo <> "\n")
      findRepo home "/nonexistent/bin/fisher" `shouldReturn` repo
    it "else the directory at or above the executable's with tools/bench" $ withSystemTempDirectory "fisher-ui-repo" $ \tmp0 -> do
      tmp <- canonicalizePath tmp0
      let home = tmp </> "gauss"
          bin = tmp </> "Fisher" </> ".stack-work" </> "install" </> "bin"
      createDirectoryIfMissing True home
      createDirectoryIfMissing True bin
      createDirectoryIfMissing True (tmp </> "Fisher" </> "tools" </> "bench")
      writeFile (bin </> "fisher") ""
      findRepo home (bin </> "fisher") `shouldReturn` (tmp </> "Fisher")

-- | Whether another process can take the lock at once.
otherCanLock :: FilePath -> IO Bool
otherCanLock p = do
  (code, _, _) <- readProcessWithExitCode "python3" ["-c", "import fcntl,sys\nf=open(sys.argv[1],'a')\ntry:\n fcntl.lockf(f, fcntl.LOCK_EX | fcntl.LOCK_NB)\nexcept OSError:\n sys.exit(1)\n", p] ""
  pure (code == ExitSuccess)

-- | Another process holding the lock while the action runs.
withHolder :: FilePath -> IO a -> IO a
withHolder p k =
  bracket
    (createProcess (proc "python3" ["-c", "import fcntl,sys,time\nf=open(sys.argv[1],'a')\nfcntl.lockf(f, fcntl.LOCK_EX)\nprint('locked', flush=True)\ntime.sleep(60)\n", p]) {std_out = CreatePipe})
    (\(_, _, _, ph) -> terminateProcess ph >> waitForProcess ph)
    (\(_, mout, _, _) -> mapM_ hGetLine mout >> k)
