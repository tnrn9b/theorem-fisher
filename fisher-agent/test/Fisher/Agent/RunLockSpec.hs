-- | The run lock (Fisher UI): held by a chat process for its life, seen
-- from another process (python3's fcntl.lockf, the same fcntl locks).
module Fisher.Agent.RunLockSpec (spec) where

import Control.Exception (bracket)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hGetLine)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.IO (FdOption (..), queryFdOption)
import System.Posix.Process (getProcessID)
import System.Process
import Test.Hspec

import Fisher.Agent.RunLock

-- | Whether another process can take the lock at once.
otherCanLock :: FilePath -> IO Bool
otherCanLock p = do
  (code, _, _) <- readProcessWithExitCode "python3" ["-c", "import fcntl,sys\nf=open(sys.argv[1],'a')\ntry:\n fcntl.lockf(f, fcntl.LOCK_EX | fcntl.LOCK_NB)\nexcept OSError:\n sys.exit(1)\n", p] ""
  pure (code == ExitSuccess)

-- | A python3 that holds the lock until it is killed.
withHolder :: FilePath -> (ProcessHandle -> IO a) -> IO a
withHolder p k =
  bracket
    (createProcess (proc "python3" ["-c", "import fcntl,sys,time\nf=open(sys.argv[1],'a')\nfcntl.lockf(f, fcntl.LOCK_EX)\nprint('locked', flush=True)\ntime.sleep(60)\n", p]) {std_out = CreatePipe})
    (\(_, _, _, ph) -> terminateProcess ph >> waitForProcess ph)
    (\(_, Just out, _, ph) -> hGetLine out >> k ph)

spec :: Spec
spec = describe "Fisher.Agent.RunLock" $ do
  it "holds the session's lock for the process's life and writes its pid" $ withSystemTempDirectory "fisher-runlock" $ \tmp -> do
    let home = tmp </> "gauss"
    runLockPath home "R" `shouldBe` home </> "fisher" </> "steps" </> "R.lock"
    holdRunLock home "R" `shouldReturn` True
    -- a second take in the same process keeps it
    holdRunLock home "R" `shouldReturn` True
    otherCanLock (runLockPath home "R") `shouldReturn` False
    pid <- getProcessID
    readFile (runPidPath home "R") `shouldReturn` (show pid <> "\n")
    -- another session's lock is free
    holdRunLock home "S" `shouldReturn` True
    otherCanLock (runLockPath home "T") `shouldReturn` True

  it "does not take a lock another process holds, and names that process" $ withSystemTempDirectory "fisher-runlock" $ \tmp -> do
    let home = tmp </> "gauss"
    holdRunLock home "X" `shouldReturn` True
    let p = runLockPath home "Q"
    writeFile p ""
    runLockHolder home "Q" `shouldReturn` Nothing
    withHolder p $ \ph -> do
      holdRunLock home "Q" `shouldReturn` False
      who <- getPid ph
      runLockHolder home "Q" `shouldReturn` fmap fromIntegral who
    runLockHolder home "nothing-here" `shouldReturn` Nothing

  it "waits a moment for a lock held briefly (a prune), then takes it" $ withSystemTempDirectory "fisher-runlock" $ \tmp -> do
    let home = tmp </> "gauss"
        p = runLockPath home "W"
    holdRunLock home "X0" `shouldReturn` True
    writeFile p ""
    (_, Just out, _, ph) <- createProcess (proc "python3" ["-c", "import fcntl,sys,time\nf=open(sys.argv[1],'a')\nfcntl.lockf(f, fcntl.LOCK_EX)\nprint('locked', flush=True)\ntime.sleep(0.6)\n", p]) {std_out = CreatePipe}
    _ <- hGetLine out
    holdRunLock home "W" `shouldReturn` True
    _ <- waitForProcess ph
    otherCanLock p `shouldReturn` False

  it "opens the lock's descriptor close-on-exec (a tool's subprocess does not keep it)" $ withSystemTempDirectory "fisher-runlock" $ \tmp -> do
    let home = tmp </> "gauss"
    holdRunLock home "E" `shouldReturn` True
    fd <- maybe (fail "no descriptor held") pure =<< heldRunLockFd home "E"
    queryFdOption fd CloseOnExec `shouldReturn` True

  it "takes a lock for a while when it is free (tryRunLock), and not when another process holds it" $ withSystemTempDirectory "fisher-runlock" $ \tmp -> do
    let home = tmp </> "gauss"
    got <- tryRunLock home "P"
    case got of
      Nothing -> expectationFailure "a free lock was not taken"
      Just fd -> do
        queryFdOption fd CloseOnExec `shouldReturn` True
        otherCanLock (runLockPath home "P") `shouldReturn` False
        releaseRunLock fd
    otherCanLock (runLockPath home "P") `shouldReturn` True
    withHolder (runLockPath home "P") $ \_ -> tryRunLock home "P" >>= (`shouldSatisfy` (== Nothing))
