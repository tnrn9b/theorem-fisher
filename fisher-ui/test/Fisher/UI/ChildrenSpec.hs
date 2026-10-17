-- | The UI's child runs: their own process group, stdin from /dev/null,
-- the tag in their environment, and a stop that takes the whole group.
module Fisher.UI.ChildrenSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Exception (SomeException, finally, try)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Signals (nullSignal, sigKILL, signalProcess)
import System.Posix.Types (CPid)
import System.Timeout (timeout)
import Test.Hspec

import Fisher.UI.Children

-- | Wait until a file holds a pid.
pidIn :: FilePath -> IO CPid
pidIn f = go (100 :: Int)
  where
    go n = do
      r <- try (readFile f)
      case r of
        Right s | [(p, _)] <- reads s -> pure (fromInteger p :: CPid)
        _ | n > 0 -> threadDelay 50000 >> go (n - 1)
        Left (_ :: SomeException) | n > 0 -> threadDelay 50000 >> go (n - 1)
        _ -> fail ("no pid in " <> f)

alive :: CPid -> IO Bool
alive p = either (\(_ :: SomeException) -> False) (const True) <$> try (signalProcess nullSignal p)

-- | Wait (up to 3 s) for a process to be gone.
goneSoon :: CPid -> IO Bool
goneSoon p = go (60 :: Int)
  where
    go n = alive p >>= \case
      False -> pure True
      True | n > 0 -> threadDelay 50000 >> go (n - 1)
      True -> pure False

spec :: Spec
spec = describe "Fisher.UI.Children" $ do
  it "starts the child with the tag, stdin from /dev/null, output to the log, and reports its exit" $ withSystemTempDirectory "fisher-ui-child" $ \d -> do
    let logp = d </> "runs" </> "x.log"
    (ch, exited) <- spawnChild (Just logp) d ["/bin/sh", "-c", "echo tag=$FISHER_UI_TAG; echo in=$(cat); pwd; exit 2"] [("FISHER_UI_TAG", "t-1")]
    chTag ch `shouldBe` "t-1"
    timeout 5000000 (readMVar exited) `shouldReturn` Just (ExitFailure 2)
    out <- lines <$> readFile logp
    take 2 out `shouldBe` ["tag=t-1", "in="]
    length out `shouldBe` 3

  it "makes a tag when none is given" $ withSystemTempDirectory "fisher-ui-child" $ \d -> do
    ch <- startChild d ["/bin/sh", "-c", "exit 0"] []
    length (show (chTag ch)) `shouldSatisfy` (> 10)

  it "stops a child that leaves on SIGTERM at once" $ withSystemTempDirectory "fisher-ui-child" $ \d -> do
    (ch, exited) <- spawnChild Nothing d ["/bin/sh", "-c", "sleep 300 & echo $! > " <> d </> "kid; wait"] []
    kid <- pidIn (d </> "kid")
    t0 <- getCurrentTime
    stopChildWith 5000000 ch
    t1 <- getCurrentTime
    realToFrac (diffUTCTime t1 t0) `shouldSatisfy` (< (2 :: Double))
    timeout 3000000 (readMVar exited) >>= (`shouldSatisfy` maybe False (/= ExitSuccess))
    goneSoon kid `shouldReturn` True

  it "kills a child that ignores SIGTERM after the grace, and its children with it" $ withSystemTempDirectory "fisher-ui-child" $ \d -> do
    (ch, exited) <- spawnChild Nothing d ["/bin/sh", "-c", "trap '' TERM; sleep 300 & echo $! > " <> d </> "kid; wait; wait; wait"] []
    kid <- pidIn (d </> "kid")
    t0 <- getCurrentTime
    stopChildWith 1000000 ch
    t1 <- getCurrentTime
    let took = realToFrac (diffUTCTime t1 t0) :: Double
    took `shouldSatisfy` (>= 1)
    took `shouldSatisfy` (< 6)
    timeout 3000000 (readMVar exited) >>= (`shouldSatisfy` maybe False (/= ExitSuccess))
    goneSoon kid `shouldReturn` True
    groupAlive (chPid ch) `shouldReturn` False

  it "kills a grandchild that left the group for a session of its own, after the grace" $ withSystemTempDirectory "fisher-ui-child" $ \d -> do
    writeFile (d </> "detach.pl") "use POSIX; POSIX::setsid(); open(my $f, '>', $ARGV[0]) or die; print $f $$; close $f; exec 'sleep', '300';\n"
    (ch, _) <- spawnChild Nothing d ["/bin/sh", "-c", "trap '' TERM; perl " <> d </> "detach.pl " <> d </> "kid & wait; wait"] []
    kid <- pidIn (d </> "kid")
    (do stopChildWith 1000000 ch
        goneSoon kid `shouldReturn` True)
      `finally` (try (signalProcess sigKILL kid) :: IO (Either SomeException ()))
