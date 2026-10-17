-- | Real sessions: small shell children under a PTY or on pipes.
module Fisher.SwarmSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (getEnvironment)
import System.IO (hClose, hSetBuffering, BufferMode (NoBuffering))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (createPipe)
import Test.Hspec
import Control.Exception (SomeException, catch)
import System.Posix.Signals (nullSignal, signalProcess)

import Fisher.Swarm
import Fisher.Prover.Kind (ProverKind (..))
import Fisher.Swarm.Event (Progress (..), initialProgress, parseStreamEvent, parseStreamEventFor)
import Fisher.Swarm.Render
import Fisher.Swarm.Types

shell :: FilePath -> Text -> IO SpawnSpec
shell dir script = do
  env <- Map.fromList <$> getEnvironment
  pure (SpawnSpec "t" "d" ["/bin/sh", "-c", script] dir env "prove" "/lean4:prove" "Demo" dir "claude-code" Lean4)

withSwarm :: (Swarm -> FilePath -> IO a) -> IO a
withSwarm k = withSystemTempDirectory "fisher-swarm" $ \dir -> do
  sw <- newSwarm False dir
  k sw dir

spec :: Spec
spec = do
  describe "interactive sessions" $ do
    it "completes, reading stream-json progress from the PTY" $ withSwarm $ \sw dir -> do
      t <- spawnInteractive sw =<< shell dir "echo hello; printf '%s\\n' '{\"type\":\"result\",\"result\":\"done\"}'"
      v <- waitTask t
      tvStatus v `shouldBe` Complete
      pProgress (tvProgress v) `shouldBe` "Session complete"
      pResult (tvProgress v) `shouldBe` Just "done"
      tiTaskId (tvInfo v) `shouldBe` "af-001"
      "af_" `T.isPrefixOf` tiSessionId (tvInfo v) `shouldBe` True

    it "fails with the exit code" $ withSwarm $ \sw dir -> do
      v <- waitTask =<< spawnInteractive sw =<< shell dir "exit 3"
      tvStatus v `shouldBe` Failed
      pError (tvProgress v) `shouldBe` Just "exit 3"
      pProgress (tvProgress v) `shouldBe` "Claude Code exited with error"

    it "says the session ended when there is no result" $ withSwarm $ \sw dir -> do
      v <- waitTask =<< spawnInteractive sw =<< shell dir "true"
      (tvStatus v, pProgress (tvProgress v)) `shouldBe` (Complete, "Session ended successfully")

    it "cancels once, then refuses" $ withSwarm $ \sw dir -> do
      t <- spawnInteractive sw =<< shell dir "sleep 30"
      threadDelay 200000
      cancelTask sw "af-001" `shouldReturn` True
      v <- waitTask t
      (tvStatus v, pProgress (tvProgress v)) `shouldBe` (Cancelled, "Cancelled")
      cancelTask sw "af-001" `shouldReturn` False
      cancelTask sw "af-999" `shouldReturn` False

    it "stops the session's own children on cancel" $ withSwarm $ \sw dir -> do
      t <- spawnInteractive sw =<< shell dir "sleep 30 & echo $! > child.pid; wait"
      threadDelay 300000
      _ <- cancelTask sw "af-001"
      _ <- waitTask t
      pid <- read <$> readFile (dir ++ "/child.pid") :: IO Int
      threadDelay 200000
      alive <- (True <$ signalProcess nullSignal (fromIntegral pid)) `catch` \(_ :: SomeException) -> pure False
      alive `shouldBe` False

    it "reports a missing executable as upstream does" $ withSwarm $ \sw dir -> do
      s <- shell dir ""
      v <- waitTask =<< spawnInteractive sw s {ssArgv = ["/nonexistent/claude"]}
      tvStatus v `shouldBe` Failed
      (pError (tvProgress v), pProgress (tvProgress v)) `shouldBe` (Just "claude CLI not found", "Launch failed")

  describe "attach" $ do
    it "replays earlier output, forwards keys, and detaches on Ctrl-]" $ withSwarm $ \sw dir -> do
      t <- spawnInteractive sw =<< shell dir "echo early; cat"
      threadDelay 300000
      (r, w) <- createPipe
      hSetBuffering w NoBuffering
      out <- newIORef BS.empty
      let io = AttachIO r (\b -> modifyIORef' out (<> b)) Nothing (Just 0x1d)
      _ <- forkIO $ do
        threadDelay 200000
        BS.hPut w "ping\n"
        threadDelay 400000
        BS.hPut w (BS.singleton 0x1d)
      result <- attachTask t io
      result `shouldBe` Right (-1)
      seen <- readIORef out
      BC.unpack seen `shouldContain` "early"
      BC.unpack seen `shouldContain` "ping"
      _ <- cancelTask sw "af-001"
      _ <- waitTask t
      hClose w

    it "stays until the session exits, and returns its code" $ withSwarm $ \sw dir -> do
      t <- spawnInteractive sw =<< shell dir "sleep 0.3; echo bye; exit 4"
      (r, _w) <- createPipe
      out <- newIORef BS.empty
      attachTask t (AttachIO r (\b -> modifyIORef' out (<> b)) Nothing (Just 0x1d)) `shouldReturn` Right 4
      BC.unpack <$> readIORef out >>= (`shouldContain` "bye")

    it "answers Claude Code's folder-trust prompt" $ withSwarm $ \sw dir -> do
      t <- spawnInteractive sw =<< shell dir "sleep 0.2; printf 'Do you trust this folder? '; read x; echo \"got:$x\""
      (r, _w) <- createPipe
      out <- newIORef BS.empty
      attachTask t (AttachIO r (\b -> modifyIORef' out (<> b)) Nothing Nothing) `shouldReturn` Right 0
      BC.unpack <$> readIORef out >>= (`shouldContain` "got:")

    it "refuses a finished task" $ withSwarm $ \sw dir -> do
      t <- spawnInteractive sw =<< shell dir "true"
      _ <- waitTask t
      (r, _w) <- createPipe
      attachTask t (AttachIO r (const (pure ())) Nothing Nothing) `shouldReturn` Left "Task is not running or has no PTY"

  describe "background sessions" $ do
    it "parses stdout and keeps the stderr tail on failure" $ withSwarm $ \sw dir -> do
      v <- waitTask =<< spawnBackground sw =<< shell dir
        "printf '%s\\n' '{\"type\":\"content_block_start\",\"content_block\":{\"type\":\"tool_use\",\"name\":\"lean_build\"}}'; echo oops >&2; exit 2"
      tvStatus v `shouldBe` Failed
      pError (tvProgress v) `shouldBe` Just "exit 2: oops\n"
      pLean (tvProgress v) `shouldBe` Just "active"

  describe "the project's prover in a session" $ do
    let admitted = "printf '%s\\n' '{\"type\":\"tool_result\",\"content\":\"tactic admit used\"}'"
    it "labels a Rocq task's progress on admit, on pipes and under a PTY" $ withSwarm $ \sw dir -> do
      s <- shell dir admitted
      b <- waitTask =<< spawnBackground sw s {ssProver = Rocq}
      pLean (tvProgress b) `shouldBe` Just "has sorry"
      i <- waitTask =<< spawnInteractive sw s {ssProver = Rocq}
      pLean (tvProgress i) `shouldBe` Just "has sorry"
    it "leaves a Lean task's label alone on admit" $ withSwarm $ \sw dir -> do
      v <- waitTask =<< spawnBackground sw =<< shell dir admitted
      pLean (tvProgress v) `shouldBe` Just "starting"

  describe "hole words in tool results" $ do
    let result c = "{\"type\":\"tool_result\",\"content\":\"" <> c <> "\"}"
        lean k c = pLean (parseStreamEventFor k (result c) initialProgress)
    it "reads Lean's sorry by default, as before" $ do
      pLean (parseStreamEvent (result "declaration uses Sorry") initialProgress) `shouldBe` Just "has sorry"
      pLean (parseStreamEvent (result "No errors") initialProgress) `shouldBe` Just "verified"
      lean Lean4 "this uses admit" `shouldBe` Nothing
    it "reads the words of the project's prover" $ do
      lean Rocq "tactic admit used" `shouldBe` Just "has sorry"
      lean Rocq "give_up called" `shouldBe` Just "has sorry"
      lean Rocq "uses sorry" `shouldBe` Nothing
      lean Isabelle "proof ended with oops" `shouldBe` Just "has sorry"
      lean Isabelle "by sorry" `shouldBe` Just "has sorry"

  describe "views" $ do
    it "lists, finds the latest, and summarises" $ withSwarm $ \sw dir -> do
      a <- spawnInteractive sw =<< shell dir "true"
      _ <- waitTask a
      b <- spawnInteractive sw =<< shell dir "sleep 30"
      threadDelay 200000
      vs <- allViews sw
      map tvStatus vs `shouldBe` [Complete, Running]
      latest <- latestTask sw (Just Running) True
      (tiTaskId . taskInfo <$> latest) `shouldBe` Just "af-002"
      summaryLine vs `shouldBe` Just "Swarm: 1 agent running · 1 complete"
      statusBarFragment vs `shouldBe` Just "af:1/2"
      now <- nowSeconds
      let table = renderTable now vs
      head table `shouldBe` "Fisher Workflow Swarm"
      T.unpack (table !! 1) `shouldContain` "Progress"
      T.unpack (table !! 3) `shouldContain` "› attach"
      _ <- cancelTask sw "af-002"
      _ <- waitTask b
      pure ()
