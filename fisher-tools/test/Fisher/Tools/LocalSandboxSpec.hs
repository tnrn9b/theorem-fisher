-- | The local backend on this machine's bash: exit codes, merged output,
-- stdin, working directory, timeout, interrupt, and the persistent shell
-- keeping its directory and variables between commands.
module Fisher.Tools.LocalSandboxSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void)
import qualified Data.Text as T
import GHC.Clock (getMonotonicTime)
import System.Directory (canonicalizePath)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Tools.Sandbox
import Fisher.Tools.Sandbox.Local

run :: Sandbox -> T.Text -> T.Text -> Maybe Int -> Maybe T.Text -> IO ExecResult
run sb cmd cwd t stdin = sbExecute sb (ExecRequest cmd cwd t stdin)

spec :: Spec
spec = do
  describe "local oneshot" $ do
    it "output and exit code" $ do
      se <- defaultSandboxEnv
      sb <- newLocalSandbox se "/" 30 False
      run sb "echo hi" "" Nothing Nothing >>= (`shouldBe` ExecResult "hi\n" 0)
      r <- run sb "echo a; echo b >&2; exit 3" "" Nothing Nothing
      exCode r `shouldBe` 3
      T.lines (exOutput r) `shouldContain` ["a", "b"]
    it "stdin and working directory" $ withSystemTempDirectory "fisher-sb" $ \d -> do
      se <- defaultSandboxEnv
      sb <- newLocalSandbox se "/" 30 False
      run sb "cat" "" Nothing (Just "x\ny") >>= (`shouldBe` ExecResult "x\ny" 0)
      real <- canonicalizePath d
      r <- run sb "pwd -P" (T.pack d) Nothing Nothing
      exOutput r `shouldBe` T.pack real <> "\n"
    it "times out" $ do
      se <- defaultSandboxEnv
      sb <- newLocalSandbox se "/" 30 False
      t0 <- getMonotonicTime
      r <- run sb "sleep 5" "" (Just 1) Nothing
      t1 <- getMonotonicTime
      r `shouldBe` ExecResult "Command timed out after 1s" 124
      -- 1 s, plus up to 2 s waiting for the output pipe: on macOS the
      -- interactive bash puts sleep in its own process group, which the
      -- group SIGTERM misses (upstream waits the same way)
      (t1 - t0) `shouldSatisfy` (< 4.5)
    it "is interrupted" $ do
      se <- defaultSandboxEnv
      sb <- newLocalSandbox se "/" 30 False
      void . forkIO $ threadDelay 500000 >> raiseInterrupt (seInterrupt se)
      r <- run sb "sleep 5" "" Nothing Nothing
      exCode r `shouldBe` 130
      exOutput r `shouldSatisfy` T.isSuffixOf "\n[Command interrupted \8212 user sent a new message]"
  describe "local persistent shell" $ do
    it "keeps its directory and variables" $ do
      se <- defaultSandboxEnv
      sb <- newLocalSandbox se "/" 30 True
      run sb "cd /; export FOO=1" "" Nothing Nothing >>= (`shouldBe` ExecResult "" 0)
      run sb "echo $FOO; pwd" "" Nothing Nothing >>= (`shouldBe` ExecResult "1\n/" 0)
      sbCwd sb >>= (`shouldBe` "/")
      sbCleanup sb
    it "puts stdout before stderr and drops trailing newlines" $ do
      se <- defaultSandboxEnv
      sb <- newLocalSandbox se "/" 30 True
      run sb "echo err >&2; echo out; false" "" Nothing Nothing >>= (`shouldBe` ExecResult "out\nerr" 1)
      sbCleanup sb
    it "falls back to a oneshot run for stdin" $ do
      se <- defaultSandboxEnv
      sb <- newLocalSandbox se "/" 30 True
      run sb "cat" "" Nothing (Just "piped") >>= (`shouldBe` ExecResult "piped" 0)
      sbCleanup sb
