-- | @fisher launch --print@: the planned child in its headless form, run
-- with stdin from /dev/null until it exits, its exit status the launch's.
module Fisher.HeadlessLaunchSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (canonicalizePath)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (setFileMode)
import Test.Hspec

import Fisher.Workflow.Plan (printArgv)
import Fisher.Workflow.Run (runHeadless)

spec :: Spec
spec = do
  describe "printArgv" $ do
    it "runs the native child with -p in place of -q" $
      printArgv "fisher" ["/x/fisher", "chat", "-m", "M", "-q", "P"] `shouldBe` Right ["/x/fisher", "chat", "-m", "M", "-p", "P"]
    it "gives claude-code -p before the prompt" $
      printArgv "claude-code" ["/x/claude", "--model", "M", "--dangerously-skip-permissions", "P"]
        `shouldBe` Right ["/x/claude", "--model", "M", "--dangerously-skip-permissions", "-p", "P"]
    it "runs codex exec" $
      printArgv "codex" ["/x/codex", "-m", "M", "--dangerously-bypass-approvals-and-sandbox", "P"]
        `shouldBe` Right ["/x/codex", "exec", "-m", "M", "--dangerously-bypass-approvals-and-sandbox", "P"]
    it "refuses what it has no headless form for" $ do
      printArgv "nope" ["a", "P"] `shouldBe` Left "No headless form for the nope backend."
      printArgv "fisher" [] `shouldBe` Left "No headless form for an empty argv."

  describe "runHeadless" $ do
    -- a backend that reads stdin to its end, notes its arguments and
    -- directory, and exits 3
    let fake = T.unlines
          [ "#!/bin/sh"
          , "cat > \"$FAKE_DIR/stdin.txt\""
          , "printf '%s\\n' \"$@\" > \"$FAKE_DIR/args.txt\""
          , "pwd > \"$FAKE_DIR/cwd.txt\""
          , "echo \"$FAKE_MARK\" > \"$FAKE_DIR/env.txt\""
          , "exit 3" ]
    it "runs the child with stdin closed and returns its exit code" $ withSystemTempDirectory "fisher-headless" $ \d0 -> do
      d <- canonicalizePath d0
      let exe = d </> "backend"
      TIO.writeFile exe fake
      setFileMode exe 0o755
      env <- Map.fromList <$> getEnvironment
      let child = Map.insert "FAKE_MARK" "set" (Map.insert "FAKE_DIR" d env)
      r <- runHeadless [T.pack exe, "-p", "the prompt"] d child
      r `shouldBe` Right (ExitFailure 3)
      TIO.readFile (d </> "stdin.txt") `shouldReturn` ""
      TIO.readFile (d </> "args.txt") `shouldReturn` "-p\nthe prompt\n"
      TIO.readFile (d </> "cwd.txt") `shouldReturn` T.pack (d <> "\n")
      TIO.readFile (d </> "env.txt") `shouldReturn` "set\n"
    it "returns success as success" $ withSystemTempDirectory "fisher-headless" $ \d0 -> do
      d <- canonicalizePath d0
      let exe = d </> "backend"
      TIO.writeFile exe "#!/bin/sh\ncat >/dev/null\nexit 0\n"
      setFileMode exe 0o755
      env <- Map.fromList <$> getEnvironment
      runHeadless [T.pack exe] d env `shouldReturn` Right ExitSuccess
    it "says so when the child cannot start" $ withSystemTempDirectory "fisher-headless" $ \d -> do
      env <- Map.fromList <$> getEnvironment
      r <- runHeadless [T.pack (d </> "missing")] d env
      either (const True) (const False) r `shouldBe` True
