-- | The file tools against upstream's recorded command streams
-- (fileops.json): each step must send exactly the shell commands upstream
-- sent (command, working directory, timeout, stdin), in order, and given
-- the same answers must return the same JSON string.
module Fisher.Tools.FileOpsSpec (spec, scriptedSandbox) where

import Control.Exception (throwIO)
import Control.Monad (forM_, unless)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Fisher.Py.Value
import Fisher.Tools.File
import Fisher.Tools.FileOps
import Fisher.Tools.Registry
import Fisher.Tools.Sandbox
import Fisher.Tools.TestUtil

-- | A sandbox that answers from a script and fails on any command the
-- script does not expect next.
scriptedSandbox :: Text -> IORef [PyVal] -> Sandbox
scriptedSandbox root script =
  Sandbox
    { sbKind = "scripted"
    , sbExecute = \req -> do
        next <- readIORef script
        case next of
          [] -> throwIO (userError ("unexpected command: " <> T.unpack (erCommand req)))
          (c : rest) -> do
            writeIORef script rest
            let want = (txt "command" c, txt "cwd" c, case field "timeout" c of PInt n -> Just (fromIntegral n); _ -> Nothing, case field "stdin" c of PStr s -> Just s; _ -> Nothing)
                got = (erCommand req, erCwd req, erTimeout req, erStdin req)
            unless (want == got) (throwIO (userError ("command mismatch:\n  want " <> show want <> "\n  got  " <> show got)))
            pure (ExecResult (txt "output" c) (fromIntegral (intOf (field "returncode" c))))
    , sbCwd = pure root
    , sbDefaultTimeout = 60
    , sbCleanup = pure ()
    }

spec :: Spec
spec = do
  doc <- runIO (loadDoc "test/golden/b2/fileops.json")
  describe "file tools replay upstream's command streams" $
    forM_ (cases doc) $ \c -> it (T.unpack (txt "name" c)) $ do
      script <- newIORef []
      scope <- homeWriteScope (txt "home" c)
      ops <- newFileOps scope (scriptedSandbox (txt "root" c) script)
      ft <- newFileTools (const (pure (Right ops))) (pure Nothing) (pure True)
      reg <- newRegistry
      registerFileTools reg ft
      let ctx = defaultCtx {tcTaskId = txt "task" c}
      forM_ (zip [1 :: Int ..] (listOf (field "steps" c))) $ \(i, s) -> do
        writeIORef script (listOf (field "calls" s))
        out <- handleFunctionCall reg ctx (txt "tool" s) (field "args" s)
        left <- readIORef script
        (i, out) `shouldBe` (i, txt "result" s)
        (i, length left) `shouldBe` (i, 0)
  describe "ShellFileOperations probes" $
    forM_ (listOf (field "probes" doc)) $ \p -> it (T.unpack (txt "label" p)) $ do
      script <- newIORef (listOf (field "calls" p))
      scope <- homeWriteScope "/nonexistent-home"
      ops <- newFileOps scope (scriptedSandbox "/tmp/probe" script)
      let calls = listOf (field "calls" p)
          cwd0 = if null calls then "/tmp/probe" else txt "cwd" (head calls)
      ops' <- newFileOps scope (scriptedSandbox cwd0 script)
      _ <- pure ops
      v <- case txt "label" p of
        "expand ~" -> PStr <$> expandPath ops' "~"
        "expand ~/x" -> PStr <$> expandPath ops' "~/x/y"
        "expand ~root" -> PStr <$> expandPath ops' "~root/z"
        "expand ~bad" -> PStr <$> expandPath ops' "~bad;rm/z"
        "expand plain" -> PStr <$> expandPath ops' "/a/b"
        "lint js" -> lintResultDict <$> checkLint ops' (cwd0 <> "/x.js")
        "lint rs" -> lintResultDict <$> checkLint ops' (cwd0 <> "/x.rs")
        "lint none" -> lintResultDict <$> checkLint ops' (cwd0 <> "/x")
        "has rg" -> PBool <$> hasCommand ops' "rg"
        "escape" -> pure (PStr (escapeShellArg "it's a 'test'"))
        "numbers" -> pure (PStr (addLineNumbers ("a\n" <> T.replicate 2001 "b" <> "\n") 7))
        other -> fail ("unknown probe " <> T.unpack other)
      v `shouldBe` field "value" p
      left <- readIORef script
      length left `shouldBe` 0
