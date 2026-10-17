-- | Fisher only: the file tools on CLM's context file (docs/clm.md
-- section 3), on a real local sandbox: reads of it are not counted by the
-- loop guard, and @patch@ takes exact matches only there.
module Fisher.Tools.ContextPathSpec (spec) where

import Control.Monad (replicateM_)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Py.Value
import Fisher.Tools.File
import Fisher.Tools.FileOps
import Fisher.Tools.Fuzzy (exactFindAndReplace, fuzzyFindAndReplace)
import Fisher.Tools.Registry
import Fisher.Tools.Sandbox.Local (defaultSandboxEnv, newLocalSandbox)

withTools :: ((T.Text -> PyVal -> IO T.Text) -> FilePath -> FileTools -> IO a) -> IO a
withTools k = withSystemTempDirectory "fisher-ctx" $ \tmp -> do
  createDirectoryIfMissing True (tmp </> "ctx")
  se <- defaultSandboxEnv
  sb <- newLocalSandbox se (T.pack tmp) 60 False
  scope <- homeWriteScope "/nonexistent-home"
  ops <- newFileOps scope sb
  ft <- newFileTools (const (pure (Right ops))) (pure Nothing) (pure True)
  reg <- newRegistry
  registerFileTools reg ft
  k (\name args -> handleFunctionCall reg defaultCtx {tcTaskId = "t"} name args) tmp ft

spec :: Spec
spec = do
  describe "exact matching" $ do
    it "the fuzzy strategies find what exact matching does not" $ do
      let content = "<<< turn 4 tool terminal >>>\n  build ok  \n"
      case fuzzyFindAndReplace content "build ok\n" "done\n" False of
        (_, n, Nothing) -> n `shouldSatisfy` (> 0)
        (_, _, Just e) -> expectationFailure (T.unpack e)
      let (_, n, err) = exactFindAndReplace content "build ok\n" "done\n" False
      n `shouldBe` 0
      err `shouldSatisfy` maybe False ("Could not find" `T.isPrefixOf`)
    it "exact matching still replaces an exact match" $
      exactFindAndReplace "a\nbuild ok\nb" "build ok" "done" False `shouldBe` ("a\ndone\nb", 1, Nothing)
  describe "on the context file" $ do
    it "patch is exact there and fuzzy elsewhere" $ withTools $ \call tmp ft -> do
      let ctxFile = tmp </> "ctx" </> "context.txt"
          other = tmp </> "other.txt"
      BS.writeFile ctxFile (TE.encodeUtf8 "  build ok  \n")
      BS.writeFile other (TE.encodeUtf8 "  build ok  \n")
      setContextPaths ft [T.pack ctxFile]
      let patch p = call "patch" (PDict [("mode", PStr "replace"), ("path", PStr (T.pack p)), ("old_string", PStr "build ok\n"), ("new_string", PStr "done\n")])
      onCtx <- patch ctxFile
      onOther <- patch other
      onCtx `shouldSatisfy` T.isInfixOf "Could not find"
      onOther `shouldSatisfy` T.isInfixOf "\"success\": true"
    it "reads of it are not counted by the loop guard" $ withTools $ \call tmp ft -> do
      let ctxFile = tmp </> "ctx" </> "context.txt"
          other = tmp </> "other.txt"
      BS.writeFile ctxFile "x\n"
      BS.writeFile other "x\n"
      setContextPaths ft [T.pack ctxFile]
      let rd p = call "read_file" (PDict [("path", PStr (T.pack p))])
      replicateM_ 4 (rd ctxFile)
      onCtx <- rd ctxFile
      replicateM_ 4 (rd other)
      onOther <- rd other
      onCtx `shouldSatisfy` (not . T.isInfixOf "BLOCKED")
      onOther `shouldSatisfy` T.isInfixOf "BLOCKED"
