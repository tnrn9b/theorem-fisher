-- | 'Fisher.Agent.Tools.Memory' against @tools/memory_tool.py@
-- (@test/golden/b4/memory.json@): results, the frozen snapshot, the files
-- written, and what a fresh store loads from them.
module Fisher.Agent.MemorySpec (spec) where

import Control.Monad (forM, forM_)
import qualified Data.ByteString as BS
import Data.List (sort)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Agent.Prompt (MemorySnapshot (..))
import Fisher.Agent.TestUtil
import Fisher.Agent.Tools.Memory
import Fisher.Py.Value
import Fisher.Tools.Registry

snapVal :: MemorySnapshot -> PyVal
snapVal s = PDict [("memory", orNone (msMemory s)), ("user", orNone (msUser s))]
  where
    orNone t = if T.null t then PNull else PStr t

callTool :: MemoryStore -> PyVal -> IO T.Text
callTool st a = memoryTool (Just st) (field "action" a) (maybe (PStr "memory") id (get "target" a)) (field "content" a) (field "old_text" a)

spec :: Spec
spec = do
  doc <- runIO (loadDoc "test/golden/b4/memory.json")
  describe "tools/memory_tool.py" $ do
    forM_ (list "cases" doc) $ \c -> it (T.unpack (txt "name" c)) $
      withSystemTempDirectory "fisher-memory" $ \root -> do
        let mem = root </> "memories"
        createDirectoryIfMissing True mem
        case field "files" c of
          PDict kvs -> forM_ kvs $ \(f, v) -> BS.writeFile (mem </> T.unpack f) (TE.encodeUtf8 (pyStrOf v))
          _ -> pure ()
        st <- newMemoryStore mem 2200 1375
        loadFromDisk st
        s0 <- frozenSnapshot st
        snapVal s0 `shouldBe` field "snapshot" c
        rs <- forM (list "calls" c) (callTool st)
        map PStr rs `shouldBe` list "results" c
        s1 <- frozenSnapshot st
        snapVal s1 `shouldBe` field "snapshot_after" c
        names <- sort . filter (".md" `T.isSuffixOf`) . map T.pack <$> listDirectory mem
        onDisk <- forM names $ \n -> (,) n . PStr . TE.decodeUtf8 <$> BS.readFile (mem </> T.unpack n)
        PDict onDisk `shouldBe` field "files_after" c
        leftovers <- filter (".tmp" `T.isSuffixOf`) . map T.pack <$> listDirectory mem
        leftovers `shouldBe` []
        st2 <- newMemoryStore mem 2200 1375
        loadFromDisk st2
        s2 <- frozenSnapshot st2
        snapVal s2 `shouldBe` field "reloaded" c
    forM_ (list "limits" doc) $ \l -> it ("limits " <> show (field "limits" l)) $
      withSystemTempDirectory "fisher-memory" $ \root -> do
        let (ml, ul) = case [fromIntegral n | PInt n <- list "limits" l] of
              [a, b] -> (a, b)
              _ -> (0, 0)
        st <- newMemoryStore (root </> "memories") ml ul
        r <- memoryTool (Just st) (PStr "add") (PStr "memory") (PStr "abc") PNull
        PStr r `shouldBe` field "result" l
    it "no store" $ do
      r <- memoryTool Nothing (PStr "add") (PStr "memory") (PStr "x") PNull
      PStr r `shouldBe` field "no_store" doc
    it "the registry entry" $ do
      let e = field "entry" doc
      tdSchema memoryToolDef `shouldBe` field "schema" e
      PStr (tdToolset memoryToolDef) `shouldBe` field "toolset" e
      PStr (tdEmoji memoryToolDef) `shouldBe` field "emoji" e
