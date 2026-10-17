-- | The step log (Fisher UI, task 4): its lines, and which files a
-- write_file or patch result says it changed.
module Fisher.Agent.StepsSpec (spec) where

import qualified Data.ByteString as BS
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Agent.Steps
import Fisher.Py.Value

spec :: Spec
spec = describe "Fisher.Agent.Steps" $ do
  it "appends steps and reads them back in order" $ withSystemTempDirectory "fisher-steps" $ \tmp -> do
    let path = stepsPath (tmp </> "gauss") "s1"
        a = Step 1 "call_1" "/w/A.lean" (fileHash "a") "a" "write_file" 1.5
        b = Step 2 "call_2" "/w/B.lean" (fileHash "b\n") "b\n" "patch" 2.25
    path `shouldBe` tmp </> "gauss" </> "fisher" </> "steps" </> "s1.jsonl"
    readSteps path `shouldReturn` []
    appendStep path a
    appendStep path b
    readSteps path `shouldReturn` [a, b]
  it "skips a line cut short" $ withSystemTempDirectory "fisher-steps" $ \tmp -> do
    let path = tmp </> "s.jsonl"
        a = Step 1 "c" "/p" (fileHash "") "" "write_file" 0
    appendStep path a
    BS.appendFile path "{\"seq\": 2, \"call"
    readSteps path `shouldReturn` [a]
  it "hashes with SHA-256" $ do
    fileHash "" `shouldBe` "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    fileHash "abc" `shouldBe` "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
  describe "changedPaths" $ do
    let args kvs = PDict [(k, PStr v) | (k, v) <- kvs]
    it "write_file: its path, unless it failed" $ do
      changedPaths "write_file" (args [("path", "A.lean")]) "{\"bytes_written\": 3, \"dirs_created\": false}" `shouldBe` ["A.lean"]
      changedPaths "write_file" (args [("path", "A.lean")]) "{\"bytes_written\": 0, \"dirs_created\": false, \"error\": \"denied\"}" `shouldBe` []
      changedPaths "write_file" (args [("path", "A.lean")]) "Error executing tool 'write_file': boom" `shouldBe` []
    it "patch: the files it modified and created, when it succeeded" $ do
      changedPaths "patch" (args [("path", "A.lean"), ("mode", "replace")]) "{\"success\": true, \"diff\": \"x\", \"files_modified\": [\"A.lean\"]}" `shouldBe` ["A.lean"]
      changedPaths "patch" (args [("mode", "patch")]) "{\"success\": true, \"files_modified\": [\"A\"], \"files_created\": [\"B\"], \"files_deleted\": [\"C\"]}" `shouldBe` ["A", "B"]
      changedPaths "patch" (args [("path", "A.lean")]) "{\"success\": false, \"error\": \"Could not find\"}\n\n[Hint: old_string not found.]" `shouldBe` []
