-- | Importing interactive Claude Code and Codex runs into the run log
-- from those tools' own session files.
module Fisher.Swarm.ImportSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import System.Directory (canonicalizePath, copyFile, createDirectoryIfMissing, createDirectoryLink, getModificationTime, setModificationTime)
import System.FilePath ((</>))
import System.IO (IOMode (..), hSetFileSize, withFile)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Runlog
import Fisher.Runlog.Stream (StreamEvent (..))
import Fisher.Swarm.Event (initialProgress)
import Fisher.Swarm.Import
import Fisher.Swarm.Types

claudeFixture, codexFixture :: FilePath
claudeFixture = "test/fixtures/claude-session.jsonl"
codexFixture = "test/fixtures/codex-rollout.jsonl"

mtimeOf :: FilePath -> IO Double
mtimeOf p = realToFrac . utcTimeToPOSIXSeconds <$> getModificationTime p

view :: Text -> Status -> Maybe Double -> Maybe Double -> TaskView
view backend st s e =
  TaskView
    { tvInfo = TaskInfo "af-001" "prove it" "add_comm" "prove" "/lean4:prove" "Demo" "/tmp/proj" "/tmp/proj" backend "af_0011aabb"
    , tvStatus = st
    , tvProgress = initialProgress
    , tvStart = s
    , tvEnd = e
    , tvHasPty = True
    }

-- a fake Claude Code home holding the fixture as the session of /tmp/proj
claudeHome :: FilePath -> IO FilePath
claudeHome dir = do
  let d = dir </> "claude-home" </> ".claude" </> "projects" </> "-tmp-proj"
  createDirectoryIfMissing True d
  copyFile claudeFixture (d </> "abc.jsonl")
  pure (d </> "abc.jsonl")

withLog :: (FilePath -> Runlog -> IO a) -> IO a
withLog k = withSystemTempDirectory "fisher-import" $ \dir -> withRunlog (dir </> "runs.db") (k dir)

setMtime :: FilePath -> Double -> IO ()
setMtime p = setModificationTime p . posixSecondsToUTCTime . realToFrac

-- 2026-10-24T10:00:00Z, the Claude fixture's first timestamp
tenOClock :: Double
tenOClock = 1792836000

-- a session line of the cwd at the given ISO 8601 time
sessionLine :: String -> BS.ByteString
sessionLine ts =
  BC.pack ("{\"type\":\"assistant\",\"timestamp\":\"" <> ts <> "\",\"message\":{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"hi\"}]}}\n")

-- a Codex rollout whose session_meta names the cwd
rollout :: String -> BS.ByteString
rollout cwd =
  BC.pack ("{\"timestamp\":\"2026-10-24T11:00:00.000Z\",\"type\":\"session_meta\",\"payload\":{\"id\":\"x\",\"cwd\":\"" <> cwd <> "\"}}\n")

field :: Text -> Value -> Maybe Value
field k = \case
  Object o -> KM.lookup (K.fromText k) o
  _ -> Nothing

spec :: Spec
spec = do
  describe "Claude Code session files" $ do
    it "finds the cwd's newest session modified after the start" $ withSystemTempDirectory "fisher-import" $ \dir -> do
      p <- claudeHome dir
      m <- mtimeOf p
      claudeSessionFile (dir </> "claude-home") "/tmp/proj" (m - 60) `shouldReturn` Just p
      claudeSessionFile (dir </> "claude-home") "/tmp/proj" (m + 60) `shouldReturn` Nothing
      claudeSessionFile (dir </> "claude-home") "/tmp/other" (m - 60) `shouldReturn` Nothing

    it "prefers the session whose first timestamp is nearest the start" $ withSystemTempDirectory "fisher-import" $ \dir -> do
      let d = dir </> ".claude" </> "projects" </> "-tmp-proj"
      createDirectoryIfMissing True d
      BS.writeFile (d </> "near.jsonl") (sessionLine "2026-10-24T10:00:05Z")
      BS.writeFile (d </> "far.jsonl") (sessionLine "2026-10-24T10:30:00Z")
      setMtime (d </> "near.jsonl") (tenOClock + 3600)
      setMtime (d </> "far.jsonl") (tenOClock + 7200)
      claudeSessionFile dir "/tmp/proj" tenOClock `shouldReturn` Just (d </> "near.jsonl")
      claudeSessionFile dir "/tmp/proj" (tenOClock + 1795) `shouldReturn` Just (d </> "far.jsonl")

    it "also looks under the canonicalized cwd's spelling" $ withSystemTempDirectory "fisher-import" $ \dir -> do
      createDirectoryIfMissing True (dir </> "real")
      createDirectoryLink (dir </> "real") (dir </> "link")
      canon <- canonicalizePath (dir </> "real")
      let d = dir </> "home" </> ".claude" </> "projects" </> map (\c -> if c == '/' then '-' else c) canon
      createDirectoryIfMissing True d
      BS.writeFile (d </> "s.jsonl") (sessionLine "2026-10-24T10:00:05Z")
      setMtime (d </> "s.jsonl") (tenOClock + 60)
      claudeSessionFile (dir </> "home") (dir </> "link") tenOClock `shouldReturn` Just (d </> "s.jsonl")

    it "reads messages and tool calls, timed from the first line" $ do
      evs <- claudeSessionEvents <$> BS.readFile claudeFixture
      map stKind evs `shouldBe` ["message", "tool_start", "tool_end", "message"]
      map stT evs `shouldBe` map Just [2.5, 3, 4, 10]
      field "name" (stData (evs !! 1)) `shouldBe` Just (String "Read")
      field "call_id" (stData (evs !! 2)) `shouldBe` Just (String "toolu_1")
      field "name" (stData (evs !! 2)) `shouldBe` Just (String "Read")
      field "ok" (stData (evs !! 2)) `shouldBe` Just (Bool True)

    it "skips lines it cannot read and never throws" $ do
      raw <- BS.readFile claudeFixture
      let evs = claudeSessionEvents ("not json\n{\"type\":\"summary\",\"summary\":\"x\"}\n" <> raw <> "{\"type\":\"assistant\",\"message\":7}\n{\"partial")
      map stKind evs `shouldBe` ["message", "tool_start", "tool_end", "message"]
      claudeSessionEvents "" `shouldBe` []

    it "never gives a negative t to a line older than the first" $
      map stT (claudeSessionEvents (sessionLine "2026-10-24T10:00:05Z" <> sessionLine "2026-10-24T10:00:00Z"))
        `shouldBe` [Just 0, Just 0]

  describe "Codex rollout files" $ do
    it "reads one tool pair and the assistant's message" $ do
      evs <- codexSessionEvents <$> BS.readFile codexFixture
      map stKind evs `shouldBe` ["tool_start", "tool_end", "message"]
      map stT evs `shouldBe` map Just [3, 5, 9]
      field "name" (stData (head evs)) `shouldBe` Just (String "shell")
      field "call_id" (stData (evs !! 1)) `shouldBe` Just (String "call_1")
      field "name" (stData (evs !! 1)) `shouldBe` Just (String "shell")

    it "finds the cwd's rollout under sessions modified after the start" $ withSystemTempDirectory "fisher-import" $ \dir -> do
      let d = dir </> ".codex" </> "sessions" </> "2026" </> "10" </> "24"
          p = d </> "rollout-2026-10-24T11-00-00-0199.jsonl"
      createDirectoryIfMissing True d
      copyFile codexFixture p
      BC.writeFile (d </> "notes.jsonl") "{}\n"
      m <- mtimeOf p
      codexSessionFile (dir </> ".codex") "/tmp/proj" (m - 60) `shouldReturn` Just p
      codexSessionFile (dir </> ".codex") "/tmp/proj" (m + 60) `shouldReturn` Nothing
      codexSessionFile (dir </> "missing") "/tmp/proj" (m - 60) `shouldReturn` Nothing

    it "skips rollouts of another cwd, even newer ones" $ withSystemTempDirectory "fisher-import" $ \dir -> do
      let d = dir </> "sessions" </> "2026" </> "10" </> "24"
          mine = d </> "rollout-a.jsonl"
          other = d </> "rollout-b.jsonl"
          nometa = d </> "rollout-c.jsonl"
      createDirectoryIfMissing True d
      BS.writeFile mine (rollout "/tmp/proj")
      BS.writeFile other (rollout "/tmp/other")
      BS.writeFile nometa "{\"type\":\"event_msg\"}\n"
      setMtime mine (tenOClock + 60)
      setMtime other (tenOClock + 120)
      setMtime nometa (tenOClock + 180)
      codexSessionFile dir "/tmp/proj" tenOClock `shouldReturn` Just mine
      codexSessionFile dir "/tmp/other/" tenOClock `shouldReturn` Just other
      codexSessionFile dir "/tmp/none" tenOClock `shouldReturn` Nothing

  describe "sessionHomeOf" $
    it "is CODEX_HOME for codex, else <home>/.codex, and the home for Claude Code" $ do
      sessionHomeOf "codex" "/m/codex-home" (Map.fromList [("CODEX_HOME", "/elsewhere/.codex")]) `shouldBe` "/elsewhere/.codex"
      sessionHomeOf "codex" "/m/codex-home" Map.empty `shouldBe` "/m/codex-home/.codex"
      sessionHomeOf "codex" "/m/codex-home" (Map.fromList [("CODEX_HOME", "")]) `shouldBe` "/m/codex-home/.codex"
      sessionHomeOf "claude-code" "/m/claude-home" (Map.fromList [("CODEX_HOME", "/x")]) `shouldBe` "/m/claude-home"

  describe "importTask" $ do
    it "imports a Claude Code session as a launch run" $ withLog $ \dir rl -> do
      p <- claudeHome dir
      m <- mtimeOf p
      importTask rl (view "claude-code" Complete (Just (m - 60)) (Just (m + 1))) "claude-code" (dir </> "claude-home") "/tmp/proj"
        `shouldReturn` Imported
      Just r <- getRun rl "swarm-af_0011aabb"
      (rrKind r, rrCapture r, rrBackend r, rrOutcome r) `shouldBe` (LaunchRun, Imported, Just "claude-code", Just "completed")
      rrEndedAt r `shouldBe` Just (m + 1)
      evs <- runEvents rl "swarm-af_0011aabb"
      [k | (_, _, k, _) <- evs] `shouldBe` ["message", "tool_start", "tool_end", "message", "outcome"]
      [(t, d) | (_, t, "outcome", d) <- evs] `shouldBe` [(61, object ["outcome" .= ("completed" :: Text), "reason" .= Null])]

    it "leaves a shell-only run when there is no session file" $ withLog $ \dir rl -> do
      importTask rl (view "claude-code" Failed (Just 100) (Just 200)) "claude-code" (dir </> "nothing") "/tmp/proj"
        `shouldReturn` ShellOnly
      Just r <- getRun rl "swarm-af_0011aabb"
      (rrKind r, rrCapture r, rrOutcome r, rrEndedAt r) `shouldBe` (LaunchRun, ShellOnly, Just "failed", Just 200)
      evs <- runEvents rl "swarm-af_0011aabb"
      [(t, k) | (_, t, k, _) <- evs] `shouldBe` [(100, "outcome")]

    it "leaves a shell-only run, with the reason, for a session file over 64 MB" $ withLog $ \dir rl -> do
      p <- claudeHome dir
      -- a sparse tail: the file is over the limit without 64 MB on disk
      withFile p ReadWriteMode (\h -> hSetFileSize h (sessionFileLimit + 1))
      m <- mtimeOf p
      importTask rl (view "claude-code" Complete (Just (m - 60)) (Just (m + 1))) "claude-code" (dir </> "claude-home") "/tmp/proj"
        `shouldReturn` ShellOnly
      Just r <- getRun rl "swarm-af_0011aabb"
      (rrCapture r, rrOutcome r) `shouldBe` (ShellOnly, Just "completed")
      evs <- runEvents rl "swarm-af_0011aabb"
      [k | (_, _, k, _) <- evs] `shouldBe` ["import_skipped", "outcome"]
      [d | (_, _, "import_skipped", d) <- evs] `shouldSatisfy` \case
        [d] -> maybe False (\case String x -> "64 MB" `T.isInfixOf` x; _ -> False) (field "reason" d)
        _ -> False

    it "imports a Codex rollout of the cwd from the CODEX_HOME given" $ withLog $ \dir rl -> do
      let d = dir </> "codex-home" </> ".codex" </> "sessions" </> "2026" </> "10" </> "24"
          p = d </> "rollout-2026-10-24T11-00-00-0199.jsonl"
      createDirectoryIfMissing True d
      copyFile codexFixture p
      m <- mtimeOf p
      importTask rl (view "codex" Complete (Just (m - 60)) (Just m)) "codex" (dir </> "codex-home" </> ".codex") "/tmp/proj"
        `shouldReturn` Imported
      evs <- runEvents rl "swarm-af_0011aabb"
      [k | (_, _, k, _) <- evs] `shouldBe` ["tool_start", "tool_end", "message", "outcome"]

  describe "importFinished" $ do
    it "does not import a fisher backend task" $ withSystemTempDirectory "fisher-import" $ \dir -> do
      let db = dir </> "runs.db"
      importFinished db "fisher" (dir </> "home") "/tmp/proj" (view "fisher" Complete (Just 1) (Just 2))
      withRunlog db $ \rl -> getRun rl "swarm-af_0011aabb" `shouldReturn` Nothing

    it "logs a claude-code task through the run log at the path" $ withSystemTempDirectory "fisher-import" $ \dir -> do
      let db = dir </> "runs.db"
      importFinished db "claude-code" (dir </> "home") "/tmp/proj" (view "claude-code" Cancelled (Just 1) (Just 2))
      withRunlog db $ \rl -> fmap (\r -> (rrCapture r, rrOutcome r)) <$> getRun rl "swarm-af_0011aabb"
        `shouldReturn` Just (ShellOnly, Just "cancelled")

    it "warns instead of failing when the run log cannot be opened" $ withSystemTempDirectory "fisher-import" $ \dir -> do
      BC.writeFile (dir </> "file") "x"
      importFinished (dir </> "file" </> "runs.db") "claude-code" dir "/tmp/proj" (view "claude-code" Complete (Just 1) (Just 2))
        `shouldReturn` ()
