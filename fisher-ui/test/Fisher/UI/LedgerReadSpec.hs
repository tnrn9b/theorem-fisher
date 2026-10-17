-- | The agent's ledger as the server reads it: read-only, and nothing when
-- the agent never wrote one.
module Fisher.UI.LedgerReadSpec (spec) where

import qualified Data.Aeson as A
import Database.SQLite.Simple
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.UI.Db

spec :: Spec
spec = describe "Fisher.UI.Db ledger readers" $ do
  it "reads nothing from a state.db without the ledger's tables, and writes nothing to it" $ withSystemTempDirectory "fisher-ui-ledger" $ \tmp -> do
    let db = tmp </> "state.db"
    c <- open db
    execute_ c "CREATE TABLE sessions (id TEXT PRIMARY KEY)"
    close c
    withRo db (\k -> ledgerEvents k "s1" 0) `shouldReturn` []
    withRo db (`ledgerRun` "tag") `shouldReturn` Nothing
    c2 <- open db
    query_ c2 "SELECT name FROM sqlite_master WHERE name LIKE 'ui_%'" `shouldReturn` ([] :: [Only String])
    close c2
    doesFileExist (db <> "-wal") `shouldReturn` False
  it "reads a session's events after a number, in order, and a tag's run" $ withSystemTempDirectory "fisher-ui-ledger" $ \tmp -> do
    let db = tmp </> "state.db"
    c <- open db
    execute_ c "CREATE TABLE ui_events (session_id TEXT NOT NULL, seq INTEGER NOT NULL, at REAL NOT NULL, kind TEXT NOT NULL, data TEXT NOT NULL, PRIMARY KEY (session_id, seq))"
    execute_ c "CREATE TABLE ui_runs (tag TEXT PRIMARY KEY, session_id TEXT NOT NULL, pid INTEGER NOT NULL, started REAL NOT NULL)"
    execute_ c "INSERT INTO ui_events VALUES ('s1', 2, 1.0, 'end', '{\"status\":\"completed\"}'), ('s1', 1, 0.5, 'thinking', '{\"label\":\"x\"}'), ('s2', 1, 0.7, 'thinking', '{}')"
    execute_ c "INSERT INTO ui_runs VALUES ('run-1', 's1', 99, 0.1)"
    close c
    withRo db (\k -> ledgerEvents k "s1" 0)
      `shouldReturn` [(1, "thinking", A.object [("label", A.String "x")]), (2, "end", A.object [("status", A.String "completed")])]
    map (\(n, _, _) -> n) <$> withRo db (\k -> ledgerEvents k "s1" 1) `shouldReturn` [2]
    withRo db (`ledgerRun` "run-1") `shouldReturn` Just ("s1", 99)
    withRo db (`ledgerRun` "other") `shouldReturn` Nothing
