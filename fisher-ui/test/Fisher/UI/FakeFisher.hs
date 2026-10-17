-- | A fake @fisher@ for the live routes' tests and the page tests' server:
-- a shell script that writes what the agent writes with
-- @FISHER_UI_LEDGER=1@ (@ui_runs@, then @ui_events@) through one sqlite3
-- connection in WAL mode, held open while it runs, as the agent holds its
-- own.
module Fisher.UI.FakeFisher
  ( fakeScript
  , ledgerSchema
  , pageRun
  , searchRun
  ) where

import Data.Text (Text)
import qualified Data.Text as T

ledgerSchema :: [Text]
ledgerSchema =
  [ "CREATE TABLE IF NOT EXISTS ui_events (session_id TEXT NOT NULL, seq INTEGER NOT NULL, at REAL NOT NULL, kind TEXT NOT NULL, data TEXT NOT NULL, PRIMARY KEY (session_id, seq))"
  , "CREATE TABLE IF NOT EXISTS ui_runs (tag TEXT PRIMARY KEY, session_id TEXT NOT NULL, pid INTEGER NOT NULL, started REAL NOT NULL)" ]

-- | The script: its arguments, cwd and FISHER_UI_LEDGER to files in
-- GAUSS_HOME, then the body. In the body @hdr@ prints the SQL that makes
-- the tables and records the run (with a compression child @c_<tag>@ of
-- the session @s_<tag>@ when the sessions table is the test's own), @ev N
-- KIND JSON@ an event of @s_<tag>@, and @sql@ is the one connection.
fakeScript :: Bool -> Text -> Text
fakeScript ownSessions body =
  T.unlines $
    [ "#!/bin/sh"
    , "printf '%s\\n' \"$@\" > \"$GAUSS_HOME/args-$FISHER_UI_TAG.txt\""
    , "pwd > \"$GAUSS_HOME/cwd-$FISHER_UI_TAG.txt\""
    , "echo \"$FISHER_UI_LEDGER\" > \"$GAUSS_HOME/ledger-$FISHER_UI_TAG.txt\""
    , "SID=\"s_$FISHER_UI_TAG\""
    , "ev() { echo \"INSERT INTO ui_events VALUES ('$SID', $1, 0, '$2', '$3');\"; }"
    , "hdr() {"
    , "  echo '.timeout 5000'; echo 'PRAGMA journal_mode=WAL;'" ]
      <> ["  echo \"" <> T.replace "\"" "\\\"" q <> ";\"" | q <- ledgerSchema]
      <> concat [ [ "  echo \"CREATE TABLE IF NOT EXISTS sessions (id TEXT PRIMARY KEY, parent_session_id TEXT);\""
         , "  echo \"INSERT INTO sessions VALUES ('$SID', NULL), ('c_$FISHER_UI_TAG', '$SID');\"" ] | ownSessions ]
      <> [ "  echo \"INSERT INTO ui_runs VALUES ('$FISHER_UI_TAG', '$SID', $$, 0);\""
         , "}"
         , "sql() { sqlite3 \"$GAUSS_HOME/state.db\" > /dev/null; }"
         , body ]

-- | The page tests' run: three events 0.3 s apart, then each message the
-- inbox gets comes back as an inbox event, until it is stopped.
pageRun :: Text
pageRun =
  T.unlines
    [ "{ hdr; sleep 0.3; ev 1 tool_start '{\"name\":\"read_file\",\"args\":{\"path\":\"theories/Demo.v\"}}'"
    , "  sleep 0.3; ev 2 tool_end '{\"name\":\"read_file\",\"callId\":\"call_1\",\"preview\":\"Theorem t\",\"ok\":true}'"
    , "  sleep 0.3; ev 3 message '{\"role\":\"assistant\",\"text\":\"Reading the file first.\"}'"
    , "  n=3; seen=0; f=\"$GAUSS_HOME/fisher/inbox/$SID.jsonl\""
    , "  while :; do sleep 0.2"
    , "    if [ -f \"$f\" ]; then c=$(wc -l < \"$f\"); while [ $seen -lt $c ]; do seen=$((seen+1)); n=$((n+1))"
    , "      t=$(sed -n \"${seen}p\" \"$f\" | sed 's/^{\"text\": *\"//; s/\"}$//')"
    , "      ev $n inbox \"{\\\"text\\\":\\\"$t\\\"}\"; done; fi"
    , "  done; } | sql" ]

-- | The page tests' search run, as @fisher search FILE ... --out DIR@: a
-- flat run on the goal Demo.a whose events come a line at a time over
-- about four seconds (one line in two pieces), then its report.
searchRun :: Text
searchRun =
  T.unlines
    [ "while [ \"$1\" != \"--out\" ]; do shift; done; out=\"$2\"; mkdir -p \"$out\"; e=\"$out/events.jsonl\"; sleep 0.5"
    , "printf '%s\\n' '{\"event\":\"auto missed\",\"goal\":\"Demo.a\",\"seconds\":1,\"t\":1}' >> \"$e\"; sleep 0.5"
    , "printf '%s\\n' '{\"attempt\":1,\"backend\":\"gauss\",\"event\":\"launched\",\"goal\":\"Demo.a\",\"t\":1}' >> \"$e\"; sleep 1.5"
    , "printf '%s' '{\"attempt\":1,\"backend\":\"gauss\",\"event\":\"finished\",' >> \"$e\"; sleep 0.5"
    , "printf '%s\\n' '\"goal\":\"Demo.a\",\"outcome\":\"closed\",\"t\":3}' >> \"$e\""
    , "printf '%s\\n' '{\"attempt\":1,\"event\":\"closed\",\"goal\":\"Demo.a\",\"t\":3}' >> \"$e\"; sleep 0.5"
    , "printf '{\"mode\":\"swarm\",\"file\":\"Demo/Flat.lean\",\"project\":\"%s\",\"elapsed_s\":3,\"goals_total\":1,\"goals_closed\":1,\"auto_requested\":true,\"goals\":[{\"goal\":\"Demo.a\",\"status\":\"closed\",\"closedBy\":\"model\",\"attempts\":[\"gauss\"],\"by_attempt\":1,\"by_backend\":\"gauss\",\"closed_at_s\":3}],\"attempts\":[]}\\n' \"$(pwd)\" > \"$out/report.json\"" ]
