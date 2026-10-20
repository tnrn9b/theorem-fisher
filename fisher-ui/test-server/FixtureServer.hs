-- | The page tests' server: the read routes over Task 2's fixtures (the
-- scripted FISHER_HOME of "Fisher.UI.Fixture" and the copied bench g and h
-- runs) with the embedded ui/dist, on 127.0.0.1.
--
-- It is a test suite so that it is built with the tests and never
-- installed; with no arguments (as @stack test@ runs it) it exits at once.
-- @fisher-ui-fixture-server serve PORT@ (0: any free port) prints
-- @http://127.0.0.1:<port>/?t=<token>@ (the fixture's fixed token) and
-- serves until killed. The bench h gate is a stub report.py that prints a
-- fixed line, so the page shows pending, then that line. A run the page
-- starts is the fake fisher of "Fisher.UI.FakeFisher" ('pageRun'): three
-- events, then each message back as an inbox event, until stopped. A
-- step's Lean check is a fake lake: an error on a line with BAD. The goal
-- board's search root is a copy of test/fixtures/search whose runs name a
-- scratch Lean project (its path printed as @Search project: DIR@), and a
-- search the page starts is the fake of 'searchRun'. The home has a
-- config.yaml and a .env for the settings page.
module Main (main) where

import Control.Exception (bracket)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.Socket (close)
import Network.Wai.Handler.Warp (defaultSettings, runSettingsSocket, setHost, setPort)
import Control.Monad (forM_)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import System.Directory (canonicalizePath, createDirectoryIfMissing, setModificationTime)
import System.Process (readProcess)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (setFileMode)

import Fisher.UI.Access (Gate (..))
import Fisher.UI.Api.Live (LiveEnv (..), newLiveEnv, stopRuns)
import Fisher.UI.Api.Runs (stopGates)
import Fisher.UI.Db (UiEnv (..))
import Fisher.UI.Api.Search (newSearchEnv, stopSearches)
import Fisher.UI.FakeFisher (fakeScript, pageRun, searchRun)
import Fisher.UI.Fixture (Fixture (..), gate, withFixture)
import Fisher.UI.Server (allApis, bindUi, onTerminate, serve, uiUrl)
import Fisher.UI.TestDirs (fixturesDir)

gateLine :: B.ByteString
gateLine = "auto closes more, or as many for at least 25% fewer dollars: PASS"

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- SIGTERM stops it as it stops fisher ui: the runs it started go too
    ["serve", p] -> onTerminate (serveOn (read p))
    _ -> putStrLn "fisher-ui-fixture-server: pass serve PORT to serve the fixtures"

serveOn :: Int -> IO ()
serveOn want =
  withFixture $ \fx -> withSystemTempDirectory "fisher-ui-repo" $ \repo -> do
    let dir = repo </> "tools" </> "bench" </> "h"
    createDirectoryIfMissing True dir
    B.writeFile (dir </> "report.py") ("import time\ntime.sleep(1)\nprint('# H')\nprint('" <> gateLine <> "')\n")
    let env = (fxEnv fx) {ueBench = fixturesDir </> "bench", ueRepo = repo}
        fake = repo </> "fake-fisher"
        ledgerFake = repo </> "fake-ledger"
        lake = repo </> "fake-lake"
    TIO.writeFile ledgerFake (fakeScript False pageRun)
    -- fisher search is the search fake; anything else the ledger fake
    TIO.writeFile fake ("#!/bin/sh\nif [ \"$1\" = search ]; then\n" <> searchRun <> "exit 0\nfi\nexec " <> T.pack (show ledgerFake) <> " \"$@\"\n")
    -- the goal board's runs, over a scratch Lean project
    repoC <- canonicalizePath repo
    let root = repoC </> "search"
        proj = repoC </> "lean"
    createDirectoryIfMissing True root
    createDirectoryIfMissing True (proj </> "Demo")
    B.writeFile (proj </> "lakefile.lean") "import Lake\n"
    B.writeFile (proj </> "Demo" </> "Graph.lean") "namespace Demo\n\ntheorem main_thm : 1 + 1 = 2 := by\n  sorry\n\ntheorem side_thm : 2 + 2 = 4 := by\n  sorry\n\nend Demo\n"
    B.writeFile (proj </> "Demo" </> "Flat.lean") "namespace Demo\n\ntheorem a : True := by\n  sorry\n\nend Demo\n"
    -- untouched since before the fixture runs (apply refuses a file written after its run started)
    mapM_ (\f -> setModificationTime (proj </> "Demo" </> f) (UTCTime (fromGregorian 2026 10 1) 0)) ["Graph.lean", "Flat.lean"]
    _ <- readProcess "cp" ["-R", fixturesDir </> "search" </> ".", root] ""
    forM_ ["20261016-090000", "20261016-080000"] $ \d -> do
      let p = root </> d </> "report.json"
      t <- TIO.readFile p
      TIO.writeFile p (T.replace "\"/fixture/lean\"" (T.pack (show proj)) t)
    -- the settings page's config.yaml and .env (a made-up key ending 1234)
    B.writeFile (fxHome fx </> "config.yaml") "model:\n  default: example/model-a\n  provider: openrouter\nfisher:\n  context: clm\n  autoformalize:\n    model: example/model-b\n"
    B.writeFile (fxHome fx </> ".env") "# fixture\nOTHER=1\nOPENROUTER_API_KEY=sk-fixture-0000000000001234\nTERMINAL_SSH_HOST=box.local\n"
    -- a step's Lean check: an error on a line with BAD, else clean
    TIO.writeFile lake "#!/bin/sh\nif grep -n BAD \"$3\" > /dev/null; then echo \"$3:$(grep -n BAD \"$3\" | head -1 | cut -d: -f1):2: error: unknown tactic\"; exit 1; fi\nexit 0\n"
    mapM_ (`setFileMode` 0o755) [fake, ledgerFake, lake]
    bracket (bindUi want) (close . snd) $ \(port, sock) ->
      bracket (pure env) stopGates $ \e ->
        bracket ((\l -> l {leLake = lake}) <$> newLiveEnv e fake) stopRuns $ \live ->
         bracket (newSearchEnv live root) stopSearches $ \search -> do
          putStrLn ("Fisher UI fixture: " <> T.unpack (uiUrl port (gToken gate)))
          putStrLn ("Search project: " <> proj)
          hFlush stdout
          runSettingsSocket (setHost "127.0.0.1" (setPort port defaultSettings)) sock (serve gate {gPort = port} (allApis live search))
