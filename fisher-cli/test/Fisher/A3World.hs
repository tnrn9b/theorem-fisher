-- | The scratch world @tools/corpus/a3.py@ recorded in (@setup_world@):
-- fake git, uvx, rg, claude and codex; a HOME with Claude credentials; an
-- initialised project; FISHER_HOME. Roots have the recordings' lengths, so
-- the tables and banners that show paths lay out the same.
module Fisher.A3World
  ( World (..)
  , makeWorld
  , withWorldEnv
  , projectSummaryAt
  , normalizeWorld
  , maskTimes
  , fakeTool
  ) where

import Control.Exception (SomeException, bracket, try)
import Control.Monad (forM, forM_)
import Data.Char (isDigit, isHexDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.Posix.Files (setFileMode)

import Fisher.A3Golden (mask, padded)
import Fisher.Project (Project, defaultInitOptions, discoverProject, formatProjectSummary, initName, initializeProject)

data World = World
  { wRoot :: FilePath
  , wFisherHome :: FilePath
  , wHome :: FilePath
  , wBin :: FilePath
  }

fakeTool :: Text
fakeTool = T.unlines
  [ "#!/bin/bash"
  , "export PATH=\"/usr/bin:/bin:$PATH\""
  , "name=\"$(basename \"$0\")\""
  , "if [ \"$name\" = git ] && [ \"$1\" = clone ]; then"
  , "  for last in \"$@\"; do :; done"
  , "  p=\"$last/plugins/lean4\""
  , "  mkdir -p \"$p/skills/lean4/references\" \"$p/lib/scripts\" \"$p/commands\" \"$p/.claude-plugin\" \"$last/.claude-plugin\" \"$last/.git\""
  , "  printf '# lean4 skill\\n' > \"$p/skills/lean4/SKILL.md\""
  , "  printf '{\"name\": \"lean4\", \"version\": \"2.0.0\"}' > \"$p/.claude-plugin/plugin.json\""
  , "  printf '{\"name\": \"lean4-skills\"}' > \"$last/.claude-plugin/marketplace.json\""
  , "  exit 0"
  , "fi"
  , "if [ \"$name\" = git ] && [ \"$3\" = rev-parse ]; then echo def456; exit 0; fi"
  , "if [ \"$name\" = claude ]; then"
  , "  if [ \"$1 $2\" = \"plugin list\" ]; then"
  , "    printf '[{\"id\": \"lean4@lean4-skills\", \"installPath\": \"%s\"}]\\n' \"$FAKE_PLUGIN_DIR\"; exit 0"
  , "  fi"
  , "  if [ \"$1 $2\" = \"plugin install\" ]; then"
  , "    mkdir -p \"$HOME/.claude/plugins\""
  , "    printf '{\"version\": 2, \"plugins\": {\"lean4@lean4-skills\": [{\"installPath\": \"%s\", \"version\": \"9.9.9\"}]}}' \"$FAKE_PLUGIN_DIR\" > \"$HOME/.claude/plugins/installed_plugins.json\""
  , "    exit 0"
  , "  fi"
  , "fi"
  , "exit 0" ]

-- | Under two scratch bases whose paths part within ten characters (say
-- /tmp and /var/tmp), so the masks cannot take one root for the other.
makeWorld :: FilePath -> FilePath -> IO World
makeWorld tmp tmpg = do
  root <- padded tmp 100
  gh <- padded tmpg 90
  let b = root </> "bin"
      home = root </> "home"
      proj = root </> "project"
  createDirectoryIfMissing True b
  forM_ ["git", "uvx", "rg", "claude", "codex"] $ \n -> do
    TIO.writeFile (b </> n) fakeTool
    setFileMode (b </> n) 0o755
  createDirectoryIfMissing True (home </> ".claude")
  TIO.writeFile (home </> ".claude" </> ".credentials.json") "{\"claudeAiOauth\": {\"accessToken\": \"tok\"}}"
  TIO.writeFile (home </> ".claude.json") "{\"userID\": \"u1\", \"projects\": {}}"
  createDirectoryIfMissing True (proj </> "Math")
  TIO.writeFile (proj </> "lakefile.lean") "-- lean\n"
  _ <- initializeProject proj defaultInitOptions {initName = Just "Demo Project"}
  createDirectoryIfMissing True (root </> "outside")
  createDirectoryIfMissing True (root </> "fake-plugin" </> "skills" </> "lean4")
  pure (World root gh home b)

-- | The world's PATH, HOME, FISHER_HOME and plugin dir in this process, as
-- the harness set them; put back afterwards.
withWorldEnv :: World -> [(String, String)] -> IO a -> IO a
withWorldEnv w extra act = bracket set restore (const act)
  where
    vars =
      [ ("PATH", wBin w), ("HOME", wHome w), ("FISHER_HOME", wFisherHome w)
      , ("FAKE_PLUGIN_DIR", wRoot w </> "fake-plugin")
        -- the recordings are upstream's, whose MCP server is uvx lean-lsp-mcp
      , ("FISHER_LEAN_MCP", "uvx") ] <> extra
    set = forM (map fst vars <> ["FISHER_AUTOFORMALIZE_BACKEND", "TERMINAL_CWD", "TERMINAL_ENV", "TERMINAL_TIMEOUT"]) $ \k -> do
      old <- lookupEnv k
      maybe (unsetEnv k) (setEnv k) (lookup k vars)
      pure (k, old)
    restore = mapM_ (\(k, old) -> maybe (unsetEnv k) (setEnv k) old)

projectSummaryAt :: World -> FilePath -> IO (Maybe Text)
projectSummaryAt _ cwd = do
  r <- try (discoverProject cwd) :: IO (Either SomeException Project)
  case r of
    Right p -> Just <$> formatProjectSummary p (Just cwd)
    Left _ -> pure Nothing

-- | The harness's @norm@: scratch roots masked, then the clock stamps.
normalizeWorld :: World -> Text -> Text
normalizeWorld w = maskTimes . mask (T.pack (wRoot w)) "<TMP>" . mask (T.pack (wFisherHome w)) "<FISHER_HOME>"

-- | @DATETIME@, @STAMP@ and @SESSION@, in that order.
maskTimes :: Text -> Text
maskTimes = sub sessionAt "<SESSION>" . sub stampAt "<STAMP>" . sub datetimeAt "<DATETIME>"
  where
    sub matchAt tag t = go t
      where
        go s
          | T.null s = s
          | Just n <- matchAt s = tag <> go (T.drop n s)
          | otherwise = T.cons (T.head s) (go (T.tail s))
    shape pat s =
      let n = length pat
          cs = T.unpack (T.take n s)
       in if length cs == n && and (zipWith ok pat cs) then Just n else Nothing
    ok 'd' c = isDigit c
    ok 'h' c = isHexDigit c && (isDigit c || c `elem` ['a' .. 'f'])
    ok p c = p == c
    datetimeAt = shape "dddd-dd-dd dd:dd:dd"
    stampAt = shape "dddddddd-dddddd"
    sessionAt = shape "dddddddd_dddddd_hhhhhh"
