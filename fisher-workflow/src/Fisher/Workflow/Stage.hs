-- | The files a managed session is staged with. Each function is the
-- matching @_stage_*@ or @_write_*@ helper in @gauss_cli/autoformalize.py@,
-- writing the same text.
module Fisher.Workflow.Stage
  ( stageTree
  , stageOptionalFileCopy
  , stageOptionalTreeCopy
  , stageClaudeCredentials
  , stageCodexAuth
  , managedServerNames
  , mcpServerPayload
  , writeMcpConfig
  , writeCodexConfig
  , writeCodexInstructions
  , writeStartupContext
  , workflowDocPath
  , ensureProjectToolPermissions
  , copyTree
  , copyTreeKeepingLinks
  ) where

import Control.Monad (forM_, unless, when)
import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import System.Directory hiding (isSymbolicLink)
import System.FilePath (takeDirectory, (</>))
import System.Posix.Files (createSymbolicLink, getSymbolicLinkStatus, isSymbolicLink, readSymbolicLink)

import Fisher.Workflow.Assets (removeExistingPath)
import Fisher.Workflow.Command (WorkflowSpec (..))
import Fisher.Workflow.Json
import Fisher.Workflow.Types

t :: FilePath -> Text
t = T.pack

-- | @shutil.copytree(src, dst)@: links followed, metadata copied.
copyTree :: FilePath -> FilePath -> IO ()
copyTree src dst = do
  createDirectoryIfMissing True dst
  entries <- listDirectory src
  forM_ entries $ \e -> do
    isDir <- doesDirectoryExist (src </> e)
    if isDir then copyTree (src </> e) (dst </> e) else copyFileWithMetadata (src </> e) (dst </> e)

-- | @shutil.copytree(src, dst, symlinks=True)@: links copied as links.
copyTreeKeepingLinks :: FilePath -> FilePath -> IO ()
copyTreeKeepingLinks src dst = do
  createDirectoryIfMissing True dst
  entries <- listDirectory src
  forM_ entries $ \e -> do
    st <- getSymbolicLinkStatus (src </> e)
    if isSymbolicLink st
      then readSymbolicLink (src </> e) >>= \target -> createSymbolicLink target (dst </> e)
      else do
        isDir <- doesDirectoryExist (src </> e)
        if isDir then copyTreeKeepingLinks (src </> e) (dst </> e) else copyFileWithMetadata (src </> e) (dst </> e)

-- | Copy a tree and mark it with its revision; an up-to-date copy is kept.
stageTree :: FilePath -> FilePath -> Text -> IO ()
stageTree src dst revision = do
  let marker = dst </> ".gauss-managed-revision"
  exists <- doesFileExist marker
  current <- if exists then T.strip <$> TIO.readFile marker else pure ""
  unless (exists && current == revision) $ do
    removeExistingPath dst
    copyTree src dst
    TIO.writeFile marker (revision <> "\n")

stageOptionalFileCopy :: FilePath -> FilePath -> IO Bool
stageOptionalFileCopy src dst = do
  ok <- doesFileExist src
  when ok $ do
    removeExistingPath dst
    createDirectoryIfMissing True (takeDirectory dst)
    copyFileWithMetadata src dst
  pure ok

stageOptionalTreeCopy :: FilePath -> FilePath -> IO Bool
stageOptionalTreeCopy src dst = do
  ok <- doesDirectoryExist src
  when ok $ do
    removeExistingPath dst
    createDirectoryIfMissing True (takeDirectory dst)
    copyTreeKeepingLinks src dst
  pure ok

-- | Add tools to a settings file's @permissions.allow@ list, keeping what
-- is there; rewrite only when something was added.
addAllowed :: FilePath -> [Text] -> Bool -> IO ()
addAllowed path needed alwaysWrite = do
  createDirectoryIfMissing True (takeDirectory path)
  d <- loadJsonObject path
  let perms = fromMaybe KM.empty (objectOf (KM.lookup "permissions" d))
      allow = case KM.lookup "allow" perms of
        Just (Array xs) -> V.toList xs
        _ -> []
      missing = [String n | n <- needed, String n `notElem` allow]
      perms' = KM.insert "allow" (Array (V.fromList (allow <> missing))) perms
  when (alwaysWrite || not (null missing)) $
    writeJsonObject path (KM.insert "permissions" (Object perms') d)

-- | @_ensure_project_tool_permissions@: the project's own
-- @.claude/settings.local.json@ allows the tools a managed session uses.
ensureProjectToolPermissions :: FilePath -> IO ()
ensureProjectToolPermissions root =
  addAllowed (root </> ".claude" </> "settings.local.json")
    ["Read", "Write", "Edit", "Bash(*)", "mcp__*", "WebSearch", "WebFetch"] False

-- | The MCP servers Fisher stages, one per prover: Lean's, Rocq's and
-- Isabelle's.
managedServerNames :: [Text]
managedServerNames = ["lean-lsp", "rocq", "isabelle"]

mcpServerPayload :: [Text] -> FilePath -> IO Value
mcpServerPayload runner leanRoot = case runner of
  (cmd : args@(_ : _)) ->
    pure (object ["type" .= ("stdio" :: Text), "command" .= cmd, "args" .= args, "env" .= object ["LEAN_PROJECT_PATH" .= leanRoot]])
  _ -> throwW (StagingError "Invalid uv runner configuration for managed Lean MCP server.")

writeMcpConfig :: FilePath -> [Text] -> FilePath -> IO ()
writeMcpConfig path runner leanRoot = do
  server <- mcpServerPayload runner leanRoot
  writeJsonObject path (KM.fromList [("mcpServers", object ["lean-lsp" .= server])])

-- | @_stage_claude_credentials@: the OAuth file (or the Keychain entry),
-- the user's @.claude.json@ with or without its API key, staged auth, MCP
-- servers, and an allow-everything @settings.local.json@.
stageClaudeCredentials
  :: Machine -> FilePath -> FilePath -> Maybe (Text, Text) -> Bool -> Bool -> Maybe (KM.KeyMap Value) -> IO ()
stageClaudeCredentials m realHome claudeHome authEnv copyOauth copyKey mcpServers = do
  let dir = claudeHome </> ".claude"
      keyPath = claudeHome </> ".claude.json"
      srcCreds = realHome </> ".claude" </> ".credentials.json"
      dstCreds = dir </> ".credentials.json"
  createDirectoryIfMissing True dir
  dstExists <- doesFileExist dstCreds
  when dstExists (removeFile dstCreds)
  when copyOauth $ do
    srcExists <- doesPathExist srcCreds
    if srcExists
      then copyFileWithMetadata srcCreds dstCreds
      else mKeychain m >>= mapM_ (TIO.writeFile dstCreds)
  managed <- loadJsonObject keyPath
  local <- loadJsonObject (realHome </> ".claude.json")
  let p1 = KM.union local managed
      p2 = if copyKey then p1 else KM.delete "primaryApiKey" p1
      apiKey = case authEnv of
        Just ("ANTHROPIC_API_KEY", v) | not (T.null (T.strip v)) -> Just (T.strip v)
        _ -> Nothing
      p3 = maybe p2 (\k -> KM.insert "primaryApiKey" (String k) p2) apiKey
      p4 = case mcpServers of
        Just servers | not (KM.null servers) ->
          -- one managed server per session: the other provers' entries a
          -- shared managed home kept from an earlier session go, the
          -- user's own stay
          let existing = KM.filterWithKey (\k _ -> K.toText k `notElem` managedServerNames)
                           (fromMaybe KM.empty (objectOf (KM.lookup "mcpServers" p3)))
           in KM.insert "mcpServers" (Object (KM.union servers existing)) p3
        _ -> p3
      p5 = if KM.member "theme" p4 then p4 else KM.insert "theme" (String "dark") p4
      p6 = KM.insert "hasCompletedOnboarding" (Bool True) p5
  writeJsonObject keyPath p6
  addAllowed (dir </> "settings.local.json")
    ["Read", "Write", "Edit", "Bash(*)", "WebSearch", "WebFetch", "mcp__*", "Bash"] True

-- | @_stage_codex_auth@: copy the user's auth.json, or write an API-key one.
stageCodexAuth :: FilePath -> Maybe FilePath -> Text -> IO ()
stageCodexAuth codexHome source apiKey = do
  createDirectoryIfMissing True codexHome
  let dst = codexHome </> "auth.json"
  exists <- doesFileExist dst
  when exists (removeFile dst)
  srcOk <- maybe (pure False) doesFileExist source
  case source of
    Just s | srcOk -> copyFileWithMetadata s dst
    _ -> unless (T.null apiKey) $
      writeJsonObject dst (KM.fromList [("auth_mode", String "apikey"), ("OPENAI_API_KEY", String apiKey)])

-- | @json.dumps@ of a string, which upstream uses as a TOML string.
tomlString :: Text -> Text
tomlString = T.pack . BL8.unpack . encode . String

writeCodexConfig :: FilePath -> FilePath -> [Text] -> FilePath -> IO ()
writeCodexConfig path instructions runner leanRoot = case runner of
  (cmd : args@(_ : _)) ->
    TIO.writeFile path . T.intercalate "\n" $
      [ "model_instructions_file = " <> tomlString (t instructions)
      , ""
      , "[mcp_servers.lean-lsp]"
      , "command = " <> tomlString cmd
      , "args = [" <> T.intercalate ", " (map tomlString args) <> "]"
      , ""
      , "[mcp_servers.lean-lsp.env]"
      , "LEAN_PROJECT_PATH = " <> tomlString (t leanRoot)
      , "" ]
  _ -> throwW (StagingError "Invalid uv runner configuration for managed Lean MCP server.")

workflowDocPath :: FilePath -> Text -> IO (Maybe FilePath)
workflowDocPath pluginRoot kind = do
  let p = pluginRoot </> "commands" </> (T.unpack kind <> ".md")
  ok <- doesFileExist p
  pure (if ok then Just p else Nothing)

writeCodexInstructions
  :: FilePath -> Maybe FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath
  -> Maybe WorkflowSpec -> IO ()
writeCodexInstructions path startup projectRoot leanRoot cwd skills plugin scripts refs workflow = do
  doc <- maybe (pure Nothing) (workflowDocPath plugin . wsKind) workflow
  TIO.writeFile path . T.intercalate "\n" $
    [ "# Fisher Managed Lean Workflow Instructions"
    , ""
    , "You are in a Fisher-managed Lean workflow session."
    , ""
    , "- Project root: `" <> t projectRoot <> "`"
    , "- Lean root: `" <> t leanRoot <> "`"
    , "- Active working directory: `" <> t cwd <> "`"
    , "- Installed Lean4 skill: `" <> t skills <> "`"
    , "- Lean4 plugin root: `" <> t plugin <> "`"
    , "- Lean4 scripts: `" <> t scripts <> "`"
    , "- Lean4 references: `" <> t refs <> "`" ]
    <> ["- Startup context: `" <> t s <> "`" | Just s <- [startup]]
    <> [ ""
       , "## Codex Skill Contract"
       , "- The staged Lean workflow is exposed as the `$lean4` skill."
       , "- Skill entrypoint: `" <> t (skills </> "SKILL.md") <> "`" ]
    <> ["- Requested workflow guide: `" <> t d <> "`" | Just d <- [doc]]
    <> [ "- `/lean4:*` names in the startup context are workflow labels from the skill docs, not shell commands."
       , "- Start by invoking `$lean4`, then follow the matching workflow guide before running project commands."
       , ""
       , "## Session Contract"
       , "- Work inside the current Lean project."
       , "- Use the installed `lean4` skill when you need proving workflow guidance."
       , "- Prefer Lean/LSP-first workflows and use the `lean-lsp` MCP server for navigation and diagnostics."
       , "- Keep changes reproducible and explain blockers clearly if the formalization cannot proceed."
       , "" ]

-- | @_write_startup_context@. Returns the file written.
writeStartupContext
  :: Machine -> FilePath -> Text -> FilePath -> FilePath -> FilePath -> Text -> WorkflowSpec
  -> FilePath -> FilePath -> Maybe FilePath -> Maybe FilePath -> IO FilePath
writeStartupContext m dir backend projectRoot leanRoot cwd instruction ws plugin mcp backendCfg skills = do
  stamp <- mStamp m
  doc <- workflowDocPath plugin (wsKind ws)
  let path = dir </> (stamp <> "-" <> T.unpack (wsKind ws) <> ".md")
      codex = backend == "codex"
      configLines
        | backendCfg == Just mcp = ["- Managed backend + MCP config: `" <> t mcp <> "`"]
        | otherwise = ["- Managed backend config: `" <> t c <> "`" | Just c <- [backendCfg]] <> ["- Managed Lean MCP config: `" <> t mcp <> "`"]
      arxiv = case mArxivScript m of
        Nothing -> []
        Just a ->
          [ "Bundled helper script: `" <> t a <> "`"
          , ""
          , "```bash"
          , "python3 \"" <> t a <> "\" \"sphere packing\"              # keyword search"
          , "python3 \"" <> t a <> "\" --author \"Terence Tao\"        # author search"
          , "python3 \"" <> t a <> "\" --category math.CO --sort date  # category"
          , "python3 \"" <> t a <> "\" --id 2402.03300               # fetch by ID"
          , "```"
          , "" ]
  TIO.writeFile path . T.intercalate "\n" $
    [ "# Fisher Managed Lean Workflow Session"
    , ""
    , "- Managed backend: `" <> backend <> "`"
    , "- Fisher command: `" <> wsCanonical ws <> "`"
    , "- Backend command: `" <> wsBackend ws <> "`"
    , "- Workflow kind: `" <> wsKind ws <> "`"
    , "- Project root: `" <> t projectRoot <> "`"
    , "- Lean root: `" <> t leanRoot <> "`"
    , "- Active working directory: `" <> t cwd <> "`"
    , "- Managed Lean asset root: `" <> t plugin <> "`" ]
    <> ["- Managed Lean workflow guide: `" <> t d <> "`" | Just d <- [doc]]
    <> ["- Managed Lean skill root: `" <> t s <> "`" | Just s <- [skills]]
    <> configLines
    <> ["", "## Workflow Request", wsBackend ws, ""]
    <> (if codex
          then [ "## Codex Skill Notes"
               , "- In Codex, the `/lean4:*` workflow request is a skill-level workflow name, not a shell executable."
               , "- Invoke `$lean4` explicitly before following this workflow."
               , "" ]
          else [])
    <> [ "## Session Contract"
       , "- Work inside the current Lean project."
       , "- Prefer Lean/LSP-first workflows and use the managed Lean MCP server."
       , if codex
           then "- Treat the managed backend workflow command as the requested workflow contract and follow it through the `$lean4` skill."
           else "- Run the managed backend workflow command exactly as requested before improvising."
       , "- Keep changes reproducible and explain blockers clearly if the workflow cannot proceed."
       , "" ]
    <> (if T.null instruction then [] else ["## Forwarded Arguments", T.strip instruction, ""])
    <> [ "## arXiv Search"
       , ""
       , "You have access to arXiv paper search. Use this when the user references a"
       , "paper, theorem, or result you need to look up before formalizing."
       , "" ]
    <> arxiv
    <> [ "Direct API (no dependencies):"
       , ""
       , "```bash"
       , "curl -s \"https://export.arxiv.org/api/query?search_query=all:QUERY&max_results=5\""
       , "```"
       , ""
       , "To read a paper abstract: fetch `https://arxiv.org/abs/<ID>`"
       , "To read a full paper PDF: fetch `https://arxiv.org/pdf/<ID>`"
       , "" ]
  pure path
