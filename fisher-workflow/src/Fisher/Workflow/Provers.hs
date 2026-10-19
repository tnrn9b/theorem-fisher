{-# LANGUAGE TemplateHaskell #-}
-- | Rocq and Isabelle projects in the managed workflows: which commands
-- they take, the checks before a launch, Fisher's own skills for them
-- (embedded here and staged as the @fisher-provers@ marketplace or as a
-- Codex skill), and the prover's MCP server entry for each backend.
module Fisher.Workflow.Provers
  ( proverMarketplaceName
  , proverPluginId
  , resolveCommandFor
  , stripProverFlag
  , stripModelFlag
  , dropArgSeparator
  , proverSkillTree
  , stageProverSkills
  , stageCodexProverSkill
  , proverServerCommand
  , isabelleShim
  , isabelleShimName
  , stageIsabelleShim
  , viaIsabelleShim
  , withUvFallback
  , proverProjectEnv
  , coqcWrapper
  , stageCoqcWrapper
  , coqdepWrapper
  , stageCoqdepWrapper
  , RocqCoqc (..)
  , rocqServerCoqc
  , stageFisherBin
  , opamSwitchOf
  , proverServerPayload
  , serverPayloadWith
  , serverPayloadFor
  , nativeToolset
  , proverToolTimeoutFrom
  , proverSystemPrompt
  , clientServerPayload
  , proverToolTimeout
  , writeMcpConfigFor
  , writeCodexConfigFor
  , checkProverBinaries
  , proverConnectTimeout
  , prewarmProverServer
  , prewarmStartLine
  , coqVersion
  , proverStartupPrompt
  , writeProverStartupContext
  , writeProverCodexInstructions
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_, unless)
import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Char (isDigit, isSpace)
import qualified Data.Map.Strict as Map
import Data.List (isPrefixOf, nub)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (canonicalizePath, createDirectoryIfMissing, createFileLink, doesDirectoryExist, doesFileExist, getSymbolicLinkTarget, listDirectory, removeFile, renameFile, renamePath)
import System.Exit (ExitCode (..))
import System.Posix.Files (setFileMode)
import System.Posix.Process (getProcessID)
import qualified System.Process.Typed as P
import System.Timeout (timeout)
import System.FilePath (takeDirectory, takeFileName, (</>))

import Fisher.Prover (Prover (..), ProverKind (..), ServerSpec (..), binaryFix, findIsabelle, findRocqCompiler, prover, proverByName, proverTitle, versionBelow)
import Fisher.Compat (lookupCompatNamed)
import Fisher.Py (lookupKey)
import Data.Text.Encoding (decodeUtf8)
import Fisher.Workflow.Assets (removeExistingPath)
import Fisher.Workflow.Command (WorkflowSpec (..))
import Fisher.Workflow.Json (writeJsonObject)
import Fisher.Workflow.Run (runStaging, shlexQuote, whichPath)
import Fisher.Workflow.Types

proverMarketplaceName :: Text
proverMarketplaceName = "fisher-provers"

-- | @rocq\@fisher-provers@, @isabelle\@fisher-provers@.
proverPluginId :: ProverKind -> Text
proverPluginId k = pvName (prover k) <> "@" <> proverMarketplaceName

nameOf :: ProverKind -> Text
nameOf = pvName . prover

serverName :: ProverKind -> Text
serverName = svName . pvServer . prover

-- * Commands

-- | Lean keeps every workflow. Rocq and Isabelle take @/prove@ and
-- @/draft@, which go to the prover's skill; the others stop.
resolveCommandFor :: ProverKind -> WorkflowSpec -> Either WorkflowError WorkflowSpec
resolveCommandFor Lean4 ws = Right ws
resolveCommandFor k ws
  | wsKind ws `elem` ["prove", "draft"] =
      let backend = "/" <> nameOf k <> ":" <> wsKind ws
       in Right ws {wsBackend = if T.null (wsArgs ws) then backend else backend <> " " <> wsArgs ws}
  | otherwise = Left (UsageError (wsCanonical ws <> " is available for Lean projects only."))

-- | @--prover NAME@ or @--prover=NAME@ in the arguments: the kind it names,
-- and the spec without it (its backend command rebuilt from the rest).
stripProverFlag :: WorkflowSpec -> Either WorkflowError (Maybe ProverKind, WorkflowSpec)
stripProverFlag ws = case stripArgFlag "--prover" ws of
  Nothing -> bad
  Just (Nothing, _) -> Right (Nothing, ws)
  Just (Just v, ws') -> maybe bad (\k -> Right (Just k, ws')) (proverByName (T.toLower v))
  where
    bad = Left (UsageError "--prover takes lean4, rocq or isabelle.")

-- | Fisher only: @--model NAME@ or @--model=NAME@ in the arguments, the
-- model the workflow's child runs, and the spec without it.
stripModelFlag :: WorkflowSpec -> Either WorkflowError (Maybe Text, WorkflowSpec)
stripModelFlag ws = case stripArgFlag "--model" ws of
  Just (Just v, ws') | not (T.null (T.strip v)) -> Right (Just (T.strip v), ws')
  Just (Nothing, _) -> Right (Nothing, ws)
  _ -> Left (UsageError "--model takes a model name.")

-- | The first @FLAG VALUE@ or @FLAG=VALUE@ in the arguments: its value and
-- the spec without it (its backend command rebuilt from the rest), Nothing
-- when the flag has no value, and no value when the flag is not there.
stripArgFlag :: Text -> WorkflowSpec -> Maybe (Maybe Text, WorkflowSpec)
stripArgFlag flag ws = go (T.words (wsArgs ws)) []
  where
    go [] _ = Just (Nothing, ws)
    -- Fisher only: a literal -- ends the flags; what follows is text
    go ("--" : _) _ = Just (Nothing, ws)
    go (w : rest) seen
      | w == flag = case rest of
          (v : more) -> found v (reverse seen <> more)
          [] -> Nothing
      | Just v <- T.stripPrefix (flag <> "=") w = found v (reverse seen <> rest)
      | otherwise = go rest (w : seen)
    found v others = Just (Just v, withArgs others ws)

-- | The spec with these argument words, its backend command rebuilt.
withArgs :: [Text] -> WorkflowSpec -> WorkflowSpec
withArgs ws0 ws =
  let args = T.unwords ws0
      command = T.takeWhile (not . isSpace) (wsBackend ws)
   in ws {wsArgs = args, wsBackend = if T.null args then command else command <> " " <> args}

-- | Fisher only: the first literal @--@ out of the arguments, once the
-- flags before it are taken; the words after it stay as they are.
dropArgSeparator :: WorkflowSpec -> WorkflowSpec
dropArgSeparator ws = case break (== "--") (T.words (wsArgs ws)) of
  (before, _ : after) -> withArgs (before <> after) ws
  _ -> ws

-- * Skills

marketplaceJson, rocqPlugin, rocqSkill, rocqProve, rocqDraft, isaPlugin, isaSkill, isaProve, isaDraft :: ByteString
marketplaceJson = $(makeRelativeToProject "data/provers/marketplace.json" >>= embedFile)
rocqPlugin = $(makeRelativeToProject "data/provers/rocq/plugin.json" >>= embedFile)
rocqSkill = $(makeRelativeToProject "data/provers/rocq/SKILL.md" >>= embedFile)
rocqProve = $(makeRelativeToProject "data/provers/rocq/prove.md" >>= embedFile)
rocqDraft = $(makeRelativeToProject "data/provers/rocq/draft.md" >>= embedFile)
isaPlugin = $(makeRelativeToProject "data/provers/isabelle/plugin.json" >>= embedFile)
isaSkill = $(makeRelativeToProject "data/provers/isabelle/SKILL.md" >>= embedFile)
isaProve = $(makeRelativeToProject "data/provers/isabelle/prove.md" >>= embedFile)
isaDraft = $(makeRelativeToProject "data/provers/isabelle/draft.md" >>= embedFile)

-- | A prover's skill, prove command and draft command.
skillFiles :: ProverKind -> (ByteString, ByteString, ByteString)
skillFiles = \case
  Isabelle -> (isaSkill, isaProve, isaDraft)
  _ -> (rocqSkill, rocqProve, rocqDraft)

-- | The marketplace tree, path by path, in Claude Code's plugin layout:
-- the marketplace manifest at the root, and per plugin its manifest, its
-- skill and its two commands.
proverSkillTree :: [(FilePath, ByteString)]
proverSkillTree =
  (".claude-plugin" </> "marketplace.json", marketplaceJson)
    : concat
      [ [ (n </> ".claude-plugin" </> "plugin.json", manifest)
        , (n </> "skills" </> n </> "SKILL.md", skill)
        , (n </> "commands" </> "prove.md", prove)
        , (n </> "commands" </> "draft.md", draft) ]
      | (k, manifest) <- [(Rocq, rocqPlugin), (Isabelle, isaPlugin)]
      , let n = T.unpack (nameOf k)
            (skill, prove, draft) = skillFiles k ]

-- | Write files under a directory, replacing it when any file differs, so
-- that an up-to-date tree is left alone (two launches at once see the
-- same files).
writeTree :: FilePath -> [(FilePath, ByteString)] -> IO ()
writeTree root files = do
  same <- forM files $ \(rel, bytes) -> do
    let p = root </> rel
    exists <- doesFileExist p
    if exists then (== bytes) <$> BS.readFile p else pure False
  unless (and same) $ do
    removeExistingPath root
    forM_ files $ \(rel, bytes) -> do
      let p = root </> rel
      createDirectoryIfMissing True (takeDirectory p)
      BS.writeFile p bytes

-- | Stage the @fisher-provers@ marketplace at @<dir>/fisher-provers@ and
-- return that root. Every prover's plugin is in it, so one marketplace
-- serves every project; the caller installs the plugin of the kind given.
stageProverSkills :: ProverKind -> FilePath -> IO FilePath
stageProverSkills _ dir = do
  let root = dir </> T.unpack proverMarketplaceName
  writeTree root proverSkillTree
  pure root

-- | The prover's skill for Codex at @dest@ (its @.agents/skills/<prover>@):
-- @SKILL.md@ and the workflow guides under @commands/@.
stageCodexProverSkill :: ProverKind -> FilePath -> IO ()
stageCodexProverSkill k dest =
  let (skill, prove, draft) = skillFiles k
   in writeTree dest [("SKILL.md", skill), ("commands" </> "prove.md", prove), ("commands" </> "draft.md", draft)]

-- * The MCP server

-- | The server's command: the default, or with the override variable set,
-- @uvx --from <spec> <entry point>@, as the Lean server's spec works. For
-- Isabelle with the shim staged ('stageIsabelleShim'), the shim runs in
-- place of the entry point ('viaIsabelleShim'), the override's spec too.
proverServerCommand :: ProverKind -> [(String, String)] -> Maybe FilePath -> Either Text [Text]
proverServerCommand k env shim =
  let sv = pvServer (prover k)
      -- FISHER_..._MCP_SPEC, else GAUSS_..._MCP_SPEC; the message names
      -- the one given.
      (overrideName, overrideValue) = case lookupCompatNamed (svOverride sv) env of
        Just (n, v) -> (n, v)
        Nothing -> (svOverride sv, "")
      spec = T.strip (T.pack overrideValue)
      viaShim = case (k, shim) of
        (Isabelle, Just p) -> viaIsabelleShim p
        _ -> id
   in viaShim
        <$> if T.null spec
          then Right (svDefault sv)
          else
            if T.any isSpace spec
              then Left (T.pack overrideName <> " must be a package spec, without spaces.")
              else Right ["uvx", "--from", spec, last (svDefault sv)]

-- | Fisher's launcher for Isabelle-MCP, a Python file run by the server's
-- own Python: Isabelle-MCP 0.6.0 stops at startup off Linux (its
-- @FileWatcher.start@ logs a name bound only on Linux, after the watcher
-- has started), and the shim swallows that one error, then runs the
-- server's entry point (@isabelle_mcp.server:main@) with the arguments.
isabelleShim :: ByteString
isabelleShim = $(makeRelativeToProject "data/provers/isabelle/isabelle_mcp_shim.py" >>= embedFile)

isabelleShimName :: FilePath
isabelleShimName = "isabelle_mcp_shim.py"

-- | Write 'isabelleShim' as @<dir>/isabelle_mcp_shim.py@ (left alone when
-- it is already there, so that two launches at once see the same file)
-- and return its path. The directory holds only the shim, since Python
-- puts a script's directory first on its module path.
stageIsabelleShim :: FilePath -> IO FilePath
stageIsabelleShim dir = do
  createDirectoryIfMissing True dir
  let p = dir </> isabelleShimName
  same <- doesFileExist p >>= \case
    True -> (== isabelleShim) <$> BS.readFile p
    False -> pure False
  unless same $ do
    pid <- getProcessID
    let tmp = dir </> ("." <> isabelleShimName <> "." <> show pid)
    BS.writeFile tmp isabelleShim
    renameFile tmp p
  pure p

-- | A @uvx --from <spec> <entry point>@ command with @python <shim>@ in
-- place of the entry point.
viaIsabelleShim :: FilePath -> [Text] -> [Text]
viaIsabelleShim shim cmd = case reverse cmd of
  (_entry : front) -> reverse front <> ["python", T.pack shim]
  [] -> []

-- | @uvx@ as found on the launch's PATH, or @uv x@ when only uv is there.
withUvFallback :: Env -> [Text] -> IO [Text]
withUvFallback env = \case
  ("uvx" : rest) ->
    whichPath env "uvx" >>= \case
      Just p -> pure (T.pack p : rest)
      Nothing ->
        whichPath env "uv" >>= \case
          Just p -> pure (T.pack p : "x" : rest)
          Nothing -> pure ("uvx" : rest)
  cmd -> pure cmd

-- | The server's environment: the project root, under the name the server
-- reads (Rocq-MCP's @ROCQ_WORKSPACE@) and under Fisher's own name, and for
-- Rocq the coqc wrapper when one is staged ('rocqServerCoqc'), as
-- @ROCQ_COQC_BINARY@, the one binary Rocq-MCP takes from its environment
-- (it finds pet, dune and opam on PATH). Isabelle-MCP takes its project
-- root from its working directory or the client's roots instead.
proverProjectEnv :: ProverKind -> FilePath -> Maybe RocqCoqc -> [(Text, Text)]
proverProjectEnv k root coqc =
  let r = T.pack root
   in case k of
        Lean4 -> [("LEAN_PROJECT_PATH", r)]
        Rocq ->
          [("ROCQ_PROJECT_PATH", r), ("ROCQ_WORKSPACE", r)]
            <> [("ROCQ_COQC_BINARY", T.pack (rcWrapper c)) | Just c <- [coqc]]
            <> [("OPAM_SWITCH_PREFIX", T.pack p) | Just c <- [coqc], Just p <- [rcSwitch c]]
        Isabelle -> [("ISABELLE_PROJECT_PATH", r)]

-- | A @coqc@ that runs @rocq compile@ with its arguments.
coqcWrapper :: FilePath -> Text
coqcWrapper rocq = "#!/bin/sh\nexec " <> shlexQuote (T.pack rocq) <> " compile \"$@\"\n"

-- | A @coqdep@ that runs @rocq dep@ with its arguments (dune's coq mode
-- runs both from PATH).
coqdepWrapper :: FilePath -> Text
coqdepWrapper rocq = "#!/bin/sh\nexec " <> shlexQuote (T.pack rocq) <> " dep \"$@\"\n"

-- | Write the 'coqcWrapper' for this @rocq@ as @<dir>/coqc@, executable,
-- and return its path.
stageCoqcWrapper :: FilePath -> FilePath -> IO FilePath
stageCoqcWrapper = stageWrapper "coqc" coqcWrapper

-- | The 'coqdepWrapper' as @<dir>/coqdep@, as 'stageCoqcWrapper' does.
stageCoqdepWrapper :: FilePath -> FilePath -> IO FilePath
stageCoqdepWrapper = stageWrapper "coqdep" coqdepWrapper

-- | Written under a temporary name (@.<name>.<pid>@) and renamed into
-- place, so that a launch running at the same time never sees a partial
-- file.
stageWrapper :: FilePath -> (FilePath -> Text) -> FilePath -> FilePath -> IO FilePath
stageWrapper name body dir rocq = do
  createDirectoryIfMissing True dir
  pid <- getProcessID
  let p = dir </> name
      tmp = dir </> ("." <> name <> "." <> show pid)
  TIO.writeFile tmp (body rocq)
  setFileMode tmp 0o755
  renameFile tmp p
  pure p

-- | The coqc wrapper Rocq-MCP runs, and the opam switch the @rocq@ behind
-- it lives in. Rocq-MCP's @rocq_health@ names the switch from
-- @OPAM_SWITCH_PREFIX@ or from the coqc's path, and the wrapper's path
-- (in Fisher's managed state) names none, so the switch's prefix goes to
-- the server as @OPAM_SWITCH_PREFIX@ ('proverProjectEnv').
data RocqCoqc = RocqCoqc
  { rcWrapper :: FilePath
  , rcSwitch :: Maybe FilePath
  -- ^ the switch prefix, when the launch's environment sets no
  -- @OPAM_SWITCH_PREFIX@ of its own
  }
  deriving (Eq, Show)

-- | The coqc Rocq-MCP is to run when it cannot be the @coqc@ on PATH:
-- with only @rocq@ there (Rocq 9 without the compatibility binaries), the
-- wrapper staged under @dir@, with the opam switch of that @rocq@
-- ('opamSwitchOf') unless the environment sets @OPAM_SWITCH_PREFIX@.
-- 'Nothing' when coqc is on PATH, or neither.
rocqServerCoqc :: Env -> FilePath -> IO (Maybe RocqCoqc)
rocqServerCoqc env dir =
  findRocqCompiler (Map.toList env) >>= \case
    Just ("rocq", rocq) -> do
      w <- stageCoqcWrapper dir rocq
      sw <- case Map.lookup "OPAM_SWITCH_PREFIX" env of
        Just v | not (null v) -> pure Nothing
        _ -> opamSwitchOf rocq
      pure (Just (RocqCoqc w sw))
    _ -> pure Nothing

-- | The prefix of the opam switch a binary is installed in: the parent of
-- its @bin@ directory, when that holds opam's @.opam-switch@ directory
-- (as @~/.opam/<switch>@ and a local @_opam@ do). The path as found, then
-- with its links resolved.
opamSwitchOf :: FilePath -> IO (Maybe FilePath)
opamSwitchOf bin = do
  real <- either (const bin) id <$> try @SomeException (canonicalizePath bin)
  let prefixOf p = takeDirectory (takeDirectory p)
      isSwitch p = (takeFileName (takeDirectory p) == "bin" &&) <$> doesDirectoryExist (prefixOf p </> ".opam-switch")
  firstM isSwitch (nub [bin, real]) >>= pure . fmap prefixOf
  where
    firstM _ [] = pure Nothing
    firstM f (x : xs) = f x >>= \yes -> if yes then pure (Just x) else firstM f xs

-- | A directory holding only @fisher@, a link to this Fisher's executable
-- (@exe@), for the front of the session's PATH: the skills' @fisher check@
-- is then this Fisher, and no other program of the directory Fisher lives
-- in (a shared @bin@ may hold a @rocq@, @coqc@ or @isabelle@ of its own)
-- comes before the ones the launch checked. A link already pointing at
-- @exe@ is left alone; anything else in the directory is removed, except
-- the temporary links (@.fisher.<pid>@) of launches staging it at the same
-- time.
stageFisherBin :: FilePath -> FilePath -> IO FilePath
stageFisherBin dir exe = do
  createDirectoryIfMissing True dir
  let link = dir </> "fisher"
      inFlight n = ".fisher." `isPrefixOf` n
  others <- filter (\n -> n /= "fisher" && not (inFlight n)) <$> listDirectory dir
  forM_ others (removeExistingPath . (dir </>))
  current <- try @SomeException (getSymbolicLinkTarget link)
  unless (either (const False) (== exe) current) $ do
    pid <- getProcessID
    let tmp = dir </> (".fisher." <> show pid)
    _ <- try @SomeException (removeFile tmp)
    createFileLink exe tmp
    renamePath tmp link
  pure dir

-- | The default per-call timeout, in seconds.
proverToolTimeout :: Int
proverToolTimeout = 300

envObject :: ProverKind -> FilePath -> Maybe RocqCoqc -> Value
envObject k root coqc = object [K.fromText n .= v | (n, v) <- proverProjectEnv k root coqc]

-- | The timeout @fisher.provers.tool_timeout@ sets (in seconds, at least
-- one, a fraction dropped), else 'proverToolTimeout'.
proverToolTimeoutFrom :: Value -> Int
proverToolTimeoutFrom cfg =
  case lookupKey "fisher" cfg >>= lookupKey "provers" >>= lookupKey "tool_timeout" of
    Just (Number n) | n >= 1 -> floor (min n 1000000)
    _ -> proverToolTimeout

-- | How long, in seconds, a client waits for the server to start: a cold
-- @uvx@ resolves and installs the package first (fisher-mcp's default is
-- 60).
proverConnectTimeout :: Int
proverConnectTimeout = 300

-- | The entry Fisher's own agent reads (fisher-mcp's keys): the command,
-- the environment, the directory the server starts in (the project root,
-- where Isabelle-MCP takes its project from), the tools offered, the call
-- timeout in seconds, the connect timeout ('proverConnectTimeout'), a
-- reconnect when the server exits, and Fisher's own error texts (a timeout
-- names its seconds). The coqc is the staged
-- wrapper, if any ('proverProjectEnv').
serverPayloadFor :: ProverKind -> [Text] -> FilePath -> Maybe RocqCoqc -> [Text] -> Int -> Value
serverPayloadFor k cmd root coqc tools secs =
  object
    [ "type" .= ("stdio" :: Text)
    , "command" .= T.concat (take 1 cmd)
    , "args" .= drop 1 cmd
    , "env" .= envObject k root coqc
    , "cwd" .= root
    , "tools" .= object ["include" .= tools]
    , "timeout" .= secs
    , "connect_timeout" .= proverConnectTimeout
    , "reconnect_on_exit" .= True
    , "fisher_messages" .= True ]

-- | 'serverPayloadFor' with every tool Fisher offers and the default timeout.
serverPayloadWith :: ProverKind -> [Text] -> FilePath -> Maybe RocqCoqc -> Value
serverPayloadWith k cmd root coqc = serverPayloadFor k cmd root coqc (svTools (pvServer (prover k))) proverToolTimeout

-- | 'serverPayloadWith' for the default or overridden command, through
-- the Isabelle shim given, if any.
proverServerPayload :: ProverKind -> [(String, String)] -> Maybe FilePath -> FilePath -> Maybe RocqCoqc -> Either Text Value
proverServerPayload k env shim root coqc = (\cmd -> serverPayloadWith k cmd root coqc) <$> proverServerCommand k env shim

-- | The entry Claude Code takes in @.mcp.json@ and @.claude.json@: type,
-- command, args and env only.
clientServerPayload :: ProverKind -> [Text] -> FilePath -> Maybe RocqCoqc -> Value
clientServerPayload k cmd root coqc =
  object
    [ "type" .= ("stdio" :: Text)
    , "command" .= T.concat (take 1 cmd)
    , "args" .= drop 1 cmd
    , "env" .= envObject k root coqc ]

-- | @{"mcpServers": {"rocq": ...}}@.
writeMcpConfigFor :: ProverKind -> FilePath -> Value -> IO ()
writeMcpConfigFor k path server = do
  createDirectoryIfMissing True (takeDirectory path)
  writeJsonObject path (KM.fromList [("mcpServers", object [K.fromText (serverName k) .= server])])

tomlString :: Text -> Text
tomlString = T.pack . BL8.unpack . encode . String

-- | Codex's config: the instructions file and @[mcp_servers.<server>]@
-- with command, args, the project root as cwd (Isabelle-MCP's project
-- root), the tool timeout and env.
writeCodexConfigFor :: ProverKind -> FilePath -> FilePath -> [Text] -> FilePath -> Maybe RocqCoqc -> IO ()
writeCodexConfigFor k path instructions cmd root coqc = case cmd of
  (exe : args) ->
    TIO.writeFile path . T.intercalate "\n" $
      [ "model_instructions_file = " <> tomlString (T.pack instructions)
      , ""
      , "[mcp_servers." <> serverName k <> "]"
      , "command = " <> tomlString exe
      , "args = [" <> T.intercalate ", " (map tomlString args) <> "]"
      , "cwd = " <> tomlString (T.pack root)
      , "startup_timeout_sec = " <> T.pack (show proverConnectTimeout)
      , "tool_timeout_sec = " <> T.pack (show proverToolTimeout)
      , ""
      , "[mcp_servers." <> serverName k <> ".env]" ]
      <> [n <> " = " <> tomlString v | (n, v) <- proverProjectEnv k root coqc]
      <> [""]
  [] -> throwW (StagingError ("Invalid command for the managed " <> proverTitle k <> " MCP server."))

-- | The native toolset of a prover's sessions: @fisher-lean@,
-- @fisher-rocq@, @fisher-isabelle@.
nativeToolset :: ProverKind -> Text
nativeToolset = \case
  Lean4 -> "fisher-lean"
  k -> "fisher-" <> nameOf k

-- | The native backend's system prompt for a Rocq or Isabelle session: how
-- the skill's terms map in Fisher's own agent, then the embedded skill and
-- the workflow's command, front matter dropped. The flag says whether
-- @pet@ is on PATH (Rocq's interactive tools need it).
proverSystemPrompt :: ProverKind -> WorkflowSpec -> FilePath -> Bool -> Text
proverSystemPrompt k ws skillDir pet =
  let (skill, prove, draft) = skillFiles k
      n = nameOf k
      title = proverTitle k
      server = serverName k
      command = if wsKind ws == "draft" then draft else prove
      body b = case T.lines (decodeUtf8 b) of
        ("---" : rest) -> T.unlines (drop 1 (dropWhile (/= "---") rest))
        ls -> T.unlines ls
      tools = svTools (pvServer (prover k))
      example = case tools of
        (a : b : _) -> "`" <> a <> "` is `mcp_" <> server <> "_" <> a <> "`, `" <> b <> "` is `mcp_" <> server <> "_" <> b <> "`"
        _ -> ""
   in T.unlines $
        [ "# Fisher native " <> title <> " workflow session"
        , ""
        , "You run Fisher's " <> title <> " workflow on Fisher's own agent. This is how the skill's terms map here:"
        , ""
        , "- The " <> title <> " tools are the `" <> server <> "` MCP server's, named `mcp_" <> server <> "_<tool>` here: " <> example <> ", and so on."
        , "- Shell commands, `fisher check` among them, go through the `terminal` tool; files are read and written with `read_file`, `write_file` and `patch`, and searched with `search_files`."
        , "- There are no slash commands, hooks or subagents: do every step yourself."
        , "- The skill's own files are under `" <> T.pack skillDir <> "`; read them with `read_file` when a step points to them." ]
          <> [ "- `pet` is not on this machine's PATH, so only the compile and verify tools are offered: follow the skill's steps for a session without `pet`."
             | k == Rocq, not pet ]
          <> [ ""
             , "## The " <> n <> " skill (SKILL.md)"
             , ""
             , body skill
             , "## The `/" <> n <> ":" <> wsKind ws <> "` command"
             , ""
             , "Invocation: `" <> wsBackend ws <> "`"
             , ""
             , body command ]

-- * Launch checks

notFound :: ProverKind -> Text -> IO a
notFound k name = throwW (PreflightError (name <> " not found: " <> binaryFix k name))

-- | The first of the names on the launch's PATH.
findAny :: Env -> [Text] -> IO (Maybe FilePath)
findAny _ [] = pure Nothing
findAny env (n : ns) = whichPath env (T.unpack n) >>= maybe (findAny env ns) (pure . Just)

-- | The prover and uv must be on the launch's PATH, and Coq must be 8.17
-- or later. For Rocq that is what Rocq-MCP will run ('findRocqCompiler'):
-- @coqc@, or @rocq@ through the coqc wrapper. The version comes from
-- running it with @--version@. Isabelle may also be off PATH where
-- 'findIsabelle' looks: its @bin@ directory is returned, for the child's
-- PATH (Isabelle-MCP finds @isabelle@ on PATH).
checkProverBinaries :: ProverKind -> Env -> IO (Maybe FilePath)
checkProverBinaries k env = case k of
  Lean4 -> pure Nothing
  Rocq -> do
    exe <- findRocqCompiler (Map.toList env) >>= maybe (notFound k "coqc or rocq") (pure . snd)
    uv
    out <- runStaging [exe, "--version"] "Failed to read the Rocq version" (Just env) Nothing
    case coqVersion out of
      Just v | versionBelow v [8, 17] ->
        throwW (PreflightError ("Coq " <> T.intercalate "." (map (T.pack . show) v) <> " is older than 8.17, which Rocq-MCP does not support."))
      _ -> pure Nothing
  Isabelle -> do
    found <- findIsabelle (Map.toList env) >>= maybe (notFound k "isabelle") pure
    uv
    pure (if snd found then Nothing else Just (takeDirectory (fst found)))
  where
    uv = findAny env ["uvx", "uv"] >>= maybe (notFound k "uvx") (const (pure ()))

-- | Run the server's command once with @--help@, stdin closed, so that a
-- cold @uvx@ resolves and caches the package before a client waits on it
-- (as the Lean prewarm runs its runner with @--help@). Isabelle-MCP prints
-- its usage; Rocq-MCP ignores its arguments, starts on stdio and exits at
-- the end of its input. Bounded by 'proverConnectTimeout'. One line goes
-- to the note function before it runs ('prewarmStartLine'), and a failure
-- is a warning after it, never an error.
prewarmProverServer :: (Text -> IO ()) -> ProverKind -> Env -> [Text] -> IO ()
prewarmProverServer note k env cmd = case cmd of
  [] -> pure ()
  (exe : args) -> do
    let cfg =
          P.setStdin P.nullStream . P.setEnv (Map.toList env) $
            P.proc (T.unpack exe) (map T.unpack args <> ["--help"])
        warning why = Just ("Could not prewarm the " <> proverTitle k <> " MCP server (" <> why <> "); the session may wait for it to start.")
    note (prewarmStartLine k)
    r <- try @SomeException (timeout (proverConnectTimeout * 1000000) (P.readProcess cfg))
    mapM_ note $ case r of
      Left e -> warning (T.pack (show e))
      Right Nothing -> warning ("no answer in " <> T.pack (show proverConnectTimeout) <> " seconds")
      Right (Just (ExitSuccess, _, _)) -> Nothing
      Right (Just (ExitFailure n, _, err)) ->
        let detail = T.strip (T.pack (BL8.unpack err))
            lastLine = if T.null detail then "" else ": " <> last (T.lines detail)
         in warning ("exit " <> T.pack (show n) <> lastLine)

-- | @Starting the Rocq server (first run can take a minute)@.
prewarmStartLine :: ProverKind -> Text
prewarmStartLine k = "Starting the " <> proverTitle k <> " server (first run can take a minute)"

-- | The version in @coqc --version@'s banner (@... version 8.16.1@).
coqVersion :: Text -> Maybe [Int]
coqVersion out = case T.breakOn "version " out of
  (_, rest) | not (T.null rest) ->
    let v = T.takeWhile (\c -> isDigit c || c == '.') (T.drop 8 rest)
        parts = filter (not . T.null) (T.splitOn "." v)
     in if null parts then Nothing else Just (map (read . T.unpack) parts)
  _ -> Nothing

-- * Session texts

-- | The child's first prompt.
proverStartupPrompt :: ProverKind -> Text -> FilePath -> WorkflowSpec -> Maybe FilePath -> Text
proverStartupPrompt k backend ctx ws skills =
  let n = nameOf k
      intro = "You are in a Fisher-managed " <> proverTitle k <> " workflow session. Read the startup context at " <> shlexQuote (T.pack ctx) <> " first."
   in if backend == "codex"
        then
          T.unwords $
            [ intro
            , "Then explicitly invoke the `$" <> n <> "` skill and follow the requested " <> proverTitle k <> " workflow in the active project."
            , "Important: `/" <> n <> ":*` entries in the startup context are skill workflow names, not shell commands, so do not try to execute them in bash." ]
              <> concat
                [ [ "The staged skill entrypoint is " <> shlexQuote (T.pack (s </> "SKILL.md")) <> "."
                  , "The matching workflow guide is " <> shlexQuote (T.pack (s </> "commands" </> (T.unpack (wsKind ws) <> ".md"))) <> "." ]
                | Just s <- [skills] ]
        else intro <> " Then run this command inside the active project as your first workflow action: " <> wsBackend ws

-- | The startup context, named and placed as the Lean one is, with the
-- absolute @fisher check@ command of this Fisher. Returns the file written.
writeProverStartupContext
  :: Machine -> FilePath -> ProverKind -> Text -> FilePath -> FilePath -> FilePath -> Text -> WorkflowSpec
  -> FilePath -> FilePath -> FilePath -> FilePath -> IO FilePath
writeProverStartupContext m dir k backend projectRoot root cwd instruction ws skillDoc guide mcp fisherExe = do
  stamp <- mStamp m
  let path = dir </> (stamp <> "-" <> T.unpack (wsKind ws) <> ".md")
      t = T.pack
      title = proverTitle k
      codex = backend == "codex"
  TIO.writeFile path . T.intercalate "\n" $
    [ "# Fisher Managed " <> title <> " Workflow Session"
    , ""
    , "- Managed backend: `" <> backend <> "`"
    , "- Fisher command: `" <> wsCanonical ws <> "`"
    , "- Backend command: `" <> wsBackend ws <> "`"
    , "- Workflow kind: `" <> wsKind ws <> "`"
    , "- Project root: `" <> t projectRoot <> "`"
    , "- " <> title <> " root: `" <> t root <> "`"
    , "- Active working directory: `" <> t cwd <> "`"
    , "- " <> title <> " skill: `" <> t skillDoc <> "`"
    , "- Workflow guide: `" <> t guide <> "`"
    , "- MCP config: `" <> t mcp <> "` (server `" <> serverName k <> "`)"
    , "- Fisher check command: `" <> shlexQuote (t fisherExe) <> " check`"
    , ""
    , "## Workflow Request"
    , wsBackend ws
    , ""
    , "## Session Contract"
    , "- Work inside the current " <> title <> " project."
    , "- Use the `" <> serverName k <> "` MCP server for goals, searches and checking; the skill names its tools."
    , if codex
        then "- Invoke `$" <> nameOf k <> "` and follow the workflow guide as the requested workflow contract."
        else "- Run the managed backend workflow command exactly as requested before improvising."
    , "- A proof is done when `fisher check` passes on it; run it as the Fisher check command above."
    , "- Keep changes reproducible and explain blockers clearly if the workflow cannot proceed."
    , "" ]
      <> (if T.null instruction then [] else ["## Forwarded Arguments", T.strip instruction, ""])
      <> case mArxivScript m of
        Nothing -> []
        Just a ->
          [ "## arXiv Search"
          , ""
          , "To look up a paper or result before formalizing it: `python3 \"" <> t a <> "\" \"QUERY\"` (also `--author`, `--category`, `--id`)."
          , "" ]
  pure path

-- | Codex's instructions file for a prover session.
writeProverCodexInstructions
  :: FilePath -> ProverKind -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> WorkflowSpec -> IO ()
writeProverCodexInstructions path k ctx projectRoot root cwd skills ws = do
  let t = T.pack
      title = proverTitle k
      n = nameOf k
  TIO.writeFile path . T.intercalate "\n" $
    [ "# Fisher Managed " <> title <> " Workflow Instructions"
    , ""
    , "You are in a Fisher-managed " <> title <> " workflow session."
    , ""
    , "- Project root: `" <> t projectRoot <> "`"
    , "- " <> title <> " root: `" <> t root <> "`"
    , "- Active working directory: `" <> t cwd <> "`"
    , "- Installed " <> title <> " skill: `" <> t skills <> "`"
    , "- Startup context: `" <> t ctx <> "`"
    , ""
    , "## Codex Skill Contract"
    , "- The staged " <> title <> " workflow is exposed as the `$" <> n <> "` skill."
    , "- Skill entrypoint: `" <> t (skills </> "SKILL.md") <> "`"
    , "- Requested workflow guide: `" <> t (skills </> "commands" </> (T.unpack (wsKind ws) <> ".md")) <> "`"
    , "- `/" <> n <> ":*` names in the startup context are workflow labels from the skill docs, not shell commands."
    , "- Start by invoking `$" <> n <> "`, then follow the matching workflow guide before running project commands."
    , ""
    , "## Session Contract"
    , "- Work inside the current " <> title <> " project."
    , "- Use the `" <> serverName k <> "` MCP server for goals, searches and checking."
    , "- A proof is done when `fisher check` passes on it."
    , "- Keep changes reproducible and explain blockers clearly if the workflow cannot proceed."
    , "" ]
