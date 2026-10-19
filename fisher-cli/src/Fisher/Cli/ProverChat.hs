-- | Fisher only: the native agent in Lean, Rocq and Isabelle projects. The
-- toolsets @fisher-lean@, @fisher-rocq@ and @fisher-isabelle@ (the file and
-- terminal tools and the prover's MCP server), the toolset @fisher chat@
-- takes by default in a Rocq or Isabelle project, and the prover servers it
-- runs: the project's own when no other is named, and Rocq-MCP's coqc tools
-- alone, with one warning, when coq-lsp's @pet@ is not on PATH.
module Fisher.Cli.ProverChat
  ( fisherToolsets
  , registerChatToolsets
  , chatProject
  , defaultToolsets
  , wantsProject
  , chatEnvNames
  , chatMcpServers
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>))

import Fisher.Mcp.Stdio (whichIn)
import Fisher.Project (Project (..), discoverProject)
import Fisher.Compat (envNames)
import Fisher.Prover (Prover (..), ProverKind (..), ServerSpec (..), binaryFix, petMissingWarning, prover, pvToolsFor)
import Fisher.Py.Value
import Fisher.Tools.Registry (Registry, Toolset (..), createCustomToolset)
import Fisher.Workflow.Provers (nativeToolset, proverServerCommand, proverToolTimeout, rocqServerCoqc, serverPayloadFor, stageIsabelleShim, withUvFallback)

fileTools :: [Text]
fileTools = ["read_file", "write_file", "patch", "search_files", "terminal", "process"]

-- | The native toolsets, by name. @fisher-lean@ is the Lean workflow
-- backend's (the managed @fisher@ backend): files, the terminal, web search
-- and the Lean MCP server; the others have the same file and terminal
-- tools and their prover's server.
fisherToolsets :: [(Text, Toolset)]
fisherToolsets =
  [ ( nativeToolset Lean4
    , Toolset "Fisher's managed Lean workflow tools: files, terminal, web search and the Lean MCP server"
        fileTools ["web", "mcp-lean-lsp"] )
  , (nativeToolset Rocq, proverSet Rocq)
  , (nativeToolset Isabelle, proverSet Isabelle) ]
  where
    proverSet k =
      let s = svName (pvServer (prover k))
       in Toolset ("Fisher's managed " <> titleOf k <> " workflow tools: files, terminal and the " <> s <> " MCP server")
            fileTools ["mcp-" <> s]
    titleOf = \case
      Rocq -> "Rocq"
      Isabelle -> "Isabelle"
      Lean4 -> "Lean"

-- | Register the native toolsets the session enables (only those, so
-- @all@ stays upstream's).
registerChatToolsets :: Registry -> [Text] -> IO ()
registerChatToolsets reg enabled =
  forM_ fisherToolsets $ \(n, ts) -> if n `elem` enabled then createCustomToolset reg n ts else pure ()

-- | The prover and prover root of the Rocq or Isabelle project the
-- directory is in (by its manifest); Nothing for a Lean project or none.
chatProject :: FilePath -> IO (Maybe (ProverKind, FilePath))
chatProject dir =
  try @SomeException (discoverProject dir) >>= \case
    Right p | projectProver p /= Lean4 -> pure (Just (projectProver p, projectProverRoot p))
    _ -> pure Nothing

-- | @-t@ when given, else the project's native toolset in a Rocq or
-- Isabelle project, else the configured default (Nothing).
defaultToolsets :: Maybe (ProverKind, FilePath) -> Maybe [Text] -> Maybe [Text]
defaultToolsets _ (Just ts) = Just ts
defaultToolsets (Just (k, _)) Nothing = Just [nativeToolset k]
defaultToolsets Nothing Nothing = Nothing

-- | Whether @fisher chat@ looks for the project it runs in: without
-- @-t@, or with a prover's toolset and no @--mcp-config@ to serve it.
wantsProject :: Maybe [Text] -> Bool -> Bool
wantsProject Nothing _ = True
wantsProject (Just ts) mcpConfig =
  not mcpConfig && any (`elem` ts) [nativeToolset Rocq, nativeToolset Isabelle]

-- | The variables of the chat's environment the prover servers are built
-- from: PATH and the servers' override variables, each under its new
-- (@FISHER_@) and old (@GAUSS_@) name.
chatEnvNames :: [String]
chatEnvNames = "PATH" : concat [[new, old] | k <- [Rocq, Isabelle], let (new, old) = envNames (svOverride (pvServer (prover k)))]

-- | The servers @fisher chat@ adds to @mcp_servers@, and the warnings to
-- print, from the chat's environment ('chatEnvNames'): those of
-- @--mcp-config@; in a Rocq or Isabelle project that names no server of
-- the prover's (there or in config.yaml), the prover's own, as the
-- workflow stages it (the override variable honoured, uvx or uv found on
-- PATH, @fisher.provers.tool_timeout@ read from the config), or a warning
-- when it cannot be built; and without @pet@ on PATH, a @rocq@ server
-- (from either place) offering only the coqc tools, with one warning.
-- With only @rocq@ on PATH, the prover's own Rocq server runs a coqc
-- wrapper staged under the state directory given (@rocq-bin@,
-- 'rocqServerCoqc'), and the Isabelle server starts through Fisher's shim
-- staged there (@isabelle-mcp@, 'stageIsabelleShim'); without one, the
-- bare entry point.
chatMcpServers :: [(String, String)] -> PyVal -> Maybe FilePath -> Maybe (ProverKind, FilePath) -> [(Text, PyVal)] -> IO ([(Text, PyVal)], [Text])
chatMcpServers env cfg stateDir project extra = do
  let configured = case get "mcp_servers" cfg of
        Just (PDict kvs) -> kvs
        _ -> []
      path = T.pack <$> lookup "PATH" env
  (own, ownWarnings) <- case project of
    Just (k, root)
      | let n = svName (pvServer (prover k)), n `notElem` map fst extra, n `notElem` map fst configured -> do
          shim <- case stateDir of
            Just d | k == Isabelle -> Just <$> stageIsabelleShim (d </> "isabelle-mcp")
            _ -> pure Nothing
          case proverServerCommand k env shim of
            Left e -> pure ([], [e])
            Right cmd0 -> do
              cmd <- withUvFallback (Map.fromList env) cmd0
              case cmd of
                ("uvx" : _) -> pure ([], ["uvx not found: " <> binaryFix k "uvx"])
                _ -> do
                  coqc <- case stateDir of
                    Just d | k == Rocq -> rocqServerCoqc (Map.fromList env) (d </> "rocq-bin")
                    _ -> pure Nothing
                  let v = serverPayloadFor k cmd root coqc (svTools (pvServer (prover k))) (toolTimeout cfg)
                  pure ([(n, either (const PNull) id (loads (BL.toStrict (A.encode v))))], [])
    _ -> pure ([], [])
  pet <- isJust <$> whichIn path "pet"
  let rocq = svName (pvServer (prover Rocq))
      servers = extra <> own
      -- config.yaml's rocq server, when no other replaces it, goes through
      -- the same check (the entry given here takes its place)
      fromConfig = [(n, v) | (n, v) <- configured, n == rocq, n `notElem` map fst servers, not pet]
      checked = [(n, if n == rocq && not pet then coqcOnly v else v) | (n, v) <- servers <> fromConfig]
      usesRocq = rocq `elem` (map fst servers <> map fst configured)
  pure (checked, ownWarnings <> [petMissingWarning | not pet, usesRocq])

-- | A Rocq-MCP entry limited to the tools that work without @pet@.
coqcOnly :: PyVal -> PyVal
coqcOnly = \case
  PDict kvs ->
    let tools = case lookup "tools" kvs of
          Just (PDict t) -> t
          _ -> []
        given = case lookup "include" tools of
          Just (PList xs) -> Just [s | PStr s <- xs]
          _ -> Nothing
        keep = maybe (pvToolsFor Rocq False) (\g -> filter (`elem` g) (pvToolsFor Rocq False)) given
        tools' = [kv | kv@(k, _) <- tools, k /= "include"] <> [("include", PList (map PStr keep))]
     in PDict ([kv | kv@(k, _) <- kvs, k /= "tools"] <> [("tools", PDict tools')])
  v -> v

-- | @fisher.provers.tool_timeout@ (seconds, at least one), else 300.
toolTimeout :: PyVal -> Int
toolTimeout cfg = case get "fisher" cfg >>= get "provers" >>= get "tool_timeout" of
  Just (PInt n) | n >= 1 -> fromIntegral (min n 1000000)
  Just (PFloat d) | d >= 1 -> floor (min d 1000000)
  _ -> proverToolTimeout
