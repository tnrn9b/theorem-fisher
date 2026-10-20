-- | Which backend and auth mode the managed workflows use, from the
-- environment and @fisher.autoformalize@ (@_resolve_backend_name@,
-- @_resolve_auth_mode@ and @_resolve_uv_runner@ in
-- upstream @cli/autoformalize.py@).
module Fisher.Workflow.Settings
  ( Backend (..)
  , backendName
  , supportedBackends
  , normalizeBackendName
  , resolveBackend
  , AuthMode (..)
  , authModeName
  , resolveAuthMode
  , autoformalizeConfig
  , resolveLaunchModel
  , leanLspMcpSpec
  , resolveUvRunner
  ) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (getExecutablePath)
import System.FilePath ()

import Fisher.Py (lookupKey, pyStrOr)
import Fisher.Which (whichIn)

-- | 'Fisher' is Fisher's own: the workflows run on Fisher's native agent
-- (@fisher chat@), with the same skill, Lean MCP server and model as the
-- claude-code backend. Upstream has no such backend.
data Backend = ClaudeCode | Codex | Fisher
  deriving (Eq, Show, Enum, Bounded)

backendName :: Backend -> Text
backendName = \case
  ClaudeCode -> "claude-code"
  Codex -> "codex"
  Fisher -> "fisher"

-- | Upstream's backends, the ones its messages list ("must be one of",
-- "Available"). 'Fisher' is accepted as well but not listed, so those
-- messages stay upstream's.
supportedBackends :: [Backend]
supportedBackends = [ClaudeCode, Codex]

aliases :: [(Text, Backend)]
aliases =
  [ ("claude", ClaudeCode), ("claude-code", ClaudeCode)
  , ("codex", Codex), ("codex-cli", Codex), ("openai-codex", Codex)
  , ("native", Fisher), ("fisher", Fisher) ]

-- | Lower-case, @_@ and @/@ to @-@, then the aliases.
normalizeBackendName :: Text -> Either Text Backend
normalizeBackendName raw =
  let n = T.replace "/" "-" (T.replace "_" "-" (T.toLower (T.strip raw)))
   in case lookup n aliases of
        Just b -> Right b
        Nothing ->
          Left ("`fisher.autoformalize.backend` must be one of: "
                  <> T.intercalate ", " (map backendName supportedBackends) <> ".")

-- | @fisher.autoformalize@ when it is a mapping, else empty.
autoformalizeConfig :: Value -> Value
autoformalizeConfig cfg = case lookupKey "fisher" cfg >>= lookupKey "autoformalize" of
  Just o@(Object _) -> o
  _ -> Object KM.empty

-- | Fisher only: the model a managed workflow's child runs, from the
-- command's @--model@, else @fisher.autoformalize.model@; Nothing leaves
-- each backend its own default. Not @model.default@: that is the native
-- agent's own model, always set, and named as OpenRouter names it.
resolveLaunchModel :: Maybe Text -> Value -> Maybe Text
resolveLaunchModel flag cfg =
  let given = T.strip (fromMaybe "" flag)
      configured = T.strip (pyStrOr (lookupKey "model" (autoformalizeConfig cfg)))
   in if not (T.null given) then Just given else if not (T.null configured) then Just configured else Nothing

envText :: [(String, String)] -> String -> Text
envText env k = T.toLower (T.strip (T.pack (fromMaybe "" (lookup k env))))

-- | @FISHER_AUTOFORMALIZE_BACKEND@, else the config's @backend@, else
-- claude-code.
resolveBackend :: Value -> [(String, String)] -> Either Text Backend
resolveBackend cfg env =
  let override = envText env "FISHER_AUTOFORMALIZE_BACKEND"
      configured = T.toLower (pyStrOr (lookupKey "backend" (autoformalizeConfig cfg)))
      value
        | not (T.null override) = override
        | T.null configured = "claude-code"
        | otherwise = configured
   in normalizeBackendName value

data AuthMode = AuthAuto | AuthLogin | AuthApiKey
  deriving (Eq, Show)

authModeName :: AuthMode -> Text
authModeName = \case
  AuthAuto -> "auto"
  AuthLogin -> "login"
  AuthApiKey -> "api-key"

resolveAuthMode :: Value -> [(String, String)] -> Either Text AuthMode
resolveAuthMode cfg env =
  let override = envText env "FISHER_AUTOFORMALIZE_AUTH_MODE"
      configured = T.toLower (pyStrOr (lookupKey "auth_mode" (autoformalizeConfig cfg)))
      value = T.replace "_" "-" $
        if not (T.null override) then override else if T.null configured then "auto" else configured
   in case value of
        "auto" -> Right AuthAuto
        "login" -> Right AuthLogin
        "api-key" -> Right AuthApiKey
        _ -> Left "`fisher.autoformalize.auth_mode` must be one of: auto, login, api-key."

-- | @FISHER_AUTOFORMALIZE_LEAN_LSP_MCP_SPEC@, else @lean-lsp-mcp@.
leanLspMcpSpec :: [(String, String)] -> Text
leanLspMcpSpec env =
  let v = T.strip (T.pack (fromMaybe "" (lookup "FISHER_AUTOFORMALIZE_LEAN_LSP_MCP_SPEC" env)))
   in if T.null v then "lean-lsp-mcp" else v

-- | The command that runs the Lean MCP server: Fisher's own,
-- @fisher lean-mcp@ (this executable), unless @FISHER_LEAN_MCP=uvx@ asks
-- for upstream's: @uvx --from SPEC lean-lsp-mcp@, or @uv x ...@ when only
-- uv is installed.
resolveUvRunner :: [(String, String)] -> IO (Either Text [Text])
resolveUvRunner env = do
  let path = fromMaybe "" (lookup "PATH" env)
      spec = leanLspMcpSpec env
      own = T.toLower (T.strip (T.pack (fromMaybe "" (lookup "FISHER_LEAN_MCP" env)))) /= "uvx"
  uvx <- if own then pure Nothing else whichIn path "uvx"
  self <- if own then Just <$> getExecutablePath else pure Nothing
  case self of
   Just exe -> pure (Right [T.pack exe, "lean-mcp"])
   Nothing -> case uvx of
    Just p -> pure (Right [T.pack p, "--from", spec, "lean-lsp-mcp"])
    Nothing -> do
      uv <- whichIn path "uv"
      pure $ case uv of
        Just p -> Right [T.pack p, "x", "--from", spec, "lean-lsp-mcp"]
        Nothing -> Left "Neither `uvx` nor `uv` is available. Install uv so Fisher can run the managed Lean MCP server."
