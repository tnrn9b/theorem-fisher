-- | @fisher config show@ and @fisher config edit@ (@gauss_cli/config.py@'s
-- @show_config@ and @edit_config@).
module Fisher.Cli.ConfigShow
  ( ShowEnv (..)
  , showConfigLines
  , EditEnv (..)
  , editConfig
  , pyStr
  , pyRepr
  ) where

import Control.Monad (filterM)
import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Fisher.Py.Json (pyFloatRepr, pyFormatFixed)
import Fisher.Term (Style, cyan, bold, dim, color)

data ShowEnv = ShowEnv
  { shStyle :: Style
  , shConfig :: Value
  , shConfigPath :: FilePath
  , shEnvPath :: FilePath
  , shInstall :: FilePath
  , shEnv :: Text -> IO (Maybe Text)  -- process environment, then .env
  }

at :: [Text] -> Value -> Maybe Value
at [] v = Just v
at (k : ks) (Object o) = KM.lookup (K.fromText k) o >>= at ks
at _ _ = Nothing

-- | @str()@ of a YAML value as Python would print it in an f-string.
pyStr :: Value -> Text
pyStr = \case
  String s -> s
  other -> pyRepr other

-- | @repr()@ of a YAML value.
pyRepr :: Value -> Text
pyRepr = \case
  String s -> "'" <> T.concatMap (\c -> if c == '\'' then "\\'" else if c == '\\' then "\\\\" else if c == '\n' then "\\n" else T.singleton c) s <> "'"
  Number n -> either pyFloatRepr (T.pack . show) (floatingOrInteger n :: Either Double Integer)
  Bool b -> if b then "True" else "False"
  Null -> "None"
  Array a -> "[" <> T.intercalate ", " (map pyRepr (V.toList a)) <> "]"
  Object o -> "{" <> T.intercalate ", " [pyRepr (String (K.toText k)) <> ": " <> pyRepr v | (k, v) <- KM.toList o] <> "}"

num :: Value -> Maybe Double
num = \case
  Number n -> Just (either id fromIntegral (floatingOrInteger n :: Either Double Integer))
  Bool b -> Just (if b then 1 else 0)
  _ -> Nothing

truthy :: Value -> Bool
truthy = \case
  Bool b -> b
  Null -> False
  String s -> not (T.null s)
  Number n -> n /= 0
  Array a -> not (V.null a)
  Object o -> not (KM.null o)

redact :: Style -> Maybe Text -> Text
redact st = \case
  Nothing -> color st [dim] "(not set)"
  Just k
    | T.null k -> color st [dim] "(not set)"
    | T.length k < 12 -> "***"
    | otherwise -> T.take 4 k <> "..." <> T.takeEnd 4 k

showConfigLines :: ShowEnv -> IO [Text]
showConfigLines e = do
  let st = shStyle e
      cfg = shConfig e
      c = color st [cyan]
      h t = color st [cyan, bold] t
      get path def = fromMaybe def (at path cfg)
      getDict path = case at path cfg of
        Just o@(Object _) -> o
        _ -> Object KM.empty
      pad n t = T.justifyLeft n ' ' t
  keys <- mapM (\(k, name) -> (\v -> "  " <> pad 14 name <> " " <> redact st v) <$> shEnv e k)
    [ ("OPENROUTER_API_KEY", "OpenRouter"), ("VOICE_TOOLS_OPENAI_KEY", "OpenAI (STT/TTS)"), ("FIRECRAWL_API_KEY", "Firecrawl")
    , ("BROWSERBASE_API_KEY", "Browserbase"), ("FAL_KEY", "FAL") ]
  tok <- shEnv e "ANTHROPIC_TOKEN"
  apiKey <- shEnv e "ANTHROPIC_API_KEY"
  let anthropic = case tok of
        Just t | not (T.null t) -> Just t
        _ -> apiKey
  let terminal = getDict ["terminal"]
      tget k def = fromMaybe def (at [k] terminal)
      backend = case at ["backend"] terminal of
        Just v -> pyStr v
        Nothing -> "local"
  backendLines <- case backend of
    "docker" -> pure ["  Docker image: " <> pyStr (tget "docker_image" (String "python:3.11-slim"))]
    "singularity" -> pure ["  Image:        " <> pyStr (tget "singularity_image" (String "docker://python:3.11"))]
    "modal" -> do
      tokn <- shEnv e "MODAL_TOKEN_ID"
      pure ["  Modal image:  " <> pyStr (tget "modal_image" (String "python:3.11")), "  Modal token:  " <> (if maybe False (not . T.null) tokn then "configured" else "(not set)")]
    "daytona" -> do
      k <- shEnv e "DAYTONA_API_KEY"
      pure ["  Daytona image: " <> pyStr (tget "daytona_image" (String "nikolaik/python-nodejs:python3.11-nodejs20")), "  API key:      " <> (if maybe False (not . T.null) k then "configured" else "(not set)")]
    "ssh" -> do
      hst <- shEnv e "TERMINAL_SSH_HOST"
      usr <- shEnv e "TERMINAL_SSH_USER"
      let orNot = maybe "(not set)" (\v -> if T.null v then "(not set)" else v)
      pure ["  SSH host:     " <> orNot hst, "  SSH user:     " <> orNot usr]
    _ -> pure []
  let handoff = case at ["cli", "handoff"] cfg of
        Just (Object o) -> ["  Handoff:      " <> pyStr (fromMaybe (String "auto") (KM.lookup "mode" o))]
        _ -> []
      compression = getDict ["compression"]
      cget k def = fromMaybe def (at [k] compression)
      enabled = truthy (cget "enabled" (Bool True))
      provider = pyStr (cget "summary_provider" (String "auto"))
      compressionLines =
        ["  Enabled:      " <> (if enabled then "yes" else "no")]
          <> ( if enabled
                 then
                   [ "  Threshold:    " <> pyFormatFixed 0 (fromMaybe 0.5 (num (cget "threshold" (Number 0.5))) * 100) <> "%"
                   , "  Model:        " <> pyStr (cget "summary_model" (String "google/gemini-3-flash-preview")) ]
                     <> ["  Provider:     " <> provider | provider /= "auto"]
                 else [] )
      aux label k =
        let t = getDict ["auxiliary", k]
            prov = pyStr (fromMaybe (String "auto") (at ["provider"] t))
            mdl = case at ["model"] t of
              Just v | truthy v -> Just (pyStr v)
              _ -> Nothing
         in (label, prov, mdl)
      auxes = [aux "Vision" "vision", aux "Web extract" "web_extract"]
      overrides = [(l, p, m) | (l, p, m) <- auxes, p /= "auto" || m /= Nothing]
      auxLines =
        if null overrides
          then []
          else
            ["", h "◆ Auxiliary Models (overrides)"]
              <> [ "  " <> pad 12 l <> "  " <> T.intercalate ", " (("provider=" <> p) : maybe [] (\x -> ["model=" <> x]) m)
                 | (l, p, m) <- overrides ]
      display = getDict ["display"]
      dget k def = fromMaybe def (at [k] display)
      tz = get ["timezone"] (String "")
  telegram <- shEnv e "TELEGRAM_BOT_TOKEN"
  discord <- shEnv e "DISCORD_BOT_TOKEN"
  let configured v = if maybe False (not . T.null) v then "configured" else color st [dim] "not configured"
      toolsets = case get ["toolsets"] (Array (V.fromList [String "all"])) of
        Array a -> T.intercalate ", " (map pyStr (V.toList a))
        String s -> T.intercalate ", " (map T.singleton (T.unpack s))
        other -> pyStr other
  pure $
    [ ""
    , c "┌─────────────────────────────────────────────────────────┐"
    , c "│              ⚕ Gauss Configuration                    │"
    , c "└─────────────────────────────────────────────────────────┘"
    , "", h "◆ Paths"
    , "  Config:       " <> T.pack (shConfigPath e)
    , "  Secrets:      " <> T.pack (shEnvPath e)
    , "  Install:      " <> T.pack (shInstall e)
    , "", h "◆ API Keys" ]
      <> keys
      <> ["  " <> pad 14 "Anthropic" <> " " <> redact st anthropic]
      <> [ "", h "◆ Model"
         , "  Model:        " <> pyStr (get ["model"] (String "not set"))
         , "  Max turns:    " <> pyStr (fromMaybe (Number 90) (at ["max_turns"] (getDict ["agent"])))
         , "  Toolsets:     " <> toolsets
         , "", h "◆ Display"
         , "  Personality:  " <> pyStr (dget "personality" (String "kawaii"))
         , "  Reasoning:    " <> (if truthy (dget "show_reasoning" (Bool False)) then "on" else "off")
         , "  Bell:         " <> (if truthy (dget "bell_on_complete" (Bool False)) then "on" else "off")
         , "", h "◆ Terminal"
         , "  Backend:      " <> backend
         , "  Working dir:  " <> pyStr (tget "cwd" (String "."))
         , "  Timeout:      " <> pyStr (tget "timeout" (Number 60)) <> "s" ]
      <> handoff
      <> backendLines
      <> ["", h "◆ Timezone", if truthy tz then "  Timezone:     " <> pyStr tz else "  Timezone:     " <> color st [dim] "(server-local)"]
      <> ["", h "◆ Context Compression"]
      <> compressionLines
      <> auxLines
      <> [ "", h "◆ Messaging Platforms"
         , "  Telegram:     " <> configured telegram
         , "  Discord:      " <> configured discord
         , "", color st [dim] (T.replicate 60 "─")
         , color st [dim] "  fisher config edit     # Edit config file"
         , color st [dim] "  fisher config set <key> <value>"
         , color st [dim] "  fisher setup           # Run setup wizard"
         , "" ]

data EditEnv = EditEnv
  { edConfigPath :: FilePath
  , edConfigExists :: IO Bool
  , edWriteDefault :: IO ()
  , edEnv :: Text -> IO (Maybe Text)
  , edWhich :: Text -> IO (Maybe FilePath)
  , edRun :: Text -> [Text] -> IO ()
  , edPut :: Text -> IO ()
  }

-- | @edit_config@: make the file if missing, then $EDITOR, $VISUAL or the
-- first of nano, vim, vi, code, notepad on the path.
editConfig :: EditEnv -> IO ()
editConfig e = do
  exists <- edConfigExists e
  if exists then pure () else edWriteDefault e >> edPut e ("Created " <> T.pack (edConfigPath e))
  ed <- edEnv e "EDITOR"
  vis <- edEnv e "VISUAL"
  editor <- case filter (not . T.null) [x | Just x <- [ed, vis]] of
    (x : _) -> pure (Just x)
    [] -> listToMaybe <$> filterM (fmap (/= Nothing) . edWhich e) ["nano", "vim", "vi", "code", "notepad"]
  case editor of
    Nothing -> edPut e "No editor found. Config file is at:" >> edPut e ("  " <> T.pack (edConfigPath e))
    Just x -> do
      edPut e ("Opening " <> T.pack (edConfigPath e) <> " in " <> x <> "...")
      edRun e x [T.pack (edConfigPath e)]
