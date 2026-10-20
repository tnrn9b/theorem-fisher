-- | @fisher model@: pick a provider (the active one first), then log in or
-- enter a key as that provider needs and pick a default model. Ported from
-- upstream @cli/main.py@ (@cmd_model@ and every @_model_flow_*@, the saved
-- custom providers and their removal, the Anthropic OAuth flow). The
-- pickers are upstream's numbered fallbacks (@simple_term_menu@ is not
-- installed upstream).
module Fisher.Cli.Model
  ( modelMain
  ) where

import Control.Exception (AsyncException, SomeException, fromException, throwIO, try)
import Control.Monad (unless, void, when)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Cli.AuthArgs
import Fisher.Cli.Prompt (In (..), pyIntOf, showT)
import qualified Fisher.Cli.Prompt as P
import Fisher.Cli.Setup (runTop)
import Fisher.Cli.SetupCommon
import Fisher.LLM.Auth.Anthropic (ClaudeCreds (..), isClaudeCodeTokenValid, readClaudeCodeCredentials, runOauthSetupToken)
import Fisher.LLM.Auth.Codex (defaultCodexOpts, getCodexAuthStatus, getCodexModelIds, loginOpenAICodex, resolveCodexRuntimeCredentials)
import Fisher.LLM.Auth.Config (promptModelSelection, saveModelChoice, updateConfigForProvider)
import Fisher.LLM.Auth.Env (loadDotenvIntoSys, saveAnthropicApiKey, saveAnthropicOauthToken, saveEnvValue, useAnthropicClaudeCodeCredentials)
import Fisher.LLM.Auth.Error
import Fisher.LLM.Auth.Registry
import Fisher.LLM.Auth.Resolve (detectProvider, resolveProvider)
import Fisher.LLM.Auth.Store (deactivateProvider)
import Fisher.LLM.Sys
import Fisher.Py.Value

-- | @fisher model@.
modelMain :: Sys -> [Text] -> IO Int
modelMain sys args = do
  loadDotenvIntoSys sys
  case parseArgs modelParser args of
    Left (msg, code) -> sysOut sys msg >> pure code
    Right _ -> do
      ui <- P.newUI sys
      runTop sys (cmdModel ui sys >> pure 0)

say :: Sys -> Text -> IO ()
say = outLn

-- | @input(prompt)@ as upstream's flows read it.
ask :: P.UI -> Text -> IO In
ask = P.readLine

-- | Python's @except Exception@ (SystemExit and Ctrl-C pass through).
data Caught = NoError | ExitRaised Int | Failed SomeException

catchPy :: IO () -> IO Caught
catchPy act =
  try act >>= \case
    Right () -> pure NoError
    Left e
      | Just (SystemExit n) <- fromException e -> pure (ExitRaised n)
      | Just (_ :: AsyncException) <- fromException e -> throwIO e
      | otherwise -> pure (Failed e)

-- | @load_config()@, its @model@ made a dict, changed, then @save_config@.
updateModel :: Sys -> (PyVal -> PyVal) -> IO ()
updateModel sys f = do
  cfg <- loadCfg sys
  let model = case get "model" cfg of
        Just d@(PDict _) -> d
        Just v | truthy v -> obj [("default", v)]
        _ -> PDict []
  saveCfg sys (set "model" (f model) cfg)

clearCustomEndpoint :: Sys -> IO ()
clearCustomEndpoint sys = do
  base <- envValue sys "OPENAI_BASE_URL"
  unless (T.null base) $ do
    saveEnvValue sys "OPENAI_BASE_URL" ""
    saveEnvValue sys "OPENAI_API_KEY" ""

-- | @cmd_model(args)@.
cmdModel :: P.UI -> Sys -> IO ()
cmdModel ui sys = do
  config <- loadCfg sys
  let currentModel0 = case get "model" config of
        Just (PDict kvs) -> fromMaybe (str "") (lookup "default" kvs)
        Just v -> v
        Nothing -> PNull
      currentModel = if truthy currentModel0 then pyStrOf currentModel0 else "(not set)"
      configProvider = case get "model" config of
        Just m@(PDict _) -> case get "provider" m of
          Just v | truthy v -> Just (pyStrOf v)
          _ -> Nothing
        _ -> Nothing
  envProvider <- sysGetEnv sys "FISHER_INFERENCE_PROVIDER"
  let effective = fromMaybe "auto" (orNonEmpty [configProvider, envProvider])
  active0 <- try (resolveProvider sys (Just effective) Nothing Nothing) >>= \case
    Right p -> pure p
    Left (e :: AuthError) -> do
      say sys ("Warning: " <> formatAuthError e <> " Falling back to auto provider detection.")
      -- The detection itself, so that an auth.json whose active provider
      -- was removed (reported just above) does not stop the menu that
      -- replaces it.
      detectProvider sys
  customBase <- envValue sys "OPENAI_BASE_URL"
  let active = if active0 == "openrouter" && not (T.null customBase) then "custom" else active0
      labels =
        [ ("openrouter", "OpenRouter"), ("openai-codex", "OpenAI Codex"), ("anthropic", "Anthropic")
        , ("zai", "Z.AI / GLM"), ("kimi-coding", "Kimi / Moonshot"), ("minimax", "MiniMax"), ("minimax-cn", "MiniMax (China)")
        , ("custom", "Custom endpoint") ]
  say sys ""
  say sys ("  Current model:    " <> currentModel)
  say sys ("  Active provider:  " <> fromMaybe active (lookup active labels))
  say sys ""
  let builtIn =
        [ ("openrouter", "OpenRouter (100+ models, pay-per-use)")
        , ("openai-codex", "OpenAI Codex"), ("anthropic", "Anthropic (Claude models — API key or Claude Code)")
        , ("zai", "Z.AI / GLM (Zhipu AI direct API)"), ("kimi-coding", "Kimi / Moonshot (Moonshot AI direct API)")
        , ("minimax", "MiniMax (global direct API)"), ("minimax-cn", "MiniMax China (domestic direct API)") ]
      customs =
        [ (key, label, info)
        | PDict entry <- case get "custom_providers" config of Just (PList xs) -> xs; _ -> []
        , let e = PDict entry
              name = T.strip (getS "name" e)
              base = T.strip (getS "base_url" e)
        , not (T.null name), not (T.null base)
        , let key = "custom:" <> T.replace " " "-" (T.toLower name)
              short = T.dropWhileEnd (== '/') (T.replace "http://" "" (T.replace "https://" "" base))
              saved = getS "model" e
              label = name <> " (" <> short <> ")" <> (if T.null saved then "" else " — " <> saved)
              info = (name, base, getS "api_key" e, saved) ]
      customMap = [(k, i) | (k, _, i) <- customs]
      providers =
        builtIn <> [(k, l) | (k, l, _) <- dedupeKeys customs] <> [("custom", "Custom endpoint (enter URL manually)")]
          <> [("remove-custom", "Remove a saved custom provider") | not (null customMap)]
      activeKey = if active `elem` map fst providers then active else "custom"
      ordered =
        foldl (\acc (k, l) -> if k == activeKey then (k, l <> "  ← currently active") : acc else acc <> [(k, l)]) [] providers
          <> [("cancel", "Cancel")]
  choice <- promptProviderChoice ui sys (map snd ordered)
  case choice of
    Nothing -> say sys "No change."
    Just i -> case fst (ordered !! i) of
      "cancel" -> say sys "No change."
      "openrouter" -> flowOpenRouter ui sys currentModel
      "openai-codex" -> flowCodex sys currentModel
      "custom" -> flowCustom ui sys
      "remove-custom" -> removeCustomProvider ui sys
      "anthropic" -> flowAnthropic ui sys currentModel
      "kimi-coding" -> flowKimi ui sys currentModel
      p | p `elem` ["zai", "minimax", "minimax-cn"] -> flowApiKeyProvider ui sys p currentModel
        | Just inf <- lookupLast p customMap -> flowNamedCustom ui sys inf
        | otherwise -> pure ()
  where
    orNonEmpty xs = case [x | Just x <- xs, not (T.null x)] of
      (x : _) -> Just x
      [] -> Nothing
    -- The dict keeps the last entry for a key; the menu lists every one.
    dedupeKeys = id
    lookupLast k xs = case [v | (k', v) <- xs, k' == k] of
      [] -> Nothing
      vs -> Just (last vs)

-- | @_prompt_provider_choice(choices)@, the numbered fallback.
promptProviderChoice :: P.UI -> Sys -> [Text] -> IO (Maybe Int)
promptProviderChoice ui sys choices = do
  say sys "Select provider:"
  mapM_ (\(i, c) -> say sys ("  " <> showT i <> ". " <> c)) (zip [1 :: Int ..] choices)
  say sys ""
  let n = length choices
      loop =
        ask ui ("Choice [1-" <> showT n <> "]: ") >>= \case
          Typed v
            | T.null (T.strip v) -> pure Nothing
            | otherwise -> case pyIntOf (T.strip v) of
                Nothing -> say sys "Please enter a number" >> loop
                Just k
                  | k >= 1 && k <= toInteger n -> pure (Just (fromInteger k - 1))
                  | otherwise -> say sys ("Please enter 1-" <> showT n) >> loop
          _ -> say sys "" >> pure Nothing
  loop

-- | @_model_flow_openrouter(config, current_model)@.
flowOpenRouter :: P.UI -> Sys -> Text -> IO ()
flowOpenRouter ui sys currentModel = do
  key <- envValue sys "OPENROUTER_API_KEY"
  proceed <-
    if not (T.null key)
      then pure True
      else do
        say sys "No OpenRouter API key configured."
        say sys "Get one at: https://openrouter.ai/keys"
        say sys ""
        ask ui "OpenRouter API key (or Enter to cancel): " >>= \case
          Typed v
            | T.null (T.strip v) -> say sys "Cancelled." >> pure False
            | otherwise -> do
                saveEnvValue sys "OPENROUTER_API_KEY" (T.strip v)
                say sys "API key saved."
                say sys ""
                pure True
          _ -> say sys "" >> pure False
  when proceed $
    promptModelSelection sys modelIds currentModel >>= \case
      Just sel -> do
        clearCustomEndpoint sys
        saveModelChoice sys sel
        updateModel sys (set "base_url" (str openrouterBaseUrl) . set "provider" (str "openrouter"))
        deactivateProvider sys
        say sys ("Default model set to: " <> sel <> " (via OpenRouter)")
      Nothing -> say sys "No change."

-- | @_model_flow_openai_codex(config, current_model)@.
flowCodex :: Sys -> Text -> IO ()
flowCodex sys currentModel = do
  status <- getCodexAuthStatus sys
  proceed <-
    if get "logged_in" status == Just (PBool True)
      then pure True
      else do
        say sys "Not logged into OpenAI Codex. Starting login..."
        say sys ""
        catchPy (loginOpenAICodex sys) >>= \case
          NoError -> pure True
          ExitRaised _ -> say sys "Login cancelled or failed." >> pure False
          Failed e -> say sys ("Login failed: " <> errorStr e) >> pure False
  when proceed $ do
    tok <- try (resolveCodexRuntimeCredentials sys defaultCodexOpts) >>= \case
      Right c -> pure (case get "api_key" c of Just (PStr k) -> Just k; _ -> Nothing)
      Left (_ :: AuthError) -> pure Nothing
    models <- getCodexModelIds sys tok
    promptModelSelection sys models currentModel >>= \case
      Just sel -> do
        saveModelChoice sys sel
        void (updateConfigForProvider sys "openai-codex" defaultCodexBaseUrl Nothing)
        clearCustomEndpoint sys
        say sys ("Default model set to: " <> sel <> " (via OpenAI Codex)")
      Nothing -> say sys "No change."

-- | @_model_flow_custom(config)@.
flowCustom :: P.UI -> Sys -> IO ()
flowCustom ui sys = do
  currentUrl <- envValue sys "OPENAI_BASE_URL"
  currentKey <- envValue sys "OPENAI_API_KEY"
  say sys "Custom OpenAI-compatible endpoint configuration:"
  unless (T.null currentUrl) $ say sys ("  Current URL: " <> currentUrl)
  unless (T.null currentKey) $ say sys ("  Current key: " <> T.take 8 currentKey <> "...")
  say sys ""
  let cancelled = say sys "\nCancelled."
  ask ui ("API base URL [" <> (if T.null currentUrl then "e.g. https://api.example.com/v1" else currentUrl) <> "]: ") >>= \case
    Typed u -> ask ui ("API key [" <> (if T.null currentKey then "optional" else T.take 8 currentKey <> "...") <> "]: ") >>= \case
      Typed k -> ask ui "Model name (e.g. gpt-4, llama-3-70b): " >>= \case
        Typed m -> go (T.strip u) (T.strip k) (T.strip m) currentUrl currentKey
        _ -> cancelled
      _ -> cancelled
    _ -> cancelled
  where
    go baseUrl0 apiKey modelName currentUrl currentKey
      | T.null baseUrl0 && T.null currentUrl = say sys "No URL provided. Cancelled."
      | otherwise = do
          let url0 = if T.null baseUrl0 then currentUrl else baseUrl0
          if not ("http://" `T.isPrefixOf` url0 || "https://" `T.isPrefixOf` url0)
            then say sys ("Invalid URL: " <> url0 <> " (must start with http:// or https://)")
            else do
              let key = if T.null apiKey then currentKey else apiKey
              probe <- probeApiModels sys key url0 5.0
              (url, baseUrl) <-
                if prUsedFallback probe && not (T.null (prResolvedBaseUrl probe))
                  then do
                    say sys ("Warning: endpoint verification worked at " <> prResolvedBaseUrl probe <> "/models, not the exact URL you entered. Saving the working base URL instead.")
                    let u = prResolvedBaseUrl probe
                    pure (u, if T.null baseUrl0 then baseUrl0 else u)
                  else do
                    case prModels probe of
                      Just ms -> say sys ("Verified endpoint via " <> fromMaybe "None" (prProbedUrl probe) <> " (" <> showT (length ms) <> " model(s) visible)")
                      Nothing -> do
                        say sys ("Warning: could not verify this endpoint via " <> fromMaybe "None" (prProbedUrl probe) <> ". Fisher will still save it.")
                        mapM_ (\s -> say sys ("  If this server expects /v1, try base URL: " <> s)) (prSuggestedBaseUrl probe)
                    pure (url0, baseUrl0)
              unless (T.null baseUrl) $ saveEnvValue sys "OPENAI_BASE_URL" url
              unless (T.null apiKey) $ saveEnvValue sys "OPENAI_API_KEY" apiKey
              if not (T.null modelName)
                then do
                  saveModelChoice sys modelName
                  updateModel sys (set "base_url" (str url) . set "provider" (str "custom"))
                  deactivateProvider sys
                  say sys ("Default model set to: " <> modelName <> " (via " <> url <> ")")
                else do
                  when (not (T.null baseUrl) || not (T.null apiKey)) (deactivateProvider sys)
                  say sys ("Endpoint saved. Use `/model` in chat or `" <> cliName <> " model` to set a model.")
              saveCustomProvider sys url key modelName

-- | @_save_custom_provider(base_url, api_key, model)@: by URL, the model
-- updated when it is saved already, else a new entry named from the host.
saveCustomProvider :: Sys -> Text -> Text -> Text -> IO ()
saveCustomProvider sys baseUrl apiKey model = do
  cfg <- loadCfg sys
  let providers = case get "custom_providers" cfg of
        Just (PList xs) -> xs
        _ -> []
      strip = T.dropWhileEnd (== '/')
      matches e@(PDict _) = strip (getS "base_url" e) == strip baseUrl
      matches _ = False
  case break matches providers of
    (before, e : after) ->
      when (not (T.null model) && get "model" e /= Just (str model)) $
        saveCfg sys (set "custom_providers" (PList (before <> [set "model" (str model) e] <> after)) cfg)
    _ -> do
      let clean0 = strip (T.replace "http://" "" (T.replace "https://" "" baseUrl))
          clean = stripV1 clean0
          host = T.takeWhile (/= '/') clean
          name
            | "localhost" `T.isInfixOf` host || "127.0.0.1" `T.isInfixOf` host = "Local (" <> host <> ")"
            | "runpod" `T.isInfixOf` T.toLower host = "RunPod (" <> host <> ")"
            | otherwise = capitalize host
          entry =
            obj ([("name", str name), ("base_url", str baseUrl)] <> [("api_key", str apiKey) | not (T.null apiKey)] <> [("model", str model) | not (T.null model)])
      saveCfg sys (set "custom_providers" (PList (providers <> [entry])) cfg)
      say sys ("  💾 Saved to custom providers as \"" <> name <> "\" (edit in config.yaml)")
  where
    -- @re.sub(r"/v1/?$", "", clean)@
    stripV1 t
      | "/v1/" `T.isSuffixOf` t = T.dropEnd 4 t
      | "/v1" `T.isSuffixOf` t = T.dropEnd 3 t
      | otherwise = t

-- | @_remove_custom_provider(config)@.
removeCustomProvider :: P.UI -> Sys -> IO ()
removeCustomProvider ui sys = do
  cfg <- loadCfg sys
  let providers = case get "custom_providers" cfg of
        Just (PList xs) -> xs
        _ -> []
  if null providers
    then say sys "No custom providers configured."
    else do
      say sys "Remove a custom provider:\n"
      let label e = case e of
            PDict _ ->
              let name = maybe "unnamed" pyStrOf (get "name" e)
                  short = T.dropWhileEnd (== '/') (T.replace "http://" "" (T.replace "https://" "" (getS "base_url" e)))
               in name <> " (" <> short <> ")"
            _ -> pyStrOf e
          choices = map label providers <> ["Cancel"]
      mapM_ (\(i, c) -> say sys ("  " <> showT i <> ". " <> c)) (zip [1 :: Int ..] choices)
      say sys ""
      idx <- ask ui ("Choice [1-" <> showT (length choices) <> "]: ") >>= \case
        Typed v | not (T.null (T.strip v)) -> pure (fmap (subtract 1) (pyIntOf (T.strip v)))
        _ -> pure Nothing
      let n = toInteger (length providers)
      case idx of
        Just i | i < n && i >= negate n -> do
          let j = fromInteger (if i < 0 then n + i else i)
              removed = providers !! j
              rest = take j providers <> drop (j + 1) providers
          saveCfg sys (set "custom_providers" (PList rest) cfg)
          let removedName = case removed of
                PDict _ -> maybe "unnamed" pyStrOf (get "name" removed)
                _ -> pyStrOf removed
          say sys ("✅ Removed \"" <> removedName <> "\" from custom providers.")
        Just i | i < negate n -> throwIO (pyError "IndexError" "pop index out of range")
        _ -> say sys "No change."

-- | @_model_flow_named_custom(config, provider_info)@.
flowNamedCustom :: P.UI -> Sys -> (Text, Text, Text, Text) -> IO ()
flowNamedCustom ui sys (name, baseUrl, apiKey, saved) =
  if not (T.null saved)
    then do
      activate saved
      say sys ("✅ Switched to: " <> saved)
      say sys ("   Provider: " <> name <> " (" <> baseUrl <> ")")
    else do
      say sys ("  Provider: " <> name)
      say sys ("  URL:      " <> baseUrl)
      say sys ""
      say sys "No model saved for this provider. Fetching available models..."
      models <- fetchApiModels sys apiKey baseUrl 8.0
      chosen <- case models of
        Just ms | not (null ms) -> do
          say sys ("Found " <> showT (length ms) <> " model(s):\n")
          mapM_ (\(i, m) -> say sys ("  " <> showT i <> ". " <> m)) (zip [1 :: Int ..] ms)
          say sys ("  " <> showT (length ms + 1) <> ". Cancel")
          say sys ""
          ask ui ("Choice [1-" <> showT (length ms + 1) <> "]: ") >>= \case
            Typed v
              | T.null (T.strip v) -> say sys "Cancelled." >> pure Nothing
              | otherwise -> case pyIntOf (T.strip v) of
                  Nothing -> say sys "\nCancelled." >> pure Nothing
                  Just k
                    | k - 1 < 0 || k - 1 >= toInteger (length ms) -> say sys "Cancelled." >> pure Nothing
                    | otherwise -> pure (Just (ms !! (fromInteger k - 1)))
            _ -> say sys "\nCancelled." >> pure Nothing
        _ -> do
          say sys "Could not fetch models from endpoint. Enter model name manually."
          ask ui "Model name: " >>= \case
            Typed v
              | T.null (T.strip v) -> say sys "No model specified. Cancelled." >> pure Nothing
              | otherwise -> pure (Just (T.strip v))
            _ -> say sys "\nCancelled." >> pure Nothing
      case chosen of
        Nothing -> pure ()
        Just m -> do
          activate m
          saveCustomProvider sys baseUrl apiKey m
          say sys ("\n✅ Model set to: " <> m)
          say sys ("   Provider: " <> name <> " (" <> baseUrl <> ")")
  where
    activate m = do
      saveEnvValue sys "OPENAI_BASE_URL" baseUrl
      unless (T.null apiKey) $ saveEnvValue sys "OPENAI_API_KEY" apiKey
      saveModelChoice sys m
      updateModel sys (set "base_url" (str baseUrl) . set "provider" (str "custom"))
      deactivateProvider sys

-- | The key of an API-key provider: @get_env_value(ev) or os.getenv(ev)@
-- over its variables.
existingKeyOf :: Sys -> ProviderConfig -> IO Text
existingKeyOf sys pc = go (pcApiKeyEnvVars pc)
  where
    go [] = pure ""
    go (v : vs) = do
      a <- envValue sys v
      b <- if T.null a then fromMaybe "" <$> sysGetEnv sys v else pure a
      if T.null b then go vs else pure b

-- | Ask for the provider's first key variable when none is set. False
-- when the user cancels.
ensureKey :: P.UI -> Sys -> ProviderConfig -> Text -> IO (Maybe Text)
ensureKey ui sys pc existing
  | not (T.null existing) = do
      say sys ("  " <> pcName pc <> " API key: " <> T.take 8 existing <> "... ✓")
      say sys ""
      pure (Just existing)
  | otherwise = do
      say sys ("No " <> pcName pc <> " API key configured.")
      case pcApiKeyEnvVars pc of
        [] -> pure (Just "")
        (var : _) ->
          ask ui (var <> " (or Enter to cancel): ") >>= \case
            Typed v
              | T.null (T.strip v) -> say sys "Cancelled." >> pure Nothing
              | otherwise -> do
                  saveEnvValue sys var (T.strip v)
                  say sys "API key saved."
                  say sys ""
                  pure (Just (T.strip v))
            _ -> say sys "" >> pure Nothing

-- | @_model_flow_kimi(config, current_model)@.
flowKimi :: P.UI -> Sys -> Text -> IO ()
flowKimi ui sys currentModel = do
  let pc = fromMaybe (error "kimi-coding is in the registry") (lookupProvider "kimi-coding")
  existing <- existingKeyOf sys pc
  ensureKey ui sys pc existing >>= \case
    Nothing -> pure ()
    Just key -> do
      let coding = "sk-kimi-" `T.isPrefixOf` key
          base = if coding then kimiCodeBaseUrl else pcInferenceBaseUrl pc
      say sys (if coding then "  Detected Kimi Coding Plan key → " <> base else "  Using Moonshot endpoint → " <> base)
      override <- envValue sys (pcBaseUrlEnvVar pc)
      unless (T.null (pcBaseUrlEnvVar pc) || T.null override) $ saveEnvValue sys (pcBaseUrlEnvVar pc) ""
      say sys ""
      let models =
            if coding
              then ["kimi-for-coding", "kimi-k2.5", "kimi-k2-thinking", "kimi-k2-thinking-turbo"]
              else ["kimi-k2.5", "kimi-k2-thinking", "kimi-k2-turbo-preview", "kimi-k2-0905-preview"]
      promptModelSelection sys models currentModel >>= \case
        Just sel -> do
          clearCustomEndpoint sys
          saveModelChoice sys sel
          updateModel sys (set "base_url" (str base) . set "provider" (str "kimi-coding"))
          deactivateProvider sys
          say sys ("Default model set to: " <> sel <> " (via " <> (if coding then "Kimi Coding" else "Moonshot") <> ")")
        Nothing -> say sys "No change."

-- | @_model_flow_api_key_provider(config, provider_id, current_model)@.
flowApiKeyProvider :: P.UI -> Sys -> Text -> Text -> IO ()
flowApiKeyProvider ui sys pid currentModel = do
  let pc = fromMaybe (error "provider is in the registry") (lookupProvider pid)
      keyEnv = case pcApiKeyEnvVars pc of
        (v : _) -> v
        [] -> ""
      baseEnv = pcBaseUrlEnvVar pc
  existing <- existingKeyOf sys pc
  ensureKey ui sys pc existing >>= \case
    Nothing -> pure ()
    Just _ -> do
      currentBase <-
        if T.null baseEnv
          then pure ""
          else do
            a <- envValue sys baseEnv
            if T.null a then fromMaybe "" <$> sysGetEnv sys baseEnv else pure a
      let base0 = if T.null currentBase then pcInferenceBaseUrl pc else currentBase
      override <- ask ui ("Base URL [" <> base0 <> "]: ") >>= \case
        Typed v -> pure (T.strip v)
        _ -> say sys "" >> pure ""
      base <-
        if not (T.null override) && not (T.null baseEnv)
          then saveEnvValue sys baseEnv override >> pure override
          else pure base0
      probeKey <- if not (T.null existing) then pure existing else if T.null keyEnv then pure "" else envValue sys keyEnv
      live <- fetchApiModels sys probeKey base 5.0
      models <- case live of
        Just ms | not (null ms) -> say sys ("  Found " <> showT (length ms) <> " model(s) from " <> pcName pc <> " API") >> pure ms
        _ -> do
          let ms = providerModels pid
          unless (null ms) $ do
            say sys "  ⚠ Could not auto-detect models from API — showing defaults."
            say sys "    Use \"Enter custom model name\" if you don't see your model."
          pure ms
      selected <-
        if not (null models)
          then promptModelSelection sys models currentModel
          else ask ui "Model name: " >>= \case
            Typed v -> pure (let s = T.strip v in if T.null s then Nothing else Just s)
            _ -> pure Nothing
      case selected of
        Just sel -> do
          clearCustomEndpoint sys
          saveModelChoice sys sel
          updateModel sys (set "base_url" (str base) . set "provider" (str pid))
          deactivateProvider sys
          say sys ("Default model set to: " <> sel <> " (via " <> pcName pc <> ")")
        Nothing -> say sys "No change."

-- | @_run_anthropic_oauth_flow(save_env_value)@: True when credentials
-- were saved.
runAnthropicOauthFlow :: P.UI -> Sys -> IO Bool
runAnthropicOauthFlow ui sys = do
  r <- try $ do
    say sys ""
    say sys "  Running 'claude setup-token' — follow the prompts below."
    say sys "  A browser window will open for you to authorize access."
    say sys ""
    tok <- runOauthSetupToken sys
    case tok of
      Just t | not (T.null t) -> do
        linked <- activateClaudeCode
        if linked
          then pure True
          else do
            saveAnthropicOauthToken sys t
            say sys "  ✓ OAuth credentials saved."
            pure True
      _ -> do
        say sys ""
        say sys "  If the setup-token was displayed above, paste it here:"
        say sys ""
        ask ui "  Paste setup-token (or Enter to cancel): " >>= \case
          Typed v
            | not (T.null (T.strip v)) -> do
                saveAnthropicOauthToken sys (T.strip v)
                say sys "  ✓ Setup-token saved."
                pure True
            | otherwise -> say sys "  ⚠ Could not detect saved credentials." >> pure False
          _ -> say sys "" >> pure False
  case r of
    Right ok -> pure ok
    Left ex
      | peType ex == "FileNotFoundError" -> do
          say sys ""
          say sys "  The 'claude' CLI is required for OAuth login."
          say sys ""
          say sys "  To install and authenticate:"
          say sys ""
          say sys "    1. Install Claude Code:  npm install -g @anthropic-ai/claude-code"
          say sys "    2. Run:                  claude setup-token"
          say sys "    3. Follow the browser prompts to authorize"
          say sys ("    4. Re-run:               " <> cliName <> " model")
          say sys ""
          say sys "  Or paste an existing setup-token now (sk-ant-oat-...):"
          say sys ""
          ask ui "  Setup-token (or Enter to cancel): " >>= \case
            Typed v
              | not (T.null (T.strip v)) -> do
                  saveAnthropicOauthToken sys (T.strip v)
                  say sys "  ✓ Setup-token saved."
                  pure True
              | otherwise -> say sys "  Cancelled — install Claude Code and try again." >> pure False
            _ -> say sys "" >> pure False
      | otherwise -> throwIO ex
  where
    activateClaudeCode = do
      creds <- try (readClaudeCodeCredentials sys) >>= \case
        Right c -> pure c
        Left (_ :: SomeException) -> pure Nothing
      usable <- case creds of
        Just c -> (|| truthy (ccRefreshToken c)) <$> isClaudeCodeTokenValid sys c
        Nothing -> pure False
      if usable
        then do
          useAnthropicClaudeCodeCredentials sys
          say sys "  ✓ Claude Code credentials linked."
          say sys "    Fisher will use Claude's credential store directly instead of copying a setup-token into the active home directory."
          pure True
        else pure False

-- | @_model_flow_anthropic(config, current_model)@.
flowAnthropic :: P.UI -> Sys -> Text -> IO ()
flowAnthropic ui sys currentModel = do
  let firstOf [] = pure ""
      firstOf (a : as) = a >>= \v -> if T.null v then firstOf as else pure v
      osEnv k = fromMaybe "" <$> sysGetEnv sys k
  existing <- firstOf [envValue sys "ANTHROPIC_TOKEN", osEnv "ANTHROPIC_TOKEN", envValue sys "ANTHROPIC_API_KEY", osEnv "ANTHROPIC_API_KEY", osEnv "CLAUDE_CODE_OAUTH_TOKEN"]
  ccAvailable <- try (readClaudeCodeCredentials sys >>= maybe (pure False) (isClaudeCodeTokenValid sys)) >>= \case
    Right b -> pure b
    Left (_ :: SomeException) -> pure False
  let hasCreds = not (T.null existing) || ccAvailable
  step1 <-
    if not hasCreds
      then pure (Just True)
      else do
        if not (T.null existing)
          then say sys ("  Anthropic credentials: " <> T.take 12 existing <> "... ✓")
          else say sys "  Claude Code credentials: ✓ (auto-detected)"
        say sys ""
        say sys "    1. Use existing credentials"
        say sys "    2. Reauthenticate (new OAuth login)"
        say sys "    3. Cancel"
        say sys ""
        c <- ask ui "  Choice [1/2/3]: " >>= \case
          Typed v -> pure (T.strip v)
          _ -> pure "1"
        pure $ case c of
          "2" -> Just True
          "3" -> Nothing
          _ -> Just False
  case step1 of
    Nothing -> pure ()
    Just needsAuth -> do
      authed <-
        if not needsAuth
          then pure True
          else do
            say sys ""
            say sys "  Choose authentication method:"
            say sys ""
            say sys "    1. Claude Pro/Max subscription (OAuth login)"
            say sys "    2. Anthropic API key (pay-per-token)"
            say sys "    3. Cancel"
            say sys ""
            ask ui "  Choice [1/2/3]: " >>= \case
              Typed v -> case T.strip v of
                "1" -> runAnthropicOauthFlow ui sys
                "2" -> do
                  say sys ""
                  say sys "  Get an API key at: https://console.anthropic.com/settings/keys"
                  say sys ""
                  ask ui "  API key (sk-ant-...): " >>= \case
                    Typed k
                      | T.null (T.strip k) -> say sys "  Cancelled." >> pure False
                      | otherwise -> do
                          saveAnthropicApiKey sys (T.strip k)
                          say sys "  ✓ API key saved."
                          pure True
                    _ -> say sys "" >> pure False
                _ -> say sys "  No change." >> pure False
              _ -> say sys "" >> pure False
      when authed $ do
        say sys ""
        promptModelSelection sys (providerModels "anthropic") currentModel >>= \case
          Just sel -> do
            clearCustomEndpoint sys
            saveModelChoice sys sel
            updateModel sys (delete "base_url" . set "provider" (str "anthropic"))
            deactivateProvider sys
            say sys ("Default model set to: " <> sel <> " (via Anthropic)")
          Nothing -> say sys "No change."
