-- | The config writers login, model and setup share, and the merged config
-- read through 'Sys': the raw @yaml.safe_dump(cfg, sort_keys=False)@
-- rewrite of @config.yaml@ (@_update_config_for_provider@,
-- @_reset_config_provider@), @_save_model_choice@ (the full @save_config@)
-- and the numbered @_prompt_model_selection@ picker. Ported from
-- @gauss_cli/auth.py@.
module Fisher.LLM.Auth.Config
  ( loadConfigSys
  , loadConfigPy
  , modelConfig
  , readRawConfig
  , writeRawConfig
  , dumpRawYaml
  , updateConfigForProvider
  , resetConfigProvider
  , saveModelChoice
  , promptModelSelection
  ) where

import Control.Exception (IOException, try)
import Control.Monad (forM_, void)
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)

import Fisher.Config (loadConfigAt)
import Fisher.LLM.Auth.PyYaml (dumpPyYaml, loadConfigOrdered, parsePyYaml, saveConfigOrdered)
import Fisher.LLM.Auth.Store
import Fisher.LLM.Auth.Util (pyIntText, rstripSlash, tryInterrupt)
import Fisher.LLM.Sys
import Fisher.Paths (configPathIn)
import Fisher.Py.Value

-- | @load_config()@: the home made sure of, the user file merged over the
-- defaults; a warning printed when the file cannot be read.
loadConfigSys :: Sys -> IO A.Value
loadConfigSys sys = do
  (cfg, warnings) <- loadConfigAt (sysFisherHome sys)
  forM_ warnings (outLn sys)
  pure cfg

-- | 'loadConfigSys' as a 'PyVal' (key order is not meaningful here).
loadConfigPy :: Sys -> IO PyVal
loadConfigPy sys = fromAeson <$> loadConfigSys sys

-- | @_get_model_config@: the @model@ section as a dict (a string model
-- becomes @{"default": ...}@).
modelConfig :: PyVal -> PyVal
modelConfig cfg = case get "model" cfg of
  Just d@(PDict _) -> d
  Just (PStr s) | not (T.null (T.strip s)) -> obj [("default", str (T.strip s))]
  _ -> PDict []

-- | @yaml.safe_load(config_path.read_text()) or {}@, keys in file order
-- and ints, floats and bools as PyYAML reads them: Nothing when the file is
-- missing or does not parse.
readRawConfig :: FilePath -> IO (Maybe PyVal)
readRawConfig path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      r <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
      case r of
        Left _ -> pure Nothing
        Right bytes ->
          parsePyYaml bytes >>= \case
            Left _ -> pure Nothing
            Right PNull -> pure (Just (PDict []))
            Right v -> pure (Just v)

-- | @yaml.safe_dump(cfg, sort_keys=False)@: block style, keys in the
-- dict's order, floats as Python writes them.
dumpRawYaml :: PyVal -> BS.ByteString
dumpRawYaml = dumpPyYaml

-- | @config_path.write_text(...)@: a plain write, the mode left as it was
-- (the default for a new file).
writeRawConfig :: FilePath -> PyVal -> IO ()
writeRawConfig path cfg = do
  createDirectoryIfMissing True (takeDirectory path)
  BS.writeFile path (dumpRawYaml cfg)

-- | @_update_config_for_provider(provider_id, inference_base_url,
-- default_model)@: the provider made active in @auth.json@, then
-- @model.provider@ and @model.base_url@ (or its removal) written into
-- @config.yaml@ with the raw dump. Returns the config path.
updateConfigForProvider :: Sys -> Text -> Text -> Maybe Text -> IO FilePath
updateConfigForProvider sys pid baseUrl defaultModel = do
  withAuthLock sys $ do
    store <- loadAuthStore sys
    void (saveAuthStore sys (set "active_provider" (str pid) store))
  let path = configPathIn (sysFisherHome sys)
  createDirectoryIfMissing True (takeDirectory path)
  loaded <- readRawConfig path
  let cfg = case loaded of
        Just d@(PDict _) -> d
        _ -> PDict []
      model0 = case get "model" cfg of
        Just d@(PDict _) -> d
        Just (PStr s) | not (T.null (T.strip s)) -> obj [("default", str (T.strip s))]
        _ -> PDict []
      model1 = set "provider" (str pid) model0
      model2 =
        if not (T.null (T.strip baseUrl))
          then set "base_url" (str (rstripSlash baseUrl)) model1
          else delete "base_url" model1
      model3 = case defaultModel of
        Just m | not (T.null m) ->
          let cur = case get "default" model2 of
                Just (PStr s) -> s
                Just v | truthy v -> pyStrOf v
                _ -> ""
           in if T.null cur || "/" `T.isInfixOf` cur then set "default" (str m) model2 else model2
        _ -> model2
  writeRawConfig path (set "model" model3 cfg)
  pure path

-- | @_reset_config_provider@ (after logout): @model.provider@ back to
-- @auto@ and an existing @model.base_url@ pointed at OpenRouter.
resetConfigProvider :: Sys -> IO FilePath
resetConfigProvider sys = do
  let path = configPathIn (sysFisherHome sys)
  loaded <- readRawConfig path
  case loaded of
    Just cfg@(PDict _) -> do
      let cfg' = case get "model" cfg of
            Just m@(PDict _) ->
              let m1 = set "provider" (str "auto") m
                  m2 = if has "base_url" m1 then set "base_url" (str "https://openrouter.ai/api/v1") m1 else m1
               in set "model" m2 cfg
            _ -> cfg
      writeRawConfig path cfg'
    _ -> pure ()
  pure path

-- | @_save_model_choice(model_id)@: the merged config with
-- @model.default@ set, saved whole by @save_config@.
saveModelChoice :: Sys -> Text -> IO ()
saveModelChoice sys m = do
  cfg <- loadConfigOrdered sys
  let model = case get "model" cfg of
        Just d@(PDict _) -> set "default" (str m) d
        _ -> obj [("default", str m)]
  saveConfigOrdered sys (set "model" model cfg)

-- | @_prompt_model_selection(model_ids, current_model)@, the numbered
-- fallback (simple_term_menu is not installed upstream): the current model
-- first and marked, then custom and skip. Nothing for skip, empty input,
-- EOF or Ctrl-C.
promptModelSelection :: Sys -> [Text] -> Text -> IO (Maybe Text)
promptModelSelection sys ids current = do
  let ordered = dedupe ([current | not (T.null current), current `elem` ids] <> ids)
      label mid = if mid == current then mid <> "  ← currently in use" else mid
      n = length ordered
  outLn sys "Select default model:"
  forM_ (zip [1 :: Int ..] ordered) $ \(i, mid) -> outLn sys ("  " <> tshow i <> ". " <> label mid)
  outLn sys ("  " <> tshow (n + 1) <> ". Enter custom model name")
  outLn sys ("  " <> tshow (n + 2) <> ". Skip (keep current)")
  outLn sys ""
  let loop = do
        r <- interruptible (sysInput sys ("Choice [1-" <> tshow (n + 2) <> "] (default: skip): "))
        case r of
          Nothing -> pure Nothing
          Just raw -> do
            let choice = T.strip raw
            if T.null choice
              then pure Nothing
              else case pyIntText choice of
                Nothing -> outLn sys "Please enter a number" >> loop
                Just i
                  | i >= 1 && i <= toInteger n -> pure (Just (ordered !! (fromInteger i - 1)))
                  | i == toInteger n + 1 -> do
                      c <- interruptible (sysInput sys "Enter model name: ")
                      case c of
                        Nothing -> pure Nothing
                        Just x -> let x' = T.strip x in pure (if T.null x' then Nothing else Just x')
                  | i == toInteger n + 2 -> pure Nothing
                  | otherwise -> outLn sys ("Please enter 1-" <> tshow (n + 2)) >> loop
  loop
  where
    tshow = T.pack . show
    dedupe = foldl (\acc x -> if x `elem` acc then acc else acc <> [x]) []
    interruptible act = do
      r <- tryInterrupt act
      pure (either (const Nothing) id r)
