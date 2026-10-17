-- | @fisher llm-check@ (Fisher only): one small request to the model the
-- configuration and credentials resolve to, through the same path the
-- agent will use, and what came back.
module Fisher.Cli.LlmCheck
  ( llmCheckMain
  ) where

import Control.Exception (SomeException, try)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.LLM.Auth.Config (loadConfigPy)
import Fisher.LLM.Auth.Env (loadDotenvIntoSys)
import Fisher.LLM.Auth.Error (formatRuntimeProviderError)
import Fisher.LLM.Auth.Resolve (RuntimeRequest (..), defaultRuntimeRequest, resolveRuntimeProvider)
import Fisher.LLM.Client
import Fisher.LLM.Errors (errorClass, errorText)
import Fisher.LLM.Http (systemRuntime, tlsTransport)
import Fisher.LLM.Normalize (Outcome (..), Usage (..))
import Fisher.LLM.Prepare
import Fisher.LLM.Sys
import Fisher.LLM.Types
import Fisher.Py.Value

data Opts = Opts
  { oModel :: Maybe Text
  , oProvider :: Maybe Text
  , oPrompt :: Text
  , oMaxTokens :: Integer
  , oRaw :: Bool
  }

parseOpts :: [Text] -> Either Text Opts
parseOpts = go (Opts Nothing Nothing "Reply with the single word: pong" 256 False)
  where
    go o = \case
      [] -> Right o
      ("--model" : m : rest) -> go o {oModel = Just m} rest
      ("--provider" : p : rest) -> go o {oProvider = Just p} rest
      ("--prompt" : p : rest) -> go o {oPrompt = p} rest
      ("--max-tokens" : n : rest) | [(v, "")] <- reads (T.unpack n) -> go o {oMaxTokens = v} rest
      ("--raw" : rest) -> go o {oRaw = True} rest
      (x : _) -> Left ("unrecognized argument: " <> x)

llmCheckMain :: Sys -> [Text] -> IO Int
llmCheckMain sys args = case parseOpts args of
  Left e -> outLn sys ("usage: fisher llm-check [--provider P] [--model M] [--prompt TEXT] [--max-tokens N] [--raw]\nfisher llm-check: error: " <> e) >> pure 2
  Right o -> do
    loadDotenvIntoSys sys
    rt <- try (resolveRuntimeProvider sys defaultRuntimeRequest {rrRequested = oProvider o})
    case rt of
      Left (e :: SomeException) -> outLn sys ("Could not resolve a provider: " <> formatRuntimeProviderError e) >> pure 1
      Right r -> do
        cfg <- loadConfigPy sys
        let field k = maybe "" pyStrOf (get k r)
            cfgModel = case get "model" cfg of
              Just (PStr m) -> m
              Just d@(PDict _) -> maybe "" pyStrOf (get "default" d)
              _ -> ""
            model = fromMaybe (if T.null cfgModel then "anthropic/claude-opus-4.6" else cfgModel) (oModel o)
            mode = fromMaybe ChatCompletions (parseApiMode (field "api_mode"))
            s =
              defaultSettings
                { sMode = mode, sBaseUrl = field "base_url", sApiKey = field "api_key", sModel = model
                , sProvider = field "provider", sMaxTokens = Just (oMaxTokens o), sSessionId = "fisher-llm-check" }
            msgs = prepareMessages (PrepareInput (sBaseUrl s) Nothing Nothing [] (usesPromptCaching (sBaseUrl s) model (mode == AnthropicMessages))) [obj [("role", str "user"), ("content", str (oPrompt o))]]
        outLn sys ("Provider:  " <> field "provider" <> " (" <> apiModeName mode <> ", key from " <> field "source" <> ")")
        outLn sys ("Base URL:  " <> sBaseUrl s)
        outLn sys ("Model:     " <> model)
        tr <- tlsTransport
        let env = Env (sysUuidHex sys) (\_ _ -> pure "[image omitted]")
        res <- callModel systemRuntime tr env s msgs
        case res of
          Left e -> outLn sys ("Error:     " <> errorClass e <> ": " <> errorText e) >> pure 1
          Right (CallInvalid v) -> do
            outLn sys "Error:     the response had no choices / content / output"
            outLn sys (dumpsUnicode v)
            pure 1
          Right (CallOk outc raw) -> do
            let msg = oMessage outc
            outLn sys ("Reply:     " <> maybe "" pyStrOf (get "content" msg))
            case get "reasoning" msg of
              Just (PStr rz) | not (T.null rz) -> outLn sys ("Reasoning: " <> T.take 200 rz)
              _ -> pure ()
            outLn sys ("Finish:    " <> pyStrOf (oFinish outc))
            case oUsage outc of
              Just u -> outLn sys ("Usage:     " <> tshow (uPrompt u) <> " in, " <> tshow (uCompletion u) <> " out")
              Nothing -> pure ()
            if oRaw o then outLn sys (dumpsIndent False 2 raw) else pure ()
            pure 0
  where
    tshow = T.pack . show
