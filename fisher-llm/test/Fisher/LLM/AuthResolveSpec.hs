-- | @resolve_runtime_provider@ against upstream's recorded runs
-- (@tools/corpus/b1_auth.py@, @resolve.json@): env keys and @.env@,
-- config provider and base URL, custom providers, Codex (JWT expiry,
-- refresh, migration), Claude Code credentials and explicit names. Each
-- case is replayed through a scripted 'Sys' and compared on the result (or
-- error), what was printed, the HTTP requests made and every file left
-- behind. Then the provider Fisher removed: every way of naming it gets
-- the removal error.
module Fisher.LLM.AuthResolveSpec (spec) where

import Control.Exception (SomeException, fromException, try)
import Control.Monad (forM_, unless)
import Data.IORef
import qualified Data.Text as T
import Test.Hspec

import Fisher.LLM.Auth.Env (loadDotenvIntoSys)
import Fisher.LLM.Auth.Error
import Fisher.LLM.Auth.Registry (removedProviderMessage)
import Fisher.LLM.Auth.Resolve
import Fisher.LLM.Replay
import Fisher.LLM.Sys (Sys)
import Fisher.Py.Value

-- | The fields of an HTTP log entry the specs compare, with the timeout as
-- a float (Python passes 15 and 15.0 alike; 'Sys' carries a Double).
httpView :: PyVal -> PyVal
httpView e =
  obj
    [ (k, norm k v)
    | k <- ["via", "method", "url", "headers", "body", "timeout", "status", "unmatched", "raised"]
    , Just v <- [get k e] ]
  where
    norm "timeout" (PInt n) = PFloat (fromInteger n)
    norm _ v = v

kwText :: T.Text -> PyVal -> Maybe T.Text
kwText k kw = case get k kw of
  Just (PStr s) -> Just s
  _ -> Nothing

errorView :: SomeException -> PyVal
errorView e = case fromException e of
  Just a ->
    obj
      [ ("type", str "AuthError"), ("module", str "fisher_cli.auth"), ("message", str (formatAuthError a))
      , ("str", str (aeMessage a)), ("code", maybe PNull str (aeCode a)), ("provider", str (aeProvider a))
      , ("relogin_required", PBool (aeRelogin a)) ]
  Nothing -> case fromException e of
    Just p -> obj [("type", str (peType p)), ("module", str (peModule p)), ("message", str (peStr p)), ("str", str (peStr p))]
    Nothing -> obj [("type", str "Haskell"), ("str", str (T.pack (show e)))]

spec :: Spec
spec = do
  (doc, cases) <- runIO (loadCases "test/golden/b1/resolve.json")
  let soul = maybe "" pyStrOf (get "soul_md" doc)
  describe "resolve_runtime_provider (resolve.json)" $
    forM_ cases $ \(Case c) -> it (T.unpack (maybe "?" pyStrOf (get "name" c))) $ do
      outcome <- newIORef PNull
      let kw = maybe (obj []) id (getPath ["call", "kwargs"] c)
          req = RuntimeRequest (kwText "requested" kw) (kwText "explicit_api_key" kw) (kwText "explicit_base_url" kw)
      (w, o) <- replay doc Nothing c $ \sys -> do
        loadDotenvIntoSys sys
        r <- try (resolveRuntimeProvider sys req)
        writeIORef outcome $ case r of
          Right v -> obj [("result", v)]
          Left e -> obj [("error", errorView e)]
        pure 0
      got <- readIORef outcome
      let want = case (get "result" c, get "error" c) of
            (Just v, _) -> obj [("result", v)]
            (_, Just e) -> obj [("error", e)]
            _ -> PNull
      maskVal w got `shouldBe` want
      oTranscript o `shouldBe` maybe "" pyStrOf (get "transcript" c)
      map httpView (oHttp o) `shouldBe` map httpView (httpLog c)
      let sleeps = case get "sleeps" c of
            Just (PList xs) -> [s | Just s <- map (get "seconds") xs]
            _ -> []
      unless (null sleeps) $ map PFloat (oSleeps o) `shouldBe` map (\case PInt n -> PFloat (fromInteger n); v -> v) sleeps
      files <- snapshotFiles w soul
      files `shouldBe` expectedFiles c

  describe "the removed Nous Portal provider" $ do
    let removed = obj [("error", obj [("message", str removedProviderMessage), ("code", str "provider_removed")])]
    it "is an error when config.yaml names it" $
      resolveIn [("config.yaml", "model:\n  provider: nous\n")] [] Nothing `shouldReturn` removed
    it "is an error when FISHER_INFERENCE_PROVIDER names it" $
      resolveIn [] [("FISHER_INFERENCE_PROVIDER", "Nous")] Nothing `shouldReturn` removed
    it "is an error when it is asked for by name" $
      resolveIn [] [] (Just "nous") `shouldReturn` removed
    it "is an error when auth.json has it as the active provider" $
      resolveIn [("auth.json", activeNous)] [("OPENROUTER_API_KEY", "sk-or-v1-abcdef0123456789wxyz")] Nothing `shouldReturn` removed
    it "is an error for auto detection when auth.json has it as the active provider" $
      providerIn [("auth.json", activeNous)] [("OPENROUTER_API_KEY", "sk-or-v1-abcdef0123456789wxyz")] `shouldReturn` removed
    it "is passed over by the detection itself (fisher model goes on to its menu)" $
      runIn [("auth.json", activeNous)] [("OPENROUTER_API_KEY", "sk-or-v1-abcdef0123456789wxyz")] (fmap str . detectProvider)
        `shouldReturn` obj [("result", str "openrouter")]
    it "is not in the way of auto detection when another provider is active" $
      providerIn [("auth.json", activeCodexFree)] [("OPENROUTER_API_KEY", "sk-or-v1-abcdef0123456789wxyz")]
        `shouldReturn` obj [("result", str "openrouter")]
    it "is not an error when another provider is asked for" $
      resolveIn [("auth.json", activeNous)] [("OPENROUTER_API_KEY", "sk-or-v1-abcdef0123456789wxyz")] (Just "openrouter")
        `shouldReturn` obj [("result", str "openrouter")]
  where
    activeNous = "{\n  \"version\": 1,\n  \"providers\": {\n    \"nous\": {\"access_token\": \"at-1\"}\n  },\n  \"active_provider\": \"nous\"\n}\n"
    activeCodexFree = "{\n  \"version\": 1,\n  \"providers\": {},\n  \"active_provider\": null\n}\n"

-- | 'resolveRuntimeProvider' in a fresh home holding these files and
-- environment: the provider it picks, or the error's message and code.
resolveIn :: [(T.Text, T.Text)] -> [(T.Text, T.Text)] -> Maybe T.Text -> IO PyVal
resolveIn files env requested =
  runIn files env $ \sys ->
    maybe PNull id . get "provider" <$> resolveRuntimeProvider sys defaultRuntimeRequest {rrRequested = requested}

-- | 'resolveProvider' on auto detection (nothing asked for, no explicit key
-- or URL), in a fresh home holding these files and environment.
providerIn :: [(T.Text, T.Text)] -> [(T.Text, T.Text)] -> IO PyVal
providerIn files env = runIn files env $ \sys -> str <$> resolveProvider sys Nothing Nothing Nothing

runIn :: [(T.Text, T.Text)] -> [(T.Text, T.Text)] -> (Sys -> IO PyVal) -> IO PyVal
runIn files env act = do
  outcome <- newIORef PNull
  let c =
        obj
          [ ("name", str "removed"), ("mode", str "pipe"), ("env", obj [(k, str v) | (k, v) <- env])
          , ("initial_files", obj [("fisher_home", obj [(k, obj [("mode", str "0600"), ("text", str v)]) | (k, v) <- files]), ("home", obj [])]) ]
      doc = obj [("epoch", PInt 1790000000), ("env_base", obj [("PATH", str "/usr/bin:/bin")])]
  _ <- replay doc Nothing c $ \sys -> do
    r <- try (act sys)
    writeIORef outcome $ case r of
      Right v -> obj [("result", v)]
      Left e -> case fromException e of
        Just a -> obj [("error", obj [("message", str (formatAuthError a)), ("code", maybe PNull str (aeCode a))])]
        Nothing -> obj [("other", str (T.pack (show (e :: SomeException))))]
    pure 0
  readIORef outcome
