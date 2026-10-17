-- | @resolve_runtime_provider@ against upstream's recorded runs
-- (@tools/corpus/b1_auth.py@, @resolve.json@): env keys and @.env@,
-- config provider and base URL, custom providers, Nous (cached, refresh,
-- mint, failures), Codex (JWT expiry, refresh, migration), Claude Code
-- credentials and explicit names. Each case is replayed through a scripted
-- 'Sys' and compared on the result (or error), what was printed, the HTTP
-- requests made and every file left behind.
module Fisher.LLM.AuthResolveSpec (spec) where

import Control.Exception (SomeException, fromException, try)
import Control.Monad (forM_, unless)
import Data.IORef
import qualified Data.Text as T
import Test.Hspec

import Fisher.LLM.Auth.Env (loadDotenvIntoSys)
import Fisher.LLM.Auth.Error
import Fisher.LLM.Auth.Resolve
import Fisher.LLM.Replay
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
      [ ("type", str "AuthError"), ("module", str "gauss_cli.auth"), ("message", str (formatAuthError a))
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
