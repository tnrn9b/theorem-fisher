-- | The LLM layer against upstream's own code paths (@tools/corpus/b1.py@):
-- message preparation, the HTTP request of every wire format, responses
-- through the full call (fake transport) and the SDK's retry rules and
-- error text.
module Fisher.LLM.GoldenSpec (spec, canned, quietRuntime, stubEnv, usageVal) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.IORef
import Data.List (sort)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Hspec

import Fisher.LLM.Client
import Fisher.LLM.Errors
import Fisher.LLM.Http
import Fisher.LLM.Normalize
import Fisher.LLM.Prepare
import Fisher.LLM.Request
import Fisher.LLM.Types
import Fisher.Py.Value

golden :: FilePath -> IO [PyVal]
golden name = do
  doc <- either fail pure . loads =<< BS.readFile ("test/golden/b1/" <> name)
  case get "cases" doc of
    Just (PList cs) -> pure cs
    _ -> fail (name <> ": no cases")

txt :: T.Text -> PyVal -> T.Text
txt k c = maybe "" pyStrOf (get k c)

nameOf :: PyVal -> String
nameOf = T.unpack . txt "name"

stubEnv :: IO Env
stubEnv = do
  uu <- counterUuids
  pure (Env uu (\_ role -> pure ("[The " <> role <> " attached an image. Here's what it contains:\nAN IMAGE]")))

settingsOf :: PyVal -> Settings
settingsOf c =
  let st = maybe (obj []) id (get "settings" c)
      t k = case get k st of
        Just (PStr x) -> x
        _ -> ""
      mode = maybe ChatCompletions id (parseApiMode (txt "mode" c))
      rt = maybe (obj []) id (get "routing" st)
      ls k = case get k rt of
        Just (PList xs) -> [x | PStr x <- xs]
        _ -> []
      ms k = case get k rt of
        Just (PStr x) -> Just x
        _ -> Nothing
   in Settings
        { sMode = mode
        , sBaseUrl = t "base_url"
        , sApiKey = t "api_key"
        , sModel = t "model"
        , sProvider = t "provider"
        , sMaxTokens = case get "max_tokens" st of
            Just (PInt n) -> Just n
            _ -> Nothing
        , sReasoning = case get "reasoning" st of
            Just PNull -> Nothing
            r -> r
        , sRouting = Routing (ls "providers_allowed") (ls "providers_ignored") (ls "providers_order") (ms "provider_sort") (get "provider_require_parameters" rt == Just (PBool True)) (ms "provider_data_collection")
        , sSessionId = t "session_id"
        , sTools = case get "tools" c of
            Just (PList ts) -> Just ts
            _ -> Nothing
        }

-- | A transport answering each request with the next canned response
-- (the last repeats), its body cut into small chunks.
canned :: [HttpResp] -> IO (Transport, IORef [HttpReq])
canned answers = do
  q <- newIORef answers
  seen <- newIORef []
  let next hr = do
        modifyIORef seen (<> [hr])
        as <- readIORef q
        case as of
          [a] -> pure a
          (a : rest) -> writeIORef q rest >> pure a
          [] -> pure (HttpResp 500 [] "{}")
  pure
    ( Transport
        { tRequest = \hr _ -> Right <$> next hr
        , tStream = \hr _ k -> do
            a <- next hr
            chunks <- newIORef (chunk 7 (rBody a))
            Right <$> k (rStatus a) (rHeaders a) (atomicModifyIORef' chunks (\cs -> case cs of (x : xs) -> (xs, x); [] -> ([], BS.empty)))
        }
    , seen )
  where
    chunk n b = if BS.null b then [] else BS.take n b : chunk n (BS.drop n b)

quietRuntime :: Runtime
quietRuntime = Runtime (pure 1790000000) (\_ -> pure ()) (pure 0)

usageVal :: Maybe Usage -> PyVal
usageVal = \case
  Nothing -> PNull
  Just u ->
    obj
      [ ("prompt_tokens", PInt (uPrompt u)), ("completion_tokens", PInt (uCompletion u)), ("total_tokens", PInt (uTotal u))
      , ("cache", obj ([("cache_read", PInt r) | Just r <- [uCacheRead u]] <> [("cache_write", PInt w) | Just w <- [uCacheWrite u]])) ]

spec :: Spec
spec = do
  prep <- runIO (golden "prepare.json")
  describe "message preparation (prepare.json)" $
    forM_ prep $ \c -> it (nameOf c) $ do
      let (sy, eph) = case get "system" c of
            Just (PStr s) -> (Just s, Nothing)
            Just (PList [PStr s, PStr e]) -> (Just s, Just e)
            _ -> (Nothing, Nothing)
          pre = case get "prefill" c of
            Just (PList p) -> p
            _ -> []
          hist = case get "history" c of
            Just (PList h) -> h
            _ -> []
      PList (prepareMessages (PrepareInput (txt "base_url" c) sy eph pre (get "caching" c == Just (PBool True))) hist)
        `shouldBe` maybe PNull id (get "prepared" c)

  reqs <- runIO (golden "requests.json")
  describe "requests (requests.json)" $
    forM_ reqs $ \c -> it (nameOf c) $ do
      env <- stubEnv
      let msgs = case get "prepared" c of
            Just (PList m) -> m
            _ -> []
      r <- buildRequest env (settingsOf c) msgs
      case (r, get "request" c) of
        (Right hr, Just ex) -> do
          hrMethod hr `shouldBe` txt "method" ex
          hrUrl hr `shouldBe` txt "url" ex
          sort (hrHeaders hr) `shouldBe` sort [(k, pyStrOf v) | Just (PDict kvs) <- [get "headers" ex], (k, v) <- kvs]
          fmap toAeson (hrBody hr) `shouldBe` fmap toAeson (get "body" ex)
        (Left e, Nothing) -> Just (str e) `shouldBe` (get "error" c >>= get "message")
        (Right _, Nothing) -> expectationFailure "upstream refused this request"
        (Left e, Just _) -> expectationFailure ("refused: " <> T.unpack e)

  resps <- runIO (golden "responses.json")
  describe "responses through the call (responses.json)" $
    forM_ resps $ \c -> it (nameOf c) $ do
      env <- stubEnv
      let mode = maybe ChatCompletions id (parseApiMode (txt "mode" c))
          ctype = txt "content_type" c
          bodies = case get "bodies" c of
            Just (PList bs) -> [b | PStr b <- bs]
            _ -> [txt "body" c]
          answers = [HttpResp 200 [("content-type", ctype)] (TE.encodeUtf8 b) | b <- bodies]
          tools = Just [obj [("type", str "function"), ("function", obj [("name", str "t"), ("description", str ""), ("parameters", obj [])])]]
          s = defaultSettings {sMode = mode, sTools = tools, sModel = "m", sBaseUrl = if mode == CodexResponses then "https://chatgpt.com/backend-api/codex" else "https://x.test"}
      (tr, _) <- canned answers
      res <- callModel quietRuntime tr env s [obj [("role", str "user"), ("content", str "x")]]
      let got = case res of
            Right (CallInvalid _) -> obj [("invalid", PBool True)]
            Right (CallOk o _) -> obj [("pre_finish_reason", oPreFinish o), ("finish_reason", oFinish o), ("usage", usageVal (oUsage o)), ("message", oMessage o)]
            Left e -> obj [("raised", obj [("type", str (errorClass e)), ("message", str (errorText e))])]
          expected = maybe PNull id (get "result" c)
          -- a normalizer error is reported with the usage upstream had read
          -- by then; the call returns only the error.
          expected' = case get "error" expected of
            Just er -> obj [("raised", er)]
            Nothing -> expected
      got `shouldBe` expected'

  sdk <- runIO (golden "sdk.json")
  describe "SDK retry rules and error text (sdk.json)" $
    forM_ sdk $ \c -> it (nameOf c) $ case txt "kind" c of
      "retry" -> do
        let st = case get "status" c of
              Just (PInt n) -> fromInteger n
              _ -> 0
            hs = [(k, v) | Just (PDict kvs) <- [get "headers" c], (k, PStr v) <- kvs]
            num = \case
              PFloat d -> d
              PInt i -> fromInteger i
              _ -> 0
            expected = [(num a, num b) | Just (PList ds) <- [get "delays" c], PList [a, b] <- ds]
            got = [(retryDelay 1790000000 n hs 0, retryDelay 1790000000 n hs 1) | n <- [0, 1, 2]]
        shouldRetry 1790000000 st hs `shouldBe` (get "should_retry" c == Just (PBool True))
        forM_ (zip got expected) $ \((a, b), (x, y)) -> do
          abs (a - x) < 1e-9 `shouldBe` True
          abs (b - y) < 1e-9 `shouldBe` True
      "error" -> do
        let e = statusError (maybe 0 (\case PInt n -> fromInteger n; _ -> 0) (get "status" c)) (TE.encodeUtf8 (txt "body" c))
        errorText e `shouldBe` txt "str" c
        errorClass e `shouldBe` txt "class" c
      _ -> do
        let e = if txt "name" c == "error/connection" then ConnectionError else TimeoutError
        errorText e `shouldBe` txt "str" c
        errorClass e `shouldBe` txt "class" c

  describe "the SDK send loop" $ do
    it "retries a 429 twice, then gives up with the status error" $ do
      (tr, seen) <- canned [HttpResp 429 [] "{\"error\":{\"message\":\"slow down\"}}"]
      slept <- newIORef []
      let rt = quietRuntime {rtSleep = \d -> modifyIORef slept (<> [d])}
      r <- sdkSend rt tr (HttpReq "POST" "https://x.test/v1/chat/completions" [] Nothing False) chatTimeouts
      fmap errorText (either Just (const Nothing) r) `shouldBe` Just "Error code: 429 - {'error': {'message': 'slow down'}}"
      length <$> readIORef seen `shouldReturn` 3
      readIORef slept `shouldReturn` [0.5, 1.0]
    it "honours Retry-After and stops on success" $ do
      (tr, seen) <- canned [HttpResp 503 [("Retry-After", "3")] "", HttpResp 200 [] "{}"]
      slept <- newIORef []
      let rt = quietRuntime {rtSleep = \d -> modifyIORef slept (<> [d])}
      r <- sdkSend rt tr (HttpReq "POST" "https://x.test" [] Nothing False) chatTimeouts
      fmap rStatus r `shouldBe` Right 200
      length <$> readIORef seen `shouldReturn` 2
      readIORef slept `shouldReturn` [3.0]
    it "does not retry a 400" $ do
      (tr, seen) <- canned [HttpResp 400 [] "bad"]
      r <- sdkSend quietRuntime tr (HttpReq "POST" "https://x.test" [] Nothing False) chatTimeouts
      fmap errorText (either Just (const Nothing) r) `shouldBe` Just "bad"
      length <$> readIORef seen `shouldReturn` 1
