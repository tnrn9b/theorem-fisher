-- | 'Fisher.Agent.Tools.Web' against @tools/web_tools.py@ with
-- firecrawl-py (@test/golden/b4/web.json@): each case's tool result, the
-- requests sent to Firecrawl, the sleeps, and the summariser calls.
module Fisher.Agent.WebSpec (spec) where

import Control.Monad (forM_)
import Data.IORef
import Data.List (sort)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Hspec

import Fisher.Agent.TestUtil
import Fisher.Agent.Tools.Web
import Fisher.Agent.Types
import Fisher.LLM.Errors (LLMError (..))
import Fisher.LLM.Http (HttpResp (..), Transport (..))
import Fisher.LLM.Types (HttpReq (..))
import Fisher.Py.Value
import Fisher.Tools.Registry

-- | @{"$gen": [[text, n], ...]}@ back to the text.
expand :: PyVal -> PyVal
expand = \case
  PDict [("$gen", PList parts)] -> PStr (T.concat [T.replicate (fromIntegral n) t | PList [PStr t, PInt n] <- parts])
  PDict kvs -> PDict [(k, expand v) | (k, v) <- kvs]
  PList xs -> PList (map expand xs)
  v -> v

spec :: Spec
spec = do
  cases <- runIO (loadCases "test/golden/b4/web.json")
  describe "tools/web_tools.py" $
    forM_ cases $ \c -> it (T.unpack (txt "name" c)) $ do
      answers <- newIORef (map expand (list "answers" c))
      llmAnswers <- newIORef (list "llm_answers" c)
      reqs <- newIORef []
      sleeps <- newIORef []
      calls <- newIORef []
      let next q = atomicModifyIORef' q (\case [] -> ([], Nothing); (x : xs) -> (xs, Just x))
          respond hr = do
            modifyIORef reqs (<> [hr])
            a <- next answers
            pure $ case a of
              Nothing -> Left ConnectionError
              Just ans
                | Just (PStr _) <- get "raise" ans -> Left (ApiError (maybe "boom" pyStrOf (get "message" ans)))
                | otherwise ->
                    let body = case get "body" ans of
                          Just (PStr b) -> b
                          _ -> dumpsAscii (field "json" ans)
                        hs = case get "headers" ans of
                          Just (PDict kvs) -> [(k, pyStrOf v) | (k, v) <- kvs]
                          _ -> [("Content-Type", "application/json")]
                     in Right (HttpResp (fromIntegral (case get "status" ans of Just (PInt s) -> s; _ -> 200)) hs (TE.encodeUtf8 body))
          tr = Transport (\hr _ -> respond hr) (\_ _ _ -> pure (Left ConnectionError))
          env0 = PDict [(k, v) | (k, v) <- case field "env" c of PDict kvs -> kvs; _ -> []]
          getEnv k = pure (case get k env0 of Just (PStr v) -> Just v; _ -> Nothing)
          aux call = do
            modifyIORef calls (<> [call])
            a <- next llmAnswers
            pure $ case a of
              Nothing -> Left (AuxFailed "no scripted answer")
              Just ans -> case get "raise" ans of
                Just (PStr "RuntimeError") -> Left (AuxNoProvider "no provider")
                Just (PStr m) -> Left (AuxFailed m)
                _ -> Right (PDict [("choices", PList [PDict [("message", PDict [("content", field "content" ans)])]])])
      env <- newWebEnv tr getEnv (\s -> modifyIORef sleeps (<> [s])) aux (pure (field "interrupted" c == PBool True))
      reg <- newRegistry
      registerWebTools reg env
      out <- dispatch reg defaultCtx (txt "tool" c) (field "args" c)
      out `shouldBe` txt "result" c
      rs <- readIORef reqs
      PList [PDict [("method", PStr (hrMethod r)), ("url", PStr (hrUrl r)), ("headers", PDict [(k, PStr v) | (k, v) <- hrHeaders r]), ("body", PStr (maybe "" dumpsAscii (hrBody r)))] | r <- rs]
        `shouldBe` PList [PDict [(k, field k r) | k <- ["method", "url", "headers", "body"]] | r <- list "requests" c]
      ss <- readIORef sleeps
      sort ss `shouldBe` sort [case v of PInt n -> fromIntegral n; PFloat d -> d; _ -> 0 | v <- list "sleeps" c]
      cs <- readIORef calls
      let shown call =
            dumpsAscii . PDict $
              [ ("task", maybe PNull PStr (axTask call))
              , ("messages", PList [PDict [("role", field "role" m), ("content", squeeze (field "content" m))] | m <- axMessages call]) ]
                <> [("model", PStr m) | Just m <- [axModel call]]
                <> [("temperature", maybe PNull PFloat (axTemperature call)), ("max_tokens", maybe PNull PInt (axMaxTokens call))]
          squeeze = \case
            PStr s | T.length s > 20000 -> PDict [("len", PInt (fromIntegral (T.length s))), ("head", PStr (T.take 400 s)), ("tail", PStr (T.takeEnd 400 s))]
            v -> v
          recorded r =
            dumpsAscii . PDict $
              [("task", field "task" r), ("messages", field "messages" r)]
                <> [("model", m) | Just m <- [get "model" r]]
                <> [("temperature", field "temperature" r), ("max_tokens", field "max_tokens" r)]
      sort (map shown cs) `shouldBe` sort (map recorded (list "llm_calls" c))
  describe "web tool registration" $
    it "matches upstream's entries" $ do
      doc <- loadDoc "test/golden/b4/web.json"
      reg <- newRegistry
      tr <- pure (Transport (\_ _ -> pure (Left ConnectionError)) (\_ _ _ -> pure (Left ConnectionError)))
      env <- newWebEnv tr (\_ -> pure Nothing) (\_ -> pure ()) (\_ -> pure (Left (AuxNoProvider ""))) (pure False)
      registerWebTools reg env
      forM_ ["web_search", "web_extract"] $ \n -> do
        Just td <- lookupTool reg n
        let e = field n (field "entries" doc)
        (n, tdSchema td, tdToolset td, tdEmoji td) `shouldBe` (n, field "schema" e, txt "toolset" e, txt "emoji" e)
        PList (map PStr (tdRequiresEnv td)) `shouldBe` field "requires_env" e
