-- | The B1 gate: real provider responses, captured by
-- @tools/capture-b1.py@ with the user's own credentials, normalized by
-- upstream's code (@b1.py live-normalize@ → @live.json@), and parsed by
-- Fisher through the full call path. Pending until a capture exists.
module Fisher.LLM.LiveSpec (spec) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (doesFileExist)
import Test.Hspec

import Fisher.LLM.Client
import Fisher.LLM.Errors
import Fisher.LLM.GoldenSpec (canned, quietRuntime, stubEnv, usageVal)
import Fisher.LLM.Http
import Fisher.LLM.Normalize
import Fisher.LLM.Types
import Fisher.Py.Value

spec :: Spec
spec = do
  present <- runIO (doesFileExist "test/golden/b1/live.json")
  if not present
    then describe "real provider captures (live.json)" $ it "has a capture" $ pendingWith "run tools/capture-b1.py, then b1.py live-normalize"
    else do
      doc <- runIO (either fail pure . loads =<< BS.readFile "test/golden/b1/live.json")
      let cases = case get "cases" doc of
            Just (PList cs) -> cs
            _ -> []
      describe "real provider captures (live.json)" $ do
        it "has at least one capture" $ null cases `shouldBe` False
        forM_ cases $ \c -> it (T.unpack (maybe "" pyStrOf (get "name" c))) $ do
          env <- stubEnv
          let mode = maybe ChatCompletions id (parseApiMode (maybe "" pyStrOf (get "mode" c)))
              status = case get "status" c of
                Just (PInt n) -> fromInteger n
                _ -> 200
              bodies = case get "bodies" c of
                Just (PList bs) -> [b | PStr b <- bs]
                _ -> [maybe "" pyStrOf (get "body" c)]
              ctype = maybe "" pyStrOf (get "content_type" c)
              tools = Just [obj [("type", str "function"), ("function", obj [("name", str "get_time"), ("description", str ""), ("parameters", obj [])])]]
              s = defaultSettings {sMode = mode, sTools = tools, sModel = maybe "m" pyStrOf (get "model" c), sBaseUrl = if mode == CodexResponses then "https://chatgpt.com/backend-api/codex" else "https://x.test"}
          (tr, _) <- canned [HttpResp status [("content-type", ctype)] (TE.encodeUtf8 b) | b <- bodies]
          res <- callModel quietRuntime {rtSleep = \_ -> pure ()} tr env s [obj [("role", str "user"), ("content", str "x")]]
          let expected = maybe PNull id (get "result" c)
          case get "http_status" expected of
            Just (PInt st) -> case res of
              Left (StatusError got _ _) -> toInteger got `shouldBe` st
              _ -> expectationFailure "expected the status error upstream raised"
            _ -> do
              let got = case res of
                    Right (CallInvalid _) -> obj [("invalid", PBool True)]
                    Right (CallOk o _) -> obj [("pre_finish_reason", oPreFinish o), ("finish_reason", oFinish o), ("usage", usageVal (oUsage o)), ("message", oMessage o)]
                    Left e -> obj [("raised", obj [("type", str (errorClass e)), ("message", str (errorText e))])]
                  expected' = case get "error" expected of
                    Just er -> obj [("raised", er)]
                    Nothing -> expected
              got `shouldBe` expected'
