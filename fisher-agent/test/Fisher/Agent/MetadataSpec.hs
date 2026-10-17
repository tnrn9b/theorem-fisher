-- | 'Fisher.Agent.Metadata' against @agent/model_metadata.py@
-- (@test/golden/b3/metadata.json@): context lengths with OpenRouter's
-- model list answered (a canned list) or unreachable, the disk cache file,
-- probe tiers, limits parsed from errors, text token estimates.
module Fisher.Agent.MetadataSpec (spec) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Agent.Metadata
import Fisher.Agent.TestUtil
import Fisher.LLM.Errors (LLMError (..))
import Fisher.LLM.Http
import Fisher.Py.Value

fakeTransport :: Maybe PyVal -> Transport
fakeTransport answer =
  Transport
    { tRequest = \_ _ -> pure $ case answer of
        Just v -> Right (HttpResp 200 [] (TE.encodeUtf8 (dumpsAscii v)))
        Nothing -> Left ConnectionError
    , tStream = \_ _ _ -> pure (Left ConnectionError)
    }

envWith :: Maybe PyVal -> FilePath -> IO MetaEnv
envWith answer home = do
  c <- newModelCache
  pure (MetaEnv (fakeTransport answer) (pure 1000) home c (const (pure ())))

spec :: Spec
spec = do
  doc <- runIO (loadDoc "test/golden/b3/metadata.json")
  let cases = list "cases" doc
      answer = field "models_answer" doc
  describe "agent/model_metadata.py" $ do
    it "has upstream's tables" $ do
      PList [PList [PStr k, PInt v] | (k, v) <- defaultContextLengths] `shouldBe` field "default_context_lengths" doc
      PList (map PInt contextProbeTiers) `shouldBe` field "probe_tiers" doc
    forM_ (zip [1 :: Int ..] cases) $ \(i, c) -> it (show i <> " " <> T.unpack (txt "kind" c)) $
      case txt "kind" c of
        "context_length" -> withSystemTempDirectory "fisher-meta" $ \home -> do
          env <- envWith (if txt "net" c == "ok" then Just answer else Nothing) home
          n <- getModelContextLength env (txt "model" c) (txt "base_url" c)
          n `shouldBe` int "length" c
        "disk_cache" -> withSystemTempDirectory "fisher-meta" $ \home -> do
          env <- envWith Nothing home
          forM_ (list "steps" c) $ \st -> do
            saveContextLength home (txt "model" st) (txt "base_url" st) (int "length" st)
            f <- BS.readFile (contextCachePath home)
            TE.decodeUtf8 f `shouldBe` txt "file" st
            n <- getModelContextLength env (txt "model" st) (txt "base_url" st)
            n `shouldBe` int "lookup" st
        "probe_tier" -> maybe PNull PInt (nextProbeTier (int "current" c)) `shouldBe` field "next" c
        "parse_limit" -> maybe PNull PInt (parseContextLimitFromError (txt "error" c)) `shouldBe` field "limit" c
        "estimate_text" -> estimateTokensRough (txt "text" c) `shouldBe` int "tokens" c
        k -> expectationFailure ("unknown kind " <> T.unpack k)
