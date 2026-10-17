-- | 'Fisher.Agent.Compress' against @agent/context_compressor.py@
-- (@test/golden/b3/compress.json@): generated message lists compressed
-- with the summary call answered from a script (a summary, no provider,
-- an error, empty, None, a dict, an already-prefixed summary), the
-- tool-pair sanitizer, the prefix rule, and message token estimates.
module Fisher.Agent.CompressSpec (spec) where

import Control.Monad (forM_)
import Data.IORef
import qualified Data.Text as T
import Test.Hspec

import Fisher.Agent.Compress
import Fisher.Agent.Metadata (estimateMessagesTokensRough)
import Fisher.Agent.TestUtil
import Fisher.Agent.Types
import Fisher.Py.Value

callVal :: AuxCall -> PyVal
callVal c =
  PDict $
    [ ("task", maybe PNull PStr (axTask c)), ("messages", PList (axMessages c))
    , ("temperature", maybe PNull PFloat (axTemperature c)), ("max_tokens", maybe PNull PInt (axMaxTokens c))
    , ("timeout", PFloat (axTimeout c)) ]
      <> maybe [] (\m -> [("model", PStr m)]) (axModel c)

answer :: T.Text -> AuxCall -> Either AuxError PyVal
answer mode c = case mode of
  "runtime" -> Left (AuxNoProvider "No LLM provider configured for task=compression provider=auto. Run: gauss setup")
  "error" -> Left (AuxFailed "boom")
  "empty" -> content (PStr "")
  "none" -> content PNull
  "dict" -> content (PDict [("text", PStr "x")])
  "prefixed" -> content (PStr "  [CONTEXT SUMMARY]: did stuff  ")
  _ -> content (PStr ("Summary of " <> T.pack (show (T.length (firstContent c))) <> " chars."))
  where
    content v = Right (PDict [("choices", PList [PDict [("message", PDict [("content", v)])]])])
    firstContent x = case axMessages x of
      (m : _) | Just (PStr s) <- get "content" m -> s
      _ -> ""

spec :: Spec
spec = do
  cases <- runIO (loadCases "test/golden/b3/compress.json")
  describe "agent/context_compressor.py" $
    forM_ (zip [1 :: Int ..] cases) $ \(i, c) -> it (show i <> " " <> T.unpack (txt "kind" c)) $
      case txt "kind" c of
        "compress" -> do
          calls <- newIORef []
          printed <- newIORef []
          let override = if T.null (txt "override" c) then Nothing else Just (txt "override" c)
              comp0 = newCompressor "anthropic/claude-opus-4.6" "" 0.5 3 4 500 (field "quiet" c == PBool True) override 200000
              comp = comp0 {cCompressionCount = fromIntegral (int "compression_count" c), cLastPrompt = int "last_prompt_tokens" c}
              llm call = modifyIORef calls (<> [call]) >> pure (answer (txt "mode" c) call)
              current = case field "current_tokens" c of
                PInt n -> Just n
                _ -> Nothing
          (out, _) <- compress llm (\t -> modifyIORef printed (<> [t])) (const (pure ())) comp (list "messages" c) current
          PList out `shouldBe` field "out" c
          ps <- readIORef printed
          T.concat [p <> "\n" | p <- ps] `shouldBe` txt "printed" c
          cs <- readIORef calls
          PList (map callVal cs) `shouldBe` field "calls" c
        "sanitize" -> PList (sanitizeToolPairs (list "messages" c)) `shouldBe` field "out" c
        "prefix" -> withSummaryPrefix (txt "summary" c) `shouldBe` txt "out" c
        "estimate_messages" -> estimateMessagesTokensRough (list "messages" c) `shouldBe` int "tokens" c
        k -> expectationFailure ("unknown kind " <> T.unpack k)
