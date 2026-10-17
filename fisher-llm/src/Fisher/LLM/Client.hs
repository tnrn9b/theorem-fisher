-- | One model call, end to end: the request for the active wire format,
-- the SDK's send and retries, the Codex stream ladder (retry once, then
-- @create(stream=True)@), and the normalized result.
module Fisher.LLM.Client
  ( CallResult (..)
  , callModel
  , timeoutsFor
  , readEvents
  ) where

import Data.IORef
import qualified Data.ByteString as BS
import Data.Text (Text)

import qualified Fisher.LLM.Codex as Cx
import Fisher.LLM.Errors
import Fisher.LLM.Http
import Fisher.LLM.Normalize
import Fisher.LLM.Request
import Fisher.LLM.Sse
import Fisher.LLM.Types
import Fisher.Py.Value

data CallResult
  = CallInvalid PyVal
  -- ^ the response arrived but has no choices / content / output
  -- (upstream's invalid-shape retry); the decoded response
  | CallOk Outcome PyVal
  -- ^ the normalized outcome and the decoded response

timeoutsFor :: ApiMode -> Timeouts
timeoutsFor = \case
  ChatCompletions -> chatTimeouts
  CodexResponses -> codexTimeouts
  AnthropicMessages -> anthropicTimeouts

-- | Read a whole SSE stream into (event name, decoded data) pairs; an
-- event whose data is not JSON is skipped.
readEvents :: IO BS.ByteString -> IO [(Maybe Text, PyVal)]
readEvents reader = do
  ref <- newIORef newParser
  acc <- newIORef []
  let loop = do
        chunk <- reader
        p <- readIORef ref
        if BS.null chunk
          then modifyIORef acc (<> finish p)
          else do
            let (p', evs) = feed p chunk
            writeIORef ref p'
            modifyIORef acc (<> evs)
            loop
  loop
  evs <- readIORef acc
  pure [(seEvent e, v) | e <- evs, Right v <- [eventJson e]]

callModel :: Runtime -> Transport -> Env -> Settings -> [PyVal] -> IO (Either LLMError CallResult)
callModel rt tr env s msgs =
  buildRequest env s msgs >>= \case
    Left e -> pure (Left (LocalError "ValueError" e))
    Right hr -> case sMode s of
      CodexResponses -> codex hr
      mode ->
        sdkSend rt tr hr (timeoutsFor mode) >>= \case
          Left e -> pure (Left e)
          Right resp -> case loads (rBody resp) of
            Left _ -> pure (Left (ApiError "Invalid JSON in response body"))
            Right v -> finishWith mode v
  where
    finishWith mode v
      | responseInvalid mode v = pure (Right (CallInvalid v))
      | otherwise =
          outcomeOf (eUuidHex env) mode v >>= \case
            Left e -> pure (Left (LocalError "RuntimeError" e))
            Right o -> pure (Right (CallOk o v))
    stream hr = sdkStream rt tr hr codexTimeouts readEvents
    codex hr = attempt (0 :: Int)
      where
        attempt n =
          stream hr >>= \case
            Left e -> pure (Left e)
            Right evs -> case Cx.accumulateStream evs of
              Cx.StreamCompleted r -> finishWith CodexResponses r
              Cx.StreamError m -> pure (Left (ApiError m))
              Cx.StreamNoCompleted
                | n < 1 -> attempt (n + 1)
                | otherwise ->
                    stream hr >>= \case
                      Left e -> pure (Left e)
                      Right evs' -> case Cx.fallbackTerminal evs' of
                        Right r -> finishWith CodexResponses r
                        Left m -> pure (Left (LocalError "RuntimeError" m))
