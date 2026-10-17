{-# LANGUAGE RankNTypes #-}
-- | HTTP for the LLM clients: a transport (real, over http-client-tls, or
-- canned in tests) and the SDKs' send loop with its two retries.
module Fisher.LLM.Http
  ( HttpResp (..)
  , Timeouts (..)
  , chatTimeouts
  , codexTimeouts
  , anthropicTimeouts
  , Transport (..)
  , Runtime (..)
  , systemRuntime
  , tlsTransport
  , sdkSend
  , sdkStream
  , userAgent
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, fromException, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Client.TLS as HT
import qualified Network.HTTP.Types as HTy
import System.Random (randomRIO)

import Fisher.LLM.Errors
import Fisher.LLM.Types
import Fisher.Py.Value (dumpsUnicode)

data HttpResp = HttpResp
  { rStatus :: Int
  , rHeaders :: [(Text, Text)]
  , rBody :: BS.ByteString
  }
  deriving (Eq, Show)

-- | Connect and read limits, in seconds.
data Timeouts = Timeouts
  { toConnect :: Double
  , toRead :: Double
  }
  deriving (Eq, Show)

-- | chat completions pass @timeout=900.0@ (every phase); Codex keeps the
-- SDK default (600 s, 5 s to connect); Anthropic 900 s with 10 s to
-- connect.
chatTimeouts, codexTimeouts, anthropicTimeouts :: Timeouts
chatTimeouts = Timeouts 900 900
codexTimeouts = Timeouts 5 600
anthropicTimeouts = Timeouts 10 900

data Transport = Transport
  { tRequest :: HttpReq -> Timeouts -> IO (Either LLMError HttpResp)
  -- ^ one request, the whole body read
  , tStream :: forall a. HttpReq -> Timeouts -> (Int -> [(Text, Text)] -> IO BS.ByteString -> IO a) -> IO (Either LLMError a)
  -- ^ one request; the consumer gets status, headers and a chunk reader
  -- (empty at the end)
  }

-- | Clock, sleep and randomness, so retries can be tested.
data Runtime = Runtime
  { rtNow :: IO Double
  , rtSleep :: Double -> IO ()
  , rtRandom :: IO Double
  }

systemRuntime :: Runtime
systemRuntime =
  Runtime
    { rtNow = realToFrac <$> getPOSIXTime
    , rtSleep = \s -> threadDelay (max 0 (round (s * 1e6)))
    , rtRandom = randomRIO (0, 0.999999)
    }

userAgent :: Text
userAgent = "Fisher/0.2.0"

-- | The real transport. Fisher's own User-Agent unless the request carries
-- one (Kimi's).
tlsTransport :: IO Transport
tlsTransport = do
  mgr <- HT.newTlsManager
  let build hr to = do
        req0 <- H.parseRequest (T.unpack (hrUrl hr))
        let hs = [(CI.mk (TE.encodeUtf8 k), TE.encodeUtf8 v) | (k, v) <- hrHeaders hr]
            hs' = if any ((== "user-agent") . T.toLower . fst) (hrHeaders hr) then hs else hs <> [("User-Agent", TE.encodeUtf8 userAgent)]
        pure
          req0
            { H.method = TE.encodeUtf8 (hrMethod hr)
            , H.requestHeaders = hs'
            , H.requestBody = maybe mempty (H.RequestBodyBS . TE.encodeUtf8 . dumpsUnicode) (hrBody hr)
            , H.responseTimeout = H.responseTimeoutMicro (round (toRead to * 1e6))
            }
      headersOf r = [(TE.decodeUtf8With TEE.lenientDecode (CI.original k), TE.decodeUtf8With TEE.lenientDecode v) | (k, v) <- H.responseHeaders r]
      classify :: SomeException -> LLMError
      classify e = case fromException e of
        Just (H.HttpExceptionRequest _ c) -> case c of
          H.ResponseTimeout -> TimeoutError
          H.ConnectionTimeout -> TimeoutError
          _ -> ConnectionError
        _ -> ConnectionError
  pure
    Transport
      { tRequest = \hr to -> do
          r <- try $ do
            req <- build hr to
            resp <- H.httpLbs req mgr
            pure (HttpResp (HTy.statusCode (H.responseStatus resp)) (headersOf resp) (BL.toStrict (H.responseBody resp)))
          pure (either (Left . classify) Right r)
      , tStream = \hr to k -> do
          r <- try $ do
            req <- build hr to
            H.withResponse req mgr $ \resp ->
              k (HTy.statusCode (H.responseStatus resp)) (headersOf resp) (H.brRead (H.responseBody resp))
          pure (either (Left . classify) Right r)
      }

-- | The SDK's request loop: up to two retries on connection errors,
-- timeouts and retryable statuses, waiting as @_calculate_retry_timeout@
-- says; a non-2xx answer that is not retried becomes its status error.
sdkSend :: Runtime -> Transport -> HttpReq -> Timeouts -> IO (Either LLMError HttpResp)
sdkSend rt tr hr to = go 0
  where
    go n =
      tRequest tr hr to >>= \case
        Left e
          | n < sdkMaxRetries -> wait n [] >> go (n + 1)
          | otherwise -> pure (Left e)
        Right resp
          | rStatus resp >= 200 && rStatus resp < 300 -> pure (Right resp)
          | otherwise -> do
              now <- rtNow rt
              if n < sdkMaxRetries && shouldRetry now (rStatus resp) (rHeaders resp)
                then wait n (rHeaders resp) >> go (n + 1)
                else pure (Left (statusError (rStatus resp) (rBody resp)))
    wait n hs = do
      now <- rtNow rt
      r <- rtRandom rt
      rtSleep rt (retryDelay now n hs r)

-- | 'sdkSend' for a streamed body: the status is checked (and retried)
-- before the consumer sees the stream.
sdkStream :: Runtime -> Transport -> HttpReq -> Timeouts -> (IO BS.ByteString -> IO a) -> IO (Either LLMError a)
sdkStream rt tr hr to consume = go 0
  where
    go n = do
      r <- tStream tr hr to $ \status hs reader ->
        if status >= 200 && status < 300
          then Right <$> consume reader
          else do
            body <- drain reader
            pure (Left (status, hs, body))
      case r of
        Left e
          | n < sdkMaxRetries -> wait n [] >> go (n + 1)
          | otherwise -> pure (Left e)
        Right (Right a) -> pure (Right a)
        Right (Left (status, hs, body)) -> do
          now <- rtNow rt
          if n < sdkMaxRetries && shouldRetry now status hs
            then wait n hs >> go (n + 1)
            else pure (Left (statusError status body))
    drain reader = do
      let loop acc = do
            c <- reader
            if BS.null c then pure (BS.concat (reverse acc)) else loop (c : acc)
      loop []
    wait n hs = do
      now <- rtNow rt
      r <- rtRandom rt
      rtSleep rt (retryDelay now n hs r)
