-- | The real 'HttpClient' over http-client-tls.
module Fisher.Mcp.HttpClient
  ( tlsHttpClient
  ) where

import Control.Exception (SomeException, fromException, try)
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Client.TLS as HT
import qualified Network.HTTP.Types as HTy

import Fisher.Mcp.Http (HttpClient (..), HttpResponse (..))

-- | One client for every HTTP server (the read timeout is the SDK's 300 s;
-- connecting is bounded by each server's @connect_timeout@ in the caller).
tlsHttpClient :: IO HttpClient
tlsHttpClient = do
  mgr <- HT.newTlsManagerWith HT.tlsManagerSettings {H.managerResponseTimeout = H.responseTimeoutMicro 300000000}
  pure . HttpClient $ \method url headers body -> do
    r <- try $ do
      req0 <- H.parseRequest (T.unpack url)
      let req =
            req0
              { H.method = TE.encodeUtf8 method
              , H.requestHeaders = [(CI.mk (TE.encodeUtf8 k), TE.encodeUtf8 v) | (k, v) <- headers]
              , H.requestBody = maybe mempty H.RequestBodyBS body
              , H.responseTimeout = H.responseTimeoutMicro 300000000
              }
      resp <- H.responseOpen req mgr
      pure
        HttpResponse
          { hrStatus = HTy.statusCode (H.responseStatus resp)
          , hrHeaders = [(T.toLower (dec (CI.original k)), dec v) | (k, v) <- H.responseHeaders resp]
          , hrRead = H.brRead (H.responseBody resp)
          , hrClose = H.responseClose resp
          }
    pure $ case r of
      Right x -> Right x
      Left (e :: SomeException) -> Left (describe e)
  where
    dec = TE.decodeUtf8With TEE.lenientDecode
    describe e = case fromException e of
      Just (H.HttpExceptionRequest _ c) -> case c of
        H.ConnectionFailure _ -> "All connection attempts failed"
        H.ConnectionTimeout -> "timed out"
        H.ResponseTimeout -> "timed out"
        other -> T.pack (show other)
      Just (H.InvalidUrlException u why) -> T.pack ("Invalid URL " <> u <> ": " <> why)
      Nothing -> T.pack (show e)
