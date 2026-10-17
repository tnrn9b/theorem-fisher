-- | The search tools' HTTP over http-client-tls, with failures worded as
-- Python's urllib words them (the tools pass that text on).
module Fisher.Lean.HttpReal
  ( realHttpClient
  ) where

import Control.Exception (IOException, SomeException, fromException, try)
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Foreign.C.Types (CInt (..))
import GHC.IO.Exception (IOException (..))
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Client.TLS as HT
import qualified Network.HTTP.Types as HTy

import Fisher.Lean.Http

-- | One manager for the process; each request carries its own timeout.
realHttpClient :: IO HttpClient
realHttpClient = do
  mgr <- HT.newTlsManager
  pure $ \rq -> do
    r <- try $ do
      req0 <- H.parseRequest (T.unpack (hqUrl rq))
      let req =
            req0
              { H.method = TE.encodeUtf8 (hqMethod rq)
              , H.requestHeaders = [(CI.mk (TE.encodeUtf8 k), TE.encodeUtf8 v) | (k, v) <- hqHeaders rq]
              , H.requestBody = maybe mempty H.RequestBodyBS (hqBody rq)
              , H.responseTimeout = H.responseTimeoutMicro (round (hqTimeout rq * 1000000))
              }
      H.httpLbs req mgr
    pure $ case r of
      Right resp ->
        let st = H.responseStatus resp
            code = HTy.statusCode st
            reason = TE.decodeUtf8With TEE.lenientDecode (HTy.statusMessage st)
         in if code >= 400 then HttpError code reason else HttpResponse code reason (BL.toStrict (H.responseBody resp))
      Left (e :: SomeException) -> HttpFailure (describe (hqUrl rq) e)

describe :: T.Text -> SomeException -> T.Text
describe url e = case fromException e of
  Just (H.HttpExceptionRequest _ c) -> case c of
    H.ResponseTimeout -> readTimeout
    H.ConnectionTimeout -> "<urlopen error timed out>"
    H.ConnectionFailure inner -> urlopenError inner
    other -> "<urlopen error " <> T.pack (show other) <> ">"
  Just (H.InvalidUrlException u why) -> "<urlopen error unknown url type: " <> T.pack (u <> " (" <> why <> ")") <> ">"
  Nothing -> T.pack (show e)
  where
    -- http.client's socket timeout on a TLS read is worded by ssl
    readTimeout = if "https:" `T.isPrefixOf` url then "The read operation timed out" else "timed out"
    urlopenError inner = case fromException inner of
      Just (ioe :: IOException) ->
        let desc = T.pack (ioe_description ioe)
            reason = T.dropWhileEnd (== ')') (snd (T.breakOnEnd "(" desc))
            msg = if T.null reason then desc else reason
         in case ioe_errno ioe of
              Just (CInt n) -> "<urlopen error [Errno " <> T.pack (show n) <> "] " <> fromMaybe msg (Just msg) <> ">"
              Nothing -> "<urlopen error " <> msg <> ">"
      Nothing -> "<urlopen error " <> T.pack (show inner) <> ">"
