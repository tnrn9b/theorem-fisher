-- | Who may talk to @fisher ui@: the page on this machine that was given
-- the start token. Three rules (spec section 4):
--
--   * Host must be @127.0.0.1:<port>@ or @localhost:<port>@, so a DNS
--     rebinding page on another name is refused (403) on every path.
--   * Every @/api@ route needs the cookie @fisher_ui_<port>=<token>@ (else
--     401). The port is in the name since a browser keeps cookies per host,
--     not per port: two servers on one machine keep a cookie each.
--   * A mutating request (not GET, HEAD) needs an @Origin@ equal to
--     @http://<Host>@ (else 403).
--
-- No CORS header is ever sent.
module Fisher.UI.Access
  ( Gate (..)
  , newToken
  , guardRequest
  , cookieName
  , tokenCookie
  , cookieToken
  , isApi
  , sameText
  ) where

import Data.Bits (xor, (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (Status, forbidden403, methodGet, methodHead, unauthorized401)
import Network.Wai (Request (..))
import System.IO (IOMode (..), withBinaryFile)
import Text.Printf (printf)

-- | The start token and the port the server listens on.
data Gate = Gate { gToken :: Text, gPort :: Int }
  deriving (Eq, Show)

-- | 128 random bits from the system's generator, as 32 hex characters.
newToken :: IO Text
newToken = do
  bytes <- withBinaryFile "/dev/urandom" ReadMode (`B.hGet` 16)
  if B.length bytes /= 16
    then ioError (userError "fisher ui: could not read 16 bytes from /dev/urandom")
    else pure (T.pack (concatMap (printf "%02x") (B.unpack bytes)))

-- | The cookie's name for the server on this port.
cookieName :: Int -> B.ByteString
cookieName port = "fisher_ui_" <> BC.pack (show port)

-- | The Set-Cookie value that trades the token for a cookie.
tokenCookie :: Int -> Text -> B.ByteString
tokenCookie port t = cookieName port <> "=" <> TE.encodeUtf8 t <> "; HttpOnly; SameSite=Strict; Path=/"

-- | The value of the port's cookie, if the request has one.
cookieToken :: Int -> Request -> Maybe B.ByteString
cookieToken port r =
  case [v | (k, h) <- requestHeaders r, k == "Cookie", (n, v) <- pairs h, n == cookieName port] of
    (v : _) -> Just v
    [] -> Nothing
  where
    pairs h = [(trim n, B.drop 1 v) | part <- BC.split ';' h, let (n, v) = BC.break (== '=') (trim part)]
    trim = BC.dropWhile (== ' ') . BC.dropWhileEnd (== ' ')

isApi :: Request -> Bool
isApi r = case pathInfo r of
  ("api" : _) -> True
  _ -> False

-- | Compare without stopping at the first difference.
sameText :: B.ByteString -> B.ByteString -> Bool
sameText a b = B.length a == B.length b && B.foldl' (.|.) 0 (B.packZipWith xor a b) == 0

guardRequest :: Gate -> Request -> Either Status ()
guardRequest g r
  | not hostOk = Left forbidden403
  | mutating && origin /= fmap ("http://" <>) host = Left forbidden403
  | isApi r && not cookieOk = Left unauthorized401
  | otherwise = Right ()
  where
    host = requestHeaderHost r
    port = BC.pack (show (gPort g))
    hostOk = host `elem` map Just ["127.0.0.1:" <> port, "localhost:" <> port]
    mutating = requestMethod r `notElem` [methodGet, methodHead]
    origin = lookup "Origin" (requestHeaders r)
    cookieOk = maybe False (sameText (TE.encodeUtf8 (gToken g))) (cookieToken (gPort g) r)
