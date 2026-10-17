-- | The route type and the JSON response every route sends (shared by the
-- server and the route modules).
module Fisher.UI.Response
  ( Api
  , jsonResponse
  , pageHeaders
  ) where

import qualified Data.Aeson as A
import Network.HTTP.Types (Header, Status, hContentType)
import Network.Wai (Request, Response, responseLBS)

-- | One route family: 'Nothing' when the request is not for it.
type Api = Request -> IO (Maybe Response)

jsonResponse :: A.ToJSON a => Status -> a -> Response
jsonResponse s v = responseLBS s ((hContentType, "application/json") : ("Cache-Control", "no-store") : pageHeaders) (A.encode v)

pageHeaders :: [Header]
pageHeaders =
  [ ("X-Frame-Options", "DENY")
  , ("X-Content-Type-Options", "nosniff")
  , ("Referrer-Policy", "no-referrer")
  , ("Content-Security-Policy", "default-src 'self'; img-src 'self' data:; style-src 'self' 'unsafe-inline'; frame-ancestors 'none'")
  ]
