module Fisher.UI.AccessSpec (spec) where

import Control.Exception (bracket)
import Control.Monad (filterM, forM_, when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Char (isHexDigit)
import Data.List (find, isPrefixOf, isSuffixOf)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (NominalDiffTime, diffUTCTime)
import Network.HTTP.Types
import Network.Wai (Application, Request (..))
import Network.Wai.Test
import Network.Socket (SockAddr (..), close, getSocketName, tupleToHostAddress)
import System.Directory (doesDirectoryExist, getModificationTime, listDirectory)
import System.FilePath ((</>))
import Test.Hspec

import Fisher.UI.Access
import Fisher.UI.Server
import Fisher.UI.Static
import Fisher.UI.TestDirs (uiDir)

token :: T.Text
token = "0123456789abcdef0123456789abcdef"

gate :: Gate
gate = Gate token 8732

app :: Application
app = serve gate []

-- | A request as Warp builds it: the Host lands in 'requestHeaderHost'
-- and in the headers.
req :: Method -> B.ByteString -> B.ByteString -> [Header] -> Request
req m host pathQ hs =
  setPath
    defaultRequest
      { requestMethod = m
      , requestHeaderHost = Just host
      , requestHeaders = ("Host", host) : hs
      }
    pathQ

local :: B.ByteString
local = "127.0.0.1:8732"

cookie :: Header
cookie = (hCookie, "theme=dark; fisher_ui_8732=" <> TE.encodeUtf8 token)

run :: Request -> IO SResponse
run r = runSession (request r) app

header :: HeaderName -> SResponse -> Maybe B.ByteString
header n = lookup n . simpleHeaders

spec :: Spec
spec = do
  describe "token exchange" $ do
    it "GET /?t=<token> sets the cookie and redirects to /" $ do
      r <- run (req "GET" local ("/?t=" <> TE.encodeUtf8 token) [])
      statusCode (simpleStatus r) `shouldBe` 302
      header hLocation r `shouldBe` Just "/"
      header "Set-Cookie" r `shouldBe` Just ("fisher_ui_8732=" <> TE.encodeUtf8 token <> "; HttpOnly; SameSite=Strict; Path=/")
    it "a wrong token sets no cookie" $ do
      r <- run (req "GET" local "/?t=ffffffffffffffffffffffffffffffff" [])
      statusCode (simpleStatus r) `shouldBe` 401
      header "Set-Cookie" r `shouldBe` Nothing

  describe "the cookie" $ do
    it "GET /api/ping without the cookie is 401" $ do
      r <- run (req "GET" local "/api/ping" [])
      statusCode (simpleStatus r) `shouldBe` 401
    it "GET /api/ping with a wrong cookie is 401" $ do
      r <- run (req "GET" local "/api/ping" [(hCookie, "fisher_ui_8732=nope")])
      statusCode (simpleStatus r) `shouldBe` 401
    it "takes only the cookie of this server's port" $ do
      r1 <- run (req "GET" local "/api/ping" [(hCookie, "fisher_ui_8733=" <> TE.encodeUtf8 token)])
      statusCode (simpleStatus r1) `shouldBe` 401
      r2 <- run (req "GET" local "/api/ping" [(hCookie, "fisher_ui=" <> TE.encodeUtf8 token)])
      statusCode (simpleStatus r2) `shouldBe` 401
      r3 <- run (req "GET" local "/api/ping" [(hCookie, "fisher_ui_8733=ffff; fisher_ui_8732=" <> TE.encodeUtf8 token)])
      statusCode (simpleStatus r3) `shouldBe` 200
    it "GET /api/ping with the cookie is 200 {\"ok\":true}" $ do
      r <- run (req "GET" local "/api/ping" [cookie])
      statusCode (simpleStatus r) `shouldBe` 200
      simpleBody r `shouldBe` "{\"ok\":true}"
      header hContentType r `shouldBe` Just "application/json"
    it "an unknown /api route is 404" $ do
      r <- run (req "GET" local "/api/nothing" [cookie])
      statusCode (simpleStatus r) `shouldBe` 404

  describe "Host" $ do
    it "Host evil.example:8732 is 403 on every path" $
      forM_ ["/", "/?t=" <> TE.encodeUtf8 token, "/api/ping", "/assets/x.js", "/sessions/1"] $ \p -> do
        r <- run (req "GET" "evil.example:8732" p [cookie])
        statusCode (simpleStatus r) `shouldBe` 403
    it "a missing Host is 403" $ do
      r <- runSession (request (setPath defaultRequest "/api/ping") {requestHeaders = [cookie]}) app
      statusCode (simpleStatus r) `shouldBe` 403
    it "the wrong port is 403" $ do
      r <- run (req "GET" "127.0.0.1:9999" "/api/ping" [cookie])
      statusCode (simpleStatus r) `shouldBe` 403
    it "localhost:8732 and 127.0.0.1:8732 are allowed" $
      forM_ ["localhost:8732", "127.0.0.1:8732"] $ \h -> do
        r <- run (req "GET" h "/api/ping" [cookie])
        statusCode (simpleStatus r) `shouldBe` 200

  describe "Origin" $ do
    it "POST /api/ping with Origin http://evil.example is 403" $ do
      r <- run (req "POST" local "/api/ping" [cookie, ("Origin", "http://evil.example")])
      statusCode (simpleStatus r) `shouldBe` 403
    it "POST /api/ping with no Origin is 403" $ do
      r <- run (req "POST" local "/api/ping" [cookie])
      statusCode (simpleStatus r) `shouldBe` 403
    it "POST /api/ping with Origin http://127.0.0.1:8732 is 200" $ do
      r <- run (req "POST" local "/api/ping" [cookie, ("Origin", "http://127.0.0.1:8732")])
      statusCode (simpleStatus r) `shouldBe` 200
    it "the Origin must match the Host" $ do
      r <- run (req "POST" local "/api/ping" [cookie, ("Origin", "http://localhost:8732")])
      statusCode (simpleStatus r) `shouldBe` 403

  describe "guardRequest" $ do
    it "lets a plain page through without the cookie" $
      guardRequest gate (req "GET" local "/" []) `shouldBe` Right ()
    it "names the refusal" $ do
      guardRequest gate (req "GET" local "/api/ping" []) `shouldBe` Left unauthorized401
      guardRequest gate (req "GET" "evil.example:8732" "/" []) `shouldBe` Left forbidden403

  describe "no CORS" $
    it "no response carries Access-Control-Allow-Origin" $ do
      let reqs =
            [ req "GET" local "/api/ping" [cookie, ("Origin", "http://evil.example")]
            , req "GET" local "/api/ping" []
            , req "OPTIONS" local "/api/ping" [("Origin", "http://evil.example"), ("Access-Control-Request-Method", "POST")]
            , req "POST" local "/api/ping" [cookie, ("Origin", "http://127.0.0.1:8732")]
            , req "GET" local "/" []
            , req "GET" "evil.example:8732" "/" []
            ]
      forM_ reqs $ \q -> do
        r <- run q
        header "Access-Control-Allow-Origin" r `shouldBe` Nothing

  describe "static files" $ do
    let asset ext = find (\(p, _) -> "assets/" `isPrefixOf` p && ext `isSuffixOf` p) staticFiles
    it "embeds ui/dist with index.html and assets" $ do
      lookup "index.html" staticFiles `shouldSatisfy` maybe False ("<div id=\"root\">" `B.isInfixOf`)
      asset ".js" `shouldSatisfy` (/= Nothing)
      asset ".css" `shouldSatisfy` (/= Nothing)
    it "GET /assets/<js> serves it as JavaScript" $ do
      Just (p, body) <- pure (asset ".js")
      r <- run (req "GET" local (BC.pack ('/' : p)) [])
      statusCode (simpleStatus r) `shouldBe` 200
      header hContentType r `shouldSatisfy` maybe False ("javascript" `B.isInfixOf`)
      BL.toStrict (simpleBody r) `shouldBe` body
    it "GET /assets/<css> serves it as CSS" $ do
      Just (p, _) <- pure (asset ".css")
      r <- run (req "GET" local (BC.pack ('/' : p)) [])
      header hContentType r `shouldSatisfy` maybe False ("text/css" `B.isPrefixOf`)
    it "a missing asset is 404" $ do
      r <- run (req "GET" local "/assets/missing-0000.js" [])
      statusCode (simpleStatus r) `shouldBe` 404
    it "an unknown non-api path serves index.html" $
      forM_ ["/", "/sessions/abc", "/runs/h?arm=graph"] $ \p -> do
        r <- run (req "GET" local p [])
        statusCode (simpleStatus r) `shouldBe` 200
        header hContentType r `shouldBe` Just "text/html; charset=utf-8"
        Just (BL.toStrict (simpleBody r)) `shouldBe` lookup "index.html" staticFiles
    it "a path out of ui/dist never returns that file" $ do
      pkg <- B.readFile (uiDir </> ".." </> "fisher-ui" </> "package.yaml")
      forM_ ["/assets/../../fisher-ui/package.yaml", "/assets/..%2f..%2fpackage.yaml", "/assets/..%2f..%2ffisher-ui%2fpackage.yaml", "/../package.yaml"] $ \p -> do
        r <- run (req "GET" local p [])
        BL.toStrict (simpleBody r) `shouldNotBe` pkg
        "fisher-ui" `B.isInfixOf` BL.toStrict (simpleBody r) `shouldBe` False
        when ("/assets/" `B.isPrefixOf` p) $ statusCode (simpleStatus r) `shouldBe` 404
    it "pages are not framed and send no referrer" $ do
      r <- run (req "GET" local "/" [])
      header "X-Frame-Options" r `shouldBe` Just "DENY"
      header "Referrer-Policy" r `shouldBe` Just "no-referrer"

  describe "newToken" $
    it "gives 32 hex characters and two calls differ" $ do
      a <- newToken
      b <- newToken
      T.length a `shouldBe` 32
      T.all isHexDigit a `shouldBe` True
      a `shouldNotBe` b

  describe "the listening socket" $
    it "is bound to 127.0.0.1" $
      bracket (bindUi 0) (close . snd) $ \(port, sock) -> do
        addr <- getSocketName sock
        addr `shouldBe` SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1))

  describe "fisher ui arguments" $ do
    it "defaults to port 8732 and opening the page" $
      parseUiArgs "/h" [] `shouldBe` Right (UiConfig 8732 True "/h")
    it "reads --port N, --port=N and --no-open" $ do
      parseUiArgs "/h" ["--port", "9000", "--no-open"] `shouldBe` Right (UiConfig 9000 False "/h")
      parseUiArgs "/h" ["--port=0"] `shouldBe` Right (UiConfig 0 True "/h")
    it "refuses a bad port or an unknown flag" $ do
      parseUiArgs "/h" ["--port", "x"] `shouldSatisfy` either (const True) (const False)
      parseUiArgs "/h" ["--port", "70000"] `shouldSatisfy` either (const True) (const False)
      parseUiArgs "/h" ["--bind", "0.0.0.0"] `shouldSatisfy` either (const True) (const False)
    it "prints the URL with the token" $
      uiUrl 8732 token `shouldBe` "http://127.0.0.1:8732/?t=" <> token

  describe "ui/dist is fresh" $
    it "no file under ui/src is newer than ui/dist/index.html" $ do
      built <- getModificationTime (uiDir </> "dist" </> "index.html")
      srcs <- walk (uiDir </> "src")
      srcs `shouldSatisfy` (not . null)
      stale <- filterM (\f -> (\t -> diffUTCTime t built > slack) <$> getModificationTime f) srcs
      stale `shouldBe` []
  where
    -- a fresh git checkout writes ui/src just after ui/dist
    slack = 2 :: NominalDiffTime
    walk d = do
      xs <- map (d </>) <$> listDirectory d
      concat <$> mapM (\x -> doesDirectoryExist x >>= \isD -> if isD then walk x else pure [x]) xs
