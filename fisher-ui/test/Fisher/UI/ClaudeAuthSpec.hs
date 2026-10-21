-- | The Settings page's Claude Code routes through the whole server, with a
-- fake @claude@ that records its arguments, answers @auth status@ from a
-- file, and in @auth login@ prints a URL (in ANSI bold) and a prompt, then
-- reads a code from stdin and logs in.
module Fisher.UI.ClaudeAuthSpec (spec) where

import Control.Concurrent (threadDelay)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Network.HTTP.Types
import Network.Wai (Application, Request (..))
import Network.Wai.Test
import System.Directory
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.UI.Access (Gate (..))
import Fisher.UI.Api.ClaudeAuth
import Fisher.UI.Server (serve)

gate :: Gate
gate = Gate "0123456789abcdef0123456789abcdef" 8732

okHeaders :: [Header]
okHeaders =
  [ ("Host", "127.0.0.1:8732"), (hCookie, "fisher_ui_8732=0123456789abcdef0123456789abcdef")
  , ("Origin", "http://127.0.0.1:8732") ]

callWith :: [Header] -> Application -> Method -> B.ByteString -> BL.ByteString -> IO SResponse
callWith hs app m p body = runSession (srequest (SRequest req body)) app
  where
    req = setPath defaultRequest {requestMethod = m, requestHeaderHost = Just "127.0.0.1:8732", requestHeaders = hs} p

getJ :: Application -> B.ByteString -> IO (Int, A.Value)
getJ app p = do
  r <- callWith okHeaders app methodGet p ""
  pure (statusCode (simpleStatus r), maybe A.Null id (A.decode (simpleBody r)))

postJ :: Application -> B.ByteString -> A.Value -> IO (Int, A.Value)
postJ app p v = do
  r <- callWith ((hContentType, "application/json") : okHeaders) app methodPost p (A.encode v)
  pure (statusCode (simpleStatus r), maybe A.Null id (A.decode (simpleBody r)))

key :: Text -> A.Value -> A.Value
key k = \case
  A.Object o -> maybe A.Null id (KM.lookup (K.fromText k) o)
  _ -> A.Null

arr :: A.Value -> [A.Value]
arr = \case
  A.Array xs -> V.toList xs
  _ -> []

txt :: A.Value -> Text
txt = \case
  A.String t -> t
  _ -> ""

loggedInJson :: String
loggedInJson = "{\"loggedIn\": true, \"authMethod\": \"claude.ai\", \"apiProvider\": \"firstParty\", \"email\": \"x@y.z\", \"orgId\": \"org-secret-id\", \"orgName\": \"Org Name\", \"subscriptionType\": \"max\"}"

-- | The fake: owner rwx and kept writable (the tests may run as root).
fakeClaude :: FilePath -> String
fakeClaude dir =
  unlines
    [ "#!/bin/sh"
    , "echo \"$*\" >> " <> dir </> "argv"
    , "case \"$1 $2\" in"
    , "  'auth status') cat " <> dir </> "status.json" <> "; exit $(cat " <> dir </> "code" <> ") ;;"
    , "  'auth logout') echo '{\"loggedIn\": false}' > " <> dir </> "status.json" <> "; echo 1 > " <> dir </> "code" <> "; exit 0 ;;"
    , "  'auth login')"
    , "    if [ -f " <> dir </> "noread" <> " ]; then sleep 30; exit 1; fi"
    , "    if [ -f " <> dir </> "flood" <> " ]; then i=0; while [ $i -lt 12000 ]; do echo " <> replicate 100 'x' <> "; i=$((i+1)); done; fi"
    , "    printf 'Opening your browser\\n\\033[1mhttps://claude.ai/oauth/authorize?code=true&state=abc\\033[0m\\r\\n'"
    , "    printf 'Waiting...\\rPaste code here if prompted > '"
    , "    if read c; then"
    , "      echo \"$c\" > " <> dir </> "got"
    , "      echo '" <> loggedInJson <> "' > " <> dir </> "status.json"
    , "      echo 0 > " <> dir </> "code"
    , "      echo 'Login successful.'"
    , "      exit 0"
    , "    fi"
    , "    exit 1 ;;"
    , "esac" ]

data World = World { wDir :: FilePath, wApp :: Application }

withWorld :: Bool -> (World -> IO a) -> IO a
withWorld installed k = withSystemTempDirectory "fisher-ui-claude" $ \tmp0 -> do
  tmp <- canonicalizePath tmp0
  let exe = tmp </> "bin" </> "claude"
  createDirectoryIfMissing True (tmp </> "bin")
  createDirectoryIfMissing True (tmp </> "home")
  writeFile (tmp </> "status.json") "{\"loggedIn\": false}"
  writeFile (tmp </> "code") "1"
  writeFile exe (fakeClaude tmp)
  getPermissions exe >>= setPermissions exe . setOwnerExecutable True . setOwnerReadable True . setOwnerWritable True
  env0 <- newClaudeAuthEnv (tmp </> "home")
  let env = env0 {caClaude = pure (if installed then Just exe else Nothing), caGrace = 1000000}
  r <- k (World tmp (serve gate [claudeAuthApi env]))
  stopClaudeLogins env
  pure r

argvOf :: World -> IO [String]
argvOf w = doesFileExist (wDir w </> "argv") >>= \case
  True -> lines <$> readFile (wDir w </> "argv")
  False -> pure []

-- | Poll the login's output until it satisfies the test (up to 10 s).
outputUntil :: World -> Text -> (A.Value -> Bool) -> IO A.Value
outputUntil w tag ok = go (200 :: Int)
  where
    go n = do
      (_, v) <- getJ (wApp w) ("/api/claude-auth/login/" <> TE.encodeUtf8 tag)
      if ok v
        then pure v
        else if n > 0 then threadDelay 50000 >> go (n - 1) else fail ("the login never got there: " <> show v)

spec :: Spec
spec = describe "Fisher.UI.Api.ClaudeAuth" $ do
  describe "GET /api/claude-auth" $ do
    it "gives the status without the organisation" $ withWorld True $ \w -> do
      writeFile (wDir w </> "status.json") loggedInJson
      writeFile (wDir w </> "code") "0"
      r <- callWith okHeaders (wApp w) methodGet "/api/claude-auth" ""
      statusCode (simpleStatus r) `shouldBe` 200
      let v = maybe A.Null id (A.decode (simpleBody r))
      map (`key` v) ["installed", "loggedIn", "authMethod", "email", "subscriptionType", "login"]
        `shouldBe` [A.Bool True, A.Bool True, A.String "claude.ai", A.String "x@y.z", A.String "max", A.Null]
      BL.toStrict (simpleBody r) `shouldNotSatisfy` B.isInfixOf "org-secret-id"
      BL.toStrict (simpleBody r) `shouldNotSatisfy` B.isInfixOf "Org Name"
      argvOf w `shouldReturn` ["auth status --json"]
    it "says logged out on a non-zero exit" $ withWorld True $ \w -> do
      (_, v) <- getJ (wApp w) "/api/claude-auth"
      (key "installed" v, key "loggedIn" v) `shouldBe` (A.Bool True, A.Bool False)
    it "says not installed when there is no claude" $ withWorld False $ \w -> do
      (_, v) <- getJ (wApp w) "/api/claude-auth"
      (key "installed" v, key "loggedIn" v) `shouldBe` (A.Bool False, A.Bool False)

  describe "POST /api/claude-auth/login" $ do
    it "runs claude auth login with the options, shows its output, takes a pasted code and logs in" $ withWorld True $ \w -> do
      (code, v) <- postJ (wApp w) "/api/claude-auth/login" (A.object ["console" A..= True, "sso" A..= True, "email" A..= ("a@b.c" :: Text)])
      code `shouldBe` 200
      let tag = txt (key "tag" v)
      o <- outputUntil w tag (\x -> "Paste code" `T.isInfixOf` txt (key "partial" x))
      map txt (arr (key "lines" o)) `shouldBe` ["Opening your browser", "https://claude.ai/oauth/authorize?code=true&state=abc"]
      key "running" o `shouldBe` A.Bool True
      (_, st) <- getJ (wApp w) "/api/claude-auth"
      key "login" st `shouldBe` A.String tag
      (c2, _) <- postJ (wApp w) ("/api/claude-auth/login/" <> TE.encodeUtf8 tag <> "/input") (A.object ["text" A..= ("code-123#state" :: Text)])
      c2 `shouldBe` 200
      done <- outputUntil w tag (\x -> key "running" x == A.Bool False)
      key "exitCode" done `shouldBe` A.Number 0
      map txt (arr (key "lines" done)) `shouldSatisfy` elem "Paste code here if prompted > Login successful."
      readFile (wDir w </> "got") `shouldReturn` "code-123#state\n"
      -- from: only the lines after it
      (_, later) <- getJ (wApp w) ("/api/claude-auth/login/" <> TE.encodeUtf8 tag <> "?from=2")
      map txt (arr (key "lines" later)) `shouldBe` ["Paste code here if prompted > Login successful."]
      key "next" later `shouldBe` A.Number 3
      (_, st2) <- getJ (wApp w) "/api/claude-auth"
      (key "loggedIn" st2, key "email" st2, key "login" st2) `shouldBe` (A.Bool True, A.String "x@y.z", A.Null)
      argv <- argvOf w
      filter (T.isPrefixOf "auth login" . T.pack) argv `shouldBe` ["auth login --console --sso --email a@b.c"]
    it "passes no options for a subscription login" $ withWorld True $ \w -> do
      (_, v) <- postJ (wApp w) "/api/claude-auth/login" (A.object [])
      _ <- outputUntil w (txt (key "tag" v)) (\x -> "Paste code" `T.isInfixOf` txt (key "partial" x))
      argvOf w `shouldReturn` ["auth login"]
    it "allows one login at a time" $ withWorld True $ \w -> do
      (_, v) <- postJ (wApp w) "/api/claude-auth/login" (A.object [])
      (code, v2) <- postJ (wApp w) "/api/claude-auth/login" (A.object [])
      code `shouldBe` 409
      key "tag" v2 `shouldBe` key "tag" v
    it "refuses a bad email or option, and runs nothing" $ withWorld True $ \w -> do
      let bads =
            [ A.object ["email" A..= ("a b@c" :: Text)], A.object ["email" A..= ("a@b\n--console" :: Text)]
            , A.object ["email" A..= ("-x@y" :: Text)], A.object ["email" A..= (T.replicate 255 "a" :: Text)]
            , A.object ["email" A..= (3 :: Int)], A.object ["console" A..= ("yes" :: Text)] ]
      mapM_ (\b -> fst <$> postJ (wApp w) "/api/claude-auth/login" b >>= (`shouldBe` 400)) bads
      argvOf w `shouldReturn` []
    it "answers 409 with how to install when there is no claude" $ withWorld False $ \w -> do
      (code, v) <- postJ (wApp w) "/api/claude-auth/login" (A.object [])
      code `shouldBe` 409
      txt (key "error" v) `shouldSatisfy` T.isInfixOf "npm install -g @anthropic-ai/claude-code"

  describe "a login's input and stop" $ do
    it "takes one line only" $ withWorld True $ \w -> do
      (_, v) <- postJ (wApp w) "/api/claude-auth/login" (A.object [])
      let tag = TE.encodeUtf8 (txt (key "tag" v))
      (code, _) <- postJ (wApp w) ("/api/claude-auth/login/" <> tag <> "/input") (A.object ["text" A..= ("a\nb" :: Text)])
      code `shouldBe` 400
    it "stops the child, and refuses input after" $ withWorld True $ \w -> do
      (_, v) <- postJ (wApp w) "/api/claude-auth/login" (A.object [])
      let tag = txt (key "tag" v)
          path = "/api/claude-auth/login/" <> TE.encodeUtf8 tag
      _ <- outputUntil w tag (\x -> "Paste code" `T.isInfixOf` txt (key "partial" x))
      (code, _) <- postJ (wApp w) (path <> "/stop") (A.object [])
      code `shouldBe` 202
      done <- outputUntil w tag (\x -> key "running" x == A.Bool False)
      key "stopping" done `shouldBe` A.Bool True
      (c2, _) <- postJ (wApp w) (path <> "/input") (A.object ["text" A..= ("late" :: Text)])
      c2 `shouldBe` 409
      (c3, _) <- postJ (wApp w) (path <> "/stop") (A.object [])
      c3 `shouldBe` 409
      doesFileExist (wDir w </> "got") `shouldReturn` False
    it "is 404 for a tag it does not know" $ withWorld True $ \w -> do
      (code, _) <- getJ (wApp w) "/api/claude-auth/login/nope"
      code `shouldBe` 404

  describe "POST /api/claude-auth/logout" $
    it "runs claude auth logout and gives the status after" $ withWorld True $ \w -> do
      writeFile (wDir w </> "status.json") loggedInJson
      writeFile (wDir w </> "code") "0"
      (code, v) <- postJ (wApp w) "/api/claude-auth/logout" (A.object [])
      code `shouldBe` 200
      key "loggedIn" v `shouldBe` A.Bool False
      argvOf w `shouldReturn` ["auth logout", "auth status --json"]

  describe "access" $
    it "refuses a request without the cookie, or a POST from another origin, before running anything" $ withWorld True $ \w -> do
      r1 <- callWith (filter ((/= hCookie) . fst) okHeaders) (wApp w) methodGet "/api/claude-auth" ""
      statusCode (simpleStatus r1) `shouldBe` 401
      r2 <- callWith (("Origin", "http://evil.example") : filter ((/= "Origin") . fst) okHeaders) (wApp w) methodPost "/api/claude-auth/login" "{}"
      statusCode (simpleStatus r2) `shouldBe` 403
      r3 <- callWith (filter ((/= "Origin") . fst) okHeaders) (wApp w) methodPost "/api/claude-auth/logout" "{}"
      statusCode (simpleStatus r3) `shouldBe` 403
      argvOf w `shouldReturn` []

  describe "long output and odd input" $ do
    it "keeps 1 MB of output and says it was truncated" $ withWorld True $ \w -> do
      writeFile (wDir w </> "flood") ""
      (_, v) <- postJ (wApp w) "/api/claude-auth/login" (A.object [])
      o <- outputUntil w (txt (key "tag" v)) (\x -> "Paste code" `T.isInfixOf` txt (key "partial" x))
      key "truncated" o `shouldBe` A.Bool True
      sum (map (T.length . txt) (arr (key "lines" o))) `shouldSatisfy` (<= 1048576)
      length (arr (key "lines" o)) `shouldSatisfy` (> 9000)
    it "sends a pasted code as UTF-8" $ withWorld True $ \w -> do
      (_, v) <- postJ (wApp w) "/api/claude-auth/login" (A.object [])
      let tag = txt (key "tag" v)
      _ <- outputUntil w tag (\x -> "Paste code" `T.isInfixOf` txt (key "partial" x))
      (code, _) <- postJ (wApp w) ("/api/claude-auth/login/" <> TE.encodeUtf8 tag <> "/input") (A.object ["text" A..= ("c\243de-\252" :: Text)])
      code `shouldBe` 200
      _ <- outputUntil w tag (\x -> key "running" x == A.Bool False)
      B.readFile (wDir w </> "got") `shouldReturn` (TE.encodeUtf8 "c\243de-\252" <> "\n")
    it "does not hang on a child that never reads: the write gives up and Stop still stops it" $ withWorld True $ \w -> do
      writeFile (wDir w </> "noread") ""
      (_, v) <- postJ (wApp w) "/api/claude-auth/login" (A.object [])
      let tag = txt (key "tag" v)
          path = "/api/claude-auth/login/" <> TE.encodeUtf8 tag
          big = T.replicate 4096 "a"
          fill n = do
            (code, _) <- postJ (wApp w) (path <> "/input") (A.object ["text" A..= big])
            if code == 503 || n <= (0 :: Int) then pure code else fill (n - 1)
      fill 60 `shouldReturn` 503
      (c2, _) <- postJ (wApp w) (path <> "/stop") (A.object [])
      c2 `shouldBe` 202
      done <- outputUntil w tag (\x -> key "running" x == A.Bool False)
      key "stopping" done `shouldBe` A.Bool True

  describe "clean" $
    it "keeps the text after the last carriage return, a final one being the newline's" $ do
      clean "spin 1\rspin 2\rPaste > " `shouldBe` "Paste > "
      clean "done\r" `shouldBe` "done"
      clean "\ESC[2Kone\r\ESC[2Ktwo" `shouldBe` "two"

  describe "stripAnsi" $
    it "drops colours, titles and hyperlinks' escapes" $ do
      stripAnsi "\ESC[1;32mok\ESC[0m" `shouldBe` "ok"
      stripAnsi "\ESC]0;title\aafter" `shouldBe` "after"
      stripAnsi "\ESC]8;;https://x\ESC\\link\ESC]8;;\ESC\\" `shouldBe` "link"
