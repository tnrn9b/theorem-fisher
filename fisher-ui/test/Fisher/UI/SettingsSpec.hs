-- | @GET@ and @POST /api/settings@ over a scratch GAUSS_HOME: config.yaml
-- through Fisher's config module, credentials in .env shown only as their
-- last four characters, and never in the server's output.
module Fisher.UI.SettingsSpec (spec) where

import Control.Exception (bracket, finally)
import Control.Monad (forM_)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Yaml as Y
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Network.HTTP.Types
import Network.Wai (Application, Request (..))
import Network.Wai.Test
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath ((</>))
import System.IO (IOMode (..), hClose, hFlush, openFile, stderr, stdout)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (fileMode, getFileStatus, intersectFileModes)
import Test.Hspec

import Fisher.Config (getPath)
import Fisher.UI.Access (Gate (..))
import Fisher.UI.Api.Settings (settingsApi)
import Fisher.UI.Server (serve)

gate :: Gate
gate = Gate "0123456789abcdef0123456789abcdef" 8732

cookieH, originH :: Header
cookieH = (hCookie, "fisher_ui_8732=0123456789abcdef0123456789abcdef")
originH = ("Origin", "http://127.0.0.1:8732")

-- | The key the fixture's .env holds, and one the tests write.
oldKey, newKey :: Text
oldKey = "sk-fixture-old-0000000000009876"
newKey = "sk-fixture-new-1111111111115678"

fixtureEnv :: B.ByteString
fixtureEnv = "# keys for the fixture\nFOO=1\nOPENROUTER_API_KEY=sk-fixture-old-0000000000009876\n\nexport_ME=yes\nBAR=\"two words\"\n"

fixtureConfig :: B.ByteString
fixtureConfig =
  "model:\n  default: example/model-a\n  provider: openrouter\n\
  \fisher:\n  context: clm\n\
  \gauss:\n  autoformalize:\n    model: example/model-b\n"

withHome :: Bool -> (FilePath -> IO a) -> IO a
withHome seeded k = withSystemTempDirectory "fisher-ui-settings" $ \tmp -> do
  let home = tmp </> "gauss"
  createDirectoryIfMissing True home
  if seeded
    then B.writeFile (home </> "config.yaml") fixtureConfig >> B.writeFile (home </> ".env") fixtureEnv
    else pure ()
  k home

app :: FilePath -> Application
app home = serve gate [settingsApi home]

call :: FilePath -> Method -> [Header] -> BL.ByteString -> IO SResponse
call home m hs body = runSession (srequest (SRequest req body)) (app home)
  where
    req =
      setPath
        defaultRequest
          { requestMethod = m
          , requestHeaderHost = Just "127.0.0.1:8732"
          , requestHeaders = hs <> [("Host", "127.0.0.1:8732")] }
        "/api/settings"

getS :: FilePath -> IO (Int, A.Value, BL.ByteString)
getS home = do
  r <- call home methodGet [cookieH] ""
  pure (statusCode (simpleStatus r), maybe A.Null id (A.decode (simpleBody r)), simpleBody r)

postS :: FilePath -> A.Value -> IO (Int, A.Value, BL.ByteString)
postS home v = do
  r <- call home methodPost [cookieH, originH, (hContentType, "application/json")] (A.encode v)
  pure (statusCode (simpleStatus r), maybe A.Null id (A.decode (simpleBody r)), simpleBody r)

key :: Text -> A.Value -> A.Value
key k = \case
  A.Object o -> maybe A.Null id (KM.lookup (K.fromText k) o)
  _ -> A.Null

arr :: A.Value -> [A.Value]
arr = \case
  A.Array xs -> V.toList xs
  _ -> []

keyRow :: Text -> A.Value -> A.Value
keyRow n v = case [x | x <- arr (key "keys" v), key "name" x == A.String n] of
  (x : _) -> x
  [] -> A.Null

contains :: Text -> BL.ByteString -> Bool
contains needle hay = TE.encodeUtf8 needle `B.isInfixOf` BL.toStrict hay

-- | Run an action with this process's stdout and stderr sent to a file, and
-- give back what was written there.
captured :: FilePath -> IO a -> IO (a, B.ByteString)
captured file act = do
  hFlush stdout >> hFlush stderr
  out0 <- hDuplicate stdout
  err0 <- hDuplicate stderr
  a <- bracket (openFile file WriteMode) hClose $ \h -> do
    hDuplicateTo h stdout
    hDuplicateTo h stderr
    (act <* (hFlush stdout >> hFlush stderr))
      `finally` (hDuplicateTo out0 stdout >> hDuplicateTo err0 stderr)
  bytes <- B.readFile file
  pure (a, bytes)

configAt :: FilePath -> [Text] -> IO (Maybe A.Value)
configAt home p = getPath p <$> (Y.decodeFileThrow (home </> "config.yaml") :: IO A.Value)

spec :: Spec
spec = describe "/api/settings" $ do
  it "reads the settings from the home's config.yaml and .env" $ withHome True $ \home -> do
    (code, v, _) <- getS home
    code `shouldBe` 200
    key "provider" v `shouldBe` A.String "openrouter"
    key "model" v `shouldBe` A.String "example/model-a"
    key "autoformalizeModel" v `shouldBe` A.String "example/model-b"
    key "contextStrategy" v `shouldBe` A.String "clm"
    -- the pre-pass is fisher search's flag defaults
    key "seconds" (key "auto" v) `shouldBe` A.Number 60
    key "repair" (key "auto" v) `shouldBe` A.Bool True
    key "disprove" (key "auto" v) `shouldBe` A.Bool True
    key "tactics" (key "auto" v) `shouldSatisfy` \t -> A.String "omega" `elem` arr t
    key "settable" (key "auto" v) `shouldBe` A.Bool False

  it "gives the defaults on a home with no config.yaml" $ withHome False $ \home -> do
    (code, v, _) <- getS home
    code `shouldBe` 200
    key "provider" v `shouldBe` A.String "auto"
    key "contextStrategy" v `shouldBe` A.String "compress"
    key "autoformalizeModel" v `shouldBe` A.String ""
    key "model" v `shouldSatisfy` \case
      A.String t -> not (T.null t)
      _ -> False
    key "last4" (keyRow "OPENROUTER_API_KEY" v) `shouldBe` A.Null
    key "set" (keyRow "OPENROUTER_API_KEY" v) `shouldBe` A.Bool False

  it "returns a key only as its last 4 characters" $ withHome True $ \home -> do
    (_, v, body) <- getS home
    key "last4" (keyRow "OPENROUTER_API_KEY" v) `shouldBe` A.String "9876"
    key "set" (keyRow "OPENROUTER_API_KEY" v) `shouldBe` A.Bool True
    body `shouldNotSatisfy` contains oldKey
    body `shouldNotSatisfy` contains (T.dropEnd 4 oldKey)
    body `shouldNotSatisfy` contains "sk-fixture"
    -- a variable that is not a credential is not listed
    keyRow "FOO" v `shouldBe` A.Null

  it "writes a key in place, keeping every other .env line and its order (temp then rename, mode 600)" $ withHome True $ \home -> do
    (code, v, body) <- postS home (A.object ["keys" A..= [A.object ["name" A..= ("OPENROUTER_API_KEY" :: Text), "value" A..= newKey]]])
    code `shouldBe` 200
    body `shouldNotSatisfy` contains newKey
    key "last4" (keyRow "OPENROUTER_API_KEY" v) `shouldBe` A.String "5678"
    B.readFile (home </> ".env")
      `shouldReturn` "# keys for the fixture\nFOO=1\nOPENROUTER_API_KEY=sk-fixture-new-1111111111115678\n\nexport_ME=yes\nBAR=\"two words\"\n"
    st <- getFileStatus (home </> ".env")
    (fileMode st `intersectFileModes` 0o777) `shouldBe` 0o600
    -- no temporary file is left beside it
    left <- listDirectory home
    filter (\f -> ".env_" `isPrefixOf` f || ".config_" `isPrefixOf` f) left `shouldBe` []

  it "adds a new key at the end" $ withHome True $ \home -> do
    (code, _, _) <- postS home (A.object ["keys" A..= [A.object ["name" A..= ("OPENAI_API_KEY" :: Text), "value" A..= newKey]]])
    code `shouldBe` 200
    B.readFile (home </> ".env") `shouldReturn` (fixtureEnv <> "OPENAI_API_KEY=sk-fixture-new-1111111111115678\n")

  it "writes the model, provider, autoformalize model and strategy through config.yaml" $ withHome True $ \home -> do
    (code, v, _) <- postS home $
      A.object
        [ "model" A..= ("example/model-c" :: Text), "provider" A..= ("anthropic" :: Text)
        , "autoformalizeModel" A..= ("" :: Text), "contextStrategy" A..= ("rlm" :: Text) ]
    code `shouldBe` 200
    key "model" v `shouldBe` A.String "example/model-c"
    configAt home ["model", "default"] `shouldReturn` Just (A.String "example/model-c")
    configAt home ["model", "provider"] `shouldReturn` Just (A.String "anthropic")
    configAt home ["fisher", "context"] `shouldReturn` Just (A.String "rlm")
    configAt home ["gauss", "autoformalize", "model"] `shouldReturn` Just (A.String "")
    (_, v2, _) <- getS home
    key "provider" v2 `shouldBe` A.String "anthropic"
    key "contextStrategy" v2 `shouldBe` A.String "rlm"
    -- .env untouched
    B.readFile (home </> ".env") `shouldReturn` fixtureEnv

  it "keeps a model given as a plain string when only the provider changes" $ withHome False $ \home -> do
    B.writeFile (home </> "config.yaml") "model: example/model-a\n"
    (code, v, _) <- postS home (A.object ["provider" A..= ("openrouter" :: Text)])
    code `shouldBe` 200
    key "model" v `shouldBe` A.String "example/model-a"
    key "provider" v `shouldBe` A.String "openrouter"

  it "refuses a bad strategy, a bad key name and an empty model, and writes nothing" $ withHome True $ \home -> do
    (c1, _, _) <- postS home (A.object ["model" A..= ("example/model-c" :: Text), "contextStrategy" A..= ("magic" :: Text)])
    c1 `shouldBe` 400
    (c2, _, b2) <- postS home (A.object ["keys" A..= [A.object ["name" A..= ("FOO" :: Text), "value" A..= newKey]]])
    c2 `shouldBe` 400
    b2 `shouldNotSatisfy` contains newKey
    (c3, _, _) <- postS home (A.object ["model" A..= ("  " :: Text)])
    c3 `shouldBe` 400
    B.readFile (home </> ".env") `shouldReturn` fixtureEnv
    B.readFile (home </> "config.yaml") `shouldReturn` fixtureConfig

  it "refuses a POST without the cookie (401) or from another origin (403)" $ withHome True $ \home -> do
    let body = A.encode (A.object ["keys" A..= [A.object ["name" A..= ("OPENROUTER_API_KEY" :: Text), "value" A..= newKey]], "model" A..= ("x/y" :: Text)])
    r1 <- call home methodPost [originH, (hContentType, "application/json")] body
    statusCode (simpleStatus r1) `shouldBe` 401
    r2 <- call home methodPost [cookieH, ("Origin", "http://evil.example"), (hContentType, "application/json")] body
    statusCode (simpleStatus r2) `shouldBe` 403
    r3 <- call home methodPost [cookieH, (hContentType, "application/json")] body
    statusCode (simpleStatus r3) `shouldBe` 403
    r4 <- call home methodGet [] ""
    statusCode (simpleStatus r4) `shouldBe` 401
    B.readFile (home </> ".env") `shouldReturn` fixtureEnv
    B.readFile (home </> "config.yaml") `shouldReturn` fixtureConfig

  it "never writes a key value to the server's output" $ withHome True $ \home -> do
    let logFile = home </> ".." </> "server.log"
    (_, out) <- captured logFile $ do
      _ <- getS home
      _ <- postS home (A.object ["keys" A..= [A.object ["name" A..= ("OPENROUTER_API_KEY" :: Text), "value" A..= newKey]]])
      -- refused ones too: a bad name, a value with a newline
      _ <- postS home (A.object ["keys" A..= [A.object ["name" A..= ("not a name" :: Text), "value" A..= newKey]]])
      _ <- postS home (A.object ["keys" A..= [A.object ["name" A..= ("OPENAI_API_KEY" :: Text), "value" A..= (newKey <> "\nX=1")]]])
      getS home
    out `shouldNotSatisfy` (contains newKey . BL.fromStrict)
    out `shouldNotSatisfy` (contains oldKey . BL.fromStrict)
    out `shouldNotSatisfy` (contains "sk-fixture" . BL.fromStrict)

  it "refuses a key value with a line break (400) and leaves .env as it was" $ withHome True $ \home -> do
    forM_ [newKey <> "\nOPENAI_API_KEY=stolen", newKey <> "\rX=1", "a\0b-long-enough"] $ \bad -> do
      (code, _, body) <- postS home (A.object ["keys" A..= [A.object ["name" A..= ("OPENROUTER_API_KEY" :: Text), "value" A..= bad]]])
      code `shouldBe` 400
      body `shouldNotSatisfy` contains newKey
    B.readFile (home </> ".env") `shouldReturn` fixtureEnv

  it "answers 409 when config.yaml does not hold a mapping, and changes nothing" $ withHome True $ \home -> do
    B.writeFile (home </> "config.yaml") "- a\n- b\n"
    (code, v, _) <- postS home (A.object ["model" A..= ("example/model-c" :: Text)])
    code `shouldBe` 409
    key "error" v `shouldSatisfy` \case
      A.String t -> "mapping" `T.isInfixOf` t
      _ -> False
    B.readFile (home </> "config.yaml") `shouldReturn` "- a\n- b\n"
    B.readFile (home </> ".env") `shouldReturn` fixtureEnv

  it "refuses a model that YAML would read as a number or a boolean" $ withHome True $ \home -> do
    forM_ ["42", "3.5", "true", "off"] $ \m -> do
      (code, v, _) <- postS home (A.object ["model" A..= (m :: Text)])
      code `shouldBe` 400
      key "error" v `shouldSatisfy` \case
        A.String t -> "number or a boolean" `T.isInfixOf` t
        _ -> False
    B.readFile (home </> "config.yaml") `shouldReturn` fixtureConfig

  it "shows the .env names that hold no secret as plain fields with their values, and writes them" $ withHome True $ \home -> do
    B.appendFile (home </> ".env") "TERMINAL_SSH_HOST=box.local\nFIRECRAWL_API_URL=http://127.0.0.1:3002\nTERMINAL_SSH_KEY=/Users/me/.ssh/id_box\n"
    (_, v, _) <- getS home
    let plainOf n = [key "value" x | x <- arr (key "plain" v), key "name" x == A.String n]
    plainOf "TERMINAL_SSH_HOST" `shouldBe` [A.String "box.local"]
    plainOf "FIRECRAWL_API_URL" `shouldBe` [A.String "http://127.0.0.1:3002"]
    plainOf "TERMINAL_SSH_USER" `shouldBe` [A.String ""]
    keyRow "TERMINAL_SSH_HOST" v `shouldBe` A.Null
    -- a key file's path is kept as a key
    key "set" (keyRow "TERMINAL_SSH_KEY" v) `shouldBe` A.Bool True
    plainOf "TERMINAL_SSH_KEY" `shouldBe` []
    (code, v2, _) <- postS home (A.object ["keys" A..= [A.object ["name" A..= ("TERMINAL_SSH_USER" :: Text), "value" A..= ("tom" :: Text)]]])
    code `shouldBe` 200
    [key "value" x | x <- arr (key "plain" v2), key "name" x == A.String "TERMINAL_SSH_USER"] `shouldBe` [A.String "tom"]

  it "keeps a name with PASS, PASSWD or PASSPHRASE in it a key, never a plain field" $ withHome True $ \home -> do
    B.appendFile (home </> ".env") "TERMINAL_SSH_PASS=hunter2-long-value\nTERMINAL_SSH_PASSWD=hunter3-long-value\nTERMINAL_SSH_PASSPHRASE=hunter4-long-value\n"
    (_, v, body) <- getS home
    forM_ ["TERMINAL_SSH_PASS", "TERMINAL_SSH_PASSWD", "TERMINAL_SSH_PASSPHRASE"] $ \n -> do
      key "set" (keyRow n v) `shouldBe` A.Bool True
      [x | x <- arr (key "plain" v), key "name" x == A.String n] `shouldBe` []
    body `shouldNotSatisfy` contains "hunter"
