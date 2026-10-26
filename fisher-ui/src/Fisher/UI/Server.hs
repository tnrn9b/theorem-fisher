-- | @fisher ui@: a Warp server on 127.0.0.1 that serves JSON routes under
-- @/api@ and the embedded React bundle for every other path.
module Fisher.UI.Server
  ( UiConfig (..)
  , runUi
  , uiMain
  , parseUiArgs
  , uiUrl
  , bindUi
  , uiApis
  , liveApis
  , allApis
  , Terminated (..)
  , onTerminate
  , Api
  , serve
  , pingApi
  , jsonResponse
  , findRepo
  ) where

import Control.Concurrent (myThreadId, throwTo)
import Control.Exception (Exception, IOException, SomeException, bracket, catch, try)
import Control.Monad (when)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Streaming.Network (bindPortTCP, bindRandomPortTCP)
import Network.HTTP.Types
import Network.Mime (defaultMimeLookup)
import Network.Socket (Socket, close)
import Network.Wai
import Network.Wai.Handler.Warp (defaultSettings, runSettingsSocket, setHost, setPort)
import System.Directory (canonicalizePath, doesDirectoryExist, getCurrentDirectory, getHomeDirectory)
import System.Environment (getExecutablePath)
import System.FilePath (takeDirectory, (</>))
import System.Info (os)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.Posix.Signals (Handler (..), installHandler, sigHUP, sigTERM)
import System.Process (createProcess, proc, std_err, std_out, StdStream (NoStream))
import Text.Read (readMaybe)

import Fisher.Config (getPath, loadConfigAt)
import Fisher.UI.Access
import Fisher.UI.Api.ClaudeAuth (claudeAuthApi, newClaudeAuthEnv, stopClaudeLogins)
import Fisher.UI.Api.Context (contextApi)
import Fisher.UI.Api.Live (LiveEnv (..), liveApi, newLiveEnv, stopRuns)
import Fisher.UI.Api.Runlog (runlogApi)
import Fisher.UI.Api.Runs (runsApi, stopGates)
import Fisher.UI.Api.Search (SearchEnv, newSearchEnv, searchApi, stopSearches)
import Fisher.UI.Api.Settings (settingsApi)
import Fisher.UI.Api.Steps (stepsApi)
import Fisher.UI.Api.Templates (CreateEnv, newCreateEnv, stopCreates, templatesApi)
import Fisher.UI.Api.Sessions (sessionsApi)
import Fisher.UI.Db (UiEnv (..), newUiEnv)
import Fisher.UI.Prune (pruneLine, pruneUi)
import Fisher.UI.Response (Api, jsonResponse, pageHeaders)
import Fisher.UI.Static (staticFiles)

data UiConfig = UiConfig
  { ucPort :: Int      -- ^ 0: any free port
  , ucOpen :: Bool     -- ^ open the page in the browser
  , ucHome :: FilePath -- ^ the Fisher home
  }
  deriving (Eq, Show)

usage :: String
usage = "usage: fisher ui [--port N] [--no-open]\n\
        \       fisher ui --prune DAYS\n\n\
        \Serve the Fisher web app on 127.0.0.1 (default port 8732; 0 picks a free one)\n\
        \and open it in the browser unless --no-open.\n\n\
        \--prune DAYS (at least 1) deletes what the UI keeps of sessions not running\n\
        \whose last change is older than DAYS days (their ledger rows, step files and\n\
        \inbox), and run logs (fisher/ui-runs) older than that whose session is not\n\
        \running, then exits."

parseUiArgs :: FilePath -> [String] -> Either String UiConfig
parseUiArgs home = go (UiConfig 8732 True home)
  where
    go c = \case
      [] -> Right c
      ("--no-open" : rest) -> go c {ucOpen = False} rest
      ["--port"] -> Left "fisher ui: --port needs a number"
      ("--port" : n : rest) -> port n >>= \p -> go c {ucPort = p} rest
      (a : rest) | Just n <- stripFlag a -> port n >>= \p -> go c {ucPort = p} rest
      (a : _) -> Left ("fisher ui: unknown argument " <> a <> "\n" <> usage)
    stripFlag a = if take 7 a == "--port=" then Just (drop 7 a) else Nothing
    port n = case readMaybe n of
      Just p | p >= 0 && p <= 65535 -> Right p
      _ -> Left ("fisher ui: bad port " <> n)

uiUrl :: Int -> Text -> Text
uiUrl port tok = "http://127.0.0.1:" <> T.pack (show port) <> "/?t=" <> tok

-- | The @fisher ui@ command: exit code 0, or 1 on a usage error or a busy port.
uiMain :: FilePath -> [String] -> IO Int
uiMain home args
  | any (`elem` ["-h", "--help"]) args = putStrLn usage >> pure 0
  | ("--prune" : rest) <- args = case rest of
      -- at least a day: a session idle for minutes may only be between runs
      [d] | Just days <- readMaybe d, days >= (1 :: Double) -> do
        r <- try (pruneUi home days)
        case r of
          Right n -> putStrLn (pruneLine n) >> pure 0
          Left (e :: SomeException) -> hPutStrLn stderr ("fisher ui --prune: " <> show e) >> pure 1
      _ -> hPutStrLn stderr ("fisher ui: --prune takes a number of days, at least 1\n" <> usage) >> pure 1
  | otherwise = case parseUiArgs home args of
      Left e -> hPutStrLn stderr e >> pure 1
      Right c -> do
        r <- try (onTerminate (runUi c))
        case r of
          Right () -> pure 0
          Left (e :: IOException) -> hPutStrLn stderr ("fisher ui: " <> show e) >> pure 1

-- | SIGTERM (or SIGHUP) to the server, thrown to the thread that runs it
-- so that its brackets release (the gate and run children are stopped).
data Terminated = Terminated
  deriving (Show)

instance Exception Terminated

-- | Run the action with SIGTERM and SIGHUP thrown to this thread as
-- 'Terminated'; that ends it quietly.
onTerminate :: IO () -> IO ()
onTerminate act = do
  tid <- myThreadId
  let h = Catch (throwTo tid Terminated)
  _ <- installHandler sigTERM h Nothing
  _ <- installHandler sigHUP h Nothing
  act `catch` \Terminated -> pure ()

runUi :: UiConfig -> IO ()
runUi c = bracket (bindUi (ucPort c)) (close . snd) $ \(port, sock) -> do
  -- the bench root as the bench scripts default it; report.py from the
  -- repository 'findRepo' finds
  bench <- (</> ".cache/fisher/bench") <$> getHomeDirectory
  repo <- findRepo (ucHome c) =<< getExecutablePath
  -- the gate children (report.py) are killed when the server stops
  fisher <- getExecutablePath
  bracket (newUiEnv (ucHome c) bench repo) stopGates $ \env ->
   -- and the runs the page started
   bracket (newLiveEnv env fisher) stopRuns $ \live ->
    -- and the search runs
    bracket (newSearchEnv live (takeDirectory bench </> "search")) stopSearches $ \search ->
    -- and the creates of the New project view
    bracket (newCreateEnv live) stopCreates $ \creates ->
    -- and the Settings page's Claude Code login
    bracket (newClaudeAuthEnv (ucHome c)) stopClaudeLogins $ \claude -> do
    tok <- newToken
    let url = uiUrl port tok
    putStrLn ("Fisher UI: " <> T.unpack url)
    hFlush stdout
    when (ucOpen c) (openBrowser (T.unpack url))
    let settings = setHost "127.0.0.1" (setPort port defaultSettings)
    runSettingsSocket settings sock (serve (Gate tok port) (claudeAuthApi claude : allApis live search creates))

-- | The Fisher repository whose @tools/bench/<b>/report.py@ computes the
-- gates: the config key @fisher.repo@ when it names a directory, else the
-- first directory at or above the executable's that has @tools/bench@ (a
-- build run in place: stack's @.stack-work@ is inside the repository),
-- else the directory the server was started in.
findRepo :: FilePath -> FilePath -> IO FilePath
findRepo home exe = do
  (cfg, _) <- either (\(_ :: SomeException) -> (A.Null, [])) id <$> try (loadConfigAt home)
  configured <- case getPath ["fisher", "repo"] cfg of
    Just (A.String t) | not (T.null (T.strip t)) -> do
      let d = T.unpack (T.strip t)
      yes <- doesDirectoryExist d
      pure (if yes then Just d else Nothing)
    _ -> pure Nothing
  case configured of
    Just d -> pure d
    Nothing -> do
      exe' <- either (\(_ :: IOException) -> exe) id <$> try (canonicalizePath exe)
      up <- walk (takeDirectory exe')
      maybe getCurrentDirectory pure up
  where
    walk d = do
      yes <- doesDirectoryExist (d </> "tools" </> "bench")
      if yes then pure (Just d) else let p = takeDirectory d in if p == d then pure Nothing else walk p

-- | The read routes.
uiApis :: UiEnv -> [Api]
uiApis env = [sessionsApi env, contextApi env, runsApi env, runlogApi env]

-- | The live routes (first: @POST /api/sessions@ is theirs), the proof
-- canvas's, then the read routes.
liveApis :: LiveEnv -> [Api]
liveApis live = liveApi live : stepsApi live : uiApis (leUi live)

-- | Every route: the goal board's, the New project view's, the settings,
-- then 'liveApis'.
allApis :: LiveEnv -> SearchEnv -> CreateEnv -> [Api]
allApis live search creates = searchApi search : templatesApi creates : settingsApi (ueHome (leUi live)) : liveApis live

-- | A listening socket on 127.0.0.1 only (port 0: any free one), with the
-- port it got.
bindUi :: Int -> IO (Int, Socket)
bindUi port
  | port == 0 = bindRandomPortTCP "127.0.0.1"
  | otherwise = (,) port <$> bindPortTCP port "127.0.0.1"

openBrowser :: String -> IO ()
openBrowser url = do
  let cmd = if os == "darwin" then "open" else "xdg-open"
  r <- try (createProcess (proc cmd [url]) {std_out = NoStream, std_err = NoStream})
  case r of
    Right _ -> pure ()
    Left (_ :: IOException) -> hPutStrLn stderr ("fisher ui: could not run " <> cmd <> "; open the URL above")

-- | The gate, the token exchange, then @/api@ routes (built-in ping first),
-- then the embedded files, else @index.html@ for the page's own routes.
serve :: Gate -> [Api] -> Application
serve g apis r respond = case guardRequest g r of
  Left s -> respond (refuse s)
  Right ()
    | not (isApi r), Just (Just t) <- lookup "t" (queryString r) -> respond (exchange t)
    | isApi r -> firstApi (pingApi : apis) >>= respond
    | otherwise -> respond (static (pathInfo r))
  where
    exchange t
      | sameText t (TE.encodeUtf8 (gToken g)) =
          responseLBS found302 ((hLocation, "/") : ("Set-Cookie", tokenCookie (gPort g) (gToken g)) : pageHeaders) ""
      | otherwise = plain unauthorized401 "This link is from an earlier start of fisher ui; use the URL it printed last."
    firstApi = \case
      [] -> pure (jsonResponse notFound404 (A.object ["error" A..= ("no such route" :: Text)]))
      (a : as) -> a r >>= maybe (firstApi as) pure

refuse :: Status -> Response
refuse s
  | s == unauthorized401 = plain s "Open the URL that fisher ui printed."
  | otherwise = plain s "Refused."

plain :: Status -> BL.ByteString -> Response
plain s = responseLBS s ((hContentType, "text/plain; charset=utf-8") : pageHeaders)

pingApi :: Api
pingApi r = pure $ case pathInfo r of
  ["api", "ping"] -> Just (jsonResponse ok200 (A.object ["ok" A..= True]))
  _ -> Nothing

static :: [Text] -> Response
static segs = case lookup path staticFiles of
  Just body | path /= "index.html" ->
    responseLBS ok200 ((hContentType, defaultMimeLookup (T.pack path)) : cache : pageHeaders) (BL.fromStrict body)
  _ | take 1 segs == ["assets"] -> plain notFound404 "Not found."
    | otherwise -> index
  where
    path = T.unpack (T.intercalate "/" segs)
    -- Vite names assets by content hash
    cache = ("Cache-Control", if take 1 segs == ["assets"] then "public, max-age=31536000, immutable" else "no-cache")
    index = case lookup "index.html" staticFiles of
      Just body -> responseLBS ok200 ((hContentType, "text/html; charset=utf-8") : ("Cache-Control", "no-cache") : pageHeaders) (BL.fromStrict body)
      Nothing -> plain notFound404 "ui/dist has no index.html."
