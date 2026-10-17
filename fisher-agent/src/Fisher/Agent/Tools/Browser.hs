-- | @tools/browser_tool.py@: the eleven browser tools over the
-- @agent-browser@ CLI, in a local headless Chromium (@--session@), a
-- Browserbase cloud session or a user's CDP endpoint (@--cdp@), one
-- session per task.
module Fisher.Agent.Tools.Browser
  ( BrowserEnv (..)
  , RunOutcome (..)
  , newBrowserEnv
  , registerBrowserTools
  , checkBrowserRequirements
  , cleanupBrowser
  , cleanupAllBrowsers
  , socketRootFor
  , processRunner
  ) where

import Control.Concurrent.Async (async, wait)
import Control.Concurrent.MVar
import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM_, unless, when)
import qualified Data.ByteString as BS
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Char (isSpace, toLower)
import Data.IORef
import Data.List (isSuffixOf)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import System.Directory
import System.FilePath ((</>))
import System.Posix.Files (setFileMode)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.IO (hClose)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, terminateProcess, waitForProcess)
import qualified System.Timeout as Timeout
import Text.Read (readMaybe)

import Fisher.Agent.Tools.Web (toolSchemas)
import Fisher.Agent.Types
import Fisher.LLM.Errors (errorText)
import Fisher.LLM.Http (HttpResp (..), Timeouts (..), Transport (..))
import Fisher.LLM.Types (HttpReq (..))
import Fisher.Py.Value
import Fisher.Tools.Registry (Registry, ToolCtx (..), ToolDef (..), register)

-- | How one @subprocess.run@ ended.
data RunOutcome
  = Ran Int Text Text -- ^ exit code, stdout, stderr
  | TimedOut
  | RunFailed Text -- ^ the exception's text

data BrowserEnv = BrowserEnv
  { beRun :: [Text] -> [(Text, Text)] -> Int -> IO RunOutcome
  -- ^ argv, the environment variables set on top of ours, timeout in seconds
  , beWhich :: Text -> IO (Maybe Text)
  , beGetEnv :: Text -> IO (Maybe Text)
  , beTransport :: Transport
  -- ^ Browserbase's API
  , beAux :: AuxLLM
  , beInterrupted :: IO Bool
  , beUuidHex :: IO Text
  , beGaussHome :: FilePath
  , beSocketRoot :: FilePath
  -- ^ @_socket_safe_tmpdir()@: @/tmp@ on macOS
  , beIsDir :: FilePath -> IO Bool
  , beKill :: Int -> IO ()
  -- ^ SIGTERM to an agent-browser daemon (errors ignored)
  , beNow :: IO Double
  , beTimestamp :: IO Text
  -- ^ @time.strftime("%Y%m%d_%H%M%S")@
  , beRecordSessions :: IO Bool
  -- ^ @browser.record_sessions@ in config.yaml
  , beSessions :: MVar (M.Map Text Session)
  , beRecording :: IORef (S.Set Text)
  , beScreenshotCleanup :: IORef (M.Map FilePath Double)
  }

data Session = Session
  { sName :: Text
  , sBbId :: Maybe Text
  , sCdp :: Maybe Text
  , sFeatures :: [(Text, Bool)]
  , sFirstNav :: Bool
  }

newBrowserEnv
  :: ([Text] -> [(Text, Text)] -> Int -> IO RunOutcome) -> (Text -> IO (Maybe Text)) -> (Text -> IO (Maybe Text))
  -> Transport -> AuxLLM -> IO Bool -> IO Text -> FilePath -> FilePath -> (FilePath -> IO Bool) -> (Int -> IO ())
  -> IO Double -> IO Text -> IO Bool -> IO BrowserEnv
newBrowserEnv run which getEnv tr aux intr uuid gh sock isDir kill now ts rec =
  BrowserEnv run which getEnv tr aux intr uuid gh sock isDir kill now ts rec <$> newMVar M.empty <*> newIORef S.empty <*> newIORef M.empty

-- | @_socket_safe_tmpdir()@ for an OS name and the temp directory.
socketRootFor :: Text -> FilePath -> FilePath
socketRootFor osName tmp = if osName == "darwin" then "/tmp" else tmp

-- ------------------------------------------------------------ registration

registerBrowserTools :: Registry -> BrowserEnv -> IO ()
registerBrowserTools reg env = do
  let entry n = fromMaybe (PDict []) (get n toolSchemas)
      def n h =
        register reg
          ToolDef
            { tdName = n
            , tdToolset = "browser"
            , tdSchema = fromMaybe PNull (get "schema" (entry n))
            , tdHandler = h
            , tdCheck = Just (checkBrowserRequirements env)
            , tdRequiresEnv = []
            , tdEmoji = maybe "" pyStrOf (get "emoji" (entry n)) }
      task ctx = if T.null (tcTaskId ctx) then "default" else tcTaskId ctx
      str' k d args = case get k args of Just v -> v; Nothing -> PStr d
  def "browser_navigate" $ \ctx args -> navigate env (task ctx) (pyStrOf (str' "url" "" args))
  def "browser_snapshot" $ \ctx args -> snapshot env (task ctx) (maybe False truthy (get "full" args)) (tcUserTask ctx)
  def "browser_click" $ \ctx args -> kwargs "browser_click" ["ref"] args $ \case [ref] -> withRef ref (click env (task ctx)); _ -> pure ""
  def "browser_type" $ \ctx args -> kwargs "browser_type" ["ref", "text"] args $ \case [ref, txt] -> withRef ref (\r -> typeText env (task ctx) r (pyStrOf txt)); _ -> pure ""
  def "browser_scroll" $ \ctx args -> kwargs "browser_scroll" ["direction"] args $ \case [d] -> scroll env (task ctx) (pyStrOf d); _ -> pure ""
  def "browser_back" $ \ctx _ -> back env (task ctx)
  def "browser_press" $ \ctx args -> press env (task ctx) (pyStrOf (str' "key" "" args))
  def "browser_close" $ \ctx _ -> close env (task ctx)
  def "browser_get_images" $ \ctx _ -> images env (task ctx)
  def "browser_vision" $ \ctx args -> vision env (task ctx) (pyStrOf (str' "question" "" args)) (maybe False truthy (get "annotate" args))
  def "browser_console" $ \ctx args -> console env (task ctx) (maybe False truthy (get "clear" args))

-- | @f(**args, task_id=...)@: Python's complaints about the keywords.
kwargs :: Text -> [Text] -> PyVal -> ([PyVal] -> IO Text) -> IO Text
kwargs fn params args k =
  let kvs = case args of PDict xs -> xs; _ -> []
      extra = [n | (n, _) <- kvs, n `notElem` params]
      missing = [p | p <- params, isNothingIn p kvs]
      isNothingIn p xs = not (any ((== p) . fst) xs)
      quote n = "'" <> n <> "'"
      listed = \case
        [a] -> quote a
        [a, b] -> quote a <> " and " <> quote b
        xs -> T.intercalate ", " (map quote (init xs)) <> ", and " <> quote (last xs)
      typeError m = pure (dumpsAscii (PDict [("error", PStr ("Tool execution failed: TypeError: " <> m))]))
   in case extra of
        ("task_id" : _) -> typeError (fn <> "() got multiple values for keyword argument 'task_id'")
        (e : _) -> typeError (fn <> "() got an unexpected keyword argument '" <> e <> "'")
        []
          | not (null missing) ->
              typeError (fn <> "() missing " <> T.pack (show (length missing)) <> " required positional argument" <> (if length missing == 1 then "" else "s") <> ": " <> listed missing)
          | otherwise -> k [v | p <- params, Just v <- [lookup p kvs]]

-- | @if not ref.startswith("@"): ref = f"@{ref}"@
withRef :: PyVal -> (Text -> IO Text) -> IO Text
withRef ref k = case ref of
  PStr r -> k (if "@" `T.isPrefixOf` r then r else "@" <> r)
  v -> pure (dumpsAscii (PDict [("error", PStr ("Tool execution failed: AttributeError: '" <> typeName v <> "' object has no attribute 'startswith'"))]))

typeName :: PyVal -> Text
typeName = \case
  PNull -> "NoneType"
  PBool _ -> "bool"
  PInt _ -> "int"
  PFloat _ -> "float"
  PStr _ -> "str"
  PList _ -> "list"
  PDict _ -> "dict"

-- ------------------------------------------------------------ sessions

getEnvNonEmpty :: BrowserEnv -> Text -> IO (Maybe Text)
getEnvNonEmpty env k = (\case Just v | not (T.null v) -> Just v; _ -> Nothing) <$> beGetEnv env k

cdpOverride :: BrowserEnv -> IO Text
cdpOverride env = maybe "" T.strip <$> beGetEnv env "BROWSER_CDP_URL"

isLocalMode :: BrowserEnv -> IO Bool
isLocalMode env = do
  cdp <- cdpOverride env
  if not (T.null cdp)
    then pure False
    else do
      k <- getEnvNonEmpty env "BROWSERBASE_API_KEY"
      p <- getEnvNonEmpty env "BROWSERBASE_PROJECT_ID"
      pure (not (isJust k && isJust p))

findAgentBrowser :: BrowserEnv -> IO (Either Text [Text])
findAgentBrowser env = do
  ab <- beWhich env "agent-browser"
  case ab of
    Just p -> pure (Right [p])
    Nothing -> do
      npx <- beWhich env "npx"
      pure $ case npx of
        Just _ -> Right ["npx", "agent-browser"]
        Nothing ->
          Left "agent-browser CLI not found. Install it with: npm install -g agent-browser\nOr run 'npm install' in the repo root to install locally.\nOr ensure npx is available in your PATH."

checkBrowserRequirements :: BrowserEnv -> IO Bool
checkBrowserRequirements env = do
  ab <- findAgentBrowser env
  case ab of
    Left _ -> pure False
    Right _ -> do
      local <- isLocalMode env
      if local
        then pure True
        else do
          k <- getEnvNonEmpty env "BROWSERBASE_API_KEY"
          p <- getEnvNonEmpty env "BROWSERBASE_PROJECT_ID"
          pure (isJust k && isJust p)

-- | @_get_session_info@: Left is the exception creating it raised (its
-- class and text).
sessionInfo :: BrowserEnv -> Text -> IO (Either (Text, Text) Session)
sessionInfo env task = do
  known <- M.lookup task <$> readMVar (beSessions env)
  case known of
    Just s -> pure (Right s)
    Nothing -> do
      cdp <- cdpOverride env
      local <- isLocalMode env
      r <-
        if not (T.null cdp)
          then do
            h <- beUuidHex env
            pure (Right (Session ("cdp_" <> T.take 10 h) Nothing (Just cdp) [("cdp_override", True)] True))
          else
            if local
              then do
                h <- beUuidHex env
                pure (Right (Session ("h_" <> T.take 10 h) Nothing Nothing [("local", True)] True))
              else browserbaseSession env task
      forM_ r $ \s -> modifyMVar_ (beSessions env) (pure . M.insert task s)
      pure r

-- | @_create_browserbase_session@
browserbaseSession :: BrowserEnv -> Text -> IO (Either (Text, Text) Session)
browserbaseSession env task = do
  Just key <- getEnvNonEmpty env "BROWSERBASE_API_KEY"
  Just project <- getEnvNonEmpty env "BROWSERBASE_PROJECT_ID"
  let lowerEnv k d = maybe d (T.map toLower) <$> beGetEnv env k
  proxies <- (/= "false") <$> lowerEnv "BROWSERBASE_PROXIES" "true"
  stealth <- (== "true") <$> lowerEnv "BROWSERBASE_ADVANCED_STEALTH" "false"
  keepAlive <- (/= "false") <$> lowerEnv "BROWSERBASE_KEEP_ALIVE" "true"
  customTimeout <- getEnvNonEmpty env "BROWSERBASE_SESSION_TIMEOUT"
  let timeoutVal = customTimeout >>= \t -> readMaybe (T.unpack (T.filter (/= '_') (T.dropWhile (== '+') (T.strip t)))) :: Maybe Integer
      config ka px =
        PDict $
          [("projectId", PStr project)]
            <> [("keepAlive", PBool True) | ka]
            <> [("timeout", PInt t) | Just t <- [timeoutVal], t > 0]
            <> [("proxies", PBool True) | px]
            <> [("browserSettings", PDict [("advancedStealth", PBool True)]) | stealth]
      send body = tRequest (beTransport env) (HttpReq "POST" "https://api.browserbase.com/v1/sessions" [("Content-Type", "application/json"), ("X-BB-API-Key", key)] (Just body) False) (Timeouts 30 30)
      status = either (const 0) rStatus
  r0 <- send (config keepAlive proxies)
  (r1, kaFallback) <-
    if status r0 == 402 && keepAlive then (,True) <$> send (config False proxies) else pure (r0, False)
  (r2, pxFallback) <-
    if status r1 == 402 && proxies then (,True) <$> send (config (keepAlive && not kaFallback) False) else pure (r1, False)
  case r2 of
    Left e -> pure (Left ("ConnectionError", errorText e))
    Right resp
      | rStatus resp >= 400 ->
          pure (Left ("RuntimeError", "Failed to create Browserbase session: " <> T.pack (show (rStatus resp)) <> " " <> TE.decodeUtf8With TEE.lenientDecode (rBody resp)))
      | otherwise -> case loads (rBody resp) of
          Right d | Just (PStr i) <- get "id" d, Just (PStr u) <- get "connectUrl" d -> do
            h <- beUuidHex env
            let features =
                  [ ("basic_stealth", True), ("proxies", proxies && not pxFallback), ("advanced_stealth", stealth)
                  , ("keep_alive", keepAlive && not kaFallback), ("custom_timeout", isJust customTimeout && maybe False (> 0) timeoutVal) ]
            pure (Right (Session ("gauss_" <> task <> "_" <> T.take 8 h) (Just i) (Just u) features True))
          _ -> pure (Left ("KeyError", "'id'"))

-- | @_run_browser_command@: the parsed JSON answer (a dict).
runCommand :: BrowserEnv -> Text -> Text -> [Text] -> Int -> IO PyVal
runCommand env task command args timeout = do
  ab <- findAgentBrowser env
  case ab of
    Left e -> pure (failure e)
    Right browserCmd -> do
      intr <- beInterrupted env
      if intr
        then pure (failure "Interrupted")
        else do
          si <- sessionInfo env task
          case si of
            Left (_, e) -> pure (failure ("Failed to create browser session: " <> e))
            Right s -> do
              let backend = case sCdp s of
                      Just u -> ["--cdp", u]
                      Nothing -> ["--session", sName s]
                  argv = browserCmd <> backend <> ["--json", command] <> args
                  sockDir = beSocketRoot env </> ("agent-browser-" <> T.unpack (sName s))
              mk <- try $ do
                ex <- doesDirectoryExist sockDir
                unless ex (createDirectoryIfMissing True sockDir >> setFileMode sockDir 0o700)
              case mk of
                Left (e :: SomeException) -> pure (failure (T.pack (displayException e)))
                Right () -> do
                  path <- browserPath env
                  ran <- beRun env argv [("PATH", path), ("AGENT_BROWSER_SOCKET_DIR", T.pack sockDir)] timeout
                  case ran of
                    TimedOut -> pure (failure ("Command timed out after " <> T.pack (show timeout) <> " seconds"))
                    RunFailed e -> pure (failure e)
                    Ran rc stdout stderr -> do
                      let so = T.strip stdout
                      if not (T.null so)
                        then case loads (TE.encodeUtf8 so) of
                          Right v@(PDict _) -> pure v
                          Right _ -> pure (PDict [])
                          Left _ -> do
                            let raw = T.take 2000 so
                            recovered <-
                              if command == "screenshot"
                                then do
                                  let combined = T.intercalate "\n" (filter (not . T.null) [so, T.strip stderr])
                                  case screenshotPathFromText combined of
                                    Just p -> do
                                      ex <- doesFileExist (T.unpack p)
                                      pure (if ex then Just p else Nothing)
                                    Nothing -> pure Nothing
                                else pure Nothing
                            pure $ case recovered of
                              Just p -> PDict [("success", PBool True), ("data", PDict [("path", PStr p), ("raw", PStr raw)])]
                              Nothing -> failure ("Non-JSON output from agent-browser for '" <> command <> "': " <> raw)
                        else
                          if rc /= 0
                            then pure (failure (if T.null stderr then "Command failed with code " <> T.pack (show rc) else T.strip stderr))
                            else pure (PDict [("success", PBool True), ("data", PDict [])])
  where
    failure e = PDict [("success", PBool False), ("error", PStr e)]

-- | The PATH agent-browser runs with: Gauss's own node, then the usual
-- system directories, put in front when they exist and are missing.
browserPath :: BrowserEnv -> IO Text
browserPath env = do
  existing <- fromMaybe "" <$> beGetEnv env "PATH"
  let parts0 = filter (not . T.null) (T.splitOn ":" existing)
      cands = T.pack (beGaussHome env </> "node" </> "bin") : T.splitOn ":" "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
      step parts c = do
        d <- beIsDir env (T.unpack c)
        pure (if d && c `notElem` parts then c : parts else parts)
  parts <- foldlM' step parts0 (reverse cands)
  pure (T.intercalate ":" parts)
  where
    foldlM' f z xs = case xs of
      [] -> pure z
      (x : rest) -> f z x >>= \z' -> foldlM' f z' rest

-- | @_extract_screenshot_path_from_text@
screenshotPathFromText :: Text -> Maybe Text
screenshotPathFromText t
  | T.null t = Nothing
  | otherwise = firstJust [quoted, saved, anyPng]
  where
    firstJust = \case
      [] -> Nothing
      (Just x : _) -> Just x
      (Nothing : xs) -> firstJust xs
    clean p = let s = T.dropAround (`elem` ("'\"" :: String)) (T.strip p) in if T.null s then Nothing else Just s
    -- Screenshot saved to '/path.png'
    quoted = go (T.breakOnAll "Screenshot saved to " t)
      where
        go = \case
          [] -> Nothing
          ((_, rest) : more) ->
            let r = T.drop (T.length "Screenshot saved to ") rest
             in case T.uncons r of
                  Just (q, r') | q `elem` ("'\"" :: String), Just ('/', _) <- T.uncons r' ->
                    let body = T.takeWhile (`notElem` ("'\"" :: String)) r'
                        close' = T.drop (T.length body) r'
                     in case pngPrefix body of
                          Just p | Just (c, _) <- T.uncons close', c `elem` ("'\"" :: String), p == body -> clean p
                          _ -> go more
                  _ -> go more
    -- Screenshot saved to /path.png followed by space or end
    saved = go (T.breakOnAll "Screenshot saved to /" t)
      where
        go = \case
          [] -> Nothing
          ((_, rest) : more) ->
            let r = T.drop (T.length "Screenshot saved to ") rest
             in case pathPngEnd r of
                  Just p -> clean p
                  Nothing -> go more
    anyPng = go (T.breakOnAll "/" t)
      where
        go = \case
          [] -> Nothing
          ((_, rest) : more) -> case pathPngEnd rest of
            Just p -> clean p
            Nothing -> go more
    -- the shortest @/\S+?\.png@ followed by whitespace or the end
    pathPngEnd r =
      let word = T.takeWhile (not . isSpace) r
          cands = [T.take (i + 4) word | i <- [1 .. T.length word - 4], ".png" `T.isPrefixOf` T.drop i word]
       in case [c | c <- cands, let after = T.drop (T.length c) r, T.null after || isSpace (T.head after)] of
            (c : _) -> Just c
            [] -> Nothing
    pngPrefix body = if ".png" `T.isSuffixOf` body && T.length body > 5 then Just body else Nothing

-- ------------------------------------------------------------ the tools

blockedPatterns :: [Text]
blockedPatterns =
  [ "access denied", "access to this page has been denied", "blocked", "bot detected", "verification required"
  , "please verify", "are you a robot", "captcha", "cloudflare", "ddos protection", "checking your browser"
  , "just a moment", "attention required" ]

out :: [(Text, PyVal)] -> IO Text
out = pure . dumpsUnicode . PDict

dataOf :: PyVal -> PyVal
dataOf r = case get "data" r of
  Just d@(PDict _) -> d
  _ -> PDict []

succeeded :: PyVal -> Bool
succeeded r = maybe False truthy (get "success" r)

errOr :: PyVal -> Text -> PyVal
errOr r d = fromMaybe (PStr d) (get "error" r)

navigate :: BrowserEnv -> Text -> Text -> IO Text
navigate env task url = do
  si <- sessionInfo env task
  case si of
    Left (cls, e) -> pure (dumpsAscii (PDict [("error", PStr ("Tool execution failed: " <> cls <> ": " <> e))]))
    Right s -> do
      let first = sFirstNav s
      when first $ do
        modifyMVar_ (beSessions env) (pure . M.adjust (\x -> x {sFirstNav = False}) task)
        maybeStartRecording env task
      r <- runCommand env task "open" [url] 60
      if succeeded r
        then do
          let d = dataOf r
              title = maybe "" pyStrOf (get "title" d)
              finalUrl = fromMaybe (PStr url) (get "url" d)
              lower = T.toLower title
              bot =
                [ ("bot_detection_warning", PStr ("Page title '" <> title <> "' suggests bot detection. The site may have blocked this request. Options: 1) Try adding delays between actions, 2) Access different pages first, 3) Enable advanced stealth (BROWSERBASE_ADVANCED_STEALTH=true, requires Scale plan), 4) Some sites have very aggressive bot detection that may be unavoidable."))
                | any (`T.isInfixOf` lower) blockedPatterns ]
              feats = sFeatures s
              stealth =
                if first
                  then
                    [ ("stealth_warning", PStr "Running WITHOUT residential proxies. Bot detection may be more aggressive. Consider upgrading Browserbase plan for proxy support.")
                    | not (fromMaybe False (lookup "proxies" feats)) ]
                      <> [("stealth_features", PList [PStr k | (k, v) <- feats, v])]
                  else []
          out ([("success", PBool True), ("url", finalUrl), ("title", maybe (PStr "") id (get "title" d))] <> bot <> stealth)
        else out [("success", PBool False), ("error", errOr r "Navigation failed")]

snapshot :: BrowserEnv -> Text -> Bool -> Maybe Text -> IO Text
snapshot env task full userTask = do
  r <- runCommand env task "snapshot" (if full then [] else ["-c"]) 30
  if succeeded r
    then do
      let d = dataOf r
          text0 = maybe "" pyStrOf (get "snapshot" d)
          refs = case get "refs" d of
            Just (PDict kvs) -> length kvs
            Just (PList xs) -> length xs
            _ -> 0
      text <-
        if T.length text0 > 8000
          then case userTask of
            Just t | not (T.null t) -> extractRelevant env text0 t
            _ -> pure (PStr (truncateSnapshot text0))
          else pure (PStr text0)
      out [("success", PBool True), ("snapshot", text), ("element_count", PInt (fromIntegral refs))]
    else out [("success", PBool False), ("error", errOr r "Failed to get snapshot")]

truncateSnapshot :: Text -> Text
truncateSnapshot t = if T.length t <= 8000 then t else T.take 8000 t <> "\n\n[... content truncated ...]"

-- | @_extract_relevant_content@ with a task (the content, or the plain
-- truncation when the call fails).
extractRelevant :: BrowserEnv -> Text -> Text -> IO PyVal
extractRelevant env snap task = do
  model <- (\case Just m | not (T.null (T.strip m)) -> Just (T.strip m); _ -> Nothing) <$> beGetEnv env "AUXILIARY_WEB_EXTRACT_MODEL"
  let prompt =
        "You are a content extractor for a browser automation agent.\n\nThe user's task is: " <> task
          <> "\n\nGiven the following page snapshot (accessibility tree representation), extract and summarize the most relevant information for completing this task. Focus on:\n1. Interactive elements (buttons, links, inputs) that might be needed\n2. Text content relevant to the task (prices, descriptions, headings, important info)\n3. Navigation structure if relevant\n\nKeep ref IDs (like [ref=e5]) for interactive elements so the agent can use them.\n\nPage Snapshot:\n"
          <> snap <> "\n\nProvide a concise summary that preserves actionable information and relevant content."
  r <- beAux env (auxCall "web_extract" [PDict [("role", PStr "user"), ("content", PStr prompt)]]) {axMaxTokens = Just 4000, axTemperature = Just 0.1, axModel = model}
  pure $ case r of
    Right resp -> responseContent resp
    Left _ -> PStr (truncateSnapshot snap)

click :: BrowserEnv -> Text -> Text -> IO Text
click env task ref = do
  r <- runCommand env task "click" [ref] 30
  if succeeded r then out [("success", PBool True), ("clicked", PStr ref)] else out [("success", PBool False), ("error", errOr r ("Failed to click " <> ref))]

typeText :: BrowserEnv -> Text -> Text -> Text -> IO Text
typeText env task ref txt = do
  r <- runCommand env task "fill" [ref, txt] 30
  if succeeded r
    then out [("success", PBool True), ("typed", PStr txt), ("element", PStr ref)]
    else out [("success", PBool False), ("error", errOr r ("Failed to type into " <> ref))]

scroll :: BrowserEnv -> Text -> Text -> IO Text
scroll env task dir
  | dir `notElem` ["up", "down"] = out [("success", PBool False), ("error", PStr ("Invalid direction '" <> dir <> "'. Use 'up' or 'down'."))]
  | otherwise = do
      r <- runCommand env task "scroll" [dir] 30
      if succeeded r then out [("success", PBool True), ("scrolled", PStr dir)] else out [("success", PBool False), ("error", errOr r ("Failed to scroll " <> dir))]

back :: BrowserEnv -> Text -> IO Text
back env task = do
  r <- runCommand env task "back" [] 30
  if succeeded r
    then out [("success", PBool True), ("url", fromMaybe (PStr "") (get "url" (dataOf r)))]
    else out [("success", PBool False), ("error", errOr r "Failed to go back")]

press :: BrowserEnv -> Text -> Text -> IO Text
press env task key = do
  r <- runCommand env task "press" [key] 30
  if succeeded r then out [("success", PBool True), ("pressed", PStr key)] else out [("success", PBool False), ("error", errOr r ("Failed to press " <> key))]

close :: BrowserEnv -> Text -> IO Text
close env task = do
  had <- M.member task <$> readMVar (beSessions env)
  cleanupBrowser env task
  out ([("success", PBool True), ("closed", PBool True)] <> [("warning", PStr "Session may not have been active") | not had])

console :: BrowserEnv -> Text -> Bool -> IO Text
console env task clear = do
  let a = ["--clear" | clear]
  c <- runCommand env task "console" a 30
  e <- runCommand env task "errors" a 30
  let msgs =
        if succeeded c
          then [PDict [("type", fromMaybe (PStr "log") (get "type" m)), ("text", fromMaybe (PStr "") (get "text" m)), ("source", PStr "console")] | Just (PList ms) <- [get "messages" (dataOf c)], m <- ms]
          else []
      errs =
        if succeeded e
          then [PDict [("message", fromMaybe (PStr "") (get "message" x)), ("source", PStr "exception")] | Just (PList xs) <- [get "errors" (dataOf e)], x <- xs]
          else []
  out [("success", PBool True), ("console_messages", PList msgs), ("js_errors", PList errs), ("total_messages", PInt (fromIntegral (length msgs))), ("total_errors", PInt (fromIntegral (length errs)))]

imagesJs :: Text
imagesJs =
  "JSON.stringify(\n        [...document.images].map(img => ({\n            src: img.src,\n            alt: img.alt || '',\n            width: img.naturalWidth,\n            height: img.naturalHeight\n        })).filter(img => img.src && !img.src.startsWith('data:'))\n    )"

images :: BrowserEnv -> Text -> IO Text
images env task = do
  r <- runCommand env task "eval" [imagesJs] 30
  if succeeded r
    then do
      let raw = fromMaybe (PStr "[]") (get "result" (dataOf r))
          parsed = case raw of
            PStr s -> either (const Nothing) Just (loads (TE.encodeUtf8 s))
            v -> Just v
          count v = case v of
            PList xs -> length xs
            PDict xs -> length xs
            PStr s -> T.length s
            _ -> 0
      case parsed of
        Just imgs -> out [("success", PBool True), ("images", imgs), ("count", PInt (fromIntegral (count imgs)))]
        Nothing -> out [("success", PBool True), ("images", PList []), ("count", PInt 0), ("warning", PStr "Could not parse image data")]
    else out [("success", PBool False), ("error", errOr r "Failed to get images")]

vision :: BrowserEnv -> Text -> Text -> Bool -> IO Text
vision env task question annotate = do
  h <- beUuidHex env
  let dir = beGaussHome env </> "browser_screenshots"
      planned = dir </> ("browser_screenshot_" <> T.unpack h <> ".png")
  shotRef <- newIORef planned
  res <- try $ do
    createDirectoryIfMissing True dir
    cleanupOldScreenshots env dir
    r <- runCommand env task "screenshot" (["--annotate" | annotate] <> ["--full", T.pack planned]) 30
    local <- isLocalMode env
    let mode = if local then "local" else "cloud"
    if not (succeeded r)
      then out [("success", PBool False), ("error", PStr ("Failed to take screenshot (" <> mode <> " mode): " <> maybe "Unknown error" pyStrOf (get "error" r)))]
      else do
        forM_ (get "path" (dataOf r)) $ \case
          PStr p | not (T.null p) -> writeIORef shotRef (T.unpack p)
          _ -> pure ()
        shot <- readIORef shotRef
        exists <- doesFileExist shot
        if not exists
          then
            out
              [ ("success", PBool False)
              , ("error", PStr ("Screenshot file was not created at " <> T.pack shot <> " (" <> mode <> " mode). This may indicate a socket path issue (macOS /var/folders/), a missing Chromium install ('agent-browser install'), or a stale daemon process.")) ]
          else do
            bytes <- BS.readFile shot
            model <- (\case Just m | not (T.null (T.strip m)) -> Just (T.strip m); _ -> Nothing) <$> beGetEnv env "AUXILIARY_VISION_MODEL"
            let prompt =
                  "You are analyzing a screenshot of a web browser.\n\nUser's question: " <> question
                    <> "\n\nProvide a detailed and helpful answer based on what you see in the screenshot. If there are interactive elements, describe them. If there are verification challenges or CAPTCHAs, describe what type they are and what action might be needed. Focus on answering the user's specific question."
                dataUrl = "data:image/png;base64," <> base64 bytes
                msg = PDict [("role", PStr "user"), ("content", PList [PDict [("type", PStr "text"), ("text", PStr prompt)], PDict [("type", PStr "image_url"), ("image_url", PDict [("url", PStr dataUrl)])]])]
            a <- beAux env (auxCall "vision" [msg]) {axMaxTokens = Just 2000, axTemperature = Just 0.1, axModel = model}
            case a of
              Left e -> visionFailed shotRef (case e of AuxNoProvider m -> m; AuxFailed m -> m)
              Right resp ->
                out $
                  [("success", PBool True), ("analysis", responseContent resp), ("screenshot_path", PStr (T.pack shot))]
                    <> [("annotations", v) | annotate, Just v <- [get "annotations" (dataOf r)], truthy v]
  case res of
    Right t -> pure t
    Left (e :: SomeException) -> visionFailed shotRef (T.pack (displayException e))
  where
    -- the screenshot, when it was taken, is kept and named
    visionFailed shotRef m = do
      shot <- readIORef shotRef
      exists <- doesFileExist shot
      out $
        [("success", PBool False), ("error", PStr ("Error during vision analysis: " <> m))]
          <> concat [[("screenshot_path", PStr (T.pack shot)), ("note", PStr "Screenshot was captured but vision analysis failed. You can still share it via MEDIA:<path>.")] | exists]

-- | @_cleanup_old_screenshots@: at most once an hour per directory, files
-- older than a day go.
cleanupOldScreenshots :: BrowserEnv -> FilePath -> IO ()
cleanupOldScreenshots env dir = do
  now <- beNow env
  lastRun <- M.findWithDefault 0 dir <$> readIORef (beScreenshotCleanup env)
  unless (now - lastRun < 3600) $ do
    modifyIORef (beScreenshotCleanup env) (M.insert dir now)
    removeOlder dir now (24 * 3600) (\f -> "browser_screenshot_" `T.isPrefixOf` T.pack f && ".png" `isSuffixOf` f)

removeOlder :: FilePath -> Double -> Double -> (FilePath -> Bool) -> IO ()
removeOlder dir now age match = do
  r <- try (listDirectory dir)
  case r of
    Left (_ :: SomeException) -> pure ()
    Right fs -> forM_ (filter match fs) $ \f -> do
      _ <- try @SomeException $ do
        m <- getModificationTime (dir </> f)
        when (realToFrac (utcTimeToPOSIXSeconds m) < now - age) (removeFile (dir </> f))
      pure ()

-- ------------------------------------------------------------ recording

maybeStartRecording :: BrowserEnv -> Text -> IO ()
maybeStartRecording env task = do
  active <- S.member task <$> readIORef (beRecording env)
  enabled <- beRecordSessions env
  when (not active && enabled) $ do
    let dir = beGaussHome env </> "browser_recordings"
    createDirectoryIfMissing True dir
    now <- beNow env
    removeOlder dir now (72 * 3600) (\f -> "session_" `T.isPrefixOf` T.pack f && ".webm" `isSuffixOf` f)
    ts <- beTimestamp env
    let path = dir </> T.unpack ("session_" <> ts <> "_" <> T.take 16 task <> ".webm")
    r <- runCommand env task "record" ["start", T.pack path] 30
    when (succeeded r) (modifyIORef (beRecording env) (S.insert task))

maybeStopRecording :: BrowserEnv -> Text -> IO ()
maybeStopRecording env task = do
  active <- S.member task <$> readIORef (beRecording env)
  when active $ do
    _ <- runCommand env task "record" ["stop"] 30
    modifyIORef (beRecording env) (S.delete task)

-- ------------------------------------------------------------ cleanup

-- | @cleanup_browser(task_id)@: stop a recording, @close@, release a
-- Browserbase session, stop the daemon and remove its socket directory.
cleanupBrowser :: BrowserEnv -> Text -> IO ()
cleanupBrowser env task = do
  known <- M.lookup task <$> readMVar (beSessions env)
  forM_ known $ \s -> do
    maybeStopRecording env task
    _ <- try @SomeException (runCommand env task "close" [] 10)
    modifyMVar_ (beSessions env) (pure . M.delete task)
    local <- isLocalMode env
    forM_ (sBbId s) $ \bb -> unless local $ do
      key <- getEnvNonEmpty env "BROWSERBASE_API_KEY"
      project <- getEnvNonEmpty env "BROWSERBASE_PROJECT_ID"
      case (key, project) of
        (Just k, Just p) ->
          () <$ tRequest (beTransport env)
            (HttpReq "POST" ("https://api.browserbase.com/v1/sessions/" <> bb) [("X-BB-API-Key", k), ("Content-Type", "application/json")] (Just (PDict [("projectId", PStr p), ("status", PStr "REQUEST_RELEASE")])) False)
            (Timeouts 10 10)
        _ -> pure ()
    let sockDir = beSocketRoot env </> ("agent-browser-" <> T.unpack (sName s))
    ex <- doesDirectoryExist sockDir
    when ex $ do
      let pidFile = sockDir </> (T.unpack (sName s) <> ".pid")
      hasPid <- doesFileExist pidFile
      when hasPid $ do
        t <- try @SomeException (T.strip . TE.decodeUtf8With TEE.lenientDecode <$> BS.readFile pidFile)
        case t of
          Right txt | Just pid <- readMaybe (T.unpack txt) -> () <$ try @SomeException (beKill env pid)
          _ -> pure ()
      _ <- try @SomeException (removeDirectoryRecursive sockDir)
      pure ()

cleanupAllBrowsers :: BrowserEnv -> IO ()
cleanupAllBrowsers env = do
  tasks <- M.keys <$> readMVar (beSessions env)
  mapM_ (cleanupBrowser env) tasks

-- | Standard base64 with padding.
base64 :: BS.ByteString -> Text
base64 bs = T.pack (go (BS.unpack bs))
  where
    tbl = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" :: String
    ch n = tbl !! fromIntegral (n .&. 63)
    go = \case
      (a : b : c : rest) ->
        let n = (fromIntegral a `shiftL` 16) .|. (fromIntegral b `shiftL` 8) .|. fromIntegral c :: Int
         in ch (n `shiftR` 18) : ch (n `shiftR` 12) : ch (n `shiftR` 6) : ch n : go rest
      [a, b] ->
        let n = (fromIntegral a `shiftL` 16) .|. (fromIntegral b `shiftL` 8) :: Int
         in [ch (n `shiftR` 18), ch (n `shiftR` 12), ch (n `shiftR` 6), '=']
      [a] ->
        let n = fromIntegral a `shiftL` 16 :: Int
         in [ch (n `shiftR` 18), ch (n `shiftR` 12), '=', '=']
      [] -> []

-- | @subprocess.run(argv, capture_output=True, text=True, timeout=t,
-- env={**os.environ, ...})@
processRunner :: [Text] -> [(Text, Text)] -> Int -> IO RunOutcome
processRunner argv extra secs = case map T.unpack argv of
  [] -> pure (RunFailed "list index out of range")
  (exe : args) -> do
    base <- getEnvironment
    let overrides = [(T.unpack k, T.unpack v) | (k, v) <- extra]
        env' = overrides <> [kv | kv@(k, _) <- base, k `notElem` map fst overrides]
    r <- try $ do
      (_, Just hout, Just herr, ph) <- createProcess (proc exe args) {env = Just env', std_in = NoStream, std_out = CreatePipe, std_err = CreatePipe}
      outA <- async (BS.hGetContents hout)
      errA <- async (BS.hGetContents herr)
      done <- Timeout.timeout (secs * 1000000) (waitForProcess ph)
      case done of
        Nothing -> do
          terminateProcess ph
          _ <- waitForProcess ph
          hClose hout >> hClose herr
          pure TimedOut
        Just code -> do
          o <- wait outA
          e <- wait errA
          let dec = TE.decodeUtf8With TEE.lenientDecode
          pure (Ran (case code of ExitSuccess -> 0; ExitFailure n -> n) (dec o) (dec e))
    pure $ case r of
      Right x -> x
      Left (e :: SomeException) -> RunFailed (T.pack (displayException e))
