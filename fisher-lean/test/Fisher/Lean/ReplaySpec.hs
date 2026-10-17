-- | The C1 and C2 goldens offline: each recorded lean-lsp-mcp session is
-- run again with Fisher's tools over a fake @lake serve@ that answers from
-- the session's LSP transcript (Fisher must send what leanclient sent), and
-- a fake HTTP that answers from the session's recorded exchanges (Fisher
-- must ask what urllib asked). Every tool result must equal the recorded
-- one. lean --profile and the Mathlib sessions are left to the live check
-- (tools/e2e-c1.sh, tools/e2e-c2.sh); lean_local_search runs the real rg
-- over the real stdlib, so its session needs the toolchain installed.
module Fisher.Lean.ReplaySpec (spec, offlineSessions, prepareFixture, unmask) where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, when)
import System.Exit (ExitCode (..))
import System.Process (proc, readCreateProcessWithExitCode, CreateProcess (..))
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory
import System.FilePath ((</>), takeDirectory)
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Hspec

import Fisher.Lean.Client (ClientEnv (..), defaultClientEnv)
import Fisher.Lean.FakeLake
import Fisher.Lean.Http
import Fisher.Lean.Session
import Fisher.Lean.Text (pathAsUri)
import Fisher.Lean.Tool
import Fisher.Lean.Tools
import Fisher.Py.Value

offlineSessions :: [Text]
offlineSessions =
  [ "core-goals", "core-diagnostics", "core-unicode", "core-analysis", "core-verify-no-rg", "core-edits", "core-build"
  , "search-local", "search-local-no-rg", "search-remote", "search-stub", "search-refused" ]

-- | Sessions whose results depend on the toolchain's own sources.
stdlibSessions :: [Text]
stdlibSessions = ["search-local"]

-- | @lean --print-prefix@ in the fixture, when that toolchain is installed.
realPrefix :: FilePath -> IO (Maybe FilePath)
realPrefix root = do
  r <- try @SomeException (readCreateProcessWithExitCode (proc "lean" ["--print-prefix"]) {cwd = Just root} "")
  case r of
    Right (ExitSuccess, out, _) | p <- takeWhile (/= '\n') out, not (null p) -> do
      ok <- doesDirectoryExist (p </> "src")
      pure (if ok then Just p else Nothing)
    _ -> pure Nothing

copyTree :: FilePath -> FilePath -> IO ()
copyTree from to = do
  createDirectoryIfMissing True to
  es <- listDirectory from
  forM_ es $ \e -> do
    isDir <- doesDirectoryExist (from </> e)
    if isDir then copyTree (from </> e) (to </> e) else copyFile (from </> e) (to </> e)

-- | The core fixture as the recording saw it, under @dir@.
prepareFixture :: Text -> FilePath -> IO FilePath
prepareFixture version dir = do
  let root = dir </> "core"
  copyTree "test/fixtures/core" root
  BS.writeFile (root </> "lean-toolchain") (TE.encodeUtf8 ("leanprover/lean4:v" <> version <> "\n"))
  pure root

noLean :: String
noLean = "replay: no lean to run"

unmask :: FilePath -> FilePath -> Text -> Text
unmask root prefix =
  T.replace "<LEAN>" (T.pack prefix) . T.replace "file://<LEAN>" (pathAsUri prefix)
    . T.replace "<ROOT:core>" (T.pack root) . T.replace "file://<ROOT:core>" (pathAsUri root)

spec :: Spec
spec =
  forM_ [("c1", v) | v <- ["4.33.0", "4.33.1"]] (uncurry phaseSpec) >> forM_ [("c2", v) | v <- ["4.33.0", "4.33.1"]] (uncurry phaseSpec)

phaseSpec :: String -> Text -> Spec
phaseSpec phase version = do
    doc <- runIO (BS.readFile ("test/golden/" <> phase <> "/" <> phase <> "-" <> T.unpack version <> ".json"))
    describe (phase <> ": lean-lsp-mcp 0.31.0 on Lean " <> T.unpack version <> " (replayed)") $
      forM_ (sessionsOf doc) $ \(name, _) -> when (name `elem` offlineSessions) $
        it (T.unpack name) $ withSystemTempDirectory "fisher-c1" $ \tmp -> do
          root <- prepareFixture version tmp
          real <- if name `elem` stdlibSessions then realPrefix root else pure Nothing
          let fake = tmp </> "lean"
              prefix = maybe fake id real
          createDirectoryIfMissing True (fake </> "src")
          when (name `elem` stdlibSessions && real == Nothing) $
            pendingWith ("needs Lean " <> T.unpack version <> " installed (rg searches its sources)")
          let raw = TE.decodeUtf8 doc
          full <- either (fail . ("golden: " <>)) pure (loads (TE.encodeUtf8 (unmask root prefix raw)))
          sess <- case [s | s <- list "sessions" full, get "name" s == Just (PStr name)] of
            (x : _) -> pure x
            [] -> fail ("no session " <> T.unpack name)
          stopped <- runSession root prefix sess
          forM_ stopped $ \(i, why) -> pendingWith ("step " <> show i <> ": " <> T.unpack why)

sessionsOf :: BS.ByteString -> [(Text, ())]
sessionsOf b = case loads b of
  Right d -> [(n, ()) | s <- list "sessions" d, Just (PStr n) <- [get "name" s]]
  Left _ -> []

list :: Text -> PyVal -> [PyVal]
list k v = case get k v of Just (PList xs) -> xs; _ -> []

runSession :: FilePath -> FilePath -> PyVal -> IO (Maybe (Int, Text))
runSession root prefix sess = do
  fake <- newFakeLake (list "lsp" sess)
  web <- newIORef (list "http" sess)
  webMiss <- newIORef []
  canned <- newIORef ""
  liveOnly <- newIORef []
  diffs <- newIORef []
  calls <- newIORef (0 :: Int)
  let hide = [h | PStr h <- list "hide" sess]
      env =
        defaultLeanEnv
          { leGetEnv = \k -> pure (M.lookup k (M.fromList ([("LEAN_PROJECT_PATH", T.pack root)] <> [(k', pyStrOf v) | (k', v) <- kvs' (get "env" sess), k' /= "FISHER_LEANSEARCH_URL"])))
          , leClientEnv = defaultClientEnv {ceWire = \_ -> fakeWire fake, ceProcessId = pure 0, ceWatch = \_ _ -> pure (pure ())}
          , lePrintPrefix = \_ -> pure (Just prefix)
          , leCwd = pure root
          , leRequestTimeout = 20
          , leRgAvailable = pure ("rg" `notElem` hide)
          , leRunProcess = \_ argv _ -> case argv of
              ["lake", "build"] -> (\o -> Just (0, TE.encodeUtf8 o)) <$> readIORef canned
              ("lake" : "env" : "lean" : _) -> fail noLean
              _ -> pure (Just (0, BS.empty))
          -- the log holds the URL leansearch was asked for, not the stub's
          , leHttp = fakeHttp web webMiss
          , leNow = pure 1.0e9
          , leOs = "darwin" -- the goldens were recorded on macOS
          }
  s <- newSession env
  stopAt <- newIORef Nothing
  forM_ (zip [0 :: Int ..] (list "calls" sess)) $ \(i, step) -> do
    stopped <- readIORef stopAt
    unless (stopped /= Nothing) $ case get "op" step of
      Just (PStr "sleep") -> pure ()
      Just (PStr "write") -> do
        let p = root </> T.unpack (txt "path" step)
        createDirectoryIfMissing True (takeDirectory p)
        BS.writeFile p (TE.encodeUtf8 (txt "text" step))
      _ -> do
        let tool = txt "tool" step
            args = maybe (PDict []) id (get "args" step)
            want = maybe PNull id (get "response" step >>= get "result")
        -- lake build is fed the log the recording kept, so its filtering
        -- and the server restart around it are what is checked
        writeIORef canned (maybe "" pyStrOf (getPath ["structuredContent", "output"] want))
        case findTool tool of
          Nothing -> writeIORef stopAt (Just (i, tool <> " not ported yet"))
          Just t -> do
            r <- timeout 60000000 (runTool t s args)
            mism0 <- fakeMismatches fake
            wm <- readIORef webMiss
            writeIORef webMiss []
            let mism = mism0 <> wm
            blocked <- fakeBlockedOn fake
            case r of
              Nothing -> expectationFailure ("step " <> show i <> " " <> T.unpack tool <> " timed out; waiting for " <> show blocked <> "; mismatches " <> show mism)
              Just got -> do
                let label = "step " <> show i <> " " <> T.unpack tool <> " " <> T.unpack (dumpsUnicode args) <> (if trIsError got || T.isInfixOf "timed out" (trText got) then " (waiting for " <> show blocked <> ")" else "")
                    gotV = resultPy got
                    strip' v = PDict [(k, x) | (k, x) <- kvs v, k `elem` ["content", "isError", "structuredContent"]]
                    kvs = \case PDict xs -> xs; _ -> []
                    canon v = PDict (M.toList (M.fromList (kvs (strip' v))))
                unless (null mism) $ expectationFailure (label <> ": sent messages not in the transcript: " <> show mism)
                if T.pack noLean `T.isInfixOf` trText got
                  then modifyIORef' liveOnly (<> [i])
                  else do
                    modifyIORef' calls (+ 1)
                    -- a wrong result does not change what is sent next, so
                    -- every step is checked and each difference counted
                    unless (canon gotV == canon want) $
                      modifyIORef' diffs (<> [label <> "\n    expected: " <> T.unpack (dumpsUnicode (canon want)) <> "\n    got:      " <> T.unpack (dumpsUnicode (canon gotV))])
  ds <- readIORef diffs
  n <- readIORef calls
  unless (null ds) $
    expectationFailure (show (length ds) <> " of " <> show n <> " steps differ\n  " <> unlines (take 3 ds))
  skipped <- readIORef liveOnly
  stopped <- readIORef stopAt
  pure $ case (stopped, skipped) of
    (Just st, _) -> Just st
    (Nothing, []) -> Nothing
    (Nothing, is) -> Just (head is, "lean --profile runs only live (steps " <> T.pack (show is) <> ")")
  where
    txt k v = case get k v of Just (PStr t) -> t; _ -> ""
    kvs' = \case Just (PDict xs) -> xs; _ -> []

-- | HTTP answered from the recorded exchanges: the first unused one with
-- the same method, URL and body (and the same headers, names compared
-- without case, as urllib capitalizes them).
fakeHttp :: IORef [PyVal] -> IORef [Text] -> HttpClient
fakeHttp ref miss rq = do
  entries <- readIORef ref
  let body = maybe PNull (PStr . TE.decodeUtf8With TEE.lenientDecode) (hqBody rq)
      same e = get "method" e == Just (PStr (hqMethod rq)) && get "url" e == Just (PStr (hqUrl rq)) && maybe PNull id (get "body" e) == body
      lowerHeaders = \case Just (PDict kvs) -> M.fromList [(T.toLower k, v) | (k, v) <- kvs]; _ -> M.empty
  case break same entries of
    (pre, e : post) -> do
      writeIORef ref (pre <> post)
      let wantH = lowerHeaders (get "headers" e)
          gotH = M.fromList [(T.toLower k, PStr v) | (k, v) <- hqHeaders rq]
      if wantH /= gotH then modifyIORef' miss (<> ["headers differ for " <> hqUrl rq <> ": " <> T.pack (show gotH) <> " recorded " <> T.pack (show wantH)]) else pure ()
      if get "timeout" e /= Just (PInt (round (hqTimeout rq))) && get "timeout" e /= Just (PFloat (hqTimeout rq))
        then modifyIORef' miss (<> ["timeout differs for " <> hqUrl rq])
        else pure ()
      pure $ case get "error" e of
        Just err | get "type" err == Just (PStr "HTTPError") ->
          HttpError (maybe 0 (\case PInt n -> fromIntegral n; _ -> 0) (get "code" err)) (maybe "" pyStrOf (get "reason" err))
        Just err -> HttpFailure (maybe "" pyStrOf (get "str" err))
        Nothing ->
          HttpResponse (maybe 200 (\case PInt n -> fromIntegral n; _ -> 200) (get "status" e)) (maybe "" pyStrOf (get "reason" e))
            (TE.encodeUtf8 (maybe "" pyStrOf (get "response" e)))
    _ -> do
      modifyIORef' miss (<> ["no recorded HTTP exchange for " <> hqMethod rq <> " " <> hqUrl rq <> " " <> T.pack (show (hqBody rq))])
      pure (HttpFailure "replay: no recorded exchange")
