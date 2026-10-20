-- | The real 'Sys': the clock, this process's environment, HTTP over
-- http-client-tls, subprocesses, the browser, and the terminal.
module Fisher.LLM.SysIO
  ( newSysIO
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, SomeException, catch, fromException, try)
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.IO as TIO
import Data.Time (getTimeZone)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Numeric (showHex)
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Client.TLS as HT
import qualified Network.HTTP.Types as HTy
import qualified Network.HTTP.Types.URI as URI
import System.Directory (findExecutable)
import System.Environment (getEnvironment, setEnv)
import System.Exit (ExitCode (..))
import System.Info (os)
import System.IO (hFlush, hGetEcho, hIsTerminalDevice, hSetEcho, stdin, stdout)
import System.IO.Error (isEOFError)
import System.Posix.Process (getProcessID)
import System.Process (proc, readCreateProcessWithExitCode, waitForProcess, withCreateProcess)
import System.Random (randomRIO)

import Fisher.LLM.Http (userAgent)
import Fisher.LLM.Sys
import Fisher.Py.Value (dumpsUnicode)

-- | A Sys over this process. @home@ and @fisherHome@ are resolved by the
-- caller (HOME, and the Fisher home as 'Fisher.Paths.getFisherHome' finds it).
newSysIO :: FilePath -> FilePath -> IO Sys
newSysIO home fisherHome = do
  envRef <- newIORef . Map.fromList . map (\(k, v) -> (T.pack k, T.pack v)) =<< getEnvironment
  mgr <- HT.newTlsManager
  pid <- fromIntegral <$> getProcessID
  let now = realToFrac <$> getPOSIXTime
  pure
    Sys
      { sysNow = now
      , sysMonotonic = now
      , sysSleep = \s -> threadDelay (max 0 (round (s * 1e6)))
      , sysGetEnv = \k -> Map.lookup k <$> readIORef envRef
      , sysSetEnv = \k v -> do
          modifyIORef envRef (Map.insert k v)
          setEnv (T.unpack k) (T.unpack v) `catch` \(_ :: IOException) -> pure ()
      , sysHttp = httpCall mgr
      , sysOpenBrowser = \url -> do
          let opener = if os == "darwin" then "open" else "xdg-open"
          r <- try (readCreateProcessWithExitCode (proc opener [T.unpack url]) "") :: IO (Either SomeException (ExitCode, String, String))
          pure $ case r of
            Right (ExitSuccess, _, _) -> True
            _ -> False
      , sysRun = \argv -> case argv of
          [] -> pure (Left "[Errno 2] No such file or directory")
          (a0 : rest) -> do
            r <- try (readCreateProcessWithExitCode (proc (T.unpack a0) (map T.unpack rest)) "") :: IO (Either IOException (ExitCode, String, String))
            pure $ case r of
              Left _ -> Left "[Errno 2] No such file or directory"
              Right (code, o, e) -> Right (RunResult (exitInt code) (T.pack o) (T.pack e))
      , sysRunInteractive = \argv -> case argv of
          [] -> pure (Left "[Errno 2] No such file or directory")
          (a0 : rest) -> do
            r <- try (withCreateProcess (proc (T.unpack a0) (map T.unpack rest)) (\_ _ _ ph -> waitForProcess ph)) :: IO (Either IOException ExitCode)
            pure (either (const (Left "[Errno 2] No such file or directory")) (Right . exitInt) r)
      , sysWhich = findExecutable . T.unpack
      , sysPlatform = if os == "darwin" then "darwin" else T.pack os
      , sysUuidHex = T.concat <$> mapM (const hexDigit) [1 .. 32 :: Int]
      , sysTimeZoneAt = getTimeZone . posixSecondsToUTCTime . realToFrac
      , sysHome = home
      , sysFisherHome = fisherHome
      , sysOut = \t -> TIO.putStr t >> hFlush stdout
      , sysInput = \p -> do
          TIO.putStr p
          hFlush stdout
          (Just <$> TIO.getLine) `catch` \e -> if isEOFError e then pure Nothing else ioError e
      , sysGetPass = \p -> do
          TIO.putStr p
          hFlush stdout
          echo <- hGetEcho stdin `catch` \(_ :: IOException) -> pure True
          _ <- try (hSetEcho stdin False) :: IO (Either IOException ())
          v <- (Just <$> TIO.getLine) `catch` \e -> if isEOFError e then pure Nothing else ioError e
          _ <- try (hSetEcho stdin echo) :: IO (Either IOException ())
          TIO.putStr "\n"
          hFlush stdout
          pure v
      , sysStdinTty = hIsTerminalDevice stdin
      , sysStdoutTty = hIsTerminalDevice stdout
      , sysPid = pid
      }
  where
    exitInt = \case
      ExitSuccess -> 0
      ExitFailure n -> n
    hexDigit = do
      n <- randomRIO (0, 15 :: Int)
      pure (T.pack (showHex n ""))

httpCall :: H.Manager -> AuthReq -> IO (Either HttpFail AuthResp)
httpCall mgr ar = do
  r <- try $ do
    req0 <- H.parseRequest (T.unpack (arUrl ar))
    let (bodyBytes, ctype) = case arBody ar of
          NoBody -> (Nothing, Nothing)
          FormBody kvs -> (Just (URI.renderSimpleQuery False [(TE.encodeUtf8 k, TE.encodeUtf8 v) | (k, v) <- kvs]), Just "application/x-www-form-urlencoded")
          JsonBody j -> (Just (TE.encodeUtf8 (dumpsUnicode j)), Just "application/json")
        given = [(T.toLower k, v) | (k, v) <- arHeaders ar]
        hs =
          [(CI.mk (TE.encodeUtf8 k), TE.encodeUtf8 v) | (k, v) <- arHeaders ar]
            <> [("Content-Type", c) | Just c <- [ctype], "content-type" `notElem` map fst given]
            <> [("User-Agent", TE.encodeUtf8 userAgent) | "user-agent" `notElem` map fst given]
        req =
          req0
            { H.method = TE.encodeUtf8 (arMethod ar)
            , H.requestHeaders = hs
            , H.requestBody = maybe mempty H.RequestBodyBS bodyBytes
            , H.responseTimeout = H.responseTimeoutMicro (round (arTimeout ar * 1e6))
            }
    resp <- H.httpLbs req mgr
    pure
      ( AuthResp
          (HTy.statusCode (H.responseStatus resp))
          [(TE.decodeUtf8With TEE.lenientDecode (CI.original k), TE.decodeUtf8With TEE.lenientDecode v) | (k, v) <- H.responseHeaders resp]
          (BL.toStrict (H.responseBody resp)) )
  pure $ case r of
    Right a -> Right a
    Left (e :: SomeException) -> Left (classify e)
  where
    classify e = case fromException e of
      Just (H.HttpExceptionRequest _ c) -> case c of
        H.ResponseTimeout -> HttpFail (timeoutClass (arVia ar)) "timed out"
        H.ConnectionTimeout -> HttpFail (timeoutClass (arVia ar)) "timed out"
        other -> HttpFail (connectClass (arVia ar)) (wrap (T.pack (show other)))
      Just (H.InvalidUrlException u why) -> HttpFail "ValueError" (T.pack (why <> ": " <> u))
      Nothing -> HttpFail (connectClass (arVia ar)) (wrap (T.pack (show e)))
    timeoutClass = \case
      ViaHttpx -> "ReadTimeout"
      ViaUrllib -> "URLError"
    connectClass = \case
      ViaHttpx -> "ConnectError"
      ViaUrllib -> "URLError"
    wrap m = case arVia ar of
      ViaUrllib -> "<urlopen error " <> m <> ">"
      ViaHttpx -> m
