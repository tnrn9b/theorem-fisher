-- | Everything the credential, setup and model code does to the outside
-- world, in one record: clock, environment, HTTP, subprocesses, the
-- browser, and the terminal. Production uses 'Fisher.LLM.SysIO'; the
-- golden specs replay upstream's recorded runs through a scripted one.
module Fisher.LLM.Sys
  ( Sys (..)
  , Via (..)
  , AuthBody (..)
  , AuthReq (..)
  , AuthResp (..)
  , HttpFail (..)
  , RunResult (..)
  , getEnvT
  , sysGetEnvCompat
  , envNonEmpty
  , out
  , outLn
  , authJson
  , authText
  , authGet
  , authPostForm
  , authPostJson
  , isoNow
  , isoFromEpoch
  ) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Time (TimeZone)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

import Fisher.Compat (lookupEnvCompatWith)
import Fisher.Py.Value

-- | Which Python HTTP stack upstream used at a call site: an unreachable
-- host is a 599-like failure under httpx's mock and a URLError under
-- urllib, and the code around each reacts differently.
data Via = ViaHttpx | ViaUrllib
  deriving (Eq, Show)

data AuthBody = NoBody | FormBody [(Text, Text)] | JsonBody PyVal
  deriving (Eq, Show)

data AuthReq = AuthReq
  { arVia :: Via
  , arMethod :: Text
  , arUrl :: Text
  , arHeaders :: [(Text, Text)]
  , arBody :: AuthBody
  , arTimeout :: Double
  }
  deriving (Eq, Show)

data AuthResp = AuthResp
  { aStatus :: Int
  , aHeaders :: [(Text, Text)]
  , aBody :: BS.ByteString
  }
  deriving (Eq, Show)

-- | A request that got no HTTP answer: the exception's @str()@.
data HttpFail = HttpFail
  { hfClass :: Text
  , hfMessage :: Text
  }
  deriving (Eq, Show)

data RunResult = RunResult
  { rrCode :: Int
  , rrStdout :: Text
  , rrStderr :: Text
  }
  deriving (Eq, Show)

data Sys = Sys
  { sysNow :: IO Double
  -- ^ @time.time()@
  , sysMonotonic :: IO Double
  , sysSleep :: Double -> IO ()
  , sysGetEnv :: Text -> IO (Maybe Text)
  -- ^ @os.environ.get@ (the process environment, with the home's .env
  -- already laid over it, as upstream's CLI does at import)
  , sysSetEnv :: Text -> Text -> IO ()
  , sysHttp :: AuthReq -> IO (Either HttpFail AuthResp)
  , sysOpenBrowser :: Text -> IO Bool
  , sysRun :: [Text] -> IO (Either Text RunResult)
  -- ^ a captured subprocess; Left is FileNotFoundError's text
  , sysRunInteractive :: [Text] -> IO (Either Text Int)
  -- ^ a subprocess on the terminal (@claude setup-token@)
  , sysWhich :: Text -> IO (Maybe FilePath)
  , sysPlatform :: Text
  -- ^ @sys.platform@: "linux" or "darwin"
  , sysUuidHex :: IO Text
  , sysTimeZoneAt :: Double -> IO TimeZone
  , sysHome :: FilePath
  , sysFisherHome :: FilePath
  -- ^ the Fisher home ('Fisher.Paths.getFisherHome')
  , sysOut :: Text -> IO ()
  -- ^ stdout, as written (no newline added)
  , sysInput :: Text -> IO (Maybe Text)
  -- ^ @input(prompt)@: Nothing on EOF; Ctrl-C raises UserInterrupt
  , sysGetPass :: Text -> IO (Maybe Text)
  -- ^ @getpass(prompt)@, no echo
  , sysStdinTty :: IO Bool
  , sysStdoutTty :: IO Bool
  , sysPid :: Int
  }

getEnvT :: Sys -> Text -> IO (Maybe Text)
getEnvT = sysGetEnv

-- | 'sysGetEnv' for a renamed variable, by 'Fisher.Compat.lookupEnvCompat'
-- rule: @FISHER_X@ when set (even empty), else @GAUSS_X@. The argument is
-- @FISHER_X@, @GAUSS_X@ or the bare suffix @X@.
sysGetEnvCompat :: Sys -> Text -> IO (Maybe Text)
sysGetEnvCompat sys = lookupEnvCompatWith (sysGetEnv sys)

-- | @os.getenv(k, "").strip()@, Nothing when empty.
envNonEmpty :: Sys -> Text -> IO (Maybe Text)
envNonEmpty sys k =
  sysGetEnv sys k >>= \case
    Just v | not (T.null (T.strip v)) -> pure (Just (T.strip v))
    _ -> pure Nothing

out :: Sys -> Text -> IO ()
out = sysOut

-- | @print(x)@.
outLn :: Sys -> Text -> IO ()
outLn sys t = sysOut sys (t <> "\n")

authJson :: AuthResp -> Either String PyVal
authJson = loads . aBody

authText :: AuthResp -> Text
authText = TE.decodeUtf8With TEE.lenientDecode . aBody

authGet :: Sys -> Via -> Text -> [(Text, Text)] -> Double -> IO (Either HttpFail AuthResp)
authGet sys via url hs to = sysHttp sys (AuthReq via "GET" url hs NoBody to)

authPostForm :: Sys -> Via -> Text -> [(Text, Text)] -> [(Text, Text)] -> Double -> IO (Either HttpFail AuthResp)
authPostForm sys via url hs form to = sysHttp sys (AuthReq via "POST" url hs (FormBody form) to)

authPostJson :: Sys -> Via -> Text -> [(Text, Text)] -> PyVal -> Double -> IO (Either HttpFail AuthResp)
authPostJson sys via url hs body to = sysHttp sys (AuthReq via "POST" url hs (JsonBody body) to)

-- | @datetime.now(timezone.utc).isoformat()@: microseconds only when
-- non-zero, and a @+00:00@ suffix.
isoNow :: Sys -> IO Text
isoNow sys = isoFromEpoch <$> sysNow sys

isoFromEpoch :: Double -> Text
isoFromEpoch t =
  let whole = floor t :: Integer
      micros = round ((t - fromInteger whole) * 1e6) :: Integer
      (whole', micros') = if micros >= 1000000 then (whole + 1, micros - 1000000) else (whole, micros)
      base = T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" (posixSecondsToUTCTime (fromInteger whole')))
      frac = if micros' == 0 then "" else "." <> T.justifyRight 6 '0' (T.pack (show micros'))
   in base <> frac <> "+00:00"
