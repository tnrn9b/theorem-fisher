-- | 'Fisher.Agent.Aux' against @agent/auxiliary_client.py@
-- (@test/golden/b3/aux.json@): for environment and config.yaml variations,
-- what each task resolves to and the request @call_llm@ sends (or the
-- error it raises).
module Fisher.Agent.AuxSpec (spec, testSys) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (utc)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Agent.Aux
import Fisher.Agent.TestUtil
import Fisher.Agent.Types
import Fisher.LLM.Auth.PyYaml (loadConfigOrdered)
import Fisher.LLM.Errors (LLMError (..))
import Fisher.LLM.Http
import Fisher.LLM.Sys
import Fisher.LLM.Types
import Fisher.Py.Value

-- | A 'Sys' over a temporary home and a fixed environment; the parts the
-- agent never reaches fail loudly.
testSys :: FilePath -> [(T.Text, T.Text)] -> (T.Text -> IO ()) -> Sys
testSys home env write =
  Sys
    { sysNow = pure 1790000000
    , sysMonotonic = pure 1000
    , sysSleep = \_ -> pure ()
    , sysGetEnv = \k -> pure (lookup k env)
    , sysSetEnv = \_ _ -> pure ()
    , sysHttp = \_ -> pure (Left (HttpFail "ConnectError" "no network in tests"))
    , sysOpenBrowser = \_ -> pure False
    , sysRun = \_ -> pure (Left "no subprocesses in tests")
    , sysRunInteractive = \_ -> pure (Left "no subprocesses in tests")
    , sysWhich = \_ -> pure Nothing
    , sysPlatform = "linux"
    , sysUuidHex = pure "00000000000000000000000000000001"
    , sysTimeZoneAt = \_ -> pure utc
    , sysHome = home
    , sysGaussHome = home </> ".gauss"
    , sysOut = write
    , sysInput = \_ -> pure Nothing
    , sysGetPass = \_ -> pure Nothing
    , sysStdinTty = pure False
    , sysStdoutTty = pure False
    , sysPid = 4242
    }

keepHeaders :: [T.Text]
keepHeaders = ["authorization", "content-type", "http-referer", "x-openrouter-title", "x-openrouter-categories"]

reqVal :: HttpReq -> PyVal
reqVal hr =
  PDict
    [ ("method", PStr (hrMethod hr)), ("url", PStr (hrUrl hr))
    , ("headers", PDict [(k, PStr v) | (k, v) <- sortOnKey (hrHeaders hr), k `elem` keepHeaders || k == "user-agent"])
    , ("body", maybe PNull id (hrBody hr)) ]
  where
    sortOnKey = foldr insert []
    insert x [] = [x]
    insert x (y : ys) = if fst x <= fst y then x : y : ys else y : insert x ys

spec :: Spec
spec = do
  cases <- runIO (loadCases "test/golden/b3/aux.json")
  describe "agent/auxiliary_client.py" $
    forM_ (zip [1 :: Int ..] cases) $ \(i, c) -> it (show i <> " " <> T.unpack (txt "task" c)) $
      withSystemTempDirectory "fisher-aux" $ \home -> do
        let env = case field "env" c of
              PDict kvs -> [(k, pyStrOf v) | (k, v) <- kvs]
              _ -> []
            sys = testSys home (("HOME", T.pack home) : env) (\_ -> pure ())
        createDirectoryIfMissing True (sysGaussHome sys)
        case field "config" c of
          PStr y -> BS.writeFile (sysGaussHome sys </> "config.yaml") (TE.encodeUtf8 y)
          _ -> pure ()
        reqs <- newIORef []
        let ok = PDict [("id", PStr "x"), ("object", PStr "chat.completion"), ("created", PInt 0), ("model", PStr "m"), ("choices", PList [PDict [("index", PInt 0), ("message", PDict [("role", PStr "assistant"), ("content", PStr " sum ")]), ("finish_reason", PStr "stop")]]), ("usage", PDict [("prompt_tokens", PInt 1), ("completion_tokens", PInt 1), ("total_tokens", PInt 2)])]
            tr =
              Transport
                { tRequest = \hr _ -> modifyIORef reqs (<> [hr]) >> pure (Right (HttpResp 200 [] (TE.encodeUtf8 (dumpsAscii ok))))
                , tStream = \_ _ _ -> pure (Left ConnectionError) }
            rt = Runtime (pure 1790000000) (\_ -> pure ()) (pure 0)
        st <- newAuxState
        let aenv = AuxEnv sys rt tr (loadConfigOrdered sys) (\_ -> pure ()) st
            args = field "args" c
            model = case get "model" args of
              Just (PStr m) -> Just m
              _ -> Nothing
            call =
              (auxCall (txt "task" c) [PDict [("role", PStr "user"), ("content", PStr "summarize")]])
                { axModel = model
                , axTemperature = case get "temperature" args of
                    Just (PFloat t) -> Just t
                    _ -> Nothing
                , axMaxTokens = case get "max_tokens" args of
                    Just (PInt n) -> Just n
                    _ -> Nothing
                , axTools = list "tools" args }
        (p, m, b, k) <- resolveTaskProviderModel aenv (Just (txt "task" c)) Nothing model Nothing Nothing
        PList [PStr p, maybe PNull PStr m, maybe PNull PStr b, maybe PNull PStr k] `shouldBe` field "resolved" c
        r <- callLlm aenv call
        let outcome = case r of
              Right v -> PDict [("content", responseContent v)]
              Left (AuxNoProvider e) -> PDict [("raises", PStr "RuntimeError"), ("message", PStr e)]
              Left (AuxFailed e) -> PDict [("raises", PStr "Exception"), ("message", PStr e)]
        outcome `shouldBe` field "outcome" c
        rs <- readIORef reqs
        PList (map reqVal rs) `shouldBe` field "requests" c
