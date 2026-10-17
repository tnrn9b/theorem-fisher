-- | @fisher chat -q@ / @-Q@ against @gauss chat@ (@test/golden/b3/cli.json@,
-- from tools/corpus/b3_cli.py): the whole printed output, banner
-- included, the requests, the exit code and the session rows, with the HTTP answers and tool results upstream got.
module Fisher.AgentChatSpec (spec) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.IORef
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (LocalTime (..), TimeOfDay (..), fromGregorian, utc)
import Numeric (showHex)
import System.Directory
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Agent.Loop (ToolRunner (..))
import Fisher.Cli.AgentChat
import Fisher.LLM.Errors (LLMError (..))
import Fisher.LLM.Http
import Fisher.LLM.Sys
import Fisher.LLM.Types
import Fisher.Py.Value
import Fisher.Store (openStore, snapshotRows, closeStore)
import Fisher.Tools.Agent (AgentTools (..), newAgentTools)
import Fisher.Tools.Registry (getToolDefinitions)

txt :: Text -> PyVal -> Text
txt k c = maybe "" pyStrOf (get k c)

fld :: Text -> PyVal -> PyVal
fld k c = maybe PNull id (get k c)

lst :: Text -> PyVal -> [PyVal]
lst k c = case get k c of
  Just (PList xs) -> xs
  _ -> []

uuidAt :: Int -> Text
uuidAt n = T.justifyRight 32 '0' (T.pack (showHex (toInteger n * 0x1F2E3D * 2 ^ (104 :: Int)) ""))

normalizeOut :: Text -> [Text]
normalizeOut = map (last . T.splitOn "\r") . T.splitOn "\n"

-- | A masked path changes a line's width (the banner's cwd row, which the
-- spec draws with @<WORK>@ itself), so on those lines runs of spaces count
-- as one.
squashMasked :: [Text] -> [Text]
squashMasked = map (\l -> if "<WORK>" `T.isInfixOf` l then T.unwords (T.words l) else l)

-- | Fisher only: the exit summary counts each executed tool call once (its
-- tool result), where upstream also counts the assistant messages that
-- carry calls; the recorded line takes the count from the recorded rows
-- (a case without rows keeps upstream's line).
fisherToolCount :: PyVal -> Text -> Text
fisherToolCount c l = case T.breakOn " user, " l of
  (pre, rest)
    | fld "db" c /= PNull, "Messages:" `T.isPrefixOf` pre, " tool calls)" `T.isSuffixOf` rest ->
        let results = length [() | m <- lst "messages" (fld "db" c), get "role" m == Just (PStr "tool")]
         in pre <> " user, " <> T.pack (show results) <> " tool calls)"
  _ -> l

keep :: [Text]
keep = ["authorization", "content-type", "accept", "http-referer", "x-openrouter-title", "x-openrouter-categories"]

reqVal :: HttpReq -> PyVal
reqVal hr =
  PDict
    [ ("method", PStr (hrMethod hr)), ("url", PStr (hrUrl hr))
    , ("headers", PDict (sortOn fst [(k, PStr v) | (k, v) <- hrHeaders hr, k `elem` keep]))
    , ("body", maybe PNull sortTop (hrBody hr)) ]
  where
    sortTop = \case
      PDict kvs -> PDict (sortOn fst kvs)
      v -> v

goldenReq :: PyVal -> PyVal
goldenReq r =
  PDict
    [ ("method", fld "method" r), ("url", fld "url" r)
    , ("headers", case fld "headers" r of PDict kvs -> PDict (sortOn fst [kv | kv@(k, _) <- kvs, k `elem` keep]); v -> v)
    , ("body", case fld "body" r of PDict kvs -> PDict (sortOn fst kvs); v -> v) ]

spec :: Spec
spec = do
  doc <- runIO (either fail pure . loads =<< BS.readFile "test/golden/b3/cli.json")
  describe "gauss chat -q" $
    forM_ (lst "cases" doc) $ \c -> it (T.unpack (txt "name" c)) $ withSystemTempDirectory "fisher-chat" $ \tmp0 -> do
      tmp <- canonicalizePath tmp0
      let home = tmp </> "home"
          gauss = home </> ".gauss"
          work = tmp </> "work"
      createDirectoryIfMissing True gauss
      createDirectoryIfMissing True work
      offset <- newIORef (0 :: Double)
      uuidN <- newIORef (0 :: Int)
      outBuf <- newIORef []
      reqs <- newIORef []
      queue <- newIORef [r | r <- lst "requests" c, fld "method" r /= PStr "GET"]
      toolQ <- newIORef (lst "tools" c)
      env <- newIORef . M.fromList $
        [("HOME", T.pack home), ("COLUMNS", "60"), ("GAUSS_FORCE_PLAIN_OUTPUT", "1"), ("GAUSS_TIMEZONE", "UTC")]
          <> [("OPENROUTER_API_KEY", "sk-or-test-1234567890") | fld "no_key" c /= PBool True]
      let clock = (1790000000 +) <$> readIORef offset
          sleep d = modifyIORef offset (+ d)
          sys =
            Sys
              { sysNow = clock, sysMonotonic = (1000 +) <$> readIORef offset, sysSleep = sleep
              , sysGetEnv = \k -> M.lookup k <$> readIORef env
              , sysSetEnv = \k v -> modifyIORef env (M.insert k v)
              , sysHttp = \_ -> pure (Left (HttpFail "ConnectError" "no network in tests"))
              , sysOpenBrowser = \_ -> pure False, sysRun = \_ -> pure (Left "no"), sysRunInteractive = \_ -> pure (Left "no")
              , sysWhich = \_ -> pure Nothing, sysPlatform = "linux"
              , sysUuidHex = atomicModifyIORef' uuidN (\n -> (n + 1, uuidAt (n + 1)))
              , sysTimeZoneAt = \_ -> pure utc, sysHome = home, sysGaussHome = gauss
              , sysOut = \t -> modifyIORef outBuf (<> [t]), sysInput = \_ -> pure Nothing, sysGetPass = \_ -> pure Nothing
              , sysStdinTty = pure False, sysStdoutTty = pure False, sysPid = 4242 }
          models = fld "models_answer" c
          respond hr = do
            modifyIORef reqs (<> [hr])
            if hrMethod hr == "GET"
              then pure (Right (HttpResp 200 [] (TE.encodeUtf8 (dumpsAscii models))))
              else do
                q <- readIORef queue
                case q of
                  [] -> pure (Right (HttpResp 500 [] "{\"error\": {\"message\": \"no canned answer\"}}"))
                  (r : rest) -> do
                    writeIORef queue rest
                    let resp = fld "response" r
                    pure $
                      if get "connect_error" resp == Just (PBool True)
                        then Left ConnectionError
                        else Right (HttpResp (case fld "status" resp of PInt s -> fromIntegral s; _ -> 200) [] (TE.encodeUtf8 (dumpsAscii (fld "body" resp))))
      at <- newAgentTools gauss
      (defs, _) <- getToolDefinitions (atRegistry at) (Just ["gauss-cli"]) Nothing
      let scripted tr =
            tr
              { trCall = \name args _ _ -> do
                  q <- readIORef toolQ
                  case break (\t -> fld "name" t == PStr name && fld "args" t == args) q of
                    (pre, t : post) -> writeIORef toolQ (pre <> post) >> pure (txt "result" t)
                    _ -> pure "{\"error\": \"no recorded result\"}"
              , trReadFilesSummary = \_ -> pure [] }
          deps =
            ChatDeps
              { cdTransport = pure (Transport (\hr _ -> respond hr) (\_ _ _ -> pure (Left ConnectionError)))
              , cdTools = Just (defs, scripted)
              , cdNowLocal = pure (LocalTime (fromGregorian 2026 10 2) (TimeOfDay 4 50 7))
              , cdRuntime = Runtime clock sleep (pure 0)
              , cdChoice = \_ -> pure 0
              , cdAnimate = pure False
              , cdCwd = pure work
              , cdWhich = \_ -> pure Nothing
              , cdBannerInfo = pure ("<WORK>", Nothing, True) }
      code <- chatMain deps sys [s | PStr s <- lst "argv" c]
      o <- T.concat <$> readIORef outBuf
      let mask t = T.replace (T.pack gauss) "<GAUSS_HOME>" (T.replace (T.pack work) "<WORK>" t)
          got = squashMasked (normalizeOut (mask o))
          want = map (fisherToolCount c) (squashMasked (normalizeOut (txt "stdout" c)))
      got `shouldBe` want
      fromIntegral code `shouldBe` (case fld "exit" c of PInt n -> n; _ -> 0)
      rs <- readIORef reqs
      PList (map reqVal rs) `shouldBe` PList (map goldenReq (lst "requests" c))
      case fld "db" c of
        PNull -> pure ()
        db -> do
          st <- openStore (gauss </> "state.db")
          (ss, ms) <- snapshotRows st
          closeStore st
          PList (map PDict ms) `shouldBe` fld "messages" db
          map (filter ((/= "system_prompt") . fst)) ss `shouldBe` [[(k, v) | (k, v) <- kvs, k /= "system_prompt"] | PDict kvs <- lst "sessions" db]
