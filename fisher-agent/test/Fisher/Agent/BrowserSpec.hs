-- | 'Fisher.Agent.Tools.Browser' against @tools/browser_tool.py@
-- (@test/golden/b4/browser.json@): each scenario's tool results, the
-- agent-browser command lines (with their PATH and socket directory),
-- Browserbase requests, summariser and vision calls, daemon kills, and the
-- files left behind.
module Fisher.Agent.BrowserSpec (spec) where

import Control.Monad (forM, forM_, when)
import qualified Data.ByteString as BS
import Data.IORef
import Data.List (isPrefixOf, sort)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Numeric (showHex)
import System.Directory
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Agent.TestUtil
import Fisher.Agent.Tools.Browser
import Fisher.Agent.Types
import Fisher.LLM.Errors (LLMError (..))
import Fisher.LLM.Http (HttpResp (..), Transport (..))
import Fisher.LLM.Types (HttpReq (..))
import Fisher.Py.Value
import Fisher.Tools.Registry

spec :: Spec
spec = do
  cases <- runIO (loadCases "test/golden/b4/browser.json")
  describe "tools/browser_tool.py" $
    forM_ cases $ \c0 -> it (T.unpack (txt "name" c0)) $
      withSystemTempDirectory "fisher-browser" $ \root -> do
        let name = T.unpack (txt "name" c0)
            gh = root </> name </> "fisher"
            sock = root </> "sock"
            unmask t = T.replace "<FISHER_HOME>" (T.pack gh) (T.replace "<ROOT>" (T.pack root) (T.replace "<SOCK>" (T.pack sock) (T.replace "<NODE_BIN>" (T.pack (gh </> "node" </> "bin")) t)))
            deep = \case
              PStr s -> PStr (unmask s)
              PList xs -> PList (map deep xs)
              PDict kvs -> PDict [(k, deep v) | (k, v) <- kvs]
              v -> v
            c = deep c0
            extra = field "extra" c
        createDirectoryIfMissing True gh
        createDirectoryIfMissing True sock
        runQ <- newIORef (list "runs_answers" c)
        httpQ <- newIORef (list "http_answers" c)
        llmQ <- newIORef (list "llm_answers" c)
        runs <- newIORef []
        https <- newIORef []
        llms <- newIORef []
        kills <- newIORef []
        uuidN <- newIORef (0 :: Integer)
        let next q = atomicModifyIORef' q (\case [] -> ([], Nothing); (x : xs) -> (xs, Just x))
            run argv envs timeout = do
              modifyIORef runs (<> [PDict [("argv", PList (map PStr argv)), ("timeout", PInt (fromIntegral timeout)), ("socket_dir", maybe PNull PStr (lookup "AGENT_BROWSER_SOCKET_DIR" envs)), ("path", maybe PNull PStr (lookup "PATH" envs))]])
              a <- next runQ
              case a of
                Nothing -> pure (Ran 0 "{\"success\": true, \"data\": {}}" "")
                Just ans
                  | get "timeout" ans == Just (PBool True) -> pure TimedOut
                  | Just (PStr m) <- get "raise" ans -> pure (RunFailed m)
                  | otherwise -> do
                      when (get "write_last" ans == Just (PBool True)) $ BS.writeFile (T.unpack (last argv)) "\x89PNG fake"
                      forM_ (get "write" ans) $ \w -> BS.writeFile (T.unpack (pyStrOf w)) "\x89PNG other"
                      let stdout = case get "stdout" ans of
                            Just (PStr s) -> s
                            _ -> maybe "" dumpsAscii (get "json" ans)
                      pure (Ran (fromIntegral (int "rc" ans)) stdout (txt "stderr" ans))
            whichT = field "which" c
            envT = PDict ([(k, v) | (k, v) <- case field "env" c of PDict kvs -> kvs; _ -> []] <> [("PATH", PStr "/opt/homebrew/bin:/usr/bin"), ("FISHER_HOME", PStr (T.pack gh))])
            tr =
              Transport
                { tRequest = \hr _ -> do
                    modifyIORef https (<> [PDict [("url", PStr (hrUrl hr)), ("headers", PDict [(k, PStr v) | (k, v) <- hrHeaders hr]), ("body", PStr (maybe "" dumpsAscii (hrBody hr)))]])
                    a <- next httpQ
                    pure $ case a of
                      Nothing -> Right (HttpResp 200 [] "{}")
                      Just ans -> case get "raise" ans of
                        Just (PStr m) -> Left (ApiError m)
                        _ ->
                          let body = case get "body" ans of
                                Just (PStr b) -> b
                                _ -> dumpsAscii (maybe (PDict []) id (get "json" ans))
                           in Right (HttpResp (case get "status" ans of Just (PInt s) -> fromIntegral s; _ -> 200) [("Content-Type", "application/json")] (TE.encodeUtf8 body))
                , tStream = \_ _ _ -> pure (Left ConnectionError) }
            aux call = do
              modifyIORef llms (<> [call])
              a <- next llmQ
              pure $ case a of
                Nothing -> Left (AuxFailed "no scripted answer")
                Just ans -> case get "raise" ans of
                  Just (PStr "RuntimeError") -> Left (AuxNoProvider "No auxiliary provider")
                  Just (PStr m) -> Left (AuxFailed m)
                  _ -> Right (PDict [("choices", PList [PDict [("message", PDict [("content", field "content" ans)])]])])
            uuid = do
              modifyIORef uuidN (+ 1)
              n <- readIORef uuidN
              pure (T.justifyRight 32 '0' (T.pack (showHex (n * 0x1F2E3D * 2 ^ (104 :: Int)) "")))
        env <-
          newBrowserEnv run
            (\n -> pure (case get n whichT of Just (PStr p) -> Just p; _ -> Nothing))
            (\k -> pure (case get k envT of Just (PStr v) -> Just v; _ -> Nothing))
            tr aux (pure (get "interrupted" extra == Just (PBool True))) uuid gh sock
            (\p -> pure (p `elem` ["/usr/local/bin", "/usr/bin", "/bin", "/usr/sbin"]))
            (\pid -> modifyIORef kills (<> [PList [PInt (fromIntegral pid), PInt 15]]))
            (pure 1790000000) (pure "20261002_045007") (pure (get "record" extra == Just (PBool True)))
        reg <- newRegistry
        registerBrowserTools reg env
        results <- forM (list "calls" c) $ \call -> do
          let ctx = field "ctx" call
              tool = txt "tool" call
          when (tool == "browser_close" && has "pid" extra) $ do
            ds <- listDirectory sock
            forM_ [d | d <- ds, "agent-browser-" `isPrefixOf` d] $ \d ->
              writeFile (sock </> d </> (drop (length ("agent-browser-" :: String)) d <> ".pid")) (show (int "pid" extra) <> "\n")
          dispatch reg (ToolCtx (txt "task_id" ctx) (case get "user_task" ctx of Just (PStr u) -> Just u; _ -> Nothing) Nothing) tool (field "args" call)
        avail <- checkBrowserRequirements env
        map PStr results `shouldBe` list "results" c
        rs <- readIORef runs
        PList rs `shouldBe` PList [PDict [(k, field k r) | k <- ["argv", "timeout", "socket_dir", "path"]] | r <- list "runs" c]
        hs <- readIORef https
        PList hs `shouldBe` PList [PDict [(k, field k h) | k <- ["url", "headers", "body"]] | h <- list "http" c]
        ls <- readIORef llms
        let shown call =
              PDict . sort' $
                [ ("task", maybe PNull PStr (axTask call)), ("max_tokens", maybe PNull PInt (axMaxTokens call))
                , ("temperature", maybe PNull PFloat (axTemperature call))
                , ("messages", PList [PDict [("role", field "role" m), ("content", flat (field "content" m))] | m <- axMessages call]) ]
                  <> [("model", PStr m) | Just m <- [axModel call]]
            flat = \case
              PList parts -> PList [if txt "type" p == "image_url" then PDict [("type", PStr "image_url"), ("url", field "url" (field "image_url" p))] else PDict [("type", PStr "text"), ("text", field "text" p)] | p <- parts]
              v -> v
            sort' = sortOnFst
            recorded r = PDict (sortOnFst (case r of PDict kvs -> kvs; _ -> []))
        PList (map shown ls) `shouldBe` PList (map recorded (list "llm_calls" c))
        ks <- readIORef kills
        PList ks `shouldBe` field "kills" c
        avail `shouldBe` (field "available" c == PBool True)
        left <- sort . filter ("agent-browser-" `isPrefixOf`) <$> listDirectory sock
        PList (map (PStr . T.pack) left) `shouldBe` field "socket_dirs_left" c
        shotsDir <- doesDirectoryExist (gh </> "browser_screenshots")
        shots <- if shotsDir then sort <$> listDirectory (gh </> "browser_screenshots") else pure []
        PList (map (PStr . T.pack) shots) `shouldBe` field "screenshots" c
  where
    sortOnFst kvs = [(k, v) | k <- sort (map fst kvs), Just v <- [lookup k kvs]]
