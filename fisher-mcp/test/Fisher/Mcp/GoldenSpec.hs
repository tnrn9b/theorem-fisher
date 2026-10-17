-- | The MCP client against upstream (mcp.json): the same scripted servers
-- (test/mcp/fake_server.py, run with python3), the same config and calls;
-- Fisher must register the same tools, return the same strings, log the
-- same lines, send the servers the same messages and ask the LLM the same
-- questions. Comparisons that cannot be exact are named:
--   * log lines, the returned names and the mcp-* toolsets are compared
--     as sets (servers connect in parallel, and join in the order they
--     finish);
--   * where upstream's text comes from httpx ("[Errno ...]"), only the part
--     before it is compared;
--   * an HTTP server's GET event stream is retried on a timer, so only its
--     presence is compared, not how often it was opened; the closing DELETE
--     races upstream's shutdown wait, so it is not compared.
module Fisher.Mcp.GoldenSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception (..), bracket, throwIO)
import Control.Monad (forM, forM_, unless, when)
import qualified Data.ByteString as BS
import Data.IORef
import Data.List (sort)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (doesFileExist, makeAbsolute)
import System.FilePath ((</>))
import System.IO
import System.IO.Temp (withSystemTempDirectory)
import System.Process
import Test.Hspec

import Fisher.Mcp.Client
import Fisher.Mcp.HttpClient (tlsHttpClient)
import Fisher.Mcp.Sampling (samplingCallDict)
import Fisher.Py.Value
import Fisher.Tools.Registry

newtype Raised = Raised Text
  deriving (Show)

instance Exception Raised where
  displayException (Raised t) = T.unpack t

load :: FilePath -> IO PyVal
load p = either fail pure . loads =<< BS.readFile p

f :: Text -> PyVal -> PyVal
f k v = maybe PNull id (get k v)

t' :: Text -> PyVal -> Text
t' k v = case get k v of
  Just (PStr s) -> s
  _ -> ""

lst :: PyVal -> [PyVal]
lst = \case
  PList xs -> xs
  _ -> []

kvs :: PyVal -> [(Text, PyVal)]
kvs = \case
  PDict x -> x
  _ -> []

subst :: [(Text, Text)] -> PyVal -> PyVal
subst table = \case
  PStr s -> PStr (foldl (\acc (k, v) -> T.replace k v acc) s table)
  PList xs -> PList (map (subst table) xs)
  PDict xs -> PDict [(k, subst table v) | (k, v) <- xs]
  x -> x

spec :: Spec
spec = do
  doc <- runIO (load "test/golden/b2/mcp.json")
  server <- runIO (makeAbsolute "test/mcp/fake_server.py")
  describe "MCP client against upstream" $
    forM_ (lst (f "cases" doc)) $ \c -> it (T.unpack (t' "name" c)) $ withSystemTempDirectory "fisher-mcp" $ \work -> do
      let scen = kvs (f "scenarios" c)
          cfgs = kvs (f "config" c)
          isHttp srv = case get srv (f "config" c) >>= get "url" of
            Just (PStr u) -> u == "<URL:" <> srv <> ">"
            _ -> False
      table0 <- fmap concat . forM scen $ \(srv, sc) -> do
        let sp = work </> ("scenario-" <> T.unpack srv <> ".json")
        BS.writeFile sp (TE.encodeUtf8 (dumpsUnicode sc))
        pure [("<SCENARIO:" <> srv <> ">", T.pack sp), ("<LOG:" <> srv <> ">", T.pack (work </> ("wire-" <> T.unpack srv <> ".log")))]
      let table1 = ("<SERVER>", T.pack server) : table0
      ports <- forM [srv | (srv, _) <- cfgs, isHttp srv] $ \srv -> do
        let port = 47000 + (sum (map fromEnum (T.unpack srv)) `mod` 900) + length (T.unpack (t' "name" c)) * 7
        pure (srv, port)
      let table = table1 <> [("<URL:" <> srv <> ">", "http://127.0.0.1:" <> T.pack (show p) <> "/mcp") | (srv, p) <- ports]
          lookupT k = maybe "" id (lookup k table)
      withHttpServers server [(T.unpack (lookupT ("<SCENARIO:" <> s <> ">")), T.unpack (lookupT ("<LOG:" <> s <> ">")), p) | (s, p) <- ports] $ do
        logs <- newIORef []
        llmSeen <- newIORef []
        canned <- newIORef (lst (f "llm_replies" c))
        reg <- newRegistry
        http <- if any (\(_, v) -> get "url" v /= Nothing) cfgs then Just <$> tlsHttpClient else pure Nothing
        let env =
              McpEnv
                { meRegistry = reg
                , meLog = \lvl msg -> when (lvl /= "DEBUG") (modifyIORef logs (<> [(lvl, msg)]))
                , meSampling = \call -> do
                    modifyIORef llmSeen (<> [samplingCallDict call])
                    rs <- readIORef canned
                    case rs of
                      (r : rest) -> do
                        writeIORef canned rest
                        case get "raise" r of
                          Just (PStr m) -> throwIO (Raised m)
                          _ -> pure r
                      [] -> throwIO (Raised "no canned reply")
                , meEnviron = pure [("HOME", T.pack work), ("PATH", "/usr/local/bin:/usr/bin:/bin"), ("SHELL", "/bin/sh"), ("TERM", "dumb")]
                , meHome = T.pack work
                , meGaussHome = work </> "gauss"
                , meHttp = http
                }
        mc <- newMcpClient env
        names <- discover mc (subst table (f "config" c))
        -- servers join the table as they finish connecting, so the order of
        -- the names (and of the mcp-* toolsets) is a race upstream too
        sort names `shouldBe` sort [s | PStr s <- lst (f "names" c)]
        forM_ (kvs (f "schemas" c)) $ \(n, want) -> do
          Just td <- lookupTool reg n
          PDict [("schema", tdSchema td), ("toolset", PStr (tdToolset td))] `shouldBe` want
        tss <- toolsets reg
        sort [(k, dumpsUnicode (PDict [("description", PStr (tsDescription ts)), ("tools", PList (map PStr (tsTools ts))), ("includes", PList [])])) | (k, ts) <- tss, "mcp-" `T.isPrefixOf` k]
          `shouldBe` sort [(k, dumpsUnicode v) | (k, v) <- kvs (f "toolsets" c)]
        [x | (k, ts) <- tss, k == "gauss-cli", x <- tsTools ts, "mcp_" `T.isPrefixOf` x] `shouldBe` [s | PStr s <- lst (f "gauss_cli_extra" c)]
        let calls = lst (f "calls" c)
            steps = [(n, a) | PList [PStr n, a] <- lst (f "steps" c)]
        results <- forM steps $ \(n, a) -> case n of
          "sleep" -> threadDelay (round (num a * 1e6)) >> pure Nothing
          "status" -> Just . PDict . (\s -> [("status", s)]) <$> mcpStatus mc (PDict [("mcp_servers", subst table (f "config" c))])
          _ -> do
            visible <- not . null <$> getDefinitions reg (S.singleton n)
            r <- dispatch reg defaultCtx n a
            pure (Just (PDict [("tool", PStr n), ("args", a), ("visible", PBool visible), ("result", PStr r)]))
        let got = [r | Just r <- results]
        length got `shouldBe` length calls
        forM_ (zip got calls) $ \(g, w) -> case get "match" w of
          Just (PStr "prefix") -> do
            let p = t' "prefix" w
            T.take (T.length p) (t' "result" g) `shouldBe` p
          _ -> g `shouldBe` PDict [(k, v) | (k, v) <- kvs w, k /= "match", k /= "prefix"]
        shutdownMcp mc
        threadDelay 300000
        gotLogs <- readIORef logs
        sort (map (\(l, m) -> (l, loosen m)) gotLogs) `shouldBe` sort [(l, loosen m) | PList [PStr l, PStr m] <- lst (f "logs" c)]
        seen <- readIORef llmSeen
        PList seen `shouldBe` f "llm" c
        forM_ scen $ \(srv, _) -> do
          let logPath = T.unpack (lookupT ("<LOG:" <> srv <> ">"))
          exists <- doesFileExist logPath
          wire <- if exists then (either fail pure . mapM (loads . TE.encodeUtf8) . filter (not . T.null) . T.lines . TE.decodeUtf8) =<< BS.readFile logPath else pure []
          let want = lst (f srv (f "wire" c))
          if isHttp srv
            then do
              -- the closing DELETE races the shutdown wait upstream, so it is
              -- not compared either
              let noGet = filter (\e -> get "http" e `notElem` [Just (PStr "GET"), Just (PStr "DELETE")])
                  gets = filter (\e -> get "http" e == Just (PStr "GET"))
              map dropHeaders (noGet wire) `shouldBe` map dropHeaders (noGet want)
              map headersOf (noGet wire) `shouldBe` map headersOf (noGet want)
              unless (null (gets want)) (map headersOf (take 1 (gets wire)) `shouldBe` map headersOf (take 1 (gets want)))
            else wire `shouldBe` want
  where
    num = \case
      PInt n -> fromIntegral n
      PFloat d -> d
      _ -> 0 :: Double
    loosen m = fst (T.breakOn "; [Errno" m)
    dropHeaders e = PDict [(k, v) | (k, v) <- kvs e, k /= "headers"]
    headersOf e = f "headers" e

withHttpServers :: FilePath -> [(FilePath, FilePath, Int)] -> IO a -> IO a
withHttpServers _ [] act = act
withHttpServers server ((scen, logp, port) : rest) act =
  bracket
    ( do
        (_, Just out, _, ph) <- createProcess (proc "python3" [server, scen, logp, "--http", show port]) {std_out = CreatePipe}
        _ <- hGetLine out
        pure ph
    )
    terminateProcess
    (const (withHttpServers server rest act))
