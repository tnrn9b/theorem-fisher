-- | A scripted FISHER_HOME for the read routes: state.db with three
-- sessions, their replay logs and a CLM ctx dir, plus two projects (Lean
-- by its lakefile, Rocq by its Fisher manifest).
--
--   * s1, Lean, CLM: three priced responses (0.01, 0.02 and an SSE one at
--     0.005), a tool call, the ctx files and three @clm_*@ events.
--   * s2, Rocq, ended by compression.
--   * s3, s2's compression child, opening with the compaction summary.
--
-- s1 has a step log of three changes to the Lean project's Main.lean
-- ('leanSteps'; the file on disk is the last).
module Fisher.UI.Fixture
  ( withFixture
  , Fixture (..)
  , s1, s2, s3
  , logLine
  , httpLine
  , httpLineWith
  , get
  , getJson
  , cookie
  , gate
  , leanSteps
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types
import Network.Wai (Request (..))
import Network.Wai.Test
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Fisher.Agent.Steps (Step (..), appendStep, fileHash, stepsPath)
import Fisher.Project (schemaVersion)
import Fisher.Store
import Fisher.UI.Access (Gate (..))
import Fisher.UI.Db
import Fisher.UI.Server (serve, uiApis)

data Fixture = Fixture
  { fxEnv :: UiEnv
  , fxHome :: FilePath
  , fxLean :: FilePath
  , fxRocq :: FilePath
  }

-- | The texts of s1's steps: write_file, patch, patch.
leanSteps :: [Text]
leanSteps =
  [ "theorem t : True := by\n  sorry\n"
  , "theorem t : True := by\n  skip\n  sorry\n"
  , "theorem t : True := by\n  skip\n  trivial\n" ]

s1, s2, s3 :: Text
s1 = "20261001_100000_aaaaaa"
s2 = "20261001_110000_bbbbbb"
s3 = "20261001_113000_cccccc"

-- | A replay log line as the agent writes it: @{"kind": "K", ...}@.
logLine :: Text -> [(A.Key, A.Value)] -> BL.ByteString
logLine k kvs = "{\"kind\": \"" <> BL.fromStrict (TE.encodeUtf8 k) <> "\", " <> BL.drop 1 (A.encode (A.Object (KM.fromList kvs)))

-- | An @http@ event whose response body carries @usage@ with this cost.
httpLine :: Bool -> Double -> BL.ByteString
httpLine = httpLineWith ""

-- | The same, the body after a prefix (OpenRouter's keep-alive lines).
httpLineWith :: Text -> Bool -> Double -> BL.ByteString
httpLineWith prefix sse cost =
  let usage = A.object ["prompt_tokens" A..= (100 :: Int), "completion_tokens" A..= (10 :: Int), "cost" A..= cost
                       , "prompt_tokens_details" A..= A.object ["cached_tokens" A..= (40 :: Int)]]
      doc = A.object ["choices" A..= ([] :: [A.Value]), "usage" A..= usage]
      body
        | sse = "data: {\"choices\": [{\"delta\": {\"content\": \"x\"}}]}\n\ndata: " <> enc doc <> "\n\ndata: [DONE]\n\n"
        | otherwise = prefix <> enc doc
      enc = TE.decodeUtf8 . BL.toStrict . A.encode
   in logLine "http"
        [ ("request", A.object ["method" A..= ("POST" :: Text), "url" A..= ("https://x/v1/chat/completions" :: Text)])
        , ("response", A.object ["status" A..= (200 :: Int), "headers" A..= ([] :: [A.Value]), "body" A..= body]) ]

withFixture :: (Fixture -> IO a) -> IO a
withFixture k = withSystemTempDirectory "fisher-ui" $ \tmp -> do
  let home = tmp </> "fisher"
      lean = tmp </> "projects" </> "lean"
      rocq = tmp </> "projects" </> "rocq"
  mapM_ (createDirectoryIfMissing True) [home </> "fisher" </> "replays", lean, rocq </> ".fisher", home </> "fisher" </> "ctx" </> T.unpack s1]
  B.writeFile (lean </> "lakefile.lean") "import Lake\n"
  B.writeFile (lean </> "Main.lean") (TE.encodeUtf8 (last leanSteps))
  sequence_
    [ appendStep (stepsPath home s1) (Step n ("call_s" <> T.pack (show n)) (lean </> "Main.lean") (fileHash (TE.encodeUtf8 t)) t tool (1790000001 + fromIntegral n))
    | (n, t, tool) <- zip3 [1 ..] leanSteps ["write_file", "patch", "patch"] ]
  B.writeFile (rocq </> "_CoqProject") "-R . Demo\n"
  B.writeFile (rocq </> ".fisher" </> "project.yaml") $
    TE.encodeUtf8 ("schema_version: " <> T.pack (show schemaVersion) <> "\nname: demo\nkind: rocq\nroot: .\n")
  -- state.db
  withStore (home </> "state.db") $ \st -> do
    createSessionFull st 1790000000 s1 "cli" (Just "example/model-a") Nothing Nothing
    _ <- setSessionTitle st s1 "Lean session"
    appendMessageFull st 1790000001 s1 "user" (Just "prove t") Nothing Nothing 0 Nothing Nothing
    appendMessageFull st 1790000002 s1 "assistant" (Just "") Nothing
      (Just "[{\"id\": \"call_1\", \"type\": \"function\", \"function\": {\"name\": \"read_file\", \"arguments\": \"{}\"}}]") 1 Nothing (Just "tool_calls")
    appendMessageFull st 1790000003 s1 "tool" (Just "theorem t") (Just "read_file") Nothing 0 (Just "call_1") Nothing
    appendMessageFull st 1790000004 s1 "assistant" (Just "done") Nothing Nothing 0 Nothing (Just "stop")
    endSession st 1790000005 s1 "user_exit"
    createSessionFull st 1790001000 s2 "cli" (Just "openai/gpt-5") Nothing Nothing
    appendMessageFull st 1790001001 s2 "user" (Just "prove it in Rocq") Nothing Nothing 0 Nothing Nothing
    appendMessageFull st 1790001002 s2 "assistant" (Just "working") Nothing Nothing 0 Nothing (Just "stop")
    endSession st 1790001003 s2 "compression"
    createSessionFull st 1790001004 s3 "cli" (Just "openai/gpt-5") Nothing (Just s2)
    appendMessageFull st 1790001004 s3 "user" (Just "[CONTEXT COMPACTION] Earlier turns were compacted.\nProved two lemmas.") Nothing Nothing 0 Nothing Nothing
    appendMessageFull st 1790001005 s3 "assistant" (Just "continuing") Nothing Nothing 0 Nothing (Just "stop")
  -- replay logs
  let replay sid = replayLogPath home sid
      str :: Text -> A.Value
      str = A.String
  BL.writeFile (replay s1) . BLC.unlines $
    [ logLine "agent" [("version", A.Number 1)]
    , logLine "clm" [("path", str (T.pack (ctxDirOf home s1))), ("session", str s1)]
    , logLine "turn" [("user", str "prove t")]
    , httpLine False 0.01
    , logLine "tool" [("name", str "read_file"), ("args", A.object ["path" A..= (lean </> "Main.lean")]), ("result", str "theorem t")]
    , logLine "clm_write" [("text", str "goal: t")]
    , httpLineWith "\n         \n" False 0.02
    , logLine "clm_spill" [("item", A.Number 1), ("turn", A.Number 2), ("chars", A.Number 900)]
    , httpLine True 0.005
    , logLine "clm_read" [("file", str "goal: t\nproved")]
    , logLine "result" [("result", A.object ["completed" A..= True])] ]
  BL.writeFile (replay s2) . BLC.unlines $
    [ logLine "agent" [("version", A.Number 1)]
    , logLine "tool" [("name", str "terminal"), ("args", A.object ["command" A..= ("cd " <> T.pack rocq <> "; ls")]), ("result", str "")]
    , httpLine False 0.5 ]
  -- the CLM ctx dir
  let ctx = ctxDirOf home s1
  B.writeFile (ctx </> "context.txt") "goal: t\nproved"
  B.writeFile (ctx </> "context.prev.txt") "goal: t"
  B.writeFile (ctx </> "state.json") "{\"version\": 1, \"counter\": 3, \"turns\": []}"
  env <- newUiEnv home (tmp </> "bench") (tmp </> "repo")
  k (Fixture env home lean rocq)

gate :: Gate
gate = Gate "0123456789abcdef0123456789abcdef" 8732

cookie :: Header
cookie = (hCookie, "fisher_ui_8732=0123456789abcdef0123456789abcdef")

-- | GET a path through the whole server, with the cookie.
get :: UiEnv -> B.ByteString -> IO SResponse
get env p =
  runSession
    (request (setPath defaultRequest {requestHeaderHost = Just "127.0.0.1:8732", requestHeaders = [("Host", "127.0.0.1:8732"), cookie]} p))
    (serve gate (uiApis env))

getJson :: UiEnv -> B.ByteString -> IO (Int, A.Value)
getJson env p = do
  r <- get env p
  pure (statusCode (simpleStatus r), maybe A.Null id (A.decode (simpleBody r)))
