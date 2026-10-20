-- | @fisher rlm@ (Fisher only): answer a query over a long context with a
-- recursive language model (docs/superpowers/specs/2026-10-13-rlm-design.md
-- section 5). The context is loaded into a Python REPL, the root model
-- writes code against it, and the code's model calls are served here.
--
-- Model calls go through Fisher's LLM client and are all written to a
-- replay log under @$FISHER_HOME/fisher/replays/@. @--replay LOG@ answers
-- every call from such a log instead of the network, and @--script FILE@
-- answers them from a list of replies (for tests and the cross-check).
module Fisher.Cli.Rlm
  ( rlmMain
  , rlmMainWith
  , RlmDeps (..)
  , defaultRlmDeps
  , rlmModel
  , liveModelFor
  ) where

import Control.Concurrent.MVar
import Control.Exception (IOException, SomeException, displayException, finally, try)
import Control.Monad (forM, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (toLower)
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.IO as TIO
import System.FilePath (takeExtension)
import System.IO (stderr)
import Text.Read (readMaybe)

import Fisher.Agent.Context.RlmModels (Models (..), liveModels, rlmModel)
import Fisher.Agent.Replay (closeReplayLog, logEvent, openReplayLog, recordTransport, replayPath, requestRecord)
import Fisher.Config (loadConfigAt)
import Fisher.LLM.Auth.Env (loadDotenvIntoSys)
import Fisher.LLM.Auth.Error (formatRuntimeProviderError)
import Fisher.LLM.Auth.Resolve (defaultRuntimeRequest, resolveRuntimeProvider)
import Fisher.LLM.Errors (LLMError (..))
import Fisher.LLM.Http (HttpResp (..), Runtime (..), Transport (..), systemRuntime, tlsTransport)
import Fisher.LLM.Normalize (Usage (..))
import Fisher.LLM.Sys (Sys (..), outLn)
import Fisher.LLM.Types
import Fisher.Py.Value
import Fisher.RLM.Config (RlmConfig (..), rlmConfigFromPy)
import Fisher.RLM.Log (logIteration, openTrajectory, trajectoryPath)
import Fisher.RLM.Loop (completion)
import Fisher.RLM.Parse (Message)
import Fisher.RLM.Repl (ReplFactory, ReplHandle (..), pythonRepl)
import Fisher.RLM.Types

-- | What the command takes from the running program, so the specs can
-- stand in for the network, the REPL, stdin and stderr.
data RlmDeps = RlmDeps
  { rdTransport :: IO Transport
  , rdRuntime :: Runtime
  , rdRepl :: FilePath -> Int -> ReplFactory
  -- ^ the interpreter and the block timeout in seconds
  , rdStdin :: IO BS.ByteString
  , rdErr :: Text -> IO ()
  -- ^ one line to stderr
  }

defaultRlmDeps :: RlmDeps
defaultRlmDeps = RlmDeps tlsTransport systemRuntime pythonRepl BS.getContents (TIO.hPutStrLn stderr)

rlmMain :: Sys -> [Text] -> IO Int
rlmMain = rlmMainWith defaultRlmDeps

-- ------------------------------------------------------------------ arguments

data Opts = Opts
  { oContexts :: [Text]
  , oQuery :: Maybe Text
  , oModel :: Maybe Text
  , oSubModel :: Maybe Text
  , oMaxDepth :: Maybe Int
  , oMaxIterations :: Maybe Int
  , oMaxBudget :: Maybe Double
  , oMaxTimeout :: Maybe Double
  , oMaxErrors :: Maybe Int
  , oBlockTimeout :: Maybe Int
  , oLogDir :: Maybe FilePath
  , oJson :: Bool
  , oPython :: FilePath
  , oScript :: Maybe FilePath
  , oRecordPrompts :: Maybe FilePath
  , oReplay :: Maybe FilePath
  , oHelp :: Bool
  }

noOpts :: Opts
noOpts = Opts [] Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing False "python3" Nothing Nothing Nothing False

usageLine :: Text
usageLine =
  "usage: fisher rlm [--context FILE|-]... [--query TEXT] [--model M] [--sub-model M] [--max-depth N] [--max-iterations N] [--max-budget USD] [--max-timeout S] [--max-errors N] [--block-timeout S] [--log-dir DIR] [--json] [--python PATH] [--script FILE] [--record-prompts FILE] [--replay LOG]"

helpText :: Text
helpText =
  T.unlines
    [ usageLine
    , ""
    , "Answer a query over a long context with a recursive language model (Fisher only)."
    , "The context is loaded into a Python REPL as `context`; the model writes code"
    , "against it and may call models from that code. Settings also come from"
    , "fisher.rlm in config.yaml; flags win."
    , ""
    , "Code the model writes runs as you, in a temporary directory, with your files"
    , "and network. It is not given your API key. The REPL is not a sandbox."
    , ""
    , "options:"
    , "  --context FILE|-      a context file (- reads stdin); one gives a str, several a"
    , "                        list in order; a .json file is loaded as JSON"
    , "  --query TEXT          the root prompt"
    , "  --model M             the root model"
    , "  --sub-model M         the model for sub-calls (default: the root model)"
    , "  --max-depth N         recursion depth (default 1); 0 makes one plain call with the context as the message"
    , "  --max-iterations N    turns per run (default 30)"
    , "  --max-budget USD      stop once the whole tree has cost more"
    , "  --max-timeout S       stop once the run has taken longer"
    , "  --max-errors N        stop after N consecutive turns with errors"
    , "  --block-timeout S     seconds of Python time per block (default 300)"
    , "  --log-dir DIR         write a trajectory log (JSONL) in DIR"
    , "  --json                print the result as JSON"
    , "  --python PATH         the Python interpreter (default python3; 3.9 or newer)"
    , "  --script FILE         answer model calls from a JSON list of replies, not the network"
    , "                        ({\"sub\": \"P\"} answers each sub-call with P and its prompt's"
    , "                        first 20 characters)"
    , "  --record-prompts FILE write every message list sent to a model, as JSON"
    , "  --replay LOG          answer model calls from a replay log, not the network"
    , ""
    , "Exit codes: 0 answered, 1 stopped or failed to start, 2 bad arguments, 3 no credit."
    ]

parseOpts :: [Text] -> Either Text Opts
parseOpts = go noOpts
  where
    go o = \case
      [] -> Right o {oContexts = reverse (oContexts o)}
      (f : rest)
        | f `elem` ["-h", "--help"] -> go o {oHelp = True} rest
        | f == "--json" -> go o {oJson = True} rest
        | Just set' <- lookup f valued -> case rest of
            (v : more) -> set' o v >>= \o' -> go o' more
            [] -> Left ("argument " <> f <> ": expected one argument")
        | otherwise -> Left ("unrecognized argument: " <> f)
    valued :: [(Text, Opts -> Text -> Either Text Opts)]
    valued =
      [ ("--context", \o v -> Right o {oContexts = v : oContexts o})
      , ("--query", \o v -> Right o {oQuery = Just v})
      , ("--model", \o v -> Right o {oModel = Just v})
      , ("--sub-model", \o v -> Right o {oSubModel = Just v})
      , ("--max-depth", \o v -> (\n -> o {oMaxDepth = Just n}) <$> nonNegInt "--max-depth" v)
      , ("--max-iterations", \o v -> (\n -> o {oMaxIterations = Just n}) <$> posInt "--max-iterations" v)
      , ("--max-errors", \o v -> (\n -> o {oMaxErrors = Just n}) <$> posInt "--max-errors" v)
      , ("--block-timeout", \o v -> (\n -> o {oBlockTimeout = Just n}) <$> posInt "--block-timeout" v)
      , ("--max-budget", \o v -> (\x -> o {oMaxBudget = Just x}) <$> posNum "--max-budget" v)
      , ("--max-timeout", \o v -> (\x -> o {oMaxTimeout = Just x}) <$> posNum "--max-timeout" v)
      , ("--log-dir", \o v -> Right o {oLogDir = Just (T.unpack v)})
      , ("--python", \o v -> Right o {oPython = T.unpack v})
      , ("--script", \o v -> Right o {oScript = Just (T.unpack v)})
      , ("--record-prompts", \o v -> Right o {oRecordPrompts = Just (T.unpack v)})
      , ("--replay", \o v -> Right o {oReplay = Just (T.unpack v)})
      ]
    posInt f v = case readMaybe (T.unpack v) :: Maybe Int of
      Just n | n > 0 -> Right n
      _ -> Left ("argument " <> f <> ": expected a positive integer, got '" <> v <> "'")
    -- depth 0 is the plain arm: one call with the context as the message
    nonNegInt f v = case readMaybe (T.unpack v) :: Maybe Int of
      Just n | n >= 0 -> Right n
      _ -> Left ("argument " <> f <> ": expected a non-negative integer, got '" <> v <> "'")
    posNum f v = case readMaybe (T.unpack v) :: Maybe Double of
      Just x | x > 0, not (isInfinite x) -> Right x
      _ -> Left ("argument " <> f <> ": expected a positive number, got '" <> v <> "'")

-- | Check what the parser cannot see on its own.
checkOpts :: Opts -> Either Text Opts
checkOpts o
  | null (oContexts o) = Left "at least one --context is required"
  | length (filter (== "-") (oContexts o)) > 1 = Left "argument --context: stdin can be read only once"
  | isJust (oScript o) && isJust (oReplay o) = Left "--script and --replay cannot be used together"
  | otherwise = Right o

withFlags :: Opts -> RlmConfig -> RlmConfig
withFlags o c =
  c
    { rcModel = fromMaybe (rcModel c) (oModel o)
    , rcSubModel = maybe (rcSubModel c) Just (oSubModel o)
    , rcMaxDepth = fromMaybe (rcMaxDepth c) (oMaxDepth o)
    , rcMaxIterations = fromMaybe (rcMaxIterations c) (oMaxIterations o)
    , rcMaxBudget = maybe (rcMaxBudget c) Just (oMaxBudget o)
    , rcMaxTimeout = maybe (rcMaxTimeout c) Just (oMaxTimeout o)
    , rcMaxErrors = maybe (rcMaxErrors c) Just (oMaxErrors o)
    , rcBlockTimeout = fromMaybe (rcBlockTimeout c) (oBlockTimeout o)
    }

-- ---------------------------------------------------------------------- main

rlmMainWith :: RlmDeps -> Sys -> [Text] -> IO Int
rlmMainWith deps sys argv = case parseOpts argv >>= \o -> if oHelp o then Right o else checkOpts o of
  Left e -> do
    rdErr deps usageLine
    rdErr deps ("fisher rlm: error: " <> e)
    pure 2
  Right o
    | oHelp o -> sysOut sys helpText >> pure 0
    | otherwise -> do
        -- loadConfigPy would print the warnings to stdout, ahead of --json
        (raw, warnings) <- loadConfigAt (sysFisherHome sys)
        mapM_ (rdErr deps) warnings
        let config = fromAeson raw
        case withFlags o <$> rlmConfigFromPy (get "fisher" config >>= get "rlm") of
          Left e -> rdErr deps ("fisher rlm: error: " <> e) >> pure 2
          Right cfg -> startup deps sys argv o cfg

failWith :: RlmDeps -> Text -> IO Int
failWith deps e = rdErr deps ("fisher rlm: " <> e) >> pure 1

-- | Read the contexts, check the interpreter and choose where model calls
-- go, then run.
startup :: RlmDeps -> Sys -> [Text] -> Opts -> RlmConfig -> IO Int
startup deps sys argv o cfg = do
  ctxs <- readContexts deps (oContexts o)
  case ctxs of
    Left e -> failWith deps e
    Right items -> do
      let ctx = case items of
            [one] -> one
            many -> PList many
          repl = rdRepl deps (oPython o) (rcBlockTimeout cfg)
      checked <- repl (PStr "")
      case checked of
        Left e -> failWith deps e
        Right h -> do
          rhClose h
          runId <- sysUuidHex sys
          case (oScript o, oReplay o) of
            (Just path, _) -> do
              s <- readScript path
              case s of
                Left e -> failWith deps e
                Right (queue, rule) -> do
                  models <- scriptedModels queue rule
                  run deps sys o cfg repl ctx models Nothing (pure [])
            (_, Just path) -> do
              lg <- readReplay path
              case lg of
                Left e -> failWith deps e
                Right (header, exchanges) -> do
                  (tr, misses) <- replayTransport exchanges
                  let field k = maybe "" pyStrOf (get k header)
                      base =
                        defaultSettings
                          { sMode = fromMaybe ChatCompletions (parseApiMode (field "api_mode")), sBaseUrl = field "base_url"
                          , sApiKey = "", sProvider = field "provider", sModel = rcModel cfg
                          , sSessionId = "fisher-rlm-" <> field "run_id" }
                      rt = (rdRuntime deps) {rtSleep = \_ -> pure ()}
                  run deps sys o cfg repl ctx (liveModels rt tr (llmEnv sys) cfg base) (Just (T.pack path)) misses
            _ -> do
              live <- liveSettings sys (rcModel cfg) ("fisher-rlm-" <> runId)
              case live of
                Left e -> failWith deps e
                Right base -> do
                  let mode = sMode base
                      path = replayPath (sysFisherHome sys) ("rlm_" <> runId)
                  opened <- try (openReplayLog (sysFisherHome sys) ("rlm_" <> runId))
                  case opened of
                    Left (e :: IOException) -> failWith deps ("cannot open the replay log " <> T.pack path <> ": " <> T.pack (displayException e))
                    Right lg ->
                      ( do
                          logEvent lg "rlm"
                            [ ("version", PInt 1), ("run_id", PStr runId), ("argv", PList (map PStr argv))
                            , ("config", configToPy cfg), ("provider", PStr (sProvider base)), ("api_mode", PStr (apiModeName mode))
                            , ("base_url", PStr (sBaseUrl base)) ]
                          tr <- recordTransport lg <$> rdTransport deps
                          run deps sys o cfg repl ctx (liveModels (rdRuntime deps) tr (llmEnv sys) cfg base) (Just (T.pack path)) (pure []) )
                        `finally` closeReplayLog lg

llmEnv :: Sys -> Env
llmEnv sys = Env (sysUuidHex sys) (\_ _ -> pure "[image omitted]")

-- | The settings a live call goes out with: the home's .env loaded, the
-- provider resolved as the native agent resolves it, the model and the
-- session id given. Left is the error to print.
liveSettings :: Sys -> Text -> Text -> IO (Either Text Settings)
liveSettings sys model session = do
  -- provider resolution reads config.yaml again (twice for OpenRouter) and
  -- prints the same warnings through sysOut; the command's own read has
  -- already put each on stderr once, and stdout is for the command's output
  let quiet = sys {sysOut = \_ -> pure ()}
  loadDotenvIntoSys quiet
  rt0 <- try (resolveRuntimeProvider quiet defaultRuntimeRequest)
  pure $ case rt0 of
    Left (e :: SomeException) -> Left ("could not resolve a provider: " <> formatRuntimeProviderError e)
    Right r ->
      let field k = maybe "" pyStrOf (get k r)
       in Right
            defaultSettings
              { sMode = fromMaybe ChatCompletions (parseApiMode (field "api_mode")), sBaseUrl = field "base_url"
              , sApiKey = field "api_key", sModel = model, sProvider = field "provider", sSessionId = session }

-- | A live model over Fisher's LLM client, as @fisher rlm@'s root model is
-- built but with no replay log: the given model, the provider the
-- configuration and credentials resolve to. For @fisher search --graph@.
liveModelFor :: Sys -> Text -> IO (Either Text Model)
liveModelFor sys model = do
  runId <- sysUuidHex sys
  live <- liveSettings sys model ("fisher-search-" <> runId)
  case live of
    Left e -> pure (Left e)
    Right base -> do
      tr <- tlsTransport
      pure (Right (rlmModel systemRuntime tr (llmEnv sys) base))

-- ------------------------------------------------------------------ contexts

-- | Each context as a value: a str, or the parsed JSON of a @.json@ file.
-- Bytes that are not UTF-8 are replaced, with a note on stderr.
readContexts :: RlmDeps -> [Text] -> IO (Either Text [PyVal])
readContexts deps names = sequence <$> forM names one
  where
    one name = do
      bytes <-
        if name == "-"
          then Right <$> rdStdin deps
          else either (\(e :: IOException) -> Left (name <> ": " <> T.pack (displayException e))) Right <$> try (BS.readFile (T.unpack name))
      case bytes of
        Left e -> pure (Left e)
        Right bs -> do
          let shown = if name == "-" then "<stdin>" else name
          text <- case TE.decodeUtf8' bs of
            Right t -> pure t
            Left _ -> do
              rdErr deps ("fisher rlm: " <> shown <> ": not valid UTF-8; invalid bytes were replaced")
              pure (TE.decodeUtf8With TEE.lenientDecode bs)
          pure $
            if map toLower (takeExtension (T.unpack name)) == ".json"
              then either (\e -> Left (shown <> ": not valid JSON: " <> T.pack e)) Right (loads (TE.encodeUtf8 text))
              else Right (PStr text)

-- --------------------------------------------------------------------- runs

run :: RlmDeps -> Sys -> Opts -> RlmConfig -> ReplFactory -> PyVal -> Models -> Maybe Text -> IO [Text] -> IO Int
run deps sys o cfg repl ctx models replayLog replayMisses = do
  calls <- newMVar []
  counts <- newIORef M.empty
  let defaultFor role = if role == "sub" then fromMaybe (rcModel cfg) (rcSubModel cfg) else rcModel cfg
      wrap role model override msgs = do
        modifyMVar_ calls (pure . (callRecord role (fromMaybe (defaultFor role) override) msgs :))
        r <- model override msgs
        case r of
          Right rep -> atomicModifyIORef' counts (\m -> (M.insertWith (+) (rpModel rep) (1 :: Integer) m, ()))
          Left _ -> pure ()
        pure r
  traj <- case oLogDir o of
    Nothing -> pure Nothing
    Just dir -> do
      t <- try (openTrajectory dir cfg)
      case t of
        Left (e :: IOException) -> rdErr deps ("fisher rlm: warning: cannot write the trajectory log: " <> T.pack (displayException e)) >> pure Nothing
        Right tj -> pure (Just tj)
  logging <- newIORef True
  let onIteration _ record = case traj of
        Nothing -> pure ()
        Just tj -> do
          on <- readIORef logging
          when on $ do
            w <- try (logIteration tj record)
            case w of
              Left (e :: IOException) -> do
                writeIORef logging False
                rdErr deps ("fisher rlm: warning: trajectory logging stopped: " <> T.pack (displayException e))
              Right () -> pure ()
      env =
        RlmEnv
          { reConfig = cfg
          , reRoot = wrap ("root" :: Text) (mRoot models)
          , reSub = wrap "sub" (mSub models)
          , reRepl = repl
          , reClock = sysMonotonic sys
          , reOnIteration = onIteration
          }
  result <- completion env 0 ctx (oQuery o)
  misses <- replayMisses
  case oRecordPrompts o of
    Nothing -> pure ()
    Just path -> do
      recorded <- reverse <$> readMVar calls
      w <- try (BS.writeFile path (TE.encodeUtf8 (dumpsAscii (PList recorded))))
      case w of
        Left (e :: IOException) -> rdErr deps ("fisher rlm: warning: cannot write " <> T.pack path <> ": " <> T.pack (displayException e))
        Right () -> pure ()
  n <- readIORef counts
  let logPath = T.pack . trajectoryPath <$> traj
  if oJson o
    then outLn sys (dumpsIndent False 2 (resultJson result n logPath replayLog))
    else outLn sys (rrAnswer result)
  case rrEnding result of
    Stopped why -> rdErr deps ("fisher rlm: stopped: " <> why)
    _ -> pure ()
  mapM_ (\m -> rdErr deps ("fisher rlm: " <> m)) misses
  pure $ case rrEnding result of
    Stopped "no credit" -> 3
    Stopped _ -> 1
    _ | not (null misses) -> 1
      | otherwise -> 0

-- | One call for @--record-prompts@: the role, the model it was sent to
-- (the loop's override, else the role's default) and the messages.
callRecord :: Text -> Text -> [Message] -> PyVal
callRecord role model msgs =
  PDict
    [ ("role", PStr role), ("model", PStr model)
    , ("messages", PList [PDict [("role", PStr r), ("content", PStr c)] | (r, c) <- msgs]) ]

resultJson :: RlmResult -> M.Map Text Integer -> Maybe Text -> Maybe Text -> PyVal
resultJson r counts logPath replayLog =
  PDict
    [ ("answer", PStr (rrAnswer r))
    , ("ending", PStr ending)
    , ("reason", reason)
    , ("iterations", PInt (fromIntegral (rrIterations r)))
    , ( "usage"
      , PDict
          [ ("models", PDict [(m, modelUsage m u) | (m, u) <- M.toList (rrUsage r)])
          , ("total_cost_usd", if any (isJust . uCost) (M.elems (rrUsage r)) then PFloat (sum (map (fromMaybe 0 . uCost) (M.elems (rrUsage r)))) else PNull)
          ] )
    , ("log", maybe PNull PStr logPath)
    , ("replay_log", maybe PNull PStr replayLog)
    ]
  where
    (ending, reason) = case rrEnding r of
      Answered -> ("answered", PNull)
      DefaultAnswer -> ("default_answer", PNull)
      Stopped why -> ("stopped", PStr why)
    modelUsage m u =
      PDict
        [ ("calls", PInt (M.findWithDefault 0 m counts)), ("input_tokens", PInt (uPrompt u)), ("output_tokens", PInt (uCompletion u))
        , ("cost_usd", maybe PNull PFloat (uCost u)) ]

configToPy :: RlmConfig -> PyVal
configToPy c =
  PDict
    [ ("max_depth", PInt (fromIntegral (rcMaxDepth c))), ("max_iterations", PInt (fromIntegral (rcMaxIterations c)))
    , ("max_concurrent_subcalls", PInt (fromIntegral (rcMaxConcurrent c))), ("truncate", PInt (fromIntegral (rcTruncate c)))
    , ("block_timeout", PInt (fromIntegral (rcBlockTimeout c))), ("max_budget", maybe PNull PFloat (rcMaxBudget c))
    , ("max_timeout", maybe PNull PFloat (rcMaxTimeout c)), ("max_tokens", maybe PNull PInt (rcMaxTokens c))
    , ("max_errors", maybe PNull (PInt . fromIntegral) (rcMaxErrors c)), ("model", PStr (rcModel c))
    , ("sub_model", maybe PNull PStr (rcSubModel c)) ]

-- ------------------------------------------------------------------ scripts

-- | A @--script@ file: the replies in order, and the sub-call prefix if an
-- entry @{"sub": P}@ gives one.
readScript :: FilePath -> IO (Either Text ([Text], Maybe Text))
readScript path = do
  raw <- try (BS.readFile path)
  pure $ case raw of
    Left (e :: IOException) -> Left (T.pack path <> ": " <> T.pack (displayException e))
    Right bs -> case loads bs of
      Left e -> Left (T.pack path <> ": not valid JSON: " <> T.pack e)
      Right (PList entries) -> foldr entry (Right ([], Nothing)) entries
      Right _ -> Left (T.pack path <> ": expected a JSON list")
  where
    entry v acc = acc >>= \(rs, rule) -> case v of
      PStr t -> Right (t : rs, rule)
      PDict [("sub", PStr p)]
        | isJust rule -> Left (T.pack path <> ": script: more than one sub rule")
        | otherwise -> Right (rs, Just p)
      _ -> Left (T.pack path <> ": each entry must be a string or {\"sub\": \"prefix\"}")

scriptedModels :: [Text] -> Maybe Text -> IO Models
scriptedModels replies rule = do
  queue <- newMVar replies
  let next _ _ = modifyMVar queue $ \case
        [] -> pure ([], Left (OtherError "script: out of replies"))
        r : more -> pure (more, Right (scripted r))
      sub = case rule of
        Nothing -> next
        Just p -> \_ msgs -> pure (Right (scripted (p <> T.take 20 (lastUser msgs))))
  pure (Models next sub)
  where
    scripted t = Reply t (Usage 0 0 0 Nothing Nothing (Just 0)) "scripted"
    lastUser msgs = case [c | (r, c) <- msgs, r == "user"] of
      [] -> ""
      us -> last us

-- ------------------------------------------------------------------- replay

-- | A replay log: its @rlm@ record and its HTTP exchanges.
readReplay :: FilePath -> IO (Either Text (PyVal, [(PyVal, PyVal)]))
readReplay path = do
  raw <- try (BS.readFile path)
  pure $ case raw of
    Left (e :: IOException) -> Left (T.pack path <> ": " <> T.pack (displayException e))
    Right bs ->
      let parsed = [v | l <- BC.lines bs, not (BS.null l), Right v <- [loads l]]
          kind v = maybe "" pyStrOf (get "kind" v)
          http = [(fromMaybe PNull (get "request" v >>= get "body"), fromMaybe PNull (get "response" v)) | v <- parsed, kind v == "http"]
       in case [v | v <- parsed, kind v == "rlm"] of
            [] -> Left (T.pack path <> ": no rlm record in the replay log")
            header : _ -> Right (header, http)

-- | The text every call that is not in the log fails with.
replayMiss :: Text
replayMiss = "replay: no logged response for this request"

-- | A transport answering each request from the logged exchange with the
-- same body, each used once; and the requests it could not answer.
replayTransport :: [(PyVal, PyVal)] -> IO (Transport, IO [Text])
replayTransport exchanges = do
  left <- newMVar exchanges
  missed <- newMVar []
  let answer hr = do
        let body = fromMaybe PNull (get "body" (requestRecord hr))
        found <- modifyMVar left $ \q -> case break ((== body) . fst) q of
          (pre, (_, resp) : post) -> pure (pre <> post, Just resp)
          _ -> pure (q, Nothing)
        case found of
          Just resp -> pure (fromRecord resp)
          Nothing -> do
            let what = describeRequest hr
            modifyMVar_ missed (\ms -> pure (if what `elem` ms then ms else ms <> [what]))
            pure (Left (LocalError "" replayMiss))
      tr =
        Transport
          { tRequest = \hr _ -> answer hr
          , tStream = \hr _ k ->
              answer hr >>= \case
                Left e -> pure (Left e)
                Right resp -> do
                  rest <- newIORef (rBody resp)
                  Right <$> k (rStatus resp) (rHeaders resp) (atomicModifyIORef' rest (\b -> ("", b)))
          }
  pure (tr, map ((replayMiss <> ": ") <>) <$> readMVar missed)

-- | A request, briefly: its model and the start of its last message.
describeRequest :: HttpReq -> Text
describeRequest hr =
  "request to " <> model <> " ending " <> dumpsUnicode (PStr (T.take 80 lastText))
  where
    body = fromMaybe PNull (hrBody hr)
    model = maybe "?" pyStrOf (get "model" body)
    lastText = case get "messages" body <|> get "input" body of
      Just (PList ms@(_ : _)) -> textOf (last ms)
      _ -> ""
    textOf m = case get "content" m of
      Just (PStr t) -> t
      Just (PList ps) -> T.concat [t | p <- ps, Just (PStr t) <- [get "text" p]]
      _ -> ""
    a <|> b = maybe b Just a

-- | A logged response or error, back as the transport's result. A copy of
-- Fisher.Agent.Replay's errorFromRecord (not exported) and of the response
-- decoding in its replayFile.
fromRecord :: PyVal -> Either LLMError HttpResp
fromRecord r = case get "error" r of
  Nothing ->
    Right
      ( HttpResp
          (case get "status" r of Just (PInt s) -> fromIntegral s; _ -> 200)
          (case get "headers" r of Just (PList hs) -> [(k, v) | PList [PStr k, PStr v] <- hs]; _ -> [])
          (TE.encodeUtf8 (maybe "" pyStrOf (get "body" r))) )
  Just (PStr "TimeoutError") -> Left TimeoutError
  Just (PStr "StatusError") -> Left (StatusError (case get "status" r of Just (PInt n) -> fromIntegral n; _ -> 0) (txt "message") (fromMaybe PNull (get "body" r)))
  Just (PStr "ApiError") -> Left (ApiError (txt "message"))
  Just (PStr "LocalError") -> Left (LocalError (txt "class") (txt "message"))
  _ -> Left ConnectionError
  where
    txt k = maybe "" pyStrOf (get k r)
