-- | What the Fisher UI sees of a running session, hooked onto the agent
-- loop from outside it: the step log (always on), and with a ledger the
-- events of 'Fisher.Agent.Ledger' (tool calls from the loop's callbacks,
-- cost from the transport, messages from the session store, the run's
-- end). The hooks only watch: whatever they do is out of the replay log,
-- and a failure in one never reaches the run.
module Fisher.Agent.UiHooks
  ( UiHooks
  , newUiHooks
  , toolPathResolver
  , uiHasLedger
  , uiEmit
  , uiCallbacks
  , uiTransport
  , uiSessionDB
  , uiEnd
  , uiClose
  , usageOfBody
  , endStatus
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, void)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.FilePath (isAbsolute, (</>))

import Fisher.Agent.Ledger
import Fisher.Agent.Loop (AgentResult (..), Callbacks (..))
import Fisher.Agent.Session (SessionDB (..))
import Fisher.Agent.Steps
import Fisher.LLM.Http
import Fisher.Py.Value
import Fisher.Tools.FileOps (FileOps (..), expandPath)

data UiHooks = UiHooks
  { uhLedger :: Maybe Ledger
  , uhSteps :: FilePath
  , uhResolve :: Text -> Text -> IO FilePath
  -- ^ a task id and a path argument: the file the tool wrote
  }

-- | Hooks for a session: its ledger (none: no events), its step log, and
-- how a file tool's path argument names a file (see 'toolPathResolver').
newUiHooks :: Maybe Ledger -> FilePath -> (Text -> Text -> IO FilePath) -> IO UiHooks
newUiHooks lg steps resolve =
  pure (UiHooks lg steps resolve)

-- | A path argument as the file tools take it: through the task's
-- 'FileOps' ('expandPath' for @~@, then relative to its working directory,
-- which is TERMINAL_CWD when that is set). When the task has no file
-- operations, relative to the fallback directory.
toolPathResolver :: (Text -> IO (Either Text FileOps)) -> IO FilePath -> Text -> Text -> IO FilePath
toolPathResolver opsFor fallback task p = do
  r <- opsFor task
  case r of
    Right fo -> do
      e <- T.unpack <$> expandPath fo p
      pure (if isAbsolute e then e else T.unpack (foCwd fo) </> e)
    Left _ -> do
      let s = T.unpack p
      if isAbsolute s then pure s else (</> s) <$> fallback

-- | Whether the hooks write a ledger.
uiHasLedger :: UiHooks -> Bool
uiHasLedger = isJust . uhLedger

-- | An event to the ledger, if there is one; never fails.
uiEmit :: UiHooks -> Text -> [(Text, A.Value)] -> IO ()
uiEmit h kind kvs = forM_ (uhLedger h) $ \lg ->
  void (try @SomeException (emit lg (UiEvent kind (A.object [(K.fromText k, v) | (k, v) <- kvs]))))

jstr :: Text -> A.Value
jstr = A.String

-- | The loop's callbacks with the hooks added: thinking, each call's
-- start, and each call's end (with the step log for a file change).
uiCallbacks :: UiHooks -> Callbacks -> Callbacks
uiCallbacks h cb =
  cb
    { -- kept unset when unset: the loop then runs its own spinner
      cbThinking = (\f label -> uiEmit h "thinking" [("label", jstr label)] >> f label) <$> cbThinking cb
    , cbToolProgress = Just $ \name preview args -> do
        uiEmit h "tool_start" [("name", jstr name), ("args", toAeson args)]
        forM_ (cbToolProgress cb) (\f -> f name preview args)
    , cbToolDone = Just $ \task callId name args res -> do
        let ok = either (const False) resultOk res
            out = either id id res
        uiEmit h "tool_end" [("name", jstr name), ("callId", jstr callId), ("preview", jstr (T.take 200 out)), ("ok", A.Bool ok)]
        case res of
          Right r | name `elem` ["write_file", "patch"] -> forM_ (changedPaths name args r) (logStep h task callId name)
          _ -> pure ()
        forM_ (cbToolDone cb) (\f -> f task callId name args res)
    }
  where
    resultOk r = case loads (TE.encodeUtf8 (fst (T.breakOn "\n\n[Hint:" r))) of
      Right d@(PDict _) -> maybe True (not . truthy) (get "error" d) && get "success" d /= Just (PBool False)
      _ -> not ("Error" `T.isPrefixOf` r)

-- | A changed file to the step log (and a @step@ event): read as the tool
-- left it.
logStep :: UiHooks -> Text -> Text -> Text -> Text -> IO ()
logStep h task callId tool p = void . try @SomeException $ do
  path <- uhResolve h task p
  bytes <- BS.readFile path
  at <- realToFrac <$> getPOSIXTime
  st <- appendNextStep (uhSteps h) $ \k -> Step k callId path (fileHash bytes) (TE.decodeUtf8With TEE.lenientDecode bytes) tool at
  uiEmit h "step" [("seq", A.toJSON (stSeq st)), ("path", jstr (T.pack path)), ("hash", jstr (stHash st))]

-- | The transport with a @cost@ event for each model call that reports
-- usage.
uiTransport :: UiHooks -> Transport -> Transport
uiTransport h tr =
  Transport
    { tRequest = \hr to -> do
        r <- tRequest tr hr to
        case r of
          Right resp -> report (rBody resp)
          Left _ -> pure ()
        pure r
    , tStream = \hr to k -> do
        acc <- newIORef []
        r <- tStream tr hr to $ \status hs reader ->
          k status hs (reader >>= \c -> modifyIORef acc (c :) >> pure c)
        BS.concat . reverse <$> readIORef acc >>= report
        pure r
    }
  where
    report body = forM_ (usageOfBody body) $ \(dollars, i, o) ->
      uiEmit h "cost" [("dollars", maybe A.Null A.toJSON dollars), ("input", A.toJSON i), ("output", A.toJSON o)]

-- | The usage a response body reports (a JSON reply, or a stream's
-- @data:@ lines): dollars (OpenRouter's @usage.cost@), input and output
-- tokens.
usageOfBody :: BS.ByteString -> Maybe (Maybe Double, Integer, Integer)
usageOfBody body =
  let docs = case loads body of
        Right v -> [v]
        Left _ -> [v | l <- BC.lines body, Just j <- [BS.stripPrefix "data:" l], Right v <- [loads (BC.strip j)]]
      usages = [u | d <- docs, Just u <- [usageIn d], truthy u]
      num k u = case get k u of
        Just (PInt n) -> Just n
        Just (PFloat x) -> Just (truncate x)
        _ -> Nothing
      cost u = case get "cost" u of
        Just (PFloat x) -> Just x
        Just (PInt n) -> Just (fromIntegral n)
        _ -> Nothing
      biggest ks = maximum (0 : mapMaybe (\u -> foldr (\k a -> maybe a Just (num k u)) Nothing ks) usages)
   in if null usages
        then Nothing
        else Just (lastJust (map cost usages), biggest ["prompt_tokens", "input_tokens"], biggest ["completion_tokens", "output_tokens"])
  where
    usageIn d = case get "usage" d of
      Just u -> Just u
      Nothing -> (get "message" d >>= get "usage") `orElse` (get "response" d >>= get "usage")
    orElse a b = maybe b Just a
    lastJust xs = case [x | Just x <- reverse xs] of
      (x : _) -> Just x
      [] -> Nothing

-- | The session store with a @message@ event for each message it keeps.
uiSessionDB :: UiHooks -> SessionDB -> SessionDB
uiSessionDB h db =
  db
    { dbAppendMessage = \sid m -> do
        dbAppendMessage db sid m
        uiEmit h "message" [("role", jstr (maybe "unknown" pyStrOf (get "role" m))), ("text", jstr (contentText m))]
    }
  where
    contentText m = case get "content" m of
      Just (PStr t) -> t
      Just (PList ps) -> T.concat [t | p <- ps, Just (PStr t) <- [get "text" p]]
      _ -> ""

-- | The run's end.
uiEnd :: UiHooks -> Text -> IO ()
uiEnd h status = uiEmit h "end" [("status", jstr status)]

-- | How a turn ended: completed, interrupted, failed or error.
endStatus :: Maybe AgentResult -> Text
endStatus = \case
  Just (RunDone d)
    | get "completed" d == Just (PBool True) -> "completed"
    | get "interrupted" d == Just (PBool True) -> "interrupted"
    | otherwise -> "failed"
  _ -> "error"

-- | Close the ledger, if there is one.
uiClose :: UiHooks -> IO ()
uiClose h = forM_ (uhLedger h) (void . try @SomeException . closeLedger)
