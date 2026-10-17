-- | Choosing the context strategy for a CLI session: @fisher.context@ in
-- config.yaml or @--context@. For CLM (docs/clm.md sections 3, 9 and 11)
-- the context file and its backup under @$GAUSS_HOME/fisher/ctx/<session>/@,
-- @state.json@ beside them, and the replay log's record of every write and
-- read-back; with CLM's store (docs/superpowers/specs/2026-10-14-clm-store-design.md
-- section 4) @store.json@ beside them, recall's setup, and the replay log's
-- record of each spill and archive step; for RLM (spec phase 2, sections 4
-- to 6) the interpreter check, @rlm.json@ in the same directory, and the
-- replay log's record of each offload and recall.
module Fisher.Cli.Clm
  ( ContextChoice (..)
  , contextChoice
  , validContext
  , readContextFlag
  , clmSetupFor
  , rlmSetupFor
  , ctxDir
  , liveHistory
  , ClmRefused (..)
  , RlmRefused (..)
  ) where

import Control.Exception (Exception (..), SomeException, try)
import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

import Fisher.Agent.Context.Budget (prevPath)
import Fisher.Agent.Context.Clm
import Fisher.Agent.Context.ClmStore (Store, storeFromPy, storeToPy)
import Fisher.Agent.Context.Mirror (Turn (..))
import Fisher.Agent.Context.Recall (agentRlmConfig)
import Fisher.Agent.Context.Rlm (RlmState)
import qualified Fisher.Agent.Context.Rlm as Rlm
import Fisher.Agent.Loop (RlmSetup (..))
import Fisher.Agent.Replay (ReplayLog, recordClm, recordMirror, recordRlm)
import Fisher.Atomic (atomicWriteFile)
import Fisher.LLM.Sys (Sys (..))
import Fisher.Py.Value
import Fisher.RLM.Config (RlmConfig (..))
import Fisher.RLM.Repl (checkInterpreter, pythonNotFound, pythonRepl)

-- | Why CLM cannot run here; shown as its text alone.
newtype ClmRefused = ClmRefused Text
  deriving (Show)

instance Exception ClmRefused where
  displayException (ClmRefused t) = T.unpack t

-- | Why RLM cannot run here (no usable @python3@); shown as its text alone.
newtype RlmRefused = RlmRefused Text
  deriving (Show)

instance Exception RlmRefused where
  displayException (RlmRefused t) = T.unpack t

-- | The values @--context@ and @fisher.context@ take.
validContext :: Text -> Bool
validContext = (`elem` ["compress", "clm", "rlm"])

-- | The shell's @--context@ reader (optparse's message on a bad value).
readContextFlag :: String -> Either String Text
readContextFlag v
  | validContext (T.pack v) = Right (T.pack v)
  | otherwise = Left ("invalid choice: '" <> v <> "' (choose from 'compress', 'clm', 'rlm')")

data ContextChoice = ChooseCompress | ChooseClm | ChooseRlm
  deriving (Eq, Show)

-- | The session's strategy: the flag, else the config.
contextChoice :: Maybe Text -> PyVal -> ContextChoice
contextChoice flag cfg = case flag of
  Just f -> named f
  Nothing -> case get "fisher" cfg >>= get "context" of
    Just (PStr f) -> named f
    _ -> ChooseCompress
  where
    named = \case
      "clm" -> ChooseClm
      "rlm" -> ChooseRlm
      _ -> ChooseCompress

ctxDir :: FilePath -> Text -> FilePath
ctxDir gaussHome sid = gaussHome </> "fisher" </> "ctx" </> T.unpack sid

-- | Phase 1's interpreter check on the session's PATH: python3's path, or
-- why not ('Nothing': there is no python3 at all).
pythonFor :: Sys -> IO (Either (Maybe Text) FilePath)
pythonFor sys =
  sysWhich sys "python3" >>= \case
    Nothing -> pure (Left Nothing)
    Just py -> either (Left . Just) (const (Right py)) <$> checkInterpreter py

-- | The strategy for a session (sys, where warnings go, config, session
-- id, the agent's model, the replay log), and the state it resumes from.
-- CLM keeps its file on this machine, so it needs the local terminal
-- backend. With @fisher.clm.store@ on: @store.json@ (loaded once, here),
-- and recall set up as for @--context rlm@; without a usable python3 the
-- store works without recall, with one warning.
clmSetupFor :: Sys -> (Text -> IO ()) -> PyVal -> Text -> Text -> Maybe ReplayLog -> IO (Either Text (ClmSetup, Maybe ClmState))
clmSetupFor sys warn cfg sid model lg = do
  backend <- fromMaybe "local" <$> sysGetEnv sys "TERMINAL_ENV"
  let storeOn = ccStore (clmConfigFromPy (get "fisher" cfg >>= get "clm"))
  recallCfg <- if storeOn then pure (Just <$> agentRlmConfig model cfg) else pure (Right Nothing)
  case recallCfg of
    _ | backend /= "local" -> pure (Left ("--context clm needs the local terminal backend for now (TERMINAL_ENV is " <> backend <> ")"))
    Left e -> pure (Left e)
    Right rcfg -> do
      -- the canonical spelling (macOS's /var is /private/var), so the path
      -- in the prompt is the one the model sees in tool output
      createDirectoryIfMissing True (ctxDir (sysGaussHome sys) sid)
      dir <- canonicalizePath (ctxDir (sysGaussHome sys) sid)
      let path = dir </> "context.txt"
          prev = T.unpack (prevPath (T.pack path))
          stateFile = dir </> "state.json"
          storeFile = dir </> "store.json"
          write p t = do
            createDirectoryIfMissing True dir
            atomicWriteFile ".clm_.tmp" p (TE.encodeUtf8 t)
          io0 =
            MirrorIO
              { mioPath = T.pack path
              , mioWrite = write path
              , mioRead = do
                  ex <- doesFileExist path
                  if ex then Just <$> BS.readFile path else pure Nothing
              , mioWriteBackup = write prev }
          load = do
            ex <- doesFileExist stateFile
            if not ex
              then pure Nothing
              else either (const Nothing) (>>= stateFromPy) <$> try @SomeException (either (const Nothing) Just . loads <$> BS.readFile stateFile)
      -- the store and recall, only with the store on (off: nothing read,
      -- nothing checked)
      loadedStore <- if storeOn then loadStore warn storeFile else pure Nothing
      recall <- case rcfg of
        Nothing -> pure Nothing
        Just rc ->
          pythonFor sys >>= \case
            Right py -> pure (Just (rc, pythonRepl py (rcBlockTimeout rc)))
            Left why -> do
              warn ("fisher: " <> fromMaybe "python3 not found" why <> "; CLM's store works without recall")
              pure Nothing
      failed <- newIORef False
      let saveStore s = do
            r <- try @SomeException (atomicWriteFile ".store_.tmp" storeFile (TE.encodeUtf8 (dumpsIndent True 2 (storeToPy s))))
            case r of
              Right () -> pure ()
              Left _ -> do
                first <- atomicModifyIORef' failed (\f -> (True, not f))
                when first (warn "fisher: store.json cannot be written; the store stays in memory")
          setup0 =
            ClmSetup
              { csConfig = clmConfigFromPy (get "fisher" cfg >>= get "clm")
              , csIO = maybe io0 (`recordMirror` io0) lg
              , csObserve = \_ -> pure ()
              , csSessionId = sid
              , csLoad = load
              , csSave = \st -> write stateFile (dumpsIndent True 2 (stateToPy st))
              -- loaded once, above: its warning is given once
              , csStoreLoad = pure loadedStore
              , csStoreSave = saveStore
              , csOnSpill = \_ -> pure ()
              , csOnArchive = \_ _ -> pure ()
              , csRecall = recall
              , csOnRecall = \_ _ -> pure () }
          setup = maybe setup0 (`recordClm` setup0) lg
      st <- load
      -- Fisher only: fisher.clm.skill names a file whose text replaces the
      -- default guidance after CLM's prompt section, or is none (no
      -- guidance; docs/clm.md section 11)
      let clmCfg = get "fisher" cfg >>= get "clm"
      case clmCfg >>= get "skill" of
        Just (PStr f) | T.toLower (T.strip f) `elem` ["none", ""] ->
          let c = csConfig setup in pure (Right (setup {csConfig = c {ccSkill = ""}}, st))
        Just (PBool False) ->
          let c = csConfig setup in pure (Right (setup {csConfig = c {ccSkill = ""}}, st))
        Just (PStr f) -> do
          ok <- doesFileExist (T.unpack f)
          if not ok
            then pure (Left ("fisher.clm.skill: no such file: " <> f))
            else do
              t <- TE.decodeUtf8With (\_ _ -> Just '?') <$> BS.readFile (T.unpack f)
              let c = csConfig setup
              pure (Right (setup {csConfig = c {ccSkill = T.strip t}}, st))
        _ -> pure (Right (setup, st))

-- | @store.json@: 'Nothing' when there is none; one warning, and
-- 'Nothing', when it is not a valid store.
loadStore :: (Text -> IO ()) -> FilePath -> IO (Maybe Store)
loadStore warn file = do
  ex <- doesFileExist file
  if not ex
    then pure Nothing
    else do
      raw <- try @SomeException (BS.readFile file)
      case either (const Nothing) (either (const Nothing) storeFromPy . loads) raw of
        Just s -> pure (Just s)
        Nothing -> do
          warn "fisher: store.json is not valid; resuming with an empty store"
          pure Nothing

-- | The RLM strategy for a session (sys, config, session id, the agent's
-- model, the replay log), and the state it resumes from: recall's
-- configuration, phase 1's interpreter check (its refusal on failure),
-- @rlm.json@ under the session's context directory. Recall runs on the
-- host, so any terminal backend will do.
rlmSetupFor :: Sys -> (Text -> IO ()) -> PyVal -> Text -> Text -> Maybe ReplayLog -> IO (Either Text (RlmSetup, Maybe RlmState))
rlmSetupFor sys warn cfg sid model lg = case agentRlmConfig model cfg of
  Left e -> pure (Left e)
  Right rc -> do
    checked <- either (Left . fromMaybe (pythonNotFound "python3")) Right <$> pythonFor sys
    case checked of
      Left e -> pure (Left e)
      Right py -> do
        let dir = ctxDir (sysGaussHome sys) sid
            file = dir </> "rlm.json"
        ex <- doesFileExist file
        loaded <-
          if not ex
            then pure Nothing
            else do
              raw <- try @SomeException (BS.readFile file)
              case either (const Nothing) (either (const Nothing) Rlm.stateFromPy . loads) raw of
                Just st -> pure (Just st)
                Nothing -> do
                  warn "fisher: rlm.json is not valid; resuming with an empty store"
                  pure Nothing
        let setup =
              RlmSetup
                { rlConfig = rc
                , rlRepl = pythonRepl py (rcBlockTimeout rc)
                -- loaded once, here: the warning above is given once
                , rlLoad = pure loaded
                , rlSave = \st -> do
                    createDirectoryIfMissing True dir
                    atomicWriteFile ".rlm_.tmp" file (TE.encodeUtf8 (dumpsIndent True 2 (Rlm.stateToPy st)))
                , rlOnOffload = \_ -> pure ()
                , rlOnCut = \_ -> pure ()
                , rlOnRecall = \_ _ -> pure () }
        pure (Right (maybe setup (`recordRlm` setup) lg, loaded))

-- | A resumed session's history under CLM: the pinned prefix from the
-- stored transcript, then the live context from @state.json@.
liveHistory :: [PyVal] -> ClmState -> [PyVal]
liveHistory restored st =
  let (before, rest) = break (\m -> get "role" m == Just (PStr "user")) restored
      pinned = before <> take 1 rest
   in pinned <> map turnMsg (stTurns st)
