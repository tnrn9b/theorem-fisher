-- | The proof canvas (U3): a session's step log, each step's check
-- verdict, and a hand edit handed back to the agent.
--
--   * @GET /api/sessions/:id/steps@: @[{seq, path, hash, tool, at}]@, in
--     number order (none when the session has no step log).
--   * @GET /api/sessions/:id/steps/:seq@: that step with its @text@ (the
--     whole file as the step left it) and its @verdict@, null until a
--     check has run.
--   * @POST /api/sessions/:id/steps/:seq/verdict@: the step's verdict
--     @{errors, holes, ok, prover, hash}@, computed once and then served
--     from @steps/<session>.verdicts.json@ beside the log. It is computed
--     in a copy of the project (in @$GAUSS_HOME/fisher/ui-check@, removed
--     after), never in the project: Lean by fisher-search's 'probe' in a
--     'makeWorkspace' copy (the project's build cloned, its packages
--     shared), Rocq and Isabelle by @fisher check FILE@ in a plain copy.
--     @holes@ counts the prover's hole markers in the text; @ok@ is no
--     error and no hole. One check runs at a time.
--   * @POST /api/sessions/:id/handback@ @{path, text}@: writes the text to
--     the file (one the session's steps changed), appends a step with
--     tool @hand@, computes its verdict, then tells the agent: through the
--     inbox when a run of this server drives the session or it runs in
--     another process (its run lock is held: @elsewhere@ is that pid),
--     else by resuming it (@fisher chat -c <latest session of the chain> -p <message>@, its
--     model kept, the ledger on) as a run of this server.
--
-- Like the live routes, every route maps the id to its chain's root
-- session first (the agent keys the step log on it).
module Fisher.UI.Api.Steps
  ( stepsApi
  , StepVerdict (..)
  , verdictFor
  , handMessage
  , verdictsPath
  ) where

import Control.Concurrent.MVar
import Control.Exception (SomeException, finally, try)
import Control.Monad (forM_)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as AT
import Data.Foldable (toList)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef (atomicModifyIORef')
import Data.List (intercalate, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Types
import Network.Wai
import System.Directory
import System.Exit (ExitCode (..))
import System.FilePath (isAbsolute, makeRelative, splitDirectories, takeBaseName, takeDirectory, takeExtension, takeFileName, (</>))
import System.Info (os)
import System.Process (proc, readCreateProcessWithExitCode)
import System.Timeout (timeout)
import Text.Read (readMaybe)

import Fisher.Agent.Inbox (appendInbox, inboxPath)
import Fisher.Agent.Steps
import Fisher.Prover (ProverKind (..), findProverRoot, isProverRoot)
import Fisher.Prover.Holes (Hole (..), holes)
import Fisher.Search.Check (Probe (..), lakeLeanRunWith, probe)
import Fisher.Search.Workspace (makeWorkspace)
import Fisher.UI.Api.Live
import Fisher.UI.Children (newTag, spawnChild, stopChildWith)
import Fisher.UI.Db
import Fisher.UI.Diff (unifiedDiff)
import Fisher.UI.Files (contained, isUnder, writeOver)
import Fisher.UI.Response (Api, jsonResponse)

data StepVerdict = StepVerdict
  { svErrors :: [Text]
  , svHoles :: Int
  , svOk :: Bool
  , svProver :: Text
  , svHash :: Text
  -- ^ the step's hash the verdict is for
  }
  deriving (Eq, Show)

instance A.ToJSON StepVerdict where
  toJSON v =
    A.object
      [ "errors" A..= svErrors v, "holes" A..= svHoles v, "ok" A..= svOk v
      , "prover" A..= svProver v, "hash" A..= svHash v ]

instance A.FromJSON StepVerdict where
  parseJSON = A.withObject "verdict" $ \o ->
    StepVerdict <$> o A..: "errors" <*> o A..: "holes" <*> o A..: "ok" <*> o A..: "prover" <*> o A..: "hash"

stepsApi :: LiveEnv -> Api
stepsApi env r = case (requestMethod r, pathInfo r) of
  (m, ["api", "sessions", sid, "steps"]) | m == methodGet -> Just <$> guarded sid (listSteps env sid)
  (m, ["api", "sessions", sid, "steps", n]) | m == methodGet -> Just <$> guarded sid (withSeq n (oneStep env sid))
  (m, ["api", "sessions", sid, "steps", n, "verdict"]) | m == methodPost -> Just <$> guarded sid (withSeq n (checkStep env sid))
  (m, ["api", "sessions", sid, "handback"]) | m == methodPost -> Just <$> guarded sid (handBack env r sid)
  _ -> pure Nothing
  where
    guarded i k = if safeId i then k else pure (bad badRequest400 "bad id")
    withSeq n k = case readMaybe (T.unpack n) of
      Just (k' :: Int) | k' > 0 -> k k'
      _ -> pure (bad badRequest400 "bad step number")

home :: LiveEnv -> FilePath
home = ueHome . leUi

stepsOf :: LiveEnv -> Text -> IO [Step]
stepsOf env root = sortOn stSeq <$> cachedSteps env (stepsPath (home env) root)

stepHead :: Step -> [AT.Pair]
stepHead s =
  ["seq" A..= stSeq s, "path" A..= stPath s, "hash" A..= stHash s, "tool" A..= stTool s, "at" A..= stAt s]

listSteps :: LiveEnv -> Text -> IO Response
listSteps env sid = do
  root <- rootOf env sid
  ss <- stepsOf env root
  pure (jsonResponse ok200 [A.object (stepHead s) | s <- ss])

findStep :: LiveEnv -> Text -> Int -> IO (Text, Maybe Step)
findStep env sid n = do
  root <- rootOf env sid
  ss <- stepsOf env root
  pure (root, listToMaybe [s | s <- ss, stSeq s == n])

oneStep :: LiveEnv -> Text -> Int -> IO Response
oneStep env sid n =
  findStep env sid n >>= \case
    (_, Nothing) -> pure (bad notFound404 "no such step")
    (root, Just s) -> do
      v <- cached env root s
      pure (jsonResponse ok200 (A.object (stepHead s <> ["text" A..= stText s, "verdict" A..= v])))

checkStep :: LiveEnv -> Text -> Int -> IO Response
checkStep env sid n =
  findStep env sid n >>= \case
    (_, Nothing) -> pure (bad notFound404 "no such step")
    (root, Just s) ->
      cached env root s >>= \case
        Just v -> pure (jsonResponse ok200 v)
        Nothing -> do
          -- at most 'leCheckQueue' checks wait behind the one running
          admitted <- atomicModifyIORef' (leCheckWaiting env) $ \n ->
            if n > leCheckQueue env then (n, False) else (n + 1, True)
          if not admitted
            then pure (bad serviceUnavailable503 "too many checks are waiting; try again when they are done")
            else jsonResponse ok200 <$> (verdictFor env root s `finally` atomicModifyIORef' (leCheckWaiting env) (\n -> (n - 1, ())))

-- ------------------------------------------------------------- verdicts

verdictsPath :: FilePath -> Text -> FilePath
verdictsPath h root = stepsDir h </> T.unpack (root <> ".verdicts.json")

readVerdicts :: LiveEnv -> Text -> IO (M.Map Text StepVerdict)
readVerdicts env root = do
  r <- try @SomeException (BS.readFile (verdictsPath (home env) root))
  pure $ case r of
    Right b | Just m <- A.decodeStrict b -> m
    _ -> M.empty

-- | The step's verdict when it has been computed (for this hash).
cached :: LiveEnv -> Text -> Step -> IO (Maybe StepVerdict)
cached env root s = do
  m <- readVerdicts env root
  pure $ case M.lookup (T.pack (show (stSeq s))) m of
    Just v | svHash v == stHash s -> Just v
    _ -> Nothing

-- | The step's verdict: from the cache, else computed (one check at a
-- time) and cached.
verdictFor :: LiveEnv -> Text -> Step -> IO StepVerdict
verdictFor env root s =
  cached env root s >>= \case
    Just v -> pure v
    Nothing -> withMVar (leCheck env) $ \_ ->
      -- a check that waited for this one may find it done
      cached env root s >>= \case
        Just v -> pure v
        Nothing -> do
          v <- either (\(e :: SomeException) -> StepVerdict ["the check could not run: " <> T.pack (show e)] (length (holesIn s)) False (proverOf s) (stHash s)) id
            <$> try (compute env root s)
          m <- readVerdicts env root
          let p = verdictsPath (home env) root
              tmp = p <> ".tmp"
          createDirectoryIfMissing True (takeDirectory p)
          BL.writeFile tmp (A.encode (M.insert (T.pack (show (stSeq s))) v m))
          renameFile tmp p
          pure v

kindOf :: FilePath -> Maybe ProverKind
kindOf p = case takeExtension p of
  ".lean" -> Just Lean4
  ".v" -> Just Rocq
  ".thy" -> Just Isabelle
  _ -> Nothing

proverOf :: Step -> Text
proverOf s = case kindOf (stPath s) of
  Just Lean4 -> "lean4"
  Just Rocq -> "rocq"
  Just Isabelle -> "isabelle"
  Nothing -> "unknown"

holesIn :: Step -> [()]
holesIn s = maybe [] (\k -> () <$ holes k (stText s)) (kindOf (stPath s))

compute :: LiveEnv -> Text -> Step -> IO StepVerdict
compute env root s = do
  let path = stPath s
      nHoles = length (holesIn s)
      verdict errs = StepVerdict errs nHoles (null errs && nHoles == 0) (proverOf s) (stHash s)
  case kindOf path of
    Nothing -> pure (verdict ["no prover checks a " <> T.pack (takeExtension path) <> " file"])
    Just kind ->
      findProverRoot kind (takeDirectory path) >>= \case
        Nothing -> pure (verdict ["no " <> proverOf s <> " project at or above " <> T.pack path])
        Just proj0 -> do
         proj <- canonicalizePath proj0
         inside <- contained proj path
         let rel0 = makeRelative proj path
         if not inside || isAbsolute rel0 || ".." `elem` splitDirectories rel0
          then pure (verdict ["the file is not inside its project (or is a link out of it): " <> T.pack path])
          else do
          tag <- newTag
          let dest = home env </> "fisher" </> "ui-check" </> T.unpack (root <> "-" <> T.pack (show (stSeq s)) <> "-" <> tag)
              rel = rel0
              clean t = T.replace (T.pack (dest <> "/")) "" t
          createDirectoryIfMissing True (takeDirectory dest)
          errs <- (`finally` removeCopy dest) $ case kind of
            Lean4 -> do
              makeWorkspace proj dest rel (stText s)
              pr <- probe (lakeLeanRunWith (leLake env) (leCheckSecs env)) dest rel (stText s) []
              let probeName = T.pack ("_fisher_search_" <> takeBaseName rel <> ".lean")
                  tidy = T.replace probeName (T.pack (takeBaseName rel <> ".lean")) . clean
                  errs = map tidy (prErrors pr)
              pure $
                if null errs && prExit pr /= 0
                  then ["lean exited " <> T.pack (show (prExit pr)) <> ": " <> tidy (T.takeEnd 300 (prOutput pr))]
                  else errs
            _ -> do
              copyProject proj dest
              -- the copy's own links could lead out of it: the file's
              -- directory must be inside the copy, and a link at the file
              -- itself goes before the text is written
              okDir <- isUnder dest (takeDirectory (dest </> rel))
              if not okDir then ioError (userError "the file's directory in the copy is a link out of it") else pure ()
              _ <- try @SomeException (removeFile (dest </> rel))
              BS.writeFile (dest </> rel) (TE.encodeUtf8 (stText s))
              (code, out) <- runCheck env dest rel
              let ls = map clean (T.lines out)
                  holeDecls = [d | Just d <- map hDecl (holes kind (stText s))]
                  failsOf l = case T.stripPrefix "FAIL " l of
                    Just rest ->
                      let label = fst (T.breakOn ": " rest)
                          decl = snd (T.breakOnEnd ":" label)
                       in [l | decl `notElem` holeDecls]
                    Nothing -> []
                  fails = concatMap failsOf ls
              pure $ case code of
                ExitSuccess -> []
                ExitFailure 1 -> fails
                ExitFailure n -> fails <> ["fisher check exited " <> T.pack (show n) <> ": " <> T.takeEnd 300 (T.unlines ls)]
          pure (verdict errs)

-- | Remove a check's copy (a Lean copy's shared packages are a link: it
-- goes, what it names stays).
removeCopy :: FilePath -> IO ()
removeCopy dest = do
  _ <- try @SomeException (removeDirectoryLink (dest </> ".lake" </> "packages"))
  _ <- try @SomeException (removeDirectoryRecursive dest)
  pure ()

-- | The project's files but @.git@ in a new directory: @cp -cR@ (APFS
-- clones) on macOS, @cp -R --reflink=auto@ elsewhere.
copyProject :: FilePath -> FilePath -> IO ()
copyProject from to = do
  createDirectoryIfMissing True to
  entries <- listDirectory from
  forM_ [e | e <- entries, e /= ".git"] $ \e -> do
    let args = if os == "darwin" then ["-cR", from </> e, to </> e] else ["-R", "--reflink=auto", from </> e, to </> e]
    (code, _, err) <- readCreateProcessWithExitCode (proc "cp" args) ""
    case code of
      ExitSuccess -> pure ()
      ExitFailure _ -> ioError (userError ("cp " <> unwords args <> ": " <> err))

-- | @fisher check REL@ in the copy, its process tree stopped at the time
-- limit: the exit code and the output.
runCheck :: LiveEnv -> FilePath -> FilePath -> IO (ExitCode, Text)
runCheck env dir rel = do
  let logp = dir <> ".log"
  (ch, exited) <- spawnChild (Just logp) dir [leFisher env, "check", rel] [("GAUSS_HOME", home env)]
  r <- timeout (leCheckSecs env * 1000000) (readMVar exited)
  code <- case r of
    Just c -> pure c
    Nothing -> stopChildWith (leGrace env) ch >> pure (ExitFailure 124)
  out <- either (\(_ :: SomeException) -> "") (TE.decodeUtf8With TEE.lenientDecode) <$> try (BS.readFile logp)
  _ <- try @SomeException (removeFile logp)
  pure (code, if code == ExitFailure 124 then "timed out after " <> T.pack (show (leCheckSecs env)) <> " s\n" <> out else out)

-- ------------------------------------------------------------ hand back

-- | What the agent is told about a hand edit: the diff, or when it is
-- longer than 20,000 characters, that the file changed substantially.
handMessage :: FilePath -> Text -> Text
handMessage path diff
  | T.length diff > 20000 =
      "The user edited " <> T.pack path <> " by hand. The change: the file changed substantially; read it. Continue from the file as it is now."
  | otherwise =
      "The user edited " <> T.pack path <> " by hand. The change:\n" <> T.dropWhileEnd (== '\n') diff <> "\nContinue from the file as it is now."

-- | The chat event the session's agent recorded in the ledger (its
-- flags and variables), the latest one.
chatEvent :: LiveEnv -> Text -> IO (Maybe A.Object)
chatEvent env root = do
  evs <- readDb env [] (\c -> ledgerEvents c root 0)
  pure (listToMaybe (reverse [o | (_, "chat", A.Object o) <- evs]))

-- | The session's project root: the project of this server's run for it,
-- else the one its chat recorded (GAUSS_PROJECT_ROOT, else the prover's
-- project variable), else the prover root at or above the file's
-- directory (walking up the path as written, links not followed);
-- canonical.
projectRootFor :: LiveEnv -> Text -> FilePath -> IO (Maybe FilePath)
projectRootFor env root path = do
  run <- runFor env root
  chat <- chatEvent env root
  let recorded = case chat >>= KM.lookup "env" of
        Just (A.Object o) ->
          listToMaybe [T.unpack t | k <- ["GAUSS_PROJECT_ROOT", "LEAN_PROJECT_PATH", "ROCQ_PROJECT_PATH", "ISABELLE_PROJECT_PATH"], Just (A.String t) <- [KM.lookup k o], not (T.null t)]
        _ -> Nothing
  found <- case (rnProject <$> run, recorded) of
    (Just p, _) -> pure (Just p)
    (_, Just p) -> pure (Just p)
    _ -> case kindOf path of
      Nothing -> pure Nothing
      Just k -> walk k (takeDirectory path)
  case found of
    Nothing -> pure Nothing
    Just p -> either (\(_ :: SomeException) -> Nothing) Just <$> try (canonicalizePath p)
  where
    walk k d = do
      yes <- isProverRoot k d
      if yes then pure (Just d) else let up = takeDirectory d in if up == d then pure Nothing else walk k up

handBack :: LiveEnv -> Request -> Text -> IO Response
handBack env r sid = do
  body <- jsonBodyWith (16 * 1024 * 1024) r
  case (body >>= textField "path", body >>= textField "text", body >>= textField "base") of
    (Just p, Just text, Just base)
      | not (isAbsolute (T.unpack p)) -> pure (bad badRequest400 "the path is not absolute")
      | otherwise -> do
          root <- rootOf env sid
          let path = T.unpack p
          known <- any ((== path) . stPath) <$> stepsOf env root
          proj <- projectRootFor env root path
          inside <- maybe (pure False) (`contained` path) proj
          case proj of
            _ | not known -> pure (bad badRequest400 "the session's steps never changed this file")
            Nothing -> pure (bad badRequest400 "no project is known for this session's file")
            Just pr
              | not inside -> pure (bad badRequest400 "the file is not inside the session's project, or is a link")
              | otherwise -> handIn env root pr path base text
    _ -> pure (bad badRequest400 "send {path, text, base} as JSON (base: the hash of the step the edit started from)")

handIn :: LiveEnv -> Text -> FilePath -> FilePath -> Text -> Text -> IO Response
handIn env root proj path base text = do
  written <- withMVar (leHand env) $ \_ -> do
    oldBytes <- either (\(_ :: SomeException) -> "") id <$> try (BS.readFile path)
    let old = TE.decodeUtf8With TEE.lenientDecode oldBytes
    if fileHash oldBytes /= base
      then pure (Left (conflict409, "the file changed since the step the edit started from; reload it"))
      else if old == text
        then pure (Left (badRequest400, "the file already has this text"))
        else do
          let bytes = TE.encodeUtf8 text
          writeOver path bytes
          at <- realToFrac <$> getPOSIXTime
          st <- appendNextStep (stepsPath (home env) root) $ \n -> Step n ("hand_" <> T.pack (show n)) path (fileHash bytes) text "hand" at
          pure (Right (st, old))
  case written of
    Left (code, e) -> pure (bad code e)
    Right (st, old) -> do
      v <- verdictFor env root st
      let msg = handMessage path (unifiedDiff path old text)
          toInbox = withMVar (leInbox env) $ \_ -> appendInbox (inboxPath (home env) root) msg
      run <- runFor env root
      alive <- maybe (pure False) runAlive run
      -- a session that runs in another process (its run lock is held) is
      -- told through its inbox, never resumed beside itself
      other <- if alive then pure Nothing else elsewhere env root
      delivered <-
        if alive || isJust other
          then toInbox >> pure (Right ("inbox", Nothing, 0))
          else resume env root proj msg >>= \case
            Right x -> pure (Right x)
            -- kept for the session's next run, which reads its inbox
            Left e -> toInbox >> pure (Left e)
      pure . jsonResponse ok200 . A.object $
        [ "seq" A..= stSeq st, "verdict" A..= v, "message" A..= msg, "elsewhere" A..= other ]
          <> case delivered of
            Right (how, tag, after) -> ["delivered" A..= (how :: Text), "tag" A..= (tag :: Maybe Text), "after" A..= (after :: Int)]
            Left e -> ["delivered" A..= ("pending" :: Text), "tag" A..= A.Null, "after" A..= (0 :: Int), "error" A..= e]

-- | Resume the session's latest chain member with the message as a run
-- of this server: with the flags and variables its chat recorded in the
-- ledger (its @chat@ event: model, provider, toolsets, system prompt and
-- MCP files, context, turns), else with its model from state.db; in the
-- project the session ran in.
resume :: LiveEnv -> Text -> FilePath -> Text -> IO (Either Text (Text, Maybe Text, Int))
resume env root proj0 msg = do
  leaf <- readDb env root (`sessionLeaf` root)
  model <- readDb env Nothing (`sessionModel` leaf)
  (top, _) <- readDb env (0, Nothing) (`ledgerTail` root)
  chat <- chatEvent env root
  let field k = chat >>= KM.lookup k
      textOf k = case field k of
        Just (A.String t) | not (T.null t) -> Just (T.unpack t)
        _ -> Nothing
      opt flag v = maybe [] (\x -> [flag, x]) v
      toolsets = case field "toolsets" of
        Just (A.Array xs) | ts <- [T.unpack t | A.String t <- toList xs], not (null ts) -> Just (intercalate "," ts)
        _ -> Nothing
      turns = case field "maxTurns" of
        Just (A.Number n) -> Just (show (round n :: Int))
        _ -> Nothing
      flags = case chat of
        Just _ ->
          opt "--provider" (textOf "provider") <> opt "-m" (textOf "model") <> opt "-t" toolsets
            <> opt "--system-prompt-file" (textOf "systemPromptFile") <> opt "--mcp-config" (textOf "mcpConfig")
            <> opt "--context" (textOf "context") <> opt "--max-turns" turns
        Nothing -> opt "-m" (T.unpack <$> model)
      vars = case field "env" of
        Just (A.Object o) -> [(K.toString k, T.unpack t) | (k, A.String t) <- KM.toList o]
        _ -> []
  let proj = proj0
  let args = ["chat"] <> flags <> ["-c", T.unpack leaf, "-p", T.unpack msg]
  -- the lock again just before: a chat started since is not resumed beside
  other <- elsewhere env root
  case other of
    Just p -> pure (Left ("the session is running in another process (pid " <> T.pack (show p) <> ")"))
    Nothing -> do
      r <- spawnRunWith env proj args vars ("resume " <> leaf <> " after a hand edit") top (Just root)
      pure (fmap (\tag -> ("resume", Just tag, top)) r)
