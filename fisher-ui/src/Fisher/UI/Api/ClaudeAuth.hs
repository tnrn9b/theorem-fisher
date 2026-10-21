{-# LANGUAGE MultiWayIf #-}
-- | The Settings page's Claude Code panel: the Claude Code CLI's own login,
-- driven by running @claude@ (Fisher never reads, stores or passes the
-- token; Claude Code keeps it).
--
-- Routes (the same access rules as every route: the cookie, Host and
-- Origin, checked by the server before any of these):
--
--   * @GET /api/claude-auth@: @{installed, loggedIn, authMethod, email,
--     subscriptionType, error, login}@ from @claude auth status --json@
--     (stdin from /dev/null, 10 s; no organisation). @error@ is why the
--     status could not be read (else null), @login@ the tag of the login
--     going (else null).
--   * @POST /api/claude-auth/login@ @{console?, sso?, email?}@: starts
--     @claude auth login [--console] [--sso] [--email E]@ as a child in
--     the Fisher home, in its own process group, its stdin a pipe (for a
--     code pasted on the page) and its output kept in memory, never on
--     disk: @{tag}@. The email is one word of at most 254 characters, no
--     control characters, not starting with a dash; it goes nowhere but
--     the child's argv. One login at a time: 409 with the tag going.
--   * @GET /api/claude-auth/login/:tag?from=N@: @{tag, running, stopping,
--     exitCode, lines, partial, next}@: the complete lines of output from
--     line N (ANSI escapes and carriage returns removed), the line being
--     written (a prompt waits without a newline) and the N to ask from
--     next, and @truncated@ once the output passed 1 MB (what came after
--     is not kept). A line redrawn with a carriage return shows its last
--     text. The page polls it, and asks for the status when it has ended.
--   * @POST /api/claude-auth/login/:tag/input@ @{text}@: one line (no line
--     break, at most 4096 characters) written to the child's stdin as
--     UTF-8; 409 once it has ended, 503 when the child has not read it in
--     5 s. The text is never logged or kept.
--   * @POST /api/claude-auth/login/:tag/stop@: the child's tree stopped
--     (SIGTERM, SIGKILL after the grace): 202, or 409 once it has ended.
--   * @POST /api/claude-auth/logout@: @claude auth logout@ (stdin from
--     /dev/null, 30 s), then the status as the GET gives it.
--
-- With no @claude@ to run, the status says @installed: false@ and the login
-- and logout answer 409 with how to install it.
module Fisher.UI.Api.ClaudeAuth
  ( ClaudeAuthEnv (..)
  , newClaudeAuthEnv
  , claudeAuthApi
  , stopClaudeLogins
  , stripAnsi
  , clean
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (IOException, SomeException, try)
import Control.Monad (filterM, forM, forM_, void)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing, listToMaybe)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Network.HTTP.Types
import Network.Wai
import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose, hFlush)
import System.Timeout (timeout)
import Text.Read (readMaybe)

import Fisher.ClaudeAuth
import Fisher.UI.Api.Live (bad, jsonBodyWith)
import Fisher.UI.Children
import Fisher.UI.Response (Api, jsonResponse)

-- | One @claude auth login@ child: its stdin, and its output so far (the
-- complete lines, and the bytes of the line being written).
data LoginRun = LoginRun
  { lrChild :: Child
  , lrExit :: MVar ExitCode
  , lrIn :: MVar (Maybe Handle)
  -- ^ Nothing once closed
  , lrLines :: IORef (Seq Text)
  , lrPartial :: IORef B.ByteString
  , lrBytes :: IORef Int
  -- ^ the bytes of the lines kept
  , lrTruncated :: IORef Bool
  , lrStopping :: IORef Bool
  }

data ClaudeAuthEnv = ClaudeAuthEnv
  { caClaude :: IO (Maybe FilePath)
  -- ^ the @claude@ to run (the one on the server's PATH)
  , caHome :: FilePath
  -- ^ the Fisher home: the children's directory
  , caGrace :: Int
  -- ^ microseconds from SIGTERM to SIGKILL
  , caRuns :: IORef (M.Map Text LoginRun)
  , caStart :: MVar ()
  , caClosing :: IORef Bool
  }

-- | The env over the Fisher home, with the @claude@ on the server's PATH.
newClaudeAuthEnv :: FilePath -> IO ClaudeAuthEnv
newClaudeAuthEnv home = ClaudeAuthEnv (findExecutable "claude") home stopGrace <$> newIORef M.empty <*> newMVar () <*> newIORef False

-- | Output kept per login, in bytes (what comes after is dropped and the
-- output says it was truncated).
maxBytes :: Int
maxBytes = 1048576

claudeAuthApi :: ClaudeAuthEnv -> Api
claudeAuthApi env r = case pathInfo r of
  ["api", "claude-auth"] -> only methodGet (statusResponse env)
  ["api", "claude-auth", "login"] -> only methodPost (start env r)
  ["api", "claude-auth", "login", tag] -> only methodGet (withRun tag (output r tag))
  ["api", "claude-auth", "login", tag, "input"] -> only methodPost (withRun tag (input r))
  ["api", "claude-auth", "login", tag, "stop"] -> only methodPost (withRun tag (stop env))
  ["api", "claude-auth", "logout"] -> only methodPost (logout env)
  _ -> pure Nothing
  where
    only m k
      | requestMethod r == m = Just <$> k
      | otherwise = pure (Just (bad methodNotAllowed405 (TE.decodeUtf8 m)))
    withRun tag k = M.lookup tag <$> readIORef (caRuns env) >>= maybe (pure (bad notFound404 "no such login")) k

-- ------------------------------------------------------------- status

isRunning :: LoginRun -> IO Bool
isRunning run = isNothing <$> tryReadMVar (lrExit run)

-- | The tag of the login going, if any.
going :: ClaudeAuthEnv -> IO (Maybe Text)
going env = do
  runs <- M.toList <$> readIORef (caRuns env)
  live <- forM runs $ \(t, run) -> (\b -> if b then Just t else Nothing) <$> isRunning run
  pure (listToMaybe [t | Just t <- live])

statusView :: ClaudeAuthEnv -> IO A.Value
statusView env = do
  exe <- caClaude env
  st <- maybe (pure (Left "not installed")) claudeAuthStatus exe
  login <- going env
  let base installed loggedIn s err =
        A.object
          [ "installed" A..= installed, "loggedIn" A..= loggedIn
          , "authMethod" A..= (s >>= casAuthMethod), "email" A..= (s >>= casEmail)
          , "subscriptionType" A..= (s >>= casSubscriptionType)
          , "error" A..= (err :: Maybe Text), "login" A..= login ]
  pure $ case st of
    Left "not installed" -> base False False Nothing Nothing
    Left e -> base True False Nothing (Just e)
    Right s -> base True (casLoggedIn s) (Just s) Nothing

statusResponse :: ClaudeAuthEnv -> IO Response
statusResponse env = jsonResponse ok200 <$> statusView env

-- ------------------------------------------------------------- login

-- | The login's options from the body, or why they will not do (the
-- messages never repeat the email).
loginOptions :: A.Object -> Either Text ClaudeLogin
loginOptions o = do
  console <- flag "console"
  sso <- flag "sso"
  email <- case KM.lookup "email" o of
    Nothing -> Right Nothing
    Just A.Null -> Right Nothing
    Just (A.String e)
      | T.null (T.strip e) -> Right Nothing
      | validClaudeEmail (T.strip e) -> Right (Just (T.strip e))
      | otherwise -> Left "email is one address: no spaces or line breaks, at most 254 characters"
    Just _ -> Left "email is text"
  Right (ClaudeLogin console sso email)
  where
    flag k = case KM.lookup k o of
      Nothing -> Right False
      Just A.Null -> Right False
      Just (A.Bool b) -> Right b
      Just _ -> Left (K.toText k <> " is true or false")

start :: ClaudeAuthEnv -> Request -> IO Response
start env r =
  jsonBodyWith 4096 r >>= \case
    Nothing -> pure (bad badRequest400 "send {console, sso, email} as a JSON object")
    Just o -> case loginOptions o of
      Left e -> pure (bad badRequest400 e)
      Right opts -> do
        made <- withMVar (caStart env) $ \_ -> do
          closing <- readIORef (caClosing env)
          busy <- going env
          exe <- caClaude env
          if
            | closing -> pure (Left (bad serviceUnavailable503 "fisher ui is stopping"))
            | Just t <- busy -> pure (Left (jsonResponse conflict409 (A.object ["error" A..= ("a Claude Code login is already going" :: Text), "tag" A..= t])))
            | Nothing <- exe -> pure (Left (bad conflict409 claudeNotInstalledMessage))
            | Just claude <- exe -> do
                tag <- newTag
                res <- try (spawnChildPiped (caHome env) (claude : map T.unpack (claudeLoginArgs opts)) [("FISHER_UI_TAG", T.unpack tag)])
                case res of
                  Left (e :: SomeException) -> pure (Left (bad internalServerError500 (T.pack (show e))))
                  Right (ch, exited, hin, hout) -> do
                    run <- LoginRun ch exited <$> newMVar (Just hin) <*> newIORef Seq.empty <*> newIORef B.empty <*> newIORef 0 <*> newIORef False <*> newIORef False
                    -- the ended runs go: one login is shown at a time
                    old <- readIORef (caRuns env)
                    ended <- map fst <$> filterM (fmap not . isRunning . snd) (M.toList old)
                    atomicModifyIORef' (caRuns env) (\m -> (M.insert tag run (foldr M.delete m ended), ()))
                    _ <- forkIO (readOutput run hout)
                    pure (Right tag)
        pure $ case made of
          Left resp -> resp
          Right tag -> jsonResponse ok200 (A.object ["tag" A..= tag])

-- | Read the child's output until it ends, then close its stdin.
readOutput :: LoginRun -> Handle -> IO ()
readOutput run hout = do
  let loop = do
        chunk <- either (\(_ :: IOException) -> B.empty) id <$> try (B.hGetSome hout 4096)
        if B.null chunk
          then pure ()
          else do
            pending <- readIORef (lrPartial run)
            let parts = BC.split '\n' (pending <> chunk)
                (done0, rest0) = (init parts, last parts)
                -- a line without an end is kept to 64 KB
                (done, rest) = if B.length rest0 > 65536 then (done0 <> [rest0], B.empty) else (done0, rest0)
            writeIORef (lrPartial run) rest
            mapM_ keep done
            loop
      keep l = do
        used <- readIORef (lrBytes run)
        if used + B.length l > maxBytes
          then writeIORef (lrTruncated run) True
          else do
            writeIORef (lrBytes run) (used + B.length l)
            modifyIORef' (lrLines run) (|> clean l)
  loop
  void (try (hClose hout) :: IO (Either SomeException ()))
  _ <- readMVar (lrExit run)
  closeInput run

closeInput :: LoginRun -> IO ()
closeInput run = modifyMVar_ (lrIn run) $ \h -> do
  forM_ h (\x -> void (try (hClose x) :: IO (Either SomeException ())))
  pure Nothing

-- | A line of output as text: lenient UTF-8, without ANSI escapes, and
-- only what comes after its last carriage return (a prompt or spinner
-- redrawn in place shows its last frame; a @\r\n@ ending is a newline).
clean :: B.ByteString -> Text
clean = lastFrame . stripAnsi . TE.decodeUtf8With TEE.lenientDecode
  where
    lastFrame t = snd (T.breakOnEnd "\r" (T.dropWhileEnd (== '\r') t))

-- | The text without ANSI escape sequences (CSI such as colours, OSC such
-- as titles and hyperlinks, and two-character escapes).
stripAnsi :: Text -> Text
stripAnsi t = case T.breakOn "\ESC" t of
  (a, rest)
    | T.null rest -> a
    | otherwise -> a <> stripAnsi (dropEscape (T.drop 1 rest))
  where
    dropEscape r = case T.uncons r of
      Just ('[', r') -> T.drop 1 (T.dropWhile (\c -> c < '@' || c > '~') r')
      Just (']', r') ->
        let (_, z) = T.break (\c -> c == '\a' || c == '\ESC') r'
         in if "\ESC\\" `T.isPrefixOf` z then T.drop 2 z else T.drop 1 z
      Just (_, r') -> r'
      Nothing -> ""

output :: Request -> Text -> LoginRun -> IO Response
output r tag run = case from of
  Nothing -> pure (bad badRequest400 "from is a line number")
  Just n -> do
    ex <- tryReadMVar (lrExit run)
    stopping <- readIORef (lrStopping run)
    ls <- readIORef (lrLines run)
    partial <- readIORef (lrPartial run)
    truncated <- readIORef (lrTruncated run)
    let total = Seq.length ls
        n' = max 0 (min n total)
    pure . jsonResponse ok200 $
      A.object
        [ "tag" A..= tag, "running" A..= isNothing ex, "stopping" A..= stopping
        , "exitCode" A..= fmap (\case ExitSuccess -> 0; ExitFailure c -> c) ex
        , "lines" A..= foldr (:) [] (Seq.drop n' ls), "partial" A..= clean partial, "next" A..= total
        , "truncated" A..= truncated ]
  where
    from = case lookup "from" (queryString r) of
      Nothing -> Just 0
      Just Nothing -> Just 0
      Just (Just v) -> readMaybe (BC.unpack v) :: Maybe Int

input :: Request -> LoginRun -> IO Response
input r run =
  jsonBodyWith 16384 r >>= \case
    Just o | Just (A.String raw) <- KM.lookup "text" o -> do
      let text = T.dropWhileEnd (== ' ') raw
      if
        | T.any (`elem` ['\n', '\r', '\0']) text -> pure (bad badRequest400 "the text is one line")
        | T.length text > 4096 -> pure (bad badRequest400 "the text is at most 4096 characters")
        | otherwise -> do
            alive <- isRunning run
            if not alive
              then pure (bad conflict409 "the login has ended")
              else do
                -- bounded: a child that does not read its stdin must not hold
                -- the handle (stop closes it only after killing the child)
                wrote <- timeout 5000000 . withMVar (lrIn run) $ \case
                  Nothing -> pure False
                  Just h -> either (\(_ :: IOException) -> False) (const True) <$> try (B.hPut h (TE.encodeUtf8 (text <> "\n")) >> hFlush h)
                pure $ case wrote of
                  Just True -> jsonResponse ok200 (A.object ["sent" A..= True])
                  Just False -> bad conflict409 "the login has ended"
                  Nothing -> bad serviceUnavailable503 "the login is not reading its input"
    _ -> pure (bad badRequest400 "send {text} as JSON")

stop :: ClaudeAuthEnv -> LoginRun -> IO Response
stop env run = do
  alive <- isRunning run
  if not alive
    then pure (bad conflict409 "the login has ended")
    else do
      atomicModifyIORef' (lrStopping run) (const (True, ()))
      _ <- forkIO (stopRun env run)
      pure (jsonResponse accepted202 (A.object ["status" A..= ("stopping" :: Text)]))

stopRun :: ClaudeAuthEnv -> LoginRun -> IO ()
stopRun env run = do
  atomicModifyIORef' (lrStopping run) (const (True, ()))
  -- the child first: a write blocked on its stdin then fails and lets go
  -- of the handle, which is closed after
  void (try (stopChildWith (caGrace env) (lrChild run)) :: IO (Either SomeException ()))
  closeInput run

-- | Stop every login still going and wait for them; none starts after
-- this. The server calls it as it stops.
stopClaudeLogins :: ClaudeAuthEnv -> IO ()
stopClaudeLogins env = do
  -- under the start lock: a start past its closing check has inserted its run
  runs <- withMVar (caStart env) $ \_ -> do
    atomicModifyIORef' (caClosing env) (const (True, ()))
    M.elems <$> readIORef (caRuns env)
  dones <- forM runs $ \run -> do
    done <- newEmptyMVar
    alive <- isRunning run
    if alive then void (forkIO (stopRun env run >> putMVar done ())) else putMVar done ()
    pure done
  mapM_ takeMVar dones

-- ------------------------------------------------------------- logout

logout :: ClaudeAuthEnv -> IO Response
logout env =
  caClaude env >>= \case
    Nothing -> pure (bad conflict409 claudeNotInstalledMessage)
    Just exe ->
      claudeLogout exe >>= \case
        Left "not installed" -> pure (bad conflict409 claudeNotInstalledMessage)
        Left e -> pure (bad internalServerError500 e)
        Right () -> jsonResponse ok200 <$> statusView env
