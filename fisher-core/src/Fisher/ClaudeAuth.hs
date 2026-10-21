-- | The Claude Code CLI's own login, as Fisher drives it: Fisher only runs
-- the @claude@ executable (@claude auth login@, @auth logout@, @auth status
-- --json@) and reads what @auth status@ prints. It never reads, stores or
-- passes the OAuth token: that stays with Claude Code.
--
-- @claude auth status --json@ prints an object such as @{"loggedIn": true,
-- "authMethod": "claude.ai", "apiProvider": "firstParty", "email": ...,
-- "orgId": ..., "orgName": ..., "subscriptionType": "max"}@. The fields are
-- read leniently (a missing or mistyped one is absent), and the
-- organisation is not kept. A non-zero exit means logged out (the JSON, when
-- there is some, still gives the other fields), except 126 and 127 (it
-- could not be run at all).
module Fisher.ClaudeAuth
  ( ClaudeAuthStatus (..)
  , ClaudeLogin (..)
  , parseClaudeAuthStatus
  , claudeAuthStatus
  , claudeAuthStatusWithin
  , claudeAuthStatusWithEnv
  , claudeAuthStatusIn
  , claudeLogout
  , findClaude
  , claudeAccount
  , claudePlan
  , claudeLoginArgs
  , claudeLogoutArgs
  , validClaudeEmail
  , claudeNotInstalledMessage
  ) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, SomeException, evaluate, try)
import Control.Monad (forM_, void)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Char (isControl, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.IO (IOMode (..), hClose, openBinaryFile)
import System.Posix.Signals (sigKILL, signalProcess, signalProcessGroup)
import System.Process (CreateProcess (..), StdStream (..), createProcess, getPid, proc, waitForProcess)
import System.Timeout (timeout)

import Fisher.Which (whichEnv)

-- | What @claude auth status@ says, without the organisation.
data ClaudeAuthStatus = ClaudeAuthStatus
  { casLoggedIn :: Bool
  , casAuthMethod :: Maybe Text
  , casApiProvider :: Maybe Text
  , casEmail :: Maybe Text
  , casSubscriptionType :: Maybe Text
  }
  deriving (Eq, Show)

-- | The options of @claude auth login@: Anthropic Console (API billing)
-- instead of the subscription, SSO forced, and the email to log in as.
data ClaudeLogin = ClaudeLogin
  { clConsole :: Bool
  , clSso :: Bool
  , clEmail :: Maybe Text
  }
  deriving (Eq, Show)

claudeNotInstalledMessage :: Text
claudeNotInstalledMessage = "The 'claude' CLI is not installed. Install it with: npm install -g @anthropic-ai/claude-code"

-- | The status JSON: Left when it is not a JSON object.
parseClaudeAuthStatus :: BL.ByteString -> Either Text ClaudeAuthStatus
parseClaudeAuthStatus raw = case A.eitherDecode raw of
  Right (A.Object o) ->
    let text k = case KM.lookup k o of
          Just (A.String t) | not (T.null (T.strip t)) -> Just (T.strip t)
          _ -> Nothing
     in Right
          ClaudeAuthStatus
            { casLoggedIn = KM.lookup "loggedIn" o == Just (A.Bool True)
            , casAuthMethod = text "authMethod"
            , casApiProvider = text "apiProvider"
            , casEmail = text "email"
            , casSubscriptionType = text "subscriptionType"
            }
  Right _ -> Left "unreadable status: not a JSON object"
  Left _ -> Left "unreadable status: not JSON"

-- | 'claudeAuthStatusWithin' 10 s.
claudeAuthStatus :: FilePath -> IO (Either Text ClaudeAuthStatus)
claudeAuthStatus = claudeAuthStatusWithin 10000000

-- | @EXE auth status --json@, stdin from /dev/null, at most this many
-- microseconds: Left @not installed@ (no such executable, or it cannot be
-- started), @timed out@, or why its output could not be read.
--
-- The limit holds only on the threaded RTS (every Fisher executable and
-- test suite is built with it): on the other, the wait for the child
-- blocks the whole runtime and the timer never fires.
claudeAuthStatusWithin :: Int -> FilePath -> IO (Either Text ClaudeAuthStatus)
claudeAuthStatusWithin limit = statusRun limit Nothing

-- | 'claudeAuthStatus' in this environment (not the process's): the one
-- the caller found the executable on.
claudeAuthStatusWithEnv :: [(String, String)] -> FilePath -> IO (Either Text ClaudeAuthStatus)
claudeAuthStatusWithEnv env = statusRun 10000000 (Just env)

statusRun :: Int -> Maybe [(String, String)] -> FilePath -> IO (Either Text ClaudeAuthStatus)
statusRun limit menv exe = do
  here <- doesFileExist exe
  if not here
    then pure (Left "not installed")
    else do
      r <- runLimited limit menv exe ["auth", "status", "--json"]
      pure $ case r of
        Nothing -> Left "timed out"
        Just (Left (_ :: IOException)) -> Left "not installed"
        Just (Right (ExitSuccess, out)) -> parseClaudeAuthStatus out
        -- 126 and 127: the script could not start (its interpreter not
        -- found, not executable), which says nothing about the login
        Just (Right (ExitFailure n, _)) | n == 126 || n == 127 -> Left ("could not run claude (exit " <> T.pack (show n) <> ")")
        Just (Right (ExitFailure _, out)) ->
          Right (either (const loggedOut) (\s -> s {casLoggedIn = False}) (parseClaudeAuthStatus out))
  where
    loggedOut = ClaudeAuthStatus False Nothing Nothing Nothing Nothing

-- | Run a command in a process group of its own, stdin from /dev/null,
-- stderr discarded, and read its stdout. Nothing when it takes longer than
-- the limit (counting the time until its output closes: a descendant that
-- keeps it open counts too). Then the group and the child are killed and
-- the reader thread is killed (which closes the pipe), so nothing that
-- still holds the output, in the group or not, can hold the caller.
runLimited :: Int -> Maybe [(String, String)] -> FilePath -> [String] -> IO (Maybe (Either IOException (ExitCode, BL.ByteString)))
runLimited limit menv exe args = do
  started <- try $ do
    devIn <- openBinaryFile "/dev/null" ReadMode
    devOut <- openBinaryFile "/dev/null" WriteMode
    r <- try (createProcess (proc exe args) {std_in = UseHandle devIn, std_out = CreatePipe, std_err = UseHandle devOut, create_group = True, env = menv})
    mapM_ quietClose [devIn, devOut]
    either (\(e :: IOException) -> ioError e) pure r
  case started of
    Left e -> pure (Just (Left e))
    Right (_, Just hout, _, ph) -> do
      -- the pid now, before a wait reaps the child (the group keeps its id)
      mpid <- getPid ph
      outVar <- newEmptyMVar
      reader <- forkIO (try (B.hGetContents hout >>= evaluate) >>= putMVar outVar . either (\(_ :: SomeException) -> B.empty) id)
      done <- timeout limit ((,) <$> waitForProcess ph <*> takeMVar outVar)
      case done of
        Just (code, out) -> pure (Just (Right (code, BL.fromStrict out)))
        Nothing -> do
          forM_ mpid $ \pid -> do
            void (try (signalProcessGroup sigKILL pid) :: IO (Either SomeException ()))
            void (try (signalProcess sigKILL pid) :: IO (Either SomeException ()))
          -- hGetContents closes the pipe as the reader dies
          killThread reader
          void (timeout 1000000 (try (waitForProcess ph) :: IO (Either SomeException ExitCode)))
          pure Nothing
    Right _ -> pure (Just (Left (userError "no stdout pipe")))
  where
    quietClose h = void (try (hClose h) :: IO (Either SomeException ()))

-- | @EXE auth logout@, stdin from /dev/null, at most 30 s.
claudeLogout :: FilePath -> IO (Either Text ())
claudeLogout exe = do
  here <- doesFileExist exe
  if not here
    then pure (Left "not installed")
    else
      runLimited 30000000 Nothing exe (map T.unpack claudeLogoutArgs) >>= \case
        Nothing -> pure (Left "timed out")
        Just (Left (_ :: IOException)) -> pure (Left "not installed")
        Just (Right (ExitSuccess, _)) -> pure (Right ())
        Just (Right (ExitFailure n, _)) -> pure (Left ("claude auth logout exited with code " <> T.pack (show n)))

-- | @claude@ on the environment's PATH (this process's when it has none).
findClaude :: [(String, String)] -> IO (Maybe FilePath)
findClaude env = whichEnv env "claude"

-- | The status from the @claude@ on the environment's PATH.
claudeAuthStatusIn :: [(String, String)] -> IO (Either Text ClaudeAuthStatus)
claudeAuthStatusIn env = findClaude env >>= maybe (pure (Left "not installed")) claudeAuthStatus

-- | The plan: the subscription type, else the auth method.
claudePlan :: ClaudeAuthStatus -> Maybe Text
claudePlan s = maybe (casAuthMethod s) Just (casSubscriptionType s)

-- | @EMAIL (PLAN)@, the parentheses left out when there is no plan.
claudeAccount :: ClaudeAuthStatus -> Text
claudeAccount s = maybe "an unnamed account" id (casEmail s) <> maybe "" (\p -> " (" <> p <> ")") (claudePlan s)

claudeLoginArgs :: ClaudeLogin -> [Text]
claudeLoginArgs o =
  ["auth", "login"] <> ["--console" | clConsole o] <> ["--sso" | clSso o] <> maybe [] (\e -> ["--email", e]) (clEmail o)

claudeLogoutArgs :: [Text]
claudeLogoutArgs = ["auth", "logout"]

-- | An email to pass as one argument: one word of at most 254 characters,
-- no control characters, not starting with a dash.
validClaudeEmail :: Text -> Bool
validClaudeEmail e =
  not (T.null e)
    && T.length e <= 254
    && not (T.any (\c -> isSpace c || isControl c) e)
    && not ("-" `T.isPrefixOf` e)
