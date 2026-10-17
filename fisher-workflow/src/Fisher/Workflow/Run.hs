-- | Running external tools during staging (@_run@), and the shell quoting
-- upstream uses in prompts.
module Fisher.Workflow.Run
  ( runStaging
  , requireExecutable
  , whichPath
  , shlexQuote
  , runHeadless
  ) where

import Control.Exception (IOException, try)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import System.Directory (doesDirectoryExist, doesFileExist, executable, getPermissions)
import System.Exit (ExitCode (..))
import System.Process.Typed (nullStream, proc, readProcess, runProcess, setEnv, setStdin, setWorkingDir)

import Fisher.Which (whichIn)
import Fisher.Workflow.Types

decode :: BL.ByteString -> Text
decode = TE.decodeUtf8With TEE.lenientDecode . BL.toStrict

-- | Run a tool and return its stdout. A failure is a staging error:
-- @"<prefix>: <stderr or stdout>"@, or the bare prefix when both are
-- empty. Without an environment the tool inherits this process's, as
-- upstream's @env=None@ does.
runStaging :: [String] -> Text -> Maybe Env -> Maybe FilePath -> IO Text
runStaging argv prefix env cwd = case argv of
  [] -> throwW (StagingError (prefix <> ": empty command"))
  (exe : args) -> do
    let cfg0 = proc exe args
        cfg1 = maybe cfg0 (\e -> setEnv (Map.toList e) cfg0) env
        cfg = maybe cfg1 (`setWorkingDir` cfg1) cwd
    r <- try (readProcess cfg) :: IO (Either IOException (ExitCode, BL.ByteString, BL.ByteString))
    case r of
      Left e -> throwW (StagingError (prefix <> ": " <> T.pack (show e)))
      Right (ExitSuccess, out, _) -> pure (decode out)
      Right (ExitFailure _, out, err) -> do
        let detail = T.strip (if T.null (T.strip (decode err)) then decode out else decode err)
        throwW (StagingError (prefix <> (if T.null detail then "" else ": " <> detail)))

-- | Fisher only: run a child to its end in a directory and environment,
-- stdin from /dev/null and stdout and stderr this process's, and return
-- its exit status (or why it could not start).
runHeadless :: [Text] -> FilePath -> Env -> IO (Either Text ExitCode)
runHeadless argv cwd env = case argv of
  [] -> pure (Left "empty command")
  (exe : args) -> do
    let cfg = setStdin nullStream . setWorkingDir cwd . setEnv (Map.toList env) $ proc (T.unpack exe) (map T.unpack args)
    r <- try (runProcess cfg) :: IO (Either IOException ExitCode)
    pure (either (Left . T.pack . show) Right r)

-- | @shutil.which(name, path=env PATH)@; a name with a slash is checked
-- as it is.
whichPath :: Env -> String -> IO (Maybe FilePath)
whichPath env name
  | '/' `elem` name = do
      f <- doesFileExist name
      d <- doesDirectoryExist name
      x <- if f && not d then executable <$> getPermissions name else pure False
      pure (if x then Just name else Nothing)
  | otherwise = whichIn (Map.findWithDefault "" "PATH" env) name

requireExecutable :: String -> Text -> Env -> IO FilePath
requireExecutable name msg env = whichPath env name >>= maybe (throwW (PreflightError msg)) pure

-- | @shlex.quote@.
shlexQuote :: Text -> Text
shlexQuote s
  | T.null s = "''"
  | T.all safe s = s
  | otherwise = "'" <> T.replace "'" "'\"'\"'" s <> "'"
  where
    safe c = isAlphaNum c || c `elem` ("_@%+=:,./-" :: String)
