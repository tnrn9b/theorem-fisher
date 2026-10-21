-- | @fisher login --provider claude-code@ and @fisher logout --provider
-- claude-code@: the Claude Code CLI's own login, driven by running @claude
-- auth login@ (on the terminal: it opens the browser, and may print a URL
-- and read a code) and @claude auth logout@, then reading @claude auth
-- status@. Fisher never sees the token; Claude Code keeps it.
module Fisher.Cli.ClaudeLogin
  ( claudeCodeLogin
  , claudeCodeLoginWith
  , claudeCodeLogout
  , claudeLoginPrompt
  , claudeStatusText
  ) where

import Control.Exception (catch)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (getHomeDirectory)
import System.IO (hFlush, stdout)
import System.IO.Error (isEOFError)

import Fisher.ClaudeAuth
import Fisher.LLM.Sys
import Fisher.LLM.SysIO (newSysIO)

-- | Run @claude auth login@ with the options, then report what @claude
-- auth status@ says. 0 once logged in, else 1.
claudeCodeLogin :: Sys -> ClaudeLogin -> IO Int
claudeCodeLogin sys o =
  sysWhich sys "claude" >>= \case
    Nothing -> outLn sys claudeNotInstalledMessage >> pure 1
    Just exe -> claudeCodeLoginWith sys exe o

-- | 'claudeCodeLogin' with this @claude@.
claudeCodeLoginWith :: Sys -> FilePath -> ClaudeLogin -> IO Int
claudeCodeLoginWith sys exe o =
  sysRunInteractive sys (T.pack exe : claudeLoginArgs o) >>= \case
    Left _ -> outLn sys claudeNotInstalledMessage >> pure 1
    Right code ->
      claudeAuthStatus exe >>= \case
        Right s | casLoggedIn s -> outLn sys ("Logged in to Claude Code as " <> claudeAccount s) >> pure 0
        Right _ -> failed ("claude auth status says not logged in" <> exited code)
        Left e -> failed ("could not read claude auth status: " <> e <> exited code)
  where
    failed why = outLn sys ("Claude Code login did not complete (" <> why <> ").") >> pure 1
    exited code = if code == 0 then "" else "; claude auth login exited with code " <> T.pack (show code)

-- | @fisher doctor --fix@ on a terminal: ask whether to log in to Claude
-- Code now and, on yes, run the login with this @claude@ (the Fisher home
-- given). True when it ran the login.
claudeLoginPrompt :: FilePath -> FilePath -> IO Bool
claudeLoginPrompt fisherHome exe = do
  TIO.putStr "    Claude Code is not logged in. Log in now? [y/N] "
  hFlush stdout
  answer <- (T.toLower . T.strip <$> TIO.getLine) `catch` \e -> if isEOFError e then pure "" else ioError e
  if answer `elem` ["y", "yes"]
    then do
      home <- getHomeDirectory
      sys <- newSysIO home fisherHome
      _ <- claudeCodeLoginWith sys exe (ClaudeLogin False False Nothing)
      pure True
    else pure False

-- | @claude auth logout@. 0 when it succeeded, else 1.
claudeCodeLogout :: Sys -> IO Int
claudeCodeLogout sys =
  sysWhich sys "claude" >>= \case
    Nothing -> outLn sys claudeNotInstalledMessage >> pure 1
    Just exe ->
      claudeLogout exe >>= \case
        Right () -> outLn sys "Logged out of Claude Code." >> pure 0
        Left e -> outLn sys ("Claude Code logout failed (" <> e <> ").") >> pure 1

-- | The Claude Code line of @fisher status@ (after its check mark): whether
-- it is logged in, and as whom.
claudeStatusText :: Sys -> IO (Bool, Text)
claudeStatusText sys =
  sysWhich sys "claude" >>= \case
    Nothing -> pure (False, "claude CLI not installed")
    Just exe ->
      claudeAuthStatus exe >>= \case
        Right s | casLoggedIn s -> pure (True, "logged in as " <> claudeAccount s)
        Right _ -> pure (False, notLoggedIn)
        Left "not installed" -> pure (False, "claude CLI not installed")
        Left e -> pure (False, "status unknown (" <> e <> ")")
  where
    notLoggedIn = "not logged in (run: fisher login --provider claude-code)"
