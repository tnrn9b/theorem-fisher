-- | @fisher login --provider claude-code@, @fisher logout --provider
-- claude-code@ and the Claude Code line of @fisher status@, against a fake
-- @claude@ on PATH that records its arguments and answers @auth status@
-- from a file.
module Fisher.Cli.ClaudeLoginSpec (spec) where

import Control.Monad (forM_)
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (utc)
import System.Directory
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (IOMode (..), withBinaryFile)
import System.Process (CreateProcess (..), StdStream (..), proc, withCreateProcess, waitForProcess)
import Test.Hspec hiding (after)

import Fisher.Cli.Login (loginMain)
import Fisher.Cli.Logout (logoutMain)
import Fisher.Cli.Status (StatusInfo (..), statusMain)
import Fisher.LLM.Sys hiding (out)
import Fisher.TestUtil (withTmp)

-- | The world of one test: the bin directory, the argv log and the status
-- answer the fake gives.
data World = World { wBin :: FilePath, wArgv :: FilePath, wStatus :: FilePath, wCode :: FilePath, wHome :: FilePath }

loggedIn :: String
loggedIn = "{\"loggedIn\": true, \"authMethod\": \"claude.ai\", \"apiProvider\": \"firstParty\", \"email\": \"x@y.z\", \"orgId\": \"o-1\", \"orgName\": \"Org\", \"subscriptionType\": \"max\"}"

-- | A fake claude: each call's arguments on a line of the argv log; @auth
-- status@ prints the status file and exits with the code file's number.
withWorld :: Bool -> (World -> IO a) -> IO a
withWorld installed k = withTmp $ \tmp -> do
  let w = World (tmp </> "bin") (tmp </> "argv") (tmp </> "status.json") (tmp </> "code") (tmp </> "home")
  mapM_ (createDirectoryIfMissing True) [wBin w, wHome w </> ".fisher"]
  writeFile (wStatus w) loggedIn
  writeFile (wCode w) "0"
  if installed
    then do
      let p = wBin w </> "claude"
      writeFile p $
        "#!/bin/sh\necho \"$*\" >> " <> wArgv w <> "\n\
        \if [ \"$1 $2\" = \"auth status\" ]; then cat " <> wStatus w <> "; exit $(cat " <> wCode w <> "); fi\nexit 0\n"
      getPermissions p >>= setPermissions p . setOwnerExecutable True . setOwnerReadable True . setOwnerWritable True
    else pure ()
  k w

-- | A Sys whose which searches the bin directory only, whose interactive
-- runs are real (stdin from /dev/null here), and that records what it
-- prints.
worldSys :: World -> IO (Sys, IO Text)
worldSys w = do
  env <- newIORef (M.fromList [("PATH", T.pack (wBin w)), ("HOME", T.pack (wHome w))])
  printed <- newIORef []
  let which cmd = findExecutablesInDirectories [wBin w] (T.unpack cmd) >>= \case
        (x : _) -> pure (Just x)
        [] -> pure Nothing
      interactive = \case
        [] -> pure (Left "no command")
        (a : as) -> withBinaryFile "/dev/null" ReadMode $ \devnull ->
          withCreateProcess (proc (T.unpack a) (map T.unpack as)) {std_in = UseHandle devnull} (\_ _ _ ph -> waitForProcess ph) >>= \case
            ExitSuccess -> pure (Right 0)
            ExitFailure n -> pure (Right n)
      sys =
        Sys
          { sysNow = pure 1790000000, sysMonotonic = pure 1000, sysSleep = \_ -> pure ()
          , sysGetEnv = \k -> M.lookup k <$> readIORef env
          , sysSetEnv = \k v -> modifyIORef env (M.insert k v)
          , sysHttp = \_ -> pure (Left (HttpFail "ConnectError" "no network in tests"))
          , sysOpenBrowser = \_ -> pure False, sysRun = \_ -> pure (Left "no"), sysRunInteractive = interactive
          , sysWhich = which, sysPlatform = "linux"
          , sysUuidHex = pure "00000000000000000000000000000001"
          , sysTimeZoneAt = \_ -> pure utc, sysHome = wHome w, sysFisherHome = wHome w </> ".fisher"
          , sysOut = \t -> modifyIORef printed (<> [t]), sysInput = \_ -> pure Nothing, sysGetPass = \_ -> pure Nothing
          , sysStdinTty = pure False, sysStdoutTty = pure False, sysPid = 4242 }
  pure (sys, T.concat <$> readIORef printed)

argvLog :: World -> IO [String]
argvLog w = doesFileExist (wArgv w) >>= \case
  True -> lines <$> readFile (wArgv w)
  False -> pure []

run :: World -> ([Text] -> Sys -> IO Int) -> [Text] -> IO (Int, Text)
run w main args = do
  (sys, printed) <- worldSys w
  code <- main args sys
  (,) code <$> printed

login, logout :: [Text] -> Sys -> IO Int
login args sys = loginMain sys args
logout args sys = logoutMain sys args

status :: [Text] -> Sys -> IO Int
status args sys = statusMain (StatusInfo "<SRC>" "<PYVER>") sys args

spec :: Spec
spec = do
  describe "fisher login --provider claude-code" $ do
    forM_
      [ ([], "auth login")
      , (["--console"], "auth login --console")
      , (["--sso"], "auth login --sso")
      , (["--email", "a@b.c"], "auth login --email a@b.c")
      , (["--console", "--sso", "--email=a@b.c"], "auth login --console --sso --email a@b.c")
      , (["--sso", "--console"], "auth login --console --sso") ] $ \(flags, want) ->
        it ("runs claude " <> want <> " for " <> show flags <> ", then reads the status") $ withWorld True $ \w -> do
          (code, out) <- run w login (["--provider", "claude-code"] <> flags)
          argvLog w `shouldReturn` [want, "auth status --json"]
          out `shouldBe` "Logged in to Claude Code as x@y.z (max)\n"
          code `shouldBe` 0
    it "names the auth method when there is no subscription, and leaves the parentheses out with neither" $ withWorld True $ \w -> do
      writeFile (wStatus w) "{\"loggedIn\": true, \"authMethod\": \"console\", \"email\": \"x@y.z\"}"
      run w login ["--provider", "claude-code", "--console"] `shouldReturn` (0, "Logged in to Claude Code as x@y.z (console)\n")
      writeFile (wStatus w) "{\"loggedIn\": true, \"email\": \"x@y.z\"}"
      run w login ["--provider", "claude-code"] `shouldReturn` (0, "Logged in to Claude Code as x@y.z\n")
    it "fails when the status after the login says logged out" $ withWorld True $ \w -> do
      writeFile (wStatus w) "{\"loggedIn\": false}"
      writeFile (wCode w) "1"
      (code, out) <- run w login ["--provider", "claude-code"]
      code `shouldBe` 1
      out `shouldBe` "Claude Code login did not complete (claude auth status says not logged in).\n"
    it "says how to install claude when it is not on PATH" $ withWorld False $ \w ->
      run w login ["--provider", "claude-code"]
        `shouldReturn` (1, "The 'claude' CLI is not installed. Install it with: npm install -g @anthropic-ai/claude-code\n")
    it "refuses --console, --sso and --email without --provider claude-code, and runs nothing" $ withWorld True $ \w -> do
      forM_ [["--console"], ["--sso"], ["--email", "a@b.c"], ["--provider", "openai-codex", "--sso"]] $ \args -> do
        (code, out) <- run w login args
        code `shouldBe` 2
        T.lines out `shouldSatisfy` (elem "fisher login: error: --console, --sso and --email go with --provider claude-code")
      argvLog w `shouldReturn` []
    it "refuses an email with a space, and runs nothing" $ withWorld True $ \w -> do
      (code, out) <- run w login ["--provider", "claude-code", "--email", "a b@c"]
      code `shouldBe` 2
      out `shouldSatisfy` T.isInfixOf "fisher login: error: argument --email:"
      argvLog w `shouldReturn` []
    it "lists claude-code among the providers in its help" $ withWorld True $ \w -> do
      (code, out) <- run w login ["--help"]
      code `shouldBe` 0
      out `shouldSatisfy` T.isPrefixOf "usage: fisher login [-h] [--provider {openai-codex,claude-code}] [--no-browser]\n"
      out `shouldSatisfy` T.isInfixOf "  --email EMAIL         claude-code: the email to log in as\n"

  describe "fisher logout --provider claude-code" $ do
    it "runs claude auth logout" $ withWorld True $ \w -> do
      run w logout ["--provider", "claude-code"] `shouldReturn` (0, "Logged out of Claude Code.\n")
      argvLog w `shouldReturn` ["auth logout"]
    it "says how to install claude when it is not on PATH" $ withWorld False $ \w ->
      run w logout ["--provider", "claude-code"]
        `shouldReturn` (1, "The 'claude' CLI is not installed. Install it with: npm install -g @anthropic-ai/claude-code\n")

  describe "the Claude Code line of fisher status" $ do
    let claudeLine w = do
          (code, out) <- run w status []
          code `shouldBe` 0
          pure (filter ("  Claude Code" `T.isPrefixOf`) (T.lines out))
    it "logged in: the email and plan" $ withWorld True $ \w ->
      claudeLine w `shouldReturn` ["  Claude Code   ✓ logged in as x@y.z (max)"]
    it "logged out: how to log in" $ withWorld True $ \w -> do
      writeFile (wStatus w) "{\"loggedIn\": false}"
      writeFile (wCode w) "1"
      claudeLine w `shouldReturn` ["  Claude Code   ✗ not logged in (run: fisher login --provider claude-code)"]
    it "not installed" $ withWorld False $ \w ->
      claudeLine w `shouldReturn` ["  Claude Code   ✗ claude CLI not installed"]
    it "sits under Auth Providers, after OpenAI Codex" $ withWorld True $ \w -> do
      (_, out) <- run w status []
      let ls = T.lines out
          after x = drop 1 (dropWhile (not . T.isPrefixOf x) ls)
      take 1 (filter (\l -> "  " `T.isPrefixOf` l && not ("   " `T.isPrefixOf` l)) (after "  OpenAI Codex")) `shouldSatisfy` all (T.isPrefixOf "  Claude Code")
      takeWhile (not . T.null) (after "◆ Auth Providers") `shouldSatisfy` any (T.isPrefixOf "  Claude Code")
