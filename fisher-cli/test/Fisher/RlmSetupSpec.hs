-- | Choosing and setting up @--context rlm@ (spec phase 2, sections 2, 4
-- and 6), the parts that need no chat session: the choice from the flag and
-- the config, the shell's reader, and 'rlmSetupFor' (the interpreter check
-- on the session's PATH, @rlm.json@ loaded, warned about and saved). The
-- chat, the shell, ACP and cron are in "Fisher.RlmContextSpec".
module Fisher.RlmSetupSpec (spec, pathSys, pythonDir) where

import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (utc)
import System.Directory
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Agent.Context.Rlm (RlmState (..), StoredTurn (..), stateToPy)
import Fisher.Agent.Loop (RlmSetup (..))
import Fisher.Cli.Clm
import Fisher.LLM.Sys
import Fisher.Py.Value
import Fisher.RLM.Config (RlmConfig (..))

-- | A Sys over an environment, whose @which@ searches that environment's
-- PATH (as the real one searches the process's); what it prints.
pathSys :: FilePath -> [(Text, Text)] -> IO (Sys, IO Text)
pathSys gauss env0 = do
  env <- newIORef (M.fromList env0)
  printed <- newIORef []
  let which cmd = do
        p <- maybe "" T.unpack . M.lookup "PATH" <$> readIORef env
        findExecutablesInDirectories (splitSearchPath p) (T.unpack cmd) >>= \case
          (x : _) -> pure (Just x)
          [] -> pure Nothing
      sys =
        Sys
          { sysNow = pure 1790000000, sysMonotonic = pure 1000, sysSleep = \_ -> pure ()
          , sysGetEnv = \k -> M.lookup k <$> readIORef env
          , sysSetEnv = \k v -> modifyIORef env (M.insert k v)
          , sysHttp = \_ -> pure (Left (HttpFail "ConnectError" "no network in tests"))
          , sysOpenBrowser = \_ -> pure False, sysRun = \_ -> pure (Left "no"), sysRunInteractive = \_ -> pure (Left "no")
          , sysWhich = which, sysPlatform = "linux"
          , sysUuidHex = pure "00000000000000000000000000000001"
          , sysTimeZoneAt = \_ -> pure utc, sysHome = takeDirectory gauss, sysFisherHome = gauss
          , sysOut = \t -> modifyIORef printed (<> [t]), sysInput = \_ -> pure Nothing, sysGetPass = \_ -> pure Nothing
          , sysStdinTty = pure False, sysStdoutTty = pure False, sysPid = 4242 }
  pure (sys, T.concat <$> readIORef printed)

-- | The directory of this machine's python3.
pythonDir :: IO FilePath
pythonDir = findExecutable "python3" >>= maybe (fail "no python3 on this machine") (pure . takeDirectory)

stored :: RlmState
stored = RlmState [StoredTurn 2 "user" Nothing (PDict [("role", PStr "user"), ("content", PStr "earlier")])] 1 (Just (2, 2))

spec :: Spec
spec = do
  describe "choosing rlm" $ do
    it "the flag and the config choose it; the flag wins" $ do
      let rlmCfg = PDict [("fisher", PDict [("context", PStr "rlm")])]
      contextChoice (Just "rlm") (PDict []) `shouldBe` ChooseRlm
      contextChoice Nothing rlmCfg `shouldBe` ChooseRlm
      contextChoice (Just "compress") rlmCfg `shouldBe` ChooseCompress
      contextChoice (Just "clm") rlmCfg `shouldBe` ChooseClm
    it "takes compress, clm and rlm" $
      map validContext ["compress", "clm", "rlm", "bogus"] `shouldBe` [True, True, True, False]
    it "the shell's reader takes rlm and lists all three for anything else" $ do
      readContextFlag "rlm" `shouldBe` Right "rlm"
      readContextFlag "bogus" `shouldBe` Left "invalid choice: 'bogus' (choose from 'compress', 'clm', 'rlm')"
  describe "rlmSetupFor" $ do
    let sid = "20261013_101500_ab12cd"
        -- what was printed: stdout, then each warning as "stderr: " and its line
        setupIn tmp env cfg = do
          let gauss = tmp </> ".gauss"
          createDirectoryIfMissing True gauss
          (sys, printedOut) <- pathSys gauss env
          warned <- newIORef []
          r <- rlmSetupFor sys (\w -> modifyIORef warned (<> [w])) cfg sid "openai/gpt-5" Nothing
          o <- printedOut
          ws <- readIORef warned
          pure (r, o <> T.concat ["stderr: " <> w <> "\n" | w <- ws], ctxDir gauss sid </> "rlm.json")
    it "refuses with phase 1's message when the session's PATH has no python3" $ withSystemTempDirectory "rlm-setup" $ \tmp -> do
      createDirectoryIfMissing True (tmp </> "empty")
      (r, _, _) <- setupIn tmp [("PATH", T.pack (tmp </> "empty"))] (PDict [])
      either id (const "set up") r `shouldBe` "python3 not found: python3"
    it "with python3: recall's agent defaults, nothing loaded, nothing printed" $ withSystemTempDirectory "rlm-setup" $ \tmp -> do
      py <- pythonDir
      (r, o, _) <- setupIn tmp [("PATH", T.pack py)] (PDict [])
      case r of
        Left e -> expectationFailure (T.unpack e)
        Right (setup, loaded) -> do
          loaded `shouldBe` Nothing
          rlLoad setup `shouldReturn` Nothing
          rcModel (rlConfig setup) `shouldBe` "openai/gpt-5"
          rcMaxIterations (rlConfig setup) `shouldBe` 15
          rcMaxBudget (rlConfig setup) `shouldBe` Just 0.5
      o `shouldBe` ""
    it "does not look at the terminal backend" $ withSystemTempDirectory "rlm-setup" $ \tmp -> do
      py <- pythonDir
      (r, _, _) <- setupIn tmp [("PATH", T.pack py), ("TERMINAL_ENV", "docker")] (PDict [])
      either (const False) (const True) r `shouldBe` True
    it "refuses a bad fisher.rlm section by name" $ withSystemTempDirectory "rlm-setup" $ \tmp -> do
      py <- pythonDir
      (r, _, _) <- setupIn tmp [("PATH", T.pack py)] (PDict [("fisher", PDict [("rlm", PDict [("colour", PStr "red")])])])
      either id (const "set up") r `shouldBe` "fisher.rlm: unknown key colour"
    it "loads rlm.json, and saves it so it loads back" $ withSystemTempDirectory "rlm-setup" $ \tmp -> do
      py <- pythonDir
      let file = ctxDir (tmp </> ".gauss") sid </> "rlm.json"
      createDirectoryIfMissing True (takeDirectory file)
      BS.writeFile file (TE.encodeUtf8 (dumpsAscii (stateToPy stored)))
      (r, o, _) <- setupIn tmp [("PATH", T.pack py)] (PDict [])
      o `shouldBe` ""
      case r of
        Left e -> expectationFailure (T.unpack e)
        Right (setup, loaded) -> do
          loaded `shouldBe` Just stored
          rlLoad setup `shouldReturn` Just stored
          let next = stored {rsOffloads = 2}
          rlSave setup next
          (r2, _, _) <- setupIn tmp [("PATH", T.pack py)] (PDict [])
          fmap snd r2 `shouldBe` Right (Just next)
    it "an invalid rlm.json warns once, on stderr, and resumes with an empty store" $ withSystemTempDirectory "rlm-setup" $ \tmp -> do
      py <- pythonDir
      let file = ctxDir (tmp </> ".gauss") sid </> "rlm.json"
      createDirectoryIfMissing True (takeDirectory file)
      BS.writeFile file "{\"store\": [oops"
      (r, o, _) <- setupIn tmp [("PATH", T.pack py)] (PDict [])
      case r of
        Left e -> expectationFailure (T.unpack e)
        Right (setup, loaded) -> do
          loaded `shouldBe` Nothing
          rlLoad setup `shouldReturn` Nothing
      o `shouldBe` "stderr: fisher: rlm.json is not valid; resuming with an empty store\n"
