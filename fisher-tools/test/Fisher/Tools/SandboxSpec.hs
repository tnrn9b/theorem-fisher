-- | The sandbox backends' pure parts against upstream (sandbox.json): the
-- sudo rewrite, the subprocess environment and its blocklist, fence
-- extraction, env config parsing, and the docker and ssh command lines.
module Fisher.Tools.SandboxSpec (spec) where

import Control.Monad (forM_)
import Data.IORef
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Py.Value
import Fisher.Tools.Sandbox.Common
import Fisher.Tools.Sandbox.Config
import Fisher.Tools.Sandbox.Remote
import Fisher.Tools.TestUtil

kvs :: PyVal -> [(Text, PyVal)]
kvs = \case
  PDict x -> x
  _ -> []

strDict :: PyVal -> [(Text, Text)]
strDict v = [(k, s) | (k, PStr s) <- kvs v]

spec :: Spec
spec = do
  doc <- runIO (loadDoc "test/golden/b2/sandbox.json")
  describe "_transform_sudo_command" $
    forM_ (listOf (field "sudo" doc)) $ \c -> it (show (txt "password" c, txt "command" c)) $
      transformSudo (txt "password" c) (txt "command" c) `shouldBe` (txt "out" c, case field "stdin" c of PStr s -> Just s; _ -> Nothing)
  it "the provider env blocklist" $ providerEnvBlocklist `shouldBe` S.fromList (strList (field "blocklist" doc))
  describe "_make_run_env" $
    forM_ (zip [1 :: Int ..] (listOf (field "run_env" doc))) $ \(i, c) -> it (show i) $
      makeRunEnv (strDict (field "environ" c)) (strDict (field "extra" c)) `shouldBe` strDict (field "run_env" c)
  describe "fence extraction and shell-noise cleaning" $
    forM_ (zip [1 :: Int ..] (listOf (field "fence" doc))) $ \(i, c) -> it (show i) $ do
      extractFencedOutput (txt "raw" c) `shouldBe` txt "extracted" c
      cleanShellNoise (txt "raw" c) `shouldBe` txt "cleaned" c
  describe "_get_env_config" $
    forM_ (listOf (field "config" doc)) $ \c -> it (show (strDict (field "env" c))) $ do
      let env = strDict (field "env" c)
      r <- getEnvConfig (\k -> pure (lookup k env)) "<CWD>"
      case r of
        Left e -> ("ValueError: " <> e) `shouldBe` txt "error" c
        Right cfg -> envConfigDict cfg `shouldBe` field "config" c
  describe "ssh command line" $
    forM_ (listOf (field "ssh_argv" doc)) $ \c -> it (T.unpack (txt "host" c)) $ do
      let o = SshOpts (txt "host" c) (txt "user" c) (intOf (field "port" c)) (txt "key" c) "~" 60 False
          sock = sshControlSocket "/tmp" o
      sshBaseArgv sock o [] `shouldBe` strList (field "base" c)
      sshBaseArgv sock o ["-t"] `shouldBe` strList (field "with" c)
  describe "docker command lines" $
    forM_ (zip [1 :: Int ..] (listOf (field "docker_argv" doc))) $ \(i, c) -> it (show i) $
      withSystemTempDirectory "b2host" $ \hostDir -> do
        let sub = substitute "<HOSTDIR>" (T.pack hostDir)
            a = sub (field "args" c)
            runsWant = listOf (sub (field "runs" c))
        calls <- newIORef []
        let host =
              DockerHost
                { dhPlatform = "linux"
                , dhRun = \argv -> do
                    modifyIORef calls (<> [argv])
                    pure $ case drop 1 argv of
                      ("info" : _) -> (0, "overlay2\n")
                      ("create" : _) -> (0, "hello123\n")
                      _ -> (0, "")
                , dhSandboxDir = pure "<SANDBOX>"
                , dhIsDir = \p -> pure (p == T.pack hostDir)
                , dhMkdirs = \_ -> pure ()
                , dhWarn = \_ -> pure ()
                }
            o =
              defaultDockerOpts
                { doImage = txt "image" a
                , doCwd = if txt "cwd" a == "~" then "/root" else txt "cwd" a
                , doTimeout = fromIntegral (intOf (field "timeout" a))
                , doCpu = case field "cpu" a of PFloat d -> d; PInt n -> fromIntegral n; _ -> 0
                , doMemory = intOf (field "memory" a)
                , doDisk = intOf (field "disk" a)
                , doPersistent = boolOf (field "persistent_filesystem" a)
                , doTaskId = txt "task_id" a
                , doVolumes = case field "volumes" a of PNull -> PList []; v -> v
                , doHostCwd = case field "host_cwd" a of PStr s -> Just s; _ -> Nothing
                , doAutoMountCwd = boolOf (field "auto_mount_cwd" a)
                }
        args <- dockerRunArgs host "/usr/bin/docker" o
        made <- readIORef calls
        let wantRuns = [strList (field "run" r) | r <- runsWant, field "run" r /= PNull, take 1 (drop 1 (strList (field "run" r))) /= ["version"]]
            wantArgs = head [strList (field "run_args" (field "inner" r)) | r <- runsWant, field "inner" r /= PNull]
        made `shouldBe` wantRuns
        args `shouldBe` wantArgs
        forM_ (listOf (field "exec" c)) $ \e -> do
          let stdin = case field "stdin" e of PStr s -> Just s; _ -> Nothing
              workDir = if T.null (txt "cwd" e) then doCwd o else txt "cwd" e
              (cmd, _) = transformSudo "" (txt "command" e)
          dockerExecArgv "/usr/bin/docker" "cid123" workDir cmd (stdin /= Nothing) `shouldBe` strList (field "argv" e)
          (if maybe False (not . T.null) stdin then "PIPE" else "DEVNULL" :: Text) `shouldBe` txt "stdin_mode" e
  where
    substitute k v = \case
      PStr s -> PStr (T.replace k v s)
      PList xs -> PList (map (substitute k v) xs)
      PDict xs -> PDict [(a, substitute k v b) | (a, b) <- xs]
      x -> x
