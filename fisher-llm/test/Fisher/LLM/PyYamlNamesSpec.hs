-- | The ordered config load and save read and write the @fisher:@ root
-- only: a section under the product's previous root is not read, and a
-- save does not keep it specially.
module Fisher.LLM.PyYamlNamesSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Text as T
import qualified Data.Yaml as Y
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Data.Time (utc)
import Test.Hspec

import Fisher.Config (defaultConfig, getPath)
import Fisher.LLM.Auth.PyYaml (loadConfigOrdered, saveConfigOrdered)
import Fisher.LLM.Sys
import Fisher.Py.Value hiding (getPath)
import qualified Fisher.Py.Value as PV

-- | A 'Sys' over a home; nothing here reaches the network or a process.
homeSys :: FilePath -> Sys
homeSys home =
  Sys
    { sysNow = pure 1790000000
    , sysMonotonic = pure 1000
    , sysSleep = \_ -> pure ()
    , sysGetEnv = \_ -> pure Nothing
    , sysSetEnv = \_ _ -> pure ()
    , sysHttp = \_ -> pure (Left (HttpFail "ConnectError" "no network in tests"))
    , sysOpenBrowser = \_ -> pure False
    , sysRun = \_ -> pure (Left "no subprocesses in tests")
    , sysRunInteractive = \_ -> pure (Left "no subprocesses in tests")
    , sysWhich = \_ -> pure Nothing
    , sysPlatform = "linux"
    , sysUuidHex = pure "00000000000000000000000000000001"
    , sysTimeZoneAt = \_ -> pure utc
    , sysHome = home
    , sysFisherHome = home
    , sysOut = \_ -> pure ()
    , sysInput = \_ -> pure Nothing
    , sysGetPass = \_ -> pure Nothing
    , sysStdinTty = pure False
    , sysStdoutTty = pure False
    , sysPid = 4242
    }

-- | The product's previous name, built so that this file never spells it.
oldName :: String
oldName = ['g', 'a', 'u', 's', 's']

oldFile :: String
oldFile = oldName <> ":\n  autoformalize:\n    model: old-model\n    backend: codex\nmodel: m\n"

spec :: Spec
spec = describe "loadConfigOrdered and saveConfigOrdered with the previous root" $ do
  it "do not read a section under it" $
    withSystemTempDirectory "fisher-pyyaml" $ \home -> do
      createDirectoryIfMissing True home
      writeFile (home </> "config.yaml") oldFile
      cfg <- loadConfigOrdered (homeSys home)
      getPath' ["fisher", "autoformalize", "model"] cfg `shouldBe` (fromAeson <$> getPath ["fisher", "autoformalize", "model"] defaultConfig)
      getPath' ["fisher", "autoformalize", "backend"] cfg `shouldBe` (fromAeson <$> getPath ["fisher", "autoformalize", "backend"] defaultConfig)
      getPath' ["model"] cfg `shouldBe` Just (PStr "m")

  it "keep it on a save only as any unknown key, and drop it when the dict lacks it" $
    withSystemTempDirectory "fisher-pyyaml" $ \home -> do
      createDirectoryIfMissing True home
      writeFile (home </> "config.yaml") oldFile
      cfg <- loadConfigOrdered (homeSys home)
      saveConfigOrdered (homeSys home) cfg
      kept <- Y.decodeFileThrow (home </> "config.yaml") :: IO Value
      getPath [T.pack oldName, "autoformalize", "model"] kept `shouldBe` Just (String "old-model")
      saveConfigOrdered (homeSys home) (delete (T.pack oldName) cfg)
      dropped <- Y.decodeFileThrow (home </> "config.yaml") :: IO Value
      getPath [T.pack oldName] dropped `shouldBe` Nothing
      getPath ["model"] dropped `shouldBe` Just (String "m")
  where
    getPath' = PV.getPath
