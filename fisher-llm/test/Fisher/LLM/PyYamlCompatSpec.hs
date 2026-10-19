-- | The ordered config load and save with a config from before the rename:
-- settings under @gauss:@ are read as @fisher:@, and a save keeps the old
-- section for older binaries sharing the file.
module Fisher.LLM.PyYamlCompatSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Yaml as Y
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Data.Time (utc)
import Test.Hspec

import Fisher.Config (getPath, loadConfigAt)
import Fisher.LLM.Auth.PyYaml (foldLegacyPy, loadConfigOrdered, saveConfigOrdered)
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

legacyFile :: String
legacyFile = "gauss:\n  autoformalize:\n    model: old-model\n    backend: codex\n  ratio: 3.0\nmodel: m\n"

spec :: Spec
spec = describe "loadConfigOrdered and saveConfigOrdered with a gauss: section" $ do
  it "keep gauss.autoformalize.model through a load, a save and a load" $
    withSystemTempDirectory "fisher-pyyaml" $ \home -> do
      createDirectoryIfMissing True home
      writeFile (home </> "config.yaml") legacyFile
      cfg <- loadConfigOrdered (homeSys home)
      getPath' ["fisher", "autoformalize", "model"] cfg `shouldBe` Just (PStr "old-model")
      -- the user's gauss.* beats the fisher.* default
      getPath' ["fisher", "autoformalize", "backend"] cfg `shouldBe` Just (PStr "codex")
      has "gauss" cfg `shouldBe` False
      saveConfigOrdered (homeSys home) cfg
      (loaded, _) <- loadConfigAt home
      getPath ["fisher", "autoformalize", "model"] loaded `shouldBe` Just (String "old-model")
      again <- loadConfigOrdered (homeSys home)
      getPath' ["fisher", "autoformalize", "model"] again `shouldBe` Just (PStr "old-model")

  it "write a changed setting under fisher and keep the file's gauss section as it was" $
    withSystemTempDirectory "fisher-pyyaml" $ \home -> do
      createDirectoryIfMissing True home
      writeFile (home </> "config.yaml") legacyFile
      fileBefore <- Y.decodeFileThrow (home </> "config.yaml") :: IO Value
      cfg <- loadConfigOrdered (homeSys home)
      let fisherSec = maybe (PDict []) id (get "fisher" cfg)
          af = maybe (PDict []) id (get "autoformalize" fisherSec)
          changed = set "fisher" (set "autoformalize" (set "model" (PStr "new-model") af) fisherSec) cfg
      saveConfigOrdered (homeSys home) changed
      fileAfter <- Y.decodeFileThrow (home </> "config.yaml") :: IO Value
      getPath ["gauss"] fileAfter `shouldBe` getPath ["gauss"] fileBefore
      getPath ["fisher", "autoformalize", "model"] fileAfter `shouldBe` Just (String "new-model")
      reloaded <- loadConfigOrdered (homeSys home)
      getPath' ["fisher", "autoformalize", "model"] reloaded `shouldBe` Just (PStr "new-model")

  it "folds gauss into fisher, fisher winning, in place" $ do
    let cfg = PDict [("gauss", PDict [("a", PInt 1), ("b", PInt 2)]), ("x", PInt 0), ("fisher", PDict [("b", PInt 3)])]
    foldLegacyPy cfg `shouldBe` PDict [("x", PInt 0), ("fisher", PDict [("a", PInt 1), ("b", PInt 3)])]
    foldLegacyPy (PDict [("gauss", PDict [("a", PInt 1)]), ("x", PInt 0)])
      `shouldBe` PDict [("fisher", PDict [("a", PInt 1)]), ("x", PInt 0)]
  where
    getPath' = PV.getPath
