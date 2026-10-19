module Fisher.ConfigSpec (spec) where

import Control.Exception (ErrorCall (..), throwIO, try)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BC
import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.Yaml as Y
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (setEnv, unsetEnv)
import System.FilePath ((</>))
import System.Posix.Files (fileMode, getFileStatus, intersectFileModes)
import Test.Hspec

import Fisher.Atomic (atomicWriteWith)
import Fisher.Config
import Fisher.Paths
import Fisher.TestUtil (withTmp)

-- | Set a dotted path, creating mappings on the way, as the Python tests do
-- with @config["agent"]["max_turns"] = 42@.
setPath :: [String] -> Value -> Value -> Value
setPath [] new _ = new
setPath (k : ks) new (Object o) =
  let key = K.fromString k
   in Object (KM.insert key (setPath ks new (maybe (Object KM.empty) id (KM.lookup key o))) o)
setPath _ new _ = new

spec :: Spec
spec = do
  -- Ported from tests/gauss_cli/test_config.py.
  describe "home" $ do
    -- The whole fallback chain (GAUSS_HOME, ~/.gauss) is in CompatSpec.
    it "follows FISHER_HOME" $ do
      setEnv "FISHER_HOME" "/custom/path"
      getFisherHome `shouldReturn` "/custom/path"
      unsetEnv "FISHER_HOME"

    it "creates the subdirectories and a SOUL.md" $ withTmp $ \tmp -> do
      ensureFisherHomeAt tmp
      mapM_ (\d -> doesDirectoryExist (tmp </> d) `shouldReturn` True) ["cron", "sessions", "logs", "memories"]
      soul <- readFile (tmp </> "SOUL.md")
      soul `shouldNotBe` ""

    it "leaves an existing SOUL.md alone" $ withTmp $ \tmp -> do
      writeFile (tmp </> "SOUL.md") "custom soul"
      ensureFisherHomeAt tmp
      readFile (tmp </> "SOUL.md") `shouldReturn` "custom soul"

  describe "load" $ do
    it "returns the defaults when there is no file" $ withTmp $ \tmp -> do
      (cfg, warnings) <- loadConfigAt tmp
      warnings `shouldBe` []
      getPath ["model"] cfg `shouldBe` getPath ["model"] defaultConfig
      getPath ["agent", "max_turns"] cfg `shouldBe` getPath ["agent", "max_turns"] defaultConfig
      getPath ["max_turns"] cfg `shouldBe` Nothing
      getPath ["terminal", "backend"] cfg `shouldBe` Just (String "local")

    it "moves a legacy root-level max_turns under agent" $ withTmp $ \tmp -> do
      writeFile (tmp </> "config.yaml") "max_turns: 42\n"
      (cfg, _) <- loadConfigAt tmp
      getPath ["agent", "max_turns"] cfg `shouldBe` Just (Number 42)
      getPath ["max_turns"] cfg `shouldBe` Nothing

  describe "save and load" $ do
    it "round-trips" $ withTmp $ \tmp -> do
      (cfg, _) <- loadConfigAt tmp
      let cfg' = setPath ["agent", "max_turns"] (Number 42) (setPath ["model"] (String "test/custom-model") cfg)
      saveConfigAt tmp cfg'
      (back, _) <- loadConfigAt tmp
      getPath ["model"] back `shouldBe` Just (String "test/custom-model")
      getPath ["agent", "max_turns"] back `shouldBe` Just (Number 42)
      saved <- Y.decodeFileThrow (tmp </> "config.yaml") :: IO Value
      getPath ["agent", "max_turns"] saved `shouldBe` Just (Number 42)
      getPath ["max_turns"] saved `shouldBe` Nothing

    it "normalizes a legacy root-level max_turns on save" $ withTmp $ \tmp -> do
      saveConfigAt tmp (object ["model" .= ("test/custom-model" :: String), "max_turns" .= (37 :: Int)])
      saved <- Y.decodeFileThrow (tmp </> "config.yaml") :: IO Value
      getPath ["agent", "max_turns"] saved `shouldBe` Just (Number 37)
      getPath ["max_turns"] saved `shouldBe` Nothing

    it "keeps nested values" $ withTmp $ \tmp -> do
      (cfg, _) <- loadConfigAt tmp
      saveConfigAt tmp (setPath ["terminal", "timeout"] (Number 999) cfg)
      (back, _) <- loadConfigAt tmp
      getPath ["terminal", "timeout"] back `shouldBe` Just (Number 999)

    it "keeps the old file and leaves no temp file when a write fails" $ withTmp $ \tmp -> do
      (cfg, _) <- loadConfigAt tmp
      saveConfigAt tmp (setPath ["model"] (String "original-model") cfg)
      r <- try (atomicWriteWith ".config_.tmp" (tmp </> "config.yaml") (\_ -> throwIO (ErrorCall "disk full")))
      r `shouldBe` Left (ErrorCall "disk full")
      (back, _) <- loadConfigAt tmp
      getPath ["model"] back `shouldBe` Just (String "original-model")
      names <- listDirectory tmp
      filter (\n -> "." `isPrefixOf` n && ".tmp" `isSuffixOf` n) names `shouldBe` []

    it "writes the commented sections only when those features are unset" $ do
      let bare = BC.unpack (renderConfig (object []))
          configured =
            BC.unpack . renderConfig $
              object
                [ "security" .= object ["redact_secrets" .= False]
                , "fallback_model" .= object ["provider" .= ("openrouter" :: String), "model" .= ("m" :: String)] ]
      bare `shouldContain` "# security:"
      bare `shouldContain` "# fallback_model:"
      configured `shouldNotContain` "# security:"
      configured `shouldNotContain` "# fallback_model:"

    it "makes the saved file owner-only" $ withTmp $ \tmp -> do
      saveConfigAt tmp (object [])
      st <- getFileStatus (tmp </> "config.yaml")
      (fileMode st `intersectFileModes` 0o777) `shouldBe` 0o600

  describe "deepMerge" $ do
    it "keeps sibling defaults under a partial override" $ do
      let base = object ["tts" .= object ["voice" .= ("a" :: String), "model" .= ("m" :: String)]]
          over = object ["tts" .= object ["voice" .= ("b" :: String)]]
      deepMerge base over `shouldBe` object ["tts" .= object ["voice" .= ("b" :: String), "model" .= ("m" :: String)]]

    it "lets a scalar replace a mapping" $
      deepMerge (object ["a" .= object ["b" .= (1 :: Int)]]) (object ["a" .= (3 :: Int)])
        `shouldBe` object ["a" .= (3 :: Int)]

  it "carries the pinned upstream's config version" $
    latestConfigVersion `shouldBe` 9

  it "falls back to the defaults, with a warning, on a non-mapping file" $ withTmp $ \tmp -> do
    writeFile (tmp </> "config.yaml") "- a\n- b\n"
    (cfg, warnings) <- loadConfigAt tmp
    length warnings `shouldBe` 1
    getPath ["model"] cfg `shouldBe` getPath ["model"] defaultConfig
    doesFileExist (tmp </> "config.yaml") `shouldReturn` True
