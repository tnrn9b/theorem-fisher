-- | @config set@ against the pinned upstream's @set_config_value@.
module Fisher.ConfigSetSpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (Value)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Y
import Data.Bits ((.&.))
import System.Directory (createDirectoryIfMissing, createFileLink, doesFileExist, pathIsSymbolicLink)
import System.Posix.Files (fileMode, getFileStatus, setFileMode)
import System.FilePath ((</>))
import Test.Hspec

import Fisher.Config.Set
import Fisher.Golden
import Fisher.TestUtil (withTmp)

spec :: Spec
spec = describe "config-set.json" $ do
  cases <- runIO (goldenCases "test/golden/a1/config-set.json")
  forM_ cases $ \c -> it (T.unpack (textField "name" c)) $ withTmp $ \tmp -> do
    let home = tmp </> "gauss-home"
    createDirectoryIfMissing True home
    forM_ (field "input" c :: Maybe Text) $ \t -> BS.writeFile (home </> "config.yaml") (TE.encodeUtf8 t)
    r <- setConfigValueAt home (textField "key" c) (textField "value" c)
    case (r, field "stdout" c :: Maybe Text) of
      (Right line, Just expected) -> T.replace (T.pack tmp) "<TMP>" (line <> "\n") `shouldBe` expected
      (Left _, Nothing) -> pure ()  -- upstream raised; Fisher refuses
      (got, want) -> expectationFailure ("got " <> show got <> ", upstream printed " <> show want)
    hasCfg <- doesFileExist (home </> "config.yaml")
    saved <- if hasCfg then Just <$> (Y.decodeFileThrow (home </> "config.yaml") :: IO Value) else pure Nothing
    saved `shouldBe` (field "saved" c :: Maybe Value)
    hasEnv <- doesFileExist (home </> ".env")
    env <- if hasEnv then Just <$> TIO.readFile (home </> ".env") else pure Nothing
    env `shouldBe` (field "env" c :: Maybe Text)
  describe "setConfigValueAt's write (Fisher: a temporary file renamed over it)" $ do
    it "keeps an existing file's mode" $ withTmp $ \tmp -> do
      let home = tmp </> "gauss-home"
          cfg = home </> "config.yaml"
      createDirectoryIfMissing True home
      BS.writeFile cfg "model: a\n"
      setFileMode cfg 0o644
      _ <- setConfigValueAt home "fisher.context" "clm"
      ((.&. 0o777) . fileMode <$> getFileStatus cfg) `shouldReturn` 0o644
    it "gives a new file mode 0600" $ withTmp $ \tmp -> do
      let home = tmp </> "gauss-home"
      createDirectoryIfMissing True home
      _ <- setConfigValueAt home "fisher.context" "clm"
      ((.&. 0o777) . fileMode <$> getFileStatus (home </> "config.yaml")) `shouldReturn` 0o600
    it "writes through a link to the file it names, and the link stays" $ withTmp $ \tmp -> do
      let home = tmp </> "gauss-home"
          real = tmp </> "dotfiles" </> "fisher.yaml"
          cfg = home </> "config.yaml"
      createDirectoryIfMissing True home
      createDirectoryIfMissing True (tmp </> "dotfiles")
      BS.writeFile real "model: a\n"
      createFileLink real cfg
      _ <- setConfigValueAt home "fisher.context" "clm"
      pathIsSymbolicLink cfg `shouldReturn` True
      saved <- Y.decodeFileThrow real :: IO Value
      saved `shouldBe` Y.object ["model" Y..= ("a" :: Text), "fisher" Y..= Y.object ["context" Y..= ("clm" :: Text)]]
