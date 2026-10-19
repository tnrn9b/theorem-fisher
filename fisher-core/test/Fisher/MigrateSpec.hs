-- | Migration against the pinned upstream's runs of @migrate_config@.
module Fisher.MigrateSpec (spec) where

import Control.Exception (finally)
import Control.Monad (forM_)
import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Yaml as Y
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (setEnv, unsetEnv)
import System.FilePath ((</>))
import Test.Hspec

import Fisher.Config.Migrate
import Fisher.Golden
import Fisher.TestUtil (withTmp)

spec :: Spec
spec = interactiveSpec >> do
  describe "migrate.json" $ do
    cases <- runIO (goldenCases "test/golden/a1/migrate.json")
    forM_ cases $ \c -> it (T.unpack (textField "name" c)) $ withTmp $ \tmp -> do
      let home = tmp </> "fisher-home"
          envVars = case field "env" c :: Value of
            Object o -> [(K.toString k, T.unpack v) | (k, String v) <- KM.toList o]
            _ -> []
      createDirectoryIfMissing True home
      forM_ (field "input" c :: Maybe Text) $ \t -> BS.writeFile (home </> "config.yaml") (TE.encodeUtf8 t)
      let persisted = textField "persisted_env" c
      if T.null persisted then pure () else BS.writeFile (home </> ".env") (TE.encodeUtf8 persisted)
      mapM_ (uncurry setEnv) envVars
      out <- newIORef []
      result <- migrateConfigAt home False False noPrompter (\l -> modifyIORef' out (<> [l]))
        `finally` mapM_ (unsetEnv . fst) envVars
      let expected = field "results" c :: Value
      envAdded result `shouldBe` field "env_added" expected
      configAdded result `shouldBe` field "config_added" expected
      migrationWarnings result `shouldBe` field "warnings" expected
      printed <- T.unlines <$> readIORef out
      printed `shouldBe` textField "stdout" c
      saved <- do
        exists <- doesFileExist (home </> "config.yaml")
        if exists then Just <$> (Y.decodeFileThrow (home </> "config.yaml") :: IO Value) else pure Nothing
      saved `shouldBe` (field "saved" c :: Maybe Value)

  it "renders nested defaults the way Python's str() does" $
    pyStrDeep (Object (KM.fromList [("a", Number 1), ("b", Array mempty), ("c", String "x")]))
      `shouldBe` "{'a': 1, 'b': [], 'c': 'x'}"

-- The interactive path, with every prompt answered by an empty line (what
-- a closed stdin gives): the version is still bumped and nothing is saved
-- to .env.
interactiveSpec :: Spec
interactiveSpec = it "bumps the version when every interactive prompt is declined" $ withTmp $ \tmp -> do
  let home = tmp </> "fisher-home"
  createDirectoryIfMissing True home
  BS.writeFile (home </> "config.yaml") "_config_version: 3\n"
  out <- newIORef []
  _ <- migrateConfigAt home True False noPrompter (\l -> modifyIORef' out (<> [l]))
  printed <- readIORef out
  printed `shouldSatisfy` any ("new optional key(s) in this update:" `T.isInfixOf`)
  saved <- Y.decodeFileThrow (home </> "config.yaml") :: IO Value
  case saved of
    Object o -> KM.lookup "_config_version" o `shouldBe` Just (Number 9)
    _ -> expectationFailure "saved config is not a mapping"
  doesFileExist (home </> ".env") `shouldReturn` False
