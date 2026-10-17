-- | fisher-core against what the pinned OpenGauss did with the same inputs.
module Fisher.GoldenCoreSpec (spec) where

import Control.Exception (try)
import Control.Monad (forM_)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map


import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Yaml as Y
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Test.Hspec

import Fisher.Config
import Fisher.Env
import Fisher.Golden
import Fisher.Project
import Fisher.TestUtil (withTmp)

golden :: FilePath -> FilePath
golden name = "test/golden/a1" </> name

spec :: Spec
spec = do
  describe "config-load.json" $ do
    cases <- runIO (goldenCases (golden "config-load.json"))
    forM_ cases $ \c -> it (T.unpack (textField "name" c)) $ withTmp $ \tmp -> do
      let home = tmp </> "gauss-home"
      createDirectoryIfMissing True home
      forM_ (field "input" c :: Maybe Text) $ \t -> BS.writeFile (home </> "config.yaml") (TE.encodeUtf8 t)
      (cfg, warnings) <- loadConfigAt home
      cfg `shouldBe` (field "config" c :: Value)
      not (null warnings) `shouldBe` (field "warned" c :: Bool)

  describe "config-save.json" $ do
    cases <- runIO (goldenCases (golden "config-save.json"))
    forM_ cases $ \c -> it (T.unpack (textField "name" c)) $ withTmp $ \tmp -> do
      saveConfigAt tmp (field "input" c)
      bytes <- BS.readFile (tmp </> "config.yaml")
      saved <- either (fail . show) pure (Y.decodeEither' bytes) :: IO Value
      saved `shouldBe` (field "saved" c :: Value)
      let text = TE.decodeUtf8 bytes
      ("# security:" `T.isInfixOf` text) `shouldBe` (field "security_comment" c :: Bool)
      ("# fallback_model:" `T.isInfixOf` text) `shouldBe` (field "fallback_comment" c :: Bool)

  describe "env.json" $ do
    groups <- runIO (goldenCases (golden "env.json"))
    let g = head groups
    describe "parse" $ forM_ (field "parse" g :: [Value]) $ \c ->
      it (T.unpack (textField "name" c)) $ withTmp $ \tmp -> do
        BS.writeFile (tmp </> ".env") (TE.encodeUtf8 (textField "input" c))
        env <- loadEnvAt tmp
        env `shouldBe` (field "env" c :: Map.Map Text Text)
    describe "update" $ forM_ (field "update" g :: [Value]) $ \c ->
      it (T.unpack (textField "name" c)) $ withTmp $ \tmp -> do
        let old = textField "old" c
        if T.null old then pure () else BS.writeFile (tmp </> ".env") (TE.encodeUtf8 old)
        saveEnvValueAt tmp (textField "key" c) (textField "value" c)
        new <- TE.decodeUtf8 <$> BS.readFile (tmp </> ".env")
        new `shouldBe` textField "new" c

  describe "manifest.json" $ do
    cases <- runIO (goldenCases (golden "manifest.json"))
    forM_ cases $ \c -> it (T.unpack (textField "name" c)) $ withTmp $ \tmp -> do
      let name = textField "name" c
          proj = tmp </> "Demo"
      createDirectoryIfMissing True proj
      writeFile (proj </> "lakefile.lean") "-- lean\n"
      case field "manifest" c :: Maybe Text of
        Just text -> do
          createDirectoryIfMissing True (proj </> ".gauss")
          createDirectoryIfMissing True (proj </> "docs")
          createDirectoryIfMissing True (proj </> "sub")
          writeFile (proj </> "sub" </> "lakefile.toml") ""
          createDirectoryIfMissing True (tmp </> "Outside")
          writeFile (tmp </> "Outside" </> "lakefile.lean") ""
          BS.writeFile (proj </> ".gauss" </> "project.yaml") (TE.encodeUtf8 text)
        Nothing
          | name == "gauss-dir-no-manifest" -> createDirectoryIfMissing True (proj </> ".gauss")
          | name == "gauss-is-file" -> writeFile (proj </> ".gauss") ""
          | otherwise -> pure ()
      result <- try (loadProject proj)
      checkOutcome tmp c "ok" "error" result
      case field "discover" c :: Maybe Value of
        Nothing -> pure ()
        Just (String "ok") -> pure ()
        Just _ -> do
          r <- try (discoverProject proj)
          checkOutcome tmp c "discover-ok" "discover" r

checkOutcome :: FilePath -> Value -> Text -> Text -> Either ProjectError Project -> IO ()
checkOutcome tmp c okKey errKey = \case
  Right p -> do
    case field okKey c :: Maybe Value of
      Nothing -> expectationFailure ("loaded, but upstream refused with " <> show (field errKey c :: Value))
      Just expected -> untmp tmp (projectView p) `shouldBe` expected
    case field "created_dirs" c :: Maybe [Text] of
      Nothing -> pure ()
      Just dirs -> do
        present <- mapM (\d -> (,) d <$> isDir (tmp </> "Demo" </> T.unpack d)) dirs
        filter (not . snd) present `shouldBe` []
  Left e -> case field errKey c :: Maybe Value of
    Nothing -> expectationFailure ("refused with " <> show e <> ", but upstream loaded it")
    Just expected -> do
      let cls = textField "class" expected
          msg = textField "message" expected
          got = untmp tmp (String (projectErrorMessage e))
      errorClass e `shouldBe` cls
      if "Failed to read " `T.isPrefixOf` msg
        then fmap (T.takeWhile (/= ':')) (asText got) `shouldBe` Just (T.takeWhile (/= ':') msg)
        else got `shouldBe` String msg
  where
    isDir p = doesFileExist p >>= \f -> if f then pure False else pure True
    asText = \case
      String s -> Just s
      _ -> Nothing

errorClass :: ProjectError -> Text
errorClass = \case
  ProjectNotFound _ -> "ProjectNotFoundError"
  ProjectManifestInvalid _ -> "ProjectManifestError"
  ProjectCommandFailed _ -> "ProjectCommandError"
  ProjectTemplateUnavailable _ -> "ProjectTemplateUnavailableError"

projectView :: Project -> Value
projectView p =
  object
    [ "root" .= projectRoot p, "gauss_dir" .= projectGaussDir p
    , "manifest_path" .= projectManifestPath p, "name" .= projectName p
    , "kind" .= projectKind p, "schema_version" .= projectSchemaVersion p
    , "lean_root" .= projectLeanRoot p, "runtime_dir" .= projectRuntimeDir p
    , "cache_dir" .= projectCacheDir p, "workflows_dir" .= projectWorkflowsDir p
    , "created_at" .= projectCreatedAt p, "source_mode" .= projectSourceMode p
    , "template_source" .= projectTemplateSource p
    , "blueprint_markers" .= projectBlueprintMarkers p
    , "label" .= projectLabel p, "is_blueprint" .= isBlueprint p ]

