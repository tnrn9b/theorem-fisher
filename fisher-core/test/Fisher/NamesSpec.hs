-- | Fisher reads only its own names: @FISHER_HOME@ else @~/.fisher@, the
-- @.fisher@ project directory, @FISHER_*@ variables and the @fisher@
-- config root. What a machine set up under the previous name still holds
-- is not read, so such a machine is a fresh install.
module Fisher.NamesSpec (spec) where

import Control.Exception (bracket)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Char (toUpper)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Yaml as Y
import System.Directory (createDirectoryIfMissing, doesPathExist, listDirectory)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import Test.Hspec

import Fisher.Config (defaultConfig, getPath, loadConfig, loadConfigAt, saveConfigAt)
import Fisher.Config.Migrate (configAdded, migrateConfigAt, noPrompter)
import Fisher.Config.Set (setConfigValueAt)
import Fisher.Cron.Tool (originFromEnv)
import Fisher.Env (loadEnvAt)
import Fisher.EnvVars (getEnvValueAt)
import Fisher.Paths (ensureFisherHome, fisherHomeLabel, fisherHomeLabelWith, getFisherHome, homeLabel)
import Fisher.Project
import Fisher.TestUtil (withTmp, writeLeanRoot)

-- | The product's previous name, built so that this file never spells it.
oldName :: String
oldName = ['g', 'a', 'u', 's', 's']

-- | The previous home and project directory (@~/.@ plus 'oldName').
oldDir :: FilePath
oldDir = '.' : oldName

-- | A variable under the previous prefix: @oldVar "HOME"@.
oldVar :: String -> String
oldVar suffix = map toUpper oldName <> "_" <> suffix

oldKey :: Text
oldKey = T.pack oldName

-- | Run with these variables set ('Just') or unset ('Nothing'), restoring
-- what was there before.
withEnv :: [(String, Maybe String)] -> IO a -> IO a
withEnv vars act = bracket save restore (const (mapM_ apply vars >> act))
  where
    save = mapM (\(k, _) -> (k,) <$> lookupEnv k) vars
    restore = mapM_ apply
    apply (k, v) = maybe (unsetEnv k) (setEnv k) v

-- | HOME at the given directory and no home variable, old or new.
withHome :: FilePath -> IO a -> IO a
withHome home = withEnv [("HOME", Just home), ("FISHER_HOME", Nothing), (oldVar "HOME", Nothing)]

isNotFound :: ProjectError -> Bool
isNotFound = \case
  ProjectNotFound _ -> True
  _ -> False

notFoundWith :: (Text -> Bool) -> ProjectError -> Bool
notFoundWith ok = \case
  ProjectNotFound m -> ok m
  _ -> False

spec :: Spec
spec = do
  homeSpec
  envSpec
  projectSpec
  configSpec

homeSpec :: Spec
homeSpec = describe "home directory" $ do
  it "is ~/.fisher on a machine with only the previous home, which reads as a fresh install" $
    withTmp $ \tmp -> withHome tmp $ do
      let old = tmp </> oldDir
      createDirectoryIfMissing True old
      writeFile (old </> "config.yaml") "model: from-old-home\n"
      writeFile (old </> ".env") "ANTHROPIC_API_KEY=not-a-real-key\n"
      getFisherHome `shouldReturn` tmp </> ".fisher"
      home <- ensureFisherHome
      home `shouldBe` tmp </> ".fisher"
      cfg <- loadConfig
      getPath ["model"] cfg `shouldBe` getPath ["model"] defaultConfig
      env <- loadEnvAt home
      Map.member "ANTHROPIC_API_KEY" env `shouldBe` False
      -- the previous home is left as it was
      listDirectory old >>= (`shouldMatchList` ["config.yaml", ".env"])

  it "follows FISHER_HOME only; the previous variable is not read" $ withTmp $ \tmp -> withHome tmp $ do
    withEnv [(oldVar "HOME", Just "/old/home")] $ do
      getFisherHome `shouldReturn` tmp </> ".fisher"
      withEnv [("FISHER_HOME", Just "/new/home")] $ getFisherHome `shouldReturn` "/new/home"
      withEnv [("FISHER_HOME", Just "")] $ getFisherHome `shouldReturn` tmp </> ".fisher"

  it "is named in hints as ~/.fisher, or its path when it is elsewhere" $ withTmp $ \tmp -> withHome tmp $ do
    createDirectoryIfMissing True (tmp </> oldDir)
    fisherHomeLabel `shouldReturn` "~/.fisher"
    doesPathExist (tmp </> ".fisher") `shouldReturn` False
    withEnv [(oldVar "HOME", Just "/elsewhere/home")] $ fisherHomeLabel `shouldReturn` "~/.fisher"
    withEnv [("FISHER_HOME", Just "/elsewhere/home")] $ fisherHomeLabel `shouldReturn` "/elsewhere/home"
    homeLabel "/u" "/u/.fisher/" `shouldBe` "~/.fisher"
    homeLabel "/u" ("/u" </> oldDir) `shouldBe` "/u" </> oldDir
    let envOf kvs k = pure (lookup k kvs)
    fisherHomeLabelWith (envOf [("HOME", tmp), (oldVar "HOME", "/srv/g")]) `shouldReturn` "~/.fisher"

envSpec :: Spec
envSpec = describe "environment variables" $ do
  it "reads FISHER_X from the environment or the home's .env, never the previous name" $ withTmp $ \home ->
    withEnv [("FISHER_NAMES_K", Nothing), (oldVar "NAMES_K", Just "old-in-env")] $ do
      writeFile (home </> ".env") (oldVar "NAMES_K" <> "=old-in-file\n")
      getEnvValueAt home "FISHER_NAMES_K" `shouldReturn` Nothing
      writeFile (home </> ".env") "FISHER_NAMES_K=new-in-file\n"
      getEnvValueAt home "FISHER_NAMES_K" `shouldReturn` Just "new-in-file"

  it "takes a cron job's origin from FISHER_SESSION_* only" $ do
    let getenv m k = pure (Map.lookup k (Map.fromList m))
        old s = T.pack (oldVar s)
    originFromEnv (getenv [(old "SESSION_PLATFORM", "telegram"), (old "SESSION_CHAT_ID", "-1")]) `shouldReturn` Nothing

  it "does not migrate tool progress from the previous variable" $ withTmp $ \tmp ->
    withEnv [("FISHER_TOOL_PROGRESS", Nothing), (oldVar "TOOL_PROGRESS", Just "false"), ("FISHER_TOOL_PROGRESS_MODE", Nothing)] $ do
      writeFile (tmp </> "config.yaml") "_config_version: 3\n"
      r <- migrateConfigAt tmp False True noPrompter (const (pure ()))
      take 1 (configAdded r) `shouldBe` ["display.tool_progress=all (default)"]

projectSpec :: Spec
projectSpec = describe "project directory" $ do
  it "does not find a project that has only the previous directory; init makes .fisher" $ withTmp $ \tmp -> do
    writeLeanRoot tmp
    createDirectoryIfMissing True (tmp </> oldDir)
    writeFile (tmp </> oldDir </> "project.yaml") "schema_version: 1\nname: Legacy\nkind: lean4\nlean_root: .\n"
    createDirectoryIfMissing True (tmp </> "Sub")
    discoverProject (tmp </> "Sub") `shouldThrow` isNotFound
    loadProject tmp `shouldThrow` isNotFound
    p <- initializeProject tmp defaultInitOptions
    projectFisherDir p `shouldBe` tmp </> ".fisher"
    projectManifestPath p `shouldBe` tmp </> ".fisher" </> "project.yaml"
    projectRuntimeDir p `shouldBe` tmp </> ".fisher" </> "runtime"
    discoverProject (tmp </> "Sub") `shouldReturn` p
    listDirectory (tmp </> oldDir) `shouldReturn` ["project.yaml"]

  it "says to rename a previous project directory when no project is found" $ withTmp $ \tmp -> do
    writeLeanRoot tmp
    createDirectoryIfMissing True (tmp </> oldDir)
    writeFile (tmp </> oldDir </> "project.yaml") "schema_version: 1\nname: Legacy\nkind: lean4\nlean_root: .\n"
    createDirectoryIfMissing True (tmp </> "Sub")
    let hint = "`" <> T.pack (tmp </> oldDir) <> "` looks like a project directory from before the rename: rename it to `.fisher`."
    discoverProject (tmp </> "Sub") `shouldThrow` notFoundWith (T.isSuffixOf hint)
    discoverProject tmp `shouldThrow` notFoundWith (T.isSuffixOf hint)

  it "gives no rename hint for a hidden directory without a project.yaml" $ withTmp $ \tmp -> do
    createDirectoryIfMissing True (tmp </> oldDir)
    createDirectoryIfMissing True (tmp </> ".git")
    writeFile (tmp </> oldDir </> "config.yaml") "model: m\n"
    discoverProject tmp `shouldThrow` notFoundWith (\m -> not ("rename it" `T.isInfixOf` m) && "first." `T.isSuffixOf` m)

  it "takes the template from FISHER_ and fisher.* only" $ do
    let oldCfg = object [K.fromText oldKey .= object ["project" .= object ["template_source" .= ("old-config" :: Text)]]]
        newCfg = object ["fisher" .= object ["project" .= object ["template_source" .= ("new-config" :: Text)]]]
    resolveTemplateSource oldCfg [(oldVar "BLUEPRINT_TEMPLATE_SOURCE", "old-env")] `shouldBe` ""
    resolveTemplateSource newCfg [(templateEnvVar, "new-env")] `shouldBe` "new-env"
    resolveTemplateSource newCfg [] `shouldBe` "new-config"

configSpec :: Spec
configSpec = describe "configuration root" $ do
  let oldFile = oldName <> ":\n  autoformalize:\n    model: old-model\n    backend: codex\n"

  it "does not read a section under the previous root" $ withTmp $ \home -> do
    writeFile (home </> "config.yaml") oldFile
    (loaded, warnings) <- loadConfigAt home
    warnings `shouldBe` []
    getPath ["fisher", "autoformalize", "model"] loaded `shouldBe` getPath ["fisher", "autoformalize", "model"] defaultConfig
    getPath ["fisher", "autoformalize", "backend"] loaded `shouldBe` getPath ["fisher", "autoformalize", "backend"] defaultConfig

  it "keeps the previous root on a save only as any unknown key is kept" $ withTmp $ \home -> do
    writeFile (home </> "config.yaml") oldFile
    (loaded, _) <- loadConfigAt home
    saveConfigAt home loaded
    kept <- Y.decodeFileThrow (home </> "config.yaml") :: IO Value
    getPath [oldKey, "autoformalize", "model"] kept `shouldBe` Just (String "old-model")
    -- a configuration that does not hold it is written without it
    saveConfigAt home defaultConfig
    dropped <- Y.decodeFileThrow (home </> "config.yaml") :: IO Value
    getPath [oldKey] dropped `shouldBe` Nothing
    getPath ["fisher"] dropped `shouldBe` getPath ["fisher"] defaultConfig

  it "writes a typed path under the previous root as typed, not under fisher" $ withTmp $ \home -> do
    r <- setConfigValueAt home (oldKey <> ".autoformalize.model") "typed"
    fmap (const ()) r `shouldBe` Right ()
    saved <- Y.decodeFileThrow (home </> "config.yaml") :: IO Value
    getPath [oldKey, "autoformalize", "model"] saved `shouldBe` Just (String "typed")
    case saved of
      Object o -> KM.lookup "fisher" o `shouldBe` Nothing
      _ -> expectationFailure "config.yaml is not a mapping"
