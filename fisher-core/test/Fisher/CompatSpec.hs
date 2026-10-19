-- | The names from before the rename (Gauss) read as a fallback: every
-- fallback in the rename plan, new name first and old name second.
module Fisher.CompatSpec (spec) where

import Control.Exception (bracket)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Yaml as Y
import System.Directory (createDirectoryIfMissing, doesPathExist)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.Process.Typed (proc, readProcessStdout_, setEnv)
import Test.Hspec

import Fisher.Compat
import Fisher.Config (getPath, loadConfig, loadConfigAt, saveConfig, saveConfigAt)
import Fisher.Config.Migrate (configAdded, migrateConfigAt, noPrompter)
import Fisher.Config.Set (setConfigValueAt)
import Fisher.Cron.Tool (originFromEnv)
import Fisher.Env (loadEnvAt)
import Fisher.EnvVars (getEnvValueAt, getEnvValueNamedAt)
import Fisher.Paths (ensureFisherHome, fisherHomeLabel, fisherHomeLabelWith, getFisherHome, homeLabel)
import Fisher.Project
import Fisher.Py.Value (PyVal (..))
import Fisher.TestUtil (withTmp, writeLeanRoot)

-- | Run with these variables set ('Just') or unset ('Nothing'), restoring
-- what was there before.
withEnv :: [(String, Maybe String)] -> IO a -> IO a
withEnv vars act = bracket save restore (const (mapM_ apply vars >> act))
  where
    save = mapM (\(k, _) -> (k,) <$> lookupEnv k) vars
    restore = mapM_ apply
    apply (k, v) = maybe (unsetEnv k) (System.Environment.setEnv k) v

-- | A clean environment for the home chain: HOME at the given directory,
-- neither home variable set.
withHome :: FilePath -> IO a -> IO a
withHome home = withEnv [("HOME", Just home), ("FISHER_HOME", Nothing), ("GAUSS_HOME", Nothing)]

manifestIn :: FilePath -> [String]
manifestIn dir =
  [ "schema_version: 1", "name: Legacy", "kind: lean4", "lean_root: ."
  , "created_at: 2026-03-18T00:00:00+00:00", "source:", "  mode: init", "  template_source: ''"
  , "blueprint:", "  markers: []", "paths:", "  runtime: " <> dir <> "/runtime", "  cache: " <> dir <> "/cache"
  , "  workflows: " <> dir <> "/workflows" ]

spec :: Spec
spec = do
  envSpec
  homeSpec
  projectSpec
  configSpec
  backendSpec

envSpec :: Spec
envSpec = describe "environment variables" $ do
  it "names the new and the old variable from either name or the suffix" $ do
    envNames "FISHER_HOME" `shouldBe` ("FISHER_HOME", "GAUSS_HOME")
    envNames "GAUSS_HOME" `shouldBe` ("FISHER_HOME", "GAUSS_HOME")
    envNames "HOME" `shouldBe` ("FISHER_HOME", "GAUSS_HOME")

  it "reads FISHER_X, else GAUSS_X" $ do
    withEnv [("FISHER_COMPAT_T", Nothing), ("GAUSS_COMPAT_T", Just "old")] $ do
      lookupEnvCompat "FISHER_COMPAT_T" `shouldReturn` Just "old"
      lookupEnvCompat "COMPAT_T" `shouldReturn` Just "old"
      lookupEnvCompatNamed "FISHER_COMPAT_T" `shouldReturn` Just ("GAUSS_COMPAT_T", "old")
    withEnv [("FISHER_COMPAT_T", Just "new"), ("GAUSS_COMPAT_T", Just "old")] $
      lookupEnvCompat "FISHER_COMPAT_T" `shouldReturn` Just "new"
    withEnv [("FISHER_COMPAT_T", Nothing), ("GAUSS_COMPAT_T", Nothing)] $
      lookupEnvCompat "FISHER_COMPAT_T" `shouldReturn` Nothing

  it "reads FISHER_X, else GAUSS_X, from an environment list" $ do
    lookupCompat "FISHER_X" [("GAUSS_X", "old")] `shouldBe` Just "old"
    lookupCompat "FISHER_X" [("GAUSS_X", "old"), ("FISHER_X", "new")] `shouldBe` Just "new"
    lookupCompat "FISHER_X" [] `shouldBe` Nothing

  it "gives a child both names for every FISHER_X" $ do
    let env = childEnvCompat [("PATH", "/bin"), ("FISHER_HOME", "/h"), ("GAUSS_HOME", "/stale"), ("GAUSS_ONLY", "o")]
    lookup "FISHER_HOME" env `shouldBe` Just "/h"
    lookup "GAUSS_HOME" env `shouldBe` Just "/h"
    filter ((== "GAUSS_HOME") . fst) env `shouldBe` [("GAUSS_HOME", "/h")]
    lookup "GAUSS_ONLY" env `shouldBe` Just "o"
    lookup "PATH" env `shouldBe` Just "/bin"

  it "a child process that reads GAUSS_HOME sees the home Fisher set" $ do
    path <- maybe "/usr/bin:/bin" id <$> lookupEnv "PATH"
    out <- readProcessStdout_ $
      System.Process.Typed.setEnv (childEnvCompat [("PATH", path), ("FISHER_HOME", "/srv/fisher-home")]) $
        proc "sh" ["-c", "printf %s \"$GAUSS_HOME\""]
    out `shouldBe` BL8.pack "/srv/fisher-home"

  it "a child that inherits Fisher's environment sees GAUSS_HOME after exportEnvCompat" $
    withEnv [("FISHER_HOME", Just "/srv/inherited"), ("GAUSS_HOME", Nothing)] $ do
      exportEnvCompat
      lookupEnv "GAUSS_HOME" `shouldReturn` Just "/srv/inherited"
      out <- readProcessStdout_ (proc "sh" ["-c", "printf %s \"$GAUSS_HOME\""])
      out `shouldBe` BL8.pack "/srv/inherited"

  it "reads a FISHER_X or GAUSS_X setting from the environment or the home's .env" $ withTmp $ \home ->
    withEnv [("FISHER_COMPAT_K", Nothing), ("GAUSS_COMPAT_K", Nothing)] $ do
      writeFile (home </> ".env") "GAUSS_COMPAT_K=from-old-env-file\n"
      getEnvValueAt home "FISHER_COMPAT_K" `shouldReturn` Just "from-old-env-file"
      getEnvValueNamedAt home "FISHER_COMPAT_K" `shouldReturn` Just ("GAUSS_COMPAT_K", "from-old-env-file")
      -- An old name asked for still prefers the new one.
      withEnv [("FISHER_COMPAT_K", Just "new")] $
        getEnvValueAt home "GAUSS_COMPAT_K" `shouldReturn` Just "new"
      writeFile (home </> ".env") "FISHER_COMPAT_K=new-in-file\n"
      withEnv [("GAUSS_COMPAT_K", Just "old-in-env")] $
        getEnvValueAt home "FISHER_COMPAT_K" `shouldReturn` Just "new-in-file"

  it "takes a cron job's origin from GAUSS_SESSION_* when FISHER_SESSION_* is unset" $ do
    let getenv m k = pure (Map.lookup k (Map.fromList m))
    origin <- originFromEnv (getenv [("GAUSS_SESSION_PLATFORM", "telegram"), ("GAUSS_SESSION_CHAT_ID", "-1")])
    origin `shouldBe` Just (PDict [("platform", PStr "telegram"), ("chat_id", PStr "-1"), ("chat_name", PNull), ("thread_id", PNull)])
    both <- originFromEnv (getenv [("GAUSS_SESSION_PLATFORM", "telegram"), ("FISHER_SESSION_PLATFORM", "slack"), ("GAUSS_SESSION_CHAT_ID", "-1")])
    both `shouldBe` Just (PDict [("platform", PStr "slack"), ("chat_id", PStr "-1"), ("chat_name", PNull), ("thread_id", PNull)])

  it "migrates tool progress from GAUSS_TOOL_PROGRESS and names it" $ withTmp $ \tmp ->
    withEnv [("FISHER_TOOL_PROGRESS", Nothing), ("GAUSS_TOOL_PROGRESS", Just "false"), ("FISHER_TOOL_PROGRESS_MODE", Nothing), ("GAUSS_TOOL_PROGRESS_MODE", Nothing)] $ do
      writeFile (tmp </> "config.yaml") "_config_version: 3\n"
      r <- migrateConfigAt tmp False True noPrompter (const (pure ()))
      take 1 (configAdded r) `shouldBe` ["display.tool_progress=off (from GAUSS_TOOL_PROGRESS=false)"]

homeSpec :: Spec
homeSpec = describe "home directory" $ do
  it "is ~/.fisher on a new machine" $ withTmp $ \tmp -> withHome tmp $
    getFisherHome `shouldReturn` tmp </> ".fisher"

  it "uses ~/.gauss when only it exists, reading its config and keys and creating no ~/.fisher" $
    withTmp $ \tmp -> withHome tmp $ do
      let old = tmp </> ".gauss"
      createDirectoryIfMissing True old
      writeFile (old </> "config.yaml") "model: from-old-home\n"
      writeFile (old </> ".env") "ANTHROPIC_API_KEY=not-a-real-key\n"
      writeFile (old </> "state.db") ""
      getFisherHome `shouldReturn` old
      home <- ensureFisherHome
      home `shouldBe` old
      cfg <- loadConfig
      getPath ["model"] cfg `shouldBe` Just (String "from-old-home")
      env <- loadEnvAt home
      Map.member "ANTHROPIC_API_KEY" env `shouldBe` True
      doesPathExist (home </> "state.db") `shouldReturn` True
      saveConfig cfg
      doesPathExist (tmp </> ".fisher") `shouldReturn` False

  it "prefers ~/.fisher when both exist" $ withTmp $ \tmp -> withHome tmp $ do
    createDirectoryIfMissing True (tmp </> ".gauss")
    createDirectoryIfMissing True (tmp </> ".fisher")
    getFisherHome `shouldReturn` tmp </> ".fisher"

  it "follows FISHER_HOME, else GAUSS_HOME, before either directory" $ withTmp $ \tmp -> withHome tmp $ do
    createDirectoryIfMissing True (tmp </> ".fisher")
    withEnv [("GAUSS_HOME", Just "/old/home")] $ do
      getFisherHome `shouldReturn` "/old/home"
      withEnv [("FISHER_HOME", Just "/new/home")] $ getFisherHome `shouldReturn` "/new/home"
      withEnv [("FISHER_HOME", Just "")] $ getFisherHome `shouldReturn` "/old/home"

  it "is named in hints as the home in use: ~/.gauss on a machine with only it" $ withTmp $ \tmp -> withHome tmp $ do
    fisherHomeLabel `shouldReturn` "~/.fisher"
    createDirectoryIfMissing True (tmp </> ".gauss")
    fisherHomeLabel `shouldReturn` "~/.gauss"
    doesPathExist (tmp </> ".fisher") `shouldReturn` False
    withEnv [("GAUSS_HOME", Just "/elsewhere/home")] $ fisherHomeLabel `shouldReturn` "/elsewhere/home"
    homeLabel "/u" "/u/.fisher/" `shouldBe` "~/.fisher"
    homeLabel "/u" "/v/.gauss" `shouldBe` "/v/.gauss"

  it "is named from a given environment too" $ withTmp $ \tmp -> withHome "/nonexistent" $ do
    createDirectoryIfMissing True (tmp </> ".gauss")
    let envOf kvs k = pure (lookup k kvs)
    fisherHomeLabelWith (envOf [("HOME", tmp)]) `shouldReturn` "~/.gauss"
    fisherHomeLabelWith (envOf [("HOME", tmp), ("FISHER_HOME", "~/.fisher")]) `shouldReturn` "~/.fisher"
    fisherHomeLabelWith (envOf [("HOME", tmp), ("GAUSS_HOME", "/srv/g")]) `shouldReturn` "/srv/g"

projectSpec :: Spec
projectSpec = describe "project directory" $ do
  it "is .fisher, else an existing .gauss, else .fisher" $ withTmp $ \tmp -> do
    projectDirName tmp `shouldReturn` ".fisher"
    createDirectoryIfMissing True (tmp </> ".gauss")
    projectDirName tmp `shouldReturn` ".gauss"
    createDirectoryIfMissing True (tmp </> ".fisher")
    projectDirName tmp `shouldReturn` ".fisher"

  it "finds and loads a project with only .gauss/project.yaml" $ withTmp $ \tmp -> do
    writeLeanRoot tmp
    createDirectoryIfMissing True (tmp </> ".gauss")
    writeFile (tmp </> ".gauss" </> "project.yaml") (unlines (manifestIn ".gauss"))
    p <- loadProject tmp
    projectFisherDir p `shouldBe` tmp </> ".gauss"
    projectManifestPath p `shouldBe` tmp </> ".gauss" </> "project.yaml"
    projectRuntimeDir p `shouldBe` tmp </> ".gauss" </> "runtime"
    createDirectoryIfMissing True (tmp </> "Sub" </> "Deeper")
    discoverProject (tmp </> "Sub" </> "Deeper") `shouldReturn` p
    loadProject (tmp </> ".gauss") `shouldReturn` p
    initializeProject tmp defaultInitOptions `shouldReturn` p
    doesPathExist (tmp </> ".fisher") `shouldReturn` False

  it "defaults the paths of a .gauss manifest that names none to .gauss" $ withTmp $ \tmp -> do
    writeLeanRoot tmp
    createDirectoryIfMissing True (tmp </> ".gauss")
    writeFile (tmp </> ".gauss" </> "project.yaml") (unlines (take 8 (manifestIn ".gauss")))
    p <- loadProject tmp
    projectWorkflowsDir p `shouldBe` tmp </> ".gauss" </> "workflows"

  it "creates new projects under .fisher" $ withTmp $ \tmp -> do
    writeLeanRoot tmp
    p <- initializeProject tmp defaultInitOptions
    projectFisherDir p `shouldBe` tmp </> ".fisher"
    projectCacheDir p `shouldBe` tmp </> ".fisher" </> "cache"
    doesPathExist (tmp </> ".gauss") `shouldReturn` False

  it "takes the template from GAUSS_ and gauss.* when the new names are unset" $ do
    let oldCfg = object ["gauss" .= object ["project" .= object ["template_source" .= ("old-config" :: Text)]]]
        bothCfg = object [ "gauss" .= object ["project" .= object ["template_source" .= ("old-config" :: Text)]]
                         , "fisher" .= object ["project" .= object ["template_source" .= ("new-config" :: Text)]] ]
    resolveTemplateSource oldCfg [(legacyTemplateEnvVar, "old-env")] `shouldBe` "old-env"
    resolveTemplateSource oldCfg [(legacyTemplateEnvVar, "old-env"), (templateEnvVar, "new-env")] `shouldBe` "new-env"
    resolveTemplateSource oldCfg [] `shouldBe` "old-config"
    resolveTemplateSource bothCfg [] `shouldBe` "new-config"

configSpec :: Spec
configSpec = describe "configuration keys" $ do
  let cfg = object
        [ "gauss" .= object ["autoformalize" .= object ["model" .= ("old-model" :: Text), "backend" .= ("codex" :: Text)]]
        , "fisher" .= object ["autoformalize" .= object ["backend" .= ("claude-code" :: Text)]] ]

  it "reads fisher.<path>, else gauss.<path>, leaf by leaf" $ do
    lookupConfigCompat ["autoformalize", "model"] cfg `shouldBe` Just (String "old-model")
    lookupConfigCompat ["autoformalize", "backend"] cfg `shouldBe` Just (String "claude-code")
    lookupConfigCompat ["autoformalize"] cfg
      `shouldBe` Just (object ["model" .= ("old-model" :: Text), "backend" .= ("claude-code" :: Text)])
    lookupConfigCompat ["nothing"] cfg `shouldBe` Nothing

  it "folds gauss into fisher, fisher winning" $
    normalizeConfigCompat cfg
      `shouldBe` object ["fisher" .= object ["autoformalize" .= object ["model" .= ("old-model" :: Text), "backend" .= ("claude-code" :: Text)]]]

  it "honours gauss.autoformalize.model in a file with no fisher.* key" $ withTmp $ \home -> do
    writeFile (home </> "config.yaml") "gauss:\n  autoformalize:\n    model: old-model\n    backend: codex\n"
    (loaded, warnings) <- loadConfigAt home
    warnings `shouldBe` []
    lookupConfigCompat ["autoformalize", "model"] loaded `shouldBe` Just (String "old-model")
    getPath ["fisher", "autoformalize", "model"] loaded `shouldBe` Just (String "old-model")
    -- The user's gauss.* setting beats the fisher.* default.
    getPath ["fisher", "autoformalize", "backend"] loaded `shouldBe` Just (String "codex")
    getPath ["gauss"] loaded `shouldBe` Nothing

  it "prefers fisher.* in a file holding both" $ withTmp $ \home -> do
    writeFile (home </> "config.yaml") "gauss:\n  autoformalize:\n    model: old\nfisher:\n  autoformalize:\n    model: new\n"
    (loaded, _) <- loadConfigAt home
    getPath ["fisher", "autoformalize", "model"] loaded `shouldBe` Just (String "new")

  it "writes under fisher" $ withTmp $ \home -> do
    saveConfigAt home cfg
    saved <- Y.decodeFileThrow (home </> "config.yaml") :: IO Value
    getPath ["fisher", "autoformalize", "model"] saved `shouldBe` Just (String "old-model")
    getPath ["gauss"] saved `shouldBe` Nothing
    r <- setConfigValueAt home "gauss.autoformalize.model" "typed"
    fmap (const ()) r `shouldBe` Right ()
    afterSet <- Y.decodeFileThrow (home </> "config.yaml") :: IO Value
    getPath ["fisher", "autoformalize", "model"] afterSet `shouldBe` Just (String "typed")
    case afterSet of
      Object o -> KM.member "gauss" o `shouldBe` False
      _ -> expectationFailure "config.yaml is not a mapping"

  it "keeps the file's gauss section as it was when it saves" $ withTmp $ \home -> do
    let file = "gauss:\n  autoformalize:\n    model: old-model\n  ratio: 3.0\n  nested:\n    list: [a, 1]\nmodel: m\n"
    writeFile (home </> "config.yaml") file
    fileBefore <- Y.decodeFileThrow (home </> "config.yaml") :: IO Value
    (loaded, _) <- loadConfigAt home
    getPath ["gauss"] loaded `shouldBe` Nothing
    saveConfigAt home loaded
    fileAfter <- Y.decodeFileThrow (home </> "config.yaml") :: IO Value
    getPath ["gauss"] fileAfter `shouldBe` getPath ["gauss"] fileBefore
    getPath ["fisher", "autoformalize", "model"] fileAfter `shouldBe` Just (String "old-model")
    -- a changed setting goes under fisher, which then wins; gauss stays
    saveConfigAt home (setIn ["fisher", "autoformalize", "model"] (String "new-model") loaded)
    final <- Y.decodeFileThrow (home </> "config.yaml") :: IO Value
    getPath ["gauss"] final `shouldBe` getPath ["gauss"] fileBefore
    (reloaded, _) <- loadConfigAt home
    lookupConfigCompat ["autoformalize", "model"] reloaded `shouldBe` Just (String "new-model")

  it "maps a typed gauss.* path to fisher.*" $ do
    configPathCompat ["gauss", "a"] `shouldBe` ["fisher", "a"]
    configPathCompat ["model"] `shouldBe` ["model"]

setIn :: [Text] -> Value -> Value -> Value
setIn [] v _ = v
setIn (k : ks) v (Object o) =
  Object (KM.insert (K.fromText k) (setIn ks v (maybe (Object KM.empty) id (KM.lookup (K.fromText k) o))) o)
setIn ks v _ = setIn ks v (Object KM.empty)

backendSpec :: Spec
backendSpec = describe "backend names" $ do
  it "reads gauss as fisher and leaves the others alone" $ do
    canonicalBackendName "gauss" `shouldBe` "fisher"
    canonicalBackendName " Gauss " `shouldBe` "fisher"
    canonicalBackendName "fisher" `shouldBe` "fisher"
    canonicalBackendName "claude-code" `shouldBe` "claude-code"
    nativeBackend `shouldBe` "fisher"
