-- | @/project@ transcripts and doctor's managed-workflow section against
-- what the pinned upstream printed for the same setups.
module Fisher.GoldenCliSpec (spec) where

import Control.Monad (forM_, when)
import Data.Aeson (Value (..), encode)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (canonicalizePath, createDirectoryIfMissing, setPermissions, emptyPermissions, setOwnerExecutable, setOwnerReadable)
import System.FilePath (takeDirectory, (</>))
import Test.Hspec

import Fisher.Doctor
import Fisher.Env (loadEnvAt)
import Fisher.Golden
import Fisher.Project
import Fisher.ProjectCommand
import Fisher.Term (plainStyle)
import Fisher.TestUtil (withTmp)

golden :: FilePath -> FilePath
golden name = "test/golden/a1" </> name

lean :: FilePath -> [FilePath] -> IO ()
lean p extra = do
  createDirectoryIfMissing True p
  writeFile (p </> "lakefile.lean") "-- lean\n"
  forM_ extra $ \e -> do
    createDirectoryIfMissing True (takeDirectory (p </> e))
    writeFile (p </> e) "x\n"

setupLayout :: FilePath -> IO ()
setupLayout root = do
  lean (root </> "Demo") []
  createDirectoryIfMissing True (root </> "Demo" </> "Math")
  lean (root </> "Blue") ["templates/blueprint.yml", "lean-toolchain"]
  lean (root </> "A") []
  lean (root </> "B") []
  createDirectoryIfMissing True (root </> "plain")
  lean (root </> "Template") ["templates/blueprint.yml"]

-- | A golden JSON value with @<TMP>@ filled in, decoded back.
fill :: FilePath -> Value -> Value
fill tmp = \case
  String s -> String (retmp tmp s)
  Array a -> Array (fmap (fill tmp) a)
  Object o -> Object (fmap (fill tmp) o)
  v -> v

envPairs :: Value -> [(String, String)]
envPairs = \case
  Object o -> [(K.toString k, T.unpack v) | (k, String v) <- KM.toList o]
  _ -> []

spec :: Spec
spec = do
  describe "project-commands.json" $ do
    cases <- runIO (goldenCases (golden "project-commands.json"))
    forM_ cases $ \c -> it (T.unpack (textField "name" c)) $ withTmp $ \tmp -> do
      setupLayout tmp
      cwd <- canonicalizePath (tmp </> T.unpack (textField "cwd" c))
      ref <- newIORef Nothing
      let session = ProjectSession cwd (fill tmp (field "config" c)) (envPairs (fill tmp (field "env" c))) ref
      forM_ (zip [0 :: Int ..] (field "steps" c :: [Value])) $ \(i, step) -> do
        when (textField "name" c == "override-broken" && i == 1) $
          writeFile (tmp </> "A" </> ".fisher" </> "project.yaml") "schema_version: 7\nname: A\n"
        out <- handleProjectCommand session (retmp tmp (textField "command" step))
        let printed = T.replace (T.pack tmp) "<TMP>" (T.unlines out)
        printed `shouldBe` textField "stdout" step
        override <- readIORef ref
        (T.replace (T.pack tmp) "<TMP>" . T.pack <$> override) `shouldBe` (field "override" step :: Maybe Text)

  describe "doctor-managed.json" $ do
    cases <- runIO (goldenCases (golden "doctor-managed.json"))
    forM_ cases $ \c -> it (T.unpack (textField "name" c)) $ withTmp $ \tmp -> do
      let bin = tmp </> "bin"
          home = tmp </> "home"
          fisherHome = tmp </> "fisher-home"
      mapM_ (createDirectoryIfMissing True) [bin, home, fisherHome]
      forM_ (field "bins" c :: [Text]) $ \b -> do
        let p = bin </> T.unpack b
        writeFile p "#!/usr/bin/env bash\nexit 0\n"
        setPermissions p (setOwnerExecutable True (setOwnerReadable True emptyPermissions))
      case field "home_files" c :: Value of
        Object files -> forM_ (KM.toList files) $ \(rel, payload) -> do
          let p = home </> K.toString rel
          createDirectoryIfMissing True (takeDirectory p)
          case payload of
            String s -> TIO.writeFile p s
            v -> BL8.writeFile p (encode v)
        _ -> pure ()
      let persisted = textField "persisted_env" c
      when (not (T.null persisted)) $ TIO.writeFile (fisherHome </> ".env") persisted
      lean (tmp </> "project") []
      _ <- initializeProject (tmp </> "project") defaultInitOptions { initName = Just "Demo Project" }
      lean (tmp </> "broken") []
      createDirectoryIfMissing True (tmp </> "broken" </> ".fisher")
      writeFile (tmp </> "broken" </> ".fisher" </> "project.yaml") "name: x\n"
      createDirectoryIfMissing True (tmp </> "outside")
      let extra = [(k, T.unpack (T.replace "<HOME>" (T.pack home) (T.pack v))) | (k, v) <- envPairs (field "env" c)]
          -- upstream's doctor checks for uv, so its recordings are matched
          -- with upstream's MCP server chosen
          env = [("PATH", bin), ("HOME", home), ("FISHER_LEAN_MCP", "uvx")] <> extra
          persistedLookup k = Map.lookup k <$> loadEnvAt fisherHome
          de = DoctorEnv plainStyle (field "config" c) env (Just (tmp </> T.unpack (textField "cwd" c)))
                 persistedLookup (pure Nothing) "fisher"
      linesRef <- newIORef []
      issuesRef <- newIORef []
      checkManagedWorkflows de (\l -> modifyIORef' linesRef (<> [l])) (\i -> modifyIORef' issuesRef (<> [i]))
      printed <- T.replace (T.pack tmp) "<TMP>" . T.unlines <$> readIORef linesRef
      issues <- map (T.replace (T.pack tmp) "<TMP>") <$> readIORef issuesRef
      printed `shouldBe` textField "stdout" c
      issues `shouldBe` (field "issues" c :: [Text])

