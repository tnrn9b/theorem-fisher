-- | fisher-tools takes only Fisher's own names: the @fisher-*@ toolsets
-- (a toolset under the product's previous name is unknown), the
-- @_FISHER_FORCE_@ prefix, and the Fisher home's @.env@ on the write deny
-- list.
module Fisher.Tools.NamesSpec (spec) where

import Control.Exception (finally)
import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (createLink)
import qualified Data.Text.IO as TIO
import Test.Hspec

import Fisher.Tools.File
import Fisher.Tools.FileOps
import Fisher.Tools.Registry
import Fisher.Tools.Sandbox.Common (makeRunEnv)

-- | The product's previous name, built so that this file never spells it.
oldName :: Text
oldName = T.pack ['g', 'a', 'u', 's', 's']

spec :: Spec
spec = do
  let fresh = do
        reg <- newRegistry
        ft <- newFileTools (const (pure (Left "no sandbox in this test"))) (pure Nothing) (pure True)
        registerFileTools reg ft
        pure reg
      old suffix = oldName <> "-" <> suffix
  describe "toolset names under the previous name" $ do
    it "resolve to nothing and do not validate" $ do
      reg <- fresh
      new <- resolveToolset reg "fisher-cli"
      null new `shouldBe` False
      resolveToolset reg (old "cli") `shouldReturn` []
      validateToolset reg (old "acp") `shouldReturn` False
      toolsetInfo reg (old "cli") `shouldReturn` Nothing
    it "are unknown when enabled or disabled" $ do
      reg <- fresh
      (defs, printed) <- getToolDefinitions reg (Just [old "cli"]) Nothing
      defs `shouldBe` []
      any (("Unknown toolset: " <> old "cli") `T.isInfixOf`) printed `shouldBe` True
      (_, printedDis) <- getToolDefinitions reg Nothing (Just [old "cli"])
      any (("Unknown toolset: " <> old "cli") `T.isInfixOf`) printedDis `shouldBe` True
    it "a custom toolset may still take any name" $ do
      reg <- fresh
      createCustomToolset reg (old "mine") (Toolset "mine" ["read_file"] [])
      resolveToolset reg (old "mine") `shouldReturn` ["read_file"]
  describe "the force prefix" $
    it "re-admits a blocked name through _FISHER_FORCE_ only" $ do
      let oldForce = "_" <> T.toUpper oldName <> "_FORCE_GH_TOKEN"
          env = makeRunEnv [("PATH", "/usr/bin"), ("_FISHER_FORCE_OPENAI_API_KEY", "a"), (oldForce, "b"), ("OPENAI_API_KEY", "x")] []
      lookup "OPENAI_API_KEY" env `shouldBe` Just "a"
      lookup "GH_TOKEN" env `shouldBe` Nothing
  describe "the write deny list" $ do
    it "covers the Fisher home's .env" $
      withSystemTempDirectory "names-scope" $ \home -> do
        createDirectoryIfMissing True (home <> "/.fisher")
        scope <- homeWriteScope (T.pack home)
        wsDenied scope (T.pack (home <> "/.fisher/.env")) `shouldReturn` True
        wsDenied scope (T.pack (home <> "/.fisher/notes.md")) `shouldReturn` False
    it "covers the lessons files, the config, the templates and the run log, under the home and FISHER_HOME" $
      withSystemTempDirectory "names-scope" $ \home -> withSystemTempDirectory "names-fh" $ \fh -> do
        createDirectoryIfMissing True (home <> "/.fisher/fisher")
        createDirectoryIfMissing True (fh <> "/fisher")
        createDirectoryIfMissing True (home <> "/proj/.fisher")
        old <- lookupEnv "FISHER_HOME"
        setEnv "FISHER_HOME" fh
        scope <- homeWriteScope (T.pack home) `finally` maybe (unsetEnv "FISHER_HOME") (setEnv "FISHER_HOME") old
        forM_ [home <> "/.fisher", fh] $ \h ->
          forM_ ["/config.yaml", "/fisher/lessons.md", "/fisher/lessons-templates.yaml", "/fisher/runs.db", "/fisher/runs.db-wal", "/fisher/runs.db-shm"] $ \f ->
            wsDenied scope (T.pack (h <> f)) `shouldReturn` True
        -- any project's lessons file, also through ~ and a relative-looking path
        wsDenied scope (T.pack (home <> "/proj/.fisher/lessons.md")) `shouldReturn` True
        wsDenied scope "~/proj/.fisher/lessons.md" `shouldReturn` True
        wsDenied scope "/elsewhere/repo/.fisher/lessons.md" `shouldReturn` True
        -- other files there stay writable
        wsDenied scope (T.pack (home <> "/proj/.fisher/notes.md")) `shouldReturn` False
        wsDenied scope (T.pack (home <> "/proj/lessons.md")) `shouldReturn` False
    it "on a case-insensitive file system, denies a case variant of a protected path" $
      withSystemTempDirectory "names-scope" $ \home -> do
        createDirectoryIfMissing True (home <> "/.fisher/fisher")
        insensitive <- homeWriteScopeWith True (T.pack home)
        forM_ ["/.fisher/CONFIG.yaml", "/.FISHER/fisher/Lessons.MD", "/.fisher/.ENV", "/proj/.Fisher/LESSONS.md", "/.SSH/id_rsa"] $ \f ->
          ((f,) <$> wsDenied insensitive (T.pack (home <> f))) `shouldReturn` (f, True)
        wsDenied insensitive (T.pack (home <> "/.fisher/Notes.md")) `shouldReturn` False
        -- a case-sensitive one (Linux) keeps the exact comparison
        sensitive <- homeWriteScopeWith False (T.pack home)
        wsDenied sensitive (T.pack (home <> "/.fisher/CONFIG.yaml")) `shouldReturn` False
    it "denies another name of a protected file that exists (a hard link)" $
      withSystemTempDirectory "names-scope" $ \home -> do
        createDirectoryIfMissing True (home <> "/.fisher/fisher")
        TIO.writeFile (home <> "/.fisher/config.yaml") "fisher: {}\n"
        createLink (home <> "/.fisher/config.yaml") (home <> "/other.yaml")
        scope <- homeWriteScopeWith False (T.pack home)
        wsDenied scope (T.pack (home <> "/other.yaml")) `shouldReturn` True
        TIO.writeFile (home <> "/plain.yaml") "x\n"
        wsDenied scope (T.pack (home <> "/plain.yaml")) `shouldReturn` False
