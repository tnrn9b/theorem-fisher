module Fisher.Cli.AddSmtSpec (spec) where

import Control.Exception (AsyncException (..), bracket, throwIO)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
  ( canonicalizePath, createDirectoryIfMissing, doesFileExist, emptyPermissions, setOwnerExecutable
  , setOwnerReadable, setOwnerWritable, setPermissions )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO (hClose, readFile')
import System.Process (createPipe)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Cli.AddSmt
import Fisher.Cli.LakeRun (askOn)
import Fisher.ProjectCommand (AddSmtOpts (..))

withTmp :: (FilePath -> IO a) -> IO a
withTmp k = withSystemTempDirectory "fisher-add-smt" (\d -> canonicalizePath d >>= k)

-- | A fake @lake@ in @bin@ that appends its arguments to @calls@ and fails
-- @lake build@ when @fail-build@ exists next to it.
fakeLake :: FilePath -> IO FilePath
fakeLake tmp = do
  let bin = tmp </> "bin"
      lake = bin </> "lake"
  createDirectoryIfMissing True bin
  writeFile lake $ unlines
    [ "#!/bin/sh"
    , "echo \"$*\" >> " <> (bin </> "calls")
    , "if [ \"$1\" = build ] && [ -e " <> (bin </> "fail-build") <> " ]; then"
    , "  echo 'Demo/A.lean:1:0: error: boom'"
    , "  exit 1"
    , "fi"
    , "exit 0" ]
  setPermissions lake (setOwnerExecutable True (setOwnerWritable True (setOwnerReadable True emptyPermissions)))
  pure bin

withPath :: FilePath -> IO a -> IO a
withPath dir act = bracket (lookupEnv "PATH") (maybe (unsetEnv "PATH") (setEnv "PATH")) $ \old ->
  setEnv "PATH" (dir <> maybe "" (":" <>) old) >> act

calls :: FilePath -> IO [String]
calls bin = do
  e <- doesFileExist (bin </> "calls")
  if e then lines <$> readFile' (bin </> "calls") else pure []

lakefileToml :: String
lakefileToml = unlines
  [ "name = \"Demo\""
  , ""
  , "[[require]]"
  , "name = \"mathlib\""
  , "scope = \"leanprover-community\""
  , "rev = \"v4.34.0\""
  , ""
  , "[[lean_lib]]"
  , "name = \"Demo\"" ]

-- | A Lean project with Mathlib on v4.34.0, and no manifest yet.
project :: FilePath -> IO FilePath
project tmp = do
  let root = tmp </> "Demo"
  createDirectoryIfMissing True root
  writeFile (root </> "lakefile.toml") lakefileToml
  writeFile (root </> "lean-toolchain") "leanprover/lean4:v4.34.0\n"
  pure root

data Fake = Fake {fSaid :: IORef [Text], fFetched :: IORef Int}

deps :: [(Text, Text)] -> IO (AddSmtDeps, Fake)
deps builds = do
  said <- newIORef []
  fetched <- newIORef 0
  let d = AddSmtDeps
        { adSay = \l -> modifyIORef' said (<> [l])
        , adAsk = \_ -> pure False
        , adFetch = modifyIORef' fetched (+ 1) >> pure (Right builds)
        , adLake = "lake"
        , adSecs = 60 }
  pure (d, Fake said fetched)

reservoir :: [(Text, Text)]
reservoir = [("c35", "leanprover/lean4:v4.35.0"), ("c34", "leanprover/lean4:v4.34.0"), ("c34old", "leanprover/lean4:v4.34.0")]

yes, noBuild :: AddSmtOpts
yes = AddSmtOpts {asRev = Nothing, asNoBuild = False, asYes = True}
noBuild = yes {asNoBuild = True}

smtBlock :: String -> String
smtBlock rev = "[[require]]\nname = \"smt\"\nscope = \"ufmg-smite\"\nrev = \"" <> rev <> "\"\n"

-- | 'lakefileToml' with the smt block added before Mathlib's, then a blank line.
smtBeforeMathlib :: String -> String
smtBeforeMathlib rev = let (pre, post) = splitAt 2 (lines lakefileToml) in unlines pre <> smtBlock rev <> "\n" <> unlines post

spec :: Spec
spec = do
  it "add smt appends and is idempotent" $ withTmp $ \tmp -> do
    bin <- fakeLake tmp
    root <- project tmp
    let lakefile = root </> "lakefile.toml"
    (d, f) <- deps reservoir
    withPath bin $ do
      -- --no-build: the require is written, lake is not run
      addSmtWith d noBuild root `shouldReturn` (0, [])
      readFile' lakefile `shouldReturn` (smtBeforeMathlib "c34")
      calls bin `shouldReturn` []
      said <- readIORef (fSaid f)
      said `shouldContain` ["smt revision: c34 (Reservoir's build for leanprover/lean4:v4.34.0)"]
      said `shouldContain` ["Not built; run lake update smt and lake build in " <> T.pack root]
      -- again: already present, nothing changes, nothing runs
      addSmtWith d yes root `shouldReturn` (0, ["(^_^) smt is already present in " <> T.pack lakefile])
      readFile' lakefile `shouldReturn` (smtBeforeMathlib "c34")
      calls bin `shouldReturn` []
      readIORef (fFetched f) `shouldReturn` 1
    -- a manifest that has smt counts as present too
    let other = tmp </> "Other"
    createDirectoryIfMissing True other
    writeFile (other </> "lakefile.lean") "import Lake\nopen Lake DSL\npackage other\n"
    writeFile (other </> "lake-manifest.json") "{\"version\": \"1.1.0\", \"packages\": [{\"name\": \"smt\", \"type\": \"git\"}]}"
    addSmtWith d yes other `shouldReturn` (0, ["(^_^) smt is already present in " <> T.pack (other </> "lake-manifest.json")])

  it "add smt runs lake update smt, lake build and the check, logging to .fisher/add-smt.log" $ withTmp $ \tmp -> do
    bin <- fakeLake tmp
    root <- project tmp
    (d, f) <- deps reservoir
    createDirectoryIfMissing True (root </> ".fisher")
    writeFile (root </> ".fisher" </> "_fisher_search_stale.lean") "left by a killed check\n"
    withPath bin (addSmtWith d yes root) `shouldReturn` (0, [])
    calls bin `shouldReturn` ["update smt", "build", "lean .fisher/_fisher_search_add-smt-check.lean"]
    doesFileExist (root </> ".fisher" </> "_fisher_search_stale.lean") `shouldReturn` False
    said <- readIORef (fSaid f)
    drop (length said - 1) said `shouldBe` ["Ready: smt"]
    said `shouldContain` ["  lake update smt", "  lake build", "  check: by smt"]
    logText <- readFile' (root </> ".fisher" </> "add-smt.log")
    logText `shouldSatisfy` isInfixOf "== add smt: lake update smt\n== add smt: exit 0\n"
    logText `shouldSatisfy` isInfixOf "== add smt: lake build\n"
    logText `shouldSatisfy` isInfixOf "== add smt: probe of .fisher/add-smt-check.lean\n"
    doesFileExist (root </> ".fisher" </> "_fisher_search_add-smt-check.lean") `shouldReturn` False

  it "add smt with --rev" $ withTmp $ \tmp -> do
    bin <- fakeLake tmp
    root <- project tmp
    (d, f) <- deps reservoir
    withPath bin (addSmtWith d noBuild {asRev = Just "abc1234"} root) `shouldReturn` (0, [])
    readFile' (root </> "lakefile.toml") `shouldReturn` (smtBeforeMathlib "abc1234")
    readIORef (fFetched f) `shouldReturn` 0
    readIORef (fSaid f) >>= (`shouldContain` ["smt revision: abc1234 (--rev)"])

  it "add smt refuses a --rev outside main, no_mathlib and a hex commit" $ withTmp $ \tmp -> do
    bin <- fakeLake tmp
    root <- project tmp
    (d, _) <- deps reservoir
    (code, out) <- withPath bin (addSmtWith d noBuild {asRev = Just "v1\" ++ x"} root)
    code `shouldBe` 2
    out `shouldBe` ["(>_<) `--rev` takes main, no_mathlib or a commit of 7 to 40 hex digits, not v1\" ++ x"]
    readFile' (root </> "lakefile.toml") `shouldReturn` lakefileToml

  it "add smt failed build prints the revert line" $ withTmp $ \tmp -> do
    bin <- fakeLake tmp
    writeFile (bin </> "fail-build") ""
    root <- project tmp
    (d, f) <- deps reservoir
    (code, out) <- withPath bin (addSmtWith d yes root)
    code `shouldBe` 1
    out `shouldBe`
      [ "(>_<) lake build failed (exit 1); see .fisher/add-smt.log"
      , "  Demo/A.lean:1:0: error: boom"
      , "  Revert: delete the require line for smt in " <> T.pack (root </> "lakefile.toml") ]
    calls bin `shouldReturn` ["update smt", "build"]
    readIORef (fSaid f) >>= (`shouldNotContain` ["Ready: smt"])

  it "falls back to main with a warning, and to no_mathlib without Mathlib" $ withTmp $ \tmp -> do
    bin <- fakeLake tmp
    root <- project tmp
    (d, f) <- deps []
    withPath bin (addSmtWith d noBuild {asYes = True} root) `shouldReturn` (0, [])
    readIORef (fSaid f) >>= (`shouldContain` ["Warning: Reservoir has no build of smt for leanprover/lean4:v4.34.0; using main"])
    readFile' (root </> "lakefile.toml") >>= (`shouldSatisfy` isInfixOf (smtBlock "main"))
    let offline = d {adFetch = pure (Left "curl exited with 6")}
        root2 = tmp </> "Two"
    createDirectoryIfMissing True root2
    writeFile (root2 </> "lakefile.toml") lakefileToml
    addSmtWith offline noBuild root2 `shouldReturn` (0, [])
    readIORef (fSaid f) >>= (`shouldContain` ["Warning: could not read Reservoir's builds of smt (curl exited with 6); using main"])
    let root3 = tmp </> "Plain"
    createDirectoryIfMissing True root3
    writeFile (root3 </> "lakefile.lean") "import Lake\nopen Lake DSL\n\n-- no mathlib here\npackage plain\n"
    fetchedBefore <- readIORef (fFetched f)
    addSmtWith d noBuild root3 `shouldReturn` (0, [])
    readIORef (fFetched f) `shouldReturn` fetchedBefore
    readIORef (fSaid f) >>= (`shouldContain` ["Warning: this project has no Mathlib; using smt's no_mathlib branch"])
    readFile' (root3 </> "lakefile.lean")
      `shouldReturn` "import Lake\nopen Lake DSL\n\n-- no mathlib here\npackage plain\n\nrequire smt from git \"https://github.com/ufmg-smite/lean-smt.git\" @ \"no_mathlib\"\n"
    -- a lean lakefile with Mathlib: the require goes before Mathlib's, with its comment
    let root4 = tmp </> "LeanMathlib"
    createDirectoryIfMissing True root4
    writeFile (root4 </> "lakefile.lean") "import Lake\nopen Lake DSL\n\npackage m\n\n-- the library\nrequire mathlib from git\n  \"https://github.com/leanprover-community/mathlib4\" @ \"v4.34.0\"\n"
    addSmtWith d noBuild {asRev = Just "abc1234"} root4 `shouldReturn` (0, [])
    readFile' (root4 </> "lakefile.lean")
      `shouldReturn` "import Lake\nopen Lake DSL\n\npackage m\n\nrequire smt from git \"https://github.com/ufmg-smite/lean-smt.git\" @ \"abc1234\"\n\n-- the library\nrequire mathlib from git\n  \"https://github.com/leanprover-community/mathlib4\" @ \"v4.34.0\"\n"

  it "an interrupt while the revision is read changes nothing and exits 130" $ withTmp $ \tmp -> do
    bin <- fakeLake tmp
    root <- project tmp
    (d, _) <- deps reservoir
    withPath bin (addSmtWith d {adFetch = throwIO UserInterrupt} yes root)
      `shouldReturn` (130, ["(>_<) Interrupted; nothing was changed."])
    readFile' (root </> "lakefile.toml") `shouldReturn` lakefileToml
    calls bin `shouldReturn` []

  it "an interrupt at the question changes nothing and exits 130" $ withTmp $ \tmp -> do
    bin <- fakeLake tmp
    root <- project tmp
    (d, _) <- deps reservoir
    withPath bin (addSmtWith d {adAsk = \_ -> throwIO UserInterrupt} yes {asYes = False} root)
      `shouldReturn` (130, ["(>_<) Interrupted; nothing was changed."])
    readFile' (root </> "lakefile.toml") `shouldReturn` lakefileToml
    calls bin `shouldReturn` []

  it "askOn reads a closed or unreadable input as no" $ do
    (r, w) <- createPipe
    hClose w
    askOn r "Q? " `shouldReturn` False
    hClose r
    askOn r "Q? " `shouldReturn` False

  it "counts only a require of mathlib, not a mention" $ do
    requiresMathlib (T.pack lakefileToml) `shouldBe` True
    requiresMathlib "require mathlib from git \"https://github.com/leanprover-community/mathlib4.git\" @ \"v4.34.0\"\n" `shouldBe` True
    requiresMathlib "require \"leanprover-community\" / \"mathlib\" @ git \"v4.34.0\"\n" `shouldBe` True
    requiresMathlib "-- mathlib later\npackage p\nrequire batteries from git \"https://x/mathlib-free.git\"\n" `shouldBe` False

  it "asks before changing the lakefile, and a no changes nothing" $ withTmp $ \tmp -> do
    bin <- fakeLake tmp
    root <- project tmp
    (d, _) <- deps reservoir
    asked <- newIORef []
    let d' = d {adAsk = \q -> modifyIORef' asked (<> [q]) >> pure False}
    withPath bin (addSmtWith d' yes {asYes = False} root)
      `shouldReturn` (1, ["(>_<) Nothing was changed; pass --yes to add smt without asking."])
    readIORef asked `shouldReturn` ["Add smt at c34 to " <> T.pack (root </> "lakefile.toml") <> ", then run lake update smt and lake build? [y/N] "]
    readFile' (root </> "lakefile.toml") `shouldReturn` lakefileToml
    calls bin `shouldReturn` []

  it "stops before changing anything when lake is not on PATH" $ withTmp $ \tmp -> do
    root <- project tmp
    (d, _) <- deps reservoir
    addSmtWith d {adLake = tmp </> "no-lake"} yes root `shouldReturn` (1, ["(>_<) lake is not on PATH: install elan"])
    readFile' (root </> "lakefile.toml") `shouldReturn` lakefileToml

  it "says when there is no lakefile" $ withTmp $ \tmp -> do
    (d, _) <- deps reservoir
    addSmtWith d yes tmp `shouldReturn` (1, ["(>_<) No lakefile.lean or lakefile.toml in " <> T.pack tmp])
