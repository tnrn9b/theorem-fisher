module Fisher.Smt.LakeSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (fileMode, getFileStatus, intersectFileModes, setFileMode)
import Test.Hspec

import Fisher.Smt.Lake

withTmp :: (FilePath -> IO a) -> IO a
withTmp = withSystemTempDirectory "fisher-lake-spec"

fx :: FilePath -> FilePath
fx n = "test/fixtures/smt/lake" </> n

spec :: Spec
spec = do
  describe "manifest shapes" $ do
    it "a v7 manifest with smt gives True" $ hasSmt (fx "v7-smt") `shouldReturn` True
    it "a v7 manifest without smt gives False" $ hasSmt (fx "v7-plain") `shouldReturn` False
    it "a v6 manifest gives False" $ hasSmt (fx "v6") `shouldReturn` False
    it "an unreadable manifest gives False" $
      withTmp $ \d -> do
        createDirectory (d </> "lake-manifest.json")
        hasSmt d `shouldReturn` False
    it "a missing manifest gives False" $ hasSmt (fx "nowhere") `shouldReturn` False
    it "a manifest that is not JSON gives False" $
      withTmp $ \d -> do
        writeFile (d </> "lake-manifest.json") "not json"
        hasSmt d `shouldReturn` False
  describe "hasMathlib" $ do
    it "finds mathlib" $ do
      hasMathlib (fx "v7-plain") `shouldReturn` True
      hasMathlib (fx "v7-smt") `shouldReturn` True
    it "is False for v6 and missing" $ do
      hasMathlib (fx "v6") `shouldReturn` False
      hasMathlib (fx "nowhere") `shouldReturn` False
  describe "leanToolchain" $ do
    it "reads and trims the file" $
      leanToolchain (fx "v7-plain") `shouldReturn` Just "leanprover/lean4:v4.24.0"
    it "is Nothing when missing" $ leanToolchain (fx "nowhere") `shouldReturn` Nothing
  describe "requireSmtText for lean and toml" $ do
    it "lean form" $
      requireSmtText (LakeLean "x") "abc"
        `shouldBe` "require smt from git \"https://github.com/ufmg-smite/lean-smt.git\" @ \"abc\""
    it "toml form" $
      requireSmtText (LakeToml "x") "abc"
        `shouldBe` "[[require]]\nname = \"smt\"\nscope = \"ufmg-smite\"\nrev = \"abc\""
  describe "findLakeFile" $
    it "prefers lean, then toml, then nothing" $
      withTmp $ \d -> do
        findLakeFile d `shouldReturn` Nothing
        writeFile (d </> "lakefile.toml") ""
        findLakeFile d `shouldReturn` Just (LakeToml (d </> "lakefile.toml"))
        writeFile (d </> "lakefile.lean") ""
        findLakeFile d `shouldReturn` Just (LakeLean (d </> "lakefile.lean"))
  describe "hasSmtRequire" $
    it "sees lean-smt and name = smt, not mathlib" $ do
      hasSmtRequire "require smt from git \"https://github.com/ufmg-smite/lean-smt.git\" @ \"x\"" `shouldBe` True
      hasSmtRequire "[[require]]\nname  =  \"smt\"\n" `shouldBe` True
      hasSmtRequire "[[require]]\nname = \"mathlib\"\n" `shouldBe` False
  describe "addSmtRequire appends once" $
    it "second call is Left" $
      withTmp $ \d -> do
        let p = d </> "lakefile.lean"
        writeFile p "import Lake\nopen Lake DSL\n\npackage foo\n"
        addSmtRequire (LakeLean p) "abc" `shouldReturn` Right ()
        t <- TIO.readFile p
        t `shouldBe` ("import Lake\nopen Lake DSL\n\npackage foo\n\n" <> requireSmtText (LakeLean p) "abc" <> "\n")
        r <- addSmtRequire (LakeLean p) "abc"
        r `shouldSatisfy` either (const True) (const False)
        TIO.readFile p `shouldReturn` t
  describe "addSmtRequire writes safely" $ do
    it "refuses a lakefile that is not UTF-8 and leaves it alone" $
      withTmp $ \d -> do
        let p = d </> "lakefile.lean"
            bad = BS.pack [0x70, 0x61, 0xff, 0xfe, 0x0a]
        BS.writeFile p bad
        r <- addSmtRequire (LakeLean p) "abc"
        r `shouldSatisfy` either (T.isInfixOf "UTF-8") (const False)
        BS.readFile p `shouldReturn` bad
    it "leaves no temp file behind and keeps the mode" $
      withTmp $ \d -> do
        let p = d </> "lakefile.lean"
        writeFile p "package foo\n"
        setFileMode p 0o640
        addSmtRequire (LakeLean p) "abc" `shouldReturn` Right ()
        listDirectory d `shouldReturn` ["lakefile.lean"]
        st <- getFileStatus p
        (fileMode st `intersectFileModes` 0o777) `shouldBe` 0o640
  describe "the require's place" $ do
    let smtToml = "[[require]]\nname = \"smt\"\nscope = \"ufmg-smite\"\nrev = \"abc\"\n"
    it "toml: inserted before the mathlib block, comments above it kept with it" $
      withTmp $ \d -> do
        let p = d </> "lakefile.toml"
        writeFile p "name = \"foo\"\n\n[[require]]\nname = \"batteries\"\n\n# the library\n[[require]]\nname = \"mathlib\"\nscope = \"leanprover-community\"\nrev = \"v4.24.0\"\n\n[[lean_lib]]\nname = \"foo\"\n"
        addSmtRequire (LakeToml p) "abc" `shouldReturn` Right ()
        TIO.readFile p `shouldReturn`
          ("name = \"foo\"\n\n[[require]]\nname = \"batteries\"\n\n" <> smtToml <> "\n# the library\n[[require]]\nname = \"mathlib\"\nscope = \"leanprover-community\"\nrev = \"v4.24.0\"\n\n[[lean_lib]]\nname = \"foo\"\n")
    it "toml: appended when there is no mathlib" $
      withTmp $ \d -> do
        let p = d </> "lakefile.toml"
        writeFile p "name = \"foo\"\n\n[[require]]\nname = \"batteries\"\nrev = \"main\"\n"
        addSmtRequire (LakeToml p) "abc" `shouldReturn` Right ()
        TIO.readFile p `shouldReturn` ("name = \"foo\"\n\n[[require]]\nname = \"batteries\"\nrev = \"main\"\n\n" <> smtToml)
    it "lean: inserted before the require mathlib line" $
      withSmtRequire (LakeLean "x") "abc" "import Lake\nopen Lake DSL\n\nrequire \"leanprover-community\" / \"mathlib\" @ git \"v4.24.0\"\n\npackage foo\n"
        `shouldBe` ("import Lake\nopen Lake DSL\n\n" <> requireSmtText (LakeLean "x") "abc" <> "\n\nrequire \"leanprover-community\" / \"mathlib\" @ git \"v4.24.0\"\n\npackage foo\n")
    it "lean: a comment mentioning mathlib is not its require" $
      withSmtRequire (LakeLean "x") "abc" "package foo\n-- require mathlib later\n"
        `shouldBe` ("package foo\n-- require mathlib later\n\n" <> requireSmtText (LakeLean "x") "abc" <> "\n")
  describe "insertImportSmt" $ do
    it "goes after the last import" $
      insertImportSmt "import Mathlib\nimport Foo\n\ntheorem x : True := trivial\n"
        `shouldBe` "import Mathlib\nimport Foo\nimport Smt -- fisher smt\n\ntheorem x : True := trivial\n"
    it "goes at the top of a file with no imports" $
      insertImportSmt "theorem x : True := trivial\n"
        `shouldBe` "import Smt -- fisher smt\ntheorem x : True := trivial\n"
    it "keeps to the header: imports, then a blank line and a comment" $
      insertImportSmt "import Mathlib\n\n-- note\n/- block\nimport Fake\n-/\ntheorem x := 1\n"
        `shouldBe` "import Mathlib\nimport Smt -- fisher smt\n\n-- note\n/- block\nimport Fake\n-/\ntheorem x := 1\n"
    it "skips a doc comment before the imports" $
      insertImportSmt "/-! Module doc\nimport Fake\n-/\nimport Mathlib\n\ndef a := 1\n"
        `shouldBe` "/-! Module doc\nimport Fake\n-/\nimport Mathlib\nimport Smt -- fisher smt\n\ndef a := 1\n"
    it "ignores an import line inside a later docstring" $
      insertImportSmt "import Mathlib\n\n/-- usage:\nimport Foo\n-/\ndef a := 1\n"
        `shouldBe` "import Mathlib\nimport Smt -- fisher smt\n\n/-- usage:\nimport Foo\n-/\ndef a := 1\n"
    it "does not see an import Smt below the header" $
      insertImportSmt "import Mathlib\n\ndef a := 1\n-- import Smt\n"
        `shouldBe` "import Mathlib\nimport Smt -- fisher smt\n\ndef a := 1\n-- import Smt\n"
    it "is idempotent" $ do
      let once = insertImportSmt "import Mathlib\n\nexample := 1\n"
      insertImportSmt once `shouldBe` once
      insertImportSmt "import Smt\nexample := 1\n" `shouldBe` "import Smt\nexample := 1\n"
  describe "insertImportSmt and the header kind" $ do
    it "leaves a module file unchanged" $ do
      let src = "module\n\nimport Mathlib\n\ndef a := 1\n"
      insertImportSmt src `shouldBe` src
      headerKind src `shouldBe` Module
    it "sees module after a doc comment" $
      headerKind "/-! doc -/\nmodule\npublic import Mathlib\n" `shouldBe` Module
    it "puts the marker after the imports of a prelude file" $ do
      let src = "prelude\nimport Init.Core\nimport Init.Data\n\ndef a := 1\n"
      insertImportSmt src `shouldBe` "prelude\nimport Init.Core\nimport Init.Data\nimport Smt -- fisher smt\n\ndef a := 1\n"
      headerKind src `shouldBe` Prelude
    it "puts the marker after prelude when there are no imports" $
      insertImportSmt "-- c\nprelude\n\ndef a := 1\n"
        `shouldBe` "-- c\nprelude\nimport Smt -- fisher smt\n\ndef a := 1\n"
    it "counts public, meta and all imports" $ do
      insertImportSmt "public import Mathlib\nmeta import Foo\nimport all Bar\n\ndef a := 1\n"
        `shouldBe` "public import Mathlib\nmeta import Foo\nimport all Bar\nimport Smt -- fisher smt\n\ndef a := 1\n"
      insertImportSmt "public import Smt\ndef a := 1\n" `shouldBe` "public import Smt\ndef a := 1\n"
    it "calls an ordinary file plain" $
      headerKind "import Mathlib\ndef a := 1\n" `shouldBe` Plain
  describe "stripImportSmt" $ do
    it "removes only the marker line" $ do
      let src = "import Mathlib\nimport Smt\nimport Smt -- fisher smt\n\nexample := 1\n"
      stripImportSmt src `shouldBe` "import Mathlib\nimport Smt\n\nexample := 1\n"
    it "undoes insertImportSmt" $ do
      let src = "import Mathlib\n\nexample := 1\n"
      stripImportSmt (insertImportSmt src) `shouldBe` src
