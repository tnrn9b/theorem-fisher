module Fisher.Smt.ReservoirSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import System.Directory (setPermissions, emptyPermissions, setOwnerExecutable, setOwnerReadable, setOwnerWritable)
import System.FilePath ((</>))
import Test.Hspec

import Fisher.Smt.Reservoir
import Fisher.TestUtil (withTmp)

spec :: Spec
spec = do
  let tc = "leanprover/lean4:v4.34.0"
      bs = [("c3", "leanprover/lean4:v4.35.0"), ("c2", tc), ("c1", tc), ("c0", "leanprover/lean4:v4.33.0")]
  it "pickRevision newest match" $ do
    pickRevision (Just tc) bs True `shouldBe` Right "c2"
    pickRevision (Just "v4.33.0") bs True `shouldBe` Right "c0"
  it "pickRevision falls back to main" $ do
    pickRevision (Just "leanprover/lean4:v4.20.0") bs True `shouldBe` Left "main"
    pickRevision Nothing bs True `shouldBe` Left "main"
    pickRevision (Just tc) [] True `shouldBe` Left "main"
  it "pickRevision falls back to no_mathlib without Mathlib" $
    pickRevision (Just tc) bs False `shouldBe` Left "no_mathlib"
  it "parses the builds page" $ do
    page <- BS.readFile "test/fixtures/smt/reservoir/builds.json"
    parseBuilds page
      `shouldBe` Right
        [ ("9abdd01c13dc7ac2bac4d68d17ee661c63ed0894", "leanprover/lean4:v4.34.0")
        , ("2956f2966ff443b27a86fe2a4fb82dc56efafc51", "leanprover/lean4:v4.34.0")
        , ("5bdc51674065a074ece67b04e10024e9f426ec1f", "leanprover/lean4:v4.33.0") ]
    (parseBuilds page >>= \b -> pickRevision (Just "leanprover/lean4:v4.34.0") b True)
      `shouldBe` Right "9abdd01c13dc7ac2bac4d68d17ee661c63ed0894"
    parseBuilds "<html></html>" `shouldBe` Left "not a Reservoir builds file"
  it "orders the builds by run, newest first" $
    parseBuilds
      "{\"data\":[{\"built\":true,\"toolchain\":\"t\",\"revision\":\"0123abc\",\"runAt\":\"2026-01-01T00:00:00Z\"},\
      \{\"built\":true,\"toolchain\":\"t\",\"revision\":\"4567def\",\"runAt\":\"2026-02-01T00:00:00Z\"}]}"
      `shouldBe` Right [("4567def", "t"), ("0123abc", "t")]
  it "keeps only revisions that are main, no_mathlib or a hex commit" $ do
    map validRevision ["main", "no_mathlib", "abcdef1", T.replicate 40 "a", "9abdd01c13dc7ac2bac4d68d17ee661c63ed0894"]
      `shouldBe` replicate 5 True
    map validRevision ["", "abc123", T.replicate 41 "a", "ABCDEF1", "v4.24.0", "main\"", "abc1234\" ++ x", "de ad be ef"]
      `shouldBe` replicate 8 False
    parseBuilds
      "{\"data\":[{\"built\":true,\"toolchain\":\"t\",\"revision\":\"x\\\" @ evil\",\"runAt\":\"2\"},\
      \{\"built\":true,\"toolchain\":\"t\",\"revision\":\"abcdef1\",\"runAt\":\"1\"}]}"
      `shouldBe` Right [("abcdef1", "t")]
  it "fetches through curl, and a failing curl is a Left" $ withTmp $ \d -> do
    let exe p body = do
          writeFile p ("#!/bin/sh\n" <> body)
          setPermissions p (setOwnerExecutable True (setOwnerWritable True (setOwnerReadable True emptyPermissions)))
    writeFile (d </> "body") "{\"data\":[{\"built\":true,\"toolchain\":\"t\",\"revision\":\"abcdef1\",\"runAt\":\"x\"}]}"
    exe (d </> "curl") ("printf '%s\\n' \"$@\" > " <> (d </> "argv") <> "\ncat " <> (d </> "body") <> "\n")
    exe (d </> "failing") "exit 6\n"
    exe (d </> "noisy") "echo 'curl: (6) Could not resolve host: example.invalid' >&2\nexit 6\n"
    fetchBuildsWith (d </> "curl") "https://example.invalid/b.json" `shouldReturn` Right [("abcdef1", "t")]
    lines <$> readFile (d </> "argv") `shouldReturn` ["-fsSL", "--show-error", "--max-time", "10", "--max-filesize", "1048576", "https://example.invalid/b.json"]
    fetchBuildsWith (d </> "failing") "https://example.invalid/" `shouldReturn` Left "curl exited with 6"
    fetchBuildsWith (d </> "noisy") "https://example.invalid/" `shouldReturn` Left "curl exited with 6: curl: (6) Could not resolve host: example.invalid"
    fetchBuildsWith (d </> "missing") "https://example.invalid/" >>= (`shouldSatisfy` either (const True) (const False))
