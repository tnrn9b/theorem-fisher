module Fisher.Search.HeadlessSpec (spec) where

import Test.Hspec

import Fisher.Search.Headless
import Fisher.Search.OldName (oldName)

spec :: Spec
spec = describe "headlessArgv" $ do
  let argv = ["fisher", "chat", "-q", "/prove Main.lean"]
  it "runs the native backend, fisher" $
    headlessArgv "fisher" ["fisher", "chat", "-q", "P"] "x" 5
      `shouldBe` Right ["fisher", "chat", "-p", "P x", "--output-format", "stream-json", "--max-turns", "5"]
  it "has no headless form for the native backend's previous name" $ do
    let old = oldName
    headlessArgv old argv "x" 5 `shouldBe` Left ("no headless form for backend " <> old)
  it "still refuses a backend it does not know" $ do
    headlessArgv "nope" argv "x" 5 `shouldBe` Left "no headless form for backend nope"
