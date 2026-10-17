module Fisher.Search.HeadlessSpec (spec) where

import Test.Hspec

import Fisher.Search.Headless

spec :: Spec
spec = describe "headlessArgv" $ do
  let argv = ["fisher", "chat", "-q", "/prove Main.lean"]
  it "runs the native backend under its new name and its old one alike" $ do
    let r name = headlessArgv name ["fisher", "chat", "-q", "P"] "x" 5
    r "fisher" `shouldBe` r "gauss"
    r "fisher" `shouldBe` Right ["fisher", "chat", "-p", "P x", "--output-format", "stream-json", "--max-turns", "5"]
  it "still refuses a backend it does not know" $ do
    headlessArgv "nope" argv "x" 5 `shouldBe` Left "no headless form for backend nope"
