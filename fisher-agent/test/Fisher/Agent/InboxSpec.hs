-- | The inbox (Fisher UI, task 4): each whole line taken once, from an
-- offset kept beside the file.
module Fisher.Agent.InboxSpec (spec) where

import qualified Data.ByteString as BS
import Data.IORef
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.Agent.Inbox

spec :: Spec
spec = describe "Fisher.Agent.Inbox" $ do
  it "takes what is new, each line once" $ withSystemTempDirectory "fisher-inbox" $ \tmp -> do
    let path = inboxPath (tmp </> "gauss") "s1"
    path `shouldBe` tmp </> "gauss" </> "fisher" </> "inbox" </> "s1.jsonl"
    takeInbox path `shouldReturn` []
    appendInbox path "Also check B."
    appendInbox path "and \"quote\" ü"
    takeInbox path `shouldReturn` ["Also check B.", "and \"quote\" ü"]
    takeInbox path `shouldReturn` []
    appendInbox path "third"
    takeInbox path `shouldReturn` ["third"]
    takeInbox path `shouldReturn` []
  it "waits for a line still being written" $ withSystemTempDirectory "fisher-inbox" $ \tmp -> do
    let path = tmp </> "s.jsonl"
    BS.writeFile path "{\"text\": \"one\"}\n{\"text\": \"tw"
    takeInbox path `shouldReturn` ["one"]
    takeInbox path `shouldReturn` []
    BS.appendFile path "o\"}\n"
    takeInbox path `shouldReturn` ["two"]
  it "reads a file shorter than its offset again from the start, and says so" $ withSystemTempDirectory "fisher-inbox" $ \tmp -> do
    let path = tmp </> "s.jsonl"
    appendInbox path "a long first message"
    takeInbox path `shouldReturn` ["a long first message"]
    BS.writeFile path "{\"text\": \"new\"}\n"
    notes <- newIORef []
    takeInboxWith (\t -> modifyIORef notes (<> [t])) path `shouldReturn` ["new"]
    length <$> readIORef notes `shouldReturn` 1
    takeInboxWith (\t -> modifyIORef notes (<> [t])) path `shouldReturn` []
    length <$> readIORef notes `shouldReturn` 1
  it "takes a plain line as its text and skips a blank one" $ withSystemTempDirectory "fisher-inbox" $ \tmp -> do
    let path = tmp </> "s.jsonl"
    BS.writeFile path "plain words\n\n\"a string\"\n"
    takeInbox path `shouldReturn` ["plain words", "a string"]
