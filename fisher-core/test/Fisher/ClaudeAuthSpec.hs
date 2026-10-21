-- | The Claude Code CLI's own login, as Fisher reads it: the status JSON
-- parser, the status run against fake @claude@ scripts, the login argv and
-- the email check.
module Fisher.ClaudeAuthSpec (spec) where

import Data.List (isInfixOf)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, getPermissions, setOwnerExecutable, setOwnerReadable, setOwnerWritable, setPermissions)
import System.FilePath ((</>))
import Data.Time (diffUTCTime, getCurrentTime)
import Test.Hspec

import Fisher.ClaudeAuth
import Fisher.TestUtil (withTmp)

-- | An executable script, owner rwx (kept writable: the tests run as root).
script :: FilePath -> String -> IO FilePath
script dir body = do
  createDirectoryIfMissing True dir
  let p = dir </> "claude"
  writeFile p ("#!/bin/sh\n" <> body <> "\n")
  getPermissions p >>= setPermissions p . setOwnerExecutable True . setOwnerReadable True . setOwnerWritable True
  pure p

loggedOut :: ClaudeAuthStatus
loggedOut = ClaudeAuthStatus False Nothing Nothing Nothing Nothing

spec :: Spec
spec = do
  describe "parseClaudeAuthStatus" $ do
    it "reads a logged-in status, without the organisation" $
      parseClaudeAuthStatus
        "{\"loggedIn\": true, \"authMethod\": \"claude.ai\", \"apiProvider\": \"firstParty\", \"email\": \"x@y\", \"orgId\": \"o-1\", \"orgName\": \"Org\", \"subscriptionType\": \"max\"}"
        `shouldBe` Right (ClaudeAuthStatus True (Just "claude.ai") (Just "firstParty") (Just "x@y") (Just "max"))
    it "reads a logged-out status" $
      parseClaudeAuthStatus "{\"loggedIn\": false}" `shouldBe` Right loggedOut
    it "takes missing, empty and mistyped fields as absent" $ do
      parseClaudeAuthStatus "{}" `shouldBe` Right loggedOut
      parseClaudeAuthStatus "{\"loggedIn\": true, \"email\": \"\", \"subscriptionType\": 3, \"extra\": [1]}"
        `shouldBe` Right (ClaudeAuthStatus True Nothing Nothing Nothing Nothing)
      parseClaudeAuthStatus "{\"loggedIn\": \"yes\", \"authMethod\": \"console\"}"
        `shouldBe` Right (ClaudeAuthStatus False (Just "console") Nothing Nothing Nothing)
    it "refuses what is not a JSON object" $ do
      parseClaudeAuthStatus "" `shouldSatisfy` isLeft'
      parseClaudeAuthStatus "Not logged in" `shouldSatisfy` isLeft'
      parseClaudeAuthStatus "[true]" `shouldSatisfy` isLeft'
      parseClaudeAuthStatus "{\"loggedIn\": tru" `shouldSatisfy` isLeft'

  describe "claudeAccount" $ do
    it "names the email and the plan (the subscription, else the method)" $ do
      claudeAccount (ClaudeAuthStatus True (Just "claude.ai") Nothing (Just "x@y") (Just "max")) `shouldBe` "x@y (max)"
      claudeAccount (ClaudeAuthStatus True (Just "console") Nothing (Just "x@y") Nothing) `shouldBe` "x@y (console)"
      claudeAccount (ClaudeAuthStatus True Nothing Nothing (Just "x@y") Nothing) `shouldBe` "x@y"
      claudeAccount (ClaudeAuthStatus True Nothing Nothing Nothing (Just "pro")) `shouldBe` "an unnamed account (pro)"

  describe "claudeLoginArgs" $
    it "passes each option given, subscription by default" $ do
      claudeLoginArgs (ClaudeLogin False False Nothing) `shouldBe` ["auth", "login"]
      claudeLoginArgs (ClaudeLogin True False Nothing) `shouldBe` ["auth", "login", "--console"]
      claudeLoginArgs (ClaudeLogin False True (Just "a@b.c")) `shouldBe` ["auth", "login", "--sso", "--email", "a@b.c"]
      claudeLoginArgs (ClaudeLogin True True (Just "a@b.c")) `shouldBe` ["auth", "login", "--console", "--sso", "--email", "a@b.c"]

  describe "validClaudeEmail" $ do
    it "takes an address of one word" $ do
      validClaudeEmail "x@y.z" `shouldBe` True
      validClaudeEmail "first.last+tag@example.co.uk" `shouldBe` True
    it "refuses spaces, line breaks, control characters, a leading dash, empty and over 254" $ do
      mapM_ (\e -> validClaudeEmail e `shouldBe` False)
        ["", "a b@c", "a@b\n", "a@b\r", "a\t@b", "a\0@b", "-x@y", T.replicate 250 "a" <> "@b.cd"]
      validClaudeEmail (T.replicate 249 "a" <> "@b.cd") `shouldBe` True

  describe "claudeAuthStatus" $ do
    it "runs claude auth status --json with stdin from /dev/null" $ withTmp $ \tmp -> do
      exe <- script (tmp </> "bin")
        ("echo \"$@\" > " <> tmp <> "/argv\nif read x; then echo stdin-open > " <> tmp <> "/stdin; fi\n\
         \echo '{\"loggedIn\": true, \"email\": \"x@y\", \"subscriptionType\": \"max\", \"orgId\": \"o\"}'")
      r <- claudeAuthStatus exe
      r `shouldBe` Right (ClaudeAuthStatus True Nothing Nothing (Just "x@y") (Just "max"))
      readFile (tmp </> "argv") `shouldReturn` "auth status --json\n"
      readFile (tmp </> "stdin") `shouldThrow` anyIOException
    it "takes a non-zero exit as logged out, reading the JSON when there is some" $ withTmp $ \tmp -> do
      exe <- script (tmp </> "bin") "echo 'Not logged in' >&2\nexit 1"
      claudeAuthStatus exe `shouldReturn` Right loggedOut
      exe2 <- script (tmp </> "bin2") "echo '{\"loggedIn\": false, \"authMethod\": \"none\"}'\nexit 1"
      claudeAuthStatus exe2 `shouldReturn` Right (ClaudeAuthStatus False (Just "none") Nothing Nothing Nothing)
    it "is Left when it cannot be run at all (exit 126 or 127)" $ withTmp $ \tmp -> do
      exe <- script (tmp </> "bin") "exit 127"
      claudeAuthStatus exe `shouldReturn` Left "could not run claude (exit 127)"
      exe2 <- script (tmp </> "bin2") "exit 126"
      claudeAuthStatus exe2 `shouldReturn` Left "could not run claude (exit 126)"
    it "never takes a non-zero exit as logged in" $ withTmp $ \tmp -> do
      exe <- script (tmp </> "bin") "echo '{\"loggedIn\": true, \"email\": \"x@y\"}'\nexit 2"
      claudeAuthStatus exe `shouldReturn` Right (ClaudeAuthStatus False Nothing Nothing (Just "x@y") Nothing)
    it "is Left for output it cannot read on a zero exit" $ withTmp $ \tmp -> do
      exe <- script (tmp </> "bin") "exit 0"
      r <- claudeAuthStatus exe
      either (\e -> T.unpack e `shouldSatisfy` ("unreadable" `isInfixOf`)) (\s -> expectationFailure (show s)) r
    it "is Left \"not installed\" for a missing executable" $ withTmp $ \tmp ->
      claudeAuthStatus (tmp </> "nowhere" </> "claude") `shouldReturn` Left "not installed"
    it "is Left \"timed out\" past its limit" $ withTmp $ \tmp -> do
      exe <- script (tmp </> "bin") "sleep 5"
      claudeAuthStatusWithin 300000 exe `shouldReturn` Left "timed out"
    it "times out when the child has exited but a descendant keeps its output open, in its group or not" $ withTmp $ \tmp -> do
      exe <- script (tmp </> "bin") "sleep 5 &\necho '{\"loggedIn\": true}'"
      t0 <- getCurrentTime
      claudeAuthStatusWithin 300000 exe `shouldReturn` Left "timed out"
      exe2 <- script (tmp </> "bin2") "perl -e 'use POSIX; POSIX::setsid(); sleep 5' &\necho '{\"loggedIn\": true}'"
      claudeAuthStatusWithin 300000 exe2 `shouldReturn` Left "timed out"
      t1 <- getCurrentTime
      diffUTCTime t1 t0 `shouldSatisfy` (< 3)

  describe "claudeLogout" $ do
    it "runs claude auth logout with stdin from /dev/null" $ withTmp $ \tmp -> do
      exe <- script (tmp </> "bin") ("echo \"$@\" > " <> tmp <> "/argv\nif read x; then echo open > " <> tmp <> "/stdin; fi")
      claudeLogout exe `shouldReturn` Right ()
      readFile (tmp </> "argv") `shouldReturn` "auth logout\n"
      readFile (tmp </> "stdin") `shouldThrow` anyIOException
    it "is Left with the exit code when it fails, and for a missing executable" $ withTmp $ \tmp -> do
      exe <- script (tmp </> "bin") "exit 3"
      claudeLogout exe `shouldReturn` Left "claude auth logout exited with code 3"
      claudeLogout (tmp </> "none") `shouldReturn` Left "not installed"

  describe "findClaude" $
    it "looks on the PATH given" $ withTmp $ \tmp -> do
      exe <- script (tmp </> "bin") "exit 0"
      findClaude [("PATH", tmp </> "none:" <> tmp </> "bin")] `shouldReturn` Just exe
      findClaude [("PATH", tmp </> "none")] `shouldReturn` Nothing
  where
    isLeft' = either (const True) (const False)
