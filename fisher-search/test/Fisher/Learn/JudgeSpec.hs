{-# LANGUAGE DataKinds #-}

module Fisher.Learn.JudgeSpec (spec) where

import Data.Aeson (Value, object, (.=))
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Fisher.Learn.Judge
import Fisher.Learn.Mistakes (AttemptView (..))
import Fisher.Lessons.File (normaliseLesson)
import Fisher.Role (Said, partText, saidText)
import Fisher.Role.Agent (roleCtx, runRole)
import Fisher.Role.Internal (said')
import Fisher.Search.GraphJobs (Ask, AskError (..), Asked (..), Role (..))

good :: Text
good = "{\"mistake\": \"named a lemma that does not exist\", \"lesson\": \"Search with lean_loogle before naming a lemma.\", \"confidence\": 0.8, \"tell_yourself\": \"Look the lemma up first. Then write the proof.\"}"

view :: [(Int, Double, Text, Value)] -> AttemptView
view es = AttemptView {avEvents = es, avOutcomeText = "rejected: still sorry", avClass = "failed", avGoal = "foo", avShape = "arith"}

msg :: Int -> Text -> (Int, Double, Text, Value)
msg s t = (s, fromIntegral s, "message", object ["role" .= ("assistant" :: Text), "text" .= t])

-- | A verdict's texts and confidence, as the judge said them.
texts :: Verdict -> (Text, Maybe Text, Double, Text)
texts v = (saidText (vMistake v), saidText <$> vLesson v, vConfidence v, saidText (vTellYourself v))

userText :: AttemptView -> Text
userText av = T.concat [partText c | ("user", c) <- judgeMessages av]

-- | A judge's reply, as the role monad's ask gives it.
judge :: Text -> Said 'Judge
judge = said'

-- | A judge call through its role context (no checker, nothing recorded).
judged :: Ask -> AttemptView -> IO (Asked Verdict)
judged ask av = runRole (roleCtx @'Judge ask Nothing (\_ _ -> pure ()) (\_ _ _ -> pure ())) (judgeAttempt av)

spec :: Spec
spec = do
  describe "parseVerdict" $ do
    it "reads a good object" $
      fmap texts (parseVerdict (judge good))
        `shouldBe` Right ("named a lemma that does not exist", Just "Search with lean_loogle before naming a lemma.", 0.8, "Look the lemma up first. Then write the proof.")
    it "reads a fenced JSON block" $
      parseVerdict (judge ("Here is my verdict.\n```json\n" <> good <> "\n```\n")) `shouldBe` parseVerdict (judge good)
    it "refuses a confidence that is not a number" $ do
      parseVerdict (judge "{\"confidence\":\"high\"}") `shouldSatisfy` isLeft
      parseVerdict (judge "{\"mistake\": \"m\", \"lesson\": \"l\", \"confidence\": \"high\", \"tell_yourself\": \"t\"}") `shouldSatisfy` isLeft
    it "refuses bad JSON and missing fields" $ do
      parseVerdict (judge "nope") `shouldSatisfy` isLeft
      parseVerdict (judge "{\"mistake\": \"m\", \"confidence\": 0.9}") `shouldSatisfy` isLeft
      parseVerdict (judge "{\"lesson\": \"l\", \"confidence\": 0.9, \"tell_yourself\": \"t\"}") `shouldSatisfy` isLeft
    it "drops the lesson under confidence 0.6" $
      fmap (fmap saidText . vLesson) (parseVerdict (judge "{\"mistake\": \"m\", \"lesson\": \"l\", \"confidence\": 0.4, \"tell_yourself\": \"t\"}")) `shouldBe` Right Nothing
    it "cuts the lesson and tell_yourself to 300 characters" $ do
      let long = T.replicate 3000 "a"
          r = parseVerdict (judge ("{\"mistake\": \"m\", \"lesson\": \"" <> long <> "\", \"confidence\": 0.9, \"tell_yourself\": \"" <> long <> "\"}"))
      fmap (fmap (T.length . saidText) . vLesson) r `shouldBe` Right (Just 300)
      fmap (T.length . saidText . vTellYourself) r `shouldBe` Right 300

  describe "judgeMessages" $ do
    it "is a system and a user message with the outcome and the contract" $ do
      let ms = judgeMessages (view [msg 1 "hello"])
      map fst ms `shouldBe` ["system", "user"]
      userText (view [msg 1 "hello"]) `shouldSatisfy` T.isInfixOf "rejected: still sorry"
      userText (view [msg 1 "hello"]) `shouldSatisfy` T.isInfixOf "assistant: hello"
      T.concat (map (partText . snd) ms) `shouldSatisfy` T.isInfixOf "tell_yourself"
    it "keeps only the last 40 events" $ do
      let u = userText (view [msg s ("line " <> T.pack (show s) <> " end") | s <- [1 .. 60]])
      u `shouldSatisfy` T.isInfixOf "line 21 end"
      u `shouldSatisfy` T.isInfixOf "line 60 end"
      u `shouldNotSatisfy` T.isInfixOf "line 20 end"
    it "shows a tool call with its args cut to 200 and its result cut to 500" $ do
      let es =
            [ (1, 1, "tool_start", object ["name" .= ("mcp_lean_lsp_lean_run_code" :: Text), "args" .= object ["code" .= T.replicate 1000 "c"], "call_id" .= ("c1" :: Text)])
            , (2, 2, "tool_end", object ["name" .= ("mcp_lean_lsp_lean_run_code" :: Text), "call_id" .= ("c1" :: Text), "ok" .= True, "result" .= T.replicate 2000 "r"])
            , (3, 3, "step", object ["n" .= (1 :: Int)]) ]
          u = userText (view es)
      u `shouldSatisfy` T.isInfixOf "tool mcp_lean_lsp_lean_run_code("
      u `shouldSatisfy` T.isInfixOf (T.replicate 500 "r")
      u `shouldNotSatisfy` T.isInfixOf (T.replicate 501 "r")
      u `shouldSatisfy` T.isInfixOf (T.replicate 150 "c")
      u `shouldNotSatisfy` T.isInfixOf (T.replicate 200 "c")

  describe "judgeMessages redaction" $
    it "redacts the outcome text and the goal before the judge sees them" $ do
      let av = (view [msg 1 "hello"]) {avOutcomeText = "model error: bad key sk-ant-api03-abcdefghijklmnopqrstuvwxyz", avGoal = "g OPENAI_API_KEY=sk-live-abcdefghijklmnop"}
      userText av `shouldSatisfy` (not . T.isInfixOf "abcdefghijklmnopqrstuvwxyz")
      userText av `shouldSatisfy` (not . T.isInfixOf "sk-live-abcdefghijklmnop")
      userText av `shouldSatisfy` T.isInfixOf "model error: bad key"

  describe "judgeMessages pairing" $ do
    it "joins a native start with no call_id to its end" $ do
      let es =
            [ (1, 1, "tool_start", object ["name" .= ("lean_run_code" :: Text), "args" .= object ["code" .= ("x" :: Text)]])
            , (2, 2, "tool_end", object ["name" .= ("lean_run_code" :: Text), "call_id" .= ("k1" :: Text), "result" .= ("fine" :: Text)]) ]
          u = userText (view es)
      u `shouldSatisfy` T.isInfixOf "tool lean_run_code({\"code\":\"x\"}) -> fine"
      length (filter (T.isPrefixOf "tool ") (T.lines u)) `shouldBe` 1
    it "does not join concurrent native calls of one name to a guessed end" $ do
      let start s a = (s, fromIntegral s, "tool_start", object ["name" .= ("lean_run_code" :: Text), "args" .= object ["code" .= (a :: Text)]])
          end s r = (s, fromIntegral s, "tool_end", object ["name" .= ("lean_run_code" :: Text), "call_id" .= ("k" <> r :: Text), "result" .= r])
          u = userText (view [start 1 "a", start 2 "b", end 3 "rb", end 4 "ra"])
      u `shouldSatisfy` T.isInfixOf "tool lean_run_code({\"code\":\"a\"})\n"
      u `shouldSatisfy` T.isInfixOf "tool lean_run_code -> rb"
    it "shows an end with no start alone" $ do
      let es = [(1, 1, "tool_end", object ["name" .= ("lean_build" :: Text), "call_id" .= ("z" :: Text), "result" .= ("built" :: Text)])]
      userText (view es) `shouldSatisfy` T.isInfixOf "tool lean_build -> built"

  describe "judgeAttempt" $ do
    it "retries a reply it cannot read and sums the cost" $ do
      replies <- newIORef [("nope", 0.01), (good, 0.02)]
      seen <- newIORef []
      let ask role ms = do
            modifyIORef seen (++ [(role, ms)])
            r <- atomicModifyIORef replies (\xs -> (drop 1 xs, take 1 xs))
            pure (maybe (Left (AskFailed "none")) Right (headOr r))
          headOr = \case
            (x : _) -> Just x
            [] -> Nothing
      r <- judged ask (view [msg 1 "hello"])
      case r of
        Asked c (Right v) -> do
          c `shouldSatisfy` (\x -> abs (x - 0.03) < 1e-9)
          vConfidence v `shouldBe` 0.8
        _ -> expectationFailure "expected a verdict"
      calls <- readIORef seen
      map fst calls `shouldBe` [Judge, Judge]
      -- the judge's own reply does not go back into its prompt (it has no
      -- 'toPrompt'): the retry is the first call's messages with why
      map (map fst . snd) calls `shouldBe` [["system", "user"], ["system", "user"]]
      case map snd calls of
        [first, second] -> do
          lookup "system" second `shouldBe` lookup "system" first
          lookup "user" second `shouldBe` fmap (<> "\n\nYour previous reply could not be used: the reply is not the JSON object asked for\nReply with the JSON object only.") (lookup "user" first)
          T.concat (map snd second) `shouldNotSatisfy` T.isInfixOf "nope"
        _ -> expectationFailure "expected two calls"
    it "gives NoCredit when the call has no credit" $ do
      r <- judged (\_ _ -> pure (Left AskNoCredit)) (view [msg 1 "hello"])
      case r of
        NoCredit _ -> pure ()
        _ -> expectationFailure "expected NoCredit"

  describe "lessonKey" $ do
    it "is the first Lean identifier, else the normalised text" $ do
      lessonKey "Use Nat.succ_le here, not Nat.le_of_lt." `shouldBe` "nat.succ_le"
      lessonKey "Try `omega` first." `shouldBe` "try omega first"
      lessonKey "Use `Nat.succ_le` here." `shouldBe` "nat.succ_le"
      lessonKey "Prefer `foo_bar_baz` to the long proof." `shouldBe` "foo_bar_baz"
    it "a file name, a version, a tool or a tactic is not an identifier" $ do
      lessonKey "Edit Foo.lean only." `shouldBe` "edit foo.lean only"
      lessonKey "Lean 4.12.0 renamed it." `shouldBe` normaliseLessonOf "Lean 4.12.0 renamed it."
      lessonKey "Search with `lean_loogle` first." `shouldBe` normaliseLessonOf "Search with `lean_loogle` first."
      lessonKey "Use `simp_all` here." `shouldBe` normaliseLessonOf "Use `simp_all` here."
    it "lessons that share a tactic are not merged" $ do
      lessonKey "Use `simp` with the lemma list." `shouldNotBe` lessonKey "Do not call `simp` on a goal with a coercion."
      sameLesson "Use `simp` only after `intro`." "Never use `simp` on casts; use `push_cast`." `shouldBe` False
      lessonKey "Check the file, before you finish!" `shouldBe` "check the file before you finish"

  describe "sameLesson" $ do
    it "the same identifier is the same lesson" $
      sameLesson "Use Nat.succ_le here" "use nat.succ_le there" `shouldBe` True
    it "near texts are the same, far ones are not" $ do
      sameLesson "Check the file before you finish." "Check the file before finishing." `shouldBe` True
      sameLesson (T.replicate 30 "a") (T.replicate 10 "a" <> T.replicate 20 "b") `shouldBe` False
    it "different lemmas are not the same lesson" $
      sameLesson "Use Nat.succ_le here" "Use Nat.succ_lt here" `shouldBe` False
    it "texts over 120 characters are not compared by edit distance" $
      sameLesson (T.replicate 130 "a") (T.replicate 130 "a" <> "b") `shouldBe` False

normaliseLessonOf :: Text -> Text
normaliseLessonOf = normaliseLesson

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)
