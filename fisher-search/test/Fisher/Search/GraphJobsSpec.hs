{-# LANGUAGE DataKinds #-}

module Fisher.Search.GraphJobsSpec (spec) where

import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Fisher.Role (KnownRole, partText, saidText, static, toPrompt)
import Fisher.Role.Agent (RoleCtx, ask, roleCtx, runRole)
import Fisher.Search.Graph (AcceptError (..), acceptErrorText)
import Fisher.Search.GraphJobs
import Fisher.Search.Sketch (SketchError (..), sketchErrorText)
import Fisher.Search.Split (Problem (..), plannerMessages)

-- | An Ask that replies with the given texts in turn, 0.25 dollars each,
-- recording the role and the messages of every call.
scripted :: [Text] -> IO (Ask, IORef [(Role, [(Text, Text)])])
scripted replies = do
  left <- newIORef replies
  calls <- newIORef []
  let ask' role msgs = do
        modifyIORef' calls (++ [(role, msgs)])
        next <- atomicModifyIORef' left (\case [] -> ([], Nothing); (x : xs) -> (xs, Just x))
        pure (maybe (Left (AskFailed "no more replies")) (\t -> Right (t, 0.25)) next)
  pure (ask', calls)

-- | A role's context over the Ask: no checker (these jobs never reach the
-- kernel), nothing recorded.
ctxOf :: KnownRole r => Ask -> RoleCtx r
ctxOf ask' = roleCtx ask' Nothing (\_ _ -> pure ()) (\_ _ _ -> pure ())

spec :: Spec
spec = describe "the graph jobs" $ do
  it "a retry carries the previous reply as the planner's own part" $ do
    let first = "I would split on n, but here are no blocks."
        problem = "the reply needs a blueprint block and a lean block"
        base = "theorem main : P := by sorry"
        board = static "# Plan for main\n\n- main (open)"
    (ask', calls) <- scripted [first, "still no blocks"]
    r <- runRole (ctxOf @'Planner ask') (planJob base "main" "P" [] board ["stuck"] mempty)
    case r of
      Asked d (Left e) -> (d, partText e) `shouldBe` (0.5, problem)
      _ -> expectationFailure "expected the job to fail after two replies"
    -- the first reply as the planner said it, through the role monad's ask
    (again, _) <- scripted [first]
    said <- runRole (ctxOf @'Planner again) (ask [("user", static "go")])
    case said of
      Left _ -> expectationFailure "expected a reply"
      Right (s, _) -> do
        cs <- readIORef calls
        map fst cs `shouldBe` [Planner, Planner]
        case map snd cs of
          [m1, m2] -> do
            -- the first prompt is untouched; the second adds the planner's
            -- own reply (toPrompt) and the problem, and nothing else
            take 2 m2 `shouldBe` m1
            drop 2 m2 `shouldBe`
              [ ("assistant", partText (toPrompt s))
              , ("user", "Your reply could not be used: " <> problem <> "\nReply again with both blocks.") ]
            m2 `shouldBe` map (fmap partText) (plannerMessages base "main" [] board ["stuck"] mempty (Just (toPrompt s, FormatError (static problem))))
            T.concat (map snd m2) `shouldNotSatisfy` T.isInfixOf "lesson"
          _ -> expectationFailure "expected two calls"

  it "the false check's reason is the verifier's" $ do
    (ask', calls) <- scripted ["{\"false\": false, \"reason\": \"n = 1 is no counterexample\"}"]
    r <- runRole (ctxOf @'Verifier ask') (falseJob "lemma a : forall n : Nat, n > 0" "n = 1 is a counterexample")
    case r of
      Asked d (Right (f, why)) -> (d, f, saidText why) `shouldBe` (0.25, False, "n = 1 is no counterexample")
      _ -> expectationFailure "expected a verdict"
    map fst <$> readIORef calls >>= (`shouldBe` [Verifier])

  it "the false check retries an unreadable reply with it as the verifier's own part" $ do
    (ask', calls) <- scripted ["no json", "{\"false\": true, \"reason\": \"n = 0\"}"]
    r <- runRole (ctxOf @'Verifier ask') (falseJob "lemma a : forall n : Nat, n > 0" "n = 0 is a counterexample")
    case r of
      Asked d (Right (f, why)) -> (d, f, saidText why) `shouldBe` (0.5, True, "n = 0")
      _ -> expectationFailure "expected a verdict"
    cs <- readIORef calls
    map (map fst . snd) cs `shouldBe` [["system", "user"], ["system", "user", "assistant", "user"]]
    case cs of
      [_, (_, m2)] -> lookup "assistant" m2 `shouldBe` Just "no json"
      _ -> expectationFailure "expected two calls"

  it "a sketch error's text is the sentence's, with the planner's names from its reply" $ do
    let reply = "```lean\nlemma aux : A := by simp\nlemma dup : B := by sorry\naxiom cheat : P\nstray words\n```"
    (ask', _) <- scripted [reply]
    said <- runRole (ctxOf @'Planner ask') (ask [("user", static "go")])
    case said of
      Left _ -> expectationFailure "expected a reply"
      Right (s, _) -> do
        let errs = [ NoNodeDecl, NodeStatementChanged, LemmaNotOpen "aux", DuplicateName "dup", NameClash "aux"
                   , NameClash "Foo.aux", DeniedLine "axiom cheat : P", ExtraText "stray words" ]
        [partText (sketchProblem "Foo.main" s e) | e <- errs] `shouldBe` map sketchErrorText errs

  it "why a split could not be accepted: Fisher's sentence, with the planner's names from its sketch" $ do
    let reply = "```blueprint\nsplit\n```\n```lean\nlemma aux : A := by sorry\nlemma Foo.dup : B := by sorry\ntheorem main : P := by sorry\n```"
    (ask', _) <- scripted [reply]
    said <- runRole (ctxOf @'Planner ask') (ask [("user", static "go")])
    case said of
      Left _ -> expectationFailure "expected a reply"
      Right (s, _) -> do
        let errs = [NoSuchSplit, NotProposed, NoSuchParent, Cycle "Foo.aux", DuplicateLemma "Foo.dup", LemmaFailed "aux"]
        [partText (acceptProblem "Foo.main" s e) | e <- errs] `shouldBe` map acceptErrorText errs

  it "a refused sketch: the job's error is a part, the planner's line read out of its lean block" $ do
    let reply = "```blueprint\nsplit on n\n```\n```lean\nstray words\ntheorem main : P := by sorry\n```"
    (ask', _) <- scripted [reply, reply]
    r <- runRole (ctxOf @'Planner ask') (planJob "theorem main : P := by sorry" "main" "P" [] mempty [] mempty)
    case r of
      Asked d (Left e) -> (d, partText e) `shouldBe` (0.5, sketchErrorText (ExtraText "stray words"))
      _ -> expectationFailure "expected the job to fail after two replies"

  it "a failed call's error is Fisher's sentence and the provider's text" $ do
    (ask', calls) <- scripted []
    r <- runRole (ctxOf @'Verifier ask') (falseJob "lemma a : True" "no")
    case r of
      Asked d (Left e) -> (d, partText e) `shouldBe` (0, "the model call failed: no more replies")
      _ -> expectationFailure "expected a failure"
    length <$> readIORef calls >>= (`shouldBe` 2)
