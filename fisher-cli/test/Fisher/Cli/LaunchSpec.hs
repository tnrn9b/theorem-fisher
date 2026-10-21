-- | @fisher plan@ and @fisher launch@'s own flags, anywhere among the
-- arguments, and a headless launch run end to end against a fake native
-- backend.
module Fisher.Cli.LaunchSpec (spec) where

import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (canonicalizePath)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (setFileMode)
import Test.Hspec

import Fisher.Project (InitOptions (..), defaultInitOptions, initializeProject)
import Fisher.Workflow.Command (WorkflowSpec (..))
import Fisher.Workflow.Handoff (HandoffRequest (..))
import Fisher.Workflow.Plan (ManagedContext (..), WorkflowPlan (..))

import Fisher.Cli.Check (CheckOptions (..), defaultCheckOptions, parseCheckArgs)
import Fisher.Cli.Launch (LaunchFlags (..), OwnArgs (..), launchHeadless, launchRequest, ownArgsCommand)
import Fisher.Prover (ProverKind (..))

spec :: Spec
spec = do
 describe "ownArgsCommand" $ do
  it "gives check its arguments as they are, flags before or after the first target" $ do
    let flagsFirst = ["check", "--prover", "isabelle", "--isabelle", "/x/isabelle", "Demo.thy:t"]
        flagsLast = ["check", "Demo.thy:t", "--prover", "isabelle", "--isabelle", "/x/isabelle"]
        want = Right (Just defaultCheckOptions {coProver = Just Isabelle, coIsabelle = Just "/x/isabelle", coTargets = ["Demo.thy:t"]})
    ownArgsCommand flagsFirst `shouldBe` Just (OwnCheck (drop 1 flagsFirst))
    ownArgsCommand flagsLast `shouldBe` Just (OwnCheck (drop 1 flagsLast))
    let parsed = \case
          Just (OwnCheck a) -> Just (parseCheckArgs a)
          _ -> Nothing
    parsed (ownArgsCommand flagsFirst) `shouldBe` Just want
    parsed (ownArgsCommand flagsLast) `shouldBe` Just want
    ownArgsCommand ["check"] `shouldBe` Just (OwnCheck [])
    ownArgsCommand ["check", "--help"] `shouldBe` Just (OwnCheck ["--help"])

  it "gives plan and launch their flags before the command" $ do
    ownArgsCommand ["launch", "--model", "m", "--print", "/prove", "A.v"] `shouldBe` Just (OwnLaunch ["--model", "m", "--print", "/prove", "A.v"])
    ownArgsCommand ["plan", "--model=m", "/prove"] `shouldBe` Just (OwnPlan ["--model=m", "/prove"])

  it "gives scratch its flags even when the first argument is one" $ do
    ownArgsCommand ["scratch", "--list"] `shouldBe` Just (OwnScratch ["--list"])
    ownArgsCommand ["scratch", "--statement", "1 = 1"] `shouldBe` Just (OwnScratch ["--statement", "1 = 1"])
    ownArgsCommand ["scratch", "--help"] `shouldBe` Nothing

  it "leaves help, a bare plan or launch, and the other commands to optparse" $ do
    ownArgsCommand ["launch"] `shouldBe` Nothing
    ownArgsCommand ["plan", "--help"] `shouldBe` Nothing
    ownArgsCommand ["launch", "-h"] `shouldBe` Nothing
    ownArgsCommand ["doctor", "--all"] `shouldBe` Nothing
    ownArgsCommand [] `shouldBe` Nothing

 describe "launchRequest" $ do
  it "passes a command without flags through, normalised" $ do
    launchRequest ["/prove", "Main.lean"] `shouldBe` Right (LaunchFlags Nothing False, "/prove Main.lean")
    launchRequest ["prove", "Main.lean"] `shouldBe` Right (LaunchFlags Nothing False, "/prove Main.lean")

  it "puts --model M into the command, where the plan takes it" $ do
    launchRequest ["--model", "glm-5", "/prove", "Main.lean"] `shouldBe` Right (LaunchFlags (Just "glm-5") False, "/prove Main.lean --model glm-5")
    launchRequest ["--model=glm-5", "/draft", "two", "evens"] `shouldBe` Right (LaunchFlags (Just "glm-5") False, "/draft two evens --model glm-5")

  it "takes --print (or -p) for a headless run, with --model in either order" $ do
    launchRequest ["--print", "/prove", "Main.lean"] `shouldBe` Right (LaunchFlags Nothing True, "/prove Main.lean")
    launchRequest ["-p", "--model", "m", "/prove"] `shouldBe` Right (LaunchFlags (Just "m") True, "/prove --model m")
    launchRequest ["--model", "m", "--print", "/prove"] `shouldBe` Right (LaunchFlags (Just "m") True, "/prove --model m")

  it "refuses --model without a name" $ do
    launchRequest ["--model"] `shouldBe` Left "--model takes a model name."
    launchRequest ["--model=", "/prove"] `shouldBe` Left "--model takes a model name."

  it "takes the flags anywhere among the words" $ do
    launchRequest ["/prove", "A.v", "--print"] `shouldBe` Right (LaunchFlags Nothing True, "/prove A.v")
    launchRequest ["/prove", "-p", "A.v"] `shouldBe` Right (LaunchFlags Nothing True, "/prove A.v")
    launchRequest ["/prove", "A.v", "--model", "m"] `shouldBe` Right (LaunchFlags (Just "m") False, "/prove A.v --model m")
    launchRequest ["/prove", "A.v", "--model=m", "-p"] `shouldBe` Right (LaunchFlags (Just "m") True, "/prove A.v --model m")

  it "takes --prover before the command, where the plan takes it" $ do
    launchRequest ["--prover", "rocq", "/prove", "X"] `shouldBe` Right (LaunchFlags Nothing False, "/prove X --prover rocq")
    launchRequest ["--prover=isabelle", "--print", "/draft", "t"] `shouldBe` Right (LaunchFlags Nothing True, "/draft t --prover isabelle")
    launchRequest ["/prove", "--prover", "rocq", "X", "--model", "m"] `shouldBe` Right (LaunchFlags (Just "m") False, "/prove X --prover rocq --model m")
    launchRequest ["--prover"] `shouldBe` Left "--prover takes lean4, rocq or isabelle."

  -- the -- stays in the command, after the flags put back, so that the
  -- plan stops there too and then drops it
  it "stops at a literal --: the words after it are command text" $ do
    launchRequest ["--print", "/draft", "--", "a", "-p", "adic", "--model", "of", "x"]
      `shouldBe` Right (LaunchFlags Nothing True, "/draft -- a -p adic --model of x")
    launchRequest ["--model", "m", "/draft", "--", "--prover", "notes"] `shouldBe` Right (LaunchFlags (Just "m") False, "/draft --model m -- --prover notes")

  it "leaves a quoted description with such words alone" $
    launchRequest ["--print", "/draft", "the -p adic --model of x"] `shouldBe` Right (LaunchFlags Nothing True, "/draft the -p adic --model of x")

  it "takes a repeated flag once, and refuses two different values" $ do
    launchRequest ["--model", "m", "/prove", "--model=m"] `shouldBe` Right (LaunchFlags (Just "m") False, "/prove --model m")
    launchRequest ["--model", "a", "/prove", "--model", "b"] `shouldBe` Left "--model given twice: a and b."
    launchRequest ["--prover", "rocq", "/prove", "--prover", "isabelle"] `shouldBe` Left "--prover given twice: rocq and isabelle."

 describe "launchHeadless" $
  it "runs fisher launch /prove A.v --print headless on a fake native backend and exits with its status" $
    withSystemTempDirectory "fisher-launch" $ \d0 -> do
      d <- canonicalizePath d0
      let exe = d </> "fisher-native"
      TIO.writeFile exe $ T.unlines
        [ "#!/bin/sh"
        , "cat > \"$FAKE_DIR/stdin.txt\""
        , "printf '%s\\n' \"$@\" > \"$FAKE_DIR/args.txt\""
        , "echo answer"
        , "exit 4" ]
      setFileMode exe 0o755
      writeFile (d </> "_RocqProject") "-Q theories Demo\n"
      project <- initializeProject d defaultInitOptions {initName = Just "Demo"}
      env <- Map.insert "FAKE_DIR" d . Map.fromList <$> getEnvironment
      asked <- newIORef []
      let argv = [T.pack exe, "chat", "--provider", "openrouter", "-m", "M", "-t", "fisher-rocq", "-q", "the prompt"]
          ws = WorkflowSpec "prove" "/prove" "/prove" "/rocq:prove A.v" "A.v"
          ctx = ManagedContext "fisher" d d d d d (d </> "mcp.json") Nothing d Nothing Nothing Nothing Nothing
          req = HandoffRequest argv d env "helper" "auto" "fisher:prove" "Fisher prove session" Nothing
          resolve c = modifyIORef asked (<> [c]) >> pure (Right (WorkflowPlan req ctx "A.v" project ws))
      code <- launchHeadless resolve ["/prove", "A.v", "--print"]
      code `shouldBe` ExitFailure 4
      readIORef asked `shouldReturn` ["/prove A.v"]
      TIO.readFile (d </> "args.txt") `shouldReturn` "chat\n--provider\nopenrouter\n-m\nM\n-t\nfisher-rocq\n-p\nthe prompt\n"
      TIO.readFile (d </> "stdin.txt") `shouldReturn` ""

