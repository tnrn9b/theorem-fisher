-- | The launch's model: @--model M@ in the command (which @fisher launch
-- --model M@ puts there), else @fisher.autoformalize.model@, else the
-- backend's own default. Each backend's child takes it in its own flag:
-- claude-code @--model@, codex @-m@, the native @fisher chat -m@.
module Fisher.LaunchModelSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson
import Data.Aeson.Key (Key)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (canonicalizePath)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.A2Fixture
import Fisher.Golden
import Fisher.Workflow.Command
import Fisher.Workflow.Handoff
import Fisher.Workflow.Plan
import Fisher.Workflow.Provers (dropArgSeparator, stripModelFlag)
import Fisher.Workflow.Settings (resolveLaunchModel)
import Fisher.Workflow.Types

-- | Plan a command for a Lean project from a recorded case, with the
-- backend and more @fisher.autoformalize@ keys (given under the old @gauss@ root).
planLean :: Text -> Text -> [(Key, Value)] -> [(String, String)] -> Text -> (Either WorkflowError WorkflowPlan -> IO ()) -> IO ()
planLean caseName backend auto extraEnv command k = do
  let path = "test/golden/a2/workflow-plans.json"
  cases <- goldenCases path
  file <- either fail pure =<< (Data.Aeson.eitherDecodeFileStrict path :: IO (Either String Value))
  let c = head [x | x <- cases, textField "name" x == caseName]
  withSystemTempDirectory "fisher-model" $ \d -> do
    root <- canonicalizePath d
    base0 <- setupCase root (field "extras" c)
    let base = foldr (uncurry Map.insert) (Map.delete "OPENROUTER_API_KEY" base0) extraEnv
        cfg = object ["gauss" .= object ["autoformalize" .= object (["backend" .= backend, "auth_mode" .= textField "auth_mode" c] <> auto)]]
        li = LaunchInputs (machineFor file (root </> "gauss-home")) cfg base False (const (pure Nothing)) (Just (root </> "project"))
    r <- withProcessEnv base (resolveWorkflowRequest li command)
    k r

argvOf :: Either WorkflowError WorkflowPlan -> IO [Text]
argvOf = either (fail . T.unpack . errorMessage) (pure . hrArgv . wpRequest)

-- | The value valueAfter a flag in the argv.
valueAfter :: Text -> [Text] -> Maybe Text
valueAfter f argv = case dropWhile (/= f) argv of
  (_ : v : _) -> Just v
  _ -> Nothing

claude, codex, native :: [(Key, Value)] -> Text -> (Either WorkflowError WorkflowPlan -> IO ()) -> IO ()
claude auto = planLean "claude-prove-prewarmed" "claude-code" auto []
codex auto = planLean "codex-prove-tokens" "codex" auto []
native auto = planLean "claude-prove-prewarmed" "fisher" auto [("OPENROUTER_API_KEY", "sk-or-test")]

configured :: [(Key, Value)]
configured = [("model", String "from-config")]

spec :: Spec
spec = do
  describe "resolveLaunchModel" $ do
    let cfg = object ["gauss" .= object ["autoformalize" .= object configured]]
    it "takes the flag, then the config, then nothing" $ do
      resolveLaunchModel (Just "from-flag") cfg `shouldBe` Just "from-flag"
      resolveLaunchModel Nothing cfg `shouldBe` Just "from-config"
      resolveLaunchModel Nothing (object []) `shouldBe` Nothing
    it "reads an empty value as unset" $
      resolveLaunchModel (Just " ") (object ["gauss" .= object ["autoformalize" .= object ["model" .= ("" :: Text)]]]) `shouldBe` Nothing

  describe "stripModelFlag" $ do
    it "takes --model M and --model=M out of the arguments and the backend command" $ do
      ws <- either (fail . show) pure (parseWorkflowCommand "/prove --model glm-5 Main.lean")
      fmap (\(m, w) -> (m, wsArgs w, wsBackend w)) (stripModelFlag ws) `shouldBe` Right (Just "glm-5", "Main.lean", "/lean4:prove Main.lean")
      ws2 <- either (fail . show) pure (parseWorkflowCommand "/prove Main.lean --model=glm-5")
      fmap (\(m, w) -> (m, wsArgs w)) (stripModelFlag ws2) `shouldBe` Right (Just "glm-5", "Main.lean")
    it "leaves a command without it as it is" $ do
      ws <- either (fail . show) pure (parseWorkflowCommand "/prove Main.lean")
      stripModelFlag ws `shouldBe` Right (Nothing, ws)
    it "stops at a literal --, and dropArgSeparator then drops it" $ do
      ws <- either (fail . show) pure (parseWorkflowCommand "/draft -- the --model of x")
      stripModelFlag ws `shouldBe` Right (Nothing, ws)
      let ws' = dropArgSeparator ws
      (wsArgs ws', wsBackend ws') `shouldBe` ("the --model of x", "/lean4:draft the --model of x")
      dropArgSeparator ws' `shouldBe` ws'
    it "refuses --model without a name" $ do
      ws <- either (fail . show) pure (parseWorkflowCommand "/prove Main.lean --model")
      either errorMessage (const "") (stripModelFlag ws) `shouldBe` "--model takes a model name."

  describe "the claude-code child" $ do
    it "keeps the default model" $ claude [] "/prove Main.lean" $ \r -> do
      argv <- argvOf r
      valueAfter "--model" argv `shouldBe` Just claudeModel
    it "takes the config's model" $ claude configured "/prove Main.lean" $ \r -> do
      argv <- argvOf r
      valueAfter "--model" argv `shouldBe` Just "from-config"
    it "takes the flag over the config" $ claude configured "/prove --model from-flag Main.lean" $ \r -> do
      argv <- argvOf r
      valueAfter "--model" argv `shouldBe` Just "from-flag"
      either (const "") (wsBackend . wpSpec) r `shouldBe` "/lean4:prove Main.lean"
      T.isInfixOf "--model" (last argv) `shouldBe` False

    it "keeps the words after a literal -- as the command's" $ claude [] "/prove Main.lean --model from-flag -- --model nope" $ \r -> do
      argv <- argvOf r
      valueAfter "--model" argv `shouldBe` Just "from-flag"
      either (const "") (wsBackend . wpSpec) r `shouldBe` "/lean4:prove Main.lean --model nope"

  describe "the codex child" $ do
    it "passes no model by default, as upstream" $ codex [] "/prove Main.lean" $ \r -> do
      argv <- argvOf r
      valueAfter "-m" argv `shouldBe` Nothing
    it "takes the config's model with -m" $ codex configured "/prove Main.lean" $ \r -> do
      argv <- argvOf r
      take 3 argv `shouldBe` [head argv, "-m", "from-config"]
    it "takes the flag over the config" $ codex configured "/prove Main.lean --model from-flag" $ \r -> do
      argv <- argvOf r
      valueAfter "-m" argv `shouldBe` Just "from-flag"

  describe "the native child" $ do
    it "keeps the provider's default model" $ native [] "/prove Main.lean" $ \r -> do
      argv <- argvOf r
      valueAfter "-m" argv `shouldBe` Just nativeOpenRouterModel
    it "takes the config's model" $ native configured "/prove Main.lean" $ \r -> do
      argv <- argvOf r
      valueAfter "-m" argv `shouldBe` Just "from-config"
    it "takes the flag over the config" $ native configured "/prove --model=from-flag Main.lean" $ \r -> do
      argv <- argvOf r
      valueAfter "-m" argv `shouldBe` Just "from-flag"
      T.isInfixOf "/lean4:prove Main.lean." (last argv) `shouldBe` True

  describe "the headless form of a planned Lean child (fisher launch --print)" $ do
    let headless r = do
          p <- either (fail . T.unpack . errorMessage) pure r
          let argv = hrArgv (wpRequest p)
          h <- either (fail . T.unpack) pure (printArgv (mcBackend (wpContext p)) argv)
          pure (argv, h)
    it "turns the native child's final -q PROMPT into -p PROMPT" $ native [] "/prove Main.lean" $ \r -> do
      (argv, h) <- headless r
      drop (length argv - 2) argv `shouldBe` ["-q", last argv]
      h `shouldBe` init (init argv) <> ["-p", last argv]
      "-q" `elem` h `shouldBe` False
    it "gives the claude-code child -p before its prompt" $ claude [] "/prove Main.lean" $ \r -> do
      (argv, h) <- headless r
      h `shouldBe` init argv <> ["-p", last argv]
    it "runs the codex child as codex exec" $ codex [] "/prove Main.lean" $ \r -> do
      (argv, h) <- headless r
      h `shouldBe` take 1 argv <> ["exec"] <> drop 1 argv
