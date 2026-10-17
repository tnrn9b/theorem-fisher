-- | Launch plans against what the pinned OpenGauss produced from the same
-- setups (@tools/corpus/a2.py@).
module Fisher.PlanGoldenSpec (spec) where

import Control.Monad (forM_, void, when)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (canonicalizePath, doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Fisher.A2Fixture
import Fisher.Golden
import Fisher.Workflow.Command
import Fisher.Workflow.Handoff
import Fisher.Workflow.Plan
import Fisher.Workflow.Types
import Fisher.Project (projectLabel)

golden :: FilePath -> FilePath
golden n = "test/golden/a2" </> n

withRoot :: (FilePath -> IO a) -> IO a
withRoot k = withSystemTempDirectory "fisher-a2" (\d -> canonicalizePath d >>= k)

configFor :: Value -> FilePath -> Value
configFor c root =
  let extras = field "extras" c :: Value
      get k = case extras of
        Object o -> KM.lookup (K.fromText k) o
        _ -> Nothing
      auto = KM.fromList $
        [("backend", String (textField "backend" c)), ("auth_mode", String (textField "auth_mode" c))]
          <> [("managed_state_dir", String (T.pack (root </> T.unpack d))) | Just (String d) <- [get "state_dir"]]
          <> [("handoff_mode", m) | Just m <- [get "handoff_mode"]]
   in object ["gauss" .= object ["autoformalize" .= Object auto]]

cwdFor :: Value -> FilePath -> FilePath
cwdFor c root = case field "extras" c :: Value of
  Object o | Just (String d) <- KM.lookup "cwd" o -> root </> T.unpack d
  _ -> root </> "project"

envDelta :: FilePath -> Env -> Env -> Value
envDelta root env base =
  object
    [ "set" .= Object (KM.fromList [ (K.fromString k, String (normText root (T.pack v))) | (k, v) <- Map.toList env, Map.lookup k base /= Just v ])
    , "removed" .= [T.pack k | k <- Map.keys base, Map.notMember k env] ]

requestView :: FilePath -> Env -> HandoffRequest -> Value
requestView root base r =
  object
    [ "argv" .= map (normText root) (hrArgv r)
    , "cwd" .= normText root (T.pack (hrCwd r))
    , "env" .= envDelta root (hrEnv r) base
    , "mode" .= hrMode r, "requested_mode" .= hrRequestedMode r, "mode_note" .= hrModeNote r
    , "source" .= hrSource r, "label" .= hrLabel r ]

errorView :: FilePath -> WorkflowError -> Value
errorView root e = object ["class" .= errorClass e, "message" .= normText root (errorMessage e)]

subObject :: Text -> Value -> Maybe Value
subObject k = \case
  Object o -> KM.lookup (K.fromText k) o
  _ -> Nothing

-- | Run one recorded case, and check every part of what upstream recorded.
runCase :: Value -> Value -> (LaunchInputs -> IO (Either WorkflowError (FilePath -> Env -> Value))) -> IO ()
runCase file c resolve = withRoot $ \root -> do
  base <- setupCase root (field "extras" c)
  let li = LaunchInputs (machineFor file (root </> "gauss-home")) (configFor c root) base False
             (const (pure Nothing)) (Just (cwdFor c root))
  snapBefore <- snapshot root
  result <- withProcessEnv base (resolve li)
  snapAfter <- snapshot root
  case (result, subObject "plan" c, subObject "error" c) of
    (Right view, Just expected, _) -> view root base `shouldBe` expected
    (Left e, _, Just expected) -> errorView root e `shouldBe` expected
    (Right _, Nothing, Just expected) -> expectationFailure ("planned, but upstream failed: " <> show expected)
    (Left e, _, Nothing) -> expectationFailure ("failed, but upstream planned: " <> show e)
    _ -> expectationFailure "case has neither plan nor error"
  normalize root (changes snapBefore snapAfter) `shouldBe` field "files" c
  logged <- doesFileExist (root </> "fake.log")
  calls <- if logged then T.lines <$> TIO.readFile (root </> "fake.log") else pure []
  map (normText root) calls `shouldBe` (field "calls" c :: [Text])

workflowView :: WorkflowPlan -> FilePath -> Env -> Value
workflowView p root base =
  object
    [ "request" .= requestView root base (wpRequest p)
    , "staged_paths" .= Object (KM.fromList [(K.fromText k, String (normText root v)) | (k, v) <- stagedPaths p])
    , "user_instruction" .= wpInstruction p
    , "workflow_kind" .= wsKind (wpSpec p)
    , "frontend_command" .= wsFrontend (wpSpec p)
    , "canonical_command" .= wsCanonical (wpSpec p)
    , "backend_command" .= wsBackend (wpSpec p)
    , "project_label" .= projectLabel (wpProject p) ]

chatView :: ChatPlan -> FilePath -> Env -> Value
chatView p root base =
  object
    [ "request" .= requestView root base (cpRequest p)
    , "staged_paths" .= object
        [ "backend_name" .= cpBackend p, "cwd" .= normText root (T.pack (cpCwd p))
        , "argv0" .= normText root (fromMaybe "" (safeHead (hrArgv (cpRequest p)))) ]
    , "user_instruction" .= cpInstruction p ]
  where
    safeHead (x : _) = Just x
    safeHead [] = Nothing

loadFile :: FilePath -> IO (Value, [Value])
loadFile path = do
  cases <- goldenCases path
  whole <- either fail pure =<< (Data.Aeson.eitherDecodeFileStrict path :: IO (Either String Value))
  pure (whole, cases)

spec :: Spec
spec = do
  describe "workflow-plans.json" $ do
    (file, cases) <- runIO (loadFile (golden "workflow-plans.json"))
    forM_ cases $ \c -> it (T.unpack (textField "name" c)) $
      runCase file c $ \li -> do
        let cmd = textField "command" c
            twice = subObject "twice" (field "extras" c) == Just (Bool True)
        when twice $ void (resolveWorkflowRequest li cmd)
        fmap workflowView <$> resolveWorkflowRequest li cmd

  describe "chat-plans.json" $ do
    (file, cases) <- runIO (loadFile (golden "chat-plans.json"))
    forM_ cases $ \c -> it (T.unpack (textField "name" c)) $
      runCase file c $ \li -> fmap chatView <$> resolveChatRequest li (textField "command" c)

  describe "prewarm.json" $ do
    (file, cases) <- runIO (loadFile (golden "prewarm.json"))
    forM_ cases $ \c -> it (T.unpack (textField "name" c)) $ withRoot $ \root -> do
      base <- setupCase root (field "extras" c)
      let li = LaunchInputs (machineFor file (root </> "gauss-home")) (object []) base False (const (pure Nothing)) Nothing
      snapBefore <- snapshot root
      result <- withProcessEnv base (prepareManagedRuntimeAssets li)
      snapAfter <- snapshot root
      case (result, subObject "result" c, subObject "error" c) of
        (Right kvs, Just expected, _) ->
          Object (KM.fromList [(K.fromText k, String (normText root v)) | (k, v) <- kvs]) `shouldBe` expected
        (Left e, _, Just expected) -> errorView root e `shouldBe` expected
        (r, _, _) -> expectationFailure ("outcome differs from upstream: " <> either show (const "ok") r)
      normalize root (changes snapBefore snapAfter) `shouldBe` field "files" c
      logged <- doesFileExist (root </> "fake.log")
      calls <- if logged then T.lines <$> TIO.readFile (root </> "fake.log") else pure []
      map (normText root) calls `shouldBe` (field "calls" c :: [Text])

  describe "forgiving.json" $ do
    cases <- runIO (goldenCases (golden "forgiving.json"))
    forM_ cases $ \c -> it (show (textField "input" c)) $
      rewriteForgivingCommand (textField "input" c) `shouldBe` (field "output" c :: Maybe Text)
