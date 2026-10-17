-- | Whole CLM runs over the agent loop against test/golden/clm/scenarios.json
-- (tools/clm/cases.py; docs/clm.md sections 6 to 10).
--
-- Each scenario scripts the model (its answers, in order) and the tools
-- (each call's result, and for a call that edits the mirror the exact
-- text it leaves there). The harness (Fisher.Agent.ClmHarness, written
-- in CLM2) builds the agent with the CLM strategy, a scripted transport,
-- a scripted tool runner and the mirror in memory at the scenario's path,
-- and reports what the reference reports: per model call, the messages
-- after the pinned task, the mirror text written and read back, the
-- context line and the flags (nudge, edit, rollback, free, stop,
-- memory_flush, moved_into_live, note_replaced), and the first message of
-- the request after the system prompt (the pinned task, which must never
-- change); then the result, the final context with ids, the id counter,
-- the backup's text and the prompt section.
module Fisher.Agent.ClmScenarioSpec (spec) where

import Control.Monad (forM_, when)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Fisher.Agent.ClmHarness (runScenario)
import Fisher.Agent.TestUtil
import Fisher.Py.Value

flags :: [Text]
flags = ["nudge", "edit", "rollback", "free", "stop", "memory_flush", "moved_into_live", "note_replaced"]

spec :: Spec
spec = do
  cases <- runIO (loadCases "test/golden/clm/scenarios.json")
  describe "CLM scenarios (scenarios.json)" $
    forM_ cases $ \sc -> it (show (txt "name" sc) <> ": " <> show (txt "about" sc)) $ do
      got <- runScenario sc
      let wantSteps = list "steps" sc
          gotSteps = list "steps" got
      when (length gotSteps /= length wantSteps) $
        fail ("model calls: " <> show (length gotSteps) <> ", want " <> show (length wantSteps))
      forM_ (zip3 [1 :: Int ..] gotSteps wantSteps) $ \(i, g, w) -> do
        let at k = "call " <> show i <> " " <> k
        shouldMatch (at "pinned task") (field "task_message" g) (PDict [("role", PStr "user"), ("content", field "task" sc)])
        when ("<<< turn" `T.isInfixOf` txt "task" sc || T.isInfixOf (txt "task" sc) (txt "mirror_write" g)) $
          fail (at "the task is in the mirror")
        shouldMatch (at "context") (field "context" g) (field "context" w)
        shouldMatch (at "mirror_write") (field "mirror_write" g) (field "mirror_write" w)
        shouldMatch (at "mirror_read") (field "mirror_read" g) (field "mirror_read" w)
        shouldMatch (at "context_line") (field "context_line" g) (field "context_line" w)
        forM_ flags $ \f -> shouldMatch (at (T.unpack f)) (field f g) (field f w)
      shouldMatch "result" (field "result" got) (field "result" sc)
      shouldMatch "final_context" (field "final_context" got) (field "final_context" sc)
      shouldMatch "counter" (field "counter" got) (field "counter" sc)
      shouldMatch "backup" (field "backup" got) (field "backup" sc)
      shouldMatch "prompt_section" (field "prompt_section" got) (field "prompt_section" sc)
