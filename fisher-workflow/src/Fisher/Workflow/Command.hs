-- | The managed workflow commands: @/prove@ and friends, their aliases, and
-- the forgiving rewrite of @prove ...@ into @/prove ...@.
module Fisher.Workflow.Command
  ( WorkflowSpec (..)
  , workflowUsage
  , parseWorkflowCommand
  , rewriteForgivingCommand
  ) where

import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Workflow.Types

data WorkflowSpec = WorkflowSpec
  { wsKind :: Text
  , wsFrontend :: Text
  , wsCanonical :: Text
  , wsBackend :: Text
  , wsArgs :: Text
  }
  deriving (Eq, Show)

workflowUsage :: Text
workflowUsage =
  "Usage: /prove [scope or flags] | /draft [topic or flags] | "
    <> "/review [target or flags] | /checkpoint [target or flags] | "
    <> "/refactor [target or flags] | /golf [target or flags] | "
    <> "/autoprove [scope or flags] | /formalize [topic or flags] | "
    <> "/autoformalize [topic or flags]"

aliases :: [(Text, (Text, Text, Text))]
aliases =
  [ ("/prove", ("prove", "/prove", "/lean4:prove"))
  , ("/draft", ("draft", "/draft", "/lean4:draft"))
  , ("/review", ("review", "/review", "/lean4:review"))
  , ("/checkpoint", ("checkpoint", "/checkpoint", "/lean4:checkpoint"))
  , ("/refactor", ("refactor", "/refactor", "/lean4:refactor"))
  , ("/golf", ("golf", "/golf", "/lean4:golf"))
  , ("/autoprove", ("autoprove", "/autoprove", "/lean4:autoprove"))
  , ("/auto-proof", ("autoprove", "/autoprove", "/lean4:autoprove"))
  , ("/auto_proof", ("autoprove", "/autoprove", "/lean4:autoprove"))
  , ("/formalize", ("formalize", "/formalize", "/lean4:formalize"))
  , ("/autoformalize", ("autoformalize", "/autoformalize", "/lean4:autoformalize"))
  , ("/auto-formalize", ("autoformalize", "/autoformalize", "/lean4:autoformalize"))
  , ("/auto_formalize", ("autoformalize", "/autoformalize", "/lean4:autoformalize")) ]

forgiving :: [(Text, Text)]
forgiving =
  [ ("prove", "/prove"), ("draft", "/draft"), ("review", "/review"), ("checkpoint", "/checkpoint")
  , ("refactor", "/refactor"), ("golf", "/golf"), ("autoprove", "/autoprove"), ("auto-proof", "/autoprove")
  , ("formalize", "/formalize"), ("autoformalize", "/autoformalize"), ("auto-formalize", "/autoformalize") ]

-- | @text.split(maxsplit=1)@ on stripped text: the first word and the
-- stripped rest.
splitFirst :: Text -> (Text, Text)
splitFirst t = let (a, b) = T.break isSpace t in (a, T.strip b)

parseWorkflowCommand :: Text -> Either WorkflowError WorkflowSpec
parseWorkflowCommand raw = do
  let text = T.strip raw
  if not ("/" `T.isPrefixOf` text) then Left (UsageError workflowUsage) else pure ()
  let (name0, args) = splitFirst text
      name1 = T.toLower (T.strip name0)
      name = if name1 == "/handoff" then "/autoformalize" else name1
  case lookup name aliases of
    Nothing -> Left (UsageError workflowUsage)
    Just (kind, canonical, backend) ->
      Right WorkflowSpec
        { wsKind = kind
        , wsFrontend = name
        , wsCanonical = canonical
        , wsBackend = if T.null args then backend else backend <> " " <> args
        , wsArgs = args
        }

rewriteForgivingCommand :: Text -> Maybe Text
rewriteForgivingCommand raw = do
  let text = T.strip raw
  if T.null text || "/" `T.isPrefixOf` text then Nothing else pure ()
  let (name0, rest) = splitFirst text
      name = T.replace "_" "-" (T.toLower (T.strip name0))
  canonical <- lookup name forgiving
  pure (if T.null rest then canonical else canonical <> " " <> rest)
