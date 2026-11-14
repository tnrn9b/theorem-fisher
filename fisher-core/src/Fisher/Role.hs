{-# LANGUAGE DataKinds #-}

-- | Provenance types. A model's reply is a @'Said' r@, tagged with the role
-- @r@ that said it; it reaches a prompt only through 'toPrompt' (no
-- instance for the judge) and lesson evidence only through 'candidate'
-- (redacted, then sanitised). Every other piece of a prompt is a
-- 'PromptPart' made by a constructor that names where its text came from.
module Fisher.Role
  ( -- * Roles
    Role (..)
  , roleName
  , KnownRole (..)
    -- * What a role said
  , Said
  , saidText
  , worker
    -- * Prompt parts
  , PromptPart
  , partText
  , static
  , user
  , source
  , tool
  , parts
  , Prompt
  , Promptable (..)
    -- * Lesson candidates
  , Candidate
  , candidate
  , template
  , candidateText
  ) where

import Data.Proxy (Proxy)
import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Lessons.File (sanitiseLesson)
import Fisher.Redact (redactSensitiveText)
import Fisher.Role.Internal (Role (..), Said (..), said')

-- | The run log's spelling of a role.
roleName :: Role -> Text
roleName = \case
  Worker -> "worker"
  Planner -> "planner"
  Verifier -> "verifier"
  Judge -> "judge"

-- | The 'Role' value of a promoted role.
class KnownRole (r :: Role) where
  roleVal :: Proxy r -> Role

instance KnownRole 'Worker where roleVal _ = Worker
instance KnownRole 'Planner where roleVal _ = Planner
instance KnownRole 'Verifier where roleVal _ = Verifier
instance KnownRole 'Judge where roleVal _ = Judge

-- | A worker's reply as a backend returned it (the search's attempt
-- results, which do not come from an agent call).
worker :: Text -> Said 'Worker
worker = said'

-- | A piece of a prompt.
newtype PromptPart = PromptPart Text deriving (Eq, Show)

partText :: PromptPart -> Text
partText (PromptPart t) = t

-- | Fisher's own text: templates, headings, the guard sentence, hints.
static :: Text -> PromptPart
static = PromptPart

-- | The user's own text: @## Yours@ lines, @--statement@, a chat message.
user :: Text -> PromptPart
user = PromptPart

-- | Project source and facts read from the project (declarations, lemmas,
-- docs).
source :: Text -> PromptPart
source = PromptPart

-- | Tool and prover output: Lean diagnostics, a counterexample, a solver's
-- model.
tool :: Text -> PromptPart
tool = PromptPart

-- | Parts joined by the brief's rule: each one's trailing whitespace
-- stripped, the blank ones dropped, two newlines between the rest.
parts :: [PromptPart] -> PromptPart
parts = PromptPart . T.intercalate "\n\n" . map T.stripEnd . filter (not . T.null . T.strip) . map partText

-- | A conversation: each message's role (@"system"@, @"user"@ or
-- @"assistant"@) and its text.
type Prompt = [(Text, PromptPart)]

-- | The roles whose replies may be put into a prompt. There is no default
-- method, so every instance has to say how it puts a reply into a prompt.
-- The 'Judge instance is deliberately absent: a judge's text reaches a
-- prompt only as a 'Candidate', through the lessons gate ('candidate',
-- which redacts and sanitises it), never directly.
class Promptable (r :: Role) where
  toPrompt :: Said r -> PromptPart

instance Promptable 'Worker where toPrompt = PromptPart . saidText
instance Promptable 'Planner where toPrompt = PromptPart . saidText
instance Promptable 'Verifier where toPrompt = PromptPart . saidText

-- | Text that may become a lesson.
newtype Candidate = Candidate Text deriving (Eq, Show)

-- | A reply as lesson evidence: redacted, then sanitised; 'Nothing' when
-- the sanitiser refuses it or nothing is left.
candidate :: Said r -> Maybe Candidate
candidate s
  | T.null (T.strip redacted) = Nothing
  | otherwise = Candidate <$> sanitiseLesson redacted
  where
    redacted = redactSensitiveText (saidText s)

-- | A rule lesson rendered from the lesson templates.
template :: Text -> Candidate
template = Candidate

candidateText :: Candidate -> Text
candidateText (Candidate t) = t
