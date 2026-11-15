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
  , oneLinePart
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
import Fisher.Role.Internal (PromptPart (..), Role (..), Said (..), said')
import Fisher.Role.Part (oneLinePart, partText, parts, source, static, tool, user)

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
candidate = checked . saidText

-- | A rule lesson rendered from the lesson templates, checked as a reply
-- is ('candidate'), so every 'Candidate' has passed the gate.
template :: Text -> Maybe Candidate
template = checked

-- The gate: redacted to a fixpoint, then sanitised; 'Nothing' when the
-- sanitiser refuses it or nothing is left.
checked :: Text -> Maybe Candidate
checked raw
  | T.null (T.strip redacted) = Nothing
  | otherwise = Candidate <$> sanitiseLesson redacted
  where
    redacted = redactFixpoint raw

-- 'redactSensitiveText' is not idempotent: its env-assignment and JSON
-- field patterns match their own masks again (@TOKEN=abcdef...0123@, a
-- long token's mask, is itself a short token and becomes @TOKEN=***@), so
-- one pass can leave a token's first 6 and last 4 characters in a lesson.
-- Redact until the text stops changing, at most 3 passes.
redactFixpoint :: Text -> Text
redactFixpoint = go (3 :: Int)
  where
    go n t
      | n <= 0 = t
      | otherwise = let t' = redactSensitiveText t in if t' == t then t else go (n - 1) t'

candidateText :: Candidate -> Text
candidateText (Candidate t) = t
