-- | The prompt part type and its constructors, one per provenance. Its own
-- module so that "Fisher.Lessons.File" can build the lessons block from
-- parts ("Fisher.Role" imports "Fisher.Lessons.File" for the sanitiser).
-- "Fisher.Role" re-exports all of it.
module Fisher.Role.Part
  ( PromptPart
  , partText
  , static
  , user
  , source
  , tool
  , parts
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Fisher.Role.Internal (PromptPart (..))

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

