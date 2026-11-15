{-# LANGUAGE DataKinds #-}

-- Only Fisher.Role.Agent and Fisher.Learn.Judge.parseVerdict may import said' (besides Fisher.Role itself, for worker).
-- Only Fisher.Role.Part and Fisher.Role (for toPrompt) may import the PromptPart constructor.

-- | The constructor of 'Said'. Only "Fisher.Role.Agent" (its @ask@),
-- "Fisher.Learn.Judge" (its @parseVerdict@) and "Fisher.Role" (its
-- @worker@) import this module; nothing re-exports 'Said' with its
-- constructor or 'said'', so elsewhere a @Said r@ can only be received,
-- never made.
--
-- 'Role' is defined here because 'Said' is indexed by it; "Fisher.Role"
-- re-exports it, and it is the one thing from here that leaves.
--
-- The constructor of 'PromptPart' is here too, so that "Fisher.Role.Part"
-- (which "Fisher.Lessons.File" imports, and which "Fisher.Role" cannot be,
-- since it imports "Fisher.Lessons.File") and "Fisher.Role" (its
-- 'Fisher.Role.toPrompt' instances) can both make one; neither exports it.
module Fisher.Role.Internal
  ( Role (..)
  , Said (..)
  , said'
  , PromptPart (..)
  ) where

import Data.Text (Text)

-- | Who speaks in a run. The run log spells them with
-- 'Fisher.Role.roleName'.
data Role = Worker | Planner | Verifier | Judge deriving (Eq, Ord, Show, Enum, Bounded)

-- | Text a model in role @r@ replied with.
newtype Said (r :: Role) = Said {saidText :: Text} deriving (Eq, Show)

-- | Make a 'Said' from a reply.
said' :: Text -> Said r
said' = Said

-- | A piece of a prompt. "Fisher.Role.Part" exports it without its
-- constructor.
newtype PromptPart = PromptPart Text deriving (Eq, Show)

-- | Two parts one after the other, nothing between them: a part assembled
-- from parts each made by a constructor that names its provenance.
instance Semigroup PromptPart where
  PromptPart a <> PromptPart b = PromptPart (a <> b)

instance Monoid PromptPart where
  mempty = PromptPart mempty
