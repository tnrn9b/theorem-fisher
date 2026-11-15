{-# LANGUAGE DataKinds #-}

-- | The constructors of 'Said' and 'PromptPart'. Every module that imports
-- this one, and what it takes:
--
-- * "Fisher.Role": 'said'' (its @worker@), the 'PromptPart' constructor
--   (its 'Fisher.Role.toPrompt' instances), 'untilStable' (the lessons
--   gate's redaction).
-- * "Fisher.Role.Part": the 'PromptPart' constructor (the constructors of
--   each provenance; its own module because "Fisher.Lessons.File" imports
--   it and "Fisher.Role" imports "Fisher.Lessons.File").
-- * "Fisher.Role.Agent": 'said'' (its @ask@, and its @excerpt@ and
--   @oneLineOf@ of a reply) and the 'PromptPart' constructor (@askTwice@
--   puts a role's refused reply into the same role's second call).
-- * "Fisher.Learn.Judge": 'said'' (its @parseVerdict@).
-- * Tests: "Fisher.RoleSpec" ('said'', 'untilStable'),
--   "Fisher.Learn.JudgeSpec", "Fisher.Search.GraphRunSpec",
--   "Fisher.Search.ReviewSpec" and "Fisher.Search.SplitSpec" ('said'', to
--   make a reply without a model call).
--
-- Nothing re-exports 'Said' with its constructor, 'said'' or the
-- 'PromptPart' constructor, and none of the modules above exports a way
-- to make either from arbitrary text, so elsewhere a @Said r@ can only be
-- received and a 'PromptPart' only made by a constructor that names its
-- provenance. A new importer is a review item and goes in this list.
--
-- 'Role' is defined here because 'Said' is indexed by it; "Fisher.Role"
-- re-exports it.
module Fisher.Role.Internal
  ( Role (..)
  , Said (..)
  , said'
  , PromptPart (..)
  , untilStable
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

-- | @f@ applied until the text stops changing, at most @n@ times: the
-- stable text, or 'Nothing' when one more application would still change
-- it (fail closed: a text that is still changing is not trusted).
untilStable :: Int -> (Text -> Text) -> Text -> Maybe Text
untilStable n f t
  | t' == t = Just t
  | n <= 0 = Nothing
  | otherwise = untilStable (n - 1) f t'
  where
    t' = f t
