{-# LANGUAGE DataKinds #-}

-- Only Fisher.Role.Agent and Fisher.Learn.Judge.parseVerdict may import said' (besides Fisher.Role itself, for worker).

-- | The constructor of 'Said'. Only "Fisher.Role.Agent" (its @ask@),
-- "Fisher.Learn.Judge" (its @parseVerdict@) and "Fisher.Role" (its
-- @worker@) import this module; nothing re-exports 'Said' with its
-- constructor or 'said'', so elsewhere a @Said r@ can only be received,
-- never made.
--
-- 'Role' is defined here because 'Said' is indexed by it; "Fisher.Role"
-- re-exports it, and it is the one thing from here that leaves.
module Fisher.Role.Internal
  ( Role (..)
  , Said (..)
  , said'
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
