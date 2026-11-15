{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- Must NOT compile: 'CanCheck' is sealed (its superclass is not
-- exported), so an instance for the judge written outside
-- "Fisher.Role.Agent" has no superclass instance.
module OrphanCheck where

import Fisher.Role (Role (..))
import Fisher.Role.Agent (CanCheck)

instance CanCheck 'Judge
