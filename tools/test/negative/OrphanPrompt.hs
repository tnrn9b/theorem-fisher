{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- Must NOT compile: 'Promptable' is sealed (its superclass is not
-- exported), so an instance for the judge written outside "Fisher.Role"
-- has no superclass instance.
module OrphanPrompt where

import Fisher.Role (Promptable (..), Role (..), saidText, static)

instance Promptable 'Judge where
  toPrompt = static . saidText
