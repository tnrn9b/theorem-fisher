{-# LANGUAGE DataKinds #-}

-- Must NOT compile: a judge's reply has no 'Promptable' instance, so it
-- cannot be put into a prompt with 'toPrompt'.
module JudgePrompt where

import Fisher.Role (Promptable (..), PromptPart, Role (..), Said)

bad :: Said 'Judge -> PromptPart
bad x = toPrompt x
