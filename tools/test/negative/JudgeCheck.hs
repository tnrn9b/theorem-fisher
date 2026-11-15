{-# LANGUAGE DataKinds #-}

-- Must NOT compile: the judge has no 'CanCheck' instance, so a job in
-- 'AgentM' 'Judge cannot run the kernel check.
module JudgeCheck where

import Fisher.Role (Role (..))
import Fisher.Role.Agent (AgentM, check)
import Fisher.Search.Check (Probe)

bad :: AgentM 'Judge Probe
bad = check "theorem t : True := trivial" []
