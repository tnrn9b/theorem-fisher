-- | The gate on an edit of the mirror (docs/clm.md section 7).
module Fisher.Agent.Context.EditGate
  ( GateMode (..)
  , accept
  ) where

import Fisher.Agent.Context.Mirror (tokens)
import Fisher.Py.Value (PyVal)

data GateMode = Fit | Shrink
  deriving (Eq, Show)

-- | The limit, the editable turns before and after, and the live turn in
-- its accounting form.
accept :: GateMode -> Integer -> [PyVal] -> [PyVal] -> [PyVal] -> Bool
accept mode limit before after live =
  let shrinks = tokens after < tokens before
   in case mode of
        Shrink -> shrinks
        Fit -> tokens (after <> live) <= limit || shrinks
