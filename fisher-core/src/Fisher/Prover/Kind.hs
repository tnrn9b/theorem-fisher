-- | The kinds of prover a Fisher project can hold. Kept apart from
-- "Fisher.Prover" so that "Fisher.Prover.Holes" can take a kind while the
-- 'Fisher.Prover.Prover' record holds the hole scanner.
module Fisher.Prover.Kind (ProverKind (..)) where

data ProverKind = Lean4 | Rocq | Isabelle
  deriving (Eq, Show, Enum, Bounded)
