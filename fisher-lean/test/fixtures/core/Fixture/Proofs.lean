import Fixture.Basic

open Fixture

theorem add_zero' (n : Nat) : n + 0 = n := by
  induction n with
  | zero => rfl
  | succ k ih => simp

theorem swap_swap (p : Point) : p.swap.swap = p := by
  cases p
  rfl

theorem double_two : double 2 = 4 := by
  decide

theorem and_swap (a b : Prop) (h : a ∧ b) : b ∧ a := by
  constructor
  · exact h.2
  · exact h.1

theorem uses_hyps (a b c : Nat) (h₁ : a = b) (h₂ : b = c) (h₃ : c = c) : a = c := by
  rw [h₁, h₂]

example (n : Nat) : double n = n + n := rfl

theorem long_proof (n : Nat) : double (double n) = 4 * n := by
  simp only [double]
  omega
