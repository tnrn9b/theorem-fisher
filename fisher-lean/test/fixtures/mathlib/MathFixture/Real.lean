import Mathlib.Data.Real.Basic
import Mathlib.Tactic.Linarith
import Mathlib.Tactic.Positivity

example (a b : ℝ) (h : a < b) : a ≤ b := by
  linarith

theorem sq_nonneg_ex (x : ℝ) : 0 ≤ x ^ 2 := by
  positivity

theorem le_trans_ex (a b c : ℝ) (h₁ : a ≤ b) (h₂ : b ≤ c) : a ≤ c := by
  exact le_trans h₁ h₂
