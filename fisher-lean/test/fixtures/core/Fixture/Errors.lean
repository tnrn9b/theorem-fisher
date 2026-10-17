import Fixture.Basic

open Fixture

theorem bad (n : Nat) : n + 1 = n := by
  simp

theorem unfinished (a b : Nat) : a + b = b + a := by
  sorry

def unused (x y : Nat) : Nat := x

#eval double 21

#check double_eq

theorem needs_search (n : Nat) : n * 1 = n := by
  exact?

theorem needs_simp (n : Nat) : n + 0 = n := by
  simp?

theorem wrong_type : (2 : Nat) = "two" := by
  rfl
