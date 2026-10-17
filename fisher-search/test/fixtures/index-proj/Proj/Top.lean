import Proj.Use
import Mathlib.Order.Basic

theorem top_full (c : Proj.Counter) : (Proj.bump c).value = c.value + 1 :=
  Proj.bump_value c

open Proj.Extra in
theorem top_uses : base = 3 := base_eq

theorem top_no_open : base = 3 := sorry

theorem short_names (bump double : Nat) : bump + double = double + bump := Nat.add_comm _ _

open Proj.Extra in theorem same_line : base = 3 := base_eq

open Proj.Extra in
set_option maxHeartbeats 400000 in
theorem through_option : base = 3 := base_eq

set_option linter.unusedVariables false in theorem option_same_line : True := trivial

private theorem twin : True := trivial

theorem uses_twin (h : twin = twin) : True := trivial

theorem mathlib_name (n : Nat) : Nat.succ n = n + 1 := rfl

theorem long_statement : True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True ∧ True := by
  simp
