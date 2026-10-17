import Mathlib.Tactic
import Proj.Basic

open Proj

theorem counter_open (c : Counter) : -- not Proj.secret
    bump c = bump c := rfl

private theorem twin : True := trivial

theorem sees_private : Proj.hidden_helper = Proj.hidden_helper := rfl

namespace Proj.Extra

def base : Nat := 3

theorem base_eq : base = 3 := rfl

/-- Through the namespace and dotted names. -/
theorem dotted (c : Counter) : Color.red = Color.red ∧ 0 < (bump c).value :=
  ⟨rfl, Inner.bump_pos c⟩

theorem uses_inner (c : Counter) : Inner.bump_pos c = Inner.bump_pos c := rfl

end Proj.Extra
