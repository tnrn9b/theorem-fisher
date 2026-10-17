import Mathlib.Data.Nat.Basic

/-! # Basic definitions

theorem fake_in_comment : False := sorry
-/

namespace Proj

/-- A counter. -/
structure Counter where
  value : Nat
  deriving Repr

inductive Color where
  | red
  | green

inductive Tree : Type
  | leaf : Tree
  | node : Tree → Tree → Tree

class Sized (α : Type) where
  size : α → Nat

def bump (c : Counter) : Counter := ⟨c.value + 1⟩

def greeting : String := "hi /- not a comment"

abbrev Count := Nat

instance instSizedCounter : Sized Counter where
  size c := c.value

axiom magic : ∀ n : Nat, n = n

noncomputable opaque secret : Nat

section Lemmas
variable (c : Counter)

@[simp] theorem bump_value : (bump c).value = c.value + 1 := rfl

end Lemmas

namespace Inner

lemma bump_pos (c : Counter) :
    0 < (bump c).value := by
  simp [bump]

end Inner

private theorem hidden_helper : True := trivial

def double : Nat → Nat
  | 0 => 0
  | n + 1 => double n + 2

@[simp]
protected theorem Counter.value_mk (n : Nat) : (Counter.mk n).value = n := rfl

nonrec def nonrec_id (n : Nat) : Nat := n

partial def spin (n : Nat) : Nat := spin n

theorem _root_.top_level_fact : 1 = 1 := rfl

end Proj

instance : Inhabited Proj.Counter := ⟨⟨0⟩⟩
