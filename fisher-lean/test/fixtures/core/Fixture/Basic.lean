namespace Fixture

/-- The double of a natural number. -/
def double (n : Nat) : Nat := n + n

theorem double_eq (n : Nat) : double n = 2 * n := by
  unfold double
  omega

/-- A point in the plane. -/
structure Point where
  x : Nat
  y : Nat
deriving Repr

def Point.swap (p : Point) : Point := ⟨p.y, p.x⟩

namespace Inner

def triple (n : Nat) : Nat := 3 * n

end Inner

instance : Inhabited Point := ⟨⟨0, 0⟩⟩

end Fixture
