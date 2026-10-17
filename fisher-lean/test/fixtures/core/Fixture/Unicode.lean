-- 𝔽 and 𝕜 are outside the BMP: their UTF-16 columns differ from code points
def 𝔽 : Nat := 1

theorem 𝔽_eq : 𝔽 = 1 := by
  rfl

theorem α_β (α β : Prop) (hα : α) (hβ : β) : α ∧ β := by
  exact ⟨hα, hβ⟩

def 𝕜x (n : Nat) : Nat := n + 𝔽

theorem 𝕜x_one : 𝕜x 0 = 1 := by decide
