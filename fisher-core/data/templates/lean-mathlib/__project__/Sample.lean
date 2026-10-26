import Mathlib
{{#if smt}}
import Smt
{{/if}}

-- A sample theorem: the project is ready when this builds without `sorry`.
theorem fisher_sample (a b : ℕ) : a + b = b + a := by ring
{{#if smt}}

example : ∀ x : Int, x + 0 = x := by smt
{{/if}}
