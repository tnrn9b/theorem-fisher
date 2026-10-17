import MathFixture.Real

theorem needs_hint (a b : ℝ) (h : 0 < a) (hb : 0 < b) : 0 < a * b := by
  exact?

theorem broken (x : ℝ) : x ^ 2 < 0 := by
  positivity
