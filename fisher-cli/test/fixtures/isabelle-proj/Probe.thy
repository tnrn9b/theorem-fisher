theory Probe imports Main begin
lemma good: "x + (0::nat) = x" by simp
lemma bad: "x = (x::nat)" sorry
lemma ugly: "True" oops
end
