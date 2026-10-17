From Proj Require Import Base.

Axiom planted : False.

Theorem proved : 1 + 1 = two.
Proof. reflexivity. Qed.

Theorem admitted : 2 + 2 = 4.
Proof. Admitted.

Theorem uses_axiom : 1 = 2.
Proof. destruct planted. Qed.

Theorem aborted : 3 = 3.
Proof. Abort.
