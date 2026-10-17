theory Fisher_Check imports "Probe.Probe" begin
ML \<open>
local
  fun fisher_lookup ctxt names =
    (case names of
      [] => NONE
    | n :: ns => (case try (Proof_Context.get_thms ctxt) n of NONE => fisher_lookup ctxt ns | r => r))
  fun fisher_one ctxt (label, names) =
    (case fisher_lookup ctxt names of
      NONE => "FISHER_MISSING " ^ label
    | SOME thms =>
        (case Thm_Deps.all_oracles thms of
          [] => "FISHER_OK " ^ label
        | os => "FISHER_ORACLES " ^ label ^ ": " ^ commas (map (fn x => fst (fst x)) os)))
in
  fun fisher_check ctxt targets =
    let val rs = map (fisher_one ctxt) targets
    in if forall (String.isPrefix "FISHER_OK ") rs then writeln (cat_lines rs) else error (cat_lines rs) end
end
\<close>
ML \<open>fisher_check \<^context> [("good", ["Probe.good", "good"]), ("bad", ["Probe.bad", "bad"]), ("ugly", ["Probe.ugly", "ugly"]), ("nothere", ["Probe.nothere", "nothere"])]\<close>
end
