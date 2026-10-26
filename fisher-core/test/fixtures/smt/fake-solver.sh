#!/bin/bash
# A stand-in for z3 and cvc5 in SmtSpec; FAKE_SMT_MODE picks the behaviour.
case "${FAKE_SMT_MODE:-sat}" in
  sat)
    echo sat
    echo '(model'
    echo '  (define-fun x () Int (- 3))'
    echo '  (define-fun b () Bool true)'
    echo '  (define-fun v () (_ BitVec 8) #x1f)'
    echo '  (define-fun f ((a!1 Int)) Int 0)'
    echo ')'
    ;;
  unsat) echo unsat ;;
  unknown) echo unknown ;;
  garbage) echo hello; echo boom >&2; exit 1 ;;
  big)
    echo sat
    echo '(model'
    for i in $(seq 1 3000); do echo "  (define-fun constant_number_$i () Int 1234567)"; done
    echo ')'
    ;;
  hang) echo sat; sleep 30 ;;
  pwd) echo "cwd=$(pwd) file=${!#}" >&2; echo nothing; exit 1 ;;
esac
