#!/bin/zsh
# The role types are a compile-time guarantee, so test that the wrong
# programs do not compile. Each snippet in tools/test/negative must fail
# to type check with the expected missing-instance error. Exits 0 only
# when all do. Run from anywhere; uses the snapshot's package databases.
# The capability classes are sealed (a superclass that is not exported),
# so an orphan instance fails for want of the hidden 'Sealed' instance,
# in the instance declaration of the class it tried to extend.
cd "$(dirname "$0")/../.." || exit 1
inc=(-ifisher-core/src -ifisher-llm/src -ifisher-search/src)
rc=0
expect() {  # file, instance class, role, and for an orphan the class it extends
  local file="$1" cls="$2" role="$3" of="$4" out
  out="$(stack ghc -- -fno-code -XOverloadedStrings -XLambdaCase "${inc[@]}" "$file" 2>&1)"
  if [[ $? -ne 0 ]] && print -r -- "$out" | grep -Eq "No instance for .*$cls.*$role" \
     && { [[ -z "$of" ]] || print -r -- "$out" | grep -Eq "instance declaration for .*$of.*$role"; }; then
    echo "ok: $file rejected (No instance for $cls $role${of:+, in the $of instance})"
  else
    echo "FAIL: $file was not rejected for a missing $cls $role instance${of:+ in the $of instance}; got:"
    print -r -- "$out" | head -30
    rc=1
  fi
}
expect tools/test/negative/JudgePrompt.hs Promptable Judge
expect tools/test/negative/JudgeCheck.hs CanCheck Judge
expect tools/test/negative/OrphanPrompt.hs Sealed Judge Promptable
expect tools/test/negative/OrphanCheck.hs Sealed Judge CanCheck
exit $rc
