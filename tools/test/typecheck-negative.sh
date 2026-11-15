#!/bin/zsh
# The role types are a compile-time guarantee, so test that the wrong
# programs do not compile. Each snippet in tools/test/negative must fail
# to type check with the expected missing-instance error. Exits 0 only
# when both do. Run from anywhere; uses the snapshot's package databases.
cd "$(dirname "$0")/../.." || exit 1
inc=(-ifisher-core/src -ifisher-llm/src -ifisher-search/src)
rc=0
expect() {  # file, instance class, role
  local file="$1" cls="$2" role="$3" out
  out="$(stack ghc -- -fno-code -XOverloadedStrings -XLambdaCase "${inc[@]}" "$file" 2>&1)"
  if [[ $? -ne 0 ]] && print -r -- "$out" | grep -Eq "No instance for .*$cls.*$role"; then
    echo "ok: $file rejected (No instance for $cls $role)"
  else
    echo "FAIL: $file was not rejected for a missing $cls $role instance; got:"
    print -r -- "$out" | head -30
    rc=1
  fi
}
expect tools/test/negative/JudgePrompt.hs Promptable Judge
expect tools/test/negative/JudgeCheck.hs CanCheck Judge
exit $rc
