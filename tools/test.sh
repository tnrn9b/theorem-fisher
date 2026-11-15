#!/bin/zsh
# Build every test suite, then run each under a time limit (default 120 s)
# from its package directory, so a hung test cannot block the build
# watcher. Output goes to .build/test-<package>.txt; the summary line of
# each is printed.
#   tools/test.sh [package ...]
cd "$(dirname "$0")/.." || exit 1
limit="${FISHER_TEST_LIMIT:-120}"
pkgs=("$@")
[[ ${#pkgs} -eq 0 ]] && pkgs=(fisher-core fisher-workflow fisher-swarm fisher-llm fisher-tools fisher-mcp fisher-agent fisher-cli fisher-lean fisher-search)
stack build --test --no-run-tests "${pkgs[@]}" > .build/test-build.txt 2>&1 || { tail -40 .build/test-build.txt; exit 1; }
dist="$(stack path --dist-dir)"
rc=0
for p in "${pkgs[@]}"; do
  bin="$p/$dist/build/$p-test/$p-test"
  ( cd "$p" && ../tools/timeout.sh "$limit" "${bin#$p/}" ) > ".build/test-$p.txt" 2>&1
  code=$?
  summary="$(grep -E 'examples?, [0-9]+ failures?' ".build/test-$p.txt" | tail -1)"
  [[ $code -eq 142 ]] && summary="TIMED OUT after ${limit}s ($(grep -c '✔' ".build/test-$p.txt") passed before)"
  echo "$p: ${summary:-no summary} (exit $code)"
  [[ $code -ne 0 ]] && rc=1
done
tools/test/typecheck-negative.sh > .build/test-negative.txt 2>&1
ncode=$?
cat .build/test-negative.txt
[[ $ncode -ne 0 ]] && rc=1
exit $rc
