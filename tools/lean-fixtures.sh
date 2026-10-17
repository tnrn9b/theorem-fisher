#!/bin/zsh
# Build the Lean fixtures C1's recordings and live specs run against, one
# copy per pinned toolchain, under ~/.cache/fisher/lean-fixtures/<version>/:
#   core     fisher-lean/test/fixtures/core (Lean only)
#   mathlib  fisher-lean/test/fixtures/mathlib, requiring Mathlib at the tag
#            matching the toolchain (prebuilt oleans from `lake exe cache get`)
# Re-running rebuilds core, refreshes the mathlib copy's sources and keeps
# its downloaded packages.
cd "$(dirname "$0")/.." || exit 1
src=$PWD/fisher-lean/test/fixtures
root=${FISHER_LEAN_FIXTURES:-$HOME/.cache/fisher/lean-fixtures}
versions=(${=FISHER_LEAN_TOOLCHAINS:-4.33.0 4.33.1})
for v in $versions; do
  d=$root/$v
  mkdir -p $d
  rm -rf $d/core && cp -R $src/core $d/core
  print "leanprover/lean4:v$v" > $d/core/lean-toolchain
  (cd $d/core && lake build 2>&1 | tail -3)
  [[ -f $d/core/.lake/build/lib/lean/Fixture.olean ]] || { print "core fixture did not build in $d"; exit 1; }
  if [[ ! -f $d/mathlib/.lake/build/lib/lean/MathFixture.olean ]]; then
    mkdir -p $d/mathlib
    cp -R $src/mathlib/MathFixture $src/mathlib/MathFixture.lean $d/mathlib/
    sed "s/@MATHLIB_REV@/v$v/" $src/mathlib/lakefile.toml.in > $d/mathlib/lakefile.toml
    print "leanprover/lean4:v$v" > $d/mathlib/lean-toolchain
    (cd $d/mathlib && lake update 2>&1 | tail -2 && lake exe cache get 2>&1 | tail -2)
  fi
  cp $src/mathlib/MathFixture/*.lean $d/mathlib/MathFixture/
  (cd $d/mathlib && lake build MathFixture 2>&1 | tail -3)
  [[ -f $d/mathlib/.lake/build/lib/lean/MathFixture.olean ]] || { print "mathlib fixture did not build in $d"; exit 1; }
  print "built $d"
done
