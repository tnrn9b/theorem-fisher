#!/usr/bin/env bash
# Set up the reference RLM implementation (github.com/alexzhang13/rlm, MIT) in
# a venv under .build/rlm-ref, pinned to the commit Fisher's port follows.
# Works under bash or zsh. Run from anywhere; the repo root is two levels above
# this script (Fisher need not be a git repo). DRY_RUN=1 prints the commands.
set -euo pipefail

PIN=d04208afbad29ca675ab13478c40ee8bebc84bfe
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
VENV="$ROOT/.build/rlm-ref"
SRC="$VENV/src"

run() { if [ -n "${DRY_RUN:-}" ]; then echo "+ $*"; else "$@"; fi; }

echo "root: $ROOT"
run mkdir -p "$ROOT/.build"
[ -x "$VENV/bin/python" ] || run python3 -m venv "$VENV"
[ -d "$SRC/.git" ] || run git clone https://github.com/alexzhang13/rlm "$SRC"
run git -C "$SRC" checkout --quiet "$PIN"
run "$VENV/bin/python" -m pip install --quiet --upgrade pip
run "$VENV/bin/python" -m pip install --quiet -e "$SRC"

run "$VENV/bin/python" -c "import rlm; print('rlm import ok:', rlm.__file__)"
[ -n "${DRY_RUN:-}" ] || echo "reference at $(git -C "$SRC" rev-parse HEAD)"
