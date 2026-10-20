#!/bin/zsh
# CLM4 after the follow-ups (claude/why-clm-underperforms.md, "Next"): CLM
# with its new defaults (every batch with a write in order, text calls sent
# back, rule 3 off, the built-in guidance), three long sessions at 32K (E)
# and at 16K (F) on Sonnet 5.5. The compressor's numbers are run6-five's
# A and C (unchanged code for it).
#
#   tools/bench/clm4/follow.sh [E F]
cd ~/IdeaProjects/Fisher || exit 1
export FISHER="$(stack exec -- which fisher)"
B=~/.cache/fisher/bench/clm4/run7-follow
M=anthropic/claude-sonnet-5.5
conds=(${@:-E F})
for k in $conds; do
  case $k in
    E) extra=();;
    F) extra=(--budget 16000);;
    *) echo "unknown condition $k"; exit 2;;
  esac
  for i in 1 2 3; do
    O=$B/$k/rep$i
    mkdir -p $O
    [[ -f $O/long-clm/judge.json ]] && continue
    print "== $k rep$i clm"
    python3 tools/bench/clm4/long.py --context clm --out $O --cap-usd 15 --model $M $extra || exit 1
  done
done
python3 - <<'PY'
import json, pathlib
B = pathlib.Path('~/.cache/fisher/bench/clm4/run7-follow').expanduser()
out = {}
for d in sorted(B.glob('*/rep*/long-clm')):
    u = json.loads((d / 'usage.json').read_text())
    j = json.loads((d / 'judge.json').read_text()) if (d / 'judge.json').exists() else {}
    us = u.get('usage') or {}
    out.setdefault(d.parent.parent.name, []).append({
        'rep': d.parent.name, 'closed': j.get('closed'), 'cost': u.get('cost_usd'), 'calls': u.get('turns'),
        'wall': u.get('wall_s'), 'error': u.get('harness_error'), 'compactions': us.get('compactions'),
        'applied': us.get('edits_applied'), 'rollbacks': us.get('rollbacks'), 'rereads': us.get('rereads'),
        'refusals': us.get('refusals')})
summary = {k: {'closed': [r['closed'] for r in rs], 'total_closed': sum(r['closed'] or 0 for r in rs),
               'cost': [r['cost'] for r in rs], 'total_cost': round(sum(r['cost'] or 0 for r in rs), 2)}
           for k, rs in out.items()}
(B / 'summary.json').write_text(json.dumps({'rows': out, 'summary': summary}, indent=1))
print(json.dumps(summary, indent=1))
PY
