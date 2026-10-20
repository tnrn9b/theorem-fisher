#!/bin/zsh
# CLM4, the five fixes (claude/why-clm-underperforms.md): three long
# sessions per condition on Sonnet 5.5, each repeat its own directory, CLM
# first (its first prompt sets the compressor's threshold).
#
#   A  fixes 1 and 2 (a write and a shell call run in order; a refusal is
#      an error), both strategies, 32K
#   B  A plus fisher.clm.keep_unread false, CLM only
#   D  A plus fisher.clm.skill tools/clm/skill.md, CLM only
#   C  A at a 16K budget, both strategies
#
#   tools/bench/clm4/five.sh [A B D C]
cd ~/IdeaProjects/Fisher || exit 1
export FISHER="$(stack exec -- which fisher)"
B=~/.cache/fisher/bench/clm4/run6-five
M=anthropic/claude-sonnet-5.5
conds=(${@:-A B D C})
for k in $conds; do
  case $k in
    A) arms=(clm compress); extra=();;
    B) arms=(clm); extra=(--no-keep-unread);;
    D) arms=(clm); extra=(--skill tools/clm/skill.md);;
    C) arms=(clm compress); extra=(--budget 16000);;
    *) echo "unknown condition $k"; exit 2;;
  esac
  for i in 1 2 3; do
    O=$B/$k/rep$i
    mkdir -p $O
    for c in $arms; do
      [[ -f $O/long-$c/judge.json ]] && continue
      print "== $k rep$i $c"
      python3 tools/bench/clm4/long.py --context $c --out $O --cap-usd 15 --model $M $extra || exit 1
    done
  done
done
python3 - <<'PY'
import json, pathlib, statistics
B = pathlib.Path('~/.cache/fisher/bench/clm4/run6-five').expanduser()
out = {}
for cond in sorted(p for p in B.iterdir() if p.is_dir()):
    for rep in sorted(cond.glob('rep*')):
        for d in sorted(rep.glob('long-*')):
            u = json.loads((d / 'usage.json').read_text())
            j = json.loads((d / 'judge.json').read_text()) if (d / 'judge.json').exists() else {}
            us = u.get('usage') or {}
            out.setdefault(f"{cond.name}/{d.name[5:]}", []).append({
                'rep': rep.name, 'closed': j.get('closed'), 'cost': u.get('cost_usd'), 'calls': u.get('turns'),
                'wall': u.get('wall_s'), 'result': (u.get('result') or '')[:200], 'error': u.get('harness_error'),
                'compactions': us.get('compactions'), 'applied': us.get('edits_applied'),
                'rejected': us.get('edits_rejected'), 'rollbacks': us.get('rollbacks'), 'rereads': us.get('rereads'),
                'refusals': us.get('refusals'), 'prompt': (us.get('main') or {}).get('prompt_tokens'),
                'cached': (us.get('main') or {}).get('cached_tokens')})
summary = {k: {'closed': [r['closed'] for r in rs], 'cost': [r['cost'] for r in rs],
               'total_closed': sum(r['closed'] or 0 for r in rs),
               'mean_cost': round(statistics.mean([r['cost'] for r in rs if r['cost'] is not None] or [0]), 3)}
           for k, rs in out.items()}
(B / 'summary.json').write_text(json.dumps({'rows': out, 'summary': summary}, indent=1))
print(json.dumps(summary, indent=1))
PY
