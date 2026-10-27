import reference as R, scenarios as S, sys
for sc in S.SCENARIOS:
    if len(sys.argv)>1 and sc["name"] not in sys.argv[1:]: continue
    r=R.run(sc)
    print("==",sc["name"], r["result"])
    for s in r["steps"]:
        print("  ", s["call"], {k:v for k,v in s.items() if k in("nudge","edit","rollback","free","stop","memory_flush")}, s.get("context_line"))
    for c in sc["check"]:
        try: ok,msg=c(r)
        except Exception as e: ok,msg=False,repr(e)
        if not ok: print("   FAIL", msg)
