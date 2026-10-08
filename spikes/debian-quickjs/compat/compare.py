"""Compare the two engine runs and built-in inventories.

    python3 compare.py <system.jsonl> <ng.jsonl> <probe_system.txt> <probe_ng.txt> <plugins-js-dir>

Prints per-plugin differences, the built-ins only one engine has, and which of
the ng-only built-ins the dumped plugin JS appears to use.
"""
import collections
import json
import pathlib
import re
import sys

sys_rows, ng_rows, probe_sys, probe_ng, js_dir = sys.argv[1:6]


def load(path):
    return {r["plugin"]: r for r in map(json.loads, open(path))}


a, b = load(sys_rows), load(ng_rows)


def summary(rows):
    st = collections.Counter(h["status"] for r in rows.values() for h in r["handlers"])
    return (
        f"load ok {sum(r['load'] is None for r in rows.values())}/{len(rows)}, "
        f"host calls {sum(r['host_calls'] for r in rows.values())}, handlers {dict(sorted(st.items()))}"
    )


print("system QuickJS:", summary(a))
print("quickjs-ng:    ", summary(b))
diffs = 0
for name in sorted(set(a) | set(b)):
    ra, rb = a.get(name), b.get(name)
    if ra is None or rb is None:
        print(f"  {name}: only in one run")
        diffs += 1
        continue
    ha = [(h["name"], h["status"], h.get("error")) for h in ra["handlers"]]
    hb = [(h["name"], h["status"], h.get("error")) for h in rb["handlers"]]
    keys = ("load", "fire", "host_calls")
    if any(ra[k] != rb[k] for k in keys) or ha != hb:
        diffs += 1
        print(f"  {name}:")
        for k in keys:
            if ra[k] != rb[k]:
                print(f"    {k}: system={ra[k]!r} ng={rb[k]!r}")
        for x, y in zip(ha, hb):
            if x != y:
                print(f"    handler system={x} ng={y}")
print(f"plugins whose results differ: {diffs}")

ps = set(open(probe_sys).read().split())
pn = set(open(probe_ng).read().split())
only_ng = sorted(pn - ps)
only_sys = sorted(ps - pn)


def features(paths, other):
    """Collapse property paths into features: a new global counts once, and a
    function's own `length`/`name` are not separate features."""
    out = set()
    for p in paths:
        parts = p.split(".")
        if parts[0] not in other:
            out.add(parts[0])
        elif parts[-1] in ("length", "name") and ".".join(parts[:-1]) in paths:
            continue
        else:
            out.add(p)
    return sorted(out)


f_ng, f_sys = features(set(only_ng), ps), features(set(only_sys), pn)
print(f"\nbuilt-in features only in quickjs-ng ({len(f_ng)}):")
print("  " + " ".join(f_ng))
print(f"built-in features only in system QuickJS ({len(f_sys)}):")
print("  " + " ".join(f_sys))

js = "\n".join(p.read_text() for p in pathlib.Path(js_dir).glob("*.js"))
used = []
for path in only_ng:
    leaf = path.split(".")[-1]
    if leaf.startswith("[") or leaf in ("length", "name", "prototype", "constructor"):
        continue
    top = path.split(".")[0]
    pat = rf"\b{re.escape(top)}\b" if "." not in path else rf"\.{re.escape(leaf)}\b"
    if re.search(pat, js):
        used.append(path)
print(f"\nng-only built-ins whose name appears in the plugin JS ({len(used)}; coarse name match, check each by hand):")
print("  " + " ".join(used))
