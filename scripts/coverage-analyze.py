#!/usr/bin/env python3
"""Find dead code and large uncovered chunks in an llvm-cov coverage export.

    scripts/coverage-analyze.py [REPORT_DIR]

REPORT_DIR (default target/coverage-report) must hold `export.json` and
`lcov.info`, as written by scripts/coverage-full. Writes `analysis.json` and
prints a summary. Run from the repository root.

Three lists come out of it, in decreasing order of confidence that the code
can simply be deleted:

  * never-executed functions whose name is referenced nowhere else in the
    repository (Rust, TS or JS) -- plus, transitively, functions referenced
    only from inside such functions;
  * plugin API methods (`JsEditorApi`) that no test executes, split by whether
    a bundled plugin calls them;
  * the largest runs of uncovered lines, and the files with the most
    uncovered lines -- untested, not necessarily dead.

Name matching is textual, so a function sharing its name with anything else
(`new`, `run`, a trait method) is never reported as unreferenced. That makes
the first list conservative: short on false positives, not exhaustive.
"""
import collections
import json
import os
import re
import subprocess
import sys

REPORT = sys.argv[1] if len(sys.argv) > 1 else "target/coverage-report"
ROOT = os.getcwd()
TOK = re.compile(r"[A-Za-z_][A-Za-z0-9_]*")
QJS = "crates/fresh-plugin-runtime/src/backend/quickjs_backend.rs"


def rel(p):
    return os.path.relpath(p, ROOT) if p.startswith(ROOT + "/") else None


def is_test_path(f):
    return "/tests/" in f or f.endswith("tests.rs") or "/benches/" in f


def camel(n):
    p = n.split("_")
    return p[0] + "".join(x.title() for x in p[1:])


data = json.load(open(os.path.join(REPORT, "export.json")))["data"][0]

# ---- files ---------------------------------------------------------------
files = {}
for f in data["files"]:
    r = rel(f["filename"])
    if r and not is_test_path(r):
        s = f["summary"]
        files[r] = (s["lines"]["count"], s["lines"]["covered"])

lines = collections.defaultdict(dict)
cur = None
for ln in open(os.path.join(REPORT, "lcov.info")):
    if ln.startswith("SF:"):
        cur = rel(ln[3:].strip())
    elif ln.startswith("DA:") and cur in files:
        n, c = ln[3:].split(",")[:2]
        lines[cur][int(n)] = int(c)

# Contiguous uncovered runs; a gap of up to 3 unmapped lines (blank, comment,
# closing brace) does not break a run.
runs = []
for r, lh in lines.items():
    run = None
    for n in sorted(lh):
        if lh[n] == 0:
            if run and n - run[1] <= 3:
                run[1] = n
                run[2] += 1
            else:
                if run:
                    runs.append((run[2], r, run[0], run[1]))
                run = [n, n, 1]
        elif run:
            runs.append((run[2], r, run[0], run[1]))
            run = None
    if run:
        runs.append((run[2], r, run[0], run[1]))
runs.sort(reverse=True)

# ---- functions -----------------------------------------------------------
# Generic functions appear once per instantiation; fold them by source
# location and call the function dead only if no instantiation ran.
names = [fn["name"] for fn in data["functions"]]
demangled = subprocess.run(
    ["rustfilt"], input="\n".join(names), capture_output=True, text=True
).stdout.split("\n")
fns = {}
for fn, dn in zip(data["functions"], demangled):
    r = rel(fn["filenames"][0])
    if r not in files:
        continue
    own = [g for g in fn["regions"] if g[5] == 0]
    start, end = min(g[0] for g in own), max(g[2] for g in own)
    e = fns.setdefault((r, start), dict(
        file=r, start=start, end=end, count=0,
        name=re.sub(r"::h[0-9a-f]{16}$", "", dn)))
    e["count"] = max(e["count"], fn["count"])
    e["end"] = max(e["end"], end)
dead = [e for e in fns.values() if e["count"] == 0 and "::tests::" not in e["name"]]
for e in dead:
    e["len"] = e["end"] - e["start"] + 1
    e["short"] = re.split(r"::", re.sub(r"(::\{closure#\d+\})+$", "", e["name"]))[-1].strip(">")
dead.sort(key=lambda e: -e["len"])

# ---- references ----------------------------------------------------------
src = subprocess.run("git ls-files '*.rs' '*.ts' '*.js'", shell=True,
                     capture_output=True, text=True).stdout.split()
idx = collections.defaultdict(list)
for f in src:
    for i, l in enumerate(open(f, errors="ignore"), 1):
        for t in set(TOK.findall(l)):
            idx[t].append((f, i))


def refs_of(e):
    """Occurrences of the function's name, minus its own definition."""
    return [x for x in idx.get(e["short"], [])
            if not (x[0] == e["file"] and abs(x[1] - e["start"]) <= 2)]


candidates = [e for e in dead if "{closure" not in e["name"] and TOK.fullmatch(e["short"])
              and e["file"] != QJS and " as " not in e["name"]]  # trait impls dispatch dynamically
unref = {id(e) for e in candidates if not refs_of(e)
         and not ("_" in e["short"] and idx.get(camel(e["short"])))}
changed = True
while changed:  # add functions referenced only from inside unreferenced ones
    changed = False
    bodies = collections.defaultdict(list)
    for e in candidates:
        if id(e) in unref:
            bodies[e["file"]].append((e["start"], e["end"]))
    for e in candidates:
        if id(e) in unref:
            continue
        live = [x for x in refs_of(e) if not is_test_path(x[0])
                and not any(a <= x[1] <= b for a, b in bodies.get(x[0], []))
                and not (x[0] == e["file"] and e["start"] <= x[1] <= e["end"])]
        if not live and refs_of(e):
            unref.add(id(e))
            changed = True
unreferenced = [e for e in candidates if id(e) in unref]

# ---- plugin API ----------------------------------------------------------
qjs_lines = open(QJS).read().split("\n")
plugin_src = {p: open(p).read() for p in src
              if p.startswith("crates/fresh-editor/plugins/") and p.endswith(".ts")
              and not p.endswith(".d.ts")}
api = []
for e in dead:
    if e["file"] != QJS or "JsEditorApi>::" not in e["name"] or "{closure" in e["name"]:
        continue
    js = None
    for i in range(e["start"] - 2, max(0, e["start"] - 15), -1):
        m = re.search(r'js_name\s*=\s*"(\w+)"', qjs_lines[i]) or \
            re.search(r'rename\s*=\s*"(\w+)"', qjs_lines[i])
        if m:
            js = m.group(1)
            break
        if re.search(r"^\s*(pub )?fn ", qjs_lines[i]):
            break
    js = re.sub("Start$", "", (js or camel(e["short"])).lstrip("_"))
    users = sorted(p for p, t in plugin_src.items() if re.search(r"\b" + js + r"\b", t))
    api.append(dict(js=js, len=e["len"], line=e["start"], plugins=users))

# ---- output --------------------------------------------------------------
tot = [sum(v[0] for v in files.values()), sum(v[1] for v in files.values())]
crates = collections.defaultdict(lambda: [0, 0])
for r, (n, c) in files.items():
    k = r.split("/")[1] if r.startswith("crates/") else r
    crates[k][0] += n
    crates[k][1] += c
pct = lambda n, c: round(100.0 * c / n, 1) if n else 100.0
out = dict(
    total=dict(lines=tot[0], covered=tot[1], pct=pct(*tot),
               functions=len(fns), never_executed=len(dead)),
    crates={k: dict(lines=n, covered=c, pct=pct(n, c)) for k, (n, c) in
            sorted(crates.items(), key=lambda x: -x[1][0])},
    unreferenced=[{k: e[k] for k in ("file", "start", "end", "len", "name")} for e in unreferenced],
    plugin_api_unexecuted=sorted(api, key=lambda a: -a["len"]),
    files_by_uncovered=[dict(file=r, lines=n, uncovered=n - c, pct=pct(n, c))
                        for r, (n, c) in sorted(files.items(), key=lambda x: x[1][1] - x[1][0])[:100]],
    uncovered_runs=[dict(len=n, file=r, start=s, end=e) for n, r, s, e in runs[:200]],
    never_executed=[{k: e[k] for k in ("file", "start", "end", "len", "name")} for e in dead[:500]],
)
json.dump(out, open(os.path.join(REPORT, "analysis.json"), "w"), indent=1)

t = out["total"]
print(f"line coverage {t['pct']}% ({t['covered']}/{t['lines']}); "
      f"{t['never_executed']}/{t['functions']} functions never executed")
print(f"\nunreferenced + never executed: {len(unreferenced)} functions, "
      f"{sum(e['len'] for e in unreferenced)} lines")
for e in unreferenced[:40]:
    print(f"  {e['len']:4}  {e['file']}:{e['start']}  {e['short']}")
unused = [a for a in api if not a["plugins"]]
print(f"\nplugin API never executed: {len(api)} methods; {len(unused)} unused by bundled plugins")
print("  " + ", ".join(sorted(a["js"] for a in unused)))
print("\nlargest uncovered runs:")
for x in out["uncovered_runs"][:25]:
    print(f"  {x['len']:4}  {x['file']}:{x['start']}-{x['end']}")
print(f"\nfull results: {REPORT}/analysis.json")
