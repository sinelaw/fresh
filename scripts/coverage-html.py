#!/usr/bin/env python3
"""Bundle a coverage run into one self-contained HTML file.

    scripts/coverage-html.py [REPORT_DIR] [OUT.html]

REPORT_DIR (default target/coverage-report) must hold `lcov.info` and the
`analysis.json` written by scripts/coverage-analyze.py; OUT defaults to
REPORT_DIR/coverage.html. Run from the repository root.

The page carries every covered source file, gzipped and base64-encoded
(~6 MB for the whole workspace), and unpacks it in the browser with
DecompressionStream, so it opens offline and can be passed around as a single
file. Tabs: files (sortable, filterable, click through to annotated source),
dead code, untested plugin API, largest uncovered blocks, per-crate totals.
"""
import base64
import collections
import datetime
import gzip
import html
import json
import os
import re
import subprocess
import sys

REPORT = sys.argv[1] if len(sys.argv) > 1 else "target/coverage-report"
OUT = sys.argv[2] if len(sys.argv) > 2 else os.path.join(REPORT, "coverage.html")
ROOT = os.getcwd()


def rel(p):
    return os.path.relpath(p, ROOT) if p.startswith(ROOT + "/") else None


def is_test_path(f):
    return "/tests/" in f or f.endswith("tests.rs") or "/benches/" in f


TEMPLATE = r"""<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Fresh coverage</title>
<style>
:root {
  --bg: #fbfbf9; --panel: #ffffff; --ink: #1d1f21; --muted: #6a6f76; --line: #e3e4e0;
  --accent: #2f5fbf; --hit: #e6f4ea; --hit-ink: #1e6b34; --miss: #fde7e7; --miss-ink: #a3261f;
  --dead: #f6d5d5; --bar: #d8dbe0; --good: #3c9a5f; --mid: #c79a2a; --bad: #c9473f;
  --code-bg: #ffffff; --gutter: #f3f3f0;
  color-scheme: light;
}
@media (prefers-color-scheme: dark) {
  :root {
    --bg: #15171a; --panel: #1c1f23; --ink: #e4e6e8; --muted: #9aa0a8; --line: #2c3036;
    --accent: #7ea6f2; --hit: #183322; --hit-ink: #8fd4a4; --miss: #3a1d1d; --miss-ink: #f19b93;
    --dead: #4a2222; --bar: #33373e; --good: #4fb573; --mid: #d9ad43; --bad: #e0625a;
    --code-bg: #181a1e; --gutter: #1f2227;
    color-scheme: dark;
  }
}
* { box-sizing: border-box; }
body { margin: 0; background: var(--bg); color: var(--ink);
  font: 14px/1.45 -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif; }
a { color: var(--accent); text-decoration: none; cursor: pointer; }
a:hover { text-decoration: underline; }
.wrap { max-width: 1200px; margin: 0 auto; padding: 24px 16px 64px; }
h1 { font-size: 22px; margin: 0 0 4px; }
h2 { font-size: 16px; margin: 28px 0 8px; }
.sub { color: var(--muted); margin: 0 0 20px; }
.kpis { display: flex; flex-wrap: wrap; gap: 12px; margin-bottom: 20px; }
.kpi { background: var(--panel); border: 1px solid var(--line); border-radius: 8px; padding: 12px 16px; min-width: 150px; flex: 1; }
.kpi b { display: block; font-size: 22px; font-variant-numeric: tabular-nums; }
.kpi span { color: var(--muted); font-size: 12px; }
nav.tabs { display: flex; gap: 4px; border-bottom: 1px solid var(--line); margin: 8px 0 16px; flex-wrap: wrap; }
nav.tabs a { padding: 8px 14px; border: 1px solid transparent; border-bottom: none; border-radius: 6px 6px 0 0; color: var(--muted); }
nav.tabs a.on { background: var(--panel); border-color: var(--line); color: var(--ink); margin-bottom: -1px; text-decoration: none; }
.tablewrap { overflow-x: auto; background: var(--panel); border: 1px solid var(--line); border-radius: 8px; }
table { border-collapse: collapse; width: 100%; }
th, td { text-align: left; padding: 6px 10px; border-bottom: 1px solid var(--line); white-space: nowrap; }
th { font-weight: 600; color: var(--muted); font-size: 12px; cursor: pointer; user-select: none; position: sticky; top: 0; background: var(--panel); }
td.num, th.num { text-align: right; font-variant-numeric: tabular-nums; }
td.path { white-space: normal; word-break: break-all; min-width: 16em; font-family: ui-monospace, SFMono-Regular, Menlo, monospace; font-size: 12.5px; }
tr:last-child td { border-bottom: none; }
.bar { display: inline-block; width: 90px; height: 8px; background: var(--bar); border-radius: 4px; vertical-align: middle; overflow: hidden; margin-right: 6px; }
.bar i { display: block; height: 100%; }
.controls { display: flex; gap: 8px; margin-bottom: 10px; flex-wrap: wrap; }
input[type=search], select { background: var(--panel); color: var(--ink); border: 1px solid var(--line); border-radius: 6px; padding: 6px 10px; font: inherit; }
input[type=search] { flex: 1; min-width: 200px; }
.note { color: var(--muted); font-size: 13px; margin: 4px 0 12px; max-width: 80ch; }
.chip { display: inline-block; font-size: 11px; padding: 1px 6px; border-radius: 10px; background: var(--bar); color: var(--muted); margin-left: 6px; }
/* source view */
.srchead { display: flex; align-items: center; gap: 10px; flex-wrap: wrap; margin-bottom: 10px; }
.srchead .path { font-family: ui-monospace, monospace; font-weight: 600; word-break: break-all; }
button { background: var(--panel); color: var(--ink); border: 1px solid var(--line); border-radius: 6px; padding: 5px 10px; font: inherit; cursor: pointer; }
button:hover { border-color: var(--accent); }
.code { background: var(--code-bg); border: 1px solid var(--line); border-radius: 8px; overflow-x: auto;
  font: 12.5px/1.5 ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; }
.ln { display: flex; min-width: max-content; }
.ln .no, .ln .ct { flex: none; text-align: right; padding: 0 8px; color: var(--muted); background: var(--gutter); user-select: none; }
.ln .no { width: 56px; }
.ln .ct { width: 64px; border-right: 1px solid var(--line); }
.ln .tx { padding: 0 12px; white-space: pre; }
.ln.h .ct { color: var(--hit-ink); }
.ln.m { background: var(--miss); }
.ln.m .ct { color: var(--miss-ink); font-weight: 600; }
.ln.fn .no { box-shadow: inset 3px 0 0 var(--bad); }
.ln.target { outline: 2px solid var(--accent); outline-offset: -2px; }
.legend { display: flex; gap: 14px; flex-wrap: wrap; color: var(--muted); font-size: 12px; }
.sw { display: inline-block; width: 12px; height: 12px; border-radius: 3px; vertical-align: -2px; margin-right: 4px; border: 1px solid var(--line); }
#loading { padding: 40px 0; color: var(--muted); }
</style>
</head>
<body>
<div class="wrap">
  <h1>Fresh test coverage</h1>
  <p class="sub">__SUBTITLE__</p>
  <div id="loading">Decompressing report…</div>
  <div id="app" hidden>
    <div class="kpis" id="kpis"></div>
    <nav class="tabs" id="tabs">
      <a data-t="files">Files</a>
      <a data-t="dead">Dead code</a>
      <a data-t="api">Plugin API</a>
      <a data-t="runs">Largest uncovered blocks</a>
      <a data-t="crates">Crates</a>
    </nav>
    <section id="view"></section>
  </div>
</div>
<script id="data" type="application/octet-stream">__DATA__</script>
<script>
(async function () {
  const $ = (s, el = document) => el.querySelector(s);
  const esc = s => s.replace(/[&<>"]/g, c => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));
  const fmt = n => n.toLocaleString("en-US");
  const col = p => p >= 85 ? "var(--good)" : p >= 60 ? "var(--mid)" : "var(--bad)";
  const bar = p => `<span class="bar"><i style="width:${p}%;background:${col(p)}"></i></span>`;

  let D;
  try {
    const bin = Uint8Array.from(atob($("#data").textContent.trim()), c => c.charCodeAt(0));
    const stream = new Blob([bin]).stream().pipeThrough(new DecompressionStream("gzip"));
    D = JSON.parse(await new Response(stream).text());
  } catch (e) {
    $("#loading").textContent = "This browser could not decompress the embedded report (needs DecompressionStream: Chrome 80+, Firefox 113+, Safari 16.4+). " + e;
    return;
  }
  $("#loading").hidden = true; $("#app").hidden = false;

  const byPath = new Map(D.files.map(f => [f.p, f]));
  for (const f of D.files) { f.u = f.t - f.c; f.pct = f.t ? 100 * f.c / f.t : 100; }

  const T = D.total;
  $("#kpis").innerHTML = [
    [T.pct.toFixed(1) + "%", "line coverage"],
    [fmt(T.covered) + " / " + fmt(T.lines), "lines covered"],
    [fmt(T.never_executed) + " / " + fmt(T.functions), "functions never executed"],
    [fmt(D.unref.length), "unreferenced dead functions (" + fmt(D.unref.reduce((a, e) => a + e.len, 0)) + " lines)"],
  ].map(([b, s]) => `<div class="kpi"><b>${b}</b><span>${s}</span></div>`).join("");

  const fileLink = (p, l) => `<a href="#src/${encodeURIComponent(p)}${l ? ":" + l : ""}">${esc(p)}${l ? ":" + l : ""}</a>`;

  // ---------- views ----------
  let fileSort = { k: "u", dir: -1 }, fileFilter = "";
  function viewFiles() {
    const v = $("#view");
    v.innerHTML = `<div class="controls">
      <input type="search" id="q" placeholder="Filter by path…" value="${esc(fileFilter)}">
      </div>
      <div class="tablewrap"><table><thead><tr>
        <th data-k="p">File</th><th class="num" data-k="t">Lines</th><th class="num" data-k="u">Uncovered</th>
        <th class="num" data-k="pct">Coverage</th><th class="num" data-k="dn">Dead fns</th></tr></thead><tbody id="tb"></tbody></table></div>`;
    const draw = () => {
      const q = fileFilter.toLowerCase();
      const rows = D.files.filter(f => f.p.toLowerCase().includes(q)).map(f => (f.dn = f.d.length, f));
      rows.sort((a, b) => (a[fileSort.k] > b[fileSort.k] ? 1 : a[fileSort.k] < b[fileSort.k] ? -1 : 0) * fileSort.dir);
      $("#tb").innerHTML = rows.map(f => `<tr><td class="path">${fileLink(f.p)}</td><td class="num">${fmt(f.t)}</td>
        <td class="num">${fmt(f.u)}</td><td class="num">${bar(f.pct)}${f.pct.toFixed(1)}%</td><td class="num">${f.dn || ""}</td></tr>`).join("");
    };
    $("#q").addEventListener("input", e => { fileFilter = e.target.value; draw(); });
    v.querySelectorAll("th").forEach(th => th.addEventListener("click", () => {
      const k = th.dataset.k; fileSort = { k, dir: fileSort.k === k ? -fileSort.dir : (k === "p" ? 1 : -1) }; draw();
    }));
    draw();
  }

  function viewDead() {
    const rows = D.unref;
    $("#view").innerHTML = `<p class="note">Functions that never ran <b>and</b> whose name appears nowhere else in the repository (Rust, TS, JS),
      plus those referenced only from inside such functions or only from test files. Matching is by name, so functions with common
      names (<code>new</code>, <code>run</code>, trait methods) are excluded. Confirm each with a build before deleting.
      Also see <code>textmate_engine.rs</code>: an entire module nothing outside its own tests uses.</p>
      <div class="tablewrap"><table><thead><tr><th class="num">Lines</th><th>Function</th><th>Location</th></tr></thead><tbody>
      ${rows.map(e => `<tr><td class="num">${e.len}</td><td class="path">${esc(e.name)}</td><td class="path">${fileLink(e.file, e.start)}</td></tr>`).join("")}
      </tbody></table></div>`;
  }

  function viewApi() {
    const unused = D.api.filter(a => !a.plugins.length), used = D.api.filter(a => a.plugins.length);
    const tbl = list => `<div class="tablewrap"><table><thead><tr><th>JS name</th><th class="num">Lines</th><th>Bundled plugins using it</th><th>Rust</th></tr></thead><tbody>
      ${list.map(a => `<tr><td class="path">${esc(a.js)}</td><td class="num">${a.len}</td>
        <td class="path">${a.plugins.map(p => esc(p.split("/").pop())).join(", ") || "—"}</td>
        <td class="path">${fileLink("crates/fresh-plugin-runtime/src/backend/quickjs_backend.rs", a.line)}</td></tr>`).join("")}</tbody></table></div>`;
    $("#view").innerHTML = `<p class="note"><code>JsEditorApi</code> methods no test executed. Names like <code>info</code>/<code>error</code>
      match plugin text loosely, so their "used by" lists are overstated.</p>
      <h2>Unused by any bundled plugin (${unused.length})</h2>${tbl(unused)}
      <h2>Used by bundled plugins, but untested (${used.length})</h2>${tbl(used)}`;
  }

  function viewRuns() {
    $("#view").innerHTML = `<p class="note">Longest stretches of consecutive uncovered lines (gaps of up to 3 non-code lines don't break a run).</p>
      <div class="tablewrap"><table><thead><tr><th class="num">Lines</th><th>Location</th></tr></thead><tbody>
      ${D.runs.map(r => `<tr><td class="num">${r.len}</td><td class="path">${fileLink(r.file, r.start)}–${r.end}</td></tr>`).join("")}
      </tbody></table></div>`;
  }

  function viewCrates() {
    $("#view").innerHTML = `<div class="tablewrap"><table><thead><tr><th>Crate</th><th class="num">Lines</th><th class="num">Uncovered</th><th class="num">Coverage</th></tr></thead><tbody>
      ${Object.entries(D.crates).map(([k, c]) => `<tr><td><a href="#files" data-crate="${esc(k)}">${esc(k)}</a></td><td class="num">${fmt(c.lines)}</td>
        <td class="num">${fmt(c.lines - c.covered)}</td><td class="num">${bar(c.pct)}${c.pct.toFixed(1)}%</td></tr>`).join("")}
      </tbody></table></div>`;
    $("#view").querySelectorAll("[data-crate]").forEach(a => a.addEventListener("click", () => { fileFilter = "crates/" + a.dataset.crate + "/"; }));
  }

  function viewSrc(path, line) {
    const f = byPath.get(path);
    if (!f) { $("#view").textContent = "Unknown file " + path; return; }
    const hits = new Map(); let n = 0;
    for (let i = 0; i < f.h.length; i += 2) { n += f.h[i]; hits.set(n, f.h[i + 1]); }
    const inFn = new Set();
    for (const [a, b] of f.d) for (let i = a; i <= b; i++) inFn.add(i);
    const lines = f.s.split("\n");
    const out = [];
    for (let i = 0; i < lines.length; i++) {
      const no = i + 1, c = hits.get(no);
      const cls = "ln" + (c === undefined ? "" : c > 0 ? " h" : " m") + (inFn.has(no) ? " fn" : "");
      out.push(`<div class="${cls}" id="L${no}"><span class="no">${no}</span><span class="ct">${c === undefined ? "" : c > 0 ? (c > 99999 ? "99k+" : c) : "0"}</span><span class="tx">${esc(lines[i]) || " "}</span></div>`);
    }
    $("#view").innerHTML = `<div class="srchead"><button id="back">← Back</button><span class="path">${esc(path)}</span>
      <span class="chip">${f.pct.toFixed(1)}% · ${fmt(f.u)} uncovered</span>
      <button id="prev">↑ prev uncovered</button><button id="next">↓ next uncovered</button></div>
      <div class="legend" style="margin-bottom:8px"><span><i class="sw" style="background:var(--miss)"></i>not executed</span>
      <span>number = execution count</span><span><i class="sw" style="background:var(--bad)"></i>left edge: inside a never-executed function</span>
      <span>no count = not code</span></div>
      <div class="code">${out.join("")}</div>`;
    $("#back").onclick = () => history.back();
    // uncovered block starts, for prev/next navigation
    const starts = [];
    const nos = [...hits.keys()].sort((a, b) => a - b);
    let prevMiss = false;
    for (const no of nos) { const m = hits.get(no) === 0; if (m && !prevMiss) starts.push(no); prevMiss = m; }
    let cur = line || 0;
    const go = no => { const el = document.getElementById("L" + no); if (!el) return;
      document.querySelectorAll(".ln.target").forEach(x => x.classList.remove("target"));
      el.classList.add("target"); el.scrollIntoView({ block: "center" }); cur = no; };
    $("#next").onclick = () => { const s = starts.find(x => x > cur); if (s) go(s); };
    $("#prev").onclick = () => { const s = [...starts].reverse().find(x => x < cur); if (s) go(s); };
    if (line) requestAnimationFrame(() => go(line)); else window.scrollTo(0, 0);
  }

  const views = { files: viewFiles, dead: viewDead, api: viewApi, runs: viewRuns, crates: viewCrates };
  function route() {
    const h = decodeURIComponent(location.hash.slice(1)) || "files";
    let tab = h;
    if (h.startsWith("src/")) {
      const m = h.slice(4).match(/^(.*?)(?::(\d+))?$/);
      tab = null; viewSrc(m[1], m[2] ? +m[2] : 0);
    } else (views[h] || viewFiles)();
    document.querySelectorAll("#tabs a").forEach(a => a.classList.toggle("on", a.dataset.t === tab));
    if (tab) window.scrollTo(0, 0);
  }
  document.querySelectorAll("#tabs a").forEach(a => a.href = "#" + a.dataset.t);
  window.addEventListener("hashchange", route);
  route();
})();
</script>
</body>
</html>
"""


analysis = json.load(open(os.path.join(REPORT, "analysis.json")))

hits = collections.defaultdict(dict)
cur = None
for ln in open(os.path.join(REPORT, "lcov.info")):
    if ln.startswith("SF:"):
        cur = rel(ln[3:].strip())
        if cur and is_test_path(cur):
            cur = None
    elif cur and ln.startswith("DA:"):
        n, c = ln[3:].split(",")[:2]
        hits[cur][int(n)] = int(c)

never = collections.defaultdict(list)
for e in analysis["never_executed"]:
    never[e["file"]].append([e["start"], e["end"], e["name"]])

files = []
for path in sorted(hits):
    h = hits[path]
    flat, prev = [], 0  # [line delta, count, ...]
    for n in sorted(h):
        flat += [n - prev, h[n]]
        prev = n
    files.append(dict(p=path, t=len(h), c=sum(1 for c in h.values() if c > 0),
                      s=open(path, errors="replace").read(), h=flat, d=never.get(path, [])))

payload = dict(total=analysis["total"], crates=analysis["crates"],
               unref=analysis["unreferenced"], api=analysis["plugin_api_unexecuted"],
               runs=analysis["uncovered_runs"][:150], files=files)
blob = base64.b64encode(gzip.compress(
    json.dumps(payload, separators=(",", ":")).encode(), 9)).decode()

# Subtitle: what ran, from the nextest log scripts/coverage-full keeps.
parts = ["Full suite (unit, integration, e2e)"]
log = os.path.join(REPORT, "nextest.log")
if os.path.exists(log):
    summary = [l for l in open(log, errors="replace") if re.search(r"^\s*Summary \[", l)]
    if summary:
        m = re.search(r"(\d+ tests? run: .*)", summary[-1])
        if m:
            parts.append(re.sub(r"\x1b\[[0-9;]*m", "", m.group(1)).strip())
commit = subprocess.run(["git", "rev-parse", "--short", "HEAD"],
                        capture_output=True, text=True).stdout.strip()
version = re.search(r'^version\s*=\s*"([^"]+)"', open("Cargo.toml").read(), re.M)
parts.append(" ".join(x for x in [
    datetime.date.today().isoformat(),
    "v" + version.group(1) if version else "",
    commit and "@ " + commit] if x))

page = TEMPLATE.replace("__SUBTITLE__", html.escape(" · ".join(parts)))
page = page.replace("__DATA__", blob)
open(OUT, "w").write(page)
print(f"{OUT}: {os.path.getsize(OUT) / 1e6:.1f} MB, {len(files)} files")
