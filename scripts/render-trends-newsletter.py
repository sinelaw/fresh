#!/usr/bin/env python3
"""Render docs/internal/trends-research.md as a standalone HTML newsletter.

Usage: python3 scripts/render-trends-newsletter.py [source.md] [out.html]
Defaults: docs/internal/trends-research.md -> /tmp/trends-newsletter.html

Pure stdlib, no dependencies. Re-run this after every trends-research.md
update to regenerate the "Terminal Radar" newsletter artifact.
"""
import html
import re
import sys
from pathlib import Path

CATEGORIES = [
    # (match substring, short label, css class)
    ("sandbox", "Sandboxing & Permissions", "sbx"),
    ("permission", "Sandboxing & Permissions", "sbx"),
    ("orchestrat", "Orchestrators & Multi-Agent UIs", "orch"),
    ("cli coding agent", "CLI Coding Agents", "cli"),
    ("tui framework", "TUI Frameworks & Editors", "tui"),
    ("editor ecosystem", "TUI Frameworks & Editors", "tui"),
    ("diff", "Diff & Review Tooling", "diff"),
    ("review tooling", "Diff & Review Tooling", "diff"),
    ("spec-driven", "Spec-Driven Dev & Plan Mode", "spec"),
    ("plan mode", "Spec-Driven Dev & Plan Mode", "spec"),
    ("mcp", "MCP", "mcp"),
]

QUIET_RE = re.compile(
    r"^(no (new|essential|major|confident)|quiet)", re.IGNORECASE
)


def inline_md(text: str) -> str:
    text = html.escape(text, quote=False)
    text = re.sub(r"`([^`]+)`", r"<code>\1</code>", text)
    text = re.sub(
        r"\[([^\]]+)\]\(([^)]+)\)",
        r'<a href="\2" target="_blank" rel="noopener">\1<span class="ext">↗</span></a>',
        text,
    )
    text = re.sub(r"\*\*([^*]+)\*\*", r"<strong>\1</strong>", text)
    text = re.sub(r"(?<!\*)\*([^*]+)\*(?!\*)", r"<em>\1</em>", text)
    return text


def match_categories(heading: str):
    seen = []
    low = heading.lower()
    for needle, label, cls in CATEGORIES:
        if needle in low and (label, cls) not in seen:
            seen.append((label, cls))
    if not seen:
        seen = [(heading.strip(), "misc")]
    return seen


def parse_doc(md: str):
    chunks = re.split(r"\n---\n", md.strip() + "\n")
    preamble = chunks[0]
    title_m = re.search(r"^#\s+(.+)$", preamble, re.MULTILINE)
    doc_title = title_m.group(1).strip() if title_m else "Trends Research"
    purpose_lines = [
        l.strip()
        for l in preamble.splitlines()
        if l.strip() and not l.startswith("#")
    ]
    purpose = " ".join(purpose_lines)

    entries = []
    for chunk in chunks[1:]:
        chunk = chunk.strip()
        if not chunk.startswith("## "):
            continue
        lines = chunk.splitlines()
        date = lines[0][3:].strip()
        rest = "\n".join(lines[1:])
        sections = re.split(r"\n### ", rest)
        intro = sections[0].strip()
        cats = []
        for sec in sections[1:]:
            sec_lines = sec.splitlines()
            heading = sec_lines[0].strip()
            body = "\n".join(sec_lines[1:]).strip()
            bullets = re.findall(r"^- (.+)$", body, re.MULTILINE)
            prose = "\n".join(
                l for l in body.splitlines() if l.strip() and not l.startswith("- ")
            ).strip()
            cats.append(
                {
                    "heading": heading,
                    "bullets": bullets,
                    "prose": prose,
                }
            )
        entries.append({"date": date, "intro": intro, "cats": cats})
    entries.sort(key=lambda e: e["date"], reverse=True)
    return doc_title, purpose, entries


def render(doc_title: str, purpose: str, entries: list) -> str:
    latest_date = entries[0]["date"] if entries else ""

    toc_chips = "\n".join(
        f'<a class="chip" href="#e-{e["date"]}">{e["date"]}</a>' for e in entries
    )

    legend_seen = {}
    for e in entries:
        for c in e["cats"]:
            for label, cls in match_categories(c["heading"]):
                legend_seen.setdefault(cls, label)
    legend_html = "\n".join(
        f'<span class="legend-item"><span class="dot dot-{cls}"></span>{label}</span>'
        for cls, label in legend_seen.items()
        if cls != "misc"
    )

    entry_blocks = []
    for i, e in enumerate(entries):
        is_latest = i == 0
        badge = '<span class="badge-new">Latest</span>' if is_latest else ""
        intro_html = (
            f'<p class="entry-intro">{inline_md(e["intro"])}</p>' if e["intro"] else ""
        )
        cat_blocks = []
        for c in e["cats"]:
            pills = "".join(
                f'<span class="pill pill-{cls}">{label}</span>'
                for label, cls in match_categories(c["heading"])
            )
            body_html = ""
            if c["bullets"]:
                items = "\n".join(
                    f"<li>{inline_md(b)}</li>" for b in c["bullets"]
                )
                body_html = f'<ul class="findings">{items}</ul>'
                if c["prose"]:
                    body_html = (
                        f'<p class="cat-prose">{inline_md(c["prose"])}</p>' + body_html
                    )
            elif c["prose"]:
                quiet_cls = " quiet" if QUIET_RE.search(c["prose"].strip()) else ""
                body_html = f'<p class="cat-prose{quiet_cls}">{inline_md(c["prose"])}</p>'
            cat_blocks.append(
                f'<div class="category">'
                f'<div class="cat-head">{pills}</div>'
                f"{body_html}"
                f"</div>"
            )
        entry_blocks.append(
            f'<section class="entry" id="e-{e["date"]}">'
            f'<div class="entry-head"><h2>{e["date"]}</h2>{badge}</div>'
            f"{intro_html}"
            f'<div class="categories">{"".join(cat_blocks)}</div>'
            f"</section>"
        )

    entries_html = "\n".join(entry_blocks)

    return TEMPLATE.replace("__TITLE__", html.escape(doc_title)) \
        .replace("__PURPOSE__", inline_md(purpose)) \
        .replace("__LATEST_DATE__", latest_date) \
        .replace("__TOC__", toc_chips) \
        .replace("__LEGEND__", legend_html) \
        .replace("__ENTRIES__", entries_html) \
        .replace("__COUNT__", str(len(entries)))


TEMPLATE = r"""<title>Terminal Radar</title>
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link href="https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@500;600&family=Source+Serif+4:opsz,wght@8..60,400;8..60,600;8..60,700&display=swap" rel="stylesheet">
<style>
:root{
  --bg:#f1f3ee; --surface:#ffffff; --surface-2:#e7ebe3; --border:#d6ddd1;
  --text:#1b2320; --text-muted:#57645a; --accent:#3d6b5c; --accent-ink:#ffffff;
  --cat-orch:#3d6b5c; --cat-cli:#35597d; --cat-tui:#8a7220; --cat-sbx:#b5652f;
  --cat-diff:#7a4a6b; --cat-spec:#2f7a8a; --cat-mcp:#a13d3d; --cat-misc:#5c6b62;
  --shadow: 0 1px 2px rgba(27,35,32,0.06), 0 6px 20px -8px rgba(27,35,32,0.12);
}
@media (prefers-color-scheme: dark){
  :root:not([data-theme="light"]){
    --bg:#12160f; --surface:#191f18; --surface-2:#222922; --border:#2d362b;
    --text:#dde5da; --text-muted:#9db0a2; --accent:#75b89c; --accent-ink:#0d1a15;
    --cat-orch:#75b89c; --cat-cli:#7bb0e0; --cat-tui:#d8c04f; --cat-sbx:#e69a5e;
    --cat-diff:#c98cb8; --cat-spec:#63c6da; --cat-mcp:#e17e7e; --cat-misc:#93a498;
    --shadow: 0 1px 2px rgba(0,0,0,0.3), 0 8px 24px -8px rgba(0,0,0,0.5);
  }
}
:root[data-theme="dark"]{
  --bg:#12160f; --surface:#191f18; --surface-2:#222922; --border:#2d362b;
  --text:#dde5da; --text-muted:#9db0a2; --accent:#75b89c; --accent-ink:#0d1a15;
  --cat-orch:#75b89c; --cat-cli:#7bb0e0; --cat-tui:#d8c04f; --cat-sbx:#e69a5e;
  --cat-diff:#c98cb8; --cat-spec:#63c6da; --cat-mcp:#e17e7e; --cat-misc:#93a498;
  --shadow: 0 1px 2px rgba(0,0,0,0.3), 0 8px 24px -8px rgba(0,0,0,0.5);
}

*{box-sizing:border-box;}
body{
  background:var(--bg); color:var(--text);
  font-family:"Source Serif 4", Georgia, "Times New Roman", serif;
  font-size:17px; line-height:1.6;
}
.wrap{max-width:760px; margin:0 auto; padding:2.75rem 1.25rem 5rem;}
h1,h2,h3{text-wrap:balance; font-weight:700;}

.mono{font-family:"IBM Plex Mono", ui-monospace, SFMono-Regular, Consolas, monospace;}

header.masthead{
  display:flex; flex-direction:column; gap:.9rem;
  padding-bottom:1.6rem; border-bottom:2px solid var(--border);
  margin-bottom:1.75rem;
}
.masthead-top{display:flex; align-items:baseline; justify-content:space-between; gap:1rem; flex-wrap:wrap;}
h1.sitetitle{font-size:1.9rem; letter-spacing:-0.01em; margin:0;}
.eyebrow{
  font-family:"IBM Plex Mono", monospace; font-size:.72rem; font-weight:600;
  letter-spacing:.09em; text-transform:uppercase; color:var(--accent);
  margin:0 0 .35rem;
}
.scan-badge{
  font-family:"IBM Plex Mono", monospace; font-size:.78rem; color:var(--text-muted);
  background:var(--surface-2); border:1px solid var(--border); border-radius:99px;
  padding:.3rem .8rem; white-space:nowrap;
}
.scan-badge strong{color:var(--text); font-weight:600;}
p.purpose{color:var(--text-muted); font-size:.98rem; max-width:64ch; margin:0;}

.legend{display:flex; flex-wrap:wrap; gap:.5rem 1rem; margin-top:.25rem;}
.legend-item{
  font-family:"IBM Plex Mono", monospace; font-size:.72rem; color:var(--text-muted);
  display:inline-flex; align-items:center; gap:.4rem; letter-spacing:.02em;
}
.dot{width:.55rem; height:.55rem; border-radius:50%; display:inline-block; flex:none;}
.dot-orch{background:var(--cat-orch);} .dot-cli{background:var(--cat-cli);}
.dot-tui{background:var(--cat-tui);} .dot-sbx{background:var(--cat-sbx);}
.dot-diff{background:var(--cat-diff);} .dot-spec{background:var(--cat-spec);}
.dot-mcp{background:var(--cat-mcp);} .dot-misc{background:var(--cat-misc);}

nav.toc{display:flex; flex-wrap:wrap; gap:.5rem; margin-bottom:2.5rem;}
nav.toc .chip{
  font-family:"IBM Plex Mono", monospace; font-size:.78rem; color:var(--text);
  background:var(--surface); border:1px solid var(--border); border-radius:7px;
  padding:.32rem .65rem; text-decoration:none;
}
nav.toc .chip:hover{border-color:var(--accent); color:var(--accent);}

.entry{margin-bottom:3.25rem; padding-left:1.15rem; border-left:3px solid var(--border); position:relative;}
.entry:first-of-type{border-left-color:var(--accent);}
.entry-head{display:flex; align-items:center; gap:.7rem; margin-bottom:.5rem;}
.entry-head h2{
  font-family:"IBM Plex Mono", monospace; font-size:1.25rem; font-weight:600;
  letter-spacing:-.01em; margin:0;
}
.badge-new{
  font-family:"IBM Plex Mono", monospace; font-size:.66rem; font-weight:600;
  letter-spacing:.08em; text-transform:uppercase; color:var(--accent-ink);
  background:var(--accent); border-radius:99px; padding:.2rem .55rem;
}
p.entry-intro{color:var(--text-muted); font-style:italic; margin:0 0 1.3rem; max-width:66ch;}

.categories{display:flex; flex-direction:column; gap:1.5rem;}
.category{}
.cat-head{display:flex; flex-wrap:wrap; gap:.4rem; margin-bottom:.55rem;}
.pill{
  font-family:"IBM Plex Mono", monospace; font-size:.68rem; font-weight:600;
  letter-spacing:.04em; text-transform:uppercase; color:#fff;
  border-radius:5px; padding:.22rem .55rem;
}
.pill-orch{background:var(--cat-orch);} .pill-cli{background:var(--cat-cli);}
.pill-tui{background:var(--cat-tui); color:#1b1600;} .pill-sbx{background:var(--cat-sbx);}
.pill-diff{background:var(--cat-diff);} .pill-spec{background:var(--cat-spec);}
.pill-mcp{background:var(--cat-mcp);} .pill-misc{background:var(--cat-misc);}

ul.findings{margin:0; padding:0; list-style:none; display:flex; flex-direction:column; gap:.85rem;}
ul.findings li{
  background:var(--surface); border:1px solid var(--border); border-radius:10px;
  padding:.85rem 1rem; box-shadow:var(--shadow); font-size:.97rem;
}
p.cat-prose{margin:.2rem 0 .6rem; color:var(--text);}
p.cat-prose.quiet{
  color:var(--text-muted); font-size:.9rem; font-style:italic;
  border-left:2px dashed var(--border); padding-left:.7rem; margin-left:.05rem;
}

code{
  font-family:"IBM Plex Mono", monospace; font-size:.88em; background:var(--surface-2);
  border:1px solid var(--border); border-radius:4px; padding:.05em .3em;
}
a{color:var(--accent); text-decoration-thickness:1px; text-underline-offset:2px;}
a .ext{font-size:.75em; margin-left:.05em; text-decoration:none; display:inline-block;}
strong{font-weight:700;}
em{font-style:italic;}

footer{
  margin-top:2.5rem; padding-top:1.5rem; border-top:1px solid var(--border);
  font-family:"IBM Plex Mono", monospace; font-size:.75rem; color:var(--text-muted);
  display:flex; flex-direction:column; gap:.3rem;
}
footer a{color:var(--text-muted);}

@media (max-width:520px){
  body{font-size:16px;}
  .masthead-top{flex-direction:column; align-items:flex-start;}
}
</style>

<div class="wrap">
  <header class="masthead">
    <div class="masthead-top">
      <div>
        <p class="eyebrow">Fresh &middot; design-direction scan</p>
        <h1 class="sitetitle">Terminal Radar</h1>
      </div>
      <span class="scan-badge">Last scan <strong>__LATEST_DATE__</strong></span>
    </div>
    <p class="purpose">__PURPOSE__</p>
    <div class="legend">__LEGEND__</div>
  </header>

  <nav class="toc" aria-label="Jump to entry">__TOC__</nav>

  <main>
    __ENTRIES__
  </main>

  <footer>
    <span>Source: <code>docs/internal/trends-research.md</code> on branch <code>tui-automated-trends</code></span>
    <span>Rendered by <code>scripts/render-trends-newsletter.py</code> &middot; __COUNT__ entries logged</span>
  </footer>
</div>
"""


def main():
    src = Path(sys.argv[1]) if len(sys.argv) > 1 else Path("docs/internal/trends-research.md")
    out = Path(sys.argv[2]) if len(sys.argv) > 2 else Path("/tmp/trends-newsletter.html")
    md = src.read_text(encoding="utf-8")
    doc_title, purpose, entries = parse_doc(md)
    html_out = render(doc_title, purpose, entries)
    out.write_text(html_out, encoding="utf-8")
    print(f"Wrote {out} ({len(entries)} entries, latest {entries[0]['date'] if entries else 'n/a'})")


if __name__ == "__main__":
    main()
