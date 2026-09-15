#!/usr/bin/env python3
"""Generate scripts/clips/fresh-dock-cleanup.json from the agent's own timeline.

The capture has to photograph the dock *between* the agent's mutations. The
agent publishes the schedule it will keep (`clip-agent --timeline`), so the
sleeps here are derived from it rather than guessed — change a pause in the
agent and the spec follows. Shots are aimed at the middle of each plateau:
the dock is static between mutations, so a shot only has to land inside one,
which is what makes the take survive a few hundred ms of drift.
"""
import json, os, subprocess, sys

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, "..", "..", "fresh-dock-cleanup.json")
ROWS = json.load(open(os.path.join(HERE, "rows.json")))

tl = json.loads(subprocess.run([sys.executable, os.path.join(HERE, "bin", "clip-agent"),
                                "--timeline"], capture_output=True, text=True,
                               env=dict(os.environ, CLIP_HOME=HERE)).stdout)
at, t = {}, 0.0
for s in tl["steps"]:
    at[s["label"]] = (t, t + s["seconds"]); t += s["seconds"]
mid = lambda k: sum(at[k]) / 2.0

AGENT_START = 1.2          # Return -> pane up -> first byte from the agent
SHOTS = [("ask",     3.2),          # mid-sentence, the prompt still arriving
         ("asked",   mid("prompt") + 2.6),
         ("folders", mid("folders")),
         ("m1",      mid("move0")),
         ("m2",      mid("move1")),
         ("m3",      mid("move2")),
         ("filed",   mid("filed")),
         ("r1",      mid("rename0")),
         ("r2",      mid("rename1")),
         ("r3",      mid("rename2")),
         ("after",   at["done"][0] + 1.2)]

# The dock's compact row is `sessionNodeEntry`: state glyph, name, and the
# on-disk/pending tags — and nothing else. Branch and the git summary are not
# clipped at this width, they are simply not part of that row (card view
# carries them, and 25 rows of cards do not fit). The fixture's branches and
# uncommitted work are real either way; the dock just does not draw them here,
# and restyling it to would be filming something Fresh does not do.
keys = [{"shot": "before"},
        {"key": "ctrl+p"}, {"sleep": 0.8},
        {"type": "Clip: Run Agent"}, {"sleep": 0.8},
        {"key": "Return"}]
clock = -AGENT_START
for name, when in SHOTS:
    dt = round(when - clock, 2)
    if dt > 0.01:
        keys.append({"sleep": dt}); clock += dt
    keys.append({"shot": name})

# Dock geometry, in cells. The panel is the left column; rows start under
# its three-line header. Read off `tui-grid out/<name>/shots/solo-before.png`.
DOCK = [0, 41]   # the dock panel ends at column 39; two columns of air
HEAD, FIRST = 0, 4
N = len(ROWS["rows"])
row = lambda i, n=1: [FIRST + i, FIRST + i + n]

spec = {
  "name": "fresh-dock-cleanup",
  "capture": {
    "geometry": "150x50",
    "font": "JetBrains Mono 21",
    "screen": "2600x2000x24",
    "display": ":99",
    "settle": 58,
    "term": "xfce4-terminal",
    "key_settle": 0.2,
    "type_settle": 0.5,
    "copy_dirs": {"~/.cache/fresh-dock-clip/config/fresh": "{scratch}/config/fresh"},
    "env": {
      "XDG_CONFIG_HOME": "{scratch}/config",
      "XDG_DATA_HOME": "{scratch}/data-{pane}",
      "XDG_STATE_HOME": "{scratch}/state-{pane}",
      "XDG_CACHE_HOME": "{scratch}/cache-{pane}",
      "XDG_RUNTIME_DIR": "{scratch}/run-{pane}",
      "CLIP_HOME": "~/.cache/fresh-dock-clip"
    },
    "keys": keys,
    "panes": {"solo": {"argv": ["$FRESH_BIN", "--no-upgrade-check", "--no-restore"]}}
  },
  "render": {
    "size": [1080, 1080],
    "fps": 60,
    "rows": 50, "cols": 150,
    "title": "fresh — orchestrator dock",
    "note_size": 46,
    "views": {
      "before": {"rows": [2, 37], "cols": DOCK},
      "after":  {"rows": [2, 38], "cols": DOCK},
      "ask":    {"rows": [1, 12], "cols": [31, 92]},
      "payoff": {"rows": [3, 29], "cols": DOCK}
    },
    "intro_caption": ["25 workspaces, opened over a fortnight",
                      "not one of the names says what it is"],
    "outro_caption": ["fresh", "getfresh.dev"],
    "timing": {"intro": 2.2, "zoom": 0.9, "hold": 2.0, "pan": 0.45, "push": 0.9, "outro": 1.4},
    "annotations": [
      {"shot": "before", "rows": [4, 8], "cols": DOCK, "camera": "fit", "hold": 3.0,
       "head": "one row on the dock",
       "note": "a worktree, a branch, an agent", "note_at": "below-right"},

      {"shot": "before", "view": "before", "rows": [4, 30], "cols": DOCK, "hold": 3.4,
       "head": "three weeks of this",
       "note": "25 of them. not one says what it is.", "note_at": "below-right"},

      {"shot": "asked", "view": "ask", "rows": [3, 6], "cols": [33, 90], "hold": 3.4,
       "head": "the ask", "note": "one sentence", "note_at": "below-right"},

      {"shot": "folders", "view": "before", "rows": [4, 12], "cols": DOCK, "hold": 2.6,
       "head": "folders first", "note": "one per project", "note_at": "below-right"},

      {"shot": "m1", "rows": [4, 9], "cols": DOCK, "camera": "fit", "hold": 2.4,
       "head": "project_root \u2192 junk", "note": "never opened", "note_at": "below-right"},

      {"shot": "m2", "rows": [4, 9], "cols": DOCK, "camera": "fit", "hold": 2.4,
       "head": "fresh-31 \u2192 fresh", "note": "still running", "note_at": "below-right"},

      {"shot": "m3", "rows": [4, 10], "cols": DOCK, "camera": "fit", "hold": 2.4,
       "head": "session-2 \u2192 junk", "note": "an agent that finished", "note_at": "below-right"},

      {"shot": "filed", "rows": [13, 23], "cols": DOCK, "camera": "fit", "hold": 3.2,
       "head": "and the rest", "note": "nine were junk", "note_at": "below-right"},

      {"shot": "r1", "rows": [4, 10], "cols": DOCK, "camera": "fit", "hold": 2.3,
       "head": "fresh-31", "note": "Folding test harness", "note_at": "below-right"},

      {"shot": "r2", "rows": [4, 10], "cols": DOCK, "camera": "fit", "hold": 2.3,
       "head": "fresh-22", "note": "Status bar plugin", "note_at": "below-right"},

      {"shot": "r3", "rows": [10, 14], "cols": DOCK, "camera": "fit", "hold": 2.3,
       "head": "home-auto-7", "note": "Thermostat scheduler", "note_at": "below-right"},

      {"shot": "after", "view": "payoff", "rows": [13, 23], "cols": DOCK, "hold": 4.2,
       "head": "named, and filed",
       "note": "junk keeps its names", "note_at": "below-right"},

      {"shot": "before", "view": "before", "rows": [4, 30], "cols": DOCK, "hold": 3.0,
       "label": "BEFORE", "tone": "before", "band": False, "head": "25 rows"},

      {"shot": "after", "view": "after", "rows": [4, 36], "cols": DOCK, "hold": 4.0,
       "label": "AFTER", "tone": "after", "band": False,
       "transition": "push", "head": "7 folders, 16 named"}
    ]
  },
  "encode": {"crf": 18, "preset": "slow"}
}
json.dump(spec, open(OUT, "w"), indent=2)
print("wrote", os.path.normpath(OUT))
print("capture ends ~%.1fs after the agent starts" % SHOTS[-1][1])
