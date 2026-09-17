#!/usr/bin/env python3
"""Generate scripts/clips/fresh-dock-ready.json -- the dock's badges, filmed.

One camera, held still on the dock column, for nine seconds. Eleven agents
in two folders, staged before the take; nothing is typed and nothing moves
except the rows' own minds. Three finish, one stops to ask, and the dock
says so before you have looked at any of them.

The list never shifts, which is the point of framing it this way: every
pixel that changes is a badge changing. (It used to shift -- the dock
carried a `● N need you · ✓ N done` line that was drawn only while the
counts were non-zero, so the first check to land pushed every row down one.
That line is gone; see the withdrawal note in
docs/internal/orchestrator-ux-redesign.md 2.3.)

The whole clip is one take of real time. A row's badge is a function of when
its terminal last spoke (orchestrator.ts: `sessionState`), so the only way to
photograph a badge is to be there when it lands -- which means the camera and
the agents have to share a clock. They share an event instead: W, the first
shutter. bin/wait-for-start.py holds every agent until a raw dump appears in
the shots directory, and every `at:` below is measured from the `mark`
alongside that same shot.

The arithmetic is the plugin's, not this file's. A row that stops talking at
W+`run` is still `working` for IDLE_AFTER_MS (5s), and the sweep that
repaints it is scheduled 100ms after that, so its badge lands at
`run` + 5.1. Each shot is taken a few tenths later again -- far enough past
the repaint to be certain, near enough that the next badge has not landed on
top of it.
"""
import json, os

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.normpath(os.path.join(HERE, "..", "..", "fresh-dock-ready.json"))
TABLE = json.load(open(os.path.join(HERE, "rows.json")))

IDLE_AFTER = 5.0        # orchestrator.ts IDLE_AFTER_MS
SWEEP = 0.1             # scheduleIdleSweep's extra 100ms
MARGIN = 0.45           # past the repaint, before the next badge

def lands(name):
    """When `name`'s badge appears, in seconds after W."""
    row = next(r for r in TABLE["rows"] if r[0] == name)
    return row[4] + IDLE_AFTER + SWEEP

# The three that finish, in the order they finish, and the one that asks.
DONE = sorted((r[0] for r in TABLE["rows"] if r[3] == "done"), key=lands)
ASKS = next(r[0] for r in TABLE["rows"] if r[3] == "blocked")

# Staging is eleven `git worktree add`s and eleven terminal spawns: measured
# at 6-8s, and the first shutter must not land inside it or the rows that are
# still being built would start work mid-storm and have their opening lines
# written off as layout noise (see bin/agent.sh).
SETTLE = 24

# Everything working. Late enough that every row has been printing for a
# while and none has stopped yet: the first badge to change is
# `tf-drift`'s, at W+7.1.
BUSY_AT = 6.5

keys = [{"mark": True}, {"shot": "go"}, {"at": BUSY_AT}, {"shot": "busy"}]
for i, name in enumerate(DONE):
    keys += [{"at": round(lands(name) + MARGIN, 2)}, {"shot": f"d{i + 1}"}]
keys += [{"at": round(lands(ASKS) + MARGIN, 2)}, {"shot": "need"}]
# One beat of the settled dock, far enough past the last change that nothing
# is still moving.
keys += [{"at": round(lands(ASKS) + 1.7, 2)}, {"shot": "settle"}]

# The dock is 39 columns at this geometry and its wall sits in the 40th. The
# crop takes the column and the wall: `fit` pads a framed rect by FIT_PAD on
# each side and then fills the rest of the frame with whatever the capture
# has there, so a couple of columns of editor arrive whatever this says --
# better they arrive behind the wall, where they read as the edge of a
# window, than in front of it. Row 0 is the panel's title, which is part of
# what the clip is showing, and row 16 is the last one the list uses: three
# rows of chrome, `main`, two folder headers and eleven rows.
DOCK = [0, 40]
VIEW = {"rows": [0, 17], "cols": DOCK}
# What a tag hangs off. Tags are placed against the frame, so this rect only
# decides which part of the picture the beat is nominally about; keeping it
# off the right-hand column stops the plate being dealt over the names.
NOTE_AT = [2, 20]

GREEN = "after"           # the theme's added/ok key -- the `✓`s own colour

# One plate, in the same place for the whole clip, rather than three swapping
# over: the beats are three states of one picture, not three subjects.
TAG = {"text": "better status icons", "at": "center-right",
       "width": 0.40, "color": GREEN, "bg": True}

spec = {
  "name": "fresh-dock-ready",
  "capture": {
    "geometry": "150x50",
    "font": "JetBrains Mono 21",
    "screen": "2600x2000x24",
    "display": ":99",
    "settle": SETTLE,
    "term": "xfce4-terminal",
    "key_settle": 0.2,
    "type_settle": 0.5,
    "copy_dirs": {
      "~/.cache/fresh-dock-ready/config/fresh": "{scratch}/config/fresh"
    },
    "env": {
      "XDG_CONFIG_HOME": "{scratch}/config",
      "XDG_DATA_HOME": "{scratch}/data-{pane}",
      "XDG_STATE_HOME": "{scratch}/state-{pane}",
      "XDG_CACHE_HOME": "{scratch}/cache-{pane}",
      "XDG_RUNTIME_DIR": "{scratch}/run-{pane}",
      "CLIP_HOME": "~/.cache/fresh-dock-ready",
      # How the agents know the camera is rolling.
      "CLIP_SHOTS_DIR": "$CLIP_SHOTS_DIR"
    },
    # One pane, opened on the fixture tree: that window is the launch
    # workspace -- the row named `main`, the one that stays active so every
    # other row's work counts as unseen.
    "panes": {"dock": {"argv": ["$FRESH_BIN", "--no-upgrade-check",
                                "--no-restore", "$CLIP_HOME/tree"]}},
    "keys": keys
  },
  "render": {
    "size": [1080, 1080],
    "fps": 60,
    "rows": 50, "cols": 150,
    "title": "fresh — orchestrator dock",
    "note_size": 56,
    "views": {"list": VIEW},
    # A tube, but a quiet one. The dock-cleanup cut wears a pronounced CRT
    # because its subject is rows moving; this one's subject is five glyphs
    # that have to stay legible, and a deep scanline comb is the first thing
    # a feed's encoder turns to mush. The curve is the part that earns its
    # place: the other four are corrections applied to a flat rectangle,
    # which is what a screenshot already is, so without it the pass reads as
    # a filter over a picture rather than a picture on a tube -- and the
    # power-off needs a tube to be a power-off. `shutdown` takes the end of
    # the clip rather than adding to it: the raster collapses to a line, the
    # line to a dot, the dot decays, all inside beat 4's hold.
    "crt": {"scanlines": 0.18, "gap": 4, "bloom": 0.35, "shift": 2,
            "vignette": 0.30, "curve": 0.07,
            "shutdown": 0.55, "off_glow": 1.5},
    # No beat carries a `head` or a `sub`, so the caption bar is never drawn
    # and the viewport takes the full frame. The words are tags, not notes: a
    # note draws a leader back to its rect, which is right when it singles
    # out one row and wrong when it names the whole beat. All three sit in
    # the same place, so each reads as the one before it swapping over.
    "timing": {"intro": 0, "zoom": 0, "hold": 0.8, "pan": 0.15,
               "push": 0.9, "wipe": 0.7, "outro": 0.25},
    "annotations": [
      # 1 — eleven rows in two folders, all working.
      {"shot": "busy", "view": "list", "rows": [1, 16], "cols": NOTE_AT,
       "band": False, "hold": 1.8, "tag": TAG},

      # 2 — three checks land, one at a time, each in its own folder's stretch
      # of the list. Nothing else on screen moves.
      {"shots": [f"d{i + 1}" for i in range(len(DONE))],
       "crossfade": 0, "view": "list", "rows": [1, 16], "cols": NOTE_AT,
       "band": False, "hold": 2.5, "tag": TAG},

      # 3 — and one of them stopped to ask. Red beats green: a question is
      # checked before unseen work, so `backend` rolls up `●1 ✓1`.
      {"shot": "need", "view": "list", "rows": [1, 16], "cols": NOTE_AT,
       "band": False, "hold": 2.7, "tag": TAG},

      # 4 — the dock, said plainly, with nothing written over it. The tube
      # powers off across this beat rather than after it.
      {"shot": "settle", "view": "list", "rows": [0, 17], "cols": DOCK,
       "band": False, "hold": 1.1}
    ]
  },
  "encode": {"crf": 18, "preset": "slow"}
}

json.dump(spec, open(OUT, "w"), indent=2)
t = spec["render"]["timing"]
a = spec["render"]["annotations"]
holds = sum(x.get("hold", t["hold"]) for x in a)
travel = sum(t.get(x.get("transition", "pan"), t["pan"]) for x in a[1:])
print("wrote", OUT)
print("shots  : " + "  ".join(
    f"{k['shot']}@W+{keys[i - 1]['at']}" if "at" in keys[i - 1] else k["shot"]
    for i, k in enumerate(keys) if "shot" in k))
print("capture: %ds settle + %.1fs of take" % (SETTLE, keys[-2]["at"]))
total = holds + travel + t["outro"]
off = float(spec["render"]["crt"].get("shutdown", 0))
print("clip   : ~%.2fs over %d beats (the tube starts dying at %.2fs)"
      % (total, len(a), total - off))
