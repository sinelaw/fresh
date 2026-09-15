#!/usr/bin/env python3
"""Generate the 14s social cut, scripts/clips/fresh-dock-cleanup-short.json.

Same capture as the long cut — same shots, same take — so this file only
rewrites `render`. Run gen-spec.py first; the capture block is copied from
it rather than restated, which is what keeps the two cuts filming the same
thing when the fixture changes.

The social cut is not the long cut with beats deleted. It is three ideas —
the mess, the ask, the result — and the rename is promoted from three
annotated beats to one swipe, which is the only moment where an old name
and its replacement occupy the same pixel.
"""
import json, os, sys

HERE = os.path.dirname(os.path.abspath(__file__))
LONG = os.path.normpath(os.path.join(HERE, "..", "..", "fresh-dock-cleanup.json"))
VERT = "--vertical" in sys.argv
OUT = os.path.normpath(os.path.join(
    HERE, "..", "..",
    "fresh-dock-cleanup-short-vertical.json" if VERT
    else "fresh-dock-cleanup-short.json"))

base = json.load(open(LONG))
DOCK = [0, 41]

# The rows the rename lands on, in the order the agent renames them: the five
# `fresh` worktrees, then the two `home-auto` ones. Folder headers (4 and 10)
# carry no name change and are left alone.
SWIPE_ROWS = [5, 6, 7, 8, 9, 11, 12]

spec = {
  "name": "fresh-dock-cleanup-short-vertical" if VERT else "fresh-dock-cleanup-short",
  "capture": base["capture"],
  "render": {
    # A 41-column, 34-row dock rect is 0.54:1 — within a hair of 9:16. The
    # square frame can only ever fit that by height and leaves the surplus
    # width to the editor pane beside it; vertical fills.
    "size": [1080, 1920] if VERT else [1080, 1080],
    "fps": 60,
    "rows": 50, "cols": 150,
    "title": "fresh — orchestrator dock",
    "note_size": 52,
    "views": {
      # The mess is 26 rows; framing only those rather than the taller
      # after-list buys the dock a third more width in a square frame.
      "mess":  {"rows": [3, 31], "cols": DOCK},
      "list":  {"rows": [3, 37], "cols": DOCK},
      "agent": {"rows": [1, 17], "cols": [32, 95]},
      "names": {"rows": [3, 14], "cols": DOCK}
    },
    # No beat carries a `head` or a `sub`, so the caption bar is not drawn at
    # all and the viewport takes its height. On a feed the bar is read last or
    # not at all; every word here is a note, in the frame, on the picture.
    "outro_caption": ["fresh", "getfresh.dev"],
    "timing": {"intro": 0, "zoom": 0, "hold": 2.2, "pan": 0.2,
               "push": 0.5, "outro": 0.8},
    "annotations": [
      # 1 — the mess, cold. No establishing zoom: frame one is the whole list.
      {"shot": "before", "view": "mess", "rows": [4, 30], "cols": DOCK,
       "hold": 2.2, "note": "messy workspaces", "note_at": "below-right"},

      # 2 — the ask, and the agent actually churning: four screens of its
      # output arriving, hard-cut, so the pane reads as rolling rather than
      # as one held frame of a sentence.
      {"shots": ["asked", "folders", "m1", "m2"], "crossfade": 0,
       "view": "agent", "rows": [3, 6], "cols": [33, 90], "band": False,
       "hold": 3.2, "note": "ask agent to organize them",
       "note_at": "below-right"},

      # 3 — the same four moments from the dock's side, plus the last two.
      # Flat cadence: the evenness is what reads as a machine working.
      {"shots": ["folders", "m1", "m2", "m3", "filed"], "crossfade": 0,
       "view": "list", "rows": [4, 36], "cols": DOCK, "band": False,
       "hold": 2.4},

      # 4 — the swipe. Old names in, new names out, one row at a time.
      {"shot": "filed", "view": "names", "rows": [5, 13], "cols": DOCK,
       "hold": 3.0,
       "swipe": {"to": "after", "rows": SWIPE_ROWS,
                 "at": 0.35, "row": 0.20, "stagger": 0.10, "edge": 3},
       "note": "renamed to match the work", "note_at": "below-right"},

      # 5/6 — A/B, identical framing so only the content moves.
      {"shot": "before", "view": "list", "rows": [4, 36], "cols": DOCK,
       "hold": 0.5, "label": "BEFORE", "tone": "before", "band": False},
      {"shot": "after", "view": "list", "rows": [4, 36], "cols": DOCK,
       "hold": 0.9, "label": "AFTER", "tone": "after", "band": False}
    ]
  },
  "encode": {"crf": 18, "preset": "slow"}
}
json.dump(spec, open(OUT, "w"), indent=2)
t = spec["render"]["timing"]
a = spec["render"]["annotations"]
holds = sum(x.get("hold", t["hold"]) for x in a)
travel = t["pan"] * (len(a) - 1)
print("wrote", OUT)
print("~%.1fs  (%d beats, holds %.1f, travel %.1f, outro %.1f)"
      % (t["intro"] + t["zoom"] + holds + travel + t["outro"],
         len(a), holds, travel, t["outro"]))
