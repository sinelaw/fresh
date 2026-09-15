#!/usr/bin/env python3
"""Generate the focused cut: the dock list alone, arranging and renaming.

No agent pane, no typed prompt, no before/after — one camera on the dock,
held still, while the rows file themselves into folders and then take real
names. Everything the other two cuts spend time establishing is cut.

This one needs its own take. The long cut films only three individual moves
before jumping to all-25-filed, which is fine when the moves are annotated
one at a time and useless when the arranging *is* the subject: it reads as
three steps and then a cut. So the capture asks the agent for an even
plateau on every move (`CLIP_SLOW_MOVES=0`, `CLIP_MOVE_PLATEAU`) and takes
a shot inside each one — 25 of them, plus the two phase boundaries.

The renaming does not need 16 more shots: `filed` and `after` share exact
row geometry, so one `swipe` beat cascades down all sixteen renamed rows and
steps straight over the nine junk rows in the middle, which says what the
long cut needed a caption for.

Vertical by default. The dock is a 41-column, 35-row rect — 0.54:1, within a
hair of 9:16 — so a 1080x1920 frame fits the whole list at about 49px a row.
A square frame fits the same rect by height and lands at 42% of frame width,
which is the opposite of zoomed in; `--square` therefore frames the top of
the list only.
"""
import json, os, glob

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.normpath(os.path.join(HERE, "..", "..",
                                    "fresh-dock-cleanup-focus.json"))
ROWS = json.load(open(os.path.join(HERE, "rows.json")))
N_MOVES = len(ROWS["rows"])

# One shot per move, and the renames run fast because the swipe covers them.
PACE = {"CLIP_SLOW_MOVES": "0", "CLIP_MOVE_PLATEAU": "1.3",
        "CLIP_SLOW_RENAMES": "0", "CLIP_RENAME_PLATEAU": "0.35",
        # Where the camera drops its raw dumps. Seeing a new one is how the
        # agent knows its last change has been photographed.
        "CLIP_SHOTS_DIR": "$CLIP_SHOTS_DIR"}

# Where in a plateau to photograph it, and how wide the plateau has to be.
# Both are sized off measurement rather than taste. A move is a process spawn
# and a round trip to the editor: median 0.18s, worst case 0.32s across a
# take (`agent-timing.log` records every one). On top of that the dock
# repaints on the terminal's own cycle, about ten times a second.
#
# At 0.7 x 0.66 the shot landed 0.46s in -- 0.14s clear of the worst round
# trip, less than the repaint interval -- and exactly one move in
# twenty-five came back identical to the one before it, the state lost for
# good. 1.1 x 0.75 puts the shutter 0.83s in, half a second clear of the
# worst case and five repaints past it. Capture costs ten seconds more.
SAMPLE = 0.60

# The agent follows the camera (see Gate in bin/clip-agent): it holds each
# state until a shot has been taken of it, so these sleeps only have to be
# longer than one mutation, not aimed at one. A mutation is a process spawn
# and a round trip -- 0.18s typical, 0.32s worst measured -- so 0.9s is
# generous and drift is harmless: nothing moves until the shutter goes.
LEAD_IN = 14.0     # launch -> agent up -> prompt typed -> folders created
STEP = 0.9         # between a shot and the next mutation landing
RENAMES = 16 * 0.35 + 3.0   # the ungated tail, plus margin

keys = [{"shot": "before"},
        {"key": "ctrl+p"}, {"sleep": 0.8},
        {"type": "Clip: Run Agent"}, {"sleep": 0.8},
        {"key": "Return"},
        {"sleep": LEAD_IN}, {"shot": "folders"}]
for i in range(N_MOVES):
    keys += [{"sleep": STEP}, {"shot": f"mv{i:02d}"}]
# The last move's screen is also the "everything filed, nothing renamed"
# screen, so it doubles as the swipe's starting frame -- no separate shot.
keys += [{"sleep": RENAMES}, {"shot": "after"}]

DOCK = [0, 41]
# Rows that actually change name, in the after-list. The folder headers and
# the nine rows inside `junk` are deliberately not in this list: the cascade
# stepping over them is the whole argument, drawn rather than captioned.
RENAMED = [5, 6, 7, 8, 9, 11, 12, 24, 25, 26, 28, 29, 31, 32, 34, 35]

# What the two words hang off. A note is dealt sideways from its rect's
# corner, so a rect spanning the dock's full width lands the plate at the
# right-hand edge and half of it falls outside the frame. Anchoring the note
# to the middle of the list instead leaves it room to sit in.
NOTE_AT = [2, 17]

# Square, framed on the top of the list. A 41-column rect over 18 rows is
# 697x685 capture pixels -- near enough 1:1 that it fills a square frame with
# no letterboxing, at about 58px a row. The whole list needs 35 rows and only
# fits a square frame by height, at 42% of its width, which is the opposite
# of zoomed in; the rest of the list carries on below the frame.
view = {"rows": [3, 21], "cols": DOCK}
last_row = 20


def visible_moves(shots_dir, floor=8.0):
    """how many move shots still change anything the frame can see.

    The camera is on the top of the list, so once the rows being filed have
    dropped out of the frame the remaining shots are identical pictures: the
    dock is still working, but not where anyone is looking. Measured on a
    take, the first ten moves change the visible rows by 15-19 mean absolute
    difference and everything after them by under 7, most of it by exactly
    zero -- so the cut is not a judgement call, it is where the numbers fall
    off.

    Returns None when there is no take to measure, in which case the spec
    keeps every move and the clip is merely longer than it needs to be.
    """
    try:
        from PIL import Image, ImageChops, ImageStat
    except ImportError:
        return None
    if not glob.glob(os.path.join(shots_dir, "solo-mv*.png")):
        return None
    r0, r1 = view["rows"]
    box = (0, int(r0 * 38.04), int(DOCK[1] * 17.01), int(r1 * 38.04))

    def frame(name):
        return Image.open(os.path.join(shots_dir, f"solo-{name}.png")) \
                    .convert("RGB").crop(box)

    prev, last = frame("folders"), 0
    for i in range(N_MOVES):
        try:
            cur = frame(f"mv{i:02d}")
        except OSError:
            break
        if sum(ImageStat.Stat(ImageChops.difference(prev, cur)).mean) / 3.0 >= floor:
            last = i
        prev = cur
    return last


SHOTS_DIR = os.environ.get("CLIP_SHOTS_DIR") or os.path.expanduser(
    "~/repos/tui-clips/out/fresh-dock-cleanup-focus/shots")
LAST_MOVE = visible_moves(SHOTS_DIR)
SHOWN = N_MOVES - 1 if LAST_MOVE is None else LAST_MOVE
# Only the rows the frame can actually show: the cascade should pace itself
# to what is on screen, not spend a second wiping rows nobody can see.
swipe_rows = [r for r in RENAMED if r <= last_row]

base = json.load(open(os.path.normpath(
    os.path.join(HERE, "..", "..", "fresh-dock-cleanup.json"))))
cap = dict(base["capture"])
cap["keys"] = keys
cap["env"] = dict(cap["env"], **PACE)

spec = {
  "name": "fresh-dock-cleanup-focus",
  "capture": cap,
  "render": {
    "size": [1080, 1080],
    "fps": 60,
    "rows": 50, "cols": 150,
    "title": "fresh — orchestrator dock",
    # Big enough to read as a label on the picture rather than a caption.
    "note_size": 58,
    # The phosphor pass, over every finished frame. Light: a deep scanline
    # comb is the first thing the encoder turns to mush, and the clip has to
    # survive being scaled down a feed.
    "crt": {"scanlines": 0.34, "gap": 4, "bloom": 0.55, "shift": 3,
            "vignette": 0.45, "curve": 0.11,
            # The last beat used to hold the finished dock and then fade.
            # It powers the tube off instead: the raster collapses to a
            # line, the line shortens to a dot, the dot decays. Takes the
            # end of the clip rather than adding to it, so the final hold
            # is what is left of beat 4 in front of it.
            "shutdown": 1.1, "off_glow": 1.5},
    "views": {"list": view},
    # No beat carries a `head` or a `sub`, so the caption bar is never drawn
    # and the viewport takes its full height. The words that do appear are
    # tags, not notes: a note is anchored to a rect and draws a leader back to
    # it, which is right when the words single out one row and wrong when they
    # name the whole beat -- there the leader has nothing to point at and just
    # crosses the picture. These sit against the right edge, vertically
    # centred, in the same place both times so the second reads as the first
    # swapping over.
    "timing": {"intro": 0, "zoom": 0, "hold": 0.8, "pan": 0.15,
               "push": 0.9, "wipe": 0.70, "outro": 0.40},
    "annotations": [
      # 1 — the flat list, briefly, so there is a before to measure against.
      {"shot": "before", "view": "list", "rows": [4, 30], "cols": DOCK,
       "band": False, "hold": 1.0},

      # 2 — folders appear, then all 25 rows file themselves, one per still.
      # Only as far as the frame can see it happen. What is left over is
      # carried by the wipe into the next beat, which replaces the whole
      # screen anyway -- so nothing jumps, it is just no longer dwelt on.
      {"shots": ["folders"] + [f"mv{i:02d}" for i in range(SHOWN + 1)],
       "crossfade": 0, "view": "list", "rows": [4, 8], "cols": NOTE_AT,
       "band": False, "hold": 1.9,
       "tag": {"text": "organize into folders", "at": "center-right",
               "width": 0.40, "color": "after", "bg": True}},

      # 3 — the names change in place, top to bottom, stepping over junk.
      {"shot": f"mv{N_MOVES - 1:02d}", "view": "list", "rows": [4, 8], "cols": NOTE_AT,
       "band": False, "hold": 1.9,
       "transition": "wipe",
       "swipe": {"to": "after", "rows": swipe_rows,
                 "at": 0.35, "row": 0.20, "stagger": 0.10, "edge": 3},
       "tag": {"text": "rename", "at": "center-right",
               "width": 0.40, "color": "after", "bg": True}},

      # 4 — hold what it made.
      {"shot": "after", "view": "list", "rows": [4, last_row], "cols": DOCK,
       "band": False, "hold": 0.9}
    ]
  },
  "encode": {"crf": 18, "preset": "slow"}
}
json.dump(spec, open(OUT, "w"), indent=2)
t_ = spec["render"]["timing"]
a = spec["render"]["annotations"]
holds = sum(x.get("hold", t_["hold"]) for x in a)
print("wrote", OUT)
travel = sum(t_.get("wipe", t_["push"]) if x.get("transition") == "wipe"
             else t_["push"] if x.get("transition") == "push" else t_["pan"]
             for x in a[1:])
total = holds + travel + t_["outro"]
off = float(spec["render"]["crt"].get("shutdown", 0))
print("~%.2fs  (%d beats, %d of %d moves shown; the tube starts dying at "
      "%.2fs)" % (total, len(a), SHOWN + 1, N_MOVES, total - off))
