#!/usr/bin/env python3
"""Check a focus take photographed the states it was aimed at.

The capture and the agent keep separate clocks. They are anchored to each
other by one constant -- how long `Return` takes to become a running agent --
and if that constant is wrong every shot in the sequence lands a plateau off:
the states are all still *distinct*, so nothing looks broken, but each one is
the next move's. That is invisible in the stills and obvious only in the
finished render, which is the expensive place to find it.

So check the alignment directly, against the dock's own rows:

  folders  the folders exist, no row filed yet
  mv<i>    one more row filed than mv<i-1>
  after    the last move's screen with every rename applied

The agent now waits on the camera rather than on a clock (see Gate in
bin/clip-agent), so a shifted take should be impossible -- this stays as the
check that says so.

Usage: verify-shots.py [out/fresh-dock-cleanup-focus/shots]
"""
import json, os, sys
from PIL import Image, ImageChops, ImageStat

HERE = os.path.dirname(os.path.abspath(__file__))
ROWS = json.load(open(os.path.join(HERE, "rows.json")))
N = len(ROWS["rows"])
CW, RH = 17.01, 38.04          # capture cell, 150x50 over 2552x1902
DOCK_COLS, DOCK_ROWS = 41, 40

d = sys.argv[1] if len(sys.argv) > 1 else os.path.expanduser(
    "~/repos/tui-clips/out/fresh-dock-cleanup-focus/shots")


# A row counts as changed when enough of its pixels move, not when any of
# them do. Two screens of the same dock differ here and there without a word
# of text differing -- a caret phase, a scrollbar segment, an antialiased
# glyph edge -- and an exact comparison calls junk rows "renamed" that a
# reader cannot tell apart.
#
# Measured on a good take, the two populations do not overlap: the sixteen
# rewritten rows score 11.5 to 20.1 mean absolute difference, and the rows
# that only look different to a hash score 0.95 and 5.7. The threshold sits
# in the gap, nearer the noise.
ROW_CHANGE = 8.0          # mean absolute difference, 0-255 per channel


def row_strips(name):
    im = Image.open(os.path.join(d, f"solo-{name}.png")).convert("RGB")
    return [im.crop((0, int(r * RH), int(DOCK_COLS * CW), int((r + 1) * RH)))
            for r in range(DOCK_ROWS)]


def changed(a, b):
    out = []
    for r in range(DOCK_ROWS):
        stat = ImageStat.Stat(ImageChops.difference(a[r], b[r]))
        if sum(stat.mean) / 3.0 > ROW_CHANGE:
            out.append(r)
    return out


fail = []
folders = row_strips("folders")
mv = [row_strips(f"mv{i:02d}") for i in range(N)]
after = row_strips("after")

# Each move must move the dock on by one, and `folders` must be before any of
# them: if the whole sequence is shifted, `folders` equals mv00's predecessor
# and the last move is missing off the end.
if not changed(folders, mv[0]):
    fail.append("`folders` and `mv00` are the same screen — the sequence is "
                "shifted; raise AGENT_START in gen-spec-focus.py")
for i in range(1, N):
    if not changed(mv[i - 1], mv[i]):
        fail.append(f"mv{i-1:02d} and mv{i:02d} are the same screen")
# The last move's screen is the swipe's starting frame: everything filed,
# nothing renamed. If the take were shifted it would already carry renames,
# and the count here would come out under sixteen.
n_renamed = len(changed(mv[N - 1], after))
if n_renamed != sum(1 for r in ROWS["rows"] if r.get("rename")):
    fail.append(f"`filed`->`after` changes {n_renamed} rows, but "
                f"{sum(1 for r in ROWS['rows'] if r.get('rename'))} rows are "
                f"renamed — mv{N-1:02d} was photographed after the renames "
                f"began")

print(f"{N} moves, {n_renamed} rows renamed between mv{N-1:02d} and after")
if fail:
    for f in fail:
        print("FAIL " + f)
    raise SystemExit(1)
print("take is aligned")
