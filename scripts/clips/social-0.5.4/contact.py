#!/usr/bin/env python3
"""Contact sheet of every still in a caps/*.json, for checking a take."""
import json, os, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from PIL import Image, ImageDraw
from ansirender import Painter

cap = json.load(open(sys.argv[1]))
names = [n for n, f in cap["frames"].items() if isinstance(f, dict) and "ans" in f]
only = sys.argv[3:] if len(sys.argv) > 3 else None
if only:
    names = [n for n in names if n in only]
p = Painter(8)
tiles = []
for n in names:
    im = p.paint(cap["frames"][n], cap["cols"], cap["rows"])
    ImageDraw.Draw(im).text((4, 4), n, fill=(255, 80, 80))
    tiles.append(im)
w, h = tiles[0].size
k = 3
sheet = Image.new("RGB", (w * k, h * ((len(tiles) + k - 1) // k)))
for i, t in enumerate(tiles):
    sheet.paste(t, ((i % k) * w, (i // k) * h))
sheet.save(sys.argv[2])
print(len(tiles), "tiles", sheet.size)
