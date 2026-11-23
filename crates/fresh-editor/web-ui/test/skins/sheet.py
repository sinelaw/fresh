# python3 -I sheet.py <dir> <surface> [maxw]: one surface's shots from tour.mjs, one per skin,
# in a labelled column (SHEET-<surface>.png), for reviewing a concern across skins.
import sys, os
from PIL import Image, ImageDraw
d, c = sys.argv[1], sys.argv[2]
maxw = int(sys.argv[3]) if len(sys.argv) > 3 else 900
skins = ['cosmos','macos','macos-dark','compact','winamp','neon-hive']
ims = []
for s in skins:
    f = os.path.join(d, f'{c}--{s}.png')
    if not os.path.exists(f): continue
    im = Image.open(f).convert('RGB')
    if im.width > maxw: im = im.resize((maxw, int(im.height * maxw / im.width)))
    ims.append((s, im))
W = max(i.width for _, i in ims) + 20
H = sum(i.height + 28 for _, i in ims) + 10
out = Image.new('RGB', (W, H), (128, 128, 128))
dr = ImageDraw.Draw(out); y = 6
for s, im in ims:
    dr.text((10, y), s, fill=(0, 0, 0)); y += 18
    out.paste(im, (10, y)); y += im.height + 10
out.save(os.path.join(d, f'SHEET-{c}.png'))
print(c, out.size)
