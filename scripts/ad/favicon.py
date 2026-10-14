#!/usr/bin/env python3
"""Make the landing page's favicons from docs/logo.png: the leaf alone, brightened.

The logo's leaf is the only strongly saturated green in it, against a navy
square (hue ~0.58) and a grey-green frame (low saturation), so a hue/saturation
mask plus the largest connected region cuts it out cleanly. The dark greens are
lifted so the leaf still reads at 16 px on a dark tab bar.

Writes homepage/public/assets/{favicon.ico, favicon-32.png, apple-touch-icon.png}.
"""
import os
from collections import deque

import numpy as np
from PIL import Image, ImageFilter

REPO = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
OUT = os.path.join(REPO, "homepage", "public", "assets")
INK = (12, 13, 15)


def flood(mask, seeds):
    """Cells of `mask` reachable (4-neighbour) from `seeds`."""
    h, w = mask.shape
    seen = np.zeros_like(mask)
    q = deque()
    for y, x in seeds:
        if mask[y, x] and not seen[y, x]:
            seen[y, x] = True
            q.append((y, x))
    while q:
        y, x = q.popleft()
        for yy, xx in ((y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)):
            if 0 <= yy < h and 0 <= xx < w and mask[yy, xx] and not seen[yy, xx]:
                seen[yy, xx] = True
                q.append((yy, xx))
    return seen


def leaf():
    src = Image.open(os.path.join(REPO, "docs", "logo.png")).convert("RGB")
    a = np.asarray(src, np.float32) / 255
    r, g, b = a[..., 0], a[..., 1], a[..., 2]
    mx, mn = a.max(-1), a.min(-1)
    d = mx - mn + 1e-6
    hue = np.where(mx == r, ((g - b) / d) % 6, np.where(mx == g, (b - r) / d + 2, (r - g) / d + 4)) / 6
    sat = d / (mx + 1e-6)
    mask = (hue > 0.2) & (hue < 0.45) & (sat > 0.42) & (mx > 0.13)

    # the leaf is the region around the logo's centre; then fill its holes
    h, w = mask.shape
    ys, xs = np.nonzero(mask)
    cy, cx = h // 2, w // 2
    i = np.argmin((ys - cy) ** 2 + (xs - cx) ** 2)
    region = flood(mask, [(ys[i], xs[i])])
    border = [(y, x) for y in (0, h - 1) for x in range(w)] + [(y, x) for x in (0, w - 1) for y in range(h)]
    region = ~flood(~region, border)

    alpha = (Image.fromarray((region * 255).astype(np.uint8))
             .filter(ImageFilter.MinFilter(3)).filter(ImageFilter.GaussianBlur(1.6)))
    hsv = np.asarray(src.convert("HSV"), np.float32) / 255
    hsv[..., 2] = np.clip(0.30 + 0.95 * hsv[..., 2] ** 0.62, 0, 1)
    hsv[..., 1] = np.clip(hsv[..., 1] * 1.08, 0, 1)
    out = Image.fromarray((hsv * 255).astype(np.uint8), "HSV").convert("RGBA")
    out.putalpha(alpha)
    out = out.crop(out.getbbox())
    side = int(max(out.size) * 1.06)
    square = Image.new("RGBA", (side, side), (0, 0, 0, 0))
    square.paste(out, ((side - out.width) // 2, (side - out.height) // 2), out)
    return square


def main():
    mark = leaf()
    mark.save(os.path.join(OUT, "favicon.ico"), sizes=[(16, 16), (32, 32), (48, 48)])
    mark.resize((32, 32), Image.LANCZOS).save(os.path.join(OUT, "favicon-32.png"), optimize=True)
    # iOS fills transparency with black, so give the touch icon the page's ink
    touch = Image.new("RGBA", (180, 180), INK + (255,))
    inner = mark.resize((132, 132), Image.LANCZOS)
    touch.paste(inner, (24, 24), inner)
    touch.convert("RGB").save(os.path.join(OUT, "apple-touch-icon.png"), optimize=True)
    for f in ("favicon.ico", "favicon-32.png", "apple-touch-icon.png"):
        print(f, os.path.getsize(os.path.join(OUT, f)), "bytes")


if __name__ == "__main__":
    main()
