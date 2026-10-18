#!/usr/bin/env python3
"""Cut the captured screens into a square, ~21s social video.

    compose.py out.mp4 [--draft] [--frames a:b] [--still t ...]

Story: fresh keeps adding features -> and it is still light and fast (measured)
-> 0.5.4 is about keeping it that way (fresh FILE opens just the file) -> plus
the new Markdown modes -> outro.
"""
import json, math, os, random, subprocess, sys
from functools import lru_cache

from PIL import Image, ImageDraw, ImageFilter, ImageFont

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
REPO = os.path.abspath(os.path.join(HERE, "../../.."))
WORK = os.environ.get("CLIP_WORK", os.path.join(REPO, "target/clips/social-0.5.4"))
from ansirender import Painter  # noqa: E402

W = H = 1080
FPS = 30
INK = (12, 13, 15)
PAPER = (236, 233, 226)
SAGE = (163, 201, 174)
MINT = (0, 217, 163)
MUTE = (138, 141, 145)
OLD = (232, 128, 120)

SERIF = os.path.join(WORK, "fonts/InstrumentSerif-Regular.ttf")
SERIF_I = os.path.join(WORK, "fonts/InstrumentSerif-Italic.ttf")
INTER = "/usr/share/fonts/opentype/inter/Inter-{}.otf"
DISPLAY = "/usr/share/fonts/opentype/inter/InterDisplay-{}.otf"
MONO_B = "/usr/share/fonts/truetype/jetbrains-mono/JetBrainsMono-Bold.ttf"


@lru_cache(maxsize=None)
def font(path, size):
    return ImageFont.truetype(path, size)


# ---------------------------------------------------------------- easing

def clamp(x, a=0.0, b=1.0):
    return max(a, min(b, x))


def prog(t, a, b):
    return clamp((t - a) / (b - a)) if b > a else float(t >= a)


def out_cubic(x):
    return 1 - (1 - x) ** 3


def in_cubic(x):
    return x ** 3


def in_out(x):
    return 4 * x ** 3 if x < 0.5 else 1 - (-2 * x + 2) ** 3 / 2


def out_back(x, s=1.7):
    x -= 1
    return 1 + (s + 1) * x ** 3 + s * x ** 2


def lerp(a, b, x):
    return a + (b - a) * x


# ---------------------------------------------------------------- screens

CAPS = {}
for name in ("launch", "montage", "sess-v053", "sess-v054"):
    with open(os.path.join(WORK, "caps", name + ".json")) as f:
        CAPS[name] = json.load(f)

PAINTER = Painter(20)
COLS, ROWS = 100, 36
SW, SH = COLS * PAINTER.cw, ROWS * PAINTER.ch


@lru_cache(maxsize=None)
def screen(cap, name, idx=None, cursor=True):
    fr = CAPS[cap]["frames"][name]
    if idx is not None:
        fr = fr[idx]
    return PAINTER.paint(fr, COLS, ROWS, cursor=cursor).convert("RGB")


@lru_cache(maxsize=64)
def round_mask(w, h, r):
    m = Image.new("L", (w * 2, h * 2), 0)
    ImageDraw.Draw(m).rounded_rectangle([0, 0, w * 2 - 1, h * 2 - 1], r * 2, fill=255)
    return m.resize((w, h), Image.LANCZOS)


@lru_cache(maxsize=64)
def shadow(w, h, r, blur=28, a=170):
    pad = blur * 3
    s = Image.new("RGBA", (w + 2 * pad, h + 2 * pad), (0, 0, 0, 0))
    ImageDraw.Draw(s).rounded_rectangle([pad, pad + 12, pad + w, pad + h + 12], r,
                                        fill=(0, 0, 0, a))
    return s.filter(ImageFilter.GaussianBlur(blur)), pad


def view_of(cx, cy, vw, aspect):
    """a camera rect in screen pixels: centre and width"""
    vh = vw / aspect
    x0, y0 = cx - vw / 2, cy - vh / 2
    return (x0, y0, x0 + vw, y0 + vh)


FULL = (0, 0, SW, SH)


def shot(img, view, w, h):
    x0, y0, x1, y1 = view
    # keep the camera inside the screen, so nothing outside it ever shows
    vw, vh = x1 - x0, y1 - y0
    x0 = clamp(x0, 0, SW - vw); y0 = clamp(y0, 0, SH - vh)
    return img.resize((w, h), Image.LANCZOS if vw / w > 1.6 else Image.BICUBIC,
                      box=(x0, y0, x0 + vw, y0 + vh))


def mix_view(a, b, x):
    return tuple(lerp(p, q, x) for p, q in zip(a, b))


def panel(canvas, pic, box, r=22, alpha=1.0, border=True):
    """paste a camera picture as a rounded window with a soft shadow"""
    x, y, w, h = [int(round(v)) for v in box]
    if w < 4 or h < 4 or alpha <= 0:
        return
    if pic.size != (w, h):
        pic = pic.resize((w, h), Image.BICUBIC)
    r = max(2, min(r, w // 4, h // 4))
    sh, pad = shadow(w, h, r)
    if alpha < 1:
        sh = sh.copy(); sh.putalpha(sh.getchannel("A").point(lambda v: int(v * alpha)))
    canvas.alpha_composite(sh, (x - pad, y - pad))
    m = round_mask(w, h, r)
    if alpha < 1:
        m = m.point(lambda v: int(v * alpha))
    layer = pic.convert("RGBA"); layer.putalpha(m)
    canvas.alpha_composite(layer, (x, y))
    if border:
        d = ImageDraw.Draw(canvas)
        d.rounded_rectangle([x, y, x + w - 1, y + h - 1], r,
                            outline=(255, 255, 255, int(28 * alpha)), width=1)


def wipe(a, b, x, tone=MINT):
    """b replaces a behind a hard edge moving left to right"""
    if x <= 0:
        return a
    if x >= 1:
        return b
    w, h = a.size
    e = int(w * in_out(x))
    out = a.copy()
    out.paste(b.crop((0, 0, e, h)), (0, 0))
    ImageDraw.Draw(out).rectangle([e - 3, 0, e + 3, h], fill=tone)
    return out


# ---------------------------------------------------------------- type

def text_img(s, f, fill, stroke=0):
    l, t, r, b = f.getbbox(s, stroke_width=stroke)
    im = Image.new("RGBA", (r - l + 4, b - t + 4), (0, 0, 0, 0))
    ImageDraw.Draw(im).text((2 - l, 2 - t), s, font=f, fill=fill, stroke_width=stroke,
                            stroke_fill=INK)
    return im, (l, t)


def put(canvas, s, f, fill, x, y, anchor="l", alpha=1.0, rise=0.0, scale=1.0):
    """text at a baseline-ish position: y is the top of the ink box"""
    if alpha <= 0:
        return
    im, _ = text_img(s, f, fill)
    if scale != 1.0:
        im = im.resize((max(1, int(im.width * scale)), max(1, int(im.height * scale))),
                       Image.LANCZOS)
    if alpha < 1:
        im.putalpha(im.getchannel("A").point(lambda v: int(v * alpha)))
    if anchor == "c":
        x -= im.width / 2
    elif anchor == "r":
        x -= im.width
    canvas.alpha_composite(im, (int(x), int(y + rise)))
    return im.width


def headline(canvas, t, t0, title, sub=None, t_out=None, serif=SERIF):
    a = out_cubic(prog(t, t0, t0 + 0.35))
    if t_out is not None:
        a *= 1 - prog(t, t_out, t_out + 0.25)
    put(canvas, title, font(serif, 84), PAPER, 60, 62, alpha=a, rise=(1 - a) * 24)
    if sub:
        b = out_cubic(prog(t, t0 + 0.12, t0 + 0.5))
        if t_out is not None:
            b *= 1 - prog(t, t_out, t_out + 0.25)
        put(canvas, sub, font(INTER.format("Regular"), 30), MUTE, 62, 168, alpha=b,
            rise=(1 - b) * 16)


def pill(canvas, s, x, y, fg, bg, size=32, alpha=1.0, scale=1.0, anchor="l", weight="SemiBold",
         outline=None):
    f = font(INTER.format(weight), size)
    l, t, r, b = f.getbbox(s)
    pw, ph = r - l + size * 1.3, size * 1.75
    im = Image.new("RGBA", (int(pw) + 4, int(ph) + 4), (0, 0, 0, 0))
    d = ImageDraw.Draw(im)
    d.rounded_rectangle([1, 1, pw, ph], ph / 2, fill=bg + (235,),
                        outline=outline, width=2 if outline else 0)
    d.text(((pw - (r - l)) / 2 - l + 1, (ph - (b - t)) / 2 - t + 1), s, font=f, fill=fg)
    if scale != 1.0:
        im = im.resize((max(1, int(im.width * scale)), max(1, int(im.height * scale))),
                       Image.LANCZOS)
    if alpha < 1:
        im.putalpha(im.getchannel("A").point(lambda v: int(v * alpha)))
    if anchor == "c":
        x -= im.width / 2; y -= im.height / 2
    elif anchor == "r":
        x -= im.width
    canvas.alpha_composite(im, (int(x), int(y)))
    return im.width


@lru_cache(maxsize=None)
def pointer_img(h=46):
    s = 4
    pts = [(0, 0), (0, 16), (4.2, 12.4), (7, 18.6), (9.6, 17.4), (6.9, 11.4), (12, 11.4)]
    k = h * s / 19
    im = Image.new("RGBA", (int(14 * k) + 8, int(20 * k) + 8), (0, 0, 0, 0))
    d = ImageDraw.Draw(im)
    poly = [(4 + x * k, 4 + y * k) for x, y in pts]
    d.polygon(poly, fill=(255, 255, 255, 255), outline=(0, 0, 0, 255), width=int(1.6 * s))
    return im.resize((im.width // s, im.height // s), Image.LANCZOS)


# ---------------------------------------------------------------- scenes

PANEL = (60, 236, 960, 760)            # x y w h ; 960/760 matches the screen
ASPECT = SW / SH

FEATURES = [  # (cap, shot, chip)
    ("montage", "code", "syntax highlighting"),
    ("montage", "palette", "command palette"),
    ("montage", "multicursor", "multi-cursor"),
    ("montage", "explorer", "file explorer"),
    ("montage", "split", "split panes"),
    ("montage", "livediff", "live diff"),
    ("montage", "review", "review diff"),
    ("montage", "gitlog", "git log"),
    ("montage", "settings", "settings UI"),
    ("montage", "keys", "keybinding editor"),
    ("montage", "dock", "orchestrator"),
    ("montage", "terminal", "terminals"),
    ("launch", "md_dual", "markdown preview"),
    ("montage", "THEMES", "themes"),
]
THEMES = ["theme_nord", "theme_gruvbox", "theme_light", "theme_dracula",
          "theme_solarized_dark", "theme_tokyo_night"]

CARD_W, CARD_H = 780, 618
rng = random.Random(7)
ARRIVE = []
t = 0.30
for i in range(len(FEATURES)):
    ARRIVE.append(t)
    t += lerp(0.34, 0.17, i / (len(FEATURES) - 1))
CARD_POSE = [(rng.uniform(-7, 7), rng.uniform(-36, 36), rng.uniform(-26, 26),
              [(0, 1), (1, 0.2), (-1, 0.3), (0.3, -1), (-0.6, 1), (1, 0.8)][i % 6])
             for i in range(len(FEATURES))]
PILE_C = (540, 650)
T_COLLAPSE = ARRIVE[-1] + 0.45


@lru_cache(maxsize=None)
def card(i, theme_idx=0):
    cap, name, chip = FEATURES[i]
    if name == "THEMES":
        name = THEMES[theme_idx]
    img = shot(screen(cap, name, cursor=False), FULL, CARD_W, CARD_H).convert("RGBA")
    img.putalpha(round_mask(CARD_W, CARD_H, 18))
    pad = 40
    out = Image.new("RGBA", (CARD_W + 2 * pad, CARD_H + 2 * pad), (0, 0, 0, 0))
    sh, sp = shadow(CARD_W, CARD_H, 18, blur=18, a=200)
    out.alpha_composite(sh.crop((sp - pad, sp - pad, sp - pad + out.width,
                                 sp - pad + out.height)), (0, 0))
    out.alpha_composite(img, (pad, pad))
    ImageDraw.Draw(out).rounded_rectangle([pad, pad, pad + CARD_W - 1, pad + CARD_H - 1], 18,
                                          outline=(255, 255, 255, 40), width=2)
    pill(out, chip, pad + 22, pad + CARD_H - 86, INK, MINT, size=34, weight="Bold")
    return out


def scene_hook(c, t):
    collapse = prog(t, T_COLLAPSE, T_COLLAPSE + 0.38)
    headline(c, t, 0.05, "Fresh keeps growing.", t_out=T_COLLAPSE)
    n = sum(1 for a in ARRIVE if t >= a + 0.12)
    if n:
        ca = 1 - prog(t, T_COLLAPSE, T_COLLAPSE + 0.2)
        f = font(DISPLAY.format("Black"), 92)
        put(c, f"{n}", f, MINT, 1020, 66, anchor="r", alpha=ca)
        put(c, "features & counting", font(INTER.format("Medium"), 26), MUTE, 1020, 174,
            anchor="r", alpha=ca)
    visible = [i for i, a in enumerate(ARRIVE) if t >= a]
    for i in visible[-6:]:
        a = prog(t, ARRIVE[i], ARRIVE[i] + 0.32)
        rot, jx, jy, (dx, dy) = CARD_POSE[i]
        e = out_back(a, 1.2)
        x = PILE_C[0] + jx + dx * 1100 * (1 - e)
        y = PILE_C[1] + jy + dy * 1100 * (1 - e)
        r = rot + (1 - e) * 18 * (1 if dx >= 0 else -1)
        s = lerp(1.15, 1.0, out_cubic(a))
        theme_idx = int((t - ARRIVE[i]) * FPS / 3) % len(THEMES) if FEATURES[i][1] == "THEMES" else 0
        im = card(i, theme_idx)
        later = sum(prog(t, ARRIVE[j], ARRIVE[j] + 0.3) for j in visible if j > i)
        if collapse > 0:
            k = in_cubic(collapse)
            s *= (1 - k) * 1 + 0.001
            x, y = lerp(x, 540, k), lerp(y, 560, k)
            r += k * 40
        sc = 0.95 * s
        if sc < 0.01:
            continue
        im = im.resize((int(im.width * sc), int(im.height * sc)), Image.BILINEAR)
        im = im.rotate(r, resample=Image.BICUBIC, expand=True)
        dim = min(0.62, 0.2 * later)
        if dim > 0:
            ov = Image.new("RGBA", im.size, INK + (int(255 * dim),))
            ov.putalpha(Image.eval(im.getchannel("A"), lambda v: int(v * dim)))
            im = Image.alpha_composite(im, ov)
        c.alpha_composite(im, (int(x - im.width / 2), int(y - im.height / 2)))


T_TURN = T_COLLAPSE + 0.3
T_PROOF = T_TURN + 1.75


def scene_turn(c, t):
    u = t - T_TURN
    out = prog(t, T_PROOF - 0.25, T_PROOF)
    blink = (int(u * 2.4) % 2 == 0) or u > 1.2
    a1 = out_cubic(prog(u, 0.15, 0.5)) * (1 - out)
    a2 = out_cubic(prog(u, 0.6, 0.95)) * (1 - out)
    put(c, "Still light.", font(SERIF_I, 170), PAPER, 540, 330, anchor="c", alpha=a1,
        rise=(1 - a1) * 40)
    put(c, "Still fast.", font(SERIF_I, 170), MINT, 540, 540, anchor="c", alpha=a2,
        rise=(1 - a2) * 40)
    if blink and out < 1:
        d = ImageDraw.Draw(c)
        d.rectangle([522, 790, 558, 850], fill=MINT + (int(255 * (1 - out)),))


# --- proof: the launch, timed
P = {"type0": 0.35, "type1": 1.15, "enter": 1.45}
BOOT = CAPS["launch"]["frames"]["boot"]
BOOT_T = [f["t"] - BOOT[0]["t"] for f in BOOT]
T_SIMPLE = T_PROOF + 4.6


def launch_screen(u):
    if u < P["type0"]:
        return screen("launch", "sh0")
    if u < P["enter"]:
        k = min(13, int(prog(u, P["type0"], P["type1"]) * 14))
        return screen("launch", f"sh_t{k:02d}")
    dt = u - P["enter"]
    for i in range(len(BOOT) - 1, -1, -1):
        if BOOT_T[i] <= dt:
            if "Ln 1" in BOOT[-1]["ans"] and i == len(BOOT) - 1:
                return screen("launch", "ed_source")
            return screen("launch", "boot", i)
    return screen("launch", "sh_t13")


def boot_done(u):
    dt = u - P["enter"]
    return any(BOOT_T[i] <= dt and "Ln 1" in BOOT[i]["ans"] for i in range(len(BOOT)))


def scene_proof(c, t):
    u = t - T_PROOF
    # the window opens out of the cursor
    o = out_cubic(prog(u, 0, 0.4))
    x, y, w, h = PANEL
    bx = (lerp(522, x, o), lerp(790, y, o), lerp(36, w, o), lerp(60, h, o))
    close = in_cubic(prog(t, T_SIMPLE - 0.2, T_SIMPLE))
    zin = view_of(450, 220, 900, ASPECT)
    z = in_out(prog(u, P["enter"] + 0.25, P["enter"] + 0.75))
    v = mix_view(zin, FULL, z)
    pic = shot(launch_screen(u), v, int(w), int(h))
    headline(c, t, T_PROOF + 0.1, "Opens in a blink.",
             "fresh notes.md, from a shell", t_out=T_SIMPLE - 0.25)
    panel(c, pic, bx, alpha=1 - close * 0.0)
    # the claim: qualitative, since the first frame varies by build and
    # machine; what was measured is that it no longer grows with the number
    # of saved workspaces (see README)
    done_u = next((uu for uu in [P["enter"] + k / FPS for k in range(60)] if boot_done(uu)),
                  P["enter"] + 0.2)
    sa = prog(u, done_u, done_u + 0.3)
    if sa > 0:
        fade = 1 - prog(u, 4.15, 4.45)
        sc = out_back(sa, 2.2)
        cx, cy = 540, 560
        bw, bh = 740 * sc, 280 * sc
        if bw > 8:
            card_im = Image.new("RGBA", (740, 280), (0, 0, 0, 0))
            d = ImageDraw.Draw(card_im)
            d.rounded_rectangle([0, 0, 739, 279], 36, fill=INK + (238,),
                                outline=MINT + (255,), width=3)
            f = font(DISPLAY.format("Black"), 118)
            put(card_im, "Starts fast", f, MINT, 370, 42, anchor="c")
            put(card_im, "even with hundreds of workspaces",
                font(INTER.format("Medium"), 36), PAPER, 370, 196, anchor="c")
            card_im = card_im.resize((int(bw), int(bh)), Image.LANCZOS)
            if fade < 1:
                card_im.putalpha(card_im.getchannel("A").point(lambda q: int(q * fade)))
            c.alpha_composite(card_im, (int(cx - bw / 2), int(cy - bh / 2)))
    chips = [("1 process for fresh FILE", 2.75)]
    widths = []
    for s, at in chips:
        f = font(INTER.format("Bold"), 34)
        l, tt, r, b = f.getbbox(s)
        widths.append(r - l + 34 * 1.3 + 4)
    gap = 18
    x0 = 540 - (sum(widths) + gap * (len(widths) - 1)) / 2
    for (s, at), wd in zip(chips, widths):
        a = prog(u, at, at + 0.3)
        fade = 1 - prog(u, 4.15, 4.45)
        if a > 0:
            pill(c, s, x0 + wd / 2, 830 - (1 - out_cubic(a)) * 30, INK, MINT,
                 size=34, weight="Bold", alpha=a * fade, scale=lerp(0.7, 1, out_back(a)),
                 anchor="c")
        x0 += wd + gap


T_MD = T_SIMPLE + 3.4


def scene_simple(c, t):
    v = t - T_SIMPLE
    headline(c, t, T_SIMPLE + 0.05, "Just the file you asked for.",
             "0.5.4: no Welcome tab, dock stays closed, your session untouched",
             t_out=T_MD - 0.25)
    x, y, w, h = PANEL
    old = shot(screen("sess-v053", "v053_file"), FULL, w, h)
    new = shot(screen("sess-v054", "v054_file"), FULL, w, h)
    k = prog(v, 1.35, 1.85)
    pic = wipe(old, new, k)
    enter = out_cubic(prog(v, 0, 0.35))
    panel(c, pic, (x + (1 - enter) * 60, y, w, h), alpha=enter)
    la = enter * (1 - prog(v, 1.35, 1.5))
    pill(c, "0.5.3 · fresh notes.md", x + 24, y + h - 92, INK, OLD, size=30, weight="Bold",
         alpha=la)
    lb = prog(v, 1.75, 2.0)
    if lb > 0:
        pill(c, "0.5.4 · fresh notes.md", x + 24, y + h - 92, INK, MINT, size=30,
             weight="Bold", alpha=lb, scale=lerp(0.8, 1, out_back(lb)))
    tag = prog(v, 2.3, 2.6) * (1 - prog(t, T_MD - 0.25, T_MD))
    if tag > 0:
        pill(c, "made for $EDITOR & git commit", x + w - 24, y + h - 92, PAPER, INK,
             size=30, weight="SemiBold", alpha=tag, anchor="r", outline=MINT + (255,))


T_OUT = T_MD + 4.4
CLICK = CAPS["launch"]["frames"]["click_at"]


def cell_px(col, row):
    return ((col + 0.5) * PAINTER.cw, (row + 0.5) * PAINTER.ch)


def scene_md(c, t):
    m = t - T_MD
    headline(c, t, T_MD + 0.05, "New: Markdown, three ways.", t_out=T_OUT - 0.2)
    x, y, w, h = PANEL
    zoom = view_of(SW - 480, SH - 260, 960, ASPECT)
    left = view_of(480, SH - 260, 960, ASPECT)
    zi = in_out(prog(m, 0.0, 0.5))
    zp = in_out(prog(m, 0.98, 1.3))
    zo = in_out(prog(m, 1.72, 2.15))
    v = mix_view(mix_view(mix_view(FULL, zoom, zi), left, zp), FULL, zo)
    if m < 0.95:
        scr = screen("launch", "ed_source")
    elif m < 1.15:
        scr = screen("launch", "pal_open")
    elif m < 1.75:
        k = min(9, int(prog(m, 1.2, 1.6) * 10))
        scr = screen("launch", f"pal_t{k:02d}")
    else:
        scr = None
    if scr is not None:
        pic = shot(scr, v, w, h)
    else:
        src = shot(screen("launch", "ed_source"), FULL, w, h)
        ren = shot(screen("launch", "md_rendered"), FULL, w, h)
        dual = shot(screen("launch", "md_dual"), FULL, w, h)
        pic = wipe(src, ren, prog(m, 1.75, 2.15))
        if m >= 2.95:
            pic = wipe(ren, dual, prog(m, 2.95, 3.35))
    out = in_cubic(prog(t, T_OUT - 0.2, T_OUT + 0.15))
    panel(c, pic, (x + w * 0.5 * out * 0.2, y + h * out * 0.1, w * (1 - out * 0.2),
                   h * (1 - out * 0.2)), alpha=1 - out)
    # the pointer, travelling to the status-bar hint and clicking it
    if 0.2 < m < 1.05:
        px, py = cell_px(CLICK["col"], CLICK["row"])
        x0, y0, x1, y1 = v
        tx = x + (px - x0) / (x1 - x0) * w
        ty = y + (py - y0) / (y1 - y0) * h
        k = in_out(prog(m, 0.2, 0.75))
        sx, sy = lerp(560, tx, k), lerp(560, ty, k)
        if 0.78 < m < 1.05:
            r = 10 + 60 * prog(m, 0.78, 1.05)
            a = int(220 * (1 - prog(m, 0.78, 1.05)))
            ImageDraw.Draw(c).ellipse([sx - r, sy - r, sx + r, sy + r], outline=MINT + (a,),
                                      width=5)
        c.alpha_composite(pointer_img(), (int(sx - 4), int(sy - 4)))
    # the mode row
    modes = ["Source", "Rendered", "Side by side"]
    cur = 0 if m < 1.95 else (1 if m < 3.15 else 2)
    ra = prog(m, 1.7, 2.0) * (1 - prog(t, T_OUT - 0.25, T_OUT))
    if ra > 0:
        xx = 62
        for i, s in enumerate(modes):
            on = i == cur
            xx += pill(c, s, xx, 160, INK if on else MUTE, MINT if on else (30, 32, 36),
                       size=28, weight="Bold", alpha=ra) + 12
    if m < 1.7:
        a = prog(m, 0.1, 0.4) * (1 - prog(m, 1.55, 1.7))
        put(c, "click the palette hint (new in 0.5.4) → Cycle View Mode",
            font(INTER.format("Regular"), 30), MUTE, 62, 168, alpha=a)


T_END = T_OUT + 3.1


def scene_out(c, t):
    u = t - T_OUT
    a = out_cubic(prog(u, 0.22, 0.6))
    logo_f = font(MONO_B, 150)
    word_f = font(DISPLAY.format("Bold"), 150)
    lw = font(MONO_B, 150).getlength(">_")
    ww = word_f.getlength("fresh")
    total = lw + 36 + ww
    x0 = 540 - total / 2
    put(c, ">_", logo_f, MINT, x0, 300, alpha=a, rise=(1 - a) * 30)
    put(c, "fresh", word_f, PAPER, x0 + lw + 36, 296, alpha=a, rise=(1 - a) * 30)
    b = out_cubic(prog(u, 0.55, 0.95))
    put(c, "Every feature.", font(SERIF_I, 88), PAPER, 540, 530, anchor="c", alpha=b,
        rise=(1 - b) * 20)
    b2 = out_cubic(prog(u, 0.85, 1.25))
    put(c, "Still light.", font(SERIF_I, 88), MINT, 540, 630, anchor="c", alpha=b2,
        rise=(1 - b2) * 20)
    d = out_cubic(prog(u, 1.3, 1.7))
    pill(c, "0.5.4 · coming soon", 540, 830, INK, SAGE, size=34, weight="Bold", alpha=d,
         anchor="c", scale=lerp(0.8, 1, out_back(d)))
    put(c, "getfresh.dev", font(INTER.format("SemiBold"), 38), PAPER, 540, 905, anchor="c",
        alpha=d)


def frame(t):
    c = Image.new("RGBA", (W, H), INK + (255,))
    # a faint grain of light at the top so the ink is not a flat void
    c.alpha_composite(GLOW)
    if t < T_TURN + 0.2:
        scene_hook(c, t)
    if T_TURN <= t < T_PROOF + 0.05:
        scene_turn(c, t)
    if T_PROOF <= t < T_SIMPLE:
        scene_proof(c, t)
    if T_SIMPLE - 0.05 <= t < T_MD:
        scene_simple(c, t)
    if T_MD <= t < T_OUT + 0.2:
        scene_md(c, t)
    if t >= T_OUT:
        scene_out(c, t)
    # fade in from and out to ink
    edge = max(1 - prog(t, 0, 0.2), prog(t, T_END - 0.35, T_END))
    if edge > 0:
        c.alpha_composite(Image.new("RGBA", (W, H), INK + (int(255 * edge),)))
    return c.convert("RGB")


def make_glow():
    g = Image.new("RGBA", (W // 4, H // 4), (0, 0, 0, 0))
    d = ImageDraw.Draw(g)
    d.ellipse([-40, -120, W // 4 + 40, 90], fill=SAGE + (16,))
    return g.filter(ImageFilter.GaussianBlur(24)).resize((W, H), Image.BICUBIC)


GLOW = make_glow()


def main():
    out = sys.argv[1]
    args = sys.argv[2:]
    if "--still" in args:
        ts = [float(a) for a in args[args.index("--still") + 1:]]
        for tt in ts:
            frame(tt).save(f"{out}-{tt:05.2f}.png")
        return
    n = int(round(T_END * FPS))
    print(f"{n} frames, {T_END:.2f}s", file=sys.stderr)
    ff = subprocess.Popen(
        ["ffmpeg", "-y", "-loglevel", "error", "-f", "rawvideo", "-pix_fmt", "rgb24",
         "-s", f"{W}x{H}", "-r", str(FPS), "-i", "-", "-c:v", "libx264", "-preset", "slow",
         "-crf", "17", "-pix_fmt", "yuv420p", "-movflags", "+faststart", out],
        stdin=subprocess.PIPE)
    for i in range(n):
        ff.stdin.write(frame(i / FPS).tobytes())
        if i % 60 == 0:
            print(f"  {i}/{n}", file=sys.stderr)
    ff.stdin.close()
    ff.wait()


if __name__ == "__main__":
    main()
