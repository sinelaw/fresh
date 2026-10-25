"""Paint a captured terminal screen (SGR text + cursor) as an image.

Box drawing and block elements are drawn as rectangles rather than glyphs, so
frames and rules join seamlessly at any scale; emoji come from Noto Color
Emoji (a bitmap font, rendered at its one size and scaled into two cells).
"""
import re
from functools import lru_cache

from PIL import Image, ImageDraw, ImageFont
from wcwidth import wcwidth

JB = "/usr/share/fonts/truetype/jetbrains-mono/JetBrainsMono-{}.ttf"
CJK = "/usr/share/fonts/opentype/noto/NotoSansCJK-Regular.ttc"
EMOJI = "/usr/share/fonts/truetype/noto/NotoColorEmoji.ttf"
DEJAVU = "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf"

DEF_FG = (192, 202, 245)
DEF_BG = (22, 22, 30)

BASE16 = [(21, 22, 30), (247, 118, 142), (158, 206, 106), (224, 175, 104),
          (122, 162, 247), (187, 154, 247), (125, 207, 255), (169, 177, 214),
          (65, 72, 104), (247, 118, 142), (158, 206, 106), (224, 175, 104),
          (122, 162, 247), (187, 154, 247), (125, 207, 255), (192, 202, 245)]


def xterm256(n):
    if n < 16:
        return BASE16[n]
    if n < 232:
        n -= 16
        lv = [0, 95, 135, 175, 215, 255]
        return (lv[n // 36], lv[(n // 6) % 6], lv[n % 6])
    v = 8 + (n - 232) * 10
    return (v, v, v)


SGR = re.compile(r"\x1b\[([0-9;:]*)m")
OTHER = re.compile(r"\x1b(\[[0-9;?]*[A-Za-z]|\][^\x07]*\x07|[()][A-Z0-9])")


def parse(ans, cols, rows):
    """-> grid[row][col] = (char, fg, bg, bold, italic, underline) or None
    for the second half of a wide character."""
    grid = []
    for line in ans.split("\n")[:rows]:
        st = {"fg": None, "bg": None, "b": False, "i": False, "u": False,
              "r": False, "d": False}
        row, pos = [], 0
        while pos < len(line):
            m = SGR.match(line, pos)
            if m:
                apply_sgr(st, m.group(1))
                pos = m.end()
                continue
            m = OTHER.match(line, pos)
            if m:
                pos = m.end()
                continue
            ch = line[pos]
            pos += 1
            w = wcwidth(ch)
            if w < 0:
                continue
            if w == 0:
                if row and row[-1] is not None:
                    c = row[-1]
                    row[-1] = (c[0] + ch,) + c[1:]
                continue
            fg, bg = st["fg"] or DEF_FG, st["bg"] or DEF_BG
            if st["r"]:
                fg, bg = bg, fg
            if st["d"]:
                fg = tuple(int(a * 0.6 + b * 0.4) for a, b in zip(fg, bg))
            row.append((ch, fg, bg, st["b"], st["i"], st["u"]))
            if w == 2:
                row.append(None)
        while len(row) < cols:
            row.append((" ", DEF_FG, st["bg"] or DEF_BG, False, False, False))
        grid.append(row[:cols])
    while len(grid) < rows:
        grid.append([(" ", DEF_FG, DEF_BG, False, False, False)] * cols)
    return grid


def apply_sgr(st, params):
    ps = [p for p in re.split("[;]", params)] if params else ["0"]
    i = 0
    while i < len(ps):
        p = ps[i]
        if ":" in p:                       # 4:3 curly underline etc.
            head = p.split(":")[0]
            if head == "4":
                st["u"] = p != "4:0"
            i += 1
            continue
        n = int(p) if p else 0
        if n == 0:
            st.update(fg=None, bg=None, b=False, i=False, u=False, r=False, d=False)
        elif n == 1: st["b"] = True
        elif n == 2: st["d"] = True
        elif n == 3: st["i"] = True
        elif n == 4: st["u"] = True
        elif n == 7: st["r"] = True
        elif n == 22: st["b"] = st["d"] = False
        elif n == 23: st["i"] = False
        elif n == 24: st["u"] = False
        elif n == 27: st["r"] = False
        elif 30 <= n <= 37: st["fg"] = BASE16[n - 30]
        elif 90 <= n <= 97: st["fg"] = BASE16[n - 90 + 8]
        elif 40 <= n <= 47: st["bg"] = BASE16[n - 40]
        elif 100 <= n <= 107: st["bg"] = BASE16[n - 100 + 8]
        elif n == 39: st["fg"] = None
        elif n == 49: st["bg"] = None
        elif n in (38, 48, 58):
            key = {38: "fg", 48: "bg", 58: None}[n]
            if i + 1 < len(ps) and ps[i + 1] == "5":
                c = xterm256(int(ps[i + 2])); i += 2
            elif i + 1 < len(ps) and ps[i + 1] == "2":
                c = tuple(int(x) for x in ps[i + 2:i + 5]); i += 4
            else:
                c = None
            if key:
                st[key] = c
        i += 1


class Painter:
    def __init__(self, cell_w=24):
        self.fs = cell_w / 0.6
        self.cw = cell_w
        self.ch = round(self.fs * 1.32)
        f = lambda s: ImageFont.truetype(JB.format(s), round(self.fs))
        self.fonts = {(False, False): f("Regular"), (True, False): f("Bold"),
                      (False, True): f("Italic"), (True, True): f("BoldItalic")}
        self.cjk = ImageFont.truetype(CJK, round(self.fs))
        self.dejavu = ImageFont.truetype(DEJAVU, round(self.fs))
        self.emoji = ImageFont.truetype(EMOJI, 109)
        asc, desc = self.fonts[(False, False)].getmetrics()
        self.base = (self.ch - (asc + desc)) // 2

    @lru_cache(maxsize=512)
    def emoji_img(self, ch):
        im = Image.new("RGBA", (160, 160), (0, 0, 0, 0))
        ImageDraw.Draw(im).text((0, 0), ch, font=self.emoji, embedded_color=True)
        bb = im.getbbox() or (0, 0, 1, 1)
        im = im.crop(bb)
        s = min(2 * self.cw * 0.92 / im.width, self.ch * 0.92 / im.height)
        return im.resize((max(1, int(im.width * s)), max(1, int(im.height * s))),
                         Image.LANCZOS)

    @lru_cache(maxsize=None)
    def cmap(self, path):
        from fontTools.ttLib import TTFont
        return set(TTFont(path, fontNumber=0).getBestCmap())

    def has_glyph(self, font, ch):
        return ord(ch) in self.cmap(font.path)

    def paint(self, frame, cols, rows, cursor=True):
        cw, ch = self.cw, self.ch
        grid = parse(frame["ans"], cols, rows)
        img = Image.new("RGB", (cols * cw, rows * ch), DEF_BG)
        d = ImageDraw.Draw(img)
        # backgrounds
        for y, row in enumerate(grid):
            for x, c in enumerate(row):
                if c is None:
                    continue
                w = 2 if x + 1 < cols and row[x + 1] is None else 1
                d.rectangle([x * cw, y * ch, (x + w) * cw - 1, (y + 1) * ch - 1], fill=c[2])
        # glyphs
        for y, row in enumerate(grid):
            for x, c in enumerate(row):
                if c is None or c[0] == " ":
                    continue
                self.glyph(img, d, x, y, c)
        if cursor and frame.get("cv"):
            x, y = frame["cx"], frame["cy"]
            if 0 <= y < rows and 0 <= x < cols:
                d.rectangle([x * cw, y * ch + 2, x * cw + max(2, cw // 8), (y + 1) * ch - 3],
                            fill=(230, 230, 230))
        return img

    def glyph(self, img, d, x, y, c):
        chs, fg, bg, b, it, u = c
        ch0 = chs[0]
        X, Y, cw, chh = x * self.cw, y * self.ch, self.cw, self.ch
        if self.box(d, ch0, X, Y, fg):
            pass
        elif (ord(ch0) in self.cmap(EMOJI) and ord(ch0) > 0x2000
              and not self.has_glyph(self.fonts[(False, False)], ch0)
              and (wcwidth(ch0) == 2 or "\ufe0f" in chs)):
            e = self.emoji_img(chs.replace("️", "") or ch0)
            img.paste(e, (X + (2 * cw - e.width) // 2, Y + (chh - e.height) // 2), e)
        else:
            font = self.fonts[(b, it)]
            if not self.has_glyph(font, ch0):
                font = self.cjk if wcwidth(ch0) == 2 else self.dejavu
                if not self.has_glyph(font, ch0):
                    font = self.cjk
            w = 2 if wcwidth(ch0) == 2 else 1
            adv = font.getlength(ch0)
            dx = (w * cw - adv) / 2
            d.text((X + dx, Y + self.base), chs, font=font, fill=fg)
        if u:
            d.rectangle([X, Y + chh - max(2, chh // 14) - 1, X + cw - 1, Y + chh - 2], fill=fg)

    def box(self, d, c, X, Y, fg):
        cw, ch = self.cw, self.ch
        t = max(2, round(cw / 10))
        cx, cy = X + cw // 2, Y + ch // 2
        L = lambda: d.rectangle([X, cy - t // 2, cx + t // 2, cy - t // 2 + t - 1], fill=fg)
        R = lambda: d.rectangle([cx - t // 2, cy - t // 2, X + cw - 1, cy - t // 2 + t - 1], fill=fg)
        U = lambda: d.rectangle([cx - t // 2, Y, cx - t // 2 + t - 1, cy + t // 2], fill=fg)
        D = lambda: d.rectangle([cx - t // 2, cy - t // 2, cx - t // 2 + t - 1, Y + ch - 1], fill=fg)
        parts = {"─": "LR", "━": "LR", "│": "UD", "┃": "UD", "┌": "RD", "┐": "LD",
                 "└": "RU", "┘": "LU", "├": "UDR", "┤": "UDL", "┬": "LRD",
                 "┴": "LRU", "┼": "LRUD", "╭": "RD", "╮": "LD", "╰": "RU", "╯": "LU"}
        if c in parts:
            for p in parts[c]:
                {"L": L, "R": R, "U": U, "D": D}[p]()
            return True
        blocks = {"█": (0, 0, 1, 1), "▌": (0, 0, .5, 1), "▐": (.5, 0, 1, 1),
                  "▀": (0, 0, 1, .5), "▄": (0, .5, 1, 1), "▏": (0, 0, .125, 1),
                  "▕": (.875, 0, 1, 1), "▁": (0, .875, 1, 1), "▔": (0, 0, 1, .125)}
        if c in blocks:
            a, b, e, f = blocks[c]
            d.rectangle([X + a * cw, Y + b * ch, X + e * cw - 1, Y + f * ch - 1], fill=fg)
            return True
        return False
