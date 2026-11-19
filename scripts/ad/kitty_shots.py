#!/usr/bin/env python3
"""Landing-page stills, photographed from Fresh running in a real kitty window.

    python3 scripts/ad/kitty_shots.py              # every still
    python3 scripts/ad/kitty_shots.py palette ...  # just these

Each still replays one of rec.py's scripted clips into a kitty window of its
own size on a private Xvfb display (see kitty.py) and photographs the whole
window at the given moment, status bar and all: the current build drawing
itself in kitty with JetBrainsMono Nerd Font, in the Quiet theme (Settings in
High Contrast) with Nerd Font icons on. The theme crossfade is one still per
theme of the same file. The Orchestrator loop and still, and the Review Diff
still, come from kitty_orchestrator.py.

Run setup.sh first (demo repo, agents, the 2 GB log). PNGs land in
$AD_WORK/kshots; WebPs in homepage/public/assets/shots/.
"""
import json
import os
import sys
import time

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import rec                                   # noqa: E402  (clip timelines, demo state)
from kitty import Display, Kitty             # noqa: E402

S, DEMO, REPO = rec.S, rec.DEMO, rec.REPO
OUT = os.path.join(REPO, "homepage", "public", "assets", "shots")
PNG = os.path.join(S, "kshots")
THEME = os.environ.get("AD_THEME", "quiet")

# still: (clip, recording time, terminal size in cells (cols, rows)[, theme]).
# Each still is the whole kitty window at that size, nothing cropped, so the
# menu, tab bar and status bar are all there. The Settings editor reads best
# in High Contrast. (Review Diff comes from kitty_orchestrator.py's scene.)
SHOTS = {
    "settings":    ("settings", 16.6, (132, 42), "high-contrast"),
    "multicursor": ("multicursor", 17.2, (84, 28)),
    "huge-file":   ("huge", 10.4, (96, 30)),
    "palette":     ("palette", 8.9, (100, 30)),
}
# one still per theme for the crossfade, Quiet first: server.rs, settled
THEMES = ["quiet", "tokyo-night", "gruvbox", "nord", "light", "dracula", "solarized-dark"]
THEME_SIZE = (96, 30)


def kitty_config(base, name, theme):
    """A copy of setup.sh's config dir with the theme and Nerd Font icons."""
    x = os.path.join(S, "xdg")
    with open(os.path.join(x, base, "fresh", "config.json")) as f:
        cfg = json.load(f)
    cfg["theme"] = f"builtin://{theme}"
    cfg.setdefault("editor", {})["nerd_font_icons"] = True
    os.makedirs(os.path.join(x, name, "fresh"), exist_ok=True)
    with open(os.path.join(x, name, "fresh", "config.json"), "w") as f:
        json.dump(cfg, f, indent=1)
    return name


def clip_still():
    """server.rs, open and settled: the theme crossfade's frame."""
    return [rec.FRESH_ARGS, "src/server.rs"], rec.T().wait(4.0)


def argv_for(argv):
    if argv[0] == rec.FRESH_ARGS:
        return [rec.FRESH, "--no-upgrade-check", "--no-restore"] + argv[1:]
    if argv[0] == rec.FRESH_BARE:
        return [rec.FRESH, "--no-upgrade-check"]
    return ["bash", "--norc", "--noprofile", "-i"]


def play(d, clip_fn, shots, size, theme=THEME):
    """Run one clip in a kitty window of `size` cells and save
    `shots` = [(time, png path)], each the whole window."""
    res = clip_fn()
    argv, tl = res[0], res[1]
    cfg = res[3] if len(res) > 3 else "config"
    rec.reset_state()
    for rel, text in (res[4] if len(res) > 4 else {}).items():
        path = os.path.join(S, "xdg", rel)
        os.makedirs(os.path.dirname(path), exist_ok=True)
        with open(path, "w") as f:
            f.write(text)
    env = rec.env_for(kitty_config(cfg, f"{cfg}-kitty", theme))
    cols, rows = size
    env.update({"LANG": env.get("LANG") or "C.UTF-8", "COLUMNS": str(cols), "LINES": str(rows)})
    k = Kitty(d, argv_for(argv), cwd=DEMO, env=env, cols=cols, rows=rows)
    sched, t = [], 0.0
    for dt, kind, p in tl.ev:
        t += dt
        if kind:
            sched.append((t, p[0]))
    pending = sorted(shots)
    start = time.monotonic()
    i = 0
    try:
        while pending:
            el = time.monotonic() - start
            while i < len(sched) and sched[i][0] <= el:
                k.send(sched[i][1])
                i += 1
            if pending[0][0] <= el:
                _, path = pending.pop(0)
                print(f"  {os.path.basename(path)} {k.shot(path)}")
                continue
            nxt = min(sched[i][0] if i < len(sched) else 1e9, pending[0][0])
            time.sleep(max(0.002, min(0.02, nxt - el)))
    finally:
        k.close()
        os.system("pkill -9 -f 'target/debug/fresh' 2>/dev/null; pkill -9 -f coding_agent.py 2>/dev/null")


def to_webp(png, name):
    from PIL import Image
    im = Image.open(png).convert("RGB")
    # dense screens (the log) compress poorly; keep them a little smaller
    width, q = (1600, 80) if name == "huge-file" else (1600, 84) if name.startswith("theme-") else (2000, 88)
    if im.width > width:
        im = im.resize((width, round(im.height * width / im.width)), Image.LANCZOS)
    os.makedirs(OUT, exist_ok=True)
    im.save(os.path.join(OUT, name + ".webp"), "WEBP", quality=q, method=6)
    print(f"  -> {name}.webp {im.size}")


def main(names):
    os.makedirs(PNG, exist_ok=True)
    want = set(names) or set(SHOTS) | {"theme-" + t for t in THEMES}
    with Display() as d:
        for name, (clip, t, size, *theme) in SHOTS.items():
            if name in want:
                theme = theme[0] if theme else THEME
                print(f"{name}: {clip} at {size[0]}x{size[1]} ({theme})")
                p = os.path.join(PNG, name + ".png")
                play(d, rec.CLIPS[clip], [(t, p)], size, theme=theme)
                to_webp(p, name)
        for theme in THEMES:
            name = "theme-" + theme
            if name in want:
                print(f"{name}:")
                p = os.path.join(PNG, name + ".png")
                play(d, clip_still, [(4.0, p)], THEME_SIZE, theme=theme)
                to_webp(p, name)


if __name__ == "__main__":
    main(sys.argv[1:])
