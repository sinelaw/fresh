#!/usr/bin/env python3
"""Film the screens the video is cut from. Writes $WORK/caps/<scene>.json.

    capture.py launch            # shell -> fresh notes.md -> palette -> modes
    capture.py montage           # one full session, feature by feature
    capture.py session TAG BIN   # a lived-in session, then `fresh notes.md`

fresh runs in a detached tmux pane; every screen is kept as text with its SGR
attributes, and ansirender.py paints it at whatever size the cut needs.
"""
import os, shutil, subprocess, sys, time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from tmuxcam import Cam

REPO = os.path.abspath(os.path.join(os.path.dirname(__file__), "../../.."))
WORK = os.environ.get("CLIP_WORK", os.path.join(REPO, "target/clips/social-0.5.4"))
DEMO = os.path.join(WORK, "demo")
COLS, ROWS = 100, 36      # narrower and the status bar drops the palette hint
BIN = os.environ.get("FRESH_BIN", os.path.join(REPO, "target/release"))


def fresh_env(tag):
    x = os.path.join(WORK, "xdg-" + tag)
    shutil.rmtree(x, ignore_errors=True)
    for d in ("config/fresh", "data", "state", "cache", "run", "home"):
        os.makedirs(os.path.join(x, d))
    os.chmod(os.path.join(x, "run"), 0o700)
    with open(os.path.join(x, "config/fresh/config.json"), "w") as f:
        f.write('{"theme": "tokyo-night", "check_for_updates": false}\n')
    return {"XDG_CONFIG_HOME": x + "/config", "XDG_DATA_HOME": x + "/data",
            "XDG_STATE_HOME": x + "/state", "XDG_CACHE_HOME": x + "/cache",
            "XDG_RUNTIME_DIR": x + "/run", "HOME": x + "/home",
            "PATH": f"{BIN}:/usr/bin:/bin", "LANG": "C.UTF-8", "LC_ALL": "C.UTF-8",
            "COLORTERM": "truecolor", "SHELL": "/bin/bash", "TERM": "tmux-256color",
            "PS1": "\\[\\e[38;2;122;162;247m\\]~/demo\\[\\e[0m\\] "
                   "\\[\\e[38;2;158;206;106m\\]❯\\[\\e[0m\\] "}


def palette(c, cmd, wait=1.2):
    c.keys("C-p", settle=0.5)
    c.text(cmd, settle=0.6)
    c.keys("Enter", settle=wait)


def palette_col(c):
    last = [l for l in c.plain().split("\n") if l.strip()][-1]
    return last.index("Palette") + 3, ROWS - 1


def save(c, name):
    os.makedirs(os.path.join(WORK, "caps"), exist_ok=True)
    c.save(os.path.join(WORK, "caps", name + ".json"))


scene = sys.argv[1]
if scene == "session":
    tag, BIN = sys.argv[2], os.path.abspath(sys.argv[3])

if scene == "launch":
    c = Cam(COLS, ROWS, DEMO, fresh_env("launch"))
    c.keys("C-l", settle=0.4)
    c.shot("sh0")
    for i, ch in enumerate("fresh notes.md"):
        c.text(ch, settle=0.05)
        c.shot(f"sh_t{i:02d}")
    c.keys("Enter", settle=0)
    c.burst("boot", 1.0, fps=60)
    c.wait_for("Ln 1")
    time.sleep(1.5)
    c.shot("ed_source")
    col, row = palette_col(c)
    c.frames["click_at"] = {"col": col, "row": row}
    c.click(col, row, settle=0.8)
    c.shot("pal_open")
    for i, ch in enumerate("cycle view"):
        c.text(ch, settle=0.08)
        c.shot(f"pal_t{i:02d}")
    c.keys("Enter", settle=1.5)
    c.shot("md_rendered")
    palette(c, "cycle view", wait=1.8)
    c.shot("md_dual")
    palette(c, "Quit", wait=1.0)
    c.shot("sh_back")
    save(c, "launch")
    c.close()

elif scene == "montage":
    c = Cam(COLS, ROWS, DEMO, fresh_env("montage"),
            cmd="fresh --no-upgrade-check")
    time.sleep(4)
    c.keys("C-o", settle=0.6); c.text("main.rs", settle=0.5); c.keys("Enter", settle=1.2)
    c.shot("code")
    c.keys("C-g", settle=0.5); c.text("8", settle=0.3); c.keys("Enter", settle=0.5)
    c.keys("Home", settle=0.3)
    for _ in range(4):
        c.keys("C-d", settle=0.35)
    c.shot("multicursor")
    c.keys("Escape", settle=0.4)
    c.keys("C-p", settle=0.8)
    c.shot("palette")
    c.keys("Escape", settle=0.5)
    palette(c, "Toggle File Explorer"); c.shot("explorer")
    palette(c, "Toggle File Explorer")
    palette(c, "Split Vertical")
    c.keys("C-o", settle=0.6); c.text("lib.rs", settle=0.5); c.keys("Enter", settle=1.2)
    c.shot("split")
    palette(c, "Close Split")
    palette(c, "Live Diff: vs HEAD", wait=2.0); c.shot("livediff")
    palette(c, "Select Theme"); c.shot("theme_pick")
    c.keys("Escape", settle=0.5)
    palette(c, "Review Diff", wait=2.5); c.shot("review")
    palette(c, "Review Diff: Stop", wait=1.0)
    palette(c, "Git Log", wait=2.0); c.shot("gitlog")
    palette(c, "Git Log: Close", wait=1.0)
    palette(c, "Open Settings", wait=1.5); c.shot("settings")
    c.keys("Escape", settle=0.8)
    palette(c, "Open Keybinding Editor", wait=1.5); c.shot("keys")
    c.keys("Escape", settle=0.8)
    palette(c, "Orchestrator: Toggle Dock", wait=2.0); c.shot("dock")
    palette(c, "Orchestrator: Toggle Dock", wait=1.0)
    for th in ("nord", "gruvbox", "light", "dracula", "solarized-dark", "tokyo-night"):
        palette(c, "Select Theme", wait=0.6)
        c.keys("C-a", settle=0.1); c.keys("BSpace", settle=0.1)
        c.text(th, settle=0.4); c.keys("Enter", settle=1.0)
        c.shot("theme_" + th.replace("-", "_"))
    palette(c, "Open Terminal", wait=2.0)
    c.text("ls", settle=0.2); c.keys("Enter", settle=0.8); c.shot("terminal")
    save(c, "montage")
    c.close()

elif scene == "session":
    # 1. a session somebody has been working in. The dock is left alone: a
    #    dock the user opened is remembered as `always`, and a file launch
    #    honours that; under the default `auto` it keeps the dock closed.
    env = fresh_env("sess-" + tag)
    c = Cam(COLS, ROWS, DEMO, env, cmd="fresh --no-upgrade-check")
    time.sleep(4)
    for f in ("main.rs", "lib.rs"):
        c.keys("C-o", settle=0.6); c.text(f, settle=0.4); c.keys("Enter", settle=1.0)
    palette(c, "Quit", wait=2.5)
    c.close()
    subprocess.run(["pkill", "-9", "-f", os.path.join(BIN, "fresh")])
    time.sleep(1)
    # 2. then the file launch, the way $EDITOR or git would make it
    c = Cam(COLS, ROWS, DEMO, env)
    c.keys("C-l", settle=0.4)
    c.text("fresh notes.md", settle=0.1)
    c.keys("Enter", settle=0)
    c.wait_for("Ln ")
    time.sleep(3)
    c.shot(tag + "_file")
    save(c, "sess-" + tag)
    palette(c, "Quit", wait=1.5)
    c.close()
    subprocess.run(["pkill", "-9", "-f", os.path.join(BIN, "fresh")])
