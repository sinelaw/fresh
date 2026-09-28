#!/usr/bin/env python3
"""Record scripted Fresh clips for the ad as asciicast v2 (+ key/mouse marker events).

Usage: rec.py CLIP [CLIP...]   (see CLIPS below)

Marker events written besides "o":
  [t, "k", "Ctrl+D"]   key label to flash as a chip
  [t, "m", "x,y,btn"]  mouse pointer position (0-based cells), btn 1 while pressed
"""
import codecs
import fcntl
import json
import os
import pty
import select
import struct
import sys
import termios
import time

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.dirname(os.path.dirname(HERE))
S = os.environ.get("AD_WORK", os.path.join(REPO, "target", "ad"))   # setup.sh fills this
DEMO = os.path.join(S, "demo")
FRESH = os.environ.get("FRESH", os.path.join(REPO, "target", "debug", "fresh"))
# One uniform size for every clip; the compositor zooms/crops for legibility.
COLS, ROWS = 140, 75

ESC = "\x1b"
KEYS = {
    "Enter": "\r", "Esc": ESC, "Tab": "\t", "BS": "\x7f",
    "Up": ESC + "[A", "Down": ESC + "[B", "Right": ESC + "[C", "Left": ESC + "[D",
    "Home": ESC + "[H", "End": ESC + "[F", "PgDn": ESC + "[6~", "PgUp": ESC + "[5~",
    "Ctrl+Home": ESC + "[1;5H", "Ctrl+End": ESC + "[1;5F",
    "Ctrl+Right": ESC + "[1;5C", "Ctrl+Left": ESC + "[1;5D",
    "Ctrl+Shift+Left": ESC + "[1;6D", "Ctrl+Shift+Right": ESC + "[1;6C",
    "Shift+Down": ESC + "[1;2B", "Shift+Up": ESC + "[1;2A",
    "Shift+End": ESC + "[1;2F", "Shift+Home": ESC + "[1;2H",
    "Alt+F": ESC + "f",
}
for c in "abcdefghijklmnopqrstuvwxyz":
    KEYS["Ctrl+" + c.upper()] = chr(ord(c) - 96)


class T:
    """Timeline builder: list of (delay, kind, payload)."""

    def __init__(self):
        self.ev = []

    def wait(self, d):
        self.ev.append((d, None, None))
        return self

    def key(self, name, d=0.5, label=None, show=True):
        self.ev.append((d, "key", (KEYS[name], (label or name) if show else None)))
        return self

    def keys(self, name, n, d=0.5, show=True):
        for _ in range(n):
            self.key(name, d, show=show)
        return self

    def type(self, s, d=0.09, first=0.3):
        for i, ch in enumerate(s):
            self.ev.append((first if i == 0 else d, "key", (ch, None)))
        return self

    def raw(self, s, d=0.3):
        self.ev.append((d, "key", (s, None)))
        return self

    # mouse: x, y are 0-based cells
    def move(self, x, y, d=0.05, btn=0):
        code = 32 + (0 if btn else 3)
        self.ev.append((d, "mouse", (f"{ESC}[<{code};{x+1};{y+1}M", x, y, btn)))
        return self

    def glide(self, x0, y0, x1, y1, steps=10, d=0.03, btn=0):
        for i in range(1, steps + 1):
            f = i / steps
            f = f * f * (3 - 2 * f)
            self.move(round(x0 + (x1 - x0) * f), round(y0 + (y1 - y0) * f), d, btn)
        return self

    def down(self, x, y, d=0.1):
        self.ev.append((d, "mouse", (f"{ESC}[<0;{x+1};{y+1}M", x, y, 1)))
        return self

    def up(self, x, y, d=0.1):
        self.ev.append((d, "mouse", (f"{ESC}[<0;{x+1};{y+1}m", x, y, 0)))
        return self

    def click(self, x, y, d=0.1):
        return self.down(x, y, d).up(x, y, 0.08)

    def wheel(self, x, y, n, dirn="down", d=0.12):
        code = 65 if dirn == "down" else 64
        for _ in range(n):
            self.ev.append((d, "mouse", (f"{ESC}[<{code};{x+1};{y+1}M", x, y, 0)))
        return self


def shell_cmd(t, cmd, d=0.07):
    """Type a shell command and press Enter."""
    t.type(cmd, d=d, first=0.4)
    t.key("Enter", 0.35, show=False)
    return t


def clip_code():
    t = T().wait(1.0)
    shell_cmd(t, "fresh src/main.rs")
    t.wait(3.5)
    # go to line 11 end, add a line of code, save
    t.key("Ctrl+G", 0.4).type("10", first=0.4).key("Enter", 0.4, show=False)
    t.key("End", 0.5, show=False).key("Enter", 0.4, show=False)
    t.type("server.warm_up();", d=0.08)
    t.wait(0.6).key("Home", 0.4, show=False).key("Shift+End", 0.5, label="Shift+End")
    t.key("Ctrl+C", 0.7).key("End", 0.4, show=False).key("Enter", 0.3, show=False)
    t.key("Ctrl+V", 0.5).wait(0.8)
    t.key("Ctrl+Z", 0.6).key("Ctrl+Z", 0.5, show=False).wait(0.6)
    t.key("Ctrl+S", 0.6).wait(1.5)
    return ["bash"], t


def clip_mouse():
    t = T().wait(4.0)
    t.move(40, 20, 0.2).glide(40, 20, 3, 0, steps=14, d=0.04)
    t.click(3, 0, 0.2).wait(1.2)
    t.glide(3, 0, 6, 5, steps=8, d=0.05).wait(0.4).glide(6, 5, 6, 8, steps=6, d=0.06).wait(0.6)
    t.key("Esc", 0.3, show=False).wait(0.4)
    # drag-select a few lines
    t.glide(6, 8, 10, 10, steps=8, d=0.04)
    t.down(10, 10, 0.2).glide(10, 10, 40, 14, steps=16, d=0.05, btn=1).up(40, 14, 0.1).wait(0.8)
    t.wheel(40, 14, 8, "down", d=0.1).wait(0.3).wheel(40, 14, 8, "up", d=0.08).wait(0.8)
    return [FRESH_ARGS, "src/server.rs"], t


def clip_palette():
    t = T().wait(4.0)
    t.key("Ctrl+P", 0.4).wait(1.2)
    t.type("git", d=0.14, first=0.5).wait(1.2)
    t.keys("BS", 3, 0.1, show=False)
    t.type("split", d=0.12, first=0.3).wait(1.0)
    t.keys("BS", 5, 0.08, show=False).key("BS", 0.1, show=False)
    t.type("cach", d=0.14, first=0.4).wait(1.0).key("Enter", 0.5, show=False).wait(1.8)
    return [FRESH_ARGS, "src/server.rs"], t


def clip_themes():
    t = T().wait(4.0)
    t.key("Ctrl+P", 0.4).type("select theme", d=0.05).wait(0.6).key("Enter", 0.3, show=False).wait(1.5)
    t.keys("Up", 9, 0.9, show=False)
    t.wait(0.6).keys("Down", 9, 0.25, show=False).key("Enter", 0.4, show=False).wait(1.5)
    return [FRESH_ARGS, "src/server.rs"], t


def clip_multicursor():
    t = T().wait(4.0)
    t.key("Ctrl+Home", 0.4, show=False).keys("Down", 15, 0.08, show=False)
    t.key("Home", 0.3, show=False).key("Ctrl+Right", 0.3, show=False).key("Ctrl+Right", 0.3, show=False)
    t.key("Ctrl+Shift+Left", 0.8, show=False).wait(0.5)
    t.keys("Ctrl+D", 6, 0.9)
    t.wait(0.6).type("account", d=0.35, first=0.5).wait(1.5)
    t.key("Esc", 0.4, show=False).wait(1.0)
    return [FRESH_ARGS, "src/server.rs"], t


def clip_grep():
    t = T().wait(4.0)
    t.key("Ctrl+P", 0.4).type("live grep", d=0.05).wait(0.5).key("Enter", 0.3, show=False).wait(1.5)
    t.type("TODO", d=0.25, first=0.4).wait(2.5)
    t.keys("Down", 4, 0.9, show=False).wait(1.0)
    return [FRESH_ARGS, "src/server.rs"], t


def clip_huge():
    t = T().wait(1.0)
    shell_cmd(t, "ls -lh huge.log")
    t.wait(0.8)
    shell_cmd(t, "fresh huge.log")
    t.wait(4.0)
    t.key("Ctrl+End", 0.5).wait(2.5)
    t.keys("PgUp", 3, 0.5, show=False).wait(1.0)
    t.key("Ctrl+Home", 0.5).wait(2.0)
    return ["bash"], t


def clip_review():
    t = T().wait(4.0)
    t.key("Ctrl+P", 0.4).type("review diff", d=0.05).wait(0.5).key("Enter", 0.3, show=False).wait(3.0)
    t.key("Tab", 0.5, show=False).wait(0.8)
    t.raw("n", 0.9).raw("n", 0.9).wait(0.6)
    t.raw("s", 0.8).wait(2.0)
    return [FRESH_ARGS, "src/server.rs"], t


def clip_terminal():
    t = T().wait(4.0)
    t.key("Ctrl+P", 0.4).type("split vertical", d=0.05).wait(0.5).key("Enter", 0.3, show=False).wait(1.5)
    t.key("Ctrl+P", 0.4).type("open terminal", d=0.05).wait(0.5).key("Enter", 0.3, show=False).wait(4.0)
    t.type("git log --oneline --stat", d=0.05, first=0.4).key("Enter", 0.3, show=False).wait(2.0)
    return [FRESH_ARGS, "src/main.rs"], t


def clip_blitz():
    t = T().wait(4.0)
    t.key("Ctrl+B", 0.3).wait(1.8)                    # file explorer
    t.key("Ctrl+B", 0.3).wait(0.4)
    t.key("Ctrl+P", 0.3).type("open settings", d=0.04).key("Enter", 0.3, show=False).wait(2.5)
    t.key("Esc", 0.3, show=False).wait(0.8)
    t.key("Ctrl+P", 0.3).type("git log", d=0.04).key("Enter", 0.3, show=False).wait(2.5)
    t.raw("q", 0.3).wait(0.8)
    t.key("Ctrl+P", 0.3).type("keybinding editor", d=0.04).key("Enter", 0.3, show=False).wait(2.5)
    return [FRESH_ARGS, "README.md"], t


def new_ws(t, pick, prompt):
    """New Workspace dialog: `pick` reaches and chooses the agent; Tab x8 from
    the prompt lands on [ Launch ], which creates the worktree and visits it."""
    t.key("Ctrl+P", 0.6, show=False).type("Orchestrator: New Workspace", d=0.02).wait(1.5)
    t.key("Enter", 0.3, show=False).wait(3.0)
    for k in pick:
        if k == "BTab":
            t.raw(ESC + "[Z", 0.8)
        else:
            t.key(k, 0.8, show=False)
    t.wait(1.0)
    t.keys("Tab", 3, 0.7, show=False)
    t.type(prompt, d=0.04, first=0.5).wait(0.8)
    t.keys("Tab", 8, 0.5, show=False).key("Enter", 0.5, show=False).wait(8.0)
    return t


def palette(t, cmd, wait=2.0):
    t.key("Ctrl+P", 0.6, show=False).type(cmd, d=0.03, first=0.6).wait(1.0)
    t.key("Enter", 0.3, show=False).wait(wait)
    return t


def clip_agents():
    """Three workspaces that look different: the main checkout reviewing its
    diff, a worktree with an agent full-screen, and a worktree with a file
    split beside its agent."""
    t = T().wait(9.0)
    palette(t, "review diff", 4.0)
    new_ws(t, ["Enter", "Down", "Down", "Enter"], "add rate limiting")
    new_ws(t, ["BTab", "BTab", "BTab", "Enter", "Down", "Enter"], "speed up the db pool")
    palette(t, "split vertical", 2.5)
    t.key("Ctrl+P", 0.6, show=False).wait(0.8).key("BS", 0.3, show=False)
    t.type("server.rs", d=0.05, first=0.4).wait(1.2).key("Enter", 0.3, show=False).wait(3.0)
    t.raw(ESC + "o", 0.5).wait(2.0)
    for k in ("Up", "Up", "Down", "Down", "Up", "Up"):
        t.key(k, 2.5, show=False)
    t.wait(1.5)
    return [FRESH_BARE], t, (COLS, ROWS), "config-orch"


def clip_settings():
    t = T().wait(4.0)
    palette(t, "open settings", 2.5)
    t.key("Down", 0.9, show=False).key("Down", 0.9, show=False).wait(1.5)
    t.key("Tab", 0.8, show=False).key("Down", 0.8, show=False).wait(0.6)
    t.raw(" ", 0.8).wait(1.2).raw(" ", 1.0).wait(1.5)
    return [FRESH_ARGS, "src/server.rs"], t


FRESH_ARGS = "__fresh__"
FRESH_BARE = "__bare__"
CLIPS = {
    "code": clip_code, "mouse": clip_mouse, "palette": clip_palette, "themes": clip_themes,
    "multicursor": clip_multicursor, "grep": clip_grep, "huge": clip_huge,
    "review": clip_review, "agents": clip_agents, "settings": clip_settings, "terminal": clip_terminal, "blitz": clip_blitz,
}


def env_for(cfg="config"):
    env = os.environ.copy()
    x = os.path.join(S, "xdg")
    env.update({
        "TERM": "xterm-256color", "COLORTERM": "truecolor",
        "COLUMNS": str(COLS), "LINES": str(ROWS),
        "XDG_CONFIG_HOME": f"{x}/{cfg}", "XDG_DATA_HOME": f"{x}/data",
        "XDG_STATE_HOME": f"{x}/state", "XDG_RUNTIME_DIR": f"{x}/run",
        "PS1": "\\[\\e[1;32m\\]❯\\[\\e[0m\\] ", "PATH": f"{S}/bin:" + env["PATH"],
        "HOME": f"{x}/home",
    })
    return env


def reset_state():
    x = os.path.join(S, "xdg")
    for d in ("data", "state", "run", "home"):
        os.system(f"rm -rf {x}/{d}; mkdir -p {x}/{d}")
    os.chmod(f"{x}/run", 0o700)
    with open(f"{x}/home/.bashrc", "w") as f:
        f.write("PS1='\\[\\e[1;32m\\]❯\\[\\e[0m\\] '\n")
    os.system(f"cd {DEMO} && git worktree list --porcelain | grep '^worktree' | tail -n +2 | cut -d' ' -f2 | xargs -r -n1 git worktree remove --force; "
              f"git worktree prune; git branch | grep -v main | xargs -r git branch -D >/dev/null 2>&1; rm -rf {S}/demo-* {S}/.worktrees; "
              f"git reset -q; git checkout -q -- . 2>/dev/null; "
              f"sed -i 's/let port = 8080;/let port = env_port().unwrap_or(8080);/' src/main.rs; "
              f"sed -i 's|        let cache = Cache::with_capacity(1024);|        let cache = Cache::with_capacity(4096);\\n        log::info!(\"cache ready\");|' src/server.rs")


def record(name):
    res = CLIPS[name]()
    argv, tl = res[0], res[1]
    cols, rows = res[2] if len(res) > 2 else (COLS, ROWS)
    cfg = res[3] if len(res) > 3 else "config"
    reset_state()
    if argv[0] == FRESH_ARGS:
        argv = [FRESH, "--no-upgrade-check", "--no-restore"] + argv[1:]
    elif argv[0] == FRESH_BARE:
        argv = [FRESH, "--no-upgrade-check"]
    else:
        argv = ["bash", "--norc", "--noprofile", "-i"]
    pid, fd = pty.fork()
    if pid == 0:
        os.chdir(DEMO)
        env = env_for(cfg)
        env["COLUMNS"], env["LINES"] = str(cols), str(rows)
        os.execvpe(argv[0], argv, env)
    fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", rows, cols, 0, 0))
    out = os.path.join(S, "casts", name + ".cast")
    os.makedirs(os.path.dirname(out), exist_ok=True)
    sched, t = [], 0.0
    for d, kind, p in tl.ev:
        t += d
        if kind:
            sched.append((t, kind, p))
    total = t + 0.8
    dec = codecs.getincrementaldecoder("utf-8")(errors="replace")
    with open(out, "w") as f:
        f.write(json.dumps({"version": 2, "width": cols, "height": rows}) + "\n")
        start = time.monotonic()
        i = 0
        while True:
            el = time.monotonic() - start
            while i < len(sched) and sched[i][0] <= el:
                ts, kind, p = sched[i]
                if kind == "key":
                    os.write(fd, p[0].encode())
                    if p[1]:
                        f.write(json.dumps([el, "k", p[1]]) + "\n")
                else:
                    os.write(fd, p[0].encode())
                    f.write(json.dumps([el, "m", f"{p[1]},{p[2]},{p[3]}"]) + "\n")
                i += 1
            nxt = sched[i][0] if i < len(sched) else total
            r, _, _ = select.select([fd], [], [], max(0.003, min(0.03, nxt - el)))
            if fd in r:
                try:
                    chunk = os.read(fd, 65536)
                except OSError:
                    break
                if not chunk:
                    break
                f.write(json.dumps([time.monotonic() - start, "o",
                                    dec.decode(chunk)]) + "\n")
            if el >= total:
                break
    try:
        os.kill(pid, 9)
    except ProcessLookupError:
        pass
    os.system("pkill -9 -f 'target/debug/fresh' 2>/dev/null; pkill -9 -f coding_agent.py 2>/dev/null")
    print(f"recorded {name}: {total:.1f}s -> {out}")


if __name__ == "__main__":
    for n in sys.argv[1:]:
        record(n)
