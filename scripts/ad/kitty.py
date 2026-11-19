"""Run a program in a real kitty window on a virtual X display and photograph it.

The stills on the landing page are screenshots of Fresh as it really draws in
kitty, with a Nerd Font, rather than a re-rendering of a recording:

    with Display() as d:
        k = Kitty(d, ["fresh", "src/main.rs"], cwd=..., env=...)
        k.send(b"\\x1b[B")          # bytes go to the program as if typed
        k.shot("out.png", crop=(0, 0, 100, 31))   # crop in terminal cells
        k.close()

Needs `Xvfb`, ImageMagick's `import`, kitty (`KITTY`, default `kitty` on PATH,
then /opt/kitty.app/bin/kitty) and the font named in `FONT` installed for
fontconfig. kitty draws with OpenGL; Mesa's software renderer is enough.
"""
import base64
import json
import os
import shutil
import socket
import subprocess
import tempfile
import time

FONT = os.environ.get("AD_FONT", "JetBrainsMono Nerd Font Mono")
# 25 pt at Xvfb's 96 dpi gives JetBrains Mono a 20 px cell: the old
# canvas stills were drawn at 2x a 10 px cell, so crops keep their sizes.
FONT_SIZE = float(os.environ.get("AD_FONT_SIZE", "25"))
# The clips run with HOME pointed at a sandbox, which would hide fonts
# installed per user; kitty gets a fontconfig file naming the real ones.
FONT_DIRS = [d for d in (os.path.expanduser("~/.local/share/fonts"), os.path.expanduser("~/.fonts"))
             if os.path.isdir(d)]


def kitty_bin():
    k = os.environ.get("KITTY") or shutil.which("kitty") or "/opt/kitty.app/bin/kitty"
    if not os.path.exists(k) and not shutil.which(k):
        raise SystemExit("kitty not found: install it or set KITTY")
    return k


class Display:
    """A private Xvfb screen, big enough for a 140x75 terminal at 2x."""

    def __init__(self, num=None, size="3200x3600"):
        self.num = num or int(os.environ.get("AD_DISPLAY", "97"))
        self.size = size
        self.proc = None

    def __enter__(self):
        self.name = f":{self.num}"
        lock = f"/tmp/.X{self.num}-lock"
        if os.path.exists(lock):        # stale or ours from an earlier run: reuse it
            return self
        self.proc = subprocess.Popen(
            ["Xvfb", self.name, "-screen", "0", f"{self.size}x24", "-dpi", "96", "-nolisten", "tcp"],
            stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        for _ in range(50):
            if os.path.exists(lock):
                break
            time.sleep(0.1)
        return self

    def __exit__(self, *exc):
        if self.proc:
            self.proc.terminate()
            self.proc.wait()


class Kitty:
    def __init__(self, display, argv, cwd, env, cols=140, rows=75, title="fresh-shot"):
        self.display, self.cols, self.rows, self.title = display, cols, rows, title
        self.dir = tempfile.mkdtemp(prefix="kitty-shot-")
        self.sock = os.path.join(self.dir, "ctl")
        conf = os.path.join(self.dir, "kitty.conf")
        with open(conf, "w") as f:
            f.write("\n".join([
                f"font_family {FONT}",
                f"font_size {FONT_SIZE}",
                "disable_ligatures always",
                "window_padding_width 0",
                "placement_strategy top-left",
                "remember_window_size no",
                f"initial_window_width {cols}c",
                f"initial_window_height {rows}c",
                "hide_window_decorations yes",
                "allow_remote_control socket-only",
                "cursor_blink_interval 0",
                "cursor_trail 0",
                "enable_audio_bell no",
                "confirm_os_window_close 0",
                "update_check_interval 0",
                "shell_integration disabled",
                "",
            ]))
        fonts = os.path.join(self.dir, "fonts.conf")
        with open(fonts, "w") as f:
            f.write('<?xml version="1.0"?>\n<!DOCTYPE fontconfig SYSTEM "fonts.dtd">\n<fontconfig>\n'
                    '  <include ignore_missing="yes">/etc/fonts/fonts.conf</include>\n'
                    + "".join(f"  <dir>{d}</dir>\n" for d in FONT_DIRS)
                    + f"  <cachedir>{os.path.join(self.dir, 'fc-cache')}</cachedir>\n</fontconfig>\n")
        kenv = dict(env)
        kenv.update({"DISPLAY": display.name, "LIBGL_ALWAYS_SOFTWARE": "1",
                     "KITTY_CONFIG_DIRECTORY": self.dir, "FONTCONFIG_FILE": fonts})
        kenv.pop("TERM", None)          # kitty sets its own (xterm-kitty)
        self.proc = subprocess.Popen(
            [kitty_bin(), "--config", conf, "--listen-on", f"unix:{self.sock}",
             "--title", title, "--directory", cwd, *argv],
            env=kenv, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        self.started = time.monotonic()
        for _ in range(200):
            if os.path.exists(self.sock) and self._window():
                break
            time.sleep(0.05)
        else:
            raise RuntimeError("kitty did not come up")

    def _window(self):
        out = subprocess.run(["xwininfo", "-display", self.display.name, "-name", self.title],
                             capture_output=True, text=True).stdout
        geo = {}
        for line in out.splitlines():
            line = line.strip()
            for key in ("Absolute upper-left X", "Absolute upper-left Y", "Width", "Height"):
                if line.startswith(key + ":"):
                    geo[key] = int(line.split(":")[1])
        if len(geo) < 4 or geo["Width"] < 10:
            return None
        return geo["Absolute upper-left X"], geo["Absolute upper-left Y"], geo["Width"], geo["Height"]

    def send(self, data):
        """Write bytes to the program's input, as kitty does for typed keys."""
        if isinstance(data, str):
            data = data.encode()
        msg = {"cmd": "send-text", "version": [0, 40, 0], "no_response": True,
               "payload": {"data": "base64:" + base64.standard_b64encode(data).decode()}}
        s = socket.socket(socket.AF_UNIX)
        s.connect(self.sock)
        s.sendall(b"\x1bP@kitty-cmd" + json.dumps(msg).encode() + b"\x1b\\")
        s.close()

    def shot(self, path, crop=None):
        """Save the window (or a cell rectangle c0, r0, c1, r1 of it) as PNG."""
        x, y, w, h = self._window()
        cw, ch = w / self.cols, h / self.rows
        if crop:
            c0, r0, c1, r1 = crop
            x, y = x + round(c0 * cw), y + round(r0 * ch)
            w, h = round((c1 - c0) * cw), round((r1 - r0) * ch)
        subprocess.run(["import", "-display", self.display.name, "-window", "root",
                        "-crop", f"{w}x{h}+{x}+{y}", "+repage", path], check=True)
        return w, h

    def close(self):
        self.proc.terminate()
        try:
            self.proc.wait(5)
        except subprocess.TimeoutExpired:
            self.proc.kill()
        shutil.rmtree(self.dir, ignore_errors=True)
