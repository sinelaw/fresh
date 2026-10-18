"""Drive a program in a detached tmux pane and photograph its screen.

A "frame" is the pane's text with its SGR attributes (capture-pane -e) plus
the cursor, which is all a renderer needs to redraw it at any size.
"""
import json, os, subprocess, time

SOCK = "vidcam"


def tm(*args, check=True):
    return subprocess.run(["tmux", "-L", SOCK, *args], capture_output=True,
                          text=True, check=check).stdout


class Cam:
    def __init__(self, cols, rows, cwd, env, cmd="bash --norc --noprofile"):
        tm("kill-server", check=False)
        self.cols, self.rows = cols, rows
        import shlex
        envs = " ".join(shlex.quote(f"{k}={v}") for k, v in env.items())
        tm("-f", "/dev/null", "new-session", "-d", "-s", "c", "-x", str(cols),
           "-y", str(rows), "-c", cwd, f"env -i {envs} {cmd}")
        tm("set", "-g", "status", "off")
        tm("set", "-g", "escape-time", "0")
        tm("set", "-as", "terminal-features", ",*:RGB")
        self.frames = {}
        time.sleep(0.4)

    def keys(self, *k, settle=0.15):
        tm("send-keys", "-t", "c", *k)
        time.sleep(settle)

    def text(self, s, settle=0.15):
        tm("send-keys", "-t", "c", "-l", s)
        time.sleep(settle)

    def raw(self, s, settle=0.15):
        """bytes straight into the program's input, e.g. an SGR mouse report"""
        tm("send-keys", "-t", "c", "-H", *[f"{b:02x}" for b in s.encode()])
        time.sleep(settle)

    def click(self, col, row, settle=0.4):
        # SGR 1006 mouse: 1-based cell coordinates
        self.raw(f"\x1b[<0;{col + 1};{row + 1}M", settle=0.05)
        self.raw(f"\x1b[<0;{col + 1};{row + 1}m", settle=settle)

    def grab(self):
        ans = tm("capture-pane", "-p", "-e", "-N", "-t", "c")
        cur = tm("display", "-p", "-t", "c",
                 "#{cursor_x} #{cursor_y} #{cursor_flag}").split()
        return {"ans": ans, "cx": int(cur[0]), "cy": int(cur[1]),
                "cv": cur[2] == "1", "t": time.time()}

    def shot(self, name):
        self.frames[name] = self.grab()
        return self.frames[name]

    def plain(self):
        return tm("capture-pane", "-p", "-t", "c")

    def wait_for(self, needle, timeout=10, step=0.05):
        t0 = time.time()
        while time.time() - t0 < timeout:
            if needle in self.plain():
                return time.time() - t0
            time.sleep(step)
        raise TimeoutError(needle)

    def burst(self, name, seconds, fps=30):
        """a run of frames, as fast as asked, for the screen changing by itself"""
        out, t0 = [], time.time()
        while time.time() - t0 < seconds:
            out.append(self.grab())
            time.sleep(max(0, 1 / fps - 0.01))
        self.frames[name] = out
        return out

    def save(self, path):
        with open(path, "w") as f:
            json.dump({"cols": self.cols, "rows": self.rows,
                       "frames": self.frames}, f)

    def close(self):
        tm("kill-server", check=False)
