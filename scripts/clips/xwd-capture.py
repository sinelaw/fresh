#!/usr/bin/env python3
"""Capture a tui-clips spec with `xwd` bursts, so a clip can film motion.

`tui-capture` offers two ways to get frames, and on a headless Xvfb neither
films a program that animates on its own clock:

  * `--record` streams the X screen with ffmpeg, and gets almost nothing. The
    terminal emulator does not paint on its own here — it repaints when an X
    client asks the server for its pixels — so a recording of an agent working
    for eight seconds contains three distinct images.
  * `--shot` does force a repaint, which is why the stepped-stills clips work,
    but a screenshot costs 150-300ms and `tui-capture` sleeps 1.2s after every
    keystroke. Anything shorter than that — the orchestrator's 180ms workspace
    wipe, say — happens between two frames and is never on one.

This does the same job with `xwd`, which is the same forced repaint at a
fraction of the cost: about 20ms for a pane-sized window against ImageMagick's
150ms, because it writes the server's own bytes instead of encoding a PNG. Fast
enough to *sample* an animation rather than step past it, and driven by its own
clock rather than by a fixed sleep after each key.

    scripts/clips/xwd-capture.py scripts/clips/fresh-orchestrator-dock.json \\
        --out ~/repos/tui-clips/out/fresh-orchestrator-dock
    ~/repos/tui-clips/bin/tui-clip scripts/clips/fresh-orchestrator-dock.json \\
        --skip-capture

It writes exactly what `tui-clip --skip-capture` expects to find — `<out>/
<pane>.png`, `<out>/shots/<pane>-<shot>.png`, and a directory of numbered
frames per recorded run — so the spec stays an ordinary tui-clips spec and the
upstream capture still works on it, just less well.

Steps understood, from `capture.keys`: `key` (with `repeat`), `type`, `sleep`,
`shot`, `drag`, and `record` — the last is the interesting one, a burst of
frames at its `fps`, running *alongside* the steps that follow it, which is what
puts a keystroke and the animation it causes inside one run.

A run can also be *trimmed* to what happened in it. The terminal replays what
the program wrote at the rate it is asked, so a burst of output — a workspace
switch redrawing the screen eleven times — arrives late and takes longer to come
out than it took to go in. Wall-clock timing therefore cannot place a beat on
it. A run with `keep` grabs a long window around the keystroke and then keeps
`keep` frames around the biggest change in it, which is the switch: the camera
finds the event rather than being told when to expect it.

Grabbing never stops. The terminal here paints only when a client asks for its
pixels, and what it paints is not "now" — it is the next chunk of whatever the
program has written since the last time it was asked. So a gap in the grabbing
is a queue: let two seconds pass between runs and the next run opens by
replaying the two seconds it missed, which puts a workspace switch a whole beat
late and makes the dock's highlight look like it bounced. The grabber therefore
runs from the first step to the last, and a `record` step only decides whether
the frame it takes is *kept* — the rest are overwritten in place, and exist
only to keep the queue empty.

`drag` is the mouse: `{"drag": {"from_col": 38, "to_col": 24, "row": 10}}`
presses at one cell and releases at another, in cells rather than pixels
because a spec is written in cells. It exists for the things a program only
lets you do with a pointer — the width of the orchestrator's dock, say, which
has no setting and no key.
"""
from __future__ import annotations

import argparse
import json
import os
import shutil
import subprocess
import sys
import threading
import time

# A key press is followed by a much shorter pause than tui-capture's 1.2s: the
# point of this backend is to be sampling while the editor is still moving.
KEY_SETTLE = 0.12
TYPE_SETTLE = 0.4


def expand(value: str, ctx: dict) -> str:
    for k, v in ctx.items():
        value = value.replace("{" + k + "}", v)
    return os.path.expandvars(os.path.expanduser(value))


class Session:
    """One editor, in one terminal, on one headless display."""

    def __init__(self, spec: dict, out: str, scratch: str):
        self.cap = spec["capture"]
        self.pane = next(iter(self.cap["panes"]))
        self.pane_spec = self.cap["panes"][self.pane]
        self.display = self.cap.get("display", ":99")
        self.out = out
        self.scratch = scratch
        self.shots = os.path.join(out, "shots")
        self.raw = os.path.join(scratch, "raw")
        self.wid = ""
        self.env = dict(os.environ)
        self.env["DISPLAY"] = self.display
        # A terminal that decodes the pane's box-drawing glyphs as latin-1
        # wraps every line; the spec carries the locale, but default it too.
        self.env.setdefault("LANG", "C.UTF-8")
        self.env.setdefault("LC_ALL", "C.UTF-8")
        self.cell_w = 10.0               # measured once the window is up
        self.cell_h = 20.0
        self.pending: list[tuple[str, str]] = []   # (xwd, png)
        self.run: str | None = None      # the run being kept, if any
        self.run_n: dict[str, int] = {}
        self.trims: dict[str, int] = {}  # run -> frames to keep around the event
        self.grabbing = False

    # -- lifecycle ----------------------------------------------------------
    def x(self, *args, **kw):
        return subprocess.run(args, env=self.env, check=False, **kw)

    def start(self) -> None:
        proj = os.path.join(self.scratch, "proj")
        os.makedirs(proj, exist_ok=True)
        os.makedirs(self.shots, exist_ok=True)
        os.makedirs(self.raw, exist_ok=True)
        ctx = {"scratch": self.scratch, "proj": proj, "pane": self.pane}

        for entry in self.cap.get("copy_files", []):
            as_ = entry.get("as") if isinstance(entry, dict) else None
            src = expand(entry["src"] if isinstance(entry, dict) else entry, {})
            shutil.copy(src, os.path.join(proj, as_ or os.path.basename(src)))
        for src, dst in self.cap.get("copy_dirs", {}).items():
            dst = expand(dst, ctx)
            shutil.copytree(expand(src, {}), dst, dirs_exist_ok=True,
                            ignore=shutil.ignore_patterns(".git", "logs"))
        for k, v in {**self.cap.get("env", {}),
                     **self.pane_spec.get("env", {})}.items():
            self.env[k] = expand(v, ctx)

        subprocess.run(["pkill", "-f", f"Xvfb {self.display}"], check=False)
        time.sleep(0.5)
        screen = self.cap.get("screen", "1600x2200x24")
        subprocess.Popen(
            ["Xvfb", self.display, "-screen", "0", screen, "-nolisten", "tcp"],
            stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
            start_new_session=True)
        time.sleep(2)

        workdir = expand(self.pane_spec.get("workdir",
                                            self.cap.get("workdir", "{proj}")), ctx)
        argv = [expand(a, ctx) for a in self.pane_spec["argv"]]
        subprocess.Popen(
            ["xfce4-terminal", "--disable-server", "--hide-menubar",
             "--hide-toolbar", "--hide-scrollbar", "--hide-borders",
             f"--geometry={self.cap['geometry']}",
             f"--font={self.cap['font']}",
             f"--working-directory={workdir}", "-x", *argv],
            env=self.env, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
            start_new_session=True)

        settle = float(self.pane_spec.get("settle", self.cap.get("settle", 8)))
        print(f"  settling {settle:.0f}s ...", flush=True)
        time.sleep(settle)

        found = self.x("xdotool", "search", "--name", "Terminal - ",
                       capture_output=True, text=True)
        ids = [line for line in found.stdout.split() if line]
        if not ids:
            raise SystemExit("xwd-capture: no terminal window appeared")
        self.wid = ids[-1]
        self.x("xdotool", "windowfocus", self.wid)
        time.sleep(0.5)
        geo = self.x("xdotool", "getwindowgeometry", "--shell", self.wid,
                     capture_output=True, text=True).stdout
        dims = dict(line.split("=", 1) for line in geo.strip().splitlines()
                    if "=" in line)
        cols, rows = (int(n) for n in self.cap["geometry"].split("x"))
        self.cell_w = int(dims.get("WIDTH", 0)) / cols
        self.cell_h = int(dims.get("HEIGHT", 0)) / rows
        print(f"  window {dims.get('WIDTH')}x{dims.get('HEIGHT')}"
              f"  cell {self.cell_w:.2f}x{self.cell_h:.2f}", flush=True)

    def stop(self) -> None:
        subprocess.run(["pkill", "-f", "xfce4-terminal --disable-server"],
                       check=False)
        time.sleep(0.5)
        subprocess.run(["pkill", "-f", f"Xvfb {self.display}"], check=False)

    # -- frames -------------------------------------------------------------
    def grab(self, path_xwd: str) -> None:
        """One forced repaint, straight to disk as the server's own bytes."""
        self.x("xwd", "-silent", "-id", self.wid, "-out", path_xwd)

    def drag(self, spec: dict) -> None:
        """Press at one cell, release at another. Motion goes in steps: a
        press-and-teleport is one motion event, and a program tracking a drag
        wants to be told where the pointer went on the way."""
        cw = self.cell_w
        ch = self.cell_h
        y = int((float(spec.get("row", 5)) + 0.5) * ch)
        x0 = int((float(spec["from_col"]) + 0.5) * cw)
        x1 = int((float(spec["to_col"]) + 0.5) * cw)
        self.x("xdotool", "mousemove", "--window", self.wid, str(x0), str(y))
        time.sleep(0.2)
        self.x("xdotool", "mousedown", "1")
        time.sleep(0.2)
        steps = 8
        for k in range(1, steps + 1):
            self.x("xdotool", "mousemove", "--window", self.wid,
                   str(int(x0 + (x1 - x0) * k / steps)), str(y))
            time.sleep(0.05)
        self.x("xdotool", "mouseup", "1")
        time.sleep(0.4)
        # Park the pointer out of the frame: a cursor left over the dock
        # leaves a hover highlight on whatever row it landed on.
        self.x("xdotool", "mousemove", "--window", self.wid, "10", "10")
        time.sleep(0.2)

    def shot(self, name: str) -> None:
        raw = os.path.join(self.raw, f"shot-{name}.xwd")
        self.grab(raw)
        self.pending.append(
            (raw, os.path.join(self.shots, f"{self.pane}-{name}.png")))

    def grabber(self) -> threading.Thread:
        """Grab for the whole sequence, keeping only what a run asked for.

        The drain frames go to one path, overwritten every time: they are not
        wanted as pictures, only as the asking that keeps the terminal's queue
        from filling up behind them.
        """
        drain = os.path.join(self.raw, "drain.xwd")

        def loop():
            while self.grabbing:
                name = self.run
                if name is None:
                    self.grab(drain)
                    continue
                i = self.run_n.get(name, 0)
                self.run_n[name] = i + 1
                raw = os.path.join(self.raw, f"{name}-{i:04d}.xwd")
                self.grab(raw)
                self.pending.append(
                    (raw, os.path.join(self.shots, f"{self.pane}-{name}",
                                       f"{i:04d}.png")))

        self.grabbing = True
        t = threading.Thread(target=loop, daemon=True)
        t.start()
        return t

    def film(self, name: str, seconds: float, keep: int = 0) -> threading.Thread:
        """Keep the next `seconds` of the stream under `name`. Returns at once
        — the steps that follow are the point, since they are what makes the
        motion this run is here to hold."""
        d = os.path.join(self.shots, f"{self.pane}-{name}")
        shutil.rmtree(d, ignore_errors=True)
        os.makedirs(d, exist_ok=True)
        if keep:
            self.trims[name] = keep

        def stop():
            time.sleep(seconds)
            if self.run == name:
                self.run = None

        self.run = name
        t = threading.Thread(target=stop, daemon=True)
        t.start()
        return t

    def convert(self) -> None:
        """xwd is a capture format, not a delivery one. Convert in one batch at
        the end rather than between frames, where the cost would land in the
        middle of the motion being filmed."""
        print(f"  converting {len(self.pending)} frames ...", flush=True)
        for chunk in range(0, len(self.pending), 24):
            batch = self.pending[chunk:chunk + 24]
            procs = [subprocess.Popen(["convert", src, dst],
                                      stdout=subprocess.DEVNULL,
                                      stderr=subprocess.DEVNULL)
                     for src, dst in batch]
            for p in procs:
                p.wait()

    def trim(self) -> None:
        """Cut each `keep` run down to the frames around its biggest change.

        A third of the window before the event and two thirds after: the eye
        needs a moment of the old screen to see that it *was* the old screen,
        and rather longer of the new one to read it.
        """
        from PIL import Image
        import numpy as np

        for name, keep in self.trims.items():
            d = os.path.join(self.shots, f"{self.pane}-{name}")
            fs = sorted(f for f in os.listdir(d) if f.endswith(".png"))
            if len(fs) <= keep:
                continue
            prev, diffs = None, []
            for f in fs:
                a = np.asarray(Image.open(os.path.join(d, f))
                               .convert("L").resize((240, 120)), dtype=np.int16)
                diffs.append(0.0 if prev is None
                             else float(np.abs(a - prev).mean()))
                prev = a
            anchor = int(np.argmax(diffs))
            lo = max(0, min(len(fs) - keep, anchor - keep // 3))
            print(f"  {name}: kept {keep} of {len(fs)} frames around #{anchor}",
                  flush=True)
            for i, f in enumerate(fs):
                if not (lo <= i < lo + keep):
                    os.remove(os.path.join(d, f))
            for i, f in enumerate(sorted(os.listdir(d))):
                os.rename(os.path.join(d, f),
                          os.path.join(d, f"k{i:04d}.png"))

    # -- the sequence -------------------------------------------------------
    def play(self, steps: list[dict]) -> None:
        films: list[threading.Thread] = [self.grabber()]
        for step in steps:
            if "key" in step:
                rep = int(step.get("repeat", 1))
                if rep > 1:
                    self.x("xdotool", "key", "--window", self.wid, "--repeat",
                           str(rep), "--repeat-delay", "40", step["key"])
                    time.sleep(0.3)
                else:
                    self.x("xdotool", "key", "--window", self.wid, step["key"])
                    time.sleep(KEY_SETTLE)
            elif "type" in step:
                self.x("xdotool", "type", "--window", self.wid, "--delay", "40",
                       step["type"])
                time.sleep(TYPE_SETTLE)
            elif "sleep" in step:
                time.sleep(float(step["sleep"]))
            elif "drag" in step:
                self.drag(step["drag"])
            elif "shot" in step:
                self.shot(step["shot"])
            elif "record" in step:
                films.append(self.film(step["record"],
                                       float(step.get("seconds", 5)),
                                       int(step.get("keep", 0))))
        for t in films[1:]:
            t.join()
        self.grabbing = False
        films[0].join(timeout=2)
        # The still `tui-clip` uses for a beat that names no shot.
        raw = os.path.join(self.raw, "final.xwd")
        self.grab(raw)
        self.pending.append((raw, os.path.join(self.out, f"{self.pane}.png")))


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("spec")
    ap.add_argument("--out", required=True,
                    help="tui-clips' out/<name> directory for this clip")
    args = ap.parse_args()

    with open(args.spec) as fh:
        spec = json.load(fh)
    out = os.path.expanduser(args.out)
    os.makedirs(out, exist_ok=True)
    scratch = os.path.join(out, "scratch")
    shutil.rmtree(scratch, ignore_errors=True)
    os.makedirs(scratch, exist_ok=True)

    s = Session(spec, out, scratch)
    try:
        s.start()
        print("  filming ...", flush=True)
        s.play([*spec["capture"].get("keys", []),
                *s.pane_spec.get("keys", [])])
    finally:
        s.stop()
    s.convert()
    s.trim()
    print(f"  {len(s.pending)} frames -> {out}")


if __name__ == "__main__":
    main()
