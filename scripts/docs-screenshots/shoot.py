#!/usr/bin/env python3
"""Take the screenshots under docs/public/images from a real Fresh.

Each shot runs Fresh inside a real kitty window on a headless X server
(Xvfb), with JetBrainsMono Nerd Font Mono, TERM=xterm-kitty and
COLORTERM=truecolor. Keys go in through xdotool, text through
`kitty @ send-text`. After every step the script polls `kitty @ get-text`
until the screen has not changed for half a second, then captures the
window with ImageMagick's `import -window <id>`, cropped to
columns x rows x cell size.

    scripts/docs-screenshots/shoot.py                 # every shot
    scripts/docs-screenshots/shoot.py settings-editor # just these
    scripts/docs-screenshots/shoot.py --list

See README.md next to this file for what it needs installed.
"""

from __future__ import annotations

import argparse
import json
import os
import shutil
import subprocess
import sys
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parent.parent
sys.path.insert(0, str(HERE))



def load_shots():
    """Every SHOTS list in shots/*.py, in file order."""
    import importlib.util
    out = []
    for f in sorted((HERE / "shots").glob("*.py")):
        if f.name.startswith("_"):
            continue
        spec = importlib.util.spec_from_file_location(f"shots_{f.stem}", f)
        mod = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(mod)
        out += getattr(mod, "SHOTS", [])
    return out

FONT = "JetBrainsMono Nerd Font Mono"
FONT_SIZE = 24
DISPLAY = os.environ.get("DOCSHOT_DISPLAY", ":77")
QUIET_SECS = 0.5


def run(*argv, **kw):
    return subprocess.run(argv, check=True, capture_output=True, text=True, **kw).stdout


class Xvfb:
    def __enter__(self):
        self.proc = subprocess.Popen(
            ["Xvfb", DISPLAY, "-screen", "0", "4000x2600x24", "-nolisten", "tcp"],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
        os.environ["DISPLAY"] = DISPLAY
        for _ in range(50):
            if subprocess.run(["xdpyinfo"], capture_output=True).returncode == 0:
                return self
            time.sleep(0.1)
        raise RuntimeError("Xvfb did not start")

    def __exit__(self, *exc):
        self.proc.terminate()
        self.proc.wait()


class Kitty:
    """One kitty window running Fresh in a throwaway home directory."""

    def __init__(self, shot: dict, binary: Path, scratch: Path):
        self.shot = shot
        self.cols = shot.get("cols", 120)
        self.rows = shot.get("rows", 34)
        self.scratch = scratch
        self.sock = f"unix:{scratch}/kitty.sock"
        home = scratch / "home"
        self.project = scratch / shot.get("dir", "invoicing")
        self.config_dir = home / ".config" / "fresh"
        self.config_dir.mkdir(parents=True)

        make_project(shot.get("project", "default"), self.project)
        config = {"version": 1, **BASE_CONFIG}
        deep_merge(config, shot.get("config", {}))
        (self.config_dir / "config.json").write_text(json.dumps(config, indent=2))
        for rel, text in shot.get("config_files", {}).items():
            path = self.project / rel
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(text)

        env = dict(os.environ)
        env.update(
            HOME=str(home),
            XDG_CONFIG_HOME=str(home / ".config"),
            XDG_DATA_HOME=str(home / ".local/share"),
            XDG_STATE_HOME=str(home / ".local/state"),
            XDG_CACHE_HOME=str(home / ".cache"),
            XDG_RUNTIME_DIR=str(scratch / "run"),
            TERM="xterm-kitty",
            COLORTERM="truecolor",
            GIT_CONFIG_NOSYSTEM="1",
        )
        env.pop("TMUX", None)
        (scratch / "run").mkdir(mode=0o700)
        argv = [str(binary), *shot.get("args", [])]
        opts = {
            "font_family": FONT,
            "font_size": str(FONT_SIZE),
            "remember_window_size": "no",
            "initial_window_width": f"{self.cols}c",
            "initial_window_height": f"{self.rows}c",
            "window_padding_width": "0",
            "allow_remote_control": "yes",
            "enable_audio_bell": "no",
            "cursor_blink_interval": "0",
            "disable_ligatures": "always",
        }
        cmd = ["kitty", "--config", "NONE", "--class", "docshot",
               "--listen-on", self.sock, "--directory", str(self.project)]
        for k, v in opts.items():
            cmd += ["-o", f"{k}={v}"]
        cmd += argv
        self.proc = subprocess.Popen(cmd, env=env, stdout=subprocess.DEVNULL,
                                     stderr=open(scratch / "kitty.log", "w"))
        self.wid = self._wait_window()
        if shot.get("trust", True):
            self.answer_trust_prompt()

    def answer_trust_prompt(self):
        """Trust the project if Fresh asks, so shots show a normal workspace."""
        self.settle()
        if "SECURITY WARNING" in self.plain():
            self.key("t")
            self.settle()
            if "SECURITY WARNING" in self.plain():
                self.key("Return")
            self.settle()

    def _wait_window(self) -> str:
        for _ in range(100):
            try:
                info = json.loads(run("kitty", "@", "--to", self.sock, "ls"))
                return str(info[0]["platform_window_id"])
            except (subprocess.CalledProcessError, json.JSONDecodeError, IndexError, KeyError):
                time.sleep(0.1)
        raise RuntimeError("kitty did not come up; see " + str(self.scratch / "kitty.log"))

    def close(self):
        self.proc.terminate()
        try:
            self.proc.wait(timeout=5)
        except subprocess.TimeoutExpired:
            self.proc.kill()

    # -- input ---------------------------------------------------------
    def key(self, *keys: str):
        run("xdotool", "windowfocus", "--sync", self.wid)
        for k in keys:
            run("xdotool", "key", "--clearmodifiers", k)
            time.sleep(0.08)

    def click(self, col: int, row: int, button: int = 1):
        """Click the middle of a cell (0-based column and row)."""
        geo = run("xdotool", "getwindowgeometry", self.wid)
        w, h = (int(n) for n in geo.split("Geometry:")[1].split()[0].split("x"))
        cw, ch = w // self.cols, h // self.rows
        run("xdotool", "mousemove", "--window", self.wid, str(col * cw + cw // 2), str(row * ch + ch // 2))
        run("xdotool", "click", str(button))

    def text(self, s: str):
        subprocess.run(["kitty", "@", "--to", self.sock, "send-text", "--stdin"],
                       input=s, text=True, check=True, capture_output=True)

    # -- output --------------------------------------------------------
    def screen(self) -> str:
        return run("kitty", "@", "--to", self.sock, "get-text", "--ansi")

    def plain(self) -> str:
        return run("kitty", "@", "--to", self.sock, "get-text")

    def settle(self, timeout: float = 30.0):
        """Wait until the screen has not changed for QUIET_SECS."""
        deadline = time.time() + timeout
        last, since = None, time.time()
        while time.time() < deadline:
            cur = self.screen()
            if cur != last:
                last, since = cur, time.time()
            elif time.time() - since >= QUIET_SECS:
                return
            time.sleep(0.1)

    def wait_for(self, needle: str, timeout: float = 30.0):
        deadline = time.time() + timeout
        while time.time() < deadline:
            if needle in self.plain():
                return
            time.sleep(0.15)
        raise RuntimeError(f"timed out waiting for {needle!r}\n---\n{self.plain()}")

    def capture(self, out: Path):
        self.settle()
        geo = run("xdotool", "getwindowgeometry", self.wid)
        w, h = (int(n) for n in geo.split("Geometry:")[1].split()[0].split("x"))
        cw, ch = w // self.cols, h // self.rows
        raw = self.scratch / "raw.png"
        run("import", "-window", self.wid, str(raw))
        crop = self.shot.get("crop")  # (col, row, cols, rows), in cells
        if crop:
            c, r, nc, nr = crop
            box = f"{nc * cw}x{nr * ch}+{c * cw}+{r * ch}"
        else:
            box = f"{self.cols * cw}x{self.rows * ch}+0+0"
        out.parent.mkdir(parents=True, exist_ok=True)
        run("convert", str(raw), "-crop", box, "+repage", "-strip", str(out))
        print(f"  wrote {out}")


BASE_CONFIG = {
    "theme": "dark",
    "check_for_updates": False,
    "editor": {"nerd_font_icons": True, "animations": False, "screensaver_enabled": False},
}


def deep_merge(dst: dict, src: dict):
    for k, v in src.items():
        if isinstance(v, dict) and isinstance(dst.get(k), dict):
            deep_merge(dst[k], v)
        else:
            dst[k] = v


def make_project(name: str, dest: Path):
    src = HERE / "projects" / name
    dest.parent.mkdir(parents=True, exist_ok=True)
    shutil.copytree(src, dest, ignore=shutil.ignore_patterns("setup.sh"))
    setup = src / "setup.sh"
    if setup.exists():
        subprocess.run(["bash", str(setup)], cwd=dest, check=True, capture_output=True)


def find_frame(screen: str, title: str):
    """(col, row, cols, rows) of the frame whose top border contains `title`."""
    lines = screen.splitlines()
    for r, line in enumerate(lines):
        if title in line:
            left = line.rfind("╭", 0, line.index(title) + 1)
            right = line.find("╮", line.index(title))
            if left < 0 or right < 0:
                continue
            for r2 in range(r + 1, len(lines)):
                if len(lines[r2]) > left and lines[r2][left] == "╰":
                    return (left, r, right - left + 1, r2 - r + 1)
    raise RuntimeError(f"crop_frame: no frame titled {title!r}")


def do_steps(k: Kitty, steps, outdir: Path):
    for step in steps:
        op, *a = step
        if op == "key":
            k.key(*a)
        elif op == "click_text":  # click the first (or nth) place this text is on screen
            needle, nth = a[0], (a[1] if len(a) > 1 else 0)
            hits = [(r, line.index(needle)) for r, line in enumerate(k.plain().splitlines()) if needle in line]
            if len(hits) <= nth:
                raise RuntimeError(f"click_text: {needle!r} not on screen")
            r, c = hits[nth]
            k.click(c + len(needle) // 2, r)
        elif op == "click":
            k.click(*a)
        elif op == "text":
            k.text(a[0])
        elif op == "wait":
            k.wait_for(a[0], *(a[1:]))
        elif op == "sleep":
            time.sleep(a[0])
        elif op == "settle":
            k.settle()
        elif op == "shell":  # run a command in the project dir (setup mid-shot)
            subprocess.run(a[0], shell=True, cwd=k.project, check=True)
        elif op == "shot":
            k.capture(outdir / f"{a[0]}.png")
        elif op == "ansi":
            Path(a[0]).write_text(k.screen())
        elif op == "dump":
            print(k.plain())
        elif op == "crop_frame":  # crop to the box-drawn frame whose top edge holds this title
            k.shot["crop"] = find_frame(k.plain(), a[0])
        elif op == "crop":
            k.shot["crop"] = a[0]
        else:
            raise ValueError(f"unknown step {step!r}")


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("names", nargs="*", help="shots to take (default: all)")
    ap.add_argument("--list", action="store_true")
    ap.add_argument("--out", type=Path, default=REPO / "docs/public/images")
    ap.add_argument("--fresh", type=Path, default=REPO / "target/release/fresh")
    ap.add_argument("--keep", action="store_true", help="keep scratch dirs")
    args = ap.parse_args()

    SHOTS = load_shots()
    if args.list:
        for s in SHOTS:
            print(f"{s['id']:32} {', '.join(x[1] for x in s['steps'] if x[0] == 'shot')}")
        return
    todo = [s for s in SHOTS if not args.names or s["id"] in args.names]
    missing = set(args.names) - {s["id"] for s in SHOTS}
    if missing:
        sys.exit(f"unknown shot(s): {', '.join(sorted(missing))}")
    if not args.fresh.exists():
        sys.exit(f"{args.fresh} not found: cargo build --release -p fresh-editor")

    failed = []
    with Xvfb():
        for shot in todo:
            print(shot["id"])
            # A fixed, short path: Fresh shows the project path in some
            # screens (file browser, prompts), so it should read sensibly.
            scratch = Path("/tmp/docshot") / shot["id"]
            shutil.rmtree(scratch, ignore_errors=True)
            scratch.mkdir(parents=True)
            k = None
            try:
                k = Kitty(shot, args.fresh, scratch)
                do_steps(k, shot["steps"], args.out)
            except Exception as e:  # keep going; report at the end
                print(f"  FAILED: {e}", file=sys.stderr)
                failed.append(shot["id"])
            finally:
                if k:
                    k.close()
                if not args.keep:
                    shutil.rmtree(scratch, ignore_errors=True)
                else:
                    print(f"  scratch: {scratch}")
    if failed:
        sys.exit(f"failed: {', '.join(failed)}")


if __name__ == "__main__":
    main()
