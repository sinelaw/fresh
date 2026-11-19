#!/usr/bin/env python3
"""The landing page's Orchestrator loop and still, photographed in kitty.

    python3 scripts/ad/kitty_orchestrator.py

Starts Fresh in kitty (see kitty.py) with orchestrator_init.ts as its init.ts,
which builds the scene: six workspaces filed into Performance and Security,
each showing something different. Once the agents' panes have filled, the
dock walks through them (a quick hop across each folder header, a hold on
each workspace) and runs back to the first, so the loop closes on itself.
Every stop is a screenshot; the frames become

  homepage/public/assets/orchestrator.webp        the loop, an animated WebP
  homepage/public/assets/orchestrator.mp4         the same, for the link
  homepage/public/assets/shots/orchestrator.webp  its first frame, the still
  homepage/public/assets/shots/review-diff.webp   db-pool-tuning, dock closed

Every frame is the whole kitty window, status bar included.

Run setup.sh first.
"""
import os
import shutil
import subprocess
import sys
import tempfile
import time

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import rec                                   # noqa: E402
import kitty_shots                           # noqa: E402
from kitty import Display, Kitty             # noqa: E402

ASSETS = os.path.join(rec.REPO, "homepage", "public", "assets")
FRAMES = os.path.join(rec.S, "korch")
COLS, ROWS = 140, 44            # the window, photographed whole
FILL = float(os.environ.get("AD_FILL", "45"))   # seconds for the agents' panes to fill
# dock rows from the top (0 = first row under the search box): acme-api,
# api-docs, [Performance], rate-limiter, db-pool-tuning, [Security],
# auth-bypass, token-expiry
STOPS = [0, 1, 3, 4, 6, 7]
REVIEW = 4                      # db-pool-tuning: its change open in Review Diff
HOLD = 1.2                      # seconds on each workspace
HOLD_SHOTS = 3                  # frames per hold, so working agents move
HOP = 0.09                      # seconds per row passed over
UP, DOWN, DOCK, ESC, PALETTE = "\x1b[A", "\x1b[B", "\x1bo", "\x1b", "\x10"


def main():
    if os.path.isdir(FRAMES):
        shutil.rmtree(FRAMES)
    os.makedirs(FRAMES)
    rec.reset_state()
    x = os.path.join(rec.S, "xdg")
    os.makedirs(f"{x}/data/fresh", exist_ok=True)
    with open(f"{x}/data/fresh/chrome.json", "w") as f:
        f.write('{"dock": {"width": 34}}')      # wide enough for "token-expiry · opencode"
    cfg = kitty_shots.kitty_config("config-orch", "config-orch-kitty", kitty_shots.THEME)
    shutil.copy(os.path.join(HERE, "orchestrator_init.ts"), f"{x}/{cfg}/fresh/init.ts")
    ready = os.path.join(tempfile.mkdtemp(), "ready")
    env = rec.env_for(cfg)
    # the agents fill the pane beside the 34-column dock; wider, their redraws smear
    env.update({"LANG": env.get("LANG") or "C.UTF-8", "AD_SCENE_READY": ready, "AD_AGENT_COLUMNS": "100",
                "COLUMNS": str(COLS), "LINES": str(ROWS)})

    frames = []                 # (png, seconds on screen)
    review_png = os.path.join(FRAMES, "review-diff.png")

    with Display() as d:
        k = Kitty(d, [rec.FRESH, "--no-upgrade-check"], cwd=rec.DEMO, env=env, cols=COLS, rows=ROWS)
        try:
            for _ in range(240):
                if os.path.exists(ready):
                    break
                time.sleep(0.5)
            else:
                raise SystemExit("the scene never became ready (see orchestrator_init.ts)")
            print(f"scene ready; letting the agents work for {FILL:.0f}s")
            time.sleep(FILL)
            k.send(DOCK)
            time.sleep(0.5)
            for _ in range(12):
                k.send(UP)
                time.sleep(0.05)
            time.sleep(1.5)

            def snap(secs):
                p = os.path.join(FRAMES, f"{len(frames):04d}.png")
                k.shot(p)
                frames.append((p, secs))

            def hold():
                for _ in range(HOLD_SHOTS):
                    t0 = time.monotonic()
                    snap(HOLD / HOLD_SHOTS)
                    time.sleep(max(0, HOLD / HOLD_SHOTS - (time.monotonic() - t0)))

            def move(key, n):
                for i in range(n):
                    k.send(key)
                    time.sleep(0.6)       # let the workspace draw before the shot
                    if i < n - 1:
                        snap(HOP)

            row = STOPS[0]
            hold()
            for stop in STOPS[1:]:
                move(DOWN, stop - row)
                row = stop
                hold()
            # run back up to the first workspace; the loop restarts on its hold
            for i in range(row - STOPS[0]):
                k.send(UP)
                time.sleep(0.6)
                if i < row - STOPS[0] - 1:
                    snap(HOP)

            # the Review Diff still: db-pool-tuning with the dock closed, so
            # the diff has the whole window
            for _ in range(REVIEW - STOPS[0]):
                k.send(DOWN)
                time.sleep(0.3)
            time.sleep(1.5)
            k.send(ESC)                   # out of the dock, into the workspace
            time.sleep(0.8)
            k.send(PALETTE)
            time.sleep(0.8)
            k.send("Orchestrator: Toggle Dock")
            time.sleep(1.2)
            k.send("\r")
            time.sleep(2.0)
            k.shot(review_png)
        finally:
            k.close()
            os.system("pkill -9 -f 'target/debug/fresh' 2>/dev/null; pkill -9 -f coding_agent.py 2>/dev/null; "
                      "pkill -9 -f waiting_agent.py 2>/dev/null")
    print(f"{len(frames)} frames, {sum(s for _, s in frames):.1f}s")
    encode(frames, review_png)


def still(png, name):
    from PIL import Image
    im = Image.open(png).convert("RGB")
    im = im.resize((2000, round(im.height * 2000 / im.width)), Image.LANCZOS)
    im.save(os.path.join(ASSETS, "shots", name + ".webp"), "WEBP", quality=88, method=6)


def encode(frames, review_png):
    from PIL import Image
    imgs = []
    for p, _ in frames:
        im = Image.open(p).convert("RGB")
        imgs.append(im.resize((1600, round(im.height * 1600 / im.width)), Image.LANCZOS))
    durs = [round(s * 1000) for _, s in frames]
    # Animated WebP in a plain <img>: plays wherever images load, no codec or
    # autoplay questions (a VP9 <video> came up empty in Safari, #3822).
    imgs[0].save(os.path.join(ASSETS, "orchestrator.webp"), "WEBP", save_all=True,
                 append_images=imgs[1:], duration=durs, loop=0, quality=80, method=6)
    # the stills, at 2000 px: the loop's first frame, and Review Diff
    still(frames[0][0], "orchestrator")
    still(review_png, "review-diff")
    # an H.264 copy for the link (Safari and everything else play it)
    lst = os.path.join(FRAMES, "frames.txt")
    with open(lst, "w") as f:
        for p, s in frames:
            f.write(f"file '{p}'\nduration {s:.3f}\n")
        f.write(f"file '{frames[-1][0]}'\n")
    ffmpeg = os.environ.get("FFMPEG") or shutil.which("ffmpeg")
    subprocess.run([ffmpeg, "-loglevel", "error", "-y", "-f", "concat", "-safe", "0", "-i", lst,
                    "-vf", "scale=1600:-2:flags=lanczos,fps=30", "-c:v", "libx264", "-preset", "slow",
                    "-crf", "22", "-pix_fmt", "yuv420p", "-movflags", "+faststart",
                    os.path.join(ASSETS, "orchestrator.mp4")], check=True)
    for n in ("orchestrator.webp", "orchestrator.mp4", "shots/orchestrator.webp", "shots/review-diff.webp"):
        print(f"  -> {n} {os.path.getsize(os.path.join(ASSETS, n)) // 1024} KB")


if __name__ == "__main__":
    main()
