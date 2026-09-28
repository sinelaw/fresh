# Fresh ad (30 s, vertical)

A 30-second 1080×1920 ad for short-video social media. Every shot of the
editor is a live recording of the current build, driven by a script — not the
blog showcase GIFs.

```sh
scripts/ad/build.sh                    # record all clips, render target/ad/fresh-ad.mp4
SKIP_RECORD=1 scripts/ad/build.sh      # re-render only (after editing scenes.js / ad.html)
```

Needs a debug build (`build.sh` makes one), Python 3 with `pyte`, `Pillow` and
`numpy`, Node with Playwright's Chromium, and `ffmpeg` (or `pip install
imageio-ffmpeg`). Work files go to `target/ad/` (`AD_WORK` overrides).

## Pipeline

| File | Role |
|---|---|
| `setup.sh` | Work dir: demo repo, isolated XDG config (Tokyo Night), fake `claude`/`codex` shims, a 2 GB log, fonts, logo |
| `rec.py` | Drives `fresh` in a pty per clip from a key/mouse timeline; writes asciicast plus key (`k`) and mouse (`m`) marker events |
| `cast2frames.py` | Replays a cast through `pyte` into deduplicated screen snapshots at 30 fps |
| `music.py` | Synthesizes the soundtrack: 137 BPM, D minor with a raised C♯ and G♯ (Ukrainian Dorian), reed lead, bass-heavy mix |
| `ad.html` + `scenes.js` | Canvas compositor; `renderAt(t)` draws any frame. `scenes.js` holds the cut list |
| `render.mjs` | Headless Chromium renders each frame and pipes PNGs to ffmpeg |

## Editing the cut

Every scene starts on a bar line (1 bar = 1.752 s). A terminal scene maps
scene time to recording time with a piecewise-linear `map` of
`[sceneSeconds, recordingSeconds]` pairs, which is how keypresses land on
beats: put a keypress's recording time (listed in the clip's `keys`) at a
multiple of `B`. A repeated scene time is a hard cut inside the recording.

The Orchestrator clip runs in an 84×45 terminal; the others are 68×36.
The fake agents are pinned to `COLUMNS=58`: in that narrow layout the agent's
pty reports more columns than the pane Fresh draws, so a cursor-up redraw
(as Ink-based agents do) leaves stale spinner lines behind. At 160 columns the
same agent redraws cleanly.
