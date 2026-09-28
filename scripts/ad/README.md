# Fresh ad (30 s, vertical)

Two 30-second 1080×1920 cuts for short-video social media, from the same
recordings. Every shot of the editor is a live recording of the current build,
driven by a script, not the blog showcase GIFs.

- **`fresh-ad.mp4`**: fast. 137 BPM, a cut every bar, beat-synced keypresses.
- **`fresh-ad-calm.mp4`**: calm. 70 BPM, one long shot per bar, serif type, no flashes.

```sh
scripts/ad/build.sh                    # record all clips, render both cuts into target/ad/
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
| `music.py` | Fast cut's score: 137 BPM, D minor with a raised C♯ and G♯ (Ukrainian Dorian), reed lead, bass-heavy mix |
| `music_calm.py` | Calm cut's score: 70 BPM, D major, rolled electric-piano chords over a pad, convolution reverb |
| `engine.js` | Shared canvas compositor: terminal cells, window, captions, chips. `renderAt(t)` draws any frame |
| `ad.html` + `scenes.js` | Fast cut: page and cut list |
| `calm.html` + `calm.js` | Calm cut: page, style overrides and cut list |
| `render.mjs` | Headless Chromium renders each frame and pipes PNGs to ffmpeg (`AD_PAGE`, `AD_AUDIO` pick the cut) |

## Editing the cut

Every scene starts on a bar line (1.752 s in the fast cut, 3.43 s in the calm one). A terminal scene maps
scene time to recording time with a piecewise-linear `map` of
`[sceneSeconds, recordingSeconds]` pairs, which is how keypresses land on
beats: put a keypress's recording time (listed in the clip's `keys`) at a
multiple of `B`. A repeated scene time is a hard cut inside the recording.

The Orchestrator clip runs in an 84×45 terminal; the others are 68×36.
The fake agents are pinned to `COLUMNS=58`: in that narrow layout the agent's
pty reports more columns than the pane Fresh draws, so a cursor-up redraw
(as Ink-based agents do) leaves stale spinner lines behind. At 160 columns the
same agent redraws cleanly.
