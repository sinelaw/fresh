# Fresh ad (30 s, vertical)

Two 30-second 1080×1920 cuts for short-video social media, from the same
recordings. Every shot of the editor is a live recording of the current build,
driven by a script, not the blog showcase GIFs.

- **`fresh-ad-calm.mp4`**: calm. 70 BPM, one long shot per bar, serif type, no flashes.
  The Orchestrator gets two bars as three close-ups: the dock's workspaces, an
  agent split beside the code, and the main checkout's Review Diff. The end card
  carries six slow rows of the rest of the features.
- **`fresh-ad.mp4`**: fast. 137 BPM, a cut every bar, beat-synced keypresses.

```sh
scripts/ad/build.sh                    # record all clips, render both cuts into target/ad/
SKIP_RECORD=1 scripts/ad/build.sh      # re-render only (after editing scenes.js / ad.html)
```

Needs a debug build (`build.sh` makes one), Python 3 with `pyte`, `Pillow` and
`numpy`, Node with Playwright's Chromium, and `ffmpeg` (or `pip install
imageio-ffmpeg`). Work files go to `target/ad/` (`AD_WORK` overrides).

The landing page (`homepage/index.html`) uses the same material: its stills
are crops of these recordings and its hero plays the calm cut.
`scripts/ad/site.sh` refreshes both after a `build.sh` run.

## Pipeline

| File | Role |
|---|---|
| `setup.sh` | Work dir: demo repo, isolated XDG config (Tokyo Night), fake `claude`/`codex` shims, a 2 GB log, fonts, logo |
| `rec.py` | Drives `fresh` in a pty per clip from a key/mouse timeline; writes asciicast plus key (`k`) and mouse (`m`) marker events |
| `cast2frames.py` | Replays a cast through `pyte` into deduplicated screen snapshots at 30 fps |
| `music.py` | Fast cut's score: 137 BPM, D minor with a raised C♯ and G♯ (Ukrainian Dorian), reed lead, bass-heavy mix |
| `music_calm.py` | Calm cut's score: 70 BPM, D major, rolled electric-piano chords over a pad and sub, convolution reverb |
| `engine.js` | Shared canvas compositor: terminal cells, window, captions, chips. `renderAt(t)` draws any frame |
| `ad.html` + `scenes.js` | Fast cut: page and cut list |
| `calm.html` + `calm.js` | Calm cut: page, style overrides and cut list |
| `render.mjs` | Headless Chromium renders each frame and pipes PNGs to ffmpeg (`AD_PAGE`, `AD_AUDIO` pick the cut) |
| `shots.html` + `shots.js` | Website stills: a crop of one recorded screen, drawn at 2× |
| `site.sh` | Writes the stills and a 720×1280 encode of the calm cut into `homepage/public/assets/` |
| `favicon.py` | Cuts the leaf out of `docs/logo.png`, brightens it, and writes the landing page's favicons |

## Editing the cut

Every scene starts on a bar line (1.752 s in the fast cut, 3.43 s in the calm one). A terminal scene maps
scene time to recording time with a piecewise-linear `map` of
`[sceneSeconds, recordingSeconds]` pairs, which is how keypresses land on
beats: put a keypress's recording time (listed in the clip's `keys`) at a
multiple of `B`. A repeated scene time is a hard cut inside the recording.

Every clip is recorded at one size, 140×75: wide enough for the dock and a
split, and the same aspect as the vertical window. A shot's camera `z` zooms
into that screen (1 = all of it); shots anchor to the left edge (`fx: 0`) so the
gutter is never cropped, and the palette anchors bottom-left so its input line
stays in frame.

The Orchestrator clip seeds a 24-column dock (`chrome.json`, the width a drag
would store) and lets the agents run for 80 s before filming so their panes are
full. The fake agents are pinned to the width of the pane they are filmed in
(`claude` 112 columns, `codex` 55 in half a split). Without that, in a narrow
layout the agent's pty reports more columns than the pane Fresh draws, and a
cursor-up redraw (as Ink-based agents do) leaves stale spinner lines behind.
