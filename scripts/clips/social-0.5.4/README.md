# social-0.5.4

A ~22s square (1080×1080) video for X/Twitter: fresh keeps adding features,
and it is still light and fast. Unlike the other clips here it is not a
tui-clips spec. fresh is driven in a detached tmux pane, each screen is kept as
text with its colours (`capture-pane -e`), and the frames are painted and cut
by a small Python compositor. That keeps emoji and CJK sharp (xterm draws
neither well) and lets the camera zoom anywhere at full resolution.

| Beat | Shows |
|---|---|
| Fresh keeps growing. | fourteen real screens piling up: palette, multi-cursor, explorer, splits, live/review diff, git log, settings, keybindings, orchestrator, terminal, markdown, themes |
| Fast again. | bare `fresh` slowed down in 0.5.2 (daemon by default) and with many workspaces; 0.5.4 brings it back |
| Opens in a blink. | `fresh notes.md` typed in a shell, the captured boot frames at their real timing, then "Fast again, even with hundreds of workspaces" |
| Just the file you asked for. | `fresh notes.md` in a lived-in session: 0.5.3 brings the dock and Welcome tab, 0.5.4 opens the file alone |
| New: Markdown, three ways. | the status-bar palette hint clicked, *Cycle View Mode*: Source, Rendered, Source/Compose |

## Running it

Needs `tmux`, `ffmpeg`, Python 3 with Pillow, `wcwidth` and `fonttools`, and
the fonts JetBrains Mono, Inter, Noto Sans CJK and Noto Color Emoji
(`apt install tmux ffmpeg fonts-jetbrains-mono fonts-inter fonts-noto-cjk
fonts-noto-color-emoji`). Instrument Serif (the homepage's display face) is
fetched into the work directory.

```sh
cargo build --release -p fresh-editor
# the released build, for the before/after: 1e2c1b9 is "Bump version to 0.5.3"
git worktree add ../fresh-053 1e2c1b9 && (cd ../fresh-053 && cargo build --release -p fresh-editor)

C=scripts/clips/social-0.5.4
W=target/clips/social-0.5.4
mkdir -p $W/fonts
for f in InstrumentSerif-Regular InstrumentSerif-Italic; do
  curl -sSfL -o $W/fonts/$f.ttf \
    https://raw.githubusercontent.com/google/fonts/main/ofl/instrumentserif/$f.ttf
done
$C/make-demo.sh $W/demo
python3 $C/capture.py launch
python3 $C/capture.py montage
python3 $C/capture.py session v053 ../fresh-053/target/release
python3 $C/capture.py session v054 target/release
python3 $C/compose.py $W/fresh-0.5.4.mp4          # ~2 min
python3 $C/compose.py $W/still --still 7.9 13.9  # single frames while iterating
python3 $C/contact.py $W/caps/montage.json $W/sheet.png   # check a take
```

Everything lands under `target/clips/social-0.5.4` (`CLIP_WORK` overrides).
Each pane gets its own XDG directories and `HOME`, so a take never sees, or
leaves behind, a real session.

## The claims on screen

The clip shows no absolute timings: a first frame measured on one machine and
build is not what a viewer's install will do. Each claim below is what was
measured on official universal (musl) release builds and a master build made
the same way (`cargo build --release --features web --target
x86_64-unknown-linux-musl`), 4-vCPU Linux VM, median of 9-15 launches.

| On screen | Evidence |
|---|---|
| Fast again. | bare `fresh`, empty state: 0.30 s (0.5.1) -> 1.06 s (0.5.2), once it went through the background daemon; 0.35 s again in 0.5.3. With 200 workspaces it was still 1.69 s in 0.5.3 |
| Fast again, even with hundreds of workspaces | 200 workspaces, 800 unsaved buffers: bare `fresh` 1.69 s (0.5.3) -> 0.46 s (with #3579), against ~0.37 s for an empty state; `fresh FILE` 0.52 s -> 0.42 s, and it opens 7 workspace files instead of ~800 |
| 1 process for fresh FILE | every `fresh FILE` launch measured; a bare `fresh` runs a background daemon as well |
| no Welcome tab, dock stays closed, your session untouched | the 0.5.3/0.5.4 frames; the dock stays closed under the default `autoOpenDock: auto` (one the user pinned open stays open), and a file launch no longer writes the workspace back |
| Every feature. Light and fast: still the goal. | slogan: what we aim for, not a record; memory is unchanged release to release (~55-57 MB for a file launch) |

Not claimed, because it does not hold: a faster first frame for a single
file from an empty state. 0.5.2 paints it ~45 ms sooner than 0.5.3 or master
(338 vs 383 ms over 15 launches), though master finishes its startup work far
sooner (0.38 s vs 1.07 s).

`measure.py` is the simple harness: `fresh notes.md` in a 100x36 pty, time to
the status bar's `Ln 1`, summed RSS of every process running the binary.
Re-measure before reusing the clip for a later release.

```sh
X=$(mktemp -d); mkdir -p $X/config/fresh $X/run; chmod 700 $X/run
XDG_CONFIG_HOME=$X/config XDG_DATA_HOME=$X/data XDG_STATE_HOME=$X/state \
XDG_CACHE_HOME=$X/cache XDG_RUNTIME_DIR=$X/run \
  python3 scripts/clips/social-0.5.4/measure.py target/clips/social-0.5.4/demo 15 \
  target/release/fresh
```

## Filming notes

- **100 columns, not fewer.** At 96 the status bar has no room for the
  `Palette: Ctrl+P` hint, and the clip clicks it.
- **Mouse clicks go in as SGR reports** (`tmuxcam.Cam.click`): fresh enables
  mode 1006, so `ESC [ < 0 ; col ; row M` written into the pane is a click.
- **Leave the dock alone when seeding a session.** Toggling it open saves
  `autoOpenDock: "always"`, and a file launch honours an explicit `always`;
  only under the default `auto` does `fresh FILE` keep it closed.
- **Boot frames are grabbed at up to 60/s** and played back at their captured
  timestamps, so the cut from shell to editor in the clip is real time.
