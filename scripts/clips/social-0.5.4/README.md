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
| Still light. Still fast. | |
| Opens in a blink. | `fresh notes.md` typed in a shell, the captured boot frames at their real timing, then the measured numbers |
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

## The numbers on screen

`measure.py` launches `fresh notes.md` in a 100×36 pty fifteen times and
reports the median time to the first full frame (the status bar's `Ln 1`
written) and the summed RSS of every process running the binary after 2s.
Measured on a 4-vCPU Linux VM, release builds:

| | first frame | RSS | processes |
|---|---|---|---|
| 0.5.3, first run | 136 ms | 82.8 MB | 1 |
| master, first run | 135 ms | 82.3 MB | 1 |
| 0.5.3, saved session | 119 ms | 82.3 MB | 1 |
| master, saved session | 125 ms | 81.0 MB | 1 |

Hence "0.12 s · 1 process · ~80 MB · same speed as 0.5.3". Re-measure before reusing
the clip for a later release, and change the card if the numbers move.

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
