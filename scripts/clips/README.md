# Feature clips

Short annotated videos of a fresh feature — a before/after of one change, one
screen on its own, or one screen taken apart into the elements behind it. Each
`.json` here is a spec for [tui-clips](https://github.com/sinelaw/tui-clips),
which drives a real fresh in a headless terminal, screenshots it, and renders
the result. The specs live here, next to the features they film; the renderer
lives there.

```sh
git clone https://github.com/sinelaw/tui-clips ~/repos/tui-clips
~/repos/tui-clips/bin/tui-clip ~/repos/fresh/scripts/clips/fresh-review-syntax.json
```

Needs `Xvfb`, `xfce4-terminal`, `xdotool`, ImageMagick, `ffmpeg`, and Python
with Pillow. It runs headless, so nothing touches the terminal or the editor
you have open. Captures and the finished mp4 land under `tui-clips/out/`, not
here. While iterating, `--stills` captures without rendering and
`--skip-capture` re-renders from what it already captured.

The spec format — the three clip shapes, how a beat is framed, shots, explode
trees — is documented in the tui-clips README. What follows is only what is
specific to filming *this* program.

## The clips

| Spec | Shape | Shows |
|---|---|---|
| `fresh-markdown-compose.json` | comparison | Compose mode against a released build |
| `fresh-markdown-compose-solo.json` | solo | the same, with nothing to compare against |
| `fresh-markdown-toc.json` | solo | the table-of-contents panel following the cursor |
| `fresh-popup-rect.json` | solo, before/after | one popup placed by arithmetic, then declared |
| `fresh-review-syntax.json` | comparison | source highlighted inside a Review Diff stream |
| `fresh-ui-anatomy.json` | explode | the retained UI tree, one element at a time |
| `fresh-welcome-scroll.json` | solo, stepped | the Welcome screen, scrolled from the wordmark to the theme card, then restyled live |

`assets/<clip>/fresh/config.json` is a config directory a spec copies in, so a
capture gets a deliberate theme and a known set of enabled plugins instead of
whatever the machine happens to have. `assets/fresh-review-syntax/make-repo.sh`
builds the demo repo that clip reviews — Review Diff reads a working tree, so
the diff on screen has to come from a real one — and
`assets/fresh-popup-rect/make-files.sh` writes two versions of one function
straight out of git, checking that the lines it films are still the ones it
means to. `assets/fresh-welcome-scroll/make-repo.sh` builds a small repo with no
project manifest in it, which is the only way the Welcome screen's live cards
film as live — see below.

## Filming fresh specifically

**Point it at a debug build, and check the build is current.** The specs run
`~/repos/fresh/target/debug/fresh`. Plugins are embedded into the binary at
build time (`include_dir!` over `crates/fresh-editor/plugins`), so a stale
binary silently films the *old* plugin — the feature simply will not be there,
with nothing on screen to say why. `cargo build -p fresh-editor` first, and if
a clip is meant to show a plugin change, confirm the binary has it:

```sh
strings -a target/debug/fresh | grep -c setSyntaxRegions
```

Debug builds paint slowly; give the pane a `settle` of 16-18s. A clip of
something *moving* is the one case for filming `target/release/fresh` instead:
the paint is the motion, and a debug paint is visibly slower than the thing it
is filming. Build it with `cargo build --release -p fresh-editor`, and check the
binary the same way.

**Give every pane its own `XDG_RUNTIME_DIR`.** Shared, the second pane attaches
to the daemon the first one left running and shows that pane's project root
rather than its own — a comparison clip where both sides film the same build.
`{scratch}/run-{pane}` is the fix, alongside per-pane `XDG_DATA_HOME` and
`XDG_STATE_HOME`.

**Read the band rows off the UI tree, not off a grid.** Bind a key to
`dump_ui_tree` in the clip's config asset, press it while the screen you are
filming is up, and `ctrl+s` on the read-only `*ui-tree*` buffer offers Save As.
That gives you every laid-out rect in cells, which is what an annotation band
or an explode piece wants:

```sh
~/repos/tui-clips/bin/tui-tree tree.json --list
```

Bind a key rather than running the command from the palette: the dump is of the
frame the *last* paint built, which for a palette invocation is the frame with
the palette open over everything.

**Filming code? Open the file past the function, not at it.** The editor puts
the caret's line in the middle of the viewport, so opening at `file:N+20` lands
line `N` at the top — which is how a function's doc comment ends up above the
screen rather than on it. A clip makes its own argument in its own captions;
the source's comments are not evidence, and reading them is not what the eye
should be doing.

**Park the caret on a blank line.** Compose mode and other rendering modes
reveal the raw source of the caret's line, which reads as a rendering flaw to
anyone who does not know the editor.

**Capture more rows than fit.** The camera pans vertically over the capture, so
a screen that exactly fills the frame has nowhere to travel. A clip that scrolls
the *program* instead of the camera wants the opposite: size the capture to the
frame it will be drawn in — `render.size` less `header_height` and
`caption_height` — and every beat gets the whole screen, tab bar and status bar
included, with nothing left over to travel through.

**Film motion as stepped stills, not as a recording.** `--record` films the
window with x11grab, which is right for something moving under its own clock and
wrong for a scroll: xfce4-terminal on Xvfb repaints a full-screen frame about
ten times a second, so a held `Down` lands as ten jumps of two or three rows
each however fast the recording is. One `{"key": "Down"}` and one `{"shot": ...}`
per row instead, played back as a beat's `shots` list, is the same travel at one
row a step and a step every output frame — perfectly even, because the timing
comes from the playback rather than from the terminal. 150 steps buys five
seconds at thirty a second, and costs about four minutes of capture.

Capture a row a step even when the clip wants a faster scroll than that: the
beat's speed is then a *cut*, not a re-shoot. Listing every third shot covers
the same travel in a third of the time, and keeps the cadence exact as long as
the output frames divide evenly by the steps — 70 steps over 140 frames is two
frames each; 208 steps over 148 is a stutter.

**A scroll starts late on the welcome screen.** The page moves its reading row
down the viewport before it moves the viewport, so the first screenful of `Down`
presses scrolls nothing. Send them as one `{"key": "Down", "repeat": N}` before
the stepped shots begin — and count only the rest against the row you mean to
stop on, which is a screenful lower than the arithmetic suggests otherwise.

**Work a live card with Tab, and back into it with `shift+Tab`.** There is no
click in a spec, and the page's controls do not need one: `Tab` walks the
focusable widgets in document order and `Return` activates the focused one, so
the theme swatches film as a walk along the row — `Tab`, `Return`, and the whole
editor is repainted in the next theme, chrome and all. Focus survives the
repaint, so the walk carries on from where it was. Two cautions: `Tab` from a
page at rest lands further on than you expect (the host remembers a focus you
never set), so `shift+Tab` back onto the row you want and check with a still
before filming 200 shots against the guess; and a `Tab` that leaves a card
scrolls the page to the next one, which ends the shot you were composing.

**A workspace with a project manifest opens Restricted, and a restricted
workspace has no live cards.** `Cargo.toml`, `package.json` and the rest are
executable-content markers, so filming inside fresh's own tree starts the
session Restricted — which blocks the `spawnProcess` calls the welcome screen's
finder and git cards are made of, and puts a red pill in the status bar besides.
A demo repo of prose and a couple of scripts has no marker in it, opens Trusted,
and films with its cards alive.
