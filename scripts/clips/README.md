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
| `fresh-orchestrator-dock.json` | solo, stepped | four worktrees, four agents, one editor — switched from the dock, ending on one agent at full size |

`assets/<clip>/fresh/config.json` is a config directory a spec copies in, so a
capture gets a deliberate theme and a known set of enabled plugins instead of
whatever the machine happens to have. `assets/fresh-review-syntax/make-repo.sh`
builds the demo repo that clip reviews — Review Diff reads a working tree, so
the diff on screen has to come from a real one — and
`assets/fresh-popup-rect/make-files.sh` writes two versions of one function
straight out of git, checking that the lines it films are still the ones it
means to. `assets/fresh-welcome-scroll/make-repo.sh` builds a small repo with no
project manifest in it, which is the only way the Welcome screen's live cards
film as live — see below. `assets/fresh-orchestrator-dock/` carries three
things of its own: the repo the clip cuts worktrees off, a `bin/` of shims that
put `claude`, `codex`, `opencode` and `aider` on `PATH`, and a
`fresh/plugins/clip_setup.ts` that builds the four workspaces at startup.

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

**The window only repaints when something asks it to, and `--shot` is what
asks.** Filming an agent working — a program that animates on its own clock,
with nobody typing — an eight-second `--record` at 30fps produced 240 frames
containing *three* distinct images, and all three landed at the moment an
`import` happened to be taken. Stills taken four seconds apart, meanwhile,
differ by 160,000 pixels. So on this stack (Xvfb + xfce4-terminal) a recording
films what the keyboard causes and little else, and the screenshot that looks
like the slow, primitive option is the one that makes a frame exist at all.
Anything self-animating is therefore filmed with `--shot`, one per frame.

**Then slow the program down to the rate the camera samples at.** A screenshot
of a full window costs 300-500ms, so a filmed pane is sampled about three times
a second. Playing those frames at thirty is a ten-times fast-forward — which
for a coding agent reads as a parody of one. The fix is to give the *subject* a
slower clock instead: `coding_agent.py --dilate 8` stretches its spinner and its
pauses by eight, so three samples a second of dilated time is thirty frames a
second of natural motion, and `--warm 9` prints the backlog a pane at
one-eighth speed would not otherwise have by the time filming starts. Measured
in the finished clip: 4.8 visible changes a second, which is what an agent's
spinner actually does.

**The switch does wipe. The camera is what cannot see it.** Arrow-navigating
the dock calls `setActiveWindowAnimated`, and the host slides everything right
of the dock in from the edge you came from — `AnimationKind::SlideIn`, 180ms.
None of it reaches a clip filmed this way: a `--record` catches one frame,
because the animation is self-driven and self-driven frames do not reach the
window (above), and `--shot` cannot help either, because `tui-capture` sleeps
1.2s after a `--key` before the next action — seven times the length of the
thing being filmed. At a small window, where a screenshot costs about 60ms
rather than 350, it is easy to catch: `110x26` at font 10 gives two or three
mid-wipe frames per switch, the outgoing workspace pushed up and the incoming
one arriving underneath.

So the clip cuts because the *camera* cuts, and it says so. A `push` transition
in the spec would not have fixed that: it would be the renderer inventing a
different motion, in a different direction, over the top of the real one.
Filming this one properly wants either a burst-of-shots step in tui-clips (fire
the key, then N screenshots with no sleep between) or the harness path the blog
showcases use, which renders every frame the editor draws.

**Keep the scaled capture off a half-pixel.** The renderer caches scaled
screens under the scale factor rounded to four decimals, so two frames whose
scale differs in the fifth can share a key — harmless until the scaled height
lands on exactly `.5`, where the two round to heights one pixel apart and the
cross-fade between two beats dies with `ValueError: images do not match`. A
140x30 capture at 1920 wide is exactly that case (1142 × 1920/2382 = 920.5);
140x29 is not. Worth knowing before blaming the capture.

**Put the agent on the left, and what checks it on the right.** Every session
in this clip is arranged that way, and one of them has **Review Diff** in the
right-hand pane rather than a file — the agent that is waiting for an answer,
with the diff of what it already changed open beside it. Two things that needs:
a file to open the split on (a split made with no file shows the pane it was cut
from, so the agent ends up on screen twice), and a wait afterwards, because
`runCommand` resolves when the command was *dispatched* and Review Diff shells
out to git. Poll `listBuffers()` for the review buffer before moving on, or the
review lands in whichever workspace the setup has reached by the time git
answers.

**A pane that gets split has to redraw its own transcript.** The host resizes an
agent's PTY the moment a split appears beside it, and a bottom-anchored pane
that answers `SIGWINCH` by clearing and redrawing only its header throws the
session away — which is how a filmed agent ends up looking like one that has
just started. The fixture keeps its committed lines and re-lays the last
screenful of them, which is what a real agent does.

**Stage a multi-window clip with a plugin, not with keystrokes.** Cutting three
worktrees through the New Workspace dialogue is thirty keystrokes, thirty
chances for a capture to desync, and none of them the thing being filmed. The
scripting API does each in one call — `orch.newWorkspace({ newBranch, agent })`
*is* what the dialogue submits — so the setup goes in a plugin in the clip's own
config asset, on the `ready` hook, and the capture opens on a workspace that is
already several tasks deep. Two details it taught: a split inherits the tab list
of the pane it was cut from, so a code pane split off an agent's terminal opens
carrying a tab for it (`closeBuffersToLeftInSplit` drops it without touching the
terminal); and `git worktree` leaves its branches behind, so the repo script has
to run before *every* capture or the second one fails with "Branch already
exists".

**The agents are `tests/fixtures/coding_agent.py`, aliased by a shim.** It is a
scripted fake — a transcript, tool calls, diff hunks, a todo list and a spinner,
none of it real — and `--as <name>` makes it rename its own process, so the tab
and the dock card read the agent's name the way a real launch would. The names
are invented (`quill`, `marlin`, `tern`, `scout`): a staged transcript filmed
under a real agent's name is a picture of that agent saying things it never
said. `--ask` stops one of them on a permission prompt and leaves it there,
which is what the dock's "which session is waiting on you" is *for*: with it,
three cards carry the working mark and the fourth does not.

**Give the capture a UTF-8 locale.** `LANG=C.UTF-8`, or xfce4-terminal decodes
the pane's box-drawing and bullet glyphs as latin-1 — three columns per
character, every line wrapped, and a program that draws a bottom-anchored pane
(any coding agent, real or fake) desynced from the first frame.

**A workspace with a project manifest opens Restricted, and a restricted
workspace has no live cards.** `Cargo.toml`, `package.json` and the rest are
executable-content markers, so filming inside fresh's own tree starts the
session Restricted — which blocks the `spawnProcess` calls the welcome screen's
finder and git cards are made of, and puts a red pill in the status bar besides.
A demo repo of prose and a couple of scripts has no marker in it, opens Trusted,
and films with its cards alive.
