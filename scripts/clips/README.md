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
| `fresh-wave-logic.json` | solo, chrome | how the Wave animation actually works, explained inside Word 6.0 |
| `fresh-ide.json` | solo, chrome | what fresh is, over a buffer and a coding agent side by side |

`assets/<clip>/fresh/config.json` is a config directory a spec copies in, so a
capture gets a deliberate theme and a known set of enabled plugins instead of
whatever the machine happens to have. `assets/fresh-review-syntax/make-repo.sh`
builds the demo repo that clip reviews — Review Diff reads a working tree, so
the diff on screen has to come from a real one — and
`assets/fresh-popup-rect/make-files.sh` writes two versions of one function
straight out of git, checking that the lines it films are still the ones it
means to.

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

Debug builds paint slowly; give the pane a `settle` of 16-18s.

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
a screen that exactly fills the frame has nowhere to travel.

## The Wave clip

`fresh-wave-logic.json` uses tui-clips' `render.chrome`, so the frame is a
Microsoft Word 6.0 window and the captions are set in WordArt inside its
document. It films the `word6` built-in theme — white page, `#C0C0C0` chrome,
`#000080` selection — because the wave's blue water reads as water against
paper and as another dark rectangle against a dark theme.

`WINGDINGS.TXT` is the file it washes away: a dense field of ornaments and
nothing else. That is the argument the clip makes — `WaveEffect::init` calls a
cell a particle when it draws anything at all, a glyph or merely a background
unlike its neighbours', so a page of dingbats is flung about by exactly the
code that flings source.

Every codepoint in it is one with **no emoji presentation**. The obvious
choices — the aeroplane, the telephone, the heart, the pointing hand — fall
through fontconfig to Noto Color Emoji, which puts a single full-colour face in
an otherwise sixteen-colour frame. It then sinks into the water still in
colour, where it reads as a rendering fault rather than as a joke.

The chrome needs period fonts and Word's toolbar artwork. Both are Microsoft's,
licensed with the Windows they ship on, so they are gitignored and live outside
the tree — see `~/Documents/fresh-clip-assets`. Without them the clip still
renders, with free substitute faces and Wingdings buttons.

## The IDE clip

`fresh-ide.json` is the same shape as the Wave clip — Word 6.0 chrome, WordArt
in a left column, the copy typing itself beside it — over a recording of fresh
being used: `src/main.rs` in one pane, a coding agent in an embedded terminal in
the other, focus moving between them.

It needs its demo project built first, because it points `workdir` at a real
directory rather than the scratch copy:

```sh
./assets/fresh-ide/make-repo.sh ~/.cache/fresh-clips/ide-demo
```

A real cargo project and a real git repo, not a lone file: an agent pointed at a
directory with no manifest and no history has nothing to say about it, and the
Orchestrator's unit of work is a worktree. `make-repo.sh` checks every line fits
the split editor pane and fails if one does not — the clip halves a 140-column
capture, so a long line is a line the viewer sees cut off.

The copy is drawn from the feature table in the top-level README. Keep it that
way: a clip that claims something fresh does not do is worse than no clip.
