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
| `fresh-review-syntax.json` | comparison | source highlighted inside a Review Diff stream |
| `fresh-ui-anatomy.json` | explode | the retained UI tree, one element at a time |

`assets/<clip>/fresh/config.json` is a config directory a spec copies in, so a
capture gets a deliberate theme and a known set of enabled plugins instead of
whatever the machine happens to have. `assets/fresh-review-syntax/make-repo.sh`
builds the demo repo that clip reviews — Review Diff reads a working tree, so
the diff on screen has to come from a real one.

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

**Park the caret on a blank line.** Compose mode and other rendering modes
reveal the raw source of the caret's line, which reads as a rendering flaw to
anyone who does not know the editor.

**Capture more rows than fit.** The camera pans vertically over the capture, so
a screen that exactly fills the frame has nowhere to travel.
