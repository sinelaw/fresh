# 0.4.0 Showcase Plan

Tracking the showcase GIFs for the upcoming **"What's New in Fresh (0.4.0)"**
rollup (`docs/blog/fresh-0.4.0/`). Rolls up everything since the 0.3.0 blog
(0.3.1 → 0.3.12 plus the unreleased work on `master`).

Each showcase is generated the same way: an `#[ignore]`d e2e test in
`crates/fresh-editor/tests/e2e/blog_showcases.rs` captures SVG frames, then
`scripts/frames-to-gif.sh docs/blog/fresh-0.4.0/<name>` builds the GIF.
Regenerate one with:

```
cargo test -p fresh-editor --test e2e_tests blog_showcase_fresh_0_4_0_<name> -- --ignored
scripts/frames-to-gif.sh docs/blog/fresh-0.4.0/<name>
```

## Done ✅

- [x] **orchestrator-dock** — persistent left-column session switcher; three
  independent sessions, **two running a live fake "Coding Agent"**
  (`tests/fixtures/coding_agent.py`) in their terminals, one with the file
  explorer open. Bouncing between the agents shows each further along (live
  spinner + new log lines).
- [x] **ssh-session** — start a remote SSH session from the New Session dialog
  against a real local user-space sshd (fake `demo-box` host via `/etc/hosts`):
  run commands in the remote terminal, expand the remote file explorer, open a
  remote file, then hop back to a local session via the dock.
- [x] **universal-search** — multi-scope Live Grep overlay (files / buffers /
  terminals) with Word/Regex modes, the scope toolbar, git-grep provider, and
  the live syntax-highlighted preview pane.
- [x] **wave-screensaver** — the decorative wave animation, fired from the
  palette and captured at ~26 fps with the real-time `animate` helper. Built
  with `frames-to-gif.sh --colors 32 --dither none` (new flags) since the
  flat-colour frames don't need dithering — keeps the high-fps GIF ~1.9 MB.

- [x] **terminal-path-links** — Ctrl+hover underlines a `path:line` in a
  terminal `grep` result, Ctrl+Click jumps to that file and line.
- [x] **live-diff** — enable Live Diff (vs HEAD) on a committed file, then
  type: added lines get a green `+` gutter, an edited line shows its old
  text above with a `-` gutter — updating live as the buffer changes.
- [x] **review-diff** — the reimagined review: a FILES sidebar (with a `*`
  comment badge), a side-by-side OLD/NEW view (`1`), and a comment left with
  `c` landing in the COMMENTS panel.
- [x] **agent-sessions** — the New Session dialog's **Agent:** dropdown
  (terminal / claude ↻ / aider ↻ / custom…); picking an agent fills the
  Agent Command, and the `↻ resumes on restart` legend is shown. 160-wide
  so the legend isn't truncated.

## Tier 1 — headliners still to create

*(all done — see Done above)*



## Tier 2 — worth a GIF

- [ ] **env-managers** — `Env: Activate` injects a project's venv/direnv/mise
  environment into every spawned process; opt-in `env` status-bar element.
- [ ] **go-to-lsp-symbol** — symbol finder with live preview, precise jump.
- [ ] **rainbow-brackets** — matching-bracket colorization across the viewport.
- [ ] **settings-overhaul** — tree-view categories, direct number typing,
  inline list editing, `Ctrl+R` reset-to-default.
- [ ] **theme-extends** — `extends: "builtin://dark"` inheritance + the new
  `terminal` theme that uses the host palette.
- [ ] **send-to-terminal** *(new on master)* — send the selection (or current
  line) to the integrated terminal, REPL-style, and focus it.
- [ ] **workspace-trust** *(new on master)* — the first-class, always-visible,
  clickable `{trust}` status-bar element now leading the default left status bar.

## Mention-only ("Also New" text sections — no GIF)

Editing: occurrence-highlight toggle, distribute-clipboard-across-cursors
(column paste), add-cursors-to-line-ends, move-to-paragraph, user-configurable
indentation rules, hide current-line highlight while selecting.
Terminal: tab auto-naming, scrollback survives resize/clear, nested `fresh`
opens in parent, `+` new-tab button.
File Explorer: compact directories, follow-active-buffer, natural-order sort,
Duplicate / Copy (Relative) Path.
Platform/Plugins: LSP over SSH, `lsp_enabled` master switch, `auto_read_only`,
minimal static musl binary + ~18 MB grammar trim, `git_statusbar` / status-bar
element API, `tab_actions`, plugin-registered config items, overlay toolbar
widgets, `httpFetch`.
Languages: C3, Templ, HDL (Verilog/SystemVerilog/VHDL), Racket, GDScript.

## Notes / open questions

- **Terminal-scope search**: in manual testing, universal search matched
  *retained/closed* terminal scrollback but not a *live* terminal's latest
  output — confirm whether that's intended before the blog leans on it.
- The rollup `index.md` embeds the eight finished GIFs (orchestrator-dock,
  ssh-session, universal-search, wave-screensaver, live-diff, terminal-path-
  links, review-diff, agent-sessions). **All Tier-1 GIFs are done.** Remaining work
  is Tier-2 / mention-only.
