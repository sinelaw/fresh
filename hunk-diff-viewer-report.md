# Exploring `hunk` — a review-first terminal diff viewer

**Date:** 2026-06-07
**Tool:** [`modem-dev/hunk`](https://github.com/modem-dev/hunk) v0.14.1 (npm package `hunkdiff`)
**Test repo:** `sinelaw/fresh` (~97 MB, 1,702 tracked files, 877 Rust sources)
**Method:** Driven interactively inside `tmux`; panes captured with `tmux capture-pane -e` to preserve true-color output.

---

## 1. What `hunk` is

There are several unrelated tools named "hunk" on GitHub. The one explored here is
**`modem-dev/hunk`** — *"a review-first terminal diff viewer for agent-authored
changesets,"* built on **OpenTUI** and **Pierre diffs**. It is purpose-built for the
workflow of reviewing changes produced by AI coding agents: instead of dumping a
unified diff as scrollable text (like `git diff` or even `delta`), it opens the
changeset in a structured, navigable **review UI** with a file sidebar, side-by-side
panes, syntax highlighting, and — the headline feature — **inline review comments that
an agent can place programmatically** via a local daemon.

Other tools sharing the name (not tested): `roasbeef/hunk` (Go CLI for sparse commits),
`wkentaro/git-hunk` (Python non-interactive staging), `smolcars/hunk` (GPUI desktop app).

### Installation

```bash
npm i -g hunkdiff          # what I used; needs Node 18+ (had v22.22.2)
# alternatives: brew install modem-dev/tap/hunk  |  Nix flake
```

Install was clean (~175 deps, ~21 s) and the binary landed at `hunk` on `PATH`.

---

## 2. Command surface

```
hunk diff [target] [-- <pathspec>]   review working tree (or compare vs a ref/revset)
hunk diff --staged                   review staged changes
hunk diff <left> <right>             compare two concrete files
hunk show [target]                   review last commit / a ref
hunk stash show [ref]                review a stash entry
hunk patch [file|-]                  review a patch file or stdin
hunk pager                           drop-in Git/jj/Sapling pager
hunk difftool <left> <right> [path]  Git difftool integration
hunk session <subcommand>            inspect/control a *live* session via daemon
hunk skill path                      print bundled agent "skill" doc
hunk daemon serve                    run the local session daemon
```

Common review flags (work on `diff`/`show`/`pager`): `--mode auto|split|stack`,
`--theme <name>`, `--watch`, `--line-numbers/--no-line-numbers`, `--wrap/--no-wrap`,
`--hunk-headers/--no-hunk-headers`, `--agent-notes/--no-agent-notes`,
`--exclude-untracked`, `--agent-context <json>`. It auto-detects **Jujutsu** and
**Sapling** checkouts and uses native revsets there.

---

## 3. Test setup

`fresh` is a Rust terminal editor — a genuinely large, multi-crate repo. I created a
non-trivial, mixed working-tree changeset purely as review fodder:

| File | Change | Shape |
|------|--------|-------|
| `README.md` | added a release-note blockquote | 1 hunk, +2 |
| `…/app/path_utils.rs` | new doc lines + new `is_within()` fn + `#[cfg(test)]` module | 2 hunks, +30 |
| `…/app/types/theme.rs` | new struct fields + an `impl` block | 2 hunks, +12 |
| `…/app/workspace_guard.rs` | brand-new untracked file | new file, +45 |

I drove the TUI in a 220×50 `tmux` pane (`TERM=tmux-256color`, 256 colors) and also
exercised the daemon CLI. *(All scratch edits were reverted afterward — the repo is
left clean; only this report is added.)*

---

## 4. What the UI looks like

`hunk diff` opens a three-region layout:

- **Top menu bar** — `File · View · Navigate · Theme · Agent · Help`, plus a right-aligned
  title (`fresh working tree`) and a running `+89 −0` total.
- **Left sidebar** — the file list, each row showing a VCS status glyph (`M` modified,
  `?` untracked), the basename, a per-file `+N/−N` count, and a `*N` badge once a file
  carries review comments. Parent directories are shown as dimmed group headers.
- **Right pane** — the diff itself. In **split** mode it's side-by-side old/new with
  independent line-number gutters, `@@ … @@` hunk headers, and **collapsible "N unchanged
  lines" folds** between hunks. Word-level intraline highlighting is applied within
  changed lines.

A representative split-view capture (Midnight theme):

```
  File  View  Navigate  Theme  Agent  Help                       fresh working tree  +89  -0
 ──────────────────────────────────┬──────────────────────────────────────────────────────
   M README.md                  +2 │ README.md                                      +2 -0
  crates/fresh-editor/src/app/     │▌▾ 13 unchanged lines
   M path_utils.rs             +30 │▌@@ -14,6 +14,8 @@ Fresh brings the intuitive UX …
  crates/fresh-editor/src/app/typ. │▌14                                ▌14
   M theme.rs                  +12 │▌15   Built for real-world perfor… ▌15   Built for real-world perfor…
  crates/fresh-editor/src/app/     │▌16                                ▌16
   ? workspace_guard.rs        +45 │▌                                 ▌17 + > **New in this release:** …
```

---

## 5. Features exercised

### Layout modes
- **`1` Split** — side-by-side, the default for wide terminals.
- **`2` Stack** — unified/inline view; both old & new line numbers appear in two narrow
  gutters (`15 15`, `17 +`, `12 15`), so you keep dual-number context without the
  horizontal split.
- **`0` Auto** — picks split vs. stack based on terminal width.

### Navigation (all confirmed live)
`[`/`]` prev/next **hunk** · `,`/`.` prev/next **file** · `{`/`}` prev/next **comment**
· `↑`/`↓` line · `Space`/`b` page · `d`/`u` half-page · `g`/`G` and `Home`/`End` top/bottom
· `←`/`→` horizontal code scroll. Mouse wheel scrolls; `Shift`+wheel scrolls horizontally.
Pressing `.` correctly advanced the daemon's reported focus to the next file.

### View toggles
`z` collapses/expands the unchanged-context folds (label flips to "Hide N unchanged
lines" and the full file body appears). `s` sidebar, `a` agent notes, `l` line numbers,
`w` wrap, `m` hunk metadata. The **View** menu mirrors these as live checkboxes, so it
doubles as a status readout of current toggle state.

### Themes
`Theme` menu lists **Graphite, Midnight (default), Paper, Ember, Catppuccin Latte,
Catppuccin Mocha**. Switching is instant and total — verified in the `-e` captures:
Midnight renders on a near-black background (`rgb(23,26,29)`) with green additions
(`rgb(136,211,155)`); selecting **Paper** flipped the whole canvas to a cream background
(`rgb(255,250,243)`) with dark-brown text (`rgb(47,36,23)`). Full syntax highlighting is
present (Rust keywords in pink `rgb(255,103,141)`, strings in orange `rgb(255,163,89)`,
etc.), so it's true syntax-aware coloring, not just add/remove tinting.

### Help & menus
`?` opens a complete keymap overlay. `F10` opens the menu bar; `←`/`→` move between
menus (File / View / Navigate / Theme / Agent / Help), each a normal dropdown that also
shows the bound key beside every item. Menus are mouse-selectable too.

### `show` / range diffs / reload
Via the daemon I swapped the *same* live window between inputs without relaunching:
- `hunk session reload --repo . -- show HEAD` → retitled to "fresh show HEAD +18 −16"
  and rendered the last commit (this one had real red deletions + per-character
  intraline highlights on SVG/snapshot edits).
- `hunk session reload --repo . -- diff HEAD~3` → a 24-file, +258/−42 range diff,
  including i18n JSON files whose multibyte/Unicode content (German, etc.) rendered
  correctly.

### File filter
`/` focuses a filter box; typing `locale` instantly narrowed the sidebar to the
`locales/*.json` files. `Tab` toggles focus between the file list and the filter.

---

## 6. The standout feature — agentic inline review

This is what differentiates `hunk` from `delta`/`difftastic`/`git diff`. A background
**daemon** tracks every live TUI session, and a scriptable `hunk session` CLI lets a
non-interactive agent *inspect and annotate the human's open review window*:

```bash
hunk session list                       # discover live sessions
hunk session get --repo .               # path/repo/source + file & hunk summary
hunk session review --repo . --json     # full structured file/hunk model (ranges, headers)
hunk session context --repo .           # current cursor focus (file/hunk/line)
hunk session navigate --repo . --file F --hunk N        # move the human's cursor
hunk session comment add  … --file F --new-line N --summary … --rationale … --focus
hunk session comment apply --stdin      # batch-apply many notes from a JSON payload
hunk session reload --repo . -- show HEAD~1             # swap what's under review
```

`session review --json` returns a clean machine model — `selectedFile`, `selectedHunk`,
and a `files[]` array each with `additions`, `deletions`, `hunkCount`, and per-hunk
`header` + `oldRange`/`newRange`. `--include-patch` adds raw unified text only on demand.

I added a note with:

```bash
hunk session comment add --repo . \
  --file crates/fresh-editor/src/app/path_utils.rs --new-line 52 \
  --summary "starts_with is component-wise, good — confirm both paths are absolute first" \
  --rationale "is_within on two relative paths could give a false positive…" \
  --author agent --focus
```

…and the TUI **immediately** drew a bordered callout anchored between the diff lines:

```
 ╭─ agent note - crates/fresh-editor/src/app/path_utils.rs R52 ──────────────────╮
 │ starts_with is component-wise, good — confirm both paths are absolute first    │
 │ is_within on two relative paths could give a false positive; callers should    │
 │ pass canonical roots                                                           │
 ╰────────────────────────────────────────────────────────────────────────────────╯
```

The sidebar row gained a `*1` badge, `{`/`}` jump between annotated hunks, and
`comment list --type all` enumerates notes with author/hunk/body. The mental model:
**the agent reviews and leaves margin notes; the human reads them in a real UI** — the
inverse of the usual "human reviews the agent" flow, and notably without the agent ever
touching the interactive TTY itself (the bundled `hunk skill` doc explicitly tells agents
*not* to run `hunk diff` directly, but to drive the daemon).

---

## 7. Observations & verdict

**Strengths**
- Genuinely polished TUI: split/stack/auto, folds, syntax highlighting, six themes,
  full keyboard + mouse + menu navigation, discoverable `?` help.
- True-color rendering confirmed end-to-end (24-bit SGR in `capture-pane -e`).
- The daemon + `session` CLI is the real innovation — a clean, JSON-first control plane
  that cleanly separates "the human's interactive window" from "the agent's automation."
- Drop-in Git ergonomics: `hunk pager`, `difftool`, `patch -`, `--staged`, ref/range
  targets, and native jj/Sapling support.
- Live `reload` lets one window cycle through working-tree → commit → range without
  relaunching.

**Caveats / notes**
- Node 18+ runtime dependency (heavier than a single Rust/Go binary like `delta`).
- It's review-oriented, not a stager — no hunk-level staging like `git add -p`
  (that's a different "hunk" tool).
- The agent-review model is the whole point; if you just want a prettier `git diff`
  pager, it works, but that's underusing it.

**Bottom line:** `hunk` is best understood not as a `delta` competitor but as a
**review surface for agent-authored changes** — the interactive viewer is for the human,
and the daemon/CLI is for the agent to navigate and annotate that view. For a workflow
where an AI writes code and a human (or another agent) reviews it, the inline-note
mechanism is the compelling differentiator. For plain diff viewing it's pleasant and
capable but heavier than the minimalist alternatives.

---

## Sources

- [modem-dev/hunk — GitHub](https://github.com/modem-dev/hunk/)
- [hunk README](https://github.com/modem-dev/hunk/blob/main/README.md)
- [hunk releases](https://github.com/modem-dev/hunk/releases)
- [altbox — hunk](https://altbox.dev/tool/hunk/)
- Bundled `hunk-review` SKILL.md (`hunk skill path`)
