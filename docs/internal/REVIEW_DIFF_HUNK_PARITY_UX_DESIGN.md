# Review Diff v2 — "Hunk-feel" review surface (UX design + forward plan)

Status: **forward-looking design**. Nothing here is built yet. This document
specifies a second-generation Review Diff experience that reproduces the
look-and-feel of [`hunk`](https://github.com/modem-dev/hunk) (the review-first
terminal diff viewer) **using Fresh's existing primitives correctly**, while
keeping the things Fresh's review tool already does that `hunk` cannot
(hunk/line staging, in-place editing, comment persistence + export).

Companion documents:
- `fresh-vs-hunk-review-gaps.md` (repo root) — the factual feature comparison this design closes.
- `PLAN-git-log-diff-folding-and-highlighting.md` — the syntect diff-highlighting + scalable folding work this design depends on (Part 2 especially).
- `REVIEW_DIFF_COMBINED_UX_REPORT.md`, `REVIEW_DIFF_REMAINING_ISSUES.md` — bugs/known-issues in the v1 tool (`audit_mode` plugin) that v2 must not regress.
- `SIDE_BY_SIDE_HUNK_NAV_REBINDABLE.md` — the composite-buffer hunk-nav rebinding work that the split layout reuses.
- `UNIFIED_UI_FRAMEWORK_PLAN.md` — the controls library (`text_list`, `dropdown`, `scroll_panel`, `button`) that the sidebar, menu bar, and theme picker should be built on.
- `buffer-groups-design.md` — the buffer-group/panel model the whole surface lives in.

---

## 1. Thesis

> Make Review Diff *read* as well as `hunk` and *act* as well as Fresh.

`hunk` is a better **reader** (live split/stack/auto layouts, syntax highlighting,
a real file sidebar, diff-specific themes, an agent control CLI, a `?` help
overlay, multi-line bordered inline notes). Fresh's v1 review tool is a better
**actor** (real `git add -p`-style staging/unstaging/discarding, jump-to-and-edit
the real file, comment persistence, Markdown/JSON export, PR-branch/range modes).

v2's job is to take v1's actor model and give it `hunk`'s reading ergonomics,
**without inventing new rendering machinery** — every surface below maps onto a
primitive Fresh already exposes to plugins or already renders in core.

### Design constraints (non-negotiable)
1. **No new bespoke renderer.** Compose from buffer-group panels, virtual
   buffers, composite buffers, overlays, virtual lines, folds, and the controls
   library. If a surface can't be expressed with these, the gap is a *host
   primitive* to add deliberately (see §12), not a one-off draw call in the plugin.
2. **Everything keyboard-driven is a rebindable `Action` in a `diff-view` mode**,
   discoverable in the keybinding editor — not hardcoded in a router (the v1
   mistake tracked by `SIDE_BY_SIDE_HUNK_NAV_REBINDABLE.md`).
3. **Colors come from the theme**, via documented `editor.diff_*` / `syntax.*`
   keys, never hardcoded RGB. (`hunk` ships its own diff themes; Fresh's answer is
   theme keys that every built-in theme defines, so the review inherits the
   user's active theme automatically and looks native.)
4. **Don't regress the v1 actor features** listed in §10.

---

## 2. What we are matching (the convenient `hunk` UX, enumerated)

This is the checklist v2 must cover. Each item is designed in §5–§9.

| # | `hunk` affordance | Default key (hunk) |
|---|---|---|
| H1 | Live **split / stack / auto** layout toggle | `1` / `2` / `0` |
| H2 | **File sidebar**: status glyph, `+/-` counts, `*N` comment badge, dir group rows | (always on; `s` toggles) |
| H3 | **Syntax highlighting** on both diff sides | (always) |
| H4 | **Word-level intraline** change highlight | (always) |
| H5 | **Collapsible "N unchanged lines"** context folds | `z` toggle |
| H6 | **Inline bordered review notes** anchored to a line (summary + rationale) | `c` |
| H7 | **Comment navigation** between annotated hunks | `{` / `}` |
| H8 | **Hunk / file navigation** | `[` `]` / `,` `.` |
| H9 | **Top menu bar** File/View/Navigate/Theme/Agent/Help + running `+N -N` total | `F10` |
| H10 | **`?` keymap overlay** | `?` |
| H11 | **Toggles**: line numbers / wrap / hunk metadata / agent notes / context | `l` `w` `m` `a` `z` |
| H12 | **Diff-specific themes**, switched live | Theme menu |
| H13 | **File filter** | `/` |
| H14 | **Watch / auto-reload** on working-tree change | `--watch` |
| H15 | **Inputs**: working tree, staged, ref/range, commit, stash, patch/stdin | subcommands |
| H16 | **Agent control surface** (inspect/navigate/comment programmatically) | `hunk session …` |
| H17 | **Open file in editor** from a hunk | `Enter` / `Alt+o` |

---

## 3. Target layout (wireframes)

The review is a **buffer group** (one tab, `*Review*`) with named panels, exactly
like v1 — but with a first-class **sidebar** panel and a **layout-aware content**
panel. The menu bar is the editor's existing top menu bar, contextually extended.

### 3.1 Split mode (`1`) — the default on wide terminals

```
 File  Edit  View  Selection  Go  Review  ▸ Navigate  Theme        review · working tree   +89 −0
┌─ files ───────────────┬─ content (split) ───────────────────────────────────────────────────┐
│ ▾ src/app             │ path_utils.rs                                              +30 −0      │
│   M path_utils.rs *1 +30   @@ -9,6 +9,9 @@ fn normalize_path …                                 │
│   M theme.rs       +12 │  9 │/// Normalize a path …      │  9 │/// Normalize a path …          │
│ ▾ (root)              │     │                            │ 12 +│/// Note: lexical only …       │
│   M README.md      +2 │ 12 │pub(crate) fn normalize…    │ 15 │pub(crate) fn normalize…        │
│   ? workspace_guard.rs│ ╭─ note · R52 ─────────────────────────────────────────────────────╮  │
│     +45               │ │ starts_with is component-wise — confirm both paths are absolute   │  │
│                       │ ╰───────────────────────────────────────────────────────────────────╯ │
└───────────────────────┴───────────────────────────────────────────────────────────────────────┘
 diff-view · path_utils.rs · Hunk 1/2 · [n]ext [p]rev  [c]omment  [s]tage  ? help        LF UTF-8
```

- **Left**: sidebar panel (file tree-ish list, §5.2).
- **Right**: a **composite buffer** showing OLD│NEW (the v1 side-by-side machinery,
  promoted to be the primary view — see `SIDE_BY_SIDE_HUNK_NAV_REBINDABLE.md`).
- **Inline note**: a bordered box rendered with **virtual lines** below R52 (§5.6).
- **Menu bar**: the editor menu bar gains a contextual **Review / Navigate / Theme**
  set while the review buffer is focused (§5.9).

### 3.2 Stack mode (`2`) — unified, the v1 look refined

```
┌─ files ───────────────┬─ content (stack) ───────────────────────────────────────────────────┐
│ … sidebar …           │ ▾ UNSTAGED (2)                                                         │
│                       │ ▾ README.md                                              +2 −0         │
│                       │ ⋯ 13 unchanged lines ⋯                                                  │
│                       │ 15 15  Built for real-world performance …                              │
│                       │    17 + > **New in this release:** …                                   │
│                       │ ▾ path_utils.rs                                          +30 −0         │
│                       │ …                                                                       │
└───────────────────────┴─────────────────────────────────────────────────────────────────────┘
```

- **Content** is a single **virtual buffer** (`createVirtualBuffer` +
  `setVirtualBufferContent`) the plugin composes — this is v1's unified view, kept
  but upgraded with syntax highlighting (§5.3), real folds (§5.5), and the bordered
  note style (§5.6).

### 3.3 Auto mode (`0`)
Pick split when `content` width ≥ a threshold (default 140 cols, configurable),
else stack. Recompute on the `resize` event (the v1 resize bug —
`REVIEW_DIFF_COMBINED_UX_REPORT.md` BUG-2 — must be fixed here by driving relayout
from the host group layout, not async plugin rebuild).

---

## 4. Architecture mapping (the "use Fresh correctly" core)

| Surface | Fresh primitive | API |
|---|---|---|
| The review container | **Buffer group** (one tab) with named panels | `createBufferGroup`, `setBufferGroupPanelBuffer`, `focusBufferGroupPanel` |
| Toolbar / status hint line | fixed-height panel of `TextPropertyEntry` | `setPanelContent` |
| File sidebar | panel backed by the **`text_list` control** (UNIFIED_UI_FRAMEWORK_PLAN) | `setPanelContent` + controls |
| Stack (unified) content | **virtual buffer** | `createVirtualBuffer`, `setVirtualBufferContent` |
| Split (side-by-side) content | **composite buffer** (OLD│NEW) | `createCompositeBuffer`, `getCompositeCursorInfo` |
| Syntax highlighting | **syntect**, via the diff-grammar + `HighlightCategory` bg work | host (PLAN-git-log Part 2) |
| Word-level intraline | **overlays** (the `live_diff` mechanism) | `addOverlay`, `clearOverlaysInRangeForNamespace` |
| Context folds | **folding ranges** + standard `toggle_fold` | `setFoldingRanges`, `addFold`, `clearFolds` |
| Inline note boxes | **virtual lines / styled virtual text** | `addVirtualLine`, `addVirtualTextStyled`, `clearVirtualTextNamespace` |
| Menu bar entries | core **menu** contributions + contextual menus | menu contribution API + `setContext` |
| `?` help overlay, theme picker | **floating panel control** | `floatingPanelControl` |
| File filter | **prompt** with live suggestions | `startPrompt`, `setPromptSuggestions`, `setPromptToolbar` |
| Keymap | **`diff-view` buffer mode** + rebindable `Action`s | `defineMode`, `setContext`, keybindings |
| Watch | file-watcher event → debounced refresh | `editor.on('…')` |
| Agent control | the native **`ReviewHunk` / `SetReviewDiffHunks`** path + a review-session command surface | `PluginCommand`, §9 |

The key realization: **Fresh already has two diff renderers** — the composite
buffer (real two-pane, real file content, real per-language syntect) and the
plugin-composed virtual buffer (v1 unified). v2 does not build a third; it makes
**split = composite** and **stack = virtual buffer**, and the layout toggle simply
swaps which one occupies the `content` panel.

---

## 5. Feature-by-feature design

### 5.1 Layout modes — H1 (`1` split / `2` stack / `0` auto)
- State: `state.layout: 'split' | 'stack' | 'auto'`, persisted in the review
  session file alongside comments (v1 already persists to `.review/`).
- `setLayout(mode)` swaps the `content` panel's buffer:
  - **stack** → the virtual buffer (`setBufferGroupPanelBuffer(group, 'content', stackBufId)`).
  - **split** → the composite buffer for the focused file
    (`createCompositeBuffer` lazily per file; cache by file id).
- Cursor/selection/scroll position is preserved across the swap by translating the
  focused **hunk id + side + line** (we have `getCompositeCursorInfo` and the
  virtual buffer's row map). Same anchor model both ways.
- `auto` subscribes to `resize`; recompute mode when the `content` panel width
  crosses the threshold. Relayout is driven by the **host group layout**, never by
  an async plugin rebuild (fixes BUG-2).
- Bound as Actions `review_layout_split/stack/auto` in `diff-view` mode.

### 5.2 File sidebar — H2
- A left panel (replaces leaning on the global File Explorer, which caused v1
  BUG-3 focus theft). Built on the **`text_list` control** so it gets scrolling,
  selection, and hit-testing for free (UNIFIED_UI_FRAMEWORK_PLAN).
- Each row carries: directory-group header (dimmed), status glyph (`M`/`A`/`D`/`R`/`?`),
  basename, right-aligned `+N −N`, and a `*N` **comment badge** when the file has
  notes. Glyph/count colors from theme `editor.diff_*` + `vcs.*` keys.
- Grouping is **selectable**: by directory (hunk-style) *or* by git index state
  (Fresh's STAGED/UNSTAGED/UNTRACKED — kept for the actor workflow). `View ▸ Group by`.
- Selecting a row scrolls `content` to that file's first hunk; `,`/`.` move the
  selection (H8). `s` toggles sidebar visibility (panel show/hide), mirroring hunk.

### 5.3 Syntax highlighting — H3 (the biggest visible gap)
- **Split**: free. The composite buffer holds real file content with the file's
  real extension, so syntect already runs per-language. The only fix needed is
  honoring **diff background** on the add/remove rows — see PLAN-git-log Part 2
  (`HighlightCategory::{Inserted,Deleted,Changed}` + whole-line bg fill). Result:
  green/red wash *plus* full token coloring, exactly like hunk.
- **Stack**: the virtual buffer is plugin-composed text (gutters + `+`/`-` + code).
  Two layers:
  1. **Diff scopes** via syntect's bundled `Diff` grammar give the +/-/@@ wash
     (same host work as split).
  2. **Code token coloring** of the payload: set per-file *language regions* on the
     virtual buffer so syntect tokenizes the code columns with the file's grammar.
     Where region-tagging a synthetic buffer is impractical, fall back to
     plugin-side `addOverlay` spans computed from the composite buffer's already-
     tokenized lines (we have them in split). Prefer the host path; overlays are the
     256 KB-gated fallback (matches `live_diff`).
- Net host cost is the ~120 LOC already scoped in PLAN-git-log Part 2; v2 is its
  first real consumer.

### 5.4 Word-level intraline — H4
- Reuse the `live_diff` word-range mechanism: compute intra-line add/remove spans
  (token or char diff) and emit them as overlays in a dedicated namespace
  (`review:worddiff`), cleared per refresh with `clearOverlaysInRangeForNamespace`.
- Theme keys `editor.diff_add_emphasis_bg` / `editor.diff_remove_emphasis_bg`
  (brighter than the line wash). Works in both layouts (overlays apply to composite
  and virtual buffers alike).

### 5.5 Context folds ("N unchanged lines") — H5
- Use the **folding-range** path from PLAN-git-log Part 1, not bespoke collapsing.
  The plugin publishes a fold per inter-hunk context gap via `setFoldingRanges`;
  the standard `toggle_fold` key (and `z`) expand/collapse them.
- The collapsed affordance row (`⋯ 13 unchanged lines ⋯`, clickable to expand) is a
  **styled virtual line** (`addVirtualTextStyled`) on the fold's first row, so it
  reads like hunk's "N unchanged lines" fold marker. Clicking or `z` toggles.
- `z a` / `z r` (fold-all / unfold-all) reuse the existing fold-all actions.

### 5.6 Inline review notes — H6 (bordered, multi-line)
- v1 renders a single truncated line (`» [20] text`) plus a side panel. v2 renders
  hunk-style **bordered boxes** inline using **virtual lines**:
  `addVirtualLine` for each box row, styled via `addVirtualTextStyled`, in
  namespace `review:notes`. The box shows **summary + rationale + author**, wraps to
  the content width, and is anchored under the commented line on the correct side.
- Because virtual lines are host-rendered insertions (not buffer text), they:
  - don't perturb diff line numbers or fold ranges,
  - survive layout swaps (re-emitted from the note model on each `setLayout`),
  - clear cleanly with `clearVirtualTextNamespace('review:notes')`.
- The right-hand **COMMENTS panel** stays (Fresh edge: a scannable index + export
  source) but is made multi-line and scrollable (fixes the v1 truncation noted in
  `fresh-vs-hunk-review-gaps.md`). It becomes a **`text_list`** panel; `Enter` on a
  row jumps the content to that note.
- Note model is unchanged from v1 (persisted in `.review/`), so export (Markdown /
  JSON) and PR-branch/range reviews keep working.

### 5.7 Comments + hunk/file navigation — H7, H8
- Actions in `diff-view` mode, all rebindable:
  `review_next_hunk` `[` `]` → `review_prev_hunk`; `review_next_file` `,` `.`;
  `review_next_comment` `{` `}` (jump between annotated hunks; reuse the note model).
- Split hunk-nav reuses `composite_next_hunk_active` (per
  `SIDE_BY_SIDE_HUNK_NAV_REBINDABLE.md`); stack hunk-nav uses the virtual buffer's
  row→hunk map. Both call the same `Action`, so the key is layout-independent.

### 5.8 Themes — H12
- No new theme *engine*; add **diff-review theme keys** to the schema and every
  built-in theme (see §8), then the review inherits the active editor theme — so it
  looks native, and switching the editor theme reskins the review live.
- A **Theme picker** (hunk's Theme menu) is a `floatingPanelControl` listing
  `getAllThemes()`; selecting calls `reloadAndApplyTheme(name)`. This is a thin
  wrapper over the existing theme system, surfaced from the review menu bar for
  parity, but it switches the *whole editor* theme (correct Fresh behavior — we do
  not fork a review-only palette).

### 5.9 Menu bar + `?` help — H9, H10
- While the review buffer is focused (`setContext('review-mode', true)`), the
  editor menu bar shows contextual **Review / Navigate / Theme** menus contributed
  via the standard menu API (the same mechanism plugins already use). Items mirror
  the keymap and show their (rebindable) keys — so `F10` → menus works for free.
- `?` opens a **floating help overlay** (`floatingPanelControl`) rendering the
  current `diff-view` keymap from the keybinding registry (so it stays correct when
  users rebind). This is the discoverability win over v1's truncatable 2-line hint
  bar (also fixes BUG-10 — the hint bar becomes secondary).

### 5.10 View toggles — H11
- `l` line numbers, `w` wrap, `m` hunk-metadata rows, `a` agent-notes visibility,
  `z` context folds — each a boolean in `state`, applied via the relevant view
  setting (`setViewMode` / virtual-buffer recomposition / fold publish) and
  reflected as checkboxes in the **View** menu (parity with hunk's View menu, which
  doubles as a live status readout).

### 5.11 File filter — H13
- `/` starts a **prompt** (`startPrompt`) scoped to the review's file set, with
  `setPromptSuggestions` live-filtering the sidebar. `Tab` toggles focus between the
  filter and the file list (the v1 behavior, kept). Matches narrow the sidebar and
  the stack content to matching files.

### 5.12 Watch / auto-reload — H14
- Subscribe to the workspace file-watcher events Fresh already emits; debounce
  (~150 ms) and re-run the diff fetch, diffing the new hunk set against the old to
  **preserve comment anchors and fold/scroll state** (the note model already prunes
  orphans — `pruneOrphanComments`). Toggle via `View ▸ Watch` / an Action; off by
  default to match the explicit-refresh muscle memory, opt-in like hunk's `--watch`.

### 5.13 Inputs — H15
- Keep v1's **working tree / staged / range / PR-branch** commands. Add, for hunk
  parity:
  - **stash review**: `git stash show -p <ref>` → same hunk model (`Review Stash`).
  - **patch / stdin review**: open a `.patch` or piped diff into the review model
    (`Review Patch`), so `fresh - <<<"$(git diff)"`-style flows work. This also makes
    the review usable as a `git difftool` / pager target later.
- All inputs converge on one `ReviewModel` (files → hunks → lines), so every UI
  surface above is input-agnostic.

### 5.14 Open file / edit-in-place — H17 (Fresh edge, keep)
- `Enter` jumps to the source location and `Alt+o` opens the real file for editing
  in the same editor — v1 behavior retained and surfaced in the Navigate menu.
- This is strictly better than hunk (which shells out to `$EDITOR`); preserve it.

---

## 6. Keymap (default; all rebindable in `diff-view` mode)

| Action | Default | hunk parity |
|---|---|---|
| `review_layout_split` / `_stack` / `_auto` | `1` / `2` / `0` | H1 |
| `review_toggle_sidebar` | `s` | H2 |
| `review_next_hunk` / `review_prev_hunk` | `]` / `[` | H8 |
| `review_next_file` / `review_prev_file` | `.` / `,` | H8 |
| `review_next_comment` / `review_prev_comment` | `}` / `{` | H7 |
| `review_add_comment` | `c` | H6 |
| `review_toggle_context` (`toggle_fold`) | `z` | H5 |
| `fold_all` / `unfold_all` | `z a` / `z r` | H5 |
| `review_toggle_line_numbers` | `l` | H11 |
| `review_toggle_wrap` | `w` | H11 |
| `review_toggle_hunk_meta` | `m` | H11 |
| `review_toggle_agent_notes` | `a` | H11 |
| `review_filter_files` | `/` | H13 |
| `review_help` | `?` | H10 |
| `open_menu` | `F10` | H9 |
| `review_open_file` | `Alt+o` | H17 |
| `review_jump` | `Enter` | H17 |
| **`stage` / `unstage` / `discard`** (Fresh edge) | `s`*/`u`/`d`* | — |
| **`stage_file` / `unstage_file` / `discard_file`** | `S`/`U`/`D` | — |
| `review_export_markdown` / `_json` | (menu) | — |
| `review_refresh` | `r` | — |
| `review_close` | `q` | — |

\* Note the `s` collision: hunk uses `s` for sidebar; Fresh v1 uses `s` for stage.
Resolution: in `diff-view` mode keep `s` = **stage** (the actor workflow is Fresh's
identity), move **sidebar toggle** to `\` (and the View menu). Document this as the
one intentional divergence from hunk's keymap; everything else matches.

---

## 7. State model

```ts
interface ReviewModel {
  input: { kind: 'worktree'|'staged'|'range'|'branch'|'stash'|'patch'; ref?: string };
  files: FileEntry[];            // path, status, +/-, hunks[]
  layout: 'split'|'stack'|'auto';
  group: 'dir'|'index';          // sidebar grouping
  toggles: { lineNumbers; wrap; hunkMeta; agentNotes; context; watch };
  notes: Note[];                 // {id, file, side, line, summary, rationale, author}
  focus: { file: string; hunk: number; side: 'old'|'new'; line: number };
}
```
Persisted to `.review/session.json` (v1 already writes `.review/`); export reads
the same model (Markdown to `.review/session.md`, JSON for tooling/agents).

---

## 8. Theme keys to add

Add to the theme schema (`view/theme/types.rs`) and **all built-in theme JSONs**
(reuse existing where present — see PLAN-git-log Part 2's "reuse" recommendation):

| Key | Use |
|---|---|
| `editor.diff_add_bg` / `diff_remove_bg` / `diff_modify_bg` | line wash (exists) |
| `editor.diff_add_emphasis_bg` / `diff_remove_emphasis_bg` | word-level intraline (H4) |
| `review.sidebar_status_added/modified/deleted/untracked_fg` | sidebar glyphs |
| `review.comment_border_fg` / `comment_bg` / `comment_author_fg` | inline note box (H6) |
| `review.context_fold_fg` | "N unchanged lines" row (H5) |
| `review.section_header_fg` | STAGED/UNSTAGED group headers |

No review-only *palette* — these are normal theme keys, so the review looks native
in every theme and third-party themes get sane defaults via schema fallback.

---

## 9. Agent / scriptable control surface — H16 (highest-ceiling)

`hunk`'s differentiator is its daemon + `hunk session` CLI: an agent inspects the
human's live review, navigates it, and posts inline notes as JSON. Fresh can match
**and beat** this because the human can then *edit and stage* in the same window.

Fresh already has the native scaffolding: `PluginCommand::SetReviewDiffHunks { hunks: Vec<ReviewHunk> }`
and the `ReviewHunk { id, file, context_header, status, base_range, modified_range }`
type on `Window.review_hunks`. The plan:

- Define a **review-session command vocabulary** (mirror of `hunk session`) exposed
  through Fresh's existing external control channel (the same IPC the editor already
  uses for remote/daemon control — see `server/local_control.rs`), not a new daemon:
  - `review.list` / `review.get` — sessions + file/hunk structure (JSON).
  - `review.context` — current focus.
  - `review.navigate --file F (--hunk N | --line N)` — drive the cursor.
  - `review.comment.add` / `review.comment.apply --stdin` — inject notes (the note
    model, rendered via §5.6).
  - `review.reload -- <input>` — swap inputs (§5.13) live.
- The vocabulary is a thin command layer over `ReviewModel` + the existing
  `SetReviewDiffHunks`/note APIs — no new rendering, reusing §5 surfaces.
- **Beyond hunk**: because notes carry `--rationale` and the review is *in the
  editor*, an agent can leave a note **and** a suggested edit; the human applies,
  stages the hunk, and moves on — a loop hunk structurally cannot close.

This item is explicitly **Phase 4** (largest, most optional); §5.1–§5.12 stand alone.

---

## 10. What we explicitly KEEP (Fresh's edge — do not regress)

- Hunk/line/file **stage / unstage / discard** against the git index (`s/u/d`, `S/U/D`).
- **STAGED / UNSTAGED / UNTRACKED** grouping (selectable alongside dir grouping).
- **Open + edit the real file** in place (`Enter` / `Alt+o`).
- **Comment persistence** (`.review/`) + **Markdown / JSON export**.
- **PR-branch** and **flattened-range** review modes.

## 11. Non-goals
- A review-only theme palette (we inherit editor themes by design).
- A standalone process / external TUI (Fresh's win is *integration*; the agent
  surface in §9 is in-editor IPC, not a separate binary).
- `jj` / Sapling support in v1 of this design (hunk has it; defer — Fresh is
  git-first today).
- Reimplementing syntect or the fold engine in the plugin (use the host paths).

---

## 12. New host primitives this design needs

Most surfaces use existing APIs. The deliberate host additions are:

1. **Diff background highlighting** — `HighlightCategory::{Inserted,Deleted,Changed}`
   + whole-line bg fill (PLAN-git-log Part 2; ~120 LOC). *Shared with git-log/live-diff.*
2. **`diff-view` mode Actions** for everything in §6, plus making composite hunk-nav
   rebindable (`SIDE_BY_SIDE_HUNK_NAV_REBINDABLE.md`).
3. **Layout-preserving relayout on resize** driven by the host buffer-group layout
   (fixes BUG-2 root cause instead of papering over it).
4. **`setBufferCursor` for panel buffers** (REVIEW_DIFF_REMAINING_ISSUES.md Issue 3)
   — needed for fast programmatic navigation (§5.7, §9) without the O(n) move-down
   workaround.
5. (Phase 4) the **review-session command vocabulary** over the existing local
   control channel (§9).

Everything else — sidebar (`text_list`), inline notes (`addVirtualLine` /
`addVirtualTextStyled`), folds (`setFoldingRanges`), filter (`startPrompt`), help &
theme picker (`floatingPanelControl`), watch (file-watcher events) — is **already
exposed to plugins today**.

---

## 13. Phased rollout (forward plan)

**Phase 0 — foundations (host).** Diff bg highlighting (P12.1), rebindable composite
hunk-nav, resize relayout fix, `setBufferCursor` panel fix. Ships value to git-log
and live-diff too. *Exit:* split view shows full syntax + diff wash; resize is
stable; `n`/`p` rebindable.

**Phase 1 — the reading surface.** Sidebar (`text_list`) with badges; split/stack/auto
toggle swapping composite↔virtual buffer; context folds via fold ranges; line-number/
wrap/meta toggles; `?` help overlay; contextual menu bar; theme keys + picker.
*Exit:* a reviewer can read a multi-file changeset with hunk's ergonomics.

**Phase 2 — notes & filter.** Bordered inline notes (virtual lines) + multi-line
scrollable comments panel; comment navigation; file filter; word-level intraline.
*Exit:* commenting/reading parity with hunk; v1 comment bugs (BUG-6) gone.

**Phase 3 — inputs & watch.** Stash + patch/stdin inputs; watch/auto-reload with
anchor-preserving refresh. *Exit:* input parity with hunk (minus jj/sl).

**Phase 4 — agent surface (optional, highest ceiling).** Review-session command
vocabulary over local control (§9). *Exit:* an agent can drive a live review and
leave actionable notes that a human edits + stages in place.

Each phase is independently shippable and leaves the v1 actor features intact.

---

## 14. Risks & open questions

1. **Stack-mode code token highlighting.** Region-tagging a synthetic virtual buffer
   for per-language syntect may be awkward; the overlay fallback is proven
   (`live_diff`) but 256 KB-gated. Decide early whether stack gets full token color
   or only the diff wash + split-only token color. *Recommendation:* ship diff-wash
   everywhere + token color in split first (Phase 1), add stack token color via
   overlays in Phase 2 if user feedback demands it.
2. **`s` keymap collision** (stage vs sidebar) — resolved in §6 (keep stage; sidebar
   → `\`). Validate with a quick usability pass (tmux), per the team's NN/g practice.
3. **Virtual-line note reflow on resize/layout swap** — notes must re-wrap and
   re-anchor; covered by re-emitting from the model, but worth an e2e test
   (resize + layout toggle + assert note still anchored).
4. **Watch refresh churn** on large repos — debounce + diff-the-diff; cap refresh
   rate; never block the UI thread.
5. **Auto threshold** (split↔stack) — 140 cols is a guess; make it a config key and
   tune from real terminals.

---

## 15. Success criteria

A reviewer dropped into v2 with no instructions can, using only hunk's muscle memory:
read a multi-file diff in split or stack, fold context, jump hunks/files/comments,
leave a bordered note, filter files, switch theme, and open `?` — **and** then do
what hunk can't: stage the hunks they approved and edit the one they didn't, in the
same window. Visually it is indistinguishable from "a diff viewer with hunk's
layout" while being, under the hood, ordinary Fresh panels, buffers, overlays, and
themes.
