# TUI Editor Layout & UX — Live Grep Float and Utility Dock

> **Status**: Design Document
> **Date**: April 2026
> **Branch**: `claude/design-tui-editor-layout-J5iU4`
> **Adapts**: external "TUI Editor Layout & UX Design Spec" (April 2026) to fresh's
> actual editor architecture (modes, prompt, popup, splits, panel ids, Finder lib).

## Motivation

Two layout-shaped UX problems keep coming up in plugin reviews and usability
sessions:

1. **Live Grep is a tabbed full split.** The current `live_grep.ts` plugin
   uses `Finder` in `prompt` mode plus a vertical preview split. The prompt is
   anchored to the minibuffer row; the suggestion list is rendered just above
   it via the same machinery as the command palette. There is no centred
   "search overlay" feel, no Resume affordance, and no way to dump current
   results into a parked panel ("quickfix") without keeping the palette open.
   This is tracked as
   [issue #1796 — *Live Grep: use a floating/full-window UI instead of
   splitting the focused pane*](https://github.com/sinelaw/fresh/issues/1796),
   which is the canonical bug for Section 1 of this design.

2. **Utility panels spam splits.** Every utility (diagnostics, search/replace,
   merge conflict, future terminal/tasks/output) creates its own split via
   `editor.createVirtualBufferInSplit({ panelId, direction: "horizontal" })`.
   Today the `panel_id` mapping (`app/mod.rs:786`) prevents *the same*
   utility from spawning twice, but two different utilities still produce
   two separate splits — and neither remembers the user's preferred location.

This document specifies the two features in terms of fresh's existing
primitives and identifies the smallest set of additions that make them work.

## Background: the primitives we already have

This section is a tour of the pieces this design composes — read it before
the section that builds on it. File/line citations point at the current code.

### Modes (`KeyContext`)

`crates/fresh-editor/src/input/keybindings.rs:221` defines:

```
Global, Normal, Prompt, Popup, Completion, FileExplorer, Menu, Terminal,
Settings, CompositeBuffer, Mode(String)
```

Two facts matter for this design:

- **Composite mode**: `Prompt` and `Popup` can be active simultaneously. The
  command palette already does this — typing happens in `Prompt`, while
  Up/Down navigate the suggestion list owned by the same `Prompt` (see
  `prompt_select_prev` / `prompt_select_next` in `keymaps/default.json`).
  We do *not* need a `Popup` separately to scroll a result list under a
  prompt; `Prompt::suggestions` already covers it.
- **Buffer-local modes** (`Mode("foo")`): plugins use these via
  `editor.defineMode(...)`. `diagnostics_panel.ts` uses one for its panel
  buffer; `search_replace.ts` uses `mode:search-replace-list`.

### Prompt (`view/prompt.rs:181`)

`Prompt { message, input, cursor_pos, prompt_type, suggestions,
selected_suggestion, scroll_offset, ... }` is the minibuffer/picker model.
`PromptType::QuickOpen` (`view/prompt.rs:38`) is the unified Ctrl+P prompt
backed by the provider trait at `input/quick_open/mod.rs:181`.

The renderer draws the prompt on the bottom row and a suggestion list of up
to `MAX_VISIBLE_SUGGESTIONS = 10` rows directly above it
(`view/prompt.rs:215`). It is **not** a centred overlay today.

### Popup (`view/popup.rs:219`)

`Popup` is the centred / cursor-anchored floating window primitive. Used for
LSP completion, hover, code actions, plugin action popups. `PopupPosition`
already supports `Centered`, `Fixed { x, y }`, and a few cursor-anchored
modes — exactly what a "floating overlay" needs. Confirm/cancel dispatch
goes through `PopupResolver`.

### Splits (`view/split.rs`)

Tree of `SplitNode::{Leaf, Split, Grouped}` (`view/split.rs:68`). Every
on-screen pane is a `LeafId`. `SplitManager::split_active_positioned`
(`view/split.rs:1137`) is the one entry point that creates new splits.
`SplitViewState` (`view/split.rs:272`) carries per-buffer view state and is
keyed by `LeafId` in `Editor::split_view_states`.

### Panel singletons (`app/mod.rs:786`)

```rust
panel_ids: HashMap<String, BufferId>,
```

Plugins call `editor.createVirtualBufferInSplit({ panelId, ... })`. The
dispatcher at `app/plugin_dispatch.rs:1986` does the singleton check: if the
`panel_id` already maps to an existing buffer, it updates content and
focuses the split that owns it; otherwise it creates a fresh split.

This is the seed of the "utility dock" — but the mapping is keyed per
utility, not per *role*, and the location is not persisted across utility
switches.

### Tab dragging (`app/tab_drag.rs`)

Tabs (including utility-buffer tabs) can already be dragged to other splits
via `move_tab_to_split` (`app/tab_drag.rs:245`). That means once a panel
is hosted in a window, the user can already physically relocate it; the
missing piece is *remembering* that relocation for future utility opens.

### Finder library (`plugins/lib/finder.ts`)

`Finder<T>` exposes three modes: `prompt(...)`, `panel(...)`, and
`livePanel(...)`. `live_grep.ts`, `diagnostics_panel.ts`,
`find_references.ts`, `git_grep.ts`, and the file-finder all use it.
This is the single TS-side seam for both features below.

### Free keys (audited against `keymaps/default.json`)

The external spec calls for `Ctrl+Shift+F`, `Alt+S`, `Alt+Shift+S`, `Alt+Q`,
`Alt+D`, `Alt+~`, and `Ctrl+Enter`. Audited:

| Key            | Status in default keymap                  |
|----------------|-------------------------------------------|
| `Ctrl+Shift+F` | **free** (Ctrl+F is `search`)             |
| `Alt+S`        | **free** (`Ctrl+S` is `save`; Alt+S unbound) |
| `Alt+Shift+S`  | **free**                                  |
| `Alt+Q`        | **free** (`Ctrl+Q` is `quit` — must not reuse) |
| `Alt+D`        | **free**                                  |
| `Alt+\``       | **free** (Alt+~ is the same shifted key)  |
| `Ctrl+Enter`   | **free**                                  |

`Alt+]` / `Alt+[` are already bound to `next_split` / `prev_split` in
Normal context, which we rely on rather than replace.

## Section 1 — Live Grep as a floating overlay

> Implements the user-facing requirements in
> [issue #1796](https://github.com/sinelaw/fresh/issues/1796):
>
> - render Live Grep as a floating window over the editor area, regardless
>   of current split configuration (no permanent layout mutation);
> - `Esc` dismisses the float and leaves the underlying splits untouched;
> - the user can restore / re-show the float without re-running the search,
>   so they can flip between editing a result and viewing the next match;
> - a two-column layout (input + result list on the left, file preview on
>   the right) must work *inside* the floating window — the preview is no
>   longer a permanent vertical split.

### Goal

A centred floating overlay occupying ~80% of the terminal that combines:

- a search input field at the top-left of the float,
- a scrollable results list directly under it (on the left half), and
- a file preview pane on the right half (issue #1796 requirement).

Typing edits the query; arrows move the selection in the result list
without losing focus on the input; Enter opens the selected match in the
*underlying* split (the one that was active before the overlay opened);
Esc closes the overlay, restoring the user's prior split layout exactly
(issue #1796: "return to their previous split layout untouched"); the
search state can be resumed; results can be exported into the Utility
Dock as a parked Quickfix list.

### How this maps to fresh's primitives

The natural mapping is **Prompt + Popup composite**, with the prompt
hidden from the bottom minibuffer row and instead drawn inside the popup
frame. We do *not* need a new mode: the existing composite that the
command palette uses already proves Prompt+Popup can coexist.

```
┌──────────────── Live Grep ──────────────────────[?]─[Resume]─[Quickfix]─[×]┐
│ Search: split_active|                12/142 │  src/view/split.rs            │
│ ─────────────────────────────────────────── │ ──────────────────────────── │
│  src/view/split.rs:1117  pub fn split_a…    │ 1115 ┊                       │
│  src/view/split.rs:1123  self.split_act…    │ 1116 ┊  /// Split the active │
│  src/view/split.rs:1128  pub fn split_a…    │ 1117 ► pub fn split_active(  │
│  src/view/split.rs:1137  pub fn split_a…    │ 1118 ┊      &mut self,       │
│  src/view/split.rs:1623  let result = m…    │ 1119 ┊      direction: …,    │
│ …                                           │ 1120 ┊      buffer_id: …,    │
│                                             │ 1121 ┊      ratio: f32,      │
│       Prompt + suggestion list              │       File preview pane      │
│       (left half — input + Popup list)      │       (right half)           │
└────────────────────────────────────────────────────────────────────────────┘
```

### Concrete components

1. **A new `PopupPosition::CenteredOverlay { width_pct, height_pct }`**
   variant in `view/popup.rs`. The existing `Centered` variant clamps to a
   fixed inner content size; the overlay needs to *reserve* a percentage of
   the frame regardless of result count, so neither the input nor the
   preview visually jumps when results stream in.

2. **`PromptType::LiveGrep`** added to `view/prompt.rs:11`. We do not reuse
   `Plugin { custom_type }` because Live Grep needs first-class layout
   handling (it is not minibuffer-rendered) and its key bindings differ
   (Ctrl+Enter exports to dock, Up/Down navigate the *attached popup* list
   rather than the prompt's own suggestion list).

3. **A binding in the renderer** (the place that today decides "if there is
   a `Prompt`, draw it on the bottom row") that, if `prompt.prompt_type ==
   LiveGrep`, draws the overlay frame *over* the editor area without
   touching the split tree, and inside that frame:
   - left column: prompt input (top row of inner area) + suggestion list;
   - right column: a read-only preview of the currently selected match's
     file, scrolled to the match line.

   The popup itself takes `PopupPosition::CenteredOverlay { 80, 80 }`.
   Crucially, the editor's `SplitManager` is **not modified** while the
   overlay is up — this is what guarantees issue #1796's "return to the
   previous split layout untouched" requirement: there is no layout to
   restore because no layout was changed.

4. **Plugin side**: extend `Finder<T>` with a `floatingOverlay: true`
   option for the `prompt` mode, plus `previewInOverlay: true` so the
   preview is rendered inside the float instead of into a sibling split.
   `live_grep.ts` opts in to both. The plugin keeps using the same
   `prompt_*` event handlers it already uses; it just gets a different
   visual shell. The current behaviour — `createVirtualBufferInSplit`
   with `panelId: "live-grep-preview"` and `direction: "vertical"` — is
   removed for Live Grep; the preview lives inside the float.

### Keybinding integration (Live Grep)

| Action                       | Binding                | Context     | Notes |
|------------------------------|------------------------|-------------|-------|
| Open Live Grep               | `Ctrl+Shift+F`         | Normal      | Primary. `Ctrl+F` stays buffer-local search. |
| Open Live Grep (alt)         | `Alt+S`                | Normal      | Mnemonic "Search". Mirrors the spec; secondary so muscle memory survives terminals that mangle Shift+modifier combos. |
| Resume last grep             | `Alt+Shift+S`          | Normal      | Reopens the overlay with the prior query and selected index. |
| Move selection ↓ / ↑         | `Down` / `Up`          | Prompt (`prompt_type == LiveGrep`) | Reuses `prompt_select_next` / `prompt_select_prev`. |
| Confirm — open in active split | `Enter`              | Prompt      | Reuses `prompt_confirm`. |
| Cancel                       | `Esc`                  | Prompt      | Reuses `prompt_cancel`. |
| **Export to Quickfix dock**  | `Alt+Q` *or* `Ctrl+Enter` | Prompt (`prompt_type == LiveGrep`) | New action `live_grep_export_quickfix`. **Must not reuse `Ctrl+Q`** — that is `quit` in Normal. |

### State preservation ("Return to Work")

Issue #1796 calls out *"restore / re-show the floating Live Grep without
re-running the search (so they can flip between editing a result and
viewing the next match)."* That requirement is what shapes this struct:

```rust
pub struct LiveGrepLastState {
    pub query: String,
    pub selected_index: Option<usize>,
    /// Cached results from the last invocation. When present, Resume
    /// re-displays them immediately and does NOT re-run the search;
    /// the user gets a fresh search only by editing the query.
    pub cached_results: Option<Vec<GrepMatch>>,
    pub cached_at: Option<std::time::Instant>,
    /// Optional Quickfix snapshot id (Section 1 → Section 2 hand-off).
    pub last_results_snapshot_id: Option<u64>,
}
```

`Resume` (`Alt+Shift+S`):

1. Reopens the overlay with `query` already typed and the prior
   `selected_index` highlighted.
2. **Does not re-issue the ripgrep call** if `cached_results` is `Some`.
   This is the issue-#1796 flip-between-edit-and-next-match flow.
3. Editing the query (any keystroke that mutates `prompt.input`) marks
   `cached_results = None` and resumes normal debounced live search.

If the user prefers always-fresh resumes, a config bool
`live_grep.resume_reruns_search` can flip default behaviour.

### Quickfix export flow

`Alt+Q` / `Ctrl+Enter` while in the Live Grep overlay does:

1. Snapshot current filtered results into a `Vec<GrepMatch>`.
2. Close the overlay (Prompt cancel).
3. Push the snapshot into the Utility Dock as a virtual buffer with
   `panel_id = "quickfix"` (Section 2 explains why this is one panel id,
   not per-feature).
4. Focus the dock so the user can scroll/Enter into matches with arrow
   keys (the Finder's existing panel-mode bindings already do this).

The dock buffer's `live_grep` mode adds a one-shot binding `Alt+Shift+S`
that re-opens the overlay seeded from the snapshot, completing the
Resume↔Quickfix round trip.

## Section 2 — Utility Dock (tagged singleton)

### Goal

Diagnostics, search/replace results, terminal, build/test output, the
Quickfix list from Section 1, and any future "panel-like" plugin all share
one persistent, user-relocatable docking window. Opening any utility
*either* spawns the dock once (default: a horizontal split at the bottom
of the screen) *or* swaps the existing dock window's active buffer to that
utility — never adds a new split.

### How this maps to fresh's primitives

The seed is already in `app/mod.rs:786` — the `panel_ids: HashMap<String,
BufferId>` mapping that `app/plugin_dispatch.rs:1986` consults before
splitting. Today each utility owns its own `panel_id` (`"diagnostics"`,
`"search-replace-panel"`, `"merge-theirs"`, …), so two different utilities
still spawn two splits.

The fix is: **promote the singleton from per-utility to per-role.** A new
`utility_dock` role attaches to a window in the split tree; any utility
opened with `role: "utility_dock"` lands in *that* window regardless of
which `panel_id` it carries.

### Concrete components

1. **`SplitRole` tag on `SplitNode::Leaf`.** Today the leaf only carries
   `buffer_id` and `split_id` (`view/split.rs:68`). Add:

   ```rust
   SplitNode::Leaf {
       buffer_id: BufferId,
       split_id: LeafId,
       #[serde(default)]
       role: Option<SplitRole>,   // None for ordinary editor leaves
   }

   pub enum SplitRole {
       UtilityDock,
       // future: BottomBar, Sidebar, etc.
   }
   ```

   Persisted across workspace serialization, the same way the existing
   `fixed_first` / `fixed_second` fields are persisted (default `None`).

2. **`Editor::utility_dock_leaf: Option<LeafId>`.** Cached pointer to the
   leaf currently tagged `UtilityDock`. Recomputed on workspace load by
   walking the tree once. Always reconciled with the split tree on close
   (so a manually closed dock leaf clears the cache).

3. **Dispatcher change.** In `handle_create_virtual_buffer_in_split`
   (`app/plugin_dispatch.rs:1969`), when the request includes
   `role: "utility_dock"`:

   - If `utility_dock_leaf` is `Some(leaf)` *and* the leaf still exists,
     **do not split**. Swap the active buffer of that leaf to the new
     virtual buffer (`SplitViewState::switch_buffer` already handles per-
     buffer view state). Add the buffer as a tab in that leaf so users
     can flip between, e.g., Diagnostics and Quickfix without losing
     either.
   - If `utility_dock_leaf` is `None`, create a horizontal split at the
     bottom (the current code path), tag the new leaf with
     `role = Some(UtilityDock)`, set `utility_dock_leaf = Some(leaf)`.

4. **Plugin API surface.** Extend `editor.createVirtualBufferInSplit`
   options:

   ```ts
   createVirtualBufferInSplit({
     panelId: "diagnostics",
     role: "utility_dock",   // NEW — default unset for back-compat
     direction: "horizontal",
     ratio: 0.3,
     // ...
   })
   ```

   `role` is advisory: when set and the dock exists, `direction` and
   `ratio` are ignored (the dock's geometry wins). When the dock doesn't
   exist yet, they seed its initial geometry.

### Existing utilities to migrate

| Plugin                  | Today                                | After |
|-------------------------|--------------------------------------|-------|
| `diagnostics_panel.ts`  | own bottom split (`panel_id: "diagnostics"`) | dock with `role: "utility_dock"` |
| `search_replace.ts`     | own bottom split (`panel_id: "search-replace-panel"`) | dock |
| `find_references.ts`    | own panel via Finder | dock |
| `git_grep.ts`           | own preview split | overlay (Section 1) + dock for Quickfix |
| **Quickfix** (new)      | n/a | dock with `panel_id: "quickfix"` |
| **Built-in terminal**   | own split via `open_terminal()` | dock (opt-in via config; preserves the existing user mental model for power users who deliberately open terminals in custom splits) |

`merge_conflict.ts` keeps its own splits — its `panel_id`s
(`merge-theirs`, `merge-result`) are part of a 3-way diff layout, not a
docking utility. We do **not** force every multi-buffer plugin into the
dock.

### Keybinding integration (Utility Dock)

The dock is just a leaf in the split tree, so existing window navigation
already works:

- `Alt+]` / `Alt+[` (`next_split` / `prev_split` in Normal context) cycle
  through it like any other split. No new binding needed.

New bindings for direct focus and toggling:

| Action                    | Binding   | Context | Notes |
|---------------------------|-----------|---------|-------|
| Focus / toggle dock       | `Alt+D`   | Normal  | Mnemonic "Dock". If the dock exists, focus it; pressing again from inside the dock returns focus to the previous editor split. |
| Direct: terminal in dock  | `Alt+\``  | Normal  | Opens a terminal *in the dock* (swap or create). Avoids `Ctrl+\`` collisions on terminals that capture it. |
| Direct: diagnostics       | (palette) | Normal  | The existing palette command continues to work; no dedicated key, since power users already palette-driven. |
| Hide dock                 | `Alt+D`   | Normal  | When focus is already in the dock, `Alt+D` hides it (closes the leaf, keeps `panel_id` mappings — re-opening any utility re-creates the dock leaf with the *same geometry seed*). |

### "Respect the tag" — physical relocation

Because `SplitRole::UtilityDock` lives on the leaf, the user can already
move the dock anywhere via the existing `move_tab_to_split` machinery
(`app/tab_drag.rs:245`):

1. User drags the dock's tab into a side split.
2. The drop creates a new leaf with the dropped buffer; the new leaf
   inherits the `role` of the source leaf if and only if the source leaf
   is being absorbed (i.e., the source leaf's last tab is moved). This
   keeps the rule simple: "the role follows the *window*, not the buffer."
3. `utility_dock_leaf` is updated to point at the new location.
4. Future utility invocations route to the new location, regardless of
   coordinates. The Quickfix export (Section 1) lands here too.

If the user's drop *splits* the source leaf rather than absorbing it
(e.g., dragging only one of two utility tabs out), the original leaf
keeps the role and the dragged tab opens in a brand-new untagged leaf,
i.e., it stops being part of the dock. This is the principled answer to
"what if the user wants two docks?" — they don't, by construction; if
they need two utility windows, they untag one explicitly via a
palette command (`Untag Utility Dock`).

### Anti-spam invariant

The invariant we maintain through every code path:

> **At most one leaf in the split tree has `role == UtilityDock`.**

Enforcement points:

- Dispatcher path (Section 2 step 3) routes by tag, never creates a
  second tagged leaf.
- Tab-drag path: when a role-bearing leaf is *absorbed* by a drop, the
  role transfers to the destination leaf in the same atomic mutation.
- Workspace deserialization: if two leaves come back tagged
  `UtilityDock` (e.g., a hand-edited workspace file), the editor logs a
  warning and clears the role from the second one on first paint.

## Section 3 — Keymap diff (`keymaps/default.json`)

New `Action` variants, registered in `input/keybindings.rs:305`:

```rust
Action::OpenLiveGrep,             // string id: "open_live_grep"
Action::ResumeLiveGrep,           //            "resume_live_grep"
Action::ExportLiveGrepQuickfix,   //            "live_grep_export_quickfix"
Action::FocusUtilityDock,         //            "focus_utility_dock"
Action::ToggleUtilityDock,        //            "toggle_utility_dock"
Action::OpenTerminalInDock,       //            "open_terminal_in_dock"
```

Default keymap additions (concrete JSON shape mirrors existing entries
in `keymaps/default.json`):

```jsonc
// Live Grep — open / resume
{ "key": "f", "modifiers": ["ctrl", "shift"], "action": "open_live_grep",     "when": "normal" },
{ "key": "s", "modifiers": ["alt"],            "action": "open_live_grep",     "when": "normal" },
{ "key": "s", "modifiers": ["alt", "shift"],   "action": "resume_live_grep",   "when": "normal" },

// Live Grep — Quickfix export (only inside the overlay's prompt)
{ "key": "q",     "modifiers": ["alt"],  "action": "live_grep_export_quickfix", "when": "prompt" },
{ "key": "Enter", "modifiers": ["ctrl"], "action": "live_grep_export_quickfix", "when": "prompt" },

// Utility Dock
{ "key": "d", "modifiers": ["alt"], "action": "toggle_utility_dock",   "when": "normal" },
{ "key": "`", "modifiers": ["alt"], "action": "open_terminal_in_dock", "when": "normal" }
```

The `live_grep_export_quickfix` action has a `prompt` `when` clause —
it must only fire when the active prompt is the Live Grep overlay. The
handler short-circuits when `prompt.prompt_type != PromptType::LiveGrep`
to avoid stealing `Alt+Q` from the (already nonexistent, but defensive)
hypothetical other prompts.

### Conflict audit

| Proposed key      | Pre-existing binding                       | Conflict? |
|-------------------|--------------------------------------------|-----------|
| `Ctrl+Shift+F`    | none (`Ctrl+F` is `search`)                | no        |
| `Alt+S`           | `Alt+S` is `menu_open` in **Global**       | **yes** — collision with menu mnemonic |
| `Alt+Shift+S`     | none                                       | no        |
| `Alt+Q`           | none (`Ctrl+Q` is `quit`)                  | no        |
| `Alt+D`           | none                                       | no        |
| `Alt+\``          | none                                       | no        |
| `Ctrl+Enter`      | none                                       | no        |

The `Alt+S` conflict matters: in Global context, `Alt+S` opens the menu
bar's "Search" menu (line 65 of `keymaps/default.json`). Two viable
resolutions:

- **A.** Drop `Alt+S` as a Live Grep alias; keep only `Ctrl+Shift+F` and
  `Alt+Shift+S` (Resume). The menu mnemonic stays.
- **B.** Move Live Grep into the Search menu so `Alt+S → L` reaches it
  through the menu, and add `Ctrl+Shift+F` as the direct binding.

This design recommends **B**, since the menu already has a Search entry
and adding "Live Grep…" under it is the most discoverable place for new
users. `Ctrl+Shift+F` covers the keyboard-driven path.

## Section 4 — Implementation phases

Each phase is independently shippable; no phase merges before the
preceding ones land.

### Phase 1 — Centred overlay primitive (no Live Grep wiring yet)

- Add `PopupPosition::CenteredOverlay { width_pct, height_pct }` and
  layout logic in `view/popup.rs` + the popup renderer.
- Add a temporary palette command `Debug: Show Centered Overlay` that
  pops a stub overlay with placeholder text; verify resizing and
  Esc-dismiss in tmux at 80×24, 200×50, and 300×80.

**Acceptance**: Esc dismisses without touching the split tree
(`SplitManager` snapshot before/after is byte-equal).

### Phase 2 — `PromptType::LiveGrep` + composite render

- Add `LiveGrep` variant to `PromptType` (`view/prompt.rs:11`).
- Teach the renderer to draw the prompt and the existing
  `prompt.suggestions` *inside* the overlay frame instead of the
  bottom row when `prompt_type == LiveGrep`.
- Wire `prompt_select_next` / `prompt_select_prev` so they navigate the
  list as drawn (already true — just verify no fall-throughs to
  popup-context bindings).

**Acceptance**: existing Live Grep ripgrep pipeline (untouched) renders
inside the overlay; Esc closes; selection and Enter still open files in
the underlying split.

### Phase 3 — In-overlay preview

- Add right-half preview pane to the overlay.
- Plumb selection-change events to the preview renderer (it tails the
  selected match's file from disk via the existing FS abstraction —
  same code path the current side-split preview uses).
- Remove the `panelId: "live-grep-preview"` split from `live_grep.ts`.

**Acceptance**: this phase closes issue #1796: the float covers the
editor area, has the two-column layout from the issue, and dismissing
returns the user to an unmodified split layout.

### Phase 4 — Resume + LiveGrepLastState

- Add the `LiveGrepLastState` cache and the Resume binding
  (`Alt+Shift+S`).
- Verify the issue-#1796 flow: open Live Grep, pick a match, Enter,
  edit a few characters, `Alt+Shift+S`, see prior results without a
  re-search, Down arrow to next match, Enter again.

### Phase 5 — Utility Dock primitive

- Add `SplitRole` and the `Editor::utility_dock_leaf` cache.
- Migrate dispatcher (Section 2 step 3) to honour the role.
- Migrate `diagnostics_panel.ts` and `search_replace.ts` to pass
  `role: "utility_dock"`. Verify only one bottom split exists when
  flipping between them.

### Phase 6 — Quickfix export

- Add `Action::ExportLiveGrepQuickfix` and the `Alt+Q` /
  `Ctrl+Enter` overlay bindings.
- Implement the snapshot → virtual buffer (`panel_id: "quickfix"`,
  `role: "utility_dock"`) flow.
- Add the dock-side `Alt+Shift+S` binding that re-seeds the overlay
  from the snapshot (closes the Resume↔Quickfix loop).

### Phase 7 — Tab-drag tag transfer + dock toggle

- Update `move_tab_to_split` to transfer `SplitRole` on absorption only.
- Implement `Action::ToggleUtilityDock` (`Alt+D`) with the focus-toggle
  semantics from Section 2.
- Workspace round-trip test: kill editor with dock in a non-default
  position, reopen, assert dock survived in same place.

## Section 5 — Open questions

1. **Mouse interaction with the overlay.** Should clicking outside the
   overlay frame dismiss it (Mac-style modal sheet) or be ignored
   (terminal-classic — treat the overlay as opaque)? Recommendation:
   ignore, with a single click on the title bar's `[×]` to close.
   Keeps muscle memory consistent with the existing menu/popup behaviour.

2. **Preview pane size when terminal is narrow.** Below ~120 columns the
   left/right split inside the overlay becomes cramped. Either:
   (a) auto-stack the preview *under* the result list at narrow widths,
   or (b) hide the preview and require Tab to flip into it. Issue #1796
   doesn't take a side. Recommendation: (a), with a config
   `live_grep.preview_min_width_cols = 120`.

3. **Should Quickfix be one global list or one per source feature?**
   This design proposes one global list keyed by `panel_id =
   "quickfix"`. Alternative: per-feature (`quickfix.live_grep`,
   `quickfix.lsp_references`, …) tabbed in the dock. We can defer this
   — start with the single list and split later if users complain.

4. **Terminal in dock by default?** The current `open_terminal()` in
   `app/terminal.rs` creates its own split at the active position. Many
   users like that. Section 2 routes terminals to the dock *only* when
   invoked through the new `Alt+\`` action; the existing `Open
   Terminal` menu entry stays put. Worth revisiting after a usability
   pass.

5. **Workspace-format compatibility.** The `role` field on
   `SplitNode::Leaf` is added with `#[serde(default)]`, so old workspace
   files load fine. New workspaces saved with a tagged dock won't load
   on older builds (the field is silently ignored, the dock just looks
   like an ordinary leaf). Acceptable.

## Section 6 — Non-goals

- **Generalised floating-window plugin API.** Issue #1796 hints at one
  ("a floating-window primitive would also be useful for other transient
  UIs"), but this design exposes the primitive only inside the renderer
  via `PopupPosition::CenteredOverlay`. Plugin authors get the
  capability through `Finder<T>`'s `floatingOverlay` flag, not raw
  access. Generalising further is a separate design.
- **Multiple concurrent docks.** One dock leaf, by invariant. Users who
  want a sidebar *and* a bottom panel can use the existing split system
  + plugin-managed `panel_id`s without `role`.
- **Replacing the prompt minibuffer.** The bottom-row prompt remains the
  default for everything that isn't Live Grep. Quick Open, file open,
  search/replace, etc. are unchanged.
- **Touchpad/mouse drag-resize for the overlay.** Centred 80% is fixed
  in this iteration; resize is left to the underlying terminal window.

## Appendix — Files touched

```
crates/fresh-editor/src/view/popup.rs            (CenteredOverlay variant)
crates/fresh-editor/src/view/prompt.rs           (PromptType::LiveGrep)
crates/fresh-editor/src/view/split.rs            (SplitRole on Leaf)
crates/fresh-editor/src/input/keybindings.rs     (new Action variants)
crates/fresh-editor/src/app/mod.rs               (LiveGrepLastState,
                                                  utility_dock_leaf cache)
crates/fresh-editor/src/app/plugin_dispatch.rs   (role-aware dispatcher)
crates/fresh-editor/src/app/tab_drag.rs          (role transfer on absorb)
crates/fresh-editor/src/app/split_actions.rs     (toggle_utility_dock)
crates/fresh-editor/keymaps/default.json         (new bindings)
crates/fresh-editor/plugins/lib/finder.ts        (floatingOverlay,
                                                  previewInOverlay,
                                                  role)
crates/fresh-editor/plugins/live_grep.ts         (opt-in, drop preview split)
crates/fresh-editor/plugins/diagnostics_panel.ts (role: utility_dock)
crates/fresh-editor/plugins/search_replace.ts    (role: utility_dock)
```

## References

- Issue [#1796](https://github.com/sinelaw/fresh/issues/1796) — Live
  Grep floating window (canonical bug for Section 1).
- `docs/internal/finder-abstraction.md` — the TS-side seam.
- `docs/internal/grouped-splitnode-design.md` — prior art for adding a
  variant/field to `SplitNode`.
- `docs/internal/project-search-replace.md` — the search backend Live
  Grep continues to rely on.
- `eval-diagnostics-panel.md` — usability bugs in the existing
  diagnostics panel that the dock migration should regression-test
  against.
