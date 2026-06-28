# Input, Keybindings, and the Action/Event Model

Purpose: explain how Fresh turns a raw terminal keystroke (or mouse event) into a
buffer change — the key-translation layer, the modal dispatch priority, unified
keybinding resolution, the command → action → event pipeline, multi-cursor, and
fuzzy/quick-open input. Code references are `path:line`; everything below is
IMPLEMENTED unless explicitly flagged PLANNED.

All paths are relative to `crates/fresh-editor/src/` unless noted.

---

## 1. The pipeline at a glance

A keystroke flows through these stages:

```
Terminal (crossterm KeyEvent)
  → KeyTranslator.translate()         (input/key_translator.rs) — calibration fixups
  → Editor::handle_key()              (app/input.rs:446)        — modal priority + chord state
      → KeybindingResolver.resolve()  (input/keybindings.rs:1971) — key → Action
  → Editor::handle_action(Action)     (app/input.rs:990)        — giant match; side-effects or…
      → action_to_events(Action)      (input/actions.rs:…)      — Action → Vec<Event>
  → log_and_apply_event / bulk edit   (app/event_apply.rs:31)   — Events mutate buffer + undo log
```

Three distinct vocabularies live here and the separation is deliberate (§4):

- **Command** — a user-facing, localized, context-filtered palette entry
  (`input/commands.rs`). Wraps exactly one `Action`.
- **Action** — what a keypress *means* in editor terms (`MoveUp`, `Save`,
  `InsertChar('a')`). The `Action` enum is the rebinding currency and the
  serialization unit for keymaps/macros (`input/keybindings.rs:364`).
- **Event** — what actually happens to the buffer/cursors (`Event::Insert`,
  `Event::Delete`, `Event::MoveCursor`, `Event::BulkEdit`). Events are the undo
  unit and the plugin-hook unit (`app/event_apply.rs`).

---

## 2. Key translation across terminals/platforms

Terminals are inconsistent about what bytes they emit for a given physical key.
Fresh handles this in three layers, applied in order, *before* keybinding
resolution.

### 2.1 KeyTranslator (per-user calibration)

`input/key_translator.rs` is a `HashMap<KeyEventKey, KeyEventKey>` that rewrites a
raw `KeyEvent` to a normalized one (`translate()`, line 213). It is populated by
the input-calibration wizard (`Action::CalibrateInput`) and persisted to
`key_calibration.json` (line 322). The doc comment fixes the canonical order
(line 6): "Terminal → KeyTranslator → KeybindingResolver". Rationale (commit
`1b832afb8`): keep "fix the broken terminal" separate from "rebind the action" —
calibration must not pollute the keymap. Unmapped keys pass through untouched.

### 2.2 `normalize_key` (resolution-time canonicalization)

`input/keybindings.rs:25` folds redundant modifier noise so a single binding
matches every terminal's encoding of the same key:

- `BackTab`/`Backspace` + SHIFT → drop the redundant SHIFT (lines 26–31).
- Uppercase `Char('P')` → lowercase + inferred SHIFT, *unless* CONTROL is set
  (lines 32–41). This is the crux of "the SHIFT-inference saga" (commits
  `5217c41ab`, `77360e6c6`, #1899): kitty-protocol terminals send a real SHIFT
  with an uppercase char, non-kitty terminals send a bare uppercase char with no
  modifier, and CapsLock+Ctrl sends uppercase + CONTROL. Inferring SHIFT for the
  no-CONTROL case makes `Shift+P` / `Alt+Shift+F` bindings match uniformly, while
  the CONTROL exception preserves the long-standing "CapsLock+Ctrl+A still fires
  Ctrl+A" intent (doc comment, lines 18–24).

`normalize_key` is applied to both the live key and every chord-state key in
`resolve` (line 1974) and `resolve_chord` (line 1911).

### 2.3 `terminal_key_equivalents` (alias expansion)

`input/keybindings.rs:179` maps control-key combos that terminals encode
differently into aliases that are *inserted alongside* the primary binding at
load time (`insert_binding_with_equivalents`, line 1688):

- `Ctrl+/` ↔ `Ctrl+7`, `Ctrl+Backspace` ↔ `Ctrl+H`, `Ctrl+Space` ↔ `Ctrl+@`
  (NUL), `Ctrl+-` ↔ `Ctrl+_` (commit `f817c9817`).

Explicit bindings always win over auto-generated equivalents; a conflict only
logs a warning (lines 1705–1721). `keybinding_priority_score` (line 157)
deprioritizes the terminal-equivalent half when picking a canonical display key.

`is_text_input_modifier` (line 73) additionally treats Windows AltGr
(reported as Ctrl+Alt by crossterm) as text-input-capable so international layouts
can type `@ [ ] { }` (#993).

PLANNED/orthogonal: the kitty keyboard protocol enhancement flags
(`DISAMBIGUATE_ESCAPE_CODES`, `REPORT_ALTERNATE_KEYS`) are enabled at the
terminal layer (commits `1a32acc7c`, `cc16acd76`) so capable terminals can report
`Shift+Up`, etc.; unsupported terminals degrade rather than getting a custom
fallback.

---

## 3. Modal dispatch priority

`Editor::handle_key` (`app/input.rs:446`) is the central router. It establishes a
strict precedence so that overlays consume input before the buffer does. The
priority is computed from an **overlay layer stack**, not a hand-listed ladder —
`get_key_context()` (line 439) and `dispatch_modal_input` both consult
`overlay_layers()` so the keyboard, mouse, and terminal paths cannot drift out of
sync (this is the modern replacement for the older hardcoded order).

Order of checks in `handle_key`:

1. **Event-debug dialog** intercepts *all* keys first (line 485).
2. **Terminal mode** — `dispatch_terminal_input` (`app/input_dispatch.rs:22`).
   Returns `None` (falls through) if any *blocking* overlay is up
   (`presents_blocking_overlay`, line 431). Plugin commands flagged
   `terminalBypass: true` are resolved against the Normal context and dispatched
   *before* the PTY claims the key (lines 75–94) — that is how `Orchestrator:
   Open` stays reachable from inside `htop`.
3. **`getNextKey()` plugin callback** — a plugin awaiting the next key (vi
   find-char, flash labels) gets it before any other dispatch (line 505).
4. **Floating widget panel** (focused) and **focused dock** swallow keys; the
   dock-focus toggle is resolved early so it's symmetric in/out (lines 517–548).
5. **Transient popups** (hover, signature help) are dismissed on any key except
   the copy/focus-popup keys (lines 607–651); **unfocused popup** cancel/focus
   keys are honored next (line 658).
6. **`dispatch_modal_input`** (`app/input_dispatch.rs:180`) — the heart of the
   modal ladder:
   - `dispatch_modal_keyboard` walks the overlay stack top-down for the four
     *capture-all* modals — **Settings → KeybindingEditor → CalibrationWizard →
     Menu** (lines 130–174). These early-return.
   - **Prompt** (line 194): Alt+char prompt bindings resolved
     context-only first; then file-browser / query-replace / overlay-toolbar
     handlers; finally the prompt's own `dispatch_input`. A prompt that returns
     `Ignored` falls through to global keybindings (line 264).
   - **Popup** (line 276), gated by `popups_capture_keys()` so editor-pane popups
     don't eat keys when the file explorer is focused. Completion popups consult
     the `Completion` keybinding context first (line 281); global popups outrank
     buffer popups (line 307). `Ignored` falls through.
7. **Buffer-mode bindings** (line 690) — only in `Normal`/`CompositeBuffer`
   context. Chord then single-key resolution against the `Mode(name)` context.
8. **Composite-buffer routing** (line 863) via `try_route_composite_key`.
9. **Chord + single-key resolution** in the resolved context (lines 874–905), then
   `handle_action(action)` (line 930).

The net priority is **Settings → Menu → Prompt → Popup → (mode) → Normal**, with
terminal mode and floating overlays gating ahead of all of them.

### 3.1 The hierarchical InputHandler trait + DeferredAction

Modal components (Settings, Menu, Prompt, Popup, file browser) implement
`InputHandler` (`input/handler.rs:208`). The contract (module doc, lines 1–17):

- **Leaf-first, bubble up**: `dispatch_input` (line 229) tries the focused child
  first, then the handler itself.
- **Explicit consumption**: handlers return `InputResult::Consumed` (stop) or
  `Ignored` (try parent).
- **Modals consume by default** *unless they explicitly return `Ignored`* (lines
  244–254) — this opt-out is what lets, e.g., Ctrl+P toggle Quick Open closed
  while the prompt is open. No DOM-style capture phase, by choice (line 15).

Handlers cannot mutate `Editor` directly (they only borrow their own sub-state),
so they enqueue **`DeferredAction`** values into `InputContext.deferred_actions`
(`input/handler.rs:99`). After dispatch, `process_deferred_actions`
(`app/input_dispatch.rs:339`) drains the queue with full `&mut Editor` access and
`execute_deferred_action` (line 356) performs the real side-effect
(`CloseSettings`, `ExecuteMenuAction`, `SendTerminalKey`, `ExecuteAction`, …).
This is the borrow-checker-friendly bridge between self-contained handlers and
editor-global mutation. `DeferredAction::ExecuteAction(Action)` (line 195) lets a
handler fall back into the normal action pipeline.

---

## 4. Actions vs Events: the command → action → event pipeline

### 4.1 Why three layers

- **Rebindability & serialization.** `Action` is `Serialize/Deserialize`
  (`input/keybindings.rs:364`) and round-trips through string names via the
  `define_action_str_mapping!` macro (line 869), which generates `from_str`,
  `to_action_str`, and `all_action_names` from one definition so the keymap
  parser, keybinding editor, and macro recorder can never drift. Keybindings and
  macros store Actions, not Events — Events are position-specific and would not
  replay.
- **Undo/redo & plugin hooks.** Events are the transaction record. Every buffer
  mutation flows through `log_and_apply_event` (`app/event_apply.rs:31`), which
  appends to the `EventLog` (undo chain) and then applies. `Action`s that aren't
  edits (e.g. `Quit`, `OpenSettings`) produce no events at all
  (`action_to_events` returns `None`).
- **Layout independence.** `Action::MoveUp` is purely semantic; the conversion to
  a concrete `Event::MoveCursor` resolves soft-wrap visual lines via the cached
  layout (`app/action_events.rs`, `handle_visual_line_movement`). One action, many
  possible byte targets.

### 4.2 The two-step conversion

`handle_action` (`app/input.rs:990`) is a ~700-arm match. Two kinds of arms:

1. **Direct side-effects** — `Save` opens a prompt or writes the file
   (line 1028); `AddCursorNextMatch` (line 1714) calls into `multi_cursor.rs`.
2. **Catch-all edit/movement** — delegated to `apply_action_as_events`
   (`app/input_helpers.rs:279`), which calls `Window::action_to_events`
   (`app/action_events.rs:17`). That bridge tries visual-line movement and page
   motion first, then falls back to `input/actions.rs::action_to_events` for the
   logical conversion (`app/action_events.rs:68`).

`input/actions.rs` is the pure `(state, cursors, events, …)` converter (decomposed
from a 2308-line god-match into per-action `handle_*` helpers, commits
`477894390`/`1c6bd8ce9`). It is where editor semantics live: auto-close/auto-pair
(`insert_char_events`, line 919), smart-backspace dedent
(`handle_delete_backward`, line 1738), tiered auto-indent (tree-sitter grammar →
per-language regex rules → C-style bracket scanner, lines 1179–1207), CRLF-as-one
handling (`next_position_for_crlf`, line 199), and visual-column sticky movement
(`handle_vertical_up/down`, lines 1497/1551).

### 4.3 Applying events

`app/event_apply.rs` has three entry points (module doc, lines 1–17):

- **`log_and_apply_event`** (line 31) — canonical single-event path.
- **`apply_event_to_active_buffer`** (line 46) — apply without logging (replay).
  Critically, LSP change positions and plugin-hook line info are computed *before*
  the buffer mutates (lines 78–84) so byte offsets are pre-modification.
- **`apply_events_as_bulk_edit`** (line 246) — batches multi-event edits under one
  undo boundary, sorting by descending position so earlier edits don't shift later
  ones, producing one `Event::BulkEdit` with old/new snapshots. Used by
  replace-all, format-on-save, LSP rename, and **multi-cursor typing** — this is
  the O(n) (not O(n²)) path.

The caller in `apply_action_as_events` (`app/input_helpers.rs:312`) picks the
path: >1 event with buffer mods → bulk edit; >1 event without mods → `Event::Batch`
(atomic undo for multi-cursor moves); single event → `log_and_apply_event`.

### 4.4 Commands

`input/commands.rs` defines ~340 static `CommandDef`s (`COMMAND_DEFS`, line 175),
each i18n-keyed with an `action` and a context list. A `Command` (line 16) is the
localized, context-aware wrapper the palette shows; its `.action` field is the
`Action` actually executed. `command_registry.rs` (`CommandRegistry`, line 12)
merges builtin commands with plugin-registered ones (`Arc<RwLock<Vec<Command>>>`),
supports first-writer-wins plugin registration (`try_register`, line 120), tracks
usage history for recency sorting, and exposes `filter()` (line 206) — the palette
entry point that fuzzy-matches + context-filters + sorts. Selecting a command
ultimately runs its `Action` through `handle_action`.

---

## 5. Unified keybinding resolution

### 5.1 Goal and structure

The design goal (`docs/internal/unified-keybinding-resolution.md`): a **single
resolution path** for builtin, keymap, user-custom, and plugin-mode bindings — no
separate `ModeRegistry` lookup, no mode inheritance. This was implemented (commit
`a3c7899de`); `ModeRegistry` (`input/buffer_mode.rs:73`) is now metadata-only
(`read_only`, `allow_text_input`, `inherit_normal_bindings`, `plugin_name`).

`KeybindingResolver` (`input/keybindings.rs:1561`) holds six maps plus a set:

| Field | Tier | Source |
|---|---|---|
| `bindings` | custom | user `keybindings` in config |
| `default_bindings` | keymap | active keymap (`default`/`emacs`/`vscode`…) |
| `plugin_defaults` | plugin | `defineMode()` runtime registration |
| `chord_bindings` / `default_chord_bindings` / `plugin_chord_defaults` | as above, multi-key | |
| `inheriting_modes` | — | modes wanting Normal fallthrough |

### 5.2 Contexts (`KeyContext`)

`KeyContext` (`input/keybindings.rs:232`): `Global, Normal, Prompt, SearchPrompt,
Popup, Completion, FileExplorer, Dock, Menu, Terminal, Settings, CompositeBuffer,
Mode(String)`. Several have layered semantics:

- **`SearchPrompt`** is a *narrowing* of `Prompt` (`parent_context`, line 313): it
  owns the match-mode toggles (case/word/regex/confirm-each) and inherits every
  generic editing key from `Prompt`. This keeps Alt+W from flipping whole-word
  match while an unrelated close-confirmation prompt is up (doc, lines 238–245).
- **`Completion`** takes precedence over `Popup` so accept/dismiss can be bound
  independently of generic popup keys (lines 247–251).
- **Fallthrough policy** (lines 278–298): `CompositeBuffer` falls through to *all*
  Normal bindings (`allows_normal_fallthrough`); `FileExplorer`, `Dock`, and
  plugin `Mode(_)` fall through only for the curated `is_terminal_ui_action`
  whitelist (`allows_ui_fallthrough`) so split-nav/palette/save work while focus
  is on chrome, but the panel's own keys aren't shadowed (#1903; §18 of
  `search-replace-scope-replan-on-widgets.md`).

`from_when_clause` (line 321) parses the `"when"` strings (`mode:git-log`,
`searchPrompt`, …) used in keymap/plugin bindings.

### 5.3 Single-key resolution order

`resolve` (`input/keybindings.rs:1971`) checks, in order:

1. custom **Global**, 2. keymap **Global**,
3. custom **context**, 4. keymap **context**, 5. **plugin_defaults** context,
6. **parent context** (custom then keymap — e.g. SearchPrompt → Prompt, lines
   2039–2050),
7. **Normal fallthrough** (lines 2055–2106): full for `allows_normal_fallthrough`
   / inheriting modes, otherwise only `is_application_wide_action` (Quit, Save,
   Esc-cancel) or — for UI-fallthrough contexts — `is_terminal_ui_action`. A
   user binding in Normal *shadows* the keymap default even when it doesn't
   qualify for fallthrough, so a user can disable an application-wide default like
   `Ctrl+Q → Quit` with `noop` (#2030, lines 2061–2073/2090).
8. Finally, raw character input in text-input contexts (`InsertChar`, line 2108+).

A related helper `resolve_in_context_only` (line 2124) bypasses Global/Normal so
prompt-specific Alt bindings don't collide with menu mnemonics (used at
`app/input_dispatch.rs:203`).

### 5.4 Chords

Chords are multi-key sequences. `chord_state` is a `Vec<(KeyCode, KeyModifiers)>`
held per-window. `resolve_chord` (line 1902) builds the normalized full sequence
and searches the same tiers (custom global → keymap global → custom context →
keymap context → plugin context, lines 1923–1937), returning
`Complete(action)` / `Partial` (a prefix of some binding) / `NoMatch`. In
`handle_key`, `Partial` pushes the key and waits (line 895), `Complete` clears the
state and dispatches, `NoMatch` clears any stale prefix (lines 885–905). Chord
resolution runs both at the mode level (line 702) and the normal level (line 876).

### 5.5 Rebindability & reload

User overrides, keymap selection, and the keybinding editor all rebuild the
config-derived tiers. `reload_from_config` (line 1628) is the safe rebuild: it
reconstructs `bindings`/`default_bindings` from config but **carries over** the
runtime-only plugin state (`plugin_defaults`, `plugin_chord_defaults`,
`inheriting_modes`) via `mem::take` — because that state lives only in the resolver,
not in `Config`. Using a fresh `KeybindingResolver::new` instead would silently
drop every plugin binding until restart (#2307, commit `4b6e1d2f2`).

Plugin modes register via `load_plugin_default` / `load_plugin_chord_default`
(lines 1774/1788) under a `Mode(name)` context, cleared per-mode with
`clear_plugin_defaults_for_mode` (line 1801). `set_mode_inherits_normal_bindings`
(line 1810) toggles membership in `inheriting_modes`.

Discrepancy vs the plan doc: `unified-keybinding-resolution.md` proposes a
metadata `BufferMode` *without* `inherit_normal_bindings`; the shipped
`BufferMode` (`input/buffer_mode.rs:26`) adds that field plus the resolver's
`inheriting_modes` set — a later refinement that lets viewer-style modes inherit
Normal motion/selection/copy without re-declaring them. Treat that doc as the
intent; the code is authoritative.

---

## 6. Buffer modes

A buffer (or the global editor) can carry a named **mode**. `effective_mode()`
returns the buffer-local mode if present else the global mode, so virtual-buffer
modes aren't hijacked by a global mode (`app/input.rs:693`). Mode handling in
`handle_key` (lines 690–852):

- Mode chord + single-key resolution against `Mode(name)` (lines 700–732).
- If the mode `allows_text_input` (e.g. `search-replace-list`), unbound printable
  chars become `PluginAction("mode_text_input:<char>")` (line 755); clipboard /
  select-all keys are forwarded to the focused widget (lines 772–777);
  Shift+arrows extend the focused widget's selection (lines 786–837); other
  unbound keys are blocked.
- If the mode is `read_only` and *not* text-input, unbound keys are dropped
  (line 843); otherwise they fall through to normal dispatch.

`ModeRegistry` (`input/buffer_mode.rs`) only answers `is_read_only`,
`allows_text_input`, `inherits_normal_bindings`, and attribution — all *binding*
lookups go through `KeybindingResolver`.

---

## 7. Multi-cursor

The cursor set is `Cursors` (model). Multi-cursor follows one rule learned the
hard way (commits `dbfd12811`, `ec390ee73`): **never special-case the primary
cursor; always emit one event per cursor through the shared action→event path.**

`input/multi_cursor.rs` provides the *add-cursor* operations (invoked from
`handle_action` arms, `app/input.rs:1714`):

- `add_cursor_above` / `add_cursor_below` (lines 203/280) — same visual column on
  the adjacent line, clamped to line length, skipping the newline.
- `add_cursor_at_next_match` (line 68) — VSCode Ctrl-D: with no selection, selects
  the word at the cursor (`AddCursorResult::WordSelected`); with a selection,
  finds the next non-occupied occurrence of the pattern and adds a cursor there,
  preserving selection direction. Cycle detection guards against
  all-matches-occupied (lines 191–196). (Ctrl-D honoring an active *search* match
  rather than the surrounding word is commit `7bdbcd210`.)
- `line_end_positions_in_selection` (line 252) — Sublime "split selection into
  lines": every line touched by *any* cursor's selection contributes its
  end-of-line position, deduped in document order.

Once cursors exist, every edit/movement action runs for all of them inside
`input/actions.rs` (each `handle_*` iterates `cursors.iter()`), and the resulting
`Vec<Event>` (>1 event) is applied via `apply_events_as_bulk_edit` for O(n) edits
or `Event::Batch` for non-edit moves (`app/input_helpers.rs:312`). Block
(column/rectangular) selection lives in `actions.rs`
(`block_select_action`, line 356) and is converted to per-line cursors via
`convert_block_selection_to_cursors` (line 470) before normal multi-cursor logic
runs. `RemoveSecondaryCursors` collapses back to one (`app/input.rs:1915`).

`line_move.rs` implements `MoveLineUp/Down` (`move_lines`, line 213): it merges
each cursor's line range, swaps the block with the adjacent line as a
Delete+Insert pair, and remaps every cursor/anchor position into the moved region
(`map_position_in_region`, line 171) so selections survive the move.

---

## 8. Mouse → action mapping

`MouseInput::handle_mouse` (`app/mouse_input.rs:125`) is the mouse counterpart to
`handle_key`. Architecture (see `docs/internal/event-dispatch-architecture.md`):
ratatui is render-only, so Fresh maps screen coordinates back to components using
a **cached layout / retained-mode hit test** produced during render. Cached
regions include `tab_areas`, `status_bar_area`, `file_explorer_area`, and split
separators; the long-term plan (Phase 2/3 in that doc, PLANNED) is a unified
`HitArea` + z-index and eventually a Helix-style compositor. Issue #832 (menu bar
hardcoded to `row == 0`) is the canonical example of why fixed-row checks are
being eliminated.

Dispatch order mirrors the keyboard path:

1. `dispatch_modal_mouse` (line 141) walks the same overlay stack top-down as
   `get_key_context`, so modal capture stays in lock-step with the keyboard.
2. Terminal forwarding: `try_forward_mouse_to_terminal` (line 187) sends
   SGR-encoded mouse events to the PTY when over an alternate-screen terminal —
   suppressed while a chrome drag (dock/separator/explorer resize) is in progress
   (lines 180–191).
3. `Ctrl+Click` on a terminal-printed path opens the file (`try_open_terminal_link`,
   line 196).
4. Editor-pane routing: click positions the cursor; **drag** extends selection;
   **double-click → `Action::SelectWord`** (`handle_mouse_double_click`, line
   1321 → line 1487); **triple-click → `Action::SelectLine`** (line 1514 → 1646);
   scroll scrolls the viewport; status-bar indicator clicks map to actions
   (`SetLineEnding`/`SetEncoding`/`SetLanguage`/`ShowLspStatus`, lines 2410–2423).

So mouse gestures, where they have an editing meaning, fold into the *same*
`handle_action` pipeline as keys; chrome interactions (resize drags, tab close,
scrollbar) are handled directly against cached layout regions.

Windows note (`docs/internal/windows-mouse-input.md`, IMPLEMENTED): the real fix
was honoring `wRepeatCount` on coalesced console KEY_EVENT records (mode-1003
all-motion tracking floods the buffer) in the `fresh-winterm` crate; the earlier
"conhost 4KB / ConPTY self-hosting" theory in that doc is **stale/incorrect** and
should not be relied on.

---

## 9. Supporting subsystems

- **Position history** (`input/position_history.rs`) — VSCode-style
  Alt+Left/Right. Consumes `MoveCursor` events and *coalesces* consecutive small
  moves (≤ `LARGE_JUMP_THRESHOLD` = 50 bytes, line 52) into one jump entry;
  buffer switches and large jumps commit the pending movement. Fed via
  `track_cursor_movement` (`app/input_helpers.rs:353`), gated by `in_navigation`
  so back/forward navigation doesn't pollute the trail. `NavigateBack/Forward`
  actions drive it.
- **Input history** (`input/input_history.rs`) — bash/readline-style prompt
  history: Up/Down navigate prior entries non-destructively, separate histories
  per prompt type, serialization-ready (doc, lines 6–21). Driven by the
  `PromptHistoryPrev/Next` deferred actions (`app/input_dispatch.rs:619`).
- **Composite router** (`input/composite_router.rs`) — for side-by-side diff
  buffers, `route_key_event` (line 112) intercepts *only* composite-specific keys
  (j/k scroll, Tab/Shift+Tab pane switch) and returns `Unhandled` for everything
  else so normal dispatch and the `CompositeBuffer` keymap handle arrows, typing,
  and rebindable hunk nav (n/p/]/[). Wired in at `app/input_helpers.rs:376`.

---

## 10. Fuzzy matching and Quick Open

### 10.1 Fuzzy matcher (`input/fuzzy/`)

A custom fzf-style scorer (`fuzzy/mod.rs`, `fuzzy/matcher.rs`). Two strategies run
in parallel and the higher score wins: a DP pass over interleaved query chars and
a contiguous-substring pass that rewards tight matches. Scoring constants
(`fuzzy/mod.rs:44`): `CONSECUTIVE` (16), `WORD_BOUNDARY` (32, after
space/`_`/`-`/`/`/`.`), `START_OF_STRING` (48), `CAMEL_CASE` (24),
`EXACT_MATCH`/`EXACT_BASENAME_MATCH` (100/80), `CONTIGUOUS_SUBSTRING` (64),
`BASENAME_PREFIX` (64), `PATH_SEGMENT_PREFIX` (32), and gap penalties (−3/−5).
Pattern parsing (`fuzzy/pattern.rs`): query is **space-separated AND terms** (each
must match), case-insensitive, with an ASCII byte-level fast path
(`ascii_lower`) and a non-allocating subsequence pre-rejection. There are **no**
prefix operators (`'`, `^`, `!`). `FuzzyMatcher` (`matcher.rs:64`) reuses scratch
buffers via an arena-of-backpointers so the hot path is O(n·m).

### 10.2 Quick Open (`input/quick_open/`)

A unified prompt with **prefix-based routing** (module doc, "inspired by VSCode's
Ctrl+P"). `QuickOpenProvider` (`quick_open/mod.rs:181`) exposes `prefix()`,
`suggestions(query, ctx)`, and `on_select()`. `QuickOpenRegistry::
get_provider_for_input` (line 260) sorts prefixes longest-first, strips the
matched prefix, and falls back to the empty-prefix default. Built-in providers
(`quick_open/providers.rs`):

| Prefix | Provider | Result |
|---|---|---|
| (empty) | `FileProvider` (line 318) | `OpenFile { path, line, col }` |
| `>` | `CommandProvider` (line 20) | `ExecuteAction(Action)` |
| `#` | `BufferProvider` (line 113) | `ShowBuffer(id)` |
| `:` | `GotoLineProvider` (line 192) | `GotoLine(Absolute/Relative)` |

`on_select` returns a `QuickOpenResult` enum (`mod.rs:23`) that the host maps to an
action/navigation. `FileProvider` finds files via `git ls-files` (falling back to
a directory walk), loads asynchronously with partial UI updates, applies frecency
scoring (time-decay × access count), and parses a trailing `:line:col`. It can
swap filesystem backends for remote authorities (`set_backends`, line 370). The
corresponding actions (`QuickOpen`, `QuickOpenFiles`, `QuickOpenBuffers`,
`CommandPalette`) are all in the `Action` enum; `CommandPalette` is kept as an
alias of `QuickOpen` for keymap/plugin compatibility (`input/keybindings.rs:546`).

PLANNED: `docs/internal/finder-abstraction.md` proposes a generic `Finder<T>`
to collapse the five finder *plugins* (live grep, git grep, etc.) into one shared
abstraction (~87% code reduction). Not yet built; the core Quick Open above is
shipped. PLANNED: `docs/internal/flash-jump-plan.md` (flash/EasyMotion-style
label jump) is a validated proposal that depends on `getNextKey()` /
wildcard-binding plugin-API additions; the input path it needs already exists
(the `getNextKey` callback hook at `app/input.rs:505`).

---

## 11. Discrepancies & notes for maintainers

- `unified-keybinding-resolution.md` is an accurate description of the *intent*
  but predates `inherit_normal_bindings` / `inheriting_modes`; the code is the
  source of truth.
- `event-dispatch-architecture.md` describes Phases 2–3 (unified HitArea,
  compositor) as PLANNED; only the cached-layout retained-mode hit testing is
  shipped.
- `windows-mouse-input.md` contains a **stale** ConPTY-bug theory; the shipped fix
  is `wRepeatCount` handling. Flag before citing.
- `design-decisions.md` §4 (Fuzzy Finder UX) and §16 (Event Dispatch) describe the
  shipped behavior plus planned evolution; §13's "view transform one frame late"
  is a known, partially-mitigated timing issue unrelated to core input.

---

## Superseded / consolidated source docs

This document consolidates and supersedes the input-relevant content of:

- `docs/internal/unified-keybinding-resolution.md` — fully realized; kept only as
  historical design rationale (intent now diverges from code re:
  `inherit_normal_bindings`).
- `docs/internal/event-dispatch-architecture.md` — its "current state" is captured
  here in §3/§8; its Phase 2/3 recommendations remain a standalone PLANNED roadmap.

Still authoritative as standalone references (cited, not superseded):

- `docs/internal/flash-jump-plan.md` — PLANNED feature spec.
- `docs/internal/finder-abstraction.md` — PLANNED plugin-side refactor.
- `docs/internal/windows-mouse-input.md` — platform deep-dive (mind the stale
  ConPTY section).
- `docs/internal/design-decisions.md` §4, §16 — broader product/architecture
  decisions beyond input.
