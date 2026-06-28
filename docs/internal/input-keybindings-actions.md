# Input, Keybindings, and the Action/Event Model

> _AI-generated: describes Fresh's architecture and design rationale, not implementation details; where it disagrees with the source, the source is authoritative._

Purpose: explain how Fresh turns a raw terminal keystroke (or mouse event) into a
buffer change — the key-translation layer, the modal dispatch priority, unified
keybinding resolution, the command → action → event pipeline, multi-cursor, and
fuzzy/quick-open input. Everything below is IMPLEMENTED unless explicitly flagged
PLANNED.

---

## 1. The pipeline at a glance

A keystroke flows through these stages:

```
Terminal (crossterm KeyEvent)
  → KeyTranslator.translate()      — calibration fixups
  → Editor::handle_key()           — modal priority + chord state
      → KeybindingResolver.resolve() — key → Action
  → Editor::handle_action(Action)  — large match; side-effects or…
      → action_to_events(Action)   — Action → events
  → log_and_apply_event / bulk edit — events mutate buffer + undo log
```

Three distinct vocabularies live here and the separation is deliberate (§4):

- **Command** — a user-facing, localized, context-filtered palette entry. Wraps
  exactly one `Action`.
- **Action** — what a keypress *means* in editor terms (`MoveUp`, `Save`,
  `InsertChar('a')`). The `Action` enum is the rebinding currency and the
  serialization unit for keymaps/macros.
- **Event** — what actually happens to the buffer/cursors (`Event::Insert`,
  `Event::Delete`, `Event::MoveCursor`, `Event::BulkEdit`). Events are the undo
  unit and the plugin-hook unit.

---

## 2. Key translation across terminals/platforms

Terminals are inconsistent about what bytes they emit for a given physical key.
Fresh handles this in three layers, applied in order, *before* keybinding
resolution.

### 2.1 KeyTranslator (per-user calibration)

The `KeyTranslator` is a map that rewrites a raw `KeyEvent` to a normalized one.
It is populated by the input-calibration wizard (`Action::CalibrateInput`) and
persisted to a calibration file. The canonical order is fixed:
"Terminal → KeyTranslator → KeybindingResolver". Rationale: keep "fix the broken
terminal" separate from "rebind the action" — calibration must not pollute the
keymap. Unmapped keys pass through untouched.

### 2.2 `normalize_key` (resolution-time canonicalization)

`normalize_key` folds redundant modifier noise so a single binding matches every
terminal's encoding of the same key:

- `BackTab`/`Backspace` + SHIFT → drop the redundant SHIFT.
- Uppercase `Char('P')` → lowercase + inferred SHIFT, *unless* CONTROL is set.
  This handles the divergence in how terminals report shifted characters:
  kitty-protocol terminals send a real SHIFT with an uppercase char, non-kitty
  terminals send a bare uppercase char with no modifier, and CapsLock+Ctrl sends
  uppercase + CONTROL. Inferring SHIFT for the no-CONTROL case makes `Shift+P` /
  `Alt+Shift+F` bindings match uniformly, while the CONTROL exception preserves
  the "CapsLock+Ctrl+A still fires Ctrl+A" intent.

`normalize_key` is applied to both the live key and every chord-state key in both
single-key and chord resolution.

### 2.3 `terminal_key_equivalents` (alias expansion)

`terminal_key_equivalents` maps control-key combos that terminals encode
differently into aliases that are *inserted alongside* the primary binding at
load time:

- `Ctrl+/` ↔ `Ctrl+7`, `Ctrl+Backspace` ↔ `Ctrl+H`, `Ctrl+Space` ↔ `Ctrl+@`
  (NUL), `Ctrl+-` ↔ `Ctrl+_`.

Explicit bindings always win over auto-generated equivalents; a conflict only
logs a warning. The keybinding priority score deprioritizes the
terminal-equivalent half when picking a canonical display key.

The text-input-modifier check additionally treats Windows AltGr (reported as
Ctrl+Alt by crossterm) as text-input-capable so international layouts can type
`@ [ ] { }`.

PLANNED/orthogonal: the kitty keyboard protocol enhancement flags
(`DISAMBIGUATE_ESCAPE_CODES`, `REPORT_ALTERNATE_KEYS`) are enabled at the
terminal layer so capable terminals can report `Shift+Up`, etc.; unsupported
terminals degrade rather than getting a custom fallback.

---

## 3. Modal dispatch priority

`Editor::handle_key` is the central router. It establishes a strict precedence so
that overlays consume input before the buffer does. The priority is computed from
an **overlay layer stack**, not a hand-listed ladder — `get_key_context()` and
`dispatch_modal_input` both consult the overlay layers so the keyboard, mouse, and
terminal paths cannot drift out of sync (this is the modern replacement for an
older hardcoded order).

Order of checks in `handle_key`:

1. **Event-debug dialog** intercepts *all* keys first.
2. **Terminal mode** — `dispatch_terminal_input`. Returns `None` (falls through)
   if any *blocking* overlay is up. Plugin commands flagged `terminalBypass: true`
   are resolved against the Normal context and dispatched *before* the PTY claims
   the key — that is how `Orchestrator: Open` stays reachable from inside `htop`.
3. **`getNextKey()` plugin callback** — a plugin awaiting the next key (vi
   find-char, flash labels) gets it before any other dispatch.
4. **Floating widget panel** (focused) and **focused dock** swallow keys; the
   dock-focus toggle is resolved early so it's symmetric in/out.
5. **Transient popups** (hover, signature help) are dismissed on any key except
   the copy/focus-popup keys; **unfocused popup** cancel/focus keys are honored
   next.
6. **`dispatch_modal_input`** — the heart of the modal ladder:
   - `dispatch_modal_keyboard` walks the overlay stack top-down for the four
     *capture-all* modals — **Settings → KeybindingEditor → CalibrationWizard →
     Menu**. These early-return.
   - **Prompt**: Alt+char prompt bindings resolved context-only first; then
     file-browser / query-replace / overlay-toolbar handlers; finally the prompt's
     own `dispatch_input`. A prompt that returns `Ignored` falls through to global
     keybindings.
   - **Popup**, gated by `popups_capture_keys()` so editor-pane popups don't eat
     keys when the file explorer is focused. Completion popups consult the
     `Completion` keybinding context first; global popups outrank buffer popups.
     `Ignored` falls through.
7. **Buffer-mode bindings** — only in `Normal`/`CompositeBuffer` context. Chord
   then single-key resolution against the `Mode(name)` context.
8. **Composite-buffer routing** via `try_route_composite_key`.
9. **Chord + single-key resolution** in the resolved context, then
   `handle_action(action)`.

The net priority is **Settings → Menu → Prompt → Popup → (mode) → Normal**, with
terminal mode and floating overlays gating ahead of all of them.

### 3.1 The hierarchical InputHandler trait + DeferredAction

Modal components (Settings, Menu, Prompt, Popup, file browser) implement
`InputHandler`. The contract:

- **Leaf-first, bubble up**: `dispatch_input` tries the focused child first, then
  the handler itself.
- **Explicit consumption**: handlers return `InputResult::Consumed` (stop) or
  `Ignored` (try parent).
- **Modals consume by default** *unless they explicitly return `Ignored`* — this
  opt-out is what lets, e.g., Ctrl+P toggle Quick Open closed while the prompt is
  open. No DOM-style capture phase, by choice.

Handlers cannot mutate `Editor` directly (they only borrow their own sub-state),
so they enqueue **`DeferredAction`** values into `InputContext.deferred_actions`.
After dispatch, `process_deferred_actions` drains the queue with full `&mut Editor`
access and `execute_deferred_action` performs the real side-effect
(`CloseSettings`, `ExecuteMenuAction`, `SendTerminalKey`, `ExecuteAction`, …).
This is the borrow-checker-friendly bridge between self-contained handlers and
editor-global mutation. `DeferredAction::ExecuteAction(Action)` lets a handler
fall back into the normal action pipeline.

---

## 4. Actions vs Events: the command → action → event pipeline

### 4.1 Why three layers

- **Rebindability & serialization.** `Action` is `Serialize/Deserialize` and
  round-trips through string names via a declarative action-name-mapping macro,
  which generates `from_str`, `to_action_str`, and `all_action_names` from one
  definition so the keymap parser, keybinding editor, and macro recorder can never
  drift. Keybindings and macros store Actions, not Events — Events are
  position-specific and would not replay.
- **Undo/redo & plugin hooks.** Events are the transaction record. Every buffer
  mutation flows through `log_and_apply_event`, which appends to the `EventLog`
  (undo chain) and then applies. `Action`s that aren't edits (e.g. `Quit`,
  `OpenSettings`) produce no events at all (`action_to_events` returns `None`).
- **Layout independence.** `Action::MoveUp` is purely semantic; the conversion to
  a concrete `Event::MoveCursor` resolves soft-wrap visual lines via the cached
  layout. One action, many possible byte targets.

### 4.2 The two-step conversion

`handle_action` is a large match. Two kinds of arms:

1. **Direct side-effects** — `Save` opens a prompt or writes the file;
   `AddCursorNextMatch` calls into the multi-cursor module.
2. **Catch-all edit/movement** — delegated to `apply_action_as_events`, which
   calls `Window::action_to_events`. That bridge tries visual-line movement and
   page motion first, then falls back to the logical `action_to_events` converter.

`action_to_events` is the pure `(state, cursors, events, …)` converter, decomposed
from a single large match into per-action `handle_*` helpers. It is where editor
semantics live: auto-close/auto-pair, smart-backspace dedent, tiered auto-indent
(tree-sitter grammar → per-language regex rules → C-style bracket scanner),
CRLF-as-one handling, and visual-column sticky movement.

### 4.3 Applying events

The event-apply layer has three entry points:

- **`log_and_apply_event`** — canonical single-event path.
- **`apply_event_to_active_buffer`** — apply without logging (replay). Critically,
  LSP change positions and plugin-hook line info are computed *before* the buffer
  mutates so byte offsets are pre-modification.
- **`apply_events_as_bulk_edit`** — batches multi-event edits under one undo
  boundary, sorting by descending position so earlier edits don't shift later
  ones, producing one `Event::BulkEdit` with old/new snapshots. Used by
  replace-all, format-on-save, LSP rename, and **multi-cursor typing** — this is
  the linear-time (not quadratic) path.

The caller in `apply_action_as_events` picks the path: more than one event with
buffer mods → bulk edit; more than one event without mods → `Event::Batch` (atomic
undo for multi-cursor moves); single event → `log_and_apply_event`.

### 4.4 Commands

The command layer defines a set of static command definitions, each i18n-keyed
with an `action` and a context list. A `Command` is the localized, context-aware
wrapper the palette shows; its `.action` field is the `Action` actually executed.
The `CommandRegistry` merges builtin commands with plugin-registered ones, supports
first-writer-wins plugin registration, tracks usage history for recency sorting,
and exposes `filter()` — the palette entry point that fuzzy-matches +
context-filters + sorts. Selecting a command ultimately runs its `Action` through
`handle_action`.

---

## 5. Unified keybinding resolution

### 5.1 Goal and structure

The design goal: a **single resolution path** for builtin, keymap, user-custom,
and plugin-mode bindings — no separate `ModeRegistry` lookup, no mode inheritance.
This is implemented; `ModeRegistry` is now metadata-only (`read_only`,
`allow_text_input`, `inherit_normal_bindings`, `plugin_name`).

`KeybindingResolver` holds six maps plus a set:

| Field | Tier | Source |
|---|---|---|
| `bindings` | custom | user `keybindings` in config |
| `default_bindings` | keymap | active keymap (`default`/`emacs`/`vscode`…) |
| `plugin_defaults` | plugin | `defineMode()` runtime registration |
| `chord_bindings` / `default_chord_bindings` / `plugin_chord_defaults` | as above, multi-key | |
| `inheriting_modes` | — | modes wanting Normal fallthrough |

### 5.2 Contexts (`KeyContext`)

`KeyContext` values: `Global, Normal, Prompt, SearchPrompt, Popup, Completion,
FileExplorer, Dock, Menu, Terminal, Settings, CompositeBuffer, Mode(String)`.
Several have layered semantics:

- **`SearchPrompt`** is a *narrowing* of `Prompt` (its parent context): it owns the
  match-mode toggles (case/word/regex/confirm-each) and inherits every generic
  editing key from `Prompt`. This keeps Alt+W from flipping whole-word match while
  an unrelated close-confirmation prompt is up.
- **`Completion`** takes precedence over `Popup` so accept/dismiss can be bound
  independently of generic popup keys.
- **Fallthrough policy**: `CompositeBuffer` falls through to *all* Normal bindings
  (`allows_normal_fallthrough`); `FileExplorer`, `Dock`, and plugin `Mode(_)` fall
  through only for the curated `is_terminal_ui_action` whitelist
  (`allows_ui_fallthrough`) so split-nav/palette/save work while focus is on
  chrome, but the panel's own keys aren't shadowed.

`from_when_clause` parses the `"when"` strings (`mode:git-log`, `searchPrompt`, …)
used in keymap/plugin bindings.

### 5.3 Single-key resolution order

`resolve` checks, in order:

1. custom **Global**, 2. keymap **Global**,
3. custom **context**, 4. keymap **context**, 5. **plugin_defaults** context,
6. **parent context** (custom then keymap — e.g. SearchPrompt → Prompt),
7. **Normal fallthrough**: full for `allows_normal_fallthrough` / inheriting modes,
   otherwise only `is_application_wide_action` (Quit, Save, Esc-cancel) or — for
   UI-fallthrough contexts — `is_terminal_ui_action`. A user binding in Normal
   *shadows* the keymap default even when it doesn't qualify for fallthrough, so a
   user can disable an application-wide default like `Ctrl+Q → Quit` with `noop`.
8. Finally, raw character input in text-input contexts (`InsertChar`).

A related helper `resolve_in_context_only` bypasses Global/Normal so prompt-specific
Alt bindings don't collide with menu mnemonics.

### 5.4 Chords

Chords are multi-key sequences. The chord state is a per-window list of
`(KeyCode, KeyModifiers)` pairs. `resolve_chord` builds the normalized full
sequence and searches the same tiers (custom global → keymap global → custom
context → keymap context → plugin context), returning `Complete(action)` /
`Partial` (a prefix of some binding) / `NoMatch`. In `handle_key`, `Partial` pushes
the key and waits, `Complete` clears the state and dispatches, `NoMatch` clears any
stale prefix. Chord resolution runs both at the mode level and the normal level.

### 5.5 Rebindability & reload

User overrides, keymap selection, and the keybinding editor all rebuild the
config-derived tiers. `reload_from_config` is the safe rebuild: it reconstructs
`bindings`/`default_bindings` from config but **carries over** the runtime-only
plugin state (`plugin_defaults`, `plugin_chord_defaults`, `inheriting_modes`)
because that state lives only in the resolver, not in `Config`. Constructing a
fresh resolver instead would silently drop every plugin binding until restart.

Plugin modes register via `load_plugin_default` / `load_plugin_chord_default` under
a `Mode(name)` context, cleared per-mode with `clear_plugin_defaults_for_mode`.
`set_mode_inherits_normal_bindings` toggles membership in `inheriting_modes`.

Relationship to the design intent: the original unified-resolution design proposed
a metadata `BufferMode` *without* `inherit_normal_bindings`; the shipped
`BufferMode` adds that field plus the resolver's `inheriting_modes` set — a later
refinement that lets viewer-style modes inherit Normal motion/selection/copy
without re-declaring them. The source is authoritative.

---

## 6. Buffer modes

A buffer (or the global editor) can carry a named **mode**. `effective_mode()`
returns the buffer-local mode if present else the global mode, so virtual-buffer
modes aren't hijacked by a global mode. Mode handling in `handle_key`:

- Mode chord + single-key resolution against `Mode(name)`.
- If the mode `allows_text_input` (e.g. `search-replace-list`), unbound printable
  chars become `PluginAction("mode_text_input:<char>")`; clipboard / select-all
  keys are forwarded to the focused widget; Shift+arrows extend the focused
  widget's selection; other unbound keys are blocked.
- If the mode is `read_only` and *not* text-input, unbound keys are dropped;
  otherwise they fall through to normal dispatch.

`ModeRegistry` only answers `is_read_only`, `allows_text_input`,
`inherits_normal_bindings`, and attribution — all *binding* lookups go through
`KeybindingResolver`.

---

## 7. Multi-cursor

The cursor set is the `Cursors` model. Multi-cursor follows one rule learned the
hard way: **never special-case the primary cursor; always emit one event per
cursor through the shared action→event path.**

The multi-cursor module provides the *add-cursor* operations (invoked from
`handle_action` arms):

- `add_cursor_above` / `add_cursor_below` — same visual column on the adjacent
  line, clamped to line length, skipping the newline.
- `add_cursor_at_next_match` — VSCode Ctrl-D: with no selection, selects the word
  at the cursor (`AddCursorResult::WordSelected`); with a selection, finds the next
  non-occupied occurrence of the pattern and adds a cursor there, preserving
  selection direction. Cycle detection guards against all-matches-occupied. Ctrl-D
  honors an active *search* match rather than the surrounding word when one is
  present.
- `line_end_positions_in_selection` — Sublime "split selection into lines": every
  line touched by *any* cursor's selection contributes its end-of-line position,
  deduped in document order.

Once cursors exist, every edit/movement action runs for all of them inside the
action→event converter (each `handle_*` iterates the cursors), and the resulting
events (more than one) are applied via `apply_events_as_bulk_edit` for linear-time
edits or `Event::Batch` for non-edit moves. Block (column/rectangular) selection
lives in the action converter and is converted to per-line cursors via
`convert_block_selection_to_cursors` before normal multi-cursor logic runs.
`RemoveSecondaryCursors` collapses back to one.

The line-move logic implements `MoveLineUp/Down`: it merges each cursor's line
range, swaps the block with the adjacent line as a Delete+Insert pair, and remaps
every cursor/anchor position into the moved region so selections survive the move.

---

## 8. Mouse → action mapping

`MouseInput::handle_mouse` is the mouse counterpart to `handle_key`. Architecture:
ratatui is render-only, so Fresh maps screen coordinates back to components using a
**cached layout / retained-mode hit test** produced during render. Cached regions
include tab areas, the status-bar area, the file-explorer area, and split
separators; the long-term plan (PLANNED) is a unified `HitArea` + z-index and
eventually a Helix-style compositor. Fixed-row checks (e.g. a menu bar hardcoded to
`row == 0`) are being eliminated in favor of the cached-region approach.

Dispatch order mirrors the keyboard path:

1. `dispatch_modal_mouse` walks the same overlay stack top-down as
   `get_key_context`, so modal capture stays in lock-step with the keyboard.
2. Terminal forwarding: `try_forward_mouse_to_terminal` sends SGR-encoded mouse
   events to the PTY when over an alternate-screen terminal — suppressed while a
   chrome drag (dock/separator/explorer resize) is in progress.
3. `Ctrl+Click` on a terminal-printed path opens the file.
4. Editor-pane routing: click positions the cursor; **drag** extends selection;
   **double-click → `Action::SelectWord`**; **triple-click → `Action::SelectLine`**;
   scroll scrolls the viewport; status-bar indicator clicks map to actions
   (`SetLineEnding`/`SetEncoding`/`SetLanguage`/`ShowLspStatus`).

So mouse gestures, where they have an editing meaning, fold into the *same*
`handle_action` pipeline as keys; chrome interactions (resize drags, tab close,
scrollbar) are handled directly against cached layout regions.

Windows note (IMPLEMENTED): the fix was honoring `wRepeatCount` on coalesced
console KEY_EVENT records (mode-1003 all-motion tracking floods the buffer) in the
`fresh-winterm` crate.

---

## 9. Supporting subsystems

- **Position history** — VSCode-style Alt+Left/Right. Consumes `MoveCursor` events
  and *coalesces* consecutive small moves (below a large-jump threshold) into one
  jump entry; buffer switches and large jumps commit the pending movement. Fed via
  `track_cursor_movement`, gated by `in_navigation` so back/forward navigation
  doesn't pollute the trail. `NavigateBack/Forward` actions drive it.
- **Input history** — bash/readline-style prompt history: Up/Down navigate prior
  entries non-destructively, separate histories per prompt type,
  serialization-ready. Driven by the `PromptHistoryPrev/Next` deferred actions.
- **Composite router** — for side-by-side diff buffers, `route_key_event`
  intercepts *only* composite-specific keys (j/k scroll, Tab/Shift+Tab pane switch)
  and returns `Unhandled` for everything else so normal dispatch and the
  `CompositeBuffer` keymap handle arrows, typing, and rebindable hunk nav
  (n/p/]/[).

---

## 10. Fuzzy matching and Quick Open

### 10.1 Fuzzy matcher (`input/fuzzy/`)

A custom fzf-style scorer. Two strategies run in parallel and the higher score
wins: a dynamic-programming pass over interleaved query chars and a
contiguous-substring pass that rewards tight matches. Scoring rewards consecutive
matches, word boundaries (after space/`_`/`-`/`/`/`.`), start-of-string,
camelCase, exact and exact-basename matches, contiguous substrings, basename
prefixes, and path-segment prefixes, with gap penalties. Pattern parsing: the query
is **space-separated AND terms** (each must match), case-insensitive, with an ASCII
byte-level fast path and a non-allocating subsequence pre-rejection. There are
**no** prefix operators (`'`, `^`, `!`). The matcher reuses scratch buffers via an
arena-of-backpointers so the hot path is linear in the product of the input
lengths.

### 10.2 Quick Open

A unified prompt with **prefix-based routing**, inspired by VSCode's Ctrl+P. A
`QuickOpenProvider` exposes `prefix()`, `suggestions(query, ctx)`, and
`on_select()`. The registry's `get_provider_for_input` sorts prefixes
longest-first, strips the matched prefix, and falls back to the empty-prefix
default. Built-in providers:

| Prefix | Provider | Result |
|---|---|---|
| (empty) | `FileProvider` | `OpenFile { path, line, col }` |
| `>` | `CommandProvider` | `ExecuteAction(Action)` |
| `#` | `BufferProvider` | `ShowBuffer(id)` |
| `:` | `GotoLineProvider` | `GotoLine(Absolute/Relative)` |

`on_select` returns a `QuickOpenResult` enum that the host maps to an
action/navigation. `FileProvider` finds files via `git ls-files` (falling back to a
directory walk), loads asynchronously with partial UI updates, applies frecency
scoring (time-decay × access count), and parses a trailing `:line:col`. It can swap
filesystem backends for remote authorities. The corresponding actions (`QuickOpen`,
`QuickOpenFiles`, `QuickOpenBuffers`, `CommandPalette`) are all in the `Action`
enum; `CommandPalette` is kept as an alias of `QuickOpen` for keymap/plugin
compatibility.

PLANNED: a generic `Finder<T>` abstraction proposes collapsing the finder *plugins*
(live grep, git grep, etc.) into one shared abstraction. Not yet built; the core
Quick Open above is shipped. PLANNED: a flash/EasyMotion-style label jump is a
validated proposal that depends on `getNextKey()` / wildcard-binding plugin-API
additions; the input path it needs already exists (the `getNextKey` callback hook).

---

## 11. Notes for maintainers

- The unified-keybinding-resolution design doc describes the *intent* but predates
  `inherit_normal_bindings` / `inheriting_modes`; the source is the source of
  truth.
- The event-dispatch architecture doc describes a unified HitArea and compositor as
  PLANNED; only the cached-layout retained-mode hit testing is shipped.
- The Windows mouse-input fix that is shipped is `wRepeatCount` handling.
- The design-decisions doc covers Fuzzy Finder UX and Event Dispatch — shipped
  behavior plus planned evolution. The "view transform one frame late" item is a
  known, partially-mitigated timing issue unrelated to core input.

---

## Superseded / consolidated source docs

This document consolidates and supersedes the input-relevant content of:

- The unified-keybinding-resolution design doc — fully realized; kept only as
  historical design rationale (intent now diverges from code re:
  `inherit_normal_bindings`).
- The event-dispatch architecture doc — its "current state" is captured here in
  §3/§8; its later-phase recommendations remain a standalone PLANNED roadmap.

Still relevant as standalone references:

- The flash-jump plan — PLANNED feature spec.
- The finder-abstraction proposal — PLANNED plugin-side refactor.
- The Windows mouse-input deep-dive — platform-specific detail.
- The design-decisions doc — broader product/architecture decisions beyond input.
