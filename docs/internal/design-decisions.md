# Design Decisions & Architectural Trade-offs

This document preserves the key design decisions, trade-offs, and rationale
from Fresh's development history. It serves as an audit trail so future
contributors can understand *why* things are the way they are without needing
to rediscover the reasoning.

For the full original design documents, see the individual files in this
directory where they still exist as active/in-progress work.

> **Scope**: Covers decisions that have been **implemented and shipped**.
> In-progress designs remain in their own files.

---

## Table of Contents

1. [Time Abstraction (TimeSource)](#1-time-abstraction-timesource)
2. [Bulk Edit Optimization](#2-bulk-edit-optimization)
3. [CLI Architecture & Session Management](#3-cli-architecture--session-management)
4. [Fuzzy File Finder UX](#4-fuzzy-file-finder-ux)
5. [File Encoding Support](#5-file-encoding-support)
6. [Diff View & Scroll Sync](#6-diff-view--scroll-sync)
7. [Paste Handling](#7-paste-handling)
8. [Session Persistence (Client/Server)](#8-session-persistence-clientserver)
9. [Internationalization (i18n)](#9-internationalization-i18n)
10. [Per-Buffer Per-View State](#10-per-buffer-per-view-state)
11. [EditorState Refactoring](#11-editorstate-refactoring)
12. [Configuration System (4-Layer Overlay)](#12-configuration-system-4-layer-overlay)
13. [Plugin Architecture (Provider Pattern)](#13-plugin-architecture-provider-pattern)
14. [Vi Mode](#14-vi-mode)
15. [Markdown Compose Mode](#15-markdown-compose-mode)
16. [Event Dispatch & Hit Testing](#16-event-dispatch--hit-testing)
17. [Warning & Notification UX](#17-warning--notification-ux)
18. [Terminal Architecture](#18-terminal-architecture)
19. [Theme System](#19-theme-system)
20. [Visual Layout & Width Calculations](#20-visual-layout--width-calculations)

---

## 1. Time Abstraction (TimeSource)

**Problem**: Tests using real wall-clock time are slow and non-deterministic.

**Decision**: Introduce a `TimeSource` trait (`src/services/time_source.rs`)
with `RealTimeSource` for production and `TestTimeSource` for tests.
`TestTimeSource` advances logical time rather than sleeping.

**Trade-offs considered**:
- Full time abstraction everywhere vs selective abstraction
- Chose **selective**: abstract where testable, keep real time where it
  fundamentally must be (main event loop's `crossterm::event::poll`, signal
  handler `thread::sleep`)

**Key principle**: Services receive `SharedTimeSource` through composition.
Future time-based code should use this abstraction.

*Original doc: `timesource-design.md` (self-declares "now implemented")*

---

## 2. Bulk Edit Optimization

**Problem**: Multi-cursor edits via sequential `Event::Batch` had O(n²)
complexity — each event triggered a full tree traversal.

**Decision**: Introduce `Event::BulkEdit` that applies all edits in a single
tree traversal. Use `Arc` clone of the tree snapshot for O(1) undo instead of
storing individual events.

**Impact**: ~500× improvement for multi-cursor operations. All multi-cursor,
replace-all, toggle-comment, indent, LSP rename, and multi-cursor paste now
use `BulkEdit`.

**Key principle**: Converting N sequential operations into 1 structural
operation. Arc snapshots are cheap — exploit that for undo.

*Original doc: `bulk-edit-optimization.md`*

---

## 3. CLI Architecture & Session Management

**Problem**: Flat flag structure didn't scale as Fresh gained session
management, remote editing, and file-opening features.

**Decision**: Move to git/cargo-style subcommands (`fresh session attach`,
`fresh session list`) with backward-compatible shortcuts (`fresh -a`).
Deprecated flags produce warnings rather than breaking.

**Trade-offs considered**:
- Discoverability (explicit subcommands) vs power-user efficiency (shortcuts)
- Chose **both**: full subcommands + hidden shortcuts for common cases
- Progressive disclosure: `fresh file.txt` (simple) vs
  `fresh session attach --name dev` (explicit)

*Original doc: `cli-redesign.md` (marked "Implemented (Experimental)")*

---

## 4. Fuzzy File Finder UX

**Problem**: Needed a universal entry point for file finding, buffer switching,
and command execution.

**Decision**: Unified `Ctrl+P` with prefix-based mode switching (VSCode model):
no prefix = files, `>` = commands, `#` = buffers, `:` = go-to-line.

**Research**: Comparative analysis of VSCode, Sublime, Neovim (Telescope),
JetBrains, and Emacs. Key takeaways:
- Default to the common case (file finding)
- Make modes discoverable via hint line
- Use frecency ranking (frequency × recency) for personalized results

**File discovery hierarchy**: git ls-files → fd → find → manual traversal
(best performance where available, respects `.gitignore`).

**Implementation**: `plugins/find_file.ts` with the `Finder<T>` abstraction
planned for further deduplication (see `finder-abstraction.md` for the
in-progress design that targets 87% code reduction across 5 finder plugins).

*Original doc: `FUZZY_FILE_FINDER_UX.md`*

---

## 5. File Encoding Support

**Problem**: Support non-UTF-8 files (Latin-1, Shift-JIS, GBK, etc.) without
breaking the UTF-8-based editing pipeline.

**Decision**: **Normalize on Load** — convert to UTF-8 immediately, track
original encoding, convert back on save. Mirrors the CR/LF architecture
(detect → track → convert back).

**Alternatives rejected**:
- *Lazy Transcoding*: decode on demand — complex cursor math, fragile
- *Hybrid*: normalize small files, lazy for large — two code paths
- *UTF-8 Only*: lose non-UTF-8 users

**Open questions preserved**: Invalid byte handling strategy, mixed-encoding
detection, chunk boundary alignment for multi-byte encodings.

*Original doc: `encoding-support-design.md`*

---

## 6. Diff View & Scroll Sync

**Problem**: Side-by-side diff needs aligned rendering of non-consecutive
lines, and scroll sync between panes causes feedback loops when done via async
plugin hooks.

**Diff view decision**: Introduce `CompositeBuffer` with `ChunkAlignment`
markers at hunk boundaries. Markers are O(chunks) not O(lines) and
auto-adjust when edits occur. Separate rendering path for composite buffers
(aligned with gaps) vs normal buffers (consecutive lines).

**Scroll sync decision**: Use **marker-based sync anchors** instead of async
plugin hooks. Single source of truth: `scroll_line` in the left buffer's line
space, derived positions for the right pane. Synchronous sync at render time
eliminates race conditions and jitter. Leverages existing `MarkerList` /
`IntervalTree` infrastructure.

*Original docs: `diff-view.md`, `scroll-sync-design.md`*

---

## 7. Paste Handling

**Problem**: External paste (Cmd+V in terminal) produces a burst of key events
indistinguishable from fast typing, causing unwanted auto-close/auto-indent.

**Decision**: Two-tier approach:
1. **Bracketed paste mode** (primary) — terminal signals paste boundaries
2. **Burst coalescing heuristic** (fallback) — detect rapid input bursts for
   terminals without bracketed paste support

Both paths produce a single "atomic insert" for consistent undo behavior.
Auto-close and skip-over are suppressed during paste.

*Original doc: `paste-handling.md`*

---

## 8. Session Persistence (Client/Server)

**Problem**: Terminal editors lose state when the terminal disconnects. Need
detach/reattach like tmux but integrated into the editor.

**Decision**: **Dual-socket client/server** architecture:
- Data socket: raw terminal I/O bytes (high throughput, zero parsing)
- Control socket: JSON messages for resize, handshake, version negotiation

**Ultra-light client principle**: Client is ~80-100 lines, a "dumb pipe." All
complexity lives server-side for easier testing and fault isolation.

**Alternatives rejected** (with detailed trade-off matrix):
1. *Single socket with escape framing* — parsing overhead, false positives
2. *Reconnection on resize* — loses state between connections
3. *Shared memory* — platform-specific, complex synchronization
4. *TCP* — unnecessary network stack overhead for local IPC

**IPC**: Unix sockets (Linux/macOS), named pipes (Windows) via `interprocess`
crate.

**Known limitations** (documented for future work): single client at a time,
no crash resurrection, no multi-client broadcast.

*Original doc: `session-persistence-design.md`*

---

## 9. Internationalization (i18n)

**Problem**: All UI strings were hardcoded in English.

**Decision**: Use `rust-i18n` crate with compile-time embedding via
`include_str!`. JSON locale files, zero runtime overhead for the default
locale.

**Alternatives rejected**:
- *Project Fluent*: more sophisticated pluralization but heavier runtime, less
  familiar format
- *gettext-rs*: industry standard but requires `.po` toolchain, FFI dependency

**Migration strategy**: 6-phase approach prioritized by visibility:
status bar → menus → dialogs → errors → internal. ~170 strings categorized
across 10 UI components.

*Original doc: `i18n-design.md`*

---

## 10. Per-Buffer Per-View State

**Problem**: When the same buffer is open in multiple splits, cursor positions
and view state were shared, causing confusing synchronized scrolling.

**Decision**: `BufferViewState` keyed by `BufferId`, stored per-split. Content
is shared (one `EditorState`), view state is independent (one
`BufferViewState` per split per buffer).

**Plugin state**: `HashMap<String, serde_json::Value>` allows plugins to store
arbitrary per-buffer-per-split state without Rust-side enum changes. Write-
through cache (`EditorStateSnapshot`) enables immediate read-back within the
same hook execution.

**Workspace persistence**: `file_states: HashMap<PathBuf, SerializedFileState>`
stores per-file state that survives session restarts.

*Original doc: `per-buffer-view-state-design.md` (implemented Feb 2026)*

---

## 11. EditorState Refactoring

**Problem**: `EditorState` had 18 fields with mixed concerns (decorations,
highlighting, mode flags), making it hard to reason about.

**Decision**: Extract into coherent sub-structs:
- `DecorationState` (6 fields): visual annotations sharing marker-list substrate
- `HighlightState` (6 fields): all derived from buffer language
- `BufferFlags` (3 fields, optional): user capability controls

**Execution order** chosen to maximize value-per-churn: `DecorationState`
first (clearest grouping, ~40 touch points), then `HighlightState` (~25),
skip `BufferFlags` (only 3 fields, marginal benefit).

**Status**: `ComposeState` extracted as proof-of-concept. Remaining extractions
identified but deferred.

*Original doc: `editor-state-refactoring.md`*

---

## 12. Configuration System (4-Layer Overlay)

**Problem**: Single config file doesn't support project-specific settings,
platform overrides, or volatile session state.

**Decision**: 4-level overlay hierarchy:
**System** (hardcoded defaults) → **User** (`~/.config/fresh/config.json`) →
**Project** (`.fresh/config.json`) → **Session** (volatile, in-memory)

**Merge strategy**:
- Scalars: highest-precedence layer wins
- Maps: recursive deep merge (enables per-language overrides like
  `languages.python.tab_size`)
- Lists: replace entirely (simpler than element-level merge)

**Delta serialization**: Only save differences from the parent layer. Setting
a value equal to the inherited value prunes the key, preventing config drift.

**Conditional layers**: Platform-specific (`config_linux.json`) and
language-specific overrides injected dynamically.

*Original docs: `config-design.md`, `config-implementation-plan.md`*

---

## 13. Plugin Architecture (Provider Pattern)

**Problem**: Plugins that "own the UI" (Controller pattern via virtual buffers)
must reimplement navigation, selection, and keybindings, leading to
inconsistent UX.

**Decision**: Standardize on the **Provider pattern** — plugins provide data,
the editor handles UI rendering.

**Two-tier API**:
- `QuickPick`: transient searches (Live Grep, Git Grep)
- `ResultsPanel`: persistent panels (Find References, Diagnostics) with
  bidirectional cursor sync via `syncWithEditor`

**Atomic actions** preferred over selection-based for operator+motion
combinations to avoid async timing issues.

**Event system**: `EventEmitter<T>` with typed events and `Disposable` cleanup.

*Original doc: `plugin-architecture-plan.md`*

---

## 14. Vi Mode

**Decision**: **Plugin-based with minimal core changes**. All modal editing
logic in TypeScript, core provides atomic actions.

**Trade-offs**:
- Atomic Rust actions for common operator+motion combos (delete word, yank to
  line end) — avoids async race conditions
- Selection-based fallback for complex motions
- `executeActions()` batch API with count support for efficient `3dw`

**Coverage**: Movement, count prefix, operators, text objects, visual modes,
colon command mode (30+ commands), repeat (`.`), find char (`f`/`t`/`F`/`T`).
Missing: registers and macros (low priority).

*Original doc: `vi-mode-design.md` (fully implemented, ~900 lines TypeScript)*

---

## 15. Markdown Compose Mode

**Decision**: Token pipeline integration — compose rendering uses view
transforms with conceal ranges and overlays at the token level.

**Key principles**:
- **Cursor-aware concealment**: Syntax markers shown when cursor is inside the
  span, hidden otherwise (Typora's "blur/focus" model)
- **Table grid rendering**: Pipes → box-drawing characters with cursor-aware
  per-row reveal
- **Visual line navigation**: Up/Down moves through wrapped display lines

**Known issue**: Race condition between async plugin hook execution and render
state — plugin transforms arrive 1 frame late, showing stale content briefly.
Proposed fixes: hold old content during scroll, atomic conceal swap for typing.

*Original docs: `markdown.md`, `markdown-compose-vs-glow.md`,
`typora-seamless-canvas-plan.md`*

---

## 16. Event Dispatch & Hit Testing

**Current architecture**: Layout cached in `render.rs` using ratatui's
constraint system. Some components use cached layout (tab bar, status bar);
others hardcode coordinates (menu bar).

**Planned evolution** (incremental):
1. **Immediate**: Add `menu_bar_row` to cached layout for consistency
2. **Medium-term**: Unified hit-test with `HitArea` and z-index for overlapping UI
3. **Future**: Compositor pattern (like Helix) for complex nested dialogs

**Key principle**: Retained-mode hit testing — rendering produces layout
objects (cached `Rect`s) consumed by input handling on the next frame.

*Original doc: `event-dispatch-architecture.md`*

---

## 17. Warning & Notification UX

**Problem**: Auto-opening warning log tabs was intrusive and disruptive.

**Decision**: Two-tier system:
1. Visual indicator (colored status bar badge) — always visible
2. Optional user-initiated popup with actionable solutions — on demand

**Architecture**: `WarningDomain` trait allows LSP, plugins, and config to
register custom warning handlers. Generic domain system decouples warning
sources from presentation.

**Plugin-based install helpers**: Language-specific LSP installation plugins
bundled (Python, Rust, TypeScript), user-extensible.

**UX principles**: Nielsen Norman heuristics — user control/freedom,
progressive disclosure.

*Original doc: `warning-notification-ux.md` (fully implemented)*

---

## 18. Terminal Architecture

**Decision**: Incremental scrollback streaming with append-only backing file.

**Dual mode**: Terminal mode (live PTY) and Scrollback mode (read-only buffer
view with editor navigation).

**Performance**:
- Mode switch: ~5ms (was ~500ms with full replay)
- Session restore: ~10ms via lazy load (was ~1000ms)
- PTY overhead: ~0.1ms per scroll

**Session persistence**: Backing file contains complete scrollback + visible
screen snapshot. On restore, load as read-only buffer immediately; replay only
if user re-enters terminal mode (deferred).

*Original doc: `terminal.md`*

---

## 19. Theme System

**Key decisions**:
- Quick selection via command palette, interactive Theme Editor for fine-tuning
- JSON theme files with RGB arrays, embedded built-in themes
- Override built-in themes by naming a local theme identically

**Planned consolidation** (not yet shipped): Move hardcoded Rust themes to
embedded JSON files (`include_str!`), validate at CI time via deserialization
test, expose `getBuiltinThemes()` API for plugins.

**Usability issues identified** (from testing):
- Theme Editor starts empty (can't edit existing themes directly)
- No unsaved-changes confirmation on quit
- Navigation inconsistency (arrows navigate all lines, Enter only works on
  field lines)

*Original docs: `theme-consolidation-plan.md`, `theme-user-flows.md`,
`theme-usability-improvements.md`*

---

## 20. Visual Layout & Width Calculations

**Problem**: Inconsistent width calculations across rendering, navigation,
mouse hit testing, and status bar — each reimplements character width logic
differently, especially for ANSI escapes, tabs, and zero-width characters.

**Decision**: Unified `visual_layout.rs` module with `LineMappings` struct
providing per-character and per-visual-column indexing.

**Design principle**: O(1) rendering and hit testing (via pre-computed
mappings), O(n) navigation (walk characters per line).

**Current fragmentation**: Rendering uses `ViewLine.char_mappings`, mouse
clicks reuse that mapping, but MoveUp/Down uses `str_width()` on raw buffer
(doesn't understand ANSI, tabs).

*Original doc: `visual-layout-unification.md`*

---

## Cross-Cutting Principles

These principles emerge repeatedly across the designs above:

1. **Provider over Controller**: Plugins provide data; the editor owns UI
   rendering and navigation.
2. **Selective abstraction**: Abstract what's testable, leave real
   implementations where they fundamentally must be.
3. **Single source of truth**: Avoid derived state that can desync (scroll sync
   markers, config layer resolution, cursor ownership).
4. **Atomic operations over sequential**: BulkEdit, atomic actions for vi mode,
   single-undo-step paste — convert N operations into 1.
5. **Progressive disclosure**: Simple defaults, explicit power-user paths
   (CLI subcommands, config layers, prefix-based modes).
6. **Graceful degradation**: Bracketed paste → burst heuristic, git ls-files →
   fd → find, LSP folding → indent-based folding.
7. **Ultra-light boundaries**: Session client is a dumb pipe, plugin thread
   communicates via commands, config layers are pure data.
