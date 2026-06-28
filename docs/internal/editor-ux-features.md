# Editor UX Features (Catch-All)

> _AI-generated: describes Fresh's architecture and design rationale, not implementation details; where it disagrees with the source, the source is authoritative._

Records the design decisions, trade-offs, and shipped-vs-planned status of editor
UX features that don't warrant their own dedicated doc. Each section is
self-contained.

> Status legend: **SHIPPED** = in code and reachable; **PARTIAL** = core works,
> gaps remain; **PLANNED/DEAD** = designed but not built or vestigial.

---

## 1. Markdown Compose Mode — SHIPPED

A Typora-style **inline "preview-while-editing"** mode for markdown buffers. It
is an in-place transform over the *editable source* (conceals + style overlays +
soft-break markers + virtual border lines), **not** a separate rendered pane.
There is no separate preview window — the "Compose/Preview" command names
describe one inline mode.

**Two components share the name "markdown" but only one is the feature:**

- A standalone `pulldown_cmark` parser provides markdown rendering for **popup
  surfaces** (LSP hover, signature help), including word-wrap. It is largely
  independent of compose mode; recent work has been theme-contrast fixes for
  popups.
- The markdown compose plugin **is** the feature. A sibling source-mode plugin
  handles smart list editing (Enter continues bullets, Tab cycles bullet
  markers) in source mode.

**Core integration.** A buffer carries a `ViewMode` of either source or page
view (the latter was previously called compose mode). The view-mode handler maps
both `"compose"` and `"page_view"` to page view on the active split's per-buffer
view state, so "toggle all" can target inactive buffers. Buffer info exposes the
view mode, whether the buffer is composing in any split, and a compose width —
the per-split flag exists because conceals are buffer-level but filtered per
split.

**Toggle paths (no default keybinding).** A toggle-page-view action (with a
compose-mode alias) routes to its handler; command palette entries toggle compose
for the active file and for all files; menu checkbox state reflects the mode.
State persists across sessions via a global compose flag with per-buffer restore
on buffer activation.

### Vision vs. what shipped

The seamless-canvas plan is marked "Phase 2 complete"; a later UX evaluation pass
found no remaining major-severity issues. Shipped: cursor-aware blur/focus
concealment for bold/italic/bold-italic/code/strikethrough, link concealment,
box-drawing table rendering with per-row cursor reveal and accumulate-and-grow
column alignment, virtual table borders, centered page width, and visual-line
navigation through wrapped lines.

**The load-bearing design decision (from the async-hook lag class of bugs):**
the feature originally used the async view-transform token-rewrite pipeline,
which caused render flicker on every scroll/edit because the plugin's transformed
tokens arrived **one frame late**. It was rebuilt around **synchronous
marker-based conceals and soft-breaks computed in the lines-changed hook**,
eliminating the round-trip. Two supporting techniques: a proactive line refresh in
Rust before the async cursor-moved hook, and atomic clear-and-rebuild batching
within a single command-drain pass.

**Not shipped (plan items still open):** OSC-8 clickable links (links are styled
but no terminal hyperlink escape), header marker concealment (headings keep their
markers), and task-list/code-fence concealment into checkbox glyphs (parsed but
not concealed). The remaining-work list (multi-pass transforms, column-guide
rendering) is also still pending.

---

## 2. Code Tour — SHIPPED (relocated from the design)

A JSON-driven walkthrough that guides users through a codebase via overlays and a
virtual dock buffer. **Built and functional**, but it landed differently from the
design, which proposed a multi-file package.

**What actually exists:**

- A **single-file** code-tour plugin: a tour manager, its own namespace, and four
  palette commands to load, advance, reverse, and exit a tour (underscore-named,
  not the design's colon form). Navigation also comes from action-popup result
  buttons.
- A validation schema for tour files (renamed from the proposed schema name).
- A working sample tour at the repo root, touring Fresh's own plugin system. Its
  `$schema` points at the new schema path; it omits the optional commit-hash
  field.

**The proposed plugin-API additions all exist** — most added end-to-end, one a
pre-existing field newly exposed: scroll-to-line-center, get-line-end-position,
get-buffer-line-count, and an overlay option to extend an overlay to line end.
Each spans an enum or field, a QuickJS method, and dispatch plus handler.

**Deviation from doc:** the async handlers landed in the dispatch layer, not the
command layer as the design's checklist specified. The design's estimate that most
of the API was already present held up — only small, established-pattern additions
were needed.

---

## 3. Input Calibration Wizard — SHIPPED

A fail-safe wizard for **hostile terminals** (web SSH, tmux-in-ssh-in-screen, VM
consoles) where special keys (Backspace, Home/End, Ctrl/Alt+Arrow) arrive mangled
because the Kitty keyboard protocol is unavailable. Fully wired:
action → state machine → translator → JSON persistence → input pipeline, with unit
tests.

**Components:**

- The state machine, which defines several keys across multiple groups (Basic
  Editing, Line/Word/Document Navigation, Emacs-Style) and a two-phase
  capture-then-verify step, and builds a translator.
- The editor glue: opening the wizard, saving calibration (persists then hot-swaps
  the live translator), and handling calibration input.
- The ratatui modal.

**Key design decision — translate, don't override.** The calibration sits in a
key-translator layer *before* keybinding resolution (raw → normalized key → keymap
→ action), so emacs/vscode keymap customization still composes on top. Overriding
raw-key→action directly was rejected because it bypasses the keymap. The wizard UI
deliberately uses **only lowercase ASCII** controls (skip, back, group-skip, abort,
save, restart) because Enter/Esc/Ctrl may themselves be broken — and
verify-before-save plus always-available abort are explicit goals.

**Persistence.** The key translator writes a calibration JSON file under the config
directory — an array of raw/expected pairs. It is loaded at startup; a missing file
yields an empty (no-op) translator. Live translation is applied in the event loop
before key handling.

**Launch.** A calibrate-input action reaches the wizard via a command-palette entry
or direct dispatch; input is routed through a dedicated overlay layer. The
optional `--calibrate` CLI flag was **not** implemented.

---

## 4. Vi Mode — SHIPPED (plugin-based)

All modal logic lives in TypeScript with a minimal, mode-agnostic core. The vi-mode
plugin is large (many handler registrations); the core has no vi-specific symbols —
the plugin drives the global editor mode via a set-editor-mode API.

**Core support is generic, not vi-shaped.** A per-buffer mode registry holds mode
*metadata* (read-only flag, text passthrough, binding inheritance) — **not** the vi
state machine. The "atomic actions" decision is real: a batch execute-actions API
plus operator-specific atomic actions (delete-to-line-start for `d0`,
yank-to-line-end, etc.) avoid async race conditions for operator+motion combos.

**Coverage verified:** movement, count prefix, operators (`d`/`c`/`y`), text
objects, visual/visual-line/visual-block, find-char `f`/`t`/`F`/`T` with `;`/`,`
repeat, and repeat `.` via last-change capture. The colon-command table has many
entries. Enabled via plugin config auto-start or a toggle command.

**Registers and macros are absent from the vi plugin.** Worth flagging: a
**separate, native** register-keyed macro system exists — it records actions and
codegens an execute-actions block — but it is independent of and not wired into the
vi plugin, so the "missing registers and macros" claim is correct in scope.

---

## 5. Internationalization (i18n) — SHIPPED, with a doc discrepancy

UI strings are externalized to JSON locales across many languages. The crate is
`rust-i18n`, and locale JSON bytes are embedded at compile time.

**Plugin strings are localized too,** via a separate mechanism: per-plugin i18n
JSON files sit next to plugins, loaded through register/translate plugin-string
helpers (a lock-guarded map with variable interpolation) — independent of the
rust-i18n backend.

**Locale selection precedence** (CLI > config > env): a `--locale` flag, then the
configured locale, then locale detection from environment variables
(`LC_ALL`→`LC_MESSAGES`→`LANG`, region-aware, e.g. `pt_BR`→`pt-BR`, else `"en"`).

> **DISCREPANCY — the design's i18n note says "compile-time embedding, zero runtime
> overhead." That is inaccurate.** Fresh overrides rust-i18n's default backend with
> a custom runtime backend. JSON *bytes* are embedded at compile time, but they are
> **parsed at runtime** — the backend lazily parses, flattens, and leaks the JSON on
> first use per locale. The module's own doc-comment states the intent: it replaces
> the compile-time macro expansion with runtime JSON parsing to significantly reduce
> compiler memory usage. So the real trade was **lower compile-time memory at the
> cost of a one-time runtime parse per locale** — not "zero runtime overhead." The
> stale header comment still repeats the old framing and should be corrected.

---

## 6. Menu Bar, Command Palette, Help, Bookmarks — all SHIPPED

### 6.1 Menu bar
The menu model carries a locale-independent `id` (keybinding match) split from the
translatable `label` (display); a menu item is an untagged enum —
separator / action (with optional checkbox) / submenu / dynamic submenu (sourced,
e.g. themes) / label. The menu context is a string-to-bool map; both `when`
(enable) and `checkbox` resolve against it, and it is recomputed each frame. Clicks
hit-test a cached menu layout from the prior render frame; unknown action names
fall back to a generic plugin-action. One expansion path is shared by the TUI
renderer and the web menu view so frontends can't diverge.

### 6.2 Command palette
**Not a separate overlay** — it is a *mode of the unified Quick Open picker*. The
command-palette action is explicitly an alias kept for keymap/plugin
compatibility; it delegates to Quick Open seeded with the `">"` prefix. There is
one prompt of the Quick Open type; the leading char routes the mode (`>` commands,
`#` buffers, `:` go-to-line, none = files). The command registry holds built-ins
plus thread-safe plugin commands and a recency history; matching is fuzzy.
Localized command names use a `%`-prefix convention.

### 6.3 Help overlay
**Not a floating overlay** — two read-only virtual buffers. The manual content is
bundled at compile time. The help orchestrators insert the static manual, while the
keyboard-shortcuts buffer is **dynamically generated from the live keymap** so it
reflects the user's actual bindings. Both run in a special buffer mode that binds
`q` to close and blocks edits, triggered by show-help and show-keyboard-shortcuts
actions. (A richer interactive keybinding *editor* exists separately.)

### 6.4 Bookmarks
**Named single-char register marks, position-based (not marker-tracked).** A
bookmark holds a buffer id and a byte position, keyed in a char-to-bookmark map;
jumps clamp the position to the buffer length since a raw byte offset can drift
after edits. Actions cover set, jump, clear, and list, plus interactive
prompt-set/jump. The jump operation stays on the editor (not the window) because it
fires plugin hooks; it switches buffers, force-recenters, and forgets a bookmark
whose buffer is gone. **Persisted across sessions** by file path, not buffer id, and
re-resolved on load, dropping bookmarks whose files aren't reopened. State is
mid-migration from editor-level to per-window.

---

## 7. Warning / Notification UX and Status Log — PARTIAL (tier 2 partly vestigial)

Implements the core warning-UX decision faithfully: **no auto-opening warning
tabs**; a tier-1 always-visible status-bar badge plus a tier-2 on-demand view. But
the generic-trait popup machinery is largely dead code, and the live actionable UX
is routed through LSP instead.

**A warning-domain trait** exposes id, label, level, popup content, and a
has-warnings flag. There are only **two built-in implementors** — a general domain
(from tracing WARN/ERROR) and an LSP domain (from LSP statuses). The registry holds
two **concrete fields**, not a vector of trait objects, and exposes **no
register/add method**.

> **DISCREPANCY #1 — extensibility overstated.** The doc says "LSP, plugins, and
> config register custom warning handlers." There is no runtime registration API;
> the trait is object-safe but never used polymorphically. Plugins influence
> warnings only indirectly through the LSP-status hook (below), not the trait. The
> warning-domain set is fixed, not an open extensibility API.

**Tier 1 — badge (SHIPPED).** Gated on a config flag. Two inputs: a general-warning
**count** badge, and LSP severity applied as a **colored background** on the
existing LSP status segment (the LSP domain's label returns empty by design). The
count is driven by draining a notification channel each tick.

**Tier 2 — on-demand "actionable solutions" (PARTIAL).**

> **DISCREPANCY #2.** The trait's popup-content path, with warning-action install
> commands, **has no callers** — it is vestigial. The general-warning click path
> does **not** show a popup; it opens the log file read-only. The real actionable
> popup is the **separate LSP-status popup**: clicking the LSP segment fires an
> LSP-status-clicked hook, and **plugins inject fix-it rows** via an
> LSP-menu-contributions API (e.g. "Copy: rustup component add rust-analyzer",
> "Disable Rust LSP"). So the two-tier actionable-popup vision is realized for **LSP
> warnings only**, not general warnings.

**status_log vs warning_log.** The warning log is a tracing layer capturing
WARN+ERROR to a file with time/count dedup and a notification channel — it drives
the badge. The status log is a tracing layer capturing only status-targeted events —
**no dedup, no channel, no badge**; it is purely the transient status-message
history ("notification log"), opened via a show-status-log action or by clicking the
status-message segment.

**LSP install-helper plugins (SHIPPED, broader than documented).** The doc names
only Python, Rust, and TypeScript; in reality **many** per-language LSP plugins ship
(Python, Rust, TypeScript, Go, clangd, Bash, Java, Ruby, PHP, Zig, and more), each
contributing copy-install and disable actions to the LSP popup. **DISCREPANCY #3 —
doc undersells breadth.**

---

## Cross-Cutting Observations

- **Provider over Controller, again.** Code tour, vi mode, and markdown compose are
  all plugins that *provide data/commands* while the editor owns rendering and
  navigation. Where a feature needed core support it was added as a generic,
  mode-agnostic primitive (conceals, batch execute-actions, set-editor-mode,
  LSP-menu-contributions), never a feature-specific one.
- **Async-hook frame lag is the recurring villain.** It forced the markdown-compose
  rewrite from view-transforms to synchronous markers and shaped the vi-mode atomic
  action API. Any new content-transforming plugin should prefer synchronous
  marker/overlay state over the async view-transform path.
- **Doc drift to fix in the design-decisions doc:** the i18n note ("zero runtime
  overhead" is wrong — it's runtime JSON parsing) and the warnings note (overstated
  trait extensibility; the generic popup-content path is dead; install helpers number
  many, not three).
