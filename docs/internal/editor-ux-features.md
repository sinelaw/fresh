# Editor UX Features (Catch-All)

Records the design decisions, trade-offs, and shipped-vs-planned status of editor
UX features that don't warrant their own dedicated doc. Each section is
self-contained; all `path:line` references are to the monorepo layout (plugins
live under `crates/fresh-editor/plugins/`, not a top-level `plugins/`).

> Status legend: **SHIPPED** = in code and reachable; **PARTIAL** = core works,
> gaps remain; **PLANNED/DEAD** = designed but not built or vestigial.

---

## 1. Markdown Compose Mode — SHIPPED

A Typora-style **inline "preview-while-editing"** mode for markdown buffers. It
is an in-place transform over the *editable source* (conceals + style overlays +
soft-break markers + virtual border lines), **not** a separate rendered pane.
There is no glow-style preview window — the "Compose/Preview" command names
describe one inline mode.

**Two files share the name "markdown" but only one is the feature:**

- `crates/fresh-editor/src/view/markdown.rs` (1427 lines, ~770 of them tests) is
  a standalone `pulldown_cmark` parser for **popup surfaces** (LSP hover,
  signature help) — `parse_markdown()` at `markdown.rs:448`, word-wrap at
  `markdown.rs:37`/`:102`. It is largely independent of compose mode; recent
  history is theme-contrast fixes for popups (issue #2033).
- `crates/fresh-editor/plugins/markdown_compose.ts` (1854 lines) **is** the
  feature. A sibling `markdown_source.ts` (357 lines) handles smart list editing
  (Enter continues bullets, Tab cycles `*`→`-`→`+`) in source mode.

**Core integration.** `ViewMode { Source, PageView }` lives at
`crates/fresh-editor/src/state.rs:73` (the doc-comment notes PageView "was
previously called compose mode"). `handle_set_view_mode`
(`app/plugin_commands.rs:1555`) maps both `"compose"` and `"page_view"` →
`PageView` on the active split's per-buffer view state, so "toggle all" can
target inactive buffers. `BufferInfo` exposes `view_mode`,
`is_composing_in_any_split`, and `compose_width` (`fresh-core/src/api.rs:464`) —
the per-split flag exists because conceals are buffer-level but filtered per
split.

**Toggle paths (no default keybinding).** `Action::TogglePageView`
(`input/keybindings.rs:573`, aliased `toggle_compose_mode` at `:1303`) →
`handle_toggle_page_view` (`app/input.rs:1543`); command palette entries
"Markdown: Toggle Compose/Preview" and "(All Files)"; menu checkbox state via
`menu_context.rs:188`. State persists across sessions (global flag
`globalComposeEnabled`, per-buffer restore on `buffer_activated`,
`markdown_compose.ts:1801`).

### Vision vs. what shipped

The seamless-canvas plan (`typora-seamless-canvas-plan.md`) is marked **"Phase 2
COMPLETE"**; the later UX eval (`MARKDOWN_COMPOSE_UX_EVAL.md`, 2026-04-13
post-fix pass) found **no remaining major-severity issues**. Confirmed shipped:
cursor-aware blur/focus concealment for bold/italic/bold-italic/code/strikethrough
(`processLineConceals`, `markdown_compose.ts:1054`), link concealment,
box-drawing table rendering with per-row cursor reveal and accumulate-and-grow
column alignment (`processTableAlignment`, `:1503`), virtual table borders
(`processTableBorders`, `:151`), centered page width, and visual-line navigation
through wrapped lines.

**The load-bearing design decision (from the async-hook lag class of bugs):**
the feature originally used the async `view_transform_request` token-rewrite
pipeline, which caused render flicker on every scroll/edit because the plugin's
transformed tokens arrived **one frame late** (root-cause analysis in
`typora-seamless-canvas-plan.md:51-235`). It was rebuilt around **synchronous
marker-based conceals + soft-breaks computed in the `lines_changed` hook**
(`processLineSoftBreaks`, `:1348`; rationale comment `:1664-1667`), eliminating
the round-trip. Two supporting techniques documented in design-decisions #13:
proactive `refreshLines()` in Rust before the async `cursor_moved` hook, and
atomic clear+rebuild batching within a single `process_commands()` drain.

**Not shipped (plan items still open):** OSC-8 clickable links (links are styled
but no terminal hyperlink escape), header `#` concealment (headings keep their
markers), and task-list/code-fence concealment into `☐`/`☑` (parsed but not
concealed). The `markdown.md` doc's "remaining work" list (multi-pass
transforms, column-guide rendering) is also still pending.

---

## 2. Code Tour — SHIPPED (relocated from the design)

A JSON-driven walkthrough that guides users through a codebase via overlays and a
virtual dock buffer. **Built and functional**, but it landed differently from
`code-tour-design.md`, which proposed a multi-file `plugins/code-tour/` package
(`index.ts`, `tour-manager.ts`, `tour-dock.ts`, `types.ts`).

**What actually exists:**

- `crates/fresh-editor/plugins/code-tour.ts` (401 lines) — a **single-file**
  plugin: `TourManager` (`:48`), namespace `"code-tour"` (`:64`), and four
  palette commands `tour_load` / `tour_next` / `tour_prev` / `tour_exit`
  (`:356-382`, underscores not the doc's `tour:next` colon form). Navigation also
  comes from `action_popup_result` buttons (`:385-399`).
- `crates/fresh-editor/plugins/schemas/tour.schema.json` — the validation schema
  (renamed from the proposed `tour-schema.json`).
- `.fresh-tour.json` (repo root) — a working 4-step sample touring Fresh's
  own plugin system (QuickJS backend → `PluginCommand` enum → `fresh.d.ts` →
  `git_find_file.ts`). Note its `$schema` points at the new schema path; it
  omits the optional `commit_hash` field.

**The 4 proposed plugin-API additions all exist** (3 added end-to-end, 1 was a
pre-existing field newly exposed):

| API | Enum / field | QJS method | Dispatch + handler |
|-----|-------------|-----------|--------------------|
| `scrollToLineCenter` | `api.rs:3447` | `quickjs_backend.rs:1749` | `plugin_dispatch.rs:1227` / handler `:2476` |
| `getLineEndPosition` | `api.rs:3380` | `quickjs_backend.rs:1697` | `plugin_dispatch.rs:1192` / `:2269` |
| `getBufferLineCount` | `api.rs:3390` | `quickjs_backend.rs:1716` | `plugin_dispatch.rs:1199` / `:2274` |
| `extendToLineEnd` | `OverlayOptions` `api.rs:705` | parsed `quickjs_backend.rs:3455` | applied `plugin_commands.rs:73` |

**Deviation from doc:** the three async handlers landed in `plugin_dispatch.rs`,
not `plugin_commands.rs` as the design's Phase 1 checklist specified. Git arc:
`5ceda02bb` (API support) then `d5cb129ad` (plugin + schema + sample), both
2026-02-03. The design-doc claim of "~85% of API already present" held up — only
small, established-pattern additions were needed.

---

## 3. Input Calibration Wizard — SHIPPED

A fail-safe wizard for **hostile terminals** (web SSH, tmux-in-ssh-in-screen, VM
consoles) where special keys (Backspace, Home/End, Ctrl/Alt+Arrow) arrive mangled
because the Kitty keyboard protocol is unavailable. Fully wired:
action → state machine → translator → JSON persistence → input pipeline, with 18
unit tests.

**Files:**

- `app/calibration_wizard.rs` (993 lines) — the state machine. `CalibrationWizard`
  (`:261`); `calibration_groups()` (`:53`) defines **24 keys across 5 groups**
  (Basic Editing, Line/Word/Document Navigation, Emacs-Style); two-phase
  `CalibrationStep::{Capture, Verify}` (`:198`); `build_translator` (`:690`).
- `app/calibration_actions.rs` (93 lines) — `Editor` glue: `open_calibration_wizard`
  (`:13`), `save_calibration` (`:19`, persists then hot-swaps the live
  translator), `handle_calibration_input` (`:39`).
- `view/calibration_wizard.rs` (399 lines) — the ratatui modal.

**Key design decision — translate, don't override.** The calibration sits in a
`KeyTranslator` layer *before* keybinding resolution (raw → normalized key → keymap
→ action), so emacs/vscode keymap customization still composes on top
(`input-calibration-wizard.md:69-91`). Overriding raw-key→action directly was
rejected because it bypasses the keymap. The wizard UI deliberately uses **only
lowercase ASCII** controls (`s`kip, `b`ack, `g`roup-skip, `a`bort, `y`save,
`r`estart) because Enter/Esc/Ctrl may themselves be broken — and verify-before-save
plus always-available abort are explicit goals.

**Persistence.** `KeyTranslator` (`input/key_translator.rs`) writes
`<config_dir>/key_calibration.json` (`calibration_path` `:322`) — a
`translations` array of `{raw, expected}` pairs. Loaded at startup
(`main.rs:1775`); a missing file yields an empty (no-op) translator. Live
translation is applied in the event loop at `app/lifecycle.rs:285` before
`handle_key`.

**Launch.** `Action::CalibrateInput` (`fresh-core/src/action.rs:415`,
`"calibrate_input"`) via command palette "Calibrate Keyboard" (`commands.rs:1373`)
or dispatch at `input.rs:2454`. Input is routed through overlay layer
`LayerKind::CalibrationWizard`. The doc's optional `fresh --calibrate` CLI flag
was **not** implemented.

---

## 4. Vi Mode — SHIPPED (plugin-based)

Confirms design-decisions #14: all modal logic in TypeScript, minimal mode-agnostic
core. `crates/fresh-editor/plugins/vi_mode.ts` is **4191 lines** (142 handler
registrations); the core has no vi-specific symbols — the plugin drives the global
editor mode via `editor.setEditorMode("vi-normal"|…)` backed by
`fresh-core/src/api.rs:3468`.

**Core support is generic, not vi-shaped.** `input/buffer_mode.rs` (167 lines) is
a per-buffer mode *metadata* registry (read-only flag, text passthrough, binding
inheritance) — **not** the vi state machine. The "atomic actions" decision is real:
the batch `executeActions([{action, count}])` API (`vi_mode.ts:256`;
`plugin_dispatch.rs:2105`) plus operator-specific atomic actions
(`DeleteToLineStart` for `d0`, `YankToLineEnd`, etc.) were added in commit
`ce320fd52`, avoiding async race conditions for operator+motion combos.

**Coverage verified:** movement, count prefix, operators (`d`/`c`/`y`), text
objects (`vi_mode.ts:2343-2475`), visual/visual-line/visual-block, find-char
`f`/`t`/`F`/`T` with `;`/`,` repeat, and repeat `.` (`LastChange` capture
`:184-207`). The colon-command table (`:3301`) has **59** entries — well past the
doc's "30+". Enabled via plugin config `autoStart` (`:77`) or the
`%cmd.toggle_vi_mode` command (`:4150`).

**"Missing registers and macros" — accurate for the vi plugin** (zero `macro`
occurrences; "register" hits are all `registerHandler`/clipboard). Worth flagging:
a **separate, native** register-keyed macro system exists at `app/macros.rs`
(`MacroState`) / `macro_actions.rs` — it records actions and codegens an
`executeActions` block — but it is independent of and not wired into the vi plugin,
so the doc's claim is correct in scope.

---

## 5. Internationalization (i18n) — SHIPPED, with a doc discrepancy

UI strings are externalized to JSON locales across **14 languages**
(`crates/fresh-editor/locales/*.json`: cs, de, en, es, fr, it, ja, ko, pt-BR, ru,
th, uk, vi, zh-CN). The crate **is** `rust-i18n` (`Cargo.toml:50`), and locale JSON
**is** embedded with `include_str!`.

**Plugin strings are localized too,** via a separate mechanism: ~40 `*.i18n.json`
files sit next to plugins, loaded through `register_plugin_strings` /
`translate_plugin_string` (`i18n.rs:32-86`, `RwLock<HashMap>` with `%{var}`
interpolation) — independent of the rust-i18n backend.

**Locale selection precedence** (CLI > config > env): `main.rs:1716-1719` —
`--locale` flag, then `config.locale`, then `detect_locale()` (`i18n.rs:142`:
`LC_ALL`→`LC_MESSAGES`→`LANG`, region-aware e.g. `pt_BR`→`pt-BR`, else `"en"`).

> **DISCREPANCY — design-decisions #9 says "compile-time embedding, zero runtime
> overhead." That is inaccurate.** Fresh overrides rust-i18n's default backend with
> a custom `RuntimeBackend` (`lib.rs:9`; `i18n/runtime_backend.rs`). JSON *bytes*
> are embedded via `include_str!`, but they are **parsed at runtime** —
> `RuntimeBackend::translate` (`:145`) lazily `serde_json::from_str` + flattens +
> `Box::leak`s on first use per locale (`:106-129`). The module's own doc-comment
> states the intent: *"replaces the compile-time macro expansion with runtime JSON
> parsing, significantly reducing compiler memory usage."* So the real trade was
> **lower compile-time memory at the cost of a one-time runtime parse per locale** —
> not "zero runtime overhead." The header comment in `i18n.rs:1-5` still repeats the
> old framing and should be corrected. The rejected alternatives (Project Fluent,
> gettext-rs) leave no traces, consistent with never being adopted.

---

## 6. Menu Bar, Command Palette, Help, Bookmarks — all SHIPPED

### 6.1 Menu bar
Model in `fresh-core/src/menu.rs` (89 lines): `Menu` (`:75`) with locale-independent
`id` (keybinding match) split from translatable `label` (display); `MenuItem`
(`:47`) is an untagged enum — `Separator`/`Action {checkbox}`/`Submenu`/
`DynamicSubmenu {source}` (e.g. `"themes"`)/`Label`. `MenuContext` (`:14`) is just a
`HashMap<String,bool>`; both `when` (enable) and `checkbox` resolve against it.
`update_menu_context()` (`menu_context.rs:67`) recomputes ~30 booleans each frame.
Clicks hit-test a cached `menu_layout` from the prior render frame
(`menu_actions.rs:208`); unknown action names fall back to `Action::PluginAction`
(`:154`). One expansion path `all_menus_expanded()` is shared by the TUI renderer
and the web `menu_view()` so frontends can't diverge.

### 6.2 Command palette
**Not a separate overlay** — it is a *mode of the unified Quick Open picker*,
confirming design-decisions #4. `Action::CommandPalette` is explicitly an alias:
`// kept for keymap/plugin compatibility` (`keybindings.rs:546`); it delegates to
`start_quick_open()` seeded with the `">"` prefix (`prompt_lifecycle.rs:138`). One
`Prompt` with `PromptType::QuickOpen`; the leading char routes the mode (`>`
commands, `#` buffers, `:` go-to-line, none = files). `CommandRegistry`
(`command_registry.rs`) holds built-ins (`commands.rs`, 1604 lines) plus
thread-safe plugin commands and a 50-entry recency history; fuzzy match via
`input::fuzzy`. Localized command names use a `%`-prefix convention
(`commands.rs:47`).

### 6.3 Help overlay
**Not a floating overlay** — two read-only virtual buffers. `app/help.rs` is 13
lines of constants: `*Fresh Manual*` content is `include_str!("../../docs/fresh.txt")`
(compile-time bundled). Orchestrators in `help_actions.rs` (161 lines):
`open_help_manual` (`:64`) inserts the static manual; `open_keyboard_shortcuts`
(`:99`) is **dynamically generated from the live keymap**
(`resources.keybindings…get_all_bindings()`, `:111-143`) so it reflects the user's
actual bindings. Both run in a `"special"` BufferMode that binds `q → CloseTab` and
blocks edits. Triggered by `Action::ShowHelp` / `ShowKeyboardShortcuts`
(`input.rs:1288`). (A richer interactive keybinding *editor* exists separately at
`view/keybinding_editor.rs`.)

### 6.4 Bookmarks
**Named single-char register marks, position-based (not marker-tracked).**
`Bookmark { buffer_id, position: usize }` (`app/bookmarks.rs:14`) keyed in
`HashMap<char, Bookmark>` (`:21`); jumps clamp to `position.min(buffer.len())`
(`bookmark_actions.rs:107`) since a raw byte offset can drift after edits. Actions:
`SetBookmark(char)`, `JumpToBookmark(char)`, `ClearBookmark`, `ListBookmarks`, plus
interactive `PromptSet/JumpToBookmark` (`keybindings.rs:498-529`).
`jump_to_bookmark` stays on `Editor` (not `Window`) because it fires plugin hooks;
it switches buffers, force-recenters (`ensure_active_cursor_visible_for_navigation(true)`,
fixing #1689), and forgets a bookmark whose buffer is gone (`bookmark_actions.rs:96`).
**Persisted across sessions** by file path, not BufferId: `SerializedBookmark
{ file_path, position }` (`workspace.rs:419`), re-resolved on load via
`restore_bookmarks_from_workspace` (`:677`), dropping bookmarks whose files aren't
reopened. State is mid-migration from `Editor` to per-`Window`.

---

## 7. Warning / Notification UX and Status Log — PARTIAL (tier 2 partly vestigial)

Implements design-decisions #17's core decision faithfully: **no auto-opening
warning tabs**; a tier-1 always-visible status-bar badge plus a tier-2 on-demand
view. But the generic-trait popup machinery is largely dead code, and the live
actionable UX is routed through LSP instead.

**Files:** `services/warning_log.rs` (194), `services/status_log.rs` (90),
`app/warning_domains.rs` (381).

**`WarningDomain` trait** (`warning_domains.rs:54`): `id`/`label`/`level`/
`popup_content`/`has_warnings`. Only **two built-in implementors** —
`GeneralWarningDomain` (`:113`, from tracing WARN/ERROR) and `LspWarningDomain`
(`:223`, from LSP statuses). The registry (`:338`) holds two **concrete fields**,
not `Vec<Box<dyn WarningDomain>>`, and exposes **no register/add method**.

> **DISCREPANCY #1 — extensibility overstated.** The doc says "LSP, plugins, and
> config register custom warning handlers." There is no runtime registration API;
> the trait is object-safe but never used polymorphically. Plugins influence
> warnings only indirectly through the LSP-status hook (below), not the trait.

**Tier 1 — badge (SHIPPED).** Rendered in `render.rs:1634-1667`, gated on
`config.warnings.show_status_indicator`. Two inputs: a general-warning **count**
badge `[⚠ N]` (`GeneralWarningDomain::label()` `:118`), and LSP severity applied as
a **colored background** on the existing LSP status segment (`LspWarningDomain::label()`
returns empty by design, `:231`). The count is driven by `check_warning_log()`
(`editor_accessors.rs:1260`) draining an `mpsc` channel each tick.

**Tier 2 — on-demand "actionable solutions" (PARTIAL).**

> **DISCREPANCY #2.** The trait's `popup_content()` with `WarningAction` install
> commands (`:130-157`, `:238-290`) **has no callers** — it is vestigial. The
> general-warning click path (`Action::ShowWarnings` → `show_warnings_popup`,
> `popup_dialogs.rs:137`) does **not** show a popup; it opens the log file
> read-only (`open_warning_log`). The real actionable popup is the **separate
> LSP-status popup** (`show_lsp_status_popup`, `popup_dialogs.rs:153`): clicking the
> LSP segment fires the `lsp_status_clicked` hook, and **plugins inject fix-it rows**
> via `setLspMenuContributions` (e.g. "Copy: rustup component add rust-analyzer",
> "Disable Rust LSP"). So the two-tier actionable-popup vision is realized for **LSP
> warnings only**, not general warnings.

**status_log vs warning_log.** `warning_log` is a tracing `Layer` capturing
WARN+ERROR to a file with 5s/100-entry dedup (`DeduplicationState` `:20`) and a
notification channel — it drives the badge. `status_log` is a tracing `Layer`
capturing only `target = "status"` events (`status_log.rs:53`) — **no dedup, no
channel, no badge**; it is purely the transient status-message history
("notification log"), opened via `Action::ShowStatusLog` (`editor_accessors.rs:1239`)
or by clicking the status-message segment.

**LSP install-helper plugins (SHIPPED, broader than documented).** Doc names
"(Python, Rust, TypeScript)"; in reality **49** `*-lsp.ts` plugins ship
(`python-lsp.ts` pipx/pip, `rust-lsp.ts` rustup/brew, `typescript-lsp.ts`, plus go,
clangd, bash, java, ruby, php, zig, …), each contributing copy-install + disable
actions to the LSP popup. **DISCREPANCY #3 — doc undersells breadth.**

---

## Cross-Cutting Observations

- **Provider over Controller, again.** Code tour, vi mode, and markdown compose are
  all plugins that *provide data/commands* while the editor owns rendering and
  navigation. Where a feature needed core support it was added as a generic,
  mode-agnostic primitive (conceals, `executeActions`, `setEditorMode`,
  `setLspMenuContributions`), never a feature-specific one.
- **Async-hook frame lag is the recurring villain.** It forced the markdown-compose
  rewrite from view-transforms to synchronous markers and shaped the vi-mode atomic
  action API. Any new content-transforming plugin should prefer synchronous
  marker/overlay state over `view_transform_request`.
- **Doc drift to fix in design-decisions.md:** #9 i18n ("zero runtime overhead" is
  wrong — it's runtime JSON parsing) and #17 warnings (overstated trait
  extensibility; the generic `popup_content` path is dead; install helpers number
  ~49 not 3).
