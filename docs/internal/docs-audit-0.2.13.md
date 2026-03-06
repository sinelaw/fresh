# Documentation Audit — Fresh 0.2.9 → 0.2.13

Audit date: 2026-03-06

This document classifies all docs by status and lists items that need
manual/thresh validation before the documentation can be considered correct.

---

## 1. Documents to MOVE to `docs/internal/`

These are developer-focused or obsolete and should not be in the user-facing
docs tree.

| File | Reason |
|------|--------|
| `docs/architecture.md` | Developer internals (runtime model, event loop, state ownership). Not user-facing. |

### Already correctly in `docs/internal/`

The following are design docs for **already-shipped features** and could be
pruned or archived per the README's policy ("Completed plans have been
removed"):

| File | Shipped in |
|------|-----------|
| `timesource-design.md` | Pre-0.2; doc self-declares "now implemented" |
| `bulk-edit-optimization.md` | 0.2.x; migration tracking complete |
| `cli-redesign.md` | 0.2.0; marked "Implemented (Experimental)" |
| `FUZZY_FILE_FINDER_UX.md` | 0.2.x; doc self-declares implemented |
| `finder-abstraction.md` | Implemented |
| `encoding-support-design.md` | 0.1.99 |
| `diff-view.md` | 0.2.x |
| `scroll-sync-design.md` | 0.2.4 |
| `paste-handling.md` | 0.2.x |
| `session-persistence-design.md` | 0.2.0 |
| `i18n-design.md` | 0.1.x |
| `per-buffer-view-state-design.md` | 0.2.4 |
| `search-next-occurrence.md` | 0.2.x |
| `editor-state-refactoring.md` | 0.2.x |
| `theme-consolidation-plan.md` | 0.2.x |
| `config-design.md` | 0.2.x (4-layer config is live) |
| `config-implementation-plan.md` | 0.2.x (config layers are live) |

### Duplicate internal docs (pick one, delete the other)

| Pair | Notes |
|------|-------|
| `SSH_REMOTE_EDITING_DESIGN.md` ↔ `ssh-remote-editing-design.md` | Same topic, different filenames |
| `PLUGIN_MARKETPLACE_DESIGN.md` ↔ `plugin-marketplace-design.md` | Same topic, different filenames |

---

## 2. User-facing docs that need UPDATES for 0.2.9–0.2.13 features

### `docs/features/lsp.md` — HIGH priority

Missing documentation for:

- **LSP Environment Variables** (0.2.13) — pass env vars to LSP server binaries
  via config
- **LSP Language ID Overrides** (0.2.13) — `language_id_overrides` in LSP server
  config
- **Rust LSP Mode Switching** (0.2.13) — command palette action to switch
  full / reduced-memory rust-analyzer
- **`restartLspForLanguage`** plugin API (0.2.13)
- **Process limits** for `registerLspServer` (0.2.13)
- **Disabled LSP start/restart commands** for languages without LSP config
  (0.2.13)
- **Inline Diagnostics** (0.2.13) — diagnostic text at end of line, enable in
  Settings UI

### `docs/features/editing.md` — HIGH priority

Missing documentation for:

- **Hanging Line Wrap** (0.2.13) — wrapped continuation lines preserve parent
  indentation
- **Auto-Close Config** (0.2.12) — separate `auto_close` toggle, per-language
  overrides via `languages.<lang>.auto_close`
- **Surround Selection** (0.2.12) — typing delimiter with selection wraps it
- **Smart Quote Suppression** (0.2.12) — quotes inside strings don't auto-close
- **Read-Only Mode** (0.2.12) — auto-detection for library/toolchain paths,
  toggle command, `[RO]` status bar indicator
- **Whitespace Indicators** (0.2.11) — granular per-position control (leading,
  inner, trailing) for spaces and tabs
- **Indent-Based Code Folding** (0.2.11) — folding without LSP, works in large
  file mode
- **Signature Help** rendering improvements (0.2.13) — markdown in popups

Also has leftover `TODO` comments on lines ~105, ~107.

### `docs/features/themes.md` — MEDIUM priority

Missing:

- **Theme Editor Redesign** (0.2.13) — virtual scrolling, mouse support,
  flicker-free inline styling
- **"Inspect Theme at Cursor"** command (0.2.13)
- **Ctrl+Right-Click theme info popup** (0.2.13)
- **`whitespace_indicator_fg`** theme color (0.2.11)

### `docs/features/command-palette.md` — MEDIUM priority

Missing:

- **Open File Jump** (0.2.13) — `path:line[:col]` syntax in Open File prompt
  and Quick Open

### `docs/plugins/index.md` — HIGH priority

Missing:

- **Load Plugin from Buffer** (0.2.13) — run & hot-reload plugins from an open
  buffer with LSP support
- **Plugin API `registerHandler()`** (0.2.13) — replacing `globalThis` pattern
- **`restartLspForLanguage`** and **async `reloadGrammars()`** (0.2.13)
- **Strict TypeScript** requirement across all plugins (0.2.13)
- **Plugin API v2** (0.2.4) — `createTerminal`, `sendTerminalInput`,
  `closeTerminal`, `getAllCursors`, plugin state API

### `docs/configuration/index.md` — MEDIUM priority

Missing from editor settings reference:

- `auto_close` toggle and per-language overrides (0.2.12)
- `auto_surround` config (0.2.12)
- Whitespace indicator settings (0.2.11)
- LSP `language_id_overrides` (0.2.13)
- LSP environment variables (0.2.13)
- `show_status_bar` toggle (0.2.13)
- `diagnostics_inline_text` setting (0.2.13)

### `docs/features/file-explorer.md` — LOW priority

Missing:

- Quick search (type to filter) — shipped pre-0.2.9 but never documented

### `docs/features/terminal.md` — LOW priority

Minor:

- Bracket paste mode fixes (0.2.5, 0.2.13) — user-visible in client/server
  sessions

### `docs/getting-started/index.md` — LOW priority

- Could benefit from `path:line:col` syntax examples and a mention of session
  mode (`fresh -a`)

### Blog posts — NO ACTION NEEDED

Blog posts are point-in-time snapshots. They are accurate for the versions they
describe and don't need retroactive updates.

---

## 3. Validation checklist — things to verify with thresh / manual testing

Each item below should be confirmed to work as documented (or as we plan to
document) before publishing updated docs.

### LSP features (test with a Rust and a TypeScript project)

- [ ] **LSP env vars**: Add `"env": {"FOO": "bar"}` to an LSP server config in
  `config.json`, verify the LSP process receives it (e.g. use a shell wrapper
  that prints env)
- [ ] **Language ID overrides**: Set `"language_id_overrides": {"tsx": "typescriptreact"}`
  and verify the LSP receives the overridden language ID on `textDocument/didOpen`
- [ ] **Rust LSP mode switching**: Open a Rust project, use command palette to
  switch between Full and Reduced Memory modes, confirm rust-analyzer restarts
  with appropriate config
- [ ] **Inline diagnostics**: Enable "diagnostics inline text" in Settings UI,
  trigger an error, verify diagnostic text appears at end of line. Verify
  staleness dropping (edit the line, confirm diagnostic fades/disappears)
- [ ] **Disabled LSP commands**: Open a file with no LSP config (e.g. plain
  text), confirm "Start LSP" / "Restart LSP" commands are disabled/hidden in
  command palette
- [ ] **Signature help markdown**: Open a TypeScript file, trigger signature
  help (type a function call), verify markdown renders correctly in popup

### Editing features

- [ ] **Hanging line wrap**: Enable line wrap, open a file with indented code,
  resize window so lines wrap — verify continuation lines preserve parent
  indent level
- [ ] **Auto-close config**: Set `"auto_close": false` in config, verify
  brackets/quotes no longer auto-close. Test per-language override:
  `"languages": {"markdown": {"auto_close": false}}`
- [ ] **Surround selection**: Select text, type `(` — verify it wraps to
  `(selection)`. Test with `[`, `{`, `"`, `'`
- [ ] **Smart quote suppression**: Position cursor inside a string, type `"` —
  verify it inserts one quote, not two
- [ ] **Read-only mode**: Open a file from `/usr/include` or a rustup toolchain
  path — verify `[RO]` appears and edits are blocked. Test "Toggle Read Only"
  command to override
- [ ] **Whitespace indicators**: Configure `"whitespace": {"space": {"leading": true, "inner": false, "trailing": true}}` — verify dots appear only in
  leading/trailing positions
- [ ] **Indent-based code folding**: Open a Python file (no LSP), verify fold
  indicators appear in gutter based on indentation. Test fold/unfold via
  gutter click and command palette

### Theme features

- [ ] **Theme editor virtual scrolling**: Open theme editor with a theme that
  has many fields — verify smooth scrolling, no flicker
- [ ] **Theme editor mouse support**: Click on color fields in theme editor,
  verify mouse interaction works
- [ ] **Inspect Theme at Cursor**: Place cursor on colored text, run "Inspect
  Theme at Cursor" from command palette — verify it shows the theme scope/color
- [ ] **Ctrl+Right-Click**: Right-click with Ctrl on colored text, verify theme
  info popup appears
- [ ] **`whitespace_indicator_fg`**: Add this key to a custom theme, verify
  whitespace indicators use the custom color

### Plugin API

- [ ] **Load Plugin from Buffer**: Open a `.ts` file with plugin code, run
  "Load Plugin from Buffer" command — verify plugin activates. Edit the file,
  verify hot-reload works
- [ ] **LSP in plugin dev buffers**: While editing a plugin buffer, verify
  TypeScript LSP completions work (e.g. `fresh.` should autocomplete API
  methods)
- [ ] **`registerHandler()`**: Write a plugin using `registerHandler()` instead
  of `globalThis`, verify it works
- [ ] **`restartLspForLanguage()`**: Call from a plugin, verify the LSP for that
  language restarts
- [ ] **`reloadGrammars()`**: Call async `reloadGrammars()` from a plugin,
  verify syntax highlighting refreshes
- [ ] **Process limits**: Set `process_limits` in `registerLspServer()` config,
  verify the LSP process respects memory/CPU limits

### Command palette / navigation

- [ ] **Open File Jump**: Type `src/main.rs:42` in Open File prompt — verify it
  opens the file at line 42. Test `src/main.rs:42:10` for column positioning
- [ ] **Status bar toggle**: Run "Toggle Status Bar" from command palette,
  verify it hides. Run again, verify it shows. Check config persists across
  restart

### Configuration

- [ ] **All new config keys**: Verify each new config key listed in section 2
  is accepted by Fresh without errors and has the documented effect
- [ ] **Settings UI**: Verify new settings (inline diagnostics, auto_close,
  auto_surround, whitespace indicators, show_status_bar) appear in the
  Settings UI with correct types and descriptions

---

## 4. Summary of work needed

| Priority | Category | Items |
|----------|----------|-------|
| **High** | Doc updates | `features/lsp.md`, `features/editing.md`, `plugins/index.md` |
| **Medium** | Doc updates | `features/themes.md`, `features/command-palette.md`, `configuration/index.md` |
| **Low** | Doc updates | `features/file-explorer.md`, `features/terminal.md`, `getting-started/index.md` |
| **Housekeeping** | Move to internal | `architecture.md` |
| **Housekeeping** | Archive shipped designs | ~17 completed design docs in `docs/internal/` |
| **Housekeeping** | Deduplicate | 2 pairs of duplicate internal docs |
| **Validation** | Thresh/manual | 25 test items across 5 categories |
