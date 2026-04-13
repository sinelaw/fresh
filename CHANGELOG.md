# Release Notes

## 0.2.24

### Improvements

* **Preview Tabs in File Explorer**: Single-clicking a file in the explorer now opens it in an ephemeral "preview" tab that is replaced by the next single-click instead of accumulating tabs (#1403). Preview tabs are promoted to a permanent tab when the user edits the buffer, double-clicks or presses Enter on the file in the explorer, clicks the tab itself, or performs a layout action (split, move tab, close split, focus a different pane). At most one preview tab exists editor-wide, and it's anchored to the split it was opened in — moving focus to another pane commits the previous preview. **Config:** enabled by default; disable via Settings UI (File Explorer -> Preview Tabs or use search).

* **LSP Status Bar Indicator**: Redesigned to a stable-width `LSP (on/off/error)` status indicator with a spinner while a server is starting or working — no more status bar noise as progress messages arrive. Configured-but-dormant servers are now visible as `LSP (off)` so you can see at a glance that an LSP is available to start. Clicking opens a popup with per-server status and live progress under the server name.

* **LSP Indicator: missing-binary detection and dismissable pill**: The LSP popup now probes each configured server's binary up-front, so opening the popup for a language like Python or Go on a system without `pylsp`/`gopls` installed labels the server "binary not in PATH" and replaces the actionable "Start" row with a disabled "Install … to enable" advisory — no more starting a server only to watch it fail. The popup also offers a "Disable LSP pill for {language}" row that mutes the indicator without touching your on-disk config (re-enable it from the same popup; dismissal is session-scoped). Plugins receive the same information through an extended `lsp_status_clicked` hook payload (`missing_servers`, `user_dismissed`).

* **Diagnostics in Hover Popup**: When the cursor sits on a symbol that also carries an error, warning, or hint, the hover popup prepends the overlapping diagnostic (severity and source like `rustc`/`clippy`/`clangd`) above the hover body.

* **C++ Header Detection**: Added heuristics to assign an `.h` files the C++ rather than C language. You can change it manually by clicking on the language in the status bar.

* **Review Diff**: Clicking a file in the files panel now selects it (previously clicks were ignored), and keyboard navigation auto-scrolls the files panel to keep the selected entry visible.

### Bug Fixes

* Fixed LSP workspace diagnostic refresh and inlay-hint refresh occasionally pulling from buffers of the wrong language (e.g. asking the Rust server about `package.json`, producing `file not found` errors).

## 0.2.23

### Improvements

* **Windows-1251 Encoding**: Added support for Windows-1251 (Cyrillic) encoding for loading and saving Cyrillic-script text files (#1453). Available in the encoding selector; auto-detected for text mixing uppercase and lowercase Cyrillic letters.

* **Theme Editor and Package Manager**: Multi-panel plugin UIs now behave like native splits — per-panel mouse-wheel scrolling and scrollbars, draggable panel dividers, and the theme editor's own colors now use the active theme.

* **File Finder in Command Palette (Ctrl+P)**: Much faster and more responsive on large local and remote trees — file enumeration runs in the background with results streaming in as they're found, typing a path like `etc/hosts` produces instant filesystem-confirmed matches, and ranking now reliably prefers contiguous matches (`results` finds `results.json` first) including multi-term queries that reconstruct a path or identifier (`etc hosts` → `/etc/hosts`, `save file` → `save_file.rs`).

* **Review Diff**: Brought back features that were dropped in the rewrite in version 0.2.22: stage, unstage, and discard individual hunks; jump between hunks with `n`/`p`; leave line comments (`c`) and overall session notes (`N`), edit or delete them with confirmation, see notes in the file list panel, and export your review notes to a markdown file. Redesigned toolbar of styled key hints that adapts to the focused panel.

* **Keybinding Editor**: Special keys like Esc, Tab, and Enter can now be bound — press Enter on the key field to enter capture mode, then the next keypress is recorded as-is (#1501). Fixed parent modal to be dimmed while a sub-dialog is open.

* **Customizable Status Bar**: The left and right sides of the status bar are now configurable via the Settings UI using a new DualList picker (transfer items between Available/Included columns, reorder with arrows). Includes a new `{clock}` element that displays HH:MM with a blinking colon. Thanks @1612elphi!

* **LSP Status Bar Indicator**: Simplified to a single color-coded "LSP" label — clicking LSP in the status bar now opens an interactive popup with per-server status and actions (restart, stop, view log).

* **Universal LSP Servers**: LSP servers configured for all languages are now spawned once per project instead of once per opened language, eliminating duplicate processes.

### Bug Fixes

* **Regression** - Fixed multi-byte UTF-8 characters not being parsed correctly in the input handler, and IME-composed characters delivered as key-up events being ignored on Windows (#1538). Thanks @wellorbetter! Reported by @AugustusZane.

* Fixed blank panels appearing after terminal resize.

* Fixed terminal mode not being exited when the active buffer is switched to a non-terminal.

* Fixed Review Diff hunk navigation (`n`/`p`) not working in the diff panel, files panel not receiving focus on launch, hunk-level comments not displaying (#1503), and deleted-file drill-down crashing.

* Fixed Settings UI section headers being invisible in the high-contrast theme.

* Fixed word wrap producing single-character-per-line output on narrow terminals with deeply indented code — the hanging indent was being double-counted (#1502).

* Fixed LSP completion popup showing duplicate entries when reopened (#1514).

* Fixed LSP `auto_start` being ignored on a per-server basis when multiple servers are configured for one language — opening a file no longer drags in every enabled server, only those individually marked `auto_start`.

* Fixed mouse input issue - mouse state not being restored in the terminal - after leaving Fresh (Windows only, #1530).


## 0.2.22

### Features

* **Review Diff Rewrite**: The review diff view has been rewritten with a magit-style split-panel UI. The left panel lists files grouped by staged/unstaged/untracked sections (sorted by category), and the right panel shows the diff for the selected file. Navigate with arrow keys, switch focus between panels with Tab, and drill down into individual files. Hunk navigation jumps between changes with auto-centering. Untracked and newly added files are now shown correctly. Diff colors are now theme-aware with per-theme highlight overrides.

* **Remote Mode**: SSH connections now auto-reconnect in the background with a disconnected indicator in the status bar. Filesystem operations no longer block the event loop. File explorer roots at the provided remote path instead of the home directory. File finder (Ctrl+P) works on remote filesystems. Error messages are cleaner — hints about SSH installation, and a "Connecting via SSH to ..." message on startup.

### Improvements

* **Create Directories on Save**: When saving a file to a path where the parent directory doesn't exist, Fresh now prompts to create the directory instead of failing (#1434).

* **Grammar Short Name Aliases**: Grammars can now be referenced by short names (e.g., `"bash"` instead of `"Bourne Again Shell (bash)"`) in config and the Set Language popup. Packages can declare a `shortName` in their grammar definition.

* **Default Language Setting**: The `default_language` setting replaces the previous `fallback` object. Set it to a language key (e.g., `"bash"`) so unrecognized file types use that language's full configuration (#1219).

* **AutoHotkey Syntax Highlighting**: Built-in grammar for `.ahk` and `.ahk2` files with v1/v2 command coverage.

* **Settings UI**: Added inherit/unset support for nullable settings with an Inherit button and inherited badge. The Delete key now unsets a setting override.

* **Theme Selector**: Installed theme packages now appear correctly even when multiple themes share the same name. The selector strips URL schemes and sizes the name column to content.

* **File Finder (Ctrl+P)**: Fixed showing no files on Windows when git isn't being used.

* **Selection Prompts**: Pre-filled text is now selected so typing immediately replaces it.

* **Theme Fixes**: Fixed low contrast in Nord, Solarized Dark, Light, and Dracula themes. Fixed command palette selected row using wrong foreground color. Syntax highlighting colors are now preserved in text selections.

### Bug Fixes

* Fixed out-of-memory crash caused by an infinite loop in the line-wrapping transform when indentation exceeds half the available width (#1454).

* Fixed `didOpen` notification only being sent to the first LSP server when multiple are configured for a language.

* Fixed status bar line number not updating when stepping through search matches with F3.

* Fixed `.bash_profile` appearing read-only when symlinked to a macOS library path (#1469).

* Fixed session `open-file` command failing when a session exists but its name doesn't match the socket.

* Fixed scrollbar track hover highlighting more than the hovered cell.

* Fixed self-update URL pattern not matching all release URLs.

## 0.2.21

### Features

* **Fast Completions without LSP**: New basic completions providers without language server — buffer-word candidates appear below LSP results in the popup. Also, a new setting (config) controls auto-trigger vs explicit Ctrl+Space (default: explicit). Enter dismisses the popup (Tab accepts). I plan to further improve it (make it more intelligent) in future releases.

* **Current Line Highlighting**: Highlights the cursor line. Enabled by default, togglable from the command palette and Settings UI (caveat: wrapped lines are currently highlighted in their entirety, this should probably be changed to visual lines).

* **LSP Code Actions**: Code action modal now actually works! Select an action by number or up/down arrows and enter (#1405). Supports resolve, execute command, and server-initiated workspace edits (previously dropped silently). File create/rename/delete operations handled. Actions from multiple servers unified into one popup. Default keybinding changed to Alt+. - because Ctrl+. is filtered by many terminals.

* **LSP Completion Resolve and Formatting**: Auto-imports applied on completion accept. Format Buffer falls back to LSP when no external formatter is configured. Also adds range formatting and pre-rename validation.

* **LSP Server Selection for Restart/Stop**: Popup to choose which server to restart/stop individually, or all at once.

* **Grammar Listing**: `fresh --cmd grammar list` and `editor.listGrammars()` plugin API show all available grammars with source and extensions. When specifying a grammar in a `languages` entry in the config, you must currently use a full name from this list - for example "Bourne Again Shell (bash)" rather than "bash". This will be improved once I add grammar aliases.

### Improvements

* **Theme Contrast**: Replaced all named ANSI colors with explicit RGB in built-in themes for deterministic rendering. Improved contrast ratios across both high-contrast and light themes. Diagnostic and semantic overlays now re-apply correctly on theme change, including during live preview.

* **Git Status Marker Refresh**: File explorer markers update on terminal focus gain and by polling for git index changes (#1431).

* **Config-Only Languages**: Custom languages without a built-in grammar (e.g., "fish") appear in the Set Language popup and are detected correctly — no more fallthrough to wrong built-in grammars.

* **Theme Inspector**: Records exact theme keys during rendering instead of reverse-mapping via heuristics. Theme editor Save As improved for built-in themes.

* **LSP Reliability**: Diagnostics cleared on server stop/crash, buffers re-opened on server start, document version checking for workspace edits, LSP notified after undo/redo of bulk edits, pending requests drained on server death to prevent deadlocks, hover suppressed while popups are visible.

### Vim Mode

22 bug fixes: C/D/S/cc, e motion, nG, h/l line clamping, ^, $, J with space, f/t special chars, r replace, ~ toggle case, visual mode entry/switching, count display. Key motions moved from async plugin commands to native Rust actions, eliminating race conditions.

If you use the Vim plugin please drop a note at https://github.com/sinelaw/fresh/discussions/417 - I need feedback on this feature.

### Bug Fixes

* Fixed Ctrl+W panic on accented/multi-byte characters (#1332).

* Fixed LSP diagnostics from stopped servers reappearing from queued messages.

## 0.2.20

### Features

* **Multi-LSP Server Support**: Configure multiple LSP servers per language (e.g., pylsp + pyright for Python). Servers are routed by feature using `only_features`/`except_features` filters, completions are merged from all eligible servers, and diagnostics are tracked per-server. Per-server status is shown in the status bar (#971).

* **Per-Language Editor Settings**: `line_wrap`, `wrap_column`, `page_view`, and `page_width` can now be configured per-language. For example, wrap Markdown at 80 columns while keeping code unwrapped (#1371).

* **Diff Chunk Navigation Plugin**: New built-in plugin for navigating between diff chunks, merging git and saved-diff sources.

### Improvements

* **Faster Startup (~350ms → ~170ms)**: Syntax grammars are pre-compiled at build time, package loading moved from JavaScript to Rust, plugin I/O and transpilation run in parallel, and redundant grammar rebuilds are eliminated. Plugins can now declare dependencies via `import type` from `"fresh:plugin/..."` and are topologically sorted.

* **Settings UI Overhaul**: Modernized visual design with wider modal (160 cols), rounded corner borders, Nerd Font category icons, styled `[✓]` toggles, and reverse-video key hints. Keyboard navigation rewritten: Tab cycles sequentially through all fields and buttons, composite controls (Map, ObjectArray, TextList) support internal navigation, entry dialogs have section headers with explicit field ordering, PageDown/PageUp work in the main panel, and TextList edits auto-accept on navigation. Focus indicator now highlights per-row in composite controls.

* **Settings Deep Search**: Also in the Settings UI: Search now walks into Map entries, TextList items, and nested JSON values. Searching "python" finds the "python" key in language/LSP maps. Results show hierarchical breadcrumbs (e.g., "Languages > python") and auto-focus the matching entry.

* **Per-Language Workspace Root Detection**: New `root_markers` field on LSP server configs. The editor walks upward from the file's directory looking for configured markers (e.g., `Cargo.toml`, `package.json`), replacing the old cwd-based root (#1360).

* **Page View Mode**: "Compose" mode renamed to "Page View". Can now auto-activate per language via `page_view: true` in language config. Old keybinding names continue to work.

* **256-Color Contrast Enforcement**: When running in a 256-color terminal, foreground colors are automatically adjusted to meet WCAG 3.0:1 minimum contrast ratio against their background. Fixes illegible text in Solarized Dark, Nord, Dracula, and light themes under tmux without truecolor.

* **LSP in Library Files**: Files in library paths (site-packages, node_modules, .cargo) now keep LSP enabled for Goto Definition, Hover, and Find References while remaining read-only (#1344).

* **Goto Matching Bracket**: Works inside bracket bodies by searching backward for the nearest enclosing bracket, matching VS Code and JetBrains behavior. All bracket searches are bounded to prevent hangs on huge files (#1258).

* **LSP Head-of-Line Blocking Fix**: LSP notifications (didClose, didChange, shutdown) are no longer blocked behind pending request responses.

* **New Settings**: `show_tilde` to hide EOF tilde markers (#1290), `menu_bar_mnemonics` to disable Alt+key menu shortcuts (#1257).

* **`getPluginDir()` Plugin API**: Plugins can now locate their own package directory to find bundled scripts or install local dependencies.

### Bug Fixes

* Fixed CSI u and xterm modifyOtherKeys key sequences inserted as literal text in terminal session mode (#1113).

* Fixed word selection (Ctrl+W) stopping at accented/Unicode characters (#1332).

* Fixed double-click backward drag losing the initial word selection (#1334).

* Fixed block cursor invisible in zellij due to double cursor-inversion (#1338).

* Fixed cursor visibility and command palette rendering in zellij (#1255).

* Fixed undo incorrectly clearing the modified flag after hot exit recovery, which could cause data loss.

* Fixed bulk edit (e.g., toggle comment) displacing inlay hints on subsequent lines (#1263). Displaced markers are now restored to exact positions on undo.

* Fixed large file syntax highlighting lost when revisiting a file, caused by checkpoint offset drift during partial cache updates.

* Fixed embedded language highlighting (e.g., CSS in HTML) breaking at large file offsets.

* Fixed Enter key leaking into the markdown buffer when the file explorer panel is focused.

* Fixed large file recovery saving the entire file as individual chunks instead of using the recovery format.

* Fixed read-only detection for files not owned by the current user (now checks effective uid/gid instead of file mode bits).

## 0.2.18

### Features

* **Bracketed Paste on Windows & Input Overhaul**: Bracketed paste now works on Windows Terminal (reverted in v0.2.17 due to #1284), and keyboard issues are resolved (#1054). **Mouse hover is disabled by default on Windows** because reliable bracketed paste requires cell-motion tracking; enabling hover switches to all-motion tracking which can insert corrupt text under heavy mouse movement or slow CPU. Re-enable it in Settings UI under Editor → Mouse Hover Enabled. Under the hood, crossterm's Windows input handling is replaced with a new `fresh-winterm` crate using direct VT input reads, with corrupt mouse sequence detection, UTF-16 surrogate handling, and console mode heartbeat to counteract ConPTY drift.

* **30 New Syntax Grammars**: Dockerfile, CMake, INI, SCSS, LESS, PowerShell, Kotlin, Swift, Dart, Elixir, F#, Nix, Terraform/HCL, Protobuf, GraphQL, Julia, Nim, Gleam, V, Solidity, KDL, Nushell, Starlark, Justfile, Earthfile, Go Module, Vue, Svelte, Astro, Hyprlang (#1266). These grammars are preliminary — please report highlighting issues for your language so we can improve them.

* **Broad LSP Support**: Added LSP configs and helper plugins (with install instructions) for Nix, Kotlin, Swift, Scala, Elixir, Erlang, Haskell, OCaml, Clojure, R, Julia, Perl, Nim, Gleam, F#, Dart (#1252), Nushell (#1031), Solidity (#857), Vue, Svelte, Astro, Tailwind CSS, Terraform/HCL, CMake, Protobuf, GraphQL, SQL, Bash, Lua, Ruby, PHP, YAML, TOML, and Typst. LSP integration for these languages is early-stage — feedback from users of these languages is welcome.

* **Deno LSP Auto-Detection**: Automatically detects and uses the Deno language server for JS/TS projects (#1191).

* **`show_prompt_line` Setting**: New config option to auto-hide the prompt line. Applied immediately from Settings UI (#1273).

* **`use_tabs` Setting**: Global `editor.use_tabs` config option for default tab indentation (#1295).

### Improvements

* **Plugin Commands in Keybinding Editor**: Plugin-registered commands are now shown and searchable in the keybinding editor.

* **Theme Editor ANSI Colors**: Named ANSI colors display as "terminal native" instead of misleading RGB values, with correct native color swatches (#1301).

* **Status Bar Language Info**: Shows "[syntax only]" when a language has no LSP config entry.

* **Default Language**: Set `default_language` to a language key (e.g., `"bash"`) so undetected file types use that language's full configuration (#1219). Replaces the previous `fallback` object; the old key is still accepted for backwards compatibility.

* **File Deletion Uses Trash**: `removePath` now uses the system trash instead of permanent deletion.

* **Package Manager Cross-Platform**: Plugin package manager uses cross-platform APIs instead of Unix-specific commands on Windows (#1215).

### Bug Fixes

* Fixed arrow keys not working in `less`/`git log` in the embedded terminal, including `TERM` env var not being set on Unix.

* Fixed Tab key getting trapped in TextList editing mode in Settings UI.

* Fixed `{`, `}`, `;` highlighted as operators instead of punctuation in C/C++ (#1318, #1319).

* Fixed auto-dedent for languages without tree-sitter, e.g. Dart.

* Fixed auto-indent after closing brace in nested C++ blocks.

* Fixed mouse click selecting wrong item in scrolled settings list.

* Fixed keybindings for plugin-registered commands not executing (#1312).

* Fixed Find Next/Find Previous ignoring cursor position (#1305).

* Fixed Tab indent affecting lines outside selection (#1304).

* Fixed Shift+letter keybinding deletion not persisting (#1303).

* Fixed word selection not preserved when dragging after double-click (#1202, #1317).

* Fixed `removePath` failing on Windows due to UNC path mismatch.

* Fixed external files missing from tab bar after session restore.

* Fixed scroll wheel targeting focused split instead of split under pointer (#1270).

* Fixed wrap indent not working with tab indentation (#1283).

* Fixed LSP "no server configured" for Kotlin and 30+ other languages.

* Fixed Diff syntax highlighting scope-to-category mappings.

* Fixed extension mappings for `.cjs`, `.mjs`, `.mts`, `.cts`, `Jenkinsfile`, `Brewfile`.

* Fixed LSP initialization timeout too short (increased from 10s to 60s).

### Internal

* Added syntax highlighting validation suite with 93 fixture files and e2e tests.

* Added e2e tests for Settings UI, keybinding editor, search/replace, and plugin commands.

* Fixed multiple flaky e2e tests (search/replace, plugin uninstall, Settings UI).

* Removed redundant `SIGUSR1` handler; relies on harness signal handler for backtraces.

* Cleaned up completed design docs.

---

## 0.2.17

### Bug Fixes

* **Reverted Windows Bracketed Paste Fix**: Reverted the bracketed paste fix for Windows Terminal (#1218) as it broke mouse input (#1284). The fix enabled `ENABLE_VIRTUAL_TERMINAL_INPUT` which interfered with mouse event handling.

---
## 0.2.16

### Features

* **Project-Wide Search & Replace**: Search and replace across the entire project. Works reliably with unsaved buffers, large files, and up to 10,000 results. Alt+Enter to replace in project.

* **Hot Exit**: All buffers — including unnamed scratch buffers — persist across sessions automatically. Configurable via `hot_exit` setting (#1148, #1233).

* **Workspace Storage**: Session state always restored on startup, even when opening specific files from CLI. Plugin state also persists across sessions.

### Improvements

* **Keybinding Editor**: Collapsible section headers and plugin mode bindings shown as first-class entries.

* **Markdown Compose Mode**: Easier to discover via global toggle and searchable command palette entries.

* **Tab Naming**: Duplicate tab names are disambiguated with appended numbers.

* **View...Keybinding Style: Menu Checkboxes**: Submenu items now show checkbox indicators for toggled settings.

### Bug Fixes

* Fixed crash when workspace references deleted files (#1278).

* Fixed CapsLock breaking keyboard shortcuts like Ctrl+A, Ctrl+C, etc.

* Fixed bracketed paste not working on Windows Terminal.

* Fixed clipboard not working in client-server session mode.

* Fixed Latin-1 files misdetected as UTF-8 for short files with trailing high bytes.

* Fixed line number bugs: Delete key not updating status bar (#1261), relative line numbers miscounting (#1262).

* Fixed C# LSP not working due to language ID mismatch.

* Fixed remote editing using hardcoded `/tmp` instead of querying the remote system.

* Fixed high memory usage on Windows (#1205).

* Fixed PageUp/PageDown not working in Theme Editor sidebar.

* Fixed unbound keys being swallowed in plugin modes.

### Packaging

* **Linux**: Icons and desktop files added to all packaging methods (deb, rpm, Flatpak, AppImage). Fixed Flatpak AppStream metadata for app stores.

---
## 0.2.14

### Improvements

* **Keybinding Map Checkboxes**: Submenu items in the keybinding map now show checkbox indicators for toggled settings.

### Bug Fixes

* **Windows Memory Usage**: Fixed high memory usage on Windows caused by buffered input event draining before render (#1205).

---

## 0.2.13

### Features

* **Inline Diagnostics**: Diagnostic text displayed at end of lines, right-aligned, with version-aware staleness dropping. Disabled by default — enable "diagnostics inline text" in the Settings UI (#1175).

* **Hanging Line Wrap**: Wrapped continuation lines preserve the indentation of their parent logical line (#1169).

* **Theme Editor Redesign**: Virtual scrolling, mouse support, flicker-free inline styling. New "Inspect Theme at Cursor" command and Ctrl+Right-Click theme info popup.

* **Open File Jump**: `path:line[:col]` syntax in Open File prompt and Quick Open (#1081, #1149).

### Improvements

* **Plugin API**: `registerHandler()` replacing `globalThis` pattern, `restartLspForLanguage`, process-limits for `registerLspServer`, async `reloadGrammars()`. Strict TypeScript across all plugins.

* **Load Plugin from Buffer**: Run and hot-reload plugins directly from an open buffer, with LSP support for plugin dev buffers.

* **Status Bar Toggle**: Command palette command and config option to show/hide the status bar.

* **LSP Environment Variables**: Pass environment variables to LSP server binaries via config (#1159).

* **LSP Language ID Overrides**: Configurable `language_id_overrides` in LSP server config.

* **Rust LSP Mode Switching**: Command palette action to switch between Full and Reduced Memory modes for rust-analyzer.

* **Signature Help Rendering**: Markdown rendering for signature help popups with hanging indent and paragraph spacing.

* **Non-Blocking Grammar Builds**: `SyntaxSet::build()` moved to a background thread. Buffered input events drained before render for CPU-constrained responsiveness.

* Disabled LSP start/restart commands for languages without LSP config (#1168).

### Bug Fixes

* **LSP Bracket Paths**: Fixed LSP failing for file paths containing `[` or `]` (#953).

* **Search F3 Navigation**: Fixed F3 losing matches after viewport scroll (#1155).

* **Settings JSON Copy**: Fixed Ctrl+C not working in settings JSON editor (#1159).

* **Line Numbers on New Files**: Fixed line numbers showing when disabled in settings for newly opened files (#1181).

* **Client/Server Paste**: Fixed bracketed paste mode and terminal feature parity in client/server sessions (#1168).

* **Popup Selection**: Fixed popup text selection copying wrong text when lines wrap (#1170).

* **Suggestions Popup Border**: Fixed bottom border overwritten by status bar (#1174).

* **TSX/JSX Language ID**: Fixed wrong `languageId` sent to LSP for TSX/JSX files (#1174).

* **LSP Error Suppression**: Suppress ContentModified/ServerCancelled errors per LSP spec instead of logging as errors.

* **Semantic Tokens**: Skip degraded semantic token responses to preserve syntax highlighting.

* **Theme Save**: Fixed save failing when themes directory doesn't exist (#1180). Fixed saving incomplete theme files.

* **LSP Completion**: Fixed completion debounce, cleanup-on-disable, and popup positioning issues.

---

## 0.2.12

### Features

* **Auto-Close Config**: Separate `auto_close` toggle (default: true) to independently control bracket/quote auto-close, skip-over, and pair deletion — previously coupled to `auto_indent`. Per-language overrides via `languages.<lang>.auto_close` (#1144).

* **Surround Selection**: Typing an opening delimiter with a selection wraps it instead of replacing it (e.g. selecting `hello` and typing `(` produces `(hello)`). Controlled by `auto_surround` config with per-language overrides.

* **Smart Quote Suppression**: Quotes typed inside an existing string no longer auto-close, preventing unwanted doubled quotes (#1142).

* **Read-Only Mode**: Files without write permission and library/toolchain paths (rustup, /usr/include, /nix/store, Homebrew Cellar, .nuget, Xcode SDKs) automatically open as read-only. New "Toggle Read Only" command to override. Status bar shows `[RO]` indicator.

### Bug Fixes

* **Multi-Cursor Enter**: Fixed Enter key in markdown mode only inserting a newline at the last cursor, ignoring secondary cursors. Falls back to built-in insert_newline when multiple cursors are active (#1140).

* **Multi-Cursor Position Drift**: Fixed cursors with no events during bulk edits (e.g. Delete at end of buffer) drifting to stale positions. Uses saturating arithmetic to prevent overflow with overlapping selections (#1140).

### Improvements

* **Log Noise Reduction**: Disabled span close events (~90% of log volume) by default and moved 12 high-frequency log sites to trace level. Typical log size reduced from ~266MB to ~5-10MB. Set `FRESH_LOG_SPANS=1` to re-enable (#1154).

### Internal

* Added multi-cursor shadow model property-based tests with random operation sequences across 2-3 cursors.
* Added e2e regression tests for multi-cursor Enter with auto_indent, Ctrl+D selection, tree-sitter, and markdown grammar.

---

## 0.2.11

### Features

* **Whitespace Indicators**: Granular control over whitespace visibility — configure space (·) and tab (→) indicators independently for leading, inner, and trailing positions. Master toggle, per-language overrides, and a new `whitespace_indicator_fg` theme color.

* **Indent-Based Code Folding**: Code folding now works in large file mode and for files without LSP folding ranges, using indentation analysis as a fallback. Fold from any line within a block (not just the header). Unified byte-offset pipeline for consistent gutter indicators.

* **Session Open-File Enhancements**: `--wait` flag blocks the CLI until the user dismisses a popup or closes the buffer — enables use as `git core.editor`. Range selection syntax (`file:L-EL`, `file:L:C-EL:EC`) and hover messages (`file:L@"markdown msg"`) for annotated file opening. Auto-attaches a client when `open-file` starts a new session.

* **GUI: macOS Native Integration** (experimental): Native menu bar with dynamic when/checkbox conditions, Cmd keybindings (`macos-gui` keymap), app icon, and `.app` bundle resources. Menu tracking detection prevents state mutations from causing menu jumps.

* **Platform Icons**: Application icons for Windows `.exe`, Linux `.deb`/`.rpm` packages, and macOS app bundles.

### Bug Fixes

* **Bracket Highlight Hanging on Large Files**: Bracket matching now caps scanning at 1MB and uses 16KB bulk reads instead of byte-at-a-time, preventing hangs on large files.

* **Markdown Plugin Activation**: Plugin now activates based on buffer language (not just file extension), fixing cases where `Set Language` to markdown didn't enable smart editing (#1117). Reverse bullet cycling on Shift+Tab now works correctly (#1116).

* **Settings UI**: Fixed Save button mouse click not closing the dialog. Fixed Reset button not showing confirmation dialog. Fixed Discard dialog persisting on reopen.

* **Active Tab Styling Bleed**: Fixed active tab border color bleeding through dropdown menus.

* **Cursor Corruption on Tab Click**: Fixed hardware cursor appearing at wrong position when clicking a tab in an inactive split.

* **Comment Delimiter Colors**: Fixed comment delimiter characters (e.g. `//`) using the wrong color in syntax highlighting.

* **Scroll Events Routing**: Fixed mouse scroll events going to the file explorer panel regardless of mouse position.

* **File Explorer Border**: Fixed hover/drag bugs on the file explorer resize border.

* **Windows Named Pipe Crash**: Fixed crash in `Server::handle_new_connection` on Windows.

* **Bar/Underline Cursor Invisible**: Fixed bar and underline cursor styles being invisible on characters due to REVERSED modifier creating a block-like highlight (#851).

* **Wrapped Line Viewport Scroll**: Fixed viewport scroll limit counting logical lines instead of visual rows, causing erratic scrolling, skipped wrapped rows, and stuck End key with line wrap enabled (#1147).

* **Search on Large Files**: Fixed multi-GB memory consumption, O(N²) offset accumulation, and search scan never completing when capped at max matches. Chunked incremental search, viewport-only overlays, and 100K match cap (#1146).

* **macOS Menu Hover Jump**: Fixed menu bar jumping to leftmost menu during hover by using `WaitUntil` instead of `Poll` and caching menu item states.

### Improvements

* Status log and warning log buffers are now read-only.
* Replaced `buffer_modified` JS plugin with native Rust diff indicators, eliminating JS↔Rust round-trips on every edit/scroll.

### Internal

* Folding system refactored to use byte offsets instead of line numbers for gutter indicators, fixing consistency issues in large file mode.
* Unified fold indicator pipeline shared between LSP-based and indent-based folding.
* Fixed Nix build: include PNG files in source filter for GUI icon resources.

---

## 0.2.9

### Features

* **Code Folding**: Fold/unfold code blocks via LSP foldingRange. Per-view fold state, gutter indicators for collapsed ranges, fold-aware scrolling. Toggle via command palette ("Toggle Fold") (#900). Thanks @asukaminato0721 !

* **Large File Line Numbers**: Large files show byte offsets in gutter/status bar until scanned. On-demand parallel line index scanning (via Ctrl+G prompt or "Scan Line Index" command) gives exact line numbers with progress indicator. Remote scanning counts newlines server-side without data transfer.

* **Markdown Source Editing**: New plugin for smart Markdown source-mode editing — auto-continues list items on Enter (bullets, ordered lists, checkboxes), removes empty markers, Tab indents + cycles bullet style (#1095).

* **GUI mode - can run without terminal** (highly experimental): GPU-accelerated windowed mode via winit + wgpu. Build with `--features gui` and run with `--gui` to try it.

### Improvements

* **Smart Backspace Dedent**: Backspace in leading whitespace removes one indent unit (tab_size spaces or 1 tab) instead of a single character.

* **Diagnostics Panel**: Up/Down now scrolls the editor to preview diagnostic location. Enter jumps and focuses the editor.

* **Glob Patterns in Language Config**: `filenames` field now supports glob patterns (`*.conf`, `*rc`, `/etc/**/rc.*`) for extensionless file detection (#1083).

* Disabled single-quote auto-close in Markdown files (interferes with apostrophes).

### Bug Fixes

* **Auto-Indent**: Fixed `tab_size` setting ignored for auto-indent; fixed indent level lost on normal statement lines; fixed Go auto-dedent using spaces instead of tabs (#1068); fixed Python nested indent after consecutive `:` lines (#1069).

* **File Explorer Dotfiles**: Fixed dotfiles always visible regardless of "Show hidden files" toggle. Config `show_hidden`/`show_gitignored` now applied on init (#1079).

* **LSP Toggle Desync**: Fixed state corruption when toggling LSP off/on — now sends `didClose` so re-enable gets fresh `didOpen` (#952).

* **LSP Client Capabilities**: Now advertises all supported capabilities including `publishDiagnostics`, enabling diagnostics from strict servers like pyright (#1006).

* **LSP Status Indicator**: Fixed status bar indicator disappearing after each request completion (#952).

* **Set Language**: Fixed command storing display name instead of canonical ID, breaking LSP config lookups (#1078).

* **Escape Sequences in Client Mode**: Fixed mouse codes, Shift+Tab, and standalone ESC not working in `fresh -a` attach mode (#1089).

* **Client Mode Terminal Reset**: Fixed terminal not fully restored on exit in client mode (#1089).

* **Ctrl+End with Line Wrap**: Fixed viewport not scrolling to trailing empty line; fixed Down arrow not reaching it (#992).

* **Diagnostics Panel Windows Paths**: Fixed file URIs not decoded properly on Windows (#1071).

* **Debug Keyboard Dialog**: Fixed not capturing keys in client/server mode (#1089).

### Performance

* Replaced linear span lookups in syntax highlighting with O(1) amortized cursor.
* Eliminated JSON round-trip and JS re-parsing in plugin hook dispatch (~16% CPU reduction).
* Path-copying PieceTree mutations with structural diff via `Arc::ptr_eq` — O(edit regions) instead of O(all leaves).
* Viewport-aware filtering and batch API for large file gutter indicators (~780K IPC commands → ~50 per edit).

### Internal

* Update flake.nix to rust 1.92.0
* Split GUI backend into separate `fresh-gui` crate.
* Unified language detection with `DetectedLanguage` struct and single `apply_language()` mutation point.
* CI now runs clippy with `--all-features` to lint GUI code.

---

## 0.2.5

### Features

* **Persistent Auto-Save**: New `auto_save_enabled` config option (default: false) to automatically save modified buffers to their original file at a configurable interval (`auto_save_interval_secs`, default: 30s) (#542)

* **Smart Home**: Home key now uses smart home behavior by default, toggling between the first non-whitespace character and column 0. On soft-wrapped lines, smart home respects visual line boundaries instead of jumping to the physical line start (#1064).

### Bug Fixes

* **Diff View Scrollbar**: Fixed scrollbar click-to-jump and thumb drag not working in side-by-side diff views. Composite buffer views now use row-based scrolling via CompositeViewState.

* **Terminal Bracket Paste**: Fixed pasted text going into the editor buffer instead of the terminal PTY when in terminal mode (#1056).

* **LSP did_open Reliability**: Fixed buffer being incorrectly marked as LSP-opened when the did_open send fails, which prevented retry and could corrupt server document state.

* **Remote Editing Data Loss**: Fixed intermittent data loss when loading large files via SSH remote editing on macOS. The bounded channel now uses backpressure instead of silently dropping data when the buffer overflows (#1059).

### Configuration

* Renamed `auto_save_interval_secs` (recovery) to `auto_recovery_save_interval_secs` to distinguish it from the new persistent auto-save feature. Added `auto_recovery_save_interval_secs` config option (default: 2s).

### Internal

* Introduced typed `LeafId` and `ContainerId` wrappers around `SplitId` to enforce leaf-vs-container constraints at compile time.
* Enabled `#![deny(clippy::let_underscore_must_use)]` crate-wide; all ignored `Result` values now have explicit annotations or proper error handling.
* Made `request_completion` and `request_signature_help` infallible, removing dead `Result` return types.
* Added CONTRIBUTING.md with development guidelines.

---

## 0.2.4

### Features

* **Markdown Compose Mode**: Distraction-free writing mode with concealed markup, soft breaks, table rendering, and mouse support. Split-view allows editing source and rendered markdown side-by-side with synchronized scrolling.

* **Vertical Rulers**: Configurable column rulers with add/remove commands via command palette. Per-buffer state and Settings UI JSON editor support (#1028).

* **Horizontal Scrollbar**: New horizontal scrollbar with drag support and toggle commands (#972).

* **Smooth Scrolling**: Cursor movement now scrolls one line at a time instead of jumping/recentering the viewport (#1040).

### Improvements

* **Macro Keybinding**: F4 shortcut for Play Last Macro. Removed Ctrl+0-9 and Alt+Shift+0-9 macro keybindings (#700).

* **Configurable Clipboard**: New `clipboard` config with `use_osc52` and `use_system_clipboard` toggles to prevent hangs in certain terminals (#964). Useful for Putty and other terminals that sometimes cause Fresh to hang on OSC 52.

* **Scrollbar Visibility**: New `show_vertical_scrollbar` and `show_horizontal_scrollbar` config options (#974).

* **Package Manager**: Reinstall support for local-path packages.

* **File Explorer Persistence**: Show hidden and show gitignored toggles now persist to config immediately (#569).

### Bug Fixes

* **Macro correctness**: Replaying a macro now respects the exact same effect as interactive flow.

* **Cursor Navigation**: Cursor up/down now lands at end-of-line when goal column is past content (#514).

* **Line Numbers**: Fixed line numbers leaking between splits due to shared margins state. Line numbers now live exclusively in per-split BufferViewState.

### Plugin API

* **Plugin API v2**: Versioned plugin API with `createTerminal`, `sendTerminalInput`, `closeTerminal`, `getAllCursors`, and plugin state API. Improved type declarations in `fresh.d.ts` (#1045).

* **Split Labels**: Splits can be labeled to prevent files opening in managed splits. Labels persist across save/restore. New `before` option to place buffers left/top.

### Internal

* Refactored per-buffer view state: cursors owned by SplitViewState, ComposeState extracted from EditorState.
* Conceal ranges, soft breaks, and overlay filtering by view_mode at render time.
* Plugin state snapshot reports active split's view_mode and compose flag.
* i18n updates for vertical rulers and macro shortcuts across all locales.
* PieceTree Performance: Use binary search instead of linear scan in line lookup.

---

## 0.2.3

### Bug Fixes

* **Undo Corruption After Save**: Fixed undo corrupting text after a bulk edit (e.g. multi-cursor or replace-all) followed by save. Piece tree buffers are now restored alongside the tree during undo.

* **Theme Not Found After Restart**: Fixed theme reverting to default when the JSON `name` field differs from the filename (#1001).

* **AltGr+Shift Input on Windows**: Fixed AltGr+Shift key combinations not being recognized as text input (#993).

* **Winget Manifest**: Fixed `UpgradeBehavior` indentation causing schema validation failure in winget publishing.

### Internal

* Added shadow model undo/redo property-based tests for increased piece tree correctness confidence.
* Added blog showcase system with animated GIF demos for the documentation site.

---

## 0.2.2

### Bug Fixes

* **Regex Find-and-Replace**: Fixed regex mode not working. Added capture group support (`$1`, `$2`, `${name}`) and a toolbar hint when regex mode is active.

* **Keybinding Editor**: Fixed actions with no default keybinding missing from the editor; all actions are now listed. Fixed inability to delete built-in keymap bindings. Fixed deleted bindings reappearing after save.

* **LSP Completion Popup**: Fixed completion popup blocking typing when not working in non-English locales.

* **Revised in-editor help**: Rewrote the in-memory help manual, should be a bit more useful.

### Internal

* Refactored keybinding editor into multi-file module.
* Locked Cargo.toml dependency versions to minor; docs recommend `--locked`. Dependency updates.
* ~53 documentation clarity fixes.

---

## 0.2.0

### Features

* Experimental **Session Persistence**: Detach from and reattach to editor sessions with full state preservation. Start with `fresh -a <name>` or `fresh -a` (directory-based), detach via File menu or command palette. Sessions persist across terminal disconnections. Use `fresh --cmd session list/kill/attach` and `fresh --cmd session open-file NAME FILES` to manage sessions from the command line. Allows using Fresh across other applications, e.g. yazi edit action triggers a file open in Fresh.

* **Keybinding Editor**: Full-featured editor for customizing keybindings. Search by text or record key, filter by context/source, add/edit/delete bindings with conflict detection and autocomplete. Try menus: Edit..Keybinding Editor, or command palette. Changes are saved in config.json

### Improvements

* **Line Editing**: Move lines up/down and duplicate lines, matching modern editor behavior. Multi-cursor support (@Asuka-Minato).

* **Triple-Click Selection**: Triple-click selects entire line (#597).

* **Vietnamese Localization**: Full Vietnamese (Tiếng Việt) language support.

* **Typst Language Support**: Syntax highlighting and tinymist LSP configuration for `.typ` files (#944).

* **LSP Improvements**:
  - Per-buffer LSP toggle command to enable/disable LSP for individual files
  - Default LSP configs for bash, lua, ruby, php, yaml, toml (#946)

### Bug Fixes

* **LSP Document Sync**: Fixed document corruption when LSP servers received didChange after didOpen, and when bulk edits (selection replacement, multi-cursor) bypassed LSP notifications.

* **LSP Completion Popup**: Fixed popup swallowing non-word characters, arrow keys, and other keys. Popup now dismisses correctly allowing keystrokes to pass through (#931)

* **LSP Diagnostics**: Fixed diagnostic gutter markers not appearing on implicit trailing lines with zero-width ranges (clangd-style diagnostics).

* **Line Wrapping**: End/Home keys now navigate by visual line when wrapping is enabled, matching VS Code/Notepad behavior (#979).

* **Syntax Highlighting**: Fixed highlighting lost when saving files without extension (shebang detection) outside working directory (#978).

* **Buffer Settings**: User-configured tab size, indentation, and line numbers now preserved across auto-revert.

* **Terminal Scrollback**: Any character key exits scrollback mode instead of just 'q' (#863).

* **32-bit ARM Build**: Fixed setrlimit type mismatch on ARMv7l platforms (#957).

### Configuration

* Added C++20 module extensions (.cppm, .ixx) for C++ syntax highlighting (#955).

### Documentation

* Added FreeBSD installation note (@lwhsu).

---

## 0.1.99

### Features

* **Windows Terminal Support**: Full terminal emulation on Windows using ConPTY (Windows 10 1809+). Handles PowerShell DSR cursor queries, prefers PowerShell over cmd.exe, and supports stdin piping (`type file | fresh`).

* **Text Encoding Support**: Detect and convert files in UTF-8, UTF-16 LE/BE, Latin-1, Windows-1252, Windows-1250, GBK, Shift-JIS, EUC-KR, and GB18030. Encoding shown in status bar (clickable to change). "Reload with Encoding..." command in File menu. Confirmation prompt for large files with non-resynchronizable encodings (#488).

* **Encoding Selection in File Browser**: Toggle "Detect Encoding" with Alt+E when opening files. When disabled, prompts for manual encoding selection.

* **Bundle Package Type**: New package type containing multiple languages, plugins, and themes in a single package. Shown with "B" tag in package manager.

* **Space-Separated Fuzzy Search**: Queries with spaces are now split into independent terms, all of which must match. For example, "features groups-view" now matches "/features/groups/groups-view.tsx" (#933).

### Bug Fixes

* Fixed Escape key not closing the manual and keyboard shortcuts pages (#840).

* Fixed scrollbar and mouse wheel scrolling not working with line wrap enabled.

* Fixed scrollbar thumb drag jumping to mouse position instead of following drag movement.

* Fixed AltGr character input not working on Windows (#762).

* Fixed custom themes not appearing in "Select Theme" on macOS due to incorrect config path resolution.

* Fixed LSP servers registered via plugins being disabled by default.

* Fixed language packs being installed to plugins directory instead of languages directory.

* Fixed theme changes not persisting when selecting the default theme.

* Fixed popup positioning not accounting for file explorer width (#898).

* Fixed LSP did_open sending wrong language for multi-language LSP servers.

* Fixed manual LSP start not working when LSP config was disabled; settings now sync immediately.

### Internal

* Refactored config path handling to pass DirectoryContext via call chain instead of static methods.

* Added shadow model property-based tests for TextBuffer.

* Bumped tree-sitter (0.26.5), actions/checkout (v6), actions/upload-pages-artifact (v4) (@dependabot).

---

## 0.1.98

### Features

* **File Explorer Quick Search**: Type to filter files/directories with fuzzy matching. ESC or Backspace clears the search (#892).

* **Sort Lines Command**: New command to alphabetically sort selected lines.

* **Paragraph Selection**: Ctrl+Shift+Up/Down extends selection to previous/next empty line.

* **Local Package Install**: Package manager now supports installing plugins/themes from local file paths (e.g., `/path/to/package`, `~/repos/plugin`).

* **Plugin API**: Added `setLineWrap` for plugins to control line wrapping.

### Bug Fixes

* Fixed data corruption when saving large files with in-place writes.

* Fixed UI hang when loading shortcuts in Open File dialog (#903).

* Fixed file explorer failing to open at root path "/" (#902).

* Fixed Settings UI search results not scrolling properly (#905).

* Fixed multi-cursor cut operations not batching undo correctly.

---

## 0.1.96

### Features

* **Visual Line Movement**: Up/Down arrows now move by visual lines when line wrap is enabled, matching expected behavior in wrapped text.

### Bug Fixes

* Fixed excessive filesystem polling during render, especially on remote filesystems like rclone mounts (#886).

### Packaging

* **FreeBSD Release**: FreeBSD x86_64 binaries now included in releases (#887).

---

## 0.1.95

### Bug Fixes

* Fixed data corruption issue when saving a large file multiple times (#882)

* Fixed hidden menus showing up when using left/right arrow keys to move between menus

* Fixed language pack plugins not being loaded properly in some cases

## 0.1.94

### Documentation

* **New documentation site**: @radiorambo contributed a complete restructure and build for a documentation section in the website. Kudos, awesome work!

See [getfresh.dev/docs](https://getfresh.dev/docs)

### Features

* **Event Debug Dialog**: New diagnostic tool for troubleshooting keyboard and terminal issues. Shows raw key codes and modifiers as they are received, helping diagnose keybinding problems. Access via Command Palette → "Event Debug".

* **File Explorer Keybindings**: Reorganized the keys and updated the docs. Ctrl+E now toggles focus between file explorer and editor. Ctrl+B toggles sidebar visibility. Single-click opens files without leaving explorer; double-click or Enter opens and focuses editor (#748).

### Bug Fixes

* **Case Conversion Enhancement**: To Upper (Alt+U) and To Lower (Alt+L) now automatically select the current word when no text is selected, matching common editor behavior.

* **Block Selection Copy**: Fixed Ctrl+C copying entire lines instead of the rectangular region. Block selection (Alt+Shift+Arrow) now correctly copies only the characters within the column bounds for each line.

* **Block Selection Editing**: Block selection now converts to multiple cursors for editing actions (typing, delete, backspace), enabling proper rectangular editing.

* **Dropdown Menu Position**: Fixed Help menu dropdown appearing at wrong position when Explorer menu was hidden. Menu position calculation now correctly skips hidden menus.

* **Settings Access**: Moved Settings to Edit menu and removed broken Ctrl+, keybinding which doesn't work reliably in terminals. Settings remain accessible via Edit → Settings... and Command Palette.

* **Block Selection Rendering**: Fixed double rendering of block selections that could cause visual artifacts.

### Internal

* **Remote Save Optimization**: SSH remote editing now uses recipe-based patched saves. For large files with small edits, only the changed portions are transferred instead of the entire file. A 10MB file with a 100-byte edit now transfers ~200 bytes instead of 10MB.

---

## 0.1.93

### Experimental

* **SSH Remote Editing**: Edit files on remote machines via SSH using `fresh user@host:path`. Supports password/key auth, sudo save, and file explorer integration.

### Features

* **Bracket Matching**: Highlight matching brackets with rainbow colors based on nesting depth. Configurable via `highlight_matching_brackets` and `rainbow_brackets`.
* **Whitespace Cleanup**: New `trim_trailing_whitespace_on_save` and `ensure_final_newline_on_save` options, plus manual commands.
* **Shift+Click Selection**: Extend selection to clicked position with Shift+click or Ctrl+click.
* **Terminal Mouse Forwarding**: Mouse events forwarded to terminal in alternate screen mode (vim, htop, etc.) (#853).
* **Tab Bar Scroll Buttons**: Click `<`/`>` buttons to scroll through tabs.
* **Library Files Protection**: Files outside project root are read-only and have LSP disabled.
* **Buffer Focus History**: Closing a buffer returns to previously focused buffer instead of adjacent tab.

### Bug Fixes

* **Multi-Cursor Cut**: Fixed cut not deleting all selections with multiple cursors.
* **Tab Scroll**: Fixed tab scroll buttons and active tab visibility.

### Packaging

* **AUR aarch64**: Added aarch64 support for Arch Linux ARM (#856).

### Internal

* Nix: Switched to `flake.parts`, added `shell.nix`/`default.nix` compatibility (@drupol).

---

## 0.1.90

### Features

* **Package Manager**: Browse, install, and uninstall plugins, themes, and language packs from the [official registry](https://github.com/sinelaw/fresh-plugins-registry). Features search, package validation, background registry sync with local caching, and automatic theme reloading after install.
  - **Language packs** bundle syntax highlighting (`.sublime-syntax`), language settings, and LSP server configuration
  - Filter by package type: Plugins, Themes, Languages
  - See [fresh-plugins](https://github.com/sinelaw/fresh-plugins) for example packages

* **Command Palette** (Ctrl+P): Unified prompt for navigating files, commands, buffers, and lines. Use prefix characters to switch modes:
  - No prefix: fuzzy file finder
  - `>` prefix: commands
  - `#` prefix: switch open buffers by name
  - `:` prefix: go to line number

  Includes hints line showing available prefixes and Tab completion.

* **Status Message Log**: Click status bar messages to view full message history.

* **Package Scaffolding (`--init`)**: Create new plugin, theme, or language pack projects with `fresh --init`. Interactive wizard generates package.json, entry files, and proper directory structure.

* **Theme Schema**: JSON Schema for theme validation. Use `scripts/validate-theme.sh` or any JSON Schema validator.

### Bug Fixes

* **Bracket Expansion**: Pressing Enter between matching brackets expands them with proper indentation (#629).
* **Ctrl+D Word Selection**: Ctrl+D selects the entire word when no selection exists.
* **Ctrl+Right Word Jump**: Ctrl+Right jumps to word end, matching Ctrl+Shift+Right behavior.
* **Alt+N/P Search**: Search invalidates when cursor moves manually, preventing stale matches.
* **Theme Fallback**: Falls back to default theme when configured theme is not found.
* **Cross-Platform Theme Paths**: Theme path handling works correctly on Windows.

### Internal

* Moved calculator, color-highlighter, todo-highlighter plugins to external repository (installable via package manager).
* Moved catppuccin and xscriptor themes to external repository (installable via package manager).
* Added WASM feature flag for shared editor core modules.
* Italian translation update (#839).

---

## 0.1.88

### Features

* **Status Bar Language Indicator**: Click the language name in the status bar to change syntax highlighting. Supports mouse wheel scrolling and type-to-filter.
* **VS Code-like Completion UX**: Debounced completion triggers, Tab accepts completion, uppercase letters work in type-to-filter.
* **Per-Language LSP Root URI**: LSP servers can now have per-language root URI detection. Includes automatic C# project root detection via `.csproj` files.
* **Settings UI Improvements**: Settings organized by topic sections, improved focus colors, search navigates to setting, better Map control navigation.

### Bug Fixes

* **Tab Bar Mouse Events**: Fixed clicks on tabs not working when menu bar is hidden (#832).
* **LSP Deadlock**: Fixed deadlock when LSP server sends requests while client is awaiting a response.
* **LSP Root URI**: Include `root_uri` in LSP initialize params for server compatibility.
* **Terminal Scrollback**: Fixed race condition truncating terminal backing file when PTY already wrote content.
* **Plugin i18n**: Fixed placeholder format to use `%{variable}` syntax.
* **Settings UI**: Fixed confirm dialog mouse clicks/Tab navigation, dropdown option selection, search result navigation, and content bleeding into footer.

### Packaging

* **Winget**: Added Windows Package Manager (winget) publishing to release pipeline.

### Internal

* **FileSystem Trait**: New IO abstraction layer enabling different backends (local, remote, WASM). All filesystem operations now use injectable `FileSystem` trait.

---

## 0.1.87

### Features

* **Language Support**: Added LSP configurations and syntax highlighting for Zig, Java, LaTeX, Markdown, and Templ.
* **Git File Highlighting**: Syntax highlighting for git-related files (.gitignore, .gitattributes, .gitmodules).
* **Plugin Type Safety**: TypeScript type definitions for plugin API with compile-time validation.

### Bug Fixes

* **Hover Popup**: Fixed scrolling to bottom, dismiss on click outside, block clicks inside popup.
* **Settings UI**: Fixed overwriting manual config.json edits when saving from Settings UI (#806).
* **Windows Terminal**: Fixed truecolor detection and 256-color grayscale conversion overflow.
* **Composite Buffers**: Fixed mouse click sync, deserialization errors, and cursor positioning.
* **Plugin Stability**: Plugin thread panics now propagate to main thread for proper error handling.
* **Review Diff Plugin**: Fixed side-by-side diff commands not appearing in command palette.

---

## 0.1.86

### Features

* **Popup Text Selection**: Select and copy text from LSP hover popups and tooltips. Click and drag to select, Ctrl+C to copy.
* **File Explorer Status Tooltips**: Hover over git status indicators (M, U, A) to see detailed explanations and diff stats. Directory tooltips show list of modified files.
* **Terminal Background Transparency**: New `use_terminal_bg` config option allows terminal transparency or custom backgrounds to show through the editor (#640).
* **Vi Mode Improvements**: Added `:w filename` to save to path, `:wq filename` to save and quit, `:q!` to force quit without saving. Added Ctrl+P (command palette) and Ctrl+Q (quit) to all vi modes.

### Bug Fixes

* **Settings UI Add Button**: Fixed "Add New" button not appearing for LSP and Languages maps in Settings UI.
* **LSP Hover Markdown**: Improved markdown rendering - soft breaks now create newlines (fixing Python docstring formatting), inline code rendered without visible backticks.
* **Symlink Directories**: Fixed symlinks to directories not showing expand marker and causing "Is a directory" error when opened (#787).
* **Live Grep Preview**: Fixed preview not updating when navigating through search results (#636).
* **Terminal Keyboard State**: Fixed arrow keys and Enter not working after exiting the editor due to Kitty keyboard protocol cleanup issue (#773).
* **Plugin Commands Visibility**: Fixed many plugin commands (Toggle Vi Mode, Git Blame, Diagnostics Panel, etc.) not appearing in command palette.

### UI Changes

* **File Explorer Layout**: Git status indicators moved to rightmost column, matching VS Code's layout. Removed file size and item count for cleaner appearance.
* **Quieter Startup**: Removed plugin "ready/loaded" status messages that cluttered the status bar on startup.

### Internal

* Separated I/O from pure types in theme and grammar modules for better testability and future WASM compatibility.
* Fixed workspace crate dependencies for crates.io publishing.
* Improved install.sh reliability for containers and edge cases.

---

## 0.1.83

### Breaking Changes

* **QuickJS Plugin Runtime**: Replaced Deno with QuickJS for the plugin system. Each plugin now runs in its own isolated context.

### Features

* **Cargo Workspace Architecture**: Refactored into modular crates (fresh-core, fresh-editor, fresh-languages, fresh-parser-js, fresh-plugin-runtime, fresh-plugin-api-macros).

### Bug Fixes

* **Toggle Comment YAML**: Fixed toggle comment not working for YAML files by falling back to config-based language detection (#774).
* **Undo History Panic**: Fixed panic when undoing past a save point and making new edits caused out-of-bounds slice access (#776).
* **Sudo Save Prompt**: Fixed permission denied crash when saving files owned by another user; now shows sudo prompt correctly (#775).
* **Musl Plugin Support**: Plugins now work on musl target builds (x86_64/aarch64-unknown-linux-musl).
* **LSP Server Requests**: Fixed LSP server-to-client request handling not being dispatched to plugins.
* **Git Find File Selection**: Fixed race condition causing wrong file selection when pressing Enter quickly.
* **Plugin Cache**: Embedded plugins now cached in XDG cache dir instead of leaking temp directories.

### Internal

* Improved compile times via LLVM optimization flag.
* Cross-platform path handling fixes for Windows.
* Test reliability improvements.

---

## 0.1.77

### Documentation

* **macOS Terminal Tips**: Added keyboard enhancement flags configuration guide.

### Features

* **LSP Semantic Highlighting** (@Asuka-Minato).
* **macOS Keybinding Display**: Native symbols (⌃, ⌥, ⇧) instead of Ctrl+/Alt+/Shift+.
* **Odin Language Support**: Syntax highlighting (sublime-syntax from @Tetralux) and OLS LSP configuration (@xoxorwr).
* **File Explorer Git Indicators**: Shows modified/added status for files and folders via new plugin (#526) (@Asuka-Minato).
* **Keyboard Enhancement Flags Config**: New config options for more granular control over kitty protocol usage (`keyboard_disambiguate_escape_codes`, `keyboard_report_event_types`, `keyboard_report_alternate_keys`, `keyboard_report_all_keys_as_escape_codes`).

### Bug Fixes

* **Menu Keybinding Display**: Consistent keybinding symbols in menus on macOS (#703).
* **Git Find File Popup**: Smart path truncation preserving filename (#707).
* **File Owner Preservation**: Preserve owner when saving files with group write privileges (#743).

### Internal

* Telemetry and update checks now debounce to once per day.
* Terminal mode handling refactored into dedicated module.
* Resolved ~300+ clippy warnings.
* Bumped url (2.5.8), libc (0.2.180) (@dependabot).

---

## 0.1.76

### Features

* **Anonymous Telemetry**: Basic anonymous telemetry (version, OS, terminal type) sent with update checks. Disable via `check_for_updates` config or `--no-upgrade-check` flag.
* **Toggle Tab Bar/Menu Bar**: Hide or show tab bar and menu bar via command palette or View menu (#618).
* **Plugin Enable/Disable**: New config options to enable or disable individual plugins.
* **Improved Settings UI**: Layer-aware modified indicators, column headers for Map controls, visual indication for read-only fields in Settings UI entry dialogs.
* **Git Grep Preview**: Live preview panel with debouncing for Git Grep results.

### Bug Fixes

* **Map Control Click**: Fixed "Add new" button requiring double-click instead of single click (#604).
* **File Explorer Session**: Persist `show_hidden` and `show_gitignored` settings across sessions (#569).
* **Line Numbers Config**: Respect `line_numbers` config when launching without a file argument (#539).
* **Find References UX**: Now uses prompt mode for consistent search experience.
* **i18n Placeholders**: Fixed string interpolation format in plugin translations (#706).

### Internal

* ResultsPanel abstraction with VS Code-style Provider pattern for plugin UI.
* TypeScript type checking for plugins.
* Test reliability improvements for e2e tests.

---

## 0.1.75

This is mostly a bugfix release.

### Bug Fixes

* **Prompt History**: Generic prompt history system with Up/Down navigation, now available for Go to Line and other prompts.
* **Session External Files**: Files opened from outside the project directory are now restored in sessions.
* **Fuzzy Search Exact Match Priority**: Open File dialog now prioritizes exact filename matches over fuzzy matches.
* **Horizontal Scroll**: Fixed cursor position with horizontal scroll after Open File dialog and pressing Enter on long lines.
* **Multi-Cursor Bracket Skip**: Fixed bracket skip-over with multiple cursors in bulk edit.
* **F3 Search**: Fixed F3 to allow searching more after editing and to update positions correctly after buffer modifications.
* **File Explorer**: Removed plain letter shortcuts causing accidental actions, fixed focus after rename/delete, improved new file command behavior.
* **Terminal**: Fixed scrollback colors, mouse scroll now exits to scrollback mode, fixed viewport position bugs, persist exit message.
* **Theme Editor**: Fixed reopening after closing the theme editor, allow editing builtin themes (#696), store builtin themes as json instead of hardcoded inside rust.
* **LSP Diagnostics**: Made diagnostic cache per-buffer to prevent marker position bugs.
* **Cursor Visibility**: You can see the letter under the block cursor now! Apply REVERSED style to primary cursor for better visibility.
* **Open Terminal**: Command now available in all contexts.
* **Open File Dialog**: When run while a terminal is focused, use CWD instead of the internal backing file directory.

### Internal

* Refactored reference highlighting to use overlay system (#694).
* Built-in themes now loaded from JSON artifacts at build time instead of hardcoded Rust.
* Removed duplicate dead code from LspTask.

---

## 0.1.74

### Features

* **Italian Locale**: Full Italian translation support added across the editor and all core plugins (@fdefilippo).
* **Interactive Links in Popups**: Markdown popups (such as LSP hover) now support clickable hyperlinks (OSC 8). Clicking a link opens it in your default web browser (@Asuka-Minato).
* **Sudo Save Fallback**: When saving a file fails due to insufficient permissions, the editor now offers to save using `sudo` (Linux/macOS) (#301).
* **Improved Language Features**: Improved word navigation, auto-pairs, and multi-cursor behavior.

### Bug Fixes

* **LSP Hover Reliability**: Fixed multiple issues with hover popups, including race conditions during rapid mouse movement, incorrect positioning on empty lines, and popups triggering past the end of a line.
* **Popup Scrollbar Drag**: You can now click and drag the scrollbar in popups (like hover and completion) to scroll through long content.
* **Inlay Hint Positioning**: Corrected inlay hint placement in Rust files to prevent them from shifting line content (#626, @Asuka-Minato).
* **Theme Editor Path Resolution**: Fixed a bug where the theme editor couldn't find the correct configuration directory on some systems.

### Internal

* **Error Handling**: Migrated to `anyhow` for more robust error tracking and backtraces.
* **Plugin API**: Added `editor.getConfigDir()` and `editor.getThemesDir()` to the plugin API.
* **Dependency Updates**: Bumped `clap` to 4.5.54.

---

## 0.1.71

### Features

* **Side-by-Side Diff View**: Word-level highlighting, synchronized scrolling, cursor navigation.
* **Theme Editor**: JSON Schema API, color swatches, command palette integration, delete theme.
* **Create Files from Open Dialog**: Type non-existent filename to create new buffer.
* **Tilde Expansion**: `~/path` works in Save As, Open File, Switch Project.

### Bug Fixes

* **Toggle Comment**: Use language config for comment prefixes, preserve selection, don't hang (#681).
* **Split Close**: Close split when closing last buffer instead of empty buffer.
* **Terminal**: Resume mode on buffer switch, sync content, clean up on close.
* **Hidden Buffers**: Skip in next/prev buffer, fix tab click targets.

### Internal

* Plugin i18n completeness tests. Bumped libc, tokio, tree-sitter-lua.

---

## 0.1.70

### Features

* **Input Calibration Wizard**: New wizard to calibrate keyboard input for terminals with broken key sequences. Access via "Calibrate Keyboard" in command palette or View menu. Uses failsafe ASCII-only navigation (#219).

* **Terminal Cursor Color**: Cursor color now set via OSC 12 escape sequence for proper visibility across all themes, especially light theme.

### Bug Fixes

* **Dynamic Keybinding Hints**: Status messages now show actual keybindings from keymap instead of hardcoded shortcuts (#659).

* **Search in Large Files**: Fixed "Buffer not fully loaded" error when searching in large plain text files (#657).

* **LSP Config Preservation**: Fixed LSP command field becoming empty when toggling enabled state. Partial config now merges with defaults (#630, #631).

* **Multi-Cursor End of Line**: Fixed secondary cursors rendering at line start instead of end (#632).

* **Selection at Cursor**: Fixed selection background not showing at primary cursor position with bar/underline cursor styles (#614).

* **Locale Interpolation**: Fixed locale name not appearing in "Locale changed" message (#624).

* **Cursor Past Trailing Newline**: Allow cursor to navigate to the empty line after trailing newline (#622, @Asuka-Minato).

* **.env Syntax Highlighting**: Added .env to default shell syntax patterns (#559).

* **Spanish Translation**: Fixed typo in menu bar (@osniel).

* **Audit Mode Keybindings**: Use Emacs-style key notation in diff-view bindings (@xunzhou).

### Internal

* Refactored config system to use layered PartialConfig resolution everywhere.
* Code cleanup: use `Self` where possible, merge match arms (@adamnemecek).
* Clean up log output by resetting to column zero (@Martin-Häcker).
* Bumped windows-sys to 0.61.2 (@dependabot).

---

## 0.1.69

> **macOS Users**: This release includes significant improvements for macOS terminal compatibility. See the new [macOS Terminal Tips](docs/USER_GUIDE.md#macos-terminal-tips) guide for recommended terminal emulators and keyboard configuration. The macOS keymap ([`keymaps/macos.json`](keymaps/macos.json)) is a work in progress—please submit patches based on your experience with different terminals and keyboard layouts!

### Features

* **macOS Keymap**: Terminal-friendly keybindings that avoid broken Ctrl+Shift combinations, ASCII control char collisions (Ctrl+J=LF), and international keyboard conflicts (Ctrl+Alt+L=@ on German). Key bindings: Ctrl+R (redo), Ctrl+G (find next), Ctrl+L (go to line), Ctrl+T (go to symbol), Alt+B/F (word movement). See [macOS Terminal Tips](docs/USER_GUIDE.md#macos-terminal-tips) (#219).

* **4-Level Config System**: Configuration now merges user, platform, project, and session layers. Settings UI shows layer indicators and allows editing specific config files.

* **Tab Context Menu**: Right-click tabs for Close, Close Others, Close All, Close to Right options.

* **Drag-to-Split Tabs**: Drag tabs to screen edges to create new splits.

* **Plugin Logging**: New `editor.error()`, `editor.warn()`, `editor.info()`, `editor.debug()` methods route plugin output through the editor's logging system.

* **Log Management**: Logs moved to XDG state directory with automatic 24-hour cleanup. Use `--show-paths` to see log locations.

### Experimental

*These features are work-in-progress. Expect rough edges and breaking changes.*

* **Internationalization (i18n)**: Full i18n support with 11 languages (German, French, Spanish, Japanese, Korean, Chinese, Russian, Ukrainian, Czech, Portuguese, Thai). Select locale via command palette or Settings UI. Plugins support translation via `editor.t()` and `.i18n.json` files. *Note*: Keybinding shortcuts do not take the active layout into account, which is why this feature is still experimental. Also I need you to provide feedback on the translations since they were all machine-generated and I don't speak any of the languages added.

* **Vi Mode Plugin**: Added `.` repeat command, visual block mode, and colon command mode with comprehensive vim commands (`:w`, `:q`, `:wq`, `:e`, `:split`, etc.).

* **Review Diff Plugin**: Side-by-side diff view with synchronized scrolling, line alignment, and word-level highlighting. Access via "Side-by-Side Diff" command.

### Bug Fixes

* **Tab Size Zero Panic**: Fixed division by zero when tab_size is 0 (#580).

* **Hidden Cursor Panic**: Fixed crash when rendering buffers with hidden cursors (#607, yoooughtul).

* **Settings Paste**: Fixed clipboard paste not working in Settings UI edit dialogs (#605, Tyooughtul).

* **Show Hidden Truncation**: Fixed "Show Hidden" checkbox label truncated in file dialog (#558).

* **Syntax Highlighting Config**: User-configured filename patterns now work for syntax highlighting (#565).

* **Replace All Performance**: Fixed O(n²) performance issue causing hangs on large files (#564).

* **Plugin Thread Hang**: Fixed plugin thread hanging on shutdown.

* **File Explorer Crash**: Fixed crash when scroll_offset exceeds tree size (#562).

* **Background Revert Jump**: Fixed viewport jumping when auto-reverting background files.

* **Scrollbar Gaps**: Render scrollbars with background fills to avoid glyph gaps in some terminals (Oleksii Smotrov).

### Performance

* **BulkEdit Operations**: Multi-cursor and replace-all now use O(n) algorithm instead of O(n²).

* **Semantic Highlighting**: Debounced to reduce CPU usage during rapid cursor movement.

---

## 0.1.67

### Features

* **Find Selection Next/Previous**: Search for word under cursor without opening find panel. Ctrl+F3/Ctrl+Shift+F3 or Alt+N/Alt+P (#489).

* **Cursor Style Configuration**: Configure terminal cursor style (block/bar/underline, blinking/steady) via command palette (#341).

* **Case Conversion**: Transform selected text to uppercase (Alt+U) or lowercase (Alt+L) (#522).

* **Folder Modified Indicators**: Parent folders show dot indicator when containing unsaved files (#526).

* **Line Ending Indicator**: Status bar shows LF/CRLF/CR, clickable to change. Conversion on save, configurable default (#487).

### Experimental

*These features are work-in-progress. Expect rough edges and breaking changes.*

* **LSP Helper Plugins**: Popup with install commands when LSP server not found for Python, Rust, TypeScript (#502).

* **Vi Mode Plugin**: Full vi-style modal editing with normal/insert/visual modes, operators (d/c/y), motions (hjkl, w/b/e, gg/G), text objects (iw, i", i(), etc.), counts, and find character (f/t).

* **Review Diff Plugin**: Code review for AI-generated changes or git diffs. Side-by-side view with synchronized scrolling, line comments, approve/reject/stage actions, export to Markdown/JSON.

### Bug Fixes

* **Line Numbers with Wrapped Lines**: Fixed line numbers desyncing when scrolling through wrapped lines (#552).

* **Click Past End of Line**: Now positions cursor at line end instead of next line start (#547).

* **Line Wrapping**: Fixed characters being clipped at wrap boundaries with tabs and grapheme clusters (#550).

* **Zsh Dotfiles**: .zshrc, .zprofile, .zshenv now highlighted as shell scripts (#537).

* **Cursor on Status Bar**: Fixed cursor jumping to status bar when scrolling to end of file (#468).

* **Large Single-Line Files**: Fixed memory exhaustion and 100% CPU on files like minified JSON (#481).

* **Config Editor Keys**: Fixed Delete, Home/End, Ctrl+A in JSON text box.

* **Search Term Persistence**: Alt+N/Alt+P keeps original search term when landing on longer word.

### Packaging

* **AUR**: Use stable source tarball to fix sha256sum validation failures.

---

## 0.1.65

### Features

* **Warning Indicators**: Non-intrusive warning notifications in the status bar. Click or use commands to view warnings, with domains for LSP and general warnings.

* **Format Buffer Command**: Explicit command to format the current buffer on demand.

* **Config Applied on Open**: `line_numbers` and `line_wrap` settings now properly apply when opening new buffers.

### Bug Fixes

* **Settings Persistence**: Fixed settings not persisting after save and reopen (#474, #457).

* **SaveAs Overwrite Confirmation**: Added confirmation dialog when SaveAs would overwrite an existing file (#476).

* **Multi-Byte Character Input**: Fixed panic when editing multi-byte characters in text inputs and prompts (#466).

* **TextList Dialog**: Fixed add-new input not rendering in entry dialogs.

---

## 0.1.64

* To prevent accidental deletion of files, removed 'd' / delete key bindings from File Explorer, changed the underlying delete to show a prompt and to move files to trash instead of really deleting.

## 0.1.63

### Features

* **Shell Command Prompt**: Pipe buffer or selection through shell commands (Alt+|).

* **On-Save Actions**: Run formatters/linters on save. Default formatters included for Rust (rustfmt), JavaScript/TypeScript (prettier), Python (ruff), C/C++ (clang-format), Go (gofmt).

* **Stdin Input**: Pipe content via stdin with background streaming (`echo "hello" | fresh -`).

* **Multi-File CLI**: Open multiple files from command line (#389).

* **Tab Indent Selection**: Tab indents selected lines, Shift+Tab dedents (#353).

* **Toggle Menu Bar**: Hide/show menu bar via command palette for extra screen space.

* **Global File Positions**: Cursor/scroll positions stored globally per file, not per project (#423).

* **Relative Line Numbers**: Show relative distances from cursor in gutter for easier vim-style navigation. Enable via `relative_line_numbers` config (#454).

### Bug Fixes

* **On-Save Missing Tools**: Graceful handling when formatter/linter command not found.

* **Settings UI Nested Dialogs**: Fixed nested ObjectArray navigation and save not persisting (e.g., editing on_save inside language config).

* **Live Grep Working Directory**: Fixed search plugins using process cwd instead of project working directory.

* **Open File Path Resolution**: Fixed relative paths resolving incorrectly when editor launched from different directory.

### Performance

* **Live Grep UI**: Fixed UI freezing for seconds during large codebase searches by making plugin event loop non-blocking.

### Internal

* Embedded plugins in binary as fallback for cargo-binstall (#416).

* Removed duplicate theme JSON files (#438).

* Extracted modules from mod.rs (file_operations, split_actions, clipboard, etc.).

* Pinned Rust 1.92 via rust-toolchain.toml (#338).

* Windows build switched from MSVC to GNU target.

---

## 0.1.59

### Features

* **Copy with Formatting**: Copy selected text as HTML with syntax highlighting. Works in Google Docs, Word, etc. Available via Edit menu submenu or command palette.

* **Pascal Language Support**: Auto-indentation and semantic highlighting for `.pas` and `.p` files (@casibbald).

* **Set Line Ending Command**: Change buffer line ending format (LF/CRLF/CR) via command palette.

* **Buffer Settings Commands**: Toggle auto_indent, use_tabs, and tab_size via command palette.

* **Settings UI**: Recursive dialog stack for nested arrays/maps, focus indicators, Ctrl+S to save, select-all on number input edit.

### Bug Fixes

* **Tab Size Config**: Fixed tab_size config not being respected (#384).

* **Windows Multi-Line Paste**: Fixed CRLF paste appearing as single line (#427).

* **CRLF Highlighting**: Fixed syntax highlighting offset drift in CRLF files.

* **CRLF Cursor**: Fixed cursor invisible at end of line in CRLF mode.

* **Menu Navigation**: Keyboard navigation now skips disabled items.

* **Cut/Copy Disabled**: Menu items grayed out when no selection.

### Internal

* Extracted CRLF helpers, consolidated TextMateHighlighter into TextMateEngine.

* Updated insta (1.45.0), deno_core (0.376.0).

---

## 0.1.57

### Bug Fixes

* **External Paste with Prompts**: Fixed paste via terminal (Ctrl+Shift+V / bracketed paste) going to editor instead of open prompt (#406).

* **Block Selection Escape**: Fixed Escape key not canceling block selection mode (#405).

* **CRLF Line Endings**: Fixed CRLF handling to preserve original line endings. Enter inserts correct line ending, End key positions before \r\n, backspace/delete treat \r\n as single unit (#401).

* **RPM Package**: Fixed /usr/bin/fresh entry missing from RPM package manifest.

* **Settings Percentage Values**: Fixed percentage settings saving as integers instead of floats.

* **Windows Unicode**: Fixed unicode character not supported on Windows (#400).

### Packaging

* **AUR Source Package**: Fixed sha256sum not being updated when publishing.

* **Nix Flake**: Fixed missing sublime-syntax grammar files in source filter.

* **Flatpak/AppImage**: Strip binaries before bundling for smaller package sizes.

### Internal

* **Test Reliability**: Fixed flaky e2e tests on macOS by removing timing sensitivity.

* **Release Workflow**: Added package upgrade tests and nix build test.

---

## 0.1.56

### Features

* **Per-Language Tab Settings**: Added `use_tabs` and `show_whitespace_tabs` config options per language. Go and Makefile default to tabs (#364).
* **AppImage Packaging**: AppImage bundles now included in GitHub releases (#365).
* **Terminal Color Detection**: Auto-detection of terminal color capabilities with fallback to 256 colors. Override via `FRESH_COLOR_MODE`.
* **TOML Syntax Highlighting**: Added embedded TextMate grammar for TOML files.
* **Language Detection by Filename**: Detect languages by filename (`.bashrc`, `Makefile`, `Dockerfile`, etc.) (#383).
* **Minimal Config Saves**: Config file only saves non-default fields.
* **Settings UI**: Mouse click/double-click support, hover effects, improved scrolling.

### Bug Fixes

* **LSP**: Improved error messages when server not found (#363). Fixed didOpen ordering (#399). Check diagnosticProvider capability before pull diagnostics (#399).
* **Terminal Mode Reset**: Fixed terminal_mode not being reset when closing a terminal buffer.
* **cargo-binstall**: Fixed missing binaries warning (#388).
* **macOS Keybinding Display**: Fixed showing ⌘ instead of Ctrl (#356).
* **tmux Truecolor**: Fixed detection when `COLORTERM=truecolor` is set.
* **RPM Upgrade**: Fixed upgrade failing when older version installed (#387).

## 0.1.54

### Features

* **Universal Install Script**: New `install.sh` script for easy installation across Linux and macOS.

* **Settings UI Enhancements**:
  - Entry dialogs for editing Languages, LSP servers, and keybindings
  - Schema-driven dialog builder with automatic field generation
  - Dimming effect for modal dialogs
  - Column-aligned controls for cleaner layout
  - Setting descriptions now displayed inline
  - Map controls with flat navigation, entry highlighting, and delete buttons

* **LSP Hover Improvements**: Hover popups now persist when moving within a symbol or hovering over the popup itself. Popups dismiss on focus loss.

* **Replace History**: Search & replace now supports history navigation for the replace field.

### Bug Fixes

* **Paste with Selection**: Fixed paste not replacing selected text - previously inserted without deleting selection.

* **Multi-Cursor Paste**: Fixed paste only working at primary cursor - now pastes at all cursor positions.

* **Bracketed Paste**: Enabled bracketed paste mode for proper handling of external paste (Ctrl+Shift+V). External pastes now arrive as single atomic events instead of character streams.

* **Settings Input Isolation**: Fixed keyboard input leaking between Settings UI panels.

* **Map Control Buttons**: Fixed [+] Add new buttons not working for Map controls.

* **File Browser Navigation**: Fixed input routing issues in file browser modal.

* **Config Loading**: Fixed config not loading from working directory; changes now apply to runtime state immediately.

### Configuration

* **rust-analyzer Defaults**: Added minimal performance defaults for rust-analyzer LSP.

### Internal

* **Input Handling Refactor**: New hierarchical `InputHandler` trait system for cleaner modal input routing.

* **Component Pattern**: Refactored all Settings UI controls (Button, Toggle, NumberInput, TextInput, Dropdown, TextList, MapInput, KeybindingList) to consistent component pattern.

* **Config Module**: Consolidated config path resolution and loading into `config_io` module. Config editor now saves only non-default values.

* **Code Organization**: Extracted action handlers into dedicated modules (menu_actions, lsp_actions, prompt_actions, undo_actions, mouse_input).

---

## 0.1.52

### Bug Fixes

* **musl Build**: Enabled the `runtime` feature for musl builds.
* **Flatpak**: Fixed CI and metainfo handling (official Flathub container + flatpak-builder action, appstream-compose deps, avoid corrupting XML declaration, remove invalid `launchable` tag).

### Internal

* **Version Bump Script**: Version bumps now skip `cargo check`.

---

## 0.1.45

### Features

* **Settings UI**: New graphical settings editor accessible via View menu or command palette. Features:
  - Two-panel layout with categories on left and settings on right
  - Fuzzy search to quickly find settings
  - Full keyboard navigation (Tab cycles through panels, arrow keys navigate items)
  - Mouse support with scrolling, scrollbar dragging, and hover indicators
  - Dropdown, number input, text list, and map editing controls
  - Reset to default functionality for individual settings
  - Confirmation dialog when discarding unsaved changes
  - Help overlay showing keyboard shortcuts

* **Default/Reset Color Support**: Theme colors can now use "Default" or "Reset" values for terminal transparency. The theme editor plugin shows these special colors with a "∅" placeholder swatch. Terminal background and foreground can inherit from the user's terminal emulator settings.

* **Flatpak Packaging**: Added Flatpak support for Linux installation (#340). Flatpak bundles are now included in releases.

### Bug Fixes

* **File Permissions Loss on Save**: Fixed file permissions/mode bits being lost when saving files (#329). Executable scripts and other special permissions are now preserved.

* **Polling File Watcher**: Replaced inotify/FSEvents-based file watching with a simple polling approach (#321). This fixes "too many open files" errors on large projects. Configurable via `auto_revert_poll_interval_ms` (default 2s) and `file_tree_poll_interval_ms` (default 3s).

* **Terminal Input Capture**: Fixed terminal capturing keyboard input when the Settings UI is opened while a terminal split is focused.

* **Search Result Scrolling**: Fixed settings UI not scrolling to show selected search results.

### Configuration

* **Memory Limit**: Changed `max_memory_mb` to `max_memory_percent` (default 50%) for consistent behavior across machines with different RAM.

### Packaging

* **AUR**: Updated package names to match conventions (fresh-editor vs fresh-editor-bin). Added `--syncdeps` to makepkg commands (#343).

### Internal

* **TimeSource Abstraction**: Added TimeSource trait for testability, making time-dependent behavior deterministic in tests (issue #314).

* **Test Reliability**: Replaced thread::sleep with testable time source in e2e tests. Fixed flaky tests on macOS and Windows.

* **Dependency Updates**: Updated deno_core, deno_error, actions/upload-artifact, actions/download-artifact, and actions/setup-node.

---

## 0.1.44

### Features

* **Double-Click Word Selection**: Double-click now selects the word under the cursor. Both clicks must be at the same position within the configurable time threshold (`double_click_time_ms`, default 500ms).

* **Multi-Byte Character Support**: Full support for CJK characters, emoji, and other double-width Unicode characters. Includes correct visual width calculation, cursor positioning, mouse click handling, line wrapping, and display across all UI components (status bar, tabs, file explorer, suggestions). (reported by @pm100)

* **Nix Flakes Support**: Added Nix flakes for reproducible builds and development. Includes crane-based Rust caching, dev shell with toolchain and dependencies, checks for clippy/tests/formatting, and direnv integration.

### Bug Fixes

* **Mouse Escape Codes After Panic**: Fixed mouse control codes littering the terminal after a crash by disabling mouse capture in the panic handler (#311, reported by @rluvaton).

* **Hover Popup Screen Edge**: Fixed panic when LSP hover popup appears near the edge of the screen.

* **File Explorer Click Focus**: Fixed typing not working after clicking on empty area in the file explorer and then clicking back on the editor.

### Infrastructure

* **npm Publish Workflow**: Consolidated npm publishing into a single workflow that works both standalone and when called from release.yml.

### Credits

Thanks to @blissartt, @dvchd, @jakoss, @pm100, @rluvaton, @sottey, and @Yousa-Mirage for bug reports, suggestions, and contributions.

---

## 0.1.40

### Features

* **Switch Project Command**: New "Switch Project" command (renamed from "Open Folder") to change project root with full context switch. Sessions are automatically saved and restored when switching projects, preserving open files, cursor positions, and split layouts.

* **Nested Submenu Support**: Menus now support nested submenus with proper arrow indicators and positioning.

* **Select Keybinding Map Command**: New popup selector to choose between different keybinding schemes.

* **Double-Click in File Dialog**: Can now double-click to open files in the file open dialog.

* **File Explorer UX Improvements**:
  - Ctrl+E now focuses the file explorer instead of toggling it
  - File explorer automatically focuses when closing the last tab
  - Menu checkboxes properly sync with file explorer visibility state

* **Split Auto-Close**: Closing the last tab in a split now automatically closes the split.

### Bug Fixes

* **Mouse Click Below Last Line**: Fixed mouse click below the last line incorrectly jumping to position 0,0.

* **Menu Checkbox Sync**: Fixed View menu checkboxes not syncing with file explorer visibility state.

* **Duplicate Buffer on Project Switch**: Fixed duplicate buffer creation when switching projects.

* **Wrong Upgrade Tip**: Fixed incorrect upgrade tip message (#293).

### Infrastructure

* **Build System Overhaul**: Replaced cargo-dist with direct cargo builds and custom packaging for more control over the release process.

* **npm OIDC Publishing**: Improved npm publish workflow with OIDC trusted publishing and provenance attestations.

* **GitHub Actions Updates**: Bumped actions/checkout to v6, actions/upload-artifact to v5, actions/download-artifact to v6, and actions/setup-node to v6.

* **Test Improvements**: Many test reliability improvements including Windows compatibility fixes, flaky test fixes, and better test isolation for session persistence tests.

---

## 0.1.35

### Features

* **XDG Config Paths**: Support standard XDG config paths for user configuration. On macOS, `~/.config/fresh/config.json` is now prioritized if it exists, in addition to the system default path. (@Yousa-Mirage)

### Packaging

* **cargo-binstall**: Added cargo-binstall as an installation method in documentation. (@dvchd)

* **npm OIDC Publishing**: Switched npm publish to OIDC trusted publishing with provenance attestations.

---

## 0.1.28

### Features

* **Integrated Terminal**: Full terminal emulation using alacritty_terminal. Open a terminal split with "Open Terminal" command, run shell commands, and interact with TUI applications. Supports:
  - Keyboard capture mode (F9) for sending all keys to terminal
  - Scrollback history with file-backed storage
  - Session persistence - terminals restore across editor restarts
  - Paste support (Ctrl+V)
  - Click to focus terminal splits
  - Auto-restore terminal mode when switching back to terminal tabs
  - Dimmed UI indication when keyboard capture is active

* **Mouse Hover for LSP**: Hover over symbols to see LSP hover information (type info, documentation). Configurable delay before showing hover popup.

* **Toggle Maximize Split**: New command to maximize/restore the current split view.

* **Close Tab Command**: New command to close a tab without closing the underlying buffer.

* **C# Language Support**: Added C# language configuration with LSP support (csharp-ls or csharp-language-server) and auto-indent. Includes proactive `dotnet restore` on C# file open.

* **Config Editor Improvements**: New `getConfig`/`getUserConfig` plugin APIs. Config editor now properly merges user config with defaults for LSP and languages sections. Timestamped backups created before saving config.

* **LSP Menu**: New LSP menu in menu bar with common LSP actions. Menu items are disabled when LSP server is not ready.

* **Common LSP Keybindings**: Added default keybindings for common LSP operations.

* **C/C++ Language Support**: Added C and C++ language configurations to defaults.

### Bug Fixes

* **LSP Focus Stealing**: Fixed LSP error and warning buffers stealing focus from the active buffer.

* **Terminal Scrollback**: Fixed multiple issues with terminal scrollback not being captured, restored, or displayed correctly after session restore and mode toggles.

* **Terminal View Following**: Fixed terminal view not following output when at the bottom of the screen.

* **Config Editor**: Fixed config editor saving null instead of user changes. Fixed undefined defaultValue reference.

* **Duplicate LSP didOpen**: Fixed duplicate didOpen notifications being sent to strict LSP servers.

* **LSP didChange Race**: Fixed LSP didChange notification being sent before didOpen.

### Internal

* **Musl Builds**: Added musl builds without plugins for fully static Linux binaries.

* **Plugin Build Flag**: Added cargo feature (`no-plugins`) to disable plugins at the dependency level, reducing binary size and startup time.

* **Test Organization**: Moved plugin-related and LSP find_references tests to dedicated plugins directory.

* **Test Reliability**: Fixed flaky e2e tests, skipped platform-specific tests on Windows/macOS where appropriate.

* **Terminal Architecture**: Implemented incremental streaming architecture for terminal scrollback with PTY logging and file-backed buffers.

---

## 0.1.27

### Features

* **Update Checker**: Automatically checks for new versions periodically (every 24 hours) and on quit, showing a notification when updates are available.

* **Diagnostics Panel**: New diagnostics panel plugin showing all errors/warnings in a dedicated split view. Opens in horizontal split, auto-updates on buffer change, and syncs cursor position with F8/Shift+F8 navigation. Includes help line with keybinding hints.

* **Diagnostics API**: New plugin API for accessing LSP diagnostics programmatically.

* **LSP Initialization Options**: Added support for `initialization_options` in LSP server configuration.

* **Warning Log Layer**: Captures WARN+ level logs to a file and can open them in the editor for debugging.

* **Plugin Hook**: Added `cursor_moved` hook for plugins to respond to cursor position changes. Standardized hook naming to use underscores.

### Bug Fixes

* **UTF-8 Status Bar**: Fixed panic when truncating status bar text mid-character.

* **Session Restore**: Fixed session restore when a plugin buffer was the active buffer.

* **Viewport Sync**: Fixed viewport sync issues after SplitViewState refactoring.

* **LSP Null Response**: Treat null LSP response as valid result instead of error.

* **LSP Auto-Start**: Persist LSP auto-start setting when manually stopping the server.

* **Safe String Slicing**: Use safe string slicing in get_text_to_end_of_line to prevent panics.

### Internal

* **SplitViewState Refactoring**: Made SplitViewState authoritative for viewport state.

* **Default Log Path**: Use system temp directory for default log file path.

* **Test Reliability**: Fixed flaky tests on macOS and Windows, improved diagnostics panel tests.

* **Dependency Updates**: Updated deno_core, schemars, libloading, and GitHub Actions dependencies.

* **Documentation**: Added macOS plugin location information, documented reloadConfig plugin API.

---

## 0.1.26

### Bug Fixes

* **aarch64 Build**: Fixed build on aarch64 Linux by enabling v8_use_custom_libcxx.

---

## 0.1.25

### Features

* **GPM Mouse Support**: Added mouse support in Linux virtual consoles (TTY) via the GPM daemon (#231). Uses dlopen to load libgpm.so at runtime, so the binary works on systems without GPM installed. Gracefully falls back to standard terminal mouse protocol when GPM is unavailable.

* **Configurable Highlight Context**: Syntax highlighting lookback/lookforward is now configurable via `highlight_context_bytes` in config (default increased from 1KB to 10KB). Fixes inaccurate highlighting when viewing the middle of files with long multi-line constructs.

### Bug Fixes

* **Mouse Wheel After Keyboard**: Fixed mouse wheel scroll not working in main editor after keyboard navigation (#248).

### Internal

* **Reduced Logging**: Reduced verbose debug logging in default config.

* **Signal Handling**: Removed ctrlc dependency, use nix sigaction directly.

* **Test Reliability**: Fixed flaky auto-revert tests on macOS (FSEvents latency) and filesystems with 1-second mtime granularity.

* **Dependency Updates**: Reduced and updated dependencies.

---

## 0.1.24

### Bug Fixes

* **Windows Build**: Fixed Windows build compatibility.

---

## 0.1.23

### Bug Fixes

* **Split Close Tab Preservation**: Fixed tabs not being preserved when closing a split.

### Performance

* **Diff Optimization**: Optimized diff_since_saved with two-phase algorithm.

---

## 0.1.22

### Features

* **CLI file:line:col**: Support `file:line:col` format on CLI command (#217).

* **LSP Error Logging**: LSP stderr is now piped to a file and opened as read-only buffer on error.

* **Config Languages**: Use config languages section for LSP language detection.

### Bug Fixes

* **TypeScript Highlighting**: Fixed TypeScript syntax highlighting by falling back to tree-sitter.

* **Plugin Race Condition**: Fixed race condition in plugin hooks reading stale state snapshot.

* **Long Path Truncation**: Truncate long paths in Open File prompt with styled [...].

* **Graceful Shutdown**: Prevent spurious LspError on graceful shutdown.

### Internal

* **Syntect for Highlighting**: Use syntect for syntax highlighting, retain tree-sitter for other features (#237).

---

## 0.1.21

### Packaging

* **AUR Package**: Added AUR package automation and installation instructions.

* **npm and crates.io**: Added automated npm and crates.io publishing.

---

## 0.1.20

### Features

* **Theme Editor Plugin**: New interactive theme editor for customizing colors. Allows editing all theme color values with a visual interface.

* **Drag-to-Select Mouse Support**: Click and drag to select text, similar to graphical editors.

* **Homebrew Distribution**: Preliminary setup for Homebrew distribution on macOS.

### Bug Fixes

* **File Open Dialog**: Fixed handling of pasted paths in the file open dialog. Previously pasting a full path would fail; now it correctly opens the file or navigates to the directory.

* **Mouse Click on Wrapped Lines**: Fixed mouse click positioning not working correctly on wrapped lines and empty lines.

### Packaging

* **Linux Packages**: `.deb` and `.rpm` packages are now available for Debian/Ubuntu and Fedora/RHEL distributions respectively.

* **Homepage**: Set official homepage to https://sinelaw.github.io/fresh/

---

## 0.1.19

### Packaging

* **Linux packages fix**: Fixed `.deb` and `.rpm` packages not being included in GitHub releases.

---

## 0.1.18

### Features

* **Auto-load user config**: Startup now loads the default config file (e.g. `~/.config/fresh/config.json`) so themes and preferences persist without needing `--config`.
* **Clearer confirmation prompts**: Destructive prompts now use action verbs (revert, overwrite, discard) instead of generic y/n, reducing misclicks.

### Bug Fixes

* **UTF-8 safe deletion**: Backspace/Delete operate on full Unicode characters (emojis, accented letters, currency symbols) instead of raw bytes.

### Packaging

* **Deb/RPM artifacts**: Release workflow now builds stripped `.deb` and `.rpm` packages for x86_64 and aarch64, with matrixed install/uninstall tests across Ubuntu 22.04/24.04, Debian 12, Fedora 39/40, and Rocky Linux 9.

---

## 0.1.15 - Unreleased

### Features

* **TextMate Grammar Support**: Syntax highlighting now uses TextMate grammars via syntect for languages without tree-sitter support. Includes proper highlighting for Markdown (headings, bold, italic, code, links, quotes, lists).

* **Fuzzy Matching**: Command palette and file browser now use fzf-style fuzzy matching. Matches are highlighted and scored by consecutive characters, word boundaries, and match position.

* **Tab Navigation Commands**: New commands "Go to Next Tab" and "Go to Previous Tab" in the command palette for keyboard-driven tab switching.

* **File Recovery**: Emacs-style auto-recovery for unsaved changes. Buffers are automatically saved every 2 seconds to `~/.local/share/fresh/recovery/`. On startup, automatically recovers unsaved changes from crashed sessions. Uses chunked storage for large files to avoid memory issues.

* **Explorer Menu**: New menu bar entry with file explorer actions (New File, New Folder, Rename, Delete) and keybindings. Disabled items shown in theme colors when not applicable.

* **File Explorer Rename**: Press F2 or use Explorer menu to rename files/folders. Project root is protected from renaming.

* **Emacs-Style Readline Bindings**: Added terminal key equivalents for common operations:
  - Ctrl+A: Home (beginning of line)
  - Ctrl+E: End (end of line)
  - Ctrl+K: Kill to end of line
  - Ctrl+U: Kill to beginning of line
  - Ctrl+W: Kill word backward
  - Alt+D: Kill word forward
  - Ctrl+Y: Yank (paste from kill ring)

### Bug Fixes

* **Multi-Cursor Selection**: Fixed Ctrl+D selection replacement not working correctly (issue #210).

* **LSP Auto-Restart**: Fixed stopped LSP server incorrectly auto-restarting on edit.

* **File Explorer Selection**: Fixed selection being lost after rename completes.

* **Markdown Highlighting**: Fixed markdown files not getting syntax highlighting for headers, bold, italic, links, etc.

### Performance

* **Recovery Write Performance**: Removed sync_all from recovery writes, reducing disk I/O overhead.

* **Large File Recovery**: Chunked recovery format applies edits directly without loading entire file into memory.

---

## 0.1.14

See git history for changes.

---

## 0.1.13

### Features

* **Git Gutter Plugin**: Shows git diff indicators in the gutter for lines changed vs HEAD:
  - │ (green): Added line
  - │ (yellow): Modified line
  - ▾ (red): Deleted line(s) below

* **Buffer Modified Plugin**: Shows unsaved changes with │ (blue) indicators for lines modified since last save.

* **Line Indicator System**: New plugin API for gutter indicators with automatic position tracking. Indicators use byte-position markers that shift automatically when text is inserted/deleted. Priority system allows multiple indicator types to coexist (diagnostics > git > buffer modified).

* **LCS-Based Line Diff**: Buffer modified indicators now use the classic LCS (Longest Common Subsequence) algorithm - the foundation of Unix diff - for accurate change detection. Correctly handles insertions without marking shifted lines as changed, and detects deletion points.

* **Content-Based Diff**: Diff comparison now uses actual byte content rather than piece tree structure. This means if you delete text and paste it back, the indicator correctly clears because the content matches the saved state.

### Bug Fixes

* **Save As Undo History**: Fixed undo history being cleared after Save As due to auto-revert triggered by file watcher detecting the newly created file. Uses optimistic concurrency with mtime comparison to avoid spurious reverts.

* **Save As Dirty State**: Fixed undo dirty state not being tracked correctly after Save As on unnamed buffers (issue #191).

### Performance

* **Large File Mode**: Diffing is now disabled in large file mode for performance. Uses the simpler is_modified() flag instead of expensive diff calculations for files with >10MB or unknown line counts.

---

## 0.1.12

### Features

* **Live Grep Plugin**: Project-wide search with ripgrep integration and live preview. Search results update as you type (minimum 2 characters), with a split pane showing file context and syntax highlighting. Press Enter to open file at location, ESC to close preview.

* **Calculator Plugin**: Scientific calculator with clickable buttons and keyboard input. Supports parentheses, exponents (^), sqrt, ln, log, trig functions, pi, and e. Mouse click/hover support, copy button for results, and ANSI-colored UI with Unicode box drawing. ESC to close, DEL to clear.

* **File Explorer Improvements**:
  - Shows file sizes (KB/MB/GB) and directory entry counts
  - Close button (×) in title bar to hide explorer
  - Left arrow on file/collapsed directory selects parent
  - Keybinding changed from Ctrl+B to Ctrl+E (avoids tmux conflict)

* **Split View Close Buttons**: Split views now show a × button on the right side of the tab row (only when multiple splits exist) for easy closing.

* **Close Last Buffer**: Closing the last buffer now creates a fresh anonymous buffer instead of blocking with "Cannot close last buffer".

* **Alt+W Keybinding**: New shortcut to close the current tab.

* **Command Palette Source Column**: Shows where each command comes from - "builtin" or the plugin filename - in a right-aligned column.

* **Relative Buffer Names**: Buffer display names are now shown relative to the working directory.

### Bug Fixes

* **File Explorer Toggle**: Fixed Ctrl+B/Ctrl+E toggle not working correctly - now properly opens/closes instead of just focusing.

* **Session Restore**: Fixed file explorer not initializing when restoring a session with explorer visible.

* **Open File Popup**: Hide status bar when file browser popup is shown; improved high-contrast theme colors (cyan instead of yellow).

---

## 0.1.11

See git history for changes.

---

## 0.1.10

### Features

* **Session Persistence**: Automatically saves per-project state (open files, tabs, split layout, cursor/scroll positions, file explorer state, search/replace history and options, bookmarks) to the XDG data dir and restores it on launch. Session restore is skipped when opening a specific file; use `--no-session` to start fresh.

* **Unified Search & Replace**: Replace (Ctrl+H) and Query Replace (Ctrl+Shift+H) now share the same interface with a "Confirm each" toggle (Alt+E). Query Replace enables confirmation by default; Replace uses the toggle state. Confirmation prompt shows `(y)es (n)o (a)ll (c)ancel` options.

### Bug Fixes

* **Session Restore Reliability**: Fixed session rehydration to reopen files/splits with the correct active buffer, cursor, and scroll position (including nested splits) instead of jumping back to the top on first render.

* **macOS Build**: Fixed Linux-specific `.init_array` by using cross-platform V8 initialization.

* **Syntax Highlighting**: Fixed invisible/hard-to-read highlighting in light and nostalgia themes by using theme-based color resolution instead of hardcoded colors.

* **Theme Colors**: Improved status bar and prompt colors across all themes (dark, high-contrast, light, nostalgia).

* **Search Prompt**: Search/replace prompts now cancel when focus leaves the editor (switching buffers or focusing file explorer).

---

## 0.1.9

### Features

* **Native File Browser**: New built-in file browser for Open File command (Ctrl+O) that works without plugins. Features sortable columns (name, size, modified), navigation shortcuts (parent, home, root), filtering with grayed non-matches, mouse support with hover indicators, and async directory loading.

* **CRLF Line Ending Support**: Transparent handling of Windows-style line endings. Files are detected and normalized internally, then saved with their original line ending format preserved.

* **CLI Enhancements**: Added `--version`, `--no-plugins` (skip JS runtime for faster startup), `--log-file`, and `--config` flags.

* **UI Improvements**:
  - Tab hover effects with close button changing to red on hover
  - Menu hover-to-switch when a menu is open
  - Buffer name shown in modified buffer confirmation prompts
  - Fixed column widths in command palette for stable layout

### Bug Fixes

* **V8 Segfault**: Fixed crash when creating multiple Editor instances (e.g., in tests) by initializing V8 platform once at library load.

* **Windows**: Fixed duplicate key presses caused by processing both Press and Release events.

---

## 0.1.8

### Bug Fixes

* **Open File Prompt**: Fixed completions not showing immediately (issue #193) by enabling ICU support for Unicode functions.

* **Keyboard Shortcuts Help**: Fixed crash when reopening keyboard shortcuts buffer (issue #192).

* **Undo Save Points**: Fixed extra undo step at beginning of save history (issue #191).

* **Scroll Keybindings**: Fixed Ctrl+Up/Down scroll not working by syncing viewport between SplitViewState and EditorState.

---

## 0.1.7

### Features

* **Select Theme Command**: New theme picker accessible from the command palette and View menu. Includes a new "nostalgia" theme inspired by Turbo Pascal 5 / WordPerfect 5.

* **Compose Mode Improvements**: Paper-on-desk visual effect with desk margin colors, and hanging indent support for markdown lists and blockquotes.

* **Binary File Detection**: Binary files are now detected and opened in read-only mode to prevent accidental corruption.

### Bug Fixes

* **Light Theme**: Fixed colors for status bar, prompt, scrollbar, tabs, and file explorer to use proper light theme colors.

* **Mouse Performance**: Fixed slow mouse movement on large terminals by skipping redundant renders when hover target hasn't changed. Added mouse event coalescing to skip stale positions.

* **UTF-8 Truncation**: Fixed panic when truncating suggestion descriptions mid-character.

### Internal Changes

* **Code Refactoring**: Major cleanup extracting helpers and reducing duplication across many modules including `process_async_messages`, `handle_plugin_command`, `render_view_lines`, `multi_cursor`, `highlight_color`, and more. Consolidated duplicate `hook_args_to_json` implementations.

* **Test Improvements**: Fixed flaky tests by removing timing assertions, made shortcut tests platform-aware for macOS.

* **Documentation**: Reorganized internal planning docs, updated plugin README from Lua to TypeScript, and added embedded help manual using `include_str!()`.
