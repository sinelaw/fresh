# Release Notes

## 0.1.66

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
