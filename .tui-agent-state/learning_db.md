# Fresh TUI Editor — Knowledge Base

## Application Overview
- **Name:** Fresh — a modern terminal text editor
- **Binary:** `/opt/node22/bin/fresh` (installed via npm @fresh-editor/fresh-editor)
- **Version:** 0.3.9
- **Launch:** `fresh [file]` — opens with optional file argument
- **Session Persistence:** Fresh restores all previously open buffers on next launch (built-in session memory)

## tmux Session Setup
- Session: `fresh-tui-test`
- Size: 220 columns × 50 rows
- Create: `tmux new-session -d -s fresh-tui-test -x 220 -y 50`
- Kill: `tmux kill-session -t fresh-tui-test`
- Send keys: `tmux send-keys -t fresh-tui-test 'text' Enter`
- Read output: `tmux capture-pane -t fresh-tui-test -p`
- Read with colors: `tmux capture-pane -t fresh-tui-test -p -e`

## Critical tmux send-keys Notes
- Spaces in `send-keys` arguments are interpreted as KEY SEPARATORS
  - BAD: `tmux send-keys -t s "Split H"` → sends "Split" (invalid key) and "H" (capital H)
  - GOOD: `tmux send-keys -t s 'S' 'p' 'l' 'i' 't' ' ' 'H'` → sends each char individually
- Special key names: Enter, Escape, BSpace (backspace), Space, Up/Down/Left/Right, Home/End
- Ctrl keys: `C-a`, `C-p`, `C-z`, etc.
- Alt keys: `M-w`, `M-a`, etc.  
- Shift+arrow: `S-Down`, `S-Up`, `S-Left`, `S-Right`
- F-keys: `F1`, `F3`, `F10` — but F10 reliability is timing-dependent (can insert `[21~]` escape sequence)

## Menu Bar Navigation
- **F10** — opens File menu (first menu)
- **Right/Left** — navigate between menus while one is open
- **Down/Up** — navigate within a menu (WARNING: Down from File menu switches to Edit; can confuse)
- **Escape** — close menu
- **Enter** — execute menu item
- Menu order: File | Edit | View | Selection | Go | LSP | Help
- Menu navigation with 3+ separate Right presses correctly navigates: File→Edit→View→Selection

## Key Bindings (Confirmed)
### File Operations
- **Ctrl+N** — New File (new empty buffer tab)
- **Ctrl+O** — Open File (file picker with browser + sizes/timestamps)
- **Ctrl+S** — Save (prompts Save As for new buffers)
- **Ctrl+Q** — Quit (no prompt for unsaved new buffers)
- **Alt+W** — Close Tab (but ALSO toggles whole-word search depending on context!)
- **Close Buffer** (command palette) — prompts save/discard for modified buffers

### Save/Discard Dialog
- Prompt: `'[filename]' modified. (s)ave, (d)iscard, (C)ancel? `
- Input: Type letter + Enter (NOT single keypress)
- 's' + Enter → save
- 'd' + Enter → discard
- 'C' + Enter or Escape → cancel

### Editing
- **Ctrl+Z** — Undo (single character/action granularity)
- **Ctrl+Y** — Redo
- **Ctrl+X** — Cut
- **Ctrl+C** — Copy (shows "Copied" status)
- **Ctrl+V** — Paste
- **Ctrl+A** — Select All
- **Ctrl+W** — Select Word (at cursor)
- **Ctrl+L** — Select Line
- **Ctrl+D** — Add cursor at next match (multi-cursor)
- **Ctrl+/** — Toggle Comment (works on code files; no effect on plain text)
- **Ctrl+Alt+↑/↓** — Add cursor above/below
- **Alt+Shift+I** — Add cursors to line ends

### Navigation
- **Ctrl+G** — Go to Line (prompt at bottom: type number + Enter)
- **Ctrl+Right/Left** — Word movement
- **Shift+Down/Up/Left/Right** — Selection extend
- **Ctrl+PgDn / Ctrl+NPage** — Next Buffer/Tab
- **Ctrl+PgUp / Ctrl+PPage** — Previous Buffer/Tab
- **Home/End** — Beginning/End of line
- **F3** — Find Next (uses last search)
- **Shift+F3** — Find Previous

### Search & Replace
- **Ctrl+F** — Find (bar at bottom with options)
  - **Alt+C** — Toggle Case Sensitive
  - **Alt+W** — Toggle Whole Word  
  - **Alt+R** — Toggle Regex
  - **Enter** — Jump to current match and CLOSE search bar
  - **F3** — Next match WITHOUT closing search bar
- **Ctrl+Alt+R** — Query Replace
  - First field: search term (Enter to advance)
  - Second field: replacement (Enter to start)
  - Per-match: y=yes, n=skip, a=all remaining, c=cancel
- **Alt+A** — Search and Replace in Project (cross-file)

### Views
- **Ctrl+B** — Toggle File Explorer (sidebar with directory tree)
- **F10** — Access menu bar
- **Ctrl+`** / **Alt+`** — Open Terminal in Utility Dock

### Command Palette
- **Ctrl+P** — Open Command Palette
  - Default mode: `>command` (command search)
  - Delete `>` → `file` mode (fuzzy file search)
  - `:` → line mode
  - `#` → buffer mode
  - Type characters with spaces: use single-char `send-keys` format
  - **`#` buffer switcher lists virtual buffers (Run #39, #2373 fix, v0.4.1):** the `#buffer` mode now lists VIRTUAL buffers (plugin panels: `*blame:<file>*`, `*Keyboard Shortcuts*`, `*Git Log:*`, etc.) alongside file buffers — previously only file-backed buffers appeared. File buffers render with their fs path (`name  /abs/path`); virtual buffers render name-only (no path). Fuzzy-filterable by name (`#blame`), highlighted row `48;5;25m`, Enter navigates to it. To open the switcher over tmux: `C-p` then `C-u` (clear the leading `>`) then `#`. NOTE the bottom prompt + result panel appear BELOW the status bar — `capture-pane -p | tail` to see them. Verified general across plugin types (blame + keyboard-shortcuts), not blame-specific.

### Terminal
- **Alt+`** — Open Terminal in Utility Dock
- **Ctrl+Space** — Toggle terminal input mode (enter/exit interactive terminal)
- When terminal mode active: editor keyboard shortcuts are captured by terminal
- Close terminal split via "Close Split" command

## UI Layout
- Menu bar: top row (File/Edit/View/Selection/Go/LSP/[Explorer]/Help)
- Tab bar: second row (shows all open buffers, * = modified)
- Editor area: main area with line numbers + content
- Status bar: bottom-1 (filename, cursor pos, status message, encoding, filetype, hint)
- Dialogs/prompts: appear at very bottom (BELOW status bar) — easy to miss!

## Status Bar Format
`Local | [filename] [+] | Ln N, Col N | [message] | encoding | filetype | hint`
- `[+]` = unsaved changes (same as `*` in tab)
- Encoding: UTF-8, ASCII, LF
- Filetype: Text, TOML, Rust, etc.
- Hint: "Palette: Ctrl+P" = normal mode; "Terminal mode..." = in terminal

## Important Behaviors

### Session Persistence
Fresh remembers all open buffers across sessions. Each launch restores the previous state. This means test buffers accumulate. Explicitly close/discard them before each run.

### Multi-cursor
1. Position cursor at first word
2. Ctrl+W to select word
3. Ctrl+D to add cursor at NEXT occurrence
4. Repeat Ctrl+D for more cursors
5. Type to edit all cursors simultaneously

### Save/Discard Dialog
The save prompt appears at the VERY BOTTOM of the screen, below the status bar. Typing after the dialog opens inputs into the dialog's text field (not the editor). Always check for this dialog before typing.

### Alt+W Quirk
Alt+W behaves differently depending on context:
- In search bar: toggles Whole Word search option
- On a tab (when not in search): closes the tab ("Close Tab")
- Outside search, while in editing: may toggle whole-word setting (BUG CANDIDATE)
Safer alternative: Use "Close Buffer" or "Close Tab" commands via Ctrl+P.

### F10 Reliability
F10 can sometimes insert escape sequence `[21~]` into the active buffer instead of opening the menu. This is timing-dependent with tmux. If this happens, undo with Ctrl+Z. Prefer using command palette (Ctrl+P) for menu actions.

### Large File Mode
Fresh enters a "virtual byte-offset mode" for large files (observed at 49MB / 500K lines):
- Status bar shows: `Byte N` instead of `Ln N, Col N`
- Gutter shows BYTE OFFSETS (0, 98, 196...) instead of line numbers
- Navigation (Ctrl+End to bottom) is immediate
- Search still works (found 2 matches in <2s for 49MB file)

### Binary File Mode
Fresh opens binary files gracefully:
- Tab title shows `[BIN]` tag (e.g., `ls [BIN] ×`)
- Non-printable bytes rendered as `<XX>` hex notation
- No crash, no freeze
- Line wrap applies to binary content

### Go to Matching Bracket
- Command: "Go to Matching Bracket" in command palette
- Shortcut: `Ctrl+]` (but `Ctrl+]` = ASCII 0x1D may not transmit correctly via tmux send-keys)
- Use command palette invocation for testing: `Ctrl+P` → type "matching" → Enter
- Works on: `(` → `)`, `{` → `}`, and presumably `[` → `]`

### Position History
- `Alt+Left` = Go Back in position history (previous cursor positions)
- `Alt+Right` = Go Forward in position history
- History is built by navigating (Ctrl+G jumps, bracket matches, etc.)
- Works across lines (tested: lines 5 → line 1 via Alt+Left)

### Alt+W Confirmed Behavior (NOT a bug)
- Normal editing mode (not in search bar): `Alt+W` = **Close Tab** ("Tab closed" status)
- Search bar active: `Alt+W` = **Toggle Whole Word** (search option)
- This is correct context-sensitive behavior, not a bug

### Line Wrap Toggle
- View menu → `☑ Line Wrap` item toggles wrapping
- No keyboard shortcut (use View menu or command palette if it exists)
- Default state: ON (confirmed by visual wrap of long lines at startup)
- No "Toggle Line Wrap" found in command palette — must use View menu

## Alt+A — Project-wide Search & Replace Panel

### Panel Layout
- Opens at the bottom of the screen as a split panel
- Two fields: `Search:` and `Replace:` (Tab to navigate between them)
- Scope checkbox: `[v] All Files` (when checked, searches all project files)
- Options: `Case (Alt+C)`, `Regex (Alt+R)`, `Whole Word (Alt+W)`
- Replace button changes context-sensitively to "Replace All in [filename] (Alt+Ret)" when scoped
- Match list: grouped by file with counts (e.g. `▼ [v] TX tmp_test_files/test_file1.txt (3/3)`)
- Hint line: `Tab next  Space include/exclude  Enter open  Alt+Ret replace selected  Esc close`

### Key Behaviors
- **Scope via Space:** Pressing Space on a file group header toggles its inclusion
  - When pressed on a non-current-file group, it deselects "All Files" and scopes to current file only
  - Shows `Only in: [filename]` indicator when scoped
- **Replace confirmation:** "Replace N match(es) in M file(s)? Undo only covers files still open. Press Enter to confirm, Esc to cancel."
- **Outside-workspace files:** Fixed in commit b7e7e64 — files outside the workspace root (e.g., /tmp) ARE now included in search results
- **Status**: Shows match count in status bar and "Replaced N occurrences in M files" after replace

## Calibrate Keyboard Wizard

### Overview
- Command: "Calibrate Keyboard" in command palette
- Steps: 24 steps across 5 groups
- Groups: Basic Editing, Line Navigation, Word Navigation, Document Navigation, Emacs-Style
- Controls: `[s]` Skip key, `[b]` Back, `[g]` Skip group, `[a]` Abort

### Groups and Keys Tested
1. **Basic Editing**: BACKSPACE, DELETE, TAB, SHIFT+TAB
2. **Line Navigation**: HOME, END, SHIFT+HOME, SHIFT+END (and others)
3. **Word Navigation**: ALT+LEFT/RIGHT, ALT+SHIFT+LEFT/RIGHT, CTRL+LEFT/RIGHT, CTRL+SHIFT+LEFT/RIGHT
4. **Document Navigation**: PAGE UP, PAGE DOWN (and others)
5. **Emacs-Style**: CTRL+A, CTRL+E, CTRL+K, CTRL+Y

### Key Finding
- **Does NOT test CTRL+H** — the terminal compatibility issue (#2109, Ctrl+H = Backspace) is NOT addressed by the wizard
- The wizard would need a group for "Troublesome Keys" to detect this (see IMP-003)
- Final screen: "Input Calibration - Verify" with `[y] Save [a] Abort`
- Summary shows: "All Keys Working!" if all captured keys matched expected

### Rapid Input Behavior
- Fresh handles rapid keystrokes without dropped input (tested: 50 chars at full tmux speed)
- Rapid Ctrl+Z (20 × with no delay) correctly undoes char-by-char with no corruption
- All keys in fast-burst sequences are received and processed correctly

### Resize Reflow
- Fresh adapts layout correctly when tmux window is resized (220×50 → 80×24 → 180×40)
- Status bar truncates gracefully: `Ln 1, Col 1` becomes `Ln 1, ...` at narrow widths
- Resize mid-typing produces no corruption or dropped characters
- Editor remains fully responsive after resize

## Flash: Jump Plugin
- Command: "Flash: Jump" in command palette
- Behavior: Overlays single-character hint labels on every visible word/position in the editor
- Pressing a hint character jumps the cursor to that position
- Status bar shows "Flash[]" while active
- Works correctly — cursor moves to the position labeled by the typed hint char

## Package Manager
- Commands: "Package: Packages" and "Package: Install from URL" (source: `pkg`)
- **Package: Packages** opens a package browser buffer with:
  - Package list (left panel): 13 available packages with type tags [P]=Plugin, [T]=Theme, [L]=Language
  - Detail panel (right): version, author, license, description, tags, repo URL, [Install] button
  - Filter tabs at top: All | Installed | Plugins | Themes | Languages | Bundles | Sync
  - `/` opens search input; Enter confirms; search filters list by name/description
  - Status bar: "Registry synced (N/N sources)"
  - Navigation: ↑↓ navigate, Tab next, / search, Enter select, Esc close
- **Package: Install from URL** shows "Git URL or local path:" prompt at bottom

## Live Diff Plugin
- Commands all prefixed "Live Diff:" in command palette (source: `live_diff`)
- Available modes: vs HEAD, vs Disk (unsaved changes), vs Branch..., vs Default Branch, Refresh, Toggle (Buffer), Toggle (Global), Set Default Mode
- **vs HEAD**: Green `│` gutter markers (ANSI 38;5;78) on added lines; green background (48;5;22) on added content. Status: "Live Diff: comparing against HEAD"
- **vs Disk**: `+` marker (ANSI 38;5;71) on lines in buffer not yet saved. Status: "Live Diff: comparing against file on disk"
- **vs Branch...**: Prompts "Branch or ref" input with "main" pre-filled. Status: "Live Diff: comparing against [branch]"
- All modes confirmed working and switching via status bar confirmation

## Block Selection (Alt+Shift+Arrow)
- **Keys (confirmed working in Run #15):**
  - Block select down: `M-S-Down` (Alt+Shift+↓)
  - Block select up: `M-S-Up` (Alt+Shift+↑)
  - Block select left: `M-S-Left` (Alt+Shift+←)
  - Block select right: `M-S-Right` (Alt+Shift+→)
- **Behavior:** Rectangular (column) selection — same columns selected on every row
- **Confirmed by:** Selecting "Line " (5 chars, cols 1-5) on rows 1-4, then typing '>' replaced it on all rows simultaneously
- **Note:** Run #12 reported M-S-Down did NOT work. Run #15 confirms it DOES work on this build. The key sequences are correct.

## Dev Container Features
- Commands all prefixed "Dev Container:" in command palette (source: `devcontainer`)
- **Create Config**: Creates `.devcontainer/devcontainer.json` with minimal Ubuntu base template `{"name": "...", "image": "mcr.microsoft.com/devcontainers/base:ubuntu"}`
- **Show Info**: Opens `*Dev Container*` panel showing container name, image, action buttons (Run Lifecycle, Open Config, Rebuild, Close). Controls: Tab cycle, Enter activate, Alt+r run, Alt+o open, Alt+b rebuild, **q close** (q works here, unlike *Keyboard Shortcuts*)
- **Show Features**: Returns "No features configured" if devcontainer.json has no features section
- **Show Forwarded Ports**: Opens panel with "No configured or runtime ports to show."; controls: r refresh, q/Esc close
- **Without devcontainer.json**: "No devcontainer.json found" status message
- These Dev Container panels properly close with 'q' — showing the 'q' issue is specific to certain [RO] buffer types

## Live Grep Provider Cycling
- Alt+P cycles providers: git-grep → rg → grep → (back to git-grep)
- Only 3 providers available in this environment (command palette says rg, ag, git-grep, ack, fff, grep — but only 3 installed)
- Search results appear for all providers with match count ("1000+" for common terms)
- Provider shown in toolbar as "[ git-grep ]", "[ rg ]", "[ grep ]"

## Run History
- Run 1 (2026-05-26): First run, built binary, tested ~35 test cases across all sprints
  - Sprints 1-9 largely completed
  - One bug candidate identified (Alt+W inconsistency)
  - Session cleanup: fresh exited cleanly via Ctrl+Q
- Run 13 (2026-05-27): Bug verification + new feature tests
  - TB01 confirmed (BUG-001), TB02 confirmed (BUG-002), TB03 resolved (not a bug)
  - T28 (bracket match), T30 (pos history), T37 (line wrap), T45 (large file), T46 (binary) all PASSED
  - Filed issue #2135; added comment to #2125
  - Binary built with debug profile (target/debug/fresh)
- Run 14 (2026-05-27): T47/T48/Alt+A/Calibrate Keyboard + bug recheck
  - T47 PASS (rapid input), T48 PASS (resize reflow)
  - Alt+A PASS (project-wide search/replace, scoping, confirmation dialog)
  - Calibrate Keyboard TESTED (24 steps/5 groups, no Ctrl+H)
  - #2125 PARTIAL FIX: Diagnostics panel q/a confirmed fixed; *Keyboard Shortcuts* 'q' still broken
  - #2112 CONFIRMED FIXED: /tmp files now found in Search/Replace panel
  - Binary built from release profile (target/release/fresh) on branch claude/awesome-clarke-c7jCY
- Run 15 (2026-05-27): Plugin features + bug rechecks
  - Flash:Jump PASS, Package Manager PASS (Packages + Install from URL), Live Diff PASS (all modes), Live Grep Cycle Provider PASS, Block Selection PASS (M-S-Down/M-S-Right confirmed working), Dev Container PASS (4 commands tested)
  - *Keyboard Shortcuts* 'q' STILL BROKEN; #2117 STILL BROKEN
  - Binary built from release profile (target/release/fresh) on branch claude/awesome-clarke-cN0ma
- Run 16 (2026-05-31): Bug rechecks + new features in v0.3.10
  - #2117 CONFIRMED FIXED (v0.3.10): Review Diff discard hunk works correctly
  - *Keyboard Shortcuts* 'q' STILL BROKEN → new #2165 filed (parent #2125 closed)
  - Diagnostics 'q' STILL WORKING (confirmed)
  - Git Blame PASS (blame buffer, commit info, 'b' go-back, 'q' close)
  - Live Diff: Set Default Mode PASS (all modes accepted)
  - Orchestrator features PASS (Alt+P, Alt+T, Details, filter search)
  - Package install (via "Package: Install from URL") PASS; Color Highlighter plugin PASS (swatches)
  - Uninstall via file deletion works (rm -rf plugin dir); package removed immediately
  - Dev Container: Attach "CLI Not Found" dialog PASS (shows npm install command)
  - Binary built from release profile (target/release/fresh) on branch claude/awesome-clarke-jWgGn

## Git Blame Plugin
- Command: "Git Blame" in command palette (source: `git_blame`)
- Opens `*blame:<filename>*` [RO] buffer with per-line commit annotations
- Format: `── <hash> (<author>, <time>) "<commit message>" ──` as block header
- Status bar: "Git blame: N blocks | b: blame at parent | q: close"
- **'b'** — go back to parent commit of current line's commit
  - If file was added in that commit (initial), shows: "Cannot get blame at SHA^ (may be initial commit or file didn't exist)"
- **'q'** — closes the blame panel correctly (unlike *Keyboard Shortcuts* buffer)
- Tested on: single-commit files and README.md (monorepo context)

## Package Manager: Install via URL
- **Install command:** "Package: Install from URL" (command palette)
- **Prompt:** "Git URL or local path:" at the bottom
- **URL format for monorepo plugins:** `https://github.com/owner/repo#subfolder`
  - Example: `https://github.com/sinelaw/fresh-plugins#color-highlighter`
- **After install:** Status shows "Installed and activated <name> v<version>"
- **Package location:** `/root/.config/fresh/plugins/packages/<name>/`
- **Package files:** `<name>.ts`, `<name>.i18n.json`, `package.json`
- **Uninstall:** No "Package: Uninstall" command exists. Must delete package directory manually: `rm -rf /root/.config/fresh/plugins/packages/<name>/`
  - Fresh detects removal in real-time (no restart needed)
  - Plugin deactivates immediately when directory deleted

## Package Manager: UI Navigation Notes
- Package browser Tab cycle (from list item): 
  - Tab 1: "Enter Select" (list navigation)
  - Tab 2: "Enter Activate" (shows `[ Install ]`/`[ Uninstall ]` brackets in right panel)  
  - Tab 3-4: "Enter Search" (search field)
  - Tab 5-9+: "Enter Filter" (filter tabs: All, Installed, Plugins, Themes, Languages, Bundles)
  - Tab 10-11: "Enter Sync" (Sync tab)
- ⚠️ Tab 2 "Enter Activate" MAY activate the search field rather than the Install button (inconsistent behavior observed — further testing needed)
- For reliable install: use "Package: Install from URL" command in palette
- For uninstall: manually delete the plugin directory

## Color Highlighter Plugin
- Install URL: `https://github.com/sinelaw/fresh-plugins#color-highlighter`
- Commands: "Color Highlighter: Enable", "Color Highlighter: Disable", "Color Highlighter: Toggle"
- **Enable behavior:** Shows filled block `█` character before each color code
  - `#ff0000` → `[38;5;196m█` (red, ANSI 256-color approximation)
  - `rgb(0, 128, 255)` → `[38;5;33m█` (blue)
  - `hsl(120, 100%, 50%)` → `[38;5;46m█` (green)
  - `#abc` → `[38;5;145m█` (light blue/lavender = #aabbcc)
- Supports CSS files and any file with color expressions
- Plugin activates immediately on install (no restart needed)
- Plugin deactivates in real-time on file deletion (no restart needed)

## Dev Container: Attach Error Handling
- When devcontainer CLI is NOT installed:
  - Dialog: "Dev Container CLI Not Found"
  - Message: "The devcontainer CLI is needed for rebuild. Copy the install command below, or dismiss."
  - Shows: "Copy: npm i -g @devcontainers/cli"
  - Dismiss with ESC
- When devcontainer.json is NOT found: status shows "No devcontainer.json found" (no dialog)
- Docker daemon not running does NOT affect this error path (CLI check happens before docker check)

## File Explorer (Run #17)
- **Toggle sidebar:** `Ctrl+B` — shows/hides file explorer sidebar
- **Focus explorer:** `Ctrl+E` — moves keyboard focus from editor to file explorer
- **Navigation:** Up/Down = move cursor; Right = expand directory; Left = collapse directory; Enter = open file (permanent)
- **Auto-preview:** Cursor movement auto-previews files in editor without permanently opening them
- **New file:** `Ctrl+N` when explorer is focused (NOT when editor is focused) — creates file in current dir
- **Delete file:** `Delete` key when file is highlighted → confirmation `y`/`n` → "Moved to trash" on confirm
- **IMPORTANT:** `Ctrl+N` when editor is focused = new buffer, NOT explorer new file. Must press `Ctrl+E` first to focus explorer.
- The `>` prefix in file names (or tree node markers) distinguishes folders from files.

## Settings Panel Navigation (Run #17)
- **Open settings:** Command palette `Ctrl+P` → "Open Settings"
- **Panel structure:** 3-panel overlay: [Categories list | Settings items | Right content panel]
- **Tab cycle:** Categories → Settings → Footer → Categories (one Tab per panel)
- **Categories panel:** When focused (blue `[48;5;25m` bg + `>` prefix), Up/Down navigates categories; right panel updates to show that category's settings
- **Settings panel:** When focused, Up/Down navigates between individual setting controls. Each control shows a description box below it.
- **Footer panel:** Shows `[ Edit ]  [ User ]  [ Reset ]  [ Save ]  [ Cancel ]`. Tab cycles: Save → Cancel → Edit → wrap to Categories (Layer/Reset buttons skipped by Tab per known usability bug).
- **Enter in Settings panel:** For Toggle = toggle value; for Dropdown = open dropdown; for Number = enter edit mode; for TextList = enters editing mode (Up/Down navigate items, Delete removes, Enter confirms/adds)
- **TextList interaction model:**
  1. Navigate to TextList header (e.g., "Sources:") with Up/Down in Settings panel
  2. Press Up to navigate INTO existing items (cursor moves to items list)
  3. Item focused state: item text shown with blue bracket color `[38;5;25m`, hint text "Del:remove  Enter:edit" appears
  4. Press Delete to remove focused item; Enter opens inline editor for item text
  5. Tab exits TextList (jumps to next panel in Tab cycle)
- **TextList [x] buttons:** Visual-only for mouse users; NOT keyboard-accessible via Tab. Keyboard deletion = Delete key.
- **Escape behavior:** Closes settings and DISCARDS unsaved changes (no confirmation dialog when changes are pending — contrary to expectation)
- **Selection highlight:** Blue `[48;5;25m` background = keyboard focus; `[48;5;17m]` darker blue = modified/active indicator
- **Modification indicator:** `●` appears next to setting label when value differs from default

## LSP: Pyright Status (Run #17)
- **pyright-langserver** (from `pip install pyright`): Initialize handshake SUCCEEDS ("LSP (python) ready")
- BUT all request-based features TIMEOUT after 30s: hover, definition, completion, signatureHelp, diagnostics
- Log evidence: warnings log shows 10 timeouts; `LSP initialize result: position_encoding=None` (suspicious)
- pyright processes (Python wrapper + Node child) both running but idle (sleeping state = waiting for input)
- Bug filed: #2197
- **Suspected cause:** Position encoding mismatch — `position_encoding=None` may mean Fresh defaults to UTF-16 but pyright expects UTF-8, causing it to silently discard all subsequent requests
- **Alternative to try next run:** clangd on a tiny C project (avoid rust-analyzer = too heavy)

## LSP: clangd Integration (Run #18)
- **Install:** `apt-get install clangd` (not pre-installed; installs clangd-18 via Ubuntu package)
- **Config for clangd:** `{"lsp": {"c": {"command": "clangd", "args": ["--log=verbose"], "enabled": true}}}`
- **compile_commands.json:** Place in project root to help clangd find include paths
- **Auto-start behavior:** Despite `enabled: true` in config, clangd shows as "not running" on Fresh launch. Must manually start via LSP Status popup (click/run "Show LSP Status" → "Start clangd (always)"). After "Start clangd (always)", LSP shows "ready".
- **Confirmed working features:**
  - Hover (Alt+K): Shows function signature popup
  - Go to Definition (F12): Jumps to definition, "Jumped to definition at <file>:<line>" in status
  - Completions (Ctrl+Space): Shows typed-function-matching completions with signature
  - Find References (Shift+F12): Opens popup with all reference locations
  - Rename Symbol (F2): Prompts "Rename to:", renames at definition + all call sites
  - Inlay hints: Auto-shown (parameter names in call sites, e.g., "add(a: 3, b: 4)")
- **Code Actions (Alt+.):** "No code actions available" at C error location (undeclared malloc). May be clangd limitation — C++ errors typically have more code actions. Needs further testing with C++ or different error type.
- **Language ID:** Use "c" (lowercase) in config.json for C files (Fresh detects .c files as language "C")
- **LSP Status popup format:** `○ clangd (not running)` then options: "Start clangd (always)", "Start clangd once", "Disable LSP for c", "View Log", "Dismiss"

## text-actions Plugin (Run #18)
- **Install URL:** `https://github.com/PavelLoparev/fresh-text-actions-plugin`
- **No subfolder needed** (standalone repo, not monorepo)
- **Version:** v0.1.0
- **Status after install:** "Installed and activated fresh-text-actions-plugin v0.1.0"
- **Commands added (source: "plugin"):**
  - "Encode String to Base64"
  - "Encode String to JSON String"
  - "Encode String to URI Component Encoded"
  - "Encode String to URI Encoded"
  - "Decode URI Component Encoded to String"
  - "Decode URI Encoded to String"
  - "Encode JSON Byte Array to Hex String"
- **Usage:** Select text BEFORE opening command palette. Ctrl+L (select line) + Ctrl+P works reliably. Ctrl+A + Ctrl+P may lose selection.
- **Base64 verified:** "Hello World" → "SGVsbG8gV29ybGQ=" (correct)
- **Uninstall:** `rm -rf /root/.config/fresh/plugins/packages/fresh-text-actions-plugin`

## Encoding Handling (Run #19)
- **Auto-detection:** Fresh auto-detects encoding on file open. Latin-1 encoded files are detected as "Windows-1252" (a superset — correct and reasonable).
- **Status bar encoding:** Shows current encoding (e.g., `LF  Windows-1252  Text`)
- **Reload with Encoding...** command (command palette or File menu):
  - Opens an encoding picker with 8+ encodings: UTF-8, UTF-8 BOM, UTF-16 LE, UTF-16 BE, ASCII, Latin-1, Windows-1252 (+ more)
  - Current encoding marked with "current" label in the picker
  - Bottom prompt shows selected encoding name: "Reload with encoding: Latin-1"
  - Navigation: Up/Down; use ANSI capture (`-e`) to confirm highlighted item (`[48;5;25m` background)
  - Pressing Enter reloads the file from disk with the selected encoding
  - Characters render correctly after reload with correct encoding
- **Set Encoding** command (command palette):
  - Same encoding picker as Reload
  - Changes the BUFFER encoding (how Fresh interprets/saves the bytes) without reloading from disk
  - Marks buffer as modified `[+]` with asterisk `*` in tab title
  - Status: "Encoding set to UTF-8"
  - Saving after Set Encoding writes the file in the new encoding
  - **Round-trip verified:** Latin-1 file → Set Encoding to UTF-8 → Save → file bytes are valid UTF-8 (confirmed by hex)
- **Encoding picker navigation tip:** Arrow keys work; plain-text capture doesn't show selection — use `capture-pane -e` and grep for `48;5;25m` to find highlighted item

## Themes (Run #19)
- **8 themes in v0.3.8:** dark, dracula, high-contrast, light, nord, nostalgia, solarized-dark, terminal
- **"nord" is NEW** compared to the list observed in earlier tests (v0.3.9 had 7 themes without nord)
- **Select Theme** command in command palette; no default keyboard shortcut
- **Theme picker navigation:** Arrow keys; current theme marked "(current)" in list; ANSI `-e` capture needed to see selection highlight (`48;5;25m`)
- **Theme apply confirmation:** Status bar shows "Theme changed to '[name]'"
- **ANSI color evidence (selected samples):**
  - high-contrast: menu bar `38;5;231m` fg, `48;5;236m` bg
  - dark: menu bar `38;5;252m` fg, `48;5;237m` bg
  - light: menu bar `38;5;234m` fg, `48;5;254m` bg (near-white background)
  - nord: menu bar `38;5;188m` fg, `48;5;237m` bg (light blue-grey text)

## LSP: auto_start Setting (Run #19)
- **Config schema defines:** `auto_start: boolean, default: false`
  - Description: "Whether to auto-start this LSP server when opening matching files. If false (default), the server must be started manually via command palette"
- **`enabled: true`** ≠ auto-start. It means "this server is configured and not disabled."
- **To auto-start:** Set `"auto_start": true` in LSP config. Example:
  ```json
  {"lsp": {"cpp": {"command": "clangd", "enabled": true, "auto_start": true}}}
  ```
- **Built-in LSP config docs:** "Fresh will use it automatically" = the CONFIG is pre-built (no manual JSON needed). NOT that the server auto-launches.
- **Without `auto_start: true`:** User must open LSP Status popup and click "Start [server] (always)" to start it

## LSP: Code Actions Root Cause (Run #19)
- **Bug #2212:** Fresh always sends `"context":{"diagnostics":[]}` (empty array) in all `textDocument/codeAction` requests
- **Effect:** clangd's fix-based code actions (triggered by diagnostics) are never returned — clangd returns `[]` when `context.diagnostics` is empty
- **LSP log evidence:**
  - Incoming: `publishDiagnostics` with N diagnostics including `"(fix available)"` markers
  - Outgoing: `codeAction` with `{"context":{"diagnostics":[]},"range":...}`
  - Reply: `{"result":[]}`
- **Root cause (from closed issue #1915 source comment):** `// TODO: Implement diagnostic retrieval when needed` in `app/lsp_requests.rs`
- **Not a clangd limitation:** This is a Fresh implementation gap. Clangd DOES return fix actions — but requires the diagnostic objects in the context.
- **Affected:** All diagnostic-based code actions for any LSP server (clangd, potentially others). Non-diagnostic refactoring actions may also be absent for other reasons.

## Git Blame: Multi-Commit History Navigation (Run #18)
- **'b' key behavior with multi-commit files:** Navigates to PARENT commit of the commit on the current cursor line
- **Depth tracking:** Status bar shows "Git blame at <SHA>^ | depth: N | b: go deeper | q: close"
  - depth: 0 = HEAD blame (initial state)
  - depth: 1 = one parent commit back
  - depth: 2 = two parent commits back
  - etc.
- **Confirmed on CHANGELOG.md (399 blame blocks):**
  - Line 3 at HEAD: bc11f2b (1 week ago)
  - After 'b': 059f4ab (2 weeks ago) at depth: 1
  - After 'b' again: 60d0ba2 (2 weeks ago) at depth: 2
- **When at initial commit:** "Cannot get blame at <SHA>^ (may be initial commit or file didn't exist)"
- **Important:** Fresh must be launched from the git repo directory for git blame to work. If launched from /tmp, git blame returns "No blame information available (not a git file or error)"

## text-actions Plugin: Full Decode Command Set (Run #20 — updated)

Learning_db.md previously documented only 7 commands. The plugin v0.1.0 has MORE commands:

**Encode commands:**
- "Encode String to Base64"
- "Encode String to JSON String"
- "Encode String to URI Component Encoded"
- "Encode String to URI Encoded"
- "Encode JSON Byte Array to Hex String"

**Decode commands (previously MISSING from docs):**
- "Decode Base64 to String" — `"SGVsbG8gV29ybGQ="` → `"Hello World"` ✅
- "Decode URI Component Encoded to String" — `"Hello%20World%21"` → `"Hello World!"` ✅
- "Decode URI Encoded to String" (also present)
- "Decode JSON String to String" — `"Hello\nWorld\t!"` → multiline with ANSI newline+tab ✅
- "Decode Hex String to JSON Byte Array" — `"48656c6c6f"` → `"[72,101,108,108,111]"` ✅

**Round-trip verified:** "Fresh Editor 2026" → Base64 → decode → "Fresh Editor 2026" (exact match to `echo -n "Fresh Editor 2026" | base64`)

**Usage:** Select text with Ctrl+L (select line) BEFORE opening command palette. All commands work on selected text (replace selection with result).

## Bookmarks (Run #20 — full slot test)
- **Set:** Ctrl+P → "Set Bookmark" → Enter → type digit (0-9) + Enter → "Bookmark 'N' set"
- **Jump:** Alt+N (where N = 0-9). Status: "Jumped to bookmark 'N'" on success
- **Unset slot:** "Bookmark 'N' not set" (stays on current line)
- **Slots 0-9 all confirmed working** (Run #20 tested 0, 1, 5, 9 + unset slot 2)
- Bookmarks persist per-session (session restore carries them)

## Keyboard Macros (Run #20 — complex macro confirmed)
- **Record:** Ctrl+P → "Record Macro" → slot digit (0-9) + Enter → "Recording macro 'N' (F5 or Ctrl+P → Stop Recording)"
- **During recording:** All keystrokes are captured LIVE (macro actions are actually applied to the buffer during recording)
- **Stop:** F5 → "Macro 'N' saved (X actions) - F4 → Play Last Macro"
- **Play:** F4 → "Played macro 'N' (X actions)". Plays the LAST recorded macro.
- **Named play:** Ctrl+P → "Play Macro" (if available) to play a specific slot
- **List:** Ctrl+P → "List Macros" → `*Macros*` buffer showing action-level detail per slot
  - Actions shown: SmartHome, InsertChar('X'), MoveDown, MoveUp, etc.
- **Complex macro confirmed working** (Run #20): 5-action macro = Home + "#" + " " + Down + Home. Played on 5 consecutive lines, all received "# " prefix correctly.
- **Slots:** 0-9 (tested slot 3 in Run #20; slots 0-9 in Run #10 earlier)

## SSH Remote Editing (Run #21)

### Forms Documented (docs/features/ssh.md)
- **scp-style:** `user@host:path[:line[:col]]` — WORKS (correctly triggers SSH connection path)
- **URL-style:** `ssh://[user@]host[:port]/path[:line[:col]]` — BUG (#2221): treated as local path

### scp-style Behavior
- `fresh user@host:/path` → shows "Connecting via SSH to user@host..." in terminal stdout
- Fails at SSH-spawn step if `ssh` binary not installed: "Failed to spawn SSH process (No such file or directory)"
- Error message is actionable: "Is the `ssh` command installed and in your PATH?"
- Requires Python 3 on remote host for the agent component

### URL-style Bug (#2221)
- `fresh ssh://host/path` is treated as a **local relative path** (CWD + URI string)
- Log evidence: `open_file_no_focus: path="/home/user/fresh/ssh://localhost/etc/hosts"`
- Empty local file opened; no error shown; status bar shows "Local | ssh://..."
- Workaround: use scp-style form (`user@host:/path`)

### Requirements for SSH
- `ssh` binary must be in PATH (error message tells you if not)
- Python 3 on remote host (for the remote agent)
- Status bar shows `[SSH:user@host]` when connected (per docs)

## Keybinding Editor (Run #21 — full workflow tested)

### Opening
- Command palette → "Open Keybinding Editor" (or Edit → Keybinding Editor...)
- Shows 852 bindings by default; Config path shown at top

### Adding a New Binding
1. Press `a` → "Add Keybinding" dialog
2. Press `Enter` → key capture mode → press any key → key captured (e.g., F9)
3. `Tab` → Action field: type action name; autocomplete popup with Up/Down/Tab/Enter navigation
4. `Tab` → Context field: `[normal]  ←/→ to change` (cycles: global, normal, prompt, popup, file_explorer, menu, terminal)
5. `Tab` → Save/Cancel buttons at bottom
6. `Enter` on Save → dialog closes, binding count increments, `[modified]` shown

### Saving Changes
- `Ctrl+S` in keybinding editor → saves to config.json, status bar: "Keybinding changes saved"
- Config format: `{"keybindings": [{"key": "F9", "action": "save", "when": "normal"}]}`

### Verified Working (Run #21)
- Add F9 → save → normal → Save → Ctrl+S → "Keybinding changes saved" ✅
- F9 triggers file save correctly after adding binding ✅
- config.json written correctly ✅
- Search (`/`): filters by action name/description/key; shows count (e.g., "10/852 shown") ✅

## Search in Selection (Run #21)
- **NOT IMPLEMENTED** — Find bar has no "In Selection" toggle
- Search always spans entire buffer regardless of active selection
- See IMP-014 in potential_improvements.md

## Multi-Root Workspaces (Run #21)
- **Workspace scoping:** Ctrl+P file mode shows ONLY files from the CWD (workspace root)
- **Cross-workspace:** Files outside workspace root opened via Ctrl+O appear with full absolute paths
- **Project-wide search (Alt+A):** Includes ALL open buffers — workspace files (relative paths) AND out-of-root files (full paths)
- **Session isolation:** Workspace root = CWD at launch. Different CWDs have separate sessions.

## Markdown Compose Mode (Run #20 — full verification)
- **Toggle:** Ctrl+P → "Markdown: Toggle Compose/Preview" → prompts "Compose width: None" → press Enter for viewport width
  - Second toggle = OFF
  - Status on ON: "Markdown Compose: ON (soft breaks, centered)"
  - Status: "Markdown compose width: using viewport width" (first activation message)
- **Rendering in compose mode:**
  - `**bold**` → ANSI `[1m` bold attribute; asterisks HIDDEN ✅
  - `*italic*` → ANSI `[3m` italic attribute; asterisks HIDDEN ✅
  - `` `inline code` `` → colored `[38;5;69m`; backticks STRIPPED ✅
  - `# Heading` → heading color `[38;5;51m`; `#` prefix STILL VISIBLE
  - `## Heading` → heading color; `##` STILL VISIBLE
  - ` ```python ... ``` ` code blocks → fence markers STILL VISIBLE; code syntax-highlighted ✅
  - `> blockquote` → `>` colored `[38;5;6m` (teal); quote marker STILL VISIBLE
  - `- lists`, `1. ordered lists`, `---` HR → all visible, normal rendering
- **Line numbers:** HIDDEN in compose mode (no `N │` prefix in display)
- **Editing inside code blocks works in compose mode** — new lines added correctly; display updates immediately
- **Toggle workflow quirk:** First Ctrl+P → "Toggle" activation shows a "Compose width" prompt. Second activation (same command) DIRECTLY toggles ON (no prompt). Third activation toggles OFF.

## Workspace Trust (Run #22 — NEW in v0.3.12)
- **Enforcement is ON** in v0.3.12 (was "groundwork, off by default" in 0.3.10 CHANGELOG). A folder with tooling markers (e.g. `compile_commands.json`) triggers a "⚠ SECURITY WARNING" dialog on launch.
- **Dialog options:** `(*)` radio list — "Trust folder & Allow Tooling (T)", "Keep Restricted (Default) (K)", "Block All Execution (B)". Letter selects the radio, **Enter confirms**. **Esc does NOT dismiss the dialog.**
- **Persistence:** `~/.local/share/fresh/workspaces/<percent-encoded-path>/trust.json` → `{"level": "trusted"}`. Delete the dir to reset for re-testing.
- **LSP is gated on trust:** log shows `LSP for 'cpp' not auto-started: workspace is not trusted`. After trusting, `auto_start: true` works.
- **Trust confirm = FULL EDITOR RESTART** (log: `Restart requested with new working directory`). Session restore rebuilds buffers in default mode; with `--no-restore` the CLI file + unsaved edits are silently lost → BUG #2291.
- "Keep Restricted" + Enter does NOT restart; file stays open; status: "Workspace restricted — repo-controlled execution is blocked".
- Palette command to reopen dialog: **"Workspace Trust…"** (type `Trust` — searching `Trust Lev` finds nothing; old "Set Workspace Trust Level" command is gone).

## SSH Loopback Test Recipe (Run #22)
- `apt-get update && apt-get install -y openssh-server` (404s without update first)
- `ssh-keygen -A; ssh-keygen -t ed25519 -N "" -f ~/.ssh/id_ed25519 -q; cat ~/.ssh/id_ed25519.pub >> ~/.ssh/authorized_keys; chmod 600 ~/.ssh/authorized_keys; mkdir -p /run/sshd; /usr/sbin/sshd -p 22`
- scp-style works END-TO-END (v0.3.12): `fresh --no-restore root@localhost:/tmp/file.txt` → status-bar origin segment becomes `root@localhost`, content loads, Ctrl+S writes through ("Saved").
- ssh:// URL form still broken (#2221) — status stays `Local`, empty buffer.

## Keybinding Editor: Add/Delete/Record (Run #22 — v0.3.12)
- Footer: `Enter:Edit  a:Add  d:Delete  /:Search  r:Record Key  c:Context  s:Source  ?:Help  Ctrl+S:Save  Esc:Close`
- **Add dialog focus path:** Key field → Enter (start capture) → press key → Tab → type action (autocomplete popup; Enter accepts) → **Tab lands on Context** (←/→ to change) → **Tab again lands on [Save]** → Enter. Pressing Enter while on Context CLOSES the dialog WITHOUT saving (silent — caused a lost add in Run #22).
- After the Add dialog closes there can be a transient state where r//, Esc are ignored until an arrow key is pressed (similar to #2143 item 5 "invisible dialog steals input"). Buffer does NOT receive the keys.
- **Delete:** select custom row (Source column = `custom`) → `d` → "Custom binding removed" + [modified] → Ctrl+S → "Keybinding changes saved"; the `keybindings` key is removed from config.json entirely when last custom binding deleted.
- **Record Key Search:** `r` → "Record Key: Press a key..." → pressing any key filters (e.g. Ctrl+S → 3/866 across normal/merge/theme contexts). CAVEAT: arrows/Esc are CAPTURED as search keys — you cannot navigate results in record mode. To act on a result use `/` text search → Enter (keeps filter, returns focus to list) → arrows → d/Enter.
- Quirk to re-observe: total binding count differed between two opens in one session (866 vs 548). **→ ROOT-CAUSED Run #28, see below; filed #2307.**

## Keybinding Map Switching + Editor Count Bug (Run #28 — v0.3.12 @ 67d0c6e6c)
- **Four keymaps:** `default` (current at clean launch), `emacs`, `vscode`, `macos`. Switch via palette → **"Select Keybinding Map"** → arrow/type-filter + Enter. Status bar: "Switched to '<map>' keybindings". Selection PERSISTS to `~/.config/fresh/config.json` as `"active_keybinding_map": "<map>"` (selecting a map also writes a full default config the first time).
- **Palette key is KEYMAP-DEPENDENT:** default/macos/vscode = `Ctrl+P`; **emacs = `M-x` (Alt+x)** (Ctrl+P = move-up in emacs, so it leaks text into the buffer — not a bug). Status bar hint "Palette: <key>" tells you the current one. To open the Keybinding Editor regardless of keymap, use the **Edit menu**: `F10 → Right (to Edit) → Up (wraps to last item "Keybinding Editor...") → Enter`.
- **Editor title shows active map:** `┌ Keybinding Editor ─ [default] ─...`. Header line: `Context: [All]  Source: [All]  <N> bindings`. `s` cycles Source filter (All→Custom→Keymap→Plugin→All); `c` cycles Context (long list: All, global, normal, prompt, popup, menu, terminal, file_explorer, mode:* …). Filtered view shows `M/N shown` (denominator = current total).
- **Per-map FIRST-LOAD totals (clean, correct, stable across repeated opens):** default **866** (Source Plugin 391, Keymap 260), emacs **519** (Builtin group 363), macos **600** (Builtin group 455). vscode not measured. So a count differing *between maps* is EXPECTED, not a bug.
- **BUG #2307 (the real "866 vs 548"):** a SINGLE keymap round-trip back to an already-loaded map drops the count and removes the plugin layer. Repro: clean `default` → editor = 866 (Plugin 391/866) → Select Keybinding Map `emacs` → Select Keybinding Map `default` → reopen editor = **547** with **Source[Plugin] = 0/547** (all 391 plugin bindings gone); Source[Keymap] still 260. 100% reproducible, persists across reopens + multi-second wait. **App restart restores 866.** The plugin bindings STILL FUNCTION after the round-trip (Alt+O = Toggle Orchestrator Dock Focus still opens the dock), so it's a Keybinding-Editor listing/reporting defect, not loss of function.
- Note: the editor's tree-group "Builtin (N)" header and the Source-column filter categorize differently (group Builtin 400→402 across the round-trip while Source-derived numbers move more) — cite the directly-read header total and the Source[Plugin]/Source[Keymap] filtered counts, which are reliable.

## Workspace Trust — Full 3-State Enforcement Matrix (Run #23 — v0.3.12, master @ f4ee3630)
Supersedes/extends the Run #22 notes above. Enforcement is process-layer and works correctly.
- **Dialog (richer than Run #22):** now has explicit `[ OK ]` and `[ Quit (Ctrl+Q) ]` buttons. Three radios with descriptions that ARE the documented enforcement contract:
  - `(*) Trust folder & Allow Tooling (T)` — "Runs everything: language servers, build scripts, tasks, env activation."
  - `(*) Keep Restricted (Default) (K)` — "Runs system tools found on your PATH (git, ripgrep, the system python). Blocks: executables & scripts located inside this project (./gradlew, .venv/bin/python, node_modules/.bin/*), env activation (.env/.envrc/mise), and language servers."
  - `(*) Block All Execution (B)` — "Nothing runs — no system tools, language servers, scripts, or tasks."
  - Letter (T/K/B) selects the radio; **Enter confirms** the selected one (don't need to Tab to [OK]). Use the test reference: assert against these descriptions.
- **Status-bar word reflects state:** `Restricted` / `Blocked` / `Trusted` (leftmost segment).
- **RESTRICTED (default):** LSP gated OFF (log: `LSP for '<lang>' not auto-started: workspace is not trusted`). **git ALLOWED** — git blame works (multi-commit blocks), git_explorer/git_gutter/merge_conflict all spawn. **ripgrep/git-grep ALLOWED** — Live Grep returns matches with both `git-grep` and `rg` providers. Does NOT restart on confirm.
- **BLOCK ALL:** every subprocess denied at spawn — log: `Process error: workspace trust is set to Blocked — no processes may run` (exit_code -1). User-facing: git blame → "No blame information available (not a git file or error)"; Live Grep → "No matches" (misleading — see IMP-017). LSP off. **Confirm triggers an editor restart** (File Explorer auto-opens) but **preserves the open file** (#2291 fix).
- **TRUSTED:** ungates the tooling layer — the `clangd-lsp` plugin loads & registers commands (clangdProjectSetup/SwitchSourceHeader) that it does NOT register in Restricted/Blocked; "not trusted" gate messages stop. BUT actual LSP server start is still governed by `auto_start` (default false → status still `LSP (off)` even when trusted). Confirm triggers a restart. trust.json → `{"level":"trusted"}` (levels seen: `trusted`; restricted/blocked presumably analogous).
- **Palette surface:** only ONE command, "Workspace Trust…" (opens the dialog). No direct trust/restrict/block palette commands; `workspace_trust_block` is not palette-exposed.
- **Test recipe:** real git C project in /tmp with `compile_commands.json` (trust trigger) + 2 commits; `rm -rf ~/.local/share/fresh/workspaces/*` to reset trust; tail `~/.local/state/fresh/logs/fresh-*.log` for the `not trusted` / `Blocked — no processes may run` evidence. NOTE git in this env enforces commit signing — use `-c commit.gpgsign=false` for throwaway repos.

## Orchestrator Dock (Run #24 — v0.3.12, master @ f4ee3630)
The dock is a persistent, non-modal LEFT-column session switcher (CHANGELOG headline feature). Fully tested; works as documented, NO bugs.
- **Toggle/focus:** `Alt+O` opens the dock and focuses it; `Alt+O` again toggles. Header shows underlined `Orchestrator` title (Alt+O mnemonic).
- **Dock layout (top→bottom):** `[ + New Alt+N ]` `[ Manage ]` · `[ view: card|compact ]` `[ All ▾ ]` (project dropdown) · `[ ] all worktrees` · `[ ] show empty` · `Filter [/ to search]` · separator · session rows.
- **Session row (card view):** bordered box, 2 lines — `· <project>   ▣ <session>` / `▸ <branch>   <gitstatus>` (e.g. "clean"). Leading glyph = working/idle indicator (`·` idle, `*` active/current session).
- **Focus model inside dock:** `Tab`/`BTab` cycle controls; focused control gets blue bg `48;2;0;100;200`. **Arrows (Up/Down) LIVE-SWITCH the active session** — retarget the editor pane to the selected session with NO restart (bidirectional; confirmed editor pane swaps main.rs ↔ remote terminal instantly).
- **View toggle:** focus `[ view: card ]` + Enter → `[ view: compact ]`. Compact = 1-line rows (`· <session>  clean`); card = bordered 2-line boxes.
- **Project dropdown `[ All ▾ ]`:** Enter opens a popup: `[ ● All projects ]` + one row per project (`[ tmp/orch_test24 ]`). Radio scope filter.
- **Filter:** press `/` (when dock focused) to activate, type to narrow session list live (e.g. "test24-1" → only that row shows). Escape clears/closes.
- **Manage button** → opens the FULL modal Orchestrator dialog (`ORCHESTRATOR :: Sessions — all projects`): left Sessions panel (New / Project dropdown Alt+P / Show all worktrees Alt+T / Show empty Alt+I / Filter / session list) + right detail panel with `[ Visit ] [ Details ] [ Stop ] [ Archive ] [ Delete ]` and a live file preview. Same dialog as prior runs' Orchestrator tests.
- **Right-click context menu:** inject SGR right-click on a session row (`\e[<2;COL;ROWM` press + `m` release). Popup anchored at cursor: header = session name, `[ Visit… ]` `[ Archive ]` `[ Delete ]`, "Esc to close". Matches CHANGELOG.
  - **Archive** (left-click `\e[<0;COL;ROWM`/`m`) → centered "Confirm Archive" dialog: "This will: • SIGKILL all session processes • close the editor session • move the worktree to .archived/  Reversible via Unarchive." `[ Cancel ] [ Confirm Archive ]`. Escape cancels.
- **New Session dialog (`Alt+N`):** `ORCHESTRATOR :: New Session`. **Type selector** `Run in: [ Local ] [ SSH ] [ Kubernetes ] [ Devcontainer ]` — focus the "Run in" row (it's ABOVE Project Path; the dialog OPENS with focus on Project Path, so `BTab` once to reach it) then ←/→ live-switches type AND reflows the whole form:
  - **Local:** Project Path (auto-detects cwd) · `[v] Create a new git worktree` (auto-disabled "(disabled — non-git)" when path isn't a git repo) · Session Name (auto `<proj>-N`) · Agent `[ terminal ][ claude ↻ ][ aider ↻ ][ custom… ]` · Agent Command · Branch (auto "HEAD (no origin configured)").
  - **SSH:** Host `[user@]host[:port]` (placeholder mentions "or paste ssh://host/path") · Remote Path · Identity file · SSH options (`-J jump-host`/ProxyCommand) · Session Name · Agent · Agent Command "remote login shell".
  - **Kubernetes:** Target · Context (kubeconfig) · Namespace (default) · Pod · Workspace path (/workspace) · info "ⓘ kubectl exec into the pod — any cluster (EKS/GKE/AKS/k3d)".
  - **Devcontainer:** Project Path · info "ⓘ runs `devcontainer up`, then attaches (docker exec)". Create with no devcontainer config → graceful error "Devcontainer: open the project and run 'Dev Containers: Reopen in Container'".
- **Create Session button focus quirk (NOT a bug — verified):** the primary `[ Create Session ]` button shows NO highlight when UNfocused (plain cyan `38;2;0;255;255`), but DOES get the blue bg `48;2;0;100;200` when Tab-focused — and Enter then creates the session. Tab order (Local): Project Path → worktree → Session Name → Agent → Agent Command → Branch → Cancel → Create Session. Cancel is the 6th Tab (blue), Create Session the 7th. Keyboard creation WORKS; my earlier "Enter does nothing" was measurement error (was on the Branch text field, not the button). Mouse left-click on the button also works.
- **Footer:** `Tab next / accept  S-Tab prev  ↑↓ suggest / history  Space toggle  Enter advance / act  Esc cancel`. NOTE: in `tmux send-keys`, use `BTab` for Shift+Tab — `S-Tab` is inserted as the LITERAL text "S-Tab" into the focused field in this tmux build (corrupts the form).
- **Creating a Local worktree session** spawns a real git worktree under `~/.local/share/fresh/orchestrator/<sanitized_proj>/<session>/` (slashes→`_`) and switches the editor to it (terminal agent → bash prompt in the worktree). Clean up with `git worktree remove --force <path>` from the parent repo + `git worktree prune`.

## Go to LSP Symbol — Document Symbol Finder (Run #25, 0.3.12)
- **Palette command:** "Go to LSP Symbol" (source `lsp_navigation`, desc "List document symbols from LSP and navigate to selected"). No default keybinding. Sibling LSP nav cmds: Rename Symbol (F2), Find References (Shift+F12), Go to Definition (F12), Show Hover Info (Alt+K).
- **Scope = DOCUMENT symbols only** (current file). clangd `textDocument/documentSymbol`, NOT workspace symbols. Filtering for a symbol that lives in another open-but-inactive file (e.g. `helper` in helpers.c) → "No matches". This matches its description; NOT a bug.
- **List format:** `[kind] name    <source line preview>`. Kinds seen on C: `[class]` (struct/typedef — clangd emits each struct TWICE: the typedef + the struct tag), `[field]`, `[var]` (file-scope global), `[fn]` (function). Prompt at bottom: `Go to symbol:`.
- **Live filtering:** type to narrow the list live (substring match on name). `make` → make_rectangle; `print` → print_rectangle; `main` → main.
- **LIVE PREVIEW (the marquee behavior):** as you filter/arrow-navigate, the editor pane scrolls to and highlights the selected symbol's location WITHOUT committing. Editor shows the symbol name highlighted yellow (`38;5;226m`); the selected list row has blue bg (`48;5;25m`). Up/Down arrows move the selection and re-preview.
- **Enter = commit jump:** cursor lands correctly on the symbol (verified via `tmux display-message -p '#{cursor_y}'` — physical terminal cursor on the symbol's editor line).
- **Escape = cancel + RESTORE:** cursor returns to the exact pre-open position (e.g. open from Ln 16 → preview around → Esc → back at Ln 16, Col 1). Good live-preview/restore model, matches VS Code.
- **BUG #2301 (filed Run #25, low-sev):** after Enter-jump, the status bar **line number is stale** — it keeps the pre-jump line; only `Col` updates. Self-corrects on the next cursor move. Reproduced 3/3 (start Ln30 → jump `main`@44 → status `Ln 30, Col 5` while cursor really on 44). **Feature-specific:** Ctrl+G and F12 both update `Ln` immediately (F12 also prints `Jumped to definition at …:N`). So it's the symbol-jump path that fails to refresh the line component.

## Workspace Trust pre-seed: encoding does NOT match (Run #25)
- Tried pre-seeding `~/.local/share/fresh/workspaces/<urlencoded path>/trust.json` with `%2Ftmp%2F...` (Python `urllib.parse.quote`). Fresh did NOT recognize it — trust dialog still appeared on launch. Fresh's workspace-dir encoding scheme differs from straight percent-encoding. **Just trust via the dialog (`T` then Enter)** — simpler and reliable. (Trust confirm triggers a restart but in default mode session-restore brings the file back; File Explorer auto-opens — IMP-015.)
- **clangd auto-start DID work this run:** config `{"lsp":{"c":{"command":"clangd","args":["--background-index","--log=verbose"],"enabled":true,"auto_start":true}}}` + trusting the workspace → on restart, status bar shows `LSP (on)` and inlay hints render (`make_rectangle(x1: 0.0, …)`, `{.x= 0.0}`) with no manual "Start clangd". Confirms IMP-013/Run#19: `auto_start:true` + Trusted = clangd launches automatically.

## Rainbow Brackets (Run #26) — 0.3.12, on by default, PASS
- **Feature:** "Rainbow bracket colorization for matching brackets across the viewport" (CHANGELOG 0.3.12, #1088). Built-in, **on by default — no config needed**; no `docs/configuration` entry (only `docs/internal/*` mention a `rainbow_brackets: bool` theme field).
- **How it colors:** by **nesting depth**, not bracket type. Verified `tmux capture-pane -p -e` foreground SGR codes:
  - Depth→color cycle (6 colors, then repeats): `0=38;5;6`(cyan) `1=38;5;2`(green) `2=38;5;3`(yellow) `3=38;5;126/127`(magenta) `4=38;5;15`(white) `5=38;5;27`(blue).
  - **Matching open/close pairs share the same color** (e.g. `((((( deep )))))` opens 6,2,3,126,15 → closes mirror 15,126,3,2,6).
  - **Works across bracket types:** `[ { ( [ ( { } ) ] ) } ]` colored 6,2,3,126,15,27 then exact mirror — `(`/`[`/`{` at the same depth get the same color.
  - **Across the whole viewport** (all visible lines colored regardless of cursor position), not just near the cursor.
- **Deep nesting:** 11-level `(((((((((((` cycles `[6,2,3,126,15,27]` and repeats (depth6→6 …), and all 11 closers mirror their openers precisely. No clamp/breakage.
- **Unbalanced handling (robust):** unmatched open `( [ { a } ] ;` → the stray `(` stays its depth color, inner matched pairs unaffected (no cascade). Stray closers `a ) b ] c } ;` → all rendered at depth-0 color (6), no crash, no negative-depth glitch.
- Minor: depth-3 magenta is sometimes `126` vs `127` between lines — both indistinguishable magenta; NOT a fileable bug.

## Brackets in comments/strings — #2405 (Run #40) — v0.4.1 @ 33e2ed130, PASS (extends Rainbow Brackets)
- **Fix (commit `1008134eb`, #2405):** bracket overlay now SKIPS any bracket inside a Comment or String span — for (1) rainbow colorization, (2) cursor-on-bracket matching, AND (3) nesting-depth calculation. Before, every `()[]{}<>` got colored/matched even inside prose/data.
- **Black-box method:** a Python file with brackets in 4 contexts (real code / inside a string literal / inside a `#` comment / real code followed by a trailing comment full of fake brackets). ANSI `capture-pane -p -e` on each line; plus "Go to Matching Bracket".
- **Verified (ANSI):**
  - String `"… ( [ { } ] ) …"` renders as ONE string-color span (`38;5;34` green for Python strings) — inner brackets keep string color, NOT rainbow. Only the enclosing *real* `(`/`)` are rainbow (`38;5;6` cyan d0).
  - Comment `# … ( [ { } ] ) …` whole line = comment color (`38;5;253`); brackets keep comment color.
  - **Depth excludes them:** `z = (10 + 20)  # ))) ((( fake` → the real `(`/`)` are cyan d0/d0 (mirrored); the comment's `)))`/`(((` (all `38;5;253`) do NOT shift depth.
- **Matching command:** "Go to Matching Bracket" (`Ctrl+]`, builtin, "Jump to the matching bracket, parenthesis, or brace"). From a real `(` → lands on the real `)`, skipping comment fake brackets. From a **comment bracket** → status `No matching bracket found` (correctly non-structural).
- **tmux gotcha:** `Ctrl+]` via `tmux send-keys "C-]"` does NOT reach the editor (tmux intercepts it) — invoke the command via the palette ("Go to Matching Bracket" + Enter) instead. Also: the command palette overlay renders at the BOTTOM of the screen (rows ~43-50 on a 50-row pane) under a `> command | : line | # buffer` legend — `capture-pane | sed -n '1,14p'` shows only editor content above it; grep the whole capture or look at the bottom rows.
- **Occurrence-highlight reminder (#2312, still present):** the word under the cursor gets a fixed `48;5;16` background (e.g. `code` highlighted) — that bg-16 is occurrence highlight, NOT a bracket/cursor artifact. Don't mistake it for a bracket-match highlight.

## Terminal Auto-Naming (Run #26) — 0.3.12, on by default, PASS
- **Feature:** "Terminal tab auto-naming: tabs follow the foreground process and OSC title. Setting `editor.terminal_auto_title` (on by default)" (CHANGELOG 0.3.12).
- **Tab name format:** `<foreground process> — <OSC title>`. On opening "Open Terminal" (palette; opens in current split as a buffer tab), the tab read `bash — root@vm: /home/user/fresh` (the `root@vm: /home/user/fresh` part is bash's own `\u@\h: \w` OSC title from PROMPT_COMMAND).
- **Follows foreground process:** ran `python3` → tab became `python3 — root@vm: /home/user/fresh`; `exit()` → reverted to `bash — root@vm: /home/user/fresh`.
- **Follows OSC title:** setting a manual `printf '\033]0;HELLO-FROM-OSC\007'` initially appeared to "not stick" — that was bash's PROMPT_COMMAND overwriting the title on the very next prompt (standard shell behavior, reproducible in any terminal emulator). After `PROMPT_COMMAND=""; PS1="\$ "` then the printf, the tab correctly read `bash — HELLO-FROM-OSC`. So OSC title IS followed; the apparent failure was a bash-side overwrite, NOT a Fresh bug.
- **tmux gotcha (re-confirmed):** `M-grave` is NOT parsed by `tmux send-keys` for Alt+backtick — it inserts the literal text "M-grave" into the buffer. Use the command palette ("Open Terminal" / "Open Terminal in Utility Dock") instead of the Alt+` accelerator when driving via tmux.

## Open File From a Diff (Run #27) — 0.3.12, PASS
- **Feature (CHANGELOG 0.3.12):** "Open file from a diff: in the side-by-side and review-diff views, Enter opens the working-tree file (NEW pane) or the read-only HEAD version (OLD pane) at that line."
- **Entry path (black-box):** Palette → **Review Diff** opens the *unified* `*Review Diff*` buffer (file list + hunks, two line-number columns OLD|NEW). Header legend: `[Enter] jump  [Alt+o] open file`. Pressing **Enter on a hunk line opens the SIDE-BY-SIDE `*Diff: <file>*` view** (NOT the file directly) — that side-by-side view is where the actual open-file actions live.
- **Side-by-side `*Diff: <file>*` view:** two panes — `OLD (HEAD)` (left) / `NEW (Working)` (right), word-level aligned with blank-row padding for inserted/deleted lines. Header legend: OLD `[Enter] open this version  [n/p] hunks  [q] close`; NEW `[Enter/Alt+o] open file`. It is a read-only composite buffer (`[RO]`); `q` closes it.
- **OLD pane Enter** → opens a NEW read-only tab `*HEAD:<file>*` `[RO]`, status `Opened HEAD version (read-only) at line N`. Cursor **lands on the correct HEAD line** (verified: highlighted row `48;5;233` + `tmux display-message -p '#{cursor_y}'` on the right buffer line). Content = `git show HEAD:<file>`. Each Enter opens a *fresh* HEAD tab (e.g. `*HEAD:calc.py* 1`, `*HEAD:calc.py* 2`) rather than reusing — minor.
- **Alt+o** → opens the **working-tree file** (the real editable `<file>` tab), status `Opened <file>`, cursor at the line. This is the NEW/Working action and works regardless of which pane has focus.
- **tmux gotcha:** focusing the NEW (Working) pane via **Tab is unreliable** over tmux — Tab pushed `cursor_x` to the far-right (~219, the COMMENTS column) instead of cleanly switching panes. For the working-file path just use **Alt+o** (universal), don't fight Tab.
- **Related glitch (NOT a new issue):** status bar shows stale `Ln 1, Col 1` for ~one keypress immediately after the diff→open jump (cursor is physically on the right line). Self-corrects on any cursor movement. **Same family as #2301** — confirmed it is NOT LSP-specific; commented on #2301 (Run #27) instead of re-filing. When testing any "jump to location" command, verify the *physical* cursor (`display-message cursor_y` / highlighted row), not just the status bar, which can lag.

## auto_read_only / read-only `[RO]` indicator (Run #29) — v0.3.12 @ 2dee83697
- **New option (commit 9738ac661, not yet in CHANGELOG):** `editor.auto_read_only` (default `true`). Disables Fresh's automatic read-only mode. Set in `~/.config/fresh/config.json` as `{ "editor": { "auto_read_only": false } }`.
- **Auto read-only triggers** (per `docs/features/editing.md:42`): files without write permission AND known library/toolchain paths (`/usr/include`, rustup toolchains, `/nix/store`, Homebrew Cellar, `.nuget`, Xcode SDKs). Binary files always open read-only.
  - **Root gotcha:** running as root, the *no-write-permission* path won't trigger (root bypasses perms; a `chmod 444` file is still writable to root). Use a **library path** (`/usr/include/stdio.h`) or a **binary** to exercise auto-RO deterministically as root.
- **Behavior matrix — ALL PASS:**
  - default (auto_read_only unset/true) + `/usr/include/stdio.h` → read-only; typing → status `Editing disabled in this buffer`, content unchanged.
  - `auto_read_only:false` + same library file → **editable** (chars inserted, no "Editing disabled").
  - `auto_read_only:false` + binary file → **still read-only** ("Editing disabled"), tab shows `[BIN]` (doc edge case honored).
  - Palette **"Toggle Read-Only Mode"** ("Enable or disable read-only mode for the current buffer", builtin) → flips per-buffer; status `Read-only mode enabled` / `Read-only mode disabled`; after disabling, editing works.
- **BUG #2309 (filed Run #29):** the documented `[RO]` status-bar indicator (editing.md:42 + 0.2.18 blog: "The status bar shows `[RO]`") is **NEVER rendered** for ANY read-only buffer. Verified across 5 buffer types — auto library-path file, binary file, manual Toggle-RO, side-by-side `*Diff: f.txt*`, and `*HEAD:f.txt*` ("Opened HEAD version (read-only)") — each `grep "[RO]"` over the full screen = 0 (excluding false positives from file *content* that literally contains the text `[RO]`, e.g. an open copy of confirmed_bugs.md). Only feedback is transient status messages + the `[BIN]` tag.
  - **Correction to Run #27 note:** Run #27's "side-by-side composite buffer (`[RO]`)" and "HEAD buffer `[RO]`" annotations were *shorthand for "read-only"*, not literal captures — the binary does not render a `[RO]` segment on those buffers either.
- **Status-bar segment layout (220w):** `[trust]  [Local]  [Ln X, Col Y]  [transient message area]  [EOL: LF]  [encoding: UTF-8/ASCII]  [filetype: C/Text/Diff]  [LSP (off/on)]  [Palette: Ctrl+P]`. There is NO read-only segment.
- **tmux gotcha (palette double-`>`):** the command palette opens already in `>` command mode. Typing another `>` yields `>>…` and returns no results. Just type the command name directly (or BSpace once into file mode). Also `Shift+F1` to open *Keyboard Shortcuts* did not register over tmux send-keys (`S-F1`) — buffer stayed `[No Name]`.
- **git in this container:** commits require `-c commit.gpgsign=false` for local test repos (a signing wrapper otherwise fails with "signing server returned status 400"). pyright + pyright-langserver are on PATH; clangd is NOT installed.

---

## Wave Animation (Run #30)

**Command:** "Wave Animation" (command palette `Ctrl+P` → type `Wave` → Enter). Source: **builtin**. Description: "Send a wave through the editor — bounce all content up, down, and sideways". i18n keys: `cmd.wave_animation`, `cmd.wave_animation_desc`, `action.trigger_wave_animation`, `wave.triggered`. No default keybinding. Added on origin/master in 5 commits (66e1bcf06 → 232eceed7); NOT in CHANGELOG as of v0.3.12 @ 232eceed7. Docs: `docs/wave-animation-wireframe.txt` (user-facing ASCII wireframe — safe to read).

**Behavior (verified black-box):**
- Snapshots EVERY painted cell (menu bar, tab bar, gutter, text, status bar) into "ink" particles. A wave crest of glyphs `~ ≈ ∿` rises from the bottom edge; as it sweeps a row it kicks particles up + sideways (per-column L/R), which then spring back to home. Letters in tight words visibly spread apart mid-flight.
- **Runs until input** (commit "run until input") — does NOT auto-stop. Confirmed still animating >3s. The wireframe's "hard 2.5s cap / settles on its own" note is superseded.
- Status line shows `🌊 Wave! — press any key or move the mouse to stop` (the status bar itself jitters as particles during flight).
- **Any keypress stops it and is CONSUMED** — it does not perform the key's normal action and does not leak into the buffer. Verified: stopping with printable `Z` inserted nothing; stopping with `Ctrl+P` did NOT open the palette. Buffer is never marked modified.
- On stop, content settles back **exactly** to its original cells — no leftover artifacts, no corruption. Empty buffer handled gracefully (no crash).

**Testing tip:** It runs until input, so you have ample time to `capture-pane` many frames after triggering. Capture ~0.25–0.4s apart to catch distinct animation states (crest near bottom vs. content airborne near top). The status message is legible once stopped; mid-flight it's scrambled because the status bar is itself displaced.

**Verdict:** Comprehensive PASS, no bug. A purely cosmetic effect that correctly restores state and never touches buffer contents.

## Send Selection to Terminal (Run #31)

**Feature:** v0.4.0 (#1871, requested by @aquasync). CHANGELOG: "Terminal: send the selection (or current line) to the terminal". Two commits: `6ac61f927` (core) + `4b4d14946` (focus terminal after send). NOT yet documented in `docs/features/terminal.md`.

**Access:** Command palette `Ctrl+P` → "Send Selection to Terminal" (**builtin**, **no default keybinding**). i18n: `cmd.send_selection_to_terminal` / `cmd.send_selection_to_terminal_desc` = "Run the selected text (or current line) in the most recently used terminal"; `action.send_selection_to_terminal`; status `terminal.sent_selection` = "Sent to terminal %{id}". There is also a **right-click context "Terminal" submenu** (`menu.terminal.*`: Open / Close / Send Selection / Toggle Keyboard Capture) — NOT in the F10 menu bar (bar is File/Edit/View/Selection/Go/LSP/Help). Could NOT drive it via tmux (SGR mouse right-click not passed through — harness limitation; relates IMP-009).

**Behavior (verified black-box, v0.4.0 @ 1b5d7f8c8):**
- **No terminal open** → status "No open terminal — open a terminal first". No crash, does NOT auto-open a terminal.
- **No selection** → sends the CURRENT LINE. Text is sent WITH a trailing newline, so the shell EXECUTES it immediately (`first line text` → `bash: first: command not found`).
- **Selection (single line)** → sends exactly the selected line, executed.
- **Selection (multi-line)** → sends ALL selected lines; each is run individually (one prompt per line).
- **Selection (partial / sub-line)** → sends exactly the selected substring. Clean proof: select just `pwd` → terminal prints `/tmp/...`. Positive proof: select `echo "LINE-ONE-MARKER"` → terminal prints `LINE-ONE-MARKER`.
- **Targets the MOST-RECENTLY-USED terminal** ("terminal 0" here).
- **Focus moves to the terminal after send** (commit 4b4d14946) — VERIFIED definitively: a printable key pressed right after a send lands at the terminal prompt, not in the editor buffer.
- **Buffer is never modified** (no `*` on the tab, content intact) — sending is read-only w.r.t. the source buffer.
- **Pending terminal input is NOT cleared before sending.** Leftover unentered prompt text concatenates with the sent text (e.g. stray `CCC` + sent `pwd` ran as `CCCpwd`). This matches VS Code "Run Selected Text in Active Terminal" — **NOT a bug**.

**Verdict:** COMPREHENSIVE PASS, no bug, no false positive. One workflow-friction note (IMP-018): auto-focus-to-terminal forces a manual editor refocus before each subsequent send, unlike VS Code which keeps editor focus for rapid line-by-line sending.

**tmux gotchas (IMPORTANT — cost a bad capture this run):**
- After "Send Selection to Terminal", **keyboard focus is on the TERMINAL**. Any subsequent editor keystrokes (Ctrl+Home, Shift+arrows, etc.) leak into the terminal. ALWAYS re-focus the editor with **Alt+J** ("Toggle Utility Dock" focus) before the next editor operation.
- Verify editor focus BEFORE selecting: check the status-bar filetype (e.g. `Bourne Again Shell (bash)` for a .sh file) and confirm the selection highlight `48;5;17m` (blue bg) appears after Shift+End.
- **Alt+`** = "Open Terminal in Utility Dock" (bottom dock — editor + terminal both visible, ideal for this test). **Alt+J** toggles focus editor ↔ dock. **Ctrl+Space** toggles terminal input ↔ scrollback (but does NOT move focus to the editor — use Alt+J for that). "Focus Terminal" (palette) jumps into terminal input mode.

## Occurrence Highlight + Current-Line + Clear Search (Run #32, v0.4.0)
Three brand-new 0.4.0 features by @masmu. Read the PRs first to nail expected behavior — saved me from two false positives.

### Occurrence highlighting (#2154)
- Palette **"Toggle Occurrence Highlight"** (`cmd.toggle_occurrence_highlight`, builtin, NO default key). Config `editor.highlight_occurrences` (**enabled by default**). Status msg `view.occurrence_highlight_state` = "Occurrence highlight enabled/disabled".
- Highlights ALL occurrences of the **whole word** under the cursor (cursor on `items` does NOT match `item`).
- **BUG #2312:** highlight bg is a FIXED color **16** (near-black), NOT theme-derived. To prove visibility issues, use an ON/OFF differential ANSI capture (`capture-pane -e`, save to file, `diff`). In high-contrast (bg 16) the diff shows NOTHING changes on non-current lines → invisible. In light it's an inverted black box. Dark themes (bg 234/235) → subtle box, fine.

### Hide current-line highlight on selection (#2153)
- Config **`editor.hide_current_line_on_selection`** (Display section), **DEFAULT FALSE**. The CHANGELOG line "current-line highlight now hides while text is selected" is OPT-IN, not automatic — default behavior (highlight stays during selection) is CORRECT.
- When enabled: current-line bg (dark theme `235`) drops to non-current bg (`234`) the instant any cursor has a non-empty selection; returns to `235` when selection cleared. Also the gutter line-number fg reverts from the current-line color (`65`) to normal (`242`).
- Sibling: `editor.highlight_current_line` (the on/off for the highlight itself), `Toggle Current Line Highlight`, `Toggle Current Column Highlight` / `highlight_current_column`.

### Clear Search (#2152)
- Palette **"Clear Search Highlights"** (`cmd.clear_search`, NO default key). Clears active search highlights. Also exposes `has_active_search()` to plugins.
- **Find-close behavior map (important):** `Escape` closes the find bar AND clears highlights ("Search cancelled."). `Enter` closes the find bar but PERSISTS the match highlights and shows "Found N matches for '...'". `clear_search` removes those persistent highlights.
- The PR's "without closing the find widget" benefit is only reachable via a custom keybinding or a plugin: opening the palette closes the find bar first, and while the find INPUT is focused it swallows the keybinding (F8 ignored). → IMP-019.

### Keybinding config + Keybinding Editor save flow (learned this run)
- Config schema: `"keybindings":[{"key":"F8","action":"clear_search"}]` (top-level `keybindings` array).
- Editor flow: palette "Open Keybinding Editor" → `/` + action name → Enter (commit search) → Down to the row → Enter (Edit dialog) → Enter (capture) → press the key (shows `Key: F8`) → it stages (header `[modified]`, row Source `custom`) → **Ctrl+S** to persist ("Keybinding changes saved"). The Edit-dialog Save button alone does NOT write the config file. Footer help: `Enter:Edit a:Add d:Delete /:Search r:Record Key c:Context s:Source ?:Help Ctrl+S:Save Esc:Close`.

### Theme + Settings UI gotchas
- **Select Theme** list opens with the CURRENT theme pre-selected (NOT the top) — navigate relative to current. Order: dark, dracula, high-contrast, light, nord, nostalgia, solarized-dark, terminal.
- **Settings UI** (`Open Settings`): press `/` to search; each result shows its JSON path, e.g. `Editor > /editor/hide_current_line_on_selection`. Fastest way to find the TRUE config key/nesting for a setting (a flat top-level key that should be under `editor` is silently ignored).

## lsp_enabled master switch (Run #33)
**Feature:** 0.4.0 global LSP kill-switch (#1770, requested by @XhstormR). Docs: `docs/features/lsp.md` ("Disabling LSP") + `docs/configuration/index.md`.
- **Config key (IMPORTANT):** top-level **`lsp_enabled`** — root level, NOT under `editor`. `{ "lsp_enabled": false }`. Default (absent) = enabled.
- **Documented contract:** with `lsp_enabled:false`, "no language server auto-starts for any language (universal servers included)"; the status bar shows a "dimmed `LSP (off)` pill when servers are configured for the current language"; a manual start via palette **"Start/Restart LSP Server"** overrides the switch for that language. Settings UI: General section.
- **Black-box verification (pyright on a small Python project, `~/.config/fresh/config.json` = `{"lsp_enabled":false,"lsp":{"python":{"command":"pyright-langserver","args":["--stdio"],"enabled":true,"auto_start":true}}}`):**
  - **CONTROL** (omit `lsp_enabled`): on launch with `main.py`, status `LSP (python) ready`, right pill `LSP (on)`; `pgrep -f pyright-langserver` shows TWO procs (python wrapper + node child). Workspace auto-trusted (status `Trusted`).
  - **`lsp_enabled:false`:** launch → `pgrep -f pyright-langserver` count **0** (no auto-start), no "ready" msg, right pill `LSP (off)` (rendered for the configured Python language). ✓ matches docs.
  - **OVERRIDE:** with `lsp_enabled:false` still set, palette `Ctrl+P` → "Start/Restart LSP Server" (builtin, desc "Start or restart the LSP server for the current language") → pyright spawns (both procs), pill flips to `LSP (on)`, `LSP (python) ready`. ✓ manual override works.
- **Verdict: COMPREHENSIVE PASS, no bug.** All three documented behaviors correct.
- **Doc nit (R3, → potential_improvements IMP-020, NOT filed):** docs say "dimmed `LSP (off)` pill", but `tmux capture-pane -p -e` of the status line shows the off pill in the SAME default foreground as the on pill — no `[2m` dim attribute anywhere on the line, no distinct color; only the literal word `off`/`on` differs. The whole status bar uses bg `48;5;233`. So "dimmed" is inaccurate (purely cosmetic/doc; trivial).
- **Container LSP availability (Run #33):** `pyright` + `pyright-langserver` on PATH (`/root/.local/bin`, uv-installed); `rust-analyzer` on PATH (too heavy); clangd NOT installed; pylsp NOT installed. pyright initialize still succeeds (#2197 unchanged: requests time out, but server START is observable — perfect for auto-start tests).
- **tmux gotcha (Run #33):** compound bash lines mixing `tmux send-keys ... C-q`, `pkill`, and a heredoc sometimes abort the WHOLE script with **exit 144** before later commands run (config not written, session not killed). Run cleanup/`pkill`/quit as SEPARATE one-line Bash calls; write config files with the Write/Edit tool, not heredocs, to be safe.

## Configurable indent rules `[languages.<id>.indent]` (Run #34) — v0.4.0, **BUG #2314**
**Feature:** 0.4.0 headline — per-language auto-indent rules via `[languages.<id>.indent]`, "VS Code-style regex rules" (CHANGELOG 0.4.0). **Canonical doc:** `docs/configuration/index.md` → "Customize Auto-Indentation". Five optional fields, each a regex (regex-crate syntax, no look-around); comment/string spans blanked before matching; step = one `tab_size` unit:
  - `increase_indent_pattern` (matches reference line → new line +1 level, persists)
  - `decrease_indent_pattern` (matches new line's leading text → that line −1 level)
  - `indent_next_line_pattern` (reference line → next line ONLY +1, one-shot)
  - `dedent_next_line_pattern` (reference line → following line −1, one-shot)
  - `self_close_pattern` (reference line → cancels increase for that line)
- **VERDICT: BROKEN — all 5 patterns are NO-OPS. Filed #2314.** The custom-language `indent` block parses enough that the language is recognized (status-bar filetype shows the custom name + Trusted), but no pattern fires. Auto-indent falls back ENTIRELY to Fresh's built-in heuristics.
- **How to test it correctly (the trap):** the doc's own examples (`[\{\[\(]\s*$`, `:\s*$`) COINCIDE with Fresh's built-in bracket/colon indentation — so they *look* like they work even though the custom rule never fires. To isolate the custom rule, use a token the built-in does NOT recognize — **`OPEN` / `CLOSE` / `HDR` / `RET`** etc. Then the indent simply doesn't happen → proves the custom rule is ignored.
- **Proof built-in is what's running:** a custom language with NO `indent` block at all still indents after `:` and after `{` (and dedents `}`). And a custom language WITH an `indent` block STILL gets built-in colon/brace indent (so the block doesn't replace built-in either — it's just not consulted).
- **Scope:** confirmed at BOTH project `.fresh/config.json` and user `~/.config/fresh/config.json`. `self_close_pattern` is unobservable because it only modifies `increase_indent_pattern`, which never fires.
- **Config shape (validated):** `{"languages":{"<name>":{"extensions":["ext"],"tab_size":4,"use_tabs":false,"indent":{"<field>":"<regex>"}}}}` — matches the doc kotlin example exactly. JSON string ⇒ escape backslashes (`\\s`, `\\{`).
- **tmux gotchas learned this run (IMPORTANT, reusable):**
  1. **Editor cwd = the TMUX SHELL's cwd, NOT your Bash tool cwd.** `cd /tmp/foo && tmux send-keys '... fresh file'` launches fresh from the tmux session's shell (which was at `~/fresh`), so `/tmp/foo/.fresh/config.json` is NOT loaded and the file opens at the wrong path. FIX: `tmux send-keys -t S 'cd /tmp/foo' Enter` FIRST, then launch. Symptom of getting it wrong: status shows generic `Text` + `Restricted` instead of your custom filetype name.
  2. **`tmux send-keys 'end'` (and `down`/`up`/`home`/`tab`/`space`…) are interpreted as KEY NAMES, not literal text** — typing the word `end` instead jumps the cursor (End key). FIX: use `tmux send-keys -t S -l 'literal text'` (the `-l` flag forces literal) for any word that might collide with a key name. Words like `HEADER`/`body`/`return`/`child` are safe, but use `-l` by default for buffer text.
  3. **Verify indentation with `capture-pane -p | cat -A`** — the gutter is `│ N │ <content>`; indentation = spaces between the second `│ ` and content. `│ 2 │ child` = col 0; `│ 2 │     child` = 4-space indent.
  4. Status-bar `Ln/Col` lags one keypress after Enter (known #2301 family); rely on the buffer content capture, not the status readout, for indent assertions.

---

## Review Diff — reworked (Run #35) — v0.4.0 @ 1b5d7f8c8
**Flagship 0.4.0 feature. Comprehensive PASS + 1 bug (#2315).** Read CHANGELOG 0.4.0 + docs/features/git.md FIRST.

**Entry points (palette, all `audit_mode` source):** `Review Diff` (working tree), `Review Range (Commit or Branch)` (e.g. `main..HEAD`), `Review Stash` (prompts for ref, pre-filled `stash@{0}`). Opens a buffer named `*Review Diff*` / `*Review stash@{0}*`.

**3-pane layout:** `FILES` sidebar (left) │ unified/side-by-side diff (center) │ `COMMENTS` (right).
- **Sidebar** grouped by status — `STAGED (n)` / `UNSTAGED (n)` / `UNTRACKED (n)` — then by **directory** (`▾ src/core/  +A -B`), then file rows `M helpers.py  +2 -2` / `? new_feature.py  +2 -0`. Comment badge appended as `*N` (e.g. `M engine.py  +6 -1 *1`). Badge + grouping follow the file when staged/unstaged.
- **Header legend (2 lines):** `[Tab] focus  [, .] file │ [n] next hunk [p] prev hunk │ [s] stage [u] unstage [d] discard [c] comment` / `[1 2] split/stack [↑↓] move in panel [Enter] jump [Alt+o] open file │ [S U D] file-level [/] filter [?] help [q] close`.
- **Center breadcrumb (line 6):** `▸<GROUP> · <path>   +A / -B`; the `▸` marks the focused panel.

**Full keybindings (from `?` → `*Review Keys*` buffer):**
- Focus: `Tab`/`S-Tab` cycle files→diff→comments; `↑↓` (j/k) move within focused panel.
- Navigate: `n`/`p` hunks, `,`/`.` files, `]`/`[` comments.
- Fold: `za`/`zr` fold/unfold all; `Enter` folds one (on a header).
- Layout: `1`=split (side-by-side), `2`=stack (unified), `0`=auto. Status echoes "Side-by-side view" / "Unified view". Side-by-side KEEPS the sidebar + comments (the rework's key win over old standalone side-by-side); shows `OLD (HEAD)` | `NEW (Working)` aligned with blank-row padding.
- View: `a` show/hide inline notes; `/` filter files; `W` watch (auto-reload on save).
- Review: `c` add comment, `x` delete comment, `s`/`u`/`d` stage/unstage/discard (hunk or file under cursor), `S`/`U`/`D` whole file, `v` line selection.
- Open: `Enter` = side-by-side OR open the comment under cursor; `Alt+o` open working-tree file at line (needs cursor on a diff hunk line).
- Session: `r` refresh, `e` export, `q` close.

**Verified PASS:** word-level intra-line diff coloring (sub→mul, `return a - b`→`a * b`); file nav `,`/`.`; hunk nav `n`/`p`; layout 1/2; `/` filter (applies on **Enter**, not live-as-typed; header shows `FILES  /engine`; empty input + Enter clears → "Filter cleared"); `?` help; **stage/unstage `s`/`u`** (verified via `git status`: ` M`→`M `; sidebar regroups STAGED↔UNSTAGED with a **brief render lag** — re-capture after ~1s, NOT a bug; comment badge follows the file); **comments** — `c` prompts `Comment on L<n>:`, renders an inline bordered box in the diff + a `<file>:<line>` entry in COMMENTS panel + `*N` sidebar badge; **persist per-repo across editor restart** (store: `~/.local/share/fresh/audit/<sanitized_repo>/worktree.json`, full struct: id/hunk_id/file/text/timestamp/old_line/new_line/line_content/line_type); **export `e`** → `<repo>/.review/session.md` (Markdown: title+date, Summary files/hunks/files-with-comments, per-file `- **line N**: text` + code-line context); **Review Stash** (dedicated buffer, status "working tree not included", contents verified ACCURATE vs `git stash show`).

**BUG #2315 (filed Run #35, med):** Review Diff does NOT expand untracked **directories**. A new dir of files (`?? assets/` in `git status --short`) shows as `▾ assets/  +0 -0` header + a file row with a **completely blank name** (`   ?   +0 -0`, verified byte-for-byte via `capture-pane|cat -A`); center shows `(untracked directory)` placeholder (after `zr`), no hunks, `+0/-0`. Contained files never listed/reviewable. Contrast: untracked FILE in a TRACKED dir (`src/core/new_feature.py`) shows `+2/-0` + content correctly. Mirrors `git status --short` collapse instead of expanding like VS Code `-uall`. Contradicts docs ("everything ... untracked in the working tree").

**Pending items — ALL RESOLVED in Run #36 (see "Review Diff — residue resolved (Run #36)" section below):**
- **Delete comment `x`:** RESOLVED — it's a Delete/Cancel selectable menu (arrow+Enter), NOT a y/n prompt. PASS.
- **Watch `W`:** RESOLVED — in-editor edit→Ctrl+S→auto-reload confirmed. PASS.

**git/fixture gotchas:** commits need `-c commit.gpgsign=false` (signing wrapper else 400s). Launch the editor from inside the **tmux shell's** cwd (`send-keys 'cd /tmp/rdiff' Enter` BEFORE launching) so the repo loads. `git stash push -- <pathspec>` may still capture staged files in other paths — always verify stash contents with `git stash show` before asserting a Review-Stash discrepancy (avoided a false positive this way).

## Review Diff — residue resolved (Run #36) — v0.4.0 @ 1b5d7f8c8
**All Run #35 pending items closed. 2 NEW BUGS filed (#2317, #2318); watch/delete-comment/Review-Range all PASS.**

- **Delete comment `x` — RESOLVED, PASS (not a y/n prompt).** With the diff cursor on the commented line, `x` opens a **Delete / Cancel selectable menu** (palette-style; `Delete` highlighted by default, ANSI `48;5;25m`). Press **Enter** to confirm → status `Deleted`, COMMENTS panel clears, `*N` badge removed. Run #35's failure was treating it as a y/n text prompt and typing `y` (which got consumed as no-op → "Delete cancelled"). **Lesson: Review Diff confirm dialogs (delete-comment, discard hunk/file) are ALL Delete/Cancel selectable menus — arrow+Enter, never y/n.**
- **Watch `W` save→reload — PASS.** `W` → status "Watching for changes". Open the changed file in a normal buffer, edit it, **Ctrl+S** → the `*Review Diff*` buffer **auto-reloads** (README went +2/-0 → +4/-0 live, no manual `r`). Confirms reload is **save-triggered** (Fresh's own save events), matching docs; external fs edits are ignored (Run #35).
- **Review Range (`Review Range (Commit or Branch)`) — PASS.** Palette → prompts `Review (range A..B or commit SHA):` prefilled `HEAD`, with a **recent-commit picker** list. Entered `HEAD~1..HEAD` → buffer `*Review HEAD~1..HEAD*`, single group header `HEAD~1..HEAD (n)` (no STAGED/UNSTAGED/UNTRACKED), diff content matches `git diff HEAD~1..HEAD`. Status: `Review Diff: N hunks · working tree not included`. Header legend correctly **omits `[s]/[u]/[d]`** (only `[v] select [c] comment … [e] export`) — staging N/A for a historical range. Clean.
- **Hunk-level discard `d` — PASS (reconfirms #2117 fix in 0.4.0).** `d` on a hunk → Discard/Cancel menu → Enter → `Hunk discarded`, file reverted on disk, git status clears. No patch-apply error.
- **File-level discard `D` — partial.** On an **unstaged** file: PASS (`D` → "Discard changes in file" menu → reverts working tree to HEAD, `Discarded: <file>`, git clean). On an **untracked** file: menu says "Delete file" (deletes the new file). On a **fully-staged** file (`M `): **BUG #2318** — reports `Discarded: <file>` but the staged change PERSISTS (index + disk unchanged). `D` only ever touches the working tree, never the index. **`D` only fires when the cursor is on a hunk CONTENT row** (on a group/blank row it's a silent no-op — tmux gotcha; use `n` to jump to the hunk first).
- **`v` line-level visual stage/unstage/discard — BUG #2317 (all broken).** Help bar `Visual: j/k extend, s/u/d apply, Esc cancel`. With cursor ANSI-verified on a real `+`/`-` line: `v`+`s` and `v`+`d` → `Selection has no add/remove lines or crosses hunk boundary` (no-op); `v`+`u` → `Patch failed: … patch does not apply`. Tried single `+` line, `v`+`j` over full `-`/`+` change, and pure single-add — all fail. **Control: plain hunk `s`/`u`/`d` (no `v`) all work in the same session.** So only the `v` line-level path is dead.
- **Multi-line comment input — single-line BY DESIGN (not a bug).** `c` opens a one-line bottom prompt `Comment on L<n>:`; Enter submits. Docs call it a "line comment" — no multi-line promised, so absence is not a defect.

**tmux key gotchas (Review Diff):** `D`/`d`/`x` need the cursor on a **hunk content row** to fire. Palette opens in `>command` mode — to switch to **file** mode (`README.md`) or **#buffer** mode, send `C-u` then **`BSpace`** to delete the leading `>`, then type. Buffer-switch via palette `#<name>` then Enter works to hop between `*Review Diff*` and a file buffer for the watch test. `send-keys -l` for literal text that collides with key names.

## Per-buffer view toggles + #474 global persistence (Run #37 — v0.4.1 @ 42bfbb586)
Two NEW 0.4.1 palette commands (commit `93ac8d5ff`), both `builtin`, no default key:
- **"Toggle Line Numbers (Current Buffer)"** — "Show or hide line numbers for the current buffer only". Status: `Line numbers hidden` / (shown). Hides the `N │` gutter for the ACTIVE buffer only.
- **"Toggle Line Wrap (Current Buffer)"** — "Enable or disable line wrapping for the current buffer only". Status: `Line wrap disabled` / (enabled). When disabled a long line renders on ONE row (no continuation `│`-prefixed wrap row).
- **Defaults:** line numbers ON, line wrap ON. A 313-char line wraps to a continuation row by default; toggling wrap off truncates it to a single visible row.
- **Scope is genuinely per-buffer:** toggled file2 → gutter/wrap change; file1 (untouched) keeps the global default. Verified by switching tabs (Ctrl+PageUp/PageDown).
- **Persistence across restart (the headline):** per-buffer overrides are saved in the per-file workspace snapshot and re-applied on **session restore** — file2's hidden-gutter + no-wrap both survived a full quit (Ctrl+Q) + relaunch (NO `--no-restore`). A buffer with no override (file1) still follows the global default after restart.
- **GOTCHA — `--no-restore` skips SAVE too:** a session launched with `--no-restore` does not write the session, so the next launch has nothing to restore (you get `[No Name]`). To test any persistence/restore behavior, launch WITHOUT `--no-restore` on BOTH the setup run and the verify run.
- **GOTCHA — palette `C-u` clear is unreliable:** sending `C-u` then typing a fresh palette query produced a stale/garbled selection once (ran nothing useful → status `No selection`). Reliable pattern: open palette fresh with `C-p`, type a UNIQUE prefix of the command (e.g. `Toggle Line Numbers (Current`), confirm the single match via capture, then Enter.

**#474 — global View toggles now persist (commit `233c5cb64`):** the global **"Toggle Line Numbers"** (distinct from the "(Current Buffer)" variant; it's the top fuzzy match for `Toggle Line Numbers`) now writes `{"editor":{"line_numbers":false}}` to `~/.config/fresh/config.json` immediately, and the setting **survives restart** (a no-override buffer shows no gutter after relaunch). Previously this was runtime-only and forgotten on next launch. Commit also covers horizontal/vertical scrollbar toggles + Keybinding-Style menu persistence (not separately isolated this run).

## Review Diff line-level `v` ops FIXED + i18n key leak (Run #38 — v0.4.1 @ 205b9640e)
**#2317 CONFIRMED FIXED** (fix commit `a1d3e4352` "resolve line-level stage/unstage/discard hunk by id"; maintainer closed 2026-06-22). All three line-level visual ops now work; one new low-sev string bug found (#2420).
- **`v`+`s` stage — WORKS.** Cursor on a `+` line → `v` (status `Visual: j/k extend, s/u/d apply, Esc cancel`) → `s` → status `Lines staged`; `git diff --cached` shows exactly that one line staged (rest of hunk left unstaged). Works on a pure addition AND on the `+` half of a modification (stages it as an addition, keeps the old line).
- **`v`+`u` unstage — WORKS.** On a STAGED line → `v` → `u` → line returns to unstaged (`M ` → ` M`, `git diff --cached` empty). (To reach a staged line's content you must SELECT the staged file in the FILES sidebar first — the combined view renders STAGED files header-only until selected.)
- **`v`+`d` discard — WORKS for a pure addition** (line removed from working tree, git clean) **and for a modification when the full `-`/`+` pair is selected** (`v` on the `-` line, `j` to extend over the `+`, `d` → reverts to HEAD, git clean). **Does NOT work on the lone `+` of a modification** → `Patch failed: ... patch does not apply` (reverse-applying half a modification is ambiguous; likely expected — IMP-021, not filed).
- **BUG #2420 (low):** the line-level **discard** success message prints the **raw i18n key `status.lines_discardd`** (typo "discardd", double-d) in the status bar instead of a localized string. CONTRAST: stage shows proper `Lines staged`. Deterministic. Discard itself git-verified correct.

**tmux navigation lessons for Review Diff (CRITICAL — this run wasted effort on it):**
- **Focus cycles FILES → diff → COMMENTS via `Tab`.** The `▸` marker in the CENTER header (`▸UNSTAGED · file …`) shows the FOCUSED panel: when `▸` is on the center diff, Up/Down move the **line cursor**; when `▸FILES`, the same arrows move the **sidebar selection** (and the center cursor sits still). ALWAYS confirm `▸` is on the center before line-navigating.
- **Cursor row = ANSI bg `48;5;243`.** Verify the EXACT landing line with `tmux capture-pane -p -e | awk '/48;5;243/{...}'` before EVERY op — Down can skip a visual row near hunk/context boundaries, and landing on a CONTEXT line (not +/-) makes `v`+op fail with `Visual selection requires a diff line`. Do not trust Down/Up step counts.
- **STAGED hunks show file-header-only** (no content rows) in the combined STAGED/UNSTAGED view; to operate on a staged line, Tab to FILES, select that staged file (its content then expands in the center), Tab back to the diff, navigate.
- Reset the git fixture between op-tests (`git checkout -- f` / `git reset HEAD f` / recreate change + `r` to refresh) — a half-staged repo from a prior test produces confusing "patch does not apply" results that aren't real bugs.

## Keybinding Editor count after #2307 fix (Run #37 — v0.4.1)
- **#2307 FIXED.** v0.4.1 default-map first load: `Source: [All] 875 bindings` (was 866 in 0.3.12 — new 0.4.1 bindings), `[Plugin] 392/875`, `[Keymap] 261/875`, `[Custom] 0/875`. After a `default→emacs→default` keymap round-trip + reopen, the editor STILL shows **875 / Plugin 392** (the bug used to collapse to 547 / Plugin 0). Robust across a second round-trip via a different map (`default→macos→default`). Repro recipe unchanged from Run #28 (Edit menu or palette to open the editor; emacs palette = `M-x`; `s` cycles Source filter).

## Clipboard ANSI strip on default copy (Run #41) — v0.4.1 @ 33e2ed130 (commit 7d636f1de)
- **Feature:** ANSI-aware buffers (any non-binary file containing raw ESC `\033[..m` bytes) render those escapes as zero-width styling — the user sees colored text, not literal `^[[31m`. `7d636f1de` makes the **default copy paths strip the ANSI codes** so the clipboard carries the *visible* text; "Copy with Formatting" remains the styled variant.
- **How to reproduce an ANSI-aware buffer black-box:** `printf '\033[31mRED\033[0m middle \033[32mGREEN\033[0m end\n' > f.txt` then `fresh --no-restore f.txt`. Buffer shows `RED middle GREEN end` (no literal codes); `capture-pane -e` shows RED=`38;5;160`, GREEN=`38;5;2` etc. Status bar filetype = `Text`, encoding `ASCII` (NOT `[BIN]`).
- **Verifying clipboard content black-box:** copy → go to doc end (Ctrl+End) → Enter → Ctrl+V paste → Ctrl+S → inspect the appended line with `od -c` / `cat -A`. Pure-ASCII = stripped; presence of `033 [ 3 1 m` = retained. (Internal clipboard round-trips fine in headless tmux; no OS clipboard needed.)
- **Default copy = STRIP (verified Run #41):** selection copy (status "Copied"), whole-line copy (no selection → status "Copied line"), and block/rectangular copy (Alt+Shift+arrows → status "Copied") all paste escape-free visible text.
- **Block paste = column/rectangular insert** (VS Code #1057 semantics): a block copied with Alt+Shift selection pastes its rectangle across multiple lines at the cursor column — fragments interleave with existing lines. Expected, not a bug.
- **"Copy with Formatting" = KEEP styling (the contrast):** palette builtin "Copy selection with syntax highlighting colors (as rich text)". It opens a **THEME PICKER** (dark/dracula/high-contrast/light/nord/nostalgia/solarized-dark/terminal) — pick which theme's colors to style with. Status then "Copied as plain text" (TUI has no GUI rich-clipboard, so it falls back to a plain flavor that *embeds ANSI codes*). Pasting it yields `\033[31mRED\033[0m …` WITH escapes. So default-copy stripping is path-specific, not global.
- **tmux gotchas reused:** Ctrl+C/Ctrl+V are copy/paste; Ctrl+G→N→Enter = go-to-line (use it — bare Down×N from C-Home miscounted once); Shift+End on an ANSI line reports a logical Col that counts the ESC bytes (e.g. Col 39 for 20 visible chars) — the selection includes the escape bytes, and the copy strips them.

## vi mode "Vim compatibility options" + motions (Run #42) — v0.4.1 @ eb3a349e6 (commits b82b9b8b4, 471826514)
- **Two new master commits:** `b82b9b8b4` "feat: add Vim compatibility options to vi mode" + `471826514` "fix: align vi compatibility motions with Vim". Commit messages are bare (subject only, no body). Build from origin/master worktree `/tmp/fresh-master` (container was reclaimed; rebuilt from scratch ~from-scratch, exit 0).
- **Enable vi mode:** `Ctrl+P` → "Toggle Vi mode" → Enter. Status bar then shows `Vi mode enabled - NORMAL`, and (after later transitions) `-- NORMAL --` Vim-style. AutoStart is OFF by default (Settings → "Plugin: vi_mode" → AutoStart `[ ]`), so vi mode is per-session opt-in.
- **vi_mode Settings UI (Settings → left list bottom "Plugin: vi_mode") exposes ONLY 3 toggles:** `ArrowKeys` (on), `AutoStart` (off), `SearchWordUnderCursor` (on, `*`/`#`). There is NO visible "Vim compatibility" toggle in the Settings UI, no palette "compat" command, and nothing in docs (`docs/features/editing.md` §Vim Mode is a 1-liner). So the new "compatibility options" are not user-discoverable as settings — the change is to motion BEHAVIOR. (Discoverability/doc gap → potential_improvements, low-sev, not its own issue.)
- **BUG-016 / #2437 (med): `cw` eats the trailing whitespace** instead of acting like `ce`. Vim special case (`:help cw`): on a non-blank, `cw`==`ce` (change to end of word, KEEP the space). Fresh: `cw`+`X` on `hello world foo bar` → `Xworld foo bar` (space gone); `ce`+`X` → `X world foo bar` (correct). 2/2 at word start; mid-word (col 3) `cw`+`X` → `heXworld foo bar`. Control: `dw` correctly DOES eat the space (`world foo bar`) — only `cw` is wrong.
- **VERIFIED-WORKING motions (Run #42):** `$` lands on the LAST char (col 19 of a 19-char line, NOT col 20 — correct Vim, cursor can't sit past EOL); `l` stops at the last char (no past-EOL); `dw` deletes word+trailing-space; `gg`/`0`/`G` position correctly; `cc`/insert/`Esc`/`u` undo all work.
- **CRITICAL tmux harness lesson for vi motions (cost real time this run):**
  - **Verify motions by BUFFER-TEXT EFFECT, never by cursor polling.** `tmux display-message -p '#{cursor_x}'` gave LAGGED + DUPLICATED readings over tmux (e.g. `w` sequence read col 1,1,13,13 — every other press appeared to register). The physical cursor is repositioned a render behind. Use operator+insert-marker and read the resulting line: `cap(){ tmux capture-pane -t SES -p | sed -n '3p' | sed 's/.*│ //'; }`.
  - **Operator+motion is timing-sensitive:** sending `de` as ONE combined send-keys arg was a NO-OP; sending `d` then `e` as TWO sends ~0.4s apart over-deleted (`de`→` foo bar`, deleted two words). Inconclusive `d{motion}` results are usually HARNESS timing, not product bugs — only file when the buffer effect is STABLE across repeats. `cw`/`ce` were stable 2/2 (file-worthy); `de`/`dw`-via-split-send were flaky (NOT filed).
  - **Status-bar `Ln/Col` also lags one keypress** for vi motions (consistent with the known status-refresh staleness, #2301). Don't trust a single post-motion Col reading.
  - **Restore between tests** with a burst of `u` (6-8×) and re-verify the line is back to baseline before the next trial; over-undo is harmless and fully reverts.

## vi mode motion sweep — text-objects, counts, indent ops (Run #43) — v0.4.1 @ 8ee2baf31
- **Master @ 8ee2baf31** = Run #42's `eb3a349e6` + 1 commit `8ee2baf31` "refactor(editor): extract plugin loading out of with_options God constructor" (pure refactor, no user-facing change). Still v0.4.1. Continued the vi-compat motion sweep started in Run #42 (#2437 `cw`).
- **PASS / Vim-correct motions (do NOT re-test idly per R1):**
  - `diw` on a word → deletes the word, keeps surrounding spaces (`hello world foo bar` → `hello  foo bar`, 2 spaces).
  - `daw` → deletes word + 1 trailing space (`hello foo bar`).
  - `3dw` from line start → deletes 3 words (`bar`). Count + operator + motion works.
  - `2dd` → deletes 2 lines (linewise count works).
  - `ci"`/`di"` WHEN CURSOR INSIDE the quotes → works: `di"` → `the "" brown fox`; `ci"` → INSERT, replaces inner text.
- **BUG-017 / #2438 (med): indent ops `>>`/`<<` + visual `>`/`<` are NO-OPS.** NORMAL `>>`/`<<` change nothing (status stays `-- NORMAL --`); visual `V` then `>` leaves the line unchanged AND stays `-- VISUAL LINE --` (Vim indents + returns to NORMAL). `>` key transmits fine (literal `>` inserts in INSERT). Whole indent operator family unhandled, no feedback.
- **BUG-018 / #2439 (med): quote text-objects `i"`/`a"` don't search forward on the line.** `di"`/`ci"` from col 1 (before the quote, same line) = no-op (2/2); Vim searches forward on the current line. Only works cursor-inside. `"` transmits fine in INSERT.
- **Harness reconfirmations (cost time again):**
  - **Operator+motion MUST be sent as SEPARATE send-keys args ~0.4s apart.** Combined (`diw` as one arg) = NO-OP. This was the #1 false-no-op trap — always retry separated before concluding "no-op".
  - **capture-pane lags ~1 render.** A per-line `sed -n 'Np'` right after an edit can show STALE text (saw `one/two/three` post-`2dd` that was actually deleted). Re-capture the FULL pane after a ~0.5–1s beat to confirm.
  - **Status `Ln/Col` lags one keypress** (#2301) — don't trust a single post-motion reading; confirm cursor row by the edit's effect.
  - **`"` (and `>`/`<`) shift chars transmit fine via `send-keys -l`** — prove key transmission in INSERT mode before blaming the feature for a no-op.
  - Restore between trials with `u` bursts (over-undo is harmless); `ZZ` does NOT save-quit in Fresh's vi mode (typed `ZZZ` in NORMAL was a no-op, editor stayed open).

## vi mode motion sweep — find-char, paste, case ops, vertical clamp (Run #44) — v0.4.1 @ 3b8c2eca1
- **Master @ 3b8c2eca1** = Run #43's `8ee2baf31` + 5 commits, ALL e2e test-deflake fixes (`7b95b41b2`/`a50cfc4a8` open_folder instrumentation, `a430d2d71` loaded-parent wait, `36678bf8f` vi paste/dot-repeat wait, `3b8c2eca1` vi paragraph-motion wait). NO user-facing change. Still v0.4.1. Per R1 no open agent-issue fix landed → skipped rechecks; per R2 continued the vi-compat sweep.
- **BUG-019 / #2441 (med): find-char motions broken with operators + `;`/`,` no-op.** Pure `f`/`t` cursor motions WORK (e.g. `fr`→col9, `t,`→col before comma). But: (A) `d`/`c` + `f`/`t` HANGS in `-- OPERATOR (d/c) --` indefinitely — `dfr`/`dt,`/`cfr` all stuck, target char + even `Z` swallowed, only Esc recovers (reproduced at 0.4s AND 1.2s gaps, NOT a timing race). (B) when the find target is itself a motion key, the `f` is dropped and the op degrades silently: `dfw`→`dw` (deletes `hello `→`world foo bar baz`, NOT `hello w`). (C) `;`/`,` repeat-find = no-op (cursor never advances; proved via `fo`+`;`+`x` deleting col5 not col8).
- **BUG-020 / #2442 (med): vertical `j`/`k` onto a SHORTER line parks cursor 1 past last char → `x` joins lines.** `$` on line1 (col23, correct) then `j` onto `short` (5 chars) → status `Ln2, Col6` (=len+1, past EOL); `x` deletes the NEWLINE, joining line3 → `shorta longer line...`. Reproduced 3/3: `$`+j, `l`-to-col10 + j, `k` from line3. HORIZONTAL clamps correctly (`$` direct on `short`→col5→`x`=`shor`; `l` stops at last char) — so vertical column-clamp is off-by-one vs `$`/`l`. Data-corruption from navigate-then-`x`.
- **PASS / Vim-correct (do NOT re-test idly, R1):** `%` match-bracket (on `(` of `nested (parens [and brackets] inside) end` → jumps to the OUTER `)`, correctly skipping nested `[...]`; `x` confirmed); `r`+char (replace single char, `h`→`X`); `~` (toggle case of char + advance cursor); `p`/`P` linewise (`yy`+`p` duplicates line below / `P` above, cursor on new line, status `Pasted`); `p` charwise (`x` then `p` pastes after cursor: `hello`→`ello`→`ehllo`); `$` direct + `l` clamp to last char (no past-EOL horizontally).
- **NOT IMPLEMENTED (no-ops / fall-through; logged → potential_improvements IMP-023, candidate for one consolidated "vi missing standard commands" issue, NOT yet filed):** `R` (Replace/overtype mode) — `R` stays `-- NORMAL --`, then the next key runs as a normal command (observed `R`+`A`→append-at-EOL fired; `R`+`x`→deleted char). `gU`/`gu`/`g~` case OPERATORS — `gUw` left text unchanged, `w` just moved cursor (g+U swallowed). (`~` single-char DOES work; only the `g`-prefixed operators are missing.)
- **Harness notes (reconfirmed):** find-char + operator over tmux: the hang is REAL not timing (held at 1.2s gaps). Verify motions by buffer-text effect (`x`/`d` then read line), never cursor polling. `u` over-undo across many trials desyncs the buffer (a mid-sweep `R` test got a corrupted baseline) — when undo state gets murky, **`Ctrl+Q`→`d` (discard) and relaunch** for a clean fixture (file on disk stays pristine if never saved), then re-`Ctrl+P Toggle Vi mode`. Launch needs `send-keys -l '<binpath> --no-restore file'` then `Enter` (a `cd && bin` one-liner via send-keys sometimes didn't start the editor here).

## vi mode command sweep — o/O/s/S/D/C/Y, dot-repeat, search-word (Run #45) — v0.4.1 @ 1c6bd8ce9
- **Master @ 1c6bd8ce9** = Run #44's `3b8c2eca1` + 1 commit `1c6bd8ce9` "refactor(actions): extract inline editing arms from action_to_events" (pure refactor, no user-facing change). Still v0.4.1. Per R1 no open agent-issue fix → skipped rechecks; per R2 finished the vi-command sweep.
- **BUG-021 / #2443 (med): dot-repeat (`.`) of cursor-repositioning insert commands corrupts the buffer.** Canonical: `o`+`abc`+`Esc` (line2=`abc`, correct) then `.` → line2=`abhello world foo bar baz`, line3=`abcc` (an unrelated line's content is INJECTED instead of replaying `abc`; expected line3=`abc`). Reproduced across `o`/`A`/`a`: `A`+`X`→`shortX` then `.`→`shortshortXX`; `A`+`QZQ`→`shortQZQ` then `.`→`shortQZshortQZQQ`; `a`+`Y`(col1)→`sYhort` then `.`→`ssYYhort`. **Controls that WORK:** `i`+`AB`→`ABalpha...` then `.`→`AABBalpha...` (correct); `x`,`.`,`.` deletes successive chars (correct). So `.` replay is broken ONLY for the insert commands that reposition the cursor before inserting (`o`/`O`/`a`/`A`); `i`/`x` fine. Deterministic + selective-by-command ⇒ NOT a tmux timing artifact.
- **PASS / Vim-correct (do NOT re-test, R1):** `o`/`O` open-line below/above (enter INSERT); `s` substitute-char (delete char + INSERT, `s`+`X`→`Xello...`); `S` substitute-whole-line (clears line + INSERT); `D` delete-to-EOL (from col1 clears line keeping other lines intact, status `Cut`; mid-line at `world`→`hello `; undo restores correctly); `C` change-to-EOL (`hello NEWTAIL`); `Y` LINEWISE yank (Vim `Y`==`yy`; `Y` then `p` duplicates the line — NB `Y` is whole-line, not `y$`); `3G`/count+`G` (jump to line N, x-confirmed); `*` search-word-under-cursor forward (line1 `foo`→line7 `foo`); `#` search-word backward; `n`/`N` repeat-search fwd/back (verified across successive `foo` on line7).
- **Still NOT implemented (unchanged, → IMP-023, NOT filed):** `R` (Replace/overtype) and `gU`/`gu`/`g~` case operators. After this sweep the vi gap list is SMALL (just these two) — most common commands work — so no consolidated "missing commands" issue yet.
- **Harness lessons (reconfirm + new, IMPORTANT):**
  - Doubled-key commands (`yy`, `dd`, `gg`) MUST be sent as SEPARATE keystrokes. `'yy'` in ONE `send-keys -l` arg got stuck in `-- OPERATOR (y) --` (the 2nd `y` was swallowed); two separate `y` presses ~0.4s apart work and yank the line.
  - Status-bar `Ln`/`Col` lags ~1 keypress (#2301 family): after a motion the status often still shows the PRE-move position (e.g. `3G` showed `Ln 1` but `x` proved cursor on line 3; `*` showed `Ln 1` but landed on line 7). ALWAYS verify cursor by buffer effect (`x`/`d` then read the line), NEVER by the status line.
  - **FALSE-ALARM avoided (record this):** an early `w`→`D`→`u` sequence left the WHOLE buffer apparently empty and undo looked broken — I almost reported data loss. A clean controlled re-test from a fresh launch showed `D` (delete-to-EOL) AND undo are BOTH correct; the empty buffer was a one-off undo/status desync from a messy multi-undo state. Lesson: NEVER file from a murky multi-undo state — reproduce every suspected bug from a fresh `--no-restore` launch with keys sent individually before concluding.
  - Relaunch flakiness: after `Ctrl+Q`→`d` (discard) the editor sometimes drops to shell without restarting on the first launch command; and a leftover `Toggle Vi mode` string from a failed palette-open can pollute the next shell command line. Fix: `Ctrl+C` + `clear` + Enter before re-sending `send-keys -l '<bin> --no-restore file'` + Enter; re-enable vi via `Ctrl+P`→`Toggle Vi mode`→Enter (verify status `Vi mode enabled - NORMAL` before testing).
  - Trust dialog appears when launching with cwd at the repo root (detects Cargo.toml/.envrc/package.json). "Keep Restricted (K)" is sufficient for vi-mode testing (no tooling needed).

## Virtual space (Run #46) — v0.4.3 @ 9f6135001, PASS + BUG #2577
**Feature:** 0.4.3 headline. `editor.virtual_space` = `off` (default) / `on` / `block`. Emulates Visual Studio + Vim `virtualedit` (per CHANGELOG). Toggle per-buffer via palette **"Toggle Virtual Space (Current Buffer)"** (builtin, no default key, "Turn virtual space on or off for the current buffer only"); status `Virtual space: on (this buffer)`; persisted with the workspace (per docs — NOT verified across restart this run; use a non-`--no-restore` launch to test persistence).
- **Default OFF (baseline):** Right at EOL WRAPS to next line (`short` End=Col6 → Right → Ln3,Col1). Correct.
- **`on` mode (verified black-box):**
  - **Horizontal past EOL WORKS:** Right no longer wraps; cursor advances into the void. Typing materializes the position with **leading spaces** (6 Rights from col6 → type `X` → `short      X` = 6 spaces + X at col12). CHANGELOG match.
  - **Vertical below last line WORKS:** ArrowDown past the last line parks the cursor on virtual lines (screen cursor row advances). Typing there materializes the intervening lines as **empty newlines** + the text at the virtual col with leading spaces (2 Downs + `VOID` → empty lines 6-8 + `          VOID` at line 9 col 11). CHANGELOG "fills the gap with newlines and spaces" ✓.
- **BUG #2577 (low–med, FILED):** the status-bar `Ln, Col` readout FREEZES at the last real-text position while the cursor is in virtual space. Horizontal: `Col` frozen at the EOL col across ALL Right presses (never 7/8/9). Vertical: BOTH `Ln` and `Col` frozen. Only corrects when the cursor lands on real text. Distinct from #2301 (post-jump Ln staleness) in trigger + symptom.
- **HARNESS — how to read the true cursor position in virtual space (CRITICAL, since #2577 makes the status bar lie):**
  - `tmux display-message -t <sess> -p '#{cursor_x}'` / `'#{cursor_y}'` = the ACTUAL on-screen cursor cell (0-based). Gutter `  N │ ` occupies screen cols 0-5, so **text col = cursor_x − 5** on a 220-wide default layout (verify by pressing End on a known-length line and back-solving). This is the reliable ground truth for any virtual-space / cursor test.
  - OR type a single marker char and read where it materializes in the buffer (`capture-pane -p | cat -A` — count the leading spaces).
- `block` mode + click-past-EOL + persistence NOT yet tested → Run #47 NEXT.

## `[RO]` indicator FIXED (#2309) + occurrence-highlight #2312 STILL BROKEN (Run #46) — v0.4.3 @ 9f6135001
- **#2309 FIXED:** the documented `[RO]` status-bar segment now RENDERS (persistent, between filetype and `LF`). Verified on `/usr/include/stdio.h` (auto-RO library path): `[RO]` present, survives cursor movement; Toggle Read-Only Mode removes/re-adds it. Supersedes the Run #29 "no `[RO]` ever rendered" note — grep the status line for `[RO]` now works.
- **#2312 STILL REPRODUCES (issue closed as completed but bug persists):** occurrence-highlight background is STILL fixed palette index 16, not theme-derived. Re-proof method = ON/OFF differential `-e` capture, count `48;5;16mitems`: light theme → **4 ON / 0 OFF** (4 near-black boxes on a light bg); high-contrast → no change on non-current lines (bg 16 == editor bg 16 → invisible). Commented on the closed #2312 with the v0.4.3 evidence; did NOT re-file. Watch for a re-open.

## Configurable indent rules #2314 FIXED (core) (Run #46) — v0.4.3, extends Run #34
- **#2314 core FIXED:** custom-language `increase_indent_pattern` now fires. Re-ran Run #34's isolating method (non-built-in token `OPEN`): control line ("… no token") → next line at col 0; positive line ("… OPEN") → next line indented 4sp. In Run #34 both were no-ops. So the custom `indent` block IS now consulted.
- **Decrease nuance (murky, NOT filed):** `decrease_indent_pattern:"^\s*CLOSE\b"` did NOT self-dedent the matching `CLOSE` line (stayed at indent 4; VS Code / Fresh's own doc say the matching line dedents to 0). But in one capture the *following* line dropped to 0. Inconsistent across captures → per filing standards (don't file from a murky state) logged for a focused Run #47 characterization instead of filing. Re-test cleanly: pre-indent a line via OPEN, then on the next (inherited-indent) line type only `CLOSE` and check whether THAT line dedents.
- **Review Diff center-pane gotcha reconfirmed (v0.4.3):** the center diff expands ONLY the FOCUSED file's hunk; other files render header-only (`▾ file +N/-M` with no content). Navigate focus with `,`/`.` (the `▸GROUP · file` breadcrumb names the focused file) until the target file is focused, THEN its `-`/`+` lines appear and are line-navigable. Line-level discard success msg `Lines discarded` is briefly overwritten by the `Review Diff: Hunk 1 of 1` refresh status — capture at ~0.15s AND ~0.4s after `d`.

## Virtual space block mode + persistence + click-past-EOL (Run #47) — v0.4.3 @ 9f6135001, ALL PASS
**Completes the Run #46 virtual-space coverage. No new bug (Ln/Col freeze = existing #2577, also triggers on click-past-EOL).**
- **`block` mode (project `.fresh/config.json` → `{"editor":{"virtual_space":"block"}}`):**
  - Plain arrows do NOT go virtual: Right at EOL of `short` wraps to next line Col 1 (off-like). ✓ per CHANGELOG.
  - **Block selection (M-S-arrows) extends past line ends**: rectangle cols 4-10 over ragged lines renders highlight (`48;5;17`) through virtual cells (a 2-char line showed 7 highlighted virtual cells). OFF-mode control: same selection CLAMPS at EOL (no virtual highlight, block cursor clamps to short line's EOL) → config genuinely switches behavior.
  - **True-rectangle COPY**: Ctrl+C then paste → every fragment exactly rect-width; ragged rows PADDED with spaces (`rt    `), fully-virtual row = all spaces. Disk-verified via save + `od -c`.
  - **Paste with a single cursor** = VS Code column-paste (fragment 1 at cursor, rest inserted as NEW lines) — same as #1057/Run #41 established; NOT a bug, and NOT Vim-style into-column paste.
  - **True-rectangle INSERT (typing)**: with a block selection spanning virtual cells, typing `X` inserts at the block column on EVERY line, padding shorter lines (`ab` → `ab     X`). ✓
  - **Toggle cycle from block-config buffer: block → on → off → on…** ("Toggle Virtual Space (Current Buffer)" switches off↔on only) — a buffer can never RETURN to block mode via the toggle → IMP-024.
- **Per-buffer persistence across restart (NO `--no-restore`): PASS.** Toggle on → quit → relaunch: buffer restores with virtual space still on, and even the CURSOR'S VIRTUAL POSITION is restored (screen x showed col 8 on a 5-char line right after restore; status bar shows the frozen EOL col per #2577). Materialization proof: typed marker landed at the restored virtual col.
- **Click-past-EOL + click-below-last-line: PASS** (driven via raw SGR mouse — see harness lesson below). Click at text col 40 on a 23-char line parks the caret at col 40 (cursor_x-verified; typed `Q` landed at col 40 after 16 pad spaces). Click on a row below the last line parks the caret there; typing fills newlines + leading spaces exactly like keyboard vertical virtual space. Status bar freezes on click-past-EOL too (= #2577, commented nothing new).

## HARNESS BREAKTHROUGH — mouse IS drivable through tmux via raw SGR sequences (Run #47)
Previous runs recorded mouse as untestable ("tmux mouse passthrough" — Run #31 right-click, Run #24). WRONG for basic clicks: Fresh enables SGR mouse reporting, and `send-keys -l` can inject the raw escape bytes as if the terminal sent them:
- Left click at 1-based screen (COL,ROW): press `\e[<0;COL;ROWM` + release `\e[<0;COL;ROWm` (~0.1s apart): `tmux send-keys -t S -l "$(printf '\033[<0;%d;%dM' C R)"` then the `m` variant.
- Coordinate math on the default 220×50 layout: buffer line N is at SGR row N+2 (menu=1, tabs=2); text col C is at SGR col C+6 (gutter `  N │ ` = 6 cells). Verify landings via `tmux display-message -p '#{cursor_x}'` (0-based; text col = cursor_x−5).
- Opens up previously-skipped coverage: right-click menus (`\e[<2;…M`), Ctrl+Click (`\e[<16;…M` — modifier bit +16), drag (press, `\e[<32;…M` motion, release), scroll wheel (`\e[<64/65;…M`). Try these on the still-untested terminal Ctrl+Click / '+' tab popup / right-click menus.
- tmux gotchas this run: Go-to-line (`M-g`) prompt opens slowly and swallows/leaks early digits — type into it only after CONFIRMING `Go to line:` shows a typed char (`capture-pane` after each step), or navigate small files with arrows instead. A `C-v` sent while a prompt is open pastes the clipboard INTO the prompt (block clipboard flattens to one line there).

## Search & Replace match stepping — Ctrl+Alt+→/← (Run #48, 0.4.2 feature #2434)
- Palette names: **"Next Search Match"** / **"Previous Search Match"** (plugin `search_replace`; keybinding column shows `Ctrl+Alt+→/←`). tmux sends them fine as `C-M-Right`/`C-M-Left`.
- Works from editor focus AND with panel focused; each step opens the file in the source split, moves REAL focus to the editor (typing edits the buffer at the match immediately), status shows `Match N/M`, and the panel's list row highlight (`48;5;25`) follows the current match. Wraps at both ends. After the panel is closed (or no search yet): graceful status `No Search & Replace results — run a search first`.
- Panel focus dance: `Alt+A` focuses the Search field (Enter there RE-RUNS the search); `Alt+]` focuses the panel's match LIST (Enter there OPENS the selected row); Esc closes the panel only when it has focus — from editor focus Esc is consumed elsewhere.
- **BUG #2583:** positions are search-time snapshots. Any edit that shifts text (line inserted above, chars before the match on its line) makes later steps + Enter-open in that buffer land on the OLD line/col (verify with `#{cursor_x}/#{cursor_y}`, status agrees). Unedited files unaffected. Re-running the search fixes everything. When testing stepping flows, re-run the search after ANY buffer edit or your landings are garbage.

## Indentation guides (Run #48, 0.4.2 feature #2388) — COMPREHENSIVE PASS
- Config: `editor.indentation_guide` = `none` (default) | `all` | `active`; `editor.indentation_guide_glyph` (default `▏`). Settings UI: `/` search "guide" finds both; the mode is a dropdown (Enter opens; list order none/all/active; **the dropdown does NOT start on the current value's row — check the rendered `[value ▼]` after Enter, an overshoot silently selects the wrong mode**); `Ctrl+S` saves to the User layer (`~/.config/fresh/config.json` verified) and applies live.
- `all`: full staircase — one glyph per indent level at cols 1,5,9,… (tab-size-derived), continues through blank lines inside a block. `active`: ONLY the innermost guide of the cursor's enclosing block, spanning that block's lines; follows the cursor when it moves to another block. Glyph swap (`┊`) renders immediately.
- Visual-only contract holds: glyphs replace leading-whitespace CELLS only — Right/Home traversal is per-column as if spaces; saved file is pure spaces (od -c verified); guides never dirty the buffer.
- ANSI: guides render fg `38;5;59` (dim), text `38;5;231` — inherits whitespace-indicator color when the theme lacks `indentation_guide_fg`, per docs.
- Regression spot-checks of owner-filed fixes (closed 2026-07-06, in this head): **#2535 FIXED** (guides DO render on wrapped continuation rows, wrap_indent-aligned — verified in `active` mode); **#2564-shape** no column drift crossing a blank line inside a block (col 1 stays col 1); goal column correctly restores after clamping on a blank line (End col 25 → blank col 1 → next line col 21).
- Harness: `Home` toggles first-non-blank ↔ col 1 (probe cursor between EACH keypress — batching Home+arrows in one send-keys races and reads garbage). With wrap OFF and cursor at a huge column, the viewport h-scrolls and every short line renders EMPTY — looks like buffer corruption, is not; check the status `Col` before panicking.

## Mouse-driven UI sweep via the SGR harness (Run #49) — ALL PASS, harness extensions proven
Full mouse coverage delivered with `scratchpad/mouse.sh`-style raw SGR injection (`tmux send-keys -l` with `\e[<B;COL;ROW M/m`). Verified button/modifier codes end-to-end: left=0, right=2, Ctrl+left=16, Ctrl+right=18, drag motion=32 (button0+motion), bare hover move=35, Ctrl+hover=51, wheel up/down=64/65. Findings (v0.4.3 @ 89d91e84d):
- **'+' tab-bar button:** click opens the New Terminal/New File popup; **keyboard grab verified** (0.4.2 claim): printable keys do NOT leak to the buffer; arrows move highlight (`48;5;25`), Enter activates (both items verified working), Esc closes. The '+' col moves as tabs open — recompute from row 2 each time (python `s.index('+')+1` works while all tab chars are single-width; `×` IS single-width but 3 bytes, so `grep -bo` byte offsets mislead).
- **Tab context menu (right-click any tab, file or terminal):** Close / Close Others / Close to the Right / Close to the Left / Close All / Copy Relative Path / Copy Full Path. Keyboard grab verified; Esc closes; Copy Full Path clipboard-verified via paste; Close closes the right-clicked tab (not the active one). Menu is identical for terminal tabs.
- **Editor mouse:** plain click = exact cursor placement (text col = SGR col − 6 held); drag = char-precise selection (bg `48;5;17`); double-click = word select; **double-click+hold+drag = word-by-word extension** (docs table row verified — selection snaps to whole words). Editor-area RIGHT-click = deliberate no-op (no editor context menu; full-screen diff empty) → IMP-025.
- **Scroll wheel (editor):** 3 lines per notch, cursor does NOT move (viewport-only, VS Code convention).
- **Menu bar:** top-level menus open on click; menu ITEMS activate on click (Select All verified by 5-line selection).
- **Terminal Ctrl+Click paths (docs/features/terminal.md):** absolute path → Ctrl+hover underlines (`[4m` exactly spanning the path), Ctrl+Click opens the file ("Opened hello.py", correct filetype). **Relative paths need the shell to emit OSC 7** — plain bash doesn't, so relative paths are (correctly) not clickable until `printf '\033]7;file://%s%s\033\\' "$(hostname)" "$PWD"` runs; after that the same relative path underlines + opens against the announced cwd. NOT a bug — the docs name OSC 7 as the mechanism. Tab auto-title updates to the new cwd too.
- **Ctrl+Right-Click theme popup (themes.md):** works — "Theme Info" popup with Region, Foreground/Background theme keys + RGB swatches + "Open in Theme Editor".
- **GOTCHAS:** (1) after Ctrl+Click opens a file, FOCUS MOVES TO THE EDITOR — subsequent send-keys meant for the shell type into the buffer (I corrupted long.txt this way; undo is per-char). Re-click the terminal tab before typing. (2) Menu highlight capture lags one keypress (existing lesson holds for popup menus — trust the RESULT, e.g. status message, over the last highlight capture). (3) With a terminal focused, the command palette renders at the BOTTOM of the screen (rows ~45-50 on 220×50), not the top — capture the whole pane before concluding it didn't open. (4) `menu.terminal.*` i18n strings are unreachable in any menu (no Terminal menu bar entry) — don't hunt for a terminal right-click menu again.
- **0.4.2 split terminals:** palette "Open Terminal to the Right" → vertical split w/ own tab bar ("Terminal 1 opened"); "Open Terminal Below" from that pane → horizontal split nested in the right pane ("Terminal 2 opened"). Both PASS.

## Shader grammars GLSL/HLSL/WGSL/Slang (Run #50) — ALL PASS
- Fixtures: plain `.glsl`/`.hlsl`/`.wgsl`/`.slang` files opened with `--no-restore`. Status bar filetype segment shows `GLSL` / `HLSL` / `WGSL` / `Slang` respectively — extension mapping works with no config.
- Real syntax coloring in all four (ANSI-verified): keywords/types `38;5;51` (`uniform`,`vec4`,`cbuffer`,`float4x4`,`struct`,`fn`,`var`), comments `38;5;253`, numbers `38;5;69`, function names `38;5;226`, strings `38;5;34`, punctuation `38;5;251`, brackets rainbow-cycled. WGSL `@group/@binding/@compute/@workgroup_size` attributes and `var<uniform>`/`vec3<u32>` generics colored; Slang `[shader("fragment")]` attribute + semantics colored.
- Slang additionally shows an **`LSP (off)` status segment** (slangd integration registered; slangd absent in this container so Go-to-Def into builtin modules — 0.4.3 #2536/#2539 — remains UNTESTABLE here; GLSL/HLSL/WGSL show no LSP segment at all).

## File Explorer right-click context menu (Run #50) — menu works, keyboard-grab BUG #2587
- Menu (right-click any file/dir/empty space in the explorer): New File / New Directory / Rename / Cut / Copy / Paste / Delete / Duplicate / Copy Full Path / Copy Relative Path. Undocumented in docs/features/file-explorer.md; added by #1684 (0.4.1 "honoring the active multi-selection").
- Right-click SETS the tree selection (`48;5;17` marker) to the clicked row and opens the menu at the cursor. Menu actions act on the **live selection at Enter-time**, not a captured target.
- Verified end-to-end: Duplicate → `alpha copy.txt` (disk); Delete → `Delete file 'X'? (y)es, (N)o:` is a **typed prompt needing `y`+Enter** (single `y` shows in prompt, doesn't fire) → "Moved to trash: X"; New Directory → prompt PREFILLED `New Folder <unix-ts>` (`C-u` clears); Rename → prompt prefilled current name; New File → creates `untitled_<ts>.txt` then renames (status leaks "Renamed untitled_… to <name>"); Copy Full Path → absolute; Copy Relative Path → workspace-relative (clipboard-verified via paste). Empty-space right-click shows the SAME full menu acting on the current selection (VS Code shows a reduced menu — noted IMP-026, not filed).
- **BUG #2587 (med):** menu consumes arrows/Enter/Esc but NOT printable keys/Backspace — those hit the explorer type-ahead find under the menu (header `File Explorer` → `/x`), MOVING the selection behind the open menu, and the next menu action fires on the moved selection (right-click alpha.txt + type `sub` + Copy Relative Path → copied `subdir`). Filter persists after menu close. Contrast: tab context menu + '+' popup grab keys (0.4.2, re-verified Run #49).
- Explorer type-ahead baseline (useful): with explorer focused, typing letters = jump-to-match FIND (header `/str`, selection jumps, tree NOT narrowed); Esc clears it.
- Harness: explorer row SGR coords on 220×50 = tree line N at row N+2 (menu=1, panel border=2, root=3); after expand/create the rows SHIFT — recount from a fresh capture before every click. Menu-item navigation: highlight capture lags one keypress (trust the executed result). Item order fixed: New File(0)…Copy Relative Path(9) — N Downs + Enter from anywhere is reliable.

## vi j/k clamp #2442 FIXED + goal column (Run #50)
- `4e945b494` fixes #2442: `j`/`k` onto a shorter line clamps to the LAST CHAR (Col len), `x` deletes a char, never joins. Goal column now preserved Vim-style: col 10 → `j` onto 5-char line (col 5) → `j` onto long line RESTORES col 10.
- Palette-open gotcha (cost a relaunch): sending `Ctrl+P` then the query 0.6s later can TYPE the query into the buffer if the palette hasn't opened yet — ALWAYS capture-pane and confirm the `>` prompt row exists before typing the query.

## Monorepo/multi-repo workspace support (Run #51) — v0.4.3 @ 4e945b494, mostly PASS + 2 BUGS
Series `3a78ad5ec`…`0bfb54edf`. Two fixtures: **A)** root NOT a repo (`/tmp/mono51`: sub-repos `app` w/ 2-commit history + uncommitted add, sibling `app2` sharing the `app` path prefix, level-3 repo `svc/inner/deep3`, level-4 repo `lvl/a/b/deep4`); **B)** root IS a repo with nested sub-repo `vendored/` (uncommitted change in each).
- **Discovery contract characterized:** sub-repos at levels 1–3 below the root get explorer decorations (`app` ●, `app2` ●, `svc` ● from deep3); the level-4 repo (`lvl`) gets NONE — matches the BFS levels 1..=3 contract in the commit message. Not a bug.
- **PASS (fixture A, root-not-a-repo):** per-file explorer decorations scoped per repo (`app/main.py` M, sibling clean; `app`/`app2` prefix mis-grouping fixed — app2's stage never touched app); Live Diff gutter marks sub-repo changes; **Git Blame** in sub-repo correct (4 blocks) and **`b` history walk shows TRUE historical content** (depth 1 = `VERSION-ONE`, no working-tree bleed — the `<rev>:./<name>` fix verified); **Git Find File** lists repo-RELATIVE paths, opens the right absolute file, scopes to the active buffer's repo, graceful `0 items available` from a non-repo buffer; **Review Diff** scopes to the buffer's repo (app → only main.py; app2 → only mod2.py) and hunk stage/unstage git-verified in the right index; **Live Grep git-grep provider** resolves the buffer's repo.
- **BUG #2591 (med):** standalone **Git Grep** palette command still greps the WORKSPACE ROOT: root-not-a-repo → exit 128 `fatal: not a git repository` logged as plugin ERROR + `[⚠]` badge while the UI says only "No matches" (3/3); nested case → silently searches the OUTER repo (inner content unfindable). Also logs normal exit-1 no-match as ERROR. Contrast: git_find_file/live_grep/git_blame/audit_mode all resolve the buffer's repo.
- **BUG #2592 (med):** when the workspace root IS a repo, explorer decorations treat the workspace as single-repo: `vendored/inner.py` (modified in its own repo) gets NO decoration, `vendored` shows only outer's `U` — while gutter/blame/Review Diff on the same file all resolve the inner repo. Decorations work fine for sub-repos when the root is NOT a repo (fixture A).
- **IMP-027:** Review Diff from an outer buffer renders untracked nested-repo dir as `▾ vendored/ +0 -0` + a BLANK-named `? +0 -0` child row (#2315 shape; git can't expand a nested repo).
- **Harness:** the palette REFILLS `>` on every open — `C-p` then typing `>Cmd` yields `>>Cmd` and matches nothing (cost two dead Enters this run). Either `C-u` first then type `>Cmd`, or type the query without `>`. `[⚠ N]` status-bar badge = plugin warnings; read them via palette "Show Warnings" (opens a `warnings-<pid>.log` RO buffer; close with "Close Buffer" AFTER clearing the palette's `>`). Git Grep prompt is at the very bottom (below status bar), results in a dropdown above it; live results, no Enter needed.

## Theme color-transition animation + tab-switch slide + dotfiles reveal + GDScript (Run #52) — v0.4.3 @ 4e945b494
### Theme color-transition — DOCUMENTED BUT NEVER PLAYS → BUG #2594 (med)
- Docs promise it: `docs/configuration/index.md` §Screensaver "Switching themes plays a brief color-transition animation"; 0.4.0 blog "Animations framework — … a color-transition on theme switch (toggleable)". Two animation settings exist (Settings → Editor → Display): **Animations** `[v]` (default on; "frame-buffer animations — tab-switch slides, dashboard bringup, plugin-driven effects; false → every animation call a no-op") and **Cursor Jump Animation** `[v]`.
- Tested ALL FIVE theme-switch paths with a millisecond frame-burst harness (`scratchpad/burst.py`: send key, then loop `tmux capture-pane -e -p` ~4ms/frame for 200–300 frames, diff the color-code signature on a fixed line). EVERY path swaps colors instantly — old palette in frame N, final new palette in frame N+1 (4ms), ZERO interpolated frames over 1s+: (1) Select Theme picker live-preview (arrows); (2) picker apply (Enter, status `Theme changed to '<name>'`); (3) picker cancel/revert (Escape after previewing); (4) Settings UI Theme dropdown + Ctrl+S (`Settings saved to User layer`); (5) plugin API `editor.applyTheme("high-contrast")` from an init.ts-registered command.
- CONTROL proving the harness + terminal are fine: **tab-switch slide DOES animate** — Next Buffer (`Ctrl+PgDn`) shows buffer content horizontally offset ~16 cols for ~170ms (frames at 155–318ms) before settling. Truecolor works (modal dim renders `38;2;R;G;B`). So interpolated RGB frames COULD display; the transition just never fires. Filed #2594.
### Harness — measuring transient animations over tmux
- `scratchpad/burst.py <session> <outdir> <key> [nframes] [line]`: fires ONE key then burst-captures with `capture-pane -e -p` at ~4ms/frame (measured: 20 caps in 74ms), timestamps each frame in the filename, and prints the frames where the color-code signature on `line` CHANGES. If only 2 signatures appear (before/after) with no intermediates over a 1s window → no animation. Use a syntax-colorful fixture and a line with distinctive colors.
- Theme-picker gotchas: picker opens with CURRENT theme pre-highlighted (`48;5;25`), NOT top — navigate relative to current. Order: dark, dracula, high-contrast, light, nord, nostalgia, solarized-dark, terminal. Live-preview happens on every arrow move (instant). Enter applies; Escape reverts to the pre-open theme.
- Settings dropdown for Theme lists ONLY 5 (dark/light/high-contrast/nostalgia/terminal — a curated subset, fewer than the 8 in the palette picker). Committing the dropdown is PENDING; the theme applies on Ctrl+S (which also CLOSES Settings — a stray `/` typed after Ctrl+S leaks into the buffer; undo with Ctrl+Z per char).
- Plugin API path: write `~/.config/fresh/init.ts` with `globalThis.fn = ()=>{ editor.applyTheme("x"); }; editor.registerCommand("fn","Label","fn");` → appears in palette tagged `init.ts`. `editor.applyTheme(name)` swaps instantly (also confirms no transition on the programmatic path).
### GDScript (#2238, PR merged 0.4.2-era) — COMPREHENSIVE PASS
- `.gd` → status filetype **GDScript** with no config. Real tree-sitter coloring (ANSI): keywords `extends`/`const`/`var`/`func`/`signal`/`if`/`not`/`void` = `38;5;51`, comments `38;5;253`, numbers `38;5;69`, function names `38;5;226`, punctuation/brackets `38;5;6`, `@export`/`@` annotations colored, `→` tab indicators shown. Auto-indent after `:` works (typing `func x():`+Enter+`pass` → pass indented under func). Occurrence/reference highlight works (dark theme: `health` under cursor + all refs get `48;5;16` box — same feature as #2312, invisible in high-contrast). Godot LSP registered but **disabled by default** → `LSP (off)` segment (matches "disabled-by-default Godot LSP bridge" in the PR). Not filed — clean pass.
### Dotfiles reveal in Open File dialog (#2407, 0.4.2) — PASS, two mechanisms
- Baseline: Open File (`Ctrl+O`) at a dir lists only non-dot files. Header has an explicit **`☐ Show Hidden (Alt+.)`** checkbox.
- **Mechanism 1 (#2407):** typing a filter that STARTS WITH `.` reveals dotfiles inline (`/tmp/d/` list shows only visible files; typing `.` → `.config_secret`/`.hidden_rc` appear). Narrow further (`.hid`) then Tab-completes the path to `/tmp/d/.hidden_rc`; Enter opens it. PASS.
- **Mechanism 2:** `Alt+.` toggles the Show Hidden checkbox → dotfiles appear regardless of filter. PASS.
- Dialog Enter semantics (characterized, NOT a bug): when the text field is a bare directory path ending `/`, Enter opens the ARROW-selected list row; when the field holds a partial filename (e.g. `.hid`), Enter takes the FIELD literally and CREATES a new file with that name (standard "type a new filename to create" behavior). So arrow-select + Enter only works when the field is a dir path — otherwise Tab-complete first.

## JSONC config + macros save/promote + per-buffer view toggles (Run #53) — v0.4.3 @ 4e945b494
### JSONC config (#2497, 0.4.2) — PASS, and save is COMMENT-PRESERVING
- Config layers are JSONC (`docs/configuration/index.md`): `// line` + `/* block */` comments, trailing commas, and **lenient small-slip recovery** (a missing comma between entries OR an unquoted key still loads — verified: `editor:` unquoted + no comma before `line_numbers` → still applied `tab_size:9`). Applies to project `.fresh/config.json`, user `~/.config/fresh/config.json`, session, and `--config <file>`.
- **A genuinely broken file (unterminated object) is NOT silent:** loads defaults + raises `[⚠ N]` badge; "Show Warnings" log: `Failed to load layered config: Parse error: <path>: Parse error: Unterminated object on line L column C, using defaults`.
- **No-clobber on save (the #2497 headline):** with a broken config on disk, editing a setting in Settings UI + `Ctrl+S` → modal **"Couldn't save settings"** dialog (title `[×]`), body quotes the parse error + "Your config file was left unchanged so you can fix it. / Press Esc to open your config file and fix it." **File is byte-identical after the failed save** (md5-verify before/after). **Esc opens the broken config in-editor** (status "Editing User config: …").
- **Positive save preserves comments:** saving a setting into a valid commented JSONC user config does a **surgical value edit** (e.g. `tab_size 6`→`2`) and leaves `//` comments + trailing commas intact on disk — it does NOT strip-and-rewrite. (Exceeds the changelog claim.)
- Verify a config value fast: palette "Open Settings" → `/` search → e.g. "tab size" → Enter → read `Tab Size : [  N ]`. Tab in a text buffer inserts N spaces = another cross-check of `tab_size`.

### Macros: Save to init.ts / Promote to command (#2487, 0.4.2) — PASS
- Record: palette "Record Macro" → "Record macro (0-9):" prompt → digit + Enter → "Recording macro 'N' …". **F5** (or palette "Stop Recording") ends it → "Macro 'N' saved (K actions)". **F4** = Play Last Macro. Palette "Play Macro" → digit replays a specific register.
- **Macro: Save to init.ts** → "Save macro to init.ts (0-9):" → digit → writes a sentinel block to `~/.config/fresh/init.ts` (which it also creates from a rich starter template if absent) and hot-reloads: `// fresh:macro N — generated…` + `// types: "<typed chars>"` + `editor.defineMacro("N", [ …ActionSpec[]… ]);` + `// fresh:end macro N`. Steps are the literal recorded actions (`move_line_end`, `insert_char{args:{char}}`, `smart_home`, `move_down`, …). **Re-seeds the register at startup** — after restart, "Play Macro N" works with no re-record.
- **Macro: Promote to command** → "Promote macro to command (0-9):" → digit → **upserts the SAME sentinel block in place**, replacing `defineMacro` with `registerHandler("macro_N", async ()=>{ await editor.executeActions([…steps…]); /*arbitrary logic*/ })` + `editor.registerCommand("Macro N","Run promoted macro N","macro_N")`. Promoted command appears in the palette tagged source `init.ts` and runs the steps. Sentinel-delimited = re-running either command rewrites its block, not appends.
- init.ts errors surface as `[⚠]`; a clean load shows none. `init: Reload` / `init: Check` in the palette.

### Per-buffer view toggles (0.4.2) — PASS + restore nuance
- Two variants each: global "Toggle Line Numbers"/"Toggle Line Wrap" AND "… (Current Buffer)". The Current-Buffer variant is the SECOND palette match — arrow Down once past the global one before Enter (both share the `Toggle Line Numbers` prefix; the global is pre-highlighted).
- **Active-buffer scope confirmed:** hiding line numbers on buffer A leaves buffer B's gutter intact; disabling wrap on A (a 300-char line stops wrapping) doesn't touch B. Status strings: "Line numbers hidden"/"Line wrap disabled".
- **Persistence works ONLY via pure session restore.** Toggle off → quit → relaunch `fresh` **with NO file argument** → the buffer reopens with the toggle still applied (both line-numbers-hidden and wrap-off verified independently). 
- **CRITICAL harness nuance (not a bug):** relaunching with an EXPLICIT file arg (`fresh long.txt`) opens a **fresh buffer with DEFAULTS** — per-buffer view state is NOT restored on that path (gutter + wrap both return). Only the no-arg pure-restore path restores view state. Do NOT misread the file-arg revert as a persistence bug — it's the standard "open a file" vs "restore workspace" distinction (VS Code behaves the same). `--no-restore` on the toggle session ALSO defeats the test (it likely skips saving session state too) — use plain `fresh` for any persistence test.

## Large-file 10MB threshold + on-demand line-index scan + rust-analyzer inlay hints (Run #54)

**Large-file mode contract (v0.4.3, post-#2540):**
- Single setting `editor.large_file_threshold_bytes` (default 10MB) decides eager (line numbers, LSP, exact scrollbar) vs lazy (byte-offset gutter `0/40/80…`, status `Byte N`, LSP skipped). Verified both directions with a custom config value (1MB → 2MB file goes lazy; 20MB → 12MB file goes eager).
- In large-file mode: `Ctrl+G` prompts `Scan file for exact line numbers? (y/N)` (prompt row renders BELOW the status bar); `n` falls back to `Go to byte offset:`; `y`+Enter → `Line index built successfully` → real line numbers. Palette "Scan Line Index" does the same directly. 150MB scan = seconds.
- **#2596:** FIRST scan's TOTAL line count can be wrong (12MB fixture: 299828 from-top 3/3, 298189 from-EOF 2/2, truth 300001) → Go to Line clamps below the real end; EOF row mislabeled; status vs gutter can disagree on the same position. RE-SCAN corrects (2/2). Intermediate line→content mappings are exact even when the total is wrong. When testing line-number features on >10MB files, run "Scan Line Index" TWICE before trusting the max.
- **#2597:** palette `:N` in an unscanned large file = bogus jump to EOF + "Jumped to line 1". Use `Ctrl+G` there instead.
- Fixture recipe: `python3` fixed-width 40-byte numbered lines (`('line %06d '%i).ljust(39,'x')+'\n'`) — makes every line self-identifying so gutter/jump correctness is assertable from content alone.

**rust-analyzer harness (unblocks LSP testing in this container):**
- `rust-analyzer` on PATH is a **rustup shim**; the component may not be installed → Fresh shows `LSP (error)` + warnings `LSP server closed stdout (EOF)` (shim prints an error and exits). Fix: `rustup component add rust-analyzer` **from the test project's directory** — the fresh repo pins its own toolchain, so installing from `/home/user/fresh` does NOT cover a `/tmp` crate using `stable`. Verify with `cd <crate> && rust-analyzer --version` before blaming Fresh.
- Flow: open `src/main.rs` → Trust dialog `T`+Enter (`Trusted` status segment) → palette `>Start/Restart LSP Server` (after `C-u` you must retype the `>` — input reverts to file mode!) → `LSP (on)` in ~10s on a tiny crate; hints appear a few seconds later.
- rust-analyzer emits type hints (`let x: i32`) AND parameter-name hints at call sites; hints render fg `38;5;244` (dim) vs code `231` — assert hint-ness via ANSI, and hint-freeness of the buffer via `Ln/Col` (real columns only) + no `*` dirty marker.
- **Inlay hints + wrap/scroll (#2190) verified fixed:** hint widths count in the wrap decision (a 41-char line wraps at 60 cols because rendered width ≈ 90), continuation rows carry hints at correct spots, no dropped text (character-account the wrapped rows minus hint strings = source); no-wrap End scrolls the true EOL + cursor into view.
- `tmux resize-window -x 60` is an effective wrap-forcing tool; the status bar ellipsizes (`Col...`) at narrow widths — widen before reading Ln/Col.
- GOTCHA (cost a corrupted-buffer detour): before sending a launch command line to the tmux session, CONFIRM the previous fresh instance actually exited — a still-running editor eats the command line as buffer text (undo + `C-q` + `d` recovers; the discard prompt is `(s)ave/(d)iscard/(q)uit/(C)ancel`).

## Rust LSP request deep dive — hover/def/refs/impl/rename/completions + Reduced Memory mode (Run #55)

**Feature results (v0.4.3 @ 4e945b494, rust-analyzer 1.94.1, Full mode — the #2197-blocked request family, ALL PASS):**
- Fixture: `/tmp/ralsp` trait crate — `Shape` trait + `Circle`/`Square` impls in shapes.rs; `describe()`/`area_sum()` + `println!` call sites in main.rs. Cross-file refs by design; every request type has a target.
- **Hover Alt+K:** popup `Hover [Alt+T to focus]`, body = crate name + italic signature (`fn describe(shape: &dyn Shape) -> String`). Esc dismisses.
- **F12:** same-file and cross-file both land at symbol start + `Jumped to definition at <abs path>:<line>`; cross-file opens a new tab. `Ln` updates immediately (no #2301-family staleness on this path).
- **Shift+F12 on trait method `area`:** popup `References to 'area' (6)` = declaration + both impls + all call sites, with TWO rows for two refs on one line (`a.area() + b.area()`); Enter on a row cross-file opens at exact ref column (`Opened <path>:<line>`).
- **Ctrl+F12 (textDocument/implementation, 0.4.1 `6df567f04`, first-ever test):** on trait NAME → `Implementations of 'Shape' (2)` (both `impl` blocks); on a METHOD CALL through `&dyn Shape` → `Implementations of 'area' (2)` (both impl fns); Enter jumps.
- **F2 rename:** prompt `Rename to:` prefilled with symbol; cross-file rename applies atomically (`Renamed successfully (4 changes)`, both buffers dirty); undo reverts the rename's edits in-buffer; F2 on whitespace → graceful `Cannot rename: LSP error ... prepareRename ... (code -32602)` and NO prompt opens (logs one ⚠ warning). BUG #2599: cross-file rename from a use site steals focus to the def's file at that buffer's STALE cursor pos (def-site + single-file invocations stay put).
- **Ctrl+Space completions:** `c.` on a Circle binding → `radius f64 (Tab)` + `λ area/name(as Shape)` + blanket `try_into`/`into`; Tab accepts and inlay `: f64` updates immediately. NO auto-popup on `.` — documented default ("explicit only", editing.md §Basic Completions) — NOT a bug.
- **"Rust LSP: Configure Mode" (docs call it "Switch Rust Analyzer Mode" — name mismatch, in #2598 + potential_improvements):** dialog `Rust LSP Mode` = Full / Reduced Memory / Cancel; switching restarts the server and prints a mode status line; mechanically works both directions.

**BUG #2598 (med): Reduced Memory mode = macro blindness.** Warmed (60s–5min): hover/F12 dead on `println!` itself and on ANY symbol inside `println!`/`format!` args ("No definition found"/"No hover information available"); bare calls, struct literals, fn defs, `self` all fine seconds apart; Full mode fixes the same position within seconds. Plus: EVERY reduced-mode (re)start has a ~30–60s window where ALL requests return empty while status claims `LSP (rust) ready` (tiny crate!). Testing lesson: in reduced mode, distinguish the transient all-dead warm-up from the permanent macro-position failures — retest at 60s+ and 5min before concluding.

**Harness lessons (cost real time):**
- **NEVER type into a prompt you haven't confirmed on screen.** F2 with cursor on whitespace shows the error WITHOUT opening the prompt — my blind `C-u`+text+Enter went into the BUFFER (deleted to EOL via C-u? + inserted text + split line). Recover: `C-z` per keystroke (undo is per-char; count your typed chars, re-capture after each few). Same rule as the Run #54 launch-line gotcha: capture → confirm `Rename to:` → then type.
- **Go-to-line preserves the previous COLUMN** (`Ctrl+G N` keeps Col from before the jump, it does not Home). Always `Home` first, and remember Home is a smart-home (first non-blank, col 5 on a 4-indent line) — count Rights from there, and re-read `Ln, Col` from the status bar before firing the request. A drifted cursor (Right at EOL WRAPS to the next line) produced a phantom "format!-position works" data point that was really F12-on-`self` in the OTHER FILE (I'd forgotten a cross-file F12 had switched buffers). Verify WHICH buffer is focused (`sed -n '2p'` tab line: the `main.rs* ×` entries don't show focus in plain capture — use content) before interpreting a MURKY result.
- **LSP status panel ("Show LSP Status"):** rows = `● rust-analyzer (ready)` / `Restart rust-analyzer` / `Stop rust-analyzer`; my Down+Enter hit STOP (selection starts on Restart? unverified) — palette `>Start LSP` recovers. Panel claims "ready" even while requests return empty (reduced-mode warm-up) — don't trust it as a request-readiness signal.
- **Palette fuzzy ranking:** query "Switch Rust" ranks "Switch to Previous Tab" ABOVE "Rust LSP: Configure Mode" — arrow down + ANSI-verify (`48;5;25m`) the highlighted row before Enter.
- rustup component install from Run #54 confirmed again: `rustup component add rust-analyzer` from the CRATE dir (not the fresh repo), verify `rust-analyzer --version` before launching.

## LSP diagnostics deep-dive + signature help + code actions + auto-import (Run #56) — v0.4.3 @ 11dccfad5, rust-analyzer 1.94.1 Full mode

**Feature map (fixture: tiny crate, struct Point + describe(p: &Point, verbose: bool)):**
- **Diagnostics channels:** RA-native (missing-fields) publishes LIVE while typing (~2-3s, no save). rustc/flycheck (unresolved field, unused-var W, hints H) publish on SAVE via checkOnSave. Same defect can appear TWICE at one position (per-server tracking): F8 visits each, status echoes each message.
- **Severity presentation:** status segments `E:n W:n I:n` (hints count under I, panel tags [H]); hover fusion `✖ Error`=bold 203 / `⚠ Warning`=226 + `(rustc)` source tag; inline text (`diagnostics_inline_text`, Settings UI) severity-coloured 203/226, right-aligned, highest-severity per line. GUTTER ● is ALWAYS red 160 regardless (nit, IMP-029).
- **Panel** (`Show/Toggle Diagnostics Panel`): read-only buffer in the dock; rows are cursor lines — Enter on a `[E] line:col` row jumps (exact col) + focuses editor + CLOSES panel; Enter on non-item row → "No item selected". `q`/Esc close. `a` toggles Current-File↔All-Files (header text does NOT update — verify by list content/status msg). Up/Down PREVIEW-scrolls the editor; previewing another file's row OPENS it as a real tab (left open after).
- **F8/Shift+F8** cycle with wrap, no panel needed.
- **Clears-on-fix:** live for RA-native (undo the bad line → E count drops in ~3s, no save).
- **Signature help:** auto-triggers on typed `(`; unfocused popup; layout = signature / blank / ACTIVE-PARAM LINE / rule / markdown docs. Param line advances on `,`. NOT an inline-bold presentation — don't report "no active-param highlight" (Run #56 near-false-positive from a truncated capture).
- **Code actions Alt+.:** number keys AND arrows+Enter both accept. rust-analyzer supplies BOTH diagnostic quickfixes (Fill struct fields) and assists (Generate impl/new etc.); WorkspaceEdits apply atomically with `Applied: <title> (N change(s))` status. clangd diagnostics-context gap #2212 is clangd-specific.
- **Completions:** unimported symbols NEVER appear (no flyimport) → #2603; members/prelude/qualified paths fine. Don't conclude "completion broken" without those controls.

**BUGS:** #2601 hover-dismiss (Esc or cursor-move) wipes non-overlapping ERROR diagnostics from gutter/status/F8 until next save (panel keeps them; hover-on-the-error preserves). #2602 positions frozen at publish — edits above (incl. editor-applied code actions) leave markers/F8 on stale lines until save. Retest hooks for both: save restores everything.

**Harness (BIG):**
- **tmux swallows a trailing unescaped `;`** in `send-keys` — bare `';'` arg AND `-l 'text;'` both lose it (tmux command separator). Use `tmux send-keys '\;'` (escaped at tmux parse level). Symptom: "the `;` key is dead" — it never reached the app.
- Fixture reset: prefer palette **"Revert File"** (prompt `Buffer has unsaved changes. (r)evert, (C)ancel?` — needs `r` THEN Enter) over undo bursts: per-char undo happily rolls back code-action WorkspaceEdits and prior test edits (silently corrupted the fixture twice). BSpace bursts likewise overshoot across line joins — count exact chars or Revert.
- Settings UI: `/`-search → Down to the result row → Enter jumps to the setting; Enter toggles a checkbox `[ ]`→`[v]`; Ctrl+S "Settings saved to User layer"; REMOVE `~/.config/fresh/config.json` at cleanup.
- Palette-open race hit again — ALWAYS capture and confirm the bare `>` prompt line before typing the query.

## Format Buffer + formatter config (Run #57) — v0.4.3 @ 6ab255709, rust-analyzer 1.94.1

**Feature map ("Format Buffer" palette cmd, builtin, "Format the current buffer with the configured formatter"):**
- **Default path is the EXTERNAL formatter, not LSP:** rust (and per CHANGELOG js/ts/py/c/c++/go) ships a built-in default formatter. Status `Formatted with rustfmt` appears even with the LSP STOPPED. Do not attribute default-config formatting results to the LSP path.
- **Formatting is applied as ONE undoable buffer edit; disk is untouched until save.** Proven with a marker script that rewrites `$1` in place: buffer got the marker, disk did NOT (Fresh formats a copy and diffs the result back in). Ctrl+Z fully reverts + clears dirty flag.
- **Config shape:** `languages.<id>.formatter` = **struct** `{"command": "...", "args": [...]}`; the buffer's file path is auto-appended to args (language-packs.md). The `"formatter": "prettier --write"` STRING form shown in docs/configuration/index.md:203 is INVALID → `Parse error ... expected struct FormatterConfig, using defaults` — the ENTIRE layered config is discarded (⚠ badge + warnings log). IMP-030.
- **`formatter: null` does NOT unset the built-in default** (field-level merge treats null as absent) → there is no reachable "none is set" state for rust; the docs' LSP-formatting fallback incl. RANGE formatting can never run (→ #2605). `""`/nonexistent command → graceful `Format failed: Formatter 'X' not found`, NO LSP fallback even with a ready server.
- **Selection + Format Buffer = whole-file reformat** (selection silently ignored; cursor to end of former selection) → #2605.
- **No formatter + no LSP** (Text buffer): graceful `Formatting not supported by LSP server` (copy slightly off — no server is even configured — nit).

**Harness:**
- Vi-recheck measurement traps (cost 2 confused rounds): (1) sending `d f w` in ONE send-keys call races undo/verify captures — for operator sequences send ONE key per call with ≥0.4s gaps and verify buffer text between steps; (2) status-bar Ln/Col + mode segment lag ~0.5–1s behind the buffer — poll twice before concluding a motion failed (a `;` "no-op" was really status lag; the buffer `x`-proof showed col 8 was reached).
- Trailing `;` swallow struck again typing Rust via `-l 'let x=compute(3,4);'` — line arrived WITHOUT the semicolon (→ RA error + signature-help popup). Append it separately via `tmux send-keys '\;'`.
- Palette prefills `>` on open (again): file-mode paths need `C-u` first; palette file mode can NOT open absolute paths outside the workspace (`No selection`) — use **Ctrl+O**, `C-u`, type the absolute path (creates missing dirs on save via `(c)reate` prompt; `Directory 'src/bin' does not exist. (c)reate, (A)bort?` needs `c`+Enter).
- Workspace Trust dialog appears on FIRST launch per cwd (radio `(*) Keep Restricted` default; `T` selects Trust, then Enter = OK). Trust persists across relaunches in the same cwd. LSP needs Trusted.
- `rustup component add rust-analyzer` needed again after container reclaim (from the crate dir). Two.rs finding: new `src/bin/<name>.rs` created mid-session DID get full diagnostics (E:2 ~2s after save, ●, F8, clears-on-fix) — Run #56's no-diagnostics observation not reproducible; treat as transient RA workspace-reload race unless seen again (then capture the LSP log via status-popup → view log).

## Code folding deep-dive + vi visual-block sweep (Run #58) — v0.4.3 @ 6ab255709

**Folding feature map (docs: editing.md §Code Folding, lsp.md §Code Folding, 0.2.9 blog):**
- Two modes, both live: LSP `foldingRange` when server ready; indent-based fallback otherwise AND in large-file mode. **Litmus between them:** the `use`-imports block gets ▾ ONLY with LSP on — indent fallback can't produce that range. Fold indicators appear pre-LSP too (fallback), so "indicators visible" alone does not prove LSP folding.
- Gutter glyphs at SGR col 1: ▾ expanded / ▸ collapsed; collapsed row renders `▸N │ <header> { ...` and line numbering continues after the hidden range (LSP fn ranges end BEFORE the closing brace, so `}` stays visible).
- "Toggle Fold" = palette builtin, no default key. On a blank/non-foldable line: silent no-op (no status message).
- **Fold-from-within moves the cursor to the header** (VS Code-like) but the status bar keeps the OLD hidden `Ln` while `Col` refreshes → #2301 family (comment added, 3rd trigger). `tmux display -p '#{cursor_y}'` is the reliable read; typing before any cursor move edits the HEADER line while status claims the hidden line. Folding from the header itself doesn't desync.
- Up/Down skip folded regions bidirectionally; from a hidden-cursor state Down → first visible line after the fold, Up → header. Works in large-file mode too (byte-gutter offsets skip whole blocks).
- Gutter mouse click toggles: `send-keys -l $'\033[<0;1;ROWM\033[<0;1;ROWm'` (line N at row N+2 on 220x50). Both directions verified.
- Edits above a fold: header + indicators track, content intact after unfold, undo clean (#1571 fix holds).
- Per-split fold state independent (Split Horizontal = stacked panes; fold in one, other unaffected). Closing the folded split leaves the survivor unfolded.
- Wrap: folded regions hide their wrapped continuation rows correctly. NIT (IMP-031): when the HEADER line itself wraps, the `...` is appended to the header's FIRST visual row and the continuation row still renders below it.
- Session persistence: folds (incl. LSP-only ranges) survive quit + pure-restore relaunch; after an external edit while closed, folds relocate by header text (#1568 holds — sed-inserted 2 lines, fold followed `fn magnitude` 14→16). Unfolding a persisted LSP fold with LSP off removes the indicator (no indent range there).
- Nested fold memory: inner fold state preserved across outer fold/unfold.
- A diagnostic ● on a hidden line is not surfaced on the fold-header row while folded; status severity counts unaffected (minor, unfiled).

**vi sweep results:** `d2fo` PASS (count+find under operator, inclusive). `cc`/`S` PASS = clear line + INSERT (Vim noai default). **Visual block:** Ctrl+V → `-- VISUAL BLOCK --`; block `x` deletes the rect and returns NORMAL; **`>`/`<` in block mode = NO-OP that also stays in the mode → #2606** (controls: `V`+`>` works post-#2438; block `x` works).

**Harness lessons:**
- **vi operator render lag can exceed 1s:** after `c`+`c` the first capture showed `OPERATOR (c)` + unchanged buffer; 1.2s later the line was cleared + INSERT. Re-poll at ~1.5s before declaring any vi operator dead (extends the Run #57 0.5–1s guidance).
- Vi-mode quit: the `(s)ave/(d)iscard/(q)uit` prompt can swallow the first `d` right after leaving a vi visual mode — confirm the prompt is gone, else resend `d`+Enter.
- Toggle Line Wrap from the palette can silently not apply on the first invocation (likely palette-open race) — verify a continuation row exists (blank-number gutter `    │`) before testing wrap-dependent behavior.
- Go-to-line then Toggle Fold sequence is reliable for fold targeting; `Ctrl+G` prompt renders below the status bar as before.
