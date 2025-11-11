# TODO

## Completed Features ✅

### Core Functionality
- Event-driven architecture with unlimited undo/redo
- Multi-cursor editing
- Clipboard operations (copy/cut/paste)
- Position history navigation (Alt+Left/Right)
- Line wrapping
- Large file support (1GB+) with instant startup
- Advanced prompt editing (word deletion, copy/paste/cut in all input prompts)
- **Auto-indent (Jan 2025)** - Tree-sitter based, hybrid heuristic approach, supports all languages

### UI & Layout
- Split views (horizontal/vertical)
- Scrollbar, tab bar, status bar, line numbers
- Command palette (Ctrl+P), help system (Ctrl+H)
- File explorer (lazy loading, gitignore support, create/delete, unsaved indicators)
- Themes (dark/light/high-contrast)

### LSP Integration
- Diagnostics (errors/warnings)
- Code completion (Ctrl+Space)
- Go-to-definition, rename refactoring (F2)
- Multi-language support, process resource limits

### Search & Replace
- ✅ **Streaming search** (Nov 2025) - Literal & regex, efficient on GB+ files with overlapping chunks
- ✅ **Replace operations** (Nov 2025) - replace_range(), replace_next(), replace_all(), replace_all_regex() with capture groups
- ✅ **Replace UI** (Ctrl+R) - Emacs-style two-step prompts with incremental highlighting
- ✅ **Interactive replace** (Ctrl+Alt+R) - Query-replace with y/n/!/q prompts, proper undo/redo
- ✅ **Search in selection** (Nov 2025) - Limit search to selected range
- ✅ **Search history** (Nov 2025) - Up/Down navigation, bash-like, 100 items per history
- Basic text search UI (F3/Shift+F3), wrap-around, highlighting, incremental search

### File Operations
- Open/save/close, multiple buffers, async I/O
- File explorer (create/delete files/dirs, show/hide hidden, respect gitignore, auto-expand on focus)

### Git Integration
- Git grep (Ctrl+Shift+G) - **Can be converted to plugin** (see Plugin Refactoring below)
- Git find file (Ctrl+Shift+P) - **Can be converted to plugin** (see Plugin Refactoring below)

### Plugin System
- ✅ **Lua 5.4 runtime** - Fully integrated plugin manager, lifecycle management
- ✅ **Dynamic hooks** - 16+ hook types (render-line, after-save, etc.)
- ✅ **Command registration** - Plugins can register custom commands
- ✅ **Async process spawning** - Non-blocking external commands
- ✅ **Buffer query API** - Metadata queries, streaming content access via render-line hook
- ✅ **Overlay lifecycle** - clear_all_overlays(), remove_overlays_by_prefix()
- ✅ **Example plugins** - TODO Highlighter (optimized for GB+ files), async demos

### Performance & Optimization
- ✅ **Marker system (IntervalTree)** - O(log n) marker operations, lazy delta propagation for position tracking
- ✅ **ChunkTree optimization** (Jan 2025) - 4KB chunks → 38x speedup (file loading: 3.2s → 83ms)
- ✅ **Scroll optimization** - O(n) → O(viewport_height)
- ✅ **Buffer cache removal** - Eliminated expensive `buffer.to_string()` calls
- 400+ unit tests, 59 E2E tests, property-based tests, visual regression testing

---

## Remaining Work

### Priority 1: Critical Editor Features

#### Search & Replace Enhancements
- [ ] Case-sensitive/insensitive toggle
- [ ] Whole word matching
- [ ] Persist search history across sessions
- [ ] Multi-file search/replace (integrate with git grep)
- [ ] Progress bar for replace_all on huge files

#### Smart Editing
- [ ] Smart home key (toggle between line start and first non-whitespace)
- [ ] Bracket matching & auto-close
- [ ] Auto-pair deletion (delete both opening and closing)
- [ ] Electric indent (auto-adjust indentation)
- [ ] Toggle comment (Ctrl+/, language-aware)
- [ ] Indent/dedent selection (Tab/Shift+Tab)

#### Navigation
- [ ] Go to line number (Ctrl+G)
- [ ] Go to matching bracket
- [ ] Jump to next/previous error (F8/Shift+F8)
- [ ] Bookmark system (set/clear/jump)

#### Selection & Editing
- [ ] Rectangular/block selection (Alt+drag or Ctrl+Alt+arrows)
- [ ] Expand selection to scope (by AST nodes)

#### Macros
- [ ] Record macro (q + key)
- [ ] Play macro (@ + key)
- [ ] Macro persistence

### Priority 2: LSP & Developer Tools

### LSP Support (Maturity Roadmap)

This plan aims to evolve the LSP client to be performant, full-featured, and robust, based on the principles for building a mature LSP client.

#### Priority 1: Performance & Stability Foundation

- [ ] **Implement Incremental Synchronization (Deltas):**
    - **Problem:** The editor currently sends the entire file content on every keystroke, causing significant UI lag in large files.
    - **Solution:** Modify the `didChange` notification to send only the changed text (deltas). This requires checking for the `TextDocumentSyncKind::Incremental` capability from the server and calculating the text diffs to send. This is the highest priority performance fix.
- [ ] **Implement Request Cancellation:**
    - **Problem:** Slow or outdated results (e.g., from code completion) can appear after the user has already moved on, creating UI "jank."
    - **Solution:** Implement support for sending `$/cancelRequest` notifications when a new request is issued before an old one completes (e.g., typing more characters while a completion menu is visible).
- [ ] **Robust Server Lifecycle Management:**
    - **Problem:** A crashed or hung LSP server can leave the editor in a broken state with no feedback.
    - **Solution:** Implement robust error handling to detect when the LSP process dies. Notify the user and offer to restart the server.
- [ ] **Harden JSON-RPC Message Parsing:**
    - **Problem:** A malformed or partial message from the LSP server could crash the editor's message handling loop.
    - **Solution:** Improve the robustness of the JSON-RPC transport layer to gracefully handle framing errors, corrupt headers, or invalid JSON, preventing panics.

#### Priority 2: Core UX Features

- [ ] **Dedicated Diagnostics Panel:**
    - **Problem:** Diagnostics are only visible as squiggles in the text. There is no way to see a full list of problems in the current file or project.
    - **Solution:** Create a new UI panel that lists all diagnostics from `textDocument/publishDiagnostics`, allowing users to quickly navigate to each error location.
- [ ] **Hover Documentation:** Show documentation for the symbol under the cursor in a popup window on `textDocument/hover`.
- [ ] **Code Actions:** Query for `textDocument/codeAction` and allow the user to apply quick fixes and refactorings (e.g., via a menu).
- [ ] **Find References:** Implement `textDocument/references` and display the results in a list or quickfix window.
- [ ] **Signature Help:** Show function/method parameter hints as the user is typing, triggered by `textDocument/signatureHelp`.

#### Priority 3: Advanced Features & Polish

- [ ] **Semantic Tokens:** Implement `textDocument/semanticTokens` for more advanced and accurate syntax highlighting.
- [ ] **Document & Workspace Symbols:** Implement `textDocument/documentSymbol` for an outline/breadcrumb view and `workspace/symbol` for project-wide symbol search.
- [ ] **Inlay Hints:** Display inlay hints (`textDocument/inlayHint`) for type annotations and parameter names.
- [ ] **Progress Reporting:** Handle `$/progress` notifications from the server to show activity indicators in the UI (e.g., for indexing).
- [ ] **Server Communication & Logging:**
    - [ ] Handle `window/logMessage` to display server logs for debugging.
    - [ ] Handle `window/showMessage` and `window/showMessageRequest` to show info/warnings and ask questions.
- [ ] **Document Formatting:** Add commands for `textDocument/formatting` and `textDocument/rangeFormatting`.
- [ ] **Call Hierarchy / Type Hierarchy:** Implement `callHierarchy/incomingCalls` and `typeHierarchy/supertypes`.
- [ ] **Code Lens / Folding Ranges:** Implement `textDocument/codeLens` and `textDocument/foldingRange`.

#### Priority 4: Project & Configuration

- [ ] **Multi-Root Workspace Support:** Support `workspace/workspaceFolders` to correctly handle projects with multiple sub-projects, potentially launching separate LSP instances per folder.
- [ ] **Configuration Synchronization:** Send `workspace/didChangeConfiguration` notifications when editor settings (like tab size or diagnostics settings) change.

#### File Explorer Polish
- [ ] Input dialog system for custom names
- [ ] Rename with custom name
- [ ] Copy/move operations (Ctrl+C/X/V in explorer)
- [ ] File watching for auto-refresh
- [ ] Search/filter within explorer
- [ ] Sort options (name, date, size, type)

### Priority 3: Visual Enhancements & UX

#### Visual Improvements
- [ ] Indent guides
- [ ] Current line highlighting
- [ ] Whitespace visualization
- [ ] Color column (vertical ruler)
- [ ] Git gutter (show added/modified/deleted lines)
- [ ] Minimap (optional)

#### Themes & Appearance
- [ ] More built-in themes (Solarized, Monokai, Dracula, Nord)
- [ ] Theme customization UI
- [ ] Font configuration (size, family)
- [ ] Ligature support

#### Command Palette Improvements
- [ ] Fuzzy matching (currently substring)
- [ ] Command history
- [ ] Recently used commands at top
- [ ] Show keyboard shortcuts in palette

#### Snippets & Templates
- [ ] Snippet system with Tab expansion
- [ ] Tabstops and placeholders
- [ ] Snippet variables ($1, $2, $TM_FILENAME, etc.)
- [ ] Language-specific snippets

#### User Experience
- [ ] Welcome screen & onboarding
- [ ] Configuration UI (settings editor)
- [ ] Better error messages
- [ ] Crash recovery (restore unsaved files)
- [ ] Session persistence (restore open files)

### Priority 4: Advanced Features

#### Git Integration
- [ ] Git status in file explorer
- [ ] Git blame
- [ ] Git diff view (side-by-side or unified)
- [ ] Stage/unstage hunks
- [ ] Commit UI / Branch switching
- [ ] Git log viewer
- [ ] Merge conflict resolution UI
- [ ] Magit-style interface (via plugin)

#### Terminal & Debugger
- [ ] Embedded terminal (Ctrl+`)
- [ ] Multiple terminals / split terminal
- [ ] Debug adapter protocol (DAP) support
- [ ] Breakpoints (toggle, conditional)
- [ ] Debug toolbar / Variables view / Call stack

#### Project Management
- [ ] Project/workspace concept
- [ ] Project-specific configuration
- [ ] Multiple workspace folders

### Priority 5: Plugin System Refactoring

#### Convert Git Operations to Plugins (High Value Refactoring)

**Analysis Summary (2025-01-11):**

The current git grep and git find files are hardcoded in `src/git.rs` (200 lines), `src/actions.rs`, and `src/editor.rs`. These can be refactored into Lua plugins with minimal new API additions.

**Current Implementation:**
- **Git grep**: Spawns `git grep -n --column -I`, parses output, sends to AsyncBridge
- **Git find files**: Spawns `git ls-files`, fuzzy filters, sends to AsyncBridge
- **Integration**: Hardcoded in Action enum, PromptType enum, and editor event loop
- **UI**: Uses prompt system with suggestions populated from AsyncMessage results

**Emacs Approach (Magit Reference):**
- Uses `make-process` for async git commands with filters for streaming output
- Process objects allow cancellation and incremental updates
- Buffers display results with custom keybindings and interactive commands
- Minimal coupling to core editor - all UI is plugin-managed

**✅ Available Plugin APIs (Already Implemented):**
- `editor.spawn(cmd, args, callback)` - Async process execution (lines 417-471 in plugin_manager.rs)
- `editor.on(hook, callback)` - Event hooks (16+ types)
- `editor.register_command({name, action, contexts})` - Dynamic commands
- `editor.add_overlay(...)` - Visual highlighting
- `editor.set_status(msg)` - Status messages
- `editor.get_buffer_info(id)` - Buffer metadata queries
- `editor.list_buffers()` - Enumerate open buffers
- Process callbacks with stdout/stderr/exit_code

**✅ Available Plugin APIs (Phase 1 Complete):**
- ✅ `editor.open_file({path, line, column})` - Open file at specific location (Jan 2025)

**❌ Missing APIs for Full Git Plugin Support:**

1. **Prompt/Selection UI API** (CRITICAL) - **Using Hook-Based Design**

   **Hook-based approach** (simpler than callbacks):
   ```lua
   -- Start a prompt
   editor.start_prompt({
       label = "Git grep: ",
       prompt_type = "git-grep"  -- For filtering hooks
   })

   -- React to user input via hooks (fires as they type)
   editor.on("prompt-changed", function(args)
       if args.prompt_type == "git-grep" then
           local query = args.input

           -- Spawn async git grep
           editor.spawn("git", {"grep", "-n", "--column", "-I", "--", query},
               function(stdout, stderr, exit_code)
                   local results = parse_git_grep(stdout)
                   editor.set_prompt_suggestions(results)
               end)
       end
   end)

   -- Handle selection via hooks
   editor.on("prompt-confirmed", function(args)
       if args.prompt_type == "git-grep" and args.selected then
           editor.open_file({
               path = args.selected.file,
               line = args.selected.line,
               column = args.selected.column
           })
       end
   end)
   ```

   **Advantages over callback-based design:**
   - Uses existing hook infrastructure (no new callback storage)
   - More Emacs-like (hooks are the Emacs standard)
   - Simpler implementation (~200-300 LOC vs 500+)
   - Natural cleanup when prompt closes
   - Multiple plugins can react to same prompt
   - No complex async callback lifetime management

3. **Virtual/Scratch Buffers** (MEDIUM - for Magit-style interfaces)
   ```lua
   -- Need: Create non-file buffer with custom keybindings
   local buf = editor.create_virtual_buffer("*git-status*", {
       read_only = true,
       context = "git-status-mode"
   })
   ```

4. **Custom Context/Keybindings** (MEDIUM)
   ```lua
   -- Need: Define buffer-local keybindings
   editor.register_keybinding({
       key = "Enter",
       context = "git-grep-results",
       action = function() jump_to_match() end
   })
   ```

**Recommended Implementation Plan:**

**Phase 1: Add File Opening API** ✅ **COMPLETE** (Jan 2025)
- ✅ Add `PluginCommand::OpenFileAtLocation`
- ✅ Expose via Lua as `editor.open_file({path, line, column})`
- ✅ Created git_grep_demo.lua prototype

**Phase 2: Add Hook-Based Prompt API (1 day)**
- Add 3 new hook types: `prompt-changed`, `prompt-confirmed`, `prompt-cancelled`
- Add `PluginCommand::StartPrompt` and `PluginCommand::SetPromptSuggestions`
- Wire hooks in editor when prompt state changes
- Expose `editor.start_prompt()` and `editor.set_prompt_suggestions()` in Lua

**Phase 3: Implement Git Grep Plugin (1 day)**
```lua
-- plugins/git-grep.lua
editor.register_command({
    name = "Git Grep",
    description = "Search in git-tracked files",
    action = "git-grep",
    contexts = {"normal"}
})

-- Start prompt when command is invoked
editor.on("command-executed", function(args)
    if args.command == "git-grep" then
        editor.start_prompt({
            label = "Git grep: ",
            prompt_type = "git-grep"
        })
    end
end)

-- Update results as user types
editor.on("prompt-changed", function(args)
    if args.prompt_type == "git-grep" and args.input ~= "" then
        editor.spawn("git", {"grep", "-n", "--column", "-I", "--", args.input},
            function(stdout, stderr, exit_code)
                if exit_code == 0 then
                    local results = parse_git_grep(stdout)
                    editor.set_prompt_suggestions(results)
                end
            end)
    end
end)

-- Jump to selected result
editor.on("prompt-confirmed", function(args)
    if args.prompt_type == "git-grep" and args.selected then
        editor.open_file({
            path = args.selected.file,
            line = args.selected.line,
            column = args.selected.column
        })
    end
end)
```

**Phase 4: Implement Git Find Files Plugin (1 day)**
- Similar structure to git grep
- Uses `git ls-files` with fuzzy filtering in Lua
- Demonstrates reusability of hook-based prompt API

**Phase 5: Remove Hardcoded Git Code (1 day)**
- Delete `src/git.rs` (200 lines)
- Remove `Action::GitGrep`, `Action::GitFindFile`
- Remove `PromptType::GitGrep`, `PromptType::GitFindFile`
- Remove async message handlers in editor.rs
- Bundle git plugins as default plugins (auto-load)

**Benefits:**
- ✅ Reduces core editor code by ~300+ lines
- ✅ Makes git features user-customizable (change git args, add new commands)
- ✅ Demonstrates plugin system capabilities
- ✅ Opens path for community git plugins (git blame, git log, magit-style interface)
- ✅ Validates plugin API design with real-world use case
- ✅ Core editor becomes more focused (fewer hardcoded features)

**Risks/Challenges:**
- Prompt API needs careful design (balances power vs. complexity)
- Maintaining same UX quality as hardcoded version
- Migration path for users (plugins must be auto-loaded initially)
- Performance: Lua overhead for parsing git output (likely negligible)

**Alternative: Enhanced Plugin APIs First**
If prompt API is too complex, prioritize virtual buffers + custom contexts:
- Git grep displays results in a buffer (like LSP diagnostics)
- Navigate results with j/k, press Enter to jump
- More Emacs-like, potentially more powerful than prompts

**Next Steps (Hook-Based Approach):**
1. ✅ Phase 1 complete - File opening API implemented
2. Implement hook-based prompt API (~200-300 LOC):
   - Add hook types: `prompt-changed`, `prompt-confirmed`, `prompt-cancelled`
   - Add plugin commands: `StartPrompt`, `SetPromptSuggestions`
   - Wire hooks in editor prompt handling code
3. Implement full git grep plugin using hooks
4. Implement git find files plugin
5. Remove hardcoded git code (~300 lines removed)

---

### Priority 6: Plugin System (Advanced APIs - Post Git Refactoring)

**Note:** Priority 5 (Git Operations as Plugins) should be completed first, as it will inform the design of these advanced APIs.

#### Interactive UI API
- [ ] Virtual buffers / selection lists / input dialogs (partially addressed by Prompt API in Priority 5)
- [ ] Read-only buffers
- [ ] Generic popups (custom content rendering)
- [ ] Tree/list widgets for structured data

#### Modal Interaction & Navigation
- [ ] Define custom modes/contexts (needed for git grep results, magit)
- [ ] Buffer-local keybindings (partially addressed in Priority 5)
- [ ] Goto line/position API, set selection, scroll control
- [ ] Cursor manipulation API

#### Enhanced Hooks & Integration
- [ ] More hooks: `on_buffer_open`, `on_selection_change`, `on_key_press`, `on_cursor_moved`
- [ ] State persistence API (plugin configuration, session state)
- [ ] LSP access API (query diagnostics, trigger completion from plugins)
- [ ] Search API (invoke search from plugins, get search state)
- [ ] Undo history API (query undo tree, create undo boundaries)
- [ ] Process cancellation support (cancel long-running spawned processes)

#### Target Showcase Plugins
- [ ] Git grep & find files (Priority 5 - in progress)
- [ ] Magit-style Git interface (needs virtual buffers + custom contexts)
- [ ] Telescope-style fuzzy finder (reuses prompt API from Priority 5)
- [ ] Undo tree visualizer (needs virtual buffers + undo history API)
- [ ] Project search & replace (needs search API + prompt API)
- [ ] Git blame (line annotations + async git commands)

### Priority 7: Future Enhancements

#### Performance & Optimization
- [ ] Syntax highlighting cache
- [ ] Lazy plugin loading
- [ ] Memory usage profiling

#### Dialogs & Progress
- [ ] Confirmation dialogs
- [ ] Progress indicators
- [ ] Toast notifications

#### Accessibility
- [ ] Screen reader support
- [ ] Configurable UI scale
- [ ] Color-blind friendly themes

#### Advanced/Future Features
- [ ] Remote editing (SSH/SFTP)
- [ ] Collaborative editing (CRDT-based)
- [ ] Plugin marketplace
- [ ] Diff editor (compare files)
- [ ] Markdown preview
- [ ] Vi/Vim emulation mode

---

## Technical Debt & Refactoring

### Line Wrapping Refactoring
- [ ] Unify wrapping and no-wrapping code paths (treat no-wrap as infinite-width)
- [ ] Move cursor position calculation into rendering traversal (eliminate duplicate iteration)
- [ ] Fix style preservation during wrapping (currently loses syntax highlighting)

**Benefits**: Single source of truth, better performance, massive code deduplication

### Code Organization
- [x] Extract UI rendering (~430 lines → 6 modules)
- [x] Extract commands & prompts (~335 lines → 2 modules)
- [ ] Create BufferView abstraction (~500 lines)
- [ ] Extract multi-cursor operations (~200 lines)
- [ ] Split large modules (editor.rs is ~3000 lines)

### Test Infrastructure
- [ ] Fix async file loading in test harness (6 tests ignored)
- [ ] Fix BIG.txt generation timing (2 scrolling tests fail)
- [ ] Support independent buffers per split (if desired)
- [ ] Add more E2E tests for complex workflows
- [ ] Performance regression tests

---

## Summary

### Current Status (January 2025)
**Strengths**: Multi-cursor editing, search & replace, auto-indent, LSP basics, large file support (1GB+), fully integrated Lua plugin system, IntervalTree marker system, strong test coverage (400+ tests)

**Recent Major Completions**:
- ✅ Search & Replace (Nov 2025) - Complete with interactive replace, history, search in selection
- ✅ Auto-indent (Jan 2025) - Tree-sitter based with hybrid heuristics
- ✅ Plugin System (Nov 2025) - Fully integrated with Lua runtime, hooks, and overlay management
- ✅ Marker System (Nov 2025) - O(log n) IntervalTree implementation with lazy delta propagation
- ✅ Git Plugin Refactoring Analysis (Jan 2025) - Identified path to convert git operations to plugins

**Critical Gaps**:
- Advanced LSP features (hover, code actions, find references)
- Bracket matching, snippets
- Terminal integration
- Plugin prompt/selection UI API (needed for git grep/find files as plugins)

**Next Steps**:
1. **High Priority**: Implement plugin prompt API + convert git operations to plugins (demonstrates plugin system, reduces core code by ~300 lines)
2. **Medium Priority**: LSP advanced features (hover, code actions, find references)
3. **Medium Priority**: Smart editing (bracket matching, toggle comment)

### Milestones

**M1: Essential Editing** ✅ **COMPLETE**
- [x] Core editing, multi-cursor, undo/redo
- [x] Search & replace
- [x] Auto-indent, go to line

**M2: Developer Experience** (Target: Daily Driver)
- [x] LSP basics (diagnostics, completion, go-to-def, rename)
- [ ] LSP advanced (hover, code actions, find references) ← **CURRENT FOCUS**
- [ ] Snippets, toggle comment, bracket matching

**M3: Advanced Features** (Target: Best-in-Class)
- [x] Large file support, plugin system (Lua)
- [x] Performance optimization (marker system, ChunkTree)
- [ ] Advanced plugin APIs (custom modes, virtual buffers)
- [ ] Magit/Telescope-style plugins
- [ ] Terminal & debugger integration

**M4: Polish & Ecosystem** (Target: Production-Ready)
- [ ] Welcome screen, configuration UI
- [ ] Crash recovery, session persistence
- [ ] Plugin marketplace, comprehensive docs
