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

### Search
- ✅ **Streaming search implementation** (Nov 2025)
  - Literal string search with `find_next()` using overlapping chunks
  - Regex search with `find_next_regex()` using overlapping chunks
  - VSCode-style buffered iteration to avoid materializing entire file
  - Comprehensive property-based tests (14 tests, 100 cases each)
  - Works efficiently on GB+ files with O(chunk_size) memory usage
- Basic text search UI (forward/backward with F3/Shift+F3)
- Search wrap-around at document boundaries
- Search highlighting (viewport-optimized for huge files)
- Incremental search (as-you-type highlighting in prompt)

### File Operations
- Open/save/close, multiple buffers, async I/O
- File explorer (create/delete files/dirs, show/hide hidden, respect gitignore, auto-expand on focus)

### Git Integration
- Git grep (Ctrl+Shift+G)
- Git find file (Ctrl+Shift+P)

### Plugin System
- Lua 5.4 runtime, plugin manager
- Command registration, event hooks
- Async process spawning, buffer query API, overlay system
- Example: TODO Highlighter plugin

### Testing & Performance
- 400+ unit tests, 59 E2E tests
- Property-based tests, visual regression testing framework
- **Performance (Jan 2025)**: Massive improvements for huge files (61MB, 789K lines)
  - ChunkTree optimization: 4KB chunks → 38x speedup (file loading: 3.2s → 83ms)
  - Scroll limit: O(n) → O(viewport_height)
  - Buffer cache removal: Eliminated `buffer.to_string()` calls (3.9s for 61MB!)
  - render-line hook: Scales to 1GB+ files
  - Test performance: ~1,580x speedup in some tests

---

## Remaining Work

### Priority 1: Critical Editor Features

#### Search & Replace
**Status**: Core replace functionality complete! (Nov 2025)

**Completed:**
- ✅ Streaming search (literal & regex) - efficient on GB+ files
- ✅ Search UI (F3/Shift+F3)
- ✅ Wrap-around, highlighting, incremental search
- ✅ **Replace functions** (Nov 2025):
  - `replace_range()` - Replace specific byte range
  - `replace_next()` - Find and replace next occurrence
  - `replace_all()` - Replace all occurrences (literal strings)
  - `replace_all_regex()` - Replace all with regex capture groups (${1}, ${2})
- ✅ **Replace UI** (Ctrl+R) - Emacs-like two-step prompts:
  - With active search: directly prompts for replacement
  - Without active search: prompts for search query, then replacement
  - Incremental highlighting during search input
- ✅ **Property-based tests** - 11 tests, 1,100 cases covering all replace functions
- ✅ **Search in selection** (Nov 2025) - Limit search to selected range, no wrap-around
- ✅ **Interactive replace** (Ctrl+Alt+R, Nov 2025):
  - Emacs-style query-replace with y/n/!/q prompts
  - y: Replace current and move to next
  - n: Skip current and move to next
  - !: Replace all remaining
  - q/Esc: Quit
  - Event-based architecture for proper undo/redo
  - Position adjustment after each replacement
  - Progress indicator: "Replace this occurrence? (y/n/!/q) [N/M]"

**TODO:**
- [ ] Case-sensitive/insensitive toggle
- [ ] Whole word matching
- [ ] **Search history** - Store recent search/replace terms and allow navigation:
  - Pre-fill search prompts with most recent term
  - Up/Down arrow keys to cycle through history
  - Edit historical terms before using
  - Persist history across sessions
  - Separate histories for search vs. replace
- [ ] Multi-file search/replace (integrate with git grep)
- [ ] Progress bar for replace_all on huge files

#### Auto-Indent & Smart Editing
- [ ] Auto-indent on newline (language-aware)
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

#### LSP Features (Complete Integration)
- [ ] Hover documentation
- [ ] Code actions (quick fixes, refactorings)
- [ ] Find references
- [ ] Document symbols (outline/breadcrumb)
- [ ] Workspace symbols (find symbol across project)
- [ ] Signature help (parameter hints)
- [ ] Inlay hints (type annotations, parameter names)
- [ ] Call hierarchy / Type hierarchy
- [ ] Document formatting / Range formatting
- [ ] Semantic tokens (advanced syntax highlighting)
- [ ] Code lens / Folding ranges

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

### Priority 5: Plugin System (Phase 3 APIs)

#### Interactive UI API
- [ ] Virtual buffers / selection lists / input dialogs
- [ ] Read-only buffers
- [ ] Generic popups

#### Modal Interaction & Navigation
- [ ] Define custom modes
- [ ] Dynamic keybindings
- [ ] Goto line/position, set selection, scroll control

#### Enhanced Hooks & Integration
- [ ] More hooks: `on_buffer_open`, `on_selection_change`, `on_key_press`
- [ ] State persistence API
- [ ] LSP access / Search API / Undo history API
- [ ] Process cancellation support

#### Overlay Lifecycle Management ⚠️
**Priority: High** - Blocks TODO highlighter plugin from working correctly with text edits

**Problem**: Stale overlays aren't automatically removed when text changes. Old overlays persist with stale IDs while new ones are created.

**Solution**:
- [ ] Implement `editor.remove_overlays_by_prefix(buffer_id, prefix)` for bulk removal
- [ ] Or `editor.clear_all_overlays(buffer_id)`

#### Target Showcase Plugins
- [ ] Magit-style Git interface
- [ ] Telescope-style fuzzy finder
- [ ] Undo tree visualizer
- [ ] Project search & replace

### Priority 6: Future Enhancements

#### Performance & Optimization
- [ ] Incremental LSP sync
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

### Current Status
**Strengths**: Multi-cursor editing, LSP basics, large file support (1GB+), plugin system, strong test coverage

**Critical Gaps**: Replace functionality, auto-indent, bracket matching, snippets

**Next Steps**: Implement replace (single + all), then focus on auto-indent and smart editing

### Milestones

**M1: Essential Editing** (Target: MVP+)
- [x] Core editing, multi-cursor, undo/redo
- [ ] **Search & replace** ← IN PROGRESS
- [ ] Auto-indent, bracket matching, go to line

**M2: Developer Experience** (Target: Daily Driver)
- [x] LSP basics (diagnostics, completion, go-to-def, rename)
- [ ] LSP advanced (hover, code actions, find references)
- [ ] Snippets, toggle comment

**M3: Advanced Features** (Target: Best-in-Class)
- [x] Large file support, plugin system (Lua)
- [ ] Plugin Phase 3 APIs
- [ ] Magit/Telescope-style plugins
- [ ] Terminal & debugger integration

**M4: Polish & Ecosystem** (Target: Production-Ready)
- [ ] Welcome screen, configuration UI
- [ ] Crash recovery, session persistence
- [ ] Plugin marketplace, comprehensive docs

---

## Architecture Notes

### Lazy-Edit Approach Analysis (Nov 2025)
**Status:** Analyzed and **NOT RECOMMENDED**

**Proposal**: Store edits in a Write-Ahead Log (WAL) and apply lazily when sections are viewed

**Problems Identified**:
- Position tracking cascade failure (cursors, markers, overlays all need complex region tracking)
- Multi-cursor consistency nightmare
- Line cache invalidation chaos
- Syntax highlighting corruption
- Memory overhead explosion
- Cascading materialization (save, search, etc. force full materialization anyway)
- WAL complexity and crash recovery issues

**Conclusion**: Current architecture (ChunkTree-based Rope with persistent structure) is well-designed. The "problem" (occasional `to_string()` calls) is fixable with streaming search (already implemented ✅). Lazy-edit would introduce massive complexity for no real benefit.

**Key Insight**: Current architecture already achieves:
- O(log n) edits via persistent tree
- O(viewport) rendering via chunked iteration
- O(1) undo/redo via structural sharing


**Architecture Summary**:
- **Layer 1 (ChunkTree)**: Persistent Rope structure with Arc-sharing (efficient edits, instant undo)
- **Layer 2 (VirtualBuffer)**: 16MB LRU cache + edit log for iterator position adjustment
- **Layer 3 (Buffer)**: Text operations, line cache, file I/O
- **Layer 4 (EditorState)**: EventLog for undo/redo + UI state
- **Layer 5 (Rendering)**: Iterator-based O(viewport) display

**Why it works**: Streaming search (✅ implemented Nov 2025) eliminates the `to_string()` bottleneck. No architectural changes needed.

**Files**: `src/chunk_tree.rs`, `src/virtual_buffer.rs`, `src/buffer.rs`, `src/chunked_search.rs`
