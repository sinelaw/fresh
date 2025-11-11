# Fresh Editor Roadmap

## Executive Summary

Fresh has achieved its core architectural goals: a **fast, stable, and truly extensible editor** powered by Rust and Lua. The plugin system is production-ready, marker-based overlays are implemented, and the editor handles GB+ files with ease. The path forward focuses on closing LSP feature gaps and building showcase plugins to demonstrate the platform's unique capabilities.

## Current Status

### ✅ Major Architectural Achievements

1. **Plugin System - FULLY OPERATIONAL** ✅
   - ✅ Lua 5.4 runtime integrated into main editor loop
   - ✅ 20+ hook types (render-line, after-insert, prompt-changed, etc.)
   - ✅ Marker-based overlays with automatic position tracking
   - ✅ Comprehensive APIs: buffer query, async process spawning, file operations, overlay management
   - ✅ Production plugins: TODO highlighter, git-grep, git-find-file
   - ✅ Hook-based prompt API for interactive UIs

2. **Performance & Scalability** ✅
   - ✅ Marker system (IntervalTree) with O(log n) operations and lazy delta propagation
   - ✅ ChunkTree optimization (38x speedup on file loading)
   - ✅ Handles 1GB+ files with instant startup
   - ✅ Incremental LSP synchronization
   - ✅ Viewport-based rendering optimizations

3. **Core Editor Features** ✅
   - ✅ Multi-cursor editing, unlimited undo/redo
   - ✅ Search & replace (interactive, history, regex)
   - ✅ Auto-indent (tree-sitter based, 14+ languages)
   - ✅ Split views, file explorer, command palette
   - ✅ LSP basics: diagnostics, completion, go-to-definition, rename

### Current Feature Gaps

Based on comprehensive codebase analysis:

1. **LSP Integration - Partial** (4/15 features implemented)
   - ✅ Implemented: Diagnostics, Completion, Go-to-definition, Rename
   - ❌ Missing High-Value: Hover, Code Actions, Find References, Signature Help
   - ❌ Missing Advanced: Semantic tokens, Document symbols, Inlay hints, Formatting

2. **Smart Editing - Partial** (1/5 features implemented)
   - ✅ Auto-indent (tree-sitter based)
   - ❌ Bracket matching & auto-pairing
   - ❌ Comment toggling (Ctrl+/)
   - ❌ Smart home key
   - ❌ Electric indent

3. **Search & Replace - Mostly Complete** (5/7 features)
   - ✅ Interactive replace, History, Regex, Search in selection
   - ❌ Case-sensitive/whole-word toggles
   - ❌ Multi-file search/replace

4. **Plugin Ecosystem**
   - ✅ Runtime & APIs fully functional
   - ❌ No plugin discovery/installation UI
   - ❌ No plugin marketplace
   - ✅ Example plugins demonstrate capabilities

______________________________________________________________________

## Strategic Vision: "Extensible Performance"

**Competitive Position**: Fresh combines the performance of Rust-based editors (Zed, Helix) with the extensibility of VSCode and Neovim, while avoiding their pitfalls:
- ✅ Unlike Zed: Powerful plugin system (Lua, not WASM)
- ✅ Unlike Helix: Stable with large files, extensible architecture
- ✅ Unlike Neovim: Low configuration tax, integrated experience
- ✅ Unlike VSCode: Native performance, no Electron overhead

**Unique Advantage**: Marker-based overlays solve a fundamental problem that other editors struggle with - plugins can add UI decorations that automatically track text edits without manual repositioning. This unlocks powerful plugin capabilities not easily achievable elsewhere.

______________________________________________________________________

## Roadmap Phases

## Phase 1: Daily Driver Readiness

**Goal**: Close the feature gaps that prevent developers from using Fresh full-time.

### Priority 1: High-Value LSP Features

Implement the "big three" LSP features that have outsized impact on developer experience:

1. **textDocument/hover** - Show documentation on mouse hover or Ctrl+K
   - Display type information, doc comments
   - Render markdown formatting
   - Show in non-intrusive popup

2. **textDocument/codeAction** - Quick fixes and refactorings
   - Trigger on Ctrl+. or lightbulb icon
   - Show available actions in menu
   - Apply edits to buffer

3. **textDocument/references** - Find all usages
   - Show results in quickfix-style panel
   - Jump to reference locations
   - Group by file

**Impact**: These three features alone cover 80% of daily LSP usage and make Fresh competitive with mainstream IDEs.

### Priority 2: Essential Smart Editing

Focus on the highest-impact editing features:

1. **Bracket Matching & Auto-pairing**
   - Highlight matching brackets on cursor position
   - Auto-close brackets, quotes, parens
   - Smart deletion (remove both opening and closing)

2. **Comment Toggling** (Ctrl+/)
   - Language-aware comment syntax
   - Toggle line comments, block comments
   - Works with selections and multi-cursor

**Impact**: These features have an outsized effect on the perceived "polish" of the editing experience.

### Priority 3: Search Enhancements

Complete the search & replace feature set:

1. **Case-sensitive/Whole-word toggles**
   - Add UI controls to search prompt
   - Persist settings across searches

2. **Multi-file search** (leverage git-grep plugin)
   - Integrate git-grep results with replace workflow
   - Project-wide find/replace

______________________________________________________________________

## Phase 2: Showcase the Plugin System

**Goal**: Demonstrate Fresh's unique architectural advantages through killer-app plugins.

### Priority 1: Magit-Style Git Interface

Build a comprehensive Git UI that's only possible due to Fresh's plugin architecture:

**Requirements**:
- Custom buffer with keybindings (virtual buffer API)
- Async git operations (✅ already implemented)
- Interactive staging/unstaging hunks
- Commit UI with message editor
- Branch switching, log viewer
- Diff visualization

**Why it matters**:
- Demonstrates the full power of the plugin system
- Addresses a major pain point in terminal editors
- Proves Fresh can match/exceed Emacs Magit (the gold standard)

### Priority 2: Advanced Plugin APIs

Implement the remaining APIs needed for showcase plugins:

1. **Virtual Buffers & Custom Modes**
   - Read-only buffers with custom rendering
   - Buffer-local keybindings
   - Custom contexts for modal interaction

2. **Enhanced UI APIs**
   - Tree/list widgets for structured data
   - Split/panel management from plugins
   - Status bar segments

3. **Integration APIs**
   - LSP query API (access diagnostics from plugins)
   - Search API (invoke search, get state)
   - Undo history API (query undo tree)

**Target Showcases** (in order):
1. ✅ Git grep & find file (COMPLETE)
2. Magit-style Git interface
3. Telescope-style fuzzy finder
4. Undo tree visualizer
5. Project-wide search & replace

______________________________________________________________________

## Phase 3: Ecosystem & Polish

**Goal**: Build a thriving plugin ecosystem and production-ready user experience.

### Priority 1: Plugin Infrastructure

1. **Plugin Discovery & Installation**
   - Built-in plugin browser UI (Ctrl+Shift+P → "Install Plugin")
   - GitHub-based plugin registry
   - One-click installation
   - Dependency management

2. **Plugin Management**
   - Update notifications
   - Enable/disable plugins
   - Configuration UI
   - Plugin documentation viewer

3. **Developer Experience**
   - Plugin starter templates
   - Comprehensive API documentation
   - Testing infrastructure for Lua plugins
   - Debug console for plugin development

**Impact**: Dramatically lowers barrier to entry, mirrors VSCode's key strength while maintaining Neovim's flexibility.

### Priority 2: User Experience Polish

1. **Welcome & Onboarding**
   - First-run welcome screen
   - Interactive tutorial
   - Recommended plugin suggestions

2. **Configuration UI**
   - Settings editor (GUI for config.toml)
   - Keybinding customization
   - Theme browser with live preview

3. **Stability & Recovery**
   - Crash recovery (restore unsaved files)
   - Session persistence (restore open files)
   - Better error messages

### Priority 3: Advanced LSP Features

Complete the LSP implementation:

1. **Signature Help** - Parameter hints while typing
2. **Semantic Tokens** - Advanced syntax highlighting
3. **Document/Workspace Symbols** - Outline view, symbol search
4. **Inlay Hints** - Type annotations, parameter names
5. **Formatting** - Code formatting on save
6. **Call/Type Hierarchy** - Navigate type relationships

______________________________________________________________________

## Phase 4: Advanced Features

**Goal**: Become the best-in-class editor for specific workflows.

### Terminal Integration
- Embedded terminal (Ctrl+`)
- Multiple terminals, split terminals
- Task runner integration

### Debug Adapter Protocol (DAP)
- Breakpoints (toggle, conditional)
- Debug toolbar, variables view, call stack
- Step through, watch expressions

### Advanced Git Integration
- Git blame in gutter
- Stage/unstage hunks inline
- Merge conflict resolution UI
- Git log with graph visualization

### Visual Enhancements
- Minimap, indent guides
- Git gutter (show added/modified/deleted lines)
- Current line highlighting
- Whitespace visualization

______________________________________________________________________

## Technical Priorities

### High Priority: Line Cache Refactoring

**Problem**: Line number ↔ byte offset conversions are a major bottleneck:
- `populate_line_cache()` takes **61.95%** of diagnostic processing time
- Separate systems for markers (IntervalTree, lazy) and lines (BTreeMap, eager)

**Solution**: Unify line tracking into IntervalTree marker system
- Lines ARE intervals (between newlines)
- O(log N) edits instead of O(K log K)
- Single source of truth for all position tracking
- Sparse caching (only accessed lines)

**Expected Gain**: 62% reduction in LSP diagnostic processing time

### Medium Priority: Line Wrapping Refactoring

Unify wrapping/no-wrapping code paths, fix style preservation during wrapping, eliminate duplicate cursor calculations.

### Ongoing: Test Infrastructure

- Lua plugin testing framework
- Performance regression tests
- Visual regression testing expansion
- E2E tests for complex workflows

______________________________________________________________________

## Success Metrics

### Phase 1 Complete When:
- [ ] LSP hover, code actions, find references implemented
- [ ] Bracket matching and comment toggling work
- [ ] Case-sensitive/whole-word search toggles added
- [ ] Developer survey: "Would use Fresh as daily driver" >50%

### Phase 2 Complete When:
- [ ] Magit-style Git plugin fully functional
- [ ] Virtual buffer API documented and stable
- [ ] 3+ showcase plugins demonstrate unique capabilities
- [ ] Plugin development guide published

### Phase 3 Complete When:
- [ ] Plugin marketplace live with 20+ plugins
- [ ] One-click plugin installation works
- [ ] New user onboarding tutorial complete
- [ ] Crash recovery and session persistence implemented

### Long-term Vision:
- [ ] 100+ community plugins
- [ ] 1000+ daily active users
- [ ] Recognized as "Neovim performance with VSCode usability"
- [ ] Featured in developer tool roundups and "best of" lists

______________________________________________________________________

## Anti-Goals

What Fresh explicitly **won't** focus on:

❌ **Emulate Vim/Emacs keybindings** - Standard modern keybindings by default (plugins can add modal editing)

❌ **Be all things to all people** - Focus on text editing excellence, not replacing IDEs entirely

❌ **Compete on language-specific features** - Let LSP and plugins handle language support

❌ **Support every platform** - Linux/macOS/Windows desktop first, no mobile/web

______________________________________________________________________

## How to Contribute

See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

**High-impact areas**:
1. LSP feature implementation (hover, code actions, references)
2. Plugin development (showcase the system!)
3. Smart editing features (bracket matching, comment toggle)
4. Documentation (API docs, plugin guides)
5. Testing (E2E tests, plugin test infrastructure)

**Vision alignment**: Focus on making Fresh the **fastest extensible editor** - contributions should prioritize either **performance** or **extensibility**, ideally both.
