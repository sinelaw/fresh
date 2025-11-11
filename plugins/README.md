# Plugins

This directory contains production-ready plugins for the editor. Plugins are automatically loaded when the editor starts.

## Available Plugins

### TODO Highlighter (`todo_highlighter.lua`)

**A complete, useful plugin demonstrating Phase 2 API capabilities.**

Highlights TODO/FIXME/HACK/NOTE/XXX/BUG keywords in comments with color-coded overlays.

**Features:**
- Multi-language comment support (C/C++, Python, Lua, JavaScript, HTML, etc.)
- Color-coded highlighting:
  - 🟠 **TODO** - Orange (tasks to do)
  - 🔴 **FIXME** - Red (things to fix)
  - 🟡 **HACK** - Yellow (temporary workarounds)
  - 🟢 **NOTE** - Green (important notes)
  - 🟣 **XXX** - Magenta (items needing review)
  - 🔺 **BUG** - Dark Red (known bugs)
- Smart comment detection (only highlights keywords in comments, not in regular text)

**Commands:**
- `TODO Highlighter: Toggle` - Enable/disable highlighting
- `TODO Highlighter: Enable` - Turn on highlighting
- `TODO Highlighter: Disable` - Turn off and clear highlights
- `TODO Highlighter: Refresh` - Re-scan current buffer
- `TODO Highlighter: Show Keywords` - Display tracked keywords

**Usage:**
1. Open any file with TODO comments
2. Press `Ctrl+P` to open command palette
3. Type "TODO" and select `TODO Highlighter: Toggle`
4. Keywords in comments will be highlighted!

**APIs Used:**
- Buffer Query API: `get_active_buffer_id()`, `get_buffer_content()`
- Overlay API: `add_overlay()`, `remove_overlay()`
- Command Registration: `register_command()`

---

### Git Grep (`git-grep.lua`)

**Full-featured git grep with hook-based prompt API (Phase 2 - Jan 2025)**

Interactive search through all git-tracked files with real-time results.

**Features:**
- Search as you type with async git grep
- Shows file:line:column context for each match
- Opens files at exact match location
- Graceful handling of empty results and errors
- ~150 lines of Lua demonstrating prompt API

**Usage:**
```lua
start_git_grep()  -- From Lua or keybinding
```
Or use command palette: "Git Grep"

**APIs Used:**
- Hook-based Prompt API: `start_prompt()`, `set_prompt_suggestions()`
- Prompt Hooks: `prompt-changed`, `prompt-confirmed`, `prompt-cancelled`
- Async Process: `editor.spawn()`
- File Navigation: `editor.open_file({path, line, column})`

---

### Git Find File (`git-find-file.lua`)

**Fast fuzzy file finder for git repos (Phase 2 - Jan 2025)**

Find and open git-tracked files with fuzzy matching, similar to Ctrl+P in VSCode.

**Features:**
- Fuzzy file name filtering (all chars match in order)
- Caches git ls-files for instant filtering
- Shows up to 100 matches in real-time
- Opens selected file or manual path
- ~150 lines of pure Lua implementation

**Usage:**
```lua
start_git_find_file()  -- From Lua or keybinding
```
Or use command palette: "Git Find File"

**APIs Used:**
- Same hook-based prompt API as git grep
- Demonstrates reusability of prompt system
- Pure Lua fuzzy matching algorithm

---

### Welcome (`welcome.lua`)

Simple welcome message plugin that demonstrates basic plugin loading and status messages.

**Commands:**
- Various demo commands showing basic plugin capabilities

---

## Example Plugins

See `examples/` directory for educational examples demonstrating specific API features:
- `hello.lua` - Minimal plugin example
- `highlight_demo.lua` - Overlay API demonstrations
- `buffer_query_demo.lua` - Buffer state querying (Phase 2)
- `async_demo.lua` - Async process spawning (Phase 2)

---

## Plugin Development

For plugin development guides, see:
- **Quick Start:** `../PLUGINS_QUICKSTART.md`
- **API Reference:** `examples/README.md`
- **Implementation:** `../docs/PLUGIN_SYSTEM_IMPLEMENTATION.md`
