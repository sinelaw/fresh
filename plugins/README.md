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
