# Plugin System - Quick Start Guide

**🎉 Plugins are now working!** You can see them in action right now.

---

## How to See Plugins in Action

### 1. Build and Run the Editor

```bash
cargo build --release
./target/release/editor
```

### 2. Look for Plugin Feedback

When the editor starts, you should immediately see:

**✅ Status Bar Message:**
```
✨ Plugins are working! Welcome Plugin loaded successfully!
```

This proves the `welcome.lua` plugin loaded and executed!

**Note:** The status message appears briefly and may be replaced by other editor messages. Check the logs (see step 4) to confirm the plugin loaded.

### 3. Open the Command Palette

Press **`Ctrl+P`** to open the command palette.

You'll see **five new commands** registered by the plugin:
- **`Plugin Demo: Open Help`** - Uses built-in action (toggles help screen)
- **`Plugin Demo: Save File`** - Uses built-in action (saves current file)
- **`Plugin Demo: Say Hello`** - Lua callback (shows greeting message)
- **`Plugin Demo: Show Time`** - Lua callback (displays current time)
- **`Plugin Demo: Insert Text`** - Lua callback (inserts sample text)

These commands appear alongside the built-in commands (Save, Open, Quit, etc.)

**Try selecting one!** The Lua callback commands will actually execute Lua code and show you real-time feedback.

### 4. Check the Logs

Plugin initialization is logged. View the logs:

```bash
tail -f /tmp/editor.log
```

You should see:
```
🎉 Welcome plugin initialized successfully!
📝 Registered 5 commands - try Ctrl+P to see them!
   - 'Plugin Demo: Open Help' - toggles help screen (built-in action)
   - 'Plugin Demo: Save File' - saves current file (built-in action)
   - 'Plugin Demo: Say Hello' - shows greeting (Lua callback)
   - 'Plugin Demo: Show Time' - displays current time (Lua callback)
   - 'Plugin Demo: Insert Text' - inserts sample text (Lua callback)
```

When you execute a plugin command, you'll also see log entries like:
```
Plugin callback executed: say_hello
Plugin callback executed: show_time at 14:23:45
```

---

## What's Happening

1. **Editor starts** → Looks for `plugins/` directory
2. **Finds `welcome.lua`** → Loads it into Lua runtime
3. **Plugin executes:**
   - Calls `editor.set_status()` → Status message appears
   - Calls `editor.register_command()` five times → Commands added to palette
   - Some commands use built-in actions (`show_help`, `save`)
   - Some commands register Lua callbacks (stored in global `_plugin_callbacks` table)
   - Calls `print()` → Messages go to logs

4. **You press Ctrl+P** → Command registry (now includes plugin commands) is queried
5. **Plugin commands appear** in the list!
6. **You select a command:**
   - If it has a built-in action → Editor executes that action
   - If it has a Lua callback → `plugin_manager.execute_action()` runs the Lua function
   - The callback can call editor API functions (set_status, insert_text, etc.)

---

## Create Your Own Plugin

### Step 1: Create a `.lua` file in `plugins/`

```bash
cat > plugins/my_plugin.lua <<'EOF'
-- My First Plugin

editor.set_status("My plugin loaded!")

-- Register a command with a Lua callback
editor.register_command({
    name = "My Custom Action",
    description = "Execute custom Lua code",
    action = "my_custom_action",
    contexts = {"normal"},
    callback = function()
        editor.set_status("🎨 My custom action executed!")
        print("Custom action was triggered")
    end
})

print("My plugin initialized")
EOF
```

### Step 2: Restart the editor

```bash
./target/release/editor
```

### Step 3: Press `Ctrl+P`

Your command "My Command" will appear in the palette!

---

## Available Plugin API

### Status Messages
```lua
editor.set_status("Hello from plugin!")
```

### Register Commands

**With built-in action:**
```lua
editor.register_command({
    name = "Command Name",
    description = "What it does",
    action = "save",  -- Use built-in action: "save", "quit", "show_help", etc.
    contexts = {"normal"}
})
```

**With Lua callback:**
```lua
editor.register_command({
    name = "My Custom Command",
    description = "Runs custom Lua code",
    action = "my_action_name",  -- Unique action identifier
    contexts = {"normal"},
    callback = function()
        editor.set_status("Callback executed!")
        -- Your custom code here
    end
})
```

### Insert Text
```lua
-- Insert at buffer 0, position 0
editor.insert_text(0, 0, "Hello, World!")
```

### Add Visual Overlays
```lua
-- Add red wavy underline from position 0 to 10
editor.add_overlay(0, "my-overlay-id", 0, 10, 255, 0, 0, true)
```

### Remove Overlays
```lua
editor.remove_overlay(0, "my-overlay-id")
```

### Hooks (Basic Support)
```lua
editor.on("after-file-save", function(args)
    editor.set_status("File saved!")
    return true  -- return false to cancel
end)
```

### Query Buffer State (Phase 2 - NEW!)

**Get Active Buffer ID:**
```lua
local buffer_id = editor.get_active_buffer_id()
-- Returns: number (buffer ID)
```

**Get Buffer Content:**
```lua
local content = editor.get_buffer_content(buffer_id)
-- Returns: string or nil
```

**Get Specific Line (1-indexed):**
```lua
local line = editor.get_line(buffer_id, 5)  -- Get line 5
-- Returns: string or nil
```

**List All Open Buffers:**
```lua
local buffers = editor.list_buffers()
-- Returns: array of {id, path, modified, length}
for _, buf in ipairs(buffers) do
    print(string.format("Buffer %d: %s (%d bytes)", buf.id, buf.path, buf.length))
end
```

**Get Buffer Info:**
```lua
local info = editor.get_buffer_info(buffer_id)
-- Returns: {id, path, modified, length} or nil
if info then
    print("Path: " .. info.path)
    print("Modified: " .. tostring(info.modified))
    print("Size: " .. info.length .. " bytes")
end
```

**Get Primary Cursor:**
```lua
local cursor = editor.get_primary_cursor()
-- Returns: {position, selection} or nil
if cursor then
    print("Cursor at: " .. cursor.position)
    if cursor.selection then
        print("Selection: " .. cursor.selection.start .. "-" .. cursor.selection["end"])
    end
end
```

**Get All Cursors (Multi-cursor Support):**
```lua
local cursors = editor.get_all_cursors()
-- Returns: array of {position, selection}
print("Active cursors: " .. #cursors)
for i, cursor in ipairs(cursors) do
    print(string.format("Cursor %d at position %d", i, cursor.position))
end
```

**Get Viewport Info:**
```lua
local vp = editor.get_viewport()
-- Returns: {top_byte, left_column, width, height} or nil
if vp then
    print(string.format("Viewport: %dx%d", vp.width, vp.height))
    print("Scrolled to byte: " .. vp.top_byte)
end
```

### Async Process Spawning (Phase 2 - NEW!)

**Spawn External Process:**
```lua
-- Basic usage: spawn(command, args, callback)
editor.spawn("git", {"status", "--short"}, function(stdout, stderr, exit_code)
    if exit_code == 0 then
        editor.set_status("Git status: " .. stdout)
    else
        editor.set_status("Git failed: " .. stderr)
    end
end)
```

**With Working Directory:**
```lua
-- spawn(command, args, options, callback)
editor.spawn("pwd", {}, {cwd = "/tmp"}, function(stdout, stderr, exit_code)
    editor.set_status("Working dir: " .. stdout)
end)
```

**Example: Git Branch Checker**
```lua
editor.register_command({
    name = "Show Git Branch",
    description = "Display current git branch",
    action = "show_git_branch",
    contexts = {"normal"},
    callback = function()
        editor.spawn("git", {"branch", "--show-current"}, function(stdout, stderr, exit_code)
            if exit_code == 0 then
                local branch = stdout:gsub("\n", "")
                editor.set_status("On branch: " .. branch)
            else
                editor.set_status("Not a git repository")
            end
        end)
    end
})
```

**Key Features:**
- ✅ Fully asynchronous - editor remains responsive
- ✅ Captures stdout and stderr separately
- ✅ Returns exit code for error handling
- ✅ Working directory control via `{cwd = "/path"}`
- ✅ Multiple processes can run concurrently

---

## Example: Auto-Save Message Plugin

```lua
-- auto_save_message.lua
-- Shows a friendly message when files are saved

editor.on("after-file-save", function(args)
    editor.set_status("💾 File saved successfully!")
    print("File saved at: " .. os.date("%H:%M:%S"))
    return true
end)

editor.register_command({
    name = "Toggle Auto-Save Messages",
    description = "Enable/disable save notifications",
    action = "none",
    contexts = {"normal"}
})

print("Auto-save message plugin loaded")
```

---

## Testing Your Plugin

### Option 1: Through the Editor
1. Place `.lua` file in `plugins/`
2. Start editor: `./target/release/editor`
3. Check status bar for messages
4. Press `Ctrl+P` to see commands
5. Check logs: `tail -f /tmp/editor.log`

### Option 2: Through Unit Tests
```rust
use editor::plugin_manager::PluginManager;
use editor::hooks::HookRegistry;
use editor::command_registry::CommandRegistry;
use std::sync::{Arc, RwLock};

let hooks = Arc::new(RwLock::new(HookRegistry::new()));
let commands = Arc::new(RwLock::new(CommandRegistry::new()));

let mut manager = PluginManager::new(hooks, commands)?;
manager.load_plugin(Path::new("plugins/my_plugin.lua"))?;

// Check what commands were sent
let plugin_commands = manager.process_commands();
assert_eq!(plugin_commands.len(), 1); // Should have SetStatus command
```

---

## Plugin Directory Structure

```
editor/
├── plugins/
│   ├── welcome.lua          # Demo plugin (already loaded!)
│   ├── my_plugin.lua        # Your plugin here
│   └── another_plugin.lua
└── plugins/examples/
    ├── hello.lua            # Example plugins (not auto-loaded)
    ├── highlight_demo.lua
    └── README.md            # Detailed API docs
```

**Note:** Only `.lua` files in `plugins/` (not subdirectories) are auto-loaded.

---

## Troubleshooting

### Plugin not loading?
- Check `/tmp/editor.log` for error messages
- Make sure file is in `plugins/` directory (not `plugins/examples/`)
- File must end in `.lua`
- Lua syntax must be valid

### Commands not appearing?
- Make sure you called `editor.register_command()`
- Check that `contexts` includes "normal"
- Restart the editor after changing plugin files

### Status message not showing?
- Status messages are temporary (they may be replaced by other messages)
- Check logs with `tail -f /tmp/editor.log` to confirm plugin loaded

---

## Complete Plugin Examples

### TODO Highlighter (`plugins/todo_highlighter.lua`)

A fully functional plugin that demonstrates Phase 2 capabilities. It highlights TODO/FIXME/HACK/NOTE/XXX/BUG keywords in comments with different colors.

**Features:**
- Pattern matching across multiple comment styles (C, Python, Lua, HTML)
- Color-coded overlays for different keyword types
- Five interactive commands:
  - `TODO Highlighter: Enable` - Start highlighting
  - `TODO Highlighter: Disable` - Stop and clear highlights
  - `TODO Highlighter: Toggle` - Quick on/off
  - `TODO Highlighter: Refresh` - Re-scan current buffer
  - `TODO Highlighter: Show Keywords` - Display tracked keywords

**APIs Demonstrated:**
- `editor.get_active_buffer_id()` - Get current buffer
- `editor.get_buffer_content()` - Read buffer text
- `editor.add_overlay()` / `editor.remove_overlay()` - Visual highlights
- `editor.register_command()` - Add commands to palette
- `editor.set_status()` - User feedback

**Try it:**
1. Open a file with TODO comments (e.g., `test_todo_comments.txt`)
2. Press `Ctrl+P` and run `TODO Highlighter: Toggle`
3. See keywords highlighted with different colors!

---

## What's Next?

### Already Working ✅
- ✅ Command registration (both built-in actions and Lua callbacks)
- ✅ Lua callback execution
- ✅ Status messages
- ✅ Text insertion
- ✅ Visual overlays
- ✅ Event-driven hooks (automatic hook invocation from events)
- ✅ **Buffer query API (Phase 2)** - get content, cursor position, buffer info
- ✅ **Async process spawning (Phase 2)** - run external commands

### Coming Soon 🚧
- 🚧 More hook types (on_buffer_changed, on_file_open, etc.)
- 🚧 Virtual buffers & custom UI
- 🚧 Popup API (custom dialogs, menus)
- 🚧 Custom keybindings from plugins
- 🚧 Process cancellation / kill support
- 🚧 WASM plugin support

---

## Resources

- **API Documentation:** `plugins/examples/README.md`
- **Example Plugins:** `plugins/examples/*.lua`
- **Implementation Details:** `docs/PLUGIN_SYSTEM_IMPLEMENTATION.md`
- **Architecture:** `docs/PLUGIN_SYSTEM_ANALYSIS.md`

---

## Performance Best Practices for Overlay Plugins

When writing plugins that use overlays (like syntax highlighters, diagnostic markers, or TODO highlighters), follow these patterns to avoid performance issues:

### ❌ Anti-Pattern: Recreating Overlays Every Frame

```lua
-- DON'T DO THIS
editor.on("render-line", function(args)
    -- This runs for every visible line on EVERY frame (60fps!)
    editor.remove_overlay(args.buffer_id, "my-overlay-" .. args.line_number)
    editor.add_overlay(args.buffer_id, "my-overlay-" .. args.line_number,
                      args.byte_start, args.byte_start + 10, 255, 0, 0, false)
    return true
end)
```

**Problem:** This creates/destroys overlays constantly, causing:
- Flickering (overlays disappear and reappear each frame)
- Marker ID explosion (thousands of markers created per second)
- High CPU usage even when idle
- Poor performance

### ✅ Pattern: Content-Based Change Detection

```lua
-- Track line content to detect actual changes
local line_hashes = {}

local function hash_string(str)
    local hash = 5381
    for i = 1, #str do
        hash = ((hash * 33) + string.byte(str, i)) % 2147483647
    end
    return hash
end

editor.on("render-line", function(args)
    if not line_hashes[args.buffer_id] then
        line_hashes[args.buffer_id] = {}
    end

    local content_hash = hash_string(args.content)
    local previous_hash = line_hashes[args.buffer_id][args.line_number]

    -- Only recreate overlays if content actually changed
    if content_hash ~= previous_hash then
        -- Clear old overlays for this line
        local prefix = string.format("my_overlay_L%d_", args.line_number)
        editor.remove_overlays_by_prefix(args.buffer_id, prefix)

        -- Add new overlays based on content
        scan_and_add_overlays(args)

        -- Update hash
        line_hashes[args.buffer_id][args.line_number] = content_hash
    end

    return true
end)
```

**Benefits:**
- No work when scrolling (content unchanged)
- No work when idle (no frames triggered)
- Overlays only recreated when line content actually changes
- Markers automatically adjust positions when text is inserted/deleted elsewhere

### ✅ Pattern: Smart Invalidation on Edits

```lua
editor.on("after-insert", function(args)
    local needs_rescan = false

    -- Only invalidate if the edit might affect your overlays
    -- For example, check if inserted text contains keywords you care about
    if args.text:find("TODO", 1, true) then
        needs_rescan = true
    end

    if needs_rescan then
        -- Clear overlays and hashes - next render will rescan
        editor.remove_overlays_by_prefix(args.buffer_id, "my_overlay_")
        line_hashes[args.buffer_id] = nil
    end
    -- Otherwise: markers auto-adjust, no action needed!

    return true
end)
```

**Key insight:** Don't invalidate on every edit!
- **Simple typing** (no keywords): Markers auto-adjust → zero plugin work
- **Newlines alone**: Markers auto-adjust → let content hash detect changes
- **Keywords appear**: Invalidate and rescan

### ❌ Anti-Pattern: Invalidating on Newlines

```lua
-- DON'T DO THIS
editor.on("after-insert", function(args)
    if args.text:find("\n") then
        -- This triggers full invalidation on every Enter press!
        clear_all_overlays(args.buffer_id)
    end
    return true
end)
```

**Problem:** Pressing Enter causes full buffer rescan unnecessarily. Markers already handle position adjustments!

### ✅ Pattern: Per-Line Overlay IDs with Prefix

```lua
-- Include line number in overlay ID for efficient clearing
local overlay_id = string.format("my_plugin_L%d_keyword_O%d",
                                line_number, occurrence)

-- Clear just this line's overlays
local prefix = string.format("my_plugin_L%d_", line_number)
editor.remove_overlays_by_prefix(buffer_id, prefix)
```

**Benefits:**
- Clear overlays per-line instead of entire buffer
- Much cheaper than removing overlays one by one
- Enables selective invalidation strategies

### Understanding Marker-Based Overlays

The editor uses **markers** (self-adjusting position trackers) for overlays:

```
Buffer: "Hello World"
         ^     ^
         m1    m2
        (0)   (6)

Insert "Beautiful " at position 6:
Buffer: "Hello Beautiful World"
         ^                ^
         m1               m2
        (0)              (16)  ← Automatically adjusted!
```

When you create an overlay at positions (10, 20), the editor:
1. Creates markers at those positions
2. When text is inserted at position 5, markers move to (15, 25)
3. Your overlay stays anchored to the correct content - no plugin code needed!

**This is why you should minimize invalidation and trust markers to handle position tracking.**

### Performance Checklist

For overlay-based plugins, ensure:
- ✅ Content hash tracking prevents unnecessary recreation
- ✅ Invalidation only happens when content semantically changes
- ✅ Frame-duplicate detection (don't process same line twice in one frame)
- ✅ Per-line overlay IDs with prefixes for efficient clearing
- ✅ No invalidation on plain newlines (markers handle it)
- ✅ Test by monitoring `/tmp/editor.log` - should be quiet when idle

---

## Questions?

Check the logs for debugging:
```bash
tail -f /tmp/editor.log
```

All plugin errors are logged but don't crash the editor.

**Happy plugin hacking! 🚀**
