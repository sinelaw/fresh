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

## What's Next?

### Already Working ✅
- ✅ Command registration (both built-in actions and Lua callbacks)
- ✅ Lua callback execution
- ✅ Status messages
- ✅ Text insertion
- ✅ Visual overlays
- ✅ Event-driven hooks (automatic hook invocation from events)

### Coming Soon 🚧
- 🚧 Buffer query API (get content, cursor position, etc.)
- 🚧 More hook types (file-open, mode-change, etc.)
- 🚧 Hook exposure to Lua (editor.on() currently exists but needs more hook types)
- 🚧 Async task spawning (for git, external commands)
- 🚧 Popup API (custom dialogs, menus)
- 🚧 Custom keybindings from plugins
- 🚧 WASM plugin support

---

## Resources

- **API Documentation:** `plugins/examples/README.md`
- **Example Plugins:** `plugins/examples/*.lua`
- **Implementation Details:** `docs/PLUGIN_SYSTEM_IMPLEMENTATION.md`
- **Architecture:** `docs/PLUGIN_SYSTEM_ANALYSIS.md`

---

## Questions?

Check the logs for debugging:
```bash
tail -f /tmp/editor.log
```

All plugin errors are logged but don't crash the editor.

**Happy plugin hacking! 🚀**
