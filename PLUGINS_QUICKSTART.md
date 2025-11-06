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

### 3. Open the Command Palette

Press **`Ctrl+P`** to open the command palette.

You'll see two new commands registered by the plugin:
- **`Plugin Demo: Say Hello`**
- **`Plugin Demo: Show Status`**

These commands appear alongside the built-in commands (Save, Open, Quit, etc.)

### 4. Check the Logs

Plugin initialization is logged. View the logs:

```bash
tail -f /tmp/editor.log
```

You should see:
```
🎉 Welcome plugin initialized successfully!
📝 Registered 2 commands - try Ctrl+P to see them
```

---

## What's Happening

1. **Editor starts** → Looks for `plugins/` directory
2. **Finds `welcome.lua`** → Loads it into Lua runtime
3. **Plugin executes:**
   - Calls `editor.set_status()` → Status message appears
   - Calls `editor.register_command()` twice → Commands added to palette
   - Calls `print()` → Messages go to logs

4. **You press Ctrl+P** → Command registry (now includes plugin commands) is queried
5. **Plugin commands appear** in the list!

---

## Create Your Own Plugin

### Step 1: Create a `.lua` file in `plugins/`

```bash
cat > plugins/my_plugin.lua <<'EOF'
-- My First Plugin

editor.set_status("My plugin loaded!")

editor.register_command({
    name = "My Command",
    description = "My custom command",
    action = "none",
    contexts = {"normal"}
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
```lua
editor.register_command({
    name = "Command Name",
    description = "What it does",
    action = "none",  -- or "save", "quit", etc.
    contexts = {"normal"}  -- or {"help", "prompt", "popup", "file_explorer"}
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
- ✅ Command registration
- ✅ Status messages
- ✅ Text insertion
- ✅ Visual overlays
- ✅ Basic hooks (structure exists)

### Coming Soon 🚧
- 🚧 Buffer query API (get content, cursor position, etc.)
- 🚧 More hook invocations (on open, on insert, on delete)
- 🚧 Async task spawning (for git, external commands)
- 🚧 Popup API (custom dialogs, menus)
- 🚧 Custom keybindings
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
