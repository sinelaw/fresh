# Plugin System

This document provides a comprehensive overview of the Fresh editor's plugin system, including a quick-start guide, architectural details, and a full API reference.

## Quick Start Guide

**🎉 Plugins are now working!** You can see them in action right now.

### How to See Plugins in Action

#### 1. Build and Run the Editor

```bash
cargo build --release
./target/release/editor
```

#### 2. Look for Plugin Feedback

When the editor starts, you should immediately see:

**✅ Status Bar Message:**
```
✨ Plugins are working! Welcome Plugin loaded successfully!
```

This proves the `welcome.lua` plugin loaded and executed!

**Note:** The status message appears briefly and may be replaced by other editor messages. Check the logs (see step 4) to confirm the plugin loaded.

#### 3. Open the Command Palette

Press **`Ctrl+P`** to open the command palette.

You'll see **five new commands** registered by the plugin:
- **`Plugin Demo: Open Help`** - Uses built-in action (toggles help screen)
- **`Plugin Demo: Save File`** - Uses built-in action (saves current file)
- **`Plugin Demo: Say Hello`** - Lua callback (shows greeting message)
- **`Plugin Demo: Show Time`** - Lua callback (displays current time)
- **`Plugin Demo: Insert Text`** - Lua callback (inserts sample text)

These commands appear alongside the built-in commands (Save, Open, Quit, etc.)

**Try selecting one!** The Lua callback commands will actually execute Lua code and show you real-time feedback.

#### 4. Check the Logs

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

### Create Your Own Plugin

#### Step 1: Create a `.lua` file in `plugins/`

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

#### Step 2: Restart the editor

```bash
./target/release/editor
```

#### Step 3: Press `Ctrl+P`

Your command "My Command" will appear in the palette!

## Architecture

The plugin system is designed to be powerful and flexible, drawing inspiration from Emacs and Neovim. It is built on a few core principles:

1.  **Event-Driven:** The editor's core is event-driven. All state changes are represented as events, which can be replayed, undone, and broadcast to plugins.
2.  **Message-Passing:** Plugins do not directly access the editor's state. Instead, they send commands to the editor via a message-passing API. This ensures that the editor remains in control of its state and prevents plugins from causing corruption.
3.  **Lua First:** The primary scripting language is Lua (specifically Lua 5.4), chosen for its speed, small footprint, and easy integration with Rust.
4.  **Hooks:** Plugins can subscribe to a wide range of events using a hook system. This allows plugins to react to user actions, file changes, and other editor events.
5.  **Async Operations:** Plugins can spawn asynchronous tasks for long-running operations like running external commands, ensuring the editor remains responsive.

### Core Components

-   **`PluginManager`:** The central component responsible for loading, managing, and communicating with plugins. It hosts the Lua runtime and exposes the `editor` API to plugins.
-   **`HookRegistry`:** A thread-safe registry for managing hooks. Plugins can register callbacks for specific events, and the editor will invoke them at the appropriate times.
-   **`CommandRegistry`:** A dynamic registry for commands. Plugins can register new commands, which are then available in the command palette.
-   **`PluginApi`:** A safe, message-passing interface that plugins use to interact with the editor.

## API Reference

The following API is available to Lua plugins via the global `editor` object.

### Commands

#### `editor.register_command(command)`

Registers a new command in the editor.

-   `command`: A table with the following fields:
    -   `name` (string): The name of the command, as it will appear in the command palette.
    -   `description` (string): A brief description of what the command does.
    -   `action` (string): A unique identifier for the action. If a `callback` is provided, this is used to identify the callback. If not, it should be the name of a built-in editor action (e.g., "save", "quit").
    -   `contexts` (table): A list of contexts in which the command is available (e.g., `{"normal"}`).
    -   `callback` (function, optional): A Lua function to be executed when the command is run.

**Example (built-in action):**
```lua
editor.register_command({
    name = "Save File",
    description = "Saves the current file",
    action = "save",
    contexts = {"normal"}
})
```

**Example (Lua callback):**
```lua
editor.register_command({
    name = "Say Hello",
    description = "Shows a greeting message",
    action = "say_hello",
    contexts = {"normal"},
    callback = function()
        editor.set_status("Hello from a plugin!")
    end
})
```

### Status Bar

#### `editor.set_status(message)`

Displays a message in the status bar.

-   `message` (string): The message to display.

**Example:**
```lua
editor.set_status("File saved successfully!")
```

### Buffer Operations

#### `editor.insert_text(buffer_id, position, text)`

Inserts text into a buffer.

-   `buffer_id` (number): The ID of the buffer to modify.
-   `position` (number): The byte offset at which to insert the text.
-   `text` (string): The text to insert.

**Example:**
```lua
editor.insert_text(0, 0, "Hello, World!")
```

### Overlays

Overlays are used to add visual decorations to text, such as highlights or underlines.

#### `editor.add_overlay(buffer_id, overlay_id, start_pos, end_pos, r, g, b, underline)`

Adds an overlay to a buffer.

-   `buffer_id` (number): The ID of the buffer.
-   `overlay_id` (string): A unique ID for the overlay.
-   `start_pos` (number): The starting byte offset of the overlay.
-   `end_pos` (number): The ending byte offset of the overlay.
-   `r`, `g`, `b` (number): The RGB color of the overlay.
-   `underline` (boolean): Whether to underline the text.

**Example:**
```lua
editor.add_overlay(0, "my-overlay", 0, 10, 255, 0, 0, true)
```

#### `editor.remove_overlay(buffer_id, overlay_id)`

Removes an overlay from a buffer.

-   `buffer_id` (number): The ID of the buffer.
-   `overlay_id` (string): The ID of the overlay to remove.

#### `editor.remove_overlays_by_prefix(buffer_id, prefix)`

Removes all overlays with a given prefix.

-   `buffer_id` (number): The ID of the buffer.
-   `prefix` (string): The prefix to match.

### Hooks

Plugins can subscribe to editor events using hooks.

#### `editor.on(hook_name, callback)`

Registers a callback for a specific hook.

-   `hook_name` (string): The name of the hook to subscribe to (e.g., "after-file-save").
-   `callback` (function): A function to be called when the hook is triggered. The function will receive a table of arguments specific to the hook. It should return `true` to allow the operation to continue, or `false` to cancel it.

**Example:**
```lua
editor.on("after-file-save", function(args)
    editor.set_status("File saved!")
    return true
end)
```

### Buffer & Editor State Queries

#### `editor.get_active_buffer_id()`

Returns the ID of the currently active buffer.

-   **Returns:** `number`

#### `editor.get_buffer_info(buffer_id)`

Returns information about a buffer.

-   `buffer_id` (number): The ID of the buffer.
-   **Returns:** A table with the following fields, or `nil` if the buffer doesn't exist:
    -   `id` (number)
    -   `path` (string)
    -   `modified` (boolean)
    -   `length` (number): The size of the buffer in bytes.

#### `editor.list_buffers()`

Returns a list of all open buffers.

-   **Returns:** An array of buffer info tables.

#### `editor.get_primary_cursor()`

Returns information about the primary cursor.

-   **Returns:** A table with the following fields, or `nil`:
    -   `position` (number): The byte offset of the cursor.
    -   `selection` (table, optional): A table with `start` and `end` byte offsets if there is a selection.

#### `editor.get_all_cursors()`

Returns a list of all active cursors.

-   **Returns:** An array of cursor info tables.

#### `editor.get_viewport()`

Returns information about the current viewport.

-   **Returns:** A table with the following fields, or `nil`:
    -   `top_byte` (number)
    -   `left_column` (number)
    -   `width` (number)
    -   `height` (number)

### Asynchronous Operations

#### `editor.spawn(command, args, options, callback)`

Spawns an external command asynchronously.

-   `command` (string): The command to run.
-   `args` (table): A list of string arguments for the command.
-   `options` (table, optional): A table of options. Currently supports:
    -   `cwd` (string): The working directory for the command.
-   `callback` (function): A function to be called when the process completes. It receives `stdout`, `stderr`, and `exit_code` as arguments.

**Example:**
```lua
editor.spawn("git", {"status", "--short"}, function(stdout, stderr, exit_code)
    if exit_code == 0 then
        editor.set_status("Git status: " .. stdout)
    else
        editor.set_status("Git failed: " .. stderr)
    end
end)
```

## Advanced Topics

### Performance Best Practices for Overlay Plugins

When writing plugins that use overlays (like syntax highlighters or diagnostic markers), it's important to avoid recreating overlays on every frame. This can cause flickering and high CPU usage.

The recommended approach is to use content-based change detection. By hashing the content of a line and only updating the overlays when the hash changes, you can avoid unnecessary work.

See the `todo_highlighter.lua` plugin for a complete example of this pattern.

### Marker-Based Overlays

The editor uses a marker-based overlay system. This means that when you create an overlay, the editor creates markers at the start and end positions. These markers automatically adjust their positions when text is inserted or deleted. This is a powerful feature that allows overlays to stay anchored to the correct content without any manual intervention from the plugin.

This is why you should minimize invalidation and trust markers to handle position tracking.
