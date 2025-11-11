# Example Plugins

This directory contains example plugins demonstrating the editor's plugin system.

## Available Examples

### hello.lua
A simple "Hello World" plugin that demonstrates:
- Registering a custom command
- Setting status messages
- Basic plugin structure

### highlight_demo.lua
Demonstrates visual overlays:
- Multiple command registration
- Adding colored overlays to buffers
- Using the overlay API

### git_grep_demo.lua
Demonstrates git integration and file navigation:
- Spawning async git processes
- Parsing git grep output
- Opening files at specific line:column positions
- Prototype for implementing git grep as a plugin

## Plugin API

### Available Functions

#### editor.register_command(command_table)
Register a new command in the command palette.

```lua
editor.register_command({
    name = "My Command",
    description = "What this command does",
    action = "none",
    contexts = {"normal"}  -- or {"help", "prompt", "popup", "file_explorer"}
})
```

#### editor.set_status(message)
Set the status bar message.

```lua
editor.set_status("Plugin loaded successfully")
```

#### editor.insert_text(buffer_id, position, text)
Insert text at a specific position in a buffer.

```lua
editor.insert_text(0, 0, "Hello, World!")
```

#### editor.add_overlay(buffer_id, overlay_id, start, end, r, g, b, underline)
Add a visual overlay (highlight/underline) to a buffer.

```lua
-- Add red underline to positions 0-10 in buffer 0
editor.add_overlay(0, "my-overlay", 0, 10, 255, 0, 0, true)
```

#### editor.on(hook_name, callback)
Register a hook callback (currently simplified).

```lua
editor.on("after-file-save", function(args)
    print("File saved!")
    return true  -- return false to cancel operation
end)
```

#### editor.spawn(command, args, callback) or editor.spawn(command, args, options, callback)
Spawn an async process and get its output.

```lua
-- Simple form
editor.spawn("git", {"status", "--porcelain"}, function(stdout, stderr, exit_code)
    editor.set_status("Git status: " .. stdout)
end)

-- With options (e.g., working directory)
editor.spawn("ls", {"-la"}, {cwd = "/tmp"}, function(stdout, stderr, exit_code)
    print("Files: " .. stdout)
end)
```

#### editor.open_file(path) or editor.open_file({path, line, column})
Open a file, optionally jumping to a specific line and column.

```lua
-- Open file at start
editor.open_file("src/main.rs")

-- Open file and jump to line 42, column 10 (1-indexed)
editor.open_file({
    path = "src/main.rs",
    line = 42,
    column = 10
})
```

This is particularly useful for implementing features like git grep, LSP go-to-definition, etc.

## Available Hooks

- `before-file-open` - Before a file is opened
- `after-file-open` - After a file is successfully opened
- `before-file-save` - Before a file is saved
- `after-file-save` - After a file is saved
- `after-insert` - After text is inserted
- `after-delete` - After text is deleted
- `pre-command` - Before a command executes
- `post-command` - After a command executes

## Writing Your Own Plugin

1. Create a `.lua` file in the plugins directory
2. Use the API functions above to add functionality
3. The plugin will be automatically loaded when the editor starts

Example template:

```lua
-- My Custom Plugin

-- Register commands
editor.register_command({
    name = "My Custom Command",
    description = "Does something cool",
    action = "none",
    contexts = {"normal"}
})

-- Add hooks if needed
editor.on("after-file-save", function(args)
    editor.set_status("File saved - plugin notified!")
    return true
end)

-- Initialization message
print("My custom plugin loaded")
```

## Testing Plugins

Currently, plugins are unit tested through the plugin_manager tests. Integration tests will be added in a future update.

## Future Enhancements

Planned features:
- Buffer query API (get content, cursor position, etc.)
- Popup API (custom dialogs, menus)
- Async task spawning (for git operations, external commands)
- More comprehensive hook system
- WASM plugin support for multi-language plugins
