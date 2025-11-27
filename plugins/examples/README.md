# Example Plugins

This directory contains example plugins demonstrating the editor's plugin system. These are educational examples showing specific API features.

For the complete API reference, see **[docs/plugin-api.md](../../docs/plugin-api.md)**.

## Available Examples

### hello_world.ts

A simple "Hello World" plugin that demonstrates:
- Registering a custom command
- Setting status messages
- Basic plugin structure

### async_demo.ts

Demonstrates async process spawning:
- Running external commands with `spawnProcess`
- Processing stdout/stderr
- Handling exit codes

### buffer_query_demo.ts

Demonstrates buffer queries:
- Getting buffer metadata with `getBufferInfo`
- Listing all open buffers
- Querying cursor and viewport information

### virtual_buffer_demo.ts

Demonstrates virtual buffer creation:
- Creating virtual buffers with `createVirtualBufferInSplit`
- Using text properties for embedded metadata
- Defining custom modes with keybindings
- Handling "go to" navigation from results

### bookmarks.ts

A complete bookmark management example:
- Managing persistent state across sessions
- Creating navigation commands
- Using overlays for visual markers

### git_grep.ts

Git grep implementation demonstrating:
- Spawning async git processes
- Parsing structured output
- Opening files at specific line:column positions
- Interactive search with prompt API

## Writing Your Own Plugin

1. Create a `.ts` file in the plugins directory
2. Use the `editor` global object to access the API
3. Register commands with `editor.registerCommand()`
4. The plugin will be automatically loaded when the editor starts

Example template:

```typescript
/// <reference path="../types/fresh.d.ts" />

// Define the command handler
globalThis.my_command = function(): void {
  editor.setStatus("My command executed!");
};

// Register the command
editor.registerCommand(
  "My Custom Command",
  "Does something cool",
  "my_command",
  "normal"
);

// Initialization message
editor.debug("My custom plugin loaded");
```

## Further Reading

- **Getting Started:** [docs/PLUGIN_DEVELOPMENT.md](../../docs/PLUGIN_DEVELOPMENT.md)
- **API Reference:** [docs/plugin-api.md](../../docs/plugin-api.md)
