# Fresh Plugin Development

This guide covers creating plugins for Fresh.

## Package Types

Fresh supports three types of packages:

| Type | Description | Guide |
|------|-------------|-------|
| **Plugin** | TypeScript code extending editor functionality | This page |
| **Theme** | Color schemes for the editor | See `:pkg init theme` |
| **Language Pack** | Syntax highlighting, language config, and LSP | [Language Packs](./language-packs.md) |

Use `fresh --init` to scaffold any package type.

## Introduction

Fresh plugins are written in **TypeScript** and run in a sandboxed QuickJS environment (transpiled via oxc_transformer). Plugins have access to a TypeScript API for extending the editor.

API reference: **[Plugin API](../api/)**

## Getting Started: "Hello, World!"

Let's start by creating a simple "Hello, World!" plugin.

1.  **Create a new file:** Create a new TypeScript file in the `plugins/` directory (e.g., `my_plugin.ts`).
2.  **Add the following code:**

    ```typescript
    /// <reference path="../types/fresh.d.ts" />

    // Register a command that inserts text at the cursor
    globalThis.my_plugin_say_hello = function(): void {
      editor.insertAtCursor("Hello from my new plugin!\n");
      editor.setStatus("My plugin says hello!");
    };

    editor.registerCommand(
      "my_plugin_say_hello",
      "Inserts a greeting from my plugin",
      "my_plugin_say_hello"
    );

    editor.setStatus("My first plugin loaded!");
    ```

3.  **Run Fresh:**
    ```bash
    cargo run
    ```
4.  **Open the command palette:** Press `Ctrl+P` and search for "my_plugin_say_hello".
5.  **Run the command:** You should see the text "Hello from my new plugin!" inserted into the buffer.

## Core Concepts

### Plugin Lifecycle

Plugins are loaded automatically when Fresh starts. There is no explicit activation step. All `.ts` files in the `plugins/` directory are transpiled via oxc_transformer and executed in the QuickJS runtime.

### The `editor` Object

The global `editor` object is the main entry point for the Fresh plugin API. It provides methods for:
- Registering commands
- Reading and modifying buffers
- Adding visual overlays
- Spawning external processes
- Subscribing to editor events

### Commands

Commands are actions that can be triggered from the command palette or bound to keys. Register them with `editor.registerCommand()`:

```typescript
globalThis.my_action = function(): void {
  // Do something
};

editor.registerCommand(
  "my_command_name",      // Display name in command palette
  "Human readable desc",   // Description for command palette
  "my_action"             // Global function to call
);
```

#### Conditional visibility with `context`

`registerCommand` accepts an optional 4th parameter to control when the command is visible in the palette. When omitted, the command is always visible — this is what you want for most commands.

If you provide a context string, the command is **hidden** unless that context is currently active. Contexts are activated by your plugin via `editor.setContext(name, true)` or by matching the focused buffer's virtual mode (from `defineMode()`). This is useful for commands that only make sense in a specific plugin state:

```typescript
// Always visible — no context needed
editor.registerCommand("My Plugin: Start", "Start a review session", "start_review");

// Only visible while a review session is active
editor.registerCommand("My Plugin: Next Item", "Go to next review item", "next_item", "review-active");

// Activate/deactivate the context from your plugin logic
editor.setContext("review-active", true);   // "Next Item" now appears in palette
editor.setContext("review-active", false);  // "Next Item" is hidden again
```

::: warning
The context parameter is for **plugin-defined** contexts only. Values like `"normal"` or `"insert"` do not correspond to built-in editor modes and will make your command permanently invisible.
:::

### Asynchronous Operations

Many API calls return `Promise`s. Use `async/await` to work with them:

```typescript
globalThis.search_files = async function(): Promise<void> {
  const result = await editor.spawnProcess("rg", ["TODO", "."]);
  if (result.exit_code === 0) {
    editor.setStatus(`Found matches`);
  }
};
```

### Event Handlers

Subscribe to editor events with `editor.on()`. Handlers must be global functions:

```typescript
globalThis.onSave = function(data: { buffer_id: number, path: string }): void {
  editor.debug(`Saved: ${data.path}`);
};

editor.on("buffer_save", "onSave");
```

**Available Events:**
- `buffer_save` - After a buffer is saved
- `buffer_closed` - When a buffer is closed
- `cursor_moved` - When cursor position changes
- `render_start` - Before screen renders
- `lines_changed` - When visible lines change (batched)
