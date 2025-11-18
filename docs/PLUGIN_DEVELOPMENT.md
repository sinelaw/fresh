# Fresh Plugin Development

Welcome to the Fresh plugin development guide! This document will walk you through the process of creating your own plugins for Fresh.

## Introduction

Fresh plugins are written in **TypeScript** and run in a sandboxed Deno environment. This provides a safe and modern development experience with access to a powerful set of APIs for extending the editor.

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
      "My Plugin: Say Hello",
      "Inserts a greeting from my plugin",
      "my_plugin_say_hello",
      "normal"
    );

    editor.setStatus("My first plugin loaded!");
    ```

3.  **Run Fresh:**
    ```bash
    cargo run
    ```
4.  **Open the command palette:** Press `Ctrl+P` and search for "My Plugin: Say Hello".
5.  **Run the command:** You should see the text "Hello from my new plugin!" inserted into the buffer.

## Core Concepts

### Plugin Lifecycle

Plugins are loaded automatically when Fresh starts. There is no explicit activation step. All files in the `plugins/` directory are executed in the Deno environment.

### The `editor` Object

The global `editor` object is the main entry point for the Fresh plugin API. It provides a set of methods for interacting with the editor, such as registering commands, manipulating buffers, and adding overlays.

### Asynchronous Nature

Many of the API calls are asynchronous and return `Promise`s. This ensures that your plugin doesn't block the editor's main thread. You should use `async/await` to work with these asynchronous methods.

## API Reference

The following API is available to TypeScript plugins via the global `editor` object.

### Commands

**`editor.registerCommand(name: string, description: string, action: string, context: string): void`**

Registers a new command in the editor.

*   `name`: The name of the command as it will appear in the command palette.
*   `description`: A brief description of what the command does.
*   `action`: The name of a global function that will be called when the command is executed.
*   `context`: The context in which the command is available (e.g., `"normal"`).

### UI

**`editor.setStatus(message: string): void`**

Displays a message in the status bar.

**`editor.debug(message: string): void`**

Makes a message to the editor's log file.

### Buffers

**`editor.getActiveBufferId(): number`**

Returns the ID of the currently active buffer.

**`editor.insertAtCursor(text: string): void`**

Inserts text at the current cursor position.

**`editor.addOverlay(bufferId: number, overlayId: string, start: number, end: number, r: number, g: number, b: number, underline: boolean): void`**

Adds a colored overlay to a buffer. `underline` should be `false` to use a background color.

**`editor.removeOverlaysByPrefix(bufferId: number, prefix: string): void`**

Removes all overlays from a buffer that have an ID starting with the given prefix.

**`editor.refreshLines(bufferId: number): void`**

Forces a refresh of the lines in the given buffer, which will trigger the `lines_changed` hook for all visible lines.

### Hooks

You can register a function to be called when an editor event occurs. To do this, you define a global function and then register it with `editor.on`.

**`editor.on(eventName: string, handlerName: string): void`**

*   `eventName`: The name of the event to listen for.
*   `handlerName`: The name of the global function to call.

**Example:**
```typescript
globalThis.onBufferClosed = function(data: { buffer_id: number }): void {
  editor.debug(`Buffer ${data.buffer_id} was closed.`);
};

editor.on("buffer_closed", "onBufferClosed");
```

**Available Hooks:**

*   `render_start`: Fired before the screen is rendered.
*   `lines_changed`: Fired when lines in a buffer are changed. This is a batched event for efficiency.
*   `after-insert`: Fired after text is inserted into a buffer.
*   `after-delete`: Fired after text is deleted from a buffer.
*   `buffer_closed`: Fired when a buffer is closed.

## Example Plugins

The `plugins/` directory contains several example plugins that you can use as a reference.

*   **`welcome.ts`:** A simple plugin that demonstrates how to register commands and show status messages.
*   **`todo_highlighter.ts`:** A more advanced plugin that uses overlays and hooks to highlight keywords in comments. This is a great example of how to write an efficient and powerful plugin.

By studying these examples, you can learn how to use the Fresh plugin API to create your own powerful and useful extensions for the editor.
