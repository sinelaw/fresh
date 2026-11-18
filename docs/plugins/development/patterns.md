# Common Plugin Patterns

## Common Patterns

### Highlighting Text

Use overlays to highlight text without modifying content:

```typescript
registerHandler("highlight_word", () => {
  const bufferId = editor.getActiveBufferId();
  const cursor = editor.getCursorPosition();

  // Highlight 5 bytes starting at the cursor with a yellow background
  editor.addOverlay(bufferId, "my_highlight", cursor, cursor + 5, {
    bg: [255, 255, 0],  // RGB array, or a theme key such as "editor.selection_bg"
    underline: false,
  });
});

registerHandler("clear_highlights", () => {
  // Remove every overlay in the namespace at once
  editor.clearNamespace(editor.getActiveBufferId(), "my_highlight");
});
```

### Creating Results Panels

Display search results, diagnostics, or other structured data in a virtual buffer:

```typescript
registerHandler("show_results", async () => {
  // Define keybindings for the results panel (read-only)
  editor.defineMode("my-results", [
    ["Return", "my_goto_result"],
    ["q", "close_buffer"]
  ], true);

  // Create the panel with embedded metadata
  await editor.createVirtualBufferInSplit({
    name: "*Results*",
    mode: "my-results",
    readOnly: true,
    entries: [
      {
        text: "src/main.rs:42: found match\n",
        properties: { file: "src/main.rs", line: 42 }
      },
      {
        text: "src/lib.rs:100: another match\n",
        properties: { file: "src/lib.rs", line: 100 }
      }
    ],
    ratio: 0.7,           // Existing content keeps 70%, the panel gets 30%
    panelId: "my-results" // Reuse panel if it exists
  });
});

// Handle "go to" when user presses Enter
registerHandler("my_goto_result", () => {
  const bufferId = editor.getActiveBufferId();
  const props = editor.getTextPropertiesAtCursor(bufferId);

  if (props.length > 0 && props[0].file) {
    editor.openFile(props[0].file as string, props[0].line as number, 0);
  }
});

editor.registerCommand("my_goto_result", "Go to result", "my_goto_result", "my-results");
```

### Running External Commands

Use `runOnTarget` to run a command on the active window's machine — on its
SSH host or in its container when the workspace is remote. No shell is
involved: pass the program and its arguments already split.

```typescript
registerHandler("run_tests", async () => {
  editor.setStatus("Running tests...");

  const result = await editor.runOnTarget("cargo", ["test"], editor.getCwd());

  if (result.code === 0) {
    editor.setStatus("Tests passed!");
  } else {
    editor.setStatus(`Tests failed: ${result.stderr.split('\n')[0]}`);
  }
});
```

To run a command in another window's workspace, use the machine
`editor.openMachine({ kind: "window", window: id })` returns: its `run` takes
the same arguments and runs where that window's files are. Both return a
handle you can `await` or `kill()`. `spawnProcess` is deprecated: it does the
same as `runOnTarget` but resolves with `exit_code` instead of `code`.

### Invoking LSP Requests

Plugins can call `editor.sendLspRequest(language, method, params)` to run language-server-specific RPCs (clangd extensions, type hierarchy, switch header, etc.). Provide the target language ID (e.g., `"cpp"`) and the full method name, and handle the raw JSON response yourself.

```typescript
registerHandler("switch_header", async () => {
  const bufferId = editor.getActiveBufferId();
  const path = editor.getBufferPath(bufferId);
  const uri = `file://${path}`;
  const result = await editor.sendLspRequest("cpp", "textDocument/switchSourceHeader", {
    textDocument: { uri }
  });
  if (result && typeof result === "string") {
    editor.openFile(result, 0, 0);
  }
});
```

### File System Operations

Read and write files, check paths:

```typescript
registerHandler("process_file", () => {
  const path = editor.getBufferPath(editor.getActiveBufferId());

  const content = editor.fileExists(path) ? editor.readFile(path) : null;
  if (content !== null) {
    const modified = content.replace(/TODO/g, "DONE");
    // `replaceFile`, not `writeFile`: running this twice on the same buffer
    // should rewrite the output, and `writeFile` refuses a path that exists.
    editor.replaceFile(path + ".processed", modified);
  }
});
```
