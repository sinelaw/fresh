# Common Plugin Patterns

## Common Patterns

### Highlighting Text

Use overlays to highlight text without modifying content:

```typescript
globalThis.highlight_word = function(): void {
  const bufferId = editor.getActiveBufferId();
  const cursor = editor.getCursorPosition();

  // Highlight 5 bytes starting at cursor with yellow background
  editor.addOverlay(
    bufferId,
    "my_highlight:1",  // Unique ID (use prefix for batch removal)
    cursor,
    cursor + 5,
    255, 255, 0,       // RGB color
    false              // underline
  );
};

// Later, remove all highlights with the prefix
editor.removeOverlaysByPrefix(bufferId, "my_highlight:");
```

### Creating Results Panels

Display search results, diagnostics, or other structured data in a virtual buffer:

```typescript
globalThis.show_results = async function(): Promise<void> {
  // Define keybindings for the results panel
  editor.defineMode("my-results", "special", [
    ["Return", "my_goto_result"],
    ["q", "close_buffer"]
  ], true);

  // Create the panel with embedded metadata
  await editor.createVirtualBufferInSplit({
    name: "*Results*",
    mode: "my-results",
    read_only: true,
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
    ratio: 0.3,           // Panel takes 30% of height
    panel_id: "my-results" // Reuse panel if it exists
  });
};

// Handle "go to" when user presses Enter
globalThis.my_goto_result = function(): void {
  const bufferId = editor.getActiveBufferId();
  const props = editor.getTextPropertiesAtCursor(bufferId);

  if (props.length > 0 && props[0].file) {
    editor.openFile(props[0].file, props[0].line, 0);
  }
};

editor.registerCommand("my_goto_result", "Go to result", "my_goto_result", "my-results");
```

### Running External Commands

Use `spawnProcess` to run shell commands:

```typescript
globalThis.run_tests = async function(): Promise<void> {
  editor.setStatus("Running tests...");

  const result = await editor.spawnProcess("cargo", ["test"], null);

  if (result.exit_code === 0) {
    editor.setStatus("Tests passed!");
  } else {
    editor.setStatus(`Tests failed: ${result.stderr.split('\n')[0]}`);
  }
};
```

### Invoking LSP Requests

Plugins can call `editor.sendLspRequest(language, method, params)` to run language-server-specific RPCs (clangd extensions, type hierarchy, switch header, etc.). Provide the target language ID (e.g., `"cpp"`) and the full method name, and handle the raw JSON response yourself.

```typescript
globalThis.switch_header = async function(): Promise<void> {
  const bufferId = editor.getActiveBufferId();
  const path = editor.getBufferPath(bufferId);
  const uri = `file://${path}`;
  const result = await editor.sendLspRequest("cpp", "textDocument/switchSourceHeader", {
    textDocument: { uri }
  });
  if (result && typeof result === "string") {
    editor.openFile(result, 0, 0);
  }
};
```

### File System Operations

Read and write files, check paths:

```typescript
globalThis.process_file = async function(): Promise<void> {
  const path = editor.getBufferPath(editor.getActiveBufferId());

  if (editor.fileExists(path)) {
    const content = await editor.readFile(path);
    const modified = content.replace(/TODO/g, "DONE");
    // `replaceFile`, not `writeFile`: running this twice on the same buffer
    // should rewrite the output, and `writeFile` refuses a path that exists.
    await editor.replaceFile(path + ".processed", modified);
  }
};
```
