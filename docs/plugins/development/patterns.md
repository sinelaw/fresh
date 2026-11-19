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

### Smart Enter and Tab: Edit Rules

To change what Enter, Tab or Shift+Tab do in some languages — continue a list or a comment, indent a blank item — use **edit rules** rather than binding the keys in a mode. Rules that never change belong in the language's config: its `edit_rules` setting (`languages.<id>.edit_rules`, see [Edit rules](../../configuration/index.md#edit-rules)), which a [language pack](language-packs.md) ships as `editRules` in its `package.json`, with no code. Markdown's list editing is built in this way. A plugin registers rules at run time for what depends on its state — rules it turns on and off, or builds from its own settings. Rules are data: the editor applies them itself, inside the key's own edit, so they keep up with fast typing, work under any keymap (vi's insert mode included), apply at every cursor, and undo with the key in one step. A key whose line no rule matches does what it always does.

```typescript
const editor = getEditor();

// Markdown-style lists in plain-text files.
editor.registerEditRules("lists", {
  languages: ["text"],
  rules: [
    // Enter on an empty item: remove the marker.
    { on: "newline", before: "^[ \\t]*[-*+] \\s*$", after: "^\\s*$",
      replaceBefore: "", replaceAfter: "" },
    // Enter on an ordered item: the next number.
    { on: "newline", before: "^([ \\t]*)(\\d+)\\. ", insert: "\n$1${inc:2}. " },
    // Tab on a blank bullet: one level deeper, next bullet.
    { on: "indent", before: "^([ \\t]*)([-*+]) \\s*$", after: "^\\s*$",
      replaceBefore: "$1${indent}${cycle:2:*|-|+} ", replaceAfter: "" },
  ],
});
```

Each rule names its key (`on`: `"newline"`, `"indent"` or `"dedent"`) and two optional regular expressions, searched in the cursor's line before and after the cursor. The first rule whose patterns both match rewrites the line as `(replaceBefore ?? before) + (insert ?? "") + cursor + (replaceAfter ?? after)`. Templates take `$1` / `${name}` captures, `${inc:N}` (a number plus one), `${cycle:N:a|b|c}` (the next value in a list), `${indent}` / `${dedent:N}` (one indentation level of the buffer, added or taken off), and a line break (a real one, or the escape `\n`) for the buffer's line ending. In config.json and a pack's manifest the keys are snake_case (`replace_before`, `replace_after`). Actions a plugin runs with `executeAction` (vi's `o` is `move_line_end` + `insert_newline`) take only the rules that add text at the cursor. See [`registerEditRules`](../api/buffer.md) and `EditRule` in the [type reference](../api/types.md) for the details. A malformed rule throws when registered; a plugin's rules go away when it is unloaded or disabled. A buffer's configured `edit_rules` are tried before the rules plugins register, so the user's config wins on a line both match; `"edit_rules": []` leaves only the plugins' rules. Markdown's built-in rules (`languages.markdown.edit_rules` in the default config) are a complete example.
