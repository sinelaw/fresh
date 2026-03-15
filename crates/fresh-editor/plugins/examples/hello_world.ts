/// <reference path="../../types/fresh.d.ts" />

/**
 * Hello World TypeScript Plugin for Fresh Editor
 *
 * This is a simple example plugin that demonstrates:
 * - Querying editor state (buffer info, cursor position)
 * - Obtaining selected text and replacing selection (primary cursor, buffer text)
 * - Sending commands (status messages, text insertion)
 * - Using async/await for plugin actions
 */
// Global action: Replace selection with quoted selection
async function quote_selection() : void {
  const bufferId = editor.getActiveBufferId();
  const cursorInfo = editor.getPrimaryCursor();
  if (! cursorInfo.selection) {
      editor.setStatus(`Nothing is highlighted!`);
  }
  const startSelection = cursorInfo.selection.start;
  const endSelection = cursorInfo.selection.end;
  const bufText = await 
      editor.getBufferText(bufferId, startSelection, endSelection);

  const success1 = editor.insertText(bufferId, startSelection, "'");
  if (!success1) {
    editor.setStatus("Failed to insert start quote");
    return;
  }

  const success2 = editor.insertText(bufferId, endSelection+1, "'");
  if (!success2) {
    editor.setStatus("Failed to insert end quote");
    return
  }
  const statusMessage = `Quoted: ${bufText}`;
  editor.setStatus(statusMessage)
}
registerHandler("quote_selection", quote_selection);
 
// Global action: Display primary cursor information
function show_cursor_info() : void {
  const cursorInfo = editor.getPrimaryCursor();
  
  const status = `Cursor at ${cursorInfo.position} Selection start-${cursorInfo.selection.start}, end-${cursorInfo.selection.end}`;

  editor.setStatus(status);
  editor.debug(`Cursor info: ${status}`);
}
registerHandler("show_cursor_info", show_cursor_info);

// Global action: Display buffer information
function show_buffer_info() : void {
  const bufferId = editor.getActiveBufferId();
  const path = editor.getBufferPath(bufferId);
  const length = editor.getBufferLength(bufferId);
  const modified = editor.isBufferModified(bufferId);
  const cursorPos = editor.getCursorPosition();

  const status = `Buffer ${bufferId}: ${path || "[untitled]"} | ${length} bytes | ${
    modified ? "modified" : "saved"
  } | cursor@${cursorPos}`;

  editor.setStatus(status);
  editor.debug(`Buffer info: ${status}`);
}
registerHandler("show_buffer_info", show_buffer_info);

// Global action: Insert timestamp at cursor
function insert_timestamp() : void {
  const bufferId = editor.getActiveBufferId();
  const cursorPos = editor.getCursorPosition();
  const timestamp = new Date().toISOString();

  const success = editor.insertText(bufferId, cursorPos, timestamp);
  if (success) {
    editor.setStatus(`Inserted timestamp: ${timestamp}`);
  } else {
    editor.setStatus("Failed to insert timestamp");
  }
}
registerHandler("insert_timestamp", insert_timestamp);

// Global action: Highlight current line (demo overlay)
function highlight_region() : void {
  const bufferId = editor.getActiveBufferId();
  const cursorPos = editor.getCursorPosition();

  // Highlight 10 characters around cursor
  const start = Math.max(0, cursorPos - 5);
  const end = cursorPos + 5;

  // Use namespace "demo" for batch operations
  const success = editor.addOverlay(bufferId, "demo", start, end, {
    fg: [255, 255, 0],  // Yellow highlight
  });

  if (success) {
    editor.setStatus(`Highlighted region ${start}-${end}`);
  }
}
registerHandler("highlight_region", highlight_region);

// Global action: Remove highlight
function clear_highlight() : void {
  const bufferId = editor.getActiveBufferId();
  // Clear all overlays in the "demo" namespace
  const success = editor.clearNamespace(bufferId, "demo");
  if (success) {
    editor.setStatus("Cleared highlight");
  }
}
registerHandler("clear_highlight", clear_highlight);

// Global async action: Demonstrate async/await
async function async_demo() : Promise<void> {
  editor.setStatus("Starting async operation...");

  // Simulate some async work
  await Promise.resolve();

  const bufferId = editor.getActiveBufferId();
  const length = editor.getBufferLength(bufferId);

  editor.setStatus(`Async operation complete! Buffer has ${length} bytes`);
}
registerHandler("async_demo", async_demo);

// Register commands so they appear in the command palette (Ctrl+P)
editor.registerCommand(
  "Hello: Quote Selection",
  "Quote Selection",
  "quote_selection"
);

editor.registerCommand(
  "Hello: Show Cursor Info",
  "Display primary cursor information",
  "show_cursor_info"
);

editor.registerCommand(
  "Hello: Show Buffer Info",
  "Display active buffer information",
  "show_buffer_info"
);

editor.registerCommand(
  "Hello: Insert Timestamp",
  "Insert an ISO timestamp at the cursor",
  "insert_timestamp"
);

editor.registerCommand(
  "Hello: Highlight Region",
  "Highlight 10 characters around cursor",
  "highlight_region"
);

editor.registerCommand(
  "Hello: Clear Highlight",
  "Remove demo highlights",
  "clear_highlight"
);

editor.registerCommand(
  "Hello: Async Demo",
  "Demonstrate async/await",
  "async_demo"
);

// Log that plugin loaded
editor.debug("Hello World plugin loaded!");
editor.setStatus("Hello World plugin ready");
