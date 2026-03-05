/// <reference path="../../types/fresh.d.ts" />

/**
 * Hello World TypeScript Plugin for Fresh Editor
 *
 * This is a simple example plugin that demonstrates:
 * - Querying editor state (buffer info, cursor position)
 * - Sending commands (status messages, text insertion)
 * - Using async/await for plugin actions
 */

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

// Log that plugin loaded
editor.debug("Hello World plugin loaded!");
editor.setStatus("Hello World plugin ready");
