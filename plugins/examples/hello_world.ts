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
globalThis.show_buffer_info = function (): void {
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
};

// Global action: Insert timestamp at cursor
globalThis.insert_timestamp = function (): void {
  const bufferId = editor.getActiveBufferId();
  const cursorPos = editor.getCursorPosition();
  const timestamp = new Date().toISOString();

  const success = editor.insertText(bufferId, cursorPos, timestamp);
  if (success) {
    editor.setStatus(`Inserted timestamp: ${timestamp}`);
  } else {
    editor.setStatus("Failed to insert timestamp");
  }
};

// Global action: Highlight current line (demo overlay)
globalThis.highlight_region = function (): void {
  const bufferId = editor.getActiveBufferId();
  const cursorPos = editor.getCursorPosition();

  // Highlight 10 characters around cursor
  const start = Math.max(0, cursorPos - 5);
  const end = cursorPos + 5;

  const success = editor.addOverlay(
    bufferId,
    "demo-highlight",
    start,
    end,
    255, // Red
    255, // Green
    0, // Blue (yellow highlight)
    false // No underline
  );

  if (success) {
    editor.setStatus(`Highlighted region ${start}-${end}`);
  }
};

// Global action: Remove highlight
globalThis.clear_highlight = function (): void {
  const bufferId = editor.getActiveBufferId();
  const success = editor.removeOverlay(bufferId, "demo-highlight");
  if (success) {
    editor.setStatus("Cleared highlight");
  }
};

// Global async action: Demonstrate async/await
globalThis.async_demo = async function (): Promise<void> {
  editor.setStatus("Starting async operation...");

  // Simulate some async work
  await Promise.resolve();

  const bufferId = editor.getActiveBufferId();
  const length = editor.getBufferLength(bufferId);

  editor.setStatus(`Async operation complete! Buffer has ${length} bytes`);
};

// Log that plugin loaded
editor.debug("Hello World plugin loaded!");
editor.setStatus("Hello World plugin ready");
