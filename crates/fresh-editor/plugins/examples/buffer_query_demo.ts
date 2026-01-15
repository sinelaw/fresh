/// <reference path="../../types/fresh.d.ts" />

/**
 * Buffer Query Demo Plugin
 * Demonstrates the buffer query APIs in Phase 2
 */

// Show buffer info
globalThis.show_buffer_info_demo = function(): void {
  const bufferId = editor.getActiveBufferId();
  const info = editor.getBufferInfo(bufferId);

  if (info) {
    const msg = `Buffer ${info.id}: ${info.path || "[No Name]"} (${
      info.modified ? "modified" : "saved"
    }, ${info.length} bytes)`;
    editor.setStatus(msg);
  } else {
    editor.setStatus("No buffer info available");
  }
};

editor.registerCommand(
  "Query Demo: Show Buffer Info",
  "Display information about the current buffer",
  "show_buffer_info_demo",
  "normal"
);

// Show cursor position with selection info
globalThis.show_cursor_info_demo = function(): void {
  const cursor = editor.getPrimaryCursor();

  if (cursor) {
    let msg: string;
    if (cursor.selection) {
      msg = `Cursor at ${cursor.position}, selection: ${cursor.selection.start}-${cursor.selection.end} (${
        cursor.selection.end - cursor.selection.start
      } chars)`;
    } else {
      msg = `Cursor at byte position ${cursor.position} (no selection)`;
    }
    editor.setStatus(msg);
  } else {
    editor.setStatus("No cursor info available");
  }
};

editor.registerCommand(
  "Query Demo: Show Cursor Position",
  "Display cursor position and selection info",
  "show_cursor_info_demo",
  "normal"
);

// Count all cursors (multi-cursor support)
globalThis.count_cursors_demo = function(): void {
  const cursors = editor.getAllCursors();
  editor.setStatus(`Active cursors: ${cursors.length}`);
};

editor.registerCommand(
  "Query Demo: Count All Cursors",
  "Display the number of active cursors",
  "count_cursors_demo",
  "normal"
);

// List all buffers
globalThis.list_all_buffers_demo = function(): void {
  const buffers = editor.listBuffers();
  let modifiedCount = 0;

  for (const buf of buffers) {
    if (buf.modified) {
      modifiedCount++;
    }
  }

  editor.setStatus(`Open buffers: ${buffers.length} (${modifiedCount} modified)`);
};

editor.registerCommand(
  "Query Demo: List All Buffers",
  "Show count of open buffers",
  "list_all_buffers_demo",
  "normal"
);

// Show viewport info
globalThis.show_viewport_demo = function(): void {
  const vp = editor.getViewport();

  if (vp) {
    const msg = `Viewport: ${vp.width}x${vp.height}, top_byte=${vp.top_byte}, left_col=${vp.left_column}`;
    editor.setStatus(msg);
  } else {
    editor.setStatus("No viewport info available");
  }
};

editor.registerCommand(
  "Query Demo: Show Viewport Info",
  "Display viewport dimensions and scroll position",
  "show_viewport_demo",
  "normal"
);

editor.setStatus("Buffer Query Demo plugin loaded! Try the 'Query Demo' commands.");
editor.debug("Buffer Query Demo plugin initialized (TypeScript version)");
