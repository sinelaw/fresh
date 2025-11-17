// Fresh Editor TypeScript Plugin Bootstrap
// This file sets up the global 'editor' API object that plugins can use

const core = Deno.core;

// Create the editor API object
const editor = {
  // Status bar
  setStatus(message) {
    core.ops.op_fresh_set_status(message);
  },

  // Logging
  debug(message) {
    core.ops.op_fresh_debug(message);
  },

  // Buffer operations (placeholders for now)
  getActiveBufferId() {
    return core.ops.op_fresh_get_active_buffer_id();
  },

  // TODO: Add more ops as they are implemented in Rust
  // - getBufferInfo
  // - insertText
  // - deleteRange
  // - addOverlay
  // - removeOverlay
  // - registerCommand
  // - defineMode
  // - createVirtualBufferInSplit (async)
  // - spawn (async)
  // - openFile (async)
};

// Make editor globally available
globalThis.editor = editor;

// Log that the runtime is ready
console.log("Fresh TypeScript plugin runtime initialized");
