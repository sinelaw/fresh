// Virtual Buffer Demo Plugin
// Demonstrates the virtual buffer API for creating diagnostic panels, search results, etc.

// Register a command to show a demo virtual buffer
editor.registerCommand(
  "Virtual Buffer Demo",
  "Show a demo virtual buffer with sample diagnostics",
  "show_virtual_buffer_demo",
  "normal"
);

// Define a custom mode for the demo buffer
editor.defineMode(
  "demo-list", // mode name
  null,         // no parent mode
  [
    ["Return", "demo_goto_item"],
    ["n", "demo_next_item"],
    ["p", "demo_prev_item"],
    ["q", "demo_close_buffer"],
  ],
  true // read-only
);

// Register actions for the mode
globalThis.demo_goto_item = () => {
  const bufferId = editor.getActiveBufferId();
  const props = editor.getTextPropertiesAtCursor(bufferId);

  if (props.length > 0) {
    const location = props[0].location as { file: string; line: number; column: number } | undefined;
    if (location) {
      editor.openFile(location.file, location.line, location.column || 0);
      editor.setStatus(`Jumped to ${location.file}:${location.line}`);
    } else {
      editor.setStatus("No location info for this item");
    }
  } else {
    editor.setStatus("No properties at cursor position");
  }
};

globalThis.demo_next_item = () => {
  editor.setStatus("Next item (not implemented in demo)");
};

globalThis.demo_prev_item = () => {
  editor.setStatus("Previous item (not implemented in demo)");
};

globalThis.demo_close_buffer = () => {
  editor.setStatus("Close buffer (not implemented in demo)");
};

// Main action: show the virtual buffer
globalThis.show_virtual_buffer_demo = () => {
  editor.setStatus("Creating virtual buffer demo...");

  // Create sample diagnostic entries
  const entries = [
    {
      text: "[ERROR] src/main.rs:42:10 - undefined variable 'foo'\n",
      properties: {
        severity: "error",
        location: { file: "src/main.rs", line: 42, column: 10 },
        message: "undefined variable 'foo'",
      },
    },
    {
      text: "[WARNING] src/lib.rs:100:5 - unused variable 'bar'\n",
      properties: {
        severity: "warning",
        location: { file: "src/lib.rs", line: 100, column: 5 },
        message: "unused variable 'bar'",
      },
    },
    {
      text: "[INFO] src/utils.rs:25:1 - consider using 'if let' instead of 'match'\n",
      properties: {
        severity: "info",
        location: { file: "src/utils.rs", line: 25, column: 1 },
        message: "consider using 'if let' instead of 'match'",
      },
    },
    {
      text: "[HINT] src/config.rs:8:20 - type annotation unnecessary\n",
      properties: {
        severity: "hint",
        location: { file: "src/config.rs", line: 8, column: 20 },
        message: "type annotation unnecessary",
      },
    },
  ];

  // Create the virtual buffer in a horizontal split
  const success = editor.createVirtualBufferInSplit({
    name: "*Demo Diagnostics*",
    mode: "demo-list",
    read_only: true,
    entries: entries,
    ratio: 0.7, // Original pane takes 70%, demo buffer takes 30%
    panel_id: "demo-diagnostics",
    show_line_numbers: false,
    show_cursors: true,
  });

  if (success) {
    editor.setStatus(`Created demo virtual buffer with ${entries.length} items - Press RET to jump to location`);
  } else {
    editor.setStatus("Failed to create virtual buffer");
  }
};

// Log that the plugin loaded
editor.debug("Virtual buffer demo plugin loaded");
