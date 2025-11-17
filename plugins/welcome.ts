/// <reference path="../types/fresh.d.ts" />

/**
 * Welcome Plugin
 * Shows a welcome message and registers demo commands
 */

// Show welcome message in status bar
editor.setStatus("Plugins are working! Welcome Plugin loaded successfully!");

// Register commands that use built-in actions
editor.registerCommand(
  "Plugin Demo: Open Help",
  "Open the editor help page (uses built-in action)",
  "show_help",
  "normal"
);

editor.registerCommand(
  "Plugin Demo: Save File",
  "Save the current file (uses built-in action)",
  "save",
  "normal"
);

// Register commands with custom TypeScript callbacks
globalThis.plugin_say_hello = function(): void {
  editor.insertAtCursor("Hello from TypeScript! The plugin system is working!\n");
  editor.setStatus("Inserted greeting at cursor position");
  editor.debug("Plugin callback executed: say_hello");
};

editor.registerCommand(
  "Plugin Demo: Say Hello",
  "Insert a friendly greeting into the buffer",
  "plugin_say_hello",
  "normal"
);

globalThis.plugin_insert_time = function(): void {
  const time = new Date().toLocaleTimeString();
  editor.insertAtCursor(`Current time: ${time}\n`);
  editor.setStatus("Inserted time at cursor position");
  editor.debug(`Plugin callback executed: insert_time at ${time}`);
};

editor.registerCommand(
  "Plugin Demo: Insert Time",
  "Insert the current time at cursor position",
  "plugin_insert_time",
  "normal"
);

globalThis.plugin_insert_comment = function(): void {
  editor.insertAtCursor("// This comment was inserted by a TypeScript plugin!\n");
  editor.setStatus("Comment inserted by plugin");
  editor.debug("Plugin callback executed: insert_comment");
};

editor.registerCommand(
  "Plugin Demo: Insert Comment",
  "Insert a sample comment at cursor position",
  "plugin_insert_comment",
  "normal"
);

// Debug output
editor.debug("Welcome plugin initialized successfully!");
editor.debug("Registered 5 commands - try Ctrl+P to see them!");
editor.debug("   - 'Plugin Demo: Open Help' - toggles help screen (built-in action)");
editor.debug("   - 'Plugin Demo: Save File' - saves current file (built-in action)");
editor.debug("   - 'Plugin Demo: Say Hello' - inserts greeting (TypeScript callback)");
editor.debug("   - 'Plugin Demo: Insert Time' - inserts current time (TypeScript callback)");
editor.debug("   - 'Plugin Demo: Insert Comment' - inserts sample comment (TypeScript callback)");
