/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();


/**
 * Welcome Plugin
 * Shows a welcome message and registers demo commands
 */

// Show welcome message in status bar

// Register commands that use built-in actions
editor.registerCommand(
  "%cmd.open_help",
  "%cmd.open_help_desc",
  "show_help",
  null
);

editor.registerCommand(
  "%cmd.save_file",
  "%cmd.save_file_desc",
  "save",
  null
);

// Register commands with custom TypeScript callbacks
function plugin_say_hello() : void {
  editor.insertAtCursor(editor.t("text.greeting") + "\n");
  editor.setStatus(editor.t("status.greeting_inserted"));
  editor.debug("Plugin callback executed: say_hello");
}
registerHandler("plugin_say_hello", plugin_say_hello);

editor.registerCommand(
  "%cmd.say_hello",
  "%cmd.say_hello_desc",
  "plugin_say_hello",
  null
);

function plugin_insert_time() : void {
  const time = new Date().toLocaleTimeString();
  editor.insertAtCursor(`Current time: ${time}\n`);
  editor.setStatus(editor.t("status.time_inserted"));
  editor.debug(`Plugin callback executed: insert_time at ${time}`);
}
registerHandler("plugin_insert_time", plugin_insert_time);

editor.registerCommand(
  "%cmd.insert_time",
  "%cmd.insert_time_desc",
  "plugin_insert_time",
  null
);

function plugin_insert_comment() : void {
  editor.insertAtCursor(editor.t("text.comment") + "\n");
  editor.setStatus(editor.t("status.comment_inserted"));
  editor.debug("Plugin callback executed: insert_comment");
}
registerHandler("plugin_insert_comment", plugin_insert_comment);

editor.registerCommand(
  "%cmd.insert_comment",
  "%cmd.insert_comment_desc",
  "plugin_insert_comment",
  null
);

// Debug output
editor.debug("Welcome plugin initialized successfully!");
editor.debug("Registered 5 commands - try Ctrl+P to see them!");
editor.debug("   - 'Plugin Demo: Open Help' - toggles help screen (built-in action)");
editor.debug("   - 'Plugin Demo: Save File' - saves current file (built-in action)");
editor.debug("   - 'Plugin Demo: Say Hello' - inserts greeting (TypeScript callback)");
editor.debug("   - 'Plugin Demo: Insert Time' - inserts current time (TypeScript callback)");
editor.debug("   - 'Plugin Demo: Insert Comment' - inserts sample comment (TypeScript callback)");
