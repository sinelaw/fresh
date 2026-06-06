/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * GDScript LSP Helper Plugin
 *
 * Provides user-friendly error handling for the Godot editor's built-in
 * GDScript language server. Fresh connects through netcat to Godot's default
 * TCP LSP port.
 */

const INSTALL_COMMANDS = {
  macos: "brew install netcat",
  debian: "sudo apt install netcat-openbsd",
  arch: "sudo pacman -S openbsd-netcat",
};

let gdscriptLspError: { serverCommand: string; message: string } | null = null;

editor.on("lsp_server_error", (data) => {
  if (data.language !== "gdscript") {
    return;
  }

  editor.debug(
    `gdscript-lsp: Server error - ${data.error_type}: ${data.message}`
  );

  gdscriptLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  if (data.error_type === "not_found") {
    editor.setStatus(
      `GDScript LSP connector '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(
      "GDScript LSP error. Start Godot with the project open and check port 6005."
    );
  }
});

editor.on("lsp_status_clicked", (data) => {
  if (data.language !== "gdscript" || !gdscriptLspError) {
    return;
  }

  editor.debug("gdscript-lsp: Status clicked, showing help popup");

  editor.showActionPopup({
    id: "gdscript-lsp-help",
    title: "GDScript Language Server Unavailable",
    message: `"${gdscriptLspError.serverCommand}" connects Fresh to Godot's built-in GDScript language server at 127.0.0.1:6005. Open the project in Godot, enable the language server in Godot editor settings if needed, and install netcat if the connector is missing.`,
    actions: [
      { id: "copy_macos", label: `Copy: ${INSTALL_COMMANDS.macos}` },
      { id: "copy_debian", label: `Copy: ${INSTALL_COMMANDS.debian}` },
      { id: "copy_arch", label: `Copy: ${INSTALL_COMMANDS.arch}` },
      { id: "disable", label: "Disable GDScript LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
});

editor.on("action_popup_result", (data) => {
  if (data.popup_id !== "gdscript-lsp-help") {
    return;
  }

  editor.debug(`gdscript-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_macos":
      editor.setClipboard(INSTALL_COMMANDS.macos);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.macos);
      break;

    case "copy_debian":
      editor.setClipboard(INSTALL_COMMANDS.debian);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.debian);
      break;

    case "copy_arch":
      editor.setClipboard(INSTALL_COMMANDS.arch);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.arch);
      break;

    case "disable":
      editor.disableLspForLanguage("gdscript");
      editor.setStatus("GDScript LSP disabled");
      gdscriptLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      break;

    default:
      editor.debug(`gdscript-lsp: Unknown action: ${data.action_id}`);
  }
});

editor.debug("gdscript-lsp: Plugin loaded");
