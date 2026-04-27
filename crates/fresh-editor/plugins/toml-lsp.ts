/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * TOML LSP Helper Plugin
 *
 * Provides user-friendly error handling for TOML LSP server issues.
 * When taplo fails to start, this plugin shows an actionable
 * popup with installation instructions.
 *
 * Features:
 * - Detects TOML LSP server errors (taplo)
 * - Shows popup with install commands (cargo, npm, brew)
 * - Allows copying install commands to clipboard
 * - Provides option to disable TOML LSP
 *
 * Notes:
 * - Taplo supports schema validation for Cargo.toml, pyproject.toml, etc.
 * - Also available as a VS Code extension and CLI formatter
 */

interface LspServerErrorData {
  language: string;
  server_command: string;
  error_type: string;
  message: string;
}

interface LspStatusClickedData {
  language: string;
  has_error: boolean;
}

interface ActionPopupResultData {
  popup_id: string;
  action_id: string;
}

// Install commands for TOML LSP server (taplo)
// See: https://taplo.tamasfe.dev/cli/installation.html
const INSTALL_COMMANDS = {
  cargo: "cargo install taplo-cli --locked",
  npm: "npm i -g @taplo/cli",
  brew: "brew install taplo",
};

// Track error state for TOML LSP
let tomlLspError: { serverCommand: string; message: string } | null = null;

/**
 * Handle LSP server errors for TOML
 */


// Register hook for LSP server errors
editor.on("lsp_server_error", (data) => {
  // Only handle TOML language errors
  if (data.language !== "toml") {
    return;
  }

  editor.debug(`toml-lsp: Server error - ${data.error_type}: ${data.message}`);

  // Store error state for later reference
  tomlLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  // Show a status message for immediate feedback
  if (data.error_type === "not_found") {
    editor.setStatus(
      `TOML LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`TOML LSP error: ${data.message}`);
  }
});

/**
 * Handle status bar click when there's a TOML LSP error
 */


// Register hook for status bar clicks
editor.on("lsp_status_clicked", (data) => {
  // Only handle TOML language clicks when there's an error
  if (data.language !== "toml" || !tomlLspError) {
    return;
  }

  editor.debug("toml-lsp: Status clicked, showing help popup");

  // Show action popup with install options
  editor.showActionPopup({
    id: "toml-lsp-help",
    title: "TOML Language Server Not Found",
    message: `"${tomlLspError.serverCommand}" provides code completion, validation, formatting, and schema support for TOML files (Cargo.toml, pyproject.toml, etc.). Copy a command below to install it, or visit https://taplo.tamasfe.dev/cli/installation.html for details.`,
    actions: [
      { id: "copy_cargo", label: `Copy: ${INSTALL_COMMANDS.cargo}` },
      { id: "copy_npm", label: `Copy: ${INSTALL_COMMANDS.npm}` },
      { id: "copy_brew", label: `Copy: ${INSTALL_COMMANDS.brew}` },
      { id: "disable", label: "Disable TOML LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
});

/**
 * Handle action popup results for TOML LSP help
 */


// Register hook for action popup results
editor.on("action_popup_result", (data) => {
  // Only handle our popup
  if (data.popup_id !== "toml-lsp-help") {
    return;
  }

  editor.debug(`toml-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_cargo":
      editor.setClipboard(INSTALL_COMMANDS.cargo);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.cargo);
      break;

    case "copy_npm":
      editor.setClipboard(INSTALL_COMMANDS.npm);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.npm);
      break;

    case "copy_brew":
      editor.setClipboard(INSTALL_COMMANDS.brew);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.brew);
      break;

    case "disable":
      editor.disableLspForLanguage("toml");
      editor.setStatus("TOML LSP disabled");
      tomlLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      // Just close the popup without action
      break;

    default:
      editor.debug(`toml-lsp: Unknown action: ${data.action_id}`);
  }
});

editor.debug("toml-lsp: Plugin loaded");
