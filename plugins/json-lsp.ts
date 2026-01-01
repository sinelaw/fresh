/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();


/**
 * JSON LSP Helper Plugin
 *
 * Provides user-friendly error handling for JSON LSP server issues.
 * When the JSON language server fails to start, this plugin shows an
 * actionable popup with installation instructions.
 *
 * Features:
 * - Detects JSON LSP server errors
 * - Shows popup with install commands (npm)
 * - Allows copying install commands to clipboard
 * - Provides option to disable JSON LSP
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

// Install commands for JSON LSP server
// vscode-langservers-extracted provides HTML, CSS, and JSON language servers
// See: https://www.npmjs.com/package/vscode-langservers-extracted
const INSTALL_COMMANDS = {
  npm: "npm install -g vscode-langservers-extracted",
};

// Track error state for JSON LSP
let jsonLspError: { serverCommand: string; message: string } | null = null;

/**
 * Handle LSP server errors for JSON
 */
globalThis.on_json_lsp_server_error = function (
  data: LspServerErrorData
): void {
  // Only handle JSON language errors
  if (data.language !== "json") {
    return;
  }

  editor.debug(`json-lsp: Server error - ${data.error_type}: ${data.message}`);

  // Store error state for later reference
  jsonLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  // Show a status message for immediate feedback
  if (data.error_type === "not_found") {
    editor.setStatus(
      `JSON LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`JSON LSP error: ${data.message}`);
  }
};

// Register hook for LSP server errors
editor.on("lsp_server_error", "on_json_lsp_server_error");

/**
 * Handle status bar click when there's a JSON LSP error
 */
globalThis.on_json_lsp_status_clicked = function (
  data: LspStatusClickedData
): void {
  // Only handle JSON language clicks when there's an error
  if (data.language !== "json" || !jsonLspError) {
    return;
  }

  editor.debug("json-lsp: Status clicked, showing help popup");

  // Show action popup with install options
  editor.showActionPopup({
    id: "json-lsp-help",
    title: "JSON Language Server Not Found",
    message: `"${jsonLspError.serverCommand}" provides code completion, validation, and formatting for JSON files. Copy the command below to install it.`,
    actions: [
      { id: "copy_npm", label: `Copy: ${INSTALL_COMMANDS.npm}` },
      { id: "disable", label: "Disable JSON LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
};

// Register hook for status bar clicks
editor.on("lsp_status_clicked", "on_json_lsp_status_clicked");

/**
 * Handle action popup results for JSON LSP help
 */
globalThis.on_json_lsp_action_result = function (
  data: ActionPopupResultData
): void {
  // Only handle our popup
  if (data.popup_id !== "json-lsp-help") {
    return;
  }

  editor.debug(`json-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_npm":
      editor.setClipboard(INSTALL_COMMANDS.npm);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.npm);
      break;

    case "disable":
      editor.disableLspForLanguage("json");
      editor.setStatus("JSON LSP disabled");
      jsonLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      // Just close the popup without action
      break;

    default:
      editor.debug(`json-lsp: Unknown action: ${data.action_id}`);
  }
};

// Register hook for action popup results
editor.on("action_popup_result", "on_json_lsp_action_result");

editor.debug("json-lsp: Plugin loaded");
