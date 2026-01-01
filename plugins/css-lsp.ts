/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();


/**
 * CSS LSP Helper Plugin
 *
 * Provides user-friendly error handling for CSS LSP server issues.
 * When the CSS language server fails to start, this plugin shows an
 * actionable popup with installation instructions.
 *
 * Features:
 * - Detects CSS LSP server errors
 * - Shows popup with install commands (npm)
 * - Allows copying install commands to clipboard
 * - Provides option to disable CSS LSP
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

// Install commands for CSS LSP server
// vscode-langservers-extracted provides HTML, CSS, and JSON language servers
// See: https://www.npmjs.com/package/vscode-langservers-extracted
const INSTALL_COMMANDS = {
  npm: "npm install -g vscode-langservers-extracted",
};

// Track error state for CSS LSP
let cssLspError: { serverCommand: string; message: string } | null = null;

/**
 * Handle LSP server errors for CSS
 */
globalThis.on_css_lsp_server_error = function (data: LspServerErrorData): void {
  // Only handle CSS language errors
  if (data.language !== "css") {
    return;
  }

  editor.debug(`css-lsp: Server error - ${data.error_type}: ${data.message}`);

  // Store error state for later reference
  cssLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  // Show a status message for immediate feedback
  if (data.error_type === "not_found") {
    editor.setStatus(
      `CSS LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`CSS LSP error: ${data.message}`);
  }
};

// Register hook for LSP server errors
editor.on("lsp_server_error", "on_css_lsp_server_error");

/**
 * Handle status bar click when there's a CSS LSP error
 */
globalThis.on_css_lsp_status_clicked = function (
  data: LspStatusClickedData
): void {
  // Only handle CSS language clicks when there's an error
  if (data.language !== "css" || !cssLspError) {
    return;
  }

  editor.debug("css-lsp: Status clicked, showing help popup");

  // Show action popup with install options
  editor.showActionPopup({
    id: "css-lsp-help",
    title: "CSS Language Server Not Found",
    message: `"${cssLspError.serverCommand}" provides code completion, diagnostics, and formatting for CSS files. Copy the command below to install it.`,
    actions: [
      { id: "copy_npm", label: `Copy: ${INSTALL_COMMANDS.npm}` },
      { id: "disable", label: "Disable CSS LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
};

// Register hook for status bar clicks
editor.on("lsp_status_clicked", "on_css_lsp_status_clicked");

/**
 * Handle action popup results for CSS LSP help
 */
globalThis.on_css_lsp_action_result = function (
  data: ActionPopupResultData
): void {
  // Only handle our popup
  if (data.popup_id !== "css-lsp-help") {
    return;
  }

  editor.debug(`css-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_npm":
      editor.setClipboard(INSTALL_COMMANDS.npm);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.npm);
      break;

    case "disable":
      editor.disableLspForLanguage("css");
      editor.setStatus("CSS LSP disabled");
      cssLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      // Just close the popup without action
      break;

    default:
      editor.debug(`css-lsp: Unknown action: ${data.action_id}`);
  }
};

// Register hook for action popup results
editor.on("action_popup_result", "on_css_lsp_action_result");

editor.debug("css-lsp: Plugin loaded");
