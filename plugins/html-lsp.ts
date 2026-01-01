/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();


/**
 * HTML LSP Helper Plugin
 *
 * Provides user-friendly error handling for HTML LSP server issues.
 * When the HTML language server fails to start, this plugin shows an
 * actionable popup with installation instructions.
 *
 * Features:
 * - Detects HTML LSP server errors
 * - Shows popup with install commands (npm)
 * - Allows copying install commands to clipboard
 * - Provides option to disable HTML LSP
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

// Install commands for HTML LSP server
// vscode-langservers-extracted provides HTML, CSS, and JSON language servers
// See: https://www.npmjs.com/package/vscode-langservers-extracted
const INSTALL_COMMANDS = {
  npm: "npm install -g vscode-langservers-extracted",
};

// Track error state for HTML LSP
let htmlLspError: { serverCommand: string; message: string } | null = null;

/**
 * Handle LSP server errors for HTML
 */
globalThis.on_html_lsp_server_error = function (
  data: LspServerErrorData
): void {
  // Only handle HTML language errors
  if (data.language !== "html") {
    return;
  }

  editor.debug(`html-lsp: Server error - ${data.error_type}: ${data.message}`);

  // Store error state for later reference
  htmlLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  // Show a status message for immediate feedback
  if (data.error_type === "not_found") {
    editor.setStatus(
      `HTML LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`HTML LSP error: ${data.message}`);
  }
};

// Register hook for LSP server errors
editor.on("lsp_server_error", "on_html_lsp_server_error");

/**
 * Handle status bar click when there's an HTML LSP error
 */
globalThis.on_html_lsp_status_clicked = function (
  data: LspStatusClickedData
): void {
  // Only handle HTML language clicks when there's an error
  if (data.language !== "html" || !htmlLspError) {
    return;
  }

  editor.debug("html-lsp: Status clicked, showing help popup");

  // Show action popup with install options
  editor.showActionPopup({
    id: "html-lsp-help",
    title: "HTML Language Server Not Found",
    message: `"${htmlLspError.serverCommand}" provides code completion, diagnostics, and formatting for HTML files. Copy the command below to install it.`,
    actions: [
      { id: "copy_npm", label: `Copy: ${INSTALL_COMMANDS.npm}` },
      { id: "disable", label: "Disable HTML LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
};

// Register hook for status bar clicks
editor.on("lsp_status_clicked", "on_html_lsp_status_clicked");

/**
 * Handle action popup results for HTML LSP help
 */
globalThis.on_html_lsp_action_result = function (
  data: ActionPopupResultData
): void {
  // Only handle our popup
  if (data.popup_id !== "html-lsp-help") {
    return;
  }

  editor.debug(`html-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_npm":
      editor.setClipboard(INSTALL_COMMANDS.npm);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.npm);
      break;

    case "disable":
      editor.disableLspForLanguage("html");
      editor.setStatus("HTML LSP disabled");
      htmlLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      // Just close the popup without action
      break;

    default:
      editor.debug(`html-lsp: Unknown action: ${data.action_id}`);
  }
};

// Register hook for action popup results
editor.on("action_popup_result", "on_html_lsp_action_result");

editor.debug("html-lsp: Plugin loaded");
