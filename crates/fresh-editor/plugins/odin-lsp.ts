/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();


/**
 * Odin LSP Helper Plugin
 *
 * Provides user-friendly error handling for Odin LSP server issues.
 * When ols (Odin Language Server) fails to start, this plugin shows an actionable
 * popup with installation instructions.
 *
 * Features:
 * - Detects Odin LSP server errors (ols)
 * - Shows popup with build instructions
 * - Allows copying build commands to clipboard
 * - Provides option to disable Odin LSP
 *
 * OLS: https://github.com/DanielGavin/ols
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

// OLS GitHub repository
const OLS_URL = "https://github.com/DanielGavin/ols";

// Track error state for Odin LSP
let odinLspError: { serverCommand: string; message: string } | null = null;

/**
 * Handle LSP server errors for Odin
 */
globalThis.on_odin_lsp_server_error = function (data: LspServerErrorData): void {
  // Only handle Odin language errors
  if (data.language !== "odin") {
    return;
  }

  editor.debug(`odin-lsp: Server error - ${data.error_type}: ${data.message}`);

  // Store error state for later reference
  odinLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  // Show a status message for immediate feedback
  if (data.error_type === "not_found") {
    editor.setStatus(
      `Odin LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`Odin LSP error: ${data.message}`);
  }
};

// Register hook for LSP server errors
editor.on("lsp_server_error", "on_odin_lsp_server_error");

/**
 * Handle status bar click when there's an Odin LSP error
 */
globalThis.on_odin_lsp_status_clicked = function (
  data: LspStatusClickedData
): void {
  // Only handle Odin language clicks when there's an error
  if (data.language !== "odin" || !odinLspError) {
    return;
  }

  editor.debug("odin-lsp: Status clicked, showing help popup");

  // Show action popup with install options
  editor.showActionPopup({
    id: "odin-lsp-help",
    title: "Odin Language Server Not Found",
    message: `"${odinLspError.serverCommand}" (OLS) provides code completion, diagnostics, and navigation for Odin files.\n\nInstallation: ${OLS_URL}`,
    actions: [
      { id: "disable", label: "Disable Odin LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
};

// Register hook for status bar clicks
editor.on("lsp_status_clicked", "on_odin_lsp_status_clicked");

/**
 * Handle action popup results for Odin LSP help
 */
globalThis.on_odin_lsp_action_result = function (
  data: ActionPopupResultData
): void {
  // Only handle our popup
  if (data.popup_id !== "odin-lsp-help") {
    return;
  }

  editor.debug(`odin-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "disable":
      editor.disableLspForLanguage("odin");
      editor.setStatus("Odin LSP disabled");
      odinLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      // Just close the popup without action
      break;

    default:
      editor.debug(`odin-lsp: Unknown action: ${data.action_id}`);
  }
};

// Register hook for action popup results
editor.on("action_popup_result", "on_odin_lsp_action_result");

editor.debug("odin-lsp: Plugin loaded");
