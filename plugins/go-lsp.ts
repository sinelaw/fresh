/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();


/**
 * Go LSP Helper Plugin
 *
 * Provides user-friendly error handling for Go LSP server issues.
 * When gopls fails to start, this plugin shows an actionable
 * popup with installation instructions.
 *
 * Features:
 * - Detects Go LSP server errors (gopls)
 * - Shows popup with install commands
 * - Allows copying install commands to clipboard
 * - Provides option to disable Go LSP
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

// Install commands for Go LSP server (gopls)
// go install is the official recommended method
// See: https://pkg.go.dev/golang.org/x/tools/gopls
const INSTALL_COMMANDS = {
  go: "go install golang.org/x/tools/gopls@latest",
};

// Track error state for Go LSP
let goLspError: { serverCommand: string; message: string } | null = null;

/**
 * Handle LSP server errors for Go
 */
globalThis.on_go_lsp_server_error = function (data: LspServerErrorData): void {
  // Only handle Go language errors
  if (data.language !== "go") {
    return;
  }

  editor.debug(`go-lsp: Server error - ${data.error_type}: ${data.message}`);

  // Store error state for later reference
  goLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  // Show a status message for immediate feedback
  if (data.error_type === "not_found") {
    editor.setStatus(
      `Go LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`Go LSP error: ${data.message}`);
  }
};

// Register hook for LSP server errors
editor.on("lsp_server_error", "on_go_lsp_server_error");

/**
 * Handle status bar click when there's a Go LSP error
 */
globalThis.on_go_lsp_status_clicked = function (
  data: LspStatusClickedData
): void {
  // Only handle Go language clicks when there's an error
  if (data.language !== "go" || !goLspError) {
    return;
  }

  editor.debug("go-lsp: Status clicked, showing help popup");

  // Show action popup with install options
  editor.showActionPopup({
    id: "go-lsp-help",
    title: "Go Language Server Not Found",
    message: `"${goLspError.serverCommand}" provides code completion, diagnostics, and navigation for Go files. Copy the command below to install it.`,
    actions: [
      { id: "copy_go", label: `Copy: ${INSTALL_COMMANDS.go}` },
      { id: "disable", label: "Disable Go LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
};

// Register hook for status bar clicks
editor.on("lsp_status_clicked", "on_go_lsp_status_clicked");

/**
 * Handle action popup results for Go LSP help
 */
globalThis.on_go_lsp_action_result = function (
  data: ActionPopupResultData
): void {
  // Only handle our popup
  if (data.popup_id !== "go-lsp-help") {
    return;
  }

  editor.debug(`go-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_go":
      editor.setClipboard(INSTALL_COMMANDS.go);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.go);
      break;

    case "disable":
      editor.disableLspForLanguage("go");
      editor.setStatus("Go LSP disabled");
      goLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      // Just close the popup without action
      break;

    default:
      editor.debug(`go-lsp: Unknown action: ${data.action_id}`);
  }
};

// Register hook for action popup results
editor.on("action_popup_result", "on_go_lsp_action_result");

editor.debug("go-lsp: Plugin loaded");
