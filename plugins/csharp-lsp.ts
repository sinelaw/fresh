/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();


/**
 * C# LSP Helper Plugin
 *
 * Provides user-friendly error handling for C# LSP server issues.
 * When csharp-ls fails to start, this plugin shows an actionable
 * popup with installation instructions.
 *
 * Features:
 * - Detects C# LSP server errors (csharp-ls)
 * - Shows popup with install commands (dotnet tool)
 * - Allows copying install commands to clipboard
 * - Provides option to disable C# LSP
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

// Install commands for C# LSP server (csharp-ls)
// Requires .NET SDK to be installed
// See: https://github.com/razzmatazz/csharp-language-server
const INSTALL_COMMANDS = {
  dotnet: "dotnet tool install --global csharp-ls",
};

// Track error state for C# LSP
let csharpLspError: { serverCommand: string; message: string } | null = null;

/**
 * Handle LSP server errors for C#
 */
globalThis.on_csharp_lsp_server_error = function (
  data: LspServerErrorData
): void {
  // Only handle C# language errors
  if (data.language !== "csharp") {
    return;
  }

  editor.debug(
    `csharp-lsp: Server error - ${data.error_type}: ${data.message}`
  );

  // Store error state for later reference
  csharpLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  // Show a status message for immediate feedback
  if (data.error_type === "not_found") {
    editor.setStatus(
      `C# LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`C# LSP error: ${data.message}`);
  }
};

// Register hook for LSP server errors
editor.on("lsp_server_error", "on_csharp_lsp_server_error");

/**
 * Handle status bar click when there's a C# LSP error
 */
globalThis.on_csharp_lsp_status_clicked = function (
  data: LspStatusClickedData
): void {
  // Only handle C# language clicks when there's an error
  if (data.language !== "csharp" || !csharpLspError) {
    return;
  }

  editor.debug("csharp-lsp: Status clicked, showing help popup");

  // Show action popup with install options
  editor.showActionPopup({
    id: "csharp-lsp-help",
    title: "C# Language Server Not Found",
    message: `"${csharpLspError.serverCommand}" provides code completion, diagnostics, and navigation for C# files. Requires .NET SDK. Copy the command below to install it.`,
    actions: [
      { id: "copy_dotnet", label: `Copy: ${INSTALL_COMMANDS.dotnet}` },
      { id: "disable", label: "Disable C# LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
};

// Register hook for status bar clicks
editor.on("lsp_status_clicked", "on_csharp_lsp_status_clicked");

/**
 * Handle action popup results for C# LSP help
 */
globalThis.on_csharp_lsp_action_result = function (
  data: ActionPopupResultData
): void {
  // Only handle our popup
  if (data.popup_id !== "csharp-lsp-help") {
    return;
  }

  editor.debug(`csharp-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_dotnet":
      editor.setClipboard(INSTALL_COMMANDS.dotnet);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.dotnet);
      break;

    case "disable":
      editor.disableLspForLanguage("csharp");
      editor.setStatus("C# LSP disabled");
      csharpLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      // Just close the popup without action
      break;

    default:
      editor.debug(`csharp-lsp: Unknown action: ${data.action_id}`);
  }
};

// Register hook for action popup results
editor.on("action_popup_result", "on_csharp_lsp_action_result");

editor.debug("csharp-lsp: Plugin loaded");
