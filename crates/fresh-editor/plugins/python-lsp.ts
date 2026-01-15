/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();


/**
 * Python LSP Helper Plugin
 *
 * Provides user-friendly error handling for Python LSP server issues.
 * When the Python LSP server fails to start, this plugin shows an
 * actionable popup with installation instructions.
 *
 * Features:
 * - Detects Python LSP server errors (pylsp, python-lsp-server)
 * - Shows popup with install commands (pip, pipx)
 * - Allows copying install commands to clipboard
 * - Provides option to disable Python LSP
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

// Install commands for Python LSP server (python-lsp-server / pylsp)
// pipx provides isolated installation (recommended)
// pip_all includes all optional dependencies (rope, pyflakes, etc.)
// See: https://github.com/python-lsp/python-lsp-server
const INSTALL_COMMANDS = {
  pipx: "pipx install python-lsp-server",
  pip: "pip install python-lsp-server",
  pip_all: "pip install 'python-lsp-server[all]'",
};

// Track error state for Python LSP
let pythonLspError: { serverCommand: string; message: string } | null = null;

/**
 * Handle LSP server errors for Python
 */
globalThis.on_python_lsp_server_error = function (
  data: LspServerErrorData
): void {
  // Only handle Python language errors
  if (data.language !== "python") {
    return;
  }

  editor.debug(
    `python-lsp: Server error - ${data.error_type}: ${data.message}`
  );

  // Store error state for later reference
  pythonLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  // Show a status message for immediate feedback
  if (data.error_type === "not_found") {
    editor.setStatus(
      `Python LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`Python LSP error: ${data.message}`);
  }
};

// Register hook for LSP server errors
editor.on("lsp_server_error", "on_python_lsp_server_error");

/**
 * Handle status bar click when there's a Python LSP error
 */
globalThis.on_python_lsp_status_clicked = function (
  data: LspStatusClickedData
): void {
  // Only handle Python language clicks when there's an error
  if (data.language !== "python" || !pythonLspError) {
    return;
  }

  editor.debug("python-lsp: Status clicked, showing help popup");

  // Show action popup with install options
  editor.showActionPopup({
    id: "python-lsp-help",
    title: "Python Language Server Not Found",
    message: `"${pythonLspError.serverCommand}" provides code completion, diagnostics, and navigation for Python files. Copy a command below to install it, or search online for your platform.`,
    actions: [
      { id: "copy_pipx", label: `Copy: ${INSTALL_COMMANDS.pipx}` },
      { id: "copy_pip", label: `Copy: ${INSTALL_COMMANDS.pip}` },
      { id: "copy_pip_all", label: `Copy: ${INSTALL_COMMANDS.pip_all}` },
      { id: "disable", label: "Disable Python LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
};

// Register hook for status bar clicks
editor.on("lsp_status_clicked", "on_python_lsp_status_clicked");

/**
 * Handle action popup results for Python LSP help
 */
globalThis.on_python_lsp_action_result = function (
  data: ActionPopupResultData
): void {
  // Only handle our popup
  if (data.popup_id !== "python-lsp-help") {
    return;
  }

  editor.debug(`python-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_pipx":
      editor.setClipboard(INSTALL_COMMANDS.pipx);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.pipx);
      break;

    case "copy_pip":
      editor.setClipboard(INSTALL_COMMANDS.pip);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.pip);
      break;

    case "copy_pip_all":
      editor.setClipboard(INSTALL_COMMANDS.pip_all);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.pip_all);
      break;

    case "disable":
      editor.disableLspForLanguage("python");
      editor.setStatus("Python LSP disabled");
      pythonLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      // Just close the popup without action
      break;

    default:
      editor.debug(`python-lsp: Unknown action: ${data.action_id}`);
  }
};

// Register hook for action popup results
editor.on("action_popup_result", "on_python_lsp_action_result");

editor.debug("python-lsp: Plugin loaded");
