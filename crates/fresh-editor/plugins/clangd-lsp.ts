/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();


/**
 * C/C++ LSP Helper Plugin
 *
 * Provides user-friendly error handling for C/C++ LSP server issues.
 * When clangd fails to start, this plugin shows an actionable
 * popup with installation instructions.
 *
 * Features:
 * - Detects C/C++ LSP server errors (clangd)
 * - Shows popup with install commands (apt, brew, etc.)
 * - Allows copying install commands to clipboard
 * - Provides option to disable C/C++ LSP
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

// Install commands for C/C++ LSP server (clangd)
// See: https://clangd.llvm.org/installation
const INSTALL_COMMANDS = {
  apt: "sudo apt install clangd",
  brew: "brew install llvm",
  pacman: "sudo pacman -S clang",
};

// Languages handled by this plugin
const HANDLED_LANGUAGES = ["c", "cpp"];

// Track error state for C/C++ LSP
let clangdLspError: {
  serverCommand: string;
  message: string;
  language: string;
} | null = null;

/**
 * Handle LSP server errors for C/C++
 */
globalThis.on_clangd_lsp_server_error = function (
  data: LspServerErrorData
): void {
  // Only handle C/C++ language errors
  if (!HANDLED_LANGUAGES.includes(data.language)) {
    return;
  }

  editor.debug(`clangd-lsp: Server error - ${data.error_type}: ${data.message}`);

  // Store error state for later reference
  clangdLspError = {
    serverCommand: data.server_command,
    message: data.message,
    language: data.language,
  };

  // Show a status message for immediate feedback
  if (data.error_type === "not_found") {
    editor.setStatus(
      `C/C++ LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`C/C++ LSP error: ${data.message}`);
  }
};

// Register hook for LSP server errors
editor.on("lsp_server_error", "on_clangd_lsp_server_error");

/**
 * Handle status bar click when there's a C/C++ LSP error
 */
globalThis.on_clangd_lsp_status_clicked = function (
  data: LspStatusClickedData
): void {
  // Only handle C/C++ language clicks when there's an error
  if (!HANDLED_LANGUAGES.includes(data.language) || !clangdLspError) {
    return;
  }

  editor.debug("clangd-lsp: Status clicked, showing help popup");

  // Show action popup with install options
  editor.showActionPopup({
    id: "clangd-lsp-help",
    title: "C/C++ Language Server Not Found",
    message: `"${clangdLspError.serverCommand}" provides code completion, diagnostics, and navigation for C/C++ files. Copy a command below to install it for your platform.`,
    actions: [
      { id: "copy_apt", label: `Copy: ${INSTALL_COMMANDS.apt}` },
      { id: "copy_brew", label: `Copy: ${INSTALL_COMMANDS.brew}` },
      { id: "copy_pacman", label: `Copy: ${INSTALL_COMMANDS.pacman}` },
      { id: "disable", label: "Disable C/C++ LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
};

// Register hook for status bar clicks
editor.on("lsp_status_clicked", "on_clangd_lsp_status_clicked");

/**
 * Handle action popup results for C/C++ LSP help
 */
globalThis.on_clangd_lsp_action_result = function (
  data: ActionPopupResultData
): void {
  // Only handle our popup
  if (data.popup_id !== "clangd-lsp-help") {
    return;
  }

  editor.debug(`clangd-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_apt":
      editor.setClipboard(INSTALL_COMMANDS.apt);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.apt);
      break;

    case "copy_brew":
      editor.setClipboard(INSTALL_COMMANDS.brew);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.brew);
      break;

    case "copy_pacman":
      editor.setClipboard(INSTALL_COMMANDS.pacman);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.pacman);
      break;

    case "disable":
      // Disable for both C and C++
      editor.disableLspForLanguage("c");
      editor.disableLspForLanguage("cpp");
      editor.setStatus("C/C++ LSP disabled");
      clangdLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      // Just close the popup without action
      break;

    default:
      editor.debug(`clangd-lsp: Unknown action: ${data.action_id}`);
  }
};

// Register hook for action popup results
editor.on("action_popup_result", "on_clangd_lsp_action_result");

editor.debug("clangd-lsp: Plugin loaded");
