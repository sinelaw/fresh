/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Bash/Shell LSP Helper Plugin
 *
 * Provides user-friendly error handling for Bash LSP server issues.
 * When bash-language-server fails to start, this plugin shows an actionable
 * popup with installation instructions.
 *
 * Features:
 * - Detects Bash LSP server errors (bash-language-server)
 * - Shows popup with install commands (npm, yarn, pnpm)
 * - Allows copying install commands to clipboard
 * - Provides option to disable Bash LSP
 *
 * Alternatives:
 * - ShellCheck: Linting/static analysis for shell scripts (https://www.shellcheck.net/)
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

// Install commands for Bash LSP server (bash-language-server)
// See: https://github.com/bash-lsp/bash-language-server
const INSTALL_COMMANDS = {
  npm: "npm i -g bash-language-server",
  yarn: "yarn global add bash-language-server",
  pnpm: "pnpm add -g bash-language-server",
};

// Track error state for Bash LSP
let bashLspError: { serverCommand: string; message: string } | null = null;

/**
 * Handle LSP server errors for Bash
 */


// Register hook for LSP server errors
editor.on("lsp_server_error", (data) => {
  // Only handle Bash language errors
  if (data.language !== "bash") {
    return;
  }

  editor.debug(`bash-lsp: Server error - ${data.error_type}: ${data.message}`);

  // Store error state for later reference
  bashLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  // Show a status message for immediate feedback
  if (data.error_type === "not_found") {
    editor.setStatus(
      `Bash LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`Bash LSP error: ${data.message}`);
  }
});

/**
 * Handle status bar click when there's a Bash LSP error
 */


// Register hook for status bar clicks
editor.on("lsp_status_clicked", (data) => {
  // Only handle Bash language clicks when there's an error
  if (data.language !== "bash" || !bashLspError) {
    return;
  }

  editor.debug("bash-lsp: Status clicked, showing help popup");

  // Show action popup with install options
  editor.showActionPopup({
    id: "bash-lsp-help",
    title: "Bash Language Server Not Found",
    message: `"${bashLspError.serverCommand}" provides code completion, diagnostics, and navigation for shell scripts. Requires Node.js. Copy a command below to install it, or visit https://github.com/bash-lsp/bash-language-server for details. For linting, also consider ShellCheck (https://www.shellcheck.net/).`,
    actions: [
      { id: "copy_npm", label: `Copy: ${INSTALL_COMMANDS.npm}` },
      { id: "copy_yarn", label: `Copy: ${INSTALL_COMMANDS.yarn}` },
      { id: "copy_pnpm", label: `Copy: ${INSTALL_COMMANDS.pnpm}` },
      { id: "disable", label: "Disable Bash LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
});

/**
 * Handle action popup results for Bash LSP help
 */


// Register hook for action popup results
editor.on("action_popup_result", (data) => {
  // Only handle our popup
  if (data.popup_id !== "bash-lsp-help") {
    return;
  }

  editor.debug(`bash-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_npm":
      editor.setClipboard(INSTALL_COMMANDS.npm);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.npm);
      break;

    case "copy_yarn":
      editor.setClipboard(INSTALL_COMMANDS.yarn);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.yarn);
      break;

    case "copy_pnpm":
      editor.setClipboard(INSTALL_COMMANDS.pnpm);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.pnpm);
      break;

    case "disable":
      editor.disableLspForLanguage("bash");
      editor.setStatus("Bash LSP disabled");
      bashLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      // Just close the popup without action
      break;

    default:
      editor.debug(`bash-lsp: Unknown action: ${data.action_id}`);
  }
});

editor.debug("bash-lsp: Plugin loaded");
