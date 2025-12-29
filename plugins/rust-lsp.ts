/// <reference path="./lib/fresh.d.ts" />

/**
 * Rust LSP Helper Plugin
 *
 * Provides user-friendly error handling for Rust LSP server issues.
 * When rust-analyzer fails to start, this plugin shows an actionable
 * popup with installation instructions.
 *
 * Features:
 * - Detects Rust LSP server errors (rust-analyzer)
 * - Shows popup with install commands (rustup, brew)
 * - Allows copying install commands to clipboard
 * - Provides option to disable Rust LSP
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

// Install commands for Rust LSP server
const INSTALL_COMMANDS = {
  rustup: "rustup component add rust-analyzer",
  brew: "brew install rust-analyzer",
  cargo: "cargo install rust-analyzer",
};

// Track error state for Rust LSP
let rustLspError: { serverCommand: string; message: string } | null = null;

/**
 * Handle LSP server errors for Rust
 */
globalThis.on_rust_lsp_server_error = function (
  data: LspServerErrorData
): void {
  // Only handle Rust language errors
  if (data.language !== "rust") {
    return;
  }

  editor.debug(`rust-lsp: Server error - ${data.error_type}: ${data.message}`);

  // Store error state for later reference
  rustLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  // Show a status message for immediate feedback
  if (data.error_type === "not_found") {
    editor.setStatus(
      `Rust LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`Rust LSP error: ${data.message}`);
  }
};

// Register hook for LSP server errors
editor.on("lsp_server_error", "on_rust_lsp_server_error");

/**
 * Handle status bar click when there's a Rust LSP error
 */
globalThis.on_rust_lsp_status_clicked = function (
  data: LspStatusClickedData
): void {
  // Only handle Rust language clicks when there's an error
  if (data.language !== "rust" || !rustLspError) {
    return;
  }

  editor.debug("rust-lsp: Status clicked, showing help popup");

  // Show action popup with install options
  editor.showActionPopup({
    id: "rust-lsp-help",
    title: "Rust LSP Error",
    message: `Server '${rustLspError.serverCommand}' not found.\n\nInstall with one of these commands:`,
    actions: [
      { id: "copy_rustup", label: `Copy: ${INSTALL_COMMANDS.rustup}` },
      { id: "copy_brew", label: `Copy: ${INSTALL_COMMANDS.brew}` },
      { id: "copy_cargo", label: `Copy: ${INSTALL_COMMANDS.cargo}` },
      { id: "disable", label: "Disable Rust LSP" },
      { id: "dismiss", label: "Dismiss" },
    ],
  });
};

// Register hook for status bar clicks
editor.on("lsp_status_clicked", "on_rust_lsp_status_clicked");

/**
 * Handle action popup results for Rust LSP help
 */
globalThis.on_rust_lsp_action_result = function (
  data: ActionPopupResultData
): void {
  // Only handle our popup
  if (data.popup_id !== "rust-lsp-help") {
    return;
  }

  editor.debug(`rust-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_rustup":
      editor.setClipboard(INSTALL_COMMANDS.rustup);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.rustup);
      break;

    case "copy_brew":
      editor.setClipboard(INSTALL_COMMANDS.brew);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.brew);
      break;

    case "copy_cargo":
      editor.setClipboard(INSTALL_COMMANDS.cargo);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.cargo);
      break;

    case "disable":
      editor.disableLspForLanguage("rust");
      editor.setStatus("Rust LSP disabled");
      rustLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      // Just close the popup without action
      break;

    default:
      editor.debug(`rust-lsp: Unknown action: ${data.action_id}`);
  }
};

// Register hook for action popup results
editor.on("action_popup_result", "on_rust_lsp_action_result");

editor.debug("rust-lsp: Plugin loaded");
