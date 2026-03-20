/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Solidity LSP Helper Plugin
 *
 * Provides user-friendly error handling for Solidity LSP server issues.
 * When nomicfoundation-solidity-language-server fails to start, this plugin
 * shows an actionable popup with installation instructions.
 *
 * Features:
 * - Detects Solidity LSP server errors
 * - Shows popup with install commands (npm)
 * - Provides option to disable Solidity LSP
 *
 * VS Code: "Solidity" by Nomic Foundation (Hardhat), "Solidity" by Juan Blanco
 * Neovim: nvim-lspconfig solidity
 * Alternative: solc (Solidity compiler with diagnostics)
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

// Install commands for Solidity LSP server
// See: https://github.com/NomicFoundation/hardhat-vscode/tree/main/server
const INSTALL_COMMANDS = {
  npm: "npm i -g @nomicfoundation/solidity-language-server",
};

// Track error state for Solidity LSP
let solidityLspError: { serverCommand: string; message: string } | null = null;

/**
 * Handle LSP server errors for Solidity
 */
function on_solidity_lsp_server_error(data: LspServerErrorData): void {
  if (data.language !== "solidity") {
    return;
  }

  editor.debug(`solidity-lsp: Server error - ${data.error_type}: ${data.message}`);

  solidityLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  if (data.error_type === "not_found") {
    editor.setStatus(
      `Solidity LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`Solidity LSP error: ${data.message}`);
  }
}
registerHandler("on_solidity_lsp_server_error", on_solidity_lsp_server_error);
editor.on("lsp_server_error", "on_solidity_lsp_server_error");

/**
 * Handle status bar click when there's a Solidity LSP error
 */
function on_solidity_lsp_status_clicked(data: LspStatusClickedData): void {
  if (data.language !== "solidity" || !solidityLspError) {
    return;
  }

  editor.debug("solidity-lsp: Status clicked, showing help popup");

  editor.showActionPopup({
    id: "solidity-lsp-help",
    title: "Solidity Language Server Not Found",
    message: `"${solidityLspError.serverCommand}" (by Nomic Foundation) provides code completion, diagnostics, and navigation for Solidity smart contracts. Requires Node.js. Copy the command below to install it, or visit https://github.com/NomicFoundation/hardhat-vscode for details.`,
    actions: [
      { id: "copy_npm", label: `Copy: ${INSTALL_COMMANDS.npm}` },
      { id: "disable", label: "Disable Solidity LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
}
registerHandler("on_solidity_lsp_status_clicked", on_solidity_lsp_status_clicked);
editor.on("lsp_status_clicked", "on_solidity_lsp_status_clicked");

/**
 * Handle action popup results for Solidity LSP help
 */
function on_solidity_lsp_action_result(data: ActionPopupResultData): void {
  if (data.popup_id !== "solidity-lsp-help") {
    return;
  }

  editor.debug(`solidity-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_npm":
      editor.setClipboard(INSTALL_COMMANDS.npm);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.npm);
      break;

    case "disable":
      editor.disableLspForLanguage("solidity");
      editor.setStatus("Solidity LSP disabled");
      solidityLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      break;

    default:
      editor.debug(`solidity-lsp: Unknown action: ${data.action_id}`);
  }
}
registerHandler("on_solidity_lsp_action_result", on_solidity_lsp_action_result);
editor.on("action_popup_result", "on_solidity_lsp_action_result");

editor.debug("solidity-lsp: Plugin loaded");
