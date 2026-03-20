/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Nushell LSP Helper Plugin
 *
 * Provides user-friendly error handling for Nushell LSP server issues.
 * The LSP server is built into the Nushell binary (nu --lsp).
 *
 * Features:
 * - Detects Nushell LSP server errors
 * - Shows popup with install instructions for Nushell
 * - Provides option to disable Nushell LSP
 *
 * VS Code: "Nushell" extension (vscode-nushell-lang)
 * Neovim: nvim-lspconfig nushell
 * Note: The LSP server is built into the `nu` binary itself
 * No separate LSP installation needed if Nushell is installed
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

// Install commands for Nushell (which includes the language server)
// See: https://www.nushell.sh/book/installation.html
const INSTALL_COMMANDS = {
  cargo: "cargo install nu",
  brew: "brew install nushell",
  winget: "winget install nushell",
};

// Track error state for Nushell LSP
let nushellLspError: { serverCommand: string; message: string } | null = null;

/**
 * Handle LSP server errors for Nushell
 */
function on_nushell_lsp_server_error(data: LspServerErrorData): void {
  if (data.language !== "nushell") {
    return;
  }

  editor.debug(`nushell-lsp: Server error - ${data.error_type}: ${data.message}`);

  nushellLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  if (data.error_type === "not_found") {
    editor.setStatus(
      `Nushell LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`Nushell LSP error: ${data.message}`);
  }
}
registerHandler("on_nushell_lsp_server_error", on_nushell_lsp_server_error);
editor.on("lsp_server_error", "on_nushell_lsp_server_error");

/**
 * Handle status bar click when there's a Nushell LSP error
 */
function on_nushell_lsp_status_clicked(data: LspStatusClickedData): void {
  if (data.language !== "nushell" || !nushellLspError) {
    return;
  }

  editor.debug("nushell-lsp: Status clicked, showing help popup");

  editor.showActionPopup({
    id: "nushell-lsp-help",
    title: "Nushell Language Server Not Found",
    message: `The Nushell LSP server is built into the "nu" binary. Install Nushell to get LSP support. Visit https://www.nushell.sh/book/installation.html for platform-specific instructions.`,
    actions: [
      { id: "copy_cargo", label: `Copy: ${INSTALL_COMMANDS.cargo}` },
      { id: "copy_brew", label: `Copy: ${INSTALL_COMMANDS.brew}` },
      { id: "copy_winget", label: `Copy: ${INSTALL_COMMANDS.winget} (Windows)` },
      { id: "disable", label: "Disable Nushell LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
}
registerHandler("on_nushell_lsp_status_clicked", on_nushell_lsp_status_clicked);
editor.on("lsp_status_clicked", "on_nushell_lsp_status_clicked");

/**
 * Handle action popup results for Nushell LSP help
 */
function on_nushell_lsp_action_result(data: ActionPopupResultData): void {
  if (data.popup_id !== "nushell-lsp-help") {
    return;
  }

  editor.debug(`nushell-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_cargo":
      editor.setClipboard(INSTALL_COMMANDS.cargo);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.cargo);
      break;

    case "copy_brew":
      editor.setClipboard(INSTALL_COMMANDS.brew);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.brew);
      break;

    case "copy_winget":
      editor.setClipboard(INSTALL_COMMANDS.winget);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.winget);
      break;

    case "disable":
      editor.disableLspForLanguage("nushell");
      editor.setStatus("Nushell LSP disabled");
      nushellLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      break;

    default:
      editor.debug(`nushell-lsp: Unknown action: ${data.action_id}`);
  }
}
registerHandler("on_nushell_lsp_action_result", on_nushell_lsp_action_result);
editor.on("action_popup_result", "on_nushell_lsp_action_result");

editor.debug("nushell-lsp: Plugin loaded");
