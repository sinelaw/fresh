/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Gleam LSP Helper Plugin
 *
 * Server: Built into the Gleam compiler (gleam lsp)
 * VS Code: "Gleam" extension
 * Neovim: nvim-lspconfig gleam
 * Note: LSP is bundled with the Gleam binary - install Gleam to get it
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

const INSTALL_COMMANDS = {
  brew: "brew install gleam",
  cargo: "cargo install gleam",
  nix: "nix-env -iA nixpkgs.gleam",
};

let gleamLspError: { serverCommand: string; message: string } | null = null;

function on_gleam_lsp_server_error(data: LspServerErrorData): void {
  if (data.language !== "gleam") {
    return;
  }

  editor.debug(`gleam-lsp: Server error - ${data.error_type}: ${data.message}`);

  gleamLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  if (data.error_type === "not_found") {
    editor.setStatus(
      `Gleam LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`Gleam LSP error: ${data.message}`);
  }
}
registerHandler("on_gleam_lsp_server_error", on_gleam_lsp_server_error);
editor.on("lsp_server_error", "on_gleam_lsp_server_error");

function on_gleam_lsp_status_clicked(data: LspStatusClickedData): void {
  if (data.language !== "gleam" || !gleamLspError) {
    return;
  }

  editor.debug("gleam-lsp: Status clicked, showing help popup");

  editor.showActionPopup({
    id: "gleam-lsp-help",
    title: "Gleam Language Server Not Found",
    message: `The Gleam language server is built into the Gleam compiler binary. Install Gleam to get LSP support - no separate installation needed.\n\nProvides completion, diagnostics, hover, go-to-definition, and formatting.\nVS Code users: Install the "Gleam" extension.\nSee: https://gleam.run/getting-started/installing/`,
    actions: [
      { id: "copy_brew", label: `Copy: ${INSTALL_COMMANDS.brew}` },
      { id: "copy_cargo", label: `Copy: ${INSTALL_COMMANDS.cargo}` },
      { id: "copy_nix", label: `Copy: ${INSTALL_COMMANDS.nix}` },
      { id: "disable", label: "Disable Gleam LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
}
registerHandler("on_gleam_lsp_status_clicked", on_gleam_lsp_status_clicked);
editor.on("lsp_status_clicked", "on_gleam_lsp_status_clicked");

function on_gleam_lsp_action_result(data: ActionPopupResultData): void {
  if (data.popup_id !== "gleam-lsp-help") {
    return;
  }

  editor.debug(`gleam-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_brew":
      editor.setClipboard(INSTALL_COMMANDS.brew);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.brew);
      break;

    case "copy_cargo":
      editor.setClipboard(INSTALL_COMMANDS.cargo);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.cargo);
      break;

    case "copy_nix":
      editor.setClipboard(INSTALL_COMMANDS.nix);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.nix);
      break;

    case "disable":
      editor.disableLspForLanguage("gleam");
      editor.setStatus("Gleam LSP disabled");
      gleamLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      break;

    default:
      editor.debug(`gleam-lsp: Unknown action: ${data.action_id}`);
  }
}
registerHandler("on_gleam_lsp_action_result", on_gleam_lsp_action_result);
editor.on("action_popup_result", "on_gleam_lsp_action_result");

editor.debug("gleam-lsp: Plugin loaded");
