/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Nim LSP Helper Plugin
 *
 * Server: nimlangserver (github.com/nim-lang/langserver)
 * VS Code: "Nim" extension (nimsaem.nimvscode)
 * Neovim: nvim-lspconfig nim_langserver
 * Install via: nimble (Nim package manager)
 * Alternative: nimlsp (older, less maintained)
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
  nimble: "nimble install nimlangserver",
  choosenim: "choosenim stable && nimble install nimlangserver",
};

let nimLspError: { serverCommand: string; message: string } | null = null;

function on_nim_lsp_server_error(data: LspServerErrorData): void {
  if (data.language !== "nim") {
    return;
  }

  editor.debug(`nim-lsp: Server error - ${data.error_type}: ${data.message}`);

  nimLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  if (data.error_type === "not_found") {
    editor.setStatus(
      `Nim LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`Nim LSP error: ${data.message}`);
  }
}
registerHandler("on_nim_lsp_server_error", on_nim_lsp_server_error);
editor.on("lsp_server_error", "on_nim_lsp_server_error");

function on_nim_lsp_status_clicked(data: LspStatusClickedData): void {
  if (data.language !== "nim" || !nimLspError) {
    return;
  }

  editor.debug("nim-lsp: Status clicked, showing help popup");

  editor.showActionPopup({
    id: "nim-lsp-help",
    title: "Nim Language Server Not Found",
    message: `"${nimLspError.serverCommand}" provides completion, diagnostics, hover, and go-to-definition for Nim. Requires Nim and nimble.\n\nInstall Nim via choosenim: curl https://nim-lang.org/choosenim/init.sh -sSf | sh\nVS Code users: Install the "Nim" extension.\nSee: https://github.com/nim-lang/langserver`,
    actions: [
      { id: "copy_nimble", label: `Copy: ${INSTALL_COMMANDS.nimble}` },
      { id: "copy_choosenim", label: `Copy: ${INSTALL_COMMANDS.choosenim}` },
      { id: "disable", label: "Disable Nim LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
}
registerHandler("on_nim_lsp_status_clicked", on_nim_lsp_status_clicked);
editor.on("lsp_status_clicked", "on_nim_lsp_status_clicked");

function on_nim_lsp_action_result(data: ActionPopupResultData): void {
  if (data.popup_id !== "nim-lsp-help") {
    return;
  }

  editor.debug(`nim-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_nimble":
      editor.setClipboard(INSTALL_COMMANDS.nimble);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.nimble);
      break;

    case "copy_choosenim":
      editor.setClipboard(INSTALL_COMMANDS.choosenim);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.choosenim);
      break;

    case "disable":
      editor.disableLspForLanguage("nim");
      editor.setStatus("Nim LSP disabled");
      nimLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      break;

    default:
      editor.debug(`nim-lsp: Unknown action: ${data.action_id}`);
  }
}
registerHandler("on_nim_lsp_action_result", on_nim_lsp_action_result);
editor.on("action_popup_result", "on_nim_lsp_action_result");

editor.debug("nim-lsp: Plugin loaded");
