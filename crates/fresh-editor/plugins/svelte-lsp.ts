/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Svelte LSP Helper Plugin
 *
 * Server: svelte-language-server (binary: svelteserver)
 * VS Code: "Svelte for VS Code" extension
 * Neovim: nvim-lspconfig svelte
 * Note: Also install typescript-svelte-plugin for TS integration
 * CLI tool: svelte-check for CI diagnostics
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
  npm: "npm install -g svelte-language-server",
  yarn: "yarn global add svelte-language-server",
  pnpm: "pnpm add -g svelte-language-server",
};

let svelteLspError: { serverCommand: string; message: string } | null = null;

function on_svelte_lsp_server_error(data: LspServerErrorData): void {
  if (data.language !== "svelte") {
    return;
  }

  editor.debug(`svelte-lsp: Server error - ${data.error_type}: ${data.message}`);

  svelteLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  if (data.error_type === "not_found") {
    editor.setStatus(
      `Svelte LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`Svelte LSP error: ${data.message}`);
  }
}
registerHandler("on_svelte_lsp_server_error", on_svelte_lsp_server_error);
editor.on("lsp_server_error", "on_svelte_lsp_server_error");

function on_svelte_lsp_status_clicked(data: LspStatusClickedData): void {
  if (data.language !== "svelte" || !svelteLspError) {
    return;
  }

  editor.debug("svelte-lsp: Status clicked, showing help popup");

  editor.showActionPopup({
    id: "svelte-lsp-help",
    title: "Svelte Language Server Not Found",
    message: `"${svelteLspError.serverCommand}" provides completion, diagnostics, and formatting for Svelte components.\n\nFor TypeScript integration, also install typescript-svelte-plugin in your project.\nUse svelte-check for CI diagnostics.\nVS Code users: Install the "Svelte for VS Code" extension.\nSee: https://github.com/sveltejs/language-tools`,
    actions: [
      { id: "copy_npm", label: `Copy: ${INSTALL_COMMANDS.npm}` },
      { id: "copy_pnpm", label: `Copy: ${INSTALL_COMMANDS.pnpm}` },
      { id: "disable", label: "Disable Svelte LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
}
registerHandler("on_svelte_lsp_status_clicked", on_svelte_lsp_status_clicked);
editor.on("lsp_status_clicked", "on_svelte_lsp_status_clicked");

function on_svelte_lsp_action_result(data: ActionPopupResultData): void {
  if (data.popup_id !== "svelte-lsp-help") {
    return;
  }

  editor.debug(`svelte-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_npm":
      editor.setClipboard(INSTALL_COMMANDS.npm);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.npm);
      break;

    case "copy_pnpm":
      editor.setClipboard(INSTALL_COMMANDS.pnpm);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.pnpm);
      break;

    case "disable":
      editor.disableLspForLanguage("svelte");
      editor.setStatus("Svelte LSP disabled");
      svelteLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      break;

    default:
      editor.debug(`svelte-lsp: Unknown action: ${data.action_id}`);
  }
}
registerHandler("on_svelte_lsp_action_result", on_svelte_lsp_action_result);
editor.on("action_popup_result", "on_svelte_lsp_action_result");

editor.debug("svelte-lsp: Plugin loaded");
