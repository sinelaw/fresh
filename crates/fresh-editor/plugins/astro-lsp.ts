/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Astro LSP Helper Plugin
 *
 * Server: @astrojs/language-server (binary: astro-ls)
 * VS Code: "Astro" official extension
 * Neovim: nvim-lspconfig astro
 * Note: Powered by Volar; may need typescript.tsdk init option
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
  npm: "npm install -g @astrojs/language-server",
  yarn: "yarn global add @astrojs/language-server",
  pnpm: "pnpm add -g @astrojs/language-server",
};

let astroLspError: { serverCommand: string; message: string } | null = null;

function on_astro_lsp_server_error(data: LspServerErrorData): void {
  if (data.language !== "astro") {
    return;
  }

  editor.debug(`astro-lsp: Server error - ${data.error_type}: ${data.message}`);

  astroLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  if (data.error_type === "not_found") {
    editor.setStatus(
      `Astro LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`Astro LSP error: ${data.message}`);
  }
}
registerHandler("on_astro_lsp_server_error", on_astro_lsp_server_error);
editor.on("lsp_server_error", "on_astro_lsp_server_error");

function on_astro_lsp_status_clicked(data: LspStatusClickedData): void {
  if (data.language !== "astro" || !astroLspError) {
    return;
  }

  editor.debug("astro-lsp: Status clicked, showing help popup");

  editor.showActionPopup({
    id: "astro-lsp-help",
    title: "Astro Language Server Not Found",
    message: `"${astroLspError.serverCommand}" provides completion, diagnostics, and formatting for Astro components. Powered by the Volar framework.\n\nRequires TypeScript to be installed in your project for full functionality.\nVS Code users: Install the official "Astro" extension.\nSee: https://github.com/withastro/language-tools`,
    actions: [
      { id: "copy_npm", label: `Copy: ${INSTALL_COMMANDS.npm}` },
      { id: "copy_pnpm", label: `Copy: ${INSTALL_COMMANDS.pnpm}` },
      { id: "disable", label: "Disable Astro LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
}
registerHandler("on_astro_lsp_status_clicked", on_astro_lsp_status_clicked);
editor.on("lsp_status_clicked", "on_astro_lsp_status_clicked");

function on_astro_lsp_action_result(data: ActionPopupResultData): void {
  if (data.popup_id !== "astro-lsp-help") {
    return;
  }

  editor.debug(`astro-lsp: Action selected - ${data.action_id}`);

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
      editor.disableLspForLanguage("astro");
      editor.setStatus("Astro LSP disabled");
      astroLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      break;

    default:
      editor.debug(`astro-lsp: Unknown action: ${data.action_id}`);
  }
}
registerHandler("on_astro_lsp_action_result", on_astro_lsp_action_result);
editor.on("action_popup_result", "on_astro_lsp_action_result");

editor.debug("astro-lsp: Plugin loaded");
