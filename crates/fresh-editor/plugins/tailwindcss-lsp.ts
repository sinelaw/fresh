/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Tailwind CSS LSP Helper Plugin
 *
 * Server: @tailwindcss/language-server (binary: tailwindcss-language-server)
 * VS Code: "Tailwind CSS IntelliSense" official extension
 * Neovim: nvim-lspconfig tailwindcss
 * Note: Needs Tailwind CSS in your project (tailwind.config.js or CSS @import)
 * Features: class completion, color preview, hover info, linting
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
  npm: "npm install -g @tailwindcss/language-server",
  yarn: "yarn global add @tailwindcss/language-server",
  pnpm: "pnpm add -g @tailwindcss/language-server",
};

let tailwindLspError: { serverCommand: string; message: string } | null = null;

function on_tailwindcss_lsp_server_error(data: LspServerErrorData): void {
  if (data.language !== "tailwindcss") {
    return;
  }

  editor.debug(`tailwindcss-lsp: Server error - ${data.error_type}: ${data.message}`);

  tailwindLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  if (data.error_type === "not_found") {
    editor.setStatus(
      `Tailwind CSS LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`Tailwind CSS LSP error: ${data.message}`);
  }
}
registerHandler("on_tailwindcss_lsp_server_error", on_tailwindcss_lsp_server_error);
editor.on("lsp_server_error", "on_tailwindcss_lsp_server_error");

function on_tailwindcss_lsp_status_clicked(data: LspStatusClickedData): void {
  if (data.language !== "tailwindcss" || !tailwindLspError) {
    return;
  }

  editor.debug("tailwindcss-lsp: Status clicked, showing help popup");

  editor.showActionPopup({
    id: "tailwindcss-lsp-help",
    title: "Tailwind CSS Language Server Not Found",
    message: `"${tailwindLspError.serverCommand}" provides class name completion, color previews, hover info, and linting for Tailwind CSS.\n\nRequires Tailwind CSS configured in your project (tailwind.config.js or v4 CSS @import).\nVS Code users: Install "Tailwind CSS IntelliSense" extension.\nSee: https://github.com/tailwindlabs/tailwindcss-intellisense`,
    actions: [
      { id: "copy_npm", label: `Copy: ${INSTALL_COMMANDS.npm}` },
      { id: "copy_pnpm", label: `Copy: ${INSTALL_COMMANDS.pnpm}` },
      { id: "disable", label: "Disable Tailwind CSS LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
}
registerHandler("on_tailwindcss_lsp_status_clicked", on_tailwindcss_lsp_status_clicked);
editor.on("lsp_status_clicked", "on_tailwindcss_lsp_status_clicked");

function on_tailwindcss_lsp_action_result(data: ActionPopupResultData): void {
  if (data.popup_id !== "tailwindcss-lsp-help") {
    return;
  }

  editor.debug(`tailwindcss-lsp: Action selected - ${data.action_id}`);

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
      editor.disableLspForLanguage("tailwindcss");
      editor.setStatus("Tailwind CSS LSP disabled");
      tailwindLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      break;

    default:
      editor.debug(`tailwindcss-lsp: Unknown action: ${data.action_id}`);
  }
}
registerHandler("on_tailwindcss_lsp_action_result", on_tailwindcss_lsp_action_result);
editor.on("action_popup_result", "on_tailwindcss_lsp_action_result");

editor.debug("tailwindcss-lsp: Plugin loaded");
