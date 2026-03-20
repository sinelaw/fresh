/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Vue LSP Helper Plugin
 *
 * Server: vue-language-server (@vue/language-server, formerly Volar)
 * VS Code: "Vue - Official" extension (replaces deprecated Vetur)
 * Neovim: nvim-lspconfig volar
 * Note: Supports hybrid mode with @vue/typescript-plugin for TS integration
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
  npm: "npm install -g @vue/language-server",
  yarn: "yarn global add @vue/language-server",
  pnpm: "pnpm add -g @vue/language-server",
};

let vueLspError: { serverCommand: string; message: string } | null = null;

function on_vue_lsp_server_error(data: LspServerErrorData): void {
  if (data.language !== "vue") {
    return;
  }

  editor.debug(`vue-lsp: Server error - ${data.error_type}: ${data.message}`);

  vueLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  if (data.error_type === "not_found") {
    editor.setStatus(
      `Vue LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`Vue LSP error: ${data.message}`);
  }
}
registerHandler("on_vue_lsp_server_error", on_vue_lsp_server_error);
editor.on("lsp_server_error", "on_vue_lsp_server_error");

function on_vue_lsp_status_clicked(data: LspStatusClickedData): void {
  if (data.language !== "vue" || !vueLspError) {
    return;
  }

  editor.debug("vue-lsp: Status clicked, showing help popup");

  editor.showActionPopup({
    id: "vue-lsp-help",
    title: "Vue Language Server Not Found",
    message: `"${vueLspError.serverCommand}" (formerly Volar) provides completion, diagnostics, and refactoring for Vue SFCs. It replaces the deprecated Vetur.\n\nFor TypeScript integration, also install @vue/typescript-plugin.\nVS Code users: Install the "Vue - Official" extension.\nSee: https://github.com/vuejs/language-tools`,
    actions: [
      { id: "copy_npm", label: `Copy: ${INSTALL_COMMANDS.npm}` },
      { id: "copy_pnpm", label: `Copy: ${INSTALL_COMMANDS.pnpm}` },
      { id: "disable", label: "Disable Vue LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
}
registerHandler("on_vue_lsp_status_clicked", on_vue_lsp_status_clicked);
editor.on("lsp_status_clicked", "on_vue_lsp_status_clicked");

function on_vue_lsp_action_result(data: ActionPopupResultData): void {
  if (data.popup_id !== "vue-lsp-help") {
    return;
  }

  editor.debug(`vue-lsp: Action selected - ${data.action_id}`);

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
      editor.disableLspForLanguage("vue");
      editor.setStatus("Vue LSP disabled");
      vueLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      break;

    default:
      editor.debug(`vue-lsp: Unknown action: ${data.action_id}`);
  }
}
registerHandler("on_vue_lsp_action_result", on_vue_lsp_action_result);
editor.on("action_popup_result", "on_vue_lsp_action_result");

editor.debug("vue-lsp: Plugin loaded");
