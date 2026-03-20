/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Protobuf LSP Helper Plugin
 *
 * Provides user-friendly error handling for Protobuf LSP server issues.
 * When buf fails to start, this plugin shows an actionable
 * popup with installation instructions.
 *
 * Features:
 * - Detects Protobuf LSP server errors (buf)
 * - Shows popup with install commands (brew, npm, etc.)
 * - Provides option to disable Protobuf LSP
 *
 * VS Code: "Buf" extension (auto-installs buf CLI), "Protobuf support" by peterj
 * Neovim: nvim-lspconfig bufls
 * Alternative: protols (open-source, tree-sitter based, standalone)
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

// Install commands for Protobuf LSP server (buf)
// See: https://buf.build/docs/installation
const INSTALL_COMMANDS = {
  brew: "brew install bufbuild/buf/buf",
  npm: "npm i -g @bufbuild/buf",
  go: "go install github.com/bufbuild/buf/cmd/buf@latest",
};

// Track error state for Protobuf LSP
let protobufLspError: { serverCommand: string; message: string } | null = null;

/**
 * Handle LSP server errors for Protobuf
 */
function on_protobuf_lsp_server_error(data: LspServerErrorData): void {
  if (data.language !== "protobuf") {
    return;
  }

  editor.debug(`protobuf-lsp: Server error - ${data.error_type}: ${data.message}`);

  protobufLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  if (data.error_type === "not_found") {
    editor.setStatus(
      `Protobuf LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`Protobuf LSP error: ${data.message}`);
  }
}
registerHandler("on_protobuf_lsp_server_error", on_protobuf_lsp_server_error);
editor.on("lsp_server_error", "on_protobuf_lsp_server_error");

/**
 * Handle status bar click when there's a Protobuf LSP error
 */
function on_protobuf_lsp_status_clicked(data: LspStatusClickedData): void {
  if (data.language !== "protobuf" || !protobufLspError) {
    return;
  }

  editor.debug("protobuf-lsp: Status clicked, showing help popup");

  editor.showActionPopup({
    id: "protobuf-lsp-help",
    title: "Protobuf Language Server Not Found",
    message: `"${protobufLspError.serverCommand}" (Buf CLI) provides code completion, diagnostics, linting, and formatting for Protocol Buffer files. Copy a command below to install it, or visit https://buf.build/docs/installation for details.`,
    actions: [
      { id: "copy_brew", label: `Copy: ${INSTALL_COMMANDS.brew}` },
      { id: "copy_npm", label: `Copy: ${INSTALL_COMMANDS.npm}` },
      { id: "copy_go", label: `Copy: ${INSTALL_COMMANDS.go}` },
      { id: "disable", label: "Disable Protobuf LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
}
registerHandler("on_protobuf_lsp_status_clicked", on_protobuf_lsp_status_clicked);
editor.on("lsp_status_clicked", "on_protobuf_lsp_status_clicked");

/**
 * Handle action popup results for Protobuf LSP help
 */
function on_protobuf_lsp_action_result(data: ActionPopupResultData): void {
  if (data.popup_id !== "protobuf-lsp-help") {
    return;
  }

  editor.debug(`protobuf-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_brew":
      editor.setClipboard(INSTALL_COMMANDS.brew);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.brew);
      break;

    case "copy_npm":
      editor.setClipboard(INSTALL_COMMANDS.npm);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.npm);
      break;

    case "copy_go":
      editor.setClipboard(INSTALL_COMMANDS.go);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.go);
      break;

    case "disable":
      editor.disableLspForLanguage("protobuf");
      editor.setStatus("Protobuf LSP disabled");
      protobufLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      break;

    default:
      editor.debug(`protobuf-lsp: Unknown action: ${data.action_id}`);
  }
}
registerHandler("on_protobuf_lsp_action_result", on_protobuf_lsp_action_result);
editor.on("action_popup_result", "on_protobuf_lsp_action_result");

editor.debug("protobuf-lsp: Plugin loaded");
