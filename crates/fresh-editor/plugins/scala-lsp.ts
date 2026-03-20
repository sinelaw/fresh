/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Scala LSP Helper Plugin
 *
 * Server: Metals (scalameta.org/metals)
 * VS Code: "Scala (Metals)" extension
 * Neovim: nvim-metals plugin (recommended over nvim-lspconfig)
 * Requires: Java 11+ and Coursier
 * Supports: sbt, Mill, Gradle, Maven via Bloop/BSP
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
  coursier: "cs install metals",
  brew_cs: "brew install coursier/formulas/coursier && cs install metals",
  manual: "curl -fL https://github.com/coursier/coursier/releases/latest/download/cs-x86_64-pc-linux.gz | gzip -d > cs && chmod +x cs && ./cs install metals",
};

let scalaLspError: { serverCommand: string; message: string } | null = null;

function on_scala_lsp_server_error(data: LspServerErrorData): void {
  if (data.language !== "scala") {
    return;
  }

  editor.debug(`scala-lsp: Server error - ${data.error_type}: ${data.message}`);

  scalaLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  if (data.error_type === "not_found") {
    editor.setStatus(
      `Scala LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`Scala LSP error: ${data.message}`);
  }
}
registerHandler("on_scala_lsp_server_error", on_scala_lsp_server_error);
editor.on("lsp_server_error", "on_scala_lsp_server_error");

function on_scala_lsp_status_clicked(data: LspStatusClickedData): void {
  if (data.language !== "scala" || !scalaLspError) {
    return;
  }

  editor.debug("scala-lsp: Status clicked, showing help popup");

  editor.showActionPopup({
    id: "scala-lsp-help",
    title: "Scala Language Server Not Found",
    message: `"${scalaLspError.serverCommand}" (Metals) provides completion, diagnostics, refactoring, and debugging for Scala. Requires Java 11+ and Coursier.\n\nSupports sbt, Mill, Gradle, Maven via Bloop/BSP.\nVS Code users: Install "Scala (Metals)" extension.\nNeovim users: Use nvim-metals plugin.\nSee: https://scalameta.org/metals/docs/editors/overview`,
    actions: [
      { id: "copy_coursier", label: `Copy: ${INSTALL_COMMANDS.coursier}` },
      { id: "copy_brew", label: `Copy: ${INSTALL_COMMANDS.brew_cs}` },
      { id: "disable", label: "Disable Scala LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
}
registerHandler("on_scala_lsp_status_clicked", on_scala_lsp_status_clicked);
editor.on("lsp_status_clicked", "on_scala_lsp_status_clicked");

function on_scala_lsp_action_result(data: ActionPopupResultData): void {
  if (data.popup_id !== "scala-lsp-help") {
    return;
  }

  editor.debug(`scala-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_coursier":
      editor.setClipboard(INSTALL_COMMANDS.coursier);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.coursier);
      break;

    case "copy_brew":
      editor.setClipboard(INSTALL_COMMANDS.brew_cs);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.brew_cs);
      break;

    case "disable":
      editor.disableLspForLanguage("scala");
      editor.setStatus("Scala LSP disabled");
      scalaLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      break;

    default:
      editor.debug(`scala-lsp: Unknown action: ${data.action_id}`);
  }
}
registerHandler("on_scala_lsp_action_result", on_scala_lsp_action_result);
editor.on("action_popup_result", "on_scala_lsp_action_result");

editor.debug("scala-lsp: Plugin loaded");
