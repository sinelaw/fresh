/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Clojure LSP Helper Plugin
 *
 * Server: clojure-lsp (github.com/clojure-lsp/clojure-lsp)
 * VS Code: "Calva" extension (bundles clojure-lsp)
 * Neovim: nvim-lspconfig clojure_lsp
 * Note: Works with Clojure, ClojureScript, and ClojureDart
 * No project setup needed - analyzes classpath automatically
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
  brew: "brew install clojure-lsp/brew/clojure-lsp-native",
  nix: "nix-shell -p clojure-lsp",
  script: "sudo bash < <(curl -s https://raw.githubusercontent.com/clojure-lsp/clojure-lsp/master/install)",
};

let clojureLspError: { serverCommand: string; message: string } | null = null;

function on_clojure_lsp_server_error(data: LspServerErrorData): void {
  if (data.language !== "clojure") {
    return;
  }

  editor.debug(`clojure-lsp: Server error - ${data.error_type}: ${data.message}`);

  clojureLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  if (data.error_type === "not_found") {
    editor.setStatus(
      `Clojure LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`Clojure LSP error: ${data.message}`);
  }
}
registerHandler("on_clojure_lsp_server_error", on_clojure_lsp_server_error);
editor.on("lsp_server_error", "on_clojure_lsp_server_error");

function on_clojure_lsp_status_clicked(data: LspStatusClickedData): void {
  if (data.language !== "clojure" || !clojureLspError) {
    return;
  }

  editor.debug("clojure-lsp: Status clicked, showing help popup");

  editor.showActionPopup({
    id: "clojure-lsp-help",
    title: "Clojure Language Server Not Found",
    message: `"${clojureLspError.serverCommand}" provides completion, diagnostics, refactoring, and navigation for Clojure/ClojureScript.\n\nNo special project setup needed - it analyzes classpath automatically.\nVS Code users: Install "Calva" (bundles clojure-lsp and nREPL client).\nSee: https://clojure-lsp.io`,
    actions: [
      { id: "copy_brew", label: `Copy: ${INSTALL_COMMANDS.brew}` },
      { id: "copy_nix", label: `Copy: ${INSTALL_COMMANDS.nix}` },
      { id: "copy_script", label: `Copy: install script (Linux/macOS)` },
      { id: "disable", label: "Disable Clojure LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
}
registerHandler("on_clojure_lsp_status_clicked", on_clojure_lsp_status_clicked);
editor.on("lsp_status_clicked", "on_clojure_lsp_status_clicked");

function on_clojure_lsp_action_result(data: ActionPopupResultData): void {
  if (data.popup_id !== "clojure-lsp-help") {
    return;
  }

  editor.debug(`clojure-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_brew":
      editor.setClipboard(INSTALL_COMMANDS.brew);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.brew);
      break;

    case "copy_nix":
      editor.setClipboard(INSTALL_COMMANDS.nix);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.nix);
      break;

    case "copy_script":
      editor.setClipboard(INSTALL_COMMANDS.script);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.script);
      break;

    case "disable":
      editor.disableLspForLanguage("clojure");
      editor.setStatus("Clojure LSP disabled");
      clojureLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      break;

    default:
      editor.debug(`clojure-lsp: Unknown action: ${data.action_id}`);
  }
}
registerHandler("on_clojure_lsp_action_result", on_clojure_lsp_action_result);
editor.on("action_popup_result", "on_clojure_lsp_action_result");

editor.debug("clojure-lsp: Plugin loaded");
