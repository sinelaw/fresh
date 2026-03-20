/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Julia LSP Helper Plugin
 *
 * Server: LanguageServer.jl (Julia package)
 * VS Code: "Julia" extension (bundles LanguageServer.jl)
 * Neovim: nvim-lspconfig julials
 * Install via: Julia's Pkg.add() - runs as a Julia script
 * Note: First startup can be slow due to Julia compilation
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
  julia: 'julia -e \'using Pkg; Pkg.add("LanguageServer")\'',
};

let juliaLspError: { serverCommand: string; message: string } | null = null;

function on_julia_lsp_server_error(data: LspServerErrorData): void {
  if (data.language !== "julia") {
    return;
  }

  editor.debug(`julia-lsp: Server error - ${data.error_type}: ${data.message}`);

  juliaLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  if (data.error_type === "not_found") {
    editor.setStatus(
      `Julia LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`Julia LSP error: ${data.message}`);
  }
}
registerHandler("on_julia_lsp_server_error", on_julia_lsp_server_error);
editor.on("lsp_server_error", "on_julia_lsp_server_error");

function on_julia_lsp_status_clicked(data: LspStatusClickedData): void {
  if (data.language !== "julia" || !juliaLspError) {
    return;
  }

  editor.debug("julia-lsp: Status clicked, showing help popup");

  editor.showActionPopup({
    id: "julia-lsp-help",
    title: "Julia Language Server Not Found",
    message: `The Julia language server (LanguageServer.jl) provides completion, diagnostics, formatting, and navigation for Julia. Julia must be installed.\n\nNote: First startup is slow due to Julia's compilation. Consider using PackageCompiler.jl for faster restarts.\nVS Code users: Install the "Julia" extension (auto-installs LanguageServer.jl).\nSee: https://github.com/julia-vscode/LanguageServer.jl`,
    actions: [
      { id: "copy_julia", label: `Copy: ${INSTALL_COMMANDS.julia}` },
      { id: "disable", label: "Disable Julia LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
}
registerHandler("on_julia_lsp_status_clicked", on_julia_lsp_status_clicked);
editor.on("lsp_status_clicked", "on_julia_lsp_status_clicked");

function on_julia_lsp_action_result(data: ActionPopupResultData): void {
  if (data.popup_id !== "julia-lsp-help") {
    return;
  }

  editor.debug(`julia-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_julia":
      editor.setClipboard(INSTALL_COMMANDS.julia);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.julia);
      break;

    case "disable":
      editor.disableLspForLanguage("julia");
      editor.setStatus("Julia LSP disabled");
      juliaLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      break;

    default:
      editor.debug(`julia-lsp: Unknown action: ${data.action_id}`);
  }
}
registerHandler("on_julia_lsp_action_result", on_julia_lsp_action_result);
editor.on("action_popup_result", "on_julia_lsp_action_result");

editor.debug("julia-lsp: Plugin loaded");
