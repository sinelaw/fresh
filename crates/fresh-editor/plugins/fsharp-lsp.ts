/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * F# LSP Helper Plugin
 *
 * Server: fsautocomplete (FsAutoComplete)
 * VS Code: "Ionide-fsharp" extension (bundles fsautocomplete)
 * Neovim: nvim-lspconfig fsautocomplete
 * Install via: dotnet tool, brew, or nix
 * Requires: .NET SDK installed
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
  dotnet: "dotnet tool install -g fsautocomplete",
  brew: "brew install fsautocomplete",
  nix: "nix-env -iA nixpkgs.fsautocomplete",
};

let fsharpLspError: { serverCommand: string; message: string } | null = null;

function on_fsharp_lsp_server_error(data: LspServerErrorData): void {
  if (data.language !== "fsharp") {
    return;
  }

  editor.debug(`fsharp-lsp: Server error - ${data.error_type}: ${data.message}`);

  fsharpLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  if (data.error_type === "not_found") {
    editor.setStatus(
      `F# LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`F# LSP error: ${data.message}`);
  }
}
registerHandler("on_fsharp_lsp_server_error", on_fsharp_lsp_server_error);
editor.on("lsp_server_error", "on_fsharp_lsp_server_error");

function on_fsharp_lsp_status_clicked(data: LspStatusClickedData): void {
  if (data.language !== "fsharp" || !fsharpLspError) {
    return;
  }

  editor.debug("fsharp-lsp: Status clicked, showing help popup");

  editor.showActionPopup({
    id: "fsharp-lsp-help",
    title: "F# Language Server Not Found",
    message: `"${fsharpLspError.serverCommand}" (FsAutoComplete) provides completion, diagnostics, code actions, and refactoring for F#. Requires .NET SDK.\n\nVS Code users: Install "Ionide-fsharp" (bundles fsautocomplete).\nSee: https://github.com/fsharp/FsAutoComplete`,
    actions: [
      { id: "copy_dotnet", label: `Copy: ${INSTALL_COMMANDS.dotnet}` },
      { id: "copy_brew", label: `Copy: ${INSTALL_COMMANDS.brew}` },
      { id: "copy_nix", label: `Copy: ${INSTALL_COMMANDS.nix}` },
      { id: "disable", label: "Disable F# LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
}
registerHandler("on_fsharp_lsp_status_clicked", on_fsharp_lsp_status_clicked);
editor.on("lsp_status_clicked", "on_fsharp_lsp_status_clicked");

function on_fsharp_lsp_action_result(data: ActionPopupResultData): void {
  if (data.popup_id !== "fsharp-lsp-help") {
    return;
  }

  editor.debug(`fsharp-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_dotnet":
      editor.setClipboard(INSTALL_COMMANDS.dotnet);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.dotnet);
      break;

    case "copy_brew":
      editor.setClipboard(INSTALL_COMMANDS.brew);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.brew);
      break;

    case "copy_nix":
      editor.setClipboard(INSTALL_COMMANDS.nix);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.nix);
      break;

    case "disable":
      editor.disableLspForLanguage("fsharp");
      editor.setStatus("F# LSP disabled");
      fsharpLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      break;

    default:
      editor.debug(`fsharp-lsp: Unknown action: ${data.action_id}`);
  }
}
registerHandler("on_fsharp_lsp_action_result", on_fsharp_lsp_action_result);
editor.on("action_popup_result", "on_fsharp_lsp_action_result");

editor.debug("fsharp-lsp: Plugin loaded");
