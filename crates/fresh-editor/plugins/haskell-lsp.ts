/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Haskell LSP Helper Plugin
 *
 * Server: haskell-language-server (HLS)
 * VS Code: "Haskell" extension (installs HLS via GHCup)
 * Neovim: nvim-lspconfig hls
 * Install via: GHCup (recommended), brew, nix
 * Note: HLS must match your GHC version
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
  ghcup: "ghcup install hls",
  brew: "brew install haskell-language-server",
  nix: "nix-env -iA nixpkgs.haskell-language-server",
};

let haskellLspError: { serverCommand: string; message: string } | null = null;


editor.on("lsp_server_error", (data) => {
  if (data.language !== "haskell") {
    return;
  }

  editor.debug(`haskell-lsp: Server error - ${data.error_type}: ${data.message}`);

  haskellLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  if (data.error_type === "not_found") {
    editor.setStatus(
      `Haskell LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`Haskell LSP error: ${data.message}`);
  }
});


editor.on("lsp_status_clicked", (data) => {
  if (data.language !== "haskell" || !haskellLspError) {
    return;
  }

  editor.debug("haskell-lsp: Status clicked, showing help popup");

  editor.showActionPopup({
    id: "haskell-lsp-help",
    title: "Haskell Language Server Not Found",
    message: `"${haskellLspError.serverCommand}" (HLS) provides completion, diagnostics, code actions, and refactoring for Haskell. HLS must match your GHC version.\n\nRecommended: Install via GHCup (manages GHC + HLS versions).\nInstall GHCup: curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh\nVS Code users: Install the "Haskell" extension (auto-installs HLS via GHCup).\nSee: https://haskell-language-server.readthedocs.io`,
    actions: [
      { id: "copy_ghcup", label: `Copy: ${INSTALL_COMMANDS.ghcup}` },
      { id: "copy_brew", label: `Copy: ${INSTALL_COMMANDS.brew}` },
      { id: "copy_nix", label: `Copy: ${INSTALL_COMMANDS.nix}` },
      { id: "disable", label: "Disable Haskell LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
});


editor.on("action_popup_result", (data) => {
  if (data.popup_id !== "haskell-lsp-help") {
    return;
  }

  editor.debug(`haskell-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_ghcup":
      editor.setClipboard(INSTALL_COMMANDS.ghcup);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.ghcup);
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
      editor.disableLspForLanguage("haskell");
      editor.setStatus("Haskell LSP disabled");
      haskellLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      break;

    default:
      editor.debug(`haskell-lsp: Unknown action: ${data.action_id}`);
  }
});

editor.debug("haskell-lsp: Plugin loaded");
