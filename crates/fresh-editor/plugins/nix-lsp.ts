/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Nix LSP Helper Plugin
 *
 * Server: nil (github.com/oxalica/nil)
 * VS Code: "Nix IDE" extension (uses nil or nixd)
 * Neovim: nvim-lspconfig nil_ls
 * Alternative: nixd (richer completions, option evaluation, flake support)
 * Note: rnix-lsp is deprecated, use nil or nixd instead
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
  nix_profile: "nix profile install github:oxalica/nil",
  nix_env: "nix-env -iA nixpkgs.nil",
  nixd: "nix profile install nixpkgs#nixd",
};

let nixLspError: { serverCommand: string; message: string } | null = null;


editor.on("lsp_server_error", (data) => {
  if (data.language !== "nix") {
    return;
  }

  editor.debug(`nix-lsp: Server error - ${data.error_type}: ${data.message}`);

  nixLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  if (data.error_type === "not_found") {
    editor.setStatus(
      `Nix LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`Nix LSP error: ${data.message}`);
  }
});


editor.on("lsp_status_clicked", (data) => {
  if (data.language !== "nix" || !nixLspError) {
    return;
  }

  editor.debug("nix-lsp: Status clicked, showing help popup");

  editor.showActionPopup({
    id: "nix-lsp-help",
    title: "Nix Language Server Not Found",
    message: `"${nixLspError.serverCommand}" provides completion, diagnostics, go-to-definition, and rename for Nix files.\n\nAlternative: nixd offers richer completions, option evaluation, and flake support.\nNote: rnix-lsp is deprecated.\nVS Code users: Install "Nix IDE" extension.\nSee: https://github.com/oxalica/nil`,
    actions: [
      { id: "copy_nix_profile", label: `Copy: ${INSTALL_COMMANDS.nix_profile}` },
      { id: "copy_nix_env", label: `Copy: ${INSTALL_COMMANDS.nix_env}` },
      { id: "copy_nixd", label: `Copy: ${INSTALL_COMMANDS.nixd}` },
      { id: "disable", label: "Disable Nix LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
});


editor.on("action_popup_result", (data) => {
  if (data.popup_id !== "nix-lsp-help") {
    return;
  }

  editor.debug(`nix-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_nix_profile":
      editor.setClipboard(INSTALL_COMMANDS.nix_profile);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.nix_profile);
      break;

    case "copy_nix_env":
      editor.setClipboard(INSTALL_COMMANDS.nix_env);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.nix_env);
      break;

    case "copy_nixd":
      editor.setClipboard(INSTALL_COMMANDS.nixd);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.nixd);
      break;

    case "disable":
      editor.disableLspForLanguage("nix");
      editor.setStatus("Nix LSP disabled");
      nixLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      break;

    default:
      editor.debug(`nix-lsp: Unknown action: ${data.action_id}`);
  }
});

editor.debug("nix-lsp: Plugin loaded");
